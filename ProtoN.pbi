;{ ProtoN protocol }

	; v 1.1.1.0
	; 2020			Luna Sole
	DeclareModule ProtoN
		EnableExplicit
		DisableDebugger
		Structure PROTON_LINK
			Status.a							; link status
			OutBytes.q							; a total count of bytes transmitted in outcoming direction
			InBytes.q							; a total count of received bytes
		EndStructure
		Structure PROTON_INSTANCE
			Type.a								; instance type: connection or server
			*Link.PROTON_LINK					; for client instance, it's pointer to client link. for server, it is always 0.
			Status.a							; instance status. if it receives the status CLOSED, instance is released immediately.
		EndStructure
		Enumeration PROTON_TYPES 1				; instance types
			#PN_CONNECTION						; client
			#PN_SERVER							; server
		EndEnumeration
		Enumeration PROTON_STATUS 1				; possible status of link & instance
			#PN_INIT							; (link) a state of connection init phase
			#PN_DATA							; (link, instance) a state of data transfer
			#PN_CLOSED							; (link, instance) closed state
		EndEnumeration
		Declare ProtoNNewConnection(Address$, Port, InitTimeOut, *Callback, OutSpeedLimitB, SessionSize)
		Declare ProtoNNewServer(ServerIP$, Port, InitTimeOut, *Callback, OutSpeedLimitB, SessionSize)
		Declare ProtoNDataAdd(*Link, *hMem, Size)
		Declare ProtoNDataSize()
		Declare ProtoNTick()
		Prototype ProtonCallback (*Instance.ProtoN::PROTON_INSTANCE, *Link.ProtoN::PROTON_LINK, *Data, nLen)
	EndDeclareModule
	Module ProtoN
		Structure PACKET
			magic1.a							; a packet signature-1
			command.a							; type of protocol command
			replyid.u							; session ID
			id.a								; packet ID (unique within current session)
			magic2.a							; a packet signature-2
			hash.l								; packet hash (CRC32)
			magic3.a							; a packet signature-3
			nSize.u								; the size of attached payload. data bytestream follows next after last structure field.
			magic4.a							; a packet signature-4
		EndStructure
		#PN_MAGIC1	= $42						; Proton packet signatures
		#PN_MAGIC2	= $24						; Probably 4 bytes is too much, but let it be for now
		#PN_MAGIC3	= $21
		#PN_MAGIC4	= $10
		#PN_PACKET_SIZE = 1280					; max allowed size of UDP packet
		#PN_HEAD_SIZE = SizeOf(PACKET)			; Proton packet header size
		#PN_DATA_SIZE = 0 +						; the max allowed size of nData (payload)
						(#PN_PACKET_SIZE - #PN_HEAD_SIZE) - (#PN_PACKET_SIZE - #PN_HEAD_SIZE) % 4
		#PN_BUFFER_LEN = 0 +					; a size of input buffer
						 (#PN_HEAD_SIZE+#PN_DATA_SIZE) * 3
		Enumeration PROTON_COMMANDS 1
			#PN_CMD_FIRST
			#PN_CMD_INIT						; (client, server) a command to init a connection. Data: PROTON_PARAM
			#PN_CMD_PING						; (client, server) ping command (used to keep connection alive). Data: PROTON_PARAM
			#PN_CMD_EXIT						; (client, server) used by both sides to alert other side that connection is closed. Data: none
			#PN_CMD_SEND						; (sender, receiver) is sent to receiver to start a new session (push packets). Data: PROTON_PARAM_EX
			#PN_CMD_RECV						; (sender, receiver) is sent to request packets in answer to #PN_CMD_SEND. Data: PROTON_PARAM_EX
			#PN_CMD_DATA						; (sender, receiver) data packet, transfers payload within session. Data: a payload with variable size
			#PN_CMD_LAST
		EndEnumeration
		Structure PROTON_PARAM
			RTT.w								; used to synchronize RTT between sides, -1 means no data
			Param.u								; depends on command
		EndStructure
		Structure PROTON_PARAM_EX Extends PROTON_PARAM
			Bitfield.l							; used to synchronize bitfields between sides
		EndStructure
		#PN_PARAM_SIZE = SizeOf(PROTON_PARAM)
		#PN_PARAM_SIZE_EX = SizeOf(PROTON_PARAM_EX)
		#PN_SESSION_SIZE_MAX = 32				; maximum possible size of single session (packets)
		#PN_32BITS = 0 +						; dummy bitfield which represents all 32 bits
					 %11111111111111111111111111111111
		#PN_OUTQUEUE_MAX			= 128		; a maximum number of packets to store in outcoming queue
		#PN_IDLE_TIMEOUT 			= 30 		; a timeout since last incoming transfer (seconds) to treat link as "idle". after this, pings are sent to other side to check status.
		#PN_BREAK_TIMEOUT 			= 30		; a timeout enabled after "idle", if it expires without getting ping replies, link becomes broken and closes
		#PN_IDLEPING_INTERVAL		= 10		; interval between pings in "idle"
		#PN_FIN_DELAY				= 0 +		; delay which occurs after link is closed on server side.
					 				  #PN_IDLE_TIMEOUT+#PN_BREAK_TIMEOUT+20	; server waits this time before releasing resources, to ensure that client will not start a new link from same port after current link closed.
		#PN_RTT_DEFAULT 			= 300		; initial value of RTT, it's assigned to every new PROTON_LINK
		#PN_RTT_MAX 				= 3000		; maximum possible RTT value (the same limit is applied to D variable, so maximum response timeout can reach #PN_RTT_MAX*2)
		Structure PROTON_LINK_IT Extends PROTON_LINK
			RTTStamp.q							; timestamp used to calculate/update RTT
			RTT.w								; time taken to send packet and receive reply to it, ms. RTT logic here is similar to one used in TCP.
			D.w									; extra value to smooth RTT
			TmrInitTimeout.q					; a timeout to establish a connection. if this timeout expires, link is closed
			TmrInit.q							; a timer to control delay between connection attempts
			TmrIdleness.q						; a timer to monitor link idleness, updated on any incoming data (regardless of valid packet or not)
			TmrPingTimeout.q					; a timeout to close a link if there are no ping reply
			TmrPing.q							; a timer to control ping interval
			TmrFinRelease.q						; a timer used in link cleanup
			SessionSize.a						; maximum outcoming session size for this link
			OutSession.u						; current outcoming session ID. Is synchronized with InNextExpected (taken from opposite side)
			OutMask.l							; bitfield which represents current session packets
			OutCount.a							; a number of packets in current outcoming session
			OutCurrent.a						; out packets counter used when sending data
			TmrOutReq.q							; a timer which sends #PN_CMD_SEND commands during outcoming session
			OutAttempts.l						; a count of attempts performed for current outcoming action, it affects the delay between actions
			InExpectedMask.l					; bitfield, incoming session packets list (synchronized with OutMask when session starts)
			InMask.l							; bitfield, a list of packets which are already received. session becomes complete when this mask is equal to InExpectedMask
			InCount.a							; number of packets in incoming session (is synchronized with OutCount when session starts)
			InSession.u							; current session ID
			InNextExpected.u					; session ID which is expected to be next
			TmrInDat.q							; a timer to trigger incoming session logic, while it remains 0, logic is skipped
			*InBuf								; pointer to a memory allocated for incoming network buffer
			InBufLen.u							; the size of data in network buffer
			List *Inpackets.PACKET()			; temporary list to store incoming packets
			List *OutQueue.PACKET()				; a list to store outcoming packets. when new session begins, packets are moved from here to *OutItems. maximum size of this list = #PN_OUTQUEUE_MAX
			SpeedOutLimit.l						; outcoming transfer speed limitation. inherited from PROTON_INSTANCE
			SpeedOut.f							; a counter used in outcoming speed control
			TmrSpeedControl.q					; a timer used in outcoming speed control
			*Connection							; connection handle returned by Purebasic
			*Instance.PROTON_INSTANCE_IT		; a pointer to instance which owns this link
			*OutDat.PACKET[#PN_SESSION_SIZE_MAX]; outcoming session packets
			*InDat.PACKET[#PN_SESSION_SIZE_MAX]	; incoming session packets
		EndStructure
		Structure PROTON_INSTANCE_IT Extends PROTON_INSTANCE
			SpeedOutLimit.l						; limits outcoming speed. specified in bytes/per second, can't be lower than NData size if set.
			*Handle								; for server it is server PB handle, for client it duplicates value from actual PROTON_LINK
			*Callback.ProtonCallback			; a procedure to receive callbacks, see prototype
			Map *Links.PROTON_LINK_IT()			; a data about attached links.
			InitTime.u							; a timeout for PROTON_LINK to establish a connection (seconds)
			SessionSize.a						; the maximum allowed number of packets in outcoming sessions
		EndStructure
		Global ProtoNTime.q = ElapsedMilliseconds()		; Global protocol timer
		Global NewList *Protons.PROTON_INSTANCE_IT()	; Global storage of existing instances
		UseCRC32Fingerprint()
		Macro PNBitGet (Bitfield, Bit)
			Bool(Bitfield & (1 << Bit))
		EndMacro
		Macro PNBitSet (Bitfield, Bit)
			Bitfield | (1 << Bit)
		EndMacro
		Declare ProtoNLinkNew(*Instance.PROTON_INSTANCE_IT, ConnectionID)
		If InitNetwork() = #False
			MessageRequester("ProtoN module", "InitNetwork() failed!", #PB_MessageRequester_Error)
		EndIf
		; Generates CRC32-hash of given packet
		; RETURN:		4-byte hash
		Procedure.l ProtoNPacketHash(*Packet.PACKET)
			Protected Res.l
			Protected T = StartFingerprint(#PB_Any, #PB_Cipher_CRC32)
			If T
				AddFingerprintBuffer(T, *Packet, OffsetOf(PACKET\hash))
				AddFingerprintBuffer(T, *Packet+OffsetOf(PACKET\hash)+SizeOf(PACKET\hash), #PN_HEAD_SIZE-(OffsetOf(PACKET\hash)+SizeOf(PACKET\hash))+*Packet\nSize)
				Res = Val("$" + FinishFingerprint(T))
			EndIf
			ProcedureReturn Res
		EndProcedure
		; Updates internal time
		; RETURN:		none
		Procedure ProtoNTimeUpdate()
			ProtoNTime + ElapsedMilliseconds() - ProtoNTime
		EndProcedure
		; Reads incoming network data, checks and parses packets
		; *Link			a link to process
		; RETURN:		number of valid packets received. packets are added to a link temporary list
		Procedure ProtoNPacketRead(*Link.PROTON_LINK_IT)
			Protected Err = -1
			Protected msgCount
			Protected mOffset
			Protected sOffset = -1
			Protected bOffset = *Link\InBufLen
			Protected bytes_read
			If #PN_BUFFER_LEN-bOffset >= #PN_HEAD_SIZE
				bytes_read = ReceiveNetworkData (*Link\Connection, *Link\InBuf + bOffset, #PN_BUFFER_LEN-bOffset)
			EndIf
			If bytes_read = -1
				ProcedureReturn 0
			EndIf
			bOffset + bytes_read
			If *Link\Status <> #PN_CLOSED
				*Link\InBytes + bytes_read
				*Link\TmrIdleness = ProtoNTime + #PN_IDLE_TIMEOUT * 1000
				For mOffset = bOffset-#PN_HEAD_SIZE To 0 Step -1
					Protected *msg.PACKET = *Link\InBuf+mOffset
					If *msg\magic1 = #PN_MAGIC1 And *msg\magic2 = #PN_MAGIC2 And *msg\magic3 = #PN_MAGIC3 And *msg\magic4 = #PN_MAGIC4 And
					   *msg\nSize <= #PN_DATA_SIZE And *msg\command > #PN_CMD_FIRST And *msg\command < #PN_CMD_LAST
						If mOffset + #PN_HEAD_SIZE + *msg\nSize > bOffset
							sOffset = mOffset
						Else
							If *msg\hash = ProtoNPacketHash(*msg)
								ResetList(*Link\Inpackets())
								If AddElement(*Link\Inpackets())
									*Link\Inpackets() = AllocateMemory(#PN_HEAD_SIZE + *msg\nSize)
									If *Link\Inpackets()
										CopyMemory(*msg, *Link\Inpackets(), #PN_HEAD_SIZE + *msg\nSize)
										msgCount + 1
									Else
										DeleteElement(*Link\Inpackets())
										Err = mOffset + #PN_HEAD_SIZE + *msg\nSize
										Break
									EndIf
								Else
									Err = mOffset + #PN_HEAD_SIZE + *msg\nSize
									Break
								EndIf
							Else
							EndIf
						EndIf
					Else
					EndIf
				Next
			EndIf
			If Err >= 0
				*Link\InBufLen = Err
			Else
				*Link\InBufLen = 0
			EndIf
			If sOffset >= 0
				If sOffset < Err
					sOffset = Err
				Else
					CopyMemory(*Link\InBuf + sOffset, *Link\InBuf + *Link\InBufLen, bOffset-sOffset)
				EndIf
				*Link\InBufLen + bOffset-sOffset
			EndIf
			ProcedureReturn msgCount
		EndProcedure
		; Sends packet to a specified link
		; *Link			a link
		; *Packet		packet to send
		; RETURN:		#True on success
		Procedure ProtoNPacketSend(*Link.PROTON_LINK_IT, *Packet.PACKET)
			Protected sent_bytes
			Protected result
			If *Link\Status <> #PN_CLOSED
				If *Link\SpeedOutLimit > 0 And *Packet\command = #PN_CMD_DATA And *Link\SpeedOut >= *Link\SpeedOutLimit
					ProcedureReturn #False
				EndIf
				*Packet\hash = ProtoNPacketHash(*Packet)
				sent_bytes = SendNetworkData(*Link\Connection, *Packet, #PN_HEAD_SIZE + *Packet\nSize)
				If Not sent_bytes = -1
					*Link\OutBytes + sent_bytes
					If sent_bytes = #PN_HEAD_SIZE + *Packet\nSize
						result = #True
						If *Packet\command = #PN_CMD_DATA
							*Link\SpeedOut + *Packet\nSize
						EndIf
					EndIf
				EndIf
			EndIf
			ProcedureReturn result
		EndProcedure
		; Generates a service packet with specified parameters and sends it to link
		; RETURN:		#True on success
		Procedure ProtoNCommandSend (*Link.PROTON_LINK_IT, Command, PacketID, TargetID, *Data, DataSize)
			Protected Res
			Protected *CMD.PACKET = AllocateMemory(#PN_HEAD_SIZE + DataSize)
			If *CMD
				*CMD\command = Command
				*CMD\id = PacketID
				*CMD\replyid = TargetID
				If *Data And DataSize > 0 And DataSize <= #PN_DATA_SIZE
					*CMD\nSize = DataSize
					CopyMemory(*Data, *CMD + #PN_HEAD_SIZE, DataSize)
				EndIf
				*CMD\magic1 = #PN_MAGIC1
				*CMD\magic2 = #PN_MAGIC2
				*CMD\magic3 = #PN_MAGIC3
				*CMD\magic4 = #PN_MAGIC4
				Res = ProtoNPacketSend(*Link, *CMD)
				FreeMemory(*CMD)
			EndIf
			ProcedureReturn Res
		EndProcedure
		; Creates a new Proton instance of type "connection"
		; InitTimeOut		time to establish a connection (seconds), 0 means infinite
		; *Callback			callback procedure address
		; OutSpeedLimitB	limit of outcoming transfer speed (bytes), 0 means unlimited
		; SessionSize		maximum number of packets in single session (1 to #PN_SESSION_SIZE_MAX).
		; 					greater value will reduce confirmation overhead a lot, but each session will take more time to finish
		; RETURN:			a pointer to instance on success
		Procedure ProtoNNewConnection(Address$, Port, InitTimeOut, *Callback, OutSpeedLimitB, SessionSize)
			Protected *Proton.PROTON_INSTANCE_IT = AllocateStructure(PROTON_INSTANCE_IT)
			Protected Connect = OpenNetworkConnection(Address$, Port, #PB_Network_UDP)
			Protected Res
			If *Proton And Connect
				If AddElement(*Protons())
					*Protons() = *Proton
					With *Proton
						\Type = #PN_CONNECTION
						\Callback = *Callback
						\Handle = Connect
						If InitTimeOut < 0
							InitTimeOut = 30
						EndIf
						\InitTime = InitTimeOut
						If OutSpeedLimitB And OutSpeedLimitB < #PN_DATA_SIZE
							OutSpeedLimitB = #PN_DATA_SIZE
						EndIf
						\SpeedOutLimit = OutSpeedLimitB
						If SessionSize > #PN_SESSION_SIZE_MAX
							SessionSize = #PN_SESSION_SIZE_MAX
						ElseIf SessionSize < 1
							SessionSize = 1
						EndIf
						\SessionSize = SessionSize
						\Link = ProtoNLinkNew(*Proton, \Handle)
						If \Link
							Res = *Proton
						EndIf
					EndWith
				EndIf
			EndIf
			If Res = 0
				If *Proton
					FreeStructure(*Proton)
				EndIf
				If Connect
					CloseNetworkConnection(Connect)
				EndIf
			EndIf
			ProcedureReturn Res
		EndProcedure
		; Creates a new Proton instance of type "server"
		; ServerIP$	an IP or hostname where server will listen to connections. leave empty or "0.0.0.0" to use all interfaces
		; InitTimeOut		time to wait for clients init (seconds), 0 means infinite
		; *Callback			callback procedure address
		; OutSpeedLimitB	limit of outcoming transfer speed (bytes), 0 means unlimited
		; SessionSize		maximum number of packets in single session (1 to #PN_SESSION_SIZE_MAX).
		; 					greater value will reduce confirmation overhead a lot, but each session will take more time to finish
		; RETURN:			a pointer to instance on success
		Procedure ProtoNNewServer(ServerIP$, Port, InitTimeOut, *Callback, OutSpeedLimitB, SessionSize)
			Protected *Proton.PROTON_INSTANCE_IT = AllocateStructure(PROTON_INSTANCE_IT)
			Protected Connect = CreateNetworkServer(#PB_Any, Port, #PB_Network_UDP, ServerIP$)
			Protected Res
			If *Proton And Connect
				If AddElement(*Protons())
					*Protons() = *Proton
					With *Proton
						\Type = #PN_SERVER
						\Callback = *Callback
						\Handle = Connect
						If InitTimeOut < 0
							InitTimeOut = 30
						EndIf
						\InitTime = InitTimeOut
						If OutSpeedLimitB And OutSpeedLimitB < #PN_DATA_SIZE
							OutSpeedLimitB = #PN_DATA_SIZE
						EndIf
						\SpeedOutLimit = OutSpeedLimitB
						If SessionSize > #PN_SESSION_SIZE_MAX
							SessionSize = #PN_SESSION_SIZE_MAX
						ElseIf SessionSize < 1
							SessionSize = 1
						EndIf
						\SessionSize = SessionSize
						Res = *Protons()
					EndWith
				EndIf
			EndIf
			If Res = 0
				If *Proton
					FreeStructure(*Proton)
				EndIf
				If Connect
					CloseNetworkConnection(Connect)
				EndIf
			EndIf
			ProcedureReturn Res
		EndProcedure
		; Adds data to a link outcoming queue
		; *Link			any valid link of any instance
		; *hMem			pointer to a data to send
		; Size			data size (must be <= #PN_DATA)
		; RETURN:		true on success, false on error or if link's outcoming queue is full
		Procedure ProtoNDataAdd(*Link, *hMem, Size)
			Protected Res
			Protected *TLink.PROTON_LINK_IT = *Link
			If *TLink <= 0 Or *TLink\Status <> #PN_DATA Or *TLink\Instance\Status = #PN_CLOSED
				ProcedureReturn #False
			EndIf
			If *hMem > 0 And Size > 0 And Size <= #PN_DATA_SIZE And ListSize(*TLink\OutQueue()) + 1 <= #PN_OUTQUEUE_MAX
				With *TLink
					If AddElement(\OutQueue())
						\OutQueue() = AllocateMemory(#PN_HEAD_SIZE + Size)
						If \OutQueue()
							\OutQueue()\command = #PN_CMD_DATA
							\OutQueue()\nSize = Size
							\OutQueue()\magic1 = #PN_MAGIC1
							\OutQueue()\magic2 = #PN_MAGIC2
							\OutQueue()\magic3 = #PN_MAGIC3
							\OutQueue()\magic4 = #PN_MAGIC4
							CopyMemory(*hMem, \OutQueue()+#PN_HEAD_SIZE, Size)
							MoveElement(\OutQueue(), #PB_List_Last)
							Res = #True
						Else
							DeleteElement(\OutQueue())
						EndIf
					EndIf
				EndWith
			EndIf
			ProcedureReturn Res
		EndProcedure
		; Returns the maximum size of payload
		; RETURN:		#PN_DATA_SIZE
		Procedure ProtoNDataSize()
			ProcedureReturn #PN_DATA_SIZE
		EndProcedure
		; Allocates new LINK and fills/inherits main fields
		; *Instance		valid ProtoN instance to add link
		; ConnectionID	PB connection
		; RETURN:		pointer to allocated link on success
		Procedure ProtoNLinkNew(*Instance.PROTON_INSTANCE_IT, ConnectionID)
			Protected *Link.PROTON_LINK_IT
			Protected *InBuf
			Protected LinkName$ = Str(ConnectionID)
			Protected Res
			If *Instance
				*Link = AllocateStructure(PROTON_LINK_IT)
				*InBuf = AllocateMemory(#PN_BUFFER_LEN)
				If *Link And *InBuf And AddMapElement(*Instance\Links(), LinkName$)
					ProtoNTimeUpdate()
					*Instance\Links(LinkName$) = *Link
					*Link\Connection = ConnectionID
					*Link\RTT = #PN_RTT_DEFAULT
					*Link\SpeedOutLimit = *Instance\SpeedOutLimit
					*Link\SpeedOut = *Link\SpeedOutLimit
					If *Instance\InitTime
						*Link\TmrInitTimeout = ProtoNTime + *Instance\InitTime * 1000
					EndIf
					*Link\Status = #PN_INIT
					*Link\InBuf = *InBuf
					*Link\SessionSize = *Instance\SessionSize
					*Link\Instance = *Instance
					Res = *Link
				EndIf
			EndIf
			If Res = 0
				If *Link
					FreeStructure(*Link)
				EndIf
				If *InBuf
					FreeMemory(*InBuf)
				EndIf
			EndIf
			ProcedureReturn Res
		EndProcedure
		; Update RTT of specified link
		; *Link			valid ProtoN link
		; M				new RTT value (ms)
		; RETURN:		none
		Procedure ProtoNLinkUpdateRTT(*Link.PROTON_LINK_IT, M)
			Protected TRtt = 0.875**Link\RTT + (1.0-0.875)*M
			Protected TD = 0.875**Link\D + (1.0-0.875)*Abs(*Link\RTT-M)
			If TRtt > #PN_RTT_MAX
				TRtt = #PN_RTT_MAX
			EndIf
			If TD > #PN_RTT_MAX
				TD = #PN_RTT_MAX
			EndIf
			With *Link
				\RTT = TRtt
				\D = TD
			EndWith
		EndProcedure
		; Releases link and all it's resources
		; RETURN:		none
		Procedure ProtoNLinkFree(*Link.PROTON_LINK_IT)
			FreeMemory(*Link\InBuf)
			ForEach *Link\OutQueue()
				FreeMemory(*Link\OutQueue())
			Next
			Protected TT
			For TT = 0 To #PN_SESSION_SIZE_MAX-1
				If *Link\InDat[TT]
					FreeMemory(*Link\InDat[TT])
				EndIf
				If *Link\OutDat[TT]
					FreeMemory(*Link\OutDat[TT])
				EndIf
			Next
			FreeStructure(*Link)
		EndProcedure
		; Main-1:		read all incoming network data and recognize valid packets
		Procedure ProtoNReadIn(*P.PROTON_INSTANCE_IT)
			Protected Event
			Protected *C.PROTON_LINK_IT
			Repeat
				Select *P\Type
					Case #PN_CONNECTION:
						*C = *P\Link
						Event = NetworkClientEvent(*C\Connection)
						If Event = #PB_NetworkEvent_Data
							ProtoNPacketRead(*C)
						EndIf
					Case #PN_SERVER:
						Event = NetworkServerEvent(*P\Handle)
						If Event = #PB_NetworkEvent_Data
							If FindMapElement(*P\Links(), Str(EventClient()))
								*C = *P\Links()
							Else
								*C = ProtoNLinkNew(*P, EventClient())
								If *C = 0
									Break
								EndIf
							EndIf
							ProtoNPacketRead(*C)
						EndIf
				EndSelect
			Until Event <> #PB_NetworkEvent_Data
		EndProcedure
		; Main-2:		process all recognized packets and do all the stuff with them
		; This step heavily depends on Main-3, lot of intersecting code
		Procedure ProtoNProcessIn(*P.PROTON_INSTANCE_IT, *C.PROTON_LINK_IT)
			Protected *I.PACKET
			Protected tRSData.PROTON_PARAM_EX
			Protected *trRSData.PROTON_PARAM_EX
			ForEach *C\Inpackets()
				*I = *C\Inpackets()
				Select *I\command
					Case #PN_CMD_INIT:
						If (*C\Status = #PN_INIT Or *C\Status = #PN_DATA) And *I\nSize = #PN_PARAM_SIZE
							*trRSData = *I + #PN_HEAD_SIZE
							If *trRSData\RTT >= 0
								ProtoNLinkUpdateRTT(*C, *trRSData\RTT)
							EndIf
							If *trRSData\Param = #True
								If *P\Type = #PN_SERVER And ProtoNTime >= *C\TmrInit
									tRSData\RTT = *C\RTT
									tRSData\Param = #False
									Debug "Server receives init"
									ProtoNCommandSend(*C, #PN_CMD_INIT, 0, 0, tRSData, #PN_PARAM_SIZE)
									*C\TmrInit = ProtoNTime + 1 * 1000
								EndIf
							Else
								If *P\Type = #PN_CONNECTION
									Debug "Client received init"
								EndIf
							EndIf
							If *C\Status = #PN_INIT
								*C\Status = #PN_DATA
								*C\OutAttempts = 0
								CallFunctionFast(*P\Callback, *P, *C, 0, 0)
							EndIf
						EndIf
					Case #PN_CMD_PING:
						If *C\Status = #PN_DATA And *I\nSize = #PN_PARAM_SIZE
							*trRSData = *I + #PN_HEAD_SIZE
							If *trRSData\RTT >= 0
								ProtoNLinkUpdateRTT(*C, *trRSData\RTT)
							EndIf
							If *trRSData\Param = #True
								tRSData\RTT = *C\RTT
								tRSData\Param = #False
								Debug "Sending reply to a ping"
								ProtoNCommandSend(*C, #PN_CMD_PING, 0, 0, tRSData, #PN_PARAM_SIZE)
							EndIf
							*C\TmrPingTimeout = 0
							*C\TmrPing = 0
						EndIf
					Case #PN_CMD_SEND:
						If *C\Status = #PN_DATA And *I\nSize = #PN_PARAM_SIZE_EX
							*trRSData = *I + #PN_HEAD_SIZE
							If *trRSData\RTT >= 0
								ProtoNLinkUpdateRTT(*C, *trRSData\RTT)
							EndIf
							If *I\replyid = *C\InNextExpected And *C\InCount = 0
								If *trRSData\Param <= #PN_SESSION_SIZE_MAX
									Protected.l cZ1 = *trRSData\Bitfield << (#PN_SESSION_SIZE_MAX - *trRSData\Param), cZ2 = #PN_32BITS << (#PN_SESSION_SIZE_MAX-*trRSData\Param)
									If cZ1 = cZ2
										*C\InSession = *C\InNextExpected
										*C\TmrInDat = ProtoNTime
										*C\InMask = 0
										*C\InExpectedMask = *trRSData\Bitfield
										*C\InCount = *trRSData\Param
										Debug "Receiver gets data push #" + Str(*C\InSession) + ", count=" + Str(*C\InCount) + ", " + Bin(*C\InExpectedMask, #PB_Long)
									EndIf
								EndIf
							ElseIf *I\replyid = *C\InSession And *C\InCount
								Debug "Receiver gets REQ #" + Str(*C\InSession)
								If *C\TmrInDat = 0
									*C\TmrInDat = ProtoNTime
								EndIf
							ElseIf *C\InCount = 0
								Debug "Receiver forces sender to finish " + Str(*I\replyid)
								tRSData\Bitfield = #PN_32BITS
								tRSData\RTT = *C\RTT
								tRSData\Param = *C\InNextExpected
								ProtoNCommandSend(*C, #PN_CMD_RECV, 0, *I\replyid, tRSData, #PN_PARAM_SIZE_EX)
							EndIf
						EndIf
					Case #PN_CMD_RECV:
						If *C\Status = #PN_DATA And *C\OutCount And *C\OutCurrent = *C\OutCount And *I\replyid = *C\OutSession And *I\nSize = #PN_PARAM_SIZE_EX
							*trRSData = *I + #PN_HEAD_SIZE
							If *trRSData\RTT >= 0
								ProtoNLinkUpdateRTT(*C, *trRSData\RTT)
							EndIf
							If *C\RTTStamp
								ProtoNLinkUpdateRTT(*C, ProtoNTime - *C\RTTStamp)
								*C\RTTStamp = 0
							EndIf
							*C\OutAttempts = 0
							*C\OutMask = *trRSData\Bitfield
							*C\OutMask = ~*C\OutMask
							If *C\OutMask = 0
								Debug "Sender finished " + Str(*I\replyid)
								While *C\OutCount > 0
									*C\OutCount - 1
									FreeMemory(*C\OutDat[*C\OutCount])
									*C\OutDat[*C\OutCount] = 0
								Wend
								*C\OutMask = 0
								*C\OutCurrent = 0
								*C\OutSession = *trRSData\Param
							Else
								Debug "Sender receives data request, " + Bin(*C\OutMask, #PB_Long)
								*C\OutCurrent = 0
							EndIf
						EndIf
					Case #PN_CMD_DATA:
						If *C\Status = #PN_DATA And *C\InCount And *I\replyid = *C\InSession And PNBitGet(*C\InExpectedMask, *I\id)
							If PNBitGet(*C\InMask, *I\id) = #False And *C\InDat[*I\id] = 0
								*C\InDat[*I\id] = *I
								PNBitSet(*C\InMask, *I\id)
								Debug "Receiver stores data: " + Str(*I\replyid) + ", " + Str(*I\id)
								*I = 0
								If *C\InMask = *C\InExpectedMask
									*C\TmrInDat = ProtoNTime
								EndIf
							EndIf
						EndIf
					Case #PN_CMD_EXIT:
						*C\Status = #PN_CLOSED
						Debug "Received FIN"
				EndSelect
				If *I
					FreeMemory(*I)
				EndIf
			Next
			ClearList(*C\Inpackets())
		EndProcedure
		; Main-3:		process link after two previous steps
		; This step heavily depends on Main-2, lot of intersecting code
		Procedure ProtoNProcess(*P.PROTON_INSTANCE_IT, *C.PROTON_LINK_IT)
			Protected tRSData.PROTON_PARAM_EX
			Protected *trRSData.PROTON_PARAM_EX
			Select *C\Status
				Case #PN_INIT:
					If *P\Type = #PN_CONNECTION
						If ProtoNTime <= *C\TmrInitTimeout Or *C\TmrInitTimeout = 0
							If ProtoNTime >= *C\TmrInit
								tRSData\RTT = *C\RTT
								tRSData\Param = #True
								Debug "Client: send connection init"
								ProtoNCommandSend(*C, #PN_CMD_INIT, 0, 0, tRSData, #PN_PARAM_SIZE)
								*C\OutAttempts + 1
								If *C\OutAttempts > 100
									*C\OutAttempts = 100
								EndIf
								*C\TmrInit = ProtoNTime + Sqr(*C\OutAttempts) * 1000
							EndIf
						Else
							Debug "Init timeout expired"
							*C\Status = #PN_CLOSED
						EndIf
					ElseIf *P\Type = #PN_SERVER
						If ProtoNTime >= *C\TmrInitTimeout
							Debug "Server: client init timeout expired"
							*C\Status = #PN_CLOSED
						EndIf
					EndIf
				Case #PN_DATA:
					If ProtoNTime >= *C\TmrIdleness
						If *C\TmrPingTimeout = 0
							*C\TmrPingTimeout = ProtoNTime + #PN_BREAK_TIMEOUT * 1000
							If *P\Type = #PN_SERVER
								*C\TmrPing = ProtoNTime + #PN_IDLEPING_INTERVAL * 1000
							EndIf
						EndIf
						If ProtoNTime >= *C\TmrPingTimeout
							Debug "Idle timeout expired"
							*C\Status = #PN_CLOSED
						Else
							If ProtoNTime >= *C\TmrPing
								tRSData\RTT = *C\RTT
								tRSData\Param = #True
								ProtoNCommandSend(*C, #PN_CMD_PING, 0, 0, tRSData, #PN_PARAM_SIZE)
								Debug "Sending ping to check connection"
								*C\TmrPing = ProtoNTime + #PN_IDLEPING_INTERVAL * 1000
							EndIf
						EndIf
					EndIf
					If *C\OutCount = 0 And ListSize(*C\OutQueue()) >= 1
						*C\TmrOutReq = 0
						*C\OutMask = 0
						*C\OutAttempts = 0
						ResetList(*C\OutQueue())
						While *C\OutCount < *C\SessionSize And NextElement(*C\OutQueue())
							PNBitSet(*C\OutMask, *C\OutCount)
							*C\OutDat[*C\OutCount] = *C\OutQueue()
							*C\OutDat[*C\OutCount]\id = *C\OutCount
							*C\OutDat[*C\OutCount]\replyid = *C\OutSession
							DeleteElement(*C\OutQueue())
							*C\OutCount + 1
						Wend
						*C\OutCurrent = *C\OutCount
					EndIf
					If *C\OutCount
						If *C\OutCurrent < *C\OutCount
							If *C\SpeedOutLimit > 0
								*C\SpeedOut - *C\SpeedOutLimit * ((ProtoNTime - *C\TmrSpeedControl) / 1000.0)
								If *C\SpeedOut < 0.0
									*C\SpeedOut = 0.0
								EndIf
								*C\TmrSpeedControl = ProtoNTime
							EndIf
							While *C\OutCurrent < *C\OutCount
								If PNBitGet(*C\OutMask, *C\OutCurrent)
									If ProtoNPacketSend(*C, *C\OutDat[*C\OutCurrent])
										Debug "Sender DATA for id " + Str(*C\OutSession) + ", " + *C\OutDat[*C\OutCurrent]\id
										*C\OutCurrent + 1
									Else
										Break
									EndIf
								Else
									*C\OutCurrent + 1
								EndIf
							Wend
							If *C\OutCurrent >= *C\OutCount
								*C\TmrOutReq = ProtoNTime + *C\RTT * 0.7
							EndIf
						ElseIf ProtoNTime >= *C\TmrOutReq
							Debug "Sender REQ id " + Str(*C\OutSession) + ", "  + Bin(*C\OutMask, #PB_Long)
							tRSData\Bitfield = *C\OutMask
							tRSData\RTT = *C\RTT
							tRSData\Param = *C\OutCount
							ProtoNCommandSend(*C, #PN_CMD_SEND, 0, *C\OutSession, tRSData, #PN_PARAM_SIZE_EX)
							*C\OutAttempts + 1
							If *C\OutAttempts > 42
								*C\OutAttempts = 42
							EndIf
							*C\TmrOutReq = ProtoNTime + (*C\RTT + *C\D) * Sqr(*C\OutAttempts)
							If *C\RTTStamp = 0
								*C\RTTStamp = ProtoNTime
							EndIf
						EndIf
					EndIf
					If *C\InCount And *C\TmrInDat And ProtoNTime >= *C\TmrInDat
						If *C\InMask = *C\InExpectedMask
							Debug "Receiver finished " + Str(*C\InSession) + "."
							*C\InNextExpected + 1
							If *C\InNextExpected = 0
								*C\InNextExpected = 1
							EndIf
							tRSData\Bitfield = #PN_32BITS
							tRSData\RTT = *C\RTT
							tRSData\Param = *C\InNextExpected
							ProtoNCommandSend(*C, #PN_CMD_RECV, 0, *C\InSession, tRSData, #PN_PARAM_SIZE_EX)
							Protected Ci = 0
							While Ci < *C\InCount
								CallFunctionFast(*P\Callback, *P, *C, *C\InDat[Ci]+#PN_HEAD_SIZE, *C\InDat[Ci]\nSize)
								FreeMemory(*C\InDat[Ci])
								*C\InDat[Ci] = 0
								Ci + 1
							Wend
							*C\InCount = 0
							*C\TmrInDat = 0
						Else
							Debug "Receiver sends data request " + Bin(*C\InMask, #PB_Long)
							tRSData\Bitfield = *C\InMask
							tRSData\RTT = *C\RTT
							tRSData\Param = 0
							ProtoNCommandSend(*C, #PN_CMD_RECV, 0, *C\InSession, tRSData, #PN_PARAM_SIZE_EX)
							*C\TmrInDat = 0
						EndIf
					EndIf
				Case #PN_CLOSED:
					Select *P\Type
						Case #PN_CONNECTION:
							*C\Status = 0
							ProtoNCommandSend(*C, #PN_CMD_EXIT, 0, 0, 0, 0)
							*C\Status = #PN_CLOSED
							*P\Status = #PN_CLOSED
						Case #PN_SERVER:
							If *C\TmrFinRelease = 0
								*C\TmrFinRelease = ProtoNTime + #PN_FIN_DELAY * 1000
								*C\Status = 0
								ProtoNCommandSend(*C, #PN_CMD_EXIT, 0, 0, 0, 0)
								*C\Status = #PN_CLOSED
								CallFunctionFast(*P\Callback, *P, *C, 0, 0)
							EndIf
							If ProtoNTime >= *C\TmrFinRelease
								ProtoNLinkFree(*C)
								DeleteMapElement(*P\Links())
							EndIf
					EndSelect
			EndSelect
		EndProcedure
		; A main procedure where all protocol logic gathered
		; Should be called continuously from outside
		; RETURN:		none
		Procedure ProtoNTick()
			Protected *P.PROTON_INSTANCE_IT
			Protected *C.PROTON_LINK_IT
			ForEach *Protons()
				*P = *Protons()
				ProtoNTimeUpdate()
				ProtoNReadIn(*P)
				ForEach *P\Links()
					*C = *P\Links()
					ProtoNProcessIn(*P, *C)
					ProtoNProcess(*P, *C)
				Next *P\Links()
				If *P\Status = #PN_CLOSED
					CallFunctionFast(*P\Callback, *P, 0, 0, 0)
					Select *P\Type
						Case #PN_CONNECTION:
							CloseNetworkConnection(*P\Handle)
						Case #PN_SERVER:
							CloseNetworkServer(*P\Handle)
					EndSelect
					ForEach *P\Links()
						ProtoNLinkFree(*P\Links())
					Next
					FreeStructure(*Protons())
					DeleteElement(*Protons())
				EndIf
			Next *Protons()
		EndProcedure
	EndModule
;}
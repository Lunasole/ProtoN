EnableExplicit

; Include ProtoN module
XIncludeFile "ProtoN.pbi"

;;;;;;;;;;;;;;;
; Here is some test example.
; It launches one instance of server and one client which connects to it.
; After successful connection, data transfer starts (in both directions simultaneously)

; This is set to true when client connected successfully
Global ClientConnected
; A host to connect
Global TestHost$ = "localhost"
Global TestPort = 22112

; Instances: server and client
Global *Server.ProtoN::PROTON_INSTANCE
Global *Connection.ProtoN::PROTON_INSTANCE

; Link variable: when client connects to a server, server creates a "link" for it
; Then link is used to send data to this client
Global *SrvClient1Link.ProtoN::PROTON_LINK


; Callback procedure
; This one is used both for server and client instance
; Instance and Link status may be safely changed from here (for example, to #PB_CLOSED)
; This can be used to close a connection manually (for now there is no such function yet)
; *Data	- is the pointer to a data received from connection. It may be null when link just changes status.
Procedure ProtonCB(*Instance.ProtoN::PROTON_INSTANCE, *Link.ProtoN::PROTON_LINK, *Data, nLen)
	Select *Instance
		Case *Connection:
			; A callback from client instance
			If *Instance\Status = ProtoN::#PN_CLOSED
				Debug "Client callback: Closed instance"
				*Connection = 0
				ClientConnected = #False
			Else
				Debug "Client callback: " + Str(*Link\Status)
				If *Link\Status = ProtoN::#PN_DATA
					; here client received *Data of size nLen (or client link status changed to #PN_DATA)
					ClientConnected = #True
				Else
					ClientConnected = #False
				EndIf
			EndIf
			
		Case *Server:
			; A callback from server instance
			If *Instance\Status = ProtoN::#PN_CLOSED
				Debug "Server callback: Closed instance"
				*SrvClient1Link = 0
				*Server = 0
			Else
				Debug "Server callback: " + Str(*Link\Status)
				If *Link\Status = ProtoN::#PN_DATA
					; here server received *Data of size nLen from *Link. Or link status changed to #PN_DATA.
					; Store this client link for future
					*SrvClient1Link = *Link
				Else
					*SrvClient1Link = 0
				EndIf
			EndIf
	EndSelect
EndProcedure



; This is dummy memory to send
Define S = AllocateMemory(ProtoN::ProtoNDataSize())
FillMemory(S, MemorySize(S), $FA, #PB_Byte)

OpenConsole("ProtoN simple test")
PrintN("START")

; Create server instance with no outcoming speed limit and outcoming session len = 5 packets.
*Server = ProtoN::ProtoNNewServer("0.0.0.0", TestPort, 20, @ProtonCB(), 0, 5)
; Create client instance with no outcoming speed limit and outcoming session len = 6 packets.
*Connection = ProtoN::ProtoNNewConnection(TestHost$, TestPort, 30, @ProtonCB(), 0, 6)

; Some variables used to measure client's transfer speed
Define LSpeed.q
Define LSpeedIn.q
Define LSec
; Misc
Define Calls

; Main
Repeat
	; Protocol main procedure must be called continously to process all the things
	ProtoN::ProtoNTick()
	
	If *Connection
		; Measure client out/in speed and print it to console
		If Not Date() = LSec
			PrintN("Out: " + StrF((*Connection\Link\OutBytes - LSpeed) / 1024 / 1024, 3) + " mb/s, In: " +
			       StrF((*Connection\Link\InBytes - LSpeedIn) / 1024 / 1024, 3) + " mb/s")
			LSpeed = *Connection\Link\OutBytes
			LSpeedIn = *Connection\Link\InBytes
			LSec = Date()
		EndIf	
		
		; Send lot of data through client connection
		If ClientConnected 
			While ProtoN::ProtoNDataAdd(*Connection\Link, S, MemorySize(S))
			Wend
		EndIf
	EndIf
	
	; Randomly send some data from server to client
	If *Server And *SrvClient1Link And Random(10, 1) = 1
		ProtoN::ProtoNDataAdd(*SrvClient1Link, S, MemorySize(S))
	EndIf
	
	; delays in loop greatly affecting maximum possible transfer speed, remove them to get full speed (and total processor load)
	
	Calls + 1
	If Calls % 4 = 0
		Delay(1)
	EndIf
ForEver
; IDE Options = PureBasic 5.70 LTS (Windows - x86)
; CursorPosition = 73
; FirstLine = 55
; Folding = ---
; EnableXP
; CPU = 1
; CompileSourceDirectory
; EnablePurifier
; EnableBuildCount = 0
; EnableExeConstant
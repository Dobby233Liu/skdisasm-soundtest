; function to make a little-endian 16-bit pointer for the Z80 sound driver
z80_ptr function x,(x)<<8&$FF00|(x)>>8&$7F|$80

; values for the type argument
VRAM = %100001
CRAM = %101011
VSRAM = %100101
; values for the rwd argument
READ = %001100
WRITE = %000111
DMA = %100111
; makes a VDP command
vdpComm function addr,type,rwd,(((type&rwd)&3)<<30)|((addr&$3FFF)<<16)|(((type&rwd)&$FC)<<2)|((addr&$C000)>>14)

; tells the Z80 to stop, and waits for it to finish stopping (acquire bus)
stopZ80 macro
	move.w	#$100,(Z80_bus_request).l ; stop the Z80
-	btst	#0,(Z80_bus_request).l
	bne.s	-	; loop until it says it's stopped
    endm

; tells the Z80 to start again
startZ80 macro
	move.w	#0,(Z80_bus_request).l    ; start the Z80
    endm

wait macro time
	move.w	#time-1,d0
-	bsr	Wait_VSync
	dbf	d0,-
	endm

music macro id
	stopZ80
	move.b	#signextendB(id),(Z80_RAM+zMusicNumber).l
	startZ80
	endm
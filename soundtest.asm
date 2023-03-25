		cpu 68000

		include "sonic3k.macrosetup.asm"		; include a few basic macros
		include "sonic3k.macros.mini.asm"		; include some simplifying macros and functions
		include "sonic3k.constants.mini.asm"	; include some constants

		org 0
	if * <> 0
		fatal "Start of ROM was $\{*} but it should be 0"
	endif

Vectors:	dc.l	0,	EntryPoint,	ErrorTrap,	ErrorTrap	; 0
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 4
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 8
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 12
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 16
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 20
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 24
		dc.l	H_int_jump,	ErrorTrap,	V_int_jump,	ErrorTrap	; 28
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 32
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 36
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 40
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 44
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 48
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 52
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 56
		dc.l	ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	; 60
Header:		dc.b "SEGA GENESIS    "
Copyright:	dc.b "(C)SEGA 1994.JUN"
Domestic_Name:	dc.b "SOUND TEST PROGRAM"
    align20 $150
Overseas_Name:	dc.b "SOUND TEST PROGRAM"
    align20 $180
Serial_Number:	dc.b "GM 0000test-00"
Checksum:	dc.w $0000
    align20 $1A0
ROMStartLoc:	dc.l 0
ROMEndLoc:	dc.l EndOfROM-1
RAMStartLoc:	dc.l (RAM_start&$FFFFFF)
RAMEndLoc:	dc.l (RAM_start&$FFFFFF)+$FFFF
CartRAM_Info:	dc.b "  "
CartRAM_Type:	dc.b "  "
CartRAMStartLoc:	dc.b "    "
CartRAMEndLoc:	dc.b "    "
Modem_Info:	dc.b "  "
		dc.b "          "
Country_Code:	dc.b "JUE "

; ---------------------------------------------------------------------------

ErrorTrap:	bra.s *

;-------------------------------------------------------------------------
; Streamlined Startup for Sonic the Hedgehog 1
; Targets Sonic 3K SonicRetro AS, but can be adapted to
; other disassemblies and other games.
; (Made by RepellantMold, based on Sonic 2 SonicRetro AS)
; NOTE: by default, this assumes you're hacking Sonic (3) & Knuckles.
; Commented out lines with (Sonic 3) above them will be for s3.asm.
; Includes code from MarkeyJester's init library:
; https://pastebin.com/KXpmQxQp
;-------------------------------------------------------------------------
EntryPoint:
		lea	System_stack,sp				; set stack pointer
		lea	SetupValues(pc),a0				; load setup array
		move.w	(a0)+,sr					; disable interrupts during setup; they will be reenabled by the Sega Screen
		movem.l (a0)+,a1-a3/a5/a6				; Z80 RAM start, work RAM start, Z80 bus request register, VDP data port, VDP control port
		movem.w (a0)+,d1/d2					; first VDP register value ($8004), VDP register increment/value for Z80 stop and reset release ($100)
		moveq	#SetupVDP_end-SetupVDP-1,d5			; VDP registers loop counter
		moveq	#0,d4						; DMA fill/memory clear/Z80 stop bit test value
		movea.l d4,a4						; clear a4
		move.l	a4,usp						; clear user stack pointer
		
		tst.w	HW_Expansion_Control-1-Z80_bus_request(a3)	; was this a soft reset?
		bne.s	.wait_dma					; if so, skip setting region and the TMSS check

		move.b	HW_Version-Z80_bus_request(a3),d6		; load hardware version
		move.b	d6,d3						; copy to d3 for checking revision (d6 will be used later to set region and speed)
		andi.b	#$F,d3						; get only hardware version ID
		beq.s	.wait_dma					; if Model 1 VA4 or earlier (ID = 0), branch
		move.l	#'SEGA',Security_addr-Z80_bus_request(a3)	; satisfy the TMSS
		
.wait_dma:
		move.w	(a6),ccr					; copy status register to CCR, clearing the VDP write latch and setting the overflow flag if a DMA is in progress
		bvs.s	.wait_dma					; if a DMA was in progress during a soft reset, wait until it is finished
	   
.loop_vdp:
		move.w	d2,(a6)						; set VDP register
		add.w	d1,d2						; advance register ID
		move.b	(a0)+,d2					; load next register value
		dbf	d5,.loop_vdp					; repeat for all registers ; final value loaded will be used later to initialize I/0 ports
	   
		move.l	(a0)+,(a6)					; set DMA fill destination
		move.w	d4,(a5)						; set DMA fill value (0000), clearing the VRAM
			
		tst.w	HW_Expansion_Control-1-Z80_bus_request(a3)	; was this a soft reset?
		bne.s	.clear_every_reset				; if so, skip clearing RAM addresses $FE00-$FFFF
	   
		movea.l	(a0),a4						; System_stack	  (increment will happen later)
		move.w	4(a0),d5					; repeat times
		
.loop_ram1:
		move.l	d4,(a4)+
		dbf	d5,.loop_ram1					; clear RAM ($FE00-$FFFF)

.clear_every_reset:
		addq	#6,a0						; advance to next position in setup array
		move.w	(a0)+,d5					; repeat times
		
.loop_ram2:
		move.l	d4,(a2)+					; a2 = start of 68K RAM
		dbf	d5,.loop_ram2					; clear RAM ($0000-$FDFF)

		move.w	d1,(a3)						; stop the Z80 (we will clear the VSRAM and CRAM while waiting for it to stop)
		move.w	d1,Z80_reset-Z80_bus_request(a3)		; deassert Z80 reset (ZRES is held high on console reset until we clear it)

		move.w	(a0)+,(a6)					; set VDP increment to 2

		move.l	(a0)+,(a6)					; set VDP to VSRAM write
		moveq	#$14-1,d5					; set repeat times
		
.loop_vsram:
		move.l	d4,(a5)						; clear 4 bytes of VSRAM
		dbf	d5,.loop_vsram					; repeat until entire VSRAM has been cleared

		move.l	(a0)+,(a6)					; set VDP to CRAM write
		moveq	#$20-1,d5					; set repeat times
		
.loop_cram:
		move.l	d4,(a5)						; clear two palette entries
		dbf	d5,.loop_cram					; repeat until entire CRAM has been cleared

.waitz80:
		btst	d4,(a3)						; has the Z80 stopped?
		bne.s	.waitz80					; if not, branch

		move.w	#$2000-1,d5					; size of Z80 ram - 1
		
.clear_Z80_RAM:
		move.b 	d4,(a1)+					; clear the Z80 RAM
		dbf	d5,.clear_Z80_RAM
		
		moveq	#4-1,d5						; set number of PSG channels to mute
		
.psg_loop:
		move.b	(a0)+,PSG_input-VDP_data_port(a6)		; set the PSG channel volume to null (no sound)
		dbf	d5,.psg_loop					; repeat for all channels

		move.w	d4,d5						; clear d5
		
;.load_sound_driver:
		movem.w	d1/d2/d4,-(sp)					; back up these registers
		move.l	a3,-(sp)

		lea	(z80_SoundDriverStart).l,a0				; Load Z80 SMPS sound driver
		lea	(Z80_RAM).l,a1
		bsr	Kos_Decomp

		andi.b	#$C0,d6							; get region and speed settings
		btst	#6,d6							; are we on a PAL console?
		sne		zPalFlag(a1)					; if so, set the driver's PAL flag
		
		move.l	(sp)+,a3
		movem.w (sp)+,d1/d2/d4					; restore registers

		move.w	d4,Z80_reset-Z80_bus_request(a3)		; reset Z80
		
		move.b	d2,HW_Port_1_Control-Z80_bus_request(a3)	; initialise port 1
		move.b	d2,HW_Port_2_Control-Z80_bus_request(a3)	; initialise port 2
		move.b	d2,HW_Expansion_Control-Z80_bus_request(a3)	; initialise port e

		move.w	d1,Z80_reset-Z80_bus_request(a3)		; release Z80 reset
		move.w	d4,(a3)						; start the Z80
		
		move.w	#$4EF9,d0					; machine code for jmp
		move.w	d0,V_int_jump
		move.l	#VInt,V_int_addr
		move.w	d0,H_int_jump
		move.l	#HInt,H_int_addr

		bra	GameInitTrue
		
; ---------------------------------------------------------------------------	

SetupValues:
		dc.w	$2700						; disable interrupts
		dc.l	Z80_RAM
		dc.l	RAM_start
		dc.l	Z80_bus_request
		dc.l	VDP_data_port
		dc.l	VDP_control_port

		dc.w	$100						; VDP Reg increment value & opposite initialisation flag for Z80
		dc.w	$8004						; $8004; normal color mode, horizontal interrupts disabled
SetupVDP:
		dc.b	$8134&$FF					; $8134; mode 5, NTSC, vertical interrupts and DMA enabled 
		dc.b	($8200+($C000>>10))&$FF				; $8230; foreground nametable starts at $C000
		dc.b	($8300+($8000>>10))&$FF				; $833C; window nametable starts at $8000
		dc.b	($8400+($E000>>13))&$FF				; $8407; background nametable starts at $E000
		dc.b	($8500+($F800>>9))&$FF				; $856C; sprite attribute table starts at $F800
		dc.b	$8600&$FF					; $8600; unused (high bit of sprite attribute table for 128KB VRAM)
		dc.b	$8700&$FF					; $8700; background colour (palette line 0 color 0)
		dc.b	$8800&$FF					; $8800; unused (mode 4 hscroll register)
		dc.b	$8900&$FF					; $8900; unused (mode 4 vscroll register)
		dc.b	($8A00+0)&$FF					; $8A00; horizontal interrupt register (set to 0 for now)
		dc.b	$8B00&$FF					; $8B00; full-screen vertical/horizontal scrolling
		dc.b	$8C81&$FF					; $8C81; H40 display mode
		dc.b	($8D00+($F000>>10))&$FF				; $8D3F; hscroll table starts at $FC00
		dc.b	$8E00&$FF					; $8E00: unused (high bits of fg and bg nametable addresses for 128KB VRAM)
		dc.b	($8F00+1)&$FF					; $8F01; VDP increment size (will be changed to 2 later)
		dc.b	$9001&$FF					; $9001; 64x32 plane size
		dc.b	$9100&$FF					; $9100; unused (window horizontal position)
		dc.b	$9200&$FF					; $9200; unused (window vertical position)

		dc.w	$FFFF						; $93FF/$94FF - DMA length
		dc.w	0						; VDP $9500/9600 - DMA source
		dc.b	$9780&$FF					; VDP $9780 - DMA fill VRAM

		dc.b	$40						; I/O port initialization value
	   
SetupVDP_end:

		dc.l	vdpComm($0000,VRAM,DMA)				; DMA fill VRAM
		dc.l	System_stack					; start of RAM only cleared on cold boot
		dc.w	(($FFFFFFFF-$FFFFFE00+1)/4)-1			; loops to clear RAM cleared only on cold boot
		dc.w	((System_stack&$FFFF)/4)-1			; loops to clear RAM cleared on all boots
		dc.w	$8F00+2						; VDP increment
		dc.l	vdpComm($0000,VSRAM,WRITE)			; VSRAM write mode
		dc.l 	vdpComm($0000,CRAM,WRITE)			; CRAM write mode

		dc.b	$9F,$BF,$DF,$FF					; PSG mute values (PSG 1 to 4) 
		even

VInt:
		movem.l	d0-a6,-(sp)
-		move.w	(VDP_control_port).l,d0
		andi.w	#8,d0
		beq	-	; wait until vertical blanking is taking place
		move.b	#0,V_int_executing
		movem.l	(sp)+,d0-a6
HInt:
		rte

GameInitTrue:
		; delay moment
		wait	60

		music	mus_Default

		bra	*

; ---------------------------------------------------------------------------
; Called at the end of each frame to wait for vertical synchronization
; ---------------------------------------------------------------------------

Wait_VSync:
		move.b	#1,V_int_executing
		move	#$2300,sr
-
		tst.b	V_int_executing
		bne	-	; wait until V-int's run
		rts
; End of function Wait_VSync

; ---------------------------------------------------------------------------
; |||||||||||||||||||||| S U B R O U T I N E ||||||||||||||||||||||||||||||||
; ---------------------------------------------------------------------------
; Kosinski decompression subroutine that decompresses data right when
; it's run
;
; For format explanation see http://info.sonicretro.org/Kosinski_compression
; New faster version by written by vladikcomper, with additional improvements by
; MarkeyJester and Flamewing
; ---------------------------------------------------------------------------
_Kos_UseLUT := 1
_Kos_LoopUnroll := 3
_Kos_ExtremeUnrolling := 1

_Kos_RunBitStream macro
	dbra	d2,.skip
	moveq	#7,d2					; Set repeat count to 8.
	move.b	d1,d0					; Use the remaining 8 bits.
	not.w	d3						; Have all 16 bits been used up?
	bne.s	.skip					; Branch if not.
	move.b	(a0)+,d0				; Get desc field low-byte.
	move.b	(a0)+,d1				; Get desc field hi-byte.
	if _Kos_UseLUT==1
	move.b	(a4,d0.w),d0			; Invert bit order...
	move.b	(a4,d1.w),d1			; ... for both bytes.
	endif
.skip
	endm

_Kos_ReadBit macro
	if _Kos_UseLUT==1
	add.b	d0,d0					; Get a bit from the bitstream.
	else
	lsr.b	#1,d0					; Get a bit from the bitstream.
	endif
	endm
; ===========================================================================
; KozDec_193A:
Kos_Decomp:
	moveq	#(1<<_Kos_LoopUnroll)-1,d7
	if _Kos_UseLUT==1
	moveq	#0,d0
	moveq	#0,d1
	lea	KosDec_ByteMap(pc),a4		; Load LUT pointer.
	endif
	move.b	(a0)+,d0				; Get desc field low-byte.
	move.b	(a0)+,d1				; Get desc field hi-byte.
	if _Kos_UseLUT==1
	move.b	(a4,d0.w),d0			; Invert bit order...
	move.b	(a4,d1.w),d1			; ... for both bytes.
	endif
	moveq	#7,d2					; Set repeat count to 8.
	moveq	#0,d3					; d3 will be desc field switcher.
	bra.s	.FetchNewCode
; ---------------------------------------------------------------------------
.FetchCodeLoop:
	; Code 1 (Uncompressed byte).
	_Kos_RunBitStream
	move.b	(a0)+,(a1)+

.FetchNewCode:
	_Kos_ReadBit
	bcs.s	.FetchCodeLoop			; If code = 1, branch.

	; Codes 00 and 01.
	moveq	#-1,d5
	lea	(a1),a5
	_Kos_RunBitStream
	if _Kos_ExtremeUnrolling==1
	_Kos_ReadBit
	bcs.w	.Code_01

	; Code 00 (Dictionary ref. short).
	_Kos_RunBitStream
	_Kos_ReadBit
	bcs.s	.Copy45
	_Kos_RunBitStream
	_Kos_ReadBit
	bcs.s	.Copy3
	_Kos_RunBitStream
	move.b	(a0)+,d5				; d5 = displacement.
	adda.w	d5,a5
	move.b	(a5)+,(a1)+
	move.b	(a5)+,(a1)+
	bra.s	.FetchNewCode
; ---------------------------------------------------------------------------
.Copy3:
	_Kos_RunBitStream
	move.b	(a0)+,d5				; d5 = displacement.
	adda.w	d5,a5
	move.b	(a5)+,(a1)+
	move.b	(a5)+,(a1)+
	move.b	(a5)+,(a1)+
	bra.w	.FetchNewCode
; ---------------------------------------------------------------------------
.Copy45:
	_Kos_RunBitStream
	_Kos_ReadBit
	bcs.s	.Copy5
	_Kos_RunBitStream
	move.b	(a0)+,d5				; d5 = displacement.
	adda.w	d5,a5
	move.b	(a5)+,(a1)+
	move.b	(a5)+,(a1)+
	move.b	(a5)+,(a1)+
	move.b	(a5)+,(a1)+
	bra.w	.FetchNewCode
; ---------------------------------------------------------------------------
.Copy5:
	_Kos_RunBitStream
	move.b	(a0)+,d5				; d5 = displacement.
	adda.w	d5,a5
	move.b	(a5)+,(a1)+
	move.b	(a5)+,(a1)+
	move.b	(a5)+,(a1)+
	move.b	(a5)+,(a1)+
	move.b	(a5)+,(a1)+
	bra.w	.FetchNewCode
; ---------------------------------------------------------------------------
	else
	moveq	#0,d4					; d4 will contain copy count.
	_Kos_ReadBit
	bcs.s	.Code_01

	; Code 00 (Dictionary ref. short).
	_Kos_RunBitStream
	_Kos_ReadBit
	addx.w	d4,d4
	_Kos_RunBitStream
	_Kos_ReadBit
	addx.w	d4,d4
	_Kos_RunBitStream
	move.b	(a0)+,d5				; d5 = displacement.

.StreamCopy:
	adda.w	d5,a5
	move.b	(a5)+,(a1)+				; Do 1 extra copy (to compensate +1 to copy counter).

.copy:
	move.b	(a5)+,(a1)+
	dbra	d4,.copy
	bra.w	.FetchNewCode
	endif
; ---------------------------------------------------------------------------
.Code_01:
	moveq	#0,d4					; d4 will contain copy count.
	; Code 01 (Dictionary ref. long / special).
	_Kos_RunBitStream
	move.b	(a0)+,d6				; d6 = %LLLLLLLL.
	move.b	(a0)+,d4				; d4 = %HHHHHCCC.
	move.b	d4,d5					; d5 = %11111111 HHHHHCCC.
	lsl.w	#5,d5					; d5 = %111HHHHH CCC00000.
	move.b	d6,d5					; d5 = %111HHHHH LLLLLLLL.
	if _Kos_LoopUnroll==3
	and.w	d7,d4					; d4 = %00000CCC.
	else
	andi.w	#7,d4
	endif
	bne.s	.StreamCopy				; if CCC=0, branch.

	; special mode (extended counter)
	move.b	(a0)+,d4				; Read cnt
	beq.s	.Quit					; If cnt=0, quit decompression.
	subq.b	#1,d4
	beq.w	.FetchNewCode			; If cnt=1, fetch a new code.

	adda.w	d5,a5
	move.b	(a5)+,(a1)+				; Do 1 extra copy (to compensate +1 to copy counter).
	move.w	d4,d6
	not.w	d6
	and.w	d7,d6
	add.w	d6,d6
	lsr.w	#_Kos_LoopUnroll,d4
	jmp	.largecopy(pc,d6.w)
; ---------------------------------------------------------------------------
.largecopy:
	rept (1<<_Kos_LoopUnroll)
	move.b	(a5)+,(a1)+
	endm
	dbra	d4,.largecopy
	bra.w	.FetchNewCode
; ---------------------------------------------------------------------------
	if _Kos_ExtremeUnrolling==1
.StreamCopy:
	adda.w	d5,a5
	move.b	(a5)+,(a1)+				; Do 1 extra copy (to compensate +1 to copy counter).
	if _Kos_LoopUnroll==3
	eor.w	d7,d4
	else
	eori.w	#7,d4
	endif
	add.w	d4,d4
	jmp	.mediumcopy(pc,d4.w)
; ---------------------------------------------------------------------------
.mediumcopy:
	rept 8
	move.b	(a5)+,(a1)+
	endm
	bra.w	.FetchNewCode
	endif
; ---------------------------------------------------------------------------
.Quit:
	rts	
; End of function KosDec
; ===========================================================================
	if _Kos_UseLUT==1
KosDec_ByteMap:
	dc.b	$00,$80,$40,$C0,$20,$A0,$60,$E0,$10,$90,$50,$D0,$30,$B0,$70,$F0
	dc.b	$08,$88,$48,$C8,$28,$A8,$68,$E8,$18,$98,$58,$D8,$38,$B8,$78,$F8
	dc.b	$04,$84,$44,$C4,$24,$A4,$64,$E4,$14,$94,$54,$D4,$34,$B4,$74,$F4
	dc.b	$0C,$8C,$4C,$CC,$2C,$AC,$6C,$EC,$1C,$9C,$5C,$DC,$3C,$BC,$7C,$FC
	dc.b	$02,$82,$42,$C2,$22,$A2,$62,$E2,$12,$92,$52,$D2,$32,$B2,$72,$F2
	dc.b	$0A,$8A,$4A,$CA,$2A,$AA,$6A,$EA,$1A,$9A,$5A,$DA,$3A,$BA,$7A,$FA
	dc.b	$06,$86,$46,$C6,$26,$A6,$66,$E6,$16,$96,$56,$D6,$36,$B6,$76,$F6
	dc.b	$0E,$8E,$4E,$CE,$2E,$AE,$6E,$EE,$1E,$9E,$5E,$DE,$3E,$BE,$7E,$FE
	dc.b	$01,$81,$41,$C1,$21,$A1,$61,$E1,$11,$91,$51,$D1,$31,$B1,$71,$F1
	dc.b	$09,$89,$49,$C9,$29,$A9,$69,$E9,$19,$99,$59,$D9,$39,$B9,$79,$F9
	dc.b	$05,$85,$45,$C5,$25,$A5,$65,$E5,$15,$95,$55,$D5,$35,$B5,$75,$F5
	dc.b	$0D,$8D,$4D,$CD,$2D,$AD,$6D,$ED,$1D,$9D,$5D,$DD,$3D,$BD,$7D,$FD
	dc.b	$03,$83,$43,$C3,$23,$A3,$63,$E3,$13,$93,$53,$D3,$33,$B3,$73,$F3
	dc.b	$0B,$8B,$4B,$CB,$2B,$AB,$6B,$EB,$1B,$9B,$5B,$DB,$3B,$BB,$7B,$FB
	dc.b	$07,$87,$47,$C7,$27,$A7,$67,$E7,$17,$97,$57,$D7,$37,$B7,$77,$F7
	dc.b	$0F,$8F,$4F,$CF,$2F,$AF,$6F,$EF,$1F,$9F,$5F,$DF,$3F,$BF,$7F,$FF
	endif

; ===========================================================================

Size_of_Snd_driver_guess = $1200
		include "Sound/Flamedriver/Flamedriver.asm"

EndOfROM:	END
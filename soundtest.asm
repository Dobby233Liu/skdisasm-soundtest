; ---------------------------------------------------------------------------
		cpu 68000
		include "sonic3k.macrosetup.asm"	; include a few basic macros
		include "sonic3k.macros.asm"		; include some simplifying macros and functions
		include "sonic3k.constants.asm"		; include some constants
		org 0
; ---------------------------------------------------------------------------

add_padding = 0
Size_of_Snd_driver_guess = $1200
mus_Default	= $01
V_int_executing = V_int_routine

; ---------------------------------------------------------------------------

StartOfROM:
	if * <> 0
		fatal "StartOfROM was $\{*} but it should be 0"
	endif

Vectors:	dc.l	$00000000,	EntryPoint,	ErrorTrap,	ErrorTrap	; 0
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
Domestic_Name:	dc.b "SOUND TEST PROGRAM                              "
Overseas_Name:	dc.b "SOUND TEST PROGRAM                              "
Serial_Number:	dc.b "GM MK-IDFK -00"
Checksum:	dc.w $0000
Input:		dc.b "J               "
ROMStartLoc:	dc.l StartOfROM
ROMEndLoc:	dc.l EndOfROM-1
RAMStartLoc:	dc.l (RAM_start&$FFFFFF)
RAMEndLoc:	dc.l (RAM_start&$FFFFFF)+$FFFF
CartRAM_Info:	dc.b "  "
CartRAM_Type:	dc.w %10000000100000
CartRAMStartLoc:dc.l $20202020
CartRAMEndLoc:	dc.l $20202020
Modem_Info:	dc.b "  "
		dc.b "          "
Unknown_Header:	dc.w  0
		dc.b  "      "
		dc.w  0,  0
		dc.l  EndOfROM-1	; 0	;CHECKLATER (ROM Bank Info)
		dc.b  "        "
KiS2ROM_Info:	dc.b  "RO"
KiS2ROM_Type:	dc.w %10000000100000
KiS2ROMStartLoc:	tribyte $300000
KiS2ROMEndLoc:		tribyte $33FFFF
KiS2ROMStartLoc2:	tribyte $300000
KiS2ROMEndLoc2:		tribyte $33FFFF
Country_Code:	dc.b "JUE             "
; ---------------------------------------------------------------------------
SetupValues:	dc.w $8000,bytesToLcnt($10000),$100
		dc.l Z80_RAM
		dc.l Z80_bus_request
		dc.l Z80_reset
		dc.l VDP_data_port, VDP_control_port

; values for VDP registers
VDPInitValues:
		dc.b 4			; Command $8004 - HInt off, Enable HV counter read
		dc.b $14		; Command $8114 - Display off, VInt off, DMA on, PAL off
		dc.b $30		; Command $8230 - Scroll A Address $C000
		dc.b $3C		; Command $833C - Window Address $F000
		dc.b 7			; Command $8407 - Scroll B Address $E000
		dc.b $6C		; Command $856C - Sprite Table Address $D800
		dc.b 0			; Command $8600 - Null
		dc.b 0			; Command $8700 - Background color Pal 0 Color 0
		dc.b 0			; Command $8800 - Null
		dc.b 0			; Command $8900 - Null
		dc.b $FF		; Command $8AFF - Hint timing $FF scanlines
		dc.b 0			; Command $8B00 - Ext Int off, VScroll full, HScroll full
		dc.b $81		; Command $8C81 - 40 cell mode, shadow/highlight off, no interlace
		dc.b $37		; Command $8D37 - HScroll Table Address $DC00
		dc.b 0			; Command $8E00 - Null
		dc.b 1			; Command $8F01 - VDP auto increment 1 byte
		dc.b 1			; Command $9001 - 64x32 cell scroll size
		dc.b 0			; Command $9100 - Window H left side, Base Point 0
		dc.b 0			; Command $9200 - Window V upside, Base Point 0
		dc.b $FF		; Command $93FF - DMA Length Counter $FFFF
		dc.b $FF		; Command $94FF - See above
		dc.b 0			; Command $9500 - DMA Source Address $0
		dc.b 0			; Command $9600 - See above
		dc.b $80		; Command $9700	- See above + VRAM fill mode
VDPInitValues_End:
		dc.l	vdpComm($0000,VRAM,DMA)	; value for VRAM write mode

; Z80 instructions (not the sound driver; that gets loaded later)
Z80StartupCodeBegin:
	if (*)+$26 < $10000
	save
	CPU Z80	; start assembling Z80 code
	phase 0	; pretend we're at address 0
		xor	a	; clear a to 0
		ld	bc,((Z80_RAM_end-Z80_RAM)-zStartupCodeEndLoc)-1	; prepare to loop this many times
		ld	de,zStartupCodeEndLoc+1	; initial destination address
		ld	hl,zStartupCodeEndLoc	; initial source address
		ld	sp,hl	; set the address the stack starts at
		ld	(hl),a	; set first byte of the stack to 0
		ldir		; loop to fill the stack (entire remaining available Z80 RAM) with 0
		pop	ix	; clear ix
		pop	iy	; clear iy
		ld	i,a	; clear i
		ld	r,a	; clear r
		pop	de	; clear de
		pop	hl	; clear hl
		pop	af	; clear af
		ex	af,af'	; swap af with af'
		exx		; swap bc/de/hl with their shadow registers too
		pop	bc	; clear bc
		pop	de	; clear de
		pop	hl	; clear hl
		pop	af	; clear af
		ld	sp,hl	; clear sp
		di		; clear iff1 (for interrupt handler)
		im	1	; interrupt handling mode = 1
		ld	(hl),0E9h	; replace the first instruction with a jump to itself
		jp	(hl)		; jump to the first instruction (to stay there forever)
zStartupCodeEndLoc:
	dephase	; stop pretending
		restore
	padding off	; unfortunately our flags got reset so we have to set them again...
	else	; due to an address range limitation I could work around but don't think is worth doing so:
		message "Warning: using pre-assembled Z80 startup code."
		dc.w $AF01,$D91F,$1127,$0021,$2600,$F977,$EDB0,$DDE1,$FDE1,$ED47,$ED4F,$D1E1,$F108,$D9C1,$D1E1,$F1F9,$F3ED,$5636,$E9E9
	endif
Z80StartupCodeEnd:
		dc.w $8104			; value for VDP display mode
		dc.w $8F02			; value for VDP increment
		dc.l vdpComm($0000,CRAM,WRITE)	; value for CRAM write mode
		dc.l vdpComm($0000,VSRAM,WRITE)	; value for VSRAM write mode
PSGInitValues:	dc.b $9F,$BF,$DF,$FF		; values for PSG channel volumes
PSGInitValues_End:
VDP_register_values:
		dc.w $8004	; H-int disabled
		dc.w $8134	; V-int enabled, display blanked, DMA enabled, 224 line display
		dc.w $8230	; Scroll A PNT base $C000
		dc.w $8320	; Window PNT base $8000
		dc.w $8407	; Scroll B PNT base $E000
		dc.w $857C	; Sprite attribute table base $F800
		dc.w $8600
		dc.w $8700	; Backdrop color is color 0 of the first palette line
		dc.w $8800
		dc.w $8900
		dc.w $8A00
		dc.w $8B00	; Full-screen horizontal and vertical scrolling
		dc.w $8C81	; 40 cell wide display, no interlace
		dc.w $8D3C	; Horizontal scroll table base $F000
		dc.w $8E00
		dc.w $8F02	; Auto-ncrement is 2
		dc.w $9001	; Scroll planes are 64x32 cells
		dc.w $9100
		dc.w $9200	; Window disabled
; ---------------------------------------------------------------------------

; Trap for real unlike in SK
ErrorTrap:
		bra.s ErrorTrap

EntryPoint:
		lea	(System_stack).w,sp
		tst.l	(HW_Port_1_Control-1).l
		bne.s	+
		tst.w	(HW_Expansion_Control-1).l
+
		bne.s	.start	; in case of a soft reset
		lea	SetupValues(pc),a5
		movem.w	(a5)+,d5-d7
		movem.l	(a5)+,a0-a4
		move.b	HW_Version-Z80_bus_request(a1),d0	; get hardware version
		andi.b	#$F,d0
		beq.s	.initVDP	; branch if hardware is older than Genesis III
		move.l	#'SEGA',Security_addr-Z80_bus_request(a1)	; satisfy the TMSS

	.initVDP:
		move.w	(a4),d0	; check if VDP works
		moveq	#0,d0
		movea.l	d0,a6
		move.l	a6,usp	; set usp to $0
		moveq	#VDPInitValues_End-VDPInitValues-1,d1

	.initVDPLoop:
		move.b	(a5)+,d5
		move.w	d5,(a4)
		add.w	d7,d5
		dbf	d1,.initVDPLoop ; set all 24 registers

		move.l	(a5)+,(a4)	; set VRAM write mode
		move.w	d0,(a3)	; clear the screen
		move.w	d7,(a1)	; stop the Z80
		move.w	d7,(a2)	; reset the Z80

	.initZ80:
		btst	d0,(a1)	; has the Z80 stopped?
		bne.s	.initZ80	; if not, branch
		moveq	#Z80StartupCodeEnd-Z80StartupCodeBegin-1,d2

	.initZ80Loop:
		move.b	(a5)+,(a0)+
		dbf	d2,.initZ80Loop
		move.w	d0,(a2)
		move.w	d0,(a1)	; start the Z80
		move.w	d7,(a2)	; reset the Z80

	.clearRAM:
		move.l	d0,-(a6)		; Clear normal RAM
		dbf	d6,.clearRAM

		move.l	(a5)+,(a4)	; set VDP display mode and increment
		move.l	(a5)+,(a4)	; set VDP to CRAM write
		moveq	#bytesToLcnt($80),d3

	.clearCRAM:
		move.l	d0,(a3)			; Clear CRAM
		dbf	d3,.clearCRAM

		move.l	(a5)+,(a4)
		moveq	#bytesToLcnt($50),d4

	.clearVSRAM:
		move.l	d0,(a3)			; Clear VSRAM
		dbf	d4,.clearVSRAM

		moveq	#PSGInitValues_End-PSGInitValues-1,d5

	.initPSG:
		move.b	(a5)+,PSG_input-VDP_data_port(a3)	; reset the PSG
		dbf	d5,.initPSG

		move.w	d0,(a2)
		movem.l	(a6),d0-a6	; clear all registers
		move	#$2700,sr	; set the sr

	.start:
		tst.w	(VDP_control_port).l
		move.w	#$4EF9,(V_int_jump).w	; machine code for jmp
		move.l	#VInt,(V_int_addr).w
		move.w	#$4EF9,(H_int_jump).w
		move.l	#HInt,(H_int_addr).w
-
		move.w	(VDP_control_port).l,d1
		btst	#1,d1
		bne.s	-	; wait till a DMA is completed
		lea	((RAM_start&$FFFFFF)).l,a6
		moveq	#0,d7
		move.w	#bytesToLcnt($FE00),d6
-
		move.l	d7,(a6)+
		dbf	d6,-

		moveq	#0,d1


GameStartup:

Init_VDP:
		lea	(VDP_control_port).l,a0
		lea	(VDP_data_port).l,a1
		lea	(VDP_register_values).l,a2
		moveq	#18,d7

$$setRegisters:
		move.w	(a2)+,(a0)
		dbf	d7,$$setRegisters
		move.w	(VDP_register_values+2).l,d0	; get command for register #1
		move.w	d0,(VDP_reg_1_command).w	; and store it in RAM (for easy display blanking/enabling)
		move.w	#$8ADF,(H_int_counter_command).w
		moveq	#0,d0
		move.l	#vdpComm($0000,VSRAM,WRITE),(VDP_control_port).l
		move.w	d0,(a1)
		move.w	d0,(a1)
		move.l	#vdpComm($0000,CRAM,WRITE),(VDP_control_port).l
		move.w	#$3F,d7

$$clearCRAM:
		move.w	d0,(a1)
		dbf	d7,$$clearCRAM
		clr.l	(V_scroll_value).w
		clr.l	(_unkF61A).w
		move.l	d1,-(sp)
		dmaFillVRAM 0,$0000,$10000	; clear entire VRAM
		move.l	(sp)+,d1
; End of function Init_VDP

SndDrvInit:
		nop
		move.w	#$100,(Z80_bus_request).l
		move.w	#$100,(Z80_reset).l	; release Z80 reset

		; Load SMPS sound driver
		lea	(Z80_SoundDriver).l,a0
		lea	(Z80_RAM).l,a1
		bsr.w	Kos_Decomp
		; Load default variables
		moveq	#0,d1
		lea	(Z80_RAM+z80_stack).l,a1
		move.w	#bytesToXcnt(zTracksStart-z80_stack, 8),d0
-
		movep.l	d1,0(a1)
		movep.l	d1,1(a1)
		addq.w	#8,a1
		dbf	d0,-
		; Detect PAL region consoles
		btst	#6,(Graphics_flags).w
		beq.s	+
		move.b	#1,(Z80_RAM+zPalFlag).l
+
		move.w	#0,(Z80_reset).l	; reset Z80
		nop
		nop
		nop
		nop
		move.w	#$100,(Z80_reset).l	; release reset
		startZ80
; End of function SndDrvInit

GameInitTrue:
	; delay moment
		move.w	#$40,d0
	.wait1:
		bsr.w	Wait_VSync
		dbf	d0,.wait1

		move.w	#signextendB(mus_Default),d0
		bsr.w	Play_Music

GameLoop:
		bra.s	GameLoop

; ---------------------------------------------------------------------------

; ---------------------------------------------------------------------------
; Called at the end of each frame to perform vertical synchronization
; ---------------------------------------------------------------------------

; =============== S U B R O U T I N E =======================================


Wait_VSync:
		move.b	#1,(V_int_executing).w
		move	#$2300,sr
-
		tst.b	(V_int_executing).w
		bne.s	-	; wait until V-int's run
		rts
; End of function Wait_VSync

; ---------------------------------------------------------------------------
; Vertical interrupt handler
; ---------------------------------------------------------------------------

VInt:
		movem.l	d0-a6,-(sp)
-
		move.w	(VDP_control_port).l,d0
		andi.w	#8,d0
		beq.s	-	; wait until vertical blanking is taking place
		move.b	#0,(V_int_executing).w
		movem.l	(sp)+,d0-a6
		rte

HInt:
		rte

; ---------------------------------------------------------------------------
; Always replaces an index previous passed to this function
; ---------------------------------------------------------------------------

; =============== S U B R O U T I N E =======================================


Play_Music:
		stopZ80
		move.b	d0,(Z80_RAM+zMusicNumber).l
		startZ80
		rts
; End of function Play_Music

; ---------------------------------------------------------------------------
; Can handle up to two different indexes in one frame
; ---------------------------------------------------------------------------

; =============== S U B R O U T I N E =======================================


Play_SFX:
		stopZ80
		cmp.b	(Z80_RAM+zSFXNumber0).l,d0
		beq.s	++
		tst.b	(Z80_RAM+zSFXNumber0).l
		bne.s	+
		move.b	d0,(Z80_RAM+zSFXNumber0).l
		startZ80
		rts
+
		move.b	d0,(Z80_RAM+zSFXNumber1).l
+
		startZ80

Play_SFX_Done:
		rts
; End of function Play_SFX


; =============== S U B R O U T I N E =======================================


Change_Music_Tempo:
		stopZ80
		move.b	d0,(Z80_RAM+zTempoSpeedup).l
		startZ80
		rts
; End of function Change_Music_Tempo

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
KosDec:
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

; ---------------------------------------------------------------------------

	; sound driver
	include "Sound/Flamedriver/Flamedriver.asm"
	align $8000

; ---------------------------------------------------------------------------

	; End-of-ROM padding stuff
	if add_padding
	if (*)&(*-1)
		cnop -1,2<<lastbit(*)
		dc.b $FF ; AS would automatically strip this padding if we didn't specifically declare one byte at the end
	else
		even
	endif
	else
		even
	endif

EndOfROM:
		END
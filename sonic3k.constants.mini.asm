; ---------------------------------------------------------------------------
; Address equates
; ---------------------------------------------------------------------------

; Z80 addresses
Z80_RAM =   $A00000

; I/O Area
HW_Version =    $A10001
HW_Port_1_Control = $A10009
HW_Port_2_Control = $A1000B
HW_Expansion_Control =  $A1000D
Z80_bus_request =   $A11100
Z80_reset = $A11200
Security_addr = $A14000

; VDP addresses
VDP_data_port = $C00000
VDP_control_port =  $C00004
PSG_input = $C00011

; RAM addresses
	phase $FFFF0000
RAM_start =			*

Stack_contents			ds.b $100		; stack contents
System_stack =			*			; this is the top of the stack, it grows downwards

V_int_jump              ds.b 6			; contains an instruction to jump to the V-int handler
V_int_addr :=			V_int_jump+2		; long

H_int_jump              ds.b 6			; contains an instruction to jump to the H-int handler
H_int_addr :=			H_int_jump+2		; long

V_int_executing			ds.b 1

RAM_allocated_end =	(*)&$FFFFFF
	if (RAM_allocated_end>0)&(RAM_allocated_end<$FF0000)
		fatal "RAM definitions are too large by $\{*} bytes!"
	endif
	dephase

; ---------------------------------------------------------------------------
; Sound ID equates
; ---------------------------------------------------------------------------

	phase $01
mus__First =			*		; ID of the first music
mus_Default			ds.b 1		; $01
mus__End =			*		; next ID after last music
	dephase

; ---------------------------------------------------------------------------

	!org 0				; make sure we reset the ROM position to 0
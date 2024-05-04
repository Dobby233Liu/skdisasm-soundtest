incspdat macro file,fileid

	include	file
	even
	dc		fileid
	even

	endm

	if (1==0)
	else
		incspdat	"Sound/Flamedriver/sk/disasm/Menu (Sonic & Knuckles).asm",	"sk/disasm/Menu (Sonic & Knuckles)"
	endif
	;include "Sound/Flamedriver/sk/music-optimized/LBZ2.asm"
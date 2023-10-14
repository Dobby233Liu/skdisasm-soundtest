incspdat macro file,fileid

	include	file
	even
	dc		fileid
	even

	endm

	if (1==0)
	else
		incspdat	"Sound/Flamedriver/s3d/music-improved/The Final Fight.asm","s3d/music-improved/The Final Fight"
	endif
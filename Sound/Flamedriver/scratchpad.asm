Snd_LBZ1_Header:
	smpsHeaderStartSong 3
	smpsHeaderVoice     Snd_LBZ_Voices
	smpsHeaderChan      $06, $03
	smpsHeaderTempo     $01, $2F

	smpsHeaderDAC       Snd_LBZ1_DAC
	smpsHeaderFM        DummyCh,	$00, $0C
	smpsHeaderFM        Snd_LBZ1_FM2,	$0C, $08
	smpsHeaderFM        DummyCh,	$00, $0C
	smpsHeaderFM        DummyCh,	$00, $0C
	smpsHeaderFM        DummyCh,	$0C, $0C
	smpsHeaderPSG       DummyCh,	$F4, $02, $00, $00
	smpsHeaderPSG       DummyCh,	$F4, $04, $00, $00
	smpsHeaderPSG       DummyCh,	$00, $02, $00, $00

DummyCh: smpsStop

; FM2 Data
Snd_LBZ1_FM2:
	smpsSetvoice        $00
	smpsCall            Snd_LBZ1_Call06
	smpsLoop            $00, $04, Snd_LBZ1_FM2

Snd_LBZ1_Loop0A:
	smpsCall            Snd_LBZ1_Call06
	smpsLoop            $00, $03, Snd_LBZ1_Loop0A
	dc.b	nRst, $42, nD2, $06, nG2, $0C, nD2
	smpsCall            Snd_LBZ1_Call07
	smpsCall            Snd_LBZ1_Call08
	smpsCall            Snd_LBZ1_Call07
	smpsCall            Snd_LBZ1_Call07

Snd_LBZ1_Loop0B:
	smpsCall            Snd_LBZ1_Call06
	smpsLoop            $00, $08, Snd_LBZ1_Loop0B
	smpsCall            Snd_LBZ1_Call07
	smpsCall            Snd_LBZ1_Call08
	smpsCall            Snd_LBZ1_Call07
	smpsCall            Snd_LBZ1_Call07
	dc.b	nC2, nC1, nC3, nRst, $4E
	smpsJump            Snd_LBZ1_Loop0A

Snd_LBZ1_Call06:
	dc.b	nG1, $12, nD2, nA2, $1E, nD2, $06, nG2, $0C, nD2
	smpsReturn

Snd_LBZ1_Call07:
	dc.b	nC2, $06, nRst, nC2, $0C, nBb1, $12, nA1, $06, nRst, $2A, nA1
	dc.b	$06, nF1, nRst, nF1, $0C, nFs1, $12, nG1, $06, nRst, $18, nD2
	dc.b	$0C, nG2, $06, nD2
	smpsReturn

Snd_LBZ1_Call08:
	dc.b	nC2, nRst, nC2, $0C, nBb1, $12, nA1, $06, nRst, $2A, nA1, $06
	dc.b	nF1, nRst, nF1, $0C, nFs1, $12, nG1, $06, nRst, $18, nD2
	smpsReturn

; DAC Data
Snd_LBZ1_DAC:
	dc.b	dModLooseKick, $12, nRst, dModLooseKick, $3C

Snd_LBZ1_Loop00:
	dc.b	dModLooseKick, $06, dHiHitDrum, nRst, dHiHitDrum, dLowHitDrum, nRst, dModLooseKick, dHiHitDrum, nRst, dHiHitDrum, dLowHitDrum
	dc.b	dPowerTom, dPowerTom, $18
	smpsLoop            $00, $02, Snd_LBZ1_Loop00
	dc.b	dModLooseKick, $0C, dHiWoodBlock, $06, dModLooseKick, dLowWoodBlock, dLowWoodBlock, dModLooseKick, dHiWoodBlock, nRst, dHiWoodBlock, dLowWoodBlock
	dc.b	dLowWoodBlock, dLowWoodBlock, dLowWoodBlock, dLowWoodBlock, dLowWoodBlock

Snd_LBZ1_Jump00:
	smpsCall            Snd_LBZ1_Call00
	dc.b	dModLooseKick, $06, dHiHitDrum, nRst, dModLooseKick, dSnareGo, nRst, dModLooseKick, dHiHitDrum, nRst, dHiHitDrum, dLowHitDrum
	dc.b	dSnareGo, dSnareGo, nRst, dLowWoodBlock, dLowWoodBlock
	smpsCall            Snd_LBZ1_Call00
	dc.b	dModLooseKick, $06, nRst, dHiWoodBlock, nRst, dLowWoodBlock, dLowWoodBlock, dLowWoodBlock, dHiWoodBlock, nRst, nRst, $2A
	dc.b	dModLooseKick, $0C, dModLooseKick, dSnareGo, $12, dModLooseKick, $1E, dMetalCrashHit, $18
	smpsCall            Snd_LBZ1_Call01

Snd_LBZ1_Loop01:
	dc.b	dModLooseKick, $0C, dModLooseKick, dSnareGo, $12, dModLooseKick, $1E, dSnareGo, $24
	smpsCall            Snd_LBZ1_Call02
	smpsLoop            $00, $02, Snd_LBZ1_Loop01
	dc.b	dModLooseKick, $0C, dModLooseKick, dSnareGo, $12, dModLooseKick, $1E, dSnareGo, $18, nRst, $36, dQuietGlassCrash
	dc.b	$12, dMetalCrashHit, $0B, dMetalCrashHit, $0D

Snd_LBZ1_Loop02:
	dc.b	dModLooseKick, $06, dHiHitDrum, nRst, dModLooseKick, dSnareGo, nRst, dModLooseKick, dHiHitDrum, nRst, dHiHitDrum, dLowHitDrum
	dc.b	dPowerTom, dSnareGo, nRst, dLowWoodBlock, dLowWoodBlock, dModLooseKick, nRst, dHiWoodBlock, dModLooseKick, dSnareGo, nRst, dModLooseKick
	dc.b	dHiWoodBlock, nRst, dHiWoodBlock, dLowWoodBlock, dSnareGo, dSnareGo, nRst, dLowWoodBlock, dLowWoodBlock
	smpsLoop            $00, $03, Snd_LBZ1_Loop02
	dc.b	dModLooseKick, $06, dHiHitDrum, nRst, dModLooseKick, dSnareGo, nRst, dModLooseKick, dHiHitDrum, nRst, dHiHitDrum, dLowHitDrum
	dc.b	dPowerTom, dSnareGo, nRst, dLowWoodBlock, dLowWoodBlock, dModLooseKick, nRst, dHiWoodBlock, dModLooseKick, dSnareGo, nRst, dModLooseKick
	dc.b	dHiWoodBlock, nRst, dSnareGo, nRst, dSnareGo, dSnareGo, nRst, dSnareGo, nRst, dModLooseKick, $06, nRst
	dc.b	dModLooseKick, nRst, dSnareGo, nRst, nRst, dModLooseKick, nRst, nRst, dGo, nRst, dSnareGo, nRst
	dc.b	dGo, nRst, dModLooseKick, nRst, dGo, nRst, dSnareGo, nRst, dGo, dModLooseKick, dModLooseKick, nRst
	dc.b	dGo, dSnareGo, dSnareGo, nRst, dGo, nRst

Snd_LBZ1_Loop03:
	dc.b	dModLooseKick, $06, nRst, dGo, nRst, dSnareGo, nRst, dGo, dModLooseKick, dModLooseKick, nRst, dGo
	dc.b	nRst, dSnareGo, nRst, dGo, dModLooseKick, dModLooseKick, nRst, dGo, nRst, dSnareGo, nRst, dGo
	dc.b	dModLooseKick, dModLooseKick, nRst, dGo, dSnareGo, dSnareGo, nRst, dGo, nRst
	smpsLoop            $00, $02, Snd_LBZ1_Loop03
	dc.b	dModLooseKick, $06, nRst, dGo, nRst, dSnareGo, nRst, dGo, dModLooseKick, dModLooseKick, nRst, dGo
	dc.b	nRst, dSnareGo, nRst, dGo, dModLooseKick, dModLooseKick, nRst, dGo, nRst, dSnareGo, nRst, dGo
	dc.b	dModLooseKick, dModLooseKick, dSnareGo, dGo, dSnareGo, dSnareGo, nRst, dSnareGo, nRst, dModLooseKick, dModLooseKick, dSnareGo
	dc.b	$54
	smpsJump            Snd_LBZ1_Jump00

Snd_LBZ1_Call00:
	dc.b	dModLooseKick, $06, dHiHitDrum, nRst, dHiHitDrum, dSnareGo, nRst, dModLooseKick, dHiHitDrum, nRst, dHiHitDrum, dLowHitDrum
	dc.b	dPowerTom, dSnareGo, nRst, dLowWoodBlock, dLowWoodBlock
	smpsReturn

Snd_LBZ1_Call01:
	dc.b	dModLooseKick, $18, dSnareGo, $0C, dScratchS3, dPowerTom, $12, dSnareGo, $06, dSnareGo, $0C, dScratchS3
	smpsReturn

Snd_LBZ1_Call02:
	dc.b	dModLooseKick, $0C, dSnareGo, $12, dModLooseKick, $06, dPowerTom, $12, dSnareGo, $06, dSnareGo, $18
	smpsReturn

Snd_LBZ_Voices:
;	Voice $00
;	$03
;	$62, $40, $44, $31, 	$1F, $1F, $1F, $1C, 	$0B, $0A, $02, $01
;	$08, $0B, $04, $06, 	$1F, $1F, $1F, $1F, 	$2A, $1A, $2B, $80
	smpsVcAlgorithm     $03
	smpsVcFeedback      $00
	smpsVcUnusedBits    $00
	smpsVcDetune        $03, $04, $04, $06
	smpsVcCoarseFreq    $01, $04, $00, $02
	smpsVcRateScale     $00, $00, $00, $00
	smpsVcAttackRate    $1C, $1F, $1F, $1F
	smpsVcAmpMod        $00, $00, $00, $00
	smpsVcDecayRate1    $01, $02, $0A, $0B
	smpsVcDecayRate2    $06, $04, $0B, $08
	smpsVcDecayLevel    $01, $01, $01, $01
	smpsVcReleaseRate   $0F, $0F, $0F, $0F
	smpsVcTotalLevel    $80, $2B, $1A, $2A
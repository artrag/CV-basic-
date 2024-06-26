	' TMSColor 2.2.1 Mar/29/2024
	' Command: ..\tmscolor -z -t -b -o result.bmp ScrollTest.bmp ScrollTest.bas 
	' Created: Mon May 13 14:38:54 2024

	' Display image.
	MODE 0
	DEFINE CHAR PLETTER 0,231,image_char
	DEFINE COLOR PLETTER 0,231,image_color

    SCREEN image_pattern,0,0,32,16,64
	#X = 0
	WHILE (1)
		WAIT
		SCREEN image_pattern,#X/4+64*5+64*19*((#X) and 3),32*5,32,19,64
		IF CONT.RIGHT THEN #X = (#X + 1) and 127
		IF CONT.LEFT  THEN #X = (#X - 1) and 127
	WEND
	


image_char:
	DATA BYTE $30,$00,$00,$30,$48,$40,$1c,$21
	DATA BYTE $11,$00,$00,$ee,$30,$08,$08,$05
	DATA BYTE $01,$01,$18,$71,$89,$89,$07,$11
	DATA BYTE $1b,$10,$11,$11,$1b,$17,$16,$19
	DATA BYTE $0e,$07,$1e,$24,$24,$07,$8d,$21
	DATA BYTE $02,$03,$06,$07,$cf,$00,$07,$c6
	DATA BYTE $1f,$99,$91,$8d,$07,$c7,$21,$1f
	DATA BYTE $44,$88,$48,$48,$a0,$0f,$70,$88
	DATA BYTE $f8,$c0,$05,$18,$20,$79,$22,$6c
	DATA BYTE $22,$37,$00,$d8,$00,$e0,$10,$74
	DATA BYTE $f0,$07,$3d,$08,$60,$0c,$05,$20
	DATA BYTE $20,$2c,$6c,$32,$1f,$38,$34,$44
	DATA BYTE $44,$26,$69,$73,$7f,$0d,$00,$ce
	DATA BYTE $11,$1f,$ac,$17,$22,$05,$0e,$40
	DATA BYTE $40,$79,$45,$1f,$86,$00,$10,$10
	DATA BYTE $a0,$07,$05,$21,$21,$51,$51,$30
	DATA BYTE $27,$e7,$a6,$70,$e1,$8d,$08,$98
	DATA BYTE $00,$0f,$07,$04,$1c,$84,$8a,$8a
	DATA BYTE $af,$11,$0e,$11,$36,$27,$00,$e0
	DATA BYTE $00,$09,$49,$30,$e4,$06,$78,$22
	DATA BYTE $e8,$d2,$0f,$61,$69,$71,$07,$9b
	DATA BYTE $3b,$c8,$07,$78,$ac,$70,$1c,$8b
	DATA BYTE $83,$b4,$02,$e2,$03,$88,$c8,$48
	DATA BYTE $87,$8b,$07,$5f,$91,$00,$90,$5a
	DATA BYTE $05,$e1,$27,$62,$04,$08,$0f,$e1
	DATA BYTE $1c,$88,$17,$3d,$1c,$80,$80,$78
	DATA BYTE $07,$f0,$86,$e1,$21,$93,$47,$1f
	DATA BYTE $c4,$63,$ea,$c1,$eb,$10,$e0,$ea
	DATA BYTE $03,$1c,$01,$0e,$00,$40,$22,$d9
	DATA BYTE $8a,$43,$40,$e9,$38,$90,$07,$08
	DATA BYTE $87,$70,$f1,$27,$cf,$a2,$07,$20
	DATA BYTE $00,$d2,$07,$1f,$e8,$57,$00,$a0
	DATA BYTE $41,$38,$41,$80,$07,$f9,$05,$68
	DATA BYTE $05,$0f,$21,$f0,$af,$69,$09,$aa
	DATA BYTE $43,$00,$1f,$a0,$a0,$63,$17,$91
	DATA BYTE $8e,$60,$37,$45,$45,$79,$41,$b4
	DATA BYTE $00,$bb,$6c,$53,$8e,$41,$1c,$20
	DATA BYTE $30,$0c,$04,$80,$6b,$70,$80,$c0
	DATA BYTE $30,$10,$64,$e0,$57,$58,$48,$c4
	DATA BYTE $e6,$84,$88,$08,$88,$89,$88,$84
	DATA BYTE $e6,$4b,$48,$b2,$c8,$89,$cb,$12
	DATA BYTE $c4,$53,$1a,$17,$57,$09,$19,$89
	DATA BYTE $d9,$63,$40,$ca,$8b,$ce,$ef,$03
	DATA BYTE $8f,$3c,$20,$86,$93,$40,$d3,$00
	DATA BYTE $07,$f3,$92,$26,$33,$4f,$80,$64
	DATA BYTE $92,$0e,$81,$07,$e3,$ee,$44,$64
	DATA BYTE $24,$c3,$07,$b9,$62,$22,$00,$a1
	DATA BYTE $e0,$13,$c4,$81,$c9,$83,$40,$41
	DATA BYTE $b0,$2f,$88,$eb,$8c,$4c,$88,$00
	DATA BYTE $68,$8d,$81,$91,$9f,$90,$90,$8f
	DATA BYTE $88,$88,$04,$06,$01,$ed,$b6,$8e
	DATA BYTE $13,$9d,$0f,$0e,$22,$bd,$88,$8f
	DATA BYTE $1f,$69,$87,$1f,$65,$11,$1f,$8b
	DATA BYTE $f5,$48,$3f,$e4,$b7,$01,$00,$03
	DATA BYTE $07,$0f,$1f,$3f,$7f,$fe,$ff,$f2
	DATA BYTE $00,$c5,$00,$e0,$f0,$f8,$fc,$fe
	DATA BYTE $7f,$f8,$17,$fc,$f8,$0f,$f0,$e0
	DATA BYTE $c0,$80,$a0,$1d,$00,$3f,$1f,$0f
	DATA BYTE $07,$9d,$eb,$6d,$27,$4c,$17,$09
	DATA BYTE $4e,$00,$14,$c0,$07,$1e,$7f,$3f
	DATA BYTE $00,$7d,$15,$07,$55,$00,$2a,$03
	DATA BYTE $4c,$76,$0f,$07,$6a,$74,$73,$00
	DATA BYTE $23,$00,$1e,$26,$c0,$2d,$00,$1f
	DATA BYTE $3c,$3e,$00,$37,$5a,$f8,$34,$3c
	DATA BYTE $00,$3f,$00,$79,$fc,$00,$a9,$77
	DATA BYTE $c8,$2b,$80,$8b,$db,$6e,$39,$5f
	DATA BYTE $1b,$01,$00,$bb,$be,$d8,$e3,$e6
	DATA BYTE $83,$1a,$61,$ed,$bd,$61,$2c,$0f
	DATA BYTE $01,$7f,$f1,$b9,$b5,$08,$07,$e1
	DATA BYTE $da,$a8,$e1,$39,$aa,$41,$aa,$e8
	DATA BYTE $95,$ae,$03,$1b,$98,$6a,$54,$37
	DATA BYTE $13,$d8,$f1,$fe,$95,$5e,$fe,$7c
	DATA BYTE $57,$cf,$6a,$19,$9f,$11,$c7,$eb
	DATA BYTE $3c,$07,$01,$97,$1b,$fc,$99,$24
	DATA BYTE $4a,$97,$44,$17,$63,$07,$6c,$0f
	DATA BYTE $e7,$1a,$0f,$99,$00,$24,$e5,$4b
	DATA BYTE $08,$ed,$57,$b7,$4a,$ca,$f8,$e0
	DATA BYTE $44,$6f,$f3,$57,$00,$28,$13,$5e
	DATA BYTE $03,$34,$77,$03,$00,$d6,$dd,$1d
	DATA BYTE $c3,$23,$47,$6f,$c3,$10,$12,$fe
	DATA BYTE $fd,$02,$fb,$63,$8f,$07,$9b,$0f
	DATA BYTE $87,$b7,$f7,$1d,$a4,$00,$1b,$ca
	DATA BYTE $3d,$a9,$2e,$de,$08,$cf,$a5,$39
	DATA BYTE $1f,$02,$16,$06,$3e,$10,$a8,$af
	DATA BYTE $7e,$ad,$64,$0d,$0f,$23,$f8,$e5
	DATA BYTE $6c,$f0,$3f,$dc,$17,$b1,$ff,$76
	DATA BYTE $c3,$5e,$d9,$07,$27,$a7,$67,$c3
	DATA BYTE $ef,$f0,$ce,$00,$8b,$7e,$93,$6c
	DATA BYTE $6b,$9f,$7b,$00,$73,$24,$32,$52
	DATA BYTE $4c,$17,$ec,$07,$74,$7d,$3f,$1a
	DATA BYTE $9e,$1c,$5b,$43,$08,$cb,$4f,$01
	DATA BYTE $44,$45,$97,$80,$37,$34,$cf,$ae
	DATA BYTE $00,$20,$ae,$38,$aa,$db,$03,$a8
	DATA BYTE $23,$77,$bb,$ea,$1b,$57,$cb,$00
	DATA BYTE $fb,$fd,$fe,$fe,$f8,$f6,$90,$9e
	DATA BYTE $ef,$8f,$3f,$6f,$83,$a7,$df,$df
	DATA BYTE $ae,$e3,$d3,$3a,$ec,$e6,$a1,$31
	DATA BYTE $d9,$a1,$db,$07,$f9,$a5,$e7,$27
	DATA BYTE $0a,$27,$1e,$1a,$10,$a0,$37,$1e
	DATA BYTE $a8,$19,$a5,$00,$3f,$76,$23,$cb
	DATA BYTE $fe,$5d,$c0,$47,$2b,$cf,$a9,$cf
	DATA BYTE $a5,$76,$5e,$22,$95,$1f,$67,$30
	DATA BYTE $e3,$00,$c0,$ce,$00,$f3,$3e,$fb
	DATA BYTE $2c,$63,$87,$7b,$00,$71,$24,$ff
	DATA BYTE $70,$4a,$80,$ff,$ee,$07,$6c,$fb
	DATA BYTE $1a,$3c,$1c,$43,$bb,$08,$0d,$e7
	DATA BYTE $9f,$e5,$57,$07,$4c,$a3,$97,$f7
	DATA BYTE $aa,$3c,$3f,$ff,$36,$ba,$43,$03
	DATA BYTE $34,$db,$03,$a8,$23,$7f,$9b,$d3
	DATA BYTE $23,$0e,$d3,$23,$e0,$67,$ef,$f6
	DATA BYTE $f8,$f9,$25,$e0,$d8,$ae,$bf,$22
	DATA BYTE $23,$bf,$f7,$00,$fb,$17,$ae,$e3
	DATA BYTE $b9,$6d,$10,$9d,$b9,$d9,$ad,$e7
	DATA BYTE $1f,$2a,$3c,$1a,$6a,$17,$7f,$a0
	DATA BYTE $ff,$ff,$ff,$e0

image_color:
	DATA BYTE $3b,$f1,$af,$f8,$00,$51,$68,$71
	DATA BYTE $00,$75,$d2,$07,$0d,$7c,$51,$0f
	DATA BYTE $71,$f3,$07,$6b,$00,$17,$ef,$00
	DATA BYTE $be,$0f,$1f,$02,$91,$b1,$f1,$b1
	DATA BYTE $b1,$91,$00,$0c,$e5,$b5,$a1,$61
	DATA BYTE $3e,$16,$54,$fe,$eb,$0e,$0d,$09
	DATA BYTE $41,$f1,$e1,$86,$07,$81,$91,$81
	DATA BYTE $1f,$00,$61,$81,$7d,$17,$27,$f2
	DATA BYTE $00,$61,$0d,$a7,$01,$16,$41,$d3
	DATA BYTE $0f,$5b,$f5,$4d,$07,$8e,$8c,$6f
	DATA BYTE $b5,$0f,$e2,$00,$71,$41,$1b,$ee
	DATA BYTE $03,$0b,$ee,$06,$2b,$ad,$74,$48
	DATA BYTE $ee,$0f,$00,$fa,$0f,$04,$db,$00
	DATA BYTE $ef,$7a,$b7,$07,$34,$17,$07,$d9
	DATA BYTE $95,$b4,$7d,$e0,$17,$f7,$7d,$dd
	DATA BYTE $07,$13,$df,$00,$cf,$95,$c0,$0b
	DATA BYTE $95,$b5,$f5,$b5,$b5,$5f,$95,$00
	DATA BYTE $07,$07,$51,$b5,$a5,$65,$03,$1f
	DATA BYTE $e1,$eb,$ba,$b6,$a1,$00,$97,$f8
	DATA BYTE $e9,$b8,$b8,$0f,$a8,$86,$61,$84
	DATA BYTE $9d,$07,$fe,$17,$ce,$8f,$df,$27
	DATA BYTE $1a,$86,$8a,$87,$65,$15,$cd,$17
	DATA BYTE $6f,$07,$96,$1f,$51,$75,$ce,$a7
	DATA BYTE $f8,$9f,$b0,$75,$d9,$de,$03,$ef
	DATA BYTE $14,$98,$8c,$dc,$d6,$fc,$1f,$51
	DATA BYTE $9f,$00,$1f,$f9,$3f,$91,$3e,$bb
	DATA BYTE $ce,$a5,$b7,$1f,$db,$07,$3a,$ad
	DATA BYTE $f3,$af,$6b,$17,$df,$c3,$df,$07
	DATA BYTE $7f,$17,$3f,$b7,$3f,$8f,$3a,$9f
	DATA BYTE $fc,$b7,$ef,$cf,$af,$ce,$9f,$bf
	DATA BYTE $3f,$a7,$7f,$07,$7c,$00,$eb,$7c
	DATA BYTE $8d,$ed,$15,$f7,$2a,$f3,$a5,$af
	DATA BYTE $33,$a7,$8c,$bf,$37,$a7,$1b,$ce
	DATA BYTE $a5,$e7,$d7,$de,$00,$b8,$9f,$fd
	DATA BYTE $ef,$fd,$ff,$ef,$cf,$9f,$df,$00
	DATA BYTE $cf,$a7,$df,$d7,$7e,$e7,$e2,$a7
	DATA BYTE $54,$fc,$cf,$f5,$4f,$00,$be,$16
	DATA BYTE $b7,$fe,$6e,$dd,$9d,$ad,$59,$af
	DATA BYTE $ff,$ff,$ff,$ff,$c0

image_pattern:
	DATA BYTE $00,$01,$02,$03,$04,$05,$06,$07
	DATA BYTE $08,$09,$0a,$0b,$0c,$09,$0d,$0e
	DATA BYTE $0f,$10,$11,$12,$13,$14,$15,$16
	DATA BYTE $17,$18,$19,$1a,$1b,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1d,$1e,$1f,$20,$21,$22,$23,$24
	DATA BYTE $25,$26,$27,$28,$29,$2a,$2b,$2c
	DATA BYTE $2d,$2e,$2f,$30,$31,$32,$33,$34
	DATA BYTE $35,$36,$37,$38,$39,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $3a,$3b,$3c,$3d,$3e,$3f,$40,$41
	DATA BYTE $42,$43,$44,$45,$46,$47,$48,$49
	DATA BYTE $4a,$4b,$4c,$4d,$4e,$4f,$50,$51
	DATA BYTE $52,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $53,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$54,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$55,$56,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$54,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$55,$56,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$57,$58,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$5a,$5b,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$57,$58,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$5a,$5b,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$57,$58,$59,$59,$59
	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DATA BYTE $59,$59,$59,$5a,$5b,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$57,$58,$59,$59,$59
	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DATA BYTE $59,$59,$59,$5a,$5b,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$57,$58,$59,$59,$5d,$5e
	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DATA BYTE $61,$62,$59,$59,$5a,$5b,$1c,$1c
	DATA BYTE $1c,$1c,$57,$58,$59,$59,$5d,$5e
	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DATA BYTE $61,$62,$59,$59,$5a,$5b,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $66,$67,$67,$67,$67,$67,$67,$67
	DATA BYTE $67,$67,$67,$68,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $66,$67,$67,$67,$67,$67,$67,$67
	DATA BYTE $67,$67,$67,$68,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $69,$59,$59,$59,$59,$5c,$5c,$59
	DATA BYTE $59,$59,$59,$6a,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $69,$59,$59,$59,$59,$5c,$5c,$59
	DATA BYTE $59,$59,$59,$6a,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $69,$59,$5d,$5e,$5f,$60,$60,$5f
	DATA BYTE $61,$62,$59,$6a,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $69,$59,$5d,$5e,$5f,$60,$60,$5f
	DATA BYTE $61,$62,$59,$6a,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$66,$67,$67,$67
	DATA BYTE $67,$67,$67,$67,$67,$67,$68,$59
	DATA BYTE $69,$59,$59,$59,$59,$64,$64,$59
	DATA BYTE $59,$59,$59,$6a,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$66,$67,$67,$67
	DATA BYTE $67,$67,$67,$67,$67,$67,$68,$59
	DATA BYTE $69,$59,$59,$59,$59,$64,$64,$59
	DATA BYTE $59,$59,$59,$6a,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$69,$59,$66,$67
	DATA BYTE $68,$59,$6b,$6b,$6b,$59,$6a,$59
	DATA BYTE $6c,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$6e,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$69,$59,$66,$67
	DATA BYTE $68,$59,$6b,$6b,$6b,$59,$6a,$59
	DATA BYTE $6c,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$6e,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$69,$59,$69,$6f
	DATA BYTE $6a,$59,$6b,$6b,$6b,$59,$6a,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$69,$59,$69,$6f
	DATA BYTE $6a,$59,$6b,$6b,$6b,$59,$6a,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$69,$59,$6c,$6d
	DATA BYTE $70,$59,$6b,$6b,$6b,$59,$6a,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$69,$59,$6c,$6d
	DATA BYTE $70,$59,$6b,$6b,$6b,$59,$6a,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$6c,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6e,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$6c,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6e,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DATA BYTE $1c,$1c,$71,$72,$59,$59,$59,$59
	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DATA BYTE $59,$59,$59,$59,$73,$74,$1c,$1c
	DATA BYTE $1c,$1c,$71,$72,$59,$59,$59,$59
	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DATA BYTE $59,$59,$59,$59,$73,$74,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$71,$72,$59,$5d,$5e
	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DATA BYTE $61,$62,$59,$73,$74,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$71,$72,$59,$5d,$5e
	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DATA BYTE $61,$62,$59,$73,$74,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$71,$72,$59,$59
	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DATA BYTE $59,$59,$73,$74,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$71,$72,$59,$59
	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DATA BYTE $59,$59,$73,$74,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$71,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$75,$74,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$1c,$71,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$75,$74,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$76,$77,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$78,$79,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$76,$77,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$78,$79,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$76,$7a,$7b,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$7c,$79,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$76,$7a,$7b,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$7c,$79,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$76,$7a,$7b,$59,$59,$59
	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DATA BYTE $59,$59,$59,$7c,$79,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$76,$7a,$7b,$59,$59,$59
	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DATA BYTE $59,$59,$59,$7c,$79,$1c,$1c,$1c
	DATA BYTE $1c,$76,$7a,$7b,$59,$7f,$80,$5f
	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DATA BYTE $83,$84,$59,$59,$7c,$79,$1c,$1c
	DATA BYTE $1c,$76,$7a,$7b,$59,$7f,$80,$5f
	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DATA BYTE $83,$84,$59,$59,$7c,$79,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8a
	DATA BYTE $8b,$67,$67,$67,$67,$67,$67,$67
	DATA BYTE $67,$67,$67,$8c,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8a
	DATA BYTE $8b,$67,$67,$67,$67,$67,$67,$67
	DATA BYTE $67,$67,$67,$8c,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$6a
	DATA BYTE $59,$59,$59,$59,$7d,$5c,$7e,$59
	DATA BYTE $59,$59,$59,$8d,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$6a
	DATA BYTE $59,$59,$59,$59,$7d,$5c,$7e,$59
	DATA BYTE $59,$59,$59,$8d,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$6a
	DATA BYTE $59,$7f,$80,$5f,$81,$60,$82,$5f
	DATA BYTE $83,$84,$59,$8d,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$6a
	DATA BYTE $59,$7f,$80,$5f,$81,$60,$82,$5f
	DATA BYTE $83,$84,$59,$8d,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$8a,$8b,$67,$67,$67
	DATA BYTE $67,$67,$67,$67,$67,$67,$8c,$6a
	DATA BYTE $59,$59,$59,$59,$87,$64,$88,$59
	DATA BYTE $59,$59,$59,$8d,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$8a,$8b,$67,$67,$67
	DATA BYTE $67,$67,$67,$67,$67,$67,$8c,$6a
	DATA BYTE $59,$59,$59,$59,$87,$64,$88,$59
	DATA BYTE $59,$59,$59,$8d,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$6a,$59,$8a,$8b,$67
	DATA BYTE $8c,$8e,$6b,$6b,$8f,$59,$8d,$90
	DATA BYTE $91,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$92,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$6a,$59,$8a,$8b,$67
	DATA BYTE $8c,$8e,$6b,$6b,$8f,$59,$8d,$90
	DATA BYTE $91,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$92,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$6a,$59,$6a,$93,$94
	DATA BYTE $8d,$8e,$6b,$6b,$8f,$59,$8d,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$6a,$59,$6a,$93,$94
	DATA BYTE $8d,$8e,$6b,$6b,$8f,$59,$8d,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$6a,$59,$90,$91,$6d
	DATA BYTE $92,$8e,$6b,$6b,$8f,$59,$8d,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$6a,$59,$90,$91,$6d
	DATA BYTE $92,$8e,$6b,$6b,$8f,$59,$8d,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$90,$91,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$92,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$90,$91,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$92,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DATA BYTE $1c,$95,$96,$97,$59,$59,$59,$59
	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DATA BYTE $59,$59,$59,$59,$98,$99,$1c,$1c
	DATA BYTE $1c,$95,$96,$97,$59,$59,$59,$59
	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DATA BYTE $59,$59,$59,$59,$98,$99,$1c,$1c
	DATA BYTE $1c,$1c,$95,$96,$97,$7f,$80,$5f
	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DATA BYTE $83,$84,$59,$98,$99,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$95,$96,$97,$7f,$80,$5f
	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DATA BYTE $83,$84,$59,$98,$99,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$95,$96,$97,$59,$59
	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DATA BYTE $59,$59,$98,$99,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$95,$96,$97,$59,$59
	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DATA BYTE $59,$59,$98,$99,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$95,$9a,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$9b,$99,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$95,$9a,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$9b,$99,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$9c,$9d,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$9e,$9f,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$9c,$9d,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$9e,$9f,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$9c,$a0,$a1,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$a2,$a3,$9f,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$9c,$a0,$a1,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$a2,$a3,$9f,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$9c,$a0,$a1,$59,$59,$59
	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DATA BYTE $59,$59,$a2,$a3,$9f,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$9c,$a0,$a1,$59,$59,$59
	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DATA BYTE $59,$59,$a2,$a3,$9f,$1c,$1c,$1c
	DATA BYTE $1c,$9c,$a0,$a1,$59,$a6,$a7,$5f
	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DATA BYTE $aa,$ab,$59,$a2,$a3,$9f,$1c,$1c
	DATA BYTE $1c,$9c,$a0,$a1,$59,$a6,$a7,$5f
	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DATA BYTE $aa,$ab,$59,$a2,$a3,$9f,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b0
	DATA BYTE $67,$67,$67,$67,$67,$67,$67,$67
	DATA BYTE $67,$67,$67,$b1,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b0
	DATA BYTE $67,$67,$67,$67,$67,$67,$67,$67
	DATA BYTE $67,$67,$67,$b1,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8d
	DATA BYTE $59,$59,$59,$59,$a4,$5c,$a5,$59
	DATA BYTE $59,$59,$59,$b2,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8d
	DATA BYTE $59,$59,$59,$59,$a4,$5c,$a5,$59
	DATA BYTE $59,$59,$59,$b2,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8d
	DATA BYTE $59,$a6,$a7,$5f,$a8,$60,$a9,$5f
	DATA BYTE $aa,$ab,$59,$b2,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8d
	DATA BYTE $59,$a6,$a7,$5f,$a8,$60,$a9,$5f
	DATA BYTE $aa,$ab,$59,$b2,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$b0,$67,$67,$67,$67
	DATA BYTE $67,$67,$67,$67,$67,$67,$b1,$8d
	DATA BYTE $59,$59,$59,$59,$ad,$64,$ae,$59
	DATA BYTE $59,$59,$59,$b2,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$b0,$67,$67,$67,$67
	DATA BYTE $67,$67,$67,$67,$67,$67,$b1,$8d
	DATA BYTE $59,$59,$59,$59,$ad,$64,$ae,$59
	DATA BYTE $59,$59,$59,$b2,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$8d,$59,$b0,$67,$67
	DATA BYTE $b1,$b3,$6b,$6b,$b4,$59,$b2,$b5
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$b6,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$8d,$59,$b0,$67,$67
	DATA BYTE $b1,$b3,$6b,$6b,$b4,$59,$b2,$b5
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$b6,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$8d,$59,$8d,$b7,$b8
	DATA BYTE $b2,$b3,$6b,$6b,$b4,$59,$b2,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$8d,$59,$8d,$b7,$b8
	DATA BYTE $b2,$b3,$6b,$6b,$b4,$59,$b2,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$8d,$59,$b5,$6d,$6d
	DATA BYTE $b6,$b3,$6b,$6b,$b4,$59,$b2,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$8d,$59,$b5,$6d,$6d
	DATA BYTE $b6,$b3,$6b,$6b,$b4,$59,$b2,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$b5,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$b6,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$b5,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$b6,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DATA BYTE $1c,$b9,$ba,$bb,$59,$59,$59,$59
	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DATA BYTE $59,$59,$59,$bc,$bd,$be,$1c,$1c
	DATA BYTE $1c,$b9,$ba,$bb,$59,$59,$59,$59
	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DATA BYTE $59,$59,$59,$bc,$bd,$be,$1c,$1c
	DATA BYTE $1c,$1c,$b9,$ba,$bb,$a6,$a7,$5f
	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DATA BYTE $aa,$ab,$bc,$bd,$be,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$b9,$ba,$bb,$a6,$a7,$5f
	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DATA BYTE $aa,$ab,$bc,$bd,$be,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$b9,$ba,$bb,$59,$59
	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DATA BYTE $59,$bc,$bd,$be,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$b9,$ba,$bb,$59,$59
	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DATA BYTE $59,$bc,$bd,$be,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$b9,$bf,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$c0,$be,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$b9,$bf,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$c0,$be,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$c1,$c2,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$c3,$c4,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$c1,$c2,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DATA BYTE $55,$c3,$c4,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$c1,$c5,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$c6,$c7,$c4,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$c1,$c5,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$c6,$c7,$c4,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$c1,$c5,$59,$59,$59,$59
	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DATA BYTE $59,$59,$c6,$c7,$c4,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$c1,$c5,$59,$59,$59,$59
	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DATA BYTE $59,$59,$c6,$c7,$c4,$1c,$1c,$1c
	DATA BYTE $1c,$c1,$c5,$59,$59,$ca,$cb,$5f
	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DATA BYTE $ce,$cf,$59,$c6,$c7,$c4,$1c,$1c
	DATA BYTE $1c,$c1,$c5,$59,$59,$ca,$cb,$5f
	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DATA BYTE $ce,$cf,$59,$c6,$c7,$c4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$d5
	DATA BYTE $67,$67,$67,$67,$67,$67,$67,$67
	DATA BYTE $67,$67,$d6,$d7,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$d5
	DATA BYTE $67,$67,$67,$67,$67,$67,$67,$67
	DATA BYTE $67,$67,$d6,$d7,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b2
	DATA BYTE $59,$59,$59,$59,$c8,$5c,$c9,$59
	DATA BYTE $59,$59,$59,$69,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b2
	DATA BYTE $59,$59,$59,$59,$c8,$5c,$c9,$59
	DATA BYTE $59,$59,$59,$69,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b2
	DATA BYTE $59,$ca,$cb,$5f,$cc,$60,$cd,$5f
	DATA BYTE $ce,$cf,$59,$69,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b2
	DATA BYTE $59,$ca,$cb,$5f,$cc,$60,$cd,$5f
	DATA BYTE $ce,$cf,$59,$69,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$d5,$67,$67,$67,$67
	DATA BYTE $67,$67,$67,$67,$67,$d6,$d7,$b2
	DATA BYTE $59,$59,$59,$59,$d1,$64,$d2,$59
	DATA BYTE $59,$59,$59,$69,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$d5,$67,$67,$67,$67
	DATA BYTE $67,$67,$67,$67,$67,$d6,$d7,$b2
	DATA BYTE $59,$59,$59,$59,$d1,$64,$d2,$59
	DATA BYTE $59,$59,$59,$69,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$b2,$59,$d5,$67,$d6
	DATA BYTE $d7,$d8,$6b,$6b,$d9,$59,$69,$da
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$db,$dc,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$b2,$59,$d5,$67,$d6
	DATA BYTE $d7,$d8,$6b,$6b,$d9,$59,$69,$da
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$db,$dc,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$b2,$59,$b2,$dd,$de
	DATA BYTE $69,$d8,$6b,$6b,$d9,$59,$69,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$b2,$59,$b2,$dd,$de
	DATA BYTE $69,$d8,$6b,$6b,$d9,$59,$69,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$b2,$59,$da,$6d,$db
	DATA BYTE $df,$d8,$6b,$6b,$d9,$59,$69,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$b2,$59,$da,$6d,$db
	DATA BYTE $df,$d8,$6b,$6b,$d9,$59,$69,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$da,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$db,$dc,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$da,$6d,$6d,$6d,$6d
	DATA BYTE $6d,$6d,$6d,$6d,$6d,$db,$dc,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DATA BYTE $1c,$e0,$e1,$59,$59,$59,$59,$59
	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DATA BYTE $59,$59,$59,$e2,$e3,$e4,$1c,$1c
	DATA BYTE $1c,$e0,$e1,$59,$59,$59,$59,$59
	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DATA BYTE $59,$59,$59,$e2,$e3,$e4,$1c,$1c
	DATA BYTE $1c,$1c,$e0,$e1,$59,$ca,$cb,$5f
	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DATA BYTE $ce,$cf,$e2,$e3,$e4,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$e0,$e1,$59,$ca,$cb,$5f
	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DATA BYTE $ce,$cf,$e2,$e3,$e4,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$e0,$e1,$59,$59,$59
	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DATA BYTE $59,$e2,$e3,$e4,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$e0,$e1,$59,$59,$59
	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DATA BYTE $59,$e2,$e3,$e4,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$e0,$e5,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$e6,$e4,$1c,$1c,$1c,$1c,$1c
	DATA BYTE $1c,$1c,$1c,$1c,$e0,$e5,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DATA BYTE $75,$e6,$e4,$1c,$1c,$1c,$1c,$1c

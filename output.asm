COLECO:	equ 0
SG1000:	equ 0
MSX:	equ 1
SGM:	equ 0
CVBASIC_MUSIC_PLAYER:	equ 0
CVBASIC_COMPRESSION:	equ 1
CVBASIC_BANK_SWITCHING:	equ 0
	;
	; CVBasic prologue (BASIC compiler for Colecovision)
	;
	; by Oscar Toledo G.
	; https://nanochess.org/
	;
	; Creation date: Feb/27/2024.
	; Revision date: Feb/29/2024. Turns off sound. Now it reads the controllers.
	;                             Added 16-bit multiply, division, modulo, and abs.
	;                             Added random generator. Added sound routines.
	; Revision date: Mar/03/2024. Removed fname directive to use gasm80.
	; Revision date: Mar/05/2024. Added support for Sega SG1000.
	; Revision date: Mar/06/2024. Added ENASCR, DISSCR, and CPYBLK.
	; Revision date: Mar/08/2024. Added modes 0, 1 and 2.
	; Revision date: Mar/12/2024. Added support for MSX.
	; Revision date: Mar/14/2024. Added _sgn16.
	; Revision date: Mar/15/2024. Added upper 16k enable for MSX.
	; Revision date: Apr/11/2024. Added support for formatting numbers. Added
	;                             support for Super Game Module.
	; Revision date: Apr/13/2024. Saved bytes in SG-1000 ROMs. Faster LDIRVM.
	;                             Shorter mode setting subroutines.
	; Revision date: Apr/26/2024. Interruption handler saves current bank.
	; Revision date: Apr/27/2024. Music player now supports bank switching.
	;

VDP:    equ $98+$26*COLECO+$26*SG1000
JOYSEL:	equ $c0
KEYSEL:	equ $80

PSG:    equ $ff-$80*SG1000
JOY1:   equ $fc-$20*SG1000
JOY2:   equ $ff-$22*SG1000

BASE_RAM: equ $e000-$7000*COLECO-$2000*SG1000+$0c00*SGM

STACK:	equ $f000-$7c00*COLECO-$2c00*SG1000+$0c00*SGM

    if COLECO
	org $8000
	db $55,$aa
	dw 0
	dw 0
	dw 0
	dw 0
	dw START

	jp 0	; rst $08
	jp 0	; rst $10
	jp 0	; rst $18
	jp 0	; rst $20
	jp 0	; rst $28
	jp 0	; rst $30
	jp 0	; rst $38

	jp nmi_handler
    endif
    if SG1000
	org $0000
	di
	im 1
	jp START
	db $ff,$ff
	jp 0
	db $ff,$ff,$ff,$ff,$ff
	jp 0
	db $ff,$ff,$ff,$ff,$ff
	jp 0
	db $ff,$ff,$ff,$ff,$ff
	jp 0
	db $ff,$ff,$ff,$ff,$ff
	jp 0
	db $ff,$ff,$ff,$ff,$ff
	jp 0
	db $ff,$ff,$ff,$ff,$ff
	jp nmi_handler	; It should be called int_handler.
    endif
    if MSX
	ORG $4000
	db "AB"
	dw START
	dw $0000
	dw $0000
	dw $0000
	dw $0000
    endif

WRTVDP:
	ld a,b
	out (VDP+1),a
	ld a,c
	or $80
	out (VDP+1),a
	ret

SETWRT:
	ld a,l
	out (VDP+1),a
	ld a,h
	or $40
	out (VDP+1),a
	ret

SETRD:
	ld a,l
	out (VDP+1),a
	ld a,h
        and $3f
	out (VDP+1),a
	ret

WRTVRM:
	push af
	call SETWRT
	pop af
	out (VDP),a
	ret

    if SG1000
	db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

	; Located at $0066
	ei		; NMI handler (pause button)
	retn
    endif

RDVRM:
        push af
        call SETRD
        pop af
        ex (sp),hl
        ex (sp),hl
        in a,(VDP)
        ret

FILVRM:
	push af
	call SETWRT
.1:	pop af
	out (VDP),a
	push af
	dec bc
	ld a,b
	or c
	jp nz,.1
	pop af
	ret

LDIRVM:
        EX DE,HL
        CALL SETWRT
        EX DE,HL
        DEC BC
        INC C
        LD A,B
        LD B,C
        INC A
        LD C,VDP
.1:
    if SG1000
	NOP	; SG1000 is faster (reported by SiRioKD)
    endif
	OUTI
        JP NZ,.1
        DEC A
        JP NZ,.1
        RET

LDIRVM3:
	call .1
	call .1
.1:	push hl
	push de
	push bc
	call LDIRVM
	pop bc
	pop de
	ld a,d
	add a,8
	ld d,a
	pop hl
	ret

DISSCR:
	call nmi_off
	ld bc,$a201
	call WRTVDP
	jp nmi_on

ENASCR:
	call nmi_off
	ld bc,$e201
	call WRTVDP
	jp nmi_on

CPYBLK:
	pop hl
	ex af,af'
	pop af
	ld b,a
	pop af
	ld c,a
	pop de
	ex (sp),hl
	call nmi_off
.1:	push bc
	push hl
	push de
	ld b,0
	call LDIRVM
	pop hl
	ld bc,$0020
	add hl,bc
	ex de,hl
	pop hl
	ex af,af'
	ld c,a
	ld b,0
	add hl,bc
	ex af,af'
	pop bc
	djnz .1
	jp nmi_on
	
nmi_off:
    if COLECO
	push hl
	ld hl,mode
	set 0,(hl)
	pop hl
    endif
    if SG1000+MSX
        di
    endif
	ret

nmi_on:
    if COLECO
	push af
	push hl
	ld hl,mode
	res 0,(hl)
	nop
	bit 1,(hl)
	jp nz,nmi_handler.0
	pop hl
	pop af
    endif
    if SG1000+MSX
        ei
    endif
	ret

    if COLECO
keypad_table:
        db $0f,$08,$04,$05,$0c,$07,$0a,$02
        db $0d,$0b,$00,$09,$03,$01,$06,$0f
    endif

cls:
	ld hl,$1800
	ld (cursor),hl
	ld bc,$0300
	ld a,$20
	call nmi_off
	call FILVRM
	jp nmi_on

print_string:
	ld c,a
	ld b,0
	ld de,(cursor)
	ld a,d
	and $03
	or $18
	ld d,a
	push de
	push bc
	call nmi_off
	call LDIRVM
	call nmi_on
	pop bc
	pop hl
	add hl,bc
	ld (cursor),hl
	ret

print_number:
	ld b,0
	call nmi_off
print_number5:
	ld de,10000
	call print_digit
print_number4:
	ld de,1000
	call print_digit
print_number3:
	ld de,100
	call print_digit
print_number2:
	ld de,10
	call print_digit
print_number1:
	ld de,1
	ld b,e
	call print_digit
	jp nmi_on

print_digit:
	ld a,$2f
	or a
.2:	inc a
	sbc hl,de
	jp nc,.2
	add hl,de
	cp $30
	jr nz,.3
	ld a,b
	or a
	ret z
	dec a
	jr z,.4
	ld a,c
	jr .5	
.4:
	ld a,$30
.3:	ld b,1
.5:	push hl
	ld hl,(cursor)
	ex af,af'
	ld a,h
	and $03
	or $18
	ld h,a
	ex af,af'
	call WRTVRM
	inc hl
	ld (cursor),hl
	pop hl
	ret

define_sprite:
	ex de,hl
	ld l,a
	ld h,0
	add hl,hl	; x2
	add hl,hl	; x4
	add hl,hl	; x8
	add hl,hl	; x16
	add hl,hl	; x32
	ld c,l
	ld b,h
	pop af
	pop hl
	push af
	add hl,hl	; x2
	add hl,hl	; x4
	ld h,$07
	add hl,hl	; x8
	add hl,hl	; x16
	add hl,hl	; x32
	ex de,hl
	call nmi_off
	call LDIRVM
	jp nmi_on
	
define_char:
	ex de,hl
	ld l,a
	ld h,0
	add hl,hl	; x2
	add hl,hl	; x4
	add hl,hl	; x8
	ld c,l
	ld b,h
	pop af
	pop hl
	push af
	add hl,hl	; x2
	add hl,hl	; x4
	add hl,hl	; x8
	ex de,hl
	call nmi_off
	ld a,(mode)
	and 4
	jr nz,.1
	call LDIRVM3
	jp nmi_on
	
.1:	call LDIRVM
	jp nmi_on

define_color:
	ex de,hl
	ld l,a
	ld h,0
	add hl,hl	; x2
	add hl,hl	; x4
	add hl,hl	; x8
	ld c,l
	ld b,h
	pop af
	pop hl
	push af
	add hl,hl	; x2
	add hl,hl	; x4
	add hl,hl	; x8
	ex de,hl
	set 5,d
	call nmi_off
	call LDIRVM3
	jp nmi_on
	
update_sprite:
	pop bc
	ld (sprite_data+3),a
	pop af
	ld (sprite_data+2),a
	pop af
	ld (sprite_data+1),a
	pop af
	ld (sprite_data),a
	pop af
	push bc
	ld de,sprites
	add a,a
	add a,a
	ld e,a
	ld hl,sprite_data
	ld bc,4
	ldir
	ret

	; Fast 16-bit multiplication.
_mul16:
	ld b,h
	ld c,l
	ld a,16
	ld hl,0
.1:
	srl d
	rr e
	jr nc,.2
	add hl,bc
.2:	sla c
	rl b
	dec a
	jp nz,.1
	ret

	; Fast 16-bit division.
_div16:
	ld b,h
	ld c,l
	ld hl,0
	ld a,16
.1:
	rl c
	rl b
	adc hl,hl
	sbc hl,de
	jp nc,.2	
	add hl,de
.2:
	ccf
	dec a
	jp nz,.1
	rl c
	rl b
	ld h,b
	ld l,c
	ret

	; Fast 16-bit modulo.
_mod16:
	ld b,h
	ld c,l
	ld hl,0
	ld a,16
.1:
	rl c
	rl b
	adc hl,hl
	sbc hl,de
	jp nc,.2	
	add hl,de
.2:
	ccf
	dec a
	jp nz,.1
	ret

_abs16:
	bit 7,h
	ret z
	ld a,h
	cpl
	ld h,a
	ld a,l
	cpl
	ld l,a
	inc hl
	ret

_sgn16:
	ld a,h
	or l
	ret z
	bit 7,h
	ld hl,$ffff
	ret nz
	inc hl
	inc hl
	ret

	; Random number generator.
	; From my game Mecha Eight.
random:
        ld hl,(lfsr)
        ld a,h
        or l
        jr nz,.0
        ld hl,$7811
.0:     ld a,h
        and $80
        ld b,a
        ld a,h
        and $02
        rrca
        rrca
        xor b
        ld b,a
        ld a,h
        and $01
        rrca
        xor b
        ld b,a
        ld a,l
        and $20
        rlca
        rlca
        xor b
        rlca
        rr h
        rr l
        ld (lfsr),hl
        ret

sn76489_freq:
    if COLECO+SG1000
	ld b,a
	ld a,l
	and $0f
	or b
	out (PSG),a
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld a,h
	and $3f
	out (PSG),a
    endif
	ret

sn76489_vol:
    if COLECO+SG1000
	cpl
	and $0f
	or b
	out (PSG),a
    endif
	ret

sn76489_control:
    if COLECO+SG1000
	and $0f
	or $e0
	out (PSG),a
    endif
	ret

ay3_reg:
    if COLECO
	push af
	ld a,b
	out ($50),a
	pop af
	out ($51),a
	ret
    endif
    if SG1000
        ret
    endif
    if MSX
	ld e,a
	ld a,b
	jp WRTPSG
    endif

ay3_freq:
    if COLECO
	out ($50),a
	push af
	ld a,l
	out ($51),a
	pop af
	inc a
	out ($50),a
	push af
	ld a,h
	and $0f
	out ($51),a
	pop af
	ret
    endif
    if SG1000
	ret
    endif
    if MSX
	ld e,l
	call WRTPSG
	ld e,h
	inc a
	jp WRTPSG
    endif

    if SG1000
	; Required for SG1000 as it doesn't have a BIOS
	;
        ; My personal font for TMS9928.
        ;
        ; Patterned after the TMS9928 programming manual 6x8 letters
        ; with better lowercase letters, also I made a proper
        ; AT sign.
        ;
font_bitmaps:
        db $00,$00,$00,$00,$00,$00,$00,$00      ; $20 space
        db $20,$20,$20,$20,$20,$00,$20,$00      ; $21 !
        db $50,$50,$50,$00,$00,$00,$00,$00      ; $22 "
        db $50,$50,$f8,$50,$f8,$50,$50,$00      ; $23 #
        db $20,$78,$a0,$70,$28,$f0,$20,$00      ; $24 $
        db $c0,$c8,$10,$20,$40,$98,$18,$00      ; $25 %
        db $40,$a0,$40,$a0,$a8,$90,$68,$00      ; $26 &
        db $60,$20,$40,$00,$00,$00,$00,$00      ; $27 '
        db $10,$20,$40,$40,$40,$20,$10,$00      ; $28 (
        db $40,$20,$10,$10,$10,$20,$40,$00      ; $29 )
        db $00,$a8,$70,$20,$70,$a8,$00,$00      ; $2a *
        db $00,$20,$20,$f8,$20,$20,$00,$00      ; $2b +
        db $00,$00,$00,$00,$00,$60,$20,$40      ; $2c ,
        db $00,$00,$00,$fc,$00,$00,$00,$00      ; $2d -
        db $00,$00,$00,$00,$00,$00,$60,$00      ; $2e .
        db $00,$08,$10,$20,$40,$80,$00,$00      ; $2f /
        db $70,$88,$98,$a8,$c8,$88,$70,$00      ; $30 0
        db $20,$60,$20,$20,$20,$20,$f8,$00      ; $31 1
        db $70,$88,$08,$10,$60,$80,$f8,$00      ; $32 2
        db $70,$88,$08,$30,$08,$88,$70,$00      ; $33 3
        db $30,$50,$90,$90,$f8,$10,$10,$00      ; $34 4
        db $f8,$80,$f0,$08,$08,$08,$f0,$00      ; $35 5
        db $30,$40,$80,$f0,$88,$88,$70,$00      ; $36 6
        db $f8,$08,$10,$20,$20,$20,$20,$00      ; $37 7
        db $70,$88,$88,$70,$88,$88,$70,$00      ; $38 8
        db $70,$88,$88,$78,$08,$10,$60,$00      ; $39 9
        db $00,$00,$00,$60,$00,$60,$00,$00      ; $3a :
        db $00,$00,$00,$60,$00,$60,$20,$40      ; $3b ;
        db $10,$20,$40,$80,$40,$20,$10,$00      ; $3c <
        db $00,$00,$f8,$00,$f8,$00,$00,$00      ; $3d =
        db $08,$04,$02,$01,$02,$04,$08,$00      ; $3e >
        db $70,$88,$08,$10,$20,$00,$20,$00      ; $3f ?
        db $70,$88,$98,$a8,$98,$80,$70,$00      ; $40 @
        db $20,$50,$88,$88,$f8,$88,$88,$00      ; $41 A
        db $f0,$88,$88,$f0,$88,$88,$f0,$00      ; $42 B
        db $70,$88,$80,$80,$80,$88,$70,$00      ; $43 C
        db $f0,$88,$88,$88,$88,$88,$f0,$00      ; $44 D
        db $f8,$80,$80,$f0,$80,$80,$f8,$00      ; $45 E
        db $f8,$80,$80,$f0,$80,$80,$80,$00      ; $46 F
        db $70,$88,$80,$b8,$88,$88,$70,$00      ; $47 G
        db $88,$88,$88,$f8,$88,$88,$88,$00      ; $48 H
        db $70,$20,$20,$20,$20,$20,$70,$00      ; $49 I
        db $08,$08,$08,$08,$88,$88,$70,$00      ; $4A J
        db $88,$90,$a0,$c0,$a0,$90,$88,$00      ; $4B K
        db $80,$80,$80,$80,$80,$80,$f8,$00      ; $4C L
        db $88,$d8,$a8,$a8,$88,$88,$88,$00      ; $4D M
        db $88,$c8,$c8,$a8,$98,$98,$88,$00      ; $4E N
        db $70,$88,$88,$88,$88,$88,$70,$00      ; $4F O
        db $f0,$88,$88,$f0,$80,$80,$80,$00      ; $50 P
        db $70,$88,$88,$88,$88,$a8,$90,$68      ; $51 Q
        db $f0,$88,$88,$f0,$a0,$90,$88,$00      ; $52 R
        db $70,$88,$80,$70,$08,$88,$70,$00      ; $53 S
        db $f8,$20,$20,$20,$20,$20,$20,$00      ; $54 T
        db $88,$88,$88,$88,$88,$88,$70,$00      ; $55 U
        db $88,$88,$88,$88,$50,$50,$20,$00      ; $56 V
        db $88,$88,$88,$a8,$a8,$d8,$88,$00      ; $57 W
        db $88,$88,$50,$20,$50,$88,$88,$00      ; $58 X
        db $88,$88,$88,$70,$20,$20,$20,$00      ; $59 Y
        db $f8,$08,$10,$20,$40,$80,$f8,$00      ; $5A Z
        db $78,$60,$60,$60,$60,$60,$78,$00      ; $5B [
        db $00,$80,$40,$20,$10,$08,$00,$00      ; $5C \
        db $F0,$30,$30,$30,$30,$30,$F0,$00      ; $5D ]
        db $20,$50,$88,$00,$00,$00,$00,$00      ; $5E 
        db $00,$00,$00,$00,$00,$00,$f8,$00      ; $5F _
        db $40,$20,$10,$00,$00,$00,$00,$00      ; $60 
        db $00,$00,$68,$98,$88,$98,$68,$00      ; $61 a
        db $80,$80,$f0,$88,$88,$88,$f0,$00      ; $62 b
        db $00,$00,$78,$80,$80,$80,$78,$00      ; $63 c
        db $08,$08,$68,$98,$88,$98,$68,$00      ; $64 d
        db $00,$00,$70,$88,$f8,$80,$70,$00      ; $65 e
        db $30,$48,$40,$e0,$40,$40,$40,$00      ; $66 f
        db $00,$00,$78,$88,$88,$78,$08,$70      ; $67 g
        db $80,$80,$f0,$88,$88,$88,$88,$00      ; $68 h
        db $20,$00,$60,$20,$20,$20,$70,$00      ; $69 i
        db $08,$00,$18,$08,$88,$88,$70,$00      ; $6a j
        db $80,$80,$88,$90,$e0,$90,$88,$00      ; $6b k
        db $60,$20,$20,$20,$20,$20,$70,$00      ; $6c l
        db $00,$00,$d0,$a8,$a8,$a8,$a8,$00      ; $6d m
        db $00,$00,$b0,$c8,$88,$88,$88,$00      ; $6e n
        db $00,$00,$70,$88,$88,$88,$70,$00      ; $6f o
        db $00,$00,$f0,$88,$88,$88,$f0,$80      ; $70 p
        db $00,$00,$78,$88,$88,$88,$78,$08      ; $71 q
        db $00,$00,$b8,$c0,$80,$80,$80,$00      ; $72 r
        db $00,$00,$78,$80,$70,$08,$f0,$00      ; $73 s
        db $20,$20,$f8,$20,$20,$20,$20,$00      ; $74 t
        db $00,$00,$88,$88,$88,$98,$68,$00      ; $75 u
        db $00,$00,$88,$88,$88,$50,$20,$00      ; $76 v
        db $00,$00,$88,$a8,$a8,$a8,$50,$00      ; $77 w
        db $00,$00,$88,$50,$20,$50,$88,$00      ; $78 x
        db $00,$00,$88,$88,$98,$68,$08,$70      ; $79 y
        db $00,$00,$f8,$10,$20,$40,$f8,$00      ; $7a z
        db $18,$20,$20,$40,$20,$20,$18,$00      ; $7b {
        db $20,$20,$20,$20,$20,$20,$20,$00      ; $7c |
        db $c0,$20,$20,$10,$20,$20,$c0,$00      ; $7d } 
        db $00,$00,$40,$a8,$10,$00,$00,$00      ; $7e
        db $70,$70,$20,$f8,$20,$70,$50,$00      ; $7f
    endif

vdp_generic_mode:
	call nmi_off
	call WRTVDP
	ld bc,$a201
	call WRTVDP
	ld bc,$0602	; $1800 for pattern table.
	call WRTVDP
	ld b,d
	ld c,$03	; for color table.
	call WRTVDP
	ld b,e
	ld c,$04	; for bitmap table.
	call WRTVDP
	ld bc,$3605	; $1b00 for sprite attribute table.
	call WRTVDP
	ld bc,$0706	; $3800 for sprites bitmaps.
	call WRTVDP
	ld bc,$0107
	jp WRTVDP

mode_0:
	ld hl,mode
	res 2,(hl)
	ld bc,$0200
	ld de,$ff03	; $2000 for color table, $0000 for bitmaps.
	call vdp_generic_mode
    if COLECO
	ld hl,($006c)
	ld de,-128
	add hl,de
    endif
    if SG1000
	ld hl,font_bitmaps
    endif
    if MSX
	ld hl,($0004)   
	inc h
    endif
	ld de,$0100
	ld bc,$0300
	call LDIRVM3
	call nmi_on
	call nmi_off
	ld hl,$2000
	ld bc,$1800
	ld a,$f0
	call FILVRM
	call nmi_on
	call cls
vdp_generic_sprites:
	call nmi_off
	ld hl,$1b00
	ld bc,$0080
	ld a,$d1
	call FILVRM
	ld hl,sprites
	ld de,sprites+1
	ld bc,127
	ld (hl),$d1
	ldir
	call nmi_on
	call nmi_off
	ld bc,$e201	; Enable screen and interrupts.
	call WRTVDP
	jp nmi_on

mode_1:
	ld hl,mode
	res 2,(hl)
	ld bc,$0200
	ld de,$ff03	; $2000 for color table, $0000 for bitmaps.
	call vdp_generic_mode
	ld hl,$0000
	ld bc,$1800
	xor a
	call FILVRM
	call nmi_on
	call nmi_off
	ld hl,$2000
	ld bc,$1800
	ld a,$f0
	call FILVRM
	call nmi_on
	ld hl,$1800
.1:	call nmi_off
	ld b,32
.2:	ld a,l
	call WRTVRM
	inc hl
	djnz .2
	call nmi_on
	ld a,h
	cp $1b
	jp nz,.1
	jp vdp_generic_sprites

mode_2:
	ld hl,mode
	set 2,(hl)
	ld bc,$0000
	ld de,$8000	; $2000 for color table, $0000 for bitmaps.
	call vdp_generic_mode
    if COLECO
	ld hl,($006c)
	ld de,-128
	add hl,de
    endif
    if SG1000
	ld hl,font_bitmaps
    endif
    if MSX
	ld hl,($0004)   
	inc h
    endif
	ld de,$0100
	ld bc,$0300
	call LDIRVM
	call nmi_on
	call nmi_off
	ld hl,$2000
	ld bc,$0020
	ld a,$f0
	call FILVRM
	call nmi_on
	call cls
	jp vdp_generic_sprites

    if MSX
ENASLT: EQU $0024       ; Select slot (H=Addr, A=Slot)
RSLREG: EQU $0138       ; Read slot status in A

        ;
        ; Get slot mapping
        ; B = 16K bank (0 for $0000, 1 for $4000, 2 for $8000, 3 for $c000)
        ; A = Current slot selection status (CALL RSLREG)
        ;
get_slot_mapping:
        call rotate_slot
        ld c,a
        add a,$C1       ; EXPTBL
        ld l,a
        ld h,$FC
        ld a,(hl)
        and $80         ; Get expanded flag
        or c
        ld c,a
        inc hl
        inc hl
        inc hl
        inc hl
        ld a,(hl)       ; SLTTBL
        call rotate_slot
        rlca
        rlca
        or c            ; A contains bit 7 = Marks expanded
                        ;            bit 6 - 4 = Doesn't care
                        ;            bit 3 - 2 = Secondary mapper
                        ;            bit 1 - 0 = Primary mapper
        ret

rotate_slot:
        push bc
        dec b
        inc b
        jr z,.1
.0:     rrca
        rrca
        djnz .0
.1:     and 3
        pop bc
        ret

    endif

nmi_handler:
	push af
	push hl
	ld hl,mode
	bit 0,(hl)
	jr z,.1
	set 1,(hl)
	pop hl
	pop af
	retn

.0:	res 1,(hl)

.1:	push bc
	push de
  if CVBASIC_BANK_SWITCHING
    if COLECO
	ld a,($ffbf)
    endif
    if SG1000
	ld a,($7fff)
    endif
    if MSX
	ld a,($bfff)
    endif
	push af
  endif
    if SG1000+MSX
	in a,(VDP+1)
    endif
	ld bc,$8000+VDP
	bit 2,(hl)
	jr z,.4

	ld hl,$1b00
	call SETWRT
	ld hl,sprites
	outi
	jp nz,$-2
	jr .5

.4:
	ld hl,$1b00
	call SETWRT
	ld a,(flicker)
	add a,$04
	ld (flicker),a
	ld l,a
	ld h,sprites>>8
	ld de,24
	ld b,128
.6:
	res 7,l
	outi
	jp $+3
	outi
	jp $+3
	outi
	jp $+3
	outi
	jp $+3
	add hl,de
	jp nz,.6
.5:

    if COLECO
	out (JOYSEL),a
	ex (sp),hl
	ex (sp),hl
	in a,(JOY1)
	or $b0
	ld b,a
	in a,(JOY2)
	or $b0
	ld c,a

	out (KEYSEL),a
	ex (sp),hl
	ex (sp),hl
	in a,(JOY1)
	ld d,a
	in a,(JOY2)
	ld e,a

	ld a,d
	rlca
	or $7f
	and b
	cpl
	ld (joy1_data),a

	ld a,e
	rlca
	or $7f
	and c
	cpl
	ld (joy2_data),a

	ld a,d
	and $0f
	ld c,a
	ld b,0
	ld hl,keypad_table
	add hl,bc
	ld a,(hl)
	ld (key1_data),a

	ld a,e
	and $0f
	ld c,a
	ld hl,keypad_table
	add hl,bc
	ld a,(hl)
	ld (key2_data),a
    endif
    if SG1000
        ld b,$ff
        in a,(JOY1)
        bit 0,a
        jr nz,$+4
        res 0,b
        bit 1,a
        jr nz,$+4
        res 2,b
        bit 2,a
        jr nz,$+4
        res 3,b
        bit 3,a
        jr nz,$+4
        res 1,b
        bit 4,a
        jr nz,$+4
        res 6,b
        bit 5,a
        jr nz,$+4
        res 7,b
	push af
	ld a,b
	cpl
	ld (joy1_data),a
	pop af

	ld b,$ff
        bit 6,a
        jr nz,$+4
        res 0,b
        bit 7,a
        jr nz,$+4
        res 2,b

        in a,(JOY2)
        bit 0,a
        jr nz,$+4
        res 3,b
        bit 1,a
        jr nz,$+4
        res 1,b
        bit 2,a
        jr nz,$+4
        res 4,b
        bit 3,a
        jr nz,$+4
        res 5,b
	push af
	ld a,b
	cpl
	ld (joy2_data),a
	pop af

    endif
    if MSX

	ld a,15
	call RDPSG
	and $b0
	or $4f
	ld e,a
	ld a,15
	call WRTPSG
	ld a,14
	call RDPSG
	ld b,$ff
	bit 0,a
	jr nz,$+4
	res 0,b
	bit 3,a
	jr nz,$+4
	res 1,b
	bit 1,a
	jr nz,$+4
	res 2,b
	bit 2,a
	jr nz,$+4
	res 3,b
	bit 4,a
	jr nz,$+4
	res 6,b
	bit 5,a
	jr nz,$+4
	res 7,b
	ld a,b
	cpl
	ld (joy2_data),a

        ld b,$ff
	in a,($aa)
	and $f0
	or $08
	out ($aa),a
	in a,($a9)
	bit 5,a
	jr nz,$+4
        res 0,b
	bit 7,a
	jr nz,$+4
        res 1,b
        bit 6,a
        jr nz,$+4
        res 2,b
        bit 4,a
        jr nz,$+4
        res 3,b
	bit 0,a
	jr nz,$+4
	res 6,b
	in a,($aa)
	and $f0
	or $04
	out ($aa),a
	in a,($a9)
	bit 2,a
	jr nz,$+4
	res 7,b

	ld a,15
	call RDPSG
	and $b0
	or $0f
	ld e,a
	ld a,15
	call WRTPSG
	ld a,14
	call RDPSG
	bit 0,a
	jr nz,$+4
	res 0,b
	bit 3,a
	jr nz,$+4
	res 1,b
	bit 1,a
	jr nz,$+4
	res 2,b
	bit 2,a
	jr nz,$+4
	res 3,b
	bit 4,a
	jr nz,$+4
	res 6,b
	bit 5,a
	jr nz,$+4
	res 7,b

	ld a,b
	cpl
	ld (joy1_data),a
    endif

    if CVBASIC_MUSIC_PLAYER
	ld a,(music_mode)
	or a
	call nz,music_hardware
    endif

	ld hl,(frame)
	inc hl
	ld (frame),hl

	ld hl,lfsr	; Make LFSR more random
	inc (hl)
	inc (hl)
	inc (hl)

    if CVBASIC_MUSIC_PLAYER
	;
	; Music is played with a 50hz clock.
	;
	ld a,(ntsc)
	or a
	jr z,.2
	ld a,(music_tick)
	inc a
	cp 6
	jr nz,$+3
	xor a
	ld (music_tick),a
	jr z,.3
.2:
	ld a,(music_mode)
	or a
	call nz,music_generate
.3:
    endif

  if CVBASIC_BANK_SWITCHING
	pop af
    if COLECO
	ld l,a
	ld h,$ff
	ld a,(hl)
    endif
    if SG1000
	ld ($fffe),a
    endif
    if MSX
	ld ($7000),a
    endif
  endif
	pop de
	pop bc
	pop hl
    if COLECO
	in a,(VDP+1)
	pop af
	retn
    endif
    if SG1000
	pop af
        ei
        reti
    endif
    if MSX
	pop af
        ret
    endif

	;
	; The music player code comes from my
	; game Princess Quest for Colecovision (2012)
	;

        ;
        ; Init music player.
        ;
music_init:
    if COLECO+SG1000
        ld a,$9f
        out (PSG),a
        ld a,$bf
        out (PSG),a
        ld a,$df
        out (PSG),a
        ld a,$ff
        out (PSG),a
        ld a,$ec
        out (PSG),a
    endif
    if MSX
WRTPSG:	equ $0093
RDPSG:	equ $0096

	ld a,$08
	ld e,$00
	call WRTPSG
	ld a,$09
	ld e,$00
	call WRTPSG
	ld a,$0a
	ld e,$00
	call WRTPSG
	ld a,$07
	ld e,$b8
	call WRTPSG
    endif
    if SGM
	ld b,$08
	xor a
	call ay3_reg
	ld b,$09
	call ay3_reg
	ld b,$0a
	call ay3_reg
	ld b,$07
	ld a,$b8
	call ay3_reg
    endif
    if CVBASIC_MUSIC_PLAYER
    else
	ret
    endif

    if CVBASIC_MUSIC_PLAYER
        ld a,$ff
        ld (audio_vol4hw),a
        ld a,$ec
        ld (audio_control),a
        ld a,$b8
        ld (audio_mix),a
	ld hl,music_silence
        ;
	; Play a music.
	; HL = Pointer to music.
        ;
music_play:
        call nmi_off
        ld a,(hl)          
        ld (music_timing),a
        inc hl
        ld (music_start),hl
        ld (music_pointer),hl
        xor a
        ld (music_note_counter),a
	inc a
	ld (music_playing),a
  if CVBASIC_BANK_SWITCHING
    if COLECO
	ld a,($ffbf)
    endif
    if SG1000
        ld a,($7fff)
    endif
    if MSX
        ld a,($bfff)
    endif
        ld (music_bank),a
  endif
        jp nmi_on

        ;
        ; Generates music.
        ;
music_generate:
        ld a,(audio_mix)
        and $c0                 
        or $38
        ld (audio_mix),a
        xor a                ; Turn off all the sound channels.
        ld l,a
        ld h,a
        ld (audio_vol1),hl   ; audio_vol1/audio_vol2
        ld (audio_vol3),a
	ld a,$ff
	ld (audio_vol4hw),a

        ld a,(music_note_counter)
        or a
        jp nz,.6
        ld hl,(music_pointer)
.15:    push hl
  if CVBASIC_BANK_SWITCHING
	ld a,(music_bank)
    if COLECO
	ld e,a
	ld d,$ff
	ld a,(de)
    endif
    if SG1000
        ld ($fffe),a
    endif
    if MSX
	ld ($7000),a
    endif
  endif
        ld b,(hl)
        inc hl
        ld c,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld e,(hl)
        pop hl
        ld a,(music_timing)
        rlca
        jr nc,.16
        ld e,d
        ld d,0
        jr .17

.16:    rlca
        jr nc,.17
        ld e,0
.17:    ld a,b		; Read first byte.
        cp -2           ; End of music?
        jr nz,.19       ; No, jump.
        xor a		; Keep at same place.
        ld (music_playing),a
        ret

.19:    cp -3           ; Repeat music?
        jp nz,.0
        ld hl,(music_start)
        jr .15

.0:     ld a,(music_timing)
        and $3f         ; Restart note time.
        ld (music_note_counter),a
        ld a,b
        cp $3f          ; Sustain?
        jr z,.1
        rlca
        rlca
        and 3
        ld (music_instrument_1),a    
        ld a,b
        and $3f
        ld (music_note_1),a    
        xor a         
        ld (music_counter_1),a    
.1:     ld a,c          
        cp $3f          
        jr z,.2
        rlca
        rlca
        and 3
        ld (music_instrument_2),a    
        ld a,c
        and $3f
        ld (music_note_2),a    
        xor a         
        ld (music_counter_2),a    
.2:     ld a,d          
        cp $3f          
        jr z,.3
        rlca
        rlca
        and 3
        ld (music_instrument_3),a    
        ld a,d
        and $3f
        ld (music_note_3),a    
        xor a         
        ld (music_counter_3),a    
.3:     ld a,e          
        ld (music_drum),a
        xor a
        ld (music_counter_4),a
        inc hl
        inc hl
        inc hl
        ld a,(music_timing)
        and $c0
        jr nz,.14
        inc hl
.14:    ld (music_pointer),hl

.6:     ld a,(music_note_1)    
        or a            
        jr z,.7         
        ld bc,(music_instrument_1)
        call music_note2freq
        ld (audio_freq1),hl 
        ld (audio_vol1),a

.7:     ld a,(music_note_2)    
        or a            
        jr z,.8         
        ld bc,(music_instrument_2)
        call music_note2freq
        ld (audio_freq2),hl 
        ld (audio_vol2),a

.8:     ld a,(music_note_3)    
        or a            
        jr z,.9         
        ld bc,(music_instrument_3)
        call music_note2freq
        ld (audio_freq3),hl 
        ld (audio_vol3),a

.9:     ld a,(music_drum)    
        or a            
        jr z,.4         
        dec a           ; 1 - Long drum.
        jr nz,.5
        ld a,(music_counter_4)
        cp 3
        jp nc,.4
.10:    ld a,5
        ld (audio_noise),a
        call enable_drum
        jr .4

.5:     dec a           ; 2 - Short durm.
        jr nz,.11
        ld a,(music_counter_4)
        or a
        jp nz,.4
        ld a,8
        ld (audio_noise),a
        call enable_drum
        jr .4

.11:    ;dec a           ; 3 - Roll.
        ;jp nz,.4
        ld a,(music_timing)
        and $3e
        rrca
        ld b,a
        ld a,(music_counter_4)
        cp 2
        jp c,.10
        cp b
        jp c,.4
        dec a
        dec a
        cp b
        jp c,.10
.4:
        ld a,(music_counter_1)
        inc a
        cp $18
        jp nz,$+5
        sub $08
        ld (music_counter_1),a

        ld a,(music_counter_2)
        inc a
        cp $18
        jp nz,$+5
        sub $08
        ld (music_counter_2),a

        ld a,(music_counter_3)
        inc a
        cp $18
        jp nz,$+5
        sub $08
        ld (music_counter_3),a

        ld hl,music_counter_4
        inc (hl)
        ld hl,music_note_counter
        dec (hl)
        ret

        ;
        ; Converts note to frequency.
 	; Input:
	;   A = Note (1-62).
	;   B = Instrument counter.
	;   C = Instrument.
        ; Output:
	;   HL = Frequency.
	;   A = Volume.
	;
music_note2freq:
        add a,a
        ld e,a
        ld d,0
        ld hl,music_notes_table
        add hl,de
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld a,c
        or a
        jp z,music_piano
        dec a
        jp z,music_clarinet
        dec a
        jp z,music_flute
        ;
        ; Bass instrument.
        ;
music_bass:
        add hl,hl

        ;
        ; Piano instrument.
        ;
music_piano:
        ld a,b
        add a,.1&255
        ld c,a
        adc a,.1>>8
        sub c
        ld b,a
        ld a,(bc)
        ret

.1:
        db 12,11,11,10,10,9,9,8
        db 8,7,7,6,6,5,5,4
        db 4,4,5,5,4,4,3,3

        ;
        ; Clarinet instrument.
        ;
music_clarinet:
        ld a,b
        add a,.1&255
        ld c,a
        adc a,.1>>8
        sub c
        ld b,a
        ld a,(bc)
        ld e,a
        rlca
        sbc a,a
        ld d,a
        add hl,de
        srl h           
        rr l
        jp nc,.2
        inc hl
.2:     ld a,c
        add a,24
        ld c,a
	jr nc,$+3
	inc b
        ld a,(bc)
        ret

.1:
        db 0,0,0,0
        db -2,-4,-2,0
        db 2,4,2,0
        db -2,-4,-2,0
        db 2,4,2,0
        db -2,-4,-2,0

        db 13,14,14,13,13,12,12,12
        db 11,11,11,11,12,12,12,12
        db 11,11,11,11,12,12,12,12

        ;
        ; Flute instrument.
        ;
music_flute:
        ld a,b
        add a,.1&255
        ld c,a
        adc a,.1>>8
        sub c
        ld b,a
        ld a,(bc)
        ld e,a
        rlca
        sbc a,a
        ld d,a
        add hl,de
        ld a,c
        add a,24
        ld c,a
	jr nc,$+3
	inc b
        ld a,(bc)
        ret

.1:
        db 0,0,0,0
        db 0,1,2,1
        db 0,1,2,1
        db 0,1,2,1
        db 0,1,2,1
        db 0,1,2,1
                 
        db 10,12,13,13,12,12,12,12
        db 11,11,11,11,10,10,10,10
        db 11,11,11,11,10,10,10,10

        ;
        ; Emit sound.
        ;
music_hardware:
    if COLECO+SG1000
	ld a,(music_mode)
	cp 4		; PLAY SIMPLE?
	jr c,.7		; Yes, jump.
        ld a,(audio_vol2)
        or a
        jp nz,.7
        ld a,(audio_vol3)
        or a
        jp z,.7
        ld (audio_vol2),a
        xor a
        ld (audio_vol3),a
        ld hl,(audio_freq3)
        ld (audio_freq2),hl
.7:
        ld hl,(audio_freq1)
        ld a,h
        cp 4
        ld a,$9f
        jp nc,.1
        ld a,l
        and $0f
        or $80
        out (PSG),a
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        ld a,h
        out (PSG),a
        ld a,(audio_vol1)
        add a,ay2sn&255
        ld l,a
        adc a,ay2sn>>8
        sub l
        ld h,a
        ld a,(hl)
        or $90
.1:     out (PSG),a

        ld hl,(audio_freq2)
        ld a,h
        cp 4
        ld a,$bf
        jp nc,.2
        ld a,l
        and $0f
        or $a0
        out (PSG),a
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        ld a,h
        out (PSG),a
        ld a,(audio_vol2)
        add a,ay2sn&255
        ld l,a
        adc a,ay2sn>>8
        sub l
        ld h,a
        ld a,(hl)
        or $b0
.2:     out (PSG),a

	ld a,(music_mode)
	cp 4		; PLAY SIMPLE?
	jr c,.6		; Yes, jump.

        ld hl,(audio_freq3)
        ld a,h
        cp 4
        ld a,$df
        jp nc,.3
        ld a,l
        and $0f
        or $c0
        out (PSG),a
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        ld a,h
        out (PSG),a
        ld a,(audio_vol3)
        add a,ay2sn&255
        ld l,a
        adc a,ay2sn>>8
        sub l
        ld h,a
        ld a,(hl)
        or $d0
.3:     out (PSG),a

.6:
	ld a,(music_mode)
	and 1		; NO DRUMS?
	ret z		; Yes, return.

        ld a,(audio_vol4hw)
        inc a           
        jr z,.4        
        ld a,(audio_noise)
        cp 16
        ld b,$ec        
        jp c,.5
        ld b,$ed        
;       ld b,$ee        
.5:     ld a,(audio_control)
        cp b
        jr z,.4
        ld a,b
        ld (audio_control),a
        out (PSG),a
.4:     ld a,(audio_vol4hw)
        out (PSG),a
        ret
    endif
    if MSX
	ld a,(music_mode)
	cp 4		; PLAY SIMPLE?
	jr c,.8		; Yes, jump.	
	ld hl,audio_freq1
	ld bc,$0b00
	ld a,c
	ld e,(hl)
	call WRTPSG
	inc hl
	inc c
	djnz $-7
	ret
.8:
	ld hl,audio_freq1
	ld bc,$0400
	ld a,c
	ld e,(hl)
	call WRTPSG
	inc hl
	inc c
	djnz $-7
	inc hl
	inc hl
	inc c
	inc c
	ld a,(music_mode)
	and 1
	jr z,.9
	ld a,c
	ld e,(hl)
	call WRTPSG
	inc hl
	inc c
	ld a,c
	ld e,(hl)
	call WRTPSG
	inc hl
	inc c
	jr .10
.9:	inc hl
	inc c
	inc hl
	inc c
.10:	ld b,$02
	ld a,c
	ld e,(hl)
	call WRTPSG
	inc hl
	inc c
	djnz $-7
	ret
    endif

        ;
        ; Enable drum.
        ;
enable_drum:
    if COLECO+SG1000
        ld a,$f5
        ld (audio_vol4hw),a
    else
        ld hl,audio_mix
        ld a,(audio_vol2)
        or a
        jr nz,.1
        ld a,10
        ld (audio_vol2),a
        set 1,(hl)
.1:     res 4,(hl)
    endif
        ret

        ;
	; Musical notes table.
	;
music_notes_table:
        ; Silence - 0
        dw 0
        ; 2nd octave - 1
        dw 1721,1621,1532,1434,1364,1286,1216,1141,1076,1017,956,909
        ; 3rd octave - 13
        dw 854,805,761,717,678,639,605,571,538,508,480,453
        ; 4th octave - 25
        dw 427,404,380,360,339,321,302,285,270,254,240,226
        ; 5th octave - 37
        dw 214,202,191,180,170,160,151,143,135,127,120,113
        ; 6th octave - 49
        dw 107,101,95,90,85,80,76,71,67,64,60,57
        ; 7th octave - 61
	dw 54,51,48

    if COLECO+SG1000
        ;
        ; Converts AY-3-8910 volume to SN76489
        ;
ay2sn:
        db $0f,$0f,$0f,$0e,$0e,$0e,$0d,$0b,$0a,$08,$07,$05,$04,$03,$01,$00
    endif

music_silence:
	db 8
	db 0,0,0,0
	db -2
    endif

    if CVBASIC_COMPRESSION
define_char_unpack:
	ex de,hl
	pop af
	pop hl
	push af
	add hl,hl	; x2
	add hl,hl	; x4
	add hl,hl	; x8
	ex de,hl
	ld a,(mode)
	and 4
	jp z,unpack3
	jp unpack

define_color_unpack:
	ex de,hl
	pop af
	pop hl
	push af
	add hl,hl	; x2
	add hl,hl	; x4
	add hl,hl	; x8
	ex de,hl
	set 5,d
unpack3:
	call .1
	call .1
.1:
	push de
	push hl
	call unpack
	pop hl
	pop de
	ld a,d
	add a,8	
	ld d,a
	ret
	
        ;
        ; Pletter-0.5c decompressor (XL2S Entertainment & Team Bomba)
        ;
unpack:
; Initialization
        ld a,(hl)
        inc hl
	exx
        ld de,0
        add a,a
        inc a
        rl e
        add a,a
        rl e
        add a,a
        rl e
        rl e
        ld hl,.modes
        add hl,de
        ld c,(hl)
        inc hl
        ld b,(hl)
        push bc
        pop ix
        ld e,1
	exx
        ld iy,.loop

; Main depack loop
.literal:
        ex af,af'
        call nmi_off
        ld a,(hl)
        ex de,hl
        call WRTVRM
        ex de,hl
        inc hl
        inc de
        call nmi_on
        ex af,af'
.loop:   add a,a
        call z,.getbit
        jr nc,.literal

; Compressed data
	exx
        ld h,d
        ld l,e
.getlen: add a,a
        call z,.getbitexx
        jr nc,.lenok
.lus:    add a,a
        call z,.getbitexx
        adc hl,hl
        ret c   
        add a,a
        call z,.getbitexx
        jr nc,.lenok
        add a,a
        call z,.getbitexx
        adc hl,hl
        ret c  
        add a,a
        call z,.getbitexx
        jr c,.lus
.lenok:  inc hl
	exx
        ld c,(hl)
        inc hl
        ld b,0
        bit 7,c
        jr z,.offsok
        jp (ix)

.mode6:  add a,a
        call z,.getbit
        rl b
.mode5:  add a,a
        call z,.getbit
        rl b
.mode4:  add a,a
        call z,.getbit
        rl b
.mode3:  add a,a
        call z,.getbit
        rl b
.mode2:  add a,a
        call z,.getbit
        rl b
        add a,a
        call z,.getbit
        jr nc,.offsok
        or a
        inc b
        res 7,c
.offsok: inc bc
        push hl
	exx
        push hl
	exx
        ld l,e
        ld h,d
        sbc hl,bc
        pop bc
        ex af,af'
.loop2: 
        call nmi_off
        call RDVRM              ; unpack
        ex de,hl
        call WRTVRM
        ex de,hl        ; 4
        call nmi_on
        inc hl          ; 6
        inc de          ; 6
        dec bc          ; 6
        ld a,b          ; 4
        or c            ; 4
        jr nz,.loop2     ; 10
        ex af,af'
        pop hl
        jp (iy)

.getbit: ld a,(hl)
        inc hl
	rla
	ret

.getbitexx:
	exx
        ld a,(hl)
        inc hl
	exx
	rla
	ret

.modes:
        dw      .offsok
        dw      .mode2
        dw      .mode3
        dw      .mode4
        dw      .mode5
        dw      .mode6

    endif

START:
    if SG1000
	ld a,$92	; Setup 8255 for SC3000.
	out ($df),a
	ld a,$07	; Read joysticks instead of keyboard.
	out ($de),a
    else
	di
    endif
	ld sp,STACK
	in a,(VDP+1)
	ld bc,$8201
	call WRTVDP
	in a,(VDP+1)
	ld bc,$8201
	call WRTVDP
  if CVBASIC_BANK_SWITCHING
    if COLECO
	ld a,($ffc0)	; Megacart
    endif
    if SG1000
        ld a,1		; Sega mapper
        ld ($fffe),a
    endif
    if MSX
        ld a,1		; ASCII 16K
        ld ($7000),a
    endif
  endif
	ld hl,(lfsr)	; Save RAM trash for random generator.
	ld de,BASE_RAM
	xor a
	ld (de),a
	inc de
	bit 2,d
	jp z,$-4
	ld (lfsr),hl

    if COLECO
	ld a,($0069)
	cp 50
	ld a,0
	jr z,$+4
	ld a,1
	ld (ntsc),a
    endif
    if SG1000
	ld a,1
	ld (ntsc),a
    endif
    if MSX
	ld a,($002b)
	cpl
	rlca
	and $01
	ld (ntsc),a

        call RSLREG
        ld b,1          ; $4000-$7fff
        call get_slot_mapping
        ld h,$80
        call ENASLT     ; Map into $8000-$BFFF
    endif

    if SGM
WRITE_REGISTER:	equ $1fd9
FILL_VRAM:	equ $1f82
WRITE_VRAM:	equ $1fdf

        ld b,$00	; First step.
.0:     ld hl,$2000	; RAM at $2000.
.1:     ld (hl),h	; Try to write a byte.
        inc h
        jp p,.1		; Repeat until reaching $8000.
        ld h,$20	; Go back at $2000.
.2:     ld a,(hl)	; Read back byte.
        cp h		; Is it correct?
        jr nz,.3	; No, jump.
        inc h
        jp p,.2		; Repeat until reaching $8000.
        jp .4		; Memory valid!

.3:     ld a,$01        ; Enable SGM
        out ($53),a
        inc b
        bit 1,b         ; Already enabled?
        jr z,.0		; No, test RAM again.

        ld bc,$0000
        call WRITE_REGISTER
        ld bc,$0180
        call WRITE_REGISTER
        ld bc,$0206
        call WRITE_REGISTER
        ld bc,$0380
        call WRITE_REGISTER
        ld bc,$0400
        call WRITE_REGISTER
        ld bc,$0536
        call WRITE_REGISTER
        ld bc,$0607
        call WRITE_REGISTER
        ld bc,$070D
        call WRITE_REGISTER
        ld bc,$03F0
        ld de,$00E8 
        ld hl,$158B     ; Note! direct access to Colecovision ROM
        call WRITE_VRAM
        ld hl,$2000
        ld de,32 
        ld a,$FD
        call FILL_VRAM
        ld hl,$1B00
        ld de,128
        ld a,$D1
        call FILL_VRAM
        ld hl,$1800
        ld de,769
        ld a,$20
        call FILL_VRAM
        ld bc,$0020
        ld de,$1980 
        ld hl,.5
        call WRITE_VRAM
        ld bc,$01C0
        call WRITE_REGISTER
        jr $

.5:     db " SUPER GAME MODULE NOT DETECTED "

.4:
	ld ix,(lfsr)
        ld hl,$2000
        ld de,$2001
        ld bc,$5FFF
        ld (hl),0
        ldir
	ld (lfsr),ix
    endif
	call music_init

	xor a
	ld (mode),a

	call mode_0

	ld a,$ff
	ld (joy1_data),a
	ld (joy2_data),a
	ld a,$0f
	ld (key1_data),a
	ld (key2_data),a

    if MSX
	ld hl,nmi_handler
	ld ($fd9b),hl
	ld a,$c3
	ld ($fd9a),a
    endif

	; CVBasic program start.
	; 	' TMSColor 2.2.1 Mar/29/2024
	; 	' Command: ..\tmscolor -z -t -b -o result.bmp ScrollTest.bmp ScrollTest.bas 
	; 	' Created: Mon May 13 14:38:54 2024
	; 
	; 	' Display image.
	; 	MODE 0
	CALL mode_0
	; 	DEFINE CHAR PLETTER 0,231,image_char
	LD HL,0
	PUSH HL
	LD A,231
	LD HL,cvb_IMAGE_CHAR
	CALL define_char_unpack
	; 	DEFINE COLOR PLETTER 0,231,image_color
	LD HL,0
	PUSH HL
	LD A,231
	LD HL,cvb_IMAGE_COLOR
	CALL define_color_unpack
	; 
	;     SCREEN image_pattern,0,0,32,16,64
	LD HL,cvb_IMAGE_PATTERN
	PUSH HL
	LD HL,6144
	PUSH HL
	LD A,32
	PUSH AF
	LD A,16
	PUSH AF
	LD A,64
	CALL CPYBLK
	; 	#X = 0
	LD HL,0
	LD (cvb_#X),HL
	; 	WHILE (1)
cv1:
	; 		WAIT
	HALT
	; 		SCREEN image_pattern,#X/4+64*5+64*19*((#X) and 3),32*5,32,19,64
	LD HL,(cvb_#X)
	LD A,L
	AND 3
	LD L,A
	LD H,0
	LD DE,1216
	CALL _mul16
	PUSH HL
	LD HL,(cvb_#X)
	SRL H
	RR L
	SRL H
	RR L
	LD DE,320
	ADD HL,DE
	POP DE
	ADD HL,DE
	LD DE,cvb_IMAGE_PATTERN
	ADD HL,DE
	PUSH HL
	LD HL,6304
	PUSH HL
	LD A,32
	PUSH AF
	LD A,19
	PUSH AF
	LD A,64
	CALL CPYBLK
	; 		IF CONT.RIGHT THEN #X = (#X + 1) and 127
	LD A,(joy2_data)
	LD B,A
	LD A,(joy1_data)
	OR B
	AND 2
	JP Z,cv3
	LD HL,(cvb_#X)
	INC HL
	LD A,L
	AND 127
	LD L,A
	LD H,0
	LD (cvb_#X),HL
cv3:
	; 		IF CONT.LEFT  THEN #X = (#X - 1) and 127
	LD A,(joy2_data)
	LD B,A
	LD A,(joy1_data)
	OR B
	AND 8
	JP Z,cv4
	LD HL,(cvb_#X)
	DEC HL
	LD A,L
	AND 127
	LD L,A
	LD H,0
	LD (cvb_#X),HL
cv4:
	; 	WEND
	JP cv1
cv2:
	; 	
	; 
	; 
	; image_char:
cvb_IMAGE_CHAR:
	; 	DATA BYTE $30,$00,$00,$30,$48,$40,$1c,$21
	DB $30
	DB $00
	DB $00
	DB $30
	DB $48
	DB $40
	DB $1c
	DB $21
	; 	DATA BYTE $11,$00,$00,$ee,$30,$08,$08,$05
	DB $11
	DB $00
	DB $00
	DB $ee
	DB $30
	DB $08
	DB $08
	DB $05
	; 	DATA BYTE $01,$01,$18,$71,$89,$89,$07,$11
	DB $01
	DB $01
	DB $18
	DB $71
	DB $89
	DB $89
	DB $07
	DB $11
	; 	DATA BYTE $1b,$10,$11,$11,$1b,$17,$16,$19
	DB $1b
	DB $10
	DB $11
	DB $11
	DB $1b
	DB $17
	DB $16
	DB $19
	; 	DATA BYTE $0e,$07,$1e,$24,$24,$07,$8d,$21
	DB $0e
	DB $07
	DB $1e
	DB $24
	DB $24
	DB $07
	DB $8d
	DB $21
	; 	DATA BYTE $02,$03,$06,$07,$cf,$00,$07,$c6
	DB $02
	DB $03
	DB $06
	DB $07
	DB $cf
	DB $00
	DB $07
	DB $c6
	; 	DATA BYTE $1f,$99,$91,$8d,$07,$c7,$21,$1f
	DB $1f
	DB $99
	DB $91
	DB $8d
	DB $07
	DB $c7
	DB $21
	DB $1f
	; 	DATA BYTE $44,$88,$48,$48,$a0,$0f,$70,$88
	DB $44
	DB $88
	DB $48
	DB $48
	DB $a0
	DB $0f
	DB $70
	DB $88
	; 	DATA BYTE $f8,$c0,$05,$18,$20,$79,$22,$6c
	DB $f8
	DB $c0
	DB $05
	DB $18
	DB $20
	DB $79
	DB $22
	DB $6c
	; 	DATA BYTE $22,$37,$00,$d8,$00,$e0,$10,$74
	DB $22
	DB $37
	DB $00
	DB $d8
	DB $00
	DB $e0
	DB $10
	DB $74
	; 	DATA BYTE $f0,$07,$3d,$08,$60,$0c,$05,$20
	DB $f0
	DB $07
	DB $3d
	DB $08
	DB $60
	DB $0c
	DB $05
	DB $20
	; 	DATA BYTE $20,$2c,$6c,$32,$1f,$38,$34,$44
	DB $20
	DB $2c
	DB $6c
	DB $32
	DB $1f
	DB $38
	DB $34
	DB $44
	; 	DATA BYTE $44,$26,$69,$73,$7f,$0d,$00,$ce
	DB $44
	DB $26
	DB $69
	DB $73
	DB $7f
	DB $0d
	DB $00
	DB $ce
	; 	DATA BYTE $11,$1f,$ac,$17,$22,$05,$0e,$40
	DB $11
	DB $1f
	DB $ac
	DB $17
	DB $22
	DB $05
	DB $0e
	DB $40
	; 	DATA BYTE $40,$79,$45,$1f,$86,$00,$10,$10
	DB $40
	DB $79
	DB $45
	DB $1f
	DB $86
	DB $00
	DB $10
	DB $10
	; 	DATA BYTE $a0,$07,$05,$21,$21,$51,$51,$30
	DB $a0
	DB $07
	DB $05
	DB $21
	DB $21
	DB $51
	DB $51
	DB $30
	; 	DATA BYTE $27,$e7,$a6,$70,$e1,$8d,$08,$98
	DB $27
	DB $e7
	DB $a6
	DB $70
	DB $e1
	DB $8d
	DB $08
	DB $98
	; 	DATA BYTE $00,$0f,$07,$04,$1c,$84,$8a,$8a
	DB $00
	DB $0f
	DB $07
	DB $04
	DB $1c
	DB $84
	DB $8a
	DB $8a
	; 	DATA BYTE $af,$11,$0e,$11,$36,$27,$00,$e0
	DB $af
	DB $11
	DB $0e
	DB $11
	DB $36
	DB $27
	DB $00
	DB $e0
	; 	DATA BYTE $00,$09,$49,$30,$e4,$06,$78,$22
	DB $00
	DB $09
	DB $49
	DB $30
	DB $e4
	DB $06
	DB $78
	DB $22
	; 	DATA BYTE $e8,$d2,$0f,$61,$69,$71,$07,$9b
	DB $e8
	DB $d2
	DB $0f
	DB $61
	DB $69
	DB $71
	DB $07
	DB $9b
	; 	DATA BYTE $3b,$c8,$07,$78,$ac,$70,$1c,$8b
	DB $3b
	DB $c8
	DB $07
	DB $78
	DB $ac
	DB $70
	DB $1c
	DB $8b
	; 	DATA BYTE $83,$b4,$02,$e2,$03,$88,$c8,$48
	DB $83
	DB $b4
	DB $02
	DB $e2
	DB $03
	DB $88
	DB $c8
	DB $48
	; 	DATA BYTE $87,$8b,$07,$5f,$91,$00,$90,$5a
	DB $87
	DB $8b
	DB $07
	DB $5f
	DB $91
	DB $00
	DB $90
	DB $5a
	; 	DATA BYTE $05,$e1,$27,$62,$04,$08,$0f,$e1
	DB $05
	DB $e1
	DB $27
	DB $62
	DB $04
	DB $08
	DB $0f
	DB $e1
	; 	DATA BYTE $1c,$88,$17,$3d,$1c,$80,$80,$78
	DB $1c
	DB $88
	DB $17
	DB $3d
	DB $1c
	DB $80
	DB $80
	DB $78
	; 	DATA BYTE $07,$f0,$86,$e1,$21,$93,$47,$1f
	DB $07
	DB $f0
	DB $86
	DB $e1
	DB $21
	DB $93
	DB $47
	DB $1f
	; 	DATA BYTE $c4,$63,$ea,$c1,$eb,$10,$e0,$ea
	DB $c4
	DB $63
	DB $ea
	DB $c1
	DB $eb
	DB $10
	DB $e0
	DB $ea
	; 	DATA BYTE $03,$1c,$01,$0e,$00,$40,$22,$d9
	DB $03
	DB $1c
	DB $01
	DB $0e
	DB $00
	DB $40
	DB $22
	DB $d9
	; 	DATA BYTE $8a,$43,$40,$e9,$38,$90,$07,$08
	DB $8a
	DB $43
	DB $40
	DB $e9
	DB $38
	DB $90
	DB $07
	DB $08
	; 	DATA BYTE $87,$70,$f1,$27,$cf,$a2,$07,$20
	DB $87
	DB $70
	DB $f1
	DB $27
	DB $cf
	DB $a2
	DB $07
	DB $20
	; 	DATA BYTE $00,$d2,$07,$1f,$e8,$57,$00,$a0
	DB $00
	DB $d2
	DB $07
	DB $1f
	DB $e8
	DB $57
	DB $00
	DB $a0
	; 	DATA BYTE $41,$38,$41,$80,$07,$f9,$05,$68
	DB $41
	DB $38
	DB $41
	DB $80
	DB $07
	DB $f9
	DB $05
	DB $68
	; 	DATA BYTE $05,$0f,$21,$f0,$af,$69,$09,$aa
	DB $05
	DB $0f
	DB $21
	DB $f0
	DB $af
	DB $69
	DB $09
	DB $aa
	; 	DATA BYTE $43,$00,$1f,$a0,$a0,$63,$17,$91
	DB $43
	DB $00
	DB $1f
	DB $a0
	DB $a0
	DB $63
	DB $17
	DB $91
	; 	DATA BYTE $8e,$60,$37,$45,$45,$79,$41,$b4
	DB $8e
	DB $60
	DB $37
	DB $45
	DB $45
	DB $79
	DB $41
	DB $b4
	; 	DATA BYTE $00,$bb,$6c,$53,$8e,$41,$1c,$20
	DB $00
	DB $bb
	DB $6c
	DB $53
	DB $8e
	DB $41
	DB $1c
	DB $20
	; 	DATA BYTE $30,$0c,$04,$80,$6b,$70,$80,$c0
	DB $30
	DB $0c
	DB $04
	DB $80
	DB $6b
	DB $70
	DB $80
	DB $c0
	; 	DATA BYTE $30,$10,$64,$e0,$57,$58,$48,$c4
	DB $30
	DB $10
	DB $64
	DB $e0
	DB $57
	DB $58
	DB $48
	DB $c4
	; 	DATA BYTE $e6,$84,$88,$08,$88,$89,$88,$84
	DB $e6
	DB $84
	DB $88
	DB $08
	DB $88
	DB $89
	DB $88
	DB $84
	; 	DATA BYTE $e6,$4b,$48,$b2,$c8,$89,$cb,$12
	DB $e6
	DB $4b
	DB $48
	DB $b2
	DB $c8
	DB $89
	DB $cb
	DB $12
	; 	DATA BYTE $c4,$53,$1a,$17,$57,$09,$19,$89
	DB $c4
	DB $53
	DB $1a
	DB $17
	DB $57
	DB $09
	DB $19
	DB $89
	; 	DATA BYTE $d9,$63,$40,$ca,$8b,$ce,$ef,$03
	DB $d9
	DB $63
	DB $40
	DB $ca
	DB $8b
	DB $ce
	DB $ef
	DB $03
	; 	DATA BYTE $8f,$3c,$20,$86,$93,$40,$d3,$00
	DB $8f
	DB $3c
	DB $20
	DB $86
	DB $93
	DB $40
	DB $d3
	DB $00
	; 	DATA BYTE $07,$f3,$92,$26,$33,$4f,$80,$64
	DB $07
	DB $f3
	DB $92
	DB $26
	DB $33
	DB $4f
	DB $80
	DB $64
	; 	DATA BYTE $92,$0e,$81,$07,$e3,$ee,$44,$64
	DB $92
	DB $0e
	DB $81
	DB $07
	DB $e3
	DB $ee
	DB $44
	DB $64
	; 	DATA BYTE $24,$c3,$07,$b9,$62,$22,$00,$a1
	DB $24
	DB $c3
	DB $07
	DB $b9
	DB $62
	DB $22
	DB $00
	DB $a1
	; 	DATA BYTE $e0,$13,$c4,$81,$c9,$83,$40,$41
	DB $e0
	DB $13
	DB $c4
	DB $81
	DB $c9
	DB $83
	DB $40
	DB $41
	; 	DATA BYTE $b0,$2f,$88,$eb,$8c,$4c,$88,$00
	DB $b0
	DB $2f
	DB $88
	DB $eb
	DB $8c
	DB $4c
	DB $88
	DB $00
	; 	DATA BYTE $68,$8d,$81,$91,$9f,$90,$90,$8f
	DB $68
	DB $8d
	DB $81
	DB $91
	DB $9f
	DB $90
	DB $90
	DB $8f
	; 	DATA BYTE $88,$88,$04,$06,$01,$ed,$b6,$8e
	DB $88
	DB $88
	DB $04
	DB $06
	DB $01
	DB $ed
	DB $b6
	DB $8e
	; 	DATA BYTE $13,$9d,$0f,$0e,$22,$bd,$88,$8f
	DB $13
	DB $9d
	DB $0f
	DB $0e
	DB $22
	DB $bd
	DB $88
	DB $8f
	; 	DATA BYTE $1f,$69,$87,$1f,$65,$11,$1f,$8b
	DB $1f
	DB $69
	DB $87
	DB $1f
	DB $65
	DB $11
	DB $1f
	DB $8b
	; 	DATA BYTE $f5,$48,$3f,$e4,$b7,$01,$00,$03
	DB $f5
	DB $48
	DB $3f
	DB $e4
	DB $b7
	DB $01
	DB $00
	DB $03
	; 	DATA BYTE $07,$0f,$1f,$3f,$7f,$fe,$ff,$f2
	DB $07
	DB $0f
	DB $1f
	DB $3f
	DB $7f
	DB $fe
	DB $ff
	DB $f2
	; 	DATA BYTE $00,$c5,$00,$e0,$f0,$f8,$fc,$fe
	DB $00
	DB $c5
	DB $00
	DB $e0
	DB $f0
	DB $f8
	DB $fc
	DB $fe
	; 	DATA BYTE $7f,$f8,$17,$fc,$f8,$0f,$f0,$e0
	DB $7f
	DB $f8
	DB $17
	DB $fc
	DB $f8
	DB $0f
	DB $f0
	DB $e0
	; 	DATA BYTE $c0,$80,$a0,$1d,$00,$3f,$1f,$0f
	DB $c0
	DB $80
	DB $a0
	DB $1d
	DB $00
	DB $3f
	DB $1f
	DB $0f
	; 	DATA BYTE $07,$9d,$eb,$6d,$27,$4c,$17,$09
	DB $07
	DB $9d
	DB $eb
	DB $6d
	DB $27
	DB $4c
	DB $17
	DB $09
	; 	DATA BYTE $4e,$00,$14,$c0,$07,$1e,$7f,$3f
	DB $4e
	DB $00
	DB $14
	DB $c0
	DB $07
	DB $1e
	DB $7f
	DB $3f
	; 	DATA BYTE $00,$7d,$15,$07,$55,$00,$2a,$03
	DB $00
	DB $7d
	DB $15
	DB $07
	DB $55
	DB $00
	DB $2a
	DB $03
	; 	DATA BYTE $4c,$76,$0f,$07,$6a,$74,$73,$00
	DB $4c
	DB $76
	DB $0f
	DB $07
	DB $6a
	DB $74
	DB $73
	DB $00
	; 	DATA BYTE $23,$00,$1e,$26,$c0,$2d,$00,$1f
	DB $23
	DB $00
	DB $1e
	DB $26
	DB $c0
	DB $2d
	DB $00
	DB $1f
	; 	DATA BYTE $3c,$3e,$00,$37,$5a,$f8,$34,$3c
	DB $3c
	DB $3e
	DB $00
	DB $37
	DB $5a
	DB $f8
	DB $34
	DB $3c
	; 	DATA BYTE $00,$3f,$00,$79,$fc,$00,$a9,$77
	DB $00
	DB $3f
	DB $00
	DB $79
	DB $fc
	DB $00
	DB $a9
	DB $77
	; 	DATA BYTE $c8,$2b,$80,$8b,$db,$6e,$39,$5f
	DB $c8
	DB $2b
	DB $80
	DB $8b
	DB $db
	DB $6e
	DB $39
	DB $5f
	; 	DATA BYTE $1b,$01,$00,$bb,$be,$d8,$e3,$e6
	DB $1b
	DB $01
	DB $00
	DB $bb
	DB $be
	DB $d8
	DB $e3
	DB $e6
	; 	DATA BYTE $83,$1a,$61,$ed,$bd,$61,$2c,$0f
	DB $83
	DB $1a
	DB $61
	DB $ed
	DB $bd
	DB $61
	DB $2c
	DB $0f
	; 	DATA BYTE $01,$7f,$f1,$b9,$b5,$08,$07,$e1
	DB $01
	DB $7f
	DB $f1
	DB $b9
	DB $b5
	DB $08
	DB $07
	DB $e1
	; 	DATA BYTE $da,$a8,$e1,$39,$aa,$41,$aa,$e8
	DB $da
	DB $a8
	DB $e1
	DB $39
	DB $aa
	DB $41
	DB $aa
	DB $e8
	; 	DATA BYTE $95,$ae,$03,$1b,$98,$6a,$54,$37
	DB $95
	DB $ae
	DB $03
	DB $1b
	DB $98
	DB $6a
	DB $54
	DB $37
	; 	DATA BYTE $13,$d8,$f1,$fe,$95,$5e,$fe,$7c
	DB $13
	DB $d8
	DB $f1
	DB $fe
	DB $95
	DB $5e
	DB $fe
	DB $7c
	; 	DATA BYTE $57,$cf,$6a,$19,$9f,$11,$c7,$eb
	DB $57
	DB $cf
	DB $6a
	DB $19
	DB $9f
	DB $11
	DB $c7
	DB $eb
	; 	DATA BYTE $3c,$07,$01,$97,$1b,$fc,$99,$24
	DB $3c
	DB $07
	DB $01
	DB $97
	DB $1b
	DB $fc
	DB $99
	DB $24
	; 	DATA BYTE $4a,$97,$44,$17,$63,$07,$6c,$0f
	DB $4a
	DB $97
	DB $44
	DB $17
	DB $63
	DB $07
	DB $6c
	DB $0f
	; 	DATA BYTE $e7,$1a,$0f,$99,$00,$24,$e5,$4b
	DB $e7
	DB $1a
	DB $0f
	DB $99
	DB $00
	DB $24
	DB $e5
	DB $4b
	; 	DATA BYTE $08,$ed,$57,$b7,$4a,$ca,$f8,$e0
	DB $08
	DB $ed
	DB $57
	DB $b7
	DB $4a
	DB $ca
	DB $f8
	DB $e0
	; 	DATA BYTE $44,$6f,$f3,$57,$00,$28,$13,$5e
	DB $44
	DB $6f
	DB $f3
	DB $57
	DB $00
	DB $28
	DB $13
	DB $5e
	; 	DATA BYTE $03,$34,$77,$03,$00,$d6,$dd,$1d
	DB $03
	DB $34
	DB $77
	DB $03
	DB $00
	DB $d6
	DB $dd
	DB $1d
	; 	DATA BYTE $c3,$23,$47,$6f,$c3,$10,$12,$fe
	DB $c3
	DB $23
	DB $47
	DB $6f
	DB $c3
	DB $10
	DB $12
	DB $fe
	; 	DATA BYTE $fd,$02,$fb,$63,$8f,$07,$9b,$0f
	DB $fd
	DB $02
	DB $fb
	DB $63
	DB $8f
	DB $07
	DB $9b
	DB $0f
	; 	DATA BYTE $87,$b7,$f7,$1d,$a4,$00,$1b,$ca
	DB $87
	DB $b7
	DB $f7
	DB $1d
	DB $a4
	DB $00
	DB $1b
	DB $ca
	; 	DATA BYTE $3d,$a9,$2e,$de,$08,$cf,$a5,$39
	DB $3d
	DB $a9
	DB $2e
	DB $de
	DB $08
	DB $cf
	DB $a5
	DB $39
	; 	DATA BYTE $1f,$02,$16,$06,$3e,$10,$a8,$af
	DB $1f
	DB $02
	DB $16
	DB $06
	DB $3e
	DB $10
	DB $a8
	DB $af
	; 	DATA BYTE $7e,$ad,$64,$0d,$0f,$23,$f8,$e5
	DB $7e
	DB $ad
	DB $64
	DB $0d
	DB $0f
	DB $23
	DB $f8
	DB $e5
	; 	DATA BYTE $6c,$f0,$3f,$dc,$17,$b1,$ff,$76
	DB $6c
	DB $f0
	DB $3f
	DB $dc
	DB $17
	DB $b1
	DB $ff
	DB $76
	; 	DATA BYTE $c3,$5e,$d9,$07,$27,$a7,$67,$c3
	DB $c3
	DB $5e
	DB $d9
	DB $07
	DB $27
	DB $a7
	DB $67
	DB $c3
	; 	DATA BYTE $ef,$f0,$ce,$00,$8b,$7e,$93,$6c
	DB $ef
	DB $f0
	DB $ce
	DB $00
	DB $8b
	DB $7e
	DB $93
	DB $6c
	; 	DATA BYTE $6b,$9f,$7b,$00,$73,$24,$32,$52
	DB $6b
	DB $9f
	DB $7b
	DB $00
	DB $73
	DB $24
	DB $32
	DB $52
	; 	DATA BYTE $4c,$17,$ec,$07,$74,$7d,$3f,$1a
	DB $4c
	DB $17
	DB $ec
	DB $07
	DB $74
	DB $7d
	DB $3f
	DB $1a
	; 	DATA BYTE $9e,$1c,$5b,$43,$08,$cb,$4f,$01
	DB $9e
	DB $1c
	DB $5b
	DB $43
	DB $08
	DB $cb
	DB $4f
	DB $01
	; 	DATA BYTE $44,$45,$97,$80,$37,$34,$cf,$ae
	DB $44
	DB $45
	DB $97
	DB $80
	DB $37
	DB $34
	DB $cf
	DB $ae
	; 	DATA BYTE $00,$20,$ae,$38,$aa,$db,$03,$a8
	DB $00
	DB $20
	DB $ae
	DB $38
	DB $aa
	DB $db
	DB $03
	DB $a8
	; 	DATA BYTE $23,$77,$bb,$ea,$1b,$57,$cb,$00
	DB $23
	DB $77
	DB $bb
	DB $ea
	DB $1b
	DB $57
	DB $cb
	DB $00
	; 	DATA BYTE $fb,$fd,$fe,$fe,$f8,$f6,$90,$9e
	DB $fb
	DB $fd
	DB $fe
	DB $fe
	DB $f8
	DB $f6
	DB $90
	DB $9e
	; 	DATA BYTE $ef,$8f,$3f,$6f,$83,$a7,$df,$df
	DB $ef
	DB $8f
	DB $3f
	DB $6f
	DB $83
	DB $a7
	DB $df
	DB $df
	; 	DATA BYTE $ae,$e3,$d3,$3a,$ec,$e6,$a1,$31
	DB $ae
	DB $e3
	DB $d3
	DB $3a
	DB $ec
	DB $e6
	DB $a1
	DB $31
	; 	DATA BYTE $d9,$a1,$db,$07,$f9,$a5,$e7,$27
	DB $d9
	DB $a1
	DB $db
	DB $07
	DB $f9
	DB $a5
	DB $e7
	DB $27
	; 	DATA BYTE $0a,$27,$1e,$1a,$10,$a0,$37,$1e
	DB $0a
	DB $27
	DB $1e
	DB $1a
	DB $10
	DB $a0
	DB $37
	DB $1e
	; 	DATA BYTE $a8,$19,$a5,$00,$3f,$76,$23,$cb
	DB $a8
	DB $19
	DB $a5
	DB $00
	DB $3f
	DB $76
	DB $23
	DB $cb
	; 	DATA BYTE $fe,$5d,$c0,$47,$2b,$cf,$a9,$cf
	DB $fe
	DB $5d
	DB $c0
	DB $47
	DB $2b
	DB $cf
	DB $a9
	DB $cf
	; 	DATA BYTE $a5,$76,$5e,$22,$95,$1f,$67,$30
	DB $a5
	DB $76
	DB $5e
	DB $22
	DB $95
	DB $1f
	DB $67
	DB $30
	; 	DATA BYTE $e3,$00,$c0,$ce,$00,$f3,$3e,$fb
	DB $e3
	DB $00
	DB $c0
	DB $ce
	DB $00
	DB $f3
	DB $3e
	DB $fb
	; 	DATA BYTE $2c,$63,$87,$7b,$00,$71,$24,$ff
	DB $2c
	DB $63
	DB $87
	DB $7b
	DB $00
	DB $71
	DB $24
	DB $ff
	; 	DATA BYTE $70,$4a,$80,$ff,$ee,$07,$6c,$fb
	DB $70
	DB $4a
	DB $80
	DB $ff
	DB $ee
	DB $07
	DB $6c
	DB $fb
	; 	DATA BYTE $1a,$3c,$1c,$43,$bb,$08,$0d,$e7
	DB $1a
	DB $3c
	DB $1c
	DB $43
	DB $bb
	DB $08
	DB $0d
	DB $e7
	; 	DATA BYTE $9f,$e5,$57,$07,$4c,$a3,$97,$f7
	DB $9f
	DB $e5
	DB $57
	DB $07
	DB $4c
	DB $a3
	DB $97
	DB $f7
	; 	DATA BYTE $aa,$3c,$3f,$ff,$36,$ba,$43,$03
	DB $aa
	DB $3c
	DB $3f
	DB $ff
	DB $36
	DB $ba
	DB $43
	DB $03
	; 	DATA BYTE $34,$db,$03,$a8,$23,$7f,$9b,$d3
	DB $34
	DB $db
	DB $03
	DB $a8
	DB $23
	DB $7f
	DB $9b
	DB $d3
	; 	DATA BYTE $23,$0e,$d3,$23,$e0,$67,$ef,$f6
	DB $23
	DB $0e
	DB $d3
	DB $23
	DB $e0
	DB $67
	DB $ef
	DB $f6
	; 	DATA BYTE $f8,$f9,$25,$e0,$d8,$ae,$bf,$22
	DB $f8
	DB $f9
	DB $25
	DB $e0
	DB $d8
	DB $ae
	DB $bf
	DB $22
	; 	DATA BYTE $23,$bf,$f7,$00,$fb,$17,$ae,$e3
	DB $23
	DB $bf
	DB $f7
	DB $00
	DB $fb
	DB $17
	DB $ae
	DB $e3
	; 	DATA BYTE $b9,$6d,$10,$9d,$b9,$d9,$ad,$e7
	DB $b9
	DB $6d
	DB $10
	DB $9d
	DB $b9
	DB $d9
	DB $ad
	DB $e7
	; 	DATA BYTE $1f,$2a,$3c,$1a,$6a,$17,$7f,$a0
	DB $1f
	DB $2a
	DB $3c
	DB $1a
	DB $6a
	DB $17
	DB $7f
	DB $a0
	; 	DATA BYTE $ff,$ff,$ff,$e0
	DB $ff
	DB $ff
	DB $ff
	DB $e0
	; 
	; image_color:
cvb_IMAGE_COLOR:
	; 	DATA BYTE $3b,$f1,$af,$f8,$00,$51,$68,$71
	DB $3b
	DB $f1
	DB $af
	DB $f8
	DB $00
	DB $51
	DB $68
	DB $71
	; 	DATA BYTE $00,$75,$d2,$07,$0d,$7c,$51,$0f
	DB $00
	DB $75
	DB $d2
	DB $07
	DB $0d
	DB $7c
	DB $51
	DB $0f
	; 	DATA BYTE $71,$f3,$07,$6b,$00,$17,$ef,$00
	DB $71
	DB $f3
	DB $07
	DB $6b
	DB $00
	DB $17
	DB $ef
	DB $00
	; 	DATA BYTE $be,$0f,$1f,$02,$91,$b1,$f1,$b1
	DB $be
	DB $0f
	DB $1f
	DB $02
	DB $91
	DB $b1
	DB $f1
	DB $b1
	; 	DATA BYTE $b1,$91,$00,$0c,$e5,$b5,$a1,$61
	DB $b1
	DB $91
	DB $00
	DB $0c
	DB $e5
	DB $b5
	DB $a1
	DB $61
	; 	DATA BYTE $3e,$16,$54,$fe,$eb,$0e,$0d,$09
	DB $3e
	DB $16
	DB $54
	DB $fe
	DB $eb
	DB $0e
	DB $0d
	DB $09
	; 	DATA BYTE $41,$f1,$e1,$86,$07,$81,$91,$81
	DB $41
	DB $f1
	DB $e1
	DB $86
	DB $07
	DB $81
	DB $91
	DB $81
	; 	DATA BYTE $1f,$00,$61,$81,$7d,$17,$27,$f2
	DB $1f
	DB $00
	DB $61
	DB $81
	DB $7d
	DB $17
	DB $27
	DB $f2
	; 	DATA BYTE $00,$61,$0d,$a7,$01,$16,$41,$d3
	DB $00
	DB $61
	DB $0d
	DB $a7
	DB $01
	DB $16
	DB $41
	DB $d3
	; 	DATA BYTE $0f,$5b,$f5,$4d,$07,$8e,$8c,$6f
	DB $0f
	DB $5b
	DB $f5
	DB $4d
	DB $07
	DB $8e
	DB $8c
	DB $6f
	; 	DATA BYTE $b5,$0f,$e2,$00,$71,$41,$1b,$ee
	DB $b5
	DB $0f
	DB $e2
	DB $00
	DB $71
	DB $41
	DB $1b
	DB $ee
	; 	DATA BYTE $03,$0b,$ee,$06,$2b,$ad,$74,$48
	DB $03
	DB $0b
	DB $ee
	DB $06
	DB $2b
	DB $ad
	DB $74
	DB $48
	; 	DATA BYTE $ee,$0f,$00,$fa,$0f,$04,$db,$00
	DB $ee
	DB $0f
	DB $00
	DB $fa
	DB $0f
	DB $04
	DB $db
	DB $00
	; 	DATA BYTE $ef,$7a,$b7,$07,$34,$17,$07,$d9
	DB $ef
	DB $7a
	DB $b7
	DB $07
	DB $34
	DB $17
	DB $07
	DB $d9
	; 	DATA BYTE $95,$b4,$7d,$e0,$17,$f7,$7d,$dd
	DB $95
	DB $b4
	DB $7d
	DB $e0
	DB $17
	DB $f7
	DB $7d
	DB $dd
	; 	DATA BYTE $07,$13,$df,$00,$cf,$95,$c0,$0b
	DB $07
	DB $13
	DB $df
	DB $00
	DB $cf
	DB $95
	DB $c0
	DB $0b
	; 	DATA BYTE $95,$b5,$f5,$b5,$b5,$5f,$95,$00
	DB $95
	DB $b5
	DB $f5
	DB $b5
	DB $b5
	DB $5f
	DB $95
	DB $00
	; 	DATA BYTE $07,$07,$51,$b5,$a5,$65,$03,$1f
	DB $07
	DB $07
	DB $51
	DB $b5
	DB $a5
	DB $65
	DB $03
	DB $1f
	; 	DATA BYTE $e1,$eb,$ba,$b6,$a1,$00,$97,$f8
	DB $e1
	DB $eb
	DB $ba
	DB $b6
	DB $a1
	DB $00
	DB $97
	DB $f8
	; 	DATA BYTE $e9,$b8,$b8,$0f,$a8,$86,$61,$84
	DB $e9
	DB $b8
	DB $b8
	DB $0f
	DB $a8
	DB $86
	DB $61
	DB $84
	; 	DATA BYTE $9d,$07,$fe,$17,$ce,$8f,$df,$27
	DB $9d
	DB $07
	DB $fe
	DB $17
	DB $ce
	DB $8f
	DB $df
	DB $27
	; 	DATA BYTE $1a,$86,$8a,$87,$65,$15,$cd,$17
	DB $1a
	DB $86
	DB $8a
	DB $87
	DB $65
	DB $15
	DB $cd
	DB $17
	; 	DATA BYTE $6f,$07,$96,$1f,$51,$75,$ce,$a7
	DB $6f
	DB $07
	DB $96
	DB $1f
	DB $51
	DB $75
	DB $ce
	DB $a7
	; 	DATA BYTE $f8,$9f,$b0,$75,$d9,$de,$03,$ef
	DB $f8
	DB $9f
	DB $b0
	DB $75
	DB $d9
	DB $de
	DB $03
	DB $ef
	; 	DATA BYTE $14,$98,$8c,$dc,$d6,$fc,$1f,$51
	DB $14
	DB $98
	DB $8c
	DB $dc
	DB $d6
	DB $fc
	DB $1f
	DB $51
	; 	DATA BYTE $9f,$00,$1f,$f9,$3f,$91,$3e,$bb
	DB $9f
	DB $00
	DB $1f
	DB $f9
	DB $3f
	DB $91
	DB $3e
	DB $bb
	; 	DATA BYTE $ce,$a5,$b7,$1f,$db,$07,$3a,$ad
	DB $ce
	DB $a5
	DB $b7
	DB $1f
	DB $db
	DB $07
	DB $3a
	DB $ad
	; 	DATA BYTE $f3,$af,$6b,$17,$df,$c3,$df,$07
	DB $f3
	DB $af
	DB $6b
	DB $17
	DB $df
	DB $c3
	DB $df
	DB $07
	; 	DATA BYTE $7f,$17,$3f,$b7,$3f,$8f,$3a,$9f
	DB $7f
	DB $17
	DB $3f
	DB $b7
	DB $3f
	DB $8f
	DB $3a
	DB $9f
	; 	DATA BYTE $fc,$b7,$ef,$cf,$af,$ce,$9f,$bf
	DB $fc
	DB $b7
	DB $ef
	DB $cf
	DB $af
	DB $ce
	DB $9f
	DB $bf
	; 	DATA BYTE $3f,$a7,$7f,$07,$7c,$00,$eb,$7c
	DB $3f
	DB $a7
	DB $7f
	DB $07
	DB $7c
	DB $00
	DB $eb
	DB $7c
	; 	DATA BYTE $8d,$ed,$15,$f7,$2a,$f3,$a5,$af
	DB $8d
	DB $ed
	DB $15
	DB $f7
	DB $2a
	DB $f3
	DB $a5
	DB $af
	; 	DATA BYTE $33,$a7,$8c,$bf,$37,$a7,$1b,$ce
	DB $33
	DB $a7
	DB $8c
	DB $bf
	DB $37
	DB $a7
	DB $1b
	DB $ce
	; 	DATA BYTE $a5,$e7,$d7,$de,$00,$b8,$9f,$fd
	DB $a5
	DB $e7
	DB $d7
	DB $de
	DB $00
	DB $b8
	DB $9f
	DB $fd
	; 	DATA BYTE $ef,$fd,$ff,$ef,$cf,$9f,$df,$00
	DB $ef
	DB $fd
	DB $ff
	DB $ef
	DB $cf
	DB $9f
	DB $df
	DB $00
	; 	DATA BYTE $cf,$a7,$df,$d7,$7e,$e7,$e2,$a7
	DB $cf
	DB $a7
	DB $df
	DB $d7
	DB $7e
	DB $e7
	DB $e2
	DB $a7
	; 	DATA BYTE $54,$fc,$cf,$f5,$4f,$00,$be,$16
	DB $54
	DB $fc
	DB $cf
	DB $f5
	DB $4f
	DB $00
	DB $be
	DB $16
	; 	DATA BYTE $b7,$fe,$6e,$dd,$9d,$ad,$59,$af
	DB $b7
	DB $fe
	DB $6e
	DB $dd
	DB $9d
	DB $ad
	DB $59
	DB $af
	; 	DATA BYTE $ff,$ff,$ff,$ff,$c0
	DB $ff
	DB $ff
	DB $ff
	DB $ff
	DB $c0
	; 
	; image_pattern:
cvb_IMAGE_PATTERN:
	; 	DATA BYTE $00,$01,$02,$03,$04,$05,$06,$07
	DB $00
	DB $01
	DB $02
	DB $03
	DB $04
	DB $05
	DB $06
	DB $07
	; 	DATA BYTE $08,$09,$0a,$0b,$0c,$09,$0d,$0e
	DB $08
	DB $09
	DB $0a
	DB $0b
	DB $0c
	DB $09
	DB $0d
	DB $0e
	; 	DATA BYTE $0f,$10,$11,$12,$13,$14,$15,$16
	DB $0f
	DB $10
	DB $11
	DB $12
	DB $13
	DB $14
	DB $15
	DB $16
	; 	DATA BYTE $17,$18,$19,$1a,$1b,$1c,$1c,$1c
	DB $17
	DB $18
	DB $19
	DB $1a
	DB $1b
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1d,$1e,$1f,$20,$21,$22,$23,$24
	DB $1d
	DB $1e
	DB $1f
	DB $20
	DB $21
	DB $22
	DB $23
	DB $24
	; 	DATA BYTE $25,$26,$27,$28,$29,$2a,$2b,$2c
	DB $25
	DB $26
	DB $27
	DB $28
	DB $29
	DB $2a
	DB $2b
	DB $2c
	; 	DATA BYTE $2d,$2e,$2f,$30,$31,$32,$33,$34
	DB $2d
	DB $2e
	DB $2f
	DB $30
	DB $31
	DB $32
	DB $33
	DB $34
	; 	DATA BYTE $35,$36,$37,$38,$39,$1c,$1c,$1c
	DB $35
	DB $36
	DB $37
	DB $38
	DB $39
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $3a,$3b,$3c,$3d,$3e,$3f,$40,$41
	DB $3a
	DB $3b
	DB $3c
	DB $3d
	DB $3e
	DB $3f
	DB $40
	DB $41
	; 	DATA BYTE $42,$43,$44,$45,$46,$47,$48,$49
	DB $42
	DB $43
	DB $44
	DB $45
	DB $46
	DB $47
	DB $48
	DB $49
	; 	DATA BYTE $4a,$4b,$4c,$4d,$4e,$4f,$50,$51
	DB $4a
	DB $4b
	DB $4c
	DB $4d
	DB $4e
	DB $4f
	DB $50
	DB $51
	; 	DATA BYTE $52,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $52
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $53,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $53
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$54,$55,$55
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $54
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$56,$1c,$1c,$1c,$1c,$1c
	DB $55
	DB $55
	DB $56
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$54,$55,$55
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $54
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$56,$1c,$1c,$1c,$1c,$1c
	DB $55
	DB $55
	DB $56
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$57,$58,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $57
	DB $58
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$5a,$5b,$1c,$1c,$1c,$1c
	DB $59
	DB $59
	DB $5a
	DB $5b
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$57,$58,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $57
	DB $58
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$5a,$5b,$1c,$1c,$1c,$1c
	DB $59
	DB $59
	DB $5a
	DB $5b
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$57,$58,$59,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $57
	DB $58
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DB $59
	DB $59
	DB $5c
	DB $5c
	DB $5c
	DB $5c
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DB $59
	DB $59
	DB $5c
	DB $5c
	DB $5c
	DB $5c
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$5a,$5b,$1c,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $5a
	DB $5b
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$57,$58,$59,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $57
	DB $58
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DB $59
	DB $59
	DB $5c
	DB $5c
	DB $5c
	DB $5c
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DB $59
	DB $59
	DB $5c
	DB $5c
	DB $5c
	DB $5c
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$5a,$5b,$1c,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $5a
	DB $5b
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$57,$58,$59,$59,$5d,$5e
	DB $1c
	DB $1c
	DB $57
	DB $58
	DB $59
	DB $59
	DB $5d
	DB $5e
	; 	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DB $5f
	DB $5f
	DB $60
	DB $60
	DB $60
	DB $60
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DB $5f
	DB $5f
	DB $60
	DB $60
	DB $60
	DB $60
	DB $5f
	DB $5f
	; 	DATA BYTE $61,$62,$59,$59,$5a,$5b,$1c,$1c
	DB $61
	DB $62
	DB $59
	DB $59
	DB $5a
	DB $5b
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$57,$58,$59,$59,$5d,$5e
	DB $1c
	DB $1c
	DB $57
	DB $58
	DB $59
	DB $59
	DB $5d
	DB $5e
	; 	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DB $5f
	DB $5f
	DB $60
	DB $60
	DB $60
	DB $60
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DB $5f
	DB $5f
	DB $60
	DB $60
	DB $60
	DB $60
	DB $5f
	DB $5f
	; 	DATA BYTE $61,$62,$59,$59,$5a,$5b,$1c,$1c
	DB $61
	DB $62
	DB $59
	DB $59
	DB $5a
	DB $5b
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DB $59
	DB $59
	DB $64
	DB $64
	DB $64
	DB $64
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DB $59
	DB $59
	DB $64
	DB $64
	DB $64
	DB $64
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DB $59
	DB $59
	DB $64
	DB $64
	DB $64
	DB $64
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DB $59
	DB $59
	DB $64
	DB $64
	DB $64
	DB $64
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $66,$67,$67,$67,$67,$67,$67,$67
	DB $66
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$68,$59,$65,$1c,$1c
	DB $67
	DB $67
	DB $67
	DB $68
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $66,$67,$67,$67,$67,$67,$67,$67
	DB $66
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$68,$59,$65,$1c,$1c
	DB $67
	DB $67
	DB $67
	DB $68
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $69,$59,$59,$59,$59,$5c,$5c,$59
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $5c
	DB $5c
	DB $59
	; 	DATA BYTE $59,$59,$59,$6a,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $69,$59,$59,$59,$59,$5c,$5c,$59
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $5c
	DB $5c
	DB $59
	; 	DATA BYTE $59,$59,$59,$6a,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $69,$59,$5d,$5e,$5f,$60,$60,$5f
	DB $69
	DB $59
	DB $5d
	DB $5e
	DB $5f
	DB $60
	DB $60
	DB $5f
	; 	DATA BYTE $61,$62,$59,$6a,$59,$65,$1c,$1c
	DB $61
	DB $62
	DB $59
	DB $6a
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $69,$59,$5d,$5e,$5f,$60,$60,$5f
	DB $69
	DB $59
	DB $5d
	DB $5e
	DB $5f
	DB $60
	DB $60
	DB $5f
	; 	DATA BYTE $61,$62,$59,$6a,$59,$65,$1c,$1c
	DB $61
	DB $62
	DB $59
	DB $6a
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$66,$67,$67,$67
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $66
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$67,$67,$67,$68,$59
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $68
	DB $59
	; 	DATA BYTE $69,$59,$59,$59,$59,$64,$64,$59
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $64
	DB $64
	DB $59
	; 	DATA BYTE $59,$59,$59,$6a,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$66,$67,$67,$67
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $66
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$67,$67,$67,$68,$59
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $68
	DB $59
	; 	DATA BYTE $69,$59,$59,$59,$59,$64,$64,$59
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $64
	DB $64
	DB $59
	; 	DATA BYTE $59,$59,$59,$6a,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$69,$59,$66,$67
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $69
	DB $59
	DB $66
	DB $67
	; 	DATA BYTE $68,$59,$6b,$6b,$6b,$59,$6a,$59
	DB $68
	DB $59
	DB $6b
	DB $6b
	DB $6b
	DB $59
	DB $6a
	DB $59
	; 	DATA BYTE $6c,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DB $6c
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$6e,$59,$65,$1c,$1c
	DB $6d
	DB $6d
	DB $6d
	DB $6e
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$69,$59,$66,$67
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $69
	DB $59
	DB $66
	DB $67
	; 	DATA BYTE $68,$59,$6b,$6b,$6b,$59,$6a,$59
	DB $68
	DB $59
	DB $6b
	DB $6b
	DB $6b
	DB $59
	DB $6a
	DB $59
	; 	DATA BYTE $6c,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DB $6c
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$6e,$59,$65,$1c,$1c
	DB $6d
	DB $6d
	DB $6d
	DB $6e
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$69,$59,$69,$6f
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $69
	DB $59
	DB $69
	DB $6f
	; 	DATA BYTE $6a,$59,$6b,$6b,$6b,$59,$6a,$59
	DB $6a
	DB $59
	DB $6b
	DB $6b
	DB $6b
	DB $59
	DB $6a
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$69,$59,$69,$6f
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $69
	DB $59
	DB $69
	DB $6f
	; 	DATA BYTE $6a,$59,$6b,$6b,$6b,$59,$6a,$59
	DB $6a
	DB $59
	DB $6b
	DB $6b
	DB $6b
	DB $59
	DB $6a
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$69,$59,$6c,$6d
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $69
	DB $59
	DB $6c
	DB $6d
	; 	DATA BYTE $70,$59,$6b,$6b,$6b,$59,$6a,$59
	DB $70
	DB $59
	DB $6b
	DB $6b
	DB $6b
	DB $59
	DB $6a
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$69,$59,$6c,$6d
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $69
	DB $59
	DB $6c
	DB $6d
	; 	DATA BYTE $70,$59,$6b,$6b,$6b,$59,$6a,$59
	DB $70
	DB $59
	DB $6b
	DB $6b
	DB $6b
	DB $59
	DB $6a
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$6c,$6d,$6d,$6d
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $6c
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6e,$59
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6e
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$6c,$6d,$6d,$6d
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $6c
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6e,$59
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6e
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$63,$59,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $63
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$65,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $65
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$71,$72,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $71
	DB $72
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DB $59
	DB $59
	DB $5c
	DB $5c
	DB $5c
	DB $5c
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DB $59
	DB $59
	DB $5c
	DB $5c
	DB $5c
	DB $5c
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$73,$74,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $73
	DB $74
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$71,$72,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $71
	DB $72
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DB $59
	DB $59
	DB $5c
	DB $5c
	DB $5c
	DB $5c
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$5c,$5c,$5c,$5c,$59,$59
	DB $59
	DB $59
	DB $5c
	DB $5c
	DB $5c
	DB $5c
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$73,$74,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $73
	DB $74
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$71,$72,$59,$5d,$5e
	DB $1c
	DB $1c
	DB $1c
	DB $71
	DB $72
	DB $59
	DB $5d
	DB $5e
	; 	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DB $5f
	DB $5f
	DB $60
	DB $60
	DB $60
	DB $60
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DB $5f
	DB $5f
	DB $60
	DB $60
	DB $60
	DB $60
	DB $5f
	DB $5f
	; 	DATA BYTE $61,$62,$59,$73,$74,$1c,$1c,$1c
	DB $61
	DB $62
	DB $59
	DB $73
	DB $74
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$71,$72,$59,$5d,$5e
	DB $1c
	DB $1c
	DB $1c
	DB $71
	DB $72
	DB $59
	DB $5d
	DB $5e
	; 	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DB $5f
	DB $5f
	DB $60
	DB $60
	DB $60
	DB $60
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$5f,$60,$60,$60,$60,$5f,$5f
	DB $5f
	DB $5f
	DB $60
	DB $60
	DB $60
	DB $60
	DB $5f
	DB $5f
	; 	DATA BYTE $61,$62,$59,$73,$74,$1c,$1c,$1c
	DB $61
	DB $62
	DB $59
	DB $73
	DB $74
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$71,$72,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $71
	DB $72
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DB $59
	DB $59
	DB $64
	DB $64
	DB $64
	DB $64
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DB $59
	DB $59
	DB $64
	DB $64
	DB $64
	DB $64
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$73,$74,$1c,$1c,$1c,$1c
	DB $59
	DB $59
	DB $73
	DB $74
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$71,$72,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $71
	DB $72
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DB $59
	DB $59
	DB $64
	DB $64
	DB $64
	DB $64
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$64,$64,$64,$64,$59,$59
	DB $59
	DB $59
	DB $64
	DB $64
	DB $64
	DB $64
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$73,$74,$1c,$1c,$1c,$1c
	DB $59
	DB $59
	DB $73
	DB $74
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$71,$75,$75
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $71
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$74,$1c,$1c,$1c,$1c,$1c
	DB $75
	DB $75
	DB $74
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$1c,$71,$75,$75
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $71
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$74,$1c,$1c,$1c,$1c,$1c
	DB $75
	DB $75
	DB $74
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$76,$77,$55,$55
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $76
	DB $77
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$78,$79,$1c,$1c,$1c,$1c,$1c
	DB $55
	DB $78
	DB $79
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$76,$77,$55,$55
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $76
	DB $77
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$78,$79,$1c,$1c,$1c,$1c,$1c
	DB $55
	DB $78
	DB $79
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$76,$7a,$7b,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $76
	DB $7a
	DB $7b
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$7c,$79,$1c,$1c,$1c,$1c
	DB $59
	DB $59
	DB $7c
	DB $79
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$76,$7a,$7b,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $76
	DB $7a
	DB $7b
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$7c,$79,$1c,$1c,$1c,$1c
	DB $59
	DB $59
	DB $7c
	DB $79
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$76,$7a,$7b,$59,$59,$59
	DB $1c
	DB $1c
	DB $76
	DB $7a
	DB $7b
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DB $59
	DB $7d
	DB $5c
	DB $5c
	DB $5c
	DB $7e
	DB $59
	DB $59
	; 	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DB $59
	DB $7d
	DB $5c
	DB $5c
	DB $5c
	DB $7e
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$7c,$79,$1c,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $7c
	DB $79
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$76,$7a,$7b,$59,$59,$59
	DB $1c
	DB $1c
	DB $76
	DB $7a
	DB $7b
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DB $59
	DB $7d
	DB $5c
	DB $5c
	DB $5c
	DB $7e
	DB $59
	DB $59
	; 	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DB $59
	DB $7d
	DB $5c
	DB $5c
	DB $5c
	DB $7e
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$7c,$79,$1c,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $7c
	DB $79
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$76,$7a,$7b,$59,$7f,$80,$5f
	DB $1c
	DB $76
	DB $7a
	DB $7b
	DB $59
	DB $7f
	DB $80
	DB $5f
	; 	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DB $5f
	DB $81
	DB $60
	DB $60
	DB $60
	DB $82
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DB $5f
	DB $81
	DB $60
	DB $60
	DB $60
	DB $82
	DB $5f
	DB $5f
	; 	DATA BYTE $83,$84,$59,$59,$7c,$79,$1c,$1c
	DB $83
	DB $84
	DB $59
	DB $59
	DB $7c
	DB $79
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$76,$7a,$7b,$59,$7f,$80,$5f
	DB $1c
	DB $76
	DB $7a
	DB $7b
	DB $59
	DB $7f
	DB $80
	DB $5f
	; 	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DB $5f
	DB $81
	DB $60
	DB $60
	DB $60
	DB $82
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DB $5f
	DB $81
	DB $60
	DB $60
	DB $60
	DB $82
	DB $5f
	DB $5f
	; 	DATA BYTE $83,$84,$59,$59,$7c,$79,$1c,$1c
	DB $83
	DB $84
	DB $59
	DB $59
	DB $7c
	DB $79
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DB $59
	DB $87
	DB $64
	DB $64
	DB $64
	DB $88
	DB $59
	DB $59
	; 	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DB $59
	DB $87
	DB $64
	DB $64
	DB $64
	DB $88
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DB $59
	DB $87
	DB $64
	DB $64
	DB $64
	DB $88
	DB $59
	DB $59
	; 	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DB $59
	DB $87
	DB $64
	DB $64
	DB $64
	DB $88
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8a
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $8a
	; 	DATA BYTE $8b,$67,$67,$67,$67,$67,$67,$67
	DB $8b
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$8c,$59,$89,$1c,$1c
	DB $67
	DB $67
	DB $67
	DB $8c
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8a
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $8a
	; 	DATA BYTE $8b,$67,$67,$67,$67,$67,$67,$67
	DB $8b
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$8c,$59,$89,$1c,$1c
	DB $67
	DB $67
	DB $67
	DB $8c
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$6a
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	; 	DATA BYTE $59,$59,$59,$59,$7d,$5c,$7e,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $7d
	DB $5c
	DB $7e
	DB $59
	; 	DATA BYTE $59,$59,$59,$8d,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $8d
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$6a
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	; 	DATA BYTE $59,$59,$59,$59,$7d,$5c,$7e,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $7d
	DB $5c
	DB $7e
	DB $59
	; 	DATA BYTE $59,$59,$59,$8d,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $8d
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$6a
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	; 	DATA BYTE $59,$7f,$80,$5f,$81,$60,$82,$5f
	DB $59
	DB $7f
	DB $80
	DB $5f
	DB $81
	DB $60
	DB $82
	DB $5f
	; 	DATA BYTE $83,$84,$59,$8d,$59,$89,$1c,$1c
	DB $83
	DB $84
	DB $59
	DB $8d
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$6a
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	; 	DATA BYTE $59,$7f,$80,$5f,$81,$60,$82,$5f
	DB $59
	DB $7f
	DB $80
	DB $5f
	DB $81
	DB $60
	DB $82
	DB $5f
	; 	DATA BYTE $83,$84,$59,$8d,$59,$89,$1c,$1c
	DB $83
	DB $84
	DB $59
	DB $8d
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$8a,$8b,$67,$67,$67
	DB $1c
	DB $85
	DB $86
	DB $8a
	DB $8b
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$67,$67,$67,$8c,$6a
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $8c
	DB $6a
	; 	DATA BYTE $59,$59,$59,$59,$87,$64,$88,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $87
	DB $64
	DB $88
	DB $59
	; 	DATA BYTE $59,$59,$59,$8d,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $8d
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$8a,$8b,$67,$67,$67
	DB $1c
	DB $85
	DB $86
	DB $8a
	DB $8b
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$67,$67,$67,$8c,$6a
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $8c
	DB $6a
	; 	DATA BYTE $59,$59,$59,$59,$87,$64,$88,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $87
	DB $64
	DB $88
	DB $59
	; 	DATA BYTE $59,$59,$59,$8d,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $8d
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$6a,$59,$8a,$8b,$67
	DB $1c
	DB $85
	DB $86
	DB $6a
	DB $59
	DB $8a
	DB $8b
	DB $67
	; 	DATA BYTE $8c,$8e,$6b,$6b,$8f,$59,$8d,$90
	DB $8c
	DB $8e
	DB $6b
	DB $6b
	DB $8f
	DB $59
	DB $8d
	DB $90
	; 	DATA BYTE $91,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DB $91
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$92,$59,$89,$1c,$1c
	DB $6d
	DB $6d
	DB $6d
	DB $92
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$6a,$59,$8a,$8b,$67
	DB $1c
	DB $85
	DB $86
	DB $6a
	DB $59
	DB $8a
	DB $8b
	DB $67
	; 	DATA BYTE $8c,$8e,$6b,$6b,$8f,$59,$8d,$90
	DB $8c
	DB $8e
	DB $6b
	DB $6b
	DB $8f
	DB $59
	DB $8d
	DB $90
	; 	DATA BYTE $91,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DB $91
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$92,$59,$89,$1c,$1c
	DB $6d
	DB $6d
	DB $6d
	DB $92
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$6a,$59,$6a,$93,$94
	DB $1c
	DB $85
	DB $86
	DB $6a
	DB $59
	DB $6a
	DB $93
	DB $94
	; 	DATA BYTE $8d,$8e,$6b,$6b,$8f,$59,$8d,$59
	DB $8d
	DB $8e
	DB $6b
	DB $6b
	DB $8f
	DB $59
	DB $8d
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$6a,$59,$6a,$93,$94
	DB $1c
	DB $85
	DB $86
	DB $6a
	DB $59
	DB $6a
	DB $93
	DB $94
	; 	DATA BYTE $8d,$8e,$6b,$6b,$8f,$59,$8d,$59
	DB $8d
	DB $8e
	DB $6b
	DB $6b
	DB $8f
	DB $59
	DB $8d
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$6a,$59,$90,$91,$6d
	DB $1c
	DB $85
	DB $86
	DB $6a
	DB $59
	DB $90
	DB $91
	DB $6d
	; 	DATA BYTE $92,$8e,$6b,$6b,$8f,$59,$8d,$59
	DB $92
	DB $8e
	DB $6b
	DB $6b
	DB $8f
	DB $59
	DB $8d
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$6a,$59,$90,$91,$6d
	DB $1c
	DB $85
	DB $86
	DB $6a
	DB $59
	DB $90
	DB $91
	DB $6d
	; 	DATA BYTE $92,$8e,$6b,$6b,$8f,$59,$8d,$59
	DB $92
	DB $8e
	DB $6b
	DB $6b
	DB $8f
	DB $59
	DB $8d
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$90,$91,$6d,$6d,$6d
	DB $1c
	DB $85
	DB $86
	DB $90
	DB $91
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$92,$59
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $92
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$90,$91,$6d,$6d,$6d
	DB $1c
	DB $85
	DB $86
	DB $90
	DB $91
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$92,$59
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $92
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$85,$86,$59,$59,$59,$59,$59
	DB $1c
	DB $85
	DB $86
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$89,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $89
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$95,$96,$97,$59,$59,$59,$59
	DB $1c
	DB $95
	DB $96
	DB $97
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DB $59
	DB $7d
	DB $5c
	DB $5c
	DB $5c
	DB $7e
	DB $59
	DB $59
	; 	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DB $59
	DB $7d
	DB $5c
	DB $5c
	DB $5c
	DB $7e
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$98,$99,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $98
	DB $99
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$95,$96,$97,$59,$59,$59,$59
	DB $1c
	DB $95
	DB $96
	DB $97
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DB $59
	DB $7d
	DB $5c
	DB $5c
	DB $5c
	DB $7e
	DB $59
	DB $59
	; 	DATA BYTE $59,$7d,$5c,$5c,$5c,$7e,$59,$59
	DB $59
	DB $7d
	DB $5c
	DB $5c
	DB $5c
	DB $7e
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$98,$99,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $98
	DB $99
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$95,$96,$97,$7f,$80,$5f
	DB $1c
	DB $1c
	DB $95
	DB $96
	DB $97
	DB $7f
	DB $80
	DB $5f
	; 	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DB $5f
	DB $81
	DB $60
	DB $60
	DB $60
	DB $82
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DB $5f
	DB $81
	DB $60
	DB $60
	DB $60
	DB $82
	DB $5f
	DB $5f
	; 	DATA BYTE $83,$84,$59,$98,$99,$1c,$1c,$1c
	DB $83
	DB $84
	DB $59
	DB $98
	DB $99
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$95,$96,$97,$7f,$80,$5f
	DB $1c
	DB $1c
	DB $95
	DB $96
	DB $97
	DB $7f
	DB $80
	DB $5f
	; 	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DB $5f
	DB $81
	DB $60
	DB $60
	DB $60
	DB $82
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$81,$60,$60,$60,$82,$5f,$5f
	DB $5f
	DB $81
	DB $60
	DB $60
	DB $60
	DB $82
	DB $5f
	DB $5f
	; 	DATA BYTE $83,$84,$59,$98,$99,$1c,$1c,$1c
	DB $83
	DB $84
	DB $59
	DB $98
	DB $99
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$95,$96,$97,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $95
	DB $96
	DB $97
	DB $59
	DB $59
	; 	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DB $59
	DB $87
	DB $64
	DB $64
	DB $64
	DB $88
	DB $59
	DB $59
	; 	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DB $59
	DB $87
	DB $64
	DB $64
	DB $64
	DB $88
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$98,$99,$1c,$1c,$1c,$1c
	DB $59
	DB $59
	DB $98
	DB $99
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$95,$96,$97,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $95
	DB $96
	DB $97
	DB $59
	DB $59
	; 	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DB $59
	DB $87
	DB $64
	DB $64
	DB $64
	DB $88
	DB $59
	DB $59
	; 	DATA BYTE $59,$87,$64,$64,$64,$88,$59,$59
	DB $59
	DB $87
	DB $64
	DB $64
	DB $64
	DB $88
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$98,$99,$1c,$1c,$1c,$1c
	DB $59
	DB $59
	DB $98
	DB $99
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$95,$9a,$75,$75
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $95
	DB $9a
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$9b,$99,$1c,$1c,$1c,$1c,$1c
	DB $75
	DB $9b
	DB $99
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$95,$9a,$75,$75
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $95
	DB $9a
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$9b,$99,$1c,$1c,$1c,$1c,$1c
	DB $75
	DB $9b
	DB $99
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$9c,$9d,$55,$55
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $9c
	DB $9d
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$9e,$9f,$1c,$1c,$1c,$1c,$1c
	DB $55
	DB $9e
	DB $9f
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$9c,$9d,$55,$55
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $9c
	DB $9d
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$9e,$9f,$1c,$1c,$1c,$1c,$1c
	DB $55
	DB $9e
	DB $9f
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$9c,$a0,$a1,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $9c
	DB $a0
	DB $a1
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$a2,$a3,$9f,$1c,$1c,$1c,$1c
	DB $59
	DB $a2
	DB $a3
	DB $9f
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$9c,$a0,$a1,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $9c
	DB $a0
	DB $a1
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$a2,$a3,$9f,$1c,$1c,$1c,$1c
	DB $59
	DB $a2
	DB $a3
	DB $9f
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$9c,$a0,$a1,$59,$59,$59
	DB $1c
	DB $1c
	DB $9c
	DB $a0
	DB $a1
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DB $59
	DB $a4
	DB $5c
	DB $5c
	DB $5c
	DB $a5
	DB $59
	DB $59
	; 	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DB $59
	DB $a4
	DB $5c
	DB $5c
	DB $5c
	DB $a5
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$a2,$a3,$9f,$1c,$1c,$1c
	DB $59
	DB $59
	DB $a2
	DB $a3
	DB $9f
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$9c,$a0,$a1,$59,$59,$59
	DB $1c
	DB $1c
	DB $9c
	DB $a0
	DB $a1
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DB $59
	DB $a4
	DB $5c
	DB $5c
	DB $5c
	DB $a5
	DB $59
	DB $59
	; 	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DB $59
	DB $a4
	DB $5c
	DB $5c
	DB $5c
	DB $a5
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$a2,$a3,$9f,$1c,$1c,$1c
	DB $59
	DB $59
	DB $a2
	DB $a3
	DB $9f
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$9c,$a0,$a1,$59,$a6,$a7,$5f
	DB $1c
	DB $9c
	DB $a0
	DB $a1
	DB $59
	DB $a6
	DB $a7
	DB $5f
	; 	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DB $5f
	DB $a8
	DB $60
	DB $60
	DB $60
	DB $a9
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DB $5f
	DB $a8
	DB $60
	DB $60
	DB $60
	DB $a9
	DB $5f
	DB $5f
	; 	DATA BYTE $aa,$ab,$59,$a2,$a3,$9f,$1c,$1c
	DB $aa
	DB $ab
	DB $59
	DB $a2
	DB $a3
	DB $9f
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$9c,$a0,$a1,$59,$a6,$a7,$5f
	DB $1c
	DB $9c
	DB $a0
	DB $a1
	DB $59
	DB $a6
	DB $a7
	DB $5f
	; 	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DB $5f
	DB $a8
	DB $60
	DB $60
	DB $60
	DB $a9
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DB $5f
	DB $a8
	DB $60
	DB $60
	DB $60
	DB $a9
	DB $5f
	DB $5f
	; 	DATA BYTE $aa,$ab,$59,$a2,$a3,$9f,$1c,$1c
	DB $aa
	DB $ab
	DB $59
	DB $a2
	DB $a3
	DB $9f
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DB $59
	DB $ad
	DB $64
	DB $64
	DB $64
	DB $ae
	DB $59
	DB $59
	; 	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DB $59
	DB $ad
	DB $64
	DB $64
	DB $64
	DB $ae
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DB $59
	DB $ad
	DB $64
	DB $64
	DB $64
	DB $ae
	DB $59
	DB $59
	; 	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DB $59
	DB $ad
	DB $64
	DB $64
	DB $64
	DB $ae
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $b0
	; 	DATA BYTE $67,$67,$67,$67,$67,$67,$67,$67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$b1,$6a,$af,$1c,$1c
	DB $67
	DB $67
	DB $67
	DB $b1
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $b0
	; 	DATA BYTE $67,$67,$67,$67,$67,$67,$67,$67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$b1,$6a,$af,$1c,$1c
	DB $67
	DB $67
	DB $67
	DB $b1
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8d
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $8d
	; 	DATA BYTE $59,$59,$59,$59,$a4,$5c,$a5,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $a4
	DB $5c
	DB $a5
	DB $59
	; 	DATA BYTE $59,$59,$59,$b2,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $b2
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8d
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $8d
	; 	DATA BYTE $59,$59,$59,$59,$a4,$5c,$a5,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $a4
	DB $5c
	DB $a5
	DB $59
	; 	DATA BYTE $59,$59,$59,$b2,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $b2
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8d
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $8d
	; 	DATA BYTE $59,$a6,$a7,$5f,$a8,$60,$a9,$5f
	DB $59
	DB $a6
	DB $a7
	DB $5f
	DB $a8
	DB $60
	DB $a9
	DB $5f
	; 	DATA BYTE $aa,$ab,$59,$b2,$6a,$af,$1c,$1c
	DB $aa
	DB $ab
	DB $59
	DB $b2
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$8d
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $8d
	; 	DATA BYTE $59,$a6,$a7,$5f,$a8,$60,$a9,$5f
	DB $59
	DB $a6
	DB $a7
	DB $5f
	DB $a8
	DB $60
	DB $a9
	DB $5f
	; 	DATA BYTE $aa,$ab,$59,$b2,$6a,$af,$1c,$1c
	DB $aa
	DB $ab
	DB $59
	DB $b2
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$b0,$67,$67,$67,$67
	DB $1c
	DB $ac
	DB $69
	DB $b0
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$67,$67,$67,$b1,$8d
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $b1
	DB $8d
	; 	DATA BYTE $59,$59,$59,$59,$ad,$64,$ae,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $ad
	DB $64
	DB $ae
	DB $59
	; 	DATA BYTE $59,$59,$59,$b2,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $b2
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$b0,$67,$67,$67,$67
	DB $1c
	DB $ac
	DB $69
	DB $b0
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$67,$67,$67,$b1,$8d
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $b1
	DB $8d
	; 	DATA BYTE $59,$59,$59,$59,$ad,$64,$ae,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $ad
	DB $64
	DB $ae
	DB $59
	; 	DATA BYTE $59,$59,$59,$b2,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $b2
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$8d,$59,$b0,$67,$67
	DB $1c
	DB $ac
	DB $69
	DB $8d
	DB $59
	DB $b0
	DB $67
	DB $67
	; 	DATA BYTE $b1,$b3,$6b,$6b,$b4,$59,$b2,$b5
	DB $b1
	DB $b3
	DB $6b
	DB $6b
	DB $b4
	DB $59
	DB $b2
	DB $b5
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$b6,$6a,$af,$1c,$1c
	DB $6d
	DB $6d
	DB $6d
	DB $b6
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$8d,$59,$b0,$67,$67
	DB $1c
	DB $ac
	DB $69
	DB $8d
	DB $59
	DB $b0
	DB $67
	DB $67
	; 	DATA BYTE $b1,$b3,$6b,$6b,$b4,$59,$b2,$b5
	DB $b1
	DB $b3
	DB $6b
	DB $6b
	DB $b4
	DB $59
	DB $b2
	DB $b5
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$b6,$6a,$af,$1c,$1c
	DB $6d
	DB $6d
	DB $6d
	DB $b6
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$8d,$59,$8d,$b7,$b8
	DB $1c
	DB $ac
	DB $69
	DB $8d
	DB $59
	DB $8d
	DB $b7
	DB $b8
	; 	DATA BYTE $b2,$b3,$6b,$6b,$b4,$59,$b2,$59
	DB $b2
	DB $b3
	DB $6b
	DB $6b
	DB $b4
	DB $59
	DB $b2
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$8d,$59,$8d,$b7,$b8
	DB $1c
	DB $ac
	DB $69
	DB $8d
	DB $59
	DB $8d
	DB $b7
	DB $b8
	; 	DATA BYTE $b2,$b3,$6b,$6b,$b4,$59,$b2,$59
	DB $b2
	DB $b3
	DB $6b
	DB $6b
	DB $b4
	DB $59
	DB $b2
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$8d,$59,$b5,$6d,$6d
	DB $1c
	DB $ac
	DB $69
	DB $8d
	DB $59
	DB $b5
	DB $6d
	DB $6d
	; 	DATA BYTE $b6,$b3,$6b,$6b,$b4,$59,$b2,$59
	DB $b6
	DB $b3
	DB $6b
	DB $6b
	DB $b4
	DB $59
	DB $b2
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$8d,$59,$b5,$6d,$6d
	DB $1c
	DB $ac
	DB $69
	DB $8d
	DB $59
	DB $b5
	DB $6d
	DB $6d
	; 	DATA BYTE $b6,$b3,$6b,$6b,$b4,$59,$b2,$59
	DB $b6
	DB $b3
	DB $6b
	DB $6b
	DB $b4
	DB $59
	DB $b2
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$b5,$6d,$6d,$6d,$6d
	DB $1c
	DB $ac
	DB $69
	DB $b5
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$b6,$59
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $b6
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$b5,$6d,$6d,$6d,$6d
	DB $1c
	DB $ac
	DB $69
	DB $b5
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$b6,$59
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $b6
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$ac,$69,$59,$59,$59,$59,$59
	DB $1c
	DB $ac
	DB $69
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$6a,$af,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $6a
	DB $af
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$b9,$ba,$bb,$59,$59,$59,$59
	DB $1c
	DB $b9
	DB $ba
	DB $bb
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DB $59
	DB $a4
	DB $5c
	DB $5c
	DB $5c
	DB $a5
	DB $59
	DB $59
	; 	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DB $59
	DB $a4
	DB $5c
	DB $5c
	DB $5c
	DB $a5
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$bc,$bd,$be,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $bc
	DB $bd
	DB $be
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$b9,$ba,$bb,$59,$59,$59,$59
	DB $1c
	DB $b9
	DB $ba
	DB $bb
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DB $59
	DB $a4
	DB $5c
	DB $5c
	DB $5c
	DB $a5
	DB $59
	DB $59
	; 	DATA BYTE $59,$a4,$5c,$5c,$5c,$a5,$59,$59
	DB $59
	DB $a4
	DB $5c
	DB $5c
	DB $5c
	DB $a5
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$bc,$bd,$be,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $bc
	DB $bd
	DB $be
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$b9,$ba,$bb,$a6,$a7,$5f
	DB $1c
	DB $1c
	DB $b9
	DB $ba
	DB $bb
	DB $a6
	DB $a7
	DB $5f
	; 	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DB $5f
	DB $a8
	DB $60
	DB $60
	DB $60
	DB $a9
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DB $5f
	DB $a8
	DB $60
	DB $60
	DB $60
	DB $a9
	DB $5f
	DB $5f
	; 	DATA BYTE $aa,$ab,$bc,$bd,$be,$1c,$1c,$1c
	DB $aa
	DB $ab
	DB $bc
	DB $bd
	DB $be
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$b9,$ba,$bb,$a6,$a7,$5f
	DB $1c
	DB $1c
	DB $b9
	DB $ba
	DB $bb
	DB $a6
	DB $a7
	DB $5f
	; 	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DB $5f
	DB $a8
	DB $60
	DB $60
	DB $60
	DB $a9
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$a8,$60,$60,$60,$a9,$5f,$5f
	DB $5f
	DB $a8
	DB $60
	DB $60
	DB $60
	DB $a9
	DB $5f
	DB $5f
	; 	DATA BYTE $aa,$ab,$bc,$bd,$be,$1c,$1c,$1c
	DB $aa
	DB $ab
	DB $bc
	DB $bd
	DB $be
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$b9,$ba,$bb,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $b9
	DB $ba
	DB $bb
	DB $59
	DB $59
	; 	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DB $59
	DB $ad
	DB $64
	DB $64
	DB $64
	DB $ae
	DB $59
	DB $59
	; 	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DB $59
	DB $ad
	DB $64
	DB $64
	DB $64
	DB $ae
	DB $59
	DB $59
	; 	DATA BYTE $59,$bc,$bd,$be,$1c,$1c,$1c,$1c
	DB $59
	DB $bc
	DB $bd
	DB $be
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$b9,$ba,$bb,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $b9
	DB $ba
	DB $bb
	DB $59
	DB $59
	; 	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DB $59
	DB $ad
	DB $64
	DB $64
	DB $64
	DB $ae
	DB $59
	DB $59
	; 	DATA BYTE $59,$ad,$64,$64,$64,$ae,$59,$59
	DB $59
	DB $ad
	DB $64
	DB $64
	DB $64
	DB $ae
	DB $59
	DB $59
	; 	DATA BYTE $59,$bc,$bd,$be,$1c,$1c,$1c,$1c
	DB $59
	DB $bc
	DB $bd
	DB $be
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$b9,$bf,$75,$75
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $b9
	DB $bf
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$c0,$be,$1c,$1c,$1c,$1c,$1c
	DB $75
	DB $c0
	DB $be
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$b9,$bf,$75,$75
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $b9
	DB $bf
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$c0,$be,$1c,$1c,$1c,$1c,$1c
	DB $75
	DB $c0
	DB $be
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$c1,$c2,$55,$55
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $c1
	DB $c2
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$c3,$c4,$1c,$1c,$1c,$1c,$1c
	DB $55
	DB $c3
	DB $c4
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$c1,$c2,$55,$55
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $c1
	DB $c2
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$55,$55,$55,$55,$55,$55,$55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	DB $55
	; 	DATA BYTE $55,$c3,$c4,$1c,$1c,$1c,$1c,$1c
	DB $55
	DB $c3
	DB $c4
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$c1,$c5,$59,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $c1
	DB $c5
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$c6,$c7,$c4,$1c,$1c,$1c,$1c
	DB $59
	DB $c6
	DB $c7
	DB $c4
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$c1,$c5,$59,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $c1
	DB $c5
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$c6,$c7,$c4,$1c,$1c,$1c,$1c
	DB $59
	DB $c6
	DB $c7
	DB $c4
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$c1,$c5,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $c1
	DB $c5
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DB $59
	DB $c8
	DB $5c
	DB $5c
	DB $5c
	DB $c9
	DB $59
	DB $59
	; 	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DB $59
	DB $c8
	DB $5c
	DB $5c
	DB $5c
	DB $c9
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$c6,$c7,$c4,$1c,$1c,$1c
	DB $59
	DB $59
	DB $c6
	DB $c7
	DB $c4
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$c1,$c5,$59,$59,$59,$59
	DB $1c
	DB $1c
	DB $c1
	DB $c5
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DB $59
	DB $c8
	DB $5c
	DB $5c
	DB $5c
	DB $c9
	DB $59
	DB $59
	; 	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DB $59
	DB $c8
	DB $5c
	DB $5c
	DB $5c
	DB $c9
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$c6,$c7,$c4,$1c,$1c,$1c
	DB $59
	DB $59
	DB $c6
	DB $c7
	DB $c4
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$c1,$c5,$59,$59,$ca,$cb,$5f
	DB $1c
	DB $c1
	DB $c5
	DB $59
	DB $59
	DB $ca
	DB $cb
	DB $5f
	; 	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DB $5f
	DB $cc
	DB $60
	DB $60
	DB $60
	DB $cd
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DB $5f
	DB $cc
	DB $60
	DB $60
	DB $60
	DB $cd
	DB $5f
	DB $5f
	; 	DATA BYTE $ce,$cf,$59,$c6,$c7,$c4,$1c,$1c
	DB $ce
	DB $cf
	DB $59
	DB $c6
	DB $c7
	DB $c4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$c1,$c5,$59,$59,$ca,$cb,$5f
	DB $1c
	DB $c1
	DB $c5
	DB $59
	DB $59
	DB $ca
	DB $cb
	DB $5f
	; 	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DB $5f
	DB $cc
	DB $60
	DB $60
	DB $60
	DB $cd
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DB $5f
	DB $cc
	DB $60
	DB $60
	DB $60
	DB $cd
	DB $5f
	DB $5f
	; 	DATA BYTE $ce,$cf,$59,$c6,$c7,$c4,$1c,$1c
	DB $ce
	DB $cf
	DB $59
	DB $c6
	DB $c7
	DB $c4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DB $59
	DB $d1
	DB $64
	DB $64
	DB $64
	DB $d2
	DB $59
	DB $59
	; 	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DB $59
	DB $d1
	DB $64
	DB $64
	DB $64
	DB $d2
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DB $59
	DB $d1
	DB $64
	DB $64
	DB $64
	DB $d2
	DB $59
	DB $59
	; 	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DB $59
	DB $d1
	DB $64
	DB $64
	DB $64
	DB $d2
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$d5
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d5
	; 	DATA BYTE $67,$67,$67,$67,$67,$67,$67,$67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$d6,$d7,$d3,$d4,$1c,$1c
	DB $67
	DB $67
	DB $d6
	DB $d7
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$d5
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d5
	; 	DATA BYTE $67,$67,$67,$67,$67,$67,$67,$67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$d6,$d7,$d3,$d4,$1c,$1c
	DB $67
	DB $67
	DB $d6
	DB $d7
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b2
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $b2
	; 	DATA BYTE $59,$59,$59,$59,$c8,$5c,$c9,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $c8
	DB $5c
	DB $c9
	DB $59
	; 	DATA BYTE $59,$59,$59,$69,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $69
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b2
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $b2
	; 	DATA BYTE $59,$59,$59,$59,$c8,$5c,$c9,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $c8
	DB $5c
	DB $c9
	DB $59
	; 	DATA BYTE $59,$59,$59,$69,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $69
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b2
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $b2
	; 	DATA BYTE $59,$ca,$cb,$5f,$cc,$60,$cd,$5f
	DB $59
	DB $ca
	DB $cb
	DB $5f
	DB $cc
	DB $60
	DB $cd
	DB $5f
	; 	DATA BYTE $ce,$cf,$59,$69,$d3,$d4,$1c,$1c
	DB $ce
	DB $cf
	DB $59
	DB $69
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$b2
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $b2
	; 	DATA BYTE $59,$ca,$cb,$5f,$cc,$60,$cd,$5f
	DB $59
	DB $ca
	DB $cb
	DB $5f
	DB $cc
	DB $60
	DB $cd
	DB $5f
	; 	DATA BYTE $ce,$cf,$59,$69,$d3,$d4,$1c,$1c
	DB $ce
	DB $cf
	DB $59
	DB $69
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$d5,$67,$67,$67,$67
	DB $1c
	DB $d0
	DB $59
	DB $d5
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$67,$67,$d6,$d7,$b2
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $d6
	DB $d7
	DB $b2
	; 	DATA BYTE $59,$59,$59,$59,$d1,$64,$d2,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d1
	DB $64
	DB $d2
	DB $59
	; 	DATA BYTE $59,$59,$59,$69,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $69
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$d5,$67,$67,$67,$67
	DB $1c
	DB $d0
	DB $59
	DB $d5
	DB $67
	DB $67
	DB $67
	DB $67
	; 	DATA BYTE $67,$67,$67,$67,$67,$d6,$d7,$b2
	DB $67
	DB $67
	DB $67
	DB $67
	DB $67
	DB $d6
	DB $d7
	DB $b2
	; 	DATA BYTE $59,$59,$59,$59,$d1,$64,$d2,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d1
	DB $64
	DB $d2
	DB $59
	; 	DATA BYTE $59,$59,$59,$69,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $69
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$b2,$59,$d5,$67,$d6
	DB $1c
	DB $d0
	DB $59
	DB $b2
	DB $59
	DB $d5
	DB $67
	DB $d6
	; 	DATA BYTE $d7,$d8,$6b,$6b,$d9,$59,$69,$da
	DB $d7
	DB $d8
	DB $6b
	DB $6b
	DB $d9
	DB $59
	DB $69
	DB $da
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$db,$dc,$d3,$d4,$1c,$1c
	DB $6d
	DB $6d
	DB $db
	DB $dc
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$b2,$59,$d5,$67,$d6
	DB $1c
	DB $d0
	DB $59
	DB $b2
	DB $59
	DB $d5
	DB $67
	DB $d6
	; 	DATA BYTE $d7,$d8,$6b,$6b,$d9,$59,$69,$da
	DB $d7
	DB $d8
	DB $6b
	DB $6b
	DB $d9
	DB $59
	DB $69
	DB $da
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$6d,$6d,$6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$db,$dc,$d3,$d4,$1c,$1c
	DB $6d
	DB $6d
	DB $db
	DB $dc
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$b2,$59,$b2,$dd,$de
	DB $1c
	DB $d0
	DB $59
	DB $b2
	DB $59
	DB $b2
	DB $dd
	DB $de
	; 	DATA BYTE $69,$d8,$6b,$6b,$d9,$59,$69,$59
	DB $69
	DB $d8
	DB $6b
	DB $6b
	DB $d9
	DB $59
	DB $69
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$b2,$59,$b2,$dd,$de
	DB $1c
	DB $d0
	DB $59
	DB $b2
	DB $59
	DB $b2
	DB $dd
	DB $de
	; 	DATA BYTE $69,$d8,$6b,$6b,$d9,$59,$69,$59
	DB $69
	DB $d8
	DB $6b
	DB $6b
	DB $d9
	DB $59
	DB $69
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$b2,$59,$da,$6d,$db
	DB $1c
	DB $d0
	DB $59
	DB $b2
	DB $59
	DB $da
	DB $6d
	DB $db
	; 	DATA BYTE $df,$d8,$6b,$6b,$d9,$59,$69,$59
	DB $df
	DB $d8
	DB $6b
	DB $6b
	DB $d9
	DB $59
	DB $69
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$b2,$59,$da,$6d,$db
	DB $1c
	DB $d0
	DB $59
	DB $b2
	DB $59
	DB $da
	DB $6d
	DB $db
	; 	DATA BYTE $df,$d8,$6b,$6b,$d9,$59,$69,$59
	DB $df
	DB $d8
	DB $6b
	DB $6b
	DB $d9
	DB $59
	DB $69
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$da,$6d,$6d,$6d,$6d
	DB $1c
	DB $d0
	DB $59
	DB $da
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$db,$dc,$59
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $db
	DB $dc
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$da,$6d,$6d,$6d,$6d
	DB $1c
	DB $d0
	DB $59
	DB $da
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	; 	DATA BYTE $6d,$6d,$6d,$6d,$6d,$db,$dc,$59
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $6d
	DB $db
	DB $dc
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$d0,$59,$59,$59,$59,$59,$59
	DB $1c
	DB $d0
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$59,$59,$59,$59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$59,$d3,$d4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $59
	DB $d3
	DB $d4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$e0,$e1,$59,$59,$59,$59,$59
	DB $1c
	DB $e0
	DB $e1
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DB $59
	DB $c8
	DB $5c
	DB $5c
	DB $5c
	DB $c9
	DB $59
	DB $59
	; 	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DB $59
	DB $c8
	DB $5c
	DB $5c
	DB $5c
	DB $c9
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$e2,$e3,$e4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $e2
	DB $e3
	DB $e4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$e0,$e1,$59,$59,$59,$59,$59
	DB $1c
	DB $e0
	DB $e1
	DB $59
	DB $59
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DB $59
	DB $c8
	DB $5c
	DB $5c
	DB $5c
	DB $c9
	DB $59
	DB $59
	; 	DATA BYTE $59,$c8,$5c,$5c,$5c,$c9,$59,$59
	DB $59
	DB $c8
	DB $5c
	DB $5c
	DB $5c
	DB $c9
	DB $59
	DB $59
	; 	DATA BYTE $59,$59,$59,$e2,$e3,$e4,$1c,$1c
	DB $59
	DB $59
	DB $59
	DB $e2
	DB $e3
	DB $e4
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$e0,$e1,$59,$ca,$cb,$5f
	DB $1c
	DB $1c
	DB $e0
	DB $e1
	DB $59
	DB $ca
	DB $cb
	DB $5f
	; 	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DB $5f
	DB $cc
	DB $60
	DB $60
	DB $60
	DB $cd
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DB $5f
	DB $cc
	DB $60
	DB $60
	DB $60
	DB $cd
	DB $5f
	DB $5f
	; 	DATA BYTE $ce,$cf,$e2,$e3,$e4,$1c,$1c,$1c
	DB $ce
	DB $cf
	DB $e2
	DB $e3
	DB $e4
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$e0,$e1,$59,$ca,$cb,$5f
	DB $1c
	DB $1c
	DB $e0
	DB $e1
	DB $59
	DB $ca
	DB $cb
	DB $5f
	; 	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DB $5f
	DB $cc
	DB $60
	DB $60
	DB $60
	DB $cd
	DB $5f
	DB $5f
	; 	DATA BYTE $5f,$cc,$60,$60,$60,$cd,$5f,$5f
	DB $5f
	DB $cc
	DB $60
	DB $60
	DB $60
	DB $cd
	DB $5f
	DB $5f
	; 	DATA BYTE $ce,$cf,$e2,$e3,$e4,$1c,$1c,$1c
	DB $ce
	DB $cf
	DB $e2
	DB $e3
	DB $e4
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$e0,$e1,$59,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $e0
	DB $e1
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DB $59
	DB $d1
	DB $64
	DB $64
	DB $64
	DB $d2
	DB $59
	DB $59
	; 	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DB $59
	DB $d1
	DB $64
	DB $64
	DB $64
	DB $d2
	DB $59
	DB $59
	; 	DATA BYTE $59,$e2,$e3,$e4,$1c,$1c,$1c,$1c
	DB $59
	DB $e2
	DB $e3
	DB $e4
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$e0,$e1,$59,$59,$59
	DB $1c
	DB $1c
	DB $1c
	DB $e0
	DB $e1
	DB $59
	DB $59
	DB $59
	; 	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DB $59
	DB $d1
	DB $64
	DB $64
	DB $64
	DB $d2
	DB $59
	DB $59
	; 	DATA BYTE $59,$d1,$64,$64,$64,$d2,$59,$59
	DB $59
	DB $d1
	DB $64
	DB $64
	DB $64
	DB $d2
	DB $59
	DB $59
	; 	DATA BYTE $59,$e2,$e3,$e4,$1c,$1c,$1c,$1c
	DB $59
	DB $e2
	DB $e3
	DB $e4
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$e0,$e5,$75,$75
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $e0
	DB $e5
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$e6,$e4,$1c,$1c,$1c,$1c,$1c
	DB $75
	DB $e6
	DB $e4
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	; 	DATA BYTE $1c,$1c,$1c,$1c,$e0,$e5,$75,$75
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $e0
	DB $e5
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$75,$75,$75,$75,$75,$75,$75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	DB $75
	; 	DATA BYTE $75,$e6,$e4,$1c,$1c,$1c,$1c,$1c
	DB $75
	DB $e6
	DB $e4
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	DB $1c
	;
	; CVBasic epilogue (BASIC compiler for Colecovision)
	;
	; by Oscar Toledo G.
	; https://nanochess.org/
	;
	; Creation date: Feb/27/2024.
	; Revision date: Feb/29/2024. Added joystick, keypad, frame, random, and
	;                             read_pointer variables.
	; Revision date: Mar/04/2024. Added music player.
	; Revision date: Mar/05/2024. Added support for Sega SG1000.
	; Revision date: Mar/12/2024. Added support for MSX.
	; Revision date: Mar/13/2024. Added Pletter decompressor.
	; Revision date: Mar/19/2024. Added support for sprite flicker.
	; Revision date: Apr/11/2024. Added support for Super Game Module.
	; Revision date: Apr/13/2024. Updates LFSR in interruption handler.
	; Revision date: Apr/26/2024. All code moved to cvbasic_prologue.asm so it
	;                             can remain accessible in bank 0 (bank switching).
	;

	org BASE_RAM

sprites:
	rb 128
sprite_data:
	rb 4
frame:
	rb 2
read_pointer:
	rb 2
cursor:
	rb 2
lfsr:
	rb 2
mode:
	rb 1
flicker:
	rb 1
joy1_data:
	rb 1
joy2_data:
	rb 1
key1_data:
	rb 1
key2_data:
	rb 1
ntsc:
	rb 1

    if CVBASIC_MUSIC_PLAYER
music_tick:             rb 1
music_mode:             rb 1

    if CVBASIC_BANK_SWITCHING
music_bank:             rb 1
    endif
music_start:		rb 2
music_pointer:		rb 2
music_playing:		rb 1
music_timing:		rb 1
music_note_counter:	rb 1
music_instrument_1:	rb 1
music_counter_1:	rb 1
music_note_1:		rb 1
music_instrument_2:	rb 1
music_counter_2:	rb 1
music_note_2:		rb 1
music_instrument_3:	rb 1
music_counter_3:	rb 1
music_note_3:		rb 1
music_counter_4:	rb 1
music_drum:		rb 1

audio_freq1:		rb 2
audio_freq2:		rb 2
audio_freq3:		rb 2
audio_noise:		rb 1
audio_mix:		rb 1
audio_vol1:		rb 1
audio_vol2:		rb 1
audio_vol3:		rb 1

audio_control:		rb 1
audio_vol4hw:		rb 1
    endif

    if SGM
	org $2000	; Start for variables.
    endif
cvb_#X:	rb 2

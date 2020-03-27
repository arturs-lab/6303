	seg
	ORG $7000

;	jmp ymz284
;	jmp ay38910
;	jmp ampltest
;	jmp envtest

ymz284 subroutine

ymbase	equ $1000	; external IO triggered in range $1000 - $107f
aybase	equ $1040	; external IO triggered in range $1000 - $107f

ymz284
	pshx
	psha
	ldx #yminit
.1	ldaa	0,x
	staa ymbase
	inx
	ldaa	0,x
	staa ymbase+1
	inx
	cpx #ymend
	bne .1
	pula
	pulx
	rts

ay38910
	pshx
	psha
	ldx #ayinit
.2	ldaa	0,x
	staa aybase
	ldaa	yminit-ayinit,x
	staa ymbase
	inx
	ldaa	0,x
	staa aybase+1
	ldaa	yminit-ayinit,x
	staa ymbase+1
	inx
	cpx #ayend
	bne .2
	pula
	pulx
	rts

ayinit
	dc $00,$40	; A fine
	dc $00,$40	; A coarse
	dc $00,$40	; B fine
	dc $03,$01	; B coarse
	dc $04,$20	; C fine
	dc $05,$01	; C coarse (4b)
	dc $06,$07	; noise (5b)
	dc $07,$07<<3	; mixer
	dc $08,$10	; A level
	dc $09,$10	; B level
	dc $0a,$10	; C level
	dc $0b,$00	; envelope F fine
	dc $0c,$01	; envelope F coarse
	dc $0d,$0e	; envelope shape
ayend equ .

yminit
	dc $00,$20	; A fine
	dc $00,$20	; A coarse
	dc $00,$20	; B fine
	dc $03,$01	; B coarse
	dc $04,$20	; C fine
	dc $05,$01	; C coarse (4b)
	dc $06,$07	; noise (5b)
	dc $07,$07<<3	; mixer
	dc $08,$10	; A level
	dc $09,$00	; B level
	dc $0a,$00	; C level
	dc $0b,$80	; envelope F fine
	dc $0c,$00	; envelope F coarse
	dc $0d,$0e	; envelope shape
ymend equ .

; test amplitude
; first decrease then increase
; repeat 3 times

ampltest
	psha
	pshb
	pshx
	ldaa #$08		; channel A level
	staa ymbase
	staa aybase
	ldab #$04
.7	ldaa #$10
.4	deca			; first decrement level
	staa ymbase+1	; set current level on both chips
	staa aybase+1
	ldx #$ffff		; delay loop
.3	dex
	bne .3
	cmpa #$00
	bne .4		; next level if not 0 yet
.6	inca			; otherwise start incrementing
	staa ymbase+1	; set current level on both chips
	staa aybase+1
	ldx #$ffff		; delay loop
.5	dex
	bne .5
	cmpa #$0f
	bne .6		; done when reached $0f
	decb
	bne .7		; do it 4 times
	pulx
	pulb
	pula
	rts

; test envelope
; set each envelope in turn and wait
envtest
	psha
	pshb
	pshx
	ldaa #$08		; channel A level
	staa ymbase
	staa aybase
	ldaa #$10		; set to 'envelope controlled'
	staa ymbase+1
	staa aybase+1
	ldaa #$0d		; select envelope register on both chips
	staa ymbase
	staa aybase
	ldab #$40
.12	ldaa #$00		; start with envelope #0
	staa ymbase+1
	staa aybase+1
	ldx #$ffff
.8	dex
	bne .8
	decb
	bne .12
	ldab #$40
.13	ldaa #$04		; then envelope #$04
	staa ymbase+1
	staa aybase+1
	ldx #$ffff
.9	dex
	bne .9
	decb
	bne .13
	ldaa #$08		; then envelope #$08 - $0f
.11	staa ymbase+1
	staa aybase+1
	ldab #$40
.14	ldx #$ffff
.10	dex
	bne .10
	decb
	bne .14
	inca
	cmpa #$10
	bne .11
	ldaa #$08		; channel A level
	staa ymbase
	staa aybase
	ldaa #$0f		; set to 'envelope controlled'
	staa ymbase+1
	staa aybase+1
	pulx
	pulb
	pula
	rts

ymz284a
	psha
	ldaa #$00
	staa ymbase
	ldaa #$80
	staa ymbase+1	; set frequency of channel A
	ldaa #$01
	staa ymbase
	ldaa #$01
	staa ymbase+1	; set frequency of channel A
	ldaa #$7
	staa ymbase
	ldaa #$7<<3
	staa ymbase+1	; enable sound, disable noise
	ldaa #$8
	staa ymbase
	ldaa #$0f
	staa ymbase+1	; set volume of channel A
	ldaa #$0c
	staa ymbase
	ldaa #$04
	staa ymbase+1	; envelope frequency
	ldaa #$0d
	staa ymbase
	ldaa #$0d
	staa ymbase+1	; envelope

	pula
	rts

; 3c 36 ce 70 4e a6 00 b7 10 00 08 a6 00 b7 10 01 08 8c 70 6a 26 ef 32 38 39 3c 36 ce 70 32 a6 00 b7 10 40 08 a6 00 b7 10 41 08 8c 70 4e 26 ef 32 38 39 00 40 01 00 02 40 03 01 04 20 05 01 06 07 07 38 08 0f 09 00 0a 00 0b 00 0c 01 0d 0e 00 20 01 00 02 40 03 01 04 20 05 01 06 07 07 38 08 0f 09 00 0a 00 0b 80 0c 00 0d 0e 36 37 3c 86 08 b7 10 00 b7 10 40 c6 04 86 10 4a b7 10 01 b7 10 41 ce ff ff 09 26 fd 81 00 26 ef 4c b7 10 01 b7 10 41 ce ff ff 09 26 fd 81 0f 26 ef 5a 26 d9 38 33 32 39 

;36 37 3c 86 08 b7 10 00 b7 10 40 86 10 b7 10 01 b7 10 41 86 0d b7 10 00 b7 10 40 86 00 b7 10 01 b7 10 41 c6 40 ce ff ff 09 26 fd 5a 26 f7 86 04 b7 10 01 b7 10 41 c6 40 ce ff ff 09 26 fd 5a 26 f7 86 08 b7 10 01 b7 10 41 c6 40 ce ff ff 09 26 fd 5a 26 f7 4c 81 10 26 ea 86 08 b7 10 00 b7 10 40 86 0f b7 10 01 b7 10 41 38 33 32 39 

;36 86 00 b7 10 00 86 40 b7 10 01 86 01 b7 10 00 86 00 b7 10 01 86 02 b7 10 00 86 40 b7 10 01 86 03 b7 10 00 86 02 b7 10 01 86 04 b7 10 00 86 20 b7 10 01 86 05 b7 10 00 86 02 b7 10 01 86 06 b7 10 00 86 10 b7 10 01 86 07 b7 10 00 86 38 b7 10 01 86 08 b7 10 00 86 10 b7 10 01 86 09 b7 10 00 86 00 b7 10 01 86 0a b7 10 00 86 00 b7 10 01 86 0c b7 10 00 86 40 b7 10 01 86 0d b7 10 00 86 0e b7 10 01 32 39


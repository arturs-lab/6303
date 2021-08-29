aytestall	jsr ayporttest
	jsr aytest
	pshx
	pshb
	ldab #$08
.14	ldx #$ffff
.10	dex
	bne .10
	decb
	bne .14
	pulb
	pulx
	jsr ampltest
	jsr envtest
	rts

aytest subroutine

aytest
	pshx
	psha
	ldx #ayinit
.1	ldaa	0,x
	staa AYSEL
	inx
	ldaa	0,x
	staa AYSEL+1
	inx
	cpx #ayend
	bne .1
	pula
	pulx
	rts

ymbase	equ $11d8	; EXTSEL external IO triggered in range $11d4 - $11df

ymz284
	pshx
	psha
	ldx #ayinit
.2	ldaa	0,x
	staa AYSEL
	ldaa	yminit-ayinit,x
	staa ymbase
	inx
	ldaa	0,x
	staa AYSEL+1
	ldaa	yminit-ayinit,x
	staa ymbase+1
	inx
	cpx #ayend
	bne .2
	pula
	pulx
	rts

ayinit
	dc $00,$DC	; A fine A4=$106, C5=$DC E5=$AF FRQout = CPU FRQ / 16 / N
	dc $01,$00	; A coarse
	dc $02,$06	; B fine
	dc $03,$01	; B coarse
	dc $04,$AF	; C fine
	dc $05,$00	; C coarse (4bit)
	dc $06,$07	; noise (5bit)
	dc $07,$38	; mixer
	dc $08,$0f	; A level
	dc $09,$0f	; B level
	dc $0a,$0f	; C level
	dc $0b,$00	; envelope F fine
	dc $0c,$01	; envelope F coarse
	dc $0d,$0e	; envelope shape
ayend equ .

yminit
	dc $00,$0b	; A fine
	dc $01,$02	; A coarse
	dc $02,$0b	; B fine
	dc $03,$02	; B coarse
	dc $04,$0b	; C fine
	dc $05,$02	; C coarse (4bit)
	dc $06,$07	; noise (5bit)
	dc $07,$38	; mixer
	dc $08,$0f	; A level
	dc $09,$0f	; B level
	dc $0a,$0f	; C level
	dc $0b,$80	; envelope F fine
	dc $0c,$00	; envelope F coarse
	dc $0d,$0e	; envelope shape
ymend equ .

setayreg
	psha
	ldaa #00
	staa AYSEL
	ldaa #01
	staa AYSEL+1
	pula
	rts

; test amplitude
; first decrease then increase
; repeat 3 times
tonet	dc $06,$01	; A
	dc $06,$01	; B
	dc $06,$01	; C

ampltest
	psha
	pshb
	pshx
	ldx #tonet	; preset all channels to A440
	ldaa #0
.16	staa AYSEL
	ldab 0,x
	stab AYSEL+1
	inx
	inca
	cmpa #$06
	bne .16

	ldaa #$09		; channel B level
	staa AYSEL
	staa ymbase
	ldaa #$00		; set to off
	staa AYSEL+1
	staa ymbase+1
	ldaa #$0a		; channel C level
	staa AYSEL
	staa ymbase
	ldaa #$00		; set to off
	staa AYSEL+1
	staa ymbase+1
	ldaa #$08		; channel A level
	staa AYSEL
	staa ymbase
	ldx #saycha
	jsr txstring
	ldaa "A"
	jsr txbyte
	ldx #sayamplval
	jsr txstring
	jsr sndwav

	ldaa #$09		; channel B level
	staa AYSEL
	staa ymbase
	ldx #saycha
	jsr txstring
	ldaa "B"
	jsr txbyte
	ldx #sayamplval
	jsr txstring
	jsr sndwav

	ldaa #$0a		; channel C level
	staa AYSEL
	staa ymbase
	ldx #saycha
	jsr txstring
	ldaa "C"
	jsr txbyte
	ldx #sayamplval
	jsr txstring
	jsr sndwav

	jsr txcrlf
	ldaa #$09		; channel B level
	staa ymbase
	staa AYSEL
	ldaa #$00		; set to off
	staa ymbase+1		; set current level on both chips
	staa AYSEL+1
	pulx
	pulb
	pula
	rts

sndwav	ldaa #$ff
.4	inca			; first decrement level
	staa AYSEL+1
	staa ymbase+1		; set current level on both chips
	psha
	jsr txhex
	ldaa #$0d
	jsr txbyte
	pula
	ldx #$ffff		; delay loop
.3	dex
	bne .3
	cmpa #$0f
	bne .4		; next level if not 0 yet
.6	deca			; otherwise start incrementing
	staa ymbase+1		; set current level on both chips
	staa AYSEL+1
	psha
	jsr txhex
	ldaa #$0d
	jsr txbyte
	pula
	ldx #$ffff		; delay loop
.5	dex
	bne .5
	cmpa #$00
	bne .6		; done when reached $0f
	rts

; test envelope
; set each envelope in turn and wait
envtest
	psha
	pshb
	pshx
	ldaa #$0a		; channel C level
	staa AYSEL
	staa ymbase
	ldaa #$00		; set to off
	staa AYSEL+1
	staa ymbase+1
	ldaa #$09		; channel B level
	ldaa #$00		; set to off
	staa AYSEL
	staa ymbase
	staa AYSEL+1
	staa ymbase+1

	ldaa #$08		; channel A level
	staa AYSEL
	staa ymbase
	ldaa #$1f		; set to 'envelope controlled'
	staa AYSEL+1
	staa ymbase+1
	ldaa #$0d		; select envelope register on both chips
	staa ymbase
	staa AYSEL
	ldx #saycha
	jsr txstring
	ldaa "A"
	jsr txbyte
	ldx #sayenvnum
	jsr txstring
	jsr envtdo
	ldaa #$08		; channel A level
	staa AYSEL
	staa ymbase
	ldaa #$00		; set to off
	staa AYSEL+1
	staa ymbase+1

	ldaa #$09		; channel B level
	staa AYSEL
	staa ymbase
	ldaa #$1f		; set to 'envelope controlled'
	staa AYSEL+1
	staa ymbase+1
	ldaa #$0d		; select envelope register on both chips
	staa ymbase
	staa AYSEL
	ldx #saycha
	jsr txstring
	ldaa "B"
	jsr txbyte
	ldx #sayenvnum
	jsr txstring
	jsr envtdo
	ldaa #$09		; channel B level
	staa AYSEL
	staa ymbase
	ldaa #$00		; set to off
	staa AYSEL+1
	staa ymbase+1

	ldaa #$0a		; channel C level
	staa AYSEL
	staa ymbase
	ldaa #$1f		; set to 'envelope controlled'
	staa AYSEL+1
	staa ymbase+1
	ldaa #$0d		; select envelope register on both chips
	staa ymbase
	staa AYSEL
	ldx #saycha
	jsr txstring
	ldaa "C"
	jsr txbyte
	ldx #sayenvnum
	jsr txstring
	jsr envtdo
	ldaa #$0a		; channel C level
	staa AYSEL
	staa ymbase
	ldaa #$00		; set to off
	staa AYSEL+1
	staa ymbase+1

	ldaa #$07		; disable sound
	staa ymbase
	staa AYSEL
	ldaa #$3f		; disable sound
	staa ymbase+1
	staa AYSEL+1
	pulx
	pulb
	pula
	rts

envtdo	ldab #$10
	ldaa #$00
	jsr txhex
	ldaa #$0d
	jsr txbyte
.12	ldaa #$00		; start with envelope #0
	staa ymbase+1
	staa AYSEL+1
	ldx #$ffff
.8	dex
	bne .8
	decb
	bne .12
	ldab #$10
	ldaa #$04
	jsr txhex
	ldaa #$0d
	jsr txbyte
.13	ldaa #$04		; then envelope #$04
	staa ymbase+1
	staa AYSEL+1
	ldx #$ffff
.9	dex
	bne .9
	decb
	bne .13
	ldaa #$08		; then envelope #$08 - $0f
.11	staa ymbase+1
	staa AYSEL+1
	psha
	jsr txhex
	ldaa #$0d
	jsr txbyte
	pula
	ldab #$10
.14	ldx #$ffff
.10	dex
	bne .10
	decb
	bne .14
	inca
	cmpa #$09
	bne .15
	inca
.15	cmpa #$0f
	bne .11
	rts

ayportt subroutine

ayporttest
	ldx #confaya
	ldaa #$78		; port A output B input
	jsr confayAB	; configure AY ports
	ldx #confayb	; send message about testing port
	jsr txstring

	ldab #$00
	stab RAMLO
.1	ldaa #$0e		; address port A
	staa AYSEL
	stab AYSEL+1	; write to port A
	tba
	jsr txhexbyte	; display tested value
	ldaa #" "		; followed by a space
	jsr txbyte
	ldaa #$0f		; address port B
	staa AYSEL
	ldaa AYSEL+1	; read from port B
	jsr txhexbyte	; display tested value
;	jsr txcrlf
	ldaa #$0d
	jsr txbyte
	cmpb AYSEL+1	; compare reg B with value read from port B
	beq .3
	jsr cmpayfail
.3	ldaa #$1
;	jsr dly1
	incb
	bne .1		; continue loop for all 255 values

	ldaa #$0a
	jsr txbyte
	ldx #confayd	; send message about success
	jsr txstring
	ldaa RAMLO
	beq .5
	jsr cmpayfailmsg
	bra .0
.5	ldx #ok
	jsr txstring
	jsr txcrlf

; now test port B as output
.0	ldx #confayc
	ldaa #$b8		; port B output A input
	jsr confayAB	; configure AY ports
	ldx #confayb	; send message about testing port
	jsr txstring
	ldab #$00
	stab RAMLO
.2	ldaa #$0f		; address port B
	staa AYSEL
	stab AYSEL+1		; write to port B
	tba
	jsr txhexbyte	; display tested value
	ldaa #" "		; followed by a space
	jsr txbyte
	ldaa #$0e		; address port A
	staa AYSEL
	ldaa AYSEL+1	; read from port A
	jsr txhexbyte	; display tested value
;	jsr txcrlf
	ldaa #$0d
	jsr txbyte
	cmpb AYSEL+1	; compare reg B with value read from port A
	beq .4
	jsr cmpayfail
.4	ldaa #$1
;	jsr dly1
	incb
	bne .2		; continue loop for all 255 values

	ldaa #$0a
	jsr txbyte
	ldx #confaye	; send message about success
	jsr txstring
	ldaa RAMLO
	bne cmpayfailmsg
	ldx #ok
	jsr txstring
	jsr txcrlf
	bra cmpayxit

cmpayfail
	ldaa #$0a
	stab RAMLO
	jsr txbyte
	rts

cmpayfailmsg
	ldx #fail
	jsr txstring
	jsr txcrlf
cmpayxit
	ldx #confayf
	ldaa #$38		; both ports inputs
	jsr confayAB	; configure AY ports
	rts
		
; configure AY ports A and B 
confayAB
	psha
	jsr txstring
	ldaa #$07		; select register 7
	staa AYSEL
	pula
	staa AYSEL+1		; write to register 7
	rts

ymz284a
	psha
	ldaa #$00
	staa ymbase
	ldaa #$80
	staa ymbase+1		; set frequency of channel A
	ldaa #$01
	staa ymbase
	ldaa #$01
	staa ymbase+1		; set frequency of channel A
	ldaa #$7
	staa ymbase
	ldaa #$7<<3
	staa ymbase+1		; enable sound, disable noise
	ldaa #$8
	staa ymbase
	ldaa #$0f
	staa ymbase+1	; set volume of channel A
	ldaa #$0c
	staa ymbase
	ldaa #$04
	staa ymbase+1		; envelope frequency
	ldaa #$0d
	staa ymbase
	ldaa #$0d
	staa ymbase+1		; envelope

	pula
	rts

confaya	dc "Configuring AY PRA output PRB input",$0d,$0a,$0
confayb	dc "Testing ports",$0d,$0a," W  R",$0d,$0a,$0
confayc	dc "Configuring AY PRA input PRB output",$0d,$0a,$0
confayd	dc "A -> B ",$0
confaye	dc "B -> A ",$0
confayf	dc "Configuring AY PRA and PRB input",$0d,$0a,$0
sayinit	dc "Initializing AY chip",$0d,$0a,$0
sayampl	dc "Testing AY amplitude",$0d,$0a,$0
saycha	dc "Channel ",$0
sayamplval	dc " amplitude: ",$0d,$0a,$0
sayenv	dc "Testing AY envelopes",$0d,$0a,$0
sayenvnum	dc " envelope: ",$0d,$0a,$0

; sn76489 PSG test routines
; f = 1843230/(32*n)
; n = 1843230/(32*f)
; A4 = 131 $83

dlybyte	equ $04	; delay between bytes
dlylong	equ 10	; long delay between frq changes

sn76489t subroutine
sn76489t	jsr sn76init

	ldaa #$90	; channel 0 attenuation
	jsr setsn

	ldx #sn76ch0	
	jsr txstring
	ldab #$80	; channel 0
	jsr sn74frqtest

	ldaa #$9f	; channel 0 attenuation
	jsr setsn

	ldaa #$b0	; channel 1 attenuation
	jsr setsn

	ldx #sn76ch1	
	jsr txstring
	ldab #$a0	; channel 1
	jsr sn74frqtest

	ldaa #$bf	; channel 1 attenuation
	jsr setsn

	ldaa #$d0	; channel 2 attenuation
	jsr setsn

	ldx #sn76ch2	
	jsr txstring
	ldab #$c0	; channel 2
	jsr sn74frqtest

	ldaa #$04
	jsr sn74scan	; full frequency sweep

	ldaa #$df	; channel 2 attenuation
	jsr setsn

; play a chord
	ldx #sn76vol	
	jsr txstring
	ldaa #$80		; channel 0
	ldx #sn76ch1f	; point to tone value
	jsr setsnf		; set register
	ldaa #$a0		; channel 1
	ldx #sn76ch2f	; point to tone value
	jsr setsnf		; set register
	ldaa #$c0		; channel 2
	ldx #sn76ch3f	; point to tone value
	jsr setsnf		; set register

	ldab #$0e		; initial attenuation
.1	tba
	jsr txhexbyte
	ldaa #$0d
	jsr txbyte
	tba
	oraa #$90	; channel 0
	jsr setsn
	tba
	oraa #$b0	; channel 1
	jsr setsn
	tba
	oraa #$d0	; channel 2
	jsr setsn

	ldaa #$04
	jsr dly1

	decb
	bne .1

.3	tba
	jsr txhexbyte
	ldaa #$0d
	jsr txbyte
	tba
	oraa #$90	; channel 0
	jsr setsn
	tba
	oraa #$b0	; channel 1
	jsr setsn
	tba
	oraa #$d0	; channel 2
	jsr setsn

	ldaa #$04
	jsr dly1

	incb
	cmpb #$10
	bne .3

; noise
sn76noise subroutine
	ldx #sn76ch3	
	jsr txstring
	ldaa #$f0	; noise channel attenuation
	jsr setsn

	ldaa #$00	; noise channel N/512
	staa sn76chns

;	ldaa sn76chns
.1	jsr txhex
	ldaa #$0d
	jsr txbyte
	ldaa #$e0	; noise channel
	adda sn76chns
	jsr setsn
	ldaa #$10
	jsr dly1

	inc sn76chns

	ldaa sn76chns
	cmpa #$04
	bne .1

	ldaa #$0a
	jsr txbyte
	ldab #$c0	; channel 2
	ldaa #$08
	jsr sn74scan	; full frequency sweep
	ldaa #$0a
	jsr txbyte

	ldaa sn76chns
.2	jsr txhex
	ldaa #$0d
	jsr txbyte
	ldaa #$e0	; noise channel
	adda sn76chns
	jsr setsn
	ldaa #$10
	jsr dly1

	inc sn76chns

	ldaa sn76chns
	cmpa #$08
	bne .2

	ldaa #$0a
	jsr txbyte
	ldab #$c0	; channel 2
	ldaa #$08
	jsr sn74scan	; full frequency sweep

	ldaa #$ff	; noise channel attenuation
	jsr setsn

	rts

sn74frqtest subroutine on entry B contains channel number
.2	ldx #sn76chna	; send tone value to terminal
	jsr txhexword
	ldaa #$0d
	jsr txbyte
	ldx #sn76chna	; point to tone value again
	tba		; set channel
	jsr setsnf	; play

	ldaa #dlylong
	jsr dly1

	pshb		; save B for later
	ldd sn76chna	; have to use D because can't shift X
	asld
	std sn76chna
	xgdx		; have to use X because can't compare D
	pulb
	cpx #$0400
	bne .2

	ldx #$03ff		; lowest tone
	stx sn76chna	; store it
	ldx #sn76chna	; point to it
	jsr txhexword
	ldaa #$0d
	jsr txbyte
	ldx #sn76chna	; lowest tone
	tba		; set channel
	jsr setsnf	; play

	ldaa #dlylong
	jsr dly1

	ldx #$0004	; restore initial frequency
	stx sn76chna

	rts

sn74scan subroutine on entry B contains channel number, A contains speed
	psha
.1	ldx #sn76chna
	jsr txhexword
	ldaa #$0d
	jsr txbyte
	ldx #sn76chna
	tba		; set channel
	jsr setsnf

	pula
	psha
	pshb
.3	ldab #$ff
.2	decb
	bne .2
	deca
	bne .3
	pulb

	ldx sn76chna
	inx
	stx sn76chna
	cpx #$0400
	bne .1
	pula

	ldx #$0004
	stx sn76chna

	rts

sn76ch0	dc "Channel 0",$0d,$0a,$0
sn76ch1	dc "Channel 1",$0d,$0a,$0
sn76ch2	dc "Channel 2",$0d,$0a,$0
sn76ch3	dc "Noise",$0d,$0a,$0
sn76vol	dc "Volume",$0d,$0a,$0

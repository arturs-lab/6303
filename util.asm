ledtoggle
	eorb LED
	stab LED
	rts

delay subroutine
delay	psha
	ldaa #$ff	; standard delay length
	bra .9
dly1	psha		; or if you call delay1, give delay length in reg A
.9	pshx
.8	jsr dlyxl
	deca
	bne .8
	pulx
	pula
	rts

dlyxl	ldx #$ffff
dlyx	dex
	bne dlyx
	rts
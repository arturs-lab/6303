; routine to call procedure in teh other bank
; address of routine to call is stored in PARMBUF

callbank
	psha
	ldaa #01
	eora ROMB		; flip rom bank bit
	staa ROMB
	pula
	jsr 0,x		; jump to subroutine pointed to by x
	psha
	ldaa #01
	eora ROMB		; flip rom bank bit
	staa ROMB
	pula
	rts


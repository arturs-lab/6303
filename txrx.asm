txhexword subroutine
txhexword
	ldaa 0,x
	jsr txhexbyte
	ldaa 1,x
	jsr txhexbyte
	rts

txhexbyte subroutine	; transmit one byte - two HEX digits value of A
txhexbyte
	psha		; save for later
	lsra		; get high nibble
	lsra
	lsra
	lsra
	jsr txhex

	pula		; get saved A
	psha
	anda #$0f	; work on low nibble
	jsr txhex
	pula		; get saved A
	rts

txhex subroutine	; transmit single HEX digit given in A
txhex
	psha
	pshb
	adda #"0"	; make is ASCII code for 0-9
	cmpa #"9"	; is A > 9?
	bls .1	; no, skip next
	adda #("a" - ":")	; if A > "9" then A + ASCII("a") - ASCII(":") = ASCII("a")
.1	tab
	ldaa TDRE	; wait for transmit register to be empty
.2	bita REG_TRCSR1
	beq .2
	tba
	staa REG_TDR	; send a string out via serial
	pulb
	pula
	rts

txword subroutine ; transmit two bytes given in D
	jsr txbyte
	tba

txbyte subroutine	; transmit byte given in A
txbyte
	psha
	ldaa TDRE	; wait for transmit register to be empty
.1	bita REG_TRCSR1
	beq .1
	pula
	staa REG_TDR	; send a string out via serial
	rts

txstrsp subroutine	; transmit a string whose address is on stack
			; used when string to be sent is immediately after jsr instruction
	pulx
	jsr txstring
	inx		; point to next instruction
	pshx
	rts

txcrlf subroutine
txcrlf		; commonly used piece of code. load CRLF pointer
			; into X and continue, which will drop us into txstring routine
			; this speeds up code
	ldx #crlf

txstring subroutine
txstring
	psha
.0	ldaa TDRE	; wait for transmit register to be empty
.1	bita REG_TRCSR1
	beq .1
	ldaa 0,x
	beq .2
	staa REG_TDR	; send a string out via serial
	inx
	bra .0
.2	pula
	rts


R65hello	dc $0d,$0a,"Hello World!",$0d,$0a,$00
R65ctrlerr	dc "Control register: wrote $00, read $",$00
R65cmderr	dc "Command register: wrote $0a, read $",$00
R65timout	dc $0d,$0a,"Tx ready timed out",$0d,$0a,$00

r6551t	subroutine
r6551t	
	ldaa #$10		; set R6551 control register
	staa R65CTRL
	ldab R65CTRL	; read it back and display error if contents does not match (bad chip?)
	tba
	cmpa #$00
	beq .6
	ldx #R65ctrlerr
	jsr txstring
	tba
	jsr txhexbyte
	ldx #crlf
	jsr txstring
	rts

.6	ldaa #$0a		; set R6551 command register
	staa R65CMND
	ldab R65CMND	; read it back and display error if contents does not match (bad chip?)
	tba
	cmpa #$0a
	beq .5
	ldx #R65cmderr
	jsr txstring
	tba
	jsr txhexbyte
	ldx #crlf
	jsr txstring
	rts

.5	ldab #$10		; repeat test 16 times
	ldaa #$aa
.1	pshb			; save test counter
	psha

	ldaa #$7f		; for troubleshooting
	staa CPLDh

	ldab #$00		; timeout counter
.2	incb
	beq .4		; on timeout jump
	ldaa R65STATUS	; wait for transmitter to be ready

	psha			; save for later value read from status reg
	jsr txhexbyte	; for troubleshooting display it
	ldaa #$0d
	jsr txbyte
	pula			; restore it
	anda #R65_TXE	; is TX register empty?
	beq .2		; loop if not

	ldaa #$3f		; for troubleshooting
	staa CPLDh

	pula			; get last sent byte
	eora #$ff		; invert it
	psha			; save for next send
	staa R65DATA	; and send it

	ldaa #$ff		; for troubleshooting
	staa CPLDh

;	ldaa #$10
;	jsr dly1
	pula			; have to pul A to get to B
	pulb			; get test iteration counter (16 loops total)
	decb
	bne .1
	rts			; done all 16 loops

.4	pula			; remove leftovers from stack
	pulb
	ldx #R65timout	; display error message
	jsr txstring
	rts			; and return

;fast version without testing and delays
r6551f	subroutine
r6551f	
	ldaa #$10		; set R6551 control register
	staa R65CTRL
	ldaa #$0a		; set R6551 command register
	staa R65CMND
	ldx #R65hello
.1	ldab #$00		; timeout counter
.2	incb
	beq .4		; on timeout jump
	ldaa R65STATUS	; wait for transmitter to be ready
	anda #R65_TXE	; is TX register empty?
	beq .2		; loop if not
	ldaa 0,x		; fetch character to be sent
	bne .5
	rts			; exit if end of string
.5	staa R65DATA	; else send it
	inx			; point to next char
	bra .1		; continue loop

.4	pula			; remove leftovers from stack
	pulb
	ldx #R65timout	; display error message
	jsr txstring
	rts			; and return


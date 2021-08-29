
; R6551 ACIA
R65DATA	equ EXTSEL
R65STATUS	equ EXTSEL+1
R65CMND	equ EXTSEL+2
R65CTRL	equ EXTSEL+3
; status register biits
R65_IRQ	equ $80	; bit 7 IRQ flag
R65_DSR	equ $40	; bit 6 
R65_DCD	equ $20	; bit 5 
R65_TXE	equ $10	; bit 4 transmit register empty
R65_RXF	equ $08	; bit 3 receive register full
R65_OVR	equ $04	; bit 2 overrun
R65_FE	equ $02	; bit 1 framing error
R65_PARITY	equ $01	; bit 0 parity error

;;************************************************************************
;; R65INIT   Initialize the Serial Port
;;************************************************************************
R65INIT subroutine
	ldaa #$10		; set R6551 control register
	staa R65CTRL
	ldaa #$0b		; set R6551 command register
	staa R65CMND
	rts

;;************************************************************************
;; OUTR65CHR  Transmit a serial byte from A                            OK
;;
;;************************************************************************
OUTR65CH  subroutine
OUTR65CH	psha
	pshb
	ldab #$00		; timeout counter
.2	incb
	beq .4		; on timeout jump
	ldaa R65STATUS	; wait for transmitter to be ready
	anda #R65_TXE	; is TX register empty?
	beq .2		; loop if not
	pulb
	pula			; fetch character to be sent
	staa R65DATA	; else send it
	rts

.4	pulb			; remove leftovers from stack
	pula
;	ldx #R65timout	; display error message
;	jsr txstring
	rts			; and return

;;************************************************************************
;; INR65CHR  wait for a serial byte and return in A
;;
;;************************************************************************
INR65CHRER subroutine
	LDAA  R65DATA	        ;ON ERROR, FLUSH BUFFER AND CLEAR ERROR FLAG
INR65CHR	LDAA  R65STATUS
	bita	#R65_PARITY		; parity error?
	bne INR65CHRER
	bita	#R65_FE		; framing error?
	bne INR65CHRER
	bita	#R65_OVR		; overrun?
	bne INR65CHRER
	ANDA  #R65_RXF	          ; check if byte available
	BEQ	  INR65CHR           ;WAIT FOR CHARACTER
	LDAA  R65DATA         ;READ RECIEVED CHARACTER
	STAA  RX_BYTE         ;Save in RX_BYTE
	RTS
;;************************************************************************
;; INCHRE  wait for a serial byte and return in A with echo
;;************************************************************************
INR65CHRE  subroutine
        JSR   INR65CHR
        LDAA  FLAGS_A
        ANDA  #$01        ;Is ECHO ON?
        BEQ   INR65CHRE1     ;NO = Skip OUTCHR
        LDAA  RX_BYTE        
        JSR   OUTR65CH
INR65CHRE1 LDAA  RX_BYTE
        RTS
;******************************************************************
; INCHRIF  Input a character if available
; Returns with input character or zero (with C=1) if none available
;******************************************************************
INR65CHRIF	LDAA  R65STATUS
	ANDA  #R65_RXF	          ; check if byte available
	BEQ	  INR65CHRNC         ; No Data Ready,exit
	jsr INR65CHR         ;READ RECIEVED CHARACTER
	CLC                   ; Clear The Carry
	RTS                   ; Return
INR65CHRNC	LDAA  #$00            ; No Data - Return zero and C=1
	SEC
	RTS
       

LCD_RS	equ $01	; 0 - control, 1 - data
LCD_RW	equ $02	; 0 - write, 1 - read
LCD_EN	equ $04	; 0 - disable, 1 - enable

; initialize LCD
lcdinit subroutine
	psha
	pshx
	clra
	ldab #$30	; set mode 8 bit
	jsr lcdwrn
	ldx #$0900
	jsr dlyx
	ldab #$30	; set mode 8 bit
	jsr lcdwrn
	ldx #$0900
	jsr dlyx
	ldab #$30	; set mode 8 bit
	jsr lcdwrn
	ldx #$0080
	jsr dlyx
	ldab #$20	; set mode 4 bit
	jsr lcdwrn
	ldab #$28	; set mode 4 bit
	jsr lcdwrc
	ldab #$0e	; display on/off: display on, cursor on, no blink 
	jsr lcdwrc
	ldab #$01	; clear display
	jsr lcdwrc
	ldab #$06	; entry mode: cursor move, no shift
	jsr lcdwrc
	pulx
	pula
	rts

; print text pointed to by top of stack
; upon return resume execution from first
; instruction after null terminated string
lcdprnsp subroutine
	pulx		; get address of data to be printed
	jsr lcdprn	; print it
	inx		; point to next instruction
	pshx		; save return address
	rts

.2	jsr lcdwrd	; print char in B
	inx		; point ot next
lcdprn
	ldab 0,x	; get next char
	bne .2	; is it zero? no, jump
	rts		; yes return

; returns byte read from LCD in A
lcdrd	subroutine
	pshb
	ldaa #$0f		; upper nibble input
	staa REG_DDRP1
	ldaa #LCD_RW	; read cycle
	staa REG_PORT1
	oraa #LCD_EN	; LCD enable
	staa REG_PORT1
	anda #~LCD_EN	; do this before reading to give LCD time
	ldab REG_PORT1	; read data from LCD
	staa REG_PORT1	; disable LCD
	andb #$f0		; keep only upper nibble
	pshb
	oraa #LCD_EN	; LCD enable
	staa REG_PORT1
	anda #~LCD_EN	; do this before reading to give LCD time
	ldab REG_PORT1	; read data from LCD
	staa REG_PORT1	; disable LCD
	anda #~LCD_RW	; disable read
	staa REG_PORT1
	lsrb			; shift lower nibble where it belongs
	lsrb
	lsrb
	lsrb
	pula			; get upper nibble
	aba			; add it
	ldab #$ff		; upper nibble output
	stab REG_DDRP1
	pulb
	rts

; lcd write byte given in B, A contains 0 for control reg or 1 for data reg
lcdwrc	clra			; zero register A when writing to control register
	bra lcdwr
lcdwrd	ldaa #LCD_RS	; set LCD_RS bit when writing data
lcdwr	psha				; entry point with A already set to correct value; preserve a
.1	jsr lcdrd			; wait for lcd ready
	bita #$80
	bne .1
	pula				; restore a
	pshb
	andb #$f0			; make sure we only use upper nibble
	jsr lcdwrn
	pulb
	aslb
	aslb
	aslb
	aslb
	jsr lcdwrn
	rts

; lcd write nibble given in B7-B4
lcdwrn	anda #~($f0+LCD_EN+LCD_RW)	; make sure that enable bit is not active yet
	aba
	staa REG_PORT1	; write dat to LCD
	oraa #LCD_EN
	staa REG_PORT1	; enable LCD
	nop
	nop
	nop
	anda #~LCD_EN	; disable LCD
	staa REG_PORT1
	rts

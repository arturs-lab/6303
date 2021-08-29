	processor HD6303

; debugging level of sn76 code
SN76DBG = 1
SN76PLS = 0

;dly1	equ $f7dd
;txhex	equ $f805
;txhexword	equ $f7e9
;txbyte	equ $f820
;txhexbyte	equ $f7f4
;txstring	equ $f82e

	INCLUDE 6303vars.asm

	org   $c000	    ;Start of ROM $8000 (originally)

;RESET	subroutine
;	SEI            ;Disable interrupts

;	lds #$00ff		; temporary stack location

	dc "ROM BANK 1"


	org $e000

; swap ROM into RAM

swp2ram ldaa #07		; map bank2 to RAM page 7
	staa RAMB8

	ldx #$ffff		; end of block is $FFFF
	stx DISADD1
	ldx #RAMMID		; destination is bank 2
	stx DISADD2
	ldx #RAMHI		; point X to beginning of ROM
	stx ADDR_HI
swp1	ldaa 0,x
	ldx DISADD2
	staa 0,x        ;store value in destination
	inx
	stx DISADD2
	ldx ADDR_HI
	cpx DISADD1    ;Match on End Adress?
	beq swpex		; exit
	inx
	stx ADDR_HI
	jmp swp1

swpex	rts

swptx	ldaa #"A"
	staa SWPO
	jsr txstrsp
;	jsr lcdinit
;	jsr lcdprnsp
	dc "Running from R"
SWPO	dc "OM",$0d,$0a,$0
	rts
	

	org BANKSW
	INCLUDE callbank.asm

calltest
	ldaa #2
	rts

; sn76
	INCLUDE sn76init.asm
	INCLUDE sn76test.asm
	INCLUDE sn76driver.asm

	INCLUDE notes.asm

;	INCLUDE sn76beverly2.asm
;	INCLUDE sn76mississippi.asm
	INCLUDE sn76light.asm
;	INCLUDE sn76riders.asm

	INCLUDE lcddriver.asm

	org UTIL
	INCLUDE util.asm
	INCLUDE txrx.asm

crlf		dc $0d,$0a,$0

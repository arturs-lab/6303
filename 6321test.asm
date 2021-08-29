; connect port A to port B and CA1 to CB2 and CA2 to CB1
test6321 subroutine

test6321
	jsr conf21Aout	; configure port A as output
	ldx #conf21b	; send message about testing port
	jsr txstring

; temporary test. just display port b forever
;.99	ldaa PRB		; read from PRB
;	jsr txhexbyte	; display tested value
;	ldaa #$0d
;	jsr txbyte
;	ldaa #$2
;	jsr dly1
;	bra .99

	ldab #$00
	stab RAMLO
.1	stab PRA		; write to PRA
	tba			; copy reg B to A
	asla
	asla
	anda #$08
	oraa #$34
	staa CRA
	tba
	jsr txhexbyte	; display tested value
	ldaa #" "		; followed by a space
	jsr txbyte
	ldaa CRB		; read from CRB
	anda #$c0		; keep only interppupt status bits
	beq .7
	ldaa #$aa
.7	jsr txhexbyte	; display interrupt bits value
	ldaa #" "		; followed by a space
	jsr txbyte
	ldaa PRB		; read from PRB
	jsr txhexbyte	; display tested value
;	jsr txcrlf
	ldaa #$0d
	jsr txbyte
	cmpb PRB		; compare reg B with value read from CRB
	beq .3
	jsr cmpfail
.3	ldaa #$1
	jsr dly1
	incb
	bne .1		; continue loop for all 255 values

	ldaa #$0a
	jsr txbyte
	jsr txstrsp
conf21d	dc "A -> B ",$0
	ldaa RAMLO
	beq .5
	jsr cmpfailmsg
	bra .0
.5	ldx #ok
	jsr txstring
	jsr txcrlf

; now test port B as output
.0	jsr conf21Bout	; configure port B as output
	ldx #conf21b	; send message about testing port
	jsr txstring
	ldab #$00
	stab RAMLO
.2	stab PRB		; write to PRB
	tba			; copy reg B to A
	asla
	asla
	anda #$08
	oraa #$34
	staa CRB
	tba
	jsr txhexbyte	; display tested value
	ldaa #" "		; followed by a space
	jsr txbyte
	ldaa CRA		; read from CRA
	anda #$c0		; keep only interppupt status bits
	beq .8
	ldaa #$aa
.8	jsr txhexbyte	; display interrupt bits value
	ldaa #" "		; followed by a space
	jsr txbyte
	ldaa PRA		; read from PRA
	jsr txhexbyte	; display tested value
;	jsr txcrlf
	ldaa #$0d
	jsr txbyte
	cmpb PRA		; compare reg B with value read from CRA
	beq .4
	jsr cmpfail
.4	ldaa #$1
	jsr dly1
	incb
	bne .2		; continue loop for all 255 values

	ldaa #$0a
	jsr txbyte
	jsr txstrsp
conf21e	dc "B -> A ",$0
	ldaa RAMLO
	bne cmpfailmsg
	ldx #ok
	jsr txstring
	jsr txcrlf
	bra cmpxit

cmpfail
	ldaa #$0a
	stab RAMLO
	jsr txbyte
	rts

cmpfailmsg
	ldx #fail
	jsr txstring
	jsr txcrlf
cmpxit
	jsr conf21ABin
	rts
		
; configure HD6321 ports A and B as inputs
conf21ABin
	jsr txstrsp
conf21f	dc "Configuring HD6321 PRA and PRB input",$0d,$0a,$0
	ldaa #$30		; select DDRB,CA2, CB2 outputs
	staa CRB		; write that to CRB
	staa CRA		; write that to CRA
	ldaa #$00		; all PA PB pins inputs
	staa DDRA		; DDRA
	staa DDRB		; DDRB
	ldaa #$34		; select Peripheral Reg access, Cx2 outputs
	staa CRA		; write that to CRA
	staa CRB		; and to CRB
	rts

; configure HD6321 port A as output and port B as input
conf21Aout
	jsr txstrsp
conf21a	dc "Configuring HD6321 PRA output PRB input",$0d,$0a,$0
	ldaa #$30		; select DDRB,CA2, CB2 outputs
	staa CRB		; write that to CRB
	staa CRA		; write that to CRA
	ldaa #$00		; all PB pins inputs
	staa DDRB		; DDRB
	ldaa #$ff		; all PA pins outputs
	staa DDRA		; DDRA
	ldaa #$34		; select Peripheral Reg access, Cx2 outputs
	staa CRA		; write that to CRA
	staa CRB		; and to CRB
	rts

; configure HD6321 port B as output and port A as input
conf21Bout
	jsr txstrsp
conf21c	dc "Configuring HD6321 PRA input PRB output",$0d,$0a,$0
	ldaa #$30		; select DDRA,CA2 output
	staa CRA		; write that to CRA
	staa CRB		; write that to CRB
	ldaa #$00		; all pins inputs
	staa DDRA		; DDRA
	ldaa #$ff		; all pins outputs
	staa DDRB		; DDRB
	ldaa #$34		; select Peripheral Reg access, Cx2 outputs
	staa CRA		; write that to CRA
	staa CRB		; and to CRB
	rts

conf21b	dc "Testing ports",$0d,$0a," W     R",$0d,$0a,$0

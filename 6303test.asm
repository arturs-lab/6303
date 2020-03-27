
	PROCESSOR HD6303

p1ddr	equ 0
p2ddr	equ 1
p1	equ 2
p2	equ 3
p3ddr	equ 4
p4ddr	equ 5
p3	equ 6
p4	equ 7
tcsr	equ 8
ch	equ 9
cl	equ $a
ocrh	equ $b
ocrl	equ $c
icrh	equ $d
icrl	equ $e
p3csr	equ $f
trmcr	equ $10
trcsr	equ $11
rxr	equ $12
txr	equ $13
rcr	equ $14

; tcsr bits
OLVL	equ $01	; output level
IEDG	equ $02	; input edge
ETOI	equ $04	; enable timer overflow int
EOCI	equ $08	; enable output compare int
EICI	equ $10	; enable input capture int
TOF	equ $20	; timer overflow flag
OCF	equ $40	; output compare flag
ICF	equ $80	; input capture flag

; trcsr bits - Transmit Receive Control Status Register
WU	equ $01	; Wake Up
TE	equ $02	; Transmit Enable
TIE	equ $04	; Transmit Interrupt Enable
RE	equ $08	; Receive Enable
RIE	equ $10	; Receive Interupt Enable
TDRE	equ $20	; Transmit Data Register Empty
ORFE	equ $40	; Over Run Framing Error
RDRF	equ $80	; Receive Data Register Full

inram	equ $80	; internal ram
exram	equ $4000	; external RAM start
extop	equ $c000	; external RAM end
hd6321	equ $10f8 ; HD6321 PIA
PRA	equ hd6321
PRB	equ hd6321 + 2
DDRA	equ hd6321
DDRB	equ hd6321 + 2
CRA	equ hd6321 + 1
CRB	equ hd6321 + 3

CPLDl	equ $10fc	; base address of CPLDl
CPLDh	equ $10fd	; base address of CPLDh
LED	equ $10fe	; LED address
OUTREG	equ	$10ff ; special output register
tp1	equ $55	; test pattern 1
tp2	equ $aa	; test pattern 2

MIC	equ 8		; mic bit of tape output

ratmp	equ $80	; temp storage for reg A
rbtmp	equ $81	; temp storage for reg B
rxtmp	equ $82	; temp storage for reg X
irqcnt	equ $84
ocrcnt	equ $86
ocrv	equ $88	; calculate to be half of irqdel and use for OCR
intcntl	equ $8a	; INT counter low
intcntm	equ $8b	; INT counter medium
intcnth	equ $8c	; INT counter high

irqdel equ $7040	; count for timer to achieve 50Hz IRQ
delayl equ $10	; length of delay before restart

	org exram
; when loading header from tape, first 8 bytes are file name,
; followed by address to be saved to
; followed by length of data
; https://faqwiki.zxnet.co.uk/wiki/Spectrum_tape_interface#Header_block
tsavebuf	equ exram			; buffer for header, file type
tsavename	equ tsavebuf + 1		; file name
tsavelen	equ tsavename + 10	; count of bytes to be saved to tape
tsavep1	equ tsavelen + 2		; param 1
tsavep2	equ tsavep1 + 2		; param 2
tsavetim	equ tsavep2 + 2		; leader timers


	seg
	org $e000

main subroutine
	
start	
;	ldaa #$ff
;	staa p1ddr
;	ldaa #$00
;x1	staa p1
;	staa CPLDh
;	staa LED
;	ldx #$0000
;	stx exram
;x2	inc exram
;	bne x2
;	inc exram+1
;	bne x2
;	inca
;	bra x1

	ldaa #$00	; first phase
	staa LED
	ldaa #$04	; set serial port as internal clock, 115200 baud
;	ldaa #$05	; set serial port as internal clock, 57600 baud
	staa trmcr
	ldaa #TE + #RE	; enable TX and RX
	staa trcsr
	ldaa rxr	; clear RX buffer

	ldaa #$ff	; configure port 1 as all outputs
	staa p1ddr
	clra
	staa p1

	ldaa #$01	; 2nd phase
	staa LED
l0	ldx #intram
tx1	ldaa trcsr	; wait for transmit register to be empty
	asla
	asla
	asla
	bcc tx1
	ldaa 0,x
	beq txt2
	staa txr	; send a string out via serial
	inx
	bra tx1
txt2

	ldab #tp1	; RAM test pattern
l4	ldx #$80	; point to built in RAM
	tba
	anda #$3f
;	staa CPLDh	; store pattern in CPLD output register
l2	stab 0,x	; store pattern in RAM address pointed to by X
	inx
	cpx #$100	; done last byte?
	bne l2		; no, keep going
	ldx #$80	; yes, point to beginning of RAM again
l3	cmpb 0,x	; check if RAM matches reg. B
	bne tfail1	; jump if no match
	inx		; otherwise keep checking
	cpx #$100	; until end of RAM
	bne l3
	cmpb #tp2	; was the pattern tp2?
	beq l5	; yes, done
	ldab #tp2	; no, use tp2 as pattern on 2nd pass
	bra l4

tfail1  jmp tfail
; now that internal RAM is verified we can set up stack pointer and use subroutines
l5	ldaa #$02	; 3rd phase
	staa LED
	lds #$00FF	; Locate stack at top of internal RAM
	ldx #ok	; point to text "OK\r\n"
	jsr txstring	; call subroutine to send it out serial port
; we can also set up interrupts!
	clra
	clrb
	std irqcnt		; initialize interrupt counter
	std ocrcnt
	std intcntl
	staa intcnth
	ldd #irqdel		; initialize timer counter register
	std ch
	lsrd
	lsrd
	std ocrh		; set output compare reg to trigger at quarter count
	std ocrv

	ldaa #$03		; set P2.0, P2.1 as output
	staa p2ddr
	;oimd #$03,p2ddr
	ldaa #EOCI + ETOI	;enable Output Compare interrupt and Timer Overflow interrupt
	;ldaa #ETOI		; enable Timer Overflow interrupt
	oraa tcsr
;	anda #~EOCI
	staa tcsr
	;oim tcsr,#EOCI
	cli			; enable interrupts

; interrupts. timer stores time incremented at 50Hz (20ms)
.11	ldaa #$04	; 4th phase
	staa LED
	jsr showcounters

ldel	ldx #delays
	jsr txstring
	ldaa #delayl	; wait a bit and then repeat loop
.9	ldx #$ffff
.7	dex
	bne .7
	deca
	beq .10
	tab
	ldaa #"."
	jsr txbyte
	tba
	bra .9
.10	jsr txcrlf

;	swi
;	bra .11

.12	;ldaa #1
	;jsr delay1
	jsr tsaveh
	bra .12

	jmp ramtest		; jump to testing RAM

; internal RAM failed. Send message via serial and restart after a delay

tfail	txs		; first store X in SP which we can't use with bad RAM anyway
	ldaa #$0f	; fail
	staa LED
	ldx #fail
	ldaa TDRE	; wait for transmit register to be empty
tx3	bita trcsr
	beq tx3
	ldaa 0,x
	beq tx3a
	staa txr	; send a string out via serial
	inx
	bra tx3
	ldx #ataddr
	ldaa TDRE	; wait for transmit register to be empty
tx3a	bita trcsr
	beq tx3a
	ldaa 0,x
	beq txt4
	staa txr	; send a string out via serial
	inx
	bra tx3a
txt4	tsx		; get X from SP
	xgdx		; then put X in AB
	lsra		; get high nibble
	lsra
	lsra
	lsra
	ldab #"0"	; prepare reg B
	cmpa #$09	; is A > 9?
	ble txb1	; no, skip next
	ldab #("A" - 10)	; if A == 10 then A + ASCII("A") - 10 = ASCII("A")
txb1	aba
	tab
	ldaa TDRE	; wait for transmit register to be empty
tx7	bita trcsr
	beq tx7
	tba
	staa txr	; send a string out via serial

	tsx
	xgdx
	anda #$0f	; work on low nibble
	ldab #"0"	; prepare reg B
	cmpa #$09	; is A > 9?
	ble txb2	; mo, skip next
	ldab #("A" - 10)	; if A == 10 then A + ASCII("A") - 10 = ASCII("A")
txb2	aba
	tab
	ldaa TDRE	; wait for transmit register to be empty
tx8	bita trcsr
	beq tx8
	tba
	staa txr	; send a string out via serial

	tsx
	xgdx		; then put X in AB
	tba		; get lower byte
	lsra		; get high nibble
	lsra
	lsra
	lsra
	ldab #"0"	; prepare reg B
	cmpa #$09	; is A > 9?
	ble txb3	; mo, skip next
	ldab #("A" - 10)	; if A == 10 then A + ASCII("A") - 10 = ASCII("A")
txb3	aba
	tab
	ldaa TDRE	; wait for transmit register to be empty
tx9	bita trcsr
	beq tx9
	tba
	staa txr	; send a string out via serial

	tsx
	xgdx
	tba
	anda #$0f
	ldab #"0"	; prepare reg B
	cmpa #$09	; is A > 9?
	ble txb4	; mo, skip next
	ldab #("A" - 10)	; if A == 10 then A + ASCII("A") - 10 = ASCII("A")
txb4	aba
	tab
	ldaa TDRE	; wait for transmit register to be empty
tx10	bita trcsr
	beq tx10
	tba
	staa txr	; send a string out via serial

	ldx #crlf
	ldaa TDRE	; wait for transmit register to be empty
tx11	bita trcsr
	beq tx11
	ldaa 0,x
	beq txt4
	staa txr	; send a string out via serial
	inx
	bra tx11

	jmp delrst	; jump to delay and restart routine

ramtest subroutine
ramtest
	ldaa #$08	; 5th phase
	staa LED
	jsr showcounters

	ldx #extram
	jsr txstring

	ldaa #tp1		; load test pattern
.1	psha
	jsr txhexbyte	; display it
	ldaa #" "		; then display space
	jsr txbyte

	ldab  #$07		; pulse outreg to measure duration of RAM test
	stab OUTREG

	ldx #exram		; point index at beginning of RAM
	pula
.2	staa 0,x		; store it in memory pointed to by X
	inx			; increment pointer
	cpx #extop
	bne .2		; not yet, clear stack and continue loop

	ldab  #$03		; pulse outreg to measure duration of RAM test
	stab OUTREG

	ldx #exram		; point index at beginning of RAM
.3	cmpa 0,x		; compate test pattern with RAM
	bne ramfail		; differs, jump to fail routine
	inx			; is good, increment pointer
	cpx #extop
	bne .3		; if so, print success

	ldab  #$01		; pulse outreg to measure duration of RAM test
	stab OUTREG

.4	cmpa #tp2		; was second pattern tested?
	beq .6		; yes, print success
	ldaa #tp2		; no, test second pattern
	bra .1 

.6	ldab  #$00		; pulse outreg to measure duration of RAM test
	stab OUTREG
	stx rxtmp
	jsr txcrlf
	ldx #rxtmp
	jsr txhexword
	ldaa #" "
	jsr txbyte
	ldx #ok
	jsr txstring
;	jsr txcrlf

;	jsr io		; call subroutine to generate some pulses on port P1

	jsr showcounters
	jsr txcrlf

;	jsr test6321

	jmp ldel
;	jmp l0		; rinse and repeat
	jmp ramtest

; FAILED RAM TESTS
ramfail
	ldaa #$0a	; first phase
	staa LED
	stx rxtmp	; store X in internal RAM
	ldx #fail
	jsr txstring
	ldx #ataddr
	jsr txstring

	ldaa rxtmp	; then put Xh in A
	jsr txhexbyte

	ldaa rxtmp+1	; then put Xl in A
	jsr txhexbyte

	jsr txcrlf

	ldab #$80		; init counter of 4 * 16 bytes
	ldaa rxtmp+1	; now go back to last address in format $xxx0
	anda #$f0
	staa rxtmp+1
.11	ldx #rxtmp	; point to that address stored at rx
	jsr txhexword	; display it
	ldaa #":"		; then display space
	jsr txbyte
	ldaa #" "		; then display space
	jsr txbyte
	ldx rxtmp		; load that address to X
.8	ldaa 0,x		; get byte from RAM
	jsr txhexbyte	; display it
	ldaa #" "		; followed by a space
	jsr txbyte
	inx			; point ot next
	decb			; dec counter
	beq .10		; done?
	tba
	anda #$0f
	bne .8
	stx rxtmp
	jsr txcrlf
	bra .11
.10	jsr txcrlf
	
delrst
	ldaa #delayl	; RAM test failed, wait a bit and then restart
.9	ldx #$ffff
.7	dex
	bne .7
	deca
	bne .9

	jsr showcounters

	ldx #restart		; restart
	jsr txstring

	jmp ramtest			; repeat ramtest
	ldx $fffe			; or jump to reset vector
	jmp 0,x

test6321 subroutine
test6321
	jsr conf21Aout	; configure port A as output
	ldx #conf21b	; send message about testing port
	jsr txstring
	ldab #$00
	stab exram
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
	jsr cmpAfail
.3	ldaa #$2
	jsr delay1
	incb
	bne .1		; continue loop for all 255 values

	ldaa #$0a
	jsr txbyte
	ldx #conf21d	; send message about success
	jsr txstring
	ldaa exram
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
	stab exram
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
	jsr cmpBfail
.4	ldaa #$2
	jsr delay1
	incb
	bne .2		; continue loop for all 255 values

	ldaa #$0a
	jsr txbyte
	ldx #conf21e	; send message about success
	jsr txstring
	ldaa exram
	bne cmpfailmsg
	ldx #ok
	jsr txstring
	jsr txcrlf
	rts

cmpAfail
	ldaa #$0a
	stab exram
	jsr txbyte
	rts

cmpBfail
	ldaa #$0a
	stab exram
	jsr txbyte
	rts

cmpfailmsg
	ldx #fail
	jsr txstring
	jsr txcrlf
	rts
		
; configure HD6321 port A as output and port B as input
conf21Aout
	ldx #conf21a
	jsr txstring
	ldaa #$30		; select DDRB,CB2 output
	staa CRB		; write that to CRB
	staa CRA		; write that to CRA
	ldaa #$00		; all pins inputs
	staa DDRB		; DDRB
	ldaa #$ff		; all pins outputs
	staa DDRA		; DDRA
	ldaa #$34		; select Peripheral Reg access, Cx2 outputs
	staa CRA		; write that to CRA
	staa CRB		; and to CRB
	rts

; configure HD6321 port B as output and port A as input
conf21Bout
	ldx #conf21c
	jsr txstring
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

tsavesub subroutine
; on entry X points to data to be saved
; and data count is stored in tsavecnt
; https://retrocomputing.stackexchange.com/questions/12783/what-should-be-the-waveform-for-zx-spectrum-tapes
; A 'pulse' here is either a mark or a space, so 2 pulses makes a complete square wave cycle.
; 
; Pilot tone: before each block is a sequence of 8063 (header) or 3223 (data) pulses, each of length 2168 T-states.
;
;Sync pulses: the pilot tone is followed by two sync pulses of 667 and 735 T-states resp.
;
;A '0' bit is encoded as 2 pulses of 855 T-states each.
;
;A '1' bit is encoded as 2 pulses of 1710 T-states each (ie. twice the length of a '0')
;
;The initial polarity of the signal does not matter - everything in the ROM loader is edge-triggered rather than level-triggered.

tsaveh
	pshx
;	ldx #$3f42	; 5 seconds
	ldx #8063	; 5 seconds
	bra tsave

tsaved
	pshx
;	ldx #$194e	; 2 seconds
	ldx #3233	; 2 seconds

tsave	sei		; disable interrupts
	ldaa #MIC	; set mic pin
	tab		; also set B reg
.1	decb
	nop
	nop
	bne .1
	staa p1	; LED
	eora #MIC	; invert mic bit
	ldab #$bf	; timing constant for correct frequency 820Hz. Speccy 2168 T-states
	dex
	bne .1	; header length counter

	ldab #$56	; short pulse - 667T
.2	decb
	bne .2
	staa p1	; LED
	eora #MIC	; invert mic bit

	ldab #$60	; long pulse - 735T
.3	decb
	bne .3
	staa p1	; LED
	eora #MIC	; invert mic bit

	pulx		; get address of data

.9	ldaa 0,X	; data to be sent
	ldab #8	; bit counter

.8	pshb
	ldab #$70	; 0 - 855T
	lsra
	bcc .4
	ldab #$e0	; 1 - 1710T
.4	psha
.5	decb
	bne .5
	ldaa #MIC
	eora p1	; LED	; invert mic bit
	pula
	ldab #$70	; 0 - 855T
	lsra
	bcc .6
	ldab #$e0	; 1 - 1710T
.6	psha
.7	decb
	bne .7
	ldaa #MIC
	eora p1	; LED	; invert mic bit
	pula
	pulb
	decb
	bne .8

	inx

	cli
	rts

tsavein
	pshx
	ldx #$fffe
	jsr tsaveh
	pulx
	rts

showcounters subroutine
showcounters
	ldx #timer		; display value of timer
	jsr txstring
	ldx irqcnt
	stx rxtmp
	ldaa rxtmp
	staa CPLDh
	ldx #rxtmp
	jsr txhexword
	jsr txcrlf
	ldx #cpldstr
	jsr txstring
	ldaa CPLDh
	jsr txhexbyte
	jsr txcrlf
;	ldx #counter		; display value of timer
;	jsr txstring
;	ldx ocrcnt
;	stx rxtmp
;	ldx #rxtmp
;	jsr txhexword
;	jsr txcrlf
	rts

delay subroutine
delay	psha
	ldaa #delayl	; standard delay length defined by delay1
	bra .9
delay1	psha
.9	ldx #$ffff		; or of you call delay1, give delay length in reg A
.7	dex
	bne .7
	deca
	bne .9
	pula
	rts

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
	pshb
	adda #"0"	; make is ASCII code for 0-9
	cmpa #"9"	; is A > 9?
	bls .1	; no, skip next
	adda #("a" - ":")	; if A > "9" then A + ASCII("a") - ASCII(":") = ASCII("a")
.1	tab
	ldaa TDRE	; wait for transmit register to be empty
.2	bita trcsr
	beq .2
	tba
	staa txr	; send a string out via serial
	pulb
	rts

txbyte subroutine	; transmit byte given in A
txbyte
	psha
	ldaa TDRE	; wait for transmit register to be empty
.1	bita trcsr
	beq .1
	pula
	staa txr	; send a string out via serial
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
.1	bita trcsr
	beq .1
	ldaa 0,x
	beq .2
	staa txr	; send a string out via serial
	inx
	bra .0
.2	pula
	rts

io	subroutine
io	clra	
.1	tab
	ldaa p1
	anda #$c0	; preserve current state of P1 bits 6 & 7
	aba
	staa p1	; update P1
	inca
	anda #$3f	; remove P1 bits 6 & 7 used in interrupts
	bne .1	; continue till reach 0 again
	rts

conf21a	dc "Configuring HD6321 PRA output PRB input",$0d,$0a,$0
conf21b	dc "Testing ports",$0d,$0a," W     R",$0d,$0a,$0
conf21c	dc "Configuring HD6321 PRA input PRB output",$0d,$0a,$0
conf21d	dc "A -> B ",$0
conf21e	dc "B -> A ",$0

cpldstr	dc "CPLD: ",$0
timer		dc "Timer: ",$0
counter	dc "Counter: ",$0
delays	dc "delay ",$0d,$0a,$0
intram	dc $0d,$0a,"Internal RAM ",$0
extram	dc "External RAM ",$0
ok		dc "OK",$0d,$0a,$0
fail		dc "FAIL",$0d,$0a,$0
ataddr	dc " at addr ",$0
restart 	dc "RESTART",$0d,$0a,$0d,$0a,$0
crlf		dc $0d,$0a,$0

	org $ff80

tofs subroutine
tofs	sei
	psha
	pshb

	ldd tcsr		; clear interrupt bit
	ldd #irqdel		; initialize timer counter register
	std ch
	inc irqcnt+1		; load interrupt counter value
	bne .3
	inc irqcnt
.3
;	ldaa #OLVL		; load tcst and inver OLVL bit
;	eora tcsr
;	staa tcsr
	ldaa #$40	; pulse P1 bit 6 on every interrupt
	eora p1
	staa p1
	pulb
	pula
	cli
	rti

ocfs subroutine
ocfs	sei
	psha
	pshb

	ldaa tcsr
	ldd ocrv
	std ocrh		; clear interrupt bit
	inc ocrcnt+1		; load interrupt counter value
	bne .3
	inc ocrcnt
.3
;	ldaa #OLVL		; load tcst and inver OLVL bit
;	eora tcsr
;	staa tcsr
	ldaa #$20	; pulse P1 bit 5 on every interrupt
	eora p1
	staa p1
	pulb
	pula
	cli
	rti

int subroutine
int	sei
	psha
	tpa
	psha
	ldaa #$80	; pulse P1 bit 7 on every interrupt
	eora p1
	staa p1
;	ldaa #"@"		; then display space
;	jsr txbyte
	inc intcntl
	bne .1
	inc intcntm
	bne .1
	inc intcnth
.1	ldaa intcnth
	staa CPLDl
	pula
	tap
	pula
	cli
	rti

	nop
	nop
	nop

	org $ffee
trapv	.word int	; TRAP

	org $fff0
sciv	.word int	; SCI (RDRF + ORFE + TDRE)
tofv	.word tofs	; TOF
ocfv	.word ocfs	; OCF
icfv	.word int	; ICF
irqv	.word int	; IRQ or IS3
swiv	.word int	; SWI
nmiv	dc.w int	; NMI
rstv	dc.w start	; RESET


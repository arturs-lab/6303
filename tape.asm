
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
	ldx #8062	; 5 seconds header
;	ldx #8	; 8 pulses for testing
	bra tsave

tsaved
	pshx
	ldx #3222	; 2 seconds data

tsave	sei		; disable interrupts
	ldaa #MIC	; set mic pin
	bra .0

; delay loop for proper pulse length
.1	decb
	nop
	nop
	bne .1
.0	staa SNDSEL	; tape output
	eora #MIC	; invert mic bit
	ldab #$bb	; timing constant for correct frequency 807Hz. Speccy 3.5MHz / 2168 T-states / 2 pulses per cycle. 619us each pulse
	dex		; count of lead in pulses
	bne .1

	ldab #$54	; short pulse - 3.5 MHz / 667T = 190us
.2	decb
	bne .2
	staa SNDSEL	; tape output
	eora #MIC	; invert mic bit

	ldab #$5f	; long pulse - 3.5 MHz / 735T = 210us
.3	decb
	bne .3
	staa SNDSEL	; tape output
	eora #MIC	; invert mic bit

.10	pulx		; get address of data

.9	ldaa 0,X	; data to be sent
	ldab #8	; bit counter

.8	pshb		; save bit counter
	ldab #$68	; 0 - 3.5 MHz / 855T = 244us
	lsra
	bcc .4
	ldab #$d8	; 1 - 3.5 MHz / 1710T = 489us
.4	psha		; save transmitted byte
.5	decb		; count down pulse length
	bne .5
	ldaa SNDSEL	; get current speaker output value
	eora #MIC	; invert mic bit
	staa SNDSEL	; tape output

	pula		; get data byte
	pulb		; get bit counter
	decb
	bne .8	; all bits sent?

	inx		; point to next byte

	pshx
	ldx tsavecnt	; decrement data count
	dex
	beq .11		; all data sent?
	stx tsavecnt
	pulx

	ldaa 0,X	; data to be sent
	ldab #8	; bit counter

	pshb

; timing of first bit of the following byte will be affected by above code
	ldab #$60	; 0 - 3.5 MHz / 855T = 244us
	lsra
	bcc .12
	ldab #$d0	; 1 - 3.5 MHz / 1710T = 489us
.12	bra .4

.11	pulx
	ldaa SNDSEL
	anda ~#MIC	; make sure speaker is off
	staa SNDSEL	; tape output
	cli
	rts

tsavein
	pshx
	ldx #$0008		; send 8 bytes
	stx tsavecnt
	ldx #tap00		; send it
	jsr tsaveh
	pulx
	rts

tap00	dc.b $00,$00
tapff	dc.b $ff,$ff
tap55	dc.b $55,$55
tapaa	dc.b $aa,$aa


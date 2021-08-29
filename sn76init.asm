SN76SEL	equ $10	; bit of SPECIAL reg that enables SN76 chip

sn76chab	equ RAMLO + 0	; song begin ch 1
sn76cha	equ RAMLO + 2	; current note pointer ch 1
sn76chacn	equ RAMLO + 4	; current note time ch 1
sn76chaad	equ RAMLO + 5	; current note decay tempo
sn76ch1f	equ RAMLO + 6	; sn76 registers - chan 1 frequency
sn76ch1a	equ RAMLO + 8	; sn76 registers - chan 1 attenuation

sn76chbb	equ RAMLO + 9	; song begin ch 2
sn76chb	equ RAMLO + 11	; current note pointer ch 2
sn76chbcn	equ RAMLO + 13	; current note time ch 2
sn76chbad	equ RAMLO + 14	; current note decay tempo
sn76ch2f	equ RAMLO + 15	; sn76 registers - chan 2 frequency
sn76ch2a	equ RAMLO + 17	; sn76 registers - chan 2 attenuation

sn76chcb	equ RAMLO + 18	; song begin ch 3
sn76chc	equ RAMLO + 20	; current note pointer ch 3
sn76chccn	equ RAMLO + 22	; current note time ch 3
sn76chcad	equ RAMLO + 23	; current note decay tempo
sn76ch3f	equ RAMLO + 24	; sn76 registers - chan 3 frequency
sn76ch3a	equ RAMLO + 26	; sn76 registers - chan 3 attenuation

sn76chns	equ RAMLO + 27	; sn76 registers - noise source
sn76chna	equ RAMLO + 28	; sn76 registers - noise attenuation

sn76curchan	equ RAMLO + 29	; stores pointer to current channel
sn76tmpo	equ RAMLO + 31	; song tempo
sn76temp	equ RAMLO + 33	; temporary reg for testing


sn76init subroutine
sn76init	jsr sn76off

	ldaa #$0f
	staa sn76ch1a	; channel 1 attenuation
	staa sn76ch2a	; channel 2 attenuation
	staa sn76ch3a	; channel 0 attenuation
	staa sn76chna	; noise attenuation

	ldaa #$07
	staa sn76chns	; noise source

	ldx #$0083	; A4
	stx sn76ch1f
	ldx #$006e	; C5
	stx sn76ch2f
	ldx #$0057	; E5
	stx sn76ch3f
	ldx #$00ff
	stx sn76tmpo	; playback speed

	ldx #$0004
	stx sn76chna

	rts

sn76off	ldaa #$ff
	jsr setsn
	ldaa #$df
	jsr setsn
	ldaa #$bf
	jsr setsn
	ldaa #$9f
	jsr setsn

	rts


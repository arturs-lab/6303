; set SN registers to values pointed to by X
setsnregs subroutine
	ldx #sn76ch1f	; point to first channel
setsnregx	ldab #$80
.1	jsr setsnf		; set frequency register
	addb #$10		; address next register
	ldaa 2,X		; get third byte
	aba
	jsr setsn		; set attenuation register
	tba			; save B for later
	ldab #sn76chbb - sn76chab	; point to next channel
	abx
	tab			; restore B
	addb #$10		; point to next sn76 register
	cmpb #$e0		; is it noise source?
	bne .1		; no, set next channel
	ldaa sn76chns	; noise control
	aba
	jsr setsn
	addb #$10
	ldaa sn76chna	; noise attenuation
	aba

	IF SN76DBG < 3
	bra setsn
	ENDIF
	IF SN76DBG > 2
	jsr setsn
	ldaa #$0d
	jsr txbyte
	ldaa #$0a
	jsr txbyte
	rts
	ENDIF

; set two byte frequency pointed to by X for channel given in B
setsnf
	ldaa 1,X	; put low byte in A while setting high nibble to required channel
	anda #$0f
	aba

	jsr setsn

	pshb		; save B for later
	ldd 0,X
	lsrd
	lsrd
	lsrd
	lsrd
	tba		; tone 1 pitch byte 2
	pulb
	anda #$3f
;	jsr setsn
;	rts

; set one byte register given in A
setsn	staa CPLDl

	IF SN76DBG > 2
	jsr txhexbyte
	ldaa #" "
	jsr txbyte
	ENDIF

	ldaa SPREG
	oraa #SN76SEL
	staa SPREG
	ldaa #$10		; delay between control pin changes
.16	deca
	bne .16
	ldaa SPREG
	anda ~#SN76SEL
	staa SPREG
	rts

sn76play subroutine
	jsr sn76init	; initialize regs

	IF SN76DBG > 0
	ldaa #$0d
	jsr txbyte
	ldaa #$0a
	jsr txbyte
	ENDIF

	ldaa #$0f
	staa sn76chna	; noise attenuation
	ldx song
	stx sn76tmpo	; playback speed
	ldx #song+7		; beginning of song channel A
	stx sn76chab
	stx sn76cha
	ldx song+5		; beginning of song channel B
	stx sn76chbb
	stx sn76chb
	ldx song+3		; beginning of song channel C
	stx sn76chcb
	stx sn76chc
	clra
	staa sn76chacn
	staa sn76chbcn
	staa sn76chccn

; process all notes 25us
	IF SN76DBG > 0
	staa CPLDh
.1	ldaa #$03
	eora CPLDh
	staa CPLDh
	ldaa #"1"
	ELSE
.1
	ENDIF
	ldx #sn76chab		; 60us
	jsr sn76procnote

	IF SN76DBG > 0
	ldaa #$06
	eora CPLDh
	staa CPLDh
	ldaa #"2"
	ENDIF

	ldx #sn76chbb		; 80us
	jsr sn76procnote

	IF SN76DBG > 0
	ldaa #$0c
	eora CPLDh
	staa CPLDh
	ldaa #"3"
	ENDIF

	ldx #sn76chcb		; 60us
	jsr sn76procnote

	IF SN76DBG > 0
	ldaa #$18
	eora CPLDh
	staa CPLDh
	ENDIF

	jsr setsnregs	; play note 900us

	IF SN76DBG > 0
	ldaa #$30
	eora CPLDh
	staa CPLDh
	ENDIF

	ldx sn76tmpo	; delay one song tick at $1800 tempo = about 13.4ms
.5	dex
	bne .5

	IF SN76DBG > 0
	ldaa #$20
	eora CPLDh
	staa CPLDh
	ENDIF

	bra .1	; complete song loop. at $1800 tempo = about 14.32ms


; on entry X points to sn76ch{a,b,c}b - beginning of song for channel {a,b,c} is stored there
sn76procnote subroutine
	stx sn76curchan			; store pointer to current channel
	ldab sn76chacn - sn76chab,x	; load note duration counter
	beq .1				; still playing current note?
	decb					; yes, decrement note time
	stab sn76chacn - sn76chab,x	; store note duration counter
	andb sn76chaad - sn76chab,x	; and with decay tempo
	bne .8
	ldab sn76ch1a - sn76chab,x	; load note attenuation
	cmpb #$0f
	beq .8
	incb
	stab  sn76ch1a - sn76chab,x	; update note attenuation
.8	ldab #14	; equalize duration of new note and same note tick
.7	decb
	bne .7
	rts

.1	ldx sn76cha - sn76chab,x	; load pointer to current note

	IF SN76DBG > 1
	pshx
	jsr txdbg1
	ENDIF

	ldaa 0,x				; load note
	staa sn76temp

	IF SN76DBG > 1
	ldd 1,x				; get note duration and attenuation
	std sn76temp + 1
	ldx #sn76temp
	jsr txhexword
	ldaa sn76temp+2
	jsr txhexbyte
	ldaa #$0d
	jsr txbyte
	ldaa #$0a
	jsr txbyte
	pulx					; load pointer to current note
	ENDIF

	ldd 1,x				; get note duration and attenuation
	ldx sn76curchan			; load pointer to beginnign of cur channel
	deca					; adjust duration
	staa sn76chacn - sn76chab,x	; store note duration counter
	tba					; get attenuation
	anda #$0f
	staa sn76ch1a - sn76chab,x	; store note attenuation

	andb #$f0				; get decay tempo
	bne dtrol
	ldab song + 2			; load default attenuation
	bra dtsv
dtrol	lsrb
	lsrb
	lsrb
	lsrb
dtsv	stab sn76chaad - sn76chab,x	; store note attenuation

	ldd sn76cha - sn76chab,x	; load pointer to current note
	xgdx
	inx		; point to next note
	inx
	inx
	xgdx
	std sn76cha - sn76chab,x	; store pointer to next note

	ldab sn76temp
	tba					; just to update flags
	beq pause				; pause note
	cmpb #74
	bcs procnote

	IF SN76DBG > 1
	jsr txhexbyte
	ldaa #$0d
	jsr txbyte
	ldaa #$0a
	jsr txbyte
	ENDIF

	cmpb #253				; noise?
	bcs noise				; yes
	bne loop
	ldaa #$0f				; turn off noise
	staa sn76chna - sn76chcb,x	; store it in noise attenuation
	rts

loop	cmpb #254				; loop?
	bne endsong
;	ldx sn76curchan			; load pointer to beginnign of cur channel
	ldd 0,x				; get address of beginning of notes for current channel
	std sn76cha - sn76chab,x	; point to first note
	bra .1				; start over, read the note

procnote	ldx #notes	; point to note table
	decb
	aslb
	abx		; add note offset
	ldd 0,x				; get note value
	ldx sn76curchan			; get current channel pointer
	std sn76ch1f-sn76chab,x		; store note value in appropriate channel's memory
	bra .3

pause	ldaa #4	; equalize duration of note and pause processing
.2	deca
	bne .2
	ldaa #$0f
	staa sn76ch1a - sn76chab,x	; store note attenuation
.3	rts

noise	ldaa sn76ch1a - sn76chab,x	; get note attenuation
	staa sn76chna - sn76chcb,x	; store it in noise attenuation
	ldaa #$0f
	staa sn76ch1a - sn76chab,x	; set note attenuation
	clc
	sbcb #73
	clra
	ldx sn76curchan			; get current channel pointer
	std sn76ch1f-sn76chab,x		; store note value in appropriate channel's memory
	bra .3
	
endsong	pulx		; remove extra return address from stack
	IF SN76DBG > 1
	ldaa #0
	staa CPLDh
	ENDIF

	jmp sn76off	; turn off audio and exit

	IF SN76DBG > 1
txdbg1
	jsr txbyte
	ldaa #" "
	jsr txbyte
	stx sn76temp
	ldx #sn76temp
	jsr txhexword
	ldaa #" "
	jsr txbyte
	ldx sn76temp
	rts
	ENDIF

	INCLUDE sn76lookup.asm


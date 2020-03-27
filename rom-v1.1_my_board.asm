	PROCESSOR HD6303

;**************
; MC3 monitor 1.1
; Daniel Tufvesson 2013

;**************
; DEFINITION OF INTERNAL CPU REGISTERS
PIA1DIR	EQU	$00
PIA1DAT	EQU	$02
PIA2DIR	EQU	$01
PIA2DAT	EQU	$03
ACIAMOD EQU     $10     ; RATE AND MODE CONTROL REGISTER
ACIASTA EQU     $11     ; TXD/RXD CONTROL AND STATUS REG.
ACIARXD EQU     $12     ; RECEIVE DATA REGISTER
ACIATXD EQU     $13     ; TRANSMIT DATA REGISTER
RAMCONT EQU     $14     ; RAM CONTROL REGISTER

;**************
; DEFINITION OF VARIABELS
	SEG.U variables
	ORG     $CF00
STACK   ds     1       ; STACK POINTER
        ds     1       ; CONDITIONS CODES
        ds     1       ; B-ACC
        ds     1       ; A-ACC
        ds     1       ; X-HIGH
        ds     1       ; X-LOW
        ds     1       ; P-HIGH
        ds     1       ; P-LOW
SP      ds     2       ; STACK POINTER
CKSM	ds	1	; CHECKSUM
TEMP	ds	1	; TEMP
XHI	ds	1	; X-TEMP HIGH
XLOW	ds	1	; X-TEMP LOW
XTEMP	ds	2	; X-TEMP

;**************
; DEFINITION OF VECTORS
; 3 BYTES JMP+ADDR
	ORG     $CFE5
CONSVEC	ds	3	; CONSOLE STATUS VECTOR
CONOVEC	ds	3	; CONSOLE OUTPUT VECTOR
CONIVEC	ds	3	; CONSOLE INPUT VECTOR
TMOFVEC ds     3       ; TIMER OVER FLOW INTERUPT VECTOR
TMOCVEC	ds	3	; TIMER OUTPUT COMPARE INTERUPT VECTOR
TMICVEC	ds     3       ; TIMER INPUT CAPTURE INTERUPT VECTOR
IRQVEC	ds     3       ; IRQ INTERUPT VECTOR
SWIVEC	ds     3       ; SWI INTERUPT VECTOR
NMIVEC	ds     3	; NMI INTERUPT VECTOR

;**************
; ROM BEGIN HERE
	SEG code
	ORG	$E000
; JUMPTABLE
	JMP	RETURN
	JMP	CONOVEC	; 'OUTCHAR'
	JMP	INCHAR
	JMP	PDATA
	JMP	OUTHR
	JMP	OUTHL
	JMP	OUT2HS
	JMP	OUT4HS
	JMP	INHEX
	JMP	INBYTE
	JMP	BADDR
; STRINGS
CRLFTX	dc	$0D,$0A,$04
PROMPTX	dc	$0D,$0A
	dc	"> "
	dc	$04
HELPTX	dc	$0D,$0A,$0A
	dc	"MC3 monitor 1.1"
	dc	$0D,$0A
	dc	"Daniel Tufvesson 2013"
	dc	$0D,$0A,$0D,$0A
	dc	" G  Go (RTI)"
	dc	$0D,$0A
	dc	" J  Jump to address"
	dc	$0D,$0A
	dc	" L  Load S1 from console"
	dc	$0D,$0A
	dc	" MC Memory change"
	dc	$0D,$0A
	dc	" MD Memory dump"
	dc	$0D,$0A
	dc	" RR Print contents of stack"
	dc	$0D,$0A
	dc	" RS Reset stack pointer"
	dc	$0D,$0A
	dc	" RC Change stack CC"
	dc	$0D,$0A
	dc	" RA Change stack A"
	dc	$0D,$0A
	dc	" RB Change stack B"
	dc	$0D,$0A
	dc	" RX Change stack X"
	dc	$0D,$0A
	dc	" RP Change stack PC"
	dc	$0D,$0A
	dc	" P  Select I/O page"
	dc	$0D,$0A,$04
REGTX	dc     $0D,$0A
	dc     "CC B  A  X    PC   SP     H I N Z V C"
	dc     $0D,$0A,$04
DUMPTX	dc     $0D,$0A
	dc     "ADDR  0  1  2  3  4  5  6  7   8  9  A  B  C  D  E  F"
	dc	$0D,$0A,$04
LDTX	dc     $0D,$0A
	dc     "Load S19 record"
	dc     $0D,$0A,$04
LFAILTX	dc     $0D,$0A,$0A
	dc     "Load fail - Press Y to continue"
	dc     $0D,$0A,$04
LDOKTX	dc     $0D,$0A
	dc     "Load OK"
	dc     $0D,$0A,$04
TRAPTX	dc     $0D,$0A
	dc     "TRAP at address: $"
	dc	$04
PAERRTX	dc     $0D,$0A
	dc     "Parameter error"
	dc     $0D,$0A,$04

;**********
; START FROM RESET
RESET	LDS	#STACK	; INIT STACK POINTER
	STS	SP
	JSR	INITVEC	; INIT VECTORS
	JSR	SCIINIT	; INIT INTERNAL ACIA
	LDAA	#$FF	; SETUP PAGE REGISTER
	STAA	PIA1DIR	
	LDAA	#$87	; MASK EXTERNAL INTERRUPTS AND SELECT PAGE 7
	STAA	PIA1DAT
	CLRA
	STAA	RAMCONT	; DISABLE CPU INTERNAL RAM
PROMPT	LDX	#PROMPTX
	JSR	PDATA
	JSR	INCHAR
	CMPA	#$0D
	BEQ	PROMPT
	ANDA	#$DF	; CONVERT TO UPPER CASE
	CMPA	#'H
	BNE	*+5
	JMP	HELP	; PRINT HELP
	CMPA	#'G
	BNE	*+5
	JMP	GO	; GOTO USER PROGRAM
	CMPA	#'J
	BNE	*+5
	JMP	JUMP	; JUMP TO USER PROGRAM
	CMPA	#'R
	BNE	*+5
	JMP	MENU_R	; REG/STACK CMDS
	CMPA	#'M
	BNE	*+5
	JMP	MENU_M	; MEMORY CMDS
	CMPA	#'L
	BNE	*+5
	JMP	LOAD	; LOAD S1
	CMPA	#'P
	BNE	*+5
	JMP	PAGE	; SELECT I/O PAGE
	LDAA	#'?
	JSR	OUTCHAR
	BRA	PROMPT

MENU_M	JSR	INCHAR
	CMPA	#$0D
	BNE	*+5
	JMP	PROMPT
	ANDA	#$DF	; CONVERT TO UPPER CASE
	CMPA	#'C
	BNE	*+5
	JMP	CHANGE	; MEMORY CHANGE
	CMPA	#'F
	BNE	*+5
	JMP	MFILL	; MEMORY FILL
	CMPA	#'D
	BNE	*+5
	JMP	DUMP	; MEMORY DUMP
	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

MENU_R  JSR	INCHAR
	CMPA	#$0D
	BNE	*+5
	JMP	PROMPT
	ANDA	#$DF
	CMPA	#'R
	BNE	*+5
	JMP	PRTREG	; REGISTER PRINT
	CMPA	#'A
	BNE	*+5
	JMP	REGACH	; REGISTER A CHANGE
	CMPA	#'B
	BNE	*+5
	JMP	REGBCH	; REGISTER B CHANGE
	CMPA	#'X
	BNE	*+5
	JMP	REGXCH	; REGISTER X CHANGE
	CMPA	#'P
	BNE	*+5
	JMP	REGPCH	; REGISTER PC CHANGE
	CMPA	#'C
	BNE	*+5
	JMP	REGCCH	; REGISTER CC CHANGE
	CMPA	#'S
	BNE	*+5
	JMP	REGLDS	; RESET STACK POINTER
	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

;**************
; HELP
HELP	LDX	#HELPTX
	JSR	PDATA
	JMP	PROMPT

;**************
; GO
GO      LDS     SP
	RTI

;**************
; JUMP TO ADDRESS
JUMP    LDAA    #$20
        JSR     OUTCHAR
	JSR     BADDR
        BCC	*+5	; ADDRESS INPUT OK?
        JMP	0,X	; JUMP TO ADDRESS IN X
        JMP	PROMPT

;**************
; RETURN FROM USER PROGRAM
RETURN	STS	SP
	JMP	PROMPT

;**************
; PRINT CONTENTS OF STACK
PRTREG	LDX     #REGTX
	JSR     PDATA
	LDX     SP
	INX
        JSR     OUT2HS  ; CONDITION CODES
        JSR     OUT2HS  ; ACC-B
        JSR     OUT2HS  ; ACC-A
        JSR     OUT4HS  ; X-REG
        JSR     OUT4HS  ; P-COUNTER
	LDX     #SP
        JSR     OUT4HS  ; STACK POINTER

        LDAA    #$20
        JSR     OUTCHAR
        LDX     SP
        INX
        LDAB    ,X
        LDX     #$06
        ASLB
        ASLB
CCLOOP  LDAA    #$20
        JSR     OUTCHAR
        ASLB
        BCS     CCONE
CCZERO  LDAA    #'0
        JSR     OUTCHAR
        JMP     CCEND        
CCONE   LDAA    #'1
        JSR     OUTCHAR

CCEND   DEX
        BNE     CCLOOP
	JMP	PROMPT

;**************
; REGISTER CHANGE ROUTINES (A B X PC)
REGACH  LDAA    #'=
        JSR     OUTCHAR
        LDX     SP
        JSR     INBYTE
        BCC     ENDA
        STAA    3,X
ENDA    JMP     PROMPT
REGBCH  LDAA    #'=
        JSR     OUTCHAR
        LDX     SP
        JSR     INBYTE
        BCC     ENDB
        STAA    2,X
ENDB    JMP     PROMPT
REGXCH  LDAA    #'=
        JSR     OUTCHAR
        JSR     BADDR
        BCC     ENDX
        STX     XHI
        LDX     SP
        LDD     XHI
        STD     4,X
ENDX    JMP     PROMPT
REGPCH  LDAA    #'=
        JSR     OUTCHAR
        JSR     BADDR
        BCC     ENDP
        STX     XHI
        LDX     SP
        LDD     XHI
        STD     6,X
ENDP    JMP     PROMPT
REGCCH  LDAA    #'=
        JSR     OUTCHAR
        LDX     SP
        JSR     INBYTE
        BCC     ENDC
        STAA    1,X
ENDC    JMP     PROMPT
REGLDS	LDS	#STACK
	STS	SP
	JMP	PROMPT

;**************
; CHANGE MEMORY (MC AAAA DD NN)
CHANGE  LDAA    #$20
	JSR     OUTCHAR
	JSR     BADDR   ; BUILD ADDRESS
	BCC     CHANGER
CHA51   LDX     #CRLFTX
	JSR     PDATA   ; C/R L/F
	LDX     #XHI
        JSR     OUT4HS  ; PRINT ADDRESS
	LDX     XHI
        JSR     OUT2HS  ; PRIND DATA (OLD)
	STX     XHI     ; SAVE DATA ADDRESS
	JSR     INBYTE	; INPUT NEW DATA
	BCC     CHANG1
	DEX    
	STAA    ,X      ; CHANGE MEMEORY
	CMPA    ,X
	BEQ     CHA51   ; DID CHANGE
	LDAA    #'?
	JSR     OUTCHAR
	BRA     CHA51
CHANG1  CMPA    #$DD
	BEQ     CHA51
CHANGEE JMP     PROMPT
CHANGER	LDX	#PAERRTX
	JSR	PDATA
	JMP	PROMPT

;**************
; FILL MEMORY (F SADR-EADR DA)
MFILL   LDAA    #$20    ; PRINT SPACE
        JSR     OUTCHAR
        JSR     BADDR   ; BUILD STARTING ADDRESS
        BCC     MFILLE	; CHECK IF CORRECT
        STX     XTEMP   ; SAVE STARTING ADDRESS
        LDAA    #'-     ; PRINT SEPARATOR
        JSR     OUTCHAR
        JSR     BADDR   ; BUILD ENDING ADRESS
        BCC     MFILLE	; CHECK IF CORRECT
        STX     XHI
        CPX     XTEMP   ; CHECK IF CORRECT ADDRESS RANGE
        BLS     MFILLE  ; IF NOT, EXIT ROUTINE
        INX
        STX     XHI
        LDAA    #$20    ; PRINT SPACE
        JSR     OUTCHAR
        JSR     INBYTE	; LOAD FILL DATA
        BCC     MFILLE ; CHECK IF CORRECT
        TAB
        LDX     XTEMP
MFILL2  STAB    ,X     ; STORE DATA
        INX
        CPX     XHI
        BNE     MFILL2
        JMP     PROMPT
MFILLE  LDX     #PAERRTX
        JSR     PDATA
        JMP     PROMPT

;**************
; DUMP MEMORY (MD AAAA)
DUMPERR	LDX	#PAERRTX
	JSR	PDATA
	JMP	PROMPT
DUMP	LDAA	#$20
	JSR	OUTCHAR
	JSR	BADDR
	BCC	DUMPERR	; END IF ADDRESS NOT OK
	STX	XHI
	LDX	#CRLFTX
	JSR	PDATA
DUMP0	LDAA	XLOW
	ANDA	#$F0	; BEGIN DUMP AT $xxx0
	STAA	XLOW
	LDX	#DUMPTX
	JSR	PDATA
	LDAA	#16
	STAA	TEMP	; ROW COUNTER. 16 ROWS = 1 MEMORY PAGE
;* PRINT ROW
DUMP1	LDX	#XHI
	JSR	OUT4HS	; PRINT ADDRESS
	LDAA	#$20
	JSR	OUTCHAR
	LDX	XHI
	STX	XTEMP	; SAVE X FOR ASCII PRINT
;* PRINT ROW OF BYTES
DUMP2	JSR	OUT2HS
	STX	XHI	; SAVE NOW INCREMENTED X
	LDAA	XLOW
	ANDA	#$0F	; FILTER OUT LAST NIB
	CMPA	#$08
	BNE	DUMP25
	LDAA	#$20
	JSR	OUTCHAR
DUMP25	CMPA	#$00	; LAST BYTE IN ROW?
	BNE	DUMP2
	LDX	XTEMP	; RESTORE POINTER
	STX	XHI	  ; FOR ASCII DUMP
	LDAA	#$20
	JSR	OUTCHAR
;* PRINT ROW OF ASCII
DUMP3	LDAA	0,X
	CMPA	#$7E
	BHI	DUMP4	; BYTE IS NOT PRINTABLE
	CMPA	#$20
	BGE	DUMP5	; BYTE IS PRINTABLE
DUMP4	LDAA	#'.
DUMP5	JSR	OUTCHAR	; PRINT ASCII CHAR
	INX
	STX	XHI	; POINT TO NEXT CHARACTER
	LDAA	XLOW
	ANDA	#$0F
	BNE	DUMP3	; LAST CHARACTER IN ROW?
	LDX	#CRLFTX
	JSR	PDATA
	DEC	TEMP
	BEQ	DUMPE	; LAST ROW?
	BRA	DUMP1
DUMPE	JSR	INCHAR
	CMPA	#$0D
	BEQ	DUMP0	; DUMP NEXT PAGE
	JMP	PROMPT

;**************
; LOAD S1 RECORD
LOAD	LDX	#LDTX
	JSR	PDATA
LOAD1	LDAA	#$0D
	JSR	OUTCHAR
LOAD2	LDAA	#$39	; (RTS)
	STAA	CONOVEC	; DISABLE CONSOLE OUTPUT
	JSR	INCHAR
	CMPA	#'S
	BNE	LOAD2   ; 1ST CHAR NOT (S)
	JSR	INCHAR  ; READ CHAR
	CMPA	#'9      
	BEQ	LOAD21  ; 2ND CHAR (9)
	CMPA	#'1
	BNE	LOAD2   ; 2ND CHAR NOT (1)
	CLR	CKSM    ; CLEAR CHECKSUM
	JSR	INBYTE	; READ BYTE
	TAB
	ADDB	CKSM
	STAB	CKSM
	SUBA	#2
	STAA	TEMP	; BYTE COUNT
	JSR	BADDR
	BCC	LOAD19	; ADDRESS OK?
	LDAB	CKSM
	ADDB	XHI
	ADDB	XLOW
	STAB	CKSM
LOAD11	JSR	INBYTE
	TAB
	ADDB	CKSM
	STAB	CKSM
	DEC	TEMP
	BEQ	LOAD15	; ZERO BYTE COUNT
	STAA	,X	; STORE DATA
	INX
	BRA	LOAD11
LOAD15	INC	CKSM	; INCREMENT CHECKSUM
	BEQ	LOAD1
LOAD19	LDAA	#$7E	; (JMP EXTENDED)
	STAA	CONOVEC	; ENABLE CONSOLE OUTPUT
	LDX	#LFAILTX	; PRINT ERROR MESSAGE
        JSR	PDATA
LOAD20	JSR	CONIVEC
        ANDA	#$DF
        CMPA	#'Y
        BNE	LOAD20
	JMP	PROMPT
LOAD21	JSR	INCHAR
	CMPA	#$0D
	BNE	LOAD21
	LDAA	#$7E	; (JMP EXTENDED)
	STAA	CONOVEC	; ENABLE CONSOLE OUTPUT
	LDX	#LDOKTX
	JSR	PDATA
	JMP	PROMPT

;**************
; PAGE SELECT ROUTINE
PAGE	LDAA	#'=
	JSR	OUTCHAR
	LDAA	#$FF
	STAA	PIA1DIR	; SET ALL OUTPUT
	JSR	INHEX
	BCC	PAGERR
	CMPA	#$07
	BLS	PAGESET
PAGERR	LDX	#PAERRTX
	JSR	PDATA
	JMP	PROMPT
PAGESET	ORAA	#$80	; MASK EXTERNAL INTERRUPTS
	STAA	PIA1DAT
	JMP	PROMPT

;**************
; INIT BUILTIN ACIA
SCIINIT	LDAA    #$04    ; ENABLE INTERNAL ACIA, INTERNAL CLOCK, 115200 BAUD
	STAA    ACIAMOD
	LDAA    #$0A    ; ENABLE RECIEVE AND TRANSMITT DATA
	STAA    ACIASTA
	LDAA    ACIARXD	; FLUSH BUFFER AND CLEAR ERROR FLAGS
	RTS
	
;**************
; BUILTIN ACIA OUTPUT FROM A-ACC
SCIOUT	PSHB		; SAVE B-REG
SCIOUT1	LDAB	ACIASTA
	ASLB
	ASLB
	ASLB
	BCC	SCIOUT1	; READY FOR NEXT CHARACTER
	STAA	ACIATXD
	PULB	; RESTORE	B-REG
	RTS

;**************
; BUILTIN ACIA INPUT TO A-ACC
SCIINER	LDAA	ACIARXD	; ON ERROR, FLUSH BUFFER AND CLEAR ERROR FLAG
SCIIN	LDAA	ACIASTA
	ANDA	#$C0	; FILTER OUT RDRF AND ORFE
	CMPA	#$00
	BEQ	SCIIN	; WAIT FOR CHARACTER
	CMPA	#$40
	BEQ	SCIINER	; CHECK FOR FRAMING ERROR
	LDAA	ACIARXD	; READ RECIEVED CHARACTER
        RTS

;**************
; BUILTIN ACIA STATUS TO A-ACC
;  RETURNS 1 ON CHAR WAITING. 0 ON NO CHAR
SCISTAE	LDAA	ACIARXD	; ON ERROR, FLUSH BUFFER AND CLEAR ERROR FLAG
SCISTAT	LDAA	ACIASTA
	ANDA	#$C0	; FILTER OUT RDRF AND ORFE
	CMPA	#$00
	BEQ	SCISTA0	; NO ERROR AND NO CHARACTER
	CMPA	#$40
	BEQ	SCISTAE	; CHECK FOR ERROR
	LDAA	#$01	; CHARACTER WAITING
        RTS
SCISTA0	LDAA	#$00
	RTS

;**************
; OUTPUT/INPUT ONE CHAR TO/FROM A-REGISTER AND ECHO
INCHAR  JSR	CONIVEC
	JMP	CONOVEC
OUTCHAR EQU	CONOVEC

;**************
; PRINT DATA POINTED AT BY X-REG
PDATA2  JSR     OUTCHAR
	INX
PDATA   LDAA    ,X
	CMPA    #4
	BNE     PDATA2  ; GO ON IF NOT EOT
	RTS

;**************
; OUTPUT HEX CHARS
OUTHL   LSRA            ; OUT HEX LEFT BCD DIGIT
	LSRA
	LSRA
	LSRA
OUTHR   ANDA    #$F     ; OUT HEX RIGHT BCD DIGIT
	ADDA    #$30
	CMPA    #$39
	BLS     OUTHE
	ADDA    #$7
OUTHE	JMP	OUTCHAR

OUT2H   LDAA    0,X
        JSR     OUTHL   ; OUTPUT LEFT HEX CHAR
	LDAA    0,X
	INX
        JMP     OUTHR   ; OUTPUT RIGHT HEX CHAR

OUT4HS  BSR     OUT2H   ; OUTPUT 4 HEX CHAR + SPACE
OUT2HS  BSR     OUT2H   ; OUTPUT 2 HEX CHAR + SPACE
OUTS    LDAA    #$20    ; SPACE
	JMP     OUTCHAR   ; (BSR & RTS)

;**************
; INPUT HEX CHAR INTO A-ACC
INHEX   JSR     INCHAR
	SUBA    #$30
	BMI     NOTHEX
	CMPA    #$09
        BLE     IN1HG
        ANDA    #$DF    ; CONVERT TO UPPER CASE
	CMPA    #$11
	BMI     NOTHEX
	CMPA    #$16
	BGT     NOTHEX
	SUBA    #7
IN1HG	SEC	; INPUT OK. SET CARRY
	RTS
NOTHEX  CLC	; INPUT BAD. CLEAR CARRY
	RTS

;**************
; INPUT BYTE (TWO FRAMES) INTO A-ACC
INBYTE	JSR	INHEX	; GET HEX CHAR
	BCC	INBYTE1
	ASLA
	ASLA
	ASLA
	ASLA
	TAB
        JSR	INHEX
	BCC	INBYTE1
	ABA
	SEC	; GOOD INPUT
	RTS
INBYTE1	CLC	; BAD INPUT
	RTS

;**************
; BUILD ADDRESS INTO X-REG
BADDR   BSR     INBYTE	; READ FIRST FRAME
	BCC     BADDRE
	STAA    XHI
	BSR     INBYTE	; READ SECOND FRAME
	BCC     BADDRE
	STAA    XLOW
	LDX     XHI     ; (X) ADDRESS WE BUILD
BADDRE  RTS


;**************
; SOFTWARE INTERRUPT SEQUENCE
SFE	STS	SP
	JSR	PRTREG
	JMP	PROMPT

;**************
; TRAP INTERRUPT SEQUENCE
TRAP    STS     SP      ; SAVE TARGET STACKPOINTER
        LDX     #TRAPTX
        JSR     PDATA
        LDX     SP
        LDAB    #$6
        ABX
        JSR	OUT4HS
        LDX     #CRLFTX
        JSR     PDATA
        JMP     PROMPT

;**************
; INITIATE VECTOR JUMPTABLE
INITVEC	LDAA	#$7E	; JMP EXT OP CODE
	LDX     #PROMPT
	STAA	NMIVEC
        STX     NMIVEC+1
        LDX     #SFE
	STAA	SWIVEC
        STX     SWIVEC+1
        LDX     #GO
	STAA	IRQVEC
        STX     IRQVEC+1
        LDX     #GO
	STAA	TMICVEC
        STX     TMICVEC+1
        LDX     #GO
	STAA	TMOCVEC
        STX     TMOCVEC+1
        LDX     #GO
	STAA	TMOFVEC
        STX     TMOFVEC+1
	LDX	#SCIOUT
	STAA	CONOVEC
	STX	CONOVEC+1
	LDX	#SCIIN
	STAA	CONIVEC
	STX	CONIVEC+1
	LDX	#SCISTAT
	STAA	CONSVEC
	STX	CONSVEC+1
        RTS

;**************
; VECTORS
        ORG     $FFEE
        dc.w     TRAP	; FFEE-EF	TRAP

	ORG     $FFF2
        dc.w     TMOFVEC	; FFF2-3  TIMER OVER FLOW
        dc.w     TMOCVEC	; FFF4-5  TIMER OUTPUT COMPARE
        dc.w     TMICVEC	; FFF6-7  TIMER INPUT CAPTURE
	dc.w     IRQVEC	; FFF8-9	IRQ
	dc.w     SWIVEC	; FFFA-B	SOFTWARE INTERUPT
	dc.w     NMIVEC	; FFFC-D	NMI
	dc.w     RESET	; FFFE-F	RESET

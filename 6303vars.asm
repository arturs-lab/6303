END         equ $FF     ; Mark END OF TEXT      
REG_DDRP1   equ $00     ; PORT 1 DDR
REG_PORT1   equ $02     ; PORT 1 I/O Address
REG_DDRP2   equ $01     ; PORT 2 DDR
REG_PORT2   equ $03     ; PORT 2 I/O Address
REG_TCSR1   equ $08     ;Timer Control/Status Reg 1
REG_FRCH    equ $09     ; Free Running Counter MSB
REG_FRCL    equ $0A     ; Free Running Counter LSB
REG_OCR1H   equ $0B     ; Output Compare Reg 1 MSB
REG_OCR1L   equ $0C     ; Output Compare Reg 1 LSB
REG_TCSR2   equ $0F     ;Timer Control/Status Reg 2
REG_RMCR    equ $10     ;RATE AND MODE CONTROL REGISTER
REG_TRCSR1  equ $11     ;TXD/RXD CONTROL AND STATUS REG.
REG_RDR     equ $12     ;RECEIVE DATA REGISTER
REG_TDR     equ $13     ;TRANSMIT DATA REGISTER
REG_PORT5   equ $15     ; PORT 5 I/O Address
REG_DDRP6   equ $16     ; PORT 6 DDR       
REG_PORT6   equ $17     ; PORT 6 I/O Address
REG_DDRP5   equ $20     ; PORT 5 DDR
REG_TRCSR2  equ $1E     ;TXD/RXD CONTROL AND STATUS REG#2.
REG_TCSSR3  equ $1B     ;TIMER CONTROL AND STATUS REG#3.
REG_TCONR   equ $1C     ;TIMER2 CONSTANT REG.
REG_T2CNT   equ $1D     ;TIMER2 COUNTER REG.

; AMJ: 20191230 code cleanup and changes to make this work with my board
; my board has following memory map:
; $0000 - $1fff IO ports
; $2000 - $3fff 8K space left for additional RAM, maybe video
; $4000 - $bfff RAM, 32K - 2x 16K banks switched among 8 available banks
; $c000 - $ffff ROM, 16K
; moved everything from $220 address range to my location of RAM
; changed all ORGs to suit my layout
; also, I'm running at 7.5MHz clock, so delay loop and timing vars had to be changed too

; VARIOUS DELAYS
DEL20MS	equ $2400	; 20 ms delay originally 3510 dec.
DEL1MS	equ $a6	; 1 ms delay originally 175
DEL125MS	equ $5200	; delay 125 ms originally 21937
DELMAX	equ $FFFF	; maximim delay, approx 384ms
DELBEEP	equ 18934	; beep delay orig $300C=12300

; utility procedures - delay, serial transmit etc
UTIL		equ $f7c4

; location of bank switching routine
BANKSW		equ $e054

; HD6321 or W6522 chip $11c0 - 11cf
hd6321	equ $11c0	; HD6321 PIA
PRA		equ hd6321
PRB		equ hd6321 + 2
DDRA		equ hd6321
DDRB		equ hd6321 + 2
CRA		equ hd6321 + 1
CRB		equ hd6321 + 3

AYSEL		equ $11d0	; select AY chip, $11d0-$11d1
SNDSEL	equ $11d2	; speccy sound IF $11d2
MIC		equ $02	; microphone sound input (tape) is on D0,
				; sound output register is on D1
AUDIOSTAT	equ $11d3	; HD6321 IRQ lines
EXTSEL	equ $11d4	; external select connector $11d4 - 11df

RAMB4		equ $11f8	; low ram ($4000-$7FFF) bank mapping
RAMB8		equ $11f9	; high  ram ($8000-$BFFF) bank mapping
RAMBc		equ $11fa	; ram in ROM area($C000-$FFFF) bank mapping
ROMB		equ $11fb	; bit 0: ROM bank 0 or 1
				; bit 1: select ROM or RAM page indicated by RAMBc
CPLDl		equ $11fc	; CPLD extra port low
CPLDh		equ $11fd	; CPLD extra port high
LED		equ $11fe	; LED address
SPREG		equ $11ff	; CPLD special reg, spare pins on system bus connector
RAMLO		equ $4000	; beginning of RAM
RAMMID		equ $8000	; beginning of second bank of RAM, middle of ram
RAMHI		equ $c000	; end of RAM + 1
GOADDR	equ RAMLO + $1000	; address for "go" command $2000 originally

;SPINIT	equ RAMHI - 4027	; = $B045 initial value of SP in BASIC
;XINIT		equ RAMHI - 3969	; = $B07F initial value of X in BASIC
;SPTOP		equ RAMHI - 16	; = $BFF0 SP stack top
;MEMENDI	equ RAMHI - 4097	; = $AFFF

; moved to under $8000 so that second bank of RAM can be switched around
SPINIT	equ RAMMID - 4027	; = $7045 initial value of SP in BASIC
XINIT		equ RAMMID - 3969	; = $707F initial value of X in BASIC
SPTOP		equ RAMMID - 16	; = $7FF0 SP stack top
MEMENDI	equ RAMMID - 4097	; = $6FFF

BUFNXTI	equ RAMLO + $B0
VARPAGE	equ (RAMLO >> 8) + $2	; AMJ: on what RAM page (A8-A15) are vars located? Originally $00, then $02, now $42 for my board
VARADDR	equ VARPAGE << 8	; needed later for other definitions

;**** INTERNAL RAM System Variables *****************************
;** The HD6303YCP has 256 bytes of internal RAM at $0040-$013F **
;** The HD6303RP has 128 bytes of internal RAM at $0080-$00FF **
            org	   RAMLO + $40        ; Start of Internal RAM + $40	    
TX_BYTE     equ    RAMLO + $40          ; Byte to send
RX_BYTE     equ    RAMLO + $41          ; Byte recieved
FLAGS_A     equ    RAMLO + $42          ; Flags A: Bit 0=ECHO                            
COUNT_A     equ    RAMLO + $43          ; Counter A
COUNT_B     equ    RAMLO + $44          ; Counter B
COUNT_C     equ    RAMLO + $45          ; Counter C
ADDR_HI     equ    RAMLO + $46
ADDR_LO     equ    RAMLO + $47
TEMP_01     equ    RAMLO + $48          ; Temp Storage 1
TEMP_02     equ    RAMLO + $49          ; Temp Storage 2
TEMP_03     equ    RAMLO + $4A          ; Temp Storage 3
TEMP_04     equ    RAMLO + $4B          ; Temp Storage 4
REC_LEN     equ    RAMLO + $4C          ; iHex Record Length 
REC_TYPE    equ    RAMLO + $4D          ; iHex Record Type
REC_CSUM    equ    RAMLO + $4F          ; iHex Record Checksum
BLINK_CT    equ    RAMLO + $50          ; Blink Counter
IRQFLAG1    equ    RAMLO + $51          ; IRQ Save Flag
IRQFLAG2    equ    RAMLO + $52          ; IRQ Read Flag

OPCD        equ    RAMLO + $53          ; Opcode for disassembly
ROWADDH     equ    RAMLO + $54          ; Mnemonic Table Row Address
ROWADDL     equ    RAMLO + $55          
OPCFLAGS    equ    RAMLO + $56          ;Opcode flags
DISABUFP    equ    RAMLO + $57          ;Dissasmbly buffer pointer (2 bytes)
DISADD1     equ    RAMLO + $59          ;Disassembly start address (2 bytes)
DISADD2     equ    RAMLO + $5B          ;Disassembly End address (2 bytes)
LINECT      equ    RAMLO + $5D          ;Disassembly Line count 

; these two did not like being moved out of page 0 because of code being used to acess them
; so had to keep them on page 0
DISABUF     equ    $80          ;Dissasmbly buffer (16 bytes $60-$6F) AMJ: my internal RAM starts at $80 so I had to move this there
PARMBUF     equ    $90          ;Parameter buffer 5 bytes $70-$74
PARMLEN     equ    RAMLO + $75          ;Parameter Length 
MSTMRXL     equ    RAMLO + $76          ;10ms Timer Low byte
MSTMRXM     equ    RAMLO + $77          ;10ms Timer Mid byte
MSTMRXH     equ    RAMLO + $78          ;10ms Timer Hi byte 

IRQJUMP     equ    RAMLO + $80          ; IRQ Jump Table Start(store ISR address at IRXxxx+1)
IRQNMI      equ    RAMLO + $83          ; Non Maskable Interrupt
IRQSWI      equ    RAMLO + $86          ; Software Interrupt
IRQIRQ1     equ    RAMLO + $89          ; IRQ1
IRQICI      equ    RAMLO + $8C          ; Timer1 Input Capture
IRQOCI      equ    RAMLO + $8F          ; Timer 1 Output Capture
IRQTOI      equ    RAMLO + $92          ; Timer 1 Overflow
IRQCMI      equ    RAMLO + $95          ; Timer 2 Counter Match
IRQIRQ2     equ    RAMLO + $98          ; IRQ2
IRQSIO      equ    RAMLO + $9B          ; RDRF+ORFE+TDRE+PER

DREG_A      equ    RAMLO + $F0          ;A Register
DREG_B      equ    RAMLO + $F1          ;B Register
DREG_XH     equ    RAMLO + $F2          ;X Register
DREG_XL     equ    RAMLO + $F3
DREG_SPH    equ    RAMLO + $F4          ;Stack Pointer
DREG_SPL    equ    RAMLO + $F5
DREG_PCH    equ    RAMLO + $F6          ;Program Counter
DREG_PCL    equ    RAMLO + $F7
DREG_F      equ    RAMLO + $F8          ;Flags
GOPTR		equ	 RAMLO + $FE

; *************************************************************************
;   MicroBASIC Constants      $0220-$0390
; *************************************************************************
MAXLIN      equ    $72	 ; dc.b $72     ; Max Line Length
BACKSP      equ    $7F	 ; dc.b $7F    ;// EMK puTTY backspace = 127
CANCEL      equ    $1B	 ; dc.b $1B    ;// EMK Use ESC as cancel

;  **** ORG $4220  *** MEMORY VARIABLES *****     
            ORG VARADDR
INDEX1      equ    VARADDR + $20	 
INDEX2      equ    VARADDR + $22	  
INDEX3      equ    VARADDR + $24	  
INDEX4      equ    VARADDR + $26	  
SAVESP      equ    VARADDR + $28	  
NEXTBA      equ    VARADDR + $2A	 ;  dc.w END/BASICTOP  
WORKBA      equ    VARADDR + $2C	 ;  dc.w END/BASICTOP  
SOURCE      equ    VARADDR + $2E	 ;  dc.w END/BASICTOP  
PACKLN      equ    VARADDR + $30	  
HIGHLN      equ    VARADDR + $32	  
BASPNT      equ    VARADDR + $34	  
BASLIN      equ    VARADDR + $36	  
PUSHTX      equ    VARADDR + $38	  
XSTACK      equ    VARADDR + $3A	 ; dc.w $707F 
RNDVAL      equ    VARADDR + $3C	  
DIMPNT      equ    VARADDR + $3E	  
DIMCAL      equ    VARADDR + $40	  
PRCNT       equ    VARADDR + $42	               ;Print counter
MEMEND      equ    VARADDR + $46	 ; dc.w $6FFF 	;FDB $1FFF 			#### /DTU
ARRTAB      equ    VARADDR + $48	  
KEYWD       equ    VARADDR + $4A	  
TSIGN       equ    VARADDR + $4C	   
NCMPR       equ    VARADDR + $4D	   
TNUMB       equ    VARADDR + $4E	   
ANUMB       equ    VARADDR + $4F	   
BNUMB       equ    VARADDR + $50	   
AESTK       equ    VARADDR + $51	 ; dc.w ASTACK 
FORPNT      equ    VARADDR + $53	 ; dc.w FORSTK 
VARPNT      equ    VARADDR + $55	 ; dc.w VARTAB 
SBRPNT      equ    VARADDR + $57	 ; dc.w SBRSTK 
SBRSTK      equ    VARADDR + $59	   
FORSTK      equ    VARADDR + $69	 
DIMVAR      equ    VARADDR + $99	 ;dc.w VARTAB      
BUFNXT      equ    VARADDR + $AC	 ;dc.w $00B0 
ENDBUF      equ    VARADDR + $AE	 ;dc.w $00B0
BUFFER      equ    VARADDR + $B0	 
VARTAB      equ    VARADDR + $100	 
ASTACK      equ    VARADDR + $18C
BASICTOP	  equ    VARADDR + $190   ;Use this in ROM implementation

; *************************************************************************
;;OPCDTYPE    equ    $F840
;;OPCDEXCP    equ    $F850
;;MNETBLH     equ    $F860
;;MNETBLL     equ    $F9F0
; ***********************************************************************
; * External Call Jump Table
; ***********************************************************************
;DELAYX    equ    $FBD0         ;Delay based on contents of X approx 10.5us/count         
;OUTCHR    equ    $FBD3         ;Send byte in A to Serial Port
;INCHR     equ    $FBD6         ;wait for a serial byte and return in A
;INCHRE    equ    $FBD9         ;wait for a serial byte and return in A with echo
;PUTS      equ    $FBDC         ;Transmit data indexed by X
;OUTHEX    equ    $FBDF         ;Output A as 2 HEX digits
;GETHEXB   equ    $FBE2         ;Wait until a HEX byte is entered 
;INHEXB    equ    $FBE5         ;Input 2 hex digits return with byte value in A
;GETADDR   equ    $FBE8         ;Get 4 byte address, save in ADDR_HI & ADDR_LO
;DODUMP    equ    $FBEB         ;Jump here to save regs, print regs and return 
;DMPREG    equ    $FBEE         ;Save current state of registers in RAM
;PRTREGS   equ    $FBF1         ;Send saved register values to terminal
;BEEP      equ    $FBF4         ;Beep based on contents of A & B
;INCHRIF   equ    $FBF7         ;Input char if available .. Returns zero if none

	org RAMLO
; when loading header from tape, first 8 bytes are file name,
; followed by address to be saved to
; followed by length of data
; https://faqwiki.zxnet.co.uk/wiki/Spectrum_tape_interface#Header_block
tsavecnt	equ RAMLO			; length of data
tsavebuf	equ tsavecnt + 2		; buffer for header, file type
tsavename	equ tsavebuf + 1		; file name
tsavelen	equ tsavename + 10	; count of bytes to be saved to tape
tsavep1	equ tsavelen + 2		; param 1
tsavep2	equ tsavep1 + 2		; param 2
tsavetim	equ tsavep2 + 2		; leader timers

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


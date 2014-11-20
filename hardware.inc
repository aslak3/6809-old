; memory map
; $xx - ram
; 1$x - io
; 11xx - rom

; ram map
; 0000 -> 0fff - temp storage (stack at 0fff)
; 4000 -> 7fff - 16kb for rom when uploading, also banked
; rom map
; c000 -> ffff - program

; ascii constants

NULL            equ $00
BS              equ $08
CR              equ $0d
LF              equ $0a
SOH		equ $01
ACK		equ $06
NAK		equ $15
EOT		equ $04
SP		equ $20

;;; onboard

; 88681

BASE88681	equ $8000
MRA88681	equ BASE88681+$0
SRA88681	equ BASE88681+$1
CSRA88681	equ BASE88681+$1
CRA88681	equ BASE88681+$2
THRA88681	equ BASE88681+$3
RHRA88681	equ BASE88681+$3
ACR88681	equ BASE88681+$4
IMR88681	equ BASE88681+$5
CTU88681	equ BASE88681+$6
CTL88681	equ BASE88681+$7
MRB88681	equ BASE88681+$8
SRB88681	equ BASE88681+$9
CSRB88681	equ BASE88681+$9
CRB88681	equ BASE88681+$a
THRB88681	equ BASE88681+$b
RHRB88681	equ BASE88681+$b
IVR886861	equ BASE88681+$c
OPCR88681	equ BASE88681+$d
OPRSET88681	equ BASE88681+$e ; write
STARTCT88681	equ BASE88681+$e ; read command
OPRRESET88681	equ BASE88681+$f ; write
STOPCT88681	equ BASE88681+$f ; read command

; ide

IDEBASE		equ $8100
IDEDATA		equ IDEBASE+$0
IDEERR		equ IDEBASE+$1
IDEFEATURES	equ IDEBASE+$1
IDECOUNT	equ IDEBASE+$2
IDELBA0		equ IDEBASE+$3
IDELBA1		equ IDEBASE+$4
IDELBA2		equ IDEBASE+$5
IDELBA3		equ IDEBASE+$6
IDEHEADS	equ IDEBASE+$6
IDESTATUS	equ IDEBASE+$7
IDECOMMAND	equ IDEBASE+$7

; v9938 (not really expansion, as no enable pin on expansion header)

VBASE		equ $8200
VPORT0		equ VBASE
VPORT1		equ VBASE+1
VPORT2		equ VBASE+2
VPORT3		equ VBASE+3

VDIRECTPORT	equ VPORT1

VSTATUSREG	equ 15
VSTATUSPORT	equ VPORT1
VPALETTEREG	equ 16
VPALETTEPORT	equ VPORT2
VINDIRECTREG	equ 17
VINDIRECTPORT	equ VPORT3
VADDRREG	equ 14
VADDRPORT	equ VPORT1

VBANKREG	equ 45
VMODE0REG	equ 0
VMODE1REG	equ 1
VMODE2REG	equ 8
VMODE3REG	equ 9
VVIDBASEREG	equ 2
VCOLBASELREG	equ 3
VCOLBASEHREG	equ 10
VPATTBASEREG	equ 4
VSPRBASELREG	equ 5
VSPRBASEHREG	equ 11
VSPRGBASEREG	equ 6
VCOLOUR1REG	equ 7
VCOLOUR2REG	equ 12
VBLINKREG	equ 13
VCOLBURST1REG	equ 20
VCOLBURST2REG	equ 21
VCOLBURST3REG	equ 22
VDISPLAYPOSREG	equ 18
VDISPLAYOFFREG	equ 23
VINTLINEREG	equ 19
VSRCXLREG	equ 32
VSRCXHREG	equ 33
VSRCYLREG	equ 34
VSRCYHREG	equ 35
VDSTXLREG	equ 36
VDSTXHREG	equ 37
VDSTYLREG	equ 38
VDSTYHREG	equ 39
VDOTSXLREG	equ 40
VDOTSXHREG	equ 41
VDOTSYLREG	equ 42
VDOTSYHREG	equ 43
VCOMCOLOURREG	equ 44
VCOMARGREG	equ 45
VCOMCOMMANDREG	equ 46

; ay

AYBASE		equ $8300
AYLATCHADDR	equ AYBASE	; address latch
AYWRITEADDR	equ AYBASE+$1	; write
AYREADADDR	equ AYBASE	; read

AYAFREQL	equ $00	; tone generator A frequency (low 8 bits)
AYAFREQH	equ $01	; tone generator A frequency (high 4 bits)
AYBFREQL	equ $02	; tone generator B frequency (low 8 bits)
AYBFREQH	equ $03	; tone generator B frequency (high 4 bits)
AYCFREQL	equ $04	; tone generator C frequency (low 8 bits)
AYCFREQH	equ $05	; tone generator C frequency (high 4 bits)
AYNOISEFREQ	equ $06	; noise generator frequency (low 5 bits)
AYCTRL		equ $07	; I/O port and mixer control
AYAAMPL		equ $08	; channel A amplitude (4 bits), envelope enable
AYBAMPL		equ $09	; channel B amplitude (4 bits), envelope enable
AYCAMPL		equ $0a	; channel C amplitude (4 bits), envelope enable
AYENVFREQL	equ $0b	; envelope frequency (low 8 bits)
AYENVFREQH	equ $0c	; envelope frequency (high 8 bits)
AYENVSHAPE	equ $0d	; envelope shape (low 4 bits)
AYIOA		equ $0e	; I/O port A data
AYIOB		equ $0f	; I/O port B data

; joystick masks

JOYUP		equ $01
JOYDOWN		equ $02
JOYLEFT		equ $04
JOYRIGHT	equ $08
JOYFIRE1	equ $10
JOYFIRE2	equ $20

;;; expansion

; 6522

BASE6522	equ $9000
PORTB6522	equ BASE6522+$0
PORTA6522	equ BASE6522+$1
DDRB6522	equ BASE6522+$2
DDRA6522	equ BASE6522+$3
T1CL6522	equ BASE6522+$4
T1CH6522	equ BASE6522+$5
T1LL6522	equ BASE6522+$6
T1LH6522	equ BASE6522+$7
T2LL6522	equ BASE6522+$8
T2CL6522	equ BASE6522+$8
T2CH6522	equ BASE6522+$9
SR6522		equ BASE6522+$a
ACR6522		equ BASE6522+$b
PCR6522		equ BASE6522+$c
IFR6522		equ BASE6522+$d
IER6522		equ BASE6522+$e
ORAX6522	equ BASE6522+$f

; 65spi

SPIBASE		equ $9100
SPIDATAIN	equ SPIBASE+$0
SPIDATAOUT	equ SPIBASE+$0
SPISTATUS	equ SPIBASE+$1
SPICONTROL	equ SPIBASE+$1
SPIDIV		equ SPIBASE+$2
SPISS		equ SPIBASE+$3

;;; cpld registers

; ide high byte latch

IDEHIGH		equ $a000

; memory bank latch

BANKLATCH	equ $a100

; sounder pitch

SOUNDER		equ $a200

; config data

CONFIGJUMPERS	equ $a300
CONFIGWRITEROM	equ $01
CONFIGUARTMODE	equ $02

; irq routing

IRQFILTER	equ $a400
FIRQFILTER	equ $a500

; current irq state

IRQSTATUS	equ $a600

; irq bits

IRQDS3234	equ $01
IRQ65SPI	equ $02
IRQ65C22	equ $04
IRQVDC		equ $08
IRQ88C681	equ $10

; memory

HIRAMSTART	equ $4000
HIRAMEND	equ $7fff
RAMSTART        equ $0000
RAMEND          equ $7fff
ROMSTART        equ $c000
ROMEND          equ $ffff
ROMCOPYSTART    equ $4000
ROMCOPYEND      equ $7fff

STACKEND	equ $1fff
USERSTACKEND	equ $2800

PCOFFSET	equ 10

; macros

enableinter	macro
		andcc #$af
		endm

disableinter	macro
		orcc #$50
		endm
		
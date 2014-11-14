; memory map
; 0xxx - ram
; 10xx - io
; 11xx - rom

; ram map
; 0000 -> 0fff - temp storage (stack at 0fff)
; 4000 -> 7fff - 16kb for rom when uploading, also banked
; rom map
; c000 -> ffff - program

; ascii constants

NULL            .equ 0x00
BS              .equ 0x08
CR              .equ 0x0d
LF              .equ 0x0a
SOH		.equ 0x01
ACK		.equ 0x06
NAK		.equ 0x15
EOT		.equ 0x04
SP		.equ 0x20

;;; onboard

; 88681

BASE88681	.equ 0x8000
MRA88681	.equ BASE88681+0x0
SRA88681	.equ BASE88681+0x1
CSRA88681	.equ BASE88681+0x1
CRA88681	.equ BASE88681+0x2
THRA88681	.equ BASE88681+0x3
RHRA88681	.equ BASE88681+0x3
ACR88681	.equ BASE88681+0x4
IMR88681	.equ BASE88681+0x5
CTU88681	.equ BASE88681+0x6
CTL88681	.equ BASE88681+0x7
MRB88681	.equ BASE88681+0x8
SRB88681	.equ BASE88681+0x9
CSRB88681	.equ BASE88681+0x9
CRB88681	.equ BASE88681+0xa
THRB88681	.equ BASE88681+0xb
RHRB88681	.equ BASE88681+0xb
IVR886861	.equ BASE88681+0xc
OPCR88681	.equ BASE88681+0xd
OPRSET88681	.equ BASE88681+0xe ; write
STARTCT88681	.equ BASE88681+0xe ; read command
OPRRESET88681	.equ BASE88681+0xf ; write
STOPCT88681	.equ BASE88681+0xf ; read command

; ide

IDEBASE		.equ 0x8100
IDEDATA		.equ IDEBASE+0x0
IDEERR		.equ IDEBASE+0x1
IDEFEATURES	.equ IDEBASE+0x1
IDECOUNT	.equ IDEBASE+0x2
IDELBA0		.equ IDEBASE+0x3
IDELBA1		.equ IDEBASE+0x4
IDELBA2		.equ IDEBASE+0x5
IDELBA3		.equ IDEBASE+0x6
IDEHEADS	.equ IDEBASE+0x6
IDESTATUS	.equ IDEBASE+0x7
IDECOMMAND	.equ IDEBASE+0x7

; v9938 (not really expansion, as no enable pin on expansion header)

VBASE		.equ 0x8200
VPORT0		.equ VBASE
VPORT1		.equ VBASE+1
VPORT2		.equ VBASE+2
VPORT3		.equ VBASE+3

VDIRECTPORT	.equ VPORT1

VSTATUSREG	.equ 15
VSTATUSPORT	.equ VPORT1
VPALETTEREG	.equ 16
VPALETTEPORT	.equ VPORT2
VINDIRECTREG	.equ 17
VINDIRECTPORT	.equ VPORT3
VADDRREG	.equ 14
VADDRPORT	.equ VPORT1

VBANKREG	.equ 45
VMODE0REG	.equ 0
VMODE1REG	.equ 1
VMODE2REG	.equ 8
VMODE3REG	.equ 9
VVIDBASEREG	.equ 2
VCOLBASELREG	.equ 3
VCOLBASEHREG	.equ 10
VPATTBASEREG	.equ 4
VSPRBASELREG	.equ 5
VSPRBASEHREG	.equ 11
VSPRGBASEREG	.equ 6
VCOLOUR1REG	.equ 7
VCOLOUR2REG	.equ 12
VBLINKREG	.equ 13
VCOLBURST1REG	.equ 20
VCOLBURST2REG	.equ 21
VCOLBURST3REG	.equ 22
VDISPLAYPOSREG	.equ 18
VDISPLAYOFFREG	.equ 23
VINTLINEREG	.equ 19
VSRCXLREG	.equ 32
VSRCXHREG	.equ 33
VSRCYLREG	.equ 34
VSRCYHREG	.equ 35
VDSTXLREG	.equ 36
VDSTXHREG	.equ 37
VDSTYLREG	.equ 38
VDSTYHREG	.equ 39
VDOTSXLREG	.equ 40
VDOTSXHREG	.equ 41
VDOTSYLREG	.equ 42
VDOTSYHREG	.equ 43
VCOMCOLOURREG	.equ 44
VCOMARGREG	.equ 45
VCOMCOMMANDREG	.equ 46

; ay

AYBASE		.equ 0x8300
AYLATCHADDR	.equ AYBASE	; address latch
AYWRITEADDR	.equ AYBASE+0x1	; write
AYREADADDR	.equ AYBASE	; read

AYAFREQL	.equ 0x00	; tone generator A frequency (low 8 bits)
AYAFREQH	.equ 0x01	; tone generator A frequency (high 4 bits)
AYBFREQL	.equ 0x02	; tone generator B frequency (low 8 bits)
AYBFREQH	.equ 0x03	; tone generator B frequency (high 4 bits)
AYCFREQL	.equ 0x04	; tone generator C frequency (low 8 bits)
AYCFREQH	.equ 0x05	; tone generator C frequency (high 4 bits)
AYNOISEFREQ	.equ 0x06	; noise generator frequency (low 5 bits)
AYCTRL		.equ 0x07	; I/O port and mixer control
AYAAMPL		.equ 0x08	; channel A amplitude (4 bits), envelope enable
AYBAMPL		.equ 0x09	; channel B amplitude (4 bits), envelope enable
AYCAMPL		.equ 0x0a	; channel C amplitude (4 bits), envelope enable
AYENVFREQL	.equ 0x0b	; envelope frequency (low 8 bits)
AYENVFREQH	.equ 0x0c	; envelope frequency (high 8 bits)
AYENVSHAPE	.equ 0x0d	; envelope shape (low 4 bits)
AYIOA		.equ 0x0e	; I/O port A data
AYIOB		.equ 0x0f	; I/O port B data

; joystick masks

JOYUP		.equ 0x01
JOYDOWN		.equ 0x02
JOYLEFT		.equ 0x04
JOYRIGHT	.equ 0x08
JOYFIRE1	.equ 0x10
JOYFIRE2	.equ 0x20

;;; expansion

; 6522

BASE6522	.equ 0x9000
PORTB6522	.equ BASE6522+0x0
PORTA6522	.equ BASE6522+0x1
DDRB6522	.equ BASE6522+0x2
DDRA6522	.equ BASE6522+0x3
T1CL6522	.equ BASE6522+0x4
T1CH6522	.equ BASE6522+0x5
T1LL6522	.equ BASE6522+0x6
T1LH6522	.equ BASE6522+0x7
T2LL6522	.equ BASE6522+0x8
T2CL6522	.equ BASE6522+0x8
T2CH6522	.equ BASE6522+0x9
SR6522		.equ BASE6522+0xa
ACR6522		.equ BASE6522+0xb
PCR6522		.equ BASE6522+0xc
IFR6522		.equ BASE6522+0xd
IER6522		.equ BASE6522+0xe
ORAX6522	.equ BASE6522+0xf

; 65spi

SPIBASE		.equ 0x9100
SPIDATAIN	.equ SPIBASE+0x0
SPIDATAOUT	.equ SPIBASE+0x0
SPISTATUS	.equ SPIBASE+0x1
SPICONTROL	.equ SPIBASE+0x1
SPIDIV		.equ SPIBASE+0x2
SPISS		.equ SPIBASE+0x3

; 65spi

SPIBASE		.equ 0x9100
SPIDATAIN	.equ SPIBASE+0x0
SPIDATAOUT	.equ SPIBASE+0x0
SPISTATUS	.equ SPIBASE+0x1
SPICONTROL	.equ SPIBASE+0x1
SPIDIV		.equ SPIBASE+0x2
SPISS		.equ SPIBASE+0x3

;;; cpld registers

; ide high byte latch

IDEHIGH		.equ 0xa000

; memory bank latch

BANKLATCH	.equ 0xa100

; sounder pitch

SOUNDER		.equ 0xa200

; config data

CONFIGJUMPERS	.equ 0xa300
CONFIGWRITEROM	.equ 0x01
CONFIGUARTMODE	.equ 0x02

; irq routing

IRQFILTER	.equ 0xa400
FIRQFILTER	.equ 0xa500

; current irq state

IRQSTATUS	.equ 0xa600

; irq bits

IRQDS3234	.equ 0x01
IRQ65SPI	.equ 0x02
IRQ65C22	.equ 0x04
IRQVDC		.equ 0x08
IRQ88C681	.equ 0x10

; memory

HIRAMSTART	.equ 0x4000
HIRAMEND	.equ 0x7fff
RAMSTART        .equ 0x0000
RAMEND          .equ 0x7fff
ROMSTART        .equ 0xc000
ROMEND          .equ 0xffff
ROMCOPYSTART    .equ 0x4000
ROMCOPYEND      .equ 0x7fff

STACKEND	.equ 0x1fff
USERSTACKEND	.equ 0x2800

PCOFFSET	.equ 10

; macros

.macro		enableinterrupts
		andcc #0xaf
.endm

.macro		disableinterrupts
		orcc #0x50
.endm
		
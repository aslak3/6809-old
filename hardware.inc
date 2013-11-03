; memory map
; 0xxx - ram
; 10xx - io
; 11xx - rom

; io map
; 10xx x000 xxxx xxxX - serial
; 10xx x001 xxxx xxxX - unused
; 10xx x010 xxxx xxxx - simple diagnostic latch
; 10xx x011 xxxx XXXX - 6522
; 10xx x100 xxxx xXXX - ide port

; ram map
; 0000 -> 0fff - temp storage (stack at 0fff)
; 4000 -> 7fff - 16kb for rom when uploading
; rom map
; c000 -> ffff - program

; ascii constants

NULL            .equ 0x00
BS              .equ 0x08
CR              .equ 0x0d
LF              .equ 0x0a

; 88681

BASE88681	.equ 0x8000
MRA88681	.equ BASE88681+0x0
SRA88681	.equ BASE88681+0x1
CSRA88681	.equ BASE88681+0x1
CRA88681	.equ BASE88681+0x2
THRA88681	.equ BASE88681+0x3
RHRA88681	.equ BASE88681+0x3
ACR886861	.equ BASE88681+0x4
; ...
OPCR88681	.equ BASE88681+0xd
OPRSET88681	.equ BASE88681+0xe
OPRRESET88681	.equ BASE88681+0xf

; latch

LATCH		.equ 0x8010

; IDE

IDEBASE		.equ 0x8020
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

; ay

AYBASE		.equ 0x8030
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

; 6522

BASE6522	.equ 0x8040
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

; memory

RAMSTART        .equ 0x0000
RAMEND          .equ 0x7fff
ROMSTART        .equ 0xc000
ROMEND          .equ 0xffff
ROMCOPYSTART    .equ 0x4000
ROMCOPYEND      .equ 0x7fff

STACKEND	.equ 0x1fff

PCOFFSET	.equ 10

; SPI bits

MOSI		.equ 0x01
SCLK 		.equ 0x02
SS		.equ 0x04
MISO		.equ 0x80

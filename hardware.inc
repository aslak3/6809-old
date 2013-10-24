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

; serial port addresses - for readability dupe the read/write locations

SERIALCR	.equ 0x8000
SERIALRX	.equ 0x8001
SERIALSR	.equ 0x8000
SERIALTX	.equ 0x8001

; latch

LATCH		.equ 0x8200

; 6522

PORTB6522	.equ 0x8300
PORTA6522	.equ 0x8301
DDRB6522	.equ 0x8302
DDRA6522	.equ 0x8303
T1CL6522	.equ 0x8304
T1CH6522	.equ 0x8305
T1LL6522	.equ 0x8306
T1LH6522	.equ 0x8307
T2LL6522	.equ 0x8308
T2CL6522	.equ 0x8308
T2CH6522	.equ 0x8309
SR6522		.equ 0x830a
ACR6522		.equ 0x830b
PCR6522		.equ 0x830c
IFR6522		.equ 0x830d
IER6522		.equ 0x830e
ORAX6522	.equ 0x830f

; IDE

IDEDATA		.equ 0x8400
IDEERR		.equ 0x8401
IDEFEATURES	.equ 0x8401
IDECOUNT	.equ 0x8402
IDELBA0		.equ 0x8403
IDELBA1		.equ 0x8404
IDELBA2		.equ 0x8405
IDELBA3		.equ 0x8406
IDEHEADS	.equ 0x8406
IDESTATUS	.equ 0x8407
IDECOMMAND	.equ 0x8407

; 88681

MRA88681	.equ 0x8500 
SRA88681	.equ 0x8501
CSRA88681	.equ 0x8501
CRA88681	.equ 0x8502
THRA88681	.equ 0x8503
RHRA88681	.equ 0x8503

OPCR88681	.equ 0x850d
OPRSET88681	.equ 0x850e
OPRRESET88681	.equ 0x850f

; ay

AY		.equ 0x8500 
AYLATCHADDR	.equ AY		; address latch
AYWRITEADDR	.equ AY+1	; write
AYREADADDR	.equ AY		; read

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

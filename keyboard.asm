; contains routines for dealing with the keyboard; interrupt handler etc

normaltab:

; row 0

		.byte 0				; escape
		.byte 0				; unwired
		.byte 0				; f1
		.byte 0				; f2
		.byte 0				; f3
		.byte 0				; f4
		.byte 0				; f5
		.byte 0				; unwired

		.byte 0				; f6
		.byte 0				; blank
		.byte 0				; f7
		.byte 0				; f8
		.byte 0				; f9
		.byte 0				; f10
		.byte 0				; help
		.byte 0				; unused

; row 1

		.ascii '~'
		.ascii '1'
		.ascii '2'
		.ascii '3'
		.ascii '4'
		.ascii '5'
		.ascii '6'
		.ascii '7'

		.ascii '8'
		.ascii '9'
		.ascii '0'
		.ascii '-'
		.ascii '='
		.ascii '\'
		.byte 0				; cursor up
		.byte 0				; unused

; row 2

		.byte 0				; tab
		.ascii 'q'
		.ascii 'w'
		.ascii 'e'
		.ascii 'r'
		.ascii 't'
		.ascii 'y'
		.ascii 'u'

		.ascii 'i'
		.ascii 'o'
		.ascii 'p'
		.ascii '['
		.ascii ']'
		.byte 0xff			; replaced: return
		.byte 0				; cursor up
		.byte 0				; unused

; row 3

		.byte 0				; caps lock
		.ascii 'a'
		.ascii 's'
		.ascii 'd'
		.ascii 'f'
		.ascii 'g'
		.ascii 'h'
		.ascii 'j'

		.ascii 'k'
		.ascii 'l'
		.ascii ';'
		.ascii /'/
		.byte 0				; blank
		.byte 0xff			; delete
		.byte 0				; cursor right
		.byte 0				; unused

; row 4

		.byte 0				; blank
		.ascii 'z'
		.ascii 'x'
		.ascii 'c'
		.ascii 'v'
		.ascii 'b'
		.ascii 'n'
		.ascii 'm'

		.ascii ','
		.ascii '.'
		.ascii '/'
		.byte 0				; unwired
		.ascii ' '
		.byte 0xff			; backspace
		.byte 0				; cursor down
		.byte 0				; unused

; row 5 (meta)

		.byte 0xff			; right shift
		.byte 0				; right alt
		.byte 0				; right amiga
		.byte 0xff			; ctrl
		.byte 0xff			; left shift
		.byte 0				; left alt
		.byte 0				; left amiga

shifttab:

; row 0

		.byte 0				; escape
		.byte 0				; unwired
		.byte 0				; f1
		.byte 0				; f2
		.byte 0				; f3
		.byte 0				; f4
		.byte 0				; f5
		.byte 0				; unwired

		.byte 0				; f6
		.byte 0				; blank
		.byte 0				; f7
		.byte 0				; f8
		.byte 0				; f9
		.byte 0				; f10
		.byte 0				; help
		.byte 0				; unused

; row 1

		.ascii '`'
		.ascii '!'
		.ascii '@'
		.ascii ' '
		.ascii '$'
		.ascii '%'
		.ascii '^'
		.ascii '&'

		.ascii '*'
		.ascii '('
		.ascii ')'
		.ascii '_'
		.ascii '+'
		.ascii '|'
		.byte 0				; cursor up
		.byte 0				; unused

; row 2

		.byte 0				; tab
		.ascii 'Q'
		.ascii 'W'
		.ascii 'E'
		.ascii 'R'
		.ascii 'T'
		.ascii 'Y'
		.ascii 'U'

		.ascii 'I'
		.ascii 'O'
		.ascii 'P'
		.ascii '{'
		.ascii '}'
		.byte 0xff			; replaced: return
		.byte 0				; cursor up
		.byte 0				; unused

; row 3

		.byte 0				; caps lock
		.ascii 'A'
		.ascii 'S'
		.ascii 'D'
		.ascii 'F'
		.ascii 'G'
		.ascii 'H'
		.ascii 'J'

		.ascii 'K'
		.ascii 'L'
		.ascii ':'
		.ascii '"'
		.byte 0				; blank
		.byte 0xff			; delete
		.byte 0				; cursor right
		.byte 0				; unused

; row 4

		.byte 0				; blank
		.ascii 'Z'
		.ascii 'X'
		.ascii 'C'
		.ascii 'V'
		.ascii 'B'
		.ascii 'N'
		.ascii 'M'

		.ascii '<'
		.ascii '>'
		.ascii '?'
		.byte 0				; unwired
		.ascii ' '
		.byte 0xff			; backspace
		.byte 0				; cursor down
		.byte 0				; unused

; row 5 (meta)

		.byte 0xff			; right shift
		.byte 0				; right alt
		.byte 0				; right amiga
		.byte 0xff			; ctrl
		.byte 0xff			; left shift
		.byte 0				; left alt
		.byte 0				; left amiga

keyinit:	clr keyrawmode

		clr asciilshifton
		clr asciirshifton

		clr keyreadpointer
		clr keywritepointer

		; port d handler routine
		ldx #keyhandler
		stx handleuartpd

		; set port d up as 9600,8,n,1
		lda #0b10000011
		sta LCRPD16C654			; 8n1 and config baud
		lda #0x30
		sta DLLPD16C654
		lda #0x00
		sta DLMPD16C654		; 9600
		lda #0b00000011
		sta LCRPD16C654			; 8n1 and back to normal

		; enable rx interrupt on port d
		lda #0x01
		sta IERPD16C654

		rts

keyhandler:	lda RHRPD16C654
		ldb keyrawmode
		tstb
		beq asciimode
		ldx #keybuffer
		bsr keypushbuf
keyhandlero:	rts

asciimode:	bita #0x80
		beq keydown
		clrb
		bra keydownend
keydown:	ldb #1
keydownend:	anda #0x7f		; mask out the up/down; its in b
		cmpa #0x54		; left shift
		beq asciilshift
		cmpa #0x50		; right shift
		beq asciirshift
		tstb			; key direction?
		bne asciiconvert	; down, so convert it
		rts			; otherwise we are done

asciilshift:	stb asciilshifton
		rts			; done in the keyboard handler
asciirshift:	stb asciirshifton
		rts			; done in the keyboard handler

asciiconvert:	ldx #keybuffer
		cmpa #0x2d		; return
		beq asciireturn
		cmpa #0x4d		; backspace
		beq asciibacksp
		tst asciilshifton
		bne asciishifted
		tst asciirshifton
		bne asciishifted
		ldx #normaltab
asciiconvertdo:	lda a,x			; find ascii value
		ldx #keybuffer
		bsr keypushbuf
asciiconverto:	rts

asciishifted:	ldx #shifttab
		bra asciiconvertdo

asciireturn:	lda #CR
		bsr keypushbuf
		bra asciiconverto

asciibacksp:	lda #BS
		bsr keypushbuf
		bra asciiconverto

; sub to push a into the keyboard buffer

keypushbuf:	tsta				; don't fill nulls
		beq keypushbufo
		ldb keywritepointer
		sta b,x
		incb
		andb #0x3f
		stb keywritepointer
keypushbufo:	rts

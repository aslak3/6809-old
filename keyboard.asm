		section _main

; contains routines for dealing with the keyboard; interrupt handler etc

normaltab:

; column a

		.ascii '1'			; column a
		.byte 0				; left arrow
		.byte 0				; control
		.byte 0				; run/stop
		.ascii ' '
		.byte 0				; commodore
		.ascii 'q'
		.ascii '2'

; column b

		.ascii '3'
		.ascii 'w'
		.ascii 'a'
		.byte 0				; left shift
		.ascii 'z'
		.ascii 's'
		.ascii 'e'
		.ascii '4'

; column c

		.ascii '5'
		.ascii 'r'
		.ascii 'd'
		.ascii 'x'
		.ascii 'c'
		.ascii 'f'
		.ascii 't'
		.ascii '6'

; column d

		.ascii '7'
		.ascii 'y'
		.ascii 'g'
		.ascii 'v'
		.ascii 'b'
		.ascii 'h'
		.ascii 'u'
		.ascii '8'

; column e

		.ascii '9'
		.ascii 'i'
		.ascii 'j'
		.ascii 'n'
		.ascii 'm'
		.ascii 'k'
		.ascii 'o'
		.ascii '0'

; column f

		.ascii '+'
		.ascii 'p'
		.ascii 'l'
		.ascii ','
		.ascii '.'
		.ascii ':'
		.ascii '@'
		.ascii '-'

; column g

		.byte 0
		.ascii '*'
		.ascii ';'
		.ascii '/'
		.byte 0			; right shift
		.ascii '='
		.byte 0			; home
		.byte 0

; column h

		.byte 0xff		; replaced: delete
		.byte 0xff		; replaced: return
		.byte 0			; left/right
		.byte 0			; up/down
		.byte 0			; f1
		.byte 0			; f3
		.byte 0			; f5
		.byte 0			; f7

shifttab:

; column a

		.ascii '!'			; column a
		.byte 0				; left arrow
		.byte 0				; control
		.byte 0				; run/stop
		.ascii ' '
		.byte 0				; commodore
		.ascii 'Q'
		.ascii '"'

; column b

		.ascii '#'
		.ascii 'W'
		.ascii 'A'
		.byte 0				; left shift
		.ascii 'Z'
		.ascii 'S'
		.ascii 'E'
		.ascii '$'

; column c

		.ascii '%'
		.ascii 'R'
		.ascii 'D'
		.ascii 'X'
		.ascii 'C'
		.ascii 'F'
		.ascii 'T'
		.ascii '&'

; column d

		.ascii "'"
		.ascii 'Y'
		.ascii 'G'
		.ascii 'V'
		.ascii 'B'
		.ascii 'H'
		.ascii 'U'
		.ascii '('

; column e

		.ascii ')'
		.ascii 'I'
		.ascii 'J'
		.ascii 'N'
		.ascii 'M'
		.ascii 'K'
		.ascii 'O'
		.byte 0

; column f

		.byte 0
		.ascii 'P'
		.ascii 'L'
		.ascii '<'
		.ascii '>'
		.ascii '{'
		.byte 0
		.byte 0

; column g

		.byte 0
		.byte 0
		.ascii '}'
		.ascii '?'
		.byte 0			; right shift
		.byte 0
		.byte 0			; home
		.byte 0

; column h

		.byte 0xff		; replaced: delete
		.byte 0xff		; replaced: return
		.byte 0			; left/right
		.byte 0			; up/down
		.byte 0			; f1
		.byte 0			; f3
		.byte 0			; f5
		.byte 0			; f7

keyhandler:	lda PORTA6522
		ldb keyrawmode
		tstb
		beq asciimode
		ldx #keybuffer
		bsr keypushbuf
		rts

asciimode:	bita #0x80
		beq keydown
		clrb
		bra keydownend
keydown:	ldb #1
keydownend:	anda #0x7f		; mask out the up/down; its in b
		cmpa #@13		; left shift
		beq asciilshift
		cmpa #@64		; right shift
		beq asciirshift
		tstb			; key direction?
		bne asciiconvert	; down, so convert it
		rts			; otherwise we are done

asciilshift:	stb asciilshifton
		rts			; done in the keyboard handler
asciirshift:	stb asciirshifton
		rts			; done in the keyboard handler

asciiconvert:	ldx #keybuffer
		cmpa #@71		; return
		beq asciireturn
		cmpa #@01		; backspace
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

		endsection

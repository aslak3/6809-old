		include 'v99.inc'

		section _main

; local terminal stuff

twocolpalette:	fcb $00,$00,$00  ; black background
		fcb $07,$07,$07  ; white text

terminit:	lbsr vinit

		loadconstreg VMODE0REG,%00000100
		loadconstreg VMODE1REG,%01010000
		loadconstreg VMODE2REG,%00001000
		loadconstreg VMODE3REG,%00000010

		loadconstreg VPATTBASEREG,$00	; bottom half - patterns
		loadconstreg VVIDBASEREG,$23	; top half - video

		ldx #twocolpalette	; set the colour reg pointer
		ldy #2			; 2 colours
		lbsr vsetcolours

		loadconstreg VCOLOUR1REG,$10

		lbsr vclearvram

		ldx #fontdata
		ldy #8*32
		ldu #8*96		; 96 characters of font data
		lbsr vwrite

		lda #24
		ldx #tlinestarts
		ldy #$8000
linecalcnext:	sty ,x++
		leay 80,y
		deca
		bne linecalcnext

		clr trow
		clr tcol

		; for the keyboard

		; via interrupt routing

		lda IRQFILTER
		ora #IRQ65C22
		sta IRQFILTER

		; via handshaking etc

		lda #$08
		sta PCR6522
		lda #$82
 		sta IER6522

		rts

tclearscreen:	clr trow
		clr tcol
		ldy #$8000
		lbsr vseekcommon
		lbsr vseekwrite
		ldx 80*24
		clra
tclearscnext:	sta VPORT0
		leax 1,x
		bne tclearscnext
		rts

tputchar:	pshs x,y,u
		cmpa #CR
		beq handlecr
		cmpa #LF
		beq handlelf
		cmpa #BS
		beq handlebs

		lbsr tmovecursor
		sta VPORT0
		sleep
		lda #127
		sta VPORT0

		lda tcol
		inca
		sta tcol
		cmpa #80
		beq newline

tputcharout:	puls x,y,u
		rts
		
handlecr:	lbsr tblankcurrent
		clr tcol
		bra tputcharout

newline:	clr tcol
handlelf:	lda trow
		inca
		sta trow
		cmpa #24
		bne newlineout
		lbsr tscroller
		lda #23
		sta trow
newlineout:	bra tputcharout

handlebs:	lbsr tblankcurrent
		tst tcol
		beq oldline
		dec tcol
		lbsr tdrawcursor
		bra tputcharout

oldline:	lda #79
		sta tcol
		dec trow
		bra tputcharout

tdrawcursor:	lbsr tmovecursor
		lda #127
		sta VPORT0
		rts

tmovecursor:	pshs a
		ldb trow
		lslb
		ldy #tlinestarts
		ldy b,y
		ldb tcol
		leay b,y
		lbsr vseekcommon
		lbsr vseekwrite
		puls a
		rts

tblankcurrent:	lbsr tmovecursor
		clra
		sta VPORT0
		rts

tscroller:	ldb #23
		ldy #$8000+80
tscrollernext:	pshs b
		ldx #tscrollline
		ldu #80
		lbsr vread
		leay -80,y
		ldx #tscrollline
		ldu #80
		lbsr vwrite
		leay 80+80,y
		puls b
		decb
		bne tscrollernext
		leay -80,y
		lbsr vseekcommon
		lbsr vseekwrite
		clra
		ldy #80
scrollclearn:	sta VPORT0
		leay -1,y
		bne scrollclearn
		rts

; gets a character and sticks it in a

tgetchar:	lda keywritepointer
		suba keyreadpointer
		bne keyfound
;		sync
		bra tgetchar
keyfound:	pshs b,x
		ldb keyreadpointer
		ldx #keybuffer
		lda b,x
		incb
		andb #$3f
		stb keyreadpointer
		puls b,x
		rts

; make the v port the current active io device

tactive:	ldx #tgetchar
		stx iogetcharp
		ldx #tputchar
		stx ioputcharp
		ldx #ionull
		stx iogetwto
		rts

		endsection
		
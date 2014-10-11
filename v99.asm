; ym9938 (and 58)

; stores whats in a in the constant register, register

.macro		sleep
		nop
		nop
		nop
.endm

.macro		loadareg register
		sta VDIRECTPORT
		lda #register|0x80
		sta VDIRECTPORT
.endm

; stores the value in the register

.macro		loadconstreg register, value
		lda #value
		sta VDIRECTPORT
		lda #register|0x80
		sta VDIRECTPORT
.endm

; gets the value of the status register

.macro		getastatusreg register
		loadareg VSTATUSREG
		lda VSTATUSPORT
.endm

; trival colour palette for text mode


; save into the registers, starting from the register in a. the values
; pointed by x and counted by y are copied in

ymindirect:	ora #0x80		; auto incmreneting mode
		loadareg VINDIRECTREG	; set up the indirect reg
ymindirectnext:	lda ,x+			; get the value
		sta VINDIRECTPORT	; save it in the register
		leay -1,y		; dec the counter
		bne ymindirectnext	; see if there is more
		rts

; sets the colour in a to the r g b poited to by x

ymsetcolour:	pshs a			; save the current colour
		loadareg VPALETTEREG	; we are writing to the colour reg
		lda ,x+			; get red
		lsla			; move it to the high nibble
		lsla			; ..
		lsla			; ..
		lsla			; ..
		ldb ,x+			; next, put green in b
		ora ,x+			; or in blue over red
		sta VPALETTEPORT	; output red and blue
		stb VPALETTEPORT	; output green
		puls a			; get the colour back
		rts

; set the colours - x points at r g b list, y sets the number of colours to
; set

ymsetcolours:	clra			; start from col 0
ymsetcoloursn:	bsr ymsetcolour		; sets this colour
		inca			; next colour
		leay -1,y		; dec the count of colours
		bne ymsetcoloursn	; more?
		rts

twocolpalette:	.byte 0x00, 0x00, 0x00	; black background
		.byte 0x07, 0x07, 0x07	; white text

; sets up "core" registers

yminitterm:	loadconstreg VBANKREG, 0x00
		loadconstreg VDISPLAYPOSREG, 0x00
		loadconstreg VDISPLAYOFFREG, 0x00
		loadconstreg VINTLINEREG, 0x00

ymsettext1mode:	loadconstreg VMODE0REG, 0b00000100
		loadconstreg VMODE1REG, 0b01010000
		loadconstreg VMODE2REG, 0b00001000
		loadconstreg VMODE3REG, 0b00000010

		loadconstreg VPATTBASEREG, 0x00	; bottom half - patterns
		loadconstreg VVIDBASEREG, 0x23	; top half - video

		ldx #twocolpalette	; set the colour reg pointer
		ldy #2			; 2 colours
		lbsr ymsetcolours

		loadconstreg VCOLOUR1REG, 0x10

		lbsr ymclearvram

		ldx #fontdata
		ldy #8*32
		ldu #8*96		; 96 characters of font data
		lbsr ymwrite

		lda #24
		ldx #ymlinestarts
		ldy #0x8000
linecalcnext:	sty ,x++
		leay 80,y
		deca
		bne linecalcnext

		clr ymrow
		clr ymcol

		rts

ymclearvram:	lda #1
		ldy #0x0000
		lbsr ymseekvram
		ldx #0x0000
		clra
ymclearnext:	sta VPORT0
		leax 1,x
		bne ymclearnext		; 64kbytes
		rts

ymclearscreen:	clr ymrow
		clr ymcol
		lda #1
		ldy #0x8000
		lbsr ymseekvram
		ldx 80*24
		clra
ymclearscnext:	sta VPORT0
		leax 1,x
		bne ymclearscnext
		rts

; writes to y in vram, count u bytes, from x in mpu ram

ymwrite:	lda #1			; writing
		pshs y
		lbsr ymseekvram
		tfr u,y			; leau does not set z, so need y
ymwritenext:	lda ,x+
		sta VPORT0
		leay -1,y
		bne ymwritenext
		puls y
		rts

; reads into x in mpu, count u bytes, from y in vram

ymread:		clra			; reading
		pshs y
		lbsr ymseekvram		; setup for writing using y
		tfr u,y			; leau does not set z, so need y
ymreadnext:	lda VPORT0
		sta ,x+
		leay -1,y
		bne ymreadnext
		puls y
		rts

; prepare the vdc for reading or writing from y in vram. a is 1 for writing

ymseekvram:	pshs a			; save writing state
		tfr y,d
		lsra
		lsra
		lsra
		lsra
		lsra
		lsra			; six shifts put a15 at bit 1
		loadareg VADDRREG
		tfr y,d			; retore original address
		stb VADDRPORT		; the low 8 bits of address (easy)
		anda #0b00111111	; mask out the high two bits
		puls b			; get writing flag into b
		tstb			; see if we are not writing
		beq ymseekout		; if reading then just output
		ora #0b01000000		; set writing mode
ymseekout:	sta VADDRPORT
		sleep
		rts

ymputchar:	cmpa #CR
		beq handlecr
		cmpa #LF
		beq handlelf
		cmpa #BS
		beq handlebs

		lbsr ymmovecursor
		sta VPORT0
		sleep
		lda #127
		sta VPORT0

		lda ymcol
		inca
		sta ymcol
		cmpa #80
		beq newline

		rts
		
handlecr:	lbsr ymblankcurrent
		clr ymcol
		rts

newline:	clr ymcol
handlelf:	lda ymrow
		inca
		sta ymrow
		cmpa #24
		bne newlineout
		lbsr ymscroller
		lda #23
		sta ymrow
newlineout:	rts

handlebs:	lbsr ymblankcurrent
		tst ymcol
		beq oldline
		dec ymcol
		lbsr ymdrawcursor
		rts

oldline:	lda #79
		sta ymcol
		dec ymrow
		rts

ymdrawcursor:	lbsr ymmovecursor
		lda #127
		sta VPORT0
		rts

ymmovecursor:	pshs a
		ldb ymrow
		lslb
		ldy #ymlinestarts
		ldy b,y
		ldb ymcol
		leay b,y
		lda #1
		lbsr ymseekvram
		puls a
		rts

ymblankcurrent:	lbsr ymmovecursor
		clra
		sta VPORT0
		rts

ymscroller:	pshs x,u
		ldb #23
		ldy #0x8000+80
ymscrollernext:	pshs b
		ldx #ymscrollline
		ldu #80
		lbsr ymread
		leay -80,y
		ldx #ymscrollline
		ldu #80
		lbsr ymwrite
		leay 80+80,y
		puls b
		decb
		bne ymscrollernext
		lda #1
		leay -80,y
		lbsr ymseekvram
		clra
		ldy #80
scrollclearn:	sta VPORT0
		leay -1,y
		bne scrollclearn
		puls x,u
		rts
		
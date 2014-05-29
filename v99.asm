; ym9938 (and 58)

; stores whats in a in the constant register, register

.macro		loadareg register
		sta VDIRECTPORT
		lda #register|0x80
		sta VDIRECTPORT
.endm

; stores the value in the register

.macro		loadconstreg register value
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

twocolpalette:	.byte 0x00, 0x00, 0x04	; blue tinged background
		.byte 0x07, 0x07, 0x07	; white text

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

; sets up "core" registers

ymsetbasic:	loadconstreg VDISPLAYPOSREG, 0x00
		loadconstreg VDISPLAYOFFREG, 0x00
		loadconstreg VINTLINEREG, 0x00
		rts

ymsettext1mode:	loadconstreg VMODE0REG, 0b00000000
		loadconstreg VMODE1REG, 0b01010000
		loadconstreg VMODE2REG, 0b00001010
		loadconstreg VMODE3REG, 0b10000010

		loadconstreg VPATTBASEREG 0x10	; top half of vram - video
		loadconstreg VVIDBASEREG 0x00	; bottom half of vram - fonts

		rts

ymsettwocols:	ldx #twocolpalette	; set the colour reg pointer
		ldy #2			; 2 colours
		lbsr ymsetcolours

		loadconstreg VCOLOUR1REG 0x10

		rts

ymclearvram:	loadconstreg VADDRREG, 0x00
		clra
		sta VADDRPORT
		lda #0x40		; write mode
		sta VADDRPORT
		ldx #0x0000
ymclearnext:	clr VPORT0
		leax 1,x
		bne ymclearnext		; 64kbytes
		rts

ymloadfonts:	loadconstreg VADDRREG, 0x00
		clra
		sta VADDRPORT
		lda #0x41		; write to 0x100 bytes in for space
		sta VADDRPORT
		ldx #fontdata
		ldy #8*96		; 96 characters of font data
ymloadfontsn:	lda ,x+
		sta VPORT0
		leay -1,y
		bne ymloadfontsn
		rts

ymshowmsg:	loadconstreg VADDRREG, 0x20
		clra
		sta VADDRPORT
		lda #0x40		; write mode
		sta VADDRPORT
ymshowmsgnext:	lda ,x+
		beq ymshowmsgout
		sta VPORT0
		bra ymshowmsgnext
ymshowmsgout:	rts
		
testmsg:	.asciz "Testing the V9938/58... ABCDEFGHJKLMNOPQRSTUVWXYZ"

yminit:		lbsr ymsetbasic
		lbsr ymclearvram
		lbsr ymloadfonts
		lbsr ymsettwocols
		lbsr ymsettext1mode
		
		ldx #testmsg
		lbsr ymshowmsg

		rts

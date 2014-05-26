; ym9938 (and 58)

.macro		loadconstreg register value
		lda #value
		sta YMDIRECTPORT
		lda #register|0x80
		sta YMDIRECTPORT
.endm

.macro		loadareg register
		sta YMDIRECTPORT
		lda #register|0x80
		sta YMDIRECTPORT
.endm

.macro		getastatusreg register
		loadareg YMSTATUSREG
		lda YMSTATUSPORT
.endm

twocolpalette:	.byte 0x00, 0x00, 0x04	; blue tinged background
		.byte 0x07, 0x07, 0x07	; white text

; save into the registers, starting from the register in a. the values
; pointed by x and counted by y are copied in

ymindirect:	ora #0x80		; auto incmreneting mode
		loadareg YMINDIRECTREG	; set up the indirect reg
ymindirectnext:	lda ,x+			; get the value
		sta YMINDIRECTPORT	; save it in the register
		leay -1,y		; dec the counter
		bne ymindirectnext	; see if there is more
		rts

; sets the colour in a to the r g b poited to by x

ymsetcolour:	pshs a			; save the current colour
		loadareg YMPALETTEREG	; we are writing to the colour reg
		lda ,x+			; get red
		lsla			; move it to the high nibble
		lsla			; ..
		lsla			; ..
		lsla			; ..
		ldb ,x+			; next, put green in b
		ora ,x+			; or in blue over red
		sta YMPALETTEPORT	; output red and blue
		stb YMPALETTEPORT	; output green
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

ymsetbasic:	loadconstreg YMDISPLAYPOSREG, 0x00
		loadconstreg YMDISPLAYOFFREG, 0x00
		loadconstreg YMINTLINEREG, 0x00
		rts

ymsettext1mode:	loadconstreg YMMODE0REG, 0b00000000
		loadconstreg YMMODE1REG, 0b01010000
		loadconstreg YMMODE2REG, 0b00001010
		loadconstreg YMMODE3REG, 0b10000010

		loadconstreg YMPATTBASEREG 0x10 ; top half of vram - video
		loadconstreg YMVIDBASEREG 0x00 ; bottom half of vram - fonts

		rts

ymsettwocols:	ldx #twocolpalette
		ldy #2
		lbsr ymsetcolours

; white on black/blue!

		loadconstreg YMCOLOUR1REG 0x10

		rts

ymclearvram:	loadconstreg YMADDRREG, 0x00
		clra
		sta YMADDRPORT
		lda #0x40		; write mode
		sta YMADDRPORT
		ldx #0x0000
ymclearnext:	clr YMPORT0
		leax 1,x
		bne ymclearnext
		rts

ymloadfonts:	loadconstreg YMADDRREG, 0x00
		clra
		sta YMADDRPORT
		lda #0x41		; write to 0x100 bytes in for space
		sta YMADDRPORT
		ldx #fontdata
		ldy #8*96		; 96 characters of font data
ymloadfontsn:	lda ,x+
		sta YMPORT0
		leay -1,y
		bne ymloadfontsn
		rts

ymshowmsg:	loadconstreg YMADDRREG, 0x20
		clra
		sta YMADDRPORT
		lda #0x40		; write mode
		sta YMADDRPORT
ymshowmsgnext:	lda ,x+
		beq ymshowmsgout
		sta YMPORT0
		bra ymshowmsgnext
ymshowmsgout:	rts
		
testmsg:	.asciz "Testing the YM9938/58... ABCDEFGHJKLMNOPQRSTUVWXYZ"

yminit:		lbsr ymsetbasic
		lbsr ymclearvram
		lbsr ymloadfonts
		lbsr ymsettwocols
		lbsr ymsettext1mode
		
		ldx #testmsg
		lbsr ymshowmsg

		rts

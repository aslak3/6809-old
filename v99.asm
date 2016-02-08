; v9938 (and 58) - low level routines

; stores whats in a in the constant register, register

		.include 'v99.inc'

; save into the registers, starting from the register in a. the values
; pointed by x and counted by y are copied in

vindirect:	ora #0x80		; auto incmreneting mode
		loadareg VINDIRECTREG	; set up the indirect reg
vindirectnext:	lda ,x+			; get the value
		sta VINDIRECTPORT	; save it in the register
		leay -1,y		; dec the counter
		bne vindirectnext	; see if there is more
		rts

; sets the colour in a to the r g b poited to by x

vsetcolour:	pshs a			; save the current colour
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

vsetcolours:	clra			; start from col 0
vsetcoloursn:	bsr vsetcolour		; sets this colour
		inca			; next colour
		leay -1,y		; dec the count of colours
		bne vsetcoloursn	; more?
		rts

; sets up "core" registers

vinit:		loadconstreg VBANKREG, 0x00
		loadconstreg VDISPLAYPOSREG, 0x08
		loadconstreg VDISPLAYOFFREG, 0x00
		loadconstreg VINTLINEREG, 0x00

		lbsr vclearvram

		rts

vclearvram:	ldy #0x0000
		lbsr vseekcommon
		lbsr vseekwrite
		ldx #0x0000
		clra
vclearnext:	sta VPORT0
		leax -1,x
		bne vclearnext		; 64kbytes
		rts

; writes to y in vram, count u bytes, from x in mpu ram

vwrite:		pshs y
		lbsr vseekcommon
		lbsr vseekwrite
		tfr u,y			; leau does not set z, so need y
;		stx DMASRC
;		ldx #VPORT0
;		stx DMADST
;		sty DMALENGTH
;		lda #0x01
;		sta DMAFLAGS
vwritenext:	lda ,x+
		sta VPORT0
		leay -1,y
		bne vwritenext
		puls y
		rts

; reads into x in mpu, count u bytes, from y in vram

vread:		pshs y
		lbsr vseekcommon	; setup for writing using y
		lbsr vseekread
		tfr u,y			; leau does not set z, so need y
vreadnext:	lda VPORT0
		sta ,x+
		leay -1,y
		bne vreadnext
		puls y
		rts

; prepare the vdc for reading or writing from y in vram

vseekcommon:	tfr y,d
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
		rts

vseekwrite:	ora #0b01000000		; set writing mode
		sta VADDRPORT
		sleep
		rts

vseekread:	sta VADDRPORT
		sleep
		rts
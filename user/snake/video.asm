; Video bits

videoinit:	jsr jvinit

		loadconstreg VMODE0REG, 0b00000000
		loadconstreg VMODE1REG, 0b01000000
		loadconstreg VMODE2REG, 0b00001000
		loadconstreg VMODE3REG, 0b00000010

		loadconstreg VPATTBASEREG, 0x00 ; bottom half - patterns
		loadconstreg VVIDBASEREG, 0x20  ; top half - video

		loadconstreg VCOLBASELREG, 0x00
		loadconstreg VCOLBASEHREG, 0x01

		leax printattab,pcr
		ldy #0x8000
		lda #24
printattabn:	sty ,x++
		leay 0x20,y
		deca
		bne printattabn

		leax spectrumfont,pcr
		ldy #8*0x20
		ldu #8*96		; 98 characters of font data
		jsr jvwrite

		leax snaketiles,pcr
		ldy #8*0x80
		ldu #8*4		; 3 characters of font data
		jsr jvwrite

		leax badtiles,pcr
		ldy #8*0x88
		ldu #8*1		; 1 characters of font data
		jsr jvwrite

		leax walltiles,pcr
		ldy #8*0x90
		ldu #8*8		; 8 characters of font data
		jsr jvwrite

		ldy #0x8000
		jsr jvseekcommon
		jsr jvseekwrite
		clra
msgdataloop:	sta VPORT0
		inca
		bne msgdataloop

		leax colours,pcr
		ldy #0x4000
		ldu #0x20
		jsr jvwrite

		lda #12
		ldb #10
		leax testmessage,pcr
		lbsr printstrat



		lda #TOPLEFTTILE
		sta stamp,pcr
		clra
		clrb
		lbsr stampat

		lda #TOPTILE
		sta stamp,pcr
		clra
		ldb #1
topborder:	lbsr stampat
		incb
		cmpb #31
		bne topborder

		lda #TOPRIGHTTILE
		sta stamp,pcr
		clra
		ldb #31
		lbsr stampat

		lda #RIGHTTILE
		sta stamp,pcr
		lda #1
		ldb #31
rightborder:	lbsr stampat
		inca
		cmpa #23
		bne rightborder

		lda #BOTTOMRIGHTTILE
		sta stamp,pcr
		lda #23
		ldb #31
		lbsr stampat

		lda #BOTTOMTILE
		sta stamp,pcr
		lda #23
		ldb #1
bottomborder:	lbsr stampat
		incb
		cmpb #31
		bne bottomborder

		lda #BOTTOMLEFTTILE
		sta stamp,pcr
		lda #23
		clrb
		lbsr stampat

		lda #LEFTTILE
		sta stamp,pcr
		lda #1
		clrb
leftborder:	lbsr stampat
		inca
		cmpa #23
		bne leftborder

		lda #SNAKEBODYTILE
		sta stamp,pcr
		lda #8
		ldb #2
snaketest:	lbsr stampat
		ldy #0xffff
		jsr jdelay
		incb
		cmpb #30
		bne snaketest


		rts

testmessage:	.asciz "Hello ZX Spectrum!"

; Prints the string at x (until null) at row a, col b

printstrat:	leay printattab,pcr
		lsla
		ldy a,y
		leay b,y
		jsr jvseekcommon
		jsr jvseekwrite
printstratn:	lda ,x+
		sta VPORT0
		bne printstratn

		rts

; Set VRAM for writing to at row a, col b and output the char at stamp

stampat:	pshs a,b
		leay printattab,pcr
		lsla
		ldy a,y
		leay b,y
		jsr jvseekcommon
		jsr jvseekwrite

		lda stamp,pcr
		sta VPORT0

		puls a,b
		rts

printattab:	.rmb 2*24
stamp:		.rmb 1

colours:

; Speccy font

		.byte 0x10
		.byte 0x10
		.byte 0x10
		.byte 0x10

		.byte 0x10
		.byte 0x10
		.byte 0x10
		.byte 0x10

		.byte 0x10
		.byte 0x10
		.byte 0x10
		.byte 0x10

		.byte 0x10
		.byte 0x10
		.byte 0x10
		.byte 0x10

; "Graphics"

		.byte 0x30		; green snake
		.byte 0x80		; red bad things
		.byte 0x10		; border
		.byte 0x30

		.byte 0x30
		.byte 0x30
		.byte 0x30
		.byte 0x30

		.byte 0x30
		.byte 0x30
		.byte 0x30
		.byte 0x30

		.byte 0x30
		.byte 0x30
		.byte 0x30
		.byte 0x30

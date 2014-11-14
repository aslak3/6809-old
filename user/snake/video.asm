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

		leax colours,pcr
		ldy #0x4000
		ldu #0x20
		jsr jvwrite

		clra
		clrb
		pshu a,b
		lda #23
		ldb #31
		pshu a,b
		lbsr drawbox
		leau 4,u

		lda #0
		ldb #16-4
		leax titlemessage,pcr
		lbsr printstrat

		rts

titlemessage:	.asciz " SNAKE! "

; Prints the string at x (until null) at row a, col b

printstrat:	leay printattab,pcr
		lsla
		ldy a,y
		leay b,y
		jsr jvseekcommon
		jsr jvseekwrite
printstratn:	lda ,x+
		beq printstratout
		sta VPORT0
		bra printstratn

printstratout:	rts

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

peekat:		leay printattab,pcr
		lsla
		ldy a,y
		leay b,y
		jsr jvseekcommon
		jsr jvseekread

		lda VPORT0

		rts

; draw a box:
; u+3 : top left : column
; u+2 : top left : row
; u+1 : bottom right, column
; u+0 : bottom right, row

drawbox:	lda #TOPLEFTTILE
		sta stamp,pcr
		lda 2,u
		ldb 3,u
		lbsr stampat

		lda #TOPTILE
		sta stamp,pcr
		lda 2,u
		ldb 3,u
		incb
topborder:	lbsr stampat
		incb
		cmpb 1,u
		bne topborder

		lda #TOPRIGHTTILE
		sta stamp,pcr
		lda 2,u
		ldb 1,u
		lbsr stampat

		lda #RIGHTTILE
		sta stamp,pcr
		lda 2,u
		ldb 1,u
		inca
rightborder:	lbsr stampat
		inca
		cmpa 0,u
		bne rightborder

		lda #BOTTOMRIGHTTILE
		sta stamp,pcr
		lda 0,u
		ldb 1,u
		lbsr stampat

		lda #BOTTOMTILE
		sta stamp,pcr
		lda 0,u
		ldb 3,u
		incb
bottomborder:	lbsr stampat
		incb
		cmpb 1,u
		bne bottomborder

		lda #BOTTOMLEFTTILE
		sta stamp,pcr
		lda 0,u
		ldb 3,u
		lbsr stampat

		lda #LEFTTILE
		sta stamp,pcr
		lda 2,u
		ldb 3,u
		inca
leftborder:	lbsr stampat
		inca
		cmpa 0,u
		bne leftborder

		rts

; Variables

printattab:	.rmb 2*24
stamp:		.rmb 1

colours:

; Speccy font tiles

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

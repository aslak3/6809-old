; Video bits

videoinit:	jsr jvinit		; do the core init, clear vram

		; graphics mode 1 - 32x24, 8x8 tiles
		loadconstreg VMODE0REG, 0b00000000
		loadconstreg VMODE1REG, 0b01100000
		loadconstreg VMODE2REG, 0b00001000
		loadconstreg VMODE3REG, 0b00000010

		; 0x0000 - tiles
		loadconstreg VPATTBASEREG, 0x00
		; 0x800 - video
		loadconstreg VVIDBASEREG, 0x20
		; 0x400 - colours of tiles
		loadconstreg VCOLBASELREG, 0x00
		loadconstreg VCOLBASEHREG, 0x01
		; hack
		loadconstreg VDISPLAYPOSREG, 0x08

;		leax vdchandler,pcr
;		stx handlevdc

;		lda IRQFILTER
;		ora #IRQVDC
;		sta IRQFILTER

		clr vcounter

		leax printattab,pcr	; prepae lookup table of row->vram
		ldy #0x8000		; vram starts at 0x8000
		lda #24			; 24 rows on the screen
printattabn:	sty ,x++		; save the start of the row
		leay 32,y		; a row is 32 coloumns wide
		deca			; next row
		bne printattabn		; until we've got to the last one

		; load the tile data, starting with the spectrum font

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
		ldu #8*2		; 2 characters of font data
		jsr jvwrite

		leax walltiles,pcr
		ldy #8*0x90
		ldu #8*8		; 8 characters of font data
		jsr jvwrite

		; and the colours (8 tiles per colour)
		leax colours,pcr
		ldy #0x4000
		ldu #0x20
		jsr jvwrite

		rts

; clear whole screen

clearscreen:	ldy #0x8000		; start at the top left corner
		jsr jvseekcommon	; seek here
		jsr jvseekwrite		; for writing
		ldx #24*32		; total number of tiles on screen
		clra			; cant clr MM as it will read
clearscreenn:	sta VPORT0		; write to vram
		leax -1,x		; dec the column count
		bne clearscreenn	; more tiles?
		rts

clearplayarea:	ldy #0x8000		; start at the top left corner
		lda #22			; we need 22 rows (24-2)
		sta ,u			; save row count in a variable
		leay 32,y		; skip the top row of border
nextrow:	leay 1,y		; move across the left hand border
		jsr jvseekcommon	; seek here
		jsr jvseekwrite		; for writing
		ldx #30			; we need 32-2=30 blank tiles
		clra			; clr MM will read, so clear reg a
nextrown:	sta VPORT0		; store it in the vram
		leax -1,x		; dec the coloumn counter
		bne nextrown		; more coloumns?
		leay 31,y		; skip to the left most col on next row
		dec ,u			; dec the row counter
		bne nextrow		; more rows?
		rts

; prints the string at x (until null) at row a, col b

printstrat:	leay printattab,pcr	; the row->vram addres table
		lsla			; double the row; table of addresses
		ldy a,y			; get the vram address
		leay b,y		; add the column count on
		jsr jvseekcommon	; seek to vram address y
		jsr jvseekwrite		; and set write mode
printstratn:	lda ,x+			; get the frist byte we are writing
		beq printstratout	; see if it's a null, then done
		sta VPORT0		; output it to the vdc
		bra printstratn		; back for more
printstratout:	rts

; write the stamp byte to row a, col b and output the char. a and b are
; retained for conistency with peekat.

stampat:	pshs a,b		; save a and b
		leay printattab,pcr	; the row->vram addres table
		lsla			; double the row; table of addresses
		ldy a,y			; get the vram address
		leay b,y		; add the column count on
		jsr jvseekcommon	; seek to vram address y
		jsr jvseekwrite		; and set write mode
		lda stamp,pcr		; load what we are stamping
		sta VPORT0		; output it to the vdc
		puls a,b		; restore a and b
		rts

; the reverse of stampat; the tile at a, b is saved into the stamp variable.
; a, b is retained so a following stampat call can replace the tile
; (assuming it is nothing bad)

peekat:		pshs a,b		; save a and b
		leay printattab,pcr	; the row->vram addres table
		lsla			; double the row; table of addresses
		ldy a,y			; get the vram address
		leay b,y		; add the column count on
		jsr jvseekcommon	; seek to vram address y
		jsr jvseekread		; set to read mode
		lda VPORT0		; read what's at the tile position
		sta peek,pcr		; ave it into the peek variable
		puls a,b		; a and b have the row, col on exit
		rts

; draw a box. passed the coordinates on the u stack as follows, on entry:
;   u+3 : top left : column (b)
;   u+2 : top left : row (a)
;   u+1 : bottom right, column (b)
;   u+0 : bottom right, row (a)

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

vdchandler:	getstatusreg #0
		inc vcounter,pcr
		rts


; Variables

printattab:	.rmb 2*24
stamp:		.rmb 1
peek:		.rmb 1
vcounter:	.rmb 1

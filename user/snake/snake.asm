; Make snake source

		.include 'user.inc'
		.include 'snake.inc'

		.area PROG (REL)

		.org 0

		ldu #USERSTACKEND

		lbsr videoinit

		clr snakebornflag,pcr

		lda #12
		leax rowsnake,pcr
		clrb
rowinitloop:	sta ,x+
		decb
		bne rowinitloop

		lda #16
		leax colsnake,pcr
		clrb
colinitloop:	sta ,x+
		decb
		bne colinitloop

		lda #100
		sta snakelength,pcr
		clr headpos,pcr

		clr rowdirection,pcr
		lda #1
		sta coldirection,pcr

		lda #FOODTILE
		sta stamp,pcr
		lda #3
		ldb #3
		lbsr stampat

		lda #FOODTILE
		sta stamp,pcr
		lda #10
		ldb #30
		lbsr stampat

		lda #FOODTILE
		sta stamp,pcr
		lda #20
		ldb #8
		lbsr stampat

mainloop:	clra
		ldb headpos,pcr
		subb snakelength,pcr
		lbsr drawsnakepart

		lda #SNAKEBODYTILE
		ldb headpos,pcr
		lbsr drawsnakepart

		lbsr movesnake

		ldb headpos,pcr
		lbsr testcollide

		beq nocollision

		lbsr docollision
		beq death

nocollision:	lda #SNAKEHEADTILE
		ldb headpos,pcr
		lbsr drawsnakepart

		ldx #0x2000
controlloop:	lbsr controlsnake
		leax -1,x
		bne controlloop

		bra mainloop

death:		swi

drawsnakepart:	sta stamp,pcr

		leax rowsnake,pcr
		abx
		lda ,x

		leax colsnake,pcr
		abx
		ldb ,x

		lbsr stampat

		rts

movesnake:	ldb headpos,pcr

		leax rowsnake,pcr
		abx
		lda ,x
		adda rowdirection,pcr
		leax rowsnake,pcr
		incb
		abx
		sta ,x

		ldb headpos,pcr

		leax colsnake,pcr
		abx
		lda ,x
		adda coldirection,pcr
		leax colsnake,pcr
		incb
		abx
		sta ,x

		inc headpos,pcr
		rts

testcollide:	leax rowsnake,pcr
		abx
		lda ,x

		leax colsnake,pcr
		abx
		ldb ,x

		lbsr peekat

		rts

docollision:	cmpa #FOODTILE
		beq yumyum
		clra
docollisiono:	rts

yumyum:		inc snakelength,pcr
		lda #1
		bra docollisiono

controlsnake:	jsr jreadjoystick
		bita #JOYLEFT
		bne moveleft
		bita #JOYRIGHT
		bne moveright
		bita #JOYUP
		bne moveup
		bita #JOYDOWN
		bne movedown
controlsnakeo:	rts

moveleft:	lda #-1
		bra moveleftright
moveright:	lda #1
		bra moveleftright
moveup:		lda #-1
		bra moveupdown
movedown:	lda #1
		bra moveupdown

moveleftright:	sta coldirection,pcr
		clr rowdirection,pcr
		bra controlsnakeo
moveupdown:	clr coldirection,pcr
		sta rowdirection,pcr
		bra controlsnakeo

rowsnake:	.rmb 256
colsnake:	.rmb 256
headpos:	.rmb 1
snakelength:	.rmb 1

rowdirection:	.rmb 1
coldirection:	.rmb 1

snakebornflag:	.rmb 1

		.include 'graphics.asm'
		.include 'video.asm'

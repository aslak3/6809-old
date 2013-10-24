; AY-8910

ay8910write:	sta AYLATCHADDR
		stb AYWRITEADDR
		rts

ay8910quiet:	lda #AYCTRL
		ldb #0b11111111
		bsr ay8910write
		rts

ay8910play:	lda #AYCTRL
		ldb #0b11000000
		bsr ay8910write

playloop:	ldd ,x++
		cmpa #0xff
		beq ay8910delay
		bsr ay8910write

		bra playloop

ay8910playout:	lbsr ay8910quiet
		
		rts

ay8910delay:	cmpb #0
		beq ay8910playout
delayb:		lda #0xff
delaya:		deca
		bne delaya
		decb
		bne delayb
		bra playloop

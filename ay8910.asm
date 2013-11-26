; AY-8910

start:    	.byte 0x07, 0xf8, 0x08, 0x18
		.byte 0xff, 0xff

stop:		.byte 0x07, 0xff
		.byte 0xff, 0xff

envelope:	.byte 0x0b, 0x00, 0x0c, 0x03, 0x0d, 0x09 	; envelope
		.byte 0xff, 0xff

notetable:	.byte 0x00, 0x00
		.byte 0x00, 0xee
		.byte 0x00, 0xe5
		.byte 0x00, 0xd4
		.byte 0x00, 0xc8
		.byte 0x00, 0xbd
		.byte 0x00, 0xb2
		.byte 0x00, 0xa8
		.byte 0x00, 0x9f
		.byte 0x00, 0x96
		.byte 0x00, 0x8e
		.byte 0x00, 0x86
		.byte 0x00, 0x7e

ay8910start:	pshs x
		ldx #start
		bsr ay8910streamer
		puls x
		rts

ay8910stop:	pshs x
		ldx #stop
		bsr ay8910streamer
		puls x
		rts

ay8910streamer:	ldd ,x++
		cmpa #0xff
		beq ay8910stout
		sta AYLATCHADDR
		stb AYWRITEADDR
		bra ay8910streamer
ay8910stout:  	rts

ay8910playnote:	pshs x,y
		lsla
		ldy #notetable
		leay a,y
		ldx #envelope
		bsr ay8910streamer
		lda #0x01
		sta AYLATCHADDR
		lda ,y+
		sta AYWRITEADDR
		lda #0x00
		sta AYLATCHADDR
		lda ,y+
		sta AYWRITEADDR
		ldy #0x8000
		lbsr delay
		puls x,y
		rts
 
ay8910playtune:	lbsr ay8910start

playtuneloop:	lda ,x+
		cmpa #0xff
		beq playtuneout
		cmpa #0
		beq playtunedelay

		lbsr ay8910playnote

		bra playtuneloop
		
playtunedelay:	ldy #0xffff
		lbsr delay
		bra playtuneloop

playtuneout:	lbsr ay8910stop
		rts

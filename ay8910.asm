; AY-8910

start:    	.byte 0x07, 0x38, 0x08, 0x18
		.byte 0xff, 0xff

stop:		.byte 0x07, 0xff, 0x08, 0x00
		.byte 0xff, 0xff

envelope:	.byte 0x0b, 0x00, 0x0c, 0x08	 	; envelope
		.byte 0xff, 0xff

shapetable:	.byte 0x00, 0x04, 0x0b, 0x0d
		.byte 0x09, 0x0c, 0x0e, 0x0a

notetable:	.ascii 'c'
		.byte 0x07, 0x77
		.byte 0x80
		.byte 0x07, 0x0e
		.ascii 'd'
		.byte 0x06, 0xa6
		.byte 0x80
		.byte 0x06, 0x46
		.ascii 'e'
		.byte 0x05, 0xec
		.ascii 'f'
		.byte 0x05, 0x96
		.byte 0x80
		.byte 0x05, 0x48
		.ascii 'g'
		.byte 0x04, 0xfb
		.byte 0x80
		.byte 0x04, 0xb4
		.ascii 'a'
		.byte 0x04, 0x70
		.byte 0x80
		.byte 0x04, 0x30
		.ascii 'b'
		.byte 0x03, 0xf4
		.byte 0x80
		.byte 0x03, 0xbb
		.byte 0

ay8910start:	pshs x
		ldx #start
		bsr ay8910streamer
		ldx #envelope
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

ay8910playnote:	pshs x
		lbsr ay8910start
		puls x
		lda #0x0d
		sta AYLATCHADDR
		ldb ayshape
		stb AYWRITEADDR
		tfr x,d
		lda #0x00
		sta AYLATCHADDR
		stb AYWRITEADDR
		tfr x,d
		ldb #0x01
		stb AYLATCHADDR
		sta AYWRITEADDR
		ldx #0xffff
		lbsr aydelay
		rts

ay8910shifter:	stx ayduration
		tfr b,a
		adda ayoctave
		inca
ay8910shloop:	deca
		beq ay8910shout
		lsr ayduration
		ror ayduration+1
		bra ay8910shloop
ay8910shout:	ldx ayduration
		rts
 
ay8910playtune:	lda #4
		sta ayoctave
		clr ayshape
		tfr x,y
playtuneloop:	clr aysharp
noclrsharploop:	lda ,y+
		cmpa #0x00
		beq playtuneout
		cmpa #0x20
		beq playtunedelay
		cmpa #0x6f
		beq playtuneoctave
		cmpa #0x77
		beq playtuneshape
		cmpa #0x23
		beq playtunesharp
		ldx #notetable
notescanloop:	cmpa ,x
		beq notefoundlc
		tfr a,b
		adda #0x20
		cmpa ,x
		tfr b,a
		beq notefounduc
		tst ,x
		beq playtuneloop
		leax 3,x		; skip the duration
		bra notescanloop
notefounduc:	ldb #1
		bra notefound
notefoundlc:	clrb
		bra notefound
notefound:	leax 1,x
		tst aysharp
		beq notenotsharp
		leax 3,x		; sharpen
notenotsharp:	ldx ,x			; we can now get the duration
		lbsr ay8910shifter	; shift to the right octave
		lbsr ay8910playnote
		bra playtuneloop		
playtunedelay:	ldx #0xffff
		lbsr aydelay
		bra playtuneloop
playtuneoctave: lda ,y+
		suba #0x30
		sta ayoctave
		bra playtuneloop
playtuneshape:	lda ,y+
		suba #0x30
		ldx #shapetable
		lda a,x
		sta ayshape
		bra playtuneloop
playtunesharp:	lda #1
		sta aysharp
		bra noclrsharploop
playtuneout:	rts

aydelay:	leax -1,x
		bne aydelay
		rts

readjoystick:	lda #AYIOA
		sta AYLATCHADDR
		lda AYREADADDR
		coma
		rts

		section _main

; AY-8910

start:    	fcb $07,$38,$08,$18
		fcb $ff,$ff

stop:		fcb $07,$ff,$08,$00
		fcb $ff,$ff

envelope:	fcb $0b,$00,$0c,$08	 	; envelope
		fcb $ff,$ff

shapetable:	fcb $00,$04,$0b,$0d
		fcb $09,$0c,$0e,$0a

notetable:	fcc 'c'
		fcb $07,$77
		fcb $80
		fcb $07,$0e
		fcc 'd'
		fcb $06,$a6
		fcb $80
		fcb $06,$46
		fcc 'e'
		fcb $05,$ec
		fcc 'f'
		fcb $05,$96
		fcb $80
		fcb $05,$48
		fcc 'g'
		fcb $04,$fb
		fcb $80
		fcb $04,$b4
		fcc 'a'
		fcb $04,$70
		fcb $80
		fcb $04,$30
		fcc 'b'
		fcb $03,$f4
		fcb $80
		fcb $03,$bb
		fcb 0

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
		cmpa #$ff
		beq ay8910stout
		sta AYLATCHADDR
		stb AYWRITEADDR
		bra ay8910streamer
ay8910stout:  	rts

ay8910playnote:	pshs x
		lbsr ay8910start
		puls x
		lda #$0d
		sta AYLATCHADDR
		ldb ayshape
		stb AYWRITEADDR
		tfr x,d
		lda #$00
		sta AYLATCHADDR
		stb AYWRITEADDR
		tfr x,d
		ldb #$01
		stb AYLATCHADDR
		sta AYWRITEADDR
		ldx #$ffff
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
		cmpa #$00
		beq playtuneout
		cmpa #$20
		beq playtunedelay
		cmpa #$6f
		beq playtuneoctave
		cmpa #$77
		beq playtuneshape
		cmpa #$23
		beq playtunesharp
		ldx #notetable
notescanloop:	cmpa ,x
		beq notefoundlc
		tfr a,b
		adda #$20
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
playtunedelay:	ldx #$ffff
		lbsr aydelay
		bra playtuneloop
playtuneoctave: lda ,y+
		suba #$30
		sta ayoctave
		bra playtuneloop
playtuneshape:	lda ,y+
		suba #$30
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

		endsection

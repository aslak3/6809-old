		include 'hardware.inc'

		section _main

;;; SPI

spiinit:	lda #$01		; CPOL=0 and CPHA=1, which is
		sta SPICONTROL		; needed by the DS1305
		lda #$02		; div=2, which is E by 4
		sta SPIDIV
		lda #$ff		; "disable" the SS outputs
		sta SPISS
		rts

;;; SPI low level routines

spistart:	sta SPISS
		nop
		rts

spistop:	lda #$ff		; deassert chip select
		sta SPISS
		nop
		rts

; spiwrite - send the byte in b

spiwrite:	stb SPIDATAOUT
		
writeloop:	tst SPISTATUS
		bpl writeloop

		rts

; spiwriteblock - sends y bytes from x

spiwriteblock:	ldb ,x+
		stb SPIDATAOUT

writeblockloop:	tst SPISTATUS
		bpl writeblockloop

		leay -1,y
		bne spiwriteblock

		rts

; spiread - read the next spi byte into b

spiread:	clr SPIDATAOUT

readloop:	tst SPISTATUS
		bpl readloop

		ldb SPIDATAIN

		rts

; spireadblock - read y bytes into the buffer starting at x

spireadblock:	clr SPIDATAOUT

readblockloop:	tst SPISTATUS
		bpl readblockloop

		ldb SPIDATAIN
		stb ,x+

		leay -1,y
		bne spireadblock

		rts

		endsection

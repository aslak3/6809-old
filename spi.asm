;;; SPI

spiinit:	lda #0x01		; CPOL=0 and CPHA=1, which is
		sta SPICONTROL		; needed by the DS1305
		lda #0x02		; div=2, which is E by 4
		sta SPIDIV
		lda #0x00		; "disable" the SS outputs
		sta SPISS
		rts

;;; SPI low level routines

spistart:	lda #0x01
		sta SPISS
		nop

		rts

spistop:	clra			; deassert chip select
		sta SPISS
		nop

		rts

; spiwrite - send the byte in b

spiwrite:	stb SPIDATAOUT
		
writeloop:	lda SPISTATUS
		anda #0x80
		beq writeloop

		rts

; spiwriteblock - sends y bytes from x

spiwriteblock:	ldb ,x+
		stb SPIDATAOUT

writeblockloop:	lda SPISTATUS
		anda #0x80
		beq writeblockloop

		leay -1,y

		bne spiwriteblock

		rts

; spiread - read the next spi byte into b

spiread:	clr SPIDATAOUT

readloop:	lda SPISTATUS
		anda #0x80
		beq readloop

		ldb SPIDATAIN

		rts

; spireadblock - read y bytes into the buffer starting at x

spireadblock:	clr SPIDATAOUT

readblockloop:	lda SPISTATUS
		anda #0x80
		beq readblockloop

		ldb SPIDATAIN
		stb ,x+

		leay -1,y
		bne spireadblock

		rts


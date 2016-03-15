;;; SPI low level routines

spistart:	clrb			; clear clock and MOSI
		stb SPIOUT
		lda #0b01001111		; assert chip select on 1305
		sta SPISELECTS
		nop			; pause

		rts

spistop:	lda #0b00001011		; Disable all SPI selects
		sta SPISELECTS
		clrb
		stb SPIOUT		; clear clock and MOSI
		nop			; pause

		rts

; spiwrite - send the byte in b

spiwrite:

; bit 7

		clra			; start from nothing
		lslb			; move the bit to send into carry
		adca #0			; add carry so low bit is set for out
		sta SPIOUT		; output
		ora #SCLK		; assert the clock by oring it in
		sta SPIOUT		; output
		nop			; wait a bit
		eora #SCLK		; clear the clock
		sta SPIOUT
		nop			; output and wait

; bit 6

		clra
		lslb
		adca #0
		sta SPIOUT
		ora #SCLK
		sta SPIOUT
		nop
		eora #SCLK
		sta SPIOUT
		nop

; bit 5

		clra
		lslb
		adca #0
		sta SPIOUT
		ora #SCLK
		sta SPIOUT
		nop
		eora #SCLK
		sta SPIOUT
		nop

; bit 4

		clra
		lslb
		adca #0
		sta SPIOUT
		ora #SCLK
		sta SPIOUT
		nop
		eora #SCLK
		sta SPIOUT
		nop

; bit 3

		clra
		lslb
		adca #0
		sta SPIOUT
		ora #SCLK
		sta SPIOUT
		nop
		eora #SCLK
		sta SPIOUT
		nop

; bit 2

		clra
		lslb
		adca #0
		sta SPIOUT
		ora #SCLK
		sta SPIOUT
		nop
		eora #SCLK
		sta SPIOUT
		nop

; bit 1

		clra
		lslb
		adca #0
		sta SPIOUT
		ora #SCLK
		sta SPIOUT
		nop
		eora #SCLK
		sta SPIOUT
		nop

; bit 0

		clra
		lslb
		adca #0
		sta SPIOUT
		ora #SCLK
		sta SPIOUT
		nop
		eora #SCLK
		sta SPIOUT
		nop

		rts

spiread:	clrb

; bit 7

		lda #SCLK
		sta SPIOUT
		nop
		lda SPIIN
		rora
		rolb
		clra
		sta SPIOUT
		nop

; bit 6

		lda #SCLK
		sta SPIOUT
		nop
		lda SPIIN
		rora
		rolb
		clra
		sta SPIOUT
		nop

; bit 5

		lda #SCLK
		sta SPIOUT
		nop
		lda SPIIN
		rora
		rolb
		clra
		sta SPIOUT
		nop

; bit 4

		lda #SCLK
		sta SPIOUT
		nop
		lda SPIIN
		rora
		rolb
		clra
		sta SPIOUT
		nop

; bit 3

		lda #SCLK
		sta SPIOUT
		nop
		lda SPIIN
		rora
		rolb
		clra
		sta SPIOUT
		nop

; bit 2

		lda #SCLK
		sta SPIOUT
		nop
		lda SPIIN
		rora
		rolb
		clra
		sta SPIOUT
		nop

; bit 1

		lda #SCLK
		sta SPIOUT
		nop
		lda SPIIN
		rora
		rolb
		clra
		sta SPIOUT
		nop

; bit 0

		lda #SCLK
		sta SPIOUT
		nop
		lda SPIIN
		rora
		rolb
		clra
		sta SPIOUT
		nop

		rts

; spiwriteblock - sends y bytes from x

spiwriteblock:	ldb ,x+
		lbsr spiwrite
		leay -1,y
		bne spiwriteblock

		rts

; spireadblock - read y bytes into the buffer starting at x

spireadblock:	lbsr spiread
		stb ,x+
		leay -1,y
		bne spireadblock

		rts


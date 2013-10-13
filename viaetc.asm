;;; VIA

viainit:	clra
		sta PORTA6522
		ora #SS
		ora #SCLK
		ora #MOSI
		sta DDRA6522
		rts

;;; SPI low level routines

spistart:	lda #SS			; assert chip select on 1305
		sta PORTA6522
		nop			; wait for read

		rts

spistop:	clra			; deassert chip select
		sta PORTA6522
		nop			; pause

		rts

; spiwrite - send the byte in b

spiwrite:

; bit 7

		lda #SS			; ensure chip select
		lslb			; move the bit to send into carry
		adca #0			; add carry so low bit is set for out
		sta PORTA6522		; output
		ora #SCLK		; assert the clock by oring it in
		sta PORTA6522		; output
		nop			; wait a bit
		eora #SCLK		; clear the clock
		sta PORTA6522
		nop			; output and wait

; bit 6

		lda #SS
		lslb
		adca #0
		sta PORTA6522
		ora #SCLK
		sta PORTA6522
		nop
		eora #SCLK
		sta PORTA6522
		nop

; bit 5

		lda #SS
		lslb
		adca #0
		sta PORTA6522
		ora #SCLK
		sta PORTA6522
		nop
		eora #SCLK
		sta PORTA6522
		nop

; bit 4

		lda #SS
		lslb
		adca #0
		sta PORTA6522
		ora #SCLK
		sta PORTA6522
		nop
		eora #SCLK
		sta PORTA6522
		nop

; bit 3

		lda #SS
		lslb
		adca #0
		sta PORTA6522
		ora #SCLK
		sta PORTA6522
		nop
		eora #SCLK
		sta PORTA6522
		nop

; bit 2

		lda #SS
		lslb
		adca #0
		sta PORTA6522
		ora #SCLK
		sta PORTA6522
		nop
		eora #SCLK
		sta PORTA6522
		nop

; bit 1

		lda #SS
		lslb
		adca #0
		sta PORTA6522
		ora #SCLK
		sta PORTA6522
		nop
		eora #SCLK
		sta PORTA6522
		nop

; bit 0

		lda #SS
		lslb
		adca #0
		sta PORTA6522
		ora #SCLK
		sta PORTA6522
		nop
		eora #SCLK
		sta PORTA6522
		nop

		rts

spiread:	clrb

; bit 7

		lda #SS
		ora #SCLK
		sta PORTA6522
		nop
		lda PORTA6522
		rola
		rolb
		lda #SS
		sta PORTA6522
		nop

; bit 6

		lda #SS
		ora #SCLK
		sta PORTA6522
		nop
		lda PORTA6522
		rola
		rolb
		lda #SS
		sta PORTA6522
		nop

; bit 5

		lda #SS
		ora #SCLK
		sta PORTA6522
		nop
		lda PORTA6522
		rola
		rolb
		lda #SS
		sta PORTA6522
		nop

; bit 4

		lda #SS
		ora #SCLK
		sta PORTA6522
		nop
		lda PORTA6522
		rola
		rolb
		lda #SS
		sta PORTA6522
		nop

; bit 3

		lda #SS
		ora #SCLK
		sta PORTA6522
		nop
		lda PORTA6522
		rola
		rolb
		lda #SS
		sta PORTA6522
		nop

; bit 2

		lda #SS
		ora #SCLK
		sta PORTA6522
		nop
		lda PORTA6522
		rola
		rolb
		lda #SS
		sta PORTA6522
		nop

; bit 1

		lda #SS
		ora #SCLK
		sta PORTA6522
		nop
		lda PORTA6522
		rola
		rolb
		lda #SS
		sta PORTA6522
		nop

; bit 0

		lda #SS
		ora #SCLK
		sta PORTA6522
		nop
		lda PORTA6522
		rola
		rolb
		lda #SS
		sta PORTA6522
		nop

		rts

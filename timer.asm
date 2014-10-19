;;; TIMER ;;;

timerinit:	lda #0b01110000		; tick on the 3.684Mhz crystal / 16
		sta ACR88681
		lda #0b00001000		; mask in the timer overflow
		sta IMR88681
		lda #0x11
		sta CTU88681
		lda #0xfd
		sta CTL88681		; 4605 decimal - 25/sec
		lda STARTCT88681	; update the timer overflow

		; interrupt register

		lda FIRQFILTER		; get current value
		ora #IRQ88C681		; or in the duart line
		sta FIRQFILTER		; for the duart timer interrupt

		rts

; uptime counter - increment 32 bit counter

timerhandler:	pshs a,x
		lda STOPCT88681		; clear interrupt
		ldx uptimel		; get current lowword uptime
		leax 1,x		; add 1
		stx uptimel		; store it back
		bne timerhandlero	; if not 0 then done
		ldx uptimeh		; otherwise low current highword
		leax 1,x		; add 1
		stx uptimeh		; store it back
timerhandlero:	puls a,x
		rts

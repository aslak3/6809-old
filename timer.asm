;;; TIMER ;;;

timerinit:	lda #0b01110000		; tick on the 3.684Mhz crystal / 16
		sta ACR88681
		lda #0b00001000		; mask in the timer overflow
		sta IMR88681
		lda #0x01
		sta CTU88681
		lda #0xcc
		sta CTL88681		; 4605 decimal - 25/sec
		lda STARTCT88681	; update the timer overflow

		; interrupt handler
		ldx #timerhandler
		stx handle88c681

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
		bne uptimeo		; if not 0 then done
		ldx uptimeh		; otherwise low current highword
		leax 1,x		; add 1
		stx uptimeh		; store it back

uptimeo:	ldx timer
		beq timerdone
		leax -1,x
		stx timer

timerdone:	puls a,x
		rts

timerwait:	stx timer
timerwaitloop:	ldx timer
		bne timerwaitloop
		rts

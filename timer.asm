;;; TIMER ;;;

timerinit:	clra			; clear uptime
		clrb
		std uptimel
		std uptimeh

		lda #0b11000000
		sta IER6522		; enable T1 interrupts
        
		lda #0b01000000
		sta ACR6522		; T1 continuous, PB7 disabled

		; 250th of a second?
		lda #0x40
		sta T1CL6522
		lda #0x1f
		sta T1CH6522

		; interrupt handler
		ldx #timerhandler
		stx handle6522

		; interrupt register
		lda #INT6522
		sta IRQSOURCESS		; for the timer interrupt

		rts

; uptime counter - increment 32 bit counter

timerhandler:	pshs a,x
		lda T1CL6522		; clear interrupt
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

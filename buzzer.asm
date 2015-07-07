;;; SOUNDER ;;;

buzzerinit:	clr BUZZERTONE		; clear the buzzer frequency...
		clr BUZZERDURATION	; and duration registers
		clr buzzerpointer	; clear the "next note" pointer
		clr buzzerpointer+1

		ldx #buzzerhandler	; get the handler address
		stx handlebuzzer	; and save it in the vector table

		; interrupt register
		lda IRQFILTER		; get current value
		ora #IRQBUZZER		; or in the buzzer irq line
		sta IRQFILTER		; for the buzzer interrupt

		rts

; uptime counter - increment 32 bit counter

buzzerhandler:	pshs a,b,x		; save our registers!
		ldx buzzerpointer	; load the current pointer
		lda ,x+			; get the period
		sta BUZZERTONE		; save it in the register
		lda ,x+			; get the length
		sta BUZZERDURATION	; save it in the register
		; a zero here will cause no no next to play
		stx buzzerpointer	; save the moved along pointer
buzzerhandlero: puls a,b,x		; restore the registers
		rts

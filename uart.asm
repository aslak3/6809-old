; top level uart stuff

uartinit:	; uart interrupt handler
		ldx #uarthandler
		stx handleuart

		; disable interrupts on all ports
		clra
		sta IERPA16C654
		sta IERPB16C654
		sta IERPC16C654
		sta IERPD16C654

		; uart interrupt routing
		lda #INTUART
		sta IRQSOURCESS

		rts

; uarthandler - check the status on each port register and call to the
; registered handler

uarthandler:	lda ISRPA16C654
		bita #0x01
		bne doneporta
		jsr [handleuartpa]
doneporta:	lda ISRPB16C654
		bita #0x01
		bne doneportb
		jsr [handleuartpb]
doneportb:	lda ISRPC16C654
		bita #0x01
		bne doneportc
		jsr [handleuartpc]
doneportc:	lda ISRPD16C654
		bita #0x01
		bne doneportd
		jsr [handleuartpd]
doneportd:	rts

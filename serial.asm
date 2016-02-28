;;; SERIAL PORT ;;;

; serial port setup - this needs more comments

serialinit:	lda #0b10000011
		sta LCRPA16C654		; 8n1 and config baud
		lda #0x30
		sta DLLPA16C654
		lda #0x00
		sta DLMPA16C654		; 9600
		lda #0b00000011
		sta LCRPA16C654		; 8n1 and back to normal

		rts

; put the char in a, returning when its sent - corrupts b

serialputchar:	ldb LSRPA16C654		; get status
		andb #0b00100000	; transmit empty
		beq serialputchar	; wait for port to be idle
		sta THRPA16C654		; output the char
		rts

; serialgetchar - gets a char, putting it in a

serialgetchar:	lda LSRPA16C654		; get status
		anda #0b0000001		; input empty?
		beq serialgetchar	; go back and look again
		lda RHRPA16C654		; get the char into a
serialgetcharo:	rts

; serialgetwto - same as above but with a c. 2 sec timeout

serialgetwto:	pshs x
		ldx #0xffff
timeoutloop:	lda LSRPA16C654
		anda #0b00000001
		beq notready
		lda RHRPA16C654
		clrb			; no timeout occured
		bra serialgettwoo
notready:	leax -1,x
		bne timeoutloop
		ldb #1			; timeout occured
serialgettwoo:	puls x
		rts

; make the serial port the current active io device

serialactive:	ldx #serialgetchar
		stx iogetcharp
		ldx #serialputchar
		stx ioputcharp
		ldx #serialgetwto
		stx iogetwtop
		rts

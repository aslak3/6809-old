		section _main

;;; SERIAL PORT ;;;

; serial port setup - this needs more comments

serialinit:	lda #%00010011		; no parity, 8 bits/char - MR1A,B
		sta MRA88681
		sta MRB88681
		lda #%00000111		; 1.000 stop bits - MR2A,B
		sta MRA88681
		sta MRB88681
		lda #%00000101		; enable tx and rx
		sta CRA88681
		lda #%10000000		; extend on rx
		sta CRA88681
		lda #%10100000		; extend on tx
		sta CRA88681
		lda #%10001000		; 115.2K
		sta CSRA88681

		; OP0 and OP1 - turn off the led

		lda #0xff
		sta OPCR88681		; set manual control for OP0,1
		sta OPRSET88681		; set port to 0, which means high :/

		rts

; put the char in a, returning when its sent - corrupts b

serialputchar:	ldb SRA88681		; get status
		andb #%00000100	; transmit empty
		beq serialputchar	; wait for port to be idle
		sta THRA88681		; output the char
		rts

; serialgetchar - gets a char, putting it in a

serialgetchar:	lda SRA88681		; get status
		anda #%00000001	; input empty?
		beq serialgetchar	; go back and look again
		lda RHRA88681		; get the char into a
serialgetcharo:	rts

; serialgetwto - same as above but with a c. 2 sec timeout

serialgetwto:	pshs x
		ldx #0xffff
timeoutloop:	lda SRA88681
		anda #%00000001
		beq notready
		lda RHRA88681
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

		endsection		
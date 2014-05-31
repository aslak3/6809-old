;;; SERIAL PORT ;;;

; serial port setup - this needs more comments

serialinit:	lda #0b00010011		; no parity, 8 bits/char - MR1A,B
		sta MRA88681
		sta MRB88681
		lda #0b00000111		; 1.000 stop bits - MR2A,B
		sta MRA88681
		sta MRB88681
		lda #0b00000101		; enable tx and rx
		sta CRA88681
		lda #0b10000000		; extend on rx
		sta CRA88681
		lda #0b10100000		; extend on tx
		sta CRA88681
		lda #0b10001000		; 115.2K
		sta CSRA88681

		; OP0 and OP1 - turn off the led

		lda #0xff
		sta OPCR88681		; set manual control for OP0,1
		sta OPRSET88681		; set port to 0, which means high :/

		; 25 timer ticks per second		

		lda #0b01110000		; tick on the 3.684Mhz crystal / 16
		sta ACR88681
		lda #0b00001000		; mask in the timer overflow
		sta IMR88681
		lda #0x11
		sta CTU88681
		lda #0xfd
		sta CTL88681		; 4605 decimal - 25/sec
		lda STARTCT88681	; update the timer overflow

		rts

; put the char in a, returning when its sent - corrupts b

serialputchar:	ldb SRA88681		; get status
		andb #0b00000100	; transmit empty
		beq serialputchar	; wait for port to be idle
		sta THRA88681		; output the char
		rts

; puts the null terminated string pointed to by x

serialputstr:	lda ,x+			; get the next char
		beq serialputstro	; null found, bomb out
		bsr serialputchar	; output the character
		bra serialputstr	; more chars
serialputstro:	rts

; serialgetchar - gets a char, putting it in a

serialgetchar:	lda SRA88681		; get status
		anda #0b00000001	; input empty?
		beq serialgetchar	; go back and look again
		lda RHRA88681		; get the char into a
serialgetcharo:	rts
		
; serialgetstr - gets a line, upto a cr, filling x as we go

serialgetstr:	clr inputcount		; set the length to 0
serialgetstr2:	bsr serialgetchar	; get a char in a
		cmpa #CR		; cr?
		beq serialgetstro	; if it is, then out
		cmpa #LF		; lf?
		beq serialgetstro	; if it is, then out
		cmpa #BS		; backspace pressed?
		beq serialbs		; handle backspace
		cmpa #SP		; check if less then space
		blo serialgetstr2	; if so, then ignore it
		sta ,x+			; add it to string
		inc inputcount		; increment the number of chars
serialecho:	bsr serialputchar	; echo it
		bra serialgetstr2	; get more
serialgetstro:	clr ,x+			; add a null
		rts
serialbs:	tst inputcount		; see if the char count is 0
		beq serialgetstr2	; do nothing if already zero
		dec inputcount		; reduce count by 1
		clr ,x			; null the current char
		leax -1,x		; move the pointer back 1
		lda #BS			; move cursor back one
		bsr serialputchar
		lda #SP			; then erase and move forward
		bsr serialputchar
		lda #BS			; then back one again
		bsr serialputchar
		bra serialgetstr2	; echo the bs and charry on

; serialgetbyte - get a byte in ascii hex and load it in a

serialgetbyte:	ldx #inputbuffer	; reset input buffer
		lbsr serialgetstr	; get a line
		ldx #newlinemsg		; echo a newline...
		lbsr serialputstr	; ...to clean up the screen
		ldx #inputbuffer	; reset input buffer to what we got
		lbsr aschextobyte	; turn it into a byte in a
		rts

; serialputlab - outputs x, followed by y dereferenced  converted to a word,
; and a newline. useful for doing things like - File size: 1234

serialputlab:	lbsr serialputstr	; outputs whats in x (hope no newline)
		ldd ,y			; deref y into d
		ldx #outputbuffer	; reset output buffer
		lbsr wordtoaschex	; convert d to a word
		ldy #newlinemsg		; ...
		lbsr concatstr		; add a newline
		clr ,x+			; and a null
		ldx #outputbuffer	; reset output buffer again
		lbsr serialputstr	; and output [y] with a newline
		rts


;;; SERIAL PORT ;;;

; serial port setup

serialinit:	lda #0b00000011		; master reset
		sta SERIALCR
		; divider (=16), databits (=8n1), no rts and no interrupts
		lda #0b00010101
		sta SERIALCR
		rts

; put the char in a, returning when its sent - corrupts b

serialputchar:	ldb SERIALSR
		andb #0b00000010	; transmit empty
		beq serialputchar	; wait for port to be idle
		sta SERIALTX		; output the char
		rts

; puts the null terminated string pointed to by x

serialputstr:	lda ,x+			; get the next char
		beq serialputstro	; null found, bomb out
		bsr serialputchar	; output the character
		bra serialputstr	; more chars
serialputstro:	rts

; serialgetchar - gets a char, putting it in a

serialgetchar:	lda SERIALSR		; get status
		anda #0b00000001	; input empty?
		beq serialgetchar	; go back and look again
		lda SERIALRX		; get the char into a
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
		sta ,x+			; add it to string
		inc inputcount		; increment the number of chars
serialecho:	bsr serialputchar	; echo it
		bra serialgetstr2	; get more
serialgetstro:	clr ,x+			; add a null
		rts
serialbs:	tst inputcount		; see if the char count is 0
		bne serialgetstr2	; do nothing if already zero
		dec inputcount		; reduce count by 1
		clr ,x			; null the current char
		leax -1,x		; move the pointer back 1
		bra serialecho		; echo the bs and charry on

; serialgetbyte

serialgetbyte:	ldx #inputbuffer
		lbsr serialgetstr
		ldx #newlinemsg
		lbsr serialputstr
		ldx #inputbuffer
		lbsr aschextobyte
		rts

serialputlab:	lbsr serialputstr
		ldd ,y
		ldx #outputbuffer
		lbsr wordtoaschex
		ldy #newlinemsg
		lbsr concatstr
		clr ,x+
		ldx #outputbuffer
		lbsr serialputstr
		rts


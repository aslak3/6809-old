;;; IO ;;;

; put the char in a

ioputchar:	jmp [ioputcharp]

; iogetchar - gets a char, putting it in a

iogetchar:	jmp [iogetcharp]

; iogetwto - gets a char with a 2 sec(or so) timeout

iogetwto:	jmp [iogetwtop]

; puts the null terminated string pointed to by x

ioputstr:	lda ,x+			; get the next char
		beq ioputstro		; null found, bomb out
		bsr ioputchar		; output the character
		bra ioputstr		; more chars
ioputstro:	rts

; iogetstr - gets a line, upto a cr, filling x as we go

iogetstr:	clr inputcount		; set the length to 0
iogetstr2:	bsr iogetchar		; get a char in a
		cmpa #CR		; cr?
		beq iogetstro		; if it is, then out
		cmpa #LF		; lf?
		beq iogetstro		; if it is, then out
		cmpa #BS		; backspace pressed?
		beq iobs		; handle backspace
		cmpa #SP		; check if less then space
		blo iogetstr2		; if so, then ignore it
		sta ,x+			; add it to string
		inc inputcount		; increment the number of chars
ioecho:	bsr ioputchar			; echo it
		bra iogetstr2		; get more
iogetstro:	clr ,x+			; add a null
		rts
iobs:		tst inputcount		; see if the char count is 0
		beq iogetstr2		; do nothing if already zero
		dec inputcount		; reduce count by 1
		clr ,x			; null the current char
		leax -1,x		; move the pointer back 1
		lda #BS			; move cursor back one
		bsr ioputchar
		lda #SP			; then erase and move forward
		bsr ioputchar
		lda #BS			; then back one again
		bsr ioputchar
		bra iogetstr2		; echo the bs and charry on

; iogetbyte - get a byte in ascii hex and load it in a

iogetbyte:	ldx #inputbuffer	; reset input buffer
		lbsr iogetstr		; get a line
		ldx #newlinemsg		; echo a newline...
		lbsr ioputstr		; ...to clean up the screen
		ldx #inputbuffer	; reset input buffer to what we got
		lbsr aschextobyte	; turn it into a byte in a
		rts

; ioputlab - outputs x, followed by y dereferenced  converted to a word,
; and a newline. useful for doing things like - File size: 1234

ioputlab:	lbsr ioputstr		; outputs whats in x (hope no newline)
		ldd ,y			; deref y into d
		ldx #outputbuffer	; reset output buffer
		lbsr wordtoaschex	; convert d to a word
		ldy #newlinemsg		; ...
		lbsr concatstr		; add a newline
		clr ,x+			; and a null
		ldx #outputbuffer	; reset output buffer again
		lbsr ioputstr		; and output [y] with a newline
		rts

ionull:		rts

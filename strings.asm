;;; STRINGS ;;;

; aschextonib - convert a char on x to a low nibble in a

aschextonib:	lda ,x+			; get the charater
		suba #0x30		; subtract '0'
		cmpa #0x10		; less then 9?
		blo aschextonibout	; yes? we are done
		suba #0x07		; no? subtract 'A'-'9'
		cmpa #0x10		; less then 16?
		blo aschextonibout	; was uppercase
		suba #0x20
aschextonibout:	rts

; aschextobyte - convert two characters on x to a byte in a

aschextobyte:	leas -1,s		; make room on stack for 1 temp byte
		bsr aschextonib		; convert the low nibble
		lsla			; make it the high nibble
		lsla			; ..
		lsla			; ..
		lsla			; ..
		sta ,s			; save it on the stack
		bsr aschextonib		; convert the real low nibble
		ora ,s			; combine it with the high nibble
		leas 1,s		; shrink the stack again
		rts

; aschextowrd - convert four characters on x to a word in d

aschextoword:	bsr aschextobyte	; convert the first byte in the string
		tfr a,b			; move it asside
		bsr aschextobyte	; convert the second byte
		exg a,b			; swap the bytes
		rts			; d is now the word

; parse a stream of ascii hex into the u stack

parseinput:	ldx #inputbuffer+1	; point at the inputbuffer
		ldy #parambuffer	; setup parser location
nextparseinput:	lda ,x			; check for an initiial null
		beq parseinputout	; null? out we go
		bsr skipspaces		; skip spaces
		lda ,x			; get the char under the new pos
		cmpa #0x22		; double quote
		beq parsestring		; if so it is a string
		lda 2,x			; get the next but one char
		beq parsebyte		; null? it's a byte
		cmpa #0x20		; space
		beq parsebyte		; yes? it's a byte
		lda 4,x			; get the next but 3 char
		beq parseword		; null? it's a word
		cmpa #0x20		; space
		beq parseword		; yes? it's a word
		bra parseinputout	; no match, so end
parsebyte:	lda #1			; code 1 for bytes
		sta ,y+			; add it into the stream
		bsr aschextobyte	; yes? this pair must be a byte
		sta ,y+			; save it in u
		bra nextparseinput	; look for more
parseword:	lda #2			; code 2 for words
		sta ,y+			; add it in to the stream
		bsr aschextoword	; if we get here it must be a word
		std ,y++		; save the word in u
		bra nextparseinput
parsestring:	lda #3			; type 3 for strings
		sta ,y+			; save the type
		leax 1,x		; move to after the quote
stringloop:	lda ,x+			; get the string data
		beq parsestringout	; end of the string (bad though)
		cmpa #0x22		; closing quote
		beq parsestringout	; yes? end of the string
		sta ,y+			; add it in
		bne stringloop		; check for nulls too
parsestringout:	clr ,y+			; finish the string
		bra nextparseinput	; back for more elements
parseinputout:	clr ,y+			; null ender
		ldy #parambuffer	; reset back for the caller
		rts

; nibtoaschex - convert a low nibble in a to a character in x, advancing it

nibtoaschex:	anda #0x0f		; mask out the high nibble
		adda #0x30		; add '0'
		cmpa #0x39		; see if we are past '9'
		ble nibtoaschexout	; no? number then, so we're done
		adda #0x07		; yes? letter then, add 'A'-'9'
nibtoaschexout:	sta ,x+			; add it to the string
		rts		

; bytetoaschex - convert a byte in a to two characters in x, advancing it
		
bytetoaschex:	pshs a			; save original input byte
		lsra			; move high nibble into low nibble
		lsra			; ..
		lsra			; ..
		lsra			; ..
		bsr nibtoaschex		; convert the low nibble
		puls a			; get the original input back
		bsr nibtoaschex		; convert the high nibble
		clr ,x
		rts

; wordtoaschex - convert a word in d to four characters in x, advancing it

wordtoaschex:	pshs b			; save low byte
		bsr bytetoaschex	; output high byte
		puls a			; restore low byte
		bsr bytetoaschex	; and output low byte
		clr ,x
		rts

; jump x across spaces

skipspaces:	lda ,x+			; skip a space
		cmpa #0x20		; is it a space?
		beq skipspaces		; yes? then go back and look for more
		leax -1,x		; back 1
		rts

; printableasc - converts non printables to . in a

printableasc:	cmpa #0x20		; compare with space
		blo printabletodot	; lower? it must be a unprintable
		cmpa #0x7e		; compare with the end char
		bhi printabletodot	; higher? it must be unprintable
		rts			; if not, leave it alone
printabletodot:	lda #0x2e		; otherwise flatten it to a dot
		rts

; concatstr - add string y to string x, not copying the null

concatstr:	lda ,y+			; get the char in y
		beq concatstrout	; if its a null then finish
		sta ,x+			; otherwise add it to x
		bra concatstr		; go back for more
concatstrout:	rts

; concatstrn - add a chars of y to x, not ocpying the null

concatstrn:	ldb ,y+
		stb ,x+
		deca
		beq concatstrnout
		bra concatstrn
concatstrnout:	rts

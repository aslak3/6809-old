		.include 'user.inc'

; time

showtime:	lds #USERSTACKEND

		jsr jspistart		; mark with start

		clrb
		jsr jspiwrite		; write address 0

		leax timeoutput,pcr	; read into this buffer
		ldy #8			; we need 8 bytes

		jsr jspireadblock	; get y bytes into x

		jsr jspistop		; mark with stop

		leax outputbuffer,pcr	; setup output buffer
		leay timeoutput,pcr	; setup rtc data buffer

		lda 2,y			; get hours
		jsr jbytetoaschex	; render hex into x
		lda #0x3a		; ':'
		sta ,x+			; add the colon in
		lda 1,y			; get minutes
		jsr jbytetoaschex	; render hex into x
		lda #0x3a		; ':'
		sta ,x+			; add the colon in
		lda 0,y			; get seconds
		jsr jbytetoaschex	; render hex into x
		lda #0x20		; space char
		sta ,x+			; add the space in
		clra			; we need a null
		sta ,x+			; add it
		leax outputbuffer,pcr	; reset x for printing
		jsr jserialputstr	; print the time followed by space

		lda 3,y			; get days (1-7)
		deca			; make it into 0-6
		lsla			; multiply by 4 as each day is 4...
		lsla			; bytes including a null
		leax days,pcr		; setup pointer to day array
		leax a,x		; add on the offset from above
		jsr jserialputstr	; output the day

		leax outputbuffer,pcr	; back in the ram buffer
		lda #0x20		; space char
		sta ,x+			; add it so its after the day
		lda 4,y			; get date in month
		jsr jbytetoaschex	; turn it into hex in x
		lda #0x2f		; '/'
		sta ,x+			; add the slash
		lda 5,y			; get the month
		jsr jbytetoaschex	; turn it into hex in x
		lda #0x2f		; '/'
		sta ,x+			; add the slash
		lda 6,y			; get the year
		jsr jbytetoaschex	; turn it into hex in x
		clra			; we need a null
		sta ,x+			; add it
		leax outputbuffer,pcr	; reset output buffer
		jsr jserialputstr	; output the date ( DD/MM/YY)

		leax newlinemsg,pcr
		jsr jserialputstr	; and add a newline

		swi			; back to monitor

; days array - each day is 4 bytes long

days:		.asciz 'Sun'
		.asciz 'Mon'
		.asciz 'Tue'
		.asciz 'Wed'
		.asciz 'Thu'
		.asciz 'Fri'
		.asciz 'Sat'

newlinemsg:	.asciz '\r\n'
outputbuffer:	.rmb 16
timeoutput:	.rmb 16

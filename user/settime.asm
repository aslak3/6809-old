
		.include 'user.inc'

; set the time time

		lds #USERSTACKEND

		lda #0x11
		leax spibuffer,pcr
clear:		clr ,x+
		deca
		bne clear

		leay spibuffer,pcr
		lda #0x80		; msb set for writes
		sta ,y

		leax promptmsg,pcr
		jsr jserialputstr

		leax hourmsg,pcr
		jsr jserialputstr
		jsr jserialgetbyte
		sta 3,y

		leax minmsg,pcr
		jsr jserialputstr
		jsr jserialgetbyte
		sta 2,y

		leax secmsg,pcr
		jsr jserialputstr
		jsr jserialgetbyte
		sta 1,y

		leax daymsg,pcr
		jsr jserialputstr
		jsr jserialgetbyte
		sta 5,y

		leax monthmsg,pcr
		jsr jserialputstr
		jsr jserialgetbyte
		sta 6,y

		leax yearmsg,pcr
		jsr jserialputstr
		jsr jserialgetbyte
		sta 7,y

		leax dayofweekmsg,pcr
		jsr jserialputstr
		jsr jserialgetbyte
		sta 4,y

		jsr jspistart		; mark with start

		leax spibuffer,pcr
		lda #0x11
		sta bytecounter,pcr

settimenext:	ldb ,x+

		jsr jspiwrite		; write address 0

		lda bytecounter,pcr
		deca
		sta bytecounter,pcr

		bne settimenext

		jsr jspistop		; mark with stop

		swi			; back to monitor

; variables

bytecounter:	.rmb 1
spibuffer:	.rmb 0x11

; messages

promptmsg:	.asciz 'Enter the time and date:\r\n\r\n'
hourmsg:	.asciz 'Hour? (00-23) '
minmsg:		.asciz 'Minute? (00-59) '
secmsg:		.asciz 'Second? (00-59) '
daymsg:		.asciz 'Day? (01-31) '
monthmsg:	.asciz 'Month? (01-12) '
yearmsg:	.asciz 'Year? (00-99) '
dayofweekmsg:	.asciz 'Day number? (01=Sun-07=Sat) '

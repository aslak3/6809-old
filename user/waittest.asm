
		.include 'user.inc'

; set the time time

		lds #USERSTACKEND
		jsr jserialgetbyte

		swi			; back to monitor

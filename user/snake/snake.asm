; Make snake source

		.include 'user.inc'
		.include 'snake.inc'

		.area PROG (REL)

		.org 0

		lds #USERSTACKEND

		lbsr videoinit

;hop:		bra hop

		swi

		.include 'graphics.asm'
		.include 'video.asm'

; Main snake source

		.include 'user.inc'
		.include 'snake.inc'

		.area PROG (REL)

		.org 0			; dummy. we are using pic here

		ldu #USERSTACKEND	; setup the user stack

		lbsr videoinit

		lbsr game

		swi

		.include 'game.asm'
		.include 'graphics.asm'
		.include 'video.asm'

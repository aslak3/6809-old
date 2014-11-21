PROG = snake.bin
ASM = snake.asm

OTHER_ASM = snake.asm game.asm graphics.asm video.asm

all: $(PROG)

%.bin: %.ihx
	hex2bin -out $@ $<

%.ihx: %.rel
	aslink -nmwi $<
	
%.rel: $(ASM) $(OTHER_ASM) user.inc
	as6809 -oxs $<

clean:
	rm -f $(PROG) *.rel *.ihx *.map *.sym

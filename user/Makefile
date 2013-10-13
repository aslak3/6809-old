BINS = testprog.bin showtime.bin

all: $(BINS)

%.bin: %.ihx
	hex2bin -out $@ $<

%.ihx: %.rel
	aslink -nmwi $<
	
%.rel: %.asm
	as6809 -oxs $<

clean:
	rm -f $(BINS) *.rel *.ihx *.map *.sym

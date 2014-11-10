BINS = testprog.bin showtime.bin settime.bin type.bin showtemp.bin

all: $(BINS)

%.bin: %.ihx
	hex2bin -out $@ $<

%.ihx: %.rel
	aslink -nmwi $<
	
%.rel: %.asm user.inc
	as6809 -oxs $<

clean:
	rm -f $(BINS) *.rel *.ihx *.map *.sym

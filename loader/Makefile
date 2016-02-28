BIN = loader.bin
MAP = loader.map
INC = loader.inc
IHX = loader.ihx
MIF = loader.mif

LOADER_ASM = loader.asm

INCS = ../hardware.inc

all: $(MIF)

%.mif: %.bin
	./bin2mif.pl < $< > $@

%.bin: %.ihx
	hex2bin -out $@ $<

%.ihx: %.rel
	aslink -nmwi $@ $<
	
%.rel: $(INCS) $(LOADER_ASM)
	as6809 -oxs $(LOADER_ASM)

clean:
	rm -f $(BIN) $(INC) *.rel *.ihx *.map *.sym *.mif

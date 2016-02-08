UPLOAD = $(HOME)/8bitcomputer/eepromprogrammer/upload/upload

BINS = romonlytest.bin ramtest.bin serialtest.bin flasher.bin flasherram.bin

all: $(BINS)

%.bin: %.hex
	hex2bin -out $@ $<
	
%.hex: %.a
	a09 -X $<

clean:
	rm -f $(BINS) *.hex *.bin

upload:
	$(UPLOAD) -f $(BIN) -s /dev/ttyUSB0
		
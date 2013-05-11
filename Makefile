UPLOAD = $(HOME)/8bitcomputer/eepromprogrammer/upload/upload
FLASHER = ./flasher/flasher

PROG_SERIAL = /dev/ttyUSB0
6809_SERIAL = /dev/ttyUSB0

BINS = romonlytest.bin ramtest.bin serialtest.bin monitor.bin

all: $(BINS)

%.bin: %.hex
	hex2bin -out $@ $<
	
%.hex: %.a
	a09 -X $<

clean:
	rm -f $(BINS) *.hex *.bin

doupload:
	$(UPLOAD) -f $(BIN) -s $(PROG_SERIAL)

doflasher:
	$(FLASHER) -f $(BIN) -s $(6809_SERIAL)
	
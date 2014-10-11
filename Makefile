UPLOAD = $(HOME)/8bitcomputer/eepromprogrammer/upload/upload
FLASHER = ./flasher/flasher

PROG_SERIAL = /dev/ttyUSB1
6809_SERIAL = /dev/ttyUSB0

BIN = monitor.bin
MAP = monitor.map
INC = monitor.inc

MONITOR_ASM = monitor.asm
ASMS = jumptable.asm misc.asm ramvars.asm storage.asm spi.asm serial.asm \
	strings.asm ay8910.asm disassembly.asm font.asm v99.asm keyboard.asm
INCS = hardware.inc

all: $(BIN) $(INC)

%.bin: %.ihx
	hex2bin -out $@ $<

%.ihx: %.rel
	aslink -nmwi $<
	
%.rel: $(ASMS) $(INCS) $(MONITOR_ASM)
	as6809 -oxs $(MONITOR_ASM)

$(INC): $(MAP)
	./map2inc.pl < $(MAP) > $(INC)

clean:
	rm -f $(BIN) $(INC) *.rel *.ihx *.map *.sym

doupload:
	$(UPLOAD) -f $(BIN) -s $(PROG_SERIAL)

doflasher:
	$(FLASHER) -f $(BIN) -s $(6809_SERIAL)
	
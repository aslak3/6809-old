UPLOAD = $(HOME)/8bitcomputer/eepromprogrammer/upload/upload
FLASHER = ./flasher/flasher

PROG_SERIAL = /dev/ttyUSB1
6809_SERIAL = /dev/ttyUSB0

BIN = monitor.bin

ASMS = monitor.asm jumptable.asm misc.asm ramvars.asm storage.asm spi.asm serial.asm \
	strings.asm ay8910.asm disassembly.asm font.asm v99.asm keyboard.asm \
	timer.asm io.asm terminal.asm ramvars.asm
OBJS = monitor.o jumptable.o misc.o ramvars.o storage.o spi.o serial.o \
	strings.o ay8910.o disassembly.o font.o v99.o keyboard.o \
	timer.o io.o terminal.o ramvars.o

INCS = hardware.inc

all: $(BIN)

$(BIN): $(OBJS)
	lwlink --raw --output=$@ $(OBJS)

%.o: %.asm
	lwasm --obj --output=$@ --6809 --pragma=undefextern --pragma=export $<

clean:
	rm -f $(BIN) $(INC) *.rel *.ihx *.map *.sym *.o

doupload:
	$(UPLOAD) -f $(BIN) -s $(PROG_SERIAL)

doflasher:
	$(FLASHER) -f $(BIN) -s $(6809_SERIAL)
	
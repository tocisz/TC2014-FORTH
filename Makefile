ASSEMBLER = z80-unknown-coff-as
LINKER = z80-unknown-coff-ld
OBJCOPY = z80-unknown-coff-objcopy
PYTHON = python3

ASFLAGS = -a
LDFLAGS =

LD_FILES := $(wildcard *.ld)
OBJ_FILES := int32k.o forth.o

all: rom.bin ram.hex

rom.bin: rom.out
	$(OBJCOPY) -O binary -j.rom $< $@

ram.hex: ram.out
	$(OBJCOPY) --set-start=0 -O ihex -j.ram $< $@

rom.out: $(OBJ_FILES) $(LD_FILES)
	$(LINKER) $(LDFLAGS) -T rom.ld -Map=rom.map $(OBJ_FILES) -o $@

ram.out: $(OBJ_FILES) $(LD_FILES)
	$(LINKER) $(LDFLAGS) -T ram.ld -Map=ram.map $(OBJ_FILES) -o $@

%.o: %.asm
	$(ASSEMBLER) $(ASFLAGS) $< -o $@ > $<.lst

forth.asm: forth.py
	$(PYTHON) $< essential > $@

clean:
	rm -f forth.asm *.hex *.out *.o *.bin *.map *.lst

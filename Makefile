ASSEMBLER = z80-unknown-coff-as
LINKER = z80-unknown-coff-ld
OBJCOPY = z80-unknown-coff-objcopy
PYTHON = python3

ASFLAGS = -a
LDFLAGS =

LD_FILES := $(wildcard *.ld)
OBJ_FILES := forth.o const.o
OBJ_FILES_ROM := int32k.o $(OBJ_FILES)

all: rom.bin ram.hex

rom.bin: forth.rom
	$(OBJCOPY) -O binary -j.rom $< $@

ram.hex: forth.ram
	$(OBJCOPY) --set-start=0 -O ihex -j.ram $< $@

forth.rom: $(OBJ_FILES_ROM) $(LD_FILES)
	$(LINKER) $(LDFLAGS) -T rom.ld -Map=rom.map $(OBJ_FILES_ROM) -o $@

forth.ram: $(OBJ_FILES) $(LD_FILES)
	$(LINKER) $(LDFLAGS) -T ram.ld -Map=ram.map $(OBJ_FILES) -o $@

%.o: %.asm
	$(ASSEMBLER) $(ASFLAGS) $< -o $@ > $<.lst

forth.asm: forth.py
	$(PYTHON) $< > $@

clean:
	rm -f forth.asm forth.rom forth.ram *.hex *.o *.bin *.map *.lst

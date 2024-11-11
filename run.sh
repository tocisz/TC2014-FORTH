#!/bin/sh
dd bs=1024 count=32 if=/dev/zero of=F.bin
dd bs=1024 count=8  seek=0 if=rom.bin conv=notrunc of=F.bin
rc2014 -a -r F.bin  < Z80
#-d 16 2> debug.txt
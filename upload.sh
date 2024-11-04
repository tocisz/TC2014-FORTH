#!/bin/bash
z80cat() {
	tr "\n" "\r" | nc 192.168.0.46 23
}
curl -X POST http://192.168.0.46/console/reset
docker exec z80 bash -c "cd ~/RC2014-FORTH; make"
echo -n C | z80cat
sleep 0.2
echo 33792 | z80cat
sleep 0.2
echo hload | z80cat
cat ram.hex | z80cat
echo "?usr(0)" | z80cat

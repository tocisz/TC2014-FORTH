#!/bin/bash
curl -X POST http://192.168.0.46/console/reset
docker exec z80 bash -c "cd ~/RC2014-FORTH; make"
(
echo -n C
sleep 0.2
echo 33792
sleep 0.2
echo hload
cat ram.hex
echo "?usr(0)"
) | tr "\n" "\r" | nc 192.168.0.46 23
#!/usr/bin/env bash

export TIME="%e"

TFILE=time.txt
TCMD="/usr/bin/env time --append --output time.txt"

rm -f $TFILE

for i in $(seq 0 30); do
   $TCMD ./Asm < numbers.txt > out.txt
done

awk '{a+=$1} END{printf("%.2f\n", a/FNR)}' $TFILE
rm -f $TFILE

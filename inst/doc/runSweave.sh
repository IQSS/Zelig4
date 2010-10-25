#!/bin/sh

#
# run Sweave() in all of Rnw files
# 
#

for f in `ls *.Rnw`
do
    echo "Sweave(\"$f\")" | R --vanilla --slave
done

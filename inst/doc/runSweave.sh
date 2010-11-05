#!/bin/sh

#
# run Sweave() in all of Rnw files
# 
#
RCMD="/usr/bin/R"

for f in `ls *.Rnw`
do
  #echo "Sweave(\"$f\")" | R --vanilla --slave
  $RCMD CMD Sweave $f
done

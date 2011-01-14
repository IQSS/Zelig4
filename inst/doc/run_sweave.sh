#!/usr/bin/sh

R=/usr/bin/R

for f in `ls *.Rnw`
do
  ${R} CMD Sweave ${f}
done


if [ -d "Rplots.pdf" ]
then
  rm Rplots.pdf
fi

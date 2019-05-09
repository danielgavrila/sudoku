PROGNAME=sudoku
rm $PROGNAME
ghc  -O3 basic.hs -o $PROGNAME
./$PROGNAME  ../Inputs/easy01.txt
./$PROGNAME  ../Inputs/expert01.txt
./$PROGNAME  ../Inputs/expert02.txt
./$PROGNAME  ../Inputs/expert03.txt



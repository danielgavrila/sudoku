PROGNAME=sudoku
rm $PROGNAME
ghc  -O3 basic.hs -o $PROGNAME
./$PROGNAME  ../Inputs/easy01.txt
./$PROGNAME  ../Inputs/easy02.txt
./$PROGNAME  ../Inputs/easy03.txt
./$PROGNAME  ../Inputs/easy04.txt
./$PROGNAME  ../Inputs/medium01.txt
./$PROGNAME  ../Inputs/medium02.txt
./$PROGNAME  ../Inputs/medium03.txt
./$PROGNAME  ../Inputs/medium04.txt
./$PROGNAME  ../Inputs/expert01.txt
./$PROGNAME  ../Inputs/expert02.txt
./$PROGNAME  ../Inputs/expert03.txt
./$PROGNAME  ../Inputs/expert04.txt



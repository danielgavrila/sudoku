PROGNAME=sudoku
rm $PROGNAME
ghc  -O3 basic.hs -o $PROGNAME
./sudoku ../Inputs/expert01.txt

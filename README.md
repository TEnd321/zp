# User documentation

Program will take crossword puzzle in given file and a list of words and will try to arrange the words inside given puzzle, so that at the end, no free spaces will be left in puzzle.

## Format of input puzzle file

Input file for puzzle is made out of puzzle dimensions and puzzle matrix itself.

Dimensions are specified on the first row of the file as follows:

`ROWS COLS`

Where `ROWS` is a number of rows of the puzzle and `COLS` is the number of columns. These two are separated by space.

All following lines are description of walls inside a puzzle in a form of matrix.

Rows of the matrix describe rows of the puzzle, etc. Each character represents, wether there is a wall right next to it, or below it, or both. If there is no wall next to a character,
or below it, character `n` should be used.

If there is a wall only to its right, `r` should be used.

If there is a wall only below it, `d` should be used.

If there is a wall to its right and below it, any other character than `r`, `d` of `n` should be used.

For example:

```
3 3
dnn
nrd
nnn
```

Would be parsed into 3x3 puzzle like this:

```
+--+--+--+
|  :  :  |
+--+..+..+
|  :  |  |
+..+..+--+
|  :  :  |
+--+--+--+
```

This is for illustrational purposes only. Both `--` and `|` represents walls.

A puzzle like this would require one character long word in order to be properly solved. Since such words are mostly ommited from crosswords, a `-n` flag could be used in order to indicate, that spaces requiring one character long words will be ommited from solution.

## Format of input words file

Input file for words consist of one input word for each line. These words will be used during execution.

## Example inputs

### 1. Puzzle input file

```
5 6
nrnnnn
nrnnnn
nrnnnn
nnnnnn
nnnnnn
```

### 1. Words input file

```
oaklan
mlados
ut
utkal
lana
au
ii
autom
ii
pp
23
iana
e
vlika
e
laald
a
annao
a
qqqqq
tk
vlak
kaans
qqqq
```

### 1. Result

```
auvlak
utlana
tkiana
oaklan
mlados
```

### 2. Puzzle input file

```
3 3
nnn
nnn
nnn
```

### 2. Words input file

```
aaa
aaa
bbb
aab
aab
aan
```

### 2. Result

Nothing, since given words cant be filled into puzzle correctly, so there is no solution.

### 3. Puzzle input file

```
2 3
nrn
nrn
```

### 3. Words input file

```
aa
ab
ab
bb
cc
```

### 3. Result

```
abc
abc

aac
bbc
```

This example would not work, if the `-n` flag would not be set, since there are no words with length equal to one.
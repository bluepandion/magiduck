DEFINT A-Z

DIM array(100)
DIM sarray(100) AS STRING * 1
DIM p AS INTEGER
COMMON t AS DOUBLE

CLS

t = TIMER
FOR n = 1 TO 20000
 p = 0
 data1 = array(p)
 data2 = array(p + 1)
 data3 = array(p + 2)
 data4 = array(p + 3)
 data5 = array(p + 4)
 data6 = array(p + 5)
 data7 = array(p + 6)
NEXT n

PRINT TIMER - t

t = TIMER

DEF SEG = VARSEG(array(0))
p = VARPTR(array(0))
FOR n = 1 TO 20000
 data1 = PEEK(p)
 data2 = PEEK(p + 2)
 data3 = PEEK(p + 3)
 data4 = PEEK(p + 4)
 data5 = PEEK(p + 5)
 data6 = PEEK(p + 6)
 data7 = PEEK(p + 7)
NEXT n

PRINT TIMER - t


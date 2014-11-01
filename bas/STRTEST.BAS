DEFINT A-Z

CLS

DIM t AS DOUBLE

TYPE stringtype
 a AS STRING * 1
END TYPE

DIM str AS stringtype

str.a = "6"
a = 6
c = 4096

IF str.a > "5" THEN PRINT "over"

t = TIMER
FOR n = 1 TO 10000
 test = c \ 256
 IF test > 6 THEN test = 6
NEXT n
PRINT TIMER - t

t = TIMER
FOR n = 1 TO 10000
 IF a > 6 THEN a = 6
NEXT n
PRINT TIMER - t


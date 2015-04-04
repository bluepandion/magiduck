SCREEN 1

CONST PI = 3.141593

TYPE angleType
 X(0 TO 63) AS INTEGER
 Y(0 TO 63) AS INTEGER
END TYPE

DIM angle AS angleType

DIM sint AS INTEGER
DIM cost AS INTEGER

DIM n AS DOUBLE
DIM aAdd AS DOUBLE

tt = 0
b = 32
PSET (160, 100), 2
aAdd = (2 * PI / 65)

FOR a = 0 TO 31
 n = n + aAdd
 'IF a AND 1 THEN nAdd = nAdd + .05
 b = b + 1
 sint = INT(COS(n) * -20)
 cost = INT(SIN(n) * 10)
 X = 160 + cost
 Y = 150 + sint
 PSET (X, Y), 1
 angle.X(tt) = cost
 angle.Y(tt) = sint

 angle.X(tt + 32) = -cost
 angle.Y(tt + 32) = -sint

 tt = tt + 1
NEXT a

SCREEN 0
WIDTH 80, 25
FOR nn = 0 TO 63 STEP 16
 FOR n = nn TO nn + 15
  LOCATE , (n AND 15) * 4 + 1: PRINT angle.X(n);
 NEXT n
 PRINT
NEXT nn
PRINT
PRINT
FOR nn = 0 TO 63 STEP 16
 FOR n = nn TO nn + 15
  LOCATE , (n AND 15) * 4 + 1: PRINT angle.Y(n);
 NEXT n
 PRINT
NEXT nn


OPEN "sincos.def" FOR BINARY AS #1
  PUT #1, 1, angle
CLOSE #1


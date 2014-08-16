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

tt = 0
b = 32
PSET (160, 100), 2
FOR a = 0 TO 63
 n = n + (2 * PI / 64)
 b = b + 1
 sint = INT(COS(n) * -20)
 cost = INT(SIN(n) * 10)
 X = 160 + cost
 Y = 100 + sint
 PSET (X, Y), 1
 angle.X(tt) = cost
 angle.Y(tt) = sint
 tt = tt + 1
NEXT a

FOR n = 0 TO 63
 PRINT angle.X(n);
NEXT n
PRINT
FOR n = 0 TO 63
 PRINT angle.Y(n);
NEXT n


OPEN "sincos.def" FOR BINARY AS #1
  PUT #1, 1, angle
CLOSE #1


DEFINT A-Z

mask(0) = &HFF
mask(1) = &HFF00
DIM tileVal(0 TO 60, 0 TO 1) AS INTEGER
DIM tileMul(0 TO 79) AS INTEGER
DIM levelX(0 TO 79) AS INTEGER

a = 0
b = 0
FOR n = 0 TO 79
 levelX(n) = b
 tileMul(n) = a
 IF a THEN b = b + 1
 a = a XOR 1
NEXT n

DIM level(0 TO 19, 0 TO 39) AS INTEGER

t = 255
FOR y = 0 TO 79
 FOR x = 0 TO 19
  level(x, (levelX(y))) = t
 NEXT x
NEXT y


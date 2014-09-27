
DEFINT A-Z

COMMON t AS DOUBLE
COMMON i0 AS INTEGER
COMMON i1 AS INTEGER
COMMON i2 AS INTEGER
COMMON i3 AS INTEGER
COMMON i4 AS INTEGER

TYPE testType
 i0 AS INTEGER
 i1 AS INTEGER
 i2 AS INTEGER
 i3 AS INTEGER
 i4 AS INTEGER
END TYPE

DIM array(100) AS testType

an = 49

CLS

t = TIMER
 
FOR n = 0 TO 5000
 array(an).i0 = 1 + 5
 array(an).i1 = 2 + 5
 array(an).i2 = 3 + 5
 array(an).i3 = 3 + 5
 array(an).i4 = 4 + 5

 array(an).i0 = 1 + 5
 array(an).i1 = 2 + 5
 array(an).i2 = 3 + 5
 array(an).i3 = 3 + 5
 array(an).i4 = 4 + 5

 array(an).i0 = 1 + 5
 array(an).i1 = 2 + 5
 array(an).i2 = 3 + 5
 array(an).i3 = 3 + 5
 array(an).i4 = 4 + 5

 array(an).i0 = 1 + 5
 array(an).i1 = 2 + 5
 array(an).i2 = 3 + 5
 array(an).i3 = 3 + 5
 array(an).i4 = 4 + 5
NEXT n

PRINT TIMER - t

t = TIMER

FOR n = 0 TO 5000
 i0 = array(an).i0
 i1 = array(an).i1
 i2 = array(an).i2
 i3 = array(an).i3
 i4 = array(an).i4

 i0 = 1 + 5
 i1 = 2 + 5
 i2 = 3 + 5
 i3 = 4 + 5
 i4 = 5 + 5

 i0 = 1 + 5
 i1 = 2 + 5
 i2 = 3 + 5
 i3 = 4 + 5
 i4 = 5 + 5

 i0 = 1 + 5
 i1 = 2 + 5
 i2 = 3 + 5
 i3 = 4 + 5
 i4 = 5 + 5

 i0 = 1 + 5
 i1 = 2 + 5
 i2 = 3 + 5
 i3 = 4 + 5
 i4 = 5 + 5

 array(an).i0 = i0
 array(an).i1 = i1
 array(an).i2 = i2
 array(an).i3 = i3
 array(an).i4 = i4
NEXT n

PRINT TIMER - t


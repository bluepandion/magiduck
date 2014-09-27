'Converts 40-column text files to prerendered composite text with attributes.

'$INCLUDE: 'libelf.bi'
DEFINT A-Z

DECLARE SUB hudprint (x%, text$, colour1%, colour2%)
DECLARE SUB initLookUps ()

initLookUps

OPEN "ducktext.ctt" FOR BINARY AS #2

OPEN "..\convert\hudtext.txt" FOR INPUT AS #1
 DO
  INPUT #1, n
  IF n > -1 THEN
    INPUT #1, s$
    INPUT #1, c0
    INPUT #1, c1
    hudprint 0, s$, c0, c1
    PUT #2, n + 1, hudBuffer
    PRINT s$
   END IF
 LOOP WHILE n > -1
CLOSE #1

CLOSE #2


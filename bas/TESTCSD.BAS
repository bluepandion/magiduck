DEFINT A-Z
DIM n AS INTEGER

WIDTH 80, 50

CLS
mode = 3

OPEN "spenemy0.csd" FOR BINARY AS #1
 fileOfs = 1
 DO
  COLOR 15
  GET #1, fileOfs, n
  PRINT fileOfs, n
  fileOfs = fileOfs + 2
  IF n <> -1 THEN
    COLOR 9
    FOR c = 1 TO mode
     GET #1, fileOfs, n
     PRINT fileOfs, n
     fileOfs = fileOfs + 2
    NEXT c
   ELSE
    IF mode = 12 THEN CLOSE #1: END
    IF mode = 3 THEN mode = 6
  END IF
 DO
 LOOP WHILE INKEY$ = ""
 LOOP
CLOSE #1


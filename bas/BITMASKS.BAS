cMask(1) = &H1
cMask(2) = &H2
cMask(3) = &H4
cMask(4) = &H8
cMask(5) = &H10
cMask(6) = &H20
cMask(7) = &H40
cMask(8) = &H80

OPEN "debug.txt" FOR OUTPUT AS #1
   FOR n = 0 TO 255
      s$ = ""
      FOR c = 1 TO 8
        IF cMask(c) AND n THEN
         s$ = s$ + "F"
        ELSE
         s$ = s$ + "0"
        END IF
      NEXT c
      PRINT #1, s$;
   NEXT n
CLOSE #1


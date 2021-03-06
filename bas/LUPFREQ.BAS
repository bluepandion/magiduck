' Create sound engine frequency lookups and save them in binary format.
'
'


CLS

DIM note(0 TO 7, 1 TO 12)


OPEN "freqs.def" FOR BINARY AS #2
 fileOfs = 1
OPEN "freq.def" FOR INPUT AS #1
FOR n = 1 TO 12
 a$ = ""
  
  FOR o = 0 TO 7
  nn = o * 12 + n
  INPUT #1, v#
  freq# = 1193180# / v#
  loByte = INT(freq# AND 255#)
  hiByte = INT(freq# / 256#)
  IF hiByte > 255 THEN hiByte = 255
  PUT #2, fileOfs, hiByte
  PUT #2, fileOfs + 2, loByte
  fileOfs = fileOfs + 2
  PRINT HEX$(hiByte); HEX$(loByte); "  ";
 
  's$ = "0" + HEX$(loByte)
  'IF LEN(s$) < 3 THEN s$ = "0" + s$

  'a$ = a$ + s$ + "h, "
  's$ = "0" + HEX$(hiByte)
  'IF LEN(s$) < 3 THEN s$ = "0" + s$
  
  IF freq# > 65535 THEN freq# = 65535
  note(o, n) = INT(freq#)
  
 NEXT o
 
 PRINT
NEXT n
CLOSE #1

CLOSE #2

OPEN "debug.txt" FOR OUTPUT AS #1
 FOR o = 0 TO 7
  FOR n = 1 TO 12
   PRINT #1, note(o, n);
   IF n < 12 THEN PRINT #1, ",";
  NEXT n
  PRINT #1, ""
 NEXT o

CLOSE #1


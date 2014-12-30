CLS

OPEN "freqs.def" FOR BINARY AS #2
 fileOfs = 1
OPEN "freq.def" FOR INPUT AS #1
FOR n = 1 TO 12
 a$ = "DATA"
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
 NEXT o
 PRINT
NEXT n
CLOSE #1

CLOSE #2


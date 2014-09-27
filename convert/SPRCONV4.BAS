'Convert sprites to two pixels / byte + mask

infile$ = "ducks2.tga"

outfile$ = "ducks2"

alphacolor = 11


'---------------------------------------------------------------------------

alphac1 = alphacolor
alphac2 = alphacolor * 16

DIM SHARED spriteimport AS STRING
DIM SHARED conversion AS STRING * 6400
DIM SHARED getbyte AS STRING * 1

CLS
SCREEN 0

 PRINT "Converting "; file$; " ..."

COLOR 15

x = 0
y = 150
a = 67 + 80 * 9

DEF SEG = &HB800
vofs = 3998

OPEN infile$ FOR BINARY AS #1

spriteimport = ""
lineget$ = ""

FOR f = a TO LOF(1) - 26
 GET #1, f, getbyte
 c = 255 - ASC(getbyte)

 IF c > 15 THEN c = 15
 
 POKE vofs, 219
 POKE vofs + 1, c

 vofs = vofs - 2
 IF vofs < 0 THEN vofs = 3998

 lineget$ = lineget$ + CHR$(c * 16)
 x = x + 1
 IF x = 80 THEN
  x = 0
  spriteimport = lineget$ + spriteimport
  LOCATE 1, 1: PRINT lineget$
  lineget$ = ""
 END IF
NEXT f

CLOSE #1

'FOR a = 1 TO LEN(spriteimport)
'PRINT HEX$(ASC(MID$(spriteimport, a, 1)));
'
'DO: LOOP WHILE INKEY$ = ""
'NEXT a

PRINT "TGA load OK. Converting..."

'----------------------------------------------------------------------------
 
DEF SEG = SSEG(spriteimport)
ofs = SADD(spriteimport)
outposeven = 1
FOR y = 0 TO 79
 FOR x = 0 TO 78 STEP 2

  c = PEEK(ofs + 1)
  c2 = PEEK(ofs)
  c = c / 16

  mask = &HFF
  IF c <> alphac1 THEN mask = &HF0
  IF c2 <> alphac2 THEN mask = &HF
  IF c <> alphac1 AND c2 <> alphac2 THEN mask = 0

  IF c = alphac1 THEN c = 0
  IF c2 = alphac2 THEN c2 = 0

  MID$(conversion, outposeven, 1) = CHR$(c OR c2)
  MID$(conversion, outposeven + 1, 1) = CHR$(mask)

  ofs = ofs + 2
  outposeven = outposeven + 2
 NEXT x
NEXT y

'---------------------------------------------------------------------------

IF LEN(DIR$(outfile$ + ".csg")) THEN KILL outfile$ + ".csg"

OPEN outfile$ + ".csg" FOR BINARY AS #1
 PUT #1, 1, conversion
CLOSE #1

SHELL "copy " + outfile$ + ".csg ..\bas"

PRINT "Done. Press ESC to exit, Any other to preview."
DO
d$ = INKEY$
LOOP WHILE d$ = ""
IF d$ = CHR$(27) THEN END

'---------------------------------------------------------------------------
PRINT "Saving info."
OPEN "sprdebug.txt" FOR OUTPUT AS #1
FOR n = 1 TO LEN(conversion) - 79 STEP 80
 c = 1
 b$ = ""
 FOR a = n TO n + 79 STEP 2
  a$ = HEX$(ASC(MID$(conversion, a, 1)))
  IF LEN(a$) < 2 THEN a$ = "0" + a$
  b$ = b$ + a$
 NEXT a
 PRINT #1, b$
NEXT n
CLOSE #1
PRINT "See sprdebug.txt for full info."


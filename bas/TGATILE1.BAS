'16 color, indexed TGA load and conversion

'Converts to tilemap format (2 pixels per byte)

DIM tileBank AS STRING * 2000
tilebankseg = VARSEG(tileBank)
tilebankofs = VARPTR(tileBank)

inputfile$ = "..\convert\tileset1.tga"

outPutFile$ = "tileset1"

rawpixel$ = ""

DIM c AS INTEGER

SCREEN 0
'WIDTH 80, 43

COLOR 15
LOCATE 24, 1:
PRINT "Loading "; inputfile$


OPEN inputfile$ FOR BINARY AS #1

PRINT LOF(1); " Bytes"

x = 0
y = 150
a = 67 + 80 * 9
linemul = 16
lineget$ = ""

DEF SEG = &HB800

op = 0

FOR f = a TO LOF(1) - 26
 GET #1, a, c
 a = a + 1
 c = 255 - c
 c = c AND &HFF
 IF c > 15 THEN c = 15
 POKE (op), 219
 POKE (op + 1), c

 op = op + 2
 IF op > 3999 THEN op = 0

 lineget$ = lineget$ + CHR$(c * linemul)
 linemul = linemul + 15
 IF linemul > 17 THEN linemul = 1
 x = x + 1
 IF x = 80 THEN
  x = 0
  'y = y - 1
  rawpixel$ = lineget$ + rawpixel$
  lineget$ = ""
 END IF
NEXT f
CLOSE #1

DO: LOOP WHILE INKEY$ = ""

LOCATE 21, 1: PRINT "Converting to 2 pixels per byte ";

compPixel$ = ""
FOR nn = 1 TO LEN(rawpixel$) - 80 STEP 80
 FOR n = 0 TO 78 STEP 2
  c = ASC(MID$(rawpixel$, n + nn, 1))
  c2 = ASC(MID$(rawpixel$, n + nn + 1, 1))
  compPixel$ = compPixel$ + CHR$(c OR c2)
 NEXT n
PRINT ".";
NEXT nn

PRINT
PRINT "Converting to addressable tiles ..."


 DEF SEG = tilebankseg

 readofs = 1
 writeofs = tilebankofs

 FOR y = 0 TO 40 STEP 8
  FOR x = 0 TO 36 STEP 4
   readofs = y * 40 + x + 1
   FOR yy = y TO y + 7
   FOR xx = x TO x + 3

    c = ASC(MID$(compPixel$, readofs, 1))
    POKE writeofs, c

    writeofs = writeofs + 1
    readofs = readofs + 1
   NEXT xx
    readofs = readofs + 36
   NEXT yy
  NEXT x
 NEXT y

PRINT
PRINT "Saving... ";

IF LEN(DIR$(outPutFile$ + ".ctg")) THEN KILL outPutFile$ + ".ctg"

DEF SEG = VARSEG(tileBank)
BSAVE outPutFile$ + ".ctg", VARPTR(tileBank), 2000

'OPEN outputfile$ + ".ctg" FOR BINARY AS #1
' PUT #1, 1, tilebank
'CLOSE #1

PRINT "Done."

END

n = 0
FOR a = 1 TO LEN(compPixel$)
 a$ = HEX$(ASC(MID$(compPixel$, a, 1)))
 IF LEN(a$) < 2 THEN a$ = "0" + a$
 PRINT a$;
 'n = n + 1
 'IF n > 40 THEN n = 0: PRINT ""
NEXT a

DO: LOOP WHILE INKEY$ = ""

n = 0
FOR a = 1 TO LEN(tileBank)
 a$ = HEX$(ASC(MID$(tileBank, a, 1)))
 IF LEN(a$) < 2 THEN a$ = "0" + a$
 PRINT a$;
 n = n + 1
 IF n > 3 THEN n = 0: PRINT ""
NEXT a


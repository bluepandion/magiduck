'Convert sprites to two pixels / byte + mask
'---------------------------------------------------------------------------
DIM spriteOffset(128)   AS INTEGER      'Sprite bank offsets per sprite
DIM spriteWidth(128)  AS INTEGER        'and some basic data.
DIM spriteHeight(128)  AS INTEGER
DIM spriteFlipIndex(128) AS INTEGER
DIM saveThisSprite(128) AS INTEGER

TYPE animDataType
 index(0 TO 1) AS INTEGER
 X(0 TO 1) AS INTEGER
 Y AS INTEGER
 jump AS INTEGER
END TYPE
DIM animation(0 TO 255) AS animDataType
DIM saveThisAnim(0 TO 255) AS INTEGER

DIM n AS INTEGER
DIM longOfs AS LONG

'---------------------------------------------------------------------------

file$ = "spenemy0"
isAuxBank = 1

infile$ = "..\convert\" + file$ + ".tga"

outFile$ = file$

alphacolor = 11

'---------------------------------------------------------------------------

alphac1 = alphacolor
alphac2 = alphacolor * 16

DIM SHARED spriteimport AS STRING
DIM SHARED conversion AS STRING * 6400
DIM SHARED getbyte AS STRING * 1

DIM SHARED fileLength AS INTEGER

CLS
SCREEN 0

 PRINT "Converting "; file$; " ..."

COLOR 15

fileLength = 0
X = 0
Y = 150
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

 fileLength = fileLength + 1

 lineget$ = lineget$ + CHR$(c * 16)
 X = X + 1
 IF X = 80 THEN
  X = 0
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

PRINT "TGA load OK. Converting graphics...";

'----------------------------------------------------------------------------

DEF SEG = SSEG(spriteimport)
ofs = SADD(spriteimport)
outposeven = 1
FOR Y = 0 TO 79
 FOR X = 0 TO 78 STEP 2

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
 NEXT X
NEXT Y

'---------------------------------------------------------------------------

IF LEN(DIR$(outFile$ + ".csg")) THEN KILL outFile$ + ".csg"
IF LEN(DIR$(outFile$ + ".csd")) THEN KILL outFile$ + ".csd"

'OPEN outfile$ + ".csg" FOR BINARY AS #1
' PUT #1, 1, conversion
'CLOSE #1

DEF SEG = VARSEG(conversion)
BSAVE outFile$ + ".csg", VARPTR(conversion), fileLength
DEF SEG

PRINT "Done"
'---------------------------------------------------------------------------
PRINT "Converting defintions/animations to binary...";

 flipIndex = 80 + (isAuxBank * 20)
 
 OPEN outFile$ + ".def" FOR INPUT AS #1
  INPUT #1, a$

' Sprite data
   spriteOffset(0) = spriteBankOfs
   spriteWidth(0) = 0
   spriteHeight(0) = 0
   FOR n = 1 TO 127
    spriteOffset(n) = spriteBankOfs
    spriteWidth(n) = 2
    spriteHeight(n) = 1
   NEXT n

   DO
    INPUT #1, name$
    IF name$ <> "#anims" THEN
     INPUT #1, index

     INPUT #1, X
     INPUT #1, Y

     IF index > 199 THEN
       spriteFlipIndex(index - 200) = flipIndex
       index = flipIndex
       flipIndex = flipIndex + 1
       X = X + 40
      ELSE
       spriteFlipIndex(index) = index
     END IF

     saveThisSprite(index) = 1

     plong& = 0
     plong& = plong& + X + (Y * 80)
     a$ = MKL$(plong&)
     spriteOffset(index) = CVI(a$)

     INPUT #1, spriteWidth(index)
     spriteWidth(index) = spriteWidth(index) / 2
     INPUT #1, spriteHeight(index)
    END IF
   LOOP WHILE name$ <> "#anims"


' Animation data

   n = 0
   DO
   INPUT #1, name$
   IF name$ <> "#end" THEN
    INPUT #1, n
    INPUT #1, parts
    DO
       INPUT #1, a$
       'PRINT a$
       'DO: LOOP WHILE INKEY$ = ""
       INPUT #1, animation(n).index(0)
       INPUT #1, animation(n).X(0)
       INPUT #1, animation(n).Y
       animation(n).X(0) = animation(n).X(0) / 2
       INPUT #1, animation(n).jump
       animation(n).index(1) = spriteFlipIndex(animation(n).index(0))
       animation(n).X(1) = animation(n).X(0) * -1 - spriteWidth(animation(n).index(0))
       saveThisAnim(n) = 1
       n = n + 1
    LOOP WHILE animation(n - 1).jump > -1
   END IF
   LOOP WHILE name$ <> "#end"

 CLOSE #1

 OPEN outFile$ + ".csd" FOR BINARY AS #1
  fileOfs = 1
  FOR n = 0 TO 127
   IF saveThisSprite(n) = 1 THEN
    PUT #1, fileOfs, n
    PUT #1, fileOfs + 2, spriteOffset(n)
    PUT #1, fileOfs + 4, spriteWidth(n)
    PUT #1, fileOfs + 6, spriteHeight(n)
    fileOfs = fileOfs + 8
   END IF
  NEXT n

  n = -1
  PUT #1, fileOfs, n
  fileOfs = fileOfs + 2

  FOR n = 0 TO 255
   IF saveThisAnim(n) = 1 THEN
    PUT #1, fileOfs, n
    PUT #1, fileOfs + 2, animation(n)
    fileOfs = fileOfs + 14
   END IF
  NEXT n
  n = -1
  PUT #1, fileOfs, n

 CLOSE #1

'---------------------------------------------------------------------------
PRINT "Done."
PRINT
PRINT "Press ESC to exit, Any other for debug."
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


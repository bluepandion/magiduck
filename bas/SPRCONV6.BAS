DECLARE FUNCTION getCmdToken$ (n!)
' Sprite convert
'
' Converts sprites from TGA (File$) to two pixels / byte + mask.
' Converts .DEF-file with same filename to binary definition format.
'
' < isAuxBank = 1 > offets sprite definitions to match aux-sprite
'                   memory locations.
'---------------------------------------------------------------------------
' Arguments:
'---------------------------------------------------------------------------

DIM ofs AS LONG
DIM outPosEven AS LONG
TYPE animDataType
 offset(0 TO 1) AS INTEGER
 x AS INTEGER
 y AS INTEGER
 w AS INTEGER
 h AS INTEGER
 jump AS INTEGER
END TYPE
DIM animation(0 TO 128) AS animDataType

DIM saveThisAnim(0 TO 128) AS INTEGER

DIM n AS INTEGER
DIM longOfs AS LONG
 
 IF COMMAND$ = "" THEN PRINT " Parameters: [filename] [1, if aux bank]": END

 'file$ = "spenemy0"
 'isAuxBank = 1
 CLS
 file$ = getCmdToken$(0)
 isAuxBank = VAL(getCmdToken$(1))
 'PRINT file$
 'PRINT isauxbank
 'DO: LOOP WHILE INKEY$ = ""
 'END

 alphacolor = 11

 inFile$ = "..\convert\" + file$ + ".tga"
 inDEF$ = "..\convert\" + file$ + ".txt"
 outFile$ = file$

'---------------------------------------------------------------------------

'---------------------------------------------------------------------------

alphac1 = alphacolor
alphac2 = alphacolor * 16

DIM SHARED spriteimport AS STRING
DIM SHARED conversion AS STRING * 7680
DIM SHARED getbyte AS STRING * 1

DIM SHARED fileLength AS INTEGER

CLS
SCREEN 0

 PRINT "Converting "; file$; " ..."

COLOR 15

fileLength = 0
x = 0
y = 150
a = 67 + 80 * 9

DEF SEG = &HB800
vofs = 3998

OPEN inFile$ FOR BINARY AS #1

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

PRINT "TGA load OK. Converting graphics...";

'----------------------------------------------------------------------------

DEF SEG = SSEG(spriteimport)
ofs = SADD(spriteimport)
outPosEven = 1
FOR y = 0 TO 95
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

  MID$(conversion, outPosEven, 1) = CHR$(c OR c2)
  MID$(conversion, outPosEven + 1, 1) = CHR$(mask)

  ofs = ofs + 2
  outPosEven = outPosEven + 2
 NEXT x
NEXT y

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
' Animation data
	OPEN inDEF$ FOR INPUT AS #1
	n = 0
	INPUT #1, name$
	IF name$ <> "#anims" THEN PRINT "Not a valid animation definition file.": END
	DO
		INPUT #1, name$
		IF name$ = "" THEN
		 CLS
		 COLOR 7, 0
		 PRINT "Unnamed entry found in animation listing. (Prev:"; n; ")."
		END IF
		IF name$ <> "#end" THEN
			INPUT #1, n
			saveThisAnim(n) = 1
		 INPUT #1, x
			INPUT #1, y
			animation(n).offset(0) = y * 80 + x
			animation(n).offset(1) = y * 80 + x + 40
			INPUT #1, animation(n).w
			animation(n).w = animation(n).w / 2
			INPUT #1, animation(n).h
			INPUT #1, animation(n).x
			animation(n).x = animation(n).x / 2
			INPUT #1, animation(n).y
			INPUT #1, animation(n).jump
			animation(n).jump = n + animation(n).jump
		END IF
	LOOP WHILE name$ <> "#end"
 CLOSE #1

 OPEN outFile$ + ".csd" FOR BINARY AS #1
  fileOfs = 1
  
  FOR n = 0 TO 128
	 IF saveThisAnim(n) THEN
		PUT #1, fileOfs, n
		fileOfs = fileOfs + 2
		PUT #1, fileOfs, animation(n).offset(0)
		PUT #1, fileOfs + 2, animation(n).offset(1)
		PUT #1, fileOfs + 4, animation(n).w
		PUT #1, fileOfs + 6, animation(n).h
		PUT #1, fileOfs + 8, animation(n).x
		PUT #1, fileOfs + 10, animation(n).y
		PUT #1, fileOfs + 12, animation(n).jump
		fileOfs = fileOfs + 14
		'PRINT n; ",";
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
IF d$ = CHR$(27) THEN
	CLS
	COLOR 7
	PRINT "Converted "; inFile$; " to "; outFile$; ".CSG and "; outFile$; ".CSD ";
	IF isAuxBank THEN PRINT " As AUX bank."
	END
END IF
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

FUNCTION getCmdToken$ (n)
 'PRINT COMMAND$
 token$ = ""
 num = 0
 FOR nn = 1 TO LEN(COMMAND$)
	a$ = MID$(COMMAND$, nn, 1)
	'PRINT a$; " -";
	IF a$ = " " THEN
		IF num = n THEN
			getCmdToken$ = token$
			'PRINT
			'PRINT n;
			'PRINT "RETURN "; token$
			EXIT FUNCTION
		END IF
		a$ = ""
		token$ = ""
		num = num + 1
	END IF
	token$ = token$ + a$
 NEXT nn
 'PRINT
 'PRINT "........................ FINAL TOKEN "; token$
 getCmdToken$ = token$
END FUNCTION


DECLARE SUB initScreen ()
DEFINT A-Z
'$INCLUDE: 'libelf.bi'

testString$ = ".,?!'0123456789  MAGIDUCK  WAVE  LIVES"

DIM tBox(320 * 24 + 2)

DIM copy AS INTEGER

TYPE charPair
 letter1(32 TO 90) AS STRING * 1
 letter2(32 TO 90) AS STRING * 1
END TYPE

TYPE convCharType
 letter1(48 TO 90) AS STRING * 1
 letter2(48 TO 90) AS STRING * 1
END TYPE

'DIM w40Char AS charPair
DIM convChar AS convCharType

OPEN "w40char.def" FOR BINARY AS #1
 IF LOF(1) < 2 THEN
  FOR n = 32 TO 90
	w40Char.glyph1(n) = CHR$(n)
	w40Char.glyph2(n) = CHR$(n)
  NEXT n
  CLOSE #1
 ELSE
  GET #1, , w40Char
  CLOSE #1
 END IF

'FOR a = 48 TO 90
' w40char.glyph1(a) = convChar.glyph1(a)
' w40char.glyph2(a) = convChar.glyph2(a)
'NEXT a
'FOR a = 32 TO 47
' w40char.glyph1(a) = CHR$(a)
' w40char.glyph2(a) = CHR$(a)
'NEXT a

'initScreen

SCREEN 13

editc = 48

COLOR 1
'FOR a = 1 TO 40
' LOCATE 23, a: PRINT CHR$(221)
' LOCATE 24, a: PRINT CHR$(222)
'NEXT a

'COLOR 7, 0
'LOCATE 20, 1
'FOR n = 32 TO 255
' PRINT CHR$(n);
'NEXT n

updateScreen:
LOCATE 11, 1: PRINT TIMER
row = 1
col = 1
COLOR 8
FOR n = 32 TO 90
 LOCATE row, col: PRINT w40Char.glyph1(n)
 LOCATE row + 1, col: PRINT w40Char.glyph2(n)
 IF editc = n THEN
  COLOR 7
  LOCATE row, col: PRINT w40Char.glyph1(n)
  LOCATE row + 1, col: PRINT w40Char.glyph2(n)
  COLOR 8
 END IF
 col = col + 1
 IF col = 41 THEN col = 1: row = row + 4
NEXT n
GET (0, 8)-(319, 11), tBox
PUT (0, 4), tBox, PSET
LINE (0, 8)-(319, 15), 0, BF
LOCATE 12, 1: PRINT TIMER

DO
 LOCATE 3, 1: PRINT CHR$(editc)
 d$ = INKEY$

 IF d$ = CHR$(13) THEN
  OPEN "w40char.def" FOR BINARY AS #1
	PUT #1, 1, w40Char
  CLOSE #1
  LOCATE 15, 1: PRINT "Saved "; TIMER
 END IF
 IF d$ = CHR$(0) + "K" AND editc > 32 THEN editc = editc - 1: GOTO updateScreen
 IF d$ = CHR$(0) + "M" AND editc < 90 THEN editc = editc + 1: GOTO updateScreen

 IF d$ = "q" AND ASC(w40Char.glyph1(editc)) > 0 THEN w40Char.glyph1(editc) = CHR$(ASC(w40Char.glyph1(editc)) - 1): GOTO updateScreen
 IF d$ = "w" AND ASC(w40Char.glyph1(editc)) < 255 THEN w40Char.glyph1(editc) = CHR$(ASC(w40Char.glyph1(editc)) + 1): GOTO updateScreen
 IF d$ = "a" AND ASC(w40Char.glyph2(editc)) > 0 THEN w40Char.glyph2(editc) = CHR$(ASC(w40Char.glyph2(editc)) - 1): GOTO updateScreen
 IF d$ = "s" AND ASC(w40Char.glyph2(editc)) < 255 THEN w40Char.glyph2(editc) = CHR$(ASC(w40Char.glyph2(editc)) + 1): GOTO updateScreen
 
 IF d$ = "c" THEN copy = editc
 IF d$ = "v" AND copy > 32 THEN
  w40Char.glyph1(editc) = w40Char.glyph1(copy)
  w40Char.glyph2(editc) = w40Char.glyph2(copy)
  GOTO updateScreen
 END IF

 IF d$ = "t" OR d$ = "i" THEN
  IF d$ = "i" THEN testString$ = STR$(ASC(w40Char.glyph1(editc))) + STR$(ASC(w40Char.glyph2(editc)))
  COLOR 2
  row = 15
  col = 1
  FOR n = 1 TO LEN(testString$)
	c = ASC(MID$(testString$, n, 1))
	IF c > 32 THEN
	COLOR 7
	LOCATE row, col: PRINT w40Char.glyph1(c)
	COLOR 12
	LOCATE row + 1, col: PRINT w40Char.glyph2(c)
	END IF
	col = col + 1
	IF col = 41 THEN col = 1: row = row + 2
  NEXT n
  GET (0, 15 * 8)-(319, 15 * 8 + 3), tBox
  PUT (0, 15 * 8 - 4), tBox, PSET
  LINE (0, 120)-(319, 127), 0, BF
 END IF
 
LOOP WHILE d$ <> CHR$(27)

SCREEN 0
SHELL "cls"

SUB initScreen

 ON LOCAL ERROR GOTO nextmode

 gfx.adapter = 13

 SCREEN gfx.adapter
 GOTO continueInit

nextmode:
 IF gfx.adapter = 1 THEN gfx.adapter = 0
 IF gfx.adapter = 7 THEN gfx.adapter = 1
 IF gfx.adapter = 13 THEN gfx.adapter = 7
 RESUME

continueInit:

 IF gfx.adapter = 0 THEN CLS : PRINT "This game requires a CGA-compatible video card.": END

 SCREEN 0
 WIDTH 40, 25
 VIEW PRINT
 CLS

 IF gfx.adapter = 1 THEN gfx.rows = 49: gfx.glyphHeight = 3: PRINT "CGA";
 IF gfx.adapter = 7 THEN gfx.rows = 49: gfx.glyphHeight = 6: PRINT "EGA";
 IF gfx.adapter = 13 THEN gfx.rows = 49: gfx.glyphHeight = 7: PRINT "VGA";

 PRINT " detected."

 OUT &H3D4, &H9
 OUT &H3D5, gfx.glyphHeight

 OUT &H3D4, &H4
 OUT &H3D5, gfx.rows

 'Blink disable
 IF gfx.adapter = 1 THEN
	OUT &H3D8, 8
  ELSE
	b = INP(&H3DA)
	b = INP(&H3C1)
	OUT &H3C0, &H10
	OUT &H3C0, 16
 END IF

END SUB


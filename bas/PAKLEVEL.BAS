DECLARE SUB killOldPak (file$)
DECLARE SUB saveLevel (file$, No%)
DECLARE SUB pause ()
DECLARE SUB clearLevel ()
DECLARE SUB addActor (actor%, wave%, x%, y%)
DECLARE SUB exitCode (code%)


'$INCLUDE: 'libelf.bi'

CONST CScoreDiamond = 50
CONST CScoreDiamondBig = 50
CONST CScoreChest = 50
CONST CScoreKey = 50
CONST CScoreCoin = 50
CONST CScorePowerOrb = 50

TYPE actorConv
 name AS STRING * 6
 id AS INTEGER
 xOfs AS INTEGER
 yOfs AS INTEGER
 flags AS INTEGER
END TYPE

DIM SHARED actorConvert(1 TO 210) AS actorConv

DIM SHARED levelMenuRef(0 TO 15) AS INTEGER

DIM strGet AS STRING * 1

DIM SHARED rowInfo
DIM SHARED rowDebug

DIM SHARED levelStr$

DIM SHARED ofsDATA AS INTEGER

DIM SHARED maxScore AS LONG

ofsDATA = 64

rowDebug = 8
rowInfo = 1

tokenLayer$ = "layer"
tokenData$ = "data encoding"

pakFile$ = "levels"

VIEW PRINT 1 TO 25

CLS
PRINT COMMAND$

listFile$ = "lvlconv.txt" 'COMMAND$

OPEN listFile$ FOR BINARY AS #1
 PRINT LOF(1)
 IF LOF(1) < 1 THEN
  PRINT "File does not exist "; listFile$
  CLOSE #1
  END
 END IF
CLOSE #1

listMode = 0
readToggle = 1

VIEW PRINT 1 TO 7
LOCATE 1, 1, 1

OPEN listFile$ FOR INPUT AS #1
 DO
  IF readToggle THEN INPUT #1, a$
  readToggle = 1

  IF LCASE$(a$) = "actors" THEN
	listMode = 1
	PRINT " Reading actor conversion list."
  END IF
  
  IF LCASE$(a$) = "/convert" THEN CLOSE #1: END

  fileOfs = 1
  IF listMode = 1 THEN
	DO
	 INPUT #1, a$
	 n = VAL(a$)
	 IF n THEN
	  IF actorConvert(n).id > 0 THEN
		COLOR 4
		PRINT " Duplicate Actor conversion definition: "; n
		PRINT " "; actorConvert(n).name
		BEEP
		END
	  END IF
	  INPUT #1, actorConvert(n).name
	  INPUT #1, actorConvert(n).id
	  INPUT #1, actorConvert(n).xOfs
	  actorConvert(n).xOfs = actorConvert(n).xOfs / 2
	  INPUT #1, actorConvert(n).yOfs
	  INPUT #1, actorConvert(n).flags
	 END IF
	LOOP WHILE n
	listMode = 0
	readToggle = 0
  END IF

'-------------------------------------------------------------------------
  IF LCASE$(a$) = "pakfile" THEN
	INPUT #1, pakFile$
	INPUT #1, a$
	killOldPak pakFile$
	PRINT " Found output file "; pakFile$ + "."
  END IF

  IF LCASE$(a$) = "maps" THEN
	listMode = 2
	PRINT " Reading level list"
	INPUT #1, a$
  END IF

  IF listMode = 2 THEN
	IF a$ <> "/maps" THEN
	clearLevel
	levelNo = val(a$)
	INPUT #1, levelName$
	INPUT #1, inFile$
	INPUT #1, level.tileSet
	INPUT #1, level.spriteSet	
	n = -1
	DO
		n = n + 1
		INPUT #1, levelMenuRef(n)
	LOOP WHILE levelMenuRef(n) > -1
	level.playerX = 0
	level.playerY = 0
	token$ = ""
	tokenMode = 0
	levelStr$ = ""
	tileSet = 0
	actorWave = 0
	actorSpawner = 1
	level.keys = 0
	level.coins = 0
	x = 0
	y = 0
'-------------------------------------------------------------------------
	
	PRINT "Converting level "; inFile$

	OPEN inFile$ FOR BINARY AS #2
	 FOR fileOfs = 1 TO LOF(2)
		'LOCATE 1, 1: PRINT tokenMode
		GET #2, fileOfs, strGet
		
		IF strGet = "<" THEN
		 strGet = CHR$(255)
		 token$ = ""
		END IF

		IF strGet = ">" THEN
		 strGet = CHR$(255)
		 token$ = ""
		END IF

		IF strGet = "," THEN
		 strGet = CHR$(255)

		 IF tokenMode = 2 THEN
		  tile = (VAL(token$) - 141) MOD 60
		  IF tile < 0 THEN tile = 0
		  IF tile = 28 OR tile = 58 THEN level.coins = level.coins + 1
		  levelStr$ = levelStr$ + CHR$(tile)
		 END IF

		 IF tokenMode = 4 THEN
		  tile = VAL(token$)
		  IF tile > 0 THEN

			IF tile = 1 THEN
			 level.playerX = x
			 level.playerY = y + 8
			 PRINT "   Player start"
			END IF

			IF tile > 1 AND tile < 9 THEN
			 actorWave = tile - 2
			 actorSpawner = 1
			 'pause
			 CLS 1
			END IF

			IF tile > 11 AND tile < 19 THEN
			 nWave = tile - 12
			 'IF nWave = actorWave THEN
			 '  nWave = nWave + 1
			 'END IF
			 addActor tile, actorWave, x, y
			 'IF actorWave < 5 THEN addActor tile, actorWave + 1, x, y
			END IF

			IF tile > 19 OR tile = 11 THEN
			 addActor tile, actorWave, x, y
			END IF
		  END IF
		  x = x + 4
		  IF x > 79 THEN x = 0: y = y + 8
		 END IF
		 token$ = ""
		END IF

		SELECT CASE tokenMode
		 CASE 0
		  IF token$ = tokenLayer$ THEN
			token$ = ""
			tokenMode = 1
			PRINT " Found tile layer."
		  END IF
		 CASE 1
		  IF token$ = tokenData$ THEN
			token$ = ""
			tokenMode = 2
			PRINT "  Found tile data."
		  END IF
		 CASE 2
		  IF token$ = tokenLayer$ THEN
			token$ = ""
			tokenMode = 3
			PRINT " Found actor layer."
		  END IF
		 CASE 3
		  IF token$ = tokenData$ THEN
			token$ = "": tokenMode = 4
			PRINT "  Found actor data."
		  END IF
		END SELECT

		IF token$ = "/map" THEN fileOfs = LOF(2): PRINT " Map conversion complete."

		IF strGet <> CHR$(255) THEN token$ = token$ + LCASE$(strGet)

	 NEXT fileOfs
	CLOSE #2
	 IF level.playerX = 0 AND level.playerY = 0 THEN
	  COLOR 4
	  BEEP
	  PRINT "PLAYER START NOT FOUND."
	  pause
	 END IF
	 'OPEN fileOut$ + ".lvl" FOR BINARY AS #2
	 'l = LOF(2)
	 'CLOSE #2
	 'IF l THEN KILL fileOut$ + ".lvl"
	 'OPEN fileOut$ + ".lvl" FOR BINARY AS #2
	 ' PUT #2, 1, levelStr$
	 ' PUT #2, 1601, level
	 'CLOSE #2
	 PRINT " Saving at "; ofsDATA; "..."
	 saveLevel pakFile$, levelNo
	 PRINT " Level converted and saved to "; pakFile$ + ".clp"
	 'pause
	ELSE
	readToggle = 0
	listMode = 0
	END IF
  END IF
'-------------------------------------------------------------------------
 LOOP WHILE NOT EOF(1)
CLOSE #1

'-------------------------------------------------------------------------
'-------------------------------------------------------------------------
'-------------------------------------------------------------------------

SUB addActor (actor, wave, x, y)
 STATIC aRow
 IF aRow = 0 THEN aRow = 8
 pRow = CSRLIN

 VIEW PRINT 8 TO 25
 LOCATE aRow

 IF actorConvert(actor).id > 0 THEN
  FOR spawner = 1 TO 16
	nn = 16 * wave + spawner
	IF level.spawner(nn).object = 0 THEN
	 COLOR 7
	 IF actorConvert(actor).id = 44 THEN level.keys = level.keys + 1	 
	 level.spawner(nn).object = actorConvert(actor).id
	 level.spawner(nn).x = actorConvert(actor).xOfs + x
	 level.spawner(nn).y = actorConvert(actor).yOfs + y
	 level.spawner(nn).flags = actorConvert(actor).flags
	 IF actorConvert(actor).id = 33 THEN 
		level.spawner(nn).flags = levelMenuRef(actorConvert(actor).flags)
	 END IF
	 PRINT "   Wave:"; wave; " Spawner:"; spawner, "Actor: "; actor,
	 PRINT actorConvert(actor).name, x; ","; y
	 spawner = 999: GOTO exitWindow
	END IF
  NEXT spawner
	COLOR 12
	PRINT "     Too many actors per wave "; wave
	BEEP
	pause
	COLOR 7
	IF actor > 11 AND actor < 19 THEN
		COLOR 4
		PRINT "CRITICAL: Checkpoint missing!"
		BEEP
		END
	END IF
  ELSE
	IF actorConvert(actor).id = 0 THEN
		COLOR 4
		PRINT "     Actor not found: "; actor
		pause
		COLOR 7
	END IF
 END IF
exitWindow:
 aRow = CSRLIN
 VIEW PRINT 1 TO 7
 LOCATE pRow, , 1
END SUB

SUB clearLevel
	FOR n = 0 TO 80
		level.spawner(n).object = 0
		level.spawner(n).x = 0
		level.spawner(n).y = 0
		level.spawner(n).flags = 0
	NEXT n
END SUB

SUB exitCode (code)
	SELECT CASE code
		CASE 1
			PRINT "Too many actors per wave."
		END SELECT
	END
END SUB

SUB killOldPak (file$)
	OPEN file$ + ".clp" FOR BINARY AS #2
		n = LOF(1)
	CLOSE #2
	IF n THEN KILL file$ + ".clp"
	BEEP
END SUB

SUB pause
	r = CSRLIN
	c = POS(0)
	cc = 0
	COLOR 14
	LOCATE 25, 65, 1: PRINT "continue y/n";
	DO
		d$ = INKEY$
		LOOP WHILE d$ = ""
	IF LCASE$(d$) = "n" THEN END
	COLOR 7
	LOCATE 25, 65, 0: PRINT "            "
	LOCATE r, c, 0
END SUB

SUB saveLevel (file$, No)
	STATIC tile AS STRING * 1
	STATIC prevTile AS STRING * 1
	STATIC rleCount AS INTEGER
	STATIC byte AS STRING * 1

	OPEN file$ + ".clp" FOR BINARY AS #2
		PUT #2, No * 2 + 1, ofsDATA

		PUT #2, ofsDATA, level.playerX
		PUT #2, ofsDATA + 2, level.playerY
		ofsDATA = ofsDATA + 4

		PUT #2, ofsDATA, level.tileSet
		PUT #2, ofsDATA + 8, level.spriteSet
		ofsDATA = ofsDATA + 16

		PUT #2, ofsDATA, level.coins
		PUT #2, ofsDATA + 2, level.keys
		ofsDATA = ofsDATA + 4

		n = 1
		rleCount = 1
		prevTile = MID$(levelStr$, n, 1)
		DO
			n = n + 1
			tile = MID$(levelStr$, n, 1)
			IF tile <> prevTile OR rleCount = 255 OR n = 1600 THEN
				byte = CHR$(rleCount)
				PUT #2, ofsDATA, byte
				PUT #2, ofsDATA + 1, prevTile
				ofsDATA = ofsDATA + 2
				prevTile = tile
				rleCount = 0
			END IF
			rleCount = rleCount + 1
		LOOP WHILE n < 1601

		rleCount = 0
		FOR n = 1 TO 80
			IF level.spawner(n).object THEN rleCount = rleCount + 1
		NEXT n
		byte = CHR$(rleCount)
		PUT #2, ofsDATA, byte
		ofsDATA = ofsDATA + 1
		FOR n = 1 TO 80
			IF level.spawner(n).object THEN
				byte = CHR$(n)
				PUT #2, ofsDATA, byte
				PUT #2, ofsDATA + 1, level.spawner(n)
				ofsDATA = ofsDATA + 9
			END IF
		NEXT n
	CLOSE #2
END SUB

SUB switchWindow
END SUB


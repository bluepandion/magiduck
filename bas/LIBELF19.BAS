DECLARE FUNCTION getSpawnSlot% ()
DECLARE SUB handleCamera ()
DECLARE SUB handleScroll ()
DECLARE SUB damagePlayer ()
DECLARE SUB loadLevel ()
DECLARE SUB myPrint (x%, y%, text$, attributes%)
DECLARE SUB elfInit ()
DECLARE SUB initLookUps ()
DECLARE SUB loadSprites (file$, offset%)
DECLARE SUB loadTiles (file$)
DECLARE SUB loadSounds (file$)
DECLARE SUB quit ()
DECLARE SUB debugOut (text$)
DECLARE SUB hudTextLoad (offset AS INTEGER, show AS INTEGER)
DECLARE SUB menuLoad (menuNo%)
DECLARE SUB menuClear ()
DECLARE FUNCTION tileRead% (x%, y%)
DECLARE FUNCTION menuGetByte% ()
DECLARE FUNCTION menuGetWord% ()
DECLARE SUB ProgressBar (progress%)
DECLARE SUB SaveGame ()
DECLARE SUB LoadGame ()

DEFINT A-Z

'$INCLUDE: 'libelf.bi'

'-------------------------------------------------------------------------------
' Variables and types
'-------------------------------------------------------------------------------

DIM SHARED p0%
DIM SHARED p1%
DIM SHARED p2%
DIM SHARED p3%
DIM SHARED p4%
DIM SHARED p5%
DIM SHARED p6%
DIM SHARED p7%
DIM SHARED p8%
DIM SHARED p9%
DIM SHARED p10%
DIM SHARED p11%

DIM SHARED writeOfs AS LONG               'General purpose memory
DIM SHARED readOfs AS LONG                ' Read/Write variables
DIM SHARED storeOfs AS LONG
DIM SHARED pageOfs AS INTEGER

'DIM SHARED asmKBroutine AS STRING
'DIM SHARED KBoldSeg AS INTEGER              'KB state save.
'DIM SHARED KBoldOfs AS INTEGER
DIM SHARED Keyflags AS INTEGER

DIM SHARED spriteBank AS STRING * 7680

DIM SHARED tilemapReadX AS INTEGER              '0-7 scroll offset
DIM SHARED tilemapReadY AS INTEGER              '0-3 scroll offset
DIM SHARED tileMapReadOfs AS INTEGER            'Tilemap read offset

DIM SHARED tileBuffer AS STRING * 11362         '7760 + 1600 + 2000

DIM SHARED tilePosXlookUp(-16 TO 95) AS INTEGER
DIM SHARED tilePosYlookUp(-16 TO 655) AS INTEGER

DIM SHARED camSpeedX(-23 TO 23)
DIM SHARED camSpeedY(-19 TO 19)

DIM SHARED soundTrack AS STRING * 256

DIM SHARED menuPreload AS STRING
DIM SHARED menuLoadPos AS INTEGER
DIM SHARED menuLoadSrc AS INTEGER

SUB aUpdateTile (x, y)
	p2% = tilePosXlookUp(x) + tilePosYlookUp(y)
	IF p2% < 0 THEN p2% = 0
	IF p2% > 1599 THEN p2% = 1599
	
	p0% = (tilePosYlookUp(y) - gfx.tileOfs) \ 20
	IF p0% > 11 THEN p0% = 11
	
	aTileDraw p0%, tilePosXlookUp(x), p2%
END SUB

SUB elfInit
	gfx.adapter = aVideoDetect(VARSEG(hudBuffer), VARPTR(hudBuffer))
	CLS
	COLOR 12, 0
	PRINT " WARNING!"
	COLOR 7, 0
	PRINT 
	PRINT "  Beta 1 (2015-12-05)"
	PRINT "  This game has not been fully tested"
	PRINT "  on real hardware."
	COLOR 12, 0
	PRINT
	PRINT "  You are running this software at your"
	PRINT "  own risk!"
	PRINT	
	COLOR 7, 0
	a$ = UCASE$(MID$(COMMAND$, 1, 1))
	IF a$ = "C" THEN gfx.adapter = 1
	IF a$ = "P" THEN gfx.adapter = 2
	IF a$ = "T" THEN gfx.adapter = 3
	IF a$ = "M" THEN gfx.adapter = 5
	IF a$ = "E" THEN gfx.adapter = 6
	IF a$ = "V" THEN gfx.adapter = 7
	PRINT "  ";
	SELECT CASE gfx.adapter
	CASE 0
		PRINT "No compatible adapter";
	CASE 1
		PRINT "CGA";
	CASE 2
		PRINT "PCJR";
	CASE 3
		PRINT "Tandy SL/TL";
	CASE 4
		PRINT "Tandy 1000";
	CASE 5
		PRINT "MCGA";
	CASE 6
		PRINT "EGA";
	CASE 7
		PRINT "VGA";
	END SELECT
	PRINT " detected."
	PRINT
	COLOR 8, 0
	PRINT "  Use command line parameter 
	PRINT "   CGA, EGA, VGA, MCGA, TANDY or PCJR"
	PRINT "   to override automatic detection."
	PRINT
	COLOR 14, 0
	PRINT "  Press Y to start the game, 
	PRINT "   any other key to exit."
	COLOR 7, 0
	DO
	 D$ = INKEY$
	LOOP WHILE D$ = ""
	IF LCASE$(D$) <> "y" THEN END
	PRINT
	PRINT "MagiDuck startup..."
	'--------------------------------------------------------------------------
	' Setup variables
	'--------------------------------------------------------------------------
	hudBufferSeg = VARSEG(hudBuffer)
	hudBufferOfs = VARPTR(hudBuffer)

	spriteBankSeg = VARSEG(spriteBank)

	pLong& = VARPTR(spriteBank)
	a$ = MKL$(pLong&)
	spriteBankOfs = CVI(a$)                        'Sprite Bank main
	pLong& = pLong& + 4480
	a$ = MKL$(pLong&)
	spriteBankAuxOfs = CVI(a$)                     'Sprite Bank aux portion

	tileBufferSeg = VARSEG(tileBuffer)             'Tilemap render buffer
	tileBufferOfs = VARPTR(tileBuffer)             '7680 bytes (80x96)
																  '+ 80 bytes for safety
	pLong& = tileBufferOfs

	pLong& = pLong& + 7760
	a$ = MKL$(pLong&)
	tileBankOfs = CVI(a$)                          'Tile bank   = 2000 bytes
	pLong& = pLong& - 40
	a$ = MKL$(pLong&)
	tileBufferWrap = CVI(a$)

	pLong& = tileBufferOfs
	pLong& = pLong& + 9760
	a$ = MKL$(pLong&)
	TileMapOfs = CVI(a$)                           'Tile map    = 1600  bytes
																  '              20x80 tiles
	pLong& = tileBufferOfs
	pLong& = pLong& + 40                          'Offset tilebuffer by 40
	a$ = MKL$(pLong&)                              'for some safety
	tileBufferOfs = CVI(a$)
	'------------------------------------------------------
	gfx.windowOfs(0) = 0
	gfx.windowOfs(1) = 0

	FOR n = 0 TO 1
	 gfx.screenX(n) = 0
	 gfx.screenY(n) = 0
	 gfx.screenOldX(n) = 0
	 gfx.screenOldY(n) = 0
	 gfx.windowX(n) = 0
	 gfx.windowY(n) = 24
	 gfx.windowOfs(n) = 1920
	 spriteList(2, n).y = 255
	NEXT n
	'-----------------------------------------------------------------------
	' Set up lookups, preload some data and setup screen
	'-----------------------------------------------------------------------
	'a$ = MID$("CEV", gfx.adapter, 1) + "GA DETECTED."
	'myPrint 2, 1, a$, &H20E0
	'aPageFlip gfx.videoWrap, hudBufferSeg, hudBufferOfs, gfx.pageOfs(gfx.page), gfx.pageOfs(gfx.page XOR 1)
	'myPrint 2, 4, "LOOKUPS...", &H7070
	'debugOut "lookups init"
	initLookUps
	w40char.seg = VARSEG(w40char.glyph1(32))
	w40char.ofs = VARPTR(w40char.glyph1(32))
	'--------------------------------------------------------------------------
	' Store asm-routine variables to KBarray and send them to setup.
	'--------------------------------------------------------------------------
	KBarray(0) = VARSEG(soundTrack)
	KBarray(1) = VARPTR(soundTrack)
	KBarray(2) = VARSEG(KBarray(0))
	KBarray(3) = VARPTR(KBarray(0))
	KBarray(4) = spriteBankSeg
	KBarray(5) = spriteBankOfs
	KBarray(6) = tileBufferSeg
	KBarray(7) = tileBankOfs
	KBarray(8) = tileMapOfs
	KBarray(9) = tileBufferOfs
	KBarray(10) = hudBufferSeg
	KBarray(11) = hudBufferOfs
	KBarray(12) = VARSEG(w40char.glyph1(32))
	KBarray(13) = VARPTR(w40char.glyph1(32))
	aSetup VARSEG(KBarray(0)), VARPTR(KBarray(0))
	aSoundStop
	FOR n = 0 TO 130
	 KBarray(n) = 0
	NEXT n
	'--------------------------------------------------------------------------
	' Detect and initialize video adapter
	'--------------------------------------------------------------------------
	 'aInitVideo VARSEG(hudBuffer), VARPTR(hudBuffer)
	 
	 'gfx.adapter = ASC(MID$(hudBuffer, 1, 1))
	 'gfx.oldMode = ASC(MID$(hudBuffer, 3, 1))	 
	 IF gfx.adapter > 0 THEN
		aVideoSet gfx.adapter
		gfx.pageOfs(0) = 0
		gfx.pageOfs(1) = 4000
		gfx.page = 0
		IF gfx.adapter > 4 THEN
			gfx.pageWrap = 16383
			gfx.videoWrap = 32767
			gfx.pageOfs(0) = 3000
			gfx.pageOfs(1) = 10000
		ELSE
			gfx.pageWrap = 8191
			gfx.videoWrap = 16383
		END IF
	  ELSE
		PRINT "Display adapter not compatible."
		END		
	 END IF
	 'Save video mode to hudbuffers end, so it can be referenced by aPageFlip
	 MID$(hudBuffer, 161, 1) = MID$(hudBuffer, 1, 1)
	'------------------------------------------------------
	'debugOut "timer init"
	CALL aTimerStart
	'-----------------------------------------------------------------------
	' Setup keyboard
	'-----------------------------------------------------------------------
	DEF SEG = 0
	Keyflags = PEEK(&H417) AND &H70       'Save keyboard flags
	DEF SEG
	aKBinit 'KBoldSeg, KBoldOfs, p2%, p3%
END SUB

FUNCTION getSpawnSlot
	FOR n = 4 TO 60
		IF actor(n).exist = 0 THEN getSpawnSlot = n: EXIT FUNCTION
	NEXT n
	getSpawnSlot = 60
	'debugOut "WARNING: All spawn slots occupied!"
END FUNCTION

SUB handleCamera
	IF screenTargetX < 0 THEN screenTargetX = 0
	IF screenTargetX > 40 THEN screenTargetX = 40
	IF screenTargetY < 4 THEN screenTargetY = 4
	IF screenTargetY > 590 THEN screenTargetY = 590
	 
	FOR n = 0 TO 1
	 
	 diffX = screenTargetX - gfx.screenX(n)
	 diffY = screenTargetY - gfx.screenY(n)
	 
	 IF diffX THEN
	  IF ABS(diffX) > 23 THEN
		 speed = 4
		 IF diffX < -23 THEN speed = -4
	  ELSE
		 speed = camSpeedX(diffX)
	  END IF
	  gfx.screenX(n) = gfx.screenX(n) + speed
	  IF gfx.screenX(n) < 0 THEN gfx.screenX(n) = 0
	  IF gfx.screenX(n) > 40 THEN gfx.screenX(n) = 40
	 END IF

	 IF diffY THEN
	  IF ABS(diffY) > 19 THEN
		 speed = 4
		 IF diffY < -19 THEN speed = -4
	  ELSE
		 speed = camSpeedY(diffY)
	  END IF
	  gfx.screenY(n) = gfx.screenY(n) + speed
	  IF gfx.screenY(n) < 4 THEN gfx.screenY(n) = 4
	  IF gfx.screenY(n) > 590 THEN gfx.screenY(n) = 590
	 END IF
			
	NEXT n
END SUB

SUB handleScroll
 
	diffX = gfx.screenX(gfx.page) - gfx.screenOldX(gfx.page)
	diffY = gfx.screenY(gfx.page) - gfx.screenOldY(gfx.page)

	IF diffX = 0 AND diffY = 0 THEN EXIT SUB

	n = 0

	IF diffX THEN
		gfx.screenOldX(gfx.page) = gfx.screenX(gfx.page)
		gfx.windowOfs(gfx.page) = gfx.windowOfs(gfx.page) + diffX
		gfx.pageOfs(gfx.page) = gfx.pageOfs(gfx.page) + diffX
		IF diffX > 0 THEN
			tileCopyList(n, gfx.page).x = 40 - diffX
			tileCopyList(n, gfx.page).y = 0
			tileCopyList(n, gfx.page).w = diffX
			tileCopyList(n, gfx.page).h = 50
			n = n + 1
		END IF
		IF diffX < 0 THEN
			tileCopyList(n, gfx.page).x = 0
			tileCopyList(n, gfx.page).y = 0
			tileCopyList(n, gfx.page).w = ABS(diffX)
			tileCopyList(n, gfx.page).h = 50
			n = n + 1
		END IF
	END IF

	IF diffY THEN
		gfx.screenOldY(gfx.page) = gfx.screenY(gfx.page)
		gfx.windowY(gfx.page) = gfx.windowY(gfx.page) + diffY
		gfx.windowOfs(gfx.page) = gfx.windowOfs(gfx.page) + diffY * 80
		gfx.pageOfs(gfx.page) = gfx.pageOfs(gfx.page) + diffY * 40
		IF diffY > 0 THEN
			tileCopyList(n, gfx.page).x = 0
			tileCopyList(n, gfx.page).y = 50 - diffY - 1
			tileCopyList(n, gfx.page).w = 40
			tileCopyList(n, gfx.page).h = diffY + 1
			n = n + 1
		  ELSE
			tileCopyList(n, gfx.page).x = 0
			tileCopyList(n, gfx.page).y = 0
			tileCopyList(n, gfx.page).w = 40
			tileCopyList(n, gfx.page).h = ABS(diffY) + 2
			n = n + 1
		END IF
	END IF
	
	gfx.pageOfs(gfx.page) = gfx.pageOfs(gfx.page) AND gfx.pageWrap
	
	IF gfx.windowY(gfx.page) < 0 THEN
		gfx.windowY(0) = gfx.windowY(0) + 24
		gfx.windowY(1) = gfx.windowY(1) + 24
		gfx.windowOfs(0) = gfx.windowOfs(0) + 1920
		gfx.windowOfs(1) = gfx.windowOfs(1) + 1920
		gfx.tileOfs = gfx.tileOfs - 60 'tilePosYlookUp(gfx.screenY(gfx.page)) - 60
		
		aTilePan 1
		
		aTileArea gfx.tileOfs, gfx.tileArea
	END IF

	IF gfx.windowY(gfx.page) > 47 THEN
		gfx.windowY(0) = gfx.windowY(0) - 24
		gfx.windowY(1) = gfx.windowY(1) - 24

		gfx.windowOfs(0) = gfx.windowOfs(0) - 1920
		gfx.windowOfs(1) = gfx.windowOfs(1) - 1920
		
		gfx.tileOfs = gfx.tileOfs + 60 ' tilePosYlookUp(gfx.screenY(gfx.page)) - 60

		aTilePan 0

		aTileArea gfx.tileOfs + 180, gfx.tileArea + 3
		
	END IF

	tileCopyList(n, gfx.page).y = 255

	aRectList gfx.windowOfs(gfx.page), gfx.pageOfs(gfx.page), VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)
	
	IF gfx.adapter > 4 THEN
		IF gfx.pageOfs(0) < 500 OR gfx.pageOfs(1) > 13500 THEN
			'debugOut str$(gfx.pageOfs(0)) + str$(gfx.pageOfs(1))
			aCopyPage gfx.pageOfs(0), 3000
			aCopyPage gfx.pageOfs(1), 10000
			tileCopyList(0, 0).x = 0
			tileCopyList(0, 0).y = 0
			tileCopyList(0, 0).w = 40
			tileCopyList(0, 0).h = 2
			tileCopyList(1, 0).y = 255
			aClearList gfx.pageOfs(gfx.page xor 1), VARSEG(tileCopyList(0, 0).offset), VARPTR(tileCopyList(0, 0).offset), &HDE
			gfx.pageOfs(0) = 3000
			gfx.pageOfs(1) = 10000
		END IF
	END IF
	
END SUB

SUB initLookUps
	OPEN "w40char.clu" FOR BINARY AS #1
		GET #1, , w40char
	CLOSE #1

	OPEN "sincos.clu" FOR BINARY AS #1
		GET #1, 1, angle
	CLOSE #1

	OPEN "floats.clu" FOR INPUT AS #1
		FOR ff = 0 TO 9
			FOR n = 0 TO 9
				INPUT #1, v
				FOR f = 0 TO 3
					fraction(f * 10 + ff, n) = f + v
					fraction(f * -10 - ff, n) = (f + v) * -1
				NEXT f
			NEXT n
		NEXT ff
	CLOSE #1

	m# = .2
	FOR n = 1 TO 20                        '2  = 0.4
		mulTable(n, 0) = 0
		FOR a = 1 TO 20                    '4  = 0.8
			v = INT(m# * a)
			mulTable(n, a) = v             '8  = 1.6
			mulTable(n, -a) = -v
		NEXT a                             '12 = 2.4
		m# = m# + .2                       '16 = 3.2
	NEXT n

	C = 0
	x = 0
	FOR n = -16 TO 95
		tilePosXlookUp(n) = x
		IF n > -1 AND n < 79 THEN C = C + 1
		IF C = 4 THEN C = 0: x = x + 1
	NEXT n

	C = 0
	y = 0
	FOR n = -16 TO 655
		tilePosYlookUp(n) = y
		IF n > -1 AND n < 639 THEN C = C + 1
		IF C = 8 THEN C = 0: y = y + 20
	NEXT n

	FOR n = -23 TO 23
		speed = 0
		IF ABS(n) > 0 THEN speed = 1
		IF ABS(n) > 17 THEN speed = 2
		IF n < 0 THEN speed = -speed
		camSpeedX(n) = speed
	NEXT n

	FOR n = -19 TO 19
		speed = 0
		IF ABS(n) > 0 THEN speed = 1
		IF ABS(n) > 6 THEN speed = 2
		IF ABS(n) > 12 THEN speed = 3
		IF n < 0 THEN speed = -speed
		camSpeedY(n) = speed
	NEXT n

	OPEN "menuprel.cmp" FOR BINARY AS #1
		n = LOF(1)
		menuPreload = SPACE$(n)
		GET #1, 1, menuPreload
	CLOSE #1	
END SUB

SUB loadLevel
	
	IF player.levelPrev <> player.level THEN
		
		FOR n = 1 to 64
			level.spawner(n).object = 0
		NEXT n
	ELSE
		FOR n = 1 to 64
			level.spawner(n).object = level.spawner(n).object AND &HFF
		NEXT n
	END IF
	
	OPEN "levels.clp" FOR BINARY as # 1
		GET #1, player.level * 2 + 1, fileOfs
	
		IF player.levelPrev <> player.level THEN			
			GET #1, fileOfs, level.playerX
			GET #1, fileOfs + 2, level.playerY
			fileOfs = fileOfs + 4
			GET #1, fileOfs, level.tileSet
			GET #1, fileOfs + 8, level.spriteSet
			fileOfs = fileOfs + 16
			GET #1, fileOfs, level.coins
			GET #1, fileOfs + 2, level.keys
			fileOfs = fileOfs + 4
		ELSE		
			fileOfs = fileOfs + 24
		END IF
				
		progressBar 1
		GET #1, fileOfs, loadInt		
		fileOfs = fileOfs + 2		
		loadVarStr = SPACE$(loadInt)		
		GET #1, fileOfs, loadVarStr		
		debugOut str$(fileOfs)
		fileOfs = fileOfs + loadInt
		progressBar 2
		p0% = SSEG(loadVarStr)
		p1% = SADD(loadVarStr)	
		CALL aUnPackLevelRLE (p0%, p1%)		
		
		progressBar 3		
		IF player.levelPrev <> player.level THEN			
			GET #1, fileOfs, loadStr
			fileOfs = fileOfs + 1
			loadInt = ASC(loadStr)			
			IF loadInt > 0 THEN
				FOR n = 1 TO loadInt
					GET #1, fileOfs, loadStr
					fileOfs = fileOfs + 1					
					GET #1, fileOfs, level.spawner(ASC(loadStr))
					fileOfs = fileOfs + 8
				NEXT n
			END IF
		END IF
		ProgressBar 4
	CLOSE #1
	
	IF player.spriteSet <> level.spriteSet THEN 
		
		loadSprites level.spriteSet, spriteBankAuxOfs
		player.spriteSet = file$
	END IF	
	
	ProgressBar 5
	
	IF player.tileSet <> level.tileSet THEN loadTiles level.tileSet	
	
	w40char.text = "        |"
	aPrint &HFFFF, &H0000, 0, 0, hudBufferSeg, hudBufferOfs		
	
END SUB

SUB loadSounds (file$)
	OPEN file$ + ".cst" FOR BINARY AS #1
		GET #1, 1, soundTrack
	CLOSE #1
END SUB

SUB loadSprites (file$, offset)	
	IF offset = 0 THEN
		offset = spriteBankOfs
	ELSE
		offset = spriteBankAuxOfs		
	END IF

	DEF SEG = spriteBankSeg
	BLOAD file$ + ".csg", offset
	DEF SEG	
	OPEN file$ + ".csd" FOR BINARY AS #1
		fileOfs = 1
		DO
			GET #1, fileOfs, n
			fileOfs = fileOfs + 2
			IF n > -1 THEN
				GET #1, fileOfs, animation(n)
				
				'Calculate and convert actual sprite memory offset
				' for both flip-states.
				pLong& = animation(n).offset(0)
				pLong& = pLong& + spriteBankOfs
				a$ = MKL$(pLong&)                              					
				animation(n).offset(0) = CVI(a$)
				
				pLong& = animation(n).offset(1)
				pLong& = pLong& + spriteBankOfs
				a$ = MKL$(pLong&)                              					
				animation(n).offset(1) = CVI(a$)				
				
				fileOfs = fileOfs + 14
				'debugOut str$(n) + str$(animation(n).w) + str$(animation(n).h) + str$(animation(n).offset(0)) + str$(animation(n).offset(1))
			END IF
		LOOP UNTIL n = -1
	CLOSE #1
END SUB

SUB loadTiles (file$)
	DEF SEG = tileBufferSeg
	BLOAD file$ + ".ctg", tileBankOfs
	player.levelColor = PEEK(tileBankOfs)
	DEF SEG
	player.levelColor = player.levelColor + player.levelColor * 256
	player.levelColor = player.levelColor AND &HF0F0
	player.tileSet = file$
END SUB

SUB myPrint (x, y, text$, attributes)
	w40char.text = text$ + "|"
	aPrint gfx.videoWrap, attributes, x, y, &HB800, gfx.pageOfs(gfx.page)
END SUB

SUB quit
	IF gfx.adapter > 0 THEN
		aKBremove 					     'Restore old keyboard routine
		DEF SEG = 0
		n = PEEK(&H417) AND &H8F
		Keyflags = Keyflags OR n
		POKE (&H417), Keyflags           'Restore old keyboard flags
		DEF SEG
		CALL aSoundStop
		CALL aTimerEnd
	END IF
	 
	 'aExitVideo gfx.adapter, gfx.oldMode
	 aVideoExit

	 'SHELL "cls"	 
	 PRINT FRE(-1)
	 PRINT gfx.pageOfs(0), gfx.pageOfs(1)
	 C = 1

	 FOR n = 1 TO 64
	  C = C + 4
	  COLOR (n AND 1) + 7
	  PRINT HEX$(level.spawner(n).object); TAB(C);
	  IF ((n - 1) AND 15) = 15 THEN PRINT : C = 1
	 NEXT n
	n = 1
	nn = 1
	DO				
		n = actor(n).lNext
		PRINT n; actor(n).control;actor(n).x;actor(n).y;actor(n).frame,
		'PRINT actor(n).control; "["; actor(n).frame; "]";
		nn = nn + 1
	LOOP WHILE n > 1	
	 PRINT 
	 PRINT "n = "; nn	 
	 COLOR 7	 
	 PRINT " MagiDuck, its source code and its"
	 PRINT "  project files are Freeware."
	 PRINT
	 PRINT " Thanks for playing!"	
	 'OPEN "debug.txt" for output as #1
	 'FOR nn = 9760 + n TO 9760 + 1599
	 '  	PRINT #1, HEX$(ASC(MID$(tileBuffer, nn, 1)));
	 'NEXT nn
	 'close #1
	 END
END SUB

SUB screenFakePos (y)
 ' - Works on the assumption that the buffer window Y > 24, how to fix?
 '
 ' - windowY and windowOfs must match current situation.
 '
	yTileRelative = gfx.screenY(gfx.page) AND 7

	y = (y AND &HFFF8) + yTileRelative

	diffY = y - gfx.screenY(gfx.page)

	FOR n = 2 TO 60
		actor(n).y = actor(n).y + diffY
		actor(n).parentDY = actor(n).parentDY + diffY
	NEXT n

	FOR n = 0 TO 1
		gfx.screenY(n) = y
		gfx.screenOldY(n) = y
	'gfx.windowOfs(n) = gfx.windowX(gfx.page)
	'gfx.windowX(n) = gfx.windowX(gfx.page)
	'gfx.windowY(n) = 0
	NEXT n

	IF gfx.windowY(gfx.page) < 24 THEN

	END IF

	IF gfx.windowY(gfx.page) > 23 AND gfx.windowY(gfx.page) < 48 THEN
		aTileArea gfx.tileOfs, 0
	END IF

	gfx.windowOfs(gfx.page XOR 1) = gfx.windowOfs(gfx.page)
	gfx.windowY(gfx.page XOR 1) = gfx.windowY(gfx.page)

	'gfx.windowX(gfx.page XOR 1) = gfx.windowX(gfx.page)

	gfx.tileOfs = tilePosYlookUp(y) 
END SUB

DEFSNG A-Z
SUB screenResetPos (x%, y%)
	 ' To-Do
	 '
	 ' -Keep current offset (don't flip)
	 ' -Add 4000 AND 16383 to both page offsets (changes won't be visible).
	 ' -Draw new data to set offset
	 ' -Page flip to set offset 
	IF x% < 0 THEN x% = 0
	IF x% > 40 THEN x% = 40
	IF y% < 0 THEN y% = 0
	IF y% > 615 THEN y% = 615

	screenTargetYlock = -1
	screenTargetX = x%
	screenTargetY = y%

	yp = y% - (y% AND &HFFF8)

	FOR n = 0 TO 1
		gfx.screenX(n) = x%
		gfx.screenY(n) = y%
		gfx.screenOldX(n) = x%
		gfx.screenOldY(n) = y%
		gfx.windowX(n) = x%
		gfx.windowY(n) = 24 + yp
		gfx.windowOfs(n) = 1920 + x% + yp * 80
		spriteList(0, n).y = 255
	NEXT n

	x% = x% AND &HFFFC
	y% = y% AND &HFFF8

	'IF Y% < 24 THEN Y% = 24
	'IF Y% > 590 THEN Y% = 590

	gfx.tileOfs = tilePosYlookUp(y%) - 60
	gfx.tileArea = 0

	aTileArea gfx.tileOfs, 0
	aTileArea gfx.tileOfs + 60, 1
	aTileArea gfx.tileOfs + 120, 2
	aTileArea gfx.tileOfs + 180, 3

	spriteList(1, gfx.page).y = 255
	spriteList(0, gfx.page).x = 0
	spriteList(0, gfx.page).y = 0
	spriteList(0, gfx.page).w = 40
	spriteList(0, gfx.page).h = 50

	aRectList gfx.windowOfs(gfx.page), gfx.pageOfs(gfx.page), VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)

	aPageFlip gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
	gfx.page = gfx.page XOR 1

	spriteList(1, gfx.page).y = 255
	spriteList(0, gfx.page).x = 0
	spriteList(0, gfx.page).y = 0
	spriteList(0, gfx.page).w = 40
	spriteList(0, gfx.page).h = 50

	aRectList gfx.windowOfs(gfx.page), gfx.pageOfs(gfx.page), VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)

	' FOR n = 0 to 1
	'	 handleScroll
	'	 aPageFlip gfx.videoWrap, hudBufferSeg, hudBufferOfs, gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
	'	 gfx.Page = gfx.Page xor 1
	' NEXT n
END SUB

DEFINT A-Z
SUB switchAnim (objectIndex, anim)
	IF actor(objectIndex).anim = anim THEN EXIT SUB
	actor(objectIndex).frame = anim
	actor(objectIndex).anim = anim
END SUB

FUNCTION tileRead (x, y)
	DEF SEG = tileBufferSeg
	readOfs = 0& + TileMapOfs + tilePosXlookUp(x) + tilePosYlookUp(y)
	tileRead = PEEK(readOfs)
	DEF SEG
END FUNCTION

DEFSNG A-Z
SUB tileWrite (x AS INTEGER, y AS INTEGER, index AS INTEGER)
	 DEF SEG = tileBufferSeg
	 writeOfs = 0& + TileMapOfs + tilePosXlookUp(x) + tilePosYlookUp(y)
	 POKE writeOfs, index
	 DEF SEG
END SUB

SUB hudTextLoad (offset AS INTEGER, show AS INTEGER)
	loadStr = MID$(hudBuffer, 161, 1)
	OPEN "ducktext.ctt" FOR BINARY AS #1
		GET #1, offset + 1, hudBuffer
	CLOSE #1
	MID$(hudBuffer, 161, 1) = loadStr
	IF show THEN aPageFlip gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
END SUB

SUB menuLoad (menuNo%)
	if menuNo% < 0 THEN menuNo% = 0
	menuClear
	IF menuNo% > 9 THEN
		OPEN "menu.cmm" FOR BINARY AS #1
		menuLoadSrc = 1
	ELSE
		menuLoadSrc = 0
	END IF	
	
	menuLoadPos = menuNo% * 6 + 1
	fileOfs = menuGetWord%
	itemCount = menuGetWord%
	player.menuItem = menuGetWord%
	menuLoadPos = fileOfs	
	tileCopyList(0, gfx.page).x = menuGetByte%	
	tileCopyList(0, gfx.page).y = menuGetByte%
	tileCopyList(0, gfx.page).w = menuGetByte%
	tileCopyList(0, gfx.page).h = menuGetByte%	
	loadInt = menuGetWord%
	tileCopyList(1, gfx.page).y = 255
	aClearList gfx.pageOfs(gfx.page), VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset), loadInt
	prevGfxItem = 1
	
	for item = 1 to itemCount
		itemIDX = menuGetWord%
		n = getSpawnSlot
		actor(n).lPrev = menuGetWord%
		actor(n).lNext = menuGetWord%
		actor(n).x = menuGetWord%
		actor(n).y = menuGetWord%
		actor(n).vecX = menuGetWord%
		actor(n).vecY = menuGetWord%
		actor(n).frame = menuGetWord%
		IF itemIDX > 0 THEN 
			actor(n).exist = 1
			actor(n).control = -16000
			menuItem(itemIDX) = n
			aMenuHiLite actor(n).x, actor(n).y, actor(n).vecX, actor(n).vecY, actor(n).frame, gfx.pageOfs(gfx.page)
		END IF
		IF itemIDX < 0 THEN 
			actor(n).exist = 1
			actor(n).control = -16001
			actor(n).x = actor(n).x + gfx.screenX(0)
			actor(n).y = actor(n).y + gfx.screenY(0)
			actor(n).vecX = 0
			actor(n).vecY = 0
			actor(prevGfxItem).lNext = n
			actor(1).lPrev = n
			actor(n).lPrev = prevGfxItem
			actor(n).lNext = 1
			actor(n).visX = 50
			actor(n).visY = 60
			actor(n).life = -1
			actor(n).flags = 0
			prevGfxItem = n
		END IF
		IF itemIDX > -1 THEN
			itemLen = menuGetByte%		
			IF menuNo% > 9 THEN
				loadVarStr = SPACE$(itemLen)
				GET #1, menuLoadPos, loadVarStr
				w40char.text = loadVarStr
			ELSE
				w40char.text = MID$(menuPreload, menuLoadPos, itemLen)
			END IF
			menuLoadPos = menuLoadPos + itemLen				
			aPrint gfx.videoWrap, actor(n).frame, actor(n).x, actor(n).y + 1, &HB800, gfx.pageOfs(gfx.page)
		END IF
	NEXT item
	
	'debugOut " Menu load OK. Actors:"
	'FOR n = 1 to 60
'		IF actor(n).exist OR n = 1 THEN
'			debugOut str$(n) + str$(actor(n).lPrev) + str$(actor(n).lNext) + str$(actor(n).control)
'		END IF
'	NEXT n
	
	IF menuNo% > 9 THEN CLOSE #1	
END SUB

SUB menuClear
	FOR n = 1 to 16
		nn = menuItem(n)
		IF nn > 0 THEN
			actor(nn).exist = 0
			actor(nn).frame = 0
			actor(nn).lNext = 0
			actor(nn).lPrev = 0
		END IF
		menuItem(n) = 0
	NEXT n
	FOR n = 1 TO 60
		IF actor(n).control = -16001 THEN actor(n).exist = 0
	NEXT n
END SUB

'
' Read a byte/word from streamed or preloaded menu data.
'
FUNCTION menuGetByte%
	IF menuLoadSrc THEN
		GET #1, menuLoadPos, loadStr
		menuGetByte% = ASC(loadStr)		
	ELSE
		menuGetByte% = ASC(MID$(menuPreload, menuLoadPos, 1))
	END IF
	menuLoadPos = menuLoadPos + 1
	'debugOut STR$(menuLoadPos)
END FUNCTION

FUNCTION menuGetWord%
	IF menuLoadSrc THEN
		GET #1, menuLoadPos, loadInt
		menuGetWord% = loadInt
	ELSE
		menuGetWord% = CVI(MID$(menuPreload, menuLoadPos, 2))
	END IF
	menuLoadPos = menuLoadPos + 2
	'debugOut STR$(menuLoadPos)
END FUNCTION

SUB DEBUGOUT (s$)
	OPEN "debug.txt" FOR APPEND AS #3
		PRINT #3, s$
	CLOSE #3
END SUB

SUB ProgressBar(progress%)
	w40char.text = ".|"
	aPrint gfx.videoWrap, &H0C0C, 8 + progress%, 0, &HB800, gfx.pageOfs(gfx.page)
END SUB

SUB LoadGame
	fileOfs = 1 + player.saveSlot * 128
	OPEN "savegame.sav" FOR BINARY AS #1
		FOR n = 0 to 15
			GET #1, fileOfs, player.levelScore(n)
			fileOfs = fileOfs + 4
			GET #1, fileOfs, player.levelCompletion(n)
			fileOfs = fileOfs + 2
			GET #1, fileOfs, player.levelTime(n)
			fileOfs = fileOfs + 2
		NEXT n
	CLOSE #1
END SUB

SUB SaveGame
	player.levelCompletion(0) = 0
	FOR n = 1 TO 15
		player.levelCompletion(0) = player.levelCompletion(0) + player.levelCompletion(n)
	NEXT n	
	IF player.levelCompletion(0) > 1385 AND player.levelCompletion(0) < 1400 THEN
		player.levelCompletion(0) = 99
	ELSE
		player.levelCompletion(0) = player.levelCompletion(0) / 14
	END IF
	
	fileOfs = 1 + player.saveSlot * 128
	OPEN "savegame.sav" FOR BINARY AS #1		
		FOR n = 0 to 15
			PUT #1, fileOfs, player.levelScore(n)
			fileOfs = fileOfs + 4
			PUT #1, fileOfs, player.levelCompletion(n)
			fileOfs = fileOfs + 2
			PUT #1, fileOfs, player.levelTime(n)
			fileOfs = fileOfs + 2
		NEXT n
	CLOSE #1
	
	fileOfs = 536 + player.saveSlot * 90
	OPEN "menu.cmm" FOR BINARY AS #1
		a$ = DATE$
		PUT #1, fileOfs, a$
		fileOfs = fileOfs + 30
		'a$ = "SCORE:         |"
		'PUT #1, fileOfs, a$
		a$ = STR$(player.levelScore(0)) + " P|"
		'n = fileOfs
		'fileOfs = fileOfs + 15 - LEN(a$)
		PUT #1, fileOfs, a$
		'fileOfs = n + 30
		'a$ = "COMPLETE:     %|"
		'PUT #1, fileOfs, a$
		fileOfs = fileOfs + 30
		a$ = STR$(player.levelCompletion(0)) + " %|"
		'fileOfs = fileOfs + 14 - LEN(a$)
		PUT #1, fileOfs, a$
	CLOSE #1
END SUB


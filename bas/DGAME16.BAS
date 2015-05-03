DECLARE SUB DEBUGOUT (s$)
DECLARE SUB actorListNew (index%)
DECLARE SUB unParent (object%)
DECLARE SUB makeParent (child%, parent%)
DECLARE SUB myPrint (x%, y%, text$, attributes%)
DECLARE SUB screenFakePos (y%)
DECLARE SUB initTestGame ()
DECLARE SUB initMainMenu ()
DECLARE FUNCTION speedDiff% (x0%, x1%)
DECLARE SUB hudTextLoad (offset AS INTEGER, show AS INTEGER)
DECLARE SUB hudUpdate (flags AS INTEGER)
DECLARE SUB addScore (eventType%)
DECLARE FUNCTION collision% (x0%, y0%, w0%, h0%, x1%, y1%, w1%, h1%)
DECLARE SUB damagePlayer ()
DECLARE SUB hudPrint (x%, text$, colour1%, colour2%)
DECLARE SUB aUpdateTile (x%, y%)
DECLARE SUB handleCollision ()
DECLARE SUB screenResetPos (x%, y%)
DECLARE SUB menuLoad (menuNo%)
DECLARE SUB menuClear ()

'$INCLUDE: 'libelf.bi'

DECLARE SUB destroyObject (index%)
DECLARE SUB deltaTime ()
DECLARE SUB elfInit ()
DECLARE SUB handleCamera ()
DECLARE SUB handleScroll ()
DECLARE SUB handleSpawn (wave)
DECLARE SUB loadLevel ()
DECLARE SUB loadSprites (file$, offset%)
DECLARE SUB loadTiles (file$)
DECLARE SUB loadSounds (file$)
DECLARE FUNCTION tileRead (x%, y%)
DECLARE SUB tileWrite (x AS INTEGER, y AS INTEGER, index AS INTEGER)
DECLARE SUB switchAnim (objectIndex%, anim%)
DECLARE SUB spawnObject (x%, y%, object%, spawnerFlags%, spawner)
DECLARE SUB handleBehaviors ()
DECLARE SUB main ()
DECLARE SUB quit ()
DECLARE FUNCTION getSpawnSlot% ()

'DECLARE SUB oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo ()

DEFINT A-Z

DIM SHARED scoreHit AS INTEGER
DIM SHARED scorePickup(10 TO 20) AS INTEGER
DIM SHARED scoreKill(1 TO 40) AS INTEGER

TYPE scoreType
 name AS STRING * 3
 score AS LONG
END TYPE

DIM SHARED spriteClipX(-10 TO 50) AS INTEGER
DIM SHARED spriteClipY(-20 TO 70) AS INTEGER
DIM SHARED spriteClipYofs(-20 TO 70) AS INTEGER

DIM SHARED walkNote(0 TO 7) AS INTEGER

CONST defaultVisX = 25
CONST defaultVisY = 30
CONST playerObj = 2

CONST SndExplode = 44
CONST SndDie = 22
CONST SndDamage = 4
CONST SndGetGemSmall = 92
CONST SndGetGemBig = 59
CONST SndGetHeart = 34
CONST SndGetKey = 132
CONST SndGetChest = 104
CONST SndGetCoin = 92
CONST SndShoot = 44
CONST SndJump = 0
CONST SndLand = 16
CONST SndEnemyDie = 0
CONST SndEggyJumpLo = 100
CONST SndEggyJumpHi = 158
CONST SndSlime = 145
CONST SndEnemyShoot = 173
CONST SndFireBreath = 66
CONST SndHelmetBump = 151
CONST SndEnemyDrop = 163
CONST SndExitLevel = 0
CONST SndEnterLevel = 0
CONST SndTileBounce = 98
CONST SndTileMove = 115
CONST SndTileStop = 127
CONST SndMenuPrev = 87
CONST SndMenuNext = 82
CONST SndPause = 76
CONST ItemKey = 21
CONST ItemGemSmall = 22
CONST ItemGemBig = 23
CONST ItemHeart = 25

'===============================================================================

SUB actorListNew (index)
	'debugOut "actorListNew"
	actor(index).exist = 1
	
	actor(index).lNext = 2
	actor(index).lPrev = actor(2).lPrev
	
	actor(actor(2).lPrev).lNext = index
	actor(2).lPrev = index
	
	'actor(actor(1).lPrev).lNext = index
	'actor(index).lPrev = actor(1).lPrev
	'actor(1).lPrev = index
END SUB

SUB addScore (eventType)
	s = 0
	SELECT CASE eventType
	CASE ItemGemSmall
		s = 100
	CASE ItemGemBig
		s = player.hp * 50
	CASE ItemHeart
		s = 0
		IF player.hp < 4 THEN player.hp = player.hp + 1
		hudUpdate 2
	CASE 998
		s = 15
	END SELECT
	player.score = player.score + s
END SUB

SUB damagePlayer
    IF actor(2).flash = 0 THEN
		player.hp = player.hp + (player.hp > 0)
		actor(2).flash = 40
		aSoundFX 0, 4
		hudUpdate 2
	END IF
END SUB

SUB DEBUGOUT (s$)
	OPEN "debug.txt" FOR APPEND AS #3
		PRINT #3, s$
	CLOSE #3
END SUB

SUB destroyObject (n)
	actor(n).exist = 0
	actor(n).collider = 0
	actor(n).anim = 0
	actor(n).frame = 0
	actor(n).flags = 0
	actor(n).x = 0
	actor(n).y = 0
	actor(n).vecX = 0
	actor(n).vecY = 0

	thisPrev = actor(n).lPrev
	thisNext = actor(n).lNext
	actor(thisNext).lPrev = thisPrev
	actor(thisPrev).lNext = thisNext	

	'debugOut STR$(thisPrev) + "-" + STR$(n) + "-" + STR$(thisNext)
END SUB

'==============================================================================
 REM ______  _______ _     _ _______ _    _ _____  _____   ______ _______
 REM |_____] |______ |_____| |_____|  \  /    |   |     | |_____/ |______
 REM |_____] |______ |     | |     |   \/   __|__ |_____| |    \_ ______|                                                                     
'==============================================================================
 SUB handleBehaviors
	STATIC minY
	STATIC maxY
	STATIC minYs
	STATIC maxYs
	STATIC speedMod
	STATIC friction
	STATIC collisionFloor
	STATIC scoreDelta AS LONG
	STATIC parent
	STATIC advanceAnims
	STATIC dtAnims
	
	entityCounter = 0
	scoreDelta = player.score
	spriteListIndex = 0
	soundEvent = -1
	
	fractionC = (fractionC + 1) MOD 10
'------------------------------------------------------------------------------
	colCount(1) = -1
	colCount(2) = -1
	colCount(3) = -1
	
	dtAnims = dtAnims + 1
	IF dtAnims > 2 THEN
		dtAnims = 0
		advanceAnims = -1
	ELSE
		advanceAnims = 0
	END IF
	
	n = 2
'------------------------------------------------------------------------------
 REM  _____         _______ __   __ _______  ______
 REM |_____] |      |_____|   \_/   |______ |_____/
 REM |       |_____ |     |    |    |______ |    \_
'------------------------------------------------------------------------------                                               
SELECT CASE actor(2).control
'------------------------------------------------------------------------------
'Main menu pan
CASE 263
	handleCamera
	actor(2).count = actor(2).count + 1
	IF actor(2).count > 2 THEN actor(2).control = 258
'------------------------------------------------------------------------------
'Level Clear - Score Count
CASE 262
      pageOfs = gfx.pageOfs(gfx.page)

      SELECT CASE actor(2).jump
       CASE -2 to -1
		actor(2).jump = actor(2).jump + 1
	   CASE 0 TO 1
          c = &H0D0D OR player.levelColor
		  myPrint 2, 22, "TIME BONUS            X 30", c
          myPrint 2, 26, "LIFE BONUS            X 1000", c
          myPrint 2, 30, "KEYS                  /" + str$(level.keys), c
		  myPrint 2, 34, "CHESTS                /" + str$(level.keys), c
		  myPrint 2, 38, "COINS                 /" + str$(level.coins), c
		  c = &H0E0F OR player.levelColor
		  myPrint 20, 30, str$(player.gotKeys), c
		  myPrint 20, 34, str$(player.gotChests), c
		  myPrint 20, 38, str$(player.gotCoins), c
		  
		  a$ = "1000P"
		  IF player.gotKeys = level.keys THEN player.score = player.score + 1000: myPrint 33, 30, a$, c
		  IF player.gotChests = level.keys THEN player.score = player.score + 1000: myPrint 33, 34, a$, c
		  IF player.gotCoins = level.coins THEN player.score = player.score + 1000: myPrint 33, 38, a$, c
		  
		  actor(2).jump = actor(2).jump + 1

       CASE 2 TO 3
          actor(2).jump = actor(2).jump + 1
          player.scoreCounter = 1
          player.scoreCounterInc = 10
          player.scoreCounterTarget = player.time
          player.scoreCounterMode = 0
          player.scoreCounterMul = 300
		  player.scoreDelta = player.time * 30

       CASE 4 TO 6
          w40char.text = "        |"
          a$ = STR$(0& + player.scoreCounter)
          MID$(w40char.text, 8 - LEN(a$), LEN(a$)) = a$
          c = &H0D0D OR player.levelColor
		  aPrint gfx.videoWrap, c, w40char.seg, w40char.ofs, 16, 22 + player.scoreCounterMode * 4, &HB800, pageOfs

          IF player.scoreCounter >= player.scoreCounterTarget THEN
               player.scoreCounter = player.scoreCounterTarget
               actor(2).jump = actor(2).jump + 1
          ELSE
               player.scoreCounter = 0& + player.scoreCounter + player.scoreCounterInc
               player.score = 0& + player.score + player.scoreCounterMul
          END IF
          IF gfx.page THEN soundEvent = 52

       CASE 7 TO 16
          IF actor(2).jump = 7 THEN
              soundEvent = 0
              w40char.text = "         |"
              a$ = STR$(player.scoreDelta)
              MID$(w40char.text, 8 - LEN(a$), LEN(a$)) = a$
          END IF
          c = &H0E0F OR player.levelColor
		  aPrint gfx.videoWrap, c, w40char.seg, w40char.ofs, 30, 22 + player.scoreCounterMode * 4, &HB800, pageOfs
          actor(2).jump = actor(2).jump + 1

       CASE 17
          player.scoreCounterMode = player.scoreCounterMode + 1
          SELECT CASE player.scoreCounterMode
          CASE 1
               player.scoreCounter = 1
               player.scoreCounterInc = 1
               player.scoreCounterTarget = player.hp
               player.scoreCounterMul = 1000
			   player.scoreDelta = player.hp * 1000
               actor(2).jump = 4
			   hudUpdate 4
          CASE 2
               actor(2).jump = 99
               soundEvent = 92
			   hudUpdate 4
          END SELECT

        CASE 99 TO 100
            actor(2).jump = actor(2).jump + 1
			IF player.score > player.levelScore(player.level) THEN
				IF player.levelscore(player.level) > 0 THEN 
					c = &H0E0E OR player.levelColor
					myPrint 8, 43, "YOU IMPROVED YOUR SCORE", c
				END IF
				player.levelScore(player.level) = player.score
			END IF
            c = &H0707 OR player.levelColor
			myPrint 8, 46, "PRESS SPACE TO CONTINUE", c
        
		CASE 101
			IF kbArray(57) THEN actor(2).jump = 999

        CASE 999 TO 1000
			actor(2).jump = actor(2).jump + 1
            actor(2).frame = 0
			actor(3).frame = 0
            tileCopyList(0, gfx.page).x = 0
            tileCopyList(0, gfx.page).y = 0
            tileCopyList(0, gfx.page).w = 40
            tileCopyList(0, gfx.page).h = 50
            tileCopyList(1, gfx.page).y = 255
            aClearList gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), &HDE, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)

        CASE 1001
			CALL aSoundStop
			player.level = 1
			actor(n).control = 259
      END SELECT
 '------------------------------------------------------------------------------              
'Level Clear - fly out of level
CASE 261
               
   IF actor(2).x < 40 THEN actor(2).x = actor(2).x + 1: actor(2).flags = 0
   IF actor(2).x > 40 THEN actor(2).x = actor(2).x - 1: actor(2).flags = 1
   'IF actor(2).y < 22 THEN actor(2).y = actor(2).y + 2
   IF actor(2).y > 22 THEN actor(2).y = actor(2).y - 2

   IF actor(2).x = 40 AND actor(2).y <= 22 THEN
      FOR delObj = 4 TO 60
          actor(delObj).exist = 0
          actor(delObj).control = 0
      NEXT delObj
      actor(3).lNext = 1
	  actor(1).lPrev = 3
	  actor(2).count = 0
      actor(2).jump = -2
      actor(2).dir = 0
      actor(2).control = 262
      player.hudLock = 0
      hudUpdate 255
   END IF

   screenTargetX = actor(2).x - 19
   screenTargetY = actor(2).y - 36
   handleCamera
'------------------------------------------------------------------------------
'Level Clear - sync video pages
CASE 260
	switchAnim n, 5
	screenTargetX = actor(2).x - 19
	screenTargetY = actor(2).y - 36
	handleCamera
    
	IF gfx.screenY(0) = screenTargetY AND gfx.screenY(1) = screenTargetY AND gfx.screenX(0) = screenTargetX AND gfx.screenX(1) = screenTargetX THEN 
		actor(2).count = actor(2).count + 1
	ELSE
		actor(2).count = 0
	END IF
	IF actor(2).count > 2 THEN
		screenFakePos 80
		DEF SEG = tileBufferSeg
	FOR n = 0 TO 199
		writeOfs = 0& + TileMapOfs + n
		POKE writeOfs, 0
	NEXT n
		DEF SEG
		soundEvent = 220
		actor(2).control = 261
	END IF
'------------------------------------------------------------------------------			   
'Menu
CASE 258
	'
	' PLAYER: MENU
	'
	debugOut "Menu mode active."
	SELECT CASE player.menuMode
	CASE 0		'Display menu load message
		debugOut " menu: Set actor list"
		'w40char.text = " LOADING PAGE ...                      |"
		player.hudLock = 999
		'aPrint &HFFFF, &HC0E, w40char.seg, w40char.ofs, 0, 0, hudBufferSeg, hudBufferOfs
		'player.menuMode = 1
		actor(2).jump = actor(1).lNext
		actor(1).lNext = 1
	'CASE 1		'Load and display menu
		debugOut " menu: soundstop"
		aSoundStop
		debugOut " menu: scroll"
		handleScroll
		debugOut " menu: loadmenu"
		menuLoad player.menuContext - 1
		debugOut " menu: copypage"
		aCopyPage gfx.pageOfs(gfx.page), gfx.pageOfs(gfx.page xor 1)
		debugOut " menu: infotext"
		w40char.text = " ARROW KEYS TO MOVE .. ENTER TO SELECT |"
		aPrint &HFFFF, &H0E0E, w40char.seg, w40char.ofs, 0, 0, hudBufferSeg, hudBufferOfs
		player.hudLock = 999
		'player.menuMode = 2
		FOR c = 0 to 31
			spriteList(c, 0).y = 255
			spriteList(c, 1).y = 255
		NEXT c
		debugOut " menu: pf"
		aPageFlip gfx.videoWrap, hudBufferSeg, hudBufferOfs, gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
		gfx.page = gfx.page XOR 1
		aCopyPage gfx.pageOfs(gfx.page), gfx.pageOfs(gfx.page xor 1)
		player.menuMode = 2
		player.menuHLcolor = &HCECF
		soundEvent = SndPause
		debugOut "Menu display complete."
	CASE 2		'Item highlight and wait for input
		IF actor(2).count > 0 THEN
			IF actor(2).count > 3 THEN
				nn = menuItem(player.menuPrev)
				x = actor(nn).x
				y = actor(nn).y
				w = actor(nn).vecX
				h = actor(nn).vecY
				aMenuHiLite x, y, w, h, actor(nn).frame, gfx.pageOfs(gfx.page)
				player.menuHLcolor = &H6E6E
			END IF
			IF actor(2).count < 5 THEN
				IF kbArray(80) = 0 AND kbArray(72) = 0 THEN actor(2).count = 1
			END IF
			actor(2).count = actor(2).count - 1
			player.menuHLcolor = &HCECF
		ELSE
			player.menuPrev = player.menuItem
			'IF kbArray(player.keyRelease) = 0 THEN
			'	player.keyRelease = 0
				nn = menuItem(player.menuItem)
				IF kbArray(80) THEN 
					IF player.menuItem <> actor(nn).lNext THEN
						player.menuItem = actor(nn).lNext
						soundEvent = sndMenuNext
						actor(2).count = 5
						player.menuHLcolor = &H4747
						'player.KeyRelease = 80
					END IF
				END IF
				IF kbArray(72) THEN 
					IF player.menuItem <> actor(nn).lPrev THEN
						player.menuItem = actor(nn).lPrev
						soundEvent = sndMenuPrev
						actor(2).count = 5
						player.menuHLcolor = &H4747
						'player.KeyRelease = 72
					END IF
				END IF
				IF kbArray(28) THEN player.menuMode = 5
			'END IF
		END IF
		nn = menuItem(player.menuItem)
		x = actor(nn).x
		y = actor(nn).y
		w = actor(nn).vecX
		h = actor(nn).vecY
		aMenuHiLite x, y, w, h, player.menuHLcolor, gfx.pageOfs(gfx.page)
	'--------------------------------------------------------------------------
	CASE 5								'MENUCONTEXT Process selected menu item
		player.menuMode = 2
		SELECT CASE player.menuContext
		'----------------------------------------------------------------------
		CASE 1 TO 2	'Pause menu
				SELECT CASE player.menuItem
				CASE 1
					player.menuMode = 999
					player.menuContext = 0
				CASE 4	'quit to main menu
					player.menuContextPrev = player.menuContext
					player.menuContext = 5
					player.menuMode = 999
				CASE 5	'quit to dos
					player.menuContextPrev = player.menuContext
					player.menuContext = 4
					player.menuMode = 999
				END SELECT
		'----------------------------------------------------------------------
		CASE 3	'Death menu
				SELECT CASE player.menuItem
				CASE 1	'no
					player.score = 0
					player.menuContext = -1
					player.menuMode = 999
				CASE 2	'try again
					player.level = 1
					player.menuContext = -1
					player.menuMode = 999
				END SELECT
		'----------------------------------------------------------------------
		CASE 4	'Quit to dos
				SELECT CASE player.menuItem
				CASE 1	'no
					player.menuContext = player.menuContextPrev
					player.menuContextPrev = 0
					player.menuMode = 999
				CASE 2	'yes
					actor(2).control = 259
					player.level = -1
				END SELECT
		'----------------------------------------------------------------------
		CASE 5	'Quit to main menu
				SELECT CASE player.menuItem
				CASE 1	'no
					player.menuContext = player.menuContextPrev
					player.menuContextPrev = 0
					player.menuMode = 999
				CASE 2	'yes
					player.level = 0
					player.menuContext = -1
					player.menuMode = 999
				END SELECT
		'----------------------------------------------------------------------
		CASE 10 'Main menu
				SELECT CASE player.menuItem
				CASE 1	'new game
					player.level = 1
					player.menuContext = -1
					player.menuMode = 999
				CASE 3
					player.menuContextPrev = player.menuContext
					player.menuContext = 11
					player.menuMode = 999
				CASE 5	'quit to dos
					player.menuContextPrev = player.menuContext
					player.menuContext = 4
					player.menuMode = 999
				END SELECT
		CASE 11	'Help
			SELECT CASE player.menuItem
				CASE 1	'back
					player.menuContext = player.menuContextPrev
					player.menuContextPrev = 0
					player.menuMode = 999
				END SELECT
		END SELECT
	'--------------------------------------------------------------------------
	CASE 999 to 1000		' Exit menu: clear screen
		tileCopyList(0, gfx.page).x = 0
		tileCopyList(0, gfx.page).y = 0
		tileCopyList(0, gfx.page).w = 40
		tileCopyList(0, gfx.page).h = 50
		tileCopyList(1, gfx.page).y = 255
		aClearList gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), &HDE, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)
		aRectList gfx.WindowOfs(gfx.page), gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), tileBufferWrap, tileBufferSeg, tileBufferOfs, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)
		player.menuMode = player.menuMode + 1
	CASE 1001
		actor(1).lNext = actor(2).jump
		menuClear
		IF player.menuContext > 0 THEN
			player.menuMode = 0
			EXIT SUB
		END IF
		IF player.menuContext = 0 THEN
			actor(2).control = 255
			player.menuMode = 0
			player.hudLock = 0
			hudUpdate 255
		END IF
		IF player.menuContext < 0 THEN
			actor(2).control = 259
			player.menuMode = 0
			EXIT SUB
		END IF
	END SELECT
	
'Wait after death
CASE 256
	'actor(2).count = actor(2).count + 1	
	IF kbArray(57) THEN 
		actor(2).count = 0
		player.menuContext = 3
		player.menuMode = 0
		actor(2).control = 258
		debugOut "Switch to menu mode after player death."
	END IF
'------------------------------------------------------------------------------
'Normal player
CASE 255
	minY = -30

	c = tileRead(actor(2).x, actor(2).y)

	IF c > 29 THEN
		speedMod = 8
		friction = 5
		actor(2).y = (actor(2).y AND &HFFF8)
		screenTargetYlock = actor(2).y - 46

		IF kbArray(29) = 0 THEN player.jumpRelease = -1

		collisionFloor = 2
		player.Bouncing = 0
		SELECT CASE c
		CASE 52
			spawnObject actor(2).x AND &HFFFC, actor(2).y AND &HFFF8, 99, 2, -5
			player.vecY = -38
			actor(2).y = actor(2).y - 2
			player.Bouncing = 30
			collisionFloor = 0
			maxY = 38
			player.jumpRelease = 0
			soundEvent = 98
		END SELECT
		IF player.vecY > 0 THEN soundEvent = 16: player.vecY = 0
		screenTargetY = screenTargetYlock
	ELSE
		collisionFloor = collisionFloor + (collisionFloor > 0)
		maxY = 30
		speedMod = 10
		friction = 5
		IF player.ShootDelay < 6 THEN switchAnim n, 5
		IF kbArray(29) THEN
			player.vecY = player.vecY + 2
			maxY = 10
		ELSE
			player.jumpRelease = -1
			IF player.Bouncing > 0 THEN
				maxY = 38
				player.Bouncing = player.Bouncing - 1
				player.vecY = player.vecY + 2 - gfx.page
			ELSE
				player.vecY = player.vecY + 15
			END IF
		END IF
	END IF

	IF actor(2).y - screenTargetYlock > 50 THEN 
		screenTargetYlock = -1
	END IF

'	'IF c = 27 OR c = 26 THEN
'		IF kbArray(29) = 0 THEN player.jumpRelease = -1
'			collisionFloor = 1
'			maxY = 5
'			IF c = 27 THEN minY = -15
'			friction = 1
'			speedMod = 4
'		END IF
'	END IF

    IF actor(2).exist > 1 THEN
		IF kbArray(29) = 0 THEN player.jumpRelease = -1
		screenTargetYlock = -1
		collisionFloor = 1
		player.vecY = 0
		screenTargetY = actor(2).y - 30
    END IF
   
    IF collisionFloor THEN
		IF player.vecX = 0 AND player.ShootDelay < 6 THEN 
			actor(2).frame = 1
			actor(2).anim = 1
		END IF
		IF kbArray(29) AND player.jumpRelease THEN
			player.jumpRelease = 0
			player.vecY = -35
			actor(n).Y = actor(n).Y - 2
			soundEvent = 0
		END IF
	END IF

   c = tileRead(actor(n).x, actor(n).y - 7)
   IF c > 29 THEN
      actor(n).y = (actor(n).y AND &HFFF8) + 7
      player.vecY = 0
   END IF

   IF c = 28 THEN
      player.gotCoins = player.gotCoins + 1
	  addScore 998
      soundEvent = 92
      x = (actor(2).x) AND &HFFFC
      y = (actor(2).y - 7) AND &HFFF8
      tileWrite x, y, 0
      aUpdateTile x, y
      spawnObject x, y, 99, 0, 0
   END IF

   d = player.vecX
   IF d > 0 THEN
      IF friction > d THEN friction = d
      friction = -friction
      player.vecX = player.vecX + friction
   END IF
   IF d < 0 THEN
      IF -friction < d THEN friction = -d
      player.vecX = player.vecX + friction
   END IF

   IF kbArray(75) THEN
      player.vecX = player.vecX - speedMod
      actor(n).flags = 1
      IF collisionFloor THEN
         switchAnim n, 2
         'IF fraction(2, fractionC) THEN
         '   'sounds.queue(sounds.readPos) = walkNote(player.walkCounter)
         '   player.walkCounter = (player.walkCounter + 1) AND 7
         'END IF
      END IF
   END IF

   IF kbArray(77) THEN
      player.vecX = player.vecX + speedMod
      actor(n).flags = 0
      IF collisionFloor THEN
         switchAnim n, 2
         'IF fraction(2, fractionC) THEN
         '   'sounds.queue(sounds.readPos) = walkNote(player.walkCounter)
         '   player.walkCounter = (player.walkCounter + 1) AND 7
         'END IF
      END IF
   END IF

   IF kbArray(80) THEN
      screenTargetY = actor(2).y - 24
   END IF

   player.ShootDelay = player.ShootDelay + (player.ShootDelay > 0)

   IF kbArray(56) AND player.ShootDelay = 0 THEN
      player.ShootDelay = 10
      spawnObject actor(2).x, actor(2).y - 6, 100, actor(n).flags, 0
      soundEvent = 55
      switchAnim n, 8
   END IF

   IF player.hp = 0 THEN
      actor(2).exist = 1
	  actor(2).parentDX = 0
	  actor(2).parentDY = 0
	  actor(2).collider = 0
      actor(2).control = 256
      actor(2).count = 0
	  actor(2).frame = 0
	  actor(3).frame = 0
      actor(2).vecX = 0
      actor(2).vecY = 0
      FOR b = 0 TO 63 STEP 8
         xs = angle.x(b) \ 2
         ys = angle.y(b) \ 2
         spawnObject actor(2).x, actor(2).y, 257, xs, ys
      NEXT b
      aSoundFX 4, 22
      aSoundFX 14, 23
      'hudTextLoad &H140, 0
      player.hudLock = 999
      player.score = 0
      'actor(2).x = 0
      'actor(2).y = 0
      player.jumpRelease = -1
   END IF

   IF player.vecX > 10 THEN player.vecX = 10
   IF player.vecX < -10 THEN player.vecX = -10
   IF player.vecY > maxY THEN player.vecY = maxY
   IF player.vecY < minY THEN player.vecY = minY

   actor(n).x = actor(n).x + fraction(player.vecX, fractionC)
   actor(n).y = actor(n).y + fraction(player.vecY, fractionC)

   c = tileRead(actor(n).x + 2, actor(n).y - 4)
   IF c > 29 THEN
       actor(n).x = (actor(n).x AND &HFFFC) + 2
   END IF
   c = tileRead(actor(n).x - 2, actor(n).y - 4)
   IF c > 29 THEN
       actor(n).x = (actor(n).x AND &HFFFC) + 2
   END IF

   screenTargetX = actor(n).x - 20
   handleCamera
   IF screenTargetYlock = -1 THEN
      screenTargetY = actor(2).y - 28
   END IF
END SELECT

playerX = actor(2).x
playerY = actor(2).y

actor(3).flags = actor(2).flags
actor(3).flash = actor(2).flash

screenX = gfx.screenX(gfx.page)
screenY = gfx.screenY(gfx.page)

listIDX = 1

DO

	n = actor(listIDX).lNext
	'IF n = 0 THEN quit
	listIDX = n
	
	'debug
	IF actor(2).control = 258 THEN
		debugOut str$(n)
	END IF
	
	
	'------------------------------------------------------------------------------
	 REM  _____  _______ _     _ _______  ______              
	 REM |     |    |    |_____| |______ |_____/              
	 REM |_____|    |    |     | |______ |    \_              
	
	 REM  _____  ______  _____ _______ _______ _______ _______
	 REM |     | |_____]   |   |______ |          |    |______
	 REM |_____| |_____] __|   |______ |_____     |    ______|
	'------------------------------------------------------------------------------
	IF actor(n).exist > 0 THEN

	entityCounter = entityCounter + 1

	visDeltaX = ABS(screenX + 20 - actor(n).x)
	visDeltaY = ABS(screenY + 24 - actor(n).y)

	IF visDeltaX < actor(n).visX AND visDeltaY < actor(n).visY THEN
		IF actor(n).exist > 1 THEN
			parent = actor(n).exist
			'
			'Add parent delta transform.
			' and store current parent transform for next delta.
			'
			actor(n).x = actor(n).x + (actor(parent).x - actor(n).parentDX)
			actor(n).y = actor(n).y + (actor(parent).y - actor(n).parentDY)
			actor(n).parentDX = actor(parent).x
			actor(n).parentDY = actor(parent).y
		END IF
	  
	'Add speed vector to transform.
	'
	actor(n).x = actor(n).x + fraction(actor(n).vecX, fractionC)
	actor(n).y = actor(n).y + fraction(actor(n).vecY, fractionC)
    '
    IF actor(n).y < 8 THEN actor(n).y = 8
    IF actor(n).y > 630 THEN actor(n).y = 630
    IF actor(n).x < -7 THEN actor(n).x = -7
    IF actor(n).x > 85 THEN actor(n).x = 85

	'------------------------------------------------------------------------------
	'
	'	All frames behavior checks:
	'
	'------------------------------------------------------------------------------
	SELECT CASE actor(n).control
	
	'SPIKE BALL U, O-, O+
	CASE 17 TO 19
        SELECT CASE actor(n).control
		CASE 17
			c = angle.y(dtMain0) + 32
		CASE 18	
			c = 63 - dtMain0
		CASE 19
			c = dtMain0
		END SELECT
		actor(n).x = actor(n).count + mulTable(actor(n).jump, angle.x(c))
        actor(n).y = actor(n).dir + mulTable(actor(n).jump, angle.y(c))
	
	'Platform / elevator
    CASE 50
		playerDeltaX = ABS(actor(n).x - playerX)		

		IF actor(2).exist <> n THEN
			playerDeltaY = actor(n).y - playerY
			IF playerDeltaX < 6 AND playerDeltaY < 1 AND playerDeltaY > -4 AND player.vecY > -1 THEN
				actor(n).jump = 1
				soundEvent = 115
				actor(2).y = actor(n).y
				makeParent 2, n
			END IF
		ELSE
			playerDeltaY = ABS(actor(n).y - playerY)
			IF playerDeltaX > 5 OR playerDeltaY > 2 OR player.vecY < -5 THEN 
				unParent 2
				actor(n).jump = 0
			END IF				
        END IF
		
		' Platform type specific events
		c = tileRead(actor(n).x, actor(n).y)
		SELECT CASE actor(n).count
		' Horizontal
		CASE 50
			IF actor(n).dir THEN
				actor(n).dir = actor(n).dir - 1
			ELSE
				IF c THEN actor(n).vecX = -actor(n).vecX
				actor(n).dir = 16
			END IF
		' Vertical
		CASE 51
			IF actor(n).jump THEN
				actor(n).vecY = -10
				IF c = 1 THEN actor(n).vecY = 0
			ELSE
				actor(n).vecY = 0
				IF actor(n).y < level.spawner(actor(n).spawner).y - 2 THEN actor(n).vecY = 20
				IF actor(n).y > level.spawner(actor(n).spawner).y + 2 THEN actor(n).vecY = -20
			END IF
		END SELECT
		
	'Eggy
	CASE 1
		speed = 5
		SELECT CASE actor(n).count
		CASE 0 
			soundEvent = SndEggyJumpHi
			actor(n).vecY = -30
			actor(n).frame = 16
			actor(n).count = actor(n).count + 1
		CASE 5
			soundEvent = SndEggyJumpLo
			actor(n).vecY = -20
			actor(n).frame = 16
			actor(n).count = actor(n).count + 1
		CASE 10
			soundEvent = SndEggyJumpLo
			actor(n).vecY = -20
			actor(n).frame = 16
			actor(n).count = actor(n).count + 1
		CASE 15
			actor(n).count = -1
		END SELECT
		IF actor(n).vecY > 0 THEN
			c = tileRead(actor(n).x, actor(n).y)
			IF c > 29 THEN
				actor(n).frame = 17
				actor(n).y = actor(n).y AND &HFFF8
				actor(n).vecY = 0
				actor(n).count = actor(n).count + 1
				speed = 0
			END IF
		ELSE
			c = tileRead(actor(n).x, actor(n).y - 6)
			IF c > 29 THEN
				actor(n).y = actor(n).y AND &HFFF8 + 6
				actor(n).vecY = 0
			END IF
		END IF
		IF actor(n).vecY < 20 THEN
			actor(n).vecY = actor(n).vecY + 2
		END IF	
		
		IF actor(n).flags THEN
			actor(n).vecX = -speed
			s = -2
			p = 1
		ELSE
			actor(n).vecX = speed
			s = 2
			p = 3
		END IF
		c = tileRead(actor(n).x + s, actor(n).y - 2)
		IF c > 29 THEN
			actor(n).vecX = 0
			actor(n).flags = actor(n).flags XOR 1
			actor(n).x = (actor(n).x AND &HFFFC) + p
		END IF
	
	'Player Bullet
	CASE 100
		c = tileRead(actor(n).x, actor(n).y)
		IF c > 29 THEN
			IF c > 54 THEN
				actor(n).x = actor(n).x AND &HFFFC
				actor(n).y = actor(n).y AND &HFFF8
				SELECT CASE C
				'hits bonus blocks
				CASE 55					
					soundEvent = 122
					'a = 38
					t = 54
					actor(n).vecY = 0
					IF player.hp > 1 THEN
						o = 23
					ELSE
						o = 25
					END IF
					spawnObject actor(n).x + 2, actor(n).y, 41, o, 0
					actor(n).frame = 39
				'hits mover block
				CASE 56					
					soundEvent = 115
					IF actor(n).vecX < 0 THEN
						s = -10
					ELSE
						s = 10
					END IF
					'a = 38
					spawnObject actor(n).x, actor(n).y, 99, 3, s
					actor(n).frame = 38
				'hits destructibles
				CASE 57 TO 59			
					soundEvent = 44
					'a = 0
					'spawnObject actor(n).x, actor(n).y, 99, 2, 5
					actor(n).vecY = 0
					actor(n).frame = 38
					t = 0
					IF c = 58 THEN t = 28
				END SELECT
				tileWrite actor(n).x, actor(n).y, t
				aUpdateTile actor(n).x, actor(n).y
				actor(n).vecX = 0
				actor(n).flags = 0
				actor(n).control = 0
				actor(n).life = 3
				actor(n).collider = 0
			ELSE
				soundEvent = 44
				actor(n).flags = 0
				actor(n).control = 0
				actor(n).collider = 0
				actor(n).life = 2
				actor(n).frame = 34
				actor(n).vecY = -5
				actor(n).vecX = 0
				soundEvent = 44
			END IF
		END IF
		IF visDeltaX > 30 THEN actor(n).life = 0
     
	'Enemy bullet
	CASE 102
		playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)
		IF playerDeltaX < 3 AND playerDeltaY < 10 THEN
			damagePlayer
			actor(n).life = 0
		END IF

	'Drip
	CASE 104
		playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)
		IF actor(n).vecX > 0 THEN
			actor(n).vecX = actor(n).vecX - 1
		ELSEIF actor(n).vecX < 0 THEN
			actor(n).vecX = actor(n).vecX + 1
		END IF
		IF actor(n).vecY < 30 THEN actor(n).vecY = actor(n).vecY + 3
		IF playerDeltaX < 3 AND playerDeltaY < 6 THEN
			damagePlayer
			actor(n).flash = 16
			actor(n).life = 0
			actor(n).control = 0
			actor(n).vecY = -10
		END IF

	'Fire Bomb
	CASE 112
		playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)    
		IF actor(n).life < 15 THEN
			c = tileRead(actor(n).x, actor(n).y)
			IF c > 29 THEN
				actor(n).life = 12
				actor(n).count = 10
				actor(n).control = 0
				actor(n).vecY = -5
				actor(n).y = actor(n).y AND &HFFF8
				actor(n).flash = 12
				soundEvent = 44
			END IF
		END IF
		IF actor(n).vecY < 25 THEN actor(n).vecY = actor(n).vecY + 2
		IF playerDeltaX < 4 AND playerDeltaY < 6 THEN
			damagePlayer
			actor(n).life = 0
		END IF		
	END SELECT	
'------------------------------------------------------------------------------
'
'	Every-other-frame behavior checks
'
'------------------------------------------------------------------------------  
	IF gfx.page THEN
	SELECT CASE actor(n).control
      CASE 99
      'Tile FX
          IF actor(n).vecX < 0 THEN
               t = actor(n).x
          ELSE
               t = actor(n).x + 4
          END IF
          c = tileRead(t, actor(n).y)

          IF c > 29 THEN
             IF actor(n).vecX < 0 THEN actor(n).x = actor(n).x + 4
             actor(n).x = actor(n).x AND &HFFFC
             actor(n).y = actor(n).y AND &HFFF8
             tileWrite actor(n).x, actor(n).y, 56
             aUpdateTile actor(n).x, actor(n).y
             actor(n).life = 4
             actor(n).control = 0
             actor(n).vecX = 0
             actor(n).vecY = 0
             soundEvent = 127
          END IF
       
	'Dropping particle
	CASE 200
		IF actor(n).vecY < 20 THEN actor(n).vecY = actor(n).vecY + 4
       
	'Pickup
	CASE 41
		playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)
	
		c = tileRead(actor(n).x, actor(n).y)
		IF actor(n).vecY < 30 THEN actor(n).vecY = actor(n).vecY + 4
            
		IF c > 29 AND actor(n).y > actor(n).count THEN
            IF actor(n).vecY > 15 THEN
				actor(n).y = actor(n).y AND &HFFF8
				actor(n).vecY = (NOT actor(n).vecY) \ 2
				soundEvent = 32
            ELSE
				actor(n).vecY = 0
            END IF
		END IF

		IF actor(n).life < 24 AND actor(n).frame <> 21 THEN actor(n).flash = 36

		IF playerDeltaX < 5 AND ABS(actor(2).y - actor(n).y) < 8 THEN
            level.spawner(actor(n).spawner).object = 0
            soundEvent = 110
            IF actor(n).frame = 21 THEN
				player.score = player.score + 500
				player.hasKey = player.hasKey + 1
				player.gotKeys = player.gotKeys + 1
				actor(n).control = 0
				actor(n).vecX = 0
				actor(n).vecY = -15
				actor(n).life = 6
				actor(n).flash = 16
				hudUpdate 16
				soundEvent = 132
			ELSE
				addScore actor(n).frame
				actor(n).y = actor(n).y - 3
				actor(n).vecY = 0
				actor(n).flash = 0
				actor(n).life = 5
				actor(n).control = 0
				actor(n).frame = 30
				if actor(n).frame = 25 THEN soundEvent = 110
			END IF
		END IF

	'Chest
	CASE 45
		playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)
		
		IF playerDeltaX < 5 AND playerDeltaY < 10 AND player.hasKey THEN
			player.score = player.score + 1000
			player.gotChests = player.gotChests + 1
			player.hasKey = player.hasKey - 1
			spawnObject actor(n).x - 5, actor(n).y - 12, 41, 22, 0
			spawnObject actor(n).x - 3, actor(n).y - 14, 41, 22, 0
			spawnObject actor(n).x, actor(n).y - 16, 41, 23, 0
			spawnObject actor(n).x + 3, actor(n).y - 15, 41, 22, 0
			spawnObject actor(n).x + 5, actor(n).y - 14, 41, 22, 0
			actor(n).control = 0
			actor(n).life = 3
			actor(n).frame = 0
			'actor(n).vecY = -5
			soundEvent = SndGetChest
			level.spawner(actor(n).spawner).object = 0
			hudUpdate 16
		END IF
       
	'HELMET
	CASE 2
		c = tileRead(actor(n).x, actor(n).y + 1) > 29
        IF c THEN
			actor(n).y = actor(n).y AND &HFFF8
			actor(n).vecY = 0
        ELSE
			'actor(n).vecX = 0
			actor(n).vecY = 20
        END IF

		IF actor(n).life > -50 THEN
			actor(n).y = actor(n).y - 6
			actor(n).life = -50
			actor(n).count = 16
			actor(n).collider = 2
			actor(n).visY = 50
			'actor(n).dir = actor(n).vecX
			'actor(n).vecX = 0
        END IF
        
		IF actor(n).count = 0 THEN 
			speed = 4
		ELSE
			speed = 10
			IF c THEN actor(n).count = actor(n).count - 1
			actor(n).frame = 75
			IF actor(n).count = 0 THEN
				actor(n).frame = 72
				actor(n).collider = 1
				actor(n).visY = 35
				'actor(n).vecX = actor(n).dir
			END IF
		END IF
		
		IF actor(n).flags THEN
			xp = -2
			flags = 0
			speed = -speed
		ELSE
			xp = 2
			speed = speed
			flags = 1
		END IF
		actor(n).vecX = speed

		c = tileRead(actor(n).x + xp, actor(n).y - 2)
		c2 = tileRead(actor(n).x + xp, actor(n).y + 2)
		IF actor(n).count THEN c2 = 999
		IF c > 29 OR c2 < 30 THEN
			IF actor(n).count THEN soundEvent = SndHelmetBump
			actor(n).flags = flags
			actor(n).vecX = -speed
			actor(n).x = actor(n).x AND &HFFFC + 2
		END IF
		
	'Dragon
	CASE 3
		actor(n).count = actor(n).count + 1
		IF actor(n).count < 32 THEN
			c = actor(2).x - 15
		ELSE
			c = actor(2).x + 15
			actor(n).count = actor(n).count AND 63
		END IF
		IF actor(n).x < c THEN actor(n).vecX = actor(n).vecX + 2
        IF actor(n).x > c THEN actor(n).vecX = actor(n).vecX - 2
        
        IF actor(n).y < actor(2).y - 24 THEN actor(n).vecY = actor(n).vecY + 2
        IF actor(n).y > actor(2).y - 19 THEN actor(n).vecY = actor(n).vecY - 3
        IF actor(n).vecY < -10 THEN actor(n).vecY = -10
        IF actor(n).vecY > 10 THEN actor(n).vecY = 10
        IF actor(n).vecX < -5 THEN actor(n).vecX = -5
        IF actor(n).vecX > 5 THEN actor(n).vecX = 5
        IF playerDeltaX < 6 THEN
			IF actor(n).jump < 1 THEN
				spawnObject actor(n).x, actor(n).y + 1, 112, 0, 0
				actor(n).jump = 24
				soundEvent = 173
			END IF
		END IF
		actor(n).jump = actor(n).jump + (actor(n).jump > 0)

	'Drooly
	CASE 4
		IF actor(n).vecX THEN
			c = tileRead(actor(n).x + actor(n).vecX, actor(n).y + 1)
			IF c < 30 THEN actor(n).vecX = -actor(n).vecX 
			c = tileRead(actor(n).x + actor(n).vecX, actor(n).y - 2)
			IF c > 29 THEN actor(n).vecX = -actor(n).vecX
		END IF
        actor(n).jump = actor(n).jump + 1
        IF actor(n).jump = 15 THEN
			actor(n).count = actor(n).vecX
			actor(n).vecX = 0
			xp = INT(RND * 30) - 15
			spawnObject actor(n).x + 1, actor(n).y + 2, 104, xp, 0
			soundEvent = 145
        END IF
		IF actor(n).jump > 25 THEN
			actor(n).jump = 0
			actor(n).vecX = actor(n).count
        END IF
		IF actor(n).life = 0 THEN
			actor(n).count = actor(n).vecX
			actor(n).vecX = 0
			spawnObject actor(n).x - 1, actor(n).y - 5, 104, -15, 0
			spawnObject actor(n).x + 1, actor(n).y - 5, 104, 15, 0
        END IF

	'Babat
	CASE 5
        playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)
		
		actor(n).jump = actor(n).jump + 1
         
         SELECT CASE actor(n).count
         CASE 0
            IF playerDeltaX < 16 AND playerDeltaY < 32 AND actor(2).y > actor(n).y OR actor(n).life > -2 THEN
             actor(n).count = 1
             actor(n).frame = 86
             actor(n).jump = actor(2).x
            END IF

         CASE 1
            IF actor(n).y < actor(2).y THEN
               actor(n).vecY = 5
               IF actor(n).x < actor(2).x THEN actor(n).vecX = actor(n).vecX + 1
               IF actor(n).x > actor(2).x THEN actor(n).vecX = actor(n).vecX - 1
            ELSE
              actor(n).vecY = 10
              actor(n).vecX = 0
              actor(n).count = 2
              actor(n).jump = actor(2).y - 32
              actor(n).dir = actor(2).x
            END IF
         
         CASE 2
          IF actor(n).y > actor(n).jump + 5 THEN
            actor(n).vecY = actor(n).vecY - 5
           ELSE
            actor(n).count = 3
            actor(n).jump = 0
          END IF

         CASE 3
          actor(n).jump = actor(n).jump + 1
          actor(n).vecY = mulTable(2, actor(n).vecY)
          IF actor(n).jump > 16 THEN actor(n).count = 1
         END SELECT

         IF actor(n).vecY > 20 THEN actor(n).vecY = 20
         IF actor(n).vecY < -20 THEN actor(n).vecY = -20
         IF actor(n).vecX > 10 THEN actor(n).vecX = 10
         IF actor(n).vecX < -10 THEN actor(n).vecX = -10
       
	'SKULL
	CASE 6         
		IF actor(n).flags THEN
			actor(n).vecX = -8
			IF actor(n).x < screenX - 4 THEN actor(n).flags = 0 ' soundEvent = 151
		ELSE
			actor(n).vecX = 8
			IF actor(n).x > screenX + 44 THEN actor(n).flags = 1 ' soundEvent = 151
		END IF
		c = (dtMain0 + actor(n).count) AND 63
		actor(n).y = actor(n).count + mulTable(1, angle.y(c))
	
	'SPIKY
	CASE 8 
		IF actor(n).count = 0 THEN
			SELECT CASE actor(n).dir
			CASE 0
				frontX = 0
				frontY = -2
				leftX = -2
				leftY = 0
			CASE 1
				frontX = 1
				frontY = 0
				leftX = 0
				leftY = -4
			CASE 2
				frontX = 0
				frontY = 2
				leftX = 2
				leftY = 0
			CASE 3
				frontX = -1
				frontY = 0
				leftX = 0
				leftY = 4
			END SELECT				
			IF actor(n).jump THEN
				frontX = -frontX
				leftX = -leftX
				'frontY = -frontY
			END IF
			c = tileRead(actor(n).x + frontX, actor(n).y + frontY)
			c2 = tileRead(actor(n).x + leftX, actor(n).y + leftY)
			IF c > 29 THEN
				actor(n).dir = (actor(n).dir + 1) AND 3
				actor(n).count = 2
			ELSE
				IF c2 < 30 THEN
					actor(n).dir = (actor(n).dir - 1) AND 3
					actor(n).count = 2
				END IF
			END IF
			SELECT CASE actor(n).dir
			CASE 0
				actor(n).vecX = 0
				actor(n).vecY = -10
			CASE 1
				actor(n).vecX = 5
				actor(n).vecY = 0
			CASE 2
				actor(n).vecX = 0
				actor(n).vecY = 10
			CASE 3
				actor(n).vecX = -5
				actor(n).vecY = 0
			END SELECT				
			IF actor(n).jump THEN
				actor(n).vecX = -actor(n).vecX
				'actor(n).vecY = -actor(n).vecY
			END IF
		ELSE
			actor(n).count = actor(n).count - 1
		END IF
			
	'FIRE BREATH
	CASE 11
		 x = (dtMain1 + actor(n).y) AND 31
		 IF x > 10 THEN
          IF x = 11 THEN
           actor(n).frame = 48: soundEvent = 66
          END IF
          IF x = 15 THEN
           spawnObject actor(n).x + actor(n).dir, actor(n).y, 111, actor(n).flags, -10
           spawnObject actor(n).x + actor(n).dir, actor(n).y, 111, actor(n).flags, 0
           spawnObject actor(n).x + actor(n).dir, actor(n).y, 111, actor(n).flags, 10
          END IF
         ELSE
           actor(n).frame = 47
         END IF

    'SPIKE BALL BOUNCE
    CASE 15
        c = tileRead(actor(n).x, actor(n).y + 6)
        IF c > 29 THEN
          actor(n).y = actor(n).y AND &HFFF8 - 6
          actor(n).vecY = -30
          aSoundNote 40
         ELSE
          actor(n).vecY = actor(n).vecY + 6
          IF actor(n).vecY > 20 THEN actor(n).vecY = 20
        END IF

	'CHECKPOINT
	CASE 62
        playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)
		
		IF playerDeltaY < 32 THEN
			check = 0
			IF player.wave = actor(n).flags THEN
				IF player.wavePrev < actor(n).flags THEN
					player.wavePrev = player.wave
					player.wave = actor(n).flags + 1
					check = 1
				END IF
			ELSE
				IF player.wave > actor(n).flags THEN
					IF player.wavePrev > actor(n).flags THEN
						player.wavePrev = player.wave
						player.wave = actor(n).flags
						check = 1
					END IF
				END IF
			END IF
			IF check THEN
				level.spawner(actor(n).spawner).object = 62
				FOR nn = 4 TO 60
					IF ABS(actor(nn).y - actor(2).y) > 240 THEN
						IF actor(nn).control > 0 AND actor(nn).control < 63 THEN
							'spawnerEnabler(actor(nn).spawner) = spawnerEnabler(actor(nn).spawner) AND &HFFF
							level.spawner(actor(nn).spawner).object = level.spawner(actor(nn).spawner).object AND &HFFF
						END IF
						actor(nn).control = 0
						actor(nn).life = 0
					END IF
				NEXT nn
				handleSpawn player.wave
			END IF
		END IF

	'EXIT
	CASE 63
		actor(n).frame = 60
		
		IF (dtMain1 AND 3) = 2 THEN
			x = actor(n).x - 4 + INT(RND * 10)
			y = actor(n).y - 12 + INT(RND * 12)
			spawnObject x, y, 93, 0, 3
		END IF
		
		c = actor(n).spawner
		actor(n).y = level.spawner(c).y + mulTable(2, angle.x(dtMain0))
		
		playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)
		IF playerDeltaX < 6 AND playerDeltaY < 10 THEN
			actor(2).control = 260
			actor(2).count = 0
			actor(n).control = 0
			actor(n).exist = 0
			FOR b = 0 TO 63 STEP 8
				xs = angle.x(b) \ 2
				ys = angle.y(b) \ 2
				spawnObject actor(n).x, actor(n).y, 93, xs, ys
			NEXT b
		END IF

		'LEVEL DOOR
        CASE 64
			playerDeltaX = ABS(actor(n).x - playerX)
			playerDeltaY = ABS(actor(n).y - playerY)
			IF player.levelScore(actor(n).jump) > 0 THEN actor(n).frame = 60
			playerDeltaY = actor(2).y - actor(n).y
			IF playerDeltaX < 4 AND playerDeltaY > 4 AND playerDeltaY < 32 THEN
				IF actor(n).count = 0 AND player.levelScore(actor(n).jump) = 0 THEN
					w40char.text = "         PRESS UP TO ENTER LEVEL        |"
					aPrint &HFFFF, &HE0E, w40char.seg, w40char.ofs, 0, 0, hudBufferSeg, hudBufferOfs
				END IF
				player.hudLock = 999
				actor(n).count = 1
				IF kbArray(72) THEN
					player.level = actor(n).jump
					player.score = 0
					actor(2).control = 259
				END IF
			ELSE
				IF actor(n).count THEN
					player.hudLock = 0
					hudUpdate 255
					actor(n).count = 0
				END IF
			END IF
		END SELECT		
'----------------------------------------------------------------------------
	END IF	'Page check
'----------------------------------------------------------------------------
	flash = NOT actor(n).flash AND 1
	actor(n).flash = actor(n).flash + (actor(n).flash > 0)
	frame = actor(n).frame
	IF advanceAnims THEN actor(n).frame = animation(frame).jump
	
	'IF animation(frame).offset(0) = 0 THEN
	' errorMessage$ = "Actor"+str$(n)+"/"+str$(actor(n).control)+" empty frame @ "+str$(frame)
	' quit
	'END IF
	
	x = actor(n).x - screenX + animation(frame).x
	y = actor(n).y - screenY + animation(frame).y 

	IF actor(n).collider THEN           
		IF frame AND x > -10 AND x < 40 AND y > -10 AND y < 50 THEN
			IF flash THEN
				spriteList(spriteListIndex, gfx.page).offset = animation(frame).offset(actor(n).flags)
				spriteList(spriteListIndex, gfx.page).x = x
				spriteList(spriteListIndex, gfx.page).y = y
				spriteList(spriteListIndex, gfx.page).w = animation(frame).w
				spriteList(spriteListIndex, gfx.page).h = animation(frame).h
				spriteListIndex = spriteListIndex + 1
			END IF
			ctype = actor(n).collider 
			colCount(ctype) = colCount(ctype) + 1
			colList(ctype, colCount(ctype)).index = n
			colList(ctype, colCount(ctype)).x0 = x
			colList(ctype, colCount(ctype)).y0 = y
			colList(ctype, colCount(ctype)).x1 = x + animation(frame).w - 1
			colList(ctype, colCount(ctype)).y1 = y + animation(frame).h - 1
		END IF   
	ELSE
		IF frame AND flash > 0 AND x > -10 AND x < 40 AND y > -10 AND y < 50 THEN
			spriteList(spriteListIndex, gfx.page).offset = animation(frame).offset(actor(n).flags)
			spriteList(spriteListIndex, gfx.page).x = x
			spriteList(spriteListIndex, gfx.page).y = y
			spriteList(spriteListIndex, gfx.page).w = animation(frame).w
			spriteList(spriteListIndex, gfx.page).h = animation(frame).h
			spriteListIndex = spriteListIndex + 1
		END IF
	END IF
  END IF 'Actor-in-range check
'----------------------------------------------------------------------------
		'Reduce HP, if in counter-mode (HP > 0)
		'Destroy if HP = 0
		'
		IF gfx.page > 0 AND actor(n).life > -1 THEN
			IF actor(n).life = 0 THEN
				IF actor(n).control > 0 AND actor(n).control < 63 THEN
					level.spawner(actor(n).spawner).object = 0				
				END IF
				IF actor(n).control > 0 AND actor(n).control < 11 THEN
					soundEvent = SndEnemyDrop
					actor(n).exist = 1
					actor(n).flags = 0
					actor(n).control = 41
					actor(n).flash = 4
					actor(n).vecX = 0
					actor(n).vecY = -20
					actor(n).y = actor(n).y - 4
					actor(n).collider = 0
					actor(n).life = 60
					actor(n).frame = 22
					c = tileRead(actor(n).x, actor(n).y)
					IF c > 29 THEN
						actor(n).count = actor(n).y + 8
					ELSE
						actor(n).count = actor(n).y
					END IF
				ELSE
					destroyObject n    ' getObject
				END IF
			END IF
			actor(n).life = actor(n).life - 1
		END IF	
	END IF 'Actor-exists check
LOOP WHILE actor(n).lNext > 1
 
 IF gfx.page THEN dtMain1 = (dtMain1 + 1) AND 63

 spriteList(spriteListIndex, gfx.page).y = 255

 IF soundEvent > -1 THEN aSoundFX 0, soundEvent

 IF player.score > scoreDelta THEN
  hudUpdate 4
 END IF

 dtMain0 = (dtMain0 + 1) AND 63

END SUB

'==============================================================================
SUB handleCollision
	soundEvent = -1
	'Collision lists
	' 1 = Affect Player
	' 2 = Affect Enemy
	' 3 = Affect Bullet
	
	'Affect player
	IF actor(2).control = 255 AND actor(2).flash = 0 AND colCount(0) > -1 THEN
		px = actor(2).x - gfx.screenX(gfx.page)
		py = actor(2).y - gfx.screenY(gfx.page)
		FOR a = 0 TO colCount(1)
			n = colList(1, a).index
			IF px > colList(1, a).x0 - 2 AND px < colList(1, a).x1 + 2 AND py > colList(1, a).y0 + 2 AND py < colList(1, a).y1 + 8 THEN
				damagePlayer
				a = 9999
			END IF
		NEXT a
	END IF
	'Affect enemy
	FOR b = 0 TO colCount(2)
		n = colList(2, b).index
		px = actor(n).x - gfx.screenX(gfx.page)
		py = actor(n).y - gfx.screenY(gfx.page)
		FOR a = 0 TO colCount(1)
			IF px > colList(1, a).x0 - 2 AND px < colList(1, a).x1 + 2 AND py > colList(1, a).y0 - 3 AND py < colList(1, a).y1 + 3 THEN
				n2 = colList(1, a).index
				IF actor(n2).control = 2 THEN
					IF actor(n).vecX > 0 THEN
						actor(n2).flags = 0
					ELSE
						actor(n2).flags = 1
					END IF
				END IF
				IF actor(n2).life > -999 THEN
					actor(n2).flash = 10
					actor(n2).life = actor(n2).life + 1
				END IF
				IF actor(n).control = 100 THEN
					actor(n).collider = 0
					actor(n).life = 4
					actor(n).vecY = -5
					actor(n).control = 0
					actor(n).vecX = 0
					actor(n).flags = 0
					'actor(n).flash = 20
					IF actor(n2).life > -1 THEN
						actor(n).x = actor(n2).x
						actor(n).frame = 53
						soundEvent = 44
					ELSE
						actor(n).frame = 34
						soundEvent = 44
					END IF
				END IF
			END IF
		NEXT a
	NEXT b

	IF soundEvent > -1 THEN aSoundFX 0, soundEvent
END SUB

'==============================================================================
SUB hudUpdate (flags AS INTEGER)
	IF player.hudLock THEN
		player.hudLock = player.hudLock - 1
		IF player.hudLock = < 2 THEN
			flags = 255
			player.hudLock = 0
		ELSE
			EXIT SUB
		END IF
	END IF
	IF player.level > 1 THEN
		IF flags AND 1 THEN
			w40char.text = "KEYS:     0000000              TIME:    |"
			aPrint &HFFFF, &HE0C, w40char.seg, w40char.ofs, 0, 0, hudBufferSeg, hudBufferOfs
		END IF
		IF flags AND 8 THEN
			a$ = STR$(player.time)
			w40char.text = a$ + "|"
			aPrint &HFFFF, &HF0E, w40char.seg, w40char.ofs, 39 - LEN(a$), 0, hudBufferSeg, hudBufferOfs
		END IF
		IF flags AND 16 THEN
			w40char.text = STR$(player.hasKey) + "|"
			aPrint &HFFFF, &HE0C, w40char.seg, w40char.ofs, 5, 0, hudBufferSeg, hudBufferOfs
		END IF
	ELSE
		IF flags AND 1 THEN
			w40char.text = "          0000000                       |"
			aPrint &HFFFF, &HE0C, w40char.seg, w40char.ofs, 0, 0, hudBufferSeg, hudBufferOfs
		END IF
	END IF
	IF flags AND 2 THEN
		IF player.hp < 4 THEN
			w40char.text = "#$#$#$#$|"
			aPrint &HFFFF, &H505, w40char.seg, w40char.ofs, 19, 0, hudBufferSeg, hudBufferOfs
		END IF
		a$ = "#$#$#$#$"
		IF player.hp THEN
			w40char.text = MID$(a$, 1, player.hp * 2) + "|"
			aPrint &HFFFF, &HCECE, w40char.seg, w40char.ofs, 19, 0, hudBufferSeg, hudBufferOfs
		END IF
	END IF
	IF flags AND 4 THEN
		a$ = STR$(player.score)
		c = LEN(a$)
		a$ = MID$(a$, 2, c)
		w40char.text = a$ + "|"
		aPrint &HFFFF, &HC0E, w40char.seg, w40char.ofs, 18 - c, 0, hudBufferSeg, hudBufferOfs
	END IF
END SUB

SUB initGame
	OPEN "debug.txt" FOR OUTPUT AS #1
		PRINT #1, TIME$
	CLOSE #1
	elfInit
	
	loadSounds "sound"
	walkNote(0) = 30
	walkNote(1) = 32
	walkNote(2) = 34
	walkNote(3) = 35
	walkNote(4) = 30
	walkNote(5) = 32
	walkNote(6) = 28
	walkNote(7) = 27
	
	loadSprites "sprites0", 0
	player.spriteSet = "sprites0"
	
	initTestGame
	'initMainMenu
END SUB

SUB initTestGame
	player.level = 0
	IF LEN(COMMAND$) THEN player.level = VAL(COMMAND$)
	DO
		main
		IF player.level = -1 THEN quit
	LOOP
END SUB

'==============================================================================
 REM _______ _______ _____ __   _
 REM |  |  | |_____|   |   | \  |
 REM |  |  | |     | __|__ |  \_|                             
'==============================================================================
 SUB main
	hudTextLoad &HA0, 1

	loadLevel
	
	for n = 1 to 60
		actor(n).exist = 0
		actor(n).parentDX = 0
		actor(n).parentDY = 0
		actor(n).lNext = 1
		actor(n).lPrev = 1
		actor(n).control = 0
	next n
		
	actor(1).x = 0
	actor(1).y = 0
	actor(1).lNext = 2
	actor(1).lPrev = 3
	actor(1).exist = 0

	colCount(1) = -1
	colCount(2) = -1
	colCount(3) = -1
	
	player.hp = 4
	player.time = 300
	player.hasKey = 0
	player.jumpRelease = -1
	player.wave = 0
	player.wavePrev = -1
	player.score = 0
	player.gotCoins = 0
	player.gotKeys = 0
	player.gotChests = 0	
	actor(2).flash = 0
	actor(2).lPrev = 1
	actor(2).lNext = 3
	actor(2).exist = 1
	actor(2).control = 255
	actor(2).life = -3
	actor(2).x = level.playerX
	actor(2).y = level.playerY
	actor(2).parentDX = 0
	actor(2).parentDY = 0
	actor(2).collider = 0
	actor(2).vecX = 0
	actor(2).vecY = 0
	player.vecY = 0
	player.vecX = 0
	player.Bouncing = 0
	actor(2).frame = 1
	actor(2).anim = 1
	actor(2).visX = 999
	actor(2).visY = 999
	actor(2).count = 0
	playerOldX = -999
	playerOldY = -999
	
	actor(3).control = 0
	actor(3).frame = 11
	actor(3).flags = 0
	actor(3).x = actor(2).x
	actor(3).y = actor(2).y
	actor(3).vecX = 0
	actor(3).vecY = 0
	actor(3).parentDX = actor(2).x
	actor(3).parentDY = actor(2).y
	actor(3).exist = 2
	actor(3).lPrev = 2
	actor(3).lNext = 1
	actor(3).life = -999
	actor(3).collider = 0
	actor(3).visX = 999
	actor(3).visY = 999
	
	handleSpawn 0
	
	updateText = 1
	
	IF player.level = 1 THEN
		for n = 1 to 10
			player.score = player.score + player.levelScore(n)
		next n
	END IF
	
	IF player.level = 0 THEN
		'hudTextLoad &H6E0, 0
		player.hudLock = 999
		actor(2).control = 258
		actor(2).frame = 1
		player.menuContext = 10
		screenResetPos actor(2).x - 20, actor(2).y - 52
		for n = 1 to 16
			player.levelScore(n) = 0
		next n
		player.score = 0
	ELSE
		hudTextLoad &H1E0 + player.level * 160, 0
		player.hudLock = 5
		screenResetPos actor(2).x - 20, actor(2).y - 46
	END IF
		
	CALL aTimerReset
	
	player.levelPrev = player.level
	
	DO
		CALL aTimerWait
		handleCollision
		handleBehaviors
		CALL aSoundPlay
		handleScroll
		aSpriteList gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), spriteBankSeg, spriteBankOfs, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)		
		IF actor(2).control = 259 THEN aSoundStop: EXIT SUB				
		IF actor(2).control = 255 THEN
			IF kbArray(1) THEN
				actor(2).control = 258
				player.menuContext = 2
				IF player.level = 1 THEN player.menuContext = 1
				player.menuMode = 0		
			END IF
			updateText = updateText + 1
			IF updateText = 24 THEN
				IF actor(2).control = 255 THEN player.time = player.time + (player.time > 0)
				updateText = 0
				hudUpdate &H8
			END IF
			END IF
	  aPageFlip gfx.videoWrap, hudBufferSeg, hudBufferOfs, gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
      gfx.page = gfx.page XOR 1
	  aRectList gfx.WindowOfs(gfx.page), gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), tileBufferWrap, tileBufferSeg, tileBufferOfs, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)
	LOOP
END SUB

'==============================================================================
 REM _______  _____  _______ _  _  _ __   _ _______  ______ _______
 REM |______ |_____] |_____| |  |  | | \  | |______ |_____/ |______
 REM ______| |       |     | |__|__| |  \_| |______ |    \_ ______|
'==============================================================================
SUB handleSpawn (wave)
	FOR n = 1 TO 16
		nn = wave * 16 + n
		o = level.spawner(nn).object
		'IF spawnerEnabler(nn) > 0 THEN o = spawnerEnabler(nn)
		IF o > 0 THEN
			spawnObject level.spawner(nn).x, level.spawner(nn).y, o, level.spawner(nn).flags, nn
			level.spawner(nn).object = level.spawner(nn).object OR &HF000
			'spawnerEnabler(nn) = o OR &HF000
			'level.wave(wave).spawner(n).object = 0			
		END IF
	NEXT n
END SUB

'==============================================================================
SUB spawnObject (x, y, object, spawnerFlags, spawner) 
	n = getSpawnSlot
	actor(n).spawner = 0
	actor(n).flash = 0
	actor(n).count = 0
	actor(n).flags = 0
	actor(n).life = -999
	actor(n).x = x
	actor(n).y = y
	actor(n).parentDX = actor(1).x
	actor(n).parentDY = actor(1).y
	actor(n).vecX = 0
	actor(n).vecY = 0
	actor(n).collider = 0

	actor(n).visX = defaultVisX
	actor(n).visY = defaultVisY

	SELECT CASE object
	CASE 257	'Player Die
		actorListNew n
		actor(n).control = 0
		actor(n).life = 15
		actor(n).frame = 34
		actor(n).vecX = spawnerFlags
		actor(n).vecY = spawner
		actor(n).visY = 40
		actor(n).spawner = 0
	CASE 200	'Explode
		actorListNew n
		actor(n).control = 0
		actor(n).life = 12
		actor(n).spawner = 0
		actor(n).collider = 0
		actor(n).frame = 34
		actor(n).anim = 1
	CASE 99   'Clear Tile
		actorListNew n
		actor(n).control = 0
		actor(n).spawner = 0
		SELECT CASE spawnerFlags
		CASE 0							'Twinkle
			actor(n).life = 4
			actor(n).flash = 0
			actor(n).frame = 38
		CASE 1							'Debris left/right
			actor(n).life = 4
			actor(n).flash = 8
			actor(n).frame = 57
			actor(n).vecX = spawner
		CASE 2							'Bounce
			actor(n).life = 4
			actor(n).frame = 39
			actor(n).vecY = spawner
		CASE 3							'Mover
			actor(n).life = 40
			actor(n).frame = 39
			actor(n).vecX = spawner
			actor(n).visX = 70
			actor(n).visY = 70
			actor(n).control = 99
		CASE 4							'Bounce
			actor(n).life = 4
			actor(n).frame = 39
			actor(n).vecY = spawner
			actor(n).visX = 999
			actor(n).visY = 999
			actor(n).control = 99
		END SELECT
		actor(n).jump = spawnerFlags
	
	CASE 93		'Twinkle particle
		actorListNew n   
		actor(n).control = 0
		actor(n).frame = 30
		actor(n).life = 6
		actor(n).vecX = spawnerFlags
		actor(n).vecY = spawner
		actor(n).spawner = 0
	CASE 100	'Player Bullet
		actorListNew n
		actor(n).control = 100
		actor(n).life = 7
		actor(n).spawner = 0
		actor(n).flags = spawnerFlags
		actor(n).collider = 2
		actor(n).vecX = 30 + (spawnerFlags <> 0) * 60
		actor(n).frame = 13
	CASE 102	'Enemy Bullet
		actorListNew n
		actor(n).control = 102
		actor(n).life = 24
		actor(n).spawner = 0
		actor(n).vecX = 10 + (spawnerFlags <> 0) * 20
		actor(n).vecY = 5
		actor(n).frame = 40
		actor(n).anim = 40
		actor(n).visX = 40
		actor(n).visY = 40   
	CASE 104	'DRIP
		actorListNew n
		actor(n).control = 104
		actor(n).life = 18
		actor(n).vecX = spawnerFlags
		actor(n).vecY = -30
		actor(n).frame = 82
		actor(n).anim = 214
		actor(n).visY = 40
		actor(n).spawner = 0		
	CASE 111	'FIRE PARTICLE
		actorListNew n
		actor(n).control = 102
		actor(n).life = 15
		actor(n).spawner = 0
		actor(n).vecX = 7 + (spawnerFlags <> 0) * 14
		actor(n).vecY = spawner
		actor(n).frame = 50
		actor(n).visX = 40
		actor(n).visY = 40   
	CASE 112	'FIRE BOMB
		actorListNew n
		actor(n).control = 112
		actor(n).life = 18
		actor(n).spawner = 0
		actor(n).vecX = 0
		actor(n).vecY = 5
		actor(n).frame = 50
		actor(n).visX = 40
		actor(n).visY = 50   
	CASE 1	'Eggy
		actorListNew n
		actor(n).flags = spawnerFlags
		actor(n).control = 1   
		actor(n).life = -2
		actor(n).collider = 1
		actor(n).frame = 15
		actor(n).anim = 60
		actor(n).count = 0
		actor(n).visX = 25
		actor(n).visY = 40   
	CASE 2	'HELMET
		actorListNew n
		actor(n).control = 2   
		actor(n).life = -50
		actor(n).collider = 1
		actor(n).frame = 72
		actor(n).count = 0
		actor(n).visY = 35
		actor(n).visX = 60
	CASE 3	'DRAGON
		actorListNew n
		actor(n).control = 3   
		actor(n).life = -2
		actor(n).collider = 1
		actor(n).frame = 89
		actor(n).count = 0
		actor(n).visY = 55
		actor(n).visX = 55
	CASE 4	   'DROOLY
		actorListNew n
		actor(n).control = 4   
		actor(n).life = -1
		actor(n).collider = 1
		actor(n).vecX = 2
		actor(n).frame = 78
		actor(n).anim = 208
		actor(n).visY = 35   
	CASE 5	'BABAT
		actorListNew n
		actor(n).control = 5   
		actor(n).spawner = spawner
		actor(n).life = -2
		actor(n).dir = y
		actor(n).collider = 1
		actor(n).frame = 85
		actor(n).visX = 40
		actor(n).visY = 50
		actor(n).count = 0   
	CASE 6	'SKULL
		actorListNew n
		actor(n).control = 6   
		actor(n).spawner = spawner
		actor(n).life = -999
		actor(n).collider = 1
		actor(n).frame = 70
		actor(n).visX = 999
		actor(n).visY = 30
		actor(n).count = y 
	CASE 8 TO 9	'SPIKY
		actorListNew n
		actor(n).control = 8
		actor(n).life = -999
		actor(n).collider = 1
		actor(n).frame = 76
		actor(n).visX = 80
		actor(n).visY = 60
		actor(n).count = 0
		actor(n).dir = 0
		IF object = 8 THEN
			actor(n).jump = 0
		ELSE
			actor(n).jump = 1
		END IF
	CASE 11 TO 12	'FIRE BREATH R / L
	   actorListNew n
	   actor(n).control = 11   
	   actor(n).spawner = spawner
	   actor(n).life = -999
	   actor(n).frame = 47
	   actor(n).anim = 150
	   actor(n).visY = 25
	   actor(n).visX = 21
	   IF object = 12 THEN 
			actor(n).flags = 1
			actor(n).dir = -1
		ELSE
			actor(n).dir = 1
		END IF
	CASE 15			'Bouncing spike ball
		actorListNew n
		actor(n).collider = 1
		actor(n).control = 15    
		actor(n).life = -999
		actor(n).frame = 45
		actor(n).visX = 24
		actor(n).visY = 55
	CASE 16  		'SPAWNER
		actorListNew n
		actor(n).control = 16    
		actor(n).life = -999
		actor(n).frame = 0
		actor(n).visX = 30
		actor(n).visY = 70
		actor(n).jump = spawnerFlags
	CASE 17 TO 19	'SPIKE BALL U, O+, O-
		c = object
		FOR p = 0 TO 1
			actorListNew n
			actor(n).x = x
			actor(n).y = y
			actor(n).count = x
			actor(n).dir = y
			actor(n).control = c
			actor(n).exist = 1
			actor(n).spawner = spawner
			actor(n).life = -999
			actor(n).jump = 2 + p
			actor(n).frame = 46
			actor(n).anim = 148
			actor(n).visX = 45
			actor(n).visY = 50
			actor(n).flags = 0
			n = getSpawnslot
		NEXT p
		actorListNew n
		actor(n).collider = 1
		actor(n).x = x
		actor(n).y = y
		actor(n).count = x
		actor(n).dir = y
		actor(n).control = c
		actor(n).flags = 0
		actor(n).exist = 1
		actor(n).spawner = spawner
		actor(n).life = -999
		actor(n).jump = 5
		actor(n).frame = 45
		actor(n).anim = 146
		actor(n).visX = 50
		actor(n).visY = 56
	CASE 41	   'PICKUP GEM/KEY/ETC...
		actorListNew n
		actor(n).control = 41
		actor(n).spawner = 0
		actor(n).life = 60
		actor(n).count = y
		actor(n).vecY = -20
		actor(n).frame = spawnerFlags
		'actor(n).anim =  spawnerFlags
		actor(n).visY = 34
	CASE 44   'KEY
		actorListNew n
		actor(n).control = 41
		actor(n).spawner = spawner
		actor(n).life = -999
		actor(n).vecY = 0
		actor(n).frame = 21		
	CASE 45   'CHEST
		actorListNew n
		actor(n).control = 45
		actor(n).spawner = spawner
		actor(n).life = -999
		actor(n).vecY = 0
		actor(n).frame = 20	
    CASE 50 TO 51  	'H and V platform
		actorListNew n
		actor(n).count = object
		actor(n).jump = 0
		actor(n).control = 50
		actor(n).dir = 0
		actor(n).spawner = spawner
		actor(n).life = -999
		actor(n).frame = 18
		actor(n).visX = 999
		actor(n).visY = 160
		actor(n).vecY = 0
		actor(n).vecX = 0
		IF object = 50 THEN actor(n).vecX = 5
	CASE 62	   'Checkpoint / Spawner portal
		actorListNew n
		actor(n).control = 62
		actor(n).spawner = spawner
		actor(n).flags = spawnerFlags
		actor(n).life = -3
		actor(n).frame = 0
		actor(n).anim = 0
		actor(n).visX = 999
	CASE 63		'EXIT
		actorListNew n
		actor(n).spawner = spawner
		actor(n).frame = 0
		actor(n).control = 63
	CASE 64		'LEVEL
		actorListNew n
		actor(n).spawner = spawner
		actor(n).jump = spawnerFlags
		IF player.levelScore(spawnerFlags) THEN
			actor(n).frame = 53
		ELSE
			actor(n).frame = 0
		END IF
		actor(n).control = 64
		IF player.levelPrev = spawnerFlags THEN
			actor(2).x = x
			actor(2).y = y + 26
		END IF
	CASE 71	   'MENU CURSOR
		actor(2).flash = 0
		actor(2).control = 258
		actor(2).exist = 1
		actor(2).x = x
		actor(2).y = y
		actor(2).vecX = 0
		actor(2).vecY = 0
		actor(2).anim = 10
		actor(2).frame = 1
		actor(2).flags = 0
		screenResetPos x - 20, actor(2).y
		player.targetX = x
		player.targetY = y
		actor(2).count = 0
	CASE 72	   'MENU ITEM
		actorListNew n
		actor(n).control = 72
		actor(n).life = -999
		actor(n).frame = 0
		actor(n).flags = spawnerFlags
		actor(n).spawner = spawner
	END SELECT
END SUB

'==============================================================================
FUNCTION speedDiff% (x0, x1)
	d = ABS(x0 - x1)
	IF d = 0 THEN speedDiff% = 0: EXIT FUNCTION
	IF x0 < x1 THEN
		m = 1
	ELSE
		m = -1
	END IF
	IF d > 16 THEN speedDiff% = 30 * m: EXIT FUNCTION
	IF d > 8 THEN speedDiff% = 20 * m: EXIT FUNCTION
	speedDiff% = 10 * m
END FUNCTION

'==============================================================================
SUB makeParent (child, parent)	
	actor(child).exist = parent
	actor(child).parentDX = actor(parent).X
	actor(child).parentDY = actor(parent).Y
	
	n = child
	do
		n = actor(n).lPrev
		if n = parent then EXIT SUB
	loop while n>1
	
	cNext = actor(child).lNext
	cPrev = actor(child).lPrev
	pNext = actor(parent).lNext

	actor(cPrev).lNext = actor(child).lNext
	actor(cNext).lPrev = actor(child).lPrev
	actor(child).lPrev = parent
	actor(child).lNext = pNext
	actor(parent).lNext = child
	actor(pNext).lPrev = child	
END SUB

SUB unParent (index)
	'debugOut "unParent"
	actor(index).parentDX = 0
	actor(index).parentDY = 0
	actor(index).exist = 1
END SUB

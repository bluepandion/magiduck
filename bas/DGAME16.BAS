DECLARE SUB DEBUGOUT (s$)
DECLARE SUB actorListNew (index%)
DECLARE SUB actorListNewLast (index%)
DECLARE SUB unParent (object%)
DECLARE SUB makeParent (child%, parent%)
DECLARE SUB getChildrenFor(parent%)
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
DECLARE SUB SaveGame
DECLARE SUB LoadGame
DECLARE FUNCTION DrawHighScores% ()
DECLARE SUB SaveHighScore (offset)
'DECLARE SUB oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo ()

DEFINT A-Z

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
CONST ItemSuperOrb = 22
CONST ItemGemSmall = 23
CONST ItemGemBig = 24
CONST ItemHeart = 25
CONST SndButton = 184
CONST SndTileGrow = 193
CONST SndBossDamage = 200

'===============================================================================

SUB actorListNew (index)
	'debugOut "actorListNew"
	actor(index).exist = 1
	
	actor(index).lPrev = 1
	actor(index).lNext = actor(1).lNext
	
	actor(actor(1).lNext).lPrev = index
	
	actor(1).lNext = index
	
	''actor(index).lNext = 2 '1
	''actor(index).lPrev = actor(2).lPrev 'actor(1).lPrev
	
	
	
	'Using actor 2 here will keep player on top of the linked list
	'
	'actor(actor(2).lPrev).lNext = index
	'actor(2).lPrev = index
	
	''actor(actor(1).lPrev).lNext = index
	''actor(1).lPrev = index
	
	'actor(actor(1).lPrev).lNext = index
	'actor(index).lPrev = actor(1).lPrev
	'actor(1).lPrev = index
END SUB

SUB actorListNewLast (index)
	actor(index).exist = 1
	
	actor(index).lPrev = actor(1).lPrev
	actor(index).lNext = 1
	
	actor(actor(1).lPrev).lNext = index
	
	actor(1).lPrev = index
END SUB

SUB addScore (eventType)
	s = 0
	SELECT CASE eventType
	CASE 1
		s = 15
	CASE ItemSuperOrb
		s = 1000
	CASE ItemGemSmall
		s = 100
	CASE ItemGemBig
		s = player.hp * 200
	CASE ItemHeart
		s = 0
		IF player.hp < 4 THEN player.hp = player.hp + 1
		hudUpdate 2	
	CASE ItemKey
		s = 500		
	END SELECT
	IF player.supaFly THEN s = s * 20
	player.score = player.score + s
END SUB

SUB damagePlayer
    IF actor(2).flash = 0 THEN
		player.supaFly = 0
		player.hp = player.hp + (player.hp > 0)
		actor(2).flash = 40
		aSoundFX 0, SndDamage
		hudUpdate 2
		'w40char.text = SPACE$(40) + "|"
		'aPrint &HFFFF, &HC040, 0, 0, hudBufferSeg, hudBufferOfs
		'player.hudLock = 4
	END IF
END SUB

SUB destroyObject (n)
	IF actor(n).exist < 0 THEN
		FOR c = 4 TO 60
			IF ABS(actor(c).exist) = n THEN actor(c).exist = 1
		NEXT c
	END IF
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
	STATIC friction
	STATIC collisionFloor
	STATIC scoreDelta AS LONG
	STATIC parent
	STATIC advanceAnims
	STATIC tickAnim
	
	entityCounter = 0
	scoreDelta = player.score
	spriteListIndex = 0
	soundEvent = -1
	
	fractionC = (fractionC + 1) MOD 10
'------------------------------------------------------------------------------
	colCount(1) = -1
	colCount(2) = -1
	colCount(3) = -1
	
	tickAnim = tickAnim + 1
	IF tickAnim > 2 THEN
		tickAnim = 0
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
CASE 263	'Main menu pan
	handleCamera
	actor(2).count = actor(2).count + 1
	IF actor(2).count > 2 THEN actor(2).control = 258
'------------------------------------------------------------------------------
CASE 265	'High score - draw screen			
	myPrint 16, 4, "TOP DUCKS", &H1A17
	actor(2).frame = 0
	actor(3).frame = 0
	c = DrawHighScores
	actor(2).jump = (10 - c) * -3 + 40
	actor(2).dir = (10 - c) * 12 + 1
	actor(2).count = 0	
	highScoreEntry.name = "       "
	highScoreEntry.score = player.score
	IF c > 0 THEN 		
		w40char.text = " PLEASE TYPE YOUR NAME AND PRESS ENTER. |"
		aPrint &HFFFF, &H0E0E, 0, 0, hudBufferSeg, hudBufferOfs
		actor(2).control = 266
		aKBremove		
	ELSE
		actor(2).control = 258
		actor(2).dir = 0
		player.menuMode = 0
		player.menuContext = 14		
	END IF	
CASE 266	'High score - get input	
	d$ = inkey$
	IF d$ <> "" THEN 
		c = ASC(UCASE$(d$))
	ELSE
		c = 0
	END IF
	
	IF c > 31 AND c < 91 THEN		
		IF actor(2).count < 8 THEN
			actor(2).count = actor(2).count + 1			
		END IF
		MID$(highScoreEntry.name, actor(2).count, 1) = CHR$(c)		
	END IF
	IF c = 8 THEN				
		IF actor(2).count > 0 THEN
			MID$(highScoreEntry.name, actor(2).count, 1) = " "		
			actor(2).count = actor(2).count - 1
		END IF
	END IF
	myPrint 11, actor(2).jump, highScoreEntry.name, &H1F1F
	myPrint 25, actor(2).jump, str$(highScoreEntry.score), &H1F1F
	nn = &H1E1E
	IF gfx.page THEN nn = &H1D1D
	myPrint 11 + actor(2).count, actor(2).jump, ". ", nn
	IF c = 13 THEN 
		actor(2).control = 267
		myPrint 10 + actor(2).count, actor(2).jump, "  ", nn
	END IF
CASE 267	'High score - sync pages after input	
	myPrint 11 + actor(2).count, actor(2).jump, "  ", &H1111
	actor(2).control = 268
CASE 268	'High score - reinstall keyboard int, save score	
	aKBinit
	SaveHighScore actor(2).dir
	player.score = 0
	actor(2).control = 265
	
CASE 262	'Level Clear - Score Count
      pageOfs = gfx.pageOfs(gfx.page)

      SELECT CASE actor(2).jump
       CASE -2 to -1
		actor(2).jump = actor(2).jump + 1
	   CASE 0
          IF player.supaFly THEN
			player.supaFly = 1500
			c = &H0A0A OR player.levelColor
			myPrint 2, 5, "POWER BONUS!", c
		  ELSE
		    player.supaFly = 30
		  END IF
		  c = &H0A0A OR player.levelColor
		  myPrint 2, 22, "TIME BONUS            X" + STR$(player.supaFly), c
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
		  
		  player.scoreCounter = level.keys + level.keys + level.coins + 1
		  player.scoreCounterInc = player.gotKeys + player.gotChests + player.gotCoins + 1
		  player.scoreCounterTarget = (player.scoreCounterInc / player.scoreCounter) * 50 + 50
		  
		  IF player.levelCompletion(player.level) < player.scoreCounterTarget THEN 
			player.levelCompletion(player.level) = player.scoreCounterTarget
			IF player.scoreCounterTarget = 100 THEN
				myPrint 2, 5, "POWER ORB", c
				myPrint 2, 8, "UNLOCKED!", c
			END IF
		  END IF
		  IF player.time > player.levelTime(player.level) THEN
			player.levelTime(player.level) = player.time
		  END IF
		  
		  actor(2).jump = actor(2).jump + 1
	   CASE 1
		  aCopyPage gfx.pageOfs(gfx.page xor 1), gfx.pageOfs(gfx.page)
		  actor(2).jump = 2
       CASE 2 TO 3
          actor(2).jump = actor(2).jump + 1
          player.scoreCounter = 1
          player.scoreCounterInc = 10
          player.scoreCounterTarget = player.time
          player.scoreCounterMode = 0
          player.scoreCounterMul = player.supaFly
		  player.scoreDelta = 1& * player.time * player.supaFly

       CASE 4 TO 6
          w40char.text = "        |"
          a$ = STR$(0& + player.scoreCounter)
          MID$(w40char.text, 8 - LEN(a$), LEN(a$)) = a$
          c = &H0A0A OR player.levelColor
		  aPrint gfx.videoWrap, c, 16, 22 + player.scoreCounterMode * 4, &HB800, pageOfs
          IF player.scoreCounter + player.scoreCounterInc >= player.scoreCounterTarget THEN
               player.scoreCounter = player.scoreCounterTarget
               actor(2).jump = actor(2).jump + 1
          ELSE
               player.scoreCounter = 0& + player.scoreCounter + player.scoreCounterInc               
          END IF
          IF gfx.page THEN soundEvent = 195

       CASE 7 
		  player.score = 0& + player.score + player.scoreDelta
		  soundEvent = 132
		  actor(2).jump = 8		  
	   
	   CASE 8 TO 16          		  
		  w40char.text = "         |"
          a$ = STR$(player.scoreDelta)
          MID$(w40char.text, 8 - LEN(a$), LEN(a$)) = a$		  
		  c = &H0E0F OR player.levelColor		  
		  aPrint gfx.videoWrap, c, 30, 22 + player.scoreCounterMode * 4, &HB800, pageOfs
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
				actor(2).jump = 20
				hudUpdate 4
				IF player.score > player.levelScore(player.level) THEN
					player.levelScore(player.level) = player.score
				END IF				
				player.levelScore(0) = 0
				for c = 1 to 15
					player.levelScore(0) = player.levelScore(0) + player.levelScore(c)
				next c
				SaveGame
				soundEvent = 92
          END SELECT

        CASE 20 TO 21
            actor(2).jump = actor(2).jump + 1			
			c = &H0E0E OR player.levelColor
			myPrint 15, 43, "GAME SAVED", c
			c = &H0707 OR player.levelColor
			myPrint 8, 46, "PRESS SPACE TO CONTINUE", c
        
		CASE 22
			IF kbArray(57) THEN actor(2).jump = 23

        CASE 23 TO 24
			actor(2).jump = actor(2).jump + 1
            actor(2).frame = 0
			actor(3).frame = 0
            tileCopyList(0, gfx.page).x = 0
            tileCopyList(0, gfx.page).y = 0
            tileCopyList(0, gfx.page).w = 40
            tileCopyList(0, gfx.page).h = 50
            tileCopyList(1, gfx.page).y = 255
            aClearList gfx.pageOfs(gfx.page), VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset), &HDE

        CASE 25
			CALL aSoundStop
			IF player.level = 11 THEN 
				player.level = 25
			ELSE
				player.level = 0
			END IF
			actor(n).control = 259
      END SELECT
 '------------------------------------------------------------------------------              
CASE 261	'Level Clear - fly out of level
               
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
CASE 260	'Level Clear - sync video pages
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
CASE 258	'Menu	
	SELECT CASE player.menuMode
	CASE 0		'Display menu load message		
		'w40char.text = " LOADING PAGE ...                      |"
		player.menuExitCtrl = 255
		player.hudLock = 999
		'aPrint &HFFFF, &HC0E, w40char.seg, w40char.ofs, 0, 0, hudBufferSeg, hudBufferOfs
		'player.menuMode = 1
		actor(2).jump = actor(1).lNext
		actor(1).lNext = 1
		aSoundStop
		handleScroll
		menuLoad player.menuContext - 1
		
		aCopyPage gfx.pageOfs(gfx.page), gfx.pageOfs(gfx.page xor 1)
		w40char.text = " ARROW KEYS TO MOVE ... ENTER TO SELECT |"
		aPrint &HFFFF, &H0E0E, 0, 0, hudBufferSeg, hudBufferOfs
		player.hudLock = 999
		spriteList(0, 0).y = 255
		spriteList(0, 1).y = 255
		'aPageFlip gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
		'gfx.page = gfx.page XOR 1
		'aCopyPage gfx.pageOfs(gfx.page), gfx.pageOfs(gfx.page xor 1)
		player.menuMode = 2
		player.menuHLcolor = &HCECF
		soundEvent = player.menuSound
		
	CASE 2		'Item highlight and wait for input
		IF actor(2).count > 0 THEN
			IF actor(2).count > 3 THEN
				nn = menuItem(player.menuPrev)
				x = actor(nn).x
				y = actor(nn).y
				w = actor(nn).vecX
				h = actor(nn).vecY
				aMenuHiLite x, y, w, h, actor(nn).frame, gfx.pageOfs(gfx.page)				
			END IF
			IF actor(2).count < 5 THEN
				IF kbArray(80) = 0 AND kbArray(72) = 0 AND kbArray(75) = 0 AND kbArray(77) = 0 THEN actor(2).count = 1
			END IF
			actor(2).count = actor(2).count - 1			
		ELSE
			player.menuPrev = player.menuItem
			nn = menuItem(player.menuItem)
			IF kbArray(80) OR kbArray(77) THEN 
				IF player.menuItem <> actor(nn).lNext THEN
					player.menuItem = actor(nn).lNext
					soundEvent = sndMenuNext
					actor(2).count = 5					
				END IF
			END IF
			IF kbArray(72) OR kbArray(75) THEN 
				IF player.menuItem <> actor(nn).lPrev THEN
					player.menuItem = actor(nn).lPrev
					soundEvent = sndMenuPrev
					actor(2).count = 5					
				END IF
			END IF
			IF kbArray(28) THEN player.menuMode = 5
		END IF
		nn = menuItem(player.menuItem)
		x = actor(nn).x
		y = actor(nn).y
		w = actor(nn).vecX
		h = actor(nn).vecY
		aMenuHiLite x, y, w, h, player.menuHLcolor, gfx.pageOfs(gfx.page)
	'--------------------------------------------------------------------------
	CASE 5								'MENUCONTEXT Process selected menu item
		'
		'menuContext -1 = change level, 0 = exit menu behavior, >0 = change menu
		'menuMode 999 = menu clear
		'
		player.menuMode = 2
		player.targetX = 0
		player.targetY = 0
		SELECT CASE player.menuContext
		'----------------------------------------------------------------------
		CASE 1 TO 2	'Pause menu
				player.menuMode = 999
				SELECT CASE player.menuItem
				CASE 1					
					player.menuContext = 0
				CASE 2	'quit to main menu
					player.menuContextPrev = player.menuContext
					player.menuContext = 5					
				CASE 3	'quit to dos
					player.menuContextPrev = player.menuContext
					player.menuContext = 4					
				CASE 7	'forfeit					
					player.menuContext = -1
					player.level = 0
				END SELECT
		'----------------------------------------------------------------------
		CASE 3	'Death menu
				player.menuMode = 999
				SELECT CASE player.menuItem
				CASE 1	'try again
					player.score = 0
					player.menuContext = -1					
				CASE 2	'no
					player.level = 0
					player.menuContext = -1					
				END SELECT
		'----------------------------------------------------------------------
		CASE 4	'Quit to dos
				SELECT CASE player.menuItem
				CASE 1	'no
					player.menuContext = player.menuContextPrev
					player.menuContextPrev = 0
					player.menuMode = 999
				CASE 2	'yes
					actor(1).lNext = actor(2).jump
					actor(2).control = 259
					player.level = -1
				END SELECT
		'----------------------------------------------------------------------
		CASE 5	'Quit to main menu
				player.menuMode = 999
				SELECT CASE player.menuItem
				CASE 1	'no
					player.menuContext = player.menuContextPrev
					player.menuContextPrev = 0					
				CASE 2	'yes
					player.level = 24
					player.menuContext = -1					
				END SELECT
		'----------------------------------------------------------------------
		CASE 10 'Main menu
				player.menuMode = 999
				SELECT CASE player.menuItem
				CASE 1	'new game
					player.menuContext = 12
					player.menuContextPrev = 1					
					'screenResetPos 
					player.targetX = actor(2).x - 20
					player.targetY = actor(2).y + 84
				CASE 2 'continue
					player.menuContext = 12
					player.menuContextPrev = 2					
					'screenResetPos 
					player.targetX = actor(2).x - 20
					player.targetY = actor(2).y + 20
				CASE 3 'help
					player.menuContextPrev = player.menuContext
					player.menuContext = 11					
				CASE 4  'high scores
					player.score = 0					
					player.menuExitCtrl = 265
					player.menuContext = 0
					'screenResetPos 
					player.targetX = actor(2).x - 20
					player.targetY = 520
				CASE 5	'quit to dos
					player.menuContextPrev = player.menuContext
					player.menuContext = 4					
				END SELECT		
		CASE 11 'Help contents
			player.menuMode = 999
			SELECT CASE player.menuItem			
			CASE 1								
				player.menuContext = 40
			CASE 3	'enemies
				player.menuContext = 30
				'screenResetPos 
				player.targetX = actor(2).x - 20
				player.targetY = actor(2).y + 316
			CASE 4	'special tiles
				player.menuContext = 45
				'screenResetPos 
				player.targetX = actor(2).x - 20
				player.targetY = actor(2).y + 148
			CASE 5	'items
				player.menuContext = 46
				'screenResetPos 
				player.targetX = actor(2).x - 20
				player.targetY = actor(2).y + 316
			CASE 7	'credits		
				player.menuContext = 41				
			CASE 12
				player.menuContext = player.menuContextPrev
				'screenResetPos 
				player.targetX = actor(2).x - 20
				player.targetY = actor(2).y - 52
			END SELECT
		CASE 12	'Load Game
			player.menuMode = 999
			IF player.menuItem < 4 THEN
				player.saveSlot = player.menuItem - 1
				loadGame
				IF player.menuContextPrev = 1 THEN
					IF player.levelScore(0) = 0 THEN
						player.level = 0
						player.menuContext = -1						
					ELSE
						player.menuContext = 13						
					END IF
				ELSE
					player.level = 0
					player.menuContext = -1					
				END IF
			ELSE
				player.menuContext = 10				
				'screenResetPos 
				player.targetX = actor(2).x - 20
				player.targetY = actor(2).y - 52
			END IF
		CASE 13	'Clear save data?
			player.menuMode = 999
			IF player.menuItem = 2 THEN
				player.level = 0
				FOR c = 0 to 15
					player.levelScore(c) = 0
					player.levelCompletion(c) = 0
					player.levelTime(c) = 0
				NEXT c
				player.menuContext = -1
				player.menuContextPrev = 1				
			ELSE
				player.menuContext = 12				
			END IF
		CASE 14	'Highscore exit
			player.menuMode = 999
			IF player.level = 24 THEN 
				'screenResetPos 
				player.targetX = actor(2).x - 20
				player.targetY = actor(2).y - 52
				player.menuContext = 10
			ELSE
				player.level = 24
				player.menuContext = -1
			END IF
		CASE 15	'Skip demo?
			player.menuMode = 999
			player.menuContext = 0
			IF player.menuItem = 2 THEN				
				player.menuExitCtrl = 265
				player.menuContext = 0							
				player.targetX = 16
				player.targetY = 50
			ELSE
				player.menuExitCtrl = 264				
			END IF
		CASE 21 TO 23	'Secret level unlocked message
				player.menuContext = 0
				player.menuMode = 999
		CASE 30 TO 50	'Help flippable pages
			player.menuMode = 999
			SELECT CASE player.menuItem			
			CASE 1
				player.menuContext = 11				
			CASE 2
				player.menuContext = player.menuContext - 1								
			CASE 3
				player.menuContext = player.menuContext + 1								
			END SELECT
		CASE 60 TO 70	'Ending text
			player.menuMode = 999
			player.menuContext = 0
			IF player.menuItem = 1 THEN				
				player.menuExitCtrl = 264				
			ELSE								
				IF player.menuItem = 3 THEN
					player.score = player.levelScore(0)					
					player.menuExitCtrl = 265					
				END IF				
			END IF
		CASE 80 TO 120
			player.menuMode = 999			
			IF player.menuItem = 1 THEN				
				player.menuContext = player.menuContext + 1
			ELSE												
				player.menuContext = 0
			END IF
		END SELECT
	'--------------------------------------------------------------------------
	CASE 999 to 1000		' Exit menu: clear screen
		tileCopyList(0, gfx.page).x = 0
		tileCopyList(0, gfx.page).y = 0
		tileCopyList(0, gfx.page).w = 40
		tileCopyList(0, gfx.page).h = 50
		tileCopyList(1, gfx.page).y = 255
		aClearList gfx.pageOfs(gfx.page), VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset), &HDE
		aRectList gfx.WindowOfs(gfx.page), gfx.pageOfs(gfx.page), VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)
		player.menuMode = player.menuMode + 1
	CASE 1001
		IF player.targetX <> 0 OR player.targetY <> 0 THEN
			screenResetPos player.targetX, player.targetY
		END IF
		actor(1).lNext = actor(2).jump
		menuClear
		IF player.menuContext > 0 THEN
			player.menuMode = 0
			player.menuSound = SndPause
			EXIT SUB
		END IF
		IF player.menuContext = 0 THEN
			actor(2).control = player.menuExitCtrl
			player.menuMode = 0
			player.hudLock = 4
			player.menuSound = SndPause
			hudUpdate 255
		END IF
		IF player.menuContext < 0 THEN
			actor(2).control = 259
			player.menuMode = 0
			EXIT SUB
		END IF
	END SELECT
	
CASE 256	'Wait after death
	'actor(2).count = actor(2).count + 1	
	actor(2).x = 0
	actor(2).y = 0
	IF kbArray(57) THEN 
		actor(2).count = 0
		player.menuContext = 3
		player.menuMode = 0
		actor(2).control = 258
	END IF
'------------------------------------------------------------------------------
CASE 255	'Normal player
	minY = -30

	c = tileRead(actor(2).x, actor(2).y)

	IF c > 29 THEN				
		actor(2).y = (actor(2).y AND &HFFF8)
		screenTargetYlock = actor(2).y - 46		
		
		collisionFloor = 1
		player.bouncing = 0
		IF c = 52 THEN 
			spawnObject actor(2).x AND &HFFFC, actor(2).y AND &HFFF8, 99, 2, -5
			player.vecY = -38
			actor(2).y = actor(2).y - 2
			player.Bouncing = 30
			collisionFloor = 0
			minY = -38						
			soundEvent = 98
		END IF
		IF player.vecY > 0 THEN soundEvent = 16: player.vecY = 0
		screenTargetY = screenTargetYlock
	ELSE		
		collisionFloor = 0
		maxY = 30		
		'friction = 5
		IF player.ShootDelay < 6 THEN switchAnim n, 5		
		IF kbArray(44) THEN		
			player.vecY = player.vecY + 2			
			maxY = 10			
			'IF player.parent = 0 THEN kbArray(129) = 0
		ELSE			
			IF player.Bouncing > 0 THEN				
				screenTargetYlock = -1
				minY = -38
				player.Bouncing = player.Bouncing - 1
				IF player.Bouncing > 6 THEN
					player.vecY = player.vecY + fraction(14, fractionC)
				ELSE
					player.vecY = player.vecY + 5
				END IF				
			ELSE
				player.vecY = player.vecY + 15
			END IF
		END IF		
		IF actor(2).y - screenTargetYlock > 50 THEN screenTargetYlock = -1
	END IF	

    IF player.parent > 0 THEN		
		screenTargetYlock = -1
		collisionFloor = 1
		player.vecY = 0
		screenTargetY = actor(2).y - 30
    END IF	
	
	IF player.supaFly THEN 		
		screenTargetYlock = -1
		minY = -30
		maxY = 20
	END IF
	
	IF player.inWater THEN 				
		screenTargetYlock = -1
		maxY = 10
		minY = -20						
	END IF	
	
	IF (collisionFloor OR player.inWater OR player.supaFly) AND kbArray(129) AND kbArray(44) THEN		
		kbArray(129) = 0
		player.vecY = -35
		actor(n).Y = actor(n).Y - 2
		soundEvent = 0
	END IF
	
    IF collisionFloor THEN
		IF player.vecX = 0 AND player.ShootDelay < 6 THEN 
			actor(2).frame = 1
			actor(2).anim = 1
		END IF
	END IF

   c = tileRead(actor(n).x, actor(n).y - 7)
   IF c > 29 THEN
      actor(n).y = (actor(n).y AND &HFFF8) + 7
      player.vecY = 0
   END IF

   IF c = 28 THEN
      player.gotCoins = player.gotCoins + 1
	  addScore 1
      soundEvent = 92
      x = (actor(2).x) AND &HFFFC
      y = (actor(2).y - 7) AND &HFFF8
      tileWrite x, y, 0
      aUpdateTile x, y
      spawnObject x, y, 99, 0, 0
   END IF

   player.vecX = 0

	IF kbArray(75) THEN
      'player.vecX = -10 'player.vecX - speedMod
      IF actor(n).flags THEN player.vecX = -10
	  actor(n).flags = 1
      IF collisionFloor THEN
         switchAnim n, 2
      END IF
	END IF
	IF kbArray(77) THEN
      IF actor(n).flags = 0 THEN player.vecX = 10
	  'player.vecX = 10 ' player.vecX + speedMod
      actor(n).flags = 0
      IF collisionFloor THEN
         switchAnim n, 2         
      END IF
	END IF

	IF kbArray(80) OR kbArray(76) THEN screenTargetY = actor(2).y - 24	

	player.ShootDelay = player.ShootDelay + (player.ShootDelay > 0)
	IF kbArray(45) AND player.ShootDelay = 0 THEN
		player.ShootDelay = 10
		spawnObject actor(2).x, actor(2).y - 6, 100, actor(n).flags, 0
		soundEvent = 55
		actor(2).anim = 8
		actor(2).frame = 8
		actor(3).frame = 9
	END IF
	
	'CHEATS
	IF kbArray(16) THEN player.hp = 4
	IF kbArray(17) THEN
		FOR c = 0 TO 15
			player.levelScore(c) = 1
		NEXT c
	END IF
	IF kbArray(18) THEN maxY = 1
	
	IF player.hp = 0 THEN		
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
		player.hudLock = 999
		player.score = 0
		kbArray(129) = 0
		w40char.text = "         PRESS SPACE TO CONTINUE        |"
		aPrint &HFFFF, &HD0C, 0, 0, hudBufferSeg, hudBufferOfs
	END IF

	IF player.vecX > 10 THEN player.vecX = 10
	IF player.vecX < -10 THEN player.vecX = -10
	IF player.vecY > maxY THEN player.vecY = maxY
	IF player.vecY < minY THEN player.vecY = minY

	actor(n).x = actor(n).x + fraction(player.vecX, fractionC)
	actor(n).y = actor(n).y + fraction(player.vecY, fractionC)
	
	IF player.parent THEN
		actor(2).x = actor(2).x + fraction(actor(player.parent).vecX, fractionC)
		actor(2).y = actor(2).y + fraction(actor(player.parent).vecY, fractionC)
	END IF
	
	c = tileRead(actor(n).x + 2, actor(n).y - 4)
	IF c > 29 THEN
       actor(n).x = (actor(n).x AND &HFFFC) + 1
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
	
	IF kbArray(1) THEN
		actor(2).control = 258
		player.menuContext = 2
		IF player.level = 0 THEN player.menuContext = 1
		player.menuMode = 0		
	END IF
	
	tickSec = tickSec + 1
	IF tickSec > 23 THEN
		tickSec = 0
		IF player.level > 0 AND player.level < 16 THEN 
			IF player.time > 0 THEN
				player.time = player.time - 1
			ELSE
				player.hp = 0
			END IF
		END IF		
		hudUpdate &H8
	END IF			
	
CASE 264	'Ending demo
	'w40char.text = time$ + str$(actor(3).jump) + str$(actor(3).dir) + str$(actor(2).y) + str$(actor(2).vecY) + "|"
	'aPrint &HFFFF, &HF1F1, 0, 0, hudBufferSeg, hudBufferOfs
	screenTargetX = actor(2).x - 20
	screenTargetY = actor(2).y - 46
	handleCamera	
	IF actor(2).vecY > -3 THEN 
		actor(2).vecY = -3		
		w40char.text = " ESC TO SKIP DEMO                       |"
		aPrint &HFFFF, &H0E0E, 0, 0, hudBufferSeg, hudBufferOfs
	END IF
	IF actor(2).y - 46 = actor(3).dir THEN				
		actor(2).vecY = 0
		actor(2).control = 258
		player.menuMode = 0
		player.menuSound = -1
		player.menuContext = actor(3).jump + 59
		actor(3).jump = actor(3).jump + 1
		FOR c = 1 TO 64
			IF level.spawner(c).object = &HF022 AND level.spawner(c).flags = actor(3).jump THEN
				actor(3).dir = level.spawner(c).y
				EXIT FOR
			END IF
		NEXT c	
	END IF
	IF kbArray(1) THEN
		actor(2).control = 258
		player.menuContext = 15		
		player.menuMode = 0
		actor(2).vecY = 0
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
	
	
	'------------------------------------------------------------------------------
	 REM  _____  _______ _     _ _______  ______              
	 REM |     |    |    |_____| |______ |_____/              
	 REM |_____|    |    |     | |______ |    \_              
	
	 REM  _____  ______  _____ _______ _______ _______ _______
	 REM |     | |_____]   |   |______ |          |    |______
	 REM |_____| |_____] __|   |______ |_____     |    ______|
	'------------------------------------------------------------------------------
	IF actor(n).exist <> 0 THEN

	'entityCounter = entityCounter + 1

	'visDeltaX = 
	'visDeltaY = 

	IF ABS(screenX + 20 - actor(n).x) < actor(n).visX AND ABS(screenY + 24 - actor(n).y) < actor(n).visY THEN
		parent = ABS(actor(n).exist)
		IF parent > 1 THEN			
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
    IF actor(n).y < 1 THEN actor(n).y = 1
    IF actor(n).y > 630 THEN actor(n).y = 630
    IF actor(n).x < -7 THEN actor(n).x = -7
    IF actor(n).x > 85 THEN actor(n).x = 85
	
	'IF actor(n).collider AND 16 THEN
	'	playerDeltaX = ABS(actor(n).x - playerX)
	'	playerDeltaY = actor(n).y - playerY
	'	IF playerDeltaX < 3 AND playerDeltaY < 4 AND playerDeltaY > -3 AND player.vecY > -2 THEN
	'		soundEvent = 115
	'		actor(2).y = actor(n).y
	'		player.vecY = -30
	'		player.Bouncing = 20				
	'		soundEvent = 98
	'	END IF
	'END IF

	'------------------------------------------------------------------------------
	'
	'	All frames behavior checks:
	'
	'------------------------------------------------------------------------------
	SELECT CASE actor(n).control
		
	'Platform / elevator
    CASE 50 TO 53
		
	IF ABS(level.spawner(actor(n).spawner).y - playerY) < 200 THEN
		
		playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)
		
		IF player.parent = 0 THEN
			playerDeltaY = actor(n).y - playerY
			IF playerDeltaX < 5 AND playerDeltaY < 1 AND playerDeltaY > -4 AND player.vecY > -1 THEN
				actor(n).jump = 1
				soundEvent = 115
				actor(2).y = actor(n).y
				player.vecY = 0
				makeParent 2, n				
				player.parent = n
				actor(2).exist = -1				
			END IF
		ELSEIF player.parent = n THEN
			IF playerDeltaX > 4 OR playerDeltaY > 2 OR player.vecY < -5 THEN
				unParent 2				
				player.parent = 0
				actor(n).jump = 0
			END IF
        END IF
		
		' Platform type specific events
		c = tileRead(actor(n).x, actor(n).y)
		SELECT CASE actor(n).control
		' Horizontal
		CASE 50
			IF actor(n).dir THEN
				actor(n).dir = actor(n).dir - 1
			ELSE
				IF c THEN actor(n).anim = actor(n).anim XOR 1
				actor(n).dir = 3
				IF actor(n).anim THEN
					actor(n).vecX = 5
				ELSE
					actor(n).vecX = -5
				END IF
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
		' Path
		CASE 52
			IF actor(n).dir = 0 THEN				
				actor(n).dir = 7
				IF c = 1 THEN 
					actor(n).count = (actor(n).count + actor(n).anim) AND 7
						ELSEIF c = 2 THEN 
					actor(n).count = (actor(n).count - actor(n).anim) AND 7
				END IF
				SELECT CASE actor(n).count								
				CASE 0
					actor(n).vecX = 5
					actor(n).vecY = 0
				CASE 1
					actor(n).vecX = 5
					actor(n).vecY = 10
				CASE 2
					actor(n).vecX = 0
					actor(n).vecY = 10
				CASE 3
					actor(n).vecX = -5
					actor(n).vecY = 10
				CASE 4
					actor(n).vecX = -5
					actor(n).vecY = 0
				CASE 5
					actor(n).vecX = -5
					actor(n).vecY = -10
				CASE 6
					actor(n).vecX = 0
					actor(n).vecY = -10
				CASE 7
					actor(n).vecX = 5
					actor(n).vecY = -10
				END SELECT
			ELSE
				actor(n).dir = actor(n).dir - 1
			END IF
		END SELECT
	ELSE
		actor(n).count = level.spawner(actor(n).spawner).flags
		actor(n).dir = 0
		actor(n).vecX = 0
		actor(n).vecY = 0
		actor(n).x = level.spawner(actor(n).spawner).x
		actor(n).y = level.spawner(actor(n).spawner).y
	END IF
	
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
		
	'Eggy Shell
	CASE 201
		IF actor(n).exist = 1 THEN
			IF actor(n).dir < actor(n).life THEN
				spawnObject actor(n).x, actor(n).y - 2, 41, ItemGemSmall, 0
				soundEvent = SndEnemyDrop
				actor(n).vecY = -10
				actor(n).y = actor(n).y - 2
				IF actor(n).life > -48 THEN actor(n).count = 72				
			END IF
			c = tileRead(actor(n).x, actor(n).y + 5)
			IF c < 30 THEN 
				IF actor(n).vecY < 20 THEN actor(n).vecY = actor(n).vecY + 5
			ELSE			
					actor(n).vecY = 0
					actor(n).y = (actor(n).y AND &HFFF8) + 3
			END IF
			actor(n).count = actor(n).count + 1
			IF actor(n).count > 71 THEN
				actor(n).life = 5
				actor(n).control = 0
				actor(n).collider = 0
			END IF
		ELSE
			playerDeltaX = ABS(actor(n).x - playerX)
			playerDeltaY = ABS(actor(n).y - playerY)
			IF playerDeltaX < 3 AND playerDeltaY < 4 AND playerDeltaY > -3 AND player.vecY > -2 THEN
				soundEvent = 115
				actor(2).y = actor(n).y
				player.vecY = -30
				player.Bouncing = 20				
				soundEvent = 98
			END IF
			IF actor(actor(n).exist).life > -2 THEN
				actor(n).flash = 96
				actor(n).exist = 1
				actor(n).vecY = -10
				actor(n).life = -50
			END IF
		END IF
		actor(n).dir = actor(n).life
	
	'HELMET
	CASE 2
		playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - 9 - playerY)
		IF playerDeltaX < 4 AND playerDeltaY < 3 AND playerDeltaY > -3 AND player.vecY > -1 THEN
			soundEvent = 115
			player.vecY = -30
			player.Bouncing = 20			
			soundEvent = 98
			actor(n).life = -45
		END IF
		
		c = tileRead(actor(n).x, actor(n).y + 1) > 29
        IF c THEN
			actor(n).y = actor(n).y AND &HFFF8
			actor(n).vecY = 0
        ELSE			
			actor(n).vecY = 20
        END IF

		IF actor(n).life > -50 THEN
			IF actor(n).life = -49 THEN actor(n).y = actor(n).y - 6
			actor(n).life = -50
			actor(n).count = 16
			actor(n).collider = 0			
        END IF
        
		IF actor(n).count = 0 THEN 
			speed = 4
		ELSE
			speed = 0
			actor(n).vecX = 0
			IF c THEN actor(n).count = actor(n).count - 1
			actor(n).frame = 75
			IF actor(n).count = 0 THEN
				actor(n).frame = 72
				actor(n).collider = 1				
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
		IF c > 29 OR c2 < 30 AND actor(n).count = 0 THEN
			'IF actor(n).count THEN soundEvent = SndHelmetBump
			actor(n).flags = flags
			actor(n).vecX = -speed
			actor(n).x = actor(n).x AND &HFFFC + 2
		END IF		
	
	CASE 10 'Boss Magpie
		IF actor(n).x < actor(2).x THEN
			actor(n).flags = 1
		ELSE
			actor(n).flags = 0
		END IF
		IF actor(n).flash THEN actor(n).frame = 75
		IF actor(n).flash = 9 THEN soundEvent = SndBossDamage
		IF actor(n).anim < 3 AND actor(n).flash = 1 THEN 		
			actor(n).anim = 3		
		END IF
		'w40char.text = str$(actor(n).y) + str$(actor(2).y) + str$(actor(n).anim) + str$(actor(n).count) + "|"
		'aPrint &HFFFF, &HF1F1, 0, 0, hudBufferSeg, hudBufferOfs
		SELECT CASE actor(n).anim
		CASE 0
			IF actor(n).x < actor(2).x THEN actor(n).vecX = actor(n).vecX + 2
			IF actor(n).x > actor(2).x THEN actor(n).vecX = actor(n).vecX - 2
			IF actor(n).y > actor(2).y - 44 THEN actor(n).vecY = actor(n).vecY - 2
			IF actor(n).y < actor(2).y - 40 THEN actor(n).vecY = actor(n).vecY + 1
			IF actor(n).x = actor(2).x AND actor(n).vecY < actor(2).y - 40 THEN actor(n).anim = 1
		CASE 1 
			IF actor(n).jump THEN 
				c = actor(2).x + 18
			ELSE
				c = actor(2).x - 18
			END IF
			IF c < 8 THEN actor(n).jump = 1
			IF c > 72 THEN actor(n).jump = 0
			IF actor(n).x < c AND actor(n).vecX < 5 THEN actor(n).vecX = actor(n).vecX + 1
			IF actor(n).x > c AND actor(n).vecX > -5 THEN actor(n).vecX = actor(n).vecX - 1			
			IF actor(n).y < actor(2).y - 12 THEN actor(n).vecY = actor(n).vecY + 2
			IF actor(n).y > actor(2).y - 4 THEN actor(n).vecY = actor(n).vecY - 2
			IF ABS(actor(n).y = actor(2).y) < 4 AND ((tick24 AND 31) = 15) THEN 
				SpawnObject actor(n).x, actor(n).y, 111, actor(n).flags xor 1, 0
				soundEvent = 59
				actor(n).frame = 84
			END IF			
		CASE 3 			
			FOR c = 4 to 60
				IF actor(c).control = 31 AND actor(c).jump = actor(n).count THEN 
					actor(n).dir = actor(c).y
					EXIT FOR
				END IF
			NEXT c
			player.buttonFlags = player.buttonFlags OR actor(n).count
			actor(n).count = actor(n).count + actor(n).count
			actor(n).jump = actor(n).jump XOR 1
			actor(n).anim = 4			
		CASE 4 TO 5			
			IF actor(n).x < 41 THEN actor(n).vecX = actor(n).vecX + 1
			IF actor(n).x > 39 THEN actor(n).vecX = actor(n).vecX - 1					
			IF actor(n).y > actor(n).dir THEN 
				actor(n).vecY = actor(n).vecY - 2
			ELSE
				actor(n).vecY = actor(n).vecY + 2
				actor(n).anim = 5
			END IF
			IF actor(n).anim = 5 THEN				
				IF ABS(actor(n).y - actor(2).y) < 24 THEN actor(n).anim = 0
			END IF
			IF actor(n).count > 64 THEN
				IF (tick24 AND 7) = 7 THEN
					SpawnObject actor(n).x, actor(n).y + 8, 200, 0, 0
					soundEvent = SndExplode
					actor(n).vecY = -1
				END IF
				IF actor(n).y < 24 THEN
				IF level.spawner(63).object = 0 THEN 
						SpawnObject actor(n).x -9, 32, 63, 63, 0
						level.spawner(63).object = -63
					END IF					
					actor(n).control = 0
					actor(n).vecX = 0
					actor(n).vecY = 0
					FOR c = 4 TO 60
						actor(c).collider = 0
					NEXT c
				END IF
			END IF
		END SELECT
		IF actor(n).y < 15 THEN actor(n).y = 15
		IF actor(n).vecX > 8 THEN actor(n).vecX = 8
		IF actor(n).vecX < -8 THEN actor(n).vecX = -8
		IF actor(n).vecY > 15 THEN actor(n).vecY = 15
		IF actor(n).vecY < -15 THEN actor(n).vecY = -15
	
	CASE 20	'Water / Lava
	playerDeltaX = ABS(actor(n).x - playerX)
	playerDeltaY = actor(n).y - playerY
	IF playerDeltaX < 9 THEN
		IF playerDeltaY < 1 AND playerDeltaY > -8 THEN
			player.inWater = 1
			IF actor(n).anim = 64 THEN player.hp = 0
		ELSEIF playerDeltaY > 1 AND playerDeltaY < 8 THEN
			player.inWater = 0
		END IF			
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
					t = 0
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
		playerDeltaY = actor(n).y - playerY
		IF playerDeltaX < 3 AND playerDeltaY < 7 AND playerDeltaY > -6 THEN
			damagePlayer
			actor(n).life = 0
		END IF
	
	'Flamer
	CASE 17 TO 19        				
		IF ABS(actor(2).y - actor(n).jump) < 60 THEN			
			playerDeltaX = ABS(actor(n).x - playerX)
			playerDeltaY = actor(n).y - playerY
			IF playerDeltaX < 3 AND playerDeltaY < 7 AND playerDeltaY > -6 THEN
				damagePlayer			
			END IF
			c = actor(n).dir			
			actor(n).vecX = angle.x(c)
			actor(n).vecY = angle.y(c)
			actor(n).count = actor(n).count + 1
			IF actor(n).count > 9 THEN					
				actor(n).dir = (actor(n).dir + 7) AND 63				
				c = actor(n).spawner
				actor(n).x = level.spawner(c).x
				actor(n).y = level.spawner(c).y
				actor(n).count = 0
			END IF
		ELSE
			actor(n).vecX = 0
			actor(n).vecY = 0
		END IF
		IF level.spawner(actor(n).spawner).object > 0 THEN 
			actor(n).life = 0
			actor(n).control = 0
			actor(n).spawner = 0
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
				actor(n).life = 8
				actor(n).count = 10
				actor(n).control = 0
				actor(n).vecY = -5
				actor(n).y = actor(n).y AND &HFFF8
				actor(n).flash = 24
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

		IF actor(n).life < 24 AND actor(n).frame > 22 THEN actor(n).flash = 36

		IF playerDeltaX < 5 AND ABS(actor(2).y - actor(n).y) < 8 THEN
            level.spawner(actor(n).spawner).object = 0
            soundEvent = 110            		
			IF actor(n).frame = ItemSuperOrb THEN 
				soundEvent = 132
				player.supaFly = 1
				hudUpdate 2
			END IF
			addScore actor(n).frame
			IF actor(n).frame = ItemKey THEN				
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
		IF actor(n).jump > 0 THEN 
			actor(n).jump = actor(n).jump + 1			
		END IF
		SELECT CASE actor(n).jump
		CASE 0
		IF playerDeltaX < 5 AND playerDeltaY < 10 AND player.hasKey THEN
			actor(n).jump = 1
			player.score = player.score + 1000
			player.gotChests = player.gotChests + 1
			player.hasKey = player.hasKey - 1									
			actor(n).life = 10
			actor(n).frame = 0
			'actor(n).vecY = -5			
			level.spawner(actor(n).spawner).object = 0
			hudUpdate 16
			soundEvent = SndGetChest
			spawnObject actor(n).x - 2, actor(n).y - 14, 41, ItemGemSmall, 0
		END IF				
		CASE 1
		spawnObject actor(n).x + 2, actor(n).y - 15, 41, ItemGemSmall, 0
		CASE 2
		spawnObject actor(n).x - 3, actor(n).y - 12, 41, ItemGemSmall, 0
		soundEvent = SndGetChest
		CASE 3
		spawnObject actor(n).x + 3, actor(n).y - 14, 41, ItemGemSmall, 0			
		CASE 4
		spawnObject actor(n).x, actor(n).y - 16, 41, ItemGemBig, 0	
		CASE 5
		actor(n).control = 0
       	END SELECT
	'Dragon
	CASE 3
		IF actor(n).dir = 0 THEN
			playerDeltaX = ABS(actor(n).x - playerX)
			playerDeltaY = ABS(actor(n).y - playerY)
			IF playerDeltaX < 25 AND playerDeltaY < 40 THEN actor(n).dir = 1 
		ELSE
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
		END IF
		
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
		
		'actor(n).jump = actor(n).jump + 1
         
         SELECT CASE actor(n).count
         CASE 0
            IF playerDeltaX < 18 AND playerDeltaY < 32 AND actor(2).y > actor(n).y OR actor(n).life > -2 THEN
             actor(n).count = 1
             actor(n).frame = 86
             actor(n).jump = actor(2).x
            END IF

         CASE 1
            IF actor(n).y < actor(2).y THEN
               actor(n).vecY = 12
               IF actor(n).x < actor(2).x THEN actor(n).vecX = actor(n).vecX + 1
               IF actor(n).x > actor(2).x THEN actor(n).vecX = actor(n).vecX - 1
            ELSE
              actor(n).vecY = 10
              actor(n).vecX = mulTable(3, actor(n).vecX)
              actor(n).count = 2
              actor(n).jump = actor(2).y - 30
              IF actor(n).jump < 20 THEN actor(n).jump = 20
			  actor(n).dir = actor(2).x
            END IF
         
         CASE 2
          IF actor(n).y > actor(n).jump THEN
            actor(n).vecY = actor(n).vecY - 5
		   ELSE
            actor(n).vecX = 0
			actor(n).count = 3
			actor(n).jump = 0
          END IF

         CASE 3
          actor(n).jump = actor(n).jump + 1
          actor(n).vecY = mulTable(2, actor(n).vecY)
          IF actor(n).jump > 10 THEN actor(n).count = 1
         END SELECT

         IF actor(n).vecY > 20 THEN actor(n).vecY = 20
         IF actor(n).vecY < -15 THEN actor(n).vecY = -15
         IF actor(n).vecX > 7 THEN actor(n).vecX = 7
         IF actor(n).vecX < -7 THEN actor(n).vecX = -7
       
	'SKULL
	CASE 6         		
		IF actor(n).flags THEN
			actor(n).vecX = -8
			IF actor(n).x < screenX - 4 THEN actor(n).flags = 0 ' soundEvent = 151
		ELSE
			actor(n).vecX = 8
			IF actor(n).x > screenX + 44 THEN actor(n).flags = 1 ' soundEvent = 151
		END IF
		c = (tick24 + actor(n).count) AND 63
		actor(n).y = actor(n).count + mulTable(1, angle.y(c))
		
	CASE 8 'SPIKY
		IF actor(n).count = 0 THEN
			playerDeltaX = ABS(actor(n).x - playerX)		
			playerDeltaY = ABS(actor(n).y - playerY)			
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
				IF playerDeltaX < 30 AND playerDeltaY < 35 THEN soundEvent = 178
			ELSE
				IF c2 < 30 THEN
					actor(n).dir = (actor(n).dir - 1) AND 3
					actor(n).count = 2
					IF playerDeltaX < 30 AND playerDeltaY < 35 THEN soundEvent = 178
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
	
	CASE 11 'FIRE BREATH
		'c = (tick12 + actor(n).y) AND 31
		IF actor(n).jump < 24 THEN actor(n).jump = actor(n).jump + 1
		IF actor(n).jump > 19 THEN
			IF ABS(actor(n).y - playerY) < 45 THEN
				IF actor(n).jump = 20 THEN
					actor(n).frame = 48
					soundEvent = 66
				END IF
				actor(n).jump = actor(n).jump + 1
			END IF						
			IF actor(n).jump > 24 THEN
				actor(n).jump = 0
				c = 0
				IF actor(2).y > actor(n).y + 5 THEN c = 5
				IF actor(2).y > actor(n).y + 20 THEN c = 10
				IF actor(2).y < actor(n).y - 5 THEN c = -5
				IF actor(2).y < actor(n).y - 20 THEN c = -10
				spawnObject actor(n).x + actor(n).dir, actor(n).y + 4, 111, actor(n).flags, c
			END IF
		ELSE
           actor(n).frame = 47
        END IF
    
    CASE 15 'SPIKE BALL BOUNCE
        c = tileRead(actor(n).x, actor(n).y + 1)
        IF c > 29 AND actor(n).vecY > 10 THEN
			actor(n).y = actor(n).y AND &HFFF8 - 1			
			actor(n).vecY = -33
			aSoundNote 40
		ELSE
			actor(n).vecY = actor(n).vecY + 6
			IF actor(n).vecY > 20 THEN actor(n).vecY = 20
        END IF
	
	CASE 30	'BUTTON
		playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)
		IF playerDeltaX < 3 AND playerDeltaY < 4 AND player.vecY > -1 THEN
			player.buttonFlags = player.buttonFlags OR actor(n).jump
			actor(n).control = 0
			actor(n).spawner = 0
			actor(n).frame = 27
			actor(2).y = actor(n).y - 5
			player.vecY = -15
			soundEvent = SndButton
			'FOR c = 4 TO 60
			'	IF actor(c).control = 31 THEN
			'		IF actor(n).jump = actor(c).jump THEN 
			'			actor(c).anim = 1
			'			actor(c).count = 4
			'			actor(c).frame = 38
			'			actor(c).y = actor(c).y + 8
			'		END IF
			'	END IF
			'NEXT c
		END IF
	
	CASE 31 'DOOR OPEN or TILE GROW
		IF actor(n).anim THEN			
			actor(n).count = actor(n).count + 1
			IF actor(n).count > 4 THEN
				actor(n).count = 0
				actor(n).y = actor(n).y - 8
				c = tileRead(actor(n).x, actor(n).y)				
				IF actor(n).dir = 31 THEN
					IF c < 40 THEN 
						actor(n).life = 0
						actor(n).control = 0
					ELSE
						tileWrite actor(n).x, actor(n).y, 0
						aUpdateTile actor(n).x, actor(n).y
						soundEvent = SndTileGrow
					END IF
				ELSE
					IF c > 0 THEN
						actor(n).life = 0
						actor(n).control = 0
					ELSE
						tileWrite actor(n).x, actor(n).y, 49
						aUpdateTile actor(n).x, actor(n).y
						soundEvent = SndTileGrow
					END IF
				END IF
			END IF
		ELSE
			IF player.buttonFlags AND actor(n).jump THEN 
				actor(n).anim = 1
				actor(n).count = 4
				actor(n).frame = 38
				actor(n).y = actor(n).y + 8
			END IF
		END IF
	
	CASE 33	
		actor(n).y = actor(n).dir + mulTable(2, angle.x(tick24))
		playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)		
		IF playerDeltaX < 4 AND playerDeltaY < 16 THEN									
			actor(n).count = 1
			w40char.text = "          PRESS UP TO READ NOTE         |"
			aPrint &HFFFF, &HE0E, 0, 0, hudBufferSeg, hudBufferOfs
			IF kbArray(72) THEN
				actor(2).control = 258
				player.menuMode = 0
				player.menuContext = actor(n).jump
			END IF
		ELSE
			IF actor(n).count THEN
				player.hudLock = 0
				hudUpdate 255
				actor(n).count = 0
			END IF
		END IF
		
		
	'CHECKPOINT
	CASE 65
		playerDeltaY = ABS(actor(n).y - playerY)
		
		IF playerDeltaY < 32 THEN
			c = 0
			'IF player.wave = actor(n).flags THEN			
				IF player.wavePrev <> actor(n).flags THEN
					'debugOut "wave+ " + str$(player.wave) + str$(player.wavePrev) + str$(actor(n).flags)					
					'player.wave = actor(n).flags '+ 1
					IF player.wavePrev < actor(n).flags THEN
						player.wavePrev = player.wave
						player.wave = actor(n).flags + 1
					ELSE
						player.wavePrev = player.wave
						player.wave = actor(n).flags
					END IF
					c = 1
				END IF
			'END IF
			'ELSE				
			'	IF player.wavePrev > actor(n).flags THEN
			'		'debugOut "wave- " + str$(player.wave) + str$(player.wavePrev) + str$(actor(n).flags)
			'		player.wavePrev = player.wave
			'		player.wave = actor(n).flags
			'		c = 1
			'	END IF
			'END IF
			IF c THEN
				FOR nn = 4 TO 60
					IF ABS(actor(nn).y - actor(2).y) > 240 THEN
						IF actor(nn).control > 0 AND actor(nn).control < 65 THEN
							level.spawner(actor(nn).spawner).object = level.spawner(actor(nn).spawner).object AND &HFFF
							'debugOut "destroyed " + str$(actor(nn).control) + "  (" + str$(actor(nn).spawner) + " - " + hex$(level.spawner(actor(nn).spawner).object)
						END IF
						IF actor(nn).control <> 65 THEN 
							actor(nn).control = 0
							actor(nn).life = 0
						END IF
					END IF
				NEXT nn
				handleSpawn player.wave
			END IF
		END IF

	'EXIT / SECRET LEVEL UNLOCK
	CASE 63						
		actor(n).y = actor(n).dir + mulTable(2, angle.x(tick24))
		
		playerDeltaX = ABS(actor(n).x - playerX)
		playerDeltaY = ABS(actor(n).y - playerY)
		IF actor(n).frame = 60 THEN
			IF playerDeltaX < 6 AND playerDeltaY < 10 THEN
				actor(2).control = 260
				actor(2).count = 0
				actor(n).control = 0
				actor(n).exist = 0
				FOR b = 4 to 60
					actor(b).exist = 0
				NEXT b
				actor(3).lNext = 1
				actor(3).lPrev = 2
				actor(2).lPrev = 1				
				actor(2).lNext = 3
				actor(1).lNext = 2
				actor(1).lPrev = 3
				FOR b = 0 TO 63 STEP 8
					xs = angle.x(b) \ 2
					ys = angle.y(b) \ 2
					spawnObject actor(n).x, actor(n).y, 93, xs, ys
				NEXT b
			END IF
		ELSE
			IF playerDeltaX < 5 AND playerDeltaY < 8 THEN
				actor(n).life = 8
				actor(n).frame = 30
				actor(n).control = 0
				actor(2).control = 263				
				player.menuContext = 9 + actor(n).jump
				player.levelCompletion(actor(n).jump) = 1				
			END IF
		END IF

		'LEVEL DOOR
        CASE 64
			playerDeltaX = ABS(actor(n).x - playerX)			
			playerDeltaY = actor(2).y - actor(n).y
			IF playerDeltaX < 4 AND playerDeltaY > 4 AND playerDeltaY < 32 THEN
				IF actor(n).count = 0 THEN 
					IF player.levelScore(actor(n).jump) = 0 THEN
						w40char.text = "         PRESS UP TO ENTER LEVEL        |"
						aPrint &HFFFF, &HE0E, 0, 0, hudBufferSeg, hudBufferOfs
					ELSE
						w40char.text = " SCORE         TIME       COMPLETE     " 
						a$ = STR$(player.levelScore(actor(n).jump))
						mid$(w40char.text, 7, len(a$)) = a$
						
						a$ = STR$(player.levelTime(actor(n).jump))
						mid$(w40char.text, 20, 5) = a$
						
						a$ = STR$(player.levelCompletion(actor(n).jump)) + "%"
						mid$(w40char.text, 35, 5) = a$
						
						mid$(w40char.text, 41, 1) = "|"
						aPrint &HFFFF, &HE0F, 0, 0, hudBufferSeg, hudBufferOfs
					END IF
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
	
	x = actor(n).x + animation(frame).x - screenX
	y = actor(n).y + animation(frame).y - screenY

	IF actor(n).collider THEN           
		IF frame AND x > -16 AND x < 40 AND y > -10 AND y < 50 THEN
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
		IF frame AND flash > 0 AND x > -16 AND x < 40 AND y > -10 AND y < 50 THEN
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
					level.spawner(actor(n).spawner).object = level.spawner(actor(n).spawner).object AND &H8FFF
				END IF
				actor(n).spawner = 0
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
					actor(n).frame = ItemGemSmall
					c = tileRead(actor(n).x, actor(n).y)
					IF c > 29 THEN
						actor(n).count = actor(n).y + 8
					ELSE
						actor(n).count = actor(n).y
					END IF
				ELSE
					destroyObject n
				END IF
			END IF
			actor(n).life = actor(n).life - 1
		END IF	
	END IF 'Actor-exists check
LOOP WHILE actor(n).lNext > 1
 
 IF gfx.page THEN tick12 = (tick12 + 1) AND 63

 spriteList(spriteListIndex, gfx.page).y = 255

 IF soundEvent > -1 THEN aSoundFX 0, soundEvent

 IF player.score > scoreDelta THEN
	hudUpdate 4
 END IF

 tick24 = (tick24 + 1) AND 63
	
	'w40char.text = str$(player.wave) + str$(player.waveprev) + "|"
	'aPrint &HFFFF, &HF1F1, 0, 0, hudBufferSeg, hudBufferOfs
END SUB

'==============================================================================
SUB handleCollision
	soundEvent = -1
	'Collision lists
	' 1 = Player to Enemy
	' 2 = Bullet to Enemy
	' 3 = Bullet to Passive
	
	'Affect player
	IF actor(2).control = 255 AND actor(2).flash = 0 AND colCount(1) > -1 THEN
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
		FOR cList = 1 to 3 step 2
		IF colCount(cList) > -1 THEN
			FOR a = 0 TO colCount(cList)
				IF px > colList(cList, a).x0 - 2 AND px < colList(cList, a).x1 + 2 AND py > colList(cList, a).y0 - 3 AND py < colList(cList, a).y1 + 3 THEN
					n2 = colList(cList, a).index
					IF actor(n2).control = 2 THEN
						IF actor(n).vecX > 0 THEN
							actor(n2).flags = 0
						ELSE
							actor(n2).flags = 1
						END IF
					END IF
					IF actor(n2).life > -999 THEN
						actor(n2).flash = 10
						actor(n2).life = actor(n2).life + 1 + player.supaFly
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
		END IF
		NEXT cList
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
	IF player.level > 0 THEN
		IF flags AND 1 THEN
			w40char.text = " KEYS:    0000000              TIME:    |"
			aPrint &HFFFF, &HE0C, 0, 0, hudBufferSeg, hudBufferOfs
		END IF
		IF flags AND 8 THEN
			a$ = STR$(player.time)
			w40char.text = a$ + "|"
			aPrint &HFFFF, &HF0E, 39 - LEN(a$), 0, hudBufferSeg, hudBufferOfs
		END IF
		IF flags AND 16 THEN
			w40char.text = STR$(player.hasKey) + "|"
			aPrint &HFFFF, &HF0E, 6, 0, hudBufferSeg, hudBufferOfs
		END IF
	ELSE
		IF flags AND 1 THEN
			w40char.text = "          0000000                       |"
			aPrint &HFFFF, &HE0C, 0, 0, hudBufferSeg, hudBufferOfs
		END IF
	END IF
	IF flags AND 2 THEN
		IF player.hp < 4 THEN
			w40char.text = "#$#$#$#$|"
			aPrint &HFFFF, &H505, 19, 0, hudBufferSeg, hudBufferOfs
		END IF
		a$ = "#$#$#$#$"
		IF player.hp THEN
			w40char.text = MID$(a$, 1, player.hp * 2) + "|"
			IF player.supaFly THEN w40char.text = "--POW!--|"
			aPrint &HFFFF, &HCECE, 19, 0, hudBufferSeg, hudBufferOfs
		END IF
	END IF
	IF flags AND 4 THEN
		a$ = STR$(player.score)
		c = LEN(a$)
		a$ = MID$(a$, 2, c)
		w40char.text = a$ + "|"
		aPrint &HFFFF, &HC0E, 18 - c, 0, hudBufferSeg, hudBufferOfs
	END IF
END SUB

SUB initGame
	OPEN "debug.txt" FOR OUTPUT AS #1
		PRINT #1, TIME$
	CLOSE #1
	elfInit
	
	loadSounds "sound"
	
	loadSprites "sprites0", 0
	player.spriteSet = "sprites0"
	
	initTestGame
	'initMainMenu
END SUB

SUB initTestGame
	player.level = 24
	IF LEN(COMMAND$) THEN player.level = VAL(COMMAND$)
	DO		
		main
		IF player.level = -1 THEN 
			quit
		END IF
	LOOP
END SUB

'==============================================================================
 REM _______ _______ _____ __   _
 REM |  |  | |_____|   |   | \  |
 REM |  |  | |     | __|__ |  \_|                             
'==============================================================================
 SUB main
	
	'Load level
	w40char.text = " LOADING                               |"
	aPrint &HFFFF, &HE0C, 0, 0, hudBufferSeg, hudBufferOfs
	aPageFlip gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
	loadLevel
	
	'Clear all actors.
	for n = 1 to 60
		actor(n).exist = 0
		actor(n).parentDX = 0
		actor(n).parentDY = 0
		actor(n).lNext = 1
		actor(n).lPrev = 1
		actor(n).control = 0
	next n
	
	'World actor, or linked-list first object.
	actor(1).x = 0
	actor(1).y = 0
	actor(1).lNext = 2
	actor(1).lPrev = 3
	actor(1).exist = 0
	
	'Collision list must be cleared, because it's handled before behavior.
	colCount(1) = -1
	colCount(2) = -1
	colCount(3) = -1
	
	player.hp = 4
	player.time = 300
	player.hasKey = 0
	kbArray(129) = 1
	player.inWater = 0
	player.bouncing = 0
	player.supaFly = 0
	player.parent = 0
	
	player.wave = 0
	player.wavePrev = -1
	
	player.score = 0
	player.gotCoins = 0
	player.gotKeys = 0
	player.gotChests = 0		
	player.buttonFlags = 0
	
	'Player body.
	actor(2).flash = 0
	actor(2).lPrev = 1
	actor(2).lNext = 3
	actor(2).exist = -1
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
	actor(2).frame = 1
	actor(2).anim = 1
	actor(2).visX = 999
	actor(2).visY = 999
	actor(2).count = 0
	playerOldX = -999
	playerOldY = -999
	
	'Player head, parented to body.
	actor(3).flash = 0
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
	
	tick24 = 0
	
	handleSpawn 0
	
	updateText = 1
	
	IF player.level = 0 THEN
		player.score = player.levelScore(0)
		handleSpawn 1
		handleSpawn 2
		handleSpawn 3
		player.wave = 1
		player.wavePrev = 0
	END IF
	
	IF player.level = 24 THEN
		'hudTextLoad &H6E0, 0
		player.hudLock = 999
		actor(2).control = 258
		actor(2).frame = 0
		player.menuContext = 10
		screenResetPos actor(2).x - 20, actor(2).y - 52
	ELSE
		'hudTextLoad &H1E0 + player.level * 160, 0
		player.hudLock = 0
		hudUpdate 255
		screenResetPos actor(2).x - 20, actor(2).y - 46
	END IF
	
	player.levelPrev = player.level
	
	CALL aTimerReset
		
	DO
		CALL aTimerWait
		handleCollision
		handleBehaviors
		CALL aSoundPlay
		handleScroll
		aSpriteList gfx.pageOfs(gfx.page), VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)		
		IF actor(2).control = 259 THEN aSoundStop: EXIT SUB						
		aPageFlip gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
		gfx.page = gfx.page XOR 1
		aRectList gfx.WindowOfs(gfx.page), gfx.pageOfs(gfx.page), VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)
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
		actorListNewLast n
		actor(n).control = 0
		actor(n).life = 15
		actor(n).frame = 34
		actor(n).vecX = spawnerFlags
		actor(n).vecY = spawner
		actor(n).visY = 40
	CASE 258	'Empty
		actorListNew n
		actor(n).control = 0
		actor(n).frame = spawnerFlags
	CASE 200	'Explode
		actorListNew n
		actor(n).control = 0
		actor(n).life = 12		
		actor(n).collider = 0
		actor(n).frame = 34		
	CASE 99   'Clear Tile
		actorListNew n
		actor(n).control = 0
		actor(n).spawner = 0
		SELECT CASE spawnerFlags
		CASE 0							'Twinkle
			actor(n).life = 3
			actor(n).flash = 0
			actor(n).frame = 38
		CASE 1							'Debris left/right
			actor(n).life = 3
			actor(n).flash = 8
			actor(n).frame = 57
			actor(n).vecX = spawner
		CASE 2							'Bounce
			actor(n).life = 2
			actor(n).frame = 39
			actor(n).vecY = spawner
		CASE 3							'Mover
			actor(n).life = 40
			actor(n).frame = 39
			actor(n).vecX = spawner
			actor(n).visX = 70
			actor(n).visY = 70
			actor(n).control = 99
		CASE 4							'---
			actor(n).life = 4
			actor(n).frame = 39
			actor(n).vecY = spawner
			actor(n).visX = 999
			actor(n).visY = 999
			actor(n).control = 99
		END SELECT
		actor(n).jump = spawnerFlags
	
	CASE 93		'Twinkle particle
		actorListNewLast n   
		actor(n).control = 0
		actor(n).frame = 30
		actor(n).life = 6
		actor(n).vecX = spawnerFlags
		actor(n).vecY = spawner
		actor(n).spawner = 0
	CASE 100	'Player Bullet
		actorListNewLast n
		actor(n).control = 100
		actor(n).life = 7
		actor(n).flags = spawnerFlags
		actor(n).collider = 2
		actor(n).vecX = 30 + (spawnerFlags <> 0) * 60
		actor(n).frame = 13
	CASE 102	'Enemy Bullet
		actorListNewLast n
		actor(n).control = 102
		actor(n).life = 24
		actor(n).vecX = 10 + (spawnerFlags <> 0) * 20
		actor(n).vecY = 5
		actor(n).frame = 40
		actor(n).anim = 40
		actor(n).visX = 40
		actor(n).visY = 40
	CASE 104	'DRIP
		actorListNewLast n
		actor(n).control = 104
		actor(n).life = 18
		actor(n).vecX = spawnerFlags
		actor(n).vecY = -30
		actor(n).frame = 82
		actor(n).anim = 214
		actor(n).visY = 40
	CASE 111	'FIRE PARTICLE
		actorListNewLast n
		actor(n).control = 102
		actor(n).life = 15
		actor(n).vecX = 7 + (spawnerFlags <> 0) * 14
		actor(n).vecY = spawner
		actor(n).frame = 50
		actor(n).visX = 70
		actor(n).visY = 40   
	CASE 112	'FIRE BOMB
		actorListNewLast n
		actor(n).control = 112
		actor(n).life = 18
		actor(n).vecX = 0
		actor(n).vecY = 5
		actor(n).frame = 50
		actor(n).visX = 40
		actor(n).visY = 50   
	CASE 113	'FIRE PIT FIRE
		actorListNew n
		actor(n).control = 102
		actor(n).life = 4
		actor(n).vecX = 0
		actor(n).vecY = -5
		actor(n).frame = 50
		actor(n).visX = 25
		actor(n).visY = 40
		actor(n).flash = 50
	CASE 114	'FIRE, arbitrary angle
		actorListNewLast n
		actor(n).control = 102
		actor(n).life = 5
		actor(n).vecX = angle.x(spawnerFlags)
		actor(n).vecY = angle.y(spawnerFlags)
		actor(n).frame = 50
		actor(n).visX = 70
		actor(n).visY = 40   
	CASE 1	'Eggy
		actorListNew n
		c = spawnerFlags > 1
		actor(n).exist = 1 + c + c
		actor(n).flags = spawnerFlags AND 1
		actor(n).control = 1   
		actor(n).life = -2
		actor(n).collider = 1
		actor(n).frame = 15
		actor(n).anim = 60
		actor(n).count = 0
		actor(n).visX = 25
		actor(n).visY = 40
		actor(n).spawner = spawner
	'---------------------
		IF spawnerFlags > 1 THEN
			c = n
			n = getSpawnSlot
			actorListNewLast n			
			actor(n).exist = c
			actor(n).X = x
			actor(n).Y = y - 11
			actor(n).vecX = 0
			actor(n).vecY = 0
			actor(n).parentDX = x
			actor(n).parentDY = y
			actor(n).control = 201
			actor(n).life = -80
			actor(n).collider = 3
			actor(n).spawner = 0
			actor(n).visX = 35
			actor(n).visY = 60
			actor(n).frame = 19
			actor(n).count = 0
			actor(n).dir = -5
			actor(n).flags = 0
		END IF
	CASE 2	'HELMET
		actorListNew n
		actor(n).control = 2   
		actor(n).life = -50
		actor(n).collider = 1
		actor(n).frame = 72
		actor(n).count = 0		
		actor(n).visX = 60
		actor(n).visY = 50
		actor(n).spawner = spawner
	CASE 3	'DRAGON
		actorListNew n
		actor(n).control = 3   
		actor(n).life = -2
		actor(n).collider = 1
		actor(n).frame = 89
		actor(n).count = 0
		actor(n).visY = 60
		actor(n).visX = 55
		actor(n).dir = 0
		actor(n).spawner = spawner
	CASE 4	   'DROOLY
		actorListNew n
		actor(n).control = 4   
		actor(n).life = -1
		actor(n).collider = 1
		actor(n).vecX = 2
		actor(n).frame = 78
		actor(n).anim = 208
		actor(n).visY = 35
		actor(n).spawner = spawner
	CASE 5	'BABAT
		actorListNew n
		actor(n).control = 5   
		actor(n).spawner = spawner
		actor(n).life = -2
		actor(n).dir = y
		actor(n).collider = 1
		actor(n).frame = 85
		actor(n).visX = 60
		actor(n).visY = 60
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
		actor(n).spawner = spawner
		IF object = 8 THEN
			actor(n).jump = 0
		ELSE
			actor(n).jump = 1
		END IF
	CASE 10	'BOSS
		'head
		actorListNew n		
		actor(n).exist = -1
		actor(n).control = 10
		actor(n).visX = 80
		actor(n).visY = 512
		actor(n).frame = 80
		actor(n).collider = 1
		actor(n).life = -50
		actor(n).jump = 0
		actor(n).dir = 0
		actor(n).count = 1
		actor(n).anim = 0	
		c = n		
		'body
		n = getSpawnSlot
		actorListNew n		
		actor(n).exist = 1
		actor(n).flags = 0		
		actor(n).frame = 90
		actor(n).x = x
		actor(n).y = y
		actor(n).life = -999
		actor(n).collider = 1
		actor(n).vecX = 0
		actor(n).vecY = 0
		actor(n).visX = 80
		actor(n).visY = 512
		makeParent n, c
		'tail
		n = getSpawnSlot
		actorListNew n		
		actor(n).exist = 1
		actor(n).flags = 0		
		actor(n).frame = 95
		actor(n).x = x
		actor(n).y = y
		actor(n).life = -999
		actor(n).collider = 0
		actor(n).vecX = 0
		actor(n).vecY = 0
		actor(n).visX = 80
		actor(n).visY = 512
		makeParent n, c		
	CASE 11 TO 12	'FIRE BREATH R / L
	   actorListNew n
	   actor(n).control = 11   
	   actor(n).spawner = spawner
	   actor(n).life = -999
	   actor(n).frame = 47
	   actor(n).anim = 150
	   actor(n).visY = 25
	   actor(n).visX = 21
	   actor(n).jump = 15
	   IF object = 12 THEN 
			actor(n).flags = 1
			actor(n).dir = -2
		ELSE
			actor(n).dir = 2
		END IF
	CASE 15			'Bouncing spike ball
		actorListNew n
		actor(n).collider = 1
		actor(n).control = 15    
		actor(n).life = -999
		actor(n).frame = 45
		actor(n).visX = 24
		actor(n).visY = 55		
		actor(n).spawner = spawner	
	CASE 17 TO 19	'Flamer
		actorListNewLast n		
		actor(n).control = 17
		actor(n).x = x
		actor(n).y = y
		actor(n).count = 0
		actor(n).frame = 50
		c = (object > 17)
		actor(n).dir = 0'1 - c - c
		actor(n).visX = 80
		actor(n).visY = 80
		actor(n).life = -999		
		actor(n).jump = y
		actor(n).spawner = spawner
		n = getSpawnSlot
		actorListNew n
		actor(n).life = -999
		actor(n).control = 17
		actor(n).exist = 1		
		actor(n).x = x
		actor(n).y = y
		actor(n).count = 5
		actor(n).frame = 50		
		actor(n).dir = 0 '1 - c - c
		actor(n).visX = 80
		actor(n).visY = 80
		actor(n).jump = y
		actor(n).spawner = spawner
		'actor(n).anim = 0
		'actor(n).control = object
		'actor(n).flags = 0
		'actor(n).exist = 1
		'actor(n).spawner = spawner
		'actor(n).life = -50
		'actor(n).jump = 0
		'actor(n).frame = 0		
		'actor(n).visX = 50
		'actor(n).visY = 50
	CASE 20		'Water / Lava
		actorListNew n
		actor(n).anim = 61 + spawnerFlags
		actor(n).control = 20
		actor(n).spawner = spawner
		actor(n).frame = 61 + spawnerFlags
	CASE 30		'Button
		actorListNew n
		actor(n).jump = spawnerFlags
		actor(n).control = 30
		actor(n).spawner = spawner
		actor(n).frame = 26
	CASE 31 TO 32		'Door opener or Tile Grow
		actorListNew n
		actor(n).jump = spawnerFlags
		actor(n).frame = 30
		actor(n).control = 31
		actor(n).anim = 0
		actor(n).spawner = spawner
		actor(n).dir = object
		actor(n).visY = 40
	CASE 33				'Info letter
		actorListNew n
		actor(n).visY = 50
		actor(n).frame = 68
		actor(n).control = 33
		actor(n).spawner = spawner
		actor(n).jump = spawnerFlags + 80		
		actor(n).dir = y
	CASE 34				'Demo camera target
		IF spawnerFlags = 1 THEN
			actor(3).dir = y
		END IF
		IF spawnerFlags = 0 THEN
			actor(2).x = x + 18
			actor(2).y = y + 46
			actor(2).control = 264			
			actor(3).jump = 1
			actor(2).frame = 0
			actor(3).frame = 0			
		END IF
	CASE 41	   'PICKUP GEM/KEY/ETC...
		actorListNew n
		actor(n).control = 41
		actor(n).life = 60
		actor(n).count = y
		actor(n).vecY = -20
		actor(n).frame = spawnerFlags		
		actor(n).visY = 34
	CASE 44   'KEY
		actorListNew n
		actor(n).control = 41
		actor(n).spawner = spawner		
		actor(n).frame = 21		
	CASE 45   'CHEST
		actorListNew n
		actor(n).jump = 0
		actor(n).control = 45
		actor(n).spawner = spawner		
		actor(n).frame = 20
		actor(n).visY = 35
	CASE 46		'SUPER ORB
		IF player.levelCompletion(player.level) = 100 THEN
			actorListNew n
			actor(n).control = 41
			actor(n).spawner = spawner			
			actor(n).frame = ItemSuperOrb
		END IF
    CASE 50 TO 53  	'Platforms / Elevators
		actorListNew n		
		actor(n).jump = 0
		actor(n).control = object
		actor(n).dir = 0
		actor(n).spawner = spawner
		actor(n).life = -999
		actor(n).frame = 18
		actor(n).visX = 999
		actor(n).visY = 480
		actor(n).vecY = 0
		actor(n).vecX = 0
		actor(n).anim = 0
		IF object = 50 THEN actor(n).vecX = 5
		IF object > 51 THEN 
			actor(n).control = 52
			actor(n).dir = 7
			actor(n).count = spawnerFlags
			actor(n).anim = object - 51
		END IF
	CASE 63		'EXIT
		actorListNew n
		actor(n).dir = y
		actor(n).spawner = spawner		
		actor(n).frame = 60
		actor(n).control = 63		
	CASE 64		'LEVEL ENTRANCE
		actorListNew n
		actor(n).visY = 55
		'teleport player at door if s/he was just there
		IF player.levelPrev = spawnerFlags THEN
			actor(2).x = x
			actor(2).y = y + 26
		END IF
		actor(n).spawner = spawner
		'secret level
		IF spawnerFlags > 11 AND spawnerFlags < 16 THEN							
			actor(n).control = 0
			actor(n).jump = spawnerFlags
			IF player.levelCompletion(spawnerFlags) = 1 THEN 				
				actor(n).control = 64
				tileWrite x, y, 24				
			ELSEIF player.levelCompletion(spawnerFlags) > 49 THEN 				
				actor(n).control = 64
				tileWrite x, y, 25
			END IF			
			'actor(n).y = y + 20
			actor(n).frame = 0			
		'normal level
		ELSE		
			actor(n).jump = spawnerFlags
			IF player.levelScore(spawnerFlags) THEN
				actor(n).frame = 60
			ELSE
				actor(n).frame = 0
			END IF
			actor(n).control = 64			
		END IF						
	CASE 65	   'Checkpoint / Spawner portal
		actorListNew n
		actor(n).control = 65
		actor(n).spawner = spawner
		actor(n).flags = spawnerFlags
		actor(n).life = -3
		actor(n).frame = 0
		actor(n).anim = 0
		actor(n).visX = 999		
	CASE 66		'Hub-level mini star slot
		IF player.levelScore(spawnerFlags) THEN 
			tileWrite x, y, 27
		END IF
	CASE 67		'Hub-level Star Door		
		FOR c = 1 TO 10
			IF player.levelScore(c) = 0 THEN n = 0
		NEXT c
		IF n THEN			
			actorListNew n
			actor(n).jump = spawnerFlags			
			actor(n).frame = 38
			actor(n).control = 31
			actor(n).life = -999
			actor(n).anim = 1
			actor(n).spawner = spawner
			actor(n).dir = 31
			actor(n).visY = 50
			'actor(n).y = actor(n).y + 8
			actor(n).count = 4			
		END IF
	CASE 68		'Secret level unlock				
		IF player.levelCompletion(spawnerFlags) = 0 THEN			
			actorListNew n			
			actor(n).spawner = spawner		
			actor(n).control = 63
			actor(n).frame = 67
			actor(n).jump = spawnerFlags
			actor(n).dir = y
		END IF
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
	'
	' Set transformation parent
	' Negative exist(=parent) value means the actor has children, so
	' maintain such a value as negative after changing the parent.
	'
	IF actor(child).exist > 0 THEN
		actor(child).exist = parent
	ELSE
		actor(child).exist = -parent
	END IF
		
	actor(child).parentDX = actor(parent).X
	actor(child).parentDY = actor(parent).Y
	'
	' Check if linked-list rearrange is nescessary for
	' consistent transformation inheritance.
	'	
	n = child
	do
		n = actor(n).lPrev
		if n = parent then EXIT SUB
	loop while n>1
	'
	' Rearrange linked list so child is handled after the parent.
	'
	cNext = actor(child).lNext
	cPrev = actor(child).lPrev
	pNext = actor(parent).lNext

	actor(cPrev).lNext = actor(child).lNext
	actor(cNext).lPrev = actor(child).lPrev
	actor(child).lPrev = parent
	actor(child).lNext = pNext
	actor(parent).lNext = child
	actor(pNext).lPrev = child
	'debugOut str$(child) + ":" + str$(actor(child).exist) + " moved in list after " + str$(parent)
	'
	' Check if child has any children and arrange them after it
	' in the linked-list to maintain draw order.
	'	
	IF actor(child).exist < 0 THEN getChildrenFor child		
END SUB

SUB getChildrenFor(parent)	
	'
	'Finds children of an actor and arranges them after the parent in the linked list
	'
	'debugOut " Looking for children..."
	FOR child = 2 TO 60
		IF ABS(actor(child).exist) = parent THEN
			cNext = actor(child).lNext
			cPrev = actor(child).lPrev
			pNext = actor(parent).lNext
			actor(cPrev).lNext = actor(child).lNext
			actor(cNext).lPrev = actor(child).lPrev
			actor(child).lPrev = parent
			actor(child).lNext = pNext
			actor(parent).lNext = child
			actor(pNext).lPrev = child
			'debugOut "Child " + str$(child) + " rearranged."
			IF actor(child).exist < 0 THEN getChildrenFor child
		END IF
	NEXT child
END SUB

SUB unParent (index)
	'debugOut "unParent"
	actor(index).parentDX = 0
	actor(index).parentDY = 0
	IF actor(index).exist > 1 THEN
		actor(index).exist = 1
	ELSE
		actor(index).exist = -1
	END IF
END SUB

FUNCTION DrawHighScores
	DrawHighScores = -1
	c = 1
	y = 40	
	OPEN "hiscores.def" FOR BINARY AS #1
		FOR n = 10 to 1 STEP -1						
			GET #1, c, highScoreEntry					
			IF player.score >= highScoreEntry.score THEN 				
				DrawHighScores = n										
			END IF
			myPrint 6, y, str$(n) + "." , &H1717
			myPrint 11, y, highScoreEntry.name, &H1E1E
			myPrint 25, y, STR$(highScoreEntry.score), &H1E1E
			y = y - 3 
			c = c + 12
		NEXT n
	CLOSE #1
	aCopyPage gfx.pageOfs(gfx.page), gfx.pageOfs(gfx.page xor 1)	
END FUNCTION

SUB SaveHighScore (offset)	
	OPEN "hiscores.def" FOR BINARY AS #1
		PUT #1, offset, highScoreEntry
	CLOSE #1	
END SUB
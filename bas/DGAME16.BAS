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
DECLARE SUB queueSound (offset%, Position%)
DECLARE SUB playSounds ()
DECLARE SUB handleCollision ()
DECLARE SUB screenResetPos (x%, y%)

'$INCLUDE: 'libelf.bi'

DECLARE SUB destroyObject (index%)
DECLARE SUB deltaTime ()
DECLARE SUB elfInit ()
DECLARE SUB handleCamera ()
DECLARE SUB handleScroll ()
DECLARE SUB handleSpawn (wave)
DECLARE SUB loadLevel (fileName AS STRING)
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

SUB actorListNew (index)
	'debugOut "actorListNew"
	actor(index).lNext = 1
	actor(actor(1).lPrev).lNext = index
	actor(index).lPrev = actor(1).lPrev
	actor(1).lPrev = index
END SUB

SUB addScore (eventType)
	s = 0
	SELECT CASE eventType
	CASE 1
		s = 25
	CASE 8
		s = 50
	CASE 98
		s = 25
	CASE 88
		s = player.hp * 1000
	CASE 158
		s = 0
		IF player.hp < 4 THEN player.hp = player.hp + 1
		hudUpdate 2
	CASE 998
		s = 20
	CASE 999
		s = 10
	END SELECT
	player.score = player.score + s
END SUB

SUB damagePlayer
    IF actor(2).flash = 0 THEN
		player.hp = player.hp + (player.hp > 0)
		actor(2).flash = 40
		queueSound 0, 4
		hudUpdate 2
	END IF
END SUB

SUB DEBUGOUT (s$)
	OPEN "debug.txt" FOR APPEND AS #1
		PRINT #1, s$
	CLOSE #1
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

	entityCounter = 0
	scoreDelta = player.score
	spriteListIndex = 0
	soundEvent = -1

	dtAnims = dtAnims + 1
	fractionC = (fractionC + 1) MOD 10
'--------------------------------------------------------------------------
	colCount(1) = -1
	colCount(2) = -1
	colCount(3) = -1

	n = 2

SELECT CASE actor(2).control

'Level Clear Score Count
CASE 262
      pageOfs = gfx.pageOfs(gfx.page) * 2

      SELECT CASE actor(2).jump
       CASE 0 TO 1
          myPrint 2, 22, "LEVEL SCORE", &H171E
          myPrint 2, 28, "TIME   BONUS          X 30", &H1D1D
          myPrint 2, 32, "LIFE   BONUS          X 2000", &H1D1D
          myPrint 2, 36, "SECRET BONUS          X 2500", &H1D1D
          actor(2).jump = actor(2).jump + 1

       CASE 2 TO 3
          actor(2).jump = actor(2).jump + 1
          player.scoreCounter = 1
          player.scoreCounterInc = 10
          player.scoreCounterTarget = player.time
          player.scoreCounterMode = 0
          player.scoreCounterMul = 300

       CASE 4 TO 6
          w40char.text = "        |"
          a$ = STR$(0& + player.scoreCounter)
          MID$(w40char.text, 8 - LEN(a$), LEN(a$)) = a$
          aPrint gfx.videoWrap, &H1D1D, w40char.seg, w40char.ofs, 16, 28 + player.scoreCounterMode * 4, &HB800, pageOfs

          w40char.text = "         |"
          a$ = STR$(0& + player.score - player.scoreBase)
          MID$(w40char.text, 8 - LEN(a$), LEN(a$)) = a$
          aPrint gfx.videoWrap, &H1E1F, w40char.seg, w40char.ofs, 30, 22, &HB800, pageOfs

          IF player.scoreCounter >= player.scoreCounterTarget THEN
               player.scoreCounter = player.scoreCounterTarget
               actor(2).jump = actor(2).jump + 1
          ELSE
               player.scoreCounter = 0& + player.scoreCounter + player.scoreCounterInc
               player.score = 0& + player.score + player.scoreCounterMul
          END IF

          IF dtAnims = 1 THEN soundEvent = 52

       CASE 7 TO 16
          IF actor(2).jump = 7 THEN
              soundEvent = 0
              w40char.text = "         |"
              a$ = STR$(0& + player.scoreCounter * player.scoreCounterMul / player.scoreCounterInc)
              MID$(w40char.text, 8 - LEN(a$), LEN(a$)) = a$
          END IF
          aPrint gfx.videoWrap, &H1E1F, w40char.seg, w40char.ofs, 30, 28 + player.scoreCounterMode * 4, &HB800, pageOfs
          actor(2).jump = actor(2).jump + 1

       CASE 17
          player.scoreCounterMode = player.scoreCounterMode + 1
          SELECT CASE player.scoreCounterMode
          CASE 1
               player.scoreCounter = 1
               player.scoreCounterInc = 1
               player.scoreCounterTarget = player.hp
               player.scoreCounterMul = 1000
               actor(2).jump = 4
          CASE 2
               actor(2).jump = 99
               soundEvent = 92
          END SELECT

        CASE 99
            myPrint 12, 43, "#$ GAME SAVED #$", &H1D1D
            myPrint 8, 46, "PRESS SPACE TO CONTINUE", &H1717
            IF kbArray(57) THEN actor(2).jump = 999

        CASE 999 TO 1000
            actor(2).jump = actor(2).jump + 1
            actor(2).frame = 0
            tileCopyList(0, gfx.page).x = 0
            tileCopyList(0, gfx.page).y = 0
            tileCopyList(0, gfx.page).w = 40
            tileCopyList(0, gfx.page).h = 50
            tileCopyList(1, gfx.page).y = 255
            aClearList gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), &HDE, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)

        CASE 1001
            sounds.queue(sounds.readPos) = 0
            playSounds
            actor(n).control = 259
      END SELECT
               
'Level Clear Wait
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
      actor(2).count = 0
      actor(2).jump = 0
      actor(2).dir = 0
      actor(2).control = 262
      player.hudLock = 0
      hudUpdate 255
   END IF

   screenTargetX = actor(2).x - 19
   handleCamera
   screenTargetY = actor(2).y - 36
               

'Level Clear
CASE 260
   switchAnim n, 10

   screenTargetX = actor(2).x - 19
   handleCamera
   screenTargetY = actor(2).y - 36

   IF gfx.screenY(0) = screenTargetY AND gfx.screenY(1) = screenTargetY THEN actor(2).count = actor(2).count + 1

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
               
'Menu cursor
CASE 258
   actor(2).count = actor(2).count + (actor(2).count > -12)
   IF player.keyPrev AND kbArray(player.keyPrev) = 0 THEN
      actor(2).count = -12
      player.keyPrev = 0
   END IF
   
   IF kbArray(28) AND actor(2).vecY = 0 THEN
       IF player.menutarget = 3 THEN quit
   END IF

   IF player.menutarget = 1 THEN
      hudTextLoad &H0, 1
      loadTiles "tileset0"
      player.level = 1
      main
   END IF
   
   IF kbArray(80) AND actor(2).count = -12 THEN
       soundEvent = 82
       player.keyPrev = 80
       actor(2).count = 0
       player.menutarget = player.menutarget + 1
       menuFind = -1
   END IF

   IF kbArray(72) AND actor(2).count = -12 THEN
       soundEvent = 87
       player.keyPrev = 72
       actor(2).count = 0
       player.menutarget = player.menutarget - 1
       menuFind = 1
   END IF

   IF menuFind THEN
       Find = 0
       FOR s = 4 TO 60
           IF actor(s).control = 72 THEN
               IF actor(s).flags = player.menutarget THEN
                   Find = 1
                   player.targetX = actor(s).x
                   player.targetY = actor(s).y
                   s = 999
               END IF
            END IF
        NEXT s
       IF Find = 0 THEN
           player.menutarget = player.menutarget + menuFind
           soundEvent = -1
       END IF
       menuFind = 0
   END IF

   actor(2).vecX = speedDiff%(actor(2).x, player.targetX)
   actor(2).vecY = speedDiff%(actor(2).y, player.targetY)

   screenTargetY = actor(2).y - 34
   handleCamera

'Wait after death
CASE 256

   IF kbArray(57) THEN
       sounds.queue(sounds.readPos) = 0
       playSounds
       actor(n).control = 259
   END IF

'Normal player
CASE 255
   minY = -30

   c = tileRead(actor(2).x, actor(2).y)

   IF c > 29 THEN
      IF player.vecX = 0 AND player.ShootDelay < 6 THEN switchAnim n, 5

      speedMod = 8
      friction = 5

      actor(2).y = (actor(2).y AND &HFFF8)
      screenTargetYlock = actor(2).y - 46

      IF kbArray(29) = 0 THEN player.jumpRelease = -1

      collisionFloor = 1
      player.Bouncing = 0
      SELECT CASE c
      CASE 54
         spawnObject actor(2).x AND &HFFFC, actor(2).y AND &HFFF8, 99, 2, -5
         player.vecY = -38
         actor(2).y = actor(2).y - 2
         player.Bouncing = 30
         collisionFloor = 0
         maxY = 38
         player.jumpRelease = 0
         soundEvent = 98
      CASE 55
         friction = 0
         speedMod = 4
      CASE 58
         IF player.vecY > 0 THEN damagePlayer
      END SELECT
      IF player.vecY > 0 THEN soundEvent = 16: player.vecY = 0
      screenTargetY = screenTargetYlock
   ELSE
      collisionFloor = 0
      maxY = 30
      speedMod = 10
      friction = 5
      IF player.ShootDelay < 6 THEN switchAnim n, 10
      IF kbArray(29) THEN
         player.vecY = player.vecY + 2
         maxY = 20
      ELSE
         IF player.Bouncing > 0 THEN
            maxY = 38
            player.Bouncing = player.Bouncing - 1
            player.vecY = player.vecY + 2 - gfx.page
         ELSE
            player.vecY = player.vecY + 15
         END IF
   END IF

   a = actor(2).y - screenTargetYlock
   IF a > 50 THEN  'a>0
      screenTargetYlock = -1
   END IF

   IF c = 27 OR c = 26 THEN
      IF kbArray(29) = 0 THEN player.jumpRelease = -1
         collisionFloor = 2
         maxY = 5
         IF c = 27 THEN minY = -15
         friction = 1
         speedMod = 4
      END IF
   END IF

   IF actor(2).exist > 1 THEN
      IF kbArray(29) = 0 THEN player.jumpRelease = -1
      screenTargetYlock = -1
      collisionFloor = 1
      player.vecY = 0
   END IF

   c = tileRead(actor(n).x, actor(n).y - 8)
   IF c > 29 THEN
      actor(n).y = (actor(n).y AND &HFFF8) + 8
      player.vecY = 0
      IF c = 57 THEN damagePlayer
   END IF

   IF c = 28 THEN
      addScore 998
      soundEvent = 92
      x = (actor(2).x) AND &HFFFC
      y = (actor(2).y - 8) AND &HFFF8
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

   IF kbArray(29) AND player.jumpRelease THEN
      player.jumpRelease = 0
      player.vecY = -35
      'actor(n).Y = actor(n).Y - 2
      soundEvent = 0
   END IF

   IF kbArray(75) THEN
      player.vecX = player.vecX - speedMod
      actor(n).flags = 1
      IF collisionFloor = 1 THEN
         switchAnim n, 40
         IF fraction(2, fractionC) THEN
            sounds.queue(sounds.readPos) = walkNote(player.walkCounter)
            player.walkCounter = (player.walkCounter + 1) AND 7
         END IF
      END IF
   END IF

   IF kbArray(77) THEN
      player.vecX = player.vecX + speedMod
      actor(n).flags = 0
      IF collisionFloor = 1 THEN
         switchAnim n, 40
         IF fraction(2, fractionC) THEN
            sounds.queue(sounds.readPos) = walkNote(player.walkCounter)
            player.walkCounter = (player.walkCounter + 1) AND 7
         END IF
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
      switchAnim n, 30
   END IF

   IF player.hp = 0 THEN
      actor(n).collider = 0
      actor(n).control = 256
      actor(n).frame = 0
      actor(n).vecX = 0
      actor(n).vecY = 0
      FOR b = 0 TO 63 STEP 8
         xs = angle.x(b) \ 2
         ys = angle.y(b) \ 2
         spawnObject actor(n).x, actor(n).y, 257, xs, ys
      NEXT b
      queueSound 4, 22
      queueSound 14, 23
      hudTextLoad &H140, 0
      player.hudLock = 999
      player.score = player.scoreBase
      actor(2).x = 0
      actor(2).y = 0
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
       actor(n).x = (actor(n).x AND &HFFFC) + 1
   END IF

   screenTargetX = actor(n).x - 19
   handleCamera
   IF screenTargetYlock = -1 THEN
      screenTargetY = actor(2).y - 28
   END IF
END SELECT

playerX = actor(2).x
playerY = actor(2).y

screenX = gfx.screenX(gfx.page)
screenY = gfx.screenY(gfx.page)

listIDX = 1

DO

n = actor(listIDX).lNext
IF n = 0 THEN quit
listIDX = n

'----------------------------------------------------------------------------
IF actor(n).exist > 0 THEN

   entityCounter = entityCounter + 1

   visDeltaX = ABS(screenX + 20 - actor(n).x)
   visDeltaY = ABS(screenY + 24 - actor(n).y)

   IF visDeltaX < actor(n).visX AND visDeltaY < actor(n).visY THEN
      '
      'Get parent
      '
      parent = actor(n).exist
      '
      'Add parent delta transform.
      '
      actor(n).x = actor(n).x + (actor(parent).x - actor(n).parentDX)
      actor(n).y = actor(n).y + (actor(parent).y - actor(n).parentDY)
      '
      'Add speed vector to transform.
      '
      actor(n).x = actor(n).x + fraction(actor(n).vecX, fractionC)
      actor(n).y = actor(n).y + fraction(actor(n).vecY, fractionC)
      '
      'Store current parent transform for next delta.
      '
      actor(n).parentDX = actor(parent).x
      actor(n).parentDY = actor(parent).y

      IF actor(n).y < 8 THEN actor(n).y = 8
      IF actor(n).y > 630 THEN actor(n).y = 630
      IF actor(n).x < -7 THEN actor(n).x = -7
      IF actor(n).x > 85 THEN actor(n).x = 85
'----------------------------------------------------------------------------
      'Player Bullet
      IF actor(n).control = 100 THEN
         c = tileRead(actor(n).x, actor(n).y)
         IF c > 29 THEN
            IF c = 39 OR c = 53 OR c = 56 OR c = 49 THEN
               actor(n).x = actor(n).x AND &HFFFC
               actor(n).y = actor(n).y AND &HFFF8
               IF c = 39 OR c = 49 THEN
                  soundEvent = 44
                  a = 130
                  spawnObject actor(n).x, actor(n).y, 99, 0, 0
                  actor(n).vecY = 2
                  actor(n).flash = 16
                  t = 0
                  IF c = 49 THEN t = 28
               END IF
               IF c = 53 THEN
                  soundEvent = 122
                  a = 160
                  t = 52
                  actor(n).vecY = 0
                  IF player.hp > 1 THEN
                     o = 88
                  ELSE
                     o = 158
                  END IF
                  spawnObject actor(n).x + 2, actor(n).y, 41, o, 0
               END IF
               IF c = 56 THEN
                  soundEvent = 115
                  IF actor(n).vecX < 0 THEN
                     s = -10
                  ELSE
                     s = 10
                  END IF
                  a = 144
                  spawnObject actor(n).x, actor(n).y, 99, 3, s
               END IF

               addScore 999
               tileWrite actor(n).x, actor(n).y, t
               aUpdateTile actor(n).x, actor(n).y
               actor(n).vecX = 0
               actor(n).flags = 0
               actor(n).control = 0
               actor(n).life = 4
               actor(n).frame = a
            ELSE
               soundEvent = 44
               actor(n).flags = 0
               actor(n).control = 0
               actor(n).vecX = 0
               actor(n).collider = 0
               actor(n).life = 4
               switchAnim n, 110
               actor(n).vecY = -5
               soundEvent = 44
            END IF
         END IF
         IF visDeltaX > 30 THEN actor(n).life = 0
      END IF

   SELECT CASE actor(n).control
   'SPIKE BALL U
   CASE 17
        c = angle.y(counter0) + 32
        actor(n).x = actor(n).count + mulTable(actor(n).jump, angle.x(c))
        actor(n).y = actor(n).dir + mulTable(actor(n).jump, angle.y(c))

   CASE 18
   'SPIKE BALL O+
        actor(n).x = actor(n).count + mulTable(actor(n).jump, angle.x(counter0))
        actor(n).y = actor(n).dir + mulTable(actor(n).jump, angle.y(counter0))
   END SELECT

   IF gfx.page > 0 AND actor(n).control THEN
      
		actor(n).frame = actor(n).frame + animation(actor(n).frame).jump
		IF animation(actor(n).frame).jump < 0 THEN
			actor(n).frame = actor(n).frame + animation(actor(n).frame).jump
		END IF
		  
	  playerDeltaX = ABS(actor(n).x - playerX)
      playerDeltaY = ABS(actor(n).y - playerY)
      
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
             actor(n).life = 3
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

         IF actor(n).life < 37 AND actor(n).anim <> 90 THEN actor(n).flash = 36

         IF playerDeltaX < 5 AND ABS(actor(2).y - actor(n).y) < 8 THEN
            level.spawner(actor(n).spawner).object = 0
            soundEvent = 34
            IF actor(n).anim = 90 THEN
               player.score = player.score + 500
               player.hasKey = 1
               actor(n).control = 0
               actor(n).vecX = 0
               actor(n).vecY = -15
               actor(n).life = 8
               actor(n).flash = 16
            ELSE
               addScore actor(n).frame
               actor(n).y = actor(n).y - 3
               actor(n).vecY = 0
               actor(n).flash = 0
               actor(n).life = 9
               actor(n).control = 0
               switchAnim n, 93
            END IF
         END IF

       'Chest
       CASE 45
         IF playerDeltaX < 5 AND playerDeltaY < 10 AND player.hasKey THEN
          player.hasKey = 0
          spawnObject actor(n).x - 6, actor(n).y - 12, 41, 98, 0
          spawnObject actor(n).x - 4, actor(n).y - 14, 41, 98, 0
          spawnObject actor(n).x, actor(n).y - 16, 41, 88, 0
          spawnObject actor(n).x + 4, actor(n).y - 15, 41, 98, 0
          spawnObject actor(n).x + 6, actor(n).y - 14, 41, 98, 0
          actor(n).life = 5
          actor(n).frame = 195
          actor(n).vecY = -5
          soundEvent = 104
          level.spawner(actor(n).spawner).object = 0
         END IF

       'V platform
       CASE 52
         actor(n).count = actor(n).count + 1
         IF actor(n).count > 7 THEN
          c = tileRead(actor(n).x, actor(n).y)
          IF c <> 0 THEN
           actor(n).count = 0
           IF actor(n).vecY < 0 THEN
            actor(n).vecY = 10
            ELSEIF actor(n).vecY > 0 THEN actor(n).vecY = -10
           END IF
          END IF
         END IF
         
         IF actor(2).exist <> n THEN
          IF playerDeltaX < 5 AND playerDeltaY < 6 AND actor(2).vecY > -1 THEN
           actor(2).y = actor(n).y
           makeParent 2, n
		   listIDX = actor(n).lNext
          END IF
         ELSE
          IF playerDeltaX > 4 OR playerDeltaY > 1 THEN unParent 2
         END IF

       'Enemy bullet
       CASE 102
         IF playerDeltaX < 4 AND playerDeltaY < 6 THEN
          damagePlayer
          actor(n).life = 0
         END IF

       'Drip
       CASE 104
         IF actor(n).vecX > 0 THEN
           actor(n).vecX = actor(n).vecX - 1
          ELSEIF actor(n).vecX < 0 THEN
           actor(n).vecX = actor(n).vecX + 1
         END IF
         IF actor(n).vecY < 30 THEN actor(n).vecY = actor(n).vecY + 4
         IF playerDeltaX < 2 AND playerDeltaY < 5 THEN
          damagePlayer
          actor(n).flash = 16
          actor(n).life = 0
          actor(n).control = 0
          actor(n).vecY = -10
         END IF

       'Fire Bomb
       CASE 112
        IF actor(n).count < 10 THEN
         'c = tileRead(actor(n).x, actor(n).y)
         'IF c > 29 THEN
         ' actor(n).life = 6
         ' actor(n).count = 10
         'END IF
         IF actor(n).vecY < 35 THEN actor(n).vecY = actor(n).vecY + 2
        END IF
         IF playerDeltaX < 4 AND playerDeltaY < 6 THEN
          damagePlayer
          actor(n).life = 0
         END IF



       'Eggy
       CASE 1
           actor(n).vecY = 20
           IF actor(n).dir = 2 THEN
             IF actor(n).count < 0 AND actor(n).count > -8 THEN
               actor(n).vecY = angle.y(actor(n).jump)
               actor(n).jump = (actor(n).jump + 2) AND 63
               c = tileRead(actor(n).x, actor(n).y - 8)
               IF c > 29 THEN actor(n).vecY = 0: actor(n).y = actor(n).y AND &HFFF8 + 7
              ELSE
               actor(n).jump = 0
             END IF
            ELSE
             IF actor(n).count < 0 AND actor(n).count > -5 THEN
               actor(n).vecY = angle.y(actor(n).jump)
               actor(n).jump = (actor(n).jump + 4) AND 63
               c = tileRead(actor(n).x, actor(n).y - 8)
               IF c > 29 THEN actor(n).vecY = 0: actor(n).y = actor(n).y AND &HFFF8 + 7
              ELSE
               actor(n).jump = 0
             END IF
           END IF

            IF actor(n).vecY > -1 THEN
             c = tileRead(actor(n).x, actor(n).y)
             IF c > 29 THEN
               actor(n).vecX = 0
               actor(n).frame = 60
               actor(n).vecY = 0
               actor(n).y = actor(n).y AND &HFFF8
               IF actor(n).count = 0 AND actor(n).jump = 0 THEN
                actor(n).frame = 62
                actor(n).vecX = 5 + (actor(n).flags > 0) * 10
               END IF
             END IF
            END IF

            actor(n).count = actor(n).count + (actor(n).count < 1)

            IF actor(n).count < -10 AND c > 29 THEN
             actor(n).count = 0
             actor(n).jump = 0
             actor(n).dir = actor(n).dir + 1
             IF actor(n).dir > 2 THEN actor(n).dir = 0
             soundEvent = 142
            END IF

         IF actor(n).flags THEN
           s = -2
           p = 1
          ELSE
           s = 2
           p = 3
         END IF
         c = tileRead(actor(n).x + s, actor(n).y - 1)
         IF c > 29 THEN
            actor(n).flags = actor(n).flags XOR 1
            actor(n).vecX = 0
            actor(n).x = (actor(n).x AND &HFFFC) + p
         END IF

       
       'HELMET
       CASE 2
        c = tileRead(actor(n).x, actor(n).y)
        IF c > 30 THEN
          actor(n).y = actor(n).y AND &HFFF8
          actor(n).vecY = 0
         ELSE
          actor(n).vecX = 0
          actor(n).vecY = 20
        END IF
        IF actor(n).life > -50 THEN
         actor(n).y = actor(n).y - 6
         actor(n).life = -50
         actor(n).count = 32
         actor(n).collider = 0
         actor(n).dir = actor(n).vecX
         actor(n).vecX = 0
        END IF
       IF actor(n).count = 0 THEN
        IF c > 30 THEN
           IF actor(n).flags THEN
             actor(n).vecX = -4
             c = tileRead(actor(n).x - 2, actor(n).y - 2)
             c2 = tileRead(actor(n).x - 2, actor(n).y + 2)
              IF c > 29 OR c2 < 30 THEN
               actor(n).flags = 0
               actor(n).vecX = 4
               actor(n).x = actor(n).x AND &HFFFC + 2
              END IF
           ELSE
             actor(n).vecX = 4
             c = tileRead(actor(n).x + 2, actor(n).y - 2)
             c2 = tileRead(actor(n).x + 2, actor(n).y + 2)
              IF c > 29 OR c2 < 30 THEN
               actor(n).flags = 1
               actor(n).vecX = -4
               actor(n).x = actor(n).x AND &HFFFC + 2
              END IF
           END IF
        END IF
       ELSE
        actor(n).count = actor(n).count - 1
        actor(n).frame = 239
        IF actor(n).count = 0 THEN
         actor(n).frame = 232
         actor(n).collider = 1
         actor(n).vecX = actor(n).dir
        END IF
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
        
        IF actor(n).y < actor(2).y - 27 THEN actor(n).vecY = actor(n).vecY + 5
        IF actor(n).y > actor(2).y - 25 THEN actor(n).vecY = actor(n).vecY - 5
        IF actor(n).vecY < -10 THEN actor(n).vecY = -10
        IF actor(n).vecY > 10 THEN actor(n).vecY = 10
        IF actor(n).vecX < -8 THEN actor(n).vecX = -8
        IF actor(n).vecX > 8 THEN actor(n).vecX = 8
        
        IF playerDeltaX < 5 THEN
         IF actor(n).jump < 1 THEN
           spawnObject actor(n).x, actor(n).y + 1, 112, 0, 0
           actor(n).jump = 4
          ELSE
           actor(n).jump = actor(n).jump - 1
         END IF
        END IF

       'Drooly
       CASE 4
        IF actor(n).vecX THEN
          c = tileRead(actor(n).x + actor(n).vecX, actor(n).y + 1)
          IF c < 30 THEN actor(n).vecX = actor(n).vecX * -1
          c = tileRead(actor(n).x + actor(n).vecX, actor(n).y - 2)
          IF c > 29 THEN actor(n).vecX = actor(n).vecX * -1
        END IF

        actor(n).jump = actor(n).jump + 1

        IF actor(n).jump = 15 THEN
          actor(n).count = actor(n).vecX
          actor(n).vecX = 0
          spawnObject actor(n).x + 1, actor(n).y, 104, -10, 0
          spawnObject actor(n).x + 1, actor(n).y, 104, 10, 0
          soundEvent = 145
        END IF

        IF actor(n).jump > 25 THEN
         actor(n).jump = 0
         actor(n).vecX = actor(n).count
        END IF

       'Babat
       CASE 5
         actor(n).jump = actor(n).jump + 1
         
         SELECT CASE actor(n).count
         CASE 0
            IF playerDeltaX < 16 AND playerDeltaY < 32 AND actor(2).y > actor(n).y THEN
             actor(n).count = 1
             actor(n).frame = 202
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
         c = (counter0 + actor(n).count) AND 63
         actor(n).y = actor(n).count + mulTable(1, angle.y(c))

         'FIRE BREATH
       CASE 11
         x = (counter1 + actor(n).y) AND 31
         IF x > 10 THEN
          IF x = 11 THEN
           actor(n).frame = 152: soundEvent = 66
          END IF
          IF x = 15 THEN
           spawnObject actor(n).x, actor(n).y - 2, 111, actor(n).flags, -10
           spawnObject actor(n).x, actor(n).y - 2, 111, actor(n).flags, 0
           spawnObject actor(n).x, actor(n).y - 2, 111, actor(n).flags, 10
          END IF
         ELSE
           actor(n).frame = 150
         END IF

       'SPIKE BALL BOUNCE
       CASE 15
        c = tileRead(actor(n).x, actor(n).y + 6)
        IF c > 29 THEN
          actor(n).y = actor(n).y AND &HFFF8 - 6
          actor(n).vecY = -30
          sounds.queue(sounds.readPos) = 40
         ELSE
          actor(n).vecY = actor(n).vecY + 6
          IF actor(n).vecY > 20 THEN actor(n).vecY = 20
        END IF

       'CHECKPOINT
       CASE 62
           'IF playerDeltaY < 10 AND player.wave <> actor(n).flags THEN
		IF playerDeltaY < 52 THEN
			check = 0
			IF actor(2).Y < actor(n).Y - 20 AND player.wave =< actor(n).flags THEN
				player.wave = actor(n).flags + 1
				check = 1
				'debugOut "Checkpoint from Lo"
			ELSEIF actor(2).Y > actor(n).Y + 20 AND player.wave > actor(n).flags THEN
				player.wave = actor(n).flags
				check = 1
				'debugOut "Checkpoint from Hi"
			END IF
			IF check THEN
				'spawnerEnabler(actor(n).spawner) = spawnerEnabler(actor(n).spawner) AND &HFFF
				'player.wave = actor(n).flags
				level.spawner(actor(n).spawner).object = 62
				FOR nn = 3 TO 60
					IF ABS(actor(nn).y - actor(2).y) > 96 THEN
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
         IF (counter1 AND 3) = 2 THEN
          x = actor(n).x - 4 + INT(RND * 10)
          y = actor(n).y - 22 + INT(RND * 15)
          spawnObject x, y, 93, 0, 0
         END IF

         IF playerDeltaX < 16 AND playerDeltaY < 10 THEN
          IF kbArray(72) THEN
           actor(2).control = 260
           actor(2).count = 0
           actor(n).control = 0
           actor(n).exist = 0
          END IF
         END IF

        'TILE FX
        CASE 99
         IF actor(n).jump = 1 THEN
         END IF

       END SELECT

      END IF 'Page check

'----------------------------------------------------------------------------

      parts = animation(actor(n).frame).jump AND actor(n).frame > 0
      
      flipIndex = actor(n).flags AND 1

      flash = NOT actor(n).flash AND 1
      actor(n).flash = actor(n).flash + (actor(n).flash > 0)

      xx = actor(n).x - screenX + 20
      yy = actor(n).y - screenY + 20

      IF actor(n).collider THEN
       FOR animPart = actor(n).frame TO actor(n).frame + parts - 1
         spriteIndex = animation(animPart).index(flipIndex)
         x = xx + animation(animPart).x(flipIndex)
         y = yy + animation(animPart).y
         'sx = x' - screenX
         'sy = y' - screenY

         IF spriteIndex AND x > 12 AND x < 60 AND y > 5 AND y < 70 THEN
          IF flash THEN
           spriteList(spriteListIndex, gfx.page).offset = spriteOffset(spriteIndex)
           spriteList(spriteListIndex, gfx.page).x = x' + 20
           spriteList(spriteListIndex, gfx.page).y = y' + 20
           spriteList(spriteListIndex, gfx.page).w = spriteWidth(spriteIndex)
           spriteList(spriteListIndex, gfx.page).h = spriteHeight(spriteIndex)
           spriteListIndex = spriteListIndex + 1
          END IF
          ctype = actor(n).collider AND (animation(animPart).jump > 0)
            IF ctype THEN
             colCount(ctype) = colCount(ctype) + 1
             colList(ctype, colCount(ctype)).index = n
             colList(ctype, colCount(ctype)).x0 = x
             colList(ctype, colCount(ctype)).y0 = y
             colList(ctype, colCount(ctype)).x1 = x + spriteWidth(spriteIndex) - 1
             colList(ctype, colCount(ctype)).y1 = y + spriteHeight(spriteIndex) - 1
            END IF
         END IF
       NEXT animPart
      ELSE
       IF flash THEN
         FOR animPart = actor(n).frame TO actor(n).frame + parts - 1
          spriteIndex = animation(animPart).index(flipIndex)
          x = xx + animation(animPart).x(flipIndex)
          y = yy + animation(animPart).y
      
          IF spriteIndex AND x > 12 AND x < 60 AND y > 5 AND y < 70 THEN
           spriteList(spriteListIndex, gfx.page).offset = spriteOffset(spriteIndex)
           spriteList(spriteListIndex, gfx.page).x = x '+ 20
           spriteList(spriteListIndex, gfx.page).y = y '+ 20
           spriteList(spriteListIndex, gfx.page).w = spriteWidth(spriteIndex)
           spriteList(spriteListIndex, gfx.page).h = spriteHeight(spriteIndex)
           spriteListIndex = (spriteListIndex + 1) AND 31
          END IF
        NEXT animPart
       END IF
      END IF
  END IF 'visibility check
'----------------------------------------------------------------------------
   IF actor(n).life > -1 THEN
    IF actor(n).life = 0 THEN
     'debugOut "destroy " + STR$(n)
     IF actor(n).control > 0 AND actor(n).control < 63 THEN
	  'spawnerEnabler(actor(n).spawner) = 0
	  level.spawner(actor(n).spawner).object = 0
      'spawnerEnabler(actor(n).spawner) = spawnerEnabler(actor(n).spawner) AND &HFFF
     END IF
     IF actor(n).control > 0 AND actor(n).control < 11 THEN
          actor(n).exist = 1
          actor(n).flags = 0
          actor(n).control = 41
          actor(n).flash = 4
          actor(n).vecX = 0
          actor(n).vecY = -20
          actor(n).y = actor(n).y - 4
          actor(n).collider = 0
          actor(n).life = 113
          actor(n).frame = 98
          c = tileRead(actor(n).x, actor(n).y)
          IF c > 29 THEN
            actor(n).count = actor(n).y + 8
           ELSE
            actor(n).count = actor(n).y
          END IF
      ELSE
          destroyObject n    ' getObject
          'listIDX = actor(n).lNext
          'getObject = getObject - 1
     END IF
    END IF
    actor(n).life = actor(n).life - 1
   END IF
   END IF 'exist check
LOOP WHILE actor(listIDX).lNext > 1

 IF gfx.page THEN counter1 = (counter1 + 1) AND 63

 spriteList(spriteListIndex, gfx.page).y = 255

 IF soundEvent > -1 THEN queueSound 0, soundEvent

 IF player.score > scoreDelta THEN
  hudUpdate 4
 END IF

 counter0 = (counter0 + 1) AND 63

END SUB

SUB handleCollision
	soundEvent = -1
	'Collision lists
	' 1 = Affect Player
	' 2 = Affect Enemy
	' 3 = Affect Bullet
	IF actor(2).control = 255 AND actor(2).flash = 0 THEN
		px = actor(2).x - gfx.screenX(gfx.page) + 20
		py = actor(2).y - gfx.screenY(gfx.page) + 14
		FOR a = 0 TO colCount(1)
			n = colList(1, a).index
			IF px > colList(1, a).x0 - 2 AND px < colList(1, a).x1 + 2 AND py > colList(1, a).y0 - 3 AND py < colList(1, a).y1 + 3 THEN
				damagePlayer
				a = 9999
			END IF
		NEXT a
	END IF

	FOR b = 0 TO colCount(2)
		n = colList(2, b).index
		px = actor(n).x - gfx.screenX(gfx.page) + 20
		py = actor(n).y - gfx.screenY(gfx.page) + 20
		FOR a = 0 TO colCount(1)
			IF px > colList(1, a).x0 - 2 AND px < colList(1, a).x1 + 2 AND py > colList(1, a).y0 - 3 AND py < colList(1, a).y1 + 3 THEN
				n2 = colList(1, a).index
				actor(n).collider = 0
				actor(n).life = 4
				actor(n).vecY = -5
				actor(n).control = 0
				actor(n).vecX = 0
				actor(n).flags = 0
				'actor(n).flash = 20
				IF actor(n2).life > -999 THEN
					actor(n2).flash = 10
					actor(n2).life = actor(n2).life + 1
				END IF
				IF actor(n2).life > -1 THEN
					actor(n).x = actor(n2).x
					actor(n).frame = 162
					soundEvent = 44
				ELSE
					actor(n).frame = 110
					soundEvent = 44
				END IF
			END IF
		NEXT a
	NEXT b

	IF soundEvent > -1 THEN queueSound 0, soundEvent
END SUB

SUB hudTextLoad (offset AS INTEGER, show AS INTEGER)
	loadStr = MID$(hudBuffer, 161, 1)
	OPEN "ducktext.ctt" FOR BINARY AS #1
		GET #1, offset + 1, hudBuffer
	CLOSE #1
	MID$(hudBuffer, 161, 1) = loadStr
	IF show THEN aPageFlip gfx.videoWrap, hudBufferSeg, hudBufferOfs, gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
END SUB

SUB hudUpdate (flags AS INTEGER)
 IF player.hudLock THEN
   player.hudLock = player.hudLock - 1
   IF player.hudLock = 1 THEN
     flags = 255
    ELSE
     EXIT SUB
   END IF
 END IF


 IF flags AND 1 THEN
  'hudPrint 0, "FPS:      0000000              TIME:    ", &HE, &HC

  w40char.text = "FPS:      0000000              TIME: |"
  aPrint &HFFFF, &HE0C, w40char.seg, w40char.ofs, 0, 0, hudBufferSeg, hudBufferOfs

 END IF


 IF flags AND 2 THEN
   IF player.hp < 4 THEN
    w40char.text = "#$#$#$#$|"
    aPrint &HFFFF, &H505, w40char.seg, w40char.ofs, 18, 0, hudBufferSeg, hudBufferOfs
   END IF
   a$ = "#$#$#$#$"
   IF player.hp THEN
    w40char.text = MID$(a$, 1, player.hp * 2) + "|"
    aPrint &HFFFF, &HCECE, w40char.seg, w40char.ofs, 18, 0, hudBufferSeg, hudBufferOfs
   END IF
 END IF


 IF flags AND 4 THEN
  a$ = STR$(player.score)
  c = LEN(a$)
  a$ = MID$(a$, 2, c)

  w40char.text = a$ + "|"
  aPrint &HFFFF, &HC0E, w40char.seg, w40char.ofs, 18 - c, 0, hudBufferSeg, hudBufferOfs

  'hudPrint 18 - C, a$, &HE, &HC
 END IF


 IF flags AND 8 THEN
  a$ = STR$(player.time)
  w40char.text = a$ + "|"
  aPrint &HFFFF, &HF0E, w40char.seg, w40char.ofs, 39 - LEN(a$), 0, hudBufferSeg, hudBufferOfs

  'hudPrint 39 - LEN(a$), a$, &HF, &HE
 END IF
 IF flags AND 16 THEN
  'w40char.text = STR$(INT(fps)) + "|"
  'aPrint &HFFFF, &HE0C, w40char.seg, w40char.ofs, 4, 0, hudBufferSeg, hudBufferOfs
  'hudPrint 4, STR$(INT(fps)), &HF, &HE
 END IF
 'w40char.text = STR$(ASC(MID$(hudBuffer, 161, 1))) + "|"
 'aPrint &HFFFF, &HF0F, w40char.seg, w40char.ofs, 7, 0, hudBufferSeg, hudBufferOfs
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
	initTestGame
	'initMainMenu
END SUB

SUB initMainMenu
	hudTextLoad &H0, 1
	loadTiles "logotile"
	player.level = 8
	randomOffset = 0
	DO
		main
	LOOP
END SUB

SUB initTestGame
	player.level = 0
	DO
		main
	LOOP
END SUB

SUB main
	hudTextLoad &HA0, 1
	loadLevel "level" + MID$(STR$(player.level), 2, 1)
	
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
	actor(1).lPrev = 2
	actor(1).exist = 0

	colCount(1) = -1
	colCount(2) = -1
	colCount(3) = -1

	player.hp = 4
	player.time = 300
	player.hasKey = 0
	player.jumpRelease = -1
	player.wave = 0
	player.scoreBase = player.score

	actor(2).lPrev = 1
	actor(2).lNext = 1
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
	actor(2).frame = 10
	actor(2).anim = 10
	actor(2).visX = 999
	actor(2).visY = 999
	playerOldX = -999
	playerOldY = -999

	handleSpawn 0
	screenResetPos actor(2).x - 20, actor(2).y - 46
	updateText = 1

	IF player.level = 8 THEN
		hudTextLoad &H6E0, 0
		player.hudLock = 999
	ELSE
		hudTextLoad &H1E0 + player.level * 160, 0
		player.hudLock = 10
	END IF

	CALL aTimerReset

	DO
		CALL aTimerWait
		handleCollision
		handleBehaviors
		playSounds
		handleScroll
		aSpriteList gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), spriteBankSeg, spriteBankOfs, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)

		IF actor(2).control = 259 THEN EXIT SUB
		IF actor(2).control = 255 THEN
			IF kbArray(1) THEN
				hudTextLoad &H780, 1
				queueSound 0, 76
				DO
					CALL aTimerWait
					playSounds		 
					IF kbArray(21) THEN
						quit
						EXIT SUB
					END IF
				LOOP WHILE kbArray(49) = 0
				queueSound 0, 76
				player.hudLock = 0
				hudUpdate 255
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

SUB handleSpawn (wave)
	'debugOut "handleSpawn"+str$(wave)
	FOR n = 1 TO 16
		nn = wave * 16 + n
		o = level.spawner(nn).object
		'IF spawnerEnabler(nn) > 0 THEN o = spawnerEnabler(nn)
		IF o > 0 THEN
			'debugOut "#"+str$(nn)+" found"+str$(o)
			spawnObject level.spawner(nn).x, level.spawner(nn).y, o, level.spawner(nn).flags, nn
			level.spawner(nn).object = level.spawner(nn).object OR &HF000
			'spawnerEnabler(nn) = o OR &HF000
			'level.wave(wave).spawner(n).object = 0			
		END IF
	NEXT n
END SUB

SUB spawnObject (x, y, object, spawnerFlags, spawner) 
	n = getSpawnSlot
	
	'debugOut "spawnObject"+str$(object)
	
	actor(n).exist = 1
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
		actor(n).life = 25
		actor(n).frame = 110
		actor(n).vecX = spawnerFlags
		actor(n).vecY = spawner
		actor(n).visY = 40
		actor(n).spawner = 0
	CASE 200	'Explode
		actorListNew n
		actor(n).control = 0
		actor(n).life = 15
		actor(n).spawner = 0
		actor(n).collider = 0
		actor(n).frame = 110
		actor(n).anim = 110
	CASE 99   'Clear Tile
		actorListNew n
		actor(n).control = 0
		actor(n).spawner = 0
		SELECT CASE spawnerFlags
		CASE 0
			actor(n).life = 3
			actor(n).frame = 144
		CASE 1
			actor(n).life = 2
			actor(n).frame = 160
			actor(n).vecX = spawner
		CASE 2
			actor(n).life = 2
			actor(n).frame = 160
			actor(n).vecY = spawner
		CASE 3
			actor(n).life = 40
			actor(n).frame = 160
			actor(n).vecX = spawner
			actor(n).visX = 70
			actor(n).visY = 70
			actor(n).control = 99
		CASE 4
			actor(n).life = 40
			actor(n).frame = 160
			actor(n).vecY = spawner
			actor(n).visX = 999
			actor(n).visY = 999
			actor(n).control = 99
		END SELECT
		actor(n).jump = spawnerFlags
	CASE 98	'Tile FX
		actorListNew n
		actor(n).control = 98   
		actor(n).life = spawnerFlags
		actor(n).frame = 160
		actor(n).anim = 160
		actor(n).visX = 999
		actor(n).visY = 999
		actor(n).spawner = 0
	CASE 93		'Twinkle particle
		actorListNew n   
		actor(n).control = 0
		actor(n).frame = 93
		actor(n).life = 10
		actor(n).vecY = 3
		actor(n).spawner = 0
	CASE 100	'Player Bullet
		actorListNew n
		actor(n).control = 100
		actor(n).life = 20   
		actor(n).spawner = 0
		actor(n).flags = spawnerFlags
		actor(n).collider = 2
		actor(n).vecX = 30 + (spawnerFlags <> 0) * 60
		actor(n).frame = 100
		actor(n).anim = 100  
	CASE 102	'Enemy Bullet
		actorListNew n
		actor(n).control = 102
		actor(n).life = 20   
		actor(n).spawner = 0
		actor(n).vecX = 10 + (spawnerFlags <> 0) * 20
		actor(n).vecY = 5
		actor(n).frame = 102
		actor(n).anim = 102
		actor(n).visX = 40
		actor(n).visY = 40   
	CASE 104	'DRIP
		actorListNew n
		actor(n).control = 104   
		actor(n).life = 20
		actor(n).vecX = spawnerFlags
		actor(n).vecY = -30
		actor(n).frame = 214
		actor(n).anim = 214
		actor(n).visY = 30
		actor(n).spawner = 0		
	CASE 111	'FIRE PARTICLE
		actorListNew n
		actor(n).control = 102
		actor(n).life = 20   
		actor(n).spawner = 0
		actor(n).vecX = 7 + (spawnerFlags <> 0) * 14
		actor(n).vecY = spawner
		actor(n).frame = 156
		actor(n).visX = 40
		actor(n).visY = 40   
	CASE 112	'FIRE BOMB
		actorListNew n
		actor(n).control = 112
		actor(n).life = 30   
		actor(n).spawner = 0
		actor(n).vecX = 0
		actor(n).vecY = 5
		actor(n).frame = 156
		actor(n).visX = 40
		actor(n).visY = 50   
	CASE 1	'Eggy
		actorListNew n
		actor(n).flags = spawnerFlags
		actor(n).control = 1   
		actor(n).life = -2
		actor(n).collider = 1
		actor(n).frame = 60
		actor(n).anim = 60
		actor(n).count = 0
		actor(n).visX = 25
		actor(n).visY = 40   
	CASE 2	'HELMET
		actorListNew n
		actor(n).control = 2   
		actor(n).life = -50
		actor(n).collider = 1
		actor(n).frame = 232
		actor(n).count = 0
		actor(n).visY = 35
	CASE 3	'DRAGON
		actorListNew n
		actor(n).control = 3   
		actor(n).life = -2
		actor(n).collider = 1
		actor(n).frame = 227
		actor(n).count = 0
		actor(n).visY = 55
		actor(n).visX = 55
	CASE 4	   'DROOLY
		actorListNew n
		actor(n).control = 4   
		actor(n).life = -1
		actor(n).collider = 1
		actor(n).vecX = 2
		actor(n).frame = 208
		actor(n).anim = 208
		actor(n).visY = 30   
	CASE 5	'BABAT
		actorListNew n
		actor(n).control = 5   
		actor(n).spawner = spawner
		actor(n).life = -2
		actor(n).dir = y
		actor(n).collider = 1
		actor(n).frame = 200
		actor(n).visX = 30
		actor(n).visY = 50
		actor(n).count = 0   
	CASE 6	'SKULL
		actorListNew n
		actor(n).control = 6   
		actor(n).spawner = spawner
		actor(n).life = -999
		actor(n).collider = 1
		actor(n).frame = 220
		actor(n).visX = 999
		actor(n).visY = 30
		actor(n).count = y 
	CASE 11 TO 12	'FIRE BREATH R / L
	   actorListNew n
	   actor(n).control = 11   
	   actor(n).spawner = spawner
	   actor(n).life = -9999
	   actor(n).frame = 150
	   actor(n).anim = 150
	   actor(n).visY = 25
	   actor(n).visX = 21
	   d = 8 + ((11 - object) * 13)
	   actor(n).dir = d
	   IF object = 12 THEN actor(n).flags = 1
	CASE 15			'Bouncing spike ball
		actorListNew n
		actor(n).collider = 1
		actor(n).control = 15    
		actor(n).life = -9999
		actor(n).frame = 146
		actor(n).visX = 24
		actor(n).visY = 50 
	CASE 16  		'SPAWNER
		actorListNew n
		actor(n).control = 16    
		actor(n).life = -9999
		actor(n).frame = 0
		actor(n).visX = 30
		actor(n).visY = 70
		actor(n).jump = spawnerFlags
	CASE 17 TO 18	'SPIKE BALL U, O+, O-
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
			actor(n).frame = 148
			actor(n).anim = 148
			actor(n).visX = 45
			actor(n).visY = 45
			n = getSpawnslot
		NEXT p
		actorListNew n
		actor(n).collider = 1
		actor(n).x = x
		actor(n).y = y
		actor(n).count = x
		actor(n).dir = y
		actor(n).control = c
		actor(n).exist = 1
		actor(n).spawner = spawner
		actor(n).life = -999
		actor(n).jump = 5
		actor(n).frame = 146
		actor(n).anim = 146
		actor(n).visX = 50
		actor(n).visY = 50 
	CASE 41	   'PICKUP GEM/KEY/ETC...
		actorListNew n
		actor(n).control = 41
		actor(n).spawner = 0
		actor(n).life = 113
		actor(n).count = y
		actor(n).vecY = -20
		actor(n).frame = spawnerFlags
		actor(n).anim = spawnerFlags
		actor(n).visY = 34
	CASE 44   'KEY
		actorListNew n
		actor(n).control = 41
		actor(n).spawner = spawner
		actor(n).life = -999
		actor(n).vecY = 0
		actor(n).frame = 90
		actor(n).anim = 90   
	CASE 45   'CHEST
		actorListNew n
		actor(n).control = 45
		actor(n).spawner = spawner
		actor(n).life = -999
		actor(n).vecY = 0
		actor(n).frame = 86
		actor(n).anim = 86
    CASE 51   	'H Platform

	CASE 52		'V Platform
		actorListNew n
		actor(n).control = 52   
		actor(n).spawner = spawner
		actor(n).life = -999
		actor(n).vecY = 10
		actor(n).frame = 168
		actor(n).visX = 999
		actor(n).visY = 96  
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
	CASE 71	   'MENU CURSOR
		actor(n).spawner = spawner
		actor(2).flash = 0
		actor(2).control = 258
		actor(2).exist = 1
		actor(2).x = x
		actor(2).y = y
		actor(2).vecX = 0
		actor(2).vecY = 0
		actor(2).anim = 10
		actor(2).frame = 10
		actor(2).flags = 0
		screenResetPos x - 20, actor(2).y
		player.menutarget = 0
		player.mode = 0
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

SUB makeParent (child, parent)
	'debugOut "makeParent"
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

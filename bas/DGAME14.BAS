DECLARE SUB myPrint (X%, Y%, text$, attributes%)
DECLARE SUB screenFakePos (Y%)
DECLARE SUB initTestGame ()
DECLARE SUB initMainMenu ()
DECLARE FUNCTION speedDiff% (x0%, x1%)
DECLARE SUB hudTextLoad (offset AS INTEGER, show AS INTEGER)
DECLARE FUNCTION randInt% ()
DECLARE SUB hudUpdate (flags AS INTEGER)
DECLARE SUB addScore (eventType%)
DECLARE FUNCTION collision% (x0%, y0%, w0%, h0%, x1%, y1%, w1%, h1%)
DECLARE SUB damagePlayer ()
DECLARE SUB hudPrint (X%, text$, colour1%, colour2%)
DECLARE SUB aUpdateTile (X%, Y%)

DECLARE SUB queueSound (offset%, Position%)
DECLARE SUB playSounds ()


DECLARE SUB handleCollision ()

DECLARE SUB screenResetPos (X%, Y%)

'$INCLUDE: 'libelf.bi'

DECLARE SUB destroyObject (index%)
DECLARE SUB deltaTime ()
DECLARE SUB elfInit ()

DECLARE SUB handleCamera ()
DECLARE SUB handleScroll ()
DECLARE SUB handleSpawn (wave)

DECLARE SUB incSpawnIndex ()

DECLARE SUB loadLevel (fileName AS STRING)
DECLARE SUB loadSprites (file$, offset%)
DECLARE SUB loadTiles (file$)
DECLARE SUB loadSounds (file$)

DECLARE FUNCTION tileRead (X%, Y%)
DECLARE SUB tileWrite (X AS INTEGER, Y AS INTEGER, index AS INTEGER)

DECLARE SUB switchAnim (objectIndex%, anim%)

DECLARE SUB spawnObject (X%, Y%, object%, spawnerFlags%, spawner)
DECLARE SUB handleBehaviors ()
DECLARE SUB main ()

DECLARE SUB quit ()

DECLARE FUNCTION getSpawnSlot% ()

DEFINT A-Z

DIM SHARED scoreHit AS INTEGER
DIM SHARED scorePickup(10 TO 20) AS INTEGER
DIM SHARED scoreKill(1 TO 40) AS INTEGER

DIM SHARED killY AS INTEGER

TYPE scoreType
 name AS STRING * 3
 score AS LONG
END TYPE

DIM SHARED spriteClipX(-10 TO 50) AS INTEGER
DIM SHARED spriteClipY(-20 TO 70) AS INTEGER
DIM SHARED spriteClipYofs(-20 TO 70) AS INTEGER

DIM SHARED walkNote(0 TO 7) AS INTEGER

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

    IF object(2).flash = 0 THEN
     player.hp = player.hp + (player.hp > 0)
     object(2).flash = 40
     queueSound 0, 4
     hudUpdate 2
    END IF

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
 STATIC parentDeltaX
 STATIC parentDeltaY

 entityCounter = 0
' entityVisibles = 0

 scoreDelta = player.score

 spriteListIndex = 0
 
 soundEvent = -1

 dtAnims = dtAnims + 1

 floatC = (floatC + 1) MOD 10
 
 'hudprint 10, STR$(spawnIndex), &HF, &HF

 '--------------------------------------------------------------------------
  colCount(1) = -1
  colCount(2) = -1
  colCount(3) = -1

  n = objectIndex(2)
              
  SELECT CASE object(2).control

      'Level Clear Score Count
      CASE 262

               pageOfs = gfx.pageOfs(gfx.page) * 2

               SELECT CASE object(2).jump
                CASE 0 TO 1
                  
                   myPrint 2, 22, "LEVEL SCORE", &H171E
                   myPrint 2, 28, "TIME   BONUS          X 30", &H1D1D
                   myPrint 2, 32, "LIFE   BONUS          X 2000", &H1D1D
                   myPrint 2, 36, "SECRET BONUS          X 2500", &H1D1D

                   object(2).jump = object(2).jump + 1

                CASE 2 TO 3

                   object(2).jump = object(2).jump + 1

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
                     object(2).jump = object(2).jump + 1
                    ELSE
                     player.scoreCounter = 0& + player.scoreCounter + player.scoreCounterInc
                     player.score = 0& + player.score + player.scoreCounterMul
                   END IF

                   IF dtAnims = 1 THEN soundEvent = 52

                CASE 7 TO 16
                   IF object(2).jump = 7 THEN
                    soundEvent = 0
                    w40char.text = "         |"
                    a$ = STR$(0& + player.scoreCounter * player.scoreCounterMul / player.scoreCounterInc)
                    MID$(w40char.text, 8 - LEN(a$), LEN(a$)) = a$
                   END IF
                   
                   aPrint gfx.videoWrap, &H1E1F, w40char.seg, w40char.ofs, 30, 28 + player.scoreCounterMode * 4, &HB800, pageOfs

                   object(2).jump = object(2).jump + 1

                CASE 17
                   player.scoreCounterMode = player.scoreCounterMode + 1
                   SELECT CASE player.scoreCounterMode
                    CASE 1
                     player.scoreCounter = 1
                     player.scoreCounterInc = 1
                     player.scoreCounterTarget = player.hp
                     player.scoreCounterMul = 1000
                     object(2).jump = 4
                    CASE 2
                     object(2).jump = 99
                     soundEvent = 92
                   END SELECT

                 CASE 99
                  myPrint 12, 43, "#$ GAME SAVED #$", &H1D1D
                  myPrint 8, 46, "PRESS SPACE TO CONTINUE", &H1717
                  IF kbArray(57) THEN object(2).jump = 999

                 CASE 999 TO 1000
                  object(2).jump = object(2).jump + 1
                  object(2).animFrame = 0
                  tileCopyList(0, gfx.page).X = 0
                  tileCopyList(0, gfx.page).Y = 0
                  tileCopyList(0, gfx.page).w = 40
                  tileCopyList(0, gfx.page).h = 50
                  tileCopyList(1, gfx.page).Y = 255
                  aClearList gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), &HDE, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)

                 CASE 1001
                  sounds.queue(sounds.readPos) = 0
                  playSounds
                  object(n).control = 259
               END SELECT
               
      'Level Clear Wait
      CASE 261
               
               IF physics(2).X < 40 THEN physics(2).X = physics(2).X + 1: object(2).flags = 0
               IF physics(2).X > 40 THEN physics(2).X = physics(2).X - 1: object(2).flags = 1
               'IF physics(2).y < 22 THEN physics(2).y = physics(2).y + 2
               IF physics(2).Y > 22 THEN physics(2).Y = physics(2).Y - 2

               IF physics(2).X = 40 AND physics(2).Y <= 22 THEN
                FOR delObj = 4 TO 60
                 object(delObj).exist = 0
                 object(delObj).control = 0
                NEXT delObj
                object(2).counter = 0
                object(2).jump = 0
                object(2).direction = 0
                object(2).control = 262
                player.hudLock = 0
                hudUpdate 255
               END IF

               screenTargetX = physics(2).X - 19
               handleCamera
               screenTargetY = physics(2).Y - 36
               

      'Level Clear
      CASE 260
          switchAnim n, 10

          screenTargetX = physics(2).X - 19
          handleCamera
          screenTargetY = physics(2).Y - 36

          IF gfx.screenY(0) = screenTargetY AND gfx.screenY(1) = screenTargetY THEN object(2).counter = object(2).counter + 1

          IF object(2).counter > 2 THEN
           screenFakePos 80
           DEF SEG = tileBufferSeg
           FOR n = 0 TO 199
            writeOfs = 0& + TileMapOfs + n
            POKE writeOfs, 0
           NEXT n
           DEF SEG
           soundEvent = 220
           object(2).control = 261
          END IF
               
      'Menu cursor
      CASE 258
               object(2).counter = object(2).counter + (object(2).counter > -12)
               IF player.keyPrev AND kbArray(player.keyPrev) = 0 THEN object(2).counter = -12: player.keyPrev = 0

               IF kbArray(28) AND physics(2).speedY = 0 THEN
                IF player.menutarget = 3 THEN
                 quit
                END IF

                IF player.menutarget = 1 THEN
                  hudTextLoad &H0, 1
                  loadTiles "tileset0"
                  player.level = 1
                  main
                END IF
               END IF

               IF kbArray(80) AND object(2).counter = -12 THEN
                soundEvent = 82
                player.keyPrev = 80
                object(2).counter = 0
                player.menutarget = player.menutarget + 1
                menuFind = -1
               END IF

               IF kbArray(72) AND object(2).counter = -12 THEN
                soundEvent = 87
                player.keyPrev = 72
                object(2).counter = 0
                player.menutarget = player.menutarget - 1
                menuFind = 1
               END IF

               IF menuFind THEN
                Find = 0
                FOR s = 4 TO 60
                 IF object(s).control = 72 THEN
                  IF object(s).flags = player.menutarget THEN
                   Find = 1
                   player.targetX = physics(s).X
                   player.targetY = physics(s).Y
                   s = 999
                  END IF
                 END IF
                NEXT s
                IF Find = 0 THEN player.menutarget = player.menutarget + menuFind: soundEvent = -1
                menuFind = 0
               END IF

               physics(2).speedX = speedDiff%(physics(2).X, player.targetX)
               physics(2).speedY = speedDiff%(physics(2).Y, player.targetY)
               
               screenTargetY = physics(2).Y - 34
               handleCamera

      'Wait after death
      CASE 256

               IF kbArray(57) THEN
                sounds.queue(sounds.readPos) = 0
                playSounds
                object(n).control = 259
               END IF

      'Normal player
      CASE 255
                minY = -30
                
                c = tileRead(physics(2).X, physics(2).Y)
                
                IF c > 29 THEN
                  IF player.speedX = 0 AND player.ShootDelay < 6 THEN switchAnim n, 5

                  speedMod = 8
                  friction = 5

                  physics(2).Y = (physics(2).Y AND &HFFF8)
                  screenTargetYlock = physics(2).Y - 46
                  'killY = physics(2).y - 16

                  IF kbArray(29) = 0 THEN player.jumpRelease = -1

                  collisionFloor = 1
                  player.Bouncing = 0
                  SELECT CASE c
                    CASE 54
                     spawnObject physics(2).X AND &HFFFC, physics(2).Y AND &HFFF8, 99, 2, -5
                     player.speedY = -38
                     physics(2).Y = physics(2).Y - 2
                     player.Bouncing = 30
                     collisionFloor = 0
                     maxY = 38
                     player.jumpRelease = 0
                     soundEvent = 98
                    CASE 55
                     friction = 0
                     speedMod = 4
                    CASE 58
                     IF player.speedY > 0 THEN damagePlayer
                  END SELECT

                  IF player.speedY > 0 THEN soundEvent = 16: player.speedY = 0

                  screenTargetY = screenTargetYlock
                  
                 ELSE

                  collisionFloor = 0
                  maxY = 30
                  speedMod = 10
                  friction = 5
                  IF player.ShootDelay < 6 THEN switchAnim n, 10
                  IF kbArray(29) THEN
                      player.speedY = player.speedY + 2
                      maxY = 20
                     ELSE
                      IF player.Bouncing > 0 THEN
                        maxY = 38
                        player.Bouncing = player.Bouncing - 1
                        player.speedY = player.speedY + 2 - gfx.page
                       ELSE
                        player.speedY = player.speedY + 15
                     END IF
                  END IF

                  'IF physics(2).y > killY + 24 THEN player.hp = 0: killY = killY + 32

                  a = physics(2).Y - screenTargetYlock
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

                c = tileRead(physics(n).X, physics(n).Y - 8)
                IF c > 29 THEN
                 physics(n).Y = (physics(n).Y AND &HFFF8) + 8
                 player.speedY = 0
                 IF c = 57 THEN damagePlayer
                END IF

                IF c = 28 THEN
                  addScore 998
                  soundEvent = 92
                  X = (physics(2).X) AND &HFFFC
                  Y = (physics(2).Y - 8) AND &HFFF8
                  tileWrite X, Y, 0
                  aUpdateTile X, Y
                  spawnObject X, Y, 99, 0, 0
                END IF

                d = player.speedX
                IF d > 0 THEN
                 IF friction > d THEN friction = d
                 friction = -friction
                 player.speedX = player.speedX + friction
                END IF
                IF d < 0 THEN
                 IF -friction < d THEN friction = -d
                 player.speedX = player.speedX + friction
                END IF

                IF kbArray(29) AND player.jumpRelease THEN
                 player.jumpRelease = 0
                 player.speedY = -35
                 'physics(n).Y = physics(n).Y - 2
                 soundEvent = 0
                END IF

                IF kbArray(75) THEN
                 player.speedX = player.speedX - speedMod
                 object(n).flags = 1
                 IF collisionFloor = 1 THEN
                  switchAnim n, 40
                  IF fakeFloat(2, floatC) THEN
                   sounds.queue(sounds.readPos) = walkNote(player.walkCounter)
                   player.walkCounter = (player.walkCounter + 1) AND 7
                  END IF
                 END IF
                END IF

                IF kbArray(77) THEN
                 player.speedX = player.speedX + speedMod
                 object(n).flags = 0
                 IF collisionFloor = 1 THEN
                  switchAnim n, 40
                  IF fakeFloat(2, floatC) THEN
                   sounds.queue(sounds.readPos) = walkNote(player.walkCounter)
                   player.walkCounter = (player.walkCounter + 1) AND 7
                  END IF
                 END IF
                END IF
                IF kbArray(80) THEN
                 screenTargetY = physics(2).Y - 24
                END IF

                player.ShootDelay = player.ShootDelay + (player.ShootDelay > 0)
                IF kbArray(56) AND player.ShootDelay = 0 THEN
                 player.ShootDelay = 10
                 spawnObject physics(2).X, physics(2).Y - 6, 100, object(n).flags, 0
                 soundEvent = 55
                 switchAnim n, 30
                END IF

                IF player.hp = 0 THEN
                 physics(n).collider = 0
                 object(n).control = 256
                 object(n).animFrame = 0
                 physics(n).speedX = 0
                 physics(n).speedY = 0
                 FOR b = 0 TO 63 STEP 8
                  xs = angle.X(b) \ 2
                  ys = angle.Y(b) \ 2
                  spawnObject physics(n).X, physics(n).Y, 257, xs, ys
                 NEXT b
                 queueSound 4, 22
                 queueSound 14, 23
                 
                 hudTextLoad &H140, 0
                 player.hudLock = 999

                 player.score = player.scoreBase
                 physics(2).X = 0
                 physics(2).Y = 0
                 player.jumpRelease = -1
                END IF

                IF player.speedX > 10 THEN player.speedX = 10
                IF player.speedX < -10 THEN player.speedX = -10
                IF player.speedY > maxY THEN player.speedY = maxY
                IF player.speedY < minY THEN player.speedY = minY

                physics(n).X = physics(n).X + fakeFloat(player.speedX, floatC)
                physics(n).Y = physics(n).Y + fakeFloat(player.speedY, floatC)

                c = tileRead(physics(n).X + 2, physics(n).Y - 4)
                  IF c > 29 THEN
                   physics(n).X = (physics(n).X AND &HFFFC) + 2
                   IF c = 59 THEN damagePlayer
                  END IF
                c = tileRead(physics(n).X - 2, physics(n).Y - 4)
                  IF c > 29 THEN
                   physics(n).X = (physics(n).X AND &HFFFC) + 1
                   IF c = 59 THEN damagePlayer
                  END IF

                screenTargetX = physics(n).X - 19
                handleCamera
                IF screenTargetYlock = -1 THEN
                  screenTargetY = physics(2).Y - 28
                END IF
  END SELECT

 screenX = gfx.screenX(gfx.page)
 screenY = gfx.screenY(gfx.page)
 

 FOR getObject = 2 TO spawnIndex

 n = objectIndex(getObject)

  '-------------------------------------------------------------------------
  IF object(n).exist > 0 THEN

      entityCounter = entityCounter + 1
      
      visDeltaX = ABS(screenX + 20 - physics(n).X)
      visDeltaY = ABS(screenY + 24 - physics(n).Y)

      IF visDeltaX < object(n).visX AND visDeltaY < object(n).visY THEN

       'entityVisibles = entityVisibles + 1

       'spdX = fakeFloat(physics(n).speedX, floatC)
       'spdY = fakeFloat(physics(n).speedY, floatC)

       parent = object(n).exist

       parentDeltaX = physics(n).X                       'Store local position
       parentDeltaY = physics(n).Y
       physics(n).X = physics(parent).X + physics(n).X   'Change to global position
       physics(n).Y = physics(parent).Y + physics(n).Y
       physics(n).X = physics(n).X + fakeFloat(physics(n).speedX, floatC)
       physics(n).Y = physics(n).Y + fakeFloat(physics(n).speedY, floatC)
       

        IF physics(n).Y < 8 THEN physics(n).Y = 8
        IF physics(n).Y > 630 THEN physics(n).Y = 630
        IF physics(n).X < -7 THEN physics(n).X = -7
        IF physics(n).X > 85 THEN physics(n).X = 85
      
      '-------------------------------------------------------------------
      'Player Bullet
      IF object(n).control = 100 THEN
            c = tileRead(physics(n).X, physics(n).Y)
            IF c > 29 THEN
             IF c = 38 OR c = 53 OR c = 56 THEN
              physics(n).X = physics(n).X AND &HFFFC
              physics(n).Y = physics(n).Y AND &HFFF8
              IF c = 38 THEN
               soundEvent = 44
               a = 130
               spawnObject physics(n).X, physics(n).Y, 99, 0, 0
               physics(n).speedY = 2
               object(n).flash = 16
               IF dtAnims = 1 THEN
                 t = 28
                ELSE
                 t = 0
               END IF
              END IF
              IF c = 53 THEN
               soundEvent = 122
               a = 160
               t = 52
               physics(n).speedY = 0
                IF player.hp > 1 THEN
                  o = 88
                 ELSE
                  o = 158
                END IF
               spawnObject physics(n).X + 2, physics(n).Y, 41, o, 0
              END IF
              IF c = 56 THEN
               soundEvent = 115
               IF physics(n).speedX < 0 THEN
                 s = -10
                ELSE
                 s = 10
                END IF
               a = 144
               spawnObject physics(n).X, physics(n).Y, 99, 3, s
              END IF

              addScore 999
              tileWrite physics(n).X, physics(n).Y, t
              aUpdateTile physics(n).X, physics(n).Y
              physics(n).speedX = 0
              object(n).flags = 0
              object(n).control = 0
              object(n).life = 4
              object(n).animFrame = a
             ELSE
              soundEvent = 44
              object(n).flags = 0
              object(n).control = 0
              physics(n).speedX = 0
              physics(n).collider = 0
              object(n).life = 4
              switchAnim n, 110
              physics(n).speedY = -5
              soundEvent = 44
             END IF
            END IF
            IF visDeltaX > 30 THEN object(n).life = 0
      END IF

      SELECT CASE object(n).control
       'SPIKE BALL U
       CASE 17
        c = angle.Y(mainCounter0) + 32
        physics(n).X = object(n).counter + multiplyTable(object(n).jump, angle.X(c))
        physics(n).Y = object(n).direction + multiplyTable(object(n).jump, angle.Y(c))

       CASE 18
       'SPIKE BALL O+
        physics(n).X = object(n).counter + multiplyTable(object(n).jump, angle.X(mainCounter0))
        physics(n).Y = object(n).direction + multiplyTable(object(n).jump, angle.Y(mainCounter0))
       
      END SELECT

      IF gfx.page > 0 AND object(n).control THEN

      playerDeltaX = ABS(physics(n).X - physics(2).X)
      playerDeltaY = ABS(physics(n).Y - (physics(2).Y - 6))
      
      SELECT CASE object(n).control

       CASE 99
       'Tile FX
       IF physics(n).speedX < 0 THEN
         t = physics(n).X
        ELSE
         t = physics(n).X + 4
       END IF
       c = tileRead(t, physics(n).Y)

       IF c > 29 THEN
        IF physics(n).speedX < 0 THEN physics(n).X = physics(n).X + 4
        physics(n).X = physics(n).X AND &HFFFC
        physics(n).Y = physics(n).Y AND &HFFF8
        tileWrite physics(n).X, physics(n).Y, 56
        aUpdateTile physics(n).X, physics(n).Y
        object(n).life = 3
        object(n).control = 0
        physics(n).speedX = 0
        physics(n).speedY = 0
        soundEvent = 127
       END IF
       
       'Dropping particle
       CASE 200
            IF physics(n).speedY < 20 THEN physics(n).speedY = physics(n).speedY + 4
       
       'Pickup
       CASE 41
            c = tileRead(physics(n).X, physics(n).Y)
            IF physics(n).speedY < 30 THEN physics(n).speedY = physics(n).speedY + 4
            
            IF c > 29 AND physics(n).Y > object(n).counter THEN
             IF physics(n).speedY > 15 THEN
               physics(n).Y = physics(n).Y AND &HFFF8
               physics(n).speedY = (NOT physics(n).speedY) \ 2
               soundEvent = 32
              ELSE
               physics(n).speedY = 0
             END IF
            END IF

            IF object(n).life < 37 AND object(n).anim <> 90 THEN object(n).flash = 36

            IF playerDeltaX < 5 AND ABS(physics(2).Y - physics(n).Y) < 8 THEN
            spawnerEnabler(object(n).spawner) = 0
            soundEvent = 34
             IF object(n).anim = 90 THEN
               player.score = player.score + 500
               player.hasKey = 1
               object(n).control = 0
               physics(n).speedX = 0
               physics(n).speedY = -15
               object(n).life = 8
               object(n).flash = 16
              ELSE
               addScore object(n).animFrame
               physics(n).Y = physics(n).Y - 3
               physics(n).speedY = 0
               object(n).flash = 0
               object(n).life = 9
               object(n).control = 0
               switchAnim n, 93
             END IF
            END IF

       'Chest
       CASE 45
         IF playerDeltaX < 5 AND playerDeltaY < 10 AND player.hasKey THEN
          player.hasKey = 0
          spawnObject physics(n).X - 6, physics(n).Y - 12, 41, 98, 0
          spawnObject physics(n).X - 4, physics(n).Y - 14, 41, 98, 0
          spawnObject physics(n).X, physics(n).Y - 16, 41, 88, 0
          spawnObject physics(n).X + 4, physics(n).Y - 15, 41, 98, 0
          spawnObject physics(n).X + 6, physics(n).Y - 14, 41, 98, 0
          object(n).life = 5
          object(n).animFrame = 195
          physics(n).speedY = -5
          soundEvent = 104
          spawnerEnabler(object(n).spawner) = 0
         END IF

       'Enemy bullet
       CASE 102
         IF playerDeltaX < 4 AND playerDeltaY < 6 THEN
          damagePlayer
          object(n).life = 0
         END IF

       'Drip
       CASE 104
         IF physics(n).speedX > 0 THEN
           physics(n).speedX = physics(n).speedX - 1
          ELSEIF physics(n).speedX < 0 THEN
           physics(n).speedX = physics(n).speedX + 1
         END IF
         IF physics(n).speedY < 30 THEN physics(n).speedY = physics(n).speedY + 4
         IF playerDeltaX < 2 AND playerDeltaY < 5 THEN
          damagePlayer
          object(n).flash = 16
          object(n).life = 0
          object(n).control = 0
          physics(n).speedY = -10
         END IF

       'Fire Bomb
       CASE 112
        IF object(n).counter < 10 THEN
         'c = tileRead(physics(n).x, physics(n).y)
         'IF c > 29 THEN
         ' object(n).life = 6
         ' object(n).counter = 10
         'END IF
         IF physics(n).speedY < 35 THEN physics(n).speedY = physics(n).speedY + 2
        END IF
         IF playerDeltaX < 4 AND playerDeltaY < 6 THEN
          damagePlayer
          object(n).life = 0
         END IF



       'Eggy
       CASE 1
           physics(n).speedY = 20
           IF object(n).direction = 2 THEN
             IF object(n).counter < 0 AND object(n).counter > -8 THEN
               physics(n).speedY = angle.Y(object(n).jump)
               object(n).jump = (object(n).jump + 2) AND 63
               c = tileRead(physics(n).X, physics(n).Y - 8)
               IF c > 29 THEN physics(n).speedY = 0: physics(n).Y = physics(n).Y AND &HFFF8 + 7
              ELSE
               object(n).jump = 0
             END IF
            ELSE
             IF object(n).counter < 0 AND object(n).counter > -5 THEN
               physics(n).speedY = angle.Y(object(n).jump)
               object(n).jump = (object(n).jump + 4) AND 63
               c = tileRead(physics(n).X, physics(n).Y - 8)
               IF c > 29 THEN physics(n).speedY = 0: physics(n).Y = physics(n).Y AND &HFFF8 + 7
              ELSE
               object(n).jump = 0
             END IF
           END IF

            IF physics(n).speedY > -1 THEN
             c = tileRead(physics(n).X, physics(n).Y)
             IF c > 29 THEN
               physics(n).speedX = 0
               object(n).animFrame = 60
               physics(n).speedY = 0
               physics(n).Y = physics(n).Y AND &HFFF8
               IF object(n).counter = 0 AND object(n).jump = 0 THEN
                object(n).animFrame = 62
                physics(n).speedX = 5 + (object(n).flags > 0) * 10
               END IF
             END IF
            END IF

            object(n).counter = object(n).counter + (object(n).counter < 1)

            IF object(n).counter < -10 AND c > 29 THEN
             object(n).counter = 0
             object(n).jump = 0
             object(n).direction = object(n).direction + 1
             IF object(n).direction > 2 THEN object(n).direction = 0
             soundEvent = 142
            END IF

         IF object(n).flags THEN
           s = -2
           p = 1
          ELSE
           s = 2
           p = 3
         END IF
         c = tileRead(physics(n).X + s, physics(n).Y - 1)
         IF c > 29 THEN
            object(n).flags = object(n).flags XOR 1
            physics(n).speedX = 0
            physics(n).X = (physics(n).X AND &HFFFC) + p
         END IF

       
       'HELMET
       CASE 2
        c = tileRead(physics(n).X, physics(n).Y)
        IF c > 30 THEN
          physics(n).Y = physics(n).Y AND &HFFF8
          physics(n).speedY = 0
         ELSE
          physics(n).speedX = 0
          physics(n).speedY = 20
        END IF
        IF object(n).life > -50 THEN
         physics(n).Y = physics(n).Y - 6
         object(n).life = -50
         object(n).counter = 32
         physics(n).collider = 0
         object(n).direction = physics(n).speedX
         physics(n).speedX = 0
        END IF
       IF object(n).counter = 0 THEN
        IF c > 30 THEN
           IF object(n).flags THEN
             physics(n).speedX = -4
             c = tileRead(physics(n).X - 2, physics(n).Y - 2)
             c2 = tileRead(physics(n).X - 2, physics(n).Y + 2)
              IF c > 29 OR c2 < 30 THEN
               object(n).flags = 0
               physics(n).speedX = 4
               physics(n).X = physics(n).X AND &HFFFC + 2
              END IF
           ELSE
             physics(n).speedX = 4
             c = tileRead(physics(n).X + 2, physics(n).Y - 2)
             c2 = tileRead(physics(n).X + 2, physics(n).Y + 2)
              IF c > 29 OR c2 < 30 THEN
               object(n).flags = 1
               physics(n).speedX = -4
               physics(n).X = physics(n).X AND &HFFFC + 2
              END IF
           END IF
        END IF
       ELSE
        object(n).counter = object(n).counter - 1
        object(n).animFrame = 239
        IF object(n).counter = 0 THEN
         object(n).animFrame = 232
         physics(n).collider = 1
         physics(n).speedX = object(n).direction
        END IF
       END IF

       'Dragon
       CASE 3
        object(n).counter = object(n).counter + 1
         IF object(n).counter < 32 THEN
          c = physics(2).X - 15
         ELSE
          c = physics(2).X + 15
          object(n).counter = object(n).counter AND 63
         END IF
        IF physics(n).X < c THEN physics(n).speedX = physics(n).speedX + 2
        IF physics(n).X > c THEN physics(n).speedX = physics(n).speedX - 2
        
        IF physics(n).Y < physics(2).Y - 27 THEN physics(n).speedY = physics(n).speedY + 5
        IF physics(n).Y > physics(2).Y - 25 THEN physics(n).speedY = physics(n).speedY - 5
        IF physics(n).speedY < -10 THEN physics(n).speedY = -10
        IF physics(n).speedY > 10 THEN physics(n).speedY = 10
        IF physics(n).speedX < -8 THEN physics(n).speedX = -8
        IF physics(n).speedX > 8 THEN physics(n).speedX = 8
        
        IF playerDeltaX < 5 THEN
         IF object(n).jump < 1 THEN
           spawnObject physics(n).X, physics(n).Y + 1, 112, 0, 0
           object(n).jump = 4
          ELSE
           object(n).jump = object(n).jump - 1
         END IF
        END IF

       'Drooly
       CASE 4
        IF physics(n).speedX THEN
          c = tileRead(physics(n).X + physics(n).speedX, physics(n).Y + 1)
          IF c < 30 THEN physics(n).speedX = physics(n).speedX * -1
          c = tileRead(physics(n).X + physics(n).speedX, physics(n).Y - 2)
          IF c > 29 THEN physics(n).speedX = physics(n).speedX * -1
        END IF

        object(n).jump = object(n).jump + 1

        IF object(n).jump = 15 THEN
          object(n).counter = physics(n).speedX
          physics(n).speedX = 0
          spawnObject physics(n).X + 1, physics(n).Y, 104, -10, 0
          spawnObject physics(n).X + 1, physics(n).Y, 104, 10, 0
          soundEvent = 145
        END IF

        IF object(n).jump > 25 THEN
         object(n).jump = 0
         physics(n).speedX = object(n).counter
        END IF

       'Babat
       CASE 5
         object(n).jump = object(n).jump + 1
         
         SELECT CASE object(n).counter
         CASE 0
            IF playerDeltaX < 16 AND playerDeltaY < 32 AND physics(2).Y > physics(n).Y THEN
             object(n).counter = 1
             object(n).animFrame = 202
             object(n).jump = physics(2).X
            END IF

         CASE 1
            IF physics(n).Y < physics(2).Y THEN
               physics(n).speedY = 5
               IF physics(n).X < physics(2).X THEN physics(n).speedX = physics(n).speedX + 1
               IF physics(n).X > physics(2).X THEN physics(n).speedX = physics(n).speedX - 1
            ELSE
              physics(n).speedY = 10
              physics(n).speedX = 0
              object(n).counter = 2
              object(n).jump = physics(2).Y - 32
              object(n).direction = physics(2).X
            END IF
         
         CASE 2
          IF physics(n).Y > object(n).jump + 5 THEN
            physics(n).speedY = physics(n).speedY - 5
           ELSE
            object(n).counter = 3
            object(n).jump = 0
          END IF

         CASE 3
          object(n).jump = object(n).jump + 1
          physics(n).speedY = multiplyTable(2, physics(n).speedY)
          IF object(n).jump > 16 THEN object(n).counter = 1
         END SELECT

         IF physics(n).speedY > 20 THEN physics(n).speedY = 20
         IF physics(n).speedY < -20 THEN physics(n).speedY = -20
         IF physics(n).speedX > 10 THEN physics(n).speedX = 10
         IF physics(n).speedX < -10 THEN physics(n).speedX = -10
       
       'SKULL
       CASE 6
         
         IF object(n).flags THEN
           physics(n).speedX = -8
           IF physics(n).X < screenX - 4 THEN object(n).flags = 0 ' soundEvent = 151
          ELSE
           physics(n).speedX = 8
           IF physics(n).X > screenX + 44 THEN object(n).flags = 1 ' soundEvent = 151
         END IF
         c = (mainCounter0 + object(n).counter) AND 63
         physics(n).Y = object(n).counter + multiplyTable(1, angle.Y(c))

         'FIRE BREATH
       CASE 11
         X = (mainCounter1 + physics(n).Y) AND 31
         IF X > 10 THEN
          IF X = 11 THEN
           object(n).animFrame = 152: soundEvent = 66
          END IF
          IF X = 15 THEN
           spawnObject physics(n).X, physics(n).Y - 2, 111, object(n).flags, -10
           spawnObject physics(n).X, physics(n).Y - 2, 111, object(n).flags, 0
           spawnObject physics(n).X, physics(n).Y - 2, 111, object(n).flags, 10
          END IF
         ELSE
           object(n).animFrame = 150
         END IF

       'SPIKE BALL BOUNCE
       CASE 15
        c = tileRead(physics(n).X, physics(n).Y + 6)
        IF c > 29 THEN
          physics(n).Y = physics(n).Y AND &HFFF8 - 6
          physics(n).speedY = -30
          sounds.queue(sounds.readPos) = 40
         ELSE
          physics(n).speedY = physics(n).speedY + 6
          IF physics(n).speedY > 20 THEN physics(n).speedY = 20
        END IF

       'CHECKPOINT
       CASE 62
          IF playerDeltaY < 10 AND player.wave <> object(n).flags THEN
            spawnerEnabler(object(n).spawner) = spawnerEnabler(object(n).spawner) AND &HFFF
            player.wave = object(n).flags
            FOR nn = 4 TO 60
             IF ABS(physics(nn).Y - physics(2).Y) > 40 THEN
              IF object(nn).control > 0 AND object(nn).control < 63 THEN
               spawnerEnabler(object(nn).spawner) = spawnerEnabler(object(nn).spawner) AND &HFFF
              END IF
              object(nn).control = 0
              object(nn).life = 0
             END IF
            NEXT nn
            handleSpawn player.wave
          END IF

        'EXIT
        CASE 63
         IF (mainCounter1 AND 3) = 2 THEN
          X = physics(n).X - 4 + INT(RND * 10)'(randInt AND 7)
          Y = physics(n).Y - 22 + INT(RND * 15)' (randInt AND 15)
          spawnObject X, Y, 93, 0, 0
         END IF

         IF playerDeltaX < 16 AND playerDeltaY < 10 THEN
          IF kbArray(72) THEN
           object(2).control = 260
           object(2).counter = 0
           object(n).control = 0
           object(n).exist = 0
          END IF
         END IF

        'TILE FX
        CASE 99
         IF object(n).jump = 1 THEN
         END IF

       END SELECT

      END IF 'Page check
        '-------------------------------------------------------------------

      parts = animation(object(n).animFrame).jump AND object(n).animFrame > 0
      
      flipIndex = object(n).flags AND 1

      flash = NOT object(n).flash AND 1
      object(n).flash = object(n).flash + (object(n).flash > 0)

      xx = physics(n).X - screenX + 20
      yy = physics(n).Y - screenY + 20

      physics(n).X = physics(n).X - physics(parent).X    'Return position
      physics(n).Y = physics(n).Y - physics(parent).Y    ' to local space.
   
      IF physics(n).collider THEN
       FOR animPart = object(n).animFrame TO object(n).animFrame + parts - 1
         spriteIndex = animation(animPart).index(flipIndex)
         X = xx + animation(animPart).X(flipIndex)
         Y = yy + animation(animPart).Y
         'sx = x' - screenX
         'sy = y' - screenY

         IF spriteIndex AND X > 12 AND X < 60 AND Y > 5 AND Y < 70 THEN
          IF flash THEN
           spriteList(spriteListIndex, gfx.page).offset = spriteOffset(spriteIndex)
           spriteList(spriteListIndex, gfx.page).X = X' + 20
           spriteList(spriteListIndex, gfx.page).Y = Y' + 20
           spriteList(spriteListIndex, gfx.page).w = spriteWidth(spriteIndex)
           spriteList(spriteListIndex, gfx.page).h = spriteHeight(spriteIndex)
           spriteListIndex = spriteListIndex + 1
          END IF
          ctype = physics(n).collider AND (animation(animPart).jump > 0)
            IF ctype THEN
             colCount(ctype) = colCount(ctype) + 1
             colList(ctype, colCount(ctype)).index = n
             colList(ctype, colCount(ctype)).x0 = X
             colList(ctype, colCount(ctype)).y0 = Y
             colList(ctype, colCount(ctype)).x1 = X + spriteWidth(spriteIndex) - 1
             colList(ctype, colCount(ctype)).y1 = Y + spriteHeight(spriteIndex) - 1
            END IF
         END IF
       NEXT animPart
      ELSE
       IF flash THEN
         FOR animPart = object(n).animFrame TO object(n).animFrame + parts - 1
          spriteIndex = animation(animPart).index(flipIndex)
          X = xx + animation(animPart).X(flipIndex)
          Y = yy + animation(animPart).Y
      
          IF spriteIndex AND X > 12 AND X < 60 AND Y > 5 AND Y < 70 THEN
           spriteList(spriteListIndex, gfx.page).offset = spriteOffset(spriteIndex)
           spriteList(spriteListIndex, gfx.page).X = X '+ 20
           spriteList(spriteListIndex, gfx.page).Y = Y '+ 20
           spriteList(spriteListIndex, gfx.page).w = spriteWidth(spriteIndex)
           spriteList(spriteListIndex, gfx.page).h = spriteHeight(spriteIndex)
           spriteListIndex = (spriteListIndex + 1) AND 31
          END IF
        NEXT animPart
       END IF
      END IF
  END IF 'visibility check

   IF gfx.page THEN
    object(n).life = object(n).life + (object(n).life > 0)
    IF object(n).life = 0 THEN
     IF object(n).control > 0 AND object(n).control < 63 THEN
      spawnerEnabler(object(n).spawner) = 0
      'spawnerEnabler(object(n).spawner) = spawnerEnabler(object(n).spawner) AND &HFFF
     END IF
     IF object(n).control > 0 AND object(n).control < 11 THEN
          object(n).exist = 1
          object(n).flags = 0
          object(n).control = 41
          object(n).flash = 4
          physics(n).speedX = 0
          physics(n).speedY = -20
          physics(n).Y = physics(n).Y - 4
          physics(n).collider = 0
          object(n).life = 113
          object(n).animFrame = 98
          c = tileRead(physics(n).X, physics(n).Y)
          IF c > 29 THEN
            object(n).counter = physics(n).Y + 8
           ELSE
            object(n).counter = physics(n).Y
          END IF
      ELSE
          destroyObject getObject
          getObject = getObject - 1
     END IF
    END IF
   END IF

  END IF 'exist check
 NEXT getObject


 IF dtAnims > 3 THEN
  dtAnims = 0
  mainCounter2 = (mainCounter2 + 1) AND 63
      FOR getObject = 2 TO spawnIndex
       n = objectIndex(getObject)
       object(n).animFrame = object(n).animFrame + animation(object(n).animFrame).jump
       IF animation(object(n).animFrame).jump < 0 THEN object(n).animFrame = object(n).animFrame + animation(object(n).animFrame).jump
       'IF physics(n).y - object(n).visY > killY THEN
       ' IF n <> 2 THEN object(n).control = 0: object(n).life = 0
       'END IF
      NEXT getObject
 END IF

 IF gfx.page THEN mainCounter1 = (mainCounter1 + 1) AND 63

 'dtAnims = dtAnims AND 3

 spriteList(spriteListIndex, gfx.page).Y = 255

 IF soundEvent > -1 THEN queueSound 0, soundEvent

 IF player.score > scoreDelta THEN
  hudUpdate 4
 END IF

 mainCounter0 = (mainCounter0 + 1) AND 63

END SUB

SUB handleCollision

  soundEvent = -1

      'Collision lists
      ' 1 = Affect Player
      ' 2 = Affect Enemy
      ' 3 = Affect Bullet

 IF object(2).control = 255 AND object(2).flash = 0 THEN
  'px0 = physics(2).x - gfx.screenX(gfx.page) + 20 - 2
  'py0 = physics(2).y - gfx.screenY(gfx.page) + 20 - 8
  'px1 = px0 + 3
  'py1 = py0 + 5
  px = physics(2).X - gfx.screenX(gfx.page) + 20
  py = physics(2).Y - gfx.screenY(gfx.page) + 14

  FOR a = 0 TO colCount(1)
   n = colList(1, a).index
   'IF collision(px0, py0, px1, py1, colList(1, a).x0, colList(1, a).y0, colList(1, a).x1, colList(1, a).y1) THEN
   'IF px0 > colList(1, a).x0 AND px0 < colList(1, a).x1 AND py0 > colList(1, a).y0 AND py0 < colList(1, a).y1 THEN
   IF px > colList(1, a).x0 - 2 AND px < colList(1, a).x1 + 2 AND py > colList(1, a).y0 - 3 AND py < colList(1, a).y1 + 3 THEN
    damagePlayer
    a = 9999
   END IF
  NEXT a
 END IF

  FOR b = 0 TO colCount(2)
   n = colList(2, b).index
   px = physics(n).X - gfx.screenX(gfx.page) + 20
   py = physics(n).Y - gfx.screenY(gfx.page) + 20
   FOR a = 0 TO colCount(1)

     'IF collision(colList(1, a).x0, colList(1, a).y0, colList(1, a).x1, colList(1, a).y1, colList(2, b).x0, colList(2, b).y0, colList(2, b).x1, colList(2, b).y1) THEN
     IF px > colList(1, a).x0 - 2 AND px < colList(1, a).x1 + 2 AND py > colList(1, a).y0 - 3 AND py < colList(1, a).y1 + 3 THEN
       n2 = colList(1, a).index
       physics(n).collider = 0
       object(n).life = 4
       physics(n).speedY = -5
       object(n).control = 0
       physics(n).speedX = 0
       object(n).flags = 0
       'object(n).flash = 20
       IF object(n2).life > -999 THEN
        object(n2).flash = 10
        object(n2).life = object(n2).life + 1
       END IF
       IF object(n2).life > -1 THEN
         physics(n).X = physics(n2).X
         object(n).animFrame = 162
         soundEvent = 44
        ELSE
         object(n).animFrame = 110
         soundEvent = 44
       END IF
      END IF
   NEXT a
  NEXT b

  IF soundEvent > -1 THEN queueSound 0, soundEvent

END SUB

SUB handleSpawn (wave)
 
 FOR n = 1 TO 16
  nn = wave * 16 + n
  o = level.wave(wave).spawner(n).object
  IF spawnerEnabler(nn) > 0 THEN o = spawnerEnabler(nn)

  IF o THEN
    spawnObject level.wave(wave).spawner(n).X, level.wave(wave).spawner(n).Y, o, level.wave(wave).spawner(n).flags, nn
    spawnerEnabler(nn) = o OR &HF000
    level.wave(wave).spawner(n).object = 0
  END IF
 NEXT n
 
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
  w40char.text = STR$(INT(fps)) + "|"
  aPrint &HFFFF, &HE0C, w40char.seg, w40char.ofs, 4, 0, hudBufferSeg, hudBufferOfs

  'hudPrint 4, STR$(INT(fps)), &HF, &HE
 END IF

 w40char.text = STR$(ASC(MID$(hudBuffer, 161, 1))) + "|"
 aPrint &HFFFF, &HF0F, w40char.seg, w40char.ofs, 7, 0, hudBufferSeg, hudBufferOfs

END SUB

SUB initGame

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

 'loadSprites "spenemy0", 1

 'loadTiles "tileset0"

 player.level = 1

 randomOffset = 0

 DO
  main
 LOOP

END SUB

SUB main

 hudTextLoad &HA0, 1
 
 loadLevel "level" + MID$(STR$(player.level), 2, 1)
 
 physics(1).X = 0
 physics(1).Y = 0

 FOR n = 0 TO 128
  spawnerEnabler(n) = 0
 NEXT n

 FOR n = 2 TO 60
  objectIndex(n) = n
 NEXT n

 FOR n = 2 TO 60
  destroyObject 2
 NEXT n

 spawnIndex = 4

 colCount(1) = -1
 colCount(2) = -1
 colCount(3) = -1
 
 player.hp = 4
 player.time = 300
 player.hasKey = 0
 player.jumpRelease = -1
 player.wave = 0
 player.scoreBase = player.score

 object(2).exist = 1
 object(2).control = 255
 object(2).life = -3
 physics(2).X = level.PlayerX
 physics(2).Y = level.PlayerY
 physics(2).collider = 0

 physics(2).speedX = 0
 physics(2).speedY = 0
 player.speedY = 0
 player.speedX = 0
 player.Bouncing = 0

 object(2).animFrame = 10
 object(2).anim = 10

 object(2).visX = 999
 object(2).visY = 999

 objectIndex(2) = 2

 playerOldX = -999
 playerOldY = -999

 killY = 999

 handleSpawn 0

 screenResetPos physics(2).X - 20, physics(2).Y - 46

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

      IF object(2).control = 259 THEN EXIT SUB

      IF object(2).control = 255 THEN
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
         IF object(2).control = 255 THEN player.time = player.time + (player.time > 0)
         updateText = 0
         hudUpdate &H8
        END IF
      END IF

      aPageFlip gfx.videoWrap, hudBufferSeg, hudBufferOfs, gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
      gfx.page = gfx.page XOR 1

      aRectList gfx.WindowOfs(gfx.page), gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), tileBufferWrap, tileBufferSeg, tileBufferOfs, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)
 LOOP

END SUB

FUNCTION randInt
 r = (PEEK(randomOffset) + 2)
 randomOffset = (r + randomOffset) AND 8191
 randInt = r
END FUNCTION

SUB spawnObject (X, Y, object, spawnerFlags, spawner)
 
 'n = getSpawnSlot
 n = 0
 FOR b = 4 TO 60
  IF object(b).exist = 0 THEN n = b: b = 999
 NEXT b
 
 object(n).spawner = spawner
 object(n).flash = 0
 object(n).counter = 0
 object(n).flags = 0
 object(n).life = -999
 physics(n).X = X
 physics(n).Y = Y
 physics(n).speedX = 0
 physics(n).speedY = 0
 physics(n).collider = 0
 
 object(n).visX = defaultVisX
 object(n).visY = defaultVisY
 
 SELECT CASE object

  CASE 257
   'Player Die
   objectIndex(spawnIndex) = n
   object(n).control = 0
   object(n).life = 25
   object(n).exist = 1
   object(n).animFrame = 110
   physics(n).speedX = spawnerFlags
   physics(n).speedY = spawner
   object(n).visY = 40
   incSpawnIndex

  CASE 200
   'Explode
   objectIndex(spawnIndex) = n
   object(n).control = 0
   object(n).life = 15
   object(n).exist = 1
   object(n).spawner = 0
   physics(n).collider = 0
   object(n).animFrame = 110
   object(n).anim = 110
   incSpawnIndex

  CASE 99
   'Clear Tile

   objectIndex(spawnIndex) = n
   object(n).control = 0

   SELECT CASE spawnerFlags
    CASE 0
     object(n).life = 3
     object(n).animFrame = 144
    CASE 1
     object(n).life = 2
     object(n).animFrame = 160
     physics(n).speedX = spawner
    CASE 2
     object(n).life = 2
     object(n).animFrame = 160
     physics(n).speedY = spawner
    CASE 3
     object(n).life = 40
     object(n).animFrame = 160
     physics(n).speedX = spawner
     object(n).visX = 70
     object(n).visY = 70
     object(n).control = 99
    CASE 4
     object(n).life = 40
     object(n).animFrame = 160
     physics(n).speedY = spawner
     object(n).visX = 999
     object(n).visY = 999
     object(n).control = 99
   END SELECT
   object(n).exist = 1
   object(n).jump = spawnerFlags
   incSpawnIndex

  CASE 98
   'Tile FX
   objectIndex(spawnIndex) = n
   object(n).control = 98
   object(n).exist = 1
   object(n).life = spawnerFlags
   object(n).animFrame = 160
   object(n).anim = 160
   object(n).visX = 999
   object(n).visY = 999
   incSpawnIndex
   
  CASE 93
  'Twinkle particle
   objectIndex(spawnIndex) = n
   object(n).exist = 1
   object(n).control = 0
   object(n).animFrame = 93
   object(n).life = 10
   physics(n).speedY = 3
   incSpawnIndex
   
  CASE 100
   'Player Bullet
   objectIndex(spawnIndex) = n
   object(n).control = 100
   object(n).life = 20
   object(n).exist = 1
   object(n).spawner = 0
   object(n).flags = spawnerFlags
   physics(n).collider = 2
   physics(n).speedX = 30 + (spawnerFlags <> 0) * 60
   object(n).animFrame = 100
   object(n).anim = 100
   incSpawnIndex
   
  CASE 102
   'Enemy Bullet
   objectIndex(spawnIndex) = n
   object(n).control = 102
   object(n).life = 20
   object(n).exist = 1
   object(n).spawner = 0
   physics(n).speedX = 10 + (spawnerFlags <> 0) * 20
   physics(n).speedY = 5
   object(n).animFrame = 102
   object(n).anim = 102
   object(n).visX = 40
   object(n).visY = 40
   incSpawnIndex

  CASE 104
   'DRIP
   objectIndex(spawnIndex) = n
   object(n).control = 104
   object(n).exist = 1
   object(n).life = 20
   physics(n).speedX = spawnerFlags
   physics(n).speedY = -30
   object(n).animFrame = 214
   object(n).anim = 214
   object(n).visY = 30
   incSpawnIndex

  CASE 111
   'FIRE PARTICLE
   objectIndex(spawnIndex) = n
   object(n).control = 102
   object(n).life = 20
   object(n).exist = 1
   object(n).spawner = 0
   physics(n).speedX = 7 + (spawnerFlags <> 0) * 14
   physics(n).speedY = spawner
   object(n).animFrame = 156
   object(n).visX = 40
   object(n).visY = 40
   incSpawnIndex

  CASE 112
   'FIRE BOMB
   objectIndex(spawnIndex) = n
   object(n).control = 112
   object(n).life = 30
   object(n).exist = 1
   object(n).spawner = 0
   physics(n).speedX = 0
   physics(n).speedY = 5
   object(n).animFrame = 156
   object(n).visX = 40
   object(n).visY = 50
   incSpawnIndex
   
  CASE 1
   'Eggy
   objectIndex(spawnIndex) = n
   object(n).flags = spawnerFlags
   object(n).control = 1
   object(n).exist = 1
   object(n).life = -2
   physics(n).collider = 1
   object(n).animFrame = 60
   object(n).anim = 60
   object(n).counter = 0
   object(n).visX = 25
   object(n).visY = 40
   incSpawnIndex

  CASE 2
   'HELMET
   objectIndex(spawnIndex) = n
   object(n).control = 2
   object(n).exist = 1
   object(n).life = -50
   physics(n).collider = 1
   object(n).animFrame = 232
   object(n).counter = 0
   object(n).visY = 35
   incSpawnIndex

  CASE 3
   'DRAGON
   objectIndex(spawnIndex) = n
   object(n).control = 3
   object(n).exist = 1
   object(n).life = -2
   physics(n).collider = 1
   object(n).animFrame = 227
   object(n).counter = 0
   object(n).visY = 55
   object(n).visX = 55
   incSpawnIndex


  CASE 4
   'DROOLY
   objectIndex(spawnIndex) = n
   object(n).control = 4
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -1
   physics(n).collider = 1
   physics(n).speedX = 2
   object(n).animFrame = 208
   object(n).anim = 208
   object(n).visY = 30
   incSpawnIndex

  CASE 5
   'BABAT
   objectIndex(spawnIndex) = n
   object(n).control = 5
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -2
   object(n).direction = Y
   physics(n).collider = 1
   object(n).animFrame = 200
   object(n).visX = 30
   object(n).visY = 50
   object(n).counter = 0
   incSpawnIndex

  CASE 6
   'SKULL
   objectIndex(spawnIndex) = n
   object(n).control = 6
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -999
   physics(n).collider = 1
   object(n).animFrame = 220
   object(n).visX = 999
   object(n).visY = 30
   object(n).counter = Y
   incSpawnIndex

  CASE 11 TO 12
   'FIRE BREATH R / L
   objectIndex(spawnIndex) = n
   object(n).control = 11
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -9999
   object(n).animFrame = 150
   object(n).anim = 150
   object(n).visY = 25
   object(n).visX = 21
   d = 8 + ((11 - object) * 13)
   object(n).direction = d
   IF object = 12 THEN object(n).flags = 1
   incSpawnIndex

  CASE 15
  'Bouncing spike ball

    objectIndex(spawnIndex) = n
    physics(n).collider = 1
    object(n).control = 15
    object(n).exist = 1
    object(n).life = -9999
    object(n).animFrame = 146
    object(n).visX = 24
    object(n).visY = 50
    incSpawnIndex

  CASE 16
  'SPAWNER
    objectIndex(spawnIndex) = n
    object(n).control = 16
    object(n).exist = 1
    object(n).life = -9999
    object(n).animFrame = 0
    object(n).visX = 30
    object(n).visY = 70
    object(n).jump = spawnerFlags
    incSpawnIndex


  CASE 17 TO 18
  'SPIKE BALL U, O+, O-
   c = object
   FOR p = 0 TO 1
    objectIndex(spawnIndex) = n
    physics(n).X = X
    physics(n).Y = Y
    object(n).counter = X
    object(n).direction = Y
    object(n).control = c
    object(n).exist = 1
    object(n).spawner = spawner
    object(n).life = -999
    object(n).jump = 2 + p
    object(n).animFrame = 148
    object(n).anim = 148
    object(n).visX = 45
    object(n).visY = 45
    incSpawnIndex
    n = getSpawnSlot
   NEXT p
    objectIndex(spawnIndex) = n
    physics(n).collider = 1
    physics(n).X = X
    physics(n).Y = Y
    object(n).counter = X
    object(n).direction = Y
    object(n).control = c
    object(n).exist = 1
    object(n).spawner = spawner
    object(n).life = -999
    object(n).jump = 5
    object(n).animFrame = 146
    object(n).anim = 146
    object(n).visX = 50
    object(n).visY = 50
    incSpawnIndex

  CASE 41
   'PICKUP GEM/KEY/ETC...
   objectIndex(spawnIndex) = n
   object(n).control = 41
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = 113
   object(n).counter = Y
   physics(n).speedY = -20
   object(n).animFrame = spawnerFlags
   object(n).anim = spawnerFlags
   object(n).visY = 34
   incSpawnIndex

  CASE 44
   'KEY
   objectIndex(spawnIndex) = n
   object(n).control = 41
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -999
   physics(n).speedY = 0
   object(n).animFrame = 90
   object(n).anim = 90
   incSpawnIndex

  CASE 45
   'CHEST
   objectIndex(spawnIndex) = n
   object(n).control = 45
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -999
   physics(n).speedY = 0
   object(n).animFrame = 86
   object(n).anim = 86
   incSpawnIndex
   
  CASE 62
   'Checkpoint / Spawner portal
   objectIndex(spawnIndex) = n
   object(n).control = 62
   object(n).exist = 1
   object(n).flags = spawnerFlags
   object(n).life = -3
   object(n).animFrame = 0
   object(n).anim = 0
   object(n).visX = 999
   incSpawnIndex

   'p = n
   'n = getSpawnSlot
   'objectIndex(spawnIndex) = n
   'object(n).exist = p
   'incSpawnIndex

  CASE 63
   'EXIT
   objectIndex(spawnIndex) = n
   object(n).animFrame = 0
   object(n).control = 63
   object(n).exist = 1
   incSpawnIndex

  CASE 71
   'MENU CURSOR
   objectIndex(2) = 2
   object(2).flash = 0
   object(2).control = 258
   object(2).exist = 1
   physics(2).X = X
   physics(2).Y = Y
   physics(2).speedX = 0
   physics(2).speedY = 0
   object(2).anim = 10
   object(2).animFrame = 10
   object(2).flags = 0
   screenResetPos X - 20, physics(2).Y
   player.menutarget = 0
   player.mode = 0
   player.targetX = X
   player.targetY = Y
   object(2).counter = 0

  CASE 72
   'MENU ITEM
   objectIndex(spawnIndex) = n
   object(n).control = 72
   object(n).exist = 1
   object(n).life = -999
   object(n).animFrame = 0
   object(n).flags = spawnerFlags
   incSpawnIndex

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
 'IF d > 3 THEN speedDiff% = 10 * m: EXIT FUNCTION
 speedDiff% = 10 * m
END FUNCTION


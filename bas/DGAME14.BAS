DECLARE FUNCTION randInt% ()
DECLARE SUB hudUpdate (flags AS INTEGER)
DECLARE SUB addScore (eventType%)
DECLARE FUNCTION collision% (x0%, y0%, w0%, h0%, x1%, y1%, w1%, h1%)
DECLARE SUB damagePlayer ()
DECLARE SUB hudPrint (x%, text$, colour1%, colour2%)
DECLARE SUB aDrawTiles (area%, forceOfs%)
DECLARE SUB aUpdateTile (x%, y%)
DECLARE SUB aCopyWindow (page%)
DECLARE SUB waitForRetrace ()
DECLARE SUB aUpdateVideo ()
DECLARE SUB aUpdateBuffer ()
DECLARE SUB queueSound (offset%, Position%)

DECLARE SUB playSounds ()
DECLARE SUB aSprite ()
DECLARE SUB handleCollision ()
DECLARE SUB debugWrite (message$)
DECLARE SUB screenResetPos (x%, y%)
'$INCLUDE: 'libelf.bi'

DECLARE SUB destroyObject (index%)
DECLARE SUB tileWrite (x AS INTEGER, y AS INTEGER, index AS INTEGER)
DECLARE SUB deltaTime ()
DECLARE SUB elfInit ()
DECLARE SUB fakeFloats ()
DECLARE SUB handleAnims ()
DECLARE SUB handleCamera ()
DECLARE SUB handleKeyboard ()
DECLARE SUB handleScroll ()
DECLARE SUB handleSpawn ()
DECLARE SUB handleSprites ()
DECLARE SUB incSpawnIndex ()
DECLARE SUB loadLevel (fileName AS STRING)
DECLARE SUB loadSprites (file$)
DECLARE SUB loadTiles (file$)
DECLARE SUB loadSounds (file$)
DECLARE SUB myPrint (x%, y%, text$, colour1%, colour2%, overRideSeg%, overRideOfs%)
DECLARE SUB quit ()
DECLARE SUB switchAnim (objectIndex%, anim%)
DECLARE SUB spawnObject (x%, y%, object%, spawnerFlags%, spawner)
DECLARE SUB handleBehaviors ()
DECLARE SUB main ()
DECLARE FUNCTION tileRead (x%, y%)
DECLARE FUNCTION getSpawnSlot% ()

DEFINT A-Z

DIM SHARED scoreHit AS INTEGER
DIM SHARED scorePickup(10 TO 20) AS INTEGER
DIM SHARED scoreKill(1 TO 40) AS INTEGER

DIM SHARED behaviorType(0 TO 79) AS INTEGER

'DIM SHARED divtable(1 TO 40, 2 TO 10) AS INTEGER

SUB addScore (eventType)

 s = 0
 SELECT CASE eventType
  CASE 1
   s = 25
  CASE 8
   s = 50
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

 STATIC playerFlying

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
              

              IF object(n).control = 256 THEN
               IF kbArray(57) THEN main
              END IF

              IF object(n).control = 255 THEN

                c = tileRead(physics(n).x, physics(n).y)
                screenTargetY = screenTargetYlock

                IF c > 29 THEN
                  IF physics(n).speedX = 0 AND player.ShootDelay < 6 THEN switchAnim n, 5
                  speedMod = 10
                  friction = 5
                  physics(n).y = (physics(n).y AND &HFFF8)
                  screenTargetYlock = physics(n).y - 46
                  IF physics(n).speedY > 0 THEN
                   soundEvent = 16
                   IF c = 56 THEN damagePlayer
                  END IF
                  physics(n).speedY = 0
                  screenTargetY = screenTargetYlock
                  collisionFloor = 1
                 ELSE
                  maxY = 20
                  speedMod = 10
                  friction = 5
                  IF player.ShootDelay < 6 THEN switchAnim n, 10
                  'physics(n).speedY = 20
                  IF physics(n).speedY < 40 THEN
                    IF kbArray(29) THEN
                      'IF physics(n).speedY > 1 THEN physics(n).speedY = 5
                      physics(n).speedY = physics(n).speedY + 1
                      maxY = 10
                     ELSE
                      physics(n).speedY = physics(n).speedY + 10
                    END IF
                  END IF
                  a = physics(n).y - 46 - screenTargetYlock
                  IF a > 0 OR a < -32 THEN
                   screenTargetYlock = -1
                  END IF
                  collisionFloor = 0
                END IF

                c = tileRead(physics(n).x, physics(n).y - 8)
                IF c > 29 THEN
                 physics(n).y = (physics(n).y AND &HFFF8) + 8
                 physics(n).speedY = 0
                 IF c = 57 THEN damagePlayer
                END IF
                IF c = 28 THEN
                  addScore 998
                  soundEvent = 34
                  x = (physics(n).x) AND &HFFFC
                  y = (physics(n).y - 8) AND &HFFF8
                  tileWrite x, y, 0
                  aUpdateTile x, y
                  spawnObject x, y, 99, 0, 0
                END IF

                d = ABS(physics(n).speedX)
                p = 0
                IF physics(n).speedX > 0 THEN
                 p = -friction
                 ELSEIF physics(n).speedX < 0 THEN
                 p = friction
                END IF
                physics(n).speedX = physics(n).speedX + p
                IF d - ABS(physics(n).speedX) < 0 THEN physics(n).speedX = 0

                'IF object(n).jump AND collisionFloor AND kbArray(29) = 0 THEN
                '  object(n).jump = 0
                'END IF

                'IF object(n).jump = 1 AND collisionFloor = 0 THEN
                '  object(n).jump = 0
                '  physics(n).speedY = 1
                'END IF

                'playerJumpToggle = playerJumpToggle + (playerJumpToggle > 0)

                object(n).jump = object(n).jump + (object(n).jump > 0)

                IF kbArray(29) AND object(n).jump = 0 THEN
                 IF collisionFloor THEN
                     object(n).jump = 10
                     physics(n).speedY = -35
                     physics(n).y = physics(n).y - 2
                     soundEvent = 0
                 END IF
                END IF

                IF kbArray(75) THEN
                 physics(n).speedX = physics(n).speedX - speedMod
                 object(n).flags = object(n).flags OR flags.flip
                 IF collisionFloor THEN
                  switchAnim n, 40
                  IF fakeFloat(2, floatC) THEN sounds.queue(sounds.readpos) = 25 + dtAnims
                 END IF
                END IF
                IF kbArray(77) THEN
                 physics(n).speedX = physics(n).speedX + speedMod
                 object(n).flags = object(n).flags OR flags.flip XOR flags.flip
                 IF collisionFloor THEN
                  switchAnim n, 40
                  IF fakeFloat(2, floatC) THEN sounds.queue(sounds.readpos) = 25 + dtAnims
                 END IF
                END IF

                player.ShootDelay = player.ShootDelay + (player.ShootDelay > 0)
                IF kbArray(56) AND player.ShootDelay = 0 THEN
                 player.ShootDelay = 10
                 spawnObject physics(2).x, physics(2).y - 6, 100, object(n).flags AND flags.flip, 0
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
                  xs = angle.x(b) \ 2
                  ys = angle.y(b) \ 2
                  spawnObject physics(n).x, physics(n).y, 257, xs, ys
                 NEXT b
                 queueSound 4, 22
                 queueSound 14, 23
                 queueSound 24, 25
                 hudPrint 0, "          PRESS SPACE TO RESTART        ", &HC, &H5
                 player.hudLock = 999
                 player.score = 0
                 physics(2).x = 0
                 physics(2).y = 0
                END IF

                IF physics(n).speedX > 10 THEN physics(n).speedX = 10
                IF physics(n).speedX < -10 THEN physics(n).speedX = -10
                IF physics(n).speedY > maxY THEN physics(n).speedY = maxY
                IF physics(n).speedY < -30 THEN physics(n).speedY = -30

                screenX = gfx.screenX(gfx.page)
                screenY = gfx.screenY(gfx.page)
                
         END IF

 FOR getObject = 2 TO spawnIndex

 n = objectIndex(getObject)

  '-------------------------------------------------------------------------
  IF object(n).exist > 0 THEN

      visDeltaX = ABS(screenX + 20 - physics(n).x)
      visDeltaY = ABS(screenY + 24 - physics(n).y)

      IF visDeltaX < object(n).visX AND visDeltaY < object(n).visY THEN
       spdX = fakeFloat(physics(n).speedX, floatC)
       spdY = fakeFloat(physics(n).speedY, floatC)
       IF object(n).exist > 1 THEN
         physics(n).x = physics(object(n).exist).x + physics(n).childX
         physics(n).y = physics(object(n).exist).y + physics(n).childY
         physics(n).childX = physics(n).childX + fakeFloat(physics(n).speedX, floatC)
         physics(n).childY = physics(n).childY + fakeFloat(physics(n).speedY, floatC)
       ELSE
         physics(n).x = physics(n).x + fakeFloat(physics(n).speedX, floatC)
         physics(n).y = physics(n).y + fakeFloat(physics(n).speedY, floatC)
         IF n = 2 THEN

                c = tileRead(physics(n).x + 2, physics(n).y - 5)
                IF c > 29 THEN
                 physics(n).x = (physics(n).x AND &HFFFC) + 2
                 IF c = 59 THEN damagePlayer
                END IF

                c = tileRead(physics(n).x - 2, physics(n).y - 5)
                IF c > 29 THEN
                 physics(n).x = (physics(n).x AND &HFFFC) + 1
                 IF c = 58 THEN damagePlayer
                END IF

                screenTargetX = physics(n).x - 19
                IF screenTargetYlock = -1 THEN
                 screenTargetY = physics(n).y - 28
                END IF
                handleCamera
                screenX = gfx.screenX(gfx.page)
                screenY = gfx.screenY(gfx.page)

         ' screenX = screenX + fakeFloat(physics(n).speedX, floatC)
         END IF
       END IF

        IF physics(n).y < 32 THEN physics(n).y = 32
        IF physics(n).y > 630 THEN physics(n).y = 630
        IF physics(n).x < 2 THEN physics(n).x = 2
        IF physics(n).x > 76 THEN physics(n).x = 76
      
      '-------------------------------------------------------------------
      'Player Bullet
      IF object(n).control = 100 THEN
            c = tileRead(physics(n).x, physics(n).y)
            IF c > 29 THEN
             IF c = 38 THEN
              IF dtAnims = 1 THEN
                t = 28
               ELSE
                t = 0
              END IF
              addScore 999
              tileWrite physics(n).x, physics(n).y, t
              aUpdateTile physics(n).x AND &HFFFC, physics(n).y AND &HFFF8
              physics(n).x = physics(n).x AND &HFFFC
              physics(n).y = physics(n).y AND &HFFF8
              physics(n).speedX = 0
              physics(n).speedY = 2
              object(n).flags = 0
              object(n).control = 200
              object(n).life = 6
              object(n).flash = 20
              switchAnim n, 130
              spawnObject physics(n).x, physics(n).y, 99, 0, 0
             ELSE
              object(n).flags = 0
              object(n).control = 0
              physics(n).speedX = 0
              physics(n).collider = 0
              object(n).life = 7
              switchAnim n, 110
              physics(n).speedY = -5
             END IF
             soundEvent = 44
            END IF
            IF visDeltaX > 30 THEN object(n).life = 0
      END IF

      IF gfx.page > 0 AND object(n).control THEN

      playerDeltaX = ABS(physics(n).x - physics(2).x)
      playerDeltaY = ABS(physics(n).y - physics(2).y + 2)
      
      SELECT CASE object(n).control
       
       'Dropping particle
       CASE 200
            IF physics(n).speedY < 20 THEN physics(n).speedY = physics(n).speedY + 4
       
       'Pickup
       CASE 41
            c = tileRead(physics(n).x, physics(n).y)
            IF physics(n).speedY < 40 THEN physics(n).speedY = physics(n).speedY + 4
            
            IF c > 29 THEN
             IF physics(n).speedY > 9 THEN
               physics(n).y = physics(n).y AND &HFFF8
               physics(n).speedY = (NOT physics(n).speedY) \ 2
               soundEvent = 32
              ELSE
               physics(n).speedY = 0
             END IF
            END IF
            IF object(n).life < 37 AND object(n).anim <> 90 THEN object(n).flash = 36
            IF playerDeltaX < 5 AND playerDeltaY < 8 THEN
            soundEvent = 34
             IF object(n).anim = 90 THEN
               player.score = player.score + 500
               player.hasKey = 1
               object(n).exist = 2
               object(n).control = 0
               physics(n).speedX = 0
               physics(n).speedY = 0
               physics(n).childX = 0
               physics(n).childY = -11
              ELSE
               player.score = player.score + 25
               physics(n).y = physics(n).y - 3
               physics(n).speedY = 0
               object(n).flash = 0
               object(n).life = 9
               object(n).control = 0
               switchAnim n, 93
             END IF
            END IF

       'Enemy bullet
       CASE 102
         IF playerDeltaX < 4 AND playerDeltaY < 5 THEN
          damagePlayer
          object(n).life = 0
         END IF

       'Drip
       CASE 104
         IF object(n).animFrame = 179 AND physics(n).speedY < 20 THEN physics(n).speedY = physics(n).speedY + 2
         IF playerDeltaX < 3 AND playerDeltaY < 5 THEN
          damagePlayer
          object(n).flash = 16
          object(n).life = 3
          object(n).animFrame = 181
          object(n).control = 0
          physics(n).speedY = -10
         END IF

       'Fire Breath Fire
       CASE 111
         IF object(n).life = 4 THEN object(n).flash = 8
         IF playerDeltaX < 9 AND playerDeltaY + 4 < 8 THEN
          damagePlayer
         END IF
         sounds.queue(sounds.readpos) = 10 + floatC

        'Eggy
       CASE 1
           IF object(n).spawner = 2 THEN
             IF object(n).counter < 0 AND object(n).counter > -8 THEN
               physics(n).speedY = angle.y(object(n).jump)
               object(n).jump = (object(n).jump + 2) AND 63
               c = tileRead(physics(n).x, physics(n).y - 8)
               IF c > 29 THEN physics(n).speedY = 0: physics(n).y = physics(n).y AND &HFFF8 + 7
              ELSE
               object(n).jump = 0
             END IF
            ELSE
             IF object(n).counter < 0 AND object(n).counter > -5 THEN
               physics(n).speedY = angle.y(object(n).jump)
               object(n).jump = (object(n).jump + 4) AND 63
               c = tileRead(physics(n).x, physics(n).y - 8)
               IF c > 29 THEN physics(n).speedY = 0: physics(n).y = physics(n).y AND &HFFF8 + 7
              ELSE
               object(n).jump = 0
             END IF
           END IF

            s = object(n).flags > 0

            IF object(n).jump = 0 THEN
             c = tileRead(physics(n).x, physics(n).y)
             IF c < 30 THEN
               physics(n).speedY = 20
              ELSE
               physics(n).speedX = 0
               object(n).animFrame = 60
               physics(n).speedY = 0
               physics(n).y = physics(n).y AND &HFFF8
             END IF
             IF object(n).counter = 0 THEN
              object(n).animFrame = 62
              physics(n).speedX = 3 + s * 6
             END IF
            END IF

            object(n).counter = object(n).counter + (object(n).counter < 1)

            IF object(n).counter < -10 AND c > 29 THEN
             object(n).counter = 0
             object(n).jump = 0
             object(n).spawner = object(n).spawner + 1
             IF object(n).spawner = 3 THEN object(n).spawner = 0
            END IF

         IF object(n).flags THEN
           s = -2
           p = 1
          ELSE
           s = 2
           p = 3
         END IF
         c = tileRead(physics(n).x + s, physics(n).y - 3)
         IF c > 29 THEN
            object(n).flags = object(n).flags XOR 1
            physics(n).speedX = 0
            physics(n).x = (physics(n).x AND &HFFFC) + p
         END IF

       'Drooly
       CASE 4
        IF physics(n).speedX THEN
          'c = tileRead(physics(n).x + physics(n).speedX, physics(n).y + 1)
          'IF c < 30 THEN physics(n).speedX = physics(n).speedX * -1
          c = tileRead(physics(n).x + physics(n).speedX, physics(n).y - 2)
          IF c > 29 THEN physics(n).speedX = physics(n).speedX * -1
        END IF

        object(n).jump = object(n).jump + 1

        IF object(n).jump = 15 THEN
          object(n).counter = physics(n).speedX
          physics(n).speedX = 0
          spawnObject physics(n).x + 2, physics(n).y + 8, 104, 0, 0
        END IF

        IF object(n).jump = 25 THEN
         object(n).jump = 0
         physics(n).speedX = object(n).counter
        END IF

       'Key keeper
       CASE 5
         object(n).jump = object(n).jump + 1

         IF object(n).jump > 35 THEN
          object(n).jump = 0
          spawnObject physics(n).x, physics(n).y - 8, 102, 1, 0
          soundEvent = 50
         END IF

         physics(n).y = object(n).direction + angle.y(mainCounter1 AND 63) \ 2

         c = tileRead(physics(n).x + 2, physics(n).y - 8)
         IF c > 29 THEN physics(n).x = (physics(n).x AND &HFFFC): physics(n).speedX = -1
         c = tileRead(physics(n).x - 2, physics(n).y - 8)
         IF c > 29 THEN physics(n).x = (physics(n).x AND &HFFFC) + 2: physics(n).speedX = 1


       'FIRE BREATH
       CASE 11
         x = (mainCounter1 + physics(n).y) AND 31
         IF x > 10 THEN
          IF x = 11 THEN
           object(n).animFrame = 152: soundEvent = 66
          END IF
          IF x = 15 THEN
           spawnObject physics(n).x + object(n).direction, physics(n).y - 2, 111, object(n).direction, 0
          END IF
         ELSE
           object(n).animFrame = 150
         END IF

       'SPIKE BALL O
       CASE 18
        c = mainCounter1 AND 63
        x = physics(n).x
        y = physics(n).y
        FOR b = getObject + 1 TO getObject + 3
         nn = objectIndex(b)
         physics(nn).x = x + angle.x(c) * object(nn).jump \ 10
         physics(nn).y = y + angle.y(c) * object(nn).jump \ 10
        NEXT b


        'IF ABS(physics(nn).x - physics(2).x) < 5 AND ABS(physics(nn).y - physics(2).y + 6) < 6 THEN damagePlayer
        
        'Lock explode
        CASE 98
         object(n).counter = object(n).counter + 1
         IF object(n).counter = 3 THEN
          object(n).counter = 0
          c = tileRead(physics(n).x, physics(n).y)
          IF c < 40 THEN object(n).life = 0
          IF c > 39 THEN
           tileWrite physics(n).x, physics(n).y, 0
           aUpdateTile physics(n).x AND &HFFFC, physics(n).y AND &HFFF8
           spawnObject physics(n).x AND &HFFFC, physics(n).y AND &HFFF8, 99, 0, 0
           physics(n).x = physics(n).x + 4 * object(n).jump
           soundEvent = 44
          END IF
         END IF

        'CHECKPOINT
        CASE 62
          IF object(n).life < 3 AND object(n).life > 0 THEN
            player.wave = player.wave + 1
            handleSpawn
            object(n).life = 0
          END IF

          IF player.hasKey THEN
           IF playerDeltaX < 10 AND playerDeltaY < 20 THEN
            player.hasKey = 0
            FOR xx = 4 TO 60
             object(xx).life = 0
            NEXT xx

            object(n).life = 10

            spawnObject physics(n).x + 1, physics(n).y + 4 AND &HFFF8, 98, -1, 0
            spawnObject physics(n).x + 2, physics(n).y + 4 AND &HFFF8, 98, 1, 0
            spawnObject physics(n).x + 1, physics(n).y + 10 AND &HFFF8, 98, -1, 0
            spawnObject physics(n).x + 2, physics(n).y + 10 AND &HFFF8, 98, 1, 0
            spawnObject physics(n).x + 3, physics(n).y, 10, 88, 0
           END IF
          END IF

        'EXIT
        CASE 63
         IF (mainCounter1 AND 3) = 2 THEN
          x = physics(n).x - 4 + INT(RND * 10)'(randInt AND 7)
          y = physics(n).y - 22 + INT(RND * 15)' (randInt AND 15)
          spawnObject x, y, 93, 0, 0
         END IF

       END SELECT

      END IF 'Page check
        '-------------------------------------------------------------------

      parts = animation(object(n).animFrame).jump
      
      flipIndex = ABS((object(n).flags AND flags.flip) <> 0)

      flash = NOT object(n).flash AND 1
      object(n).flash = object(n).flash + (object(n).flash > 0)

      xx = physics(n).x - screenX + 20
      yy = physics(n).y - screenY + 20
   
      IF physics(n).collider THEN
       FOR animPart = object(n).animFrame TO object(n).animFrame + parts - 1
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
          ctype = physics(n).collider AND (animation(animPart).jump > 0)
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
         FOR animPart = object(n).animFrame TO object(n).animFrame + parts - 1
          spriteIndex = animation(animPart).index(flipIndex)
          x = xx + animation(animPart).x(flipIndex)
          y = yy + animation(animPart).y
      
          IF spriteIndex AND x > 12 AND x < 60 AND y > 5 AND y < 70 THEN
           spriteList(spriteListIndex, gfx.page).offset = spriteOffset(spriteIndex)
           spriteList(spriteListIndex, gfx.page).x = x '+ 20
           spriteList(spriteListIndex, gfx.page).y = y '+ 20
           spriteList(spriteListIndex, gfx.page).w = spriteWidth(spriteIndex)
           spriteList(spriteListIndex, gfx.page).h = spriteHeight(spriteIndex)
           spriteListIndex = spriteListIndex + 1
          END IF
        NEXT animPart
       END IF
      END IF
      
      IF dtAnims = 1 THEN
       object(n).animFrame = object(n).animFrame + parts
       IF animation(object(n).animFrame).jump < 0 THEN object(n).animFrame = object(n).animFrame + animation(object(n).animFrame).jump
      END IF
  END IF 'visibility check

   IF gfx.page THEN
    object(n).life = object(n).life + (object(n).life > 0)
    IF object(n).life = 0 THEN
     IF object(n).control > 0 AND object(n).control < 11 THEN
          object(n).exist = 1
          object(n).flags = 0
          object(n).control = 41
          object(n).flash = 4
          physics(n).speedX = 0
          physics(n).speedY = -20
          physics(n).y = physics(n).y - 2
          physics(n).collider = 0
          object(n).life = 113
          object(n).animFrame = 98
      ELSE
          destroyObject getObject
          getObject = getObject - 1
     END IF
    END IF
   END IF

  END IF 'exist check
 NEXT getObject

 IF dtAnims = 4 THEN mainCounter2 = (mainCounter2 + 1) AND 63
 IF gfx.page THEN mainCounter1 = (mainCounter1 + 1) AND 63

 dtAnims = dtAnims AND 3

 spriteList(spriteListIndex, gfx.page).y = 255

 IF soundEvent > -1 THEN queueSound 0, soundEvent

 IF player.score > scoreDelta THEN
  hudUpdate 4
 END IF

END SUB

SUB handleCollision

  soundEvent = -1

      'Collision lists
      ' 1 = Affect Player
      ' 2 = Affect Enemy
      ' 3 = Affect Bullet

 IF object(2).control = 255 AND object(2).flash = 0 THEN
  px0 = physics(2).x - gfx.screenX(gfx.page) + 20 - 2
  py0 = physics(2).y - gfx.screenY(gfx.page) + 20 - 8
  px1 = px0 + 3
  py1 = py0 + 5

  FOR a = 0 TO colCount(1)
   n = colList(1, a).index
   'IF collision(colList(1, 0).x0, colList(1, 0).y0, colList(1, 0).x1, colList(1, 0).y1, colList(1, a).x0, colList(1, a).y0, colList(1, a).x1, colList(1, a).y1) THEN
   IF collision(px0, py0, px1, py1, colList(1, a).x0, colList(1, a).y0, colList(1, a).x1, colList(1, a).y1) THEN
    damagePlayer
    a = 9999
   END IF
  NEXT a
 END IF
  FOR b = 0 TO colCount(2)
   n = colList(2, b).index
   FOR a = 0 TO colCount(1)
     IF collision(colList(1, a).x0, colList(1, a).y0, colList(1, a).x1, colList(1, a).y1, colList(2, b).x0, colList(2, b).y0, colList(2, b).x1, colList(2, b).y1) THEN
       n2 = colList(1, a).index
       physics(n).collider = 0
       object(n).life = 8
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
         physics(n).x = physics(n2).x
         object(n).animFrame = 195
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

SUB handleSpawn

 FOR n = 1 TO 16
  IF level.wave(player.wave).spawner(n).object AND (level.wave(player.wave).spawner(n).flags AND 1) = 0 THEN
    spawnObject level.wave(player.wave).spawner(n).x, level.wave(player.wave).spawner(n).y, level.wave(player.wave).spawner(n).object, level.wave(player.wave).spawner(n).flags, n
    level.wave(player.wave).spawner(n).flags = level.wave(player.wave).spawner(n).flags XOR &H1
   END IF
 NEXT n
 
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
  hudPrint 0, "FPS:      0000000              TIME:    ", &HE, &HC
 END IF

 IF flags AND 2 THEN
  cc = &HCE
   FOR n = 1 TO 4
    IF n > player.hp THEN cc = &H5
    hudPrint 18 + n * 2, CHR$(33) + CHR$(34), cc, cc
   NEXT n
 END IF

 IF flags AND 4 THEN
  a$ = STR$(player.score)
  c = LEN(a$)
  a$ = MID$(a$, 2, c)
  hudPrint 18 - c, a$, &HE, &HC
 END IF

 IF flags AND 8 THEN
  hudPrint 36, STR$(player.Time), &HF, &HE
 END IF

 IF flags AND 16 THEN
  hudPrint 4, STR$(INT(fps)), &HF, &HE
 END IF

END SUB

SUB initGame

elfInit

loadTiles "ducktile"

loadSounds "sound"

'FOR a = 1 TO 40
' FOR b = 2 TO 10
'  divtable(a, b) = a / b
' NEXT b
'NEXT a

player.level = 1

randomOffset = 0

main

END SUB

SUB main

 loadLevel "level" + MID$(STR$(player.level), 2, 1)

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
 player.Time = 300

 object(2).exist = 1
 object(2).control = 255
 object(2).life = -3
 physics(2).x = level.PlayerX
 physics(2).y = level.PlayerY
 physics(2).collider = 0

 object(2).animFrame = 10
 object(2).anim = 10

 object(2).visX = 25
 object(2).visY = 30

 objectIndex(2) = 2

 playerOldX = -999
 playerOldY = -999

 screenResetPos physics(2).x - 20, physics(2).y - 30

 updatetext = 1

 dtGame = TIMER

 frameSkip = 0
 skipFrame = 0

 handleSpawn

 hudPrint 0, "ARROW KEYS . CTRL . ALT . ESC . TO PLAY", &HF, &HE
 player.hudLock = 15
 
 DO
      
  IF TIMER > dtGame THEN

      'skipFrame = (TIMER - dtGame > .3) AND frameSkip
      'frameSkip = frameSkip XOR -1

      dtGame = TIMER + .3

      handleCollision

      handleBehaviors        'Create new sprite list

      playSounds

      handleScroll

      aSpriteList gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), spriteBankSeg, spriteBankOfs, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)
      
      aPageFlip gfx.videoWrap, VARSEG(hudBuffer), VARPTR(hudBuffer), gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
      gfx.page = gfx.page XOR 1

      'aCopyTileAreas gfx.page, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)
      aRectList gfx.WindowOfs(gfx.page), gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), tileBufferWrap, tileBufferSeg, tileBufferOfs, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)
      
      IF kbArray(1) THEN quit

    updatetext = updatetext + 1
    IF updatetext = 24 THEN
     player.Time = player.Time + (player.Time > 0)
     deltaTime                     'Not needed!
     updatetext = 0
     fps = 192 / dt
     hudUpdate 16
     hudUpdate 8
    ' LOCATE 1, 1: PRINT INT(fps); "   "
     'LOCATE 1, 14: PRINT gfx.tileOfs
    ' LOCATE 1, 22: PRINT gfx.pageOfs(1)
    ' LOCATE 2, 1: PRINT gfx.screenX(0)
    ' LOCATE 2, 5: PRINT gfx.windowY(0)
    ' LOCATE 2, 10: PRINT gfx.screenX(0)
    ' LOCATE 2, 15: PRINT gfx.windowY(1)
    END IF

   END IF
 LOOP

END SUB

FUNCTION randInt
 r = (PEEK(randomOffset) + 2)
 randomOffset = (r + randomOffset) AND 8191
 randInt = r
END FUNCTION

SUB spawnObject (x, y, object, spawnerFlags, spawner)
 
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
 physics(n).x = x
 physics(n).y = y
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
   object(n).exist = 1
   object(n).life = 3
   object(n).animFrame = 144
   object(n).anim = 144
   incSpawnIndex

  CASE 98
   'Destroy lock
   objectIndex(spawnIndex) = n
   object(n).control = 98
   object(n).exist = 1
   object(n).life = -2
   object(n).animFrame = 116
   object(n).anim = 116
   object(n).jump = spawnerFlags
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
   object(n).spawner = spawner
   object(n).life = 30
   physics(n).collider = 0
   object(n).animFrame = 175
   object(n).anim = 175
   object(n).visY = 30
   incSpawnIndex

  CASE 111
   'FIRE PARTICLE
   objectIndex(spawnIndex) = n
   object(n).control = 111
   object(n).exist = 1
   object(n).life = 16
   physics(n).collider = 0
   physics(n).x = x
   physics(n).y = y
   physics(n).speedY = 0'-2
   physics(n).speedX = 0'spawnerFlags + spawnerFlags
   object(n).animFrame = 185
   object(n).anim = 185
   incSpawnIndex
   
  CASE 1
   'Eggy
   objectIndex(spawnIndex) = n
   object(n).spawner = 0
   object(n).control = 1
   object(n).exist = 1
   object(n).life = -2
   physics(n).collider = 1
   object(n).animFrame = 60
   object(n).anim = 60
   object(n).visX = 40
   object(n).counter = 0
   incSpawnIndex

  CASE 4
   'DROOLY
   objectIndex(spawnIndex) = n
   object(n).control = 4
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -2
   physics(n).collider = 1
   physics(n).speedX = 2
   object(n).animFrame = 170
   object(n).anim = 170
   object(n).visY = 30
   incSpawnIndex

  CASE 5
   'Key Keeper
   objectIndex(spawnIndex) = n
   object(n).control = 5
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -3
   object(n).direction = y
   physics(n).collider = 1
   physics(n).speedX = 1
   object(n).animFrame = 64
   object(n).anim = 64
   object(n).visX = 30
   object(n).visY = 50
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
   d = 8 + ((11 - object) * 13)
   object(n).direction = d
   IF object = 12 THEN object(n).flags = flags.flip
   incSpawnIndex
   'FOR p = 0 TO 3  'Reserve 4 flame parts for fast spawning.
   ' n = getSpawnSlot
   ' objectIndex(spawnIndex) = n
   ' object(n).control = 111
   ' object(n).exist = -1
   ' object(n).life = -999
   ' physics(n).collider = 0
   ' physics(n).speedY = -2
   ' physics(n).speedX = d
   ' object(n).animFrame = 160
   ' object(n).anim = 160
   ' physics(n).x = 999
   ' physics(n).y = 999
   ' object(n).visX = defaultVisX
   ' object(n).visY = defaultVisY
   ' incSpawnIndex
   'NEXT p


  CASE 18
   'SPIKE BALL

    objectIndex(spawnIndex) = n
    object(n).control = 18
    object(n).exist = 1
    object(n).spawner = spawner
    object(n).life = -999
    object(n).animFrame = 0
    object(n).anim = 0
    object(n).visX = 50
    object(n).visY = 50
    incSpawnIndex
    n = getSpawnSlot
    
   FOR p = 2 TO 3
    objectIndex(spawnIndex) = n
    object(n).control = 0
    object(n).exist = 1
    object(n).spawner = spawner
    object(n).life = -999
    object(n).jump = 3 * p - 2
    object(n).animFrame = 148
    object(n).anim = 148
    object(n).visX = defaultVisX
    object(n).visY = defaultVisY
    incSpawnIndex
    n = getSpawnSlot
   NEXT p
    objectIndex(spawnIndex) = n
    physics(n).collider = 1
    object(n).control = 0
    object(n).exist = 1
    object(n).spawner = spawner
    object(n).life = -9999
    object(n).jump = 13
    object(n).animFrame = 146
    object(n).anim = 146
    object(n).visX = defaultVisX
    object(n).visY = defaultVisY
    incSpawnIndex

  CASE 41
   'GEM
   objectIndex(spawnIndex) = n
   object(n).control = 41
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = 113
   physics(n).speedY = -20
   object(n).animFrame = spawnerFlags
   object(n).anim = spawnerFlags
   incSpawnIndex
   
  CASE 62
   'Checkpoint
   objectIndex(spawnIndex) = n
   object(n).control = 62
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -3
   object(n).animFrame = 0
   object(n).anim = 0
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
  
  CASE 64
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

 END SELECT
 
END SUB


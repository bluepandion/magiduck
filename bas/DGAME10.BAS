DECLARE SUB hudPrint (x%, text$, colour1%, colour2%)
DECLARE SUB aDrawTiles (area%, forceOfs%)
DECLARE SUB aUpdateTile (x%, y%)
DECLARE SUB aCopyWindow (page%)
DECLARE SUB waitForRetrace ()
DECLARE SUB aCopyTileAreas (page%, listSeg%, listOfs%)
DECLARE SUB aUpdateVideo ()
DECLARE SUB aUpdateBuffer ()
DECLARE SUB queueSound (offset%, Frequency%)

DECLARE SUB playSounds ()
DECLARE SUB aSprite ()
DECLARE SUB handleCollision ()
DECLARE SUB debugWrite (message$)
DECLARE SUB screenResetPos (x%, y%)
'$INCLUDE: 'libelf.bi'

DECLARE SUB destroyObject (index%)
DECLARE SUB tileWrite (x AS INTEGER, y AS INTEGER, index AS INTEGER)
DECLARE SUB aPageFlip ()
DECLARE SUB deltaTime ()
DECLARE SUB elfInit ()
DECLARE SUB fakeFloats ()
DECLARE SUB handleAnims ()
DECLARE SUB handlecamera ()
DECLARE SUB handleKeyboard ()
DECLARE SUB handleScroll ()
DECLARE SUB handleSpawn ()
DECLARE SUB handleSprites ()
DECLARE SUB incSpawnIndex ()
DECLARE SUB loadLevel (fileName AS STRING)
DECLARE SUB loadSprites (file$)
DECLARE SUB loadTiles (file$)
DECLARE SUB myPrint (x%, y%, text$, colour1%, colour2%, overRideSeg%, overRideOfs%)
DECLARE SUB quit ()
DECLARE SUB saveLevel (fileName AS STRING)
DECLARE SUB switchAnim (objectIndex%, anim%)
DECLARE SUB spawnObject (x%, y%, object%, spawnerFlags%, spawner)
DECLARE SUB handleBehaviors ()
DECLARE SUB testControl ()
DECLARE FUNCTION tileRead (x%, y%)
DECLARE FUNCTION getSpawnSlot% ()



DEFINT A-Z

SUB handleBehaviors

 STATIC playerFlying

 spriteListIndex = 0
 
 dtAnims = dtAnims + 1

 floatC = (floatC + 1) MOD 10
 
 '--------------------------------------------------------------------------
  colCount(1) = -1
  colCount(2) = -1
  colCount(3) = -1
 
                n = objectIndex(2)

                IF physics(n).speedX > 20 THEN physics(n).speedX = 20
                IF physics(n).speedX < -20 THEN physics(n).speedX = -20
                IF physics(n).speedY > 10 THEN physics(n).speedY = 10
                IF physics(n).speedY < -20 THEN physics(n).speedY = -20

                c = tileRead(physics(n).x, physics(n).y)
                screenTargetY = screenTargetYlock

                IF c > 29 THEN
                  IF physics(n).speedX = 0 AND player.ShootDelay < 6 THEN switchAnim n, 5
                  screenTargetYlock = physics(n).y - 42
                  physics(n).y = (physics(n).y AND &HFFF8)
                  physics(n).speedY = 0
                  screenTargetY = screenTargetYlock
                  collisionFloor = 1
                 ELSE
                  IF player.ShootDelay < 6 THEN switchAnim n, 10
                  physics(n).speedY = 20
                  a = physics(n).y - 42 - screenTargetYlock
                  IF a > 0 OR a < -32 THEN
                   screenTargetYlock = -1
                   screenTargetY = physics(n).y - 28
                  END IF
                  collisionFloor = 0
                END IF

                c = tileRead(physics(n).x + 2, physics(n).y - 5)
                IF c > 29 THEN physics(n).x = (physics(n).x AND &HFFFC) + 1
                c = tileRead(physics(n).x - 2, physics(n).y - 5)
                IF c > 29 THEN physics(n).x = (physics(n).x AND &HFFFC) + 2
                c = tileRead(physics(n).x, physics(n).y - 8)
                IF c > 29 THEN physics(n).y = (physics(n).y AND &HFFF8) + 7

                physics(n).speedX = 0

                IF object(n).jump > 1 THEN
                  object(n).jump = object(n).jump - 1
                  physics(n).speedY = -20
                END IF

                IF object(n).jump = 1 AND collisionFloor = 0 THEN
                  object(n).jump = 0
                  physics(n).speedY = 1
                END IF

                playerJumpToggle = playerJumpToggle + (playerJumpToggle > 0)

                IF KBarray(29) THEN
                 IF playerJumpToggle < 8 THEN
                    IF playerFlying = 0 THEN screenTargetYlock = -1
                    playerFlying = 1
                    object(n).direction = 0
                     IF object(n).jump = 0 THEN
                      object(n).jump = 1
                      queueSound 0, 35
                      queueSound 1, 40
                      queueSound 2, 42
                     END IF
                    object(n).jump = object(n).jump + 2
                    playerJumpToggle = playerJumpToggle + 2
                 ELSE
                    IF object(n).jump < 2 THEN object(n).jump = 1
                    playerJumpToggle = 9
                 END IF
                ELSE
                 playerFlying = 0
                END IF

                IF KBarray(75) THEN
                 physics(n).speedX = -10
                 object(n).flags = object(n).flags OR flags.flip
                 IF collisionFloor THEN switchAnim n, 40
                END IF
                IF KBarray(77) THEN
                 physics(n).speedX = 10
                 object(n).flags = object(n).flags OR flags.flip XOR flags.flip
                 IF collisionFloor THEN switchAnim n, 40
                END IF

                player.ShootDelay = player.ShootDelay + (player.ShootDelay > 0)
                IF KBarray(56) AND player.ShootDelay = 0 THEN
                 player.ShootDelay = 10
                 spawnObject physics(2).x, physics(2).y - 6, 100, object(n).flags AND flags.flip, 0
                 queueSound 0, 40
                 queueSound 2, 42
                 switchAnim n, 30
                END IF
                screenTargetX = physics(n).x - 19
                handlecamera
                screenX = gfx.screenX(gfx.page)
                screenY = gfx.screenY(gfx.page)
 
 FOR getObject = 2 TO spawnIndex

 n = objectIndex(getObject)

  '-------------------------------------------------------------------------
  IF object(n).exist > 0 THEN
      
      IF ABS(physics(n).x - physics(2).x) < 60 AND ABS(physics(n).y - physics(2).y) < 50 THEN
       IF object(n).exist > 1 THEN
         physics(n).x = physics(object(n).exist).x + physics(n).childX
         physics(n).y = physics(object(n).exist).y + physics(n).childY
        ELSE
         physics(n).x = physics(n).x + fakeFloat(physics(n).speedX, floatC)
         physics(n).y = physics(n).y + fakeFloat(physics(n).speedY, floatC)
      END IF

        IF physics(n).y < 32 THEN physics(n).y = 32
        IF physics(n).y > 630 THEN physics(n).y = 630
        IF physics(n).x < 2 THEN physics(n).x = 2
        IF physics(n).x > 76 THEN physics(n).x = 76
        
      '-------------------------------------------------------------------
      IF gfx.page THEN
      SELECT CASE object(n).control

       CASE 10
            c = tileRead(physics(n).x, physics(n).y)
            IF physics(n).speedY < 20 THEN physics(n).speedY = physics(n).speedY + 3
            
            IF c > 29 THEN
             physics(n).y = physics(n).y AND &HFFF8
             IF physics(n).speedY > 9 THEN
               physics(n).speedY = (NOT physics(n).speedY) \ 2
               queueSound 0, 70
              ELSE
               physics(n).speedY = 0
             END IF
            END IF
            IF object(n).life < 37 AND object(n).anim <> 90 THEN object(n).flash = 36
            IF ABS(physics(2).x - physics(n).x) < 4 AND ABS(physics(2).y - physics(n).y) < 6 THEN
               queueSound 1, 40
               queueSound 2, 45
               queueSound 3, 50
               queueSound 4, 55
               queueSound 6, 72
               queueSound 8, 75
             IF object(n).anim = 90 THEN
               player.hasKey = 1
               object(n).exist = 2
               object(n).control = 0
               physics(n).childX = 0
               physics(n).childY = -11
              ELSE
               physics(n).y = physics(n).y - 3
               physics(n).speedY = 0
               object(n).flash = 0
               object(n).life = 9
               object(n).control = 0
               switchAnim n, 93
             END IF
            END IF

       CASE 102
         IF ABS(physics(2).x - physics(n).x) < 3 AND ABS(physics(2).y - 6 - physics(n).y) < 4 THEN
          object(2).flash = 36
         END IF
        
       CASE 100

            c = tileRead(physics(n).x, physics(n).y)
            IF c > 29 OR object(n).life = 0 THEN
             object(n).flags = 0
             object(n).control = 0
             physics(n).speedX = 0
             physics(n).collider = 0
             object(n).life = 7
             switchAnim n, 110
             queueSound 0, 20
             queueSound 1, 40
             queueSound 2, 15
             queueSound 3, 30
             queueSound 4, 12
             queueSound 5, 5
            END IF
        
        CASE 60 'Egg head
        
         physics(n).speedY = 0
         c = tileRead(physics(n).x, physics(n).y)
         IF c < 30 THEN physics(n).speedY = 10

         IF object(n).flags AND flags.flip THEN
           physics(n).speedX = -2
           c = tileRead(physics(n).x - 2, physics(n).y + 1)
           IF c < 30 THEN physics(n).speedX = 0: object(n).flags = object(n).flags XOR flags.flip
           c = tileRead(physics(n).x - 2, physics(n).y - 2)
           IF c > 29 THEN physics(n).speedX = 0: object(n).flags = object(n).flags XOR flags.flip
          ELSE
           physics(n).speedX = 2
           c = tileRead(physics(n).x + 2, physics(n).y + 1)
           IF c < 30 THEN physics(n).speedX = 0: object(n).flags = object(n).flags XOR flags.flip
           c = tileRead(physics(n).x + 2, physics(n).y - 2)
           IF c > 29 THEN physics(n).speedX = 0: object(n).flags = object(n).flags XOR flags.flip
         END IF

         IF object(n).life > -1 THEN
          object(n).flags = 0
          object(n).control = 10
          object(n).flash = 4
          physics(n).speedX = 0
          physics(n).speedY = -20
          physics(n).y = physics(n).y - 2

          physics(n).collider = 0
          object(n).life = 113
          switchAnim n, 98
         END IF

       CASE 65 'Key keeper
         object(n).Counter = (object(n).Counter + 1) AND 31
         object(n).jump = object(n).jump + 1

         IF object(n).jump > 35 THEN
          object(n).jump = 0
          spawnObject physics(n).x, physics(n).y - 8, 102, 1, 0
          queueSound 0, 50
          queueSound 1, 55
         END IF

         physics(n).speedY = angle.y(object(n).Counter)

         c = tileRead(physics(n).x, physics(n).y - 12)
         IF c > 29 THEN physics(n).y = (physics(n).y AND &HFFF8) + 3: object(n).Counter = 12
         c = tileRead(physics(n).x, physics(n).y)
         IF c > 29 THEN physics(n).y = physics(n).y AND &HFFF8: object(n).Counter = 0

         c = tileRead(physics(n).x + 2, physics(n).y - 8)
         IF c > 29 THEN physics(n).x = (physics(n).x AND &HFFFC): physics(n).speedX = NOT physics(n).speedX
         c = tileRead(physics(n).x - 2, physics(n).y - 8)
         IF c > 29 THEN physics(n).x = (physics(n).x AND &HFFFC) + 2: physics(n).speedX = NOT physics(n).speedX

         IF object(n).life > -1 THEN
          object(n).flags = 0
          object(n).control = 10
          object(n).flash = 4
          physics(n).speedX = 0
          physics(n).speedY = -10
          physics(n).y = physics(n).y - 2

          physics(n).collider = 0
          object(n).life = -1
          switchAnim n, 90
         END IF

        CASE 98 'Lock explode
         c = tileRead(physics(n).x, physics(n).y)
         IF c > 39 THEN
          IF c > 42 THEN object(n).life = 12
          tileWrite physics(n).x, physics(n).y, 0
          aUpdateTile physics(n).x AND &HFFFC, physics(n).y AND &HFFF8
          queueSound 0, 20
          queueSound 1, 40
          queueSound 2, 15
          queueSound 3, 30
         END IF
         IF physics(n).x < 4 OR physics(n).x > 74 THEN object(n).life = 0

        CASE 99 'Checkpoint
          IF object(n).life = 1 THEN player.wave = player.wave + 1: handleSpawn
          IF player.hasKey THEN
           IF ABS(physics(2).x - physics(n).x) < 10 AND ABS(physics(2).y - physics(n).y) < 20 THEN
            player.hasKey = 0
            FOR xx = 4 TO spawnIndex
             IF object(xx).control <> 1 OR object(xx).control <> 10 THEN object(xx).life = 0
            NEXT xx
            object(n).life = 10
            spawnObject physics(n).x + 1, physics(n).y + 4 AND &HFFF8, 98, -3, 0
            spawnObject physics(n).x + 2, physics(n).y + 4 AND &HFFF8, 98, 3, 0
            spawnObject physics(n).x + 1, physics(n).y + 10 AND &HFFF8, 98, -1, 0
            spawnObject physics(n).x + 2, physics(n).y + 10 AND &HFFF8, 98, 1, 0
            spawnObject physics(n).x + 3, physics(n).y, 10, 88, 0
           END IF
          END IF
       END SELECT


        object(n).life = object(n).life + (object(n).life > 0)
        IF object(n).life = 0 THEN destroyObject n
       END IF
        '-------------------------------------------------------------------

      parts = animation(object(n).animFrame).jump
      
      flipIndex = ABS((object(n).flags AND flags.flip) <> 0)

      flash = NOT object(n).flash AND 1
      object(n).flash = object(n).flash + (object(n).flash > 0)
   
      FOR animParts = 1 TO parts
        spriteIndex = animation(object(n).animFrame).index(flipIndex)
        x = physics(n).x + animation(object(n).animFrame).x(flipIndex)
        y = physics(n).y + animation(object(n).animFrame).y
        sx = x - screenX
        sy = y - screenY
   
        IF spriteIndex AND sx > -5 AND sx < 40 AND sy > -5 AND sy < 50 THEN
         IF flash THEN
          spritelist(spriteListIndex, gfx.page).offset = spriteOffset(spriteIndex)
          spritelist(spriteListIndex, gfx.page).x = sx + 20
          spritelist(spriteListIndex, gfx.page).y = sy + 20
          spritelist(spriteListIndex, gfx.page).w = spriteWidth(spriteIndex)
          spritelist(spriteListIndex, gfx.page).h = spriteHeight(spriteIndex)
          spriteListIndex = spriteListIndex + 1
         END IF
         ctype = physics(n).collider AND (animation(object(n).animFrame).jump > 0)
           IF ctype THEN
            colCount(ctype) = colCount(ctype) + 1
            colList(ctype, colCount(ctype)).index = n
            colList(ctype, colCount(ctype)).x0 = x
            colList(ctype, colCount(ctype)).y0 = y
            colList(ctype, colCount(ctype)).x1 = x + spriteWidth(spriteIndex) - 1
            colList(ctype, colCount(ctype)).y1 = y + spriteHeight(spriteIndex) - 1
           END IF
        END IF

       object(n).animFrame = object(n).animFrame + 1
      NEXT animParts
      object(n).animFrame = object(n).animFrame + parts * (dtAnims < 4)
      IF animation(object(n).animFrame).jump < 0 THEN object(n).animFrame = object(n).anim
   END IF
 
  END IF
 NEXT getObject

 dtAnims = dtAnims AND 3

 spritelist(spriteListIndex, gfx.page).y = 255
 'gfx.listLen(gfx.page) = spriteListIndex

END SUB

SUB handleSpawn

' dtSpawn = dtSpawn + 1
' IF dtSpawn > 20 THEN
'  dtSpawn = 0
'   FOR n = 4 TO spawnIndex
'    IF object(n).exist = 1 THEN
'     IF ABS(physics(2).x - physics(n).x) > 160 THEN
'       destroyObject n
'     END IF
'    END IF
'   NEXT n
' END IF

'diffx = ABS(playerOldX - physics(2).x)

'IF diffx > 20 THEN
' playerOldX = physics(2).x
 
 FOR n = 1 TO 16
  IF level.wave(player.wave).spawner(n).object AND (level.wave(player.wave).spawner(n).flags AND 1) = 0 THEN
    spawnObject level.wave(player.wave).spawner(n).x, level.wave(player.wave).spawner(n).y, level.wave(player.wave).spawner(n).object, level.wave(player.wave).spawner(n).flags, n
    level.wave(player.wave).spawner(n).flags = level.wave(player.wave).spawner(n).flags XOR &H1
   END IF
 NEXT n
 
'END IF

END SUB

SUB initGame

elfInit

screenX = 8
screenY = 8
screenoldx = 8
screenoldy = 8

spawnIndex = 4

loadLevel "level0"

testControl

END SUB

SUB spawnObject (x, y, object, spawnerFlags, spawner)
 
 n = getSpawnSlot
 
 object(n).spawner = spawner
 object(n).flash = 0
 object(n).Counter = 0
 physics(n).x = x
 physics(n).y = y
 
 SELECT CASE object

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

  CASE 98
   'Destroy lock
   objectIndex(spawnIndex) = n
   object(n).control = 98
   object(n).exist = 1
   object(n).life = -2
   physics(n).collider = 0
   object(n).animFrame = 116
   object(n).anim = 116
   physics(n).speedX = spawnerFlags
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
   physics(n).speedX = 20 + (spawnerFlags <> 0) * 40
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
   object(n).flags = 0
   physics(n).collider = 0
   physics(n).speedX = 10 + (spawnerFlags <> 0) * 20
   physics(n).speedY = 5
   object(n).animFrame = 102
   object(n).anim = 102
   incSpawnIndex
   
  CASE 1
   'Egg Walk
   objectIndex(spawnIndex) = n
   object(n).control = 60
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -3
   physics(n).collider = 1
   object(n).animFrame = 60
   object(n).anim = 60
   incSpawnIndex

  CASE 5
   'Key Keeper
   objectIndex(spawnIndex) = n
   object(n).control = 65
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -3
   physics(n).collider = 1
   physics(n).x = x
   physics(n).y = y
   physics(n).speedX = 1
   object(n).animFrame = 64
   object(n).anim = 64
   incSpawnIndex

  CASE 10
   'GEM
   objectIndex(spawnIndex) = n
   object(n).control = 10
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = 113
   physics(n).collider = 0
   physics(n).x = x
   physics(n).y = y
   physics(n).speedY = -20
   object(n).animFrame = spawnerFlags
   object(n).anim = spawnerFlags
   incSpawnIndex

  CASE 62
   'Checkpoint
   objectIndex(spawnIndex) = n
   object(n).control = 99
   object(n).exist = 1
   object(n).spawner = spawner
   object(n).life = -3
   physics(n).collider = 0
   physics(n).x = x
   physics(n).y = y
   object(n).animFrame = 0
   object(n).anim = 0
   incSpawnIndex

   'p = n
   'n = getSpawnSlot
   'objectIndex(spawnIndex) = n
   'object(n).exist = p
   'incSpawnIndex
 END SELECT
 
END SUB

SUB testControl
 
 object(2).exist = 1
 object(2).control = 1
 object(2).life = -3
 physics(2).x = level.PlayerX
 physics(2).y = level.PlayerY
 physics(2).collider = 0

 object(2).animFrame = 10
 object(2).anim = 10

 objectIndex(2) = 2

 playerOldX = -999
 playerOldY = -999

 screenResetPos 0, 520

 updatetext = 1

 dtGame = TIMER

 frameskip = 0
 skipframe = 0

 handleSpawn

 hudPrint 0, "FPS:     00000000  LEVEL: 1 TIME: 000", &HE, &HC

 DO
      
  IF TIMER > dtGame THEN

      dtGame = TIMER + .1

      handleCollision

      handleBehaviors        'Create new sprite list

      handleScroll

      aSprite               'Draw new sprites
      
      playSounds

      aPageFlip

      aCopyTileAreas gfx.page, VARSEG(spritelist(0, gfx.page).offset), VARPTR(spritelist(0, gfx.page).offset)

      IF KBarray(1) THEN quit

    updatetext = updatetext + 1
    IF updatetext = 10 THEN
     deltaTime                     'Not needed!
     updatetext = 0
     fps = 80 / dt
     hudPrint 4, STR$(INT(fps)), &HF, &HE
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


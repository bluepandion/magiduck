DECLARE SUB addActor (actor%, wave%, X%, Y%)
DECLARE SUB exitCode (code%)


'$INCLUDE: 'libelf.bi'

TYPE actorConv
 name AS STRING * 6
 id AS INTEGER
 xOfs AS INTEGER
 yOfs AS INTEGER
 flags AS INTEGER
END TYPE

DIM SHARED actorConvert(1 TO 160) AS actorConv

DIM strGet AS STRING * 1

tokenLayer$ = "layer"
tokenData$ = "data encoding"

CLS
PRINT COMMAND$

listFile$ = "lvlconv.txt" 'COMMAND$

OPEN listFile$ FOR BINARY AS #1
 PRINT LOF(1)
 IF LOF(1) < 1 THEN
  PRINT "File does not exist."
  CLOSE #1
  END
 END IF
CLOSE #1

listMode = 0
readToggle = 1

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
     INPUT #1, actorConvert(n).name
     INPUT #1, actorConvert(n).id
     INPUT #1, actorConvert(n).xOfs
     INPUT #1, actorConvert(n).yOfs
     INPUT #1, actorConvert(n).flags
    END IF
   LOOP WHILE n
   listMode = 0
   readToggle = 0
  END IF

'-------------------------------------------------------------------------
  IF LCASE$(a$) = "maps" THEN
   INPUT #1, a$
   IF a$ <> "/maps" THEN
   inFile$ = a$
   INPUT #1, level.tileSet
   INPUT #1, level.spriteSet
   INPUT #1, fileOut$
   token$ = ""
   tokenMode = 0
   levelStr$ = ""
   tileSet = 0
   actorWave = 0
   actorSpawner = 1
   X = 0
   Y = 0
'-------------------------------------------------------------------------
   PRINT "Converting level "; file$
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
        levelStr$ = levelStr$ + CHR$(tile)
       END IF
       IF tokenMode = 4 THEN
        tile = VAL(token$)
        IF tile > 0 THEN

         IF tile = 1 THEN
          level.playerX = X
          level.playerY = Y + 7
          PRINT "   Player start"
         END IF

         IF tile > 1 AND tile < 9 THEN
          actorWave = tile - 2
          actorSpawner = 1
         END IF

         IF tile > 11 AND tile < 19 THEN
          nWave = tile - 12
          'IF nWave = actorWave THEN
          '  nWave = nWave + 1
          'END IF
          addActor tile, actorWave, X, Y
         END IF

         IF tile > 19 THEN
          addActor tile, actorWave, X, Y
         END IF
        END IF
        X = X + 4
        IF X > 79 THEN X = 0: Y = Y + 8
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
    OPEN fileOut$ + ".lvl" FOR BINARY AS #2
     PUT #2, 1, levelStr$
     PUT #2, 1601, level
    CLOSE #2
    PRINT " Level converted and saved to "; fileOut$ + ".lvl"
   ELSE
   readToggle = 0
   listMode = 0
   END IF
  END IF
'-------------------------------------------------------------------------
 LOOP WHILE NOT EOF(1)
CLOSE #1

SUB addActor (actor, wave, X, Y)
 
 IF actorConvert(actor).id > 0 THEN
  FOR spawner = 1 TO 16
   IF level.wave(wave).spawner(spawner).object = 0 THEN
    COLOR 7
    level.wave(wave).spawner(spawner).object = actorConvert(actor).id
    level.wave(wave).spawner(spawner).X = actorConvert(actor).xOfs + X
    level.wave(wave).spawner(spawner).Y = actorConvert(actor).yOfs + Y
    level.wave(wave).spawner(spawner).flags = actorConvert(actor).flags
    PRINT "   Wave:"; wave; " Spawner:"; spawner, "Actor: "; actor,
    PRINT actorConvert(actor).name, X; ","; Y
    spawner = 999: EXIT SUB
   END IF
  NEXT spawner
   COLOR 12
   PRINT "     Too many actors per wave "; wave
   COLOR 7
  ELSE
   COLOR 4
   PRINT "     Not found: "; actor
   COLOR 7
 END IF

END SUB

SUB exitCode (code)
 SELECT CASE code
  CASE 1
   PRINT "Too many actors per wave."
 END SELECT
 END
END SUB


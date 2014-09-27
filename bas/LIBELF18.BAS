DECLARE SUB aClearList (clearWord%, listSeg%, listOfs%)
DECLARE SUB syncPages ()
DECLARE SUB aTilePan (direction%)
DECLARE SUB setErrorMsg (msg$)
DECLARE SUB aDrawTiles (area%, forceOfs%)
DECLARE SUB aCopyTileAreas (page%, listSeg%, listOfs%)
DECLARE SUB aScroll ()
DECLARE SUB queueSound (offset%, Note%)
DECLARE FUNCTION collision% (x0%, y0%, w0%, h0%, x1%, y1%, w1%, h1%)
DECLARE SUB debugWrite (message$)
DECLARE SUB loadLevel (fileName AS STRING)
DECLARE SUB testControl ()
DECLARE SUB handleKeyboard ()
DECLARE SUB myPrint (x%, y%, text$, colour1%, colour2%, overRideSeg%, overRideOfs%)
DECLARE SUB aPageFlip ()
DECLARE SUB animEditor ()
DECLARE SUB leveleditor ()
DECLARE SUB aKBinit ()
DECLARE SUB deltaTime ()
DECLARE SUB elfInit ()
DECLARE SUB loadSprites (file$)
DECLARE SUB loadTiles (file$)
DECLARE SUB spawnObject (x%, y%, object%, tags%, spawner%)
DECLARE SUB aKBremove ()
DECLARE SUB aSprite ()
DECLARE FUNCTION tileRead% (x%, y%)
DECLARE SUB destroyObject (index%)
DECLARE FUNCTION getSpawnSlot% ()
DECLARE SUB initLookups ()
DECLARE SUB InitScreen ()
DECLARE SUB initTimer ()
DECLARE SUB loadAssembler ()
DECLARE SUB quit ()
DEFINT A-Z

DECLARE SUB drawTileMapArea (x%, y%, w%, h%)
'$INCLUDE: 'libelf.bi'

'-------------------------------------------------------------------------
' Variables and types
'-------------------------------------------------------------------------

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

DIM SHARED loadstr AS STRING * 1            'General purpose file read
DIM SHARED loadint AS INTEGER               ' variables

DIM SHARED asmKBinit AS STRING
DIM SHARED asmKBroutine AS STRING
DIM SHARED asmKBremove AS STRING

DIM SHARED asmPageFlip AS STRING

DIM SHARED asmTile AS STRING                'Assembler routine storage
DIM SHARED asmCopyTileArea AS STRING
DIM SHARED asmSpriteList AS STRING
DIM SHARED asmTilePan AS STRING
DIM SHARED asmClearList AS STRING
DIM SHARED asmUpdateTile AS STRING

DIM SHARED KBoldseg AS INTEGER               'KB state save.
DIM SHARED KBoldofs AS INTEGER
DIM SHARED Keyflags AS INTEGER

'----------------------------------------------------------------------------
' Graphics storage and screen buffer related
'----------------------------------------------------------------------------
'Sprite graphics bank
DIM SHARED spriteBank AS STRING * 6400
DIM SHARED spriteBankSeg AS INTEGER
DIM SHARED spriteBankOfs AS INTEGER

DIM SHARED tilemapReadX AS INTEGER              '0-7 scroll offset
DIM SHARED tilemapReadY AS INTEGER              '0-3 scroll offset
DIM SHARED tileMapReadOfs AS INTEGER            'Tilemap read offset

DIM SHARED tileBuffer AS STRING * 11360         '7760 + 1600 + 2000
DIM SHARED tileBufferSeg AS INTEGER
DIM SHARED tileBufferOfs AS INTEGER
DIM SHARED tileBankOfs AS INTEGER
DIM SHARED tileMapOfs AS INTEGER
DIM SHARED tileBufferWrap AS INTEGER

DIM SHARED hudBuffer AS STRING * 160

TYPE charPair
 glyph1(32 TO 90) AS STRING * 1
 glyph2(32 TO 90) AS STRING * 1
END TYPE
DIM SHARED w40char AS charPair

'---------------------------------------------------------------------------
' Object related
'---------------------------------------------------------------------------

DIM SHARED tilePosXlookUp(-8 TO 87) AS INTEGER
DIM SHARED tilePosYlookUp(-16 TO 655) AS INTEGER

DIM SHARED spriteclipx(-20 TO 100) AS INTEGER
DIM SHARED spriteclipy(-10 TO 30) AS INTEGER
DIM SHARED spriteclipofsx(-20 TO 100) AS INTEGER

DIM SHARED spriteclipofsy(-10 TO 30) AS INTEGER

TYPE soundType
 Queue(0 TO 63) AS INTEGER
 readPos AS INTEGER
 speakerOn AS INTEGER
 speakerOff AS INTEGER
END TYPE
DIM SHARED sounds AS soundType

DIM SHARED soundFreqTable(1 TO 96, 2) AS INTEGER

DIM SHARED errorMessage$

SUB aClearList (clearWord, listSeg, listOfs)
 
 page = gfx.page

 p0% = gfx.videoWrap
 p1% = &HB800
 p2% = gfx.pageOfs(page) * 2
 p3% = clearWord
 p4% = listSeg
 p5% = listOfs

 DEF SEG = SSEG(asmClearList)
 CALL absolute(BYVAL p0%, BYVAL p1%, BYVAL p2%, BYVAL p3%, BYVAL p4%, BYVAL p5%, SADD(asmClearList))
 DEF SEG

END SUB

SUB aCopyTileAreas (page, listSeg, listOfs)

 p8% = gfx.WindowOfs(page)

 p0% = gfx.videoWrap
 p1% = &HB800
 p2% = gfx.pageOfs(page) * 2
 p3% = tileBufferWrap
 p4% = tileBufferSeg
 p5% = tileBufferOfs
 p6% = listSeg
 p7% = listOfs
 
 DEF SEG = SSEG(asmCopyTileArea)
 CALL absolute(BYVAL p8%, BYVAL p0%, BYVAL p1%, BYVAL p2%, BYVAL p3%, BYVAL p4%, BYVAL p5%, BYVAL p6%, BYVAL p7%, SADD(asmCopyTileArea))
 DEF SEG

END SUB

SUB aDrawTiles (area, forceOfs)
 'EXIT SUB
 
 p0% = forceOfs  'gfx.tileOfs

 IF p0% < 0 THEN p0% = 0
 IF p0% > 1540 THEN p0% = 1540

 p1% = area AND 3
 p2% = tileBufferSeg
 p3% = tileBufferOfs
 p4% = tileMapOfs
 p5% = tileBankOfs

 DEF SEG = SSEG(asmTile)
 CALL absolute(BYVAL p0%, BYVAL p1%, BYVAL p2%, BYVAL p3%, BYVAL p4%, BYVAL p5%, SADD(asmTile))
 DEF SEG

END SUB

SUB aKBinit

 DEF SEG = 0
 Keyflags = PEEK(&H417) AND &H70       'Save keyboard flags
 DEF SEG

 p2% = SSEG(asmKBroutine)
 p3% = SADD(asmKBroutine)

 DEF SEG = SSEG(asmKBinit)
 CALL absolute(KBoldseg, KBoldofs, BYVAL p2%, BYVAL p3%, SADD(asmKBinit))

 DEF SEG

END SUB

SUB aKBremove
 
p0% = KBoldseg
p1% = KBoldofs

DEF SEG = SSEG(asmKBremove)
CALL absolute(BYVAL p0%, BYVAL p1%, SADD(asmKBremove))
DEF SEG

DEF SEG = 0
POKE (&H417), Keyflags           'Restore old keyboard flags
DEF SEG

END SUB

SUB aPageFlip
 
 p0% = gfx.videoWrap
 p1% = VARSEG(hudBuffer)
 p2% = VARPTR(hudBuffer)
 p3% = gfx.pageOfs(gfx.page XOR 1)
 p4% = gfx.pageOfs(gfx.page)


 DEF SEG = SSEG(asmPageFlip)
 CALL absolute(BYVAL p0%, BYVAL p1%, BYVAL p2%, BYVAL p3%, BYVAL p4%, SADD(asmPageFlip))
 DEF SEG
 
 gfx.page = gfx.page XOR 1

END SUB

SUB aSprite

p0% = gfx.videoWrap

p1% = &HB800
p2% = gfx.pageOfs(gfx.page) * 2

p3% = spriteBankSeg
p4% = spriteBankOfs

p5% = VARSEG(spriteList(0, gfx.page).offset)
p6% = VARPTR(spriteList(0, gfx.page).offset)

DEF SEG = SSEG(asmSpriteList)
CALL absolute(BYVAL p0%, BYVAL p1%, BYVAL p2%, BYVAL p3%, BYVAL p4%, BYVAL p5%, BYVAL p6%, SADD(asmSpriteList))
DEF SEG

END SUB

SUB aTilePan (direction)

 p0% = direction
 p1% = tileBufferSeg
 p2% = tileBufferOfs

 DEF SEG = SSEG(asmTilePan)
 CALL absolute(BYVAL p0%, BYVAL p1%, BYVAL p2%, SADD(asmTilePan))
 DEF SEG

END SUB

SUB aUpdateTile (x, y)

 p2% = tilePosXlookUp(x) + tilePosYlookUp(y)

 IF p2% < 0 THEN p2% = 0
 IF p2% > 1599 THEN p2% = 1599

 p0% = (tilePosYlookUp(y) - gfx.tileOfs) / 20

 IF p0% > 11 THEN p0% = 11

 p1% = tilePosXlookUp(x)

 p3% = tileBufferSeg
 p4% = tileBufferOfs
 p5% = tileMapOfs
 p6% = tileBankOfs
 
 DEF SEG = SSEG(asmUpdateTile)
 CALL absolute(BYVAL p0%, BYVAL p1%, BYVAL p2%, BYVAL p3%, BYVAL p4%, BYVAL p5%, BYVAL p6%, SADD(asmUpdateTile))
 DEF SEG

' FOR n = 0 TO 1
'  xx = (X AND &HFFFC) - gfx.screenX(n)
'  yy = (y AND &HFFF8) - gfx.screenY(n) '+ 4
'  IF xx > 36 THEN xx = 36
'  IF yy > 42 THEN yy = 42
'  tileCopyList(0, n).X = xx
'  tileCopyList(0, n).y = yy
'  tileCopyList(0, n).w = 4
'  tileCopyList(0, n).h = 8
'  tileCopyList(1, n).y = 255
'  aCopyTileAreas n, VARSEG(tileCopyList(0, n).offset), VARPTR(tileCopyList(0, n).offset)
' NEXT n
 
END SUB

FUNCTION collision (x0%, y0%, x1%, y1%, x2%, y2%, x3%, y3%)

  a = (x0% > x3%) OR (x1% < x2%) OR (y0% > y3%) OR (y1% < y2%)
  collision = NOT a

END FUNCTION

SUB debugWrite (message$)
 IF debugLog = 1 THEN
  PRINT #2, message$
 END IF
END SUB

SUB deltaTime
 dt = TIMER - dtCompare
 dtCompare = TIMER
END SUB

SUB destroyObject (index)
 n = index

 FOR a = 1 TO spawnIndex
  IF objectIndex(a) = n THEN index = a: a = 999
 NEXT a
 
 object(n).exist = 0
 physics(n).collider = 0
 object(n).anim = 0
 object(n).animFrame = 0
 object(n).flags = 0
 physics(n).x = 0
 physics(n).y = 0
 physics(n).speedX = 0
 physics(n).speedY = 0

 IF object(n).spawner THEN level.Wave(0).spawner(object(n).spawner).flags = 0
 
 spawnIndex = spawnIndex - 1

 FOR a = index TO 59
   objectIndex(a) = objectIndex(a + 1)
 NEXT a

 objectIndex(60) = 0

END SUB

SUB elfInit
CLS
COLOR 11, 1
LOCATE 1, 1: PRINT "Magiduck Startup"
COLOR 7, 0
PRINT "Init."

'-----------------------------------------------------------------------
' Setup and initialize
'-----------------------------------------------------------------------

flags.flip = &H1
flags.animFinished = &H2
flags.flash = &H4
flags.jump = &H100

spriteBankSeg = VARSEG(spriteBank)
spriteBankOfs = VARPTR(spriteBank)

tileBufferSeg = VARSEG(tileBuffer)             'Tilemap render buffer
tileBufferOfs = VARPTR(tileBuffer)             '7680 bytes (80x96)
                                               '+ 80 bytes for safety
plong& = tileBufferOfs
plong& = plong& + 7760
a$ = MKL$(plong&)
tileBankOfs = CVI(a$)                          'Tile bank   = 2000 bytes
plong& = plong& - 40
a$ = MKL$(plong&)
tileBufferWrap = CVI(a$)

plong& = tileBufferOfs
plong& = plong& + 9760
a$ = MKL$(plong&)
tileMapOfs = CVI(a$)                           'Tile map    = 1600  bytes
                                               '              30x40 tiles
plong& = tileBufferOfs
plong& = plong& + 40                          'Offset tilebuffer by 40
a$ = MKL$(plong&)                              'for some safety
tileBufferOfs = CVI(a$)

'------------------------------------------------------
gfx.videoWrap = 16383
gfx.pageWrap = 8191

gfx.WindowOfs(0) = 0
gfx.WindowOfs(1) = 0
gfx.pageOfs(0) = 1000
gfx.pageOfs(1) = 5000
gfx.page = 0

FOR n = 0 TO 1
 gfx.screenX(n) = 0
 gfx.screenY(n) = 0
 gfx.screenOldX(n) = 0
 gfx.screenOldY(n) = 0
 gfx.windowX(n) = 0
 gfx.WindowY(n) = 24
 gfx.WindowOfs(n) = 1920
 spriteList(2, n).y = 255
NEXT n

'------------------------------------------------------

soundReadPos = 0

sounds.speakerOn = INP(&H61) OR 3
sounds.speakerOff = sounds.speakerOn XOR 3

'------------------------------------------------------

RANDOMIZE TIMER
initTimer

PRINT "Lookups..."
initLookups
PRINT "Binaries..."
loadAssembler
PRINT "Tiles..."
loadTiles "duck1"
PRINT "Sprites..."
loadSprites "ducks2"

PRINT "Video..."

'DEF SEG = tileBufferSeg
'FOR a& = tileBufferOfs TO 0& + tileBufferOfs + 7679
' POKE a&, &H19
'NEXT a&
'DEF SEG

InitScreen

gfx.page = 0
gfx.pageOfs(0) = 0
FOR n = 0 TO 3
 tileCopyList(0, gfx.page).x = 0
 tileCopyList(0, gfx.page).y = 0
 tileCopyList(0, gfx.page).w = 40
 tileCopyList(0, gfx.page).h = 50
 tileCopyList(1, gfx.page).y = 255
 aClearList &HDE, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)
 gfx.pageOfs(0) = gfx.pageOfs(0) + 2000
NEXT n

DEF SEG = &HB800
FOR a& = 16000 TO 16382 STEP 2
 POKE a& + 1, 0
 POKE a&, 222
NEXT a&
DEF SEG

'-----------------------------------------------------------------------
' Preload data.
'-----------------------------------------------------------------------

aKBinit

debugLog = 0

IF debugLog THEN OPEN "duck.log" FOR OUTPUT AS #2

END SUB

FUNCTION getSpawnSlot

 FOR n = 4 TO 60
  IF object(n).exist = 0 THEN getSpawnSlot = n: EXIT FUNCTION
 NEXT n

 setErrorMsg "Too many objects."
 quit

END FUNCTION

SUB handlecamera
' n = 2

' IF KBarray(80) THEN
'  screenTargetY = physics(n).y + 1
' END IF

' IF KBarray(72) THEN
'  screenTargetY = physics(n).y - 20
' END IF

FOR n = 0 TO 1
 
 diffX = screenTargetX - gfx.screenX(n)
 diffY = screenTargetY - gfx.screenY(n)
 
 IF diffX THEN
  speedX = 1
  IF ABS(diffX) > 1 THEN speedX = 2
  IF ABS(diffX) > 17 THEN speedX = 3
  IF ABS(diffX) > 23 THEN speedX = 4
  IF diffX < 0 THEN speedX = speedX * -1
  gfx.screenX(n) = gfx.screenX(n) + speedX
  IF gfx.screenX(n) < 0 THEN gfx.screenX(n) = 0
  IF gfx.screenX(n) > 40 THEN gfx.screenX(n) = 40
 END IF

 IF diffY THEN
  speedY = 1
  IF ABS(diffY) > 4 THEN speedY = 2
  IF ABS(diffY) > 11 THEN speedY = 3
  IF ABS(diffY) > 19 THEN speedY = 4
  IF diffY < 0 THEN speedY = speedY * -1
  gfx.screenY(n) = gfx.screenY(n) + speedY
  IF gfx.screenY(n) < 4 THEN gfx.screenY(n) = 4
  IF gfx.screenY(n) > 580 THEN gfx.screenY(n) = 580
 END IF
      
NEXT n

END SUB

SUB handleCollision

      'Collision lists
      ' 1 = Affect Player
      ' 2 = Affect Enemy
      ' 3 = Affect Bullet

  FOR a = 0 TO colCount(1)
   n = colList(1, a).index
   'IF collision(colList(1, 0).x0, colList(1, 0).y0, colList(1, 0).x1, colList(1, 0).y1, colList(1, a).x0, colList(1, a).y0, colList(1, a).x1, colList(1, a).y1) THEN
   IF collision(physics(2).x - 2, physics(2).y - 8, physics(2).x + 2, physics(2).y - 2, colList(1, a).x0, colList(1, a).y0, colList(1, a).x1, colList(1, a).y1) THEN
    'object(2).animFrame = 10
    IF object(2).flash = 0 THEN
     object(2).flash = 40
     queueSound 0, 48
     queueSound 1, 49
    END IF
    a = 9999
   END IF
  NEXT a

  FOR b = 0 TO colCount(2)
   n = colList(2, b).index
   FOR a = 0 TO colCount(1)
     IF collision(colList(1, a).x0, colList(1, a).y0, colList(1, a).x1, colList(1, a).y1, colList(2, b).x0, colList(2, b).y0, colList(2, b).x1, colList(2, b).y1) THEN
       n2 = colList(1, a).index
       object(n).life = 0
       object(n2).flash = 40
       object(n2).life = object(n2).life + 1
       physics(n2).x = physics(n2).x + physics(n).speedX \ 10
       queueSound 0, 38
       queueSound 1, 34
      END IF
   NEXT a
  NEXT b

END SUB

SUB handleKeyboard
 
 plustime = 0
 IF TIMER > dtKeys THEN dtKeys = TIMER + .2: plustime = 1

  FOR n = 0 TO 127

   IF kbDelta(n) > KBdelay(n) THEN
     kbDelta(n) = 9999
     KBavail(n) = 1 AND kbArray(n)
     IF kbArray(n) THEN kbDelta(n) = 0
    ELSE
     kbDelta(n) = kbDelta(n) + plustime
     KBavail(n) = 0
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
  gfx.WindowOfs(gfx.page) = gfx.WindowOfs(gfx.page) + diffX
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
  gfx.WindowY(gfx.page) = gfx.WindowY(gfx.page) + diffY
  gfx.WindowOfs(gfx.page) = gfx.WindowOfs(gfx.page) + diffY * 80
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
 'IF gfx.windowOfs(gfx.page) > 7679 THEN gfx.windowOfs(gfx.page) = gfx.windowOfs(gfx.page) - 7680
 'IF gfx.windowOfs(gfx.page) < 0 THEN gfx.windowOfs(gfx.page) = gfx.windowOfs(gfx.page) + 7680
 
 IF gfx.WindowY(gfx.page) < 0 THEN
   gfx.WindowY(0) = gfx.WindowY(0) + 24
   gfx.WindowY(1) = gfx.WindowY(1) + 24
   gfx.WindowOfs(0) = gfx.WindowOfs(0) + 1920
   gfx.WindowOfs(1) = gfx.WindowOfs(1) + 1920
   gfx.tileOfs = gfx.tileOfs - 60 'tilePosYlookUp(gfx.screenY(gfx.page)) - 60
   'gfx.tileArea = gfx.tileArea - 1
   'gfx.tileArea = gfx.tileArea AND 3
   aTilePan 1
   aDrawTiles gfx.tileArea, gfx.tileOfs
 END IF

 IF gfx.WindowY(gfx.page) > 47 THEN
   gfx.WindowY(0) = gfx.WindowY(0) - 24
   gfx.WindowY(1) = gfx.WindowY(1) - 24

   gfx.WindowOfs(0) = gfx.WindowOfs(0) - 1920
   gfx.WindowOfs(1) = gfx.WindowOfs(1) - 1920
   
   gfx.tileOfs = gfx.tileOfs + 60 ' tilePosYlookUp(gfx.screenY(gfx.page)) - 60
   'gfx.tileArea = gfx.tileArea + 1
   'gfx.tileArea = gfx.tileArea AND 3
   aTilePan 0
   aDrawTiles gfx.tileArea + 3, gfx.tileOfs + 180
 END IF

 tileCopyList(n, gfx.page).y = 255
 
 aCopyTileAreas gfx.page, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)

END SUB

SUB hudPrint (x, text$, colour1, colour2)
 
 DEF SEG = VARSEG(hudBuffer)
 writeOfs = 0& + VARPTR(hudBuffer) + x * 2
 writeOfs2 = 80& + VARPTR(hudBuffer) + x * 2

 FOR n = 1 TO LEN(text$)
  l = ASC(MID$(text$, n, 1))

  POKE writeOfs, ASC(w40char.glyph1(l))
  POKE writeOfs2, ASC(w40char.glyph2(l))

  writeOfs = writeOfs + 1
  writeOfs2 = writeOfs2 + 1

  POKE writeOfs, colour1
  POKE writeOfs2, colour2

  writeOfs = writeOfs + 1
  writeOfs2 = writeOfs2 + 1
 NEXT n

 DEF SEG

END SUB

SUB incSpawnIndex
 spawnIndex = spawnIndex + 1
 IF spawnIndex > 60 THEN spawnIndex = 60
END SUB

SUB initLookups

OPEN "w40char.def" FOR BINARY AS #1
 GET #1, , w40char
CLOSE #1

OPEN "sincos.def" FOR BINARY AS #1
 GET #1, 1, angle
CLOSE #1

OPEN "floats.def" FOR INPUT AS #1
FOR ff = 0 TO 9
 FOR n = 0 TO 9
  INPUT #1, v
  FOR f = 0 TO 3
   fakeFloat(f * 10 + ff, n) = f + v
   fakeFloat(f * -10 - ff, n) = (f + v) * -1
  NEXT f
 NEXT n
NEXT ff
CLOSE #1

OPEN "freq.def" FOR INPUT AS #1
FOR n = 1 TO 12
  FOR o = 0 TO 7
  nn = o * 12 + n
  INPUT #1, v
  freq = 1193180# / v
  soundFreqTable(nn, 0) = freq AND 255
  soundFreqTable(nn, 1) = freq / 256
 NEXT o
NEXT n
CLOSE #1

c = 0
x = 0
FOR n = -8 TO 87
 tilePosXlookUp(n) = x
 IF n > -1 AND n < 79 THEN c = c + 1
 IF c = 4 THEN c = 0: x = x + 1
NEXT n

c = 0
y = 0
FOR n = -16 TO 655
 tilePosYlookUp(n) = y
 IF n > -1 AND n < 639 THEN c = c + 1
 IF c = 8 THEN c = 0: y = y + 20
NEXT n

FOR n = -20 TO 100
 spriteclipx(n) = 0
 spriteclipofsx(n) = 0
 IF n < 0 THEN spriteclipx(n) = ABS(n): spriteclipofsx(n) = ABS(n) * 2
 IF n > 79 THEN spriteclipx(n) = ABS(80 - n)
NEXT n

FOR n = -10 TO 30
 spriteclipy(n) = 0
 spriteclipofsy(n) = 0
 IF n < 1 THEN spriteclipy(n) = ABS(n - 1): spriteclipofsy(n) = ABS(n - 1) * 160
 IF n > 24 THEN spriteclipy(n) = ABS(24 - n)
NEXT n

END SUB

SUB InitScreen

 gfx.adapter = 1
 gfx.glyphheight = 3
 gfx.rows = 50

 'Disable video
 OUT &H3D8, 1

 SCREEN 0
 WIDTH 40, 25
 CLS

 'Disable blink
 IF gfx.adapter <> 1 THEN
   b = INP(&H3DA)
   'b = INP(&H3C1)
   OUT &H3C0, &H30
   b = INP(&H3C1)
   b = b AND &HF7
   OUT &H3C0, b
 END IF
 
 OUT &H3D4, &H9
 OUT &H3D5, gfx.glyphheight

 OUT &H3D4, &H4
 OUT &H3D5, gfx.rows

 OUT &H3D4, &H6
 OUT &H3D5, gfx.rows

 OUT &H3D4, &H7
 OUT &H3D5, gfx.rows

'Enable video, Bit 0 = 0, for low resolution text mode.
'              Bit 3 = 1, to enable video.
'              Bit 5 = 1, to enable blink.

 OUT &H3D8, &H8   '&H8 = 40 column, &H9 = 80 column

END SUB

SUB initTimer

 '-------------------------------------------------------------------
 'Main Timer (timer 0)

 ticksPerTimer = 1193181# / 145.6#     'Use a multiple of 18.2

 OUT &H43, &H34               'Specifies bitmap setting the timer style
                              ' and initiates ports to receive desired
                              ' clock trigger rate

 OUT &H40, ticksPerTimer AND 255    'Send low byte
 OUT &H40, ticksPerTimer / 256      'Send high byte

 '----------------------------------------------------------------------
 'Sound Timer (timer 2)


END SUB

SUB loadAssembler


OPEN "kbrou02.com" FOR BINARY AS #1          'Keyboard interrupt routine
 asmKBroutine = SPACE$(LOF(1))
 GET #1, , asmKBroutine
CLOSE #1

'Write KB array segment and offset into the KB routine.
MID$(asmKBroutine, 5, 2) = MKI$(VARSEG(kbArray(0)))      'Segment
MID$(asmKBroutine, 10, 2) = MKI$(VARPTR(kbArray(0)))    'Offset

END SUB

SUB loadLevel (fileName AS STRING)

   OPEN fileName + ".lvl" FOR BINARY AS #1
    
    FOR n = 1 TO 1600
     GET #1, n, loadstr
     MID$(tileBuffer, 9760 + n, 1) = loadstr
    NEXT n

    readOfs = 1601
    GET #1, readOfs, level
   CLOSE #1

   player.Wave = 0

END SUB

SUB loadSprites (file$)

'Sprite bank load

 OPEN file$ + ".csg" FOR BINARY AS #1
  GET #1, 1, spriteBank
 CLOSE #1

 OPEN file$ + ".def" FOR INPUT AS #1
  INPUT #1, a$

' Sprite data
   spriteOffset(0) = spriteBankOfs
   spriteWidth(0) = 0
   spriteHeight(0) = 0
   FOR n = 1 TO 127
    spriteOffset(n) = spriteBankOfs
    spriteWidth(n) = 2
    spriteHeight(n) = 1
   NEXT n

   DO
    INPUT #1, name$
    IF name$ <> "#anims" THEN
     INPUT #1, index

     INPUT #1, x
     INPUT #1, y
     
     IF index > 199 THEN index = index - 140: y = y + 40

     plong& = spriteBankOfs
     plong& = plong& + x + (y * 80)
     a$ = MKL$(plong&)
     spriteOffset(index) = CVI(a$)

     INPUT #1, spriteWidth(index)
     spriteWidth(index) = spriteWidth(index) / 2
     INPUT #1, spriteHeight(index)

     'IF index = 6 THEN
     ' PRINT name$
     ' PRINT x; y
     ' PRINT spriteWidth(index); spriteHeight(index)
     ' DO: LOOP WHILE INKEY$ = ""
     'END IF
    END IF
   LOOP WHILE name$ <> "#anims"

 
' Animation data

   n = 0
   DO
   INPUT #1, name$
   IF name$ <> "#end" THEN
    INPUT #1, n
    INPUT #1, parts
    DO
       INPUT #1, a$
       INPUT #1, animation(n).index(0)
       INPUT #1, animation(n).x(0)
       INPUT #1, animation(n).y
       animation(n).x(0) = animation(n).x(0) / 2
       INPUT #1, animation(n).jump
       animation(n).index(1) = animation(n).index(0) + 60
       animation(n).x(1) = animation(n).x(0) * -1 - spriteWidth(animation(n).index(0))
       n = n + 1
    LOOP WHILE animation(n - 1).jump > -1
   END IF
   LOOP WHILE name$ <> "#end"

 CLOSE #1

END SUB

SUB loadTiles (file$)

 DEF SEG = tileBufferSeg
 
 BLOAD file$ + ".ctg", tileBankOfs

 DEF SEG

 EXIT SUB

 OPEN file$ + ".ctg" FOR BINARY AS #1
  IF LOF(1) < 2000 THEN quit

  FOR a = 1 TO 2000
   GET #1, a, loadstr
   MID$(tileBuffer, 7760 + a, 1) = loadstr
  NEXT a
 CLOSE #1
 
 DEF SEG

END SUB

SUB myAttrib (x%, y%, lenght%, colour%)

 EXIT SUB

 DEF SEG = screenBufferSeg

 writeOfs = screenBufferOfs + x * 2 + y * 160 + 1
 
 IF x + a > 79 THEN a = a + (80 - (x + a))

 FOR n = 1 TO lenght%
  POKE writeOfs, colour
  writeOfs = writeOfs + 2
 NEXT n

 DEF SEG

END SUB

SUB myPrint (x, y, text$, colour1, colour2, overRideSeg, overRideOfs)
 IF x < 0 THEN EXIT SUB

 slen = LEN(text$)
 sOfs = 1
 IF x + slen > 39 THEN slen = slen + (40 - (x + slen))
 
 IF slen < 1 THEN EXIT SUB
 IF sOfs > slen THEN EXIT SUB

 IF overRideSeg = 0 THEN
  DEF SEG = &HB800
  writeOfs = gfx.pageOfs(gfx.page) * 2 + x * 2 + y * 80
 ELSE
  DEF SEG = overRideSeg
  writeOfs = overRideOfs + x * 2 + y * 80
 END IF

 writeOfs2 = writeOfs + 80

 FOR n = sOfs TO slen
  writeOfs = writeOfs AND gfx.videoWrap
  writeOfs2 = writeOfs2 AND gfx.videoWrap
  l = ASC(MID$(text$, n, 1))
  POKE writeOfs, ASC(w40char.glyph1(l))
  POKE writeOfs + 1, colour1
  POKE writeOfs2, ASC(w40char.glyph2(l))
  POKE writeOfs2 + 1, colour2
  writeOfs = writeOfs + 2
  writeOfs2 = writeOfs2 + 2
 NEXT n

 DEF SEG

END SUB

SUB playSounds

 IF sounds.Queue(sounds.readPos) THEN
   OUT &H43, &HB6     'Tell timer 2 we want to change countdown value

   'Send countdown value
   OUT &H42, soundFreqTable(sounds.Queue(sounds.readPos), 0)'Send low byte
   OUT &H42, soundFreqTable(sounds.Queue(sounds.readPos), 1)'Send high byte
  
   'Connect speaker to Timer 2
   OUT &H61, sounds.speakerOn

   sounds.Queue(sounds.readPos) = 0
  ELSE
  'Disconnect speaker
   OUT &H61, sounds.speakerOff
 END IF

 sounds.readPos = (sounds.readPos + 1) AND 63

END SUB

SUB queueSound (offset%, Note%)

 writePos = (offset% + sounds.readPos) AND 63
 sounds.Queue(writePos) = Note%

END SUB

SUB quit

 IF debugLog = 1 THEN CLOSE #2

 aKBremove

 OUT &H43, &H34         'Return Timer to normal operation
 OUT &H40, 0            'Set low 8 bits of trigger to 0
 OUT &H40, 0            'Set hi  8 bits of trigger to 0

 'Disconnect speaker from Timer 2
 OUT &H61, sounds.speakerOff
 
 SCREEN 0, 1, 0, 0
 WIDTH 80, 25

 SHELL "cls"
 PRINT errorMessage$
 PRINT FRE(-1)
 PRINT
 PRINT gfx.pageOfs(0); gfx.pageOfs(1)
 PRINT gfx.WindowOfs(0); gfx.WindowOfs(1)
 PRINT gfx.screenX(0); gfx.screenY(0); screenTargetX; screenTargetY
 PRINT "fps:"; fps
 FOR n = 0 TO 50
  IF spriteList(n, gfx.page).y = 255 THEN PRINT "sprites:"; n: n = 999
 NEXT n

 PRINT gfx.listLen(0)
 PRINT gfx.listLen(1)

 PRINT
 FOR n = 4 TO 30
  PRINT object(n).control; physics(n).x; physics(n).y; "|";
 NEXT n
END

END SUB

SUB saveLevel (fileName AS STRING)

   OPEN fileName + ".lvl" FOR OUTPUT AS #1
    PRINT #1, MID$(tileBuffer, 9761, 1600)
    PRINT #1, SPACE$(254)
   CLOSE #1

   OPEN fileName + ".lvl" FOR BINARY AS #1
    writeOfs = 1601
    PUT #1, writeOfs, level
   CLOSE #1

END SUB

DEFSNG A-Z
SUB screenResetPos (x%, y%)

 screenTargetX = x%
 screenTargetY = y%

 FOR n = 0 TO 1
  gfx.screenX(n) = x%
  gfx.screenY(n) = y%
  gfx.screenOldX(n) = x%
  gfx.screenOldY(n) = y%
  gfx.windowX(n) = x%
  gfx.WindowY(n) = 24
  gfx.WindowOfs(n) = 1920 + x%
  spriteList(0, n).y = 255
 NEXT n

 gfx.tileOfs = tilePosYlookUp(y%) - 60
 gfx.tileArea = 0

 aDrawTiles 0, gfx.tileOfs
 aDrawTiles 1, gfx.tileOfs + 60
 aDrawTiles 2, gfx.tileOfs + 120
 aDrawTiles 3, gfx.tileOfs + 180

 spriteList(1, gfx.page).y = 255
 spriteList(0, gfx.page).x = 0
 spriteList(0, gfx.page).y = 0
 spriteList(0, gfx.page).w = 40
 spriteList(0, gfx.page).h = 50
 aCopyTileAreas gfx.page, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)

 aPageFlip

 spriteList(1, gfx.page).y = 255
 spriteList(0, gfx.page).x = 0
 spriteList(0, gfx.page).y = 0
 spriteList(0, gfx.page).w = 40
 spriteList(0, gfx.page).h = 50
 aCopyTileAreas gfx.page, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)

END SUB

DEFINT A-Z
SUB setErrorMsg (msg$)
 errorMessage$ = msg$
END SUB

SUB switchAnim (objectIndex, anim)

 IF object(objectIndex).anim = anim THEN EXIT SUB

 object(objectIndex).animFrame = anim
 object(objectIndex).anim = anim
 
END SUB

FUNCTION tileRead (x, y)

 DEF SEG = tileBufferSeg

 readOfs = 0& + tileMapOfs + tilePosXlookUp(x) + tilePosYlookUp(y)
 
 tileRead = PEEK(readOfs)
 
 DEF SEG

END FUNCTION

DEFSNG A-Z
SUB tileWrite (x AS INTEGER, y AS INTEGER, index AS INTEGER)

    DEF SEG = tileBufferSeg

    writeOfs = 0& + tileMapOfs + tilePosXlookUp(x) + tilePosYlookUp(y)

    POKE writeOfs, index

    DEF SEG

END SUB


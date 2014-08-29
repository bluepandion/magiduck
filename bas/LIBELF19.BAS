DECLARE SUB handleCamera ()
DECLARE SUB handleScroll ()
DECLARE SUB damagePlayer ()
DECLARE SUB hudPrint (x%, text$, colour1%, colour2%)
DECLARE SUB syncPages ()
DECLARE SUB setErrorMsg (msg$)
DECLARE SUB aDrawTiles (area%, forceOfs%)
DECLARE SUB queueSound (offset%, position%)
DECLARE SUB playSounds ()
DECLARE FUNCTION collision% (x0%, y0%, w0%, h0%, x1%, y1%, w1%, h1%)
DECLARE SUB debugWrite (message$)
DECLARE SUB loadLevel (fileName AS STRING)
DECLARE SUB testControl ()
DECLARE SUB handleKeyboard ()
DECLARE SUB myPrint (x%, y%, text$, colour1%, colour2%, overRideSeg%, overRideOfs%)
DECLARE SUB deltaTime ()
DECLARE SUB elfInit ()
DECLARE SUB loadSprites (file$)
DECLARE SUB loadTiles (file$)
DECLARE SUB loadSounds (file$)
DECLARE SUB spawnObject (x%, y%, object%, tags%, spawner%)
DECLARE SUB aSprite ()
DECLARE FUNCTION tileRead% (x%, y%)
DECLARE SUB destroyObject (index%)
DECLARE FUNCTION getSpawnSlot% ()
DECLARE SUB initLookUps ()
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

DIM SHARED KBoldSeg AS INTEGER               'KB state save.
DIM SHARED KBoldOfs AS INTEGER
DIM SHARED Keyflags AS INTEGER

'----------------------------------------------------------------------------
' Graphics storage and screen buffer related
'----------------------------------------------------------------------------
'Sprite graphics bank
DIM SHARED spriteBank AS STRING * 6400

DIM SHARED tilemapReadX AS INTEGER              '0-7 scroll offset
DIM SHARED tilemapReadY AS INTEGER              '0-3 scroll offset
DIM SHARED tileMapReadOfs AS INTEGER            'Tilemap read offset

DIM SHARED tileBuffer AS STRING * 11360         '7760 + 1600 + 2000

TYPE charPair
 glyph1(32 TO 90) AS STRING * 1
 glyph2(32 TO 90) AS STRING * 1
END TYPE
DIM SHARED w40char AS charPair

'---------------------------------------------------------------------------
' Object related
'---------------------------------------------------------------------------

DIM SHARED tilePosXlookUp(-16 TO 95) AS INTEGER
DIM SHARED tilePosYlookUp(-16 TO 655) AS INTEGER

DIM SHARED soundFreqTable(1 TO 96, 2) AS INTEGER

DIM SHARED errorMessage$

SUB aDrawTiles (area, forceOfs)
 'EXIT SUB
 
 p0% = forceOfs  'gfx.tileOfs

 IF p0% < 0 THEN p0% = 0
 IF p0% > 1540 THEN p0% = 1540

 'p1% = area AND 3
 'p2% = tileBufferSeg
 'p3% = tileBufferOfs
 'p4% = TileMapOfs
 'p5% = tileBankOfs

 'DEF SEG = SSEG(asmTile)
 'CALL absolute(BYVAL p0%, BYVAL p1%, BYVAL p2%, BYVAL p3%, BYVAL p4%, BYVAL p5%, SADD(asmTile))
 'DEF SEG

 aTileArea p0%, area AND 3, tileBufferSeg, tileBufferOfs, tileMapOfs, tileBankOfs

END SUB

SUB aUpdateTile (x, y)
 
 p2% = tilePosXlookUp(x) + tilePosYlookUp(y)

 IF p2% < 0 THEN p2% = 0
 IF p2% > 1599 THEN p2% = 1599

 p0% = (tilePosYlookUp(y) - gfx.tileOfs) / 20

 IF p0% > 11 THEN p0% = 11

 aTileDraw p0%, tilePosXlookUp(x), p2%, tileBufferSeg, tileBufferOfs, tileMapOfs, tileBankOfs

 EXIT SUB

 tileCopylist(1, 0).y = 255
 tileCopylist(0, 0).w = 4
 tileCopylist(0, 0).h = 8

 FOR p = 0 TO 1
  tileCopylist(0, 0).x = x - gfx.screenOldX(p)
  tileCopylist(0, 0).y = y - gfx.screenOldY(p)

  IF tileCopylist(0, 0).x < 0 THEN tileCopylist(0, 0).x = 0
  IF tileCopylist(0, 0).y < 0 THEN tileCopylist(0, 0).y = 0
  IF tileCopylist(0, 0).x > 36 THEN tileCopylist(0, 0).x = 36
  IF tileCopylist(0, 0).y > 42 THEN tileCopylist(0, 0).y = 42
  
  aRectList gfx.WindowOfs(p), gfx.videoWrap, &HB800, gfx.pageOfs(p), tileBufferWrap, tileBufferSeg, tileBufferOfs, VARSEG(tileCopylist(0, 0).offset), VARPTR(tileCopylist(0, 0).offset)
 NEXT p
 
END SUB

FUNCTION collision (x0%, y0%, x1%, y1%, x2%, y2%, x3%, y3%)

  a = (x0% > x3%) OR (x1% < x2%) OR (y0% > y3%) OR (y1% < y2%)
  collision = NOT a

END FUNCTION

SUB deltaTime
 dt = TIMER - dtCompare
 dtCompare = TIMER
END SUB

SUB destroyObject (index)

 n = objectIndex(index)

 'FOR a = 1 TO spawnIndex
 ' IF objectIndex(a) = n THEN index = a: a = 999
 'NEXT a
 
 object(n).exist = 0
 physics(n).collider = 0
 object(n).anim = 0
 object(n).animFrame = 0
 object(n).flags = 0
 physics(n).x = 0
 physics(n).y = 0
 physics(n).speedX = 0
 physics(n).speedY = 0

 'IF object(n).spawner THEN level.Wave(0).spawner(object(n).spawner).flags = 0
 
 spawnIndex = spawnIndex - 1

 FOR a = index TO 59
   objectIndex(a) = objectIndex(a + 1)
 NEXT a

 'objectIndex(60) = 0

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

sounds.ReadPos = 0

sounds.speakerOn = INP(&H61) OR 3
sounds.speakerOff = sounds.speakerOn XOR 3

'------------------------------------------------------

RANDOMIZE TIMER
initTimer

PRINT "Lookups..."
initLookUps
PRINT "Binaries..."
loadAssembler
PRINT "Sprites..."
loadSprites "ducks2"

'DEF SEG = tileBufferSeg
'FOR a& = tileBufferOfs TO 0& + tileBufferOfs + 7679
' POKE a&, &H19
'NEXT a&
'DEF SEG

InitScreen

gfx.page = 0
gfx.pageOfs(0) = 0
FOR n = 0 TO 3
 tileCopylist(0, gfx.page).x = 0
 tileCopylist(0, gfx.page).y = 0
 tileCopylist(0, gfx.page).w = 40
 tileCopylist(0, gfx.page).h = 50
 tileCopylist(1, gfx.page).y = 255
 
 aClearList gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), &HDE, VARSEG(tileCopylist(0, gfx.page).offset), VARPTR(tileCopylist(0, gfx.page).offset)
 
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

 DEF SEG = 0
 Keyflags = PEEK(&H417) AND &H70       'Save keyboard flags
 DEF SEG

 p2% = SSEG(asmKBroutine)
 p3% = SADD(asmKBroutine)

 aKBinit KBoldSeg, KBoldOfs, p2%, p3%

END SUB

FUNCTION getSpawnSlot

 FOR n = 4 TO 60
  IF object(n).exist = 0 THEN getSpawnSlot = n: EXIT FUNCTION
 NEXT n

 setErrorMsg "Too many objects."
 quit

END FUNCTION

SUB handleCamera
' n = 2

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
  IF gfx.screenY(n) > 590 THEN gfx.screenY(n) = 590
 END IF
      
NEXT n

END SUB

SUB handleKeyboard
 
 plustime = 0
 IF TIMER > dtKeys THEN dtKeys = TIMER + .1: plustime = 1

  FOR n = 0 TO 127

   IF kbDelta(n) > KBdelay(n) THEN
     kbDelta(n) = 9999
     KBavail(n) = 1 AND KBarray(n)
     IF KBarray(n) THEN kbDelta(n) = 0
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
      tileCopylist(n, gfx.page).x = 40 - diffX
      tileCopylist(n, gfx.page).y = 0
      tileCopylist(n, gfx.page).w = diffX
      tileCopylist(n, gfx.page).h = 50
      n = n + 1
    END IF

   IF diffX < 0 THEN
      tileCopylist(n, gfx.page).x = 0
      tileCopylist(n, gfx.page).y = 0
      tileCopylist(n, gfx.page).w = ABS(diffX)
      tileCopylist(n, gfx.page).h = 50
      n = n + 1
   END IF
 END IF

 IF diffY THEN
  gfx.screenOldY(gfx.page) = gfx.screenY(gfx.page)
  gfx.WindowY(gfx.page) = gfx.WindowY(gfx.page) + diffY
  gfx.WindowOfs(gfx.page) = gfx.WindowOfs(gfx.page) + diffY * 80
  gfx.pageOfs(gfx.page) = gfx.pageOfs(gfx.page) + diffY * 40
   IF diffY > 0 THEN
      tileCopylist(n, gfx.page).x = 0
      tileCopylist(n, gfx.page).y = 50 - diffY - 1
      tileCopylist(n, gfx.page).w = 40
      tileCopylist(n, gfx.page).h = diffY + 1
      n = n + 1
     ELSE
      tileCopylist(n, gfx.page).x = 0
      tileCopylist(n, gfx.page).y = 0
      tileCopylist(n, gfx.page).w = 40
      tileCopylist(n, gfx.page).h = ABS(diffY) + 2
      n = n + 1
   END IF
 END IF

 gfx.pageOfs(gfx.page) = gfx.pageOfs(gfx.page) AND gfx.pageWrap
 
 IF gfx.WindowY(gfx.page) < 0 THEN
   gfx.WindowY(0) = gfx.WindowY(0) + 24
   gfx.WindowY(1) = gfx.WindowY(1) + 24
   gfx.WindowOfs(0) = gfx.WindowOfs(0) + 1920
   gfx.WindowOfs(1) = gfx.WindowOfs(1) + 1920
   gfx.tileOfs = gfx.tileOfs - 60 'tilePosYlookUp(gfx.screenY(gfx.page)) - 60

   aTilePan 1, tileBufferSeg, tileBufferOfs

   aDrawTiles gfx.tileArea, gfx.tileOfs
 END IF

 IF gfx.WindowY(gfx.page) > 47 THEN
   gfx.WindowY(0) = gfx.WindowY(0) - 24
   gfx.WindowY(1) = gfx.WindowY(1) - 24

   gfx.WindowOfs(0) = gfx.WindowOfs(0) - 1920
   gfx.WindowOfs(1) = gfx.WindowOfs(1) - 1920
   
   gfx.tileOfs = gfx.tileOfs + 60 ' tilePosYlookUp(gfx.screenY(gfx.page)) - 60

   aTilePan 0, tileBufferSeg, tileBufferOfs

   aDrawTiles gfx.tileArea + 3, gfx.tileOfs + 180
 END IF

 tileCopylist(n, gfx.page).y = 255
 
 'aCopyTileAreas gfx.page, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)

 aRectList gfx.WindowOfs(gfx.page), gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), tileBufferWrap, tileBufferSeg, tileBufferOfs, VARSEG(tileCopylist(0, gfx.page).offset), VARPTR(tileCopylist(0, gfx.page).offset)

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

SUB initLookUps

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
  freq# = 1193180# / v
  soundFreqTable(nn, 0) = INT(freq# AND 255)
  soundFreqTable(nn, 1) = INT(freq# / 256)
 NEXT o
NEXT n
CLOSE #1

c = 0
x = 0
FOR n = -16 TO 95
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

END SUB

SUB InitScreen

 gfx.adapter = 1
 gfx.glyphheight = 3
 gfx.rows = 49

 'Disable video
 OUT &H3D8, 1
 
 'Disable blink
 IF gfx.adapter <> 1 THEN
   b = INP(&H3DA)
   'b = INP(&H3C1)
   OUT &H3C0, &H30
   b = INP(&H3C1)
   b = b AND &HF7
   OUT &H3C0, b
 END IF

 OUT &H3D4, &H0            'Horizontal total
 OUT &H3D5, &H38

 OUT &H3D4, &H1            'Horizontal displayed
 OUT &H3D5, &H28

 OUT &H3D4, &H2            'Horizontal sync pos
 OUT &H3D5, &H2D

 OUT &H3D4, &H3            'Horizontal sync width
 OUT &H3D5, &HA
 
 OUT &H3D4, &H4            'Vertical total
 OUT &H3D5, &H3F           '(nearest 2^ to row count = 63)

 OUT &H3D4, &H5            'Vertical total adjust
 OUT &H3D5, 6              '

 OUT &H3D4, &H6            'Vertical displayed
 OUT &H3D5, 50             '

 OUT &H3D4, &H7            'Vertical sync position
 OUT &H3D5, 56             'Row count + 6

 OUT &H3D4, &H8            'Interlace mode
 OUT &H3D5, 2
 
 OUT &H3D4, &H9            'Maximum scan line address, or glyph height.
 OUT &H3D5, 3


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


OPEN "kbint.bin" FOR BINARY AS #1          'Keyboard interrupt routine
 asmKBroutine = SPACE$(LOF(1))
 GET #1, , asmKBroutine
CLOSE #1

'Write KB array segment and offset into the KB routine.
MID$(asmKBroutine, 5, 2) = MKI$(VARSEG(KBarray(0)))      'Segment
MID$(asmKBroutine, 10, 2) = MKI$(VARPTR(KBarray(0)))    'Offset

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

   player.wave = 0

END SUB

SUB loadSounds (file$)
 OPEN file$ + ".stk" FOR BINARY AS #1
  GET #1, 1, sounds
 CLOSE #1
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
       'PRINT a$
       'DO: LOOP WHILE INKEY$ = ""
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

 IF sounds.Queue(sounds.ReadPos) THEN
   OUT &H43, &HB6     'Tell timer 2 we want to change countdown value

   'Send countdown value
   OUT &H42, soundFreqTable(sounds.Queue(sounds.ReadPos), 0)'Send low byte
   OUT &H42, soundFreqTable(sounds.Queue(sounds.ReadPos), 1)'Send high byte
  
   'Connect speaker to Timer 2
   OUT &H61, sounds.speakerOn

   sounds.Queue(sounds.ReadPos) = 0
  ELSE
  'Disconnect speaker
   OUT &H61, sounds.speakerOff
 END IF

 sounds.ReadPos = (sounds.ReadPos + 1) AND 63

END SUB

SUB queueSound (offset%, position%)

 writePos = (offset% + sounds.ReadPos) AND 63
 DO
  IF sounds.Track(position%) = 96 THEN EXIT SUB
  sounds.Queue(writePos) = sounds.Track(position%)
  writePos = (writePos + 1) AND 63
  position% = position% + 1
 LOOP

END SUB

SUB quit

 IF debugLog = 1 THEN CLOSE #2
 
 aKBremove KBoldSeg, KBoldOfs
 
 DEF SEG = 0
 POKE (&H417), Keyflags           'Restore old keyboard flags
 DEF SEG
 

 OUT &H43, &H34         'Return Timer to normal operation
 OUT &H40, 0            'Set low 8 bits of trigger to 0
 OUT &H40, 0            'Set hi  8 bits of trigger to 0

 'Disconnect speaker from Timer 2
 OUT &H61, sounds.speakerOff
 
 'Disable video
 OUT &H3D8, 1
 
 OUT &H3D4, &H0            'Horizontal total
 OUT &H3D5, &H71

 OUT &H3D4, &H1            'Horizontal displayed
 OUT &H3D5, &H50

 OUT &H3D4, &H2            'Horizontal sync pos
 OUT &H3D5, &H5A

 OUT &H3D4, &H3            'Horizontal sync width
 OUT &H3D5, &HA

 OUT &H3D4, &H4            'Vertical total
 OUT &H3D5, &H1F

 OUT &H3D4, &H5            'Vertical total adjust
 OUT &H3D5, 6              '

 OUT &H3D4, &H6            'Vertical displayed
 OUT &H3D5, &H19

 OUT &H3D4, &H7            'Vertical sync position
 OUT &H3D5, &H1C

 OUT &H3D4, &H8            'Interlace mode
 OUT &H3D5, 2

 OUT &H3D4, &H9            'Maximum scan line address, or glyph height.
 OUT &H3D5, 7


'Enable video, Bit 0 = 0, for low resolution text mode.
'              Bit 3 = 1, to enable video.
'              Bit 5 = 1, to enable blink.

 gfx.page = 0
 gfx.pageOfs(0) = 0
 tileCopylist(0, gfx.page).x = 0
 tileCopylist(0, gfx.page).y = 0
 tileCopylist(0, gfx.page).w = 40
 tileCopylist(0, gfx.page).h = 50
 tileCopylist(1, gfx.page).y = 255

 aClearList gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), &H7, VARSEG(tileCopylist(0, gfx.page).offset), VARPTR(tileCopylist(0, gfx.page).offset)

 OUT &H3D8, &H9 OR 16

 SHELL "cls"
 PRINT errorMessage$
 PRINT FRE(-1)
 PRINT
 PRINT "fps:"; fps
 PRINT

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

 IF x% < 0 THEN x% = 0
 IF x% > 40 THEN x% = 40

 screenTargetX = x%
 screenTargetY = y%
 
 x% = x% AND &HFFFC
 y% = y% AND &HFFF8

 IF y% < 24 THEN y% = 24
 IF y% > 590 THEN y% = 590

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

 'aCopyTileAreas gfx.page, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)
 aRectList gfx.WindowOfs(gfx.page), gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), tileBufferWrap, tileBufferSeg, tileBufferOfs, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)

 aPageFlip gfx.videoWrap, VARSEG(hudBuffer), VARPTR(hudBuffer), gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
 gfx.page = gfx.page XOR 1
 
 spriteList(1, gfx.page).y = 255
 spriteList(0, gfx.page).x = 0
 spriteList(0, gfx.page).y = 0
 spriteList(0, gfx.page).w = 40
 spriteList(0, gfx.page).h = 50
 'aCopyTileAreas gfx.page, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)
 aRectList gfx.WindowOfs(gfx.page), gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), tileBufferWrap, tileBufferSeg, tileBufferOfs, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)

' FOR c = 0 TO 7
'  handleCamera
'  handleScroll
' NEXT c

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


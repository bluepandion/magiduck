DECLARE SUB progressBar (stage AS INTEGER)
DECLARE FUNCTION getSpawnSlot% ()
DECLARE SUB loadText (screenOfs%, textOfs%)
DECLARE SUB handleCamera ()
DECLARE SUB handleScroll ()
DECLARE SUB damagePlayer ()
DECLARE SUB hudPrint (x%, text$, colour1%, colour2%)
DECLARE SUB aDrawTiles (area%, forceOfs%)
DECLARE SUB queueSound (offset%, position%)
DECLARE SUB playSounds ()
DECLARE SUB loadLevel (fileName AS STRING)
DECLARE SUB myPrint (x%, y%, text$, attributes%)
DECLARE SUB elfInit ()
DECLARE SUB initLookUps ()
DECLARE SUB InitScreen ()
DECLARE SUB loadSprites (file$, offset%)
DECLARE SUB loadTiles (file$)
DECLARE SUB loadSounds (file$)
DECLARE SUB loadKeyboard ()
DECLARE SUB quit ()

DECLARE FUNCTION tileRead% (x%, y%)

DEFINT A-Z

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

DIM SHARED asmKBinit AS STRING
DIM SHARED asmKBroutine AS STRING
DIM SHARED asmKBremove AS STRING

DIM SHARED asmPageFlip AS STRING

DIM SHARED asmTile AS STRING                'Assembly routine storage
DIM SHARED asmCopyTileArea AS STRING
DIM SHARED asmSpriteList AS STRING
DIM SHARED asmTilePan AS STRING
DIM SHARED asmClearList AS STRING
DIM SHARED asmUpdateTile AS STRING

DIM SHARED KBoldSeg AS INTEGER               'KB state save.
DIM SHARED KBoldOfs AS INTEGER
DIM SHARED Keyflags AS INTEGER

DIM SHARED spriteBank AS STRING * 6402

DIM SHARED tilemapReadX AS INTEGER              '0-7 scroll offset
DIM SHARED tilemapReadY AS INTEGER              '0-3 scroll offset
DIM SHARED tileMapReadOfs AS INTEGER            'Tilemap read offset

DIM SHARED tileBuffer AS STRING * 11362         '7760 + 1600 + 2000

DIM SHARED tilePosXlookUp(-16 TO 95) AS INTEGER
DIM SHARED tilePosYlookUp(-16 TO 655) AS INTEGER

DIM SHARED soundFreqTable(1 TO 96, 2) AS INTEGER

DIM SHARED errorMessage$

DIM SHARED camSpeedX(-23 TO 23)
DIM SHARED camSpeedY(-19 TO 19)

SUB aDrawTiles (area, forceOfs)
 'EXIT SUB
 
 p0% = forceOfs  'gfx.tileOfs

 'IF p0% < 0 THEN p0% = 0
 'IF p0% > 1540 THEN p0% = 1540

 'p1% = area AND 3
 'p2% = tileBufferSeg
 'p3% = tileBufferOfs
 'p4% = TileMapOfs
 'p5% = tileBankOfs

 'DEF SEG = SSEG(asmTile)
 'CALL absolute(BYVAL p0%, BYVAL p1%, BYVAL p2%, BYVAL p3%, BYVAL p4%, BYVAL p5%, SADD(asmTile))
 'DEF SEG

 aTileArea p0%, area AND 3, tileBufferSeg, tileBufferOfs, TileMapOfs, tileBankOfs

END SUB

SUB aUpdateTile (x, y)
 
 p2% = tilePosXlookUp(x) + tilePosYlookUp(y)

 IF p2% < 0 THEN p2% = 0
 IF p2% > 1599 THEN p2% = 1599

 p0% = (tilePosYlookUp(y) - gfx.tileOfs) \ 20

 IF p0% > 11 THEN p0% = 11

 aTileDraw p0%, tilePosXlookUp(x), p2%, tileBufferSeg, tileBufferOfs, TileMapOfs, tileBankOfs

END SUB

SUB elfInit

'-----------------------------------------------------------------------
' Detect and initialize video adapter
'-----------------------------------------------------------------------
 aInitVideo VARSEG(hudBuffer), VARPTR(hudBuffer)

 gfx.adapter = ASC(MID$(hudBuffer, 1, 1))
 gfx.oldMode = ASC(MID$(hudBuffer, 3, 1))
 
 IF gfx.adapter > 0 THEN
   PRINT MID$("CEV", gfx.adapter, 1); "GA detected."
  ELSE
   errorMessage$ = "This game requires a CGA, EGA or VGA compatible color display adapter."
   quit
 END IF


 'Save video mode to hudbuffers end, so it can be referenced by aPageFlip
 MID$(hudBuffer, 161, 1) = MID$(hudBuffer, 1, 1)

'-----------------------------------------------------------------------
' Setup variables
'-----------------------------------------------------------------------

hudBufferSeg = VARSEG(hudBuffer)
hudBufferOfs = VARPTR(hudBuffer)

spriteBankSeg = VARSEG(spriteBank)

pLong& = VARPTR(spriteBank)
a$ = MKL$(pLong&)
spriteBankOfs = CVI(a$)                        'Sprite Bank main
pLong& = pLong& + 3840
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

FOR n = 0 TO 127
 spriteOffset(n) = spriteBankOfs
 spriteWidth(n) = 1
 spriteHeight(n) = 1
NEXT n

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

'------------------------------------------------------

sounds.readPos = 0

sounds.speakerOn = INP(&H61) OR 3
sounds.speakerOff = sounds.speakerOn XOR 3

'------------------------------------------------------

CALL aTimerStart

'-----------------------------------------------------------------------
' Set up lookups, preload some data and setup screen
'-----------------------------------------------------------------------

PRINT "Lookups..."
initLookUps
PRINT "Sprites..."
loadSprites "spducks", 0

w40char.seg = VARSEG(w40char.glyph1(32))
w40char.ofs = VARPTR(w40char.glyph1(32))

InitScreen

'-----------------------------------------------------------------------
' Setup keyboard
'-----------------------------------------------------------------------
 loadKeyboard

 DEF SEG = 0
 Keyflags = PEEK(&H417) AND &H70       'Save keyboard flags
 DEF SEG

 p2% = SSEG(asmKBroutine)
 p3% = SADD(asmKBroutine)

 aKBinit KBoldSeg, KBoldOfs, p2%, p3%

END SUB

FUNCTION getSpawnSlot

 FOR n = 3 TO 60
  IF actor(n).exist = 0 THEN getSpawnSlot = n: EXIT FUNCTION
 NEXT n

END FUNCTION

SUB handleCamera

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

   aTilePan 1, tileBufferSeg, tileBufferOfs

   aDrawTiles gfx.tileArea, gfx.tileOfs
 END IF

 IF gfx.windowY(gfx.page) > 47 THEN
   gfx.windowY(0) = gfx.windowY(0) - 24
   gfx.windowY(1) = gfx.windowY(1) - 24

   gfx.windowOfs(0) = gfx.windowOfs(0) - 1920
   gfx.windowOfs(1) = gfx.windowOfs(1) - 1920
   
   gfx.tileOfs = gfx.tileOfs + 60 ' tilePosYlookUp(gfx.screenY(gfx.page)) - 60

   aTilePan 0, tileBufferSeg, tileBufferOfs

   aDrawTiles gfx.tileArea + 3, gfx.tileOfs + 180
 END IF

 tileCopyList(n, gfx.page).y = 255
 
 'aCopyTileAreas gfx.page, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)

 aRectList gfx.windowOfs(gfx.page), gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), tileBufferWrap, tileBufferSeg, tileBufferOfs, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)

END SUB

SUB importLevel
   OPEN "..\convert\import.csv" FOR INPUT AS #1
    n = 1
    DO
     INPUT #1, value
     value = value - 1
     IF value > -1 THEN
      MID$(tileBuffer, 9760 + n, 1) = CHR$(value)
      n = n + 1
      IF n = 1601 THEN CLOSE #1: EXIT SUB
     END IF
    LOOP WHILE NOT EOF(1)
   CLOSE #1
END SUB

SUB incSpawnIndex
 spawnIndex = spawnIndex + 1 + (spawnIndex > 60)
 'IF spawnIndex > 60 THEN spawnIndex = 60
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

m# = .2
FOR n = 1 TO 20                        '2  = 0.4
 mulTable(n, 0) = 0
 FOR a = 1 TO 20                       '4  = 0.8
  v = INT(m# * a)
  mulTable(n, a) = v              '8  = 1.6
  mulTable(n, -a) = -v
 NEXT a                                '12 = 2.4
 m# = m# + .2                          '16 = 3.2
NEXT n

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
  IF ABS(n) > 11 THEN speed = 2
  IF n < 0 THEN speed = -speed
  camSpeedY(n) = speed
 NEXT n

END SUB

SUB InitScreen

SELECT CASE gfx.adapter

 CASE 3  'VGA

  gfx.videoWrap = 32767
  gfx.pageWrap = 16383


  OUT &H3D4, &H9            'Maximum scan line
  OUT &H3D5, &H83           'Bit 7 true = 200 to 400 scanline conversion
                            'Bits 3-0   = Glyph height

 CASE 2  'EGA

  gfx.videoWrap = 32767
  gfx.pageWrap = 16383

  OUT &H3D4, &H6            'Vertical total
  OUT &H3D5, &H4

  OUT &H3D4, &H7            'Overflow
  OUT &H3D5, &H11

  OUT &H3D4, &H8            'Preset row SC
  OUT &H3D5, &H0

  OUT &H3D4, &H9            'Maximum scan line
  OUT &H3D5, &H3            'Bits 3-0   = Glyph height

  OUT &H3D4, &H10           'Vert retrace start
  OUT &H3D5, &HE1

  OUT &H3D4, &H11           'Vert retrace end
  OUT &H3D5, &H24

  OUT &H3D4, &H12           'Vert display end
  OUT &H3D5, &HC7

  OUT &H3D4, &H15           'Start vBlank
  OUT &H3D5, &HE0

  OUT &H3D4, &H16           'End vBlank
  OUT &H3D5, &HF0

 CASE 1  'CGA

  gfx.videoWrap = 16383
  gfx.pageWrap = 8191

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


  'Enable video,             Bit 0 = 0, for low resolution text mode.
  '                          Bit 3 = 1, to enable video.
  '                          Bit 5 = 1, to enable blink.
 
  OUT &H3D8, &H8   '&H8 = 40 column, &H9 = 80 column
 
END SELECT

clearCount = 3
IF gfx.adapter >= 1 THEN clearCount = 7

gfx.page = 0
gfx.pageOfs(0) = 0
FOR n = 0 TO clearCount
 tileCopyList(0, gfx.page).x = 0
 tileCopyList(0, gfx.page).y = 0
 tileCopyList(0, gfx.page).w = 40
 tileCopyList(0, gfx.page).h = 50
 tileCopyList(1, gfx.page).y = 255

 aClearList gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), &HDE, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)

 gfx.pageOfs(0) = gfx.pageOfs(0) + 2000
NEXT n

v1 = 16000
v2 = 16382
IF gfx.adapter > 1 THEN v1 = 32000: v2 = 32766

DEF SEG = &HB800
FOR a& = v1 TO v2 STEP 2
 POKE a& + 1, 0
 POKE a&, 222
NEXT a&
DEF SEG

gfx.pageOfs(0) = 0
gfx.pageOfs(1) = 5000
gfx.page = 0


END SUB

SUB loadKeyboard

OPEN "kbint.bin" FOR BINARY AS #1          'Keyboard interrupt routine
 asmKBroutine = SPACE$(LOF(1))
 GET #1, , asmKBroutine
CLOSE #1

'Write KB array segment and offset into the KB routine.
MID$(asmKBroutine, 5, 2) = MKI$(VARSEG(KBarray(0)))      'Segment
MID$(asmKBroutine, 10, 2) = MKI$(VARPTR(KBarray(0)))    'Offset

END SUB

SUB loadLevel (fileName AS STRING)

   'debugwrite "Level load " + fileName

   OPEN fileName + ".lvl" FOR BINARY AS #1
    
    FOR n = 1 TO 1600
     GET #1, n, loadStr
     MID$(tileBuffer, 9760 + n, 1) = loadStr
    NEXT n

    progressBar 0

    readOfs = 1601
    GET #1, readOfs, level
   CLOSE #1

   progressBar 1

   IF player.spriteSet <> level.spriteSet THEN loadSprites level.spriteSet, spriteBankAuxOfs

   progressBar 2

   IF player.tileSet <> level.tileSet THEN loadTiles level.tileSet

   progressBar 3

   'debugwrite "OK"

END SUB

SUB loadSounds (file$)
 OPEN file$ + ".stk" FOR BINARY AS #1
  GET #1, 1, sounds
 CLOSE #1
END SUB

SUB loadSprites (file$, offset)

 IF offset = 0 THEN
   offset = spriteBankOfs
  ELSE
   offset = spriteBankAuxOfs
   player.spriteSet = file$
 END IF

 DEF SEG = spriteBankSeg

 BLOAD file$ + ".csg", offset

 DEF SEG

 OPEN file$ + ".csd" FOR BINARY AS #1
  fileOfs = 1
  a$ = MKL$(offset)
  baseOfs& = CVL(a$)

  DO
   GET #1, fileOfs, n
    IF n <> -1 THEN
     GET #1, fileOfs + 2, loadInt
      pLong& = baseOfs& + loadInt
      a$ = MKL$(pLong&)
      spriteOffset(n) = CVI(a$)
      'PRINT CVL(a$)
      'PRINT spriteOffset(n)
      'DO: LOOP WHILE INKEY$ = ""
     GET #1, fileOfs + 4, spriteWidth(n)
     GET #1, fileOfs + 6, spriteHeight(n)
     fileOfs = fileOfs + 8
    END IF
  LOOP UNTIL n = -1
  fileOfs = fileOfs + 2

  DO
   GET #1, fileOfs, n
   IF n <> -1 THEN
    GET #1, fileOfs + 2, animation(n)
    fileOfs = fileOfs + 14
   END IF
  LOOP UNTIL n = -1
 CLOSE #1

END SUB

SUB loadText (screenOfs, textOfs)
 
 DEF SEG = &HB800

 OPEN "ducktext.ctt" FOR BINARY AS #1
  FOR n = 1 TO 160

   textOfs = textOfs + 1
   GET #1, textOfs, loadStr

   POKE screenOfs, ASC(loadStr)
   screenOfs = screenOfs + 1

  NEXT n
 CLOSE #1

 DEF SEG
   
END SUB

SUB loadTiles (file$)

 DEF SEG = tileBufferSeg
 
 BLOAD file$ + ".ctg", tileBankOfs

 player.tileSet = file$

 DEF SEG

END SUB

SUB myPrint (x, y, text$, attributes)

 w40char.text = text$ + "|"
 aPrint gfx.videoWrap, attributes, w40char.seg, w40char.ofs, x, y, &HB800, gfx.pageOfs(gfx.page) * 2

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

SUB progressBar (stage AS INTEGER)

 n = gfx.pageOfs(gfx.page) * 2 + 40
 POKE n, 219
 POKE n + 1, &HFE
 n = n + 78
 POKE n, 219
 POKE n + 1, &HFE

END SUB

SUB queueSound (offset%, position%)

 writePos = (offset% + sounds.readPos) AND 63
 DO
  IF sounds.Track(position%) = 96 THEN EXIT SUB
  sounds.Queue(writePos) = sounds.Track(position%)
  writePos = (writePos + 1) AND 63
  position% = position% + 1
 LOOP

END SUB

SUB quit

IF gfx.adapter > 0 THEN
 aKBremove KBoldSeg, KBoldOfs     'Restore old keyboard routine
 
 DEF SEG = 0
 POKE (&H417), Keyflags           'Restore old keyboard flags
 DEF SEG

 'Disconnect speaker from Timer 2
 OUT &H61, sounds.speakerOff

 CALL aTimerEnd

END IF
 
 aExitVideo gfx.adapter, gfx.oldMode

 GOTO exitgame

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
 tileCopyList(0, gfx.page).x = 0
 tileCopyList(0, gfx.page).y = 0
 tileCopyList(0, gfx.page).w = 40
 tileCopyList(0, gfx.page).h = 50
 tileCopyList(1, gfx.page).y = 255

 aClearList gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), &H7, VARSEG(tileCopyList(0, gfx.page).offset), VARPTR(tileCopyList(0, gfx.page).offset)

 OUT &H3D8, &H9 OR 16

exitgame:

 'SHELL "cls"
 PRINT errorMessage$
 PRINT FRE(-1)
 PRINT
 PRINT "fps:"; fps
 PRINT

 C = 1

 FOR n = 1 TO 128
  C = C + 4
  COLOR (n AND 1) + 7
  PRINT HEX$(spawnerEnabler(n)); TAB(C);
  IF ((n - 1) AND 15) = 15 THEN PRINT : C = 1
 NEXT n

 FOR n = 1 TO 30
  PRINT n; "("; actor(n).control; "):"; actor(n).lPrev; actor(n).lNext,
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

SUB screenFakePos (y)

 ' - Works on the assumption that the buffer window Y > 24, how to fix?
 '
 ' - windowY and windowOfs must match current situation.
 '
 ' - y(change) && 0x8 must match y(current) && 0x8.


yTileRelative = gfx.screenY(gfx.page) AND 7

y = (y AND &HFFF8) + yTileRelative

diffY = y - gfx.screenY(gfx.page)

 FOR n = 2 TO 60
  actor(n).y = actor(n).y + diffY
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
   aDrawTiles 0, gfx.tileOfs
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

 screenTargetYlock = -1
 screenTargetX = x%
 screenTargetY = y%
 
 x% = x% AND &HFFFC
 y% = y% AND &HFFF8

 'IF Y% < 24 THEN Y% = 24
 'IF Y% > 590 THEN Y% = 590
 IF y% < 0 THEN y% = 0
 IF y% > 615 THEN y% = 615


 FOR n = 0 TO 1
  gfx.screenX(n) = x%
  gfx.screenY(n) = y%
  gfx.screenOldX(n) = x%
  gfx.screenOldY(n) = y%
  gfx.windowX(n) = x%
  gfx.windowY(n) = 24
  gfx.windowOfs(n) = 1920 + x%
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

 aRectList gfx.windowOfs(gfx.page), gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), tileBufferWrap, tileBufferSeg, tileBufferOfs, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)

 aPageFlip gfx.videoWrap, hudBufferSeg, hudBufferOfs, gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
 gfx.page = gfx.page XOR 1
 
 spriteList(1, gfx.page).y = 255
 spriteList(0, gfx.page).x = 0
 spriteList(0, gfx.page).y = 0
 spriteList(0, gfx.page).w = 40
 spriteList(0, gfx.page).h = 50

 aRectList gfx.windowOfs(gfx.page), gfx.videoWrap, &HB800, gfx.pageOfs(gfx.page), tileBufferWrap, tileBufferSeg, tileBufferOfs, VARSEG(spriteList(0, gfx.page).offset), VARPTR(spriteList(0, gfx.page).offset)

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


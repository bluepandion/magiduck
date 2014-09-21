DEFINT A-Z

DECLARE SUB aPageFlip (BYVAL wrapOfs%, BYVAL hudSSeg%, BYVAL hudSofs%, BYVAL prevPageOfs%, BYVAL setPageOfs%)
DECLARE SUB aKBinit (oldSeg%, oldOfs%, BYVAL newSeg%, BYVAL newOfs%)
DECLARE SUB aKBremove (BYVAL oldSeg%, BYVAL oldOfs%)
DECLARE SUB aClearList (BYVAL wrapOfs%, BYVAL videoSeg%, BYVAL videoOfs%, BYVAL ClearWord%, BYVAL sprListSeg%, BYVAL sprListOfs%)
DECLARE SUB aRectList (BYVAL tileBufWindowOfs%, BYVAL wrapOfs%, BYVAL videoSeg%, BYVAL videoOfs%, BYVAL tileBufWrap%, BYVAL tileBufSeg%, BYVAL tileBugOfs%, BYVAL sprListSeg%, BYVAL sprListOfs%)
DECLARE SUB aSpriteList (BYVAL wrapOfs%, BYVAL videoSeg%, BYVAL videoOfs%, BYVAL sprBankSeg%, BYVAL sprBankOfs%, BYVAL sprListSeg%, BYVAL sprListOfs%)
DECLARE SUB aTileArea (BYVAL forceOfs%, BYVAL area%, BYVAL tileBufferSeg%, BYVAL tileBufferOfs%, BYVAL TileMapOfs%, BYVAL tileBankOfs%)
DECLARE SUB aTileDraw (BYVAL tileYOfs%, BYVAL tileXOfs%, BYVAL tileMapReadOfs%, BYVAL tileBufSeg%, BYVAL tileBufOfs%, BYVAL TileMapOfs%, BYVAL tileBankOfs%)
DECLARE SUB aTilePan (BYVAL panDir%, BYVAL tileBufSeg%, BYVAL tileBufOfs%)

'-------------------------------------------------------------------------
' Variables and types
'-------------------------------------------------------------------------

'$STATIC

COMMON SHARED dataFolder AS STRING

COMMON SHARED hudBuffer AS STRING * 160

DIM KBarray(130)  AS INTEGER
COMMON SHARED KBarray() AS INTEGER

COMMON SHARED fps AS DOUBLE
COMMON SHARED dt AS DOUBLE                     'Delta time
COMMON SHARED dtCompare AS DOUBLE
COMMON SHARED dtAnims AS INTEGER
COMMON SHARED dtFrame AS DOUBLE
COMMON SHARED dtGame AS DOUBLE
COMMON SHARED dtKeys AS DOUBLE

COMMON SHARED mainCounter0 AS INTEGER
COMMON SHARED mainCounter1 AS INTEGER
COMMON SHARED mainCounter2 AS INTEGER

DIM multiplyTable(1 TO 20, -20 TO 20) AS INTEGER
COMMON SHARED multiplyTable() AS INTEGER

DIM fakeFloat(-40 TO 40, 10) AS INTEGER
COMMON SHARED fakeFloat() AS INTEGER
COMMON SHARED floatC AS INTEGER

COMMON SHARED spawnIndex AS INTEGER

'----------------------------------------------------------------------------
' Graphics storage and screen buffer related
'----------------------------------------------------------------------------

DIM spriteOffset(128)   AS INTEGER      'Sprite bank offsets per sprite
DIM spriteWidth(128)  AS INTEGER        'and some basic data.
DIM spriteHeight(128)  AS INTEGER
COMMON SHARED spriteOffset() AS INTEGER
COMMON SHARED spriteWidth() AS INTEGER
COMMON SHARED spriteHeight() AS INTEGER

COMMON SHARED spriteBankSeg AS INTEGER
COMMON SHARED spriteBankOfs AS INTEGER

COMMON SHARED tileBufferSeg AS INTEGER
COMMON SHARED tileBufferOfs AS INTEGER
COMMON SHARED tileBankOfs AS INTEGER
COMMON SHARED TileMapOfs AS INTEGER
COMMON SHARED tileBufferWrap AS INTEGER

COMMON SHARED hudBufferSeg AS INTEGER
COMMON SHARED hudBufferOfs AS INTEGER

TYPE physicsType
 X AS INTEGER
 Y AS INTEGER
 speedX AS INTEGER
 speedY AS INTEGER
 collider AS INTEGER
 childX AS INTEGER
 childY AS INTEGER
  'h0 Hit Player
  'h1 Hit Enemy
  'h2 Hit Bullet
  'h3
END TYPE

TYPE behaviorType
 anim AS INTEGER
 animFrame AS INTEGER
 life AS INTEGER
 control AS INTEGER
 jump AS INTEGER
 counter AS INTEGER
 flash AS INTEGER
 exist AS INTEGER
 spawner AS INTEGER
 flags AS INTEGER
 direction AS INTEGER
 visX AS INTEGER
 visY AS INTEGER
END TYPE

TYPE spawnerType
 X AS INTEGER
 Y AS INTEGER
 object AS INTEGER
 flags AS INTEGER
END TYPE

TYPE waveType
 spawner(1 TO 16) AS spawnerType
 scoreTime(1 TO 3) AS INTEGER
 length AS INTEGER
 flags AS INTEGER
END TYPE

TYPE levelType
 wave(0 TO 7) AS waveType
 playerX AS INTEGER
 playerY AS INTEGER
END TYPE

TYPE animDataType
 index(0 TO 1) AS INTEGER
 X(0 TO 1) AS INTEGER
 Y AS INTEGER
 jump AS INTEGER
END TYPE

TYPE collisionType
 index AS INTEGER
 x0 AS INTEGER
 y0 AS INTEGER
 x1 AS INTEGER
 y1 AS INTEGER
END TYPE

TYPE flagsType
 jump AS INTEGER
 flip AS INTEGER
 animFinished AS INTEGER
 flash AS INTEGER
END TYPE

TYPE playerType
 score AS LONG
 lives AS INTEGER
 hp AS INTEGER
 time AS INTEGER
 hasKey AS INTEGER
 shootDelay AS INTEGER
 flying AS INTEGER
 level AS INTEGER
 wave AS INTEGER
 hudLock AS INTEGER
 mode AS INTEGER
 menuTarget AS INTEGER
 targetX AS INTEGER
 targetY AS INTEGER
 keyPrev AS INTEGER
 keyRelease AS INTEGER
 jumpRelease AS INTEGER
END TYPE

TYPE spriteListType
 offset AS INTEGER
 X AS INTEGER
 Y AS INTEGER
 w AS INTEGER
 h AS INTEGER
END TYPE

TYPE gfxType
 width AS INTEGER
 height AS INTEGER
 segment AS INTEGER
 adapter AS INTEGER
 glyphHeight AS INTEGER
 rows AS INTEGER
 videoWrap AS INTEGER
 pageWrap AS INTEGER
 tileOfs AS INTEGER
 tileArea AS INTEGER
 windowOfs(0 TO 1) AS INTEGER
 windowX(0 TO 1) AS INTEGER
 windowY(0 TO 1) AS INTEGER
 screenX(0 TO 1) AS INTEGER
 screenY(0 TO 1) AS INTEGER
 screenoldx(0 TO 1) AS INTEGER
 screenoldy(0 TO 1) AS INTEGER
 pageOfs(0 TO 1) AS INTEGER
 page AS INTEGER
 listLen(0 TO 1) AS INTEGER
END TYPE

TYPE angleType
 X(0 TO 63) AS INTEGER
 Y(0 TO 63) AS INTEGER
END TYPE

COMMON SHARED gfx AS gfxType

DIM spriteList(0 TO 31, 0 TO 1) AS spriteListType         'Sprite list = 600 bytes
COMMON SHARED spriteList() AS spriteListType

DIM tileCopyList(0 TO 2, 0 TO 1) AS spriteListType
COMMON SHARED tileCopyList() AS spriteListType
                                               
COMMON SHARED player AS playerType

COMMON SHARED flags AS flagsType

DIM physics(0 TO 60) AS physicsType
COMMON SHARED physics()  AS physicsType

DIM object(0 TO 60) AS behaviorType
COMMON SHARED object()  AS behaviorType

COMMON SHARED level AS levelType              'Level object data

DIM animation(0 TO 255) AS animDataType
COMMON SHARED animation() AS animDataType

DIM colCount(1 TO 3) AS INTEGER
COMMON SHARED colCount() AS INTEGER
DIM colList(1 TO 3, 30) AS collisionType
COMMON SHARED colList() AS collisionType

DIM objectIndex(0 TO 65) AS INTEGER
COMMON SHARED objectIndex() AS INTEGER

COMMON SHARED spriteCount AS INTEGER
COMMON SHARED objCount AS INTEGER

COMMON SHARED playerJumpToggle AS INTEGER
COMMON SHARED playerOldX AS INTEGER
COMMON SHARED playerOldY AS INTEGER

COMMON SHARED screenX AS INTEGER                        'Screen position
COMMON SHARED screenY AS INTEGER                        'in Tile Map.
COMMON SHARED screenoldx AS INTEGER
COMMON SHARED screenoldy AS INTEGER
COMMON SHARED screenTargetX AS INTEGER
COMMON SHARED screenTargetY AS INTEGER
COMMON SHARED screenTargetYlock AS INTEGER

COMMON SHARED playerObj AS INTEGER

COMMON SHARED skippedFrame AS INTEGER

COMMON SHARED angle AS angleType

TYPE soundType
 Queue(0 TO 63) AS INTEGER
 readPos AS INTEGER
 speakerOn AS INTEGER
 speakerOff AS INTEGER
 Track(0 TO 255) AS INTEGER
END TYPE

COMMON SHARED sounds AS soundType

CONST defaultVisX = 25
CONST defaultVisY = 30

COMMON SHARED randomOffset AS INTEGER


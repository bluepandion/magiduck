DEFINT A-Z
DECLARE SUB aPageFlip (BYVAL prevPageOfs%, BYVAL setPageOfs%)
DECLARE SUB aKBinit ()
DECLARE SUB aKBremove ()
DECLARE SUB aClearList (BYVAL videoOfs%, BYVAL sprListSeg%, BYVAL sprListOfs%, BYVAL ClearWord%)
DECLARE SUB aRectList (BYVAL tileBufWindowOfs%, BYVAL videoOfs%, BYVAL sprListSeg%, BYVAL sprListOfs%)
DECLARE SUB aSpriteList (BYVAL videoOfs%, BYVAL sprListSeg%, BYVAL sprListOfs%)
DECLARE SUB aTileArea (BYVAL forceOfs%, BYVAL area%)
DECLARE SUB aTileDraw (BYVAL tileYOfs%, BYVAL tileXOfs%, BYVAL tileMapReadOfs%)
DECLARE SUB aTilePan (BYVAL panDir%)
DECLARE SUB aPrint (BYVAL wrapOfs%, BYVAL attributes%, BYVAL Column%, BYVAL Row%, BYVAL writeSeg%, BYVAL writeOfs%)

DECLARE FUNCTION aVideoDetect% (BYVAL dataSeg%, BYVAL dataOfs%)
DECLARE SUB aVideoSet (BYVAL mode%)
DECLARE SUB aVideoExit ()

DECLARE SUB aSoundFX (BYVAl fx%, BYVAL offset%)
DECLARE SUB aSoundNote (BYVAL note%)
DECLARE SUB aSoundStop ()
DECLARE SUB aSoundPlay ()

DECLARE SUB aSetup (BYVAL dataSeg%, BYVAL dataOfs%)
DECLARE SUB aCopyPage (BYVAL sourceOfs%, BYVAL destOfs%)
DECLARE SUB aMenuHiLite (BYVAL x%, BYVAL y%, BYVAL w%, BYVAL h%, BYVAL attribs%, BYVAL pageOfs%)

DECLARE SUB aTimerStart ()
DECLARE SUB aTimerEnd ()
DECLARE SUB aTimerWait ()
DECLARE SUB aTimerReset ()

'-------------------------------------------------------------------------
' Variables and types
'-------------------------------------------------------------------------

'$STATIC

COMMON SHARED loadStr AS STRING * 1             'General purpose file read
COMMON SHARED loadVarStr AS STRING				'/write variables
COMMON SHARED loadInt AS INTEGER               
COMMON SHARED loadLong AS LONG
COMMON SHARED fileOfs AS INTEGER

COMMON SHARED hudBuffer AS STRING * 164

DIM KBarray(0 to 130)  AS INTEGER
COMMON SHARED KBarray() AS INTEGER

COMMON SHARED fps AS DOUBLE
COMMON SHARED dtAnims AS INTEGER
COMMON SHARED dtMain0 AS INTEGER
COMMON SHARED dtMain1 AS INTEGER
COMMON SHARED dtMain2 AS INTEGER

DIM mulTable(1 TO 20, -20 TO 20) AS INTEGER
COMMON SHARED mulTable() AS INTEGER

DIM fraction(-40 TO 40, 10) AS INTEGER
COMMON SHARED fraction() AS INTEGER
COMMON SHARED fractionC AS INTEGER

COMMON SHARED spawnIndex AS INTEGER

'----------------------------------------------------------------------------
' Graphics storage and screen buffer related
'----------------------------------------------------------------------------

COMMON SHARED spriteBankSeg AS INTEGER
COMMON SHARED spriteBankOfs AS INTEGER
COMMON SHARED spriteBankAuxOfs AS INTEGER

COMMON SHARED tileBufferSeg AS INTEGER
COMMON SHARED tileBufferOfs AS INTEGER
COMMON SHARED tileBankOfs AS INTEGER
COMMON SHARED TileMapOfs AS INTEGER
COMMON SHARED tileBufferWrap AS INTEGER

COMMON SHARED hudBufferSeg AS INTEGER
COMMON SHARED hudBufferOfs AS INTEGER

TYPE objectType
	X AS INTEGER
	Y AS INTEGER
	frame AS INTEGER
	flags AS INTEGER
	collider AS INTEGER
	flash AS INTEGER

	vecX AS INTEGER
	vecY AS INTEGER
	parentDX AS INTEGER
	parentDY AS INTEGER

	exist AS INTEGER
	control AS INTEGER
	life AS INTEGER
	anim AS INTEGER
	jump AS INTEGER
	count AS INTEGER
	dir AS INTEGER
	spawner AS INTEGER
	visX AS INTEGER
	visY AS INTEGER

	lNext AS INTEGER
	lPrev AS INTEGER
END TYPE

TYPE spawnerType
	X AS INTEGER
	Y AS INTEGER
	object AS INTEGER
	flags AS INTEGER
END TYPE

TYPE levelType
	spawner(0 TO 64) as spawnerType
	playerX AS INTEGER
	playerY AS INTEGER
	tileSet AS STRING * 8
	spriteSet AS STRING * 8
	keys AS INTEGER
	coins AS INTEGER
END TYPE

TYPE animDataType
	offset(0 to 1)	AS INTEGER
	w AS INTEGER
	h AS INTEGER
	x AS INTEGER
	y AS INTEGER
	jump AS INTEGER
END TYPE
DIM animation(0 TO 119) AS animDataType
COMMON SHARED animation() AS animDataType

TYPE collisionType
	index AS INTEGER
	x0 AS INTEGER
	y0 AS INTEGER
	x1 AS INTEGER
	y1 AS INTEGER
END TYPE

TYPE playerType	
	score AS LONG
	secrets AS INTEGER
	lives AS INTEGER
	hp AS INTEGER
	time AS INTEGER
	hasKey AS INTEGER
	shootDelay AS INTEGER
	flying AS INTEGER
	level AS INTEGER
	levelPrev AS INTEGER
	wave AS INTEGER
	wavePrev AS INTEGER
	hudLock AS INTEGER
	mode AS INTEGER
	menuMode AS INTEGER
	menuContext AS INTEGER
	menuContextPrev AS INTEGER
	menuItem AS INTEGER
	menuPrev AS INTEGER
	menuHLcolor AS INTEGER
	targetX AS INTEGER
	targetY AS INTEGER		
	bouncing AS INTEGER	
	inWater AS INTEGER
	supaFly AS INTEGER	
	vecX AS INTEGER
	vecY AS INTEGER	
	scoreCounter AS LONG
	scoreDelta AS LONG
	scoreCounterTarget AS LONG
	scoreCounterInc AS INTEGER
	scoreCounterMode AS INTEGER
	scoreCounterSound AS INTEGER
	scoreCounterMul AS INTEGER
	tileSet AS STRING * 8
	spriteSet AS STRING * 8
	episode AS STRING * 8
	levelScore(1 TO 16) AS LONG
	levelCompletion(1 TO 16) AS INTEGER
	levelTime(1 to 16) AS INTEGER
	saveSlot AS INTEGER
	gotKeys AS INTEGER
	gotCoins AS INTEGER
	gotChests AS INTEGER
	levelColor AS INTEGER
	parent AS INTEGER
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
	oldMode AS INTEGER
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

DIM actor(1 TO 60) AS objectType
COMMON SHARED actor()  AS objectType

COMMON SHARED level AS levelType

DIM colCount(1 TO 3) AS INTEGER
COMMON SHARED colCount() AS INTEGER
DIM colList(1 TO 3, 30) AS collisionType
COMMON SHARED colList() AS collisionType

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

COMMON SHARED angle AS angleType

COMMON SHARED tRead AS INTEGER
COMMON SHARED tReadSeg AS INTEGER
COMMON SHARED tReadOfs AS INTEGER

TYPE w40textType
 glyph1(32 TO 90) AS STRING * 1
 glyph2(32 TO 90) AS STRING * 1
 text AS STRING * 45
 seg AS INTEGER
 ofs AS INTEGER
END TYPE
COMMON SHARED w40char AS w40textType

DIM menuItem(1 to 16) AS INTEGER
COMMON SHARED menuItem() AS INTEGER

COMMON SHARED errorMessage$
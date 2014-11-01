TYPE gfxType
 tileMap AS STRING * 1600
 tileGfx AS STRING * 6400
 tileBuffer AS STRING * 6400
END TYPE

DIM gfx AS gfxType

CLS

PRINT VARSEG(gfx.tileMap), VARPTR(gfx.tileMap)
PRINT VARSEG(gfx.tileGfx), VARPTR(gfx.tileGfx)
PRINT VARSEG(gfx.tileBuffer), VARPTR(gfx.tileBuffer)

OPEN "level0.lvl" FOR BINARY AS #1
 GET #1, 1, gfx.tileMap
CLOSE #1

FOR n = 1 TO 1600
 PRINT HEX$(ASC(MID$(gfx.tileMap, n, 1)));
NEXT n


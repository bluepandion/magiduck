DEFINT A-Z

DIM tiles(2000) AS STRING*1
DIM outStr AS STRING * 1

fileOutOfs = 33

OPEN "tiles.ctp" FOR OUTPUT AS #1
	PRINT #1, "error"
CLOSE #1

OPEN "tiles.ctp" FOR BINARY AS #2
	OPEN "..\convert\tilepak.txt" FOR INPUT AS #1
	DO
		INPUT #1, a$
		IF UCASE$(a$) <> "END" THEN
			tileSetNum = VAL(A$)
			INPUT #1, a$
			DEF SEG = VARSEG(tiles(0))
			BLOAD a$ + ".ctg", VARPTR(tiles(0))
			PRINT a$ + ".ctg", fileOutOfs
			rleIndex$ = tiles(0)
			rleCmp$ = tiles(0)
			rleLen = 1
			PUT #2, tileSetNum * 2 + 1, fileOutOfs
			FOR n = 1 to 1999				
				rleIndex$ = tiles(n)
				rleLen = rleLen + 1
				IF rleIndex$ <> rleCmp$ OR rleLen = 255 THEN										
					PRINT rleLen;
					outStr = chr$(rleLen)
					PUT #2, fileOutOfs, outStr
					PUT #2, fileOutOfs + 1, rleCmp$
					fileOutOfs = fileOutOfs + 2					
					rleLen = 0
					rleCmp$ = rleIndex$
				END IF				
			NEXT n				
		END IF
	LOOP WHILE UCASE$(a$) <> "END"
	CLOSE #1
CLOSE #2
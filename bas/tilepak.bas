DEFINT A-Z

DECLARE SUB CheckBytes ()
DECLARE SUB SaveRLE ()
DECLARE SUB SavePaletteRLE ()

DIM SHARED tiles(2000) AS STRING*1
DIM SHARED outStr AS STRING * 1
DIM SHARED tileByte(256) AS INTEGER
DIM SHARED numDifferentBytes AS INTEGER
DIM SHARED numMatches(256) AS INTEGER
DIM SHARED tPalette(0 to 14) AS STRING*1

FOR n = 0 to 255
	tileByte(n) = -1
	numMatches(n) = 0
NEXT n

DIM SHARED fileOutOfs AS INTEGER
fileOutOfs = 33

OPEN "tiles.ctp" FOR OUTPUT AS #1
	PRINT #1, "error"
CLOSE #1

nonRLString$ = ""
OPEN "debug.txt" FOR output as #3

OPEN "tiles.ctp" FOR BINARY AS #2
	OPEN "..\convert\tilepak.txt" FOR INPUT AS #1
	DO
		COLOR 7
		INPUT #1, a$
		IF UCASE$(a$) <> "END" THEN
			numDifferentBytes = 0
			numMatches = 0
			tileSetNum = VAL(A$)
			INPUT #1, a$
			DEF SEG = VARSEG(tiles(0))
			BLOAD a$ + ".ctg", VARPTR(tiles(0))
			PRINT #3, a$ + ".ctg", fileOutOfs			
			rleLen = 1
			PUT #2, tileSetNum * 2 + 1, fileOutOfs
			CheckBytes
			COLOR 8
			PRINT #3, "  Different attribute pairs: "; numDifferentBytes
			maxM = 9999
			tot = 0
			PRINT #3, "  Top 15: ";
			FOR c = 0 TO 14
				m = 0			
				nn = 0
				FOR n = 0 to 255
					IF numMatches(n) > m AND numMatches(n) < maxM THEN m = numMatches(n):nn= n
				NEXT n			
				tPalette(c) = CHR$(tileByte(nn))
				'PRINT " Max Matches:    "; m'numMatches
				PRINT #3, m;
				tot = tot + m
				maxM = m
			NEXT c
			PRINT #3, ""
			PRINT #3, "  Total appearences in top 15: ";tot
			'SavePaletteRLE
			SaveRLE
			PRINT #3, ""
		END IF
		FOR n = 0 to 255
			tileByte(n) = -1
			numMatches(n) = 0
		NEXT n
	LOOP WHILE UCASE$(a$) <> "END"
	CLOSE #1
CLOSE #2

CLOSE #3

SUB CheckBytes
	FOR tile = 1 TO 1920
		byte = ASC(tiles(tile))
		FOR n = 0 to 255
			IF byte = tileByte(n) THEN			
				numMatches(n) = numMatches(n) + 1
				n = 255
				EXIT FOR
			ELSEIF tileByte(n) = -1 THEN
				numDifferentBytes = numDifferentBytes + 1
				tileByte(n) = byte
				n = 255
				EXIT FOR
			END IF
		NEXT n
	NEXT tile
END SUB

SUB SaveRle 
	outPut$ = ""
	rleLen = 0
	rleIndex$ = tiles(0)
	rleCmp$ = tiles(0)
	FOR n = 1 to 1920							
		rleIndex$ = tiles(n)				
		nonRLString$ = nonRLString$ + rleIndex$
		rleLen = rleLen + 1
		IF rleIndex$ <> rleCmp$ OR rleLen = 15 THEN
			
			PRINT rleLen;
			IF rleLen > 1 THEN
				outStr = chr$(rleLen OR &HB0)				
				outPut$ = outPut$ + outStr
				'outStr = chr$(rleLen)
				'PUT #2, fileOutOfs, outStr
				outPut$ = outPut$ + rleCmp$				
			ELSE						
				outPut$ = outPut$ + rleCmp$
				nonRLString$ = ""
			END IF
			rleLen = 0
			rleCmp$ = rleIndex$
		END IF				
	NEXT n
	n = len(outPut$)
	PUT #2, fileOutOfs, n
	fileOutOfs = fileOutOfs + 2
	PUT #2, fileOutOfs, outPut$
	fileOutOfs = fileOutOfs + n
END SUB

SUB SavePaletteRLE 
	outPut$ = ""
	rleLen = 0
	rleIndex$ = tiles(0)
	rleCmp$ = tiles(0)
	numPalEntries = 0
	numNormEntries = 0
	
	FOR n = 0 TO 14		
		PUT #2, fileOutOfs, tPalette(n)
		fileOutOfs = fileOutOfs + 1
	NEXT n
	
	FOR n = 1 to 1920									
		
		rleIndex$ = tiles(n)
		'nonRLString$ = nonRLString$ + rleIndex$
		rleLen = rleLen + 1
		
		IF rleIndex$ <> rleCmp$ OR rleLen = 15 THEN
			
			out$ = ""
			palTile1 = -1
			palTile2 = -1
			FOR c = 0 to 14
				IF tPalette(c) = rleCmp$ THEN palTile1 = c
				IF tPalette(c) = rleIndex$ THEN palTile2 = c
			NEXT c
			
			IF rleLen < 2 THEN				
			
				IF palTile1 > -1 AND palTile2 >-1 THEN 
				
					out$ = CHR$(palTile1 * 16 + palTile2)
					n = n + 1
					rleIndex$ = tiles(n)
					'numPalEntries = numPalEntries + 1
				ELSE
					
					out$ = CHR$(&HF1) + rleCmp$
					'numNormEntries = numNormEntries + 1
				END IF
			ELSE
				IF palTile1 > -1 THEN				
					out$ = CHR$(palTile1 * 16 + rleLen)
				ELSE
					out$ = CHR$(&HF0 + rleLen) + rleCmp$
				END IF
			END IF
			outPut$ = outPut$ + out$			
			rleLen = 0
			rleCmp$ = rleIndex$
		END IF				
	NEXT n
	PUT #2, fileOutOfs, outPut$
	fileOutOfs = fileOutOfs + LEN(outPut$)
	PRINT #3, "  Normal entries:  "; numNormEntries
	PRINT #3, "  Palette entries: "; numPalEntries
END SUB
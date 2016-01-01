DEFINT A-Z
DIM SHARED ofsOFFSETS AS INTEGER
DIM SHARED ofsData AS INTEGER
DIM SHARED itemCount AS INTEGER
DIM SHARED byte AS STRING * 1

DIM SHARED saveString AS STRING
DIM SHARED fileString AS STRING
DIM SHARED prelString AS STRING

fileString = STRING$(200, 0)
prelString = STRING$(20, 0)

ofsOFFSETS = 1
ofsData = 0

'----------------------------------------------------------------------------

CLS

   OPEN "..\convert\menus.txt" FOR INPUT AS #1
   DO
      INPUT #1, menuNo
      menuNo = menuNo - 1
      INPUT #1, startItem
      INPUT #1, escItem
	  INPUT #1, fineTuneX
      INPUT #1, fineTuneY
      INPUT #1, clearX
      INPUT #1, clearY
      INPUT #1, clearW
      INPUT #1, clearH
      INPUT #1, clearchar$
      INPUT #1, clearAttr

      saveString = ""

      'Save streamed or preloaded menu data
      '----------------------------------------------------------------------
         byte = CHR$(startItem)
         saveString = saveString + byte
		 byte = CHR$(escItem)
         saveString = saveString + byte
		 byte = CHR$(clearX + fineTuneX)
         saveString = saveString + byte
         byte = CHR$(clearY + fineTuneY)
         saveString = saveString + byte
         byte = CHR$(clearW)
         saveString = saveString + byte
         byte = CHR$(clearH)
         saveString = saveString + byte
         byte = clearchar$
         saveString = saveString + byte
         byte = CHR$(clearAttr)
         saveString = saveString + byte

         itemCount = 0
         DO
            INPUT #1, itemIDX
            IF itemIDX <> -1 THEN
               itemCount = itemCount + 1
               PRINT itemIDX;
               INPUT #1, itemPrev
               INPUT #1, itemNext
               
			   xSpace = 0
			   xAdd = 1
			   itemText$ = ""
			   INPUT #1, a$
               
			   FOR i = 1 TO LEN(a$)
				IF MID$(a$, i, 1) <> "_" THEN 
					itemText$ = itemText$ + MID$(a$, i, 1)
					xAdd = 0
				ELSE
					xSpace = xSpace + xAdd
				END IF
			   NEXT i
			   
			   IF itemIDX = -16 THEN itemText$ = "|"
			   			   
			   itemLen = LEN(itemText$) 
               INPUT #1, itemX
               itemX = itemX + fineTuneX + xSpace
               INPUT #1, itemY
               itemY = itemY + fineTuneY
               INPUT #1, itemW
               INPUT #1, itemH
               itemW = itemLen
               itemH = 4
               INPUT #1, itemColor2
               INPUT #1, itemColor1

               'PRINT itemText$
               'PRINT ofsDATA
			   
			   IF itemIDX < 0 THEN itemIDX = 255

               saveString = saveString + CHR$(itemIDX)
               saveString = saveString + CHR$(itemPrev)
               saveString = saveString + CHR$(itemNext)
               saveString = saveString + CHR$(itemX)
               saveString = saveString + CHR$(itemY)
               saveString = saveString + CHR$(itemW)
               saveString = saveString + CHR$(itemH)
                  
               byte = CHR$(itemColor1)
               saveString = saveString + byte
               byte = CHR$(itemColor2)
               saveString = saveString + byte
			   
               IF itemIDX > -1 THEN
                  byte = CHR$(itemLen)
                  saveString = saveString + byte
                  saveString = saveString + UCASE$(itemText$)
               END IF
            END IF
         LOOP WHILE itemIDX <> -1
         		 
		 saveString = CHR$(itemCount) + saveString		 
		 saveString = MKI$(LEN(saveString) + 2) + saveString
		 
		 IF menuNo > 9 THEN                        
			MID$(fileString, menuNo * 2 + 1, 2) = MKI$(LEN(fileString) + 1)
            'MID$(fileString, menuNo * 6 + 3, 2) = MKI$(itemCount)
            'MID$(fileString, menuNo * 6 + 5, 2) = MKI$(startItem)
            fileString = fileString + saveString
            PRINT "Streamed item added: "; menuNo
         ELSE
            MID$(prelString, menuNo * 2 + 1, 2) = MKI$(LEN(prelString) + 1)
            'MID$(prelString, menuNo * 6 + 3, 2) = MKI$(itemCount)
            'MID$(prelString, menuNo * 6 + 5, 2) = MKI$(startItem)
            prelString = prelString + saveString
            PRINT "Preload item added:  "; menuNo
         END IF
   LOOP WHILE NOT EOF(1)
   CLOSE #1

OPEN "menu.cmm" FOR BINARY AS #1
   n = LOF(1)
CLOSE #1
IF n THEN KILL "menu.cmm"
OPEN "menu.cmm" FOR BINARY AS #2
   PUT #2, 1, fileString
CLOSE #2

OPEN "menuprel.cmp" FOR BINARY AS #1
   n = LOF(1)
CLOSE #1
IF n THEN KILL "menuprel.cmp"
OPEN "menuprel.cmp" FOR BINARY AS #2
   PUT #2, 1, prelString
CLOSE #2

PRINT LEN(fileString)
PRINT LEN(prelString)


DEFINT A-Z
DIM ofsOFFSETS AS INTEGER
DIM ofsData AS INTEGER
DIM itemCount AS INTEGER


ofsOFFSETS = 1
ofsData = 385

DIM byte AS STRING * 1

DIM menuData AS STRING * 10000

CLS
OPEN "menu.cmm" FOR BINARY AS #1
 n = LOF(1)
CLOSE #1
IF n THEN KILL "menu.cmm"
OPEN "menu.cmm" FOR BINARY AS #2

   OPEN "..\convert\menus.txt" FOR INPUT AS #1
   DO
      INPUT #1, menuNo
      menuNo = menuNo - 1
      INPUT #1, startItem
      INPUT #1, fineTuneX
      INPUT #1, fineTuneY
      INPUT #1, clearX
      INPUT #1, clearY
      INPUT #1, clearW
      INPUT #1, clearH
      INPUT #1, clearchar$
      INPUT #1, clearAttr
         'PUT #2, ofsOFFSETS + menuNo * 6, ofsDATA
         'PUT #2, ofsOFFSETS + menuNo * 6 + 4, startItem
         MID$(menuData, menuNo * 2 + 1, 1) = CHR$(ofsData)
         MID$(menuData, menuNo * 2 + 2, 1) = CHR$(startItem)

         byte = CHR$(clearX + fineTuneX)
         MID$(menuData, ofsData, 1) = byte
         'PUT #2, ofsDATA, byte
            ofsData = ofsData + 1
         byte = CHR$(clearY + fineTuneY)
         PUT #2, ofsData, byte
            ofsData = ofsData + 1
         byte = CHR$(clearW)
         PUT #2, ofsData, byte
            ofsData = ofsData + 1
         byte = CHR$(clearH)
         PUT #2, ofsData, byte
            ofsData = ofsData + 1
         byte = clearchar$
         PUT #2, ofsData, byte
            ofsData = ofsData + 1
         byte = CHR$(clearAttr)
         PUT #2, ofsData, byte
            ofsData = ofsData + 1
         itemCount = 0
         DO
            INPUT #1, itemIDX
            IF itemIDX > -1 THEN
               itemCount = itemCount + 1
               PRINT itemIDX;
               INPUT #1, itemPrev
               INPUT #1, itemNext
               INPUT #1, itemText$
               itemText$ = itemText$ + "|"
               itemLen = LEN(itemText$)
               INPUT #1, itemX
               itemX = itemX + fineTuneX
               INPUT #1, itemY
               itemY = itemY + fineTuneY
               INPUT #1, itemW
               INPUT #1, itemH
               itemW = itemLen - 1
               itemH = 4
               INPUT #1, itemColor2
               INPUT #1, itemColor1

               'PRINT itemText$
               'PRINT ofsDATA

               dataWrite = 1
               dataMode = 0
               DO
                  SELECT CASE dataMode
                  CASE 0
                     PUT #2, ofsData, itemIDX
                     ofsData = ofsData + 2
                  CASE 1
                     PUT #2, ofsData, itemPrev
                     ofsData = ofsData + 2
                  CASE 2
                     PUT #2, ofsData, itemNext
                     ofsData = ofsData + 2
                  CASE 3
                     PUT #2, ofsData, itemX
                     ofsData = ofsData + 2
                  CASE 4
                     PUT #2, ofsData, itemY
                     ofsData = ofsData + 2
                  CASE 5
                     PUT #2, ofsData, itemW
                     ofsData = ofsData + 2
                  CASE 6
                     PUT #2, ofsData, itemH
                     ofsData = ofsData + 2
                  CASE 7
                     byte = CHR$(itemColor1)
                     PUT #2, ofsData, byte
                     ofsData = ofsData + 1
                  CASE 8
                     byte = CHR$(itemColor2)
                     PUT #2, ofsData, byte
                     ofsData = ofsData + 1
                  CASE 9
                     byte = CHR$(itemLen)
                     PUT #2, ofsData, byte
                     ofsData = ofsData + 1
                  CASE 10 TO 50
                     byte = UCASE$(MID$(itemText$, dataMode - 9, 1))
                     PUT #2, ofsData, byte
                     ofsData = ofsData + 1
                  END SELECT
                  dataMode = dataMode + 1
                  IF dataMode = itemLen + 10 THEN dataWrite = 0
               LOOP WHILE dataWrite
            END IF
         LOOP WHILE itemIDX > -1
         PUT #2, ofsOFFSETS + menuNo * 6 + 2, itemCount
   LOOP WHILE NOT EOF(1)
   CLOSE #1

CLOSE #2


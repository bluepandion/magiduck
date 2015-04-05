DEFINT A-Z
DIM ofsOFFSETS AS INTEGER
DIM ofsDATA AS INTEGER
DIM itemCount AS INTEGER


ofsOFFSETS = 1
ofsDATA = 385

DIM byte AS STRING * 1

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
      INPUT #1, clearX
      INPUT #1, clearY
      INPUT #1, clearW
      INPUT #1, clearH
      INPUT #1, clearchar$
      INPUT #1, clearAttr
         PUT #2, ofsOFFSETS + menuNo * 6, ofsDATA
         PUT #2, ofsOFFSETS + menuNo * 6 + 4, startItem
         byte = CHR$(clearX - 1)
         PUT #2, ofsDATA, byte
            ofsDATA = ofsDATA + 1
         byte = CHR$(clearY - 1)
         PUT #2, ofsDATA, byte
            ofsDATA = ofsDATA + 1
         byte = CHR$(clearW)
         PUT #2, ofsDATA, byte
            ofsDATA = ofsDATA + 1
         byte = CHR$(clearH + 1)
         PUT #2, ofsDATA, byte
            ofsDATA = ofsDATA + 1
         byte = clearchar$
         PUT #2, ofsDATA, byte
            ofsDATA = ofsDATA + 1
         byte = CHR$(clearAttr)
         PUT #2, ofsDATA, byte
            ofsDATA = ofsDATA + 1
         itemCount = 0
         DO
            INPUT #1, itemIdx
            IF itemIdx > -1 THEN
               itemCount = itemCount + 1
               PRINT itemIdx;
               INPUT #1, itemPrev
               INPUT #1, itemNext
               INPUT #1, itemText$
               itemLen = LEN(itemText$)
               INPUT #1, itemX
               INPUT #1, itemY
               INPUT #1, itemW
               INPUT #1, itemH
               INPUT #1, itemColor2
               INPUT #1, itemColor1

               'PRINT itemText$
               'PRINT ofsDATA

               dataWrite = 1
               dataMode = 0
               DO
                  SELECT CASE dataMode
                  CASE 0
                     byte = CHR$(itemIdx)
                  CASE 1
                     byte = CHR$(itemPrev)
                     PRINT itemPrev;
                  CASE 2
                     byte = CHR$(itemNext)
                     PRINT itemNext
                  CASE 3
                     byte = CHR$(itemX - 1)
                  CASE 4
                     byte = CHR$(itemY - 1)
                  CASE 5
                     byte = CHR$(itemW - 1)
                  CASE 6
                     byte = CHR$(itemH)
                  CASE 7
                     byte = CHR$(itemColor1)
                  CASE 8
                     byte = CHR$(itemColor2)
                  CASE 9
                     byte = CHR$(itemLen)
                  CASE 10 TO 50
                     byte = UCASE$(MID$(itemText$, dataMode - 9, 1))
                  END SELECT
                  PUT #2, ofsDATA, byte
                  ofsDATA = ofsDATA + 1
                  dataMode = dataMode + 1
                  IF dataMode = itemLen + 10 THEN dataWrite = 0
               LOOP WHILE dataWrite
            END IF
         LOOP WHILE itemIdx > -1
         PUT #2, ofsOFFSETS + menuNo * 6 + 2, itemCount
   LOOP WHILE NOT EOF(1)
   CLOSE #1

CLOSE #2


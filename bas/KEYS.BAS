'
' Show keyboard scan-codes by pressing keys.
'
DO
LOCATE 1, 1: PRINT INP(&H60)
LOOP WHILE INKEY$ <> CHR$(27)


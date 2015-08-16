a$ = STRING$(128 * 3, 0)
OPEN "savegame.sav" FOR OUTPUT AS #1
 PRINT #1, a$
CLOSE #1


'
' Fill high-score file with default data.
'

TYPE scoreType
 name AS STRING * 8
 score AS LONG
END TYPE

TYPE scoretable
 entry(0 TO 9) AS scoreType
END TYPE

DIM scores AS scoretable

FOR n = 0 TO 9
 scores.entry(n).name = CHR$(0) + CHR$(1) + CHR$(2) + CHR$(3) + CHR$(4) + CHR$(5) + CHR$(6) + CHR$(7)
 scores.entry(n).score = 12345678
NEXT n

OPEN "hiscores.def" FOR BINARY AS #1
 PUT #1, 1, scores
CLOSE #1


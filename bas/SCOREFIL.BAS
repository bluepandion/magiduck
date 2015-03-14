'
' Fill high-score file with default data.
'

TYPE scoreType
 name AS STRING * 3
 score AS LONG
END TYPE

TYPE scoretable
 entry(0 TO 9) AS scoreType
END TYPE

DIM scores AS scoretable

FOR n = 0 TO 9
 scores.entry(n).name = "ABC"
 scores.entry(n).score = 1234567
NEXT n

OPEN "hiscores.def" FOR BINARY AS #1
 PUT #1, 1, scores
CLOSE #1


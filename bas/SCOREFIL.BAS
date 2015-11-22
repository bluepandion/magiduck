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


scores.entry(0).name = "CYGNUS"
scores.entry(0).score = 50000
scores.entry(1).name = "ID"
scores.entry(1).score = 100000
scores.entry(2).name = "TREALITY"
scores.entry(2).score = 200000
scores.entry(3).name = "ECLIPSE"
scores.entry(3).score = 300000
scores.entry(4).name = "LUCAS"
scores.entry(4).score = 400000
scores.entry(5).name = "EPIC"
scores.entry(5).score = 500000
scores.entry(6).name = "APOGEE"
scores.entry(6).score = 1000000
scores.entry(7).name = "PAKU"
scores.entry(7).score = 2000000
scores.entry(8).name = "COMPUTER"
scores.entry(8).score = 3000000
scores.entry(9).name = "VINTAGE"
scores.entry(9).score = 4000000


OPEN "hiscores.def" FOR BINARY AS #1
 PUT #1, 1, scores
CLOSE #1


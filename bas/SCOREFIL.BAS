'
' Fill high-score file with default data.
'

DECLARE SUB Scramble (n)

TYPE scoreType
 name AS STRING * 8
 score AS LONG
END TYPE

TYPE scoretable
 entry(0 TO 9) AS scoreType
END TYPE

DIM SHARED scores AS scoretable


scores.entry(0).name = "CYGNUS"
scores.entry(0).score = 100000
scores.entry(1).name = "TREALITY"
scores.entry(1).score = 200000
scores.entry(2).name = "ID"
scores.entry(2).score = 300000
scores.entry(3).name = "ECLIPSE"
scores.entry(3).score = 400000
scores.entry(4).name = "LUCAS"
scores.entry(4).score = 500000
scores.entry(5).name = "EPIC"
scores.entry(5).score = 600000
scores.entry(6).name = "APOGEE"
scores.entry(6).score = 700000
scores.entry(7).name = "PAKU"
scores.entry(7).score = 800000
scores.entry(8).name = "COMPUTER"
scores.entry(8).score = 1000000
scores.entry(9).name = "VINTAGE-"
scores.entry(9).score = 2000000

FOR n = 0 to 9
	Scramble n
next n

OPEN "hiscores.def" FOR BINARY AS #1
 PUT #1, 1, scores
CLOSE #1

SUB Scramble (n)
	FOR c = 1 TO 8
		d = c + n + 65
		MID$(scores.entry(n).name, c, 1) = CHR$(ASC(MID$(scores.entry(n).name, c, 1)) XOR d)
	NEXT c
END SUB
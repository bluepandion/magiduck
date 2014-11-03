CLS
PRINT -15 AND &H800F
PRINT HEX$(-15)

FOR A = -25 TO 25 STEP 1
'PRINT A, A XOR (A < 0)
'PRINT ABS(A) AND &HF 'XOR (A < 0)
PRINT (A < -25) + (A < -20) + (A < -10) + (A < 0) + (A > 0) + (A > 0) + (A > 10) + (A > 20) + (A > 25) XOR (A > 0);

NEXT A


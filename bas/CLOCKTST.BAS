DECLARE FUNCTION CLOCK& ()

DIM SHARED ticks AS LONG

CLS

DO
 LOCATE 1, 1
 PRINT CLOCK&
 PRINT TIMER
 PRINT TIME$
LOOP WHILE INKEY$ = ""

CLS

t# = TIMER
tick = 0
DO
OUT &H43, &H4
lsb = INP(&H40)
hsb = INP(&H40)
IF hsb > lasthsb THEN tick = tick + 1
lasthsb = hsb
IF lsb > lastlsb THEN tickl = tickl + 1
lastsb = lsb
LOCATE 1, 1
PRINT HEX$(hsb * 255), HEX$(lsb)
PRINT tick, tickl
PRINT TIMER
tl& = TIMER * 1000
PRINT tl&
LOOP WHILE INKEY$ = ""

CLS

tick = 0
DO
OUT &H43, &H4
lsb = 255 - INP(&H40)
hsb = INP(&H40)

tl& = INT(TIMER * 10) * 1000 + lsb
tick = 1
'IF lsb > 230 THEN tick = 1
IF tick THEN PRINT tl&, hsb, lsb

LOOP WHILE INKEY$ = ""

FUNCTION CLOCK&

' Get the number of timer ticks at
' 0000:046C
DEF SEG = 0
ticks = PEEK(&H46C)
ticks = 0& + ticks + PEEK(&H46D) * 256&
ticks = 0& + ticks + PEEK(&H46E) * 65536
DEF SEG

' Latch the counter and obtain the PIT
' countdown status.
OUT &H43, &H4
lsb = INP(&H40)
hsb = 255 - INP(&H40)

' Compute the CLOCK& value
CLOCK& = ticks * 256 + hsb

END FUNCTION


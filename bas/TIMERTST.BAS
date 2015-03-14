'$INCLUDE: 'libelf.bi'

DIM debugOut(0 TO 1) AS INTEGER

CLS

CALL aTimerStart(VARSEG(debugOut(0)), VARPTR(debugOut(0)))

LOCATE 10, 1

PRINT VARSEG(debugOut(0))
PRINT VARPTR(debugOut(0))

PRINT HEX$(debugOut(0))
PRINT HEX$(debugOut(1))

PRINT debugOut(0)
PRINT debugOut(1)
aTimerReset

t = 0
DIM tt AS DOUBLE
tt = TIMER

DO
 CALL aTimerWait
 LOCATE 1, 1: PRINT t
 LOCATE 2, 1: PRINT TIMER
 LOCATE 3, 1: PRINT TIME$
 t = t + 1
 IF t > 23 THEN
  LOCATE 7, 1:
  PRINT 0& + TIMER - tt
  t = 0: tt = TIMER
  'PRINT debugOut(0)
  'PRINT debugOut(1)
 END IF
LOOP WHILE INKEY$ = ""

CALL aTimerEnd

DO
 LOCATE 2, 1: PRINT TIMER
 LOCATE 3, 1: PRINT TIME$
LOOP WHILE INKEY$ = ""


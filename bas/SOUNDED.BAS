'$INCLUDE: 'libelf.bi'
DECLARE SUB queueSound (offset%, Note%)
DECLARE SUB playSounds ()
DECLARE SUB initLookUps ()
DECLARE SUB initTimer ()

initLookUps
initTimer

sounds.readPos = 0

sounds.speakerOn = INP(&H61) OR 3
sounds.speakerOff = sounds.speakerOn XOR 3

CLS


DIM viewx(0 TO 255) AS INTEGER
DIM viewy(0 TO 255) AS INTEGER
DIM noteTable(0 TO 96) AS STRING * 5

noteTable(0) = "-----"
o = 0
FOR n = 1 TO 96 STEP 12
 noteTable(n) = "C-" + STR$(o)
 noteTable(n + 1) = "C#" + STR$(o)
 noteTable(n + 2) = "D-" + STR$(o)
 noteTable(n + 3) = "D#" + STR$(o)
 noteTable(n + 4) = "E " + STR$(o)
 noteTable(n + 5) = "F-" + STR$(o)
 noteTable(n + 6) = "F#" + STR$(o)
 noteTable(n + 7) = "G-" + STR$(o)
 noteTable(n + 8) = "G#" + STR$(o)
 noteTable(n + 9) = "A-" + STR$(o)
 noteTable(n + 10) = "A#" + STR$(o)
 noteTable(n + 11) = "B " + STR$(o)
 o = o + 1
NEXT n
noteTable(96) = " END "

sounds.track(255) = 96

WIDTH 80, 25

selection = 0
octave = 0
playMode = 0
playReturn = 0

reFresh:
COLOR 7, 0
X = 4
Y = 2
FOR n = 0 TO 255
 LOCATE Y, X: PRINT noteTable(sounds.track(n))
 viewx(n) = X
 viewy(n) = Y
 Y = Y + 1
 IF Y = 24 THEN Y = 2: X = X + 6
NEXT n

COLOR 9, 0
LOCATE 1, 1: PRINT "Octave"
LOCATE 1, 15: PRINT "Pos"

t! = TIMER

DO
 IF TIMER > t! THEN
  t! = 0! + TIMER + .3
  playSounds
  IF playMode THEN
   sounds.readPos = 0
   sounds.queue(0) = sounds.track(selection)
   COLOR 7, 0
   LOCATE viewy(selection), viewx(selection): PRINT noteTable(sounds.track(selection))
   selection = selection + 1
   IF sounds.track(selection) = 96 THEN playMode = 0: selection = playReturn
  END IF
 END IF

 COLOR 15, 0
  LOCATE 1, 10: PRINT octave
  LOCATE 1, 20: PRINT selection

 COLOR 0, 7
  LOCATE viewy(selection), viewx(selection): PRINT noteTable(sounds.track(selection))

 d$ = INKEY$
  IF d$ = CHR$(0) + CHR$(83) THEN
   n = selection
   DO
    n = n + 1
    sounds.track(n - 1) = sounds.track(n)
   LOOP UNTIL sounds.track(n) = 96
   GOTO reFresh
  END IF

  IF d$ = CHR$(27) THEN
   'Disconnect speaker from Timer 2
   OUT &H61, sounds.speakerOff

   OUT &H43, &H34         'Return Timer to normal operation
   OUT &H40, 0            'Set low 8 bits of trigger to 0
   OUT &H40, 0            'Set hi  8 bits of trigger to 0
   COLOR 7, 0
   CLS
   END
  END IF

 IF d$ = CHR$(13) THEN
   IF playMode THEN
     COLOR 7, 0
     LOCATE viewy(selection), viewx(selection): PRINT noteTable(sounds.track(selection))
     selection = playReturn
    ELSE
     playReturn = selection
   END IF
  playMode = playMode XOR 1
 END IF

 IF d$ = "," AND octave > 0 THEN octave = octave - 1
 IF d$ = "." AND octave < 6 THEN octave = octave + 1

 addNote = 0
 IF d$ = "z" THEN addNote = octave * 12 + 1
 IF d$ = "s" THEN addNote = octave * 12 + 2
 IF d$ = "x" THEN addNote = octave * 12 + 3
 IF d$ = "d" THEN addNote = octave * 12 + 4
 IF d$ = "c" THEN addNote = octave * 12 + 5
 IF d$ = "v" THEN addNote = octave * 12 + 6
 IF d$ = "g" THEN addNote = octave * 12 + 7
 IF d$ = "b" THEN addNote = octave * 12 + 8
 IF d$ = "h" THEN addNote = octave * 12 + 9
 IF d$ = "n" THEN addNote = octave * 12 + 10
 IF d$ = "j" THEN addNote = octave * 12 + 11
 IF d$ = "m" THEN addNote = octave * 12 + 12

 IF d$ = "q" THEN addNote = octave * 12 + 13
 IF d$ = "2" THEN addNote = octave * 12 + 14
 IF d$ = "w" THEN addNote = octave * 12 + 15
 IF d$ = "3" THEN addNote = octave * 12 + 16
 IF d$ = "e" THEN addNote = octave * 12 + 17
 IF d$ = "r" THEN addNote = octave * 12 + 18
 IF d$ = "5" THEN addNote = octave * 12 + 19
 IF d$ = "t" THEN addNote = octave * 12 + 20
 IF d$ = "6" THEN addNote = octave * 12 + 21
 IF d$ = "y" THEN addNote = octave * 12 + 22
 IF d$ = "7" THEN addNote = octave * 12 + 23
 IF d$ = "u" THEN addNote = octave * 12 + 24
 
 IF d$ = "p" THEN addNote = octave * 12 + INT(RND * 24)
                                             
 IF d$ = " " THEN addNote = 0
 IF d$ = "-" THEN addNote = 96

 IF addNote OR d$ = " " THEN
  sounds.track(selection) = addNote
  sounds.readPos = 0
  sounds.queue(0) = addNote
  t& = 0
  COLOR 7, 0
  LOCATE viewy(selection), viewx(selection): PRINT noteTable(sounds.track(selection))
  IF selection < 254 THEN selection = selection + 1
 END IF
 
 IF d$ = CHR$(0) + "H" OR d$ = CHR$(0) + "P" OR d$ = CHR$(0) + "M" OR d$ = CHR$(0) + "K" THEN
  COLOR 7, 0
  LOCATE viewy(selection), viewx(selection): PRINT noteTable(sounds.track(selection))
  IF d$ = CHR$(0) + "H" AND selection > 0 THEN selection = selection - 1
  IF d$ = CHR$(0) + "P" AND selection < 254 THEN selection = selection + 1
  IF d$ = CHR$(0) + "K" AND selection > 21 THEN selection = selection - 22
  IF d$ = CHR$(0) + "M" AND selection < 233 THEN selection = selection + 22
 END IF


 IF d$ = "+" THEN
  LOCATE 1, 60: PRINT "Saved  "; TIME$
  OPEN "sound.stk" FOR BINARY AS #1
   PUT #1, 1, sounds
  CLOSE #1
 END IF

 IF d$ = "*" THEN
  COLOR 7, 0
  LOCATE 1, 60: PRINT "Loaded "; TIME$

  OPEN "sound.stk" FOR BINARY AS #1
   GET #1, 1, sounds
  CLOSE #1

  GOTO reFresh
 END IF


LOOP



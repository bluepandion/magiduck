OPEN "floats.clu" FOR BINARY AS #2
 ofs = 1
   OPEN "floats.txt" FOR INPUT AS #1
      FOR ff = 0 TO 9
         FOR n = 0 TO 9
            INPUT #1, v
            FOR f = 0 TO 3

               'fraction(f * 10 + ff, n) = f + v
               'fraction(f * -10 - ff, n) = (f + v) * -1
               PUT #2, ofs,f*10+ff

               ofs = ofs + 2
            NEXT f
         NEXT n
      NEXT ff
   CLOSE #1

CLOSE #2


DEFINT A-Z
GOTO sync
overflow = INP(&H3D5)
PRINT "overflow bits           "; overflow
vt8 = ((overflow AND &H1) <> 0) * -256
vt9 = ((overflow AND &H20) <> 0) * -512
vrs8 = ((overflow AND &H4) <> 0) * -256
vrs9 = ((overflow AND &H80) <> 0) * -512
OUT &H3D4, &H6
vtotal = INP(&H3D5)
PRINT "vertical total          "; vtotal + vt8 + vt9
OUT &H3D4, &H10
vrstart = INP(&H3D5)
PRINT "vertical retrace start: "; vrstart + vrs8 + vrs9

OUT &H3D4, &H10
OUT &H3D5, 0

sync:

  OUT &H3D4, &H9            'Maximum scan line
  OUT &H3D5, &H83           'Bit 7 true = 200 to 400 scanline conversion
                            'Bits 3-0   = Glyph height

  OUT &H3D4, &H6            'Vertical total
  OUT &H3D5, &H6C

  OUT &H3D4, &H7            'Overflow
  OUT &H3D5, &H1F

  OUT &H3D4, &H8            'Preset row SC
  OUT &H3D5, &H0

  OUT &H3D4, &H10           'Vert retrace start
  OUT &H3D5, &H5E

  OUT &H3D4, &H11           'Vert retrace end
  OUT &H3D5, &H2B

  OUT &H3D4, &H12           'Vert display end
  OUT &H3D5, &H5D

  OUT &H3D4, &H15           'Start vBlank
  OUT &H3D5, &H5E

  OUT &H3D4, &H16           'End vBlank
  OUT &H3D5, &HA


DEF SEG = &HB800
n = 0
DO
'PRINT INP(&H3DA);
POKE n, 219
POKE n + 1, INP(&H3DA)
n = n + 2
n = n AND 4095
LOOP WHILE INKEY$ = ""


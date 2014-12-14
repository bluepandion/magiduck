SCREEN 13
CLS

OUT &H3D4, &H7
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

OUT &H3D4, &H11
vrend = INP(&H3D5)
PRINT "vertical retrace end:   "; vrend AND &H15
vre = overflow AND &H15
vt9 = ((overflow AND &H20) <> 0) * -512
vrs8 = ((overflow AND &H4) <> 0) * -256
vrs9 = ((overflow AND &H80) <> 0) * -512


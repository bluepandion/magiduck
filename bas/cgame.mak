c:\qb71\binb\bc.exe bgame.bas bgame.obj /O /FPi /Ot /Lr /Fs /S
c:\qb71\binb\bc.exe libelf19.bas libelfb.obj /O /FPi /Ot /Lr /Fs /S
c:\qb71\binb\bc.exe dgame14.bas dgame.obj /O /FPi /Ot /Lr /Fs /S

bcoptions = /O /FPi /Ot /Lr /Fs /S
objfiles = bgame.obj libelfb.obj dgame.obj libelf.obj
stubfiles = noedit.obj+nocom.obj+nolpt.obj+smallerr.obj+nograph.obj+noevent.obj+noisam.obj
options = 
duck.exe : bgame.obj libelfb.obj dgame.obj
link duck.obj 
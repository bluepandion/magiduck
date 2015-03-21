@echo off

c:\qb71\binb\bc.exe bgame.bas bgame.obj nul.lst /O /FPi /Ot /Lr /Fs /S
c:\qb71\binb\bc.exe libelf19.bas libelfb.obj nul.lst /O /FPi /Ot /Lr /Fs /S
c:\qb71\binb\bc.exe dgame14.bas dgame.obj nul.lst /O /FPi /Ot /Lr /Fs /S

rem c:\qb71\binb\link.exe /EX /NOE /NOD:BRT71FR.LIB libelf13.obj+dgame10.obj+noedit.obj+nocom.obj+nolpt.obj+smallerr.obj+tscniofr.obj,duck.exe,nul.map,c:\qb71\lib\bc71efr.lib+c:\qb71\lib\qbx.lib
rem c:\qb71\binb\link.exe /EX /NOE /NOD:BRT71FR.LIB libelf13.obj+dgame10.obj,duck.exe,nul.map,c:\qb71\lib\qbx.lib

rem +c:\qb71\lib\qbx.lib

rem c:\qb71\binb\link.exe bgame+libelfb+dgame+libelf+noedit+nocom+nolpt+smallerr+nograph+noevent+noisam,duck,nul,BCL71EFR+libelf /EX /NOE /NOD:BRT71FR.LIB





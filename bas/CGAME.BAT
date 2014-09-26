@echo off

set toolPath=\qb71\myTools\
set destPath=\elf\gitduck\bas\
set sourcePath=c:\elf\gitduck\bas\
set link=\qb71\binb\link.exe
set bc=\qb71\binb\bc.exe
set linkFiles=bgame.obj+libelf19.obj+dgame.obj+libelf.obj+noedit.obj+nocom.obj+nolpt.obj+smallerr.obj,duck.exe,nul.map,c:\qb71\lib\BCL71EFR.LIB+c:\elf\gitduck\bas\libelf.lib

@echo on

copy libelf.lib c:%toolPath%
copy libelf.obj c:%toolPath%

cd%toolPath%

bc.exe %sourcePath%bgame.bas bgame.obj /O /FPi /Ot /Lr /Fs /S
bc.exe %sourcePath%libelf19.bas libelf19.obj /O /FPi /Ot /Lr /Fs /S
bc.exe %sourcePath%dgame14.bas dgame.obj /O /FPi /Ot /Lr /Fs /S

REM c:\qb71\binb\link.exe /EX /NOE /NOD:BRT71FR.LIB libelf13.obj+dgame10.obj+noedit.obj+nocom.obj+nolpt.obj+smallerr.obj+tscniofr.obj,duck.exe,nul.map,c:\qb71\lib\bc71efr.lib+c:\qb71\lib\qbx.lib
REM c:\qb71\binb\link.exe /EX /NOE /NOD:BRT71FR.LIB libelf13.obj+dgame10.obj,duck.exe,nul.map,c:\qb71\lib\qbx.lib

link.exe /EX /NOE /NOD:BRT71FR.LIB 

copy duck.exe %sourcePath%

cd%destPath%



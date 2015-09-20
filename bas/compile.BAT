@echo off

set compile=...\compile\
set gamedir=..\gitduck\bas\
set linker=c:\qb71\binb\link

c:\qb71\binb\bc.exe %1.bas %1.obj nul.lst /O /FPi /Ot /Lr /Fs /S
copy %1.obj %compile%

del %1.obj

cd%compile%

@echo on

%linker% %1, %1, NUL,BCL71EFR /EX /NOE /NOD:BRT71FR.LIB

@echo off

cd%gamedir%

copy %compile%%1.exe
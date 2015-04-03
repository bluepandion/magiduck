@echo off
set compile=...\compile\
set gamedir=..\gitduck\bas\
set linker=c:\qb71\binb\link

copy dgame.obj %compile%
copy libelfb.obj %compile%
copy libelf.obj %compile%
copy bgame.obj %compile%

cd%compile%

@echo on

%linker% bgame+libelfb+dgame+libelf+noedit+nocom+nolpt+smallerr+nograph+noisam,duck,NUL,BCL71EFR+libelf /EX /NOE /NOD:BRT71FR.LIB

@echo off

cd%gamedir%

copy %compile%duck.exe


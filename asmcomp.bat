@echo off 
set disk=c:
set fileOut=libElf
set file0=c:\elf\gitDuck\asm\libElf.asm
set dest=\elf\gitDuck\
set masm=cd\masm611\bin\
set link=cd\bc7\binb\

@echo on

%masm%
ml %file0% 

copy %fileOut%.obj %dest%
del %fileOut%.exe

cd%dest%
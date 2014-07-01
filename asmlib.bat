@echo off 
set disk=c:
set fileOut=libElf
set file0=c:\elf\gitDuck\asm\pageflip.asm
set file1=c:\elf\gitDuck\asm\kbinit02.asm
set file2=c:\elf\gitDuck\asm\rectlst2.asm
set file3=c:\elf\gitDuck\asm\sprlist7.asm
set file4=c:\elf\gitDuck\asm\tile_801.asm
set file5=c:\elf\gitDuck\asm\tilemov0.asm
set file6=c:\elf\gitDuck\asm\uptile00.asm
set file7=c:\elf\gitDuck\asm\clrlist1.asm
set dest=\elf\gitDuck\
set masm=cd\masm611\bin\
set link=cd\bc7\binb\

@echo on

%masm%
ml %file0%,%file1%,%file2%,%file3%,%file4%,%file5%,%file6% /c
copy %fileOut%.obj %dest%

%link%
link /q %dest%%fileOut%.obj, %dest%%fileOut%.qlb, ,c:\bc7\lib\qbx.lib c:\bc7\lib\qbxqlb.lib;

cd%dest%
copy %fileOut%.qlb .\bas
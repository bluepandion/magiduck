@echo off 
set disk=c:
set fileOut=libElf
set file0=c:\elf\gitDuck\asm\pageFlip.asm
set file1=c:\elf\gitDuck\asm\kbInit.asm
set file2=c:\elf\gitDuck\asm\rectList.asm
set file3=c:\elf\gitDuck\asm\sprList.asm
set file4=c:\elf\gitDuck\asm\tileArea.asm
set file5=c:\elf\gitDuck\asm\tilePan.asm
set file6=c:\elf\gitDuck\asm\tileDraw.asm
set file7=c:\elf\gitDuck\asm\clrList.asm
set dest=\elf\gitDuck\
set masm=cd\masm611\bin\
set link=cd\bc7\binb\

@echo on

%masm%
ml %file0%,%file1%,%file2%,%file3%,%file4%,%file5%,%file6%,%file7% /c
copy %fileOut%.obj %dest%

%link%
link /q %dest%%fileOut%.obj, %dest%%fileOut%.qlb, ,c:\bc7\lib\qbx.lib c:\bc7\lib\qbxqlb.lib;

cd%dest%
copy %fileOut%.qlb .\bas
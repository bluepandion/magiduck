@echo off 
set file=libElf
set fileList=c:\elf\duckGit\asm\pageflip.asm,c:\elf\duckGit\asm\tile_801.asm,c:\elf\duckGit\asm\
set src=c:\elf\duckGit\asm\libElf.asm
set dest=c:\elf\duckGit\
set lib=c:\elf\duckGit\bas
set masm=cd\masm611\bin\
set link=cd\bc7\binb\

@echo on

%masm%
ml c:\elf\duckGit\asm\libelf.asm /c
copy %file%.obj %dest%

%link%
link /q %dest%%file%.obj, %dest%%file%.qlb, ,c:\bc7\lib\qbx.lib c:\bc7\lib\qbxqlb.lib;

cd\elf\duckGit
copy %file%.qlb .\bas
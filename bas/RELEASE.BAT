copy *.clp ..\release
copy *.ctp ..\release
copy *.csd ..\release
copy *.csp ..\release
copy sound.cst ..\release
....\compack\compack duck.exe c:\elf\gitduck\release\duck.exe
savegclr
scorefil
menuconv
copy savegame.sav ..\release
copy hiscores.def ..\release
copy w40char.clu ..\release
copy floats.clu ..\release
copy sincos.clu ..\release
copy *.cmm ..\release
copy *.cmp ..\release


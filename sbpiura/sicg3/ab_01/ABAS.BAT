@ECHO OFF
CLS
SET WORKER=OFF

act.exe "abas.exe"

abas.exe

DEL *.IDX
DEL *.TMP
del *.lst
del *.dbf

@ECHO ON

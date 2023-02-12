@ECHO OFF
CLS
SET WORKER=OFF

act.exe "cpresu.exe"

cpresu.EXE

DEL *.IDX
DEL *.TMP
del *.lst
del *.dbf

@ECHO ON

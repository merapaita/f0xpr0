@ECHO OFF
CLS
SET WORKER=OFF

act.exe "teso.exe"

teso.EXE

DEL *.IDX
DEL *.TMP
del *.lst
del *.dbf

@ECHO ON

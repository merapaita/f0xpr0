@ECHO OFF
CLS
SET WORKER=OFF

act.exe "contab.exe"

contab.EXE

DEL *.IDX
DEL *.TMP
del *.lst
del *.dbf

@ECHO ON

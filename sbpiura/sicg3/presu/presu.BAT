@ECHO OFF
CLS
SET WORKER=OFF

act.exe "presu.exe"

presu.EXE

DEL *.IDX
DEL *.TMP
del *.lst
del *.dbf

@ECHO ON

@ECHO OFF
CLS
SET WORKER=OFF

act.exe "comven.exe"

comven.EXE

DEL *.IDX
DEL *.TMP
del *.lst
del *.dbf

@ECHO ON

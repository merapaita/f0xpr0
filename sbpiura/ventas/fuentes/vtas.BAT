@ECHO OFF
CLS
SET WORKER=OFF

act.exe "ventas.exe"

ventas.EXE

DEL *.IDX
DEL *.TMP
del *.lst
del *.dbf

@ECHO ON

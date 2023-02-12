@ECHO OFF
CLS
SET WORKER=OFF

rem net use  h: \\srver2\benedata

rem act.exe "Kardex.exe"

Kardex.Exe

DEL *.IDX
DEL *.TMP
@ECHO ON

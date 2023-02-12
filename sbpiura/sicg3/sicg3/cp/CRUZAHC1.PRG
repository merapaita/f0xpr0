*----------------------------------
* Program que cruza los 
* la informaci¢n del total de la hoja de control 
* contra lo que suma su hijo
* FMC 
*----------------------------------

SET TALK ON
USE HOJCON IN 1 ORDER TAG Hojcon1 ALIAS Hojcon
USE ITEHC  IN 2 ORDER TAG Itehc1  ALIAS Itehc
SET DEVICE TO FILE KKKK.TXT

I=1
vtotal=0
vanula=0

SELE Hojcon
SET FILTER TO SUBSTR(CODCAL,8,2)='02'
GO TOP

SCAN
    SELE Itehc
    SEEK Hojcon.Nummes+Hojcon.Numhc
    SCAN WHILE Hojcon.Nummes+Hojcon.Numhc=Itehc.Nummes+Itehc.Numhc
         IF TIPOPE='-' 
            vtotal=vtotal-itehc.Valpart
            vanula=vanula+itehc.Valpart
         ELSE
            vtotal=vtotal+itehc.Valpart
         ENDIF   
         SELE ITEHC
    ENDSCAN
    IF Hojcon.Imptot<>vtotal
        @ I,1 SAY HOJCON.NUMMES+' '+HOJCON.NUMHC+' '+STR(HOJCON.IMPTOT,15,2)+' '+STR(VTOTAL,15,2)+' '+STR(Vanula,15,2)
        I=I+1
    ENDIF
    vtotal=0
    vanula=0

	SELE Hojcon
ENDSCAN
CLOS DATA
SET DEVICE TO SCREEN
RETURN

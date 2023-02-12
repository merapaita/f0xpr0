*----------------------------------
* Program que cruza los 
* calendarios para determinar el 
* error de las hojas de control  
* FMC 
*----------------------------------
SET TALK ON
USE HOJCON IN 1 ORDER TAG Hojcon1 ALIAS Hojcon
USE ITEHC  IN 2 ORDER TAG Itehc1  ALIAS Itehc
SET DEVICE TO FILE JJJJ.TXT

I=1
SELE Hojcon
GO TOP

SCAN
    SELE Itehc
    SEEK Hojcon.Nummes+Hojcon.Numhc
    SCAN WHILE Hojcon.Nummes+Hojcon.Numhc=Itehc.Nummes+Itehc.Numhc
         DO CASE 
            CASE TIPFUN='F' 
                 IF ALLTRIM(HOJCON.Codcal)<>ALLTRIM('96'+Itehc.Nummes+Itehc.Codfte+Itehc.Codprg+Itehc.Codsubpr+Itehc.Codact)
                    @ I,1 SAY Hojcon.Nummes+' '+Hojcon.Numhc+' '+Itehc.Nummes+' '+Itehc.Numhc+' '+ALLTRIM(HOJCON.Codcal)+' '+'96'+Itehc.Nummes+Itehc.Codfte+Itehc.Codprg+Itehc.Codsubpr+Itehc.Codact+' '+Hojcon.Tipfun+' '+Itehc.Tipfun+' '+itehc.NUMHM
					I=I+1
                 ENDIF
            CASE TIPFUN='I' 
                 IF ALLTRIM(HOJCON.Codcal)<>ALLTRIM('96'+Itehc.Nummes+Itehc.Codfte+Itehc.Codprg+Itehc.Codsubpr+Itehc.Codproy+Itehc.Codsupry)
                    @ I,1 SAY Hojcon.Nummes+' '+Hojcon.Numhc+' '+Itehc.Nummes+' '+Itehc.Numhc+' '+ALLTRIM(HOJCON.Codcal)+' '+'96'+Itehc.Nummes+Itehc.Codfte+Itehc.Codprg+Itehc.Codsubpr+Itehc.Codproy+Itehc.Codsupry+' '+Hojcon.Tipfun+' '+Itehc.Tipfun+' '+itehc.NUMHM
                    I=I+1
                 ENDIF
         ENDCASE
         SELE ITEHC
    ENDSCAN
    
	SELE Hojcon
ENDSCAN
CLOS DATA
SET DEVICE TO SCREEN
RETURN

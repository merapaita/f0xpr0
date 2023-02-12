** ----------------------------------
** ERRAUX.PRG
** Reporte de errores de tipo de auxiliar
** AUTOR : UDEP - OAOC - 1995
** ----------------------------------

DO CreaTemporal

* Abrir Archivos de trabajo
USE Diario   IN 1 ORDER TAG Diario4                   ALIAS Diar
USE Auxil    IN 2 ORDER TAG Auxil1                    ALIAS Auxi
USE Temporal IN 3 ALIAS Temp
USE Parmae   IN 4 ORDER TAG parmae1                   ALIAS parma
USE Compro   IN 5 ORDER TAG compro4                   ALIAS compro

SELE 1
SET RELATION TO comprob INTO 5

DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
  DEFINE WINDOW MesAno FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE ' °° Errores por Tipo de Auxiliar °° ' COLOR SCHEME 5
  ACTIVATE WINDOW MesAno
  SELECT Parma
  SEEK "MESANO"+"ACTUA"
  vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))
  xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
  xAno     = IIF(FOUND(), VAL(SUBSTR(STR(YEAR(vmesact),4),3,2)), VAL(SUBSTR(STR(YEAR(DATE()),4),3,2)))
  @ 1,01 SAY '                Mes : ' GET xMes PICTURE '99' VALID xMes > 0 .AND. xMes <= 12
  @ 3,01 SAY '                A¤o : ' GET xAno PICTURE '99'
  @ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
  READ
  HIDE WINDOW MesAno


  IF OKCANCEL=1 .AND. LASTKEY()<>27
     SELECT Diar
  	 DO ESPERA WITH 1,"Errores por Tipo de Auxiliar en proceso ..."
	 DO CreaArch 
     SELE 3
     DO ESPERA WITH 2
     GO TOP
     IF EOF()
        DO STANDBY WITH "No hay registros para procesar."
     ELSE
        DO REPORTE WITH 2,"ErrAux"," Documentos Errados ",1,.f.,.t.
     ENDIF
  ENDIF
  CLOSE DATA
  DEACTIVATE WINDOW MesAno
RETURN



PROCEDURE Salida
*---------------
CLOSE DATABASE
*!DEL temporal.*
ERASE TEMPORAL.*
RELEASE    WINDOW Consis
ACTIVATE   SCREEN
RETURN


PROCEDURE CreaTemporal
*---------------------
CREATE TABLE Temporal ;
	(comprob C(14), comadi C(2), codcal C(19), fecemi D(8), cuenta C(10), ;
	tipo C(1), monto N(18,3), codctc C(14))
RETURN

PROCEDURE CreaArch
*------------------
STORE SPACE(2) TO vtipo
STORE SPACE(6) TO vregistro

SELE 3
INDEX ON ALLTRIM(comprob)+ALLTRIM(comadi) TAG tempo UNIQUE 
SET ORDER TO TAG 1
  
SELE Diar
SET FILTER TO MONTH(fecemi)=xMes 
GO TOP
SCAN
	vtipo=tipaux
	vregistro=registro
	SELE Auxi
	IF !SEEK(vtipo+vregistro)
		SELE 3
		APPEND BLANK
		REPLACE comprob WITH Diar.comprob
		REPLACE comadi  WITH Diar.comadi
		REPLACE codcal  WITH Compro.codcal
		REPLACE fecemi  WITH Diar.fecemi
		REPLACE cuenta  WITH Diar.cuenta
		REPLACE tipo    WITH Diar.tipo   
		REPLACE monto   WITH Diar.montos 
		REPLACE codctc   WITH Diar.Codctc
	ENDIF
	SELE Diar 	
ENDSCAN
RETURN

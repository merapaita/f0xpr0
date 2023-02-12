* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ Ftot5.PRG  30/09/96                                 L: 0047  ณ	
* ณ Emisiขn del T-5C                                             ณ
* ณ AUTOR   : CWL - UDEP                                         ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
PARAMETER TMES,TANO
USE IN 1
USE IN 2
USE IN 4
*ABRIr Archivos de trabajo
USE CAJAS   IN 1 ORDER TAG cajas8        ALIAS caja
USE BCOCTA  IN 2 ORDER TAG bcocta1       ALIAS bcocta
  IF LASTKEY()#27 AND okcancel = 1
	DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
	ACTIVATE WINDOW Xwait
	@0,0 SAY " Espere un momento...." COLOR W+/N*

	SELECT I.CODCTC,I.CLASE,I.DESCRI,I.BANCO,J.SALINI,J.TOTCAR,J.TOTABO,J.SALFIN,I.ENCARGO;
	FROM CAJA I, bcocta J;
	WHERE I.CODCTC=J.CODCTC AND J.MES=allt(TMES) AND I.ENCARGO='C'; 
	ORDER BY I.CODCTC ;
	INTO CURSOR cajas2
    SELECT CAJAS2
	GO TOP
  	IF EOF()
	   DO STANDBY WITH 'No hay datos..'
   	ELSE
	   DO REPORTE WITH 2, "RepT5C", ' Reporte T5-B ',2,.F.,.T.			
   	ENDIF
	DEACTIVATE WINDOW Xwait
	CLOSE DATA
  ENDIF
  RETURN


FUNCTION MFecha
*--------------
PARAMETERS TMES, TANO
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,VAL(TMES)*9-8,9)) + ' ' + TANO

PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR Okcancel=1 OR Okcancel=2
	RETURN .T.
ENDIF
RETURN .F.
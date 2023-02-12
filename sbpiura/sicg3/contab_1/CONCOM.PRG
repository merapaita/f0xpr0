* ----------------------------------------------------
* Programa de Consistencia de Datos 
* Reporte Consistencia de Documentos de Contabilidad 
* ----------------------------------------------------

* Abrir Archivos de Trabajo

USE Compro  IN 1 ORDER TAG Compro1         ALIAS Comp
USE Diario  IN 2 ORDER TAG Diario1         ALIAS Diar
USE Parmae  IN 3 ORDER TAG Parmae1         ALIAS parma
STORE 0   TO xano, xMes, xCuadre, vdebe, vhaber

DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
DEFINE WINDOW MesAno FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE ' °° Consistencia General °° ' COLOR SCHEME 5
ACTIVATE WINDOW MesAno
SELECT Parma
SEEK "MESANO"+"ACTUA"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
xAno     = IIF(FOUND(), YEAR(vmesact),   YEAR(DATE()))
xCuadre = 0
@ 1,01 SAY '                Mes : ' GET xMes PICTURE '99' VALID xMes > 0 .AND. xMes <= 12
@ 3,01 SAY '                A¤o : ' GET xAno PICTURE '9,999'
@ 5,10 GET xCuadre FUNCTION '*C \<Solo Errores de Cuadre'

@ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

CLEAR WIND MesAno
RELEASE WINDOES MesAno

IF OKCANCEL = 1 .AND. LASTKEY() # 27
	vind = SYS(3) + '.idx'
	DO ESPERA WITH 1,"Consistencia General en proceso ..."
	SELECT Comp
	INDEX ON Comprob+ComAdi TO (vind) FOR MONTH(Comp.Fecha) = xMes .AND. YEAR(Comp.Fecha) = xAno AND IIF(xCuadre=1,TotDeb#TotHab,.T.)
	DO ESPERA WITH 2
	GO TOP
	IF !EOF()
		DO REPORTE WITH 2, "ConCom", ' Consistencia de Documentos '
	 ELSE
		DO STANDBY WITH "No existe informaci¢n en dicho mes y a¤o"
	ENDIF
	CLOSE DATA
	ERASE (vind)
ENDIF
DEACTIVATE WINDOW Consis
RETURN


PROCEDURE Salida
*---------------
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


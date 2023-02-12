*-----------------------------------------
* RepSub.Prg
* Reporte de Creditos Registrados
* Autor : A. S. Marco Rodriguez Abad
*-----------------------------------------
*- Abriendo Archivos

CLOS DATA
USE Subvenc  IN  1 ORDER TAG Subvenc2	ALIAS Subvenc
USE Clientes IN  2 ORDER TAG Clientes1	ALIAS Clien
USE Parmae   IN  3 ORDER TAG Parmae1	ALIAS Parma

SELE Subvenc
SET Relation to CodCli INTO Clien

DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
mFecha1 = DATE()
mFecha2 = DATE()

DEFINE WINDOW wLista FROM 8,15 TO 17,70 FLOAT DOUBLE TITLE 'Reporte de Caja' COLOR SCHEME 5
ACTIVATE WINDOW wLista

@ 05,01 SAY "      Rango de Fechas: " GET mFecha1
@ 05,36 GET mFecha2

@ 07,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW wLista

DO Espera WITH 1, "Procesando Caja"

IF LASTKEY()#27 AND OkCancel#2
	SET FILTER TO BETW(FecSub,mFecha1,mFecha2)
	GO TOP
	DO Espera WITH 2
	IF !EOF()
		DO REPORTE WITH 2,"Subvenc","Listado de Subvenciones Realizadas"
	 ELSE
		DO StandBy WITH "No Existe Informaci¢n para Procesar."
	ENDIF
	SET FILTER TO
 ELSE
	DO Espera WITH 2
	DO StandBy WITH "Proceso Cancelado."
ENDIF

RETURN

PROCEDURE Salida
*---------------
ACTIVATE SCREEN
CLOSE DATABASE
RETURN

FUNCTION RetSb
*-------------
PARAMETER mSb
PRIVATE mRet
IF SEEK('ENTSUB'+mSb,'Parma')
	mRet = Parma.Descri
ELSE
	mRet = "Sin Descripci¢n"
ENDIF
RETURN mRet

*-----------------------------------------
* RepCre.Prg
* Reporte de Creditos Registrados
* Autor : A. S. Marco Rodriguez Abad
*-----------------------------------------
*- Abriendo Archivos

CLOS DATA
USE Creditos IN  1 ORDER TAG Creditos1	ALIAS Creditos
USE Clientes IN  2 ORDER TAG Clientes1	ALIAS Clien
USE Parmae   IN  3 ORDER TAG Parmae1	ALIAS Parma

SELE Creditos
SET Relation to CodCli INTO Clien

DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
mFecha1 = DATE()
mFecha2 = DATE()
vToCr  = 2
m.TipVta = SPACE(2)


DEFINE WINDOW wLista FROM 8,15 TO 17,70 FLOAT DOUBLE TITLE 'Reporte de Creditos' COLOR SCHEME 5
ACTIVATE WINDOW wLista
@ 01,01 SAY "    Todas las Creditos : " GET vToCr	 	FUNCTION '*RNH \<Si;\<No' SIZE 1,10,2
@ 03,01 SAY "  Tipo: " GET m.TipVta		WHEN vToCr=2	VALID VAL_PARA(m.TipVta,'TIPVTA',' ',32,20)

@ 05,01 SAY "      Rango de Fechas: " GET mFecha1
@ 05,36 GET mFecha2

@ 07,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW wLista

DO Espera WITH 1, "Procesando Reporte de Creditos"

IF LASTKEY()#27 AND OkCancel#2
	SET FILTER TO TipVta=IIF(vToCr=1,"",m.TipVta) AND BETW(FecCre,mFecha1,mFecha2)
	GO TOP
	DO Espera WITH 2
	IF !EOF()
		DO REPORTE WITH 2,"Creditos","Listado de Creditos"
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


FUNCTION RetTV
*-------------
PARAMETER mTv
PRIVATE mRet
IF SEEK('TIPVTA'+mTv,'Parma')
	mRet = Parma.Descri
ELSE
	mRet = "Sin Descripci¢n"
ENDIF
RETURN mRet

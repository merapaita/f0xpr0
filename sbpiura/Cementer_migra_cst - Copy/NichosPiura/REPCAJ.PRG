*-----------------------------------------
* RepCaj.Prg
* Reporte de Ingresos de Caja
* Autor : A. S. Marco Rodriguez Abad
*-----------------------------------------
*- Abriendo Archivos

CLOS DATA
USE Caja     IN  1 ORDER TAG Caja1      ALIAS Caja
USE Clientes IN  3 ORDER TAG Clientes1	ALIAS Clien
USE Parmae   IN  2 ORDER TAG Parmae1	ALIAS Parma

SELE Caja
SET RELATION TO CodCli INTO Clien ADDITIVE

DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
mFecha1 = DATE()
mFecha2 = DATE()
vToCj  = 2
m.TipCaj = SPACE(1)


DEFINE WINDOW wLista FROM 8,15 TO 17,70 FLOAT DOUBLE TITLE 'Reporte de Caja' COLOR SCHEME 5
ACTIVATE WINDOW wLista
@ 01,01 SAY "    Todas las Cajas : " GET vToCj	 	FUNCTION '*RNH \<Si;\<No' SIZE 1,10,2
@ 03,01 SAY "  Caja: " GET m.TipCaj		WHEN vToCj=2	VALID VAL_PARA(m.TipCaj,'TIPCAJ',' ',32,20)

@ 05,01 SAY "      Rango de Fechas: " GET mFecha1
@ 05,36 GET mFecha2

@ 07,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW wLista

DO Espera WITH 1, "Procesando Caja"

IF LASTKEY()#27 AND OkCancel#2
	SET FILTER TO TipCaj=IIF(vToCj=1,"",+m.TipCaj) AND BETW(FecReg,mFecha1,mFecha2)
	GO TOP
	DO Espera WITH 2
	IF !EOF()
		DO REPORTE WITH 2,"Caja","Listado de Ingresos a Caja"
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

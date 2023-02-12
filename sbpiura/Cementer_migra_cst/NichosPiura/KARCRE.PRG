*-------------------------------------------
* KarCre.Prg
* Reporte de Kardex de Creditos
* A.S. Marco Rodriguez Abad
* Fecha : 15/06/2007
*------------------------------------------
CLOS DATA
USE KarCre   IN 1 ORDER TAG KarCre1   ALIAS Kardex
USE Creditos IN 2 ORDER TAG Creditos1 ALIAS Creditos
USE Clientes IN 3 ORDER TAG Clientes1 ALIAS Clien
*USE ArtMae   IN 4 ORDER TAG ArtMae1  ALIAS ArtMae
*USE IteArt   IN 5 ORDER TAG IteArt1  ALIAS IteArt

SELE Kardex
SET RELATION TO codcre INTO CREDITOS ADDITIVE

SELE Creditos
SET RELATION TO CodCli INTO Clien ADDITIVE

SELE Kardex

DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------

vToCr  = 2
m.CodCre = SPACE(7)

*mFecha1  = CTOD("01/01/"+STR(YEAR(m.FecSis),4))
*mFecha2  = DATE()


DEFINE WINDOW wLista FROM 3,15 TO 20,70 FLOAT DOUBLE TITLE 'Reporte de Kardex' COLOR SCHEME 5
ACTIVATE WINDOW wLista
@ 01,01 SAY "    Todas los Creditos : " GET vToCr	 	FUNCTION '*RNH \<Si;\<No' SIZE 1,10,2
@ 03,01 SAY "  Credito: " GET m.CodCre	WHEN vToCr=2	VALID ValCre()

*@ 13,01 SAY "      Rango de Fechas: " GET mFecha1	WHEN .F.
*@ 13,36 GET mFecha2

@ 15,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW wLista

DO Espera WITH 1, "Procesando Kardex"

IF LASTKEY()#27 AND OkCancel#2
	SET FILTER TO CodCre=IIF(vToCr=1,"",+m.CodCre) 			&&AND BETW(Fecha,mFecha1,mFecha2)
	GO TOP
	DO Espera WITH 2
	IF !EOF()
		DO REPORTE WITH 2, "Kardex","Reporte de Kardex" ,2,.F.,.T.
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


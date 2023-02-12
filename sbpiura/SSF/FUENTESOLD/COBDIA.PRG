*------------------------------------------------------------------
* InsSoc.Prg
* Registra Socios
* Autor: Analista Segundo Correa Sandoval
*--------------------------------------------------------------------------
*- Abriendo Archivos
CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE maesoc   IN 2  ORDER TAG maesoc1  ALIAS socio
USE itecob   IN 3  ORDER TAG itecob1  ALIAS itecob
USE maecob   IN 4  ORDER TAG maecob1  ALIAS cobra
USE maepro   IN 5  ORDER TAG maepro1  ALIAS promo

*-
*- Mensajes de aviso al usuario
PUBLIC VSUMOFI,VSUMCOB,ok
STORE 0 TO vSumaTodo
STORE .T. TO ok
vmens01 = 'No hay Registros'
SELECT IteCob
SET RELATION TO CODCON INTO SOCIO
SET SKIP TO SOCIO
GO TOP
DO LISTA
DO fin_opcion
RETURN

PROCEDURE lista
*--------------
vAlias = Alias()
vorder = Order()
vrecno = Recno()
STORE 1  TO vToCli,vOrden,vTipPro,vLista,vTofue,vTomes
STORE date() to vfecini,vfecfin
vCli = SPACE(6)
vCodins = SPACE(3)

DEFINE WINDOW LIS FROM 10,15 TO 15,65 FLOAT DOUBLE TITLE 'Cobranzas del D¡a' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 00,01 SAY "      Fechas : "
@ 00,22 GET vfecini  PICTURE '@D'   COLOR SCHEME 7 
@ 01,01 SAY " Instituci¢n : " GET vcodins  PICTURE '999' valid IIF(!EMPTY(vcodins),val_para(vcodins,'INSTIT',' ',17,18),.T.)   COLOR SCHEME 7 WHEN vtocli=1
@ 03,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	SET ORDER TO ITECOB3 
	SET FILTER TO itecob.fecpag=vfecini and SOCIO.estado<>'99' AND IIF(!EMPTY(vcodins),SOCIO.codins=allt(vcodins),.T.)
	IF !EOF()
		do reporte with 2,"CobDia1",'Cobranzas Diarias',2
	ELSE
		DO STANDBY WITH vmens01
	ENDIF
ENDIF
SET FILTER TO
SET RELATION TO
SELECT (vAlias)
SET ORDER TO (vorder)
RETURN

PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RESTORE SCREEN FROM principal
RETURN

FUNCTION SUMOFI
*--------------
vrecno = recno()
go top
SUM CANCOB TO vsumofi FOR CODPRO="0000" AND EMPTY(CODCOB)
return vsumofi

FUNCTION SUMcob
*--------------
vrecno = recno()
go top
SUM CANCOB TO vsumcob FOR CODPRO="    " AND !EMPTY(CODCOB)
return vsumcob

FUNCTION VSIGUE
*---------------
vok=.f.
IF eof()
	vok=.T.
ElSE
	SKIP 1
	IF EOF()
		vok=.T.
	ELSE
		SKIP -1
	ENDIF	
ENDIF
RETURN vok

Function vsuma
*-------------
PARAMETER vmonto
vsumaTodo = vsumatodo + vmonto
RETURN " "
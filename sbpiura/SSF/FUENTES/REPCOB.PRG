*------------------------------------------------------------------
* InsSoc.Prg
* Registra Socios
* Autor: Analista Segundo Correa Sandoval
*--------------------------------------------------------------------------
*- Abriendo Archivos
CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE maesoc   IN 2  ORDER TAG maesoc1  ALIAS socio
USE itesoc   IN 3  ORDER TAG itesoc1  ALIAS itesoc
USE maepro   IN 4  ORDER TAG maepro1  ALIAS promo
*-
*- Mensajes de aviso al usuario
vmens01 = 'No hay Registros'
SELECT socio
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
DEFINE WINDOW LIS FROM 10,15 TO 15,65 FLOAT DOUBLE TITLE 'Cobranzas a Socios' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 00,01 SAY "      Fechas : "
@ 00,22 GET vfecini  PICTURE '@D'   COLOR SCHEME 7
@ 01,01 SAY " Instituci¢n : " GET vcodins  PICTURE '999' valid val_para(vcodins,'INSTIT',' ',17,18)   COLOR SCHEME 7 WHEN vtocli=1
@ 03,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	*SET FILTER TO DAY(socio.fecins)=DAY(vfecini) and MONTH(socio.fecins)+YEAR(socio.fecins) < MONTH(vfecini)+YEAR(vfecini) and SOCIO.estado<>'99' AND SOCIO.codins=allt(vcodins)
	SET FILTER TO DAY(socio.fecins)=DAY(vfecini) and socio.fecins<=vfecini and SOCIO.estado<>'99' AND SOCIO.codins=allt(vcodins)
	IF !EOF()
		do reporte with 2,"CobDia",'Cobranzas Diarias',2
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

*-----------------------------------
* Kardex1.Prg
* Kardex de Socios
* Autor: Marco Rodriguez Abad
*-----------------------------------
*- Abriendo Archivos
CLOS DATA
USE maesoc   IN 1  ORDER TAG maesoc1  ALIAS socio
USE KarSSF   IN 2  ORDER TAG KarSSF2  ALIAS KarSSF
USE parmae   IN 3  ORDER TAG parmae1  ALIAS parma
USE maePRO   IN 4  ORDER TAG maePRO1  ALIAS PROMO
*USE IteCob   IN 5  ORDER TAG IteCob1  ALIAS IteCob
*-
*- Mensajes de aviso al usuario
vmens01 = 'No hay Registros'
vmens02 = 'Relaci?n de Socios'
SELECT Socio
GO TOP
DO LISTA
DO fin_opcion
RETURN

PROCEDURE lista
*--------------
vAlias = Alias()
vorder = Order()
vrecno = Recno()
SELECT Socio
SET RELATION TO CODCON INTO KarSSF
SET SKIP TO KarSSF
STORE 1  TO vToCli,vOrden,vTOFEC,vLista,vTofue,vTomes
STORE date() to vfecini,vfecfin,vfecins
vcodcon = SPACE(6)
vCodins = SPACE(3)

DEFINE WINDOW LIS FROM 2,15 TO 20,65 FLOAT DOUBLE TITLE 'Listado de Cobranzas' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 01,01 SAY "   Por Socio : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6

@ 03,01 SAY "      C?digo : " 
@ 03,22 GET vCodcon PICTURE '!!!!!!' VALID Val_socio() when vTOCLI=1

@ 05,01 SAY " Instituci?n : " GET vcodins  PICTURE '999' valid IIF(EMPTY(vcodins),.T.,val_para(vcodins,'INSTIT',' ',17,18))   COLOR SCHEME 7 WHEN vtocli=2

*@ 07,01 SAY "Fec. Inscrip.: " GET vTOfec  FUNCTION '*RNH \<Si;\<No' SIZE 5,10,6 WHEN vtocli=1
*@ 09,01 SAY "       Fecha : "
*@ 09,22 GET vfecins  PICTURE '@D'   COLOR SCHEME 7 WHEN vTOfec=1  and vtocli=1

@ 11,01 SAY "   Cobranzas : " GET vToFue  FUNCTION '*RNH \<Todas;\<Por fechas' SIZE 1,10,6  && VALID VALTOD(VTOfue,5,15) 
@ 13,01 SAY " Desde-Hasta : "
@ 13,22 GET vfecini  PICTURE '@D'   COLOR SCHEME 7 WHEN vtoFUE=2
@ 13,32 GET vfecfin  PICTURE '@D'   COLOR SCHEME 7 VALID (vfecfin >= vfecini) WHEN vtoFUE=2

@ 15,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	IF vtocli=1
		SET FILTER TO codcon=vCodcon and iif(vtofue=2,BETWEEN(Karssf.feccob,vfecini,vfecfin),.t.)  and socio.estado<>'99'
*		SET FILTER TO codcon=vCodcon and iif(vtofue=2,BETWEEN(ITECOB.feccob,vfecini,vfecfin),.t.)  and socio.estado<>'99'
	Else
		SET FILTER TO iif(vtofue=2,BETWEEN(KarSsf.feccob,vfecini,vfecfin),.t.) and iif(vtofue=2,BETWEEN(socio.fecins,vfecini,vfecfin),.t.) AND SOCIO.codins=allt(vcodins) And socio.estado<>'99'
*		SET FILTER TO iif(vtofue=2,BETWEEN(ITECOB.feccob,vfecini,vfecfin),.t.) and iif(vtofue=2,BETWEEN(socio.fecins,vfecini,vfecfin),.t.) AND SOCIO.codins=allt(vcodins) And socio.estado<>'99'
	endif
	do reporte with 2,"Kardex",'Listado de Cobranza',2
ENDIF
SET FILTER TO
SET RELATION TO
SELECT (vAlias)
SET ORDER TO (vorder)
GO vrecno
RETURN

PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RESTORE SCREEN FROM principal
RETURN

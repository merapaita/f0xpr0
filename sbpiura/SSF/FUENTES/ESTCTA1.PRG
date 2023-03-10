*-----------------------------------
* EstCta1.Prg
* Estado de Cuentas
* Autor: Marco Rodriguez Abad
*-----------------------------------
*- Abriendo Archivos
CLOS DATA
USE maesoc   IN 1  ORDER TAG maesoc1  ALIAS socio
USE KarSSF   IN 2  ORDER TAG KarSSF2  ALIAS KarSSF
USE parmae   IN 3  ORDER TAG parmae1  ALIAS parma
USE maePRO   IN 4  ORDER TAG maePRO1  ALIAS PROMO

vDbf = SYS(3) + '.Dbf'
CREATE TABLE (vDbf) (CodCon C(6), Socio C(36), Estado C(2), CuoPen N(3), Fecha D, FecCob D, CanCob N(6,2),;
					 SdoCob N(8,2),SdoCC N(8,2),SdoGA N(8,2),MtoSer N(8,2),;
					 Deuda N(8,2),sFavor N(8,2))
					 
USE
USE (vDbf) IN 0 ALIAS EstCta


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
STORE 1  TO vTo,vOr,vTOFEC,vLista,vTofue,vTomes
STORE date() to vfecini,vfecfin,vfecCor
vcodcon = SPACE(6)
vCodins = SPACE(3)
vMesIni = 0
vMesFin = 999
lcon = .F.
cCon1 = SPACE(6)
cCon2 = SPACE(6)

DEFINE WINDOW LIS FROM 2,15 TO 22,65 FLOAT DOUBLE TITLE 'Resumen de Estados de Cuenta' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 01,10 SAY "Rango de Cuotas Pendientes "
@ 03,04 SAY "Desde-Hasta : "
@ 03,19 GET vMesIni  PICTURE '999'   COLOR SCHEME 7
@ 03,25 GET vMesfin  PICTURE '999'   COLOR SCHEME 7 VALID (vMesfin >= vMesini)
@ 05,01 SAY "   Seleccionar : "		GET vTo		PICTURE '@^ Activos;Anulados;Recindidos;Solo con servicio' SIZE 1,15,6
@ 08,01 SAY "      Ordenado : " GET vOr  PICTURE '@^ Meses Pendientes;Contratos' SIZE 1,15,6
@ 11,01 SAY "Considerar Rango Contratos?" GET lCon FUNCTION '*C'
@ 13,04 SAY "Desde-Hasta : "
@ 13,20 GET cCon1		PICTURE "999999" WHEN lCon
@ 13,32 GET cCon2		PICTURE "999999" WHEN lCon 	VALID cCon2 >= cCon1
@ 15,01 SAY "   Fecha Corte : " GET vFecCor	PICTURE '@D'

@ 17,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	*****
	DO Espera WITH 1,"Espere un Momento"
	SELE KarSSF
	SET FILTER TO IIF(!EMPTY(vFecCor),Fecha<=vFecCor,.T.)
	SELECT Socio
	IF lCon
		SET FILTER TO CodCon>=cCon1 AND Codcon<=cCon2
	ENDIF
	SET RELATION TO CODCON INTO KarSSF
	SET SKIP TO KarSSF
	
	SELE Socio
	GO TOP
	SCAN
		vCodCon = CodCon
		vSocio  = ALLTRIM(Socio.apelli)+ ', '+ ALLTRIM(Socio.nombre)
		vEstado = Socio.Estado
		vSdoCob = 0
		vSdoGA  = 0
		vSdoCC  = 0
		vMtoSer = 0
		SCAN WHILE CodCon = vCodCon
			vFecha  = KarSSF.Fecha
			vFecCob = KarSSF.FecCob
			vCanCob = KarSSF.CanCob
			vSdoCob = vSdoCob + KarSSF.CanCob
			vSdoCC  = vSdoCC  + KarSSF.CtaCap
			vSdoGA  = vSdoGA  + KarSSF.GtoAdm
			vMtoSer = vMtoSer + KarSSF.MtoSer
		ENDSCAN
		
		IF EMPTY(vFecCob)
			vFecCob = vFecha
		ENDIF
		
		SELE EstCta
		APPEND BLANK
		REPLACE CodCon WITH vCodCon, Socio WITH vSocio,;
				Estado WITH vEstado, Fecha WITH vFecha,;
				FecCob WITH vFecCob, CanCob WITH vCanCob,;
				SdoCob WITH vSdoCob, SdoCC WITH vSdoCC,;
				SdoGA  WITH vSdoGA, MtoSer WITH vMtoSer
		
		SELE Socio
		SKIP -1
	
	ENDSCAN
	
	SELE EstCta
	GO TOP
	
	SCAN
		vDeuda  = IIF(MtoSer-SdoCC>0,MtoSer-SdoCC,0)
		vsFavor = IIF(MtoSer-SdoCC<0,MtoSer-SdoCC,0) * -1
		REPLACE Deuda WITH vDeuda, sFavor WITH vsFavor, CuoPen WITH int((vFecCor-FECCOB)/30)
	ENDSCAN
	*****
	
	SELE EstCta
	
	vIdx = SYS(3)+'.Idx'
	
	DO CASE
		CASE vOr = 1
			INDEX ON STR(CuoPen)+CodCon TO (vIdx) FOR !(EMPTY(Deuda) and EMPTY(sFavor))
		CASE vOr = 2
			INDEX ON CodCon TO (vIdx) FOR !(EMPTY(Deuda) and EMPTY(sFavor))
	ENDCASE
	
	DO CASE
		CASE vTo = 1
			SET FILTER TO BETWE(CuoPen,vMesIni,vMesFin) AND estado<>'99' and estado<>'90'
		CASE vTo = 2
			SET FILTER TO estado='99'
		CASE vTo = 3
			SET FILTER TO estado='90'
		CASE vTo = 4
			SET FILTER TO BETWE(CuoPen,vMesIni,vMesFin) AND estado='10'
	ENDCASE
	
	DO Espera WITH 2

	do reporte with 2,"EstCta1",'Resumen de Estado de Cuenta',2,.F.,.T.
*	do reporte with 2,"EstCta2",'Resumen de Estado de Cuenta',2,.F.,.T.		&& PARA DIRECCIONES
	
ENDIF

SELE Socio
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
ERASE (vDbf)
RETURN


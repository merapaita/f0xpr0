*-----------------------------------
* EstSoc.Prg
* Estadistica de cuentas de Socios
* Autor: Marco Rodriguez Abad
*-----------------------------------
*- Abriendo Archivos
CLOS DATA
USE maesoc   IN 1  ORDER TAG maesoc1  ALIAS socio
USE KarSSF   IN 2  ORDER TAG KarSSF2  ALIAS KarSSF

Create Cursor EstSoc (CodCon C(6), Movim C(20),FecIni D, CtaIni D,FecFin D,CtaFin D,Cuota N(6,2),NumAp N(3))

*- Mensajes de aviso al usuario
vmens01 = 'No hay Registros'
vmens02 = 'Relaci¢n de Socios'
SELECT Socio
GO TOP
DO LISTA
DO fin_opcion
RETURN

PROCEDURE lista
*--------------
SELECT Socio
*SET RELATION TO CODCON INTO KarSSF
vToSoc = 1
vcodcon = SPACE(6)

DEFINE WINDOW LIS FROM 5,15 TO 17,65 FLOAT DOUBLE TITLE 'Listado de Cobranzas' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 01,01 SAY "   Por Socio : " GET vTOSoc  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6

@ 03,01 SAY "      C¢digo : " 
@ 03,22 GET vCodcon PICTURE '!!!!!!' VALID Val_socio() when vTOSoc=1

@ 05,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	SELE KarSSF
	
	IF vtoSoc=1
		SET FILTER TO codcon=vCodcon		&&  and socio.estado<>'99'
	endif
	
*set step on

	SCAN
		m.CodCon = KarSSF.CodCon
		m.Tipo   = KarSSF.Tipo
		m.FecIni = KarSSF.Fecha
		m.CtaIni = KarSSF.FecCob
		m.NumAp  = 0
		SCAN WHILE CodCon = m.CodCon AND Tipo = m.Tipo
			IF Karssf.Tipo = "C"
				m.Movim  = "Aportaciones :"
				m.FecFin = KarSSF.Fecha
				m.CtaFin = KArSSF.FecCob
				m.Cuota  = KArSSF.CanCob
				m.NumAp  = m.NumAp + 1
			ENDIF
			
			IF Karssf.Tipo = "S"
				m.Movim  = "Servicio :"
				m.FecFin = {//}
				m.CtaFin = {//}
				m.Cuota  = 0
				m.NumAp  = 0
			ENDIF
		ENDSCAN
		SELE EstSoc
		IF f_Appd()
			GATHER MEMVAR
		ENDIF
		SELE KarSSF
		SKIP -1
	ENDSCAN
	
	SELE EstSoc
	Set Relation to CodCon INTO Socio
	IF RECCOUNT() > 0
		do reporte with 2,"EstSoc",'Listado de Cobranza',2
	ELSE
		DO StandBy WITH "La Informacion Solicitada no tiene Movimiento"
	ENDIF
ENDIF
SELE KarSSF
SET FILTER TO
SET RELATION TO
RETURN

PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RESTORE SCREEN FROM principal
RETURN

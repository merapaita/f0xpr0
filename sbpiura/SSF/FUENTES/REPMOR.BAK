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
*-
*- Mensajes de aviso al usuario
vmens01 = 'No hay Registros'
*BASE DE DATOS PARA AGREGAR A LOS MOROSOS
SELECT 4
CREATE TABLE agrega ;
	(codcon C(6), apelli C(30),nombre C(30), direcc C(60),;
	fecins D(8), fecCob D(8), fecpag D(8))
	
SELECT itecob
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
DEFINE WINDOW LIS FROM 10,15 TO 15,65 FLOAT DOUBLE TITLE 'Listado Cobranzas' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 00,01 SAY "      Fechas : "
@ 00,22 GET vfecini  PICTURE '@D'   COLOR SCHEME 7 
@ 00,33 GET vfecFin  PICTURE '@D'   COLOR SCHEME 7 
@ 01,01 SAY " Instituci¢n : " GET vcodins  PICTURE '999' valid val_para(vcodins,'INSTIT',' ',17,18)   COLOR SCHEME 7 WHEN vtocli=1
@ 03,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	SET FILTER TO BETWEEN(ITECOB.feccob, vfecini, vfecFin)  and SOCIO.estado<>'99' AND SOCIO.codins=allt(vcodins)
	IF !EOF()
		DO AGREGA
		SELECT agrega
		do reporte with 2,"repmor",'Reporte de Morosos',2
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

PROCEDURE AGREGA
*---------------
SELECT ITECOB
vmesrep = month (vfecini)
SCAN
	vgraba = .F.
	vcodcon1 = codcon
	vcodcon2 = codcon
	DO WHILE vcodcon1 = vcodcon2 AND !EOF()
		vmescob = MONTH(FECCOB)
		DO CASE
			CASE vmescob = 2
				IF (vfecFin-feccob)>28
					vgraba=.T.
				ELSE
					vgraba=.F.
				ENDIF
			CASE vmescob=1 or vmescob=3 or vmescob=5 or vmescob=7 or vmescob=8 or vmescob=10 or vmescob=12
				IF (vfecFin-feccob)>31
					vgraba=.T.
				ELSE
					vgraba=.F.
				ENDIF
			OTHER
				IF (vfecFin-feccob)>31
					vgraba=.T.
				ELSE
					vgraba=.F.
				ENDIF
		ENDCASE	
		SKIP
		vcodcon2 = codcon
	ENDDO
	SKIP -1	
	if vgraba
		DO adiciona
	endif	
ENDSCAN	
RETURN

FUNCTION adiciona
*----------------
SELECT agrega
APPEND BLANK
REPLACE codcon WITH itecob.codcon,;
	    apelli WITH socio.apelli,;
	    nombre WITH socio.nombre,;
	    direcc WITH socio.direcc,;
		fecins WITH socio.fecins,;
		fecCob WITH itecob.feccob,;
		fecpag WITH itecob.fecPag
SELECT ITECOB
RETURN .T.

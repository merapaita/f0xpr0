** ---------------------------------------------------
** CONSAL.PRG
** Reporte Ejecucion ESTADOS
** UDEP
** ---------------------------------------------------
* Abrir Archivos de trabajo
CLOS DATA
USE PARMAE    IN 1   ORDER TAG parmae1  ALIAS parma
USE calen    IN 2   ORDER TAG calen2   ALIAS calen
USE maepre   IN 3   ORDER TAG maepre1  ALIAS maepre
USE maepar   IN 4   ORDER TAG maepar1  ALIAS presu
USE itepar   IN 5   ORDER TAG itepar3  ALIAS itepar
USE itehc    IN 6   ORDER TAG itehc1   ALIAS iteHC

* PARA LAS AFECTACIONES DE EMERGENCIA
USE PteAnu   IN 9   order tag PteAnu9  ALIAS AnuPA

*- Mensajes de aviso al usuario
vmens01 = 'Registro de Calendario'
vmens02 = ' Calendario : REVISION '
vmens04 = 'Dicho Calendario no fue encontrado'
vmens05 = 'No existe Calendario anterior'
vmens06 = 'No existe Calendario siguiente'
vmens07 = '? Desea Anular ?ste Calendario ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Calendario ha sido anulado'
vmens10 = 'El Calendario ya est? Atendido'
vmens11 = 'El Calendario ha sido devuelto'

*----
PUBLIC TOT,VACUM,TOT1,TOTCOM
TOT = 0
TOT1 = 0
DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*--------------
PUBLIC vFecini,vFecFin
vperiodo = RIGHT(STR(YEAR(DATE()),4),2)
*store 2 to vTotal,vtipo
VTOTAL = 2
vtipo  = 2
vUniges = '01'
vUnieje = '001'
store space(2) to vcodfte,vcalend,VCODFUN
store space(3) to vcodprg
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 

DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
	TITLE ' ?? SALDO DE CALENDARIOS ?? ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1

@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
@  3,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID IIF(!EMPTY(vUniGes),val_para(vUniGes,'UNIGES',' ',18,30),.T.)
@  4,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID IIF(!EMPTY(vUniEje),val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30),.T.)
@  8,2 SAY '     Funci?n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
@  9,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
@ 10,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
@ 11,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
@ 12,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
@ 13,2 SAY '   Al mes de : ' GET vcalend    PICTURE '!!'  		VALID val_para(vcalend,'FECMES',' ',18,30)
@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 		VALID val_para(vcodfte,'CODFTE',' ',18,30)

READ VALID val_read()
DEACTIVATE WINDOW lis_1
IF LASTKEY()= 27
	RETURN
ENDIF
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*

SELECT calen
GO TOP
IF EOF()
	DO standby WITH vmens08
	RETURN
ELSE
	IF vtotal = 1
       	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO,CALEN.CODCAD,;
   			CALEN.Codfte, CALEN.TOTAFE ,calen.ampliar,CALEN.TOTOC ,CALEN.TOTOS, CALEN.ESTFUN ;
			FROM CALEN ;
			WHERE periodo=ALLTRIM(Vperiodo) AND	IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and codfte=allt(vcodfte) AND ;
				IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) AND NUMMES = ALLTRIM(vcalend) ;
			INTO CURSOR CalenX
			  		
	ELSE
	    SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO,CALEN.CODCAD,;
	    	CALEN.Codfte, CALEN.TOTAFE ,calen.ampliar,CALEN.TOTOC ,CALEN.TOTOS, CALEN.ESTFUN ;
		  	FROM CALEN ;
		  	WHERE periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) and IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)AND codfte=allt(vcodfte);
				and NUMMES = ALLTRIM(vcalend) ;
			INTO CURSOR CalenX		
	ENDIF
ENDIF	
vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 0 alias CALEN1 EXCLU
vInd = SYS(3) + '.IDX'
SELECT CALEN1
INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (Vind)

* MONTOS DE LA HOJA DE CONTROL
yind = SYS(3) + '.IDX'
SELE ITEhc
IF VTOTAL=2
	INDEX ON CODPART to (yind) FOR codfte=allt(vcodfte) and nummes=allt(vcalend) AND ESTADO#'99' ;
		AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
ELSE
	INDEX ON CODPART to (yind) FOR codfte=allt(vcodfte) and nummes=allt(vcalend) AND ESTADO#'99' ;
		AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
ENDIF	
GO TOP
SCAN
	sele maepre
	seek Vperiodo+ITEhc.UNIGES+ITEhc.UNIEJE+ITEhc.codcad
	vkey = uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODPART+ITEHC.CODFTE
	SELECT calen1
	LOCATE FOR LEFT(ESTFUN,30)+CODPART+CODFTE = vkey
	if found()
		REPLACE totafe WITH totafe+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
	else
		append blank
		REPLACE NUMMES  WITH ALLTRIM(vcalend),;
				CODPART WITH ITEHC.CODPART,;
				PERIODO WITH VPERIODO,;
				CODCAD  WITH ITEHC.CODCAD,;
				CODFTE  WITH ITEHC.CODFTE,;
				ESTFUN  WITH LEFT(VKEY,30),;
				TOTAFE  WITH IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
	endif	
	SELECT itehc
ENDSCAN

*IF VSISTEMA='1'
*	*****afectacion de emergencia
*	SELECT 3
*	USE 	
*	USE \EMER98\DATA\maepre   IN 3   ORDER TAG maepre1  ALIAS maepree
*	SELECT 6
*	USE
*	USE \EMER98\DATA\itehc    IN 6   ORDER TAG itehc1   ALIAS itehce
*	SELE ITEhce
*	IF VTOTAL=2
*		INDEX ON CODPART to (yind) FOR codfte=allt(vcodfte) and nummes=allt(vcalend) AND ESTADO#'99'
*	ELSE
*		INDEX ON CODPART to (yind) FOR codfte=allt(vcodfte) and nummes=allt(vcalend) AND ESTADO#'99'
*	ENDIF	
*	GO TOP
*	SCAN
*		sele maepree
*		seek Vperiodo+ITEhce.UNIGES+ITEhce.UNIEJE+ITEhce.codcad
*		vkey = uniges+unieje+codfun+codprg+codspr+actpry+itehce.codcom+'00001'+ITEHCe.CODPART+ITEHCe.CODFTE
*		SELECT calen1
*		LOCATE FOR LEFT(ESTFUN,30)+CODPART+CODFTE = vkey
*		if found()
*			REPLACE totafe WITH totafe+ITEHCe.valpart
*		else
*			append blank
*			REPLACE NUMMES  WITH ALLTRIM(vcalend),;
*				CODPART WITH ITEHCe.CODPART,;
*				PERIODO WITH VPERIODO,;
*				CODCAD  WITH ITEHCe.CODCAD,;
*				CODFTE  WITH ITEHCe.CODFTE,;
*				ESTFUN  WITH LEFT(VKEY,30),;
*				TOTAFE  WITH ITEHCe.VALPART
*		endif	
*		SELECT itehce
*	ENDSCAN
*ENDIF	
DEACTIVATE WINDOW Standby
SELECT CALEN1
SET FILTER TO periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) and IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)AND codfte=allt(vcodfte);
	and NUMMES = ALLTRIM(vcalend) 
GO TOP
IF vtotal = 1
	if vtipo = 1
		DO reporte WITH 2,"SalCal1",' A nivel de Marco Presupuestal'
	ELSE
		DO reporte WITH 2,"SalCal11",' A nivel de Marco Presupuestal'
	ENDIF
ELSE
	INDEX ON PERIODO+LEFT(ESTFUN,10)+CODFTE+CODPART TO (vind)
	GO TOP
	if vtipo = 1
		DO reporte WITH 2,"SalCaG1",' A nivel de Marco Presupuestal'
	ELSE
		DO reporte WITH 2,"SalCaG11",' A nivel de Marco Presupuestal'
	ENDIF
ENDIF
vInd = SYS(3) + '.IDX'
INDEX ON LEFT(ESTFUN,5)+LEFT(codpart,2) TO (vind)
DO reporte WITH 2,"SalCalr",' Consolidado de GENERAL ',1,.F.,.T.

USE 
ERASE (vind)
SELECT calen
RETURN

PROCEDURE Salida
*---------------
ACTIVATE   SCREEN
CLOSE DATABASES
RETURN

PROCEDURE SUMCAL
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE	
SUM valpart+ampliar TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumCAL1
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM valpart+ampliar TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM valpart+ampliar TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

PROCEDURE SUMCOM
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE	
SUM TOTAFE TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumCOM1
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM TOTAFE TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM TOTAFE TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

** ---------------------------------------------------
** CONSAL2.PRG
** Reporte Ejecucion ESTADOS
** UDEP
** ---------------------------------------------------
* Abrir Archivos de trabajo
CLOS DATA
USE parmae   IN 1   ORDER TAG parmae1  ALIAS parma
USE itecp    IN 2   ORDER TAG itecp4   ALIAS itecp
use maepre   IN 3   ORDER TAG maepre1  ALIAS maepre
USE maepar   IN 4   ORDER TAG maepar1  ALIAS presu
USE itepar   IN 5   ORDER TAG itepar3  ALIAS itepar
USE IteAut   IN 7   ORDER TAG IteAut4  ALIAS IteA
USE Cajas    IN 10  ORDER TAG Cajas2   ALIAS Caja

*- Mensajes de aviso al usuario
vmens01 = 'Registro de Calendario'
vmens02 = ' Calendario : REVISION '
vmens04 = 'Dicho Calendario no fue encontrado'
vmens05 = 'No existe Calendario anterior'
vmens06 = 'No existe Calendario siguiente'
vmens07 = '¨ Desea Anular ‚ste Calendario ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Calendario ha sido anulado'
vmens10 = 'El Calendario ya est  Atendido'
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
VTOTAL = 2
vtipo  = 2
*vUniges = '01'
*vUnieje = '001'
store space(2) to vcodfte,vcalend,VCODFUN
STORE SPACE(2) TO VMES
store space(3) to vcodprg
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 
store space(14) to vcodctc

DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
	TITLE ' °° SALDO DE CALENDARIOS °° ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1

@ 04,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
@ 05,2 SAY '   Al mes de : ' GET vmes        PICTURE '!!'  		VALID val_para(vmes,'FECMES',' ',18,30)
@ 06,2 SAY '   Cta. Cte. : ' GET vcodctc    					VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",vcodctc,1,12,18)
READ VALID val_read()
DEACTIVATE WINDOW lis_1
IF LASTKEY()= 27
	RETURN
ENDIF
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
sele itea
vkey=vperiodo+vmes+vcodctc
seek vkey
IF EOF()
	DO standby WITH vmens08
	RETURN
else
	par=codpart
	par1=codpart
	DO reporte WITH 2,"Salaut2",' Consolidado de GENERAL ',1,.F.,.T.
endif	
USE 
RETURN

FUNCTION val_aut
*---------------
PARAMETERS par
vvsel= SELECT()
SELECT IteA
SUM monasig TO xx FOR ALLTRIM(codctc)=ALLTRIM(vcodctc) .AND. nummes=ALLTRIM(vmes) .AND. par=codpart
SELECT (vvsel)
RETURN xx


FUNCTION Tot_afe
*---------------
PARAMETERS par1
vvreg = RECNO()
sele itecp
SUM totafe TO yy FOR LEFT(codpart,2)=par1 AND CODCTC=VCODCTC AND PERIODO=VPERIODO AND NUMMES=VMES
GO vvreg
RETURN yy

PROCEDURE Salida
*---------------
CLOSE DATABASE
ACTIVATE   SCREEN
RETURN


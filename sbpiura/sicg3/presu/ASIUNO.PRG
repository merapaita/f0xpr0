*--------------------------------------------------------------------------
* AsiUno.PRG
* Asigna Uno al Presupuesto de Gastos 
* Autor: Rodriguez Abab Marco
* FECHA: 31/12/99
*--------------------------------------------------------------------------
Vmens08 = 'No hay registros para procesar'

CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE maepar   IN 2  ORDER TAG maepar1  ALIAS presu
USE itepar   IN 3  ORDER TAG itepar1  ALIAS itepar
USE maepre   IN 4  ORDER TAG maepre1  ALIAS maepre

DO Pantalla                  && Muestra pantalla inicial
CLOS DATA
RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
DEFINE WINDOW lis_1 FROM 10,10 TO 15,70 DOUBLE ;
	TITLE ' °°  Actualizaci¢n Presupuestal °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis_1
STORE 0 TO vTotal,vtipo
STORE SPACE(2) to vperiodo,vcalend

@  1,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
@  3,2 say '  Calendario : ' GET vCalend    PICTURE '!!'	VALID VAL_PARA(vCalend  ,'FECMES',' ',18,25)

READ VALID val_read()

DEACTIVATE WINDOW lis_1
RELEASE WINDOWS Lis_1

IF LASTKEY()= 27
	RETURN
ENDIF

SELE itepar
IF EOF()
	DO standby WITH vmens08
ELSE
	DEFINE WINDOW Xwait FROM 20,06 TO 22,78 COLOR SCHEME 05 
	ACTIVATE WINDOW Xwait
	@ 0,10 SAY " Espere un Momento...Actualizando Presupuesto!" COLOR W+/BR*
	Do actualiza
	DEACTIVATE WIND xWait
	RELEASE xwait
ENDIF
CLOS DATA
RETURN

PROCEDURE actualiza
*------------------
SELECT itepar
go top
SCAN
	vmes    = 'M_'+vcalend
	REPLACE &vmes with 1
	DO chequea
ENDSCAN
RETURN

PROCEDURE chequea
*----------------
vtotmes = M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12
vValpart = valpart+cresup+tra001+tra003+tra004+tra005
IF vValpart<vtotmes
	vsaldo = vtotmes-vValpart
endif
REPLACE TRI_01 WITH M_01+M_02+M_03
REPLACE TRI_02 WITH M_04+M_05+M_06
REPLACE TRI_03 WITH M_07+M_08+M_09
REPLACE TRI_04 WITH M_10+M_11+M_12
RETURN .T.

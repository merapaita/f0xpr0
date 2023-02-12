**--------------------------------------------------
** ACMIPM.PRG
** Mantenimiento Tabla de Indices de Precios Mensuales
** Autor : A.S. Marco Rodriguez Abad
** Fecha : 06/11/2000
**--------------------------------------------------
PUBLIC nDec

Vmens01 = "Indices de Precios Mensuales y sus Respectivos Ajustes"
Vmens02 = "Ingreo de Indice de Precios Mensual"

USE Factores IN 1 ORDER TAG Factores2 ALIAS Factores
USE Parmae   IN 2 ORDER TAG Parmae1   ALIAS Parma

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
*nDec = SET("DECI")
*SET DECIMALS TO 3

*- Inicia proceso
*-----------------
DO Inicia             && Define ventanas, men£s, t¡tulos
*DO Pantalla           && Muestra pantalla inicial
DO Vista

DO Fin_opcion
RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN

DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
TITLE Vmens01 COLOR SCHEME 10 FOOTER '[F2] Corrije ÍÍÍÍ [F5] Agrega ÍÍÍÍ [F8] Elimina ÍÍÍÍ [F10] Salir'

DEFINE WINDOW Wind_1 FROM 05,10 TO 20,70 DOUBLE ;
TITLE Vmens02 COLOR SCHEME 5 

RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
ON KEY LABEL F2 DO Corrije
ON KEY LABEL F5 DO Agrega
ON KEY LABEL F8 DO Elimina
ON KEY LABEL F10 KEYBOARD(CHR(23))
ACTIVATE WINDOW Wind_0
SELECT Factores
BROW NOMENU LOCK 3 NOAPPEND NODELETE NOEDIT IN WINDOWS Wind_0 FIELD ;
	PERIODO :H='Ano',;
	MES,;
	IPM :H='Imp. Prom.':12,;
	FACENE :H='Ene.':6,;
	FACFEB :H='Feb.':6,;
	FACMAR :H='Mar.':6,;
	FACABR :H='Abr.':6,;
	FACMAY :H='May.':6,;
	FACJUN :H='Jun.':6,;
	FACJUL :H='Jul.':6,;
	FACAGO :H='Ago.':6,;
	FACSET :H='Set.':6,;
	FACOCT :H='Oct.':6,;
	FACNOV :H='Nov.':6,;
	FACDIC :H='Dic.':6

RETURN


PROCEDURE Agrega
*---------------
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE WINDOW Wind_1
CLEAR

@ 1, 2 SAY " Tipo                     :"
@ 3, 2 SAY " Per¡odo                  :"
@ 5, 2 SAY " Mes                      :" 
@ 7, 2 SAY " Indice de Precios Mensual:"
@ 9, 2 SAY " Observaciones            :"

SELECT Factores
SCATTER MEMVAR BLANK
m.Tipo    = "M"
m.periodo = STR(YEAR(DATE()),4)
*@  1,30 GET m.Tipo FUNCTION "@M Mensual ,Anual  ,Promedio" 
@  1,30 GET m.Tipo      PICTURE '!' VALID val_para(m.Tipo,'TIPAJU',' ',28,20)
@  3,30 GET m.Periodo   PICTURE '!!!!' 
@  5,30 GET m.mes       PICTURE '!!' VALID val_para(m.mes,'FECMES',' ',30,20) ;
        WHEN m.Tipo="M"
@  7,30 GET m.IPM       PICTURE '9999.999999'
@  9,30 GET m.observa   PICTURE '@!'
@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<Acepta;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

IF LASTKEY()#27 AND OkCancel#2
	DO AgrInd
ENDIF

ON KEY LABEL F2 DO Corrije
ON KEY LABEL F5 DO Agrega
ON KEY LABEL F8 DO Elimina
ON KEY LABEL F10 KEYBOARD(CHR(23))

ACTIVATE WINDOWS Wind_0
RETURN

FUNCTION AgrInd
*-------------
IF SEEK(m.Tipo+m.Periodo+m.Mes)
	DO Standby WITH "El Indice Ingresado ya Existe"
	RETURN
 ELSE
	IF f_Appd()
		REPLACE Tipo WITH m.Tipo, Periodo WITH m.Periodo, Mes WITH m.Mes, IPM WITH m.IPM
	ENDIF
ENDIF
FOR I = 0 TO VAL(m.Mes)
	IF SEEK(m.Tipo+m.Periodo+PADL(ALLTRIM(STR(i)),2,'0'))
		nIPMOld = Factores.IPM
		nAjuste = 1 + (m.IPM - nIPMOld) / nIPMOld
	ENDIF
	
	IF m.Mes # '00'
		cCampo = 'FAC'+LEFT(cmes(VAL(m.Mes)),3)
		IF SEEK(m.Tipo+m.Periodo+PADL(ALLTRIM(STR(i)),2,'0'))
			IF f_lock(1)
				REPLACE (cCampo) WITH ROUND(nAjuste,3)
			ENDIF
		 ELSE
			DO StandBy WITH 'Error el IPM'+m.tipo+' '+m.Periodo+' '+m.Mes
		ENDIF
	ENDIF
ENDFOR

RETURN 

PROCEDURE Corrije
*---------------
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE WINDOW Wind_1
CLEAR

@ 1, 2 SAY " Tipo                     :"
@ 3, 2 SAY " Per¡odo                  :"
@ 5, 2 SAY " Mes                      :" 
@ 7, 2 SAY " Indice de Precios Mensual:"
@ 9, 2 SAY " Observaciones            :"

SELECT Factores
SCATTER MEMVAR
*@  1,30 GET m.Tipo FUNCTION "@M Mensual ,Anual  ,Promedio" 
@  1,30 GET m.Tipo      PICTURE '!' VALID val_para(m.Tipo,'TIPAJU',' ',30,20)
@  3,30 GET m.Periodo   PICTURE '!!!!' 
@  5,30 GET m.mes       PICTURE '!!' VALID val_para(m.mes,'FECMES',' ',30,20) ;
        WHEN m.Tipo="M"
@  7,30 GET m.IPM    PICTURE '9999.999999'
@  9,30 GET m.observa   PICTURE '@!'
@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<Acepta;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

IF LASTKEY()#27 AND OkCancel#2
	DO CorrInd
ENDIF

ON KEY LABEL F2 DO Corrije
ON KEY LABEL F5 DO Agrega
ON KEY LABEL F8 DO Elimina
ON KEY LABEL F10 KEYBOARD(CHR(23))

ACTIVATE WINDOWS Wind_0
RETURN

FUNCTION CorrInd
*-------------

IF SEEK(m.Tipo+m.Periodo+m.Mes)
	IF f_lock(1)
		REPLACE IPM WITH m.IPM
	ENDIF
 ELSE
	DO StandBy WITH "El Indice No Existe"
ENDIF
FOR I = 0 TO VAL(m.Mes)
	IF SEEK(m.Tipo+m.Periodo+PADL(ALLTRIM(STR(i)),2,'0'))
		nIPMOld = Factores.IPM
		nAjuste = 1 + (m.IPM - nIPMOld) / nIPMOld
	ENDIF
	
	IF m.Mes # '00'
		cCampo = 'FAC'+LEFT(cmes(VAL(m.Mes)),3)
		IF SEEK(m.Tipo+m.Periodo+PADL(ALLTRIM(STR(i)),2,'0'))
			IF f_lock(1)
				REPLACE (cCampo) WITH ROUND(nAjuste,3)
			ENDIF
		 ELSE
			DO StandBy WITH 'Error el IPM'+m.tipo+' '+m.Periodo+' '+m.Mes
		ENDIF
	ENDIF
ENDFOR

RETURN 

FUNCTION Elimina
*--------------
m.Periodo = Factores.Periodo
m.Mes     = Factores.Mes
cCampo    = 'FAC'+LEFT(cmes(VAL(m.Mes)),3)
nReg      = RECNO()
GO BOTT
IF nReg=RECNO()
	DELETE NEXT 1
	IF m.Mes # '00'
		REPLACE (cCampo) WITH 0 FOR Periodo = m.Periodo
	ENDIF
 ELSE
	DO STANDBY WITH 'Por Medida de seguridad. ElSistema solo Borrar  el ultimo Registro'
ENDIF


PROCEDURE Fin_opcion
*-------------------
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_1
CLOSE DATABASE
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
*SET DECIMALS TO nDec

RETURN

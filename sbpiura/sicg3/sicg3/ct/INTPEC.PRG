*-  ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*-  º IntPec.prg                                                      º
*-  º M¢dulo de integraci¢n de Pecosas                                 º
*-  º autor: Marco Rodriguez Abad                                      º
*-  º fecha: 15-04-2003                                                º
*-  ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

PUBLIC vTip
SET MEMOWIDTH TO 100

*set step on
*-apertura de archivos

USE parmae    IN 1 ORDER TAG Parmae1			ALIAS parma
USE Compro    IN 2 ORDER TAG Compro7			ALIAS Comp
USE Diario    IN 3 ORDER TAG Diario11			ALIAS Diar
USE AstPat	  IN 6 ORDER TAG AstPat36			ALIAS AstPat

*USE Pecosa    IN 4 ORDER TAG Pecosa1			ALIAS Pecosa
*USE ItePec    IN 5 ORDER TAG ItePec1			ALIAS ItePec

SELE Parma
SEEK "CONTAB"+"CIERRE"
MesCerrado = VAL(SUBSTR(Descri,5,2))
AnoCerrado = VAL(SUBSTR(Descri,1,4))

DO inicia
DO termina
RETURN


PROCEDURE Inicia
*---------------
DEFINE WINDOW Lis FROM 05,15 TO 13,65 title " ± Integraci¢n de Pecosas ± " COLOR SCHEME 5
ACTIVATE WINDOW Lis 

*- Mes y a¤o actual
SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
VMesPec  = IIF(FOUND(),MONTH(vmesact),MONTH(DATE()))
VAnoPec  = IIF(FOUND(),YEAR(vmesact), YEAR(DATE()))

@ 01,02 SAY "Fecha del Sistema:  " + dtoc(date())
@ 02,02 SAY "         Periodo : " GET vAnoPec PICTURE '9,999'
@ 03,02 SAY "             Mes : " GET vMesPec VALID val_para(PADL(vMesPec,2,'0'),'FECMES',' ',22) PICTURE '99'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesPec>MesCerrado AND vAnoPec=AnoCerrado) OR vAnoPec>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesPec = PADL(ALLTRIM(STR(vMesPec,2)),2,'0')
	VAnoPec = STR(VanoPec,4)
	
	SELE Parma
	SEEK 'TIPDOC'+'PEC'
	IF FOUND()
		vTip = ALLTRIM(Parma.CodigoAux)
	ENDIF
	
	IF !Hay_Pecosas()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		DO Pecosa
	ENDIF
ENDIF
RETURN


PROCEDURE Pecosa
*---------------
DO Espera WITH 1,'Preparando Informacion...'

SELECT DISTINCT PECOSA.PERIODO, PECOSA.NUMPEC, PECOSA.CODCAD,;
  PECOSA.FECPEC, PECOSA.ESTADO, PECOSA.CODDEP, PECOSA.CODFTE, PECOSA.CODMAY,;
  PECOSA.FECEMI, PECOSA.FECVER, PECOSA.OBSERVA, PECOSA.ATTE, PECOSA.DESTINO,;
  PECOSA.TIPPEC, PECOSA.USER, PECOSA.USER_CR, PECOSA.USER_FC, PECOSA.USER_TP,;
  PECOSA.DESTINAT, PECOSA.CARDESTI, PECOSA.OBSALMA, ITEPEC.FECDESP;
 FROM PECOSA, ITEPEC;
 WHERE ITEPEC.PERIODO = PECOSA.PERIODO;
   AND ITEPEC.NUMPEC = PECOSA.NUMPEC;
   AND PADL(MONTH((ITEPEC.FECDESP)),2,"0") = vMesPec;
 ORDER BY PECOSA.PERIODO, PECOSA.NUMPEC;
 INTO CURSOR Pecosa1

SELE Pecosa1

COUNT TO Registros
Actual = 0
DO Espera WITH 2

*SET RELATION TO Periodo+vMesPec+NumPec+'PEC' INTO AstPat ADDITIVE

GO TOP
IF EOF()
	DO STANDBY WITH "No hay informaci¢n para procesar"
	RETURN
ENDIF
SCAN
	Actual = Actual + 1
	do progreso with 1,PADC('Procesando : '+'Asientos de Pecosa. N§ '+Pecosa1.NumPec+' '+Pecosa1.codfte,59,' ')
	DO Progreso WITH 2,'',REGISTROS,ACTUAL
*	IF !EOF("AstPat")
		*- Para el archivo compro.dbf
		m.tipdoc  = vTip
		m.comprob = vTip+PADL(Pecosa1.NumPec,5,"0")+vMesPec
		m.comadi  = PADL(DAY(Pecosa1.fecDesp),2,'0')
		m.codfte  = Pecosa1.codfte
		m.codCad  = Pecosa1.codCad
		m.codcal  = SUBS(DTOC(Pecosa1.fecPec),9,2) + VmesPec  + Pecosa1.codfte
		m.fecha   = Pecosa1.FecDesp
		m.glosa1  = "Pecosa # "+Pecosa1.NumPec
		m.glosa2  = SUBSTR(ObsAlma,1,100)
		m.glosa3  = SUBSTR(ObsAlma,101,100)
		m.glosa4  = "Fecha de Despacho: "+DTOC(Pecosa1.FecDesp)
		m.estado  = ''
		m.tipdcr  = ''
		m.docref  = Pecosa1.numPec
		m.codctc  = ''
		m.numchq  = ''
		m.fecope  = date()
		m.tipobr  = ''
		m.codobra = ''
*		m.usuario = login()
		m.fecope  = date()
		m.totdeb  = 0
		m.tothab  = 0
		
		SELECT comp
		IF f_appd()
			GATHER MEMVAR
		ENDIF
		
		*-Para el diario
		
		m.Item = 0
		
*		SELE AstPat
*		SEEK Pecosa1.Periodo+vMesPec+Pecosa1.NumPec+'PEC'
*		SCAN WHILE AstPat.NumRef=PECOSA1.NumPec AND TipDoc='PEC'
		FOR i = 1 to 2
			IF Fecha=Pecosa1.FecDesp
				m.item      = m.Item + 1
				m.orden     = '01'
				m.nummov    = 1
				m.tipdes    = '1'
				m.cuenta    = ''
				m.tipo      = IIF(i=1,'D',IIF(i=2,'H',''))
				m.montos    = 0
				m.tipaux    = ''
				m.docref    = Pecosa1.NumPec
				m.codctc    = ''
				m.coddep    = '000001'
				m.tipdcr    = ''
				m.codfte    = Pecosa1.codfte
				m.fecemi    = Pecosa1.Fecdesp
				m.registro  = ''
				IF i=1
					m.totdeb = m.totdeb+m.Montos
				ENDIF
				IF i=2
					m.totHab = m.totHab+m.Montos
				ENDIF
				SELECT diar
				IF f_appd()
					GATHER MEMVAR
				ENDIF
			ENDIF
		ENDFOR
		
		SELE Comp
		SEEK m.Comprob
		IF FOUND()
			REPLACE TotDeb WITH m.TotDeb
			REPLACE TotHab WITH m.TotHab
		ENDIF
*	 ELSE
*		* En el caso de No Exixtir Cuentas
*		LOOP
*	ENDIF
	SELECT Pecosa1
ENDSCAN
SET RELATION TO

SET CONSOLE ON
SET BELL TO 800,5
_WRAP = .F.
? CHR(7)+CHR(7)+CHR(7)
_WRAP = .T.
SET BELL TO 512,2
SET BELL TO 512,2
SET CONSOLE OFF

DO Progreso WITH 3
DO standby WITH 'Proceso Finalizado. Pecosas integradas...!'
RETURN


PROCEDURE termina
*----------------
CLOS DATABASES
RETURN



FUNCTION Hay_Pecosas
*-------------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo Pecosas
SEEK vtip+vMesPec+vAnoPec
mRet=.F.

IF FOUND()
	IF yesno("Ya se han procesado Pecosas. ¨Desea anularlas y reprocesar de nuevo?")
		DO ESPERA WITH 1,'Eliminando Asientos de Pecosas procesadas del mes ' + cMes(VAL(vMesPec))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesPec+vAnoPec
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesPec AND STR(YEAR(Diar.fecemi),4)=vAnoPec
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesPec AND STR(YEAR(Comp.Fecha),4)=vAnoPec
			SELECT Comp
			SET ORDER TO Compro7
			UNLOCK
		ENDIF
		DO Espera WITH 2
		mRet=.F.
	 ELSE
		mRet=.T.
	ENDIF
ENDIF

SET ORDER TO Diario11 IN Diar
SET ORDER TO Compro7  IN Comp

RETURN mret


*-  ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*-  º intfact.prg                                                      º
*-  º M¢dulo de integraci¢n de facturas                                º
*-  º autor: Marco Rodriguez Abad                                      º
*-  º fecha: 04-08-2000                                                º
*-  ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

PUBLIC vTip
SET MEMOWIDTH TO 100

*-apertura de archivos

USE parmae    IN 1 ORDER TAG Parmae1			ALIAS parma
USE Compro    IN 2 ORDER TAG Compro7			ALIAS Comp
USE Diario    IN 3 ORDER TAG Diario11			ALIAS Diar
USE Recing    IN 4 ORDER TAG RecIng1			ALIAS Recing
USE AstPat	  IN 5 ORDER TAG AstPat36			ALIAS AstPat
USE AstPre    IN 6 ORDER TAG AstPre5			ALIAS AstPre
USE AstOrd    IN 7 ORDER TAG AstOrd1			ALIAS AstOrd

SELE Parma
SEEK "CONTAB"+"CIERRE"
MesCerrado = VAL(SUBSTR(Descri,5,2))
AnoCerrado = VAL(SUBSTR(Descri,1,4))

DO inicia
DO termina
RETURN

PROCEDURE Inicia
*---------------
DEFINE WINDOW Lis FROM 05,15 TO 13,65 title " ± Integraci¢n de Facturas ± " COLOR SCHEME 5
ACTIVATE WINDOW Lis 

*- Mes y a¤o actual
SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
VMesFac  = IIF(FOUND(),MONTH(vmesact),MONTH(DATE()))
VAnoFac  = IIF(FOUND(),YEAR(vmesact), YEAR(DATE()))

@ 01,02 SAY "Fecha del Sistema:  " + dtoc(date())
@ 02,02 SAY "         Periodo : " GET vAnoFac PICTURE '9,999'
@ 03,02 SAY "             Mes : " GET vMesFac VALID val_para(PADL(vMesFac,2,'0'),'FECMES',' ',22) PICTURE '99'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesFac>MesCerrado AND vAnoFac=AnoCerrado) OR vAnoFac>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesFac = PADL(ALLTRIM(STR(vMesFac,2)),2,'0')
	VAnoFac = STR(VanoFac,4)
	
	SELE Parma
	SEEK 'TIPDOC'+'FAC'
	IF FOUND()
		vTip = ALLTRIM(Parma.CodigoAux)
	ENDIF
	
	IF !Hay_Facturas()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		DO integF
	ENDIF
ENDIF
RETURN



PROCEDURE integF
*---------------
DO Espera WITH 1,'Preparando Informacion...'
SELECT RecIng
SET FILTER TO RecIng.nummes=vMesFac .AND. RecIng.periodo=SUBSTR(vAnoFac,3,2)
COUNT TO Registros
Actual = 0
DO Espera WITH 2

SET RELATION TO Periodo+NumMes+NumRi+'FAC' INTO AstPat ADDITIVE
SET RELATION TO NumMes+NumRi+'FAC'		   INTO AstPre ADDITIVE
SET RELATION TO Periodo+NumMes+NumRi+'FAC' INTO AstOrd ADDITIVE

GO TOP
IF EOF()
	DO STANDBY WITH "No hay informaci¢n para procesar"
	RETURN
ENDIF
SCAN
	Actual = Actual + 1
	do progreso with 1,PADC('Procesando : '+'Asiento de Fact. del R/I N§ '+RecIng.NumRi+' '+RecIng.codfte,59,' ')
   	DO Progreso WITH 2,'',REGISTROS,ACTUAL
	IF !EOF("AstPat") OR !EOF("AstPre") OR !EOF("AstOrd")
		*- Para el archivo compro.dbf
		m.tipdoc  = vTip
		m.comprob = vTip+PADL(RecIng.NumRi,5,"0")+vMesFac
		m.comadi  = ''
		m.codfte  = RecIng.codfte
		m.codCad  = RecIng.codCad
		m.codcal  = SUBS(DTOC(RecIng.fecRI),9,2) + VmesFac  + RecIng.codfte
		m.fecha   = RecIng.FecRI
		m.glosa1  = "Fact. por ingreso # "+RecIng.NumRi
		m.glosa2  = MLINE(Observ,1)
		m.glosa3  = MLINE(Observ,2)
		m.glosa4  = MLINE(Observ,3)
		m.estado  = ''
		m.tipdcr  = ''
		m.docref  = RecIng.numri
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
		
*		@ 05,13 say 'Asiento de Fact. del R/I N§ '+RecIng.NumRi+' '+RecIng.codfte
		
		*-Para el diario
		
		m.Item = 0
		
		SELE AstPat
		SEEK RecIng.Periodo+RecIng.NumMes+RecIng.NumRI+'FAC'
		SCAN WHILE AstPat.NumRef=RecIng.NumRi AND TipDoc='FAC'
			m.item      = m.Item + 1
			m.orden     = IIF(RecIng.TipRI#'51','06','45')
			m.nummov    = 1
			m.tipdes    = '1'
			m.cuenta    = AstPat.CodCta
			m.tipo      = AstPat.TipCta
			m.montos    = IIF(m.Tipo='D',AstPat.MtoDeb,AstPat.MtoHab)
			m.tipaux    = RecIng.tipaux
			m.docref    = RecIng.NumRi
			m.codctc    = ''
			m.coddep    = '000001'
			m.tipdcr    = ''
			m.codfte    = RecIng.codfte
			m.fecemi    = m.fecha
			m.registro  = RecIng.codprv
			
			IF m.Tipo='D'
				m.totdeb = m.totdeb+m.Montos
			 ELSE
				m.totHab = m.totHab+m.Montos
			ENDIF
			
			SELECT diar
			IF f_appd()
				GATHER MEMVAR
			ENDIF
			SELE AstPat
		ENDSCAN
		
		SELE AstPre
		SEEK RecIng.NumMes+RecIng.NumRI+'FAC'
		SCAN WHILE AstPre.NumRef=RecIng.NumRi AND TipDoc='FAC'
			m.item      = m.Item + 1
			m.nummov    = 1
			m.tipdes    = '1'
			m.orden     = '08'
			m.tipo      = AstPre.Tipo
			m.cuenta    = IIF(m.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
			m.montos    = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
			m.codctc    = ''
			m.coddep    = '000001'
			
			IF m.Tipo='D'
				m.totdeb = m.totdeb+m.Montos
			 ELSE
				m.totHab = m.totHab+m.Montos
			ENDIF
		
			SELECT diar
			IF f_appd()
				GATHER MEMVAR
			ENDIF
			SELE AstPre
		ENDSCAN
		
		SELE AstOrd
		SEEK RecIng.Periodo+RecIng.NumMes+RecIng.NumRI+'FAC'
		SCAN WHILE AstOrd.NumRef=RecIng.NumRi AND TipDoc='FAC'
			m.item      = m.Item + 1
			m.nummov    = 1
			m.tipdes    = '1'
			m.orden     = '07'
			m.tipo      = AstOrd.TipCta
			m.cuenta    = AstOrd.CodCta
			m.montos    = IIF(m.Tipo='D',AstOrd.MtoDeb,AstOrd.MtoHab)
			m.codctc    = ''
			m.coddep    = '000001'
			
			IF m.Tipo='D'
				m.totdeb = m.totdeb+m.Montos
			 ELSE
				m.totHab = m.totHab+m.Montos
			ENDIF
		
			SELECT diar
			IF f_appd()
				GATHER MEMVAR
			ENDIF
			SELE AstOrd
		ENDSCAN
		SELE Comp
		SEEK m.Comprob
		IF FOUND()
			REPLACE TotDeb WITH m.TotDeb
			REPLACE TotHab WITH m.TotHab
		ENDIF
	 ELSE
		* En el caso de No Exixtir Cuentas
		LOOP
	ENDIF
	SELECT RecIng
ENDSCAN

SET CONSOLE ON
SET BELL TO 800,5
_WRAP = .F.
? CHR(7)+CHR(7)+CHR(7)
_WRAP = .T.
SET BELL TO 512,2
SET BELL TO 512,2
SET CONSOLE OFF

DO Progreso WITH 3
DO standby WITH 'Proceso Finalizado. Facturas integradas...!'
RETURN

PROCEDURE termina
*----------------
CLOS DATABASES
RETURN



FUNCTION Hay_Facturas
*-------------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo FACTURAS
SEEK vtip+vMesFAC+vAnoFAC
mRet=.F.

IF FOUND()
	IF yesno("Ya se han procesado Facturas. ¨Desea anularlas y reprocesar de nuevo?")
		DO ESPERA WITH 1,'Eliminando Asientos de Facturas procesadas del mes ' + cMes(VAL(vMesFac))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesFac+vAnoFac
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesFac AND STR(YEAR(Diar.fecemi),4)=vAnoFac
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesFac AND STR(YEAR(Comp.Fecha),4)=vAnoFac
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


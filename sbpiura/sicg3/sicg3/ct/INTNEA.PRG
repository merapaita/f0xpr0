*-  ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*-  º IntNea.prg                                                       º
*-  º M¢dulo de integraci¢n de Neas                                    º
*-  º autor: Marco Rodriguez Abad                                      º
*-  º fecha: 15-04-2003                                                º
*-  ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

PUBLIC vTip
SET MEMOWIDTH TO 100

*-apertura de archivos

USE parmae    IN 1 ORDER TAG Parmae1			ALIAS parma
USE Compro    IN 2 ORDER TAG Compro7			ALIAS Comp
USE Diario    IN 3 ORDER TAG Diario11			ALIAS Diar
USE AstPat	  IN 4 ORDER TAG AstPat36			ALIAS AstPat

SELE Parma
SEEK "CONTAB"+"CIERRE"
MesCerrado = VAL(SUBSTR(Descri,5,2))
AnoCerrado = VAL(SUBSTR(Descri,1,4))

DO inicia
DO termina
RETURN


PROCEDURE Inicia
*---------------
DEFINE WINDOW Lis FROM 05,15 TO 13,65 title " ± Integraci¢n de Neas ± " COLOR SCHEME 5
ACTIVATE WINDOW Lis 

*- Mes y a¤o actual
SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
VMesNEA  = IIF(FOUND(),MONTH(vmesact),MONTH(DATE()))
VAnoNEA  = IIF(FOUND(),YEAR(vmesact), YEAR(DATE()))

@ 01,02 SAY "Fecha del Sistema:  " + dtoc(date())
@ 02,02 SAY "         Periodo : " GET vAnoNEA PICTURE '9,999'
@ 03,02 SAY "             Mes : " GET vMesNEA VALID val_para(PADL(vMesNEA,2,'0'),'FECMES',' ',22) PICTURE '99'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesNEA>MesCerrado AND vAnoNEA=AnoCerrado) OR vAnoNEA>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesNEA = PADL(ALLTRIM(STR(vMesNEA,2)),2,'0')
	VAnoNEA = STR(VanoNEA,4)
	
	SELE Parma
	SEEK 'TIPDOC'+'NEA'
	IF FOUND()
		vTip = ALLTRIM(Parma.CodigoAux)
	ENDIF
	
	IF !Hay_NEAs()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		DO Neas
	ENDIF
ENDIF
RETURN


PROCEDURE Neas
*-------------
DO Espera WITH 1,'Preparando Informacion...'

USE NotAlm IN 5 ORDER TAG NotAlm1 ALIAS NotAlm
USE IteAlm IN 6 ORDER TAG IteAlm1 ALIAS IteAlm
SET FILTER TO NotAlm.NumMes=vMesNEA AND NotAlm.Periodo=RIGHT(vAnoNEA,2) AND NotAlm.Estado # "99"

SELE NotAlm

COUNT TO Registros
Actual = 0
DO Espera WITH 2

SET RELATION TO Periodo+vMesNEA+NumAlm+'NEA' INTO AstPat ADDITIVE

GO TOP
IF EOF()
	DO STANDBY WITH "No hay informaci¢n para procesar"
	RETURN
ENDIF
SCAN
	Actual = Actual + 1
	do progreso with 1,PADC('Procesando : '+'Asientos de NEAs. N§ '+NotAlm.NumAlm+' '+NotAlm.codfte,59,' ')
	DO Progreso WITH 2,'',REGISTROS,ACTUAL
	IF !EOF("AstPat")
		*- Para el archivo compro.dbf
		m.tipdoc  = vTip
		m.comprob = vTip+PADL(NotAlm.NumAlm,5,"0")+vMesNEA
		m.comadi  = ""
		m.codfte  = NotAlm.codfte
		m.codCad  = NotAlm.codCad
		m.codcal  = ""
		m.fecha   = NotAlm.FecAlm
		m.glosa1  = "Nota de Entrada a Almacen # "+NotAlm.NumAlm
		m.glosa2  = NotAlm.Destino
		m.glosa3  = NotAlm.Segun
		m.glosa4  = NotAlm.Observa
		m.estado  = ''
		m.tipdcr  = ''
		m.docref  = NotAlm.numAlm
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
		
		SELE AstPat
		SEEK NotAlm.Periodo+NotAlm.NumMes+NotAlm.NumAlm+'NEA'
		SCAN WHILE AstPat.NumRef=NotAlm.NumAlm AND TipDoc='NEA'
			m.item      = m.Item + 1
			m.orden     = '01'
			m.nummov    = 1
			m.tipdes    = '1'
			m.cuenta    = AstPat.CodCta
			m.tipo      = AstPat.TipCta
			m.montos    = IIF(m.Tipo='D',AstPat.MtoDeb,AstPat.MtoHab)
			m.tipaux    = ''
			m.docref    = NotAlm.NumAlm
			m.codctc    = ''
			m.coddep    = '000001'
			m.tipdcr    = ''
			m.codfte    = NotAlm.codfte
			m.fecemi    = NotAlm.FecAlm
			m.registro  = ''
			
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
	SELECT NotAlm
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
DO standby WITH 'Proceso Finalizado. Neas integradas...!'
RETURN


PROCEDURE termina
*----------------
CLOS DATABASES
RETURN



FUNCTION Hay_NEAs
*----------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo Neas
SEEK vtip+vMesNea+vAnoNea
mRet=.F.

IF FOUND()
	IF yesno("Ya se han procesado NEAs. ¨Desea anularlas y reprocesar de nuevo?")
		DO ESPERA WITH 1,'Eliminando Asientos de Neas procesadas del mes ' + cMes(VAL(vMesNEA))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesNEA+vAnoNEA
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesNEA AND STR(YEAR(Diar.fecemi),4)=vAnoNEA
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesNEA AND STR(YEAR(Comp.Fecha),4)=vAnoNEA
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


** --------------------------------------
** INTBD.PRG
** Integraci¢n por lotes de Boletas de Dep¢sito
** AUTOR   : UDEP - 1995. FMC - Version 5.0
** Fecha   : 06/11/95 
** Modif.  : M.R.A.  29/08/2000
** ---------------------------------------

USE Parmae    IN  4 ORDER TAG Parmae1                 ALIAS Parma
IF blo_sis(1)
   DO STANDBY WITH "Sistema Bloqueado, Consulte al Area de Sistemas"
   CLOS DATA
   RETURN 
ENDIF   
SEEK "CONTAB"+"CIERRE"
MesCerrado = VAL(SUBSTR(Descri,5,2))
AnoCerrado = VAL(SUBSTR(Descri,1,4))

USE Compro  IN  1 ORDER TAG Compro7  ALIAS Comp
USE Diario  IN  2 ORDER TAG Diario11 ALIAS Diar
USE Cuentas IN  3 ORDER TAG Cuentas1 ALIAS Cuen
USE AstPat  IN 12 ORDER TAG AstPat7  ALIAS AstPat
USE MovBco  IN 13 ORDER TAG MovBco7  ALIAS MovBco

DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Fin_opcion                && finaliza

RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------

DEFINE WINDOW LIS FROM 5,15 TO 13,65 FLOAT DOUBLE TITLE '  Integrando Boletas de Dep¢sito   ' COLOR SCHEME 5
ACTIVATE WINDOW LIS

SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
vmesbd  = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
vanobd  = IIF(FOUND(), YEAR(vmesact),   YEAR(DATE()))

@ 1, 0 SAY '    Mes y A¤o:' GET vmesbd PICTURE '99'    VALID vmesbd > 0 .AND. vmesbd <= 12
@ 1,20 GET vanobd                      PICTURE '9,999'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesBD>MesCerrado AND vAnoBD=AnoCerrado) OR vAnoBD>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	vmesbd   = PADL(ALLTRIM(STR(vmesbd,2)),2,'0')
	vanobd   = STR(vanobd,4)
	
	SELE Parma
	SEEK 'TIPDOC'+'B/D'
	IF !FOUND()
		=STANDBY('Parametro de Boletas de Dep¢sito no Inicializado')
		RETURN
	ENDIF
	vTip = ALLTRIM(Parma.CodigoAux)
	
	IF !HAYBD()
		SET ORDER TO Diario11 IN Diar
		SET ORDER TO Compro7  IN Comp
		IF yesno('Esta Ud. Seguro de Integrar B/D?')
			DO BolDep
		ENDIF
	ENDIF
ENDIF
RETURN

PROCEDURE BolDep
*--------------
DO Espera WITH 1,'Preparando Informacion...'
SELECT MovBco
SET FILTER TO Movbco.Periodo=RIGHT(vanobd,2) AND movbco.tipdoc = 'B/D' AND MONTH(FECHAc)=VAL(vmesbd)
COUNT TO Registros
Actual = 0
DO Espera WITH 2

GO TOP
IF EOF()
	DO STANDBY WITH 'No existe Documentos (B/D) del Movimiento Bancario ...!'
	RETURN .F.
ENDIF

SCAN
	Actual = Actual + 1
	do progreso with 1,PADC('Procesando : '+Movbco.Numero+'-'+DTOC(MovBco.Fecha)+'-'+ALLTRIM(Movbco.Codctc),59,' ')
   	DO Progreso WITH 2,'',REGISTROS,ACTUAL
   	
*	@ 07,01  SAY ' Procesando : '+Movbco.Numero+'  '+DTOC(MovBco.Fecha)+'  '+ALLTRIM(Movbco.Codctc)
	m.TipDoc     = VTIP
	m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+vmesbd
	m.ComAdi     = ''
	m.Fecha      = MovBco.FechaC
	m.Glosa1     = ALLT(MovBco.Concept)+'  '+Movbco.NumRef
	m.Glosa2     = ALLT(MovBco.Nombre) +'  '+Movbco.transa
	m.Glosa3     = Movbco.Codctc+' Mes:'+Movbco.Nummes
	m.Codctc     = Movbco.Codctc
	m.CodCaD     = Movbco.CodCad
	m.CODFTE     = Movbco.Codfte
	m.DocRef     = MovBco.Numero
	m.Nummesref  = Movbco.Nummes
	m.Periodoref = vanobd
	m.Nummesfte  = Movbco.Nummes
	m.Periodofte = vanobd
	m.Numdocfte  = Movbco.Numero
	m.TipDcr     = Val_para6(Movbco.Tipdoc,'TIPDOC')
	
	SELECT Comp
	IF F_APPD()
		GATHER MEMVAR
		UNLOCK
	ENDIF
	
	m.Item  = 0
	xTotDeb = 0
	xTotHab = 0
	SELECT AstPat
	SEEK Movbco.NumMes+Movbco.Numero		&&+Movbco.Codctc
	SCAN WHILE  MovBco.Nummes=Astpat.Nummes .AND. MovBco.Numero = Astpat.Numref
		m.Item       = m.Item + 1
		m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+vmesbd
		m.NumMov     = 1
		m.FecEmi     = MovBco.FechaC
		m.Tipo       = AstPat.TipCta
		m.NumChq     = ALLTRIM(MovBco.NumREF)
		m.Codctc     = Movbco.Codctc
		m.TipAux     = '01'
		m.Registro   = MovBco.Codban
		m.Codfte     = MovBco.Codfte
		m.Nummesref  = Movbco.Nummes
		m.Periodoref = vanobd
		m.Coddep     = '113000'
		m.Docref     = Movbco.NumRI
		m.Nummesref  = Movbco.NummesRI
		m.PeriodoRef = Movbco.PerRi
		m.Tipdcr     = '25'
		m.Cuenta     = AstPat.CodCta
		m.Montos     = IIF(m.Tipo='D',AstPat.MToDeb,AstPat.MToHab)
		
		IF m.Tipo='H'
			m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','58','57')
		 ELSE
			m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','57','58')
		ENDIF
		
		xtotDeb = xtotDeb + IIF(m.Tipo='D',m.Montos,0)
		xtotHab = xtotHab + IIF(m.Tipo='H',m.Montos,0)
		
		SELECT Diar
		IF f_appd()
			GATHER MEMVAR
			UNLOCK
		ENDIF
	ENDSCAN
	
	SELE Comp
	IF RLOCK()
		REPLACE Comp.totdeb     WITH xtotdeb
		REPLACE Comp.tothab     WITH xtothab
		REPLACE comp.fecope     WITH date()
		UNLOCK
	ENDIF
	xtotdeb=0
	xtothab=0
	m.Item = 0
	SELE Movbco
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
DO Standby WITH 'Proceso Finalizado; Movimiento Bancario  B/D Integradas ...!'
RETURN

FUNCTION val_para6
*----------------
PARAMETERS mvalor, filtro
SELECT parma
SEEK filtro+mvalor
IF FOUND()
   RETURN  parma.codigoaux 
ELSE
   RETURN  '  '    
ENDIF
RETURN 

PROCEDURE Fin_opcion
*-------------------
DO Logos WITH Rotulo1,Rotulo2
CLOSE DATA
RETURN

FUNCTION HAYBD
*-------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo B/D
SEEK vtip+vMesBD+vAnoBD
mRet=.F.

IF FOUND()
	IF yesno('B/D del Mes '+vMesBD+'-'+vAnoBD+' Ya integrado, Reprocesa?')
		DO ESPERA WITH 1,'Eliminando Asientos de B/D procesadas del mes ' + cMes(VAL(vMesBD))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesBD+vAnoBD
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesBD AND STR(YEAR(Diar.fecemi),4)=vAnoBD
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesBD AND STR(YEAR(Comp.Fecha),4)=vAnoBD
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

RETURN mRet

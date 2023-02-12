** --------------------------------------
** INTNA.PRG
** Integraci¢n por lotes de movimiento Bancario N/A
** AUTOR   : UDEP - 1995. FMC - Version 5.0
** Fecha   : 06/11/95 
** Estados : 00 Pendiente de Cancelaci¢n
**           50 Cancelado
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

USE AstPre    IN  8 ORDER TAG AstPRE5                ALIAS AstPre
USE Compro    IN  1 ORDER TAG Compro7                 ALIAS Comp
USE Diario    IN  2 ORDER TAG Diario11                ALIAS Diar
USE AstPat    IN 12 ORDER TAG AstPat6                 ALIAS AstPat
USE MovBco    IN 13 ORDER TAG MovBco9                 ALIAS MovBco
*USE Cajas     IN 10 ORDER TAG Cajas1                  ALIAS Caja
*USE AutFin    IN 09 ORDER TAG AutFin1                 ALIAS AutFin

DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Fin_opcion                && finaliza

RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 13,65 FLOAT DOUBLE TITLE '  Integrando Notas de Abono   ' COLOR SCHEME 5
ACTIVATE WINDOW LIS

SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
vmesNA  = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
vanoNA  = IIF(FOUND(), YEAR(vmesact),   YEAR(DATE()))
@ 1, 0 SAY '    Mes y A¤o:' GET vmesNA PICTURE '99' VALID vmesNA > 0 .AND. vmesNA <= 12
@ 1,20 GET vanoNA                      PICTURE '9,999'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesNA>MesCerrado AND vAnoNA=AnoCerrado) OR vAnoNA>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	vmesNA   = PADL(ALLTRIM(STR(vmesNA,2)),2,'0')
	vanoNA   = STR(vanoNA,4)
	SELE Parma
	
	SEEK 'TIPDOC'+'N/A'
	IF FOUND()
		vTip = ALLTRIM(Parma.CodigoAux)
	ENDIF
	
	IF !HAYNA()
		SET ORDER TO Diario11 IN Diar
		SET ORDER TO Compro7  IN Comp
		IF yesno('Esta Ud. Seguro de Integrar N/A?')
			DO NotAbo
		ENDIF
	ENDIF
ENDIF
RETURN
	
PROCEDURE NotAbo
*---------------
DO Espera WITH 1,'Preparando Informacion...'
SELE Movbco
SET FILTER TO movbco.nummes=vmesNA AND STR(YEAR(Movbco.Fecha),4)=vanoNA
GO TOP
COUNT TO Registros
Actual = 0
DO Espera WITH 2

SCAN
	Actual = Actual + 1
	do progreso with 1,PADC('Procesando : '+Movbco.Numero+'  '+DTOC(MovBco.Fecha)+'  '+ALLTRIM(Movbco.Codctc),59,' ')
   	DO Progreso WITH 2,'',REGISTROS,ACTUAL

*	@ 7,01  SAY ' Procesando : '+Movbco.Numero+'  '+DTOC(MovBco.Fecha)+'  '+ALLTRIM(Movbco.Codctc)
	m.TipDoc     = VTIP
	m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+vmesNA
	m.Fecha      = MOVBCO.FechaC
	m.Glosa1     = MOVBCO.Concept+'  '+Movbco.NumREF
	m.Glosa2     = MOVBCO.Nombre
	m.Glosa3     = Movbco.Codctc+' Mes:'+Movbco.Nummes
	m.Codctc     = Movbco.Codctc
	m.CodCaD     = Movbco.CodCAD
	m.CodFTE     = Movbco.CodFTE
	m.DocRef     = MovBco.Numero
	m.Nummesref  = Movbco.Nummes
	m.Periodoref = vAnoNA
	m.Nummesfte  = Movbco.Nummes
	m.Periodofte = vanoNA
	m.Numdocfte  = Movbco.Numero
	
	SELECT Comp
	IF F_APPD()
		GATHER MEMVAR
		UNLOCK
	ENDIF
	
	m.Item   = 0
	m.tipdcr = Val_para6(Movbco.Tipdoc,'TIPDOC') 
	xTotDeb  = 0
	xTotHab  = 0

	SELECT AstPat
	
	SEEK Movbco.NumMes+Movbco.Numero			&&+Movbco.Codctc 
	SCAN WHILE  MovBco.Nummes=Astpat.Nummes .AND. MovBco.Numero = Astpat.Numref
		m.Item       = m.Item + 1
		m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+vmesNA
		m.NumMov     = 1
		m.FecEmi     = MovBco.FechaC
		m.Tipo       = AstPat.TipCta
		m.Docref     = ALLTRIM(Movbco.Numero)  
		m.NumChq     = ALLTRIM(MovBco.NumREF)
		m.Codctc     = Movbco.Codctc
		m.TipAux     = '01'
		m.Registro   = MOVBCO.Codban
		m.Codfte     = Movbco.Codfte
		m.Nummesref  = Movbco.Nummes
		m.Periodoref = vanoNA
		m.Coddep     = '113000'
		m.Cuenta     = AstPat.CodCta
		m.Montos     = IIF(m.Tipo='D',AstPat.MToDeb,AstPat.MToHab)
		IF m.Tipo='H'
			m.Orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','58',IIF(LEFT(m.cuenta,3)='771','45','57'))
		 ELSE
			m.Orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','57',IIF(LEFT(m.cuenta,3)='126','45','58'))
		ENDIF
		
		xTotDeb      = xTotDeb + IIF(Tipo='D',Montos,0)
		xTotHab      = xTotHab + IIF(Tipo='H',Montos,0)
		
		SELECT Diar
		IF f_appd()
			GATHER MEMVAR
			UNLOCK
		ENDIF
		SELE AstPat
	ENDSCAN
	
	SELECT AstPre
	SEEK Movbco.NumMes+Movbco.Numero+"N/A"			&&+Movbco.Codctc
	SCAN WHILE  MovBco.Nummes=AstPre.Nummes .AND. MovBco.Numero = AstpRE.Numref AND AstPre.TipDoc='N/A'
		m.Item       = m.Item + 1
		m.ORDEN      = '01'
		m.NumMov     = 1
		m.FecEmi     = MOVBCO.FechaC
		m.Tipo       = AstPRE.Tipo
		m.Docref     = ALLTRIM(Movbco.Numero)
		m.NumChq     = ALLTRIM(MovBco.NumREF)
		m.Codctc     = Movbco.Codctc
		m.TipAux     = '01'
		m.Registro   = MOVBCO.Codban
		m.Codfte     = Movbco.Codfte
		m.Nummesref  = Movbco.Nummes
		m.Periodoref = vanobco
		m.Cuenta     = AstPre.cuenta
		m.Montos     = IIF(m.Tipo='D',AstPre.valdeb,AstPre.valhab)
		
		xtotDeb = xtotHab+IIF(m.Tipo='D',m.Montos,0)
		xtotHab = xtotHab+IIF(m.Tipo='H',m.Montos,0)
		
		SELECT Diar
		IF F_APPD()
			GATHER MEMVAR
		ENDIF
		SELE AstPre
	ENDSCAN
	
	SELE Comp
	IF RLOCK()
		REPLACE Comp.totdeb WITH xtotdeb
		REPLACE Comp.tothab WITH xtothab
		UNLOCK
	ENDIF
	
	xtotdeb=0
	xtothab=0
	vItem = 0
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
DO Standby WITH 'Proceso Finalizado; Movimiento Bancario  N/A Integradas ..!'
RETURN

FUNCTION val_para6
*-----------------
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

FUNCTION HAYNA
*-------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo N/A
SEEK vtip+vMesNA+vAnoNA
mRet=.F.

IF FOUND()
	IF yesno('N/A del Mes '+vMesNA+'-'+vAnoNA+' Ya integrado, Reprocesa?')
		DO ESPERA WITH 1,'Eliminando Asientos de N/A procesadas del mes ' + cMes(VAL(vMesNA))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesNA+vAnoNA
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesNA AND STR(YEAR(Diar.fecemi),4)=vAnoNA
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesNA AND STR(YEAR(Comp.Fecha),4)=vAnoNA
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

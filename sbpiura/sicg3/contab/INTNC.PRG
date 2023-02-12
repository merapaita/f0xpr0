** --------------------------------------
** INTNC.PRG
** Integraci¢n por lotes de Notas de Cargo
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

USE Compro    IN  1 ORDER TAG Compro7                 ALIAS Comp
USE Diario    IN  2 ORDER TAG Diario11                ALIAS Diar
USE AstPat    IN 12 ORDER TAG AstPat5                 ALIAS AstPat
USE MovBco    IN 13 ORDER TAG MovBco8                 ALIAS MovBco
USE AstPre    IN 14 ORDER TAG AstPre5                 ALIAS AstPre

DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Fin_opcion                && finaliza

RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 13,65 FLOAT DOUBLE TITLE '  Integrando Notas de Cargo   ' COLOR SCHEME 5
ACTIVATE WINDOW LIS

SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
vmesNC  = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
vanoNC  = IIF(FOUND(), YEAR(vmesact),   YEAR(DATE()))

@ 1, 0 SAY '    Mes y A¤o:' GET vmesNC PICTURE '99' VALID vmesNC > 0 .AND. vmesNC <= 12
@ 1,20 GET vanoNC                      PICTURE '9,999'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesNC>MesCerrado AND vAnoNC=AnoCerrado) OR vAnoNC>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	vmesNC = PADL(ALLTRIM(STR(vmesNC,2)),2,'0')
	vanoNC = STR(vanoNC,4)
	
	SELE Parma

***********************
* Esto se debe de Correejir desde las opciones que utilizan lan notas de cargo
***********************
	SEEK 'TIPDOC'+'N/C'
	IF FOUND()
*		vTip = ALLTRIM(Parma.CodigoAux)
	ENDIF
	vTip = '50'

	IF !HAYNC()
		SET ORDER TO Diario11 IN Diar
		SET ORDER TO Compro7  IN Comp
		IF yesno('Esta Ud. Seguro de Integrar N/C?')
			DO NotCar
		ENDIF
	ENDIF
ENDIF
RETURN

PROCEDURE NotCar
*---------------
DO Espera WITH 1,'Preparando Informacion...'
SELE Movbco
SET FILTER TO movbco.nummes=vmesNC  AND STR(YEAR(Movbco.Fecha),4)=vanoNC 
COUNT TO Registros
Actual = 0
DO Espera WITH 2

GO TOP
SCAN
	Actual = Actual + 1
	do progreso with 1,PADC('Procesando : '+Movbco.Numero+'  '+DTOC(MovBco.Fecha)+'  '+ALLTRIM(Movbco.Codctc),59,' ')
   	DO Progreso WITH 2,'',REGISTROS,ACTUAL
   	
*	@ 7,01  SAY 'Procesando : '+Movbco.Numero+'  '+DTOC(MovBco.Fecha)+'  '+ALLTRIM(Movbco.Codctc)
	m.TipDoc     = VTIP
	m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+vmesNC
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
	m.Periodoref = vanoNC
	m.Nummesfte  = Movbco.Nummes
	m.Periodofte = vanoNC
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
	SEEK Movbco.NumMes+Movbco.Numero				&&+Movbco.Codctc 
	SCAN WHILE  MovBco.Nummes=Astpat.Nummes .AND. MovBco.Numero = Astpat.Numref
		m.Item = m.Item + 1
		m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+vmesNC
		m.NumMov     = 1
		m.FecEmi     = MovBco.FechaC
		m.Tipo       = AstPat.TipCta
		m.NumChq     = ALLTRIM(MovBco.NumREF)
		m.Codctc     = Movbco.Codctc
		m.TipAux     = '01'
		m.Registro   = MovBco.Codban
		m.Codfte     = MovBco.Codfte
		m.Nummesref  = Movbco.Nummes
		m.Periodoref = vanoNC
		m.Coddep     = '113000'
		m.Docref     = ALLTRIM(Movbco.Numero)
		m.Nummesref  = Movbco.Nummes
		m.PeriodoRef = Movbco.Periodo
		m.Cuenta     = AstPat.CodCta
		m.Montos     = IIF(m.Tipo='D',AstPat.MToDeb,AstPat.MToHab)
		m.Codctc     = Movbco.Codctc
		
		IF Tipo='H'
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
		SELE AstPat
	ENDSCAN
	
	*-Cuentas Presupuestales de ejecuci¢n
*	SELECT AstPre
*	SEEK Movbco.NumMes+Movbco.Numero+"N/C"				&&+Movbco.Codctc
*	SCAN WHILE   Movbco.NumMes=AstPre.NumMes .and. Movbco.Numero = AstPre.NumRef AND AstPre.TipDoc="N/C"			&& .AND. Movbco.Codctc = AstPre.CodCtc
*		m.Item     = m.Item + 1
*		m.orden    = '32'
*		m.Comadi   = ''
*		m.NumMov   = 1
*		m.Tipo     = AstPre.Tipo
*		m.TipAux   = '01'
*		m.Registro = MovBco.Codban
*		m.CodCtc   = MovBco.CodCtc
*		m.DocRef   = MovBco.NumRef
*		m.Codpart  = Astpre.codpart
*		m.Cuenta   = IIF(m.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
*		m.Montos   = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
		
*		xtotDeb = xtotDeb + IIF(m.Tipo='D',m.Montos,0)
*		xtotHab = xtotHab + IIF(m.Tipo='H',m.Montos,0)
		
*		SELECT Diar
*		IF f_appd()
*			GATHER MEMVAR
*			UNLOCK
*		ENDIF
*		SELE AstPre
*	ENDSCAN
	
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
DO Standby WITH 'Proceso Finalizado; Movimiento Bancario  N/C Integradas ..!'
SET RELATION OFF INTO AstPat
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

FUNCTION HAYNC
*-------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo N/C
SEEK vtip+vMesNC+vAnoNC
mRet=.F.

IF FOUND()
	IF yesno('N/C del Mes '+vMesNC+'-'+vAnoNC+' Ya integrado, Reprocesa?')
		DO ESPERA WITH 1,'Eliminando Asientos de N/C procesadas del mes ' + cMes(VAL(vMesNC))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesNC+vAnoNC
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesNC AND STR(YEAR(Diar.fecemi),4)=vAnoNC
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesNC AND STR(YEAR(Comp.Fecha),4)=vAnoNC
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

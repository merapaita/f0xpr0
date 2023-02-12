** --------------------------------------
** INTRI.PRG
** Integraci¢n por lotes de la Hojas de Control
** AUTOR   : UDEP - 1995. FMC - Version 5.0
** Fecha   : 02/11/95
** Modif.  : M.R.A.  25/08/2000
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

DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Fin_opcion                && finaliza

RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 13,65 FLOAT DOUBLE TITLE '  Integrando Recibos de Ingreso  ' COLOR SCHEME 5
ACTIVATE WINDOW LIS
SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
vmesri   = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
vanori   = IIF(FOUND(),  YEAR(vmesact),  YEAR(DATE()))

@ 1,0   SAY '     Mes y A¤o:' GET vmesri     PICTURE '99' VALID vmesri > 0 .AND. vmesri <= 12
@ 1,20  GET vanori                           PICTURE '9,999'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesRI>MesCerrado AND vAnoRI=AnoCerrado) OR vAnoRI>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	vMesRI   = PADL(ALLTRIM(STR(vMesRI,2)),2,'0')
	vAnoRI   = STR(vAnoRI,4)
	
	SELE Parma
	SEEK 'TIPDOC'+'R/I'
	IF !FOUND()
		=STANDBY('Parametro de Recibosa de Ingreso no Inicializado')
		RETURN
	ENDIF
	vTip = ALLTRIM(Parma.CodigoAux)
	
	IF !HAYRI()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		IF yesno('Esta Ud. Seguro de Integrar R/I?')   
			DO RECING
		ENDIF
	ENDIF
ENDIF
RETURN
	
PROCEDURE RECING
*---------------
*-Carga valores de RECIBOS DE INGRESO

USE AstOrd	 IN  7 ORDER TAG AstOrd1	ALIAS AstOrd
USE AstPat	 IN 11 ORDER TAG AstPat14	ALIAS AstPat
USE RecIng	 IN 12 ORDER TAG RecIng1	ALIAS RecIng
USE Astpre	 IN 13 ORDER TAG Astpre20	ALIAS Astpre
USE AsiAutRI IN  3 ORDER TAG AsiAutRI1	ALIAS AsiAut

SELECT RecIng
SET RELATION TO Periodo+NumMes+NumRI		INTO AstPat ADDITIVE
SET RELATION TO Periodo+NumMes+NumRI		INTO AstPre ADDITIVE
SET RELATION TO Periodo+NumMes+NumRI+'R/I'	INTO AstOrd ADDITIVE
SET MEMOWIDTH TO 50

GO TOP
IF EOF()
	DO Standby WITH 'No existe Documentos Recibos de Ingreso'
	RETURN .F.
ENDIF
DO Espera WITH 1,'Preparando Informacion...'
SELECT RecIng
SET FILTER TO Recing.NumMes = vMesRI AND Recing.Periodo = RIGHT(vAnoRI,2) AND ReCING.ESTADO <> '99'
COUNT TO Registros
DO Espera WITH 2
Actual = 0
GO TOP
IF EOF()
	DO STANDBY WITH 'No existen Documentos a procesar....!'
 ELSE
	SCAN  && RecIng
		Actual = Actual + 1
		do progreso with 1,PADC('Procesando : '+Recing.NumRI+'-'+DTOC(Recing.Fecri),59,' ')
    	DO Progreso WITH 2,'',REGISTROS,ACTUAL
		
		REPLACE Recing.EstCon WITH "50"
		
		m.TipDoc     = VTIP
		m.Comprob    = VTIP+PADL(RecIng.NumRI,5,'0')+vmesri
		m.ComAdi     = ''
		m.Fecha      = RecIng.FecRI
		m.Glosa1     = Val_TipRI(Recing.TipRi) + 'Referencia : '+RecIng.DocRef
*		m.Glosa1     = Val_Para(Recing.TipRi,"TIPRI ","D") + 'Referencia : '+RecIng.DocRef
		m.Glosa2     = MLINE(RecIng.Observ,2)+MLINE(RecIng.Observ,3)
		m.Glosa3     = MLINE(RecIng.Observ,4)+MLINE(RecIng.Observ,5)+MLINE(RecIng.Observ,6)+MLINE(RecIng.Observ,7)
		m.Glosa4     = MLINE(RecIng.Observ,8)+MLINE(RecIng.Observ,9)+MLINE(RecIng.Observ,10)+MLINE(RecIng.Observ,11)
		m.CodCtc     = RecIng.CodCtc
		m.Codfte     = RecIng.Codfte
		m.CodCad     = RecIng.CodCad
		m.Nummesfte  = Recing.Nummes
		m.Periodofte = vanori
		m.Numdocfte  = Recing.NumRI
		m.Docref     = RecIng.NumRI
		
		SELECT Comp
		IF F_APPD()
			GATHER MEMVAR
			UNLOCK
		ENDIF
		
		xtotdeb = 0
		xtothab = 0
		m.Item  = 0
		
		
		*-Cuentas Patrimoniales            
		
		SELECT AstPat
		SEEK RecIng.Periodo+vMesRI+RecIng.NumRI			&&+vCodCtc
		
		SCAN WHILE  Recing.Periodo=Astpat.Periodo .AND. RecIng.NumMes = AstPat.NumMes AND RecIng.NumRI = AstPat.NumRef
		
			m.Item	   = m.Item + 1
			m.NumMov   = 1
			m.FecEmi   = RecIng.FecRI
			m.Cuenta   = AstPat.CodCta
			m.Tipo     = AstPat.TipCta
			m.TipAux   = Recing.Tipaux
			m.Registro = ALLTRIM(Recing.codprv)
			m.CodCtc   = AstPat.CodCtc
			m.CodDep   = '113000'
			m.CodFte   = RecIng.Codfte
			m.Montos = IIF(m.Tipo='D',AstPat.MtoDeb,AstPat.MtoHaB)
			
			IF m.Tipo='D'
				m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','57','58') 
			 ELSE
				m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','58','57')
			ENDIF
			
			xtotdeb = xtotdeb + IIF(m.Tipo='D',montos,0)
			xtothab = xtothab + IIF(m.Tipo='H',montos,0)
			
			SELECT Diar
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			SELE AstPat
		ENDSCAN   && AstPat
		
		*-Cuentas Presupuestales de ejecuci¢n
		
		SELECT AstPre
		SEEK RecIng.Periodo+vMesRI+RecIng.NumRI			&&vCodCtc
		SCAN WHILE  RecIng.Periodo=AstPre.Periodo AND RecIng.NumRI = AstPre.NumRef .AND. vMesRI = AstPre.NumMes			&& .AND. vCodCtc = AstPre.CodCtc
			m.Item     = m.Item + 1
			m.Orden    = '30'
			m.NumMov   = 1
			m.Item     = m.Item
			m.FecEmi   = RecIng.FecRI
			m.Tipo     = AstPre.Tipo
			m.TipAux   = Recing.Tipaux
			m.Registro = ALLTRIM(Recing.codprv)
			m.CodCtc   = AstPre.CodCtc
			m.CodDep   = '113000'
			m.CodFte   = RecIng.Codfte
			m.Montos   = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
			m.Cuenta   = IIF(m.Tipo='D',Astpre.CtaDeb,Astpre.CtaHab)
			
			xtotdeb = xtotdeb + IIF(m.Tipo='D',montos,0)
			xtotHab = xtotHab + IIF(m.Tipo='H',montos,0)
			
			SELECT Diar
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			SELE AstPre
		ENDSCAN 			&&Astpre
		
		SELE AstOrd
		SEEK RecIng.Periodo+RecIng.NumMes+RecIng.NumRI+'R/I'
		
		IF FOUND()
			SCAN WHILE Periodo=RecIng.Periodo AND NumMes=RecIng.NumMes AND NumRef=RecIng.NumRI AND TipDoc='R/I'
				m.Item  = m.Item + 1
				m.Orden = '31'
				m.NumMov  = 1
				m.FecEmi   = RecIng.FecRI
				m.Tipo     = AstOrd.TipCta
				m.TipAux   = Recing.Tipaux
				m.Registro = ALLTRIM(Recing.codprv)
				m.CodCtc   = ''
				m.CodDep   = '113000'
				m.CodFte   = RecIng.Codfte
				m.Cuenta   = AstOrd.CodCta
				m.Montos   = IIF(m.Tipo='D',AstOrd.MtoDeb,AstOrd.MtoHab)
				
				SELECT Diar
				IF f_appd()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				
				xTotDeb = xTotDeb+IIF(m.Tipo='D',m.Montos,0)
				xTotHab = xTotHab+IIF(m.Tipo='H',m.Montos,0)
				
				SELE AstOrd
			ENDSCAN
		ENDIF
		
		SELE Comp
		SEEK m.Comprob+m.ComAdi
		IF f_Lock(1)
			REPLACE TotDeb WITH xTotDeb
			REPLACE TotHab WITH xTotHab
			UNLOCK
		ENDIF
		
		xtotdeb = 0
		xtothab = 0
		m.Item  = 0
		SELECT RecIng
	ENDSCAN  && RecIng
	
	SET CONSOLE ON
	SET BELL TO 800,5
	_WRAP = .F.
	? CHR(7)+CHR(7)+CHR(7)
	_WRAP = .T.
	SET BELL TO 512,2
	SET BELL TO 512,2
	SET CONSOLE OFF
	
	DO Progreso WITH 3
	DO Standby WITH 'Proceso Finalizado; R/I Integradas.....!'      
ENDIF
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

FUNCTION HAYRI
*-------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo R/I
SEEK vtip+vMesRI+vAnoRI
mRet=.F.

IF FOUND()
	IF yesno('R/I del Mes '+vMesRI+'-'+vAnoRI+' Ya integrado, Reprocesa?')
		DO ESPERA WITH 1,'Eliminando Asientos de R/I procesadas del mes ' + cMes(VAL(vMesRI))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesRI+vAnoRI
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesRI AND STR(YEAR(Diar.fecemi),4)=vAnoRI
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesRI AND STR(YEAR(Comp.Fecha),4)=vAnoRI
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

FUNCTION Val_TipRI
*-----------------
PARAMETERS m.TipRI
PRIVATE mBus,mImp,cAlias
STORE .F. TO mBus,mImp
cAlias = ALIAS()
IF EMPTY(m.TipRI)
	mBus = .T.
 ELSE
	IF SEEK('R/I'+m.TipRI,"AsiAut")
		mImp = .T.
	 ELSE
		mBus = .T.
	ENDIF
ENDIF

IF mBus
	SELE AsiAut
	SET ORDER TO AsiAutRI2
	DEFINE POPUP v__xx FROM 1,30 TO 12,79 PROMPT FIELD Descri
	ON SELECTION POPUP v__xx DEACTIVATE POPUP
	ACTIVATE POPUP v__xx
	RELEASE POPUP v__xx
	
	m.TipRI = Codigo
	
	SET ORDER TO AsiAutRi1
	mImp = .T.
ENDIF

IF mImp
	m.TipRI = AsiAut.Codigo
ENDIF
SELE (cAlias)
RETURN AsiAut.Descri
** --------------------------------------
** INTPES.PRG
** Integraci¢n por lotes de la Polizas de Entrada y de Salida
** AUTOR   : UDEP - 1997.  - Version 5.0
** Fecha   : 15/01/97
** Modif.  : M.R.A.  06/09/2000
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
USE Poliza  IN 12 ORDER TAG Poliza3  ALIAS Poliza
USE ItePol  IN 13 ORDER TAG ItePol3  ALIAS ItePol
USE AstOrd  IN  7 ORDER TAG AstOrd1  ALIAS AstOrd

DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Fin_opcion                && finaliza

RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 13,65 FLOAT DOUBLE TITLE '  Integrando Polizas de Entrada/Salida  ' COLOR SCHEME 5
ACTIVATE WINDOW LIS
SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
VMesPES   = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
VAnoPES   = IIF(FOUND(),  YEAR(vmesact),  YEAR(DATE()))
vTipPol = SPACE(1)
@ 01,01 SAY "         Mes y A¤o : "
@ 01,22 GET VMesPES  PICTURE '99' VALID VMesPES > 0 .AND. VMesPES <= 12
@ 01,27 GET vAnoPES  PICTURE '9,999'
@ 03,09 SAY 'Tipo (E/S) :'
@ 03,22 GET vTipPol  PICTURE '@M E,S'

@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesPES>MesCerrado AND vAnoPES=AnoCerrado) OR vAnoPES>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesPES = PADL(ALLTRIM(STR(VMESPES,2)),2,'0')
	VAnoPES = STR(VanoPES,4)
	SELE Parma
	IF vTipPol='E'
		SEEK 'TIPDOC'+'P/E'
		IF FOUND()
			vTip = ALLTRIM(Parma.CodigoAux)
			_tipo= 'Entrada'
*			m.orden='13'
		ENDIF
	 ELSE
		SEEK 'TIPDOC'+'P/S'
		IF FOUND()
			vTip = ALLTRIM(Parma.CodigoAux)
			_tipo= 'Salida'
*			m.orden='15'
		ENDIF
	ENDIF
	
	IF !HAYPES()
		SELE Comp
		SET ORDER TO Compro7
		SELE Diar
		SET ORDE TO Diario11
		DO Poliza
	ENDIF
	
	SELE Comp
	SET ORDER TO tag Compro7
	SELE Diar
	SET ORDER TO tag Diario11
ENDIF
RETURN



PROCEDURE POLIZA
*----------------
* Carga valores de la Poliza de Entrada/Salida
*---------------------------------------------
DO Espera WITH 1,'Preparando Informacion...'
SELECT Poliza
SET FILTER TO MONTH(FECEMI)=VAL(vMesPES) and Periodo=RIGHT(vAnoPES,2) AND vTipPol=tipPol AND Poliza.Estado#'99'
COUNT TO Registros
Actual = 0
DO Espera WITH 2

SET RELATION TO Periodo+NumPol+TipPol      INTO ItePol
GO TOP
IF EOF()
	DO standby WITH 'No existe documentos Polizas de E/S'
	RETURN .F.
ENDIF

SELE Poliza
GO TOP
SCAN
	Actual = Actual + 1
	do progreso with 1,PADC('Procesando : '+Periodo+'-'+NumPol+' '+TipPol+' '+DTOC(FecEmi),59,' ')
   	DO Progreso WITH 2,'',REGISTROS,ACTUAL
*	@ 5,1 SAY  '        Procesando : '+Periodo+'-'+NumPol+' '+TipPol+' '+DTOC(FecEmi)
*	REPLACE Poliza.Estado WITH '51'
	m.TipDoc  = vTip
	m.Comprob = vTip+PADL(Poliza.NumPol,5,'0')+vMesPES
	m.ComAdi  = ''
	m.Fecha   = Poliza.FecEmi
	m.Glosa1  = IIF(Itepol.Tippol="E",'Poliza de Entrada','Poliza de Salida')+' '+Poliza.Periodo+' '+Poliza.Numpol+' '+IIF(Poliza.tippol='E','Numero de O/C:','Numero Pecosa:')+' '+itepol.TipRef+ItePol.NumRef+IIF(ITEPOL.IGV>0,'  Con IGV.',' ')
	m.Glosa2  = 'Desde '+DTOC(Poliza.FecIni)+'  Hasta '+DTOC(Poliza.FecFin)+'  Fuente : '+Poliza.codfte
	m.Glosa3  = IIF(!EMPTY(Itepol.NummesHc),'H/C: '+ItePol.NumMesHC+' '+Itepol.NumHC,' ')+' '+ALLTRIM(Itepol.Destino)
	m.Glosa4  = ''
	m.CodCad  = Itepol.CodCad
	m.codfte  = Poliza.codfte
	m.Periodofte = Itepol.Perref
	m.Numdocfte  = Itepol.NUMPOL
	m.Docref     = Itepol.Numhc
	m.Tipdcr     = IIF(!EMPTY(Itepol.NummesHc),'03','  ')
	m.Nummesref  = Itepol.Nummeshc
	
	SELECT Comp
	IF f_appd()
		GATHER MEMVAR
	ENDIF
	m.Item  = 0
	xTotDeb = 0
	xTotHab = 0
	
	SELE Itepol
	SEEK Poliza.Periodo+Poliza.Numpol+Poliza.Tippol
	SCAN WHILE Itepol.Periodo=Poliza.Periodo AND Itepol.Numpol=Poliza.Numpol AND Itepol.Tippol=Poliza.Tippol
		xNum = IIF(EMPTY(ItePol.IGV),2,3)
		xNum = IIF(ItePol.TipPol='E',xNum,2)
		xNum = IIF(EMPTY(dCuenta) AND EMPTY(hCuenta),0,xNum)
		m.Orden  = IIF(ItePol.TipPol='E','01','01')
		FOR i = 1 TO xNum
			m.Item      = Item + 1
			m.tipdcr    = Val_para6(Itepol.TipRef,'TIPDOC')
			m.NumMov    = 1
			m.FecEmi    = m.fecha
			m.TipAux    = IIF(!EMPTY(ItePol.codprv),'20','  ')
			m.Registro  = IIF(!EMPTY(ItePol.codprv),ItePol.codprv,ItePol.Coddep)
			m.Coddep    = '111000'
			m.codfte    = Poliza.Codfte
			m.Codart    = ItePol.CodGen+' '+ItePol.CodCla+' '+ItePol.CodDet
			m.Descri    = ALLTRIM(ItePol.Descri)
			m.Docref    = ItePol.NumRef
			m.Tipdcr    = ItePol.TipRef
			
			DO CASE
				CASE i = 1
					m.Tipo   = 'D'
					m.Cuenta = ItePol.DCuenta
					m.Montos = ItePol.ValTot-ItePol.IGV
				CASE i = 2 AND xNum=3
					m.Tipo   = 'D'
					m.Cuenta = '4010400000'
					m.Montos = ItePol.Igv
				CASE (i = 2 AND xNum=2) OR (i = 3 AND xNum=3)
					m.Tipo   = 'H'
					m.Cuenta = ItePol.HCuenta
					IF ItePol.TipPol='S'
						m.Montos = ItePol.ValTot-ItePol.IGV
					 ELSE
						m.Montos = ItePol.ValTot
					ENDIF
			ENDCASE
			SELECT Diar
			IF f_appd()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			xTotDeb = xTotDeb+IIF(m.Tipo='D',m.Montos,0)
			xTotHab = xTotHab+IIF(m.Tipo='H',m.Montos,0)
			SELE Itepol
		ENDFOR
	ENDSCAN
	SELE AstOrd
	SEEK Poliza.Periodo+vMesPES+Poliza.NumPol+IIF(Poliza.Tippol='E',"P/E","")
	IF FOUND()
		IF Poliza.TipPol='E'
			SEEK Poliza.Periodo+vMesPES+Poliza.Numpol+'P/E'
			SCAN WHILE AstOrd.Periodo=Poliza.Periodo AND astOrd.NumMes=vMesPES AND AstOrd.NumRef=Poliza.NumPol AND AstOrd.TipDoc='P/E'
				m.Item     = Item + 1
				m.Orden    = '02'
				m.NumMov   = 1
				m.TipAux   = ''
				m.Registro = ''
				m.Coddep   = '111000'
				m.Codart   = ''
				m.Descri   = ''
				m.Docref   = ''
				m.Tipo     = AstOrd.TipCta
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
	ENDIF
	
	
	Sele Comp
	SEEK ALLTRIM(m.comprob)
	IF RLOCK()
		REPLACE Comp.totdeb     WITH xtotdeb
		REPLACE Comp.tothab     WITH xtothab
		UNLOCK
		xtotdeb = 0
		xtothab = 0
		m.Item  = 0
	ENDIF
	SELE Poliza
ENDSCAN
SET RELATION OFF INTO ItePol

SET CONSOLE ON
SET BELL TO 800,5
_WRAP = .F.
? CHR(7)+CHR(7)+CHR(7)
_WRAP = .T.
SET BELL TO 512,2
SET BELL TO 512,2
SET CONSOLE OFF

DO Progreso WITH 3
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
CLOSE DATA
RETURN

FUNCTION HAYPES
*-------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo P/E; P/S
SEEK vtip+vMesPES+vAnoPES
mRet=.F.

IF FOUND()
	IF YesNo("Las polizas de "+_tipo+ " ya est n integradas. Reprocesa")
		DO ESPERA WITH 1,'Eliminando Asientos de Polizas de '+ _Tipo + ' procesadas del mes ' + cMes(VAL(vMesPES))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesPES+vAnoPES
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesPES AND STR(YEAR(Diar.fecemi),4)=vAnoPES
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesPES AND STR(YEAR(Comp.Fecha),4)=vAnoPES
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

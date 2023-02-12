** --------------------------------------
** IntOSCP.PRG
** Integraci¢n por lotes de O/s para crear asientos de orden con C/P
** Fecha   : 15/01/97 
** Modif.  : M.R.A.  07/09/2000
** ---------------------------------------

USE Parmae    IN  6 ORDER TAG Parmae1                 ALIAS Parma
IF blo_sis(1)
   DO STANDBY WITH "Sistema Bloqueado, Consulte al Area de Sistemas"
   CLOS DATA
   RETURN 
ENDIF   
SEEK "CONTAB"+"CIERRE"
MesCerrado = VAL(SUBSTR(Descri,5,2))
AnoCerrado = VAL(SUBSTR(Descri,1,4))

USE Compro IN 1 ORDER TAG Compro7   ALIAS Comp
USE Diario IN 2 ORDER TAG Diario11  ALIAS Diar
USE ComPag IN 4 ORDER TAG ComPag4   ALIAS ComPag
USE HojCon IN 5 ORDER TAG Hojcon1   ALIAS hoja
USE AstOrd IN 7 ORDER TAG AstOrd2	ALIAS AstOrd
SELE Compag

DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Fin_opcion                && finaliza

RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 13,65 FLOAT DOUBLE TITLE '  Integrando Ordenes de Servicio x C/P' COLOR SCHEME 5
ACTIVATE WINDOW LIS
SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
VMesOSC = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
VAnoOSC = IIF(FOUND(), YEAR(vmesact),   YEAR(DATE()))
@ 01,01 SAY "         Mes y A¤o : " 
@ 01,22 GET VMesOSC  PICTURE '99' VALID VMesOSC > 0 .AND. VMesOSC <= 12
@ 01,27 GET vAnoOSC  PICTURE '9,999'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesOSC>MesCerrado AND vAnoOSC=AnoCerrado) OR vAnoOSC>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesOSC = PADL(ALLTRIM(STR(vMesOSC,2)),2,'0')
	VAnoOSC = STR(VanoOSC,4)
	SELE Parma
	SEEK 'TIPDOC'+'OSC' 
	IF FOUND()
		vTip = ALLTRIM(Parma.CodigoAux)
		_tipo= '  O/S  '
	ENDIF
	IF !HAYOSC()
		SELE Comp
		SET ORDER TO Compro7
		SELE Diar
		SET ORDE TO Diario11
		DO OrdSerCP
	ENDIF    
ENDIF
RETURN

PROCEDURE OrdSerCP
*----------------
DO Espera WITH 1,'Preparando Informacion...'

SELECT ComPag
SET FILTER TO NumMes=vMesOSC and Periodo=RIGHT(vAnoOSC,2) AND estado<>'99' AND !EMPTY(NumMesHC) AND !EMPTY(NumHC)
COUNT TO Registros
Actual = 0
DO Espera WITH 2

SET RELATION TO NumMesHC+NumHC INTO Hoja
GO TOP
IF EOF()
	DO standby WITH 'No existe documentos de O/S'
	RETURN .F.
ENDIF

SCAN
	Actual = Actual + 1
	do progreso with 1,PADC('Procesando : '+Periodo+'-'+NumCP+'-'+DTOC(FecCP)+' '+Hoja.TipDoc+' '+Hoja.NumRef+' '+DTOC(Hoja.FecRef),59,' ')
	DO Progreso WITH 2,'',REGISTROS,ACTUAL
	
	IF Hoja.TipDoc#'O/S'
		LOOP
	ENDIF
*	@ 5,1 SAY  ' Procesando : '+Periodo+'-'+NumCP+'-'+DTOC(FecCP)+' '+Hoja.TipDoc+' '+Hoja.NumRef+' '+DTOC(Hoja.FecRef)
	
	* Es solo un  temporal para Transpasar los datos de la o/s a los archivo de 
	* Compro y Diario.
	
	*wtipdcr=Val_para6('OSC','TIPDOC')
	
	m.TipDoc     = vTip
	m.Comprob    = vTip+PADL(Hoja.NumRef,5,'0')+vMesOSC
	m.ComAdi     = PADL(DAY(FEcCP),2,'0')
	m.Fecha      = FecCP
	m.Glosa1     = 'O/S N§: '+hoja.numref+'.'+PADL(MONTH(hoja.fecref),2,'0')+' Afectada con H/C: '+ComPag.numhc+'.'+ComPag.NumMeshc
	m.Glosa2     = 'Pagada con C/P: '+ComPag.Numcp+'.'+ComPag.nummes+' por el Importe de: '+STR(ComPag.Import,13,2)
	m.Glosa3     = ComPag.Glosa
	m.Glosa4     = ComPag.observ
	m.CodCad     = ComPag.codcad
	m.codfte     = ComPag.codfte
	m.TotDeb	 = 0
	m.TotHab	 = 0
	m.Periodofte = Hoja.PerRef
	m.Numdocfte  = Hoja.NumRef
	m.Docref     = ComPag.Numcp
	m.Tipdcr     = Val_para6('C/P','TIPDOC')
	m.Nummesref  = ComPag.NumCP
	
	SELECT Comp
	IF F_APPD()
		GATHER MEMVAR
		UNLOCK
	ENDIF
	
	xTotDeb = 0
	xTotHab = 0
	m.Item  = 0
	
	SELE AstOrd
	SEEK ComPag.Periodo+ComPag.NumMes+Hoja.NumRef+'OSC'+ComPag.NumCP
	
	SCAN WHILE Periodo=ComPag.Periodo AND NumMes=ComPag.NumMes AND NumRef=Hoja.NumRef AND TipDoc='OSC' AND NumCP=Compag.NumCP
		m.Item      = m.Item + 1
		m.orden     = '01'
		m.NumMov    = 1
		m.FecEmi    = m.fecha
		m.TipAux    = IIF(!EMPTY(ComPag.CodPrv),'20',IIF(!EMPTY(ComPag.CodEmp),'03',IIF(!EMPTY(Compag.CodRet),'08',IIF(!EMPTY(Compag.CodOtr),'09','  '))))
		m.Registro  = IIF(!EMPTY(ComPag.CodPrv),Compag.Codprv,IIF(!EMPTY(ComPag.CodEmp),ComPag.CodEmp,IIF(!EMPTY(Compag.CodRet),ComPag.CodRet,IIF(!EMPTY(Compag.CodOtr),Compag.CodOtr,'  '))))
		m.Coddep    = '111000'
		m.codfte    = ComPag.CodFte
		m.Docref    = ComPag.Numcp
		m.Nummesref = ComPag.NUMMES
		m.Tipo      = AstOrd.TipCta
		m.Cuenta= AstOrd.Codcta
		m.Montos  = IIF(m.Tipo='D',MtoDeb,MtoHab)		

		xtotdeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
		xtothab = xTotHab + IIF(m.Tipo='H',m.Montos,0)

		SELECT Diar
		IF F_APPD()
			GATHER MEMVAR
			UNLOCK
		ENDIF
		
		SELE AstOrd
	ENDSCAN
	SELE ComPag
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


FUNCTION HAYOSC
*--------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo
* O/S CON C/P

SEEK vtip+vMesOSC+vAnoOSC
mRet=.F.

IF FOUND()
	IF yesno('O/S con C/P del Mes '+vmesOSC+'-'+vanoOSC+' Ya integrado, Reprocesa?')
		DO ESPERA WITH 1,'Eliminando Asientos de O/S Pagadas en el Mes de ' + cMes(VAL(vMesOSC))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesOSC+vAnoOSC
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesOSC AND STR(YEAR(Diar.fecemi),4)=vAnoOSC
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesOSC AND STR(YEAR(Comp.Fecha),4)=vAnoOSC
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

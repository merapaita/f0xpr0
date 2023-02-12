** --------------------------------------
** INTCP.PRG
** Integraci¢n por lotes de la COMPROBANTES DE PAGO 
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

USE Compro IN  1 ORDER TAG Compro7  ALIAS Comp
USE Diario IN  2 ORDER TAG Diario11 ALIAS Diar
USE Cajas  IN 10 ORDER TAG Cajas1   ALIAS Caja

DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Fin_opcion                && finaliza

RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------

DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '  Integrando Comprobantes de Pago   ' COLOR SCHEME 5
ACTIVATE WINDOW LIS
  
SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
vdesde  = vmesact
vhasta  = vmesact
vmescp  = IIF(FOUND(),MONTH(vmesact),MONTH(DATE()))
vanocp  = IIF(FOUND(), YEAR(vmesact), YEAR(DATE()))
vcodctc = SPACE(17)
vtodas=1

@ 01,01 SAY '          Mes y A¤o :' GET vmescp     PICTURE '99' VALID vmescp > 0 .AND. vmescp <= 12
@ 01,27 GET vanocp    PICTURE '9,999'
@ 03,01 SAY "         Selecciona : "
@ 02,22 GET vtodas FUNCTION '^ Todas las Cuentas;Una Cuenta Corriente'
@ 05,01 SAY '   Cuenta Corriente : ' GET vcodctc PICTURE "@!"   WHEN vtodas=2 VALID Val_Fun("Caja","CodCTc","CodCtc+' '+Descri",vCodCTc,1,6,1)
@ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

IF !((vMesCP>MesCerrado AND vAnoCP=AnoCerrado) OR vAnoCP>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	vmescp   = PADL(ALLTRIM(STR(vmescp,2)),2,'0')
	vanocp   = STR(vanocp,4)

	SELE Parma
	SEEK 'TIPDOC'+'C/P'
	IF !FOUND()
		=STANDBY('Parametro de Comprobantes de Pago no Inicializado')
		RETURN
	ENDIF
	vTip = ALLTRIM(Parma.CodigoAux)
	
	IF !HAYCP()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		IF yesno('Esta Ud. Seguro de Integrar C/P?')
			DO ComPag
		ENDIF
	ENDIF
ENDIF
RETURN

PROCEDURE COMPAG
*---------------------------------------------
* Carga valores de COMPROBANTES DE PAGO
* ESTADO 20 CHEQUE GIRADO
*        10 RETENCIONES
* Tipaux : 20 cuando es Proveedor
*          30 cuando es Empleado
*          08 cuando es Retenci¢n 
*          09 Otros
*----------------------------------------------

USE ComPag    IN 12 ORDER TAG ComPag3                 ALIAS ComPag
USE AstPat    IN 13 ORDER TAG AstPat3                 ALIAS AstPat
*USE AstPre    IN 14 ORDER TAG AstPre7                 ALIAS AstPre

SELECT ComPag
GO TOP

IF EOF()
	DO STANDBY WITH 'No existe Documentos C/P'
	RETURN .F.
ENDIF

SELECT Compag
SET RELATION TO NumMes+NumCP+CodCtc INTO AstPat ADDITIVE
*SET RELATION TO NumMes+NumCP+CodCtc INTO AstPre ADDITIVE

SELE Compag
SET FILTER TO Compag.nummes=vmescp AND Compag.periodo=RIGHT(vanocp,2) AND IIF(vtodas=2,ALLTRIM(Compag.Codctc)=ALLTRIM(vcodctc),.T.) 

GO TOP
IF EOF()
	DO STANDBY WITH 'No existen Documentos a procesar....!'
 ELSE
	SCAN
		@ 07,01 SAY '        Procesando : '+Compag.Numcp+'-'+DTOC(Compag.Feccp)
		
		m.TipDoc     = VTip
		m.Comprob    = VTip+PADL(ComPag.NumCP,5,'0')+VMesCP
		m.Comadi     = '' 
		m.Fecha      = Compag.FecCP
		m.Glosa1     = SUBSTR(ComPag.Glosa,1,100)
		m.Glosa2     = RIGHT(ComPag.Glosa,100)
		m.Glosa3     = ComPag.Observ
		m.Glosa4     = ALLTRIM(ComPag.Nompre)+' '+Compag.Codctc
		m.CodCtc     = ComPag.CodCtc
		m.CodCaD     = ComPag.CodCaD
		m.CodFTE     = ComPag.CodFTE
		m.tipobr     = ''
		m.CodObra    = ''
		m.nummesfte  = Compag.Nummes
		m.periodofte = Compag.Periodo
		m.numdocfte  = Compag.Numcp
		m.Nummesref  = Compag.NummesHC
		m.Docref     = Compag.Numref
		m.Tipdcr     = Val_para6(Compag.Docref,'TIPDOC')
		
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
		SEEK Compag.NumMes+Compag.NumCP       &&  +vCodCtc
		SCAN WHILE  ComPag.NumCP = AstPat.NumRef .AND. ComPag.NumMes = AstPat.NumMes    && .AND. vCodCtc = AstPat.CodCtc
			m.Item      = m.Item + 1
			m.NumMov    = 1
			m.FecEmi    = Compag.FecCP
			m.Cuenta    = AstPat.CodCta
			m.Tipo      = AstPat.TipCta
			m.TipAux    = IIF(!EMPTY(ComPag.CodPrv),'20',IIF(!EMPTY(ComPag.CodEmp),'03',IIF(!EMPTY(Compag.CodRet),'08',IIF(!EMPTY(Compag.CodOtr),'09','  '))))
			m.Registro  = IIF(!EMPTY(ComPag.CodPrv),Compag.Codprv,IIF(!EMPTY(ComPag.CodEmp),ComPag.CodEmp,IIF(!EMPTY(Compag.CodRet),ComPag.CodRet,IIF(!EMPTY(Compag.CodOtr),Compag.CodOtr,'  '))))
			m.CodCtc    = Compag.CodCtc
			m.CodFte    = Compag.CodFTE
			m.CodDep    = '113000'
			m.tipobr    = ''
			m.CodObra   = ''
			m.Reten     = AstPat.Ret
			m.Montos    = IIF(m.Tipo='D',AstPat.MtoDeb,AstPat.MtoHab)
			
			xtotDeb=xtotDeb+IIF(m.Tipo='D',m.Montos,0)
			xtotHab=xtotHab+IIF(m.Tipo='H',m.Montos,0)
			
			IF m.Tipo='H'
				m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','58','57')
			 ELSE
				m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','57','58')
			ENDIF	
			IF RETEN='S'
				m.orden = '56'
			ENDIF
			
			SELECT Diar
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			SELE AstPat
		ENDSCAN
		
		*-Cuentas Presupuestales de ejecuci¢n
*		SELECT AstPre
*		SEEK vMesCP+ComPag.NumCP      &&  +vCodCtc
*		SCAN WHILE ComPag.NumCP = AstPre.NumRef .AND. ComPag.NumMes = AstPre.NumMes    && .AND. vCodCtc = AstPre.CodCtc
*			m.Item = m.Item + 1
*			m.ORDEN     = '32'
*			m.Comadi    = ''
*			m.NumMov    = 1
*			m.Tipo      = AstPre.Tipo
*			m.CodCtc    = Compag.CodCtc
*			m.Codpart   = Astpre.codpart
*			m.Montos   = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
*			m.Cuenta   = IIF(m.Tipo='D',Astpre.CtaDeb,Astpre.CtaHab)
*			
*			SELECT Diar
*			IF f_appd()
*				GATHER MEMVAR
*				UNLOCK
*			ENDIF

*			xtotdeb = xtotdeb + IIF(m.Tipo='D',m.montos,0)
*			xtotHab = xtotHab + IIF(m.Tipo='H',m.montos,0)
*			SELE AstPre
*		ENDSCAN
		
*		Sele Comp
*		IF RLOCK()
*			REPLACE Comp.totdeb     WITH xtotdeb
*			REPLACE Comp.tothab     WITH xtothab                  
*			UNLOCK
*		ENDIF
		xtotdeb=0
		xtothab=0
		vItem  =0
		SELE Compag
	ENDSCAN
	
	SET CONSOLE ON
	SET BELL TO 800,5
	_WRAP = .F.
	? CHR(7)+CHR(7)+CHR(7)
	_WRAP = .T.
	SET BELL TO 512,2
	SET BELL TO 512,2
	SET CONSOLE OFF
	
	DO Standby WITH 'Proceso Finalizado;C/P '
ENDIF
SET FILTER TO
SET RELATION OFF INTO ComPag
SET RELATION OFF INTO AstPat
*SET RELATION OFF INTO AstPre
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
RELEASE WINDOWS Lis
RETURN

FUNCTION HAYCP
*-------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo C/P
SEEK vtip+vMesCP+vAnoCP
mRet=.F.

IF FOUND()
	IF yesno('C/P del Mes '+vMesCP+'-'+vAnoCP+' Ya integrado, Reprocesa?')
		DO ESPERA WITH 1,'Eliminando Asientos de C/P procesadas del mes ' + cMes(VAL(vMesCP))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesCP+vAnoCP
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesCP AND STR(YEAR(Diar.fecemi),4)=vAnoCP
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesCP AND STR(YEAR(Comp.Fecha),4)=vAnoCP
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

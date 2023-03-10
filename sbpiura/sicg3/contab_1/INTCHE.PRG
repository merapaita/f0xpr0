** --------------------------------------
** IntCHE.PRG
** Integraci?n por lotes de Cheques Girados y Entregados para crear asientos de orden
** Fecha   : 18/08/2000
** Estados : 00 Pendiente de Cancelaci?n
**           50 Cancelado
** ---------------------------------------

CLOS DATA
USE Parmae    IN  4 ORDER TAG Parmae1                 ALIAS Parma
IF blo_sis(1)
   DO STANDBY WITH "Sistema Bloqueado, Consulte al Area de Sistemas"
   CLOS DATA
   RETURN 
ENDIF   
SEEK "CONTAB"+"CIERRE"
MesCerrado = VAL(SUBSTR(Descri,5,2))
AnoCerrado = VAL(SUBSTR(Descri,1,4))

USE Compro	IN 1 ORDER TAG Compro7	ALIAS Comp
USE Diario	IN 2 ORDER TAG Diario11	ALIAS Diar
USE Cheque	IN 3 ORDER TAG Cheque3	ALIAS Cheque
USE Compag  IN 4 ORDER TAG Compag1  ALIAS Compag
USE Parmae  IN 5 ORDER TAG Parmae1  ALIAS Parma
*USE AstOrd  IN 7 ORDER TAG AstOrd1	ALIAS AstOrd


SELE Cheque
SET RELATION TO NumMes+NumCP INTO Compag

DO Inicia                    && Define ventanas, men?s, t?tulos
DO Fin_opcion                && finaliza

RETURN

PROCEDURE Inicia             && Crea ventanas, men?s y t?tulos
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 13,65 FLOAT DOUBLE TITLE '  Integrando Cheques Girados y Entregados ' COLOR SCHEME 5
ACTIVATE WINDOW LIS
SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
VMesChe   = IIF(FOUND(),MONTH(vmesact),MONTH(DATE()))
VAnoChe   = IIF(FOUND(),YEAR(vmesact), YEAR(DATE()))
vTipChq   = 'G'
@ 01,01 SAY "         Mes y A?o : "
@ 01,22 GET VMesChe  PICTURE '99' VALID VMesChe > 0 .AND. VMesChe <= 12
@ 01,27 GET vAnoChe  PICTURE '9,999'
@ 03,05 SAY "Tipo de Cheques (G/E)" 
@ 03,27 GET vTipChq  PICTURE '@M G,E' VALID Pinta()
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesCHE>MesCerrado AND vAnoChe=AnoCerrado) OR vAnoChe>AnoCerrado)
	xmens = "??? Dicho mes ya est? cerrado !!!" 
	RETURN .F.
ENDIF


IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesChe   = PADL(ALLTRIM(STR(VMesChe,2)),2,'0')
	VAnoChe   = PADL(ALLTRIM(STR(VanoChe,4)),4,'0')
	
	IF vTipChq='G'
		cCheque = "CHG"
	 ELSE
		cCheque = "CHE"
	ENDIF
	
	SELE Parma
	SEEK 'TIPDOC'+cCheque
	IF FOUND()
		vTip = ALLTRIM(Parma.CodigoAux)
		_tipo= '  CHE  '
*		wtipdcr=Val_para6('ORC','TIPDOC')
	ENDIF
	IF !HayChe()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		DO Cheque
	ENDIF
ENDIF
RETURN



PROCEDURE Cheque
*----------------
* Carga valores de las O/C
*-------------------------
DO Espera WITH 1,'Preparando Informacion...'
SELECT Cheque
*SET FILTER TO MONTH(FecEnt)=VAL(vMesChe) and Periodo=RIGHT(vAnoChe,2) AND IIF(cCheque='CHE',estado<>'99',.T.) AND BETWEEN(ASC(LEFT(Cheque.Numchq,1)),48,57)
SET FILTER TO VAL(vMesChe)=IIF(cCheque='CHE',MONTH(FecEnt),MONTH(FecChq)) and Periodo=RIGHT(vAnoChe,2) AND IIF(cCheque='CHE',estado<>'99',.T.) AND IIF(cCheque='CHE',!EMPTY(Cheque.FecEnt),.T.) AND BETWEEN(ASC(LEFT(Cheque.Numchq,1)),48,57)

COUNT TO Registros
Actual = 0
DO Espera WITH 2

*SET RELATION TO Periodo+NumMes+NumCP+cCheque INTO AstOrd ADDITIVE

GO TOP
IF EOF()
	DO standby WITH 'No existe documentos de Cheques Entregados'
	RETURN .F.
 ELSE
	vNumCP = SPACE(4)
	SCAN
		Actual = Actual + 1
		do progreso with 1,PADC('Procesando : '+Cheque.Periodo+'-'+Cheque.NumCP+' '+' '+DTOC(Cheque.FecChq),59,' ')
   		DO Progreso WITH 2,'',REGISTROS,ACTUAL

		xTotDeb = 0
		xTotHab = 0
		
		SELE Cheque
		
		IF Cheque.NumCP#vNumCP
			m.ComAdi = '01'
		 ELSE
			m.ComAdi = PADL(ALLTRIM(STR(VAL(m.ComAdi)+1)),2,'0')
		ENDIF
		
		vNumCP = Cheque.NumCP
		
*		@ 5,1 SAY  '        Procesando : '+Cheque.Periodo+'-'+Cheque.NumCP+' '+' '+DTOC(Cheque.FecChq)
		m.Item = 0
		m.Periodo = Cheque.Periodo
		m.fecEmi  = IIF(cCheque='CHE',Cheque.FecEnt,Cheque.FecChq)
		m.TipDoc  = vTip
		m.Comprob = ALLTRIM(vTip)+PADL(Cheque.NumCP,5,'0')+vMesChe
		m.Fecha   = IIF(cCheque='CHE',Cheque.FecEnt,Cheque.FecChq)
		
		IF cCheque='CHG'
			cGlosa1 = 'Cheque Emitido # ' + Cheque.NumChq + ' con C/P N? '+Cheque.NumCP +'-'+Cheque.periodo
			cGlosa2 = 'Girado: '+DTOC(Cheque.FecChq)
			cCta1 = '910308000000000'
			cCta2 = '910408000000000'
		 ELSE
			cGlosa1 = 'Cheque Estregado # ' + Cheque.NumChq + ' con C/P N? '+Cheque.NumCP +'-'+Cheque.periodo 
			cGlosa2 = 'con Fecha '+ DTOC(Cheque.FecChq)
			cCta1 = '910408000000000'
			cCta2 = '910308000000000'
		ENDIF
		
		m.Glosa1  = cGlosa1
		m.Glosa2  = cGlosa2
		m.Glosa3  = ''
		m.Glosa4  = ''
		m.CodCad  = Cheque.codcad
		m.TotDeb  = ROUND(Cheque.valChq,2)
		m.TotHab  = ROUND(Cheque.valChq,2)
		m.Periodofte = Cheque.Periodo
		m.Numdocfte  = Cheque.NumChq
		m.Docref  = Cheque.NumCP
		m.Tipdcr  = IIF(!EMPTY(Cheque.NumCP),'26','  ')
		m.Nummesref = Cheque.NumMes
		
		SELECT Comp
		IF f_appd()
			GATHER MEMVAR
		ENDIF
		
		FOR i = 1 TO 2
			m.Item    = m.Item + 1
			m.Orden   = '01'
			m.NumMov  = 1
*			m.FecEmi  = m.fecemi		&& Cheque.FecEnt
			m.TipAux  = IIF(!EMPTY(ComPag.CodPrv),'20',IIF(!EMPTY(ComPag.CodEmp),'03',IIF(!EMPTY(Compag.CodRet),'08',IIF(!EMPTY(Compag.CodOtr),'09','  '))))
			m.Registro= IIF(!EMPTY(ComPag.CodPrv),Compag.Codprv,IIF(!EMPTY(ComPag.CodEmp),ComPag.CodEmp,IIF(!EMPTY(Compag.CodRet),ComPag.CodRet,IIF(!EMPTY(Compag.CodOtr),Compag.CodOtr,'  '))))
			m.Coddep  = '111000'
			m.Docref  = Cheque.NumCP
			m.Tipdcr  = IIF(!EMPTY(Cheque.NumCP),'26','  ')
			m.Nummesref = Cheque.Periodo
			m.Tipo    = IIF(i=1,'D','H')
			m.Cuenta  = IIF(i=1,cCta1,cCta2)
			m.Montos  = Cheque.ValChq
			
			SELECT Diar
			IF f_appd()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			
			xTotDeb = xTotDeb+IIF(m.Tipo='D',m.Montos,0)
			xTotHeb = xTotHab+IIF(m.Tipo='H',m.Montos,0)
			
		ENDFOR
		
		SELE Comp
		
		SEEK m.Comprob+m.ComAdi
		
		IF f_Lock(1)
			REPLACE TotDeb WITH xTotDeb
			REPLACE TotHab WITH xTotHab
			UNLOCK
		ENDIF
		SELE Cheque
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
* DO Logos WITH Rotulo1,Rotulo2
CLOSE DATA

RETURN


FUNCTION HAYChe
*--------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo O/C
SEEK vtip+vMesChe+vAnoChe
mRet=.F.

IF FOUND()
	IF yesno("Ya se han procesado Cheques " +IIF(vTipChq='G','Girados.','Entregados.')+" ?Desea anularlas y reprocesar de nuevo?")
		DO ESPERA WITH 1,'Eliminando Asientos de Cheques procesadas del mes ' + cMes(VAL(vMesChe))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesChe+vAnoChe
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesChe AND STR(YEAR(Diar.fecemi),4)=vAnoChe
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesChe AND STR(YEAR(Comp.Fecha),4)=vAnoChe
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


FUNCTION Pinta
*-------------
IF vTipChq='G'
	@ 03,29 SAY 'Girado'
 ELSE
	@ 03,29 SAY 'Entregado'
ENDIF
RETURN
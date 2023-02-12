** --------------------------------------
** IntOC.PRG
** Integraci¢n por lotes de O/C para crear asientos de orden
** Fecha   : 18/08/2000
** Estados : 00 Pendiente de Cancelaci¢n
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
USE OrdCom	IN 3 ORDER TAG OrdCom1	ALIAS Orden
USE AstOrd  IN 7 ORDER TAG AstOrd1	ALIAS AstOrd


SELE Orden

*- Mes y a¤o actual
SELECT parma

DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Fin_opcion                && finaliza

SHOW POPUP Menu
RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 13,65 FLOAT DOUBLE TITLE '  Integrando Ordenes de Compra ' COLOR SCHEME 5
ACTIVATE WINDOW LIS
SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
VMesOC   = IIF(FOUND(),MONTH(vmesact),MONTH(DATE()))
VAnoOC   = IIF(FOUND(),YEAR(vmesact), YEAR(DATE()))
@ 01,01 SAY "         Mes y A¤o : "
@ 01,22 GET VMesOC  PICTURE '99' VALID VMesOC > 0 .AND. VMesOC <= 12
@ 01,27 GET vAnoOC  PICTURE '9,999'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesOC>MesCerrado AND vAnoOC=AnoCerrado) OR vAnoOC>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesOC   = PADL(ALLTRIM(STR(VMESOC,2)),2,'0')
	VAnoOC   = STR(VanoOC,4)
	SELE Parma
	SEEK 'TIPDOC'+'ORC'      
	IF FOUND()
		vTip = ALLTRIM(Parma.CodigoAux)
	ENDIF
	IF !HAYOC()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		DO Orden
	ENDIF
ENDIF
RETURN

PROCEDURE orden
*--------------
* Carga valores de las O/C
*-------------------------
DO Espera WITH 1,'Preparando Informacion...'
SELECT Orden
SET FILTER TO MONTH(FecOC)=VAL(vMesOC) and periodo=RIGHT(vAnoOC,2) AND estado<>'99'
COUNT TO Registros
Actual = 0
DO Espera WITH 2

SET RELATION TO Periodo+NumMes+NumOC+'O/C' INTO AstOrd ADDITIVE

GO TOP
IF EOF()
	DO standby WITH 'No existe documentos de O/C'
	RETURN .F.
 ELSE
	SCAN
		Actual = Actual + 1
		do progreso with 1,PADC('Procesando : '+Orden.Periodo+'-'+Orden.Numoc+' '+Orden.Codfte+' '+DTOC(Orden.FecOC),59,' ')
   		DO Progreso WITH 2,'',REGISTROS,ACTUAL
   		
		xTotDeb = 0
		xTotHab = 0
		
		SELE Orden
*		@ 5,1 SAY  '        Procesando : '+Orden.Periodo+'-'+Orden.Numoc+' '+Orden.Codfte+' '+DTOC(Orden.FecOC)
		m.Item = 0
		m.Periodo = Orden.Periodo
		m.Codfte  = Orden.CodFte
		m.fecEmi  = Orden.Fecoc
		m.TipDoc  = vTip
		m.Comprob = ALLTRIM(vTip)+PADL(Orden.Numoc,5,'0')+vMesOC
		m.ComAdi  = ''
		m.Fecha   = Orden.FecOC
		m.Glosa1  = 'O/C Emitida N§ '+Orden.Periodo+'.'+Orden.Numoc+' H/C N§: '+Orden.numhc+'.'+Orden.perhc
		m.Glosa2  = 'Desde fecha '+DTOC(m.Fecha) +'  Fuente : '+m.codfte
		m.Glosa3  = Orden.docref+' Destino: '+ALLTRIM(Orden.Destino)
		m.Glosa4  = Orden.observa
		m.CodCad  = Orden.codcad
		m.TotDeb  = ROUND(Orden.valtot,2)
		m.TotHab  = ROUND(Orden.valtot,2)
		m.Periodofte = Orden.Periodo
		m.Numdocfte  = Orden.NUMoc
		m.Docref  = Orden.Numhc
		m.Tipdcr    = IIF(!EMPTY(Orden.NumHc),'03','  ')
		m.Nummesref = Orden.Perhc
		
		SELECT Comp
		IF f_appd()
			GATHER MEMVAR
			UNLOCK
		ENDIF
		SELE AstOrd
		SEEK Orden.Periodo+Orden.NumMes+Orden.NumOC+'O/C'
		
		IF FOUND()
			SCAN WHILE Periodo=Orden.Periodo AND NumMes=Orden.NumMes AND NumRef=Orden.NumOC AND TipDoc='O/C'
				m.Item  = m.Item + 1
				m.Orden = '01'
				m.NumMov  = 1
				m.FecEmi  = Orden.FecOC
				m.TipAux  = '20'
				m.Registro = Orden.codprv
				m.Coddep  = '111000'
				m.codfte  = Orden.CodFte
				m.Docref  = Orden.Numhc
				m.Tipdcr  = IIF(!EMPTY(Orden.NumHc),'03','  ')
				m.Nummesref = Orden.Perhc
				m.Tipo    = AstOrd.TipCta
				m.Cuenta  = AstOrd.CodCta
				m.Montos  = IIF(m.Tipo='D',AstOrd.MtoDeb,AstOrd.MtoHab)
				
				SELECT Diar
				IF f_appd()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				
				xTotDeb = xTotDeb+IIF(m.Tipo='D',AstOrd.MtoDeb,0)
				xTotHab = xTotHab+IIF(m.Tipo='H',AstOrd.MtoHab,0)
				
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
		SELE Orden
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

FUNCTION HAYOC
*-------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo O/C
SEEK vtip+vMesOC+vAnoOC
mRet=.F.

IF FOUND()
	IF yesno("Ya se han procesado O/C. ¨Desea anularlas y reprocesar de nuevo?")
		DO ESPERA WITH 1,'Eliminando Asientos de O/C procesadas del mes ' + cMes(VAL(vMesOC))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesOC+vAnoOC
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesOC AND STR(YEAR(Diar.fecemi),4)=vAnoOC
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesOC AND STR(YEAR(Comp.Fecha),4)=vAnoOC
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


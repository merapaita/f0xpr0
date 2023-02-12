** --------------------------------------
** INTHM.PRG
** Integraci¢n por lotes de la Hojas de Modificaci¢n 
** AUTOR   : UDEP - 1995. FMC - Version 5.0
** Fecha   : 02/11/95 
** Modif.  : M.R.A.  07/09/2000
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
DEFINE WINDOW LIS FROM 5,15 TO 13,65 FLOAT DOUBLE TITLE '  Integrando Hojas de Modificaci¢n  ' COLOR SCHEME 5
ACTIVATE WINDOW LIS

SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
VMesHM   = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
VAnoHM   = IIF(FOUND(), YEAR(vmesact), YEAR(DATE()))
@ 01,01 SAY "         Mes y A¤o : " 
@ 01,22 GET VMesHM  PICTURE '99' VALID VMesHM > 0 .AND. VMesHM <= 12
@ 01,27 GET vAnoHM  PICTURE '9,999'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesHM>MesCerrado AND vAnoHM=AnoCerrado) OR vAnoHM>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesHM   = PADL(ALLTRIM(STR(VMESHM,2)),2,'0')
	VAnoHM   = STR(VanoHM,4)
	
	SELE Parma
	SEEK 'TIPDOC'+'H/M'
	IF FOUND()
		vTip = ALLTRIM(Parma.CodigoAux)
	ENDIF
	
	IF !HAYHM()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		DO HOJMOD
	ENDIF
ENDIF
RETURN

PROCEDURE HojMod
*---------------
USE HojMod IN 12 ORDER TAG HojMOD1 ALIAS HojMod
USE IteHC  IN 13 ORDER TAG IteHC3  ALIAS IteHC
USE IteCla IN 14 ORDER TAG IteCla1 ALIAS IteCla
USE AstPre IN 15 ORDER TAG AstPre5 ALIAS AstPre

DO Espera WITH 1,'Preparando Informacion...'

SELECT HojMod
SET FILTER TO HojMod.NumMes=vMesHM AND HojMod.Periodo=RIGHT(vanoHM,2)
COUNT TO Registros
Actual = 0
DO Espera WITH 2

SET RELATION TO NumMesHC+NumHC       INTO IteHC

SELECT ITEHC
SET RELATION TO codpart+CodCla                     INTO IteCla ADDITIVE
SET RELATION TO HojMod.NumMesHC+HojMod.NumHC+'H/M' INTO AstPre ADDITIVE

SELE HojMod
GO TOP

IF EOF()
	DO STANDBY WITH ' No existe documentos H/M '
	RETURN .F.
ENDIF

SCAN
	Actual = Actual + 1
	do progreso with 1,PADC('Procesando : '+HojMod.NumHM+'-'+vMesHM+'-'+vAnoHM+'-'+DTOC(HojMod.FecHM),59,' ')
   	DO Progreso WITH 2,'',REGISTROS,ACTUAL
*	@ 07,1 SAY   " Procesando : " + HojMod.NumHM+'-'+vMesHM+'-'+vAnoHM+'-'+DTOC(HojMod.FecHM)

	* En Caso de QUE EL MES NO CORRESPONDA a procesar
	
	m.fecha=HojMod.FecHm
	
	IF PADL(ALLTRIM(STR(MONTH(HojMod.FecHm),2)),2,'0')<>VMesHM
		* se asigna el primer dia del mes
		m.fecha=CTOD('01/'+VMesHM+'/'+VAnoHM)
	ENDIF
	
	m.TipDoc     = VTIP
	m.Comprob    = VTIP+PADL(HojMod.NumHm,5,'0')+VMesHM
	m.Fecha      = HojMod.FecHm
	m.Glosa1     = ALLTRIM(HojMod.DesRef)+' '+HojMod.Destino
	m.Glosa2     = MLINE(HojMod.OBSERVA,1)+MLINE(HojMod.OBSERVA,2)+MLINE(HojMod.OBSERVA,3)+MLINE(HojMod.OBSERVA,4)
	m.Glosa3     = MLINE(HojMod.OBSERVA,5)+MLINE(HojMod.OBSERVA,6)+MLINE(HojMod.OBSERVA,7)+MLINE(HojMod.OBSERVA,8)
	m.Glosa4     = ''
	m.CodCtc     = HojMod.CodCtC
	m.CodCad     = HojMod.CodCad
	m.codfte     = HojMod.codfte
	m.Periodofte = HojMod.Periodo
	m.Nummesfte  = HojMod.Nummes
	m.Numdocfte  = HojMod.NumHm
	m.Periodoref = HojMod.Perref
	m.DocRef     = HojMod.NumHC
	m.NummesRef  = SUBSTR(DTOC(HojMod.Fechc),4,2)
	m.TipDcr     = vtip
	
	SELECT Comp
	IF F_APPD()
		GATHER MEMVAR
		UNLOCK
	ENDIF
	
	m.Item = 0
	xtotdeb= 0
	xtothab= 0
	
	*-Se generan los asientos Presupuestales por cada una de las partidas
	*-ya sea generica o anal¡tica
	* REVERSION DE HOJA DE MODIFICACION
	
	SELECT AstPre
	SEEK HojMod.NumMes+HojMod.NumHM+'H/M'
	SCAN WHILE AstPre.NumMes = HojMod.NumMes AND AstPre.NumRef = HojMod.NumHM AND TIPDOC='H/M'
		m.Item = m.Item + 1
		m.orden      = '01'
		m.NumMov     = 1
		m.FecEmi     = m.Fecha
		m.TipAux     = IIF(!EMPTY(HojMod.CodPrv),'20',IIF(!EMPTY(HojMod.CodEmp),'03','09'))
		m.Registro   = IIF(!EMPTY(HojMod.CodPrv),HojMod.CodPrv,IIF(!EMPTY(HojMod.CodEmp),HojMod.CodEmp,HojMod.CodOtr))
		m.CodCtc     = HojMod.CodCtC
		m.Coddep     = '112000'
		m.Codpart    = IteHC.CodPart
		m.Periodoref = HojMod.Perref
		m.Tipo       = AstPre.Tipo
		m.CodCtc     = AstPre.CodCtC
		m.Cuenta     = IIF(m.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
		m.Montos     = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab) 
		
		xtotdeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
		xtothab = xTotHab + IIF(m.Tipo='H',m.Montos,0)
		
		SELECT Diar
		IF F_APPD()
			GATHER MEMVAR
			UNLOCK
		ENDIF
		SELE AstPre
	ENDSCAN
	
	*- Genera asientos patrimoniales con las partidas del clasificador
	*- por objeto del gasto.
	SELE IteHC
	SEEK HojMod.NumMesHC+HojMod.NumHC
	IF FOUND()
		SCAN WHILE IteHC.NumHC = HojMod.NumHC .AND. IteHC.NumMes = HojMod.NumMesHC AND IteHC.Estado='92'
			mGenera=.T.
			IF mGenera
				SELECT IteHC
				IF !EMPTY(IteCla.CuentaD) AND !EMPTY(IteCla.CuentaH)
					vCon = 0
					FOR vCon=1 TO 2
		     			m.orden      = '02'
		     			m.NumMov     = 1
						m.Item       = m.Item+1
	     				m.DocRef     = HojMod.NumRef
	     				m.TipDcr     = vtip
		     			m.CodCtc     = HojMod.CodCtC
		     			m.Coddep     = '112000'
	    	 			m.Codpart    = Itehc.Codpart              
	     				m.Periodoref = HojMod.perref
	     				m.TipDoc     = itehc.TipDoc
	     				
						IF IteHC.TipOpe='-'
						 	DO CASE
								CASE vCon = 1
									m.Tipo   = 'H'
									m.Cuenta = PADR(ALLTRIM(IteCla.CuentaD),15,'0')
									m.Montos = IteHC.ValPart
								CASE vCon = 2
									m.Tipo   = 'D'
									m.Cuenta = PADR(ALLTRIM(IteCla.CuentaH),15,'0')
									m.Montos = IteHC.ValPart
							ENDCASE
						 ELSE
							DO CASE
								CASE vCon = 1
									m.Tipo   = 'D'
									m.Cuenta = PADR(ALLTRIM(IteCla.CuentaD),15,'0')
									m.Montos = IteHC.ValPart
								CASE vCon = 2 AND vCon=2
									m.Tipo   = 'H'
									m.Cuenta = PADR(ALLTRIM(IteCla.CuentaH),15,'0')
									m.Montos = IteHC.ValPart
							ENDCASE
						ENDIF
						
						xtotdeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
						xtothab = xTotHab + IIF(m.Tipo='H',m.Montos,0)
						SELE Diar
						IF F_APPD()
							GATHER MEMVAR
							UNLOCK
						ENDIF
						
					ENDFOR
				ENDIF
				SELE IteHC
			ENDIF
		ENDSCAN
	ENDIF

	*- Genera asientos rebaja y/o modificacion presupuestal del 
	*- proceso de devengue.
	SELE IteHC
	SEEK HojMod.NumMesHC+HojMod.NumHC
	IF FOUND()
		SCAN WHILE IteHC.NumHC = HojMod.NumHC .AND. IteHC.NumMes = HojMod.NumMesHC AND IteHC.Estado='92'
			SELECT IteHC
			vCon = 0
			FOR vCon=1 TO 2
		    	m.orden      = '06'
		    	m.NumMov     = 1
				m.Item       = m.Item+1
	     		m.DocRef     = HojMod.NumRef
	     		m.TipDcr     = vtip
		   		m.CodCtc     = HojMod.CodCtC
		   		m.CodFte     = IteHC.CodFte
		   		m.CodCad     = IteHC.CodCad
		   		m.Coddep     = '112000'
	     		m.Codpart    = Itehc.Codpart
	    		m.Periodoref = HojMod.perref
	    		m.TipDoc     = itehc.TipDoc
	    			
				IF IteHC.TipOpe='-'
				 	DO CASE
						CASE vCon = 1
							m.Tipo   = 'D'
							m.Cuenta = PADR('8601'+IIF(m.CodFte="09","0201","0402")+RIGH(m.CodCad,2),15,'0')
							m.Montos = IteHC.ValPart
						CASE vCon = 2
							m.Tipo   = 'H'
							m.Cuenta = PADR('8401'+IIF(m.CodFte="09","0201","0402")+RIGH(m.CodCad,2),15,'0')
							m.Montos = IteHC.ValPart
					ENDCASE
*							m.Cuenta = PADR(ALLTRIM(IteCla.CuentaD),15,'0')
*							m.Cuenta = PADR(ALLTRIM(IteCla.CuentaH),15,'0')
				 ELSE
					DO CASE
						CASE vCon = 1
							m.Tipo   = 'D'
							m.Cuenta = PADR('8401'+IIF(m.CodFte="09","0201","0402")+RIGH(m.CodCad,2),15,'0')
							m.Montos = IteHC.ValPart
						CASE vCon = 2
							m.Tipo   = 'H'
							m.Cuenta = PADR('8601'+IIF(m.CodFte="09","0201","0402")+RIGH(m.CodCad,2),15,'0')
							m.Montos = IteHC.ValPart
					ENDCASE
				ENDIF
				
				xtotdeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
				xtothab = xTotHab + IIF(m.Tipo='H',m.Montos,0)
				SELE Diar
				IF F_APPD()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				
			ENDFOR
			SELE IteHC
		ENDSCAN
	ENDIF
	
	Sele Comp
	IF F_Lock(1)
		REPLACE Comp.totdeb WITH xtotdeb
		REPLACE Comp.tothab WITH xtothab
		xtotdeb=0
		xtothab=0
		vitem=0
		UNLOCK
	ENDIF
	SELE HojMod
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

DO Standby WITH 'Proceso Finalizado; H/M Integradas.....!'      
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

FUNCTION HAYHM
*-------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo H/M
SEEK vtip+vMesHM+vAnoHM
mRet=.F.

IF FOUND()
	IF yesno('H/M del Mes '+vmesHM+'-'+vanoHM+' Ya integrado, Reprocesa?')
		DO ESPERA WITH 1,'Eliminando Asientos de H/M procesadas del mes ' + cMes(VAL(vMesHM))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesHM+vAnoHM
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesHM AND STR(YEAR(Diar.fecemi),4)=vAnoHM
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesHM AND STR(YEAR(Comp.Fecha),4)=vAnoHM
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

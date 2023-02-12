** --------------------------------------
** INTDEV.PRG
** Integraci¢n por lotes de la Hojas de Control DEVENGADAS
** AUTOR   : MERA
** Fecha   : 02/11/95 
** Modif.  : M.R.A.  25/08/2000
** Estados : 00 Pendiente de Cancelaci¢n
**           50 Cancelado
** ---------------------------------------

SET MEMOWIDTH TO 50

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

SHOW POPUP Menu
RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 13,65 FLOAT DOUBLE TITLE '  Integrando Hojas de Control  ' COLOR SCHEME 5
ACTIVATE WINDOW LIS

SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
VMesHC   = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
VAnoHC   = IIF(FOUND(),  YEAR(vmesact),  YEAR(DATE()))
@ 01,01 SAY "         Mes y A¤o : " 
@ 01,22 GET VMesHC  PICTURE '99' VALID VMesHC > 0 .AND. VMesHC <= 12
@ 01,27 GET vAnoHC  PICTURE '9,999'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

DEACTIVATE WINDOWS Lis
RELEASE WINDOWS Lis

IF !((vMesHC>MesCerrado AND vAnoHC=AnoCerrado) OR vAnoHC>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF


IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesHC   = PADL(ALLTRIM(STR(VMESHC,2)),2,'0')
	VAnoHC   = STR(VanoHC,4)
	
	SELE Parma
	SEEK 'TIPDOC'+'DEV'
	IF !FOUND()
		=STANDBY('Parametro de Devengues no Inicializado')
		RETURN
	ENDIF
	vTip = ALLTRIM(Parma.CodigoAux)
	
	IF !HAYHC()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		DO HOJCON
	ENDIF
ENDIF
RETURN


PROCEDURE HOJCON
*---------------
*- Carga valores de Hoja de Control

USE HojCon IN 3 ORDER TAG HojCon1 ALIAS HojCon
USE IteHC  IN 5 ORDER TAG IteHC1  ALIAS IteHC
USE IteCla IN 6 ORDER TAG IteCla1 ALIAS IteCla
USE AstPre IN 7 ORDER TAG AstPre5 ALIAS AstPre
*USE OrdSer IN 8 ORDER TAG OrdSer1 ALIAS OrdSer		&& Este Solo esta solo por que hojas de afectacion no jalan igv
													&& Corregir este Problema desde el Doc. Fuente (H/C)
*USE Auxil     IN 18 ORDER TAG Auxil1   ALIAS Auxi
*USE ProMae    IN 11 ORDER TAG ProMae1  ALIAS ProMae

SELECT HojCon
SET FILTER TO Hojcon.NumMes=vMesHC AND Hojcon.Periodo=RIGHT(vAnoHC,2) AND HojCon.Estado # "99"
COUNT TO Registros
Actual   = 0
SET RELATION TO NumMes+NumHC  INTO IteHC
SELECT ITEHC
SET RELATION TO codpart+CodCla     INTO IteCla ADDITIVE
SELE HojCon
GO TOP

IF EOF()
	DO STANDBY WITH ' No existe documentos H/C '
	RETURN .F.
ENDIF
SCAN
	ACTUAL=ACTUAL+1
	do progreso with 1,PADC('Integrando H/C: '+Hojcon.nummes+'.'+hojcon.numhc+' '+allt(hojcon.desref),59,' ')
    DO Progreso WITH 2,'',REGISTROS,ACTUAL
    
	* En Caso de QUE EL MES NO CORRESPONDA a procesar
	vfecha=HojCon.FecHC
	IF PADL(ALLTRIM(STR(MONTH(HojCon.FecHC),2)),2,'0')<>VMesHC
	* se asigna el primer dia del mes
		vfecha=CTOD('01/'+VMesHC+'/'+VAnoHC)
	ENDIF
	
	m.TipDoc     = VTIP
	m.Comprob    = VTIP+PADL(HojCon.NumHC,5,'0')+VMesHC
	m.ComAdi     = ''
	m.Fecha      = vFecha
	m.Glosa1     = 'Devengue ' + ALLTRIM(HojCon.DesRef)+' '+Hojcon.Destino
	m.Glosa2     = HojCon.Observa
	m.Glosa3     = MLINE(HojCon.Observ,1)+MLINE(HojCon.Observ,2)+MLINE(HojCon.Observ,3)+MLINE(HojCon.Observ,4)
	m.Glosa4     = MLINE(HojCon.Observ,5)+MLINE(HojCon.Observ,6)+MLINE(HojCon.Observ,7)+MLINE(HojCon.Observ,8)
	m.CodCtc     = HojCon.CodCtC
	m.CodCad     = HojCon.CodCad
	m.codfte     = Hojcon.codfte
	m.tipObr     = HojCon.tipObr
	m.CodObra    = HojCon.CodObra
	m.Periodofte = HojCon.Periodo
	m.Nummesfte  = HojCon.Nummes
	m.Numdocfte  = hojCon.NumHC
	m.Periodoref = Hojcon.Perref
	m.DocRef     = HojCon.NumRef
	m.NummesRef  = PADL(MONT(HojCon.FecRef),2,'0')
	m.TipDcr     = Val_para6(HojCon.TipDoc,'TIPDOC')
	
    SELECT Comp
   	IF F_APPD()
		GATHER MEMVAR
		UNLOCK
	ENDIF
	
	*-Se generan los Asientos Presupuestales por cada una de las partidas
	*-ya sea generica o anal¡tica
	
	m.Item = 0
	xtotdeb= 0
	xtothab= 0
	
	SELE AstPre
	SEEK HojCon.NumMes + HojCon.NumHC + 'H/C'
	IF FOUND()
		SCAN WHILE IteHC.NumHC = AstPre.NumRef .AND. IteHC.NumMes = AstPre.NumMes AND AstPre.TipDoc='H/C'
			m.Item = m.Item + 1
			m.Orden 	 = '01'
			m.NumMov     = 1
			m.FecEmi     = m.Fecha
			m.TipAux     = IIF(!EMPTY(HojCon.CodPrv),'20',IIF(!EMPTY(HojCon.CodEmp),'03','09'))
			m.Registro   = IIF(!EMPTY(HojCon.CodPrv),HojCon.CodPrv,IIF(!EMPTY(HojCon.CodEmp),HojCon.CodEmp,HojCon.CodOtr))
			m.DocRef     = HojCon.NumRef
			m.TipDcr     = m.tipdcr
			m.CodCtc     = HojCon.CodCtC
			m.Coddep     = '112000'
			m.Codpart    = IteHC.Codpart
			m.Periodoref = Hojcon.Perref
			m.Tipo		 = AstPre.Tipo
			
			m.Cuenta     = IIF(m.Tipo='D','8401'+SUBSTR(AstPre.CtaDeb,5,11),'8601'+SUBSTR(AstPre.CtaHab,5,11))
*			m.Cuenta     = IIF(AstPre.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
			m.Montos     = IIF(AstPre.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
			
			xtotdeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
			xtothab = xTotHab + IIF(m.Tipo='H',m.Montos,0)
			
			SELECT Diar
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			SELE AstPre
		ENDSCAN
	ENDIF
	
	Sele Comp
	IF F_Lock(1)
		REPLACE Comp.totdeb     WITH xtotdeb
		REPLACE Comp.tothab     WITH xtothab
		xtotdeb=0
		xtothab=0
		vitem=0
		UNLOCK
	ENDIF
	SELE Hojcon
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

* Finaliza de Recorrer el Archivo H/C
* y termina el Proceso
DO Standby WITH 'Proceso Finalizado; H/C Integradas.....!'
SET RELATION OFF INTO IteHC
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


FUNCTION repetir
*---------------
PARAMETER vnumref,vcodpart
valias = ALIAS()
vorder = ORDER()
USE Itefp  IN 8 ORDER TAG Itefp2  ALIAS itefon
SELECT 	itefon
vTotigv= 0
SCAN FOR numfp=vnumref and codpart=vcodpart
 	vTotigv = vTotigv + Totigv
ENDSCAN
SELECT (valias)
SET ORDER TO (vorder)
RETURN vTotigv

        
FUNCTION HAYHC
*-------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo H/C
SEEK vtip+vMesHC+vAnoHC
mRet=.F.

IF FOUND()
	IF yesno('H/C del Mes '+vmeshc+'-'+vanohc+' Ya integrado, Reprocesa?')
		DO ESPERA WITH 1,'Eliminando Asientos de H/C procesadas del mes ' + cMes(VAL(vMesHC))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesHC+vAnoHC
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesHC AND STR(YEAR(Diar.fecemi),4)=vAnoHC
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesHC AND STR(YEAR(Comp.Fecha),4)=vAnoHC
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

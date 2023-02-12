** --------------------------------------
** IntPRA.PRG
** Integraci¢n por lotes de Partes de rebaja de O/C, O/S
** Fecha   : 22/10/2002
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
USE PteAnu	IN 3 ORDER TAG PteAnu3	ALIAS PteAnu
USE AstOrd  IN 7 ORDER TAG AstOrd1	ALIAS AstOrd


SELE PteAnu

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
VMesPR   = IIF(FOUND(),MONTH(vmesact),MONTH(DATE()))
VAnoPR   = IIF(FOUND(),YEAR(vmesact), YEAR(DATE()))
@ 01,01 SAY "         Mes y A¤o : "
@ 01,22 GET VMesPR  PICTURE '99' VALID VMesPR > 0 .AND. VMesPR <= 12
@ 01,27 GET vAnoPR  PICTURE '9,999'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesPR>MesCerrado AND vAnoPR=AnoCerrado) OR vAnoPR>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesPR   = PADL(ALLTRIM(STR(VMESPR,2)),2,'0')
	VAnoPR   = STR(VanoPR,4)
	SELE Parma
	SEEK 'TIPDOC'+'PRA'
	IF FOUND()
		vTip = ALLTRIM(Parma.CodigoAux)
	ENDIF
	IF !HAYPRA()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		DO Rebaja
	ENDIF
ENDIF
RETURN

PROCEDURE Rebaja
*----------------------------------------------
* Carga valores del Parte de Rebaja y anulaci¢n
*----------------------------------------------
DO Espera WITH 1,'Preparando Informacion...'
SELECT PteAnu
vIdx = SYS(3)+".Idx"
INDEX ON Periodo+NumPA TO (vIdx) FOR MONTH(FecPA)=VAL(vMesPR) and periodo=RIGHT(vAnoPR,2) AND estado<>'99'
COUNT TO Registros
Actual = 0
DO Espera WITH 2

SET RELATION TO Periodo+MesPA+NumPA+IIF(Tipo='A' AND TipDoc='O/C','AOC',IIF(Tipo='A' AND TipDoc='O/S',"AOS",IIF(Tipo='R' AND TipDoc='O/C',"ROC","ROS"))) INTO AstOrd ADDITIVE

GO TOP
IF EOF()
	DO standby WITH 'No existe Partes de Rebaja y Anulacion de O/C y O/S'
	RETURN .F.
 ELSE
	SCAN
		Actual = Actual + 1
		do progreso with 1,PADC('Procesando : '+PteAnu.Periodo+'-'+PteAnu.NumPA+' '+PteAnu.Codfte+' '+DTOC(PteAnu.FecPA),59,' ')
		DO Progreso WITH 2,'',REGISTROS,ACTUAL
		
		xTotDeb = 0
		xTotHab = 0
		
		DO CASE
			CASE Tipo='A' AND TipDoc='O/C'
				vTip1 = "AOC"
				vDoc  = 'Anulaci¢n de OC'
				vAdic = '01'
				vOrd  = '01'
			CASE Tipo='A' AND TipDoc='O/S'
				vTip1 = "AOS"
				vDoc  = "Anulaci¢n de OS"
				vAdic = '01'
				vOrd  = '02'
			CASE Tipo='R' AND TipDoc='O/C'
				vTip1 = "ROC"
				vDoc  = "Rebaja de OC"
				vAdic = '02'
				vOrd  = '03'
			CASE Tipo='R' AND TipDoc='O/S'
				vTip1 = "ROS"
				vDoc  = "Rebaja de OS"
				vAdic = '02'
				vOrd  = '04'
		ENDCASE
		
		
		SELE PteAnu
		m.Item = 0
		m.Periodo = PteAnu.Periodo
		m.Codfte  = PteAnu.CodFte
		m.fecEmi  = PteAnu.FecPA
		m.TipDoc  = vTip
		m.Comprob = ALLTRIM(vTip)+PADL(PteAnu.NumPA,5,'0')+vMesPR
		m.ComAdi  = vAdic
		m.Fecha   = PteAnu.FecPA
		m.Glosa1  = vDoc +" de " + PteAnu.TipDoc + ": " + PteAnu.NumRef + " de Fecha " + DTOC(PteAnu.FecRef) + " y con fuente de financiamiento: " + PteAnu.CodFte
		m.Glosa2  = LEFT(PteAnu.Motivo,100)
		m.Glosa3  = SUBSTR(PteAnu.Motivo,101,100)
		m.Glosa4  = PteAnu.Observa
		m.CodCad  = PteAnu.codcad
		m.TotDeb  = ROUND(PteAnu.valtot,2)
		m.TotHab  = ROUND(PteAnu.valtot,2)
		m.Periodofte = PteAnu.Periodo
		m.Numdocfte  = PteAnu.NUMRef
		m.Docref  = PteAnu.TipDoc
		m.Nummesref = PADL(ALLTRIM(STR(MONT(PteAnu.FecRef))),2,"0")
		
		SELECT Comp
		IF f_appd()
			GATHER MEMVAR
			UNLOCK
		ENDIF
		SELE AstOrd
		SEEK PteAnu.Periodo+PteAnu.MesPA+PteAnu.NumPA+vTip1
		
		IF FOUND()
			SCAN WHILE Periodo=PteAnu.Periodo AND NumMes=PteAnu.MesPA AND NumRef=PteAnu.NumPA AND TipDoc=vTip1
				m.Item  = m.Item + 1
				m.Orden = vOrd
				m.NumMov  = 1
				m.FecEmi  = PteAnu.FecPA
				m.TipAux  = '20'
				m.Registro = PteAnu.codprv
				m.Coddep  = '111000'
				m.codfte  = PteAnu.CodFte
				m.Docref  = PteAnu.NumRef
				m.Tipdcr  = "09"
				m.Nummesref = PADL(ALLTRIM(STR(MONT(PteAnu.FecRef))),2,"0")
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
		SELE PteAnu
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
SELE PteAnu
SET INDEX TO
ERASE (vIdx)
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

FUNCTION HAYPRA
*-------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo O/C
SEEK vtip+vMesPR+vAnoPR
mRet=.F.

IF FOUND()
	IF yesno("Ya se han procesado Partes de Rebaja de " + vTip + " ¨Desea anularlas y reprocesar de nuevo?")
		DO ESPERA WITH 1,'Eliminando Asientos de ' + vTip + ' procesadas del mes ' + cMes(VAL(vMesPR))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesPR+vAnoPR
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesPR AND STR(YEAR(Diar.fecemi),4)=vAnoPR
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesPR AND STR(YEAR(Comp.Fecha),4)=vAnoPR
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


** --------------------------------------
** INTREBIN.PRG
** Integraci¢n por lotes de las Reversiones al Inabif
** AUTOR   : UDEP - 1995. SECS - Version 5.0
** Fecha   : 02/11/95 
** Modif.  : M.R.A.  13/09/2000
** ---------------------------------------

SET MEMOWIDTH TO 50
USE Parmae    IN  4 ORDER TAG Parmae1  ALIAS Parma
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
DEFINE WINDOW LIS FROM 5,15 TO 13,65 FLOAT DOUBLE TITLE '  Integrando Reversiones (T-6) ' COLOR SCHEME 5
ACTIVATE WINDOW LIS

SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
VMesREV   = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
VAnoREV   = IIF(FOUND(), YEAR(vmesact),   YEAR(DATE()))
@ 01,01 SAY "         Mes y A¤o : "
@ 01,22 GET VMesREV  PICTURE '99' VALID VMesREV > 0 .AND. VMesREV <= 12
@ 01,27 GET vAnoREV  PICTURE '9,999'
@ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOWS Lis
ERASE  WINDOWS Lis

IF !((vMesRev>MesCerrado AND vAnoRev=AnoCerrado) OR vAnoRev>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesREV   = PADL(ALLTRIM(STR(VMESREV,2)),2,'0')
	VAnoREV   = STR(VanoREV,4)
	
	SELE Parma
	SEEK 'TIPDOC'+'REB'
	IF FOUND()
		vTip = ALLTRIM(Parma.CodigoAux)
	ENDIF
	
	IF !HAYREV()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		DO REVERT6
	ENDIF
ENDIF
RETURN

PROCEDURE REVERT6
*---------------
DO Espera WITH 1,'Preparando Informacion...'

USE RevsnuTS  IN 12 ORDER TAG RevsNuTS1 ALIAS revs
USE Iteri     IN 13 ORDER TAG Iteri3    ALIAS Iteri
USE AstPre    IN 14 ORDER TAG AstPre5   ALIAS AstPre
       
SELECT revs
SET FILTER TO revs.NumMes=vMesRev AND Periodo=RIGHT(vanoRev,2)
SET RELATION TO NumMes+Numrev  INTO iteri   

COUNT TO Registros
Actual = 0
DO Espera WITH 2

GO TOP

IF EOF()
	DO STANDBY WITH ' No existe documentos Reversiones del T-6 '
	RETURN .F.
ENDIF

m.Item = 0
xtotdeb= 0
xtothab= 0

SCAN
	Actual = Actual + 1
	do progreso with 1,PADC('Procesando : '+revs.NumRev+DTOC(Fecha),59,' ')
	DO Progreso WITH 2,'',REGISTROS,ACTUAL
	
	m.TipDoc     = VTIP
	m.Comprob    = VTIP+PADL(revs.NumRev,5,'0')+VMesRev
	m.ComAdi	 = ""
	m.Fecha      = revs.fecha
	m.Glosa1     = 'Realizado por : '+ALLTRIM(revs.nombre)
	m.Glosa2     = MLINE(revs.OBSERVA,1)+MLINE(revs.OBSERVA,2)
	m.Glosa3     = MLINE(revs.OBSERVA,3)+MLINE(revs.OBSERVA,4)+MLINE(revs.OBSERVA,5)+MLINE(revs.OBSERVA,6)
	m.Glosa3     = MLINE(revs.OBSERVA,7)+MLINE(revs.OBSERVA,8)+MLINE(revs.OBSERVA,9)+MLINE(revs.OBSERVA,10)
	m.CodCtc     = revs.CodCtC
	m.CodCad     = revs.CodCad
	m.codfte     = revs.codfte
	m.Periodofte = revs.Periodo
	m.Nummesfte  = revs.Nummes
	m.Numdocfte  = revs.NumREV
	m.TipDcr     = vtip
	
	SELECT Comp
	IF F_Appd()
		GATHER MEMVAR
		UNLOCK
	ENDIF
	
	m.Item = 0
	xtotdeb= 0
	xtothab= 0
	
	*-Se generan los asientos Presupuestales por cada una de las partidas
	*-las Patrimoniales las ingresa en el comprobante al momento de Jirar Cheque
	
	SELECT AstPre

	SEEK vMesRev+Revs.NumRev+'REB'
	
	SCAN WHILE NumRef = Revs.NumRev .AND. NumMes = Revs.NumMes AND TipDoc="REB"
		m.Item     = m.Item + 1
		m.ORDEN    = '01'
		m.NumMov   = 1
		m.FecEmi   = m.fecha
		m.TipAux   = revs.TIPAUX
		m.Registro = revs.CodPrv
		m.CodCtc   = AstPre.CodCtC
		m.CodFte   = REVS.codfte
		m.CodCtc   = revs.CodCtC
		m.Coddep   = '113000'
		m.Codpart  = iteri.Codpart
		m.Tipo     = AstPre.Tipo
		m.Cuenta   = IIF(m.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
		m.Montos   = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
		
		xtotdeb=xtotdeb+IIF(Tipo='D',m.Montos,0)
		xtotHab=xtotHab+IIF(Tipo='H',m.Montos,0)
		
		SELECT Diar
		IF F_APPD()
			GATHER MEMVAR
			UNLOCK
		ENDIF
		SELE AstPre
	ENDSCAN
	
	Sele Comp
	IF RLOCK()
		REPLACE Comp.totdeb     WITH xtotdeb
		REPLACE Comp.tothab     WITH xtothab
		xtotdeb=0
		xtothab=0
		vitem=0
	ENDIF
	SELE revs
ENDSCAN

DO Progreso WITH 3

SET CONSOLE ON
SET BELL TO 800,5
_WRAP = .F.
? CHR(7)+CHR(7)+CHR(7)
_WRAP = .T.
SET BELL TO 512,2
SET BELL TO 512,2
SET CONSOLE OFF

DO Standby WITH 'Proceso Finalizado; REV Integradas.....!'      
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
DEACTIVATE WINDOWS Lis
RELEASE WINDOW Lis
RETURN

FUNCTION HAYREV
*--------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo 
* Reversiones del Tesoro P£blico
SEEK vtip+vMesRev+vAnoRev
mRet=.F.

IF FOUND()
	IF yesno('T-6 del Mes '+vmesRev+'-'+vanoRev+' Ya integrado, Reprocesa?')
		DO ESPERA WITH 1,'Eliminando Asientos de T-6 procesadas del mes ' + cMes(VAL(vMesRev))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesRev+vAnoRev
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesRev AND STR(YEAR(Diar.fecemi),4)=vAnoRev
			SELECT Diar
			SET ORDER TO Diario11
			UNLOCK
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesRev AND STR(YEAR(Comp.Fecha),4)=vAnoRev
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

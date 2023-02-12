CLOS DATA
*USE OrdSer   IN  1  ORDER TAG OrdSer1  ALIAS OrdSe
*USE AsiAut   IN  2  ORDER TAG AsiAut1
*USE Parmae   IN  3  ORDER TAG Parmae1  ALIAS Parma
*USE AstOrd	 IN  4	ORDER TAG AstOrd1  ALIAS AstOrd

*USE OrdCom   IN 1 ORDER TAG OrdCom1  ALIAS Orden
*USE IteOc1   IN 2 ORDER TAG Iteoc11  ALIAS IteOc1
*USE AsiAut   IN 3 ORDER TAG AsiAut1
*USE Parmae   IN 4 ORDER TAG Parmae1  ALIAS Parma
*USE AstOrd	 IN 5 ORDER TAG AstOrd1  ALIAS AstOrd

*USE Poliza   IN 1  order tag poliza1  ALIAS poliza
*USE AsiAut   IN 2 ORDER TAG AsiAut1
*USE Parmae   IN 3 ORDER TAG Parmae1  ALIAS Parma
*USE AstOrd	 IN 4 ORDER TAG AstOrd1  ALIAS AstOrd

USE ComPag   IN  1 ORDER TAG ComPag1 ALIAS ComPag 
USE HojCon   IN  2 ORDER TAG HojCon1 ALIAS Hoja   
USE AsiAut   IN  3 ORDER TAG AsiAut1
USE Parmae   IN  4 ORDER TAG Parmae1 ALIAS Parma
USE AstOrd	 IN  5 ORDER TAG AstOrd1 ALIAS AstOrd

SELE Compag
SET RELATION TO NumMesHC+NumHC INTO Hoja

SCAN FOR ESTADO#'99'
	IF Hoja.TipDoc = "O/S"
		SCATTER MEMVAR
		DO AsiOrdOS
		SELE ComPag
	ENDIF
ENDSCAN

PROCEDURE AsiOrdOS
*---------------
SELE AsiAut
SEEK "OSC"+"   "+"ASTORD"

IF !FOUND()
	DO StandBy WITH "PARAMETRO DE CTAS. DE ORDEN NO INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
	USE IN AsiAut
	RETURN
 ELSE
	cCtaD  = DCuenta
	cCtaH  = HCuenta
ENDIF
cPeriodo = ComPag.periodo
cFecha   = ComPag.FecCP
cNumMes  = ComPag.NumMes
cNumOSC  = Hoja.NumRef
nImpTot  = Hoja.ImpTot

SELE AstOrd
SEEK cPeriodo+cNumMes+cNumOSC+"OSC"
IF FOUND()
	WAIT WIND cPeriodo+" "+cNumMes+" "+cNumOSC
 ELSE
	FOR i = 1 TO 2
		IF f_Appd()
			REPLACE Periodo WITH cPeriodo ,;
					NUMMES	WITH cNumMes  ,;
					NUMREF	WITH cNumOSC   ,;
					TIPDOC	WITH "OSC"     ,;
					FECHA	WITH cFecha,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,nImpTot,0),;
					MTOHAB	WITH IIF(i=2,nImpTot,0)
			UNLOCK
		ENDIF
	ENDFOR
ENDIF
RETURN


*PROCEDURE AsiOrd
*---------------
SELE AsiAut
SEEK "P/E"+"   "+"ASTORD"

IF !FOUND()
	DO StandBy WITH "PARAMETRO DE CTAS. DE ORDEN INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
	RETURN
 ELSE
	cCtaD = DCuenta
	cCtaH = HCuenta
ENDIF

SELE AstOrd
cNumMes = PADL(MONT(M.FECINI),2,'0')
SEEK m.Periodo+cNumMes+m.NumPol+"P/E"
IF FOUND()
	WAIT WIND M.PERIODO+" "+cNUMMES+" "+M.NUMPol
 ELSE
	FOR i = 1 TO 2
		IF f_Appd()
			REPLACE Periodo WITH m.Periodo ,;
					NUMMES	WITH cNumMes  ,;
					NUMREF	WITH m.NumPol   ,;
					TIPDOC	WITH "P/E"     ,;
					FECHA	WITH m.FecIni,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,m.ValTot,0),;
					MTOHAB	WITH IIF(i=2,m.ValTot,0)
			UNLOCK
		ENDIF
	ENDFOR
ENDIF
RETURN


*PROCEDURE AsiOrd
*---------------
SELE AsiAut
SEEK "O/C"+"   "+"ASTORD"

IF !FOUND()
	DO StandBy WITH "PARAMETRO DE CTAS. DE ORDEN INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
	RETURN
 ELSE
	cCtaD = DCuenta
	cCtaH = HCuenta
ENDIF
SELE AstOrd
SEEK m.Periodo+m.NumMes+m.NumOc+"O/C"
IF FOUND()
	WAIT WIND M.PERIODO+" "+M.NUMMES+" "+M.NUMOC
 ELSE
	FOR i = 1 TO 2
		IF f_Appd()
			REPLACE Periodo WITH m.Periodo ,;
					NUMMES	WITH m.NumMes  ,;
					NUMREF	WITH m.NumOC   ,;
					TIPDOC	WITH "O/C"     ,;
					FECHA	WITH m.FecEmi,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,m.ValTot,0),;
					MTOHAB	WITH IIF(i=2,m.ValTot,0)
			UNLOCK
		ENDIF
	ENDFOR
ENDIF
RETURN

*PROCEDURE AsiOrd
*---------------
SELE AsiAut
SEEK "O/S"+"   "+"ASTORD"

IF !FOUND()
	DO StandBy WITH "PARAMETRO DE CTAS. DE ORDEN INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
	RETURN
 ELSE
	cCtaD = DCuenta
	cCtaH = HCuenta
ENDIF
SELE AstOrd
SEEK m.Periodo+m.NumMes+m.NumOS+"O/S"
IF FOUND()
	WAIT WIND M.PERIODO+" "+M.NUMMES+" "+M.NUMOS
 ELSE
	FOR i = 1 TO 2
		IF f_Appd()
			REPLACE Periodo WITH m.Periodo ,;
					NUMMES	WITH m.NumMes  ,;
					NUMREF	WITH m.NumOS   ,;
					TIPDOC	WITH "O/S"     ,;
					FECHA	WITH m.FecEmi,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,m.ValTot,0),;
					MTOHAB	WITH IIF(i=2,m.ValTot,0)
			UNLOCK
		ENDIF
	ENDFOR
ENDIF
RETURN


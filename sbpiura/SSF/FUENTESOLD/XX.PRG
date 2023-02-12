*DO AGRKAR
*DO ACT_NumCob
DO ACT_kARSSF

PROCEDURE AGRKAR
*---------------
CLOS DATA
USE ITECOB IN 1
USE KARSSF IN 2

GO TOP
SCAN
	SCATTER MEMVAR
	m.Fecha = m.FecPag
	m.Correl = m.NumCob
	SELE KARSSF
	APPEND BLANK
	GATHER MEMVAR
	SELE ITECOB
ENDSCAN
RETURN


PROCEDURE Act_NumCob
*-------------------
CLOS DATA
USE ITECOB IN 1 ORDER TAG IteCob1
GO TOP
SCAN
	m.CodCon = CodCon
	m.NumCob = 0
	scan WHILE CodCon = m.CodCon
		m.NumCob = m.NumCob + 1
		REPLACE NumCob WITH PADL(ALLTRIM(STR(m.NumCob)),4,'0')
	ENDscan
	SKIP -1
ENDSCAN
RETURN

PROCEDURE ACT_kARSSF
*-------------------
CLOS DATA
USE KARSSF IN 1 ORDER TAG KARSSF2
GO TOP
SCAN
	m.CodCon = CodCon
	m.SdoCob = 0
	m.SdoCC  = 0
	m.SdoGA  = 0
	SCAN WHILE CodCon = m.CodCon
		m.CtaCap = CanCob/2
		m.GtoAdm = CanCob/2
		m.SdoCob = m.SdoCob + CanCob
		m.SdoCC  = m.SdoCC  + m.CtaCap
		m.SdoGA  = m.SdoGA  + m.GtoAdm
		REPLACE CtaCap WITH m.CtaCap, GtoAdm WITH m.GtoAdm, SdoCob WITH m.SdoCob, SdoCC WITH m.SdoCC, SdoGA WITH m.SdoGA
	ENDSCAN
	SKIP -1
ENDSCAN
RETURN

*USE ITESOC ORDER TAG ITESOC1
*GO TOP
*SCAN
*	cCodCon = CodCon
*	nNumAfi = 0
*	SCAN WHILE cCodCon = CodCon
*		nNumAfi = nNumAfi + 1
*		REPLACE NumAfi WITH nNumAfi
*	ENDSCAN
*	SKIP -1
*ENDSCAN


*CLOS DATA
*USE ITESOC IN 1
*USE MAESOC IN 2

*SELE IteSoc
*COPY STRU TO MSBK

*USE MSBK IN 3

*SELE Maesoc
*GO TOP
*SCAN
*	m.Codcon = codcon
*	m.NomAfi = NomBre
*	m.ApeAfi = Apelli
*	m.Edad   = Edad
*	m.Parent = '00'
*	m.FecIns = FecIns
*	m.Fecder = fFecder(m.FecIns)
*	m.TipDer = '01'
*	m.Estado = '00'
*
*	SELE MSBK
*	APPEND BLANK
*	GATHER MEMVAR
*
*	SELE MaeSoc
*
*ENDSCAN

* append from msbk
* replace numafi with str(val(numafi)+1,1) all


FUNCTION fFecDer
*---------------
PARAMETERS dFec
PRIVATE vFecIns,vFecDer
vFecIns = dFec
nDia = DAY(FecIns)
nMes = MONT(FecIns)
nAno = YEAR(FecIns)

nMes=nMes+IIF(VAL(CodCon)>2000,6,4)

IF nMes>12
	nAno=nAno+1
	nMes=nMes-12
ENDIF

IF nDia=31 AND !(nMes=1 OR nMes=3 OR nMes=5 OR nMes=7 OR nMes=8 OR nMes=10 OR nMes=12)
	nDia=30
ENDIF

IF nDia=30 AND !(nMes=4 OR nMes=6 OR nMes=9 OR nMes=11)
	nDia=29
ENDIF

IF nDia=29 AND MOD(nAno,4)#0
	nDia=28
ENDIF

RETURN CTOD(PADL(nDia,2,'0')+"/"+PADL(nMes,2,'0')+"/"+PADL(nAno,4,'0'))


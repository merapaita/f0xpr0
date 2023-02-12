PARAMETERS Procedim
mRet = &Procedim

RETURN mRet

PROCEDURE VER
*------------
WAIT WIND ALIAS()

PROCEDURE CoValpar
*-----------------
*VariosP("CoValPar(1)")
PARAMETERS nOpc
IF nOpc = 1
	IF Hecho
		RETURN 0
	 ELSE
		Hecho = .T.
	ENDIF
ENDIF
PRIVATE cAlias
cAlias = ALIAS()
Borra = .F.
IF !USED("RevSNu")
	USE RevSNu IN 0 ORDER TAG RevSNu1 ALIAS Rever
	Borra = .T.
	SELE IteRi
	SET RELATION TO NumMes+NumRev INTO Rever ADDITIVE
ENDIF
USE MovBco IN 0 ORDER TAG MovBco1
SELE Repo
GO TOP
IF EOF()
	RETURN 0
ENDIF
FOR i = 1 TO VAL(vCalend)
	Campo = "MES_"+PADL(ALLTRIM(STR(I)),2,"0")
	REPLACE TotAfe WITH TotAfe-&Campo ALL
	REPLACE &Campo  WITH 0 ALL
ENDFOR
REPLACE TotCal WITH 0 ALL

SCAN

*IF REPO.CODPART='4.2.1'
*	WAIT WIND STR(REPO.TOTAFE,18,4)
*ENDIF

	SELE ItePar
	SEEK Repo.Periodo+Repo.UniGes+Repo.UniEje+Repo.CodCad+Repo.CodFte+Repo.CodPart
	IF FOUND()
		REPLACE Repo.TotAfe WITH Repo.TotAfe+ItePar.TotAfe
	ENDIF
	SELE Repo
ENDSCAN

SELE IteRi
GO TOP
SCAN
	cPartida = ALLTRIM(CodPart)
	IF EMPTY(NumRI) AND !EMPTY(NumRev)
		cCadena = Rever.CodCad
		cFuente = Rever.CodFte
		nImporte = ImpParc*-1
	 ELSE
		cCadena = RecIng.CodCad
		cFuente = RecIng.CodFte
		nImporte = ImpParc
	ENDIF
	Campo = "Mes_"+IteRi.NumMes
	nTotCal =IIF(vCalend = NumMes,nImporte,0)
	SELE Repo
	
	SEEK cCadena+cFuente+cPartida
	IF !FOUND()
		IF AT("N/A",IteRI.Glosa)#0
			SELE MovBco
			SEEK IteRi.NumMes+IteRI.NumRI
			IF FOUND()
				cCadena = CodCad
				cFuente = CodFte
			ENDIF
			SELE Repo
			SEEK cCadena+cFuente+cPartida
		ENDIF
	ENDIF
	IF FOUND() AND !EMPTY(cPartida)
		REPLACE TotAfe WITH TotAfe+nImporte
		REPLACE TotCal WITH TotCal+nTotCal
		REPLACE &Campo WITH &campo+nImporte
	 ELSE
	ENDIF
	SELE IteRI
ENDSCAN

IF Borra
	USE IN Rever
ENDIF
USE IN MovBco

SELE (cAlias)
GO TOP

RETURN 0
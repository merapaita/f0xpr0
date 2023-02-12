*--------------------------------------------------------------------------
* RepPla1.Prg
* Reporte de Planilla
* Autor:M.R.A.
* Parametro : nOpcion 1 - Personal Activo
*					  2 - Vacaciones
*--------------------------------------------------------------------------
PARAMETERS nOpcion

PUBLIC nSumaI,nSumaG,OkCancel,cMes_Vac,cMes_Sue,vTipPla,vIdx,vDbf,nTotApo01,nTotApo02,vIdx1,vIdx2,vdbf1,xSale,cPeriodo,nNumP
STORE 0 TO nSumaI,nSumaG
STORE .F. TO xSale
*- Abriendo Archivos
vDbf = SYS(3)+'.Dbf'

SELECT P.LIB_ELE, P.NRO_CTA, P.APE_NOM, P.FEC_ING, P.DIA_SUS, P.FEC_SEC, P.A¥O_SER,;
  P.TOT_SUE, P.CON_PER, P.REC_PER, P.NIV_PER, P.CAR_PER, P.AFP_PER, P.NUM_PLA, P.MES_VAC,;
  P.CODCAD, P.OBS_PER, PL.MES_SUE, PL.Periodo, PL.ESTADO, PL.TIP_ADN, PL.COD_ADN1, PL.TOT_ADN1,;
  PL.COD_ADN2, PL.TOT_ADN2, PL.COD_ADN3, PL.TOT_ADN3, PL.COD_ADN4, PL.TOT_ADN4,;
  PL.COD_ADN5, PL.TOT_ADN5, PL.Hoja, P.Reservada, P.Reservada1;
 FROM PERSONAL P, PLANILLA  Pl ;
 WHERE PL.LIB_ELE = P.LIB_ELE;
 INTO TABLE (vDbf)

CLOS DATA
USE (vDbf)    IN 1 						ALIAS Planilla
USE ParMae    IN 2 ORDER TAG ParMae1	ALIAS Parma
USE TOTAPOR   IN 4 ORDER TAG TOTAPOR1
USE ObserPla  IN 5 ORDER TAG ObserPla1	ALIAS ObsPla

SELE Parma
SEEK "FECSIS"
VMES = LEFT(Parma.Descri,2)
vAno = SUBS(Parma.Descri,4,4)

SEEK 'APORTA01'
pCEM = ALLTRIM(STR(NumDec,6,2))
SEEK 'APORTA02'
pIES = ALLTRIM(STR(NumDec,6,2))

DO Escoge

IF LASTKEY()=27 OR OkCancel=2
	CLOS DATA
	ERASE (vDbf)
	RETURN
ENDIF

SELE Planilla
SET RELATION TO Lib_Ele+cPeriodo+cMes_Sue INTO TotApor  ADDITIVE
*SET RELATION TO Periodo+Mes_Sue INTO ObsPla  ADDITIVE
*SET SKIP TO ObsPla
IF OkCancel = 1
	vIdx = SYS(3) + '.IDX'
	IF allt(vmes)#cMes_Sue AND allt(vAno)#cPeriodo
		do standby with 'MES A REPORTAR ES DIFERENTE AL DEL SISTEMA'
		CLOS DATA
		OKCANCEL = 2
		RETURN
	ENDIF
	xObs = 0
	DO CASE
*		CASE vTipPla=1
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Lib_Ele+Tip_Adn TO (vIdx) FOR (Con_Per='1' OR  Con_Per='2')  AND (Rec_Per='1') AND IIF(nOpcion=1,Estado#"03",Estado="02") AND (mes_Sue=cMes_sue) AND (Periodo=cPeriodo)
*			vtipo = IIF(nOpcion=2,'DE VACACIONES -','DE REMUNERACIONES -')+' PERMANENTES'
*			xObs = 1
*		CASE vTipPla=2
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Con_Per='5' AND Rec_Per='1') AND IIF(nOpcion=1,Estado#"03",Estado="02") AND (mes_Sue=cMes_sue) AND (Periodo=cPeriodo)
*			vtipo = IIF(nOpcion=2,'DE VACACIONES -','DE REMUNERACIONES -')+' CONTRATADOS - D.T.'
*			xObs = 2
*		CASE vTipPla=3
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Con_Per='5' OR  Con_Per='2') AND (Rec_Per='2') AND IIF(nOpcion=1,Estado#"03",Estado="02") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
**			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Con_Per='5' and Rec_Per='2') AND IIF(nOpcion=1,Estado#"03",Estado="02") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = IIF(nOpcion=2,'DE VACACIONES -','DE REMUNERACIONES -')+' PERMANENTES - D.R.'
*			xObs = 3
*		CASE vTipPla=4
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Lib_Ele+Tip_Adn TO (vIdx) FOR (Con_Per="3" OR Con_Per="4") AND Estado#'06'  AND Estado#'07' AND(mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
**			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Con_Per="3" OR Con_Per="4") AND Estado#'06'  AND Estado#'07' AND(mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = 'PENSIONISTAS'
*			xObs = 4
*		CASE vTipPla=5
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR Reservada AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo) AND IIF(nOpcion=1,Estado#"03",Estado="02")
*			vtipo = IIF(nOpcion=2,'DE VACACIONES -','DE REMUNERACIONES -')+' CONTRATADOS - DT. RD 218-99-SBP-P; RD 255-2001-SBP-P '
*			xObs = 5
*		CASE vTipPla=6
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR Reservada1 AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo) AND IIF(nOpcion=1,Estado#"03",Estado="02")
*			vtipo = IIF(nOpcion=2,'DE VACACIONES -','DE REMUNERACIONES -')+' CONTRATADOS - DT. R.P.D. N§ 176-2003-SBP-P DEL 17/06/2003'
*			xObs = 6
*		CASE vTipPla=7
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Estado="06") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = '90% PENSION DEFINITIVA'
*			xObs = 7
*		CASE vTipPla=8
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Estado="10") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = 'ADICIONAL - (DEVENGADOS)'
*			xObs = 8
*		CASE vTipPla=9
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Estado="10") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = 'ADICIONAL DE VIUDEZ - (DEVENGADOS)'
*			xObs = 9
*		CASE vTipPla=10
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Estado="08") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = 'ADICIONAL DE VIUDEZ'
*			xObs = 10
*		CASE vTipPla=11
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Estado="12") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = 'EXTRAORDINARIA DE REMUNERACIONES POR DONACIONES Y TRANSFERENCIAS'
*			xObs = 11
		CASE vTipPla=1
			SELECT Planilla
			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo) AND Con_Per='7'
			vtipo = 'PLANILLA DE REMUNERACIONES D.L. Nro. 728.'
			xObs = 12
	ENDCASE
ENDIF

*SET STEP ON
DO AgrNHoja

IF nOpcion = 1
	REPLACE Tot_Adn1 WITH 0 FOR Estado='02'
	REPLACE Tot_Adn2 WITH 0 FOR Estado='02'
	REPLACE Tot_Adn3 WITH 0 FOR Estado='02'
	REPLACE Tot_Adn4 WITH 0 FOR Estado='02'
	REPLACE Tot_Adn5 WITH 0 FOR Estado='02'
ENDIF

*Casos Escepcional
IF vTipPla = 3
	REPLACE Tot_Adn1 WITH 0 FOR Reservada
	REPLACE Tot_Adn2 WITH 0 FOR Reservada
	REPLACE Tot_Adn3 WITH 0 FOR Reservada
	REPLACE Tot_Adn4 WITH 0 FOR Reservada
	REPLACE Tot_Adn5 WITH 0 FOR Reservada
ENDIF

IF vTipPla = 3
	REPLACE Tot_Adn1 WITH 0 FOR Reservada1
	REPLACE Tot_Adn2 WITH 0 FOR Reservada1
	REPLACE Tot_Adn3 WITH 0 FOR Reservada1
	REPLACE Tot_Adn4 WITH 0 FOR Reservada1
	REPLACE Tot_Adn5 WITH 0 FOR Reservada1
ENDIF

DO REPORTE WITH 2,'Planilla', ' Planillas del Personal',2,.F.,.T.

DO Tot_Apor
DO CreaTabla
SELE Planilla
SELE ObsPla
DO REPORTE WITH 2,'ResPla1', ' Planillas del Personal',2,.F.,.T.
DO Salida

FUNCTION Sum_Tot
*----------------
PARAMETERS cTip_Adn
IF cTip_Adn='+'
	nSumaI = nSumaI+planilla.tot_adn1+planilla.tot_adn2+planilla.tot_adn3+planilla.tot_adn4+planilla.tot_adn5
 ELSE
	nSumaG = nSumaG+planilla.tot_adn1+planilla.tot_adn2+planilla.tot_adn3+planilla.tot_adn4+planilla.tot_adn5
ENDIF
RETURN 0

FUNCTION Lim_Tot
*----------------
STORE 0 TO nSumaI,nSumaG
RETURN 0

PROCEDURE Escoge
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE 'Listado de Planillas' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1 TO vTipPla,OkCancel
vCodPer = SPAC(8)
cMes_Sue = vMes
cPeriodo = vAno
vdato   = space(20)
@ 02,01 SAY " Tipo de Planilla : " GET vTipPla  FUNCTION '^ Personal 728'
@ 06,01 SAY "   Mes a Reportar : "
@ 06,22 GET cMes_Sue  Valid Val_para(cMes_Sue,'FECMES',' ',25)
@ 06,25 GET cPeriodo
@ 08,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
cMes_Vac = cMes_Sue
RELEASE WINDOW LIS
RETURN

PROCEDURE Salida
*---------------
CLOS DATA
RELEASE WIND ALL
ERASE (vIdx)
ERASE (vIdx1)
ERASE (vDbf)
ERASE (vDbf1)

FUNCTION Tit_Aum
*---------------
*PARAMETERS cCod
PRIVATE cAlias
cAlias = ALIAS()
SELE Tabla

IF xSale
	RETURN ""
ENDIF

IF Tip_Adn='+'
	cCod = Tabla.Cod_Adn
 ELSE
	SELE (cAlias)
	RETURN ""
ENDIF
SELE Parma
SEEK 'AUMLEY'+cCod
IF FOUND()
	mRet = ALLTRIM(Descri)
 ELSE
 	mRet = 'Cod. no Enconcontrado.'
ENDIF
SELE (cAlias)
RETURN mRet

FUNCTION ObsPla
*--------------
SELE ObsPla
SET FILTER TO Periodo=cPeriodo AND Mes=cMes_Sue AND TipPla= xObs
GO TOP
*BROW NORM
RETURN ""

FUNCTION Tit_Des
*---------------
PARAMETERS cCod
PRIVATE cAlias
cAlias = ALIAS()
SELE Tabla

IF xSale
	RETURN ""
ENDIF

IF Tip_Adn='-'
	cCod = Tabla.Cod_Adn
 ELSE
	SELE (cAlias)
	RETURN ""
ENDIF
SELE Parma
SEEK 'DESLEY'+cCod
IF FOUND()
	mRet = ALLTRIM(Descri)
 ELSE
	mRet = 'Cod. no Enconcontrado.'
ENDIF
SELE (cAlias)
RETURN mRet


FUNCTION Tot_Aum
*---------------
*PARAMETERS cCod
PRIVATE i,cCam1,cCam2,nSumCod,mRet,cAlias
cAlias = ALIAS()
mRet = 0
IF xSale
	RETURN 0
ENDIF
SELE Tabla
IF Tip_Adn='+'
	cCod = Tabla.Cod_Adn
 ELSE
	SELE (cAlias)
	RETURN 0
ENDIF
SELE Planilla
FOR i = 1 to 5
	cCam1 = 'Cod_Adn' + ALLTRIM(STR(i))
	cCam2 = 'Tot_Adn' + ALLTRIM(STR(i))
	SUM &cCam2 TO nSumCod FOR Tip_Adn='+' AND &cCam1=cCod
	nSumaI = nSumaI + nSumCod
	mRet = mRet + nSumCod
ENDFOR
SELE Tabla
SKIP 1
IF EOF()
	GO BOTT
	xSale = .T.
ENDIF
SELE (cAlias)

RETURN mRet

FUNCTION Tot_Des
*---------------
*PARAMETERS cCod
PRIVATE i,cCam1,cCam2,nSumCod,mRet,cAlias
cAlias = ALIAS()
mRet = 0
IF xSale
	RETURN 0
ENDIF
SELE Tabla
IF Tip_Adn='-'
	cCod = Tabla.Cod_Adn
 ELSE
	SELE (cAlias)
	RETURN 0
ENDIF
SELE Planilla
FOR i = 1 to 5
	cCam1 = 'Cod_Adn' + ALLTRIM(STR(i))
	cCam2 = 'Tot_Adn' + ALLTRIM(STR(i))
	SUM &cCam2 TO nSumCod FOR Tip_Adn='-' AND &cCam1=cCod
	nSumaG = nSumaG + nSumCod
	mRet = mRet + nSumCod
ENDFOR
SELE Tabla
SKIP 1
IF EOF()
	GO BOTT
	xSale = .T.
ENDIF
SELE (cAlias)
RETURN mRet

FUNCTION Tot_Apor
*----------------
PRIVATE cLib_Ele
SELE Planilla
nTotApo01=0
nTotApo02=0
i = 0
GO TOP
SCAN 
	cLib_Ele = Lib_Ele
	SCAN WHILE cLib_Ele = Lib_Ele
		i=i+1
		IF i=1
			nTotApo01=nTotApo01+IIF(nOpcion=1,IIF(!(Mes_Vac=Mes_Sue OR (vTipPla=3 AND (Reservada OR reservada1))),totapor.apo_01,0),IIF((Mes_Vac=Mes_Sue AND (vTipPla=3 AND (Reservada OR reservada1))),0,totapor.apo_01))
			nTotApo02=nTotApo02+IIF(nOpcion=1,IIF(!(Mes_Vac=Mes_Sue OR (vTipPla=3 AND (Reservada OR reservada1))),totapor.apo_02,0),IIF((Mes_Vac=Mes_Sue AND (vTipPla=3 AND (Reservada OR reservada1))),0,totapor.apo_02))
*			nTotApo01=nTotApo01+ROUND(IIF(Estado#'02',totapor.apo_01,0),0)
*			nTotApo02=nTotApo02+ROUND(IIF(Estado#'02',totapor.apo_02,0),0)
		ENDIF
	ENDSCAN
	SKIP -1
	i = 0
ENDSCAN
RETURN

*FUNCTION Tot_Apor
*----------------
SELE Planilla
nTotApo01=0
nTotApo02=0
i = 0
SCAN 
	i=i+1
	IF i=1
		nTotApo01=nTotApo01+ROUND(IIF(Estado#'02',totapor.apo_01,0),0)
		nTotApo02=nTotApo02+ROUND(IIF(Estado#'02',totapor.apo_02,0),0)
	 ELSE
		IF I=8
			I=0
		ENDIF
	ENDIF
ENDSCAN
RETURN

PROCEDURE CreaTabla
*------------------
vIdx1 = SYS(3)+'.IDX'
vDbf1 = SYS(3)+'.DBF'
USE ItePer IN 0
SELE ItePer
COPY TO (vDbf1) FOR Mes_Sue=cMes_Sue AND Periodo=cPeriodo AND !EMPTY(Cod_Adn)
USE (vDbf1) ALIAS Tabla EXCLU
INDEX ON Tip_Adn+Cod_Adn UNIQ TO (vIdx1)
ZAP
SELE Planilla
GO TOP
SCAN
	FOR i= 1 to 5
		cCam1 = 'Planilla.Cod_Adn' + ALLTRIM(STR(i))
		cCam2 = 'Planilla.Tot_Adn' + ALLTRIM(STR(i))
		IF EMPTY(&cCam1) AND EMPTY(&cCam2)
		 ELSE
			m.Tip_Adn = Planilla.Tip_Adn
			m.Cod_Adn = &cCam1
			m.Tot_Adn = &cCam2
			SELE Tabla
			IF !SEEK(m.Tip_Adn+m.Cod_Adn)
				APPEND BLANK
				GATHER MEMVAR
			ENDIF
			SELE Planilla
		ENDIF
	ENDFOR
ENDSCAN
SELE TABLA
GO TOP
RETURN

FUNCTION Filt
*------------
PARAMETERS nOpcion
IF nOpcion=1
	SET FILTER TO TipAdn='+' 
	GO TOP
 ELSE
	SET FILTER TO TipAdn='-'
	GO TOP
ENDIF

PROCEDURE AgrNHoja
*-----------------
PRIVATE nHoja,nCont
nHoja=1
nCont=0
GO TOP
SCAN
	cLib_Ele=Lib_Ele
	nCont=nCont+1
	SCAN WHIle cLib_Ele=Lib_Ele
		REPLACE Hoja WITH nHoja
	ENDSCAN
	IF nCont=4
		nCont=0
		nHoja=nHoja+1
	ENDIF
	SKIP -1
ENDSCAN
RETURN

FUNCTION nNumP
*-------------
nNumP = _PageNo
RETURN 0

FUNCTION nNumP1
*-------------
_Pageno = nNumP+1
RETURN 0
*--------------------------------------------------------------------------
* RepBol1.Prg
* Reporte de Boletas
* Autor:M.R.A.
* Parametro : nOpcion 1 - Personal Activo
*					  2 - Personal Jubilado
*--------------------------------------------------------------------------
PARAMETERS nOpcion

PUBLIC nSumaI,nSumaG,OkCancel,cMes_Vac,cMes_Sue,vTipPla,vIdx,vDbf,cPeriodo
STORE 0 TO nSumaI,nSumaG

*- Abriendo Archivos
vDbf = SYS(3)+'.Dbf'

SELECT P.LIB_ELE, P.NRO_CTA, P.APE_NOM, P.FEC_ING, P.DIA_SUS, P.FEC_SEC, P.A¥O_SER,;
  P.TOT_SUE, P.CON_PER, P.REC_PER, P.NIV_PER, P.CAR_PER, P.AFP_PER, P.NUM_PLA, P.MES_VAC,;
  P.CODCAD, P.OBS_PER, PL.MES_SUE, PL.Periodo, PL.ESTADO, PL.TIP_ADN, PL.COD_ADN1, PL.TOT_ADN1,;
  PL.COD_ADN2, PL.TOT_ADN2, PL.COD_ADN3, PL.TOT_ADN3, PL.COD_ADN4, PL.TOT_ADN4,;
  PL.COD_ADN5, PL.TOT_ADN5, PL.Hoja, P.Reservada, P.Reservada1;
 FROM PERSONAL P, PLANILLA  Pl;
 WHERE PL.LIB_ELE = P.LIB_ELE;
 INTO TABLE (vDbf)
 
CLOS DATA
USE (vDbf)   IN 1 					ALIAS Planilla
USE ParMae   IN 2 ORDER TAG ParMae1 ALIAS Parma

SELE Parma
SEEK "FECSIS"
VMES = LEFT(Parma.Descri,2)
vAno = SUBS(parma.descri,4,4)

DO Escoge

IF LASTKEY()=27 OR OkCancel=2
	CLOS DATA
	ERASE (vDbf)
	RETURN
ENDIF

SELE Planilla

IF OkCancel = 1 
	vIdx = SYS(3) + '.IDX'
	IF allt(vmes)#cMes_Sue AND allt(vAno)#cPeriodo
		do standby with 'MES A REPORTAR ES DIFERENTE AL DEL SISTEMA'
		CLOS DATA
		OKCANCEL = 2
		RETURN
	ENDIF
	DO CASE
*		CASE vTipPla=1
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Lib_Ele+Tip_Adn TO (vIdx) FOR (Con_Per='1' OR  Con_Per='2') AND (Rec_Per='1') AND IIF(nOpcion=1,Estado#"03",Estado="02") AND (mes_Sue=cMes_sue) AND (Periodo=cPeriodo)
*			vtipo = IIF(nOpcion=2,'DE VACACIONES -','DE REMUNERACIONES -')+' PERMANENTES'
*		CASE vTipPla=2
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR Con_Per='5' AND Rec_Per='1' AND IIF(nOpcion=1,Estado#"03",Estado="02")AND (mes_Sue=cMes_sue) AND (Periodo=cPeriodo)
*			vtipo = IIF(nOpcion=2,'DE VACACIONES -','DE REMUNERACIONES -')+' CONTRATADOS - D.T.'
*		CASE vTipPla=3
*			SELECT Planilla
*			INDEX ON Num_Pla+periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Con_Per='5' OR  Con_Per='2') AND (Rec_Per='2') AND IIF(nOpcion=1,Estado#"03",Estado="02") AND (mes_Sue=cMes_sue) AND (Periodo=cPeriodo)
*			vtipo = IIF(nOpcion=2,'DE VACACIONES -','DE REMUNERACIONES -')+' CONTRATADOS - D.R.'
*		CASE vTipPla=4
*			SELECT Planilla
*			INDEX ON Num_Pla+periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Con_Per="3".OR. Con_Per="4") AND Estado#'06'  AND Estado#'07' AND (mes_Sue=cMes_sue) AND (Periodo=cPeriodo)
*			vtipo = 'PENSIONISTAS'
*		CASE vTipPla=5
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR Reservada  AND IIF(nOpcion=1,Estado#"03",Estado="02") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = IIF(nOpcion=2,'DE VACACIONES -','DE REMUNERACIONES -')+' CONTRATADOS - D.T. R.D 218-99-SBP-P; RD 255-2001-SBP-P '
*		CASE vTipPla=6
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR Reservada1 AND  IIF(nOpcion=1,Estado#"03",Estado="02") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = IIF(nOpcion=2,'DE VACACIONES -','DE REMUNERACIONES -')+' CONTRATADOS - DT. R.P.D. N§ 176-2003-SBP-P DEL 17/06/2003'
*		CASE vTipPla=7
*			SELECT Planilla
*			INDEX ON Num_Pla+periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR Estado='06' AND (mes_Sue=cMes_sue) AND (Periodo=cPeriodo)
*			vtipo = '90% PENSION DEFINITIVA'
*		CASE vTipPla=8
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Estado="10") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = 'ADICIONAL - (DEVENGADOS)'
*		CASE vTipPla=9
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Estado="10") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = 'ADICIONAL DE VIUDEZ (DEVENGADOS)'
*		CASE vTipPla=10
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Estado="08") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = 'ADICIONAL DE VIUDEZ'
*		CASE vTipPla=11
*			SELECT Planilla
*			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (Estado="12") AND (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo)
*			vtipo = 'EXTRAORDINARIA DE REMUNERACIONES'
		CASE vTipPla=1
			SELECT Planilla
			INDEX ON Num_Pla+Periodo+Mes_Sue+Tip_Adn TO (vIdx) FOR (mes_Sue=cMes_Sue) AND (Periodo=cPeriodo) AND Con_Per='7'
			vtipo = 'PERSONAL 728 - RECURSOS DIRECTAMENTE RECAUDADOS'
   ENDCASE
ENDIF

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

DELE FOR EMPTY(Tot_Adn1) AND EMPTY(Tot_Adn2) AND EMPTY(Tot_Adn3) AND EMPTY(Tot_Adn4) AND EMPTY(Tot_Adn5) AND ESTADO#'02'

DO AgrNHoja

DO REPORTE WITH 2,'Boleta', ' Reporte de Boletas',2,.F.,.T.
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
ERASE (vDbf)

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
	IF nCont=3
		nCont=0
		nHoja=nHoja+1
	ENDIF
	SKIP -1
ENDSCAN
RETURN
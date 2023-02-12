*-----------------------------------------------------------------------
* CREAPLA.PRG
* Preoceso que crea un un formato de planilla para sus respectivos reportes
* M.R.A
* Opciones : 1 = Normal
*            2 = Vacaciones
*----------------------------------------------------------------------

PUBLIC vaumento,V26,vsuj_des,vsuj_des1,vaumentop,vsuj_desp,vdescuento,vdescuentp,cMes_Sue,mPlan,vIdx,OkCancel,cPeriodo
STORE 0 TO nAguin,vaumento,V26,V32,vsuj_des,vsuj_des1,vaumentop,vsuj_desp,vdescuento,vdescuentp
CLOS DATA
USE PERSONAL IN 1 ORDER TAG PERSONA3
USE ITEPER   IN 2 ORDER TAG ITEPER1
USE TOTAPOR  IN 3 ORDER TAG TOTAPOR1
USE Planilla IN 4 ORDER TAG Planilla1
USE Parmae   IN 5 ORDER TAG Parmae1		ALIAS Parma
SET RELATION TO Lib_Ele INTO iteper  	ADDITIVE
SET RELATION TO Lib_ele INTO totApor 	ADDITIVE

SELE Parma
SEEK "FECSIS"
VMES = LEFT(Parma.Descri,2)
vAno = SUBS(parma.descri,4,4)
DO Pregunta

IF allt(vmes)#cMes_Sue AND allt(vAno)#cPeriodo
	do standby with 'MES A REPORTAR ES DIFERENTE AL DEL SISTEMA'
	CLOS DATA
	OKCANCEL = 2
	RETURN
ENDIF

IF LASTKEY()=27 OR OkCancel=2
	CLOS DATA
	RETURN
ENDIF

SELE Planilla

IF Proc_Cerrado(cMes_Sue,cPeriodo)				&& Verifica el Estado de Proceso de las Planilla
	do standby with 'EL MES YA ESTA CERRADO NO PUEDE VOLVER A MODIFICAR'
	CLOS DATA
	OKCANCEL = 2
	RETURN
ENDIF

IF HayPla()
	IF YesNo('La Planilla ya est  Procesada, Desea Generarla Nuevamente')
		DELE FOR Mes_Sue=cMes_Sue AND Periodo=cPeriodo AND IIF(mPlan=2,Estado='02',Estado#'02')
	 ELSE 
		CLOS DATA
		RETURN
	ENDIF
ENDIF

SELE Personal
SET FILTER TO IIF(mPlan=1,.T.,Personal.Mes_Vac=cMes_Sue)		&& and ESTADO#"07"
SELE ItePer
SET FILTER TO Mes_Sue=cMes_Sue AND Periodo=cPeriodo AND IIF(mPlan=1,Personal.Mes_Vac#cMes_Sue,IIF(mPlan=2,Personal.Mes_Vac=cMes_Sue,.T.))

DO espera WITH 1,'Procesando, por favore espere...'
SELE Personal
SCAN
IF lIB_eLE='02891554'
	SET STEP ON
ENDIF
	STORE 0 TO vaumento,V26,vsuj_des,vaumentop,vsuj_desp,vdescuento,vdescuentp,vSinAfec,vSubsidios
	SELE ItePer
	IF EOF()
		LOOP
	ENDIF
	m.Lib_Ele = Lib_Ele
	IF m.lib_ele = "02781634"
*		set step on
	endif
	m.Periodo = Periodo
	m.Mes_Sue = Mes_Sue
	IF mPlan=1
		IF Personal.Mes_Vac#cMes_Sue
			m.Estado=Personal.Estado
		 ELSE
			m.Estado=""
		ENDIF
	 ELSE
		IF Personal.Mes_Vac=cMes_Sue
			m.Estado='02'
		 ELSE
			m.Estado=""
		ENDIF
	ENDIF
	m.Niv_Per = Niv_Per
	m.Tip_Adn = Tip_Adn
	nCont = 0
	FOR i = 1 to 2
		m.Tip_Adn = Tip_Adn
		STORE '' TO Cod_Adn1,Cod_Adn2,Cod_Adn3,Cod_Adn4,Cod_Adn5
		STORE 0 TO nCont,Tot_Adn1,Tot_Adn2,Tot_Adn3,Tot_Adn4,Tot_Adn5
		SELE Planilla
		APPEND BLANK
		GATHER MEMVAR
		SELE ItePer
		SCAN While Niv_Per=m.Niv_Per AND m.Tip_Adn = Tip_Adn AND Lib_Ele = m.Lib_Ele
			nCont = nCont + 1
			cCam1 = 'Planilla.Cod_Adn' + ALLTRIM(STR(nCont))
			cCam2 = 'Planilla.Tot_Adn' + ALLTRIM(STR(nCont))
			
			DO Valida_Inf
			
			Replace &cCam1 WITH Cod_Adn, &cCam2 WITH IIF((mPlan=1 AND Personal.Mes_Vac=cMes_Sue) OR EOF('ItePer'),0,IIF(porcent='N',Tot_Adn,vDescuento))
			IF nCont = 5
				STORE '' TO Cod_Adn1,Cod_Adn2,Cod_Adn3,Cod_Adn4,Cod_Adn5
				STORE 0 TO nCont,Tot_Adn1,Tot_Adn2,Tot_Adn3,Tot_Adn4,Tot_Adn5
			ENDIF
			SKIP 1
			IF nCont=0 AND m.Tip_Adn = Tip_Adn AND !EOF()
				SELE Planilla
				APPEND BLANK
				GATHER MEMVAR
				SELE ItePer
			ENDIF
			SKIP -1
		ENDSCAN
	ENDFOR
	sele personal
ENDSCAN

SELE Personal

CLOS DATA
DO espera WITH 2
RETURN

*PROCEDURE Valida_inf
*-------------------
	IF Tip_adn='+'
		IF cod_adn = '29' OR cod_adn = '56' OR cod_adn = '57' OR cod_adn = '82'	OR cod_adn = '97' OR cod_adn = '38'	  &&Aguinaldo
			nAguin = nAguin + tot_adn
		 ELSE
			nAguin = nAguin + 0
		ENDIF
		
		IF porcent='N'
			IF cod_adn <> '26' AND cod_adn <> '28' AND cod_adn <> '11' AND cod_adn <> '72'		&&AND cod_adn <> '32'
				vaumento   = vaumento   + tot_adn
			 ELSE
			 	*IF cod_adn <>'32'
			 		V26 = V26 + tot_adn
			 	* ELSE
			 	*	V32 = V32 + tot_adn   && 32 No lo Coje por Estar ya Afectado
			 	*ENDIF
			ENDIF
			IF cod_adn = '30' OR cod_adn = '31' OR cod_adn = '64' OR cod_adn = '72' 		&&cod_adn = '38' OR  OR cod_adn = '79' OR cod_adn = '80' OR cod_adn = '32'
				vSubsidios = vSubsidios + tot_adn
			ENDIF
			IF cod_adn = '65' OR cod_adn = '79' OR cod_adn = '80' OR cod_adn = '83' OR cod_adn = '91' OR cod_adn = '94' OR cod_adn = '95'
				vSinAfec = vSinAfec + tot_adn
			ENDIF
			IF cod_adn = '06' OR cod_adn = '85' OR cod_adn = '93' OR cod_adn = '43' OR cod_adn = '44'
				vsuj_des = vsuj_des + tot_adn
			ENDIF
			IF suj_des = 'N' AND !(cod_adn = '06' OR cod_adn = '85' OR cod_adn = '93' OR cod_adn = '43' OR cod_adn = '44')
				vsuj_des1 = vsuj_des1 + tot_adn
			ENDIF
		 ELSE
			vaumentop   = vaumentoP   + tot_adn
			IF suj_des = 'N'
				vsuj_desp = vsuj_desp + tot_adn
			ENDIF
		ENDIF
	 ELSE 
		IF porcent='N'
			vdescuento = vdescuento + tot_adn
		 ELSE
			vdescuentp   = vdescuentp + tot_adn
			vdescuento = vdescuento + ROUND((vaumento+vSubsidios+V26+v32-vsuj_des-vsuj_des1-vSinAfec)*vdescuentp/100,2)
*			vdescuento = vdescuento + ROUND((vaumento+V26+v32-vsuj_des-vsuj_des1-vSinAfec-vSubsidios)*vdescuentp/100,2)
			vdescuentp   = 0
		ENDIF
	ENDIF


PROCEDURE Valida_Inf
*-------------------
IF Tip_adn='+'
	IF porcent='N'
		IF cod_adn <> '28' AND cod_adn <> '11' AND cod_adn <> '30'  		&&cod_adn <> '26' AND
			vaumento   = vaumento   + tot_adn
		 ELSE
			V26 = V26 + tot_adn
		ENDIF
		IF cod_adn = '30' OR cod_adn = '31' OR cod_adn = '64' OR cod_adn = '72' 		&&cod_adn = '38' OR  OR cod_adn = '79' OR cod_adn = '80' OR cod_adn = '32'
			vSubsidios = vSubsidios + tot_adn
		ENDIF
		IF cod_adn = '65' OR cod_adn = '79' OR cod_adn = '80' OR cod_adn = '83' OR cod_adn = '91' OR cod_adn = '94' OR cod_adn = '95'
			vSinAfec = vSinAfec + tot_adn
		ENDIF
		IF suj_des = 'N'
			vsuj_des = vsuj_des + tot_adn
		ENDIF
	 ELSE
		vaumentop   = vaumentoP   + tot_adn
		IF suj_des = 'N'
			vsuj_desp = vsuj_desp + tot_adn
		ENDIF
	ENDIF
 ELSE
	IF porcent='N'
		vdescuento = vdescuento + tot_adn
	 ELSE
		vdescuentp = vdescuentp + tot_adn
*		vdescuento = ROUND((vaumento+V26-vsuj_des-vSinAfec)*vdescuentp/100,2)
		vdescuento = ROUND((vaumento+vSubsidios+V26-vsuj_des-vSinAfec)*vdescuentp/100,2)
*		vdescuento = ROUND((vaumento+vSubsidios+V26+v32-vsuj_des-vsuj_des1-vSinAfec)*vdescuentp/100,2)
		vdescuentp = 0
	ENDIF
ENDIF

PROCEDURE pregunta
*-----------------
PUBLIC mcodart,mCArt1
STORE SPACE(1) TO  vToLab,vTodMed
STORE SPACE(7) TO  mCodArt
STORE SPACE(11) TO  mCArt1
cMes_Sue = vMes
cPeriodo = vAno
OkCancel = 1
vToLab = 1
vToMed = 1
DEFINE WINDOW LIS FROM 3,15 TO 20,70 FLOAT DOUBLE TITLE 'Procesamiento de Reporte de Planilla' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1 TO mPlan
@ 01,10 GET mPlan PICTURE '@*RH Normal;Vacaciones' SIZE 1,15
@ 03,15 SAY "Mes " GET cMes_Sue Valid Val_para(cMes_Sue,'FECMES','S',0,70,7)
@ 03,23 GET cPeriodo

@ 14,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOWS LIS

RETURN

FUNCTION HayPla
*--------------
PRIVATE malias
mAlias = ALIAS()
SELE Planilla
SET FILTER TO Mes_Sue=cMes_Sue AND Periodo=cPeriodo AND IIF(mPlan=2,Estado='02',Estado#'02')
GO TOP
IF EOF()
	mRet = .F.
 ELSE
	mRet = .T.
ENDIF
SET FILTER TO

RETURN mRet


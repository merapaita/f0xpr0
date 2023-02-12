worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO Sai_lib
DO fox_ambi
vSistema='1'
CIA = 'sbp'
SET PATH TO ..\DATAPR
CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE maepar   IN 2  ORDER TAG maepar1  ALIAS presu
USE itepar   IN 3  ORDER TAG itepar1  ALIAS itepar
USE maepre   IN 4  ORDER TAG maepre1  ALIAS maepre
USE Calen    IN 5  ORDER TAG Calen2   ALIAS Calen
USE HojCon   IN 6  ORDER TAG Hojcon1  ALIAS Hoja
USE IteHc    IN 7  ORDER TAG Itehc1   ALIAS Itehc
USE traPAR   IN 8  ORDER TAG traPAR1  ALIAS TRAPAR
USE Itetra   IN 9  ORDER TAG Itetra1  ALIAS Itetra
USE CRESUP   IN 10 ORDER TAG CRESUP1  ALIAS CRESUP
USE Itecre   IN 11 ORDER TAG Itecre1  ALIAS Itecre
USE HOJMOD   IN 12 ORDER TAG hojmod1  ALIAS hojmod
USE repopre  IN 13 ALIAS REPO 

SELE REPO
vdbf = sys(3)+'.dbf'
Copy stru to (vdbf)
USE (VDBF) IN 13 ALIAS REPO EXCLU
SELE REPO
ZAP

store 0 to vTotal,vtipo
store space(2) to vperiodo,vcodfte,vcalend,VCODFUN,vUniges
store space(3) to vcodprg,vUnieje
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 
store space(8) to vcodpart
vperiodo = RIGHT(DTOC(DATE()),2)

*------ CONSOLIDADO PRESUPUESTO ANUAL -----------
DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
	TITLE ' °°   x Partidas Presupuestales     °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis_1
@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!'		VALID !EMPTY(vperiodo)
@  1,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@  2,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@  4,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID val_para(vUniGes,'UNIGES',' ',18,30)
@  5,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)

@  7,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+allT(vuniges)+allt(vunieje),' ',18,30),.T.) when vTotal=1

@  9,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
@ 10,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
@ 11,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
@ 12,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
@ 13,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
READ VALID val_read()
DEACTIVATE WINDOW lis_1
IF LASTKEY()= 27
	DO vista
	RETURN
ENDIF
SELE itepar
IF EOF()
	DO standby WITH vmens08
 ELSE
	ACTIVATE WINDOW Standby
	@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
	SELE REPO
	vind = SYS(3) + '.IDX'
	INDEX ON  LEFT(ESTFUN,5)+codcad+codpart TO (vind)
	SET INDEX TO (vind)
	
	IF vtotal = 1
		SELE itepar
		SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) AND ;
				IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)
	 ELSE
		SELE itepar
		SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)
	ENDIF
	GO TOP
	SCAN
		SCATTER MEMVAR
		SELECT REPO
		SEEK LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codpart
		vcod = 'FTE'+ALLTRIM(m.codfte)
		m.transf = m.tra001+m.tra003+m.tra004+m.tra005
		m.totcal = 0
		IF !FOUND()
			m.&vcod = m.valpart+m.cresup+m.transf
			APPEND BLANK
			GATHER MEMVAR
			m.&vcod=0
		 ELSE
			IF RLOCK()
				REPLACE &vcod WITH &vcod + m.valpart+m.cresup+m.traNSF
			ENDIF
			UNLOCK
			&vcod = 0
		ENDIF
		SELECT itepar
	ENDSCAN
	DEACTIVATE WINDOW Standby
	SELE REPO
	GO TOP
	IF EOF()
		DO standby WITH 'No existe Registros para procesar'
	 ELSE
		IF vtotal = 1
			if vtipo = 1
				DO reporte WITH 2,"LisEEP1",' Estado de Ejecuci¢n presupuestaria ',1,.F.,.T.
			 ELSE
				DO reporte WITH 2,"LisPrCA2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
			ENDIF
		 ELSE
			if vtipo = 1
				DO reporte WITH 2,"LISPRCG1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
			 ELSE
				DO reporte WITH 2,"LisPrCG2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
			ENDIF
		ENDIF
	ENDIF
ENDIF

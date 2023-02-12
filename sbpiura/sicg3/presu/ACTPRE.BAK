*--------------------------------------------------------------------------
* ActPre.Prg
* Actualizaci¢n del Presupuesto 
* Estado :
* Autor: Anal. sist. Marco Rodriguez abad
* FECHA: 31/12/99
*--------------------------------------------------------------------------
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE maepar   IN 2  ORDER TAG maepar1  ALIAS presu
USE itepar   IN 3  ORDER TAG itepar1  ALIAS itepar
USE maepre   IN 4  ORDER TAG maepre1  ALIAS maepre
USE itehc    IN 5   ORDER TAG itehc1   ALIAS itehc
USE Itetra   IN 6  ORDER TAG Itetra1  ALIAS Itetra
USE Itecre   IN 7  ORDER TAG Itecre1  ALIAS Itecre
USE repopre  IN 8  					  ALIAS REPO 
USE traPAR   IN 9  ORDER TAG traPAR1  ALIAS TRAPAR
USE CRESUP   IN 10 ORDER TAG CRESUP1  ALIAS CRESUP

DO Pantalla                  && Muestra pantalla inicial
clos data
RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
	TITLE ' °°  Actualizaci¢n Presupuestal °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis_1
store 0 to vTotal,vtipo
store space(2) to vperiodo,vcodfte,vcalend,VCODFUN
store space(3) to vcodprg
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 
vUniges = '01'
vUnieje = '001'

@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
@  1,2 say '  Calendario : ' GET vCalend    PICTURE '!!'	VALID VAL_PARA(vCalend  ,'FECMES',' ',18,25)

@  2,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@  3,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6

@  4,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID IIF(!EMPTY(vUniGes),val_para(vUniGes,'UNIGES',' ',18,30),.T.) 			
@  5,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID IIF(!EMPTY(vUniEje),val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30),.T.) 	

@  6,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+ALLT(vuniges)+ALLT(vunieje),' ',18,30),.T.) when vTotal=1
	
@  8,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 			when vTotal=2
@  9,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) 	when vTotal=2
@ 10,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
@ 11,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)				when vTotal=2
@ 12,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)				when vTotal=2

@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 	VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)

READ VALID val_read()

DEACTIVATE WINDOW lis_1

IF LASTKEY()= 27
	RETURN
ENDIF
SELE REPO
vdbf = sys(3)+'.dbf'
Copy stru to (vdbf)
USE (VDBF) IN 8  ALIAS REPO EXCLU
SELE REPO
ZAP
SELE itepar
IF EOF()
	DO standby WITH vmens08
ELSE
   	DEFINE WINDOW Xwait FROM 20,06 TO 22,78 COLOR SCHEME 05 
	ACTIVATE WINDOW Xwait
   	@ 0,10 SAY " Espere un Momento...Actualizando Presupuesto!" COLOR W+/BR*
    
	SELE REPO
	vind = SYS(3) + '.IDX'
	Yind = SYS(3) + '.IDX'
		
	INDEX ON codcad+codfte+codpart TO (vind)
	SET INDEX TO (vind)
		
	IF vtotal = 1
		SELE itepar
		SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.)
	ELSE
		SELE itepar
		SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.)
	ENDIF	
	GO TOP
	SCAN
		SCATTER MEMVAR
		m.cresup = GETCRE()
		m.transf = GETTRA()
		SELECT MAEPRE
		SEEK ITEPAR.PERIODO+ITEPAR.UNIGES+ITEPAR.UNIEJE+ITEPAR.CODCAD
		m.metas = allt(descri)
		SELECT REPO
		SEEK itepar.codcad+itepar.codfte+itepar.codpart
		vcod = 'FTE'+ALLTRIM(m.codfte)
		m.totcal = 0
		IF !FOUND()
		    m.&vcod = m.valpart+m.cresup+m.transf
			APPEND BLANK
			GATHER MEMVAR
			m.&vcod=0
		ELSE
			IF RLOCK()
				REPLACE &vcod WITH &vcod + m.valpart+m.cresup+m.transf
			ENDIF
			UNLOCK
			&vcod = 0
		ENDIF
		SELECT ItePar
	ENDSCAN
	SELE REPO
	zind = SYS(3) + '.IDX'
	INDEX ON codcad+CODFTE+CODPART TO (zind)

	** HOJAS DE AFECTACION
	SELECT IteHc
	SET FILTER TO IIF(!EMPTY(NUMHM),NUMMESHM<=ALLT(VCALEND),NUMMES<=allt(vcalend)) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
			IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) and estado # '99';
			AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.) 
	GO TOP
	SCAN
		sele maepre
		seek Vperiodo+itehc.uniges+itehc.unieje+itehc.codcad
		vkey = itehc.codcad+ITEHC.CODFTE+ITEHC.CODPART
		vkey1= uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet
		SELE REPO
		SEEK vkey
		IF FOUND()
			REPLACE TotAfe WITH TotAfe + IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
		ELSE
			append blank
			REPLACE CODPART WITH ITEHC.CODPART,;
					PERIODO WITH VPERIODO,;
					CODCAD  WITH ITEHC.CODCAD,;
					CODFTE  WITH ITEHC.CODFTE,;
					ESTFUN  WITH VKEY1,;
					TotAfe  WITH TotAfe + IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
		ENDIF
		IF ITEHC.NUMMESHM=ALLT(VCALEND)
			REPLACE totCAL WITH totCAL+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
		ELSE
			IF ITEHC.NUMMES = ALLTRIM(vcalend)
				REPLACE totCAL WITH totCAL+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
			ENDIF
		ENDIF	
		SELECT ITEHC
	ENDSCAN	
	SELE REPO
	vind = SYS(3) + '.IDX'
	INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (vind)
	SET INDEX TO (vind)
	SELE REPO
	GO TOP
	IF EOF()
		DO standby WITH 'No existe Registros para procesar'
	ELSE
		Do actualiza
	ENDIF
	RELEASE WINDOW Xwait
ENDIF
CLOS DATA
RETURN

PROCEDURE actualiza
*------------------
SELECT repo
SCAN
	vkey = PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART
	vtotcal = TotCal
	vmes    = 'M_'+vcalend
	SELECT itepar
	SEEK vkey
	IF FOUND()
		REPLACE &vmes with vtotcal
		DO chequeoa
	ENDIF
	SELECT repo
ENDSCAN

PROCEDURE chequeoa
*----------------
vtotmes = M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12
vValpart = valpart+cresup+tra001+tra003+tra004+tra005
IF vValpart<vtotmes
	vsaldo = vtotmes-vValpart
endif
REPLACE TRI_01 WITH M_01+M_02+M_03
REPLACE TRI_02 WITH M_04+M_05+M_06
REPLACE TRI_03 WITH M_07+M_08+M_09
REPLACE TRI_04 WITH M_10+M_11+M_12
RETURN .T.

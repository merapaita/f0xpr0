*--------------------------------------------------------------------------
* REGHC.Prg
* REGISTRA E-5 DE LOS OTROS PROGRAMAS
* Estado :
*   '00' Emitida   Este es el que se registra en la Orden de Compra
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
*--------------------------------------------------------------------------
CLOS DATA
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
USE iteparI  IN 11 ORDER TAG iteparI1  ALIAS iteparI
USE maeparI  IN 12 ORDER TAG maeparI1  ALIAS presuI

PUBLIC  VALCS, vCodPrg ,vCodSub , vProyec ,vCodact , vSubpry , vgun,VPART
PUBLIC XXM01,XXM02,XXM03,XXM04,XXM05,XXM06,XXM07,XXM08,XXM09,XXM10,XXM11,XXM12
PUBLIC FFM01,FFM02,FFM03,FFM04,FFM05,FFM06,FFM07,FFM08,FFM09,FFM10,FFM11,FFM12

DO Pantalla                  && Muestra pantalla inicial

RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
	TITLE ' °°  Saldo Presupuestal °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis_1
store 0 to vTotal,vtipo
store space(2) to vperiodo,vcodfte,vcalend,VCODFUN,vUniges
store space(3) to vcodprg,vUnieje
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 

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
SELE iteparI
IF EOF()
	DO standby WITH vmens08
ELSE
   	DEFINE WINDOW Xwait FROM 20,06 TO 22,78 COLOR SCHEME 05 
	ACTIVATE WINDOW Xwait
   	@ 0,10 SAY " Espere un Momento...Procesando el Listado E-5!" COLOR W+/BR*
    
	SELE REPO
	vind = SYS(3) + '.IDX'
	Yind = SYS(3) + '.IDX'
		
	INDEX ON codcad+codfte+codpart TO (vind)
	SET INDEX TO (vind)
		
	IF vtotal = 1
		SELE iteparI
		SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.)
	ELSE
		SELE iteparI
		SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.)
	ENDIF	
	GO TOP
	SCAN
		SCATTER MEMVAR
		m.cresup = GETCRE()
		m.transf = GETTRA()
		SELECT REPO
		SEEK iteparI.codcad+iteparI.codfte+iteparI.codpart
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
		SELECT IteParI
	ENDSCAN
	SELE REPO
	zind = SYS(3) + '.IDX'
	INDEX ON LEFT(ESTFUN,30)+CODFTE+CODPART TO (zind)

	** HOJAS DE AFECTACION
	SELECT IteHc
	SET FILTER TO NUMMES<=allt(vcalend) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
			IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) and estado # '99';
			AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
	GO TOP
	SCAN
		sele maepre
		seek Vperiodo+itehc.uniges+itehc.unieje+itehc.codcad

		vkey = uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE+ITEHC.CODPART
		vkey1= uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE
		SELE REPO
		SEEK vkey
		IF FOUND()
			REPLACE TotAfe WITH TotAfe + IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
		ELSE
			GO TOP	
			LOCATE FOR LEFT(ESTFUN,30)+CODFTE = vkey1
			IF FOUND()
				append blank
				REPLACE CODPART WITH ITEHC.CODPART,;
						PERIODO WITH VPERIODO,;
						CODCAD  WITH ITEHC.CODCAD,;
						CODFTE  WITH ITEHC.CODFTE,;
						ESTFUN  WITH LEFT(VKEY,30),;
						TotAfe WITH TotAfe + IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)

			ENDIF			
		ENDIF
		IF ITEHC.NUMMES = ALLTRIM(vcalend)
			REPLACE totCAL WITH totCAL+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
		ENDIF	
		SELECT ITEHC
	ENDSCAN	

*	IF SISTEM = '1'
*		** HOJAS DE AFECTACION emergencia
*		SELECT 5
*		USE 
*		USE \EMER97\DATA\itehc    IN 5   ORDER TAG itehc1   ALIAS itehce
*		SELE 4
*		USE
*		USE \EMER97\DATA\maepre   IN 4  ORDER TAG maepre1  ALIAS maepree
*		SELECT IteHce
*		SET FILTER TO NUMMES<=allt(vcalend) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
*			IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) and estado # '99';
*			AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
*		GO TOP
*		SCAN
*			sele maepree
*			seek Vperiodo+IteHce.uniges+IteHce.unieje+IteHce.codcad
*			vkey = uniges+unieje+codfun+codprg+codspr+actpry+IteHce.codcom+'00001'+IteHce.CODFTE+IteHce.CODPART
*			vkey1= uniges+unieje+codfun+codprg+codspr+actpry+IteHce.codcom+'00001'+IteHce.CODFTE
*			SELE REPO
*			SEEK vkey
*			IF FOUND()
*				REPLACE TotAfe WITH TotAfe + IIF(IteHce.TIPOPE='-',IteHce.VALPART*-1,IteHce.VALPART)
*			ELSE
*				GO TOP	
*				LOCATE FOR LEFT(ESTFUN,30)+CODFTE = vkey1
*				IF FOUND()
*					append blank
*					REPLACE CODPART WITH IteHce.CODPART,;
*						PERIODO WITH VPERIODO,;
*						CODCAD  WITH IteHce.CODCAD,;
*						CODFTE  WITH IteHce.CODFTE,;
*						ESTFUN  WITH LEFT(VKEY,30),;
*						TotAfe WITH TotAfe + IIF(IteHce.TIPOPE='-',IteHce.VALPART*-1,IteHce.VALPART)
*				ENDIF			
*			ENDIF
*			IF IteHce.NUMMES = ALLTRIM(vcalend)
*				REPLACE totCAL WITH totCAL+IIF(IteHce.TIPOPE='-',IteHce.VALPART*-1,IteHce.VALPART)
*			ENDIF	
*			SELECT IteHce
*		ENDSCAN
*		SELECT 5
*		USE 
*		USE itehc    IN 5   ORDER TAG itehc1   ALIAS itehc
*		SELE 4
*		USE
*		USE maepre   IN 4  ORDER TAG maepre1  ALIAS maepre
*	ENDIF
	SELE REPO
	GO TOP	
	* RESTRUCTURACION DE CADENAS
	SCAN
		VKEY = PERIODO+LEFT(ESTFUN,30)+CODFTE
		SELE ITEPARI
		SET ORDER TO ITEPARI4
		SEEK VKEY
		IF FOUND()
			REPLACE REPO.CODCAD WITH ITEPARI.CODCAD
		ENDIF
		SELE REPO
	ENDSCAN		
	SELE REPO
	vind = SYS(3) + '.IDX'
	INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (vind)
	SET INDEX TO (vind)
	RELEASE WINDOW Xwait
	SELE REPO
	GO TOP
	IF EOF()
		DO standby WITH 'No existe Registros para procesar'
	ELSE
		IF vtotal = 1
			if vtipo = 1
				DO reporte WITH 2,"LisE51",' Consolidado de la Ejecucion ',1,.F.,.T.
			ELSE
				DO reporte WITH 2,"LisE52",' Consolidado de la Ejecucion ',1,.F.,.T.
			ENDIF
		ELSE
			if vtipo = 1
				DO reporte WITH 2,"LisE5G1",' Consolidado de la Ejecucion ',1,.F.,.T.
			ELSE
				DO reporte WITH 2,"LisE5G2",' Consolidado de la Ejecucion ',1,.F.,.T.
			ENDIF	
		ENDIF	
	ENDIF
ENDIF

CLOS DATA
RETURN

FUNCTION sumpre2
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
ENDCASE	
SUM FTE00+FTE01+FTE09 TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumcal
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
ENDCASE	
SUM totcal TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION SUMafe
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
ENDCASE	
SUM totafe TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumsal
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
ENDCASE	
SUM (FTE00+FTE01+FTE09)-TOTAFE TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

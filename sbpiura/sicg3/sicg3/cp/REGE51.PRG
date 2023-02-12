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
USE itehc    IN 5   ORDER TAG itehc1  ALIAS itehc
USE Itetra   IN 6  ORDER TAG Itetra1  ALIAS Itetra
USE Itecre   IN 7  ORDER TAG Itecre1  ALIAS Itecre
USE repopre  IN 8  					  ALIAS REPO 
USE traPAR   IN 9  ORDER TAG traPAR1  ALIAS TRAPAR
USE CRESUP   IN 10 ORDER TAG CRESUP1  ALIAS CRESUP
USE HojCon   IN 11 ORDER TAG HojCon1  ALIAS Hoja
USE CatAsi   IN 12 ORDER TAG CatAsi4  ALIAS CatAsi

PUBLIC VALCS, vCodPrg ,vCodSub , vProyec ,vCodact , vSubpry , vgun,VPART
PUBLIC nPre,nDelMes,nAcum,nPag,nPor,nSaldo,nPre1,nDelMes1,nAcum1,nPag1,nPor1,nSaldo1
nPre1 = 0
nDelMes1 = 0
nAcum1  = 0
nPag1  = 0
nPor1  = 0
nSaldo1 = 0

SET FIXED OFF
DO Pantalla                  && Muestra pantalla inicial
SET FIXED ON

RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
DEFINE WINDOW lis_1 FROM 2,10 TO 22,70 DOUBLE ;
	TITLE ' °°  Saldo Presupuestal °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis_1
STORE 0 TO vTotal,vtipo
vPag = 1
STORE SPACE(2) TO vperiodo,vcodfte,vcalend,VCODFUN,vUniges
STORE SPACE(3) TO vcodprg,vUnieje
STORE SPACE(4) TO vcodcad,vcodSpr
STORE SPACE(5) TO vcodcom,vcodmet
STORE SPACE(6) TO vcodpart,vactpry 

@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
@  1,2 say '  Calendario : ' GET vCalend    PICTURE '!!'	VALID VAL_PARA(vCalend  ,'FECMES',' ',18,25)

@  2,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@  3,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6

@  4,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    VALID val_para(vUniGes,'UNIGES',' ',18,30)
@  5,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 	VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)

@  6,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo,' ',18,30),.T.) when vTotal=1
	
@  8,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 			when vTotal=2
@  9,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) 	when vTotal=2
@ 10,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
@ 11,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)				when vTotal=2
@ 12,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)				when vTotal=2

@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 	VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)

*@ 16,2 SAY 'Inc. Pagado? : ' GET vPag   	FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@ 16,2 SAY 'Reporte? : ' GET vPag   	FUNCTION '^ Normal;Con Comprobante;Con Porcentaje'

READ VALID val_read()

DEACTIVATE WINDOW lis_1
RELEASE WINDOW lis_1

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
   	@ 0,10 SAY " Espere un Momento...Procesando el Listado E-5!" COLOR W+/BR*
    
*    SET STEP ON
    
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
				
		m.Metas = GetCad()
		m.cresup = GETCRE()
		m.transf = GETTRA()
		=GETEje()
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
	INDEX ON LEFT(ESTFUN,30)+CODFTE+CODPART TO (zind)
	** HOJAS DE AFECTACION
	SELECT IteHc
	
	SET FILTER TO NUMMES<=allt(vcalend) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
			IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) and estado # '99';
			AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
	GO TOP
	SCAN
		if numhc='0229'
*			SET STEP ON
		ENDIF
		sele maepre
		seek Vperiodo+itehc.uniges+itehc.unieje+itehc.codcad
		
		vkey = uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE+ITEHC.CODPART
		vkey1= uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE
		SELE REPO
		
*		IF IteHC.nUMhC='0067'
*			SUSP
*		ENDIF
		
*IF IteHC.Estado#'92'
*	REPLACE TotAfe WITH TotAfe + IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
* ELSE
*	IF ITEHC.NUMMES = ALLTRIM(vcalend)
*		REPLACE TotAfe WITH TotAfe + IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
*	ENDIF
*ENDIF
		SEEK vkey
		IF FOUND()
			IF IteHC.Estado#'92'
				REPLACE TotAfe WITH TotAfe + IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
				REPLACE TotPag WITH TotPag + IIF(!EMPTY(Hoja.NumCP),IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART),0)
				*marco
			 ELSE
				IF VAL(IteHc.NumMesHM) <= VAL(ALLTRIM(vcalend))
					REPLACE TotAfe WITH TotAfe + IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
					REPLACE TotPag WITH TotPag + IIF(!EMPTY(Hoja.NumCP),IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART),0)
					* marco
				ENDIF
			ENDIF
*			REPLACE TotAfe WITH TotAfe + IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
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
						TotAfe WITH TotAfe + IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART),;
						TotPag WITH TotPag + IIF(!EMPTY(Hoja.NumCP),IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART),0)
						* marco
			ENDIF			
		ENDIF
		IF IteHC.Estado#'92'
			IF ITEHC.NUMMES = ALLTRIM(vcalend)
				REPLACE totCAL WITH totCAL+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
			ENDIF
		 ELSE
			IF IteHc.NumMesHM = ALLTRIM(vcalend)
				REPLACE totCAL WITH totCAL+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
			ENDIF
		ENDIF
*		IF ITEHC.NUMMES = ALLTRIM(vcalend)
*			REPLACE totCAL WITH totCAL+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
*		ENDIF
		SELECT ITEHC
	ENDSCAN	
	SELE REPO
	GO TOP	
	* RESTRUCTURACION DE CADENAS
	SCAN
		VKEY = PERIODO+LEFT(ESTFUN,30)+CODFTE
		SELE ITEPAR
		SET ORDER TO ITEPAR4
		SEEK VKEY
		IF FOUND()
			REPLACE REPO.CODCAD WITH ITEPAR.CODCAD
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
		DELE FOR EMPTY(fte00) AND EMPTY(fte01) AND EMPTY(fte09) AND EMPTY(fte13) AND EMPTY(fte19) AND EMPTY(totcal) AND EMPTY(totafe)
		IF vtotal = 1
			if vtipo = 1
				DO CASE
					CASE vPag = 1
						DO reporte WITH 2,"LisE51",' Consolidado de la Ejecucion ',1,.F.,.T.
						INDEX ON codfte+codpart TO (vind)
						DO reporte WITH 2,"LisE5C",' Consolidado de la Ejecucion ',1,.F.,.T.
					CASE vPag = 2
						DO reporte WITH 2,"LisE51P",' Consolidado de la Ejecucion ',1,.F.,.T.
						INDEX ON codfte+codpart TO (vind)
						DO reporte WITH 2,"LisE5CP",' Consolidado de la Ejecucion ',1,.F.,.T.
					CASE vPag = 3
						DO reporte WITH 2,"LisE51Q",' Consolidado de la Ejecucion ',1,.F.,.T.
						INDEX ON codfte+codpart TO (vind)
						DO reporte WITH 2,"LisE5CQ",' Consolidado de la Ejecucion ',1,.F.,.T.
				ENDCASE
			ELSE
				DO reporte WITH 2,"LisE52",' Consolidado de la Ejecucion ',1,.F.,.T.
				INDEX ON codfte+codpart TO (vind)
    			DO reporte WITH 2,"LisE5C2",' Consolidado de la Ejecucion ',1,.F.,.T.
			ENDIF
		ELSE
			if vtipo = 1
				DO reporte WITH 2,"LisE5G1",' Consolidado de la Ejecucion ',1,.F.,.T.
 				DO reporte WITH 2,"LisE5C", ' Consolidado de la Ejecucion ',1,.F.,.T.				
			ELSE
				DO reporte WITH 2,"LisE5G2",' Consolidado de la Ejecucion ',1,.F.,.T.
				DO reporte WITH 2,"LisE5C",' Consolidado de la Ejecucion ',1,.F.,.T.
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
	CASE vNivel = '3'
		vFiltro = 'codfte'
ENDCASE	
SUM FTE00+FTE01+FTE09+Fte13+Fte19 TO suma FOR &vFiltro= vCalen
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
	CASE vNivel = '3'
		vFiltro = 'codfte'
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
	CASE vNivel = '3'
		vFiltro = 'codfte'
ENDCASE	
SUM totafe TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION SUMPag
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'codfte'
ENDCASE	
SUM totPag TO suma FOR &vFiltro = vCalen 		&&AND EMPTY(HOJA.NUMCP)
GO vrec
RETURN suma

FUNCTION SUMPor
*-----------------
PARAMETER vCalen,vnivel
PRIVATE xTAfe,xTFte,xRes
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'codfte'
ENDCASE	
SUM TOtAFE TO xTAfe FOR &vFiltro = vCalen
SUM FTE00+FTE01+FTE09+FTE13+Fte19 TO xTFte FOR &vFiltro = vCalen
xCan = _tally
xRes = (xTAfe / xTFte) * 100
GO vrec
RETURN xRes

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
	CASE vNivel = '3'
		vFiltro = 'codfte'
ENDCASE	
SUM (FTE00+FTE01+FTE09+Fte13+Fte19)-TOTAFE TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION GetCad
*--------------
PRIVATE cAlias
cAlias = ALIAS()
SELE MaePre
SEEK ItePar.Periodo+ItePar.UniGes+ItePar.UniEje+ItePar.CodCad
IF FOUND()
	mRet = DesCri
ENDIF
SELE (cAlias)
RETURN mRet

FUNCTION nPart
*-------------
PARAMETERS cPartida
PUBLIC nReg
nReg = RECNO()
nPre = 0
nPag = 0
nPor = 0
nDelMes = 0
nAcum  = 0
nSaldo = 0
SCAN WHILE CodPart=cPartida
	nPre = nPre + Fte00+Fte01+Fte09+Fte13+Fte19
	nDelMes = nDelMes + TotCal
	nAcum  = nAcum + TotAfe
	nPag   = nPag  + TotPag
	nSaldo = nSaldo + (Fte00+Fte01+Fte09+Fte13+Fte19 - TotAfe)
ENDSCAN
nPor   = (nPag/npre) * 100
nPre1 = nPre1 + nPre
nDelMes1 = nDelMes1 + nDelMes
nAcum1 = nAcum1 + nAcum
nPag1 = nPag1 + nPag
nPor1 = (nPag1/npre1) * 100
*nPor1 = nPor1 + nPor
nSaldo1 = nSaldo1 + nSaldo
GO nReg
RETURN nPre

FUNCTION PonCero
*---------------
nPre1 = 0
nDelMes1 = 0
nAcum1  = 0
nPag1  = 0
nPor1  = 0
nSaldo1 = 0

** ---------------------------------------------------
** SALPRE.PRG
** Reporte Ejecucion ESTADOS
** ---------------------------------------------------
* Abrir Archivos de trabajo
CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE maepar   IN 2  ORDER TAG maepar1  ALIAS presu
USE itepar   IN 3  ORDER TAG itepar1  ALIAS itepar
USE maepre   IN 4  ORDER TAG maepre1  ALIAS maepre
USE Calen    IN 5  ORDER TAG Calen2   ALIAS Calen
USE repopre  IN 6  					  ALIAS REPO 
USE traPAR   IN 7  ORDER TAG traPAR1  ALIAS TRAPAR
USE Itetra   IN 8  ORDER TAG Itetra1  ALIAS Itetra
USE CRESUP   IN 9  ORDER TAG CRESUP1  ALIAS CRESUP
USE Itecre   IN 10 ORDER TAG Itecre1  ALIAS Itecre
USE IteHc    IN 11 ORDER TAG Itehc1   ALIAS Itehc


PUBLIC vCodSub,vCodAct,vProyec,vSubpry,vCalend 
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Presupuesto'
vmens02 = ' Presupuesto : REVISION '
vmens04 = 'Dicho Presupuesto no fue encontrado'
vmens05 = 'No existe Presupuesto anterior'
vmens06 = 'No existe Presupuesto siguiente'
vmens07 = '¨ Desea Anular ‚ste Presupuesto ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Presupuesto ha sido anulado'
vmens10 = 'El Presupuesto ya est  Atendido'
vmens11 = 'El Presupuesto ha sido devuelto'

TOT = 0
TOT1 = 0
DO Inicia
DO salida
CLOSE DATA
RETURN


PROCEDURE Inicia
*--------------

SELE REPO
vdbf = sys(3)+'.dbf'
Copy stru to (vdbf)
USE (VDBF) IN 6  ALIAS REPO EXCLU
SELE REPO
ZAP
PRIVATE VTEXP
SELECT presu
ord   = ORDER()
vteXp = RECNO()
store 0 to vTotal,vtipo
store space(2) to vperiodo,vcodfte,vcalend,VCODFUN,vUniges
store space(3) to vcodprg,vUniEje
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 
	*------ SALDO PRESUPUESTAL -----------

	DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
		TITLE ' °°  Saldo Presupuestal °° ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1
	@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
    @  1,2 say '  Calendario : ' GET vCalend    PICTURE '!!'	VALID VAL_PARA(vCalend  ,'FECMES',' ',18,25)

	@  2,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
	@  3,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
	
	@  4,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo,' ',18,30),.T.) when vTotal=1
	
	@  6,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID IIF(!EMPTY(vUniGes),val_para(vUniGes,'UNIGES',' ',18,30),.T.) 		when vTotal=2
	@  7,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID IIF(!EMPTY(vUniEje),val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30),.T.) when vTotal=2
	@  8,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
	@  9,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
	@ 10,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
	@ 11,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
	@ 12,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2

	@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 	VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)

	READ VALID val_read()

	DEACTIVATE WINDOW lis_1

	IF LASTKEY()= 27
		RETURN
	ENDIF
	SELE itepar
	IF EOF()
		DO standby WITH vmens08
	ELSE
	   	DEFINE WINDOW Xwait FROM 20,06 TO 22,78 COLOR SCHEME 05 
		ACTIVATE WINDOW Xwait
   		@ 0,10 SAY " Espere un Momento...Procesando Saldos Presupuestales !" COLOR W+/BR*

		SELE REPO
		vind = SYS(3) + '.IDX'
		
		INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (vind)
		SET INDEX TO (vind)
		
		IF vtotal = 1
			SELE Calen
			SET FILTER TO periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) ;
				AND NUMMES<=ALLTRIM(vcalend) AND IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)
			
			SELE itepar
			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) ;
				AND	IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)
		ELSE
			SELE Calen
			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) ;
				AND NUMMES<=ALLTRIM(vcalend) 
				
			SELE itepar
			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.)
		ENDIF	
		GO TOP
		SCAN
			SCATTER MEMVAR
			m.cresup = GETCRE()
			m.transf = GETTRA()
			SELECT REPO
			SEEK LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
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

			SELECT Calen
			vFilt = itepar.periodo+LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
			SEEK vfilt
			
			IF found()
				vTotCal = 0
				vkey1 = periodo+LEFT(ESTFUN,5)+codcad+codfte+codpart
				vKey2 = periodo+LEFT(ESTFUN,5)+codcad+codfte+codpart
				DO WHILE vkey1 = vkey2 and !eof()
					vTotCal = vTotCal + ValPart + ampliar
					SKIP
					vKey2 = periodo+LEFT(ESTFUN,5)+codcad+codfte+codpart
				ENDDO
				SELECT REPO
				REPLACE TotCal WITH TotCal + vTotCal
			ENDIF
			SELECT ItePar
		ENDSCAN
		SELE REPO
		zind = SYS(3) + '.IDX'
		INDEX ON LEFT(ESTFUN,30)+CODFTE+CODPART TO (zind)
		
		** HOJAS DE AFECTACION CTAR
		
		SELECT IteHc
		SET FILTER TO NUMMES<=allt(vcalend) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
			IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) and estado # '99' ;
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
			SELECT ITEHC
		ENDSCAN	
		
		** HOJAS DE AFECTACION EMERGENCIA
*		SELE 4
*LIST
*		USE
*		USE \EMER97\DATA\maepre   IN 4  ORDER TAG maepre1  ALIAS maepree
*		SELE 11
*		USE
*		USE \EMER97\DATA\IteHc    IN 11 ORDER TAG Itehc1   ALIAS Itehce
*		SELECT IteHce
*		SET FILTER TO NUMMES<=allt(vcalend) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
*			IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) and estado # '99';
*			AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
*		GO TOP
*		SCAN
*			sele maepree
*			seek Vperiodo+itehce.uniges+itehce.unieje+itehce.codcad
*
*			vkey = uniges+unieje+codfun+codprg+codspr+actpry+itehce.codcom+'00001'+itehce.CODFTE+itehce.CODPART
*			vkey1= uniges+unieje+codfun+codprg+codspr+actpry+itehce.codcom+'00001'+itehce.CODFTE
*			SELE REPO
*			SEEK vkey
*			IF FOUND()
*				REPLACE TotAfe WITH TotAfe + IIF(itehce.TIPOPE='-',itehce.VALPART*-1,itehce.VALPART)
*			ELSE
*				GO TOP	
*				LOCATE FOR LEFT(ESTFUN,30)+CODFTE = vkey1
*				IF FOUND()
*					append blank
*					REPLACE CODPART WITH itehce.CODPART,;
*							PERIODO WITH VPERIODO,;
*							CODCAD  WITH itehce.CODCAD,;
*							CODFTE  WITH itehce.CODFTE,;
*							ESTFUN  WITH LEFT(VKEY,30),;
*							TotAfe WITH TotAfe + IIF(itehce.TIPOPE='-',itehce.VALPART*-1,itehce.VALPART)
*
*				ENDIF			
*			ENDIF
*			SELECT itehce
*		ENDSCAN	

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
		GO TOP
	   	RELEASE WINDOW Xwait
		IF EOF()
			DO standby WITH 'No existe Registros para procesar'
		ELSE
			IF vtotal = 1
				if vtipo = 1
					DO reporte WITH 2,"SalPre1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ELSE
					DO reporte WITH 2,"SalPre2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ENDIF
			ELSE
				if vtipo = 1
					DO reporte WITH 2,"SalPreG1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ELSE
					DO reporte WITH 2,"SalPreG2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ENDIF	
			ENDIF	
		ENDIF
	ENDIF
return


PROCEDURE Salida
*---------------
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN

FUNCTION sumpre2
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
	CASE vNivel = '4'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
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
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
	CASE vNivel = '4'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
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
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
	CASE vNivel = '4'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
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
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
	CASE vNivel = '4'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE	

SUM (FTE00+FTE01+FTE09)-TOTCAL TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION summef
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM FTE00+FTE01+FTE09 TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM FTE00+FTE01+FTE09 TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma


FUNCTION summef00
*---------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM FTE00 TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM FTE00 TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

FUNCTION summef01
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM FTE01 TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM FTE01 TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

FUNCTION summef09
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM FTE09 TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM FTE09 TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma
FUNCTION sumcal1
*-----------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM totcal TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM totcal TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	

GO vrecno
RETURN suma

FUNCTION SUMafe1
*-----------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM totafe TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM totafe TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

FUNCTION sumsal1
*-----------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM (FTE00+FTE01+FTE09)-TOTCAL TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM (FTE00+FTE01+FTE09)-TOTCAL TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

* ---------------------------------------------------
** SALPRE.PRG
** Reporte Ejecucion ESTADOS
** UDEP
** Autor : Anal. SEGUNDO CORREa
** ---------------------------------------------------
* Abrir Archivos de trabajo
CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE maepar   IN 2  ORDER TAG maepar1  ALIAS presu
USE itepar   IN 3  ORDER TAG itepar1  ALIAS itepar
USE maepre   IN 4  ORDER TAG maepre1  ALIAS maepre
USE IteHc    IN 5  ORDER TAG Itehc1   ALIAS Itehc
USE Itetra   IN 10  ORDER TAG Itetra1  ALIAS Itetra
USE Itecre   IN 11  ORDER TAG Itecre1  ALIAS Itecre
USE cresup   IN 12  ORDER TAG cresup1  ALIAS cresup
USE Trapar   IN 13  ORDER TAG trapar1  ALIAS trapar


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

PRIVATE VTEXP
SELECT presu
ord   = ORDER()
vteXp = RECNO()
store 0 to vTotal,vtipo,vproact
vUniges = '01'
vUnieje = '001'
store space(2) to vperiodo,vcodfte,vcalend,VCODFUN
store space(3) to vcodprg
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 
	*------ SALDO PRESUPUESTAL -----------

	DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
		TITLE ' °°  Saldo Presupuestal °° ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1
	@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
    @  1,2 say '   Al Mes de : ' GET vCalend    PICTURE '!!'	VALID VAL_PARA(vCalend  ,'FECMES',' ',18,25)
	@  3,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
	@  4,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
	@  5,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID IIF(!EMPTY(vUniGes),val_para(vUniGes,'UNIGES',' ',18,30),.T.) 		
	@  6,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID IIF(!EMPTY(vUniEje),val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30),.T.)
	@  7,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo,' ',18,30),.T.) when vTotal=1
    @  8,2 SAY '   Proy/Act. : ' GET vproact    FUNCTION '*RNH \<Si;\<No;\<Todo' SIZE 1,7,6
	@  9,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
	@ 10,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
	@ 11,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
	@ 12,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
	@ 13,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
	@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 		VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)
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

		*- Abriendo Archivos
		USE repcal1  IN 6              &&  ALIAS rep1   
		SELE 6
		vInd = SYS(3) + '.DBF'
		COPY STRU TO (vInd)
		use (vind) in 6 alias REPO EXCLUSIVE
		SELE REPO
		zap
		vind = SYS(3) + '.IDX'
		INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (vind)
		SET INDEX TO (vind)
		IF vtotal = 1
			SELE itepar
			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
				IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)
		ELSE
			SELE itepar
			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) and IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)AND IIF(!EMPTY(ALLT(vcodfte)),codfte=allt(vcodfte),.T.)
		ENDIF	
		GO TOP
		SCAN
			SCATTER MEMVAR
			m.cresup  = getcre()
			m.transf  = gettra()
			SELECT REPO
			SEEK  LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
			STORE 0 TO m.M_01,m.M_02,m.M_03,m.M_04,m.M_05,m.M_06
			STORE 0 TO m.M_07,m.M_08,m.M_09,m.M_10,m.M_11,m.M_12
			IF !FOUND()
				m.ValPres = m.valpart
				APPEND BLANK
				GATHER MEMVAR
			ELSE
				IF RLOCK()
					REPLACE ValPres WITH m.valpart
					REPLACE cresup  WITH m.cresup
					REPLACE transf  WITH m.transf
				ENDIF
				UNLOCK
			ENDIF
			SELECT ItePar
		ENDSCAN
		SELE REPO
		zind = SYS(3) + '.IDX'
		INDEX ON LEFT(ESTFUN,30)+CODFTE+CODPART TO (zind)

		** HOJAS DE AFECTACION CTAR
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
			vmes = 'M_'+ALLTRIM(itehc.nummes)
			SELE REPO
			SEEK vkey
			IF FOUND()
				REPLACE &vmes WITH &vmes+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
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
							&vmes   WITH IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
				ENDIF			
			ENDIF
			SELECT ITEHC
		ENDSCAN		

		** HOJAS DE AFECTACION EMERGENCIA
*		SELECT 4
*		USE
*		USE \EMER97\DATA\maepre  IN 4  ORDER TAG maepre1  ALIAS maepre
*		SELECT 5
*		USE
*		USE \EMER97\DATA\IteHc    IN 5  ORDER TAG Itehc1   ALIAS Itehc
*		SELECT itehce
*		SET FILTER TO NUMMES<=allt(vcalend) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
*			IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) and estado # '99';
*			AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
*		GO TOP
*		SCAN
*			sele maepre
*			seek Vperiodo+itehce.uniges+itehce.unieje+itehce.codcad
*			vkey = uniges+unieje+codfun+codprg+codspr+actpry+itehce.codcom+'00001'+itehce.CODFTE+itehce.CODPART
*			vkey1= uniges+unieje+codfun+codprg+codspr+actpry+itehce.codcom+'00001'+itehce.CODFTE
*			vmes = 'M_'+ALLTRIM(itehce.nummes)
*			SELE REPO
*			SEEK vkey
*			IF FOUND()
*				REPLACE &vmes WITH &vmes+IIF(itehce.TIPOPE='-',itehce.VALPART*-1,itehce.VALPART)
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
*							&vmes   WITH IIF(itehce.TIPOPE='-',itehce.VALPART*-1,itehce.VALPART)
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

	   	RELEASE WINDOW Xwait
		IF EOF()
			DO standby WITH 'No existe Registros para procesar'
		ELSE
			DO CASE 
				CASE vproact = 1
					SET FILTER TO SUBST(ESTFUN,21,1) = '2'
				CASE vproact = 2	
					SET FILTER TO SUBST(ESTFUN,21,1) = '1'
			ENDCASE		
			IF vtotal = 1
				if vtipo = 1
					DO reporte WITH 2,"SalPrem1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ELSE
					DO reporte WITH 2,"SalPrem2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ENDIF
			ELSE
				if vtipo = 1
					DO reporte WITH 2,"SalPrmG1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ELSE
					DO reporte WITH 2,"SalPrmG2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ENDIF	
			ENDIF	
			if vproact<3
				set filter to
			endif
			vInd = SYS(3) + '.IDX'
			INDEX ON LEFT(ESTFUN,5)+CODFTE+codpart TO (vind)
			DO reporte WITH 2,"SalPREMR",' Consolidado de GENERAL ',1,.F.,.T.
			USE 
			ERASE (vind)
		ENDIF
	ENDIF
return


PROCEDURE Salida
*---------------
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN

FUNCTION sumpre1
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
SUM valpres+cresup+transf TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumMES
*-----------------
PARAMETER vCalen,PART,vnivel
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
SUM &part TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

PROCEDURE SUMAR
*--------------
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
SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12  TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION salprg
*---------------
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
SUM valpres+cresup+transf-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12) TO sumA FOR &vFiltro = vCalen
GO vrec
RETURN sumA

FUNCTION summef
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM valpres+cresup+transf TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM valpres+cresup+transf TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

FUNCTION summeS1
*--------------
PARAMETER vFiltro,PART
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrec = RECNO()
IF VTIPO <= 25
	SUM &part TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM &part TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrec
RETURN suma

FUNCTION suMAR1
*--------------
PARAMETER vFiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrec = RECNO()
DO CASE
	CASE VTIPO <= 25
		SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12  TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
	CASE VTIPO > 25 AND VTIPO < 28
		SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12  TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
	CASE VTIPO > 28 AND VTIPO < 30
		SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12  TO suma FOR LEFT(ESTFUN,25)+codfte+LEFT(codpart,2) = vFiltro
	CASE VTIPO > 30
		SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12  TO suma FOR LEFT(ESTFUN,25)+codfte+codpart = vFiltro
ENDCASE
GO vrec
RETURN suma

FUNCTION salprg1
*---------------
PARAMETER vFiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrec = RECNO()
DO CASE
	CASE VTIPO <= 25
		SUM valpres+cresup+transf-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12)  TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
	CASE VTIPO > 25 AND VTIPO < 28
		SUM valpres+cresup+transf-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12)  TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
	CASE VTIPO > 28 AND VTIPO < 30
		SUM valpres+cresup+transf-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12)  TO suma FOR LEFT(ESTFUN,25)+codfte+LEFT(codpart,2) = vFiltro
	CASE VTIPO > 30
		SUM valpres+cresup+transf-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12)  TO suma FOR LEFT(ESTFUN,25)+codfte+codpart = vFiltro

ENDCASE
GO vrec
RETURN suma

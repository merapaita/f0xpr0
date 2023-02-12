* RegCal.Prg
* Registra Calendarios
*--------------------------------------------------------------------------
*- Abriendo Archivos
CLOS DATA
USE parmae   IN 1   ORDER TAG parmae1  ALIAS parma
USE calen    IN 2   ORDER TAG calen2   ALIAS calen
USE maepre   IN 3   ORDER TAG maepre1  ALIAS maepre
USE maepar   IN 4   ORDER TAG maepar1  ALIAS presu
USE itepar   IN 5   ORDER TAG itepar1  ALIAS itepar
USE IteHc    IN 8   ORDER TAG Itehc1   ALIAS Itehc
USE Itetra   IN 10  ORDER TAG Itetra1  ALIAS Itetra
USE Itecre   IN 11  ORDER TAG Itecre1  ALIAS Itecre
USE HOJMOD   IN 12  ORDER TAG hojmod1  ALIAS hojmod
USE cresup   IN 13  ORDER TAG cresup1  ALIAS cresup
USE Trapar   IN 14  ORDER TAG trapar1  ALIAS trapar

*- Mensajes de aviso al usuario
vmens01 = 'Registro de Calendario'
vmens02 = ' Calendario : REVISION '
vmens04 = 'Dicho Calendario no fue encontrado'
vmens05 = 'No existe Calendario anterior'
vmens06 = 'No existe Calendario siguiente'
vmens07 = '¨ Desea Anular ‚ste Calendario ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Calendario ha sido anulado'
vmens10 = 'El Calendario ya est  Atendido'
vmens11 = 'El Calendario ha sido devuelto'

*- Abriendo Archivos

DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*--------------
SELECT calen
vtemp =RECNO()
vperiodo = RIGHT(STR(YEAR(DATE()),4),2)
store 0 to vTotal,vtipo
store space(2) to vcodfte,vcalend,VCODFUN,vUniges
store space(3) to vcodprg,vUnieje
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 

*------Listado Marco Ejecucion ----
	vtrimes=' '
	DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
	TITLE ' °°  Consolidado Presupuesto Anual  °° ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1
	@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
	@  1,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
	@  2,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
	@  3,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID IIF(!EMPTY(vUniGes),val_para(vUniGes,'UNIGES',' ',18,30),.T.) 		
	@  4,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID IIF(!EMPTY(vUniEje),val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30),.T.)

	@  6,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+allT(vuniges)+allt(vunieje),' ',18,30),.T.) when vTotal=1

	@  8,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
	@  9,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
	@ 10,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
	@ 11,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
	@ 12,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
	@ 13,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 		VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)
	@ 14,2 SAY ' N§Trimestre : ' GET vtrimes    PICTURE '@M 1,2,3,4,T'   
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

		USE IN 6
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
	
		GO TOP
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
			SELECT REPO
			SEEK  LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
			m.transf  = m.tra001+m.tra003+m.tra004+m.tra005
			STORE 0 TO m.M_01,m.M_02,m.M_03,m.M_04,m.M_05,m.M_06
			STORE 0 TO m.M_07,m.M_08,m.M_09,m.M_10,m.M_11,m.M_12
	
			IF !FOUND()
				m.ValPres = m.valpart
				APPEND BLANK
				GATHER MEMVAR
			ELSE
				IF RLOCK()
					REPLACE ValPres WITH ValPres + m.valpart
					REPLACE cresup  WITH cresup  + m.cresup
					REPLACE transf  WITH cresup  + m.transf
				ENDIF
				UNLOCK
			ENDIF
			SELECT Calen
			VKEY  = itepar.periodo+itepar.uniges+itepar.unieje+itepar.codcad+itepar.codfte+itepar.codpart
			VKEY0 = itepar.periodo+itepar.uniges+itepar.unieje+itepar.codcad+itepar.codfte+itepar.codpart
			SEEK VKEY
			IF FOUND()
				DO WHILE VKEY = VKEY0 AND !EOF()
					vTotCal = 0
					vtotafe = 0
					vkey1 = periodo+uniges+unieje+codcad+codfte+codpart+nummes
					vKey2 = periodo+uniges+unieje+codcad+codfte+codpart+nummes
					vcod1 = 'C_'+ALLTRIM(nummes)
					DO WHILE vKey1 = vKey2
						vTotCal = vTotCal + ValPart + Ampliar
						SKIP
						vKey2 = periodo+uniges+unieje+codcad+codfte+codpart+nummes
					ENDDO
					SELECT REPO
					REPLACE &vcod1 WITH &vcod1 + vTotCal
					SELECT Calen
					VKEY0 = periodo+uniges+unieje+codcad+codfte+codpart
				ENDDO	
			ENDIF	
			SELECT ItePar
		ENDSCAN
		SELE REPO
		zind = SYS(3) + '.IDX'
		INDEX ON LEFT(ESTFUN,30)+CODFTE+CODPART TO (zind)

		** HOJAS DE AFECTACION CTAR
		SELECT IteHc
		DO CASE
			CASE vtrimes = '1'
				SET FILTER TO NUMMES<='03' AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
				IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
			CASE vtrimes = '2'
				SET FILTER TO NUMMES<='06' AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
				IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)	AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
			CASE vtrimes = '3'
				SET FILTER TO NUMMES<='09' AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
				IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)	AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
			OTHER
				SET FILTER TO IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
				IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)	AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
		ENDCASE		
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

*		IF SISTEM = '1'
*			** HOJAS DE AFECTACION EMERGENCIA
*			SELECT 8
*			USE
*			USE \EMER97\DATA\IteHc    IN 8   ORDER TAG Itehc1   ALIAS ItehcE
*			SELECT 3
*			USE 
*			USE \EMER97\DATA\maepre   IN 3   ORDER TAG maepre1  ALIAS maepreE
*			SELECT IteHcE
*			DO CASE
*				CASE vtrimes = '1'
*					SET FILTER TO NUMMES<='03' and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
*						IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.);
*						AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
*				CASE vtrimes = '2'
*					SET FILTER TO NUMMES<='06' and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
*						IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)	;
*						AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
*				CASE vtrimes = '3'
*					SET FILTER TO NUMMES<='09' and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
*						IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)	;
*						AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
*				OTHER
*					SET FILTER TO IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
*						IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)	;
*						AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
*			ENDCASE		
*			GO TOP
*			SCAN
*				sele maepreE
*				seek Vperiodo+ITEHCE.uniges+ITEHCE.unieje+ITEHCE.codcad
*				vkey = uniges+unieje+codfun+codprg+codspr+actpry+ITEHCE.codcom+'00001'+ITEHCE.CODFTE+ITEHCE.CODPART
*				vkey1= uniges+unieje+codfun+codprg+codspr+actpry+ITEHCE.codcom+'00001'+ITEHCE.CODFTE
*				vmes = 'M_'+ALLTRIM(ITEHCE.nummes)
*				SELE REPO
*				SEEK vkey
*				IF FOUND()
*					REPLACE &vmes WITH &vmes+IIF(ITEHCE.TIPOPE='-',ITEHCE.VALPART*-1,ITEHCE.VALPART)
*				ELSE
*					GO TOP	
*					LOCATE FOR LEFT(ESTFUN,30)+CODFTE = vkey1
*					IF FOUND()
*						append blank
*						REPLACE CODPART WITH ITEHCE.CODPART,;
*							PERIODO WITH VPERIODO,;
*							CODCAD  WITH ITEHCE.CODCAD,;
*							CODFTE  WITH ITEHCE.CODFTE,;
*							ESTFUN  WITH LEFT(VKEY,30),;
*							&vmes   WITH IIF(ITEHCE.TIPOPE='-',ITEHCE.VALPART*-1,ITEHCE.VALPART)
*					ENDIF			
*				ENDIF
*				SELECT ITEHCE
*			ENDSCAN
*			SELECT 8
*			USE
*			USE IteHc    IN 8   ORDER TAG Itehc1   ALIAS Itehc
*			SELECT 3
*			USE 
*			USE maepre   IN 3   ORDER TAG maepre1  ALIAS maepre
*		ENDIF	
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
		GO TOP
      	IF EOF()
			DO standby WITH 'No existe Registros para procesar'
		ELSE
			DEACTIVATE WINDOW Standby
			IF vtotal = 1
				if vtipo = 1
					IF vtrimes = 'T'
      					DO reporte WITH 2,"Liscal1",' Presupuesto a nivel Trimestral(01) '
      					DO reporte WITH 2,"Liscal2",' Presupuesto a nivel Trimestral(02) '
      					DO reporte WITH 2,"Liscal3",' Presupuesto a nivel Trimestral(03) '
      					DO reporte WITH 2,"Liscal4",' Presupuesto a nivel Trimestral(04) '
      	
      				ELSE
      					VREPO = "Liscal"+vtrimes
      		    		DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
      				ENDIF
    			ELSE
    				IF vtrimes = 'T'
      					DO reporte WITH 2,"LisCal11",' Presupuesto a nivel Trimestral(01) '
      					DO reporte WITH 2,"LisCal22",' Presupuesto a nivel Trimestral(02) '
      					DO reporte WITH 2,"LisCal33",' Presupuesto a nivel Trimestral(03) '
      					DO reporte WITH 2,"LisCal44",' Presupuesto a nivel Trimestral(04) '
      				ELSE
      					VREPO = "LisCal"+vtrimes+vtrimes
      		    		DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
      				ENDIF
    			ENDIF
			ELSE
				if vtipo = 1
					IF vtrimes = 'T'
      					DO reporte WITH 2,"LisCaG1",' Presupuesto a nivel Trimestral(01) '
      					DO reporte WITH 2,"LisCaG2",' Presupuesto a nivel Trimestral(02) '
      					DO reporte WITH 2,"LisCaG3",' Presupuesto a nivel Trimestral(03) '
      					DO reporte WITH 2,"LisCaG4",' Presupuesto a nivel Trimestral(04) '
      				ELSE
      					VREPO = "LisCaG"+vtrimes
      		    		DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
      				ENDIF
    			ELSE
    				IF vtrimes = 'T'
      					DO reporte WITH 2,"LisCaG11",' Presupuesto a nivel Trimestral(01) '
      					DO reporte WITH 2,"LisCaG22",' Presupuesto a nivel Trimestral(02) '
      					DO reporte WITH 2,"LisCaG33",' Presupuesto a nivel Trimestral(03) '
      					DO reporte WITH 2,"LisCaG44",' Presupuesto a nivel Trimestral(04) '
      				ELSE
      					VREPO = "LisCaG"+vtrimes+vtrimes
      		    		DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
      				ENDIF
    			ENDIF
	  		ENDIF
        ENDIF
    ENDIF    
RETURN

PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN


PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_1
RELEASE    WINDOW wind_c1
RELEASE    MENU   mmenu
ON KEY LABEL F9
RESTORE SCREEN FROM principal
RETURN

PROCEDURE PENDIENTE
*------------------
DO CASE
   CASE ESTADO = '00' &&aprobado
       REPLACE ESTADO WITH '10'
   CASE ESTADO = '10' &&solicitado
       REPLACE ESTADO WITH '20'
 * CASE ESTADO = '20' &&no aprobado
 *     REPLACE ESTADO WITH '00'       
ENDCAS
DO VISTA       
RETURN

PROCEDURE Salida
*---------------
RELEASE    WINDOW LibDir
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN

FUNCTION sumpre
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE	
SUM VALPRES TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumcre
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
SUM CRESUP TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumtra
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
SUM TRANSF TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumar_prg
*-----------------
PARAMETER vCalen, PART,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = 1
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = 2
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE	
SUM &part TO sumsg FOR &vFiltro= vCalen
GO vrec
RETURN sumsg

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

PROCEDURE TOTALPRG1
*------------------
PARAMETER vCalen,vnivel,vtipo,vtrim
TOTALT = 0
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE	
IF VTIPO = "1"
	do case
 		case vtrim ='1'
	 		SUM  M_01+M_02+M_03 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='2'
 			SUM  M_04+M_05+M_06 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='3' 	
 			SUM  M_07+M_08+M_09 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='4'
 		    SUM  M_10+M_11+M_12 TO totalm FOR &vFiltro= vCalen
 	ENDCASE	    
 ELSE
 	do case
 		case vtrim ='1'
 			SUM C_01+C_02+C_03 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='2'	
 			SUM C_04+C_05+C_06 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='3'	
 			SUM C_07+C_08+C_09 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='4'	
 			SUM C_10+C_11+C_12 TO totalm FOR &vFiltro= vCalen
 	ENDCASE		
 ENDIF	
 TOTALT=TOTALT+TOTALM
GO TOP
GO vrec
RETURN TOTALT

*-------------------------------------------------------------------------
* MARCal.Prg
* Registra Calendarios
* Autor: Ing. Luis Castillo Dur n
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE parmae   IN 1   ORDER TAG parmae1  ALIAS parma
USE calen    IN 2   ORDER TAG calen2   ALIAS calen
USE maepre   IN 3   ORDER TAG maepre1  ALIAS maepre
USE maepar   IN 4   ORDER TAG maepar1  ALIAS presu
USE itepar   IN 5   ORDER TAG itepar1  ALIAS itepar
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

USE IN 7
USE IN 8
USE repocal  IN 7                    &&  ALIAS REPO   
USE repcal1  IN 8                    &&  ALIAS rep1   

SELE 7
vInd = SYS(3) + '.DBF'
COPY STRU TO (vInd)
use (vind) in 18 alias REPO EXCLUSIVE

SELE 8
vInd = SYS(3) + '.DBF'
COPY STRU TO (vInd)
use (vind) in 19 alias REP1 EXCLUSIVE


DO Inicia
DO salida
CLOSE DATA
RETURN


PROCEDURE Inicia
*--------------
USE IN 7
USE IN 8
USE repocal  IN 7                    &&  ALIAS REPO   
USE repcal1  IN 8                    &&  ALIAS rep1   

SELE 7
vInd = SYS(3) + '.DBF'
COPY STRU TO (vInd)
use (vind) in 18 alias REPO EXCLUSIVE

SELE 8
vInd = SYS(3) + '.DBF'
COPY STRU TO (vInd)
use (vind) in 19 alias REP1 EXCLUSIVE

SELECT calen
vRECNO = RECNO()
vOrder = ORDER()
vperiodo = RIGHT(STR(YEAR(DATE()),4),2)
store 0 to vTotal,vtipo
store space(2) to vcodfte,vcalend,VCODFUN,vUniges
store space(3) to vcodprg,vUnieje
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 
    	*------Listado Marco Presupuestal----
		DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
		TITLE ' °° Calendario Marco Presupuestal °° ' FLOAT COLOR SCHEME 5
		ACTIVATE WINDOW lis_1

		@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
		@  1,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  2,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  3,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID val_para(vUniGes,'UNIGES',' ',18,30)
		@  4,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)
		
		@  6,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+allT(vuniges)+allt(vunieje),' ',18,30),.T.) when vTotal=1
		
		@  8,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
		@  9,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
		@ 10,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
		@ 11,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
		@ 12,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
		@ 13,2 SAY '   Al mes de : ' GET vcalend    PICTURE '!!'  		VALID val_para(vcalend,'FECMES',' ',18,30)
		@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 		VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)

		READ VALID val_read()
		DEACTIVATE WINDOW lis_1
		IF LASTKEY()= 27
			RETURN
		ENDIF
	    ACTIVATE WINDOW Standby
		@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
		SELECT rep1
		ZAP
		vind = SYS(3) + '.IDX'                                                                   
		Zind = SYS(3) + '.IDX'                                                                   
		INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (Vind)
		
		IF vtotal = 1
	    	SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO, ITEPAR.CODCAD,itepar.cresup,itepar.tra001 ,;
		    	itepar.tra003,itepar.tra004,itepar.tra005,ITEPAR.Codfte,ITEPAR.TOTAFE,ITEPAR.TOTCAL,ITEPAR.UBICAC,ITEPAR.ESTFUN ;
				FROM ITEPAR ;
				WHERE periodo=ALLTRIM(Vperiodo) AND	IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
					IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) ;
				INTO CURSOR PRESUX

		ELSE
		
			SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO, ITEPAR.CODCAD,itepar.cresup,itepar.tra001 ,;
		    	itepar.tra003,itepar.tra004,itepar.tra005,ITEPAR.Codfte,ITEPAR.TOTAFE,ITEPAR.TOTCAL,ITEPAR.UBICAC,ITEPAR.ESTFUN ;
				FROM ITEPAR ;
				WHERE periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) and IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)AND IIF(!EMPTY(ALLT(vcodfte)),codfte=allt(vcodfte),.T.) ;
				INTO CURSOR PRESUX		
		ENDIF	
		GO TOP
		vInd = SYS(3) + '.DBF'
		COPY TO (vInd)
		use (vind) in 17 alias PRESU1 EXCLU

		SELE PRESU1
		INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (Zind)
		
		IF vtotal = 1

        	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO,CALEN.CODCAD,;
	    		CALEN.Codfte, CALEN.TOTAFE, CALEN.ampliar  ,CALEN.TOTOC ,CALEN.TOTOS, CALEN.ESTFUN ;
			  	FROM CALEN ;
			  	WHERE periodo=ALLTRIM(Vperiodo) AND	IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
					IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) AND NUMMES <= ALLTRIM(vcalend) ;
			  	INTO CURSOR CalenX
		ELSE
		    SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO,CALEN.CODCAD,;
	    		CALEN.Codfte, CALEN.TOTAFE , CALEN.ampliar,CALEN.TOTOC ,CALEN.TOTOS, CALEN.ESTFUN ;
			  	FROM CALEN ;
			  	WHERE periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) and IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)AND IIF(!EMPTY(ALLT(vcodfte)),codfte=allt(vcodfte),.T.) ;
					and NUMMES <= ALLTRIM(vcalend) ;
				INTO CURSOR CalenX		
		ENDIF	  	
	
		SELE PRESU1
		GO TOP

		SCAN
			SCATTER MEMVAR		
			m.valpres = m.valpart
			m.cresup  = getcre()
			m.transf  = gettra()
     		SELECT rep1
			IF F_APPD()
               GATHER MEMVAR
			ENDIF
			SELE PRESU1
	    ENDSCAN
		SELE CALENX
		GO TOP	
		SCAN
			SCATTER MEMVAR
     		vrep2 = LEFT(M.ESTFUN,5)+m.codcad+m.codfte+m.codpart
		    STORE 0 TO 	m.C_01,m.C_02,m.C_03,m.C_04,m.C_05,m.C_06,m.C_07,m.C_08,m.C_09,m.C_10,m.C_11,m.C_12
			SELECT rep1
			SEEK ALLTRIM(vrep2)
			IF !FOUND()
				vcod='C_'+ALLTRIM(m.nummes)
				m.&vcod=m.valpart + m.Ampliar
				APPEND BLANK
				GATHER MEMVAR
				vkey = LEFT(M.ESTFUN,5)+m.codCad+m.codfte+m.Codpart
				SELE PRESU1
				GO TOP
				SEEK vkey
				* CUANDO NO SE INGRESA EL MES
								
				IF FOUND()
					Vtransf = PRESU1.TRA001 +PRESU1.TRA003 +PRESU1.TRA004+PRESU1.TRA005
					SELE rep1
					REPLACE valpres WITH PRESU1.valpart,CRESUP WITH PRESU1.CRESUP ,transf with Vtransf
				ENDIF
				m.valpart=0
			ELSE
				vcod='C_'+ALLTRIM(m.nummes)
				REPLACE &vcod WITH &vcod + CALENX.VALPART + CALENx.ampliar
				m.valpart = 0
            endif
			SELECT calenX
		ENDSCAN
		USE IN 17
		SELE rep1
	    DEACTIVATE WINDOW Standby
        GO TOP
        IF vtotal = 1
			if vtipo = 1
				DO reporte WITH 2,"MarCal1",' A nivel de Marco Presupuestal'
			ELSE
				DO reporte WITH 2,"MarCal11",' A nivel de Marco Presupuestal'
			ENDIF
		ELSE
			if vtipo = 1
				DO reporte WITH 2,"MarCaG1",' A nivel de Marco Presupuestal'
			ELSE
				DO reporte WITH 2,"MarCaG11",' A nivel de Marco Presupuestal'
			ENDIF
		ENDIF	
		CLOSE INDEX
		set filt to
		SELE 13
		SET RELA TO 
		SELE 14
		SET RELA TO 
		
		SELECT calen
		SET ORDE TO CALEN2
 
return
PROCEDURE Salida
*---------------
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN

FUNCTION SUMPRE
*--------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE	
SUM valpRES+cresup+TRANSF TO suma FOR &vFiltro = vCalen
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
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+left(codpart,2)'
ENDCASE	
SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12  TO suma FOR &vFiltro= vCalen
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
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+left(codpart,2)'
	
ENDCASE	
SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12) TO sumA FOR &vFiltro = vCalen
GO vrec
RETURN sumA

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
		SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12  TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
	CASE VTIPO > 25 AND VTIPO < 28
		SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12  TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
	CASE VTIPO > 28 AND VTIPO < 30
		SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12  TO suma FOR LEFT(ESTFUN,25)+codfte+LEFT(codpart,2) = vFiltro
	CASE VTIPO > 30
		SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12  TO suma FOR LEFT(ESTFUN,25)+codfte+codpart = vFiltro
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
		SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12)  TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
	CASE VTIPO > 25 AND VTIPO < 28
		SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12)  TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
	CASE VTIPO > 28 AND VTIPO < 30
		SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12)  TO suma FOR LEFT(ESTFUN,25)+codfte+LEFT(codpart,2) = vFiltro
	CASE VTIPO > 30
		SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12)  TO suma FOR LEFT(ESTFUN,25)+codfte+codpart = vFiltro
ENDCASE
GO vrec
RETURN suma

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

* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ COMPEN.PRG        11/02/99                                   ณ	
* ณ Compromisos Pendientes de Pago                               ณ
* ณ AUTOR : PILAR PACHERRE NAVARRO           CTAR PIURA - 1999   ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
*
USE Parmae   IN  1  ORDER TAG Parmae1   ALIAS Parma
USE HOJCON   IN  2  ORDER TAG HOJCON1   ALIAS HOJA
USE ComPag   IN  3  ORDER TAG ComPag1   ALIAS ComPag
use itehc	 in  4  order tag itehc1	alias itehc 
USE Cheque   IN  9  ORDER TAG Cheque1   ALIAS Cheque
*ET EXACT ON
*
 DEFINE WINDOW COMPEN FROM 03,09 TO 16,70  DOUBLE SHADOW COLOR SCHEME 5 
Vmens08 = 'No hay registros para procesar'
*
  ACTIVATE WINDOW COMPEN
	vIdx=sys(3)+'.idx'
  vctacte  = SPACE(14)
  xMes     = SPACE(2)
  xAno     = RIGHT(DTOC(DATE()),2)
  VCODPRG  = '02'
  STORE 1 TO  vtip,vtocta
  STORE DATE()   TO vfec1,vfec2
  @ 00,01 SAY "     Tipo Reporte : " GET vTip   FUNCTION '^ Mensual;Acumulado'
  @ 03,01 SAY "      Dependencia : " GET vtocta FUNCTION '^ Sede;Energกa;Industria;Pesquerกa;Trabajo;Bayovar'
  @ 07,01 SAY '      Mes y Aคo : ' GET xMes PICTURE '!!'  VALID Val_Para(xMes,'FECMES',' ',22,9) when vtip=1
  @ 07,24 GET xAno PICTURE '!!' WHEN VTIP=1
  @ 09,01 SAY " Fec.Inic.: " GET vfec1  PICTURE '@D'   WHEN vtip=2
  @ 09,30 say " Fec.Fin. : " GET vfec2  PICTURE '@D'   WHEN vtip=2
  @ 11,10 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 5 
  READ CYCLE VALID exi()
  DEACTIVATE WINDOW COMPEN
  IF LASTKEY()=27 OR okcancel = 2
    CLOSE DATA
  	RETURN
  ENDIF
  do case
  	case vtocta=1
  		condw="(LEFT(NUMhc,1)$'0123456789' and codcad<>'0042')"
	case vtocta=2
  		condw="(LEFT(NUMhc,1)$'E')"
	case vtocta=3
  		condw="(LEFT(NUMhc,1)$'I')"
	case vtocta=4	  		
  		condw="(LEFT(NUMhc,1)$'P')"
	case vtocta=5  		
  		condw="(LEFT(NUMhc,1)$'T')"
	case vtocta=6  		
  		condw="(codcad='0042')"
  endcase
  *  	  		
  DO CASE
  	CASE VTIP=1
			DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
			ACTIVATE WINDOW Xwait
			@0,0 SAY " Espere un momento ......... " COLOR W+/BR*
			SELE hoja
		    INDEX ON CODFTE+nummes+numhc TO (vIdx) for nummes=ALLT(xmes) .AND. periodo=xano .AND. ((estado='00'OR ESTADO='20') OR (ESTADO='50' AND NUMMESCP>XMES)) and &condw
		    DEACTIVATE WINDOW xwait
       		DO REPORTE WITH 2, "compmen1", ' Compromisos Pendientes ',1,.F.,.T.			
		    DEACTIVATE WINDOW xwait
  	case vtip=2
			DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
 		    ACTIVATE WINDOW Xwait
		    @0,0 SAY " Procesando reporte...." COLOR W+/BR*
		    SELE hoja
			INDEX ON nummes+numhc TO (vidx) FOR (estado='00'OR ESTADO='20') and BETWEEN(Fechc,vFec1,vFec2) and &condw
			DEACTIVATE WINDOW xwait
            DO reporte WITH 2,"compacu"," Compromisos Pendientes Acumulados",2,.F.,.T.                                  	       
	endcase
  *
RELEASE  WINDOW libban
*HOW popup pop_08
ACTIVATE SCREEN
CLOSE    DATABASES
erase(vIdx)
RETURN
***********

FUNCTION VCHQ1
*-------------
PARAMETERS vCLAW
PRIVATE vcheq
AL=ALIAS()
STORE 0.00 TO vcheq
SELE CHEQUE
*SEEK HOJA.NUMMESCP+HOJA.NUMCP+HOJA.CODCTC
seek vclaw
IF FOUND()
	VCHEQ = VALCHQ
ENDIF   
SELEC (AL)
RETURN vCHEQ

FUNCTION VRAZ1
*-------------
PARAMETERS vclaw
PRIVATE VRAZON
AL=ALIAS()
*store 0.00 to vmonto
store space(60) to vrazon
SELE CHEQUE
SEEK HOJA.NUMMESCP+HOJA.NUMCP+HOJA.CODCTC
*eek vclaw
IF FOUND()
	VRAZON = NOMGIR
*	VMONTO = VALCHQ
ENDIF   
SELEC (AL)
RETURN vRAZON


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)

PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR Okcancel=1 OR Okcancel=2
	RETURN .T.
ENDIF
RETURN .F.

FUNCTION VPAR1
*-------------
PARAMETERS VNUMMES,VNUMHC
PRIVATE VPARTI
AL=ALIAS()
*store 0.00 to vmonto
store space(6) to vparti
SELE iteHC
SEEK HOJA.NUMMES+HOJA.NUMhc
IF FOUND()
	Vparti = codpart
ENDIF   
SELEC (AL)
RETURN vparti


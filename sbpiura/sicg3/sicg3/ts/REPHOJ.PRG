
* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ COMPEN.PRG        11/02/99                                   ณ	
* ณ Compromisos Efectuados en el Mes                             ณ
* ณ AUTOR : PILAR PACHERRE NAVARRO           CTAR PIURA - 1999   ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
*
USE Parmae   IN  1  ORDER TAG Parmae1   ALIAS Parma
USE HOJCON   IN  2  ORDER TAG HOJCON1   ALIAS HOJA
USE ITEHC	 IN  4  ORDER TAG ITEHC1    ALIAS ITEHC
USE RETEN    IN  5  ORDER TAG RETEN1    ALIAS RETEN
USE ComPag   IN  3  ORDER TAG ComPag1   ALIAS ComPag
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
* @ 00,01 SAY "     Tipo Reporte : " GET vTip   FUNCTION '^ Mensual;Acumulado'
  @ 03,01 SAY "Sede y/o Sectores : " GET vtocta FUNCTION '^ Sede;Sectores'
  @ 07,01 SAY '      Mes y Aคo : ' GET xMes PICTURE '!!'  VALID Val_Para(xMes,'FECMES',' ',22,9) when vtip=1
  @ 07,24 GET xAno PICTURE '!!' WHEN VTIP=1
* @ 09,01 SAY " Fec.Inic.: " GET vfec1  PICTURE '@D'   WHEN vtip=2
* @ 09,30 say " Fec.Fin. : " GET vfec2  PICTURE '@D'   WHEN vtip=2
  @ 11,10 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 5 
  READ CYCLE VALID exi()
  DEACTIVATE WINDOW COMPEN
  IF LASTKEY()=27 OR okcancel = 2
    CLOSE DATA
  	RETURN
  ENDIF
  DO CASE
  	CASE VTOCTA=1
			DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
			ACTIVATE WINDOW Xwait
			@0,0 SAY " Espere un momento ......... " COLOR W+/BR*
			SELE hoja
		    INDEX ON CODFTE+NUmmes+numhc TO (vIdx) for (nummes=ALLT(xmes) .AND. periodo=xano) and (LEFT(numhc,1)$'0123456789'  AND ESTADO<>'99')
		    DEACTIVATE WINDOW xwait
		    DO reporte WITH 2,"REPHOJA"," Compromisos Pendientes Acumulados",2,.F.,.T.                                  	       
  	case vTOCTA=2
			DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
 		    ACTIVATE WINDOW Xwait
		    @0,0 SAY " Procesando reporte...." COLOR W+/BR*
		    SELE hoja
		    INDEX ON CODFTE+numhc TO (vIdx) for (nummes=ALLT(xmes) .AND. periodo=xano) and (LEFT(numhc,1)$'EIPT') AND ESTADO<>'99'
			DEACTIVATE WINDOW xwait
		    DO reporte WITH 2,"REPHOJS"," Compromisos Pendientes Acumulados",2,.F.,.T.                                  	       
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
PARAMETERS vNUMES,VNUMCP,VCODCTC
PRIVATE vcheq
AL=ALIAS()
STORE 0.00 TO vcheq
SELE CHEQUE
seek vNUMES+VNUMCP+VCODCTC
IF FOUND()
	VCHEQ = VALCHQ
ENDIF   
SELEC (AL)
RETURN vCHEQ

FUNCTION VSIAF1
*-------------
PARAMETERS WNUMES,WNUMCP,WCODCTC
PRIVATE vSIAF
AL=ALIAS()
STORE SPACE(4) TO vSIAF
SELE COMPAG
seek WNUMES+WNUMCP+WCODCTC
IF FOUND()
	VSIAF = NUMSIAF
ENDIF   
SELEC (AL)
RETURN vSIAF

FUNCTION VRET1
*-------------
PARAMETERS VNUMMESCP,VNUMCP,VCODCTC
PRIVATE VRETEN
AL=ALIAS()
store 0.00 to vRETEN
SELE RETEN
SEEK HOJA.NUMMESCP+HOJA.NUMCP+HOJA.CODCTC
IF FOUND()
	DO WHILE NUMMES=HOJA.NUMMESCP AND NUMCP=HOJA.NUMMESCP AND CODCTC=HOJA.CODCTC
		VRETEN=VRETEN+VALRET
		SKIP
	ENDDO
ENDIF   
SELEC (AL)
RETURN vRETEN

FUNCTION VPAR1
*-------------
PARAMETERS VNUMMES,VNUMHC
PRIVATE VPARTI
AL=ALIAS()
*store 0.00 to vmonto
store space(6) to vparti
SELE iteHC
SEEK HOJA.NUMMES+HOJA.NUMHC
IF FOUND()
	Vparti = codpart
ENDIF   
SELEC (AL)
RETURN vparti

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

*------------------------------------------------------
* PROGRAMA: Indexa.prg
* OBJETIVO: Rutina de Organizaci¢n archivos
*           Crea ¡ndices compuestos y compacta la data.
* Autor   : UDEP ,LCD
*-------------------------------------------------------
PARAMETER _fil

PRIVATE _auto, _path, ss, pos
ss=SET('PATH')
pos = AT(";",ss)
_path=IIF(pos=0,ss,SUBSTR(ss,1,AT(";",ss)-1))

_auto = IIF( PARAMETER()>0,.T.,.F.)

CLOSE DATA

IF !WEXIST("msg2user")
	DEFINE WINDOW MSG2USER FROM 19,27 TO 23,77
ENDIF

IF !_auto
	IF escolor
		DEFINE POPUP _yt  FROM 6,55          SHADOW COLOR &L_COL
	 ELSE
		DEFINE POPUP _yt  FROM 6,55          COLOR SCHEME C_POPUP
	ENDIF
	
	DEFINE BAR  1 OF _yt PROMPT "\<a Par metros"
	DEFINE BAR  2 OF _yt PROMPT "\<b Calendario"
	DEFINE BAR  3 OF _yt PROMPT "\<c Presupuesto"
	DEFINE BAR  4 OF _yt PROMPT "\<d Cr‚ditos Sup"
	DEFINE BAR  5 OF _yt PROMPT "\<e Transferencias"
	DEFINE BAR  6 OF _yt PROMPT "\<f E-5s"
	DEFINE BAR  7 OF _yt PROMPT "\<g TODOS"    
	
	ON SELECTION POPUP _yt DO ORGANIZA WITH BAR()
	ACTIVATE POPUP _yt
	
	RELEASE POPUP _yt
 ELSE
	x = _num(_fil)
	DO organiza WITH x
ENDIF

RETURN


PROCEDURE organiza
*-----------------
PARAMETER cual

DEFINE WINDOW Working   FROM 02,40 TO 23,76 FLOAT   COLOR SCHEME 1
ACTIVATE WINDOW Working

CLOSE DATA
SET EXCLUSIVE ON
SET TALK ON
_con = SET('CONSOLE')
SET CONSOLE ON

ACTIVATE WINDOW Working

CLEAR

IF cual = 1 or cual=8
	*-Archivo de Parametros
	USE ParMae EXCLUSIVE
	PACK
	=ordena("Tipo+allt(Codigo)+CodigoAux","ParMae1")
	=ordena("TIPO+DESCRI+CODIGO+CODIGOAUX","ParMae2")
	=ordena("DESCRI","ParMae3","TIPO='CODRET'")
  USE
ENDIF

IF cual = 2 or cual=8
	*-Archivo del Calendario
	USE Calen  EXCLUSIVE
	PACK
	=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+NUMMES+CODPART","Calen1")
	=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART+NUMMES","Calen2")
*	=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+NUMMES+CODPART","Calen3")		&& Identico a calen1
	=ordena("PERIODO+LEFT(ESTFUN,30)+CODFTE+NUMMES+CODPART","Calen4") && PARA META
	=ordena("PERIODO+LEFT(ESTFUN,25)+CODFTE+NUMMES+CODPART","Calen5") && PARA COMPONENTE
	=ordena("PERIODO+LEFT(ESTFUN,20)+CODFTE+NUMMES+CODPART","Calen6") && PARA ACTIVIDAD/PROYECTO
	=ordena("PERIODO+LEFT(ESTFUN,14)+CODFTE+NUMMES+CODPART","Calen7") && PARA SUBPROGRAMA
	=ordena("PERIODO+LEFT(ESTFUN,10)+CODFTE+NUMMES+CODPART","Calen8") && PARA PROGRAMA
	=ordena("PERIODO+LEFT(ESTFUN,7)+CODFTE+NUMMES+CODPART","Calen9")  && PARA FUNCION
	USE
endif

IF cual = 3 or cual=8
	*-Archivo del Presupuesto
	USE MaePar EXCLUSIVE
	PACK
	=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE","MaePar1")
	USE
	
	USE ItePar EXCLUSIVE
	PACK
	=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART","ItePar1")
	=ordena("PERIODO+CODCAD+CODFTE+TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2","ItePar2")
	=ordena("PERIODO+CODCAD+CODFTE+CODPART","ItePar3")
	=ordena("PERIODO+LEFT(ESTFUN,30)+CODFTE+CODPART","ItePar4") && PARA META
	=ordena("PERIODO+LEFT(ESTFUN,25)+CODFTE+CODPART","ItePar5") && PARA COMPONENTE
	=ordena("PERIODO+LEFT(ESTFUN,20)+CODFTE+CODPART","ItePar6") && PARA ACTIVIDAD/PROYECTO
	=ordena("PERIODO+LEFT(ESTFUN,14)+CODFTE+CODPART","ItePar7") && PARA SUBPROGRAMA
	=ordena("PERIODO+LEFT(ESTFUN,10)+CODFTE+CODPART","ItePar8") && PARA PROGRAMA
	=ordena("PERIODO+LEFT(ESTFUN,7)+CODFTE+CODPART","ItePar9")  && PARA FUNCION
	USE
	
	*-Archivo de Maestro de Presupuesto
	USE MaePre EXCLUSIVE
	PACK
	=ordena("PERIODO+UNIGES+ UNIEJE+CODCAD","MaePre1")
	=ordena("DESCRI","MaePre2")
	=ordena("PERIODO+CODCAD+CODCOM+CODMET","MaePre3")
	=ordena("PERIODO+ UNIGES+ UNIEJE+ CODFUN+ CODPRG+ CODSPR+ ACTPRY+ CODCOM+CODMET","MaePre4")
	=ordena("PERIODO+ ACTPRY+ CODCOM","MaePre5")
	USE

ENDIF

IF cual = 4 or cual=8
	*-Archivo de Ingresos
	USE Ingreso  EXCLUSIVE
	PACK
	=ordena("CodIng+SubIng","Ingreso1")
	USE  
ENDIF

IF cual = 5 OR cual=8
	*-Archivo de Creditos Suplementarios
	USE CreSup EXCLUSIVE
	PACK
	=ordena("PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)","CreSup1")
	USE
	
	*-Archivo de Items de Creditos Suplementarios
	USE IteCre EXCLUSIVE
	PACK
	=ordena("PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+UNIGES+UNIEJE+ALLTRIM(CODFTE)","IteCre1")
	=ordena("PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+CODCAD+ALLTRIM(CODFTE)+CodPart","IteCre2")
	=ordena("PERIODO+CODCAD+CODFTE+CODPART","itecre3")
	USE
ENDIF

IF cual = 6 OR cual=8
 *-Archivo de Transferencia de Partidas
   USE TraPar EXCLUSIVE
   PACK
   =ordena("PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+CODFTE","TraPar1")
   USE
  *-Archivo de Items de Transferencia de Partidas
   USE IteTra EXCLUSIVE
   PACK
   =ordena("PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+UNIGES+UNIEJE+CODFTE+CODCAD","IteTra1")
   =ordena("PERIODO+CODCAD+ALLTRIM(CODFTE)+ CodPart","IteTra2")
   USE

endif

IF cual = 7 or cual=8
 *-Archivo del Presupuesto
*  USE REG_E5 EXCLUSIVE
*  PACK
*  =ordena("NUMMES+NUMHC","REG_E51")
*  =ordena("SUBSTR(CODCAL,8,2)+SUBSTR(CODCAL,10,3)","reg_e52")
*  =ordena("SUBSTR(CODCAL,1,12)+TIPFUN","reg_e53")
*  USE
ENDIF

RELEASE WINDOW Working
CLOSE DATA
SET EXCLU OFF
SET TALK OFF

RETURN


FUNCTION ordena
*--------------
PARAMETER key,idx,condi
   IF PARAMETERS()<3
      INDEX ON &key TAG (idx)
   ELSE
      INDEX ON &key TAG (idx) FOR &condi
   ENDIF
RETURN ""


FUNCTION _num
*------------
PARAMETER _FF

   _ff = UPPER(_ff)

   DO CASE
   CASE _ff="TODOS"
      _ffun = 1
   CASE _ff="CABDIC"
      _ffun = 2
   CASE _ff="PARMAE"
      _ffun = 3
   CASE _ff="PARMAE"
      _ffun = 3
   CASE _ff="CRESUP"
      _ffun = 4
   CASE _ff="TRAPAR"
      _ffun = 5
   OTHERWISE
      _ffun = 0
   ENDCASE

RETURN _ffun


*tabla CatAsi
*
*INDEX ON TIPPRE+ GENERIC+ SGN1 +SGN2 +ESPN1 +ESPN2 FOR catasi.tippre='1' and  catasi.detalle = 'S' TAG CATASI1 ADDITIVE
*INDEX ON TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2 FOR catasi.tippre = '2' and  catasi.detalle = 'S' TAG CATASI2 ADDITIVE
*INDEX ON TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2 FOR catasi.tippre='1' TAG CATASI3 ADDITIVE
*INDEX ON TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2 FOR catasi.tippre= '2' TAG CATASI4 ADDITIVE

*itepari
*
*PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART itepari1
*PERIODO+CODCAD+CODFTE+CODPART itepari2
*PERIODO+LEFT(ESTFUN,30)+CODFTE+CODPART itepari3
*PERIODO+LEFT(ESTFUN,25)+CODFTE+CODPART itepari4
*PERIODO+LEFT(ESTFUN,20)+CODFTE+CODPART itepari5
*PERIODO+LEFT(ESTFUN,14)+CODFTE+CODPART itepari6
*PERIODO+LEFT(ESTFUN,10)+CODFTE+CODPART itepari7
*PERIODO+LEFT(ESTFUN,7)+CODFTE+CODPART itepari8



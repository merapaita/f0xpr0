DO ORGANIZA WITH 1


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


IF cual=1 OR cual=2   && Tesoreria Regional


  
ENDIF

IF cual=1 OR cual=3   && Tesoreria Sede
  
  
ENDIF

IF cual=1 OR cual=4   &&-Archivo de Parametros

  ?'ParMae'
  USE ParMae EXCLUSIVE
  PACK
  =ordena("Tipo+Codigo+CodigoAux","ParMae1")
  =ordena("Tipo+DESCRI+Codigo+CodigoAux","ParMae2")  
  =ordena("DESCRI","ParMae3","tipo='CODRET'")  
  USE
ENDIF

RELEASE WINDOW Working
CLOSE DATA
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
   CASE _ff="TESORE"
      _ffun = 2
   CASE _ff="PARAME"
      _ffun = 3
   OTHERWISE
      _ffun = 0
   ENDCASE

RETURN _ffun
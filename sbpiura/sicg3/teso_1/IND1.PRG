*------------------------------------------------------
* PROGRAMA: Indexa.prg
* OBJETIVO: Rutina de Organizaci¢n archivos
*           Crea ¡ndices compuestos y compacta la data.
* Autor   : UDEP 
*-------------------------------------------------------
PARAMETER _fil

*PRIVATE _auto
*_auto = IIF( PARAMETER()>0,.T.,.F.)

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
    DEFINE POPUP _yt  FROM 9,55          SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP _yt  FROM 9,55          COLOR SCHEME C_POPUP
  ENDIF

  DEFINE BAR  1 OF _yt PROMPT "\<a TODOS"
  DEFINE BAR  2 OF _yt PROMPT "\<b Tesorer¡a Regional"
  DEFINE BAR  3 OF _yt PROMPT "\<c Tesorer¡a Sede" 
  DEFINE BAR  4 OF _yt PROMPT "\<d Par metros"

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

  ?'Factura'
  USE Factura EXCLUSIVE
  PACK
  =ordena("Serie+Numero","Factura1")
  =ordena("Perri+ALLT(Nummesri)+numri","Factura2")
  =ordena("Serie+Numero+Conlic","Factura3")
  USE


  
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



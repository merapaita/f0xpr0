CLOSE DATA
IF !WEXIST('standby')
   DEFINE WINDOW standby    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW COLOR SCHEME 5
ENDIF

ACTIVATE WINDOW standby
@ 02,05 SAY 'Espere un momento ...Indexando' COLOR 5*
SUSP


  ?'ItePag'
  USE ItePag EXCLUSIVE
  PACK
  =ordena("Periodo+NumMes+CodUsu","ItePag1")
  USE

  ?'Pagos'
  USE Pagos EXCLUSIVE
  PACK
  =ordena("PERIODO+NUMMES+ALLTRIM(CODUSU)","Pagos1")
  USE
  
  ?'Tablas'
  USE Tablas EXCLUSIVE
  PACK
  =ordena("TIPO+CODIGO","Tablas1")
  =ordena("DESCRI","Tablas2")
  USE
  
  ?'Usuarios'
  USE Usuarios EXCLUSIVE
  PACK
  =ordena("CODUSU","Usuarios1")
  =ordena("ALLTRIM(NOMUSU)","Usuarios2")
  =ordena("ANEXO+ZONA+NOMUSU","Usuarios3")
  USE
  
DEACTIVATE WINDOW standby


FUNCTION ordena
*--------------
PARAMETER key,idx,condi
   IF PARAMETERS()<3
      INDEX ON &key TAG (idx)
   ELSE
      INDEX ON &key TAG (idx) FOR &condi
   ENDIF
RETURN ""


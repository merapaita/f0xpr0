** --------------------------------
** CLAVE.PRG
** Clave de Acceso
** --------------------------------

PARAMETER xUser_ID,xFila1,xColumna1,xFila2,xColumna2

*-- Regresa las siguientes variables
* xUser_ID = Codigo de usuario

*-- Necesita las siguientes variables
* xFila1   = Fila de inicio de la ventana
* xColumna1= Columna de inicio de la ventana
* xFila2   = Fila de termino de la ventana
* xColumna2= Columna de termino de la ventana

*  El archivo de usuarios se asume que debe estar abierto.


xPassword= SPACE(10)
vRetorno = .F.
SELECT Usua
DEFINE WINDOW Temp FROM xFila1,xColumna1 TO xFila2,xColumna2;
  TITLE '  Usuario  ' SHADOW
xUser_ID = UPPER(xUser_ID)
@ 2,12 SAY ''
IF SEEK(xUser_ID)
   IF LASTKEY() = 27
      vretorno = .F.
   ELSE
	  vretorno = .T.
   ENDIF
ENDIF
DEACTIVATE WINDOW Temp
RELEASE WINDOW Temp
IF vretorno
  *@ 23,25 SAY IIF(VAL(SUBSTR(TIME(),1,2)) < 12,'Buenos Dias ',;
    'Buenas Tardes ') + ALLTRIM(Usua.Nombre)
*  @ 23,25 SAY 'USUARIO : ' + ALLTRIM(Usua.Nombre)
*  WAIT '' TIMEOUT 2
 * @  23,25,23,80  BOX "같같같같�" COLOR SCHEME c_fondo
   vCoddep = Usua.Coddep
ENDIF
RETURN vretorno

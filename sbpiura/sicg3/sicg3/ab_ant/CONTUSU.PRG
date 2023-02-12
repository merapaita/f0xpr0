*
*
IF LASTKEY()=27
   CLOSE DATA
   RETURN
ENDIF
hide popup pop_02H
DO CONSULTA
USE
 show popup pop_02H
RETURN

PROCEDURE CONSULTA
*------------------
SET DELE ON
USE UsuGRAU IN 0 ALIAS Usu
SELECT Usu
ON KEY LABEL ENTER DO Mensaje
ON KEY LABEL F8 DO BORRA
DEFINE WINDOW Wind_1 FROM 4,1 TO  20,79 TITLE "[ENTER] ®Enviar Mensaje¯    Usuarios de los Sistemas" COLOR SCHEME 10
BROWSE WINDOW Wind_1 NOMODI NOAPPEND NOMENU FIELDS ;
CODUSU    :H='Usuario' ,;
MACHINE   :H='M q',;
TIPMAQ    :H='Equipo',;
PROG      :H='Programa' :40,;
DIA       :H='Fecha',;
HORA      :H='Inicio'

ON KEY LABEL ENTER 
ON KEY LABEL F8

PROCEDURE BORRA
*--------------
DELETE NEXT 1
RETURN

FUNCTION Mensaje
*----------------
PRIVATE vmens,vsen,vusu
DEFINE WINDOW Wind_2 FROM 15,1 TO 17,79 TITLE "Mensaje" COLOR SCHEME 15
ACTIVATE WINDOW Wind_2
vmens = SPACE(80)
ON KEY LABEL ENTER KEYBOARD(CHR(23))
@ 0,1 SAY " Mensaje : " GET vmens VALID !EMPTY(vmens) PICT "@S64" &&SIZE 2,80
READ 
DEACTIVATE WINDOW Wind_2

vUsu = ALLTRIM(CodUsu)
IF LASTKEY()=27 AND EMPTY(vmens)
	RETURN
ENDIF
vsen = '!SEND '+'"'+ALLTRIM(VMENS)+'"'+' '+'TO'+' '+'&VUSU'
&vsen
ON KEY LABEL ENTER DO Mensaje

RETURN

FUNCTION VAL_READ1
*----------------
* Valida al terminar un Read
* Ejemplo:  READ VALID Val_Read()
PRIVATE _Fun
_Fun = .T.
IF LASTKEY() # 27
  IF .NOT. YESNO('¨ Est n correctos los datos ?')
    _Fun = .F.
  ENDIF
ENDIF
RETURN (_Fun)


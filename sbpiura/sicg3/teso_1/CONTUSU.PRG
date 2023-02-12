IF LASTKEY()=27
   CLOSE DATA
   RETURN
ENDIF
hide popup pop_09
DO CONSULTA
USE
SELE parma
USE
ON KEY
show popup pop_09
RETURN

PROCEDURE CONSULTA
*------------------
SET DELE ON
USE parmae  IN 1 ORDER Parmae1 ALIAS Parma
USE UsuGRAU IN 0               ALIAS Usu
SELECT Usu
ON KEY LABEL ENTER DO Mensaje
ON KEY LABEL F2    DO Mens_Group
ON KEY LABEL F4    DO Mens_all
ON KEY LABEL F8 DO BORRA
DEFINE WINDOW Wind_1 FROM 4,1 TO  22,79;
              TITLE "[ENTER] ®Enviar Mensaje¯ [F2] ®Mens. a Grupos¯  [F4] ®Mens. a Todos¯";
              COLOR SCHEME 10
BROWSE WINDOW Wind_1  NOMODI NOAPPEND NOMENU FIELDS ;
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
@ 0,1 SAY " Mensaje : " GET vmens VALID !EMPTY(vmens) PICT "@S64" 
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


FUNCTION Mens_Group
*------------------
PRIVATE vmens,vsen,vusu
DEFINE WINDOW Wind_2 FROM 13,1 TO 17,79 TITLE "Mensaje" COLOR SCHEME 15
ACTIVATE WINDOW Wind_2
vmens = SPACE(80)
vgrup = SPACE(02)
vdes  = SPACE(15)
ON KEY LABEL ENTER KEYBOARD(CHR(23))
@ 1,1 SAY " Grupo   : " GET vgrup VALID Val_Para(vgrup,'GRUPOS',' ',22,9) 
READ
@ 2,1 SAY " Mensaje : " GET vmens VALID !EMPTY(vmens) PICT "@S64" 
READ 
DEACTIVATE WINDOW Wind_2
SELE parma
IF SEEK ('GRUPOS'+ALLT(vgrup))
   vdes = ALLTRIM(parma.descri)
ENDIF   
IF LASTKEY()=27 AND EMPTY(vmens)
	RETURN
ENDIF
vsen = '!SEND '+'"'+ALLTRIM(VMENS)+'"'+' '+'TO'+' '+'&Vdes'
&vsen
ON KEY LABEL ENTER DO Mensaje

RETURN


FUNCTION Mens_all
*----------------
PRIVATE vmens,vsen,vusu
DEFINE WINDOW Wind_2 FROM 15,1 TO 17,79 TITLE "Mensaje" COLOR SCHEME 15
ACTIVATE WINDOW Wind_2
vmens = SPACE(80)
ON KEY LABEL ENTER KEYBOARD(CHR(23))
@ 0,1 SAY " Mensaje : " GET vmens VALID !EMPTY(vmens) PICT "@S64" 
READ 
DEACTIVATE WINDOW Wind_2

IF LASTKEY()=27 AND EMPTY(vmens)
	RETURN
ENDIF
vsen = '!SEND '+'"'+ALLTRIM(VMENS)+'"'+' '+'TO'+' '+'EVERYBODY'
&vsen
ON KEY LABEL ENTER DO Mensaje

RETURN

FUNCTION VAL_READ1
*----------------
* Valida al terminar un Read
* Ejemplo:  READ VALID Val_Read()
* Autor: C‚sar Ch varry Arias   Fecha: 14-09-93
PRIVATE _Fun
_Fun = .T.
IF LASTKEY() # 27
  IF .NOT. YESNO('¨ Est n correctos los datos ?')
    _Fun = .F.
  ENDIF
ENDIF
RETURN (_Fun)


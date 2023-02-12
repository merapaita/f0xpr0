IF LASTKEY() = 27
     CLOSE DATABASES
     RETURN
ENDIF
HIDE POPUP pop_02h
DO consulta
USE
SHOW POPUP pop_02h
RETURN
*
PROCEDURE consulta
SET DELETED ON
USE IN 0 UsuGRAU ALIAS usu
SELECT usu
ON KEY LABEL ENTER DO Mensaje
ON KEY LABEL F8 DO BORRA
DEFINE WINDOW wind_1 FROM 4, 1 TO  ;
       20, 79 TITLE  ;
       '[ENTER] ®Enviar Mensaje¯    Usuarios de los Sistemas'  ;
       COLOR SCHEME 10
BROWSE FIELDS codusu :H =  ;
       'Usuario', machine :H =  ;
       'M q', tipmaq :H =  ;
       'Equipo', prog :H =  ;
       'Programa' : 40, dia :H =  ;
       'Fecha', hora :H =  ;
       'Inicio' NOMENU NOAPPEND  ;
       NOEDIT WINDOW wind_1
ON KEY LABEL ENTER
ON KEY LABEL F8
*
PROCEDURE borra
DELETE NEXT 1
RETURN
*
PROCEDURE mensaje
PRIVATE vmens, vsen, vusu
DEFINE WINDOW wind_2 FROM 15, 1  ;
       TO 17, 79 TITLE 'Mensaje'  ;
       COLOR SCHEME 15
ACTIVATE WINDOW wind_2
vmens = SPACE(80)
ON KEY LABEL ENTER KEYBOARD(CHR(23))
@ 0, 1 SAY ' Mensaje : ' GET  ;
  vmens PICTURE '@S64' VALID   ;
  .NOT. EMPTY(vmens)
READ
DEACTIVATE WINDOW wind_2
vusu = ALLTRIM(codusu)
IF LASTKEY() = 27 .AND.  ;
   EMPTY(vmens)
     RETURN
ENDIF
vsen = '!SEND '+'"'+ALLTRIM(VMENS)+'"'+' '+'TO'+' '+'&VUSU'
&vsen
ON KEY LABEL ENTER DO Mensaje
RETURN
*
FUNCTION val_read1
PRIVATE _fun
_fun = .T.
IF LASTKEY() <> 27
     IF  .NOT. yesno( ;
         '¨ Est n correctos los datos ?' ;
         )
          _fun = .F.
     ENDIF
ENDIF
RETURN (_fun)
*

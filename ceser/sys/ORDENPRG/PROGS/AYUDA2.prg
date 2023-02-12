*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER campo1, campo2, mensaje,  ;
          clave
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'BBB', 'ESC'
DIMENSION op( 2)
op( 1) = 'Ordenados por C¢digo'
op( 2) = 'Ordenados por Nombre'
DEFINE POPUP ayu1 FROM 0, 0 TO 3,  ;
       21 SHADOW COLOR SCHEME 8
FOR i = 1 TO 2
     DEFINE BAR i OF ayu1 PROMPT  ;
            op(i)
ENDFOR
ON SELECTION POPUP ayu1 do choice with;
prompt(),clave
ACTIVATE POPUP ayu1 NOWAIT
FOR i = 0 TO 10
     MOVE POPUP ayu1 TO i, 0
ENDFOR
FOR i = 0 TO 14
     MOVE POPUP ayu1 TO 10, i * 2
ENDFOR
ACTIVATE POPUP ayu1
DEACTIVATE POPUP ayu1
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE choice
PARAMETER x, clave
FOR i = 1 TO 14
     MOVE POPUP ayu1 BY 0, -2
ENDFOR
FOR i = 1 TO 05
     MOVE POPUP ayu1 BY -1, 0
ENDFOR
FOR i = 1 TO 2
     IF op(i) = x
          EXIT
     ENDIF
ENDFOR
IF i = 1
     SET ORDER TO 1
     campox = campo1
ELSE
     SET ORDER TO 2
     campox = campo2
ENDIF
define popup ayu2 from 5,24 to 13,70 promp;
field &campox shadow title mensaje COLOR;
SCHEME 8
ON SELECTION POPUP ayu2 do choice2
ACTIVATE POPUP ayu2
DEACTIVATE POPUP ayu1
DEACTIVATE POPUP ayu2
RETURN
*
PROCEDURE choice2
IF LASTKEY() == 13
     keyboard &clave
     DEACTIVATE POPUP ayu2
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

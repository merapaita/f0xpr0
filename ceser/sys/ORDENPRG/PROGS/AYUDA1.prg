*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER campo, mensaje, clave
ON KEY
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'BBB', 'ESC'
define popup ayu0 from 0,35 to 8,79 promp;
field &campo shadow title mensaje 
ON SELECTION POPUP ayu0 do choice0
ACTIVATE POPUP ayu0 NOWAIT
FOR i = 1 TO 5
     MOVE POPUP ayu0 BY 1, 0
ENDFOR
FOR i = 1 TO 9
     MOVE POPUP ayu0 BY 0, -2
ENDFOR
ACTIVATE POPUP ayu0
DEACTIVATE POPUP ayu0
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE choice0
IF LASTKEY() == 13
     keyboard &clave
     DEACTIVATE POPUP ayu0
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

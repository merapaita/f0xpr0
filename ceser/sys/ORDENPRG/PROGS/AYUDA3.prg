*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER campo, mensaje, clave
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'ESP', 'BBB', 'ESC'
define popup ayu0 from 0,35 to 14,79 promp;
field &campo shadow title mensaje
ON SELECTION POPUP ayu0 do choice0
ACTIVATE POPUP ayu0 NOWAIT
FOR i = 1 TO 24
     IF (i > 10) .AND. (WROWS() >  ;
        40)
          MOVE POPUP ayu0 BY 1, 0
     ENDIF
ENDFOR
FOR i = 1 TO 16
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
IF LASTKEY() == 32
     IF SUBSTR(codcla, 1, 1) =  ;
        'û'
          REPLACE codcla WITH  ;
                  SPACE(4)
     ELSE
          REPLACE codcla WITH  ;
                  'û   '
     ENDIF
     KEYBOARD '{DNARROW}'
ENDIF
IF LASTKEY() == 13
     DEACTIVATE POPUP ayu0
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

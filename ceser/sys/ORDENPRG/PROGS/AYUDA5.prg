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
op( 1) = 'Ordenados por Numero'
op( 2) = 'Ordenados por Fecha '
DEFINE POPUP ayu1 FROM 0, 0 TO 3,  ;
       21 SHADOW
FOR i = 1 TO 2
     DEFINE BAR i OF ayu1 PROMPT  ;
            op(i)
ENDFOR
ON SELECTION POPUP ayu1 do choice with;
prompt()
ACTIVATE POPUP ayu1 NOWAIT
FOR i = 0 TO 08
     MOVE POPUP ayu1 TO i, 0
ENDFOR
FOR i = 0 TO 28
     MOVE POPUP ayu1 TO 08, i
ENDFOR
ACTIVATE POPUP ayu1
DEACTIVATE POPUP ayu1
ACTIVATE WINDOW trabajo
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE choice
PARAMETER x, clave
IF LASTKEY() <> 13
     RETURN
ENDIF
FOR i = 1 TO 19
     MOVE POPUP ayu1 BY 0, -1
ENDFOR
FOR i = 1 TO 2
     IF op(i) = x
          EXIT
     ENDIF
ENDFOR
DEFINE WINDOW ayu3 FROM 0, 50 TO  ;
       2, 79 SHADOW
ACTIVATE WINDOW TOP ayu3
wk_numx = 0
wk_fecx = CTOD('')
IF i = 1
     SET ORDER TO 1
     campox = campo1
     @ 0, 1 SAY 'Numero Desde :'  ;
       GET wk_numx PICTURE  ;
       '99999999'
ELSE
     SET ORDER TO 2
     campox = campo1
     @ 0, 1 SAY 'Fecha  Desde :'  ;
       GET wk_fecx
ENDIF
FOR j = 1 TO 10
     ZOOM WINDOW ayu3 NORM FROM j,  ;
          50 TO 2 + j, 79
ENDFOR
FOR j = 1 TO 16
     ZOOM WINDOW ayu3 NORM FROM  ;
          10, 50 - j TO 12, 79 -  ;
          j
ENDFOR
READ
IF LASTKEY() == 27
     DEACTIVATE POPUP ayu1
     DEACTIVATE WINDOW ayu3
     RETURN
ENDIF
IF i = 1
     wk_inix = STR(wk_numx, 8)
     wk_finx = '99999999'
ELSE
     wk_inix = wk_fecx
     wk_finx = CTOD('31/12/99')
ENDIF
DEFINE WINDOW ayu4 FROM 6, 6 TO 7,  ;
       70 SHADOW
wk_color = color5 + ',,,,,' +  ;
           SUBSTR(color5, AT(',',  ;
           color5) + 1)
browse field cer = " " :H="", uno = &campox;
:H="  Numero  Fecha     Emisor  Entidad  Marca Modelo           Est";
 key wk_inix, wk_finx in window ayu4 nowait;
 freeze cer    
ACTIVATE WINDOW TOP ayu4
FOR j = 1 TO 08
     ZOOM WINDOW ayu4 NORM FROM 6,  ;
          6 TO 7 + j, 70
     ZOOM WINDOW or_isrep NORM  ;
          FROM -1, -4 TO j, 70
ENDFOR
DEACTIVATE POPUP ayu1
DEACTIVATE WINDOW ayu3
ON KEY LABEL ENTER do choice2
BROWSE LAST KEY wk_inix, wk_finx
ON KEY LABEL ENTER
DEACTIVATE WINDOW ayu4
RETURN
*
PROCEDURE choice2
IF LASTKEY() == 13
     wk_aux = ALLTRIM(numdoc)
     IF LEN(wk_aux) < 8
          wk_aux = wk_aux +  ;
                   CHR(13)
     ENDIF
     KEYBOARD CHR(27) + wk_aux
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

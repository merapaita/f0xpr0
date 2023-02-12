*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER campo1, doc
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'BBB', 'ESC'
DO choice
ACTIVATE WINDOW trabajo
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE choice
DEFINE WINDOW ayu3 FROM 0, 50 TO  ;
       2, 79 SHADOW
ACTIVATE WINDOW TOP ayu3
wk_numx = 0
wk_fecx = CTOD('')
SET ORDER TO 1
campox = campo1
@ 0, 1 SAY 'Numero Desde :' GET  ;
  wk_numx PICTURE '99999999'
FOR j = 1 TO 12
     ZOOM WINDOW ayu3 NORM FROM j,  ;
          50 TO 2 + j, 79
ENDFOR
FOR j = 1 TO 30
     ZOOM WINDOW ayu3 NORM FROM  ;
          12, 50 - j TO 14, 79 -  ;
          j
ENDFOR
READ
IF LASTKEY() == 27
     DEACTIVATE WINDOW ayu3
     RETURN
ENDIF
wk_inix = doc + STR(wk_numx, 8)
wk_finx = '99999999'
DEFINE WINDOW ayu4 FROM 6, 6 TO 7,  ;
       70 SHADOW
ACTIVATE WINDOW TOP ayu4
ventana = SUBSTR(DBF(),  ;
          LEN(DBF()) - 11, 8)
FOR j = 1 TO 08
     ZOOM WINDOW ayu4 NORM FROM 6,  ;
          6 TO 7 + j, 70
ENDFOR
SEEK wk_inix
ON KEY LABEL ENTER do choice2
browse field cer = " " :H="", uno = &campo1;
:H="  Numero   Fch Emis.        O/R        S/S  C.Pago   Estado";
 in window ayu4   freeze cer REST;
 
DEACTIVATE WINDOW ayu3
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

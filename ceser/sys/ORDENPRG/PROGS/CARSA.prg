*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
ind_prg = '<CARSA>'
tit_prg = ' CONSULTA '
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' GARANTIA CARSA DE 4 A¥OS '
DO esc_indica WITH 1, 'AYU',  ;
   'BUS', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
CLOSE DATABASES
SELECT 1
USE SHARED GT_CARSA ORDER codigo
DEFINE WINDOW ingreso FROM 06, 04  ;
       TO 15, 71
DO WHILE .T.
     ACTIVATE WINDOW indicar
     SAVE SCREEN TO wk_pantax
     ACTIVATE WINDOW trabajo
     DIMENSION op( 3)
     op( 1) =  ;
       'Ordenados por Cliente'
     op( 2) =  ;
       'Ordenados por Modelo '
     op( 3) =  ;
       'Ordenados por Fecha'
     DEFINE POPUP ayu1 FROM 0, 0  ;
            TO 4, 21 SHADOW
     FOR i = 1 TO 3
          DEFINE BAR i OF ayu1  ;
                 PROMPT op(i)
     ENDFOR
     ON SELECTION POPUP ayu1 do choice_4;
with prompt()
     ACTIVATE POPUP ayu1 NOWAIT
     FOR i = 0 TO 08
          MOVE POPUP ayu1 TO i, 0
     ENDFOR
     FOR i = 0 TO 28
          MOVE POPUP ayu1 TO 08,  ;
               i
     ENDFOR
     ACTIVATE POPUP ayu1
     DEACTIVATE POPUP ayu1
     ACTIVATE WINDOW trabajo
     ACTIVATE WINDOW indicar
     RESTORE SCREEN FROM  ;
             wk_pantax
     ACTIVATE WINDOW trabajo
     IF LASTKEY() = 27
          EXIT
     ENDIF
ENDDO
CLOSE DATABASES
DEACTIVATE WINDOW ingreso
DO saca_win
*
PROCEDURE choice_4
PARAMETER opc
IF LASTKEY() <> 13
     RETURN
ENDIF
FOR i = 1 TO 19
     MOVE POPUP ayu1 BY 0, -1
ENDFOR
FOR i = 1 TO 3
     IF op(i) = opc
          EXIT
     ENDIF
ENDFOR
define window ayu3 from 0,40 to 2,79 shadow;
color &color5 
ACTIVATE WINDOW TOP ayu3
DO CASE
     CASE BAR() = 1
          SET ORDER TO 2
          wrk_codigo = SPACE(30)
          SET CURSOR ON
          @ 0, 1 SAY  ;
            'Nombre de Cliente :'  ;
            GET wrk_codigo  ;
            PICTURE '@!'
     CASE BAR() = 2
          SET ORDER TO 1
          wrk_codigo = SPACE(12)
          @ 0, 1 SAY 'Modelo :'  ;
            GET wrk_codigo  ;
            PICTURE '@!'
     CASE BAR() = 3
          SET ORDER TO 3
          wrk_codigo = SPACE(8)
          @ 0, 1 SAY 'Fecha :'  ;
            GET wrk_codigo  ;
            PICTURE '@!'
ENDCASE
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
SET NEAR ON
SEEK wrk_codigo
SET NEAR OFF
define window ayu4 from 6,6 to 7,70 shadow;
color &color5
IF ISCOLOR()
     wk_color = color5 + ',,,,,' +  ;
                SUBSTR(color5,  ;
                AT(',', color5) +  ;
                1)
ELSE
     wk_color = color5
ENDIF
ACTIVATE WINDOW ingreso
BROWSE FIELDS nom_modelo :H =  ;
       'Modelo', nom_client :H =  ;
       'Cliente', tip_docum :H =  ;
       'TD', nro_docum :H =  ;
       'Nro.Doc', fch_docum :H =  ;
       'Fch.Vta' NOEDIT TITLE  ;
       'GARANTIA DE 4 A¥OS'
DEACTIVATE POPUP ayu1
DEACTIVATE WINDOW ayu3
RETURN
*
PROCEDURE choice24
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

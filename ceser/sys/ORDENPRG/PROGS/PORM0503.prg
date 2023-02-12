*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORM0503>'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
set color to &color3
@ 2, 1 SAY DATE()
set color to &color1
DO saycenter WITH 1,  ;
   ' ADMINISTRACION '
DO saycenter WITH 2,  ;
   ' GENERADOR ARCHIVO LOTUS'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'BBB', 'ESC'
CLOSE DATABASES
SELECT 1
USE campofox
REPLACE codcla WITH '  ' ALL
cam = 'subst(campo1,1,40)'
men = 'Ayuda de Archivos'
DO ayuda1 WITH cam, men, 'codcla'
wk_tec = LASTKEY()
wk_aux = INKEY()
DO WHILE wk_aux<>0
     wk_aux = INKEY()
ENDDO
tot_reg = 0
IF wk_tec == 13
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'SEL'
     DO esc_indica WITH 2, 'MBV',  ;
        'ESP', 'BBB', 'ESC'
     arch = SUBSTR(campo1, 1, 12)
     use &arch
     tot_reg = RECCOUNT()
     DIMENSION a( 1)
     wk_aux = AFIELDS(a)
     set color to &color5
     DEFINE POPUP ayu0 FROM 4, 1  ;
            TO 10, 40 TITLE  ;
            'ARCHIVO ' + arch  ;
            SHADOW IN trabajo
     FOR i = 1 TO wk_aux
          a( i, 1) = '  ' + a(i, ;
           1)
          DEFINE BAR i OF ayu0  ;
                 PROMPT a(i,1) +  ;
                 '  ' + a(i,2) +  ;
                 '  ' + STR(a(i, ;
                 3), 4) + '  ' +  ;
                 STR(a(i,4), 2) +  ;
                 '  ' + IIF(a(i, ;
                 2) == 'C',  ;
                 'Caracter',  ;
                 IIF(a(i,2) ==  ;
                 'N', 'Numerico',  ;
                 IIF(a(i,2) ==  ;
                 'D', 'Fecha',  ;
                 'Memorandum')))
     ENDFOR
     ON SELECTION POPUP ayu0 do porm0531;
with bar()
     ACTIVATE POPUP ayu0
     ACTIVATE POPUP ayu0 NOWAIT
ENDIF
IF tot_reg <> 0 .AND. LASTKEY() ==  ;
   13 .AND. wk_tec == 13
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     define window ayu1 from 4,49 to 10,72;
in window trabajo color &color5 shadow
     ACTIVATE WINDOW TOP ayu1
     @ 0, 2 SAY 'Inicio  : ' +  ;
       TIME()
     @ 2, 2 SAY 'Termino : '
     @ 4, 2 SAY  ;
       '% Total : 000.00'
     SET HOURS TO 24
     SET CLOCK TO 9, 63
     ACTIVATE WINDOW trabajo
     @ 12, 10 CLEAR TO 14, 62
     @ 12, 10 TO 14, 62
     set color to &color1
     @ 13, 7 SAY '0%'
     @ 13, 64 SAY '100%'
     set color to &color5
     tot_act = 0
     tot_por = 0
     wk_camp = ''
     FOR i = 1 TO wk_aux
          IF SUBSTR(a(i,1), 1, 2) ==  ;
             'û '
               wk_camp = wk_camp +  ;
                         SUBSTR(a(i, ;
                         1), 3,  ;
                         10) +  ;
                         ','
          ENDIF
     ENDFOR
     wk_camp = SUBSTR(wk_camp, 1,  ;
               LEN(wk_camp) - 1)
     use &arch
     copy to lotus.wks fields &wk_camp;
for copia() type wk1   
     CLOSE DATABASES
     set color to &color1
ENDIF
DEACTIVATE POPUP ayu0
DEACTIVATE WINDOW ayu1
CLOSE DATABASES
wk_aux = INKEY()
DO WHILE wk_aux<>0
     wk_aux = INKEY()
ENDDO
SET CLOCK OFF
DO saca_win
RETURN
*
PROCEDURE porm0531
PARAMETER wk_i
IF LASTKEY() == 13
     DEACTIVATE POPUP ayu0
     ACTIVATE POPUP ayu0 NOWAIT
     RETURN
ENDIF
IF SUBSTR(a(wk_i,1), 1, 2) ==  ;
   'û '
     a( wk_i, 1) = '  ' +  ;
      SUBSTR(a(wk_i,1), 3)
ELSE
     a( wk_i, 1) = 'û ' +  ;
      SUBSTR(a(wk_i,1), 3)
ENDIF
DEFINE BAR wk_i OF ayu0 PROMPT  ;
       a(wk_i,1) + '  ' + a(wk_i, ;
       2) + '  ' + STR(a(wk_i,3),  ;
       4) + '  ' + STR(a(wk_i,4),  ;
       2) + '  ' + IIF(a(wk_i,2) ==  ;
       'C', 'Caracter',  ;
       IIF(a(wk_i,2) == 'N',  ;
       'Numerico', IIF(a(wk_i,2) ==  ;
       'D', 'Fecha',  ;
       'Memorandum')))
KEYBOARD '{DNARROW}'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

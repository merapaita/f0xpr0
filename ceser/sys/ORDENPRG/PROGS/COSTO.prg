*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW marco FROM 05, 05  ;
       TO 20, 75 TITLE  ;
       'ACTUALIZACION DE C.A.R.'  ;
       DOUBLE
CLOSE DATABASES
USE SHARED GC_PRO00 ORDER CODIGO
ACTIVATE WINDOW marco
@ 02, 02 SAY 'C¢digo :'
@ 04, 02 SAY 'C.A.R. :'
DO WHILE .T.
     STORE SPACE(8) TO wrk_codigo
     STORE 0 TO wrk_car
     @ 02, 11 GET wrk_codigo
     READ
     IF LASTKEY() = 27
          DEACTIVATE WINDOW marco
          CLOSE DATABASES
          CLEAR ALL
          EXIT
     ENDIF
     SEEK wrk_codigo
     IF  .NOT. FOUND()
          @ 10, 22 SAY  ;
            '<<<<<  C¢digo no Existe  >>>>>'  ;
            COLOR B/W 
          = INKEY(0, 'H')
          @ 10, 22 SAY SPACE(30)
          LOOP
     ENDIF
     STORE pro_coremo TO wrk_car
     @ 02, 28 SAY pro_descri
     @ 04, 11 GET wrk_car PICTURE  ;
       '999,999.99'
     READ
     IF LASTKEY() = 27
          LOOP
     ENDIF
     REPLACE pro_coremo WITH  ;
             wrk_car
     REPLACE pro_cosrep WITH  ;
             DATE()
ENDDO
*
*** 
*** ReFox - retrace your steps ... 
***

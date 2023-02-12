*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
@ 10, 20 TO 20, 60 DOUBLE
a = 0
SELECT 1
USE ST_IOREP ORDER CODIGO
SET FILTER TO YEAR(fecemi) = 1994
SELECT 2
USE ST_MVORD
SELECT 1
DO WHILE  .NOT. EOF()
     a = a + 1
     @ 15, 30 SAY 'REGISTRO => ' +  ;
       STR(a, 6)
     wrk_numord = numdoc
     wrk_estord = auxest
     wrk_tecni = codtec
     IF indest = 'F   ' .OR.  ;
        indest = 'B   '
          SELECT 2
          SET FILTER TO orden = wrk_numord
          GOTO BOTTOM
          IF wrk_estord = estado
               SET FILTER TO
               SELECT 1
               SKIP
               LOOP
          ELSE
               APPEND BLANK
               REPLACE dia WITH  ;
                       CTOD( ;
                       '31/12/95' ;
                       ), hora  ;
                       WITH  ;
                       '12:00:00',  ;
                       orden WITH  ;
                       wrk_numord,  ;
                       estado  ;
                       WITH  ;
                       '100 ',  ;
                       tecnico  ;
                       WITH  ;
                       wrk_tecni,  ;
                       destado  ;
                       WITH  ;
                       '** ENTREGADO Y FACTURADO ** '
               SET FILTER TO
               SELECT 1
               SKIP
          ENDIF
     ELSE
          SELECT 1
          SKIP
     ENDIF
ENDDO
CLOSE DATABASES
*
*** 
*** ReFox - retrace your steps ... 
***

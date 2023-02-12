*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
SET CONSOLE OFF
SELECT 1
USE st_isrep
SET FILTER TO codemi >= '200 ';
.AND. codemi <= '229 '
GOTO TOP
SELECT 2
USE st_iorep ORDER ord_numsol
@ 05, 05 TO 15, 50 DOUBLE
a = 0
SELECT 1
DO WHILE  .NOT. EOF()
     a = a + 1
     @ 10, 20 SAY a
     wrk_numsol = numdoc
     wrk_coddes = coddes
     IF wrk_coddes = 'D'
          SELECT 2
          SEEK wrk_numsol
          IF FOUND()
               DO rbloquea
               REPLACE codtall  ;
                       WITH  ;
                       '004 '
               UNLOCK
          ENDIF
     ENDIF
     SELECT 1
     SKIP
ENDDO
WAIT WINDOW 'PROCESO TERMINADO'
*
*** 
*** ReFox - retrace your steps ... 
***

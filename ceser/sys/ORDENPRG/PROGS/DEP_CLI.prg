*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
CLOSE DATABASES
SELECT 1
USE ST_ISREP ORDER CODENT
SELECT 2
USE ST_ICLPR
a = 0
b = 0
SCAN WHILE  .NOT. EOF()
     a = a + 1
     @ 10, 40 SAY a
     w_cliente = codent
     IF indent = 'C'
          SELECT st_isrep
          SEEK w_cliente
          IF  .NOT. FOUND()
               SELECT st_iclpr
               DELETE
               b = b + 1
               @ 10, 20 SAY b
          ENDIF
          SELECT st_iclpr
     ENDIF
ENDSCAN
WAIT WINDOW 'PROCESO TERMINADO'
CLOSE DATABASES
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
SET EXCLUSIVE OFF
SELECT 1
USE st_iorep ALIAS st_iorep
SET ORDER TO numpre
SELECT 2
USE st_ispre ALIAS st_ispre
SET ORDER TO 1
SELECT st_iorep
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT st_ispre
     SEEK st_iorep.numpre
     IF FOUND()
          REPLACE numord WITH  ;
                  st_iorep.numdoc
          ? numord +  ;
            st_iorep.numdoc
          WAIT ''
     ENDIF
     SELECT st_iorep
ENDSCAN
*
*** 
*** ReFox - retrace your steps ... 
***

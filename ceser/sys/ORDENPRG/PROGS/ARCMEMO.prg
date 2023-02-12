*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
*
PROCEDURE actmemo
CLOSE DATABASES
SELECT 1
USE SHARED isrep ORDER 1
SELECT 2
USE SHARED st_isrep ORDER 1
SELECT isrep
GOTO TOP
SET SYSMENU ON
SCAN WHILE  .NOT. EOF()
     SELECT st_isrep
     SEEK isrep.numdoc
     IF FOUND()
          DO rbloquea
          REPLACE observ WITH  ;
                  isrep.observ,  ;
                  desace WITH  ;
                  isrep.desace
          ? 'reemplazando memo a' +  ;
            isrep.numdoc
          UNLOCK
     ENDIF
     SELECT isrep
ENDSCAN
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

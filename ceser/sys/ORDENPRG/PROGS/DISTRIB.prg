*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT 1
USE st_iclpr
SELECT 2
USE GE_TAB0
SELECT 3
USE st_iorep ORDER ord_codent
SET NEAR ON
SEEK ' 33756437'
REPORT FORMAT DISTRIB TO FILE  ;
       DISTRIB.TXT FOR codent =  ;
       ' 33756437' .AND. fecent =  ;
       CTOD('  /  /  ')
MODIFY COMMAND DISTRIB.TXT
CLOSE DATABASES
*
*** 
*** ReFox - retrace your steps ... 
***

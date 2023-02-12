*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
USE IN 1 st_iclpr ORDER noment
USE IN 2 st_iorep
SELECT st_iorep
SET ORDER TO codent
SELECT st_iclpr
SET RELATION TO codent INTO st_iorep
BROWSE FOR (st_iorep.numdoc <>  ;
       SPACE(8) .AND. indent =  ;
       'C') FIELDS  ;
       st_iorep.numdoc,  ;
       st_iorep.fecemi, noment,  ;
       st_iorep.codmod,  ;
       st_iorep.numser
RETURN
SELECT st_iorep
BROWSE NOWAIT
SELECT st_iclpr
BROWSE NOWAIT
RETURN
CLOSE DATABASES
USE IN 1 st_iorep
USE IN 2 st_iclpr
SELECT st_iclpr
SET ORDER TO codent
SELECT st_iorep
SET RELATION TO codent INTO st_iclpr
SELECT st_iclpr
BROWSE NOWAIT
SELECT st_iorep
BROWSE NOWAIT
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.fecfin,  ;
       st_iorep.codemi,  ;
       st_iorep.indori,  ;
       st_iorep.indest,  ;
       st_iorep.auxest,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iorep.numser,  ;
       st_iorep.numsol,  ;
       st_iorep.codtall FROM  ;
       ST_IOREP WHERE  ;
       MONTH(st_iorep.fecemi) = 1  ;
       AND YEAR(st_iorep.fecemi) =  ;
       1996 AND st_iorep.indori =  ;
       'GARA' AND st_iorep.indest <>  ;
       'N' AND (st_iorep.codmar =  ;
       '01  ' OR st_iorep.codmar =  ;
       '37  ') ORDER BY  ;
       st_iorep.codmar,  ;
       st_iorep.codtall,  ;
       st_iorep.codmod
*
*** 
*** ReFox - retrace your steps ... 
***

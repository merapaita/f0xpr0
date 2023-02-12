*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iorep.cosrep,  ;
       st_iorep.cosmob,  ;
       st_imode.linea FROM  ;
       ST_IOREP, ST_IMODE WHERE  ;
       st_imode.codmod =  ;
       st_iorep.codmod AND  ;
       (YEAR(st_iorep.fecemi) =  ;
       1995 AND st_iorep.indest <>  ;
       'N' AND  ;
       BETWEEN(st_iorep.codemi,  ;
       '200 ', '229 "') AND  ;
       (st_iorep.indest = 'F' OR  ;
       st_iorep.indest = 'B') AND  ;
       st_iorep.indori = 'GARA')  ;
       ORDER BY st_imode.linea,  ;
       st_iorep.codmar
*
*** 
*** ReFox - retrace your steps ... 
***

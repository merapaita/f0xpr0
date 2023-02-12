*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT DISTINCT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.indori,  ;
       st_iorep.observ,  ;
       st_iorep.numsol,  ;
       st_imode.codmar,  ;
       st_imode.codmod,  ;
       st_imode.codcla,  ;
       st_imode.linea,  ;
       st_issre.codsin,  ;
       st_iorep.codtall,  ;
       MONTH(st_iorep.fecemi),  ;
       st_iorep.numser,  ;
       st_iorep.cosrep,  ;
       st_iorep.cosmob FROM  ;
       ST_IOREP, ST_IMODE,  ;
       ST_ISSRE WHERE  ;
       st_iorep.codmar =  ;
       st_imode.codmar AND  ;
       st_iorep.codmod =  ;
       st_imode.codmod AND  ;
       st_issre.numdoc =  ;
       st_iorep.numsol AND  ;
       st_iorep.codemi = '202 '  ;
       AND YEAR(st_iorep.fecemi) =  ;
       1995 AND st_iorep.indest <>  ;
       'A' ORDER BY  ;
       st_imode.codcla,  ;
       st_imode.codmod
*
*** 
*** ReFox - retrace your steps ... 
***

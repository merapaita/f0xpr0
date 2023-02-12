*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.indori,  ;
       st_iorep.indest,  ;
       st_iorep.auxest,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iorep.numsol,  ;
       st_iorep.codtall,  ;
       st_iorep.fecest FROM  ;
       ST_IOREP WHERE  ;
       st_iorep.indest <> 'N' AND  ;
       st_iorep.numdoc <>  ;
       SPACE(8) AND  ;
       st_iorep.codtall <= '009 '  ;
       AND (st_iorep.auxest <  ;
       '011 ' OR st_iorep.auxest =  ;
       '021 ' OR st_iorep.auxest =  ;
       '026 ' OR st_iorep.auxest =  ;
       '080 ') ORDER BY  ;
       st_iorep.auxest
*
*** 
*** ReFox - retrace your steps ... 
***

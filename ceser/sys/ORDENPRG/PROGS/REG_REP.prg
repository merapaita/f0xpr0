*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.codent,  ;
       st_iorep.indori,  ;
       st_iorep.indest,  ;
       st_iorep.auxest,  ;
       st_iorep.codmod,  ;
       st_iorep.numser,  ;
       st_iorep.numsol,  ;
       st_iorep.cosrep,  ;
       st_iorep.cosmob,  ;
       st_iorep.fecest,  ;
       st_iorep.codtall,  ;
       st_idped.codpro,  ;
       st_idped.canpro,  ;
       st_idped.valpro FROM  ;
       ST_IOREP, ST_IDPED WHERE  ;
       st_iorep.numdoc =  ;
       st_idped.numord AND  ;
       (st_iorep.auxest = '021 '  ;
       OR st_iorep.auxest =  ;
       '022 ' OR st_iorep.auxest =  ;
       '025 ' OR st_iorep.auxest =  ;
       '028 ' OR st_iorep.auxest =  ;
       '029 ') AND  ;
       st_iorep.indest <> 'F' AND  ;
       st_iorep.indest <> 'B' AND  ;
       st_iorep.cosrep > 0 AND  ;
       MONTH(st_iorep.fecest) = 1  ;
       AND YEAR(st_iorep.fecest) =  ;
       1996
*
*** 
*** ReFox - retrace your steps ... 
***

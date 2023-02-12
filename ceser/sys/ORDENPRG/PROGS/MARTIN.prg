*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DO proc2
RETURN
*
PROCEDURE proc1
SELECT DISTINCT st_iorep.numdoc,  ;
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
       st_iorep.cosrep,  ;
       st_iorep.cosmob,  ;
       st_iorep.codtall,  ;
       st_mvord.estado,  ;
       st_mvord.destado,  ;
       st_mvord.dia FROM ST_IOREP,  ;
       ST_MVORD WHERE  ;
       st_mvord.orden =  ;
       st_iorep.numdoc AND  ;
       (YEAR(st_iorep.fecemi) >  ;
       1994 AND st_iorep.indest <>  ;
       'N' AND st_iorep.indest <>  ;
       'F' AND st_iorep.indest <>  ;
       'B' AND  ;
       BETWEEN(st_iorep.codemi,  ;
       '200 ', '299 ') AND  ;
       st_iorep.indest <> 'C')  ;
       ORDER BY st_iorep.codmod
RETURN
*
PROCEDURE proc2
SELECT DISTINCT st_iorep.numdoc,  ;
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
       st_iorep.cosrep,  ;
       st_iorep.cosmob,  ;
       st_iorep.codtall FROM  ;
       ST_IOREP WHERE  ;
       (YEAR(st_iorep.fecemi) >  ;
       1994 AND st_iorep.indest <>  ;
       'N' AND st_iorep.indest <>  ;
       'F' AND st_iorep.indest <>  ;
       'B' AND  ;
       BETWEEN(st_iorep.codemi,  ;
       '200 ', '299 ') AND  ;
       st_iorep.indest <> 'C')  ;
       ORDER BY st_iorep.codmod
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

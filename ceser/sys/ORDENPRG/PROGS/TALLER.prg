*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT DISTINCT st_iorep.numdoc,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iorep.numsol,  ;
       st_iorep.numser,  ;
       st_iorep.fecemi,  ;
       st_iorep.indest,  ;
       st_iorep.indori,  ;
       st_iorep.codtall,  ;
       st_iseri.codent,  ;
       st_iorep.codfabo,  ;
       st_iorep.cosrep,  ;
       st_iorep.cosmob,  ;
       st_iorep.fecest FROM  ;
       ST_ISERI, ST_IOREP WHERE  ;
       st_iorep.numser =  ;
       st_iseri.numser AND  ;
       st_iorep.codmar =  ;
       st_iseri.codmar AND  ;
       st_iorep.codmod =  ;
       st_iseri.modelo AND  ;
       st_iorep.indest = 'C' AND  ;
       st_iorep.indori = 'GARA'  ;
       AND YEAR(st_iorep.fecemi) >  ;
       1994 AND st_iseri.codent =  ;
       ' 25360443' ORDER BY  ;
       st_iorep.codtall,  ;
       st_iorep.fecemi,  ;
       st_iorep.numdoc INTO  ;
       CURSOR FAC_PROV
*
*** 
*** ReFox - retrace your steps ... 
***

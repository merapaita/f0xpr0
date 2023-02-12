*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.codent,  ;
       st_iorep.indori,  ;
       st_iorep.indest,  ;
       st_iorep.codtall,  ;
       st_iorep.numsol,  ;
       st_iorep.fecest FROM  ;
       ST_IOREP WHERE  ;
       st_iorep.fecemi >= DATE() -  ;
       15 AND st_iorep.fecemi <=  ;
       DATE() - 9 AND  ;
       st_iorep.indest <> 'N' AND  ;
       BETWEEN(st_iorep.codtall,  ;
       '003', '004') ORDER BY  ;
       st_iorep.codtall,  ;
       st_iorep.fecemi,  ;
       st_iorep.numdoc INTO  ;
       CURSOR GARA
*
*** 
*** ReFox - retrace your steps ... 
***

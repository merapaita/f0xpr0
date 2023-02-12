*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
fecha1 = DATE() - 57
fecha2 = DATE() - 27
SELECT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.codtall,  ;
       st_iorep.codent,  ;
       st_iorep.indori,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iorep.numser,  ;
       st_iorep.codtec,  ;
       st_iorep.numsol,  ;
       st_iorep.auxest FROM  ;
       ST_IOREP WHERE  ;
       (st_iorep.fecemi >= fecha1  ;
       AND st_iorep.fecemi <=  ;
       fecha2) AND  ;
       st_iorep.indest <> 'N   '  ;
       AND  ;
       BETWEEN(st_iorep.codtall,  ;
       '003', '004') ORDER BY  ;
       st_iorep.codtall,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.indori,  ;
       st_iorep.numdoc INTO  ;
       CURSOR ORDEN
*
*** 
*** ReFox - retrace your steps ... 
***

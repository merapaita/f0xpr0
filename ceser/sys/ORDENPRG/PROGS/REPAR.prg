*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.indori,  ;
       st_iorep.indest,  ;
       st_iorep.auxest,  ;
       st_iorep.fecest,  ;
       st_iorep.codmod,  ;
       st_iorep.codtall,  ;
       st_mvord.estado,  ;
       st_mvord.dia,  ;
       st_mvord.orden,  ;
       st_mvord.destado,  ;
       st_iorep.numsol,  ;
       st_iorep.codmar,  ;
       st_iorep.codtec,  ;
       st_iorep.codent,  ;
       st_iorep.observ,  ;
       st_iorep.numser,  ;
       MONTH(st_mvord.dia) FROM  ;
       ST_IOREP, ST_MVORD WHERE  ;
       st_mvord.orden =  ;
       st_iorep.numdoc AND  ;
       (st_mvord.estado = '008 '  ;
       AND st_iorep.indest <> 'N'  ;
       AND  ;
       BETWEEN(MONTH(st_mvord.dia),  ;
       9, 10) AND  ;
       YEAR(st_mvord.dia) = 1995)  ;
       ORDER BY 20,  ;
       st_iorep.codemi,  ;
       st_iorep.indori INTO  ;
       CURSOR REPAR
*
*** 
*** ReFox - retrace your steps ... 
***

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
       st_iorep.fecest FROM  ;
       ST_IOREP WHERE  ;
       st_iorep.auxest <= '010 '  ;
       AND st_iorep.indest <>  ;
       'N   ' AND st_iorep.indest <>  ;
       'F   ' AND st_iorep.indest <>  ;
       'B   ' ORDER BY  ;
       st_iorep.codemi,  ;
       st_iorep.auxest,  ;
       st_iorep.indori
REPORT FORMAT PORL4073 TO FILE  ;
       TEXT2.TXT
*
*** 
*** ReFox - retrace your steps ... 
***

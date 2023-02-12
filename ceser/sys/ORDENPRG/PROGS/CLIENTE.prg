*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.indest,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iorep.numser,  ;
       st_iorep.numsol,  ;
       st_iclpr.noment FROM  ;
       ST_IOREP, ST_ICLPR WHERE  ;
       st_iclpr.codent =  ;
       st_iorep.codent AND  ;
       st_iclpr.indent = 'C'  ;
       ORDER BY st_iclpr.noment  ;
       INTO DBF  ;
       \BASES\CLIENTE.DBF
BROWSE
*
*** 
*** ReFox - retrace your steps ... 
***

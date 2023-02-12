*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT st_iprep.numdoc,  ;
       st_iprep.numord,  ;
       st_iprep.indest,  ;
       st_idped.codpro,  ;
       st_idped.canpro,  ;
       st_idped.valpro,  ;
       st_idped.totite FROM  ;
       ST_IPREP, ST_IDPED WHERE  ;
       st_idped.numdoc =  ;
       st_iprep.numdoc AND  ;
       (st_iprep.numord =  ;
       '   16844' AND  ;
       st_idped.canpro > 0 AND  ;
       st_iprep.indest <> 'N')  ;
       ORDER BY st_iprep.numdoc  ;
       INTO CURSOR TIPOS
*
*** 
*** ReFox - retrace your steps ... 
***

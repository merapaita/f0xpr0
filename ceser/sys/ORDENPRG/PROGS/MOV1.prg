*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
STORE SPACE(80) TO wrk_dessin
SELECT DISTINCT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.fecfin,  ;
       st_iorep.codemi,  ;
       st_iorep.codent,  ;
       st_iorep.indori,  ;
       st_iorep.auxest,  ;
       st_iorep.indest,  ;
       st_iorep.codmod,  ;
       st_iorep.numser,  ;
       st_iorep.numsol,  ;
       st_iorep.fecest,  ;
       st_iprep.indest,  ;
       st_isrep.fecemi,  ;
       st_issre.codsin,  ;
       st_idped.codpro,  ;
       st_iorep.observ,  ;
       st_iorep.codmar,  ;
       st_idped.canpro,  ;
       st_idped.valpro,  ;
       st_idped.totite,  ;
       st_iorep.cosrep,  ;
       st_iorep.cosmob,  ;
       st_idped.numdoc,  ;
       st_imode.codcla FROM  ;
       ST_ISREP, ST_IOREP,  ;
       ST_ISSRE, ST_IDPED,  ;
       ST_IPREP, ST_IMODE WHERE  ;
       st_iorep.numsol =  ;
       st_isrep.numdoc AND  ;
       st_issre.numdoc =  ;
       st_isrep.numdoc AND  ;
       st_imode.codmod =  ;
       st_iorep.codmod AND  ;
       st_idped.numord =  ;
       st_iorep.numdoc AND  ;
       st_iprep.numord =  ;
       st_iorep.numdoc AND  ;
       MONTH(st_isrep.fecemi) > 0  ;
       AND YEAR(st_isrep.fecemi) =  ;
       1995 AND st_iprep.indest <>  ;
       'N' AND (st_iorep.indest =  ;
       'C' OR st_iorep.indest =  ;
       'F' OR st_iorep.indest =  ;
       'B') AND  ;
       ALLTRIM(st_isrep.codemi) =  ;
       '100' ORDER BY  ;
       st_iorep.indori,  ;
       st_iorep.codmod,  ;
       st_iorep.fecemi,  ;
       st_iorep.numdoc,  ;
       st_idped.numdoc,  ;
       st_idped.codpro INTO  ;
       CURSOR QUERY
RETURN
*
PROCEDURE resumen
STORE {} TO fecha1
STORE {} TO fecha2
SELECT DISTINCT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.codent,  ;
       st_iorep.indori,  ;
       st_iorep.auxest,  ;
       st_iorep.codmod,  ;
       st_isrep.fecemi,  ;
       st_iorep.cosmob,  ;
       st_iorep.cosrep,  ;
       st_iorep.totnet FROM  ;
       ST_IOREP, ST_ISREP WHERE  ;
       st_iorep.numsol =  ;
       st_isrep.numdoc AND  ;
       (MONTH(st_isrep.fecemi) >  ;
       0 AND  ;
       YEAR(st_isrep.fecemi) =  ;
       1995 AND (st_iorep.indest =  ;
       'C' OR st_iorep.indest =  ;
       'F' OR st_iorep.indest =  ;
       'B') AND  ;
       ALLTRIM(st_isrep.codemi) =  ;
       '100') ORDER BY  ;
       st_iorep.indori,  ;
       st_iorep.codmod,  ;
       st_isrep.fecemi INTO  ;
       CURSOR QUERY
RETURN
*
FUNCTION oodessi
PARAMETER wrk_codcla, wrk_codsin
narea = SELECT()
= ooopen('ST_ISINT','CODIGO')
SEEK wrk_codcla + wrk_codsin
IF FOUND()
     wrk_dessin = ALLTRIM(wrk_dessin) +  ;
                  dessin
ENDIF
= ooclose('ST_ISINT')
SELECT (narea)
RETURN wrk_dessin
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
wrk_total = 0
SELECT 1
USE GE_TAB0
SELECT 2
SELECT st_isrep.numdoc,  ;
       st_isrep.fecemi,  ;
       st_isrep.codemi,  ;
       st_isrep.indori,  ;
       st_isrep.indest,  ;
       st_isrep.codmar,  ;
       st_isrep.codmod,  ;
       st_isrep.numser,  ;
       st_isrep.coddes,  ;
       st_iclpr.nomdis,  ;
       st_iclpr.nomciu,  ;
       st_iclpr.codcla,  ;
       st_iclpr.numte1 FROM  ;
       ST_ISREP, ST_ICLPR WHERE  ;
       st_iclpr.codent =  ;
       st_isrep.codent AND  ;
       YEAR(st_isrep.fecemi) =  ;
       1995 ORDER BY  ;
       st_iclpr.nomdis INTO DBF  ;
       J:\SISTEMAS\BASES\DIST.DBF
COUNT TO wrk_total
SET SYSMENU ON
MODIFY REPORT DIST
*
*** 
*** ReFox - retrace your steps ... 
***

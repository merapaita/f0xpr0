*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET SYSMENU ON
w_sint = '    '
SELECT 1
USE ST_SICLI ORDER CODIGO
SELECT 2
SELECT st_isrep.fecemi,  ;
       st_isrep.codemi,  ;
       st_isrep.indori,  ;
       st_isrep.indest,  ;
       st_isrep.codmar,  ;
       st_isrep.codmod,  ;
       st_isrep.numser,  ;
       st_isrep.coddes,  ;
       st_iorep.numdoc,  ;
       st_iorep.auxest,  ;
       st_iorep.cosrep,  ;
       st_iorep.cosmob,  ;
       st_iorep.flete,  ;
       st_iorep.subtot,  ;
       st_iorep.codtall,  ;
       st_iorep.numsol FROM  ;
       ST_ISREP, ST_IOREP WHERE  ;
       st_iorep.numsol =  ;
       st_isrep.numdoc AND  ;
       st_isrep.indest <> 'N' AND  ;
       YEAR(st_isrep.fecemi) =  ;
       1996 AND  ;
       MONTH(st_isrep.fecemi) =  ;
       10 AND  ;
       SUBSTR(st_isrep.codemi, 1,  ;
       1) = '2' AND  ;
       (st_isrep.indori = 'GARA'  ;
       OR st_isrep.indori =  ;
       'GREC') ORDER BY  ;
       st_iorep.codtall,  ;
       st_isrep.codemi,  ;
       st_isrep.codmod
REPORT FORMAT carsa50 TO FILE  ;
       carsa50.doc
*
FUNCTION sintoma
PARAMETER opc
SELECT 1
SET NEAR ON
SEEK opc
SET NEAR OFF
SCAN WHILE numdoc = opc
     w_sint = codsin
     IF SUBSTR(codsin, 2, 3) =  ;
        '561' .OR. SUBSTR(codsin,  ;
        2, 3) = '570'
          EXIT
     ENDIF
ENDSCAN
SELECT 2
RETURN w_sint
*
*** 
*** ReFox - retrace your steps ... 
***

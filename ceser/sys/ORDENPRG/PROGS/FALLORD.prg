*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DO conso
RETURN
*
PROCEDURE detalle
fecha1 = DATE() - 49
fecha2 = DATE() - 20
num1 = 1
SELECT DISTINCT st_iorep.numdoc,  ;
       st_iorep.codemi,  ;
       st_iorep.codent,  ;
       st_iorep.indori,  ;
       st_iorep.observ,  ;
       st_iorep.numsol,  ;
       st_imode.codmar,  ;
       st_imode.codmod,  ;
       st_imode.codcla,  ;
       st_imode.linea,  ;
       st_issre.codsin,  ;
       st_iorep.codtall,  ;
       st_iorep.fecemi,  ;
       st_iorep.numser,  ;
       st_iorep.cosrep,  ;
       st_iorep.cosmob,  ;
       st_isrep.coddes FROM  ;
       ST_IOREP, ST_IMODE,  ;
       ST_ISSRE, ST_ISREP WHERE  ;
       st_iorep.codmar =  ;
       st_imode.codmar AND  ;
       st_iorep.codmod =  ;
       st_imode.codmod AND  ;
       st_issre.numdoc =  ;
       st_iorep.numsol AND  ;
       st_iorep.numsol =  ;
       st_isrep.numdoc AND  ;
       BETWEEN(st_iorep.fecemi,  ;
       fecha1, fecha2) AND  ;
       st_iorep.indest <> 'A' AND  ;
       BETWEEN(st_isrep.codemi,  ;
       '200 ', '299') ORDER BY  ;
       st_iorep.codemi,  ;
       st_iorep.codtall,  ;
       st_iorep.codmar,  ;
       st_iorep.indori,  ;
       st_imode.codmod,  ;
       st_iorep.fecemi,  ;
       st_iorep.numdoc
COPY TO fff
RETURN
*
PROCEDURE conso
USE fff
GOTO TOP
DO WHILE  .NOT. EOF()
     ord = numdoc
     n = 1
     SCAN WHILE numdoc = ord  ;
          .AND.  .NOT. EOF()
          IF n > 1
               DELETE
          ENDIF
          n = n + 1
     ENDSCAN
ENDDO
RETURN
*
PROCEDURE nume
USE fff
GOTO TOP
DO WHILE  .NOT. EOF()
     mod = codmod
     n = 1
     DO WHILE codmod=mod .AND.   ;
        .NOT. EOF()
          ord = numdoc
          SCAN WHILE numdoc = ord  ;
               .AND.  .NOT.  ;
               EOF()
               REPLACE nume WITH  ;
                       n
          ENDSCAN
          n = n + 1
     ENDDO
ENDDO
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

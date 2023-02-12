*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET SYSMENU ON
CLEAR
CLOSE DATABASES
SELECT 1
USE ST_IOREP ORDER ORD_FECFAC
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
CREATE CURSOR TOTAL (numsol C (8),  ;
       fecdoc D, emisor C (4),  ;
       mobra N (9, 2), taller C  ;
       (4), rpto N (9, 2), subdiv  ;
       C (1))
a = 0
SELECT st_iorep
SET NEAR ON
SEEK DTOS(CTOD('01/07/98'))
SCAN WHILE MONTH(fecfabo) = 7  ;
     .AND. YEAR(fecfabo) = 1998
     IF codfabo = 'LIQU' .AND.  ;
        indest = 'F' .AND.  ;
        SUBSTR(codemi, 1, 1) =  ;
        '2'
          a = a + 1
          @ 10, 10 SAY a
          SELECT total
          APPEND BLANK
          REPLACE numsol WITH  ;
                  st_iorep.numsol,  ;
                  fecdoc WITH  ;
                  st_iorep.fecemi
          REPLACE emisor WITH  ;
                  st_iorep.codemi,  ;
                  mobra WITH  ;
                  st_iorep.cosmob
          REPLACE rpto WITH  ;
                  st_iorep.cosrep,  ;
                  taller WITH  ;
                  st_iorep.codtall
          DO CASE
               CASE st_iorep.codtall <  ;
                    '011 '
                    REPLACE subdiv  ;
                            WITH  ;
                            '1'
               CASE st_iorep.codtall <  ;
                    '021 '
                    REPLACE subdiv  ;
                            WITH  ;
                            '2'
               CASE st_iorep.codtall <  ;
                    '060 '
                    REPLACE subdiv  ;
                            WITH  ;
                            '3'
               CASE st_iorep.codtall >  ;
                    '059 '
                    REPLACE subdiv  ;
                            WITH  ;
                            '4'
          ENDCASE
     ENDIF
     SELECT st_iorep
ENDSCAN
SELECT total
INDEX ON subdiv + taller TAG  ;
      taller
SUM mobra TO w_mobra
SUM rpto TO w_rpto
SUM mobra + rpto TO w_total
COUNT TO w_totreg
MODIFY REPORT LIQUID
REPORT FORMAT liquid TO PRINTER
*
*** 
*** ReFox - retrace your steps ... 
***

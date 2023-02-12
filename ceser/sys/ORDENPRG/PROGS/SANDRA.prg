*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_iclpr ORDER codigo
SET RELATION TO 'DIST' + nomdis INTO ge_tab0
SELECT 3
USE SHARED st_iorep ORDER  ;
    ord_fecdoc
SET RELATION TO 'C' + codent INTO st_iclpr
CREATE CURSOR rep_san (fecemi D,  ;
       numsol C (8), noment C  ;
       (30), indori C (4),  ;
       codtall C (4), direcc C  ;
       (30), tele1 N (8, 0),  ;
       tele2 N (8, 0))
SELECT st_iorep
GOTO TOP
SET NEAR ON
SEEK DTOS({01/03/1996})
SET NEAR OFF
BROWSE
SCAN WHILE fecemi >= {01/03/1996}  ;
     .AND. fecemi < {01/04/1996}  ;
     .AND.  .NOT. EOF()
     IF codtall > '010 '
          SELECT rep_san
          APPEND BLANK
          REPLACE fecemi WITH  ;
                  st_iorep.fecemi
          REPLACE codtall WITH  ;
                  st_iorep.codtall
          REPLACE numsol WITH  ;
                  st_iorep.numsol
          REPLACE noment WITH  ;
                  st_iclpr.noment
          REPLACE indori WITH  ;
                  st_iorep.indori
          REPLACE direcc WITH  ;
                  ge_tab0.tab_destab
          REPLACE tele1 WITH  ;
                  st_iclpr.numte1
          REPLACE tele2 WITH  ;
                  st_iclpr.numte2
     ENDIF
     SELECT st_iorep
ENDSCAN
SELECT rep_san
INDEX ON LTRIM(noment) + numsol  ;
      TO indrep
GOTO TOP
BROWSE
SET SYSMENU ON
MODIFY REPORT rep_san
REPORT FORMAT rep_san TO PRINTER
CLOSE DATABASES
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

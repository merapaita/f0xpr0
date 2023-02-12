*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
tit_prg = ' TRASFERENCIA '
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' EXPORT. DE CLIENTES CARSA '
CLOSE DATABASES
DO WHILE .T.
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     STORE 'A' TO wrk_drive
     @ 05, 10 TO 09, 67
     @ 07, 15 SAY  ;
       'Indique el Drive a Copiar :'
     SET CURSOR ON
     @ 07, 45 GET wrk_drive  ;
       PICTURE '!' VALID  ;
       (wrk_drive $ 'AB')
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'GRA', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO WHILE .T.
          = INKEY(0, 'H')
          IF LASTKEY() = 27
               EXIT
          ENDIF
          IF LASTKEY() = -1
               DO graba
               EXIT
          ENDIF
     ENDDO
ENDDO
CLOSE DATABASES
DO saca_win
*
PROCEDURE graba
DO mensa WITH  ;
   '*****  P R O C E S A N D O  *****',  ;
   'COLO'
SELECT DISTINCT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.codent,  ;
       st_iorep.indori,  ;
       st_iorep.auxest,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iorep.numser,  ;
       st_iorep.numsol,  ;
       st_iseri.codent,  ;
       st_iseri.fecvta,  ;
       st_iseri.docgar,  ;
       st_isrep.coddes,  ;
       st_isrep.observ,  ;
       st_isrep.desace,  ;
       st_iclpr.noment,  ;
       st_iclpr.nomcal,  ;
       st_iclpr.nomdis,  ;
       st_iclpr.nomciu,  ;
       st_iclpr.numte1,  ;
       st_iclpr.numte2 FROM  ;
       ST_IOREP, ST_ISERI,  ;
       ST_ISREP, ST_ICLPR WHERE  ;
       st_iorep.numser =  ;
       st_iseri.numser AND  ;
       st_iorep.codmar =  ;
       st_iseri.codmar AND  ;
       st_iorep.codmod =  ;
       st_iseri.modelo AND  ;
       st_iorep.numsol =  ;
       st_isrep.numdoc AND  ;
       st_iorep.codent =  ;
       st_iclpr.codent AND  ;
       (st_iorep.indori = 'GARA'  ;
       AND st_iseri.codent =  ;
       ' 25360443' AND  ;
       st_iorep.auxest < '011 '  ;
       AND YEAR(st_iorep.fecemi) >  ;
       1994 AND st_isrep.coddes =  ;
       'R') ORDER BY  ;
       st_iorep.fecemi INTO  ;
       CURSOR SERIES
aa = SUBSTR(SYS(3), 1, 8) +  ;
     '.FAC'
COPY TO  &AA
CLOSE DATABASES
SELECT 1
USE &AA
bb = SUBSTR(SYS(3), 1, 8) +  ;
     '.DBF'
COPY STRU TO &BB
SELECT 2
USE &BB
APPE FROM &AA FOR SUBSTR(docgar,1,2) =;
"01"
APPE FROM &AA FOR SUBSTR(docgar,1,2) =;
"26"
APPE FROM &AA FOR SUBSTR(docgar,1,2) =;
"49"
APPE FROM &AA FOR SUBSTR(docgar,1,2) =;
"90"
APPE FROM &AA FOR SUBSTR(docgar,1,2) =;
"90"
CLOSE DATABASES
DO mensa WITH  ;
   '*****  P R O C E S A N D O  *****',  ;
   'SACA'
DO mensa WITH  ;
   '*****  G R A B A N D O  *****',  ;
   'COLO'
cc = SUBSTR(bb, 1, 8) + '.FPT'
wrk_file1 = wrk_drive +  ;
            ':\clientes.dbf'
wrk_file2 = wrk_drive +  ;
            ':\clientes.fpt'
COPY FILE &BB TO &wrk_file1
COPY FILE &CC TO &wrk_file2
DO mensa WITH  ;
   '*****  G R A B A N D O  *****',  ;
   'SACA'
DELE FILE &AA
DELE FILE &BB
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

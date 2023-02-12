*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DO directo
RETURN
*
PROCEDURE pcars2
SELECT 1
USE st_iclpr ORDER 1
SELECT 2
USE carfac
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT st_iclpr
     SEEK 'C' + carfac.codent
     IF FOUND()
          SELECT carfac
          REPLACE noment WITH  ;
                  st_iclpr.noment
     ENDIF
     SELECT carfac
ENDSCAN
RETURN
*
PROCEDURE pcarsa
SELECT 1
USE carsa ORDER fecfac
COPY TO carfac STRUCTURE
SELECT 2
USE carfac
SELECT carsa
SET NEAR ON
fecha2 = {}
SEEK DTOS({01/06/1997})
SET NEAR OFF
BROWSE
SCAN WHILE fecfac <= fecha2 .AND.   ;
     .NOT. EOF()
     SCATTER MEMVAR
     SELECT carfac
     APPEND BLANK
     GATHER MEMVAR
     SELECT carsa
ENDSCAN
RETURN
*
PROCEDURE tecnico
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER 1
SELECT 2
USE SHARED st_mvord ORDER ordia
SELECT 3
USE SHARED st_estad ORDER 2
STORE SPACE(8) TO w_orde
STORE SPACE(9) TO w_codt
DO WHILE .T.
     @ 10, 10 SAY 'Orden' GET  ;
       w_orde
     @ 12, 10 SAY 'Tecni' GET  ;
       w_codt
     READ
     IF LASTKEY() = 27
          RETURN
     ENDIF
     SELECT st_iorep
     SEEK w_orde
     IF FOUND()
          REPLACE codtec WITH  ;
                  w_codt
          SELECT st_mvord
          SEEK w_orde
          IF FOUND()
               SCAN WHILE orden =  ;
                    w_orde .AND.   ;
                    .NOT. EOF()
                    REPLACE tecnico  ;
                            WITH  ;
                            w_codt
               ENDSCAN
               SELECT st_estad
               SEEK w_orde
               IF FOUND()
                    REPLACE codtec  ;
                            WITH  ;
                            w_codt
               ENDIF
               ? 'listo' + w_orde
          ENDIF
     ENDIF
ENDDO
RETURN
*
PROCEDURE diremp
CLOSE DATABASES
SET CENTURY ON
SELECT 1
USE SHARED st_iorep ORDER  ;
    ord_fecind
SELECT 2
USE SHARED st_imode ORDER codigo
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 4
USE SHARED st_vense ORDER codigo
SELECT 5
USE SHARED st_mobra ORDER codigo
SELECT 6
CREATE CURSOR tab1 (codmar C (4),  ;
       codart C (4), cantid N (9),  ;
       import N (14, 2))
SELECT st_mobra
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT tab1
     APPEND BLANK
     REPLACE codmar WITH  ;
             st_mobra.mo_codmar,  ;
             codart WITH  ;
             st_mobra.mo_codart
     SELECT st_mobra
ENDSCAN
SELECT tab1
INDEX ON codmar + codart TAG  ;
      codigo
CLEAR
STORE DATE() TO w_fecini,  ;
      w_fecfin
@ 10, 10 SAY 'f Ini' GET w_fecini
@ 12, 10 SAY 'f Fin' GET w_fecfin
READ
IF LASTKEY() = 27
     SET CENTURY OFF
     RETURN
ENDIF
SELECT st_iorep
SET NEAR ON
SEEK DTOS(w_fecini)
SET NEAR OFF
DO WHILE fecfin<=w_fecfin .AND.   ;
   .NOT. EOF()
     STORE 0 TO w_cantid,  ;
           w_import
     STORE MONTH(fecfin) TO w_mes
     STORE YEAR(fecfin) TO w_ano
     SELECT tab1
     REPLACE cantid WITH 0,  ;
             import WITH 0 ALL
     SELECT st_iorep
     SCAN WHILE MONTH(fecfin) =  ;
          w_mes .AND.  .NOT.  ;
          EOF()
          IF SUBSTR(indori, 1, 1) =  ;
             'G' .OR.  ;
             SUBSTR(indori, 1, 1) =  ;
             'P'
               SELECT st_imode
               SEEK st_iorep.codmar +  ;
                    st_iorep.codmod
               IF FOUND()
                    IF codcla =  ;
                       '101 '  ;
                       .OR.  ;
                       codcla =  ;
                       '103 '  ;
                       .OR.  ;
                       codcla =  ;
                       '105 '  ;
                       .OR.  ;
                       codcla =  ;
                       '106 '
                         w_codart =  ;
                          '104 '
                    ELSE
                         IF codcla =  ;
                            '113 '  ;
                            .OR.  ;
                            codcla =  ;
                            '114 '  ;
                            .OR.  ;
                            codcla =  ;
                            '120 '
                              w_codart =  ;
                               '115 '
                         ELSE
                              IF codcla =  ;
                                 '150 '  ;
                                 .OR.  ;
                                 codcla =  ;
                                 '152 '  ;
                                 .OR.  ;
                                 codcla =  ;
                                 '153 '  ;
                                 .OR.  ;
                                 codcla =  ;
                                 '154 '  ;
                                 .OR.  ;
                                 codcla =  ;
                                 '155 '  ;
                                 .OR.  ;
                                 codcla =  ;
                                 '156 '
                                   w_codart = '151 '
                              ELSE
                                   IF codcla = '161 ' .OR. codcla = '163 ' .OR. codcla = '164 '
                                        w_codart = '160 '
                                   ELSE
                                        IF codcla = '165 ' .OR. codcla = '167 ' .OR. codcla = '168 '
                                             w_codart = '166 '
                                        ELSE
                                             w_codart = st_imode.codcla
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
                    SELECT tab1
                    SEEK st_imode.codmar +  ;
                         w_codart
                    IF FOUND()
                         REPLACE cantid  ;
                                 WITH  ;
                                 cantid +  ;
                                 1
                         REPLACE import  ;
                                 WITH  ;
                                 import +  ;
                                 (st_iorep.cosmob +  ;
                                 st_iorep.cosrep)
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SELECT tab1
     GOTO TOP
     SCAN WHILE  .NOT. EOF()
          IF cantid > 0
               IF codmar = '01'
                    w_codemi = '100 '
               ELSE
                    IF codmar =  ;
                       '37'
                         w_codemi =  ;
                          '300 '
                    ELSE
                         IF codmar =  ;
                            '44'
                              w_codemi =  ;
                               '500 '
                         ELSE
                              w_codemi =  ;
                               '200 '
                         ENDIF
                    ENDIF
               ENDIF
               SELECT st_vense
               SEEK STR(w_ano, 4) +  ;
                    STR(w_mes, 2) +  ;
                    tab1.codmar +  ;
                    tab1.codart
               IF  .NOT. FOUND()
                    APPEND BLANK
                    REPLACE ano  ;
                            WITH  ;
                            STR(w_ano,  ;
                            4),  ;
                            mes  ;
                            WITH  ;
                            STR(w_mes,  ;
                            2),  ;
                            codmar  ;
                            WITH  ;
                            tab1.codmar,  ;
                            codart  ;
                            WITH  ;
                            tab1.codart,  ;
                            codemi  ;
                            WITH  ;
                            w_codemi
               ENDIF
               REPLACE unidas  ;
                       WITH  ;
                       tab1.cantid,  ;
                       impors  ;
                       WITH  ;
                       tab1.import
               ? ano, mes, codmar,  ;
                 codart
               ?? unidas
               ?? impors
               SELECT tab1
          ENDIF
     ENDSCAN
     SELECT st_iorep
ENDDO
SELECT st_vense
MODIFY REPORT flory16
SET CENTURY OFF
RETURN
*
PROCEDURE directo
CLOSE DATABASES
SET CENTURY ON
SELECT 1
USE SHARED st_iorep ORDER  ;
    ord_esem
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
CREATE CURSOR pend (numdoc C (8),  ;
       codemi C (4), fecest D  ;
       (8))
INDEX ON codemi + DTOS(fecest)  ;
      TAG codigo
SELECT st_iorep
SEEK '004'
SCAN WHILE auxest = '004' .AND.   ;
     .NOT. EOF()
     SELECT pend
     APPEND BLANK
     REPLACE numdoc WITH  ;
             st_iorep.numdoc,  ;
             fecest WITH  ;
             st_iorep.fecest
     IF SUBSTR(st_iorep.codemi, 1,  ;
        1) = '9'
          w_codemi = '100'
     ELSE
          IF SUBSTR(st_iorep.codemi,  ;
             1, 3) = '202'
               w_codemi = '202'
          ELSE
               w_codemi = SUBSTR(st_iorep.codemi,  ;
                          1, 1) +  ;
                          '00'
          ENDIF
     ENDIF
     REPLACE codemi WITH w_codemi
     SELECT st_iorep
ENDSCAN
SELECT pend
SET RELATION TO 'EMIS' + codemi INTO ge_tab0
REPORT FORMAT flory14 TO FILE  ;
       pend.txt
CLOSE DATABASES
SET CENTURY OFF
RETURN
*
PROCEDURE arrefac
CLOSE DATABASES
SELECT 1
USE st_iorep ORDER ord_numfab
SELECT 2
USE SHARED st_iprep ORDER 2
SELECT 3
USE SHARED st_idped ORDER 2
SELECT st_iorep
SEEK 'FACT' + '0009001092'
SCAN WHILE codfabo = 'FACT' .AND.  ;
     numfabo = '0009001092' .AND.   ;
     .NOT. EOF()
     SELECT st_iprep
     SEEK st_iorep.numdoc
     IF FOUND()
          SCAN WHILE numord =  ;
               st_iorep.numdoc  ;
               .AND.  .NOT.  ;
               EOF()
               IF indest <> 'N'
                    SELECT st_idped
                    SEEK st_iprep.numdoc
                    SCAN WHILE  ;
                         numdoc =  ;
                         st_iprep.numdoc  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         IF canpro >  ;
                            0
                              ? numord +  ;
                                SPACE(4)
                              ?? codpro
                              ?? canpro
                              ?? valpro
                         ENDIF
                    ENDSCAN
               ENDIF
               SELECT st_iprep
          ENDSCAN
     ENDIF
     SELECT st_iorep
ENDSCAN
RETURN
*
PROCEDURE cambio
USE ff
ON KEY LABEL CTRL+W ??""
BROWSE
RETURN
*
PROCEDURE act2
CLOSE DATABASES
SELECT 1
USE SHARED st_isrep ORDER 3
SET CENTURY ON
SEEK DTOS(DATE())
BROWSE
RETURN
*
PROCEDURE act_sol
SET SYSMENU ON
CLEAR
CLOSE DATABASES
SELECT 1
USE SHARED ST_ISREP ORDER CODIGO
SELECT 2
USE SHARED ISREP ORDER CODIGO
GOTO TOP
SEEK '      10'
BROWSE
SCAN WHILE  .NOT. EOF()
     @ 10, 20 SAY numdoc
     IF  .NOT. EMPTY(observ) .OR.   ;
         .NOT. EMPTY(desace)
          SELECT st_isrep
          SEEK isrep.numdoc
          IF FOUND()
               IF EMPTY(observ)  ;
                  .AND.  .NOT.  ;
                  EMPTY(isrep.observ)
                    DO rbloquea
                    REPLACE observ  ;
                            WITH  ;
                            isrep.observ
                    UNLOCK
               ENDIF
               IF EMPTY(desace)
                    DO rbloquea
                    REPLACE desace  ;
                            WITH  ;
                            isrep.desace
                    UNLOCK
               ENDIF
          ENDIF
          SELECT isrep
     ENDIF
ENDSCAN
CLOSE DATABASES
RETURN
*
PROCEDURE ppact
SET SYSMENU ON
CLOSE DATABASES
SELECT 1
USE SHARED gc_alm00 ORDER 1
SELECT gc_alm00
GOTO TOP
DO WHILE  .NOT. EOF()
     codpro = alm_codpro
     codalm = alm_codalm
     STORE 0 TO w_c
     SCAN WHILE alm_codpro =  ;
          codpro .AND. alm_codalm =  ;
          codalm .AND.  .NOT.  ;
          EOF()
          w_c = w_c + 1
          IF w_c > 1
               BROWSE
          ENDIF
     ENDSCAN
ENDDO
RETURN
*
PROCEDURE pporden
CLOSE DATABASES
SELECT 1
USE sti
SELECT 2
USE SHARED st_iorep ORDER 1
SELECT sti
SET SYSMENU ON
SCAN WHILE  .NOT. EOF()
     IF date = DATE()
          SCATTER TO memvar
          SELECT st_iorep
          SEEK sti.numdoc
          IF  .NOT. FOUND()
               APPEND BLANK
          ENDIF
          DO rbloquea
          GATHER FROM memvar
          UNLOCK
          BROWSE
     ENDIF
     SELECT sti
ENDSCAN
RETURN
*
PROCEDURE entrega
CLOSE DATABASES
SET CENTURY ON
SELECT 1
USE SHARED st_iclpr ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_entind
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 4
USE SHARED st_isrep ORDER codigo
SELECT 5
USE SHARED st_imode ORDER codigo
CREATE CURSOR car (numord C (8),  ;
       numsol C (8), fecent D (8),  ;
       codent C (9), noment C  ;
       (30), numte1 N (8), numte2  ;
       N (8), nomdis C (25),  ;
       articulo C (30), marca C  ;
       (20), codmod C (15),  ;
       indori C (4), informe C  ;
       (50), fecsol D (8), codemi  ;
       C (4), codtall C (4))
INDEX ON DTOS(fecent) + codent +  ;
      numsol TAG codigo
SELECT st_iorep
SET RELATION TO 'C' + codent INTO st_iclpr,;
numsol INTO st_isrep
STORE .T. TO ppas
DO WHILE ppas
     CLEAR
     STORE DATE() TO fecha1,  ;
           fecha2
     @ 10, 10 SAY 'f Ini' GET  ;
       fecha1
     @ 12, 10 SAY 'f Fin' GET  ;
       fecha2
     READ
     IF LASTKEY() = 27
          ppas = .F.
     ELSE
          SET NEAR ON
          SEEK DTOS(fecha1)
          SET NEAR OFF
          SCAN WHILE fecent >=  ;
               fecha1 .AND.  ;
               fecent <= fecha2  ;
               .AND.  .NOT.  ;
               EOF()
               IF SUBSTR(codemi,  ;
                  1, 1) = '3'
                    SELECT st_imode
                    SEEK st_iorep.codmar +  ;
                         st_iorep.codmod
                    IF FOUND()
                         SELECT ge_tab0
                         SEEK 'CLAS' +  ;
                              st_imode.codcla
                    ENDIF
                    SELECT car
                    APPEND BLANK
                    REPLACE numord  ;
                            WITH  ;
                            st_iorep.numdoc,  ;
                            numsol  ;
                            WITH  ;
                            st_iorep.numsol,  ;
                            fecent  ;
                            WITH  ;
                            st_iorep.fecent,  ;
                            indori  ;
                            WITH  ;
                            st_iorep.indori,  ;
                            fecsol  ;
                            WITH  ;
                            st_isrep.fecemi,  ;
                            informe  ;
                            WITH  ;
                            st_iorep.observ,  ;
                            codtall  ;
                            WITH  ;
                            st_iorep.codtall
                    REPLACE codent  ;
                            WITH  ;
                            st_iorep.codent,  ;
                            numte1  ;
                            WITH  ;
                            st_iclpr.numte1,  ;
                            numte2  ;
                            WITH  ;
                            st_iclpr.numte2,  ;
                            noment  ;
                            WITH  ;
                            st_iclpr.noment,  ;
                            codemi  ;
                            WITH  ;
                            st_iorep.codemi,  ;
                            codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            articulo  ;
                            WITH  ;
                            ge_tab0.tab_destab
                    SELECT ge_tab0
                    SEEK 'MARC' +  ;
                         st_iorep.codmar
                    SELECT car
                    REPLACE marca  ;
                            WITH  ;
                            ge_tab0.tab_destab
                    SELECT ge_tab0
                    SEEK 'DIST' +  ;
                         st_iclpr.nomdis
                    SELECT car
                    REPLACE nomdis  ;
                            WITH  ;
                            ge_tab0.tab_destab
               ENDIF
               SELECT st_iorep
          ENDSCAN
     ENDIF
     ppas = .F.
ENDDO
SELECT car
MODIFY REPORT flory17
RETURN
*
PROCEDURE actmemo
CLOSE DATABASES
SELECT 1
USE SHARED isrep ORDER 1
SELECT 2
USE SHARED st_isrep ORDER 1
SELECT isrep
GOTO TOP
SET SYSMENU ON
SCAN WHILE  .NOT. EOF()
     SELECT st_isrep
     SEEK isrep.numdoc
     IF FOUND()
          DO rbloquea
          REPLACE observ WITH  ;
                  isrep.observ,  ;
                  desace WITH  ;
                  isrep.desace
          ? 'reemplazando memo a' +  ;
            isrep.numdoc
          UNLOCK
     ENDIF
     SELECT isrep
ENDSCAN
RETURN
*
PROCEDURE sdomi
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER  ;
    ord_numsol
SELECT 2
USE SHARED st_isrep ORDER  ;
    sol_emisor
SELECT 3
USE SHARED st_ispre ORDER 1
SELECT 4
USE SHARED st_mvord ORDER ordia
SELECT st_isrep
a = '300 '
SEEK a
SET SYSMENU ON
SET ESCAPE ON
IF FOUND()
     BROWSE
     SCAN WHILE codemi = a .AND.   ;
          .NOT. EOF()
          IF coddes = 'D' .AND.  ;
             YEAR(fecemi) < 1996
               SELECT st_iorep
               SEEK st_isrep.numdoc
               IF FOUND()
                    IF auxest =  ;
                       '080 '
                         DO rbloquea
                         IF EMPTY(fecent)
                              REPLACE  ;
                               fecent  ;
                               WITH  ;
                               fecfabo,  ;
                               horent  ;
                               WITH  ;
                               time,  ;
                               horfin  ;
                               WITH  ;
                               time,  ;
                               horest  ;
                               WITH  ;
                               time
                         ENDIF
                         ? numdoc,  ;
                           numsol,  ;
                           auxest,  ;
                           st_isrep.coddes,  ;
                           codtall
                         REPLACE auxest  ;
                                 WITH  ;
                                 '100',  ;
                                 indest  ;
                                 WITH  ;
                                 'F',  ;
                                 fecest  ;
                                 WITH  ;
                                 DATE(),  ;
                                 horest  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                         ?? 'Actualizado' +  ;
                            numdoc
                         SELECT st_mvord
                         SEEK st_iorep.numdoc +  ;
                              '080 '
                         IF FOUND()
                              w_dia =  ;
                               dia
                              w_hora =  ;
                               hora
                              DO rbloquea
                              DELETE
                              UNLOCK
                         ELSE
                              w_dia =  ;
                               DATE()
                              w_hora =  ;
                               TIME()
                         ENDIF
                         APPEND BLANK
                         DO rbloquea
                         REPLACE orden  ;
                                 WITH  ;
                                 st_iorep.numdoc,  ;
                                 dia  ;
                                 WITH  ;
                                 w_dia,  ;
                                 tecnico  ;
                                 WITH  ;
                                 st_iorep.codtec,  ;
                                 estado  ;
                                 WITH  ;
                                 st_iorep.auxest,  ;
                                 destado  ;
                                 WITH  ;
                                 'FACTURADO Y ENTREGADO',  ;
                                 date  ;
                                 WITH  ;
                                 DATE(),  ;
                                 time  ;
                                 WITH  ;
                                 TIME(),  ;
                                 hora  ;
                                 WITH  ;
                                 w_hora
                         UNLOCK
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_isrep
     ENDSCAN
ENDIF
RETURN
*
PROCEDURE gold
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER 1
SELECT 2
USE SHARED st_iseri ORDER 1
SET FILTER TO YEAR(fecing) = 1996;
.AND. fecing - fecvta <= 15
INDEX ON codmar + modelo +  ;
      DTOS(fecing) TO idx1
GOTO TOP
MODIFY REPORT flory13
RETURN
*
PROCEDURE act09
CLOSE DATABASES
CLEAR
USE SHARED st_isrep ORDER  ;
    sol_emisor
? 'isrep'
DO paso2
USE SHARED st_iorep ORDER 1
? 'iorep'
DO paso2
USE SHARED st_ispre ORDER 1
? 'ispre'
DO paso2
RETURN
*
PROCEDURE paso2
REPLACE codemi WITH '2611' ALL  ;
        FOR codemi = '2763'
REPLACE codemi WITH '2619' ALL  ;
        FOR codemi = '2769'
RETURN
*
PROCEDURE paso3
REPLACE dre_codemi WITH '2123'  ;
        ALL FOR dre_codemi =  ;
        '203 '
REPLACE dre_codemi WITH '2124'  ;
        ALL FOR dre_codemi =  ;
        '204 '
RETURN
*
PROCEDURE paso4
REPLACE hve_codemi WITH '2123'  ;
        ALL FOR hve_codemi =  ;
        '203 '
REPLACE hve_codemi WITH '2124'  ;
        ALL FOR hve_codemi =  ;
        '204 '
REPLACE hve_codemi WITH '2122'  ;
        ALL FOR hve_codemi =  ;
        '205 '
REPLACE hve_codemi WITH '2121'  ;
        ALL FOR hve_codemi =  ;
        '206 '
REPLACE hve_codemi WITH '2120'  ;
        ALL FOR hve_codemi =  ;
        '207 '
REPLACE hve_codemi WITH '2119'  ;
        ALL FOR hve_codemi =  ;
        '208 '
REPLACE hve_codemi WITH '2277'  ;
        ALL FOR hve_codemi =  ;
        '209 '
REPLACE hve_codemi WITH '2263'  ;
        ALL FOR hve_codemi =  ;
        '211 '
REPLACE hve_codemi WITH '2269'  ;
        ALL FOR hve_codemi =  ;
        '219 '
REPLACE hve_codemi WITH '2268'  ;
        ALL FOR hve_codemi =  ;
        '220 '
REPLACE hve_codemi WITH '2267'  ;
        ALL FOR hve_codemi =  ;
        '221 '
REPLACE hve_codemi WITH '2261'  ;
        ALL FOR hve_codemi =  ;
        '228 '
REPLACE hve_codemi WITH '2260'  ;
        ALL FOR hve_codemi =  ;
        '229 '
RETURN
*
PROCEDURE actfec
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER 1
SELECT 2
USE SHARED st_mvord ORDER ordia
SELECT 3
USE SHARED ge_tab0 ORDER 1
SELECT st_iorep
SET ORDER TO ord_fecind
SET NEAR ON
SEEK DTOS({01/01/1901})
BROWSE
SET NEAR OFF
SCAN WHILE  .NOT. EOF()
     IF codtall > '010'
          SELECT st_mvord
          SEEK st_iorep.numdoc +  ;
               '018 '
          IF FOUND()
               DO fec2
          ELSE
               SEEK st_iorep.numdoc +  ;
                    '026 '
               IF FOUND()
                    DO fec2
               ELSE
                    SEEK st_iorep.numdoc +  ;
                         '027 '
                    IF FOUND()
                         DO fec2
                    ELSE
                         ? 'no hay para' +  ;
                           st_iorep.numdoc
                    ENDIF
               ENDIF
          ENDIF
     ELSE
          IF auxest <> '010 '  ;
             .AND. auxest <>  ;
             '021 ' .AND. auxest <>  ;
             '026 ' .AND. auxest <>  ;
             '080 '
               SELECT st_mvord
               SEEK st_iorep.numdoc +  ;
                    '020 '
               IF FOUND()
                    DO fec2
               ELSE
                    SEEK st_iorep.numdoc +  ;
                         '022 '
                    IF FOUND()
                         DO fec2
                    ELSE
                         SEEK st_iorep.numdoc +  ;
                              '023 '
                         IF FOUND()
                              DO fec2
                         ELSE
                              SEEK  ;
                               st_iorep.numdoc +  ;
                               '024 '
                              IF FOUND()
                                   DO fec2
                              ELSE
                                   SEEK st_iorep.numdoc + '025 '
                                   IF FOUND()
                                        DO fec2
                                   ELSE
                                        SEEK st_iorep.numdoc + '030 '
                                        IF FOUND()
                                             DO fec2
                                        ELSE
                                             SEEK st_iorep.numdoc + '100 '
                                             IF FOUND()
                                                  DO fec2
                                             ELSE
                                                  ? 'no hay para' + st_iorep.numdoc
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
     SELECT st_iorep
ENDSCAN
RETURN
*
PROCEDURE fec2
SELECT st_iorep
REPLACE fecent WITH st_mvord.dia,  ;
        horent WITH  ;
        st_mvord.hora
RETURN
*
PROCEDURE reclamos
SELECT st_iorep.numdoc,  ;
       st_iorep.codemi,  ;
       st_iorep.codent,  ;
       st_iorep.codtall,  ;
       st_iorep.indori,  ;
       st_iorep.auxest,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iclpr.codent,  ;
       st_iorep.numser,  ;
       st_iorep.codtec,  ;
       st_iorep.fecest,  ;
       st_iorep.numsol,  ;
       st_iclpr.noment,  ;
       st_iorep.fecemi,  ;
       st_iorep.observ,  ;
       st_imode.nommod,  ;
       st_iclpr.nomdis,  ;
       st_iclpr.numte1 FROM  ;
       ST_IOREP, ST_IMODE,  ;
       ST_ICLPR WHERE  ;
       st_iorep.codmod =  ;
       st_imode.codmod AND  ;
       st_iorep.codent =  ;
       st_iclpr.codent AND  ;
       st_iorep.indest <> 'N' AND  ;
       SUBSTR(st_iorep.indori, 2,  ;
       1) = 'R' AND  ;
       YEAR(st_iorep.fecemi) =  ;
       1996 AND  ;
       BETWEEN(MONTH(st_iorep.fecemi),  ;
       8, 9) ORDER BY  ;
       st_iorep.indori,  ;
       st_iorep.fecemi,  ;
       st_iorep.numdoc INTO  ;
       CURSOR RECLA
MODIFY REPORT flory11
RETURN
*
PROCEDURE taller
SELECT st_iorep.numdoc,  ;
       st_iorep.codemi,  ;
       st_iorep.codent,  ;
       st_iorep.codtall,  ;
       st_iorep.indori,  ;
       st_iorep.auxest,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iorep.numser,  ;
       st_iorep.codtec,  ;
       st_iorep.fecest,  ;
       st_iorep.numsol,  ;
       st_iorep.fecemi,  ;
       st_iorep.observ,  ;
       st_imode.nommod,  ;
       st_iclpr.nomdis FROM  ;
       ST_IOREP, ST_IMODE,  ;
       ST_ICLPR WHERE  ;
       st_iorep.codmod =  ;
       st_imode.codmod AND  ;
       st_iorep.codent =  ;
       st_iclpr.codent AND  ;
       st_iorep.indest <> 'N' AND  ;
       YEAR(st_iorep.fecemi) =  ;
       1996 AND  ;
       BETWEEN(st_iorep.codtall,  ;
       '014 ', '017 ') ORDER BY  ;
       st_iorep.codtall,  ;
       st_iorep.indori,  ;
       st_iorep.fecemi,  ;
       st_iorep.codmod,  ;
       st_iclpr.nomdis INTO  ;
       CURSOR taller
MODIFY REPORT FLORY10
RETURN
*
PROCEDURE gara
CLOSE DATABASES
w_mesgar = 18
w_fchgar = DATE()
w_fchfin = GOMONTH(w_fchgar,  ;
           w_mesgar)
? w_fchgar, w_fchfin
RETURN
*
PROCEDURE norden
CLOSE DATABASES
SELECT 1
USE SHARED st_isrep ORDER emimod
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_numsol
SELECT st_isrep
SET NEAR ON
SEEK '200'
SET NEAR OFF
SET SYSMENU ON
SCAN WHILE codemi < '300 ' .AND.   ;
     .NOT. EOF()
     SELECT st_iorep
     SEEK st_isrep.numdoc
     IF  .NOT. FOUND() .AND.  ;
         st_isrep.fecemi < DATE() -  ;
         9 .AND. st_isrep.indest =  ;
         'V'
          SELECT st_isrep
          BROWSE FIELDS numdoc,  ;
                 codemi, indest,  ;
                 codmar, codmod,  ;
                 fecemi
     ELSE
          SELECT st_isrep
     ENDIF
ENDSCAN
RETURN
*
PROCEDURE sintoma
CLOSE DATABASES
SELECT 1
USE SHARED st_sicli
SELECT 2
USE SHARED st_sinid ORDER codigo
SELECT st_sinid
SET SYSMENU ON
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT st_sicli
     GOTO TOP
     SCAN WHILE  .NOT. EOF()
          IF SUBSTR(st_sicli.codsin,  ;
             2, 4) =  ;
             SUBSTR(st_sinid.codsin,  ;
             1, 3)
               ? codsin,  ;
                 st_sinid.codsin +  ;
                 ' '
               SELECT st_sinid
               IF  .NOT.  ;
                   EMPTY(st_sinid.linea)  ;
                   .AND.  .NOT.  ;
                   EMPTY(st_sinid.sinnue)
                    SELECT st_sicli
                    IF date <>  ;
                       DATE()
                         REPLACE codsin  ;
                                 WITH  ;
                                 'L' +  ;
                                 st_sinid.sinnue
                         REPLACE time  ;
                                 WITH  ;
                                 TIME()
                         REPLACE date  ;
                                 WITH  ;
                                 DATE()
                         ?? 'reemp ' +  ;
                            'de ' +  ;
                            st_sinid.codsin +  ;
                            'a ' +  ;
                            codsin
                    ENDIF
               ELSE
                    IF  .NOT.  ;
                        EMPTY(st_sinid.video)  ;
                        .AND.   ;
                        .NOT.  ;
                        EMPTY(st_sinid.codvid)
                         SELECT st_sicli
                         IF date <>  ;
                            DATE()
                              REPLACE  ;
                               codsin  ;
                               WITH  ;
                               'L' +  ;
                               st_sinid.codvid
                              REPLACE  ;
                               time  ;
                               WITH  ;
                               TIME()
                              REPLACE  ;
                               date  ;
                               WITH  ;
                               DATE()
                              ?? 'reemp ' +  ;
                                 'de ' +  ;
                                 st_sinid.codsin +  ;
                                 'a ' +  ;
                                 codsin
                         ENDIF
                    ELSE
                         IF  .NOT.  ;
                             EMPTY(st_sinid.camara)  ;
                             .AND.   ;
                             .NOT.  ;
                             EMPTY(st_sinid.codcam)
                              SELECT  ;
                               st_sicli
                              IF date <>  ;
                                 DATE()
                                   REPLACE codsin WITH 'L' + st_sinid.codcam
                                   REPLACE time WITH TIME()
                                   REPLACE date WITH DATE()
                                   ?? 'reemp ' + 'de ' + st_sinid.codsin + 'a ' + codsin
                              ENDIF
                         ELSE
                              IF   ;
                               .NOT.  ;
                               EMPTY(st_sinid.comput)  ;
                               .AND.   ;
                               .NOT.  ;
                               EMPTY(st_sinid.codcom)
                                   SELECT st_sicli
                                   IF date <> DATE()
                                        REPLACE codsin WITH 'L' + st_sinid.codcom
                                        REPLACE time WITH TIME()
                                        REPLACE date WITH DATE()
                                        ?? 'reemp ' + 'de ' + st_sinid.codsin + 'a ' + codsin
                                   ENDIF
                              ELSE
                                   IF  .NOT. EMPTY(st_sinid.blanca) .AND.  .NOT. EMPTY(st_sinid.codbla)
                                        SELECT st_sicli
                                        IF date <> DATE()
                                             REPLACE codsin WITH 'L' + st_sinid.codbla
                                             REPLACE time WITH TIME()
                                             REPLACE date WITH DATE()
                                             ?? 'reemp ' + 'de ' + st_sinid.codsin + 'a ' + codsin
                                        ENDIF
                                   ELSE
                                        ?? 'No tiene reemplazo ' + st_sicli.codsin
                                        SELECT st_sicli
                                        GOTO BOTTOM
                                        SELECT st_sinid
                                        REPLACE notien WITH 'N'
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_sicli
     ENDSCAN
     SELECT st_sinid
ENDSCAN
SELECT st_sicli
GOTO TOP
SCAN WHILE  .NOT. EOF()
     IF SUBSTR(codsin, 2, 4) =  ;
        '11Z'
          REPLACE codsin WITH  ;
                  'L411'
     ENDIF
ENDSCAN
RETURN
*
PROCEDURE car
CLOSE DATABASES
SELECT 1
USE SHARED gc_pro00 ORDER 1
SELECT 2
USE SHARED gc_dve00 ORDER 1
SET RELATION TO dve_propar INTO gc_pro00
SELECT 3
USE SHARED gc_kar00 ORDER 2
SELECT 4
USE SHARED gc_hve00 ORDER 1
SELECT 5
CREATE CURSOR car (codpro C (14),  ;
       car N (9, 2), precio N (9,  ;
       2), cantid N (9, 2), fecha  ;
       D (8), marca C (4), tipdoc  ;
       C (4), nrodoc C (10),  ;
       descri C (40), categ C  ;
       (4))
SELECT gc_hve00
GOTO TOP
SCAN WHILE  .NOT. EOF()
     IF YEAR(hve_fecdoc) > 1994  ;
        .AND. hve_codmov =  ;
        'EVTA'
          SELECT gc_dve00
          SEEK gc_hve00.hve_tipdoc +  ;
               gc_hve00.hve_nrodoc
          IF FOUND()
               SELECT gc_kar00
               SEEK gc_hve00.hve_tipdoc +  ;
                    gc_hve00.hve_nrodoc
               IF FOUND()
                    SELECT car
                    APPEND BLANK
                    REPLACE codpro  ;
                            WITH  ;
                            gc_dve00.dve_propar,  ;
                            marca  ;
                            WITH  ;
                            gc_pro00.pro_marca,  ;
                            categ  ;
                            WITH  ;
                            gc_pro00.pro_catego,  ;
                            car  ;
                            WITH  ;
                            gc_dve00.dve_coprmo
                    REPLACE precio  ;
                            WITH  ;
                            gc_dve00.dve_import,  ;
                            cantid  ;
                            WITH  ;
                            gc_dve00.dve_cantid,  ;
                            descri  ;
                            WITH  ;
                            gc_pro00.pro_descri
                    REPLACE fecha  ;
                            WITH  ;
                            gc_hve00.hve_fecdoc,  ;
                            tipdoc  ;
                            WITH  ;
                            gc_hve00.hve_tipdoc,  ;
                            nrodoc  ;
                            WITH  ;
                            gc_hve00.hve_nrodoc
                    REPLACE car  ;
                            WITH  ;
                            gc_kar00.kar_cosant
               ELSE
                    ? 'no hay' +  ;
                      gc_hve00.hve_tipdoc,  ;
                      gc_hve00.hve_nrodoc
               ENDIF
          ENDIF
     ENDIF
     SELECT gc_hve00
ENDSCAN
SELECT car
INDEX ON codpro + DTOS(fecha) TO  ;
      FLORY2
RETURN
*
PROCEDURE sinto2
CLOSE DATABASES
SELECT 1
USE SHARED st_sinnu
SELECT 2
USE SHARED st_sint
SELECT st_sinnu
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT st_sint
     APPEND BLANK
     REPLACE codsin WITH  ;
             st_sinnu.codsin,  ;
             dessin WITH  ;
             st_sinnu.dessin,  ;
             linea WITH  ;
             st_sinnu.linea
     SELECT st_sinnu
ENDSCAN
RETURN
*
PROCEDURE sinto
CLOSE DATABASES
SELECT 1
USE SHARED st_sinid
SELECT 2
USE SHARED st_sinnue
SELECT st_sinid
SET ORDER TO 6
GOTO TOP
SEEK '0'
SCAN WHILE  .NOT. EMPTY(codbla)  ;
     .AND.  .NOT. EOF()
     SELECT st_sinnu
     APPEND BLANK
     REPLACE sinnue WITH  ;
             st_sinid.codbla,  ;
             descr2 WITH  ;
             st_sinid.desbla,  ;
             linea WITH  ;
             st_sinid.blanca
     SELECT st_sinid
ENDSCAN
RETURN
*
PROCEDURE prod
CLOSE DATABASES
USE SHARED gc_pro00 ORDER 1
SET NEAR ON
SEEK '77'
SET NEAR OFF
SET SYSMENU ON
SCAN WHILE SUBSTR(pro_codpro, 1,  ;
     2) = '77' .AND.  .NOT.  ;
     EOF()
     IF pro_proced <> 'N'
          REPLACE pro_proced WITH  ;
                  'N'
          BROWSE FIELDS  ;
                 pro_codpro :R,  ;
                 pro_proced :R
     ENDIF
ENDSCAN
RETURN
*
PROCEDURE chefac
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER  ;
    ord_inesta
SELECT 2
USE SHARED st_iseri ORDER 2
SELECT st_iorep
SEEK 'GARA'
SCAN WHILE indori <= 'GREC' .AND.   ;
     .NOT. EOF()
     SELECT st_iseri
     SEEK st_iorep.codmar +  ;
          st_iorep.codmod +  ;
          st_iorep.numser
     IF  .NOT. FOUND()
          ? st_iorep.numdoc
          ?? st_iorep.fecemi
          ?? st_iorep.codmar +  ;
             st_iorep.codmod +  ;
             st_iorep.numser
     ENDIF
     SELECT st_iorep
ENDSCAN
RETURN
*
PROCEDURE chequ
SET SYSMENU ON
CLOSE DATABASES
SELECT 1
USE SHARED st_isrep ORDER 1
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_inesta
INDEX ON codemi + indori +  ;
      DTOS(fecemi) + numdoc TO  ;
      flory1
SET RELATION TO numsol INTO st_isrep
SET FILTER TO (indori <> st_isrep.indori;
.AND. YEAR(fecemi) > 1994)
MODIFY REPORT flory1
RETURN
*
PROCEDURE tecn
CLOSE DATABASES
a1 = '    27123'
a2 = '    65675'
USE SHARED st_iorep ORDER 1
REPLACE codtec WITH a2 ALL FOR  ;
        codtec = a1
REPLACE codcca WITH a2 ALL FOR  ;
        codcca = a1
USE SHARED st_mvord ORDER 1
REPLACE tecnico WITH a2 ALL FOR  ;
        tecnico = a1
USE SHARED st_iprep ORDER 1
REPLACE codtec WITH a2 ALL FOR  ;
        codtec = a1
USE SHARED st_ispre ORDER 1
REPLACE codtec WITH a2 ALL FOR  ;
        codtec = a1
USE SHARED st_users
REPLACE codemp WITH ALLTRIM(a2)  ;
        ALL FOR codemp = '27123'
RETURN
*
PROCEDURE pend
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER 1
SELECT 2
USE SHARED st_mvord ORDER  ;
    mvo_tecnic
SET RELATION TO orden INTO st_iorep
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 4
CREATE CURSOR pend (orden C (8),  ;
       dia D (8), estado C (4),  ;
       codemi C (4), codmar C (4),  ;
       codmod C (25), emisor C  ;
       (20), estad2 C (4), ano N  ;
       (9))
st1 = GOMONTH(DATE(), -1)
st1 = st1 + 14
st2 = GOMONTH(DATE(), -16)
st2 = st2 - 16
? st1
?? st2
SET SYSMENU ON
WAIT
SELECT st_mvord
SET NEAR ON
SEEK '004 ' + DTOS(st2)
SET NEAR OFF
SCAN WHILE dia <= st1 .AND.  ;
     estado = '004' .AND.  .NOT.  ;
     EOF()
     SELECT pend
     APPEND BLANK
     REPLACE orden WITH  ;
             st_mvord.orden, dia  ;
             WITH st_mvord.dia
     REPLACE estado WITH  ;
             st_mvord.estado, ano  ;
             WITH YEAR(dia)
     REPLACE codemi WITH  ;
             st_iorep.codemi,  ;
             codmar WITH  ;
             st_iorep.codmar,  ;
             codmod WITH  ;
             st_iorep.codmod
     IF SUBSTR(codemi, 1, 1) =  ;
        '2'
          REPLACE codemi WITH  ;
                  '200'
     ENDIF
     SELECT st_mvord
ENDSCAN
SELECT st_mvord
SET ORDER TO ordia
SELECT pend
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT ge_tab0
     SEEK 'ESOR' + '005'
     SCAN WHILE tab_codpre =  ;
          'ESOR' .AND.  .NOT.  ;
          EOF()
          SELECT st_mvord
          SEEK pend.orden +  ;
               ge_tab0.tab_codtab
          IF FOUND() .AND. dia <=  ;
             st1
               SELECT pend
               REPLACE estad2  ;
                       WITH  ;
                       st_mvord.estado
          ENDIF
          SELECT ge_tab0
     ENDSCAN
     SELECT pend
ENDSCAN
SELECT pend
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT ge_tab0
     SEEK 'ESOR' + '003'
     SCAN WHILE tab_codtab =  ;
          '003' .AND.  .NOT.  ;
          EOF()
          SELECT st_mvord
          SEEK pend.orden +  ;
               ge_tab0.tab_codtab
          IF FOUND() .AND. dia <=  ;
             st1
               SELECT st_iorep
               SEEK st_mvord.orden
               IF FOUND() .AND.  ;
                  auxest =  ;
                  st_mvord.estado
                    SELECT pend
                    REPLACE estad2  ;
                            WITH  ;
                            st_mvord.estado
               ENDIF
          ENDIF
          SELECT ge_tab0
     ENDSCAN
     SELECT pend
ENDSCAN
SELECT pend
DELETE ALL FOR  .NOT.  ;
       EMPTY(estad2)
INDEX ON orden + DTOS(dia) +  ;
      codemi TO codemi
GOTO TOP
SCAN WHILE  .NOT. EOF()
     w_orden = orden
     w_recno = RECNO()
     SKIP
     IF w_orden = orden
          DELETE
     ENDIF
     GOTO w_recno
ENDSCAN
INDEX ON DTOS(dia) + codemi TO  ;
      codemi
BROWSE
RETURN
*
PROCEDURE recl
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER  ;
    ord_fecdoc
st1 = GOMONTH(DATE(), -1)
st1 = st1 - 21
st2 = GOMONTH(DATE(), -2)
st2 = st2 - 21
st3 = GOMONTH(DATE(), -3)
st3 = st3 - 21
st4 = GOMONTH(DATE(), -4)
st4 = st4 - 21
STORE 0 TO recl1, recl2, recl3,  ;
      recl4
STORE 0 TO cerr1, cerr2, cerr3,  ;
      cerr4
SET NEAR ON
SEEK DTOS(st1)
SET NEAR OFF
SCAN WHILE MONTH(fecemi) =  ;
     MONTH(st1) .AND.  ;
     YEAR(fecemi) = YEAR(st1)  ;
     .AND.  .NOT. EOF()
     IF indest <> 'N   '
          IF indori = 'FREC' .OR.  ;
             indori = 'GREC'
               recl1 = recl1 + 1
          ENDIF
          IF  .NOT. EMPTY(fecfin)
               cerr1 = cerr1 + 1
          ENDIF
     ENDIF
ENDSCAN
SET NEAR ON
SEEK DTOS(st2)
SET NEAR OFF
SCAN WHILE MONTH(fecemi) =  ;
     MONTH(st2) .AND.  ;
     YEAR(fecemi) = YEAR(st2)  ;
     .AND.  .NOT. EOF()
     IF indest <> 'N   '
          IF indori = 'FREC' .OR.  ;
             indori = 'GREC'
               recl2 = recl2 + 1
          ENDIF
          IF  .NOT. EMPTY(fecfin)
               cerr2 = cerr2 + 1
          ENDIF
     ENDIF
ENDSCAN
SET NEAR ON
SEEK DTOS(st3)
SET NEAR OFF
SCAN WHILE MONTH(fecemi) =  ;
     MONTH(st3) .AND.  ;
     YEAR(fecemi) = YEAR(st3)  ;
     .AND.  .NOT. EOF()
     IF indest <> 'N   '
          IF indori = 'FREC' .OR.  ;
             indori = 'GREC'
               recl3 = recl3 + 1
          ENDIF
          IF  .NOT. EMPTY(fecfin)
               cerr3 = cerr3 + 1
          ENDIF
     ENDIF
ENDSCAN
SET NEAR ON
SEEK DTOS(st4)
SET NEAR OFF
SCAN WHILE MONTH(fecemi) =  ;
     MONTH(st4) .AND.  ;
     YEAR(fecemi) = YEAR(st4)  ;
     .AND.  .NOT. EOF()
     IF indest <> 'N   '
          IF indori = 'FREC' .OR.  ;
             indori = 'GREC'
               recl4 = recl4 + 1
          ENDIF
          IF  .NOT. EMPTY(fecfin)
               cerr4 = cerr4 + 1
          ENDIF
     ENDIF
ENDSCAN
SET PRINTER ON
SET DEVICE TO PRINTER
? SPACE(10) + 'recla ' + SPACE(3) +  ;
  'Reparad'
?? st1
?? recl1
?? cerr1
? SPACE(10)
?? st2
?? recl2
?? cerr1
? SPACE(10)
?? st3
?? recl3
?? cerr3
? SPACE(10)
?? st4
?? recl4
?? cerr4
SET PRINTER OFF
SET DEVICE TO SCREEN
? SPACE(10)
?? st1
?? recl1
?? cerr1
? SPACE(10)
?? st2
?? recl2
?? cerr1
? SPACE(10)
?? st3
?? recl3
?? cerr3
? SPACE(10)
?? st4
?? recl4
?? cerr4
RETURN
*
PROCEDURE fact
CLOSE DATABASES
USE SHARED st_iseri ORDER 1
REPLACE codent WITH ' 25360443'  ;
        ALL FOR (codent =  ;
        ' 11932690')
RETURN
*
PROCEDURE clien
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER 1
SELECT 2
USE SHARED st_mvord ORDER ordia
STORE SPACE(8) TO orde1
DO WHILE .T.
     @ 10, 10 SAY 'Orden ' GET  ;
       orde1 PICTURE '@!'
     READ
     IF LASTKEY() = 27
          RETURN
     ENDIF
     SELECT st_iorep
     SEEK orde1
     SET SYSMENU ON
     KEYBOARD '{CTRL+F10}'
     BROWSE
     SELECT st_mvord
     SEEK orde1
     SET SYSMENU ON
     KEYBOARD '{CTRL+F10}'
     BROWSE
ENDDO
RETURN
*
PROCEDURE presu
CLOSE DATABASES
SELECT 1
USE SHARED gc_dlp00 ORDER 1
SELECT 2
USE SHARED st_iorep ORDER 1
SELECT 3
USE SHARED st_ispre ORDER 1
SELECT 4
USE SHARED st_idpre ORDER 1
SET RELATION TO 'PUBL' + codpro INTO gc_dlp00
SELECT st_ispre
GOTO TOP
SET SYSMENU ON
SET PRINTER ON
SET DEVICE TO PRINTER
? '   ORDEN    PEDIDO  CODPRO          PRECIO   CANTID    PRESIGV   FECHAOR  TIPO'
SET DEVICE TO SCREEN
SET PRINTER OFF
SCAN WHILE  .NOT. EOF()
     SELECT st_iorep
     SEEK st_ispre.numord
     IF FOUND() .AND. (indest =  ;
        'P' .OR. indest = 'V')  ;
        .AND. YEAR(fecemi) >  ;
        1994
          SELECT st_idpre
          SEEK st_ispre.numdoc
          STORE 0 TO wk_tot
          IF FOUND() .AND.  ;
             st_ispre.indest <>  ;
             'N'
               SCAN WHILE numdoc =  ;
                    st_ispre.numdoc  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF valpro <>  ;
                       gc_dlp00.dlp_prsigv
                         SET PRINTER ON
                         SET DEVICE TO;
PRINTER
                         ? st_ispre.numord,  ;
                           numdoc,  ;
                           codpro,  ;
                           valpro,  ;
                           canpro,  ;
                           gc_dlp00.dlp_prsigv,  ;
                           st_iorep.fecemi,  ;
                           st_iorep.indori,  ;
                           st_iorep.auxest,  ;
                           st_iorep.indest
                         SET DEVICE TO;
SCREEN
                         SET PRINTER OFF
                         REPLACE valpro  ;
                                 WITH  ;
                                 gc_dlp00.dlp_prsigv
                         REPLACE totite  ;
                                 WITH  ;
                                 canpro *  ;
                                 valpro
                    ENDIF
                    wk_tot = wk_tot +  ;
                             totite
               ENDSCAN
               SELECT st_ispre
               IF monrep <>  ;
                  wk_tot
                    REPLACE monrep  ;
                            WITH  ;
                            wk_tot
                    REPLACE totigv  ;
                            WITH  ;
                            ROUND(((monrep +  ;
                            monman) *  ;
                            0.18  ;
                            ),  ;
                            2)
                    BROWSE
               ENDIF
          ENDIF
     ENDIF
     SELECT st_ispre
ENDSCAN
RETURN
*
PROCEDURE precio
CLOSE DATABASES
SELECT 1
USE SHARED st_iprep ORDER 1
SELECT 2
USE SHARED gc_dlp00 ORDER 1
SELECT 3
USE SHARED st_iorep ORDER 1
SELECT 4
USE SHARED st_idped ORDER 1
SET RELATION TO 'PUBL' + codpro INTO gc_dlp00
SET RELATION TO numdoc INTO st_iprep ADDITIVE
SELECT 5
USE SHARED gc_kar00 ORDER docpro
SELECT st_iorep
GOTO TOP
SET SYSMENU ON
SET PRINTER ON
SET DEVICE TO PRINTER
SET ESCAPE ON
? '   ORDEN    PEDIDO  CODPRO          PRECIO   CANTID    PRESIGV   FECHAOR  TIPO'
SCAN WHILE  .NOT. EOF()
     IF (indest = 'P' .OR. indest =  ;
        'V') .AND. YEAR(fecemi) >  ;
        1994 .OR. (auxest = '010'  ;
        .OR. auxest = '026' .OR.  ;
        auxest = '021' .OR.  ;
        auxest = '018' .OR.  ;
        auxest = '026' .OR.  ;
        auxest = '027' .OR.  ;
        auxest = '020')
          SELECT st_idped
          SEEK st_iorep.numdoc
          IF FOUND()
               SCAN WHILE numord =  ;
                    st_iorep.numdoc  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF st_iprep.indest <>  ;
                       'N'
                         IF valpro <>  ;
                            gc_dlp00.dlp_prsigv
                              ? st_idped.numord,  ;
                                st_idped.numdoc,  ;
                                st_idped.codpro,  ;
                                st_idped.valpro,  ;
                                st_idped.canpro,  ;
                                gc_dlp00.dlp_prsigv,  ;
                                st_iorep.fecemi,  ;
                                st_iorep.indori,  ;
                                st_iorep.auxest,  ;
                                st_iorep.indest
                              REPLACE  ;
                               valpro  ;
                               WITH  ;
                               gc_dlp00.dlp_prsigv
                              REPLACE  ;
                               totite  ;
                               WITH  ;
                               canpro *  ;
                               valpro
                              SELECT  ;
                               gc_kar00
                              SEEK  ;
                               'PED   ' +  ;
                               st_idped.numdoc +  ;
                               st_idped.codpro
                              IF FOUND()
                                   REPLACE kar_import WITH gc_dlp00.dlp_prsigv
                              ENDIF
                         ENDIF
                    ENDIF
                    SELECT st_idped
               ENDSCAN
          ENDIF
          SELECT st_iorep
          IF (auxest = '010' .OR.  ;
             auxest = '026' .OR.  ;
             auxest = '021' .OR.  ;
             auxest = '018' .OR.  ;
             auxest = '026' .OR.  ;
             auxest = '027' .OR.  ;
             auxest = '020')
               SELECT st_idped
               SEEK st_iorep.numdoc
               STORE 0 TO w_rep
               SCAN WHILE numord =  ;
                    st_iorep.numdoc  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF st_iprep.indest <>  ;
                       'N'
                         IF canpro >  ;
                            0
                              w_rep =  ;
                               w_rep +  ;
                               totite
                         ENDIF
                    ENDIF
               ENDSCAN
               totneto = w_rep +  ;
                         st_iorep.cosmob +  ;
                         st_iorep.flete
               totigva = ROUND(totneto *  ;
                         0.18 ,  ;
                         2)
               totbrut = totneto +  ;
                         totigva
               SELECT st_iorep
               DO rbloquea
               REPLACE subtot  ;
                       WITH  ;
                       totneto
               REPLACE totnet  ;
                       WITH  ;
                       totneto,  ;
                       totigv  ;
                       WITH  ;
                       totigva
               REPLACE totbru  ;
                       WITH  ;
                       totbrut,  ;
                       cosrep  ;
                       WITH  ;
                       w_rep
               ?? st_iorep.numdoc,  ;
                  st_iorep.totnet,  ;
                  st_iorep.totigv,  ;
                  st_iorep.totbru
               UNLOCK
          ENDIF
     ENDIF
ENDSCAN
RETURN
*
PROCEDURE fuera
CLOSE DATABASES
SELECT 1
USE SHARED ST_IOREP ORDER  ;
    ORD_ESEM
SELECT 2
USE SHARED GC_HVE00 ORDER NRDORE
SELECT st_iorep
SET FILTER TO indori = 'FGAR';
.AND. auxest = '020 ';
.AND. YEAR(fecemi) = 1994
INDEX ON auxest + codemi +  ;
      DTOS(fecemi) + numdoc TO  ;
      DD
SET SYSMENU ON
REPORT FORMAT ESTADO TO PRINTER
RETURN
*
FUNCTION pago
SELECT gc_hve00
SEEK st_iorep.numsol
STORE 0 TO acta
IF FOUND()
     SCAN WHILE hve_nrdore =  ;
          st_iorep.numsol .AND.   ;
          .NOT. EOF()
          IF hve_estdoc <> 'A'
               acta = acta +  ;
                      hve_totgen
          ENDIF
     ENDSCAN
ENDIF
RETURN acta
*
PROCEDURE aped
CLOSE DATABASES
SELECT 1
USE SHARED ST_IOREP ORDER 1
SELECT 2
USE SHARED ST_IPREP ORDER 1
SELECT 3
USE SHARED GC_PRO00 ORDER 1
SELECT 4
USE SHARED ST_IDPED ORDER 2
SET RELATION TO numord INTO st_iorep,;
codpro INTO gc_pro00
SELECT 5
CREATE CURSOR COMPRA (numdoc C  ;
       (8), codpro C (14), valpro  ;
       N (9, 2), canpro N (9, 2),  ;
       numord C (8), fecemi D (8),  ;
       codmar C (4), codmod C  ;
       (15), numser C (20),  ;
       indori C (4), codemi C (4),  ;
       codpve C (9))
SELECT 6
USE SHARED ST_ISERI ORDER 2
SELECT st_idped
GOTO TOP
STORE 0 TO tot, totg
? 'N PED.   CODIGO         DESCRIPCION                                       PRECIO     CANTIDAD   NRO O/R  FECEMI  MARCA  MODELO SERIE TIPO'
SCAN WHILE  .NOT. EOF()
     SELECT st_iprep
     SEEK st_idped.numdoc
     IF  .NOT. FOUND() .AND.  ;
         st_iorep.indori =  ;
         'GARA'
          ? st_idped.numdoc,  ;
            st_idped.codpro,  ;
            gc_pro00.pro_descri,  ;
            st_idped.valpro,  ;
            st_idped.canpro,  ;
            st_idped.numord,  ;
            st_iorep.fecemi,  ;
            st_iorep.codmar,  ;
            st_iorep.codmod,  ;
            st_iorep.numser,  ;
            st_iorep.indori
          tot = tot +  ;
                (st_idped.valpro *  ;
                st_idped.canpro)
          SELECT 5
          APPEND BLANK
          REPLACE numdoc WITH  ;
                  st_idped.numdoc,  ;
                  codpro WITH  ;
                  st_idped.codpro,  ;
                  valpro WITH  ;
                  st_idped.valpro,  ;
                  canpro WITH  ;
                  st_idped.canpro,  ;
                  numord WITH  ;
                  st_idped.numord,  ;
                  fecemi WITH  ;
                  st_iorep.fecemi,  ;
                  codmar WITH  ;
                  st_iorep.codmar,  ;
                  codmod WITH  ;
                  st_iorep.codmod,  ;
                  numser WITH  ;
                  st_iorep.numser,  ;
                  indori WITH  ;
                  st_iorep.indori
          REPLACE codemi WITH  ;
                  st_iorep.codemi
          IF st_iorep.indori =  ;
             'GARA'
               SELECT st_iseri
               SEEK st_iorep.codmar +  ;
                    st_iorep.codmod +  ;
                    st_iorep.numser
               totg = totg +  ;
                      (st_idped.valpro *  ;
                      st_idped.canpro)
               IF FOUND()
                    SELECT 5
                    REPLACE codpve  ;
                            WITH  ;
                            st_iseri.codent
               ELSE
                    ?? ' NO HAY PROVEEDOR' +  ;
                       st_iorep.numdoc
               ENDIF
          ENDIF
     ENDIF
     SELECT st_idped
ENDSCAN
SELECT 5
INDEX ON codpve TO CODPED
SET RELATION TO numord INTO st_iorep
? 'TOTAL GENERAL'
?? tot
? 'TOTAL GARANTIA'
?? totg
SET SYSMENU ON
MODIFY REPORT repues
SELECT 5
GOTO TOP
REPORT FORMAT repues TO PRINTER  ;
       FOR valpro <> 0
RETURN
*
PROCEDURE revisi
SELECT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.codtall,  ;
       st_iorep.codent,  ;
       st_iorep.indori,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iorep.numser,  ;
       st_iorep.codtec,  ;
       st_iorep.numsol,  ;
       st_isrep.coddes,  ;
       st_iorep.auxest,  ;
       st_iseri.docgar,  ;
       st_iseri.fecvta,  ;
       st_iseri.codent FROM  ;
       ST_IOREP, ST_ISREP,  ;
       ST_ISERI WHERE  ;
       st_isrep.numdoc =  ;
       st_iorep.numsol AND  ;
       st_iorep.codmar +  ;
       st_iorep.codmod +  ;
       st_iorep.numser =  ;
       st_iseri.codmar +  ;
       st_iseri.modelo +  ;
       st_iseri.numser AND  ;
       st_iseri.docgar =  ;
       'REVISION' ORDER BY  ;
       st_iseri.codent,  ;
       st_iorep.fecemi,  ;
       st_iorep.numdoc INTO  ;
       CURSOR QUERY
RETURN
*
PROCEDURE acga
CLOSE DATABASES
SELECT 1
USE SHARED ST_IOREP ORDER 1
SELECT 2
USE SHARED ST_mvord ORDER ordia
SELECT 1
CLEAR
STORE 0 TO orde
SET SYSMENU ON
STORE .T. TO f
DO WHILE f
     @ 10, 10 SAY 'numero' GET  ;
       orde PICTURE '99999999'
     READ
     IF LASTKEY() = 27
          f = .F.
     ENDIF
     SELECT st_iorep
     SEEK STR(orde, 8)
     BROWSE FIELDS numdoc, fecemi,  ;
            auxest, indori,  ;
            codemi, numfabo,  ;
            fecfabo, cosmob,  ;
            cosrep, subtot,  ;
            totnet, totigv,  ;
            totbru, user, time,  ;
            date
     SELECT st_mvord
     SEEK STR(orde, 8)
     BROWSE FIELDS dia, orden,  ;
            estado, destado,  ;
            tecnico
ENDDO
RETURN
*
PROCEDURE cliente
CLOSE DATABASES
SELECT 1
USE SHARED ST_ICLPR ORDER 1
SELECT 2
USE SHARED ST_IOREP ORDER CODENT
SET RELATION TO 'C' + codent INTO st_iclpr
SELECT 3
CREATE CURSOR CLIEN (codent C (9),  ;
       noment C (20), telefo N  ;
       (7), codemi C (4), nomdis  ;
       C (4), nomcal C (25),  ;
       codmar C (4), codmod C  ;
       (20))
SELECT st_iorep
GOTO TOP
DO WHILE  .NOT. EOF()
     wrk_code = codent
     f = 1
     SCAN WHILE codent = wrk_code  ;
          .AND.  .NOT. EOF()
          IF f = 1
               SELECT clien
               APPEND BLANK
               REPLACE codent  ;
                       WITH  ;
                       st_iorep.codent,  ;
                       noment  ;
                       WITH  ;
                       st_iclpr.noment
               REPLACE telefo  ;
                       WITH  ;
                       st_iclpr.numte1,  ;
                       codemi  ;
                       WITH  ;
                       st_iorep.codemi
               REPLACE nomdis  ;
                       WITH  ;
                       st_iclpr.nomdis,  ;
                       nomcal  ;
                       WITH  ;
                       st_iclpr.nomcal
               REPLACE codmar  ;
                       WITH  ;
                       st_iorep.codmar,  ;
                       codmod  ;
                       WITH  ;
                       st_iorep.codmod
          ENDIF
          f = f + 1
          ? f
          SELECT st_iorep
     ENDSCAN
     SELECT st_iorep
ENDDO
SELECT clien
INDEX ON nomdis + noment + codemi  ;
      TO dd
SET SYSMENU ON
MODIFY REPORT client2
BROWSE
RETURN
*
PROCEDURE carsa
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER  ;
    ord_fecind
SELECT 2
USE SHARED gc_hve00 ORDER 1
a1 = {01/11/1995}
a2 = {}
SELECT st_iorep
SET NEAR ON
SEEK DTOS(a1)
SET NEAR OFF
STORE 0 TO rep, mob, fle, tot1,  ;
      repd, mobd, fled, totd, uni,  ;
      unid
SCAN WHILE fecfin <= a2 .AND.   ;
     .NOT. EOF()
     IF (indest = 'B' .OR. indest =  ;
        'F')
          IF SUBSTR(codemi, 1, 1) =  ;
             '2' .AND.  ;
             SUBSTR(indori, 1, 1) =  ;
             'G'
               IF codemi <>  ;
                  '202 '
                    rep = rep +  ;
                          cosrep
                    mob = mob +  ;
                          cosmob
                    fle = fle +  ;
                          flete
                    tot1 = tot1 +  ;
                           (cosrep +  ;
                           cosmob +  ;
                           flete)
                    uni = uni + 1
               ELSE
                    repd = repd +  ;
                           cosrep
                    mobd = mobd +  ;
                           cosmob
                    fled = fled +  ;
                           flete
                    totd = totd +  ;
                           (cosrep +  ;
                           cosmob +  ;
                           flete)
                    unid = unid +  ;
                           1
               ENDIF
          ENDIF
     ENDIF
ENDSCAN
CLEAR
? a1
?? a2
? 'carsa  repuesto  mano obra  flete    total'
? rep
?? mob
?? fle
?? tot1
? 'carsa d'
? repd
?? mobd
?? fled
?? totd
? 'Unidades c   unida d'
?? uni
?? unid
RETURN
*
PROCEDURE ordeco
CLOSE DATABASES
SELECT 1
USE SHARED st_imode ORDER codigo
SELECT 2
USE SHARED st_movca ORDER 1
SELECT 3
USE SHARED st_iorep ORDER  ;
    ord_fecind
SET RELATION TO codmar + codmod INTO st_imode,;
numdoc INTO st_movca
SELECT 4
CREATE CURSOR car (numdoc C (8),  ;
       indori C (4), codemi C (4),  ;
       codmod C (15), fecemi D  ;
       (8), fecfin D (8), cosmob  ;
       N (9, 2), cosrep N (9, 2),  ;
       flete N (9, 2), totnet N  ;
       (9, 2), totigv N (9, 2),  ;
       totbru N (9, 2), horfin C  ;
       (8), codtall C (4), linea  ;
       C (4), numsol C (9),  ;
       numser C (20), observ M  ;
       (10), codmar C (4), nommod  ;
       C (40), movcau C (4), num2  ;
       N (9, 2))
SELECT 5
USE SHARED ge_tab0 ORDER codigo
SELECT st_iorep
CLEAR
STORE DATE() TO w_fecini,  ;
      w_fecfin
@ 10, 10 SAY 'f Ini' GET w_fecini
@ 12, 10 SAY 'f Fin' GET w_fecfin
READ
IF LASTKEY() = 27
     RETURN
ENDIF
SET NEAR ON
SEEK DTOS(w_fecini)
SET NEAR OFF
SCAN WHILE fecfin <= w_fecfin  ;
     .AND.  .NOT. EOF()
     IF (codemi = '100 ' .OR.  ;
        codemi = '900 ') .AND.  ;
        (indori = 'GARA' .OR.  ;
        indori = 'GREC') .AND.  ;
        SUBSTR(codmod, 1, 2) =  ;
        'KV' .OR. SUBSTR(codmod,  ;
        1, 2) = 'KP'
          SELECT car
          APPEND BLANK
          REPLACE numdoc WITH  ;
                  st_iorep.numdoc,  ;
                  indori WITH  ;
                  st_iorep.indori,  ;
                  codemi WITH  ;
                  st_iorep.codemi,  ;
                  codmod WITH  ;
                  st_iorep.codmod,  ;
                  fecemi WITH  ;
                  st_iorep.fecemi,  ;
                  cosmob WITH  ;
                  st_iorep.cosmob,  ;
                  cosrep WITH  ;
                  st_iorep.cosrep,  ;
                  totnet WITH  ;
                  st_iorep.totnet,  ;
                  totigv WITH  ;
                  st_iorep.totigv,  ;
                  totbru WITH  ;
                  st_iorep.totbru,  ;
                  fecfin WITH  ;
                  st_iorep.fecfin,  ;
                  horfin WITH  ;
                  st_iorep.horfin,  ;
                  codtall WITH  ;
                  st_iorep.codtall,  ;
                  linea WITH  ;
                  st_imode.linea,  ;
                  numsol WITH  ;
                  st_iorep.numsol,  ;
                  numser WITH  ;
                  st_iorep.numser,  ;
                  observ WITH  ;
                  st_iorep.observ,  ;
                  codmar WITH  ;
                  st_iorep.codmar,  ;
                  nommod WITH  ;
                  st_imode.nommod,  ;
                  movcau WITH  ;
                  st_movca.codcau
     ENDIF
     SELECT st_iorep
ENDSCAN
SELECT car
INDEX ON movcau + codmar + codmod +  ;
      DTOS(fecfin) TO codi2
GOTO TOP
STORE 0 TO num1
DO WHILE  .NOT. EOF()
     cod = movcau
     SCAN WHILE movcau = cod  ;
          .AND.  .NOT. EOF()
     ENDSCAN
     SKIP -1
     num1 = num1 + 1
     REPLACE num2 WITH num1
     SKIP
ENDDO
SET SYSMENU ON
MODIFY REPORT flory9
GOTO TOP
REPORT FORMAT flory9 TO PRINTER
RETURN
*
PROCEDURE ordec2
CLOSE DATABASES
SELECT 1
USE SHARED st_imode ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_fecind
SET RELATION TO codmar + codmod INTO st_imode
SELECT 3
CREATE CURSOR car (numdoc C (8),  ;
       indori C (4), codemi C (4),  ;
       codmod C (15), fecemi D  ;
       (8), fecfin D (8), cosmob  ;
       N (9, 2), cosrep N (9, 2),  ;
       flete N (9, 2), totnet N  ;
       (9, 2), totigv N (9, 2),  ;
       totbru N (9, 2), horfin C  ;
       (8), codtall C (4), linea  ;
       C (4), numsol C (9),  ;
       numser C (20), observ M  ;
       (10), codmar C (4), nommod  ;
       C (40))
SELECT 4
USE SHARED ge_tab0 ORDER codigo
SELECT st_iorep
CLEAR
STORE DATE() TO w_fecini,  ;
      w_fecfin
@ 10, 10 SAY 'f Ini' GET w_fecini
@ 12, 10 SAY 'f Fin' GET w_fecfin
READ
IF LASTKEY() = 27
     RETURN
ENDIF
SET NEAR ON
SEEK DTOS(w_fecini)
SET NEAR OFF
SCAN WHILE fecfin <= w_fecfin  ;
     .AND.  .NOT. EOF()
     IF (codemi = '100 ' .OR.  ;
        codemi = '900 ') .AND.  ;
        (indori = 'GARA' .OR.  ;
        indori = 'GREC') .AND.  ;
        SUBSTR(codmod, 1, 2) =  ;
        'KV' .OR. SUBSTR(codmod,  ;
        1, 2) = 'KP'
          SELECT car
          APPEND BLANK
          REPLACE numdoc WITH  ;
                  st_iorep.numdoc,  ;
                  indori WITH  ;
                  st_iorep.indori,  ;
                  codemi WITH  ;
                  st_iorep.codemi,  ;
                  codmod WITH  ;
                  st_iorep.codmod,  ;
                  fecemi WITH  ;
                  st_iorep.fecemi,  ;
                  cosmob WITH  ;
                  st_iorep.cosmob,  ;
                  cosrep WITH  ;
                  st_iorep.cosrep,  ;
                  totnet WITH  ;
                  st_iorep.totnet,  ;
                  totigv WITH  ;
                  st_iorep.totigv,  ;
                  totbru WITH  ;
                  st_iorep.totbru,  ;
                  fecfin WITH  ;
                  st_iorep.fecfin,  ;
                  horfin WITH  ;
                  st_iorep.horfin,  ;
                  codtall WITH  ;
                  st_iorep.codtall,  ;
                  linea WITH  ;
                  st_imode.linea,  ;
                  numsol WITH  ;
                  st_iorep.numsol,  ;
                  numser WITH  ;
                  st_iorep.numser,  ;
                  observ WITH  ;
                  st_iorep.observ,  ;
                  codmar WITH  ;
                  st_iorep.codmar,  ;
                  nommod WITH  ;
                  st_imode.nommod
     ENDIF
     SELECT st_iorep
ENDSCAN
SELECT car
INDEX ON codmar + codmod +  ;
      DTOS(fecfin) TO codi2
MODIFY REPORT flory6
RETURN
*
PROCEDURE usuari
CLOSE DATABASES
SELECT 1
USE SHARED gc_vnd00 ORDER codigo
SELECT 2
USE SHARED st_users ORDER codigo
SET RELATION TO 'A' + codemp INTO gc_vnd00
SELECT st_users
SET FILTER TO SUBSTR(codemp, 1, 1) > '9';
.AND. SUBSTR(codemp, 1, 1) <> 'T';
.AND. fecha = DATE();
.AND. estado <> '000'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

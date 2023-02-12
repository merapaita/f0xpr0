*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F10 DO FCINCO
@ 02, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' CONTROL DE PRODUCCION  '
CLOSE DATABASES
SELECT 1
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 2
USE SHARED ST_IOREP ORDER  ;
    ORD_FECDOC
SELECT 3
USE SHARED ST_ITECN ORDER CODIGO
SELECT 4
USE SHARED ST_MVORD ORDER ESTADO
SELECT 5
USE SHARED ST_ASIEM ORDER ASI_COD
STORE SPACE(8) TO filtxt, fildx1,  ;
      fildx2
STORE DATE() TO fecha1, fecha2,  ;
      fecha
STORE SPACE(30) TO w_detord,  ;
      w_control
STORE 0 TO tecni1, tecni2
STORE SPACE(4) TO taller1,  ;
      taller2, emisor1, emisor2
DO esc_modo WITH 'S'
@ 04, 30 SAY SPACE(30)
sigue = .T.
w_total = 0
DO WHILE sigue
     @ 04, 01 CLEAR TO 14, 78
     @ 04, 30 SAY SPACE(30)
     @ 04, 05 SAY SPACE(50)
     STORE 0 TO nco, w_reg, col1,  ;
           col2, w_tot, w_dlab,  ;
           w_dmes, w_dtec, w_mes,  ;
           w_sub
     STORE 1 TO w_orden,  ;
           w_informe, w_salida
     STORE SPACE(25) TO destec1,  ;
           destec2
     STORE SPACE(9) TO tecni,  ;
           w_codtec
     STORE SPACE(4) TO w_codtall
     DIMENSION xtmes( 12)
     DIMENSION xdmes( 12)
     DIMENSION xesta( 12)
     FOR i = 1 TO 12
          xtmes( i) = 0
          xdmes( i) = 0
          xesta( i) = SPACE(4)
     ENDFOR
     STORE SPACE(40) TO w_cond
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 01 SAY 'Del Fecha  :'
     @ 04, 38 SAY 'Al Fecha  :'
     @ 05, 01 SAY 'Del Emisor :'
     @ 05, 38 SAY 'Al Emisor :'
     @ 06, 01 SAY 'Del Taller :'
     @ 06, 38 SAY 'Al Taller :'
     @ 07, 01 SAY 'Del T‚cnico:'
     @ 07, 38 SAY 'Al T‚cnico:'
     @ 09, 01 SAY 'Orden por  :'  ;
       SIZE 01, 16, 0
     @ 09, 38 SAY 'Informe   :'  ;
       SIZE 01, 16, 0
     @ 12, 01 SAY 'Salida por :'  ;
       SIZE 01, 16, 0
     @ 04, 14 GET fecha1 WHEN  ;
       antes(2)
     @ 04, 50 GET fecha2 RANGE  ;
       fecha1 VALID fecha2 >=  ;
       fecha1 WHEN antes(2)
     @ 05, 14 GET emisor1 PICTURE  ;
       '@!' VALID valida(emisor1, ;
       1) WHEN antes(1)
     @ 05, 50 GET emisor2 RANGE  ;
       emisor1 PICTURE '@!' VALID  ;
       valida(emisor2,2) .AND.  ;
       emisor2 >= emisor1 WHEN  ;
       antes(1)
     @ 06, 14 GET taller1 PICTURE  ;
       '@!' VALID valida(taller1, ;
       1) WHEN antes(1)
     @ 06, 50 GET taller2 RANGE  ;
       taller1 PICTURE '@!' VALID  ;
       valida(taller2,2) .AND.  ;
       taller2 >= taller1 WHEN  ;
       antes(1)
     @ 07, 14 GET tecni1 PICTURE  ;
       '@!' VALID valida(tecni1, ;
       1) WHEN antes(1)
     @ 07, 50 GET tecni2 RANGE  ;
       tecni1 PICTURE '@!' VALID  ;
       valida(tecni2,2) .AND.  ;
       tecni2 >= tecni1 WHEN  ;
       antes(1)
     @ 09, 14 GET w_orden DEFAULT  ;
       1 PICTURE  ;
       '@*RVTN Fecha  ;T‚cnico'  ;
       WHEN antes(2)
     @ 09, 50 GET w_informe  ;
       DEFAULT 1 PICTURE  ;
       '@*RVTN Diario ;Semanal;Mensual;Mens.Objetivo'
     @ 12, 14 GET w_salida  ;
       DEFAULT 1 PICTURE  ;
       '@*RVTN Pantalla ;Impresora'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          sigue = .F.
          LOOP
     ENDIF
     @ 09, 50 GET w_informe  ;
       DEFAULT 1 PICTURE  ;
       '@*RVTN Diario ;Semanal;Mensual;Mens.Objetivo'
     READ
     IF LASTKEY() = 27
          LOOP
     ENDIF
     @ 12, 14 GET w_salida  ;
       DEFAULT 1 PICTURE  ;
       '@*RVTN Pantalla ;Impresora'
     READ
     IF LASTKEY() = 27
          LOOP
     ENDIF
     IF w_salida = 2
          nco = 1
          @ 14, 01 SAY  ;
            'Copias Nro.:' GET  ;
            nco PICTURE '99'  ;
            VALID nco > 0 WHEN  ;
            antes(2)
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     CREATE CURSOR DOMI4 (numdoc  ;
            C (8), fecemi D,  ;
            indori C (4), indest  ;
            C (4), mesran N (2,  ;
            0), codtec C (9),  ;
            auxest C (4), codtall  ;
            C (4), fecfin D,  ;
            codemi C (4), aniran  ;
            N (4, 0), horemi C  ;
            (8), horfin C (8),  ;
            numse N (2, 0), numme  ;
            N (2, 0), numan N (4,  ;
            0), fec_pri D, vrec N  ;
            (5), numser C (20),  ;
            codmod C (15), codmar  ;
            C (4))
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     DO CASE
          CASE taller1 > '010 '
               xesta( 1) = '003 '
               xesta( 2) = '004 '
               xesta( 3) = '007 '
               xesta( 4) = '011 '
               xesta( 5) = '012 '
               xesta( 6) = '013 '
               xesta( 7) = '014 '
               xesta( 8) = '015 '
               xesta( 9) = '016 '
               xesta( 10) =  ;
                    '017 '
          CASE taller1 <= '010 '
               xesta( 1) = '001 '
               xesta( 2) = '002 '
               xesta( 3) = '003 '
               xesta( 4) = '004 '
               xesta( 5) = '006 '
               xesta( 6) = '005 '
               xesta( 7) = '007 '
               xesta( 8) = '008 '
               xesta( 9) = '009 '
     ENDCASE
     w_dlab = dialab(fecha1, ;
              fecha2)
     w_dmes = diames(YEAR(fecha1), ;
              MONTH(fecha1), ;
              YEAR(fecha2), ;
              MONTH(fecha2))
     w_codtall = SPACE(4)
     SELECT st_iorep
     SET NEAR ON
     SEEK DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE fecemi >= fecha1  ;
          .AND. fecemi <= fecha2  ;
          .AND.  .NOT. EOF()
          w_codtall = xtall(codtec)
          IF w_codtall = SPACE(4)
               w_codtall = codtall
          ENDIF
          IF (codemi >= emisor1  ;
             .AND. codemi <=  ;
             emisor2) .AND.  ;
             (w_codtall >=  ;
             taller1 .AND.  ;
             w_codtall <=  ;
             taller2) .AND.  ;
             (codtec >=  ;
             STR(tecni1, 9) .AND.  ;
             codtec <= STR(tecni2,  ;
             9) .AND. indest <>  ;
             'N   ') .AND.  ;
             (fecfin = CTOD( ;
             '  /  /  ') .OR.  ;
             fecfin > fecha2)
               w_reg = w_reg + 1
               SELECT domi4
               APPEND BLANK
               REPLACE fec_pri  ;
                       WITH  ;
                       st_iorep.fecemi
               REPLACE numdoc  ;
                       WITH  ;
                       st_iorep.numdoc,  ;
                       fecemi  ;
                       WITH  ;
                       st_iorep.fecemi
               REPLACE indori  ;
                       WITH  ;
                       st_iorep.indori,  ;
                       indest  ;
                       WITH  ;
                       st_iorep.indest
               REPLACE auxest  ;
                       WITH  ;
                       st_iorep.auxest,  ;
                       codtec  ;
                       WITH  ;
                       st_iorep.codtec
               REPLACE codtall  ;
                       WITH  ;
                       st_iorep.codtall,  ;
                       fecfin  ;
                       WITH  ;
                       st_iorep.fecfin
               REPLACE codemi  ;
                       WITH  ;
                       st_iorep.codemi,  ;
                       horemi  ;
                       WITH  ;
                       st_iorep.horemi
               REPLACE horfin  ;
                       WITH  ;
                       st_iorep.horfin,  ;
                       numse WITH  ;
                       sema(st_iorep.fecemi)
               REPLACE numme WITH  ;
                       MONTH(st_iorep.fecemi),  ;
                       numan WITH  ;
                       YEAR(st_iorep.fecemi)
               REPLACE mesran  ;
                       WITH  ;
                       MONTH(st_iorep.fecemi),  ;
                       aniran  ;
                       WITH  ;
                       YEAR(st_iorep.fecemi)
               REPLACE numser  ;
                       WITH  ;
                       st_iorep.numser,  ;
                       codmod  ;
                       WITH  ;
                       st_iorep.codmod,  ;
                       codmar  ;
                       WITH  ;
                       st_iorep.codmar
          ENDIF
          SELECT st_iorep
          IF codtall < '010 '
               w_codtec = xtec(numdoc)
               IF w_codtec <>  ;
                  codtec .AND.  ;
                  w_codtec <>  ;
                  SPACE(9)
                    w_codtall = xtall(w_codtec)
                    IF w_codtall =  ;
                       SPACE(4)
                         w_codtall =  ;
                          codtall
                    ENDIF
                    IF (codemi >=  ;
                       emisor1  ;
                       .AND.  ;
                       codemi <=  ;
                       emisor2)  ;
                       .AND.  ;
                       (w_codtall >=  ;
                       taller1  ;
                       .AND.  ;
                       w_codtall <=  ;
                       taller2)  ;
                       .AND.  ;
                       (w_codtec >=  ;
                       STR(tecni1,  ;
                       9) .AND.  ;
                       w_codtec <=  ;
                       STR(tecni2,  ;
                       9) .AND.  ;
                       indest <>  ;
                       'N   '  ;
                       .AND.  ;
                       fecfin =  ;
                       CTOD( ;
                       '  /  /  ' ;
                       ))
                         w_reg = w_reg +  ;
                                 1
                         SELECT domi4
                         APPEND BLANK
                         REPLACE fec_pri  ;
                                 WITH  ;
                                 st_iorep.fecemi
                         REPLACE numdoc  ;
                                 WITH  ;
                                 st_iorep.numdoc,  ;
                                 fecemi  ;
                                 WITH  ;
                                 st_iorep.fecemi
                         REPLACE indori  ;
                                 WITH  ;
                                 st_iorep.indori,  ;
                                 indest  ;
                                 WITH  ;
                                 st_iorep.indest
                         REPLACE auxest  ;
                                 WITH  ;
                                 st_iorep.auxest,  ;
                                 codtec  ;
                                 WITH  ;
                                 w_codtec
                         REPLACE codtall  ;
                                 WITH  ;
                                 st_iorep.codtall,  ;
                                 fecfin  ;
                                 WITH  ;
                                 st_iorep.fecfin
                         REPLACE codemi  ;
                                 WITH  ;
                                 st_iorep.codemi,  ;
                                 horemi  ;
                                 WITH  ;
                                 st_iorep.horemi
                         REPLACE horfin  ;
                                 WITH  ;
                                 st_iorep.horfin,  ;
                                 numse  ;
                                 WITH  ;
                                 sema(st_iorep.fecemi)
                         REPLACE numme  ;
                                 WITH  ;
                                 MONTH(st_iorep.fecemi),  ;
                                 numan  ;
                                 WITH  ;
                                 YEAR(st_iorep.fecemi)
                         REPLACE mesran  ;
                                 WITH  ;
                                 MONTH(st_iorep.fecemi),  ;
                                 aniran  ;
                                 WITH  ;
                                 YEAR(st_iorep.fecemi)
                         REPLACE numser  ;
                                 WITH  ;
                                 st_iorep.numser,  ;
                                 codmod  ;
                                 WITH  ;
                                 st_iorep.codmod,  ;
                                 codmar  ;
                                 WITH  ;
                                 st_iorep.codmar
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SELECT st_iorep
     SET ORDER TO ORD_FECIND
     SET NEAR ON
     SEEK DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE fecfin >= fecha1  ;
          .AND. fecfin <= fecha2  ;
          .AND.  .NOT. EOF()
          w_codtall = xtall(codtec)
          IF w_codtall = SPACE(4)
               w_codtall = codtall
          ENDIF
          IF (codemi >= emisor1  ;
             .AND. codemi <=  ;
             emisor2) .AND.  ;
             (w_codtall >=  ;
             taller1 .AND.  ;
             w_codtall <=  ;
             taller2) .AND.  ;
             (codtec >=  ;
             STR(tecni1, 9) .AND.  ;
             codtec <= STR(tecni2,  ;
             9) .AND. indest <>  ;
             'N   ')
               w_reg = w_reg + 1
               SELECT domi4
               APPEND BLANK
               REPLACE fec_pri  ;
                       WITH  ;
                       st_iorep.fecfin
               REPLACE numdoc  ;
                       WITH  ;
                       st_iorep.numdoc,  ;
                       fecemi  ;
                       WITH  ;
                       st_iorep.fecemi
               REPLACE indori  ;
                       WITH  ;
                       st_iorep.indori,  ;
                       indest  ;
                       WITH  ;
                       st_iorep.indest
               REPLACE auxest  ;
                       WITH  ;
                       st_iorep.auxest,  ;
                       codtec  ;
                       WITH  ;
                       st_iorep.codtec
               REPLACE codtall  ;
                       WITH  ;
                       st_iorep.codtall,  ;
                       fecfin  ;
                       WITH  ;
                       st_iorep.fecfin
               REPLACE codemi  ;
                       WITH  ;
                       st_iorep.codemi,  ;
                       horemi  ;
                       WITH  ;
                       st_iorep.horemi
               REPLACE horfin  ;
                       WITH  ;
                       st_iorep.horfin,  ;
                       numse WITH  ;
                       sema(st_iorep.fecfin)
               REPLACE numme WITH  ;
                       MONTH(st_iorep.fecfin),  ;
                       numan WITH  ;
                       YEAR(st_iorep.fecfin)
               REPLACE mesran  ;
                       WITH  ;
                       MONTH(fecfin),  ;
                       aniran  ;
                       WITH  ;
                       YEAR(st_iorep.fecfin)
               REPLACE numser  ;
                       WITH  ;
                       st_iorep.numser,  ;
                       codmod  ;
                       WITH  ;
                       st_iorep.codmod,  ;
                       codmar  ;
                       WITH  ;
                       st_iorep.codmar
          ENDIF
          SELECT st_iorep
          IF codtall < '010 '
               w_codtec = xtec(numdoc)
               IF w_codtec <>  ;
                  codtec .AND.  ;
                  w_codtec <>  ;
                  SPACE(9)
                    w_codtall = xtall(w_codtec)
                    IF w_codtall =  ;
                       SPACE(4)
                         w_codtall =  ;
                          codtall
                    ENDIF
                    IF (codemi >=  ;
                       emisor1  ;
                       .AND.  ;
                       codemi <=  ;
                       emisor2)  ;
                       .AND.  ;
                       (w_codtall >=  ;
                       taller1  ;
                       .AND.  ;
                       w_codtall <=  ;
                       taller2)  ;
                       .AND.  ;
                       (w_codtec >=  ;
                       STR(tecni1,  ;
                       9) .AND.  ;
                       w_codtec <=  ;
                       STR(tecni2,  ;
                       9) .AND.  ;
                       indest <>  ;
                       'N   ')
                         w_reg = w_reg +  ;
                                 1
                         SELECT domi4
                         APPEND BLANK
                         REPLACE fec_pri  ;
                                 WITH  ;
                                 st_iorep.fecfin
                         REPLACE numdoc  ;
                                 WITH  ;
                                 st_iorep.numdoc,  ;
                                 fecemi  ;
                                 WITH  ;
                                 st_iorep.fecemi
                         REPLACE indori  ;
                                 WITH  ;
                                 st_iorep.indori,  ;
                                 indest  ;
                                 WITH  ;
                                 st_iorep.indest
                         REPLACE auxest  ;
                                 WITH  ;
                                 st_iorep.auxest,  ;
                                 codtec  ;
                                 WITH  ;
                                 w_codtec
                         REPLACE codtall  ;
                                 WITH  ;
                                 st_iorep.codtall,  ;
                                 fecfin  ;
                                 WITH  ;
                                 st_iorep.fecfin
                         REPLACE codemi  ;
                                 WITH  ;
                                 st_iorep.codemi,  ;
                                 horemi  ;
                                 WITH  ;
                                 st_iorep.horemi
                         REPLACE horfin  ;
                                 WITH  ;
                                 st_iorep.horfin,  ;
                                 numse  ;
                                 WITH  ;
                                 sema(st_iorep.fecfin)
                         REPLACE numme  ;
                                 WITH  ;
                                 MONTH(st_iorep.fecfin),  ;
                                 numan  ;
                                 WITH  ;
                                 YEAR(st_iorep.fecfin)
                         REPLACE mesran  ;
                                 WITH  ;
                                 MONTH(fecfin),  ;
                                 aniran  ;
                                 WITH  ;
                                 YEAR(st_iorep.fecfin)
                         REPLACE numser  ;
                                 WITH  ;
                                 st_iorep.numser,  ;
                                 codmod  ;
                                 WITH  ;
                                 st_iorep.codmod,  ;
                                 codmar  ;
                                 WITH  ;
                                 st_iorep.codmar
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SET ORDER TO ORD_FECDOC
     IF w_reg = 0
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO error WITH  ;
             '*** No hay informaci¢n para el Reporte ***'
     ELSE
          DO recla
          fildx1 = SYS(3)
          SELECT domi4
          GOTO TOP
          DO CASE
               CASE w_orden = 1
                    DO CASE
                         CASE w_informe =  ;
                              1
                              INDEX ON;
DTOS(FEC_PRI)+CODTEC+CODTALL to &fildx1;

                         CASE w_informe =  ;
                              2
                              INDEX ON;
STR(NUMAN,4)+STR(NUMME,2)+STR(NUMSE,2)+CODTEC+CODTALL;
to &fildx1 
                         CASE w_informe =  ;
                              3
                              INDEX ON;
STR(NUMAN,4)+STR(NUMME,2)+CODTEC+CODTALL;
to &fildx1 
                         CASE w_informe =  ;
                              4
                              INDEX ON;
CODTEC+CODTALL to &fildx1 
                    ENDCASE
               CASE w_orden = 2
                    DO CASE
                         CASE w_informe =  ;
                              1
                              INDEX ON;
CODTEC+DTOS(FEC_PRI)+codtall to &fildx1;

                         CASE w_informe =  ;
                              2
                              INDEX ON;
CODTEC+STR(NUMAN,4)+STR(NUMME,2)+STR(NUMSE,2)+codtall;
to &fildx1 
                         CASE w_informe =  ;
                              3
                              INDEX ON;
CODTEC+STR(NUMAN,4)+STR(NUMME,2)+codtall;
to &fildx1 
                         CASE w_informe =  ;
                              4
                              INDEX ON;
CODTEC+STR(ANIRAN,4)+STR(MESRAN,2)+codtall;
to &fildx1
                    ENDCASE
          ENDCASE
          SELECT 6
          CREATE CURSOR PDIA  ;
                 (codtec C (9),  ;
                 destec C (30),  ;
                 fec_pri D,  ;
                 subtal C (4),  ;
                 ce1 N (9, 2),  ;
                 ce2 N (9, 2),  ;
                 ce3 N (9, 2),  ;
                 ce4 N (9, 2),  ;
                 ce5 N (9, 2),  ;
                 tall C (4), ce6  ;
                 N (9, 2), ce7 N  ;
                 (9, 2), ce8 N (9,  ;
                 2), ce9 N (9, 2),  ;
                 ce10 N (9, 2),  ;
                 nce1 N (9, 2),  ;
                 nce2 N (9, 2),  ;
                 nce3 N (9, 2),  ;
                 nce4 N (9, 2),  ;
                 nce5 N (9, 2),  ;
                 nce6 N (9, 2),  ;
                 nce7 N (9, 2),  ;
                 pd1 N (9, 2),  ;
                 pd2 N (9, 2),  ;
                 pd3 N (9, 2),  ;
                 pd4 N (9, 2),  ;
                 cet N (9, 2),  ;
                 ptot N (9, 2),  ;
                 trec N (9, 2),  ;
                 treh N (9, 2),  ;
                 tprod N (9, 2),  ;
                 lab N (8, 2),  ;
                 obj N (8, 2),  ;
                 prods N (9, 2),  ;
                 promr N (9, 2),  ;
                 efic N (9, 2),  ;
                 observ C (25),  ;
                 tobj N (8, 2),  ;
                 tprm N (8, 2),  ;
                 t_proge1 N (9,  ;
                 2), t_proge2 N  ;
                 (9, 2), t_prost  ;
                 N (9, 2))
          DIMENSION psum( 21)
          DO CASE
               CASE w_informe = 1
                    w_cond = 'CODTEC=TECNI AND FEC_PRI=FECHA and w_taller=codtall'
               CASE w_informe = 2
                    w_cond = 'CODTEC=TECNI AND YEAR(FEC_PRI)=YEAR(FECHA) AND MONTH(FEC_PRI)=MONTH(FECHA) AND SEMA(FEC_PRI)=SEMA(FECHA) and  w_taller=codtall '
               CASE w_informe = 3
                    w_cond = 'CODTEC=TECNI AND YEAR(FEC_PRI)=YEAR(FECHA) AND MONTH(FEC_PRI)=MONTH(FECHA) and w_taller=codtall'
               CASE w_informe = 4
                    IF w_orden =  ;
                       1
                         w_cond =  ;
                          'CODTEC=TECNI and w_taller=codtall'
                    ELSE
                         w_cond =  ;
                          'CODTEC=TECNI AND W_ANIO=ANIRAN AND W_MES=MESRAN and w_taller=codtall'
                    ENDIF
          ENDCASE
          SELECT domi4
          GOTO TOP
          DO WHILE  .NOT. EOF()
               w_taller = codtall
               tecni = codtec
               fecha = fec_pri
               w_anio = aniran
               w_mes = mesran
               STORE 0 TO psum(  ;
                     1), psum( 2),  ;
                     psum( 3),  ;
                     psum( 4),  ;
                     psum( 5),  ;
                     psum( 6),  ;
                     psum( 7),  ;
                     psum( 8),  ;
                     psum( 9),  ;
                     psum( 10),  ;
                     psum( 11)
               STORE 0 TO psum(  ;
                     12), psum(  ;
                     13), psum(  ;
                     14), psum(  ;
                     15), psum(  ;
                     16), psum(  ;
                     17), psum(  ;
                     18), psum(  ;
                     19), psum(  ;
                     20), psum(  ;
                     21)
               STORE 0 TO toth,  ;
                     mini, minf,  ;
                     var, ctmp,  ;
                     tpsum, vreh,  ;
                     tvrec
               SCAN WHILE &W_COND AND;
!EOF()
                    IF fecemi >=  ;
                       fecha1  ;
                       .AND.  ;
                       fecemi <=  ;
                       fecha2
                         ctmp = ctmp +  ;
                                1
                    ENDIF
                    tvrec = tvrec +  ;
                            vrec
                    vreh = vreh +  ;
                           valest(numdoc)
                    IF fecfin <>  ;
                       CTOD( ;
                       '  /  /  ' ;
                       ) .AND.  ;
                       fecfin >=  ;
                       fecha1  ;
                       .AND.  ;
                       fecfin <=  ;
                       fecha2
                         mini = (VAL(SUBSTR(horemi,  ;
                                1,  ;
                                2)) *  ;
                                60) +  ;
                                VAL(SUBSTR(horemi,  ;
                                4,  ;
                                2))
                         minf = (VAL(SUBSTR(horfin,  ;
                                1,  ;
                                2)) *  ;
                                60) +  ;
                                VAL(SUBSTR(horfin,  ;
                                4,  ;
                                2))
                         var = (fecfin -  ;
                               fecemi) *  ;
                               24
                         var = var *  ;
                               60
                         toth = ROUND(((var +  ;
                                minf) -  ;
                                mini) /  ;
                                60,  ;
                                2)
                         DO CASE
                              CASE  ;
                               toth <=  ;
                               4
                                   psum( 1) = psum(1) + 1
                              CASE  ;
                               toth >  ;
                               4  ;
                               .AND.  ;
                               toth <=  ;
                               8
                                   psum( 2) = psum(2) + 1
                              CASE  ;
                               toth >  ;
                               8  ;
                               .AND.  ;
                               toth <=  ;
                               12
                                   psum( 3) = psum(3) + 1
                              CASE  ;
                               toth >  ;
                               12  ;
                               .AND.  ;
                               toth <=  ;
                               24
                                   psum( 4) = psum(4) + 1
                              CASE  ;
                               toth >  ;
                               24  ;
                               .AND.  ;
                               toth <=  ;
                               48
                                   psum( 5) = psum(5) + 1
                              CASE  ;
                               toth >  ;
                               48  ;
                               .AND.  ;
                               toth <=  ;
                               72
                                   psum( 6) = psum(6) + 1
                              CASE  ;
                               toth >  ;
                               72  ;
                               .AND.  ;
                               toth <=  ;
                               96
                                   psum( 7) = psum(7) + 1
                              CASE  ;
                               toth >  ;
                               96  ;
                               .AND.  ;
                               toth <=  ;
                               120
                                   psum( 8) = psum(8) + 1
                              CASE  ;
                               toth >  ;
                               120  ;
                               .AND.  ;
                               toth <=  ;
                               144
                                   psum( 9) = psum(9) + 1
                              CASE  ;
                               toth >  ;
                               144
                                   psum( 10) = psum(10) + 1
                         ENDCASE
                    ELSE
                         IF codtall >  ;
                            '010 '
                              DO CASE
                                   CASE auxest = '003 '
                                        psum( 11) = psum(11) + 1
                                   CASE auxest = '007 '
                                        psum( 12) = psum(12) + 1
                                   CASE auxest = '011 '
                                        psum( 13) = psum(13) + 1
                                   CASE auxest = '012 '
                                        psum( 14) = psum(14) + 1
                                   CASE auxest = '013 '
                                        psum( 15) = psum(15) + 1
                                   CASE auxest = '014 '
                                        psum( 16) = psum(16) + 1
                                   CASE auxest = '016 '
                                        psum( 17) = psum(17) + 1
                                   CASE auxest = '004 '
                                        psum( 18) = psum(18) + 1
                                   CASE auxest = '015 '
                                        psum( 19) = psum(19) + 1
                                   CASE auxest = '017 '
                                        psum( 20) = psum(20) + 1
                                   OTHERWISE
                                        psum( 21) = psum(21) + 1
                              ENDCASE
                         ELSE
                              DO CASE
                                   CASE auxest = '001 '
                                        psum( 11) = psum(11) + 1
                                   CASE auxest = '002 '
                                        psum( 12) = psum(12) + 1
                                   CASE auxest = '003 '
                                        psum( 13) = psum(13) + 1
                                   CASE auxest = '005 '
                                        psum( 14) = psum(14) + 1
                                   CASE auxest = '007 '
                                        psum( 15) = psum(15) + 1
                                   CASE auxest = '008 '
                                        psum( 16) = psum(16) + 1
                                   CASE auxest = '009 '
                                        psum( 17) = psum(17) + 1
                                   CASE auxest = '004 '
                                        psum( 18) = psum(18) + 1
                                   CASE auxest = '006 '
                                        psum( 19) = psum(19) + 1
                                   OTHERWISE
                                        psum( 20) = psum(20) + 1
                              ENDCASE
                         ENDIF
                    ENDIF
               ENDSCAN
               FOR i = 1 TO 21
                    tpsum = tpsum +  ;
                            psum(i)
               ENDFOR
               SELECT pdia
               APPEND BLANK
               REPLACE codtec  ;
                       WITH  ;
                       tecni
               REPLACE fec_pri  ;
                       WITH  ;
                       fecha
               REPLACE tall WITH  ;
                       w_taller
               REPLACE ce1 WITH  ;
                       psum(1),  ;
                       ce2 WITH  ;
                       psum(2),  ;
                       ce3 WITH  ;
                       psum(3),  ;
                       ce4 WITH  ;
                       psum(4)
               REPLACE ce5 WITH  ;
                       psum(5),  ;
                       ce6 WITH  ;
                       psum(6),  ;
                       ce7 WITH  ;
                       psum(7),  ;
                       ce8 WITH  ;
                       psum(8)
               REPLACE ce9 WITH  ;
                       psum(9),  ;
                       ce10 WITH  ;
                       psum(10)
               REPLACE cet WITH  ;
                       psum(1) +  ;
                       psum(2) +  ;
                       psum(3) +  ;
                       psum(4) +  ;
                       psum(5) +  ;
                       psum(6) +  ;
                       psum(7) +  ;
                       psum(8) +  ;
                       psum(9) +  ;
                       psum(10)
               REPLACE nce1 WITH  ;
                       psum(11),  ;
                       nce2 WITH  ;
                       psum(12),  ;
                       nce3 WITH  ;
                       psum(13),  ;
                       nce4 WITH  ;
                       psum(14)
               REPLACE nce5 WITH  ;
                       psum(15),  ;
                       nce6 WITH  ;
                       psum(16),  ;
                       nce7 WITH  ;
                       psum(17),  ;
                       pd1 WITH  ;
                       psum(18)
               REPLACE pd2 WITH  ;
                       psum(19),  ;
                       pd3 WITH  ;
                       psum(20),  ;
                       pd4 WITH  ;
                       psum(21)
               REPLACE ptot WITH  ;
                       ctmp, trec  ;
                       WITH tvrec,  ;
                       treh WITH  ;
                       vreh
               w_tot = w_tot +  ;
                       ctmp
               STORE 0 TO w_fac,  ;
                     w_aux
               SELECT st_itecn
               SEEK pdia.codtec
               IF FOUND()
                    SELECT ge_tab0
                    SEEK 'SUBT' +  ;
                         st_itecn.subtal
                    IF FOUND()
                         w_fac = tab_factor
                    ENDIF
                    SELECT pdia
                    REPLACE destec  ;
                            WITH  ;
                            st_itecn.noment
                    REPLACE observ  ;
                            WITH  ;
                            st_itecn.observ
                    REPLACE subtal  ;
                            WITH  ;
                            st_itecn.subtal
                    w_aux = cet -  ;
                            ((trec *  ;
                            w_fac) +  ;
                            treh)
                    REPLACE tprod  ;
                            WITH  ;
                            w_aux
                    w_total = w_total +  ;
                              w_aux
                    REPLACE t_proge1  ;
                            WITH  ;
                            w_total
                    REPLACE t_proge2  ;
                            WITH  ;
                            w_total
                    IF w_orden =  ;
                       1
                         w_dtec =  ;
                          dtec(pdia.codtec, ;
                          YEAR(fecha1), ;
                          MONTH(fecha1), ;
                          YEAR(fecha2), ;
                          MONTH(fecha2), ;
                          w_taller)
                    ELSE
                         w_dtec =  ;
                          dtec(pdia.codtec, ;
                          w_anio, ;
                          w_mes, ;
                          w_anio, ;
                          w_mes, ;
                          w_taller)
                         w_dmes =  ;
                          diames(w_anio, ;
                          w_mes, ;
                          w_anio, ;
                          w_mes)
                         IF ((w_anio *  ;
                            100) +  ;
                            w_mes) <  ;
                            ((YEAR(fecha2) *  ;
                            100) +  ;
                            MONTH(fecha2))
                              w_dlab =  ;
                               w_dmes
                         ELSE
                              w_dlab =  ;
                               dialab(fecha2 -  ;
                               DAY(fecha2) +  ;
                               1, ;
                               fecha2)
                         ENDIF
                    ENDIF
                    IF w_dtec <=  ;
                       w_dlab
                         w_aux = w_dtec
                    ELSE
                         w_aux = w_dlab
                    ENDIF
                    SELECT pdia
                    IF w_informe =  ;
                       4
                         REPLACE lab  ;
                                 WITH  ;
                                 w_aux
                    ELSE
                         REPLACE lab  ;
                                 WITH  ;
                                 w_dtec
                    ENDIF
                    SELECT ge_tab0
                    IF w_taller <  ;
                       '010 '
                         SEEK 'CLAT' +  ;
                              st_itecn.codcla
                    ELSE
                         SEEK 'CLAT' +  ;
                              st_itecn.codcl2
                    ENDIF
                    IF FOUND()
                         SELECT pdia
                         REPLACE obj  ;
                                 WITH  ;
                                 ge_tab0.tab_factor
                         REPLACE prods  ;
                                 WITH  ;
                                 ROUND(lab *  ;
                                 obj,  ;
                                 2)
                         IF lab >  ;
                            0
                              REPLACE  ;
                               promr  ;
                               WITH  ;
                               ROUND(tprod /  ;
                               lab,  ;
                               2)
                         ENDIF
                         IF obj >  ;
                            0
                              REPLACE  ;
                               efic  ;
                               WITH  ;
                               ROUND((promr /  ;
                               obj) *  ;
                               100,  ;
                               2)
                         ENDIF
                    ENDIF
               ENDIF
               SELECT domi4
               IF codtec <> tecni
                    w_sub = w_sub +  ;
                            1
               ENDIF
          ENDDO
          SELECT pdia
          GOTO TOP
          STORE 0 TO w_tobj,  ;
                w_tprm, w_recno
          w_ctec = codtec
          SCAN WHILE  .NOT. EOF()
               w_recno = RECNO()
               IF w_ctec = codtec
                    w_tobj = w_tobj +  ;
                             obj
                    w_tprm = w_tprm +  ;
                             promr
               ELSE
                    REPLACE tobj  ;
                            WITH  ;
                            w_tobj,  ;
                            tprm  ;
                            WITH  ;
                            w_tprm  ;
                            FOR  ;
                            codtec =  ;
                            w_ctec
                    GOTO w_recno
                    w_ctec = codtec
                    w_tobj = obj
                    w_tprm = promr
               ENDIF
          ENDSCAN
          REPLACE tobj WITH  ;
                  w_tobj, tprm  ;
                  WITH w_tprm FOR  ;
                  codtec =  ;
                  w_ctec
          IF w_informe = 4
               fildx2 = SYS(3)
               INDEX ON CODTEC+substr(tall,1,2);
to &fildx2 
               GOTO TOP
               DO WHILE  .NOT.  ;
                  EOF()
                    STORE 0 TO  ;
                          promr3,  ;
                          tjb3
                    w_tec1 = codtec
                    DO WHILE  ;
                       codtec= ;
                       w_tec1  ;
                       .AND.   ;
                       .NOT.  ;
                       EOF()
                         w_lab3 =  ;
                          lab
                         w_obj3 =  ;
                          obj
                         w_pro3 =  ;
                          prods
                         w_efi3 =  ;
                          efic
                         w_tal1 =  ;
                          SUBSTR(tall,  ;
                          1, 2)
                         STORE 0  ;
                               TO  ;
                               w_repa3
                         SCAN WHILE  ;
                              codtec =  ;
                              w_tec1  ;
                              .AND.  ;
                              SUBSTR(tall,  ;
                              1,  ;
                              2) =  ;
                              w_tal1
                              REPLACE  ;
                               lab  ;
                               WITH  ;
                               0,  ;
                               obj  ;
                               WITH  ;
                               0,  ;
                               prods  ;
                               WITH  ;
                               0,  ;
                               promr  ;
                               WITH  ;
                               0,  ;
                               efic  ;
                               WITH  ;
                               0
                              w_repa3 =  ;
                               w_repa3 +  ;
                               tprod
                         ENDSCAN
                         IF  .NOT.  ;
                             BOF()
                              SKIP - ;
                               1
                              IF codtec =  ;
                                 w_tec1
                                   REPLACE lab WITH w_lab3, obj WITH w_obj3, prods WITH ROUND(lab * obj, 2)
                                   IF lab > 0
                                        REPLACE promr WITH ROUND(w_repa3 / lab, 2)
                                   ENDIF
                                   IF obj > 0
                                        REPLACE efic WITH ROUND((promr / obj) * 100, 2)
                                   ENDIF
                                   promr3 = promr3 + promr
                                   IF lab > 0
                                        tjb3 = tjb3 + obj
                                   ENDIF
                              ENDIF
                              IF w_tal1 =  ;
                                 '01'
                                   REPLACE tprm WITH promr3
                                   IF tjb3 > 0
                                        REPLACE tobj WITH ROUND((tprm / tjb3 * 100), 2)
                                   ENDIF
                              ENDIF
                              SKIP
                         ENDIF
                    ENDDO
               ENDDO
          ENDIF
          IF w_informe = 4
               fildx2 = SYS(3)
               IF w_orden = 1
                    IF w_informe =  ;
                       4
                         INDEX ON substr(tall,1,2)+SUBTAL+CODTEC;
to &fildx2 
                    ELSE
                         INDEX ON tall+SUBTAL+CODTEC;
to &fildx2 
                    ENDIF
               ELSE
                    IF w_informe =  ;
                       4
                         index on substr(tall,1,2)+CODTEC+codtec;
to &fildx2 
                    ELSE
                         INDEX ON tall+CODTEC+codtec;
to &fildx2 
                    ENDIF
               ENDIF
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          GOTO TOP
          IF w_salida = 2
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               SET DEVICE TO PRINTER
               SET PRINTER ON
               ?? CHR(27) +  ;
                  CHR(15)
               FOR i = 1 TO nco
                    IF w_informe =  ;
                       4
                         REPORT FORMAT  ;
                                porl4085  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                porl4084  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDFOR
               ?? CHR(27) +  ;
                  CHR(18)
               SET PRINTER OFF
               SET DEVICE TO SCREEN
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ELSE
               filtxt = SYS(3) +  ;
                        '.TXT'
               IF w_informe = 4
                    SET SYSMENU ON
                    repo form PORL4085;
to file &FILTXT noconsole 
               ELSE
                    repo form PORL4084;
to file &FILTXT noconsole 
               ENDIF
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               modi comm &FILTXT window;
pantall NOEDIT
               DELE FILE &FILTXT
               SET SYSMENU OFF
          ENDIF
     ENDIF
ENDDO
ON KEY
DO sacawin
CLOSE DATABASES
RETURN
*
FUNCTION valest
PARAMETER codigo
x = 0
narea = SELECT()
SELECT st_mvord
SEEK codigo + '008 '
SCAN WHILE orden = codigo .AND.   ;
     .NOT. EOF()
     IF estado = '008 ' .AND.  ;
        (dia >= fecha1 .AND. dia <=  ;
        fecha2)
          x = x + 1
     ENDIF
ENDSCAN
SELECT (narea)
RETURN x
*
FUNCTION xtall
PARAMETER codigo
narea = SELECT()
w_tal = SPACE(4)
SELECT st_itecn
SEEK codigo
IF FOUND()
     w_tal = codtal
ENDIF
SELECT (narea)
RETURN w_tal
*
FUNCTION xtec
PARAMETER codigo
narea = SELECT()
w_tec = SPACE(9)
SELECT st_mvord
SEEK codigo + '016 '
IF FOUND()
     w_tec = tecnico
ENDIF
SELECT (narea)
RETURN w_tec
*
FUNCTION valida
PARAMETER w_codtab, opc
IF EMPTY(w_codtab)
     DO error WITH  ;
        'No se Permiten Blancos'
     RETURN .F.
ENDIF
x = 0
DO CASE
     CASE VARREAD() = 'EMISOR1'  ;
          .OR. VARREAD() =  ;
          'EMISOR2'
          x = 1
          SELECT ge_tab0
          SEEK 'EMIS' + w_codtab
     CASE VARREAD() = 'TALLER1'  ;
          .OR. VARREAD() =  ;
          'TALLER2'
          x = 1
          SELECT ge_tab0
          SEEK 'TALL' + w_codtab
     CASE VARREAD() = 'TECNI1'  ;
          .OR. VARREAD() =  ;
          'TECNI2'
          x = 2
          SELECT st_itecn
          SEEK STR(w_codtab, 9)
ENDCASE
IF  .NOT. FOUND()
     DO error WITH  ;
        'C¢digo No Existe'
     RETURN .F.
ENDIF
DO CASE
     CASE opc = 1 .AND. x = 1
          col = 18
     CASE opc = 2 .AND. x = 1
          col = 54
     CASE opc = 1 .AND. x = 2
          col = 14
     CASE opc = 2 .AND. x = 2
          col = 50
ENDCASE
IF SUBSTR(VARREAD(), 1, 6) =  ;
   'EMISOR' .OR. SUBSTR(VARREAD(),  ;
   1, 6) = 'TALLER'
     @ ROW(), col SAY  ;
       SUBSTR(tab_destab, 1, 20)
ELSE
     IF VARREAD() = 'TECNI1'
          destec1 = SUBSTR(noment,  ;
                    1, 25)
     ELSE
          destec2 = SUBSTR(noment,  ;
                    1, 25)
     ENDIF
     @ ROW() + 1, col SAY  ;
       SUBSTR(noment, 1, 25)
ENDIF
RETURN
*
PROCEDURE ayuda
PARAMETER var
IF SUBSTR(VARREAD(), 1, 6) =  ;
   'EMISOR' .OR. SUBSTR(VARREAD(),  ;
   1, 6) = 'TALLER'
     SELECT ge_tab0
     IF SUBSTR(VARREAD(), 1, 6) =  ;
        'TALLER'
          SET FILTER TO tab_codpre ==;
'TALL'
          titulo = 'AYUDA DE TALLER'
     ELSE
          SET FILTER TO tab_codpre ==;
'EMIS'
          titulo = 'AYUDA DE EMISOR'
     ENDIF
     campo = 'tab_codtab + "  " + tab_destab'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ELSE
     SELECT st_itecn
     campoa = '" "+codent+" "+noment+" "+CODTEC'
     campob = '" "+noment+" "+codent+" "+CODTEC'
     titulo = 'AYUDA DE TECNICOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
     SET ORDER TO codigo
ENDIF
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'ESC'
RETURN
*
PROCEDURE antes
PARAMETER opc
IF opc = 1
     ON KEY LABEL f6 do ayuda
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
ELSE
     ON KEY LABEL f6
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
ENDIF
RETURN
*
FUNCTION sema
PARAMETER fecha
vdia = DAY(fecha) + (7 -  ;
       DOW(fecha))
cmes = INT(vdia / 7)
IF MOD(vdia, 7) > 0
     cmes = cmes + 1
ENDIF
RETURN cmes
*
FUNCTION dtec
PARAMETER codi, an1, mes1, an2,  ;
          mes2, taller
STORE 0 TO w_cont
IF taller > '010 '
     w_cam = 'MED'
ELSE
     w_cam = 'MES'
ENDIF
narea = SELECT()
SELECT st_asiem
SEEK codi + STR(an1, 5)
IF FOUND()
     SCAN WHILE codi = codemp  ;
          .AND. anio <= STR(an2,  ;
          5) .AND.  .NOT. EOF()
          DO CASE
               CASE anio <  ;
                    STR(an2, 5)
                    w = mes1
                    v = 12
               CASE an1 = an2
                    w = mes1
                    v = mes2
               CASE anio >=  ;
                    STR(an2, 5)
                    w = 1
                    v = mes2
          ENDCASE
          FOR i = w TO v
               IF i <= 9
                    varmes = w_cam +  ;
                             '0' +  ;
                             ALLTRIM(STR(i,  ;
                             2))
               ELSE
                    varmes = w_cam +  ;
                             ALLTRIM(STR(i,  ;
                             2))
               ENDIF
               W_CONT=W_CONT+&VARMES
          ENDFOR
     ENDSCAN
ENDIF
SELECT (narea)
RETURN w_cont
*
FUNCTION dialab
PARAMETER fec1, fec2
STORE 0 TO cond
vfec = fec1
w_area = ALIAS()
DO WHILE vfec>=fec1 .AND. vfec<= ;
   fec2
     DO CASE
          CASE DOW(vfec) = 2 .OR.  ;
               DOW(vfec) = 3 .OR.  ;
               DOW(vfec) = 4 .OR.  ;
               DOW(vfec) = 5 .OR.  ;
               DOW(vfec) = 6
               cond = cond + 1
          CASE DOW(vfec) = 7
               cond = cond + 0.5 
     ENDCASE
     SELECT ge_tab0
     IF MONTH(vfec) < 10
          w_meslab = '0' +  ;
                     STR(MONTH(vfec),  ;
                     1)
     ELSE
          w_meslab = STR(MONTH(vfec),  ;
                     2)
     ENDIF
     IF DAY(vfec) < 10
          w_dialab = '0' +  ;
                     STR(DAY(vfec),  ;
                     1)
     ELSE
          w_dialab = STR(DAY(vfec),  ;
                     2)
     ENDIF
     SEEK 'FERI' + w_meslab +  ;
          w_dialab
     IF FOUND()
          cond = cond -  ;
                 tab_factor
     ENDIF
     vfec = vfec + 1
ENDDO
w_area = ALIAS()
RETURN cond
*
FUNCTION diames
PARAMETER an1, mes1, an2, mes2
STORE 0 TO cond
vfec = CTOD('01/' + STR(mes1, 2) +  ;
       '/' + SUBSTR(STR(an1, 4),  ;
       3, 2))
t_mes = mes1
t_anio = (an1 * 100) + mes1
t_anf = (an2 * 100) + mes2
w_area = ALIAS()
DO WHILE t_anio<=t_anf
     DO CASE
          CASE DOW(vfec) = 2 .OR.  ;
               DOW(vfec) = 3 .OR.  ;
               DOW(vfec) = 4 .OR.  ;
               DOW(vfec) = 5 .OR.  ;
               DOW(vfec) = 6
               cond = cond + 1
          CASE DOW(vfec) = 7
               cond = cond + 0.5 
     ENDCASE
     SELECT ge_tab0
     IF MONTH(vfec) < 10
          w_meslab = '0' +  ;
                     STR(MONTH(vfec),  ;
                     1)
     ELSE
          w_meslab = STR(MONTH(vfec),  ;
                     2)
     ENDIF
     IF DAY(vfec) < 10
          w_dialab = '0' +  ;
                     STR(DAY(vfec),  ;
                     1)
     ELSE
          w_dialab = STR(DAY(vfec),  ;
                     2)
     ENDIF
     SEEK 'FERI' + w_meslab +  ;
          w_dialab
     IF FOUND()
          cond = cond -  ;
                 tab_factor
     ENDIF
     vfec = vfec + 1
     t_anio = YEAR(vfec) * 100 +  ;
              MONTH(vfec)
ENDDO
sele &w_area
RETURN cond
*
FUNCTION nomes
PARAMETER mes
DO CASE
     CASE mes = 1
          desmes = 'ENERO    '
     CASE mes = 2
          desmes = 'FEBRERO  '
     CASE mes = 3
          desmes = 'MARZO    '
     CASE mes = 4
          desmes = 'ABRIL    '
     CASE mes = 5
          desmes = 'MAYO     '
     CASE mes = 6
          desmes = 'JUNIO    '
     CASE mes = 7
          desmes = 'JULIO    '
     CASE mes = 8
          desmes = 'AGOSTO   '
     CASE mes = 9
          desmes = 'SETIEMBRE'
     CASE mes = 10
          desmes = 'OCTUBRE  '
     CASE mes = 11
          desmes = 'NOVIEMBRE'
     CASE mes = 12
          desmes = 'DICIEMBRE'
ENDCASE
RETURN desmes
*
PROCEDURE recla
SELECT st_iorep
w_orde = ORDER()
SET ORDER TO ord_mamose
SELECT domi4
fildx2 = SYS(3)
index on codtec to &fildx2
GOTO TOP
SCAN WHILE  .NOT. EOF()
     IF (indori = 'FREC' .OR.  ;
        indori = 'GREC' .OR.  ;
        indori = 'PREC') .AND.  ;
        indest = 'C' .AND.  ;
        (fecfin >= fecha1 .AND.  ;
        fecfin <= fecha2)
          SELECT st_iorep
          SEEK domi4.codmar +  ;
               domi4.codmod +  ;
               domi4.numser
          IF FOUND()
               STORE SPACE(9) TO  ;
                     w_tecnic
               SCAN WHILE codmar =  ;
                    domi4.codmar  ;
                    .AND. codmod =  ;
                    domi4.codmod  ;
                    .AND. numser =  ;
                    domi4.numser  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF numdoc <>  ;
                       domi4.numdoc  ;
                       .AND.  ;
                       numdoc <  ;
                       domi4.numdoc
                         w_tecnic =  ;
                          codtec
                    ENDIF
               ENDSCAN
               SELECT domi4
               w_recno = RECNO()
               SEEK w_tecnic
               IF FOUND()
                    REPLACE vrec  ;
                            WITH  ;
                            vrec +  ;
                            1
               ENDIF
               GOTO w_recno
          ENDIF
          SELECT domi4
     ENDIF
ENDSCAN
SELECT st_iorep
set order to &w_orde
SELECT domi4
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

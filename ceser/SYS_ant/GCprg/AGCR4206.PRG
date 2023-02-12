*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW venta01 FROM 16, 05  ;
       TO 20, 30 TITLE  ;
       'PROCESO DE VENTAS' IN  ;
       screen COLOR SCHEME 20
CLOSE DATABASES
SET CENTURY ON
SELECT 1
USE SHARED GC_HVE00 ORDER  ;
    hve_fecdoc
SELECT 2
USE SHARED ST_IOREP ORDER codigo
SELECT 3
USE SHARED GC_HVEST ORDER CODIGO
SELECT 7
USE SHARED GE_TAB0 ORDER CODIGO
STORE DATE() TO fecha1, fecha2
STORE .T. TO sigue
w_facigv = facigv()
IF w_facigv = 0
     sigue = .F.
ENDIF
DO WHILE sigue
     ACTIVATE WINDOW venta01
     @ 00, 00 SAY 'Desde Fecha:'  ;
       COLOR W/N,N/W  GET fecha1
     @ 01, 00 SAY 'hasta Fecha:'  ;
       COLOR W/N,N/W  GET fecha2  ;
       RANGE fecha1
     READ
     IF LASTKEY() = 27
          EXIT
     ELSE
          rpt = f_yesno( ;
                'Desea procesar ? ' ;
                )
          IF  .NOT. rpt
               RELEASE WINDOW  ;
                       mensj
               EXIT
          ELSE
               = ooaviso( ;
                 'P R O C E S A N D O . . .' ;
                 )
               DO procesa
               SELECT query
               COUNT TO nreg
               IF nreg = 0
                    DO p_mensaje  ;
                       WITH  ;
                       ' NO SE ENCONTRO INFORMACION '
                    LOOP
               ENDIF
               DO proces1
               COPY TO antes
               DO proces2
               DEACTIVATE WINDOW  ;
                          winmensaje
               ?? CHR(7)
               ?? CHR(7)
          ENDIF
     ENDIF
ENDDO
RELEASE WINDOW venta01,  ;
        winmensaje
CLOSE DATABASES
ACTIVATE SCREEN
RETURN
*
PROCEDURE procesa
SELECT 18
USE SHARED st_imode ORDER codigo
SELECT 19
USE SHARED st_isrep ORDER codigo
SELECT 20
USE SHARED st_idfac ORDER codigo
CREATE CURSOR query (tipdoc C (4),  ;
       fecdoc D (8), nrodoc C  ;
       (10), codemi C (4), indori  ;
       C (4), cosrep N (13, 2),  ;
       cosmob N (13, 2), flete N  ;
       (13, 2), otring N (13, 2),  ;
       totnet N (13, 2), linea C  ;
       (4), cantid N (9), sucu C  ;
       (4), zona C (4), tidore C  ;
       (4), nrdore C (10), pagcta  ;
       N (13, 2), anul C (1),  ;
       hve_codmov C (4))
INDEX ON zona + sucu + indori +  ;
      codemi + linea +  ;
      DTOS(fecdoc) TAG codigo
SELECT gc_hve00
SET NEAR ON
SEEK DTOS(fecha1)
SET NEAR OFF
SCAN WHILE hve_fecdoc <= fecha2  ;
     .AND.  .NOT. EOF()
     IF hve_estdoc <> 'A' .AND.  ;
        (hve_tipdoc = 'BOLE' .OR.  ;
        hve_tipdoc = 'FACT' .OR.  ;
        hve_tipdoc = 'LIQU')
          SELECT st_idfac
          SEEK gc_hve00.hve_tipdoc +  ;
               gc_hve00.hve_nrodoc
          IF FOUND()
               STORE 0 TO w_con,  ;
                     w_descu
               w_recno = RECNO()
               SCAN WHILE tipdoc =  ;
                    gc_hve00.hve_tipdoc  ;
                    .AND. nrodoc =  ;
                    gc_hve00.hve_nrodoc  ;
                    .AND.  .NOT.  ;
                    EOF()
                    w_con = w_con +  ;
                            1
               ENDSCAN
               GOTO w_recno
               IF w_con > 0
                    w_descu = ROUND((gc_hve00.hve_totdes /  ;
                              w_facigv) /  ;
                              w_con,  ;
                              2)
               ENDIF
               SCAN WHILE tipdoc =  ;
                    gc_hve00.hve_tipdoc  ;
                    .AND. nrodoc =  ;
                    gc_hve00.hve_nrodoc  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SELECT query
                    APPEND BLANK
                    REPLACE linea  ;
                            WITH  ;
                            st_idfac.linea,  ;
                            codemi  ;
                            WITH  ;
                            st_idfac.codemi
                    REPLACE cosrep  ;
                            WITH  ;
                            ROUND(st_idfac.cosrep /  ;
                            w_facigv,  ;
                            2)
                    REPLACE cosmob  ;
                            WITH  ;
                            ROUND(st_idfac.cosmob /  ;
                            w_facigv,  ;
                            2)
                    REPLACE otring  ;
                            WITH  ;
                            ROUND(st_idfac.otring /  ;
                            w_facigv,  ;
                            2),  ;
                            cantid  ;
                            WITH  ;
                            st_idfac.cantid
                    IF cosrep >  ;
                       cosmob
                         REPLACE cosrep  ;
                                 WITH  ;
                                 cosrep -  ;
                                 w_descu
                    ELSE
                         REPLACE cosmob  ;
                                 WITH  ;
                                 cosmob -  ;
                                 w_descu
                    ENDIF
                    DO graba
                    SELECT st_idfac
               ENDSCAN
          ELSE
               STORE 1 TO  ;
                     w_cantid,  ;
                     w_pasa
               IF gc_hve00.hve_indori =  ;
                  'VENT'
                    w_linea = '0002'
               ELSE
                    STORE '0002'  ;
                          TO  ;
                          w_linea
                    IF SUBSTR(gc_hve00.hve_indori,  ;
                       1, 1) =  ;
                       'F'
                         IF gc_hve00.hve_tidore =  ;
                            'SOLI'
                              SELECT  ;
                               st_isrep
                              SEEK  ;
                               SUBSTR(gc_hve00.hve_nrdore,  ;
                               1,  ;
                               8)
                              IF FOUND()
                                   SELECT st_imode
                                   SEEK st_isrep.codmar + st_isrep.codmod
                                   IF FOUND()
                                        IF linea = '0003'
                                             w_linea = linea
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ELSE
                              SELECT  ;
                               st_iorep
                              SET ORDER;
TO codigo
                              SEEK  ;
                               SUBSTR(gc_hve00.hve_nrdore,  ;
                               1,  ;
                               8)
                              IF FOUND()
                                   SELECT st_imode
                                   SEEK st_iorep.codmar + st_iorep.codmod
                                   IF FOUND()
                                        IF linea = '0003'
                                             w_linea = linea
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ELSE
                         IF gc_hve00.hve_tidore =  ;
                            'SOLI'
                              SELECT  ;
                               st_isrep
                              SEEK  ;
                               SUBSTR(gc_hve00.hve_nrdore,  ;
                               1,  ;
                               8)
                              IF FOUND()
                                   SELECT st_imode
                                   SEEK st_isrep.codmar + st_isrep.codmod
                                   IF FOUND()
                                        IF linea = '0003'
                                             w_linea = linea
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ELSE
                              SELECT  ;
                               st_iorep
                              SET ORDER;
TO ord_numfab
                              SEEK  ;
                               gc_hve00.hve_tipdoc +  ;
                               gc_hve00.hve_nrodoc
                              IF FOUND()
                                   STORE 0 TO w_02rep, w_02mob, w_02otr, w_02can, w_03rep, w_03mob, w_03otr, w_03can
                                   SCAN WHILE codfabo = gc_hve00.hve_tipdoc .AND. numfabo = gc_hve00.hve_nrodoc .AND.  .NOT. EOF()
                                        SELECT st_imode
                                        SEEK st_iorep.codmar + st_iorep.codmod
                                        IF FOUND()
                                             IF linea = '0003'
                                                  w_linea = linea
                                                  w_03rep = w_03rep + st_iorep.cosrep
                                                  w_03mob = w_03mob + st_iorep.cosmob
                                                  w_03otr = w_03otr + st_iorep.flete
                                                  w_03can = w_03can + 1
                                             ELSE
                                                  w_02rep = w_02rep + st_iorep.cosrep
                                                  w_02mob = w_02mob + st_iorep.cosmob
                                                  w_02otr = w_02otr + st_iorep.flete
                                                  w_02can = w_02can + 1
                                             ENDIF
                                        ENDIF
                                   ENDSCAN
                                   SELECT query
                                   IF w_02can > 0
                                        APPEND BLANK
                                        REPLACE cosrep WITH w_02rep, cosmob WITH w_02mob, otring WITH w_02otr, cantid WITH w_02can, linea WITH w_linea
                                        w_pasa = 2
                                        IF w_03can > 0
                                             w_pasa = 3
                                        ENDIF
                                   ELSE
                                        APPEND BLANK
                                        REPLACE cosrep WITH w_03rep, cosmob WITH w_03mob, otring WITH w_03otr, cantid WITH w_03can, linea WITH w_linea
                                        w_pasa = 2
                                   ENDIF
                              ELSE
                                   SELECT query
                                   APPEND BLANK
                                   pasa = 1
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
               SELECT query
               IF w_pasa = 1
                    APPEND BLANK
                    REPLACE cosrep  ;
                            WITH  ;
                            gc_hve00.hve_cosrep,  ;
                            cosmob  ;
                            WITH  ;
                            gc_hve00.hve_cosmob,  ;
                            otring  ;
                            WITH  ;
                            ROUND(flete /  ;
                            w_facigv,  ;
                            2),  ;
                            cantid  ;
                            WITH  ;
                            w_cantid,  ;
                            linea  ;
                            WITH  ;
                            w_linea
               ENDIF
               DO angra
               DO graba
               DO descu
               IF w_pasa = 3
                    SELECT query
                    APPEND BLANK
                    REPLACE cosrep  ;
                            WITH  ;
                            w_03rep,  ;
                            cosmob  ;
                            WITH  ;
                            w_03mob,  ;
                            otring  ;
                            WITH  ;
                            w_03otr,  ;
                            cantid  ;
                            WITH  ;
                            w_03can,  ;
                            linea  ;
                            WITH  ;
                            w_linea
                    DO angra
                    DO graba
               ENDIF
          ENDIF
     ENDIF
     SELECT gc_hve00
ENDSCAN
RETURN
*
PROCEDURE proces1
SELECT st_iorep
SET ORDER TO codigo
SELECT query
INDEX ON tipdoc + nrodoc TAG  ;
      ventas
INDEX ON nrdore + tidore TAG  ;
      nrdore
GOTO TOP
SCAN WHILE  .NOT. EOF()
     IF pagcta > 0
          IF tidore = 'ORDE'
               SELECT st_iorep
               SEEK SUBSTR(query.nrdore,  ;
                    1, 8)
               IF FOUND()
                    SELECT query
                    w_recno = RECNO()
                    SET ORDER TO nrdore
                    w_numsol = st_iorep.numsol +  ;
                               SPACE(2)
                    SEEK w_numsol
                    SCAN WHILE  ;
                         nrdore =  ;
                         w_numsol  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         IF hve_codmov =  ;
                            'PCTA'
                              REPLACE  ;
                               anul  ;
                               WITH  ;
                               'A'
                         ENDIF
                    ENDSCAN
                    SELECT query
                    SET ORDER TO nrdore
                    GOTO w_recno
               ENDIF
          ELSE
               w_tidore = tidore
               w_nrdore = nrdore
               w_recno = RECNO()
               SET ORDER TO ventas
               SEEK w_tidore +  ;
                    w_nrdore
               IF FOUND()
                    REPLACE anul  ;
                            WITH  ;
                            'A'
               ENDIF
               SET ORDER TO nrdore
               GOTO w_recno
          ENDIF
     ENDIF
     SELECT query
ENDSCAN
SET ORDER TO codigo
RETURN
*
PROCEDURE proces2
SELECT query
DELETE ALL FOR tipdoc = SPACE(4)
GOTO TOP
DO WHILE  .NOT. EOF()
     STORE 0 TO w_mano, w_repu,  ;
           w_unid, w_otro
     STORE linea TO w_linea
     STORE sucu TO w_sucu
     STORE codemi TO w_codemi
     STORE indori TO w_indori
     STORE zona TO w_zona
     SCAN WHILE (fecdoc >= fecha1  ;
          .AND. fecdoc <= fecha2)  ;
          .AND. linea = w_linea  ;
          .AND. codemi = w_codemi  ;
          .AND. sucu = w_sucu  ;
          .AND. indori =  ;
          w_indori
          IF query.anul <> 'A'
               w_mano = w_mano +  ;
                        cosmob
               w_repu = w_repu +  ;
                        cosrep
               w_unid = w_unid +  ;
                        cantid
               w_otro = w_otro +  ;
                        otring
          ENDIF
     ENDSCAN
     SELECT gc_hvest
     SEEK STR(YEAR(fecha2), 4) +  ;
          w_zona + w_sucu +  ;
          w_indori + w_codemi +  ;
          w_linea
     IF  .NOT. FOUND()
          APPEND BLANK
     ENDIF
     STORE SPACE(5) TO mob, rep,  ;
           otr, uni
     DO campo
     DO rbloquea
     REPLACE zona WITH w_zona,  ;
             sucu WITH w_sucu,  ;
             codemi WITH w_codemi,  ;
             linea WITH w_linea,  ;
             indori WITH w_indori,  ;
             ano WITH  ;
             STR(YEAR(fecha2),  ;
             4)
     repl &mob   with w_mano,&rep with;
w_repu,&uni with w_unid,&otr with w_otro
     REPLACE totals WITH enere +  ;
             febre + marre +  ;
             abrre + mayre +  ;
             junre + julre +  ;
             agore + setre +  ;
             octre + novre +  ;
             dicre + enemo +  ;
             febmo + marmo +  ;
             abrmo + maymo +  ;
             junmo + julmo +  ;
             agomo + setmo +  ;
             octmo + novmo +  ;
             dicmo
     IF (eneun + febun + marun +  ;
        abrun + mayun + junun +  ;
        julun + agoun + setun +  ;
        octun + novun + dicun) >  ;
        0
          REPLACE prom WITH  ;
                  ROUND((enere +  ;
                  febre + marre +  ;
                  abrre + mayre +  ;
                  junre + julre +  ;
                  agore + setre +  ;
                  octre + novre +  ;
                  dicre + enemo +  ;
                  febmo + marmo +  ;
                  abrmo + maymo +  ;
                  junmo + julmo +  ;
                  agomo + setmo +  ;
                  octmo + novmo +  ;
                  dicmo) / (eneun +  ;
                  febun + marun +  ;
                  abrun + mayun +  ;
                  junun + julun +  ;
                  agoun + setun +  ;
                  octun + novun +  ;
                  dicun), 2)
     ENDIF
     REPLACE usuari WITH clave,  ;
             fecha WITH DATE(),  ;
             hora WITH TIME()
     UNLOCK
     SELECT query
ENDDO
RETURN
*
PROCEDURE campo
IF MONTH(fecha2) = 1
     mob = 'ENEMO'
     rep = 'ENERE'
     uni = 'ENEUN'
     otr = 'ENEOT'
ELSE
     IF MONTH(fecha2) = 2
          mob = 'FEBMO'
          rep = 'FEBRE'
          uni = 'FEBUN'
          otr = 'FEBOT'
     ELSE
          IF MONTH(fecha2) = 3
               mob = 'MARMO'
               rep = 'MARRE'
               uni = 'MARUN'
               otr = 'MAROT'
          ELSE
               IF MONTH(fecha2) =  ;
                  4
                    mob = 'ABRMO'
                    rep = 'ABRRE'
                    uni = 'ABRUN'
                    otr = 'ABROT'
               ELSE
                    IF MONTH(fecha2) =  ;
                       5
                         mob = 'MAYMO'
                         rep = 'MAYRE'
                         uni = 'MAYUN'
                         otr = 'MAYOT'
                    ELSE
                         IF MONTH(fecha2) =  ;
                            6
                              mob =  ;
                               'JUNMO'
                              rep =  ;
                               'JUNRE'
                              uni =  ;
                               'JUNUN'
                              otr =  ;
                               'JUNOT'
                         ELSE
                              IF MONTH(fecha2) =  ;
                                 7
                                   mob = 'JULMO'
                                   rep = 'JULRE'
                                   uni = 'JULUN'
                                   otr = 'JULOT'
                              ELSE
                                   IF MONTH(fecha2) = 8
                                        mob = 'AGOMO'
                                        rep = 'AGORE'
                                        uni = 'AGOUN'
                                        otr = 'AGOOT'
                                   ELSE
                                        IF MONTH(fecha2) = 9
                                             mob = 'SETMO'
                                             rep = 'SETRE'
                                             uni = 'SETUN'
                                             otr = 'SETOT'
                                        ELSE
                                             IF MONTH(fecha2) = 10
                                                  mob = 'OCTMO'
                                                  rep = 'OCTRE'
                                                  uni = 'OCTUN'
                                                  otr = 'OCTOT'
                                             ELSE
                                                  IF MONTH(fecha2) = 11
                                                       mob = 'NOVMO'
                                                       rep = 'NOVRE'
                                                       uni = 'NOVUN'
                                                       otr = 'NOVOT'
                                                  ELSE
                                                       mob = 'DICMO'
                                                       rep = 'DICRE'
                                                       uni = 'DICUN'
                                                       otr = 'DICOT'
                                                  ENDIF
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
RETURN
*
PROCEDURE angra
REPLACE pagcta WITH  ;
        gc_hve00.hve_pagctd,  ;
        tidore WITH  ;
        gc_hve00.hve_tidore,  ;
        nrdore WITH  ;
        gc_hve00.hve_nrdore,  ;
        codemi WITH  ;
        gc_hve00.hve_codemi
RETURN
*
PROCEDURE graba
REPLACE tipdoc WITH  ;
        gc_hve00.hve_tipdoc,  ;
        nrodoc WITH  ;
        gc_hve00.hve_nrodoc,  ;
        fecdoc WITH  ;
        gc_hve00.hve_fecdoc,  ;
        indori WITH  ;
        gc_hve00.hve_indori,  ;
        hve_codmov WITH  ;
        gc_hve00.hve_codmov
SELECT ge_tab0
SEEK 'EMIS' + query.codemi
IF FOUND()
     SELECT query
     REPLACE sucu WITH  ;
             ge_tab0.tab_parame
     SELECT ge_tab0
     SEEK 'SUCU' + query.sucu
     IF FOUND()
          SELECT query
          REPLACE zona WITH  ;
                  ge_tab0.tab_parame
     ELSE
          SELECT query
     ENDIF
ELSE
     SELECT query
ENDIF
RETURN
*
PROCEDURE descu
SELECT query
IF gc_hve00.hve_totdes > 0
     IF gc_hve00.hve_indori <>  ;
        'VENT'
          IF cosmob > cosrep
               REPLACE query.cosmob  ;
                       WITH  ;
                       query.cosmob -  ;
                       ROUND((gc_hve00.hve_totdes /  ;
                       w_facigv),  ;
                       2)
          ELSE
               REPLACE query.cosrep  ;
                       WITH  ;
                       query.cosrep -  ;
                       ROUND((gc_hve00.hve_totdes /  ;
                       w_facigv),  ;
                       2)
          ENDIF
     ELSE
          REPLACE query.cosrep  ;
                  WITH  ;
                  query.cosrep -  ;
                  ROUND((gc_hve00.hve_totdes /  ;
                  w_facigv), 2)
     ENDIF
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

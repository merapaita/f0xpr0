*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 21, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 04, 02  ;
       TO 21, 77 TITLE  ;
       'REGISTRO DE VENTAS' COLOR  ;
       SCHEME 7
w_progra = PROGRAM()
SELECT 1
USE GC_CLI00 ORDER CODIGO
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE GC_HVE00 ORDER CODIGO
SELECT 4
USE ST_ICLPR ORDER CODIGO
SELECT 5
USE GC_CMV00
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'REGISTRO DE VENTAS',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100000000000000000001', 2
w_progra = PROGRAM()
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE SPACE(4) TO w_emiini,  ;
      w_emifin
STORE 0 TO w_bolini, w_bolfin,  ;
      w_facini, w_facfin
DO WHILE .T.
     ACTIVATE WINDOW cabecera
     SET CURSOR ON
     STORE 1 TO w_nropag, opc1,  ;
           nop1
     @ 00, 01 SAY  ;
       'Per¡odo          :' COLOR  ;
       SCHEME 8
     @ 00, 20 SAY  ;
       'Del            Al'
     @ 02, 01 SAY  ;
       'Clasificaci¢n    :' COLOR  ;
       SCHEME 8
     @ 03, 43 SAY  ;
       'Boleta : Del       Al'
     @ 04, 43 SAY  ;
       'Factura: Del       Al'
     @ 05, 23 SAY  ;
       'Lugar de Emisi¢n :' COLOR  ;
       SCHEME 8
     @ 06, 41 SAY 'Del'
     @ 07, 41 SAY 'Al '
     @ 09, 01 SAY  ;
       'Tipo de Informe :' COLOR  ;
       SCHEME 8
     @ 09, 37 SAY  ;
       'Tipo de Salida : ' COLOR  ;
       SCHEME 8
     @ 00, 24 GET w_fecini  ;
       PICTURE '@D' VALID  .NOT.  ;
       EMPTY(w_fecini) .AND.  ;
       oovalida(VARREAD()) COLOR  ;
       SCHEME 8
     @ 00, 38 GET w_fecfin RANGE  ;
       w_fecini PICTURE '@D'  ;
       VALID  .NOT.  ;
       EMPTY(w_fecfin) COLOR  ;
       SCHEME 8
     @ 02, 19 GET opc1 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVN Todos ;Emisor de Factura'  ;
       VALID oovalida(VARREAD())  ;
       COLOR SCHEME 12
     @ 09, 19 GET opc2 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVN Detalle;Resumen'  ;
       WHEN oowhen(VARREAD())  ;
       COLOR SCHEME 12
     @ 09, 55 GET opc3 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVTN Pantalla ;Impresora'  ;
       COLOR SCHEME 12
     READ CYCLE
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF opc3 = 2
          @ 12, 59 SAY 'Copias :'
          @ 12, 68 GET w_nropag  ;
            PICTURE '99' VALID   ;
            .NOT. EMPTY(w_nropag)  ;
            .AND. w_nropag > 0  ;
            .AND. w_nropag < 6  ;
            COLOR SCHEME 8
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     = ooaviso( ;
       ' T R A B A J A N D O . . .' ;
       )
     w_archi = SUBSTR(f_archivo(),  ;
               1, 8) + '.DBF'
     w_bolein = f_ceros(w_bolini, ;
                4,1)
     w_bolefi = f_ceros(w_bolfin, ;
                4,1)
     w_factin = f_ceros(w_facini, ;
                4,1)
     w_factfi = f_ceros(w_facfin, ;
                4,1)
     SELECT gc_hve00
     COPY STRU TO &w_archi
     SELECT 6
     USE &w_archi EXCLU
     SELECT gc_hve00
     SET ORDER TO HVE_FECDOC
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     IF opc1 = 1
          SCAN WHILE hve_fecdoc <=  ;
               w_fecfin .AND.   ;
               .NOT. EOF()
               IF gc_hve00.hve_tipdoc =  ;
                  'FACT' .OR.  ;
                  gc_hve00.hve_tipdoc =  ;
                  'BOLE'
                    w_numdoc = hve_nrodoc
                    w_tipdoc = hve_tipdoc
                    w_fecdoc = hve_fecdoc
                    w_codent = hve_codent
                    w_tippag = hve_tippag
                    w_codmon = hve_codmon
                    w_estdoc = hve_estdoc
                    w_mtocan = hve_mtocan
                    w_solrep = hve_solrep
                    w_solmob = hve_solmob
                    w_solfle = hve_solfle
                    w_solnet = hve_solnet
                    w_soldes = hve_soldes
                    w_solvta = hve_solvta
                    w_soligv = hve_soligv
                    w_solgen = hve_solgen
                    w_tipcam = hve_tipcam
                    w_pagcts = hve_pagcts
                    w_totgen = hve_totgen
                    w_saldol = hve_totoim
                    SELECT 6
                    APPEND BLANK
                    REPLACE hve_tipdoc  ;
                            WITH  ;
                            w_tipdoc
                    REPLACE hve_nrodoc  ;
                            WITH  ;
                            w_numdoc
                    REPLACE hve_fecdoc  ;
                            WITH  ;
                            w_fecdoc
                    REPLACE hve_codent  ;
                            WITH  ;
                            w_codent
                    REPLACE hve_tippag  ;
                            WITH  ;
                            w_tippag
                    REPLACE hve_estdoc  ;
                            WITH  ;
                            w_estdoc
                    REPLACE hve_codmon  ;
                            WITH  ;
                            w_codmon
                    REPLACE hve_mtocan  ;
                            WITH  ;
                            w_mtocan
                    REPLACE hve_solrep  ;
                            WITH  ;
                            w_solrep
                    REPLACE hve_solmob  ;
                            WITH  ;
                            w_solmob
                    REPLACE hve_solfle  ;
                            WITH  ;
                            w_solfle
                    REPLACE hve_solnet  ;
                            WITH  ;
                            w_solnet
                    REPLACE hve_soldes  ;
                            WITH  ;
                            w_soldes
                    REPLACE hve_solvta  ;
                            WITH  ;
                            w_solvta
                    REPLACE hve_soligv  ;
                            WITH  ;
                            w_soligv
                    REPLACE hve_solgen  ;
                            WITH  ;
                            w_solgen
                    REPLACE hve_tipcam  ;
                            WITH  ;
                            w_tipcam
                    REPLACE hve_pagcts  ;
                            WITH  ;
                            w_pagcts
                    REPLACE hve_totgen  ;
                            WITH  ;
                            w_totgen
                    REPLACE hve_totoim  ;
                            WITH  ;
                            w_saldol
                    REPLACE hve_codemi  ;
                            WITH  ;
                            gc_hve00.hve_codemi
                    SELECT gc_hve00
               ENDIF
          ENDSCAN
     ELSE
          SCAN WHILE hve_fecdoc <=  ;
               w_fecfin .AND.   ;
               .NOT. EOF()
               IF (SUBSTR(hve_nrodoc,  ;
                  1, 4) >=  ;
                  w_bolein .AND.  ;
                  SUBSTR(hve_nrodoc,  ;
                  1, 4) <=  ;
                  w_bolefi) .OR.  ;
                  (SUBSTR(hve_nrodoc,  ;
                  1, 4) >=  ;
                  w_factin .AND.  ;
                  SUBSTR(hve_nrodoc,  ;
                  1, 4) <=  ;
                  w_factfi)
                    IF hve_fecdoc >=  ;
                       w_fecini  ;
                       .AND.  ;
                       hve_fecdoc <=  ;
                       w_fecfin
                         IF hve_codemi >=  ;
                            w_emiini  ;
                            .AND.  ;
                            hve_codemi <=  ;
                            w_emifin
                              IF gc_hve00.hve_tipdoc =  ;
                                 'FACT'  ;
                                 .OR.  ;
                                 gc_hve00.hve_tipdoc =  ;
                                 'BOLE'
                                   w_numdoc = hve_nrodoc
                                   w_tipdoc = hve_tipdoc
                                   w_fecdoc = hve_fecdoc
                                   w_codent = hve_codent
                                   w_estdoc = hve_estdoc
                                   w_tippag = hve_tippag
                                   w_codmon = hve_codmon
                                   w_mtocan = hve_mtocan
                                   w_solrep = hve_solrep
                                   w_solmob = hve_solmob
                                   w_solfle = hve_solfle
                                   w_solnet = hve_solnet
                                   w_soldes = hve_soldes
                                   w_solvta = hve_solvta
                                   w_soligv = hve_soligv
                                   w_solgen = hve_solgen
                                   w_tipcam = hve_tipcam
                                   w_pagcts = hve_pagcts
                                   w_totgen = hve_totgen
                                   w_saldol = hve_totoim
                                   SELECT 6
                                   APPEND BLANK
                                   REPLACE hve_tipdoc WITH w_tipdoc
                                   REPLACE hve_nrodoc WITH w_numdoc
                                   REPLACE hve_fecdoc WITH w_fecdoc
                                   REPLACE hve_codent WITH w_codent
                                   REPLACE hve_estdoc WITH w_estdoc
                                   REPLACE hve_mtocan WITH w_mtocan
                                   REPLACE hve_codmon WITH w_codmon
                                   REPLACE hve_tippag WITH w_tippag
                                   REPLACE hve_solrep WITH w_solrep
                                   REPLACE hve_solmob WITH w_solmob
                                   REPLACE hve_solfle WITH w_solfle
                                   REPLACE hve_solnet WITH w_solnet
                                   REPLACE hve_soldes WITH w_soldes
                                   REPLACE hve_solvta WITH w_solvta
                                   REPLACE hve_soligv WITH w_soligv
                                   REPLACE hve_solgen WITH w_solgen
                                   REPLACE hve_tipcam WITH w_tipcam
                                   REPLACE hve_pagcts WITH w_pagcts
                                   REPLACE hve_totgen WITH w_totgen
                                   REPLACE hve_totoim WITH w_saldol
                                   REPLACE hve_codemi WITH gc_hve00.hve_codemi
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
               SELECT gc_hve00
          ENDSCAN
     ENDIF
     SELECT 6
     COUNT TO nreg
     DEACTIVATE WINDOW winmensaje
     IF nreg = 0
          DO p_mensaje WITH  ;
             ' NO SE ENCONTRO INFORMACION '
          LOOP
     ENDIF
     SELECT 6
     IF opc1 = 1
          INDEX ON  ;
                DTOS(hve_fecdoc) +  ;
                hve_tipdoc +  ;
                hve_nrodoc TAG  ;
                codigo2
     ELSE
          INDEX ON  ;
                DTOS(hve_fecdoc) +  ;
                hve_tipdoc +  ;
                hve_nrodoc TAG  ;
                codigo2
     ENDIF
     SET ORDER TO CODIGO2
     IF opc3 = 1
          = ooimprime(2)
     ELSE
          = ooimprime(3)
     ENDIF
ENDDO
RELEASE WINDOW winpedido,  ;
        cabecera, detalle
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE ooimprime
PARAMETER opc
DO WHILE LASTKEY()<>27
     SELECT 6
     IF opc = 2
          = ooaviso( ;
            'C O P I A N D O . . .' ;
            )
          w_file = f_texto()
          IF opc2 = 1
               REPO FORM agcr4201 TO file;
&w_file  NOCONSOLE
          ELSE
               REPO FORM agcr4201 TO file;
&w_file  NOCONSOLE SUMMARY
          ENDIF
          SET SYSMENU ON
          MODI COMM &w_file NOEDIT WIND;
DETALLE
          SET SYSMENU OFF
          DELE FILE &w_file
          RELEASE WINDOW  ;
                  winmensaje
          CLEAR
     ELSE
          IF SYS(13) = 'OFFLINE'
               swt = f_yesno( ;
                     'Impresora fuera de Linea. Intentar nuevamente ' ;
                     )
               IF swt
                    LOOP
               ELSE
                    DEACTIVATE WINDOW  ;
                               winmensaje
                    EXIT
               ENDIF
          ENDIF
          = ooaviso( ;
            'I M P R I M I E N D O . . .' ;
            )
          IF opc2 = 1
               ??? CHR(15)
               REPORT FORMAT  ;
                      agcr4201 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      agcr4201  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
     ENDIF
     RELEASE WINDOW winmensaje
     EXIT
ENDDO
DO p_mensaje WITH  ;
   ' FIN DE IMPRESION '
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen
DO CASE
     CASE cwhen = 'W_EMIINI'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_var = 'W_EMIINI'
          ON KEY LABEL f6 do busca with;
"EMIS",wrk_var
     CASE cwhen = 'W_EMIFIN'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_var = 'W_EMIFIN'
          ON KEY LABEL f6 do busca with;
"EMIS",wrk_var
     CASE cwhen = 'OPC2'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
ENDCASE
RETURN
*
FUNCTION oovalida
PARAMETER cvalida
DO CASE
     CASE cvalida = 'W_FECINI'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
     CASE cvalida = 'OPC1'
          IF opc1 = 2
               @ 03, 56 GET  ;
                 w_bolini  ;
                 FUNCTION 'L'  ;
                 PICTURE '9999'  ;
                 VALID  .NOT.  ;
                 EMPTY(w_bolini)  ;
                 COLOR SCHEME 8
               @ 03, 66 GET  ;
                 w_bolfin RANGE  ;
                 w_bolini  ;
                 FUNCTION 'L'  ;
                 PICTURE '9999'  ;
                 VALID  .NOT.  ;
                 EMPTY(w_bolfin)  ;
                 COLOR SCHEME 8
               @ 04, 56 GET  ;
                 w_facini  ;
                 FUNCTION 'L'  ;
                 PICTURE '9999'  ;
                 VALID  .NOT.  ;
                 EMPTY(w_facini)  ;
                 COLOR SCHEME 8
               @ 04, 66 GET  ;
                 w_facfin RANGE  ;
                 w_facini  ;
                 FUNCTION 'L'  ;
                 PICTURE '9999'  ;
                 VALID  .NOT.  ;
                 EMPTY(w_facfin)  ;
                 COLOR SCHEME 8
               @ 06, 46 GET  ;
                 w_emiini  ;
                 FUNCTION '@!'  ;
                 VALID  ;
                 oovalid(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())  ;
                 COLOR SCHEME 8
               @ 07, 46 GET  ;
                 w_emifin RANGE  ;
                 w_emiini  ;
                 FUNCTION '@!'  ;
                 VALID  ;
                 oovalid(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())  ;
                 COLOR SCHEME 8
               READ
               IF LASTKEY() = 27
                    RETURN .F.
               ENDIF
          ENDIF
ENDCASE
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'W_EMIINI'  ;
          .OR. cvalid =  ;
          'W_EMIFIN'
          SELECT ge_tab0
          IF cvalid = 'W_EMIINI'
               IF EMPTY(w_emiini)
                    DO p_mensaje  ;
                       WITH  ;
                       'No se aceptan Blancos'
                    RETURN .F.
               ENDIF
               SEEK 'EMIS' +  ;
                    w_emiini
          ELSE
               IF EMPTY(w_emifin)
                    DO p_mensaje  ;
                       WITH  ;
                       'No se aceptan Blancos'
                    RETURN .F.
               ENDIF
               SEEK 'EMIS' +  ;
                    w_emifin
          ENDIF
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Emisor no Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 52 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
ENDCASE
RETURN
*
FUNCTION busca
PARAMETER wrk_busca, wrk_var
des_tab = ALLTRIM(b_tit_tab(wrk_busca))
DEFINE WINDOW bus_tab FROM 08, 23  ;
       TO 18, 63 TITLE des_tab IN  ;
       screen FOOTER  ;
       ' [F6] Busqueda x Descr. '  ;
       COLOR SCHEME 8
DEFINE WINDOW b_tabla FROM 12, 31  ;
       TO 14, 54 TITLE ' Tablas '  ;
       IN screen
SELECT ge_tab0
ON KEY
SET FILTER TO tab_codpre = wrk_busca
GOTO TOP
ACTIVATE WINDOW bus_tab
ON KEY LABEL enter do carga_codt
ON KEY LABEL f6 do busca_codt
BROWSE FIELDS tab_codtab :R :H =  ;
       'C¢digo', tab_destab :R :H =  ;
       'Descripci¢n ' IN bus_tab  ;
       COLOR SCHEME 7
ON KEY
RELEASE WINDOW bus_tab
SET FILTER TO
RETURN wrk_var
*
PROCEDURE busca_codt
ON KEY
ACTIVATE WINDOW b_tabla
c_tab = SPACE(25)
@ 00, 00 GET c_tab PICTURE '@!'
READ
SET ORDER TO descri
SET NEAR ON
SEEK c_tab
SET NEAR OFF
SET ORDER TO codigo
DEACTIVATE WINDOW b_tabla
ACTIVATE WINDOW bus_tab
ON KEY LABEL f6 do busca_codt
ON KEY LABEL enter do carga_codt
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

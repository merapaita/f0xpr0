*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CENTURY ON
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW produ FROM 12, 02  ;
       TO 20, 76 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 21, 77 COLOR SCHEME  ;
       7
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED gc_hpv00 ORDER codigo
SELECT 3
USE SHARED gc_dpv00 ORDER codigo
SELECT 4
USE SHARED gc_hco00 ORDER codigo
SELECT 5
USE SHARED gc_dco00 ORDER codigo
SELECT 6
USE SHARED gc_hve00 ORDER codigo
SELECT 7
USE SHARED gc_dve00 ORDER codigo
SELECT 8
USE SHARED gc_cli00 ORDER codigo
SELECT 9
USE SHARED gc_par00
rge_codlis = par_codlis
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'CONTROL DE PEDIDO DE VENTAS',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100000000000000000001', 2
w_progra = PROGRAM()
STORE DATE() TO w_fec1, w_fec2
STORE SPACE(4) TO w_var, w_busca,  ;
      w_campo, w_selec, w_codalm
DIMENSION cliente( 100, 2)
STORE 0 TO w_facigv, w_swf,  ;
      w_tipcav
SELECT 20
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
SEEK DTOS(DATE()) + '1' +  ;
     rge_monbas + 'DOL '
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        '**No Existe Tipo de Cambio para esta Fecha**'
     w_swf = 1
ELSE
     w_tipcav = cmv_tipcav
ENDIF
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     w_facigv = ROUND(tab_factor /  ;
                100, 2)
ELSE
     DO p_mensaje WITH  ;
        '** No Esta Definido el '+ ;
        sys_codimp
     w_swf = 1
ENDIF
STORE SPACE(01) TO w_est1, w_est2
STORE SPACE(11) TO w_cli1, w_cli2
STORE SPACE(14) TO w_cod1, w_cod2
STORE SPACE(10) TO w_ped1, w_ped2,  ;
      w_nom1, w_nom2, w_dopc1,  ;
      w_dopc2, w_dopc3
DO WHILE w_swf=0
     STORE SPACE(10) TO dopc1,  ;
           w_dopc2, w_dopc3
     STORE 1 TO opc1, opc2, opc3,  ;
           opc4, opc5, w_nropag
     ACTIVATE WINDOW cabecera
     SET CURSOR ON
     @ 02, 19 CLEAR TO 07, 49
     @ 14, 50 CLEAR TO 14, 75
     STORE 0 TO xx
     @ 00, 05 SAY  ;
       'Seleccionados por:' COLOR  ;
       SCHEME 8
     @ 00, 50 SAY  ;
       'Ordenados por   :' COLOR  ;
       SCHEME 8
     @ 09, 05 SAY  ;
       'Clasificar por   :' COLOR  ;
       SCHEME 8
     @ 09, 28 SAY  ;
       'Tipo de Informe :' COLOR  ;
       SCHEME 8
     @ 09, 50 SAY  ;
       'Tipo de Salida  :' COLOR  ;
       SCHEME 8
     @ 02, 07 GET opc1 SIZE 1, 7,  ;
       0 PICTURE  ;
       '@*RVN Cliente;Fecha  ;Pedido '  ;
       VALID oovalida(VARREAD())  ;
       COLOR SCHEME 12
     @ 02, 52 GET opc2 SIZE 1, 7,  ;
       0 PICTURE  ;
       '@*RVN Estado ;Cliente;Fecha  '  ;
       VALID oovalida(VARREAD())  ;
       COLOR SCHEME 12
     @ 11, 07 GET opc3 SIZE 1, 7,  ;
       0 PICTURE  ;
       '@*RVN Pendientes;Atendidos ;Ambos     '  ;
       VALID oovalida(VARREAD())  ;
       COLOR SCHEME 12
     @ 11, 30 GET opc4 SIZE 1, 7,  ;
       0 PICTURE  ;
       '@*RVN Detalle;Resumen'  ;
       WHEN oowhen(VARREAD())  ;
       COLOR SCHEME 12
     @ 11, 52 GET opc5 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVTN Pantalla ;Impresora'  ;
       COLOR SCHEME 12
     READ CYCLE
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF opc5 = 2
          @ 14, 50 CLEAR TO 14,  ;
            75
          @ 14, 52 SAY 'Copias :'
          @ 14, 63 GET w_nropag  ;
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
     DO CASE
          CASE opc1 = 1
               IF w_cli1 =  ;
                  SPACE(11) .OR.  ;
                  w_cli2 =  ;
                  SPACE(11)
                    DO p_mensaje  ;
                       WITH  ;
                       ' Falta Ingresar Clientes '
                    LOOP
               ENDIF
               w_dopc1 = 'SELECCIONADOS POR CLIENTES DEL : ' +  ;
                         SUBSTR(w_nom1,  ;
                         1, 20) +  ;
                         ' AL : ' +  ;
                         SUBSTR(w_nom2,  ;
                         1, 20)
          CASE opc1 = 2
               IF w_fec1 =  ;
                  CTOD(SPACE(8))  ;
                  .OR. w_fec2 =  ;
                  CTOD(SPACE(8))
                    DO p_mensaje  ;
                       WITH  ;
                       ' Falta Ingresar Fechas '
                    LOOP
               ENDIF
               w_dopc1 = 'SELECCIONADOS POR FECHAS DEL : ' +  ;
                         DTOC(w_fec1) +  ;
                         ' AL : ' +  ;
                         DTOC(w_fec2)
          CASE opc1 = 3
               IF w_ped1 =  ;
                  SPACE(10) .OR.  ;
                  w_ped2 =  ;
                  SPACE(10)
                    DO p_mensaje  ;
                       WITH  ;
                       ' Falta Ingresar Nro. de Pedidos '
                    LOOP
               ENDIF
               w_dopc1 = 'SELECCIONADOS POR PEDIDOS DEL : ' +  ;
                         w_ped1 +  ;
                         ' AL : ' +  ;
                         w_ped2
     ENDCASE
     = ooaviso( ;
       ' T R A B A J A N D O . . .' ;
       )
     CREATE CURSOR ventas (tipdoc  ;
            C (4), nrodoc C (10),  ;
            fecdoc D, fecfin D,  ;
            indest C (1), pagcta  ;
            N (9, 2), saldo N (9,  ;
            2), codpro C (14),  ;
            descri C (40), cantid  ;
            N (9, 2), indes2 C  ;
            (1), fecfi2 D, total  ;
            N (12, 2), tiate N (4,  ;
            2), candes N (9, 0),  ;
            pedido C (10), fecped  ;
            D, feclle D, codent C  ;
            (9), desent C (30),  ;
            telefo N (8), modelo  ;
            C (20), porcta N (5,  ;
            2))
     SELECT 20
     USE SHARED gc_ord00 ORDER  ;
         ord_codino
     SELECT 21
     USE SHARED gc_pro00 ORDER  ;
         codigo
     DO CASE
          CASE opc1 = 1
               DO llenamat
               FOR x = 1 TO xx
                    SELECT gc_hpv00
                    SET ORDER TO hpv_codent
                    SEEK cliente(x, ;
                         1)
                    IF FOUND()
                         SCAN WHILE  ;
                              hpv_codent =  ;
                              cliente(x, ;
                              1)  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              SELECT  ;
                               gc_dpv00
                              SEEK  ;
                               gc_hpv00.hpv_tipdoc +  ;
                               gc_hpv00.hpv_nrodoc
                              IF FOUND()
                                   SCAN WHILE gc_hpv00.hpv_tipdoc = dpv_tipdoc .AND. gc_hpv00.hpv_nrodoc = dpv_nrodoc .AND.  .NOT. EOF()
                                        SELECT ventas
                                        APPEND BLANK
                                        REPLACE tipdoc WITH gc_hpv00.hpv_tipdoc
                                        REPLACE nrodoc WITH gc_hpv00.hpv_nrodoc
                                        REPLACE fecdoc WITH gc_hpv00.hpv_fecdoc
                                        REPLACE codent WITH gc_hpv00.hpv_codent
                                        REPLACE fecfin WITH gc_hpv00.hpv_fecfin
                                        REPLACE indest WITH gc_hpv00.hpv_indest
                                        REPLACE pagcta WITH gc_hpv00.hpv_pagcta
                                        REPLACE porcta WITH gc_hpv00.hpv_porcta
                                        REPLACE codpro WITH gc_dpv00.dpv_codpro
                                        REPLACE cantid WITH gc_dpv00.dpv_cantid
                                        REPLACE candes WITH gc_dpv00.dpv_candes
                                        REPLACE indes2 WITH gc_dpv00.dpv_indest
                                        REPLACE fecfi2 WITH gc_dpv00.dpv_fecfin
                                        REPLACE tiate WITH gc_dpv00.dpv_fecfin - gc_dpv00.dpv_fecdoc
                                        SELECT gc_pro00
                                        SEEK gc_dpv00.dpv_codpro
                                        IF FOUND()
                                             SELECT ventas
                                             REPLACE descri WITH gc_pro00.pro_descri
                                             REPLACE modelo WITH gc_pro00.pro_modelo
                                        ENDIF
                                        IF ventas.tipdoc = 'BOLE'
                                             w_nro = 'B'
                                        ELSE
                                             w_nro = 'F'
                                        ENDIF
                                        SELECT gc_ord00
                                        SET ORDER TO ord_codino
                                        SEEK rge_codalm + w_nro + ventas.nrodoc + ventas.codpro
                                        IF FOUND()
                                             SELECT ventas
                                             REPLACE pedido WITH gc_ord00.ord_nrodoc
                                             REPLACE feclle WITH gc_ord00.ord_feclle
                                        ELSE
                                             SET ORDER TO ord_codipr
                                             SEEK rge_codalm + w_nro + ventas.nrodoc + ventas.codpro
                                             IF FOUND()
                                                  SELECT ventas
                                                  REPLACE pedido WITH gc_ord00.ord_nrodoc
                                                  REPLACE feclle WITH gc_ord00.ord_feclle
                                             ENDIF
                                        ENDIF
                                        IF ventas.pedido <> SPACE(10)
                                             SELECT gc_hco00
                                             SEEK gc_ord00.ord_nrodoc
                                             IF FOUND()
                                                  SELECT ventas
                                                  REPLACE fecped WITH gc_hco00.hco_fecdoc
                                             ENDIF
                                        ENDIF
                                        SELECT gc_cli00
                                        SEEK 'C' + ventas.codent
                                        IF FOUND()
                                             SELECT ventas
                                             REPLACE desent WITH gc_cli00.cli_razsoc
                                             REPLACE telefo WITH gc_cli00.cli_telefo
                                        ENDIF
                                        SELECT gc_dpv00
                                   ENDSCAN
                              ENDIF
                              SELECT  ;
                               gc_hpv00
                         ENDSCAN
                    ENDIF
               ENDFOR
          CASE opc1 = 2
               SELECT gc_hpv00
               SET ORDER TO hpv_fecdoc
               SET NEAR ON
               SEEK DTOS(w_fec1)
               SET NEAR OFF
               SCAN WHILE  ;
                    hpv_fecdoc >=  ;
                    w_fec1 .AND.  ;
                    hpv_fecdoc <=  ;
                    w_fec2 .AND.   ;
                    .NOT. EOF()
                    SELECT gc_dpv00
                    SEEK gc_hpv00.hpv_tipdoc +  ;
                         gc_hpv00.hpv_nrodoc
                    IF FOUND()
                         SCAN WHILE  ;
                              gc_hpv00.hpv_tipdoc =  ;
                              dpv_tipdoc  ;
                              .AND.  ;
                              gc_hpv00.hpv_nrodoc =  ;
                              dpv_nrodoc  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              SELECT  ;
                               ventas
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               tipdoc  ;
                               WITH  ;
                               gc_hpv00.hpv_tipdoc
                              REPLACE  ;
                               nrodoc  ;
                               WITH  ;
                               gc_hpv00.hpv_nrodoc
                              REPLACE  ;
                               fecdoc  ;
                               WITH  ;
                               gc_hpv00.hpv_fecdoc
                              REPLACE  ;
                               codent  ;
                               WITH  ;
                               gc_hpv00.hpv_codent
                              REPLACE  ;
                               fecfin  ;
                               WITH  ;
                               gc_hpv00.hpv_fecfin
                              REPLACE  ;
                               indest  ;
                               WITH  ;
                               gc_hpv00.hpv_indest
                              REPLACE  ;
                               pagcta  ;
                               WITH  ;
                               gc_hpv00.hpv_pagcta
                              REPLACE  ;
                               porcta  ;
                               WITH  ;
                               gc_hpv00.hpv_porcta
                              REPLACE  ;
                               codpro  ;
                               WITH  ;
                               gc_dpv00.dpv_codpro
                              REPLACE  ;
                               cantid  ;
                               WITH  ;
                               gc_dpv00.dpv_cantid
                              REPLACE  ;
                               candes  ;
                               WITH  ;
                               gc_dpv00.dpv_candes
                              REPLACE  ;
                               indes2  ;
                               WITH  ;
                               gc_dpv00.dpv_indest
                              REPLACE  ;
                               fecfi2  ;
                               WITH  ;
                               gc_dpv00.dpv_fecfin
                              REPLACE  ;
                               tiate  ;
                               WITH  ;
                               gc_dpv00.dpv_fecfin -  ;
                               gc_dpv00.dpv_fecdoc
                              SELECT  ;
                               gc_pro00
                              SEEK  ;
                               gc_dpv00.dpv_codpro
                              IF FOUND()
                                   SELECT ventas
                                   REPLACE descri WITH gc_pro00.pro_descri
                                   REPLACE modelo WITH gc_pro00.pro_modelo
                              ENDIF
                              IF ventas.tipdoc =  ;
                                 'BOLE'
                                   w_nro = 'B'
                              ELSE
                                   w_nro = 'F'
                              ENDIF
                              SELECT  ;
                               gc_ord00
                              SET ORDER;
TO ord_codino
                              SEEK  ;
                               rge_codalm +  ;
                               w_nro +  ;
                               ventas.nrodoc +  ;
                               ventas.codpro
                              IF FOUND()
                                   SELECT ventas
                                   REPLACE pedido WITH gc_ord00.ord_nrodoc
                                   REPLACE feclle WITH gc_ord00.ord_feclle
                              ELSE
                                   SET ORDER TO ord_codipr
                                   SEEK rge_codalm + w_nro + ventas.nrodoc + ventas.codpro
                                   IF FOUND()
                                        SELECT ventas
                                        REPLACE pedido WITH gc_ord00.ord_nrodoc
                                        REPLACE feclle WITH gc_ord00.ord_feclle
                                   ENDIF
                              ENDIF
                              IF ventas.pedido <>  ;
                                 SPACE(10)
                                   SELECT gc_hco00
                                   SEEK gc_ord00.ord_nrodoc
                                   IF FOUND()
                                        SELECT ventas
                                        REPLACE fecped WITH gc_hco00.hco_fecdoc
                                   ENDIF
                              ENDIF
                              SELECT  ;
                               gc_cli00
                              SEEK  ;
                               'C' +  ;
                               ventas.codent
                              IF FOUND()
                                   SELECT ventas
                                   REPLACE desent WITH gc_cli00.cli_razsoc
                                   REPLACE telefo WITH gc_cli00.cli_telefo
                              ENDIF
                              SELECT  ;
                               gc_dpv00
                         ENDSCAN
                    ENDIF
                    SELECT gc_hpv00
               ENDSCAN
          CASE opc1 = 3
               SELECT gc_hpv00
               SET ORDER TO codigo
               SELECT gc_ord00
               SET ORDER TO ord_nrodoc
               SELECT gc_hco00
               SET ORDER TO codigo
               SET NEAR ON
               SEEK w_ped1
               SET NEAR OFF
               SCAN WHILE  ;
                    hco_nrodoc >=  ;
                    w_ped1 .AND.  ;
                    hco_nrodoc <=  ;
                    w_ped2 .AND.   ;
                    .NOT. EOF()
                    SELECT gc_ord00
                    SEEK gc_hco00.hco_nrodoc
                    IF FOUND()
                         SCAN WHILE  ;
                              ord_nrodoc =  ;
                              gc_hco00.hco_nrodoc  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              IF ord_inorig =  ;
                                 'F'  ;
                                 .OR.  ;
                                 ord_inorig =  ;
                                 'B'  ;
                                 .AND.  ;
                                 ord_codalm =  ;
                                 rge_codalm
                                   IF ord_inorig = 'F'
                                        w_tipo = 'FACT'
                                   ELSE
                                        w_tipo = 'BOLE'
                                   ENDIF
                                   SELECT gc_dpv00
                                   IF gc_ord00.ord_codpro = SPACE(14)
                                        SEEK w_tipo + gc_ord00.ord_docref + gc_ord00.ord_codprp
                                   ELSE
                                        SEEK w_tipo + gc_ord00.ord_docref + gc_ord00.ord_codpro
                                   ENDIF
                                   IF FOUND()
                                        SELECT gc_hpv00
                                        SEEK w_tipo + gc_ord00.ord_docref
                                        IF FOUND()
                                             SELECT ventas
                                             APPEND BLANK
                                             REPLACE tipdoc WITH gc_hpv00.hpv_tipdoc
                                             REPLACE nrodoc WITH gc_hpv00.hpv_nrodoc
                                             REPLACE fecdoc WITH gc_hpv00.hpv_fecdoc
                                             REPLACE codent WITH gc_hpv00.hpv_codent
                                             REPLACE fecfin WITH gc_hpv00.hpv_fecfin
                                             REPLACE indest WITH gc_hpv00.hpv_indest
                                             REPLACE pagcta WITH gc_hpv00.hpv_pagcta
                                             REPLACE porcta WITH gc_hpv00.hpv_porcta
                                             REPLACE codpro WITH gc_dpv00.dpv_codpro
                                             REPLACE cantid WITH gc_dpv00.dpv_cantid
                                             REPLACE candes WITH gc_dpv00.dpv_candes
                                             REPLACE indes2 WITH gc_dpv00.dpv_indest
                                             REPLACE fecfi2 WITH gc_dpv00.dpv_fecfin
                                             REPLACE pedido WITH gc_ord00.ord_nrodoc
                                             REPLACE fecped WITH gc_hco00.hco_fecdoc
                                             REPLACE feclle WITH gc_ord00.ord_feclle
                                             REPLACE tiate WITH gc_dpv00.dpv_fecfin - gc_dpv00.dpv_fecdoc
                                             SELECT gc_pro00
                                             SEEK gc_dpv00.dpv_codpro
                                             IF FOUND()
                                                  SELECT ventas
                                                  REPLACE descri WITH gc_pro00.pro_descri
                                                  REPLACE modelo WITH gc_pro00.pro_modelo
                                             ENDIF
                                             SELECT gc_cli00
                                             SEEK 'C' + ventas.codent
                                             IF FOUND()
                                                  SELECT ventas
                                                  REPLACE desent WITH gc_cli00.cli_razsoc
                                                  REPLACE telefo WITH gc_cli00.cli_telefo
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                              SELECT  ;
                               gc_ord00
                         ENDSCAN
                    ENDIF
                    SELECT gc_hco00
               ENDSCAN
               SELECT gc_ord00
               SET ORDER TO ord_codino
     ENDCASE
     SELECT 20
     USE SHARED gc_dlp00 ORDER  ;
         codigo
     SELECT ventas
     w_idx = f_indice()
     index on tipdoc+nrodoc to &w_idx
     GOTO TOP
     w_tipdoc = tipdoc
     w_nrodoc = nrodoc
     w_total = 0
     w_recno = RECNO()
     DO WHILE  .NOT. EOF()
          IF tipdoc <> w_tipdoc  ;
             .OR. nrodoc <>  ;
             w_nrodoc
               w_recno = RECNO()
               IF w_tipdoc =  ;
                  'FACT'
                    REPLACE total  ;
                            WITH  ;
                            ROUND(ROUND(w_total *  ;
                            (1 +  ;
                            w_facigv),  ;
                            2) *  ;
                            w_tipcav,  ;
                            2)  ;
                            FOR  ;
                            tipdoc =  ;
                            w_tipdoc  ;
                            .AND.  ;
                            nrodoc =  ;
                            w_nrodoc
               ELSE
                    REPLACE total  ;
                            WITH  ;
                            ROUND(w_total *  ;
                            w_tipcav,  ;
                            2)  ;
                            FOR  ;
                            tipdoc =  ;
                            w_tipdoc  ;
                            .AND.  ;
                            nrodoc =  ;
                            w_nrodoc
               ENDIF
               REPLACE saldo WITH  ;
                       total -  ;
                       pagcta FOR  ;
                       tipdoc =  ;
                       w_tipdoc  ;
                       .AND.  ;
                       nrodoc =  ;
                       w_nrodoc
               w_total = 0
               GOTO w_recno
               w_tipdoc = tipdoc
               w_nrodoc = nrodoc
          ELSE
               SELECT gc_dlp00
               SEEK rge_codlis +  ;
                    ventas.codpro
               IF FOUND()
                    IF w_tipdoc =  ;
                       'FACT'
                         w_total =  ;
                          w_total +  ;
                          ROUND(dlp_prsigv *  ;
                          ventas.cantid,  ;
                          2)
                    ELSE
                         w_total =  ;
                          w_total +  ;
                          ROUND(dlp_prcigv *  ;
                          ventas.cantid,  ;
                          2)
                    ENDIF
               ENDIF
               SELECT ventas
               SKIP
          ENDIF
     ENDDO
     IF w_tipdoc = 'FACT'
          REPLACE total WITH  ;
                  ROUND(ROUND(w_total *  ;
                  (1 + w_facigv),  ;
                  2) * w_tipcav,  ;
                  2) FOR tipdoc =  ;
                  w_tipdoc .AND.  ;
                  nrodoc =  ;
                  w_nrodoc
     ELSE
          REPLACE total WITH  ;
                  ROUND(w_total *  ;
                  w_tipcav, 2)  ;
                  FOR tipdoc =  ;
                  w_tipdoc .AND.  ;
                  nrodoc =  ;
                  w_nrodoc
     ENDIF
     REPLACE saldo WITH total -  ;
             pagcta FOR tipdoc =  ;
             w_tipdoc .AND.  ;
             nrodoc = w_nrodoc
     GOTO TOP
     DO CASE
          CASE opc2 = 1
               w_dopc2 = 'ORDENADOS POR: ESTADO '
               index on indest+dtos(fecdoc)+tipdoc+nrodoc;
to  &w_idx
          CASE opc2 = 2
               w_dopc2 = 'ORDENADOS POR: CLIENTE'
               index on desent+dtos(fecdoc)+tipdoc+nrodoc;
to &w_idx
          CASE opc2 = 3
               w_dopc2 = ' ORDENADOS POR: FECHA '
               index on dtos(fecdoc)+tipdoc+nrodoc;
to &w_idx
     ENDCASE
     COUNT TO nreg
     DEACTIVATE WINDOW winmensaje
     IF nreg = 0
          DO p_mensaje WITH  ;
             ' NO SE ENCONTRO INFORMACION '
          LOOP
     ENDIF
     DO CASE
          CASE opc3 = 1
               w_dopc3 = 'CLASIFICADOS POR: PENDIENTES'
               w_cond = 'indest<>"A" and indest<>"F"'
          CASE opc3 = 2
               w_dopc3 = 'CLASIFICADOS POR: ATENDIDOS '
               w_cond = 'indest<>"A" and indest="F"'
          CASE opc3 = 3
               w_dopc3 = '   CLASIFICADOS POR: AMBOS  '
               w_cond = 'indest<>"A"'
     ENDCASE
     = ooimprime(opc5)
ENDDO
RELEASE WINDOW cabecera
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
FUNCTION t_aten
PARAMETER w_tim
STORE 0 TO w_hor, w_gen
IF w_tim > 0
     w_hor = w_tim - INT(w_tim)
     w_hor = ROUND((w_hor * 24) /  ;
             100, 2)
     w_gen = INT(w_tim) + w_hor
     w_desgen = STR(w_gen, 4) +  ;
                ' DIAS Y ' +  ;
                STR(VAL(RIGHT(STR(w_gen,  ;
                6, 2), 2)), 2) +  ;
                ' HORAS'
ELSE
     w_desgen = ' 0 DIAS Y 0 HORAS '
ENDIF
RETURN w_desgen
*
PROCEDURE ooimprime
PARAMETER opc
DO WHILE LASTKEY()<>27
     SELECT ventas
     IF opc = 1
          = ooaviso( ;
            'C O P I A N D O . . .' ;
            )
          w_file = f_texto()
          IF opc4 = 1
               repo form agcr4208 for;
&w_cond to file &w_file NOCONSOLE
          ELSE
               repo form agcr4208 for;
&w_cond to file &w_file NOCONSOLE SUMMARY
          ENDIF
          SET SYSMENU ON
          KEYBOARD '{ctrl+f10}'
          modi comm &w_file noedit wind;
pantall 
          SET SYSMENU OFF
          dele file &w_file
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
          IF opc4 = 1
               ??? CHR(15)
               repo form agcr4208 for;
&w_cond to printer noconsole
          ELSE
               repo form agcr4208 for;
&w_cond to printer NOCONSOLE SUMMARY
          ENDIF
          DO p_mensaje WITH  ;
             ' FIN DE IMPRESION '
     ENDIF
     RELEASE WINDOW winmensaje
     EXIT
ENDDO
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen
DO CASE
     CASE cwhen = 'W_EST1' .OR.  ;
          cwhen = 'W_EST2'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          w_var = cwhen
          ON KEY LABEL f6 do busca with;
"IMPO",w_var
     CASE cwhen = 'W_CLI1' .OR.  ;
          cwhen = 'W_CLI2'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          w_busca = 'C'
          w_var = cwhen
          ON KEY LABEL f6 do entidad with;
w_busca,w_var            
     CASE cwhen = 'W_COD1' .OR.  ;
          cwhen = 'W_COD2'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          w_selec = SELECT()
          w_campo = cwhen
          w_codalm = rge_codalm
          ON KEY LABEL f6 do produc2 with;
w_campo,w_selec,w_codalm
     CASE cwhen = 'W_PED1' .OR.  ;
          cwhen = 'W_PED2'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          w_var = cwhen
          ON KEY LABEL f6 do pedido with;
w_var
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
          @ 2, 19 CLEAR TO 7, 49
          DO CASE
               CASE opc1 = 1
                    @ 02, 19 SAY  ;
                      'Del:'
                    @ 04, 19 SAY  ;
                      'Al :'
                    @ 02, 24 GET  ;
                      w_cli1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      oovalid(VARREAD())  ;
                      WHEN  ;
                      oowhen(VARREAD())  ;
                      COLOR  ;
                      SCHEME 8
                    @ 04, 24 GET  ;
                      w_cli2  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      oovalid(VARREAD())  ;
                      WHEN  ;
                      oowhen(VARREAD())  ;
                      COLOR  ;
                      SCHEME 8
                    READ
                    IF LASTKEY() =  ;
                       27
                         STORE SPACE(1)  ;
                               TO  ;
                               w_est1,  ;
                               w_est2
                         @ 2, 19  ;
                           CLEAR  ;
                           TO 7,  ;
                           49
                         RETURN .F.
                    ENDIF
               CASE opc1 = 2
                    STORE DATE()  ;
                          TO  ;
                          w_fec1,  ;
                          w_fec2
                    @ 03, 19 SAY  ;
                      'Del:            Al:'
                    @ 03, 24 GET  ;
                      w_fec1  ;
                      FUNCTION  ;
                      'L' PICTURE  ;
                      '99/99/9999'  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(w_fec1)  ;
                      COLOR  ;
                      SCHEME 8
                    @ 03, 38 GET  ;
                      w_fec2  ;
                      RANGE  ;
                      w_fec1  ;
                      FUNCTION  ;
                      'L' PICTURE  ;
                      '99/99/9999'  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(w_fec2)  ;
                      COLOR  ;
                      SCHEME 8
                    READ
                    IF LASTKEY() =  ;
                       27
                         STORE CTOD(SPACE(8))  ;
                               TO  ;
                               w_fec1,  ;
                               w_fec2
                         @ 2, 19  ;
                           CLEAR  ;
                           TO 7,  ;
                           49
                         RETURN .F.
                    ENDIF
               CASE opc1 = 3
                    @ 04, 19 SAY  ;
                      'Del:            Al:'
                    @ 04, 24 GET  ;
                      w_ped1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      oovalid(VARREAD())  ;
                      WHEN  ;
                      oowhen(VARREAD())  ;
                      COLOR  ;
                      SCHEME 8
                    @ 04, 38 GET  ;
                      w_ped2  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      oovalid(VARREAD())  ;
                      WHEN  ;
                      oowhen(VARREAD())  ;
                      COLOR  ;
                      SCHEME 8
                    READ
                    IF LASTKEY() =  ;
                       27
                         STORE SPACE(10)  ;
                               TO  ;
                               w_ped1,  ;
                               w_ped2
                         @ 2, 19  ;
                           CLEAR  ;
                           TO 7,  ;
                           49
                         RETURN .F.
                    ENDIF
          ENDCASE
ENDCASE
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'W_CLI1' .OR.  ;
          cvalid = 'W_CLI2'
          if empty(&cvalid)
               RETURN .F.
          ENDIF
          SELECT gc_cli00
          SET ORDER TO codigo
          IF cvalid = 'W_CLI1'
               SEEK 'C' + w_cli1
               w_nom1 = cli_razsoc
          ELSE
               SEEK 'C' + w_cli2
               w_nom2 = cli_razsoc
          ENDIF
          IF FOUND()
               IF cvalid =  ;
                  'W_CLI2' .AND.  ;
                  ALLTRIM(w_nom2) <  ;
                  ALLTRIM(w_nom1)
                    DO p_mensaje  ;
                       WITH  ;
                       ' No Puede ser Menor que el Inicio'
                    RETURN .F.
               ELSE
                    @ ROW() + 1,  ;
                      24 SAY  ;
                      SUBSTR(cli_razsoc,  ;
                      1, 25)
               ENDIF
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo No Existe '
               RETURN .F.
          ENDIF
     CASE cvalid = 'W_PED1' .OR.  ;
          cvalid = 'W_PED2'
          if empty(&cvalid)
               RETURN .F.
          ENDIF
          IF cvalid = 'W_PED2'  ;
             .AND. w_ped2 <  ;
             w_ped1
               DO p_mensaje WITH  ;
                  ' No Puede ser Menor que el Inicio'
               RETURN .F.
          ENDIF
          SELECT gc_hco00
          SET ORDER TO codigo
          IF cvalid = 'W_PED1'
               SEEK w_ped1
               c = 24
          ELSE
               SEEK w_ped2
               c = 38
          ENDIF
          IF FOUND()
               @ ROW(), c SAY  ;
                 SUBSTR(hco_nrodoc,  ;
                 1, 10)
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo No Existe '
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
PROCEDURE oobuscar
PARAMETER opcion
ON KEY
SELECT gc_hco00
SET RELATION TO 'IMPO' + hco_indest INTO;
ge_tab0 ADDITIVE
SET FILTER TO hco_nrodoc <> hco_numfac
campo = 'transform(hco_nrodoc, "@ 9999999999") + "³" + dtoc(hco_fecdoc) + "³" + substr(ge_tab0.tab_destab,1,10) + "³" + transform(hco_totgen,"999,999.99")'
titulo = 'AYUDA DE DOCUMENTOS'
DO ayuda1 WITH campo, titulo,  ;
   'HCO_NRODOC'
SELECT gc_hco00
SET FILTER TO
SET RELATION TO
ON KEY LABEL F6 do oobuscar
RETURN
*
PROCEDURE llenamat
SELECT gc_cli00
SET ORDER TO nombre
SET NEAR ON
SEEK ALLTRIM(w_nom1)
SET NEAR OFF
x = 1
SCAN WHILE ALLTRIM(cli_razsoc) >=  ;
     ALLTRIM(w_nom1) .AND.  ;
     ALLTRIM(cli_razsoc) <=  ;
     ALLTRIM(w_nom2) .AND.  .NOT.  ;
     EOF()
     IF cli_tpper = 'C'
          DIMENSION cliente( x,  ;
                    2)
          cliente( x, 1) =  ;
                 cli_codigo
          cliente( x, 2) =  ;
                 cli_razsoc
          x = x + 1
          IF x = 101
          ENDIF
     ENDIF
ENDSCAN
SET ORDER TO codigo
xx = x - 1
RETURN
*
PROCEDURE ayuda1
PARAMETER campo, mensaje, xclave
ACTIVATE WINDOW busqueda
define popup ayu0 from 0,0 to 11,50 promp;
field &campo title mensaje color scheme;
8
ON SELECTION POPUP ayu0 do choice0
ACTIVATE POPUP ayu0 NOWAIT
ACTIVATE POPUP ayu0
DEACTIVATE POPUP ayu0
DEACTIVATE WINDOW busqueda
RETURN
*
PROCEDURE choice0
IF LASTKEY() == 13
     keyboard &xclave
     DEACTIVATE POPUP ayu0
ENDIF
RETURN
*
FUNCTION pedido
PARAMETER wrk_var
ON KEY
DEFINE WINDOW pedido FROM 08, 12  ;
       TO 18, 66 TITLE  ;
       ' BUSQUEDA DE PEDIDOS ' IN  ;
       screen FOOTER  ;
       ' [F6] B£squeda R pida '  ;
       COLOR SCHEME 10
= ooopen('gc_HCO00',1)
DEFINE WINDOW b_ped FROM 12, 30  ;
       TO 14, 42 TITLE ' Pedido '  ;
       IN screen
SET RELATION TO 'IMPO' + hco_indest INTO;
ge_tab0 ADDITIVE
SET FILTER TO hco_nrodoc <> hco_numfac
GOTO TOP
ACTIVATE WINDOW pedido
ON KEY LABEL enter do carga_cod
ON KEY LABEL f6 do busca_cod
BROWSE FIELDS hco_nrodoc :R :H =  ;
       'NRO.DOCUMENTO',  ;
       hco_fecdoc :R :H =  ;
       'FEC.DOC.',  ;
       ge_tab0.tab_destab : 10 :R  ;
       :H = 'ESTADO ', hco_totgen  ;
       :P = '9,999,999.99' :R :H =  ;
       'TOTAL GENERAL ' +  ;
       sys_simomb IN pedido
ON KEY
SET FILTER TO
SET RELATION TO
RELEASE WINDOW pedido
RETURN wrk_var
*
FUNCTION carga_cod
ON KEY
&wrk_var = hco_nrodoc
RELEASE WINDOW pedido
SET ORDER TO 1
KEYBOARD CHR(13)
RETURN wrk_var
*
PROCEDURE busca_cod
ON KEY
ACTIVATE WINDOW b_ped
c_ped = SPACE(10)
@ 0, 0 GET c_ped PICTURE '@!'
READ
SET NEAR ON
SEEK c_ped
SET NEAR OFF
DEACTIVATE WINDOW b_ped
ACTIVATE WINDOW pedido
ON KEY LABEL f6 do busca_cod
ON KEY LABEL enter do carga_cod
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

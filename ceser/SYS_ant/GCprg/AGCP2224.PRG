*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
DEFINE WINDOW t0 FROM 02, 64 TO  ;
       02, 73 IN screen NONE
DEFINE WINDOW a0 FROM 03, 03 TO  ;
       11, 76 IN screen
DEFINE WINDOW titposs FROM 10, 00  ;
       TO 12, 79 IN screen NONE  ;
       COLOR SCHEME 8
DEFINE WINDOW pidposs FROM 19, 00  ;
       TO 21, 79 IN screen NONE  ;
       COLOR SCHEME 20
ACTIVATE WINDOW tablas
flag = .F.
DO p_prestab WITH 'PROCESO',  ;
   ' TRASPASO ENTRE BODEGAS ',  ;
   'SELECCION', flag
@ 02, 63 SAY PROGRAM()
SELECT 1
USE SHARED GC_HIP00 ALIAS  ;
    gc_hip00 ORDER CODIGO
SELECT 2
USE SHARED GC_DIP00 ALIAS  ;
    gc_dip00 ORDER CODIGO
SELECT 3
USE SHARED GE_TAB0 ALIAS ge_tab0  ;
    ORDER CODIGO
SELECT 4
USE SHARED GC_UNI00 ALIAS  ;
    gc_uni00 ORDER CODIGO
SELECT 6
USE SHARED GC_KAR00 ALIAS  ;
    gc_kar00 ORDER CODIGO
SELECT 7
USE SHARED GC_CLI00 ALIAS  ;
    gc_cli00 ORDER CODIGO
SELECT 8
USE SHARED GC_VND00 ALIAS  ;
    gc_vnd00 ORDER CODIGO
SELECT 9
USE SHARED GC_ALM00 ALIAS  ;
    gc_alm00 ORDER CODIGO
SELECT 10
USE SHARED GC_CMV00 ALIAS  ;
    gc_cmv00 ORDER cmv_feinmo
SELECT 11
USE SHARED GC_PRO00 ALIAS  ;
    gc_pro00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 12
USE SHARED GC_DLP00 ORDER CODIGO
STORE 0 TO wrk_termin, wrk_facigv,  ;
      w_tipcam
= facigv()
IF wrk_facigv = 0
     DO p_mensaje WITH  ;
        'No hay impuesto a las ventas'
     wrk_termin = 1
ENDIF
w_tipcam = ootc2(DATE(),'SOL ', ;
           'DOL ','2')
IF w_tipcam = -1 .OR. w_tipcam =  ;
   0
     DO p_mensaje WITH  ;
        'No hay tipo de cambio'
     wrk_termin = 1
ENDIF
DO WHILE wrk_termin=0
     STORE 0 TO tmp_1, wrk_creo,  ;
           wrk_sw2, wrk_sw3,  ;
           wrk_totcan, tmp_totcan,  ;
           tmp_totpre, dif_totcan,  ;
           wrk_cambix, xx_pre,  ;
           wrk_pos, wrk_x,  ;
           wrk_mone1, wrk_cambio,  ;
           wrk_prec, wrk_almx,  ;
           wrk_alm, wrk_alma,  ;
           wrk_lin, wrk_count,  ;
           wrk_bus2, wrk_bus1,  ;
           fac_mon, fac_uni
     STORE 1 TO wrk_sw4,  ;
           wrk_cambiy, wrk_swfin
     STORE SPACE(01) TO wrk_descr
     STORE SPACE(10) TO  ;
           wrk_docref
     DIMENSION cod( 1), can( 1),  ;
               doc( 1), des( 1),  ;
               facuni( 1),  ;
               facmon[ 1], alm1(  ;
               1), alm2( 1),  ;
               valor( 1),  ;
               factor2( 1), alma(  ;
               1), pre[ 1]
     SCATTER BLANK TO cod
     STORE SPACE(30) TO alm1( 1)
     STORE 0 TO can[ 1], valor[  ;
           1], factor2[ 1], pre[  ;
           1]
     STORE 0 TO wrk_flag,  ;
           tran_ant, wrk_mal
     STORE SPACE(10) TO doc[ 1]
     ACTIVATE WINDOW t0
     @ 00, 00 SAY 'SELECCIONA'  ;
       COLOR N+/W 
     SET CURSOR ON
     SELECT gc_hip00
     DIMENSION a1( FCOUNT())
     SCATTER BLANK TO a1
     DO proceso
ENDDO
DEACTIVATE WINDOW tablas, titpass
RELEASE WINDOW titposs, pidposs
RELEASE WINDOW t0, a0, a1, c0, a2,  ;
        a3
CLOSE DATABASES
ON KEY
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE proceso
DO pinta1
tmp_1 = 0
ON KEY
IF wrk_sw2 = 0
     DO WHILE .T. .AND. tmp_1=0
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          a1( 01) = 'GUIA'
          wrk_busca = 'DOCU'
          wrk_var = 'A1(01)'
          ON KEY LABEL F6 DO BUSCA WITH;
WRK_BUSCA,WRK_VAR
          @ 00, 13 GET a1( 01)  ;
            PICTURE '@!'
          READ
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               LOOP
          ENDIF
          IF LASTKEY() = 27
               tmp_1 = 1
               EXIT
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' + a1(01)
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'Tipo de Documento no Existe'
               LOOP
          ELSE
               @ 00, 18 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 12)
          ENDIF
          DO WHILE .T. .AND.  ;
             tmp_1=0
               ON KEY
               DO p_footer WITH  ;
                  '100000000000000000001',  ;
                  2
               ACTIVATE WINDOW a0
               @ 00, 46 GET a1(  ;
                 02) PICTURE  ;
                 '@!'
               READ
               IF LASTKEY() = 5  ;
                  .OR. LASTKEY() =  ;
                  19
                    EXIT
               ENDIF
               IF LASTKEY() = 27
                    tmp_1 = 1
                    EXIT
               ENDIF
               a1( 02) =  ;
                 f_ceros(a1(02), ;
                 10,2)
               @ 00, 46 SAY  ;
                 a1(02)
               IF a1(02) =  ;
                  '0000000000'
                    DO p_mensaje  ;
                       WITH  ;
                       'Documento en Blanco'
                    LOOP
               ENDIF
               SELECT gc_hip00
               SEEK a1(01) +  ;
                    a1(02)
               IF FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'Documento ya fue Ingresado'
                    LOOP
               ENDIF
               tmp_1 = 1
          ENDDO
     ENDDO
     IF LASTKEY() = 27
          wrk_termin = 1
     ENDIF
ENDIF
DO WHILE LASTKEY()<>27 .AND.  ;
   wrk_sw4<>0
     tmp_1 = 0
     ON KEY
     wrk_swmot = 1
     DO WHILE .T. .AND. tmp_1=0
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          IF wrk_sw3 = 0
               a1( 03) = DATE()
          ENDIF
          @ 01, 13 GET a1( 03)  ;
            PICTURE '99/99/99'
          READ
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               LOOP
          ENDIF
          IF LASTKEY() = 27
               tmp_1 = 1
               EXIT
          ENDIF
          DO WHILE .T. .AND.  ;
             tmp_1=0
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ACTIVATE WINDOW a0
               a1( 05) = 'TRBO'
               wrk_busca = 'TRAS'
               wrk_var = 'A1(05)'
               ON KEY LABEL F6 DO BUSCA;
WITH WRK_BUSCA,WRK_VAR
               @ 02, 13 GET a1(  ;
                 05) PICTURE  ;
                 '@!'
               READ
               IF LASTKEY() = 5  ;
                  .OR. LASTKEY() =  ;
                  19
                    EXIT
               ENDIF
               IF LASTKEY() = 27
                    tmp_1 = 1
                    EXIT
               ENDIF
               SELECT ge_tab0
               SEEK wrk_busca +  ;
                    a1(05)
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'Tipo de Movimiento no Existe'
                    LOOP
               ELSE
                    @ 02, 18 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 13)
               ENDIF
               DO WHILE .T. .AND.  ;
                  tmp_1=0
                    a1( 30) =  ;
                      '006 '
                    wrk_busca = 'MTRA'
                    wrk_var = 'A1(30)'
                    ON KEY LABEL F6 DO;
BUSCA WITH WRK_BUSCA,WRK_VAR
                    @ 02, 46 GET  ;
                      a1( 30)  ;
                      PICTURE  ;
                      '@!'
                    READ
                    IF LASTKEY() =  ;
                       5 .OR.  ;
                       LASTKEY() =  ;
                       19
                         EXIT
                    ENDIF
                    IF LASTKEY() =  ;
                       27
                         tmp_1 = 1
                         EXIT
                    ENDIF
                    SELECT ge_tab0
                    SEEK wrk_busca +  ;
                         a1(30)
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'Tipo de Movimiento no Existe'
                         LOOP
                    ELSE
                         @ 02, 52  ;
                           SAY  ;
                           SUBSTR(tab_destab,  ;
                           1,  ;
                           17)
                    ENDIF
                    DO WHILE .T.  ;
                       .AND.  ;
                       tmp_1=0
                         ON KEY
                         IF wrk_sw2 =  ;
                            0  ;
                            .OR.  ;
                            wrk_x =  ;
                            0
                              a1(  ;
                                07) =  ;
                                rge_codalm
                              wrk_busca =  ;
                               'ALMA'
                              wrk_var =  ;
                               'A1(7)'
                              ON KEY LABEL;
F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                              @ 03,  ;
                                13  ;
                                GET  ;
                                a1(  ;
                                7)  ;
                                PICTURE  ;
                                '@!'
                              READ
                              IF LASTKEY() =  ;
                                 5  ;
                                 .OR.  ;
                                 LASTKEY() =  ;
                                 19
                                   EXIT
                              ENDIF
                              IF LASTKEY() =  ;
                                 27
                                   tmp_1 = 1
                                   EXIT
                              ENDIF
                              SELECT  ;
                               ge_tab0
                              SEEK  ;
                               'ALMA' +  ;
                               a1(7)
                              IF   ;
                               .NOT.  ;
                               FOUND()
                                   DO p_mensaje WITH 'Almac‚n no Existe'
                                   LOOP
                              ELSE
                                   @ 03, 18 SAY SUBSTR(tab_destab, 1, 13)
                              ENDIF
                         ENDIF
                         DO WHILE  ;
                            .T.  ;
                            .AND.  ;
                            tmp_1= ;
                            0
                              ON KEY
                              wrk_busca =  ;
                               'ALMA'
                              wrk_var =  ;
                               'A1(6)'
                              ON KEY LABEL;
F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                              @ 03,  ;
                                46  ;
                                GET  ;
                                a1(  ;
                                6)  ;
                                PICTURE  ;
                                '@!'
                              READ
                              IF LASTKEY() =  ;
                                 5  ;
                                 .OR.  ;
                                 LASTKEY() =  ;
                                 19
                                   EXIT
                              ENDIF
                              IF LASTKEY() =  ;
                                 27
                                   tmp_1 = 1
                                   EXIT
                              ENDIF
                              IF a1(6) =  ;
                                 a1(7)
                                   DO p_mensaje WITH 'Almac‚n Destino Igual a Almac‚n Despacho.. [ERROR]'
                                   LOOP
                              ENDIF
                              SELECT  ;
                               ge_tab0
                              SEEK  ;
                               'ALMA' +  ;
                               a1(6)
                              IF   ;
                               .NOT.  ;
                               FOUND()
                                   DO p_mensaje WITH 'Almac‚n no Existe'
                                   LOOP
                              ELSE
                                   @ 03, 52 SAY SUBSTR(tab_destab, 1, 17)
                              ENDIF
                              DO WHILE  ;
                                 .T.  ;
                                 .AND.  ;
                                 tmp_1= ;
                                 0
                                   ON KEY
                                   a1( 09) = 'A   '
                                   wrk_busca = 'ENTI'
                                   wrk_var = 'A1(09)'
                                   ON KEY LABEL F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                                   @ 04, 13 GET a1( 09) PICTURE '@!'
                                   READ
                                   IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                        EXIT
                                   ENDIF
                                   IF LASTKEY() = 27
                                        tmp_1 = 1
                                        EXIT
                                   ENDIF
                                   SELECT ge_tab0
                                   SEEK 'ENTI' + a1(09)
                                   IF  .NOT. FOUND()
                                        DO p_mensaje WITH 'No Existe'
                                        LOOP
                                   ELSE
                                        @ 04, 18 SAY SUBSTR(tab_destab, 1, 13)
                                   ENDIF
                                   DO WHILE .T. .AND. tmp_1=0
                                        ON KEY
                                        wrk_busca = a1(09)
                                        IF a1(09) = 'P  ' .OR. a1(09) = 'C   ' .OR. a1(09) = 'I   '
                                             a1( 10) = SUBSTR(a1(10), 1, 9)
                                        ELSE
                                             a1( 10) = SUBSTR(a1(10), 1, 5)
                                             @ 04, 52 SAY SPACE(4)
                                        ENDIF
                                        wrk_var = 'A1(10)'
                                        ON KEY LABEL F6 do entidad with wrk_busca,wrk_var
                                        @ 04, 46 GET a1( 10) PICTURE '@!'
                                        READ
                                        IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                             EXIT
                                        ENDIF
                                        IF LASTKEY() = 27
                                             tmp_1 = 1
                                             EXIT
                                        ENDIF
                                        IF a1(09) = 'P   ' .OR. a1(09) = 'C   ' .OR. a1(09) = 'I   '
                                             SELECT gc_cli00
                                             SEEK SUBSTR(a1(09), 1, 1) + SUBSTR(a1(10), 1, 9)
                                        ELSE
                                             SELECT gc_vnd00
                                             SEEK SUBSTR(a1(09), 1, 1) + SUBSTR(a1(10), 1, 5)
                                        ENDIF
                                        IF  .NOT. FOUND()
                                             IF a1(09) = 'P   ' .OR. a1(09) = 'C   ' .OR. a1(09) = 'I   '
                                                  DO p_mensaje WITH 'Proveedor o Cliente no Existe'
                                                  LOOP
                                             ELSE
                                                  DO p_mensaje WITH 'Administrativo o Vendedor no Existe'
                                                  LOOP
                                             ENDIF
                                        ELSE
                                             IF a1(09) = 'P   ' .OR. a1(09) = 'C   ' .OR. a1(09) = 'I   '
                                                  @ 04, 57 SAY SUBSTR(cli_razsoc, 1, 13)
                                             ELSE
                                                  @ 04, 57 SAY SUBSTR(vnd_nombre, 1, 13)
                                             ENDIF
                                        ENDIF
                                        DO WHILE .T. .AND. tmp_1=0
                                             a1( 31) = 'T   '
                                             wrk_busca = 'ENTI'
                                             wrk_var = 'A1(31)'
                                             ON KEY LABEL F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                                             @ 05, 13 GET a1( 31) PICTURE '@!'
                                             READ
                                             IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                  EXIT
                                             ENDIF
                                             IF LASTKEY() = 27
                                                  tmp_1 = 1
                                                  EXIT
                                             ENDIF
                                             IF  .NOT. EMPTY(a1(31))
                                                  SELECT ge_tab0
                                                  SEEK 'ENTI' + a1(31)
                                                  IF  .NOT. FOUND()
                                                       DO p_mensaje WITH 'No existe...'
                                                       LOOP
                                                  ELSE
                                                       @ 05, 18 SAY SUBSTR(tab_destab, 1, 13)
                                                  ENDIF
                                             ENDIF
                                             DO WHILE .T. .AND. tmp_1=0
                                                  wrk_busca = a1(31)
                                                  IF a1(31) = 'P   ' .OR. a1(31) = 'C   ' .OR. a1(31) = 'I   '
                                                       a1( 29) = SUBSTR(a1(29), 1, 9)
                                                  ELSE
                                                       a1( 29) = SUBSTR(a1(29), 1, 5)
                                                       @ 05, 52 SAY SPACE(4)
                                                  ENDIF
                                                  wrk_var = 'A1(29)'
                                                  ON KEY LABEL F6 DO ENTIDAD WITH WRK_BUSCA,WRK_VAR
                                                  @ 05, 46 GET a1( 29) PICTURE '@!'
                                                  READ
                                                  IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                       EXIT
                                                  ENDIF
                                                  IF LASTKEY() = 27
                                                       tmp_1 = 1
                                                       EXIT
                                                  ENDIF
                                                  IF a1(31) = 'P   ' .OR. a1(31) = 'C   ' .OR. a1(31) = 'I   '
                                                       SELECT gc_cli00
                                                       SEEK SUBSTR(a1(31), 1, 1) + SUBSTR(a1(29), 1, 9)
                                                  ELSE
                                                       SELECT gc_vnd00
                                                       SEEK SUBSTR(a1(31), 1, 1) + SUBSTR(a1(29), 1, 5)
                                                  ENDIF
                                                  IF  .NOT. FOUND()
                                                       IF a1(31) = 'P' .OR. a1(31) = 'C' .OR. a1(31) = 'I'
                                                            DO p_mensaje WITH 'Proveedor o Cliente no Existe'
                                                            LOOP
                                                       ELSE
                                                            DO p_mensaje WITH 'Administrativo o Vendedor no Existe'
                                                            LOOP
                                                       ENDIF
                                                  ELSE
                                                       IF a1(31) = 'P' .OR. a1(31) = 'C' .OR. a1(31) = 'I'
                                                            @ 05, 57 SAY SUBSTR(cli_razsoc, 1, 13)
                                                       ELSE
                                                            @ 05, 57 SAY SUBSTR(vnd_nombre, 1, 13)
                                                       ENDIF
                                                  ENDIF
                                                  DO WHILE .T. .AND. tmp_1=0
                                                       wrk_busca = 'DOCU'
                                                       wrk_var = 'A1(13)'
                                                       ON KEY LABEL F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                                                       ACTIVATE WINDOW a0
                                                       @ 06, 17 GET a1( 13) PICTURE '@!'
                                                       READ
                                                       IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                            EXIT
                                                       ENDIF
                                                       IF LASTKEY() = 27
                                                            tmp_1 = 1
                                                            EXIT
                                                       ENDIF
                                                       IF  .NOT. EMPTY(a1(13))
                                                            SELECT ge_tab0
                                                            SEEK 'DOCU' + a1(13)
                                                            IF  .NOT. FOUND()
                                                                 DO p_mensaje WITH 'Tipo de Documento no Existe'
                                                                 LOOP
                                                            ELSE
                                                                 @ 06, 22 SAY SUBSTR(tab_destab, 1, 12)
                                                            ENDIF
                                                       ENDIF
                                                       DO WHILE .T. .AND. tmp_1=0
                                                            DO p_footer WITH '100000000000000000001', 2
                                                            ON KEY
                                                            ACTIVATE WINDOW a0
                                                            @ 06, 49 GET a1( 14) PICTURE '@!'
                                                            READ
                                                            IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                                 EXIT
                                                            ENDIF
                                                            IF LASTKEY() = 27
                                                                 tmp_1 = 1
                                                                 EXIT
                                                            ENDIF
                                                            SELECT gc_hip00
                                                            tmp_1 = 1
                                                       ENDDO
                                                  ENDDO
                                             ENDDO
                                        ENDDO
                                   ENDDO
                              ENDDO
                         ENDDO
                    ENDDO
               ENDDO
          ENDDO
     ENDDO
     CLEAR GETS
     IF LASTKEY() <> 27
          wrk_sw2 = 0
          DO detalle
     ENDIF
ENDDO
RETURN
*
PROCEDURE detalle
ON KEY
DEFINE POPUP detalle FROM 13, 00  ;
       TO 18, 79 PROMPT FIELDS  ;
       codigo + '³' +  ;
       SUBSTR(descrip, 1, 18) +  ;
       '³' + codig2 + '³' +  ;
       docref + '³' +  ;
       TRANSFORM(cantidad,  ;
       '999,999') + '³' +  ;
       IIF(codigo <> codig2 .AND.   ;
       .NOT. EMPTY(codig2),  ;
       TRANSFORM(coprmo2,  ;
       '999,999.99'),  ;
       TRANSFORM(importe,  ;
       '999,999.99')) IN screen  ;
       COLOR SCHEME 9
ON SELECTION POPUP detalle DEAC POPUP;
DETALLE
ACTIVATE WINDOW titposs
@ 00, 00 SAY  ;
  'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿'
@ 01, 00 SAY  ;
  '³   CODIGO     ³    DESCRIPCION   ³   A CODIGO   ³ OBSERVA. ³CANTID.³PRECIO UNI³'
@ 02, 00 SAY  ;
  'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ'
DO WHILE LASTKEY()<>27 .AND.  ;
   wrk_sw4<>0
     IF wrk_sw3 = 1
          DIMENSION mat(  ;
                    RECCOUNT(),  ;
                    FCOUNT())
          DO carmat
     ENDIF
     wrk_sel1 = SELECT()
     DO carque
     DO WHILE LASTKEY()<>27 .AND.  ;
        LASTKEY()<>-1 .AND.  ;
        wrk_sw4<>0 .AND. wrk_sw2<> ;
        1
          DO WHILE wrk_swfin=1
               wrk_swfin = 0
               SELECT trasp
               ACTIVATE POPUP  ;
                        detalle
          ENDDO
          wrk_swfin = 1
     ENDDO
     SELECT gc_alm00
     IF LASTKEY() = 27
          SELECT trasp
          COUNT TO wrk_total
          GOTO TOP
          IF wrk_total > 0
               SCAN WHILE  .NOT.  ;
                    EOF()
                    SELECT gc_alm00
                    SEEK trasp.codigo +  ;
                         a1(7)
                    IF FOUND()
                         DO rbloquea
                         REPLACE alm_stkres  ;
                                 WITH  ;
                                 alm_stkres -  ;
                                 trasp.cantidad
                         REPLACE alm_usuari  ;
                                 WITH  ;
                                 clave
                         REPLACE alm_fecha  ;
                                 WITH  ;
                                 DATE()
                         REPLACE alm_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                    ENDIF
                    SELECT trasp
               ENDSCAN
          ENDIF
     ENDIF
     IF LASTKEY() <> -4
          IF wrk_sw3 = 1 .AND.  ;
             LASTKEY() = 27
               ON KEY
          ENDIF
     ELSE
          DO proceso
     ENDIF
ENDDO
DEACTIVATE WINDOW tot
tmp_1 = 0
ON KEY
DO p_footer WITH  ;
   '100010000000000001', 2
RETURN
*
FUNCTION p_lee
PARAMETER aux_opcion
SET CURSOR ON
ON KEY
swt = .T.
STORE 0 TO tmp_fin, tmp_1
wrk_reg = RECNO()
DO WHILE swt .AND. (LASTKEY()=13  ;
   .OR. LASTKEY()=-2)
     ACTIVATE WINDOW pidposs
     @ 00, 00 SAY  ;
       'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿'
     @ 01, 00 SAY  ;
       '³              ³                  ³              ³          ³       ³          ³'
     @ 02, 00 SAY  ;
       'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ'
     DIMENSION a2( FCOUNT())
     IF aux_opcion = 1
          SCATTER BLANK TO a2
     ELSE
          SCATTER TO a2
     ENDIF
     IF aux_opcion = 2
          @ 01, 01 SAY a2(1)
          @ 01, 16 SAY  ;
            SUBSTR(a2(3), 1, 18)
          IF a2(1) <> a2(7)
               @ 01, 35 SAY a2(7)
          ENDIF
          @ 01, 50 SAY a2(6)
          @ 01, 61 SAY a2(4)  ;
            PICTURE '999,999'
          IF a2(1) <> a2(7) .AND.   ;
             .NOT. EMPTY(a2(7))
               @ 01, 69 SAY a2(8)  ;
                 PICTURE  ;
                 '999,999.99'
          ELSE
               @ 01, 69 SAY a2(5)  ;
                 PICTURE  ;
                 '999,999.99'
          ENDIF
     ENDIF
     tmp_err = 1
     DO WHILE .T. .AND. tmp_1=0
          IF aux_opcion = 1
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        pidposs
               wrk_campo = 'A2(1)'
               wrk_selec = SELECT()
               ON KEY LABEL F6 DO PRODUC;
WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO 
               @ 01, 01 GET a2(  ;
                 1) PICTURE '@!'
               READ
               IF LASTKEY() = 27
                    tmp_1 = 1
                    EXIT
               ENDIF
               IF LASTKEY() = 19  ;
                  .OR. LASTKEY() =  ;
                  5
                    EXIT
               ENDIF
               SELECT gc_pro00
               SET ORDER TO CODIGO
               SEEK a2(1)
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'PRODUCTO NO EXISTE'
                    LOOP
               ELSE
                    IF pro_estope =  ;
                       'B'
                         DO p_mensaje  ;
                            WITH  ;
                            'PRODUCTO ESTA BLOQUEADO'
                         LOOP
                    ENDIF
                    fac_uni = 1
                    a2( 2) =  ;
                      pro_unimed
                    a2( 3) =  ;
                      pro_descri
                    a2( 5) =  ;
                      ooprecio(a2(1), ;
                      2)
                    @ 01, 16 SAY  ;
                      SUBSTR(a2(3),  ;
                      1, 18)
               ENDIF
          ENDIF
          DO WHILE .T. .AND.  ;
             tmp_1=0
               IF aux_opcion = 1
                    wrk_var = 'A2(2)'
                    wrk_busca = a2(1)
                    IF LASTKEY() =  ;
                       27
                         tmp_1 = 1
                         EXIT
                    ENDIF
                    IF LASTKEY() =  ;
                       19 .OR.  ;
                       LASTKEY() =  ;
                       5
                         EXIT
                    ENDIF
                    wrk_bus2 = ASCAN(cod,  ;
                               a2(1) +  ;
                               a2(2))
                    IF wrk_bus2 <>  ;
                       0
                         DO p_mensaje  ;
                            WITH  ;
                            'EL PRODUCTO YA EXISTE'
                         EXIT
                         LOOP
                    ENDIF
               ENDIF
               SELECT gc_uni00
               SEEK a2(1) + a2(2)
               IF  .NOT. FOUND()
                    SELECT gc_pro00
                    SEEK a2(1)
                    IF FOUND()
                         IF a2(2) <>  ;
                            pro_unimed
                              DO p_mensaje  ;
                                 WITH  ;
                                 'UNIDAD NO EXISTE EN EL PRODUCTO NI COMO ALTERNATIVO'
                              LOOP
                         ELSE
                              uni_facto1 =  ;
                               1
                              uni_facto2 =  ;
                               1
                         ENDIF
                    ENDIF
               ENDIF
               = uniinv(uni_facto1, ;
                 uni_facto2)
               DO WHILE .T. .AND.  ;
                  tmp_1=0
                    wrk_campo = 'A2(7)'
                    wrk_selec = SELECT()
                    ON KEY LABEL F6 DO;
PRODUC WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO;

                    @ 01, 35 GET  ;
                      a2( 7)  ;
                      PICTURE  ;
                      '@!'
                    READ
                    IF LASTKEY() =  ;
                       27
                         tmp_1 = 1
                         EXIT
                    ENDIF
                    IF LASTKEY() =  ;
                       19 .OR.  ;
                       LASTKEY() =  ;
                       5
                         EXIT
                    ENDIF
                    SELECT gc_pro00
                    SET ORDER TO CODIGO
                    SEEK a2(7)
                    IF  .NOT.  ;
                        FOUND()  ;
                        .AND.   ;
                        .NOT.  ;
                        EMPTY(a2(7))
                         DO p_mensaje  ;
                            WITH  ;
                            'PRODUCTO NO EXISTE'
                         LOOP
                    ELSE
                         IF pro_estope =  ;
                            'B'
                              DO p_mensaje  ;
                                 WITH  ;
                                 'PRODUCTO ESTA BLOQUEADO'
                              LOOP
                         ENDIF
                         a2( 8) =  ;
                           pro_coprmo
                    ENDIF
                    DO WHILE .T.  ;
                       .AND.  ;
                       tmp_1=0
                         ON KEY
                         DO p_footer  ;
                            WITH  ;
                            '100000000000000000001',  ;
                            2
                         ACTIVATE  ;
                          WINDOW  ;
                          pidposs
                         @ 01, 50  ;
                           GET  ;
                           a2( 6)  ;
                           PICTURE  ;
                           '@!'
                         READ
                         IF LASTKEY() =  ;
                            27
                              tmp_1 =  ;
                               1
                              EXIT
                         ENDIF
                         IF LASTKEY() =  ;
                            19  ;
                            .OR.  ;
                            LASTKEY() =  ;
                            5
                              EXIT
                         ENDIF
                         DO WHILE  ;
                            .T.  ;
                            .AND.  ;
                            tmp_1= ;
                            0
                              wrk_cla1 =  ;
                               a2(1) +  ;
                               a1(7)
                              IF aux_opcion =  ;
                                 1
                                   wrk_bus2 = ASCAN(alm1, a2(1) + a2(2))
                                   IF wrk_bus2 = 0
                                        wrk_bus1 = wrk_bus1 + 1
                                        DIMENSION alm1[ wrk_bus1]
                                        DIMENSION alm2[ wrk_bus1]
                                        alm1[ wrk_bus1] = a2(1) + a2(2)
                                        alm2[ wrk_bus1] = 0
                                   ENDIF
                              ELSE
                                   wrk_bus2 = ASCAN(alm1, a2(1) + a2(2))
                                   alm2[ wrk_bus2] = alm2(wrk_bus2) - a2(4) * fac_uni
                              ENDIF
                              @ 01,  ;
                                61  ;
                                GET  ;
                                a2(  ;
                                4)  ;
                                DEFAULT  ;
                                0  ;
                                PICTURE  ;
                                '999,999'  ;
                                VALID  ;
                                vali2c()  ;
                                WHEN  ;
                                when2c()
                              READ
                              IF a2(1) <>  ;
                                 a2(7)  ;
                                 .AND.   ;
                                 .NOT.  ;
                                 EMPTY(a2(7))
                                   @ 01, 69 SAY a2(8) PICTURE '999,999.99'
                              ELSE
                                   @ 01, 69 SAY a2(5) PICTURE '999,999.99'
                              ENDIF
                              IF LASTKEY() =  ;
                                 27  ;
                                 .AND.  ;
                                 aux_opcion =  ;
                                 2
                                   alm2[ wrk_bus1] = alm2(wrk_bus1) + a2(4) * fac_uni
                                   tmp_1 = 1
                                   EXIT
                              ENDIF
                              IF aux_opcion =  ;
                                 1
                                   alm2[ wrk_bus1] = a2(4) * fac_uni
                              ELSE
                                   alm2[ wrk_bus2] = a2(4) * fac_uni
                              ENDIF
                              IF a2(1) <>  ;
                                 a2(7)  ;
                                 .AND.   ;
                                 .NOT.  ;
                                 EMPTY(a2(7))
                                   w_prec = a2(8)
                                   @ 01, 69 GET a2( 8) PICTURE '999,999.99'
                                   READ
                                   IF a2(8) < 0
                                        DO p_mensaje WITH 'IMPORTE NEGATIVO.'
                                        RETURN .F.
                                   ENDIF
                                   IF w_prec <> a2(8)
                                        = INKEY(0)
                                   ENDIF
                              ENDIF
                              tmp_1 =  ;
                               1
                         ENDDO
                    ENDDO
               ENDDO
          ENDDO
     ENDDO
     IF LASTKEY() <> 27 .AND.  ;
        LASTKEY() = 13
          wrk_pos = ASCAN(cod,  ;
                    a2(1) +  ;
                    a2(2))
          IF wrk_pos = 0
               wrk_x = wrk_x + 1
               DIMENSION cod[  ;
                         wrk_x]
               DIMENSION can[  ;
                         wrk_x]
               DIMENSION des[  ;
                         wrk_x]
               DIMENSION pre[  ;
                         wrk_x]
               DIMENSION doc[  ;
                         wrk_x]
               DIMENSION facuni[  ;
                         wrk_x]
               DIMENSION facmon[  ;
                         wrk_x]
               DIMENSION valor[  ;
                         wrk_x]
               DIMENSION factor2[  ;
                         wrk_x]
               = AINS(cod, wrk_x)
               = AINS(can, wrk_x)
               = AINS(des, wrk_x)
               = AINS(pre, wrk_x)
               = AINS(doc, wrk_x)
               = AINS(facuni,  ;
                 wrk_x)
               = AINS(facmon,  ;
                 wrk_x)
               = AINS(valor,  ;
                 wrk_x)
               = AINS(factor2,  ;
                 wrk_x)
               cod[ wrk_x] =  ;
                  SPACE(18)
               can[ wrk_x] = 0
               des[ wrk_x] =  ;
                  SPACE(20)
               pre[ wrk_x] = 0
               doc[ wrk_x] =  ;
                  SPACE(10)
               facuni[ wrk_x] = 0
               facmon[ wrk_x] = 0
               valor[ wrk_x] = 0
               factor2[ wrk_x] =  ;
                      0
               wrk_pos = wrk_x
          ENDIF
          cod[ wrk_pos] = a2(1) +  ;
             a2(2)
          can[ wrk_pos] = a2(4) *  ;
             fac_uni
          des[ wrk_pos] = a2(3)
          pre[ wrk_pos] = a2(5)
          doc[ wrk_pos] = a2(6)
          facuni[ wrk_pos] =  ;
                fac_uni
          facmon[ wrk_pos] =  ;
                fac_mon
          factor2[ wrk_pos] =  ;
                 fac_uni
          valor[ wrk_pos] =  ;
               (((xx_pre *  ;
               wrk_cambix) /  ;
               wrk_cambio) *  ;
               fac_uni)
          SELECT trasp
          IF aux_opcion = 1
               APPEND BLANK
               REPLACE codigo  ;
                       WITH  ;
                       a2(1)
               REPLACE unidad  ;
                       WITH  ;
                       a2(2)
               REPLACE descrip  ;
                       WITH  ;
                       a2(3)
          ENDIF
          DO rbloquea
          REPLACE cantidad WITH  ;
                  a2(4)
          REPLACE importe WITH  ;
                  a2(5)
          REPLACE docref WITH  ;
                  a2(6)
          REPLACE codig2 WITH  ;
                  a2(7)
          REPLACE coprmo2 WITH  ;
                  a2(8)
          UNLOCK
          DEACTIVATE WINDOW  ;
                     pidpass,  ;
                     pidposs
     ENDIF
     EXIT
ENDDO
DEACTIVATE WINDOW pidpass,  ;
           pidposs
SELECT trasp
COUNT TO wrk_count
reg_count = wrk_count
DO cartec
RETURN
*
PROCEDURE p_borra
ON KEY
aux_swt = .T.
wrk_cla1 = codigo + a1(7)
fac_uni = oofactor(codigo,unidad)
SELECT gc_alm00
SEEK trasp.codigo + a1(7)
IF FOUND()
     DO rbloquea
     REPLACE alm_stkres WITH  ;
             (alm_stkres -  ;
             trasp.cantidad)
     REPLACE alm_usuari WITH  ;
             clave
     REPLACE alm_fecha WITH  ;
             DATE()
     REPLACE alm_hora WITH TIME()
     UNLOCK
ENDIF
SELECT trasp
wrk_pos = ASCAN(cod, trasp.codigo +  ;
          trasp.unidad)
DELETE
COUNT TO wrk_count
IF wrk_pos <> 0
     = ADEL(cod, wrk_pos)
     = ADEL(can, wrk_pos)
     = ADEL(des, wrk_pos)
     = ADEL(pre, wrk_pos)
     = ADEL(doc, wrk_pos)
     = ADEL(facuni, wrk_pos)
     = ADEL(facmon, wrk_pos)
     = ADEL(valor, wrk_pos)
     = ADEL(factor2, wrk_pos)
     wrk_x = wrk_x - 1
ENDIF
DO cartec
RETURN
*
PROCEDURE crea
ON KEY
STORE 0 TO wrk_sw4, wrk_totcan,  ;
      tmp_totcan, tmp_totpre,  ;
      dif_totcan, wrk_total
DEFINE WINDOW tot FROM 17, 15 TO  ;
       21, 63 IN screen
ACTIVATE WINDOW tot
SELECT trasp
GOTO TOP
SCAN WHILE  .NOT. EOF()
     tmp_totcan = tmp_totcan +  ;
                  cantidad
     IF codigo <> codig2 .AND.   ;
        .NOT. EMPTY(codig2)
          tmp_totpre = tmp_totpre +  ;
                       (cantidad *  ;
                       coprmo2)
     ELSE
          tmp_totpre = tmp_totpre +  ;
                       (cantidad *  ;
                       importe)
     ENDIF
ENDSCAN
wrk_totcan = tmp_totcan
@ 00, 22 SAY 'DIFERENCIA'
@ 01, 01 SAY 'TOTAL CANTIDAD :'
@ 02, 01 SAY 'TOTAL IMPORTE  :'
@ 01, 22 GET wrk_totcan PICTURE  ;
  '99,999,999'
@ 02, 19 SAY tmp_totpre PICTURE  ;
  '99,999,999.99'
READ
IF LASTKEY() <> 27
     dif_totcan = tmp_totcan -  ;
                  wrk_totcan
     IF dif_totcan <> 0
          wrk_sw4 = 1
     ENDIF
     @ 01, 33 SAY dif_totcan  ;
       PICTURE '999,999.99'
     IF wrk_sw4 = 0
          ON KEY LABEL F7 DO IMPRIME
          DO WHILE LASTKEY()<>27  ;
             .AND. LASTKEY()<>-1
               DO p_footer WITH  ;
                  '111101000000101000101',  ;
                  2
               = INKEY(0, 'M')
               IF LASTKEY() = 27
                    wrk_swfin = 1
                    wrk_sw4 = 1
                    DEACTIVATE WINDOW  ;
                               tot
                    DO p_footer  ;
                       WITH  ;
                       '111101000000101000101',  ;
                       2
                    wrk_flag = 1
                    DO carque
                    DO cartec
                    wrk_flag = 0
               ELSE
                    IF LASTKEY() = - ;
                       6
                         = imprime()
                         wrk_swfin =  ;
                          0
                         EXIT
                    ELSE
                         wrk_swfin =  ;
                          0
                    ENDIF
               ENDIF
          ENDDO
          IF wrk_swfin = 0
               SELECT gc_alm00
               SET FILTER TO
               SELECT trasp
               GOTO TOP
               SCAN WHILE  .NOT.  ;
                    EOF()
                    SCATTER MEMVAR
                    wrk_lin = wrk_lin +  ;
                              1
                    SELECT gc_dip00
                    APPEND BLANK
                    DO rbloquea
                    REPLACE dip_tipdoc  ;
                            WITH  ;
                            a1(1)
                    REPLACE dip_nrodoc  ;
                            WITH  ;
                            a1(2)
                    REPLACE dip_propar  ;
                            WITH  ;
                            m.codigo
                    REPLACE dip_unimed  ;
                            WITH  ;
                            m.unidad
                    REPLACE dip_cantid  ;
                            WITH  ;
                            m.cantidad
                    REPLACE dip_docref  ;
                            WITH  ;
                            m.docref
                    REPLACE dip_pordes  ;
                            WITH  ;
                            0
                    REPLACE dip_import  ;
                            WITH  ;
                            m.importe
                    REPLACE dip_total  ;
                            WITH  ;
                            ROUND(dip_import *  ;
                            dip_cantid,  ;
                            2)
                    REPLACE dip_usuari  ;
                            WITH  ;
                            clave
                    REPLACE dip_fecha  ;
                            WITH  ;
                            DATE()
                    REPLACE dip_hora  ;
                            WITH  ;
                            TIME()
                    UNLOCK
                    IF (m.codigo <>  ;
                       m.codig2)  ;
                       .AND.   ;
                       .NOT.  ;
                       EMPTY(m.codig2)
                         APPEND BLANK
                         DO rbloquea
                         REPLACE dip_tipdoc  ;
                                 WITH  ;
                                 a1(1)
                         REPLACE dip_nrodoc  ;
                                 WITH  ;
                                 a1(2)
                         REPLACE dip_propar  ;
                                 WITH  ;
                                 m.codig2
                         REPLACE dip_unimed  ;
                                 WITH  ;
                                 m.unidad
                         REPLACE dip_cantid  ;
                                 WITH  ;
                                 m.cantidad
                         REPLACE dip_docref  ;
                                 WITH  ;
                                 m.docref
                         REPLACE dip_pordes  ;
                                 WITH  ;
                                 0
                         REPLACE dip_import  ;
                                 WITH  ;
                                 m.coprmo2
                         REPLACE dip_total  ;
                                 WITH  ;
                                 ROUND(dip_import *  ;
                                 dip_cantid,  ;
                                 2)
                         REPLACE dip_usuari  ;
                                 WITH  ;
                                 clave
                         REPLACE dip_fecha  ;
                                 WITH  ;
                                 DATE()
                         REPLACE dip_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                    ENDIF
                    a1( 11) =  ;
                      'DOL'
                    SELECT gc_kar00
                    APPEND BLANK
                    DO rbloquea
                    REPLACE kar_codpro  ;
                            WITH  ;
                            m.codigo
                    REPLACE kar_fecing  ;
                            WITH  ;
                            DATE()
                    REPLACE kar_horing  ;
                            WITH  ;
                            TIME()
                    REPLACE kar_tipdoc  ;
                            WITH  ;
                            a1(1)
                    REPLACE kar_nrodoc  ;
                            WITH  ;
                            a1(2)
                    REPLACE kar_lindet  ;
                            WITH  ;
                            STR(wrk_lin,  ;
                            2)
                    REPLACE kar_fecdoc  ;
                            WITH  ;
                            a1(3)
                    IF (m.codigo <>  ;
                       m.codig2)  ;
                       .AND.   ;
                       .NOT.  ;
                       EMPTY(m.codig2)
                         REPLACE kar_codmov  ;
                                 WITH  ;
                                 'EVTA'
                    ELSE
                         REPLACE kar_codmov  ;
                                 WITH  ;
                                 a1(5)
                    ENDIF
                    REPLACE kar_unimed  ;
                            WITH  ;
                            m.unidad
                    REPLACE kar_codmon  ;
                            WITH  ;
                            a1(11)
                    REPLACE kar_tipent  ;
                            WITH  ;
                            a1(09)
                    REPLACE kar_codent  ;
                            WITH  ;
                            a1(10)
                    REPLACE kar_tidore  ;
                            WITH  ;
                            a1(13)
                    REPLACE kar_nrdore  ;
                            WITH  ;
                            a1(14)
                    UNLOCK
                    SELECT gc_alm00
                    wrk_cla2 = trasp.codigo +  ;
                               a1(7)
                    SEEK wrk_cla2
                    IF FOUND()
                         wrk_almant =  ;
                          alm_stkfis
                    ELSE
                         wrk_almant =  ;
                          0
                    ENDIF
                    SELECT gc_kar00
                    DO rbloquea
                    REPLACE kar_stkant  ;
                            WITH  ;
                            wrk_almant
                    REPLACE kar_cantid  ;
                            WITH  ;
                            m.cantidad
                    REPLACE kar_import  ;
                            WITH  ;
                            ooprecio(m.codigo, ;
                            1)
                    REPLACE kar_usuari  ;
                            WITH  ;
                            clave
                    REPLACE kar_fecha  ;
                            WITH  ;
                            DATE()
                    REPLACE kar_hora  ;
                            WITH  ;
                            TIME()
                    UNLOCK
                    SELECT gc_pro00
                    SEEK m.codigo
                    IF FOUND()
                         DO rbloquea
                         REPLACE pro_ultmov  ;
                                 WITH  ;
                                 DATE()
                         REPLACE pro_usuari  ;
                                 WITH  ;
                                 clave,  ;
                                 pro_fecha  ;
                                 WITH  ;
                                 DATE(),  ;
                                 pro_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                         wrk_proant =  ;
                          pro_coprmo
                         wrk_proanb =  ;
                          pro_coprmb
                         wrk_cosuni =  ;
                          pro_ulcomb
                    ENDIF
                    SELECT gc_kar00
                    DO rbloquea
                    IF (trasp.codigo <>  ;
                       trasp.codig2)  ;
                       .AND.   ;
                       .NOT.  ;
                       EMPTY(trasp.codig2)
                         REPLACE kar_almdes  ;
                                 WITH  ;
                                 a1(7)
                    ELSE
                         REPLACE kar_almrec  ;
                                 WITH  ;
                                 a1(6)
                         REPLACE kar_almdes  ;
                                 WITH  ;
                                 a1(7)
                    ENDIF
                    REPLACE kar_cosant  ;
                            WITH  ;
                            wrk_proant
                    REPLACE kar_cosanb  ;
                            WITH  ;
                            wrk_proanb
                    REPLACE kar_cosuni  ;
                            WITH  ;
                            wrk_cosuni
                    REPLACE kar_usuari  ;
                            WITH  ;
                            clave
                    REPLACE kar_fecha  ;
                            WITH  ;
                            DATE()
                    REPLACE kar_hora  ;
                            WITH  ;
                            TIME()
                    UNLOCK
                    IF (trasp.codigo <>  ;
                       trasp.codig2)  ;
                       .AND.   ;
                       .NOT.  ;
                       EMPTY(trasp.codig2)
                         SELECT gc_kar00
                         APPEND BLANK
                         DO rbloquea
                         REPLACE kar_codpro  ;
                                 WITH  ;
                                 m.codig2
                         REPLACE kar_fecing  ;
                                 WITH  ;
                                 DATE()
                         REPLACE kar_horing  ;
                                 WITH  ;
                                 TIME()
                         REPLACE kar_tipdoc  ;
                                 WITH  ;
                                 a1(1)
                         REPLACE kar_nrodoc  ;
                                 WITH  ;
                                 a1(2)
                         REPLACE kar_lindet  ;
                                 WITH  ;
                                 STR(wrk_lin,  ;
                                 2)
                         REPLACE kar_fecdoc  ;
                                 WITH  ;
                                 a1(3)
                         REPLACE kar_codmov  ;
                                 WITH  ;
                                 'IFL '
                         REPLACE kar_unimed  ;
                                 WITH  ;
                                 m.unidad
                         REPLACE kar_codmon  ;
                                 WITH  ;
                                 a1(11)
                         REPLACE kar_tipent  ;
                                 WITH  ;
                                 a1(09)
                         REPLACE kar_codent  ;
                                 WITH  ;
                                 a1(10)
                         REPLACE kar_tidore  ;
                                 WITH  ;
                                 a1(13)
                         REPLACE kar_nrdore  ;
                                 WITH  ;
                                 a1(14)
                         UNLOCK
                         SELECT gc_alm00
                         SEEK trasp.codig2
                         STORE 0  ;
                               TO  ;
                               w_stkant
                         SCAN WHILE  ;
                              alm_codpro =  ;
                              trasp.codig2  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              w_stkant =  ;
                               w_stkant +  ;
                               alm_stkfis
                         ENDSCAN
                         GOTO TOP
                         SEEK trasp.codig2 +  ;
                              a1(6)
                         IF FOUND()
                              wrk_almant =  ;
                               alm_stkfis
                         ELSE
                              wrk_almant =  ;
                               0
                         ENDIF
                         SELECT gc_kar00
                         DO rbloquea
                         REPLACE kar_stkant  ;
                                 WITH  ;
                                 wrk_almant
                         REPLACE kar_cantid  ;
                                 WITH  ;
                                 m.cantidad
                         REPLACE kar_import  ;
                                 WITH  ;
                                 ooprecio(m.codig2, ;
                                 1)
                         REPLACE kar_usuari  ;
                                 WITH  ;
                                 clave
                         REPLACE kar_fecha  ;
                                 WITH  ;
                                 DATE()
                         REPLACE kar_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                         SELECT gc_pro00
                         SEEK trasp.codig2
                         IF FOUND()
                              wrk_proant =  ;
                               pro_coprmo
                              wrk_proanb =  ;
                               pro_coprmb
                         ELSE
                              wrk_proant =  ;
                               0
                              wrk_proanb =  ;
                               0
                         ENDIF
                         SELECT gc_kar00
                         DO rbloquea
                         REPLACE kar_almrec  ;
                                 WITH  ;
                                 a1(6)
                         REPLACE kar_cosant  ;
                                 WITH  ;
                                 ROUND(((w_stkant *  ;
                                 wrk_proant) +  ;
                                 (trasp.cantidad *  ;
                                 trasp.coprmo2)) /  ;
                                 (w_stkant +  ;
                                 trasp.cantidad),  ;
                                 2)
                         REPLACE kar_cosanb  ;
                                 WITH  ;
                                 ROUND(((w_stkant *  ;
                                 wrk_proanb) +  ;
                                 (trasp.cantidad *  ;
                                 (ROUND(trasp.coprmo2 *  ;
                                 w_tipcam,  ;
                                 2)))) /  ;
                                 (w_stkant +  ;
                                 trasp.cantidad),  ;
                                 2)
                         REPLACE kar_cosuni  ;
                                 WITH  ;
                                 coprmo2
                         REPLACE kar_usuari  ;
                                 WITH  ;
                                 clave
                         REPLACE kar_fecha  ;
                                 WITH  ;
                                 DATE()
                         REPLACE kar_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                         SELECT gc_pro00
                         SEEK trasp.codig2
                         IF FOUND()
                              DO rbloquea
                              REPLACE  ;
                               pro_coprmo  ;
                               WITH  ;
                               gc_kar00.kar_cosant
                              REPLACE  ;
                               pro_coprmb  ;
                               WITH  ;
                               gc_kar00.kar_cosanb
                              REPLACE  ;
                               pro_ulcomb  ;
                               WITH  ;
                               gc_kar00.kar_cosuni
                              REPLACE  ;
                               pro_coremo  ;
                               WITH  ;
                               gc_kar00.kar_cosuni
                              REPLACE  ;
                               pro_ultmov  ;
                               WITH  ;
                               DATE()
                              REPLACE  ;
                               pro_ultcom  ;
                               WITH  ;
                               a1(3)
                              REPLACE  ;
                               pro_cosrep  ;
                               WITH  ;
                               DATE()
                              REPLACE  ;
                               pro_usuari  ;
                               WITH  ;
                               clave,  ;
                               pro_fecha  ;
                               WITH  ;
                               DATE(),  ;
                               pro_hora  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                         ENDIF
                    ENDIF
                    SELECT trasp
               ENDSCAN
               a1( 12) = DATE()
               IF (m.codigo <>  ;
                  m.codig2) .AND.   ;
                  .NOT.  ;
                  EMPTY(m.codig2)
                    a1( 15) =  ;
                      ROUND(tmp_totpre,  ;
                      2)
               ELSE
                    a1( 15) =  ;
                      ROUND(tmp_totpre /  ;
                      wrk_facigv,  ;
                      2)
               ENDIF
               a1( 17) =  ;
                 ROUND(a1(15) *  ;
                 wrk_facigv, 2)
               a1( 16) = a1(17) -  ;
                 a1(15)
               a1( 20) = a1(17)
               a1( 25) = 'C'
               SELECT gc_hip00
               SEEK a1(1) + a1(2)
               IF  .NOT. FOUND()
                    APPEND BLANK
               ENDIF
               DO rbloquea
               GATHER FROM a1
               REPLACE hip_usuari  ;
                       WITH  ;
                       clave
               REPLACE hip_fecha  ;
                       WITH  ;
                       DATE()
               REPLACE hip_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
               wrk_creo = 1
               SELECT trasp
               GOTO TOP
               SCAN WHILE  .NOT.  ;
                    EOF()
                    IF codigo <>  ;
                       codig2  ;
                       .AND.   ;
                       .NOT.  ;
                       EMPTY(codig2)
                         wrk_cla2 =  ;
                          codig2 +  ;
                          a1(6)
                    ELSE
                         wrk_cla2 =  ;
                          codigo +  ;
                          a1(6)
                    ENDIF
                    wrk_cla1 = codigo +  ;
                               a1(7)
                    SELECT gc_alm00
                    SEEK wrk_cla1
                    IF FOUND()
                         DO rbloquea
                         REPLACE alm_stkfis  ;
                                 WITH  ;
                                 alm_stkfis -  ;
                                 trasp.cantidad
                         REPLACE alm_stkres  ;
                                 WITH  ;
                                 alm_stkres -  ;
                                 trasp.cantidad
                         REPLACE alm_usuari  ;
                                 WITH  ;
                                 clave
                         REPLACE alm_fecha  ;
                                 WITH  ;
                                 DATE()
                         REPLACE alm_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                    ENDIF
                    SEEK wrk_cla2
                    IF FOUND()
                         DO rbloquea
                         REPLACE alm_stkfis  ;
                                 WITH  ;
                                 alm_stkfis +  ;
                                 trasp.cantidad
                         REPLACE alm_usuari  ;
                                 WITH  ;
                                 clave
                         REPLACE alm_fecha  ;
                                 WITH  ;
                                 DATE()
                         REPLACE alm_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                    ELSE
                         APPEND BLANK
                         DO rbloquea
                         REPLACE alm_codpro  ;
                                 WITH  ;
                                 SUBSTR(wrk_cla2,  ;
                                 1,  ;
                                 14)
                         REPLACE alm_codalm  ;
                                 WITH  ;
                                 a1(6)
                         REPLACE alm_stkfis  ;
                                 WITH  ;
                                 trasp.cantidad
                         REPLACE alm_usuari  ;
                                 WITH  ;
                                 clave
                         REPLACE alm_fecha  ;
                                 WITH  ;
                                 DATE()
                         REPLACE alm_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                    ENDIF
                    SELECT trasp
               ENDSCAN
               DEACTIVATE WINDOW  ;
                          titpass,  ;
                          pidpass,  ;
                          a0,  ;
                          titposs,  ;
                          pidposs
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ON KEY
               DEACTIVATE WINDOW  ;
                          tot
               DEACTIVATE POPUP  ;
                          detalle
               ACTIVATE WINDOW a0
          ENDIF
     ELSE
          = INKEY(0)
          DEACTIVATE WINDOW tot
          ON KEY LABEL ENTER DO P_LEE;
   WITH 2
          ON KEY LABEL F2 DO CREA 
          ON KEY LABEL F3 DO P_LEE;
   WITH 1
          ON KEY LABEL F4 DO P_BORRA 
     ENDIF
ELSE
     DEACTIVATE WINDOW tot
     ON KEY LABEL ENTER DO P_LEE;
   WITH 2
     ON KEY LABEL F2 DO CREA 
     ON KEY LABEL F3 DO P_LEE ;
  WITH 1
     ON KEY LABEL F4 DO P_BORRA 
ENDIF
wrk_sw4 = 1
RETURN
*
PROCEDURE facigv
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     wrk_facigv = (1 +  ;
                  (tab_factor /  ;
                  100))
ENDIF
RETURN
*
PROCEDURE pinta1
DEACTIVATE WINDOW titposs
ACTIVATE WINDOW a0
@ 00, 00 CLEAR TO 08, 74
@ 00, 59 SAY 'TC:'
@ 00, 62 SAY w_tipcam PICTURE  ;
  '999,999.99'
@ 00, 00 SAY 'Tipo Doc.. :'
@ 00, 32 SAY 'Nro. Doc.   :'
@ 01, 00 SAY 'Fecha..... :'
@ 02, 00 SAY 'Cod. Mov.  :'
@ 02, 32 SAY 'Mot. Traslad:'
@ 03, 00 SAY 'Alm. Desp. :'
@ 03, 32 SAY 'Alm. Recp.  :'
@ 04, 00 SAY 'Tip. Ent.  :'
@ 04, 32 SAY 'Cod. Ent... :'
@ 05, 00 SAY 'Tip. Transp:'
@ 05, 32 SAY 'Cod. Transp.:'
@ 06, 00 SAY 'Tip. Doc. Ref. :'
@ 06, 32 SAY 'Nro. Doc. Ref. :'
RETURN
*
PROCEDURE carmat
ON KEY
m = 0
SELECT trasp
GOTO TOP
SCAN WHILE  .NOT. EOF()
     m = m + 1
     mat( m, 1) = codigo
     mat( m, 2) = unidad
     mat( m, 3) = descrip
     mat( m, 4) = cantidad
     mat( m, 6) = importe
     mat( m, 7) = docref
ENDSCAN
RETURN
*
PROCEDURE imprime
ON KEY
DO p_mensaje WITH  ;
   'Impresi¢n en proceso. Espere......'
STORE 70 TO aux_lin
STORE 0 TO aux_sw1, aux_con1,  ;
      aux_con2, aux_tv, aux_ta
SET DEVICE TO PRINTER
aux_pep = 'PEDID'
tmp_pep = f_reporte(aux_pep)
set print to &TMP_PEP
SELECT gc_cli00
DO imprimir
ON KEY
rpt = .T.
rpt = f_yesno( ;
      'PREPARE LA IMPRESORA Y CONFIRME ACCION' ;
      )
IF rpt
     stat = SYS(13)
     DO WHILE stat='OFFLINE'
          swt = f_yesno( ;
                'LA IMPRESORA NO ESTA LISTA, VERIFIQUE POR FAVOR' ;
                )
          IF swt
               stat = SYS(13)
          ELSE
               DEACTIVATE POPUP  ;
                          impresora
               RETURN
          ENDIF
     ENDDO
     SET DEVICE TO PRINTER
     DO imprimir
ENDIF
erase  &tmp_pep   
RETURN
*
PROCEDURE imprimir
IF a1(01) = 'GUIA'
     DO r_titu
     aux_lin = aux_lin + 1
     STORE 1 TO w_count
     SELECT trasp
     GOTO TOP
     SCAN WHILE  .NOT. EOF()
          SCATTER MEMVAR
          IF aux_lin > 41
               DO r_titu
          ENDIF
          aux_lin = aux_lin + 1
          @ aux_lin, 00 SAY  ;
            w_count PICTURE  ;
            '9,999'
          @ aux_lin, 06 SAY  ;
            m.codigo
          @ aux_lin, 21 SAY  ;
            SUBSTR(m.descrip, 1,  ;
            38)
          IF trasp.codigo <>  ;
             trasp.codig2 .AND.   ;
             .NOT.  ;
             EMPTY(trasp.codig2)
               @ aux_lin, 60 SAY  ;
                 trasp.codig2
          ENDIF
          SELECT gc_alm00
          SEEK trasp.codigo +  ;
               a1(7)
          IF FOUND()
               w_ubica = SUBSTR(gc_alm00.alm_ubicac,  ;
                         1, 5)
          ELSE
               STORE SPACE(5) TO  ;
                     w_ubica
          ENDIF
          SELECT trasp
          IF codigo <> codig2  ;
             .AND.  .NOT.  ;
             EMPTY(codig2)
               SELECT gc_alm00
               GOTO TOP
               SEEK trasp.codig2 +  ;
                    a1(6)
               IF FOUND()
                    w_ubica = w_ubica +  ;
                              ' ' +  ;
                              SUBSTR(gc_alm00.alm_ubicac,  ;
                              1,  ;
                              5)
               ENDIF
               SELECT trasp
               @ aux_lin, 75 SAY  ;
                 w_ubica
               @ aux_lin, 088 SAY  ;
                 m.docref
               @ aux_lin, 099 SAY  ;
                 m.cantidad  ;
                 PICTURE  ;
                 '999,999'
               @ aux_lin, 107 SAY  ;
                 m.coprmo2  ;
                 PICTURE  ;
                 '999,999.99'
               @ aux_lin, 121 SAY  ;
                 (m.coprmo2 *  ;
                 m.cantidad)  ;
                 PICTURE  ;
                 '9,999,999.99'
          ELSE
               @ aux_lin, 75 SAY  ;
                 w_ubica
               @ aux_lin, 088 SAY  ;
                 m.docref
               @ aux_lin, 099 SAY  ;
                 m.cantidad  ;
                 PICTURE  ;
                 '999,999'
               @ aux_lin, 107 SAY  ;
                 m.importe  ;
                 PICTURE  ;
                 '999,999.99'
               @ aux_lin, 121 SAY  ;
                 (m.importe *  ;
                 m.cantidad)  ;
                 PICTURE  ;
                 '9,999,999.99'
          ENDIF
          w_count = w_count + 1
     ENDSCAN
ELSE
     DO r_titulo
     aux_lin = aux_lin + 1
     STORE 1 TO w_count
     SELECT trasp
     GOTO TOP
     SCAN WHILE  .NOT. EOF()
          SCATTER MEMVAR
          IF aux_lin > 60
               DO r_titulo
          ENDIF
          aux_lin = aux_lin + 1
          @ aux_lin, 00 SAY  ;
            w_count PICTURE  ;
            '9,999'
          @ aux_lin, 06 SAY  ;
            m.codigo
          @ aux_lin, 21 SAY  ;
            m.descrip
          IF trasp.codigo <>  ;
             trasp.codig2 .AND.   ;
             .NOT.  ;
             EMPTY(trasp.codig2)
               @ aux_lin, 60 SAY  ;
                 trasp.codig2
          ENDIF
          SELECT gc_alm00
          SEEK trasp.codigo +  ;
               a1(7)
          IF FOUND()
               w_ubica = SUBSTR(gc_alm00.alm_ubicac,  ;
                         1, 5)
          ELSE
               STORE SPACE(5) TO  ;
                     w_ubica
          ENDIF
          SELECT trasp
          IF codigo <> codig2  ;
             .AND.  .NOT.  ;
             EMPTY(codig2)
               SELECT gc_alm00
               GOTO TOP
               SEEK trasp.codig2 +  ;
                    a1(6)
               IF FOUND()
                    w_ubica = w_ubica +  ;
                              ' ' +  ;
                              SUBSTR(gc_alm00.alm_ubicac,  ;
                              1,  ;
                              5)
               ENDIF
               SELECT trasp
               @ aux_lin, 75 SAY  ;
                 w_ubica
               @ aux_lin, 088 SAY  ;
                 m.docref
               @ aux_lin, 099 SAY  ;
                 m.cantidad  ;
                 PICTURE  ;
                 '999,999'
               @ aux_lin, 107 SAY  ;
                 m.coprmo2  ;
                 PICTURE  ;
                 '999,999.99'
               @ aux_lin, 121 SAY  ;
                 (m.coprmo2 *  ;
                 m.cantidad)  ;
                 PICTURE  ;
                 '9,999,999.99'
          ELSE
               @ aux_lin, 75 SAY  ;
                 w_ubica
               @ aux_lin, 088 SAY  ;
                 m.docref
               @ aux_lin, 099 SAY  ;
                 m.cantidad  ;
                 PICTURE  ;
                 '999,999'
               @ aux_lin, 107 SAY  ;
                 m.importe  ;
                 PICTURE  ;
                 '999,999.99'
               @ aux_lin, 121 SAY  ;
                 (m.importe *  ;
                 m.cantidad)  ;
                 PICTURE  ;
                 '9,999,999.99'
          ENDIF
          IF aux_lin > 53
               DO r_titulo
          ENDIF
          w_count = w_count + 1
     ENDSCAN
     aux_lin = aux_lin + 1
     @ aux_lin, 099 SAY  ;
       'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ'
     aux_lin = aux_lin + 1
     @ aux_lin, 090 SAY 'TOTAL :'
     @ aux_lin, 099 SAY  ;
       tmp_totcan PICTURE  ;
       '999,999'
     @ aux_lin, 119 SAY  ;
       tmp_totpre PICTURE  ;
       '999,999,999.99'
     @ aux_lin, 00 SAY CHR(27) +  ;
       'F'
     @ aux_lin, 00 SAY CHR(18)
     @ 56, 00 SAY  ;
       'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
     @ 57, 00 SAY  ;
       '³    OBSERVACIONES    ³  Vo.Bo. Almac‚n  ³   VoBo Ventas  ³ VoBoAdministraci¢n ³'
     @ 58, 00 SAY  ;
       'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´'
     @ 59, 00 SAY  ;
       '³                     ³                  ³                ³                    ³'
     @ 60, 00 SAY  ;
       'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
     ??? CHR(27) + 'P'
ENDIF
EJECT
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE r_titulo
@ 00, 00 SAY CHR(27) + 'U' + '1'
@ 00, 00 SAY CHR(27) + 'M'
@ 00, 00 SAY CHR(14)
@ 00, 00 SAY CHR(27) + 'E'
@ 00, 00 SAY rge_razsoc
@ 01, 00 SAY CHR(14)
@ 01, 00 SAY rge_abrev
@ 04, 00 SAY CHR(14)
wrk_busca = 'TRAS'
@ 04, 10 SAY  ;
  'TRASPASO ENTRE BODEGAS'
@ 05, 00 SAY CHR(27) + 'F'
@ 05, 00 SAY CHR(20)
@ 05, 00 SAY  ;
  'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 06, 00 SAY  ;
  '³TIPO DE DOCUMENTO:'
@ 06, 20 SAY a1(01)
SELECT ge_tab0
SEEK 'DOCU' + a1(01)
@ 06, 25 SAY SUBSTR(tab_destab, 1,  ;
  17)
@ 06, 43 SAY 'NUMERO        :'
@ 06, 64 SAY a1(02)
@ 06, 79 SAY '³'
@ 07, 00 SAY  ;
  '³FECHA            :'
@ 07, 20 SAY a1(03)
@ 07, 79 SAY '³'
@ 08, 00 SAY  ;
  '³MOVIMIENTO       :'
@ 08, 20 SAY a1(05)
SELECT ge_tab0
SEEK wrk_busca + a1(05)
@ 08, 25 SAY SUBSTR(tab_destab, 1,  ;
  17)
@ 08, 43 SAY 'MOTIV.TRASLADO:'
@ 08, 58 SAY a1(30)
SELECT ge_tab0
SEEK 'MTRA' + a1(30)
@ 08, 63 SAY SUBSTR(tab_destab, 1,  ;
  17)
@ 08, 79 SAY '³'
@ 09, 00 SAY  ;
  '³ALM. DE DESPACHO :'
@ 09, 20 SAY a1(7)
SELECT ge_tab0
SEEK 'ALMA' + a1(7)
@ 09, 25 SAY SUBSTR(tab_destab, 1,  ;
  17)
@ 09, 43 SAY 'ALM. DE RECEP.:'
@ 09, 58 SAY a1(6)
SELECT ge_tab0
SEEK 'ALMA' + a1(6)
@ 09, 63 SAY SUBSTR(tab_destab, 1,  ;
  17)
@ 09, 79 SAY '³'
@ 10, 00 SAY  ;
  '³ENTIDAD          :'
@ 10, 20 SAY a1(09)
SELECT ge_tab0
SEEK 'ENTI' + a1(09)
@ 10, 25 SAY SUBSTR(tab_destab, 1,  ;
  17)
@ 10, 43 SAY 'CODIGO :'
@ 10, 51 SAY a1(10)
IF a1(09) = 'P' .OR. a1(09) = 'C'  ;
   .OR. a1(09) = 'I'
     SELECT gc_cli00
ELSE
     SELECT gc_vnd00
ENDIF
SEEK ALLTRIM(a1(09)) + a1(10)
IF  .NOT. FOUND()
     @ 10, 63 SAY 'No Existe'
ELSE
     IF a1(09) = 'P' .OR. a1(09) =  ;
        'C' .OR. a1(09) = 'I'
          @ 10, 61 SAY  ;
            SUBSTR(cli_razsoc, 1,  ;
            17)
     ELSE
          @ 10, 61 SAY  ;
            SUBSTR(vnd_nombre, 1,  ;
            17)
     ENDIF
ENDIF
@ 10, 79 SAY '³'
@ 11, 00 SAY  ;
  '³T.TRANSPORTISTA  :'
@ 11, 20 SAY a1(31)
SELECT ge_tab0
SEEK 'ENTI' + a1(31)
@ 11, 25 SAY SUBSTR(tab_destab, 1,  ;
  17)
@ 11, 43 SAY 'CODIGO :'
@ 11, 51 SAY a1(29)
IF a1(31) = 'P' .OR. a1(31) = 'C'  ;
   .OR. a1(31) = 'I'
     SELECT gc_cli00
ELSE
     SELECT gc_vnd00
ENDIF
SEEK ALLTRIM(a1(31)) + a1(29)
IF  .NOT. FOUND()
     @ 11, 63 SAY 'No Existe'
ELSE
     IF a1(31) = 'P' .OR. a1(31) =  ;
        'C' .OR. a1(31) = 'I'
          @ 11, 61 SAY  ;
            SUBSTR(cli_razsoc, 1,  ;
            17)
     ELSE
          @ 11, 61 SAY  ;
            SUBSTR(vnd_nombre, 1,  ;
            17)
     ENDIF
ENDIF
@ 11, 79 SAY '³'
@ 12, 00 SAY '³'
@ 12, 01 SAY 'TIPO DE DOC.  REF:'
@ 12, 20 SAY a1(13)
SELECT ge_tab0
SEEK 'DOCU' + a1(13)
IF FOUND()
     @ 12, 25 SAY  ;
       SUBSTR(tab_destab, 1, 17)
ENDIF
@ 12, 43 SAY 'NUMERO        :'
@ 12, 58 SAY a1(14)
@ 12, 79 SAY '³'
@ 13, 00 SAY  ;
  'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
@ 14, 00 SAY CHR(27) + 'F'
@ 14, 00 SAY CHR(15)
@ 14, 00 SAY  ;
  'ÚÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 15, 00 SAY  ;
  '³ITEM³    CODIGO   ³          DESCRIPCION              ³    A CODIGO    ³   UBICACION  ³  OBSERVA. ³CANTID.³PRECIO UNI³    TOTAL    ³'
@ 16, 00 SAY  ;
  'ÀÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
aux_lin = 17
SELECT trasp
RETURN
*
PROCEDURE r_titu
@ 00, 00 SAY CHR(27) + 'F'
@ 00, 00 SAY CHR(18)
@ 03, 54 SAY 'R.U.C.  ' +  ;
  rge_numruc
@ 04, 00 SAY CHR(15)
@ 10, 03 SAY  ;
  'Domicilio fiscal : AV. REP. DE PANAMA 4123 - SURQUILLO'
IF a1(30) = '001'
     @ 13, 00 SAY 'X'
ELSE
     IF a1(30) = '002'
          @ 14, 00 SAY 'X'
     ELSE
          IF a1(30) = '003'
               @ 15, 00 SAY 'X'
          ELSE
               IF a1(30) = '004'
                    @ 13, 27 SAY  ;
                      'X'
               ELSE
                    IF a1(30) =  ;
                       '005'
                         @ 14, 27  ;
                           SAY  ;
                           'X'
                    ELSE
                         IF a1(30) =  ;
                            '006'
                              @ 15,  ;
                                27  ;
                                SAY  ;
                                'X'
                         ELSE
                              IF a1(30) =  ;
                                 '007'
                                   @ 13, 42 SAY 'X'
                              ELSE
                                   @ 14, 42 SAY 'X'
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
@ 15, 090 SAY a1(01)
@ 15, 100 SAY a1(02)
@ 17, 21 SAY rge_abrev
@ 17, 101 SAY a1(03)
@ 18, 21 SAY ALLTRIM(rge_calle) +  ;
  ' ' + ALLTRIM(rge_distri) +  ;
  ' - ' + rge_provin
SELECT ge_tab0
SEEK 'ALMA' + a1(6)
IF a1(06) < '0080' .OR. a1(06) =  ;
   '0100' .OR. a1(06) = '0130'  ;
   .OR. a1(06) = '0140'
     @ 19, 21 SAY  ;
       ALLTRIM(rge_abrev) + ' ' +  ;
       tab_destab
ELSE
     @ 19, 21 SAY tab_destab
ENDIF
@ 19, 101 SAY '25360443'
SEEK 'DISU' + a1(6)
IF FOUND()
     @ 20, 21 SAY tab_destab
ELSE
     @ 20, 21 SAY ' '
ENDIF
IF a1(31) = 'P' .OR. a1(31) = 'C'  ;
   .OR. a1(31) = 'I'
     SELECT gc_cli00
ELSE
     SELECT gc_vnd00
ENDIF
SEEK ALLTRIM(a1(31)) + a1(29)
w_direc = SPACE(30)
IF  .NOT. FOUND()
     @ 22, 023 SAY ' '
ELSE
     IF a1(31) = 'P' .OR. a1(31) =  ;
        'C' .OR. a1(31) = 'I'
          @ 22, 21 SAY cli_razsoc
          w_direc = cli_calle +  ;
                    cli_numero
     ELSE
          @ 22, 21 SAY vnd_nombre
          w_direc = vnd_direc
     ENDIF
ENDIF
IF a1(31) = 'T' .AND.  ;
   SUBSTR(vnd_observ, 1, 8) <>  ;
   SPACE(8)
     @ 22, 101 SAY  ;
       SUBSTR(vnd_observ, 1, 8)
ENDIF
@ 23, 21 SAY w_direc
@ 25, 110 SAY 'US $'
@ 25, 126 SAY 'US $'
aux_lin = 25
SELECT trasp
RETURN
*
PROCEDURE moneda
PARAMETER mon_base, mon_origen
ON KEY
fac_mon = mon_base / mon_origen
RETURN
*
PROCEDURE uniinv
PARAMETER uni_facto1, uni_facto2
ON KEY
fac_uni = uni_facto1 / uni_facto2
RETURN
*
PROCEDURE carque
ON KEY
IF wrk_flag = 0
     CREATE CURSOR TRASP (codigo  ;
            C (14), unidad C (4),  ;
            descrip C (40),  ;
            cantidad N (9, 2),  ;
            importe N (9, 2),  ;
            docref C (10), codig2  ;
            C (14), coprmo2 N (9,  ;
            2))
     DIMENSION a2( FCOUNT())
ELSE
     SELECT trasp
     DIMENSION a2( FCOUNT() + 1)
ENDIF
SCATTER BLANK TO a2
COUNT TO wrk_count
reg_count = wrk_count
DO cartec
RETURN
*
PROCEDURE cartec
ON KEY
IF wrk_count = 0
     DO p_footer WITH  ;
        '101010000000000000001000',  ;
        2
ELSE
     DO p_footer WITH  ;
        '111100000000101000001000',  ;
        2
     ON KEY LABEL ENTER DO P_LEE;
   WITH 2
     ON KEY LABEL F2 DO CREA 
     ON KEY LABEL F4 DO P_BORRA
ENDIF
IF wrk_count > 15 .AND. a1(01) =  ;
   'GUIA'
     ON KEY LABEL F3
ELSE
     ON KEY LABEL F3 DO P_LEE ;
  WITH 1
ENDIF
SELECT trasp
RETURN
*
PROCEDURE transito_2
PARAMETER can_2
SELECT gc_alm00
SEEK a2(1) + a1(7)
IF FOUND()
     DO rbloquea
     IF (can_2 * fac_uni) <=  ;
        (alm_stkfis - alm_stkres +  ;
        tran_ant)
          REPLACE alm_stkres WITH  ;
                  alm_stkres +  ;
                  (can_2 *  ;
                  fac_uni) -  ;
                  tran_ant
          REPLACE alm_usuari WITH  ;
                  clave
          REPLACE alm_fecha WITH  ;
                  DATE()
          REPLACE alm_hora WITH  ;
                  TIME()
          wrk_mal = 0
     ELSE
          DO p_mensaje WITH  ;
             'CANTIDAD SOLICITADA MAYOR QUE STOCK DISPONIBLE'
          wrk_mal = 1
     ENDIF
     UNLOCK
ELSE
     DO p_mensaje WITH  ;
        'PRODUCTO NO EXISTE EN EL ALMACEN'
     wrk_mal = 1
ENDIF
RETURN
*
PROCEDURE when2c
tran_ant = a2(4)
RETURN
*
FUNCTION vali2c
IF LASTKEY() <> 13
     IF LASTKEY() = 27
          RETURN
     ELSE
          RETURN .F.
     ENDIF
ENDIF
IF a2(4) = 0
     DO p_mensaje WITH  ;
        'CANTIDAD NO PUEDE SER CERO..'
     RETURN .F.
ENDIF
IF a2(4) < 0
     DO p_mensaje WITH  ;
        'CANTIDAD NO PUEDE SER NEGATIVO..'
     RETURN .F.
ENDIF
= transito_2(a2(4))
IF wrk_mal = 1
     a2( 4) = tran_ant
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE produc
PARAMETER wrk_campo, wrk_selec,  ;
          wrk_selpro
ON KEY
wrk_order = ORDER()
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW produ FROM 12, 02  ;
       TO 20, 76 IN screen COLOR  ;
       SCHEME 8
DEFINE POPUP prod FROM 16, 31  ;
       COLOR SCHEME 8
DEFINE BAR 1 OF prod PROMPT  ;
       '\<C¢digo'
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripci¢n'
DEFINE BAR 3 OF prod PROMPT  ;
       '\<Sub-Categ.'
DEFINE BAR 4 OF prod PROMPT  ;
       '\<Nro. Parte'
ON SELECTION POPUP prod do buspro with;
bar(),WRK_SELPRO
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide
ON KEY LABEL f6 do produc with WRK_CAMPO,WRK_SELEC,WRK_SELPRO
IF LASTKEY() <> 27
     &WRK_CAMPO = substr(gc_pro00.pro_codpro,1,14)
ENDIF
SELECT (wrk_selec)
KEYBOARD CHR(13)
RETURN
*
PROCEDURE buspro
PARAMETER bar, wrk_selpro
ON KEY
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, -1
ENDFOR
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY -1, 0
ENDFOR
ACTIVATE WINDOW pide
SELECT (wrk_selpro)
IF bar = 1
     w_codpro = SPACE(14)
     SET ORDER TO codigo
     @ 00, 00 SAY 'C¢digo :'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 2
     w_codpro = SPACE(40)
     SET ORDER TO descri
     @ 00, 00 SAY 'Descr. :'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 3
     w_codpro = SPACE(4)
     SET ORDER TO subcat
     @ 00, 00 SAY 'Sub-Cat :'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 4
     w_codpro = SPACE(10)
     SET ORDER TO parara
     @ 00, 00 SAY 'Partida :'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
READ
SELECT gc_alm00
SET FILTER TO
SET FILTER TO alm_codalm = a1(7)
SELECT gc_pro00
SET RELATION TO pro_codpro INTO gc_alm00
IF LASTKEY() <> 27
     SET NEAR ON
     SEEK w_codpro
     SET NEAR OFF
     w_codpro = SPACE(14)
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do tomacod
     BROWSE FIELDS pro_codpro :R  ;
            :H = 'C¢d. Produc.',  ;
            pro_descri :R : 25 :H =  ;
            'Descripci¢n ',  ;
            pro_modelo :R : 10 :H =  ;
            'Modelo', pro_codree  ;
            :R :H = 'Reemplazo',  ;
            gc_alm00.alm_stkfis  ;
            :R :P = '9999' :H =  ;
            'F¡sic',  ;
            gc_alm00.alm_stkres  ;
            :R :P = '9999' :H =  ;
            'Reser', pro_coprmo  ;
            :R : 10 :H =  ;
            'CostoProm.' :P =  ;
            '999,999.99',  ;
            pro_subcat :R :H =  ;
            'Sub-Cat' IN produ
ENDIF
SET FILTER TO
ON KEY
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

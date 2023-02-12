*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
DEFINE WINDOW t0 FROM 02, 64 TO  ;
       02, 73 IN screen NONE
DEFINE WINDOW a0 FROM 04, 03 TO  ;
       08, 76 IN screen
DEFINE POPUP detalle FROM 13, 03  ;
       TO 18, 76 PROMPT FIELDS  ;
       codigo + '³' + unidad +  ;
       '³' + descrip + '³ ' +  ;
       TRANSFORM(cantidad,  ;
       '@ 999,999.99') + '³' +  ;
       TRANSFORM(importe,  ;
       '@ 999,999.99') + '³' +  ;
       TRANSFORM(total,  ;
       '@ 999,999.99') IN screen  ;
       COLOR SCHEME 8
ON SELECTION POPUP detalle DEAC POPUP;
DETALLE
ACTIVATE WINDOW tablas
flag = .F.
DO p_prestab WITH 'PROCESO',  ;
   'CONSUMO INTERNO', 'SELECCION',  ;
   flag
@ 2, 63 SAY PROGRAM()
SELECT 1
USE SHARED GC_HIP00 ORDER CODIGO
SELECT 2
USE SHARED GC_DIP00 ORDER CODIGO
SELECT 3
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 4
USE SHARED GC_UNI00 ORDER CODIGO
SELECT 6
USE SHARED GC_KAR00 ORDER CODIGO
SELECT 7
USE SHARED GC_CLI00 ORDER CODIGO
SELECT 8
USE SHARED GC_VND00 ORDER CODIGO
SELECT 9
USE SHARED GC_ALM00 ORDER CODIGO
SELECT 10
USE SHARED GC_CMV00 ORDER CODIGO
SELECT 11
USE SHARED GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 12
USE SHARED GC_DLP00 ORDER CODIGO
STORE 0 TO wrk_termin, wrk_facigv
STORE SPACE(12) TO wrk_idx
= facigv()
IF wrk_facigv = 0
     DO pmensaje WITH  ;
        'NO HAY IMPUESTO A LAS VENTAS'
     wrk_termin = 1
ENDIF
SELECT gc_hip00
DIMENSION a1( FCOUNT())
SCATTER BLANK TO a1
DO WHILE wrk_termin=0
     STORE 0 TO tmp_1, wrk_creo,  ;
           wrk_sw2, wrk_sw3,  ;
           wrk_totcan, wrk_totpre,  ;
           tmp_totcan, tmp_totpre,  ;
           dif_totcan, dif_totpre,  ;
           wrk_pos, wrk_x,  ;
           wrk_mone1, wrk_cambio,  ;
           wrk_prec, wrk_almx,  ;
           wrk_alm, wrk_alma,  ;
           wrk_lin, wrk_gas,  ;
           wrk_totgas, wrk_totdip,  ;
           wrk_err, wrk_cambix,  ;
           wrk_stkant, wrk_bus2,  ;
           wrk_bus1, fac_mon,  ;
           fac_uni, costo,  ;
           cos_pro, wrk_count,  ;
           tran_1, tran_ant,  ;
           wrk_mal, tran_ant
     STORE 1 TO wrk_sw4,  ;
           wrk_cambiy, wrk_swfin
     ACTIVATE WINDOW t0
     @ 00, 00 SAY 'SELECCIONA'
     SET CURSOR ON
     DO proceso
ENDDO
DEACTIVATE WINDOW tablas, titpass,  ;
           a0, titposs
RELEASE WINDOW t0, a0, a1, c0, a2,  ;
        a3
CLOSE DATABASES
IF FILE(wrk_idx)
     ERASE &WRK_IDX
ENDIF
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
             '100010000000000000000',  ;
             2
          ACTIVATE WINDOW a0
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
                 1, 13)
          ENDIF
          DO WHILE .T. .AND.  ;
             tmp_1=0
               ON KEY
               DO p_footer WITH  ;
                  '100000000000000000000',  ;
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
             '100000000000000000000',  ;
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
                  '100010000000000000000',  ;
                  2
               ACTIVATE WINDOW a0
               a1( 05) = 'COI '
               wrk_busca = 'CONS'
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
                    ON KEY
                    wrk_busca = 'CENC'
                    wrk_var = 'A1(08)'
                    ON KEY LABEL F6 DO;
BUSCA WITH WRK_BUSCA,WRK_VAR
                    ACTIVATE WINDOW  ;
                             a0
                    @ 02, 46 GET  ;
                      a1( 08)  ;
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
                    SEEK 'CENC' +  ;
                         a1(08)
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'Centro de Costos no Existe'
                         LOOP
                    ELSE
                         @ 02, 51  ;
                           SAY  ;
                           SUBSTR(tab_destab,  ;
                           1,  ;
                           13)
                    ENDIF
                    DO WHILE .T.  ;
                       .AND.  ;
                       tmp_1=0
                         ON KEY
                         IF wrk_sw2 =  ;
                            0  ;
                            .AND.  ;
                            wrk_x =  ;
                            0
                              a1(  ;
                                07) =  ;
                                rge_codalm
                              wrk_busca =  ;
                               'ALMA'
                              wrk_var =  ;
                               'A1(07)'
                              ON KEY LABEL;
F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                              @ 03,  ;
                                13  ;
                                GET  ;
                                a1(  ;
                                07)  ;
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
                               a1(07)
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
                               'ENTI'
                              wrk_var =  ;
                               'A1(09)'
                              a1(  ;
                                09) =  ;
                                'A   '
                              ON KEY LABEL;
F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                              @ 04,  ;
                                13  ;
                                GET  ;
                                a1(  ;
                                09)  ;
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
                               'ENTI' +  ;
                               a1(09)
                              IF   ;
                               .NOT.  ;
                               FOUND()
                                   DO p_mensaje WITH 'No Existe'
                                   LOOP
                              ELSE
                                   @ 04, 18 SAY SUBSTR(tab_destab, 1, 13)
                              ENDIF
                              DO WHILE  ;
                                 .T.  ;
                                 .AND.  ;
                                 tmp_1= ;
                                 0
                                   ON KEY
                                   wrk_busca = a1(09)
                                   wrk_var = 'A1(10)'
                                   ON KEY LABEL F6 DO ENTIDAD WITH WRK_BUSCA,WRK_VAR
                                   @ 04, 46 GET a1( 10) PICTURE '@!'
                                   READ
                                   IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                        EXIT
                                   ENDIF
                                   IF LASTKEY() = 27
                                        tmp_1 = 1
                                        EXIT
                                   ENDIF
                                   IF a1(09) = 'P' .OR. a1(09) = 'C'
                                        SELECT gc_cli00
                                   ELSE
                                        SELECT gc_vnd00
                                   ENDIF
                                   SEEK ALLTRIM(a1(09)) + a1(10)
                                   IF  .NOT. FOUND()
                                        IF a1(09) = 'P' .OR. a1(09) = 'C'
                                             DO p_mensaje WITH 'Proveedor o Cliente no Existe'
                                             LOOP
                                        ELSE
                                             DO p_mensaje WITH 'Administrativo o Vendedor no Existe'
                                             LOOP
                                        ENDIF
                                   ELSE
                                        IF a1(09) = 'P' .OR. a1(09) = 'C'
                                             @ 04, 56 SAY SUBSTR(cli_razsoc, 1, 15)
                                        ELSE
                                             @ 04, 56 SAY SUBSTR(vnd_nombre, 1, 15)
                                        ENDIF
                                   ENDIF
                                   DO WHILE .T. .AND. tmp_1=0
                                        DO p_footer WITH '100010000000000000000', 2
                                        wrk_busca = 'DOCU'
                                        wrk_var = 'A1(13)'
                                        ON KEY LABEL F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                                        ACTIVATE WINDOW a0
                                        @ 05, 17 GET a1( 13) PICTURE '@!'
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
                                                  @ 05, 22 SAY SUBSTR(tab_destab, 1, 13)
                                             ENDIF
                                        ENDIF
                                        DO WHILE .T. .AND. tmp_1=0
                                             DO p_footer WITH '100000000000000000000', 2
                                             ON KEY
                                             ACTIVATE WINDOW a0
                                             @ 05, 49 GET a1( 14) PICTURE '@!'
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
     CLEAR GETS
     IF LASTKEY() = 27 .AND.  ;
        tmp_1 = 1 .AND. wrk_count >  ;
        0
          SELECT consu
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SELECT gc_alm00
               SEEK consu.codigo +  ;
                    a1(7)
               IF FOUND()
                    DO rbloquea
                    REPLACE alm_stkres  ;
                            WITH  ;
                            alm_stkres -  ;
                            consu.cantidad
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
               SELECT consu
          ENDSCAN
          RETURN
     ENDIF
     IF LASTKEY() <> 27
          DO detalle
          wrk_sw2 = 0
     ENDIF
     IF wrk_creo = 0 .AND.  ;
        wrk_sw3 = 0 .AND.  ;
        wrk_count > 0
          SELECT consu
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SELECT gc_alm00
               SEEK consu.codigo +  ;
                    a1(7)
               IF FOUND()
                    DO rbloquea
                    REPLACE alm_stkres  ;
                            WITH  ;
                            alm_stkres -  ;
                            consu.cantidad
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
               SELECT consu
          ENDSCAN
     ENDIF
ENDDO
RETURN
*
PROCEDURE detalle
ON KEY
DEFINE WINDOW titpass FROM 10, 03  ;
       TO 12, 76 IN screen NONE  ;
       COLOR SCHEME 8
DEFINE WINDOW pidpass FROM 19, 03  ;
       TO 21, 76 IN screen NONE  ;
       COLOR SCHEME 20
FOR x = 1 TO 3
     SIZE WINDOW a0 BY -1, 0
ENDFOR
ACTIVATE WINDOW titpass
@ 00, 00 SAY  ;
  'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 01, 00 SAY  ;
  '³   PRODUCTO   ³UNI.³   DESCRIPCION   ³  CANTIDAD ³ COS.PROM ³   TOTAL   ³'
@ 02, 00 SAY  ;
  'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ'
DO WHILE LASTKEY()<>27 .AND.  ;
   wrk_sw4<>0
     DO carque
     DO WHILE LASTKEY()<>27 .AND.  ;
        LASTKEY()<>-1 .AND.  ;
        wrk_sw4<>0 .AND. wrk_sw2<> ;
        1
          DO WHILE wrk_swfin=1
               wrk_swfin = 0
               SELECT consu
               ACTIVATE POPUP  ;
                        detalle
          ENDDO
          wrk_swfin = 1
     ENDDO
ENDDO
DEACTIVATE WINDOW tot, titpass
tmp_1 = 0
ON KEY
DO p_footer WITH  ;
   '100010000000000000', 2
RETURN
*
PROCEDURE p_lee
PARAMETER aux_opcion
ON KEY
swt = .T.
STORE 0 TO tmp_fin, tmp_1
DO WHILE swt .AND. (LASTKEY()=13  ;
   .OR. LASTKEY()=-2)
     ACTIVATE WINDOW pidpass
     @ 00, 00 SAY  ;
       'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿'
     @ 01, 00 SAY  ;
       '³              ³    ³                 ³           ³          ³           ³'
     @ 02, 00 SAY  ;
       'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ'
     SELECT consu
     DIMENSION a2( FCOUNT())
     IF aux_opcion = 1
          SCATTER BLANK TO a2
     ELSE
          SCATTER TO a2
     ENDIF
     IF aux_opcion = 2
          @ 01, 01 SAY a2(1)  ;
            PICTURE '@!'
          @ 01, 16 SAY a2(2)  ;
            PICTURE '@!'
          @ 01, 21 SAY a2(3)  ;
            PICTURE '@!'
          @ 01, 40 SAY a2(4)  ;
            PICTURE '99,999,999'
          @ 01, 51 SAY a2(5)  ;
            PICTURE '999,999.99'
          @ 01, 62 SAY a2(6)  ;
            PICTURE  ;
            '9999,999.99'
     ENDIF
     tmp_err = 1
     DO WHILE .T. .AND. tmp_1=0
          IF aux_opcion = 1
               DO p_footer WITH  ;
                  '100010000000000000000',  ;
                  2
               ACTIVATE WINDOW  ;
                        pidpass
               wrk_selec = SELECT()
               wrk_campo = 'A2(1)'
               ON KEY LABEL F6 DO PRODUC;
WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO
               @ 01, 01 GET a2(  ;
                 01) PICTURE  ;
                 '@!'
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
                    a2( 3) =  ;
                      SUBSTR(pro_descri,  ;
                      1, 17)
                    a2( 2) =  ;
                      pro_unimed
                    @ 01, 21 SAY  ;
                      a2(3)
               ENDIF
          ENDIF
          DO WHILE .T. .AND.  ;
             tmp_1=0
               IF aux_opcion = 1
                    DO p_footer  ;
                       WITH  ;
                       '100010000000000000000',  ;
                       2
                    ACTIVATE WINDOW  ;
                             pidpass
                    wrk_var = 'A2(2)'
                    wrk_busca = a2(1)
                    ON KEY LABEL F6 DO;
UNIALT WITH WRK_BUSCA,WRK_VAR
                    @ 01, 16 GET  ;
                      a2( 2)  ;
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
                    SELECT consu
                    SEEK a2(1) +  ;
                         a2(2)
                    IF FOUND()
                         wrk_pos =  ;
                          1
                    ELSE
                         wrk_pos =  ;
                          0
                    ENDIF
                    IF wrk_pos =  ;
                       1
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
                         ENDIF
                    ENDIF
               ENDIF
               fac_uni = 1
               DO WHILE .T. .AND.  ;
                  tmp_1=0
                    @ 01, 41 GET  ;
                      a2( 4)  ;
                      DEFAULT 0  ;
                      PICTURE  ;
                      '9,999,999'  ;
                      VALID  ;
                      vali2c()  ;
                      WHEN  ;
                      transito_1()
                    READ
                    IF LASTKEY() <>  ;
                       27
                         SELECT gc_pro00
                         SEEK a2(1)
                         IF FOUND()
                              a2(  ;
                                5) =  ;
                                (pro_coprmo *  ;
                                fac_uni)
                         ELSE
                              a2(  ;
                                5) =  ;
                                0
                         ENDIF
                         @ 01, 51  ;
                           SAY  ;
                           a2(5)  ;
                           PICTURE  ;
                           '999,999.99'
                         a2( 6) =  ;
                           a2(4) *  ;
                           a2(5)
                         @ 01, 62  ;
                           SAY  ;
                           a2(6)  ;
                           PICTURE  ;
                           '9999,999.99'
                    ENDIF
                    tmp_1 = 1
               ENDDO
          ENDDO
     ENDDO
     IF LASTKEY() <> 27 .AND.  ;
        LASTKEY() = 13
          SELECT consu
          IF aux_opcion = 1
               APPEND BLANK
               REPLACE codigo  ;
                       WITH  ;
                       a2(1)
               REPLACE descrip  ;
                       WITH  ;
                       a2(3)
          ENDIF
          REPLACE unidad WITH  ;
                  a2(2)
          REPLACE cantidad WITH  ;
                  a2(4)
          REPLACE importe WITH  ;
                  a2(5)
          REPLACE total WITH  ;
                  a2(6)
          DEACTIVATE WINDOW  ;
                     pidpass,  ;
                     pidposs
     ENDIF
     EXIT
ENDDO
DEACTIVATE WINDOW pidpass,  ;
           pidposs
SELECT consu
COUNT TO wrk_count
DO cartec
RETURN
*
PROCEDURE p_borra
ON KEY
aux_swt = .T.
DO WHILE aux_swt
     SELECT gc_alm00
     SEEK consu.codigo + a1(7)
     IF FOUND()
          DO rbloquea
          REPLACE alm_stkres WITH  ;
                  alm_stkres -  ;
                  consu.cantidad
          REPLACE alm_usuari WITH  ;
                  clave
          REPLACE alm_fecha WITH  ;
                  DATE()
          REPLACE alm_hora WITH  ;
                  TIME()
          UNLOCK
     ENDIF
     SELECT consu
     DELETE
     EXIT
ENDDO
DO cartec
RETURN
*
PROCEDURE crea
ON KEY
STORE 0 TO wrk_sw4, tmp_totcan,  ;
      tmp_totpre
DEFINE WINDOW tot FROM 17, 15 TO  ;
       21, 65 IN screen
ACTIVATE WINDOW tot
SELECT consu
GOTO TOP
SCAN WHILE  .NOT. EOF()
     tmp_totcan = tmp_totcan +  ;
                  cantidad
     tmp_totpre = tmp_totpre +  ;
                  total
ENDSCAN
@ 1, 0 SAY ' TOTAL CANTIDAD :'
@ 2, 0 SAY ' MONTO TOTAL    :'
@ 1, 22 GET tmp_totcan PICTURE  ;
  '99,999,999' DISABLE COLOR  ;
  SCHEME 2
@ 2, 22 GET tmp_totpre PICTURE  ;
  '999,999.99'
READ
IF LASTKEY() <> 27
     IF wrk_sw4 = 0
          DO WHILE LASTKEY()<>27  ;
             .AND. LASTKEY()<>-1
               DO p_footer WITH  ;
                  '110001000000000000000',  ;
                  2
               = INKEY(0, 'H')
               IF LASTKEY() = 27
                    wrk_swfin = 1
                    wrk_sw4 = 1
                    RELEASE WINDOW  ;
                            tot
                    DO cartec
               ELSE
                    IF LASTKEY() = - ;
                       6
                         = imprime()
                    ENDIF
               ENDIF
          ENDDO
          IF wrk_swfin = 0
               SELECT gc_hip00
               SEEK a1(1) + a1(2)
               IF  .NOT. FOUND()
                    APPEND BLANK
                    DO rbloquea
                    REPLACE hip_tipdoc  ;
                            WITH  ;
                            a1(1),  ;
                            hip_nrodoc  ;
                            WITH  ;
                            a1(2)
                    UNLOCK
               ENDIF
               DO rbloquea
               REPLACE hip_fecdoc  ;
                       WITH a1(3),  ;
                       hip_totnet  ;
                       WITH  ;
                       tmp_totpre,  ;
                       hip_totigv  ;
                       WITH  ;
                       ROUND(tmp_totpre *  ;
                       wrk_facigv,  ;
                       2),  ;
                       hip_totoim  ;
                       WITH  ;
                       tmp_totpre +  ;
                       hip_totigv,  ;
                       hip_totgen  ;
                       WITH  ;
                       hip_totoim
               REPLACE hip_mondoc  ;
                       WITH 'DOL',  ;
                       hip_almdes  ;
                       WITH a1(7),  ;
                       hip_cencon  ;
                       WITH a1(8),  ;
                       hip_estdoc  ;
                       WITH 'C',  ;
                       hip_mondoc  ;
                       WITH  ;
                       rge_monbas,  ;
                       hip_fecmon  ;
                       WITH  ;
                       a1(3)
               REPLACE hip_tipent  ;
                       WITH a1(9),  ;
                       hip_codent  ;
                       WITH  ;
                       a1(10),  ;
                       hip_tidore  ;
                       WITH  ;
                       a1(13),  ;
                       hip_nrdore  ;
                       WITH  ;
                       a1(14),  ;
                       hip_almdes  ;
                       WITH  ;
                       a1(7)
               REPLACE hip_usuari  ;
                       WITH clave,  ;
                       hip_fecha  ;
                       WITH  ;
                       DATE(),  ;
                       hip_hora  ;
                       WITH  ;
                       TIME(),  ;
                       hip_codmov  ;
                       WITH 'COI',  ;
                       hip_estdoc  ;
                       WITH 'C'
               UNLOCK
               wrk_creo = 1
               DEACTIVATE WINDOW  ;
                          titpass,  ;
                          pidpass,  ;
                          a0,  ;
                          titposs,  ;
                          pidposs
               DO p_footer WITH  ;
                  '100010000000000000000',  ;
                  2
               SELECT consu
               GOTO TOP
               SCAN WHILE  .NOT.  ;
                    EOF()
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
                            consu.codigo
                    REPLACE dip_unimed  ;
                            WITH  ;
                            consu.unidad
                    REPLACE dip_cantid  ;
                            WITH  ;
                            consu.cantidad
                    REPLACE dip_pordes  ;
                            WITH  ;
                            0
                    REPLACE dip_import  ;
                            WITH  ;
                            consu.importe
                    REPLACE dip_total  ;
                            WITH  ;
                            consu.total
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
                    SELECT gc_pro00
                    SEEK consu.codigo
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
                         wrk_coulco =  ;
                          pro_ulcomb
                    ELSE
                         wrk_proant =  ;
                          0
                         wrk_proanb =  ;
                          0
                         wrk_coulco =  ;
                          0
                    ENDIF
                    SELECT gc_alm00
                    SEEK consu.codigo +  ;
                         a1(7)
                    IF FOUND()
                         wrk_almant =  ;
                          alm_stkfis
                    ELSE
                         wrk_almant =  ;
                          0
                    ENDIF
                    SELECT gc_kar00
                    APPEND BLANK
                    DO rbloquea
                    REPLACE kar_codpro  ;
                            WITH  ;
                            consu.codigo
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
                            a1(5)
                    REPLACE kar_unimed  ;
                            WITH  ;
                            consu.unidad
                    REPLACE kar_codmon  ;
                            WITH  ;
                            'DOL'
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
                    REPLACE kar_almdes  ;
                            WITH  ;
                            a1(7)
                    REPLACE kar_stkant  ;
                            WITH  ;
                            wrk_almant
                    REPLACE kar_cantid  ;
                            WITH  ;
                            consu.cantidad
                    REPLACE kar_cosant  ;
                            WITH  ;
                            wrk_proant
                    REPLACE kar_cosanb  ;
                            WITH  ;
                            wrk_proanb
                    REPLACE kar_cosuni  ;
                            WITH  ;
                            wrk_coulco
                    REPLACE kar_import  ;
                            WITH  ;
                            ooprecio(consu.codigo, ;
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
                    SELECT consu
               ENDSCAN
               SELECT consu
               GOTO TOP
               SCAN WHILE  .NOT.  ;
                    EOF()
                    SELECT gc_alm00
                    SEEK consu.codigo +  ;
                         a1(7)
                    IF FOUND()
                         DO rbloquea
                         REPLACE alm_stkres  ;
                                 WITH  ;
                                 (alm_stkres -  ;
                                 consu.cantidad)
                         REPLACE alm_stkfis  ;
                                 WITH  ;
                                 (alm_stkfis -  ;
                                 (consu.cantidad *  ;
                                 fac_uni))
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
                    SELECT consu
               ENDSCAN
               SELECT gc_hip00
               DIMENSION a1(  ;
                         FCOUNT())
               SCATTER BLANK TO  ;
                       a1
               ON KEY
               RELEASE WINDOW tot
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
     RELEASE WINDOW tot
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
PROCEDURE pinta1
ACTIVATE WINDOW a0
IF wrk_sw4 = 1 .OR. LASTKEY() =  ;
   27 .OR. wrk_creo = 1
     FOR x = 1 TO 3
          SIZE WINDOW a0 BY 1, 0
     ENDFOR
ENDIF
@ 00, 00 CLEAR TO 08, 74
@ 00, 00 SAY 'Tipo Doc.. :'
@ 00, 32 SAY 'Nro. Doc.   :'
@ 01, 00 SAY 'Fecha..... :'
@ 02, 00 SAY 'Cod. Mov.  :'
@ 02, 32 SAY 'Cen. Cons.  :'
@ 03, 00 SAY 'Alm. Desp. :'
@ 04, 00 SAY 'Tip. Ent.  :'
@ 04, 32 SAY 'Cod. Ent... :'
@ 05, 00 SAY 'Tip. Doc. Ref. :'
@ 05, 32 SAY 'Nro. Doc. Ref. :'
RETURN
*
PROCEDURE imprime
ON KEY
DO p_mensaje WITH  ;
   'Impresi¢n en proceso. Espere......'
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
     DO imprimir
ENDIF
RETURN
*
PROCEDURE imprimir
SELECT consu
SET RELATION TO codigo INTO gc_pro00
SET PRINTER ON
??? CHR(27) + CHR(80)
REPORT FORMAT AGCP2225 TO PRINTER  ;
       NOCONSOLE
SET PRINTER OFF
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
CREATE CURSOR CONSU (codigo C  ;
       (14), unidad C (4),  ;
       descrip C (17), cantidad N  ;
       (9, 2), importe N (9, 2),  ;
       total N (9, 2))
wrk_idx = f_indice()
INDEX ON CODIGO+UNIDAD TO &WRK_IDX
COUNT TO wrk_count
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
ON KEY LABEL F3 DO P_LEE    WITH 1
SELECT consu
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
PROCEDURE transito_1
STORE a2(4) TO tran_ant
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
PROCEDURE facigv
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     wrk_facigv = tab_factor /  ;
                  100
ENDIF
RETURN
*
FUNCTION oodescli
IF a1(09) = 'P' .OR. a1(09) = 'C'
     SELECT gc_cli00
ELSE
     SELECT gc_vnd00
ENDIF
SEEK ALLTRIM(a1(09)) + a1(10)
IF  .NOT. FOUND()
     nombre = 'No Existe'
ELSE
     IF a1(09) = 'P' .OR. a1(09) =  ;
        'C'
          nombre = SUBSTR(cli_razsoc,  ;
                   1, 20)
     ELSE
          nombre = SUBSTR(vnd_nombre,  ;
                   1, 20)
     ENDIF
ENDIF
RETURN nombre
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
DEFINE POPUP produ FROM 15, 18 TO  ;
       20, 73 PROMPT FIELDS  ;
       pro_codpro + '³' +  ;
       SUBSTR(pro_descri, 1, 20) +  ;
       '³' + SUBSTR(pro_modelo, 1,  ;
       20) + '³' + pro_subcat IN  ;
       screen COLOR SCHEME 8
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
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
SET FILTER TO alm_codalm = a1(7)
SELECT gc_pro00
SET RELATION TO pro_codpro INTO gc_alm00
IF LASTKEY() <> 27
     SET NEAR ON
     SEEK w_codpro
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
            'Reser', pro_subcat  ;
            :R :H = 'Sub-Cat' IN  ;
            produ
ENDIF
ON KEY
SET FILTER TO
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
SET NEAR OFF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

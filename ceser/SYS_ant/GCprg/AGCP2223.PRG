*-------------------------------------
* Agcp2223.Prg
* Ajustes y Mermas
*-------------------------------------
ON KEY
CLOSE DATABASES
DEFINE WINDOW t0 FROM 02, 64 TO 02, 73 IN screen NONE
DEFINE WINDOW a0 FROM 04, 03 TO 11, 76 IN screen
DEFINE WINDOW titposs FROM 10, 03 TO 12, 76 IN screen NONE COLOR SCHEME 08
DEFINE WINDOW pidposs FROM 19, 03 TO 21, 76 IN screen NONE COLOR SCHEME 20
ACTIVATE WINDOW tablas
flag = .F.
DO p_prestab WITH 'PROCESO', 'AJUSTES Y MERMAS', 'SELECCION', flag
@ 02, 63 SAY PROGRAM()
SELECT 1
USE SHARED GC_HIP00 ORDER CODIGO
SELECT 2
USE SHARED GC_DIP00 ORDER CODIGO
SELECT 3
USE SHARED GE_TAB0 ORDER CODIGO
wrk_sele1 = SELECT()
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
USE SHARED GC_CMV00 ORDER cmv_feinmo
SELECT 11
USE SHARED GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 12
USE SHARED GC_DLP00 ORDER CODIGO
STORE 0 TO wrk_termin, wrk_facigv, w_tipcam
= facigv()
IF wrk_facigv = 0
     DO p_mensaje WITH 'No hay impuesto a las ventas'
     wrk_termin = 1
ENDIF
w_tipcam = ootc2(DATE(),'SOL ', 'DOL ','2')
IF w_tipcam = -1 .OR. w_tipcam = 0
     DO p_mensaje WITH 'No hay tipo de cambio'
     wrk_termin = 1
ENDIF
DO WHILE wrk_termin=0
     STORE 0 TO tmp_1, wrk_creo, wrk_sw2, wrk_sw3, wrk_totcan, tmp_totcan, tmp_totpre, dif_totcan, wrk_pos, wrk_x, wrk_mone1, wrk_prec, wrk_almx, wrk_alm, wrk_alma, wrk_lin, wrk_totdip, wrk_cambix, wrk_stkant, wrk_bus2, wrk_bus1, fac_mon, fac_uni, wrk_count, wrk_mal
     STORE 1 TO wrk_sw4, wrk_swfin, wrk_sel1
     DIMENSION cod( 1), doc( 1), can( 1), des( 1), facuni( 1), alm1( 1), alm2( 1), valor(1), factor2(1), alma(1), pre[1]
     SCATTER BLANK TO cod
     STORE SPACE(30) TO alm1(1)
     STORE 0 TO can[ 1], valor[1], factor2[ 1], pre[1], wrk_flag, tran_ant
     STORE SPACE(10) TO doc[ 1]
     ACTIVATE WINDOW t0
     @ 00, 00 SAY 'SELECCIONA'
     SET CURSOR ON
     SELECT gc_hip00
     DIMENSION a1( FCOUNT())
     SCATTER BLANK TO a1
     DO proceso
ENDDO
DEACTIVATE WINDOW tablas, titpass, a0, titposs
RELEASE WINDOW t0, a0, a1, c0, a2, a3
ON KEY LABEL F6
CLOSE DATABASES
DO p_footer WITH '100000000001011000001', 1
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
          a1( 01) = 'AJUS'
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
               IF a1(01) <>  ;
                  'AJUS'
                    DO p_mensaje  ;
                       WITH  ;
                       'Tipo de Documento no V lido'
                    LOOP
               ELSE
                    @ 00, 18 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 12)
               ENDIF
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
               wrk_busca = 'AJUS'
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
                      tab_destab
               ENDIF
               DO WHILE .T. .AND.  ;
                  tmp_1=0
                    IF wrk_sw2 =  ;
                       0 .OR.  ;
                       wrk_x = 0
                         ON KEY
                         IF a1(05) =  ;
                            'EMER'  ;
                            .OR.  ;
                            a1(05) =  ;
                            'AFNE'
                              wrk_busca =  ;
                               'ALMA'
                              wrk_var =  ;
                               'A1(07)'
                              a1(  ;
                                07) =  ;
                                rge_codalm
                              ON KEY LABEL;
F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                              @ 03,  ;
                                13  ;
                                GET  ;
                                a1(  ;
                                07)  ;
                                PICTURE  ;
                                '@!'
                         ELSE
                              wrk_busca =  ;
                               'ALMA'
                              wrk_var =  ;
                               'A1(06)'
                              a1(  ;
                                06) =  ;
                                rge_codalm
                              ON KEY LABEL;
F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                              @ 03,  ;
                                13  ;
                                GET  ;
                                a1(  ;
                                06)  ;
                                PICTURE  ;
                                '@!'
                         ENDIF
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
                              tmp_1 =  ;
                               1
                              EXIT
                         ENDIF
                         SELECT ge_tab0
                         IF a1(05) =  ;
                            'EMER'  ;
                            .OR.  ;
                            a1(05) =  ;
                            'AFNE'
                              SEEK  ;
                               'ALMA' +  ;
                               a1(07)
                         ELSE
                              SEEK  ;
                               'ALMA' +  ;
                               a1(06)
                         ENDIF
                         IF  .NOT.  ;
                             FOUND()
                              DO p_mensaje  ;
                                 WITH  ;
                                 'Almac‚n no Existe'
                              LOOP
                         ELSE
                              @ 03,  ;
                                18  ;
                                SAY  ;
                                SUBSTR(tab_destab,  ;
                                1,  ;
                                13)
                         ENDIF
                    ENDIF
                    DO WHILE .T.  ;
                       .AND.  ;
                       tmp_1=0
                         ON KEY
                         a1( 09) =  ;
                           'A   '
                         wrk_busca =  ;
                          'ENTI'
                         wrk_var =  ;
                          'A1(09)'
                         ON KEY LABEL;
F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                         @ 04, 13  ;
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
                              tmp_1 =  ;
                               1
                              EXIT
                         ENDIF
                         SELECT ge_tab0
                         SEEK 'ENTI' +  ;
                              a1(09)
                         IF  .NOT.  ;
                             FOUND()
                              DO p_mensaje  ;
                                 WITH  ;
                                 'No Existe'
                              LOOP
                         ELSE
                              @ 04,  ;
                                18  ;
                                SAY  ;
                                SUBSTR(tab_destab,  ;
                                1,  ;
                                13)
                         ENDIF
                         DO WHILE  ;
                            .T.  ;
                            .AND.  ;
                            tmp_1= ;
                            0
                              ON KEY
                              wrk_busca =  ;
                               a1(09)
                              wrk_var =  ;
                               'A1(10)'
                              ON KEY LABEL;
F6 DO ENTIDAD WITH WRK_BUSCA,WRK_VAR
                              @ 04,  ;
                                46  ;
                                GET  ;
                                a1(  ;
                                10)  ;
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
                              IF a1(09) =  ;
                                 'P'  ;
                                 .OR.  ;
                                 a1(09) =  ;
                                 'C'
                                   SELECT gc_cli00
                              ELSE
                                   SELECT gc_vnd00
                              ENDIF
                              SEEK  ;
                               ALLTRIM(a1(09)) +  ;
                               a1(10)
                              IF   ;
                               .NOT.  ;
                               FOUND()
                                   IF a1(09) = 'P' .OR. a1(09) = 'C'
                                        DO p_mensaje WITH 'Proveedor o Cliente no Existe'
                                        LOOP
                                   ELSE
                                        DO p_mensaje WITH 'Administrativo o Vendedor no Existe'
                                        LOOP
                                   ENDIF
                              ELSE
                                   IF a1(09) = 'P' .OR. a1(09) = 'C'
                                        @ 04, 58 SAY SUBSTR(cli_razsoc, 1, 13)
                                   ELSE
                                        @ 04, 58 SAY SUBSTR(vnd_nombre, 1, 13)
                                   ENDIF
                              ENDIF
                              DO WHILE  ;
                                 .T.  ;
                                 .AND.  ;
                                 tmp_1= ;
                                 0
                                   ON KEY
                                   wrk_busca = 'MONE'
                                   wrk_var = 'A1(11)'
                                   a1( 11) = 'DOL '
                                   ON KEY LABEL F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                                   @ 05, 13 GET a1( 11) PICTURE '@!'
                                   READ
                                   IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                        EXIT
                                   ENDIF
                                   IF LASTKEY() = 27
                                        tmp_1 = 1
                                        EXIT
                                   ENDIF
                                   SELECT ge_tab0
                                   SEEK 'MONE' + a1(11)
                                   IF  .NOT. FOUND()
                                        DO p_mensaje WITH 'Moneda no Existe'
                                        LOOP
                                   ELSE
                                        @ 05, 18 SAY SUBSTR(tab_destab, 1, 13)
                                   ENDIF
                                   DO WHILE .T. .AND. tmp_1=0
                                        DO p_footer WITH '100000000000000000001', 2
                                        ACTIVATE WINDOW a0
                                        ON KEY
                                        IF wrk_sw2 = 0
                                             a1( 12) = DATE()
                                        ENDIF
                                        @ 05, 46 GET a1( 12) PICTURE '99/99/99'
                                        READ
                                        IF LASTKEY() = 27
                                             tmp_1 = 1
                                             EXIT
                                        ENDIF
                                        IF a1(12) <> DATE()
                                             w_tipcam = ootc2(a1(12),'SOL ','DOL ','2')
                                             IF w_tipcam = -1 .OR. w_tipcam = 0
                                                  DO p_mensaje WITH 'No hay tipo de cambio'
                                                  LOOP
                                             ENDIF
                                        ENDIF
                                        IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                             EXIT
                                        ENDIF
                                        tmp_1 = 1
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
ACTIVATE WINDOW titposs
IF SUBSTR(a1(5), 2, 1) = 'V'
     DEFINE POPUP detalle FROM 13,  ;
            03 TO 18, 76 PROMPT  ;
            FIELDS codigo + '³' +  ;
            unidad + '³' +  ;
            SUBSTR(descrip, 1,  ;
            20) + '³  ' + docref +  ;
            '  ³ ' +  ;
            TRANSFORM(importe,  ;
            '@ 999,999.99') IN  ;
            screen COLOR SCHEME  ;
            9
     ON SELECTION POPUP detalle DEAC POPUP;
DETALLE
     @ 00, 00 SAY  ;
       'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
     @ 01, 00 SAY  ;
       '³   PRODUCTO   ³UNID³  DESCRIPCION       ³DOC.REFERENCIA³    PROMEDIO    ³'
     @ 02, 00 SAY  ;
       'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
ELSE
     DEFINE POPUP detalle FROM 13,  ;
            03 TO 18, 76 PROMPT  ;
            FIELDS codigo + '³' +  ;
            unidad + '³' +  ;
            SUBSTR(descrip, 1,  ;
            20) + '³  ' + docref +  ;
            '  ³ ' +  ;
            TRANSFORM(cantidad,  ;
            '@ 999,999') IN  ;
            screen COLOR SCHEME  ;
            9
     ON SELECTION POPUP detalle DEAC POPUP;
DETALLE
     @ 00, 00 SAY  ;
       'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
     @ 01, 00 SAY  ;
       '³   PRODUCTO   ³UNID³  DESCRIPCION       ³DOC.REFERENCIA³    CANTIDAD    ³'
     @ 02, 00 SAY  ;
       'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
ENDIF
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
               SELECT ajust
               ACTIVATE POPUP  ;
                        detalle
          ENDDO
          wrk_swfin = 1
     ENDDO
     IF LASTKEY() = 27
          IF a1(05) = 'EMER' .OR.  ;
             a1(05) = 'AFNE'
               SELECT ajust
               COUNT TO wrk_total
               GOTO TOP
               IF wrk_total > 0
                    SCAN WHILE   ;
                         .NOT.  ;
                         EOF()
                         SELECT gc_alm00
                         SEEK ajust.codigo +  ;
                              a1(7)
                         IF FOUND()
                              DO rbloquea
                              REPLACE  ;
                               alm_stkres  ;
                               WITH  ;
                               alm_stkres -  ;
                               ajust.cantidad
                              REPLACE  ;
                               alm_usuari  ;
                               WITH  ;
                               clave
                              REPLACE  ;
                               alm_fecha  ;
                               WITH  ;
                               DATE()
                              REPLACE  ;
                               alm_hora  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                         ENDIF
                         SELECT ajust
                    ENDSCAN
               ENDIF
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
PROCEDURE p_lee
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
       'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
     @ 01, 00 SAY  ;
       '³              ³    ³                    ³              ³                ³'
     @ 02, 00 SAY  ;
       'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
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
          @ 01, 44 SAY a2(6)  ;
            PICTURE '@!'
          IF SUBSTR(a1(5), 2, 1) =  ;
             'V'
               @ 01, 58 SAY a2(5)  ;
                 PICTURE  ;
                 '999,999.99'
          ELSE
               @ 01, 58 SAY a2(4)  ;
                 PICTURE  ;
                 '999,999'
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
                       'B' .AND.  ;
                       xnnn <>  ;
                       'A7'
                         DO p_mensaje  ;
                            WITH  ;
                            'PRODUCTO ESTA BLOQUEADO'
                         LOOP
                    ENDIF
                    a2( 2) =  ;
                      pro_unimed
                    a2( 3) =  ;
                      SUBSTR(pro_descri,  ;
                      1, 20)
                    IF SUBSTR(a1(05),  ;
                       2, 1) <>  ;
                       'V'
                         a2( 05) =  ;
                           pro_coprmo
                    ENDIF
                    @ 01, 21 SAY  ;
                      a2(3)
               ENDIF
          ENDIF
          DO WHILE .T. .AND.  ;
             tmp_1=0
               IF aux_opcion = 1
                    wrk_var = 'A2(2)'
                    wrk_busca = a2(1)
                    @ 01, 16 SAY  ;
                      a2(2)
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
                    ON KEY
                    DO p_footer  ;
                       WITH  ;
                       '100000000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             pidposs
                    @ 01, 44 GET  ;
                      a2( 6)  ;
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
                    DO WHILE .T.  ;
                       .AND.  ;
                       tmp_1=0
                         wrk_bus1 =  ;
                          ASCAN(alm1,  ;
                          a2(1))
                         IF wrk_bus1 =  ;
                            0
                              wrk_bus2 =  ;
                               wrk_bus2 +  ;
                               1
                              DIMENSION  ;
                               alm1[  ;
                               wrk_bus2]
                              DIMENSION  ;
                               alm2[  ;
                               wrk_bus2]
                              = AINS(alm1,  ;
                                wrk_bus2)
                              = AINS(alm2,  ;
                                wrk_bus2)
                              alm1[  ;
                               wrk_bus2] =  ;
                               a2(1)
                              alm2[  ;
                               wrk_bus2] =  ;
                               0
                              wrk_bus1 =  ;
                               wrk_bus2
                         ELSE
                              wrk_bus2 =  ;
                               wrk_bus1
                              alm2[  ;
                               wrk_bus2] =  ;
                               alm2(wrk_bus2) -  ;
                               a2(4) *  ;
                               fac_uni
                         ENDIF
                         IF SUBSTR(a1(5),  ;
                            2, 1) =  ;
                            'F'  ;
                            .OR.  ;
                            SUBSTR(a1(5),  ;
                            2, 1) =  ;
                            'M'
                              IF a1(05) =  ;
                                 'EMER'  ;
                                 .OR.  ;
                                 a1(05) =  ;
                                 'AFNE'
                                   wrk_cla1 = a2(1) + a1(7)
                                   @ 01, 58 GET a2( 4) DEFAULT 0 PICTURE '999,999' VALID vali2c() WHEN when2c()
                              ELSE
                                   wrk_cla1 = a2(1) + a1(6)
                                   @ 01, 58 GET a2( 4) DEFAULT 0 PICTURE '999,999' VALID vali2c()
                              ENDIF
                              READ
                              IF LASTKEY() =  ;
                                 27
                                   alm2[ wrk_bus2] = alm2(wrk_bus2) + a2(4) * fac_uni
                                   tmp_1 = 1
                                   EXIT
                              ENDIF
                              IF LASTKEY() =  ;
                                 19  ;
                                 .OR.  ;
                                 LASTKEY() =  ;
                                 5
                                   EXIT
                              ENDIF
                              alm2[  ;
                               wrk_bus1] =  ;
                               alm2(wrk_bus1) +  ;
                               a2(4) *  ;
                               fac_uni
                              tmp_1 =  ;
                               1
                         ELSE
                              @ 01,  ;
                                58  ;
                                GET  ;
                                a2(  ;
                                5)  ;
                                DEFAULT  ;
                                0  ;
                                PICTURE  ;
                                '999,999.99'  ;
                                VALID  ;
                                precio2()  ;
                                WHEN  ;
                                precio1()
                              READ
                              tmp_1 =  ;
                               1
                              = INKEY(0)
                         ENDIF
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
               = AINS(valor,  ;
                 wrk_x)
               = AINS(factor2,  ;
                 wrk_x)
               cod[ wrk_x] = 0
               can[ wrk_x] = 0
               des[ wrk_x] =  ;
                  SPACE(20)
               pre[ wrk_x] = 0
               doc[ wrk_x] =  ;
                  SPACE(10)
               facuni[ wrk_x] = 0
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
          factor2[ wrk_pos] =  ;
                 fac_uni
          valor[ wrk_pos] =  ;
               (pre(wrk_pos) *  ;
               fac_uni)
          SELECT ajust
          IF aux_opcion = 1
               APPEND BLANK
          ENDIF
          DO rbloquea
          REPLACE codigo WITH  ;
                  a2(1)
          REPLACE unidad WITH  ;
                  a2(2)
          REPLACE descrip WITH  ;
                  a2(3)
          REPLACE cantidad WITH  ;
                  a2(4)
          REPLACE importe WITH  ;
                  a2(5)
          REPLACE docref WITH  ;
                  a2(6)
          UNLOCK
          DEACTIVATE WINDOW  ;
                     pidpass,  ;
                     pidposs
     ENDIF
     EXIT
ENDDO
DEACTIVATE WINDOW pidpass,  ;
           pidposs
SELECT ajust
COUNT TO wrk_count
reg_count = wrk_count
DO cartec
RETURN
*
PROCEDURE p_borra
ON KEY
IF a1(05) = 'AFNE' .OR. a1(05) =  ;
   'EMER'
     SELECT gc_alm00
     SEEK ajust.codigo + a1(7)
     IF FOUND()
          DO rbloquea
          REPLACE alm_stkres WITH  ;
                  (alm_stkres -  ;
                  ajust.cantidad)
          UNLOCK
     ENDIF
ENDIF
wrk_pos = ASCAN(cod, ajust.codigo +  ;
          ajust.unidad)
DELETE
IF wrk_pos <> 0
     = ADEL(cod, wrk_pos)
     = ADEL(can, wrk_pos)
     = ADEL(des, wrk_pos)
     = ADEL(pre, wrk_pos)
     = ADEL(doc, wrk_pos)
     = ADEL(facuni, wrk_pos)
     = ADEL(valor, wrk_pos)
     = ADEL(factor2, wrk_pos)
     wrk_x = wrk_x - 1
ENDIF
DO cartec
SELECT ajust
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
PROCEDURE crea
ON KEY
DO p_footer WITH  ;
   '100000000001000000001', 2
STORE 0 TO wrk_sw4, wrk_totcan,  ;
      tmp_totcan, tmp_totpre,  ;
      dif_totcan, wrk_total
DEFINE WINDOW tot FROM 17, 15 TO  ;
       21, 65 IN screen
ACTIVATE WINDOW tot
@ 00, 00 SAY  ;
  '                                    DIFERENCIA'
SELECT ajust
GOTO TOP
SCAN WHILE  .NOT. EOF()
     tmp_totcan = tmp_totcan +  ;
                  cantidad
     IF SUBSTR(a1(5), 2, 1) = 'V'
          tmp_totpre = tmp_totpre +  ;
                       importe
     ELSE
          tmp_totpre = tmp_totpre +  ;
                       (importe *  ;
                       cantidad)
     ENDIF
ENDSCAN
wrk_totcan = tmp_totcan
@ 01, 00 SAY ' TOTAL CANTIDAD  :'
@ 01, 22 GET wrk_totcan PICTURE  ;
  '999,999'
@ 02, 00 SAY ' TOTAL COSTO PROM:'
@ 02, 22 SAY tmp_totpre PICTURE  ;
  '999,999.99'
READ
IF LASTKEY() <> 27
     dif_totcan = tmp_totcan -  ;
                  wrk_totcan
     IF dif_totcan <> 0
          wrk_sw4 = 1
     ENDIF
     @ 01, 33 SAY dif_totcan  ;
       PICTURE '999,999'
     IF wrk_sw4 = 0
          DO WHILE LASTKEY()<>27  ;
             .AND. LASTKEY()<>-1
               DO p_footer WITH  ;
                  '110001000000000000001',  ;
                  2
               = INKEY(0, 'H')
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
               IF SUBSTR(a1(5), 2,  ;
                  1) = 'V'
                    DO ajupro
               ENDIF
               SELECT ajust
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
                    IF a1(11) =  ;
                       'DOL'
                         REPLACE dip_import  ;
                                 WITH  ;
                                 m.importe
                         REPLACE dip_total  ;
                                 WITH  ;
                                 dip_import *  ;
                                 dip_cantid
                    ELSE
                         REPLACE dip_solpre  ;
                                 WITH  ;
                                 m.importe *  ;
                                 w_tipcam
                         REPLACE dip_soltot  ;
                                 WITH  ;
                                 (dip_import *  ;
                                 dip_cantid) *  ;
                                 w_tipcam
                    ENDIF
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
                    wrk_total = ROUND(wrk_total +  ;
                                dip_total,  ;
                                2)
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
                    REPLACE kar_codmov  ;
                            WITH  ;
                            a1(5)
                    REPLACE kar_codmon  ;
                            WITH  ;
                            a1(11)
                    REPLACE kar_unimed  ;
                            WITH  ;
                            m.unidad
                    REPLACE kar_tipent  ;
                            WITH  ;
                            a1(09)
                    REPLACE kar_codent  ;
                            WITH  ;
                            a1(10)
                    UNLOCK
                    IF a1(05) =  ;
                       'EMER'  ;
                       .OR.  ;
                       a1(05) =  ;
                       'AFNE'
                         wrk_cla2 =  ;
                          m.codigo +  ;
                          a1(7)
                    ELSE
                         wrk_cla2 =  ;
                          m.codigo +  ;
                          a1(6)
                    ENDIF
                    SELECT gc_alm00
                    SEEK wrk_cla2
                    IF FOUND()
                         wrk_almant =  ;
                          alm_stkfis
                    ELSE
                         APPEND BLANK
                         DO rbloquea
                         wrk_almant =  ;
                          0
                         REPLACE alm_codpro  ;
                                 WITH  ;
                                 m.codigo
                         IF a1(05) =  ;
                            'EMER'  ;
                            .OR.  ;
                            a1(05) =  ;
                            'AFNE'
                              REPLACE  ;
                               alm_codalm  ;
                               WITH  ;
                               a1(07)
                         ELSE
                              REPLACE  ;
                               alm_codalm  ;
                               WITH  ;
                               a1(06)
                         ENDIF
                    ENDIF
                    SELECT gc_kar00
                    DO rbloquea
                    REPLACE kar_stkant  ;
                            WITH  ;
                            wrk_almant
                    REPLACE kar_cantid  ;
                            WITH  ;
                            m.cantidad
                    UNLOCK
                    SELECT gc_alm00
                    SEEK wrk_cla2
                    IF FOUND()
                         DO rbloquea
                         IF a1(5) =  ;
                            'AFPO'
                              REPLACE  ;
                               alm_stkfis  ;
                               WITH  ;
                               alm_stkfis +  ;
                               m.cantidad
                              REPLACE  ;
                               alm_fecha  ;
                               WITH  ;
                               DATE(),  ;
                               alm_hora  ;
                               WITH  ;
                               TIME(),  ;
                               alm_usuari  ;
                               WITH  ;
                               clave
                              UNLOCK
                         ENDIF
                         IF a1(5) =  ;
                            'AFNE'  ;
                            .OR.  ;
                            a1(5) =  ;
                            'EMER'
                              REPLACE  ;
                               alm_stkfis  ;
                               WITH  ;
                               alm_stkfis -  ;
                               m.cantidad
                              REPLACE  ;
                               alm_stkres  ;
                               WITH  ;
                               alm_stkres -  ;
                               m.cantidad
                              REPLACE  ;
                               alm_fecha  ;
                               WITH  ;
                               DATE(),  ;
                               alm_hora  ;
                               WITH  ;
                               TIME(),  ;
                               alm_usuari  ;
                               WITH  ;
                               clave
                              UNLOCK
                         ENDIF
                    ENDIF
                    SELECT gc_pro00
                    SEEK m.codigo
                    IF FOUND()
                         DO rbloquea
                         REPLACE pro_ultmov  ;
                                 WITH  ;
                                 DATE()
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
                    IF a1(05) =  ;
                       'EMER'  ;
                       .OR.  ;
                       a1(05) =  ;
                       'AFNE'
                         REPLACE kar_almdes  ;
                                 WITH  ;
                                 a1(7)
                    ELSE
                         REPLACE kar_almrec  ;
                                 WITH  ;
                                 a1(6)
                    ENDIF
                    IF a1(05) =  ;
                       'AVPO'  ;
                       .OR.  ;
                       a1(05) =  ;
                       'AVNE'
                         REPLACE kar_import  ;
                                 WITH  ;
                                 m.importe
                    ELSE
                         REPLACE kar_import  ;
                                 WITH  ;
                                 ooprecio(m.codigo, ;
                                 1)
                    ENDIF
                    REPLACE kar_nrdore  ;
                            WITH  ;
                            m.docref
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
                    SELECT ajust
               ENDSCAN
               a1( 26) = '001'
               a1( 28) = 'C'
               a1( 12) = DATE()
               a1( 15) =  ;
                 ROUND(wrk_total,  ;
                 2)
               a1( 17) =  ;
                 ROUND(a1(15) *  ;
                 wrk_facigv, 2)
               a1( 16) = a1(17) -  ;
                 a1(15)
               a1( 20) = a1(17)
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
               wrk_swfin = 1
          ENDIF
     ELSE
          = INKEY(2)
          DEACTIVATE WINDOW tot
          SELECT (wrk_sel1)
          DO p_footer WITH  ;
             '111101000000101000101',  ;
             2
          ON KEY LABEL ENTER DO P_LEE;
   WITH 2
          ON KEY LABEL F2 DO CREA 
          ON KEY LABEL F3 DO P_LEE;
   WITH 1
          ON KEY LABEL F4 DO P_BORRA 
     ENDIF
ELSE
     DEACTIVATE WINDOW tot
     DO p_footer WITH  ;
        '111101000000101000101',  ;
        2
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
DEACTIVATE WINDOW titposs
ACTIVATE WINDOW a0
@ 00, 00 CLEAR TO 08, 74
@ 00, 00 SAY 'Tipo Doc.  :'
@ 00, 32 SAY 'Nro. Doc.   :'
@ 01, 00 SAY 'Fecha      :'
@ 02, 00 SAY 'Cod. Mov.  :'
@ 03, 00 SAY 'Cod. Alm.  :'
@ 04, 00 SAY 'Tip. Ent.  :'
@ 04, 32 SAY 'Cod. Ent.   :'
@ 05, 00 SAY 'Cod. Moneda:'
@ 05, 32 SAY 'Fecha T.C.  :'
RETURN
*
PROCEDURE carmat
ON KEY
m = 0
SELECT ajust
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
               SET CURSOR OFF
               ERASE &TMP_PEP  
               RETURN
          ENDIF
     ENDDO
     SET DEVICE TO PRINTER
     DO imprimir
ENDIF
ERASE &TMP_PEP  
RETURN
*
PROCEDURE imprimir
DO r_titulo
aux_lin = aux_lin + 1
SELECT ajust
GOTO TOP
DO WHILE  .NOT. EOF()
     SCATTER MEMVAR
     IF aux_lin > 60
          DO r_titulo
     ENDIF
     aux_lin = aux_lin + 1
     @ aux_lin, 02 SAY m.codigo
     @ aux_lin, 19 SAY m.unidad
     SELECT gc_pro00
     SEEK m.codigo
     @ aux_lin, 26 SAY pro_descri
     SELECT ajust
     @ aux_lin, 70 SAY m.docref
     IF SUBSTR(a1(5), 2, 1) = 'V'
          @ aux_lin, 90 SAY  ;
            m.importe PICTURE  ;
            '999,999,999.99'
     ELSE
          @ aux_lin, 90 SAY  ;
            m.cantidad PICTURE  ;
            '999,999,999'
     ENDIF
     IF aux_lin > 53
          DO r_titulo
     ENDIF
     SKIP
ENDDO
IF SUBSTR(a1(5), 2, 1) = 'V'
     aux_lin = aux_lin + 1
     @ aux_lin, 90 SAY  ;
       'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ'
     aux_lin = aux_lin + 1
     @ aux_lin, 83 SAY 'TOTAL :'
     @ aux_lin, 90 SAY tmp_totpre  ;
       PICTURE '999,999,999.99'
ELSE
     aux_lin = aux_lin + 1
     @ aux_lin, 90 SAY  ;
       'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ'
     aux_lin = aux_lin + 1
     @ aux_lin, 83 SAY 'TOTAL :'
     @ aux_lin, 90 SAY tmp_totcan  ;
       PICTURE '999,999,999'
ENDIF
@ aux_lin, 00 SAY CHR(27) + 'F'
@ aux_lin, 00 SAY CHR(18)
@ 56, 00 SAY  ;
  'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 57, 00 SAY  ;
  '³    OBSERVACIONES    ³  Vo.Bo. Almac‚n  ³  Vo.Bo. Ventas ³ Vo.Bo.Administrci¢n³'
@ 58, 00 SAY  ;
  'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´'
@ 59, 00 SAY  ;
  '³                     ³                  ³                ³                    ³'
@ 60, 00 SAY  ;
  'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
??? CHR(27) + 'P'
EJECT
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE r_titulo
@ 00, 00 SAY CHR(27) + 'U' + '1'
@ 00, 00 SAY CHR(27) + 'M'
@ 00, 00 SAY CHR(14)
@ 00, 00 SAY CHR(27) + 'M'
@ 00, 00 SAY rge_razsoc
@ 01, 00 SAY CHR(14)
@ 01, 00 SAY rge_abrev
@ 04, 00 SAY CHR(14)
wrk_busca = 'AJUS'
@ 04, 10 SAY 'AJUSTES Y MERMAS'
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
@ 07, 00 SAY '³FECHA         :'
@ 07, 20 SAY a1(3)
@ 07, 79 SAY '³'
@ 08, 00 SAY  ;
  '³MOVIMIENTO       :'
@ 08, 20 SAY a1(05)
SELECT ge_tab0
SEEK wrk_busca + a1(05)
@ 08, 25 SAY tab_destab
@ 08, 79 SAY '³'
@ 09, 00 SAY  ;
  '³ALM. DE RECEP.   :'
IF a1(05) = 'EMER' .OR. a1(05) =  ;
   'AFNE'
     @ 09, 20 SAY a1(07)
ELSE
     @ 09, 20 SAY a1(06)
ENDIF
SELECT ge_tab0
IF a1(05) = 'EMER' .OR. a1(05) =  ;
   'AFNE'
     SEEK 'ALMA' + a1(07)
ELSE
     SEEK 'ALMA' + a1(06)
ENDIF
@ 09, 25 SAY SUBSTR(tab_destab, 1,  ;
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
IF a1(09) = 'P' .OR. a1(09) = 'C'
     SELECT gc_cli00
ELSE
     SELECT gc_vnd00
ENDIF
SEEK ALLTRIM(a1(09)) +  ;
     ALLTRIM(a1(10))
IF  .NOT. FOUND()
     @ 10, 63 SAY 'No Existe'
ELSE
     IF a1(09) = 'P' .OR. a1(09) =  ;
        'C'
          @ 10, 62 SAY  ;
            SUBSTR(cli_razsoc, 1,  ;
            17)
     ELSE
          @ 10, 62 SAY  ;
            SUBSTR(vnd_nombre, 1,  ;
            17)
     ENDIF
ENDIF
@ 10, 79 SAY '³'
@ 11, 00 SAY  ;
  'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
@ 12, 00 SAY CHR(27) + 'F'
@ 12, 00 SAY CHR(15)
IF SUBSTR(a1(5), 2, 1) = 'V'
     @ 12, 00 SAY  ;
       'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
     @ 13, 00 SAY  ;
       '³   PRODUCTO     ³ UNI. ³       D  E  S  C  R  I  P  C  I  O  N    ³  DOC.REFERENCIA   ³    P R O M E D I O    ³'
     @ 14, 00 SAY  ;
       'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
ELSE
     @ 12, 00 SAY  ;
       'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
     @ 13, 00 SAY  ;
       '³   PRODUCTO     ³ UNI. ³       D  E  S  C  R  I  P  C  I  O  N    ³  DOC.REFERENCIA   ³     C A N T I D A D   ³'
     @ 14, 00 SAY  ;
       'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
ENDIF
aux_lin = 15
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
     CREATE CURSOR AJUST (codigo  ;
            C (14), unidad C (4),  ;
            descrip C (20),  ;
            cantidad N (9, 2),  ;
            importe N (9, 2),  ;
            docref C (10))
     DIMENSION a2( FCOUNT())
ELSE
     SELECT ajust
     DIMENSION a2( FCOUNT() + 1)
ENDIF
SCATTER BLANK TO a2
DO cartec
COUNT TO wrk_count
reg_count = wrk_count
RETURN
*
PROCEDURE cartec
ON KEY
IF RECCOUNT() = 0
     DO p_footer WITH  ;
        '101010000000000000101',  ;
        2
ELSE
     DO p_footer WITH  ;
        '111100000000101000101',  ;
        2
     ON KEY LABEL ENTER DO P_LEE;
   WITH 2
     ON KEY LABEL F2 DO CREA 
     ON KEY LABEL F4 DO P_BORRA 
ENDIF
ON KEY LABEL F3 DO P_LEE    WITH 1
RETURN
*
PROCEDURE ajupro
ON KEY
m = 0
FOR m = 1 TO wrk_x
     wrk_pos = ASCAN(cod, cod(m))
     IF cod(m) <> '*'
          SELECT gc_pro00
          SEEK SUBSTR(cod(wrk_pos),  ;
               1, 14)
          IF FOUND()
               DO rbloquea
               IF a1(5) = 'AVPO'
                    IF a1(11) =  ;
                       'DOL'
                         REPLACE pro_coprmo  ;
                                 WITH  ;
                                 pro_coprmo +  ;
                                 valor(wrk_pos)
                         REPLACE pro_coprmb  ;
                                 WITH  ;
                                 pro_coprmb +  ;
                                 (valor(wrk_pos) *  ;
                                 w_tipcam)
                    ELSE
                         REPLACE pro_coprmo  ;
                                 WITH  ;
                                 pro_coprmo +  ;
                                 (valor(wrk_pos) /  ;
                                 w_tipcam)
                         REPLACE pro_coprmb  ;
                                 WITH  ;
                                 pro_coprmb +  ;
                                 valor(wrk_pos)
                    ENDIF
               ELSE
                    IF a1(11) =  ;
                       'DOL'
                         REPLACE pro_coprmo  ;
                                 WITH  ;
                                 pro_coprmo -  ;
                                 valor(wrk_pos)
                         REPLACE pro_coprmb  ;
                                 WITH  ;
                                 pro_coprmb -  ;
                                 (valor(wrk_pos) *  ;
                                 w_tipcam)
                    ELSE
                         REPLACE pro_coprmo  ;
                                 WITH  ;
                                 pro_coprmo -  ;
                                 (valor(wrk_pos) /  ;
                                 w_tipcam)
                         REPLACE pro_coprmb  ;
                                 WITH  ;
                                 pro_coprmb -  ;
                                 valor(wrk_pos)
                    ENDIF
               ENDIF
               UNLOCK
          ENDIF
     ENDIF
ENDFOR
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
IF a1(05) = 'EMER' .OR. a1(05) =  ;
   'AFNE'
     = transito_2(a2(4))
     IF wrk_mal = 1
          a2( 4) = tran_ant
          RETURN .F.
     ENDIF
ENDIF
RETURN
*
PROCEDURE transito_2
PARAMETER can_2
SELECT gc_alm00
SEEK wrk_cla1
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
PROCEDURE precio1
tran_pre = a2(5)
RETURN
*
FUNCTION precio2
IF LASTKEY() <> 13
     IF LASTKEY() = 27
          RETURN
     ELSE
          RETURN .F.
     ENDIF
ENDIF
IF a2(5) = 0
     DO p_mensaje WITH  ;
        'PROMEDIO NO PUEDE SER CERO..'
     RETURN .F.
ENDIF
IF a2(5) < 0
     DO p_mensaje WITH  ;
        'PROMEDIO NO PUEDE SER NEGATIVO..'
     RETURN .F.
ENDIF
IF a1(5) = 'AVNE'
     IF gc_pro00.pro_coprmo <  ;
        a2(5)
          DO p_mensaje WITH  ;
             'PROMEDIO MAYOR QUE EL PROMEDIO ACTUAL..'
          RETURN .F.
     ENDIF
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
SET FILTER TO
IF a1(05) = 'EMER' .OR. a1(05) =  ;
   'AFNE'
     SET FILTER TO alm_codalm = a1(7)
ELSE
     SET FILTER TO alm_codalm = a1(6)
ENDIF
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
            'Reser', pro_subcat  ;
            :R :H = 'Sub-Cat',  ;
            pro_coprmo :R :P =  ;
            '9,999,999.99' :H =  ;
            'Promedio' IN produ
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

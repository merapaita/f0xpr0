*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW tabalm FROM 0, 0 TO  ;
       24, 79 IN screen DOUBLE
DEFINE WINDOW codigo FROM 05, 05  ;
       TO 08, 74 IN screen
DEFINE WINDOW codcom FROM 09, 30  ;
       TO 21, 47 IN screen NONE  ;
       COLOR SCHEME 8
DEFINE WINDOW presta FROM 05, 43  ;
       TO 07, 76 GROW FLOAT IN  ;
       screen NONE COLOR SCHEME  ;
       8
DEFINE POPUP tabalm FROM 12, 05  ;
       TO 18, 74 PROMPT FIELDS  ;
       '  ' + alm_codalm + ' ³ ' +  ;
       tab_destab + ' ³  ' +  ;
       STR(alm_stkfis, 9, 2) +  ;
       ' ³ ' + alm_ubicac IN  ;
       screen COLOR SCHEME 8
ON SELECTION POPUP tabalm DEAC POPUP TABALM
DEFINE WINDOW titpass FROM 09, 05  ;
       TO 11, 74 IN screen NONE  ;
       COLOR SCHEME 8
DEFINE WINDOW pidpass FROM 19, 05  ;
       TO 21, 74 IN screen NONE  ;
       COLOR SCHEME 20
ACTIVATE WINDOW tabalm
DO p_prestab WITH 'MANTENCION',  ;
   'UBICACION DE PRODUCTOS',  ;
   'MANTENCION'
@ 2, 63 SAY PROGRAM()
SELECT 1
USE GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 2
USE GC_ALM00 ORDER CODIGO
SELECT 3
USE GE_TAB0 ORDER CODIGO
DO WHILE .T.
     SET CURSOR ON
     ON KEY
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     wrk_selec = SELECT()
     wrk_campo = 'WRK_CODPRO'
     ON KEY LABEL F6 DO PRODUC WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO;

     ACTIVATE WINDOW codigo
     @ 00, 01 CLEAR TO 01, 70
     STORE SPACE(14) TO  ;
           wrk_codpro
     @ 00, 05 SAY  ;
       'C¢digo de Producto :' GET  ;
       wrk_codpro PICTURE '@!'
     READ
     IF LASTKEY() <> 27 .AND.  ;
        wrk_codpro <> SPACE(14)
          SELECT 1
          SET ORDER TO CODIGO
          SEEK wrk_codpro
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'CODIGO NO EXISTE'
               LOOP
          ENDIF
          @ 00, 41 SAY  ;
            SUBSTR(pro_descri, 1,  ;
            26)
          wrk_unimed = pro_unimed
          SELECT 3
          SEEK 'UNID' +  ;
               wrk_unimed
          IF FOUND()
               wrk_des1 = tab_destab
          ELSE
               wrk_des1 = 'No Existe esta Unidad de Inventario'
          ENDIF
          @ 01, 05 SAY  ;
            'Unidad de Inventa. : ' +  ;
            wrk_unimed + ' ' +  ;
            wrk_des1
          SELECT gc_alm00.alm_codalm,  ;
                 ge_tab0.tab_destab,  ;
                 gc_alm00.alm_stkfis,  ;
                 gc_alm00.alm_ubicac  ;
                 FROM GC_ALM00,  ;
                 GE_TAB0 WHERE  ;
                 gc_alm00.alm_codpro =  ;
                 wrk_codpro AND  ;
                 ge_tab0.tab_codtab =  ;
                 gc_alm00.alm_codalm  ;
                 AND  ;
                 ge_tab0.tab_codpre =  ;
                 'ALMA' INTO  ;
                 CURSOR ALM
          COUNT TO wrk_count
          ON KEY
          IF wrk_count = 0
               DO p_mensaje WITH  ;
                  'No Tiene Almacen Asignado'
               LOOP
          ELSE
               ON KEY LABEL ENTER DO P_LEE;
   WITH 2
               DO p_footer WITH  ;
                  '100000000000101000001',  ;
                  2
          ENDIF
          ACTIVATE WINDOW titpass
          @ 00, 00 SAY  ;
            'ÚÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
          @ 01, 00 SAY  ;
            '³ALMACEN³         DESCRIPCION            ³    STOCK   ³   UBICACION  ³'
          @ 02, 00 SAY  ;
            'ÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
          DO WHILE LASTKEY()<>27
               ACTIVATE POPUP  ;
                        tabalm
          ENDDO
          DEACTIVATE WINDOW  ;
                     titpass
     ELSE
          EXIT
     ENDIF
ENDDO
DEACTIVATE WINDOW codigo, footer,  ;
           tablas
RELEASE WINDOW titpass, pidpass,  ;
        tabalm
RELEASE POPUP tabalm
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE p_borra
ON KEY
aux_swt = .T.
wrk_reg = RECNO()
DO WHILE aux_swt .AND. LASTKEY()= ;
   13
     IF RLOCK()
          wrk_codalm = alm_codalm
          SELECT 2
          SET FILTER TO
          SEEK wrk_codpro +  ;
               wrk_codalm
          DELETE
          UNLOCK
          EXIT
     ELSE
          aux_swt = f_yesno( ;
                    'Registro bloqueado. Intentar nuevamente ' ;
                    )
     ENDIF
ENDDO
SELECT gc_alm00.alm_codalm,  ;
       ge_tab0.tab_destab,  ;
       gc_alm00.alm_stkfis,  ;
       gc_alm00.alm_ubicac FROM  ;
       GC_ALM00, GE_TAB0 WHERE  ;
       gc_alm00.alm_codpro =  ;
       wrk_codpro AND  ;
       ge_tab0.tab_codtab =  ;
       gc_alm00.alm_codalm AND  ;
       ge_tab0.tab_codpre =  ;
       'ALMA' INTO CURSOR ALM
COUNT TO wrk_count
IF wrk_count = 0
     DO p_mensaje WITH  ;
        'No Tiene Almacen Asignado'
     EXIT
ELSE
     ON KEY LABEL ENTER DO P_LEE;
   WITH 2
     DO p_footer WITH  ;
        '100000000000101000001',  ;
        2
ENDIF
RETURN
*
PROCEDURE p_lee
PARAMETER aux_opcion
DO p_footer WITH  ;
   '100000000000000000001', 2
SET CURSOR ON
ON KEY
swt = .T.
tmp_fin = 0
tmp_1 = 0
IF aux_opcion = 2
     wrk_codalm = alm_codalm
ENDIF
DO WHILE LASTKEY()=13
     IF RLOCK()
          ACTIVATE WINDOW pidpass
          @ 00, 00 SAY  ;
            'ÚÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
          @ 01, 00 SAY  ;
            '³       ³                                ³            ³              ³'
          @ 02, 00 SAY  ;
            'ÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
          SELECT 2
          SET FILTER TO
          DIMENSION a1( FCOUNT())
          IF aux_opcion = 1
               SCATTER BLANK TO  ;
                       a1
          ELSE
               SEEK wrk_codpro +  ;
                    wrk_codalm
               SCATTER TO a1
          ENDIF
          a1( 1) = wrk_codpro
          aux_espejo = a1(1)
          IF aux_opcion = 2
               @ 01, 03 SAY a1(2)  ;
                 PICTURE '9999'
               SELECT 3
               SEEK 'ALMA' +  ;
                    a1(2)
               IF FOUND()
                    @ 01, 10 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 25)
               ENDIF
               @ 01, 43 SAY a1(3)  ;
                 PICTURE  ;
                 '9999,999.99'
               @ 01, 56 GET a1(  ;
                 5) PICTURE '@!'
               READ
          ELSE
               tmp_1 = 0
               DO WHILE .T. .AND.  ;
                  tmp_1=0
                    DO p_footer  ;
                       WITH  ;
                       '100010000000000000001',  ;
                       2
                    ON KEY
                    wrk_busca = 'ALMA'
                    wrk_var = 'A1(2)'
                    ON KEY LABEL F6 DO;
BUSCA  WITH WRK_BUSCA,WRK_VAR
                    ACTIVATE WINDOW  ;
                             pidpass
                    @ 01, 03 GET  ;
                      a1( 2)  ;
                      PICTURE  ;
                      '@!'
                    READ
                    IF LASTKEY() =  ;
                       27
                         tmp_1 = 1
                         EXIT
                    ENDIF
                    IF LASTKEY() =  ;
                       5 .OR.  ;
                       LASTKEY() =  ;
                       19
                         EXIT
                    ENDIF
                    SELECT 2
                    SET FILTER TO
                    SEEK a1(1) +  ;
                         a1(2)
                    IF FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'Almacen para Ubicacion ya Existe'
                         LOOP
                    ENDIF
                    SELECT 3
                    SEEK 'ALMA' +  ;
                         a1(2)
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'Codigo de Alamacen no Existe'
                         LOOP
                    ELSE
                         @ 01, 10  ;
                           SAY  ;
                           SUBSTR(tab_destab,  ;
                           1,  ;
                           12)
                    ENDIF
                    DO WHILE .T.  ;
                       .AND.  ;
                       tmp_1=0
                         DO p_footer  ;
                            WITH  ;
                            '100000000000000000001',  ;
                            2
                         ACTIVATE  ;
                          WINDOW  ;
                          pidpass
                         @ 01, 43  ;
                           GET  ;
                           a1( 3)  ;
                           PICTURE  ;
                           '9999,999.99'
                         READ
                         IF LASTKEY() =  ;
                            27
                              tmp_1 =  ;
                               1
                              EXIT
                         ENDIF
                         IF LASTKEY() =  ;
                            5  ;
                            .OR.  ;
                            LASTKEY() =  ;
                            19
                              EXIT
                         ENDIF
                         DO WHILE  ;
                            .T.  ;
                            .AND.  ;
                            tmp_1= ;
                            0
                              @ 01,  ;
                                56  ;
                                GET  ;
                                a1(  ;
                                5)  ;
                                PICTURE  ;
                                '@!'
                              READ
                              IF LASTKEY() =  ;
                                 27
                                   tmp_1 = 1
                                   EXIT
                              ENDIF
                              IF LASTKEY() =  ;
                                 5  ;
                                 .OR.  ;
                                 LASTKEY() =  ;
                                 19
                                   EXIT
                              ENDIF
                              tmp_1 =  ;
                               1
                         ENDDO
                    ENDDO
               ENDDO
          ENDIF
          IF LASTKEY() <> 27
               SELECT 2
               IF aux_opcion = 1
                    APPEND BLANK
               ENDIF
               GATHER FROM a1
          ENDIF
          DEACTIVATE WINDOW  ;
                     pidpass
          EXIT
     ELSE
          swt = f_yesno( ;
                'Registro bloqueado. Intentar nuevamente ' ;
                )
     ENDIF
ENDDO
SET CURSOR OFF
SELECT gc_alm00.alm_codalm,  ;
       ge_tab0.tab_destab,  ;
       gc_alm00.alm_stkfis,  ;
       gc_alm00.alm_ubicac FROM  ;
       GC_ALM00, GE_TAB0 WHERE  ;
       gc_alm00.alm_codpro =  ;
       wrk_codpro AND  ;
       ge_tab0.tab_codtab =  ;
       gc_alm00.alm_codalm AND  ;
       ge_tab0.tab_codpre =  ;
       'ALMA' INTO CURSOR ALM
ON KEY LABEL ENTER DO P_LEE    WITH 2
DO p_footer WITH  ;
   '100000000000101000001', 2
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

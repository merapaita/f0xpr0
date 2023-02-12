*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW t0 FROM 02, 64 TO  ;
       02, 73 IN screen NONE
DEFINE WINDOW marco FROM 04, 01  ;
       TO 21, 78 IN screen NONE
flag = USED('gc_PAR00')
ACTIVATE WINDOW tablas
DO p_prestab WITH 'MANTENCION',  ;
   'PARAMETROS GENERALES',  ;
   'SELECCION', flag
@ 2, 63 SAY PROGRAM()
SELECT 1
USE gc_PAR00
SELECT 2
USE GE_TAB0 ORDER CODIGO
wrk_sel2 = SELECT()
SELECT 3
USE gc_HLP00 ORDER CODIGO
STORE SPACE(08) TO wrk_numruc
STORE SPACE(25) TO wrk_razsoc
STORE SPACE(20) TO wrk_calle,  ;
      wrk_distri, wrk_provin
STORE SPACE(10) TO wrk_abrev
STORE SPACE(04) TO wrk_monbas,  ;
      wrk_codalm, wrk_lptfac,  ;
      wrk_lptgui, wrk_lptped,  ;
      wrk_lptbol, wrk_codlis,  ;
      busca
STORE 0 TO tmp_ing, wrk_pormax,  ;
      tmp_cod, tmp_1, tmp_fin
STORE .F. TO tmp_est
DO WHILE tmp_fin<>27
     ON KEY
     ACTIVATE WINDOW t0
     SET COLOR TO N+/W
     @ 0, 0 SAY 'SELECCIONA'
     SET COLOR TO &MONO
     tmp_cod = 0
     tmp_1 = 0
     ACTIVATE WINDOW marco
     @ 00, 02, 07, 75 BOX  ;
       'ÚÄ¿³ÙÄÀ³ '
     @ 07, 02, 12, 75 BOX  ;
       'ÚÄ¿³ÙÄÀ³ '
     @ 07, 02 SAY 'Ã'
     @ 07, 75 SAY '´'
     @ 12, 02, 20, 75 BOX  ;
       'ÚÄ¿³ÙÄÀ³ '
     @ 12, 02 SAY 'Ã'
     @ 12, 75 SAY '´'
     @ 01, 03 SAY  ;
       'R.U.C. Empresa............ :'
     @ 02, 03 SAY  ;
       'Razon Social.............. :'
     @ 03, 03 SAY  ;
       'Raz. Soc. Abreviada....... :'
     @ 04, 03 SAY  ;
       'Direccion Empresa......... :'
     @ 05, 03 SAY  ;
       'Distrito Empresa.......... :'
     @ 06, 03 SAY  ;
       'Provincia Empresa......... :'
     @ 08, 03 SAY  ;
       'Punto Emisor ............. :'
     @ 08, 50 SAY  ;
       '% Maximo Desc.:'
     @ 09, 03 SAY  ;
       'Lista de Precio Ofic...... :'
     @ 10, 03 SAY  ;
       'Cod. Almacen Principal.... :'
     @ 11, 03 SAY  ;
       'Cod. Moneda Base.......... :'
     @ 13, 03 SAY  ;
       'Impresora Facturas........ :'
     @ 14, 03 SAY  ;
       "Impresora Guias d'Remisi¢n :"
     @ 15, 03 SAY  ;
       'Impresora Pedido de Rptos. :'
     @ 16, 03 SAY  ;
       'Impresora Boleta de Venta. :'
     SELECT 1
     IF RECCOUNT() = 0
          STORE 1 TO tmp_ing
          DO procesa
     ELSE
          STORE par_numruc TO  ;
                wrk_numruc
          STORE par_razsoc TO  ;
                wrk_razsoc
          STORE par_abrev TO  ;
                wrk_abrev
          STORE par_calle TO  ;
                wrk_calle
          STORE par_distri TO  ;
                wrk_distri
          STORE par_provin TO  ;
                wrk_provin
          STORE par_punemi TO  ;
                wrk_punemi
          STORE par_pormax TO  ;
                wrk_pormax
          STORE par_codlis TO  ;
                wrk_codlis
          STORE par_codalm TO  ;
                wrk_codalm
          STORE par_monbas TO  ;
                wrk_monbas
          STORE par_lptfac TO  ;
                wrk_lptfac
          STORE par_lptgui TO  ;
                wrk_lptgui
          STORE par_lptped TO  ;
                wrk_lptped
          STORE par_lptbol TO  ;
                wrk_lptbol
          @ 01, 32 GET wrk_numruc
          @ 02, 32 GET wrk_razsoc
          @ 03, 32 GET wrk_abrev
          @ 04, 32 GET wrk_calle
          @ 05, 32 GET wrk_distri
          @ 06, 32 GET wrk_provin
          @ 08, 32 GET wrk_punemi
          IF wrk_punemi <> '    '
               SELECT 2
               SEEK 'EMIS' +  ;
                    wrk_punemi
               IF  .NOT. FOUND()
                    @ 08, 37 SAY  ;
                      'No Existe'
               ELSE
                    @ 08, 37 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 11)
               ENDIF
          ENDIF
          @ 08, 66 GET wrk_pormax  ;
            PICTURE '999.99'
          @ 09, 32 GET wrk_codlis
          IF wrk_codlis <> ' '
               SELECT 3
               SEEK wrk_codlis
               IF  .NOT. FOUND()
                    @ 09, 37 SAY  ;
                      'No Existe'
               ELSE
                    @ 09, 37 SAY  ;
                      SUBSTR(hlp_deslis,  ;
                      1, 12)
               ENDIF
          ELSE
               @ 09, 37 SAY  ;
                 'No Existe'
          ENDIF
          @ 10, 32 GET wrk_codalm
          IF wrk_codalm <> ' '
               SELECT 2
               SEEK 'ALMA' +  ;
                    wrk_codalm
               IF  .NOT. FOUND()
                    @ 10, 37 SAY  ;
                      'No Existe'
               ELSE
                    @ 10, 37 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 12)
               ENDIF
          ENDIF
          @ 11, 32 GET wrk_monbas
          IF wrk_monbas <> ' '
               SELECT 2
               SEEK 'MONE' +  ;
                    wrk_monbas
               IF  .NOT. FOUND()
                    @ 11, 37 SAY  ;
                      'No existe'
               ELSE
                    @ 11, 37 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 12)
               ENDIF
          ENDIF
          @ 13, 32 GET wrk_lptfac
          IF wrk_lptfac <> ' '
               SELECT 2
               SEEK 'IMPR' +  ;
                    wrk_lptfac
               IF  .NOT. FOUND()
                    @ 13, 37 SAY  ;
                      'No existe'
               ELSE
                    @ 13, 37 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 12)
               ENDIF
          ENDIF
          @ 14, 32 GET wrk_lptgui
          IF wrk_lptgui <> ' '
               SELECT 2
               SEEK 'IMPR' +  ;
                    wrk_lptgui
               IF  .NOT. FOUND()
                    @ 14, 37 SAY  ;
                      'No Existe'
               ELSE
                    @ 14, 37 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 12)
               ENDIF
          ENDIF
          @ 15, 32 GET wrk_lptped
          IF wrk_lptped <> ' '
               SELECT 2
               SEEK 'IMPR' +  ;
                    wrk_lptped
               IF  .NOT. FOUND()
                    @ 15, 37 SAY  ;
                      'No Existe'
               ELSE
                    @ 15, 37 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 12)
               ENDIF
          ENDIF
          @ 16, 32 GET wrk_lptbol
          IF wrk_lptbol <> ' '
               SELECT 2
               SEEK 'IMPR' +  ;
                    wrk_lptbol
               IF  .NOT. FOUND()
                    @ 16, 37 SAY  ;
                      'No Existe'
               ELSE
                    @ 16, 37 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 12)
               ENDIF
          ENDIF
          CLEAR GETS
          DO p_footer WITH  ;
             '100000000000100000001',  ;
             2
          tmp_x = INKEY(0, 'H')
          IF LASTKEY() = 27
               EXIT
          ENDIF
          IF LASTKEY() = 13
               DO procesa
          ENDIF
     ENDIF
ENDDO
DEACTIVATE WINDOW footer, tablas,  ;
           t0, marco
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
ON KEY
RETURN
*
PROCEDURE procesa
flag = USED('gc_PAR00')
ACTIVATE WINDOW t0
SET COLOR TO N+/W
@ 0, 0 SAY 'MODIFICAR '
SET COLOR TO &MONO
tmp_err = 1
DO WHILE tmp_fin<>27 .AND.  ;
   tmp_err=1
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW marco
     ON KEY
     SET CURSOR ON
     DO WHILE .T. .AND. tmp_1=0
          ON KEY
          @ 01, 32 GET wrk_numruc  ;
            PICTURE '@!'
          READ
          IF LASTKEY() = 27
               tmp_1 = 1
               EXIT
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               EXIT
          ENDIF
          IF wrk_numruc =  ;
             SPACE(8)
               DO p_mensaje WITH  ;
                  'R.U.C. no debe ser blanco'
               LOOP
          ELSE
               tmp_err = 0
          ENDIF
          DO WHILE .T. .AND.  ;
             tmp_1=0
               ON KEY
               @ 02, 32 GET  ;
                 wrk_razsoc  ;
                 PICTURE '@!'
               READ
               tmp_err = 1
               IF LASTKEY() = 27
                    tmp_1 = 1
                    EXIT
               ENDIF
               IF LASTKEY() = 5  ;
                  .OR. LASTKEY() =  ;
                  19
                    EXIT
               ENDIF
               IF wrk_razsoc =  ;
                  SPACE(25)
                    DO p_mensaje  ;
                       WITH  ;
                       'Razon Social no debe ser Blanco'
                    LOOP
               ELSE
                    tmp_err = 0
               ENDIF
               DO WHILE .T. .AND.  ;
                  tmp_1=0
                    ON KEY
                    @ 03, 32 GET  ;
                      wrk_abrev  ;
                      PICTURE  ;
                      '@!'
                    READ
                    tmp_err = 1
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
                    IF wrk_abrev =  ;
                       SPACE(10)
                         DO p_mensaje  ;
                            WITH  ;
                            'La Abreviacion no debe ser Blanco'
                         LOOP
                    ELSE
                         tmp_err =  ;
                          0
                    ENDIF
                    DO WHILE .T.  ;
                       .AND.  ;
                       tmp_1=0
                         ON KEY
                         @ 04, 32  ;
                           GET  ;
                           wrk_calle  ;
                           PICTURE  ;
                           '@!'
                         READ
                         tmp_err =  ;
                          1
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
                         IF wrk_calle =  ;
                            SPACE(20)
                              DO p_mensaje  ;
                                 WITH  ;
                                 'Calle no debe estar en Blanco'
                              LOOP
                         ELSE
                              tmp_err =  ;
                               0
                         ENDIF
                         DO WHILE  ;
                            .T.  ;
                            .AND.  ;
                            tmp_1= ;
                            0
                              ON KEY
                              @ 05,  ;
                                32  ;
                                GET  ;
                                wrk_distri  ;
                                PICTURE  ;
                                '@!'
                              READ
                              tmp_err =  ;
                               1
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
                              IF wrk_distri =  ;
                                 SPACE(20)
                                   DO p_mensaje WITH 'Distrito no debe ser Blanco'
                                   LOOP
                              ELSE
                                   tmp_err = 0
                              ENDIF
                              DO WHILE  ;
                                 .T.  ;
                                 .AND.  ;
                                 tmp_1= ;
                                 0
                                   ON KEY
                                   @ 06, 32 GET wrk_provin PICTURE '@!'
                                   tmp_err = 1
                                   READ
                                   IF LASTKEY() = 27
                                        tmp_1 = 1
                                        EXIT
                                   ENDIF
                                   IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                        EXIT
                                   ENDIF
                                   IF wrk_provin = SPACE(20)
                                        DO p_mensaje WITH 'Provincia no debe ser Blanco'
                                        LOOP
                                   ELSE
                                        tmp_err = 0
                                   ENDIF
                                   DO WHILE .T. .AND. tmp_1=0
                                        DO p_footer WITH '100010000000000000001', 2
                                        ACTIVATE WINDOW marco
                                        ON KEY
                                        wrk_busca = 'EMIS'
                                        wrk_var = 'WRK_PUNEMI'
                                        ON KEY LABEL F6 DO BUSCA  WITH WRK_BUSCA,WRK_VAR
                                        @ 08, 32 GET wrk_punemi
                                        READ LOCK
                                        tmp_err = 1
                                        IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                             EXIT
                                        ENDIF
                                        IF LASTKEY() = 27
                                             tmp_1 = 1
                                             EXIT
                                        ENDIF
                                        SELECT 2
                                        SEEK 'EMIS' + wrk_punemi
                                        IF  .NOT. FOUND()
                                             DO p_mensaje WITH 'Codigo de Punto Emisor no Existe'
                                             LOOP
                                        ELSE
                                             @ 08, 37 SAY SUBSTR(tab_destab, 1, 12)
                                        ENDIF
                                        DO WHILE .T. .AND. tmp_1=0
                                             DO p_footer WITH '100000000000000000001', 2
                                             ACTIVATE WINDOW marco
                                             ON KEY
                                             @ 08, 66 GET wrk_pormax PICTURE '999.99'
                                             READ
                                             tmp_err = 1
                                             IF LASTKEY() = 27
                                                  tmp_1 = 1
                                                  EXIT
                                             ENDIF
                                             IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                  EXIT
                                             ENDIF
                                             IF wrk_pormax = 0
                                                  DO p_mensaje WITH 'Porcentaje MAximo no Debe ser Cero'
                                                  LOOP
                                             ELSE
                                                  tmp_err = 0
                                             ENDIF
                                             DO WHILE .T. .AND. tmp_1=0
                                                  DO p_footer WITH '100010000000000000001', 2
                                                  ACTIVATE WINDOW marco
                                                  ON KEY
                                                  wrk_var = 'WRK_CODLIS'
                                                  ON KEY LABEL F6 DO BUSHLP  WITH WRK_VAR
                                                  @ 09, 32 GET wrk_codlis PICTURE '@!'
                                                  READ
                                                  IF LASTKEY() = 27
                                                       tmp_1 = 1
                                                       EXIT
                                                  ENDIF
                                                  IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                       EXIT
                                                  ENDIF
                                                  SELECT 3
                                                  SEEK wrk_codlis
                                                  IF  .NOT. FOUND()
                                                       DO p_mensaje WITH 'Lista de Precios no Existe'
                                                       LOOP
                                                  ELSE
                                                       @ 09, 37 SAY SUBSTR(hlp_deslis, 1, 12)
                                                  ENDIF
                                                  DO WHILE .T. .AND. tmp_1=0
                                                       DO p_footer WITH '100010000000000000001', 2
                                                       ACTIVATE WINDOW marco
                                                       ON KEY
                                                       wrk_busca = 'ALMA'
                                                       wrk_var = 'WRK_CODALM'
                                                       ON KEY LABEL F6 DO BUSCA  WITH WRK_BUSCA,WRK_VAR
                                                       @ 10, 32 GET wrk_codalm PICTURE '@!'
                                                       READ
                                                       IF LASTKEY() = 27
                                                            tmp_1 = 1
                                                            EXIT
                                                       ENDIF
                                                       IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                            EXIT
                                                       ENDIF
                                                       SELECT 2
                                                       SEEK 'ALMA' + wrk_codalm
                                                       IF  .NOT. FOUND()
                                                            DO p_mensaje WITH 'Codigo de Alamacen no Existe'
                                                            LOOP
                                                       ELSE
                                                            @ 10, 37 SAY SUBSTR(tab_destab, 1, 12)
                                                       ENDIF
                                                       DO WHILE .T. .AND. tmp_1=0
                                                            DO p_footer WITH '100010000000000000001', 2
                                                            ACTIVATE WINDOW marco
                                                            ON KEY
                                                            wrk_busca = 'MONE'
                                                            wrk_var = 'WRK_MONBAS'
                                                            ON KEY LABEL F6 DO BUSCA  WITH WRK_BUSCA,WRK_VAR
                                                            @ 11, 32 GET wrk_monbas PICTURE '@!'
                                                            READ
                                                            IF LASTKEY() = 27
                                                                 tmp_1 = 1
                                                                 EXIT
                                                            ENDIF
                                                            IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                                 EXIT
                                                            ENDIF
                                                            SELECT 2
                                                            SEEK 'MONE' + wrk_monbas
                                                            IF  .NOT. FOUND()
                                                                 DO p_mensaje WITH 'Codigo de Moneda no Existe'
                                                                 LOOP
                                                            ELSE
                                                                 @ 11, 37 SAY SUBSTR(tab_destab, 1, 12)
                                                            ENDIF
                                                            DO WHILE .T. .AND. tmp_1=0
                                                                 DO p_footer WITH '100010000000000000001', 2
                                                                 ACTIVATE WINDOW marco
                                                                 ON KEY
                                                                 wrk_busca = 'IMPR'
                                                                 wrk_var = 'WRK_LPTFAC'
                                                                 ON KEY LABEL F6 DO BUSCA  WITH WRK_BUSCA,WRK_VAR
                                                                 @ 13, 32 GET wrk_lptfac PICTURE '@!'
                                                                 READ
                                                                 IF LASTKEY() = 27
                                                                      tmp_1 = 1
                                                                      EXIT
                                                                 ENDIF
                                                                 IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                                      EXIT
                                                                 ENDIF
                                                                 SELECT 2
                                                                 SEEK 'IMPR' + wrk_lptfac
                                                                 IF  .NOT. FOUND()
                                                                      DO p_mensaje WITH 'Direccion no Existe'
                                                                      LOOP
                                                                 ELSE
                                                                      @ 13, 37 SAY SUBSTR(tab_destab, 1, 12)
                                                                      CLEAR GETS
                                                                 ENDIF
                                                                 DO WHILE .T. .AND. tmp_1=0
                                                                      DO p_footer WITH '100010000000000000001', 2
                                                                      ACTIVATE WINDOW marco
                                                                      ON KEY
                                                                      wrk_busca = 'IMPR'
                                                                      wrk_var = 'WRK_LPTGUI'
                                                                      ON KEY LABEL F6 DO BUSCA  WITH WRK_BUSCA,WRK_VAR
                                                                      @ 14, 32 GET wrk_lptgui PICTURE '@!'
                                                                      READ
                                                                      IF LASTKEY() = 27
                                                                           tmp_1 = 1
                                                                           EXIT
                                                                      ENDIF
                                                                      IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                                           EXIT
                                                                      ENDIF
                                                                      SELECT 2
                                                                      SEEK 'IMPR' + wrk_lptgui
                                                                      IF  .NOT. FOUND()
                                                                           DO p_mensaje WITH 'Direccion no Existe'
                                                                           LOOP
                                                                      ELSE
                                                                           @ 14, 37 SAY SUBSTR(tab_destab, 1, 12)
                                                                           CLEAR GETS
                                                                      ENDIF
                                                                      DO WHILE .T. .AND. tmp_1=0
                                                                           ON KEY
                                                                           busca = 'IMPR'
                                                                           wrk_var = 'WRK_LPTPED'
                                                                           ON KEY LABEL F6 DO BUSCA  WITH WRK_BUSCA,WRK_VAR
                                                                           @ 15, 32 GET wrk_lptped PICTURE '@!'
                                                                           READ
                                                                           IF LASTKEY() = 27
                                                                                tmp_1 = 1
                                                                                EXIT
                                                                           ENDIF
                                                                           IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                                                EXIT
                                                                           ENDIF
                                                                           SELECT 2
                                                                           SEEK 'IMPR' + wrk_lptped
                                                                           IF  .NOT. FOUND()
                                                                                DO p_mensaje WITH 'Direccion no Existe'
                                                                                LOOP
                                                                           ELSE
                                                                                @ 15, 37 SAY SUBSTR(tab_destab, 1, 12)
                                                                                CLEAR GETS
                                                                           ENDIF
                                                                           DO WHILE .T. .AND. tmp_1=0
                                                                                ON KEY
                                                                                busca = 'IMPR'
                                                                                wrk_var = 'WRK_LPTBOL'
                                                                                ON KEY LABEL F6 DO BUSCA  WITH WRK_BUSCA,WRK_VAR
                                                                                @ 16, 32 GET wrk_lptbol PICTURE '@!'
                                                                                READ
                                                                                IF LASTKEY() = 27
                                                                                     tmp_1 = 1
                                                                                     EXIT
                                                                                ENDIF
                                                                                IF LASTKEY() = 5 .OR. LASTKEY() = 19
                                                                                     EXIT
                                                                                ENDIF
                                                                                SELECT 2
                                                                                SEEK 'IMPR' + wrk_lptbol
                                                                                IF  .NOT. FOUND()
                                                                                     DO p_mensaje WITH 'Direccion no Existe'
                                                                                     LOOP
                                                                                ELSE
                                                                                     @ 16, 37 SAY SUBSTR(tab_destab, 1, 12)
                                                                                     CLEAR GETS
                                                                                ENDIF
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
                    ENDDO
               ENDDO
          ENDDO
     ENDDO
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW marco
     CLEAR GETS
     SELECT 1
     IF LASTKEY() <> 27
          IF tmp_ing = 1
               APPEND BLANK
          ENDIF
          REPLACE par_numruc WITH  ;
                  wrk_numruc
          REPLACE par_razsoc WITH  ;
                  wrk_razsoc
          REPLACE par_abrev WITH  ;
                  wrk_abrev
          REPLACE par_calle WITH  ;
                  wrk_calle
          REPLACE par_distri WITH  ;
                  wrk_distri
          REPLACE par_provin WITH  ;
                  wrk_provin
          REPLACE par_punemi WITH  ;
                  wrk_punemi
          REPLACE par_pormax WITH  ;
                  wrk_pormax
          REPLACE par_codlis WITH  ;
                  wrk_codlis
          REPLACE par_codalm WITH  ;
                  wrk_codalm
          REPLACE par_monbas WITH  ;
                  wrk_monbas
          REPLACE par_lptfac WITH  ;
                  wrk_lptfac
          REPLACE par_lptgui WITH  ;
                  wrk_lptgui
          REPLACE par_lptped WITH  ;
                  wrk_lptped
          REPLACE par_lptbol WITH  ;
                  wrk_lptbol
          REPLACE par_usuari WITH  ;
                  clave
          REPLACE par_fecha WITH  ;
                  DATE()
          REPLACE par_hora WITH  ;
                  SUBSTR(TIME(),  ;
                  1, 5)
          DO p_menpar
          wrk_sw = .F.
          tmp_1 = 0
     ENDIF
     EXIT
ENDDO
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW tablas FROM 0, 0 TO  ;
       24, 79 IN screen DOUBLE
DEFINE WINDOW codigo FROM 05, 05  ;
       TO 07, 74 IN screen
DEFINE WINDOW codcom FROM 09, 30  ;
       TO 21, 47 IN screen NONE  ;
       COLOR SCHEME 8
DEFINE WINDOW presta FROM 05, 43  ;
       TO 07, 76 GROW FLOAT IN  ;
       screen NONE COLOR SCHEME  ;
       8
DEFINE POPUP tablas FROM 11, 05  ;
       TO 18, 74 PROMPT FIELDS  ;
       STR(tmp_f1, 4) + '    ³ ' +  ;
       tmp_un1 + ' ³' +  ;
       SUBSTR(tmp_des1, 1, 17) +  ;
       '³' + STR(tmp_f2, 4) +  ;
       '    ³ ' + tmp_un2 + ' ³ ' +  ;
       tmp_des2 + '³' IN screen  ;
       COLOR SCHEME 8
ON SELECTION POPUP tablas DEAC POPUP TABLAS
DEFINE WINDOW titpass FROM 08, 05  ;
       TO 10, 74 IN screen NONE  ;
       COLOR SCHEME 8
DEFINE WINDOW pidpass FROM 19, 05  ;
       TO 21, 74 IN screen NONE  ;
       COLOR SCHEME 20
ACTIVATE WINDOW tablas
DO p_prestab WITH 'MANTENCION',  ;
   'UNIDADES ALTERNATIVAS',  ;
   'MANTENCION'
@ 2, 63 SAY PROGRAM()
CLOSE DATABASES
SELECT 1
USE GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 2
USE GC_UNI00 ORDER CODIGO
SELECT 3
USE GE_TAB0 ORDER CODIGO
SELECT 4
USE gc_alm00 ORDER codigo
wrk_des2 = SPACE(20)
wrk_desuni = SPACE(20)
DO WHILE .T.
     ON KEY
     SET CURSOR ON
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW codigo
     wrk_campo = 'WRK_CODPRO'
     wrk_selec = SELECT()
     ON KEY LABEL F6 DO PRODUC WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO;

     STORE SPACE(14) TO  ;
           wrk_codpro
     @ 00, 41 CLEAR TO 00, 78
     @ 00, 05 SAY  ;
       'C¢digo de Producto : '
     SET CURSOR ON
     @ 00, 26 GET wrk_codpro  ;
       PICTURE '@!'
     READ
     IF LASTKEY() <> 27
          SELECT gc_pro00
          SET ORDER TO CODIGO
          SEEK wrk_codpro
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'CODIGO NO EXISTE'
               LOOP
          ELSE
               @ 00, 41 SAY  ;
                 SUBSTR(pro_descri,  ;
                 1, 35)
               wrk_unimed = pro_unimed
               SELECT ge_tab0
               SEEK 'UNID' +  ;
                    wrk_unimed
               IF FOUND()
                    wrk_desuni = tab_destab
               ENDIF
          ENDIF
          SELECT 5
          tmp_arch0 = f_archivo()
          CREATE CURSOR &TMP_ARCH0 (TMP_F1;
N(4,0), TMP_UN1 C(4), TMP_DES1 C(20),;
TMP_F2 N(4,0), TMP_UN2 C(4), TMP_DES2;
C(20))
          COPY TO &TMP_ARCH0
          USE &TMP_ARCH0 
          INDEX ON tmp_f1 TO  ;
                TEMP_IND
          SELECT gc_uni00
          SEEK wrk_codpro
          SCAN WHILE uni_codpro =  ;
               wrk_codpro
               wrk_f1 = uni_facto1
               wrk_f2 = uni_facto2
               wrk_alt = uni_unialt
               SELECT ge_tab0
               SEEK 'UNID' +  ;
                    wrk_unimed
               IF FOUND()
                    wrk_desuni = tab_destab
               ELSE
                    wrk_desuni = 'No Existe'
               ENDIF
               SEEK 'UNID' +  ;
                    wrk_alt
               IF FOUND()
                    wrk_des2 = tab_destab
               ELSE
                    wrk_des2 = 'No Existe'
               ENDIF
               SELECT 5
               APPEND BLANK
               REPLACE tmp_f1  ;
                       WITH  ;
                       wrk_f1,  ;
                       tmp_un1  ;
                       WITH  ;
                       wrk_unimed,  ;
                       tmp_des1  ;
                       WITH  ;
                       wrk_desuni
               REPLACE tmp_f2  ;
                       WITH  ;
                       wrk_f2,  ;
                       tmp_un2  ;
                       WITH  ;
                       wrk_alt,  ;
                       tmp_des2  ;
                       WITH  ;
                       wrk_des2
               SELECT gc_uni00
          ENDSCAN
          SELECT 5
          COUNT TO wrk_count
          ON KEY
          IF wrk_count = 0
               DO p_footer WITH  ;
                  '101000000000000000001',  ;
                  2
          ELSE
               ON KEY LABEL ENTER DO P_LEE;
WITH 2
               ON KEY LABEL F4 DO P_BORRA;

               DO p_footer WITH  ;
                  '101100000000101000001',  ;
                  2
          ENDIF
          ON KEY LABEL F3 DO P_LEE;
   WITH 1
          ACTIVATE WINDOW titpass
          @ 00, 00 SAY  ;
            'ÚÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
          @ 01, 00 SAY  ;
            '³FACTOR 1³UNIDAD³ DESCRIPCION     ³FACTOR 2³UNIDAD³ DESCRIPCION      ³'
          @ 02, 00 SAY  ;
            'ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
          DO WHILE LASTKEY()<>27
               ACTIVATE POPUP  ;
                        tablas
          ENDDO
          DEACTIVATE WINDOW  ;
                     titpass
     ELSE
          EXIT
     ENDIF
ENDDO
DEACTIVATE WINDOW codigo, footer,  ;
           tablas
RELEASE WINDOW titpass, pidpass
RELEASE POPUP tablas
ON KEY
DO p_footer WITH  ;
   '100000000001011001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE p_borra
ON KEY
aux_swt = .T.
wrk_reg = RECNO()
DO WHILE aux_swt
     IF RLOCK()
          SELECT 5
          wrk_unialt = tmp_un2
          DELETE
          SELECT gc_uni00
          SEEK wrk_codpro +  ;
               wrk_unialt
          DELETE
          UNLOCK
          EXIT
     ELSE
          aux_swt = f_yesno( ;
                    'Registro bloqueado. Intentar nuevamente ' ;
                    )
     ENDIF
ENDDO
SELECT 5
COUNT TO wrk_count
IF wrk_count = 0
     DO p_footer WITH  ;
        '101000000000000000001',  ;
        2
ELSE
     ON KEY LABEL ENTER DO P_LEE;
   WITH 2
     ON KEY LABEL F4 DO P_BORRA 
     DO p_footer WITH  ;
        '101100000000101000001',  ;
        2
ENDIF
ON KEY LABEL F3 DO P_LEE    WITH 1
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
wrk_reg = RECNO()
DO WHILE swt .AND. (LASTKEY()=13  ;
   .OR. LASTKEY()=-2)
     IF RLOCK()
          ACTIVATE WINDOW pidpass
          @ 00, 00 SAY  ;
            'ÚÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
          @ 01, 00 SAY  ;
            '³        ³      ³                 ³        ³      ³                 ³'
          @ 02, 00 SAY  ;
            'ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
          SELECT 5
          DIMENSION a1( FCOUNT())
          IF aux_opcion = 1
               SCATTER BLANK TO  ;
                       a1
          ELSE
               SCATTER TO a1
          ENDIF
          aux_espejo = a1(1)
          IF aux_opcion = 2
               @ 01, 03 SAY a1(1)  ;
                 PICTURE '9999'
               @ 01, 11 SAY a1(2)
               @ 01, 17 SAY  ;
                 SUBSTR(a1(3), 1,  ;
                 17)
               @ 01, 37 SAY a1(4)  ;
                 PICTURE '9999'
               @ 01, 45 SAY a1(4)
               @ 01, 51 SAY  ;
                 SUBSTR(a1(6), 1,  ;
                 17)
          ENDIF
          tmp_err = 1
          DO WHILE .T. .AND.  ;
             tmp_1=0
               IF aux_opcion = 1
                    a1( 01) = 0
                    a1( 02) =  ;
                      wrk_unimed
                    a1( 03) =  ;
                      SUBSTR(wrk_desuni,  ;
                      1, 17)
                    a1( 04) = 0
                    a1( 05) =  ;
                      SPACE(4)
                    a1( 06) =  ;
                      SPACE(20)
               ENDIF
               @ 01, 11 SAY a1(2)
               @ 01, 17 SAY  ;
                 SUBSTR(a1(3), 1,  ;
                 17)
               @ 01, 03 GET a1(  ;
                 01) PICTURE  ;
                 '9999'
               READ
               IF LASTKEY() = 27  ;
                  .OR. LASTKEY() = - ;
                  1
                    tmp_1 = 1
                    EXIT
               ENDIF
               IF LASTKEY() = 19
                    EXIT
               ENDIF
               IF a1(01) = 0
                    DO p_mensaje  ;
                       WITH  ;
                       'FACTOR 1 NO PUEDE SER CEROS'
                    LOOP
               ENDIF
               DO WHILE .T. .AND.  ;
                  tmp_1=0
                    @ 01, 37 GET  ;
                      a1( 4)  ;
                      PICTURE  ;
                      '9999'
                    READ
                    IF LASTKEY() =  ;
                       27
                         tmp_1 = 1
                         EXIT
                    ENDIF
                    IF LASTKEY() =  ;
                       19
                         EXIT
                    ENDIF
                    IF a1(04) = 0
                         DO p_mensaje  ;
                            WITH  ;
                            'FACTOR 2 NO PUEDE SER CERO'
                         LOOP
                    ENDIF
                    IF a1(04) =  ;
                       a1(01)
                         DO p_mensaje  ;
                            WITH  ;
                            'FACTOR 2 NO PUEDE IGUAL AL FACTOR 1'
                         LOOP
                    ENDIF
                    IF aux_opcion =  ;
                       1
                         DO WHILE  ;
                            .T.  ;
                            .AND.  ;
                            tmp_1= ;
                            0
                              DO p_footer  ;
                                 WITH  ;
                                 '100010000000000000001',  ;
                                 2
                              ACTIVATE  ;
                               WINDOW  ;
                               pidpass
                              wrk_var =  ;
                               'A1(5)'
                              wrk_busca =  ;
                               'UNID'
                              ON KEY LABEL;
F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
                              @ 01,  ;
                                45  ;
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
                                 19
                                   EXIT
                              ENDIF
                              IF a1(5) =  ;
                                 wrk_unimed
                                   DO p_mensaje WITH 'LA UNIDAD ALTERNATIVA NO PUEDE SER IGUAL A LA DEL PRODUCTO'
                                   LOOP
                              ENDIF
                              SELECT  ;
                               gc_uni00
                              SEEK  ;
                               wrk_codpro +  ;
                               a1(5)
                              IF FOUND()
                                   DO p_mensaje WITH 'YA EXISTE LISTA...   VERIFIQUE POR FAVOR'
                                   LOOP
                              ENDIF
                              SELECT  ;
                               ge_tab0
                              SEEK  ;
                               'UNID' +  ;
                               a1(5)
                              IF FOUND()
                                   wrk_d2 = tab_destab
                              ELSE
                                   DO p_mensaje WITH 'CODIGO NO EXISTE'
                                   LOOP
                              ENDIF
                              @ 01,  ;
                                51  ;
                                SAY  ;
                                SUBSTR(wrk_d2,  ;
                                1,  ;
                                17)
                              tmp_1 =  ;
                               1
                         ENDDO
                    ENDIF
                    tmp_1 = 1
               ENDDO
          ENDDO
          IF LASTKEY() <> 27
               SELECT gc_uni00
               IF aux_opcion = 1
                    APPEND BLANK
               ELSE
                    SEEK wrk_codpro +  ;
                         a1(5)
               ENDIF
               REPLACE uni_codpro  ;
                       WITH  ;
                       wrk_codpro,  ;
                       uni_unialt  ;
                       WITH  ;
                       a1(5)
               REPLACE uni_facto1  ;
                       WITH a1(1),  ;
                       uni_facto2  ;
                       WITH  ;
                       a1(4)
               REPLACE uni_usuari  ;
                       WITH clave,  ;
                       uni_fecha  ;
                       WITH  ;
                       DATE()
               REPLACE uni_hora  ;
                       WITH  ;
                       TIME()
               SELECT 5
               IF aux_opcion = 1
                    APPEND BLANK
               ELSE
                    GOTO wrk_reg
               ENDIF
               REPLACE tmp_f1  ;
                       WITH  ;
                       a1(1)
               REPLACE tmp_f2  ;
                       WITH  ;
                       a1(4)
               IF aux_opcion = 1
                    REPLACE tmp_un1  ;
                            WITH  ;
                            wrk_unimed
                    REPLACE tmp_des1  ;
                            WITH  ;
                            a1(3)
                    REPLACE tmp_un2  ;
                            WITH  ;
                            a1(5)
                    REPLACE tmp_des2  ;
                            WITH  ;
                            wrk_d2
               ENDIF
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
COUNT TO wrk_count
IF wrk_count = 0
     DO p_footer WITH  ;
        '101000000000000000001',  ;
        2
ELSE
     ON KEY LABEL ENTER DO P_LEE;
   WITH 2
     ON KEY LABEL F4 DO P_BORRA 
     DO p_footer WITH  ;
        '101100000000101000001',  ;
        2
ENDIF
ON KEY LABEL F3 DO P_LEE    WITH 1
SELECT 5
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

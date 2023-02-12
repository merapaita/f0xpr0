*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW tablas FROM 0, 0 TO  ;
       24, 79 IN screen DOUBLE
DEFINE WINDOW codigo FROM 04, 05  ;
       TO 06, 74 IN screen
DEFINE WINDOW codcom FROM 08, 30  ;
       TO 20, 47 IN screen NONE  ;
       COLOR SCHEME 8
DEFINE WINDOW presta FROM 04, 43  ;
       TO 06, 76 GROW FLOAT IN  ;
       screen NONE COLOR SCHEME  ;
       8
DEFINE POPUP tablas FROM 10, 05  ;
       TO 18, 74 PROMPT FIELDS  ;
       pal_copral + '   ³ ' +  ;
       SUBSTR(pro_descri, 1, 21) +  ;
       '³ ' + pal_observ IN  ;
       screen COLOR SCHEME 8
ON SELECTION POPUP tablas DEAC POPUP TABLAS
DEFINE WINDOW titpass FROM 07, 05  ;
       TO 09, 74 IN screen NONE  ;
       COLOR SCHEME 20
DEFINE WINDOW pidpass FROM 19, 05  ;
       TO 21, 74 IN screen NONE  ;
       COLOR SCHEME 20
ACTIVATE WINDOW tablas
DO p_prestab WITH 'MANTENCION',  ;
   'PRODUCTOS ALTERNATIVOS',  ;
   'MANTENCION'
@ 2, 63 SAY PROGRAM()
CLOSE DATABASES
SELECT 1
USE GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 2
USE GC_PAL00 ORDER CODIGO
SELECT 3
USE SHARED GC_ALM00 ORDER CODIGO
DO WHILE .T.
     ON KEY
     SET CURSOR ON
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW codigo
     wrk_selec = SELECT()
     wrk_campo = 'WRK_CODIGO'
     ON KEY LABEL F6 DO PRODUC WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO;

     STORE SPACE(14) TO  ;
           wrk_codigo
     @ 00, 05 SAY  ;
       'C¢digo de Producto : '  ;
       GET wrk_codigo PICTURE  ;
       '@!'
     READ
     IF LASTKEY() <> 27
          SELECT 1
          SET ORDER TO CODIGO
          SEEK wrk_codigo
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'CODIGO NO EXISTE'
               LOOP
          ENDIF
          @ 00, 42 SAY  ;
            SUBSTR(pro_descri, 1,  ;
            25)
          wrk_subc = pro_subcat
          SELECT gc_pal00.pal_copral,  ;
                 gc_pro00.pro_descri,  ;
                 gc_pal00.pal_observ  ;
                 FROM GC_PAL00,  ;
                 GC_PRO00 WHERE  ;
                 gc_pal00.pal_codpro =  ;
                 wrk_codigo AND  ;
                 gc_pal00.pal_copral =  ;
                 gc_pro00.pro_codpro  ;
                 INTO CURSOR  ;
                 PAL_A
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
            'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
          @ 01, 00 SAY  ;
            '³COD. ALTERNATIVO ³DESCRIPCION           ³OBSERVACIONES             ³'
          @ 02, 00 SAY  ;
            'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
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
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE p_borra
ON KEY
aux_swt = .T.
DO WHILE aux_swt
     IF RLOCK()
          wrk_copral = pal_copral
          SELECT gc_pal00
          SEEK wrk_codigo +  ;
               wrk_copral
          DELETE
          UNLOCK
          EXIT
     ELSE
          aux_swt = f_yesno( ;
                    'Registro bloqueado. Intentar nuevamente ' ;
                    )
     ENDIF
ENDDO
SELECT gc_pal00.pal_copral,  ;
       gc_pro00.pro_descri,  ;
       gc_pal00.pal_observ FROM  ;
       GC_PAL00, GC_PRO00 WHERE  ;
       gc_pal00.pal_codpro =  ;
       wrk_codigo AND  ;
       gc_pal00.pal_copral =  ;
       gc_pro00.pro_codpro INTO  ;
       CURSOR PAL_A
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
DO WHILE swt .AND. (LASTKEY()=13  ;
   .OR. LASTKEY()=-2)
     IF RLOCK()
          ACTIVATE WINDOW pidpass
          @ 00, 00 SAY  ;
            'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
          @ 01, 00 SAY  ;
            '³                 ³                      ³                          ³'
          @ 02, 00 SAY  ;
            'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
          wrk_copral = pal_copral
          DIMENSION a1( FCOUNT())
          IF aux_opcion = 1
               SCATTER BLANK TO  ;
                       a1
          ELSE
               SELECT gc_pal00
               SEEK wrk_codigo +  ;
                    wrk_copral
               SCATTER TO a1
          ENDIF
          IF aux_opcion = 2
               @ 01, 01 SAY a1(2)
               SELECT 1
               SEEK a1(02)
               @ 01, 20 SAY  ;
                 SUBSTR(pro_descri,  ;
                 1, 17)
               @ 01, 43 SAY  ;
                 SUBSTR(a1(3), 1,  ;
                 25)
          ENDIF
          DO WHILE .T. .AND.  ;
             tmp_1=0
               IF aux_opcion = 1
                    STORE wrk_codigo  ;
                          TO a1(  ;
                          1)
                    STORE SPACE(14)  ;
                          TO a1(  ;
                          2)
                    STORE SPACE(40)  ;
                          TO a1(  ;
                          3)
                    DO p_footer  ;
                       WITH  ;
                       '100010000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             pidpass
                    wrk_selec = SELECT()
                    wrk_campo = 'A1(02)'
                    ON KEY LABEL F6 DO;
PRODUC WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO;

                    @ 01, 01 GET  ;
                      a1( 02)
                    READ
                    IF LASTKEY() =  ;
                       27
                         tmp_1 = 1
                         EXIT
                    ENDIF
                    IF wrk_codigo =  ;
                       a1(02)
                         DO p_mensaje  ;
                            WITH  ;
                            'EL CODIGO NO DEBE REPETIRSE'
                         LOOP
                    ENDIF
                    SELECT 1
                    SET ORDER TO CODIGO
                    SEEK a1(02)
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'CODIGO NO EXISTE ...   VERIFIQUE POR FAVOR'
                         a1( 02) =  ;
                           SPACE(14)
                         LOOP
                    ENDIF
                    SELECT gc_pal00
                    SEEK wrk_codigo +  ;
                         a1(02)
                    IF FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'YA EXISTE PRODUCTO ALTERNATIVO'
                         a1( 02) =  ;
                           SPACE(14)
                         LOOP
                    ENDIF
                    wrk_campo = RECNO()
                    SELECT 1
                    @ 01, 20 SAY  ;
                      SUBSTR(pro_descri,  ;
                      1, 20)
               ENDIF
               DO WHILE .T. .AND.  ;
                  tmp_1=0
                    DO p_footer  ;
                       WITH  ;
                       '100010000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             pidpass
                    @ 01, 43 GET  ;
                      a1( 3)  ;
                      FUNCTION  ;
                      'S25'
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
                    tmp_1 = 1
               ENDDO
          ENDDO
          IF LASTKEY() <> 27
               SELECT gc_pal00
               IF aux_opcion = 1
                    APPEND BLANK
               ELSE
                    SELECT gc_pal00
                    SEEK wrk_codigo +  ;
                         a1(2)
               ENDIF
               DO rbloquea
               REPLACE pal_codpro  ;
                       WITH  ;
                       wrk_codigo
               REPLACE pal_copral  ;
                       WITH  ;
                       a1(2)
               REPLACE pal_observ  ;
                       WITH  ;
                       a1(3)
               REPLACE pal_usuari  ;
                       WITH  ;
                       clave
               REPLACE pal_fecha  ;
                       WITH  ;
                       DATE()
               REPLACE pal_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
               arch1 = f_archivo()
               copy to &arch1 for pal_codpro;
= wrk_codigo 
               SELECT 20
               use &arch1 alias tmp1
               DO WHILE  .NOT.  ;
                  EOF()
                    SELECT gc_pal00
                    SEEK tmp1.pal_copral
                    IF  .NOT.  ;
                        FOUND()
                         APPEND BLANK
                         DO rbloquea
                         REPLACE pal_codpro  ;
                                 WITH  ;
                                 tmp1.pal_copral
                         REPLACE pal_copral  ;
                                 WITH  ;
                                 wrk_codigo
                         REPLACE pal_observ  ;
                                 WITH  ;
                                 tmp1.pal_observ
                         REPLACE pal_usuari  ;
                                 WITH  ;
                                 clave
                         REPLACE pal_fecha  ;
                                 WITH  ;
                                 DATE()
                         REPLACE pal_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                    ENDIF
                    SELECT 20
                    SKIP
               ENDDO
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
SELECT gc_pal00.pal_copral,  ;
       gc_pro00.pro_descri,  ;
       gc_pal00.pal_observ FROM  ;
       GC_PAL00, GC_PRO00 WHERE  ;
       gc_pal00.pal_codpro =  ;
       wrk_codigo AND  ;
       gc_pal00.pal_copral =  ;
       gc_pro00.pro_codpro INTO  ;
       CURSOR PAL_A
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
*** 
*** ReFox - retrace your steps ... 
***

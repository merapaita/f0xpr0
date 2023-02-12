*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
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
DEFINE WINDOW vent_4 FROM 09, 01  ;
       TO 21, 78 IN screen COLOR  ;
       SCHEME 1
DEFINE POPUP tabcos FROM 12, 05  ;
       TO 18, 74 PROMPT FIELDS  ;
       pro_codpro + '³ ' +  ;
       SUBSTR(pro_descri, 1, 28) +  ;
       ' ³' + STR(pro_coremo, 11,  ;
       2) + '³ ' +  ;
       DTOC(pro_cosrep) IN screen  ;
       COLOR SCHEME 8
ON SELECTION POPUP tabcos DEAC POPUP TABCOS
DEFINE WINDOW titpass FROM 09, 05  ;
       TO 11, 74 IN screen NONE  ;
       COLOR SCHEME 8
DEFINE WINDOW pidpass FROM 19, 05  ;
       TO 21, 74 IN screen NONE  ;
       COLOR SCHEME 20
ACTIVATE WINDOW tabalm
DO p_prestab WITH 'MANTENCION',  ;
   'MARGEN COMERCIAL',  ;
   'MANTENCION'
@ 2, 63 SAY PROGRAM()
SELECT 1
USE GC_CLI00 ORDER CODIGO
SELECT 2
USE GC_PRO00 ORDER CODIGO
SELECT 3
USE GE_TAB0 ORDER CODIGO
DO WHILE .T.
     SET CURSOR ON
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ON KEY
     wrk_busca = 'P'
     wrk_var = 'WRK_CODPRO'
     ON KEY LABEL F6 DO ENTIDAD WITH WRK_BUSCA,WRK_VAR
     ACTIVATE WINDOW codigo
     @ 00, 01 CLEAR TO 01, 70
     STORE SPACE(11) TO  ;
           wrk_codpro
     @ 00, 01 SAY  ;
       'C¢digo del Proveedor :'  ;
       GET wrk_codpro PICTURE  ;
       '@!'
     READ
     IF LASTKEY() <> 27 .AND.  ;
        wrk_codpro <> SPACE(14)
          SELECT 1
          SEEK 'P' + wrk_codpro
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'CODIGO NO EXISTE'
               LOOP
          ENDIF
          @ 00, 37 SAY  ;
            SUBSTR(cli_razsoc, 1,  ;
            30)
          @ 01, 01 SAY  ;
            'Direccion .......... : ' +  ;
            ALLTRIM(cli_calle) +  ;
            ' ' + cli_numero
          SELECT 2
          SET FILTER TO pro_codpve = wrk_codpro
          GOTO TOP
          ON KEY
          IF RECCOUNT() = 0
               DO p_mensaje WITH  ;
                  'No Tiene Almacen Asignado'
               LOOP
          ELSE
               DO p_footer WITH  ;
                  '100000000000101000001',  ;
                  2
          ENDIF
          DO WHILE LASTKEY()<>27
               ACTIVATE WINDOW  ;
                        vent_4
               GOTO TOP
               wrk_busca = 'CMCO'
               wrk_var = 'GC_PRO00.PRO_CLACOM'
               ON KEY LABEL F6 DO BUSCA;
WITH WRK_BUSCA,WRK_VAR
               BROWSE FIELDS  ;
                      pro_codpro  ;
                      :W = .F. :H =  ;
                      'Codigo',  ;
                      pro_descri  ;
                      :W = .F. :H =  ;
                      ' Descripcion',  ;
                      pro_clacom  ;
                      :H =  ;
                      'Margen Comercial'  ;
                      IN vent_4
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
        tabalm, vent_4
RELEASE POPUP tabalm
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE p_lee
PARAMETER aux_opcion
DO p_footer WITH  ;
   '100000000000000000001', 2
SET CURSOR ON
ON KEY
tmp_fin = 0
tmp_1 = 0
DO WHILE LASTKEY()=13
     IF RLOCK()
          ACTIVATE WINDOW pidpass
          @ 00, 00 SAY  ;
            'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿'
          @ 01, 00 SAY  ;
            '³              ³                              ³           ³          ³'
          @ 02, 00 SAY  ;
            'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ'
          wrk_codigo = pro_codpro
          SELECT 2
          DIMENSION a1( FCOUNT())
          IF aux_opcion = 1
               SCATTER BLANK TO  ;
                       a1
          ELSE
               SEEK wrk_codigo
               SCATTER TO a1
          ENDIF
          @ 01, 01 SAY a1(1)
          @ 01, 17 SAY  ;
            SUBSTR(a1(2), 1, 27)
          tmp_1 = 0
          DO WHILE .T. .AND.  ;
             tmp_1=0
               @ 01, 47 GET a1(  ;
                 26) PICTURE  ;
                 '9999,999.99'
               @ 01, 60 GET a1(  ;
                 27) PICTURE  ;
                 '99/99/99'
               READ
               IF LASTKEY() = 27
                    tmp_1 = 1
                    EXIT
               ENDIF
               IF LASTKEY() = 5  ;
                  .OR. LASTKEY() =  ;
                  19
                    EXIT
               ENDIF
               IF a1(26) < 0
                    DO p_mensaje  ;
                       WITH  ;
                       'Importe no Puede ser Negativo'
                    LOOP
               ENDIF
               tmp_1 = 1
          ENDDO
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
SELECT gc_pro00.pro_codpro,  ;
       gc_pro00.pro_descri,  ;
       gc_pro00.pro_coremo,  ;
       gc_pro00.pro_cosrep FROM  ;
       GC_PRO00 WHERE  ;
       gc_pro00.pro_codpve =  ;
       wrk_codpro ORDER BY  ;
       gc_pro00.pro_codpro INTO  ;
       CURSOR COS
COUNT TO wrk_count
ON KEY
DO p_footer WITH  ;
   '100000000000101000001', 2
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

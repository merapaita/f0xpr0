*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
SET ESCAPE OFF
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 13, 77 IN screen  ;
       COLOR SCHEME 7
DEFINE WINDOW detalle FROM 15, 02  ;
       TO 20, 77 IN screen NONE
DEFINE WINDOW titu FROM 14, 02 TO  ;
       14, 77 IN screen NONE  ;
       COLOR SCHEME 8
DEFINE WINDOW refer FROM 10, 35  ;
       TO 13, 52 IN screen NONE
DEFINE WINDOW totales FROM 21, 02  ;
       TO 21, 77 IN screen NONE
DIMENSION linea[ 1]
DIMENSION produc[ 1]
DIMENSION descri[ 1]
DIMENSION ctotal[ 1]
linea[ 1] = ' '
p = 1
m = 0
SELECT 1
USE GE_TAB0 ORDER CODIGO
SELECT 2
USE GC_HVE00 ORDER CODIGO
SELECT 3
USE GC_DVE00 ORDER CODIGO
SELECT 4
USE GC_CLI00 ORDER CODIGO
SELECT 5
USE GC_VND00 ORDER CODIGO
SELECT 6
USE GC_CMV00
ACTIVATE WINDOW tablas
DO p_prestab WITH 'PROCESOS',  ;
   'NOTA DE DEBITO', 'PROCESO'
@ 2, 63 SAY PROGRAM()
DIMENSION refer[ 2]
refer[ 1] = 'FACTURA'
refer[ 2] = 'BOLETA DE VENTA'
wrk_tipcam = ootc(DATE(),'SOL ', ;
             'DOL ','2')
DO WHILE .T.
     ACTIVATE WINDOW cabecera
     CLEAR
     STORE SPACE(10) TO  ;
           wrk_nrodoc
     STORE SPACE(11) TO  ;
           wrk_client
     STORE SPACE(30) TO  ;
           wrk_dircli
     STORE SPACE(4) TO wrk_codref
     STORE SPACE(10) TO  ;
           wrk_nroref
     STORE SPACE(9) TO wrk_vended
     STORE SPACE(4) TO wrk_fpago
     STORE DATE() TO wrk_fecemi,  ;
           wrk_fecven
     STORE 'C' TO wrk_tiper
     STORE 'IGV ' TO wrk_igv
     p = 1
     m = 0
     item = 1
     wrk_totnet = 0
     wrk_totigv = 0
     wrk_totgen = 0
     wrk_slog = 0
     wrk_salir = 0
     wrk_cierra = 0
     wrk_factor = 0
     @ 00, 00 SAY  ;
       'C¢digo Documento  : NOTA DE DEBITO'
     @ 00, 40 SAY  ;
       'Nro. Documento :'
     @ 01, 00 SAY  ;
       'C¢digo Cliente    :'
     @ 02, 00 SAY  ;
       'Direcci¢n Cliente :'
     @ 03, 00 SAY  ;
       'C¢digo Referenc.  :'
     @ 03, 40 SAY  ;
       'Nro. de Refer. :'
     @ 04, 00 SAY  ;
       'C¢digo Vendedor   :'
     @ 05, 00 SAY  ;
       'C¢digo F. Pago    :'
     @ 06, 00 SAY  ;
       'Fecha de Emisi¢n  :'
     @ 06, 40 SAY  ;
       'Fecha de Venc. :'
     @ 07, 00 SAY  ;
       'I.G.V.            :'
     @ 00, 57 GET wrk_nrodoc  ;
       PICTURE '@!' VALID  ;
       post(1)
     @ 01, 20 GET wrk_client  ;
       PICTURE '@!' VALID post(2)  ;
       WHEN ante(2)
     @ 02, 20 SAY wrk_dircli
     @ 03, 20 GET wrk_codref  ;
       PICTURE '@!' VALID post(4)  ;
       WHEN ante(4)
     @ 03, 57 GET wrk_nroref  ;
       PICTURE '@!' VALID post(5)  ;
       WHEN ante(5)
     @ 06, 20 GET wrk_fecemi  ;
       PICTURE '@D' VALID post(8)  ;
       WHEN ante(8)
     @ 06, 57 GET wrk_fecven  ;
       PICTURE '@D' VALID  ;
       post(9)
     @ 07, 20 GET wrk_igv PICTURE  ;
       '@!' VALID post(10) WHEN  ;
       ante(10)
     READ CYCLE
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DEACTIVATE WINDOW  ;
                     tablas,  ;
                     codigo,  ;
                     footer
          DEACTIVATE WINDOW  ;
                     detalle,  ;
                     totales, a4,  ;
                     titu
          RELEASE WINDOW cabecera,  ;
                  totales, a4,  ;
                  titu, detalle
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     DO p_footer WITH  ;
        '101000000000000000001',  ;
        2
     ACTIVATE WINDOW titu
     @ 00, 07 SAY  ;
       'C¢digo              Descripci¢n                        Valor        '
     DO WHILE .T.
          ACTIVATE WINDOW detalle
          ON KEY LABEL F3 DO P_LEE WITH;
1
          @ 0, 0 GET arrayitem  ;
            DEFAULT linea(p) SIZE  ;
            06, 76 FROM linea  ;
            VALID p_lee(2) WHEN  ;
            posi() COLOR SCHEME  ;
            9
          READ CYCLE
          EXIT
     ENDDO
ENDDO
*
PROCEDURE p_lee
PARAMETER p_pasa
DEACTIVATE WINDOW totales
DO WHILE .T.
     SHOW GETS
     SHOW GET arrayitem
     STORE SPACE(4) TO wrk_codigo
     STORE SPACE(30) TO  ;
           wrk_descri
     STORE 0 TO wrk_total
     IF p_pasa = 2
          DEFINE WINDOW a4 FROM  ;
                 15 + p, 03 TO 15 +  ;
                 p, 75 IN screen  ;
                 NONE
          pos = ASCAN(linea,  ;
                arrayitem)
          wrk_codigo = produc(pos)
          wrk_descri = descri(pos)
          wrk_total = ctotal(pos)
          ACTIVATE WINDOW a4
          @ 0, 1 SAY arrayitem
     ELSE
          IF m < 4
               DEFINE WINDOW a4  ;
                      FROM 15 + m +  ;
                      1, 03 TO 15 +  ;
                      m + 1, 75  ;
                      IN screen  ;
                      NONE
          ELSE
               DEFINE WINDOW a4  ;
                      FROM 019,  ;
                      03 TO 019,  ;
                      75 IN  ;
                      screen  ;
                      NONE
          ENDIF
          ACTIVATE WINDOW a4
          @ 00, 06 SAY SPACE(6) +  ;
            '³' + SPACE(42) +  ;
            '³'
     ENDIF
     IF p_pasa = 1
          @ 00, 07 GET wrk_codigo  ;
            PICTURE '@!' VALID  ;
            post1() WHEN ante1()
     ENDIF
     @ 00, 57 GET wrk_total  ;
       PICTURE '999,999.99' VALID  ;
       post2() WHEN ante2()
     READ
     IF LASTKEY() = 27
          ACTIVATE WINDOW detalle
          EXIT
     ENDIF
     IF LASTKEY() <> 27
          IF p_pasa = 1
               m = m + 1
               DIMENSION linea[  ;
                         m]
               DIMENSION produc[  ;
                         m]
               DIMENSION descri[  ;
                         m]
               DIMENSION ctotal[  ;
                         m]
               produc[ m] =  ;
                     wrk_codigo
               descri[ m] =  ;
                     wrk_descri
               ctotal[ m] =  ;
                     wrk_total
               linea[ m] =  ;
                    SPACE(6) +  ;
                    wrk_codigo +  ;
                    ' ³ ' +  ;
                    wrk_descri +  ;
                    SPACE(10) +  ;
                    ' ³ ' +  ;
                    TRANSFORM(wrk_total,  ;
                    '999,999.99' ;
                    )
               arrayitem = SPACE(6) +  ;
                           wrk_codigo +  ;
                           ' ³ ' +  ;
                           wrk_descri +  ;
                           SPACE(10) +  ;
                           ' ³ ' +  ;
                           TRANSFORM(wrk_total,  ;
                           '999,999.99' ;
                           )
               p = m
          ELSE
               produc[ pos] =  ;
                     wrk_codigo
               descri[ pos] =  ;
                     wrk_descri
               ctotal[ pos] =  ;
                     wrk_total
               linea[ pos] =  ;
                    SPACE(6) +  ;
                    wrk_codigo +  ;
                    ' ³ ' +  ;
                    wrk_descri +  ;
                    SPACE(10) +  ;
                    ' ³ ' +  ;
                    TRANSFORM(wrk_total,  ;
                    '999,999.99' ;
                    )
               arrayitem = SPACE(6) +  ;
                           wrk_codigo +  ;
                           ' ³ ' +  ;
                           wrk_descri +  ;
                           SPACE(10) +  ;
                           ' ³ ' +  ;
                           TRANSFORM(wrk_total,  ;
                           '999,999.99' ;
                           )
               EXIT
          ENDIF
     ENDIF
     SHOW GETS
     SHOW GET arrayitem
ENDDO
DO cierra
DO p_footer WITH  ;
   '111000000000000000011', 2
ACTIVATE WINDOW detalle
ON KEY LABEL F2 DO GRABAR
ON KEY LABEL F3 DO P_LEE WITH 1
RETURN
*
FUNCTION ante1
DO p_footer WITH  ;
   '100010000000000000001', 2
ACTIVATE WINDOW a4
wrk_busca = 'DEBI'
wrk_var = 'wrk_codigo'
ON KEY LABEL F6 DO BUSCA WITH WRK_BUSCA,WRK_VAR
RETURN .T.
*
FUNCTION post1
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19 .OR. LASTKEY() = 24
     RETURN .F.
ENDIF
IF EMPTY(wrk_codigo)
     DO p_mensaje WITH  ;
        'No se Aceptan Blancos'
     RETURN .F.
ENDIF
SELECT 1
SEEK 'DEBI' + wrk_codigo
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        'C¢digo de Concepto No Existe'
     RETURN .F.
ENDIF
wrk_descri = tab_destab
@ 00, 14 SAY wrk_descri
RETURN .T.
*
PROCEDURE ante2
DO p_footer WITH  ;
   '100000000000000000001', 2
ACTIVATE WINDOW a4
RETURN
*
FUNCTION post2
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .T.
ENDIF
IF EMPTY(wrk_total)
     DO p_mensaje WITH  ;
        'No se Aceptan Blancos'
     RETURN .F.
ENDIF
wrk_cierra = 1
RETURN .T.
*
PROCEDURE cierra
ON KEY
wrk_totnet = 0
FOR a = 1 TO m
     wrk_totnet = wrk_totnet +  ;
                  ctotal(a)
ENDFOR
wrk_totigv = wrk_totnet *  ;
             (wrk_factor / 100)
wrk_totgen = wrk_totnet +  ;
             wrk_totigv
ACTIVATE WINDOW totales
@ 00, 02 SAY 'T. Neto :'
@ 00, 12 SAY wrk_totnet PICTURE  ;
  '999,999.99'
@ 00, 28 SAY 'T. Igv  :'
@ 00, 37 SAY wrk_totigv PICTURE  ;
  '999,999.99'
@ 00, 53 SAY 'T. Gral :'
@ 00, 61 SAY wrk_totgen PICTURE  ;
  '999,999.99'
RETURN
*
PROCEDURE grabar
SELECT 2
APPEND BLANK
REPLACE hve_tipdoc WITH 'NDEB'
REPLACE hve_nrodoc WITH  ;
        wrk_nrodoc
REPLACE hve_fecdoc WITH  ;
        wrk_fecemi
REPLACE hve_fecvct WITH  ;
        wrk_fecven
REPLACE hve_fechtc WITH  ;
        wrk_fecemi
REPLACE hve_tipent WITH 'D'
REPLACE hve_codent WITH  ;
        wrk_client
REPLACE hve_tidore WITH  ;
        wrk_codref
REPLACE hve_nrdore WITH  ;
        wrk_nroref
REPLACE hve_codvnd WITH  ;
        wrk_vended
REPLACE hve_codmon WITH 'SOL '
REPLACE hve_codemi WITH  ;
        rge_punemi
REPLACE hve_tippag WITH wrk_fpago
REPLACE hve_estdoc WITH 'C'
REPLACE hve_solnet WITH  ;
        wrk_totnet + wrk_totigv
REPLACE hve_solgen WITH  ;
        wrk_totgen
REPLACE hve_mtocan WITH  ;
        wrk_totgen
REPLACE hve_soligv WITH  ;
        wrk_totigv
REPLACE hve_solvta WITH  ;
        wrk_totnet
REPLACE hve_tipcam WITH  ;
        wrk_tipcam
wrk_dolgen = wrk_totgen /  ;
             wrk_tipcam
wrk_dolvta = wrk_dolgen / (1 +  ;
             (wrk_factor / 100))
wrk_doligv = wrk_dolgen -  ;
             wrk_dolvta
REPLACE hve_totnet WITH  ;
        wrk_dolvta
REPLACE hve_totigv WITH  ;
        wrk_doligv
REPLACE hve_totvta WITH  ;
        wrk_dolvta
REPLACE hve_totgen WITH  ;
        wrk_dolgen
REPLACE hve_usuari WITH clave
REPLACE hve_fecha WITH DATE()
REPLACE hve_hora WITH  ;
        SUBSTR(TIME(), 1, 5)
SELECT 3
FOR a = 1 TO m
     APPEND BLANK
     REPLACE dve_tipdoc WITH  ;
             'NDEB'
     REPLACE dve_nrodoc WITH  ;
             wrk_nrodoc
     REPLACE dve_propar WITH  ;
             produc(a)
     REPLACE dve_total WITH  ;
             ctotal(a)
     REPLACE dve_usuari WITH  ;
             clave
     REPLACE dve_fecha WITH  ;
             DATE()
     REPLACE dve_hora WITH  ;
             SUBSTR(TIME(), 1,  ;
             5)
ENDFOR
DIMENSION linea[ 1]
DIMENSION produc[ 1]
DIMENSION descri[ 1]
DIMENSION ctotal[ 1]
linea[ 1] = ' '
SHOW GETS
CLEAR READ
DEACTIVATE WINDOW detalle,  ;
           totales, a4, titu
ACTIVATE WINDOW detalle
RETURN
*
PROCEDURE regre
IF wrk_salir = 1
     CLEAR READ
ENDIF
*
PROCEDURE ante
PARAMETER opcion
DO CASE
     CASE opcion = 2
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_busca = 'C'
          wrk_var = 'wrk_client'
          ON KEY LABEL F6 DO ENTIDAD WITH;
WRK_BUSCA,WRK_VAR
     CASE opcion = 4
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_slog = 0
          ON KEY LABEL F6 DO REFEREN 
     CASE opcion = 5
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_busca = wrk_codref
          wrk_var = 'wrk_nroref'
          ON KEY LABEL F6 DO ooVerDocum;
WITH wrk_busca,wrk_var
     CASE opcion = 7
          wrk_busca = 'FPAG'
          wrk_var = 'wrk_fpago'
          ON KEY LABEL F6 DO BUSCA WITH;
wrk_busca,wrk_var
     CASE opcion = 10
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_busca = 'IGV '
          wrk_var = 'wrk_igv'
          ON KEY LABEL F6 DO BUSCA WITH;
wrk_busca,wrk_var
ENDCASE
RETURN
*
FUNCTION post
PARAMETER opcion
DO CASE
     CASE opcion = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(wrk_nrodoc)
               DO p_mensaje WITH  ;
                  'No se Aceptan Blancos'
               RETURN .F.
          ENDIF
          SELECT 2
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          wrk_nrodoc = f_ceros(wrk_nrodoc, ;
                       10,2)
          SEEK 'NDEB' +  ;
               wrk_nrodoc
          IF FOUND()
               DO p_mensaje WITH  ;
                  'N£mero de Documento ya Existe'
               RETURN .F.
          ENDIF
     CASE opcion = 2
          SELECT 4
          SEEK 'C' + wrk_client
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Cliente No Existe'
               RETURN .F.
          ENDIF
          @ 01, 33 SAY cli_razsoc
          @ 02, 25 SAY cli_calle +  ;
            cli_numero
     CASE opcion = 4
          IF EMPTY(wrk_codref)
               RETURN .T.
          ENDIF
          SELECT 1
          SEEK 'DOCU' +  ;
               wrk_codref
          IF FOUND()
               @ 03, 25 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 13)
          ELSE
               DO p_mensaje WITH  ;
                  'Tipo de Documento no Existe'
               RETURN .F.
          ENDIF
     CASE opcion = 5
          IF EMPTY(wrk_nroref)
               @ 03, 20 SAY  ;
                 SPACE(18)
               wrk_codref = '    '
               @ 04, 20 SAY  ;
                 SPACE(40)
               @ 05, 20 SAY  ;
                 SPACE(40)
               RETURN .T.
          ENDIF
          SELECT 2
          SEEK wrk_codref +  ;
               wrk_nroref
          IF FOUND()
               IF hve_estdoc =  ;
                  'A'
                    DO p_mensaje  ;
                       WITH  ;
                       'Documento se Encuentra Anulado'
                    RETURN .F.
               ENDIF
               IF wrk_client =  ;
                  hve_codent
                    wrk_vended = hve_codvnd
                    wrk_fpago = hve_tippag
                    SELECT 5
                    SEEK 'V' +  ;
                         wrk_vended
                    IF FOUND()
                         @ 04, 20  ;
                           SAY  ;
                           wrk_vended
                         @ 04, 30  ;
                           SAY  ;
                           vnd_nombre
                    ELSE
                         @ 04, 30  ;
                           SAY  ;
                           'ERROR'
                    ENDIF
                    SELECT 1
                    SEEK 'TPAG' +  ;
                         wrk_fpago
                    IF FOUND()
                         @ 05, 20  ;
                           SAY  ;
                           tab_destab
                    ELSE
                         @ 05, 20  ;
                           SAY  ;
                           'ERROR'
                    ENDIF
               ELSE
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Cliente No es Igual al Indicado Ant.'
                    RETURN .F.
               ENDIF
          ELSE
               DO p_mensaje WITH  ;
                  'N£mero de Referencia No existe'
               RETURN .F.
          ENDIF
     CASE opcion = 8
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          IF EMPTY(wrk_fecemi)
               DO p_mensaje WITH  ;
                  'No se Aceptan Blancos'
               RETURN .F.
          ENDIF
     CASE opcion = 9
          IF EMPTY(wrk_fecven)
               DO p_mensaje WITH  ;
                  'No se Aceptan Blancos'
               RETURN .F.
          ENDIF
     CASE opcion = 10
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          SELECT 1
          SEEK 'IGV ' + wrk_igv
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Impuesto No Existe'
               RETURN .F.
          ENDIF
          wrk_factor = tab_factor
          @ 07, 25 SAY tab_destab
          CLEAR READ
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          RETURN .T.
ENDCASE
RETURN
*
PROCEDURE referen
ACTIVATE WINDOW refer
@ 00, 00 GET item SIZE 02, 17  ;
  FROM refer VALID endref()
READ
DO CASE
     CASE item = 1
          wrk_codref = 'FACT'
     CASE item = 2
          wrk_codref = 'BOLE'
ENDCASE
DEACTIVATE WINDOW refer
*
PROCEDURE endref
wrk_item = item
*
PROCEDURE posi
SHOW GET arrayitem
IF LASTKEY() = 19 .OR. LASTKEY() =  ;
   4 .OR. LASTKEY() = 9
     RETURN
ENDIF
IF LASTKEY() = 24
     IF m < 5 .AND. p = m
          p = 1
     ELSE
          p = p + 1
     ENDIF
ENDIF
IF LASTKEY() = 5
     IF m < 5
          IF p = 1
               p = m
          ELSE
               p = p - 1
          ENDIF
     ELSE
          p = p - 1
     ENDIF
ENDIF
IF LASTKEY() = 6 .OR. LASTKEY() =  ;
   3
     IF m < 5
          p = m
     ELSE
          p = 4
     ENDIF
ENDIF
IF LASTKEY() = 1 .OR. LASTKEY() =  ;
   18
     p = 1
ENDIF
IF p > 4
     p = 4
ENDIF
IF p < 1
     p = 1
ENDIF
SHOW GET arrayitem
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

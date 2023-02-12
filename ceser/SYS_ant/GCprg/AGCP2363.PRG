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
USE ST_ICLPR ORDER CODIGO
SELECT 7
USE GC_CMV00 ORDER cmv_feinmo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'PROCESOS',  ;
   'NOTA DE CREDITO', 'PROCESO'
@ 2, 63 SAY PROGRAM()
DIMENSION refer[ 2]
refer[ 1] = 'FACTURA'
refer[ 2] = 'BOLETA DE VENTA'
DO WHILE .T.
     ACTIVATE WINDOW cabecera
     CLEAR
     STORE SPACE(10) TO  ;
           wrk_nrodoc,  ;
           wrk_nroref
     STORE SPACE(9) TO wrk_vended
     STORE SPACE(11) TO w_codcli
     STORE SPACE(8) TO wrk_numore
     STORE SPACE(30) TO  ;
           wrk_dircli
     STORE SPACE(4) TO wrk_codref,  ;
           wrk_fpago, wrk_igv,  ;
           wrk_indori, wrk_codmon,  ;
           w_moneda, w_fpago
     STORE DATE() TO wrk_fecemi,  ;
           wrk_fecven
     STORE SPACE(1) TO wrk_tiper
     STORE 0 TO m, wrk_slog,  ;
           wrk_salir, wrk_cierra,  ;
           wrk_factor, w_dolnet,  ;
           w_dolvta, w_doligv,  ;
           w_doltot, w_solgen,  ;
           w_solvta, w_soligv,  ;
           w_dolgen, w_totgen
     STORE 0 TO wrk_doldes,  ;
           wrk_doltot, wrk_dolvta,  ;
           wrk_doligv, wrk_dolnet,  ;
           w_tipcam, wrk_monto
     STORE 1 TO p, item
     STORE ('  /  /    ') TO  ;
           wrk_fecref
     wrk_igv = rge_igv
     @ 00, 00 SAY  ;
       'Tipo de Documento : NOTA DE CREDITO'
     @ 00, 45 SAY  ;
       'Nro. Documento :'
     @ 01, 00 SAY  ;
       'Fecha de Emisi¢n  :'
     @ 01, 45 SAY  ;
       'Fecha de Venc. :'
     @ 02, 00 TO 02, 75
     @ 03, 00 SAY  ;
       'Doc. Referencia   :'
     @ 03, 45 SAY  ;
       'Nro. Doc. Ref. :'
     @ 04, 00 TO 04, 75
     @ 05, 00 SAY  ;
       'C¢digo de Cliente :'
     @ 06, 00 SAY  ;
       'Fecha Doc. Ref.   :'
     @ 06, 45 SAY  ;
       'Moneda         :'
     @ 07, 00 SAY  ;
       'Forma de Pago     :'
     @ 07, 45 SAY  ;
       'Monto          :'
     @ 00, 62 GET wrk_nrodoc  ;
       PICTURE '@!' VALID post(1)  ;
       COLOR SCHEME 8
     @ 01, 20 GET wrk_fecemi  ;
       PICTURE '@D' VALID post(2)  ;
       COLOR SCHEME 8
     @ 01, 62 GET wrk_fecven  ;
       PICTURE '@D' VALID post(3)  ;
       COLOR SCHEME 8
     @ 03, 20 GET wrk_codref  ;
       PICTURE '@!' VALID post(4)  ;
       WHEN ante(4) COLOR SCHEME  ;
       8
     @ 03, 62 GET wrk_nroref  ;
       PICTURE '@!' VALID post(5)  ;
       WHEN ante(5) COLOR SCHEME  ;
       8
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
wrk_busca = 'NCRE'
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
SELECT ge_tab0
SEEK 'NCRE' + wrk_codigo
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
FUNCTION cierra
ON KEY
wrk_totnet = 0
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF  .NOT. FOUND()
     IF LASTKEY() = 19 .OR.  ;
        LASTKEY() = 5
          RETURN
     ENDIF
     DO p_mensaje WITH  ;
        'Tipo de I.G.V. no existe en Tablas'
     RETURN .F.
ELSE
     wrk_facigv = (1 +  ;
                  (tab_factor /  ;
                  100))
ENDIF
FOR a = 1 TO m
     wrk_totnet = wrk_totnet +  ;
                  ctotal(a)
ENDFOR
IF w_moneda = 'SOL '
     w_solgen = wrk_totnet
     w_solvta = ROUND(w_solgen /  ;
                wrk_facigv, 2)
     w_soligv = ROUND(w_solgen -  ;
                w_solvta, 2)
     w_totvta = w_solvta
     w_totigv = w_soligv
     w_totgen = w_solgen
     w_doltot = ROUND(w_solgen /  ;
                ootc2(wrk_fecemi, ;
                'SOL ','DOL ', ;
                '2'), 2)
     w_dolvta = w_doltot /  ;
                wrk_facigv
     w_doligv = w_doltot -  ;
                w_dolvta
     w_dolnet = w_dolvta
ELSE
     w_doltot = wrk_totnet
     w_dolvta = ROUND(w_doltot /  ;
                wrk_facigv, 2)
     w_doligv = w_doltot -  ;
                w_dolvta
     w_dolnet = w_dolvta
     w_totvta = w_dolvta
     w_totigv = w_doligv
     w_totgen = w_doltot
     w_solvta = ROUND(w_dolvta *  ;
                ootc2(wrk_fecemi, ;
                'SOL ','DOL ', ;
                '2'), 2)
     w_solgen = ROUND(w_solvta *  ;
                wrk_facigv, 2)
     w_soligv = ROUND(w_solgen -  ;
                w_solvta, 2)
ENDIF
ACTIVATE WINDOW totales
@ 00, 01 SAY 'Valor de Venta :'
@ 00, 29 SAY 'Valor IGV :'
@ 00, 53 SAY 'Total Gral :'
@ 00, 17 SAY w_totvta PICTURE  ;
  '999,999.99'
@ 00, 41 SAY w_totigv PICTURE  ;
  '999,999.99'
@ 00, 66 SAY w_totgen PICTURE  ;
  '999,999.99'
RETURN
*
PROCEDURE grabar
SELECT gc_hve00
APPEND BLANK
REPLACE hve_tipdoc WITH 'NCRE'
REPLACE hve_nrodoc WITH  ;
        wrk_nrodoc
REPLACE hve_fecdoc WITH  ;
        wrk_fecemi
REPLACE hve_fecvct WITH  ;
        wrk_fecven
REPLACE hve_fechtc WITH  ;
        wrk_fecemi
REPLACE hve_almrec WITH  ;
        rge_codalm
REPLACE hve_codmov WITH 'IFD '
REPLACE hve_tipent WITH 'R'
REPLACE hve_codent WITH w_codcli
REPLACE hve_tidore WITH  ;
        wrk_codref
REPLACE hve_nrdore WITH  ;
        wrk_nroref
REPLACE hve_codmon WITH w_moneda
REPLACE hve_codemi WITH  ;
        rge_punemi
REPLACE hve_tippag WITH w_fpago
REPLACE hve_lispre WITH  ;
        rge_lispre
REPLACE hve_estdoc WITH 'C'
REPLACE hve_indori WITH  ;
        wrk_indori
REPLACE hve_numore WITH  ;
        wrk_numore
REPLACE hve_totnet WITH w_dolnet * - ;
        1
REPLACE hve_totvta WITH w_dolvta * - ;
        1
REPLACE hve_totigv WITH w_doligv * - ;
        1
REPLACE hve_totgen WITH w_doltot * - ;
        1
REPLACE hve_solnet WITH  ;
        (w_solgen) * -1
REPLACE hve_solvta WITH  ;
        (w_solvta) * -1
REPLACE hve_soligv WITH  ;
        (w_soligv) * -1
REPLACE hve_solgen WITH  ;
        (w_solgen) * -1
REPLACE hve_mtocan WITH  ;
        (w_solgen) * -1
REPLACE hve_tipcam WITH w_tipcam
REPLACE hve_fechtc WITH  ;
        wrk_fecemi
REPLACE hve_usuari WITH clave
REPLACE hve_fecha WITH DATE()
REPLACE hve_hora WITH  ;
        SUBSTR(TIME(), 1, 5)
SELECT gc_dve00
FOR a = 1 TO m
     APPEND BLANK
     REPLACE dve_tipdoc WITH  ;
             'NCRE'
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
SELECT gc_hve00
SEEK wrk_codref + wrk_nroref
IF FOUND()
     REPLACE hve_ncredi WITH  ;
             wrk_nrodoc
     REPLACE hve_fecncr WITH  ;
             wrk_fecemi
ENDIF
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
          SELECT gc_hve00
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          wrk_nrodoc = f_ceros(wrk_nrodoc, ;
                       10,2)
          SEEK 'NCRE' +  ;
               wrk_nrodoc
          IF FOUND()
               DO p_mensaje WITH  ;
                  'N£mero de Documento ya Existe'
               RETURN .F.
          ENDIF
     CASE opcion = 2
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
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
          w_tipcam = ootc2(wrk_fecemi, ;
                     'SOL ', ;
                     'DOL ','2')
          IF w_tipcam = -1
               DO p_mensaje WITH  ;
                  'No hay tipo de cambio'
               RETURN .F.
          ENDIF
     CASE opcion = 3
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_fecven)
               DO p_mensaje WITH  ;
                  'No se Aceptan Blancos'
               RETURN .F.
          ENDIF
     CASE opcion = 4
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_codref)
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' +  ;
               wrk_codref
          IF FOUND()
               @ 03, 25 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 15)
          ELSE
               DO p_mensaje WITH  ;
                  'Tipo de Documento no Existe'
               RETURN .F.
          ENDIF
     CASE opcion = 5
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_nroref)
               RETURN .T.
          ENDIF
          wrk_nroref = f_ceros(wrk_nroref, ;
                       10,2)
          @ 03, 62 SAY wrk_nroref
          SELECT gc_hve00
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
               w_fpago = hve_tippag
               wrk_indori = hve_indori
               wrk_numore = hve_numore
               w_fecdoc = hve_fecdoc
               w_moneda = hve_codmon
               w_codcli = hve_codent
               IF w_moneda =  ;
                  'DOL '
                    w_monto = hve_totoim
               ELSE
                    w_monto = hve_mtocan
               ENDIF
               SELECT gc_cli00
               SEEK 'C' +  ;
                    w_codcli
               IF  .NOT. FOUND()
                    SELECT st_iclpr
                    SEEK 'C' +  ;
                         w_codcli
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Cliente No Existe'
                         RETURN .F.
                    ELSE
                         @ 05, 20  ;
                           SAY  ;
                           w_codcli
                         @ 05, 32  ;
                           SAY  ;
                           SUBSTR(noment,  ;
                           1,  ;
                           30)
                    ENDIF
               ELSE
                    @ 05, 20 SAY  ;
                      w_codcli
                    @ 05, 32 SAY  ;
                      SUBSTR(cli_razsoc,  ;
                      1, 30)
               ENDIF
               @ 06, 20 SAY  ;
                 w_fecdoc
               @ 06, 62 SAY  ;
                 w_moneda
               @ 07, 20 SAY  ;
                 w_fpago
               @ 07, 62 SAY  ;
                 w_monto PICTURE  ;
                 '999,999.99'
               SELECT ge_tab0
               SEEK 'MONE' +  ;
                    w_moneda
               IF FOUND()
                    @ 06, 67 SAY  ;
                      SUBSTR(tab_destab,  ;
                      9)
               ELSE
                    @ 06, 67 SAY  ;
                      ''
               ENDIF
               SEEK 'FPAG' +  ;
                    w_fpago
               IF FOUND()
                    @ 07, 25 SAY  ;
                      SUBSTR(tab_destab,  ;
                      6)
               ELSE
                    @ 07, 25 SAY  ;
                      '  '
               ENDIF
          ELSE
               DO p_mensaje WITH  ;
                  'N£mero de Referencia No existe'
               RETURN .F.
          ENDIF
          CLEAR READ
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
PROCEDURE p_borra
ON KEY
aux_swt = .T.
DO WHILE aux_swt
     IF RLOCK()
          pos = ASCAN(deta,  ;
                arrayitem)
          wrk_codpro = producto(pos)
          wrk_unimed = unimed(pos)
          m.hvt_totgen = m.hvt_totgen -  ;
                         candes(pos)
          = ADEL(producto, pos)
          = ADEL(unimed, pos)
          = ADEL(prouni, pos)
          = ADEL(descri, pos)
          = ADEL(cansol, pos)
          = ADEL(preuni, pos)
          = ADEL(pordes, pos)
          = ADEL(candes, pos)
          = ADEL(deta, pos)
          IF p = m
               p = p - 1
          ENDIF
          IF p = 7 .AND. m > 7
               p = 7
          ENDIF
          m = m - 1
          IF m = 0
               DIMENSION deta[ 1]
               deta[ 1] = ' '
               arrayitem = ' '
          ELSE
               DIMENSION deta[ m]
               DIMENSION producto[  ;
                         m]
               DIMENSION unimed[  ;
                         m]
               DIMENSION prouni[  ;
                         m]
               DIMENSION descri[  ;
                         m]
               DIMENSION cansol[  ;
                         m]
               DIMENSION preuni[  ;
                         m]
               DIMENSION pordes[  ;
                         m]
               DIMENSION candes[  ;
                         m]
               wrk_codpro = producto(p)
               wrk_unimed = unimed(p)
               wrk_descri = descri(p)
               wrk_cansol = cansol(p)
               wrk_preuni = preuni(p)
               wrk_pordes = pordes(p)
               wrk_candes = candes(p)
               arrayitem = wrk_codpro +  ;
                           '³' +  ;
                           wrk_unimed +  ;
                           '³' +  ;
                           wrk_descri +  ;
                           '³' +  ;
                           TRANSFORM(wrk_cansol,  ;
                           '@ 999,999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(wrk_preuni,  ;
                           '@ 999,999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(wrk_pordes,  ;
                           '@ 999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(wrk_candes,  ;
                           '@ 9999,999.99' ;
                           )
          ENDIF
          UNLOCK
          EXIT
     ELSE
          aux_swt = f_yesno( ;
                    'Registro bloqueado. Intentar nuevamente ' ;
                    )
     ENDIF
ENDDO
DEACTIVATE WINDOW a4
IF wrk_salex = 0
     CLEAR READ
ENDIF
IF LASTKEY() = 27 .AND. wrk_sale =  ;
   1
     CLEAR READ
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORM0224>'
tit_prg = 'MANTENCION'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUFAC
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' FACTURACION DE PRESUPUESTOS'
ppas = .T.
CLOSE DATABASES
USE IN 18 SHARED gc_par00
SELECT 18
rge_monbas = par_monbas
SELECT 18
USE
wk_igv = 0
USE SHARED ge_tab0 ORDER codigo
LOCATE FOR tab_codpre == 'IGV '
IF FOUND()
     wk_igv = tab_factor
ELSE
     DO error WITH  ;
        '**No Definido el IGV**'
     RETURN
ENDIF
USE
wk_valpari = 0
error = .F.
SELECT 17
USE SHARED gc_cmv00 ORDER codigo
wk_fpari = DATE()
DO fech
IF error = .T.
     ppas = .F.
ELSE
     ppas = .T.
ENDIF
DIMENSION wk_obspre( 06)
SET CURSOR ON
DO WHILE ppas
     wk_coddoc = '    '
     sw_algo = .F.
     con_eli = 0
     wk_nomcli = '  '
     wk_numdoc = 0
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     STORE CTOD('  /  /  ') TO  ;
           wk_fpari, wk_fevenga,  ;
           wk_fecven
     wk_fevenga = DATE()
     wk_feven = DATE()
     wk_codpag = '    '
     wk_monto = 0
     wk_fle = 0
     wk_otro = 0
     sw_algo = .F.
     @ 3, 1 CLEAR TO 14, 78
     DO eli_garan
     IF sw_algo = .T.
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'bbb'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'bbb',  ;
             'ESC'
          SELECT 1
          GOTO TOP
          DO WHILE  .NOT. EOF()
               wk_numdoc = 0
               IF SUBSTR(indest,  ;
                  1, 1) = 'û'
                    DO esc_indica  ;
                       WITH 1,  ;
                       'AYU',  ;
                       'BBB',  ;
                       'BBB',  ;
                       'bbb'
                    DO esc_indica  ;
                       WITH 2,  ;
                       'BBB',  ;
                       'BBB',  ;
                       'ign',  ;
                       'ESC'
                    wk_coddoc = '    '
                    num_ord = numdoc
                    wk_numsol = numsol
                    FOR i = 1 TO  ;
                        5
                         wk_obspre(  ;
                          i) =  ;
                          SUBSTR(MLINE(observ,  ;
                          i), 1,  ;
                          45)
                         wk_obspre(  ;
                          i) =  ;
                          wk_obspre(i) +  ;
                          SPACE(45 -  ;
                          LEN(wk_obspre(i)))
                    ENDFOR
                    wk_obspre( 6) =  ;
                             SPACE(33)
                    STORE 0 TO  ;
                          wk_fle,  ;
                          wk_otro,  ;
                          wk_monto,  ;
                          monfin
                    @ 6, 3 TO 10,  ;
                      75
                    @ 7, 48 SAY  ;
                      'Presupuesto N§..: ' +  ;
                      ALLTRIM(num_ord)
                    @ 7, 4 SAY  ;
                      'Fecha de Paridad......:'
                    @ 7, 27 SAY  ;
                      wk_fpari
                    @ 8, 4 SAY  ;
                      'Monto Facturacion.US$.:'
                    @ 9, 4 SAY  ;
                      'Glosa.................:'
                    @ 8, 27 GET  ;
                      wk_monto  ;
                      PICTURE  ;
                      '999,999.99'
                    @ 9, 27 GET  ;
                      wk_obspre(  ;
                      6) PICTURE  ;
                      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                    READ
                    IF LASTKEY() <>  ;
                       27 .AND.  ;
                       LASTKEY() <> - ;
                       9
                         STORE 0  ;
                               TO  ;
                               wk_repues,  ;
                               wk_mano,  ;
                               wk_fle,  ;
                               wk_otro,  ;
                               wk_totdes
                         wk_subtot =  ;
                          ROUND(wk_monto *  ;
                          wk_valpari,  ;
                          2)
                         wk_totnet =  ;
                          ROUND(wk_monto *  ;
                          wk_valpari,  ;
                          2)
                         wk_totigv =  ;
                          ROUND((wk_totnet *  ;
                          (wk_igv /  ;
                          100)),  ;
                          2)
                         wk_totbru =  ;
                          ROUND((wk_totnet +  ;
                          wk_totigv),  ;
                          2)
                         monfin =  ;
                          ROUND(wk_totbru,  ;
                          2)
                         DO muestra
                    ENDIF
                    IF LASTKEY() = - ;
                       9
                         EXIT
                    ENDIF
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
               ENDIF
               SELECT 1
               SKIP
          ENDDO
     ENDIF
     IF LASTKEY() = -9
          LOOP
     ENDIF
     IF LASTKEY() = 27
          EXIT
     ENDIF
ENDDO
USE SHARED st_iparg
REPLACE sys_lptfac WITH '0'
USE
DO saca_win
@ 24, 69 SAY SPACE(10)
ON KEY LABEL F6
SET CURSOR OFF
SELECT 1
USE
SELECT 2
USE
SELECT 3
USE
erase &es4
CLOSE DATABASES
RETURN
*
PROCEDURE busca
wk_numdoc = 1
SELECT 4
USE SHARED st_hfact ORDER CODIGO
GOTO BOTTOM
DO WHILE  .NOT. BOF()
     IF coddoc = 'FACT'
          wk_numdoc = VAL(numdoc) +  ;
                      1
          GOTO TOP
     ENDIF
     SKIP -1
ENDDO
SELECT 4
USE
RETURN
*
PROCEDURE actual
IF wk_numdoc = 0
     DO busca
ENDIF
DO mensa WITH  ;
   'Grabando   FACT N§ '+ ;
   STR(wk_numdoc, 8), 'COLO'
SELECT 11
USE st_iredo
APPEND BLANK
REPLACE indodo WITH 'PRE '
REPLACE numodo WITH num_ord
REPLACE indddo WITH 'FACT'
REPLACE numddo WITH STR(wk_numdoc,  ;
        8)
USE
SELECT 11
USE SHARED st_ispre ORDER CODIGO
SEEK num_ord
IF FOUND()
     REPLACE indest WITH 'P   '
     wk_aux = ''
     FOR i = 1 TO 5
          wk_aux = wk_aux +  ;
                   wk_obspre(i) +  ;
                   CHR(10) +  ;
                   CHR(13)
     ENDFOR
     wk_aux = wk_aux +  ;
              wk_obspre(6) +  ;
              'FACT' +  ;
              STR(wk_numdoc, 8) +  ;
              CHR(10) + CHR(13)
     REPLACE observ WITH wk_aux
ELSE
     WAIT WINDOW  ;
          'Error en Base st_ispre.dbf'
ENDIF
SELECT 11
USE
SELECT 3
USE st_hfact ORDER CODIGO
APPEND BLANK
REPLACE coddoc WITH 'FACT'
REPLACE numdoc WITH STR(wk_numdoc,  ;
        8)
REPLACE numore WITH num_ord
REPLACE fecemi WITH DATE()
REPLACE fecven WITH wk_fecven
REPLACE fecgar WITH wk_fevenga
REPLACE codpag WITH wk_codpag
REPLACE cosrep WITH wk_repues
REPLACE cosmob WITH wk_mano
REPLACE subtot WITH wk_subtot
REPLACE totdes WITH wk_totdes
REPLACE totnet WITH wk_totnet
REPLACE totigv WITH wk_totigv
REPLACE totbru WITH wk_totbru
REPLACE totfle WITH wk_fle
REPLACE tototr WITH wk_otro
REPLACE mtocan WITH monfin
SELECT 3
USE
SELECT 11
USE SHARED st_estad ORDER  ;
    EST_NUMORD
GOTO TOP
SEEK num_ord
IF FOUND()
     REPLACE coddoc WITH 'FACT'
     REPLACE numfabo WITH  ;
             wk_numdoc
ENDIF
USE
DO mensa WITH  ;
   'Grabando   FACT N§ '+ ;
   STR(wk_numdoc, 8), 'SACA'
RETURN
*
PROCEDURE eli_garan
DIMENSION op( 2)
op( 1) = 'Ppto. en Garant¡a '
op( 2) = 'Ppto. fuera Garan.'
DEFINE POPUP ayu1 FROM 0, 0 TO 3,  ;
       21 SHADOW
FOR i = 1 TO 2
     DEFINE BAR i OF ayu1 PROMPT  ;
            op(i)
ENDFOR
ON SELECTION POPUP ayu1 do choice with;
prompt()
ACTIVATE POPUP ayu1 NOWAIT
FOR i = 0 TO 7
     MOVE POPUP ayu1 TO i, 0
ENDFOR
FOR i = 0 TO 14
     MOVE POPUP ayu1 TO 7, i * 2
ENDFOR
opcion = SPACE(20)
ACTIVATE POPUP ayu1
DEACTIVATE POPUP ayu1
IF LASTKEY() == 27
     ppas = .F.
ENDIF
DO mensa WITH  ;
   'Un momento, por favor',  ;
   'COLO'
USE st_ispre
es3 = da_nombre()
copy struc extended to &es3 fields numdoc,fecemi,codent,codmar,codmod,numser,indest,indori,numsol,observ
use &es3    
APPEND BLANK
REPLACE field_name WITH 'DESCRI',  ;
        field_type WITH 'C',  ;
        field_len WITH 30
USE
es4 = da_nombre()
create &es4 from &es3
erase  &es3
USE
SELECT 1
use &es4 exclusive    
IF opcion == 'Ppto. en Garant¡a '
     DO con_garan
ENDIF
IF opcion == 'Ppto. fuera Garan.'
     DO sin_garan
ENDIF
DO mensa WITH  ;
   'Un momento, por favor',  ;
   'SACA'
IF LASTKEY() <> 27 .AND.  ;
   LASTKEY() <> -9
     GOTO TOP
     IF EOF()
          USE
          DO error WITH  ;
             '**No Existen Presupuestos Para Esta Selecci¢n**'
          sw_algo = .F.
     ELSE
          sw_algo = .T.
          campo = 'subst(indest,1,2)+numdoc+" "+dtoc(fecemi)+" "+" "+codent+" "+descri'
          mensaje = 'N§ Ppto.  Ingreso  Cliente    Descripcion                          '
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'SEL'
          DO esc_indica WITH 2,  ;
             'MBV', 'ESP', 'BBB',  ;
             'ESC'
          define popup ayu0 from 0,1 to;
08,72 promp field &campo shadow title;
mensaje
          ON SELECTION POPUP ayu0 do choice0
          ACTIVATE POPUP ayu0  ;
                   NOWAIT
          FOR i = 1 TO 04
               MOVE POPUP ayu0 BY  ;
                    1, 0
          ENDFOR
          ACTIVATE POPUP ayu0
          DEACTIVATE POPUP ayu0
     ENDIF
ENDIF
RETURN
*
PROCEDURE con_garan
index on codent TAG &es4
APPEND FROM st_ispre FOR indest ==  ;
       'V   ' .AND. indori ==  ;
       'S'
GOTO TOP
DO WHILE  .NOT. EOF()
     wk_clave = codmar + codmod +  ;
                numser
     SELECT 2
     USE SHARED st_iseri ORDER  ;
         SER_CODMAR
     SEEK '&wk_clave'
     wk_codent = 'P' + codent
     USE SHARED st_iclpr ORDER  ;
         CODIGO
     SEEK '&wk_codent'
     wk_noment = noment
     USE
     SELECT 1
     REPLACE codent WITH  ;
             SUBSTR(wk_codent,  ;
             2)
     REPLACE descri WITH  ;
             wk_noment
     REPLACE indest WITH SPACE(4)
     SKIP
ENDDO
RETURN
*
PROCEDURE sin_garan
index on numdoc TAG &es4    
APPEND FROM st_ispre FOR indest ==  ;
       'V   ' .AND. (indori ==  ;
       'FGAR' .OR. indori ==  ;
       'FGAR')
GOTO TOP
DO WHILE  .NOT. EOF()
     wk_clave = 'C' + codent
     SELECT 2
     USE SHARED st_iclpr ORDER  ;
         CODIGO
     SEEK '&wk_clave'
     wk_noment = noment
     USE
     SELECT 1
     REPLACE descri WITH  ;
             wk_noment
     REPLACE indest WITH SPACE(4)
     SKIP
ENDDO
RETURN
*
PROCEDURE choice
PARAMETER num
opcion = num
DEACTIVATE POPUP ayu1
RETURN
*
PROCEDURE choice0
IF LASTKEY() == 32
     IF SUBSTR(indest, 1, 1) =  ;
        'û'
          REPLACE indest WITH  ;
                  SPACE(4)
     ELSE
          REPLACE indest WITH  ;
                  'û   '
     ENDIF
     KEYBOARD '{DNARROW}'
ENDIF
IF LASTKEY() == 13
     DEACTIVATE POPUP ayu0
ENDIF
RETURN
*
FUNCTION vali_fe
PARAMETER toc
IF EMPTY(toc)
     IF VARREAD() = 'WK_FECVEN'
          wk_fecven = DATE()
     ELSE
          wk_fevenga = DATE()
     ENDIF
ENDIF
RETURN .T.
*
FUNCTION vali_cero
PARAMETER cero
IF cero < 0
     DO error WITH  ;
        '** Debe Ser Mayor que 0 **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION vali_mon
PARAMETER cero
boleano = 0
DO control WITH boleano
IF boleano = 1
     boleano = 0
     RETURN 0
ENDIF
IF cero <= 0
     DO error WITH  ;
        '** Debe Ser Mayor que 0 **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
RETURN .T.
*
PROCEDURE muestra
save_sele = SELECT()
confi = ' '
STORE 0 TO wk_repues, tot_titore
SELECT 14
USE SHARED st_ispre ORDER CODIGO
SEEK num_ord
IF FOUND()
     wk_codcli = codent
     wk_marca = codmar
     wk_modelo = codmod
     wk_serie = numser
ELSE
     WAIT WINDOW  ;
          'Error en Base st_ispre.dbf'
ENDIF
SELECT 14
USE
SELECT 6
USE SHARED st_iclpr ORDER CODIGO
GOTO TOP
llave = 'C' + wk_codcli
SEEK llave
IF FOUND()
     wk_nomcli = ' '
     wk_nomcli = noment
     wk_dir = SUBSTR(nomcal, 1,  ;
              20) + SUBSTR(nomciu,  ;
              1, 20)
ENDIF
SELECT 6
USE
@ 3, 1 CLEAR TO 15, 79
@ 4, 2 SAY 'E N C A B E Z A D O '
@ 5, 2 SAY  ;
  'N§ de Presupuesto..: ' +  ;
  num_ord
@ 6, 2 SAY  ;
  'Cliente............: ' +  ;
  SUBSTR(wk_nomcli, 1, 20)
@ 7, 2 SAY  ;
  'C¢digo Cliente.....: ' +  ;
  wk_codcli
@ 8, 2 SAY  ;
  'Marca..............: ' +  ;
  wk_marca
@ 09, 2 SAY  ;
  'Modelo.............: ' +  ;
  wk_modelo
@ 10, 2 SAY  ;
  'N£mero de Serie....: ' +  ;
  wk_serie
@ 11, 2 SAY  ;
  'Fecha de Emisi¢n...: ' +  ;
  DTOC(DATE())
@ 12, 2 SAY  ;
  'Fecha Gar.Venc.Rep.: ' +  ;
  DTOC(wk_fevenga)
@ 4,43 say "   T O T A L E S      (Soles) ";
color &color3
sale = 0
ON KEY LABEL f10 do f10s
@ 5, 43 SAY 'Costo En Repuesto.:' +  ;
  TRANSFORM(wk_repues,  ;
  '99,999,999.99')
@ 6, 43 SAY 'Costo Mano de Obra:' +  ;
  TRANSFORM(wk_mano,  ;
  '99,999,999.99')
@ 7, 43 SAY 'Sub-total.........:' +  ;
  TRANSFORM(wk_subtot,  ;
  '99,999,999.99')
@ 8, 43 SAY 'Total Descuento...:' +  ;
  TRANSFORM(wk_totdes,  ;
  '99,999,999.99')
@ 9, 43 SAY 'Total Neto........:' +  ;
  TRANSFORM(wk_totnet,  ;
  '99,999,999.99')
@ 10, 43 SAY  ;
  'Toal I.G.V........:' +  ;
  TRANSFORM(wk_totigv,  ;
  '99,999,999.99')
@ 11, 43 SAY  ;
  'Total Bruto.......:' +  ;
  TRANSFORM(wk_totbru,  ;
  '99,999,999.99')
@ 12, 43 SAY  ;
  'Total Flete.......:' +  ;
  TRANSFORM(wk_fle,  ;
  '99,999,999.99')
@ 13, 43 SAY  ;
  'Total Otros.......:' +  ;
  TRANSFORM(wk_otro,  ;
  '99,999,999.99')
@ 14, 43 SAY  ;
  'Monto Cancelado...:'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
@ 14, 62 GET monfin PICTURE  ;
  '99,999,999.99' VALID  ;
  vali_mon(monfin)
READ
ON KEY LABEL f10
IF sale = 0 .AND. LASTKEY() <> 27
     DO esc_indica WITH 1, 'AYU',  ;
        'GRA', 'IMP', 'IGN'
     DO esc_indica WITH 2, 'bbb',  ;
        'bbb', 'BBB', 'ESC'
     = INKEY(0)
     CLEAR TYPEAHEAD
     DO WHILE LASTKEY()<>27 .AND.  ;
        LASTKEY()<>-6 .AND.  ;
        LASTKEY()<>-1 .AND.  ;
        LASTKEY()<>-9
          = INKEY(0)
     ENDDO
     eror_imp = .F.
     wk_obli2 = .F.
     IF LASTKEY() = -6
          DO status_imp
          DO imprime
          wk_obli2 = .T.
     ENDIF
     IF LASTKEY() = -1 .OR.  ;
        wk_obli2 .AND. eror_imp =  ;
        .F.
          DO actual
     ENDIF
ENDIF
SELECT (save_sele)
@ 4, 1 CLEAR TO 14, 79
RETURN
*
PROCEDURE imprime
sw_impre = 0
DO impresora WITH sw_impre
IF sw_impre <> 1
     eror_imp = .T.
     RETURN
ENDIF
lin = 12
IF wk_numdoc = 0
     DO busca
ENDIF
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
? CHR(18)
cuenta_lin = 0
@ 1, 2 SAY DTOC(DATE())
@ 1, 60 SAY STR(wk_numdoc)
@ 3, 15 SAY SUBSTR(wk_nomcli, 1,  ;
  20)
@ 3, 55 SAY wk_codcli
@ 5, 10 SAY wk_dir
@ 7, 65 SAY num_ord
@ lin, 5 SAY wk_obspre(6) PICTURE  ;
  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
STORE 0 TO repu, iva, des, neto,  ;
      total
iva = wk_totigv
neto = wk_totnet
total = monfin
@ 25, 10 SAY TRANSFORM(wk_repues,  ;
  '99,999,999.99')
@ 25, 67 SAY TRANSFORM(wk_totnet,  ;
  '99,999,999.99')
@ 26, 10 SAY TRANSFORM(wk_mano,  ;
  '99,999,999')
@ 26, 68 SAY TRANSFORM(wk_totigv,  ;
  '9,999,999.99')
@ 27, 10 SAY TRANSFORM(wk_totdes,  ;
  '99,999,999.99')
@ 27, 67 SAY wk_totnet +  ;
  wk_totigv PICTURE  ;
  '99,999,999.99'
@ 28, 67 SAY TRANSFORM(monfin,  ;
  '99,999,999.99')
@ 29, 67 SAY TRANSFORM(monfin,  ;
  '99,999,999.99')
@ 23, 1 SAY ' '
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
CLEAR TYPEAHEAD
USE SHARED st_iparg
REPLACE sys_lptfac WITH '0'
USE
RETURN
*
PROCEDURE fech
SELECT 17
SET ORDER TO codigo_2
SEEK DTOC(wk_fpari) + '1' +  ;
     'SOL ' + 'DOL '
IF  .NOT. FOUND()
     DO error WITH  ;
        '**No Existe Tipo de Cambio para esta Fecha**'
     error = .T.
ELSE
     wk_valpari = cmv_tipcav
ENDIF
RETURN
*
PROCEDURE f10s
CLEAR READ
sale = 1
RETURN
*
PROCEDURE status_imp
wk_lptfac = '1'
DO WHILE wk_lptfac='1'
     USE SHARED st_iparg
     wk_lptfac = sys_lptfac
     IF wk_lptfac = '1'
          DO mensa WITH  ;
             '**Impresora Ocupada, FACTURANDO**',  ;
             'COLO'
     ENDIF
     USE
ENDDO
DO mensa WITH  ;
   '**Impresora Ocupada, FACTURANDO**',  ;
   'SACA'
USE SHARED st_iparg
REPLACE sys_lptfac WITH '1'
USE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

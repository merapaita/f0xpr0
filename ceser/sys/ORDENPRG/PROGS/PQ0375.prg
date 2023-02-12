*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER opc
titu1 = 'CONSULTA'
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
ON KEY LABEL F6 do ayuda12
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, titu1
DO saycenter WITH 2,  ;
   ' DOCUMENTO DE VENTA '
DO esc_modo WITH 'I'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
@ 6, 10 CLEAR TO 10, 65
@ 6, 10 TO 10, 65
SAVE SCREEN TO wk_panta
USE
wk_numfac = 0
base1 = da_nombre()
SELECT 6
Create table &base1 (pas_codpro C(15),pas_cantid;
N(12,2),pas_valor N(12,2),pas_total N(12,2));

USE
DIMENSION wk_observ( 06)
ppal = .T.
DO WHILE ppal
     STORE '  ' TO wk_numord,  ;
           wk_codcli, wk_serie,  ;
           wk_nomcli
     STORE CTOD('  /  /  ') TO  ;
           wk_fecemi, wk_fecven,  ;
           wk_fecvga
     STORE '    ' TO wk_codpag,  ;
           sw_viene
     STORE 0 TO wk_cosrep,  ;
           wk_cosmob, wk_subtot,  ;
           wk_totdes, wk_totnet,  ;
           wk_totigv
     STORE 0 TO wk_totbru,  ;
           wk_totfle, wk_tototr,  ;
           wk_mtocan, wk_numfac
     sw_hay = .F.
     RESTORE SCREEN FROM wk_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'bbb'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'ign', 'ESC'
     efecin = 1
     wk_numero = 0
     wk_coddoc = '    '
     wk_var = 0
     wk_var = 1
     @ 7, 20 SAY  ;
       'C¢digo de Documento  : '  ;
       GET wk_coddoc PICTURE  ;
       '!!!!' VALID  ;
       vali_docu(wk_coddoc)
     @ 8, 20 SAY  ;
       'Numero Documento     : '  ;
       GET wk_numfac PICTURE  ;
       '99999999' VALID  ;
       vali_fact(wk_coddoc, ;
       wk_numfac) WHEN  ;
       colocaf6()
     SET CURSOR ON
     READ
     SET CURSOR OFF
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               CLEAR GETS
               ppal = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               CLEAR GETS
               LOOP
     ENDCASE
     IF LASTKEY() <> 27
          IF sw_hay = .T.
               ON KEY LABEL F4 DO TOTALES
               DO muestra
          ENDIF
     ENDIF
ENDDO
erase  &base1
CLOSE DATABASES
ON KEY LABEL F6
ON KEY LABEL F10
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DO saca_win
@ 24, 69 SAY SPACE(15)
CLOSE DATABASES
RETURN
*
PROCEDURE ayuda12
ON KEY LABEL F6
ON KEY LABEL F8
IF VARREAD() == 'WK_NUMFAC'
     USE SHARED GC_HVE00 ORDER  ;
         CODIGO
     SET FILTER TO hve_tipdoc = wk_coddoc
     campoa = '" "+HVE_numdoc+"   "+dtoc(HVE_fecemi)+"   "+HVE_TIDORE+"   "+HVE_NRDORE+"  "+HVE_TIPPAG'
     doc = wk_coddoc
     DO ayuda6 WITH campoa, doc
     SET FILTER TO
     USE
ENDIF
ON KEY LABEL F6 do ayuda12
ON KEY LABEL F8 do solicitud
RETURN
*
FUNCTION vali_fact
PARAMETER cod, num
cod = cod + STR(num, 8)
IF LEN(TRIM(cod)) == 0
     DO error WITH  ;
        '** C¢digo No Ingresado **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
SELECT 3
USE SHARED st_hfact ORDER CODIGO
SEEK cod
IF FOUND()
     sw_hay = .T.
     wk_numord = numore
     wk_fecemi = fecemi
     wk_fecven = fecven
     wk_fecvga = fecgar
     wk_codpag = codpag
     wk_cosrep = cosrep
     wk_cosmob = cosmob
     wk_subtot = subtot
     wk_totdes = totdes
     wk_totnet = totnet
     wk_totigv = totigv
     wk_totbru = totbru
     wk_totfle = totfle
     wk_tototr = tototr
     wk_mtocan = mtocan
     SELECT 5
     USE SHARED st_iredo ORDER  ;
         REL_INDICA
     SEEK cod
     IF FOUND()
          IF indodo = 'PRE '
               sw_viene = 'PRE '
               SELECT 4
               USE SHARED  ;
                   st_ispre ORDER  ;
                   CODIGO
               SEEK wk_numord
               IF FOUND()
                    wk_codcli = codent
                    wk_serie = numser
                    FOR i = 1 TO  ;
                        6
                         wk_observ(  ;
                          i) =  ;
                          SUBSTR(MLINE(observ,  ;
                          i), 1,  ;
                          45)
                         wk_observ(  ;
                          i) =  ;
                          wk_observ(i) +  ;
                          SPACE(45 -  ;
                          LEN(wk_observ(i)))
                    ENDFOR
               ENDIF
               SELECT 4
               USE
          ELSE
               sw_viene = 'ORD '
               SELECT 4
               USE SHARED  ;
                   st_iorep ORDER  ;
                   CODIGO
               SEEK wk_numord
               IF FOUND()
                    wk_codcli = codent
                    wk_serie = numser
               ENDIF
               SELECT 4
               USE
          ENDIF
     ENDIF
     SELECT 5
     USE
     SELECT 3
     USE
     SELECT 7
     USE SHARED st_iclpr ORDER  ;
         CODIGO
     llave = 'C' + wk_codcli
     SEEK llave
     wk_nomcli = noment
     SELECT 7
     USE
     RETURN .T.
ELSE
     SELECT 3
     USE
     DO error WITH  ;
        '** C¢digo No Existe **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
DO sacaf6
RETURN .T.
*
PROCEDURE brau_fact
DEFINE WINDOW fact FROM 9, 13 TO  ;
       16, 68 SHADOW
ACTIVATE WINDOW fact
SELECT 6
use &base1 exclusive
BROWSE FIELDS pas_codpro :W = .F.  ;
       :H = ' C¢digo', pas_cantid  ;
       :W = .F. :H = 'Cantidad'  ;
       :P = '99999.99', pas_valor  ;
       :W = .F. :H = '   Valor'  ;
       :P = '999,999.99',  ;
       pas_total :W = .F. :H =  ;
       '   Total' :P =  ;
       '999,999.99' NOMENU IN  ;
       fact
SELECT 6
ZAP
USE
DEACTIVATE WINDOW fact
RELEASE WINDOW fact
RETURN
*
PROCEDURE muestra
DO esc_modo WITH 'C'
DO esc_indica WITH 1, 'AYU',  ;
   'TOT', 'IGN', 'bbb'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'bbb', 'ESC'
@ 03, 1 CLEAR TO 14, 79
@ 3, 2 SAY  ;
  'C¢digo de Documento.:' +  ;
  wk_coddoc
@ 3, 40 SAY  ;
  'N£mero de Documento.:' +  ;
  STR(wk_numfac)
@ 4, 2 SAY  ;
  'C¢digo Cliente......:' +  ;
  wk_codcli + '  ' +  ;
  SUBSTR(wk_nomcli, 1, 25)
@ 5, 2 SAY  ;
  'N£mero de Serie.....:' +  ;
  wk_serie
@ 5, 40 SAY  ;
  'C¢digo de Pago......:' +  ;
  wk_codpag
@ 6, 2 SAY  ;
  'Fecha de Emisi¢n....:' +  ;
  DTOC(DATE())
@ 6, 40 SAY  ;
  'Fecha Gar.Venc.Rep..:' +  ;
  DTOC(wk_fecvga)
IF sw_viene = 'ORD '
     SELECT 6
     use &base1  exclusive   
     SELECT 5
     USE SHARED st_dfact ORDER  ;
         CODIGO
     cod = wk_coddoc +  ;
           STR(wk_numfac, 8)
     SEEK cod
     IF FOUND()
          DO WHILE  .NOT. EOF()
               IF VAL(numdoc) =  ;
                  wk_numfac .AND.  ;
                  coddoc =  ;
                  wk_coddoc
                    wk_codpro = codpro
                    wk_canpro = cantid
                    wk_valpro = valpre
                    wk_total = totite
                    SELECT 6
                    APPEND BLANK
                    REPLACE pas_codpro  ;
                            WITH  ;
                            wk_codpro,  ;
                            pas_valor  ;
                            WITH  ;
                            wk_valpro
                    REPLACE pas_cantid  ;
                            WITH  ;
                            wk_canpro,  ;
                            pas_total  ;
                            WITH  ;
                            wk_total
               ENDIF
               SELECT 5
               SKIP
          ENDDO
     ENDIF
     SELECT 6
     USE
     DO brau_fact
     SELECT 5
     USE
ELSE
     @ 8, 35 SAY 'DETALLE'
     @ 9, 14 TO 11, 61
     @ 10, 15 SAY wk_observ(6)
     wk_inkey = INKEY(0)
     IF wk_inkey = -3
          DO totales
     ENDIF
     DO WHILE  .NOT.  ;
        (STR(wk_inkey, 2)$ ;
        '27,-9')
          wk_inkey = INKEY(0)
          IF wk_inkey = -3
               DO totales
          ENDIF
     ENDDO
     ON KEY LABEL f4
ENDIF
RETURN
*
PROCEDURE totales
ON KEY LABEL F4
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'bbb'
DEFINE WINDOW tot FROM 9, 2 TO 16,  ;
       74
ACTIVATE WINDOW tot
DO saycenter WITH 0,  ;
   'T O T A L E S'
@ 1, 2 SAY 'Cos.Repu.:' +  ;
  TRIM(TRANSFORM(wk_cosrep,  ;
  '99,999,999.99'))
@ 1, 40 SAY 'Cos.M.Ob.:' +  ;
  TRIM(TRANSFORM(wk_cosmob,  ;
  '99,999,999.99'))
@ 2, 2 SAY 'Subt.O.R.:' +  ;
  TRIM(TRANSFORM(wk_subtot,  ;
  '99,999,999.99'))
@ 2, 40 SAY 'Tot. Desc:' +  ;
  TRIM(TRANSFORM(wk_totdes,  ;
  '99,999,999.99'))
@ 3, 2 SAY 'Tot. Neto:' +  ;
  TRIM(TRANSFORM(wk_totnet,  ;
  '99,999,999.99'))
@ 3, 40 SAY 'Tot. IGV.:' +  ;
  TRIM(TRANSFORM(wk_totigv,  ;
  '99,999,999.99'))
@ 4, 2 SAY 'Tot. Brut:' +  ;
  TRIM(TRANSFORM(wk_totbru,  ;
  '99,999,999.99'))
@ 4, 40 SAY 'Tot. Flet:' +  ;
  TRIM(TRANSFORM(wk_totfle,  ;
  '99,999,999.99'))
@ 5, 2 SAY 'Tot. Otro:' +  ;
  TRIM(TRANSFORM(wk_tototr,  ;
  '99,999,999.99'))
@ 5, 40 SAY 'Mon. Canc:' +  ;
  TRIM(TRANSFORM(wk_mtocan,  ;
  '99,999,999.99'))
= INKEY(0)
DEACTIVATE WINDOW tot
RELEASE WINDOW tot
CLEAR TYPEAHEAD
ON KEY LABEL F4 DO TOTALES
DO esc_indica WITH 1, 'AYU',  ;
   'TOT', 'IGN', 'bbb'
RETURN
*
FUNCTION vali_docu
PARAMETER doc
IF doc <> 'FACT' .AND. doc <>  ;
   'BOL '
     DO error WITH  ;
        '**Debe ser [FACT]ura ¢ [BOL]eta **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ELSE
     RETURN .T.
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER opc
tit_prg = ' MANTENCION '
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE() COLOR SCHEME 8
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ACTUALIZACION DE VENTAS - MANUAL '
ppas = .T.
wrk_despag = SPACE(30)
wk_codcli = SPACE(9)
wk_crea = 1
wk_igv = 0
numvta = 0
STORE SPACE(8) TO wrk_numdoc
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED gc_pro00 ORDER codigo
SELECT 3
USE SHARED gc_par00
rge_monbas = par_monbas
SELECT 4
USE SHARED gc_cmv00 ORDER  ;
    codigo_2
SELECT 5
USE SHARED st_isrep ORDER CODIGO
SELECT 6
USE SHARED ST_IOREP AGAIN ORDER  ;
    CODIGO
SELECT 7
USE SHARED st_iclpr ORDER CODIGO
SELECT 8
USE SHARED ST_IPREP ORDER  ;
    REP_NUMORD
SELECT 9
USE SHARED st_idped ORDER codigo
SELECT 10
USE SHARED st_mvord
SELECT 11
USE SHARED st_iparg
SELECT 12
USE SHARED st_iredo
SELECT 13
USE SHARED GC_HVE00 ORDER CODIGO
SELECT 14
USE SHARED GC_DVE00 ORDER CODIGO
SELECT 15
USE SHARED st_estad ORDER  ;
    EST_NUMORD
SELECT ge_tab0
wrk_varbus = '"IGV " + "IGV "'
SEEK &wrk_varbus
IF FOUND()
     wk_igv = tab_factor
     wrk_facigv = tab_factor /  ;
                  100
ELSE
     DO error WITH  ;
        '**No Definido el IGV**'
     CLOSE DATABASES
     RETURN
ENDIF
wrk_tipcam = 0
error = .F.
wk_fpari = DATE()
SELECT gc_cmv00
SEEK DTOC(wk_fpari) + '1' +  ;
     'SOL ' + 'DOL '
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** No Existe Tipo de Cambio de esta Fecha ***'
     error = .T.
ENDIF
wrk_tipcam = cmv_tipcav
IF error = .T.
     ppas = .F.
ELSE
     ppas = .T.
ENDIF
SET CURSOR ON
DO WHILE ppas
     ON KEY LABEL F6 DO AYUFAC
     STORE 0 TO sol_cosrep,  ;
           sol_cosmob, sol_cosfle,  ;
           sol_subtot, sol_descue,  ;
           wrk_toacta, wrk_dolac
     STORE 0 TO sol_totvta,  ;
           sol_totigv, sol_totgen,  ;
           wrk_cont01, wrk_cont02,  ;
           wk_flete
     STORE SPACE(8) TO wrk_numsol
     DIMENSION nrodoc[ 1],  ;
               montos[ 1],  ;
               tipdoc[ 1]
     wk_coddoc = SPACE(4)
     con_eli = 0
     wk_nomcli = '  '
     wk_numdoc = 0
     wk_codpag = '    '
     STORE 0 TO wk_monto, abono,  ;
           wrk_numord, wk_abono,  ;
           wk_subtot, wk_cosrep,  ;
           wk_cosmob
     STORE 0 TO wk_total,  ;
           wk_totvta, wk_totgen,  ;
           wk_totigv
     STORE DATE() TO wk_fecven,  ;
           wk_fpari
     STORE (DATE() + empre7) TO  ;
           wk_fevenga
     STORE .F. TO eror_imp,  ;
           sw_algo
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'MBV', 'BUS', 'SEL'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'bbb', 'ESC'
     @ 03, 01 CLEAR TO 14, 78
     @ 03, 17 TO 12, 55
     @ 4, 20 SAY  ;
       'Orden de Reparaci¢n..:'
     @ 4, 44 GET wrk_numord  ;
       PICTURE '99999999'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     wrk_numord = STR(wrk_numord,  ;
                  8)
     SELECT st_iorep
     SEEK wrk_numord
     IF  .NOT. FOUND()
          DO error WITH  ;
             'Nro. de Orden No Existe'
          LOOP
     ELSE
          IF indest = 'F' .OR.  ;
             indest = 'B'
               DO error WITH  ;
                  '*** La Orden se encuentra Facturada ***'
               LOOP
          ENDIF
          IF indest = 'N'
               DO error WITH  ;
                  '*** La Orden Se encuentra Anulada ***'
               LOOP
          ENDIF
          IF indest <> 'C'
               DO error WITH  ;
                  '*** La Orden No Paso Control de Calidad ***'
               LOOP
          ENDIF
          IF indori = 'GARA'
               DO error WITH  ;
                  '*** No se aceptan ordenes en Garant¡a ***'
               LOOP
          ENDIF
          IF indori = 'GREC'
               DO error WITH  ;
                  '*** No se aceptan ordenes en Reclamo  ***'
               LOOP
          ENDIF
          wrk_numsol = numsol
          wk_mano = cosmob
          wk_cosfle = flete
          wrk_indori = indori
          SELECT st_isrep
          SEEK wrk_numsol
          wk_abono = monabo
     ENDIF
     STORE 0 TO wk_fle, wk_otro,  ;
           wk_monto, wk_numdoc
     @ 04, 20 SAY  ;
       'Orden de Reparaci¢n..:'
     @ 05, 20 SAY  ;
       'C¢digo de Documento..:'
     @ 06, 20 SAY  ;
       'N£mero de Documento..:'
     @ 07, 20 SAY  ;
       'Fecha de Vencimiento.:'
     @ 08, 20 SAY  ;
       'Fecha Venc.Gar.Repar.:'
     @ 09, 20 SAY  ;
       'C¢digo de Pago.......:'
     @ 05, 44 GET wk_coddoc  ;
       PICTURE '@!' VALID  ;
       vali_docu(wk_coddoc)
     @ 06, 44 GET wk_numdoc  ;
       PICTURE '9999999999' VALID  ;
       val_num()
     READ
     IF LASTKEY() <> -9 .AND.  ;
        LASTKEY() <> 27
          @ 07, 44 GET wk_fecven  ;
            VALID  ;
            vali_fe(wk_fecven)  ;
            .AND. LASTKEY() <> 5
          @ 08, 44 GET wk_fevenga  ;
            VALID  ;
            vali_fe(wk_fevenga)
          wk_codpag = '001 '
          @ 09, 44 GET wk_codpag  ;
            PICTURE '@!' VALID  ;
            vali_pag(wk_codpag)  ;
            WHEN colocaf6()
          READ
     ENDIF
     IF LASTKEY() <> 27 .AND.  ;
        LASTKEY() <> -9
          DO muestra
     ENDIF
ENDDO
DO saca_win
ON KEY LABEL F6
SET CURSOR OFF
CLOSE DATABASES
RETURN
*
PROCEDURE muestra
confi = ' '
SELECT st_iorep
SEEK wrk_numord
IF FOUND()
     wrk_numsol = numsol
     wk_codcli = codent
     wk_marca = codmar
     wk_modelo = codmod
     wk_serie = numser
     wrk_codemi = codemi
     STORE SPACE(37) TO  ;
           wrk_infte1, wrk_infte2,  ;
           wrk_infte3, wrk_infte4,  ;
           wrk_infte5
     wrk_infte1 = SUBSTR(observ,  ;
                  1, 38)
     wrk_infte2 = SUBSTR(observ,  ;
                  39, 38)
     wrk_infte3 = SUBSTR(observ,  ;
                  77, 38)
     wrk_infte4 = SUBSTR(observ,  ;
                  115, 38)
     wrk_infte5 = SUBSTR(observ,  ;
                  152, 38)
     wk_repues = cosrep
     wk_mano = cosmob
     wk_cosfle = flete
     wk_subtot = subtot
     wk_totdes = totdes
     wk_totnet = (wk_mano +  ;
                 wk_repues +  ;
                 wk_cosfle) -  ;
                 wk_totdes
     wk_infdes = infdes
ENDIF
DO acta WITH wrk_numsol
SELECT st_iclpr
llave = 'C' + wk_codcli
SEEK llave
IF FOUND()
     wk_nomcli = noment
     wk_direc = SUBSTR(nomcal, 1,  ;
                20)
     wrk_coddis = nomdis
ENDIF
SELECT ge_tab0
SEEK 'DIST' + wrk_coddis
IF FOUND()
     wk_dire = tab_destab
     wk_dir = ALLTRIM(wk_direc) +  ;
              ' ' +  ;
              SUBSTR(wk_dire, 1,  ;
              15)
ELSE
     wk_dire = SPACE(1)
     wk_dir = ALLTRIM(wk_direc) +  ;
              ' ' +  ;
              SUBSTR(wk_dire, 1,  ;
              15)
ENDIF
DO createm
@ 03, 01 CLEAR TO 15, 79
@ 03, 02 SAY wrk_tipcam PICTURE  ;
  '99.99'
@ 03, 18 SAY wk_infdes COLOR W+/N* 
@ 04, 02 SAY  ;
  ' E N C A B E Z A D O ' COLOR  ;
  SCHEME 8
@ 04, 43 SAY  ;
  '   T O T A L E S            S/.'  ;
  COLOR SCHEME 8
@ 05, 02 SAY  ;
  'N£mero de Orden....:' +  ;
  wrk_numord
@ 06, 02 SAY  ;
  'Cliente............:' +  ;
  SUBSTR(wk_nomcli, 1, 20)
@ 07, 02 SAY  ;
  'C¢digo Cliente.....:' +  ;
  wk_codcli
@ 08, 02 SAY  ;
  'Marca..............:' +  ;
  wk_marca
@ 09, 02 SAY  ;
  'Modelo.............:' +  ;
  wk_modelo
@ 10, 02 SAY  ;
  'N£mero de Serie....:' +  ;
  wk_serie
@ 11, 02 SAY  ;
  'Total a Cuenta.....:    S/.'
@ 12, 02 SAY 'Pagos...:'
IF wrk_toacta > 0
     IF wrk_cont01 > 3
          wrk_cont02 = 3
     ELSE
          wrk_cont02 = wrk_cont01
     ENDIF
     @ 11, 34 SAY wrk_toacta  ;
       PICTURE '999.99' COLOR  ;
       SCHEME 8
     b = 12
     FOR a = 1 TO wrk_cont02
          @ b, 20 SAY nrodoc(a) +  ;
            '  ' +  ;
            TRANSFORM(montos(a),  ;
            '999.99')
          b = b + 1
     ENDFOR
ENDIF
sale = 0
ON KEY LABEL f10 do f10s
IF wk_coddoc = 'FACT'
     wk_subtot = ROUND((wk_subtot *  ;
                 (1 +  ;
                 wrk_facigv)),  ;
                 2)
     wk_cosrep = wk_subtot
     sol_subtot = ROUND((sol_subtot *  ;
                  (1 +  ;
                  wrk_facigv)),  ;
                  2)
     sol_cosrep = sol_subtot
ELSE
     wk_cosrep = wk_subtot
     sol_cosrep = sol_subtot
ENDIF
wk_totnet = wk_subtot +  ;
            ROUND((wk_mano * (1 +  ;
            wrk_facigv)), 2) +  ;
            ROUND((wk_cosfle * (1 +  ;
            wrk_facigv)), 2)
wk_monto = wk_totnet
wk_totbru = wk_monto - wk_totdes
wk_totpag = wk_totbru - wrk_dolac
wk_totvta = ROUND((wk_totpag / (1 +  ;
            wrk_facigv)), 2)
wk_totigv = wk_totpag - wk_totvta
sol_cosmob = ROUND(ROUND(wk_mano *  ;
             wrk_tipcam, 2) * (1 +  ;
             wrk_facigv), 2)
sol_cosfle = ROUND(ROUND(wk_cosfle *  ;
             wrk_tipcam, 2) * (1 +  ;
             wrk_facigv), 2)
sol_total = sol_subtot +  ;
            sol_cosmob +  ;
            sol_cosfle
sol_descue = ROUND((wk_totdes *  ;
             wrk_tipcam), 2)
sol_totgen = sol_total -  ;
             sol_descue
sol_totpag = sol_totgen -  ;
             wrk_toacta
sol_totvta = ROUND((sol_totpag /  ;
             (1 + wrk_facigv)),  ;
             2)
sol_totigv = sol_totpag -  ;
             sol_totvta
@ 05, 43 SAY  ;
  'Costo En Repuesto :' +  ;
  SPACE(2) + TRANSFORM(sol_cosrep,  ;
  '999,999.99')
@ 06, 43 SAY  ;
  'Costo Mano de Obra:' +  ;
  SPACE(2) + TRANSFORM(sol_cosmob,  ;
  '999,999.99')
@ 07, 43 SAY  ;
  'Flete ............:' +  ;
  SPACE(2) + TRANSFORM(sol_cosfle,  ;
  '999,999.99')
@ 08, 43 SAY  ;
  'Sub-total.........:' +  ;
  SPACE(2) + TRANSFORM(sol_total,  ;
  '999,999.99')
@ 09, 43 SAY  ;
  'Total Descuentos..:' +  ;
  SPACE(2) + TRANSFORM(sol_descue,  ;
  '999,999.99')
@ 10, 43 SAY  ;
  'Total.General.....:' +  ;
  SPACE(2) + TRANSFORM(sol_totgen,  ;
  '999,999.99')
@ 11, 43 SAY  ;
  'Pagos a Cta. .....:' +  ;
  SPACE(2) + TRANSFORM(wrk_toacta,  ;
  '999,999.99')
@ 12, 43 SAY  ;
  'Total Vta.........:' +  ;
  SPACE(2) + TRANSFORM(sol_totvta,  ;
  '999,999.99')
@ 13, 43 SAY  ;
  'Total I.G.V.......:' +  ;
  SPACE(2) + TRANSFORM(sol_totigv,  ;
  '999,999.99')
@ 14, 43 SAY  ;
  'Saldo a Pagar.....:' +  ;
  SPACE(2) + TRANSFORM(sol_totpag,  ;
  '999,999.99')
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
STORE 0 TO wrk_desadi, wrk_dessol
@ 07, 66 GET sol_cosfle PICTURE  ;
  '9,999.99' VALID val_fle()
@ 09, 66 GET wrk_dessol PICTURE  ;
  '9,999.99' VALID dscto('2')  ;
  .AND. wrk_dessol >= 0
READ
sol_descue = sol_descue +  ;
             wrk_dessol
sol_total = sol_subtot +  ;
            sol_cosmob +  ;
            sol_cosfle
sol_totgen = sol_total -  ;
             sol_descue
sol_totpag = sol_totgen -  ;
             wrk_toacta
sol_totvta = ROUND((sol_totpag /  ;
             (1 + wrk_facigv)),  ;
             2)
sol_totigv = sol_totpag -  ;
             sol_totvta
wk_totdes = wk_totdes +  ;
            wrk_desadi
wk_monto = wk_totnet
wk_totbru = wk_monto - wk_totdes
wk_totpag = wk_totbru - wrk_dolac
wk_totvta = ROUND((wk_totpag / (1 +  ;
            wrk_facigv)), 2)
wk_totigv = wk_totpag - wk_totvta
@ 08, 64 SAY sol_total PICTURE  ;
  '999,999.99'
@ 10, 64 SAY sol_totgen PICTURE  ;
  '999,999.99'
@ 12, 64 SAY sol_totvta PICTURE  ;
  '999,999.99'
@ 13, 64 SAY sol_totigv PICTURE  ;
  '999,999.99'
@ 14, 64 SAY sol_totpag PICTURE  ;
  '999,999.99' COLOR SCHEME 8
IF sol_totpag <= 0
     DO esc_indica WITH 1, 'AYU',  ;
        'GRA', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'bbb', 'ESC'
     DO error WITH  ;
        'Presione F2 para liquidar la Orden, tiene Saldo <=CERO'
     CLEAR TYPEAHEAD
     = INKEY(0, 'H')
     DO WHILE LASTKEY()<>-1 .AND.  ;
        LASTKEY()<>27
          = INKEY(0, 'H')
     ENDDO
     IF LASTKEY() = 27
          RETURN
     ENDIF
     IF LASTKEY() = -1
          SELECT st_iorep
          SEEK wrk_numord
          DO rbloquea
          REPLACE indest WITH 'F'
          REPLACE auxest WITH  ;
                  '100 '
          wk_flete = ROUND(ROUND(sol_cosfle /  ;
                     wrk_tipcam,  ;
                     2) / (1 +  ;
                     wrk_facigv),  ;
                     2)
          REPLACE flete WITH  ;
                  wk_flete
          REPLACE subtot WITH  ;
                  cosrep + cosmob +  ;
                  flete
          REPLACE totnet WITH  ;
                  subtot
          REPLACE totigv WITH  ;
                  ROUND(subtot *  ;
                  wrk_facigv, 2)
          REPLACE totbru WITH  ;
                  totnet +  ;
                  totigv
          REPLACE fecest WITH  ;
                  DATE(), horest  ;
                  WITH TIME()
          IF codtall < '010'  ;
             .AND. SUBSTR(indori,  ;
             1, 1) = 'F'
               REPLACE fecent  ;
                       WITH  ;
                       DATE(),  ;
                       horent  ;
                       WITH  ;
                       TIME()
          ENDIF
          REPLACE user WITH users
          REPLACE time WITH  ;
                  TIME()
          REPLACE date WITH  ;
                  DATE()
          UNLOCK
          SELECT st_mvord
          APPEND BLANK
          DO rbloquea
          REPLACE dia WITH DATE(),  ;
                  hora WITH  ;
                  TIME()
          REPLACE orden WITH  ;
                  wrk_numord,  ;
                  tecnico WITH  ;
                  st_iorep.codtec
          REPLACE inftec WITH  ;
                  st_iorep.observ
          UNLOCK
          dd = nrodoc(1)
          dd2 = tipdoc(1)
          IF EMPTY(dd)
               dd = SPACE(10)
          ENDIF
          DO rbloquea
          REPLACE estado WITH  ;
                  '100 ', destado  ;
                  WITH  ;
                  ( ;
                  'FACTURADO Y ENTREGADO' +  ;
                  '  ') + dd
          REPLACE user WITH users
          REPLACE time WITH  ;
                  TIME()
          REPLACE date WITH  ;
                  DATE()
          UNLOCK
          SELECT st_iprep
          SEEK wrk_numord
          SCAN WHILE numord =  ;
               wrk_numord .AND.   ;
               .NOT. EOF()
               IF indest <> 'N'
                    DO rbloquea
                    REPLACE indest  ;
                            WITH  ;
                            'F'
                    REPLACE user  ;
                            WITH  ;
                            users
                    REPLACE time  ;
                            WITH  ;
                            TIME()
                    REPLACE date  ;
                            WITH  ;
                            DATE()
                    UNLOCK
                    IF  .NOT.  ;
                        EMPTY(dd)
                         wk_nrodoc =  ;
                          numdoc
                         SELECT st_idped
                         SEEK wk_nrodoc
                         SCAN WHILE  ;
                              numdoc =  ;
                              wk_nrodoc  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              IF canpro >  ;
                                 0
                                   wk_cod = codpro
                                   wk_can = canpro
                                   wk_val = valpro
                                   wk_pordes = pordes
                                   wk_totite = totite
                                   SELECT gc_pro00
                                   SEEK wk_cod
                                   wk_coduni = pro_unimed
                                   SELECT gc_dve00
                                   APPEND BLANK
                                   DO rbloquea
                                   REPLACE dve_tipdoc WITH dd2
                                   REPLACE dve_nrodoc WITH dd
                                   REPLACE dve_propar WITH wk_cod
                                   REPLACE dve_cantid WITH wk_can
                                   REPLACE dve_unimed WITH wk_coduni
                                   REPLACE dve_import WITH wk_val
                                   REPLACE dve_pordes WITH wk_pordes
                                   REPLACE dve_total WITH wk_totite
                                   REPLACE dve_coprmo WITH oocospro(wk_cod)
                                   REPLACE dve_coprmb WITH oocosprb(wk_cod)
                                   REPLACE dve_usuari WITH users
                                   REPLACE dve_hora WITH TIME()
                                   REPLACE dve_fecha WITH DATE()
                                   UNLOCK
                              ENDIF
                              SELECT  ;
                               st_idped
                         ENDSCAN
                         SELECT st_iprep
                    ENDIF
               ENDIF
          ENDSCAN
          SELECT st_isrep
          SEEK wrk_numsol
          DO rbloquea
          REPLACE indest WITH 'F'
          REPLACE user WITH users
          REPLACE time WITH  ;
                  TIME()
          REPLACE date WITH  ;
                  DATE()
          UNLOCK
          RETURN
     ENDIF
ENDIF
ON KEY LABEL f10
IF sale = 0 .AND. LASTKEY() <> 27
     DO esc_indica WITH 1, 'AYU',  ;
        'GRA', 'BBB', 'ign'
     DO esc_indica WITH 2, 'bbb',  ;
        'bbb', 'BBB', 'ESC'
     = INKEY(0, 'H')
     CLEAR TYPEAHEAD
     DO WHILE LASTKEY()<>27 .AND.  ;
        LASTKEY()<>-1 .AND.  ;
        LASTKEY()<>-9
          = INKEY(0, 'H')
     ENDDO
     eror_imp = .F.
     IF LASTKEY() = -1
          DO actual
          wk_obliga = .T.
     ENDIF
ENDIF
@ 4, 1 CLEAR TO 14, 79
RETURN
*
FUNCTION val_fle
IF sol_cosfle < 0
     DO error WITH  ;
        '**Flete no puede ser negativo**'
     RETURN .F.
ELSE
     sol_total = sol_subtot +  ;
                 sol_cosmob +  ;
                 sol_cosfle
     sol_totgen = sol_total -  ;
                  sol_descue
     sol_totpag = sol_totgen -  ;
                  wrk_toacta
     sol_totvta = ROUND((sol_totpag /  ;
                  (1 +  ;
                  wrk_facigv)),  ;
                  2)
     sol_totigv = sol_totpag -  ;
                  sol_totvta
     wk_totdes = wk_totdes +  ;
                 wrk_desadi
     wk_monto = wk_totnet
     wk_totbru = wk_monto -  ;
                 wk_totdes
     wk_totpag = wk_totbru -  ;
                 wrk_dolac
     wk_totvta = ROUND((wk_totpag /  ;
                 (1 +  ;
                 wrk_facigv)),  ;
                 2)
     wk_totigv = wk_totpag -  ;
                 wk_totvta
     @ 08, 64 SAY sol_total  ;
       PICTURE '999,999.99'
     @ 10, 64 SAY sol_totgen  ;
       PICTURE '999,999.99'
     @ 12, 64 SAY sol_totvta  ;
       PICTURE '999,999.99'
     @ 13, 64 SAY sol_totigv  ;
       PICTURE '999,999.99'
     @ 14, 64 SAY sol_totpag  ;
       PICTURE '999,999.99' COLOR  ;
       SCHEME 8
ENDIF
RETURN
*
PROCEDURE createm
CREATE CURSOR factu (codigo C  ;
       (14), descri C (30),  ;
       unidad C (4), cantid N (9,  ;
       2), precio N (9, 2),  ;
       pordes N (4, 2), total N  ;
       (9, 2))
STORE 0 TO sol_subtot, wk_subtot
SELECT st_iprep
SEEK wrk_numord
SCAN WHILE numord = wrk_numord  ;
     .AND.  .NOT. EOF()
     IF indest <> 'N'
          wk_nrodoc = numdoc
          SELECT st_idped
          SEEK wk_nrodoc
          SCAN WHILE numdoc =  ;
               wk_nrodoc .AND.   ;
               .NOT. EOF()
               IF canpro > 0
                    SELECT factu
                    APPEND BLANK
                    REPLACE codigo  ;
                            WITH  ;
                            st_idped.codpro
                    REPLACE cantid  ;
                            WITH  ;
                            st_idped.canpro
                    REPLACE precio  ;
                            WITH  ;
                            st_idped.valpro
                    REPLACE pordes  ;
                            WITH  ;
                            st_idped.pordes
                    REPLACE total  ;
                            WITH  ;
                            st_idped.totite
                    IF wk_coddoc =  ;
                       'FACT'
                         sol_subtot =  ;
                          sol_subtot +  ;
                          ROUND((ROUND((precio *  ;
                          wrk_tipcam),  ;
                          2) *  ;
                          cantid),  ;
                          2)
                         wk_subtot =  ;
                          wk_subtot +  ;
                          ROUND((precio *  ;
                          cantid),  ;
                          2)
                    ELSE
                         sol_subtot =  ;
                          sol_subtot +  ;
                          ROUND(ROUND(ROUND(precio *  ;
                          wrk_tipcam,  ;
                          2) * (1 +  ;
                          wrk_facigv),  ;
                          2) *  ;
                          cantid,  ;
                          2)
                         wk_subtot =  ;
                          wk_subtot +  ;
                          ROUND((ROUND((precio *  ;
                          (1 +  ;
                          wrk_facigv)),  ;
                          2) *  ;
                          cantid),  ;
                          2)
                    ENDIF
                    SELECT gc_pro00
                    SEEK factu.codigo
                    SELECT factu
                    REPLACE unidad  ;
                            WITH  ;
                            gc_pro00.pro_unimed
                    REPLACE descri  ;
                            WITH  ;
                            SUBSTR(gc_pro00.pro_descri,  ;
                            1,  ;
                            30)
               ENDIF
               SELECT st_idped
          ENDSCAN
     ENDIF
     SELECT st_iprep
ENDSCAN
RETURN
*
PROCEDURE actual
DO mensa WITH 'Grabando '+ ;
   wk_coddoc+' N§ '+STR(wk_numdoc,  ;
   9), 'COLO'
SELECT st_isrep
SEEK wrk_numsol
DO rbloquea
REPLACE indest WITH 'F'
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
UNLOCK
SELECT st_iorep
SEEK wrk_numord
IF FOUND()
     DO rbloquea
     IF wk_coddoc = 'FACT'
          REPLACE indest WITH 'F'
     ELSE
          REPLACE indest WITH 'B'
     ENDIF
     REPLACE numfabo WITH  ;
             f_ceros(wk_numdoc,10, ;
             1)
     REPLACE codfabo WITH  ;
             wk_coddoc
     REPLACE fecfabo WITH DATE()
     REPLACE auxest WITH '100 '
     wk_flete = ROUND(ROUND(sol_cosfle /  ;
                wrk_tipcam, 2) /  ;
                (1 + wrk_facigv),  ;
                2)
     REPLACE flete WITH wk_flete
     REPLACE subtot WITH cosrep +  ;
             cosmob + flete
     REPLACE totnet WITH subtot
     REPLACE totigv WITH  ;
             ROUND(subtot *  ;
             wrk_facigv, 2)
     REPLACE totbru WITH totnet +  ;
             totigv
     REPLACE fecest WITH DATE(),  ;
             horest WITH TIME()
     IF codtall < '010' .AND.  ;
        SUBSTR(indori, 1, 1) =  ;
        'F'
          REPLACE fecent WITH  ;
                  DATE(), horent  ;
                  WITH TIME()
     ENDIF
     REPLACE user WITH users
     REPLACE time WITH TIME()
     REPLACE date WITH DATE()
     UNLOCK
ENDIF
SELECT gc_hve00
APPEND BLANK
DO rbloquea
REPLACE hve_tipdoc WITH wk_coddoc
REPLACE hve_nrodoc WITH  ;
        f_ceros(wk_numdoc,10,1)
REPLACE hve_fecdoc WITH DATE()
REPLACE hve_fecvct WITH wk_fecven
REPLACE hve_fecgar WITH  ;
        wk_fevenga
REPLACE hve_codemi WITH  ;
        wrk_codemi
REPLACE hve_tipent WITH 'C   '
REPLACE hve_codent WITH wk_codcli
REPLACE hve_tippag WITH wk_codpag
REPLACE hve_codmov WITH 'EVTA'
REPLACE hve_estdoc WITH 'O'
REPLACE hve_cosmob WITH wk_mano
REPLACE hve_cosrep WITH  ;
        ROUND((wk_cosrep /  ;
        (wrk_facigv + 1)), 2)
wk_flete = ROUND(ROUND(sol_cosfle /  ;
           wrk_tipcam, 2) / (1 +  ;
           wrk_facigv), 2)
REPLACE hve_totnet WITH wk_monto
REPLACE hve_totdes WITH wk_totdes
REPLACE hve_pordes WITH  ;
        ROUND((100 * wk_totdes) /  ;
        wk_totbru, 2)
REPLACE hve_flete WITH  ;
        ROUND(sol_cosfle /  ;
        wrk_tipcam, 2)
REPLACE hve_totgen WITH wk_totbru
REPLACE hve_pagctd WITH wrk_dolac
REPLACE hve_totoim WITH wk_totbru -  ;
        hve_pagctd
REPLACE hve_totvta WITH wk_totvta
REPLACE hve_totigv WITH wk_totigv
REPLACE hve_codmon WITH 'DOL '
REPLACE hve_fechtc WITH DATE()
REPLACE hve_tidore WITH 'ORDE'
REPLACE hve_nrdore WITH  ;
        wrk_numord
REPLACE hve_numore WITH  ;
        wrk_numord
REPLACE hve_almdes WITH empre6
REPLACE hve_indori WITH  ;
        wrk_indori
REPLACE hve_solrep WITH  ;
        ROUND((sol_cosrep /  ;
        (wrk_facigv + 1)), 2)
REPLACE hve_solmob WITH  ;
        ROUND((sol_cosmob /  ;
        (wrk_facigv + 1)), 2)
REPLACE hve_solfle WITH  ;
        sol_cosfle
REPLACE hve_solnet WITH sol_total
REPLACE hve_soldes WITH  ;
        sol_descue
REPLACE hve_solgen WITH  ;
        sol_totgen
REPLACE hve_pagcts WITH  ;
        wrk_toacta
REPLACE hve_mtocan WITH  ;
        sol_totpag
REPLACE hve_solvta WITH  ;
        sol_totvta
REPLACE hve_soligv WITH  ;
        sol_totigv
REPLACE hve_tipcam WITH  ;
        wrk_tipcam
REPLACE hve_usuari WITH users
REPLACE hve_hora WITH TIME()
REPLACE hve_fecha WITH DATE()
UNLOCK
SELECT st_iprep
SEEK wrk_numord
SCAN WHILE numord = wrk_numord  ;
     .AND.  .NOT. EOF()
     IF indest <> 'N'
          DO rbloquea
          REPLACE indest WITH 'F'
          REPLACE user WITH users
          REPLACE time WITH  ;
                  TIME()
          REPLACE date WITH  ;
                  DATE()
          UNLOCK
          wk_nrodoc = numdoc
          SELECT st_idped
          SEEK wk_nrodoc
          SCAN WHILE numdoc =  ;
               wk_nrodoc .AND.   ;
               .NOT. EOF()
               IF canpro > 0
                    wk_cod = codpro
                    wk_can = canpro
                    wk_val = valpro
                    wk_pordes = pordes
                    wk_totite = totite
                    SELECT gc_pro00
                    SEEK wk_cod
                    wk_coduni = pro_unimed
                    SELECT gc_dve00
                    APPEND BLANK
                    DO rbloquea
                    REPLACE dve_tipdoc  ;
                            WITH  ;
                            wk_coddoc
                    REPLACE dve_nrodoc  ;
                            WITH  ;
                            f_ceros(wk_numdoc, ;
                            10, ;
                            1)
                    REPLACE dve_propar  ;
                            WITH  ;
                            wk_cod
                    REPLACE dve_cantid  ;
                            WITH  ;
                            wk_can
                    REPLACE dve_unimed  ;
                            WITH  ;
                            wk_coduni
                    REPLACE dve_import  ;
                            WITH  ;
                            wk_val
                    REPLACE dve_pordes  ;
                            WITH  ;
                            wk_pordes
                    REPLACE dve_total  ;
                            WITH  ;
                            wk_totite
                    REPLACE dve_coprmo  ;
                            WITH  ;
                            oocospro(wk_cod)
                    REPLACE dve_coprmb  ;
                            WITH  ;
                            oocosprb(wk_cod)
                    REPLACE dve_usuari  ;
                            WITH  ;
                            users
                    REPLACE dve_hora  ;
                            WITH  ;
                            TIME()
                    REPLACE dve_fecha  ;
                            WITH  ;
                            DATE()
                    UNLOCK
               ENDIF
               SELECT st_idped
          ENDSCAN
          SELECT st_iprep
     ENDIF
ENDSCAN
SELECT st_estad
SEEK wrk_numord
IF FOUND()
     DO rbloquea
     REPLACE coddoc WITH  ;
             wk_coddoc
     REPLACE numfabo WITH  ;
             f_ceros(wk_numdoc,10, ;
             1)
     REPLACE user WITH users
     REPLACE time WITH TIME()
     REPLACE date WITH DATE()
     UNLOCK
ENDIF
SELECT st_mvord
APPEND BLANK
DO rbloquea
REPLACE dia WITH DATE(), hora  ;
        WITH TIME()
REPLACE orden WITH wrk_numord,  ;
        inftec WITH  ;
        st_iorep.observ
REPLACE estado WITH '100 ',  ;
        destado WITH ( ;
        'FACTURADO Y ENTREGADO' +  ;
        '  ' + wk_coddoc + '  ' +  ;
        ALLTRIM(STR(wk_numdoc,  ;
        9)))
REPLACE tecnico WITH  ;
        st_iorep.codtec
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
UNLOCK
SELECT st_iredo
APPEND BLANK
DO rbloquea
REPLACE indodo WITH 'ORD'
REPLACE numodo WITH wrk_numord
REPLACE indddo WITH wk_coddoc
REPLACE numddo WITH  ;
        f_ceros(wk_numdoc,10,1)
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
UNLOCK
DO mensa WITH 'Grabando '+ ;
   wk_coddoc+' N§ '+STR(wk_numdoc,  ;
   9), 'SACA'
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
FUNCTION vali_pag
PARAMETER pag
IF LASTKEY() = 5
     RETURN .T.
ENDIF
IF EMPTY(pag)
     DO error WITH  ;
        ' ** Debe Ingresar C¢digo **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
SELECT ge_tab0
llave = 'FPAG' + pag
SEEK llave
IF FOUND()
     wrk_despag = tab_destab
     RETURN .T.
ELSE
     DO error WITH  ;
        '** C¢digo de Pago No Encontrado **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE imprime
IF wk_coddoc = 'BOLE'
     lin = 20
ELSE
     lin = 21
ENDIF
DO esc_modo WITH 'P'
SET CONSOLE OFF
SET PRINTER ON
SET DEVICE TO PRINTER
STORE 0 TO wk_x1, wk_x2, wk_x3,  ;
      wk_totcan
IF wk_coddoc = 'FACT'
ELSE
ENDIF
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(67) + CHR(51)
? CHR(18)
cuenta_lin = 0
IF wk_coddoc = 'FACT'
     @ 11, 00 SAY DTOC(DATE())
     @ 11, 15 SAY wrk_codemi
     @ 13, 10 SAY wk_nomcli
     IF wk_coddoc = 'FACT'
          @ 13, 48 SAY wk_codcli
     ENDIF
     @ 15, 10 SAY wk_dir
     @ 17, 28 SAY 'N. SOLES'
     @ 17, 40 SAY  ;
       SUBSTR(wrk_despag, 1, 13)
     @ 17, 56 SAY 'S/S:' +  ;
       ALLTRIM(SUBSTR(wrk_numsol,  ;
       1, 8))
     @ 17, 68 SAY 'O/R:' +  ;
       ALLTRIM(SUBSTR(wrk_numord,  ;
       1, 8))
ELSE
     @ 11, 00 SAY DTOC(DATE())
     @ 11, 15 SAY wrk_codemi
     @ 13, 10 SAY wk_nomcli
     @ 15, 10 SAY wk_dir
     @ 17, 28 SAY 'N. SOLES'
     @ 17, 40 SAY  ;
       SUBSTR(wrk_despag, 1, 13)
     @ 17, 56 SAY 'S/S:' +  ;
       ALLTRIM(SUBSTR(wrk_numsol,  ;
       1, 8))
     @ 17, 68 SAY 'O/R:' +  ;
       ALLTRIM(SUBSTR(wrk_numord,  ;
       1, 8))
ENDIF
SELECT factu
GOTO TOP
DO WHILE  .NOT. EOF()
     cuenta_lin = cuenta_lin + 1
     wk_codpro = codigo
     wk_canpro = cantid
     wk_valpro = precio
     wk_descri = descri
     @ lin, 00 SAY cuenta_lin  ;
       PICTURE '99'
     @ lin, 18 SAY wk_descri  ;
       PICTURE '@!'
     @ lin, 47 SAY wk_canpro  ;
       PICTURE '99,999'
     IF wk_coddoc = 'FACT'
          wk_x1 = ROUND((wk_valpro *  ;
                  wrk_tipcam),  ;
                  2)
          wk_x2 = ROUND((wk_x1 *  ;
                  wk_canpro), 2)
          @ lin, 53 SAY wk_x1  ;
            PICTURE '999,999.99'
          @ lin, 70 SAY wk_x2  ;
            PICTURE '999,999.99'
          wk_totcan = wk_totcan +  ;
                      wk_x2
     ELSE
          wk_x1 = ROUND(wk_valpro *  ;
                  wrk_tipcam, 2)
          wk_x1 = ROUND(wk_x1 *  ;
                  (1 +  ;
                  wrk_facigv),  ;
                  2)
          wk_x2 = ROUND(wk_x1 *  ;
                  wk_canpro, 2)
          @ lin, 53 SAY wk_x1  ;
            PICTURE '999,999.99'
          @ lin, 70 SAY wk_x2  ;
            PICTURE '999,999.99'
          wk_totcan = wk_totcan +  ;
                      wk_x2
     ENDIF
     lin = lin + 1
     IF wk_coddoc = 'BOLE'
          IF lin = 34
               EXIT
          ENDIF
     ELSE
          IF lin = 35
               EXIT
          ENDIF
     ENDIF
     SELECT factu
     SKIP
ENDDO
IF wk_coddoc = 'FACT'
     sol_cosrep = ROUND((wk_totcan *  ;
                  (1 +  ;
                  wrk_facigv)),  ;
                  2)
ELSE
     sol_cosrep = wk_totcan
ENDIF
IF wk_coddoc = 'FACT'
     @ 34, 01 SAY 'FLETE S/.'
     @ 34, 10 SAY sol_cosfle  ;
       PICTURE '999,999.99'
     @ 35, 10 SAY sol_cosrep  ;
       PICTURE '999,999.99'
     @ 35, 62 SAY 'S/.'
     @ 35, 67 SAY sol_totvta  ;
       PICTURE '999,999.99'
     @ 36, 10 SAY sol_cosmob  ;
       PICTURE '999,999.99'
     @ 36, 67 SAY sol_totigv  ;
       PICTURE '999,999.99'
     @ 37, 10 SAY sol_descue  ;
       PICTURE '999,999.99'
     @ 37, 67 SAY sol_totpag  ;
       PICTURE '999,999.99'
     IF wrk_toacta > 0
          @ 38, 01 SAY 'A Cta.:'
          @ 38, 10 SAY wrk_toacta  ;
            PICTURE '999,999.99'
     ENDIF
     @ 40, 01 SAY  ;
       oonumlet(sol_totpag)
     @ 42, 00 SAY wrk_infte1
     IF wrk_cont01 > 0
          @ 42, 40 SAY nrodoc(1) +  ;
            ' ' +  ;
            TRANSFORM(montos(1),  ;
            '9,999.99')
     ENDIF
     @ 43, 00 SAY wrk_infte2
     IF wrk_cont01 > 1
          @ 43, 40 SAY nrodoc(2) +  ;
            ' ' +  ;
            TRANSFORM(montos(2),  ;
            '9,999.99')
     ENDIF
     @ 44, 00 SAY wrk_infte3
     IF wrk_cont01 > 2
          @ 44, 40 SAY nrodoc(3) +  ;
            ' ' +  ;
            TRANSFORM(montos(3),  ;
            '9,999.99')
     ENDIF
     numvta = f_ceros(wk_numdoc, ;
              10,1)
     @ 44, 60 SAY numvta
ELSE
     @ 35, 01 SAY 'FLETE S/.'
     @ 35, 10 SAY sol_cosfle  ;
       PICTURE '999,999.99'
     @ 36, 10 SAY sol_cosrep  ;
       PICTURE '999,999.99'
     @ 36, 62 SAY 'S/.'
     @ 36, 67 SAY sol_totpag +  ;
       wrk_toacta PICTURE  ;
       '999,999.99'
     @ 37, 10 SAY sol_cosmob  ;
       PICTURE '999,999.99'
     IF wrk_toacta > 0
          @ 37, 67 SAY wrk_toacta  ;
            PICTURE '999,999.99'
     ENDIF
     @ 38, 10 SAY sol_descue  ;
       PICTURE '999,999.99'
     @ 38, 67 SAY sol_totpag  ;
       PICTURE '999,999.99'
     @ 40, 01 SAY  ;
       oonumlet(sol_totpag)
     @ 41, 00 SAY wrk_infte1
     IF wrk_cont01 > 0
          @ 41, 40 SAY nrodoc(1) +  ;
            ' ' +  ;
            TRANSFORM(montos(1),  ;
            '9,999.99')
     ENDIF
     @ 42, 00 SAY wrk_infte2
     IF wrk_cont01 > 1
          @ 42, 40 SAY nrodoc(2) +  ;
            ' ' +  ;
            TRANSFORM(montos(1),  ;
            '9,999.99')
     ENDIF
     @ 43, 00 SAY wrk_infte3
     IF wrk_cont01 > 2
          @ 43, 40 SAY nrodoc(3) +  ;
            ' ' +  ;
            TRANSFORM(montos(2),  ;
            '9,999.99')
     ENDIF
     @ 44, 00 SAY wrk_infte4
     IF wrk_cont01 > 3
          @ 44, 40 SAY nrodoc(4) +  ;
            ' ' +  ;
            TRANSFORM(montos(3),  ;
            '9,999.99')
     ENDIF
     @ 44, 60 SAY numvta
ENDIF
EJECT
SET PRINTER OFF
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
RETURN
*
PROCEDURE ayufac
ON KEY LABEL F6
IF VARREAD() = 'WK_CODPAG'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'FPAG'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'FORMAS DE PAGO'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SELECT ge_tab0
     SET FILTER TO
ENDIF
IF VARREAD() == 'WRK_NUMORD'
     SELECT st_iorep
     wrk_origen = 'OR'
     SET FILTER TO indest = 'C'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)+" "+ALLTRIM(INDORI)'
     DO ayuda4 WITH campoa,  ;
        wrk_origen
     SELECT st_iorep
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
ON KEY LABEL F6 DO AYUFAC
RETURN
*
FUNCTION vali_docu
PARAMETER doc
IF doc <> 'FACT' .AND. doc <>  ;
   'BOLE'
     DO error WITH  ;
        '**Debe ser [FACT]ura ¢ [BOLE]eta **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ELSE
     RETURN .T.
ENDIF
RETURN
*
FUNCTION val_num
IF EMPTY(wk_numdoc)
     DO error WITH  ;
        '*** Nro. no puede ser cero ***'
     RETURN .F.
ELSE
     SELECT gc_hve00
     SEEK wk_coddoc +  ;
          f_ceros(wk_numdoc,10, ;
          1)
     IF FOUND()
          DO error WITH  ;
             '*** Nro. de Documento existe ***'
          RETURN .F.
     ENDIF
ENDIF
RETURN
*
PROCEDURE f10s
CLEAR READ
sale = 1
RETURN
*
FUNCTION dscto
PARAMETER opc
DO CASE
     CASE opc = '1'
          IF wrk_desadi >  ;
             wk_totbru
               DO error WITH  ;
                  '*** Descuento Mayor al Monto Total ***'
               RETURN .F.
          ENDIF
          wrk_dessol = ROUND(wrk_desadi *  ;
                       wrk_tipcam,  ;
                       2)
          @ 12, 69 SAY wrk_dessol *  ;
            wrk_tipcam PICTURE  ;
            '999.99'
     CASE opc = '2'
          wrk_desadi = ROUND(wrk_dessol /  ;
                       wrk_tipcam,  ;
                       2)
          IF wrk_desadi < 0
               DO error WITH  ;
                  '*** Descuento No Puede Ser Negativo ***'
               RETURN .F.
          ENDIF
          IF wrk_dessol >  ;
             ROUND((sol_total *  ;
             rge_pormax) / 100,  ;
             2) .AND. wk_infdes =  ;
             SPACE(40)
               DO error WITH  ;
                  '*** Descuento excede el '+ ;
                  STR(rge_pormax,  ;
                  3)+ ;
                  ' % a descontar'
               RETURN .F.
          ENDIF
          IF wrk_desadi >  ;
             wk_totbru
               DO error WITH  ;
                  '*** Descuento Mayor al Monto Total ***'
               RETURN .F.
          ENDIF
ENDCASE
*
PROCEDURE acta
PARAMETER wrk_numsol
SELECT gc_hve00
SET ORDER TO nrdore
STORE 0 TO wrk_toacta, a,  ;
      wrk_dolac
DIMENSION nrodoc[ 5], montos[ 5],  ;
          tipdoc[ 5]
GOTO TOP
SEEK wrk_numsol
SCAN WHILE  ;
     VAL(gc_hve00.hve_nrdore) =  ;
     VAL(wrk_numsol) .AND.  .NOT.  ;
     EOF()
     IF gc_hve00.hve_estdoc <>  ;
        'A'
          a = a + 1
          tipdoc( a) =  ;
                gc_hve00.hve_tipdoc
          nrodoc( a) =  ;
                gc_hve00.hve_nrodoc
          montos( a) =  ;
                gc_hve00.hve_solgen
          wrk_toacta = wrk_toacta +  ;
                       hve_solgen
          wrk_dolac = wrk_dolac +  ;
                      hve_totgen
     ENDIF
ENDSCAN
wrk_cont01 = a
SET ORDER TO codigo
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER opc
tit_prg = 'MANTENCION'
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 02, 01 SAY DATE() COLOR SCHEME  ;
  8
DO saycenter WITH 1, tit_prg
IF opc = 1
     DO saycenter WITH 2,  ;
        ' VENTAS EN LINEA '
ELSE
     DO saycenter WITH 2,  ;
        ' ACTUALIZACION DE VENTAS - MANUAL '
ENDIF
ppas = .T.
w_despa2 = SPACE(10)
w_codcli = SPACE(11)
w_crea = 1
w_igv = 0
numvta = 0
STORE SPACE(8) TO w_numdoc
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_isrep ORDER codigo
SELECT 3
USE SHARED st_iorep ORDER codigo
SELECT 4
USE SHARED st_iprep ORDER  ;
    rep_numord
SELECT 5
USE SHARED st_idped ORDER codigo
SELECT 6
USE SHARED st_iparg
SELECT 7
USE SHARED st_iclpr ORDER codigo
SELECT ge_tab0
w_varbus = '"IGV " + "IGV "'
seek &w_varbus
IF FOUND()
     w_igv = tab_factor
     w_facigv = tab_factor / 100
ELSE
     do error with '**No Definido el &empre9**'
     CLOSE DATABASES
     RETURN
ENDIF
w_tipcam = 0
error = .F.
w_fpari = DATE()
SELECT 20
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
SEEK DTOS(w_fpari) + '1' + 'SOL ' +  ;
     'DOL '
w_tipcam = 3.25 
IF error = .T.
     ppas = .F.
ELSE
     ppas = .T.
ENDIF
SET CURSOR ON
DO WHILE ppas
     ON KEY LABEL f6 do ayufac
     STORE 0 TO sol_cosrep,  ;
           sol_cosmob, sol_cosfle,  ;
           sol_subtot, sol_descue,  ;
           w_toacta, w_dolac
     STORE 0 TO sol_totvta,  ;
           sol_totigv, sol_totgen,  ;
           w_cont01, w_cont02,  ;
           w_flete, w_tem1,  ;
           w_tem2, w_numero
     STORE SPACE(8) TO w_desmon
     DIMENSION nrodoc[ 1],  ;
               montos[ 1],  ;
               tipdoc[ 1]
     DIMENSION infodes( 3)
     w_coddoc = SPACE(4)
     con_eli = 0
     w_nomcli = '  '
     w_numdoc = 0
     STORE SPACE(4) TO w_codpag,  ;
           w_codmon, w_moneda
     STORE 0 TO w_monto, abono,  ;
           w_numord, w_abono,  ;
           w_subtot, w_cosrep,  ;
           w_cosmob
     STORE 0 TO w_total, w_totvta,  ;
           w_totgen, w_totigv,  ;
           li_soldes, li_totdes,  ;
           w_totdes
     STORE DATE() TO w_fecven,  ;
           w_fpari
     STORE (DATE() + empre7) TO  ;
           w_fevenga
     STORE .F. TO eror_imp,  ;
           sw_algo
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'MBV', 'BBB', 'SEL'
     DO esc_indica WITH 2, 'BBB',  ;
        'BUS', 'BBB', 'ESC'
     @ 03, 01 CLEAR TO 14, 78
     @ 03, 17 TO 12, 55
     @ 04, 20 SAY  ;
       'Nro. de Solicitud...:'
     @ 04, 44 GET w_numero  ;
       PICTURE '99999999' VALID  ;
       ordnul(w_numero)
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     w_numord = STR(w_numord, 8)
     SELECT st_iorep
     SEEK w_numord
     w_numero = numsol
     w_mano = cosmob
     w_cosfle = flete
     w_flete = ROUND(flete * (1 +  ;
               w_facigv), 2)
     w_indori = indori
     SELECT st_isrep
     SEEK w_numero
     w_abono = monabo
     STORE 0 TO w_monto
     w_codpag = '001 '
     w_codmon = rge_monbas
     IF opc = 1
          @ 04, 20 SAY  ;
            'Solicitud Servicio...:'
          @ 05, 20 SAY  ;
            'C�digo de Documento..:'
          @ 06, 20 SAY  ;
            'Fecha de Vencimiento.:'
          @ 07, 20 SAY  ;
            'Fecha Venc.Gar.Repar.:'
          @ 08, 20 SAY  ;
            'C�digo de Pago.......:'
          @ 09, 20 SAY  ;
            'C�digo de Moneda.....:'
          IF LASTKEY() <> -9  ;
             .AND. LASTKEY() <>  ;
             27
               @ 05, 44 GET  ;
                 w_coddoc PICTURE  ;
                 '@!' VALID  ;
                 vali_docu(w_coddoc)
               @ 06, 44 GET  ;
                 w_fecven VALID  ;
                 vali_fe(w_fecven)  ;
                 .AND. LASTKEY() <>  ;
                 5 WHEN sacaf6()
               @ 07, 44 GET  ;
                 w_fevenga VALID  ;
                 vali_fe(w_fevenga)
               @ 08, 44 GET  ;
                 w_codpag PICTURE  ;
                 '@!' VALID  ;
                 vali_pag(w_codpag)  ;
                 WHEN colocaf6()
               @ 09, 44 GET  ;
                 w_codmon PICTURE  ;
                 '@!' VALID  ;
                 vali_mon(w_codmon)
               READ
          ENDIF
     ELSE
          @ 04, 20 SAY  ;
            'Solicitud Servicio...:'
          @ 05, 20 SAY  ;
            'C�digo de Documento..:'
          @ 06, 20 SAY  ;
            'N�mero de Documento..:'
          @ 07, 20 SAY  ;
            'Fecha de Vencimiento.:'
          @ 08, 20 SAY  ;
            'Fecha Venc.Gar.Repar.:'
          @ 09, 20 SAY  ;
            'C�digo de Pago.......:'
          @ 10, 20 SAY  ;
            'C�digo de Moneda.....:'
          IF LASTKEY() <> -9  ;
             .AND. LASTKEY() <>  ;
             27
               @ 05, 44 GET  ;
                 w_coddoc PICTURE  ;
                 '@!' VALID  ;
                 vali_docu(w_coddoc)
               @ 06, 44 GET  ;
                 w_numdoc PICTURE  ;
                 '9999999999'  ;
                 VALID  ;
                 val_num(w_numdoc)
               @ 07, 44 GET  ;
                 w_fecven VALID  ;
                 vali_fe(w_fecven)  ;
                 .AND. LASTKEY() <>  ;
                 5
               @ 08, 44 GET  ;
                 w_fevenga VALID  ;
                 vali_fe(w_fevenga)
               @ 09, 44 GET  ;
                 w_codpag PICTURE  ;
                 '@!' VALID  ;
                 vali_pag(w_codpag)
               @ 10, 44 GET  ;
                 w_codmon PICTURE  ;
                 '@!' VALID  ;
                 vali_mon(w_codmon)
               READ
          ENDIF
     ENDIF
     IF LASTKEY() <> 27 .AND.  ;
        LASTKEY() <> -9
          DO muestra
     ENDIF
ENDDO
DO sacawin
ON KEY LABEL F6
CLOSE DATABASES
RETURN
*
FUNCTION val_num
PARAMETER w_num
IF EMPTY(w_num)
     DO error WITH  ;
        '*** Nro. no puede ser cero ***'
     RETURN .F.
ELSE
     SELECT 20
     USE SHARED gc_hve00 ORDER  ;
         codigo
     SEEK w_coddoc +  ;
          f_ceros(w_num,10,1)
     IF FOUND()
          DO error WITH  ;
             '*** Nro. de Documento existe ***'
          RETURN .F.
     ENDIF
ENDIF
RETURN .T.
*
FUNCTION ordnul
PARAMETER cod
IF cod = 0
     DO error WITH  ;
        '** Error  N� debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
cod = STR(cod, 8)
SELECT st_isrep
seek '&cod'
IF  .NOT. FOUND()
     DO error WITH  ;
        '** Error Solicitud NO EXISTE. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
SELECT st_iorep
SET ORDER TO ord_numsol
seek '&cod'
IF  .NOT. FOUND()
     DO error WITH  ;
        '** Error Orden de Reparaci�n No Existe. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF SUBSTR(indest, 1, 1) == 'N'
     DO error WITH  ;
        '** Error Solicitud esta Anulada. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF ALLTRIM(indest) = 'F' .OR.  ;
   ALLTRIM(indest) = 'B'
     DO error WITH  ;
        '** N� Solicitud de Servicio FACTURADA. **'
     RETURN .F.
ENDIF
IF ALLTRIM(indest) <> 'C'
     DO error WITH  ;
        '** N� Solicitud de Servicio No Esta Cerrada**'
     RETURN .F.
ENDIF
IF indori <> 'FGAR' .AND. indori <>  ;
   'FREC'
     DO error WITH  ;
        '** N� Solicitud de Servicio no puede ser '+ ;
        indori+' **'
     RETURN .F.
ENDIF
w_numord = VAL(numdoc)
SET ORDER TO codigo
RETURN .T.
*
PROCEDURE muestra
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'COLO'
confi = ' '
SELECT st_iorep
SEEK w_numord
IF FOUND()
     w_numero = numsol
     w_codcli = codent
     w_marca = codmar
     w_modelo = codmod
     w_serie = numser
     w_codemi = codemi
     STORE SPACE(37) TO w_infte1,  ;
           w_infte2, w_infte3,  ;
           w_infte4, w_infte5
     w_infte1 = SUBSTR(observ, 1,  ;
                38)
     w_infte2 = SUBSTR(observ, 39,  ;
                38)
     w_infte3 = SUBSTR(observ, 77,  ;
                38)
     w_infte4 = SUBSTR(observ,  ;
                115, 38)
     w_infte5 = SUBSTR(observ,  ;
                152, 38)
     w_cosrep = cosrep
     w_mano = cosmob
     w_cosfle = flete
     w_flete = ROUND(flete * (1 +  ;
               w_facigv), 2)
     w_subtot = subtot
     w_totdes = totdes
     w_desmob = desmob
     w_desrep = desrep
ENDIF
DO acta WITH w_numero
SELECT st_iclpr
llave = 'C' + w_codcli
SEEK llave
IF FOUND()
     w_nomcli = noment
     w_direc = nomcal
     w_coddis = nomdis
ENDIF
SELECT ge_tab0
SEEK 'MARC' + w_marca
IF FOUND()
     w_dmarc = SUBSTR(tab_destab,  ;
               1, 10)
ELSE
     w_dmarc = SPACE(10)
ENDIF
SEEK 'DIST' + w_coddis
IF FOUND()
     w_dire = tab_destab
     w_dir = ALLTRIM(w_direc) +  ;
             ' ' + SUBSTR(w_dire,  ;
             1, 20)
ELSE
     w_dire = SPACE(1)
     w_dir = ALLTRIM(w_direc) +  ;
             ' ' + SUBSTR(w_dire,  ;
             1, 20)
ENDIF
DO createm
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'SACA'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
@ 03, 01 CLEAR TO 15, 79
@ 03, 02 SAY w_tipcam PICTURE  ;
  '99,999,999.99'
IF w_desmob <> 0 .OR. w_desrep <>  ;
   0
     w_infdes = 'Repuesto: ' +  ;
                STR(w_desrep, 2) +  ;
                '%  Mano de Obra :' +  ;
                STR(w_desmob, 2) +  ;
                '% '
     @ 03, 17 SAY  ;
       'Descuento Especial : ' +  ;
       w_infdes COLOR W+/N* 
ENDIF
@ 04, 01 SAY  ;
  ' E N C A B E Z A D O ' COLOR  ;
  SCHEME 8
IF w_codmon = rge_monbas
     w_moneda = empre8
ELSE
     w_moneda = empre13
ENDIF
@ 04,43 say "   T O T A L E S            &w_moneda";
COLOR SCHEME 8
@ 05, 01 SAY  ;
  'N�mero S/Servicio..:' +  ;
  w_numero + ' O/R ' + w_numord
@ 06, 01 SAY  ;
  'Cliente............:' +  ;
  SUBSTR(w_nomcli, 1, 20)
@ 07, 01 SAY  ;
  'C�digo Cliente.....:' +  ;
  w_codcli
@ 08, 01 SAY  ;
  'Marca..............:' +  ;
  SUBSTR(ootab2('MARC',w_marca),  ;
  1, 12)
@ 09, 01 SAY  ;
  'Modelo.............:' +  ;
  w_modelo
@ 10, 01 SAY  ;
  'N�mero de Serie....:' +  ;
  w_serie
@ 11,01 say "Total a Cuenta.....:   &w_moneda"
@ 12, 01 SAY 'Pagos...:'
IF w_toacta > 0
     IF w_cont01 > 3
          w_cont02 = 3
     ELSE
          w_cont02 = w_cont01
     ENDIF
     IF w_codmon = rge_monbas
          w_pago = w_toacta
     ELSE
          w_pago = w_dolac
     ENDIF
     @ 11, 28 SAY w_pago PICTURE  ;
       '99,999,999.99' COLOR  ;
       SCHEME 8
     b = 12
     FOR a = 1 TO w_cont02
          @ b, 14 SAY nrodoc(a) +  ;
            '  ' +  ;
            TRANSFORM(montos(a),  ;
            '99,999,999.99')
          b = b + 1
     ENDFOR
ENDIF
sale = 0
ON KEY LABEL f10 do f10s
IF w_coddoc = 'FACT'
     w_cosrep = w_subtot
     w_subtot = ROUND((w_subtot *  ;
                (1 + w_facigv)),  ;
                2)
     sol_subtot = ROUND((sol_subtot *  ;
                  (1 + w_facigv)),  ;
                  2)
     sol_cosrep = sol_subtot
ELSE
     w_cosrep = ROUND(w_subtot /  ;
                (1 + w_facigv),  ;
                2)
     sol_cosrep = sol_subtot
ENDIF
w_tem1 = ROUND((ROUND(w_mano * (1 +  ;
         w_facigv), 2) *  ;
         w_desmob) / 100, 2)
w_tem2 = ROUND((ROUND(w_cosrep *  ;
         (1 + w_facigv), 2) *  ;
         w_desrep) / 100, 2)
w_totdes = w_tem1 + w_tem2
li_totdes = w_totdes + 1
w_totnet = w_subtot +  ;
           ROUND((w_mano * (1 +  ;
           w_facigv)), 2) +  ;
           w_flete
w_monto = w_totnet
w_totbru = w_monto - w_totdes
w_totpag = w_totbru - w_dolac
w_totvta = ROUND((w_totpag / (1 +  ;
           w_facigv)), 2)
w_totigv = w_totpag - w_totvta
sol_cosmob = ROUND(ROUND(w_mano *  ;
             w_tipcam, 2) * (1 +  ;
             w_facigv), 2)
sol_cosfle = ROUND(ROUND(w_cosfle *  ;
             w_tipcam, 2) * (1 +  ;
             w_facigv), 2)
sol_total = sol_subtot +  ;
            sol_cosmob +  ;
            sol_cosfle
w_tem1 = ROUND((sol_cosmob *  ;
         w_desmob) / 100, 2)
w_tem2 = ROUND((sol_cosrep *  ;
         w_desrep) / 100, 2)
sol_descue = w_tem1 + w_tem2
li_soldes = sol_descue + 1
sol_totgen = sol_total -  ;
             sol_descue
sol_totpag = sol_totgen -  ;
             w_toacta
sol_totvta = ROUND((sol_totpag /  ;
             (1 + w_facigv)), 2)
sol_totigv = sol_totpag -  ;
             sol_totvta
@ 05, 43 SAY  ;
  'Costo En Repuesto :' +  ;
  TRANSFORM(IIF(w_codmon =  ;
  rge_monbas, sol_subtot,  ;
  w_subtot), '99,999,999.99')
@ 06, 43 SAY  ;
  'Costo Mano de Obra:' +  ;
  TRANSFORM(IIF(w_codmon =  ;
  rge_monbas, sol_cosmob,  ;
  ROUND(w_mano * (1 + w_facigv),  ;
  2)), '99,999,999.99')
@ 07, 43 SAY  ;
  'Flete ............:' +  ;
  TRANSFORM(IIF(w_codmon =  ;
  rge_monbas, sol_cosfle,  ;
  w_flete), '99,999,999.99')
@ 08, 43 SAY  ;
  'Sub-total.........:' +  ;
  TRANSFORM(IIF(w_codmon =  ;
  rge_monbas, sol_total,  ;
  w_totnet), '99,999,999.99')
@ 09, 43 SAY  ;
  'Total Descuentos..:' +  ;
  TRANSFORM(IIF(w_codmon =  ;
  rge_monbas, sol_descue,  ;
  w_totdes), '99,999,999.99')
@ 10, 43 SAY  ;
  'Total.General.....:' +  ;
  TRANSFORM(IIF(w_codmon =  ;
  rge_monbas, sol_totgen,  ;
  w_totbru), '99,999,999.99')
@ 11, 43 SAY  ;
  'Pagos a Cta. .....:' +  ;
  TRANSFORM(IIF(w_codmon =  ;
  rge_monbas, w_toacta, w_dolac),  ;
  '99,999,999.99')
@ 12, 43 SAY  ;
  'Total Vta.........:' +  ;
  TRANSFORM(IIF(w_codmon =  ;
  rge_monbas, sol_totvta,  ;
  w_totvta), '99,999,999.99')
@13,43 say "Total &empre9.........:" +;
transform(iif(w_codmon=rge_monbas,sol_totigv,w_totigv),"99,999,999.99")
@ 14, 43 SAY  ;
  'Saldo a Pagar.....:' +  ;
  TRANSFORM(IIF(w_codmon =  ;
  rge_monbas, sol_totpag,  ;
  w_totpag), '99,999,999.99')
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
STORE 0 TO w_desadi, w_dessol
w_desadi = w_totdes
w_dessol = sol_descue
IF w_codmon = rge_monbas
     @ 07, 62 GET sol_cosfle  ;
       PICTURE '99,999,999.99'  ;
       VALID val_fle()
     @ 09, 62 GET w_dessol  ;
       PICTURE '99,999,999.99'  ;
       VALID dscto('2') .AND.  ;
       w_dessol >= 0
ELSE
     @ 07, 62 GET w_flete PICTURE  ;
       '99,999,999.99' VALID  ;
       val_fle()
     @ 09, 62 GET w_desadi  ;
       PICTURE '99,999,999.99'  ;
       VALID dscto('2') .AND.  ;
       w_dessol >= 0
ENDIF
READ
sol_descue = w_dessol
sol_total = sol_subtot +  ;
            sol_cosmob +  ;
            sol_cosfle
sol_totgen = sol_total -  ;
             sol_descue
sol_totpag = sol_totgen -  ;
             w_toacta
sol_totvta = ROUND((sol_totpag /  ;
             (1 + w_facigv)), 2)
sol_totigv = sol_totpag -  ;
             sol_totvta
w_totdes = w_desadi
w_totnet = w_subtot +  ;
           ROUND((w_mano * (1 +  ;
           w_facigv)), 2) +  ;
           w_flete
w_monto = w_totnet
w_totbru = w_monto - w_totdes
w_totpag = w_totbru - w_dolac
w_totvta = ROUND((w_totpag / (1 +  ;
           w_facigv)), 2)
w_totigv = w_totpag - w_totvta
@ 08, 62 SAY IIF(w_codmon =  ;
  rge_monbas, sol_total,  ;
  w_totnet) PICTURE  ;
  '99,999,999.99'
@ 10, 62 SAY IIF(w_codmon =  ;
  rge_monbas, sol_totgen,  ;
  w_totbru) PICTURE  ;
  '99,999,999.99'
@ 12, 62 SAY IIF(w_codmon =  ;
  rge_monbas, sol_totvta,  ;
  w_totvta) PICTURE  ;
  '99,999,999.99'
@ 13, 62 SAY IIF(w_codmon =  ;
  rge_monbas, sol_totigv,  ;
  w_totigv) PICTURE  ;
  '99,999,999.99'
@ 14, 62 SAY IIF(w_codmon =  ;
  rge_monbas, sol_totpag,  ;
  w_totpag) PICTURE  ;
  '99,999,999.99' COLOR SCHEME 8
IF sol_totpag <= 0
     DO esc_indica WITH 1, 'AYU',  ;
        'GRA', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
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
          SEEK w_numord
          DO rbloquea
          REPLACE indest WITH 'F'
          REPLACE auxest WITH  ;
                  '100 '
          REPLACE flete WITH  ;
                  w_cosfle
          REPLACE subtot WITH  ;
                  cosrep + cosmob +  ;
                  flete
          REPLACE totdes WITH  ;
                  ROUND(w_totdes /  ;
                  (1 + w_facigv),  ;
                  2)
          REPLACE totnet WITH  ;
                  subtot -  ;
                  totdes
          REPLACE totigv WITH  ;
                  ROUND(totnet *  ;
                  w_facigv, 2)
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
          SELECT 20
          USE SHARED st_mvord
          APPEND BLANK
          DO rbloquea
          REPLACE dia WITH DATE(),  ;
                  hora WITH  ;
                  TIME()
          REPLACE orden WITH  ;
                  w_numord,  ;
                  tecnico WITH  ;
                  st_iorep.codtec
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
          SELECT 20
          USE SHARED gc_pro00  ;
              ORDER codigo
          SELECT 21
          USE SHARED gc_dve00  ;
              ORDER codigo
          SELECT st_iprep
          SEEK w_numord
          SCAN WHILE numord =  ;
               w_numord .AND.   ;
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
                         w_nrodoc =  ;
                          numdoc
                         SELECT st_idped
                         SEEK w_nrodoc
                         SCAN WHILE  ;
                              numdoc =  ;
                              w_nrodoc  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              IF canpro >  ;
                                 0
                                   w_cod = codpro
                                   w_can = canpro
                                   w_val = valpro
                                   w_pordes = pordes
                                   w_totite = totite
                                   SELECT gc_pro00
                                   SEEK w_cod
                                   w_coduni = pro_unimed
                                   SELECT gc_dve00
                                   APPEND BLANK
                                   DO rbloquea
                                   REPLACE dve_tipdoc WITH dd2
                                   REPLACE dve_nrodoc WITH dd
                                   REPLACE dve_propar WITH w_cod
                                   REPLACE dve_cantid WITH w_can
                                   REPLACE dve_unimed WITH w_coduni
                                   REPLACE dve_pordes WITH w_pordes
                                   REPLACE dve_import WITH w_val
                                   REPLACE dve_total WITH w_totite
                                   REPLACE dve_impors WITH ROUND(w_val * w_tipcam, 2)
                                   REPLACE dve_totals WITH ROUND(dve_impors * dve_cantid, 2)
                                   REPLACE dve_coprmo WITH gc_pro00.pro_coprmo
                                   REPLACE dve_coprmb WITH gc_pro00.pro_coprmb
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
          SEEK w_numero
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
     IF opc = 1
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'IMP',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'IGN',  ;
             'ESC'
     ELSE
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'GRA',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'IGN',  ;
             'ESC'
     ENDIF
     = INKEY(0, 'H')
     CLEAR TYPEAHEAD
     DO WHILE LASTKEY()<>27 .AND.  ;
        (LASTKEY()<>-6 .OR. opc<> ;
        1) .AND. (LASTKEY()<>-1  ;
        .OR. opc<>2) .AND.  ;
        LASTKEY()<>-9
          = INKEY(0, 'H')
     ENDDO
     eror_imp = .F.
     w_obliga = .F.
     IF (LASTKEY() = -6 .AND. opc =  ;
        1) .OR. (LASTKEY() = -1  ;
        .AND. opc = 2)
          IF opc = 1
               ON KEY
               DO status_imp
               DO actual1
               DO imprime
               DO pormsal
          ENDIF
          DO actual
     ENDIF
ENDIF
@ 04, 1 CLEAR TO 14, 79
RETURN
*
FUNCTION val_fle
IF (sol_cosfle < 0 .OR. w_flete <  ;
   0)
     DO error WITH  ;
        '**Flete no puede ser negativo**'
     RETURN .F.
ELSE
     IF w_codmon = rge_monbas
          w_cosfle = ROUND(ROUND(sol_cosfle /  ;
                     (1 +  ;
                     w_facigv),  ;
                     2) /  ;
                     w_tipcam,  ;
                     2)
          w_flete = ROUND(w_cosfle *  ;
                    (1 +  ;
                    w_facigv),  ;
                    2)
     ELSE
          w_cosfle = ROUND(w_flete /  ;
                     (1 +  ;
                     w_facigv),  ;
                     2)
          sol_cosfle = ROUND(ROUND(w_cosfle *  ;
                       w_tipcam,  ;
                       2) * (1 +  ;
                       w_facigv),  ;
                       2)
     ENDIF
     sol_total = sol_subtot +  ;
                 sol_cosmob +  ;
                 sol_cosfle
     sol_totgen = sol_total -  ;
                  sol_descue
     sol_totpag = sol_totgen -  ;
                  w_toacta
     sol_totvta = ROUND((sol_totpag /  ;
                  (1 + w_facigv)),  ;
                  2)
     sol_totigv = sol_totpag -  ;
                  sol_totvta
     w_totdes = w_desadi
     w_totnet = w_subtot +  ;
                ROUND((w_mano *  ;
                (1 + w_facigv)),  ;
                2) + w_flete
     w_monto = w_totnet
     w_totbru = w_monto -  ;
                w_totdes
     w_totpag = w_totbru -  ;
                w_dolac
     w_totvta = ROUND((w_totpag /  ;
                (1 + w_facigv)),  ;
                2)
     w_totigv = w_totpag -  ;
                w_totvta
     @ 08, 62 SAY IIF(w_codmon =  ;
       rge_monbas, sol_total,  ;
       w_totnet) PICTURE  ;
       '99,999,999.99'
     @ 10, 62 SAY IIF(w_codmon =  ;
       rge_monbas, sol_totgen,  ;
       w_totbru) PICTURE  ;
       '99,999,999.99'
     @ 12, 62 SAY IIF(w_codmon =  ;
       rge_monbas, sol_totvta,  ;
       w_totvta) PICTURE  ;
       '99,999,999.99'
     @ 13, 62 SAY IIF(w_codmon =  ;
       rge_monbas, sol_totigv,  ;
       w_totigv) PICTURE  ;
       '99,999,999.99'
     @ 14, 62 SAY IIF(w_codmon =  ;
       rge_monbas, sol_totpag,  ;
       w_totpag) PICTURE  ;
       '99,999,999.99' COLOR  ;
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
STORE 0 TO sol_subtot, w_subtot
SELECT 20
USE SHARED gc_pro00 ORDER codigo
SELECT st_iprep
SEEK w_numord
SCAN WHILE numord = w_numord  ;
     .AND.  .NOT. EOF()
     IF indest <> 'N'
          w_nrodoc = numdoc
          SELECT st_idped
          SEEK w_nrodoc
          SCAN WHILE numdoc =  ;
               w_nrodoc .AND.   ;
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
                    IF w_coddoc =  ;
                       'FACT'
                         sol_subtot =  ;
                          sol_subtot +  ;
                          ROUND((ROUND((precio *  ;
                          w_tipcam),  ;
                          2) *  ;
                          cantid),  ;
                          2)
                         w_subtot =  ;
                          w_subtot +  ;
                          ROUND((precio *  ;
                          cantid),  ;
                          2)
                    ELSE
                         sol_subtot =  ;
                          sol_subtot +  ;
                          ROUND(ROUND(ROUND(precio *  ;
                          (1 +  ;
                          w_facigv),  ;
                          2) *  ;
                          w_tipcam,  ;
                          2) *  ;
                          cantid,  ;
                          2)
                         w_subtot =  ;
                          w_subtot +  ;
                          ROUND((ROUND((precio *  ;
                          (1 +  ;
                          w_facigv)),  ;
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
PROCEDURE actual1
SELECT st_iparg
IF w_coddoc = 'FACT'
     DO rbloquea
     REPLACE sys_numfac WITH  ;
             sys_numfac + 1
     UNLOCK
     w_numdoc = sys_numfac
ELSE
     DO rbloquea
     REPLACE sys_nrobol WITH  ;
             sys_nrobol + 1
     UNLOCK
     w_numdoc = sys_nrobol
ENDIF
numvta = w_numdoc
RETURN
*
PROCEDURE actual
DO mensa WITH 'Grabando '+ ;
   w_coddoc+' N� '+STR(w_numdoc,  ;
   10), 'COLO'
SELECT st_isrep
SEEK w_numero
DO rbloquea
REPLACE indest WITH 'F'
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
UNLOCK
SELECT st_iorep
SEEK w_numord
IF FOUND()
     DO rbloquea
     IF w_coddoc = 'FACT'
          REPLACE indest WITH 'F'
     ELSE
          REPLACE indest WITH 'B'
     ENDIF
     REPLACE numfabo WITH  ;
             f_ceros(w_numdoc,10, ;
             1)
     REPLACE codfabo WITH  ;
             w_coddoc
     REPLACE fecfabo WITH DATE()
     REPLACE auxest WITH '100 '
     REPLACE flete WITH w_cosfle
     REPLACE subtot WITH cosrep +  ;
             cosmob + flete
     REPLACE totdes WITH  ;
             ROUND(w_totdes / (1 +  ;
             w_facigv), 2)
     REPLACE totnet WITH subtot -  ;
             totdes
     REPLACE totigv WITH  ;
             ROUND(totnet *  ;
             w_facigv, 2)
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
SELECT 20
USE SHARED gc_hve00 ORDER codigo
APPEND BLANK
DO rbloquea
REPLACE hve_tipdoc WITH w_coddoc
REPLACE hve_nrodoc WITH  ;
        f_ceros(w_numdoc,10,1)
REPLACE hve_fecdoc WITH DATE()
REPLACE hve_fecvct WITH w_fecven
REPLACE hve_fecgar WITH w_fevenga
REPLACE hve_codemi WITH w_codemi
REPLACE hve_tipent WITH 'C   '
REPLACE hve_codent WITH w_codcli
REPLACE hve_tippag WITH w_codpag
REPLACE hve_codmov WITH 'EVTA'
REPLACE hve_estdoc WITH 'O'
REPLACE hve_cosmob WITH w_mano
REPLACE hve_cosrep WITH w_cosrep
REPLACE hve_totnet WITH w_monto
REPLACE hve_totdes WITH w_totdes
REPLACE hve_pordes WITH  ;
        ROUND((100 * w_totdes) /  ;
        w_monto, 2)
REPLACE hve_flete WITH  ;
        ROUND(sol_cosfle /  ;
        w_tipcam, 2)
REPLACE hve_totgen WITH w_totbru
REPLACE hve_pagctd WITH w_dolac
REPLACE hve_totoim WITH w_totbru -  ;
        hve_pagctd
REPLACE hve_totvta WITH w_totvta
REPLACE hve_totigv WITH w_totigv
REPLACE hve_codmon WITH w_codmon
REPLACE hve_fechtc WITH DATE()
REPLACE hve_tidore WITH 'ORDE'
REPLACE hve_nrdore WITH w_numord
REPLACE hve_numore WITH w_numord
REPLACE hve_almdes WITH empre6
REPLACE hve_indori WITH w_indori
REPLACE hve_solrep WITH  ;
        ROUND((sol_subtot /  ;
        (w_facigv + 1)), 2)
REPLACE hve_solmob WITH  ;
        ROUND((sol_cosmob /  ;
        (w_facigv + 1)), 2)
REPLACE hve_solfle WITH  ;
        sol_cosfle
REPLACE hve_solnet WITH sol_total
REPLACE hve_soldes WITH  ;
        sol_descue
REPLACE hve_solgen WITH  ;
        sol_totgen
REPLACE hve_pagcts WITH w_toacta
REPLACE hve_mtocan WITH  ;
        sol_totpag
REPLACE hve_solvta WITH  ;
        sol_totvta
REPLACE hve_soligv WITH  ;
        sol_totigv
REPLACE hve_tipcam WITH w_tipcam
REPLACE hve_usuari WITH users
REPLACE hve_hora WITH TIME()
REPLACE hve_fecha WITH DATE()
UNLOCK
SELECT 20
USE SHARED gc_pro00 ORDER codigo
SELECT 21
USE SHARED gc_dve00 ORDER codigo
SELECT st_iprep
SEEK w_numord
SCAN WHILE numord = w_numord  ;
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
          w_nrodoc = numdoc
          SELECT st_idped
          SEEK w_nrodoc
          SCAN WHILE numdoc =  ;
               w_nrodoc .AND.   ;
               .NOT. EOF()
               IF canpro > 0
                    w_cod = codpro
                    w_can = canpro
                    w_val = valpro
                    w_pordes = pordes
                    w_totite = totite
                    SELECT gc_pro00
                    SEEK w_cod
                    w_coduni = pro_unimed
                    SELECT gc_dve00
                    APPEND BLANK
                    DO rbloquea
                    REPLACE dve_tipdoc  ;
                            WITH  ;
                            w_coddoc
                    REPLACE dve_nrodoc  ;
                            WITH  ;
                            f_ceros(w_numdoc, ;
                            10, ;
                            1)
                    REPLACE dve_propar  ;
                            WITH  ;
                            w_cod
                    REPLACE dve_cantid  ;
                            WITH  ;
                            w_can
                    REPLACE dve_unimed  ;
                            WITH  ;
                            w_coduni
                    REPLACE dve_import  ;
                            WITH  ;
                            w_val
                    REPLACE dve_total  ;
                            WITH  ;
                            w_totite
                    REPLACE dve_pordes  ;
                            WITH  ;
                            w_pordes
                    REPLACE dve_impors  ;
                            WITH  ;
                            ROUND(w_val *  ;
                            w_tipcam,  ;
                            2)
                    REPLACE dve_totals  ;
                            WITH  ;
                            ROUND(dve_impors *  ;
                            dve_cantid,  ;
                            2)
                    REPLACE dve_coprmo  ;
                            WITH  ;
                            gc_pro00.pro_coprmo
                    REPLACE dve_coprmb  ;
                            WITH  ;
                            gc_pro00.pro_coprmb
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
SELECT 20
USE SHARED st_estad ORDER  ;
    est_numord
SEEK w_numord
IF FOUND()
     DO rbloquea
     REPLACE coddoc WITH w_coddoc
     REPLACE numfabo WITH  ;
             f_ceros(w_numdoc,10, ;
             1)
     REPLACE user WITH users
     REPLACE time WITH TIME()
     REPLACE date WITH DATE()
     UNLOCK
ENDIF
SELECT 20
USE SHARED st_mvord
APPEND BLANK
DO rbloquea
REPLACE dia WITH DATE(), hora  ;
        WITH TIME()
REPLACE orden WITH w_numord
REPLACE estado WITH '100 ',  ;
        destado WITH ( ;
        'FACTURADO Y ENTREGADO' +  ;
        '  ' + w_coddoc + '  ' +  ;
        ALLTRIM(STR(w_numdoc,  ;
        9)))
REPLACE tecnico WITH  ;
        st_iorep.codtec
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
UNLOCK
SELECT 20
USE st_iredo
APPEND BLANK
DO rbloquea
REPLACE indodo WITH 'ORD'
REPLACE numodo WITH w_numord
REPLACE indddo WITH w_coddoc
REPLACE numddo WITH  ;
        f_ceros(w_numdoc,10,1)
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
UNLOCK
DO mensa WITH 'Grabando '+ ;
   w_coddoc+' N� '+STR(w_numdoc,  ;
   10), 'SACA'
RETURN
*
FUNCTION vali_fe
PARAMETER toc
IF EMPTY(toc)
     IF VARREAD() = 'w_FECVEN'
          w_fecven = DATE()
     ELSE
          w_fevenga = DATE()
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
        ' ** Debe Ingresar C�digo **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
SELECT ge_tab0
llave = 'FPAG' + pag
SEEK llave
IF FOUND()
     w_despa2 = ALLTRIM(tab_destab)
     RETURN .T.
ELSE
     DO error WITH  ;
        '** C�digo de Pago No Encontrado **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
RETURN
*
FUNCTION vali_mon
PARAMETER mon
IF LASTKEY() = 5
     RETURN .T.
ENDIF
IF EMPTY(mon)
     DO error WITH  ;
        ' ** Debe Ingresar C�digo **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
SELECT ge_tab0
llave = 'MONE' + mon
SEEK llave
IF FOUND()
     w_desmon = tab_destab
     RETURN .T.
ELSE
     DO error WITH  ;
        '** C�digo de Moneda No Encontrado **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE imprime
IF w_coddoc = 'BOLE'
     lin = 21
ELSE
     lin = 21
ENDIF
DO esc_modo WITH 'P'
SET CONSOLE OFF
SET PRINTER ON
SET DEVICE TO PRINTER
STORE 0 TO w_x1, w_x2, w_x3,  ;
      w_totcan
IF w_coddoc = 'FACT'
     set print to &rge_lptfac  
ELSE
     set print to &rge_lptbol  
ENDIF
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(67) + CHR(51)
? CHR(18)
DO pormfact
EJECT
SET PRINTER OFF
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
SELECT st_iparg
DO rbloquea
IF w_coddoc = 'FACT'
     REPLACE sys_lptfac WITH '0'
ELSE
     REPLACE sys_lptbol WITH '0'
ENDIF
UNLOCK
RETURN
*
PROCEDURE ayufac
ON KEY LABEL f6
IF VARREAD() == 'W_NUMORD'
     SELECT st_iorep
     w_origen = 'OR'
     SET FILTER TO indest = 'C'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)+" "+ALLTRIM(INDORI)'
     DO ayuda4 WITH campoa,  ;
        w_origen
     SELECT st_iorep
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() == 'W_NUMERO'
     SELECT st_iorep
     campoa = 'numsol+" "+dtoc(fecemi)+" "+numdoc+" "+substr(numser,1,12)+" "+codent+" "+substr(codmod,1,10)+" "+subst(indest,1,2)+" "+alltrim(indori)'
     w_origen = 'OR'
     w_orden = ORDER()
     DO ayuda8 WITH campoa,  ;
        w_origen, SELECT()
     SELECT st_iorep
     set order to &w_orden
ENDIF
IF VARREAD() = 'W_CODPAG'
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
IF VARREAD() = 'W_CODDOC'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'DOCU'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'TIPO DE DOCUMENTO'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SELECT ge_tab0
     SET FILTER TO
ENDIF
IF VARREAD() = 'W_CODMON'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'MONE'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'TIPO DE MONEDA'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SELECT ge_tab0
     SET FILTER TO
ENDIF
ON KEY LABEL f6 do ayufac
RETURN
*
FUNCTION vali_docu
PARAMETER doc
IF doc <> 'FACT' .AND. doc <>  ;
   'BOLE'
     DO error WITH  ;
        '**Debe ser [FACT]ura � [BOLE]eta **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ELSE
     RETURN .T.
ENDIF
RETURN
*
PROCEDURE f10s
CLEAR READ
sale = 1
RETURN
*
PROCEDURE status_imp
w_printer = '1'
SELECT st_iparg
DO WHILE w_printer='1'
     SELECT 6
     USE
     USE SHARED st_iparg
     IF w_coddoc = 'FACT'
          w_printer = SUBSTR(sys_lptfac,  ;
                      1, 1)
     ELSE
          w_printer = SUBSTR(sys_lptbol,  ;
                      1, 1)
     ENDIF
     IF w_printer = '1'
          DO mensa WITH  ;
             '*** Impresora Ocupada, FACTURANDO ***',  ;
             'COLO'
          = INKEY(1)
     ELSE
          DO mensa WITH  ;
             '*** Impresora Ocupada, FACTURANDO ***',  ;
             'SACA'
          DO rbloquea
          IF w_coddoc = 'FACT'
               REPLACE sys_lptfac  ;
                       WITH '1'
          ELSE
               REPLACE sys_lptbol  ;
                       WITH '1'
          ENDIF
          UNLOCK
     ENDIF
ENDDO
RETURN
*
FUNCTION dscto
PARAMETER opc
DO CASE
     CASE opc = '1'
          IF w_desadi > w_totbru
               DO error WITH  ;
                  '*** Descuento Mayor al Monto Total ***'
               RETURN .F.
          ENDIF
          w_dessol = ROUND(w_desadi *  ;
                     w_tipcam,  ;
                     2)
          @ 12, 62 SAY w_dessol *  ;
            w_tipcam PICTURE  ;
            '99,999,999.99'
     CASE opc = '2'
          IF w_codmon =  ;
             rge_monbas
               w_desadi = ROUND(w_dessol /  ;
                          w_tipcam,  ;
                          2)
          ELSE
               w_dessol = ROUND(w_desadi *  ;
                          w_tipcam,  ;
                          2)
          ENDIF
          IF (w_dessol < 0 .OR.  ;
             w_desadi < 0)
               DO error WITH  ;
                  '*** Descuento No Puede Ser  Negativo ***'
               RETURN .F.
          ENDIF
          IF ((w_dessol >  ;
             ROUND((sol_total *  ;
             rge_pormax) / 100,  ;
             2) .OR. w_desadi >  ;
             ROUND((w_totnet *  ;
             rge_pormax) / 100,  ;
             2)) .AND. (w_desrep =  ;
             0 .AND. w_desmob =  ;
             0) .AND. nivell <>  ;
             'A7')
               DO error WITH  ;
                  '*** Descuento excede el '+ ;
                  STR(rge_pormax,  ;
                  3)+ ;
                  ' % a descontar'
               RETURN .F.
          ENDIF
          IF ((w_dessol >  ;
             li_soldes .OR.  ;
             w_desadi >  ;
             li_totdes) .AND.  ;
             (w_desrep <> 0 .OR.  ;
             w_desmob <> 0) .AND.  ;
             nivell <> 'A7')
               DO error WITH  ;
                  '*** Descuento excede al Monto a Descontar ***'
               RETURN .F.
          ENDIF
          IF (w_dessol >  ;
             sol_total .OR.  ;
             w_desadi >  ;
             w_totnet)
               DO error WITH  ;
                  '*** Descuento Mayor al Monto Total ***'
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
PROCEDURE acta
PARAMETER w_numero
SELECT 20
USE SHARED gc_hve00 ORDER codigo
SET ORDER TO nrdore
STORE 0 TO w_toacta, a, w_dolac
DIMENSION nrodoc[ 5], montos[ 5],  ;
          tipdoc[ 5]
GOTO TOP
SEEK w_numero
SCAN WHILE  ;
     VAL(gc_hve00.hve_nrdore) =  ;
     VAL(w_numero) .AND.  .NOT.  ;
     EOF()
     IF gc_hve00.hve_estdoc <>  ;
        'A'
          a = a + 1
          tipdoc( a) =  ;
                gc_hve00.hve_tipdoc
          nrodoc( a) =  ;
                gc_hve00.hve_nrodoc
          IF w_codmon =  ;
             rge_monbas
               montos( a) =  ;
                     gc_hve00.hve_solgen
          ELSE
               montos( a) =  ;
                     gc_hve00.hve_totgen
          ENDIF
          w_toacta = w_toacta +  ;
                     hve_solgen
          w_dolac = w_dolac +  ;
                    hve_totgen
     ENDIF
ENDSCAN
w_cont01 = a
SET ORDER TO codigo
RETURN
*
PROCEDURE ayuda12
ON KEY
SELECT st_iorep
campoa = 'numsol+" "+dtoc(fecemi)+" "+numdoc+" "+substr(numser,1,12)+" "+codent+" "+substr(codmod,1,10)+" "+subst(indest,1,2)+" "+alltrim(indori)'
w_origen = 'OR'
w_orden = ORDER()
DO ayuda8 WITH campoa, w_origen,  ;
   SELECT()
SELECT st_iorep
set order to &w_orden
ON KEY LABEL F6 do ayuda12
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER opc
tit_prg = 'MANTENCION'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUFAC
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE() COLOR SCHEME 8
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' VENTAS EN DOLARES '
ppas = .T.
wrk_despag = SPACE(30)
wk_codcli = SPACE(9)
wk_crea = 1
wk_igv = 0
numvta = 0
STORE SPACE(8) TO wrk_numdoc
CLOSE DATABASES
USE SHARED ge_tab0 ORDER codigo
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
USE
USE IN 18 SHARED gc_par00
SELECT 18
rge_monbas = par_monbas
SELECT 18
USE
wk_valpari = 0
error = .F.
USE IN 17 SHARED gc_cmv00 AGAIN  ;
    ORDER codigo
wk_fpari = DATE()
wrk_tipcam = ootc(DATE(),'SOL ', ;
             'DOL ','2')
DO fech
IF error = .T.
     ppas = .F.
ELSE
     ppas = .T.
ENDIF
SET CURSOR ON
DO WHILE ppas
     STORE 0 TO sol_cosrep,  ;
           sol_cosmob, sol_subtot,  ;
           sol_descue,  ;
           wrk_toacta
     STORE 0 TO sol_totvta,  ;
           sol_igv, sol_totgen,  ;
           wrk_cont01,  ;
           wrk_cont02
     STORE SPACE(8) TO wrk_numsol
     DIMENSION nrodoc[ 1], monto[  ;
               1]
     wk_coddoc = SPACE(4)
     con_eli = 0
     wk_nomcli = '  '
     wk_numdoc = 0
     wk_codpag = '    '
     STORE 0 TO wk_monto, wk_fle,  ;
           wk_otro, abono,  ;
           wrk_numor, wk_abono
     STORE DATE() TO wk_fecven,  ;
           wk_fpari
     STORE (DATE() + 30) TO  ;
           wk_fevenga
     STORE .F. TO eror_imp,  ;
           sw_algo
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'mbv', 'BBB', 'SEL'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'bbb', 'ESC'
     @ 03, 01 CLEAR TO 14, 78
     @ 03, 17 TO 12, 55
     @ 4, 20 SAY  ;
       'Orden de Reparaci¢n..:'
     @ 4, 44 GET wrk_numor  ;
       PICTURE '99999999'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     wrk_numord = STR(wrk_numor,  ;
                  8)
     USE SHARED ST_IOREP AGAIN  ;
         ORDER CODIGO
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
          wrk_numsol = numsol
          wk_mano = cosmob
          wrk_indori = indori
          USE SHARED st_isrep  ;
              ORDER CODIGO
          SEEK wrk_numsol
          wk_abono = monabo
     ENDIF
     STORE 0 TO wk_fle, wk_otro,  ;
           wk_monto
     @ 04, 20 SAY  ;
       'Orden de Reparaci¢n..:'
     @ 05, 20 SAY  ;
       'C¢digo de Documento..:'
     @ 06, 20 SAY  ;
       'Fecha de Vencimiento.:'
     @ 07, 20 SAY  ;
       'Fecha Venc.Gar.Repar.:'
     @ 08, 20 SAY  ;
       'C¢digo de Pago.......:'
     @ 05, 44 GET wk_coddoc  ;
       PICTURE '@!' VALID  ;
       vali_docu(wk_coddoc)
     READ
     IF LASTKEY() <> -9 .AND.  ;
        LASTKEY() <> 27
          @ 06, 44 GET wk_fecven  ;
            VALID  ;
            vali_fe(wk_fecven)  ;
            .AND. LASTKEY() <> 5
          @ 07, 44 GET wk_fevenga  ;
            VALID  ;
            vali_fe(wk_fevenga)
          wk_codpag = '001 '
          @ 08, 44 GET wk_codpag  ;
            PICTURE '@!' VALID  ;
            vali_pag(wk_codpag)  ;
            WHEN colocaf6()
          READ
     ENDIF
     IF LASTKEY() <> 27 .AND.  ;
        LASTKEY() <> -9
          DO muestra
     ENDIF
     IF LASTKEY() = -9
          LOOP
     ENDIF
     IF LASTKEY() = 27 .OR.  ;
        eror_imp = .T.
          IF eror_imp = .F.
               EXIT
          ENDIF
     ENDIF
     IF LASTKEY() = 27
          ppas = .F.
     ENDIF
ENDDO
DO saca_win
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
PROCEDURE muestra
confi = ' '
SELECT 5
USE SHARED st_iorep ORDER CODIGO
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
     wk_subtot = subtot
     wk_totdes = totdes
     wk_totnet = (wk_mano +  ;
                 wk_repues) -  ;
                 wk_totdes
ENDIF
USE
DO acta WITH wrk_numsol
SELECT 6
USE SHARED st_iclpr ORDER CODIGO
llave = 'C' + wk_codcli
SEEK llave
IF FOUND()
     wk_nomcli = noment
     wk_direc = SUBSTR(nomcal, 1,  ;
                20)
     wrk_coddis = nomciu
ENDIF
USE
USE SHARED ge_tab0 ORDER codigo
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
USE
@ 03, 01 CLEAR TO 15, 79
@ 03, 02 SAY wk_valpari PICTURE  ;
  '99.99'
@ 04, 02 SAY  ;
  ' E N C A B E Z A D O ' COLOR  ;
  SCHEME 8
@ 04, 43 SAY  ;
  '   T O T A L E S    US $    S/.'  ;
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
            TRANSFORM(monto(a),  ;
            '999.99')
          b = b + 1
     ENDFOR
ENDIF
sale = 0
ON KEY LABEL f10 do f10s
wk_totigv = ROUND(wk_subtot *  ;
            wrk_facigv, 2)
wk_totnet = wk_subtot - wk_totigv
wk_monto = wk_subtot + wk_totigv
wk_totbru = wk_monto - wk_totdes
sol_cosrep = ROUND(wk_repues *  ;
             wk_valpari, 2)
sol_cosmob = ROUND(wk_mano *  ;
             wk_valpari, 2)
sol_subtot = sol_cosrep +  ;
             sol_cosmob
sol_igv = ROUND(sol_subtot *  ;
          wrk_facigv, 2)
sol_total = sol_subtot + sol_igv
sol_descue = wk_totdes *  ;
             wk_valpari
sol_totgen = sol_total -  ;
             sol_descue
sol_totpag = sol_totgen -  ;
             wrk_toacta
@ 05, 43 SAY  ;
  'Costo En Repuesto :' +  ;
  SPACE(5) + TRANSFORM(sol_cosrep,  ;
  '9,999.99')
@ 06, 43 SAY  ;
  'Costo Mano de Obra:' +  ;
  SPACE(5) + TRANSFORM(sol_cosmob,  ;
  '9,999.99')
@ 07, 43 SAY  ;
  'Sub-total.........:' +  ;
  SPACE(5) + TRANSFORM(sol_subtot,  ;
  '9,999.99')
@ 08, 43 SAY  ;
  'Total I.G.V.......:' +  ;
  SPACE(5) + TRANSFORM(sol_igv,  ;
  '9,999.99')
@ 09, 43 SAY  ;
  'Total.............:' +  ;
  SPACE(5) +  ;
  TRANSFORM((sol_subtot +  ;
  sol_igv), '9,999.99')
@ 10, 43 SAY  ;
  'Total Descuentos..:' +  ;
  SPACE(5) + TRANSFORM(sol_descue,  ;
  '9,999.99')
@ 11, 43 SAY  ;
  'Pagos a Cta. .....:' +  ;
  SPACE(5) + TRANSFORM(wrk_toacta,  ;
  '9,999.99')
@ 12, 43 SAY  ;
  'Dscto. Adicional..:'
@ 13, 43 SAY  ;
  'Saldo a Pagar.....:' +  ;
  SPACE(5) + TRANSFORM(sol_totpag,  ;
  '9,999.99')
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
wrk_desadi = 0
wrk_dessol = 0
@ 12, 67 GET wrk_dessol PICTURE  ;
  '9,999.99' VALID dscto('2')
READ
sol_descue = sol_descue +  ;
             wrk_dessol
sol_totgen = sol_totgen -  ;
             wrk_dessol
sol_totpag = sol_totgen -  ;
             wrk_toacta
sol_totvta = ROUND(sol_totpag /  ;
             (1 + wrk_facigv),  ;
             2)
sol_igv = sol_totpag - sol_totvta
wk_totdes = wk_totdes +  ;
            wrk_desadi
wk_totbru = (wk_monto -  ;
            wk_totdes)
wk_totnet = ROUND(wk_totbru / (1 +  ;
            wrk_facigv), 2)
wk_totigv = ROUND(wk_totbru -  ;
            wk_totnet, 2)
@ 08, 67 SAY sol_igv PICTURE  ;
  '9,999.99'
@ 10, 67 SAY sol_descue PICTURE  ;
  '9,999.99'
@ 13, 67 SAY sol_totpag PICTURE  ;
  '9,999.99' COLOR SCHEME 8
IF sol_totpag <= 0
     DO error WITH  ;
        'Presione F2 para liquidar la Orden, tiene Saldo <=CERO'
     = INKEY(0, 'H')
     CLEAR TYPEAHEAD
     DO WHILE LASTKEY()<>27 .AND.  ;
        LASTKEY()<>-1
          = INKEY(0, 'H')
     ENDDO
     IF LASTKEY() = 27
          RETURN
     ENDIF
     IF LASTKEY() = -1
          USE SHARED st_iorep  ;
              ORDER codigo
          SEEK wrk_numord
          REPLACE indest WITH 'F'
          REPLACE auxest WITH  ;
                  '100 '
          REPLACE fecest WITH  ;
                  DATE()
          REPLACE user WITH users
          REPLACE time WITH  ;
                  TIME()
          REPLACE date WITH  ;
                  DATE()
          SELECT 15
          USE ST_IPREP ORDER  ;
              REP_NUMORD
          SEEK wrk_numord
          IF FOUND()
               REPLACE indest  ;
                       WITH 'F'
               REPLACE user WITH  ;
                       users
               REPLACE time WITH  ;
                       TIME()
               REPLACE date WITH  ;
                       DATE()
          ENDIF
          SELECT 9
          USE
          USE SHARED st_mvord
          APPEND BLANK
          REPLACE dia WITH DATE(),  ;
                  hora WITH  ;
                  TIME()
          REPLACE orden WITH  ;
                  wrk_numord
          dd = nrodoc(1)
          IF EMPTY(dd)
               dd = SPACE(10)
          ENDIF
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
          RETURN
     ENDIF
ENDIF
ON KEY LABEL f10
IF sale = 0 .AND. LASTKEY() <> 27
     DO esc_indica WITH 1, 'AYU',  ;
        'bbb', 'imp', 'ign'
     DO esc_indica WITH 2, 'bbb',  ;
        'bbb', 'BBB', 'ESC'
     = INKEY(0, 'H')
     CLEAR TYPEAHEAD
     DO WHILE LASTKEY()<>27 .AND.  ;
        LASTKEY()<>-6 .AND.  ;
        LASTKEY()<>-9
          = INKEY(0, 'H')
     ENDDO
     eror_imp = .F.
     wk_obliga = .F.
     IF LASTKEY() = -6
          ON KEY
          DO status_imp
          DO actual1
          DO imprime
          wk_obliga = .T.
     ENDIF
     IF LASTKEY() = -1 .OR.  ;
        wk_obliga = .T. .AND.  ;
        eror_imp = .F.
          DO actual
     ENDIF
ENDIF
@ 4, 1 CLEAR TO 14, 79
RETURN
*
PROCEDURE busca
wk_numdoc = 1
SELECT 4
USE SHARED st_hfact ORDER CODIGO
GOTO BOTTOM
DO WHILE  .NOT. BOF()
     IF coddoc = wk_coddoc
          wk_numdoc = VAL(numdoc) +  ;
                      1
          GOTO TOP
     ENDIF
     SKIP -1
ENDDO
USE
RETURN
*
PROCEDURE actual1
USE SHARED st_iparg
IF wk_coddoc = 'FACT'
     REPLACE sys_numfac WITH  ;
             sys_numfac + 1
     wk_numdoc = sys_numfac
ELSE
     REPLACE sys_nrobol WITH  ;
             sys_nrobol + 1
     wk_numdoc = sys_nrobol
ENDIF
USE
numvta = wk_numdoc
SELECT 11
USE SHARED st_iredo
APPEND BLANK
REPLACE indodo WITH 'ORD'
REPLACE numodo WITH wrk_numord
REPLACE indddo WITH wk_coddoc
REPLACE numddo WITH STR(wk_numdoc,  ;
        9)
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
USE
*
PROCEDURE actual
IF LASTKEY() = -1
     DO actual1
ENDIF
DO mensa WITH 'Grabando '+ ;
   wk_coddoc+' N§ '+STR(wk_numdoc,  ;
   9), 'COLO'
SELECT 5
USE SHARED st_iorep ORDER CODIGO
SEEK wrk_numord
IF FOUND()
     IF wk_coddoc = 'FACT'
          REPLACE indest WITH 'F'
     ELSE
          REPLACE indest WITH 'B'
     ENDIF
     REPLACE numfabo WITH  ;
             STR(wk_numdoc, 9)
     REPLACE codfabo WITH  ;
             wk_coddoc
     REPLACE fecfabo WITH DATE()
     REPLACE auxest WITH '100 '
     REPLACE fecest WITH DATE()
     REPLACE user WITH users
     REPLACE time WITH TIME()
     REPLACE date WITH DATE()
ENDIF
USE
SELECT 3
USE SHARED GC_HVE00 ORDER CODIGO
APPEND BLANK
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
REPLACE hve_cosrep WITH wk_repues
REPLACE hve_totnet WITH wk_subtot
REPLACE hve_pagctd WITH  ;
        wrk_toacta / wrk_tipcam
REPLACE hve_totdes WITH wk_totdes
REPLACE hve_totvta WITH wk_totnet
REPLACE hve_flete WITH wk_fle +  ;
        wk_otro
REPLACE hve_totigv WITH wk_totigv
REPLACE hve_totgen WITH wk_totbru
REPLACE hve_codmon WITH 'DOL '
REPLACE hve_fechtc WITH DATE()
REPLACE hve_tidore WITH 'ORDE'
REPLACE hve_nrdore WITH  ;
        wrk_numord
REPLACE hve_numore WITH  ;
        wrk_numord
REPLACE hve_almdes WITH '0001'
REPLACE hve_indori WITH  ;
        wrk_indori
REPLACE hve_solrep WITH  ;
        sol_cosrep
REPLACE hve_solmob WITH  ;
        sol_cosmob
REPLACE hve_solnet WITH  ;
        sol_subtot * 1.18 
REPLACE hve_soldes WITH  ;
        sol_descue
REPLACE hve_solvta WITH  ;
        sol_totvta
REPLACE hve_soligv WITH sol_igv
REPLACE hve_solgen WITH  ;
        sol_totgen
REPLACE hve_tipcam WITH  ;
        wrk_tipcam
REPLACE hve_pagcts WITH  ;
        wrk_toacta
REPLACE hve_mtocan WITH  ;
        sol_totpag
REPLACE hve_usuari WITH users
REPLACE hve_hora WITH TIME()
REPLACE hve_fecha WITH DATE()
USE
SELECT 9
USE SHARED gc_pro00 ORDER codigo
SELECT 8
USE SHARED GC_DVE00 ORDER CODIGO
SELECT 15
USE ST_IPREP ORDER REP_NUMORD
SEEK wrk_numord
IF FOUND()
     REPLACE indest WITH 'F'
     REPLACE user WITH users
     REPLACE time WITH TIME()
     REPLACE date WITH DATE()
ENDIF
USE
SELECT 7
USE SHARED st_idped ORDER  ;
    DRE_NUMORD
SEEK wrk_numord
IF FOUND()
     DO WHILE  .NOT. EOF()
          IF numord = wrk_numord  ;
             .AND. canpro > 0
               wk_cod = codpro
               wk_can = canpro
               wk_val = valpro
               wk_pordes = pordes
               wk_totite = totite
               SELECT 9
               SEEK wk_cod
               wk_coduni = pro_unimed
               SELECT 8
               APPEND BLANK
               REPLACE dve_tipdoc  ;
                       WITH  ;
                       wk_coddoc
               REPLACE dve_nrodoc  ;
                       WITH  ;
                       f_ceros(wk_numdoc, ;
                       10,1)
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
          ENDIF
          SELECT 7
          SKIP
     ENDDO
ENDIF
SELECT 11
USE SHARED st_estad ORDER  ;
    EST_NUMORD
SEEK wrk_numord
IF FOUND()
     REPLACE coddoc WITH  ;
             wk_coddoc
     REPLACE numfabo WITH  ;
             wk_numdoc
     REPLACE user WITH users
     REPLACE time WITH TIME()
     REPLACE date WITH DATE()
ENDIF
USE
SELECT 7
USE
SELECT 8
USE
SELECT 9
USE
USE SHARED st_mvord
APPEND BLANK
REPLACE dia WITH DATE(), hora  ;
        WITH TIME()
REPLACE orden WITH wrk_numord
REPLACE estado WITH '100 ',  ;
        destado WITH ( ;
        'Facturado y Entregado' +  ;
        '  ' + wk_coddoc + '  ' +  ;
        ALLTRIM(STR(wk_numdoc,  ;
        9)))
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
USE
DO mensa WITH 'Grabando '+ ;
   wk_coddoc+' N§ '+STR(wk_numdoc,  ;
   9), 'SACA'
RETURN
*
PROCEDURE sin_garan
index on numdoc TAG &es4    
APPEND FROM st_iorep FOR indest ==  ;
       'C   ' .AND. indori ==  ;
       'FGAR'
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
     IF SUBSTR(codfabo, 1, 1) =  ;
        'û'
          REPLACE codfabo WITH  ;
                  SPACE(4)
     ELSE
          REPLACE codfabo WITH  ;
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
USE SHARED ge_tab0 ORDER codigo
llave = 'FPAG' + pag
SEEK llave
IF FOUND()
     wrk_despag = tab_destab
     USE
     RETURN .T.
ELSE
     DO error WITH  ;
        '** C¢digo de Pago No Encontrado **'
     KEYBOARD '{CTRL+Y}'
     USE
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
SET PRINTER TO
IF wk_coddoc = 'FACT'
ELSE
ENDIF
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(67) + CHR(51)
? CHR(18)
cuenta_lin = 0
IF wk_coddoc = 'FACT'
     @ 12, 00 SAY DTOC(DATE())
     @ 12, 15 SAY wrk_codemi
     @ 14, 10 SAY wk_nomcli
     IF wk_coddoc = 'FACT'
          @ 14, 48 SAY wk_codcli
     ENDIF
     @ 16, 10 SAY wk_dir
     @ 18, 28 SAY 'N. SOLES'
     @ 18, 45 SAY  ;
       SUBSTR(wrk_despag, 1, 15)
     @ 18, 70 SAY 'O/R' + ' ' +  ;
       ALLTRIM(wrk_numord)
ELSE
     @ 10, 00 SAY DTOC(DATE())
     @ 10, 15 SAY wrk_codemi
     @ 12, 10 SAY wk_nomcli
     @ 14, 10 SAY wk_dir
     @ 16, 28 SAY 'N. SOLES'
     @ 16, 45 SAY  ;
       SUBSTR(wrk_despag, 1, 15)
     @ 16, 70 SAY 'O/R' + ' ' +  ;
       ALLTRIM(wrk_numord)
ENDIF
SELECT 13
USE SHARED gc_pro00 ORDER codigo
SELECT st_iprep.numdoc,  ;
       st_iprep.numord,  ;
       st_iprep.indest,  ;
       st_idped.codpro,  ;
       st_idped.canpro,  ;
       st_idped.valpro,  ;
       st_idped.totite FROM  ;
       ST_IPREP, ST_IDPED WHERE  ;
       st_idped.numdoc =  ;
       st_iprep.numdoc AND  ;
       (st_iprep.numord =  ;
       wrk_numord AND  ;
       st_idped.canpro > 0 AND  ;
       st_iprep.indest <> 'N')  ;
       ORDER BY st_iprep.numdoc  ;
       INTO CURSOR TIPOS
w_arch = f_archivo()
COPY TO &W_ARCH
SELECT 14
USE &W_ARCH
DO WHILE  .NOT. EOF()
     cuenta_lin = cuenta_lin + 1
     wk_codpro = codpro
     wk_canpro = canpro
     wk_valpro = valpro
     SELECT 13
     SEEK wk_codpro
     wk_descri = pro_descri
     @ lin, 00 SAY cuenta_lin  ;
       PICTURE '99'
     @ lin, 03 SAY wk_codpro  ;
       PICTURE '@!'
     @ lin, 18 SAY wk_descri  ;
       PICTURE '@!'
     @ lin, 47 SAY wk_canpro  ;
       PICTURE '99,999'
     IF wk_coddoc = 'FACT'
          @ lin, 53 SAY  ;
            (wk_valpro *  ;
            wrk_tipcam) PICTURE  ;
            '999,999.99'
          @ lin, 70 SAY  ;
            ((wk_canpro *  ;
            wk_valpro) *  ;
            wrk_tipcam) PICTURE  ;
            '999,999.99'
     ELSE
          @ lin, 53 SAY  ;
            (wk_valpro *  ;
            wrk_tipcam) * 1.18   ;
            PICTURE '999,999.99'
          @ lin, 70 SAY  ;
            ((wk_canpro *  ;
            wk_valpro) *  ;
            wrk_tipcam) * 1.18   ;
            PICTURE '999,999.99'
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
     SELECT 14
     SKIP
ENDDO
SELECT 13
USE
SELECT 14
USE
ERASE FILE &W_ARCH
SELECT st_iprep
USE
SELECT st_idped
USE
STORE 0 TO repu, iva, des, neto,  ;
      total
iva = wk_totigv
neto = wk_totnet
total = wk_monto
IF wk_coddoc = 'FACT'
     @ 36, 05 SAY 'S/.'
     @ 36, 10 SAY (sol_cosrep *  ;
       (1 + wrk_facigv)) PICTURE  ;
       '999,999.99'
     @ 36, 62 SAY 'S/.'
     @ 36, 67 SAY sol_totvta  ;
       PICTURE '999,999.99'
     @ 37, 10 SAY (sol_cosmob *  ;
       (1 + wrk_facigv)) PICTURE  ;
       '999,999.99'
     @ 37, 67 SAY sol_igv PICTURE  ;
       '999,999.99'
     @ 38, 10 SAY sol_descue  ;
       PICTURE '999,999.99'
     @ 38, 67 SAY sol_totpag  ;
       PICTURE '999,999.99'
     @ 39, 01 SAY 'A Cta.:'
     @ 39, 10 SAY wrk_toacta  ;
       PICTURE '999,999.99'
     @ 41, 01 SAY  ;
       oonumlet(sol_totpag)
     @ 43, 00 SAY wrk_infte1
     IF wrk_cont01 > 0
          @ 43, 40 SAY nrodoc(1) +  ;
            ' ' +  ;
            TRANSFORM(monto(1),  ;
            '9,999.99')
     ENDIF
     @ 44, 00 SAY wrk_infte2
     IF wrk_cont01 > 1
          @ 44, 40 SAY nrodoc(2) +  ;
            ' ' +  ;
            TRANSFORM(monto(2),  ;
            '9,999.99')
     ENDIF
     @ 45, 00 SAY wrk_infte3
     IF wrk_cont01 > 2
          @ 45, 40 SAY nrodoc(3) +  ;
            ' ' +  ;
            TRANSFORM(monto(3),  ;
            '9,999.99')
     ENDIF
     @ 45, 60 SAY numvta
ELSE
     @ 35, 05 SAY 'S/.'
     @ 35, 10 SAY (sol_cosrep *  ;
       (1 + wrk_facigv)) PICTURE  ;
       '999,999.99'
     @ 35, 62 SAY 'S/.'
     @ 35, 67 SAY sol_totpag  ;
       PICTURE '999,999.99'
     @ 36, 10 SAY (sol_cosmob *  ;
       (1 + wrk_facigv)) PICTURE  ;
       '999,999.99'
     @ 37, 10 SAY sol_descue  ;
       PICTURE '999,999.99'
     @ 38, 01 SAY 'A Cta.:'
     @ 38, 10 SAY wrk_toacta  ;
       PICTURE '999,999.99'
     @ 38, 67 SAY sol_totpag  ;
       PICTURE '999,999.99'
     @ 39, 01 SAY  ;
       oonumlet(sol_totpag)
     @ 41, 00 SAY wrk_infte1
     @ 42, 00 SAY wrk_infte2
     IF wrk_cont01 > 0
          @ 42, 40 SAY nrodoc(1) +  ;
            ' ' +  ;
            TRANSFORM(monto(1),  ;
            '9,999.99')
     ENDIF
     @ 43, 00 SAY wrk_infte3
     IF wrk_cont01 > 1
          @ 43, 40 SAY nrodoc(2) +  ;
            ' ' +  ;
            TRANSFORM(monto(2),  ;
            '9,999.99')
     ENDIF
     @ 44, 00 SAY wrk_infte4
     IF wrk_cont01 > 2
          @ 44, 40 SAY nrodoc(3) +  ;
            ' ' +  ;
            TRANSFORM(monto(3),  ;
            '9,999.99')
     ENDIF
     @ 44, 60 SAY numvta
ENDIF
EJECT
SET PRINTER OFF
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
USE SHARED st_iparg
IF wk_coddoc = 'FACT'
     REPLACE sys_lptfac WITH '0'
ELSE
     REPLACE sys_lptbol WITH '0'
ENDIF
USE
KEYBOARD '{ESC}' PLAIN
RETURN
*
PROCEDURE ayufac
ON KEY LABEL F6
IF VARREAD() = 'WK_CODPAG'
     SELECT 12
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'FPAG'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'FORMAS DE PAGO'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SELECT 12
     USE
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
wk_printer = '1'
DO WHILE wk_printer='1'
     USE SHARED st_iparg
     IF wk_coddoc = 'FACT'
          wk_printer = sys_lptfac
     ELSE
          wk_printer = sys_lptbol
     ENDIF
     IF wk_printer = '1'
          DO mensa WITH  ;
             '*** Impresora Ocupada, FACTURANDO ***',  ;
             'COLO'
          = INKEY(1, 'H')
     ELSE
          DO mensa WITH  ;
             '*** Impresora Ocupada, FACTURANDO ***',  ;
             'SACA'
          IF wk_coddoc = 'FACT'
               REPLACE sys_lptfac  ;
                       WITH '1'
          ELSE
               REPLACE sys_lptbol  ;
                       WITH '1'
          ENDIF
     ENDIF
     USE
ENDDO
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
          IF wrk_desadi >  ;
             wk_totbru
               DO error WITH  ;
                  '*** Descuento Mayor al Monto Total ***'
               RETURN .F.
          ENDIF
ENDCASE
*
PROCEDURE pcta
PARAMETER wrk_numsol
DO mensa WITH  ;
   '*** Buscando Pagos a Cuenta ***',  ;
   'COLO'
SELECT gc_hve00.hve_tipdoc,  ;
       gc_hve00.hve_nrodoc,  ;
       gc_hve00.hve_fecdoc,  ;
       gc_hve00.hve_estdoc,  ;
       gc_hve00.hve_solgen,  ;
       gc_hve00.hve_nrdore FROM  ;
       GC_HVE00 WHERE  ;
       VAL(gc_hve00.hve_nrdore) ==  ;
       VAL(wrk_numsol) AND  ;
       gc_hve00.hve_estdoc <> 'A'  ;
       ORDER BY  ;
       gc_hve00.hve_fecdoc INTO  ;
       CURSOR ACTA
w_arch2 = f_archivo()
COPY TO &W_ARCH2
SELECT 19
USE &W_ARCH2
COUNT TO wrk_cont01
IF wrk_cont01 > 0
     GOTO TOP
     DIMENSION nrodoc[  ;
               wrk_cont01],  ;
               monto[  ;
               wrk_cont01]
     FOR a = 1 TO wrk_cont01
          nrodoc[ a] = hve_nrodoc
          monto[ a] = hve_solgen
          wrk_toacta = wrk_toacta +  ;
                       hve_solgen
          SKIP
     ENDFOR
ENDIF
SELECT 19
USE
ERASE FILE &W_ARCH2
CLOSE DATABASES
DO mensa WITH  ;
   '*** Buscando Pagos a Cuenta ***',  ;
   'SACA'
RETURN
*
PROCEDURE acta
PARAMETER wrk_numsol
USE SHARED gc_hve00 ORDER nrdore
STORE 0 TO wrk_toacta, a
DIMENSION nrodoc[ 4], monto[ 4]
GOTO TOP
SEEK wrk_numsol
SCAN WHILE  ;
     VAL(gc_hve00.hve_nrdore) =  ;
     VAL(wrk_numsol) .AND.  .NOT.  ;
     EOF()
     IF gc_hve00.hve_estdoc <>  ;
        'A'
          a = a + 1
          nrodoc( a) =  ;
                gc_hve00.hve_nrodoc
          monto( a) =  ;
               gc_hve00.hve_solgen
          wrk_toacta = wrk_toacta +  ;
                       hve_solgen
     ENDIF
ENDSCAN
wrk_cont01 = a
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

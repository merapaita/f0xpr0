*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
wrk_progra = PROGRAM()
DO crea_win
tit_prg = 'MANTENCION'
ACTIVATE WINDOW trabajo
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 19, 77
CLEAR TYPEAHEAD
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'FACTURACION DE SUCURSALES'
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
SELECT 3
USE SHARED st_iclpr ORDER codigo
SELECT 4
USE SHARED gc_hve00 ORDER codigo
valor = .T.
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     w_facigv = 1 + (tab_factor /  ;
                100)
ELSE
     do error with '** No esta Definido el &empre9 **'
     valor = .F.
ENDIF
STORE SPACE(4) TO w_docvta,  ;
      w_codemi, w_numero, w_linea,  ;
      w_tipgar
STORE '001 ' TO w_codpag
STORE 'SOL ' TO w_codmon
DO WHILE valor
     @ 07, 00 CLEAR
     DIMENSION codem( 30), descr(  ;
               30), unida( 30),  ;
               cosre( 30), cosmo(  ;
               30), otrin( 30),  ;
               linea( 30), totas(  ;
               30)
     STORE 0 TO w_monmob,  ;
           w_monrep, w_tipcam,  ;
           w_solpag, w_unidad,  ;
           w_solfle, w_dolfle,  ;
           w_mondes, w_monto,  ;
           w_numdoc, w_codcli,  ;
           w_numsol, w_otring,  ;
           w_dolmob, w_dolmib,  ;
           w_doligv, w_dolvta,  ;
           w_doldes, w_doltot,  ;
           w_dolmon, w_dolimp,  ;
           w_solmob, w_solrep,  ;
           w_soligv, w_moacta,  ;
           w_solvta, w_soldes,  ;
           w_solmon, w_solimp,  ;
           w_actaso, w_actado
     FOR i = 1 TO 30
          codem( i) = SPACE(4)
          descr( i) = SPACE(10)
          linea( i) = SPACE(4)
          cosre( i) = 0
          cosmo( i) = 0
          otrin( i) = 0
          totas( i) = 0
          unida( i) = 0
     ENDFOR
     STORE DATE() TO w_fchdoc
     STORE SPACE(30) TO w_nomcli
     @ 04, 01 SAY  ;
       'Tipo Documento:'
     @ 04, 37 SAY 'No.Documento:'
     @ 05, 01 SAY  ;
       'Fecha Document:'
     @ 05, 37 SAY 'Tip Atenci¢n:'
     @ 06, 01 SAY  ;
       'Moneda .......:'
     @ 06, 37 SAY 'Emisor .....:'
     @ 07, 01 SAY  ;
       'Forma de Pago :'
     @ 07, 37 SAY 'C¢d.Cliente :'
     @ 08, 01 SAY  ;
       'Nombre Cliente:'
     @ 15, 01 SAY 'M.Obra:'
     @ 16, 01 SAY 'Reptos:'
     @ 15, 24 SAY 'O.Ings:'
     @ 16, 24 SAY 'Dscto.:'
     @ 15, 48 SAY 'A Cta.:'
     @ 16, 48 SAY 'Total.: ->'
     SET CURSOR ON
     @ 04, 17 GET w_docvta  ;
       PICTURE '@!' VALID  ;
       despues(1) WHEN antes(1)
     @ 04, 51 GET w_numdoc  ;
       PICTURE '9999999999' VALID  ;
       despues(2) WHEN antes(2)
     @ 05, 17 GET w_fchdoc  ;
       PICTURE '@D' VALID  ;
       despues(3) WHEN antes(2)
     @ 05, 51 GET w_tipgar  ;
       PICTURE '@!' VALID  ;
       despues(12) WHEN  ;
       antes(12)
     @ 06, 17 GET w_codmon  ;
       PICTURE '@!' VALID  ;
       despues(10) WHEN  ;
       antes(10)
     @ 06, 51 GET w_codemi  ;
       PICTURE '@!' VALID  ;
       despues(6) WHEN antes(6)
     @ 07, 17 GET w_codpag  ;
       PICTURE '@!' VALID  ;
       despues(11) WHEN  ;
       antes(11)
     @ 07, 51 GET w_codcli  ;
       PICTURE '99999999999'  ;
       VALID despues(4) WHEN  ;
       antes(4)
     @ 08, 17 GET w_nomcli  ;
       PICTURE '@!' VALID  ;
       despues(5) WHEN antes(2)
     @ 15, 09 GET w_monmob  ;
       PICTURE '999,999,999.99'  ;
       VALID w_monmob >= 0 WHEN  ;
       antes(2)
     @ 16, 09 GET w_monrep  ;
       PICTURE '999,999,999.99'  ;
       VALID w_monrep >= 0
     @ 15, 32 GET w_otring  ;
       PICTURE '999,999,999.99'  ;
       VALID despues(17) .AND.  ;
       w_otring >= 0
     @ 16, 32 GET w_mondes  ;
       PICTURE '999,999,999.99'  ;
       VALID despues(18) .AND.  ;
       w_mondes >= 0
     @ 15, 61 GET w_moacta  ;
       PICTURE '999,999,999.99'  ;
       VALID despues(13) .AND.  ;
       w_moacta >= 0 WHEN  ;
       antes(2)
     READ
     IF LASTKEY() = 27
          valor = .F.
     ELSE
          LOOP
     ENDIF
ENDDO
CLOSE DATABASES
RELEASE WINDOW deta, crea_p
DO sacawin
RETURN
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
          ON KEY LABEL F6 do ayuda with;
1
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
     CASE opc = 4 .OR. opc = 6  ;
          .OR. opc = 12 .OR. opc =  ;
          14
          IF opc = 4
               ON KEY LABEL F6 do ayuda;
with 4
          ENDIF
          IF opc = 6
               ON KEY LABEL F6 do ayuda;
with 6
          ENDIF
          IF opc = 12
               ON KEY LABEL F6 do ayuda;
with 12
          ENDIF
          IF opc = 14
               ON KEY LABEL F6 do ayuda;
with 14
          ENDIF
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
     CASE opc = 10 .OR. opc = 11
          IF opc = 10
               ON KEY LABEL F6 do ayuda;
with 10
          ELSE
               ON KEY LABEL F6 do ayuda;
with 11
          ENDIF
ENDCASE
RETURN
*
PROCEDURE ante2
PARAMETER opc
DO CASE
     CASE opc = 15
          ACTIVATE WINDOW crea_p
          ON KEY LABEL F6 do ayuda with;
15
     CASE opc = 16
          ACTIVATE WINDOW deta
          ON KEY LABEL F6 do ayuda with;
16
ENDCASE
RETURN
*
FUNCTION despues
PARAMETER opc
ON KEY LABEL f6
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               ON KEY LABEL F6 do ayuda;
with 1
               RETURN .F.
          ENDIF
          IF EMPTY(w_docvta)
               ON KEY LABEL F6 do ayuda;
with 1
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' + w_docvta
          IF  .NOT. FOUND()
               ON KEY LABEL F6 do ayuda;
with 1
               DO error WITH  ;
                  'Documento de Venta No Existe'
               RETURN .F.
          ENDIF
          @ 04, 22 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            14)
     CASE opc = 2
          IF EMPTY(w_numdoc)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          @ 04, 62 SAY  ;
            f_ceros(w_numdoc,10, ;
            1)
          SELECT gc_hve00
          SEEK w_docvta +  ;
               f_ceros(w_numdoc, ;
               10,1)
          IF FOUND()
               DO error WITH  ;
                  '*** N£mero de Documento Ya Existe ***'
               RETURN .F.
          ENDIF
          w_numero = f_ceros(w_numdoc, ;
                     10,1)
     CASE opc = 3
          IF EMPTY(w_fchdoc)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT gc_cmv00
          SEEK DTOS(w_fchdoc) +  ;
               '1' + 'SOL ' +  ;
               'DOL '
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** No Existe Tipo de Cambio de esta Fecha ***'
               RETURN .F.
          ENDIF
          w_tipcam = cmv_tipcav
     CASE opc = 4
          IF EMPTY(w_codcli)
               ON KEY LABEL F6 do ayuda;
with 4
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT st_iclpr
          SEEK 'C' + STR(w_codcli,  ;
               11)
          IF FOUND()
               w_nomcli = noment
          ENDIF
     CASE opc = 5
          IF EMPTY(w_nomcli)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          IF SUBSTR(w_tipgar, 1,  ;
             1) = 'F'
               @ 13, 01 SAY  ;
                 'L¡nea :' GET  ;
                 w_linea PICTURE  ;
                 '9999' VALID  ;
                 despues(14) WHEN  ;
                 antes(14)
               @ 14, 01 SAY  ;
                 'Unidad:' GET  ;
                 w_unidad PICTURE  ;
                 '999,999' VALID  ;
                 w_unidad >= 0
               READ
          ELSE
               = garanti()
          ENDIF
     CASE opc = 6
          IF EMPTY(w_codemi)
               ON KEY LABEL f6 do ayuda;
with 6
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + w_codemi
          IF  .NOT. FOUND()
               ON KEY LABEL F6 do ayuda;
with 6
               DO error WITH  ;
                  'C¢digo de Emisor No Existe'
               RETURN .F.
          ENDIF
          @ 06, 56 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
     CASE opc = 10
          IF EMPTY(w_codmon)
               ON KEY LABEL F6 do ayuda;
with 10
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'MONE' + w_codmon
          IF  .NOT. FOUND()
               ON KEY LABEL F6 do ayuda;
with 10
               DO error WITH  ;
                  'C¢digo de Moneda No Existe'
               RETURN .F.
          ENDIF
          @ 06, 22 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            14)
     CASE opc = 11
          IF EMPTY(w_codpag)
               ON KEY LABEL F6 do ayuda;
with 11
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'FPAG' + w_codpag
          IF  .NOT. FOUND()
               ON KEY LABEL F6 do ayuda;
with 11
               DO error WITH  ;
                  'C¢digo de Pago No Existe'
               RETURN .F.
          ENDIF
          @ 07, 22 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            10)
     CASE opc = 12
          IF EMPTY(w_tipgar)
               ON KEY LABEL F6 do ayuda;
with 12
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'INGA' + w_tipgar
          IF  .NOT. FOUND()
               ON KEY LABEL F6 do ayuda;
with 12
               DO error WITH  ;
                  'C¢digo de Atenci¢n no Existe'
               RETURN .F.
          ENDIF
          @ 05, 56 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
     CASE opc = 13
          IF w_moacta < 0
               RETURN .F.
          ENDIF
          w_monto = (w_monmob +  ;
                    w_monrep +  ;
                    w_otring) -  ;
                    (w_mondes +  ;
                    w_moacta)
          @ 16, 61 SAY w_monto  ;
            PICTURE  ;
            '999,999,999.99'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'GRA',  ;
             'BBB'
          IF w_monto >= 0
               DO WHILE .T.
                    = INKEY(0,  ;
                      'H')
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    IF LASTKEY() = - ;
                       1
                         DO calculo
                         DO graba
                         EXIT
                    ENDIF
               ENDDO
          ENDIF
     CASE opc = 14 .OR. opc = 16
          IF EMPTY(w_linea) .AND.  ;
             opc = 14
               IF opc = 14
                    ON KEY LABEL F6 do;
ayuda with 14
               ELSE
                    ON KEY LABEL F6 do;
ayuda with 16
               ENDIF
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          IF opc = 14
               SEEK 'LINE' +  ;
                    w_linea
          ELSE
               SEEK 'LINE' +  ;
                    detall.linea
          ENDIF
          IF  .NOT. FOUND()
               IF opc = 14
                    ON KEY LABEL F6 do;
ayuda with 14
               ELSE
                    ON KEY LABEL F6 do;
ayuda with 16
               ENDIF
               DO error WITH  ;
                  'C¢digo No Existe'
               RETURN .F.
          ELSE
               IF opc = 14
                    IF w_linea <>  ;
                       '0003'
                         w_linea =  ;
                          '0002'
                         SEEK 'LINE' +  ;
                              w_linea
                    ENDIF
               ELSE
                    SELECT detall
                    IF linea <>  ;
                       '0003'
                         REPLACE linea  ;
                                 WITH  ;
                                 '0002'
                         SELECT ge_tab0
                         SEEK 'LINE' +  ;
                              detall.linea
                         SELECT detall
                    ENDIF
                    w_recno = RECNO()
                    w_codem2 = codemi
                    w_linea = linea
                    GOTO TOP
                    LOCATE FOR  ;
                           codemi =  ;
                           w_codem2  ;
                           .AND.  ;
                           linea =  ;
                           w_linea  ;
                           .AND.  ;
                           inde =  ;
                           'A'
                    CONTINUE
                    IF FOUND()
                         GOTO w_recno
                         ON KEY LABEL;
F6 do ayuda with 16
                         DO error  ;
                            WITH  ;
                            'C¢digo Duplicado'
                         RETURN .F.
                    ELSE
                         GOTO w_recno
                    ENDIF
               ENDIF
          ENDIF
          IF opc <> 16
               @ ROW(), 14 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 17)
          ENDIF
     CASE opc = 17
          IF w_otring < 0
               RETURN .F.
          ENDIF
          w_monto = (w_monmob +  ;
                    w_monrep +  ;
                    w_otring) -  ;
                    (w_mondes +  ;
                    w_moacta)
          @ 16, 61 SAY w_monto  ;
            PICTURE  ;
            '999,999,999.99'
     CASE opc = 18
          IF w_mondes < 0
               RETURN .F.
          ENDIF
          w_monto = (w_monmob +  ;
                    w_monrep +  ;
                    w_otring) -  ;
                    (w_mondes +  ;
                    w_moacta)
          @ 16, 61 SAY w_monto  ;
            PICTURE  ;
            '999,999,999.99'
ENDCASE
RETURN
*
FUNCTION despue2
PARAMETER opc
ON KEY LABEL f6
DO CASE
     CASE opc = 15
          IF EMPTY(codigo)
               ON KEY LABEL f6 do ayuda;
with 15
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + codigo
          IF  .NOT. FOUND()
               ON KEY LABEL f6 do ayuda;
with 15
               DO error WITH  ;
                  'C¢digo de Emisor No Existe'
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
PROCEDURE calculo
IF w_codmon = 'SOL '
     w_actaso = w_moacta
     w_solmob = ROUND(w_monmob /  ;
                w_facigv, 2)
     w_solrep = ROUND(w_monrep /  ;
                w_facigv, 2)
     w_solfle = w_otring
     w_soldes = w_mondes
     w_solimp = w_monmob +  ;
                w_monrep +  ;
                w_otring
     w_solmon = (w_monmob +  ;
                w_monrep +  ;
                w_otring) -  ;
                w_soldes
     w_solpag = w_solmon -  ;
                w_actaso
     w_solvta = ROUND(w_solpag /  ;
                w_facigv, 2)
     w_soligv = w_solpag -  ;
                w_solvta
     w_dolmob = ROUND((w_solmob /  ;
                w_tipcam), 2)
     w_dolmib = ROUND((w_solrep /  ;
                w_tipcam), 2)
     w_dolfle = ROUND((w_solfle /  ;
                w_tipcam), 2)
     w_doldes = ROUND((w_mondes /  ;
                w_tipcam), 2)
     w_dolimp = ROUND(w_dolmob *  ;
                w_facigv, 2) +  ;
                ROUND(w_dolmib *  ;
                w_facigv, 2) +  ;
                w_dolfle
     w_dolmon = (w_dolimp -  ;
                w_doldes)
     w_actado = ROUND(w_moacta /  ;
                w_tipcam, 2)
     w_doltot = w_dolmon -  ;
                w_actado
     w_dolvta = ROUND(w_doltot /  ;
                w_facigv, 2)
     w_doligv = w_doltot -  ;
                w_dolvta
ELSE
     w_dolmob = ROUND(w_monmob /  ;
                w_facigv, 2)
     w_dolmib = ROUND(w_monrep /  ;
                w_facigv, 2)
     w_dolfle = w_otring
     w_doldes = w_mondes
     w_dolimp = w_monmob +  ;
                w_monrep +  ;
                w_dolfle
     w_dolmon = w_dolimp -  ;
                w_doldes
     w_actado = w_moacta
     w_doltot = w_dolmon -  ;
                w_actado
     w_dolvta = ROUND(w_doltot /  ;
                w_facigv, 2)
     w_doligv = w_doltot -  ;
                w_dolvta
     w_solmob = ROUND(w_dolmob *  ;
                w_tipcam, 2)
     w_solrep = ROUND(w_dolmib *  ;
                w_tipcam, 2)
     w_solfle = ROUND(w_dolfle *  ;
                w_tipcam, 2)
     w_soldes = ROUND(w_mondes *  ;
                w_tipcam, 2)
     w_solimp = ROUND(w_solmob *  ;
                w_facigv, 2) +  ;
                ROUND(w_solrep *  ;
                w_facigv, 2) +  ;
                w_solfle
     w_solmon = w_solimp -  ;
                w_soldes
     w_actaso = ROUND(w_moacta *  ;
                w_tipcam, 2)
     w_solpag = w_solmon -  ;
                w_actaso
     w_solvta = ROUND(w_solpag /  ;
                w_facigv, 2)
     w_soligv = w_solpag -  ;
                w_solvta
ENDIF
RETURN
*
PROCEDURE graba
DO mensa WITH 'Grabando   '+ ;
   w_docvta+' N§ '+w_numero,  ;
   'COLO'
SELECT gc_hve00
APPEND BLANK
DO rbloquea
REPLACE hve_tipdoc WITH w_docvta
REPLACE hve_nrodoc WITH w_numero
REPLACE hve_fecdoc WITH w_fchdoc
REPLACE hve_fecvct WITH DATE()
REPLACE hve_codent WITH  ;
        STR(w_codcli, 11)
REPLACE hve_codemi WITH w_codemi
REPLACE hve_tipent WITH 'C   '
REPLACE hve_codmon WITH w_codmon
REPLACE hve_fechtc WITH w_fchdoc
REPLACE hve_tidore WITH 'ORDE'
REPLACE hve_nrdore WITH  ;
        STR(w_numsol, 8)
REPLACE hve_ncre WITH w_unidad
REPLACE hve_cosmob WITH w_dolmob
REPLACE hve_cosrep WITH w_dolmib
REPLACE hve_flete WITH w_dolfle
REPLACE hve_totnet WITH w_dolimp
REPLACE hve_totdes WITH w_doldes
REPLACE hve_totgen WITH w_dolmon
REPLACE hve_pagctd WITH w_actado
REPLACE hve_totvta WITH w_dolvta
REPLACE hve_totigv WITH w_doligv
REPLACE hve_totoim WITH w_doltot
REPLACE hve_codent WITH  ;
        STR(w_codcli, 11)
REPLACE hve_tippag WITH w_codpag
REPLACE hve_codmov WITH 'EVTA'
REPLACE hve_estdoc WITH 'O'
REPLACE hve_tipcam WITH w_tipcam
REPLACE hve_indori WITH w_tipgar
REPLACE hve_solmob WITH w_solmob
REPLACE hve_solrep WITH w_solrep
REPLACE hve_solfle WITH w_solfle
REPLACE hve_solnet WITH w_solimp
REPLACE hve_soldes WITH w_soldes
REPLACE hve_solvta WITH w_solvta
REPLACE hve_soligv WITH w_soligv
REPLACE hve_solgen WITH w_solmon
REPLACE hve_pagcts WITH w_actaso
REPLACE hve_mtocan WITH w_solpag
REPLACE hve_usuari WITH users
REPLACE hve_hora WITH TIME()
REPLACE hve_fecha WITH DATE()
UNLOCK
SELECT st_iclpr
SEEK 'C' + STR(w_codcli, 11)
IF  .NOT. FOUND()
     APPEND BLANK
     DO rbloquea
     REPLACE indent WITH 'C',  ;
             codent WITH  ;
             STR(w_codcli, 11),  ;
             noment WITH  ;
             w_nomcli
     REPLACE user WITH users
     REPLACE date WITH DATE()
     REPLACE time WITH TIME()
     UNLOCK
ENDIF
SELECT 20
USE SHARED st_idfac
IF SUBSTR(w_tipgar, 1, 1) <> 'F'  ;
   .AND.  .NOT. EMPTY(codem(1))
     SELECT detall
     GOTO TOP
     SCAN WHILE  .NOT. EOF()
          SELECT st_idfac
          APPEND BLANK
          DO rbloquea
          REPLACE tipdoc WITH  ;
                  w_docvta,  ;
                  nrodoc WITH  ;
                  w_numero, linea  ;
                  WITH  ;
                  detall.linea,  ;
                  codemi WITH  ;
                  detall.codemi,  ;
                  cantid WITH  ;
                  detall.unidad
          REPLACE codmon WITH  ;
                  w_codmon,  ;
                  fechad WITH  ;
                  w_fchdoc, user  ;
                  WITH users,  ;
                  date WITH  ;
                  DATE(), time  ;
                  WITH TIME()
          IF codmon = 'DOL '
               REPLACE cosrep  ;
                       WITH  ;
                       detall.cosrep,  ;
                       cosmob  ;
                       WITH  ;
                       detall.cosmob,  ;
                       otring  ;
                       WITH  ;
                       detall.otring
               w_solrep = ROUND(detall.cosrep /  ;
                          w_facigv,  ;
                          2)
               w_solrep = ROUND(ROUND(w_solrep *  ;
                          w_tipcam,  ;
                          2) *  ;
                          w_facigv,  ;
                          2)
               w_solmob = ROUND(detall.cosmob /  ;
                          w_facigv,  ;
                          2)
               w_solmob = ROUND(ROUND(w_solmob *  ;
                          w_tipcam,  ;
                          2) *  ;
                          w_facigv,  ;
                          2)
               w_solfle = ROUND(detall.otring *  ;
                          w_tipcam,  ;
                          2)
               REPLACE solrep  ;
                       WITH  ;
                       w_solrep,  ;
                       solmob  ;
                       WITH  ;
                       w_solmob,  ;
                       solotr  ;
                       WITH  ;
                       w_solfle
          ELSE
               REPLACE solrep  ;
                       WITH  ;
                       detall.cosrep,  ;
                       solmob  ;
                       WITH  ;
                       detall.cosmob,  ;
                       solotr  ;
                       WITH  ;
                       detall.otring
               w_dolrep = ROUND(detall.cosrep /  ;
                          w_facigv,  ;
                          2)
               w_dolrep = ROUND(ROUND(w_dolrep /  ;
                          w_tipcam,  ;
                          2) *  ;
                          w_facigv,  ;
                          2)
               w_dolmob = ROUND(detall.cosmob /  ;
                          w_facigv,  ;
                          2)
               w_dolmob = ROUND(ROUND(w_dolmob /  ;
                          w_tipcam,  ;
                          2) *  ;
                          w_facigv,  ;
                          2)
               w_dolfle = ROUND(detall.otring /  ;
                          w_tipcam,  ;
                          2)
               REPLACE cosrep  ;
                       WITH  ;
                       w_dolrep,  ;
                       cosmob  ;
                       WITH  ;
                       w_dolmob,  ;
                       otring  ;
                       WITH  ;
                       w_dolfle
          ENDIF
          REPLACE totals WITH  ;
                  solrep + solmob +  ;
                  solotr, totald  ;
                  WITH cosrep +  ;
                  cosmob +  ;
                  otring
          UNLOCK
          SELECT detall
     ENDSCAN
ELSE
     IF EMPTY(w_linea)
          w_linea = '0002'
     ENDIF
     IF w_unidad = 0
          w_unidad = 1
     ENDIF
     APPEND BLANK
     DO rbloquea
     REPLACE tipdoc WITH w_docvta,  ;
             nrodoc WITH w_numero,  ;
             linea WITH w_linea,  ;
             codemi WITH w_codemi,  ;
             cantid WITH  ;
             w_unidad
     REPLACE codmon WITH w_codmon,  ;
             fechad WITH w_fchdoc,  ;
             user WITH users,  ;
             date WITH DATE(),  ;
             time WITH TIME()
     IF codmon = 'DOL '
          REPLACE cosrep WITH  ;
                  w_monrep,  ;
                  cosmob WITH  ;
                  w_monmob,  ;
                  otring WITH  ;
                  w_otring
          REPLACE solrep WITH  ;
                  ROUND(w_solrep *  ;
                  w_facigv, 2),  ;
                  solmob WITH  ;
                  ROUND(w_solmob *  ;
                  w_facigv, 2),  ;
                  solotr WITH  ;
                  w_solfle
     ELSE
          REPLACE solrep WITH  ;
                  w_monrep,  ;
                  solmob WITH  ;
                  w_monmob,  ;
                  solotr WITH  ;
                  w_otring
          REPLACE cosrep WITH  ;
                  ROUND(w_dolmib *  ;
                  w_facigv, 2),  ;
                  cosmob WITH  ;
                  ROUND(w_dolmob *  ;
                  w_facigv, 2),  ;
                  otring WITH  ;
                  w_dolfle
     ENDIF
     REPLACE totals WITH solrep +  ;
             solmob + solotr,  ;
             totald WITH cosrep +  ;
             cosmob + otring
     UNLOCK
ENDIF
DO mensa WITH  ;
   'Grabando   FACT N§ '+w_numero,  ;
   'SACA'
RETURN
*
PROCEDURE ayuda
PARAMETER opc
DO CASE
     CASE opc = 1
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'DOCU' .AND. (tab_codtab = 'BOLE' .OR. tab_codtab = 'FACT')
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DOC. VENTA'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
     CASE opc = 4
          SELECT st_iclpr
          SET FILTER TO indent == 'C'
          campoa = '"  "+codent+"  "+noment'
          campob = '"  "+noment+"  "+codent'
          titulo = 'AYUDA DE CLIENTES'
          DO ayuda2 WITH campoa,  ;
             campob, titulo,  ;
             'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
          SET FILTER TO
     CASE opc = 6 .OR. opc = 15
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'EMIS'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA EMISOR'
          IF opc = 6
               DO ayuda1 WITH  ;
                  campo, titulo,  ;
                  'tab_codtab'
          ELSE
               DO ayuda11 WITH  ;
                  campo, titulo,  ;
                  'tab_codtab'
          ENDIF
          SET FILTER TO
     CASE opc = 10
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'MONE'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA MONEDAS'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
     CASE opc = 11
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'FPAG'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA FORMA DE PAGO'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
     CASE opc = 12
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'INGA'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE ATENCION'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
     CASE opc = 14 .OR. opc = 16
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'LINE'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE LINEAS'
          IF opc = 14
               DO ayuda1 WITH  ;
                  campo, titulo,  ;
                  'tab_codtab'
          ELSE
               DO ayuda11 WITH  ;
                  campo, titulo,  ;
                  'tab_codtab'
          ENDIF
          SET FILTER TO
ENDCASE
RETURN
*
PROCEDURE garanti
DO esc_indica WITH 1, 'AYU',  ;
   'MD4', 'ELI', 'INT'
DO esc_indica WITH 2, 'RAC',  ;
   'BUS', 'BBB', 'ESC'
DEFINE WINDOW deta FROM 11, 00 TO  ;
       19, 77 IN screen
CREATE CURSOR detall (codemi C  ;
       (4), descri C (10), linea  ;
       C (4), cosrep N (12, 2),  ;
       cosmob N (12, 2), otring N  ;
       (12, 2), unidad N (7),  ;
       totas N (13, 2), inde C  ;
       (1))
SELECT detall
FOR x = 1 TO 30
     IF codem(x) <> SPACE(4)
          APPEND BLANK
          REPLACE codemi WITH  ;
                  codem(x),  ;
                  descri WITH  ;
                  SUBSTR(descr(x),  ;
                  1, 10)
          REPLACE inde WITH 'A',  ;
                  linea WITH  ;
                  linea(x)
          REPLACE unidad WITH  ;
                  unida(x),  ;
                  cosrep WITH  ;
                  cosre(x)
          REPLACE cosmob WITH  ;
                  cosmo(x),  ;
                  otring WITH  ;
                  otrin(x), totas  ;
                  WITH cosmob +  ;
                  cosrep +  ;
                  otring
     ENDIF
ENDFOR
COUNT FOR inde = 'A' TO n
IF n = 0
     DO crea_pro
ENDIF
SELECT detall
GOTO TOP
ACTIVATE WINDOW deta
ON KEY LABEL f3 do crea_pro
ON KEY LABEL f4 do eli_pro
BROWSE FOR inde <> 'N' FIELDS  ;
       codemi : 4 :R :H = 'EMIS',  ;
       descri :R : 10 :H =  ;
       'DESCRIPCIO', linea : 4 :H =  ;
       'LINE' :W = ante2(16) :V =  ;
       despues(16) :F, unidad :P =  ;
       '99,999' :H = 'UNIDAD' :V =  ;
       unidad > 0 :F, cosrep :P =  ;
       '9999,999.99' :H =  ;
       '  REPUESTOS' :V = cosrep >=  ;
       0, cosmob :P =  ;
       '9999,999.99' :H =  ;
       '  MANO OBRA' :V = cosmob >=  ;
       0, otring :P =  ;
       '9999,999.99' :H =  ;
       'OTROS INGRS' :V = otring >=  ;
       0, totas =  ;
       TRANSFORM(cosrep + cosmob +  ;
       otring, '9999,999.99') :H =  ;
       '   TOTALES' :R SAVE IN  ;
       deta COLOR SCHEME 11
DEACTIVATE WINDOW deta
ACTIVATE WINDOW trabajo
STORE 0 TO k, w_monmob, w_monrep,  ;
      w_otring
SELECT detall
GOTO TOP
SCAN WHILE  .NOT. EOF()
     IF codemi <> SPACE(4) .AND.  ;
        unidad > 0 .AND. inde =  ;
        'A'
          k = k + 1
          codem( k) = codemi
          descr( k) = descri
          linea( k) = linea
          unida( k) = unidad
          cosre( k) = cosrep
          cosmo( k) = cosmob
          otrin( k) = otring
          REPLACE totas WITH  ;
                  cosrep + cosmob +  ;
                  otring
          totas( k) = totas
          w_monmob = w_monmob +  ;
                     cosmob
          w_monrep = w_monrep +  ;
                     cosrep
          w_otring = w_otring +  ;
                     otring
     ENDIF
ENDSCAN
FOR i = k + 1 TO 30
     codem( i) = SPACE(4)
     descr( i) = SPACE(10)
     linea( i) = SPACE(4)
     unida( i) = 0
     cosre( i) = 0
     cosmo( i) = 0
     otrin( i) = 0
     totas( i) = 0
ENDFOR
FOR i = 1 TO 4
     IF codem(i) <> SPACE(4)
          @ 10 + i, 00 SAY  ;
            SPACE(76) COLOR  ;
            SCHEME 11
          @ 10 + i, 01 SAY  ;
            codem(i) COLOR SCHEME  ;
            11
          @ 10 + i, 06 SAY  ;
            descr(i) COLOR SCHEME  ;
            11
          @ 10 + i, 17 SAY  ;
            linea(i) COLOR SCHEME  ;
            11
          @ 10 + i, 22 SAY  ;
            unida(i) PICTURE  ;
            '99,999' COLOR SCHEME  ;
            11
          @ 10 + i, 29 SAY  ;
            cosre(i) PICTURE  ;
            '9999,999.99' COLOR  ;
            SCHEME 11
          @ 10 + i, 41 SAY  ;
            cosmo(i) PICTURE  ;
            '9999,999.99' COLOR  ;
            SCHEME 11
          @ 10 + i, 53 SAY  ;
            otrin(i) PICTURE  ;
            '9999,999.99' COLOR  ;
            SCHEME 11
          @ 10 + i, 64 SAY  ;
            totas(i) PICTURE  ;
            '9999,999.99' COLOR  ;
            SCHEME 11
     ELSE
          @ 10 + i, 01 SAY  ;
            SPACE(65)
     ENDIF
ENDFOR
DO esc_indica WITH 1, 'AYU',  ;
   'SOL', 'BBB', 'INT'
DO esc_indica WITH 2, 'RAC',  ;
   'BBB', 'IGN', 'ESC'
ON KEY LABEL f6
ON KEY LABEL f3
ON KEY LABEL f4
RETURN
*
FUNCTION crea_pro
COUNT FOR inde = 'A' TO fil
IF fil = 30
     DO error WITH  ;
        '*** No se puede ingresar m s de treinta***'
     RETURN .T.
ENDIF
IF fil = 0
     ACTIVATE WINDOW trabajo
     @ 09, 00 SAY REPLICATE('Ä',  ;
       76)
     @ 10, 00 SAY ' EMIS' +  ;
       SPACE(1) + 'DESCRIPCIO' +  ;
       SPACE(1) + 'LINE' +  ;
       SPACE(1) + 'UNIDAD' +  ;
       SPACE(1) + '  REPUESTOS' +  ;
       SPACE(1) + '  MANO OBRA' +  ;
       SPACE(1) + 'OTROS INGRS' +  ;
       SPACE(1) + '   TOTALES '  ;
       COLOR SCHEME 11
     DEFINE WINDOW crea_p FROM 13,  ;
            01 TO 14, 76 NOSHADOW  ;
            NONE COLOR SCHEME 11
ELSE
     IF fil > 4
          DEFINE WINDOW crea_p  ;
                 FROM 18, 01 TO  ;
                 19, 76 NONE  ;
                 COLOR SCHEME 11
     ELSE
          DEFINE WINDOW crea_p  ;
                 FROM 14 + fil,  ;
                 01 TO 15 + fil,  ;
                 76 NONE COLOR  ;
                 SCHEME 11
     ENDIF
ENDIF
ACTIVATE WINDOW crea_p
STORE SPACE(4) TO codigo
SET CURSOR ON
@ 00, 01 GET codigo PICTURE '@!'  ;
  VALID despue2(15) WHEN  ;
  ante2(15)
READ
IF LASTKEY() <> 27 .AND. codigo <>  ;
   SPACE(4)
     SELECT detall
     APPEND BLANK
     REPLACE codemi WITH codigo
     REPLACE descri WITH  ;
             SUBSTR(ge_tab0.tab_destab,  ;
             1, 10)
     REPLACE inde WITH 'A'
ENDIF
RELEASE WINDOW crea_p
RETURN
*
PROCEDURE eli_pro
REPLACE inde WITH 'N'
DEACTIVATE WINDOW deta
ACTIVATE WINDOW deta
SELECT detall
RETURN
*
PROCEDURE ayuda11
PARAMETER campo, mensaje, clave
ON KEY LABEL f6
ACTIVATE SCREEN
SAVE SCREEN TO wk_pantax
define popup ayu0 from 5,35 to 12,79 promp;
field &campo title mensaje
ON SELECTION POPUP ayu0 do choice0
ACTIVATE POPUP ayu0 NOWAIT
FOR i = 1 TO 5
     MOVE POPUP ayu0 BY 1, 0
ENDFOR
FOR i = 1 TO 9
     MOVE POPUP ayu0 BY 0, -2
ENDFOR
ACTIVATE POPUP ayu0
DEACTIVATE POPUP ayu0
ACTIVATE SCREEN
RESTORE SCREEN FROM wk_pantax
IF opc = 15
     ACTIVATE WINDOW crea_p
ELSE
     ACTIVATE WINDOW deta
ENDIF
RETURN
*
PROCEDURE choice0
IF LASTKEY() = 13
     keyboard &clave
     DEACTIVATE POPUP ayu0
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

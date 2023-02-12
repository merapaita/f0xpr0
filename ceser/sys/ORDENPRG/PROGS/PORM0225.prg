*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER w_opc
ON KEY
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
tit_prg = 'MANTENCION'
@ 2, 1 SAY DATE() COLOR SCHEME 8
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' FACTURACION DE PAGOS A CUENTA'
CLOSE DATABASES
SELECT 1
USE SHARED gc_par00
rge_monbas = par_monbas
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
SELECT 4
USE SHARED st_isrep ORDER codigo
SELECT 5
USE SHARED st_iclpr ORDER codigo
SELECT 6
USE SHARED st_iparg
SELECT 7
USE SHARED st_iorep ORDER  ;
    ord_numsol
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     w_igv = (tab_factor / 100)
ELSE
     DO error WITH  ;
        '** No est  Definido el IGV **'
     CLOSE DATABASES
     DO sacawin
     RETURN
ENDIF
w_fchpar = DATE()
SELECT gc_cmv00
SEEK DTOS(w_fchpar) + '1' +  ;
     'SOL ' + 'DOL '
w_tipcam = 3.25 
valor = .T.
DO WHILE valor
     STORE SPACE(4) TO w_coddis,  ;
           w_emisor, wk_coddoc,  ;
           w_docvta, w_concep,  ;
           w_indori
     STORE 'SOL ' TO w_codmon
     STORE 0 TO w_monto, w_codsol,  ;
           w_valigv, w_valvta,  ;
           w_abono, w_numdoc,  ;
           w_mobra, w_totgen
     STORE SPACE(8) TO w_numero,  ;
           w_numtel
     STORE SPACE(9) TO w_nomcal,  ;
           w_noment, w_desmon
     STORE SPACE(11) TO w_codcli,  ;
           w_codent
     STORE 0 TO sol_cosmob,  ;
           sol_subtot
     STORE 0 TO sol_totvta,  ;
           sol_igv, sol_totgen
     @ 05, 02 CLEAR TO 09, 71
     @ 04, 01 TO 10, 72
     @ 05, 51 SAY 'T.Cambio:'
     @ 05, 60 SAY w_tipcam  ;
       PICTURE '99.99'
     @ 05, 03 SAY  ;
       'Solicitud de Servicio N§.:'  ;
       COLOR W+/N 
     @ 06, 03 SAY  ;
       'Tipo de Doc. de Venta ...:'  ;
       COLOR W+/N 
     IF w_opc = 2
          @ 06, 51 SAY  ;
            'Nro.Doc.:' COLOR W+/ ;
            N 
     ENDIF
     @ 07, 03 SAY  ;
       'C¢digo de Moneda.........:'  ;
       COLOR W+/N 
     @ 08, 03 SAY  ;
       'Monto a Facturar ........:'  ;
       COLOR W+/N 
     @ 09, 03 SAY  ;
       'Concepto a Facturar .....:'  ;
       COLOR W+/N 
     @ 11, 01 TO 14, 72
     @ 12, 02 SAY 'Cliente :' +  ;
       SPACE(50)
     @ 13, 02 SAY 'Modelo  :' +  ;
       SPACE(50)
     SET CURSOR ON
     ON KEY LABEL f6 do ayupag
     @ 05, 30 GET w_codsol  ;
       PICTURE '99999999' VALID  ;
       despues(1) WHEN antes(1)
     @ 06, 30 GET w_docvta  ;
       PICTURE '@!' VALID  ;
       despues(2) WHEN antes(2)
     IF w_opc = 2
          @ 06, 60 GET w_numdoc  ;
            PICTURE '9999999999'  ;
            VALID despues(3) WHEN  ;
            antes(3)
     ENDIF
     @ 07, 30 GET w_codmon  ;
       PICTURE '@!' VALID  ;
       despues(4) WHEN antes(4)
     @ 08, 30 GET w_monto PICTURE  ;
       '999,999.99' VALID  ;
       despues(5) WHEN antes(5)
     @ 09, 30 GET w_concep  ;
       PICTURE '@!' VALID  ;
       despues(6) WHEN antes(6)
     READ
     IF LASTKEY() = 27
          STORE .F. TO valor
     ENDIF
ENDDO
CLOSE DATABASES
ON KEY LABEL f6
DO sacawin
RETURN
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 3
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 4
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 5
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 6
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
RETURN
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(w_codsol)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          w_numero = STR(w_codsol,  ;
                     8)
          SELECT st_isrep
          SEEK w_numero
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** N£mero de Solicitud No Existe ***'
               RETURN .F.
          ENDIF
          DO CASE
               CASE indest =  ;
                    'F   ' .OR.  ;
                    indest =  ;
                    'B   '
                    DO error WITH  ;
                       '*** S/Servicio se Encuentra Facturada ***'
                    RETURN .F.
               CASE indest =  ;
                    'C   '
                    DO error WITH  ;
                       '*** S/Servicio se Encuentra Cerrada ***'
                    RETURN .F.
               CASE indest =  ;
                    'A   '
                    DO error WITH  ;
                       '*** S/Servicio se Encuentra Anulada ***'
                    RETURN .F.
          ENDCASE
          w_codent = codent
          w_codcli = codent
          w_emisor = codemi
          w_abono = monabo
          w_indori = indori
          @ 12, 14 SAY w_codent
          @ 13, 14 SAY codmod
          @ 13, 38 SAY 'Serie :'
          @ 13, 46 SAY numser
          SELECT st_iclpr
          SEEK 'C' + w_codent
          IF FOUND()
               w_noment = noment
               w_nomcal = nomcal
               w_numtel = numte1
               w_coddis = nomdis
               @ 12, 24 SAY  ;
                 w_noment
          ENDIF
     CASE opc = 2
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(w_docvta)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' + w_docvta
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** Documento de Venta No Existe ***'
               RETURN .F.
          ELSE
               IF w_docvta <>  ;
                  'BOLE' .AND.  ;
                  w_docvta <>  ;
                  'FACT'
                    DO error WITH  ;
                       '*** No es Documento de Venta ***'
                    RETURN .F.
               ENDIF
          ENDIF
          @ 06, 35 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE opc = 3
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(w_numdoc)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          IF TYPE('w_NUMDOC') =  ;
             'N'
               w_numdoc = f_ceros(w_numdoc, ;
                          10,1)
          ELSE
               w_numdoc = f_ceros(w_numdoc, ;
                          10,2)
          ENDIF
          SELECT 20
          USE SHARED gc_hve00  ;
              ORDER codigo
          SEEK w_docvta +  ;
               w_numdoc
          IF FOUND()
               DO error WITH  ;
                  '*** Nro.de Documento  Existe ***'
               RETURN .F.
               USE
          ENDIF
          USE
          @ 6, 60 SAY w_numdoc
     CASE opc = 4
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(w_codmon)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'MONE' + w_codmon
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Moneda No Existe ***'
               RETURN .F.
          ENDIF
          @ 07, 35 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
          w_desmon = ALLTRIM(tab_destab)
     CASE opc = 5
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF w_monto <= 0
               DO error WITH  ;
                  '*** Monto Invalido  ***'
               RETURN .F.
          ENDIF
     CASE opc = 6
          IF EMPTY(w_concep)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'ACTA' + w_concep
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Concepto Venta No Existe ***'
               RETURN .F.
          ENDIF
          IF w_concep = 'PCTA'
               DO error WITH  ;
                  '*** C¢digo de Concepto Venta No Permitido ***'
               RETURN .F.
          ENDIF
          IF w_concep = '004 '
               SELECT st_iorep
               SEEK w_numero
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '*** No Existe Orden de Reparaci¢n ***'
                    RETURN .F.
               ELSE
                    IF ALLTRIM(indest) =  ;
                       'C'
                         DO error  ;
                            WITH  ;
                            '*** Orden de Reparaci¢n ya esta Cerrada ***'
                         RETURN .F.
                    ENDIF
                    IF w_concep =  ;
                       '004 '  ;
                       .AND.  ;
                       auxest <>  ;
                       '003 '
                         DO error  ;
                            WITH  ;
                            '***  Orden no esta en Presupuesto ***'
                         RETURN .F.
                    ENDIF
               ENDIF
          ENDIF
          SELECT ge_tab0
          w_descon = tab_destab
          @ 09, 35 SAY  ;
            SUBSTR(w_descon, 1,  ;
            29)
          IF w_opc = 1
               DO esc_indica WITH  ;
                  1, 'AYU', 'BBB',  ;
                  'IMP', 'BBB'
          ELSE
               DO esc_indica WITH  ;
                  1, 'AYU', 'BBB',  ;
                  'GRA', 'BBB'
          ENDIF
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          DO WHILE .T.
               = INKEY(0, 'H')
               IF LASTKEY() = 27
                    EXIT
               ENDIF
               IF (LASTKEY() = -6  ;
                  .AND. w_opc =  ;
                  1) .OR.  ;
                  (LASTKEY() = -1  ;
                  .AND. w_opc =  ;
                  2)
                    ON KEY
                    DO calculo
                    EXIT
               ENDIF
          ENDDO
ENDCASE
RETURN
*
PROCEDURE calculo
IF w_codmon = 'SOL '
     w_mobra = ROUND(ROUND(w_monto /  ;
               (1 + w_igv), 2) /  ;
               w_tipcam, 2)
     w_totgen = ROUND(w_mobra *  ;
                (1 + w_igv), 2)
     w_valvta = ROUND(w_totgen /  ;
                (1 + w_igv), 2)
     w_valigv = w_totgen -  ;
                w_valvta
     sol_totgen = w_monto
     sol_totvta = ROUND(sol_totgen /  ;
                  (1 + w_igv),  ;
                  2)
     sol_igv = sol_totgen -  ;
               sol_totvta
ELSE
     w_mobra = ROUND(w_monto / (1 +  ;
               w_igv), 2)
     w_totgen = w_monto
     w_valvta = ROUND(w_totgen /  ;
                (1 + w_igv), 2)
     w_valigv = w_totgen -  ;
                w_valvta
     sol_totgen = ROUND(ROUND((w_mobra *  ;
                  w_tipcam), 2) *  ;
                  (1 + w_igv),  ;
                  2)
     sol_totvta = ROUND(sol_totgen /  ;
                  (1 + w_igv),  ;
                  2)
     sol_igv = sol_totgen -  ;
               sol_totvta
ENDIF
IF w_opc = 1
     DO stat_print
     w_numdoc = f_ceros(w_numdoc, ;
                10,1)
ENDIF
DO graba
RETURN
*
PROCEDURE stat_print
wk_printer = '1'
DO WHILE wk_printer='1'
     SELECT 6
     USE
     USE SHARED st_iparg
     IF w_docvta = 'FACT'
          wk_printer = sys_lptfac
     ELSE
          wk_printer = sys_lptbol
     ENDIF
     IF wk_printer = '1'
          DO mensa WITH  ;
             '*** Impresora Ocupada, FACTURANDO ***',  ;
             'COLO'
          = INKEY(2, 'H')
     ELSE
          DO mensa WITH  ;
             '*** Impresora Ocupada, FACTURANDO ***',  ;
             'SACA'
          IF w_docvta = 'FACT'
               REPLACE sys_lptfac  ;
                       WITH '1'
          ELSE
               REPLACE sys_lptbol  ;
                       WITH '1'
          ENDIF
     ENDIF
ENDDO
IF w_docvta = 'FACT'
     REPLACE sys_numfac WITH  ;
             sys_numfac + 1
     w_numdoc = sys_numfac
ELSE
     REPLACE sys_nrobol WITH  ;
             sys_nrobol + 1
     w_numdoc = sys_nrobol
ENDIF
RETURN
*
PROCEDURE graba
DO mensa WITH 'Grabando   N§ '+ ;
   w_numdoc, 'COLO'
SELECT 20
USE SHARED st_iredo ORDER codigo
APPEND BLANK
DO rbloquea
REPLACE indodo WITH 'SSE '
REPLACE numodo WITH w_numero
REPLACE indddo WITH w_docvta
REPLACE numddo WITH w_numdoc
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
UNLOCK
USE SHARED gc_hve00 ORDER codigo
APPEND BLANK
DO rbloquea
REPLACE hve_tipdoc WITH w_docvta
REPLACE hve_nrodoc WITH w_numdoc
REPLACE hve_tidore WITH 'SOLI'
REPLACE hve_nrdore WITH w_numero
REPLACE hve_fecdoc WITH DATE()
REPLACE hve_fecvct WITH DATE()
REPLACE hve_codemi WITH w_emisor
REPLACE hve_tipent WITH 'C   '
REPLACE hve_codent WITH w_codent
REPLACE hve_tippag WITH '001 '
REPLACE hve_codmov WITH 'PCTA'
REPLACE hve_estdoc WITH 'O'
REPLACE hve_indori WITH w_indori
REPLACE hve_totnet WITH w_totgen
REPLACE hve_totvta WITH w_valvta
REPLACE hve_totigv WITH w_valigv
REPLACE hve_totgen WITH w_totgen
REPLACE hve_totoim WITH w_totgen
REPLACE hve_codmon WITH w_codmon
REPLACE hve_fechtc WITH DATE()
REPLACE hve_codcta WITH w_concep
IF w_concep = '005'
     REPLACE hve_flete WITH  ;
             w_totgen
     REPLACE hve_solfle WITH  ;
             sol_totgen
ELSE
     REPLACE hve_cosmob WITH  ;
             w_mobra
     REPLACE hve_solmob WITH  ;
             sol_totvta
ENDIF
REPLACE hve_solnet WITH  ;
        sol_totgen
REPLACE hve_solvta WITH  ;
        sol_totvta
REPLACE hve_soligv WITH sol_igv
REPLACE hve_solgen WITH  ;
        sol_totgen
REPLACE hve_mtocan WITH  ;
        sol_totgen
REPLACE hve_tipcam WITH w_tipcam
REPLACE hve_usuari WITH users
REPLACE hve_fecha WITH DATE()
REPLACE hve_hora WITH TIME()
UNLOCK
IF w_concep = '004 '
     SELECT st_iorep
     SEEK w_numero
     IF FOUND() .AND. (indest =  ;
        'P' .OR. indest = 'V')
          DO rbloquea
          REPLACE auxest WITH  ;
                  '007', user  ;
                  WITH users,  ;
                  date WITH  ;
                  DATE(), time  ;
                  WITH TIME()
          UNLOCK
          SELECT 20
          USE SHARED st_mvord  ;
              ORDER ordia
          SEEK st_iorep.numdoc +  ;
               '007'
          IF  .NOT. FOUND()
               APPEND BLANK
               DO rbloquea
               REPLACE orden WITH  ;
                       st_iorep.numdoc,  ;
                       dia WITH  ;
                       DATE(),  ;
                       tecnico  ;
                       WITH  ;
                       st_iorep.codtec,  ;
                       estado  ;
                       WITH  ;
                       st_iorep.auxest,  ;
                       destado  ;
                       WITH  ;
                       ootab('ESOR', ;
                       estado)
               REPLACE inftec  ;
                       WITH  ;
                       st_iorep.observ,  ;
                       user WITH  ;
                       users,  ;
                       date WITH  ;
                       DATE(),  ;
                       time WITH  ;
                       TIME(),  ;
                       hora WITH  ;
                       TIME()
               UNLOCK
          ENDIF
     ENDIF
ENDIF
SELECT st_isrep
DO rbloquea
REPLACE monabo WITH w_abono +  ;
        w_totgen
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
UNLOCK
DO mensa WITH  ;
   'Grabando   FACT N§ '+w_numdoc,  ;
   'SACA'
IF w_opc = 1
     DO imprime
ENDIF
RETURN
*
PROCEDURE imprime
SET CONSOLE OFF
IF w_docvta = 'FACT'
     set print to &rge_lptfac  
ELSE
     set print to &rge_lptbol  
ENDIF
SET PRINTER ON
SET DEVICE TO PRINTER
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(67) + CHR(51)
? CHR(18)
w_numero = VAL(w_numero)
DO pormpta
w_numero = STR(w_numero, 8)
EJECT
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
SELECT st_iparg
IF w_docvta = 'FACT'
     REPLACE sys_lptfac WITH '0'
ELSE
     REPLACE sys_lptbol WITH '0'
ENDIF
valor = .T.
RETURN
*
PROCEDURE ayupag
IF VARREAD() == 'W_CODSOL'
     w_origen = 'SS'
     w_select = SELECT()
     SELECT st_isrep
     SET FILTER TO indest <> 'B';
.AND. indest <> 'F'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+SUBSTR(NUMSER,1,12)+"    "+codent+"  "+SUBSTR(codmod,1,10)+"  "+subst(indest,1,2)+"    "+alltrim(indori)'
     DO ayuda8 WITH campoa,  ;
        w_origen, w_select
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() = 'W_DOCVTA'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'DOCU'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'TIPO DE DOCUMENTO'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() = 'W_CODMON'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'MONE'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'TIPO DE MONEDA'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() = 'W_CONCEP'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'ACTA';
.AND. tab_codtab <> 'PCTA'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'CONCEPTO PAGO A CUENTA'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
ON KEY LABEL f6 do ayupag
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

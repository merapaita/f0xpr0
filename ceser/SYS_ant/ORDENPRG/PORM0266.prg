*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
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
    codigo_2
SELECT 4
USE SHARED ST_ISREP ORDER CODIGO
SELECT 5
USE SHARED ST_ICLPR ORDER CODIGO
SELECT 6
USE SHARED GC_HVE00 ORDER CODIGO
SELECT 7
USE SHARED ST_IPARG
SELECT 8
USE SHARED ST_IREDO ORDER CODIGO
SELECT 9
USE SHARED st_iorep ORDER  ;
    ord_numsol
SELECT 10
USE SHARED st_mvord ORDER ordia
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     wrk_igv = (tab_factor / 100)
ELSE
     DO error WITH  ;
        '** No est  Definido el IGV **'
     CLOSE DATABASES
     DO sacawin
     RETURN
ENDIF
wrk_fchpar = DATE()
SELECT gc_cmv00
SEEK DTOC(wrk_fchpar) + '1' +  ;
     'SOL ' + 'DOL '
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** No Existe Tipo de Cambio de esta Fecha ***'
     CLOSE DATABASES
     DO sacawin
     RETURN
ENDIF
wrk_tipcam = cmv_tipcav
valor = .T.
DO WHILE valor
     STORE SPACE(4) TO wrk_coddis,  ;
           wrk_codemi, wk_coddoc,  ;
           wrk_indori, wrk_docvta,  ;
           wrk_concep
     STORE 'SOL ' TO wrk_codmon
     STORE 0 TO wrk_monto,  ;
           wrk_codsol, wrk_valigv,  ;
           wrk_valvta, wrk_abono,  ;
           wrk_numdoc, wrk_mobra,  ;
           wrk_totgen
     STORE SPACE(8) TO wrk_numsol,  ;
           wrk_numtel
     STORE SPACE(9) TO wrk_codent,  ;
           wrk_dirent,  ;
           wrk_noment
     STORE 0 TO sol_cosmob,  ;
           sol_subtot
     STORE 0 TO sol_totvta,  ;
           sol_igv, sol_totgen
     @ 03, 01 TO 10, 72
     @ 04, 50 SAY  ;
       'Tipo de Cambio : '
     @ 04, 67 SAY wrk_tipcam  ;
       PICTURE '99.99'
     @ 04, 03 SAY  ;
       'Solicitud de Servicio N§.. :'  ;
       COLOR W+/N 
     @ 05, 03 SAY  ;
       'Tipo de Doc. de Venta .... :'  ;
       COLOR W+/N 
     @ 06, 03 SAY  ;
       'N£mero de Tipo de Documento:'  ;
       COLOR W+/N 
     @ 07, 03 SAY  ;
       'C¢digo de Moneda.......... :'  ;
       COLOR W+/N 
     @ 08, 03 SAY  ;
       'Monto a Facturar ......... :'  ;
       COLOR W+/N 
     @ 09, 03 SAY  ;
       'Concepto a Facturar ...... :'  ;
       COLOR W+/N 
     @ 05, 33 SAY SPACE(35)
     @ 07, 33 SAY SPACE(35)
     @ 09, 33 SAY SPACE(35)
     @ 11, 01 TO 14, 72
     @ 12, 02 SAY 'Cliente :' +  ;
       SPACE(50)
     @ 13, 02 SAY 'Modelo  :' +  ;
       SPACE(50)
     SET CURSOR ON
     ON KEY LABEL F6 DO AYUPAG
     @ 04, 34 GET wrk_codsol  ;
       PICTURE '99999999' VALID  ;
       despues(1) WHEN antes(1)
     @ 05, 34 GET wrk_docvta  ;
       PICTURE '@!' VALID  ;
       despues(2) WHEN antes(2)
     @ 06, 34 GET wrk_numdoc  ;
       PICTURE '9999999999' VALID  ;
       despues(3) WHEN antes(2)
     @ 07, 34 GET wrk_codmon  ;
       PICTURE '@!' VALID  ;
       despues(4) WHEN antes(4)
     @ 08, 34 GET wrk_monto  ;
       PICTURE '999,999.99' VALID  ;
       despues(5) WHEN antes(5)
     @ 09, 34 GET wrk_concep  ;
       PICTURE '@!' VALID  ;
       despues(6) WHEN antes(6)
     READ
     IF LASTKEY() = 27
          EXIT
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
             'AYU', 'BBB', 'BBB',  ;
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
          IF EMPTY(wrk_codsol)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          wrk_numsol = STR(wrk_codsol,  ;
                       8)
          SELECT st_isrep
          SEEK wrk_numsol
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** N£mero de Solicitud No Existe ***'
               RETURN .F.
          ENDIF
          IF indori = 'GARA' .OR.  ;
             indori = 'GREC'
               DO error WITH  ;
                  '*** N£mero de Solicitud se Encuentra en '+ ;
                  indori+' ***'
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
          wrk_codent = codent
          wrk_codemi = codemi
          wrk_abono = monabo
          wrk_indori = indori
          @ 12, 14 SAY wrk_codent
          @ 13, 14 SAY codmod
          @ 13, 38 SAY 'Serie :'
          @ 13, 46 SAY numser
          SELECT st_iclpr
          SEEK 'C' + wrk_codent
          IF FOUND()
               wrk_noment = noment
               wrk_dirent = nomcal
               wrk_numtel = numte1
               wrk_coddis = nomdis
               @ 12, 24 SAY  ;
                 wrk_noment
          ENDIF
     CASE opc = 2
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_docvta)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' +  ;
               wrk_docvta
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** Documento de Venta No Existe ***'
               RETURN .F.
          ELSE
               IF wrk_docvta <>  ;
                  'BOLE' .AND.  ;
                  wrk_docvta <>  ;
                  'FACT'
                    DO error WITH  ;
                       '*** No es Documento de Venta ***'
                    RETURN .F.
               ENDIF
          ENDIF
          @ 05, 40 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            25)
     CASE opc = 3
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_numdoc)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          IF TYPE('WRK_NUMDOC') =  ;
             'N'
               wrk_numdoc = f_ceros(wrk_numdoc, ;
                            10, ;
                            1)
          ELSE
               wrk_numdoc = f_ceros(wrk_numdoc, ;
                            10, ;
                            2)
          ENDIF
          SELECT gc_hve00
          SEEK wrk_docvta +  ;
               wrk_numdoc
          IF FOUND()
               DO error WITH  ;
                  '*** Nro.de Documento  Existe ***'
               RETURN .F.
          ENDIF
     CASE opc = 4
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_codmon)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'MONE' +  ;
               wrk_codmon
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Moneda No Existe ***'
               RETURN .F.
          ENDIF
          @ 07, 40 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            25)
     CASE opc = 5
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF wrk_monto <= 0
               DO error WITH  ;
                  '*** Monto es Invalido ***'
               RETURN .F.
          ENDIF
     CASE opc = 6
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_concep)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SEEK 'ACTA' +  ;
               wrk_concep
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Concepto Venta No Existe ***'
               RETURN .F.
          ENDIF
          wrk_descon = tab_destab
          @ 09, 40 SAY  ;
            SUBSTR(wrk_descon, 1,  ;
            29)
          SEEK 'DIST' +  ;
               wrk_coddis
          IF FOUND()
               wrk_nomdis = tab_destab
          ENDIF
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'GRA',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          DO WHILE .T.
               = INKEY(0, 'H')
               IF LASTKEY() = 27
                    RETURN
               ENDIF
               IF LASTKEY() = -1
                    ON KEY
                    DO calculo
                    EXIT
               ENDIF
          ENDDO
          RETURN
ENDCASE
RETURN
*
PROCEDURE calculo
IF wrk_codmon = 'SOL '
     wrk_mobra = ROUND(ROUND(wrk_monto /  ;
                 (1 + wrk_igv),  ;
                 2) / wrk_tipcam,  ;
                 2)
     wrk_totgen = ROUND(wrk_mobra *  ;
                  (1 + wrk_igv),  ;
                  2)
     wrk_valvta = ROUND(wrk_totgen /  ;
                  (1 + wrk_igv),  ;
                  2)
     wrk_valigv = wrk_totgen -  ;
                  wrk_valvta
     sol_totgen = wrk_monto
     sol_totvta = ROUND(sol_totgen /  ;
                  (1 + wrk_igv),  ;
                  2)
     sol_igv = sol_totgen -  ;
               sol_totvta
ELSE
     wrk_mobra = ROUND(wrk_monto /  ;
                 (1 + wrk_igv),  ;
                 2)
     wrk_totgen = wrk_monto
     wrk_valvta = ROUND(wrk_totgen /  ;
                  (1 + wrk_igv),  ;
                  2)
     wrk_valigv = wrk_totgen -  ;
                  wrk_valvta
     sol_totgen = ROUND(ROUND((wrk_mobra *  ;
                  wrk_tipcam), 2) *  ;
                  (1 + wrk_igv),  ;
                  2)
     sol_totvta = ROUND(sol_totgen /  ;
                  (1 + wrk_igv),  ;
                  2)
     sol_igv = sol_totgen -  ;
               sol_totvta
ENDIF
DO graba
RETURN
*
PROCEDURE graba
DO mensa WITH 'Grabando   N§ '+ ;
   wrk_numdoc, 'COLO'
SELECT st_iredo
APPEND BLANK
DO rbloquea
REPLACE indodo WITH 'SSE '
REPLACE numodo WITH wrk_numsol
REPLACE indddo WITH wrk_docvta
REPLACE numddo WITH wrk_numdoc
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
UNLOCK
SELECT gc_hve00
APPEND BLANK
DO rbloquea
REPLACE hve_tipdoc WITH  ;
        wrk_docvta
REPLACE hve_nrodoc WITH  ;
        wrk_numdoc
REPLACE hve_tidore WITH 'SOLI'
REPLACE hve_nrdore WITH  ;
        wrk_numsol
REPLACE hve_fecdoc WITH DATE()
REPLACE hve_fecvct WITH DATE()
REPLACE hve_codemi WITH  ;
        wrk_codemi
REPLACE hve_tipent WITH 'C   '
REPLACE hve_codent WITH  ;
        wrk_codent
REPLACE hve_tippag WITH '001 '
REPLACE hve_codmov WITH 'PCTA'
REPLACE hve_estdoc WITH 'O'
REPLACE hve_indori WITH  ;
        wrk_indori
REPLACE hve_cosmob WITH wrk_mobra
REPLACE hve_totnet WITH  ;
        wrk_totgen
REPLACE hve_totvta WITH  ;
        wrk_valvta
REPLACE hve_totigv WITH  ;
        wrk_valigv
REPLACE hve_totgen WITH  ;
        wrk_totgen
REPLACE hve_totoim WITH  ;
        wrk_totgen
REPLACE hve_codmon WITH  ;
        wrk_codmon
REPLACE hve_fechtc WITH DATE()
REPLACE hve_solmob WITH  ;
        sol_totvta
REPLACE hve_solnet WITH  ;
        sol_totgen
REPLACE hve_solvta WITH  ;
        sol_totvta
REPLACE hve_soligv WITH sol_igv
REPLACE hve_solgen WITH  ;
        sol_totgen
REPLACE hve_mtocan WITH  ;
        sol_totgen
REPLACE hve_tipcam WITH  ;
        wrk_tipcam
REPLACE hve_usuari WITH users
REPLACE hve_fecha WITH DATE()
REPLACE hve_hora WITH TIME()
UNLOCK
IF wrk_concep = '004'
     SELECT st_iorep
     SEEK wrk_numsol
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
          SELECT st_mvord
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
REPLACE monabo WITH wrk_abono +  ;
        wrk_totgen
REPLACE user WITH users
REPLACE time WITH TIME()
REPLACE date WITH DATE()
UNLOCK
DO mensa WITH  ;
   'Grabando   FACT N§ '+ ;
   wrk_numdoc, 'SACA'
RETURN
*
PROCEDURE imprime
SET CONSOLE OFF
IF wrk_docvta = 'FACT'
     SET PRINT TO &RGE_LPTFAC ;
    
ELSE
     SET PRINT TO &RGE_LPTBOL ;
       
ENDIF
SET PRINTER ON
SET DEVICE TO PRINTER
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(67) + CHR(51)
? CHR(18)
IF wrk_docvta = 'FACT'
     @ 12, 00 SAY DATE()
     @ 12, 15 SAY wrk_codemi
     @ 14, 10 SAY  ;
       SUBSTR(wrk_noment, 1, 30)
     @ 14, 48 SAY wrk_codent
     @ 16, 10 SAY wrk_dirent
     @ 18, 28 SAY 'N. SOLES'
     @ 18, 45 SAY 'CONTADO'
     @ 18, 65 SAY 'S/S' + ' ' +  ;
       ALLTRIM(wrk_numsol)
     @ 22, 10 SAY wrk_concep
     @ 22, 15 SAY wrk_descon
ELSE
     @ 10, 00 SAY DATE()
     @ 10, 15 SAY wrk_codemi
     @ 12, 10 SAY  ;
       SUBSTR(wrk_noment, 1, 30)
     @ 14, 10 SAY wrk_dirent
     @ 16, 28 SAY 'N. SOLES'
     @ 16, 45 SAY 'CONTADO'
     @ 16, 65 SAY 'S/S' + ' ' +  ;
       ALLTRIM(wrk_numsol)
     @ 22, 10 SAY wrk_concep
     @ 22, 15 SAY wrk_descon
ENDIF
IF wrk_docvta = 'FACT'
     @ 36, 62 SAY 'S/.'
     @ 36, 67 SAY sol_totvta  ;
       PICTURE '999,999.99'
     @ 37, 67 SAY sol_igv PICTURE  ;
       '999,999.99'
     @ 38, 67 SAY sol_totgen  ;
       PICTURE '999,999.99'
     @ 40, 01 SAY  ;
       oonumlet(sol_totgen)
ELSE
     @ 35, 62 SAY 'S/.'
     @ 35, 67 SAY sol_totgen  ;
       PICTURE '999,999.99'
     @ 36, 10 SAY sol_totgen  ;
       PICTURE '999,999.99'
     @ 37, 67 SAY sol_totgen  ;
       PICTURE '999,999.99'
     @ 39, 01 SAY  ;
       oonumlet(sol_totgen)
ENDIF
@ 44, 62 SAY wrk_numdoc
EJECT
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
valor = .T.
RETURN
*
PROCEDURE ayupag
campoa = 'numdoc+"  "+dtoc(fecemi)+"  "+codent+"  "+SUBSTR(codmod,1,10)+"   "+SUBSTR(NUMSER,1,12)+"   "+subst(indest,1,2)+" "+ALLTRIM(INDORI)'
IF VARREAD() == 'WRK_CODSOL'
     wrk_origen = 'SS'
     SELECT st_isrep
     SET FILTER TO indest <> 'B';
.AND. indest <> 'F'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+SUBSTR(NUMSER,1,12)+"    "+codent+"  "+SUBSTR(codmod,1,10)+"  "+subst(indest,1,2)+"    "+alltrim(indori)'
     DO ayuda4 WITH campoa,  ;
        wrk_origen
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() = 'WRK_CODMON'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'MONE'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'TIPO DE MONEDA'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() = 'WRK_CONCEP'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'ACTA'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'CONCEPTO PAGO A CUENTA'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() = 'WRK_DOCVTA'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'DOCU'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'TIPO DE DOCUMENTO'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() = 'CODSOL'
     SELECT st_isrep
     GOTO TOP
     campo = 'numdoc+ "  " + dtoc(fecemi)+ "  "+ codemi'
     titulo = 'SOLICITUD DE SERVICIO'
     DO ayuda1 WITH campo, titulo,  ;
        'numdoc'
ENDIF
ON KEY LABEL F6 DO AYUPAG
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

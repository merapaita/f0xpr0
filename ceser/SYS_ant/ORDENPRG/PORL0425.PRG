*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
@ 02, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ORDENES FACTURADAS EN GARANTIA '
CLOSE DATABASES
SELECT 1
USE SHARED ST_IOREP ORDER  ;
    ORD_FECFAC
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 3
USE SHARED GC_HVE00 ORDER CODIGO
SELECT 4
USE SHARED ST_IMODE ORDER CODIGO
SELECT 5
USE SHARED ST_ISERI ORDER  ;
    ser_codmar
STORE .T. TO sigue
w_facigv = 0
SELECT ge_tab0
wrk_varbus = '"IGV " + "IGV "'
seek &wrk_varbus
IF FOUND()
     wk_igv = tab_factor
     w_facigv = tab_factor / 100
ELSE
     do error with '**No Definido el &empre9**'
     sigue = .F.
ENDIF
cont = 0
DO WHILE sigue
     @ 04, 01 CLEAR TO 13, 77
     @ 03, 01 TO 11, 77
     @ 04, 05 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     STORE 1 TO w_copia, w_fact
     STORE 0 TO wrk_cosrep,  ;
           wrk_cosmob, wrk_totnet,  ;
           wrk_totdes, w_exi,  ;
           w_totde2, w_totvt2,  ;
           w_totig2, w_totge2
     STORE 0 TO wrk_totvta,  ;
           wrk_totigv, wrk_totgen,  ;
           w_totmob, w_totfle,  ;
           w_totrep, wrk_numfac,  ;
           wrk_totact, w_cliente
     STORE SPACE(10) TO  ;
           wrk_numero, wrk_client,  ;
           w_nrofac
     STORE DATE() TO w_fecfac,  ;
           w_fecini, w_fecfin
     STORE SPACE(30) TO w_nomcli
     STORE SPACE(4) TO w_linea,  ;
           w_linini, w_linfin,  ;
           w_tipgar
     STORE SPACE(85) TO w_inftec
     SET CURSOR ON
     @ 04, 04 SAY  ;
       'Fecha Inicio :' GET  ;
       w_fecini PICTURE '@D'  ;
       VALID  .NOT.  ;
       EMPTY(w_fecini)
     @ 04, 40 SAY 'Fecha Fin :'  ;
       GET w_fecfin PICTURE '@D'  ;
       VALID  .NOT.  ;
       EMPTY(w_fecfin)
     @ 06, 04 SAY  ;
       'Tipo Informe: ' GET  ;
       w_informe DEFAULT 1  ;
       PICTURE  ;
       '@*RVN Detalle  ;Resumen'
     @ 06, 40 SAY  ;
       'Salida por  : ' GET  ;
       w_salida DEFAULT 1 PICTURE  ;
       '@*RVT Pantalla ;Impresora; Diskette'
     READ
     IF LASTKEY() = 27
          sigue = .F.
          EXIT
     ENDIF
     IF w_salida = 2
          @ 10, 55 SAY 'Copias:'  ;
            GET w_copia RANGE 1, ;
            10 PICTURE '99'
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT 6
     USE SHARED st_iclpr ORDER  ;
         codigo
     CREATE CURSOR fac_ord  ;
            (numdoc C (08),  ;
            codemi C (04), codent  ;
            C (11), numsol C (8),  ;
            codmar C (4), nrofac  ;
            C (10), codmod C (15),  ;
            numser C (15), cosrep  ;
            N (10, 2), cosmob N  ;
            (10, 2), flete N (10,  ;
            2), fchfac D (8),  ;
            subtot N (10, 2),  ;
            linea C (04), fecvta  ;
            D (8), docgar C (15),  ;
            totdes N (9, 2),  ;
            noment C (30), inftec  ;
            C (50), tipgar C  ;
            (04))
     SET RELATION TO 'C' + codent INTO;
st_iclpr
     SELECT st_iorep
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     SCAN WHILE fecfabo >=  ;
          w_fecini .AND. fecfabo <=  ;
          w_fecfin .AND.  .NOT.  ;
          EOF()
          IF indori = 'GARA' .OR.  ;
             indori = 'GREC'
          ELSE
               LOOP
          ENDIF
          w_codmar = codmar
          w_codmod = codmod
          w_numser = numser
          SELECT st_imode
          SEEK w_codmar +  ;
               w_codmod
          IF FOUND()
               w_linea = linea
          ELSE
               w_linea = SPACE(4)
          ENDIF
          SELECT st_iorep
          w_numdoc = numdoc
          w_numsol = numsol
          w_codent = codent
          w_codemi = codemi
          w_cosrep = cosrep
          w_cosmob = cosmob
          w_cosfle = flete
          w_subtot = subtot
          w_fecfac = fecfabo
          w_nrofac = numfabo
          w_inftec = SUBSTR(observ,  ;
                     1, 85)
          w_tipgar = indori
          SELECT fac_ord
          APPEND BLANK
          REPLACE numdoc WITH  ;
                  w_numdoc
          REPLACE numsol WITH  ;
                  w_numsol
          REPLACE codemi WITH  ;
                  w_codemi
          REPLACE codmar WITH  ;
                  w_codmar
          REPLACE codmod WITH  ;
                  w_codmod
          REPLACE numser WITH  ;
                  w_numser
          REPLACE cosrep WITH  ;
                  w_cosrep
          REPLACE cosmob WITH  ;
                  w_cosmob
          REPLACE flete WITH  ;
                  w_cosfle
          REPLACE subtot WITH  ;
                  w_subtot
          REPLACE codent WITH  ;
                  w_codent
          REPLACE nrofac WITH  ;
                  w_nrofac
          REPLACE fchfac WITH  ;
                  w_fecfac
          REPLACE inftec WITH  ;
                  w_inftec
          REPLACE tipgar WITH  ;
                  w_tipgar
          REPLACE totdes WITH  ;
                  st_iorep.totdes
          IF w_linea = '0002'  ;
             .OR. w_linea =  ;
             '0003'
               REPLACE linea WITH  ;
                       '0001'
          ELSE
               REPLACE linea WITH  ;
                       '0002'
          ENDIF
          REPLACE noment WITH  ;
                  st_iclpr.noment
          SELECT st_iseri
          SEEK fac_ord.codmar +  ;
               fac_ord.codmod +  ;
               fac_ord.numser
          IF FOUND()
               SELECT fac_ord
               REPLACE fecvta  ;
                       WITH  ;
                       st_iseri.fecvta,  ;
                       docgar  ;
                       WITH  ;
                       st_iseri.docgar
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SELECT fac_ord
     COUNT TO w_reg
     IF w_reg = 0
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO error WITH  ;
             '*** No hay informaci¢n para el Reporte ***'
          LOOP
     ENDIF
     INDEX ON codmar + codmod +  ;
           STR(MONTH(fchfac), 2) +  ;
           numsol TAG linea
     GOTO TOP
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     DO CASE
          CASE w_salida = 1
               DO mensa WITH  ;
                  '*** Un momento, Por Favor ... ***',  ;
                  'COLO'
               filtxt = SYS(3) +  ;
                        '.TXT'
               IF w_informe = 1
                    repo form porl0425;
to file &filtxt noconsole
               ELSE
                    repo form porl0425;
to file &filtxt noconsole summary
               ENDIF
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               modi comm &filtxt;
 window pantall noedit
               dele file &filtxt
               SET SYSMENU OFF
          CASE w_salida = 2
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               SET DEVICE TO PRINTER
               SET PRINTER ON
               ?? CHR(27) +  ;
                  CHR(15)
               FOR a = 1 TO  ;
                   w_copia
                    IF w_informe =  ;
                       1
                         REPORT FORMAT  ;
                                porl0425  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                porl0425  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDFOR
               SET PRINTER OFF
               SET DEVICE TO SCREEN
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          CASE w_salida = 3
               DO mensa WITH  ;
                  '*** G r a b a n d o ... ***',  ;
                  'COLO'
               filtxt = SYS(3) +  ;
                        '.TXT'
               IF w_informe = 1
                    repo form porl0425;
to file a:&filtxt noconsole
               ELSE
                    repo form porl0425;
to file a:&filtxt noconsole summary
               ENDIF
               DO mensa WITH  ;
                  '** G r a b a n d o ... **',  ;
                  'SACA'
     ENDCASE
ENDDO
DO sacawin
CLOSE DATABASES
RETURN
*
FUNCTION cuenta
COUNT TO cont
RETURN cont
*
*** 
*** ReFox - retrace your steps ... 
***

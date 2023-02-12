*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
tit_prg = ' INFORME '
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F10 DO FCINCO
@ 02, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ORDENES FACTURADAS Y/O LIQUIDADAS (GARA Y PVEN)'
CLOSE DATABASES
SELECT 1
USE SHARED ST_IOREP ORDER  ;
    ORD_NUMFAB
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 3
USE SHARED GC_HVE00 ORDER CODIGO
SELECT 4
USE SHARED ST_IMODE ORDER CODIGO
SELECT 5
USE SHARED ST_ISERI ORDER  ;
    ser_codmar
SELECT 6
USE ST_MOVCA ORDER NUMSOL
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
     @ 03, 02 TO 9, 77
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
           wrk_totact
     STORE 0 TO wr_subtot,  ;
           wr_valvta, wr_valigv,  ;
           wr_totgen, a
     STORE SPACE(8) TO w_fecing,  ;
           w_fecrep
     STORE SPACE(10) TO  ;
           wrk_numero,  ;
           wrk_client
     STORE DATE() TO w_fecfac
     SET CURSOR ON
     @ 04, 05 SAY  ;
       'Tipo Docume.: ' GET w_opc  ;
       DEFAULT 1 PICTURE  ;
       '@*RHN Factura  ;Liquidaci¢n'  ;
       VALID valid2(1)
     @ 05, 05 SAY  ;
       'Orden por   : ' GET  ;
       w_orden DEFAULT 1 PICTURE  ;
       '@*RHN Linea    ;Emisor  ;Marca   ;Servicio'
     @ 06, 05 SAY  ;
       'Tipo Informe: ' GET  ;
       w_informe DEFAULT 1  ;
       PICTURE  ;
       '@*RHN Detalle  ;Resumen'
     @ 07, 05 SAY  ;
       'Salida por  : ' GET  ;
       w_salida DEFAULT 1 PICTURE  ;
       '@*RHN Pantalla ;Impresora'  ;
       VALID valida(2)
     @ 08, 05 SAY  ;
       'En Diskette?: ' GET  ;
       w_disket DEFAULT 1 PICTURE  ;
       '@*RHTN No       ;Si     '
     READ
     IF LASTKEY() = 27
          sigue = .F.
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT 7
     USE SHARED st_iclpr ORDER  ;
         codigo
     CREATE CURSOR fac_ord  ;
            (numdoc C (08),  ;
            numsol C (8), fecing  ;
            D, fecrep D, fecent D,  ;
            codemi C (04), codent  ;
            C (11), noment C (30),  ;
            codmar C (4), codmod  ;
            C (15), numser C (15),  ;
            cosrep N (10, 2),  ;
            cosmob N (10, 2),  ;
            flete N (10, 2),  ;
            totdes N (9, 2),  ;
            subtot N (10, 2),  ;
            linea C (04), fecvta  ;
            D, docgar C (15),  ;
            codrep C (4), inftec  ;
            C (50))
     SET RELATION TO 'C' + codent INTO;
st_iclpr
     SELECT st_iorep
     SET NEAR ON
     IF w_opc = 1
          SEEK 'FACT' +  ;
               wrk_numero
     ELSE
          SEEK 'LIQU' +  ;
               wrk_numero
     ENDIF
     SET NEAR OFF
     SCAN WHILE numfabo =  ;
          wrk_numero .AND.  .NOT.  ;
          EOF()
          w_numdoc = numdoc
          w_numsol = numsol
          w_fecing = fecemi
          w_fecrep = fecfin
          w_codent = codent
          w_codmar = codmar
          w_codmod = codmod
          w_numser = numser
          w_codemi = codemi
          w_inftec = observ
          w_cosrep = cosrep
          w_cosmob = cosmob
          w_cosfle = flete
          w_subtot = subtot
          w_fecent = fecent
          SELECT st_imode
          SEEK w_codmar +  ;
               w_codmod
          IF FOUND()
               w_linea = linea
          ELSE
               w_linea = SPACE(4)
          ENDIF
          SELECT st_movca
          SEEK w_numsol
          w_codrep = SPACE(4)
          IF FOUND()
               SCAN WHILE numsol =  ;
                    w_numsol  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF VAL(codcau) >  ;
                       99
                         w_codrep =  ;
                          codcau
                    ENDIF
               ENDSCAN
          ENDIF
          SELECT fac_ord
          APPEND BLANK
          REPLACE numdoc WITH  ;
                  w_numdoc
          REPLACE fecing WITH  ;
                  w_fecing
          REPLACE fecrep WITH  ;
                  w_fecrep
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
          REPLACE inftec WITH  ;
                  w_inftec
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
          REPLACE codrep WITH  ;
                  w_codrep
          REPLACE fecent WITH  ;
                  w_fecent
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
          w_totrep = w_totrep +  ;
                     w_cosrep
          w_totmob = w_totmob +  ;
                     w_cosmob
          w_totfle = w_totfle +  ;
                     w_cosfle
     ENDSCAN
     IF w_fact = 1
          IF wrk_client =  ;
             '106635710' .OR.  ;
             wrk_client =  ;
             ' 10663571' .OR.  ;
             wrk_client =  ;
             ' 37270628'
               wrk_totdes = 0
          ELSE
               wrk_totdes = ROUND(w_totrep *  ;
                            0.2 ,  ;
                            2)
          ENDIF
          wr_subtot = w_totrep +  ;
                      w_totmob +  ;
                      w_totfle
          wr_valvta = wr_subtot -  ;
                      wrk_totdes
          wr_valigv = wr_valvta *  ;
                      w_facigv
          wr_totgen = wr_valvta +  ;
                      wr_valigv
     ENDIF
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
     DO CASE
          CASE w_orden = 1
               INDEX ON linea +  ;
                     numsol TAG  ;
                     linea
          CASE w_orden = 2
               INDEX ON codemi +  ;
                     linea +  ;
                     numsol TAG  ;
                     linea
          CASE w_orden = 3
               INDEX ON codrep +  ;
                     codmar +  ;
                     numsol TAG  ;
                     linea
          CASE w_orden = 4
               INDEX ON codrep +  ;
                     codemi +  ;
                     codmar +  ;
                     numsol TAG  ;
                     linea
     ENDCASE
     GOTO TOP
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     IF w_salida = 2
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          SET DEVICE TO PRINTER
          SET PRINTER ON
          ?? CHR(27) + CHR(15)
          a1 = 1
          DO WHILE a1<=w_copia
               IF w_informe = 1
                    IF w_orden =  ;
                       3 .OR.  ;
                       w_orden =  ;
                       4
                         REPORT FORMAT  ;
                                porl4170  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                porl0417  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ELSE
                    IF w_orden =  ;
                       3 .OR.  ;
                       w_orden =  ;
                       4
                         REPORT FORMAT  ;
                                porl4171  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                porl0417  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDIF
               a1 = a1 + 1
          ENDDO
          SET PRINTER OFF
          SET DEVICE TO SCREEN
          IF w_disket = 2
               filtxt = SYS(3) +  ;
                        '.TXT'
               IF w_informe = 1
                    IF w_orden =  ;
                       3 .OR.  ;
                       w_orden =  ;
                       4
                         repo form porl4170;
to file a:&filtxt noconsole
                    ELSE
                         repo form porl0417;
to file a:&filtxt noconsole
                    ENDIF
               ELSE
                    IF w_orden =  ;
                       3 .OR.  ;
                       w_orden =  ;
                       4
                         repo form porl4171;
to file a:&filtxt noconsole summary
                    ELSE
                         repo form porl0417;
to file a:&filtxt noconsole summary
                    ENDIF
               ENDIF
          ENDIF
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          DO mensa WITH  ;
             '*** Un momento, Por Favor ... ***',  ;
             'COLO'
          filtxt = SYS(3) +  ;
                   '.TXT'
          IF w_informe = 1
               IF w_orden = 3  ;
                  .OR. w_orden =  ;
                  4
                    repo form porl4170;
to file &filtxt noconsole
               ELSE
                    repo form porl0417;
to file &filtxt noconsole
               ENDIF
          ELSE
               IF w_orden = 3  ;
                  .OR. w_orden =  ;
                  4
                    repo form porl4171;
to file &filtxt noconsole summary
               ELSE
                    repo form porl0417;
to file &filtxt noconsole summary
               ENDIF
          ENDIF
          IF w_disket = 2
               w_archivo = 'A:' +  ;
                           ALLTRIM(STR(wrk_numfac,  ;
                           10)) +  ;
                           '.TXT'
               copy file &filtxt to &w_archivo
               w_archi2 = 'A:' +  ;
                          ALLTRIM(STR(wrk_numfac,  ;
                          10)) +  ;
                          '.DBF'
               SELECT fac_ord
               USE
               COPY FILE FAC_ORD.DBF TO;
&w_archi2
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &filtxt  window pantall;
noedit
          dele file &filtxt
          SET SYSMENU OFF
     ENDIF
ENDDO
DO sacawin
CLOSE DATABASES
DELETE FILE FAC_ORD.DBF
RETURN
*
FUNCTION cuenta
COUNT TO cont
RETURN cont
*
PROCEDURE valid2
PARAMETER opc
@ 04, 56 SAY 'N£mero:'
@ 04, 63 GET wrk_numfac PICTURE  ;
  '9999999999' VALID valida(1)
READ
RETURN
*
FUNCTION valida
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(wrk_numfac)
               DO error WITH  ;
                  'No se Aceptan Blancos'
               RETURN .F.
          ENDIF
          wrk_numero = f_ceros(wrk_numfac, ;
                       10,1)
          @ 04, 63 SAY wrk_numero
          SELECT gc_hve00
          IF w_opc = 1
               SEEK 'FACT' +  ;
                    wrk_numero
          ELSE
               SEEK 'LIQU' +  ;
                    wrk_numero
          ENDIF
          IF  .NOT. FOUND()
               SELECT st_iorep
               IF w_opc = 1
                    SEEK 'FACT' +  ;
                         wrk_numero
               ELSE
                    SEEK 'LIQU' +  ;
                         wrk_numero
               ENDIF
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       'N£mero de Factura No Existe'
                    RETURN .F.
               ELSE
                    IF (indori =  ;
                       'GARA'  ;
                       .OR.  ;
                       indori =  ;
                       'GREC')
                         w_fecfac =  ;
                          fecfabo
                         SELECT st_iseri
                         SEEK st_iorep.codmar +  ;
                              st_iorep.codmod +  ;
                              st_iorep.numser
                         IF FOUND()
                              wrk_client =  ;
                               codent
                         ENDIF
                         w_exi = 0
                    ENDIF
               ENDIF
          ELSE
               w_fecfac = hve_fecdoc
               IF hve_estdoc =  ;
                  'A'
                    DO error WITH  ;
                       'N£mero esta Anulada'
                    RETURN .F.
               ELSE
                    wrk_cosrep = hve_cosrep
                    wrk_cosmob = hve_cosmob
                    wrk_totnet = hve_totnet
                    wrk_totdes = hve_totdes
                    wrk_totvta = hve_totvta
                    wrk_totigv = hve_totigv
                    wrk_totact = hve_pagctd
                    wrk_totgen = hve_totoim
                    wrk_client = hve_codent
                    w_fecfac = hve_fecdoc
                    w_fact = 2
               ENDIF
          ENDIF
     CASE opc = 2
          IF w_salida = 2
               @ 07, 54 SAY  ;
                 'Copia: ' GET  ;
                 w_copia RANGE 1, ;
                 10 PICTURE '99'
               READ
          ENDIF
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

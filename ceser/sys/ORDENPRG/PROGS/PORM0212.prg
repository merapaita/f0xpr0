*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER opc
tit_prg = 'MANTENCION'
wrk_progra = PROGRAM()
DO crea_win
ON KEY
SET SYSMENU ON
SET CURSOR ON
CLOSE DATABASES
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' Planillas por Facturar LG '
a = 1
DIMENSION codigo( a), cantid( a),  ;
          pordes( a), importe( a),  ;
          totalit( a), unidad(  ;
          a)
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_idped ORDER codigo
SELECT 3
USE SHARED st_iorep ORDER codigo
SELECT 4
USE SHARED st_iseri ORDER  ;
    ser_codmar
SELECT 5
USE SHARED st_iclpr ORDER codigo
SELECT 6
USE SHARED st_iprep ORDER  ;
    rep_numord
SELECT 7
USE SHARED gc_hve00 ORDER codigo
SELECT 8
USE ST_PLANI ORDER codigo
STORE .T. TO ppas
w_facigv = facigv()
IF w_facigv = 0
     do error with '**No Definido el &empre9**'
     ppas = .F.
ENDIF
IF ppas
     SELECT 20
     USE SHARED gc_cmv00 ORDER  ;
         cmv_feinmo
     w_tipcam = ootc2(DATE(), ;
                rge_monbas,'DOL', ;
                '2')
     IF w_tipcam = -1
          DO error WITH  ;
             '**No Hay Tipo de Cambio **'
          ppas = .F.
     ENDIF
ENDIF
STORE SPACE(04) TO w_talle1,  ;
      w_talle2, w_emiini,  ;
      w_emifin
STORE 0 TO w_codcli
DO esc_modo WITH 'G'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
DO WHILE ppas
     STORE .F. TO eror_imp,  ;
           sw_algo
     STORE SPACE(4) TO w_nomcli,  ;
           w_codpag, w_despag,  ;
           w_dir, w_desmon
     STORE 'DOL ' TO w_codmon
     STORE 'FACT' TO w_coddoc
     STORE 0 TO w_numdoc,  ;
           w_desval, w_despor
     w_fevega = DATE() + empre7
     w_fecven = DATE()
     SELECT st_iclpr
     @ 03, 01 CLEAR TO 14, 78
     @ 04, 61 SAY 'TC:'
     @ 04, 65 SAY w_tipcam  ;
       PICTURE '999,999.99'
     @ 04, 20 SAY  ;
       'Tipo de Atenci¢n:'
     @ 09, 02 SAY  ;
       'Del Taller     :'
     @ 10, 02 SAY  ;
       'Al  Taller     :'
     @ 11, 02 SAY  ;
       'Del Emisor     :'
     @ 12, 02 SAY  ;
       'Al  Emisor     :'
     @ 13, 02 SAY  ;
       'C¢digo Moneda  :'
     @ 04, 38 GET w_opc DEFAULT 3  ;
       PICTURE  ;
       '@*RVT Postventa;Preventa;Ambos   '  ;
       VALID oovalid(VARREAD())
     READ
     IF LASTKEY() <> 27
          DO esc_indica WITH 1,  ;
             'AYU', 'MBV', 'BBB',  ;
             'BBB'
          DO eli_garan
     ELSE
          sw_algo = .F.
          ppas = .F.
     ENDIF
     IF sw_algo = .T.
          DIMENSION pfecha( 1),  ;
                    docum( 1),  ;
                    valod( 1),  ;
                    valos( 1),  ;
                    pnumso( 1)
          STORE 0 TO w_numdoc,  ;
                tot_flete,  ;
                tot_otro,  ;
                s_subtot,  ;
                s_totnet, pagi,  ;
                ctatot, s_ctatot,  ;
                w_totbru,  ;
                s_totbru
          STORE 0 TO w_repues,  ;
                w_mano, w_flete,  ;
                w_subtot,  ;
                w_totdes,  ;
                w_totnet,  ;
                w_totigv,  ;
                w_totbru,  ;
                s_descue,  ;
                s_totgen,  ;
                s_totvta,  ;
                s_totigv
          STORE 0 TO s_repues,  ;
                s_mano, s_flete
          STORE SPACE(8) TO  ;
                num_ord
          a = 1
          SELECT gara
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               IF SUBSTR(codfabo,  ;
                  1, 1) = 'û'
                    num_ord = numdoc
                    w_repues = w_repues +  ;
                               cosrep
                    s_repues = s_repues +  ;
                               solrep
                    w_mano = w_mano +  ;
                             cosmob
                    s_mano = s_mano +  ;
                             solmob
                    w_flete = w_flete +  ;
                              cosfle
                    s_flete = s_flete +  ;
                              solfle
                    SELECT st_iprep
                    SEEK num_ord
                    IF FOUND()
                         SCAN WHILE  ;
                              numord =  ;
                              num_ord  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              IF indest <>  ;
                                 'N'
                                   SELECT st_idped
                                   SEEK st_iprep.numdoc + st_iprep.numord
                                   IF FOUND()
                                        SCAN WHILE numdoc = st_iprep.numdoc .AND. numord = st_iprep.numord .AND.  .NOT. EOF()
                                             IF canpro > 0
                                                  DIMENSION codigo( a), cantid( a), pordes( a), importe( a), totalit( a), unidad( a)
                                                  codigo( a) = codpro
                                                  cantid( a) = canpro
                                                  pordes( a) = pordes
                                                  importe( a) = valpro
                                                  totalit( a) = totite
                                                  unidad( a) = 'UNID'
                                                  a = a + 1
                                             ENDIF
                                        ENDSCAN
                                   ENDIF
                                   SELECT st_iprep
                              ENDIF
                         ENDSCAN
                    ENDIF
                    SELECT gc_hve00
                    SET ORDER TO nrdore
                    SEEK gara.numsol
                    IF FOUND()
                         SCAN WHILE  ;
                              ALLTRIM(gc_hve00.hve_nrdore) =  ;
                              ALLTRIM(gara.numsol)  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              IF hve_estdoc <>  ;
                                 'A'  ;
                                 .AND.  ;
                                 hve_codmov =  ;
                                 'PCTA'
                                   pagi = pagi + 1
                                   DIMENSION pfecha( pagi), docum( pagi), valod( pagi), valos( pagi), pnumso( pagi)
                                   pfecha( pagi) = gc_hve00.hve_fecdoc
                                   docum( pagi) = gc_hve00.hve_nrodoc
                                   valod( pagi) = gc_hve00.hve_totgen
                                   valos( pagi) = gc_hve00.hve_solgen
                                   pnumso( pagi) = gc_hve00.hve_nrdore
                                   ctatot = ctatot + valod(pagi)
                                   s_ctatot = s_ctatot + valos(pagi)
                              ENDIF
                         ENDSCAN
                    ENDIF
                    SET ORDER TO codigo
                    SELECT gara
               ENDIF
          ENDSCAN
          a = a - 1
          STORE SPACE(4) TO  ;
                w_codemi
          ON KEY
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'IGN',  ;
             'ESC'
          @ 03, 01 CLEAR TO 15,  ;
            78
          @ 03, 17 TO 13, 55
          IF opc = 1
               @ 04, 20 SAY  ;
                 'Documento de Venta...:  FACT'
          ELSE
               @ 04, 20 SAY  ;
                 'Documento de Venta...:  PLANILLA'
          ENDIF
          @ 06, 20 SAY  ;
            'Fecha de la Planilla.:'
          @ 07, 20 SAY  ;
            'Fecha Venc.Gar.Repar.:'
          @ 08, 20 SAY  ;
            'C¢digo de Pago.......:'
          @ 09, 20 SAY  ;
            'C¢digo del Emisor....:'
          IF LASTKEY() <> -9  ;
             .AND. LASTKEY() <>  ;
             27
               IF opc = 2
                    SELECT ge_tab0
                    SET ORDER TO descr
                    SEEK 'LIQU' +  ;
                         ALLTRIM(SUBSTR(STR(w_codcli,  ;
                         11), 1,  ;
                         12))
                    IF  .NOT.  ;
                        FOUND()
                         SET ORDER TO;
codigo
                         SELECT gara
                         @ 05, 20  ;
                           SAY  ;
                           'N£mero de Planilla...:'
                         @ 05, 44  ;
                           GET  ;
                           w_numdoc  ;
                           VALID  ;
                           oovalid(VARREAD())
                    ELSE
                         SET ORDER TO;
codigo
                    ENDIF
               ENDIF
               @ 06, 44 GET  ;
                 w_fecven VALID  ;
                 oovalid(VARREAD())
               @ 07, 44 GET  ;
                 w_fevega RANGE  ;
                 w_fecven VALID  ;
                 oovalid(VARREAD())
               @ 08, 44 GET  ;
                 w_codpag PICTURE  ;
                 '@!' VALID  ;
                 oovalid(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())
               @ 09, 44 GET  ;
                 w_codemi PICTURE  ;
                 '@!' VALID  ;
                 oovalid(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())
               READ
          ENDIF
          IF LASTKEY() <> 27  ;
             .AND. LASTKEY() <> - ;
             9
               DO muestra
          ENDIF
          IF LASTKEY() = -9
               ppas = .T.
          ENDIF
          IF LASTKEY() = 27 .OR.  ;
             eror_imp = .T.
               IF eror_imp = .F.
                    ppas = .T.
               ENDIF
          ENDIF
     ENDIF
ENDDO
ON KEY
CLOSE DATABASES
DO sacawin
RETURN
*
PROCEDURE actual
DO mensa WITH 'Grabando '+ ;
   w_coddoc+' N§ '+w_numdoc,  ;
   'COLO'
SELECT gara
GOTO TOP
SCAN WHILE  .NOT. EOF()
     num_ord = numdoc
     IF SUBSTR(codfabo, 1, 1) =  ;
        'û'
          SELECT st_plani
          APPEND BLANK
          DO rbloquea
          REPLACE orden WITH  ;
                  num_ord
          REPLACE nropla WITH  ;
                  w_numdoc
          REPLACE estado WITH 1
          REPLACE fecpla WITH  ;
                  DATE()
          REPLACE user WITH users
          REPLACE date WITH  ;
                  DATE()
          REPLACE time WITH  ;
                  TIME()
          UNLOCK
     ENDIF
     SELECT gara
ENDSCAN
DO mensa WITH 'Grabando '+ ;
   w_coddoc+' N§ '+w_numdoc,  ;
   'SACA'
RETURN
*
PROCEDURE eli_garan
DO mensa WITH  ;
   '*** Espere un Momento, Por Favor ***',  ;
   'COLO'
CREATE CURSOR gara (numdoc C (8),  ;
       codmar C (4), codmod C  ;
       (15), numsol C (8), numser  ;
       C (20), cosfle N (9, 2),  ;
       fecemi D (8), indest C (4),  ;
       indori C (4), codtall C  ;
       (4), codent C (11),  ;
       codfabo C (4), cosrep N (9,  ;
       2), cosmob N (9, 2),  ;
       fecest D (8), pagctd N (12,  ;
       2), pagcts N (12, 2),  ;
       solrep N (12, 2), solmob N  ;
       (12, 2), solfle N (12, 2),  ;
       codemi C (4), estpla N  ;
       (1))
w_codpro = STR(w_codcli, 11)
SELECT st_iorep
SET ORDER TO ord_inesta
SET RELATION TO codmar + codmod + numser;
INTO st_iseri
IF w_opc = 1 .OR. w_opc = 3
     FOR i = 1 TO 2
          IF i = 1
               SEEK 'GARA' +  ;
                    'C   '
          ELSE
               SEEK 'GREC' +  ;
                    'C   '
          ENDIF
          SCAN WHILE (indori =  ;
               'GARA' .OR. indori =  ;
               'GREC') .AND.  ;
               indest = 'C   '  ;
               .AND.  .NOT.  ;
               EOF()
               IF st_iseri.codent =  ;
                  w_codpro
                    DO procesa
               ENDIF
          ENDSCAN
     ENDFOR
ENDIF
IF w_opc = 2 .OR. w_opc = 3
     SELECT st_iorep
     FOR i = 1 TO 2
          IF i = 1
               SEEK 'PVEN' +  ;
                    'C   '
          ELSE
               SEEK 'PREC' +  ;
                    'C   '
          ENDIF
          SCAN WHILE (indori =  ;
               'PVEN' .OR. indori =  ;
               'PREC') .AND.  ;
               indest = 'C   '  ;
               .AND.  .NOT.  ;
               EOF()
               IF st_iseri.codent =  ;
                  w_codpro
                    DO procesa
               ENDIF
          ENDSCAN
     ENDFOR
ENDIF
SELECT st_iorep
SET ORDER TO codigo
SELECT gara
COUNT TO w_count
IF w_count = 0
     DO error WITH  ;
        '** No hay Informaci¢n **'
ELSE
     INDEX ON numdoc +  ;
           DTOS(fecemi) TAG  ;
           w_idx
     GOTO TOP
     DO mensa WITH  ;
        '*** Espere un Momento, Por Favor ***',  ;
        'SACA'
     IF w_codmon = 'DOL '
          campo = 'subst(codfabo,1,1)+numdoc+" "+dtoc(fecemi)+" "+substr(codmod,1,09)+" "+substr(numser,1,10)+" "+str(cosrep,7,2)+" "+str(cosmob,6,2)+" "+str(cosfle,6,2)+" "+codtall+" "+STR(estpla,1)'
          mensaje = 'N§ORDENÄF.INGRESOÄÄMODELOÄÄÄÄSERIEÄÄÄÄÄÄÄREPTO.ÄÄÄMOB.ÄÄFLETEÄTALLÄEST.'
     ELSE
          campo = 'subst(codfabo,1,1)+numdoc+" "+dtoc(fecemi)+" "+substr(codmod,1,09)+" "+str(solrep,10,2)+" "+str(solmob,10,2)+" "+str(solfle,10,2)+" "+codtall+" "+STR(estpla,1)'
          mensaje = 'N§ORDENÄF.INGRESOÄÄMODELOÄÄÄÄÄÄÄÄREPTO.ÄÄÄÄÄÄM.O.BÄÄÄÄÄÄFLETEÄTALLÄEST.'
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'TOD', 'BBB', 'SEL'
     DO esc_indica WITH 2, 'MBV',  ;
        'ESP', 'IMP', 'ESC'
     ON KEY LABEL f8 do marca
     ON KEY LABEL f7 do impres
     define popup ayu0 from 04,0 to 14,74;
promp field &campo title mensaje color;
scheme 8
     ON SELECTION POPUP ayu0 do choice0
     ACTIVATE POPUP ayu0 NOWAIT
     ACTIVATE POPUP ayu0
     DEACTIVATE POPUP ayu0
     SELECT gara
     GOTO TOP
     SCAN WHILE  .NOT. EOF()
          IF SUBSTR(codfabo, 1,  ;
             1) = 'û'
               sw_algo = .T.
               GOTO BOTTOM
          ENDIF
     ENDSCAN
     IF LASTKEY() = 27
          ppas = .T.
          ON KEY
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'SEL'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     ENDIF
ENDIF
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
PROCEDURE muestra
ON KEY
DO sacaf6
sale = 0
ON KEY LABEL f10 do f10s
@ 03, 01 CLEAR TO 15, 79
@ 04, 01 SAY  ;
  ' E N C A B E Z A D O ' COLOR  ;
  SCHEME 8
@ 04, 42 SAY  ;
  ' T O T A L E S                   '  ;
  COLOR SCHEME 8
IF w_codmon = 'DOL '
     @ 04, 72 SAY empre13 COLOR  ;
       SCHEME 8
ELSE
     @ 04, 72 SAY empre8 COLOR  ;
       SCHEME 8
ENDIF
@ 06, 01 SAY  ;
  'Provedor...........: ' +  ;
  SUBSTR(w_nomcli, 1, 19)
@ 07, 01 SAY  ;
  'C¢digo Provedor....: ' +  ;
  STR(w_codcli, 11)
@ 08, 01 SAY  ;
  'Direcci¢n..........: ' +  ;
  SUBSTR(w_dir, 1, 20)
@ 09, 01 SAY  ;
  'Forma de Pago......: ' +  ;
  SUBSTR(w_despag, 1, 20)
@ 10, 01 SAY  ;
  'Fecha de Emisi¢n...: ' +  ;
  DTOC(w_fecven)
@ 10, 42 SAY  ;
  'Dscto. Adicion.(%): '
@ 11, 01 SAY  ;
  'Fecha Gar.Venc.Rep.: ' +  ;
  DTOC(w_fevega)
@ 11, 42 SAY 'Dscto. Adici.'
IF w_codmon = 'DOL '
     @ 11, 57 SAY empre13 + ': '
ELSE
     @ 11, 57 SAY empre8 + ': '
ENDIF
w_subtot = ROUND(w_repues * (1 +  ;
           w_facigv), 2) +  ;
           ROUND(w_mano * (1 +  ;
           w_facigv), 2) +  ;
           ROUND(w_flete * (1 +  ;
           w_facigv), 2)
w_totnet = w_subtot
w_totgen = w_totnet - w_totdes
w_totbru = w_totgen - ctatot
w_totvta = ROUND((w_totbru / (1 +  ;
           w_facigv)), 2)
w_totigv = w_totbru - w_totvta
s_subtot = ROUND(s_repues * (1 +  ;
           w_facigv), 2) +  ;
           ROUND(s_mano * (1 +  ;
           w_facigv), 2) +  ;
           ROUND(s_flete * (1 +  ;
           w_facigv), 2)
s_totnet = s_subtot
s_totgen = s_totnet - s_descue
s_totbru = s_totgen - s_ctatot
s_totvta = ROUND((s_totbru / (1 +  ;
           w_facigv)), 2)
s_totigv = s_totbru - s_totvta
@ 06, 42 SAY  ;
  'Costo En Repuesto :' +  ;
  TRANSFORM(IIF(w_codmon = 'DOL ',  ;
  ROUND(w_repues * (1 + w_facigv),  ;
  2), ROUND(s_repues * (1 +  ;
  w_facigv), 2)),  ;
  '999,999,999.99')
@ 07, 42 SAY  ;
  'Costo Mano de Obra:' +  ;
  TRANSFORM(IIF(w_codmon = 'DOL ',  ;
  ROUND(w_mano * (1 + w_facigv),  ;
  2), ROUND(s_mano * (1 +  ;
  w_facigv), 2)),  ;
  '999,999,999.99')
@ 08, 42 SAY  ;
  'Flete/Otros.......:' +  ;
  TRANSFORM(IIF(w_codmon = 'DOL ',  ;
  ROUND(w_flete * (1 + w_facigv),  ;
  2), ROUND(s_flete * (1 +  ;
  w_facigv), 2)),  ;
  '999,999,999.99')
@ 09, 42 SAY  ;
  'Total Neto........:' +  ;
  TRANSFORM(IIF(w_codmon = 'DOL ',  ;
  w_totnet, s_totnet),  ;
  '999,999,999.99')
@ 12, 42 SAY  ;
  'Pagos a Cuenta....:' +  ;
  TRANSFORM(IIF(w_codmon = 'DOL ',  ;
  ctatot, s_ctatot),  ;
  '999,999,999.99')
@ 13, 01 SAY  ;
  'Total Venta........:' +  ;
  TRANSFORM(IIF(w_codmon = 'DOL ',  ;
  w_totvta, s_totvta),  ;
  '999,999,999.99')
@14,01 say "Total &empre9..........:";
+ transform(iif(w_codmon='DOL ',w_totigv,s_totigv),"999,999,999.99")
@ 14, 42 SAY  ;
  'Total a Pagar.....:' +  ;
  TRANSFORM(IIF(w_codmon = 'DOL ',  ;
  w_totbru, s_totbru),  ;
  '999,999,999.99')
@ 10, 70 GET w_despor PICTURE  ;
  '99.99' VALID  ;
  oovalid(VARREAD())
@ 11, 61 GET w_desval PICTURE  ;
  '999,999,999.99' VALID  ;
  oovalid(VARREAD())
READ
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
ON KEY LABEL f10
IF sale = 0 .AND. LASTKEY() <> 27
     IF w_codmon = 'DOL '
          w_totdes = w_desval
          s_descue = ROUND(w_desval *  ;
                     w_tipcam,  ;
                     2)
     ELSE
          s_descue = w_desval
          w_totdes = ROUND(w_desval /  ;
                     w_tipcam,  ;
                     2)
     ENDIF
     w_totgen = w_totnet -  ;
                w_totdes
     w_totbru = w_totgen - ctatot
     w_totvta = ROUND((w_totbru /  ;
                (1 + w_facigv)),  ;
                2)
     w_totigv = w_totbru -  ;
                w_totvta
     s_totgen = s_totnet -  ;
                s_descue
     s_totbru = s_totgen -  ;
                s_ctatot
     s_totvta = ROUND((s_totbru /  ;
                (1 + w_facigv)),  ;
                2)
     s_totigv = s_totbru -  ;
                s_totvta
     IF w_codmon = 'DOL '
          @ 13, 21 SAY w_totvta  ;
            PICTURE  ;
            '999,999,999.99'
          @ 14, 21 SAY w_totigv  ;
            PICTURE  ;
            '999,999,999.99'
          @ 14, 61 SAY w_totbru  ;
            PICTURE  ;
            '999,999,999.99'
     ELSE
          @ 13, 21 SAY s_totvta  ;
            PICTURE  ;
            '999,999,999.99'
          @ 14, 21 SAY s_totigv  ;
            PICTURE  ;
            '999,999,999.99'
          @ 14, 61 SAY s_totbru  ;
            PICTURE  ;
            '999,999,999.99'
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'GRA', 'IGN'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     = INKEY(0, 'H')
     CLEAR TYPEAHEAD
     DO WHILE LASTKEY()<>27 .AND.  ;
        LASTKEY()<>-1 .AND.  ;
        LASTKEY()<>-9
          = INKEY(0)
     ENDDO
     eror_imp = .F.
     w_obliga = .F.
     IF LASTKEY() = -1
          ON KEY
          DO con_use
          IF eror_imp = .T.
               DO error WITH  ;
                  '** Ya se factur¢, vuelva a ingresar **'
          ELSE
               IF opc = 1
               ELSE
                    SELECT ge_tab0
                    SET ORDER TO descr
                    SEEK 'LIQU' +  ;
                         ALLTRIM(SUBSTR(STR(w_codcli,  ;
                         11), 1,  ;
                         12))
                    IF FOUND()
                         w_coddoc =  ;
                          'LIQU'
                         DO rbloquea
                         REPLACE tab_factor  ;
                                 WITH  ;
                                 tab_factor +  ;
                                 1
                         w_numdoc =  ;
                          tab_factor
                         UNLOCK
                    ENDIF
                    SET ORDER TO codigo
                    w_numdoc = f_ceros(w_numdoc, ;
                               10, ;
                               1)
               ENDIF
               DO actual
               w_obliga = .T.
          ENDIF
     ENDIF
ENDIF
@ 04, 1 CLEAR TO 14, 79
RETURN
*
PROCEDURE con_use
SELECT gara
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT st_iorep
     SEEK gara.numdoc
     IF FOUND()
          IF indest = 'F'
               eror_imp = .T.
               EXIT
          ENDIF
     ELSE
          eror_imp = .T.
     ENDIF
     SELECT gara
ENDSCAN
RETURN
*
PROCEDURE imprime
SET CONSOLE OFF
SET PRINTER ON
SET DEVICE TO PRINTER
set print to &rge_lptfac
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(67) + CHR(51)
? CHR(18)
DO pormfac2
EJECT
SET PRINTER OFF
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
CLEAR TYPEAHEAD
RETURN
*
FUNCTION vali_cli
PARAMETER cvalid
DO CASE
     CASE cvalid = 'W_CODCLI'
          IF EMPTY(w_codcli)
               DO error WITH  ;
                  '** Debe Ingresar C¢digo **'
               KEYBOARD '{CTRL+Y}'
               ON KEY LABEL f6 do ayucli
               RETURN .F.
          ENDIF
          SELECT st_iclpr
          IF w_opc = 1
               llave = 'P' +  ;
                       STR(w_codcli,  ;
                       11)
          ELSE
               llave = 'C' +  ;
                       STR(w_codcli,  ;
                       11)
          ENDIF
          SEEK llave
          IF FOUND()
               w_nomcli = noment
               w_dir = SUBSTR(nomcal,  ;
                       1, 30) +  ;
                       ' ' +  ;
                       SUBSTR(ootab2('DIST', ;
                       nomdis), 1,  ;
                       20)
               @ 08, 31 SAY  ;
                 w_nomcli
          ELSE
               DO error WITH  ;
                  '**C¢digo de Cliente No Encontrado**'
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
PROCEDURE ayucli
ON KEY LABEL f6
IF VARREAD() = 'W_CODCLI'
     DO mensa WITH  ;
        '*** Espere un Momento, Por Favor ***',  ;
        'COLO'
     SELECT st_iclpr
     IF w_opc = 1
          SET FILTER TO indent = 'P'
     ELSE
          SET FILTER TO indent = 'C'
     ENDIF
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE ENTIDADES'
     DO mensa WITH  ;
        '*** Espere un Momento, Por Favor ***',  ;
        'SACA'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
     SET ORDER TO codigo
ENDIF
SET FILTER TO
ON KEY LABEL f6 do ayucli
RETURN
*
PROCEDURE f10s
CLEAR READ
sale = 1
RETURN
*
PROCEDURE marca
ON KEY
SELECT gara
REPLACE codfabo WITH 'û   ' ALL
RETURN
*
PROCEDURE impres
DO mensa WITH  ;
   '***  I m p r i m i e n d o  ***',  ;
   'COLO'
SET CONSOLE OFF
SET PRINTER ON
??? CHR(27) + CHR(15)
SELECT gara
REPORT FORMAT porm0262 TO PRINTER  ;
       NOCONSOLE
??? CHR(18)
SET PRINTER OFF
SET CONSOLE ON
DO mensa WITH  ;
   '***  I m p r i m i e n d o  ***',  ;
   'SACA'
RETURN
*
PROCEDURE status_imp
w_printer = '1'
DO WHILE w_printer='1'
     SELECT 20
     USE
     USE SHARED st_iparg
     IF w_coddoc = 'FACT'
          w_printer = sys_lptfac
     ELSE
          w_printer = sys_lptbol
     ENDIF
     IF w_printer = '1'
          DO mensa WITH  ;
             '*** Impresora Ocupada, FACTURANDO ***',  ;
             'COLO'
          = INKEY(1, 'H')
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
PROCEDURE actual1
SELECT 20
USE SHARED st_iparg
DO rbloquea
REPLACE sys_numfac WITH  ;
        sys_numfac + 1
UNLOCK
w_numdoc = f_ceros(sys_numfac,10, ;
           1)
RETURN
*
PROCEDURE fin_imp
SELECT 20
USE SHARED st_iparg
DO rbloquea
IF w_coddoc = 'FACT'
     REPLACE sys_lptfac WITH '0'
ELSE
     REPLACE sys_lptbol WITH '0'
ENDIF
UNLOCK
RETURN
*
PROCEDURE ayuda
PARAMETER opc, titulo
ON KEY
SELECT ge_tab0
SET FILTER TO tab_codpre = opc
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
DO sacaf6
SET FILTER TO
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen
DO CASE
     CASE cwhen = 'W_CODCLI'
          DO colocaf6
          ON KEY LABEL f6 do ayucli
     CASE cwhen = 'W_TALLE1' .OR.  ;
          cwhen = 'W_EMIFIN' .OR.  ;
          cwhen = 'W_TALLE2' .OR.  ;
          cwhen = 'W_EMIINI'
          DO colocaf6
          IF cwhen = 'W_TALLE1'  ;
             .OR. cwhen =  ;
             'W_TALLE2'
               ON KEY LABEL f6 do ayuda;
with "TALL",'AYUDA DE TALLERES'
          ELSE
               ON KEY LABEL f6 do ayuda;
with "EMIS",'AYUDA DE EMISORES'
          ENDIF
     CASE cwhen = 'W_FEVEGA'
          DO sacaf6
     CASE cwhen = 'W_CODPAG'
          DO colocaf6
          ON KEY LABEL f6 do ayuda with;
"FPAG",'AYUDA DE PAGOS'
     CASE cwhen = 'W_CODEMI'
          ON KEY LABEL f6 do ayuda with;
"EMIS",'AYUDA DE EMISORES'
     CASE cwhen = 'W_CODMON'
          DO colocaf6
          ON KEY LABEL f6 do ayuda with;
"MONE",'AYUDA DE MONEDAS'
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'W_OPC'
          @ 08, 02 SAY  ;
            'C¢digo Cliente:'
          @ 08, 20 GET w_codcli  ;
            PICTURE '99999999999'  ;
            VALID  ;
            vali_cli(VARREAD())  ;
            WHEN  ;
            oowhen(VARREAD())
          @ 09, 20 GET w_talle1  ;
            FUNCTION '@!' VALID  ;
            oovalid(VARREAD())  ;
            WHEN  ;
            oowhen(VARREAD())
          @ 10, 20 GET w_talle2  ;
            RANGE w_talle1  ;
            FUNCTION '@!' VALID  ;
            oovalid(VARREAD())  ;
            WHEN  ;
            oowhen(VARREAD())
          @ 11, 20 GET w_emiini  ;
            FUNCTION '@!' VALID  ;
            oovalid(VARREAD())  ;
            WHEN  ;
            oowhen(VARREAD())
          @ 12, 20 GET w_emifin  ;
            RANGE w_emiini  ;
            FUNCTION '@!' VALID  ;
            oovalid(VARREAD())  ;
            WHEN  ;
            oowhen(VARREAD())
          @ 13, 20 GET w_codmon  ;
            FUNCTION '@!' VALID  ;
            oovalid(VARREAD())  ;
            WHEN  ;
            oowhen(VARREAD())
          READ
          IF LASTKEY() = 27
               KEYBOARD '{ENTER}'
               sw_algo = .F.
               @ 08, 00 CLEAR TO  ;
                 08, 60
          ENDIF
     CASE cvalid = 'W_TALLE1'  ;
          .OR. cvalid =  ;
          'W_TALLE2'
          SELECT ge_tab0
          IF cvalid = 'W_TALLE1'
               SEEK 'TALL' +  ;
                    w_talle1
          ELSE
               SEEK 'TALL' +  ;
                    w_talle2
          ENDIF
          IF FOUND()
               @ ROW(), 31 SAY  ;
                 tab_destab
          ELSE
               DO error WITH  ;
                  '**C¢digo No Encontrado**'
               KEYBOARD '{CTRL+Y}'
               RETURN .F.
          ENDIF
     CASE cvalid = 'W_EMIINI'  ;
          .OR. cvalid =  ;
          'W_EMIFIN'
          SELECT ge_tab0
          IF cvalid = 'W_EMIINI'
               SEEK 'EMIS' +  ;
                    w_emiini
          ELSE
               SEEK 'EMIS' +  ;
                    w_emifin
          ENDIF
          IF FOUND()
               @ ROW(), 31 SAY  ;
                 tab_destab
          ELSE
               DO error WITH  ;
                  '**C¢digo No Encontrado**'
               KEYBOARD '{CTRL+Y}'
               RETURN .F.
          ENDIF
     CASE VARREAD() = 'W_NUMDOC'
          w_numdo2 = f_ceros(w_numdoc, ;
                     10,1)
          SELECT gc_hve00
          SEEK 'FACT' + w_numdo2
          IF FOUND()
               DO error WITH  ;
                  '**N£mero ya Ingresado**'
               KEYBOARD '{CTRL+Y}'
               RETURN .F.
          ENDIF
     CASE VARREAD() = 'W_FECVEN'
          IF opc = 1
               IF LASTKEY() = 5
                    RETURN .F.
               ENDIF
          ENDIF
          IF w_fecven > DATE()
               DO error WITH  ;
                  '** Fecha Mayor a Hoy **'
               RETURN .F.
          ENDIF
     CASE VARREAD() = 'W_FEVEGA'
          IF w_fevega <= w_fecven
               DO error WITH  ;
                  '** Fecha de Garant¡a Menor a Hoy **'
               RETURN .F.
          ENDIF
     CASE VARREAD() = 'W_CODPAG'
          SELECT ge_tab0
          SEEK 'FPAG' + w_codpag
          IF FOUND()
               w_despag = tab_destab
          ELSE
               DO error WITH  ;
                  '** C¢digo de Pago No Encontrado **'
               KEYBOARD '{CTRL+Y}'
               RETURN .F.
          ENDIF
     CASE VARREAD() = 'W_CODEMI'
          SELECT ge_tab0
          SEEK 'EMIS' + w_codemi
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '** Emisor No Encontrado **'
               KEYBOARD '{CTRL+Y}'
               RETURN .F.
          ENDIF
     CASE VARREAD() = 'W_CODMON'
          SELECT ge_tab0
          SEEK 'MONE' + w_codmon
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '** Moneda No Encontrada **'
               KEYBOARD '{CTRL+Y}'
               RETURN .F.
          ELSE
               w_desmon = tab_destab
               @ ROW(), 31 SAY  ;
                 tab_destab
          ENDIF
     CASE VARREAD() = 'W_DESPOR'
          IF w_despor < 0
               RETURN .F.
          ENDIF
          IF w_despor > 0
               IF w_codmon =  ;
                  'DOL '
                    w_desval = ROUND((w_subtot *  ;
                               (w_despor /  ;
                               100)),  ;
                               2)
               ELSE
                    w_desval = ROUND((s_subtot *  ;
                               (w_despor /  ;
                               100)),  ;
                               2)
               ENDIF
               @ 11, 61 SAY  ;
                 w_desval PICTURE  ;
                 '999,999,999.99'
          ENDIF
     CASE VARREAD() = 'W_DESVAL'
          IF w_desval < 0
               RETURN .F.
          ENDIF
          IF w_desval > 0
               IF w_codmon =  ;
                  'DOL '
                    w_despor = ROUND(((w_desval /  ;
                               w_subtot) *  ;
                               100),  ;
                               2)
               ELSE
                    w_despor = ROUND(((w_desval /  ;
                               s_subtot) *  ;
                               100),  ;
                               2)
               ENDIF
               @ 10, 70 SAY  ;
                 w_despor PICTURE  ;
                 '99.99'
          ENDIF
ENDCASE
RETURN
*
PROCEDURE procesa
IF (codtall >= w_talle1 .AND.  ;
   codtall <= w_talle2) .AND.  ;
   (codemi >= w_emiini .AND.  ;
   codemi <= w_emifin)
     IF YEAR(fecemi) > 1994 .AND.  ;
        (auxest = '010 ' .OR.  ;
        auxest = '020 ' .OR.  ;
        auxest = '021 ' .OR.  ;
        auxest = '022 ' .OR.  ;
        auxest = '023 ' .OR.  ;
        auxest = '026 ' .OR.  ;
        auxest = '018 ' .OR.  ;
        auxest = '027 ' .OR.  ;
        auxest = '028 ' .OR.  ;
        auxest = '029 ' .OR.  ;
        auxest = '030 ')
          STORE 0 TO w_rep, s_rep
          SELECT st_iprep
          SEEK st_iorep.numdoc
          IF FOUND()
               SCAN WHILE numord =  ;
                    st_iorep.numdoc  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF indest <>  ;
                       'N'
                         SELECT st_idped
                         SEEK st_iprep.numdoc +  ;
                              st_iprep.numord
                         IF FOUND()
                              SCAN  ;
                               WHILE  ;
                               numdoc =  ;
                               st_iprep.numdoc  ;
                               .AND.  ;
                               numord =  ;
                               st_iprep.numord  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF()
                                   IF canpro > 0
                                        w_rep = w_rep + totite
                                        s_rep = s_rep + ROUND((ROUND(valpro * w_tipcam, 2) * canpro), 2)
                                   ENDIF
                              ENDSCAN
                         ENDIF
                         SELECT st_iprep
                    ENDIF
               ENDSCAN
          ENDIF
          SELECT st_iorep
          IF w_opc = 1
               w_fra = "(indori='GARA' or (indori='GREC' and w_rep>0))"
          ELSE
               IF w_opc = 2
                    w_fra = "(indori='PVEN' or (indori='PREC' and w_rep>0))"
               ELSE
                    w_fra = "(indori='GARA' or (indori='GREC' and w_rep>0)) or (indori='PVEN' or (indori='PREC' and w_rep>0))"
               ENDIF
          ENDIF
          if &w_fra
               SELECT st_plani
               SEEK st_iorep.numdoc
               IF  .NOT. FOUND()  ;
                   .OR. estado =  ;
                   0
                    SELECT gara
                    APPEND BLANK
                    REPLACE numdoc  ;
                            WITH  ;
                            st_iorep.numdoc,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar,  ;
                            codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            codemi  ;
                            WITH  ;
                            st_iorep.codemi
                    REPLACE numsol  ;
                            WITH  ;
                            st_iorep.numsol,  ;
                            numser  ;
                            WITH  ;
                            st_iorep.numser,  ;
                            fecemi  ;
                            WITH  ;
                            st_iorep.fecemi,  ;
                            cosfle  ;
                            WITH  ;
                            st_iorep.flete
                    REPLACE indest  ;
                            WITH  ;
                            st_iorep.indest,  ;
                            indori  ;
                            WITH  ;
                            st_iorep.indori,  ;
                            codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            fecest  ;
                            WITH  ;
                            st_iorep.fecest
                    REPLACE codent  ;
                            WITH  ;
                            st_iseri.codent,  ;
                            codfabo  ;
                            WITH  ;
                            st_iorep.codfabo,  ;
                            cosrep  ;
                            WITH  ;
                            w_rep,  ;
                            cosmob  ;
                            WITH  ;
                            st_iorep.cosmob
                    REPLACE solrep  ;
                            WITH  ;
                            s_rep,  ;
                            solmob  ;
                            WITH  ;
                            ROUND(gara.cosmob *  ;
                            w_tipcam,  ;
                            2),  ;
                            solfle  ;
                            WITH  ;
                            ROUND(gara.cosfle *  ;
                            w_tipcam,  ;
                            2)
                    REPLACE estpla  ;
                            WITH  ;
                            0
               ENDIF
          ENDIF
     ENDIF
     SELECT st_iorep
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

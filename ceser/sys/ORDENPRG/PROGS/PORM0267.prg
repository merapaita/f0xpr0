*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER opc
tit_prg = 'MANTENCION'
wrk_progra = PROGRAM()
CLOSE DATABASES
SET CENTURY ON
DO crea_win
ON KEY LABEL F6 DO AYUCLI
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' Facturas en Garantia (Provedores) '
wk_valpari = 0
wk_crea = 1
wk_igv = 0
wrk_desval = 0
wrk_despor = 0
STORE SPACE(1) TO wrk_despag
STORE SPACE(12) TO wrk_idx
a = 1
DIMENSION codigo( a), cantid( a),  ;
          pordes( a), importe( a),  ;
          totalit( a), unidad(  ;
          a)
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED gc_pro00 ORDER codigo
SELECT 3
USE SHARED st_isrep ORDER codigo
SELECT 4
USE SHARED st_idped ORDER codigo
SELECT 5
USE SHARED st_iorep ORDER codigo
SELECT 6
USE SHARED st_iredo ORDER codigo
SELECT 7
USE SHARED st_estad ORDER  ;
    est_numord
SELECT 8
USE SHARED st_mvord ORDER codigo
SELECT 9
USE SHARED st_iseri ORDER  ;
    SER_CODMAR
SELECT 10
USE SHARED st_iclpr ORDER codigo
SELECT 11
USE SHARED st_iprep ORDER  ;
    rep_numord
SELECT ge_tab0
wrk_varbus = '"IGV " + "IGV "'
SEEK &wrk_varbus
IF FOUND()
     wk_igv = tab_factor
     w_facigv = tab_factor / 100
ELSE
     do error with '**No Definido el &empre9**'
     ppas = .F.
ENDIF
error = .F.
ppas = .T.
DO fech
IF error = .T.
     ppas = .F.
ENDIF
SET CURSOR ON
STORE SPACE(04) TO w_talle1,  ;
      w_talle2, w_emiini,  ;
      w_emifin
STORE 0 TO wk_codcli
DO WHILE ppas
     STORE .F. TO eror_imp,  ;
           sw_algo, sw_algo
     con_eli = 0
     STORE SPACE(4) TO wk_nomcli,  ;
           wk_codpag
     STORE 'FACT' TO wk_coddoc
     STORE 0 TO wk_fle, wk_otro,  ;
           wk_monto, wk_numdoc,  ;
           wrk_desval,  ;
           wrk_despor
     DO esc_modo WITH 'C'
     DO esc_indica WITH 1, 'AYU',  ;
        'BUS', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wk_fevenga = DATE() + empre7
     wk_fecven = DATE()
     SELECT st_iclpr
     @ 03, 1 CLEAR TO 14, 78
     @ 04, 02 SAY  ;
       'C¢digo Provedor:'
     @ 05, 02 SAY  ;
       'Del Taller     :'
     @ 06, 02 SAY  ;
       'Al  Taller     :'
     @ 07, 02 SAY  ;
       'Del Emisor     :'
     @ 08, 02 SAY  ;
       'Al  Emisor     :'
     @ 04, 20 GET wk_codcli  ;
       PICTURE '999999999' VALID  ;
       vali_cli(wk_codcli) WHEN  ;
       antes(1)
     @ 05, 20 GET w_talle1  ;
       FUNCTION '@!' VALID  ;
       vali_pag(w_talle1,3) WHEN  ;
       antes(2)
     @ 06, 20 GET w_talle2 RANGE  ;
       w_talle1 FUNCTION '@!'  ;
       VALID vali_pag(w_talle2,3)  ;
       WHEN antes(2)
     @ 07, 20 GET w_emiini  ;
       FUNCTION '@!' VALID  ;
       vali_pag(w_emiini,2) WHEN  ;
       antes(1)
     @ 08, 20 GET w_emifin RANGE  ;
       w_emiini FUNCTION '@!'  ;
       VALID vali_pag(w_emifin,2)  ;
       WHEN antes(1)
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
          STORE 0 TO wk_flete,  ;
                wk_monto,  ;
                tot_flete,  ;
                tot_otro,  ;
                sol_cosrep,  ;
                sol_cosmob,  ;
                sol_flete,  ;
                sol_subtot,  ;
                sol_totnet
          STORE 0 TO wk_repues,  ;
                wk_mano,  ;
                wk_subtot,  ;
                wk_totdes,  ;
                wk_totnet,  ;
                wk_totigv,  ;
                wk_totbru,  ;
                sol_descue,  ;
                sol_totgen,  ;
                sol_totvta,  ;
                sol_igv
          STORE SPACE(8) TO  ;
                num_ord
          a = 1
          SELECT gara
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               IF SUBSTR(codfabo,  ;
                  1, 1) = 'û'
                    num_ord = numdoc
                    wk_numsol = numsol
                    confi = ' '
                    wk_repues = wk_repues +  ;
                                cosrep
                    wk_mano = wk_mano +  ;
                              cosmob
                    wk_flete = wk_flete +  ;
                               cosfle
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
                    SELECT gara
               ENDIF
          ENDSCAN
          a = a - 1
          wk_codemi = SPACE(4)
          ON KEY LABEL F6 DO AYUCLI
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'IGN',  ;
             'ESC'
          @ 03, 01 CLEAR TO 15,  ;
            78
          @ 03, 17 TO 13, 55
          @ 04, 20 SAY  ;
            'Documento de Venta...:  FACTURA'
          @ 05, 20 SAY  ;
            'N£mero de Factura....:'
          @ 06, 20 SAY  ;
            'Fecha de Vencimiento.:'
          @ 07, 20 SAY  ;
            'Fecha Venc.Gar.Repar.:'
          @ 08, 20 SAY  ;
            'C¢digo de Pago.......:'
          @ 09, 20 SAY  ;
            'C¢digo de la Empresa.:'
          IF LASTKEY() <> -9  ;
             .AND. LASTKEY() <>  ;
             27
               @ 05, 44 GET  ;
                 wk_numdoc VALID  ;
                 vali_doc('FACT', ;
                 wk_numdoc)
               @ 06, 44 GET  ;
                 wk_fecven VALID  ;
                 vali_fe(wk_fecven)  ;
                 .AND. LASTKEY() <>  ;
                 5
               @ 07, 44 GET  ;
                 wk_fevenga RANGE  ;
                 wk_fecven VALID  ;
                 vali_fe(wk_fevenga)
               @ 08, 44 GET  ;
                 wk_codpag  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 vali_pag(wk_codpag, ;
                 1) WHEN  ;
                 colocaf6()
               @ 09, 44 GET  ;
                 wk_codemi  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 vali_pag(wk_codemi, ;
                 2) WHEN  ;
                 colocaf6()
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
CLOSE DATABASES
IF FILE(wrk_idx)
     erase &wrk_idx
ENDIF
DO sacawin
ON KEY LABEL F6
RETURN
*
FUNCTION vali_doc
PARAMETER doc, num
IF TYPE('num') = 'C'
     num = f_ceros(num,10,2)
     wk_numdoc = f_ceros(wk_numdoc, ;
                 10,2)
ELSE
     num = f_ceros(num,10,1)
     wk_numdoc = f_ceros(wk_numdoc, ;
                 10,1)
ENDIF
boleano = 0
DO control WITH boleano
IF boleano = 1
     RETURN 0
ENDIF
IF num = '0000000000'
     DO error WITH  ;
        '**Debe Ingresar N£mero**'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
SELECT 20
USE SHARED gc_hve00 ORDER codigo
SEEK doc + num
IF FOUND()
     DO error WITH  ;
        '**C¢digo ya Ingresado**'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ELSE
     RETURN .T.
ENDIF
RETURN
*
PROCEDURE actual
STORE SPACE(4) TO w_codtall,  ;
      w_estado
DO mensa WITH 'Grabando '+ ;
   wk_coddoc+' N§ '+wk_numdoc,  ;
   'COLO'
SELECT gara
GOTO TOP
DO WHILE  .NOT. EOF()
     num_ord = numdoc
     IF SUBSTR(codfabo, 1, 1) =  ;
        'û'
          SELECT st_iredo
          APPEND BLANK
          DO rbloquea
          REPLACE indodo WITH  ;
                  'ORD'
          REPLACE numodo WITH  ;
                  num_ord
          REPLACE indddo WITH  ;
                  'FACT'
          REPLACE numddo WITH  ;
                  wk_numdoc
          REPLACE user WITH users
          REPLACE date WITH  ;
                  DATE()
          REPLACE time WITH  ;
                  TIME()
          UNLOCK
          SELECT st_iorep
          SEEK num_ord
          IF FOUND()
               DO rbloquea
               w_estado = auxest
               w_codtall = codtall
               REPLACE indest  ;
                       WITH 'F'
               REPLACE codfabo  ;
                       WITH  ;
                       wk_coddoc
               REPLACE numfabo  ;
                       WITH  ;
                       wk_numdoc
               REPLACE fecfabo  ;
                       WITH  ;
                       wk_fecven
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               IF w_codtall >  ;
                  '010 '
                    REPLACE auxest  ;
                            WITH  ;
                            '100 '
               ELSE
                    DO CASE
                         CASE w_estado =  ;
                              '010 '  ;
                              .OR.  ;
                              w_estado =  ;
                              '021 '  ;
                              .OR.  ;
                              w_estado =  ;
                              '026 '
                              REPLACE  ;
                               auxest  ;
                               WITH  ;
                               '080 '
                         CASE w_estado =  ;
                              '020 '  ;
                              .OR.  ;
                              w_estado =  ;
                              '022 '  ;
                              .OR.  ;
                              w_estado =  ;
                              '023 '  ;
                              .OR.  ;
                              w_estado =  ;
                              '028 '  ;
                              .OR.  ;
                              w_estado =  ;
                              '029 '
                              REPLACE  ;
                               auxest  ;
                               WITH  ;
                               '100 '
                    ENDCASE
               ENDIF
               REPLACE fecest  ;
                       WITH  ;
                       DATE(),  ;
                       horest  ;
                       WITH  ;
                       TIME()
               UNLOCK
               w_estado = auxest
          ENDIF
          SELECT st_mvord
          APPEND BLANK
          DO rbloquea
          REPLACE orden WITH  ;
                  num_ord, dia  ;
                  WITH DATE()
          REPLACE hora WITH  ;
                  TIME(), user  ;
                  WITH users
          REPLACE date WITH  ;
                  DATE(), time  ;
                  WITH TIME()
          REPLACE estado WITH  ;
                  w_estado,  ;
                  destado WITH  ;
                  SUBSTR(ootab('ESOR', ;
                  estado), 1, 22) +  ;
                  ' ' + wk_coddoc +  ;
                  ' ' +  ;
                  ALLTRIM(wk_numdoc)
          UNLOCK
          SELECT st_estad
          SEEK num_ord
          IF FOUND()
               DO rbloquea
               REPLACE coddoc  ;
                       WITH  ;
                       wk_coddoc
               REPLACE numfabo  ;
                       WITH  ;
                       wk_numdoc
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          SELECT st_iprep
          SEEK num_ord
          IF FOUND()
               SCAN WHILE numord =  ;
                    num_ord .AND.   ;
                    .NOT. EOF()
                    IF indest <>  ;
                       'N'
                         DO rbloquea
                         REPLACE indest  ;
                                 WITH  ;
                                 'F'
                         REPLACE user  ;
                                 WITH  ;
                                 users
                         REPLACE date  ;
                                 WITH  ;
                                 DATE()
                         REPLACE time  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                    ENDIF
               ENDSCAN
          ENDIF
          SELECT st_isrep
          SEEK gara.numsol
          IF FOUND()
               DO rbloquea
               REPLACE indest  ;
                       WITH 'F'
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
          ENDIF
     ENDIF
     SELECT gara
     SKIP
ENDDO
SELECT 20
USE SHARED gc_dve00 ORDER codigo
FOR b = 1 TO a
     APPEND BLANK
     DO rbloquea
     REPLACE dve_tipdoc WITH  ;
             'FACT'
     REPLACE dve_nrodoc WITH  ;
             wk_numdoc
     REPLACE dve_propar WITH  ;
             codigo(b)
     REPLACE dve_cantid WITH  ;
             cantid(b)
     REPLACE dve_pordes WITH  ;
             pordes(b)
     REPLACE dve_import WITH  ;
             importe(b)
     REPLACE dve_total WITH  ;
             totalit(b)
     REPLACE dve_unimed WITH  ;
             unidad(b)
     REPLACE dve_coprmb WITH  ;
             oocosprb(codigo(b))
     REPLACE dve_coprmo WITH  ;
             oocospro(codigo(b))
     REPLACE dve_usuari WITH  ;
             users
     REPLACE dve_fecha WITH  ;
             DATE()
     REPLACE dve_hora WITH TIME()
     UNLOCK
ENDFOR
DO mensa WITH 'Grabando '+ ;
   wk_coddoc+' N§ '+wk_numdoc,  ;
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
       (4), codent C (9), codfabo  ;
       C (4), cosrep N (9, 2),  ;
       cosmob N (9, 2), fecest D  ;
       (8), fecvta D (8))
wk_codpro = STR(wk_codcli, 9)
SELECT st_iorep
SET ORDER TO ord_inesta
SET RELATION TO codmar + codmod + numser;
INTO st_iseri
FOR i = 1 TO 2
     IF i = 1
          SEEK 'GARA' + 'C   '
     ELSE
          SEEK 'GREC' + 'C   '
     ENDIF
     SCAN WHILE (indori = 'GARA'  ;
          .OR. indori = 'GREC')  ;
          .AND. indest = 'C   '  ;
          .AND.  .NOT. EOF()
          IF st_iseri.codent =  ;
             wk_codpro .AND.  ;
             (codtall >= w_talle1  ;
             .AND. codtall <=  ;
             w_talle2) .AND.  ;
             (codemi >= w_emiini  ;
             .AND. codemi <=  ;
             w_emifin)
               IF YEAR(fecemi) >  ;
                  1994 .AND.  ;
                  (auxest =  ;
                  '010 ' .OR.  ;
                  auxest = '020 '  ;
                  .OR. auxest =  ;
                  '021 ' .OR.  ;
                  auxest = '022 '  ;
                  .OR. auxest =  ;
                  '023 ' .OR.  ;
                  auxest = '026 '  ;
                  .OR. auxest =  ;
                  '018 ' .OR.  ;
                  auxest = '027 '  ;
                  .OR. auxest =  ;
                  '028 ' .OR.  ;
                  auxest =  ;
                  '029 ')
                    w_rep = 0
                    SELECT st_iprep
                    SEEK st_iorep.numdoc
                    IF FOUND()
                         SCAN WHILE  ;
                              numord =  ;
                              st_iorep.numdoc  ;
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
                                                  w_rep = w_rep + totite
                                             ENDIF
                                        ENDSCAN
                                   ENDIF
                                   SELECT st_iprep
                              ENDIF
                         ENDSCAN
                    ENDIF
                    SELECT st_iorep
                    IF indori =  ;
                       'GARA'  ;
                       .OR.  ;
                       (indori =  ;
                       'GREC'  ;
                       .AND.  ;
                       w_rep >  ;
                       0)
                         SELECT gara
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
                                 st_iorep.codmod
                         REPLACE numsol  ;
                                 WITH  ;
                                 st_iorep.numsol,  ;
                                 numser  ;
                                 WITH  ;
                                 st_iorep.numser,  ;
                                 fecemi  ;
                                 WITH  ;
                                 st_iorep.fecemi
                         REPLACE indest  ;
                                 WITH  ;
                                 st_iorep.indest,  ;
                                 indori  ;
                                 WITH  ;
                                 st_iorep.indori,  ;
                                 codtall  ;
                                 WITH  ;
                                 st_iorep.codtall
                         REPLACE codent  ;
                                 WITH  ;
                                 st_iseri.codent,  ;
                                 codfabo  ;
                                 WITH  ;
                                 st_iorep.codfabo,  ;
                                 cosrep  ;
                                 WITH  ;
                                 w_rep
                         REPLACE cosmob  ;
                                 WITH  ;
                                 st_iorep.cosmob,  ;
                                 cosfle  ;
                                 WITH  ;
                                 st_iorep.flete,  ;
                                 fecest  ;
                                 WITH  ;
                                 st_iorep.fecest,  ;
                                 fecvta  ;
                                 WITH  ;
                                 st_iseri.fecvta
                    ENDIF
               ENDIF
               SELECT st_iorep
          ENDIF
     ENDSCAN
ENDFOR
SELECT st_iorep
SET ORDER TO CODIGO
wrk_idx = f_indice()
SELECT gara
INDEX ON DTOS(FECEMI)+NUMDOC TO &WRK_IDX
GOTO TOP
DO mensa WITH  ;
   '*** Espere un Momento, Por Favor ***',  ;
   'SACA'
campo = 'subst(CODFABO,1,1)+numdoc+" "+dtoc(fecemi)+" "+SUBSTR(CODMOD,1,09)+" "+SUBSTR(NUMSER,1,10)+" "+STR(COSREP,7,2)+" "+STR(COSMOB,6,2)+" "+ STR(cosfle,6,2)+" "+CODTALL+" "+indori'
mensaje = 'N§ ORDENÄINGRESOÄÄÄMODELOÄÄÄÄSERIEÄÄÄÄÄÄÄREPTO.ÄÄÄM.O.ÄÄFLET.ÄTALLÄTIPO'
DO esc_indica WITH 1, 'AYU',  ;
   'TOD', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'ESP', 'IMP', 'ESC'
ON KEY LABEL F8 DO MARCA
ON KEY LABEL F7 DO IMPRES
define popup ayu0 from 0,0 to 08,74 promp;
field &campo title mensaje COLOR SCHEME;
8
ON SELECTION POPUP ayu0 do choice0
ACTIVATE POPUP ayu0 NOWAIT
FOR i = 1 TO 04
     MOVE POPUP ayu0 BY 1, 0
ENDFOR
ACTIVATE POPUP ayu0
DEACTIVATE POPUP ayu0
SELECT gara
GOTO TOP
DO WHILE  .NOT. EOF()
     IF SUBSTR(codfabo, 1, 1) =  ;
        'û'
          sw_algo = .T.
     ENDIF
     SKIP
ENDDO
IF LASTKEY() = 27
     ppas = .T.
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
FUNCTION vali_fe
PARAMETER toc
IF EMPTY(toc)
     IF VARREAD() = 'WK_FECVEN'
          wk_fecven = DATE()
     ELSE
          wk_fevenga = DATE() +  ;
                       empre7
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
FUNCTION vali_pag
PARAMETER pag, cas
IF EMPTY(pag)
     DO error WITH  ;
        ' **Debe Ingresar C¢digo**'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
IF cas = 1
     SELECT ge_tab0
     llave = 'FPAG' + pag
     SEEK llave
     IF FOUND()
          wrk_despag = tab_destab
          RETURN .T.
     ELSE
          DO error WITH  ;
             '**C¢digo Pago No Encontrado**'
          KEYBOARD '{CTRL+Y}'
          RETURN .F.
     ENDIF
ENDIF
IF cas = 2
     SELECT ge_tab0
     llave = 'EMIS' + pag
     SEEK llave
     IF FOUND()
          @ ROW(), 31 SAY  ;
            tab_destab
          wrk_despag = tab_destab
          RETURN .T.
     ELSE
          DO error WITH  ;
             '**C¢digo No Encontrado**'
          KEYBOARD '{CTRL+Y}'
          RETURN .F.
     ENDIF
ENDIF
IF cas = 3
     ON KEY LABEL f6
     SELECT ge_tab0
     llave = 'TALL' + pag
     SEEK llave
     IF FOUND()
          @ ROW(), 31 SAY  ;
            tab_destab
          RETURN .T.
     ELSE
          DO error WITH  ;
             '**C¢digo No Encontrado**'
          KEYBOARD '{CTRL+Y}'
          RETURN .F.
     ENDIF
ENDIF
RETURN .T.
*
PROCEDURE muestra
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
confi = ' '
SELECT st_iclpr
llave = 'P' + STR(wk_codcli, 9)
SEEK llave
IF FOUND()
     wk_nomcli = SUBSTR(noment, 1,  ;
                 25)
     wk_dir = SUBSTR(nomcal, 1,  ;
              20) +  ;
              SUBSTR(ootab('DIST', ;
              nomdis), 1, 20)
ENDIF
sale = 0
ON KEY LABEL f10 do f10s
@ 03, 01 CLEAR TO 15, 79
@ 04, 01 SAY  ;
  ' E N C A B E Z A D O ' COLOR  ;
  SCHEME 8
@ 04,43 say " T O T A L E S           &empre13";
COLOR SCHEME 8
@ 06, 01 SAY  ;
  'Provedor...........: ' +  ;
  SUBSTR(wk_nomcli, 1, 20)
@ 07, 01 SAY  ;
  'C¢digo Provedor....: ' +  ;
  STR(wk_codcli, 9)
@ 08, 01 SAY  ;
  'Direcci¢n..........: ' +  ;
  SUBSTR(wk_dir, 1, 20)
@ 09, 01 SAY  ;
  'Forma de Pago......: ' +  ;
  SUBSTR(wrk_despag, 1, 20)
@ 10, 01 SAY  ;
  'Fecha de Emisi¢n...: ' +  ;
  DTOC(wk_fecven)
@ 10, 43 SAY  ;
  'Dscto. Adicion.(%): '
@ 11, 01 SAY  ;
  'Fecha Gar.Venc.Rep.: ' +  ;
  DTOC(wk_fevenga)
@ 11, 43 SAY  ;
  'Dscto. Adicion.($): '
wk_subtot = ROUND(wk_repues * (1 +  ;
            w_facigv), 2) +  ;
            ROUND(wk_mano * (1 +  ;
            w_facigv), 2) +  ;
            ROUND(wk_flete * (1 +  ;
            w_facigv), 2)
wk_totnet = wk_subtot
wk_totgen = wk_totnet - wk_totdes
wk_totbru = wk_totgen
wk_totvta = ROUND((wk_totbru / (1 +  ;
            w_facigv)), 2)
wk_totigv = wk_totbru - wk_totvta
@ 06, 43 SAY  ;
  'Costo En Repuesto : ' +  ;
  TRANSFORM(ROUND(wk_repues * (1 +  ;
  w_facigv), 2), '$999,999.99')
@ 07, 43 SAY  ;
  'Costo Mano de Obra: ' +  ;
  TRANSFORM(ROUND(wk_mano * (1 +  ;
  w_facigv), 2), '$999,999.99')
@ 08, 43 SAY  ;
  'Flete/Otros.......: ' +  ;
  TRANSFORM(ROUND(wk_flete * (1 +  ;
  w_facigv), 2), '$999,999.99')
@ 09, 43 SAY  ;
  'Total Neto.. .....: ' +  ;
  TRANSFORM(wk_totnet,  ;
  '$999,999.99')
@ 13, 01 SAY  ;
  'Total Venta.......: ' +  ;
  TRANSFORM(wk_totvta,  ;
  '$999,999.99')
@ 14,01 say "Total &empre9.........: ";
+ transform(wk_totigv,"$999,999.99")
@ 14, 43 SAY  ;
  'Total a Pagar.....: ' +  ;
  TRANSFORM(wk_totbru,  ;
  '$999,999.99')
@ 10, 69 GET wrk_despor PICTURE  ;
  '99.99' VALID despues(2)
@ 11, 64 GET wrk_desval PICTURE  ;
  '999,999.99' VALID despues(1)
READ
ON KEY LABEL f10
IF sale = 0 .AND. LASTKEY() <> 27
     wk_totdes = wrk_desval
     wk_totgen = wk_totnet -  ;
                 wk_totdes
     wk_totbru = wk_totgen
     wk_totvta = ROUND((wk_totbru /  ;
                 (1 + w_facigv)),  ;
                 2)
     wk_totigv = wk_totbru -  ;
                 wk_totvta
     @ 13, 21 SAY wk_totvta  ;
       PICTURE '$999,999.99'
     @ 14, 21 SAY wk_totigv  ;
       PICTURE '$999,999.99'
     @ 14, 63 SAY wk_totbru  ;
       PICTURE '$999,999.99'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'GRA', 'IGN'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     = INKEY(0, 'H')
     CLEAR TYPEAHEAD
     DO WHILE LASTKEY()<>27 .AND.  ;
        LASTKEY()<>-6 .AND.  ;
        LASTKEY()<>-1 .AND.  ;
        LASTKEY()<>-9
          = INKEY(0)
     ENDDO
     eror_imp = .F.
     wk_obliga = .F.
     IF LASTKEY() = -1
          ON KEY
          DO con_use
          IF eror_imp = .T.
               DO error WITH  ;
                  '**Ya se factur¢,vuelva a ingresar**'
          ELSE
               DO actual
          ENDIF
     ENDIF
ENDIF
@ 4, 1 CLEAR TO 14, 79
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
lin = 22
SET CONSOLE OFF
SET PRINTER ON
SET DEVICE TO PRINTER
SET PRINT TO &RGE_LPTFAC      ;
                   
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(67) + CHR(51)
? CHR(18)
sw_factura = 1
cuenta_lin = 0
@ 11, 00 SAY wk_fecven
@ 13, 10 SAY wk_nomcli
@ 13, 48 SAY wk_codcli
@ 15, 10 SAY wk_dir
@ 17, 28 SAY 'DOLARES'
@ 17, 45 SAY 'CREDITO '
@ 23, 16 SAY  ;
  'Por el servicio de reparaci¢n'
@ 24, 16 SAY  ;
  'de artefactos electrodom‚sticos'
@ 25, 16 SAY  ;
  'seg£n Ordenes de Reparaci¢n adjuntas'
IF wk_flete > 0
     @ 34, 02 SAY 'Flete :'
     @ 34, 10 SAY ROUND(wk_flete *  ;
       (1 + w_facigv), 2) PICTURE  ;
       '$999,999.99'
ENDIF
@ 35, 10 SAY ROUND((wk_repues *  ;
  (1 + w_facigv)), 2) PICTURE  ;
  '$999,999.99'
@ 35, 67 SAY wk_totvta PICTURE  ;
  '$999,999.99'
@ 36, 10 SAY ROUND((wk_mano * (1 +  ;
  w_facigv)), 2) PICTURE  ;
  '$999,999.99'
@ 36, 67 SAY wk_totigv PICTURE  ;
  '$999,999.99'
@ 37, 10 SAY wk_totdes PICTURE  ;
  '$999,999.99'
@ 37, 67 SAY wk_totbru PICTURE  ;
  '$999,999.99'
@ 40, 01 SAY oonumlet(wk_totbru, ;
  'DOL ')
@ 44, 60 SAY wk_numdoc
EJECT
SET PRINTER TO
SET PRINTER OFF
SET CONSOLE ON
SET DEVICE TO SCREEN
CLEAR TYPEAHEAD
RETURN
*
FUNCTION vali_cli
PARAMETER cli
boleano = 0
DO control WITH boleano
IF boleano = 1
     RETURN 0
ENDIF
IF EMPTY(cli)
     DO error WITH  ;
        '**Debe Ingresar Proveedor**'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
SELECT st_iclpr
llave = 'P' + STR(cli, 9)
SEEK llave
IF FOUND()
     wk_nomcli = SUBSTR(noment, 1,  ;
                 25)
     @ 4, 31 SAY wk_nomcli
     RETURN .T.
ELSE
     DO error WITH  ;
        '**C¢digo de Provedor No Encontrado**'
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE ayucli
ON KEY LABEL F6
IF VARREAD() = 'WK_CODCLI'
     DO mensa WITH  ;
        '*** Espere un Momento, Por Favor ***',  ;
        'COLO'
     SELECT st_iclpr
     SET FILTER TO indent == 'P'
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE ENTIDADES'
     DO mensa WITH  ;
        '*** Espere un Momento, Por Favor ***',  ;
        'SACA'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
ENDIF
IF VARREAD() = 'WK_CODPAG'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'FPAG'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE PAGOS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
ENDIF
IF VARREAD() = 'WK_CODEMI' .OR.  ;
   VARREAD() = 'W_EMIINI' .OR.  ;
   VARREAD() = 'W_EMIFIN'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'EMIS'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE EMISOR'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
ENDIF
SET FILTER TO
ON KEY LABEL F6 DO AYUCLI
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
PROCEDURE fech
SELECT 20
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
SEEK DTOS(DATE()) + '1' + 'SOL ' +  ;
     'DOL '
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
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          IF wrk_desval < 0
               RETURN .F.
          ENDIF
          IF wrk_desval = 0
               RETURN
          ELSE
               wrk_despor = ROUND(((wrk_desval /  ;
                            wk_subtot) *  ;
                            100),  ;
                            2)
               @ 10, 69 SAY  ;
                 wrk_despor  ;
                 PICTURE '99.99'
          ENDIF
     CASE opc = 2
          IF wrk_despor < 0
               RETURN .F.
          ENDIF
          IF wrk_despor = 0
               RETURN
          ELSE
               wrk_desval = ROUND((wk_subtot *  ;
                            (wrk_despor /  ;
                            100)),  ;
                            2)
               @ 11, 64 SAY  ;
                 wrk_desval  ;
                 PICTURE  ;
                 '99,999.99'
          ENDIF
ENDCASE
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
REPORT FORMAT PORM0262 TO PRINTER
?? CHR(18)
SET PRINTER OFF
SET CONSOLE ON
DO mensa WITH  ;
   '***  I m p r i m i e n d o  ***',  ;
   'SACA'
RETURN
*
PROCEDURE status_imp
wk_printer = '1'
DO WHILE wk_printer='1'
     SELECT 20
     USE
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
          DO rbloquea
          IF wk_coddoc = 'FACT'
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
wk_numdoc = f_ceros(sys_numfac,10, ;
            1)
RETURN
*
PROCEDURE fin_imp
SELECT 20
USE SHARED st_iparg
DO rbloquea
IF wk_coddoc = 'FACT'
     REPLACE sys_lptfac WITH '0'
ELSE
     REPLACE sys_lptbol WITH '0'
ENDIF
UNLOCK
RETURN
*
FUNCTION f_indice
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'TEMPO' +  ;
           LTRIM(STR(cor))
     IF FILE(ret + '.IDX')
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
PROCEDURE antes
PARAMETER opc
IF opc = 1
     ON KEY LABEL f6 do ayucli
ELSE
     ON KEY LABEL f6 do ayuda
ENDIF
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT ge_tab0
SET FILTER TO tab_codpre == 'TALL'
titulo = 'AYUDA DE TALLERES'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'ESC'
SET FILTER TO
RETURN
*
PROCEDURE facant
SELECT 20
USE SHARED gc_hve00 ORDER codigo
APPEND BLANK
DO rbloquea
REPLACE hve_tipdoc WITH 'FACT'
REPLACE hve_nrodoc WITH wk_numdoc
REPLACE hve_fecdoc WITH DATE()
REPLACE hve_fecvct WITH wk_fecven
REPLACE hve_fecgar WITH  ;
        wk_fevenga
REPLACE hve_almdes WITH empre6
REPLACE hve_tipent WITH 'C   '
REPLACE hve_codent WITH  ;
        STR(wk_codcli, 9)
REPLACE hve_codmov WITH 'EVTA'
REPLACE hve_tippag WITH wk_codpag
REPLACE hve_tidore WITH 'ORDE'
REPLACE hve_estdoc WITH 'O'
REPLACE hve_indori WITH 'GARA'
REPLACE hve_codmon WITH 'DOL '
REPLACE hve_fechtc WITH DATE()
REPLACE hve_cosmob WITH wk_mano
REPLACE hve_cosrep WITH wk_repues
REPLACE hve_totnet WITH wk_totnet
REPLACE hve_pordes WITH  ;
        wrk_despor
REPLACE hve_totdes WITH wk_totdes
REPLACE hve_totvta WITH wk_totvta
REPLACE hve_flete WITH  ;
        ROUND(wk_flete * (1 +  ;
        w_facigv), 2)
REPLACE hve_totigv WITH wk_totigv
REPLACE hve_totgen WITH wk_totgen
REPLACE hve_totoim WITH wk_totgen
REPLACE hve_usuari WITH users
REPLACE hve_fecha WITH DATE()
REPLACE hve_hora WITH TIME()
DO CASE
     CASE STR(wk_codcli, 9) =  ;
          ' 10663571'
          REPLACE hve_codemi WITH  ;
                  '100 '
     CASE STR(wk_codcli, 9) =  ;
          ' 25360443'
          REPLACE hve_codemi WITH  ;
                  wk_codemi
     CASE STR(wk_codcli, 9) =  ;
          ' 10032253' .OR.  ;
          STR(wk_codcli, 9) =  ;
          ' 10663580'
          REPLACE hve_codemi WITH  ;
                  '300 '
     OTHERWISE
          REPLACE hve_codemi WITH  ;
                  wk_codemi
ENDCASE
sol_cosrep = ROUND((wk_repues *  ;
             wk_valpari), 2)
sol_cosmob = ROUND((wk_mano *  ;
             wk_valpari), 2)
sol_flete = ROUND(ROUND(wk_flete *  ;
            wk_valpari, 2) * (1 +  ;
            w_facigv), 2)
sol_subtot = ROUND((sol_cosrep +  ;
             sol_cosmob) * (1 +  ;
             w_facigv), 2)
sol_totnet = sol_subtot +  ;
             sol_flete
sol_descue = ROUND((wk_totdes *  ;
             wk_valpari), 2)
sol_totgen = sol_totnet -  ;
             sol_descue
sol_totvta = ROUND((sol_totgen /  ;
             (1 + w_facigv)), 2)
sol_igv = sol_totgen - sol_totvta
REPLACE hve_solrep WITH  ;
        sol_cosrep
REPLACE hve_solmob WITH  ;
        sol_cosmob
REPLACE hve_solfle WITH sol_flete
REPLACE hve_solnet WITH  ;
        sol_totnet
REPLACE hve_soldes WITH  ;
        sol_descue
REPLACE hve_solvta WITH  ;
        sol_totvta
REPLACE hve_soligv WITH sol_igv
REPLACE hve_solgen WITH  ;
        sol_totgen
REPLACE hve_tipcam WITH  ;
        wk_valpari
REPLACE hve_mtocan WITH  ;
        sol_totgen
UNLOCK
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

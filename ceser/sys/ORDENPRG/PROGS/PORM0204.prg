*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER opc
tit_prg = '    ACTUALIZACION    '
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUACT
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   '   DE  PRESUPUESTO   '
w_tipcam = 0
w_crea = 1
w_igv = 0
a = 1
CLOSE DATABASES
SELECT 1
USE SHARED st_iparg
SELECT 2
USE SHARED gc_par00
rge_monbas = par_monbas
SELECT 3
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
SELECT 4
USE SHARED ge_tab0 ORDER codigo
SELECT 5
USE SHARED gc_pro00 ORDER codigo
SELECT 6
USE SHARED st_iclpr ORDER codigo
SELECT 7
USE SHARED st_idpre ORDER codigo
SELECT 8
USE SHARED st_iorep ORDER codigo
SELECT 9
USE SHARED st_ispre ORDER  ;
    pre_infenu
SELECT 10
USE SHARED gc_dlp00 ORDER codigo
SELECT ge_tab0
w_varbus = '"IGV " + "IGV "'
SEEK &w_varbus
IF FOUND()
     w_igv = tab_factor
     w_facigv = tab_factor / 100
ELSE
     DO error WITH  ;
        '**No Definido el IGV**'
     error = .T.
     RETURN
ENDIF
error = .F.
w_fpari = DATE()
SELECT gc_cmv00
SEEK DTOS(w_fpari) + '1' +  ;
     rge_monbas + 'DOL '
w_tipcam = 3.25 
IF error = .T.
     ppas = .F.
ELSE
     ppas = .T.
ENDIF
b1 = ''
b2 = '같같같같같같같같같같같같같같같같같같같같같같같같같'
b3 = '쳐컴컴컴컴컴쩡컴컴컴컴컴탠컴컴컴컴컴쩡컴컴컴컴컴캑'
b4 = '0                      50%                    100%'
SET CURSOR ON
DO WHILE ppas
     STORE 0 TO w_totman,  ;
           w_numpre, w_pagsol,  ;
           w_pagdol, w_totnet,  ;
           w_totigv, w_totgrl,  ;
           w_total, w_totrep,  ;
           w_totdes, w_totpre
     STORE 0 TO s_totpor, s_subto,  ;
           s_totgrl, s_total,  ;
           s_totrep, s_totnet,  ;
           s_totigv, s_totafe,  ;
           s_totman, s_totdes,  ;
           w_por
     STORE 0 TO con_eli, w_totant,  ;
           w_totnue, nreg, creg,  ;
           w_numte1, w_numte2,  ;
           w_numsol, w_numord,  ;
           w_codcli, w_tecnic,  ;
           w_totafe
     STORE SPACE(4) TO w_codmar,  ;
           w_codstk, w_nomcli,  ;
           w_codpag, w_nomdis,  ;
           w_nomciu, w_indori,  ;
           w_codmon, w_codta1,  ;
           w_codta2
     STORE SPACE(30) TO w_nommar,  ;
           w_destia, w_nommon,  ;
           w_nommod, w_doctia,  ;
           w_noment, w_nomcal,  ;
           w_desdis, w_desciu
     STORE SPACE(15) TO w_codmod,  ;
           w_docgar
     STORE CTOD(SPACE(8)) TO  ;
           w_fecemi, w_fecha,  ;
           w_fecven, w_fecvta
     STORE SPACE(20) TO w_numser
     DIMENSION pro( 12), dex( 12),  ;
               can( 12), prea(  ;
               12), pren( 12),  ;
               totp( 12), totn(  ;
               12), pre( 12),  ;
               pres( 12), tot(  ;
               12), tots( 12)
     DIMENSION w_obspre( 08)
     DIMENSION imp_obser( 4)
     w_nomtec = SPACE(40)
     STORE .F. TO eror_imp,  ;
           sw_algo
     STORE 1 TO w_opc, w_sal,  ;
           w_imp, ncopia
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'MBV', 'BBB', 'bbb'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'bbb', 'ESC'
     @ 3, 1 CLEAR TO 14, 78
     DO mues_por
     @ 05, 15 GET w_opc DEFAULT 1  ;
       PICTURE  ;
       '@*RVTN Procesar ; Salir   '
     READ
     IF LASTKEY() <> 27 .AND.  ;
        w_opc = 1
          @ 05, 30 SAY  ;
            'Del taller :' GET  ;
            w_codta1 PICTURE '@!'  ;
            VALID valtab(w_codta1, ;
            2)
          @ 06, 30 SAY  ;
            'Al  taller :' GET  ;
            w_codta2 PICTURE '@!'  ;
            VALID w_codta2 >=  ;
            w_codta1 .AND.  ;
            valtab(w_codta2,2)
          READ
          IF LASTKEY() <> 27  ;
             .AND. w_codta1 <>  ;
             SPACE(4) .AND.  ;
             w_codta2 <>  ;
             SPACE(4)
               w_codmon = 'DOL '
               @ 07, 30 SAY  ;
                 'Moneda     :'  ;
                 GET w_codmon  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 valtab(w_codmon, ;
                 1)
               READ
               IF LASTKEY() <> 27
                    DO pro_pre
                    IF sw_algo =  ;
                       .T.
                         DO esc_indica  ;
                            WITH  ;
                            1,  ;
                            'AYU',  ;
                            'MBV',  ;
                            'BBB',  ;
                            'bbb'
                         DO esc_indica  ;
                            WITH  ;
                            2,  ;
                            'BBB',  ;
                            'BBB',  ;
                            'bbb',  ;
                            'ESC'
                         @ 13, 25  ;
                           SAY  ;
                           'Copias    :'  ;
                           GET  ;
                           ncopia  ;
                           DEFAULT  ;
                           1  ;
                           PICTURE  ;
                           '@*RHTN 1 ;2 '
                         READ
                         IF LASTKEY() <>  ;
                            27
                              @ 14,  ;
                                25  ;
                                SAY  ;
                                'Impresi줻 :'  ;
                                GET  ;
                                w_imp  ;
                                DEFAULT  ;
                                1  ;
                                PICTURE  ;
                                '@*RHTN Si;No'
                              READ
                              IF LASTKEY() <>  ;
                                 27  ;
                                 .AND.  ;
                                 w_imp =  ;
                                 1
                                   DO imprime
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ELSE
          sw_algo = .F.
          ppas = .F.
          LOOP
     ENDIF
ENDDO
CLOSE DATABASES
DO sacawin
ON KEY LABEL F6
ON KEY
RETURN
*
FUNCTION valtab
PARAMETER codigo, opc
SELECT ge_tab0
DO CASE
     CASE opc = 1
          SEEK 'MONE' + codigo
     CASE opc = 2
          SEEK 'TALL' + codigo
ENDCASE
IF  .NOT. FOUND()
     DO error WITH  ;
        ' *** C줰igo no Existe ***'
     RETURN .F.
ENDIF
IF opc = 1
     @ ROW(), COL() + 6 SAY  ;
       SUBSTR(tab_destab, 1, 20)
ELSE
     @ ROW(), COL() + 2 SAY  ;
       SUBSTR(tab_destab, 1, 20)
ENDIF
RETURN
*
PROCEDURE mues_por
@ 09, 14 SAY b2
@ 10, 14 SAY b3
@ 11, 14 SAY b4
RETURN
*
PROCEDURE porcen
PARAMETER conta
por = conta / nreg
@ 12, 20 FILL TO 15, 58 COLOR W+/ ;
  BG 
@ 12, 20 TO 15, 58 COLOR W+/BG 
@ 13, 23 SAY  ;
  'Porcentaje del Proceso : ' +  ;
  TRANSFORM((por * 100),  ;
  '999.99') + ' %' COLOR W+/BG 
por = ROUND((por * 100), 0)
IF MOD(por, 2) = 0
     @ 9, 14 SAY REPLICATE(b1,  ;
       por / 2) COLOR W+/N 
ENDIF
RETURN
*
FUNCTION pro_pre
CREATE CURSOR presu (num N (3, 0),  ;
       numpre C (8), numsol C (8),  ;
       numord C (8), fecemi D,  ;
       fecvig D, indori C (4),  ;
       tele1 N (8), tele2 N (8),  ;
       descli C (30), codpro C  ;
       (14), despro C (20), canti  ;
       N (5), preant N (9, 2),  ;
       totant N (9, 2), prenue N  ;
       (9, 2), totnue N (9, 2),  ;
       dife N (9, 2))
CREATE CURSOR tabpre (item C (1),  ;
       numpre C (8), numsol C (8),  ;
       numord C (8), fecemi D,  ;
       fecvig D, totant N (9, 2),  ;
       totnue N (9, 2))
c = 0
creg = 0
SELECT st_ispre
SET ORDER TO pre_infenu
COUNT FOR ALLTRIM(indest) = 'V'  ;
      .AND. fecven < DATE() TO  ;
      nreg
IF nreg = 0
     DO error WITH  ;
        '*** No hay informaci줻 para procesar ***'
     RETURN .T.
ENDIF
GOTO TOP
SEEK 'V'
SCAN WHILE ALLTRIM(indest) = 'V'  ;
     .AND.  .NOT. EOF()
     IF fecven < DATE()
          w_codcli = VAL(codent)
          c = c + 1
          DO porcen WITH c
          SELECT st_iorep
          SEEK st_ispre.numord
          IF FOUND() .AND.  ;
             (ALLTRIM(indest) =  ;
             'V' .OR.  ;
             ALLTRIM(indest) =  ;
             'P') .AND. (codtall >=  ;
             w_codta1 .AND.  ;
             codtall <=  ;
             w_codta2)
               FOR i = 1 TO 12
                    pro( i) =  ;
                       SPACE(14)
                    dex( i) =  ;
                       SPACE(20)
                    can( i) = 0
                    prea( i) = 0
                    pren( i) = 0
                    totp( i) = 0
                    totn( i) = 0
               ENDFOR
               SELECT st_idpre
               SEEK st_ispre.numdoc
               IF FOUND()
                    w_totant = 0
                    w_totnue = 0
                    i = 0
                    w_totpre = 0
                    SCAN WHILE  ;
                         st_ispre.numdoc =  ;
                         st_idpre.numdoc  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         i = i +  ;
                             1
                         pro( i) =  ;
                            st_idpre.codpro
                         can( i) =  ;
                            st_idpre.canpro
                         IF w_codmon =  ;
                            rge_monbas
                              prea(  ;
                               i) =  ;
                               ROUND(st_idpre.valpro *  ;
                               w_tipcam,  ;
                               2)
                              totp(  ;
                               i) =  ;
                               ROUND(prea(i) *  ;
                               can(i),  ;
                               2)
                              SELECT  ;
                               gc_dlp00
                              SEEK  ;
                               rge_lispre +  ;
                               st_idpre.codpro
                              IF FOUND()
                                   pren( i) = ROUND(ROUND(gc_dlp00.dlp_prsigv * (1 + w_facigv), 2) * w_tipcam, 2)
                                   totn( i) = ROUND(can(i) * pren(i), 2)
                              ENDIF
                         ELSE
                              prea(  ;
                               i) =  ;
                               st_idpre.valpro
                              totp(  ;
                               i) =  ;
                               st_idpre.totite
                              SELECT  ;
                               gc_dlp00
                              SEEK  ;
                               rge_lispre +  ;
                               st_idpre.codpro
                              IF FOUND()
                                   pren( i) = ROUND(gc_dlp00.dlp_prsigv * (1 + w_facigv), 2)
                                   totn( i) = ROUND(can(i) * pren(i), 2)
                              ENDIF
                         ENDIF
                         w_totant =  ;
                          w_totant +  ;
                          totp(i)
                         w_totnue =  ;
                          w_totnue +  ;
                          totn(i)
                         SELECT gc_pro00
                         SEEK st_idpre.codpro
                         IF FOUND()
                              dex(  ;
                                 i) =  ;
                                 SUBSTR(pro_descri,  ;
                                 1,  ;
                                 20)
                         ENDIF
                         SELECT st_idpre
                         IF totp(i) <>  ;
                            totn(i)
                              DO rbloquea
                              IF w_codmon =  ;
                                 rge_monbas
                                   REPLACE valpro WITH ROUND(pren(i) / w_tipcam, 2)
                              ELSE
                                   REPLACE valpro WITH pren(i)
                              ENDIF
                              REPLACE  ;
                               totite  ;
                               WITH  ;
                               ROUND(valpro *  ;
                               can(i),  ;
                               2)
                              UNLOCK
                         ENDIF
                         w_totpre =  ;
                          w_totpre +  ;
                          totite
                    ENDSCAN
                    IF w_totant <>  ;
                       w_totnue
                         creg = creg +  ;
                                1
                         SELECT st_iclpr
                         SEEK 'C' +  ;
                              STR(w_codcli,  ;
                              11)
                         IF FOUND()
                              w_noment =  ;
                               noment
                              w_numte1 =  ;
                               numte1
                              w_numte2 =  ;
                               numte2
                         ENDIF
                         SELECT presu
                         FOR x =  ;
                             1 TO  ;
                             i
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               num  ;
                               WITH  ;
                               creg
                              REPLACE  ;
                               numpre  ;
                               WITH  ;
                               st_ispre.numdoc
                              REPLACE  ;
                               numsol  ;
                               WITH  ;
                               st_ispre.numsol
                              REPLACE  ;
                               numord  ;
                               WITH  ;
                               st_ispre.numord
                              REPLACE  ;
                               fecemi  ;
                               WITH  ;
                               st_ispre.fecemi
                              REPLACE  ;
                               fecvig  ;
                               WITH  ;
                               st_ispre.fecven
                              REPLACE  ;
                               indori  ;
                               WITH  ;
                               st_ispre.indori
                              REPLACE  ;
                               codpro  ;
                               WITH  ;
                               pro(x)
                              REPLACE  ;
                               despro  ;
                               WITH  ;
                               dex(x)
                              REPLACE  ;
                               canti  ;
                               WITH  ;
                               can(x)
                              REPLACE  ;
                               preant  ;
                               WITH  ;
                               prea(x)
                              REPLACE  ;
                               totant  ;
                               WITH  ;
                               totp(x)
                              REPLACE  ;
                               prenue  ;
                               WITH  ;
                               pren(x)
                              REPLACE  ;
                               totnue  ;
                               WITH  ;
                               totn(x)
                              REPLACE  ;
                               dife  ;
                               WITH  ;
                               totnue -  ;
                               totant
                              REPLACE  ;
                               tele1  ;
                               WITH  ;
                               w_numte1
                              REPLACE  ;
                               tele2  ;
                               WITH  ;
                               w_numte2
                              REPLACE  ;
                               descli  ;
                               WITH  ;
                               w_noment
                         ENDFOR
                         SELECT tabpre
                         APPEND BLANK
                         REPLACE item  ;
                                 WITH  ;
                                 SPACE(1)
                         REPLACE numpre  ;
                                 WITH  ;
                                 st_ispre.numdoc
                         REPLACE numsol  ;
                                 WITH  ;
                                 st_ispre.numsol
                         REPLACE numord  ;
                                 WITH  ;
                                 st_ispre.numord
                         REPLACE fecemi  ;
                                 WITH  ;
                                 st_ispre.fecemi
                         REPLACE fecvig  ;
                                 WITH  ;
                                 st_ispre.fecven
                         REPLACE totant  ;
                                 WITH  ;
                                 w_totant
                         REPLACE totnue  ;
                                 WITH  ;
                                 w_totnue
                         SELECT st_ispre
                         w_totrep =  ;
                          w_totpre
                         w_totdes =  ;
                          pordes
                         w_totnet =  ;
                          ROUND(w_totrep -  ;
                          (w_totrep *  ;
                          pordes /  ;
                          100),  ;
                          2)
                         w_totman =  ;
                          monman
                         w_totgrl =  ;
                          w_totnet +  ;
                          w_totman
                         w_totafe =  ;
                          ROUND(w_totgrl /  ;
                          (1 +  ;
                          w_facigv),  ;
                          2)
                         w_totigv =  ;
                          w_totgrl -  ;
                          w_totafe
                         DO rbloquea
                         REPLACE fecven  ;
                                 WITH  ;
                                 DATE() +  ;
                                 empre11
                         REPLACE monrep  ;
                                 WITH  ;
                                 w_totrep
                         REPLACE totigv  ;
                                 WITH  ;
                                 w_totigv
                         UNLOCK
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
     SELECT st_ispre
ENDSCAN
IF creg = 0
     DO error WITH  ;
        '*** No Hay Informaci줻 para Actualizar ***'
     RETURN .T.
ENDIF
SELECT tabpre
GOTO TOP
campo = 'item+" "+ numpre+" "+dtoc(fecemi)+" "+numsol+" "+numord+" "+str(totant,9,2)+" "+str(totnue,9,2)+ " "+ str(totnue-totant,9,2)'
mensaje = 'N� S/P.컴 FEC.EMI컴 N� S/S.컴 N� O/R.컴 TOT.ANT.컴TOT.NUE.컴 DIFER. '
DO esc_indica WITH 1, 'AYU',  ;
   'TOD', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'ESP', 'IMP', 'ESC'
ON KEY LABEL F8 DO MARCA
ON KEY LABEL F7 DO IMPRES
@ 11, 1 CLEAR TO 14, 78
define popup ayu0 from 0,0 to 10,74 promp;
field &campo title mensaje COLOR SCHEME;
8
ON SELECTION POPUP ayu0 do choice0
ACTIVATE POPUP ayu0 NOWAIT
FOR i = 1 TO 04
     MOVE POPUP ayu0 BY 1, 0
ENDFOR
ACTIVATE POPUP ayu0
DEACTIVATE POPUP ayu0
IF LASTKEY() = 27
     ppas = .T.
     RETURN
ENDIF
SELECT tabpre
GOTO TOP
DO WHILE  .NOT. EOF()
     IF item = '�'
          sw_algo = .T.
     ENDIF
     SKIP
ENDDO
RETURN
*
PROCEDURE choice0
IF LASTKEY() == 32
     IF item = '�'
          REPLACE item WITH  ;
                  SPACE(1)
     ELSE
          REPLACE item WITH '�'
     ENDIF
     KEYBOARD '{DNARROW}'
ENDIF
IF LASTKEY() == 13
     DEACTIVATE POPUP ayu0
ENDIF
RETURN
*
PROCEDURE marca
SELECT tabpre
REPLACE item WITH '�' ALL
RETURN
*
PROCEDURE impres
SELECT presu
GOTO TOP
DO mensa WITH  ;
   '***  I m p r i m i e n d o  ***',  ;
   'COLO'
SET CONSOLE OFF
SET PRINTER ON
??? CHR(27) + CHR(15)
REPORT FORMAT PORM0204 TO PRINTER  ;
       NOCONSOLE
??? CHR(18)
SET PRINTER OFF
SET CONSOLE ON
DO mensa WITH  ;
   '***  I m p r i m i e n d o  ***',  ;
   'SACA'
SELECT tabpre
RETURN
*
PROCEDURE imprime
w_print = PRINTSTATUS()
DO WHILE  .NOT. w_print
     DO error WITH  ;
        '** Error en Impresora. Continua ? (S/N) '
     IF CHR(LASTKEY()) == 'N'  ;
        .OR. CHR(LASTKEY()) ==  ;
        'n'
          EXIT
     ENDIF
     w_print = PRINTSTATUS()
ENDDO
w_print = PRINTSTATUS()
IF w_print
     SET CONSOLE OFF
     SET DEVICE TO PRINTER
     SET PRINTER ON
     SET PRINTER TO LPT1
     @ PROW(), PCOL() SAY CHR(15)
     @ PROW(), PCOL() SAY CHR(27) +  ;
       'C' + CHR(33)
     DO sol_presu
     EJECT
     SET PRINTER TO
     SET PRINTER OFF
     SET DEVICE TO SCREEN
     SET CONSOLE ON
ENDIF
RETURN
*
PROCEDURE sol_presu
SELECT tabpre
GOTO TOP
SCAN WHILE  .NOT. EOF()
     IF item = '�'
          = llenapre()
          tit_tit5 = 'P R E S U P U E S T O'
          tit_subr = '袴袴袴袴袴袴袴袴袴袴�'
          tit_tit6 = 'N� :'
          tit_nrop = VAL(w_numpre)
          tit_nomb = w_noment
          tit_dire = w_nomcal
          tit_distri = w_desdis
          tit_feccom = w_fecven
          tit_tel1 = STR(w_numte1,  ;
                     8)
          tit_tel2 = STR(w_numte2,  ;
                     8)
          tit_codi = STR(w_codcli,  ;
                     11)
          w_codi = STR(w_codcli,  ;
                   11)
          tit_mone = ALLTRIM(w_nommon)
          tit_tit3 = 'N쬢:'
          tit_ord = 'N쬜:'
          tit_soli = w_numsol
          tit_orde = w_numord
          tit_prod = w_nommod
          tit_cpro = w_codmod
          tit_tecn = STR(w_tecnic)
          tit_tit4 = 'OBS.:'
          w_fecha = DTOC(w_fecemi)
          tit_fech = SUBSTR(w_fecha,  ;
                     1, 2) +  ;
                     SPACE(3) +  ;
                     SUBSTR(w_fecha,  ;
                     4, 2) +  ;
                     SPACE(3) +  ;
                     SUBSTR(w_fecha,  ;
                     7, 2)
          ??? CHR(15)
          DO pormpre
          ??? CHR(18)
     ENDIF
ENDSCAN
RETURN
*
PROCEDURE llenapre
FOR i = 1 TO 12
     pro( i) = SPACE(14)
     dex( i) = SPACE(20)
     can( i) = 0
     pre( i) = 0
     pres( i) = 0
     tot( i) = 0
     tots( i) = 0
     IF i <= 8
          w_obspre( i) =  ;
                  SPACE(38)
     ENDIF
ENDFOR
SELECT st_ispre
SET ORDER TO codigo
SEEK tabpre.numpre
IF FOUND()
     w_codcli = VAL(codent)
     w_indori = indori
     w_codmar = codmar
     w_codmod = codmod
     w_numser = numser
     w_numpre = numdoc
     w_emisor = codemi
     w_numsol = VAL(numsol)
     w_numord = VAL(numord)
     w_fecemi = fecemi
     w_fecven = fecven
     w_horemi = horemi
     w_tecnic = VAL(codtec)
     w_numero = VAL(numsol)
     w_totrep = monrep
     w_totdes = pordes
     w_totnet = ROUND(monrep -  ;
                (monrep * pordes /  ;
                100), 2)
     w_totman = monman
     w_totgrl = w_totnet +  ;
                w_totman
     w_totafe = ROUND(w_totgrl /  ;
                (1 + w_facigv),  ;
                2)
     w_totigv = w_totgrl -  ;
                w_totafe
     w_totdes = pordes
     w_fecvta = CTOD(SPACE(8))
     w_docgar = SPACE(15)
     FOR i = 1 TO 8
          w_obspre( i) =  ;
                  SUBSTR(observ,  ;
                  1 + ((i - 1) *  ;
                  38), 38)
          w_obspre( i) =  ;
                  w_obspre(i) +  ;
                  SPACE(38 -  ;
                  LEN(w_obspre(i)))
     ENDFOR
     o = 1
     FOR i = 1 TO 4
          imp_obser( i) =  ;
                   w_obspre(o) +  ;
                   w_obspre(o +  ;
                   1)
          o = o + 2
     ENDFOR
     IF w_indori = 'GARA' .OR.  ;
        w_indori = 'GREC'
          SELECT 20
          USE SHARED st_iseri  ;
              ORDER ser_codmar
          SEEK w_codmar +  ;
               w_codmod +  ;
               w_numser
          IF FOUND()
               w_fecvta = fecvta
               w_docgar = docgar
          ELSE
               w_fecvta = CTOD(SPACE(8))
               w_docgar = SPACE(15)
          ENDIF
     ENDIF
     w_doctia = DTOC(w_fecvta) +  ;
                ' ' +  ;
                ALLTRIM(w_docgar)
     SELECT st_iclpr
     SEEK 'C' + STR(w_codcli, 11)
     w_noment = noment
     w_nomcal = nomcal
     w_nomdis = nomdis
     w_nomciu = nomciu
     w_numte1 = numte1
     w_numte2 = numte2
     SELECT ge_tab0
     SEEK 'MARC' + w_codmar
     IF FOUND()
          w_nommar = SUBSTR(tab_destab,  ;
                     1, 30)
     ENDIF
     SEEK 'INGA' + w_indori
     IF FOUND()
          w_destia = tab_destab
     ENDIF
     SEEK 'MONE' + w_codmon
     IF FOUND()
          w_nommon = SUBSTR(tab_destab,  ;
                     1, 30)
     ENDIF
     w_aux = w_codmar + w_codmod
     SELECT 20
     USE SHARED st_imode ORDER  ;
         CODIGO
     SEEK '&w_aux'
     IF FOUND()
          w_nommod = SUBSTR(nommod,  ;
                     1, 30)
     ENDIF
     w_numaux = tabpre.numpre
     SELECT st_idpre
     SEEK w_numaux
     i = 1
     s_total = 0
     SCAN WHILE numdoc ==  ;
          w_numaux
          w_aux = codpro
          pro( i) = codpro
          can( i) = canpro
          pre( i) = valpro
          pres( i) = ROUND(valpro *  ;
              w_tipcam, 2)
          tot( i) = totite
          tots( i) =  ;
              ROUND(pres(i) *  ;
              can(i), 2)
          s_total = s_total +  ;
                    tots(i)
          SELECT gc_pro00
          SEEK '&w_aux'
          IF FOUND()
               dex( i) =  ;
                  SUBSTR(pro_descri,  ;
                  1, 30)
          ELSE
               dex( i) =  ;
                  SPACE(30)
          ENDIF
          i = i + 1
          SELECT st_idpre
     ENDSCAN
     s_totrep = s_total
     s_totdes = pordes
     s_totnet = ROUND(s_totrep -  ;
                (s_totrep *  ;
                pordes / 100),  ;
                2)
     s_totman = ROUND(w_totman *  ;
                w_tipcam, 2)
     s_totgrl = s_totnet +  ;
                s_totman
     s_totafe = ROUND(s_totgrl /  ;
                (1 + w_facigv),  ;
                2)
     s_totigv = s_totgrl -  ;
                s_totafe
     w_aux = 'EMIS' + w_emisor
     SELECT ge_tab0
     SEEK '&w_aux'
     IF FOUND()
          w_nomemi = SUBSTR(tab_destab,  ;
                     1, 30)
     ELSE
          w_nomemi = SPACE(30)
     ENDIF
     w_aux = 'DIST' + w_nomdis
     SEEK '&w_aux'
     IF FOUND()
          w_desdis = SUBSTR(tab_destab,  ;
                     1, 30)
     ELSE
          w_desdis = SPACE(30)
     ENDIF
     w_aux = 'PROV' + w_nomciu
     SEEK '&w_aux'
     IF FOUND()
          w_desciu = SUBSTR(tab_destab,  ;
                     1, 30)
     ELSE
          w_desciu = SPACE(30)
     ENDIF
     w_aux = STR(w_tecnic, 9)
     SELECT 20
     USE SHARED st_itecn ORDER  ;
         codigo
     seek  '&w_aux'
     IF FOUND()
          w_nomtec = noment
     ENDIF
ENDIF
RETURN
*
PROCEDURE ayuact
IF VARREAD() = 'W_CODMON'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'MONE'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE MONEDAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() = 'W_CODTA1' .OR.  ;
   VARREAD() = 'W_CODTA2'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'TALL'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE TALLER'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
ind_prg = '<PORM0202>'
DEFINE WINDOW presu FROM 08, 18  ;
       TO 14, 58
DEFINE WINDOW pedcop FROM 42, 10  ;
       TO 42, 66 NONE
wrk_progra = PROGRAM()
CLOSE DATABASES
SELECT 1
USE SHARED st_isrep ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER codigo
SELECT 3
USE SHARED st_ispre ORDER codigo
SELECT 4
USE SHARED st_idpre ORDER codigo
SELECT 5
USE SHARED st_itecn ORDER codigo
SELECT 7
USE SHARED ge_tab0 ORDER codigo
SELECT 8
USE SHARED gc_alm00 ORDER codigo
SELECT 9
USE SHARED gc_pro00 ORDER codigo
DO crea_win
DO esc_modo WITH 'I'
DO esc_indica WITH 1, 'AYU',  ;
   'BUS', 'BBB', 'INT'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
ON KEY LABEL F6 do ayuda11
ON KEY LABEL F10 DO FCINCO
SAVE SCREEN TO w_panta
STORE 0 TO w_var, w_numord,  ;
      w_numsol, ncopia
STORE DATE() TO w_fecvta
STORE SPACE(15) TO w_docgar
SELECT 20
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
w_tipcam = ootc2(DATE(), ;
           rge_monbas,'DOL','2')
IF w_tipcam = -1
     DO error WITH  ;
        '*** No Existe Tipo de Cambio de esta Fecha ***'
     ON KEY
     CLOSE DATABASES
     DO sacawin
     RETURN
ENDIF
w_facigv = facigv()
IF w_facigv = 0
     DO error WITH  ;
        '***Tipo de Impuesto no existe en Tablas ***'
     CLOSE DATABASES
     ON KEY
     DO sacawin
     RETURN
ENDIF
IF config_prg = 1
     titu1 = 'CREACION'
     titu2 = 'SOLICITUD DE PRESUPUESTO'
     @ 02, 1 SAY DATE()
     RESTORE SCREEN FROM w_panta
     @ 06, 18 CLEAR TO 12, 55
     @ 06, 18 TO 12, 55
     DO saycenter WITH 1, titu1
     DO saycenter WITH 2, titu2
     @ 08, 20 SAY 'ORIGEN :'
     @ 08, 30 SAY  ;
       'Orden de Reparaci¢n'
     @ 10, 20 SAY 'N£mero :'
     w_var = 1
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DO sacawin
          RETURN
     ENDIF
     ncopia = 2
     w_numero = 0
     @ 10, 30 GET w_numero  ;
       PICTURE '99999999' VALID  ;
       solvi2(w_numero,w_var)
     SET CURSOR ON
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DO sacawin
          RETURN
     ENDIF
     w_numord = w_numero
ENDIF
ACTIVATE WINDOW trabajo
SET DISPLAY TO VGA50
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 42, 76
ZOOM WINDOW indicar NORM FROM 45,  ;
     0 TO 48, 76
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
w_emisor = SPACE(4)
w_nomemi = SPACE(24)
ppal = .T.
DO WHILE ppal
     STORE 0 TO w_monto2, w_monto,  ;
           w_totman, w_numpre,  ;
           w_pagsol, w_pagdol,  ;
           w_abonos, w_totnet,  ;
           w_totigv
     STORE 0 TO w_totpor, w_subto,  ;
           w_flete, w_emba,  ;
           w_totiva, w_totgrl,  ;
           w_total, w_subto2,  ;
           w_totrep, w_totdes
     STORE 0 TO s_totpor, s_subto,  ;
           s_flete, s_emba,  ;
           s_totiva, s_totgrl,  ;
           s_total, s_subto2,  ;
           s_totrep
     STORE 0 TO s_totnet,  ;
           s_totigv, s_totafe,  ;
           s_totman, s_totdes,  ;
           w_por, w_numstk,  ;
           w_codcli, w_auxpre,  ;
           w_stock
     STORE SPACE(4) TO w_codmar,  ;
           w_codstk, w_linea,  ;
           w_esta, w_codmon
     STORE SPACE(30) TO w_nommar,  ;
           w_destia, w_nommon,  ;
           w_nommod, w_doctia
     STORE SPACE(15) TO w_codmod
     STORE SPACE(20) TO w_descri
     w_nomtec = SPACE(40)
     lin = 40
     anc = 75
     des = 1
     com = 1
     IF config_prg == 1 .OR.  ;
        config_prg == 2
          DO esc_indica WITH 1,  ;
             'AYU', 'SOL', 'BBB',  ;
             'INT'
          DO esc_indica WITH 2,  ;
             'RAC', 'BBB', 'IGN',  ;
             'ESC'
     ELSE
          DO esc_indica WITH 1,  ;
             'AYU', 'SOL', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'ANU', 'BBB', 'IGN',  ;
             'ESC'
     ENDIF
     DIMENSION solic( 50)
     STORE FOPEN('presupue.txt')  ;
           TO file_handl
     FOR i = 1 TO 50
          solic( i) =  ;
               FREAD(file_handl,  ;
               77)
     ENDFOR
     = FCLOSE(file_handl)
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
     IF config_prg <> 1
          w_numpre = 0
          @ 0, 67 GET w_numpre  ;
            PICTURE '99999999'  ;
            VALID  ;
            pptvig(w_numpre) WHEN  ;
            colocaf6()
          ON KEY LABEL F6 do ayuda11
          SET CURSOR ON
          READ
          SET CURSOR OFF
          IF LASTKEY() == 27
               ppal = .F.
               LOOP
          ENDIF
     ENDIF
     DO mensa2 WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     DIMENSION w_codsin( 15)
     DIMENSION w_acceso( 15)
     DIMENSION w_observ( 06)
     DIMENSION pro( 12), dex( 12),  ;
               uni( 12), can( 12),  ;
               pre( 12), pres(  ;
               12), dec( 12),  ;
               tot( 12), tots(  ;
               12), sto( 12)
     DIMENSION w_obspre( 08)
     FOR i = 1 TO 12
          pro( i) = SPACE(14)
          dex( i) = SPACE(30)
          uni( i) = SPACE(3)
          sto( i) = 0
          can( i) = 0
          pre( i) = 0
          pres( i) = 0
          dec( i) = 0
          tot( i) = 0
          tots( i) = 0
          IF i <= 8
               w_obspre( i) =  ;
                       SPACE(38)
          ENDIF
     ENDFOR
     IF w_var = 1
          SELECT st_iorep
          SEEK STR(w_numord, 8)
          w_tecnic = VAL(codtec)
     ELSE
          SELECT st_ispre
          SEEK STR(w_numpre, 8)
     ENDIF
     IF FOUND()
          w_codcli = VAL(codent)
          w_indori = indori
          w_codmar = codmar
          w_codmod = codmod
          w_numser = numser
          w_emisor = codemi
          w_numsol = VAL(numsol)
     ENDIF
     SELECT st_isrep
     SEEK STR(w_numsol, 8)
     IF FOUND()
          w_codstk = codstk
          w_numstk = VAL(numstk)
     ENDIF
     FOR i = 1 TO 15
          w_codsin = SPACE(35)
          w_acceso( i) =  ;
                  SUBSTR(desace,  ;
                  1 + ((i - 1) *  ;
                  35), 35)
          w_acceso( i) =  ;
                  w_acceso(i) +  ;
                  SPACE(35 -  ;
                  LEN(w_acceso(i)))
          IF i <= 6
               w_observ( i) =  ;
                       SUBSTR(observ,  ;
                       1 + ((i -  ;
                       1) * 45),  ;
                       45)
               w_observ( i) =  ;
                       w_observ(i) +  ;
                       SPACE(45 -  ;
                       LEN(w_observ(i)))
          ENDIF
     ENDFOR
     = lle_abon(STR(w_numsol, 8))
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
     SELECT 20
     USE SHARED st_iclpr ORDER  ;
         CODIGO
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
     w_aux = w_codmar + w_codmod
     SELECT 20
     USE SHARED st_imode ORDER  ;
         CODIGO
     SEEK '&w_aux'
     IF FOUND()
          w_linea = linea
          w_nommod = SUBSTR(nommod,  ;
                     1, 30)
          w_aux = codcla
     ENDIF
     SELECT 20
     USE SHARED st_sint ORDER  ;
         sin_lincod
     SELECT 21
     USE SHARED st_sicli ORDER  ;
         CODIGO
     w_numaux = STR(w_numsol, 8)
     SEEK '&w_numaux'
     i = 1
     IF FOUND()
          SCAN WHILE  .NOT. EOF()  ;
               .AND. numdoc ==  ;
               w_numaux
               w_aux2 = SUBSTR(codsin,  ;
                        2, 3)
               SELECT st_sint
               SEEK w_linea +  ;
                    w_aux2
               w_codsin( i) =  ;
                       SUBSTR(dessin,  ;
                       1, 35)
               i = i + 1
               SELECT st_sicli
          ENDSCAN
     ENDIF
     w_numaux = STR(w_numpre, 8)
     SELECT st_ispre
     seek '&w_numaux'
     IF  .NOT. FOUND()
          w_fecemi = DATE()
          w_fecven = DATE() +  ;
                     empre11
          w_horemi = TIME()
          w_totrep = 0
          w_totdes = 0
          w_totnet = 0
          w_totman = 0
          w_totafe = 0
          w_totigv = 0
          w_totgrl = 0
          w_codmon = 'SOL '
     ELSE
          w_fecemi = fecemi
          w_fecven = fecven
          w_horemi = horemi
          w_emisor = codemi
          w_indori = indori
          w_tecnic = VAL(codtec)
          w_numero = VAL(numsol)
          w_totrep = monrep
          w_codmon = codmon
          w_totdes = pordes
          s_totdes = pordes
          w_totnet = ROUND(monrep -  ;
                     (monrep *  ;
                     pordes /  ;
                     100), 2)
          w_totman = monman
          w_totgrl = w_totnet +  ;
                     w_totman
          w_totafe = ROUND(w_totgrl /  ;
                     (1 +  ;
                     w_facigv),  ;
                     2)
          w_totigv = w_totgrl -  ;
                     w_totafe
          s_totman = ROUND(w_totman *  ;
                     w_tipcam,  ;
                     2)
          FOR i = 1 TO 8
               w_obspre( i) =  ;
                       SUBSTR(observ,  ;
                       1 + ((i -  ;
                       1) * 38),  ;
                       38)
               w_obspre( i) =  ;
                       w_obspre(i) +  ;
                       SPACE(38 -  ;
                       LEN(w_obspre(i)))
          ENDFOR
          s_total = 0
          SELECT st_idpre
          SEEK '&w_numaux'
          i = 1
          SCAN WHILE numdoc ==  ;
               w_numaux
               w_aux = codpro
               pro( i) = codpro
               can( i) = canpro
               pre( i) = valpro
               pres( i) =  ;
                   ROUND(valpro *  ;
                   w_tipcam, 2)
               dec( i) = pordes
               tot( i) = totite
               tots( i) =  ;
                   ROUND(pres(i) *  ;
                   can(i), 2)
               s_total = s_total +  ;
                         tots(i)
               sto( i) = 0
               SELECT gc_alm00
               SEEK w_aux +  ;
                    rge_codalm
               IF FOUND()
                    sto( i) =  ;
                       alm_stkfis
               ELSE
                    sto( i) = 0
               ENDIF
               SELECT gc_pro00
               SEEK '&w_aux'
               IF FOUND()
                    dex( i) =  ;
                       SUBSTR(pro_descri,  ;
                       1, 30)
                    uni( i) =  ;
                       SUBSTR(pro_unimed,  ;
                       1, 03)
               ELSE
                    dex( i) =  ;
                       SPACE(30)
                    uni( i) =  ;
                       SPACE(3)
               ENDIF
               i = i + 1
               SELECT st_idpre
          ENDSCAN
          IF w_fecven < DATE()
               DO mensa2 WITH  ;
                  '** Actualizando Precios **',  ;
                  'COLO'
               w_fecven = DATE() +  ;
                          empre11
               SELECT 20
               s_total = 0
               USE SHARED  ;
                   gc_dlp00 ORDER  ;
                   codigo
               FOR i = 1 TO 12
                    IF pro(i) <>  ;
                       SPACE(14)
                         SEEK 'PUBL' +  ;
                              pro(i)
                         IF FOUND()
                              pre(  ;
                                 i) =  ;
                                 ROUND(dlp_prsigv *  ;
                                 (1 +  ;
                                 w_facigv),  ;
                                 2)
                              pres(  ;
                               i) =  ;
                               ROUND(pre(i) *  ;
                               w_tipcam,  ;
                               2)
                              tot(  ;
                                 i) =  ;
                                 ROUND(can(i) *  ;
                                 pre(i),  ;
                                 2)
                              tots(  ;
                               i) =  ;
                               ROUND(can(i) *  ;
                               pres(i),  ;
                               2)
                              s_total =  ;
                               s_total +  ;
                               tots(i)
                         ENDIF
                    ELSE
                         EXIT
                    ENDIF
               ENDFOR
          ENDIF
          s_totrep = s_total
          s_totnet = s_totrep -  ;
                     ROUND(s_totrep *  ;
                     (w_por /  ;
                     100), 2)
          s_totgrl = s_totnet +  ;
                     s_totman
          s_totafe = ROUND(s_totgrl /  ;
                     (1 +  ;
                     w_facigv),  ;
                     2)
          s_totigv = s_totgrl -  ;
                     s_totafe
     ENDIF
     IF w_codmon = 'DOL '
          w_abonos = w_pagdol
     ELSE
          w_abonos = w_pagsol
     ENDIF
     w_aux = 'EMIS' + w_emisor
     SELECT ge_tab0
     SEEK '&w_aux'
     IF FOUND()
          w_nomemi = SUBSTR(tab_destab,  ;
                     1, 30)
     ELSE
          w_nomemi = SPACE(30)
     ENDIF
     SEEK 'MONE' + w_codmon
     IF FOUND()
          w_nommon = SUBSTR(tab_destab,  ;
                     1, 30)
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
     SELECT st_itecn
     seek  '&w_aux'
     IF FOUND()
          w_nomtec = noment
     ENDIF
     DO mensa2 WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     DO col_bk1c
     DO col_bk2c
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
     IF config_prg <> 1
          @ 0, 50 SAY  ;
            'S/Presupuesto N§'
          @ 0, 67 SAY w_numpre  ;
            PICTURE '99999999'
     ELSE
          @ 0, 50 SAY SPACE(30)
     ENDIF
     @ 1, 50 SAY  ;
       'S/Servicio    N§'
     @ 2, 50 SAY  ;
       'O/Reparaci¢n  N§'
     @ 1, 67 SAY w_numsol PICTURE  ;
       '99999999'
     @ 2, 67 SAY w_numord PICTURE  ;
       '99999999'
     IF config_prg <> 3
          w_valida = .T.
          blok = 1
          ppal2 = .T.
          DO WHILE ppal2
               DO CASE
                    CASE blok ==  ;
                         1
                         IF config_prg <>  ;
                            1
                              @ 0,  ;
                                50  ;
                                SAY  ;
                                'S/Presupuesto N§'
                              @ 0,  ;
                                67  ;
                                SAY  ;
                                w_numpre  ;
                                PICTURE  ;
                                '99999999'
                         ELSE
                              @ 0,  ;
                                50  ;
                                SAY  ;
                                SPACE(30)
                         ENDIF
                         @ 01, 50  ;
                           SAY  ;
                           'S/Servicio    N§'
                         @ 02, 50  ;
                           SAY  ;
                           'O/Reparaci¢n  N§'
                         @ 04, 58  ;
                           SAY  ;
                           'TC: ' +  ;
                           TRANSFORM(w_tipcam,  ;
                           '9,999,999.99' ;
                           )
                         @ 01, 67  ;
                           SAY  ;
                           w_numsol  ;
                           PICTURE  ;
                           '99999999'
                         @ 02, 67  ;
                           SAY  ;
                           w_numord  ;
                           PICTURE  ;
                           '99999999'
                         @ 15, 64  ;
                           GET  ;
                           w_fecven  ;
                           VALID  ;
                           valfec(w_fecven)
                         @ 17, 19  ;
                           SAY  ;
                           w_emisor  ;
                           PICTURE  ;
                           '!!!!'
                         = valtab2('EMIS', ;
                           w_emisor, ;
                           24, ;
                           30)
                         @ 19, 19  ;
                           GET  ;
                           w_codmon  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           moneda(w_codmon)  ;
                           WHEN  ;
                           colocaf6()
                         @ 19, 60  ;
                           SAY  ;
                           w_abonos  ;
                           PICTURE  ;
                           '999,999.99'
                         @ 20, 19  ;
                           GET  ;
                           w_tecnic  ;
                           PICTURE  ;
                           '999999999'  ;
                           VALID  ;
                           tecnico(w_tecnic, ;
                           29)  ;
                           WHEN  ;
                           colocaf6()
                         ON KEY LABEL;
f8 do versol
                         SET CURSOR ON
                         READ
                         SET CURSOR OFF
                         DO CASE
                              CASE  ;
                               LASTKEY() =  ;
                               27  ;
                               .AND.  ;
                               config_prg =  ;
                               1
                                   ppal2 = .F.
                                   ppal = .F.
                                   LOOP
                              CASE  ;
                               LASTKEY() =  ;
                               27  ;
                               .AND.  ;
                               config_prg =  ;
                               2
                                   ppal2 = .F.
                                   LOOP
                         ENDCASE
                         ON KEY LABEL;
f8
                         = lle_pro()
                         w_por = w_totdes
                         @ 38, 12  ;
                           GET  ;
                           w_por  ;
                           PICTURE  ;
                           '999.99'  ;
                           VALID  ;
                           calculo(w_por, ;
                           1)
                         IF w_codmon =  ;
                            'DOL '
                              @ 38,  ;
                                30  ;
                                GET  ;
                                w_totman  ;
                                PICTURE  ;
                                '9999999.99'  ;
                                VALID  ;
                                calculo(w_totman, ;
                                2)  ;
                                WHEN  ;
                                SUBSTR(w_indori,  ;
                                2,  ;
                                1) <>  ;
                                'R'  ;
                                .AND.  ;
                                manobra(w_codmar, ;
                                w_codmod)
                         ELSE
                              @ 38,  ;
                                30  ;
                                GET  ;
                                s_totman  ;
                                PICTURE  ;
                                '9999999.99'  ;
                                VALID  ;
                                calculo(s_totman, ;
                                2)  ;
                                WHEN  ;
                                SUBSTR(w_indori,  ;
                                2,  ;
                                1) <>  ;
                                'R'  ;
                                .AND.  ;
                                manobra(w_codmar, ;
                                w_codmod)
                         ENDIF
                         ON KEY LABEL;
f8 do versol
                         SET CURSOR ON
                         READ
                         SET CURSOR OFF
                         w_totrep =  ;
                          w_total
                         s_totrep =  ;
                          s_total
                         w_totdes =  ;
                          w_por
                         DO CASE
                              CASE  ;
                               LASTKEY() ==  ;
                               27
                                   LOOP
                              CASE  ;
                               LASTKEY() ==  ;
                               13  ;
                               .OR.  ;
                               LASTKEY() ==  ;
                               24  ;
                               .OR.  ;
                               LASTKEY() ==  ;
                               3  ;
                               .OR.  ;
                               LASTKEY() ==  ;
                               9  ;
                               .OR.  ;
                               (LASTKEY() >  ;
                               31  ;
                               .AND.  ;
                               LASTKEY() <  ;
                               123)
                                   DO col_bk1c
                                   blok = 2
                                   DO mueve3 WITH REPLICATE(CHR(24), 10)
                         ENDCASE
                    CASE blok ==  ;
                         2
                         si = .F.
                         FOR i =  ;
                             1 TO  ;
                             08
                              @ 30 +  ;
                                i,  ;
                                1  ;
                                GET  ;
                                w_obspre(  ;
                                i)  ;
                                PICTURE  ;
                                '@!'
                         ENDFOR
                         ON KEY LABEL;
F8 do versol
                         SET CURSOR ON
                         READ
                         SET CURSOR OFF
                         o = 1
                         DIMENSION  ;
                          imp_obser(  ;
                          4)
                         FOR i =  ;
                             1 TO  ;
                             4
                              imp_obser(  ;
                               i) =  ;
                               w_obspre(o) +  ;
                               w_obspre(o +  ;
                               1)
                              o =  ;
                               o +  ;
                               2
                         ENDFOR
                         ON KEY LABEL;
F8
                         DO CASE
                              CASE  ;
                               LASTKEY() ==  ;
                               27
                                   DO mueve3 WITH REPLICATE(CHR(05), 10)
                                   blok = 1
                                   LOOP
                              CASE  ;
                               LASTKEY() ==  ;
                               13  ;
                               .OR.  ;
                               LASTKEY() ==  ;
                               24  ;
                               .OR.  ;
                               LASTKEY() ==  ;
                               3  ;
                               .OR.  ;
                               LASTKEY() ==  ;
                               9  ;
                               .OR.  ;
                               (LASTKEY() >  ;
                               31  ;
                               .AND.  ;
                               LASTKEY() <  ;
                               123)
                                   DO col_bk2c
                                   ppal2 = .F.
                              CASE  ;
                               LASTKEY() ==  ;
                               05  ;
                               .OR.  ;
                               LASTKEY() ==  ;
                               18  ;
                               .OR.  ;
                               LASTKEY() ==  ;
                               127  ;
                               .OR.  ;
                               LASTKEY() ==  ;
                               15
                                   DO col_bk2c
                                   blok = 1
                                   DO mueve3 WITH REPLICATE(CHR(05), 10)
                         ENDCASE
               ENDCASE
          ENDDO
          IF LASTKEY() <> 27
               DO esc_indica WITH  ;
                  1, 'AYU', 'BBB',  ;
                  'IMP', 'BBB'
               DO esc_indica WITH  ;
                  2, 'GRA', 'BBB',  ;
                  'IGN', 'ESC'
               w_key = 0
               DO WHILE w_key<>-1  ;
                  .AND. w_key<>-6  ;
                  .AND. w_key<>-9  ;
                  .AND. w_key<> ;
                  27
                    w_key = INKEY(0)
               ENDDO
               IF w_key == 27
                    w_salir = .F.
                    ppal = .F.
               ENDIF
               IF w_key == -6  ;
                  .AND.  ;
                  config_prg = 2
                    ACTIVATE WINDOW  ;
                             pedcop
                    CLEAR
                    @ 0, 15 SAY  ;
                      'Nro. de Copias :'  ;
                      GET ncopia  ;
                      PICTURE  ;
                      '99' VALID  ;
                      ncopia <= 2  ;
                      .AND.  ;
                      ncopia >=  ;
                      1
                    SET CURSOR ON
                    READ
                    SET CURSOR OFF
                    IF LASTKEY() ==  ;
                       27
                         ncopia =  ;
                          1
                    ENDIF
                    DEACTIVATE WINDOW  ;
                               pedcop
               ENDIF
               IF w_key == -1  ;
                  .OR. w_key == - ;
                  6
                    ppal = .F.
                    w_numaux = STR(w_numpre,  ;
                               8)
                    IF config_prg ==  ;
                       1
                         SELECT 20
                         USE SHARED  ;
                             st_iparg
                         DO rbloquea
                         REPLACE sys_numpre  ;
                                 WITH  ;
                                 (sys_numpre +  ;
                                 1)
                         w_numpre =  ;
                          w_numord
                         UNLOCK
                         SELECT 20
                         USE SHARED  ;
                             st_mvord
                         APPEND BLANK
                         DO rbloquea
                         REPLACE dia  ;
                                 WITH  ;
                                 DATE(),  ;
                                 hora  ;
                                 WITH  ;
                                 TIME()
                         REPLACE orden  ;
                                 WITH  ;
                                 STR(w_numord,  ;
                                 8),  ;
                                 tecnico  ;
                                 WITH  ;
                                 STR(w_tecnic,  ;
                                 9)
                         REPLACE estado  ;
                                 WITH  ;
                                 '003 ',  ;
                                 destado  ;
                                 WITH  ;
                                 ootab('ESOR', ;
                                 estado)
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
                         SELECT st_iorep
                         SEEK STR(w_numord,  ;
                              8)
                         IF FOUND()
                              DO rbloquea
                              REPLACE  ;
                               numpre  ;
                               WITH  ;
                               STR(w_numpre,  ;
                               8)
                              REPLACE  ;
                               auxest  ;
                               WITH  ;
                               '003 '
                              REPLACE  ;
                               fecest  ;
                               WITH  ;
                               DATE()
                              w_aux =  ;
                               ''
                              FOR  ;
                               i =  ;
                               1  ;
                               TO  ;
                               8
                                   IF LEN(TRIM(w_obspre(i))) <> 0
                                        w_aux = w_aux + w_obspre(i)
                                   ENDIF
                              ENDFOR
                              REPLACE  ;
                               observ  ;
                               WITH  ;
                               w_aux
                              REPLACE  ;
                               user  ;
                               WITH  ;
                               users
                              REPLACE  ;
                               time  ;
                               WITH  ;
                               TIME()
                              REPLACE  ;
                               date  ;
                               WITH  ;
                               DATE()
                              UNLOCK
                         ENDIF
                         SELECT 20
                         USE SHARED  ;
                             st_iredo
                         APPEND BLANK
                         DO rbloquea
                         REPLACE indodo  ;
                                 WITH  ;
                                 'SSE ',  ;
                                 numodo  ;
                                 WITH  ;
                                 STR(w_numsol,  ;
                                 8)
                         REPLACE indddo  ;
                                 WITH  ;
                                 'PRE ',  ;
                                 numddo  ;
                                 WITH  ;
                                 STR(w_numpre,  ;
                                 8)
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
                         APPEND BLANK
                         DO rbloquea
                         REPLACE indodo  ;
                                 WITH  ;
                                 'ORD ',  ;
                                 numodo  ;
                                 WITH  ;
                                 STR(w_numord,  ;
                                 8)
                         REPLACE indddo  ;
                                 WITH  ;
                                 'PRE ',  ;
                                 numddo  ;
                                 WITH  ;
                                 STR(w_numpre,  ;
                                 8)
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
                         w_numauxs =  ;
                          STR(w_numsol,  ;
                          8)
                         SELECT st_isrep
                         seek '&w_numauxs'
                         DO rbloquea
                         REPLACE numpre  ;
                                 WITH  ;
                                 STR(w_numpre,  ;
                                 8)
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
                         DO mensa2  ;
                            WITH  ;
                            'Creaci¢n Presupuesto N§ '+ ;
                            STR(w_numpre,  ;
                            8),  ;
                            'COLO'
                    ENDIF
                    SELECT st_ispre
                    SEEK w_numaux
                    IF  .NOT.  ;
                        FOUND()
                         APPEND BLANK
                    ENDIF
                    DO rbloquea
                    REPLACE numdoc  ;
                            WITH  ;
                            STR(w_numpre,  ;
                            8),  ;
                            fecemi  ;
                            WITH  ;
                            w_fecemi,  ;
                            fecven  ;
                            WITH  ;
                            w_fecven,  ;
                            horemi  ;
                            WITH  ;
                            w_horemi
                    REPLACE codemi  ;
                            WITH  ;
                            w_emisor,  ;
                            codent  ;
                            WITH  ;
                            STR(w_codcli,  ;
                            11),  ;
                            indori  ;
                            WITH  ;
                            w_indori,  ;
                            codmon  ;
                            WITH  ;
                            w_codmon
                    REPLACE indest  ;
                            WITH  ;
                            'V   ',  ;
                            codmar  ;
                            WITH  ;
                            w_codmar,  ;
                            codmod  ;
                            WITH  ;
                            w_codmod,  ;
                            numser  ;
                            WITH  ;
                            w_numser
                    REPLACE codtec  ;
                            WITH  ;
                            STR(w_tecnic,  ;
                            9),  ;
                            monrep  ;
                            WITH  ;
                            w_totrep
                    REPLACE user  ;
                            WITH  ;
                            users
                    REPLACE time  ;
                            WITH  ;
                            TIME()
                    REPLACE date  ;
                            WITH  ;
                            DATE()
                    IF w_var = 1
                         REPLACE numsol  ;
                                 WITH  ;
                                 STR(w_numsol,  ;
                                 8),  ;
                                 numord  ;
                                 WITH  ;
                                 STR(w_numord,  ;
                                 8)
                    ELSE
                         REPLACE numsol  ;
                                 WITH  ;
                                 STR(w_numsol,  ;
                                 8)
                    ENDIF
                    IF w_totdes >  ;
                       1
                         REPLACE pordes  ;
                                 WITH  ;
                                 w_totdes
                    ELSE
                         REPLACE pordes  ;
                                 WITH  ;
                                 0
                    ENDIF
                    REPLACE monman  ;
                            WITH  ;
                            w_totman,  ;
                            totigv  ;
                            WITH  ;
                            w_totigv
                    w_aux = ''
                    FOR i = 1 TO  ;
                        8
                         IF LEN(TRIM(w_obspre(i))) <>  ;
                            0
                              w_aux =  ;
                               w_aux +  ;
                               w_obspre(i)
                         ENDIF
                    ENDFOR
                    REPLACE observ  ;
                            WITH  ;
                            w_aux
                    UNLOCK
                    w_numaux = STR(w_numpre,  ;
                               8)
                    SELECT produc
                    GOTO TOP
                    SCAN WHILE   ;
                         .NOT.  ;
                         EOF()
                         SELECT st_idpre
                         SEEK w_numaux +  ;
                              STR(w_numord,  ;
                              8) +  ;
                              produc.codi
                         IF produc.inde =  ;
                            'A'
                              IF   ;
                               .NOT.  ;
                               FOUND()
                                   APPEND BLANK
                              ENDIF
                              DO rbloquea
                              REPLACE  ;
                               numdoc  ;
                               WITH  ;
                               w_numaux,  ;
                               codpro  ;
                               WITH  ;
                               produc.codi,  ;
                               canpro  ;
                               WITH  ;
                               produc.canti
                              REPLACE  ;
                               valpro  ;
                               WITH  ;
                               produc.predol,  ;
                               totite  ;
                               WITH  ;
                               produc.totdol,  ;
                               numord  ;
                               WITH  ;
                               STR(w_numord,  ;
                               8)
                              REPLACE  ;
                               pordes  ;
                               WITH  ;
                               0
                              REPLACE  ;
                               user  ;
                               WITH  ;
                               users
                              REPLACE  ;
                               time  ;
                               WITH  ;
                               TIME()
                              REPLACE  ;
                               date  ;
                               WITH  ;
                               DATE()
                              UNLOCK
                         ELSE
                              IF FOUND()
                                   DELETE
                              ENDIF
                         ENDIF
                         SELECT produc
                    ENDSCAN
                    IF config_prg ==  ;
                       1
                         DO mensa2  ;
                            WITH  ;
                            'Creaci¢n Presupuesto N§ '+ ;
                            STR(w_numero,  ;
                            8),  ;
                            'SACA'
                    ENDIF
               ENDIF
               IF w_key == -6
                    ppal = .F.
                    w_print = PRINTSTATUS()
                    DO WHILE   ;
                       .NOT.  ;
                       w_print
                         DO error2  ;
                            WITH  ;
                            '** Error en Impresora. Continua ? (S/N) '
                         IF CHR(LASTKEY()) ==  ;
                            'N'  ;
                            .OR.  ;
                            CHR(LASTKEY()) ==  ;
                            'n'
                              EXIT
                         ENDIF
                         w_print =  ;
                          PRINTSTATUS()
                    ENDDO
                    w_print = PRINTSTATUS()
                    IF w_print
                         SET CONSOLE OFF
                         SET DEVICE TO;
PRINTER
                         SET PRINTER ON
                         SET PRINTER TO;
LPT1
                         @ PROW(),  ;
                           PCOL()  ;
                           SAY  ;
                           CHR(15)
                         @ PROW(),  ;
                           PCOL()  ;
                           SAY  ;
                           CHR(27) +  ;
                           'C' +  ;
                           CHR(33)
                         DO sol_presu
                         EJECT
                         SET PRINTER TO
                         SET PRINTER OFF
                         SET DEVICE TO;
SCREEN
                         SET CONSOLE ON
                    ENDIF
               ENDIF
               DO esc_indica WITH  ;
                  1, 'AYU', 'BBB',  ;
                  'BBB', 'INT'
               DO esc_indica WITH  ;
                  2, 'RAC', 'BBB',  ;
                  'IGN', 'ESC'
          ENDIF
     ELSE
          ON KEY LABEL F8 do versol
          ppal2 = .T.
          DO WHILE ppal2
               w_inkey = 0
               DO WHILE  .NOT.  ;
                  (STR(w_inkey,  ;
                  2)$ ;
                  ' 5,24,18, 3, 1, 6,27,-1,-9,-2,-4,-7' ;
                  )
                    w_inkey = INKEY(0)
               ENDDO
               DO CASE
                    CASE w_inkey ==  ;
                         27 .OR.  ;
                         w_inkey == - ;
                         9
                         IF w_inkey ==  ;
                            27
                              ppal =  ;
                               .F.
                         ENDIF
                         ppal2 = .F.
                         LOOP
                    CASE w_inkey == - ;
                         1
                         DEFINE WINDOW  ;
                                error  ;
                                FROM  ;
                                42,  ;
                                10  ;
                                TO  ;
                                42,  ;
                                66  ;
                                NONE
                         ACTIVATE  ;
                          WINDOW  ;
                          error
                         CLEAR
                         w_sinoes =  ;
                          'N'
                         @ 0, 15  ;
                           SAY  ;
                           'Confirma Anular ?  (S/N)'  ;
                           GET  ;
                           w_sinoes  ;
                           PICTURE  ;
                           '!'  ;
                           VALID  ;
                           w_sinoes $  ;
                           'SN'
                         SET CURSOR ON
                         READ
                         SET CURSOR OFF
                         IF LASTKEY() ==  ;
                            27
                              w_sinoes =  ;
                               'N'
                         ENDIF
                         DEACTIVATE  ;
                          WINDOW  ;
                          error
                         IF w_sinoes ==  ;
                            'S'
                              SELECT  ;
                               st_ispre
                              w_numaux =  ;
                               STR(w_numpre,  ;
                               8)
                              seek '&w_numaux'
                              w_orden =  ;
                               numord
                              DO rbloquea
                              REPLACE  ;
                               indest  ;
                               WITH  ;
                               'N   '
                              DELETE
                              UNLOCK
                              SELECT  ;
                               20
                              USE  ;
                               SHARED  ;
                               st_mvord  ;
                               ORDER  ;
                               eor_nroord
                              SEEK  ;
                               w_orden
                              SCAN  ;
                               WHILE  ;
                               orden =  ;
                               w_orden  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF()
                                   IF estado = '003 '
                                        DELETE
                                   ELSE
                                        w_esta = estado
                                   ENDIF
                              ENDSCAN
                              SELECT  ;
                               st_iorep
                              SEEK  ;
                               w_orden
                              DO rbloquea
                              REPLACE  ;
                               numpre  ;
                               WITH  ;
                               SPACE(8)
                              REPLACE  ;
                               auxest  ;
                               WITH  ;
                               w_esta
                              UNLOCK
                              ppal2 =  ;
                               .F.
                         ELSE
                              EXIT
                         ENDIF
                    CASE w_inkey == - ;
                         7
                         DO versol
                    OTHERWISE
                         DO mueve3  ;
                            WITH  ;
                            CHR(w_inkey)
                         IF w_inkey ==  ;
                            05  ;
                            .OR.  ;
                            w_inkey ==  ;
                            18  ;
                            .OR.  ;
                            w_inkey ==  ;
                            127  ;
                            .OR.  ;
                            w_inkey ==  ;
                            15
                              @ 0,  ;
                                50  ;
                                SAY  ;
                                'S/Presupuesto N§'
                              @ 0,  ;
                                67  ;
                                SAY  ;
                                w_numpre  ;
                                PICTURE  ;
                                '99999999'
                              @ 1,  ;
                                50  ;
                                SAY  ;
                                'S/Servicio    N§'
                              @ 2,  ;
                                50  ;
                                SAY  ;
                                'O/Reparaci¢n  N§'
                              @ 1,  ;
                                67  ;
                                SAY  ;
                                w_numsol  ;
                                PICTURE  ;
                                '99999999'
                              @ 2,  ;
                                67  ;
                                SAY  ;
                                w_numord  ;
                                PICTURE  ;
                                '99999999'
                         ENDIF
               ENDCASE
          ENDDO
     ENDIF
ENDDO
CLOSE DATABASES
ON KEY LABEL F6
ON KEY LABEL F10
ON KEY LABEL f8
ON KEY
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DO sacawin
@ 24, 69 SAY SPACE(15)
RETURN
*
PROCEDURE lle_pro
DO esc_indica WITH 1, 'AYU',  ;
   'MD4', 'ELI', 'INT'
DO esc_indica WITH 2, 'RAC',  ;
   'BUS', 'BBB', 'ESC'
DEFINE WINDOW produc FROM 23, 01  ;
       TO 38, 75
CREATE CURSOR produc (codi C (14),  ;
       descri C (30), inde C (1),  ;
       canti N (5), predol N (10,  ;
       2), presol N (10, 2),  ;
       stock N (5), totdol N (10,  ;
       2), totsol N (10, 2))
SELECT produc
FOR x = 1 TO 12
     IF pro(x) <> SPACE(14)
          APPEND BLANK
          REPLACE codi WITH  ;
                  pro(x), descri  ;
                  WITH dex(x)
          REPLACE inde WITH 'A'
          REPLACE canti WITH  ;
                  can(x), stock  ;
                  WITH sto(x)
          REPLACE predol WITH  ;
                  pre(x), totdol  ;
                  WITH tot(x)
          REPLACE presol WITH  ;
                  pres(x), totsol  ;
                  WITH tots(x)
     ENDIF
ENDFOR
COUNT TO n
IF n = 0
     DO crea_pro
ENDIF
GOTO TOP
ACTIVATE WINDOW produc
ON KEY LABEL f3 do crea_pro
ON KEY LABEL f4 do eli_pro
IF w_codmon = 'DOL '
     BROWSE FOR inde <> 'N'  ;
            FIELDS codi : 14 :R  ;
            :H = 'CODIGO  ',  ;
            descri :R : 19 :H =  ;
            '    DESCRIPCION    ',  ;
            canti : 5 :P =  ;
            '99999' :V =  ;
            pro2(canti) :F :H =  ;
            'CANTID.', predol :R  ;
            :P = '9,999,999.99'  ;
            :H = ' PRECIO ',  ;
            stock :R :P = '99999'  ;
            :H = 'STOCK', totdol  ;
            :R :P =  ;
            '9,999,999.99' :H =  ;
            ' TOTAL' FREEZE canti  ;
            IN produc NOLGRID
ELSE
     BROWSE FOR inde <> 'N'  ;
            FIELDS codi : 14 :R  ;
            :H = 'CODIGO  ',  ;
            descri :R : 19 :H =  ;
            '    DESCRIPCION    ',  ;
            canti : 5 :P =  ;
            '99999' :V =  ;
            pro2(canti) :F :H =  ;
            'CANTID.', presol :R  ;
            :P = '9,999,999.99'  ;
            :H = ' PRECIO ',  ;
            stock :R :P = '99999'  ;
            :H = 'STOCK', totsol  ;
            :R :P =  ;
            '9,999,999.99' :H =  ;
            ' TOTAL' FREEZE canti  ;
            IN produc NOLGRID
ENDIF
DEACTIVATE WINDOW produc
k = 0
w_total = 0
s_total = 0
SELECT produc
GOTO TOP
SCAN WHILE  .NOT. EOF()
     IF codi <> SPACE(14) .AND.  ;
        canti > 0 .AND. inde =  ;
        'A'
          k = k + 1
          pro( k) = codi
          dex( k) = descri
          sto( k) = stock
          can( k) = canti
          pre( k) = predol
          pres( k) = presol
          tot( k) = totdol
          tots( k) = totsol
          w_total = w_total +  ;
                    totdol
          s_total = s_total +  ;
                    totsol
     ENDIF
ENDSCAN
FOR i = k + 1 TO 12
     pro( i) = SPACE(14)
     dex( i) = SPACE(30)
     uni( i) = SPACE(3)
     sto( i) = 0
     can( i) = 0
     pre( i) = 0
     pres( i) = 0
     tot( i) = 0
     tots( i) = 0
ENDFOR
w_totrep = w_total
w_totnet = w_totrep -  ;
           ROUND(w_totrep *  ;
           (w_por / 100), 2)
w_totgrl = w_totnet + w_totman
w_totafe = ROUND(w_totgrl / (1 +  ;
           w_facigv), 2)
w_totigv = w_totgrl - w_totafe
s_totrep = s_total
s_totnet = s_totrep -  ;
           ROUND(s_totrep *  ;
           (w_por / 100), 2)
s_totgrl = s_totnet + s_totman
s_totafe = ROUND(s_totgrl / (1 +  ;
           w_facigv), 2)
s_totigv = s_totgrl - s_totafe
FOR i = 1 TO 12
     IF pro(i) <> SPACE(14)
          @ 23 + i, 01 SAY pro(i)
          @ 23 + i, 15 SAY  ;
            SUBSTR(dex(i), 1,  ;
            25)
          @ 23 + i, 37 SAY can(i)  ;
            PICTURE '99999'
          @ 23 + i, 43 SAY  ;
            IIF(w_codmon = 'DOL ',  ;
            pre(i), pres(i))  ;
            PICTURE  ;
            '9,999,999.99'
          @ 23 + i, 56 SAY sto(i)  ;
            PICTURE '99999'
          @ 23 + i, 62 SAY  ;
            IIF(w_codmon = 'DOL ',  ;
            tot(i), tots(i))  ;
            PICTURE  ;
            '9,999,999.99'
     ELSE
          @ 23 + i, 01 SAY  ;
            SPACE(73)
     ENDIF
ENDFOR
@ 38, 01 SAY IIF(w_codmon =  ;
  'DOL ', w_totrep, s_totrep)  ;
  PICTURE '9,999,999.99'
@ 38, 12 SAY w_totdes PICTURE  ;
  '999.99'
@ 38, 19 SAY IIF(w_codmon =  ;
  'DOL ', w_totnet, s_totnet)  ;
  PICTURE '999,999.99'
@ 38, 30 SAY IIF(w_codmon =  ;
  'DOL ', w_totman, s_totman)  ;
  PICTURE '999,999.99'
@ 38, 41 SAY IIF(w_codmon =  ;
  'DOL ', w_totafe, s_totafe)  ;
  PICTURE '999,999.99'
@ 38, 52 SAY IIF(w_codmon =  ;
  'DOL ', w_totigv, s_totigv)  ;
  PICTURE '999,999.99'
@ 38, 63 SAY IIF(w_codmon =  ;
  'DOL ', w_totgrl, s_totgrl)  ;
  PICTURE '9999,999.99'
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
IF fil = 12
     DO error2 WITH  ;
        '*** No se puede ingresar m s de doce ***'
     RETURN .T.
ENDIF
DEFINE WINDOW crea_p FROM 26 +  ;
       fil, 03 TO 28 + fil, 17  ;
       NONE
ACTIVATE WINDOW crea_p
codigo = SPACE(14)
ON KEY LABEL f6 do ayupro
@ 00, 00 GET codigo PICTURE '@!'  ;
  VALID val_pro(codigo)
SET CURSOR ON
READ
SET CURSOR OFF
IF LASTKEY() <> 27 .AND. codigo <>  ;
   SPACE(14)
     SELECT produc
     LOCATE FOR codi = codigo
     IF FOUND()
          DELETE
     ENDIF
     APPEND BLANK
     REPLACE codi WITH codigo
     REPLACE descri WITH w_descri
     REPLACE canti WITH 0
     REPLACE inde WITH 'A'
     REPLACE predol WITH w_auxpre
     REPLACE presol WITH  ;
             ROUND(w_auxpre *  ;
             w_tipcam, 2)
     REPLACE stock WITH w_stock
ENDIF
DEACTIVATE WINDOW crea_p
ACTIVATE WINDOW produc
SELECT produc
ON KEY LABEL f6
ON KEY LABEL f3 do crea_pro
ON KEY LABEL f4 do eli_pro
RETURN
*
FUNCTION val_pro
PARAMETER codigo
IF codigo = SPACE(14)
     RETURN .T.
ENDIF
SELECT gc_pro00
SET ORDER TO codigo
SEEK codigo
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** C¢digo de Producto No Existe **'
     RETURN .F.
ENDIF
w_descri = SUBSTR(pro_descri, 1,  ;
           20)
w_lis = IIF(w_indori == 'GARA'  ;
        .OR. w_indori == 'GREC',  ;
        empre4, empre5)
SELECT 20
USE SHARED gc_hlp00 ORDER codigo
SEEK w_lis
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** C¢digo de Lista de Precio No Existe **'
     RETURN .F.
ELSE
     IF DATE() > hlp_vighas
          DO error2 WITH  ;
             '** Lista De Precios '+ ;
             w_lis+ ;
             ' esta Vencida **'
          RETURN .F.
     ENDIF
ENDIF
w_clave = IIF(w_indori == 'GARA'  ;
          .OR. w_indori == 'GREC',  ;
          empre4, empre5) +  ;
          codigo
SELECT 20
USE SHARED gc_dlp00 ORDER codigo
SEEK w_clave
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** C¢digo en Lista Precio No Existe **'
     RETURN .F.
ENDIF
w_auxpre = ROUND(dlp_prsigv * (1 +  ;
           w_facigv), 2)
SELECT produc
LOCATE FOR codi = codigo
IF FOUND() .AND. inde = 'A'
     DO error2 WITH  ;
        '** Art¡culo Repetido en Ppto. **'
     RETURN .F.
ENDIF
w_clave = codigo + rge_codalm
SELECT gc_alm00
seek '&w_clave'
IF FOUND()
     w_stock = gc_alm00.alm_stkfis
ELSE
     w_stock = 0
ENDIF
RETURN .T.
*
PROCEDURE eli_pro
REPLACE inde WITH 'N'
DEACTIVATE WINDOW produc
ACTIVATE WINDOW produc
SELECT produc
RETURN
*
PROCEDURE ayupro
w_select = SELECT()
SELECT gc_pro00
SET ORDER TO 1
SELECT gc_pro00
w_selpro = SELECT()
w_campo = gc_pro00.pro_codpro
DO producxx WITH w_campo,  ;
   w_select, w_selpro
IF LASTKEY() <> 27
     KEYBOARD w_campo
     codigo = w_campo
ENDIF
ACTIVATE WINDOW crea_p
ON KEY LABEL f6 do ayupro
RETURN
*
PROCEDURE lle_abon
PARAMETER w_num
SELECT 20
USE SHARED gc_hve00 ORDER nrdore
STORE 0 TO w_pagsol, a, w_pagdol,  ;
      w_monto
SEEK w_num
SCAN WHILE  ;
     VAL(gc_hve00.hve_nrdore) =  ;
     VAL(w_num) .AND.  .NOT.  ;
     EOF()
     IF gc_hve00.hve_estdoc <>  ;
        'A'
          a = a + 1
          w_pagsol = w_pagsol +  ;
                     hve_solgen
          w_pagdol = w_pagdol +  ;
                     ROUND(hve_solgen /  ;
                     hve_tipcam,  ;
                     2)
     ENDIF
ENDSCAN
RETURN
*
FUNCTION moneda
PARAMETER w_codmon
SELECT ge_tab0
SEEK 'MONE' + w_codmon
IF  .NOT. FOUND()
     DO error2 WITH  ;
        ' *** C¢digo no existe ****'
     RETURN .F.
ELSE
     @ 19, 24 SAY  ;
       SUBSTR(tab_destab, 1, 20)
     w_nommon = SUBSTR(tab_destab,  ;
                1, 20)
     IF w_codmon = 'DOL '
          w_abonos = w_pagdol
     ELSE
          w_abonos = w_pagsol
     ENDIF
     @ 19, 60 SAY w_abonos  ;
       PICTURE '999,999.99'
ENDIF
FOR i = 1 TO 12
     IF pro(i) <> SPACE(14)
          pres( i) = ROUND(pre(i) *  ;
              w_tipcam, 2)
          tots( i) = pres(i) *  ;
              can(i)
          @ 23 + i, 43 SAY  ;
            IIF(w_codmon = 'DOL ',  ;
            pre(i), pres(i))  ;
            PICTURE  ;
            '9,999,999.99'
          @ 23 + i, 62 SAY  ;
            IIF(w_codmon = 'DOL ',  ;
            tot(i), tots(i))  ;
            PICTURE  ;
            '9,999,999.99'
     ENDIF
ENDFOR
@ 38, 01 SAY IIF(w_codmon =  ;
  'DOL ', w_totrep, s_totrep)  ;
  PICTURE '9999,999.99'
@ 38, 12 SAY w_totdes PICTURE  ;
  '999.99'
@ 38, 19 SAY IIF(w_codmon =  ;
  'DOL ', w_totnet, s_totnet)  ;
  PICTURE '999,999.99'
@ 38, 30 SAY IIF(w_codmon =  ;
  'DOL ', w_totman, s_totman)  ;
  PICTURE '999,999.99'
@ 38, 41 SAY IIF(w_codmon =  ;
  'DOL ', w_totafe, s_totafe)  ;
  PICTURE '999,999.99'
@ 38, 52 SAY IIF(w_codmon =  ;
  'DOL ', w_totigv, s_totigv)  ;
  PICTURE '999,999.99'
@ 38, 63 SAY IIF(w_codmon =  ;
  'DOL ', w_totgrl, s_totgrl)  ;
  PICTURE '9999,999.99'
RETURN
*
FUNCTION pptvig
PARAMETER cod
cod = STR(cod, 8)
IF LEN(TRIM(cod)) == 0
     DO error2 WITH  ;
        '** N§ Presupuesto debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
SELECT st_ispre
SEEK '&cod'
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** Error Presupuesto No Existe **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF SUBSTR(indest, 1, 1) == 'N'
     DO error2 WITH  ;
        '** Error Presupuesto esta Anulado **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF SUBSTR(indest, 1, 1) = 'C'
     DO error2 WITH  ;
        '** Presupuesto esta Cerrado **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF SUBSTR(indest, 1, 1) <> 'V'
     DO error2 WITH  ;
        '** Presupuesto no esta Vigente **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
w_numord = VAL(numord)
w_numsol = VAL(numsol)
SELECT st_iorep
SEEK STR(w_numord, 8)
IF FOUND()
     IF (SUBSTR(indest, 1, 1) =  ;
        'C' .OR. auxest > '018 ')  ;
        .AND. nivell <> 'A7'
          DO error2 WITH  ;
             '** Error Orden esta Cerrada **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
ENDIF
IF config_prg = 3 .AND. auxest >  ;
   '003 '
     DO error2 WITH  ;
        '*** No Puede Anular,la O/R tiene que estar Ppto.***'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
DO sacaf6
RETURN .T.
*
FUNCTION solvi2
PARAMETER cod, w_var
cod = STR(cod, 8)
IF LEN(TRIM(cod)) == 0
     DO error2 WITH  ;
        '** N§ Solicitud debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
SELECT st_iorep
SEEK '&cod'
IF  .NOT. FOUND()
     DO error WITH  ;
        '** Error Documento NO EXISTE. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF SUBSTR(indest, 1, 1) == 'N'
     DO error WITH  ;
        '** Error Solicitud esta Anulada. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF numpre <> SPACE(8)
     w_nuppto = numpre
     DO error WITH  ;
        '*** Documento tiene un Presupuesto Nro. '+ ;
        ALLTRIM(w_nuppto)+' ***'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
w_estado = auxest
IF w_var = 1
     IF codtall > '010 '
          IF w_estado > '015 '  ;
             .AND. w_estado <>  ;
             '016' .AND. w_estado <>  ;
             '017' .AND. w_estado <>  ;
             '031'
               DO error WITH  ;
                  '*** La O/R no puede encontrarse en estado > 015 ***'
               KEYBOARD '{CTRL+Y}'  ;
                        PLAIN
               RETURN .F.
          ENDIF
     ELSE
          IF w_estado > '006 '  ;
             .OR. w_estado =  ;
             '001 '
               DO error WITH  ;
                  '*** La O/R tiene que encontrarse en Mesa de Rep. ***'
               KEYBOARD '{CTRL+Y}'  ;
                        PLAIN
               RETURN .F.
          ENDIF
     ENDIF
ENDIF
w_numsol = VAL(numsol)
w_numord = VAL(numdoc)
DO sacaf6
RETURN .T.
*
FUNCTION tecnico
PARAMETER grupo, colu
SELECT st_itecn
codaux = STR(grupo, 9)
seek '&codaux'
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** C¢digo de T‚cnico NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
@ ROW(), colu SAY SUBSTR(noment,  ;
  1, 28)
w_valida = .T.
DO sacaf6
RETURN .T.
*
FUNCTION pro2
PARAMETER canti
IF canti <= 0 .OR. EMPTY(canti)
     DO error2 WITH  ;
        '** Cantidad no puede  ser cero **'
     RETURN .F.
ENDIF
SELECT produc
REPLACE totdol WITH ROUND(canti *  ;
        predol, 2)
REPLACE totsol WITH ROUND(canti *  ;
        presol, 2)
RETURN .T.
*
PROCEDURE manobra
PARAMETER w_codmar, w_codmod
IF config_prg = 1
     SELECT 20
     USE SHARED st_imode ORDER  ;
         codigo
     w_aux = w_codmar + w_codmod
     w_monto = 0
     SEEK '&w_aux'
     IF FOUND()
          w_codart = codcla
          USE SHARED st_mobra  ;
              ORDER codigo
          SEEK w_codmar +  ;
               w_codart
          IF FOUND()
               IF w_indori =  ;
                  'FGAR'
                    w_monto = mo_monmof
               ELSE
                    w_monto = mo_monmog
               ENDIF
          ENDIF
     ENDIF
     w_totman = ROUND(w_monto *  ;
                (1 + w_facigv),  ;
                2)
     s_totman = ROUND(w_totman *  ;
                w_tipcam, 2)
ENDIF
RETURN
*
PROCEDURE ayuda11
ON KEY LABEL F6
IF VARREAD() == 'W_NUMERO' .AND.  ;
   config_prg = 1
     SELECT st_iorep
     w_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)+" "+ALLTRIM(INDORI)'
     DO ayuda4 WITH campoa,  ;
        w_origen
     SELECT st_iorep
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() == 'W_NUMPRE' .AND.  ;
   config_prg <> 1
     SELECT st_ispre
     w_origen = 'PR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+numsol+" "+SUBSTR(NUMSER,1,12)+" "+numord+" "+codmod+" "+subst(indest,1,2)'
     DO ayuda9 WITH campoa,  ;
        w_origen
     SELECT st_ispre
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() == 'W_NUMSOL'
     SELECT st_isrep
     w_origen = 'SS'
     SET FILTER TO coddes = 'P';
.AND. SUBSTR(indest, 1, 1) = 'V'
     campoa = 'numdoc+"  "+dtoc(fecemi)+"  "+SUBSTR(NUMSER,1,12)+"  "+codent+"  "+SUBSTR(codmod,1,10)+"   "+subst(indest,1,2)'
     DO ayuda4 WITH campoa,  ;
        w_origen
     SELECT st_isrep
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() == 'W_EMISOR'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'EMIS'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA EMISOR'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() == 'W_CODMON'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'MONE'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA MONEDAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() == 'W_INDORI'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'INGA'
     GOTO TOP
     campo = 'tab_codtab+ " "+tab_destab'
     titulo = 'AYUDA DE TIPO DE ATENCION'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() == 'W_TECNIC'
     SELECT st_itecn
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE TECNICOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
     SET FILTER TO
ENDIF
ON KEY LABEL F6 do ayuda11
RETURN
*
FUNCTION valfec
PARAMETER fec
IF EMPTY(fec)
     RETURN .F.
ENDIF
IF LASTKEY() = 18 .OR. LASTKEY() =  ;
   3
     RETURN .F.
ENDIF
IF fec <= w_fecemi
     DO error2 WITH  ;
        '** No puede ser <= a la Fecha de Emisi¢n **'
     RETURN .F.
ENDIF
IF fec > w_fecemi + (empre7 * 3)
     DO error2 WITH  ;
        '** Fecha excede el tiempo de garant¡a**'
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE sol_presu
SELECT st_itecn
SEEK STR(w_tecnic, 9)
IF FOUND()
     w_nomtec = noment
ENDIF
tit_tit5 = 'P R E S U P U E S T O'
tit_subr = 'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
tit_tit6 = 'N§ :'
tit_nrop = w_numpre
tit_nomb = w_noment
tit_dire = w_nomcal
tit_distri = w_desdis
tit_feccom = w_fecven
tit_tel1 = STR(w_numte1, 8)
tit_tel2 = STR(w_numte2, 8)
tit_codi = STR(w_codcli, 11)
w_codi = STR(w_codcli, 11)
tit_mone = ALLTRIM(w_nommon)
tit_tit3 = 'N§S:'
tit_ord = 'N§O:'
tit_soli = w_numsol
tit_orde = w_numord
tit_prod = w_nommod
tit_cpro = w_codmod
tit_tecn = STR(w_tecnic)
tit_tit4 = 'OBS.:'
w_fecha = DTOC(w_fecemi)
tit_fech = SUBSTR(w_fecha, 1, 2) +  ;
           SPACE(3) +  ;
           SUBSTR(w_fecha, 4, 2) +  ;
           SPACE(3) +  ;
           SUBSTR(w_fecha, 7, 4)
??? CHR(15)
DO pormpre
??? CHR(18)
RETURN
*
FUNCTION calculo
PARAMETER impor, opc
IF impor < 0
     DO error2 WITH  ;
        '** Importe no puede ser menor de cero**'
     RETURN .F.
ENDIF
IF opc = 1
     IF impor > 100
          DO error2 WITH  ;
             '** Importe no puede ser mayor de 100%**'
          RETURN .F.
     ENDIF
ELSE
     IF impor = 0 .AND.  ;
        SUBSTR(w_indori, 2, 1) <>  ;
        'R' .AND. STR(w_tecnic,  ;
        9) <> '      500'
          DO error2 WITH  ;
             '** Falta importe de Mano de Obra con Igv**'
          RETURN .F.
     ENDIF
ENDIF
IF VARREAD() = 'W_POR'
     w_totpor = ROUND((w_totrep *  ;
                (w_por / 100)),  ;
                2)
     w_totnet = w_totrep -  ;
                w_totpor
     w_totgrl = w_totnet +  ;
                w_totman
     w_totafe = ROUND(w_totgrl /  ;
                (1 + w_facigv),  ;
                2)
     w_totigv = w_totgrl -  ;
                w_totafe
     s_totpor = ROUND((s_totrep *  ;
                (w_por / 100)),  ;
                2)
     s_totnet = s_totrep -  ;
                s_totpor
     s_totgrl = s_totnet +  ;
                s_totman
     s_totafe = ROUND(s_totgrl /  ;
                (1 + w_facigv),  ;
                2)
     s_totigv = s_totgrl -  ;
                s_totafe
     @ 38, 19 SAY IIF(w_codmon =  ;
       'DOL ', w_totnet,  ;
       s_totnet) PICTURE  ;
       '999,999.99'
     @ 38, 41 SAY IIF(w_codmon =  ;
       'DOL ', w_totafe,  ;
       s_totafe) PICTURE  ;
       '999,999.99'
     @ 38, 52 SAY IIF(w_codmon =  ;
       'DOL ', w_totigv,  ;
       s_totigv) PICTURE  ;
       '999,999.99'
     @ 38, 63 SAY IIF(w_codmon =  ;
       'DOL ', w_totgrl,  ;
       s_totgrl) PICTURE  ;
       '9999,999.99'
ENDIF
IF VARREAD() = 'W_TOTMAN' .OR.  ;
   VARREAD() = 'S_TOTMAN'
     IF VARREAD() = 'W_TOTMAN'
          s_totman = ROUND(w_totman *  ;
                     w_tipcam,  ;
                     2)
     ELSE
          w_totman = ROUND(s_totman /  ;
                     w_tipcam,  ;
                     2)
     ENDIF
     w_totgrl = w_totnet +  ;
                w_totman
     w_totafe = ROUND(w_totgrl /  ;
                (1 + w_facigv),  ;
                2)
     w_totigv = w_totgrl -  ;
                w_totafe
     s_totgrl = s_totnet +  ;
                s_totman
     s_totafe = ROUND(s_totgrl /  ;
                (1 + w_facigv),  ;
                2)
     s_totigv = s_totgrl -  ;
                s_totafe
     @ 38, 41 SAY IIF(w_codmon =  ;
       'DOL ', w_totafe,  ;
       s_totafe) PICTURE  ;
       '999,999.99'
     @ 38, 52 SAY IIF(w_codmon =  ;
       'DOL ', w_totigv,  ;
       s_totigv) PICTURE  ;
       '999,999.99'
     @ 38, 63 SAY IIF(w_codmon =  ;
       'DOL ', w_totgrl,  ;
       s_totgrl) PICTURE  ;
       '9999,999.99'
ENDIF
RETURN
*
PROCEDURE p_mensaje
PARAMETER mens
SET CURSOR OFF
l = LEN(mens)
DEFINE WINDOW mensj FROM 11, (80 -  ;
       l) / 2 - 4 TO 15, (80 + l) /  ;
       2 + 4
ACTIVATE WINDOW mensj
@ 1, (WCOLS('MENSJ') - l) / 2 SAY  ;
  mens COLOR GR+/N* 
= INKEY(1)
SET CURSOR ON
RETURN
*
PROCEDURE producxx
PARAMETER w_campo, w_selec,  ;
          w_selpro
ON KEY
w_order = ORDER()
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen
DEFINE WINDOW produ FROM 15, 01  ;
       TO 24, 78 IN screen
DEFINE POPUP prod FROM 16, 31  ;
       COLOR SCHEME 8
DEFINE BAR 1 OF prod PROMPT  ;
       '\<C¢digo '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripci¢n '
DEFINE BAR 3 OF prod PROMPT  ;
       '\<Nro. de Parte '
DEFINE BAR 4 OF prod PROMPT  ;
       '\<Sub-Categoria '
ON SELECTION POPUP prod do busproXX with;
bar(),w_selpro
DEFINE POPUP produ FROM 15, 18 TO  ;
       22, 73 PROMPT FIELDS  ;
       pro_codpro + '³' +  ;
       SUBSTR(pro_descri, 1, 20) +  ;
       '³' + SUBSTR(pro_modelo, 1,  ;
       20) IN screen COLOR SCHEME  ;
       8
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
ON KEY LABEL f6 do produc with w_campo,w_selec,w_selpro
IF LASTKEY() <> 27
     w_campo = gc_pro00.pro_codpro
ENDIF
SELECT (w_selec)
RETURN
*
PROCEDURE busproxx
PARAMETER bar, w_selpro
ON KEY
FOR w_cont = 1 TO 26
     MOVE POPUP prod BY 0, -1
ENDFOR
FOR w_cont = 1 TO 07
     MOVE POPUP prod BY -1, 0
ENDFOR
ACTIVATE WINDOW pide
SELECT (w_selpro)
IF bar = 1
     w_codpro = SPACE(14)
     w_indice = 'CODIGO'
     @ 00, 00 SAY 'C¢digo :'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 2
     w_codpro = SPACE(40)
     w_indice = 'DESCRI'
     @ 00, 00 SAY 'Descr. :'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 3
     w_codpro = SPACE(14)
     w_indice = 'NUMPAR'
     @ 00, 00 SAY 'N.Parte :'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 4
     w_codpro = SPACE(4)
     w_indice = 'SUBCAT'
     @ 00, 00 SAY 'Sub-Cat.:'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
SELECT gc_alm00
SET ORDER TO CODIGO
SET FILTER TO alm_codalm = rge_codalm
SELECT gc_pro00
w_order = ORDER()
SET ORDER TO w_INDICE
SET RELATION TO pro_codpro INTO gc_alm00
READ
IF LASTKEY() <> 27
     SET NEAR ON
     SEEK w_codpro
     SET NEAR OFF
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do tomacodXX
     BROWSE FIELDS  ;
            gc_pro00.pro_codpro  ;
            :R :H =  ;
            'Cod. Produc.',  ;
            gc_pro00.pro_descri  ;
            :R : 27 :H =  ;
            'Descripci¢n',  ;
            gc_alm00.alm_stkfis  ;
            :R : 5 :H = 'Stock'  ;
            :P = '9,999',  ;
            gc_pro00.pro_modelo  ;
            :R : 10 :H = 'Modelo',  ;
            gc_pro00.pro_codree  ;
            :R :H = 'Reemplazo'  ;
            FREEZE pro_codpro IN  ;
            produ
ENDIF
set order to &w_order
ON KEY
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR w_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR w_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
RETURN
*
PROCEDURE tomacodxx
ON KEY
w_campo = gc_pro00.pro_codpro
DEACTIVATE WINDOW pide, produ
DEACTIVATE POPUP prod
RETURN
*
FUNCTION valtab2
PARAMETER clave, codig, colu,  ;
          largo
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .T.
ENDIF
SELECT ge_tab0
codaux = clave + codig
SEEK '&codaux'
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** C¢digo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
ENDIF
DO sacaf6
RETURN .T.
*
PROCEDURE col_bk1c
DO coloca WITH 05, 20, w_codmar
DO coloca WITH 05, 25, w_nommar
DO coloca WITH 06, 20, w_codmod
DO coloca WITH 06, 36, w_nommod
DO coloca WITH 07, 20, w_numser
DO coloca WITH 08, 25,  ;
   STR(w_numstk, 9)
DO coloca WITH 10, 20,  ;
   STR(w_codcli, 11)
DO coloca WITH 10, 32, w_noment
DO coloca WITH 11, 20, w_nomcal
DO coloca WITH 12, 20, w_nomdis
DO coloca WITH 12, 25, w_desdis
DO coloca WITH 13, 20, w_nomciu
DO coloca WITH 13, 25, w_desciu
DO coloca WITH 14, 20,  ;
   STR(w_numte1, 8)
DO coloca WITH 14, 30,  ;
   STR(w_numte2, 8)
DO coloca WITH 16, 20,  ;
   DTOC(w_fecemi)
DO coloca WITH 16, 37, w_horemi
DO coloca WITH 16, 65,  ;
   DTOC(w_fecven)
DO coloca WITH 18, 20, w_emisor
DO coloca WITH 18, 25, w_nomemi
DO coloca WITH 19, 20, w_indori
DO coloca WITH 19, 25, w_destia
IF w_indori = 'GARA' .OR.  ;
   w_indori = 'GREC'
     DO coloca WITH 19, 50,  ;
        w_doctia
ENDIF
DO coloca WITH 20, 20, w_codmon
DO coloca WITH 20, 25,  ;
   SUBSTR(w_nommon, 1, 20)
DO coloca WITH 20, 60,  ;
   TRANSFORM(w_abonos,  ;
   '999,999.99')
DO coloca WITH 21, 20,  ;
   STR(w_tecnic, 9)
DO coloca WITH 21, 30, w_nomtec
FOR i = 1 TO 12
     IF pro(i) <> SPACE(14)
          DO coloca WITH 24+i, 2,  ;
             pro(i)
          DO coloca WITH 24+i, 16,  ;
             SUBSTR(dex(i), 1,  ;
             25)
          DO coloca WITH 24+i, 37,  ;
             STR(can(i), 5, 0)
          DO coloca WITH 24+i, 43,  ;
             IIF(w_codmon='DOL ',  ;
             STR(pre(i), 13, 2),  ;
             STR(pres(i), 13,  ;
             2))
          DO coloca WITH 24+i, 56,  ;
             STR(sto(i), 5, 0)
          DO coloca WITH 24+i, 62,  ;
             IIF(w_codmon='DOL ',  ;
             STR(tot(i), 13, 2),  ;
             STR(tots(i), 13,  ;
             2))
     ELSE
          DO coloca WITH 24+i, 2,  ;
             SPACE(73)
     ENDIF
ENDFOR
DO coloca WITH 39, 03,  ;
   STR(IIF(w_codmon='DOL ',  ;
   w_totrep, s_totrep), 10, 2)
DO coloca WITH 39, 14,  ;
   STR(w_totdes, 05, 2)
DO coloca WITH 39, 20,  ;
   STR(IIF(w_codmon='DOL ',  ;
   w_totnet, s_totnet), 10, 2)
DO coloca WITH 39, 31,  ;
   STR(IIF(w_codmon='DOL ',  ;
   w_totman, s_totman), 10, 2)
DO coloca WITH 39, 42,  ;
   STR(IIF(w_codmon='DOL ',  ;
   w_totafe, s_totafe), 10, 2)
DO coloca WITH 39, 53,  ;
   STR(IIF(w_codmon='DOL ',  ;
   w_totigv, s_totigv), 10, 2)
DO coloca WITH 39, 64,  ;
   STR(IIF(w_codmon='DOL ',  ;
   w_totgrl, s_totgrl), 10, 2)
RETURN
*
PROCEDURE col_bk2c
FOR i = 1 TO 08
     DO coloca WITH 41+i, 2,  ;
        w_obspre(i)
ENDFOR
RETURN
*
PROCEDURE coloca
PARAMETER li, co, de
solic( li) = SUBSTR(solic(li), 1,  ;
     co - 1) + de +  ;
     SUBSTR(solic(li), LEN(de) +  ;
     co)
RETURN
*
FUNCTION ootab
PARAMETER var1, var2
narea = SELECT()
SELECT ge_tab0
SEEK var1 + var2
IF FOUND()
     wrk_despag = SUBSTR(ge_tab0.tab_destab,  ;
                  1, 30)
ELSE
     wrk_despag = SPACE(30)
ENDIF
SELECT (narea)
RETURN wrk_despag
*
PROCEDURE mueve3
PARAMETER key
FOR y = 1 TO LEN(key)
     key1 = ASC(SUBSTR(key, y,  ;
            1))
     DO CASE
          CASE key1 = 5 .AND. des >  ;
               1
               des = des - 1
          CASE key1 = 24 .AND.  ;
               des < 51 - lin
               des = des + 1
          CASE key1 = 18
               des = des - lin
               IF des < 1
                    des = 1
               ENDIF
          CASE key1 = 3
               des = des + lin
               IF des > 51 - lin
                    des = 51 -  ;
                          lin
               ENDIF
          CASE key1 = 1
               des = 1
          CASE key1 = 6
               des = 51 - lin
     ENDCASE
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
ENDFOR
RETURN
*
PROCEDURE versol
ON KEY LABEL F8
ACTIVATE WINDOW indicar
SAVE SCREEN TO w_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
DEFINE WINDOW ayuda FROM 2, 1 TO  ;
       29, 73 IN screen
ACTIVATE WINDOW TOP ayuda
@ 0, 12 SAY 'S I N T O M A S'
@ 0, 45 SAY 'A C C E S O R I O S'
@ 1, 0 SAY REPLICATE(CHR(196),  ;
  35) + 'Â' + REPLICATE(CHR(196),  ;
  35)
FOR i = 1 TO 15
     @ 1 + i, 0 SAY w_codsin(i) +  ;
       '³' + w_acceso(i)
ENDFOR
@ 17, 0 SAY REPLICATE(CHR(196),  ;
  35) + 'Á' + REPLICATE(CHR(196),  ;
  35)
@ 18, 10 SAY  ;
  'O B S E R V A C I O N E S'
@ 19, 0 SAY REPLICATE(CHR(196),  ;
  71)
FOR i = 1 TO 6
     @ 19 + i, 0 SAY w_observ(i)
ENDFOR
SET CURSOR OFF
WAIT ''
SET CURSOR ON
DEACTIVATE WINDOW ayuda
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM w_pantax
ACTIVATE WINDOW trabajo
ON KEY LABEL F8 do versol
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

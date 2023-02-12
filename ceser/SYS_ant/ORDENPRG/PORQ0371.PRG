*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
SET SYSMENU ON
IF config_prg == 1
     titu1 = 'CONSULTA'
     titu2 = ' SOLICITUD DE SERVICIO '
ENDIF
IF config_prg == 2
     titu1 = 'CONSULTA'
     titu2 = ' ORDENES DE REPARACION '
ENDIF
IF config_prg == 3
     titu1 = 'CONSULTA'
     titu2 = ' PRESUPUESTOS '
ENDIF
PUBLIC tempo, ubicacion
STORE 'arriba' TO ubicacion
tempo = ' '
wrk_origen = SPACE(2)
wrk_progra = PROGRAM()
DO crea_win
CLOSE DATABASES
CLEAR TYPEAHEAD
ON KEY LABEL F6 do ayuda12
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, titu1
DO saycenter WITH 2, titu2
DO esc_modo WITH 'I'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
@ 6, 10 CLEAR TO 10, 65
@ 6, 10 TO 10, 65
SAVE SCREEN TO wk_panta
wk_numord = 0
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
     ON KEY
     CLOSE DATABASES
     DO sacawin
     RETURN
ENDIF
USE
ppal = .T.
USE IN 17 SHARED gc_cmv00 AGAIN  ;
    ORDER cmv_feinmo
w_tipcam = ootc2(DATE(), ;
           rge_monbas,'DOL ', ;
           '2')
DO WHILE ppal
     RESTORE SCREEN FROM wk_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     efecin = 1
     STORE 0 TO veces, wk_otec,  ;
           wk_numord, w_pagsol,  ;
           w_pagdol, wk_flete,  ;
           sol_cosfle, wk_repues
     STORE 0 TO wk_totrep,  ;
           s_totrep, wk_totdes,  ;
           s_totdes, wk_totnet,  ;
           s_totnet, wk_totafe
     STORE 0 TO wk_totman,  ;
           s_totman, wk_totgrl,  ;
           s_totgrl, wk_totigv,  ;
           s_totigv, s_totafe
     STORE ' ' TO wk_ogar,  ;
           wk_oest, wk_esor
     STORE SPACE(30) TO wk_nom1,  ;
           wk_nom2, nota1, nota2,  ;
           nota3, nota4, nota5,  ;
           nota6
     STORE SPACE(4) TO w_linea,  ;
           wk_codmon
     IF config_prg == 1
          wk_numero = 0
          wk_var = 1
          @ 8, 20 SAY  ;
            'N£mero Solicitud : '  ;
            GET wk_numero PICTURE  ;
            '99999999' VALID  ;
            ordnul(wk_numero, ;
            wk_var) WHEN  ;
            colocaf6()
          SET CURSOR ON
          READ
          SET CURSOR OFF
          DO CASE
               CASE LASTKEY() ==  ;
                    27 .AND.  ;
                    efecin == 1
                    CLEAR GETS
                    ppal = .F.
                    LOOP
               CASE LASTKEY() ==  ;
                    27 .AND.  ;
                    efecin == 2
                    CLEAR GETS
                    LOOP
          ENDCASE
     ENDIF
     IF config_prg == 2
          ppal = .T.
          DO WHILE ppal
               efecin = 1
               @ 8, 20 SAY  ;
                 ' N§ Orden Reparaci¢n '  ;
                 GET wk_numord  ;
                 PICTURE  ;
                 '99999999' VALID  ;
                 numord(wk_numord)
               SET CURSOR ON
               READ
               SET CURSOR OFF
               IF LASTKEY() == 27  ;
                  .AND. efecin ==  ;
                  2
                    LOOP
               ELSE
                    ppal = .F.
               ENDIF
          ENDDO
          IF wk_numord == 0 .OR.  ;
             LASTKEY() == 27
               ppal = .F.
          ELSE
               ppal = .T.
          ENDIF
     ENDIF
     IF config_prg == 3
          wk_numero = 0
          wk_var = 0
          wk_var = 2
          @ 8, 20 SAY  ;
            'N£mero Presupuesto : '  ;
            GET wk_numero PICTURE  ;
            '99999999' VALID  ;
            ordnul(wk_numero, ;
            wk_var) WHEN  ;
            colocaf6()
          SET CURSOR ON
          READ
          SET CURSOR OFF
          DO CASE
               CASE LASTKEY() ==  ;
                    27 .AND.  ;
                    efecin == 1
                    CLEAR GETS
                    ppal = .F.
                    LOOP
               CASE LASTKEY() ==  ;
                    27 .AND.  ;
                    efecin == 2
                    CLEAR GETS
                    LOOP
          ENDCASE
     ENDIF
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               ppal = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               LOOP
     ENDCASE
     IF config_prg == 2
          USE SHARED st_iorep  ;
              ORDER CODIGO
          wk_numaux = STR(wk_numord,  ;
                      8)
          SEEK '&wk_numaux'
          IF numpre <> SPACE(8)
               wk_numero = VAL(numpre)
               wk_var = 2
          ELSE
               wk_numero = VAL(numsol)
               wk_var = 1
          ENDIF
          USE
     ENDIF
     ACTIVATE SCREEN
     ACTIVATE WINDOW trabajo
     SET DISPLAY TO VGA50
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO 42, 76
     ZOOM WINDOW indicar NORM  ;
          FROM 45, 0 TO 48, 76
     ACTIVATE SCREEN
     ACTIVATE WINDOW trabajo
     IF wk_var == 1
          lin = 40
          anc = 75
          des = 1
          com = 1
          DIMENSION solic( 50)
          STORE FOPEN('solicitu.txt')  ;
                TO file_handl
          FOR i = 1 TO 50
               solic( i) =  ;
                    FREAD(file_handl,  ;
                    77)
          ENDFOR
          = FCLOSE(file_handl)
          IF config_prg == 2
               ON KEY LABEL f8 do veorden
               solic( 3) =  ;
                    '               O R D E N   D E   R E P A R A C I O N          N§ ' +  ;
                    STR(wk_numord,  ;
                    8) + '  '
          ENDIF
          DIMENSION wk_codsin(  ;
                    15)
          DIMENSION wk_acceso(  ;
                    15)
          DIMENSION wk_observ(  ;
                    06)
          wk_numaux = STR(wk_numero,  ;
                      8)
          USE SHARED st_isrep  ;
              ORDER CODIGO
          SEEK '&wk_numaux'
          wk_numso = VAL(numdoc)
          wk_fecemi = fecemi
          wk_feccom = feccom
          wk_emisor = codemi
          wk_codcli = VAL(codent)
          wk_indori = indori
          wk_indest = indest
          wk_codmar = codmar
          wk_codmod = codmod
          wk_numser = numser
          wk_abonos = monabo
          wk_codmon = codmon
          IF wk_codmon = 'DOL '
               wk_abonos = w_pagdol
          ELSE
               wk_abono = w_pagsol
          ENDIF
          wk_codstk = codstk
          wk_numstk = VAL(numstk)
          wk_coddes = coddes
          FOR i = 1 TO 15
               wk_codsin = SPACE(35)
               wk_acceso( i) =  ;
                        SUBSTR(desace,  ;
                        1 + ((i -  ;
                        1) * 35),  ;
                        35)
               wk_acceso( i) =  ;
                        wk_acceso(i) +  ;
                        SPACE(35 -  ;
                        LEN(wk_acceso(i)))
               IF i <= 6
                    wk_observ( i) =  ;
                             SUBSTR(observ,  ;
                             1 +  ;
                             ((i -  ;
                             1) *  ;
                             45),  ;
                             45)
                    wk_observ( i) =  ;
                             wk_observ(i) +  ;
                             SPACE(45 -  ;
                             LEN(wk_observ(i)))
               ENDIF
          ENDFOR
          wk_cliaux = 'C' +  ;
                      STR(wk_codcli,  ;
                      11)
          IF  .NOT.  ;
              USED('st_iclpr')
               USE SHARED  ;
                   st_iclpr ORDER  ;
                   CODIGO
          ELSE
               SELECT st_iclpr
               SET ORDER TO codigo
          ENDIF
          SEEK '&wk_cliaux'
          wk_noment = noment
          wk_nomcal = nomcal
          wk_nomdis = nomdis
          wk_nomciu = nomciu
          wk_numte1 = numte1
          wk_numte2 = numte2
          wk_aux = wk_codmar +  ;
                   wk_codmod
          USE SHARED st_imode  ;
              ORDER CODIGO
          SEEK '&wk_aux'
          wk_aux = codcla
          w_linea = linea
          STORE SPACE(15) TO  ;
                wk_doga, wk_prov
          STORE {} TO wk_fevt,  ;
                wk_fecg
          IF wk_indori = 'GARA'  ;
             .OR. wk_indori =  ;
             'GREC' .OR.  ;
             wk_indori = 'PVEN'  ;
             .OR. wk_indori =  ;
             'PREC'
               USE SHARED  ;
                   ST_ISERI ORDER  ;
                   SER_CODMAR
               SEEK wk_codmar +  ;
                    wk_codmod +  ;
                    wk_numser
               IF FOUND()
                    wk_prov = 'Proveedor:' +  ;
                              ALLTRIM(codent)
                    wk_doga = 'Doc.Garan:' +  ;
                              ALLTRIM(docgar)
                    wk_fevt = 'Fecha Vta:' +  ;
                              DTOC(fecvta)
                    wk_fecg = 'Fecha Fin:' +  ;
                              DTOC(fecgar)
               ELSE
                    wk_prov = 'Proveedor:'
                    wk_doga = 'Doc.Garan:'
                    wk_fevt = 'Fecha Vta:'
                    wk_fecg = 'Fecha Fin:'
               ENDIF
          ENDIF
          SELECT 2
          USE SHARED st_sint  ;
              ORDER sin_lincod
          SELECT 1
          USE SHARED st_sicli  ;
              ORDER CODIGO
          SEEK '&wk_numaux'
          i = 1
          DO WHILE  .NOT. EOF()  ;
             .AND. numdoc== ;
             wk_numaux
               wk_aux2 = SUBSTR(codsin,  ;
                         2, 3)
               SELECT 2
               SEEK w_linea +  ;
                    wk_aux2
               wk_codsin( i) =  ;
                        SUBSTR(dessin,  ;
                        1, 35)
               i = i + 1
               SELECT 1
               SKIP
          ENDDO
          CLOSE DATABASES
          DO col_bk1b
          DO col_bk2b
          FOR i = des TO (lin +  ;
              des - 1)
               @ i - des, 0 SAY  ;
                 SUBSTR(solic(i),  ;
                 com, anc)
          ENDFOR
          @ 05, 50 SAY wk_doga
          @ 06, 50 SAY wk_fevt
          DO CASE
               CASE config_prg ==  ;
                    1
                    DO esc_indica  ;
                       WITH 1,  ;
                       'AYU',  ;
                       'BBB',  ;
                       'OTR',  ;
                       'BBB'
                    DO esc_indica  ;
                       WITH 2,  ;
                       'BBB',  ;
                       'BBB',  ;
                       'IGN',  ;
                       'ESC'
               CASE config_prg ==  ;
                    2
                    DO esc_indica  ;
                       WITH 1,  ;
                       'AYU',  ;
                       'BBB',  ;
                       'OTR',  ;
                       'BBB'
                    DO esc_indica  ;
                       WITH 2,  ;
                       'OOR',  ;
                       'OOE',  ;
                       'IGN',  ;
                       'ESC'
               CASE config_prg ==  ;
                    3
                    DO esc_indica  ;
                       WITH 1,  ;
                       'AYU',  ;
                       'BBB',  ;
                       'OTR',  ;
                       'BBB'
                    DO esc_indica  ;
                       WITH 2,  ;
                       'BBB',  ;
                       'BBB',  ;
                       'IGN',  ;
                       'ESC'
          ENDCASE
          ppal2 = .T.
          DO WHILE ppal2
               wk_inkey = 0
               IF config_prg = 2
                    DO WHILE   ;
                       .NOT.  ;
                       (STR(wk_inkey,  ;
                       2)$ ;
                       ' 18, 3, 27,-9-8-3-4' ;
                       )
                         wk_inkey =  ;
                          INKEY(0)
                    ENDDO
               ELSE
                    DO WHILE   ;
                       .NOT.  ;
                       (STR(wk_inkey,  ;
                       2)$ ;
                       ' 18, 3, 27,-9-8' ;
                       )
                         wk_inkey =  ;
                          INKEY(0)
                    ENDDO
               ENDIF
               DO CASE
                    CASE wk_inkey ==  ;
                         18
                         DO mueve3  ;
                            WITH  ;
                            CHR(wk_inkey)
                    CASE wk_inkey ==  ;
                         3
                         DO mueve3  ;
                            WITH  ;
                            CHR(wk_inkey)
                    CASE wk_inkey == - ;
                         3
                         DO veorden
                         ON KEY LABEL;
f3
                    CASE wk_inkey == - ;
                         8
                         DO cuenta
                    CASE wk_inkey == - ;
                         4
                         DO veestado
                    CASE wk_inkey ==  ;
                         27 .OR.  ;
                         wk_inkey == - ;
                         9
                         IF wk_inkey ==  ;
                            27
                              ppal =  ;
                               .F.
                         ENDIF
                         ppal2 = .F.
                         LOOP
                    OTHERWISE
                         DO mueve3  ;
                            WITH  ;
                            CHR(wk_inkey)
               ENDCASE
          ENDDO
     ELSE
          lin = 40
          anc = 75
          des = 1
          com = 1
          DIMENSION solic( 50)
          STORE FOPEN('presupue.txt')  ;
                TO file_handl
          FOR i = 1 TO 50
               solic( i) =  ;
                    FREAD(file_handl,  ;
                    77)
          ENDFOR
          = FCLOSE(file_handl)
          IF config_prg == 2
               ON KEY LABEL f8 do veorden
               solic( 3) =  ;
                    '               O R D E N   D E   R E P A R A C I O N          N§ ' +  ;
                    STR(wk_numord,  ;
                    8) + '  '
          ENDIF
          wk_numaux = STR(wk_numero,  ;
                      8)
          USE SHARED st_ispre  ;
              ORDER CODIGO
          SEEK '&wk_numaux'
          wk_numsol = VAL(numsol)
          USE
          DIMENSION wk_codsin(  ;
                    15)
          DIMENSION wk_acceso(  ;
                    15)
          DIMENSION wk_observ(  ;
                    06)
          DIMENSION pro( 12),  ;
                    dex( 12),  ;
                    uni( 12),  ;
                    can( 12),  ;
                    pre( 12),  ;
                    pres( 12),  ;
                    dec( 12),  ;
                    tot( 12),  ;
                    tots( 12),  ;
                    sto( 12)
          DIMENSION wk_obspre(  ;
                    08)
          FOR i = 1 TO 12
               pro( i) =  ;
                  SPACE(14)
               dex( i) =  ;
                  SPACE(20)
               uni( i) = SPACE(3)
               sto( i) = 0
               can( i) = 0
               pre( i) = 0
               dec( i) = 0
               tot( i) = 0
               IF i <= 8
                    wk_obspre( i) =  ;
                             SPACE(38)
               ENDIF
          ENDFOR
          wk_numaux = STR(wk_numsol,  ;
                      8)
          USE SHARED st_isrep  ;
              ORDER CODIGO
          SEEK '&wk_numaux'
          wk_codcli = VAL(codent)
          wk_indori = indori
          wk_indorx = indori
          wk_codmar = codmar
          wk_codmod = codmod
          wk_numser = numser
          wk_abonos = monabo
          wk_codmon = codmon
          wk_codstk = codstk
          wk_numstk = VAL(numstk)
          FOR i = 1 TO 15
               wk_codsin = SPACE(35)
               wk_acceso( i) =  ;
                        SUBSTR(desace,  ;
                        1 + ((i -  ;
                        1) * 35),  ;
                        35)
               wk_acceso( i) =  ;
                        wk_acceso(i) +  ;
                        SPACE(35 -  ;
                        LEN(wk_acceso(i)))
               IF i <= 6
                    wk_observ( i) =  ;
                             SUBSTR(observ,  ;
                             1 +  ;
                             ((i -  ;
                             1) *  ;
                             45),  ;
                             45)
                    wk_observ( i) =  ;
                             wk_observ(i) +  ;
                             SPACE(45 -  ;
                             LEN(wk_observ(i)))
               ENDIF
          ENDFOR
          IF codmon = 'DOL '
               w_pagdol = monabo
               w_pagsol = ROUND(monabo *  ;
                          w_tipcam,  ;
                          2)
          ELSE
               w_pagsol = monabo
               w_pagdol = ROUND(monabo /  ;
                          w_tipcam,  ;
                          2)
          ENDIF
          wk_cliaux = 'C' +  ;
                      STR(wk_codcli,  ;
                      11)
          USE SHARED st_iclpr  ;
              ORDER CODIGO
          SEEK '&wk_cliaux'
          wk_noment = noment
          wk_nomcal = nomcal
          wk_nomdis = nomdis
          wk_nomciu = nomciu
          wk_numte1 = numte1
          wk_numte2 = numte2
          wk_aux = 'MARC' +  ;
                   wk_codmar
          USE SHARED ge_tab0  ;
              ORDER codigo
          SEEK '&wk_aux'
          wk_nommar = SUBSTR(tab_destab,  ;
                      1, 30)
          wk_aux = wk_codmar +  ;
                   wk_codmod
          USE SHARED st_imode  ;
              ORDER CODIGO
          SEEK '&wk_aux'
          wk_nommod = SUBSTR(nommod,  ;
                      1, 30)
          wk_aux = wk_codmar +  ;
                   wk_codmod
          USE SHARED st_imode  ;
              ORDER CODIGO
          SEEK '&wk_aux'
          wk_aux = codcla
          w_linea = linea
          SELECT 2
          USE SHARED st_sint  ;
              ORDER sin_lincod
          SELECT 1
          USE SHARED st_sicli  ;
              ORDER CODIGO
          wk_numaux = STR(wk_numsol,  ;
                      8)
          SEEK '&wk_numaux'
          i = 1
          IF FOUND()
               DO WHILE  .NOT.  ;
                  EOF() .AND.  ;
                  numdoc== ;
                  wk_numaux
                    wk_aux2 = SUBSTR(codsin,  ;
                              2,  ;
                              3)
                    SELECT 2
                    SEEK w_linea +  ;
                         wk_aux2
                    wk_codsin( i) =  ;
                             SUBSTR(dessin,  ;
                             1,  ;
                             35)
                    i = i + 1
                    SELECT 1
                    SKIP
               ENDDO
          ENDIF
          wk_numaux = STR(wk_numero,  ;
                      8)
          USE SHARED st_ispre  ;
              ORDER CODIGO
          SEEK '&wk_numaux'
          wk_fecemi = fecemi
          wk_fecven = fecven
          wk_emisor = codemi
          wk_indori = indori
          wk_tecnic = VAL(codtec)
          wk_numsol = VAL(numsol)
          wk_codmon = codmon
          wk_totrep = monrep
          wk_totdes = pordes
          wk_totnet = ROUND(monrep -  ;
                      (monrep *  ;
                      pordes /  ;
                      100), 2)
          wk_totman = monman
          wk_totgrl = wk_totnet +  ;
                      wk_totman
          wk_totigv = totigv
          wk_totafe = wk_totgrl -  ;
                      wk_totigv
          FOR i = 1 TO 8
               wk_obspre( i) =  ;
                        SUBSTR(observ,  ;
                        1 + ((i -  ;
                        1) * 38),  ;
                        38)
               wk_obspre( i) =  ;
                        wk_obspre(i) +  ;
                        SPACE(38 -  ;
                        LEN(wk_obspre(i)))
          ENDFOR
          USE SHARED st_idpre  ;
              ORDER CODIGO
          SEEK '&wk_numaux'
          SELECT 3
          USE SHARED gc_alm00  ;
              ORDER codigo
          SELECT 2
          USE SHARED gc_pro00  ;
              ORDER codigo
          SELECT 1
          i = 1
          DO WHILE numdoc== ;
             wk_numaux
               wk_aux = codpro
               pro( i) = codpro
               can( i) = canpro
               pre( i) = valpro
               dec( i) = pordes
               tot( i) = totite
               pres( i) =  ;
                   ROUND(pre(i) *  ;
                   w_tipcam, 2)
               tots( i) =  ;
                   ROUND(pres(i) *  ;
                   can(i), 2)
               s_totrep = s_totrep +  ;
                          tots(i)
               SELECT 3
               seek '&wk_aux'
               sto( i) =  ;
                  alm_stkfis
               SELECT 2
               SEEK '&wk_aux'
               dex( i) =  ;
                  SUBSTR(pro_descri,  ;
                  1, 20)
               uni( i) =  ;
                  SUBSTR(pro_unimed,  ;
                  1, 03)
               SELECT 1
               i = i + 1
               SKIP
          ENDDO
          IF wk_codmon = 'DOL '
               wk_abonos = w_pagdol
          ELSE
               wk_abono = w_pagsol
               s_totdes = pordes
               s_totnet = ROUND(s_totrep -  ;
                          (s_totrep *  ;
                          pordes /  ;
                          100),  ;
                          2)
               s_totman = ROUND(wk_totman *  ;
                          w_tipcam,  ;
                          2)
               s_totgrl = s_totnet +  ;
                          s_totman
               s_totafe = ROUND(s_totgrl /  ;
                          (1 +  ;
                          wrk_facigv),  ;
                          2)
               s_totigv = s_totgrl -  ;
                          s_totafe
          ENDIF
          wk_aux = 'EMIS' +  ;
                   wk_emisor
          USE SHARED ge_tab0  ;
              ORDER codigo
          SEEK '&wk_aux'
          wk_nomemi = SUBSTR(tab_destab,  ;
                      1, 30)
          wk_aux = STR(wk_tecnic,  ;
                   9)
          wk_aux = 'DIST' +  ;
                   wk_nomdis
          SEEK '&wk_aux'
          wk_desdis = tab_destab
          wk_aux = 'MONE' +  ;
                   wk_codmon
          SEEK '&wk_aux'
          IF FOUND()
               wk_nommon = tab_destab
          ELSE
               wk_nommon = SPACE(35)
          ENDIF
          SEEK 'INGA' + wk_indori
          wk_destia = tab_destab
          wk_aux = 'PROV' +  ;
                   wk_nomciu
          SEEK '&wk_aux'
          wk_desciu = tab_destab
          USE SHARED st_itecn  ;
              ORDER CODIGO
          SEEK '&wk_aux'
          wk_nomtec = noment
          CLOSE DATABASES
          DO col_bk1c
          DO col_bk2c
          FOR i = des TO (lin +  ;
              des - 1)
               @ i - des, 0 SAY  ;
                 SUBSTR(solic(i),  ;
                 com, anc)
          ENDFOR
          DO CASE
               CASE config_prg ==  ;
                    1
                    DO esc_indica  ;
                       WITH 1,  ;
                       'AYU',  ;
                       'BBB',  ;
                       'SOL',  ;
                       'BBB'
                    DO esc_indica  ;
                       WITH 2,  ;
                       'BBB',  ;
                       'BBB',  ;
                       'IGN',  ;
                       'ESC'
               CASE config_prg ==  ;
                    2
                    DO esc_indica  ;
                       WITH 1,  ;
                       'AYU',  ;
                       'BBB',  ;
                       'SOL',  ;
                       'BBB'
                    DO esc_indica  ;
                       WITH 2,  ;
                       'OOR',  ;
                       'BBB',  ;
                       'IGN',  ;
                       'ESC'
               CASE config_prg ==  ;
                    3
                    DO esc_indica  ;
                       WITH 1,  ;
                       'AYU',  ;
                       'BBB',  ;
                       'SOL',  ;
                       'BBB'
                    DO esc_indica  ;
                       WITH 2,  ;
                       'BBB',  ;
                       'BBB',  ;
                       'IGN',  ;
                       'ESC'
          ENDCASE
          ppal2 = .T.
          DO WHILE ppal2
               wk_inkey = 0
               IF config_prg = 2
                    DO WHILE   ;
                       .NOT.  ;
                       (STR(wk_inkey,  ;
                       2)$ ;
                       ' 5,24,18, 3, 27,-3-9-7' ;
                       )
                         wk_inkey =  ;
                          INKEY(0)
                    ENDDO
               ELSE
                    DO WHILE   ;
                       .NOT.  ;
                       (STR(wk_inkey,  ;
                       2)$ ;
                       ' 5,24,18, 3, 27,-9-7' ;
                       )
                         wk_inkey =  ;
                          INKEY(0)
                    ENDDO
               ENDIF
               DO CASE
                    CASE wk_inkey ==  ;
                         27 .OR.  ;
                         wk_inkey == - ;
                         9
                         IF wk_inkey ==  ;
                            27
                              ppal =  ;
                               .F.
                         ENDIF
                         ppal2 = .F.
                         LOOP
                    CASE wk_inkey == - ;
                         7
                         DO versol
                         ON KEY LABEL;
F8
                    CASE wk_inkey == - ;
                         3
                         DO veorden
                         ON KEY LABEL;
F3
                    OTHERWISE
                         DO mueve3  ;
                            WITH  ;
                            CHR(wk_inkey)
               ENDCASE
          ENDDO
     ENDIF
     SET DISPLAY TO VGA25
     ACTIVATE SCREEN
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO 17, 76
     ZOOM WINDOW indicar NORM  ;
          FROM 20, 0 TO 23, 76
     ACTIVATE WINDOW trabajo
ENDDO
CLOSE DATABASES
ON KEY LABEL F9
ON KEY LABEL F6
ON KEY LABEL F8
ON KEY LABEL F10
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DO sacawin
@ 24, 69 SAY SPACE(15)
RELEASE wk_pantax
RETURN
*
PROCEDURE ayuda12
ON KEY LABEL F6
ON KEY LABEL F8
IF VARREAD() == 'WK_NUMERO'
     IF wk_var == 1
          CLOSE DATABASES
          USE SHARED st_isrep  ;
              ORDER CODIGO
          wrk_origen = 'SS'
          campoa = 'numdoc+"  "+dtoc(fecemi)+"  "+codent+"  "+codmod+"   "+numser+" "+subst(indest,1,2)+" "+indori'
          DO ayuda7 WITH campoa,  ;
             wrk_origen
     ELSE
          CLOSE DATABASES
          USE SHARED st_ispre  ;
              ORDER CODIGO
          wrk_origen = 'PR'
          campoa = 'numdoc+" "+dtoc(fecemi)+" "+numsol+" "+SUBSTR(NUMSER,1,12)+" "+numord+" "+codmod+" "+subst(indest,1,2)'
          DO ayuda9 WITH campoa,  ;
             wrk_origen
     ENDIF
     USE
ENDIF
IF VARREAD() == 'WK_OEST'
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'ESOR'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA ESTADO OPERACION'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF VARREAD() == 'WK_OTEC'
     USE SHARED st_itecn ORDER  ;
         CODIGO
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE TECNICOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
     USE
ENDIF
IF VARREAD() == 'WK_NUMORD' .AND.  ;
   config_prg > 1
     USE SHARED st_iorep ORDER  ;
         CODIGO
     wrk_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+SUBSTR(NUMSER,1,12)+" "+codent+" "+codmod+" "+subst(indest,1,2)'
     DO ayuda7 WITH campoa,  ;
        wrk_origen
     USE
ENDIF
ON KEY LABEL F6 do ayuda12
RETURN
*
FUNCTION garantia
PARAMETER gar
boleano = 0
DO control WITH boleano
IF boleano = 1
     boleano = 0
     RETURN 0
ENDIF
IF gar == SPACE(1)
     DO error2 WITH  ;
        '** Error Item debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
@ 17, 35 SAY gar
RETURN .T.
*
FUNCTION numord
PARAMETER num
IF num == 0
     DO error WITH  ;
        '** Error N§ debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION numord2
PARAMETER num
IF num == 0
     DO error2 WITH  ;
        '** Error N§ debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
wk_clave = STR(num, 8)
USE SHARED st_iorep ORDER CODIGO
SEEK '&wk_clave'
IF  .NOT. FOUND()
     USE
     DO error2 WITH  ;
        '** N§ Orden Reparacion NO EXISTE. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'N'
     USE
     DO error2 WITH  ;
        '** N§ Orden Reparacion esta NULA. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
USE
RETURN .T.
*
FUNCTION ordnul
PARAMETER cod, var
IF cod = 0
     DO error WITH  ;
        '** Error N§ debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
cod = STR(cod, 8)
IF var == 1
     SELECT 1
     USE SHARED st_isrep ORDER  ;
         CODIGO
     SEEK '&cod'
     IF  .NOT. FOUND()
          USE
          DO error WITH  ;
             '** Error Solicitud NO EXISTE. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
     IF SUBSTR(indest, 1, 1) ==  ;
        'N'
          USE
          DO error WITH  ;
             '** Error Solicitud  esta Anulada. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
ELSE
     SELECT 1
     USE SHARED st_ispre ORDER  ;
         CODIGO
     SEEK '&cod'
     IF  .NOT. FOUND()
          USE
          DO error WITH  ;
             '** Error Presupuesto NO EXISTE. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
     IF SUBSTR(indest, 1, 1) ==  ;
        'N'
          USE
          DO error WITH  ;
             '** Error Presupuesto esta Anulado. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
ENDIF
USE
DO sacaf6
RETURN .T.
*
PROCEDURE veorden
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW indicar
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 28, 65
@ 13, 09 CLEAR TO 27, 64
@ 13, 09 TO 27, 64
@ 13, 32 SAY ' Consulta Orden '
wk_oaux = wk_indori
veces = veces + 1
IF veces = 1
     USE SHARED st_iorep ORDER  ;
         CODIGO
     wk_numaux = STR(wk_numord,  ;
                 8)
     SEEK '&wk_numaux'
     wk_oest = auxest
     wk_otec = VAL(codtec)
     nota1 = SUBSTR(observ, 1,  ;
             38)
     nota2 = SUBSTR(observ, 39,  ;
             38)
     nota3 = SUBSTR(observ, 78,  ;
             38)
     nota4 = SUBSTR(observ, 117,  ;
             38)
     nota5 = SUBSTR(observ, 156,  ;
             38)
     nota6 = SUBSTR(observ, 195,  ;
             38)
     wk_obux = 'ESOR' + wk_oest
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SEEK '&wk_obux'
     wk_nom1 = SUBSTR(tab_destab,  ;
               1, 28)
     SEEK 'INGA' + wk_ogar
     wk_destia = tab_destab
     wk_obux = STR(wk_otec, 9)
     USE SHARED st_itecn ORDER  ;
         CODIGO
     SEEK '&wk_obux'
     wk_nom2 = SUBSTR(noment, 1,  ;
               28)
     USE
ENDIF
@ 18, 35 SAY wk_nom1
@ 19, 35 SAY wk_nom2
@ 17, 35 SAY wk_destia
@ 15, 11 SAY 'N§ Orden    : ' +  ;
  STR(wk_numord, 8)
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(DATE())
@ 17, 11 SAY 'Garantia    :' +  ;
  wk_ogar
@ 18, 11 SAY 'Estado      :' +  ;
  wk_oest
@ 19, 11 SAY 'T‚cnico     :' +  ;
  STR(wk_otec, 9)
@ 20, 11 SAY 'Notas       :'
@ 21, 25 SAY nota1 PICTURE '@!'
@ 22, 25 SAY nota2 PICTURE '@!'
@ 23, 25 SAY nota3 PICTURE '@!'
@ 24, 25 SAY nota4 PICTURE '@!'
@ 25, 25 SAY nota5 PICTURE '@!'
@ 26, 25 SAY nota6 PICTURE '@!'
= INKEY(0)
RESTORE SCREEN FROM wk_pantax
RETURN
*
PROCEDURE veestado
SAVE SCREEN TO wk_pantax
USE SHARED st_mvord ORDER CODIGO
SET ORDER TO 2
wk_clave1 = STR(wk_numord, 8)
SET NEAR ON
SEEK '&wk_clave1'
SET NEAR OFF
wk_numero = SPACE(8)
campox = 'dtoc(dia)+"  "+hora+" "+tecnico+"       "+estado+"  "+destado'
DEFINE WINDOW ayu4 FROM 14, 3 TO  ;
       15, 74 SHADOW
wk_color = color5 + ',,,,,' +  ;
           SUBSTR(color5, AT(',',  ;
           color5) + 1)
browse field cer = " " :H="", uno = &campox;
:H="  Fecha    Hora       Tecnico  Situacion     Descripcion ";
 key wk_clave1 in window ayu4 nowait;
 freeze cer   
ACTIVATE WINDOW TOP ayu4
FOR j = 1 TO 08
     ZOOM WINDOW ayu4 NORM FROM  ;
          14, 2 TO 15 + j, 74
     ZOOM WINDOW st_mvord NORM  ;
          FROM -1, -4 TO j, 74
ENDFOR
BROWSE LAST KEY wk_clave1
DEACTIVATE WINDOW ayu4
RESTORE SCREEN FROM wk_pantax
RETURN
*
PROCEDURE cuenta
SAVE SCREEN TO w_pantax
ACTIVATE WINDOW indicar
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 30, 65
@ 13, 09 CLEAR TO 30, 64
@ 13, 09 TO 30, 64
@ 13,20 say 'Pagos a Cuenta y Saldo         &empre8 ';
color scheme 8
DIMENSION fecha( 1), docum( 1),  ;
          valor( 1)
DIMENSION fechac( 1), documc( 1),  ;
          valorc( 1)
SELECT 10
USE SHARED st_iorep ORDER  ;
    ord_numsol
SEEK STR(wk_numero, 8)
IF FOUND()
     w_numsol = numsol
     w_numord = numdoc
     w_codmar = codmar
     w_codmod = codmod
     w_indest = ALLTRIM(indest)
     w_indori = indori
     w_numfac = numfabo
     w_esor = auxest
     w_cosmob = cosmob
     w_cosrep = cosrep
     w_auxest = auxest
     w_desrep = desrep
     w_desmob = desmob
     w_flete = flete
     w_totdes = totdes
ELSE
     w_numsol = STR(wk_numero, 8)
     w_numord = SPACE(8)
     w_codmar = wk_codmar
     w_codmod = wk_codmod
     w_indest = ALLTRIM(wk_indest)
     w_indori = wk_indori
     w_numfac = SPACE(10)
     w_esor = SPACE(4)
     w_auxest = SPACE(4)
     w_cosmob = 0
     w_cosrep = 0
     w_flete = 0
     w_totdes = 0
     w_desrep = 0
     w_desmob = 0
ENDIF
w_facigv = wrk_facigv
SELECT 11
USE SHARED gc_hve00 ORDER nrdore
STORE 0 TO wrk_toacta, i, cane,  ;
      w_tipca2, s_cosfle,  ;
      s_totgen
STORE 0 TO i, i2, s_cosrep,  ;
      s_cosmob, s_cosfle,  ;
      s_toacta, s_total, s_totpag,  ;
      s_descue
DO pormcost WITH 'st_iorep'
DO pinta
w_numsol = VAL(w_numsol)
w_numord = VAL(w_numord)
SELECT st_iorep
USE
RETURN
*
PROCEDURE pinta
p = 13
IF  .NOT. EMPTY(fecha(1))
     FOR b = 1 TO i
          p = p + 1
          @ p, 20 SAY fecha(b)
          @ p, 30 SAY docum(b)
          @ p, 41 SAY valor(b)  ;
            PICTURE  ;
            '999,999,999.99'
     ENDFOR
ENDIF
IF  .NOT. EMPTY(fechac(1)) .AND.  ;
    i2 > 0
     p = p + 1
     @ p, 19 SAY  ;
       ' Cancelado con :'
     FOR b = 1 TO i2
          p = p + 1
          @ p, 20 SAY fechac(b)
          @ p, 30 SAY documc(b)
          @ p, 41 SAY valorc(b)  ;
            PICTURE  ;
            '999,999,999.99'
     ENDFOR
ENDIF
p = p + 2
@ p + 0, 20 SAY  ;
  'Costo En Repuesto..:' +  ;
  SPACE(1) + TRANSFORM(s_cosrep,  ;
  '999,999,999.99')
@ p + 1, 20 SAY  ;
  'Costo Mano de Obra.:' +  ;
  SPACE(1) + TRANSFORM(s_cosmob,  ;
  '999,999,999.99')
@ p + 2, 20 SAY  ;
  'Costo Flete........:' +  ;
  SPACE(1) + TRANSFORM(s_cosfle,  ;
  '999,999,999.99')
@ p + 3, 20 SAY  ;
  'Total..............:' +  ;
  SPACE(1) + TRANSFORM(s_total,  ;
  '999,999,999.99')
@ p + 4, 20 SAY  ;
  'Descuento..........:' +  ;
  SPACE(1) + TRANSFORM(s_descue,  ;
  '999,999,999.99')
@ p + 5, 20 SAY  ;
  'Pagos a Cta. ......:' +  ;
  SPACE(1) + TRANSFORM(s_toacta,  ;
  '999,999,999.99')
IF  .NOT. EMPTY(w_numfac)
     @ p + 7, 20 SAY  ;
       'Saldo a Pagar......:' +  ;
       SPACE(1) + TRANSFORM(0,  ;
       '999,999,999.99') COLOR N+/ ;
       W 
ELSE
     IF w_indest = 'V' .OR.  ;
        w_indest = 'P'
          @ p + 6, 20 SAY  ;
            'Saldo a Pagar......:' +  ;
            SPACE(1) +  ;
            TRANSFORM(s_totpag,  ;
            '999,999,999.99')  ;
            COLOR N/W 
          @ p + 6, 57 SAY  ;
            'Aprox.'
     ELSE
          IF (w_esor = '025 '  ;
             .OR. w_esor =  ;
             '024 ')
               @ p + 6, 20 SAY  ;
                 'Saldo a Pagar......:' +  ;
                 SPACE(1) +  ;
                 TRANSFORM(0,  ;
                 '999,999,999.99' ;
                 ) COLOR N/W 
          ELSE
               IF (w_esor =  ;
                  '029 ' .OR.  ;
                  w_esor = '028 '  ;
                  .OR. w_esor =  ;
                  '023 ' .OR.  ;
                  w_esor =  ;
                  '022 ') .AND.  ;
                  SUBSTR(wk_indori,  ;
                  2, 1) = 'R'  ;
                  .AND. w_cosrep =  ;
                  0
                    @ p + 6, 20  ;
                      SAY  ;
                      'Saldo a Pagar......:' +  ;
                      SPACE(1) +  ;
                      TRANSFORM(0,  ;
                      '999,999,999.99' ;
                      ) COLOR N/W 
               ELSE
                    IF ((w_esor =  ;
                       '028 '  ;
                       .OR.  ;
                       w_esor =  ;
                       '023 ')  ;
                       .AND.  ;
                       wk_indori =  ;
                       'FGAR'  ;
                       .AND.  ;
                       w_cosrep =  ;
                       0)
                         @ p + 6,  ;
                           20 SAY  ;
                           'Saldo a Pagar......:' +  ;
                           SPACE(1) +  ;
                           TRANSFORM(0,  ;
                           '999,999,999.99' ;
                           )  ;
                           COLOR  ;
                           N/W 
                    ELSE
                         @ p + 6,  ;
                           20 SAY  ;
                           'Saldo a Pagar......:' +  ;
                           SPACE(1) +  ;
                           TRANSFORM(s_totpag,  ;
                           '999,999,999.99' ;
                           )  ;
                           COLOR  ;
                           N/W 
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
= INKEY(0, 'H')
RESTORE SCREEN FROM w_pantax
RETURN
*
PROCEDURE xcuenta
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW indicar
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 28, 65
@ 13, 09 CLEAR TO 27, 64
@ 13, 09 TO 27, 64
@ 13, 20 SAY  ;
  'Pagos a Cuenta y Saldo' COLOR  ;
  SCHEME 8
DIMENSION cuent1( 6)
DIMENSION cuent2( 6)
DIMENSION cuent3( 6)
DO usedbf WITH 'gc_hve00',  ;
   'nrdore'
STORE 0 TO wrk_toacta, i, cane,  ;
      w_tipca2, sol_cosfle
GOTO TOP
SEEK STR(wk_numso, 8)
can = 0
p = 14
wk_flete = 0
@ p, 20 SAY 'Fecha' + SPACE(5) +  ;
  'NøDocumento' + SPACE(4) +  ;
  'Importe S/.'
SCAN WHILE  ;
     VAL(gc_hve00.hve_nrdore) =  ;
     wk_numso .AND.  .NOT. EOF()
     IF hve_estdoc <> 'A' .AND.  ;
        hve_codmov = 'PCTA'
          i = i + 1
          p = p + 1
          cuent1( i) =  ;
                gc_hve00.hve_fecdoc
          cuent2( i) =  ;
                gc_hve00.hve_nrodoc
          cuent3( i) =  ;
                gc_hve00.hve_solgen
          wrk_toacta = wrk_toacta +  ;
                       hve_solgen
          IF hve_codcta = '005 '
               wk_flete = wk_flete +  ;
                          hve_totvta
               sol_cosfle = sol_cosfle +  ;
                            hve_solgen
          ENDIF
          @ p, 20 SAY cuent1(i)
          @ p, 30 SAY cuent2(i)
          @ p, 42 SAY cuent3(i)  ;
            PICTURE '999,999.99'
     ENDIF
ENDSCAN
USE SHARED st_iorep ORDER  ;
    ord_numsol
SEEK STR(wk_numso, 8)
IF FOUND()
     wk_numord = VAL(numdoc)
     USE SHARED gc_hve00 ORDER  ;
         nrdore
     SEEK STR(wk_numord, 8)
     IF FOUND()
          p = p + 1
          @ p, 20 SAY  ;
            'Cancelado con...'
          cane = 1
     ENDIF
     SCAN WHILE  ;
          VAL(gc_hve00.hve_nrdore) =  ;
          wk_numord .AND.  .NOT.  ;
          EOF()
          IF hve_estdoc <> 'A'
               i = i + 1
               p = p + 1
               cuent1( i) =  ;
                     gc_hve00.hve_fecdoc
               cuent2( i) =  ;
                     gc_hve00.hve_nrodoc
               cuent3( i) =  ;
                     gc_hve00.hve_mtocan +  ;
                     hve_soldes
               wrk_toacta = wrk_toacta +  ;
                            hve_mtocan +  ;
                            hve_soldes
               @ p, 20 SAY  ;
                 cuent1(i)
               @ p, 30 SAY  ;
                 cuent2(i)
               @ p, 42 SAY  ;
                 cuent3(i)  ;
                 PICTURE  ;
                 '999,999.99'
               w_tipca2 = gc_hve00.hve_tipcam
               can = 1
          ENDIF
     ENDSCAN
ENDIF
USE
DO usedbf WITH 'st_iorep',  ;
   'codigo'
SEEK STR(wk_numord, 8)
IF FOUND()
     IF can = 1
          w_tipcam = w_tipca2
     ENDIF
     wk_esor = auxest
     wk_repues = cosrep
     wk_mano = cosmob
     IF ALLTRIM(indest) = 'F'  ;
        .OR. ALLTRIM(indest) =  ;
        'B' .OR. ALLTRIM(indest) =  ;
        'C'
          wk_flete = flete
          IF ALLTRIM(indest) =  ;
             'C'
               w_fla = 0
               w_fecha = DATE()
               DO usedbf WITH  ;
                  'st_mvord',  ;
                  'estado'
               SEEK STR(wk_numord,  ;
                    8) + '025'
               IF FOUND()
                    w_fla = 1
                    w_fecha = dia
               ELSE
                    SEEK STR(wk_numord,  ;
                         8) +  ;
                         '024'
                    IF FOUND()
                         w_fla = 1
                         w_fecha =  ;
                          dia
                    ELSE
                         SEEK STR(wk_numord,  ;
                              8) +  ;
                              '027'
                         IF FOUND()
                              w_fla =  ;
                               1
                         ELSE
                              SEEK  ;
                               STR(wk_numord,  ;
                               8) +  ;
                               '026'
                              IF FOUND()
                                   w_fla = 1
                              ELSE
                                   SEEK STR(wk_numord, 8) + '021'
                                   IF FOUND()
                                        w_fla = 1
                                   ENDIF
                              ENDIF
                         ENDIF
                         IF w_fla =  ;
                            1  ;
                            .AND.  ;
                            SUBSTR(wk_indori,  ;
                            1, 1) =  ;
                            'F'
                              w_fecha =  ;
                               dia
                         ENDIF
                    ENDIF
               ENDIF
               IF w_fla = 1 .AND.  ;
                  w_fecha <>  ;
                  DATE()
                    USE IN 17  ;
                        SHARED  ;
                        gc_cmv00  ;
                        AGAIN  ;
                        ORDER  ;
                        cmv_feinmo
                    w_tipcam = ootc2(w_fecha, ;
                               rge_monbas, ;
                               'DOL ', ;
                               '2')
               ENDIF
          ENDIF
          sol_cosfle = ROUND(ROUND(wk_flete *  ;
                       w_tipcam,  ;
                       2) * (1 +  ;
                       wrk_facigv),  ;
                       2)
     ELSE
          wk_flete = wk_flete +  ;
                     flete
          sol_fle = ROUND(ROUND(flete *  ;
                    w_tipcam, 2) *  ;
                    (1 +  ;
                    wrk_facigv),  ;
                    2)
          sol_cosfle = sol_cosfle +  ;
                       sol_fle
     ENDIF
     DO usedbf WITH 'st_iorep',  ;
        'codigo'
     SEEK STR(wk_numord, 8)
     wk_subtot = subtot
     wk_totdes = totdes
     wk_totnet = (wk_mano +  ;
                 wk_repues +  ;
                 wk_flete) -  ;
                 wk_totdes
     wk_totigv = ROUND(wk_totnet *  ;
                 wrk_facigv, 2)
     wk_totbru = wk_totnet +  ;
                 wk_totigv
ELSE
     wk_repues = 0
     wk_mano = 0
     wk_subtot = 0
     wk_totdes = 0
     wk_totnet = 0
     wk_totigv = 0
     wk_totbru = 0
ENDIF
IF can = 1
     sol_cosrep = ROUND(ROUND(wk_repues *  ;
                  w_tipca2, 2) *  ;
                  (1 +  ;
                  wrk_facigv),  ;
                  2)
     sol_cosmob = ROUND(ROUND(wk_mano *  ;
                  w_tipca2, 2) *  ;
                  (1 +  ;
                  wrk_facigv),  ;
                  2)
ELSE
     sol_cosrep = ROUND(ROUND(wk_repues *  ;
                  w_tipcam, 2) *  ;
                  (1 +  ;
                  wrk_facigv),  ;
                  2)
     sol_cosmob = ROUND(ROUND(wk_mano *  ;
                  w_tipcam, 2) *  ;
                  (1 +  ;
                  wrk_facigv),  ;
                  2)
ENDIF
sol_total = sol_cosrep +  ;
            sol_cosmob +  ;
            sol_cosfle
sol_subtot = ROUND(sol_total / (1 +  ;
             wrk_facigv), 2)
sol_igv = sol_total - sol_subtot
IF can = 1
     sol_descue = wk_totdes *  ;
                  w_tipca2
ELSE
     sol_descue = wk_totdes *  ;
                  w_tipcam
ENDIF
sol_totgen = sol_total -  ;
             sol_descue
sol_totpag = sol_totgen -  ;
             wrk_toacta
p = p + 1
@ p, 20 SAY  ;
  'Costo En Repuesto.: ' +  ;
  SPACE(2) + TRANSFORM(sol_cosrep,  ;
  '999,999.99')
@ p + 1, 20 SAY  ;
  'Costo Mano de Obra: ' +  ;
  SPACE(2) + TRANSFORM(sol_cosmob,  ;
  '999,999.99')
@ p + 2, 20 SAY  ;
  'Costo Flete.......: ' +  ;
  SPACE(2) + TRANSFORM(sol_cosfle,  ;
  '999,999.99')
@ p + 3, 20 SAY  ;
  'Total.............: ' +  ;
  SPACE(2) + TRANSFORM(sol_total,  ;
  '999,999.99')
@ p + 4, 20 SAY  ;
  'Pagos a Cta. .....: ' +  ;
  SPACE(2) + TRANSFORM(wrk_toacta,  ;
  '999,999.99')
IF  .NOT. EMPTY(numfabo)
     @ p + 6, 20 SAY  ;
       'Saldo a Pagar.....: ' +  ;
       SPACE(2) + TRANSFORM(0,  ;
       '999,999.99') COLOR N+/W 
ELSE
     IF indest = 'V' .OR. indest =  ;
        'P'
          @ p + 5, 20 SAY  ;
            'Saldo a Pagar.....: ' +  ;
            'Falta cerrar la O/R'  ;
            COLOR N/W 
     ELSE
          IF (wk_esor = '025 '  ;
             .OR. wk_esor =  ;
             '024 ')
               @ p + 5, 20 SAY  ;
                 'Saldo a Pagar.....: ' +  ;
                 SPACE(2) +  ;
                 TRANSFORM(0,  ;
                 '999,999.99')  ;
                 COLOR N/W 
          ELSE
               IF (wk_esor =  ;
                  '029 ' .OR.  ;
                  wk_esor =  ;
                  '028 ' .OR.  ;
                  wk_esor =  ;
                  '023 ' .OR.  ;
                  wk_esor =  ;
                  '022 ') .AND.  ;
                  SUBSTR(indori,  ;
                  2, 1) = 'R'  ;
                  .AND. wk_repues =  ;
                  0
                    @ p + 5, 20  ;
                      SAY  ;
                      'Saldo a Pagar.....: ' +  ;
                      SPACE(2) +  ;
                      TRANSFORM(0,  ;
                      '999,999.99' ;
                      ) COLOR N/W 
               ELSE
                    IF ((wk_esor =  ;
                       '028 '  ;
                       .OR.  ;
                       wk_esor =  ;
                       '023 ')  ;
                       .AND.  ;
                       indori =  ;
                       'FGAR'  ;
                       .AND.  ;
                       wk_repues =  ;
                       0)
                         @ p + 5,  ;
                           20 SAY  ;
                           'Saldo a Pagar.....: ' +  ;
                           SPACE(2) +  ;
                           TRANSFORM(0,  ;
                           '999,999.99' ;
                           )  ;
                           COLOR  ;
                           N/W 
                    ELSE
                         @ p + 5,  ;
                           20 SAY  ;
                           'Saldo a Pagar.....: ' +  ;
                           SPACE(2) +  ;
                           TRANSFORM(sol_totpag,  ;
                           '999,999.99' ;
                           )  ;
                           COLOR  ;
                           N/W 
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
= INKEY(0, 'H')
RESTORE SCREEN FROM wk_pantax
USE
RETURN
*
PROCEDURE col_bk1b
USE SHARED ge_tab0 ORDER codigo
wk_aux = 'EMIS' + wk_emisor
SEEK '&wk_aux'
IF FOUND()
     wk_nomemi = tab_destab
ELSE
     wk_nomemi = SPACE(35)
ENDIF
wk_aux = 'ESTA' + wk_indest
SEEK '&wk_aux'
wk_estado = tab_destab
wk_aux = 'MONE' + wk_codmon
SEEK '&wk_aux'
IF FOUND()
     wk_nommon = tab_destab
ELSE
     wk_nommon = SPACE(35)
ENDIF
wk_aux = 'DIST' + wk_nomdis
SEEK '&wk_aux'
wk_desdis = tab_destab
wk_aux = 'PROV' + wk_nomciu
SEEK '&wk_aux'
wk_desciu = tab_destab
USE
DO coloca WITH 01, 66,  ;
   STR(wk_numero, 8)
DO coloca WITH 16, 20,  ;
   DTOC(wk_fecemi)
DO coloca WITH 16, 38,  ;
   SUBSTR(TIME(), 1, 5)
DO coloca WITH 18, 20, wk_emisor
DO coloca WITH 18, 25,  ;
   SUBSTR(wk_nomemi, 1, 24)
DO coloca WITH 02, 01,  ;
   SUBSTR(wk_estado, 1, 06)
DO coloca WITH 05, 20, wk_codmar
DO coloca WITH 06, 20, wk_codmod
DO coloca WITH 07, 20, wk_numser
DO coloca WITH 08, 20, wk_indori
wk_aux = 'MARC' + wk_codmar
USE SHARED ge_tab0 ORDER codigo
SEEK '&wk_aux'
IF FOUND()
     DO coloca WITH 05, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
wk_aux = 'INGA' + wk_indori
USE SHARED ge_tab0 ORDER codigo
seek '&wk_aux'
IF FOUND()
     DO coloca WITH 08, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
wk_aux = wk_codmar + wk_codmod
USE SHARED st_imode ORDER CODIGO
SEEK '&wk_aux'
IF FOUND()
     DO coloca WITH 06, 36,  ;
        SUBSTR(nommod, 1, 30)
ENDIF
USE
IF wk_indori == 'GARA' .OR.  ;
   wk_indori == 'GREC' .OR.  ;
   wk_indori == 'PVEN' .OR.  ;
   wk_indori == 'PREC'
     DO coloca WITH 5, 51,  ;
        wk_prov
     DO coloca WITH 6, 51,  ;
        wk_doga
     DO coloca WITH 7, 51,  ;
        wk_fevt
     DO coloca WITH 8, 51,  ;
        wk_fecg
ENDIF
IF wk_indori == 'GARA' .OR.  ;
   wk_indori == 'GREC' .OR.  ;
   wk_indori == 'PVEN' .OR.  ;
   wk_indori == 'PREC'
     DO coloca WITH 19, 20,  ;
        'EN GARANTIA   '
ELSE
     IF wk_indori == 'FGAR' .OR.  ;
        wk_indori == 'FREC'
          DO coloca WITH 19, 20,  ;
             'FUERA GARANTIA'
     ELSE
          DO coloca WITH 19, 20,  ;
             'PREVENTA'
     ENDIF
ENDIF
DO coloca WITH 10, 20,  ;
   STR(wk_codcli, 11)
DO coloca WITH 10, 32, wk_noment
DO coloca WITH 11, 20, wk_nomcal
DO coloca WITH 12, 20, wk_nomdis
DO coloca WITH 12, 25, wk_desdis
DO coloca WITH 13, 20, wk_nomciu
DO coloca WITH 13, 25, wk_desciu
DO coloca WITH 14, 20,  ;
   STR(wk_numte1, 8)
DO coloca WITH 14, 30,  ;
   STR(wk_numte2, 8)
DO coloca WITH 16, 65,  ;
   DTOC(wk_feccom)
DO coloca WITH 20, 20,  ;
   TRANSFORM(wk_abonos,  ;
   '999,999.99')
DO coloca WITH 20, 44, wk_codmon
DO coloca WITH 20, 49,  ;
   SUBSTR(wk_nommon, 1, 15)
DO coloca WITH 21, 20, wk_coddes+ ;
   ' '+IIF(wk_coddes=='R',  ;
   'REPARACION ', 'DOMICILIO')
FOR i = 1 TO 15
     DO coloca WITH 24+i, 2,  ;
        wk_codsin(i)
ENDFOR
FOR i = 1 TO 15
     DO coloca WITH 24+i, 38,  ;
        wk_acceso(i)
ENDFOR
RETURN
*
PROCEDURE sol_servic
tit_tit5 = 'P R E S U P U E S T O'
tit_subr = 'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
tit_tit6 = 'N§ :'
tit_nrop = wrk_numero
tit_nomb = wk_noment
tit_dire = wk_nomcal
tit_tele = STR(wk_numte1, 8)
tit_codi = STR(wk_codcli)
wrk_codi = STR(wk_codcli)
tit_tit3 = 'N§S:'
tit_ord = 'N§O:'
tit_soli = wrk_numsol
tit_orde = wrk_numord
tit_prod = wk_nommod
tit_cpro = wk_codmod
tit_tecn = STR(wk_tecnic)
tit_tit4 = 'OBSERVACIONES : '
tit_maob = wk_totman
wk_fecha = DTOC(wk_fecemi)
tit_fech = SUBSTR(wk_fecha, 1, 2) +  ;
           SPACE(3) +  ;
           SUBSTR(wk_fecha, 4, 2) +  ;
           SPACE(3) +  ;
           SUBSTR(wk_fecha, 7,  ;
           2)
?? CHR(15)
@ 01, (65 - LEN(tit_tit5) / 2)  ;
  SAY tit_tit5
@ 01, 115 SAY tit_tit6 +  ;
  STR(tit_nrop, 8)
@ 02, (65 - LEN(tit_tit5) / 2)  ;
  SAY tit_subr
@ 02, 115 SAY tit_tit3 +  ;
  STR(tit_soli, 8)
@ 03, 115 SAY tit_ord +  ;
  STR(tit_orde, 8)
@ 04, 110 SAY DATE()
@ 04, 120 SAY TIME()
@ 05, 010 SAY tit_nomb
@ 05, 075 SAY wrk_codi
@ 06, 010 SAY tit_dire
@ 07, 115 SAY tit_tele
@ 09, 113 SAY wk_destia
@ 10, 003 SAY SUBSTR(tit_prod, 1,  ;
  20)
@ 10, 025 SAY SUBSTR(tit_cpro, 1,  ;
  20)
@ 10, 050 SAY SUBSTR(wk_nommar, 1,  ;
  20)
@ 10, 075 SAY wk_numser
IF wk_indori = 'GARA' .OR.  ;
   wk_indori == 'PVEN'
     @ 10, 115 SAY  ;
       DTOC(wk_fecvta) + '  ' +  ;
       wk_docgar
ENDIF
@ 11, 002 SAY 'ITEM'
@ 11, 010 SAY 'CODIGO'
@ 11, 028 SAY 'DESCRIPCION'
@ 11, 070 SAY 'CANT'
@ 11, 085 SAY 'P.UNIT.'
@ 11, 105 SAY 'P.TOTAL.'
tit_ttot = wk_totgrl
con_lin = 12
i = 1
DO WHILE  .NOT. EMPTY(pro(i))
     tit_item = LTRIM(STR(i, 2))
     tit_codi = pro(i)
     tit_desc = dex(i)
     tit_cant = can(i)
     tit_prec = pre(i)
     tit_tota = tot(i)
     @ con_lin, 004 SAY tit_item
     @ con_lin, 010 SAY tit_codi
     @ con_lin, 028 SAY  ;
       SUBSTR(tit_desc, 1, 40)
     @ con_lin, 070 SAY tit_cant  ;
       PICTURE '999'
     @ con_lin, 080 SAY tit_prec  ;
       PICTURE '99,999,999.99'
     @ con_lin, 100 SAY tit_tota  ;
       PICTURE '99,999,999.99'
     con_lin = con_lin + 1
     i = i + 1
ENDDO
@ 22, 80 SAY 'TECNICO.:'
@ 22, 90 SAY tit_tecn
@ 22, 101 SAY SUBSTR(wk_nomtec, 1,  ;
  30)
@ 23, 00 SAY  ;
  'TOTAL DE REPUESTOS :'
@ 23, 21 SAY wk_totnet PICTURE  ;
  '999,999.99'
@ 23, 33 SAY 'M/O :'
@ 23, 38 SAY tit_maob PICTURE  ;
  '999,999.99'
@ 23, 50 SAY 'I.G.V. :'
@ 23, 62 SAY wk_totigv PICTURE  ;
  '999,999.99'
@ 24, 00 SAY tit_tit4
@ 24, 50 SAY 'TOTAL US $.'
@ 24, 62 SAY tit_ttot PICTURE  ;
  '999,999.99'
@ 34, (65 - LEN(tit_tit5) / 2)  ;
  SAY tit_tit5
@ 34, 115 SAY tit_tit6 +  ;
  STR(tit_nrop, 8)
@ 35, (65 - LEN(tit_tit5) / 2)  ;
  SAY tit_subr
@ 35, 115 SAY tit_tit3 +  ;
  STR(tit_soli, 8)
@ 36, 115 SAY tit_ord +  ;
  STR(tit_orde, 8)
@ 37, 110 SAY DATE()
@ 37, 120 SAY TIME()
@ 38, 010 SAY tit_nomb
@ 38, 075 SAY wrk_codi
@ 39, 010 SAY tit_dire
@ 40, 115 SAY tit_tele
@ 42, 113 SAY wk_destia
@ 43, 003 SAY SUBSTR(tit_prod, 1,  ;
  20)
@ 43, 025 SAY SUBSTR(tit_cpro, 1,  ;
  20)
@ 43, 050 SAY SUBSTR(wk_nommar, 1,  ;
  20)
@ 43, 075 SAY wk_numser
IF wk_indori = 'GARA' .OR.  ;
   wk_indori == 'PVEN'
     @ 43, 115 SAY  ;
       DTOC(wk_fecvta) + '  ' +  ;
       wk_docgar
ENDIF
@ 44, 002 SAY 'ITEM'
@ 44, 010 SAY 'CODIGO'
@ 44, 028 SAY 'DESCRIPCION'
@ 44, 070 SAY 'CANT'
@ 44, 085 SAY 'P.UNIT.'
@ 44, 105 SAY 'P.TOTAL.'
tit_ttot = wk_totgrl
con_lin = 45
i = 1
DO WHILE  .NOT. EMPTY(pro(i))
     tit_item = LTRIM(STR(i, 2))
     tit_codi = pro(i)
     tit_desc = dex(i)
     tit_cant = can(i)
     tit_prec = pre(i)
     tit_tota = tot(i)
     @ con_lin, 004 SAY tit_item
     @ con_lin, 010 SAY tit_codi
     @ con_lin, 028 SAY  ;
       SUBSTR(tit_desc, 1, 40)
     @ con_lin, 070 SAY tit_cant  ;
       PICTURE '999'
     @ con_lin, 080 SAY tit_prec  ;
       PICTURE '99,999,999.99'
     @ con_lin, 100 SAY tit_tota  ;
       PICTURE '99,999,999.99'
     con_lin = con_lin + 1
     i = i + 1
ENDDO
@ 55, 80 SAY 'TECNICO.:'
@ 55, 90 SAY tit_tecn
@ 55, 101 SAY SUBSTR(wk_nomtec, 1,  ;
  30)
@ 56, 01 SAY  ;
  'TOTAL DE REPUESTOS :'
@ 56, 21 SAY wk_totnet PICTURE  ;
  '999,999.99'
@ 56, 33 SAY 'M/O :'
@ 56, 38 SAY tit_maob PICTURE  ;
  '999,999.99'
@ 56, 50 SAY 'I.G.V. :'
@ 56, 60 SAY wk_totigv PICTURE  ;
  '999,999.99'
@ 57, 00 SAY tit_tit4
@ 57, 50 SAY 'TOTAL US $.'
@ 57, 62 SAY tit_ttot PICTURE  ;
  '999,999.99'
?? CHR(15)
EJECT
RETURN
*
PROCEDURE col_bk1c
DO coloca WITH 01, 68,  ;
   STR(wk_numero, 8)
DO coloca WITH 05, 20, wk_codmar
DO coloca WITH 05, 25, wk_nommar
DO coloca WITH 06, 20, wk_codmod
DO coloca WITH 06, 36, wk_nommod
DO coloca WITH 07, 20, wk_numser
DO coloca WITH 10, 20,  ;
   STR(wk_codcli, 11)
DO coloca WITH 10, 32, wk_noment
DO coloca WITH 11, 20, wk_nomcal
DO coloca WITH 12, 20, wk_nomdis
DO coloca WITH 12, 25, wk_desdis
DO coloca WITH 13, 20, wk_nomciu
DO coloca WITH 13, 25, wk_desciu
DO coloca WITH 14, 20,  ;
   STR(wk_numte1, 8)
DO coloca WITH 14, 30,  ;
   STR(wk_numte2, 8)
DO coloca WITH 16, 20,  ;
   DTOC(wk_fecemi)
DO coloca WITH 16, 65,  ;
   DTOC(wk_fecven)
DO coloca WITH 18, 20, wk_emisor
DO coloca WITH 18, 25, wk_nomemi
DO coloca WITH 18, 55,  ;
   STR(wk_numstk, 9)
DO coloca WITH 19, 20, wk_indori
DO coloca WITH 19, 25, wk_destia
DO coloca WITH 20, 20, wk_codmon
DO coloca WITH 20, 25,  ;
   SUBSTR(wk_nommon, 1, 20)
DO coloca WITH 20, 60,  ;
   TRANSFORM(wk_abonos,  ;
   '999,999.99')
DO coloca WITH 21, 20,  ;
   STR(wk_tecnic, 9)
DO coloca WITH 21, 30, wk_nomtec
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
             IIF(wk_codmon='DOL ',  ;
             STR(pre(i), 13, 2),  ;
             STR(pres(i), 13,  ;
             2))
          DO coloca WITH 24+i, 56,  ;
             STR(sto(i), 5, 0)
          DO coloca WITH 24+i, 62,  ;
             IIF(wk_codmon='DOL ',  ;
             STR(tot(i), 13, 2),  ;
             STR(tots(i), 13,  ;
             2))
     ELSE
          DO coloca WITH 24+i, 2,  ;
             SPACE(73)
     ENDIF
ENDFOR
DO coloca WITH 39, 03,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totrep, s_totrep), 10, 2)
DO coloca WITH 39, 14,  ;
   STR(wk_totdes, 05, 2)
DO coloca WITH 39, 20,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totnet, s_totnet), 10, 2)
DO coloca WITH 39, 31,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totman, s_totman), 10, 2)
DO coloca WITH 39, 42,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totafe, s_totafe), 10, 2)
DO coloca WITH 39, 53,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totigv, s_totigv), 10, 2)
DO coloca WITH 39, 64,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totgrl, s_totgrl), 10, 2)
RETURN
*
PROCEDURE col_bk2c
FOR i = 1 TO 8
     DO coloca WITH 41+i, 2,  ;
        wk_obspre(i)
ENDFOR
RETURN
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
*** 
*** ReFox - retrace your steps ... 
***

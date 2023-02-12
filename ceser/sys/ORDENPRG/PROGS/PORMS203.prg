*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
IF config_prg == 1
     ind_prg = '<PORM0203>'
     titu1 = 'CREACION'
ENDIF
IF config_prg == 2
     ind_prg = '<PORM0232>'
     titu1 = 'MANTENCION'
ENDIF
IF config_prg == 3
     ind_prg = '<PORM0233>'
     titu1 = 'ANULACION'
ENDIF
titu2 = 'ORDEN  DE  REPARACION'
wrk_progra = PROGRAM()
DO crea_win
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
wk_numaux = 0
USE
IF config_prg == 1
     wk_numord = 0
     ppal = .T.
ELSE
     ppal = .T.
ENDIF
PUBLIC tecnico
DO WHILE ppal
     RESTORE SCREEN FROM wk_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     efecin = 1
     IF config_prg == 1
          @ 7, 20 SAY 'Origen :'
          @ 7, 30 PROMPT  ;
            'Solicitud'
          @ 7, 42 PROMPT  ;
            'Presupuesto'
          wk_numero = 0
          wk_otec = 0
          wrk_dogtia = SPACE(1)
          @ 9, 20 SAY 'Numero : '  ;
            GET wk_numero PICTURE  ;
            '99999999' VALID  ;
            ordnul(wk_numero, ;
            wk_var) WHEN  ;
            colocaf6()
          wk_var = 1
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
     ELSE
          wk_numord = 0
          @ 8, 20 SAY  ;
            ' N§ Orden Reparacion '  ;
            GET wk_numord PICTURE  ;
            '99999999' VALID  ;
            numord2(wk_numord)  ;
            WHEN colocaf6()
     ENDIF
     SET CURSOR ON
     READ
     SET CURSOR OFF
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               ppal = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               LOOP
     ENDCASE
     IF config_prg <> 1
          USE SHARED st_iorep  ;
              ORDER CODIGO
          wk_numaux = STR(wk_numord,  ;
                      8)
          SEEK '&wk_numaux'
          wk_numero = VAL(numsol)
          wk_var = 1
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
          DIMENSION solic( 48)
          STORE FOPEN('solicitu.txt')  ;
                TO file_handl
          FOR i = 1 TO 48
               solic( i) =  ;
                    FREAD(file_handl,  ;
                    77)
          ENDFOR
          = FCLOSE(file_handl)
          solic( 3) =  ;
               '               O R D E N   D E   R E P A R A C I O N          N§ ' +  ;
               STR(wk_numord, 8) +  ;
               '  '
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
          wk_codstk = codstk
          wk_numstk = VAL(numstk)
          wk_coddes = coddes
          FOR i = 1 TO 15
               wk_codsin = SPACE(35)
               wk_acceso( i) =  ;
                        SUBSTR(MLINE(desace,  ;
                        i), 1,  ;
                        35)
               wk_acceso( i) =  ;
                        wk_acceso(i) +  ;
                        SPACE(35 -  ;
                        LEN(wk_acceso(i)))
               IF i <= 6
                    wk_observ( i) =  ;
                             SUBSTR(MLINE(observ,  ;
                             i),  ;
                             1,  ;
                             45)
                    wk_observ( i) =  ;
                             wk_observ(i) +  ;
                             SPACE(45 -  ;
                             LEN(wk_observ(i)))
               ENDIF
          ENDFOR
          wk_cliaux = 'C' +  ;
                      STR(wk_codcli,  ;
                      9)
          USE SHARED st_iclpr  ;
              ORDER CODIGO
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
          SELECT 2
          USE SHARED st_isint  ;
              ORDER CODIGO
          SELECT 1
          USE SHARED st_issre  ;
              ORDER CODIGO
          SEEK '&wk_numaux'
          i = 1
          IF FOUND()
               DO WHILE  .NOT.  ;
                  EOF() .AND.  ;
                  numdoc== ;
                  wk_numaux
                    wk_aux2 = wk_aux +  ;
                              codsin
                    SELECT 2
                    SEEK '&wk_aux2'
                    wk_codsin( i) =  ;
                             SUBSTR(dessin,  ;
                             1,  ;
                             35)
                    i = i + 1
                    SELECT 1
                    SKIP
               ENDDO
          ENDIF
          CLOSE DATABASES
          DO col_bk1b
          DO col_bk2b
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
                       'BBB',  ;
                       'BBB'
                    DO esc_indica  ;
                       WITH 2,  ;
                       'CRE',  ;
                       'BBB',  ;
                       'IGN',  ;
                       'ESC'
               CASE config_prg ==  ;
                    2
                    DO esc_indica  ;
                       WITH 1,  ;
                       'AYU',  ;
                       'BBB',  ;
                       'BBB',  ;
                       'BBB'
                    DO esc_indica  ;
                       WITH 2,  ;
                       'MOD',  ;
                       'BBB',  ;
                       'IGN',  ;
                       'ESC'
               CASE config_prg ==  ;
                    3
                    DO esc_indica  ;
                       WITH 1,  ;
                       'AYU',  ;
                       'BBB',  ;
                       'BBB',  ;
                       'BBB'
                    DO esc_indica  ;
                       WITH 2,  ;
                       'ANA',  ;
                       'BBB',  ;
                       'IGN',  ;
                       'ESC'
          ENDCASE
          DO CASE
               CASE config_prg ==  ;
                    1
                    DO creaord
               CASE config_prg ==  ;
                    2
                    DO modiord
               CASE config_prg ==  ;
                    3
                    DO anulord
          ENDCASE
     ELSE
          lin = 40
          anc = 75
          des = 1
          com = 1
          DIMENSION solic( 48)
          STORE FOPEN('presupue.txt')  ;
                TO file_handl
          FOR i = 1 TO 48
               solic( i) =  ;
                    FREAD(file_handl,  ;
                    77)
          ENDFOR
          = FCLOSE(file_handl)
          solic( 3) =  ;
               '               O R D E N   D E   R E P A R A C I O N          N§ ' +  ;
               STR(wk_numord, 8) +  ;
               '  '
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
                    dec( 12),  ;
                    tot( 12)
          DIMENSION wk_obspre(  ;
                    06)
          FOR i = 1 TO 12
               pro( i) =  ;
                  SPACE(14)
               dex( i) =  ;
                  SPACE(20)
               uni( i) = SPACE(3)
               can( i) = 0
               pre( i) = 0
               dec( i) = 0
               tot( i) = 0
               IF i <= 6
                    wk_obspre( i) =  ;
                             SPACE(45)
               ENDIF
          ENDFOR
          wk_numaux = STR(wk_numsol,  ;
                      8)
          USE SHARED st_isrep  ;
              ORDER CODIGO
          SEEK '&wk_numaux'
          wk_coddes = coddes
          wk_feccom = feccom
          wk_codcli = VAL(codent)
          wk_indori = IIF(indori ==  ;
                      'GARA', 'S',  ;
                      'N')
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
                        SUBSTR(MLINE(desace,  ;
                        i), 1,  ;
                        35)
               wk_acceso( i) =  ;
                        wk_acceso(i) +  ;
                        SPACE(35 -  ;
                        LEN(wk_acceso(i)))
               IF i <= 6
                    wk_observ( i) =  ;
                             SUBSTR(MLINE(observ,  ;
                             i),  ;
                             1,  ;
                             45)
                    wk_observ( i) =  ;
                             wk_observ(i) +  ;
                             SPACE(45 -  ;
                             LEN(wk_observ(i)))
               ENDIF
          ENDFOR
          wk_cliaux = 'C' +  ;
                      STR(wk_codcli,  ;
                      9)
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
          wk_aux = 'MONE' +  ;
                   wk_codmon
          SEEK '&wk_aux'
          wk_nommon = SUBSTR(tab_destab,  ;
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
          SELECT 2
          USE SHARED st_isint  ;
              ORDER CODIGO
          SELECT 1
          USE SHARED st_issre  ;
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
                    wk_aux2 = wk_aux +  ;
                              codsin
                    SELECT 2
                    SEEK '&wk_aux2'
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
          wk_totrep = monrep
          wk_totdes = pordes
          wk_totnet = ROUND(monrep -  ;
                      (monrep *  ;
                      pordes /  ;
                      100), 2)
          wk_totman = monman
          wk_totafe = wk_totnet +  ;
                      wk_totman
          wk_totigv = totigv
          wk_totgrl = wk_totafe +  ;
                      wk_totigv
          FOR i = 1 TO 6
               wk_obspre( i) =  ;
                        SUBSTR(MLINE(observ,  ;
                        i), 1,  ;
                        45)
               wk_obspre( i) =  ;
                        wk_obspre(i) +  ;
                        SPACE(45 -  ;
                        LEN(wk_obspre(i)))
          ENDFOR
          USE SHARED st_idpre  ;
              ORDER CODIGO
          SEEK '&wk_numaux'
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
          wk_aux = 'EMIS' +  ;
                   wk_emisor
          USE SHARED ge_tab0  ;
              ORDER codigo
          SEEK '&wk_aux'
          wk_nomemi = SUBSTR(tab_destab,  ;
                      1, 30)
          wk_aux = 'DIST' +  ;
                   wk_nomdis
          SEEK '&wk_aux'
          wk_desdis = tab_destab
          wk_aux = 'PROV' +  ;
                   wk_nomciu
          SEEK '&wk_aux'
          wk_desciu = tab_destab
          wk_aux = STR(wk_tecnic,  ;
                   9)
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
                       'CRE',  ;
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
                       'MOD',  ;
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
                       'ANA',  ;
                       'BBB',  ;
                       'IGN',  ;
                       'ESC'
          ENDCASE
          ppal2 = .T.
          DO WHILE ppal2
               wk_inkey = 0
               DO WHILE  .NOT.  ;
                  (STR(wk_inkey,  ;
                  2)$ ;
                  ' 5,24,18, 3, 1, 6,27,-2,-9-7' ;
                  )
                    wk_inkey = INKEY(0)
               ENDDO
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
                         2
                         DO CASE
                              CASE  ;
                               config_prg ==  ;
                               1
                                   DO creaord
                              CASE  ;
                               config_prg ==  ;
                               2
                                   DO modiord
                              CASE  ;
                               config_prg ==  ;
                               3
                                   DO anulord
                         ENDCASE
                    CASE wk_inkey == - ;
                         7
                         DO versol
                         ON KEY LABEL;
F8
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
ON KEY LABEL F6
ON KEY LABEL F10
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DO saca_win
RELEASE tecnico
RETURN
*
PROCEDURE creaord
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'IGN', 'ESC'
@ 14, 10 FILL TO 28, 65
@ 13, 09 CLEAR TO 28, 64
@ 13, 09 TO 28, 64
@ 13, 30 SAY  ;
  ' Creaci¢n de Orden ' COLOR  ;
  SCHEME 8
wrk_codtal = SPACE(4)
wk_ogar = IIF(wk_indori == 'GARA'  ;
          .OR. wk_indori == 'S',  ;
          'S', 'N')
wk_oest = SPACE(4)
wk_otec = 0
wk_oaux = wk_ogar
wrk_dogtia = ooserie(wk_codmar, ;
             wk_codmod, ;
             wk_numser)
@ 18, 35 SAY IIF(wk_ogar == 'S',  ;
  'EN GARANTIA   ',  ;
  'FUERA GARANTIA')
STORE SPACE(38) TO nota1, nota2,  ;
      nota3, nota4, nota5, nota6
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(DATE())
@ 17, 11 SAY 'Taller      :' GET  ;
  wrk_codtal PICTURE '@!' VALID  ;
  valtab2('TALL',wrk_codtal,35, ;
  28) WHEN colocaf6()
@ 18, 11 SAY 'Garantia    :' GET  ;
  wk_ogar PICTURE '@!' VALID  ;
  garantia(wk_ogar) WHEN wk_oaux ==  ;
  'S'
@ 19, 11 SAY 'Estado      :' GET  ;
  wk_oest PICTURE '@!' VALID  ;
  valtab2('ESOR',wk_oest,35,28)  ;
  WHEN colocaf6()
@ 20, 11 SAY 'Tecnico     :' GET  ;
  wk_otec PICTURE '999999999'  ;
  VALID tecnico(wk_otec,35) WHEN  ;
  colocaf6()
@ 21, 11 SAY 'Notas       :'
@ 22, 25 GET nota1 PICTURE '@!'
@ 23, 25 GET nota2 PICTURE '@!'
@ 24, 25 GET nota3 PICTURE '@!'
@ 25, 25 GET nota4 PICTURE '@!'
@ 26, 25 GET nota5 PICTURE '@!'
@ 27, 25 GET nota6 PICTURE '@!'
SET CURSOR ON
READ
SET CURSOR OFF
IF LASTKEY() <> 27
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'IMP', 'BBB'
     DO esc_indica WITH 2, 'GRA',  ;
        'BBB', 'IGN', 'ESC'
     wk_key = 0
     DO WHILE wk_key<>-1 .AND.  ;
        wk_key<>-6 .AND. wk_key<>- ;
        9 .AND. wk_key<>27
          wk_key = INKEY(0)
     ENDDO
     IF wk_key == -1 .OR. wk_key == - ;
        6
          DO buscaor
          DO mensa2 WITH  ;
             'Grabando  ORDEN '+ ;
             ' N§ '+STR(wk_numord,  ;
             8), 'COLO'
          USE SHARED st_iorep
          APPEND BLANK
          DO rbloquea
          REPLACE numdoc WITH  ;
                  STR(wk_numord,  ;
                  8), fecemi WITH  ;
                  DATE()
          REPLACE codemi WITH  ;
                  wk_emisor,  ;
                  codent WITH  ;
                  STR(wk_codcli,  ;
                  9)
          REPLACE indori WITH  ;
                  IIF(wk_ogar ==  ;
                  'S', 'GARA',  ;
                  'FGAR'), indest  ;
                  WITH 'V'
          REPLACE auxest WITH  ;
                  wk_oest, codmar  ;
                  WITH wk_codmar,  ;
                  codmod WITH  ;
                  wk_codmod
          REPLACE numser WITH  ;
                  wk_numser,  ;
                  codtec WITH  ;
                  STR(wk_otec,  ;
                  9)
          REPLACE codtall WITH  ;
                  wrk_codtal,  ;
                  fecest WITH  ;
                  DATE()
          IF wk_var == 1
               REPLACE numsol  ;
                       WITH  ;
                       STR(wk_numero,  ;
                       8)
          ELSE
               REPLACE numsol  ;
                       WITH  ;
                       STR(wk_numsol,  ;
                       8)
               REPLACE numpre  ;
                       WITH  ;
                       STR(wk_numero,  ;
                       8)
          ENDIF
          wk_aux = nota1 + nota2 +  ;
                   nota3 + nota4 +  ;
                   nota5 + nota6
          REPLACE observ WITH  ;
                  wk_aux
          wk_numo = wk_numord
          USE SHARED st_iredo
          APPEND BLANK
          DO rbloquea
          IF wk_var == 1
               REPLACE indodo  ;
                       WITH  ;
                       'SSE ',  ;
                       numodo  ;
                       WITH  ;
                       STR(wk_numero,  ;
                       8)
               REPLACE indddo  ;
                       WITH  ;
                       'ORD ',  ;
                       numddo  ;
                       WITH  ;
                       STR(wk_numo,  ;
                       8)
               USE SHARED  ;
                   st_isrep ORDER  ;
                   CODIGO
               wk_aux = STR(wk_numero,  ;
                        8)
               SEEK '&wk_aux'
               DO rbloquea
               REPLACE indest  ;
                       WITH 'P'
               USE
          ELSE
               REPLACE indodo  ;
                       WITH  ;
                       'PRE ',  ;
                       numodo  ;
                       WITH  ;
                       STR(wk_numero,  ;
                       8)
               REPLACE indddo  ;
                       WITH  ;
                       'ORD ',  ;
                       numddo  ;
                       WITH  ;
                       STR(wk_numo,  ;
                       8)
               USE SHARED  ;
                   st_ispre ORDER  ;
                   CODIGO
               wk_aux = STR(wk_numero,  ;
                        8)
               SEEK '&wk_aux'
               DO rbloquea
               REPLACE indest  ;
                       WITH 'P'
               USE
          ENDIF
          USE
          wk_esorde = SPACE(40)
          wk_aux = 'ESOR' +  ;
                   wk_oest
          USE SHARED ge_tab0  ;
              ORDER codigo
          SEEK '&wk_aux'
          wk_esorde = tab_destab
          USE
          USE SHARED st_mvord
          APPEND BLANK
          DO rbloquea
          REPLACE dia WITH DATE(),  ;
                  hora WITH  ;
                  TIME()
          REPLACE orden WITH  ;
                  STR(wk_numord,  ;
                  8), tecnico  ;
                  WITH  ;
                  STR(wk_otec,  ;
                  9)
          REPLACE estado WITH  ;
                  wk_oest,  ;
                  destado WITH  ;
                  wk_esorde
          USE
          DO mensa2 WITH  ;
             'Grabando  ORDEN '+ ;
             ' N§ '+STR(wk_numord,  ;
             8), 'SACA'
          IF wk_key == -1
               wk_numord = 0
          ENDIF
          KEYBOARD '{F10}' PLAIN
     ENDIF
     IF wk_key == -6
          sw_impre = 0
          DO impresora WITH  ;
             sw_impre
          IF sw_impre <> 1
               eror_imp = .T.
               RETURN
          ENDIF
          wk_print = PRINTSTATUS()
          DO WHILE  .NOT.  ;
             wk_print
               DO error2 WITH  ;
                  '** Error en Impresora. Continua ? (S/N) '
               IF LASTKEY() == 27
                    EXIT
               ENDIF
               wk_print = PRINTSTATUS()
          ENDDO
          wk_print = PRINTSTATUS()
          IF wk_print
               DO imp_ord
               wk_numord = 0
          ENDIF
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'RAC',  ;
        'BBB', 'IGN', 'ESC'
ENDIF
IF LASTKEY() == 27
     IF efecin == 1
          KEYBOARD '{ESC}'
     ELSE
          efecin = 1
     ENDIF
ENDIF
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE modiord
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'IGN', 'ESC'
@ 14, 10 FILL TO 28, 65
@ 13, 09 CLEAR TO 28, 64
@ 13, 09 TO 28, 64
@ 13, 32 SAY ' Cambio de Estado '  ;
  COLOR SCHEME 8
wk_oaux = IIF(wk_indori == 'GARA'  ;
          .OR. wk_indori == 'S',  ;
          'S', 'N')
wrk_dogtia = ooserie(wk_codmar, ;
             wk_codmod, ;
             wk_numser)
USE SHARED st_iorep ORDER CODIGO
wk_numaux = STR(wk_numord, 8)
SEEK '&wk_numaux'
wk_ogar = IIF(indori == 'GARA',  ;
          'S', 'N')
wk_oest = auxest
wrk_fecest = fecest
wk_otec = VAL(codtec)
wrk_codtal = codtall
nota1 = SUBSTR(observ, 001, 38)
nota2 = SUBSTR(observ, 039, 38)
nota3 = SUBSTR(observ, 077, 38)
nota4 = SUBSTR(observ, 115, 38)
nota5 = SUBSTR(observ, 153, 38)
nota6 = SUBSTR(observ, 191, 38)
wk_obux = 'ESOR' + wk_oest
USE SHARED ge_tab0 ORDER codigo
SEEK '&wk_obux'
@ 19, 35 SAY SUBSTR(tab_destab, 1,  ;
  28)
SEEK 'TALL' + wrk_codtal
@ 17, 35 SAY SUBSTR(tab_destab, 1,  ;
  28)
wk_obux = STR(wk_otec, 9)
USE SHARED st_itecn ORDER CODIGO
SEEK '&wk_obux'
@ 20, 35 SAY SUBSTR(noment, 1,  ;
  28)
USE
@ 18, 35 SAY IIF(wk_ogar == 'S',  ;
  'EN GARANTIA   ',  ;
  'FUERA GARANTIA')
@ 15, 11 SAY 'N§ Orden    : ' +  ;
  STR(wk_numord, 8)
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(DATE())
@ 17, 11 SAY 'Taller      :' GET  ;
  wrk_codtal PICTURE '@!' VALID  ;
  valtab2('TALL',wrk_codtal,35, ;
  28) WHEN colocaf6()
@ 18, 11 SAY 'Garantia    :' GET  ;
  wk_ogar PICTURE '!' VALID  ;
  garantia(wk_ogar) WHEN wk_oaux ==  ;
  'S'
@ 19, 11 SAY 'Estado      :' GET  ;
  wk_oest PICTURE '!!!!' VALID  ;
  valtab2('ESOR',wk_oest,35,28)  ;
  WHEN colocaf6()
@ 20, 11 SAY 'Tecnico     :' GET  ;
  wk_otec PICTURE '999999999'  ;
  VALID tecnico(wk_otec,35) WHEN  ;
  colocaf6()
@ 21, 11 SAY 'Notas       :'
@ 22, 25 GET nota1 PICTURE '@!'
@ 23, 25 GET nota2 PICTURE '@!'
@ 24, 25 GET nota3 PICTURE '@!'
@ 25, 25 GET nota4 PICTURE '@!'
@ 26, 25 GET nota5 PICTURE '@!'
@ 27, 25 GET nota6 PICTURE '@'
SET CURSOR ON
READ
SET CURSOR OFF
IF LASTKEY() <> 27
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'IMP', 'BBB'
     DO esc_indica WITH 2, 'GRA',  ;
        'BBB', 'IGN', 'ESC'
     wk_key = 0
     DO WHILE wk_key<>-1 .AND.  ;
        wk_key<>-6 .AND. wk_key<>- ;
        9 .AND. wk_key<>27
          wk_key = INKEY(0)
     ENDDO
     IF wk_key == -1 .OR. wk_key == - ;
        6
          USE SHARED st_iorep  ;
              ORDER CODIGO
          wk_numaux = STR(wk_numord,  ;
                      8)
          SEEK '&wk_numaux'
          DO rbloquea
          REPLACE indori WITH  ;
                  IIF(wk_ogar ==  ;
                  'S', 'GARA',  ;
                  'FGAR')
          REPLACE auxest WITH  ;
                  wk_oest, fecest  ;
                  WITH DATE()
          REPLACE codtec WITH  ;
                  STR(wk_otec,  ;
                  9)
          wk_aux = nota1 + nota2 +  ;
                   nota3 + nota4 +  ;
                   nota5 + nota6
          REPLACE observ WITH  ;
                  wk_aux, codtall  ;
                  WITH  ;
                  wrk_codtal
          USE
          wk_esorde = SPACE(40)
          wk_aux = 'ESOR' +  ;
                   wk_oest
          USE SHARED ge_tab0  ;
              ORDER codigo
          SEEK '&wk_aux'
          wk_esorde = tab_destab
          USE
          USE SHARED st_mvord
          APPEND BLANK
          DO rbloquea
          REPLACE dia WITH DATE(),  ;
                  hora WITH  ;
                  TIME()
          REPLACE orden WITH  ;
                  STR(wk_numord,  ;
                  8), tecnico  ;
                  WITH  ;
                  STR(wk_otec,  ;
                  9)
          REPLACE estado WITH  ;
                  wk_oest,  ;
                  destado WITH  ;
                  wk_esorde
          USE
          KEYBOARD '{F10}' PLAIN
     ENDIF
     IF wk_key == -6
          wk_print = PRINTSTATUS()
          DO WHILE  .NOT.  ;
             wk_print
               DO error2 WITH  ;
                  '** Error en Impresora. Continua ? (S/N) '
               IF LASTKEY() == 27
                    EXIT
               ENDIF
               wk_print = PRINTSTATUS()
          ENDDO
          wk_print = PRINTSTATUS()
          IF wk_print
               DO imp_ord
          ENDIF
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'RAC',  ;
        'BBB', 'IGN', 'ESC'
ENDIF
IF LASTKEY() == 27
     IF efecin == 1
          KEYBOARD '{ESC}'
     ELSE
          efecin = 1
     ENDIF
ENDIF
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE anulord
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 28, 65
@ 13, 09 CLEAR TO 28, 64
@ 13, 09 TO 28, 64
@ 13, 32 SAY ' Anular Orden '
wk_oaux = IIF(wk_indori == 'GARA'  ;
          .OR. wk_indori == 'S',  ;
          'S', 'N')
USE SHARED st_iorep ORDER CODIGO
wk_numaux = STR(wk_numord, 8)
SEEK '&wk_numaux'
wk_ogar = IIF(indori == 'GARA',  ;
          'S', 'N')
wk_oest = auxest
wrk_fecest = fecest
wk_otec = VAL(codtec)
wrk_codtal = codtall
nota1 = SUBSTR(observ, 1, 38)
nota2 = SUBSTR(observ, 39, 38)
nota3 = SUBSTR(observ, 77, 38)
nota4 = SUBSTR(observ, 115, 38)
nota5 = SUBSTR(observ, 153, 38)
nota6 = SUBSTR(observ, 191, 38)
wk_obux = 'ESOR' + wk_oest
USE SHARED ge_tab0 ORDER codigo
SEEK '&wk_obux'
@ 19, 35 SAY SUBSTR(tab_destab, 1,  ;
  28)
SEEK 'TALL' + wrk_codtal
@ 17, 35 SAY SUBSTR(tab_destab, 1,  ;
  28)
wk_obux = STR(wk_otec, 9)
USE SHARED st_itecn ORDER CODIGO
SEEK '&wk_obux'
@ 20, 35 SAY SUBSTR(noment, 1,  ;
  28)
USE
@ 18, 35 SAY IIF(wk_ogar == 'S',  ;
  'EN GARANTIA   ',  ;
  'FUERA GARANTIA')
wk_otec = tecnico
@ 15, 11 SAY 'N§ Orden    : ' +  ;
  STR(wk_numord, 8)
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(DATE())
@ 17, 11 SAY 'Taller      :' GET  ;
  wrk_codtal PICTURE '@!' VALID  ;
  valtab2('TALL',wrk_codtal,35, ;
  28) WHEN colocaf6()
@ 18, 11 SAY 'Garantia    :' GET  ;
  wk_ogar PICTURE '!' VALID  ;
  garantia(wk_ogar) WHEN wk_oaux ==  ;
  'S'
@ 19, 11 SAY 'Estado      :' GET  ;
  wk_oest PICTURE '@!' VALID  ;
  valtab2('ESOR',wk_oest,35,28)  ;
  WHEN colocaf6()
@ 20, 11 SAY 'Tecnico     :' GET  ;
  wk_otec PICTURE '999999999'  ;
  VALID tecnico(wk_otec,35) WHEN  ;
  colocaf6()
@ 21, 11 SAY 'Notas       :'
@ 22, 25 GET nota1 PICTURE '@!'
@ 23, 25 GET nota2 PICTURE '@!'
@ 24, 25 GET nota3 PICTURE '@!'
@ 25, 25 GET nota4 PICTURE '@!'
@ 26, 25 GET nota5 PICTURE '@!'
@ 27, 25 GET nota6 PICTURE '@!'
CLEAR GETS
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'GRA',  ;
   'BBB', 'IGN', 'ESC'
wk_key = 0
DO WHILE wk_key<>-1 .AND. wk_key<>- ;
   9 .AND. wk_key<>27
     wk_key = INKEY(0)
ENDDO
IF wk_key == -1
     DO error2 WITH  ;
        '** Confirma Anular Orden ? (S/N) '
     IF UPPER(CHR(LASTKEY())) ==  ;
        'S'
          USE SHARED st_iorep  ;
              ORDER CODIGO
          wk_numaux = STR(wk_numord,  ;
                      8)
          SEEK '&wk_numaux'
          sw_sn = .T.
          DO WHILE sw_sn
               IF RLOCK()
                    REPLACE indest  ;
                            WITH  ;
                            'N   '
                    UNLOCK
                    USE
                    EXIT
               ELSE
                    sw_sn = f_yesno2( ;
                            'Registro Bloquedo, Reintentar' ;
                            )
               ENDIF
          ENDDO
     ENDIF
     KEYBOARD '{F10}' PLAIN
ENDIF
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'RAC',  ;
   'BBB', 'IGN', 'ESC'
IF LASTKEY() == 27
     IF efecin == 1
          KEYBOARD '{ESC}'
     ELSE
          efecin = 1
     ENDIF
ENDIF
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE ayuda12
ON KEY LABEL F6
ON KEY LABEL F8
IF VARREAD() == 'WK_NUMERO'
     IF wk_var == 1
          wrk_origen = 'SS'
          USE SHARED st_isrep  ;
              ORDER CODIGO
          SET FILTER TO indest <> 'P'
          campoa = 'numdoc+"  "+dtoc(fecemi)+"  "+SUBSTR(NUMSER,1,12)+"  "+codent+"  "+SUBSTR(codmod,1,10)+"   "+subst(indest,1,2)'
     ELSE
          wrk_origen = 'PP'
          USE SHARED st_ispre  ;
              ORDER CODIGO
          SET FILTER TO indest <> 'P'
          campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+ooDESCLI(CODENT)+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)'
     ENDIF
     DO ayuda4 WITH campoa,  ;
        wrk_origen
     SET FILTER TO
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
IF VARREAD() == 'WRK_CODTAL'
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'TALL'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'TALLER DE REPARACION'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF VARREAD() == 'WK_OTEC'
     USE SHARED st_itecn ORDER  ;
         CODIGO
     campoa = '" "+codent+" "+noment+" "+CODTEC'
     campob = '" "+noment+" "+codent+" "+CODTEC'
     titulo = 'AYUDA DE TECNICOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
     USE
ENDIF
IF VARREAD() == 'WK_NUMORD' .AND.  ;
   config_prg > 1
     USE SHARED st_iorep ORDER  ;
         CODIGO
     wrk_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)'
     DO ayuda4 WITH campoa,  ;
        wrk_origen
     USE
ENDIF
ON KEY LABEL F6 do ayuda12
ON KEY LABEL F8 do solicitud
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
IF gar <> 'S' .AND. gar <> 'N'
     DO error2 WITH  ;
        '** Error. Ingrese [S]i ¢ [N]o. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
@ 18, 35 SAY IIF(gar == 'S',  ;
  ooserie(wk_codmar,wk_codmod, ;
  wk_numser), 'FUERA GARANTIA')
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
IF num <= wk_numaux
     DO error WITH  ;
        '** Error N§ debe ser mayor que '+ ;
        ALLTRIM(STR(wk_numaux))+ ;
        '. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION numord2
PARAMETER num
IF num == 0
     DO error WITH  ;
        '** Error N§ debe ser Ingresado. **'
     RETURN .F.
ENDIF
wk_clave = STR(num, 8)
USE SHARED st_iorep ORDER CODIGO
SEEK '&wk_clave'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** N§ Orden Reparacion NO EXISTE. **'
     RETURN .F.
ENDIF
IF indest = 'N   '
     USE
     DO error WITH  ;
        '** N§ Orden Reparacion esta NULA. **'
     RETURN .F.
ENDIF
IF indest = 'F   '
     USE
     DO error WITH  ;
        '** N§ Orden Reparacion FACTURA. **'
     RETURN .F.
ENDIF
IF indest = 'B   '
     USE
     DO error WITH  ;
        '** N§ Orden Reparacion BOLETEADA. **'
     RETURN .F.
ENDIF
IF indest = 'C   '
     USE
     DO error WITH  ;
        '** N§ Orden Reparacion CERRADA. **'
     RETURN .F.
ENDIF
USE
RETURN .T.
*
FUNCTION ordnul
PARAMETER cod, var
cod = STR(cod, 8)
IF LEN(TRIM(cod)) == 0
     DO error WITH  ;
        '** Item debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
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
             '** Error Solicitud esta Anulada. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
     IF SUBSTR(indest, 1, 1) <>  ;
        'V'
          USE
          DO error WITH  ;
             '** Error Solicitud en Procesos. **'
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
        'N   '
          USE
          DO error WITH  ;
             '** Error Presupuesto esta Anulado. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
     IF SUBSTR(indest, 1, 1) <>  ;
        'V'
          USE
          DO error WITH  ;
             '** Error Presupuesto en Procesos. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
     tecnico = codtec
ENDIF
USE
DO sacaf6
RETURN .T.
*
PROCEDURE imp_ord
SET PRINTER ON
SET DEVICE TO PRINTER
@ PROW(), PCOL() SAY CHR(15)
@ PROW(), PCOL() SAY CHR(27) +  ;
  'C' + CHR(33)
SET CONSOLE OFF
clave = wk_codmar + wk_codmod
USE SHARED st_imode ORDER CODIGO
SEEK '&clave'
wk_desmod = nommod
USE SHARED ge_tab0 ORDER codigo
SEEK 'MARC' + wk_codmar
wk_desmar = tab_destab
USE
wk_numaux = STR(wk_numord, 8)
USE SHARED st_itecn ORDER CODIGO
SEEK STR(wk_otec, 9)
wrk_destec = noment
USE SHARED st_iorep ORDER CODIGO
SEEK '&wk_numaux'
wk_numsol = numsol
tit_client = wk_noment
tit_telef1 = wk_numte1
tit_codigo = STR(wk_codcli, 9)
tit_direcc = wk_nomcal
tit_feccom = wk_feccom
tit_tipo = IIF(wk_indori ==  ;
           'GARA', 'EN GARANTIA',  ;
           'FUERA GARANTIA')
tit_tit1 = 'DESTINO : '
tit_destin = IIF(wk_coddes == 'R',  ;
             'REPARACION',  ;
             'DOMICILIO')
wk_fecha = DTOC(DATE())
tit_fechho = SUBSTR(wk_fecha, 1,  ;
             2) + SPACE(3) +  ;
             SUBSTR(wk_fecha, 4,  ;
             2) + SPACE(3) +  ;
             SUBSTR(wk_fecha, 7,  ;
             2)
tit_tit2 = 'SINTOMAS :'
tit_tit3 = 'ACCESORIOS :'
tit_tit4 = 'OBSERVACIONES :'
tit_tit5 = 'O R D E N   D E   R E P A R A C I O N '
tit_tit6 = 'NOTAS :'
tit_fecha = wk_fecha + ' - ' +  ;
            TIME()
tit_eminro = wk_emisor + ' N§: ' +  ;
             wk_numaux
tit_nrosol = 'S/S  N§: ' +  ;
             wk_numsol
tit_codmod = wk_codmod
tit_numser = wk_numser
tit_desmar = wk_desmar
tit_desmod = wk_desmod
DIMENSION wk_sin( 9), wk_acc( 9),  ;
          wk_obs( 3)
wk_sin( 1) = wk_codsin(1) +  ;
      SUBSTR(wk_codsin(2), 01,  ;
      30)
wk_sin( 2) = SUBSTR(wk_codsin(02),  ;
      31, 05) + wk_codsin(03) +  ;
      SUBSTR(wk_codsin(04), 01,  ;
      25)
wk_sin( 3) = SUBSTR(wk_codsin(04),  ;
      26, 10) + wk_codsin(05) +  ;
      SUBSTR(wk_codsin(06), 01,  ;
      20)
wk_sin( 4) = SUBSTR(wk_codsin(06),  ;
      21, 15) + wk_codsin(07) +  ;
      SUBSTR(wk_codsin(08), 01,  ;
      15)
wk_sin( 5) = SUBSTR(wk_codsin(08),  ;
      16, 20) + wk_codsin(09) +  ;
      SUBSTR(wk_codsin(10), 01,  ;
      10)
wk_sin( 6) = SUBSTR(wk_codsin(10),  ;
      11, 25) + wk_codsin(11) +  ;
      SUBSTR(wk_codsin(12), 01,  ;
      05)
wk_sin( 7) = SUBSTR(wk_codsin(12),  ;
      06, 30) + wk_codsin(13)
wk_sin( 8) = wk_codsin(14) +  ;
      SUBSTR(wk_codsin(15), 01,  ;
      30)
wk_sin( 9) = SUBSTR(wk_codsin(15),  ;
      31, 05)
wk_acc( 1) = wk_acceso(1) +  ;
      SUBSTR(wk_acceso(2), 01,  ;
      30)
wk_acc( 2) = SUBSTR(wk_acceso(02),  ;
      31, 05) + wk_acceso(03) +  ;
      SUBSTR(wk_acceso(04), 01,  ;
      25)
wk_acc( 3) = SUBSTR(wk_acceso(04),  ;
      26, 10) + wk_acceso(05) +  ;
      SUBSTR(wk_acceso(06), 01,  ;
      20)
wk_acc( 4) = SUBSTR(wk_acceso(06),  ;
      21, 15) + wk_acceso(07) +  ;
      SUBSTR(wk_acceso(08), 01,  ;
      15)
wk_acc( 5) = SUBSTR(wk_acceso(08),  ;
      16, 20) + wk_acceso(09) +  ;
      SUBSTR(wk_acceso(10), 01,  ;
      10)
wk_acc( 6) = SUBSTR(wk_acceso(10),  ;
      11, 25) + wk_acceso(11) +  ;
      SUBSTR(wk_acceso(12), 01,  ;
      05)
wk_acc( 7) = SUBSTR(wk_acceso(12),  ;
      06, 30) + wk_acceso(13)
wk_acc( 8) = wk_acceso(14) +  ;
      SUBSTR(wk_acceso(15), 01,  ;
      30)
wk_acc( 9) = SUBSTR(wk_acceso(15),  ;
      31, 05)
o = 1
FOR i = 1 TO 3
     wk_obs( i) = wk_observ(o) +  ;
           wk_observ(o + 1)
     o = o + 2
ENDFOR
?? CHR(15)
@ 02, 70 - (LEN(tit_tit5) / 2)  ;
  SAY tit_tit5
@ 02, 115 SAY tit_eminro
@ 03, 115 SAY tit_nrosol
@ 04, 115 SAY tit_fecha
@ 05, 015 SAY tit_client
@ 05, 075 SAY tit_codigo
@ 06, 015 SAY tit_direcc
@ 06, 115 SAY tit_feccom
@ 07, 105 SAY tit_telef1
@ 09, 105 SAY tit_tipo
@ 10, 003 SAY tit_desmod
@ 10, 030 SAY tit_codmod
@ 10, 050 SAY tit_desmar
@ 10, 075 SAY tit_numser
@ 10, 105 SAY wrk_dogtia
@ 11, 002 SAY tit_tit2
@ 11, 002 SAY tit_tit2
@ 11, 070 SAY tit_tit3
@ 11, 070 SAY tit_tit3
FOR lin = 1 TO 9
     @ 11 + lin, 003 SAY  ;
       wk_sin(lin)
     @ 11 + lin, 071 SAY  ;
       wk_acc(lin)
ENDFOR
@ 21, 002 SAY tit_tit6
@ 21, 002 SAY tit_tit6
@ 21, 080 SAY 'Tecnico :'
@ 21, 080 SAY 'Tecnico :'
@ 21, 090 SAY STR(wk_otec, 9)
@ 21, 103 SAY SUBSTR(wrk_destec,  ;
  1, 20)
nota1 = SUBSTR(observ, 001, 76)
nota2 = SUBSTR(observ, 077, 76)
nota3 = SUBSTR(observ, 153, 76)
@ 22, 003 SAY nota1
@ 23, 003 SAY nota2
@ 24, 003 SAY nota3
@ 25, 002 SAY tit_tit4
@ 25, 002 SAY tit_tit4
@ 26, 003 SAY SUBSTR(wk_obs(1), 1,  ;
  80)
@ 27, 003 SAY SUBSTR(wk_obs(1),  ;
  81, 10) + SUBSTR(wk_obs(2), 1,  ;
  70)
@ 28, 003 SAY SUBSTR(wk_obs(2),  ;
  71, 20) + SUBSTR(wk_obs(3), 1,  ;
  60)
@ 28, 097 SAY tit_fechho
@ 29, 003 SAY SUBSTR(wk_obs(3),  ;
  61, 20)
?? CHR(15)
EJECT
wk_numord = wk_numord + 1
USE
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
RETURN
*
PROCEDURE buscaor
USE SHARED st_iparg
DO rbloquea
REPLACE sys_numord WITH  ;
        (sys_numord + 1)
wk_numord = sys_numord
UNLOCK
USE
RETURN
SELECT 13
USE SHARED st_iorep AGAIN ORDER  ;
    CODIGO
GOTO BOTTOM
wk_numaux = VAL(numdoc)
wk_numord = wk_numaux + 1
SELECT 13
USE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

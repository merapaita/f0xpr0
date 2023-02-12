*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
wrk_progra = PROGRAM()
titu1 = ' CONSULTA '
titu2 = ' ORDEN DE REPARACION '
DO crea_win
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
USE
SELECT 1
USE ST_IOREP ORDER CODIGO
SELECT 2
USE ST_ISREP ORDER CODIGO
SELECT 3
USE ST_ISERI ORDER SER_CODMAR
SELECT 4
USE GE_TAB0 ORDER CODIGO
SELECT 5
USE ST_ICLPR ORDER CODIGO
SELECT 6
USE ST_IMODE ORDER CODIGO
SELECT 7
USE ST_ISPRE ORDER CODIGO
SELECT 8
USE ST_IDPRE ORDER CODIGO
SELECT 9
USE ST_MVORD ORDER CODIGO
SELECT 10
USE ST_ITECN ORDER CODIGO
SELECT 11
USE GC_PRO00 ORDER CODIGO
SELECT 12
USE GC_HVE00 ORDER CODIGO
SELECT 13
USE GC_ALM00 ORDER CODIGO
SELECT 14
USE GC_CMV00 ORDER CODIGO
SELECT 15
USE ST_ISINT ORDER CODIGO
SELECT 16
USE ST_ISSRE ORDER CODIGO
w_tipcam = ootc2(DATE(),'SOL ', ;
           'DOL ','2')
wk_numord = 0
STORE SPACE(30) TO wk_destia
DO WHILE .T.
     DIMENSION solic( 1)
     lin = 40
     anc = 75
     des = 1
     com = 1
     RESTORE SCREEN FROM wk_panta
     SET DISPLAY TO VGA25
     ACTIVATE SCREEN
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO 17, 76
     ZOOM WINDOW indicar NORM  ;
          FROM 20, 0 TO 23, 76
     ACTIVATE WINDOW trabajo
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     efecin = 1
     STORE 0 TO veces, wk_otec,  ;
           wk_numord, w_numero,  ;
           wk_numso, wk_numte1,  ;
           wk_numte2
     STORE ' ' TO wk_ogar,  ;
           wk_oest
     STORE SPACE(4) TO wk_indori
     STORE SPACE(30) TO wk_nom1,  ;
           wk_nom2, nota1, nota2,  ;
           nota3, nota4, nota5,  ;
           nota6
     STORE {} TO w_fecemi
     wk_var = 1
     @ 8, 20 SAY  ;
       'N£mero Orden : ' GET  ;
       w_numero PICTURE  ;
       '99999999' VALID  ;
       ordnul(w_numero,wk_var)  ;
       WHEN colocaf6()
     SET CURSOR ON
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF w_numero = 0
          DO error WITH  ;
             '***** No se aceptan ceros *****'
          LOOP
     ENDIF
     DO carga
     SET DISPLAY TO VGA50
     DO esc_indica WITH 1, 'AYU',  ;
        'OOA', 'OTR', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     wk_inkey = 0
     DO WHILE  .NOT.  ;
        (STR(wk_inkey, 2)$'27')
          wk_inkey = INKEY(0,  ;
                     'H')
          DO CASE
               CASE wk_inkey ==  ;
                    133
                    w_numero = w_numero +  ;
                               1
                    DO carga
               CASE wk_inkey ==  ;
                    134
                    w_numero = w_numero -  ;
                               1
                    DO carga
               CASE wk_inkey ==  ;
                    27 .OR.  ;
                    wk_inkey == - ;
                    9
                    EXIT
               CASE wk_inkey == - ;
                    7
                    DO informa
                    ON KEY LABEL F8
               CASE wk_inkey == - ;
                    3
                    DO veorden
                    ON KEY LABEL F3
               CASE wk_inkey == - ;
                    4
                    DO veestado
               CASE wk_inkey == - ;
                    8
                    DO cuenta
               OTHERWISE
                    DO mueve3  ;
                       WITH  ;
                       CHR(wk_inkey)
          ENDCASE
     ENDDO
ENDDO
CLOSE DATABASES
ON KEY
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DO saca_win
RELEASE wk_pantax
RETURN
*
PROCEDURE carga
SELECT st_iorep
wk_numaux = STR(w_numero, 8)
SEEK '&wk_numaux'
IF  .NOT. FOUND()
     DO error WITH  ;
        '***** Orden de Reparaci¢n No Existe *****'
     RETURN
ENDIF
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 42, 76
ZOOM WINDOW indicar NORM FROM 45,  ;
     0 TO 48, 76
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
lin = 40
anc = 75
des = 1
com = 1
DIMENSION solic( 48)
STORE FOPEN('solicitu.txt') TO  ;
      file_handl
FOR i = 1 TO 48
     solic( i) = FREAD(file_handl,  ;
          77)
ENDFOR
= FCLOSE(file_handl)
solic( 3) =  ;
     '               O R D E N   D E   R E P A R A C I O N          N§ ' +  ;
     STR(w_numero, 8) + '  '
DIMENSION wk_codsin( 15)
DIMENSION wk_acceso( 15)
DIMENSION wk_observ( 06)
wk_numaux = STR(w_numero, 8)
SELECT st_isrep
seek '&wk_numaux'
wk_fecemi = fecemi
wk_numso = VAL(numdoc)
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
              i), 1, 35)
     wk_acceso( i) = wk_acceso(i) +  ;
              SPACE(35 -  ;
              LEN(wk_acceso(i)))
     IF i <= 6
          wk_observ( i) =  ;
                   SUBSTR(MLINE(observ,  ;
                   i), 1, 45)
          wk_observ( i) =  ;
                   wk_observ(i) +  ;
                   SPACE(45 -  ;
                   LEN(wk_observ(i)))
     ENDIF
ENDFOR
wk_cliaux = 'C' + STR(wk_codcli,  ;
            9)
SELECT st_iclpr
SEEK '&wk_cliaux'
wk_noment = noment
wk_nomcal = nomcal
wk_nomdis = nomdis
wk_nomciu = nomciu
wk_numte1 = numte1
wk_numte2 = numte2
wk_aux = wk_codmar + wk_codmod
SELECT st_imode
SEEK '&wk_aux'
wk_aux = codcla
SELECT st_isint
SELECT st_issre
SEEK '&wk_numaux'
i = 1
DO WHILE  .NOT. EOF() .AND.  ;
   numdoc==wk_numaux
     wk_aux2 = wk_aux + codsin
     SELECT st_issre
     SEEK '&wk_aux2'
     wk_codsin( i) =  ;
              SUBSTR(dessin, 1,  ;
              35)
     i = i + 1
     SELECT st_isint
     SKIP
ENDDO
STORE SPACE(20) TO wk_doga,  ;
      wk_prov, wk_fevt, wk_fecg
IF wk_indori = 'GARA' .OR.  ;
   wk_indori = 'GREC'
     SELECT st_iseri
     SEEK wk_codmar + wk_codmod +  ;
          wk_numser
     wk_prov = 'Proveedor:' +  ;
               ALLTRIM(codent)
     wk_doga = 'Doc.Garan:' +  ;
               ALLTRIM(docgar)
     wk_fevt = 'Fecha Vta:' +  ;
               DTOC(fecvta)
     wk_fecg = 'Fecha Fin:' +  ;
               DTOC(fecgar)
ENDIF
DO col_bk1b
DO col_bk2b
FOR i = des TO (lin + des - 1)
     @ i - des, 0 SAY  ;
       SUBSTR(solic(i), com,  ;
       anc)
ENDFOR
RETURN
DO CASE
     CASE LASTKEY() == 27 .AND.  ;
          efecin == 1
          CLEAR GETS
          ppal = .F.
          LOOP
     CASE LASTKEY() == 27 .AND.  ;
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
            GET wk_numord PICTURE  ;
            '99999999' VALID  ;
            numord(wk_numord)
          SET CURSOR ON
          READ
          SET CURSOR OFF
          IF LASTKEY() == 27  ;
             .AND. efecin == 2
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
     w_numero = 0
     wk_var = 0
     wk_var = 2
     @ 8, 20 SAY  ;
       'N£mero Presupuesto : '  ;
       GET w_numero PICTURE  ;
       '99999999' VALID  ;
       ordnul(w_numero,wk_var)  ;
       WHEN colocaf6()
     SET CURSOR ON
     READ
     SET CURSOR OFF
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               CLEAR GETS
               ppal = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               CLEAR GETS
               LOOP
     ENDCASE
ENDIF
DO CASE
     CASE LASTKEY() == 27 .AND.  ;
          efecin == 1
          ppal = .F.
          LOOP
     CASE LASTKEY() == 27 .AND.  ;
          efecin == 2
          LOOP
ENDCASE
IF config_prg == 2
     USE SHARED st_iorep ORDER  ;
         CODIGO
     wk_numaux = STR(wk_numord,  ;
                 8)
     SEEK '&wk_numaux'
     IF numpre <> SPACE(8)
          w_numero = VAL(numpre)
          wk_var = 2
     ELSE
          w_numero = VAL(numsol)
          wk_var = 1
     ENDIF
ENDIF
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
SET DISPLAY TO VGA50
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 42, 76
ZOOM WINDOW indicar NORM FROM 45,  ;
     0 TO 48, 76
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
     IF config_prg == 2
          ON KEY LABEL f8 do veorden
          solic( 3) =  ;
               '               O R D E N   D E   R E P A R A C I O N          N§ ' +  ;
               STR(wk_numero, 8) +  ;
               '  '
     ENDIF
     DIMENSION wk_codsin( 15)
     DIMENSION wk_acceso( 15)
     DIMENSION wk_observ( 06)
     wk_numaux = STR(w_numero, 8)
     USE SHARED st_isrep ORDER  ;
         CODIGO
     seek '&wk_numaux'
     wk_fecemi = fecemi
     wk_numso = VAL(numdoc)
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
                   i), 1, 35)
          wk_acceso( i) =  ;
                   wk_acceso(i) +  ;
                   SPACE(35 -  ;
                   LEN(wk_acceso(i)))
          IF i <= 6
               wk_observ( i) =  ;
                        SUBSTR(MLINE(observ,  ;
                        i), 1,  ;
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
     USE SHARED st_iclpr ORDER  ;
         CODIGO
     SEEK '&wk_cliaux'
     wk_noment = noment
     wk_nomcal = nomcal
     wk_nomdis = nomdis
     wk_nomciu = nomciu
     wk_numte1 = numte1
     wk_numte2 = numte2
     wk_aux = wk_codmar +  ;
              wk_codmod
     USE SHARED st_imode ORDER  ;
         CODIGO
     SEEK '&wk_aux'
     wk_aux = codcla
     USE SHARED st_isint ORDER  ;
         CODIGO
     USE SHARED st_issre ORDER  ;
         CODIGO
     SEEK '&wk_numaux'
     i = 1
     DO WHILE  .NOT. EOF() .AND.  ;
        numdoc==wk_numaux
          wk_aux2 = wk_aux +  ;
                    codsin
          SELECT 2
          SEEK '&wk_aux2'
          wk_codsin( i) =  ;
                   SUBSTR(dessin,  ;
                   1, 35)
          i = i + 1
          SELECT 1
          SKIP
     ENDDO
     STORE SPACE(20) TO wk_doga,  ;
           wk_prov, wk_fevt,  ;
           wk_fecg
     IF wk_indori = 'GARA' .OR.  ;
        wk_indori = 'GREC'
          SELECT st_iseri
          SEEK wk_codmar +  ;
               wk_codmod +  ;
               wk_numser
          wk_prov = 'Proveedor:' +  ;
                    ALLTRIM(codent)
          wk_doga = 'Doc.Garan:' +  ;
                    ALLTRIM(docgar)
          wk_fevt = 'Fecha Vta:' +  ;
                    DTOC(fecvta)
          wk_fecg = 'Fecha Fin:' +  ;
                    DTOC(fecgar)
     ENDIF
     CLOSE DATABASES
     DO col_bk1b
     DO col_bk2b
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
     DO CASE
          CASE config_prg == 1
               DO esc_indica WITH  ;
                  1, 'AYU', 'OOA',  ;
                  'OTR', 'BBB'
               DO esc_indica WITH  ;
                  2, 'BBB', 'BBB',  ;
                  'IGN', 'ESC'
          CASE config_prg == 2
               DO esc_indica WITH  ;
                  1, 'AYU', 'OOA',  ;
                  'OTR', 'BBB'
               DO esc_indica WITH  ;
                  2, 'OOR', 'OOE',  ;
                  'IGN', 'ESC'
          CASE config_prg == 3
               DO esc_indica WITH  ;
                  1, 'AYU', 'OOA',  ;
                  'OTR', 'BBB'
               DO esc_indica WITH  ;
                  2, 'BBB', 'BBB',  ;
                  'IGN', 'ESC'
     ENDCASE
     ppal2 = .T.
     DO WHILE ppal2
          wk_inkey = 0
          IF config_prg = 2
               DO WHILE  .NOT.  ;
                  (STR(wk_inkey,  ;
                  2)$ ;
                  ' 5,24,18, 3, 1, 6,27,-9-8-3-4-8' ;
                  )
                    wk_inkey = INKEY(0)
               ENDDO
          ELSE
               DO WHILE  .NOT.  ;
                  (STR(wk_inkey,  ;
                  2)$ ;
                  ' 5,24,18, 3, 1, 6,27,-9-8' ;
                  )
                    wk_inkey = INKEY(0)
               ENDDO
          ENDIF
          DO CASE
               CASE wk_inkey == - ;
                    3
                    DO veorden
                    ON KEY LABEL f3
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
                         ppal = .F.
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
     DIMENSION solic( 48)
     STORE FOPEN('presupue.txt')  ;
           TO file_handl
     FOR i = 1 TO 48
          solic( i) =  ;
               FREAD(file_handl,  ;
               77)
     ENDFOR
     = FCLOSE(file_handl)
     IF config_prg == 2
          ON KEY LABEL f8 do veorden
          solic( 3) =  ;
               '               O R D E N   D E   R E P A R A C I O N          N§ ' +  ;
               STR(wk_numord, 8) +  ;
               '  '
     ENDIF
     wk_numaux = STR(w_numero, 8)
     SELECT st_ispre
     SEEK '&wk_numaux'
     wk_numsol = VAL(numsol)
     wk_numso = VAL(numsol)
     DIMENSION wk_codsin( 15)
     DIMENSION wk_acceso( 15)
     DIMENSION wk_observ( 06)
     DIMENSION pro( 12), dex( 12),  ;
               uni( 12), can( 12),  ;
               pre( 12), dec( 12),  ;
               tot( 12), sto(  ;
               12)
     DIMENSION wk_obspre( 06)
     FOR i = 1 TO 12
          pro( i) = SPACE(14)
          dex( i) = SPACE(20)
          uni( i) = SPACE(3)
          sto( i) = 0
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
     SELECT st_isrep
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
                   SUBSTR(MLINE(desace,  ;
                   i), 1, 35)
          wk_acceso( i) =  ;
                   wk_acceso(i) +  ;
                   SPACE(35 -  ;
                   LEN(wk_acceso(i)))
          IF i <= 6
               wk_observ( i) =  ;
                        SUBSTR(MLINE(observ,  ;
                        i), 1,  ;
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
     SELECT st_iclpr
     SEEK '&wk_cliaux'
     wk_noment = noment
     wk_nomcal = nomcal
     wk_nomdis = nomdis
     wk_nomciu = nomciu
     wk_numte1 = numte1
     wk_numte2 = numte2
     wk_aux = 'MARC' + wk_codmar
     SELECT ge_tab0
     SEEK '&wk_aux'
     wk_nommar = SUBSTR(tab_destab,  ;
                 1, 30)
     wk_aux = 'MONE' + wk_codmon
     SEEK '&wk_aux'
     wk_nommon = SUBSTR(tab_destab,  ;
                 1, 30)
     wk_aux = wk_codmar +  ;
              wk_codmod
     SELECT st_imode
     SEEK '&wk_aux'
     wk_nommod = SUBSTR(nommod, 1,  ;
                 30)
     wk_aux = wk_codmar +  ;
              wk_codmod
     SELECT st_imode
     SEEK '&wk_aux'
     wk_aux = codcla
     SELECT st_isint
     SELECT st_issre
     wk_numaux = STR(wk_numsol,  ;
                 8)
     SEEK '&wk_numaux'
     i = 1
     IF FOUND()
          DO WHILE  .NOT. EOF()  ;
             .AND. numdoc== ;
             wk_numaux
               wk_aux2 = wk_aux +  ;
                         codsin
               SELECT 2
               SEEK '&wk_aux2'
               wk_codsin( i) =  ;
                        SUBSTR(dessin,  ;
                        1, 35)
               i = i + 1
               SELECT 1
               SKIP
          ENDDO
     ENDIF
     wk_numaux = STR(w_numero, 8)
     SELECT st_ispre
     SEEK '&wk_numaux'
     wk_fecemi = fecemi
     wk_fecven = fecven
     wk_emisor = codemi
     wk_indori = indori
     wk_tecnic = VAL(codtec)
     wk_numsol = VAL(numsol)
     wk_numso = VAL(numsol)
     wk_totrep = monrep
     wk_totdes = pordes
     wk_totnet = ROUND(monrep -  ;
                 (monrep * pordes /  ;
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
                   i), 1, 45)
          wk_obspre( i) =  ;
                   wk_obspre(i) +  ;
                   SPACE(45 -  ;
                   LEN(wk_obspre(i)))
     ENDFOR
     SELECT st_idpre
     SEEK '&wk_numaux'
     SELECT gc_alm00
     SELECT gc_pro00
     SELECT 1
     i = 1
     DO WHILE numdoc==wk_numaux
          wk_aux = codpro
          pro( i) = codpro
          can( i) = canpro
          pre( i) = valpro
          dec( i) = pordes
          tot( i) = totite
          SELECT 3
          seek '&wk_aux'
          sto( i) = alm_stkfis
          SELECT 2
          SEEK '&wk_aux'
          dex( i) =  ;
             SUBSTR(pro_descri, 1,  ;
             20)
          uni( i) =  ;
             SUBSTR(pro_unimed, 1,  ;
             03)
          SELECT 1
          i = i + 1
          SKIP
     ENDDO
     wk_aux = 'EMIS' + wk_emisor
     SELECT ge_tab0
     SEEK '&wk_aux'
     wk_nomemi = SUBSTR(tab_destab,  ;
                 1, 30)
     SEEK 'INGA' + wk_indori
     wk_destia = SUBSTR(tab_destab,  ;
                 1, 30)
     wk_aux = STR(wk_tecnic, 9)
     wk_aux = 'DIST' + wk_nomdis
     SEEK '&wk_aux'
     wk_desdis = tab_destab
     wk_aux = 'PROV' + wk_nomciu
     SEEK '&wk_aux'
     wk_desciu = tab_destab
     SELECT st_itecn
     SEEK '&wk_aux'
     wk_nomtec = noment
     CLOSE DATABASES
     DO col_bk1c
     DO col_bk2c
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
     DO CASE
          CASE config_prg == 1
               DO esc_indica WITH  ;
                  1, 'AYU', 'OOA',  ;
                  'OTR', 'BBB'
               DO esc_indica WITH  ;
                  2, 'BBB', 'BBB',  ;
                  'IGN', 'ESC'
          CASE config_prg == 2
               DO esc_indica WITH  ;
                  1, 'AYU', 'OOA',  ;
                  'OTR', 'BBB'
               DO esc_indica WITH  ;
                  2, 'OOR', 'BBB',  ;
                  'IGN', 'ESC'
          CASE config_prg == 3
               DO esc_indica WITH  ;
                  1, 'AYU', 'OOA',  ;
                  'OTR', 'BBB'
               DO esc_indica WITH  ;
                  2, 'BBB', 'BBB',  ;
                  'IGN', 'ESC'
     ENDCASE
     ppal2 = .T.
     DO WHILE ppal2
          wk_inkey = 0
          IF config_prg = 2
               DO WHILE  .NOT.  ;
                  (STR(wk_inkey,  ;
                  2)$ ;
                  ' 5,24,18, 3, 1, 6,27,-3-9-7-8-4' ;
                  )
                    wk_inkey = INKEY(0)
               ENDDO
          ELSE
               DO WHILE  .NOT.  ;
                  (STR(wk_inkey,  ;
                  2)$ ;
                  ' 5,24,18, 3, 1, 6,27,-9-7-8-4' ;
                  )
                    wk_inkey = INKEY(0)
               ENDDO
          ENDIF
          DO CASE
               CASE wk_inkey ==  ;
                    27 .OR.  ;
                    wk_inkey == - ;
                    9
                    IF wk_inkey ==  ;
                       27
                         ppal = .F.
                    ENDIF
                    ppal2 = .F.
                    LOOP
               CASE wk_inkey == - ;
                    7
                    DO versol
                    ON KEY LABEL F8
               CASE wk_inkey == - ;
                    3
                    DO veorden
                    ON KEY LABEL F3
               CASE wk_inkey == - ;
                    4
                    DO veestado
               CASE wk_inkey == - ;
                    8
                    DO cuenta
               OTHERWISE
                    DO mueve3  ;
                       WITH  ;
                       CHR(wk_inkey)
          ENDCASE
     ENDDO
ENDIF
SET DISPLAY TO VGA25
ACTIVATE SCREEN
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
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
DO saca_win
RELEASE wk_pantax
RETURN
*
PROCEDURE ayuda12
ON KEY LABEL F6
ON KEY LABEL F8
IF VARREAD() == 'w_numero'
     IF wk_var == 1
          SELE st_isrep ORDER  ;
               CODIGO SHARE         
          wrk_origen = 'SS'
     ELSE
          USE SHARED st_ispre  ;
              ORDER CODIGO
          wrk_origen = 'PR'
     ENDIF
     campoa = 'numdoc+"  "+dtoc(fecemi)+"  "+codemi+"  "+codent+"  "+codmar+"  "+codmod+"   "+subst(indest,1,2)'
     DO ayuda7 WITH campoa,  ;
        wrk_origen
     USE
ENDIF
IF VARREAD() == 'WK_OEST'
     CLOSE DATABASES
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
     CLOSE DATABASES
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
     CLOSE DATABASES
     USE SHARED st_iorep ORDER  ;
         CODIGO
     wrk_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+numsol+" "+SUBSTR(NUMSER,1,12)+" "+codent+" "+codmod+" "+subst(indest,1,2)'
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
IF gar <> 'S' .AND. gar <> 'N'
     DO error2 WITH  ;
        '** Error. Ingrese [S]i ¢ [N]o. **'
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
        '** N§ Orden Reparaci¢n NO EXISTE. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'N'
     USE
     DO error2 WITH  ;
        '** N§ Orden Reparaci¢n esta NULA. **'
     KEYBOARD '{CTRL+Y}' PLAIN
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
     SELECT st_isrep
     SEEK '&cod'
     IF  .NOT. FOUND()
          DO error WITH  ;
             '** Error Solicitud NO EXISTE. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
     IF SUBSTR(indest, 1, 1) ==  ;
        'N'
          DO error WITH  ;
             '** Error Solicitud esta Anulada. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
ELSE
     SELECT st_ispre
     SEEK '&cod'
     IF  .NOT. FOUND()
          DO error WITH  ;
             '** Error Presupuesto NO EXISTE. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
     IF SUBSTR(indest, 1, 1) ==  ;
        'N'
          DO error WITH  ;
             '** Error Presupuesto esta Anulado. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
ENDIF
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
veces = veces + 1
IF veces = 1
     SELECT st_iorep
     wk_numaux = STR(w_numero, 8)
     SEEK '&wk_numaux'
     wk_ogar = indori
     wk_oest = auxest
     wk_otec = VAL(codtec)
     w_fecemi = fecemi
     nota1 = SUBSTR(observ, 1,  ;
             38)
     nota2 = SUBSTR(observ, 39,  ;
             38)
     nota3 = SUBSTR(observ, 77,  ;
             38)
     nota4 = SUBSTR(observ, 115,  ;
             38)
     nota5 = SUBSTR(observ, 153,  ;
             38)
     nota6 = SUBSTR(observ, 191,  ;
             38)
     wk_obux = 'ESOR' + wk_oest
     SELECT ge_tab0
     SEEK '&wk_obux'
     wk_nom1 = SUBSTR(tab_destab,  ;
               1, 28)
     SEEK 'INGA' + wk_ogar
     wk_destia = tab_destab
     wk_obux = STR(wk_otec, 9)
     SELECT st_itecn
     SEEK '&wk_obux'
     wk_nom2 = SUBSTR(noment, 1,  ;
               28)
ENDIF
@ 18, 35 SAY wk_nom1
@ 19, 35 SAY wk_nom2
@ 17, 35 SAY wk_destia
@ 15, 11 SAY 'N§ Orden    : ' +  ;
  STR(w_numero, 8)
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(w_fecemi)
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
SELECT st_mvord
SET ORDER TO eor_nroord
wk_clave1 = STR(w_numero, 8)
SET NEAR ON
SEEK '&wk_clave1'
SET NEAR OFF
campox = 'dtoc(dia)+" "+hora+" "+tecnico+" "+estado+" "+destado'
DEFINE WINDOW ayu4 FROM 14, 3 TO  ;
       15, 74 SHADOW
wk_color = color5 + ',,,,,' +  ;
           SUBSTR(color5, AT(',',  ;
           color5) + 1)
browse field cer = " " :H="", uno = &campox;
:H="  Fecha  Hora       T‚cnico Situaci¢n     Descripci¢n ";
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
KEYBOARD '{ENTER}'
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
SELECT st_iorep
SEEK STR(w_numero, 8)
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
w_totdes = totdes
w_flete = flete
SELECT ge_tab0
wrk_varbus = '"IGV " + "IGV "'
SEEK &wrk_varbus
IF FOUND()
     wk_igv = tab_factor
     wrk_facigv = tab_factor /  ;
                  100
ELSE
     DO error WITH  ;
        '**No Definido el IGV**'
     RETURN
ENDIF
w_facigv = wrk_facigv
SELECT gc_hve00
SET ORDER TO nrdore
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
  'Subtotal...........:' +  ;
  SPACE(1) + TRANSFORM(s_total,  ;
  '999,999,999.99')
@ p + 4, 20 SAY  ;
  'Descuento..........:' +  ;
  SPACE(1) + TRANSFORM(s_descue,  ;
  '999,999,999.99')
@ p + 5, 20 SAY  ;
  'Total Pagos........:' +  ;
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
                  SUBSTR(w_indori,  ;
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
                       w_indori =  ;
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
PROCEDURE acuenta
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW indicar
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 28, 65
@ 13, 09 CLEAR TO 27, 64
@ 13, 09 TO 27, 64
@ 13, 23 SAY  ;
  'Pagos a Cuenta y Saldo' COLOR  ;
  SCHEME 8
DIMENSION cuent1( 6)
DIMENSION cuent2( 6)
DIMENSION cuent3( 6)
SELECT gc_hve00
SET ORDER TO nrdore
STORE 0 TO wrk_toacta, i
GOTO TOP
SEEK STR(wk_numso, 8)
p = 14
@ p, 20 SAY 'Fecha' + SPACE(5) +  ;
  'NøDocumento' + SPACE(4) +  ;
  'Importe'
SCAN WHILE  ;
     VAL(gc_hve00.hve_nrdore) =  ;
     wk_numso .AND.  .NOT. EOF()
     IF gc_hve00.hve_estdoc <>  ;
        'A'
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
          @ p, 20 SAY cuent1(i)
          @ p, 30 SAY cuent2(i)
          @ p, 42 SAY cuent3(i)  ;
            PICTURE '999,999.99'
     ENDIF
ENDSCAN
SEEK STR(w_numero, 8)
IF FOUND()
     p = p + 1
     @ p, 20 SAY  ;
       'Cancelado con...'
ENDIF
SCAN WHILE  ;
     VAL(gc_hve00.hve_nrdore) =  ;
     wk_numord .AND.  .NOT.  ;
     EOF()
     IF gc_hve00.hve_estdoc <>  ;
        'A'
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
          @ p, 20 SAY cuent1(i)
          @ p, 30 SAY cuent2(i)
          @ p, 42 SAY cuent3(i)  ;
            PICTURE '999,999.99'
     ENDIF
ENDSCAN
SELECT gc_cmv00
wk_valpari = ootc(DATE(),'SOL ', ;
             'DOL ','2')
SELECT ge_tab0
wrk_varbus = '"IGV " + "IGV "'
SEEK &wrk_varbus
IF FOUND()
     wk_igv = tab_factor
     wrk_facigv = tab_factor /  ;
                  100
ELSE
     DO error WITH  ;
        '**No Definido el IGV**'
     RETURN
ENDIF
SELECT st_iorep
SEEK STR(w_numero, 8)
wk_repues = cosrep
wk_mano = cosmob
wk_subtot = subtot
wk_totdes = totdes
wk_totnet = (wk_mano + wk_repues) -  ;
            wk_totdes
wk_totigv = ROUND(wk_subtot *  ;
            wrk_facigv, 2)
wk_totnet = wk_subtot - wk_totigv
wk_monto = wk_subtot + wk_totigv
wk_totbru = wk_monto - wk_totdes
sol_cosrep = ROUND(wk_repues *  ;
             wk_valpari, 2)
sol_cosmob = ROUND(wk_mano *  ;
             wk_valpari, 2)
sol_subtot = sol_cosrep +  ;
             sol_cosmob
sol_igv = ROUND(sol_subtot *  ;
          wrk_facigv, 2)
sol_total = sol_subtot + sol_igv
sol_descue = wk_totdes *  ;
             wk_valpari
sol_totgen = sol_total -  ;
             sol_descue
sol_totpag = sol_totgen -  ;
             wrk_toacta
p = p + 1
@ p, 20 SAY  ;
  'Costo En Repuesto : ' +  ;
  SPACE(2) + TRANSFORM(sol_cosrep,  ;
  '999,999.99')
@ p + 1, 20 SAY  ;
  'Costo Mano de Obra: ' +  ;
  SPACE(2) + TRANSFORM(sol_cosmob,  ;
  '999,999.99')
@ p + 2, 20 SAY  ;
  'Total.............: ' +  ;
  SPACE(2) +  ;
  TRANSFORM((sol_subtot +  ;
  sol_igv), '999,999.99')
@ p + 3, 20 SAY  ;
  'Pagos a Cta. .....: ' +  ;
  SPACE(2) + TRANSFORM(wrk_toacta,  ;
  '999,999.99')
@ p + 5, 20 SAY  ;
  'Saldo a Pagar.....: ' +  ;
  SPACE(2) + TRANSFORM(sol_totpag,  ;
  '999,999.99') COLOR N+/W 
= INKEY(0, 'H')
RESTORE SCREEN FROM wk_pantax
RETURN
*
PROCEDURE col_bk1b
SELECT ge_tab0
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
DO coloca WITH 01, 66,  ;
   STR(w_numero, 8)
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
DO coloca WITH 08, 25,  ;
   STR(wk_numstk, 9)
wk_aux = 'MARC' + wk_codmar
SELECT ge_tab0
SEEK '&wk_aux'
IF FOUND()
     DO coloca WITH 05, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
wk_aux = 'INGA' + wk_indori
SELECT ge_tab0
seek '&wk_aux'
IF FOUND()
     DO coloca WITH 08, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
wk_aux = wk_codmar + wk_codmod
SELECT st_imode
SEEK '&wk_aux'
IF FOUND()
     DO coloca WITH 06, 36,  ;
        SUBSTR(nommod, 1, 30)
ENDIF
DO coloca WITH 5, 51, wk_prov
DO coloca WITH 6, 51, wk_doga
DO coloca WITH 7, 51, wk_fevt
DO coloca WITH 8, 51, wk_fecg
IF wk_indori == 'GARA' .OR.  ;
   wk_indori == 'GREC'
     DO coloca WITH 19, 20,  ;
        'EN GARANTIA   '
ELSE
     DO coloca WITH 19, 20,  ;
        'FUERA GARANTIA'
ENDIF
DO coloca WITH 10, 20,  ;
   STR(wk_codcli, 9)
DO coloca WITH 10, 30, wk_noment
DO coloca WITH 11, 20, wk_nomcal
DO coloca WITH 12, 20, wk_nomdis
DO coloca WITH 12, 25, wk_desdis
DO coloca WITH 13, 20, wk_nomciu
DO coloca WITH 13, 25, wk_desciu
DO coloca WITH 14, 20,  ;
   STR(wk_numte1, 7)
DO coloca WITH 14, 30,  ;
   STR(wk_numte2, 7)
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
PROCEDURE informa
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW indicar
ACTIVATE WINDOW trabajo
@ 05, 07 FILL TO 41, 67
@ 04, 07 CLEAR TO 40, 66
@ 04, 07 TO 42, 66
@ 04, 12 SAY  ;
  'Centro de Informaci¢n al Cliente' +  ;
  SPACE(3) + STR(wk_numte1, 8) +  ;
  SPACE(1) + STR(wk_numte2, 8)  ;
  COLOR SCHEME 8
DO usedbf WITH 'st_iscic',  ;
   'fecsol'
SEEK STR(wk_numso, 8)
p = 06
@ p - 1, 10 SAY 'Fecha' +  ;
  SPACE(2) + 'FechaComp' +  ;
  SPACE(6) + 'Informe'
SCAN WHILE numsol = STR(wk_numso,  ;
     8) .AND.  .NOT. EOF()
     IF p >= 36
          p = 36
     ENDIF
     @ p, 08 SAY fecini
     @ p, 19 SAY feccom
     en1 = SUBSTR(inform, 1, 35)
     en2 = SUBSTR(inform, 36, 70)
     @ p, 30 SAY en1
     IF  .NOT. EMPTY(en2)
          @ p + 1, 30 SAY en2
          p = p + 2
     ELSE
          p = p + 1
     ENDIF
ENDSCAN
STORE SPACE(35) TO wk_obser1,  ;
      wk_obser2
STORE DATE() + 3 TO wk_fecco
STORE TIME() TO w_horini
@ p, 08 SAY DATE() COLOR 'n/w'
SET CURSOR ON
@ p, 19 GET wk_fecco
@ p, 30 GET wk_obser1
@ p + 1, 30 GET wk_obser2
READ
SET CURSOR OFF
IF  .NOT. EMPTY(wk_obser1)
     APPEND BLANK
     REPLACE numsol WITH  ;
             STR(wk_numso, 8),  ;
             numord WITH  ;
             STR(wk_numord, 8)
     REPLACE inform WITH  ;
             wk_obser1 +  ;
             wk_obser2, fecini  ;
             WITH DATE(), feccom  ;
             WITH wk_fecco,  ;
             horini WITH w_horini,  ;
             horfin WITH TIME(),  ;
             horcom WITH TIME()
     REPLACE user WITH users,  ;
             date WITH DATE(),  ;
             time WITH TIME()
ENDIF
RESTORE SCREEN FROM wk_pantax
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

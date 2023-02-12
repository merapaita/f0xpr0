PARAMETER config_prg
wrk_progra = PROGRAM()
CLOSE DATABASES
DO crea_win
CLEAR TYPEAHEAD
SELECT 1
USE SHARED st_isrep ORDER sol_mamose
SELECT 2
USE SHARED st_iseri ORDER ser_codmar
SELECT 3
USE SHARED st_iclpr ORDER codigo
SELECT 4
USE SHARED st_imode ORDER codigo
SELECT 5
USE SHARED ge_tab0 ORDER codigo
SELECT 6
USE SHARED st_iorep ORDER ord_mamose
SELECT 7
USE SHARED st_iparg
w_dcgar = sys_fecgar
SELECT 8
USE SHARED st_sint ORDER sin_lincod
ppal = .T.
ON KEY LABEL F6 do ayuda10
ON KEY LABEL F10 DO FCINCO
DO esc_indica WITH 1, 'AYU', 'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'BBB', 'BBB', 'IGN', 'ESC'
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
SET DISPLAY TO VGA50
ZOOM WINDOW trabajo NORM FROM 01, 00 TO 42, 76
ZOOM WINDOW indicar NORM FROM 45, 00 TO 48, 76
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
STORE .T. TO ppal
SELECT 20
USE SHARED gc_cmv00 ORDER cmv_feinmo
SEEK DTOS(DATE()) + '1' + 'SOL ' + 'DOL '
IF  .NOT. FOUND()
     DO error2 WITH '*** No Existe Tipo de Cambio de esta Fecha ***'
     ppal = .F.
ELSE
     w_tipcam = cmv_tipcav
ENDIF
w_facigv = facigv()
IF w_facigv = 0
     DO error2 WITH '***Tipo de Impuesto no existe en Tablas ***'
     ppal = .F.
ENDIF
w_numero = 0
DO WHILE ppal
     ON KEY LABEL F6 do ayuda10
     ON KEY LABEL F10 DO FCINCO
     STORE 0 TO w_valvta, w_igv, w_valor, cuenta, w_flag, nunmat, w_mobra, w_totgen
     STORE SPACE(4) TO w_docvta, w_concep, w_emisor, w_indest, w_linea
     STORE SPACE(5) TO w_codemp
     STORE SPACE(9) TO w_numdoc
     STORE SPACE(11) TO w_provee
     STORE SPACE(11) TO w_codcli
     STORE SPACE(10) TO w_gara
     STORE SPACE(15) TO w_docume, w_codbla
     STORE SPACE(30) TO w_nomemp, w_nomtit
     STORE 0 TO mm, num, w_flag, w_indsal, dm, w_indgar, sol_cosmob
     STORE 0 TO sol_subtot, sol_totgen, w_mesgar, w_mat, sol_totvta, sol_igv
     STORE SPACE(4) TO w_codmar, w_codmod, w_indori, w_tgara, w_auxest
     STORE SPACE(8) TO w_ndoref, w_ndosol
     STORE {} TO w_fdoref, w_fchgar, w_fchfin, w_fecest, w_fecent
     DIMENSION mat( 15)
     DIMENSION tem( 15)
     DIMENSION w_sin( 9)
     DIMENSION w_acc( 9)
     DIMENSION w_obs( 3)
     lin = 40
     des = 1
     com = 1
     anc = 75
     DO esc_indica WITH 1, 'AYU', 'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'RAC', 'BBB', 'IGN', 'ESC'
     DIMENSION solic( 48)
     STORE FOPEN('solicitu.txt') TO file_handl
     FOR i = 1 TO 48
          solic( i) = FREAD(file_handl, 77)
     ENDFOR
     = FCLOSE(file_handl)
     FOR i = des TO (lin + des - 1)
          @ i - des, 0 SAY SUBSTR(solic(i), com, anc)
     ENDFOR
     efecin = 1
     DIMENSION w_codsin( 15)
     DIMENSION w_acceso( 15)
     DIMENSION w_observ( 06)
     STORE SPACE(35) TO w_nomeni, w_estado, w_nommon, w_desdis, w_desciu, w_desmon
     STORE 0 TO w_abonos, w_numstk, w_numte1, w_numte2
     STORE SPACE(4) TO w_codmar, w_indori, w_codcla, w_codcla2, w_nomdis
     STORE SPACE(4) TO w_nomciu
     STORE SPACE(30) TO w_desate
     STORE SPACE(50) TO w_noment, w_nomcal
     w_feccom = DATE() + empre3
     w_fecemi = DATE()
     w_codmod = SPACE(15)
     w_numser = SPACE(20)
     w_coddes = 'R'
     w_codmon = 'SOL '
     w_indest = ' '
     FOR i = 1 TO 15
          w_codsin( i) = SPACE(35)
          w_acceso( i) = SPACE(35)
          IF i < 07
               w_observ( i) = SPACE(45)
          ENDIF
     ENDFOR
     DO col_bk1b
     DO col_bk2b
     FOR i = des TO (lin + des - 1)
          @ i - des, 0 SAY SUBSTR(solic(i), com, anc)
     ENDFOR
     w_codcla2 = SPACE(4)
     w_valida = .T.
     w_sinaux = SPACE(35)
     blok = 1
     ppal2 = .T.
     FOR i = 1 TO 15
          mat( i) = SPACE(45)
     ENDFOR
     w_numero = 0
     DO WHILE ppal2
          ON KEY LABEL F6 do ayuda10
          ON KEY LABEL F10 DO FCINCO
          efecin = 1
          DO CASE
               CASE blok == 1
                    @ 2, 58 SAY 'TC: ' + TRANSFORM(w_tipcam, '9,999,999.99')
                    SET CURSOR ON
                    @ 04, 19 GET w_codmar PICTURE '@!' VALID valtab('MARC',w_codmar,24,30) WHEN colocaf6()
                    @ 05, 19 GET w_codmod PICTURE '@!' VALID codmod2(w_codmod) WHEN colocaf6()
                    @ 06, 19 GET w_numser PICTURE '@!' VALID numser2(w_numser)
                    @ 07, 19 GET w_indori PICTURE '@!' VALID valtabtg('INGA',w_indori,24,30) WHEN colocaf6()
                    @ 09, 19 GET w_codcli PICTURE '@!' VALID cliente(29) WHEN colocaf6()
                    @ 09, 31 GET w_noment FUNCTION 'S40' PICTURE '@!' VALID codalf(w_noment) WHEN w_valida
                    @ 10, 19 GET w_nomcal FUNCTION 'S40' PICTURE '@!' VALID codalf(w_nomcal) WHEN w_valida
                    @ 11, 19 GET w_nomdis PICTURE '@!' VALID valtab('DIST',w_nomdis,24,30) WHEN colocaf6()
                    @ 12, 19 GET w_nomciu PICTURE '@!' VALID valtab('PROV',w_nomciu,24,30) WHEN colocaf6()
                    @ 13, 19 GET w_numte1 PICTURE '99999999' WHEN w_valida
                    @ 13, 29 GET w_numte2 PICTURE '99999999' WHEN w_valida
                    @ 15, 63 GET w_feccom PICTURE '99/99/9999' VALID valfec(w_feccom)
                    @ 17, 19 GET w_emisor PICTURE '@!' VALID valtab2('EMIS',w_emisor,24,30) WHEN colocaf6()
                    @ 19, 19 GET w_abonos PICTURE '99,999,999.99' VALID val_abo() WHEN w_indori = 'FGAR'
                    @ 19, 43 GET w_codmon PICTURE '@!' VALID valtab('MONE',w_codmon,48,15) WHEN w_abonos <> 0 .AND. colocaf6()
                    @ 20, 19 GET w_coddes PICTURE '@!' VALID coddes(w_coddes)
                    READ
                    IF LASTKEY() = 27
                         ON KEY LABEL F6
                         ON KEY LABEL F10
                         SET DISPLAY TO VGA25
                         ZOOM WINDOW trabajo NORM FROM 1, 0 TO 17, 76
                         ZOOM WINDOW indicar NORM FROM 20, 0 TO 23, 76
                         DO sacawin
                         CLOSE DATABASES
                         RETURN
                    ENDIF
                    DO ingsin
                    FOR i = 1 TO 15
                         w_codsin( i) = SUBSTR(mat(i), 1, 35)
                    ENDFOR
                    SET CURSOR ON
                    @ 24, 37 GET w_acceso( 01) PICTURE '@!' WHEN poracc(01)
                    @ 25, 37 GET w_acceso( 02) PICTURE '@!' WHEN poracc(02)
                    @ 26, 37 GET w_acceso( 03) PICTURE '@!' WHEN poracc(03)
                    @ 27, 37 GET w_acceso( 04) PICTURE '@!' WHEN poracc(04)
                    @ 28, 37 GET w_acceso( 05) PICTURE '@!' WHEN poracc(05)
                    @ 29, 37 GET w_acceso( 06) PICTURE '@!' WHEN poracc(06)
                    @ 30, 37 GET w_acceso( 07) PICTURE '@!' WHEN poracc(07)
                    @ 31, 37 GET w_acceso( 08) PICTURE '@!' WHEN poracc(08)
                    @ 32, 37 GET w_acceso( 09) PICTURE '@!' WHEN poracc(09)
                    @ 33, 37 GET w_acceso( 10) PICTURE '@!' WHEN poracc(10)
                    @ 34, 37 GET w_acceso( 11) PICTURE '@!' WHEN poracc(11)
                    @ 35, 37 GET w_acceso( 12) PICTURE '@!' WHEN poracc(12)
                    @ 36, 37 GET w_acceso( 13) PICTURE '@!' WHEN poracc(13)
                    @ 37, 37 GET w_acceso( 14) PICTURE '@!' WHEN poracc(14)
                    @ 38, 37 GET w_acceso( 15) PICTURE '@!' WHEN poracc(15)
                    READ
                    DO CASE
                         CASE LASTKEY() == 27
                              LOOP
                         CASE LASTKEY() == 05 .OR. LASTKEY() == 18
                              LOOP
                         CASE LASTKEY() == 13 .OR. LASTKEY() == 24 .OR. LASTKEY() == 3 .OR. LASTKEY() == 9 .OR. (LASTKEY() > 31 .AND. LASTKEY() < 123)
                              DO col_bk1b
                              blok = 2
                              DO mueve3 WITH REPLICATE(CHR(24), 08)
                    ENDCASE
                    FOR i = 1 TO 06
                         @ 32 + i, 1 GET w_observ( i) PICTURE '@!'
                    ENDFOR
                    READ
                    o = 1
                    FOR i = 1 TO 3
                         w_obs( i) = w_observ(o) + w_observ(o + 1)
                         o = o + 2
                    ENDFOR
               CASE blok == 2
                    FOR i = 1 TO 06
                         @ 32 + i, 1 GET w_observ( i) PICTURE '@!'
                    ENDFOR
                    READ
                    o = 1
                    FOR i = 1 TO 3
                         w_obs( i) = w_observ(o) + w_observ(o + 1)
                         o = o + 2
                    ENDFOR
          ENDCASE
          DO CASE
               CASE LASTKEY() == 27
                    blok = 1
                    DO mueve3 WITH REPLICATE(CHR(05), 08)
                    LOOP
               CASE LASTKEY() == 13 .OR. LASTKEY() == 24 .OR. LASTKEY() == 3 .OR. LASTKEY() == 9 .OR. (LASTKEY() > 31 .AND. LASTKEY() < 123)
                    DO col_bk2b
               CASE LASTKEY() == 05 .OR. LASTKEY() == 18 .OR. LASTKEY() == 127 .OR. LASTKEY() == 15
                    DO col_bk2b
                    blok = 1
                    DO mueve3 WITH REPLICATE(CHR(05), 08)
                    LOOP
          ENDCASE
          IF LASTKEY() <> 27 .AND. w_indori == 'FGAR' .AND. w_abonos > 0
               DO pago_acta
          ENDIF
          DO gra0201
     ENDDO
ENDDO
CLOSE DATABASES
ON KEY LABEL F6
ON KEY LABEL F10
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1, 0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20, 0 TO 23, 76
DO sacawin
RELEASE tecnico
RETURN
*
PROCEDURE gra0201
IF LASTKEY() <> 27
     DO esc_indica WITH 1, 'AYU', 'BBB', 'IMP', 'BBB'
     DO esc_indica WITH 2, 'GRA', 'BBB', 'IGN', 'ESC'
     DO emplea
     w_key = 0
     IF LASTKEY() = 27
          RETURN
     ENDIF
     DO WHILE w_key<>-1 .AND. w_key<>-6 .AND. w_key<>-9 .AND. w_key<>27


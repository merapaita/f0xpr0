*-------------------------
* Modificacion de Solicitus
*-------------------------
PARAMETER config_prg
wrk_progra = PROGRAM()
IF config_prg == 1
     ind_prg = '<PORM0201>'
ELSE
     IF config_prg == 2
          ind_prg = '<PORM0201>'
     ELSE
          ind_prg = '<PORM0201>'
          lll = 1
     ENDIF
ENDIF
CLOSE DATABASES
DO crea_win
CLEAR TYPEAHEAD
SELECT 1
USE SHARED st_isrep ORDER codigo
SELECT 2
USE SHARED st_iseri ORDER  ;
    ser_codmar
SELECT 3
USE SHARED st_iclpr ORDER codigo
SELECT 4
USE SHARED st_imode ORDER codigo
SELECT 5
USE SHARED ge_tab0 ORDER codigo
SELECT 6
USE SHARED st_iorep ORDER  ;
    ord_mamose
SELECT 7
USE SHARED st_iparg
w_dcgar = sys_fecgar
SELECT 8
USE SHARED st_sint ORDER  ;
    sin_lincod
SELECT 9
USE SHARED st_sicli ORDER codigo
ppal = .T.
ON KEY LABEL F6 do ayuda10
ON KEY LABEL F10 DO FCINCO
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
SET DISPLAY TO VGA50
ZOOM WINDOW trabajo NORM FROM 01,  ;
     00 TO 42, 76
ZOOM WINDOW indicar NORM FROM 45,  ;
     00 TO 48, 76
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
SELECT 20
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
SEEK DTOS(DATE()) + '1' + 'SOL ' +  'DOL '
*IF !FOUND()
*     DO error2 WITH  ;
*        '*** No Existe Tipo de Cambio de esta Fecha ***'
*     ppal = .F.
*ELSE
     w_tipcam = 3.25
*     w_tipcam = cmv_tipcav
*ENDIF
w_facigv = facigv()
IF w_facigv = 0
     DO error2 WITH  ;
        '***Tipo de Impuesto no existe en Tablas ***'
     ppal = .F.
ENDIF
DO WHILE ppal
     ON KEY LABEL F6 do ayuda10
     ON KEY LABEL F10 DO FCINCO
     STORE 0 TO w_valvta, w_igv,  ;
           w_valor, cuenta,  ;
           w_flag, w_numero,  ;
           nunmat, w_mobra,  ;
           w_totgen
     STORE SPACE(4) TO w_docvta,  ;
           w_concep, w_emisor,  ;
           w_indest, w_linea,  ;
           w_antgar
     STORE SPACE(5) TO w_codemp
     STORE SPACE(9) TO w_numdoc
     STORE SPACE(11) TO w_provee
     STORE SPACE(11) TO w_codcli
     STORE SPACE(10) TO w_gara
     STORE SPACE(15) TO w_docume,  ;
           w_codbla, w_desmon
     STORE SPACE(30) TO w_nomemp,  ;
           w_nomtit
     STORE 0 TO mm, num, w_flag,  ;
           w_indsal, dm, w_indgar,  ;
           sol_cosmob, w_ind2
     STORE 0 TO sol_subtot,  ;
           sol_totgen, w_mesgar,  ;
           w_mat, sol_totvta,  ;
           sol_igv
     STORE SPACE(4) TO w_codmar,  ;
           w_codmod, w_indori,  ;
           w_tgara, w_auxest
     STORE SPACE(8) TO w_ndoref,  ;
           w_numord, w_ndosol
     STORE {} TO w_fdoref,  ;
           w_fchgar, w_fchfin,  ;
           w_fecest, w_fecent
     DIMENSION mat( 15)
     DIMENSION tem( 15)
     DIMENSION w_sin( 9)
     DIMENSION w_acc( 9)
     DIMENSION w_obs( 3)
     lin = 40
     des = 1
     com = 1
     anc = 75
     IF config_prg == 1 .OR.  ;
        config_prg == 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'INT'
          DO esc_indica WITH 2,  ;
             'RAC', 'BBB', 'IGN',  ;
             'ESC'
     ELSE
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'ANU', 'BBB', 'IGN',  ;
             'ESC'
     ENDIF
     DIMENSION solic( 48)
     STORE FOPEN('solicitu.txt')  ;
           TO file_handl
     FOR i = 1 TO 48
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
     efecin = 1
     @ 2, 58 SAY 'TC: ' +  ;
       TRANSFORM(w_tipcam,  ;
       '9,999,999.99')
     IF config_prg <> 1
          w_numero = 0
          @ 0, 65 GET w_numero  ;
            PICTURE '99999999'  ;
            VALID  ;
            solvig(w_numero) WHEN  ;
            colocaf6()
     ELSE
          @ 0, 65 GET w_numero  ;
            PICTURE '99999999'  ;
            VALID  ;
            solvig2(w_numero)
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
     DIMENSION w_codsin( 15)
     DIMENSION w_acceso( 15)
     DIMENSION w_observ( 06)
     STORE SPACE(35) TO w_nomeni,  ;
           w_estado, w_nommon,  ;
           w_desdis, w_desciu
     STORE 0 TO w_abonos,  ;
           w_numstk, w_numte1,  ;
           w_numte2
     STORE SPACE(4) TO w_codmar,  ;
           w_indori, w_codcla,  ;
           w_codcla2, w_nomdis
     STORE SPACE(4) TO w_nomciu
     STORE SPACE(30) TO w_desate
     STORE SPACE(50) TO w_noment,  ;
           w_nomcal
     w_feccom = DATE() + empre3
     w_fecemi = DATE()
     w_horemi = SPACE(8)
     w_codmod = SPACE(15)
     w_numser = SPACE(20)
     w_coddes = 'R'
     w_codmon = 'SOL '
     w_indest = ' '
     FOR i = 1 TO 15
          w_codsin( i) =  ;
                  SPACE(35)
          w_acceso( i) =  ;
                  SPACE(35)
          IF i < 07
               w_observ( i) =  ;
                       SPACE(45)
          ENDIF
     ENDFOR
     FOR i = 1 TO 15
          mat( i) = SPACE(45)
     ENDFOR
     IF config_prg <> 1
          w_numaux = STR(w_numero,  ;
                     8)
          SELECT st_isrep
          SET ORDER TO codigo
          SEEK '&w_numaux'
          w_fecemi = fecemi
          w_horemi = horemi
          w_feccom = feccom
          w_emisor = codemi
          w_codcli = codent
          w_indori = indori
          w_indest = indest
          w_codmar = codmar
          w_codmod = codmod
          w_numser = numser
          w_abonos = monabo
          w_codmon = codmon
          w_numstk = VAL(numstk)
          w_coddes = coddes
          sol_totgen = monabo
          FOR i = 1 TO 15
               w_codsin = SPACE(35)
               w_acceso( i) =  ;
                       SUBSTR(desace,  ;
                       1 + ((i -  ;
                       1) * 35),  ;
                       35)
               w_acceso( i) =  ;
                       w_acceso(i) +  ;
                       SPACE(35 -  ;
                       LEN(w_acceso(i)))
               IF i <= 6
                    w_observ( i) =  ;
                            SUBSTR(observ,  ;
                            1 +  ;
                            ((i -  ;
                            1) *  ;
                            45),  ;
                            45)
                    w_observ( i) =  ;
                            w_observ(i) +  ;
                            SPACE(45 -  ;
                            LEN(w_observ(i)))
               ENDIF
          ENDFOR
          IF w_indori = 'GARA'  ;
             .OR. w_indori =  ;
             'GREC' .OR. w_indori =  ;
             'PVEN' .OR. w_indori =  ;
             'PREC'
               SELECT st_iseri
               SET ORDER TO ser_codmar
               SEEK w_codmar +  ;
                    w_codmod +  ;
                    w_numser
               IF FOUND()
                    w_provee = codent
                    w_docume = docgar
                    w_fchgar = fecvta
                    w_fchfin = fecgar
                    w_gara = DTOC(fecvta) +  ;
                             ' ' +  ;
                             docgar
               ENDIF
          ENDIF
          w_cliaux = 'C' +  ;
                     w_codcli
          SELECT st_iclpr
          SET ORDER TO codigo
          SEEK '&w_cliaux'
          w_noment = noment
          w_nomcal = nomcal
          w_nomdis = nomdis
          w_nomciu = nomciu
          w_numte1 = numte1
          w_numte2 = numte2
          w_aux = w_codmar +  ;
                  w_codmod
          SELECT st_imode
          SEEK '&w_aux'
          w_aux = codcla
          w_linea = linea
          i = 1
          SELECT st_sicli
          SEEK w_numaux
          IF FOUND()
               SCAN WHILE  ! ;
                    EOF() .AND.  ;
                    numdoc =  ;
                    w_numaux
                    w_aux2 = codsin
                    SELECT st_sint
                    SEEK w_linea +  ;
                         SUBSTR(w_aux2,  ;
                         2, 3)
                    w_codsin( i) =  ;
                            w_aux2 +  ;
                            '³' +  ;
                            SUBSTR(dessin,  ;
                            1,  ;
                            30)
                    mat( i) =  ;
                       w_aux2 +  ;
                       '³' +  ;
                       SUBSTR(dessin,  ;
                       1, 30)
                    i = i + 1
                    SELECT st_sicli
               ENDSCAN
          ENDIF
     ENDIF
     DO col_bk1b
     DO col_bk2b
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
     IF config_prg <> 3
          w_codcla2 = SPACE(4)
          w_valida = .T.
          w_sinaux = SPACE(35)
          blok = 1
          ppal2 = .T.
          DO WHILE ppal2
               ON KEY LABEL F6 do ayuda10
               ON KEY LABEL F10 DO FCINCO
               efecin = 1
               DO CASE
                    CASE blok ==  ;
                         1
                         SET CURSOR ON
                         @ 2, 58  ;
                           SAY  ;
                           'TC: ' +  ;
                           TRANSFORM(w_tipcam,  ;
                           '9,999,999.99' ;
                           )
                         @ 04, 19  ;
                           GET  ;
                           w_codmar  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           valtab('MARC', ;
                           w_codmar, ;
                           24,30)  ;
                           WHEN  ;
                           colocaf6()
                         @ 05, 19  ;
                           GET  ;
                           w_codmod  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           codmod2(w_codmod)  ;
                           WHEN  ;
                           colocaf6()
                         @ 06, 19  ;
                           GET  ;
                           w_numser  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           numser2(w_numser)
                         @ 07, 19  ;
                           GET  ;
                           w_indori  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           valtabtg('INGA', ;
                           w_indori, ;
                           24,30)  ;
                           WHEN  ;
                           colocaf6()
                         @ 09, 19  ;
                           GET  ;
                           w_codcli  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           cliente(29)  ;
                           WHEN  ;
                           colocaf6()
                         @ 09, 31  ;
                           GET  ;
                           w_noment  ;
                           FUNCTION  ;
                           'S40'  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           codalf(w_noment)  ;
                           WHEN  ;
                           w_valida
                         @ 10, 19  ;
                           GET  ;
                           w_nomcal  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           codalf(w_nomcal)  ;
                           WHEN  ;
                           w_valida
                         @ 11, 19  ;
                           GET  ;
                           w_nomdis  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           valtab('DIST', ;
                           w_nomdis, ;
                           24,30)  ;
                           WHEN  ;
                           colocaf6()
                         @ 12, 19  ;
                           GET  ;
                           w_nomciu  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           valtab('PROV', ;
                           w_nomciu, ;
                           24,30)  ;
                           WHEN  ;
                           colocaf6()
                         @ 13, 19  ;
                           GET  ;
                           w_numte1  ;
                           PICTURE  ;
                           '99999999'  ;
                           WHEN  ;
                           w_valida
                         @ 13, 29  ;
                           GET  ;
                           w_numte2  ;
                           PICTURE  ;
                           '99999999'  ;
                           WHEN  ;
                           w_valida
                         @ 15, 63  ;
                           GET  ;
                           w_feccom  ;
                           PICTURE  ;
                           '99/99/9999'  ;
                           VALID  ;
                           valfec(w_feccom)
                         @ 17, 19  ;
                           GET  ;
                           w_emisor  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           valtab2('EMIS', ;
                           w_emisor, ;
                           24,30)  ;
                           WHEN  ;
                           colocaf6()
                         @ 19, 19  ;
                           GET  ;
                           w_abonos  ;
                           PICTURE  ;
                           '99,999,999.99'  ;
                           VALID  ;
                           val_abo()  ;
                           WHEN  ;
                           w_indori =  ;
                           'FGAR'  ;
                           .AND.  ;
                           config_prg =  ;
                           1
                         @ 19, 43  ;
                           GET  ;
                           w_codmon  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           valtab('MONE', ;
                           w_codmon, ;
                           48,15)  ;
                           WHEN  ;
                           config_prg =  ;
                           1  ;
                           .AND.  ;
                           w_abonos <>  ;
                           0  ;
                           .AND.  ;
                           colocaf6()
                         @ 20, 19  ;
                           GET  ;
                           w_coddes  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID  ;
                           coddes(w_coddes)
                         READ
                         IF LASTKEY() =  ;
                            27
                              ON KEY LABEL;
F6
                              ON KEY LABEL;
F10
                              SET DISPLAY;
TO VGA25
                              ZOOM  ;
                               WINDOW  ;
                               trabajo  ;
                               NORM  ;
                               FROM  ;
                               1,  ;
                               0  ;
                               TO  ;
                               17,  ;
                               76
                              ZOOM  ;
                               WINDOW  ;
                               indicar  ;
                               NORM  ;
                               FROM  ;
                               20,  ;
                               0  ;
                               TO  ;
                               23,  ;
                               76
                              DO sacawin
                              CLOSE  ;
                               DATABASES
                              RETURN
                         ENDIF
                         DO ingsin
                         FOR i =  ;
                             1 TO  ;
                             15
                              w_codsin(  ;
                               i) =  ;
                               SUBSTR(mat(i),  ;
                               1,  ;
                               35)
                         ENDFOR
                         SET CURSOR ON
                         @ 24, 37  ;
                           GET  ;
                           w_acceso(  ;
                           01)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(01)
                         @ 25, 37  ;
                           GET  ;
                           w_acceso(  ;
                           02)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(02)
                         @ 26, 37  ;
                           GET  ;
                           w_acceso(  ;
                           03)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(03)
                         @ 27, 37  ;
                           GET  ;
                           w_acceso(  ;
                           04)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(04)
                         @ 28, 37  ;
                           GET  ;
                           w_acceso(  ;
                           05)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(05)
                         @ 29, 37  ;
                           GET  ;
                           w_acceso(  ;
                           06)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(06)
                         @ 30, 37  ;
                           GET  ;
                           w_acceso(  ;
                           07)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(07)
                         @ 31, 37  ;
                           GET  ;
                           w_acceso(  ;
                           08)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(08)
                         @ 32, 37  ;
                           GET  ;
                           w_acceso(  ;
                           09)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(09)
                         @ 33, 37  ;
                           GET  ;
                           w_acceso(  ;
                           10)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(10)
                         @ 34, 37  ;
                           GET  ;
                           w_acceso(  ;
                           11)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(11)
                         @ 35, 37  ;
                           GET  ;
                           w_acceso(  ;
                           12)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(12)
                         @ 36, 37  ;
                           GET  ;
                           w_acceso(  ;
                           13)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(13)
                         @ 37, 37  ;
                           GET  ;
                           w_acceso(  ;
                           14)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(14)
                         @ 38, 37  ;
                           GET  ;
                           w_acceso(  ;
                           15)  ;
                           PICTURE  ;
                           '@!'  ;
                           WHEN  ;
                           poracc(15)
                         READ
                         DO CASE
                              CASE  ;
                               LASTKEY() ==  ;
                               27
                                   LOOP
                              CASE  ;
                               LASTKEY() ==  ;
                               05  ;
                               .OR.  ;
                               LASTKEY() ==  ;
                               18
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
                                   DO col_bk1b
                                   blok = 2
                                   DO mueve3 WITH REPLICATE(CHR(24), 08)
                         ENDCASE
                         FOR i =  ;
                             1 TO  ;
                             06
                              @ 32 +  ;
                                i,  ;
                                1  ;
                                GET  ;
                                w_observ(  ;
                                i)  ;
                                PICTURE  ;
                                '@!'
                         ENDFOR
                         READ
                         o = 1
                         FOR i =  ;
                             1 TO  ;
                             3
                              w_obs(  ;
                               i) =  ;
                               w_observ(o) +  ;
                               w_observ(o +  ;
                               1)
                              o =  ;
                               o +  ;
                               2
                         ENDFOR
                    CASE blok ==  ;
                         2
                         FOR i =  ;
                             1 TO  ;
                             06
                              @ 32 +  ;
                                i,  ;
                                1  ;
                                GET  ;
                                w_observ(  ;
                                i)  ;
                                PICTURE  ;
                                '@!'
                         ENDFOR
                         READ
                         o = 1
                         FOR i =  ;
                             1 TO  ;
                             3
                              w_obs(  ;
                               i) =  ;
                               w_observ(o) +  ;
                               w_observ(o +  ;
                               1)
                              o =  ;
                               o +  ;
                               2
                         ENDFOR
               ENDCASE
               DO CASE
                    CASE LASTKEY() ==  ;
                         27
                         blok = 1
                         DO mueve3  ;
                            WITH  ;
                            REPLICATE(CHR(05),  ;
                            08)
                         LOOP
                    CASE LASTKEY() ==  ;
                         13 .OR.  ;
                         LASTKEY() ==  ;
                         24 .OR.  ;
                         LASTKEY() ==  ;
                         3 .OR.  ;
                         LASTKEY() ==  ;
                         9 .OR.  ;
                         (LASTKEY() >  ;
                         31 .AND.  ;
                         LASTKEY() <  ;
                         123)
                         DO col_bk2b
                    CASE LASTKEY() ==  ;
                         05 .OR.  ;
                         LASTKEY() ==  ;
                         18 .OR.  ;
                         LASTKEY() ==  ;
                         127 .OR.  ;
                         LASTKEY() ==  ;
                         15
                         DO col_bk2b
                         blok = 1
                         DO mueve3  ;
                            WITH  ;
                            REPLICATE(CHR(05),  ;
                            08)
                         LOOP
               ENDCASE
               IF LASTKEY() <> 27  ;
                  .AND. w_indori ==  ;
                  'FGAR' .AND.  ;
                  w_abonos > 0  ;
                  .AND.  ;
                  config_prg = 1
                    DO pago_acta
               ENDIF
               DO gra0201
          ENDDO
     ELSE
          DO eli0201
     ENDIF
ENDDO
CLOSE DATABASES
ON KEY LABEL F6
ON KEY LABEL F10
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DO sacawin
RELEASE tecnico
RETURN
*
FUNCTION solvig
PARAMETER cod
IF cod = 0
     DO error2 WITH  ;
        '** N§ Solicitud debe ser Ingresado. **'
     RETURN .F.
ENDIF
cod = STR(cod, 8)
SELECT st_isrep
SET ORDER TO codigo
SEEK '&cod'
IF  !FOUND()
     DO error2 WITH  ;
        '** Error Solicitud NO EXISTE. **'
     RETURN .F.
ENDIF
IF SUBSTR(indest, 1, 1) == 'N'
     DO error2 WITH  ;
        '** Error Solicitud esta Anulada. **'
     RETURN .F.
ENDIF
IF SUBSTR(indest, 1, 1) == 'B'  ;
   .OR. SUBSTR(indest, 1, 1) ==  ;
   'F' .AND. nivell <> 'A7'
     DO error2 WITH  ;
        '** Solicitud ya fue '+ ;
        IIF(SUBSTR(indest, 1, 1)= ;
        'B', 'Boleteada',  ;
        'Facturada')+'**'
     RETURN .F.
ENDIF
w_indest = indest
SELECT st_iorep
SET ORDER TO ord_numsol
SEEK cod
IF FOUND()
     IF SUBSTR(indest, 1, 1) <>  ;
        'N' .AND. config_prg = 3
          DO error2 WITH  ;
             '** Solicitud No puede Anularse,O/R no esta Anulada **'
          RETURN .F.
     ENDIF
     IF (auxest = '020 ' .OR.  ;
        auxest = '022 ' .OR.  ;
        auxest = '023 ' .OR.  ;
        auxest = '024 ' .OR.  ;
        auxest = '025 ' .OR.  ;
        auxest = '028 ' .OR.  ;
        auxest = '029 ' .OR.  ;
        auxest = '030 ' .OR.  ;
        auxest = '080 ') .AND.  ;
        config_prg = 2 .AND.  ;
        nivell <> 'A7'
          DO error2 WITH  ;
             '** Solicitud No puede Modificarse, ya se l¡quido **'
          RETURN .F.
     ENDIF
ENDIF
DO sacaf6
RETURN .T.
*
FUNCTION solvig2
PARAMETER cod
IF cod = 0
     DO error2 WITH  ;
        '** N§ Solicitud debe ser Ingresado. **'
     RETURN .F.
ENDIF
cod = STR(cod, 8)
SELECT st_isrep
SET ORDER TO codigo
SEEK '&cod'
IF FOUND()
     DO error2 WITH  ;
        '** Solicitud Ya Creada. **'
     RETURN .F.
ENDIF
DO sacaf6
RETURN .T.
*
PROCEDURE gra0201
IF LASTKEY() <> 27
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'IMP', 'BBB'
     DO esc_indica WITH 2, 'GRA',  ;
        'BBB', 'IGN', 'ESC'
     DO emplea
     w_key = 0
     IF LASTKEY() = 27
          RETURN
     ENDIF
     DO WHILE w_key<>-1 .AND.  ;
        w_key<>-6 .AND. w_key<>-9  ;
        .AND. w_key<>27
          w_key = INKEY(0, 'H')
     ENDDO
     ON KEY
     IF w_key == 27
          w_salir = .F.
     ENDIF
     IF w_key == -1 .OR. w_key == - ;
        6
          ppal2 = .F.
          SELECT st_isrep
          SET ORDER TO codigo
          SEEK STR(w_numero, 8)
          IF  !FOUND()
               APPEND BLANK
          ENDIF
          DO rbloquea
          REPLACE numdoc WITH  ;
                  STR(w_numero,  ;
                  8), feccom WITH  ;
                  w_feccom
          REPLACE codemi WITH  ;
                  w_emisor,  ;
                  codent WITH  ;
                  w_codcli
          REPLACE indori WITH  ;
                  w_indori,  ;
                  numser WITH  ;
                  w_numser
          REPLACE codmar WITH  ;
                  w_codmar,  ;
                  codmod WITH  ;
                  w_codmod
          REPLACE coddes WITH  ;
                  w_coddes
          REPLACE codmon WITH  ;
                  w_codmon
          REPLACE monabo WITH  ;
                  w_abonos
          REPLACE user WITH users
          REPLACE date WITH  ;
                  DATE()
          REPLACE time WITH  ;
                  TIME()
          IF config_prg = 1
               REPLACE indest  ;
                       WITH  ;
                       'V   '
               REPLACE horemi  ;
                       WITH  ;
                       TIME()
               REPLACE fecemi  ;
                       WITH  ;
                       DATE()
          ENDIF
          UNLOCK
          w_aux = ''
          FOR i = 1 TO 6
               IF LEN(TRIM(w_observ(i))) <>  ;
                  0
                    w_aux = w_aux +  ;
                            w_observ(i)
               ENDIF
          ENDFOR
          DO rbloquea
          REPLACE observ WITH  ;
                  w_aux
          UNLOCK
          w_aux = ''
          FOR i = 1 TO 15
               IF LEN(TRIM(w_acceso(i))) <>  ;
                  0
                    w_aux = w_aux +  ;
                            w_acceso(i)
               ENDIF
          ENDFOR
          DO rbloquea
          REPLACE desace WITH  ;
                  w_aux
          UNLOCK
          IF w_indori = 'FGAR'  ;
             .AND. w_abonos > 0  ;
             .AND. config_prg =  ;
             1
               DO calculo
               DO stat_print
               DO graba
               DO imprime
          ENDIF
          IF w_indori = 'GARA'  ;
             .OR. w_indori =  ;
             'GREC' .OR. w_indori =  ;
             'PVEN' .OR. w_indori =  ;
             'PREC'
               SELECT st_iseri
               SEEK w_codmar +  ;
                    w_codmod +  ;
                    w_numser
               IF  !FOUND()
                    APPEND BLANK
               ENDIF
               DO rbloquea
               REPLACE codent  ;
                       WITH  ;
                       w_provee
               REPLACE codmar  ;
                       WITH  ;
                       w_codmar
               REPLACE numser  ;
                       WITH  ;
                       w_numser
               REPLACE modelo  ;
                       WITH  ;
                       w_codmod
               REPLACE docgar  ;
                       WITH  ;
                       w_docume
               REPLACE fecvta  ;
                       WITH  ;
                       w_fchgar
               REPLACE fecgar  ;
                       WITH  ;
                       w_fchfin
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               REPLACE fecing  ;
                       WITH  ;
                       w_fecemi
               UNLOCK
               w_gara = DTOC(fecvta) +  ;
                        ' ' +  ;
                        docgar
          ENDIF
          w_numaux = 'C' +  ;
                     w_codcli
          SELECT st_iclpr
          SET ORDER TO codigo
          SEEK '&w_numaux'
          IF  !FOUND()
               APPEND BLANK
               DO rbloquea
               REPLACE indent  ;
                       WITH 'C',  ;
                       codent  ;
                       WITH  ;
                       w_codcli,  ;
                       noment  ;
                       WITH  ;
                       w_noment,  ;
                       nomcal  ;
                       WITH  ;
                       w_nomcal,  ;
                       nomdis  ;
                       WITH  ;
                       w_nomdis,  ;
                       nomciu  ;
                       WITH  ;
                       w_nomciu,  ;
                       numte1  ;
                       WITH  ;
                       w_numte1,  ;
                       numte2  ;
                       WITH  ;
                       w_numte2
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          w_numaux = STR(w_numero,  ;
                     8)
          SELECT st_sicli
          SEEK w_numaux
          IF FOUND()
               SCAN WHILE numdoc =  ;
                    w_numaux
                    IF ASCAN(mat,  ;
                       codsin) =  ;
                       0
                         DO rbloquea
                         DELETE
                         UNLOCK
                    ENDIF
               ENDSCAN
          ENDIF
          FOR i = 1 TO 15
               IF SUBSTR(mat(i),  ;
                  1, 4) <>  ;
                  SPACE(4)
                    SEEK w_numaux +  ;
                         SUBSTR(mat(i),  ;
                         1, 4)
                    IF  ! ;
                        FOUND()
                         APPEND BLANK
                    ENDIF
                    DO rbloquea
                    REPLACE codsin  ;
                            WITH  ;
                            SUBSTR(mat(i),  ;
                            1, 4),  ;
                            numdoc  ;
                            WITH  ;
                            STR(w_numero,  ;
                            8)
                    REPLACE user  ;
                            WITH  ;
                            users,  ;
                            date  ;
                            WITH  ;
                            DATE(),  ;
                            time  ;
                            WITH  ;
                            TIME()
                    UNLOCK
               ENDIF
          ENDFOR
          IF config_prg = 2
               SELECT st_iorep
               SET ORDER TO ord_numsol
               SEEK w_numaux
               IF FOUND()
                    DO rbloquea
                    REPLACE codent  ;
                            WITH  ;
                            w_codcli
                    REPLACE codemi  ;
                            WITH  ;
                            w_emisor
                    REPLACE codmar  ;
                            WITH  ;
                            w_codmar
                    REPLACE codmod  ;
                            WITH  ;
                            w_codmod
                    REPLACE numser  ;
                            WITH  ;
                            w_numser
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
                    w_auxest = auxest
                    w_numord = numdoc
                    IF indori <>  ;
                       w_indori
                         IF SUBSTR(w_indori,  ;
                            2, 1) <>  ;
                            'R'  ;
                            .AND.  ;
                            (indest =  ;
                            'P'  ;
                            .OR.  ;
                            indest =  ;
                            'V')  ;
                            .AND.  ;
                            auxest <  ;
                            '009'
                              w_antgar =  ;
                               indori
                              DO rbloquea
                              REPLACE  ;
                               indori  ;
                               WITH  ;
                               w_indori
                              UNLOCK
                              SELECT  ;
                               20
                              USE  ;
                               SHARED  ;
                               st_ispre  ;
                               ORDER  ;
                               st_numsol
                              SEEK  ;
                               w_numaux
                              IF FOUND()
                                   DO rbloquea
                                   REPLACE indori WITH w_indori
                                   REPLACE user WITH users
                                   REPLACE date WITH DATE()
                                   REPLACE time WITH TIME()
                                   UNLOCK
                              ENDIF
                         ELSE
                              DO error2  ;
                                 WITH  ;
                                 '** Ir luego a cambiar Tipo de Atenci¢n de O/R. **'
                         ENDIF
                    ENDIF
               ELSE
                    w_auxest = SPACE(4)
               ENDIF
          ENDIF
          SELECT 20
          USE SHARED st_users
          APPEND BLANK
          DO rbloquea
          REPLACE numsol WITH  ;
                  w_numaux
          REPLACE codemp WITH  ;
                  w_codemp
          REPLACE user WITH users
          REPLACE fecha WITH  ;
                  DATE()
          REPLACE hora WITH  ;
                  TIME()
          IF config_prg = 2
               REPLACE numord  ;
                       WITH  ;
                       w_numord
               REPLACE estado  ;
                       WITH  ;
                       w_auxest
               IF w_antgar <>  ;
                  SPACE(4)
                    REPLACE antgar  ;
                            WITH  ;
                            w_antgar
                    REPLACE nuegar  ;
                            WITH  ;
                            w_indori
               ENDIF
          ELSE
               REPLACE estado  ;
                       WITH  ;
                       '000 '
          ENDIF
          UNLOCK
     ENDIF
     IF w_key == -6
          w_print = PRINTSTATUS()
          DO WHILE  !w_print
               DO error2 WITH  ;
                  '** Error en Impresora. Continua ? (S/N) '
               IF LASTKEY() == 27
                    EXIT
               ENDIF
               w_print = PRINTSTATUS()
          ENDDO
          w_print = PRINTSTATUS()
          IF w_print
               DO sele_sol
          ENDIF
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'RAC',  ;
        'BBB', 'IGN', 'ESC'
ENDIF
RETURN
*
FUNCTION val_abo
IF LASTKEY() = 18 .OR. LASTKEY() =  ;
   3
     RETURN .F.
ENDIF
IF w_abonos < 0
     RETURN .F.
ENDIF
RETURN
*
FUNCTION codmod2
PARAMETER cod
codaux = w_codmar + cod
SELECT st_imode
SEEK '&codaux'
IF  !FOUND()
     codauy = 'MARC' + w_codmar
     SELECT ge_tab0
     SEEK '&codauy'
     nu1 = ASC(SUBSTR(tab_parame,  ;
           1, 1))
     nu2 = ASC(SUBSTR(tab_parame,  ;
           2, 1))
     IF LEN(TRIM(cod)) > nu2
          KEYBOARD CHR(25) +  ;
                   SUBSTR(cod,  ;
                   nu1, nu2) +  ;
                   CHR(13)
          RETURN .F.
     ENDIF
     DO error2 WITH  ;
        '** C¢digo Modelo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
@ ROW(), 35 SAY SUBSTR(nommod, 1,  ;
  30)
w_nomtit = nommod
w_codcla2 = codcla
w_linea = linea
w_mesgar = mesgar
DO sacaf6
RETURN .T.
*
FUNCTION numser2
PARAMETER cod
IF cod = SPACE(20)
     RETURN .F.
ENDIF
IF config_prg = 1
     w_provee = SPACE(11)
     w_docume = SPACE(15)
     STORE {} TO w_fchfin,  ;
           w_fchgar
     @ 18, 19 SAY SPACE(50)
     codaux = w_codmar + w_codmod +  ;
              cod
     w_indgar = 0
     SELECT st_isrep
     SET ORDER TO sol_mamose
     SEEK codaux
     IF FOUND()
          SCAN WHILE w_codmar =  ;
               codmar .AND.  ;
               w_codmod = codmod  ;
               .AND. cod = numser  ;
               .AND.  ! ;
               EOF()
               w_indest = indest
               w_numdoc = numdoc
          ENDSCAN
          IF ALLTRIM(w_indest) =  ;
             'V'
               DO error2 WITH  ;
                  'Solicitud  N§ '+ ;
                  w_numdoc+ ;
                  '  esta Vigente'
               RETURN .F.
          ENDIF
     ENDIF
     SELECT st_iorep
     SET ORDER TO ord_mamose
     SEEK codaux
     IF FOUND()
          SCAN WHILE w_codmar =  ;
               codmar .AND.  ;
               w_codmod = codmod  ;
               .AND. numser = cod  ;
               .AND.  ! ;
               EOF()
               IF ALLTRIM(indest) <>  ;
                  'N'
                    w_ndoref = numdoc
                    w_ndosol = numsol
                    w_fdoref = fecemi
                    w_tgara = indori
                    w_fecest = fecest
                    w_auxest = auxest
                    w_indest = indest
                    w_fecent = fecent
                    w_indgar = 1
               ENDIF
          ENDSCAN
          IF (ALLTRIM(w_indest) =  ;
             'C' .OR.  ;
             ALLTRIM(w_indest) =  ;
             'V' .OR.  ;
             ALLTRIM(w_indest) =  ;
             'P') .AND. (w_auxest <=  ;
             '018 ' .OR. w_auxest =  ;
             '021 ' .OR. w_auxest =  ;
             '026 ' .OR. w_auxest =  ;
             '027 ')
               DO error2 WITH  ;
                  'S/S  N§'+ ;
                  w_ndosol+ ;
                  ' esta en Proceso'
               w_indgar = 0
               RETURN .F.
          ENDIF
          IF (ALLTRIM(w_indest) =  ;
             'F' .OR.  ;
             ALLTRIM(w_indest) =  ;
             'B') .AND. w_auxest =  ;
             '080 '
               DO error2 WITH  ;
                  'S/S  N§'+ ;
                  w_ndosol+ ;
                  ' no tiene salida '
               w_indgar = 0
               RETURN .F.
          ENDIF
          IF ((DATE() - w_fecent) >  ;
             w_dcgar)
               w_indgar = 0
          ELSE
               IF (w_auxest =  ;
                  '020 ' .OR.  ;
                  w_auxest =  ;
                  '022 ' .OR.  ;
                  w_auxest =  ;
                  '024 ' .OR.  ;
                  w_auxest =  ;
                  '025 ' .OR.  ;
                  w_auxest =  ;
                  '029 ' .OR.  ;
                  w_auxest =  ;
                  '030 ' .OR.  ;
                  w_auxest =  ;
                  '100 ')
                    SELECT 20
                    USE SHARED  ;
                        st_mvord  ;
                        ORDER  ;
                        estado
                    IF SEEK(w_ndoref +  ;
                       '010 ')
                         w_indgar =  ;
                          1
                    ELSE
                         IF SEEK(w_ndoref +  ;
                            '018 ' ;
                            )
                              w_indgar =  ;
                               1
                         ELSE
                              w_indgar =  ;
                               0
                         ENDIF
                    ENDIF
               ELSE
                    w_indgar = 0
               ENDIF
          ENDIF
          IF w_indgar = 1
               DO CASE
                    CASE w_tgara =  ;
                         'GARA'  ;
                         .OR.  ;
                         w_tgara =  ;
                         'GREC'
                         w_indori =  ;
                          'GREC'
                    CASE w_tgara =  ;
                         'FGAR'  ;
                         .OR.  ;
                         w_tgara =  ;
                         'FREC'
                         w_indori =  ;
                          'FREC'
                    CASE w_tgara =  ;
                         'PVEN'  ;
                         .OR.  ;
                         w_tgara =  ;
                         'PREC'
                         w_indori =  ;
                          'PREC'
               ENDCASE
               SELECT ge_tab0
               SEEK 'INGA' +  ;
                    w_indori
               IF FOUND()
                    w_desate = SUBSTR(tab_destab,  ;
                               1,  ;
                               28) +  ;
                               '==>'
               ELSE
                    w_desate = SPACE(54)
               ENDIF
          ELSE
               DO CASE
                    CASE w_tgara =  ;
                         'GARA'  ;
                         .OR.  ;
                         w_tgara =  ;
                         'GREC'
                         w_indori =  ;
                          'GARA'
                    CASE w_tgara =  ;
                         'FGAR'  ;
                         .OR.  ;
                         w_tgara =  ;
                         'FREC'
                         w_indori =  ;
                          'FGAR'
                    CASE w_tgara =  ;
                         'PVEN'  ;
                         .OR.  ;
                         w_tgara =  ;
                         'PREC'
                         w_indori =  ;
                          'PVEN'
               ENDCASE
          ENDIF
          @ 18, 19 SAY w_desate +  ;
            'S/S-' +  ;
            ALLTRIM(w_ndosol) +  ;
            ' ' + DTOC(w_fdoref)
          IF w_indori = 'GARA'  ;
             .OR. w_indori =  ;
             'PVEN'
               w_gara = 'S/S-' +  ;
                        ALLTRIM(w_ndosol) +  ;
                        ' ' +  ;
                        DTOC(w_fdoref)
               SELECT st_iseri
               SEEK codaux
               IF FOUND()
                    IF fecgar >=  ;
                       DATE()
                         w_provee =  ;
                          codent
                         w_docume =  ;
                          docgar
                         w_fchgar =  ;
                          fecvta
                         w_fchfin =  ;
                          fecgar
                         w_gara =  ;
                          DTOC(fecvta) +  ;
                          ' ' +  ;
                          docgar
                    ELSE
                         w_indgar =  ;
                          0
                         IF w_indori <>  ;
                            'PVEN'
                              w_indori =  ;
                               'FGAR'
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ELSE
          IF w_indori <> 'PVEN'
               w_indori = 'FGAR'
          ENDIF
     ENDIF
     w_tgara = w_indori
ENDIF
RETURN
*
FUNCTION valtabtg
PARAMETER clave, codig, colu,  ;
          largo
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .T.
ENDIF
IF EMPTY(codig)
     RETURN .F.
ENDIF
w_indsal = 0
SELECT ge_tab0
codaux = clave + codig
SEEK codaux
IF  !FOUND()
     DO error2 WITH  ;
        '** C¢digo NO EXISTE**'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
     @ 18, 19 SAY  ;
       SUBSTR(tab_destab, 1, 30)
ENDIF
IF (w_indori = 'GREC' .OR.  ;
   w_indori = 'FREC' .OR.  ;
   w_indori = 'PREC')
     DO valreg
     IF w_indgar = 0
          RETURN .F.
     ENDIF
ENDIF
IF w_indori = 'GARA' .OR.  ;
   w_indori = 'PVEN'
     codaux = w_codmar + w_codmod +  ;
              w_numser
     SELECT st_iseri
     SEEK codaux
     IF FOUND()
          IF fecgar >= DATE()
               w_provee = codent
               w_docume = docgar
               w_fchgar = fecvta
               w_fchfin = fecgar
               w_gara = DTOC(fecvta) +  ;
                        ' ' +  ;
                        docgar
          ELSE
               IF config_prg = 1
                    DO error2  ;
                       WITH  ;
                       '** Fecha Excede el Plazo de Garant¡a del Artefacto **'
                    w_indgar = 0
                    RETURN .F.
               ENDIF
          ENDIF
     ENDIF
     DO tipogt
     IF w_indsal = 1
          RETURN .F.
     ENDIF
ENDIF
IF (w_indori = 'FGAR' .OR.  ;
   w_indori = 'PVEN')
     w_gara = SPACE(10)
ENDIF
DO sacaf6
RETURN .T.
*
FUNCTION valreg
codaux = w_codmar + w_codmod +  ;
         w_numser
w_indgar = 0
w_indo = SPACE(4)
SELECT st_iorep
SET ORDER TO ord_mamose
SEEK codaux
IF FOUND()
     SCAN WHILE w_codmar = codmar  ;
          .AND. w_codmod = codmod  ;
          .AND. numser = w_numser  ;
          .AND.  !EOF()
          IF ALLTRIM(indest) <>  ;
             'N' .AND. numsol <>  ;
             STR(w_numero, 8)
               w_ndoref = numdoc
               w_ndosol = numsol
               w_fdoref = fecemi
               w_tgara = indori
               w_fecest = fecest
               w_auxest = auxest
               w_indest = indest
               w_fecent = fecent
               w_indgar = 1
          ENDIF
     ENDSCAN
     IF ((DATE() - w_fecent) >  ;
        w_dcgar)
          DO error2 WITH  ;
             'Fecha excede el l¡mite de Reclamo'
          w_indgar = 0
          RETURN
     ENDIF
     IF (w_auxest = '020 ' .OR.  ;
        w_auxest = '022 ' .OR.  ;
        w_auxest = '024 ' .OR.  ;
        w_auxest = '025 ' .OR.  ;
        w_auxest = '029 ' .OR.  ;
        w_auxest = '030 ' .OR.  ;
        w_auxest = '100 ')
          SELECT 20
          USE SHARED st_mvord  ;
              ORDER estado
          IF SEEK(w_ndoref +  ;
             '010 ')
               w_indgar = 1
          ELSE
               IF SEEK(w_ndoref +  ;
                  '018 ')
                    w_indgar = 1
               ELSE
                    w_indgar = 0
                    DO error2  ;
                       WITH  ;
                       '*** Solicitud se liquid¢ sin Reparar ***'
                    RETURN
               ENDIF
          ENDIF
     ELSE
          DO error2 WITH  ;
             '*** Solicitud se liquid¢ sin Reparar ***'
          w_indgar = 0
          RETURN
     ENDIF
     IF w_indgar = 1
          DO CASE
               CASE w_tgara =  ;
                    'GARA' .OR.  ;
                    w_tgara =  ;
                    'GREC'
                    w_indo = 'GREC'
               CASE w_tgara =  ;
                    'FGAR' .OR.  ;
                    w_tgara =  ;
                    'FREC'
                    w_indo = 'FREC'
               CASE w_tgara =  ;
                    'PVEN' .OR.  ;
                    w_tgara =  ;
                    'PREC'
                    w_indo = 'PREC'
          ENDCASE
          IF w_indori <> w_indo
               DO error2 WITH  ;
                  'Tipo de Atenci¢n no coincide al Registro Anterior'
               w_indgar = 0
               RETURN
          ENDIF
          SELECT ge_tab0
          SEEK 'INGA' + w_indori
          IF FOUND()
               w_desate = SUBSTR(tab_destab,  ;
                          1, 28) +  ;
                          '==>'
          ELSE
               w_desate = SPACE(54)
          ENDIF
          @ 18, 19 SAY w_desate +  ;
            'S/S-' +  ;
            ALLTRIM(w_ndosol) +  ;
            ' ' + DTOC(w_fdoref)
          w_gara = 'S/S-' +  ;
                   ALLTRIM(w_ndosol) +  ;
                   ' ' +  ;
                   DTOC(w_fdoref)
     ELSE
          IF w_indori <> 'PVEN'
               w_indori = 'FGAR'
          ENDIF
     ENDIF
ELSE
     DO error2 WITH  ;
        '** No existe Registro Anterior **'
     IF w_indori <> 'PVEN'
          w_indori = 'FGAR'
     ENDIF
ENDIF
w_tgara = w_indori
RETURN .T.
*
FUNCTION cliente
PARAMETER colu
IF EMPTY(w_codcli)
     RETURN .F.
ENDIF
w_codcli = SPACE(11 -  ;
           LEN(ALLTRIM(w_codcli))) +  ;
           ALLTRIM(w_codcli)
w_codcli = STR(VAL(w_codcli), 11)
SELECT st_iclpr
SET ORDER TO codigo
codaux = 'C' + w_codcli
SEEK '&codaux'
@ 09, 19 SAY w_codcli PICTURE  ;
  '@!'
IF  !FOUND()
     DEFINE WINDOW error FROM 42,  ;
            10 TO 42, 66 NONE
     ACTIVATE WINDOW error
     CLEAR
     w_sinoes = 'N'
     @ 0, 5 SAY  ;
       'C¢digo de Cliente NO EXISTE. Lo Crea ?  (S/N)'  ;
       GET w_sinoes PICTURE '!'  ;
       VALID w_sinoes $ 'SN'
     READ
     DEACTIVATE WINDOW error
     IF LASTKEY() == 27 .OR.  ;
        w_sinoes <> 'S'
          RETURN .F.
     ELSE
          w_valida = .T.
          RETURN .T.
     ENDIF
ENDIF
w_noment = noment
w_nomcal = nomcal
w_nomdis = nomdis
w_nomciu = nomciu
w_numte1 = numte1
w_numte2 = numte2
@ 09, 31 SAY SUBSTR(w_noment, 1,  ;
  40) PICTURE '@!'
@ 10, 19 SAY w_nomcal PICTURE  ;
  '@!'
@ 11, 19 SAY w_nomdis PICTURE  ;
  '@!'
@ 12, 19 SAY w_nomciu PICTURE  ;
  '@!'
@ 13, 19 SAY w_numte1 PICTURE  ;
  '99999999'
@ 13, 29 SAY w_numte2 PICTURE  ;
  '99999999'
DO sacaf6
w_valida = .F.
RETURN
*
FUNCTION coddes
PARAMETER cod
IF cod <> 'R' .AND. cod <> 'D'
     DO error2 WITH  ;
        '** Ingrese [R]eparaci¢n ¢ [D]omicilio **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF cod == 'R'
     @ ROW(), 21 SAY 'REPARACION'
ELSE
     @ ROW(), 21 SAY 'DOMICILIO '
ENDIF
RETURN .T.
*
FUNCTION poracc
PARAMETER ind
indx = 0
FOR i = 1 TO 15
     IF LEN(ALLTRIM(w_acceso(i))) <>  ;
        0
          indx = i
     ENDIF
ENDFOR
IF ind <= indx + 1
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION ayuda10
ON KEY
IF VARREAD() == 'W_NOMDIS'
     SELECT ge_tab0
     SET ORDER TO descr
     SET FILTER TO tab_codpre == 'DIST'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE DISTRITOS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET ORDER TO codigo
     SET FILTER TO
ENDIF
IF VARREAD() = 'W_NOMCIU'
     SELECT ge_tab0
     SET ORDER TO descr
     SET FILTER TO tab_codpre == 'PROV'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE PROVINCIAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() = 'W_CODMAR'
     SELECT ge_tab0
     SET ORDER TO descr
     SET FILTER TO tab_codpre = 'MARC'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE MARCAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() == 'W_INDORI'
     SELECT ge_tab0
     SET ORDER TO descr
     SET FILTER TO tab_codpre = 'INGA'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE TIPO ATENCION'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() == 'W_CODMON'
     SELECT ge_tab0
     SET ORDER TO codigo
     SET FILTER TO tab_codpre == 'MONE'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE MONEDAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() == 'W_EMISOR'
     SELECT ge_tab0
     SET ORDER TO codigo
     SET FILTER TO tab_codpre == 'EMIS'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA EMISOR'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() == 'W_CODMOD'
     w_select = SELECT()
     SELECT st_imode
     SET FILTER TO codmar == w_codmar
     w_selpro = SELECT()
     w_campo = w_codmar
     DO pro2 WITH w_campo,  ;
        w_select, w_selpro, 1
     IF LASTKEY() <> 27
          w_codmod = w_campo
          KEYBOARD w_codmod
     ENDIF
     SELECT st_imode
     SET FILTER TO
     SET ORDER TO codigo
     SELECT (w_select)
ENDIF
IF VARREAD() = 'W_CODCLI' .OR.  ;
   VARREAD() = 'W_PROVEE'
     w_select = SELECT()
     IF VARREAD() == 'W_CODCLI'
          varcod = 'C'
          titulo = 'AYUDA DE CLIENTES'
     ELSE
          varcod = 'P'
          titulo = 'AYUDA DE PROVEEDORES'
     ENDIF
     SELECT st_iclpr
     SET FILTER TO indent = varcod
     w_selpro = SELECT()
     w_campo = varcod
     DO ayuent WITH w_campo,  ;
        w_select, w_selpro, 1
     IF LASTKEY() <> 27
          IF varcod = 'C'
               w_codcli = w_campo
               KEYBOARD w_codcli
          ELSE
               w_provee = w_campo
               KEYBOARD w_provee
          ENDIF
     ENDIF
     SELECT st_iclpr
     SET FILTER TO
     SET ORDER TO codigo
     SELECT (w_select)
ENDIF
IF VARREAD() == 'W_NUMERO' .AND.  ;
   config_prg <> 1
     SELECT st_isrep
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+substr(numser,1,15)+" "+codent+" "+substr(codmod,1,15)+" "+subst(indest,1,2)+" "+indori'
     w_origen = 'SS'
     w_orden = ORDER()
     DO ayuda8 WITH campoa,  ;
        w_origen, SELECT()
     SELECT st_isrep
     set order to &w_orden
ENDIF
ON KEY LABEL f6 do ayuda10
RETURN .T.
*
PROCEDURE sele_sol
SET CONSOLE OFF
SET DEVICE TO PRINTER
SET PRINTER ON
SET PRINTER TO lpt1
@ PROW(), PCOL() SAY CHR(15)
@ PROW(), PCOL() SAY CHR(27) +  ;
  'C' + CHR(33)
DO imp_sol1
EJECT
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
RETURN
*
PROCEDURE imp_sol1
w_ciu_2 = SPACE(40)
codauy = 'PROV' + w_nomciu
SELECT ge_tab0
SEEK '&codauy'
IF FOUND()
     w_ciu_2 = SUBSTR(tab_destab,  ;
               1, 30)
ELSE
     w_ciu_2 = SPACE(30)
ENDIF
SEEK 'DIST' + w_nomdis
IF FOUND()
     w_desdis = SUBSTR(tab_destab,  ;
                1, 30)
ELSE
     w_desdis = SPACE(30)
ENDIF
SEEK 'MARC' + w_codmar
IF FOUND()
     w_desmar = SUBSTR(tab_destab,  ;
                1, 30)
ELSE
     w_desmar = SPACE(30)
ENDIF
SEEK 'INGA' + w_indori
IF FOUND()
     w_destia = SUBSTR(tab_destab,  ;
                1, 30)
ELSE
     w_destia = SPACE(30)
ENDIF
tit_client = w_noment
tit_codigo = w_codcli
tit_direcc = w_nomcal
tit_codgeo = w_ciu_2
tit_feccom = w_feccom
tit_tel1 = w_numte1
tit_tel2 = w_numte2
tit_numser = w_numser
tit_tit1 = 'DESTINO : '
tit_destin = IIF(w_coddes == 'R',  ;
             'REPARACION',  ;
             'DOMICILIO')
w_fecha = DTOC(w_fecemi)
w_hora = w_horemi
tit_tit2 = 'SINTOMAS :'
tit_tit3 = 'ACCESORIOS :'
tit_tit4 = 'OBSERVACIONES :'
IF w_coddes = 'R'
     tit_tit5 = 'SOLICITUD  DE  SERVICIO   -   ***** DUPLICADO *****'
ELSE
     tit_tit5 = 'SOLICITUD DE SERVICIO A DOMICILIO - ** DUPLICADO **'
ENDIF
tit_fecha = w_fecha + ' - ' +  ;
            w_hora
tit_eminro = 'EMISOR :' +  ;
             w_emisor + ' Nro.: ' +  ;
             STR(w_numero, 8)
??? CHR(15)
DO pormsol
??? CHR(15)
RETURN
*
PROCEDURE pago_acta
ON KEY
DEFINE WINDOW acta FROM 23, 13 TO  ;
       30, 63 TITLE  ;
       'µ PAGO A CUENTA Æ'  ;
       DOUBLE
ACTIVATE WINDOW acta
STORE SPACE(4) TO w_docvta
STORE '002 ' TO w_concep
STORE 0 TO w_valor
@ 02, 01 SAY  ;
  'Tipo de Doc. de Venta .... :'  ;
  COLOR W+/N 
@ 03, 01 SAY  ;
  'Concepto a Facturar ...... :'  ;
  COLOR W+/N 
SET CURSOR ON
@ 02, 30 GET w_docvta PICTURE  ;
  '@!' VALID despues(1,w_docvta)  ;
  WHEN antes(1)
@ 03, 30 GET w_concep PICTURE  ;
  '@!' VALID despues(2,w_docvta)  ;
  WHEN antes(2)
READ
IF LASTKEY() = 27 .OR. w_valor =  ;
   1
     DEACTIVATE WINDOW acta
     RELEASE WINDOW acta
     RETURN
ENDIF
DEACTIVATE WINDOW acta
RELEASE WINDOW acta
RETURN
*
PROCEDURE antes
PARAMETER opc
IF opc = 2
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'ACTA';
.AND. (tab_codtab = '001';
.OR. tab_codtab = '002';
.OR. tab_codtab = '003')
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE CONCEPTOS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
RETURN
*
FUNCTION despues
PARAMETER opc, w_docvta
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(w_docvta)
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' + w_docvta
          IF  !FOUND()
               DO error2 WITH  ;
                  'Documento de Venta No Existe'
               RETURN .F.
          ENDIF
     CASE opc = 2
          IF LASTKEY() = 27
               w_valor = 1
               RETURN .T.
          ENDIF
          CLEAR READ
ENDCASE
*
PROCEDURE calculo
ON KEY
IF w_codmon = 'SOL '
     w_mobra = ROUND(ROUND(w_abonos /  ;
               (1 + w_facigv), 2) /  ;
               w_tipcam, 2)
     w_totgen = ROUND(w_mobra *  ;
                (1 + w_facigv),  ;
                2)
     w_valvta = ROUND(w_totgen /  ;
                (1 + w_facigv),  ;
                2)
     w_igv = w_totgen - w_valvta
     sol_totgen = w_abonos
     sol_totvta = ROUND(sol_totgen /  ;
                  (1 + w_facigv),  ;
                  2)
     sol_igv = sol_totgen -  ;
               sol_totvta
     w_abonos = ROUND(w_abonos /  ;
                w_tipcam, 2)
ELSE
     w_mobra = ROUND(w_abonos /  ;
               (1 + w_facigv),  ;
               2)
     w_totgen = w_abonos
     w_valvta = ROUND(w_totgen /  ;
                (1 + w_facigv),  ;
                2)
     w_igv = w_totgen - w_valvta
     sol_totgen = ROUND(ROUND((w_mobra *  ;
                  w_tipcam), 2) *  ;
                  (1 + w_facigv),  ;
                  2)
     sol_totvta = ROUND(sol_totgen /  ;
                  (1 + w_facigv),  ;
                  2)
     sol_igv = sol_totgen -  ;
               sol_totvta
ENDIF
RETURN
*
PROCEDURE stat_print
ON KEY
w_printer = '1'
DO WHILE w_printer='1'
     SELECT 7
     USE
     USE SHARED st_iparg
     IF w_docvta = 'FACT'
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
          IF w_docvta = 'FACT'
               REPLACE sys_lptfac  ;
                       WITH '1'
          ELSE
               REPLACE sys_lptbol  ;
                       WITH '1'
          ENDIF
          UNLOCK
     ENDIF
ENDDO
DO rbloquea
IF w_docvta = 'FACT'
     REPLACE sys_numfac WITH  ;
             sys_numfac + 1
     w_numdoc = STR(sys_numfac,  ;
                10)
ELSE
     REPLACE sys_nrobol WITH  ;
             sys_nrobol + 1
     w_numdoc = STR(sys_nrobol,  ;
                10)
ENDIF
UNLOCK
RETURN
*
PROCEDURE graba
SELECT 20
USE SHARED st_iredo ORDER codigo
APPEND BLANK
DO rbloquea
REPLACE indodo WITH 'SSE '
REPLACE numodo WITH STR(w_numero,  ;
        8)
REPLACE indddo WITH w_docvta
REPLACE numddo WITH  ;
        f_ceros(VAL(w_numdoc),10, ;
        1)
REPLACE user WITH users
REPLACE date WITH DATE()
REPLACE time WITH TIME()
UNLOCK
SELECT 20
USE SHARED gc_hve00 ORDER codigo
APPEND BLANK
DO rbloquea
REPLACE hve_tipdoc WITH w_docvta
REPLACE hve_nrodoc WITH  ;
        f_ceros(VAL(w_numdoc),10, ;
        1)
REPLACE hve_tidore WITH 'SOLI'
REPLACE hve_nrdore WITH  ;
        STR(w_numero, 8)
REPLACE hve_fecdoc WITH DATE()
REPLACE hve_fecvct WITH DATE()
REPLACE hve_codemi WITH w_emisor
REPLACE hve_tipent WITH 'C   '
REPLACE hve_codent WITH w_codcli
REPLACE hve_tippag WITH '001 '
REPLACE hve_estdoc WITH 'O'
REPLACE hve_codmov WITH 'PCTA'
REPLACE hve_totnet WITH w_abonos
REPLACE hve_totvta WITH w_valvta
REPLACE hve_totigv WITH w_igv
REPLACE hve_totgen WITH w_abonos
REPLACE hve_totoim WITH w_abonos
REPLACE hve_codmon WITH w_codmon
REPLACE hve_fechtc WITH DATE()
REPLACE hve_indori WITH 'FGAR'
REPLACE hve_solnet WITH  ;
        sol_totgen
REPLACE hve_solvta WITH  ;
        sol_totvta
REPLACE hve_soligv WITH sol_igv
REPLACE hve_solgen WITH  ;
        sol_totgen
REPLACE hve_tipcam WITH w_tipcam
REPLACE hve_mtocan WITH  ;
        sol_totgen
REPLACE hve_codcta WITH w_concep
IF w_concep = '005'
     REPLACE hve_flete WITH  ;
             w_totgen
     REPLACE hve_solfle WITH  ;
             sol_totgen
ELSE
     REPLACE hve_cosmob WITH  ;
             w_mobra
     REPLACE hve_solmob WITH  ;
             sol_totvta
ENDIF
REPLACE hve_usuari WITH users
REPLACE hve_fecha WITH DATE()
REPLACE hve_hora WITH TIME()
UNLOCK
RETURN
*
PROCEDURE imprime
ON KEY
SET CONSOLE OFF
IF w_docvta = 'FACT'
     SET PRINT TO &RGE_LPTFAC
ELSE
     SET PRINT TO &RGE_LPTBOL
ENDIF
SET DEVICE TO PRINTER
SET PRINTER ON
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(67) + CHR(51)
? CHR(18)
DO pormpta
EJECT
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
SELECT st_iparg
DO rbloquea
IF w_docvta = 'FACT'
     REPLACE sys_lptfac WITH '0'
ELSE
     REPLACE sys_lptbol WITH '0'
ENDIF
UNLOCK
RETURN
*
PROCEDURE tipogt
DEFINE WINDOW gtia FROM 05, 30 TO  ;
       10, 75
w_indsal = 0
IF w_indori = 'GARA' .OR.  ;
   w_indori = 'GREC' .OR.  ;
   w_indori = 'PVEN' .OR.  ;
   w_indori = 'PREC'
     ACTIVATE WINDOW gtia
     @ 00, 01 SAY 'Proveedor :'
     @ 01, 01 SAY 'Doc.Gtia. :'
     @ 02, 01 SAY 'Fecha Vta.:'
     @ 03, 01 SAY 'Fecha Fin :'
     @ 00, 13 GET w_provee  ;
       PICTURE '@!' VALID  ;
       leegar(1)
     @ 01, 13 GET w_docume VALID  ;
       leegar(2)
     @ 02, 13 GET w_fchgar VALID  ;
       leegar(3)
     @ 03, 13 GET w_fchfin VALID  ;
       leegar(4)
     READ
     IF LASTKEY() = 27
          DEACTIVATE WINDOW gtia
          w_indsal = 1
          RETURN
     ENDIF
     DEACTIVATE WINDOW gtia
ENDIF
RETURN
*
FUNCTION leegar
PARAMETER opc
DO CASE
     CASE opc = 1
          IF EMPTY(w_provee)
               DO error2 WITH  ;
                  'No se Aceptan Blancos'
               RETURN .F.
          ENDIF
          w_provee = SPACE(11 -  ;
                     LEN(ALLTRIM(w_provee))) +  ;
                     ALLTRIM(w_provee)
          SELECT st_iclpr
          SET ORDER TO codigo
          SEEK 'P' + w_provee
          IF  !FOUND()
               DO error2 WITH  ;
                  'C¢digo de Proveedor No Existe'
               RETURN .F.
          ENDIF
          @ 00, 24 SAY  ;
            SUBSTR(noment, 1,  ;
            20)
     CASE opc = 2
          IF EMPTY(w_docume)
               DO error2 WITH  ;
                  'No se Aceptan Blancos'
               RETURN .F.
          ENDIF
     CASE opc = 3
          IF EMPTY(w_fchgar)
               DO error2 WITH  ;
                  'No se Aceptan Blancos'
               RETURN .F.
          ENDIF
          IF w_fchgar > DATE()
               DO error2 WITH  ;
                  'No puede ser mayor que la fecha Actual'
               RETURN .F.
          ENDIF
          IF EMPTY(w_fchfin)
               IF w_mesgar > 0
                    w_fchfin = GOMONTH(w_fchgar,  ;
                               w_mesgar)
               ELSE
                    w_fchfin = GOMONTH(w_fchgar,  ;
                               12)
               ENDIF
          ENDIF
     CASE opc = 4
          IF EMPTY(w_fchfin)
               DO error2 WITH  ;
                  'No se Aceptan Blancos'
               RETURN .F.
          ENDIF
          IF w_fchfin < w_fchgar
               DO error2 WITH  ;
                  'Fecha de Fin de Gtia. Debe ser Mayor'
               RETURN .F.
          ENDIF
          IF w_fchfin < DATE()
               DO error2 WITH  ;
                  'Fecha de Fin de Gtia. es Menor a la Fecha Actual'
               RETURN .F.
          ENDIF
          IF (w_fchfin -  ;
             w_fchgar) > empre12
               DO error2 WITH  ;
                  'Fecha no puede ser mayor a '+ ;
                  STR(empre12, 8,  ;
                  0)+' d¡as'
               RETURN .F.
          ENDIF
          IF w_fchfin = w_fchgar
               DO error2 WITH  ;
                  'Fecha no puede ser igual a la fecha de venta'
               RETURN .F.
          ENDIF
ENDCASE
RETURN .T.
*
PROCEDURE bus_nro
SELECT ge_tab0
IF SUBSTR(w_emisor, 1, 1) = '2'  ;
   .AND. w_coddes = 'D'
     SEEK 'EMIS' + '202 '
ELSE
     IF SUBSTR(w_emisor, 1, 1) =  ;
        '2'
          SEEK 'EMIS' + '200 '
     ELSE
          SEEK 'EMIS' + w_emisor
     ENDIF
ENDIF
IF FOUND()
     DO rbloquea
     REPLACE tab_factor WITH  ;
             tab_factor + 1
     UNLOCK
     w_numero = tab_factor
ELSE
     w_numero = 0
ENDIF
RETURN
*
PROCEDURE sintoma
DEFINE WINDOW sintoma FROM 17, 25  ;
       TO 41, 74 TITLE  ;
       ' S I N T O M A S '
ACTIVATE WINDOW sintoma
CREATE CURSOR sintoma (marca C  ;
       (1), codigo C (4), descri  ;
       C (40))
SELECT st_sint
SEEK w_linea
SCAN WHILE w_linea = linea .AND.   ;
     !EOF()
     SELECT sintoma
     APPEND BLANK
     REPLACE codigo WITH  ;
             st_sint.codsin
     REPLACE descri WITH  ;
             st_sint.dessin
     SELECT st_sint
ENDSCAN
nunmat = 0
FOR i = 1 TO 15
     tem( i) = SPACE(45)
ENDFOR
ON KEY LABEL enter do v_mar
ON KEY LABEL spacebar do v_mar
ON KEY LABEL home do  posi with 1
ON KEY LABEL end do posi with 2
SELECT sintoma
GOTO TOP
BROWSE FIELDS marca :R :H = ' ',  ;
       codigo :R :H = 'COD.',  ;
       descri :R :H =  ;
       'DESCRIPCION' FREEZE  ;
       codigo IN sintoma
ON KEY LABEL enter
ON KEY LABEL home
ON KEY LABEL end
ON KEY LABEL spacebar
DEACTIVATE WINDOW sintoma
IF w_mat = 0
     l = 1
ELSE
     l = w_mat
ENDIF
FOR i = 1 TO nunmat
     IF l <= 14
          l = l + 1
          mat( l) = 'L' + tem(i)
     ENDIF
ENDFOR
IF w_mat = 0
     cod = SUBSTR(mat(1), 1, 4)
ELSE
     cod = SUBSTR(mat(w_mat), 1,  ;
           4)
ENDIF
DO mostrar WITH 1
RETURN
*
PROCEDURE v_mar
IF marca = SPACE(1)
     IF nunmat + w_mat = 15
          DO error2 WITH  ;
             'Traspas¢ el N§ M ximo de S¡ntomas'
          RETURN
     ELSE
          nunmat = nunmat + 1
          REPLACE marca WITH 'û'
          tem( nunmat) =  ;
             ALLTRIM(codigo) +  ;
             '³' + descri
     ENDIF
ELSE
     REPLACE marca WITH ' '
     pos = ASCAN(tem,  ;
           ALLTRIM(codigo))
     FOR x = pos TO nunmat - 1
          tem( x) = tem(x + 1)
     ENDFOR
     tem( nunmat) = SPACE(45)
     nunmat = nunmat - 1
ENDIF
RETURN
*
PROCEDURE posi
PARAMETER opc
IF opc = 1
     GOTO TOP
ELSE
     GOTO BOTTOM
ENDIF
RETURN
*
PROCEDURE mostrar
PARAMETER opc
p = 0
FOR x = 1 TO 15
     IF SUBSTR(mat(x), 1, 4) <>  ;
        SPACE(4)
          p = p + 1
          @ 23 + p, 1 SAY  ;
            SUBSTR(mat(x), 1, 35)  ;
            PICTURE '@!'
     ENDIF
ENDFOR
RETURN
*
PROCEDURE ingsin
cod = SPACE(4)
w_flag = 0
w_mat = 0
= colocaf6()
ON KEY LABEL F6 DO SINTOMA
IF config_prg = 1
     DO sintoma
ENDIF
FOR w_mat = 1 TO 15
     cod = SUBSTR(mat(w_mat), 1,  ;
           4)
     @ 23 + w_mat, 01 GET cod  ;
       PICTURE '@!' VALID  ;
       valmat(cod,w_mat)
     READ
     IF LASTKEY() = 27
          EXIT
     ELSE
          IF cod = SPACE(4)
               IF w_mat <= 14  ;
                  .AND.  ;
                  SUBSTR(mat(w_mat +  ;
                  1), 1, 4) <>  ;
                  SPACE(4)
                    FOR p = w_mat  ;
                        TO 15
                         IF p <=  ;
                            14  ;
                            .AND.  ;
                            SUBSTR(mat(p +  ;
                            1), 1,  ;
                            4) <>  ;
                            SPACE(4)
                              mat(  ;
                                 p) =  ;
                                 mat(p +  ;
                                 1)
                              @ 23 +  ;
                                p,  ;
                                1  ;
                                SAY  ;
                                SUBSTR(mat(p),  ;
                                1,  ;
                                35)  ;
                                COLOR  ;
                                W+/ ;
                                N 
                         ELSE
                              mat(  ;
                                 p) =  ;
                                 SPACE(45)
                              @ 23 +  ;
                                p,  ;
                                1  ;
                                SAY  ;
                                SPACE(35)
                         ENDIF
                    ENDFOR
                    w_mat = w_mat -  ;
                            1
               ELSE
                    EXIT
               ENDIF
          ENDIF
     ENDIF
     IF w_mat > 0
          @ 23 + w_mat, 01 SAY  ;
            SUBSTR(mat(w_mat), 1,  ;
            35) COLOR W+/N 
     ENDIF
ENDFOR
FOR p = w_mat TO 15
     mat( p) = SPACE(45)
     @ 23 + p, 1 SAY SPACE(35)
ENDFOR
DO mostrar WITH 1
ON KEY LABEL F6 do ayuda10
RETURN
*
FUNCTION valmat
PARAMETER codi, i
IF codi <> SPACE(4)
     pos = ASCAN(mat, codi)
     IF pos <> 0 .AND. i <> pos
          DO error2 WITH  ;
             ' ***C¢digo de S¡ntoma Repetido***'
          RETURN .F.
     ELSE
          SELECT st_sint
          SEEK w_linea +  ;
               SUBSTR(codi, 2,  ;
               3)
          IF  !FOUND()
               DO error2 WITH  ;
                  '***C¢digo no Existe***'
               RETURN .F.
          ELSE
               mat( i) = codi +  ;
                  '³' + dessin
          ENDIF
     ENDIF
ENDIF
RETURN
*
PROCEDURE eli0201
pp = .T.
DO WHILE pp
     w_inkey = 0
     DO WHILE  !(STR(w_inkey,  ;
        2)$ ;
        ' 18, 3, 1, 6,27,-1,-9,-2,-4' ;
        )
          w_inkey = INKEY(0)
     ENDDO
     DO CASE
          CASE w_inkey == 18
               DO mueve3 WITH  ;
                  CHR(w_inkey)
          CASE w_inkey == 3
               DO mueve3 WITH  ;
                  CHR(w_inkey)
          CASE w_inkey == 27 .OR.  ;
               w_inkey == -9
               pp = .F.
               LOOP
          CASE w_inkey == -1
               DO emplea
               IF LASTKEY() = 27
                    LOOP
               ENDIF
               DEFINE WINDOW  ;
                      error FROM  ;
                      42, 10 TO  ;
                      42, 66  ;
                      NONE
               ACTIVATE WINDOW  ;
                        error
               CLEAR
               w_sinoes = 'N'
               @ 0, 15 SAY  ;
                 'Confirma Anular ?  (S/N)'  ;
                 GET w_sinoes  ;
                 PICTURE '!'  ;
                 VALID w_sinoes $  ;
                 'SN'
               SET CURSOR ON
               READ
               SET CURSOR OFF
               IF LASTKEY() == 27
                    w_sinoes = 'N'
               ENDIF
               pp = .F.
               DEACTIVATE WINDOW  ;
                          error
               IF w_sinoes = 'S'  ;
                  .OR. w_sinoes =  ;
                  's'
                    SELECT st_isrep
                    SET ORDER TO codigo
                    w_numaux = STR(w_numero,  ;
                               8)
                    seek '&w_numaux'
                    DO rbloquea
                    REPLACE indest  ;
                            WITH  ;
                            'N   '
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
          OTHERWISE
               DO mueve3 WITH  ;
                  CHR(w_inkey)
     ENDCASE
ENDDO
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
IF fec < w_fecemi
     DO error2 WITH  ;
        '** No puede ser Menor a la Fecha de Emisi¢n **'
     RETURN .F.
ENDIF
IF fec > w_fecemi + empre7
     DO error2 WITH  ;
        '** Fecha excede el tiempo de garant¡a '+ ;
        TRANSFORM(empre7,  ;
        '99999')+' D¡as ***'
     RETURN .F.
ENDIF
RETURN
*
FUNCTION valtab
PARAMETER clave, codig, colu,  ;
          largo
IF EMPTY(codig)
     RETURN .F.
ENDIF
IF LASTKEY() = 18 .OR. LASTKEY() =  ;
   3
     RETURN .F.
ENDIF
SELECT ge_tab0
codaux = clave + codig
SEEK '&codaux'
IF  !FOUND()
     DO error WITH  ;
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
FUNCTION valtab2
PARAMETER clave, codig, colu,  ;
          largo
SELECT ge_tab0
codaux = clave + codig
SEEK '&codaux'
IF  !FOUND()
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
IF SUBSTR(w_emisor, 1, 2) = '27'  ;
   .AND. (w_indori = 'GARA' .OR.  ;
   w_indori = 'PVEN')
     @ 17, 55 SAY 'PRODUCTO :'  ;
       GET w_numstk PICTURE  ;
       '99999'
     READ
     IF LASTKEY() = 27
          RETURN .F.
     ENDIF
ENDIF
DO sacaf6
RETURN .T.
*
PROCEDURE col_bk1b
SELECT ge_tab0
w_aux = 'EMIS' + w_emisor
SEEK '&w_aux'
IF FOUND()
     w_nomemi = tab_destab
ELSE
     w_nomemi = SPACE(35)
ENDIF
w_aux = 'ESTA' + w_indest
SEEK '&w_aux'
IF FOUND()
     w_estado = tab_destab
ELSE
     w_estado = SPACE(35)
ENDIF
w_aux = 'MONE' + w_codmon
SEEK '&w_aux'
IF FOUND()
     w_nommon = tab_destab
     w_desmon = SUBSTR(w_nommon,  ;
                1, 12)
ELSE
     w_nommon = SPACE(35)
     w_desmon = SPACE(12)
ENDIF
w_aux = 'DIST' + w_nomdis
SEEK '&w_aux'
IF FOUND()
     w_desdis = tab_destab
ELSE
     w_desdis = SPACE(35)
ENDIF
w_aux = 'PROV' + w_nomciu
SEEK '&w_aux'
IF FOUND()
     w_desciu = tab_destab
ELSE
     w_desciu = SPACE(35)
ENDIF
DO coloca WITH 01, 66,  ;
   STR(w_numero, 8)
DO coloca WITH 16, 20,  ;
   DTOC(w_fecemi)
DO coloca WITH 16, 38,  ;
   SUBSTR(TIME(), 1, 5)
DO coloca WITH 18, 20, w_emisor
DO coloca WITH 18, 25,  ;
   SUBSTR(w_nomemi, 1, 24)
DO coloca WITH 02, 01,  ;
   SUBSTR(w_estado, 1, 06)
DO coloca WITH 05, 20, w_codmar
DO coloca WITH 06, 20, w_codmod
DO coloca WITH 07, 20, w_numser
DO coloca WITH 08, 20, w_indori
DO coloca WITH 08, 25,  ;
   STR(w_numstk, 9)
w_aux = 'MARC' + w_codmar
SEEK '&w_aux'
IF FOUND()
     DO coloca WITH 05, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
w_aux = 'INGA' + w_indori
seek '&w_aux'
IF FOUND()
     DO coloca WITH 08, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
w_aux = w_codmar + w_codmod
SELECT st_imode
SEEK '&w_aux'
IF FOUND()
     DO coloca WITH 06, 36,  ;
        SUBSTR(nommod, 1, 30)
ENDIF
SELECT ge_tab0
SEEK 'INGA' + w_indori
IF FOUND()
     DO coloca WITH 19, 20,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ELSE
     DO coloca WITH 19, 20,  ;
        SPACE(30)
ENDIF
DO coloca WITH 10, 20, w_codcli
DO coloca WITH 10, 32,  ;
   SUBSTR(w_noment, 1, 40)
DO coloca WITH 11, 20, w_nomcal
DO coloca WITH 12, 20, w_nomdis
DO coloca WITH 12, 25, w_desdis
DO coloca WITH 13, 20, w_nomciu
DO coloca WITH 13, 25, w_desciu
DO coloca WITH 14, 20,  ;
   STR(w_numte1, 8)
DO coloca WITH 14, 30,  ;
   STR(w_numte2, 8)
DO coloca WITH 16, 64,  ;
   DTOC(w_feccom)
DO coloca WITH 20, 20,  ;
   TRANSFORM(w_abonos,  ;
   '99,999,999.99')
DO coloca WITH 20, 44, w_codmon
DO coloca WITH 20, 49,  ;
   SUBSTR(w_nommon, 1, 15)
DO coloca WITH 21, 20, w_coddes+ ;
   ' '+IIF(w_coddes=='R',  ;
   'REPARACION ', 'DOMICILIO')
FOR i = 1 TO 15
     DO coloca WITH 24+i, 2,  ;
        w_codsin(i)
ENDFOR
FOR i = 1 TO 15
     DO coloca WITH 24+i, 38,  ;
        w_acceso(i)
ENDFOR
RETURN
*
PROCEDURE col_bk2b
FOR i = 1 TO 06
     DO coloca WITH 41+i, 2,  ;
        w_observ(i)
ENDFOR
RETURN
*
PROCEDURE ayuent
PARAMETER wrk_campo, wrk_selec,  ;
          wrk_selpro, wrk_nropro
ON KEY
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
IF wrk_nropro = 1
     DEFINE WINDOW produ FROM 12,  ;
            18 TO 20, 73 IN  ;
            screen COLOR SCHEME  ;
            8
ELSE
     DEFINE WINDOW produ FROM 12,  ;
            18 TO 25, 73 IN  ;
            screen COLOR SCHEME  ;
            8
ENDIF
DEFINE POPUP prod FROM 16, 31
DEFINE BAR 1 OF prod PROMPT  ;
       '\<C¢digo '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripci¢n '
ON SELECTION POPUP prod do buscli with;
bar(), wrk_selpro, wrk_campo
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
IF LASTKEY() <> 27
     wrk_campo = st_iclpr.codent
ENDIF
SELECT (wrk_selec)
RETURN
*
PROCEDURE buscli
PARAMETER bar, wrk_selpro,  ;
          wrk_marca
ON KEY
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, -1
ENDFOR
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY -1, 0
ENDFOR
ACTIVATE WINDOW pide
SELECT (wrk_selpro)
IF BAR() = 1
     wrk_codpro = SPACE(9)
     SET ORDER TO CODIGO
     @ 00, 00 SAY 'C¢digo :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF BAR() = 2
     wrk_codpro = SPACE(30)
     SET ORDER TO cli_noment
     @ 00, 00 SAY 'Descr. :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
READ
IF LASTKEY() <> 27
     IF BAR() = 1
          wrk_codpro = SPACE(9 -  ;
                       LEN(ALLTRIM(wrk_codpro))) +  ;
                       ALLTRIM(wrk_codpro)
          wrk_codigo = 'wrk_marca + wrk_codpro'
     ELSE
          wrk_codigo = 'wrk_marca + wrk_codpro'
     ENDIF
     SET NEAR ON
     seek &wrk_codigo
     SET NEAR OFF
     ACTIVATE WINDOW                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
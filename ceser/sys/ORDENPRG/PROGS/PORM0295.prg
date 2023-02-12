*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
IF SUBSTR(users, 1, 3) <> 'GER'  ;
   .AND. SUBSTR(users, 1, 3) <>  ;
   'MAR' .AND. SUBSTR(users, 1,  ;
   3) <> 'ADM'
     RETURN
ENDIF
titu1 = ' MANTENCION '
titu2 = ' SALIDA POR CORTESIA '
wrk_progra = PROGRAM()
CLOSE DATABASES
DO crea_win
ON KEY LABEL F6 do ayuda12
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, titu1
DO saycenter WITH 2, titu2
PUBLIC wrk_codemp
STORE SPACE(05) TO wrk_codemp
STORE SPACE(20) TO wrk_nomemp
DO esc_modo WITH 'I'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
@ 6, 10 CLEAR TO 10, 65
@ 6, 10 TO 10, 65
SAVE SCREEN TO wk_panta
USE
USE ge_tab0 ORDER CODIGO
SEEK 'IGV ' + 'IGV '
IF FOUND()
     wrk_igv = (tab_factor / 100)
ELSE
     DO error WITH  ;
        '** No est  Definido el IGV **'
     CLOSE DATABASES
     DO sacawin
     RETURN
ENDIF
USE
wrk_fchpar = DATE()
USE GC_CMV00 ORDER cmv_feinmo
SEEK DTOS(wrk_fchpar) + '1' +  ;
     'SOL ' + 'DOL '
wk_tipcam = 3.25 
USE
ppal = .T.
PUBLIC tecnico, muestrx
DO WHILE ppal
     CLOSE DATABASES
     RESTORE SCREEN FROM wk_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     efecin = 1
     STORE 0 TO wk_numord
     STORE SPACE(15) TO  ;
           wrk_codbla
     muestrx = ' '
     @ 8, 20 SAY  ;
       ' N§ Orden Reparaci¢n '  ;
       GET wk_numord PICTURE  ;
       '99999999' VALID  ;
       numord2(wk_numord) WHEN  ;
       colocaf6()
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
     DO usedbf WITH 'st_iorep',  ;
        'CODIGO'
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
     wk_cosrep = cosrep
     wk_cosmob = cosmob
     USE
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
          DIMENSION wk_obsord(  ;
                    06)
          wk_numaux = STR(wk_numero,  ;
                      8)
          DO usedbf WITH  ;
             'st_isrep',  ;
             'CODIGO'
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
                      9)
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
          SELECT 2
          USE SHARED st_sint  ;
              ORDER CODIGO
          SELECT 1
          USE SHARED st_sicli  ;
              ORDER CODIGO
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
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'MOD', 'BBB', 'IGN',  ;
             'ESC'
          ppal2 = .T.
          DO modiord
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
          DIMENSION wk_obsord(  ;
                    06)
          DIMENSION pro( 12),  ;
                    dex( 12),  ;
                    uni( 12),  ;
                    can( 12),  ;
                    pre( 12),  ;
                    dec( 12),  ;
                    tot( 12),  ;
                    sto( 12)
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
               sto( i) = 0
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
          wk_cliaux = 'C' +  ;
                      STR(wk_codcli,  ;
                      9)
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
          USE SHARED st_sint  ;
              ORDER CODIGO
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
          wk_totgrl = wk_totnet +  ;
                      wk_totman
          wk_totigv = totigv
          wk_totafe = wk_totgrl -  ;
                      wk_totigv
          FOR i = 1 TO 6
               wk_obspre( i) =  ;
                        SUBSTR(observ,  ;
                        1 + ((i -  ;
                        1) * 45),  ;
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
               sto( i) = 0
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
          SEEK 'INGA' + wk_indori
          wk_destia = SUBSTR(tab_destab,  ;
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
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'MOD', 'BBB', 'IGN',  ;
             'ESC'
          ppal2 = .T.
          DO modiord
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
DO sacawin
RELEASE tecnico
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
@ 13, 09 CLEAR TO 27, 65
@ 13, 09 TO 27, 65
@ 13, 21 SAY  ;
  ' Salida Pendiente de Facturaci¢n '  ;
  COLOR SCHEME 8
wk_oaux = wk_indori
DO usedbf WITH 'st_iorep',  ;
   'CODIGO'
wk_numaux = STR(wk_numord, 8)
SEEK '&wk_numaux'
wk_ogar = indori
wk_oest = auxest
wk_otec = VAL(codtec)
FOR i = 1 TO 6
     wk_obsord( i) =  ;
              SUBSTR(observ, 1 +  ;
              ((i - 1) * 38),  ;
              38)
     wk_obsord( i) = wk_obsord(i) +  ;
              SPACE(38 -  ;
              LEN(wk_obsord(i)))
ENDFOR
USE
wk_obux = 'ESOR' + wk_oest
DO usedbf WITH 'ge_tab0',  ;
   'codigo'
SEEK '&wk_obux'
@ 18, 35 SAY SUBSTR(tab_destab, 1,  ;
  28)
wk_obux = STR(wk_otec, 9)
SEEK 'INGA' + wk_ogar
wk_destia = SUBSTR(tab_destab, 1,  ;
            30)
USE SHARED st_itecn ORDER CODIGO
SEEK '&wk_obux'
@ 19, 35 SAY SUBSTR(noment, 1,  ;
  28)
USE
@ 17, 35 SAY wk_destia
@ 15, 11 SAY 'N§ Orden    : ' +  ;
  STR(wk_numord, 8)
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(DATE())
@ 17, 11 SAY 'Garantia    : ' +  ;
  wk_ogar
@ 18, 11 SAY 'Estado      : ' +  ;
  wk_oest
@ 19, 11 SAY 'Tecnico     : ' +  ;
  STR(wk_otec, 9)
@ 20, 11 SAY 'Notas       : '
FOR i = 1 TO 6
     @ 20 + i, 25 SAY  ;
       wk_obsord(i)
ENDFOR
IF LASTKEY() <> 27
     DO esc_indica WITH 1, 'AYU',  ;
        'GRI', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     IF LASTKEY() = 27
          ACTIVATE WINDOW indicar
          RESTORE SCREEN FROM  ;
                  wk_pantax
          ACTIVATE WINDOW trabajo
          RETURN
     ENDIF
     wk_key = 0
     DO WHILE wk_key<>-6 .AND.  ;
        wk_key<>-9 .AND. wk_key<> ;
        27 .AND. wk_key<>-1
          wk_key = INKEY(0)
     ENDDO
     IF wk_key == -6 .OR. wk_key == - ;
        1
          DO mensa2 WITH  ;
             '*** Espere un momento, por favor ***',  ;
             'COLO'
          IF wk_oest = '010 '  ;
             .OR. wk_oest = '018'  ;
             .OR. wk_oest =  ;
             '021 ' .OR. wk_oest =  ;
             '026 ' .OR. wk_oest =  ;
             '027 '
               wk_oest = '025 '
          ENDIF
          DO usedbf WITH  ;
             'st_iorep',  ;
             'CODIGO'
          wk_numaux = STR(wk_numord,  ;
                      8)
          SEEK '&wk_numaux'
          IF FOUND()
               w_cosrep = cosrep
               DO rbloquea
               REPLACE auxest  ;
                       WITH  ;
                       wk_oest
               REPLACE fecest  ;
                       WITH  ;
                       DATE(),  ;
                       horest  ;
                       WITH  ;
                       TIME()
               REPLACE fecent  ;
                       WITH  ;
                       DATE(),  ;
                       horent  ;
                       WITH  ;
                       TIME()
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          USE
          wk_esorde = SPACE(40)
          wk_aux = 'ESOR' +  ;
                   wk_oest
          USE SHARED ge_tab0  ;
              ORDER codigo
          SEEK '&wk_aux'
          wk_esorde = ALLTRIM(tab_destab)
          USE
          USE st_mvord
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
          REPLACE user WITH users
          REPLACE date WITH  ;
                  DATE()
          REPLACE time WITH  ;
                  TIME()
          UNLOCK
          IF w_cosrep > 0
               DO usedbf WITH  ;
                  'GC_HVE00',  ;
                  'CODIGO'
               APPEND BLANK
               DO rbloquea
               REPLACE hve_tipdoc  ;
                       WITH  ;
                       'ORDE'
               REPLACE hve_nrodoc  ;
                       WITH  ;
                       f_ceros(STR(wk_numord,  ;
                       8),10,2)
               REPLACE hve_lispre  ;
                       WITH  ;
                       'PUBL'
               REPLACE hve_fecdoc  ;
                       WITH  ;
                       DATE()
               REPLACE hve_fecvct  ;
                       WITH  ;
                       DATE()
               REPLACE hve_fecgar  ;
                       WITH  ;
                       DATE()
               REPLACE hve_tipent  ;
                       WITH  ;
                       'C   '
               REPLACE hve_codent  ;
                       WITH  ;
                       STR(wk_codcli,  ;
                       11)
               REPLACE hve_tippag  ;
                       WITH  ;
                       '001 '
               REPLACE hve_estdoc  ;
                       WITH 'O'
               REPLACE hve_almdes  ;
                       WITH  ;
                       rge_codalm
               REPLACE hve_codmov  ;
                       WITH  ;
                       'EVTA'
               REPLACE hve_codmon  ;
                       WITH  ;
                       'SOL '
               REPLACE hve_codemi  ;
                       WITH  ;
                       wk_emisor
               REPLACE hve_fechtc  ;
                       WITH  ;
                       DATE()
               REPLACE hve_tidore  ;
                       WITH  ;
                       'ORDE'
               REPLACE hve_nrdore  ;
                       WITH  ;
                       f_ceros(STR(wk_numord,  ;
                       8),10,2)
               REPLACE hve_numore  ;
                       WITH  ;
                       STR(wk_numord,  ;
                       8)
               REPLACE hve_indori  ;
                       WITH  ;
                       wk_indori
               wk_solrep = (wk_cosrep *  ;
                           wk_tipcam)
               wk_solmob = (wk_cosmob *  ;
                           wk_tipcam)
               wk_solnet = (wk_solrep +  ;
                           wk_solmob) *  ;
                           (1 +  ;
                           wrk_igv)
               wk_totnet = (wk_cosmob +  ;
                           wk_cosrep) *  ;
                           (1 +  ;
                           wrk_igv)
               REPLACE hve_cosmob  ;
                       WITH  ;
                       wk_cosmob
               REPLACE hve_cosrep  ;
                       WITH  ;
                       wk_cosrep
               REPLACE hve_totnet  ;
                       WITH  ;
                       wk_totnet
               REPLACE hve_totoim  ;
                       WITH  ;
                       wk_totnet
               REPLACE hve_totgen  ;
                       WITH  ;
                       wk_totnet
               REPLACE hve_totvta  ;
                       WITH  ;
                       wk_totnet /  ;
                       (1 +  ;
                       wrk_igv)
               REPLACE hve_totigv  ;
                       WITH  ;
                       (wk_totnet -  ;
                       (wk_totnet /  ;
                       (1 +  ;
                       wrk_igv)))
               REPLACE hve_solrep  ;
                       WITH  ;
                       wk_solrep
               REPLACE hve_solmob  ;
                       WITH  ;
                       wk_solmob
               REPLACE hve_solnet  ;
                       WITH  ;
                       wk_solnet
               REPLACE hve_solgen  ;
                       WITH  ;
                       wk_solnet
               REPLACE hve_mtocan  ;
                       WITH  ;
                       wk_solnet
               REPLACE hve_solvta  ;
                       WITH  ;
                       wk_solnet /  ;
                       (1 +  ;
                       wrk_igv)
               REPLACE hve_soligv  ;
                       WITH  ;
                       (wk_solnet -  ;
                       (wk_solnet /  ;
                       (1 +  ;
                       wrk_igv)))
               REPLACE hve_tipcam  ;
                       WITH  ;
                       wk_tipcam
               REPLACE hve_usuari  ;
                       WITH  ;
                       users
               REPLACE hve_fecha  ;
                       WITH  ;
                       DATE()
               REPLACE hve_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
               USE
               SELECT 10
               USE ST_IPREP ORDER  ;
                   REP_NUMORD
               SELECT 11
               USE ST_IDPED ORDER  ;
                   CODIGO
               SELECT 12
               USE GC_DVE00
               SELECT 13
               USE GC_PRO00 ORDER  ;
                   CODIGO
               SELECT st_iprep
               SEEK STR(wk_numord,  ;
                    8)
               SCAN WHILE numord =  ;
                    STR(wk_numord,  ;
                    8) .AND.   ;
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
                         REPLACE time  ;
                                 WITH  ;
                                 TIME()
                         REPLACE date  ;
                                 WITH  ;
                                 DATE()
                         UNLOCK
                         wk_nrodoc =  ;
                          numdoc
                         SELECT st_idped
                         SET NEAR ON
                         SEEK wk_nrodoc
                         SET NEAR OFF
                         SCAN WHILE  ;
                              numdoc =  ;
                              wk_nrodoc  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              IF canpro >  ;
                                 0
                                   wk_cod = codpro
                                   wk_can = canpro
                                   wk_val = valpro
                                   wk_totite = totite
                                   SELECT gc_pro00
                                   SEEK wk_cod
                                   wk_coduni = pro_unimed
                                   SELECT gc_dve00
                                   APPEND BLANK
                                   DO rbloquea
                                   REPLACE dve_tipdoc WITH 'ORDE'
                                   REPLACE dve_nrodoc WITH f_ceros(ALLTRIM(STR(wk_numord, 8)),10,2)
                                   REPLACE dve_propar WITH wk_cod
                                   REPLACE dve_unimed WITH 'UNID'
                                   REPLACE dve_cantid WITH wk_can
                                   REPLACE dve_unimed WITH wk_coduni
                                   REPLACE dve_import WITH wk_val
                                   REPLACE dve_total WITH wk_totite
                                   REPLACE dve_coprmo WITH oocospro(wk_cod)
                                   REPLACE dve_coprmb WITH oocosprb(wk_cod)
                                   REPLACE dve_impors WITH (wk_val * wk_tipcam)
                                   REPLACE dve_totals WITH (wk_totite * wk_tipcam)
                                   REPLACE dve_usuari WITH users
                                   REPLACE dve_hora WITH TIME()
                                   REPLACE dve_fecha WITH DATE()
                                   UNLOCK
                              ENDIF
                              SELECT  ;
                               st_idped
                         ENDSCAN
                         SELECT st_iprep
                    ENDIF
               ENDSCAN
               SELECT st_idped
               USE
               SELECT st_iprep
               USE
               SELECT gc_dve00
               USE
          ENDIF
          IF wk_key == -6
               DO imp_ord
          ENDIF
          USE
          DO mensa2 WITH  ;
             '*** Espere un momento, por favor ***',  ;
             'SACA'
     ENDIF
ENDIF
IF LASTKEY() == 27
     IF efecin == 1
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
          campoa = 'numdoc+" "+dtoc(fecemi)+" "+SUBSTR(NUMSER,1,12)+"    "+codent+"  "+SUBSTR(codmod,1,10)+"  "+subst(indest,1,2)+"    "+alltrim(indori)'
     ELSE
          wrk_origen = 'PP'
          USE SHARED st_ispre  ;
              ORDER CODIGO
          SET FILTER TO indest <> 'P'
          campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)+" "+alltrim(indori)'
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
IF VARREAD() == 'WK_NUMORD'
     DO usedbf WITH 'st_iorep',  ;
        'CODIGO'
     wrk_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)+" "+alltrim(indori)'
     DO ayuda4 WITH campoa,  ;
        wrk_origen
     USE
ENDIF
ON KEY LABEL F6 do ayuda12
RETURN
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
DO usedbf WITH 'st_iorep',  ;
   'CODIGO'
SEEK '&wk_clave'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** N§ Orden Reparacion NO EXISTE. **'
     RETURN .F.
ENDIF
IF auxest = '020 ' .OR. auxest =  ;
   '022 ' .OR. auxest = '023 '  ;
   .OR. auxest = '024 ' .OR.  ;
   auxest = '025 '
     DO error WITH  ;
        '*** N§ Orden Reparacion Sali¢ el '+ ;
        DTOC(fecest)+' ***'
     USE
     RETURN .F.
ENDIF
IF indest = 'N   '
     USE
     DO error WITH  ;
        '** N§ Orden Reparacion esta ANULADA. **'
     RETURN .F.
ENDIF
IF indest = 'F   ' .AND. auxest =  ;
   '100 '
     USE
     DO error WITH  ;
        '** N§ Orden Reparacion FACTURADA. **'
     RETURN .F.
ENDIF
IF indest = 'B   ' .AND. auxest =  ;
   '100 '
     USE
     DO error WITH  ;
        '** N§ Orden Reparacion BOLETEADA. **'
     RETURN .F.
ENDIF
IF indest = 'V' .OR. indest = 'P'
     USE
     DO error WITH  ;
        '*** O/R No se Encuentra Cerrada ***'
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
             '** Error Solicitud en Proceso. **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          RETURN .F.
     ENDIF
     IF coddes == 'P'
          USE
          DO error WITH  ;
             '** Error Solicitud para Presupuesto. **'
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
PROCEDURE buscaor
SELECT 13
DO usedbf WITH 'st_iorep',  ;
   'CODIGO'
GOTO BOTTOM
wk_numaux = VAL(numdoc)
wk_numord = wk_numaux + 1
SELECT 13
USE
RETURN
*
FUNCTION empleado
DEFINE WINDOW empleado FROM 30,  ;
       16 TO 34, 56
ACTIVATE WINDOW empleado
ON KEY LABEL F8
STORE SPACE(05) TO wrk_codemp
STORE SPACE(15) TO wrk_codbla
DO WHILE .T.
     SET CURSOR ON
     @ 01, 02 SAY 'Usuario :' GET  ;
       wrk_codemp PICTURE '@!'
     @ 02, 12 GET wrk_codbla  ;
       PICTURE '@!'
     READ
     IF LASTKEY() = 27
          USE
          RETURN .F.
     ENDIF
     USE gc_vnd00 ORDER CODIGO
     SEEK 'A' + wrk_codemp
     IF  .NOT. FOUND()
          DO error2 WITH  ;
             '*** Codigo de Empleado NO EXISTE ***'
          LOOP
     ENDIF
     wrk_nomemp = vnd_nombre
     @ 01, 17 SAY  ;
       SUBSTR(wrk_nomemp, 1, 20)
     USE
     DEACTIVATE WINDOW empleado
     RETURN .T.
ENDDO
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
USE
wk_numaux = STR(wk_numord, 8)
USE SHARED st_itecn ORDER CODIGO
SEEK STR(wk_otec, 9)
wrk_destec = noment
DO usedbf WITH 'st_iorep',  ;
   'codigo'
SEEK '&wk_numaux'
wk_numsol = numsol
wk_fecha = DTOC(fecemi)
USE
wrk_dogtia = ooseri2(wk_codmar, ;
             wk_codmod, ;
             wk_numser)
tit_client = wk_noment
tit_codigo = STR(wk_codcli, 9)
tit_direcc = wk_nomcal
tit_feccom = wk_feccom
tit_tipo = wk_destia
tit_fechho = SUBSTR(wk_fecha, 1,  ;
             2) + SPACE(3) +  ;
             SUBSTR(wk_fecha, 4,  ;
             2) + SPACE(3) +  ;
             SUBSTR(wk_fecha, 7,  ;
             2)
tit_tit2 = 'SINTOMAS :'
tit_tit3 = 'ACCESORIOS :'
tit_tit4 = 'OBSERVACIONES :'
tit_tit5 = 'SALIDA EN CORTESIA'
tit_subray = 'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
tit_tit6 = 'INF.TEC. / NOTAS :'
tit_fecha = DTOC(DATE()) + ' ' +  ;
            TIME()
tit_eminro = 'O/R  N§: ' +  ;
             wk_numaux
tit_nrosol = 'S/S  N§: ' +  ;
             wk_numsol
tit_codmod = wk_codmod
tit_desmod = wk_desmod
DIMENSION wk_obs( 3)
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
@ 03, 70 - (LEN(tit_subray) / 2)  ;
  SAY tit_subray
@ 03, 115 SAY tit_nrosol
@ 04, 115 SAY tit_fecha
@ 05, 015 SAY tit_client
@ 05, 075 SAY tit_codigo
@ 06, 015 SAY tit_direcc
@ 06, 115 SAY tit_feccom
@ 07, 115 SAY wk_numte1
@ 09, 105 SAY ALLTRIM(tit_tipo)
@ 10, 000 SAY tit_desmod
@ 10, 030 SAY wk_codmod
DO usedbf WITH 'GE_TAB0',  ;
   'CODIGO'
SEEK 'MARC' + wk_codmar
wrk_desmar = tab_destab
USE
@ 10, 050 SAY wrk_desmar
@ 10, 075 SAY wk_numser
IF wk_ogar = 'GARA'
     @ 10, 113 SAY wrk_dogtia
ENDIF
@ 11, 01 SAY tit_tit2
@ 11, 70 SAY tit_tit3
FOR lin = 1 TO 9
     IF wk_codsin(lin) <>  ;
        SPACE(35)
          @ 11 + lin, 01 SAY  ;
            wk_codsin(lin)
     ENDIF
     @ 11 + lin, 070 SAY  ;
       wk_acceso(lin)
ENDFOR
@ 21, 01 SAY tit_tit6
@ 21, 080 SAY 'T‚cnico :'
@ 21, 090 SAY STR(wk_otec, 9)
@ 21, 103 SAY SUBSTR(wrk_destec,  ;
  1, 30)
nota1 = wk_obsord(1) +  ;
        wk_obsord(2)
nota2 = wk_obsord(3) +  ;
        wk_obsord(4)
nota3 = wk_obsord(5) +  ;
        wk_obsord(6)
@ 22, 01 SAY nota1
@ 23, 01 SAY nota2
@ 24, 01 SAY nota3
@ 25, 01 SAY tit_tit4 + SPACE(10) +  ;
  'Recep.: ' + wrk_nomemp
@ 26, 01 SAY SUBSTR(wk_obs(1), 1,  ;
  76)
@ 27, 01 SAY SUBSTR(wk_obs(1), 77,  ;
  14) + SUBSTR(wk_obs(2), 1, 62)
@ 28, 01 SAY SUBSTR(wk_obs(2), 63,  ;
  28) + SUBSTR(wk_obs(3), 1, 48)
@ 28, 97 SAY tit_fechho
?? CHR(15)
EJECT
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
RETURN
*
PROCEDURE buscaor
SELECT 13
DO usedbf WITH 'st_iorep',  ;
   'CODIGO'
GOTO BOTTOM
wk_numaux = VAL(numdoc)
wk_numord = wk_numaux + 1
SELECT 13
USE
RETURN
*
PROCEDURE sintom1
xxsele = SELECT()
PUBLIC muestraw
DO usedbf WITH 'st_sint',  ;
   'codigo'
DO usedbf WITH 'st_sicli',  ;
   'codigo'
SET RELATION TO SUBSTR(codsin, 2, 3) INTO;
st_sint
GOTO TOP
mj = 1
SCAN FOR ALLTRIM(numdoc) =  ;
     ALLTRIM(STR(wk_numero))
     PUBLIC dime, muestrx( mj)
     muestrx( mj) =  ;
            st_sint.dessin
     mj = mj + 1
ENDSCAN
mostraw = ' '
DEACTIVATE POPUP sintox
DEFINE POPUP sintox FROM 24, 01  ;
       TO 38, 35
FOR xl = 1 TO mj - 1
     DEFINE BAR xl OF sintox  ;
            PROMPT muestrx(xl)
ENDFOR
SHOW POPUP sintox
SELECT (xxsele)
CLOSE DATABASES
RETURN
*
FUNCTION ooseri2
PARAMETER wrk_marca, wrk_modelo,  ;
          wrk_numser
narea = SELECT()
= ooopen('ST_ISERI',2)
= ooareat('ST_ISERI', ;
  'SER_CODMAR')
SEEK wrk_marca + wrk_modelo +  ;
     wrk_numser
IF FOUND()
     wrk_desser = DTOC(st_iseri.fecvta) +  ;
                  ' ' +  ;
                  ALLTRIM(docgar)
ELSE
     wrk_desser = ''
ENDIF
= ooclose('ST_ISERI')
SELECT (narea)
RETURN wrk_desser
*
*** 
*** ReFox - retrace your steps ... 
***

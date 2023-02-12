PARAMETER config_prg
IF config_prg == 2
     titu1 = 'CONSULTA'
     titu2 = ' ORDENES DE REPARACION '
ENDIF
wrk_progra = PROGRAM()
DO crea_win
SET SYSMENU ON
CLEAR TYPEAHEAD
ON KEY LABEL F6 do ayuda12
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, titu1
DO saycenter WITH 2, titu2
DO esc_modo WITH 'I'
DO esc_indica WITH 1, 'AYU', 'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'BBB', 'BBB', 'IGN', 'ESC'
PUBLIC ubicacion
ubicacion = '     '
@ 6, 10 CLEAR TO 10, 65
@ 6, 10 TO 10, 65
SAVE SCREEN TO wk_panta
CLOSE DATABASES
PUBLIC k_numero, wk_oest
STORE 0 TO k_numero
wk_numord = 0
sw_cuenta = 0
DIMENSION solic( 48)
DIMENSION nota( 6)
DIMENSION notao( 6)
USE IN 17 SHARED gc_cmv00 AGAIN ORDER cmv_feinmo
w_tipcam = ootc2(DATE(),'SOL ','DOL ','2')
DO usedbf WITH 'ge_tab0', 'codigo'
wrk_varbus = '"IGV " + "IGV "'
SEEK &wrk_varbus
IF FOUND()
     wk_igv = tab_factor
     wrk_facigv = tab_factor / 100
ELSE
     DO error WITH '**No Definido el IGV**'
     ON KEY
     CLOSE DATABASES
     DO sacawin
     RETURN
ENDIF
USE
ppal = .T.
STORE SPACE(30) TO wk_destia
DO WHILE ppal
     RESTORE SCREEN FROM wk_panta
     DO esc_indica WITH 1, 'AYU', 'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB', 'BBB', 'IGN', 'ESC'
     efecin = 1
     STORE 0 TO veces, wk_otec, wk_numord, w_pagsol, w_pagdol, wk_flete, sol_cosfle, wk_repues
     STORE ' ' TO wk_ogar, wk_oest, wrk_numfac, nota1, nota2, nota3, nota4, nota5, nota6, wk_esor
     STORE 0 TO wk_totrep, s_totrep, wk_totdes, s_totdes, wk_totnet, s_totnet, wk_totafe, wk_mano
     STORE 0 TO wk_totman, s_totman, wk_totgrl, s_totgrl, wk_totigv, s_totigv, s_totafe, wk_numsol
     STORE SPACE(30) TO wk_nom1, wk_nom2
     STORE SPACE(4) TO w_linea, wk_indest, wk_indori
     STORE SPACE(10) TO wrk_numfac
     IF config_prg == 2
          ppal = .T.
          DO WHILE ppal
               efecin = 1
               @ 8, 20 SAY ' N§ Orden Reparaci¢n ' GET wk_numord PICTURE '99999999' VALID numord(wk_numord) WHEN colocaf6()
               SET CURSOR ON
               READ
               SET CURSOR OFF
               IF LASTKEY() == 27 .AND. efecin == 2
                    LOOP
               ELSE
                    ppal = .F.
               ENDIF
          ENDDO
          IF wk_numord == 0 .OR. LASTKEY() == 27
               ppal = .F.
          ELSE
               ppal = .T.
          ENDIF
     ENDIF
     DO CASE
          CASE LASTKEY() == 27 .AND. efecin == 1
               ppal = .F.
               LOOP
          CASE LASTKEY() == 27 .AND. efecin == 2
               LOOP
     ENDCASE
     IF config_prg == 2
          USE SHARED st_iorep ORDER CODIGO
          wk_numaux = STR(wk_numord, 8)
          wk_numero2 = wk_numaux
          wk_auxi = codtec
          SEEK '&wk_numaux'
          FOR i = 1 TO 6
               nota( i) = SUBSTR(observ, 1 + ((i - 1) * 45), 45)
          ENDFOR
          nota1 = nota(1)
          nota2 = nota(2)
          nota3 = nota(3)
          nota4 = nota(4)
          nota5 = nota(5)
          nota6 = nota(6)
          wk_esor = auxest
          wk_codtec = codtec
          IF numpre <> SPACE(8)
               wk_auxy = numpre
               wk_numero = VAL(numsol)
               k_numero = wk_numero
               wk_numso = VAL(numsol)
               wk_usar = VAL(numpre)
               wk_var = 2
               wk_cod001 = 'P'
          ELSE
               wk_auxy = numsol
               wk_numero = VAL(numsol)
               k_numero = wk_numero
               wk_numso = VAL(numsol)
               wk_usar = VAL(numsol)
               wk_var = 1
               wk_cod001 = 'R'
          ENDIF
          USE
     ENDIF
     lin = 40
     anc = 75
     des2 = 1
     com = 1
     DIMENSION solic2( 52)
     STORE FOPEN('orden.txt') TO file_handl
     FOR i = 1 TO 51
          solic2( i) = FREAD(file_handl, 77)
     ENDFOR
     = FCLOSE(file_handl)
     DIMENSION wk_codsin( 15)
     DIMENSION wk_acceso( 15)
     DIMENSION wk_observ( 06)
     wk_numaux = STR(wk_numero, 8)
     USE SHARED st_isrep ORDER CODIGO
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
     wk_codstk = codstk
     wk_numstk = VAL(numstk)
     DIMENSION wk_acceso( 15), wk_observ( 6)
     IF wk_var = 2
          USE SHARED st_isrep ORDER CODIGO
          seek '&wk_numaux'
          FOR i = 1 TO 15
               wk_codsin( i) = SPACE(35)
               wk_acceso( i) = SUBSTR(desace, 1 + ((i - 1) * 35), 35)
               wk_acceso( i) = wk_acceso(i) + SPACE(35 - LEN(wk_acceso(i)))
               IF i <= 6
                    wk_observ( i) = SUBSTR(observ, 1 + ((i - 1) * 45), 45)
                    wk_observ( i) = wk_observ(i) + SPACE(45 - LEN(wk_observ(i)))
               ENDIF
          ENDFOR
     ELSE
          USE SHARED st_isrep ORDER CODIGO
          seek '&wk_numaux'
          FOR i = 1 TO 15
               wk_codsin( i) = SPACE(35)
               wk_acceso( i) = SUBSTR(desace, 1 + ((i - 1) * 35), 35)
               wk_acceso( i) = wk_acceso(i) + SPACE(35 - LEN(wk_acceso(i)))
               IF i <= 6
                    wk_observ( i) = SUBSTR(observ, 1 + ((i - 1) * 45), 45)
                    wk_observ( i) = wk_observ(i) + SPACE(45 - LEN(wk_observ(i)))
               ENDIF
          ENDFOR
     ENDIF
     wk_cliaux = 'C' + STR(wk_codcli, 11)
     IF  .NOT. USED('st_iclpr')
          USE SHARED st_iclpr ORDER CODIGO
     ELSE
          SELECT st_iclpr
          SET ORDER TO codigo
     ENDIF
     SEEK '&wk_cliaux'
     wk_noment = noment
     wk_numte1 = numte1
     wk_numte2 = numte2
     wk_aux = wk_codmar + wk_codmod
     USE SHARED st_imode ORDER CODIGO
     SEEK '&wk_aux'
     wk_aux = codcla
     w_linea = linea
     SELECT 2
     USE SHARED st_sint ORDER sin_lincod
     SELECT 1
     USE SHARED st_sicli ORDER CODIGO
     SEEK '&wk_numaux'
     i = 1
     IF FOUND()
          DO WHILE  .NOT. EOF() .AND. numdoc==wk_numaux
               wk_aux2 = SUBSTR(codsin, 2, 3)
               SELECT 2
               SEEK w_linea + wk_aux2
               wk_codsin( i) = SUBSTR(dessin, 1, 35)
               i = i + 1
               SELECT 1
               SKIP
          ENDDO
     ENDIF
     CLOSE DATABASES
     ACTIVATE SCREEN
     ACTIVATE WINDOW trabajo
     SET DISPLAY TO VGA50
     ZOOM WINDOW trabajo NORM FROM 1, 0 TO 42, 76
     ZOOM WINDOW indicar NORM FROM 45, 0 TO 48, 76
     ACTIVATE SCREEN
     ACTIVATE WINDOW trabajo
     DO col_or1b
     DO col_or2b
     DO col_or3b
     FOR i = des2 TO (lin + des2 - 1)
          @ i - des2, 0 SAY SUBSTR(solic2(i), com, anc)
     ENDFOR
     DO esc_indica WITH 1, 'AYU', 'OOE', 'INF', 'IGN'
     DO esc_indica WITH 2, 'OOR', 'OOA', 'OTR', 'ESC'
     ppal2 = .T.
     DO WHILE ppal2
          wk_inkey = 0
          IF config_prg = 2
               DO WHILE  .NOT. (STR(wk_inkey, 2)$' 18, 3, 1, 6,27,-9-3-4-6-8-7')
                    wk_inkey = INKEY(0)
               ENDDO
          ELSE
               DO WHILE  .NOT. (STR(wk_inkey, 2)$' 18, 3, 1, 6,27,-9-7')
                    wk_inkey = INKEY(0)
               ENDDO
          ENDIF
          DO CASE
               CASE wk_inkey == 18
                    DO mueve3a WITH CHR(wk_inkey)
               CASE wk_inkey == 3
                    DO mueve3a WITH CHR(wk_inkey)
               CASE wk_inkey == -3
                    DO veorden
                    ON KEY LABEL f3
               CASE wk_inkey == -6
                    DO ver
                    DO esc_indica WITH 1, 'AYU', 'OOA', 'OTR', 'BBB'
                    DO esc_indica WITH 2, 'OOR', 'OOE', 'IGN', 'ESC'
               CASE wk_inkey == -4

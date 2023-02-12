*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = PROGRAM()
titu1 = 'SALIDA DE RECLAMOS'
titu2 = 'ORDEN  DE  REPARACION'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 do ayuda12
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, titu1
DO saycenter WITH 2, titu2
PUBLIC wrk_codemp
STORE SPACE(05) TO wrk_codemp
DO esc_modo WITH 'I'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
@ 6, 10 CLEAR TO 10, 65
@ 6, 10 TO 10, 65
SAVE SCREEN TO wk_panta
CLOSE DATABASES
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
     wk_numord = 0
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
     USE SHARED st_iorep ORDER  ;
         CODIGO
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
     ACTIVATE SCREEN
     @ 24, 69 SAY SPACE(11)
     ACTIVATE WINDOW trabajo
     SET DISPLAY TO VGA50
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO 42, 76
     ZOOM WINDOW indicar NORM  ;
          FROM 45, 0 TO 48, 76
     ACTIVATE SCREEN
     @ 49, 69 SAY ind_prg
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
          USE
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
          wk_totafe = wk_totnet +  ;
                      wk_totman
          wk_totigv = totigv
          wk_totgrl = wk_totafe +  ;
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
          USE
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
DO saca_win
RELEASE tecnico
@ 24, 69 SAY SPACE(15)
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
  ' Salida de Ordenes en Reclamos '  ;
  COLOR SCHEME 8
wk_oaux = wk_indori
USE SHARED st_iorep ORDER CODIGO
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
        'BBB', 'GRI', 'BBB'
     DO esc_indica WITH 2, 'GRA',  ;
        'BBB', 'IGN', 'ESC'
     DO empleado
     wk_key = 0
     DO WHILE wk_key<>-1 .AND.  ;
        wk_key<>-6 .AND. wk_key<>- ;
        9 .AND. wk_key<>27
          wk_key = INKEY(0)
     ENDDO
     IF wk_key == -1 .OR. wk_key == - ;
        6
          wk_oest = '022 '
          USE SHARED ge_tab0  ;
              ORDER codigo
          SEEK 'ESOR' + wk_oest
          wk_esorde = tab_destab
          USE
          USE SHARED st_iorep  ;
              ORDER CODIGO
          wk_numaux = STR(wk_numord,  ;
                      8)
          SEEK '&wk_numaux'
          IF FOUND()
               DO rbloquea
               REPLACE indori  ;
                       WITH  ;
                       wk_ogar
               REPLACE auxest  ;
                       WITH  ;
                       wk_oest
               REPLACE codtec  ;
                       WITH  ;
                       STR(wk_otec,  ;
                       9)
               REPLACE fecest  ;
                       WITH  ;
                       DATE()
               REPLACE indest  ;
                       WITH 'C'
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
          wk_esorde = tab_destab
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
          IF wk_key == -6
               DO imp_ord
               CLEAR GETS
          ENDIF
          USE
     ENDIF
ENDIF
IF LASTKEY() == 27
     IF efecin == 1
          CLEAR GETS
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
          campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)'
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
     USE SHARED st_iorep ORDER  ;
         CODIGO
     wrk_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)+" "+alltrim(indori)'
     DO ayuda4 WITH campoa,  ;
        wrk_origen
     USE
ENDIF
ON KEY LABEL F6 do ayuda12
RETURN
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
IF auxest = '022 ' .OR. auxest =  ;
   '100 '
     DO error WITH  ;
        '*** N§ Orden Reparaci¢n Sali¢ el '+ ;
        DTOC(fecest)+' ***'
     USE
     RETURN .F.
ENDIF
IF indori = 'GARA' .OR. indori =  ;
   'FGAR'
     DO error WITH  ;
        '*** N§ Orden Reparaci¢n es '+ ;
        indori+' ***'
     USE
     RETURN .F.
ENDIF
IF indest = 'N   '
     USE
     DO error WITH  ;
        '** N§ Orden Reparaci¢n esta ANULADA. **'
     RETURN .F.
ENDIF
IF indest = 'F   ' .AND. auxest =  ;
   '100 '
     USE
     DO error WITH  ;
        '** N§ Orden Reparaci¢n FACTURADA. **'
     RETURN .F.
ENDIF
IF indest = 'B   ' .AND. auxest =  ;
   '100 '
     USE
     DO error WITH  ;
        '** N§ Orden Reparaci¢n BOLETEADA. **'
     RETURN .F.
ENDIF
IF auxest < '010 '
     USE
     DO error WITH  ;
        '*** O/R No se Encuentra Cerrada ***'
     RETURN .F.
ENDIF
USE
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
USE
wk_numaux = STR(wk_numord, 8)
USE SHARED st_itecn ORDER CODIGO
SEEK STR(wk_otec, 9)
wrk_destec = noment
USE SHARED st_iorep ORDER CODIGO
SEEK '&wk_numaux'
wk_numsol = numsol
tit_client = wk_noment
tit_codigo = STR(wk_codcli, 9)
tit_direcc = wk_nomcal
tit_feccom = wk_feccom
tit_tipo = wk_destia
wk_fecha = DTOC(fecemi)
tit_fechho = SUBSTR(wk_fecha, 1,  ;
             2) + SPACE(3) +  ;
             SUBSTR(wk_fecha, 4,  ;
             2) + SPACE(3) +  ;
             SUBSTR(wk_fecha, 7,  ;
             2)
tit_tit2 = 'SINTOMAS :'
tit_tit3 = 'ACCESORIOS :'
tit_tit4 = 'OBSERVACIONES :'
tit_tit5 = 'SALIDA DE RECLAMOS'
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
@ 09, 113 SAY tit_tipo
@ 10, 00 SAY tit_desmod
@ 10, 030 SAY wk_codmod
selex = SELECT()
DO usedbf WITH 'GE_TAB0',  ;
   'CODIGO'
SEEK 'MARC' + wk_codmar
wrk_desmar = tab_destab
@ 10, 050 SAY wrk_desmar
@ 10, 075 SAY wk_numser
@ 11, 002 SAY tit_tit2
@ 11, 002 SAY tit_tit2
@ 11, 070 SAY tit_tit3
@ 11, 070 SAY tit_tit3
FOR lin = 1 TO 9
     IF wk_codsin <> SPACE(35)
          @ 11 + lin, 002 SAY  ;
            wk_codsin(lin)
     ENDIF
     @ 11 + lin, 070 SAY  ;
       wk_acceso(lin)
ENDFOR
@ 21, 002 SAY tit_tit6
@ 21, 080 SAY 'T‚cnico :'
@ 21, 090 SAY STR(wk_otec, 9)
@ 21, 103 SAY SUBSTR(wrk_destec,  ;
  1, 30)
nota1 = wk_obsord(1) +  ;
        wk_obsord(2)
nota2 = wk_obsord(3) +  ;
        wk_obsord(4)
nota3 = wk_obsord(4) +  ;
        wk_obsord(6)
@ 22, 02 SAY nota1
@ 23, 02 SAY nota2
@ 24, 02 SAY nota3
@ 25, 02 SAY tit_tit4
@ 26, 02 SAY SUBSTR(wk_obs(1), 1,  ;
  76)
@ 27, 02 SAY SUBSTR(wk_obs(1), 77,  ;
  14) + SUBSTR(wk_obs(2), 1, 62)
@ 28, 02 SAY SUBSTR(wk_obs(2), 63,  ;
  28) + SUBSTR(wk_obs(3), 1, 48)
@ 28, 97 SAY tit_fechho
@ 29, 002 SAY SUBSTR(wk_obs(3),  ;
  49, 42)
?? CHR(15)
EJECT
USE
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
RETURN
*
PROCEDURE buscaor
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
       PICTURE '@!' COLOR W+/N,W+/ ;
       N 
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
*** 
*** ReFox - retrace your steps ... 
***

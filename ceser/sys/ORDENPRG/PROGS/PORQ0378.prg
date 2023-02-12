*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
IF config_prg == 2
     ind_prg = '<PORQ0378>'
     titu1 = 'CONSULTA'
     titu2 = 'CONSULTA DE ORDENES A TRAVES DE SOLICITUD'
ENDIF
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
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
wk_numord = 0
sw_cuenta = 0
DIMENSION solic( 48)
ppal = .T.
DO WHILE ppal
     RESTORE SCREEN FROM wk_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     efecin = 1
     STORE 0 TO veces, wk_otec,  ;
           wk_numord
     STORE ' ' TO wk_ogar,  ;
           wk_oest
     STORE SPACE(30) TO wk_nom1,  ;
           wk_nom2, nota1, nota2,  ;
           nota3, nota4, nota5,  ;
           nota6
     IF config_prg == 2
          wk_soli = 0
          vuelta = 0
          DO WHILE ppal
               wk_vesoli = 0
               efecin = 1
               @ 8, 20 SAY  ;
                 ' N§ Solicitud '  ;
                 GET wk_soli  ;
                 PICTURE  ;
                 '99999999' VALID  ;
                 sol3(wk_soli)  ;
                 WHEN colocaf6()
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
               USE SHARED  ;
                   st_iredo ORDER  ;
                   CODIGO
               busca = "'SSE '+str(wk_soli,8)"
               seek &busca    ;
                 
               IF FOUND()
                    wk_numord = VAL(numddo)
                    USE
               ELSE
                    USE
                    wk_numero = wk_soli
                    DO ve_soli
                    CLEAR TYPEAHEAD
                    ppal = .T.
               ENDIF
          ENDDO
          IF wk_numord == 0 .OR.  ;
             LASTKEY() == 27
               ppal = .F.
          ELSE
               ppal = .T.
          ENDIF
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
          wk_numero2 = wk_numaux
          wk_auxi = codtec
          SEEK '&wk_numaux'
          nota1 = SUBSTR(observ,  ;
                  1, 38)
          nota2 = SUBSTR(observ,  ;
                  39, 38)
          nota3 = SUBSTR(observ,  ;
                  77, 38)
          nota4 = SUBSTR(observ,  ;
                  115, 38)
          nota5 = SUBSTR(observ,  ;
                  153, 38)
          nota6 = SUBSTR(observ,  ;
                  191, 38)
          wk_esor = auxest
          wk_codtec = codtec
          IF numpre <> SPACE(8)
               wk_auxy = numpre
               wk_numero = VAL(numsol)
               wk_usar = VAL(numpre)
               wk_var = 2
               wk_cod001 = 'P'
          ELSE
               wk_auxy = numsol
               wk_numero = VAL(numsol)
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
     STORE FOPEN('orden.txt') TO  ;
           file_handl
     FOR i = 1 TO 51
          solic2( i) =  ;
                FREAD(file_handl,  ;
                77)
     ENDFOR
     = FCLOSE(file_handl)
     DIMENSION wk_codsin( 15)
     DIMENSION wk_acceso( 15)
     DIMENSION wk_observ( 06)
     wk_numaux = STR(wk_numero,  ;
                 8)
     USE SHARED st_isrep ORDER  ;
         CODIGO
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
     DIMENSION wk_codsin( 15),  ;
               wk_acceso( 15),  ;
               wk_observ( 6)
     IF wk_var = 2
          USE SHARED st_isrep  ;
              ORDER CODIGO
          SEEK '&wk_auxy'
          FOR i = 1 TO 15
               wk_codsin( i) =  ;
                        SPACE(35)
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
     ELSE
          USE SHARED st_isrep  ;
              ORDER CODIGO
          SEEK '&wk_auxy'
          FOR i = 1 TO 15
               wk_codsin( i) =  ;
                        SPACE(35)
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
     ENDIF
     wk_cliaux = 'C' +  ;
                 STR(wk_codcli,  ;
                 9)
     USE SHARED st_iclpr ORDER  ;
         CODIGO
     SEEK '&wk_cliaux'
     wk_noment = noment
     wk_aux = wk_codmar +  ;
              wk_codmod
     USE SHARED st_imode ORDER  ;
         CODIGO
     SEEK '&wk_aux'
     wk_aux = codcla
     SELECT 2
     USE SHARED st_isint ORDER  ;
         CODIGO
     SELECT 1
     USE SHARED st_issre ORDER  ;
         CODIGO
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
     CLOSE DATABASES
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
     DO col_or1b
     DO col_or2b
     DO col_or3b
     FOR i = des2 TO (lin + des2 -  ;
         1)
          @ i - des2, 0 SAY  ;
            SUBSTR(solic2(i), com,  ;
            anc)
     ENDFOR
     DO esc_indica WITH 1, 'AYU',  ;
        'OOA', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'OOR',  ;
        'OOE', 'IGN', 'ESC'
     ppal2 = .T.
     DO WHILE ppal2
          wk_inkey = 0
          IF config_prg = 2
               DO WHILE  .NOT.  ;
                  (STR(wk_inkey,  ;
                  2)$ ;
                  ' 5,24,18, 3, 1, 6,27,-9-3-4-6' ;
                  )
                    wk_inkey = INKEY(0)
               ENDDO
          ELSE
               DO WHILE  .NOT.  ;
                  (STR(wk_inkey,  ;
                  2)$ ;
                  ' 5,24,18, 3, 1, 6,27,-9' ;
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
                    6
                    DO ver
                    DO esc_indica  ;
                       WITH 1,  ;
                       'AYU',  ;
                       'OOA',  ;
                       'BBB',  ;
                       'BBB'
                    DO esc_indica  ;
                       WITH 2,  ;
                       'OOR',  ;
                       'OOE',  ;
                       'IGN',  ;
                       'ESC'
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
                    sw_cuenta = 0
                    ppal2 = .F.
                    LOOP
               OTHERWISE
                    DO mueve3a  ;
                       WITH  ;
                       CHR(wk_inkey)
          ENDCASE
     ENDDO
     SET DISPLAY TO VGA25
     ACTIVATE SCREEN
     @ 24, 69 SAY ind_prg
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO 17, 76
     ZOOM WINDOW indicar NORM  ;
          FROM 20, 0 TO 23, 76
     ACTIVATE WINDOW trabajo
ENDDO
CLOSE DATABASES
ON KEY LABEL F6
ON KEY LABEL F8
ON KEY LABEL F10
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DO saca_win
@ 24, 69 SAY SPACE(15)
RELEASE wk_pantax, solic
RETURN
*
PROCEDURE ayuda12
ON KEY LABEL F6
ON KEY LABEL F8
IF VARREAD() == 'WK_NUMERO'
     IF wk_var == 1
          USE SHARED st_isrep  ;
              ORDER CODIGO
          wrk_origen = 'SS'
     ELSE
          USE SHARED st_ispre  ;
              ORDER CODIGO
          wrk_origen = 'SS'
     ENDIF
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+SUBSTR(NUMSER,1,12)+" "+codent+" "+codmod+" "+subst(indest,1,2)'
     DO ayuda4 WITH campoa,  ;
        wrk_origen
     USE
ENDIF
IF VARREAD() == 'WK_NUMORD' .AND.  ;
   config_prg > 1
     USE SHARED st_iorep ORDER  ;
         CODIGO
     wrk_origen = 'SS'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)'
     DO ayuda4 WITH campoa,  ;
        wrk_origen
     USE
ENDIF
IF VARREAD() == 'WK_SOLI'
     USE SHARED st_isrep ORDER  ;
         CODIGO
     wrk_origen = 'SS'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+SUBSTR(NUMSER,1,12)+" "+codent+" "+codmod+" "+subst(indest,1,2)'
     DO ayuda4 WITH campoa,  ;
        wrk_origen
     USE
ENDIF
ON KEY LABEL F6 do ayuda12
ON KEY LABEL F8 do solicitud
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
wk_clave = STR(num, 8)
USE SHARED st_iorep ORDER CODIGO
SEEK '&wk_clave'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** N§ Orden Reparacion NO EXISTE. **'
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
wk_oaux = IIF(wk_indori == 'GARA'  ;
          .OR. wk_indori == 'S',  ;
          'S', 'N')
veces = veces + 1
IF veces = 1
     USE SHARED st_iorep ORDER  ;
         CODIGO
     wk_numaux = STR(wk_numord,  ;
                 8)
     SEEK '&wk_numaux'
     wk_ogar = IIF(indori ==  ;
               'GARA', 'S', 'N')
     wk_oest = auxest
     wk_otec = VAL(codtec)
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
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SEEK '&wk_obux'
     wk_nom1 = SUBSTR(tab_destab,  ;
               1, 28)
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
@ 17, 35 SAY IIF(wk_ogar == 'S',  ;
  'EN GARANTIA   ',  ;
  'FUERA GARANTIA')
@ 15, 11 SAY 'N§ Orden    : ' +  ;
  STR(wk_numord, 8)
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(DATE())
@ 17, 11 SAY 'Garantia    :' +  ;
  wk_ogar
@ 18, 11 SAY 'Estado      :' +  ;
  wk_oest
@ 19, 11 SAY 'Tecnico     :' +  ;
  STR(wk_otec, 9)
@ 20, 11 SAY 'Notas       :'
@ 21, 25 SAY nota1 PICTURE  ;
  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 22, 25 SAY nota2 PICTURE  ;
  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 23, 25 SAY nota3 PICTURE  ;
  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 24, 25 SAY nota4 PICTURE  ;
  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 25, 25 SAY nota5 PICTURE  ;
  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 26, 25 SAY nota6 PICTURE  ;
  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
= INKEY(0)
RESTORE SCREEN FROM wk_pantax
RETURN
*
PROCEDURE veestado
SAVE SCREEN TO wk_pantax
ON KEY LABEL f9
ON KEY LABEL f4
ON KEY LABEL f5
ON KEY LABEL f7
USE SHARED st_mvord ORDER CODIGO
SET ORDER TO 2
wk_clave1 = STR(wk_numord, 8)
SET NEAR ON
SEEK '&wk_clave1'
SET NEAR OFF
wk_numero = SPACE(8)
campox = 'dtoc(dia)+"  "+hora+" "+tecnico+"   "+estado+"  "+destado'
DEFINE WINDOW ayu4 FROM 14, 3 TO  ;
       15, 74 SHADOW
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
PROCEDURE ver
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
sw_cuenta = sw_cuenta + 1
des = 1
IF wk_var == 1
     IF sw_cuenta = 1
          lin = 40
          anc = 75
          des = 1
          com = 1
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
          ENDIF
          DIMENSION wk_codsin(  ;
                    15)
          DIMENSION wk_acceso(  ;
                    15)
          DIMENSION wk_observ(  ;
                    06)
          wk_numero = wk_usar
          wk_numaux = STR(wk_usar,  ;
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
     ENDIF
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'VOL', 'BBB', 'ESC'
     dos = .T.
     DO WHILE dos
          wk_inkey = 0
          DO WHILE  .NOT.  ;
             (STR(wk_inkey, 2)$ ;
             ' 5,24,18, 3, 1, 6,27,-4' ;
             )
               wk_inkey = INKEY(0)
          ENDDO
          DO CASE
               CASE wk_inkey ==  ;
                    27 .OR.  ;
                    wk_inkey == - ;
                    4
                    IF wk_inkey ==  ;
                       27
                         ppal = .F.
                    ENDIF
                    dos = .F.
               OTHERWISE
                    DO mueve3  ;
                       WITH  ;
                       CHR(wk_inkey)
          ENDCASE
     ENDDO
ELSE
     IF sw_cuenta = 1
          lin = 40
          anc = 75
          des = 1
          com = 1
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
          ENDIF
          wk_numero = wk_usar
          wk_numaux = STR(wk_usar,  ;
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
          wk_numaux = STR(wk_usar,  ;
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
          wk_aux = STR(wk_tecnic,  ;
                   9)
          wk_aux = 'DIST' +  ;
                   wk_nomdis
          SEEK '&wk_aux'
          wk_desdis = tab_destab
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
     ENDIF
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'VOL', 'BBB', 'ESC'
     dos = .T.
     DO WHILE dos
          wk_inkey = 0
          DO WHILE  .NOT.  ;
             (STR(wk_inkey, 2)$ ;
             ' 5,24,18, 3, 1, 6,27,-4' ;
             )
               wk_inkey = INKEY(0)
          ENDDO
          DO CASE
               CASE wk_inkey ==  ;
                    27 .OR.  ;
                    wk_inkey == - ;
                    4
                    IF wk_inkey ==  ;
                       27
                         ppal = .F.
                    ENDIF
                    dos = .F.
               OTHERWISE
                    DO mueve3  ;
                       WITH  ;
                       CHR(wk_inkey)
          ENDCASE
     ENDDO
ENDIF
RESTORE SCREEN FROM wk_pantax
RETURN
*
FUNCTION sol3
PARAMETER cod
cod = STR(cod, 8)
IF LEN(TRIM(cod)) == 0
     DO error WITH  ;
        '** N§ Solicitud debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
SELECT 1
USE SHARED st_isrep ORDER CODIGO
SEEK '&cod'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** Error Solicitud NO EXISTE. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF SUBSTR(indest, 1, 1) == 'N'
     USE
     DO error WITH  ;
        '** Error Solicitud esta Anulada. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
USE
DO sacaf6
RETURN .T.
*
PROCEDURE ve_soli
SAVE SCREEN TO polo9
ACTIVATE SCREEN
@ 24, 69 SAY SPACE(11)
ACTIVATE WINDOW trabajo
SET DISPLAY TO VGA50
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 42, 76
ZOOM WINDOW indicar NORM FROM 45,  ;
     0 TO 48, 76
ACTIVATE SCREEN
@ 49, 69 SAY ind_prg
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
IF config_prg == 2
     ON KEY LABEL f8 do veorden
ENDIF
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
USE SHARED st_iclpr ORDER CODIGO
SEEK '&wk_cliaux'
wk_noment = noment
wk_nomcal = nomcal
wk_nomdis = nomdis
wk_nomciu = nomciu
wk_numte1 = numte1
wk_numte2 = numte2
wk_aux = wk_codmar + wk_codmod
USE SHARED st_imode ORDER CODIGO
SEEK '&wk_aux'
wk_aux = codcla
SELECT 2
USE SHARED st_isint ORDER CODIGO
SELECT 1
USE SHARED st_issre ORDER CODIGO
SEEK '&wk_numaux'
i = 1
IF FOUND()
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
ENDIF
CLOSE DATABASES
DO col_bk1b
DO col_bk2b
FOR i = des TO (lin + des - 1)
     @ i - des, 0 SAY  ;
       SUBSTR(solic(i), com,  ;
       anc)
ENDFOR
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
ppal8 = .T.
DO WHILE ppal8
     wk_inkey = 0
     IF config_prg = 2
          DO WHILE  .NOT.  ;
             (STR(wk_inkey, 2)$ ;
             ' 5,24,18, 3, 1, 6,27,-9' ;
             )
               wk_inkey = INKEY(0)
          ENDDO
     ELSE
          DO WHILE  .NOT.  ;
             (STR(wk_inkey, 2)$ ;
             ' 5,24,18, 3, 1, 6,27,-9' ;
             )
               wk_inkey = INKEY(0)
          ENDDO
     ENDIF
     DO CASE
          CASE wk_inkey == 27  ;
               .OR. wk_inkey == - ;
               9
               ppal8 = .F.
          OTHERWISE
               DO mueve3 WITH  ;
                  CHR(wk_inkey)
     ENDCASE
ENDDO
SET DISPLAY TO VGA25
ACTIVATE SCREEN
@ 24, 69 SAY ind_prg
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
ACTIVATE WINDOW trabajo
RESTORE SCREEN FROM polo9
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

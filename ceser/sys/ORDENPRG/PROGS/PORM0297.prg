*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'PROCESO'
titu1 = '  PROCESO  '
titu2 = ' REINGRESO DE SALIDA '
wrk_progra = PROGRAM()
CLOSE DATABASES
DO crea_win
ON KEY LABEL F6 do ayuda12
ON KEY LABEL f10 do fcinco
@ 02, 1 SAY DATE()
DO saycenter WITH 1, titu1
DO saycenter WITH 2, titu2
STORE SPACE(05) TO w_codemp
@ 06, 10 CLEAR TO 10, 65
@ 06, 10 TO 10, 65
SAVE SCREEN TO w_panta
SELECT 1
USE ge_tab0 ORDER codigo
SELECT 2
USE st_iorep ORDER codigo
SELECT 3
USE st_isrep ORDER codigo
SELECT 4
USE st_iclpr ORDER codigo
SELECT 5
USE st_imode ORDER codigo
SELECT 6
USE st_sint ORDER sin_lincod
SELECT 7
USE st_sicli ORDER codigo
SELECT 8
USE st_idpre ORDER codigo
SELECT 9
USE st_itecn ORDER codigo
SELECT 10
USE gc_vnd00 ORDER codigo
ppal = .T.
SELECT ge_tab0
w_varbus = '"IGV " + "IGV "'
seek &w_varbus
IF FOUND()
     w_facigv = tab_factor / 100
ELSE
     DO error WITH  ;
        '**No Definido el '+ ;
        empre9+'**'
     ppal = .F.
ENDIF
STORE SPACE(4) TO w_tipgar
DIMENSION w_nota( 6)
DO WHILE ppal
     RESTORE SCREEN FROM w_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     efecin = 1
     STORE SPACE(38) TO w_nota(  ;
           1), w_nota( 2),  ;
           w_nota( 3), w_nota( 4),  ;
           w_nota( 5), w_nota(  ;
           6)
     STORE SPACE(8) TO w_numaux,  ;
           w_solici
     STORE 0 TO w_numord,  ;
           w_codcli, w_numstk,  ;
           w_abonos
     STORE SPACE(4) TO w_indori,  ;
           w_codmar, w_est,  ;
           w_codtall, w_linea,  ;
           w_emisor, w_indest,  ;
           w_codstk, w_codmon
     STORE SPACE(9) TO w_codtec1
     w_codmod = SPACE(15)
     STORE SPACE(20) TO w_numser,  ;
           w_nomcal, w_nomdis,  ;
           w_nomciu
     STORE SPACE(30) TO w_esorde,  ;
           w_noment, w_destia,  ;
           w_estado, w_desdis,  ;
           w_desciu, w_prove,  ;
           w_doga, w_fecvta,  ;
           w_fecga
     STORE CTOD('  /  /  ') TO  ;
           w_fecemi, w_feccom
     STORE SPACE(5) TO w_codemp
     STORE SPACE(1) TO w_coddes
     w_numord = 0
     STORE SPACE(8) TO w_numaux,  ;
           w_numte1, w_numte2
     @ 8, 20 SAY  ;
       ' N§ Orden Reparaci¢n '  ;
       GET w_numord PICTURE  ;
       '99999999' VALID  ;
       numord2(w_numord) WHEN  ;
       colocaf6()
     SET CURSOR ON
     READ
     IF LASTKEY() == 27
          ppal = .F.
          LOOP
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
          STR(w_numord, 8) +  ;
          '  '
     DIMENSION w_codsin( 15)
     DIMENSION w_acceso( 15)
     DIMENSION w_observ( 06)
     DIMENSION w_obsord( 06)
     SELECT st_iorep
     w_numaux = STR(w_numord, 8)
     w_numero = VAL(numsol)
     w_numsol = numsol
     SELECT st_isrep
     SEEK w_numsol
     IF FOUND()
          w_fecemi = fecemi
          w_feccom = feccom
          w_emisor = codemi
          w_codcli = VAL(codent)
          w_indori = indori
          w_indest = indest
          w_codmar = codmar
          w_codmod = codmod
          w_numser = numser
          w_abonos = monabo
          w_codmon = codmon
          w_codstk = codstk
          w_numstk = VAL(numstk)
          w_coddes = coddes
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
             'GREC'
               SELECT 20
               USE SHARED  ;
                   st_iseri ORDER  ;
                   ser_codmar
               SEEK w_codmar +  ;
                    w_codmod +  ;
                    w_numser
               IF FOUND()
                    w_prove = 'Proveedor:' +  ;
                              ALLTRIM(codent)
                    w_doga = 'Doc.Garan:' +  ;
                             ALLTRIM(docgar)
                    w_fecvta = 'Fecha Vta:' +  ;
                               DTOC(fecvta)
                    w_fecga = 'Fecha Fin:' +  ;
                              DTOC(fecgar)
               ENDIF
          ENDIF
          w_cliaux = 'C' +  ;
                     STR(w_codcli,  ;
                     9)
          SELECT st_iclpr
          SEEK w_cliaux
          IF FOUND()
               w_noment = noment
               w_nomcal = nomcal
               w_nomdis = nomdis
               w_nomciu = nomciu
               w_numte1 = numte1
               w_numte2 = numte2
          ENDIF
          w_aux = w_codmar +  ;
                  w_codmod
          SELECT st_imode
          SEEK w_aux
          IF FOUND()
               w_aux = codcla
               w_linea = linea
          ENDIF
          SELECT st_sicli
          SEEK w_numsol
          i = 1
          IF FOUND()
               DO WHILE  .NOT.  ;
                  EOF() .AND.  ;
                  numdoc== ;
                  w_numsol
                    w_aux2 = SUBSTR(codsin,  ;
                             2,  ;
                             3)
                    SELECT st_sint
                    SEEK w_linea +  ;
                         w_aux2
                    IF FOUND()
                         w_codsin(  ;
                                 i) =  ;
                                 SUBSTR(dessin,  ;
                                 1,  ;
                                 35)
                         i = i +  ;
                             1
                    ENDIF
                    SELECT st_sicli
                    SKIP
               ENDDO
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
     ppal2 = .T.
     DO modiord
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
RETURN
*
PROCEDURE modiord
ACTIVATE WINDOW indicar
SAVE SCREEN TO w_pantax
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 28, 65
@ 14, 09 CLEAR TO 27, 65
@ 14, 09 TO 27, 65
@ 14, 27 SAY  ;
  ' Reingreso de Salidas ' COLOR  ;
  SCHEME 8
w_oaux = w_indori
SELECT st_iorep
w_ogar = indori
w_oest = auxest
w_otec = VAL(codtec)
FOR i = 1 TO 6
     w_obsord( i) = SUBSTR(observ,  ;
             1 + ((i - 1) * 38),  ;
             38)
     w_obsord( i) = w_obsord(i) +  ;
             SPACE(38 -  ;
             LEN(w_obsord(i)))
ENDFOR
w_obux = 'ESOR' + w_oest
SELECT ge_tab0
SEEK w_obux
IF FOUND()
     @ 18, 35 SAY  ;
       SUBSTR(tab_destab, 1, 28)
ENDIF
w_obux = STR(w_otec, 9)
SEEK 'INGA' + w_ogar
IF FOUND()
     w_destia = SUBSTR(tab_destab,  ;
                1, 30)
ENDIF
SELECT st_itecn
SEEK w_obux
IF FOUND()
     @ 19, 35 SAY SUBSTR(noment,  ;
       1, 28)
ENDIF
@ 17, 35 SAY w_destia
@ 15, 11 SAY 'N§ Orden    : ' +  ;
  STR(w_numord, 8)
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(DATE())
@ 17, 11 SAY 'Tipo Atenc. : ' +  ;
  w_ogar
@ 18, 11 SAY 'Estado      : ' +  ;
  w_oest
@ 19, 11 SAY 'T‚cnico     : ' +  ;
  STR(w_otec, 9)
@ 20, 11 SAY 'Notas       : '
FOR i = 1 TO 6
     @ 20 + i, 25 SAY w_obsord(i)
ENDFOR
IF LASTKEY() <> 27
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     DO empleado
     IF LASTKEY() = 27
          ACTIVATE WINDOW indicar
          RESTORE SCREEN FROM  ;
                  w_pantax
          ACTIVATE WINDOW trabajo
          RETURN
     ENDIF
     DO esc_indica WITH 2, 'GRA',  ;
        'BBB', 'IGN', 'ESC'
     w_key = 0
     DO WHILE w_key<>-1 .AND.  ;
        w_key<>-9 .AND. w_key<> ;
        27
          w_key = INKEY(0)
     ENDDO
     IF w_key == -1
          DO mensa2 WITH  ;
             '*** Espere un momento, por favor ***',  ;
             'COLO'
          w_numaux = STR(w_numord,  ;
                     8)
          SELECT st_iorep
          seek '&w_numaux'
          IF FOUND()
               SELECT 20
               USE SHARED  ;
                   st_mvord ORDER  ;
                   eor_nroord
               SEEK w_numaux
               IF FOUND()
                    SCAN WHILE  ;
                         orden =  ;
                         STR(w_numord,  ;
                         8) .AND.   ;
                         .NOT.  ;
                         EOF()
                         IF estado =  ;
                            w_oest
                              DO rbloquea
                              DELETE
                              UNLOCK
                         ELSE
                              w_estado =  ;
                               estado
                         ENDIF
                    ENDSCAN
               ENDIF
               SELECT st_iorep
               DO rbloquea
               w_cosrep = cosrep
               REPLACE auxest  ;
                       WITH  ;
                       w_estado
               REPLACE fecest  ;
                       WITH  ;
                       DATE(),  ;
                       horest  ;
                       WITH  ;
                       TIME()
               REPLACE fecent  ;
                       WITH {},  ;
                       horent  ;
                       WITH  ;
                       SPACE(8)
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
               SELECT 20
               USE SHARED  ;
                   st_users
               APPEND BLANK
               DO rbloquea
               REPLACE codemp  ;
                       WITH  ;
                       w_codemp
               REPLACE numord  ;
                       WITH  ;
                       STR(w_numord,  ;
                       8)
               REPLACE numsol  ;
                       WITH  ;
                       w_solici
               REPLACE estado  ;
                       WITH  ;
                       '031 '
               REPLACE user WITH  ;
                       users
               REPLACE fecha WITH  ;
                       DATE()
               REPLACE hora WITH  ;
                       TIME()
               UNLOCK
               IF w_oest = '025 '
                    IF w_cosrep >  ;
                       0
                         USE GC_HVE00  ;
                             ORDER  ;
                             CODIGO
                         SEEK 'ORDE' +  ;
                              f_ceros(STR(w_numord,  ;
                              8), ;
                              10, ;
                              2)
                         IF FOUND()
                              DO rbloquea
                              DELETE
                              UNLOCK
                         ENDIF
                         USE
                         USE GC_DVE00  ;
                             ORDER  ;
                             CODIGO
                         SEEK 'ORDE' +  ;
                              f_ceros(STR(w_numord,  ;
                              8), ;
                              10, ;
                              2)
                         DO WHILE  ;
                            VAL(dve_nrodoc)= ;
                            w_numord  ;
                            .AND.  ;
                            dve_tipdoc= ;
                            'ORDE'
                              DO rbloquea
                              DELETE
                              UNLOCK
                              SKIP
                         ENDDO
                         USE
                    ENDIF
               ENDIF
          ENDIF
          DO mensa2 WITH  ;
             '*** Espere un momento, por favor ***',  ;
             'SACA'
     ENDIF
ENDIF
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM w_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE ayuda12
ON KEY LABEL F6
ON KEY LABEL F8
IF VARREAD() == 'W_NUMORD'
     SELECT st_iorep
     w_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)+" "+alltrim(indori)'
     DO ayuda4 WITH campoa,  ;
        w_origen
     SET ORDER TO codigo
ENDIF
ON KEY LABEL F6 do ayuda12
RETURN
*
FUNCTION numord2
PARAMETER num
IF num == 0
     DO error WITH  ;
        '** Error N§ debe ser Ingresado **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
w_clave = STR(num, 8)
SELECT st_iorep
seek '&w_clave'
IF  .NOT. FOUND()
     DO error WITH  ;
        '** N§ Orden Reparaci¢n No Existe **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'N   '
     DO error WITH  ;
        '** N§ Orden Reparaci¢n esta Anulada **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF (codtall > '010' .AND. codtall <  ;
   '020') .OR. (codtall > '060 '  ;
   .AND. codtall < '070 ')
     DO error WITH  ;
        '** N§ Orden Reparaci¢n es Servicio Domicilio **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest <> 'C   ' .AND. indest <>  ;
   'F   ' .AND. indest <> 'B   '
     DO error WITH  ;
        '** N§ Orden Reparaci¢n no esta Cerrada **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF (indori = 'FGAR' .OR. indori =  ;
   'FREC') .AND. auxest = '100 '
     DO error WITH  ;
        '** N§ Orden Reparaci¢n Tiene '+ ;
        IIF(indest='B   ',  ;
        'Boleta', 'Factura')+ ;
        ' **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF (auxest <> '020 ' .AND. auxest <>  ;
   '022 ' .AND. auxest <> '023 '  ;
   .AND. auxest <> '024 ' .AND.  ;
   auxest <> '025 ' .AND. auxest <>  ;
   '030 ' .AND. auxest <>  ;
   '100 ')
     DO error WITH  ;
        '** N§ Orden Reparaci¢n no tiene Salida **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
w_numaux = STR(w_numord, 8)
w_codtall = codtall
w_fecemi = fecemi
w_codmar = codmar
w_codmod = codmod
w_indori = indori
w_serie = numser
w_codtec1 = codtec
w_codent = codent
w_solici = numsol
w_numser = numser
FOR i = 1 TO 6
     w_nota( i) = SUBSTR(observ,  ;
           1 + ((i - 1) * 38),  ;
           38)
     w_nota( i) = w_nota(i) +  ;
           SPACE(38 -  ;
           LEN(w_nota(i)))
ENDFOR
RETURN .T.
*
FUNCTION empleado
DEFINE WINDOW empleado FROM 30,  ;
       20 TO 34, 58
ACTIVATE WINDOW empleado
STORE SPACE(05) TO w_codemp
DO WHILE .T.
     SET CURSOR ON
     @ 01, 02 SAY 'Usuario :' GET  ;
       w_codemp PICTURE '@!'
     READ
     IF LASTKEY() = 27
          RETURN .F.
     ENDIF
     SELECT gc_vnd00
     SEEK 'A' + w_codemp
     IF  .NOT. FOUND()
          DO error2 WITH  ;
             '*** C¢digo de Empleado No Existe ***'
          LOOP
     ENDIF
     w_nomemp = vnd_nombre
     @ 01, 17 SAY SUBSTR(w_nomemp,  ;
       1, 20)
     DEACTIVATE WINDOW empleado
     RETURN .T.
ENDDO
RETURN .T.
*
PROCEDURE col_bk1b
SELECT ge_tab0
w_aux = 'EMIS' + w_emisor
seek '&w_aux'
IF FOUND()
     w_nomemi = tab_destab
ELSE
     w_nomemi = SPACE(35)
ENDIF
w_aux = 'ESTA' + w_indest
seek '&w_aux'
IF FOUND()
     w_estado = tab_destab
ENDIF
w_aux = 'MONE' + w_codmon
seek '&w_aux'
IF FOUND()
     w_nommon = tab_destab
ELSE
     w_nommon = SPACE(35)
ENDIF
w_aux = 'DIST' + w_nomdis
seek '&w_aux'
IF FOUND()
     w_desdis = tab_destab
ENDIF
w_aux = 'PROV' + w_nomciu
seek '&w_aux'
IF FOUND()
     w_desciu = tab_destab
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
SELECT ge_tab0
seek '&w_aux'
IF FOUND()
     DO coloca WITH 05, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
w_aux = 'INGA' + w_indori
SELECT ge_tab0
seek '&w_aux'
IF FOUND()
     w_destia = SUBSTR(tab_destab,  ;
                1, 30)
ENDIF
DO coloca WITH 08, 25, w_destia
w_aux = w_codmar + w_codmod
SELECT st_imode
seek '&w_aux'
IF FOUND()
     DO coloca WITH 06, 36,  ;
        SUBSTR(nommod, 1, 30)
ENDIF
IF w_indori == 'GARA' .OR.  ;
   w_indori == 'GREC'
     DO coloca WITH 5, 51,  ;
        w_prove
     DO coloca WITH 6, 51, w_doga
     DO coloca WITH 7, 51,  ;
        w_fecvta
     DO coloca WITH 8, 51,  ;
        w_fecga
ENDIF
DO coloca WITH 19, 20, w_destia
DO coloca WITH 10, 20,  ;
   STR(w_codcli, 9)
DO coloca WITH 10, 30, w_noment
DO coloca WITH 11, 20, w_nomcal
DO coloca WITH 12, 20, w_nomdis
DO coloca WITH 12, 25, w_desdis
DO coloca WITH 13, 20, w_nomciu
DO coloca WITH 13, 25, w_desciu
DO coloca WITH 14, 20,  ;
   STR(w_numte1, 8)
DO coloca WITH 14, 30,  ;
   STR(w_numte2, 8)
DO coloca WITH 16, 65,  ;
   DTOC(w_feccom)
DO coloca WITH 20, 20,  ;
   TRANSFORM(w_abonos,  ;
   '999,999.99')
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
*** 
*** ReFox - retrace your steps ... 
***

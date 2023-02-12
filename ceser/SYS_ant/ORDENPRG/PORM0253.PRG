*-----------------------------
* Porm0253.Prg
* Ingreso de Flete
*------------------------------
titu1 = ' MANTENCION '
titu2 = ' INGRESO DE FLETE '
wrk_progra = PROGRAM()
CLOSE DATABASES
DO crea_win
ON KEY LABEL F6 do ayuda12
ON KEY LABEL F10 DO FCINCO
@ 02, 1 SAY DATE()
DO saycenter WITH 1, titu1
DO saycenter WITH 2, titu2
STORE SPACE(05) TO wk_codemp
DO esc_modo WITH 'I'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
@ 6, 10 CLEAR TO 10, 65
@ 6, 10 TO 10, 65
SAVE SCREEN TO wk_panta
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER codigo
SELECT 3
USE SHARED st_isrep ORDER codigo
SELECT 4
USE SHARED st_iclpr ORDER codigo
SELECT 5
USE SHARED st_imode ORDER codigo
SELECT 6
USE SHARED st_sint ORDER  ;
    sin_lincod
SELECT 7
USE SHARED st_sicli ORDER codigo
SELECT 8
USE SHARED st_idpre ORDER codigo
SELECT 9
USE SHARED st_itecn ORDER codigo
SELECT 10
USE SHARED gc_vnd00 ORDER codigo
SELECT 11
USE SHARED st_iseri ORDER  ;
    ser_codmar
ppal = .T.
SELECT ge_tab0
wk_varbus = '"IGV " + "IGV "'
seek &WK_varbus
IF FOUND()
     w_facigv = tab_factor / 100
ELSE
     DO error WITH  ;
        '**No Definido el IGV**'
     ppal = .F.
ENDIF
SELECT 2
DO WHILE ppal
     RESTORE SCREEN FROM wk_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     efecin = 1
     wk_numord = 0
     STORE SPACE(8) TO wk_numaux
     @ 8, 20 SAY  ;
       ' N§ Orden Reparaci¢n '  ;
       GET wk_numord PICTURE  ;
       '99999999' VALID  ;
       numord2(wk_numord) WHEN  ;
       colocaf6()
     SET CURSOR ON
     READ
     IF LASTKEY() == 27
          ppal = .F.
          LOOP
     ENDIF
     ACTIVATE SCREEN
     @ 24, 69 SAY SPACE(11)
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
          STR(wk_numord, 8) +  ;
          '  '
     DIMENSION wk_codsin( 15)
     DIMENSION wk_acceso( 15)
     DIMENSION wk_observ( 06)
     DIMENSION wk_obsord( 06)
     SELECT st_iorep
     wk_numaux = STR(wk_numord,  ;
                 8)
     wk_numero = VAL(numsol)
     wk_numsol = numsol
     SELECT st_isrep
     SEEK wk_numsol
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
                   1 + ((i - 1) *  ;
                   35), 35)
          wk_acceso( i) =  ;
                   wk_acceso(i) +  ;
                   SPACE(35 -  ;
                   LEN(wk_acceso(i)))
          IF i <= 6
               wk_observ( i) =  ;
                        SUBSTR(observ,  ;
                        1 + ((i -  ;
                        1) * 45),  ;
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
     SELECT st_iclpr
     SEEK wk_cliaux
     wk_noment = noment
     wk_nomcal = nomcal
     wk_nomdis = nomdis
     wk_nomciu = nomciu
     wk_numte1 = numte1
     wk_numte2 = numte2
     wk_aux = wk_codmar +  ;
              wk_codmod
     SELECT st_imode
     SEEK wk_aux
     wk_aux = codcla
     wk_linea = linea
     SELECT st_sicli
     SEEK wk_numsol
     i = 1
     IF FOUND()
          DO WHILE  .NOT. EOF()  ;
             .AND. numdoc== ;
             wk_numsol
               wk_aux2 = SUBSTR(codsin,  ;
                         2, 3)
               SELECT st_sint
               SEEK wk_linea +  ;
                    wk_aux2
               wk_codsin( i) =  ;
                        SUBSTR(dessin,  ;
                        1, 35)
               i = i + 1
               SELECT st_sicli
               SKIP
          ENDDO
     ENDIF
     DO col_bk1b
     DO col_bk2b
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'MOD',  ;
        'BBB', 'IGN', 'ESC'
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
DO saca_win
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
@ 14, 09 CLEAR TO 27, 65
@ 14, 09 TO 27, 65
@ 14, 21 SAY  ;
  ' Orden  de  Reparaci¢n ' COLOR  ;
  SCHEME 8
wk_oaux = wk_indori
SELECT st_iorep
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
wk_obux = 'ESOR' + wk_oest
SELECT ge_tab0
SEEK wk_obux
@ 18, 35 SAY SUBSTR(tab_destab, 1,  ;
  28)
wk_obux = STR(wk_otec, 9)
SEEK 'INGA' + wk_ogar
wk_destia = SUBSTR(tab_destab, 1,  ;
            30)
SELECT st_itecn
SEEK wk_obux
@ 19, 35 SAY SUBSTR(noment, 1,  ;
  28)
@ 17, 35 SAY wk_destia
@ 15, 11 SAY 'N§ Orden    : ' +  ;
  STR(wk_numord, 8)
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(DATE())
@ 17, 11 SAY 'Tipo Atenc. : ' +  ;
  wk_ogar
@ 18, 11 SAY 'Estado      : ' +  ;
  wk_oest
@ 19, 11 SAY 'T‚cnico     : ' +  ;
  STR(wk_otec, 9)
@ 20, 11 SAY 'Notas       : '
FOR i = 1 TO 6
     @ 20 + i, 25 SAY  ;
       wk_obsord(i)
ENDFOR
IF LASTKEY() <> 27
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'GRA',  ;
        'BBB', 'IGN', 'ESC'
     DEFINE WINDOW emplead1 FROM  ;
            29, 19 TO 37, 55
     ACTIVATE WINDOW emplead1
     SELECT st_iorep
     w_flete = ROUND(flete * (1 +  ;
               w_facigv), 2)
     @ 00, 19 SAY 'S/.'
     @ 01, 01 SAY 'Repuesto :'
     @ 01, 12 SAY cosrep PICTURE  ;
       '999,999.99'
     @ 02, 01 SAY 'Mano Obra:'
     @ 02, 12 SAY cosmob PICTURE  ;
       '999,999.99'
     @ 03, 01 SAY 'Flete    :'  ;
       GET w_flete PICTURE  ;
       '999,999.99'
     READ
     IF LASTKEY() = 27
          ACTIVATE WINDOW indicar
          RESTORE SCREEN FROM  ;
                  wk_pantax
          ACTIVATE WINDOW trabajo
          RETURN
     ENDIF
     w_flete = ROUND(w_flete / (1 +  ;
               w_facigv), 2)
     w_totnet = cosrep + cosmob +  ;
                w_flete
     w_totigv = ROUND(w_totnet *  ;
                w_facigv, 2)
     @ 04, 01 SAY 'Totnet   :'
     @ 04, 12 SAY w_totnet  ;
       PICTURE '999,999.99'
     @ 05, 01 SAY 'Totigv   :'
     @ 05, 12 SAY w_totigv  ;
       PICTURE '999,999.99'
     @ 06, 01 SAY 'Totgen   :'
     @ 06, 12 SAY w_totnet +  ;
       w_totigv PICTURE  ;
       '999,999.99'
     DO empleado
     wk_key = 0
     DO WHILE wk_key<>-1 .AND.  ;
        wk_key<>-9 .AND. wk_key<> ;
        27
          wk_key = INKEY(0)
     ENDDO
     IF wk_key == -1
          SELECT st_iorep
          SEEK wk_numaux
          IF FOUND()
               DO rbloquea
               REPLACE flete WITH  ;
                       w_flete
               REPLACE subtot  ;
                       WITH  ;
                       cosrep +  ;
                       cosmob +  ;
                       flete,  ;
                       totnet  ;
                       WITH  ;
                       subtot,  ;
                       totigv  ;
                       WITH  ;
                       ROUND(totnet *  ;
                       w_facigv,  ;
                       2), totbru  ;
                       WITH  ;
                       totnet +  ;
                       totigv
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
          ENDIF
     ENDIF
ENDIF
IF LASTKEY() == 27
     efecin = 1
ENDIF
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE ayuda12
ON KEY LABEL F6
ON KEY LABEL F8
IF VARREAD() == 'WK_NUMORD'
     SELECT st_iorep
     wk_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)+" "+alltrim(indori)'
     DO ayuda4 WITH campoa,  ;
        wk_origen
     SET ORDER TO codigo
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
SELECT st_iorep
SEEK wk_clave
IF  .NOT. FOUND()
     DO error WITH  ;
        '** N§ Orden Reparaci¢n NO EXISTE. **'
     RETURN .F.
ENDIF
IF indest = 'N   '
     DO error WITH  ;
        '** N§ Orden Reparaci¢n esta ANULADA. **'
     RETURN .F.
ENDIF
IF indest = 'F   '
     DO error WITH  ;
        '** N§ Orden Reparaci¢n FACTURADA. **'
     RETURN .F.
ENDIF
IF indest = 'B   '
     DO error WITH  ;
        '** N§ Orden Reparaci¢n BOLETEADA. **'
     RETURN .F.
ENDIF
IF auxest = '020 ' .OR. auxest =  ;
   '022 ' .OR. auxest = '023 '  ;
   .OR. auxest = '024 ' .OR.  ;
   auxest = '025 ' .OR. auxest =  ;
   '028 ' .OR. auxest = '029 '  ;
   .OR. auxest = '030 '
     DO error WITH  ;
        '*** Orden de Reparaci¢n  ya sali¢ ***'
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION empleado
DEFINE WINDOW empleado FROM 38,  ;
       19 TO 43, 55
ACTIVATE WINDOW empleado
ON KEY LABEL F8
STORE SPACE(05) TO wk_codemp
DO WHILE .T.
     SET CURSOR ON
     @ 01, 02 SAY 'Usuario :' GET  ;
       wk_codemp PICTURE '@!'
     READ
     IF LASTKEY() = 27
          RETURN .F.
     ENDIF
     SELECT gc_vnd00
     SEEK 'A' + wk_codemp
     IF  .NOT. FOUND()
          DO error2 WITH  ;
             '*** C¢digo de Empleado NO EXISTE ***'
          LOOP
     ENDIF
     wk_nomemp = vnd_nombre
     @ 01, 17 SAY  ;
       SUBSTR(wk_nomemp, 1, 20)
     DEACTIVATE WINDOW empleado
     RETURN .T.
ENDDO
RETURN .T.
*
FUNCTION ooseri2
PARAMETER wk_marca, wk_modelo,  ;
          wk_numser
SELECT st_iseri
SEEK wk_marca + wk_modelo +  ;
     wk_numser
IF FOUND()
     wk_desser = DTOC(st_iseri.fecvta) +  ;
                 ' ' +  ;
                 ALLTRIM(docgar)
ELSE
     wk_desser = ''
ENDIF
RETURN wk_desser
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
IF wk_indori == 'GARA' .OR.  ;
   wk_indori == 'GREC'
     DO coloca WITH 19, 20,  ;
        'EN GARANTIA   '
ELSE
     DO coloca WITH 19, 20,  ;
        'FUERA GARANTIA'
ENDIF
DO coloca WITH 10, 20,  ;
   STR(wk_codcli, 11)
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
PROCEDURE col_bk2b
FOR i = 1 TO 06
     DO coloca WITH 41+i, 2,  ;
        wk_observ(i)
ENDFOR
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

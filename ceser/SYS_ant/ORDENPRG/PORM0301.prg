*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
titu1 = ' MANTENCION '
titu2 = ' DESCUENTO POR ARTICULOS  '
wrk_progra = PROGRAM()
CLOSE DATABASES
DO crea_win
@ 02, 1 SAY DATE()
DO saycenter WITH 1, titu1
DO saycenter WITH 2, titu2
STORE SPACE(05) TO w_codemp
@ 06, 10 CLEAR TO 10, 65
@ 06, 10 TO 10, 65
SAVE SCREEN TO w_panta
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_numsol
SELECT 3
USE SHARED st_isrep ORDER codigo
SELECT 4
USE SHARED st_itecn ORDER codigo
SELECT 5
USE SHARED st_iclpr ORDER codigo
ppal = .T.
SELECT ge_tab0
w_varbus = '"IGV " + "IGV "'
seek &w_varbus
IF FOUND()
     w_facigv = tab_factor / 100
ELSE
     DO error WITH  ;
        '**No Definido el IGV**'
     ppal = .F.
ENDIF
SELECT st_iorep
DO WHILE ppal
     RESTORE SCREEN FROM w_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     efecin = 1
     STORE 0 TO w_numord,  ;
           w_desmob, w_desrep,  ;
           w_numero, w_destot
     STORE SPACE(10) TO w_acce,  ;
           w_pasace
     DIMENSION infodes( 3)
     STORE SPACE(8) TO w_numaux
     @ 8, 20 SAY  ;
       ' N§ Solicitud de Servicio  : '
     ON KEY LABEL F6 do ayuda12
     ON KEY LABEL f10 do fcinco
     @ 8, 50 GET w_numero PICTURE  ;
       '99999999' VALID  ;
       ordnul(w_numero) WHEN  ;
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
          STR(w_numord, 8) +  ;
          '  '
     DIMENSION w_codsin( 15)
     DIMENSION w_acceso( 15)
     DIMENSION w_observ( 06)
     DIMENSION w_obsord( 06)
     SELECT st_iorep
     SET ORDER TO ord_numsol
     SEEK STR(w_numero, 8)
     w_numaux = numdoc
     w_numord = VAL(numdoc)
     SELECT st_isrep
     SEEK STR(w_numero, 8)
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
                  1 + ((i - 1) *  ;
                  35), 35)
          w_acceso( i) =  ;
                  w_acceso(i) +  ;
                  SPACE(35 -  ;
                  LEN(w_acceso(i)))
          IF i <= 6
               w_observ( i) =  ;
                       SUBSTR(observ,  ;
                       1 + ((i -  ;
                       1) * 45),  ;
                       45)
               w_observ( i) =  ;
                       w_observ(i) +  ;
                       SPACE(45 -  ;
                       LEN(w_observ(i)))
          ENDIF
     ENDFOR
     w_cliaux = 'C' +  ;
                STR(w_codcli,  ;
                11)
     SELECT st_iclpr
     SEEK w_cliaux
     w_noment = noment
     w_nomcal = nomcal
     w_nomdis = nomdis
     w_nomciu = nomciu
     w_numte1 = numte1
     w_numte2 = numte2
     w_aux = w_codmar + w_codmod
     SELECT 20
     USE SHARED st_imode ORDER  ;
         codigo
     SEEK w_aux
     w_aux = codcla
     w_linea = linea
     SELECT 20
     USE SHARED st_sint ORDER  ;
         sin_lincod
     SELECT 21
     USE SHARED st_sicli ORDER  ;
         codigo
     SEEK STR(w_numero, 8)
     i = 1
     IF FOUND()
          DO WHILE  .NOT. EOF()  ;
             .AND. numdoc== ;
             STR(w_numero, 8)
               w_aux2 = SUBSTR(codsin,  ;
                        2, 3)
               SELECT st_sint
               SEEK w_linea +  ;
                    w_aux2
               w_codsin( i) =  ;
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
SAVE SCREEN TO w_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'IGN', 'ESC'
@ 14, 10 FILL TO 28, 65
@ 14, 09 CLEAR TO 27, 65
@ 14, 09 TO 27, 65
@ 14, 28 SAY  ;
  ' Orden  de  Reparaci¢n ' COLOR  ;
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
w_desmob = desmob
w_desrep = desrep
w_destot = 0
w_obux = 'ESOR' + w_oest
SELECT ge_tab0
SEEK w_obux
@ 18, 35 SAY SUBSTR(tab_destab, 1,  ;
  28)
w_obux = STR(w_otec, 9)
SEEK 'INGA' + w_ogar
w_destia = SUBSTR(tab_destab, 1,  ;
           30)
SELECT st_itecn
SEEK w_obux
@ 19, 35 SAY SUBSTR(noment, 1,  ;
  28)
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
     ON KEY
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     DEFINE WINDOW emplead1 FROM  ;
            30, 10 TO 39, 66
     ACTIVATE WINDOW emplead1
     SELECT st_iorep
     w_flete = ROUND(flete * (1 +  ;
               w_facigv), 2)
     @ 00, 22 SAY  ;
       ALLTRIM(empre13)
     @ 00, 50 SAY  ;
       ALLTRIM(empre13)
     @ 01, 01 SAY 'Repuesto   :'
     @ 01, 13 SAY cosrep PICTURE  ;
       '99,999,999.99'
     @ 02, 01 SAY 'Mano Obra  :'
     @ 02, 13 SAY cosmob PICTURE  ;
       '99,999,999.99'
     @ 03, 01 SAY 'Flete      :'
     @ 03, 13 SAY w_flete PICTURE  ;
       '99,999,999.99'
     w_totnet = cosrep + cosmob +  ;
                w_flete
     w_totigv = ROUND(w_totnet *  ;
                w_facigv, 2)
     @ 01, 27 SAY  ;
       'Total neto   :'
     @ 01, 41 SAY w_totnet  ;
       PICTURE '99,999,999.99'
     @ 02, 27 SAY 'Total ' +  ;
       ALLTRIM(empre9) + '    :'
     @ 02, 41 SAY w_totigv  ;
       PICTURE '99,999,999.99'
     @ 03, 27 SAY  ;
       'Total General:'
     @ 03, 41 SAY w_totnet +  ;
       w_totigv PICTURE  ;
       '99,999,999.99'
     @ 04, 01 SAY  ;
       '                 Informe de Descuento                '  ;
       COLOR N/W 
     @ 05, 01 SAY 'M.Obra   % :'
     @ 05, 27 SAY  ;
       'Repuesto %   :'
     @ 06, 01 SAY 'Total    % :'
     @ 07, 01 SAY 'Autoriza   :'
     @ 07, 27 SAY  ;
       'Clave        :'
     @ 05, 15 GET w_desmob  ;
       PICTURE '999.99' VALID  ;
       (w_desmob >= 0 .AND.  ;
       w_desmob <= 100)
     @ 05, 43 GET w_desrep  ;
       PICTURE '999.99' VALID  ;
       (w_desrep >= 0 .AND.  ;
       w_desrep <= 100)
     @ 06, 15 GET w_destot  ;
       PICTURE '999.99' VALID  ;
       (w_destot >= 0 .AND.  ;
       w_destot <= 100) WHEN  ;
       (w_desmob = 0 .AND.  ;
       w_desrep = 0)
     @ 07, 15 GET w_acce PICTURE  ;
       '@!' VALID val_gra(1)
     @ 07, 43 GET w_pasace  ;
       PICTURE '@!' VALID  ;
       val_gra(2) COLOR ,W/W 
     READ
     IF LASTKEY() = 27
          ACTIVATE WINDOW indicar
          RESTORE SCREEN FROM  ;
                  w_pantax
          ACTIVATE WINDOW trabajo
          RETURN
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'GRA',  ;
        'BBB', 'IGN', 'ESC'
     w_key = 0
     ACTIVATE WINDOW emplead1
     DO WHILE w_key<>-1 .AND.  ;
        w_key<>-9 .AND. w_key<> ;
        27
          w_key = INKEY(0)
     ENDDO
     IF w_key == -1
          SELECT st_iorep
          SET ORDER TO codigo
          SEEK w_numaux
          IF FOUND()
               DO rbloquea
               IF w_destot = 0
                    REPLACE desmob  ;
                            WITH  ;
                            w_desmob
                    REPLACE desrep  ;
                            WITH  ;
                            w_desrep
               ELSE
                    REPLACE desmob  ;
                            WITH  ;
                            w_destot
                    REPLACE desrep  ;
                            WITH  ;
                            w_destot
               ENDIF
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
          ENDIF
     ENDIF
     DEACTIVATE WINDOW emplead1
ENDIF
IF LASTKEY() == 27
     efecin = 1
ENDIF
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM w_pantax
ACTIVATE WINDOW trabajo
RETURN
*
FUNCTION val_gra
PARAMETER opc
SELECT 20
USE SHARED password
IF opc = 1
     IF w_acce = SPACE(10)
          DO error2 WITH  ;
             'Ingrese Usuario'
          RETURN .F.
     ENDIF
     SET ORDER TO usuario
     SEEK ALLTRIM(w_acce)
     IF  .NOT. FOUND()
          DO error2 WITH  ;
             'Usuario no reconocido'
          RETURN .F.
     ENDIF
ELSE
     IF w_pasace = SPACE(10)
          DO error2 WITH  ;
             'Ingrese Clave'
          RETURN .F.
     ENDIF
     w_cla2 = ''
     w_len = LEN(ALLTRIM(w_pasace))
     FOR i = 1 TO w_len
          w_cla2 = w_cla2 +  ;
                   SUBSTR(w_pasace,  ;
                   (w_len + 1) -  ;
                   i, 1)
     ENDFOR
     SET ORDER TO codigo
     SEEK ALLTRIM(w_cla2) +  ;
          ALLTRIM(w_acce)
     IF  .NOT. FOUND()
          DO error2 WITH  ;
             'Clave  no valida '
          w_pasace = SPACE(10)
          RETURN .F.
     ENDIF
ENDIF
IF nivelo <> 'A4' .AND. nivelo <>  ;
   'A7'
     DO error2 WITH  ;
        'Usuario sin acceso a Descuento'
     RETURN .F.
ENDIF
RETURN .T.
*
PROCEDURE ayuda12
ON KEY
SELECT st_iorep
campoa = 'numsol+" "+dtoc(fecemi)+" "+numdoc+" "+substr(numser,1,12)+" "+codent+" "+substr(codmod,1,10)+" "+subst(indest,1,2)+" "+alltrim(indori)'
w_origen = 'OR'
w_orden = ORDER()
DO ayuda8 WITH campoa, w_origen,  ;
   SELECT()
SELECT st_iorep
set order to &w_orden
ON KEY LABEL F6 do ayuda12
RETURN
*
FUNCTION ordnul
PARAMETER cod
IF cod = 0
     DO error WITH  ;
        '** Error  N§ debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
cod = STR(cod, 8)
SELECT st_isrep
seek '&cod'
IF  .NOT. FOUND()
     DO error WITH  ;
        '** Error Solicitud NO EXISTE. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF SUBSTR(indest, 1, 1) == 'N'
     DO error WITH  ;
        '** Error Solicitud esta Anulada. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indori <> 'FGAR' .AND. indori <>  ;
   'FREC'
     DO error WITH  ;
        '** N§ Solicitud de Servicio no puede ser '+ ;
        indori+' **'
     RETURN .F.
ENDIF
IF indest = 'F   '
     DO error WITH  ;
        '** N§ Solicitud de Servicio FACTURADA. **'
     RETURN .F.
ENDIF
SELECT st_iorep
SET ORDER TO ord_numsol
seek '&cod'
IF  .NOT. FOUND()
     DO error WITH  ;
        '** Error Orden de Reparaci¢n No Existe. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
w_numord = VAL(numdoc)
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
seek '&w_aux'
w_estado = tab_destab
w_aux = 'MONE' + w_codmon
seek '&w_aux'
IF FOUND()
     w_nommon = tab_destab
ELSE
     w_nommon = SPACE(35)
ENDIF
w_aux = 'DIST' + w_nomdis
seek '&w_aux'
w_desdis = tab_destab
w_aux = 'PROV' + w_nomciu
seek '&w_aux'
w_desciu = tab_destab
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
     DO coloca WITH 08, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
w_aux = w_codmar + w_codmod
SELECT 20
USE SHARED st_imode ORDER codigo
SEEK '&w_aux'
IF FOUND()
     DO coloca WITH 06, 36,  ;
        SUBSTR(nommod, 1, 30)
ENDIF
IF w_indori == 'GARA' .OR.  ;
   w_indori == 'GREC'
     DO coloca WITH 19, 20,  ;
        'EN GARANTIA   '
ELSE
     IF w_indori == 'PVEN' .OR.  ;
        w_indori == 'PREC'
          DO coloca WITH 19, 20,  ;
             'PREVENTA   '
     ELSE
          DO coloca WITH 19, 20,  ;
             'FUERA GARANTIA'
     ENDIF
ENDIF
DO coloca WITH 10, 22,  ;
   STR(w_codcli, 11)
DO coloca WITH 10, 34, w_noment
DO coloca WITH 11, 20, w_nomcal
DO coloca WITH 12, 20, w_nomdis
DO coloca WITH 12, 25, w_desdis
DO coloca WITH 13, 20, w_nomciu
DO coloca WITH 13, 25, w_desciu
DO coloca WITH 14, 20,  ;
   STR(w_numte1, 7)
DO coloca WITH 14, 30,  ;
   STR(w_numte2, 7)
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

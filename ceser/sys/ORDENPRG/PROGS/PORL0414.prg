*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
ind_prg = '<PORL0414>'
titu1 = 'REPORTE'
titu2 = 'DUPLICADO DE O/R'
wrk_progra = PROGRAM()
DO crea_win
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
wk_numaux = 0
USE
STORE SPACE(30) TO wrk_desmar,  ;
      wrk_despro, wrk_desdis
ppal = .T.
PUBLIC tecnico
DO WHILE ppal
     RESTORE SCREEN FROM wk_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     efecin = 1
     wk_numord = 0
     @ 8, 20 SAY  ;
       ' N� Orden Reparaci�n '  ;
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
     wk_numero = VAL(numsol)
     wk_var = 1
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
          '               DUPLICADO DE ORDEN DE REPARACION               N� ' +  ;
          STR(wk_numord, 8) +  ;
          '  '
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
     wk_abonos = monabo
     wk_codmon = codmon
     wk_codstk = codstk
     wk_numstk = VAL(numstk)
     wk_coddes = coddes
     FOR i = 1 TO 15
          wk_codsin( i) =  ;
                   SPACE(35)
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
     DO usedbf WITH 'st_iclpr',  ;
        'CODIGO'
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
     wk_aux = linea
     SELECT 2
     USE SHARED st_sint ORDER  ;
         sin_lincod
     SELECT 1
     USE SHARED st_sicli ORDER  ;
         CODIGO
     SEEK '&wk_numaux'
     i = 1
     IF FOUND()
          DO WHILE  .NOT. EOF()  ;
             .AND. numdoc== ;
             wk_numaux
               wk_aux2 = wk_aux +  ;
                         SUBSTR(codsin,  ;
                         2, 3)
               SELECT st_sint
               SEEK '&wk_aux2'
               wk_codsin( i) =  ;
                        SUBSTR(dessin,  ;
                        1, 35)
               i = i + 1
               SELECT st_sicli
               SKIP
          ENDDO
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
RELEASE tecnico
RETURN
*
PROCEDURE modiord
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 28, 65 COLOR N+/ ;
  N 
@ 13, 09 CLEAR TO 27, 64
@ 13, 09 TO 27, 64
@ 13, 32 SAY  ;
  'Imprime Orden de Reparaci�n'
USE SHARED st_iorep ORDER CODIGO
wk_numaux = STR(wk_numord, 8)
SEEK '&wk_numaux'
wk_ogar = indori
wk_oest = auxest
wk_otec = VAL(codtec)
nota1 = SUBSTR(observ, 1, 38)
nota2 = SUBSTR(observ, 39, 38)
nota3 = SUBSTR(observ, 77, 38)
nota4 = SUBSTR(observ, 115, 38)
nota5 = SUBSTR(observ, 153, 38)
nota6 = SUBSTR(observ, 191, 38)
wk_obux = 'ESOR' + wk_oest
DO usedbf WITH 'ge_tab0',  ;
   'codigo'
SEEK '&wk_obux'
@ 18, 35 SAY SUBSTR(tab_destab, 1,  ;
  28)
SEEK 'MARC' + wk_codmar
wrk_desmar = tab_destab
SEEK 'DIST' + wk_nomdis
wrk_desdis = tab_destab
SEEK 'PROV' + wk_nomciu
wrk_despro = tab_destab
SEEK 'INGA' + wk_ogar
wk_destia = tab_destab
wk_obux = STR(wk_otec, 9)
USE SHARED st_itecn ORDER CODIGO
SEEK '&wk_obux'
@ 19, 35 SAY SUBSTR(noment, 1,  ;
  28)
USE
wrk_dogtia = ooseri2(wk_codmar, ;
             wk_codmod, ;
             wk_numser)
@ 17, 30 SAY SUBSTR(wk_destia, 1,  ;
  27) + ALLTRIM(wrk_dogtia)
@ 15, 11 SAY 'N� Orden    : ' +  ;
  STR(wk_numord, 8)
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(DATE())
@ 17, 11 SAY 'Garantia    : ' +  ;
  wk_ogar
@ 18, 11 SAY 'Estado      : ' +  ;
  wk_oest PICTURE '@!'
@ 19, 11 SAY 'T�cnico     : ' +  ;
  STR(wk_otec, 8) PICTURE  ;
  '999999999'
@ 20, 11 SAY 'Notas       :'
@ 21, 25 SAY nota1 PICTURE '@!'
@ 22, 25 SAY nota2 PICTURE '@!'
@ 23, 25 SAY nota3 PICTURE '@!'
@ 24, 25 SAY nota4 PICTURE '@!'
@ 25, 25 SAY nota5 PICTURE '@!'
@ 26, 25 SAY nota6 PICTURE '@!'
IF LASTKEY() <> 27
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'IMP', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     wk_key = 0
     DO WHILE wk_key<>-6 .AND.  ;
        wk_key<>-9 .AND. wk_key<> ;
        27
          wk_key = INKEY(0)
     ENDDO
     IF wk_key == -6
          wk_print = PRINTSTATUS()
          DO WHILE  .NOT.  ;
             wk_print
               DO error2 WITH  ;
                  '** Error en Impresora. Continua ? (S/N) '
               IF LASTKEY() == 27
                    EXIT
               ENDIF
               wk_print = PRINTSTATUS()
          ENDDO
          wk_print = PRINTSTATUS()
          IF wk_print
               DO imp_ord
          ENDIF
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'RAC',  ;
        'BBB', 'IGN', 'ESC'
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
IF VARREAD() == 'WK_OTEC'
     USE SHARED st_itecn ORDER  ;
         CODIGO
     campoa = '" "+codent+" "+noment+" "+CODTEC'
     campob = '" "+noment+" "+codent+" "+CODTEC'
     titulo = 'AYUDA DE TECNICOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
     USE
ENDIF
IF VARREAD() == 'WK_NUMORD'
     DO usedbf WITH 'st_iorep',  ;
        'CODIGO'
     wrk_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)'
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
        '** Error N� debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF num <= wk_numaux
     DO error WITH  ;
        '** Error N� debe ser mayor que '+ ;
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
        '** Error N� debe ser Ingresado. **'
     RETURN .F.
ENDIF
wk_clave = STR(num, 8)
USE SHARED st_iorep ORDER CODIGO
SEEK '&wk_clave'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** N� Orden Reparacion NO EXISTE. **'
     RETURN .F.
ENDIF
IF indest = 'N   '
     USE
     DO error WITH  ;
        '** N� Orden Reparacion esta NULA. **'
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
             '** Error Solicitud en Procesos. **'
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
   'CODIGO'
SEEK '&wk_numaux'
wk_numsol = numsol
tit_client = wk_noment
tit_codigo = STR(wk_codcli, 11)
tit_direcc = wk_nomcal
tit_feccom = wk_feccom
tit_tipo = wk_ogar
wk_fecha = DTOC(fecemi)
wk_horemi = horemi
tit_fechho = SUBSTR(wk_fecha, 1,  ;
             2) + SPACE(3) +  ;
             SUBSTR(wk_fecha, 4,  ;
             2) + SPACE(3) +  ;
             SUBSTR(wk_fecha, 7,  ;
             2)
tit_tit2 = 'SINTOMAS :'
tit_tit3 = 'ACCESORIOS :'
tit_tit4 = 'OBSERVACIONES :'
tit_tit5 = 'DUPLICADO DE ORDEN DE REPARACION'
tit_subray = '��������������������������������'
tit_tit6 = 'INF.TEC. / NOTAS :'
tit_fecha = wk_fecha + ' - ' +  ;
            wk_horemi
tit_eminro = 'O/R  N�: ' +  ;
             wk_numaux
tit_nrosol = 'S/S  N�: ' +  ;
             wk_numsol
tit_codmod = wk_codmod
tit_desmod = wk_desmod
DIMENSION wk_sin( 9), wk_acc( 9),  ;
          wk_obs( 3)
wk_acc( 1) = wk_acceso(1) +  ;
      SUBSTR(wk_acceso(2), 01,  ;
      30)
wk_acc( 2) = SUBSTR(wk_acceso(02),  ;
      31, 05) + wk_acceso(03) +  ;
      SUBSTR(wk_acceso(04), 01,  ;
      25)
wk_acc( 3) = SUBSTR(wk_acceso(04),  ;
      26, 10) + wk_acceso(05) +  ;
      SUBSTR(wk_acceso(06), 01,  ;
      20)
wk_acc( 4) = SUBSTR(wk_acceso(06),  ;
      21, 15) + wk_acceso(07) +  ;
      SUBSTR(wk_acceso(08), 01,  ;
      15)
wk_acc( 5) = SUBSTR(wk_acceso(08),  ;
      16, 20) + wk_acceso(09) +  ;
      SUBSTR(wk_acceso(10), 01,  ;
      10)
wk_acc( 6) = SUBSTR(wk_acceso(10),  ;
      11, 25) + wk_acceso(11) +  ;
      SUBSTR(wk_acceso(12), 01,  ;
      05)
wk_acc( 7) = SUBSTR(wk_acceso(12),  ;
      06, 30) + wk_acceso(13)
wk_acc( 8) = wk_acceso(14) +  ;
      SUBSTR(wk_acceso(15), 01,  ;
      30)
wk_acc( 9) = SUBSTR(wk_acceso(15),  ;
      31, 05)
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
@ 03, 109 SAY wk_emisor
@ 03, 115 SAY tit_nrosol
@ 04, 115 SAY tit_fecha
@ 05, 015 SAY tit_client
@ 05, 075 SAY tit_codigo
@ 06, 015 SAY tit_direcc
@ 06, 115 SAY tit_feccom
@ 07, 015 SAY wrk_desdis
@ 07, 075 SAY wrk_despro
@ 07, 115 SAY wk_numte1
@ 09, 105 SAY wk_destia
@ 10, 000 SAY tit_desmod
@ 10, 030 SAY wk_codmod
@ 10, 050 SAY wrk_desmar
@ 10, 075 SAY wk_numser
IF tit_tipo = 'GARA'
     @ 10, 105 SAY  ;
       ALLTRIM(wrk_dogtia)
ENDIF
@ 11, 002 SAY tit_tit2
@ 11, 070 SAY tit_tit3
FOR lin = 1 TO 9
     @ 11 + lin, 02 SAY  ;
       wk_codsin(lin)
     @ 11 + lin, 70 SAY  ;
       wk_acceso(lin)
ENDFOR
@ 21, 002 SAY tit_tit6
@ 21, 080 SAY 'T�cnico :'
@ 21, 090 SAY STR(wk_otec, 9)
@ 21, 103 SAY SUBSTR(wrk_destec,  ;
  1, 25)
nota1 = SUBSTR(observ, 1, 76)
nota2 = SUBSTR(observ, 77, 76)
nota3 = SUBSTR(observ, 153, 76)
@ 22, 002 SAY nota1
@ 23, 002 SAY nota2
@ 24, 002 SAY nota3
@ 25, 002 SAY tit_tit4
@ 25, 002 SAY tit_tit4
@ 26, 002 SAY SUBSTR(wk_obs(1), 1,  ;
  76)
@ 27, 002 SAY SUBSTR(wk_obs(1),  ;
  77, 14) + SUBSTR(wk_obs(2), 1,  ;
  62)
@ 28, 002 SAY SUBSTR(wk_obs(2),  ;
  63, 28) + SUBSTR(wk_obs(3), 1,  ;
  48)
@ 28, 097 SAY tit_fechho
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
                  ' ' + docgar
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

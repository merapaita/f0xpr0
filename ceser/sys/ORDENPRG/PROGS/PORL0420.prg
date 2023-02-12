*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
ind_prg = PROGRAM()
titu1 = 'REPORTE'
titu2 = 'CONSOLIDADO DE LA O/R'
CLOSE DATABASES
@ 24, 69 SAY ind_prg
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
     CLOSE DATABASES
     RESTORE SCREEN FROM wk_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     efecin = 1
     STORE 0 TO wk_numord, wk_reg,  ;
           wk_tem
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
          '               DUPLICADO DE ORDEN DE REPARACION               N§ ' +  ;
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
     USE SHARED st_iclpr ORDER  ;
         CODIGO
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
CLOSE DATABASES
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 28, 65 COLOR N+/ ;
  N 
@ 13, 09 CLEAR TO 27, 64
@ 13, 09 TO 27, 64
@ 13, 32 SAY  ;
  'Imprime Orden de Reparaci¢n'
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
USE SHARED ge_tab0 ORDER codigo
SEEK '&wk_obux'
wrk_destab = tab_destab
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
@ 15, 11 SAY 'N§ Orden    : ' +  ;
  STR(wk_numord, 8)
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(DATE())
@ 17, 11 SAY 'Garantia    : ' +  ;
  wk_ogar
@ 18, 11 SAY 'Estado      : ' +  ;
  wk_oest PICTURE '@!'
@ 19, 11 SAY 'T‚cnico     : ' +  ;
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
          KEYBOARD '{ESC}'
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
     USE SHARED st_iorep ORDER  ;
         CODIGO
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
        '** Error N§ debe ser Ingresado **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
wk_clave = STR(num, 8)
USE SHARED st_iorep ORDER CODIGO
SEEK '&wk_clave'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** N§ Orden Reparaci¢n NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest = 'N   '
     USE
     DO error WITH  ;
        '** N§ Orden Reparaci¢n esta ANULADO **'
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
wk_con = 0
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
SELECT 3
USE SHARED st_iorep ORDER CODIGO
SEEK '&wk_numaux'
SELECT st_idped.numdoc,  ;
       st_idped.numord,  ;
       st_idped.codpro,  ;
       st_idped.canpro,  ;
       st_idped.valpro,  ;
       st_idped.totite,  ;
       st_iprep.codalm,  ;
       st_iprep.indest,  ;
       gc_pro00.pro_descri,  ;
       gc_pro00.pro_propie FROM  ;
       ST_IDPED, ST_IPREP,  ;
       GC_PRO00 WHERE  ;
       st_iprep.numdoc =  ;
       st_idped.numdoc AND  ;
       st_iprep.numord =  ;
       st_idped.numord AND  ;
       gc_pro00.pro_codpro =  ;
       st_idped.codpro AND  ;
       st_iprep.indest <> 'N' AND  ;
       st_idped.numord =  ;
       wk_numaux ORDER BY  ;
       st_idped.numord,  ;
       st_idped.numdoc INTO  ;
       CURSOR DETALLE
GOTO TOP
wk_reg = RECCOUNT()
IF wk_reg <> 0
     DIMENSION repue( wk_reg)
     repue( 1) = SPACE(60)
     i = 1
     SCAN WHILE  .NOT. EOF()
          repue( i) =  ;
               detalle.codpro +  ;
               SPACE(2) +  ;
               detalle.pro_descri +  ;
               STR(detalle.canpro,  ;
               9, 2) +  ;
               STR(detalle.valpro,  ;
               9, 2) + SPACE(1) +  ;
               STR(detalle.totite,  ;
               9, 2) + SPACE(1) +  ;
               detalle.codalm +  ;
               SPACE(1) +  ;
               detalle.numdoc +  ;
               SPACE(1) +  ;
               detalle.pro_propie
          i = i + 1
     ENDSCAN
ENDIF
SELECT 3
wk_numsol = numsol
tit_client = wk_noment
tit_codigo = STR(wk_codcli, 11)
tit_direcc = wk_nomcal
tit_feccom = wk_feccom
tit_tipo = wk_destia
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
tit_tit5 = 'CONSOLIDADO DE ORDEN DE REPARACION'
tit_subray = 'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
tit_tit6 = 'INF.TEC. / NOTAS :'
tit_tit7 = 'CONSUMO DE REPUESTOS'
tit_fecha = wk_fecha + ' - ' +  ;
            wk_horemi
tit_eminro = 'O/R  N§: ' +  ;
             wk_numaux
tit_nrosol = 'S/S  N§: ' +  ;
             wk_numsol
tit_codmod = wk_codmod
tit_desmod = wk_desmod
DIMENSION wk_sin( 9), wk_acc( 9),  ;
          wk_obs( 3)
wk_sin( 1) = wk_codsin(1) +  ;
      SUBSTR(wk_codsin(2), 01,  ;
      30)
wk_sin( 2) = wk_codsin(03) +  ;
      SUBSTR(wk_codsin(04), 01,  ;
      30)
wk_acc( 1) = wk_acceso(1) +  ;
      SUBSTR(wk_acceso(2), 01,  ;
      30)
wk_acc( 2) = SUBSTR(wk_acceso(02),  ;
      31, 05) + wk_acceso(03) +  ;
      SUBSTR(wk_acceso(04), 01,  ;
      25)
wk_tem = 1
wk_con = wk_reg / 5
IF MOD(wk_reg, 5) > 0
     wk_con = wk_con + 1
ENDIF
IF wk_con = 0
     wk_con = 1
ENDIF
FOR x = 1 TO wk_con
     wk_tem = 1 + (5 * (x - 1))
     DO imprime
ENDFOR
EJECT
USE
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
RETURN
*
PROCEDURE imprime
??? CHR(15)
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
@ 07, 113 SAY wk_numte1
@ 09, 105 SAY tit_tipo
@ 10, 000 SAY tit_desmod
@ 10, 030 SAY wk_codmod
@ 10, 050 SAY wrk_desmar
@ 10, 075 SAY wk_numser
IF wk_indori = 'GARA'
     @ 10, 105 SAY  ;
       ALLTRIM(wrk_dogtia)
ENDIF
@ 11, 01 SAY tit_tit2
@ 11, 70 SAY tit_tit3
FOR lin = 1 TO 2
     @ 11 + lin, 001 SAY  ;
       wk_sin(lin)
     @ 11 + lin, 070 SAY  ;
       wk_acceso(lin)
ENDFOR
@ 14, 001 SAY tit_tit7
@ 14, 70 SAY 'Repuesto.:'
@ 14, 80 SAY cosrep PICTURE  ;
  '@$99,999.99'
@ 14, 95 SAY 'Total....:'
@ 14, 105 SAY totnet PICTURE  ;
  '@$99,999.99'
@ 15, 01 SAY 'Estado: ' +  ;
  wrk_destab
@ 15, 70 SAY 'Mano Obra:'
@ 15, 80 SAY cosmob PICTURE  ;
  '@$99,999.99'
@ 15,95 SAY '&empre9......:'
@ 15, 105 SAY totigv PICTURE  ;
  '@$99,999.99'
@ 16, 70 SAY 'Flete....:'
@ 16, 80 SAY flete PICTURE  ;
  '@$99,999.99'
@ 16, 95 SAY 'T.General:'
@ 16, 105 SAY totbru PICTURE  ;
  '@$99,999.99'
IF wk_reg <> 0
     @ 17, 08 SAY SPACE(2) +  ;
       'Descripci¢n' + SPACE(34) +  ;
       'Cant.' + SPACE(2) +  ;
       'P.Unit.' + SPACE(2) +  ;
       'P.Total' + SPACE(1) +  ;
       'Almac‚n' + SPACE(1) +  ;
       'NøPed.' + 'Propie'
     c = 1
     FOR i = wk_tem TO wk_reg
          IF c = 1 .AND. wk_tem >  ;
             5
               @ 18, 01 SAY  ;
                 'VIE...'
          ENDIF
          @ 17 + c, 08 SAY  ;
            repue(i)
          IF c = 5
               @ 22, 124 SAY  ;
                 'VAN...'
               EXIT
          ENDIF
          c = c + 1
     ENDFOR
ENDIF
@ 24, 01 SAY tit_tit6
@ 24, 030 SAY 'T‚cnico :'
@ 24, 040 SAY STR(wk_otec, 9)
@ 24, 50 SAY SUBSTR(wrk_destec, 1,  ;
  25)
nota1 = SUBSTR(observ, 1, 76)
nota2 = SUBSTR(observ, 77, 76)
nota3 = SUBSTR(observ, 153, 76)
nota4 = SUBSTR(observ, 229, 76)
@ 25, 001 SAY nota1
@ 26, 001 SAY nota2
@ 27, 001 SAY nota3
@ 28, 001 SAY nota4
@ 28, 097 SAY tit_fechho
??? CHR(15)
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

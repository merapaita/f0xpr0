*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
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
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
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
USE IN 17 SHARED gc_cmv00 AGAIN  ;
    ORDER cmv_feinmo
w_tipcam = ootc2(DATE(),'SOL ', ;
           'DOL ','2')
DO usedbf WITH 'ge_tab0',  ;
   'codigo'
wrk_varbus = '"IGV " + "IGV "'
SEEK &wrk_varbus
IF FOUND()
     wk_igv = tab_factor
     wrk_facigv = tab_factor /  ;
                  100
ELSE
     DO error WITH  ;
        '**No Definido el IGV**'
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
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     efecin = 1
     STORE 0 TO veces, wk_otec,  ;
           wk_numord, w_pagsol,  ;
           w_pagdol, wk_flete,  ;
           sol_cosfle, wk_repues
     STORE ' ' TO wk_ogar,  ;
           wk_oest, wrk_numfac,  ;
           nota1, nota2, nota3,  ;
           nota4, nota5, nota6,  ;
           wk_esor
     STORE 0 TO wk_totrep,  ;
           s_totrep, wk_totdes,  ;
           s_totdes, wk_totnet,  ;
           s_totnet, wk_totafe,  ;
           wk_mano
     STORE 0 TO wk_totman,  ;
           s_totman, wk_totgrl,  ;
           s_totgrl, wk_totigv,  ;
           s_totigv, s_totafe,  ;
           wk_numsol
     STORE SPACE(30) TO wk_nom1,  ;
           wk_nom2
     STORE SPACE(4) TO w_linea,  ;
           wk_indest, wk_indori
     STORE SPACE(10) TO  ;
           wrk_numfac
     IF config_prg == 2
          ppal = .T.
          DO WHILE ppal
               efecin = 1
               @ 8, 20 SAY  ;
                 ' N§ Orden Reparaci¢n '  ;
                 GET wk_numord  ;
                 PICTURE  ;
                 '99999999' VALID  ;
                 numord(wk_numord)  ;
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
          FOR i = 1 TO 6
               nota( i) =  ;
                   SUBSTR(observ,  ;
                   1 + ((i - 1) *  ;
                   45), 45)
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
     DIMENSION wk_acceso( 15),  ;
               wk_observ( 6)
     IF wk_var = 2
          USE SHARED st_isrep  ;
              ORDER CODIGO
          seek '&wk_numaux'
          FOR i = 1 TO 15
               wk_codsin( i) =  ;
                        SPACE(35)
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
     ELSE
          USE SHARED st_isrep  ;
              ORDER CODIGO
          seek '&wk_numaux'
          FOR i = 1 TO 15
               wk_codsin( i) =  ;
                        SPACE(35)
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
     ENDIF
     wk_cliaux = 'C' +  ;
                 STR(wk_codcli,  ;
                 11)
     IF  .NOT. USED('st_iclpr')
          USE SHARED st_iclpr  ;
              ORDER CODIGO
     ELSE
          SELECT st_iclpr
          SET ORDER TO codigo
     ENDIF
     SEEK '&wk_cliaux'
     wk_noment = noment
     wk_numte1 = numte1
     wk_numte2 = numte2
     wk_aux = wk_codmar +  ;
              wk_codmod
     USE SHARED st_imode ORDER  ;
         CODIGO
     SEEK '&wk_aux'
     wk_aux = codcla
     w_linea = linea
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
               wk_aux2 = SUBSTR(codsin,  ;
                         2, 3)
               SELECT 2
               SEEK w_linea +  ;
                    wk_aux2
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
     ACTIVATE WINDOW trabajo
     SET DISPLAY TO VGA50
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO 42, 76
     ZOOM WINDOW indicar NORM  ;
          FROM 45, 0 TO 48, 76
     ACTIVATE SCREEN
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
        'OOE', 'INF', 'IGN'
     DO esc_indica WITH 2, 'OOR',  ;
        'OOA', 'OTR', 'ESC'
     ppal2 = .T.
     DO WHILE ppal2
          wk_inkey = 0
          IF config_prg = 2
               DO WHILE  .NOT.  ;
                  (STR(wk_inkey,  ;
                  2)$ ;
                  ' 18, 3, 1, 6,27,-9-3-4-6-8-7' ;
                  )
                    wk_inkey = INKEY(0)
               ENDDO
          ELSE
               DO WHILE  .NOT.  ;
                  (STR(wk_inkey,  ;
                  2)$ ;
                  ' 18, 3, 1, 6,27,-9-7' ;
                  )
                    wk_inkey = INKEY(0)
               ENDDO
          ENDIF
          DO CASE
               CASE wk_inkey ==  ;
                    18
                    DO mueve3a  ;
                       WITH  ;
                       CHR(wk_inkey)
               CASE wk_inkey == 3
                    DO mueve3a  ;
                       WITH  ;
                       CHR(wk_inkey)
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
                       'OTR',  ;
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
                    27
                    IF wk_inkey ==  ;
                       27
                         ppal = .F.
                    ENDIF
                    sw_cuenta = 0
                    ppal2 = .F.
                    LOOP
               CASE wk_inkey == - ;
                    9
                    ppal = .T.
                    sw_cuenta = 0
                    ppal2 = .F.
                    LOOP
               CASE wk_inkey == - ;
                    8
                    DO cuenta
               CASE wk_inkey == - ;
                    7
                    DO informa
               OTHERWISE
                    DO mueve3a  ;
                       WITH  ;
                       CHR(wk_inkey)
          ENDCASE
     ENDDO
     SET DISPLAY TO VGA25
     ACTIVATE SCREEN
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO 17, 76
     ZOOM WINDOW indicar NORM  ;
          FROM 20, 0 TO 23, 76
     ACTIVATE WINDOW trabajo
ENDDO
CLOSE DATABASES
ON KEY LABEL f9
ON KEY LABEL F6
ON KEY LABEL F8
ON KEY LABEL F10
ON KEY LABEL f11
SET DISPLAY TO VGA25
DEACTIVATE WINDOW cliente
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DO sacawin
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
          wrk_origen = 'PR'
     ENDIF
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+SUBSTR(NUMSER,1,12)+" "+codent+" "+codmod+" "+subst(indest,1,2)'
     DO ayuda40 WITH campoa,  ;
        wrk_origen
     USE
ENDIF
IF VARREAD() == 'WK_NUMORD' .AND.  ;
   config_prg > 1
     USE SHARED st_iorep ORDER  ;
         CODIGO
     wrk_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)+"  "+ALLTRIM(INDORI)'
     DO ayuda40 WITH campoa,  ;
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
wk_clave = STR(num, 8)
USE st_iorep ORDER CODIGO
SEEK '&wk_clave'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** N§ Orden Reparaci¢n NO EXISTE. **'
     RETURN .F.
ENDIF
IF indest = 'N'
     USE
     DO error WITH  ;
        '*** N§ Orden Reparaci¢n esta ANULADA ***'
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
USE st_iorep ORDER CODIGO
SEEK '&wk_clave'
IF  .NOT. FOUND()
     USE
     DO error2 WITH  ;
        '** N§ Orden Reparaci¢n NO EXISTE. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'N'
     USE
     DO error2 WITH  ;
        '** N§ Orden Reparaci¢n esta ANULADA **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
USE
RETURN .T.
*
FUNCTION ordnul
PARAMETER cod, var
IF cod = 0
     DO error WITH  ;
        '** Error N§ debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
cod = STR(cod, 8)
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
     USE st_ispre ORDER CODIGO
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
veces = 0
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW indicar
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 28, 65
@ 13, 09 CLEAR TO 28, 64
@ 13, 09 TO 28, 64
@ 13, 30 SAY ' Consulta Orden '  ;
  COLOR SCHEME 8
wk_oaux = wk_indori
veces = veces + 1
IF veces = 1
     USE SHARED st_iorep ORDER  ;
         CODIGO
     wk_numaux = STR(wk_numord,  ;
                 8)
     SEEK '&wk_numaux'
     wk_ogar = indori
     wk_oest = auxest
     wrk_taller = codtall
     wrk_fecemi = fecemi
     wrk_fecest = fecest
     wk_otec = VAL(codtec)
     wrk_numfac = numfabo
     FOR i = 1 TO 6
          notao( i) =  ;
               SUBSTR(observ, 1 +  ;
               ((i - 1) * 38),  ;
               38)
     ENDFOR
     wk_obux = 'ESOR' + wk_oest
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SEEK '&wk_obux'
     wk_nom1 = SUBSTR(tab_destab,  ;
               1, 28)
     SEEK 'INGA' + wk_ogar
     wk_destia = SUBSTR(tab_destab,  ;
                 1, 28)
     SEEK 'TALL' + wrk_taller
     wrk_destal = SUBSTR(tab_destab,  ;
                  1, 28)
     wk_obux = STR(wk_otec, 9)
     USE SHARED st_itecn ORDER  ;
         CODIGO
     SEEK '&wk_obux'
     wk_nom2 = SUBSTR(noment, 1,  ;
               28)
     USE
ENDIF
@ 15, 11 SAY 'N§ Orden    : ' +  ;
  STR(wk_numord, 8)
@ 16, 11 SAY 'Fecha Orden : ' +  ;
  DTOC(wrk_fecemi)
@ 17, 11 SAY 'Garantia    : ' +  ;
  wk_ogar + SPACE(7) + wk_destia
@ 18, 11 SAY 'Taller      : ' +  ;
  wrk_taller + SPACE(7) +  ;
  wrk_destal
@ 19, 11 SAY 'Estado      : ' +  ;
  wk_oest + SPACE(7) + wk_nom1
@ 20, 11 SAY 'T‚cnico     : ' +  ;
  STR(wk_otec, 9) + SPACE(2) +  ;
  wk_nom2
@ 21, 11 SAY 'Notas       :'
FOR i = 1 TO 6
     @ 21 + i, 25 SAY notao(i)  ;
       PICTURE '@!'
ENDFOR
@ 28, 11 SAY ALLTRIM(wk_nom1) +  ;
  '  ' + ALLTRIM(wrk_numfac)  ;
  COLOR N/W* 
@ 28, 54 SAY wrk_fecest COLOR N/W* 
= INKEY(0, 'H')
RESTORE SCREEN FROM wk_pantax
RETURN
*
PROCEDURE veestado
SAVE SCREEN TO wk_pantax
ON KEY LABEL f9
ON KEY LABEL f4
ON KEY LABEL f5
ON KEY LABEL f7
DO usedbf WITH 'st_mvord',  ;
   'CODIGO'
SET ORDER TO 2
wk_clave1 = STR(wk_numord, 8)
SET NEAR ON
SEEK '&wk_clave1'
SET NEAR OFF
wk_numero = SPACE(8)
campox = 'dtoc(dia)+" "+hora+" "+tecnico+" "+estado+" "+destado'
DEFINE WINDOW ayu4 FROM 14, 3 TO  ;
       15, 74 SHADOW
browse field cer = " " :H="", uno = &campox;
:H="  Fecha  Hora       T‚cnico Situaci¢n     Descripci¢n ";
key wk_clave1 in window ayu4 nowait freeze;
cer   
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
USE
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
                      11)
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
          IF FOUND()
               wk_aux = codcla
               w_linea = linea
          ENDIF
          STORE SPACE(20) TO  ;
                wk_doga, wk_prov,  ;
                wk_fevt, wk_fecg
          IF wk_indori = 'GARA'  ;
             .OR. wk_indori =  ;
             'GREC' .OR.  ;
             wk_indori = 'PVEN'  ;
             .OR. wk_indori =  ;
             'PREC'
               USE SHARED  ;
                   ST_ISERI ORDER  ;
                   SER_CODMAR
               SET STEP ON
               SEEK wk_codmar +  ;
                    wk_codmod +  ;
                    wk_numser
               IF FOUND()
                    wk_prov = 'Proveedor:' +  ;
                              ALLTRIM(codent)
                    wk_doga = 'Doc.Garan:' +  ;
                              ALLTRIM(docgar)
                    wk_fevt = 'Fecha Vta:' +  ;
                              DTOC(fecvta)
                    wk_fecg = 'Fecha Fin:' +  ;
                              DTOC(fecgar)
               ELSE
                    wk_prov = 'Proveedor:'
                    wk_doga = 'Doc.Garan:'
                    wk_fevt = 'Fecha Vta:'
                    wk_fecg = 'Fecha Fin:'
               ENDIF
          ENDIF
          SELECT 2
          USE SHARED st_sint  ;
              ORDER sin_lincod
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
                    SEEK w_linea +  ;
                         wk_aux2
                    IF FOUND()
                         wk_codsin(  ;
                          i) =  ;
                          SUBSTR(dessin,  ;
                          1, 35)
                    ENDIF
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
        'OOA', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'VOL', 'BBB', 'ESC'
     dos = .T.
     DO WHILE dos
          wk_inkey = 0
          DO WHILE  .NOT.  ;
             (STR(wk_inkey, 2)$ ;
             ' 18, 3, 1, 6,27,-4-8,-7' ;
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
               CASE wk_inkey ==  ;
                    18
               CASE wk_inkey == 3
                    DO cuenta
               CASE wk_inkey == - ;
                    8
                    DO cuenta
               CASE wk_inkey == - ;
                    7
                    DO informa
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
          DIMENSION solic( 50)
          STORE FOPEN('presupue.txt')  ;
                TO file_handl
          FOR i = 1 TO 50
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
                    pres( 12),  ;
                    dec( 12),  ;
                    tot( 12),  ;
                    tots( 12),  ;
                    sto( 12)
          DIMENSION wk_obspre(  ;
                    08)
          FOR i = 1 TO 12
               pro( i) =  ;
                  SPACE(14)
               dex( i) =  ;
                  SPACE(20)
               uni( i) = SPACE(3)
               sto( i) = 0
               can( i) = 0
               pre( i) = 0
               dec( i) = 0
               tot( i) = 0
               IF i <= 8
                    wk_obspre( i) =  ;
                             SPACE(38)
               ENDIF
          ENDFOR
          wk_numaux = STR(wk_numsol,  ;
                      8)
          USE SHARED st_isrep  ;
              ORDER CODIGO
          SEEK '&wk_numaux'
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
          IF codmon = 'DOL '
               w_pagdol = monabo
               w_pagsol = ROUND(monabo *  ;
                          w_tipcam,  ;
                          2)
          ELSE
               w_pagsol = monabo
               w_pagdol = ROUND(monabo /  ;
                          w_tipcam,  ;
                          2)
          ENDIF
          wk_cliaux = 'C' +  ;
                      STR(wk_codcli,  ;
                      11)
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
          IF FOUND()
               wk_aux = codcla
               w_linea = linea
          ENDIF
          SELECT 2
          USE SHARED st_sint  ;
              ORDER sin_lincod
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
                    SEEK w_linea +  ;
                         wk_aux2
                    IF FOUND()
                         wk_codsin(  ;
                          i) =  ;
                          SUBSTR(dessin,  ;
                          1, 35)
                    ENDIF
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
          wk_codmon = codmon
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
          FOR i = 1 TO 8
               wk_obspre( i) =  ;
                        SUBSTR(observ,  ;
                        1 + ((i -  ;
                        1) * 38),  ;
                        38)
               wk_obspre( i) =  ;
                        wk_obspre(i) +  ;
                        SPACE(38 -  ;
                        LEN(wk_obspre(i)))
          ENDFOR
          USE SHARED st_idpre  ;
              ORDER CODIGO
          SEEK '&wk_numaux'
          SELECT 3
          USE SHARED gc_alm00  ;
              ORDER codigo
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
               pres( i) =  ;
                   ROUND(pre(i) *  ;
                   w_tipcam, 2)
               tots( i) =  ;
                   ROUND(pres(i) *  ;
                   can(i), 2)
               s_totrep = s_totrep +  ;
                          tots(i)
               SELECT 3
               seek '&wk_aux'
               sto( i) =  ;
                  alm_stkfis
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
          IF wk_codmon = 'DOL '
               wk_abonos = w_pagdol
          ELSE
               wk_abonos = w_pagsol
               s_totdes = pordes
               s_totnet = ROUND(s_totrep -  ;
                          (s_totrep *  ;
                          pordes /  ;
                          100),  ;
                          2)
               s_totman = ROUND(wk_totman *  ;
                          w_tipcam,  ;
                          2)
               s_totgrl = s_totnet +  ;
                          s_totman
               s_totafe = ROUND(s_totgrl /  ;
                          (1 +  ;
                          wrk_facigv),  ;
                          2)
               s_totigv = s_totgrl -  ;
                          s_totafe
          ENDIF
          wk_aux = 'EMIS' +  ;
                   wk_emisor
          USE SHARED ge_tab0  ;
              ORDER codigo
          SEEK '&wk_aux'
          wk_nomemi = SUBSTR(tab_destab,  ;
                      1, 30)
          wk_aux = 'MONE' +  ;
                   wk_codmon
          SEEK '&wk_aux'
          wk_nommon = SUBSTR(tab_destab,  ;
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
        'OOA', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'VOL', 'BBB', 'ESC'
     dos = .T.
     DO WHILE dos
          wk_inkey = 0
          DO WHILE  .NOT.  ;
             (STR(wk_inkey, 2)$ ;
             ' 5,24,18, 3, 1, 6,27,-4-3-8' ;
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
               CASE wk_inkey == - ;
                    8
                    DO cuenta
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
PROCEDURE cuenta
SAVE SCREEN TO w_pantax
ACTIVATE WINDOW indicar
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 30, 65
@ 13, 09 CLEAR TO 30, 64
@ 13, 09 TO 30, 64
@ 13,14 say 'Pagos a Cuenta y Saldo         &empre8          US $';
color scheme 8
DIMENSION fecha( 1), docum( 1),  ;
          valor( 1)
DIMENSION fechac( 1), documc( 1),  ;
          valorc( 1)
SELECT 10
USE SHARED st_iorep ORDER codigo
SEEK STR(wk_numord, 8)
w_numsol = numsol
w_numord = numdoc
w_codmar = codmar
w_codmod = codmod
w_indest = ALLTRIM(indest)
w_indori = indori
w_numfac = numfabo
w_esor = auxest
w_cosmob = cosmob
w_cosrep = cosrep
w_auxest = auxest
w_desrep = desrep
w_desmob = desmob
w_totdes = totdes
w_flete = flete
w_facigv = wrk_facigv
SELECT 11
USE SHARED gc_hve00 ORDER nrdore
STORE 0 TO wrk_toacta, i, cane,  ;
      w_tipca2, s_cosfle,  ;
      s_totgen
STORE 0 TO i, i2, s_cosrep,  ;
      s_cosmob, s_cosfle,  ;
      s_toacta, s_total, s_totpag,  ;
      s_descue
DO pormcost WITH 'st_iorep'
DO pinta
w_numsol = VAL(w_numsol)
w_numord = VAL(w_numord)
SELECT st_iorep
USE
RETURN
*
PROCEDURE pinta
p = 13
IF  .NOT. EMPTY(fecha(1))
     FOR b = 1 TO i
          p = p + 1
          @ p, 14 SAY fecha(b)
          @ p, 24 SAY docum(b)
          @ p, 35 SAY valor(b)  ;
            PICTURE  ;
            '999,999,999.99'
     ENDFOR
ENDIF
IF  .NOT. EMPTY(fechac(1)) .AND.  ;
    i2 > 0
     p = p + 1
     @ p, 13 SAY  ;
       ' Cancelado con :'
     FOR b = 1 TO i2
          p = p + 1
          @ p, 14 SAY fechac(b)
          @ p, 24 SAY documc(b)
          @ p, 35 SAY valorc(b)  ;
            PICTURE  ;
            '999,999,999.99'
     ENDFOR
ENDIF
@ 29, 11 SAY 'Tipo de Cambio : '
@ 29, 28 SAY w_tipcam PICTURE  ;
  '99.99'
p = p + 2
@ p + 0, 14 SAY  ;
  'Costo En Repuesto..:' +  ;
  SPACE(1) + TRANSFORM(s_cosrep,  ;
  '999,999,999.99')
@ p + 1, 14 SAY  ;
  'Costo Mano de Obra.:' +  ;
  SPACE(1) + TRANSFORM(s_cosmob,  ;
  '999,999,999.99')
@ p + 2, 14 SAY  ;
  'Costo Flete........:' +  ;
  SPACE(1) + TRANSFORM(s_cosfle,  ;
  '999,999,999.99')
@ p + 3, 14 SAY  ;
  'Subtotal...........:' +  ;
  SPACE(1) + TRANSFORM(s_total,  ;
  '999,999,999.99')
@ p + 4, 14 SAY  ;
  'Descuento..........:' +  ;
  SPACE(1) + TRANSFORM(s_descue,  ;
  '999,999,999.99')
@ p + 5, 14 SAY  ;
  'Total Pagos........:' +  ;
  SPACE(1) + TRANSFORM(s_toacta,  ;
  '999,999,999.99')
IF  .NOT. EMPTY(w_numfac)
     @ p + 7, 14 SAY  ;
       'Saldo a Pagar......:' +  ;
       SPACE(1) + TRANSFORM(0,  ;
       '999,999,999.99') COLOR N+/ ;
       W 
ELSE
     IF w_indest = 'V' .OR.  ;
        w_indest = 'P'
          @ p + 6, 14 SAY  ;
            'Saldo a Pagar......:' +  ;
            SPACE(1) +  ;
            TRANSFORM(s_totpag,  ;
            '999,999,999.99')  ;
            COLOR N/W 
          @ p + 6, 49 SAY  ;
            'Aprox.' +  ;
            TRANSFORM(s_totpag /  ;
            w_tipcam, '9999.99')
     ELSE
          IF (w_esor = '025 '  ;
             .OR. w_esor =  ;
             '024 ')
               @ p + 6, 14 SAY  ;
                 'Saldo a Pagar......:' +  ;
                 SPACE(1) +  ;
                 TRANSFORM(0,  ;
                 '999,999,999.99' ;
                 ) COLOR N/W 
          ELSE
               IF (w_esor =  ;
                  '029 ' .OR.  ;
                  w_esor = '028 '  ;
                  .OR. w_esor =  ;
                  '023 ' .OR.  ;
                  w_esor =  ;
                  '022 ') .AND.  ;
                  SUBSTR(w_indori,  ;
                  2, 1) = 'R'  ;
                  .AND. w_cosrep =  ;
                  0
                    @ p + 6, 14  ;
                      SAY  ;
                      'Saldo a Pagar......:' +  ;
                      SPACE(1) +  ;
                      TRANSFORM(0,  ;
                      '999,999,999.99' ;
                      ) COLOR N/W 
               ELSE
                    IF ((w_esor =  ;
                       '028 '  ;
                       .OR.  ;
                       w_esor =  ;
                       '023 ')  ;
                       .AND.  ;
                       w_indori =  ;
                       'FGAR'  ;
                       .AND.  ;
                       w_cosrep =  ;
                       0)
                         @ p + 6,  ;
                           14 SAY  ;
                           'Saldo a Pagar......:' +  ;
                           SPACE(1) +  ;
                           TRANSFORM(0,  ;
                           '999,999,999.99' ;
                           )  ;
                           COLOR  ;
                           N/W 
                    ELSE
                         @ p + 6,  ;
                           14 SAY  ;
                           'Saldo a Pagar......:' +  ;
                           SPACE(1) +  ;
                           TRANSFORM(s_totpag,  ;
                           '999,999,999.99' ;
                           ) +  ;
                           SPACE(4) +  ;
                           TRANSFORM(s_totpag /  ;
                           w_tipcam,  ;
                           '999,999.99' ;
                           )  ;
                           COLOR  ;
                           N/W 
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
= INKEY(0, 'H')
RESTORE SCREEN FROM w_pantax
RETURN
*
PROCEDURE informano
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW indicar
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 28, 65
@ 13, 09 CLEAR TO 30, 64
@ 13, 09 TO 30, 64
@ 13, 11 SAY  ;
  'Centro de Informaci¢n al Cliente' +  ;
  SPACE(3) + STR(wk_numte1, 8) +  ;
  SPACE(1) + STR(wk_numte2, 8)  ;
  COLOR SCHEME 8
DO usedbf WITH 'st_iscic',  ;
   'fecsol'
SEEK STR(wk_numso, 8)
p = 16
@ p - 1, 10 SAY 'FecInic' +  ;
  SPACE(2) + 'Fecomp' + SPACE(4) +  ;
  'Informe'
SCAN WHILE numsol = STR(wk_numso,  ;
     8) .AND.  .NOT. EOF()
     IF p >= 36
          p = 36
     ENDIF
     @ p, 10 SAY fecini
     @ p, 19 SAY feccom
     en1 = SUBSTR(inform, 1, 35)
     en2 = SUBSTR(inform, 36, 70)
     @ p, 29 SAY en1
     @ p + 1, 29 SAY en2
     p = p + 2
ENDSCAN
STORE SPACE(35) TO wk_obser1,  ;
      wk_obser2
STORE DATE() + 3 TO wk_fecco
STORE TIME() TO w_horini
@ p, 10 SAY DATE() COLOR 'n/w'
SET CURSOR ON
@ p, 19 GET wk_fecco
@ p, 29 GET wk_obser1
@ p + 1, 29 GET wk_obser2
READ
SET CURSOR OFF
IF  .NOT. EMPTY(wk_obser1)
     APPEND BLANK
     REPLACE numsol WITH  ;
             STR(wk_numso, 8),  ;
             numord WITH  ;
             STR(wk_numord, 8)
     REPLACE inform WITH  ;
             wk_obser1 +  ;
             wk_obser2, fecini  ;
             WITH DATE(), feccom  ;
             WITH wk_fecco,  ;
             horini WITH w_horini,  ;
             horfin WITH TIME(),  ;
             horcom WITH TIME()
     REPLACE user WITH users,  ;
             date WITH DATE(),  ;
             time WITH TIME()
ENDIF
RESTORE SCREEN FROM wk_pantax
RETURN
*
PROCEDURE col_bk1b
USE SHARED ge_tab0 ORDER codigo
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
USE
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
USE SHARED ge_tab0 ORDER codigo
SEEK '&wk_aux'
IF FOUND()
     DO coloca WITH 05, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
wk_aux = 'INGA' + wk_indori
USE SHARED ge_tab0 ORDER codigo
seek '&wk_aux'
IF FOUND()
     DO coloca WITH 08, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
wk_aux = wk_codmar + wk_codmod
USE SHARED st_imode ORDER CODIGO
SEEK '&wk_aux'
IF FOUND()
     DO coloca WITH 06, 36,  ;
        SUBSTR(nommod, 1, 30)
ENDIF
USE
DO coloca WITH 5, 51, wk_prov
DO coloca WITH 6, 51, wk_doga
DO coloca WITH 7, 51, wk_fevt
DO coloca WITH 8, 51, wk_fecg
IF wk_indori == 'GARA' .OR.  ;
   wk_indori == 'GREC' .OR.  ;
   wk_indori = 'PVEN' .OR.  ;
   wk_indori = 'PREC'
     DO coloca WITH 19, 20,  ;
        'EN GARANTIA   '
ELSE
     DO coloca WITH 19, 20,  ;
        'FUERA GARANTIA'
ENDIF
DO coloca WITH 10, 20,  ;
   STR(wk_codcli, 11)
DO coloca WITH 10, 32, wk_noment
DO coloca WITH 11, 20, wk_nomcal
DO coloca WITH 12, 20, wk_nomdis
DO coloca WITH 12, 25, wk_desdis
DO coloca WITH 13, 20, wk_nomciu
DO coloca WITH 13, 25, wk_desciu
DO coloca WITH 14, 20,  ;
   STR(wk_numte1, 8)
DO coloca WITH 14, 30,  ;
   STR(wk_numte2, 8)
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
PROCEDURE consumo
DO mensa2 WITH  ;
   '*** Un momento, Por Favor ... ***',  ;
   'COLO'
DEFINE POPUP detalle FROM 11, 02  ;
       TO 16, 74 PROMPT FIELDS  ;
       detalle.codpro + ' ' +  ;
       SUBSTR(detalle.pro_descri,  ;
       1, 15) + ' ' +  ;
       TRANSFORM(detalle.canpro,  ;
       '999') + ' ' +  ;
       TRANSFORM(detalle.valpro,  ;
       '999,999.99') + ' ' +  ;
       TRANSFORM(detalle.totite,  ;
       '999,999.99') + ' ' +  ;
       detalle.codalm + ' ' +  ;
       detalle.numdoc TITLE  ;
       'ProductoÄÄÄÄÄÄDescripci¢nÄÄÄÄCant.ÄÄÄPrecioÄÄÄÄÄÄTotalÄAlmÄÄÄÄPed.Ä'  ;
       IN screen COLOR SCHEME 8
SELECT DISTINCT st_idped.numdoc,  ;
       st_idped.numord,  ;
       st_idped.codpro,  ;
       st_idped.canpro,  ;
       st_idped.valpro,  ;
       st_idped.totite,  ;
       st_iprep.codalm,  ;
       st_iprep.indest,  ;
       gc_pro00.pro_descri FROM  ;
       ST_IDPED, ST_IPREP,  ;
       GC_PRO00 WHERE  ;
       st_iprep.numdoc =  ;
       st_idped.numdoc AND  ;
       st_iprep.numord =  ;
       st_idped.numord AND  ;
       st_iprep.indest <> 'N' AND  ;
       gc_pro00.pro_codpro =  ;
       st_idped.codpro AND  ;
       st_idped.numord =  ;
       STR(wrk_numord, 8) ORDER  ;
       BY st_idped.numord,  ;
       st_idped.numdoc INTO  ;
       CURSOR DETALLE
DO mensa2 WITH  ;
   '*** Un momento, Por Favor ... ***',  ;
   'SACA'
DO WHILE LASTKEY()<>27
     ACTIVATE POPUP detalle
ENDDO
KEYBOARD '{ENTER}'
RETURN
*
PROCEDURE col_bk1c
DO coloca WITH 01, 68,  ;
   STR(wk_numero, 8)
DO coloca WITH 05, 20, wk_codmar
DO coloca WITH 05, 25, wk_nommar
DO coloca WITH 06, 20, wk_codmod
DO coloca WITH 06, 36, wk_nommod
DO coloca WITH 07, 20, wk_numser
DO coloca WITH 08, 25,  ;
   STR(wk_numstk, 9)
DO coloca WITH 10, 20,  ;
   STR(wk_codcli, 11)
DO coloca WITH 10, 32, wk_noment
DO coloca WITH 11, 20, wk_nomcal
DO coloca WITH 12, 20, wk_nomdis
DO coloca WITH 12, 25, wk_desdis
DO coloca WITH 13, 20, wk_nomciu
DO coloca WITH 13, 25, wk_desciu
DO coloca WITH 14, 20,  ;
   STR(wk_numte1, 8)
DO coloca WITH 14, 30,  ;
   STR(wk_numte2, 8)
DO coloca WITH 16, 20,  ;
   DTOC(wk_fecemi)
DO coloca WITH 16, 65,  ;
   DTOC(wk_fecven)
DO coloca WITH 18, 20, wk_emisor
DO coloca WITH 18, 25, wk_nomemi
DO coloca WITH 19, 20, wk_indori
DO coloca WITH 19, 25, wk_destia
DO coloca WITH 20, 20, wk_codmon
DO coloca WITH 20, 25,  ;
   SUBSTR(wk_nommon, 1, 20)
DO coloca WITH 20, 60,  ;
   TRANSFORM(wk_abonos,  ;
   '999,999.99')
DO coloca WITH 21, 20,  ;
   STR(wk_tecnic, 9)
DO coloca WITH 21, 30, wk_nomtec
FOR i = 1 TO 12
     IF pro(i) <> SPACE(14)
          DO coloca WITH 24+i, 2,  ;
             pro(i)
          DO coloca WITH 24+i, 16,  ;
             SUBSTR(dex(i), 1,  ;
             25)
          DO coloca WITH 24+i, 37,  ;
             STR(can(i), 5, 0)
          DO coloca WITH 24+i, 43,  ;
             IIF(wk_codmon='DOL ',  ;
             STR(pre(i), 13, 2),  ;
             STR(pres(i), 13,  ;
             2))
          DO coloca WITH 24+i, 56,  ;
             STR(sto(i), 5, 0)
          DO coloca WITH 24+i, 62,  ;
             IIF(wk_codmon='DOL ',  ;
             STR(tot(i), 13, 2),  ;
             STR(tots(i), 13,  ;
             2))
     ELSE
          DO coloca WITH 24+i, 2,  ;
             SPACE(73)
     ENDIF
ENDFOR
DO coloca WITH 39, 03,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totrep, s_totrep), 10, 2)
DO coloca WITH 39, 14,  ;
   STR(wk_totdes, 05, 2)
DO coloca WITH 39, 20,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totnet, s_totnet), 10, 2)
DO coloca WITH 39, 31,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totman, s_totman), 10, 2)
DO coloca WITH 39, 42,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totafe, s_totafe), 10, 2)
DO coloca WITH 39, 53,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totigv, s_totigv), 10, 2)
DO coloca WITH 39, 64,  ;
   STR(IIF(wk_codmon='DOL ',  ;
   wk_totgrl, s_totgrl), 10, 2)
RETURN
*
PROCEDURE col_bk2c
FOR i = 1 TO 8
     DO coloca WITH 41+i, 2,  ;
        wk_obspre(i)
ENDFOR
RETURN
*
PROCEDURE mueve3
PARAMETER key
FOR y = 1 TO LEN(key)
     key1 = ASC(SUBSTR(key, y,  ;
            1))
     DO CASE
          CASE key1 = 5 .AND. des >  ;
               1
               des = des - 1
          CASE key1 = 24 .AND.  ;
               des < 51 - lin
               des = des + 1
          CASE key1 = 18
               des = des - lin
               IF des < 1
                    des = 1
               ENDIF
          CASE key1 = 3
               des = des + lin
               IF des > 51 - lin
                    des = 51 -  ;
                          lin
               ENDIF
          CASE key1 = 1
               des = 1
          CASE key1 = 6
               des = 51 - lin
     ENDCASE
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
ENDFOR
RETURN
*
PROCEDURE informa
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW indicar
ACTIVATE WINDOW trabajo
@ 05, 07 FILL TO 41, 67
@ 04, 07 CLEAR TO 40, 66
@ 04, 07 TO 42, 66
@ 04, 12 SAY  ;
  'Centro de Informaci¢n al Cliente' +  ;
  SPACE(3) + STR(wk_numte1, 8) +  ;
  SPACE(1) + STR(wk_numte2, 8)  ;
  COLOR SCHEME 8
DO usedbf WITH 'st_iscic',  ;
   'fecsol'
SEEK STR(wk_numso, 8)
p = 06
@ p - 1, 10 SAY 'Fecha' +  ;
  SPACE(2) + 'FechaComp' +  ;
  SPACE(6) + 'Informe'
SCAN WHILE numsol = STR(wk_numso,  ;
     8) .AND.  .NOT. EOF()
     IF p >= 36
          p = 36
     ENDIF
     @ p, 08 SAY fecini
     @ p, 19 SAY feccom
     en1 = SUBSTR(inform, 1, 35)
     en2 = SUBSTR(inform, 36, 70)
     @ p, 30 SAY en1
     IF  .NOT. EMPTY(en2)
          @ p + 1, 30 SAY en2
          p = p + 2
     ELSE
          p = p + 1
     ENDIF
ENDSCAN
STORE SPACE(35) TO wk_obser1,  ;
      wk_obser2
STORE DATE() + 3 TO wk_fecco
STORE TIME() TO w_horini
@ p, 08 SAY DATE() COLOR 'n/w'
SET CURSOR ON
@ p, 19 GET wk_fecco
@ p, 30 GET wk_obser1
@ p + 1, 30 GET wk_obser2
READ
SET CURSOR OFF
IF  .NOT. EMPTY(wk_obser1)
     APPEND BLANK
     REPLACE numsol WITH  ;
             STR(wk_numso, 8),  ;
             numord WITH  ;
             STR(wk_numord, 8)
     REPLACE inform WITH  ;
             wk_obser1 +  ;
             wk_obser2, fecini  ;
             WITH DATE(), feccom  ;
             WITH wk_fecco,  ;
             horini WITH w_horini,  ;
             horfin WITH TIME(),  ;
             horcom WITH TIME()
     REPLACE user WITH users,  ;
             date WITH DATE(),  ;
             time WITH TIME()
ENDIF
RESTORE SCREEN FROM wk_pantax
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
IF config_prg == 1
     ind_prg = PROGRAM()
     tit_prg = 'CREACION'
ELSE
     ind_prg = '<PORM0302>'
     tit_prg = 'CONSULTA'
ENDIF
CLOSE DATABASES
SELECT 1
USE SHARED gc_pro00 ORDER codigo
SELECT 2
USE SHARED gc_alm00 ORDER codigo
SELECT 3
USE SHARED gc_dlp00 ORDER codigo
SELECT 4
USE SHARED ge_tab0 ORDER codigo
SELECT 5
USE SHARED st_iclpr ORDER codigo
SELECT 6
USE SHARED st_itecn ORDER codigo
SELECT 7
USE SHARED st_imode ORDER codigo
SELECT 8
USE SHARED st_isrep ORDER codigo
SELECT 9
USE SHARED st_iorep ORDER codigo
SELECT 10
USE SHARED st_sicli ORDER codigo
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA13
ON KEY LABEL F10 DO FCINCO
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
SET DISPLAY TO VGA50
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 42, 76
ZOOM WINDOW indicar NORM FROM 45,  ;
     0 TO 48, 76
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
@ 02, 1 SAY DATE()
STORE 0 TO k_numero, w_disponi
wk_print = .T.
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' PEDIDO DE REPUESTOS '
@ 07, 2 CLEAR TO 12, 72
@ 07, 2 TO 12, 72
@ 08, 5 SAY 'N£mero de Orden...:'
@ 09, 5 SAY 'Fecha de Pedido...:'
@ 10, 5 SAY 'C¢digo T‚cnico....:'
@ 11, 5 SAY 'C¢digo Almac‚n....:'
ppas = .T.
w_codent = SPACE(9)
DO WHILE ppas
     STORE 0 TO k_numero,  ;
           w_disponi, w_numped,  ;
           w_import, w_cosant,  ;
           w_cosanb, w_cosuni,  ;
           w_numord, w_codtec1,  ;
           wk_total, wk_rep,  ;
           w_valtab, w_newpre,  ;
           w_precio
     efecin = 1
     w_sigue = .F.
     @ 08, 25 CLEAR TO 11, 71
     @ 14, 02 CLEAR TO 38, 72
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'RAC',  ;
        'BBB', 'IGN', 'ESC'
     STORE SPACE(4) TO w_marca,  ;
           w_indori, w_impues,  ;
           w_linea
     STORE SPACE(8) TO w_nrosol,  ;
           w_codemi
     w_codmod = SPACE(15)
     w_numser = SPACE(20)
     w_codcli = SPACE(9)
     w_ubica = SPACE(10)
     w_fecing = DATE()
     w_codalm = empre6
     DIMENSION nota( 6)
     @ 08, 25 GET w_numord  ;
       PICTURE '99999999' VALID  ;
       numord2(w_numord) WHEN  ;
       colocaf6()
     @ 09, 25 SAY w_fecing
     @ 10, 25 GET w_codtec1  ;
       PICTURE '999999999' VALID  ;
       tecni(w_codtec1,35) WHEN  ;
       colocaf6()
     @ 11, 25 GET w_codalm  ;
       PICTURE '@!' VALID  ;
       val_tab('ALMA',w_codalm,35, ;
       30) WHEN colocaf6()
     SET CURSOR ON
     READ
     SET CURSOR OFF
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               ppas = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               LOOP
     ENDCASE
     w_numaux = STR(w_numord, 8)
     SELECT st_iorep
     SEEK '&w_numaux'
     w_indori = indori
     w_impues = SPACE(4)
     @ 16, 02 TO 18, 72
     @ 17, 03 SAY  ;
       ' C¢digo         Cantidad                         Precio    Ubicac.'
     @ 18, 02 TO 32, 72
     @ 18, 02 SAY 'Ã'
     @ 18, 72 SAY '´'
     DIMENSION pro( 12), can( 12),  ;
               dex( 12), uni( 12),  ;
               pre( 12), ubi( 12),  ;
               stk_a( 12)
     DIMENSION wk_obser( 05)
     FOR i = 1 TO 12
          pro( i) = SPACE(14)
          can( i) = 0
          pre( i) = 0
          dex( i) = SPACE(40)
          uni( i) = SPACE(3)
     ENDFOR
     FOR y = 1 TO 5
          wk_obser = SPACE(40)
     ENDFOR
     wk_total = 0
     wk_valida = .T.
     FOR i = 1 TO 12
          @ 18 + i, 03 GET pro(  ;
            i) PICTURE '@!' VALID  ;
            pro1(VAL(SUBSTR(VARREAD(),  ;
            5, 2))) .AND.  ;
            wk_valida WHEN  ;
            colocaf6() .AND.  ;
            wk_valida
          @ 18 + i, 18 GET can(  ;
            i) PICTURE '9999999'  ;
            VALID  ;
            canti(VAL(SUBSTR(VARREAD(),  ;
            5, 2))) WHEN  ;
            wk_valida
     ENDFOR
     SET CURSOR ON
     READ
     SET CURSOR OFF
     wk_rep = 0
     FOR i = 1 TO 12
          IF pro(i) <> SPACE(14)  ;
             .AND. can(i) <> 0
               wk_rep = wk_rep +  ;
                        1
               wk_total = wk_total +  ;
                          can(i)
          ENDIF
     ENDFOR
     IF LASTKEY() <> 27
          IF wk_rep = 0
               DO error2 WITH  ;
                  '** Debe Pedir al Menos Un Repuesto **'
               LOOP
          ENDIF
     ENDIF
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               LOOP
     ENDCASE
     DO esc_modo WITH 'C'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'IMP', 'IGN', 'ESC'
     keyx = 0
     DO WHILE keyx<>27 .AND. keyx<>- ;
        9 .AND. keyx<>-6
          keyx = INKEY(0)
     ENDDO
     si = .T.
     IF keyx == -6
          wk_print = PRINTSTATUS()
          DO WHILE  .NOT.  ;
             wk_print
               DO error2 WITH  ;
                  '** Error en Impresora. Continua ? (S/N) '
               IF CHR(LASTKEY()) ==  ;
                  'N' .OR.  ;
                  CHR(LASTKEY()) ==  ;
                  'n'
                    si = .F.
                    EXIT
               ENDIF
               wk_print = PRINTSTATUS()
          ENDDO
          wk_print = PRINTSTATUS()
          IF wk_print .AND. si =  ;
             .T.
               DO graba
               IF w_numped > 0
                    wk_aux = 'MARC' +  ;
                             w_marca
                    SELECT ge_tab0
                    SEEK '&wk_aux'
                    wk_desmar = SUBSTR(tab_destab,  ;
                                1,  ;
                                40)
                    wk_aux = 'ALMA' +  ;
                             w_codalm
                    SEEK '&wk_aux'
                    wk_alman = SUBSTR(tab_destab,  ;
                               1,  ;
                               30)
                    SEEK 'INGA' +  ;
                         w_indori
                    wrk_desgar = SUBSTR(tab_destab,  ;
                                 1,  ;
                                 20)
                    SEEK 'EMIS' +  ;
                         w_codemi
                    wrk_desemi = SUBSTR(tab_destab,  ;
                                 1,  ;
                                 20)
                    wk_aux = w_marca +  ;
                             w_codmod
                    wk_cliaux = 'C' +  ;
                                w_codcli
                    SELECT st_iclpr
                    SEEK wk_cliaux
                    wk_noment = noment
                    SELECT st_imode
                    SEEK '&wk_aux'
                    wk_desmod = SUBSTR(nommod,  ;
                                1,  ;
                                40)
                    SELECT st_itecn
                    SEEK STR(w_codtec1,  ;
                         9)
                    wk_nomtec = SUBSTR(noment,  ;
                                1,  ;
                                30)
                    wk_cote = codtec
                    SELECT 20
                    USE st_iredo  ;
                        ORDER  ;
                        ire_indnum
                    SEEK 'ORD ' +  ;
                         STR(w_numord,  ;
                         8) +  ;
                         'PED '
                    IF  .NOT.  ;
                        FOUND()
                         wk_soli =  ;
                          1
                    ELSE
                         wk_soli =  ;
                          0
                    ENDIF
                    SCAN WHILE  ;
                         indodo =  ;
                         'ORD '  ;
                         .AND.  ;
                         numodo =  ;
                         STR(w_numord,  ;
                         8) .AND.  ;
                         indddo =  ;
                         'PED '  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         wk_soli =  ;
                          wk_soli +  ;
                          1
                    ENDSCAN
                    SET CONSOLE OFF
                    SET PRINTER ON
                    SET PRINT TO &RGE_LPTPED
                    SET DEVICE TO PRINTER
                    @ PROW(),  ;
                      PCOL() SAY  ;
                      CHR(18)
                    @ PROW(),  ;
                      PCOL() SAY  ;
                      CHR(27) +  ;
                      'C' +  ;
                      CHR(33)
                    @ 01, 60 SAY  ;
                      'PED. N§:' +  ;
                      STR(w_numped,  ;
                      8)
                    @ 02, 32 SAY  ;
                      'PEDIDO  DE REPUESTO'
                    @ 02, 60 SAY  ;
                      'O/R  N§:' +  ;
                      STR(w_numord,  ;
                      8)
                    @ 03, 32 SAY  ;
                      'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
                    @ 03, 60 SAY  ;
                      'S/S  N§:' +  ;
                      w_nrosol
                    @ 04, 63 SAY  ;
                      DATE()
                    @ 05, 06 SAY  ;
                      SUBSTR(wk_noment,  ;
                      1, 30)
                    @ 05, 43 SAY  ;
                      w_codcli
                    @ 06, 06 SAY  ;
                      'TIPO GTIA.: ' +  ;
                      w_indori +  ;
                      '  ' +  ;
                      wrk_desgar
                    @ 07, 06 SAY  ;
                      'EMISOR    : ' +  ;
                      w_codemi +  ;
                      '  ' +  ;
                      wrk_desemi
                    @ 10, 00 SAY  ;
                      PADR(wk_desmar,  ;
                      13, ' ')
                    @ 10, 14 SAY  ;
                      PADR(w_codmod,  ;
                      15, ' ') +  ;
                      ' ' +  ;
                      PADR(wk_desmod,  ;
                      15, ' ')
                    @ 10, 45 SAY  ;
                      PADR(w_numser,  ;
                      16, ' ')
                    @ 10, 62 SAY  ;
                      'TOTAL PED.RPTO:' +  ;
                      STR(wk_soli,  ;
                      3)
                    l = 0
                    FOR i = 1 TO  ;
                        wk_rep
                         IF pro(i) <>  ;
                            SPACE(14)  ;
                            .AND.  ;
                            can(i) >  ;
                            0
                              l =  ;
                               l +  ;
                               1
                              @ 10 +  ;
                                l,  ;
                                0  ;
                                SAY  ;
                                pro(i)
                              @ 10 +  ;
                                l,  ;
                                15  ;
                                SAY  ;
                                SUBSTR(dex(i),  ;
                                1,  ;
                                20)
                              @ 10 +  ;
                                l,  ;
                                37  ;
                                SAY  ;
                                can(i)  ;
                                PICTURE  ;
                                '99,999'
                              @ 10 +  ;
                                l,  ;
                                46  ;
                                SAY  ;
                                ubi(i)
                              @ 10 +  ;
                                l,  ;
                                55  ;
                                SAY  ;
                                REPLICATE(CHR(95),  ;
                                8) +  ;
                                SPACE(1) +  ;
                                REPLICATE(CHR(95),  ;
                                14)
                         ENDIF
                    ENDFOR
                    @ 26, 00 SAY  ;
                      'TECNICO :' +  ;
                      wk_cote +  ;
                      ' ' +  ;
                      PADR(wk_nomtec,  ;
                      25, ' ')
                    @ 28, 00 SAY  ;
                      'ALMACEN :' +  ;
                      w_codalm +  ;
                      ' ' +  ;
                      PADR(wk_alman,  ;
                      25, ' ')
                    EJECT
                    SET PRINTER TO
                    SET PRINTER OFF
                    SET DEVICE TO SCREEN
                    SET CONSOLE ON
               ENDIF
          ENDIF
     ENDIF
     IF keyx == -9
          LOOP
     ENDIF
ENDDO
ON KEY LABEL F8
ON KEY LABEL F6
ON KEY LABEL F10
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DO sacawin
RELEASE wk_obser, esta, can
CLOSE DATABASES
RETURN
*
PROCEDURE ayuda13
ON KEY LABEL F6
IF VARREAD() == 'W_CODALM'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'ALMA'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE ALMACENES'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF VARREAD() == 'W_CODTEC1'
     SELECT st_itecn
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE TECNICOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
     SET FILTER TO
ENDIF
IF VARREAD() == 'W_NUMORD'
     SELECT st_iorep
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,12)+" "+subst(indest,1,2)+indori'
     wrk_origen = 'OR'
     DO ayuda4 WITH campoa,  ;
        wrk_origen
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF SUBSTR(VARREAD(), 1, 3) ==  ;
   'PRO'
     wrk_select = SELECT()
     SELECT gc_pro00
     wrk_selpro = SELECT()
     wrk_campo = pro_codpro
     DO b_prod WITH wrk_campo,  ;
        wrk_select, wrk_selpro
     IF LASTKEY() <> 27
          KEYBOARD wrk_campo
     ENDIF
     SELECT (wrk_select)
ENDIF
ON KEY LABEL F6 do ayuda13
ON KEY LABEL f8 do ve_orden
RETURN
*
FUNCTION pro1
PARAMETER ind
IF (LASTKEY() == 19 .OR.  ;
   LASTKEY() == 05) .AND.  ;
   (pro(ind) == SPACE(14))
     DO sacaf6
     RETURN .F.
ENDIF
IF ind < 12
     IF pro(ind) == SPACE(14)  ;
        .AND. pro(ind + 1) ==  ;
        SPACE(14)
          wk_valida = .T.
          CLEAR GETS
          RETURN .T.
     ENDIF
ENDIF
wk_lis = IIF(w_indori == 'S',  ;
         empre4, empre5)
wk_clave = IIF(w_indori == 'S',  ;
           empre4, empre5) +  ;
           pro(ind)
SELECT gc_dlp00
SEEK '&wk_clave'
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** C¢digo en Lista Precio NO EXISTE **'
     KEYBOARD '{CTRL+Y}{LEFTARROW}{HOME}'  ;
              PLAIN
     RETURN .F.
ENDIF
wk_prexx = dlp_prsigv
pre( ind) = dlp_prsigv
wk_clave = pro(ind) + w_codalm
SELECT gc_alm00
SEEK '&wk_clave'
IF  .NOT. FOUND()
     @ 18 + ind, 27 SAY SPACE(40)
     @ 18 + ind, 68 SAY SPACE(03)
     DO error2 WITH  ;
        '** No Existe C¢digo en Almac‚n **'
     KEYBOARD '{CTRL+Y}{LEFTARROW}{HOME}'
     can( ind) = 0
     w_disponi = 0
     RETURN .F.
ELSE
     w_disponi = alm_stkfis
ENDIF
w_ubica = alm_ubicac
FOR i = 1 TO 12
     IF (pro(ind) == pro(i))  ;
        .AND. (ind <> i)
          @ 18 + ind, 27 SAY  ;
            SPACE(40)
          @ 18 + ind, 68 SAY  ;
            SPACE(03)
          DO error2 WITH  ;
             '** Articulo Repetido en Pedido **'
          KEYBOARD '{CTRL+Y}{LEFTARROW}{HOME}'
          RETURN .F.
     ENDIF
ENDFOR
wk_clave = pro(ind)
SELECT gc_pro00
SET ORDER TO codigo
seek '&wk_clave'
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** C¢digo no existe en Ficha de Productos **'
     KEYBOARD '{CTRL+Y}{LEFTARROW}{HOME}'  ;
              PLAIN
     RETURN .F.
ENDIF
IF w_impues == SPACE(4)
     w_impues = pro_impigv
ELSE
     IF w_impues <> pro_impigv
          @ 18 + ind, 27 SAY  ;
            SPACE(40)
          @ 18 + ind, 68 SAY  ;
            SPACE(03)
          DO error2 WITH  ;
             '** Articulo con distinto IGV **'
          KEYBOARD '{CTRL+Y}{LEFTARROW}{HOME}'
          RETURN .F.
     ENDIF
ENDIF
dex( ind) = SUBSTR(pro_descri, 1,  ;
   40)
ubi( ind) = w_ubica
pre( ind) = wk_prexx
IF EMPTY(pro(ind)) .AND.  ;
   EMPTY(can(ind))
     @ 18 + ind, 27 SAY SPACE(40)
     @ 18 + ind, 50 SAY SPACE(10)
     @ 18 + ind, 64 SAY SPACE(08)
ELSE
     @ 18 + ind, 27 SAY dex(ind)
     @ 18 + ind, 50 SAY wk_prexx  ;
       PICTURE '999,999.99' COLOR  ;
       W+/N 
     @ 18 + ind, 64 SAY  ;
       SUBSTR(ubi(ind), 1, 8)
ENDIF
DO sacaf6
RETURN .T.
*
FUNCTION canti
PARAMETER ind
SELECT gc_alm00
SEEK pro(ind) + w_codalm
IF can(ind) > gc_alm00.alm_stkfis
     DO error2 WITH  ;
        '** Cantidad mayor al Stock en Almac‚n **'
     KEYBOARD '{CTRL+Y}{LEFTARROW}{HOME}'
     can( ind) = 0
     RETURN .F.
ENDIF
IF can(ind) = 0 .AND. ind <> 1  ;
   .AND. LASTKEY() = 5
     RETURN .T.
ENDIF
IF can(ind) = 0 .AND.  ;
   EMPTY(pro(ind))
     CLEAR READ
     @ 18 + ind, 27 SAY SPACE(40)
     @ 18 + ind, 50 SAY SPACE(10)
     @ 18 + ind, 64 SAY SPACE(08)
     RETURN .T.
ENDIF
IF can(ind) = 0 .AND.  .NOT.  ;
   EMPTY(pro(ind))
     DO error2 WITH  ;
        '** Cantidad en Cero **'
     KEYBOARD '{CTRL+Y}{LEFTARROW}{HOME}'
     DO sacaf6
     RETURN .F.
ENDIF
IF (LASTKEY() == 19 .OR.  ;
   LASTKEY() == 5) .AND.  ;
   (pro(ind) == SPACE(14))
     DO sacaf6
     RETURN .T.
ENDIF
IF can(ind) < 1
     DO error2 WITH  ;
        '** Cantidad Errada **'
     KEYBOARD '{CTRL+Y}{LEFTARROW}{HOME}'
     DO sacaf6
     RETURN .F.
ENDIF
@ 18 + ind, 50 SAY pre(ind) *  ;
  can(ind) PICTURE '999,999.99'
RETURN
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
SELECT st_iorep
SEEK  '&wk_clave'
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** N§ Orden Reparaci¢n NO EXISTE. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'N   '
     DO error2 WITH  ;
        '** N§ Orden Reparaci¢n esta ANULADA **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'F   ' .OR. indest ==  ;
   'B   '
     DO error2 WITH  ;
        '** N§ Orden Reparaci¢n FACTURADA. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'C   '
     DO error2 WITH  ;
        '** N§ Orden Reparaci¢n est  CERRADA **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
w_codtec1 = VAL(codtec)
w_codmod = codmod
w_marca = codmar
w_numser = numser
w_codcli = codent
w_nrosol = numsol
w_codent = codent
w_codemi = codemi
DO esc_indica WITH 1, 'AYU',  ;
   'rep', 'BBB', 'BBB'
ON KEY LABEL f8 do ve_orden
RETURN .T.
*
FUNCTION tecni
PARAMETER grupo, colu
SELECT st_itecn
codaux = STR(grupo, 9)
SEEK '&codaux'
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** C¢digo de T‚cnico NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
@ ROW(), colu SAY SUBSTR(noment,  ;
  1, 28)
RETURN .T.
*
PROCEDURE ve_orden
IF w_numord = 0
     RETURN
ENDIF
SAVE SCREEN TO pan9
DO mensa2 WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'COLO'
SELECT st_iorep
w_numaux = STR(w_numord, 8)
wk_numero2 = w_numaux
wk_auxi = codtec
SEEK '&w_numaux'
FOR i = 1 TO 6
     nota( i) = SUBSTR(observ, 1 +  ;
         ((i - 1) * 45), 45)
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
     wk_usar = VAL(numpre)
     wk_var = 2
     wk_cod001 = 'P'
ELSE
     wk_auxy = numsol
     wk_numero = VAL(numsol)
     k_numero = wk_numero
     wk_usar = VAL(numsol)
     wk_var = 1
     wk_cod001 = 'R'
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
           FREAD(file_handl, 77)
ENDFOR
= FCLOSE(file_handl)
DIMENSION wk_codsin( 15)
DIMENSION wk_acceso( 15)
DIMENSION wk_observ( 06)
w_numaux = STR(wk_numero, 8)
SELECT st_isrep
SEEK '&w_numaux'
wk_fecemi = fecemi
wk_feccom = feccom
wk_emisor = codemi
wk_codcli = VAL(codent)
w_indori = indori
wk_indest = indest
wk_codmar = codmar
w_codmod = codmod
w_numser = numser
wk_codstk = codstk
wk_numstk = VAL(numstk)
DIMENSION wk_codsin( 15),  ;
          wk_acceso( 15),  ;
          wk_observ( 6)
FOR i = 1 TO 15
     wk_codsin( i) = SPACE(35)
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
            11)
SELECT st_iclpr
SEEK '&wk_cliaux'
wk_noment = noment
wk_aux = wk_codmar + w_codmod
SELECT st_imode
SEEK '&wk_aux'
wk_aux = codcla
w_linea = linea
SELECT st_sicli
SEEK '&w_numaux'
i = 1
IF FOUND()
     DO WHILE  .NOT. EOF() .AND.  ;
        numdoc==w_numaux
          wk_aux2 = SUBSTR(codsin,  ;
                    2, 3)
          SELECT 20
          USE SHARED st_sint  ;
              ORDER sin_lincod
          SEEK w_linea + wk_aux2
          wk_codsin( i) =  ;
                   SUBSTR(dessin,  ;
                   1, 35)
          i = i + 1
          SELECT st_sicli
          SKIP
     ENDDO
ENDIF
DO mensa2 WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'SACA'
ACTIVATE WINDOW trabajo
DO col_or1b
DO col_or2b
DO col_or3b
FOR i = des2 TO (lin + des2 - 1)
     @ i - des2, 0 SAY  ;
       SUBSTR(solic2(i), com,  ;
       anc)
ENDFOR
DO esc_indica WITH 1, 'AYU',  ;
   'MBV', 'VOL', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
ppal2 = .T.
DO WHILE ppal2
     wk_inkey = 0
     DO WHILE  .NOT.  ;
        (STR(wk_inkey, 2)$ ;
        ' 5,24,18, 3, 1, 6,27,-4' ;
        )
          wk_inkey = INKEY(0)
     ENDDO
     DO CASE
          CASE wk_inkey == 27  ;
               .OR. wk_inkey == - ;
               4
               ppal2 = .F.
          CASE wk_inkey == 18
               DO mueve3a WITH  ;
                  CHR(wk_inkey)
          CASE wk_inkey == 3
               DO mueve3a WITH  ;
                  CHR(wk_inkey)
     ENDCASE
ENDDO
RESTORE SCREEN FROM pan9
DO esc_indica WITH 1, 'AYU',  ;
   'REP', 'BBB', 'BBB'
CLEAR TYPEAHEAD
RETURN
*
PROCEDURE ultimo
SELECT 20
USE SHARED st_iparg
DO rbloquea
REPLACE sys_numped WITH  ;
        (sys_numped + 1)
w_numped = sys_numped
UNLOCK
RETURN
*
PROCEDURE graba
pnum = 0
DO mensa2 WITH  ;
   '..Procesando. Espere un momento por favor..',  ;
   'COLO'
FOR i = 1 TO wk_rep
     SELECT ge_tab0
     SEEK 'MARC' + w_marca
     w_valtab = tab_factor
     SELECT gc_alm00
     SEEK pro(i) + w_codalm
     DO rbloquea
     IF can(i) > alm_stkfis
          pro( i) = SPACE(14)
          can( i) = 0
          UNLOCK
     ELSE
          stk_a( i) = alm_stkfis
          REPLACE alm_stkfis WITH  ;
                  alm_stkfis -  ;
                  can(i)
          UNLOCK
          SELECT 21
          USE SHARED gc_kar00  ;
              ORDER codigo
          APPEND BLANK
          DO rbloquea
          REPLACE kar_codpro WITH  ;
                  pro(i),  ;
                  kar_fecing WITH  ;
                  DATE()
          REPLACE kar_horing WITH  ;
                  TIME(),  ;
                  kar_tipdoc WITH  ;
                  'PED '
          UNLOCK
          pnum = pnum + 1
          IF pnum = 1
               DO ultimo
          ENDIF
          SELECT gc_kar00
          DO rbloquea
          REPLACE kar_codmon WITH  ;
                  'DOL ',  ;
                  kar_unimed WITH  ;
                  'UNID'
          REPLACE kar_stkant WITH  ;
                  stk_a(i),  ;
                  kar_cantid WITH  ;
                  can(i)
          REPLACE kar_fecdoc WITH  ;
                  DATE(),  ;
                  kar_almdes WITH  ;
                  w_codalm
          REPLACE kar_tipent WITH  ;
                  'C', kar_codmov  ;
                  WITH 'EPRD'
          REPLACE kar_codent WITH  ;
                  w_codent,  ;
                  kar_tidore WITH  ;
                  'ORDE'
          REPLACE kar_fecha WITH  ;
                  DATE(),  ;
                  kar_hora WITH  ;
                  TIME()
          REPLACE kar_nrdore WITH  ;
                  STR(w_numord,  ;
                  8), kar_usuari  ;
                  WITH users
          REPLACE kar_nrodoc WITH  ;
                  STR(w_numped,  ;
                  10)
          UNLOCK
          SELECT 20
          USE SHARED st_idped  ;
              ORDER codigo
          APPEND BLANK
          DO rbloquea
          REPLACE numdoc WITH  ;
                  STR(w_numped,  ;
                  8), numord WITH  ;
                  STR(w_numord,  ;
                  8)
          REPLACE codpro WITH  ;
                  pro(i), canpro  ;
                  WITH can(i)
          REPLACE valpro WITH  ;
                  pre(i), totite  ;
                  WITH valpro *  ;
                  canpro
          REPLACE user WITH users,  ;
                  date WITH  ;
                  DATE(), time  ;
                  WITH TIME()
          UNLOCK
          w_precio = pre(i)
          IF w_indori = 'GARA'  ;
             .OR. w_indori =  ;
             'GREC' .OR. w_indori =  ;
             'PVEN' .OR. w_indori =  ;
             'PREC'
               SELECT 22
               USE ST_ISERI ORDER  ;
                   SER_CODMAR
               SEEK w_marca +  ;
                    w_codmod +  ;
                    w_numser
               IF FOUND()
                    w_codpve = codent
                    IF w_codpve =  ;
                       '20372706288'  ;
                       .OR.  ;
                       w_codpve =  ;
                       '20467017129'  ;
                       .OR.  ;
                       w_codpve =  ;
                       '37270628'  ;
                       .OR.  ;
                       w_codpve =  ;
                       '46701712'
                         SELECT gc_pro00
                         SET ORDER TO;
codigo
                         SEEK pro(i)
                         IF FOUND()
                              IF pro_propie <>  ;
                                 'C'
                                   w_import = pro_coprmo
                                   SELECT st_idped
                                   DO rbloquea
                                   w_newpre = w_import * 1
                                   REPLACE valpro WITH w_newpre, totite WITH (w_newpre * canpro)
                                   UNLOCK
                                   w_precio = w_newpre
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
               SELECT st_iseri
               USE
          ENDIF
          SELECT gc_alm00
          SEEK pro(i) + w_codalm
          DO rbloquea
          REPLACE alm_fecha WITH  ;
                  DATE(),  ;
                  alm_hora WITH  ;
                  TIME(),  ;
                  alm_usuari WITH  ;
                  users
          UNLOCK
          SELECT gc_pro00
          SET ORDER TO codigo
          SEEK pro(i)
          IF FOUND()
               w_import = pro_coprmo
               w_cosant = pro_coprmo
               w_cosanb = pro_coprmb
               w_cosuni = pro_ulcomb
               DO rbloquea
               REPLACE pro_ultmov  ;
                       WITH  ;
                       DATE(),  ;
                       pro_ultven  ;
                       WITH  ;
                       DATE()
               UNLOCK
          ENDIF
          SELECT gc_kar00
          DO rbloquea
          REPLACE kar_cosant WITH  ;
                  w_cosant,  ;
                  kar_cosanb WITH  ;
                  w_cosanb
          REPLACE kar_cosuni WITH  ;
                  w_cosuni,  ;
                  kar_import WITH  ;
                  w_precio
          UNLOCK
     ENDIF
ENDFOR
IF w_numped = 0
     DO error2 WITH  ;
        '...No hay stock para repuesto...'
ELSE
     DO mensa2 WITH  ;
        'Grabando Pedido de Repuesto N§ '+ ;
        STR(w_numped, 8), 'COLO'
     SELECT 20
     USE SHARED st_iprep ORDER  ;
         codigo
     APPEND BLANK
     DO rbloquea
     REPLACE numdoc WITH  ;
             STR(w_numped, 8),  ;
             numord WITH  ;
             STR(w_numord, 8),  ;
             indest WITH 'V   '
     REPLACE fecemi WITH w_fecing,  ;
             codtec WITH  ;
             STR(w_codtec1, 9),  ;
             codalm WITH  ;
             w_codalm
     REPLACE user WITH users,  ;
             date WITH DATE(),  ;
             time WITH TIME()
     UNLOCK
     SELECT st_iorep
     w_numaux = STR(w_numord, 8)
     SEEK '&w_numaux'
     DO rbloquea
     REPLACE indest WITH 'P'
     UNLOCK
     SELECT 20
     USE SHARED st_iredo
     APPEND BLANK
     DO rbloquea
     REPLACE indodo WITH 'ORD',  ;
             numodo WITH  ;
             STR(w_numord, 8)
     REPLACE indddo WITH 'PED',  ;
             numddo WITH  ;
             STR(w_numped, 8)
     REPLACE user WITH users,  ;
             date WITH DATE(),  ;
             time WITH TIME()
     UNLOCK
     DO mensa2 WITH  ;
        'Grabando Pedido de Repuesto N§ '+ ;
        STR(w_numped, 8), 'SACA'
ENDIF
RETURN
*
FUNCTION val_tab
PARAMETER clave, codig, colu,  ;
          largo
SELECT ge_tab0
codaux = clave + codig
SEEK '&codaux'
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** Codigo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
ENDIF
RETURN .T.
*
PROCEDURE b_prod
PARAMETER wrk_campo, wrk_selec,  ;
          wrk_selpro
ON KEY
wrk_order = ORDER()
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen
DEFINE WINDOW produ FROM 15, 01  ;
       TO 24, 78 IN screen
DEFINE POPUP prod FROM 16, 31  ;
       COLOR SCHEME 8
DEFINE BAR 1 OF prod PROMPT  ;
       '\<C¢digo '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripcion '
DEFINE BAR 3 OF prod PROMPT  ;
       '\<Nro. de Parte '
DEFINE BAR 4 OF prod PROMPT  ;
       '\<Sub-Categoria '
ON SELECTION POPUP prod do b_prodx with;
bar(),wrk_selpro
DEFINE POPUP produ FROM 15, 18 TO  ;
       22, 73 PROMPT FIELDS  ;
       pro_codpro + '³' +  ;
       SUBSTR(pro_descri, 1, 20) +  ;
       '³' + SUBSTR(pro_modelo, 1,  ;
       20) IN screen COLOR SCHEME  ;
       8
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
ON KEY LABEL f6 do b_prod with wrk_campo,wrk_selec,wrk_selpro
IF LASTKEY() <> 27
     wrk_campo = gc_pro00.pro_codpro
ENDIF
SELECT (wrk_selec)
RETURN
*
PROCEDURE b_prodx
PARAMETER bar, wrk_selpro
ON KEY
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, -1
ENDFOR
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY -1, 0
ENDFOR
ACTIVATE WINDOW pide
SELECT (wrk_selpro)
IF bar = 1
     wrk_codpro = SPACE(14)
     wrk_indice = 'CODIGO'
     @ 00, 00 SAY 'C¢digo :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 2
     wrk_codpro = SPACE(40)
     wrk_indice = 'DESCRI'
     @ 00, 00 SAY 'Descr. :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 3
     wrk_codpro = SPACE(14)
     wrk_indice = 'NUMPAR'
     @ 00, 00 SAY 'N.Parte :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 4
     wrk_codpro = SPACE(4)
     wrk_indice = 'SUBCAT'
     @ 00, 00 SAY 'Sub-Cat.:'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
SELECT gc_alm00
SET ORDER TO CODIGO
SET FILTER TO alm_codalm = w_codalm
SELECT gc_pro00
SET ORDER TO WRK_INDICE
SET RELATION TO pro_codpro INTO gc_alm00
READ
IF LASTKEY() <> 27
     SET NEAR ON
     SEEK wrk_codpro
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do b_tomap
     BROWSE FIELDS  ;
            gc_pro00.pro_codpro  ;
            :R : 14 :H =  ;
            'C¢d. Produc.',  ;
            gc_pro00.pro_descri  ;
            :R : 27 :H =  ;
            'Descripci¢n',  ;
            gc_alm00.alm_stkfis  ;
            :R : 7 :H = 'Stock'  ;
            :P = '999,999',  ;
            gc_pro00.pro_modelo  ;
            :R : 10 :H = 'Modelo',  ;
            gc_pro00.pro_codree  ;
            :R : 14 :H =  ;
            'Reemplazo' FREEZE  ;
            pro_codpro IN produ
ENDIF
ON KEY
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
SET NEAR OFF
RETURN
*
PROCEDURE b_tomap
ON KEY
wrk_campo = gc_pro00.pro_codpro
DEACTIVATE WINDOW pide, produ
DEACTIVATE POPUP prod
RETURN
*
PROCEDURE col_or1b
SELECT ge_tab0
wk_aux = 'ESTA' + wk_indest
seek '&wk_aux'
wk_estado = tab_destab
wk_aux = 'ESOR' + wk_esor
seek '&wk_aux'
wk_desesor = tab_destab
DO coloca2 WITH 01, 66,  ;
   wk_numero2
DO coloca2 WITH 05, 20, wk_codmar
DO coloca2 WITH 06, 20, w_codmod
DO coloca2 WITH 07, 20, w_numser
DO coloca2 WITH 08, 25,  ;
   STR(wk_numstk, 9)
wk_aux = 'MARC' + wk_codmar
SELECT ge_tab0
seek '&wk_aux'
DO coloca2 WITH 05, 25,  ;
   SUBSTR(tab_destab, 1, 30)
wk_aux = wk_codmar + w_codmod
SELECT st_imode
SEEK '&wk_aux'
DO coloca2 WITH 06, 36,  ;
   SUBSTR(nommod, 1, 30)
SELECT st_itecn
SEEK wk_codtec
wk_destec = noment
DO coloca2 WITH 10, 20,  ;
   STR(wk_codcli, 11)
DO coloca2 WITH 10, 30, wk_noment
DO coloca2 WITH 12, 20,  ;
   DTOC(wk_fecemi)
DO coloca2 WITH 12, 65,  ;
   DTOC(wk_feccom)
DO coloca2 WITH 14, 20, wk_cod001+ ;
   ' '+IIF(wk_cod001=='R',  ;
   'REPARACION ', 'PRESUPUESTO')
DO coloca2 WITH 15, 20,  ;
   SUBSTR(wk_destec, 1, 40)
DO coloca2 WITH 16, 20, wk_esor
DO coloca2 WITH 16, 25,  ;
   SUBSTR(wk_desesor, 1, 40)
FOR i = 1 TO 15
     DO coloca2 WITH 19+i, 2,  ;
        wk_codsin(i)
ENDFOR
FOR i = 1 TO 15
     DO coloca2 WITH 19+i, 38,  ;
        wk_acceso(i)
ENDFOR
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
IF config_prg == 1
     tit_prg = 'CIERRE'
ELSE
     tit_prg = 'REINGRESO'
ENDIF
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER codigo
SELECT 2
USE SHARED st_itecn ORDER codigo
SELECT 3
USE SHARED st_isrep ORDER codigo
SELECT 4
USE SHARED st_idped ORDER codigo
SELECT 5
USE SHARED st_iprep ORDER  ;
    rep_numord
SELECT 6
USE SHARED st_estad ORDER  ;
    est_numord
SELECT 7
USE SHARED st_imode ORDER codigo
SELECT 8
USE SHARED st_mobra ORDER codigo
SELECT 9
USE SHARED st_iclpr ORDER codigo
SELECT 10
USE SHARED st_sint ORDER  ;
    sin_lincod
SELECT 11
USE SHARED st_sicli ORDER codigo
SELECT 12
USE SHARED ge_tab0 ORDER codigo
SELECT 13
USE SHARED st_mvord ORDER  ;
    eor_nroord
wrk_progra = PROGRAM()
DO crea_win
STORE SPACE(4) TO w_tipgar,  ;
      w_codart, w_estado,  ;
      w_estant
DIMENSION w_nota( 6)
STORE 0 TO w_facigv, w_igv
ON KEY LABEL f6 do ayuda15
ON KEY LABEL f10 do fcinco
w_igv = facigv()
ppas = .T.
IF w_igv = 0
     DO error WITH  ;
        '***Tipo de Impuesto no existe en Tablas ***'
     ppas = .F.
ENDIF
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
IF config_prg == 1
     DO saycenter WITH 2,  ;
        ' CIERRE DE ORDEN DE REPARACION'
ELSE
     DO saycenter WITH 2,  ;
        ' REINGRESO DE CIERRE DE ORDEN DE REPARACION'
ENDIF
@ 03, 02 CLEAR TO 10, 77
@ 03, 02 TO 14, 75
@ 04, 05 SAY  ;
  'N�mero de Orden   :'
@ 05, 05 SAY  ;
  'Fecha C. Calidad  :'
@ 06, 05 SAY  ;
  'Mano Obra  (US $) :'
@ 07, 05 SAY  ;
  'Informe T�cnico   :'
DO WHILE ppas
     @ 04, 25 CLEAR TO 13, 70
     @ 09, 05 CLEAR TO 14, 24
     @ 03, 02 TO 14, 75
     STORE SPACE(38) TO w_nota(  ;
           1), w_nota( 2),  ;
           w_nota( 3), w_nota( 4),  ;
           w_nota( 5), w_nota(  ;
           6)
     STORE SPACE(8) TO w_numped
     STORE 0 TO cuenta, w_rep,  ;
           sum, w_numord,  ;
           w_monman, w_mobra,  ;
           w_flete
     STORE 'N' TO w_opc
     STORE SPACE(4) TO w_indori,  ;
           w_codmar
     STORE SPACE(9) TO w_codtec1
     w_codmod = SPACE(15)
     w_numser = SPACE(20)
     w_fecemi = CTOD('  /  /  ')
     w_feccom = CTOD('  /  /  ')
     efecin = 1
     IF config_prg == 1
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'INT'
          DO esc_indica WITH 2,  ;
             'RAC', 'BBB', 'IGN',  ;
             'ESC'
          @ 05, 25 SAY  ;
            DTOC(DATE())
          @ 04, 25 GET w_numord  ;
            PICTURE '99999999'  ;
            VALID  ;
            numord2(w_numord)  ;
            WHEN colocaf6()
          @ 06, 25 GET w_monman  ;
            PICTURE '999,999.99'  ;
            VALID mano2(w_monman)  ;
            WHEN  ;
            manobra(w_mobra)
          FOR i = 1 TO 6
               @ 06 + i, 25 GET  ;
                 w_nota( i)  ;
                 PICTURE '@!'
          ENDFOR
     ELSE
          DO esc_modo WITH 'E'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'IGN',  ;
             'ESC'
          w_numord = 0
          @ 04, 25 GET w_numord  ;
            PICTURE '99999999'  ;
            VALID  ;
            numord2(w_numord)  ;
            WHEN colocaf6()
     ENDIF
     SET CURSOR ON
     READ
     SET CURSOR OFF
     IF LASTKEY() == 27
          ppas = .F.
          LOOP
     ENDIF
     IF config_prg == 2
          descu = 0
          w_numaux = STR(w_numord,  ;
                     8)
          SELECT st_iorep
          seek '&w_numaux'
          w_codtec1 = codcca
          @ 05, 25 SAY  ;
            DTOC(fecfin)
          @ 06, 25 SAY cosmob  ;
            PICTURE '9999999.99'
          IF (w_estado = '010 '  ;
             .OR. w_estado =  ;
             '018 ' .OR. w_estado =  ;
             '021 ' .OR. w_estado =  ;
             '026 ' .OR. w_estado =  ;
             '027 ') .AND.  ;
             (SUBSTR(users, 1, 2) =  ;
             'CC' .OR.  ;
             SUBSTR(users, 1, 3) =  ;
             'COM') .OR. nivell =  ;
             'A7'
               @ 13, 20 SAY  ;
                 'Desea borrar estado de cierre de O/R [ ]'
               @ 13, 58 GET w_opc  ;
                 PICTURE  ;
                 '@m N,S'
               SET CURSOR ON
               READ
               SET CURSOR OFF
               IF LASTKEY() = 27  ;
                  .OR. LASTKEY() = - ;
                  9
                    LOOP
               ENDIF
          ELSE
               w_opc = 'N'
          ENDIF
          IF w_opc = 'N'
               IF nivell = 'A2  '  ;
                  .OR. nivell =  ;
                  'A4  ' .OR.  ;
                  nivell = 'A7  '  ;
                  .OR. nivell =  ;
                  'A11 '
                    @ 13, 05  ;
                      CLEAR TO 13,  ;
                      70
                    @ 06, 25 GET  ;
                      w_monman  ;
                      PICTURE  ;
                      '999,999.99'  ;
                      VALID  ;
                      mano2(w_monman)  ;
                      WHEN  ;
                      manobra(w_mobra)
                    FOR i = 1 TO  ;
                        6
                         @ 06 + i,  ;
                           25 GET  ;
                           w_nota(  ;
                           i)  ;
                           PICTURE  ;
                           '@!'
                    ENDFOR
                    SET CURSOR ON
                    READ
                    SET CURSOR OFF
               ENDIF
          ENDIF
     ENDIF
     DO esc_modo WITH 'C'
     DO esc_indica WITH 1, 'AYU',  ;
        'REP', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'GRA',  ;
        'BBB', 'IGN', 'ESC'
     keyx = 0
     DO WHILE keyx<>27 .AND. keyx<>- ;
        9 .AND. keyx<>-1
          keyx = INKEY(0)
          IF keyx = -7
               DO ve_orden9
          ENDIF
     ENDDO
     IF keyx == -9
          LOOP
     ENDIF
     IF keyx == 27
          ppas = .F.
          LOOP
     ENDIF
     IF keyx == -1 .AND.  ;
        (config_prg == 1 .OR.  ;
        (config_prg == 2 .AND.  ;
        w_opc = 'N'))
          DO mensa WITH  ;
             '*** Espere un momento, por favor ***',  ;
             'COLO'
          w_numaux = STR(w_numord,  ;
                     8)
          w_aux = ''
          FOR i = 1 TO 6
               IF LEN(TRIM(w_nota(i))) <>  ;
                  0
                    w_aux = w_aux +  ;
                            w_nota(i)
               ENDIF
          ENDFOR
          SELECT st_isrep
          SEEK st_iorep.numsol
          IF FOUND()
               DO rbloquea
               REPLACE indest  ;
                       WITH 'C'
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
               w_feccom = feccom
          ENDIF
          SELECT st_iprep
          SEEK w_numaux
          SCAN WHILE numord =  ;
               w_numaux .AND.   ;
               .NOT. EOF()
               IF indest <> 'N'
                    w_numped = numdoc
                    SELECT st_idped
                    SEEK w_numped +  ;
                         w_numaux
                    IF FOUND()
                         SCAN WHILE  ;
                              w_numped =  ;
                              numdoc  ;
                              .AND.  ;
                              numord =  ;
                              w_numaux  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              IF canpro >  ;
                                 0
                                   w_rep = w_rep + totite
                              ENDIF
                         ENDSCAN
                    ENDIF
                    SELECT st_iprep
                    DO rbloquea
                    REPLACE indest  ;
                            WITH  ;
                            'C'
                    REPLACE user  ;
                            WITH  ;
                            users
                    REPLACE date  ;
                            WITH  ;
                            DATE()
                    REPLACE time  ;
                            WITH  ;
                            TIME()
                    UNLOCK
               ENDIF
          ENDSCAN
          subto = w_rep +  ;
                  w_monman +  ;
                  w_flete
          totneto = subto
          totigva = ROUND(totneto *  ;
                    w_igv, 2)
          totbrut = totneto +  ;
                    totigva
          SELECT st_iorep
          SEEK w_numaux
          DO rbloquea
          REPLACE indest WITH  ;
                  'C   '
          REPLACE cosmob WITH  ;
                  w_monman
          REPLACE subtot WITH  ;
                  subto
          REPLACE totnet WITH  ;
                  totneto, totigv  ;
                  WITH totigva
          REPLACE totbru WITH  ;
                  totbrut, cosrep  ;
                  WITH w_rep
          REPLACE observ WITH  ;
                  w_aux
          REPLACE user WITH users
          REPLACE date WITH  ;
                  DATE()
          REPLACE time WITH  ;
                  TIME()
          UNLOCK
          w_indori = indori
          w_codmod = codmod
          w_codmar = codmar
          w_numser = numser
          w_fecrep = fecemi
          w_fecdes = DATE()
          w_codtec1 = codtec
          w_cosmob = cosmob
          w_flete = flete
          w_solici = numsol
          w_titore = DATE() -  ;
                     w_fecrep
          w_ano = VAL(SUBSTR(STR(YEAR(DATE()),  ;
                  4), 3, 2))
          w_mes = VAL(SUBSTR(STR(MONTH(DATE()),  ;
                  4), 3, 2))
          SELECT st_estad
          APPEND BLANK
          DO rbloquea
          REPLACE indori WITH  ;
                  w_indori,  ;
                  anorep WITH  ;
                  w_ano
          REPLACE mesrep WITH  ;
                  w_mes, numord  ;
                  WITH w_numord
          REPLACE codmod WITH  ;
                  w_codmod,  ;
                  codmar WITH  ;
                  w_codmar
          REPLACE numser WITH  ;
                  w_numser,  ;
                  fecrep WITH  ;
                  w_fecrep
          REPLACE fecdes WITH  ;
                  DATE(), feccon  ;
                  WITH w_feccom
          REPLACE codtec WITH  ;
                  w_codtec1,  ;
                  valmao WITH  ;
                  w_cosmob
          REPLACE titore WITH  ;
                  w_titore,  ;
                  valrep WITH  ;
                  w_rep
          REPLACE user WITH users
          REPLACE date WITH  ;
                  DATE()
          REPLACE time WITH  ;
                  TIME()
          UNLOCK
     ENDIF
     IF keyx == -1 .AND.  ;
        config_prg == 2 .AND.  ;
        w_opc = 'S'
          w_numaux = STR(w_numord,  ;
                     8)
          DO mensa WITH  ;
             '*** Ir a cambio de estado para cerrar la O/R ***',  ;
             'COLO'
          w_estant = w_estado
          SELECT st_mvord
          SEEK w_numaux
          SCAN WHILE orden =  ;
               w_numaux .AND.   ;
               .NOT. EOF()
               IF w_estant =  ;
                  estado
                    DELETE
               ELSE
                    w_estado = estado
               ENDIF
          ENDSCAN
          SELECT st_iorep
          SEEK w_numaux
          IF FOUND()
               DO rbloquea
               REPLACE indest  ;
                       WITH  ;
                       'P   '
               REPLACE cosrep  ;
                       WITH 0,  ;
                       subtot  ;
                       WITH flete,  ;
                       totdes  ;
                       WITH 0,  ;
                       totnet  ;
                       WITH  ;
                       subtot,  ;
                       totigv  ;
                       WITH  ;
                       ROUND(subtot *  ;
                       w_igv, 2),  ;
                       totbru  ;
                       WITH  ;
                       totnet +  ;
                       totigv
               REPLACE auxest  ;
                       WITH  ;
                       w_estado
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          SELECT st_isrep
          SEEK st_iorep.numsol
          IF FOUND()
               DO rbloquea
               REPLACE indest  ;
                       WITH  ;
                       'P   '
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          SELECT st_iprep
          SEEK w_numaux
          IF FOUND()
               SCAN WHILE  ;
                    STR(w_numord,  ;
                    8) = numord  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF indest <>  ;
                       'N'
                         DO rbloquea
                         REPLACE indest  ;
                                 WITH  ;
                                 'V'
                         REPLACE user  ;
                                 WITH  ;
                                 users
                         REPLACE date  ;
                                 WITH  ;
                                 DATE()
                         REPLACE time  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                    ENDIF
               ENDSCAN
          ENDIF
          SELECT st_estad
          SEEK w_numaux
          IF FOUND()
               DO rbloquea
               DELETE
               UNLOCK
          ENDIF
          DO mensa WITH  ;
             '*** Ir a cambio de estado para cerrar la O/R ***',  ;
             'SACA'
     ENDIF
     DO mensa WITH  ;
        '*** Espere un momento, por favor ***',  ;
        'SACA'
ENDDO
CLOSE DATABASES
DO sacawin
ON KEY LABEL f6
ON KEY LABEL f10
RETURN
*
PROCEDURE ayuda15
ON KEY LABEL F6
IF VARREAD() == 'W_NUMORD'
     SELECT st_iorep
     w_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)+INDORI'
     DO ayuda4 WITH campoa,  ;
        w_origen
     SET ORDER TO codigo
ENDIF
ON KEY LABEL F6 do ayuda15
RETURN
*
FUNCTION numord2
PARAMETER num
IF num == 0
     DO error WITH  ;
        '** Error N� debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
w_clave = STR(num, 8)
SELECT st_iorep
seek '&w_clave'
IF  .NOT. FOUND()
     DO error WITH  ;
        '** N� Orden Reparaci�n No Existe **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'N   '
     DO error WITH  ;
        '** N� Orden Reparaci�n Esta Anulada **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'C   ' .AND.  ;
   config_prg == 1
     DO error WITH  ;
        '** N� Orden Reparaci�n esta Cerrada **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'F   ' .OR. indest ==  ;
   'B   '
     DO error WITH  ;
        '** N� Orden Reparaci�n esta Facturada **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF (auxest < '010' .OR. (auxest >  ;
   '010' .AND. auxest < '020')  ;
   .OR. auxest = '031 ') .AND.  ;
   config_prg == 1
     DO error WITH  ;
        '** N� O/R en Proceso, Ir a Cambio de Estado **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest <> 'C   ' .AND.  ;
   config_prg == 2
     DO error WITH  ;
        '** N� Orden Reparaci�n No Esta Cerrada **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
w_codmar = codmar
w_codmod = codmod
w_tipgar = indori
w_serie = numser
w_estado = auxest
w_codent = codent
w_solici = numsol
w_mobra = cosmob
w_monman = cosmob
w_flete = flete
FOR i = 1 TO 6
     w_nota( i) = SUBSTR(observ,  ;
           1 + ((i - 1) * 38),  ;
           38)
     w_nota( i) = w_nota(i) +  ;
           SPACE(38 -  ;
           LEN(w_nota(i)))
ENDFOR
@ 04, 36 SAY 'Cliente :' COLOR N+/ ;
  W 
@ 04, 46 SAY  ;
  SUBSTR(oodescli(w_codent), 1,  ;
  25) COLOR W+/N 
@ 05, 36 SAY 'S/Servicio :' COLOR  ;
  N+/W 
@ 05, 46 SAY w_solici COLOR W+/N 
@ 06, 25 SAY cosmob PICTURE  ;
  '999,999.99'
FOR i = 1 TO 6
     @ 06 + i, 25 SAY w_nota(i)  ;
       PICTURE '@!'
ENDFOR
@ 09, 05 SAY 'Modelo :' COLOR N+/ ;
  W 
@ 10, 05 SAY SUBSTR(w_codmod, 1,  ;
  18)
@ 11, 05 SAY 'Serie :' COLOR N+/W 
@ 12, 05 SAY SUBSTR(w_serie, 1,  ;
  18)
SELECT st_imode
SEEK w_codmar + w_codmod
IF FOUND()
     w_codart = codcla
ELSE
     w_codart = SPACE(4)
ENDIF
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
RETURN .T.
*
PROCEDURE manobra
PARAMETER w_mobra
IF w_mobra = 0
     SELECT st_mobra
     SEEK w_codmar + w_codart
     IF FOUND()
          IF w_tipgar = 'GARA'  ;
             .OR. w_tipgar =  ;
             'GREC'
               w_monman = mo_monmog
          ELSE
               w_monman = mo_monmog
          ENDIF
     ELSE
          w_monman = 0
     ENDIF
ELSE
     w_monman = w_mobra
ENDIF
RETURN
*
FUNCTION mano2
PARAMETER w_mobra
IF w_mobra < 0
     DO error WITH  ;
        '** Monto no puede ser menor de cero **'
     RETURN .F.
ENDIF
IF SUBSTR(st_iorep.indori, 2, 1) =  ;
   'R' .AND. w_mobra > 0
     DO error WITH  ;
        '** Reclamo no debe tener Mano de Obra **'
     RETURN .F.
ENDIF
IF SUBSTR(st_iorep.indori, 2, 1) <>  ;
   'R' .AND. w_mobra = 0 .AND.  ;
   st_iorep.codtec <>  ;
   '      500'
     DO error WITH  ;
        '** Falta Monto de Mano de Obra **'
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE ve_orden9
SAVE SCREEN TO panta11
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'COLO'
SELECT st_iorep
SEEK STR(w_numord, 8)
w_numaux = STR(w_numord, 8)
w_numero2 = w_numaux
w_auxi = codtec
nota1 = SUBSTR(observ, 1, 45)
nota2 = SUBSTR(observ, 46, 45)
nota3 = SUBSTR(observ, 91, 45)
nota4 = SUBSTR(observ, 136, 45)
nota5 = SUBSTR(observ, 181, 45)
nota6 = SUBSTR(observ, 226, 45)
w_esor = auxest
w_codtec1 = codtec
w_solici = numsol
w_cod001 = 'R'
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
DIMENSION w_codsin( 15)
DIMENSION w_acceso( 15)
DIMENSION w_observ( 06)
SELECT st_isrep
SEEK w_solici
w_fecemi = fecemi
w_feccom = feccom
w_emisor = codemi
w_codcli = VAL(codent)
w_indori = indori
w_indest = indest
w_codmar = codmar
w_codmod = codmod
w_numser = numser
w_codstk = codstk
w_numstk = VAL(numstk)
FOR i = 1 TO 15
     w_codsin( i) = SPACE(35)
     w_acceso( i) =  ;
             SUBSTR(MLINE(desace,  ;
             i), 1, 35)
     w_acceso( i) = w_acceso(i) +  ;
             SPACE(35 -  ;
             LEN(w_acceso(i)))
     IF i <= 6
          w_observ( i) =  ;
                  SUBSTR(MLINE(observ,  ;
                  i), 1, 45)
          w_observ( i) =  ;
                  w_observ(i) +  ;
                  SPACE(45 -  ;
                  LEN(w_observ(i)))
     ENDIF
ENDFOR
w_cliaux = 'C' + STR(w_codcli,  ;
           11)
SELECT st_iclpr
seek '&w_cliaux'
w_noment = noment
w_aux = w_codmar + w_codmod
SELECT st_imode
seek '&w_aux'
w_aux = codcla
w_linea = linea
SELECT st_sicli
SEEK w_solici
i = 1
IF FOUND()
     SCAN WHILE  .NOT. EOF()  ;
          .AND. numdoc =  ;
          w_solici
          wk_aux = SUBSTR(codsin,  ;
                   2, 3)
          SELECT st_sint
          SEEK w_linea + wk_aux
          w_codsin( i) =  ;
                  SUBSTR(dessin,  ;
                  1, 35)
          i = i + 1
          SELECT st_sicli
     ENDSCAN
ENDIF
ACTIVATE SCREEN
@ 24, 69 SAY SPACE(11)
ACTIVATE WINDOW trabajo
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'SACA'
SET DISPLAY TO VGA50
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 42, 76
ZOOM WINDOW indicar NORM FROM 45,  ;
     0 TO 48, 76
ACTIVATE SCREEN
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
     w_inkey = 0
     DO WHILE  .NOT. (STR(w_inkey,  ;
        2)$ ;
        ' 5,24,18, 3, 1, 6,27,-4' ;
        )
          w_inkey = INKEY(0)
     ENDDO
     DO CASE
          CASE w_inkey == -4 .OR.  ;
               LASTKEY() = 27
               ppal2 = .F.
          OTHERWISE
               DO mueve3a WITH  ;
                  CHR(w_inkey)
     ENDCASE
ENDDO
SET DISPLAY TO VGA25
ACTIVATE SCREEN
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
ACTIVATE WINDOW trabajo
RESTORE SCREEN FROM panta11
DO esc_indica WITH 1, 'AYU',  ;
   'REP', 'BBB', 'BBB'
DO esc_indica WITH 2, 'GRA',  ;
   'BBB', 'IGN', 'ESC'
RETURN
*
FUNCTION facigv
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF  .NOT. FOUND()
     RETURN 0
ELSE
     w_facigv = (tab_factor /  ;
                100)
ENDIF
RETURN w_facigv
*
FUNCTION oodescli
PARAMETER ccodcli
narea = SELECT()
SELECT st_iclpr
SEEK 'C' + ccodcli
IF FOUND()
     w_descli = st_iclpr.noment
ELSE
     w_descli = SPACE(30)
ENDIF
SELECT (narea)
RETURN w_descli
*
PROCEDURE col_or1b
SELECT ge_tab0
w_aux = 'ESTA' + w_indest
seek '&w_aux'
w_estado = tab_destab
w_aux = 'ESOR' + w_esor
seek '&w_aux'
w_desesor = tab_destab
DO coloca2 WITH 01, 66, w_numero2
DO coloca2 WITH 05, 20, w_codmar
DO coloca2 WITH 06, 20, w_codmod
DO coloca2 WITH 07, 20, w_numser
DO coloca2 WITH 08, 25,  ;
   STR(w_numstk, 9)
SELECT ge_tab0
w_aux = 'MARC' + w_codmar
seek '&w_aux'
DO coloca2 WITH 05, 25,  ;
   SUBSTR(tab_destab, 1, 30)
w_aux = w_codmar + w_codmod
SELECT st_imode
seek '&w_aux'
DO coloca2 WITH 06, 36,  ;
   SUBSTR(nommod, 1, 30)
SELECT st_itecn
SEEK w_codtec1
w_destec = noment
DO coloca2 WITH 10, 20,  ;
   STR(w_codcli, 11)
DO coloca2 WITH 10, 30, w_noment
DO coloca2 WITH 12, 20,  ;
   DTOC(w_fecemi)
DO coloca2 WITH 12, 65,  ;
   DTOC(w_feccom)
DO coloca2 WITH 14, 20, w_cod001+ ;
   ' '+IIF(w_cod001=='R',  ;
   'REPARACION ', 'PRESUPUESTO')
DO coloca2 WITH 15, 20,  ;
   SUBSTR(w_destec, 1, 40)
DO coloca2 WITH 16, 20, w_esor
DO coloca2 WITH 16, 25,  ;
   SUBSTR(w_desesor, 1, 40)
FOR i = 1 TO 15
     DO coloca2 WITH 19+i, 2,  ;
        w_codsin(i)
ENDFOR
FOR i = 1 TO 15
     DO coloca2 WITH 19+i, 38,  ;
        w_acceso(i)
ENDFOR
RETURN
*
PROCEDURE col_or2b
FOR i = 1 TO 06
     DO coloca2 WITH 36+i, 2,  ;
        w_observ(i)
ENDFOR
RETURN
*
PROCEDURE col_or3b
DO coloca2 WITH 45, 2, nota1
DO coloca2 WITH 46, 2, nota2
DO coloca2 WITH 47, 2, nota3
DO coloca2 WITH 48, 2, nota4
DO coloca2 WITH 49, 2, nota5
DO coloca2 WITH 50, 2, nota6
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

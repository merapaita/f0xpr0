*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'MANTENCION'
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER  ;
    ord_numsol
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
USE SHARED st_imode ORDER codigo
SELECT 7
USE SHARED st_iclpr ORDER codigo
SELECT 8
USE SHARED st_sint ORDER  ;
    sin_lincod
SELECT 9
USE SHARED st_sicli ORDER codigo
SELECT 10
USE SHARED ge_tab0 ORDER codigo
wrk_progra = PROGRAM()
DO crea_win
STORE SPACE(4) TO w_tipgar,  ;
      w_codart
DIMENSION w_nota( 6)
STORE 0 TO w_facigv
ON KEY LABEL F6 do ayuda15
ON KEY LABEL F10 do fcinco
w_facigv = facigv()
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' CIERRE DE O/R(PRESUPUESTO NO APROBADO)'
@ 03, 02 CLEAR TO 10, 77
@ 03, 02 TO 14, 75
@ 04, 05 SAY  ;
  'N£mero Solicitud  :'
@ 05, 05 SAY  ;
  'Fecha Emisi¢n O/R :'
@ 06,05 say "Mano Obra  (&empre13)  :"
@ 07, 05 SAY  ;
  'Informe T‚cnico   :'
ppas = .T.
DO WHILE ppas
     @ 04, 25 CLEAR TO 13, 70
     @ 09, 05 CLEAR TO 13, 24
     STORE SPACE(38) TO w_nota(  ;
           1), w_nota( 2),  ;
           w_nota( 3), w_nota( 4),  ;
           w_nota( 5), w_nota(  ;
           6)
     STORE SPACE(8) TO w_numped,  ;
           w_numaux, w_numord
     STORE 0 TO w_rep, w_monman,  ;
           w_cosmob, w_flete,  ;
           tot_titore, w_cuenta,  ;
           w_ctasig, w_solici,  ;
           w_numero
     STORE SPACE(4) TO w_indori,  ;
           w_codmar, w_est,  ;
           w_codtall
     STORE SPACE(9) TO w_codtec1
     w_codmod = SPACE(15)
     w_numser = SPACE(20)
     STORE SPACE(30) TO w_esorde
     STORE CTOD('  /  /    ') TO  ;
           w_titore, w_fecemi
     efecin = 1
     DO esc_modo WITH 'M'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     @ 4, 25 GET w_numero PICTURE  ;
       '99999999' VALID  ;
       numord2(w_numero) WHEN  ;
       colocaf6()
     SET CURSOR ON
     READ
     SET CURSOR OFF
     IF LASTKEY() == 27
          ppas = .F.
          LOOP
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
     IF keyx == -1
          DO mensa WITH  ;
             '*** Espere un momento, por favor ***',  ;
             'COLO'
          w_numaux = w_numord
          SELECT st_iorep
          SEEK w_solici
          IF FOUND()
               DO rbloquea
               REPLACE fecfin  ;
                       WITH  ;
                       DATE(),  ;
                       indest  ;
                       WITH 'C'
               REPLACE horfin  ;
                       WITH  ;
                       TIME()
               IF ((w_codtall >  ;
                  '010 ' .AND.  ;
                  w_codtall <  ;
                  '050 ') .OR.  ;
                  w_codtall >  ;
                  '060 ') .AND.  ;
                  w_indori <>  ;
                  'GARA' .AND.  ;
                  w_indori <>  ;
                  'PVEN'
                    REPLACE auxest  ;
                            WITH  ;
                            '028 '
               ELSE
                    REPLACE auxest  ;
                            WITH  ;
                            '026 '
               ENDIF
               REPLACE entfac  ;
                       WITH  ;
                       f_ceros(fecfin -  ;
                       fecemi,9, ;
                       1)
               REPLACE fecest  ;
                       WITH  ;
                       DATE(),  ;
                       horest  ;
                       WITH  ;
                       TIME()
               IF w_codtall >  ;
                  '010 '
                    REPLACE fecent  ;
                            WITH  ;
                            DATE()
                    REPLACE horent  ;
                            WITH  ;
                            TIME()
               ENDIF
               UNLOCK
               tot_titore = 0
               SELECT st_isrep
               SEEK st_iorep.numsol
               IF FOUND()
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
                    w_feccom = feccom
               ENDIF
               SELECT st_iprep
               SEEK w_numaux
               SCAN WHILE numord =  ;
                    w_numaux  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF indest <>  ;
                       'N'
                         w_numped =  ;
                          numdoc
                         SELECT st_idped
                         SEEK w_numped +  ;
                              w_numaux
                         IF FOUND()
                              SCAN  ;
                               WHILE  ;
                               w_numped =  ;
                               numdoc  ;
                               .AND.  ;
                               numord =  ;
                               w_numaux  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF()
                                   IF canpro > 0
                                        tot_titore = tot_titore + totite
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
               subto = tot_titore +  ;
                       w_cosmob +  ;
                       w_flete
               totneto = subto
               totigva = ROUND(totneto *  ;
                         w_facigv,  ;
                         2)
               totbrut = totneto +  ;
                         totigva
               SELECT st_iorep
               SEEK w_solici
               DO rbloquea
               REPLACE subtot  ;
                       WITH subto,  ;
                       cosmob  ;
                       WITH  ;
                       w_cosmob,  ;
                       flete WITH  ;
                       w_flete
               REPLACE totnet  ;
                       WITH  ;
                       totneto,  ;
                       totigv  ;
                       WITH  ;
                       totigva
               REPLACE totbru  ;
                       WITH  ;
                       totbrut,  ;
                       cosrep  ;
                       WITH  ;
                       tot_titore
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
               SELECT 20
               USE SHARED  ;
                   st_estad
               APPEND BLANK
               DO rbloquea
               REPLACE indori  ;
                       WITH  ;
                       w_indori,  ;
                       anorep  ;
                       WITH  ;
                       YEAR(DATE())
               REPLACE mesrep  ;
                       WITH  ;
                       MONTH(DATE()),  ;
                       numord  ;
                       WITH  ;
                       w_numero
               REPLACE codmod  ;
                       WITH  ;
                       w_codmod,  ;
                       codmar  ;
                       WITH  ;
                       w_codmar
               REPLACE numser  ;
                       WITH  ;
                       w_numser,  ;
                       fecrep  ;
                       WITH  ;
                       w_fecemi
               REPLACE fecdes  ;
                       WITH  ;
                       DATE(),  ;
                       feccon  ;
                       WITH  ;
                       w_feccom
               REPLACE codtec  ;
                       WITH  ;
                       w_codtec1,  ;
                       valmao  ;
                       WITH  ;
                       w_cosmob
               REPLACE titore  ;
                       WITH  ;
                       w_titore,  ;
                       valrep  ;
                       WITH  ;
                       tot_titore
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
               w_est = '026 '
               SELECT ge_tab0
               SEEK 'ESOR' +  ;
                    w_est
               IF FOUND()
                    w_esorde = tab_destab
               ENDIF
               SELECT 20
               USE SHARED  ;
                   st_mvord
               APPEND BLANK
               DO rbloquea
               REPLACE dia WITH  ;
                       DATE(),  ;
                       hora WITH  ;
                       TIME()
               REPLACE orden WITH  ;
                       w_numord,  ;
                       tecnico  ;
                       WITH  ;
                       w_codtec1
               REPLACE estado  ;
                       WITH  ;
                       '026 ',  ;
                       destado  ;
                       WITH  ;
                       w_esorde
               REPLACE user WITH  ;
                       users
               REPLACE time WITH  ;
                       TIME(),  ;
                       date WITH  ;
                       DATE()
               UNLOCK
               IF w_codtall >  ;
                  '010 ' .AND.  ;
                  w_indori <>  ;
                  'GARA' .AND.  ;
                  w_indori <>  ;
                  'PVEN'
                    w_est = '028 '
                    SELECT ge_tab0
                    SEEK 'ESOR' +  ;
                         w_est
                    IF FOUND()
                         w_esorde =  ;
                          tab_destab
                    ENDIF
                    SELECT 20
                    USE SHARED  ;
                        st_mvord
                    APPEND BLANK
                    DO rbloquea
                    REPLACE dia  ;
                            WITH  ;
                            DATE(),  ;
                            hora  ;
                            WITH  ;
                            TIME()
                    REPLACE orden  ;
                            WITH  ;
                            w_numord,  ;
                            tecnico  ;
                            WITH  ;
                            w_codtec1
                    REPLACE estado  ;
                            WITH  ;
                            '028 ',  ;
                            destado  ;
                            WITH  ;
                            w_esorde
                    REPLACE user  ;
                            WITH  ;
                            users
                    REPLACE time  ;
                            WITH  ;
                            TIME(),  ;
                            date  ;
                            WITH  ;
                            DATE()
                    UNLOCK
               ENDIF
               SELECT 20
               USE SHARED  ;
                   st_ispre ORDER  ;
                   st_numsol
               SEEK st_iorep.numsol
               IF FOUND()
                    REPLACE indest  ;
                            WITH  ;
                            'C'
                    REPLACE indori  ;
                            WITH  ;
                            st_iorep.indori
                    REPLACE user  ;
                            WITH  ;
                            users
                    REPLACE time  ;
                            WITH  ;
                            TIME()
                    REPLACE date  ;
                            WITH  ;
                            DATE()
               ENDIF
               SELECT ge_tab0
               SEEK 'CAUS' + 'Y'
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '** Causa no se Encontro en la Tabla **'
               ELSE
                    SELECT 20
                    USE SHARED  ;
                        st_movca  ;
                        ORDER  ;
                        codigo
                    SEEK st_iorep.numdoc
                    IF  .NOT.  ;
                        FOUND()
                         APPEND BLANK
                    ELSE
                         DO rbloquea
                         REPLACE numord  ;
                                 WITH  ;
                                 w_numord
                         REPLACE numsol  ;
                                 WITH  ;
                                 w_solici
                         REPLACE codcau  ;
                                 WITH  ;
                                 'Y   '
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
               ENDIF
               SELECT ge_tab0
               SEEK 'SOLU' + 'P'
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '** Soluci¢n no se Encontro en Tabla **'
               ELSE
                    SELECT 20
                    USE SHARED  ;
                        st_movso  ;
                        ORDER  ;
                        codigo
                    SEEK st_iorep.numdoc
                    IF  .NOT.  ;
                        FOUND()
                         APPEND BLANK
                    ELSE
                         DO rbloquea
                         REPLACE numord  ;
                                 WITH  ;
                                 w_numord
                         REPLACE numsol  ;
                                 WITH  ;
                                 w_solici
                         REPLACE codsol  ;
                                 WITH  ;
                                 'P   '
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
               ENDIF
               DO mensa WITH  ;
                  '*** Espere un momento, por favor ***',  ;
                  'SACA'
          ENDIF
     ENDIF
ENDDO
CLOSE DATABASES
DO sacawin
ON KEY LABEL f6
ON KEY LABEL f10
RETURN
*
FUNCTION pagta
PARAMETER w_solici
SELECT 20
USE SHARED gc_hve00 ORDER nrdore
STORE 0 TO w_pagsol, a, w_pagdol,  ;
      w_flete
SEEK w_solici
SCAN WHILE  ;
     ALLTRIM(gc_hve00.hve_nrdore) =  ;
     ALLTRIM(w_solici) .AND.   ;
     .NOT. EOF()
     IF gc_hve00.hve_estdoc <>  ;
        'A' .AND.  ;
        gc_hve00.hve_codmov =  ;
        'PCTA'
          IF hve_codcta = '005 '
               w_flete = w_flete +  ;
                         hve_totvta
          ELSE
               a = a + 1
               w_pagsol = w_pagsol +  ;
                          hve_solgen
               w_pagdol = w_pagdol +  ;
                          hve_totgen
          ENDIF
     ENDIF
ENDSCAN
RETURN w_pagdol
*
FUNCTION cosrep
SELECT st_iprep
SEEK w_numaux
IF FOUND()
     SCAN WHILE numord = w_numaux  ;
          .AND.  .NOT. EOF()
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
                              tot_titore =  ;
                               tot_titore +  ;
                               totite
                         ENDIF
                    ENDSCAN
               ENDIF
          ENDIF
     ENDSCAN
ENDIF
RETURN tot_titore
*
PROCEDURE ayuda15
ON KEY
IF VARREAD() == 'W_NUMERO'
     SELECT st_iorep
     SET FILTER TO indest <> 'N';
.AND. indest <> 'F';
.AND. indest <> 'B'
     campoa = 'numsol+" "+dtoc(fecemi)+" "+numdoc+" "+substr(numser,1,12)+" "+codent+" "+substr(codmod,1,10)+" "+subst(indest,1,2)+" "+alltrim(indori)'
     w_origen = 'OR'
     DO ayuda8 WITH campoa,  ;
        w_origen, SELECT()
     SELECT st_iorep
     SET ORDER TO ord_numsol
     SET FILTER TO
ENDIF
ON KEY LABEL F6 do ayuda15
RETURN
*
FUNCTION numord2
PARAMETER num
IF num = 0
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
        '** N§ Solicitud no Existe **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'N   '
     DO error WITH  ;
        '** N§ Solicitud esta Anulada **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'C   '
     DO error WITH  ;
        '** N§ Solicitud esta CERRADA **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'F   ' .OR. indest ==  ;
   'B   '
     DO error WITH  ;
        '** N§ Solicitud esta FACTURADA **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF auxest <> '003 '
     DO error WITH  ;
        '** S/S No se Encuentra en Estado Presupuesto **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
w_numord = numdoc
w_solici = numsol
w_numaux = w_numord
w_codtall = codtall
w_fecemi = fecemi
w_codmar = codmar
w_codmod = codmod
w_indori = indori
w_serie = numser
w_codtec1 = codtec
w_codent = codent
w_cosmob = cosmob
w_numser = numser
w_codtec1 = codtec
w_titore = DATE() - w_fecemi
FOR i = 1 TO 6
     w_nota( i) = SUBSTR(observ,  ;
           1 + ((i - 1) * 38),  ;
           38)
     w_nota( i) = w_nota(i) +  ;
           SPACE(38 -  ;
           LEN(w_nota(i)))
ENDFOR
w_cuenta = pagta(w_solici)
SELECT st_iorep
w_flete = w_flete + flete
w_rep = cosrep()
IF w_codtall > '010 '
     DO CASE
          CASE w_indori = 'FGAR'
               SELECT ge_tab0
               SEEK 'TALL' +  ;
                    '008 '
               IF FOUND()
                    w_cosmob = tab_factor
               ENDIF
               IF w_cuenta > 0
                    w_ctasig = ROUND(w_cuenta /  ;
                               (1 +  ;
                               w_facigv),  ;
                               2)
                    IF w_ctasig >=  ;
                       w_rep +  ;
                       w_flete +  ;
                       w_cosmob
                         w_cosmob =  ;
                          w_ctasig -  ;
                          w_rep -  ;
                          w_flete
                    ELSE
                    ENDIF
               ELSE
                    IF nivell <>  ;
                       'A7'
                         DO error  ;
                            WITH  ;
                            '*** S/S no tiene Pago a Cuenta por Revisi¢n ***'
                         KEYBOARD  ;
                          '{CTRL+Y}'  ;
                          PLAIN
                         RETURN .F.
                    ENDIF
               ENDIF
          CASE w_indori = 'GARA'  ;
               .OR. w_indori =  ;
               'PVEN'
               SELECT ge_tab0
               SEEK 'ESOR' +  ;
                    '026 '
               IF FOUND()
                    w_cosmob = tab_factor
               ENDIF
          CASE w_indori = 'FREC'  ;
               .OR. w_indori =  ;
               'GREC' .OR.  ;
               w_indori = 'PREC'
               IF w_rep > 0 .AND.  ;
                  w_indori =  ;
                  'FREC'
                    DO error WITH  ;
                       '*** S/S  tiene Saldo Por Repuesto ***'
                    KEYBOARD '{CTRL+Y}'  ;
                             PLAIN
                    RETURN .F.
               ELSE
                    w_cosmob = 0
               ENDIF
     ENDCASE
ELSE
     DO CASE
          CASE w_indori = 'GARA'  ;
               .OR. w_indori =  ;
               'PVEN'
               SELECT ge_tab0
               SEEK 'TALL' +  ;
                    '008 '
               IF FOUND()
                    w_cosmob = tab_factor
               ENDIF
          CASE w_indori = 'FGAR'  ;
               .AND. w_cuenta >  ;
               0
               SELECT ge_tab0
               SEEK 'TALL' +  ;
                    '008 '
               IF FOUND()
                    w_cosmob = tab_factor
               ENDIF
               w_rep = cosrep()
               w_ctasig = ROUND((w_cuenta /  ;
                          (1 +  ;
                          w_facigv)),  ;
                          2)
               IF w_ctasig >  ;
                  w_rep + w_flete +  ;
                  w_cosmob
                    w_cosmob = w_ctasig -  ;
                               (w_rep +  ;
                               w_flete)
               ENDIF
          CASE w_indori = 'FGAR'  ;
               .AND. w_cuenta =  ;
               0
               SELECT ge_tab0
               SEEK 'TALL' +  ;
                    '008 '
               IF FOUND()
                    w_cosmob = tab_factor
               ENDIF
          CASE w_indori = 'FREC'  ;
               .OR. w_indori =  ;
               'GREC' .OR.  ;
               w_indori = 'PREC'
               w_cosmob = 0
     ENDCASE
ENDIF
@ 04, 37 SAY 'Cliente:' COLOR N+/ ;
  W 
@ 04, 46 SAY  ;
  SUBSTR(oodescli(w_codent), 1,  ;
  25) COLOR W+/N 
@ 05, 25 SAY w_fecemi PICTURE  ;
  '99/99/9999'
@ 05, 37 SAY 'O/R    :' COLOR N+/ ;
  W 
@ 05, 46 SAY w_numord COLOR W+/N 
@ 06, 25 GET w_cosmob PICTURE  ;
  '9,999,999.99'
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
RETURN .T.
*
PROCEDURE ve_orden9
SAVE SCREEN TO panta11
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'COLO'
SELECT st_iorep
SEEK w_solici
w_numaux = w_numord
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
SEEK '&w_cliaux'
w_noment = noment
w_aux = w_codmar + w_codmod
SELECT st_imode
SEEK '&w_aux'
w_aux = codcla
w_linea = linea
SELECT st_sicli
i = 1
SEEK w_solici
SCAN WHILE  .NOT. EOF() .AND.  ;
     numdoc = w_solici
     w_aux = SUBSTR(codsin, 2, 3)
     SELECT st_sint
     SEEK w_linea + w_aux
     w_codsin( i) =  ;
             SUBSTR(st_sint.dessin,  ;
             1, 35)
     i = i + 1
     SELECT st_sicli
ENDSCAN
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
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'BBB'
RETURN
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
SEEK '&w_aux'
w_estado = tab_destab
w_aux = 'ESOR' + w_esor
SEEK '&w_aux'
w_desesor = tab_destab
DO coloca2 WITH 01, 66, w_numero2
DO coloca2 WITH 05, 20, w_codmar
DO coloca2 WITH 06, 20, w_codmod
DO coloca2 WITH 07, 20, w_numser
DO coloca2 WITH 08, 25,  ;
   STR(w_numstk, 9)
SELECT ge_tab0
w_aux = 'MARC' + w_codmar
SEEK '&w_aux'
DO coloca2 WITH 05, 25,  ;
   SUBSTR(tab_destab, 1, 30)
w_aux = w_codmar + w_codmod
SELECT st_imode
SEEK '&w_aux'
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

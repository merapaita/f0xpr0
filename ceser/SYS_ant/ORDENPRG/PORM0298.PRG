*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
tit_prg = ' PROCESO '
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' SALIDAS EN GENERAL '
DEFINE POPUP campo PROMPT FIELDS  ;
       numdoc + '�' +  ;
       SUBSTR(marca, 1, 12) + '�' +  ;
       codmod + '�' + numser +  ;
       '�' + articu TITLE  ;
       'S/S컴컴횺ARCA컴컴컴컴MODELO컴컴컴컴컴SERIE컴컴컴컴컴컴컴컴ARTICULO컴'  ;
       MARK CHR(16) MARGIN  ;
       SCROLL
SELECT 1
USE SHARED st_imode ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_numsol
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 4
USE SHARED st_iclpr ORDER codigo
SELECT 5
USE SHARED st_isrep ORDER codigo
STORE 'S' TO w_opcion
DO WHILE .T.
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     STORE 0 TO w_numero,  ;
           w_contab, w_cosrep,  ;
           w_numte1, w_numte2,  ;
           w_factor, w_sol
     STORE SPACE(09) TO w_codent,  ;
           w_nomemp, w_noment,  ;
           w_nomcal, w_emisor,  ;
           w_nomdis, w_nomciu,  ;
           w_nomdi2
     STORE SPACE(11) TO w_codcli
     STORE SPACE(04) TO w_codemi
     STORE SPACE(15) TO w_desmod,  ;
           w_codmod, w_desmod,  ;
           w_desmar
     w_flag = 1
     @ 04, 02 SAY  ;
       'Guia(G)/Salida(S):' COLOR  ;
       W/N 
     @ 05, 02 SAY  ;
       'S/S Seleccionadas:' COLOR  ;
       W/N 
     @ 07, 00 TO 16, 72 COLOR W/N 
     @ 07, 03 SAY  ;
       'S/S컴컴횺ARCA컴컴컴컴MODELO컴컴컴컴컴SERIE컴컴컴컴컴컴컴컴ARTICULO'  ;
       COLOR W/N 
     @ 04, 20 GET w_opcion  ;
       PICTURE '@m S,G' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     STORE .T. TO sigue
     DO WHILE sigue
          STORE 1 TO w_opcio2
          SET CURSOR ON
          @ 05, 20 GET w_numero  ;
            PICTURE '99999999'  ;
            VALID  ;
            oovalid(VARREAD())  ;
            WHEN  ;
            oowhen(VARREAD())  ;
            COLOR SCHEME 8
          READ CYCLE
          IF LASTKEY() = 27
               DO limpia
               sigue = .F.
          ENDIF
     ENDDO
ENDDO
RELEASE POPUP campo
CLOSE DATABASES
DO sacawin
RETURN
*
PROCEDURE limpia
CREATE CURSOR soli (numdoc C (8),  ;
       codmar C (4), marca C (25),  ;
       codmod C (15), numser C  ;
       (20), articu C (20),  ;
       codent C (9), indori C (4),  ;
       sol N (3))
INDEX ON numdoc TAG ind1
SET RELATION TO numdoc INTO st_iorep,;
codmar + codmod INTO st_imode
STORE 0 TO w_contab
@ 07, 00 GET listfield DEFAULT  ;
  soli.numdoc SIZE 16, 73 POPUP  ;
  campo COLOR W/N,W/N 
CLEAR GETS
RETURN
*
PROCEDURE oowhen
PARAMETER opc
DO CASE
     CASE VARREAD() = 'W_OPCION'
          DO sacaf6
          ON KEY LABEL f6
     CASE VARREAD() = 'W_NUMERO'
          IF w_contab > 0
               DO esc_indica WITH  ;
                  2, 'BBB', 'BBB',  ;
                  'IMP', 'ESC'
               ON KEY LABEL f7 do imprimir
          ENDIF
          DO colocaf6
          ON KEY LABEL f6 do ayuda12
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER opc
DO CASE
     CASE opc = 'W_OPCION'
          IF w_opcion <> 'S'  ;
             .AND. w_opcion <>  ;
             'G'
               RETURN .F.
          ENDIF
          IF EMPTY(w_opcion)
               DO error WITH  ;
                  '*** No se permiten blancos ***'
               RETURN .F.
          ENDIF
          DO limpia
     CASE opc = 'W_NUMERO'
          IF  .NOT.  ;
              EMPTY(w_numero)
               SELECT soli
               SEEK STR(w_numero,  ;
                    8)
               IF FOUND()
                    DO error WITH  ;
                       '*** Solicitud ya seleccionada ***'
                    RETURN .F.
               ENDIF
               IF w_contab > 6
                    DO error WITH  ;
                       '*** No se puede ingresar m쟳 de siete ***'
                    RETURN .F.
               ENDIF
               SELECT st_iorep
               SEEK STR(w_numero,  ;
                    8)
               IF  .NOT. FOUND()
                    SELECT st_isrep
                    SEEK STR(w_numero,  ;
                         8)
                    IF  .NOT.  ;
                        FOUND()
                         DO error  ;
                            WITH  ;
                            '*** Solicitud No Encontrada ***'
                         RETURN .F.
                    ELSE
                         IF w_opcion =  ;
                            'S'
                              DO error  ;
                                 WITH  ;
                                 '*** Solicitud No sale con 굎ta opci줻 ***'
                              RETURN  ;
                               .F.
                         ELSE
                              IF st_isrep.codent <>  ;
                                 w_codent  ;
                                 .AND.  ;
                                 w_contab <>  ;
                                 0
                                   DO error WITH '*** No es el mismo Cliente ***'
                                   RETURN .F.
                              ELSE
                                   IF indest = 'N'
                                        DO error WITH '*** Solicitud esta Anulada ***'
                                        RETURN .F.
                                   ELSE
                                        IF SUBSTR(indori, 1, 1) <> 'G'
                                             DO error WITH '*** Solicitud No es Garant죂 ***'
                                             RETURN .F.
                                        ELSE
                                             w_sol = 1
                                             IF w_contab = 0
                                                  @ 04, 30 SAY 'Cliente..:'
                                                  @ 05, 30 SAY 'Direcci줻:'
                                                  @ 06, 30 SAY 'Tel괽ono.:'
                                                  w_emisor = (SUBSTR(codemi, 1, 1)) + '00'
                                                  w_codemi = codemi
                                                  w_codcli = SPACE(11 - LEN(ALLTRIM(w_emisor))) + ALLTRIM(w_emisor)
                                                  w_codcli = ALLTRIM(STR(VAL(w_codcli), 11))
                                                  SELECT st_iclpr
                                                  SEEK 'C' + st_isrep.codent
                                                  IF FOUND()
                                                       @ 04, 41 SAY st_iclpr.noment
                                                       @ 05, 41 SAY st_iclpr.nomcal
                                                       @ 06, 41 SAY st_iclpr.numte1
                                                       w_codent = st_isrep.codent
                                                       w_noment = st_iclpr.noment
                                                       w_nomcal = st_iclpr.nomcal
                                                       w_numte1 = st_iclpr.numte1
                                                       w_numte2 = st_iclpr.numte2
                                                       w_nomdis = ootab('DIST',st_iclpr.nomdis)
                                                       w_nomciu = ootab('PROV',st_iclpr.nomciu)
                                                  ENDIF
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ELSE
                    IF st_iorep.codent <>  ;
                       w_codent  ;
                       .AND.  ;
                       w_contab <>  ;
                       0
                         DO error  ;
                            WITH  ;
                            '*** No es el mismo Cliente ***'
                         RETURN .F.
                    ENDIF
                    IF (codtall >  ;
                       '010'  ;
                       .AND.  ;
                       codtall <  ;
                       '020')  ;
                       .OR.  ;
                       (codtall >  ;
                       '060 '  ;
                       .AND.  ;
                       codtall <  ;
                       '070 ')
                         DO error  ;
                            WITH  ;
                            '*** Solicitud es de Domicilio ***'
                         RETURN .F.
                    ELSE
                         IF indest =  ;
                            'N'
                              DO error  ;
                                 WITH  ;
                                 '*** Solicitud esta Anulada ***'
                              RETURN  ;
                               .F.
                         ELSE
                              IF indest =  ;
                                 'P'  ;
                                 .OR.  ;
                                 indest =  ;
                                 'V'
                                   DO error WITH '*** Solicitud no esta Cerrada ***'
                                   RETURN .F.
                              ELSE
                                   IF  .NOT. EMPTY(fecent)
                                        DO error WITH '*** Solicitud sali� el '+DTOC(fecent)+' ***'
                                        RETURN .F.
                                   ELSE
                                        IF (indest = 'F' .OR. indest = 'B') .AND. auxest = '100 '
                                             DO error WITH '*** Solicitud de Servicio Facturada ***'
                                             RETURN .F.
                                        ELSE
                                             STORE 0 TO w_saldo, w_acta, w_cosrep
                                             IF SUBSTR(indori, 1, 1) = 'F'
                                                  w_facigv = facigv()
                                                  w_selec = SELECT()
                                                  SELECT 21
                                                  USE SHARED st_iprep ORDER rep_numord
                                                  SELECT 22
                                                  USE SHARED st_idped ORDER codigo
                                                  DO cosrep
                                                  SELECT 21
                                                  USE
                                                  SELECT 22
                                                  USE
                                                  SELECT (w_selec)
                                                  IF indori = 'FGAR'
                                                       w_saldo = ROUND((w_cosrep + cosmob + flete) * w_facigv, 2)
                                                  ELSE
                                                       w_saldo = ROUND((w_cosrep + flete) * w_facigv, 2)
                                                  ENDIF
                                                  DO acta WITH numsol
                                             ENDIF
                                             IF w_saldo > w_acta
                                                  DO error WITH '*** O/R Tiene saldo por Cancelar ***'
                                                  RETURN .F.
                                             ELSE
                                                  w_sol = 0
                                                  IF w_contab = 0
                                                       @ 04, 30 SAY 'Cliente..:'
                                                       @ 05, 30 SAY 'Direcci줻:'
                                                       @ 06, 30 SAY 'Tel괽ono.:'
                                                       w_emisor = (SUBSTR(codemi, 1, 1)) + '00'
                                                       w_codemi = codemi
                                                       w_codcli = SPACE(11 - LEN(ALLTRIM(w_emisor))) + ALLTRIM(w_emisor)
                                                       w_codcli = ALLTRIM(STR(VAL(w_codcli), 11))
                                                       SELECT st_iclpr
                                                       SEEK 'C' + st_iorep.codent
                                                       IF FOUND()
                                                            @ 04, 41 SAY st_iclpr.noment
                                                            @ 05, 41 SAY st_iclpr.nomcal
                                                            @ 06, 41 SAY st_iclpr.numte1
                                                            w_codent = st_iorep.codent
                                                            w_noment = st_iclpr.noment
                                                            w_nomcal = st_iclpr.nomcal
                                                            w_numte1 = st_iclpr.numte1
                                                            w_numte2 = st_iclpr.numte2
                                                            w_nomdis = ootab('DIST',st_iclpr.nomdis)
                                                            w_nomciu = ootab('PROV',st_iclpr.nomciu)
                                                       ENDIF
                                                  ENDIF
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
               SELECT soli
               APPEND BLANK
               IF w_sol = 0
                    REPLACE numdoc  ;
                            WITH  ;
                            STR(w_numero,  ;
                            8)
                    REPLACE codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            numser  ;
                            WITH  ;
                            st_iorep.numser,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar,  ;
                            codent  ;
                            WITH  ;
                            st_iorep.codent
                    REPLACE indori  ;
                            WITH  ;
                            st_iorep.indori,  ;
                            sol  ;
                            WITH  ;
                            0
               ELSE
                    REPLACE numdoc  ;
                            WITH  ;
                            STR(w_numero,  ;
                            8)
                    REPLACE codmod  ;
                            WITH  ;
                            st_isrep.codmod,  ;
                            numser  ;
                            WITH  ;
                            st_isrep.numser,  ;
                            codmar  ;
                            WITH  ;
                            st_isrep.codmar,  ;
                            codent  ;
                            WITH  ;
                            st_isrep.codent
                    REPLACE indori  ;
                            WITH  ;
                            st_isrep.indori,  ;
                            sol  ;
                            WITH  ;
                            1
               ENDIF
               SELECT ge_tab0
               SEEK 'MARC' +  ;
                    st_iorep.codmar
               IF FOUND()
                    SELECT soli
                    REPLACE marca  ;
                            WITH  ;
                            ge_tab0.tab_destab
               ENDIF
               SELECT ge_tab0
               SEEK 'CLAS' +  ;
                    st_imode.codcla
               IF FOUND()
                    SELECT soli
                    REPLACE articu  ;
                            WITH  ;
                            SUBSTR(ge_tab0.tab_destab,  ;
                            1,  ;
                            20)
               ENDIF
               w_contab = w_contab +  ;
                          1
               SELECT soli
               @ 07, 00 GET  ;
                 listfield  ;
                 DEFAULT  ;
                 soli.numdoc SIZE  ;
                 16, 73 POPUP  ;
                 campo COLOR W/N, ;
                 W/N 
          ENDIF
ENDCASE
RETURN
*
PROCEDURE imprimir
ON KEY
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
DO emplea
IF LASTKEY() = -6 .OR. LASTKEY() =  ;
   13 .OR. LASTKEY() <> 27
     DO mensa WITH  ;
        '** G R A B A N D O ... **',  ;
        'COLO'
     DO impsal
     SELECT soli
     GOTO TOP
     SCAN WHILE  .NOT. EOF()
          IF sol = 0
               DO graba
          ENDIF
     ENDSCAN
     DO mensa WITH  ;
        '** G R A B A N D O ... **',  ;
        'SACA'
     DO limpia
ENDIF
sigue = .F.
RETURN
*
FUNCTION graba
SELECT st_iorep
SEEK soli.numdoc
IF FOUND()
     IF codtall < '011 ' .OR.  ;
        (codtall > '050 ' .AND.  ;
        codtall < '060 ')
          STORE SPACE(04) TO  ;
                w_nuest
          IF auxest = '010 '
               IF indori = 'PVEN'
                    w_nuest = '030 '
               ELSE
                    IF indori =  ;
                       'GARA'
                         w_nuest =  ;
                          '020 '
                    ELSE
                         IF SUBSTR(indori,  ;
                            2, 1) =  ;
                            'R'
                              w_nuest =  ;
                               '022 '
                         ELSE
                              DO error  ;
                                 WITH  ;
                                 '*** Solicitud no puede Salir, Revisar ***'
                              w_flag =  ;
                               0
                              RETURN  ;
                               .F.
                              w_nuest =  ;
                               '025 '
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
          IF auxest = '021 ' .OR.  ;
             auxest = '026 '
               w_nuest = '023 '
          ENDIF
          IF auxest = '080 ' .OR.  ;
             auxest = '081 '
               w_nuest = '100 '
          ENDIF
          SELECT st_iorep
          DO rbloquea
          REPLACE auxest WITH  ;
                  w_nuest, fecest  ;
                  WITH DATE(),  ;
                  horest WITH  ;
                  TIME(), fecent  ;
                  WITH DATE(),  ;
                  horent WITH  ;
                  TIME()
          REPLACE user WITH users
          REPLACE date WITH  ;
                  DATE()
          REPLACE time WITH  ;
                  TIME()
          UNLOCK
          SELECT 20
          USE SHARED st_mvord
          APPEND BLANK
          DO rbloquea
          REPLACE dia WITH DATE(),  ;
                  hora WITH  ;
                  TIME()
          REPLACE orden WITH  ;
                  st_iorep.numdoc,  ;
                  tecnico WITH  ;
                  st_iorep.codtec
          REPLACE estado WITH  ;
                  w_nuest, user  ;
                  WITH users,  ;
                  date WITH  ;
                  DATE(), time  ;
                  WITH TIME()
          IF w_opcion = 'G'
               REPLACE destado  ;
                       WITH  ;
                       SUBSTR(ootab('ESOR', ;
                       w_nuest),  ;
                       1, 25) +  ;
                       STR(w_factor,  ;
                       10)
          ELSE
               REPLACE destado  ;
                       WITH  ;
                       SUBSTR(ootab('ESOR', ;
                       w_nuest),  ;
                       1, 30)
          ENDIF
          UNLOCK
     ENDIF
ENDIF
RETURN
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
RETURN
*
PROCEDURE impsal
SET CONSOLE OFF
SET DEVICE TO PRINTER
SET PRINTER ON
SET PRINTER TO lpt1
@ PROW(), PCOL() SAY CHR(15)
@ PROW(), PCOL() SAY CHR(27) +  ;
  'C' + CHR(33)
DO impsal2
EJECT
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
RETURN
*
PROCEDURE impsal2
SELECT ge_tab0
SEEK 'PROV' + st_iclpr.nomciu
IF FOUND()
     w_nomciu = SUBSTR(tab_destab,  ;
                1, 30)
ELSE
     w_numciu = SPACE(30)
ENDIF
SEEK 'DIST' + st_iclpr.nomdis
IF FOUND()
     w_desdis = SUBSTR(tab_destab,  ;
                1, 30)
ELSE
     w_desdis = SPACE(30)
ENDIF
w_fecha = DTOC(DATE()) + ' - ' +  ;
          TIME()
IF w_opcion = 'G'
     DO porm0ord WITH w_opcion
ELSE
     w_tit2 = 'SINTOMAS :'
     w_tit3 = 'ACCESORIOS :'
     w_tit4 = 'OBSERVACIONES :'
     w_tit5 = 'SALIDAS'
     w_tit6 = 'INF.TEC. / NOTAS :'
     IF w_contab > 1
          DO porm0ord WITH  ;
             w_opcion
     ELSE
          SELECT ge_tab0
          SEEK 'MARC' +  ;
               soli.codmar
          IF FOUND()
               w_desmar = SUBSTR(tab_destab,  ;
                          1, 30)
          ELSE
               w_desmar = SPACE(30)
          ENDIF
          SEEK 'INGA' +  ;
               soli.indori
          IF FOUND()
               w_indori = SUBSTR(tab_destab,  ;
                          1, 30)
          ELSE
               w_indori = SPACE(30)
          ENDIF
          w_desmod = st_imode.nommod
          w_codtec = st_iorep.codtec
          w_numord = st_iorep.numdoc
          w_codmod = soli.codmod
          w_numser = soli.numser
          w_numsol = STR(w_numero,  ;
                     8)
          SELECT 20
          USE SHARED st_itecn  ;
              ORDER codigo
          SEEK st_iorep.codtec
          IF FOUND()
               w_destec = noment
          ELSE
               w_destec = SPACE(30)
          ENDIF
          IF SUBSTR(soli.indori,  ;
             1, 1) = 'G'
               SELECT 20
               USE SHARED  ;
                   st_iseri ORDER  ;
                   ser_codmar
               SEEK soli.codmar +  ;
                    soli.codmod
               IF FOUND()
                    w_doctia = ooseri2(soli.codmar, ;
                               soli.codmod, ;
                               soli.numser)
               ELSE
                    w_doctia = SPACE(10)
               ENDIF
          ELSE
               STORE SPACE(10) TO  ;
                     w_doctia
          ENDIF
          DIMENSION w_codsin( 15)
          DIMENSION w_acceso( 15)
          DIMENSION w_obsord( 06)
          DIMENSION w_obssol( 06)
          SELECT st_isrep
          SEEK w_numsol
          IF FOUND()
               w_feccom = DTOC(feccom)
               FOR i = 1 TO 15
                    w_codsin = SPACE(35)
                    w_acceso( i) =  ;
                            SUBSTR(desace,  ;
                            1 +  ;
                            ((i -  ;
                            1) *  ;
                            35),  ;
                            35)
                    w_acceso( i) =  ;
                            w_acceso(i) +  ;
                            SPACE(35 -  ;
                            LEN(w_acceso(i)))
                    IF i <= 6
                         w_obssol(  ;
                                 i) =  ;
                                 SUBSTR(observ,  ;
                                 1 +  ;
                                 ((i -  ;
                                 1) *  ;
                                 45),  ;
                                 45)
                         w_obssol(  ;
                                 i) =  ;
                                 w_obssol(i) +  ;
                                 SPACE(45 -  ;
                                 LEN(w_obssol(i)))
                    ENDIF
               ENDFOR
          ENDIF
          SELECT 20
          USE SHARED st_sicli  ;
              ORDER codigo
          SELECT 21
          USE SHARED st_sint  ;
              ORDER codigo
          SELECT st_sicli
          SEEK w_numsol
          i = 1
          IF FOUND()
               SCAN WHILE numdoc =  ;
                    w_numsol  ;
                    .AND.  .NOT.  ;
                    EOF()
                    w_aux2 = SUBSTR(codsin,  ;
                             2,  ;
                             3)
                    SELECT st_sint
                    SEEK w_aux2
                    w_codsin( i) =  ;
                            SUBSTR(dessin,  ;
                            1,  ;
                            35)
                    i = i + 1
                    SELECT st_sicli
               ENDSCAN
          ENDIF
          FOR i = 1 TO 6
               w_obsord( i) =  ;
                       SUBSTR(st_iorep.observ,  ;
                       1 + ((i -  ;
                       1) * 38),  ;
                       38)
               w_obsord( i) =  ;
                       w_obsord(i) +  ;
                       SPACE(38 -  ;
                       LEN(w_obsord(i)))
          ENDFOR
          DO porm0ord WITH  ;
             w_opcion
     ENDIF
ENDIF
RETURN
*
PROCEDURE cosrep
SELECT st_iprep
STORE 0 TO w_solsub, w_cosrep
SEEK st_iorep.numdoc
IF FOUND()
     SCAN WHILE numord =  ;
          st_iorep.numdoc .AND.   ;
          .NOT. EOF()
          IF indest <> 'N'
               SELECT st_idped
               SEEK st_iprep.numdoc +  ;
                    st_iorep.numdoc
               IF FOUND()
                    SCAN WHILE  ;
                         numdoc =  ;
                         st_iprep.numdoc  ;
                         .AND.  ;
                         numord =  ;
                         st_iorep.numdoc  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         IF canpro >  ;
                            0
                              w_cosrep =  ;
                               w_cosrep +  ;
                               ROUND((ROUND((valpro *  ;
                               w_facigv),  ;
                               2) *  ;
                               canpro),  ;
                               2)
                         ENDIF
                    ENDSCAN
               ENDIF
          ENDIF
          SELECT st_iprep
     ENDSCAN
ENDIF
RETURN
*
PROCEDURE acta
PARAMETER w_numsol
w_selec = SELECT()
SELECT 21
USE SHARED gc_hve00 ORDER nrdore
SEEK w_numsol
STORE 0 TO w_acta, w_acts
SCAN WHILE  ;
     VAL(gc_hve00.hve_nrdore) =  ;
     VAL(w_numsol) .AND.  .NOT.  ;
     EOF()
     IF gc_hve00.hve_estdoc <>  ;
        'A'
          w_acta = w_acta +  ;
                   hve_totgen
          w_acts = w_acts +  ;
                   hve_solgen
     ENDIF
ENDSCAN
SELECT (w_selec)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

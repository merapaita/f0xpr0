*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F10 DO FCINCO
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INGRESO DE SERVICIOS POR USUARIO '
CLOSE DATABASES
SELECT 1
USE st_isrep ORDER sol_fchemi
SELECT 2
USE ge_tab0 ORDER codigo
SELECT 3
USE st_iseri ORDER ser_codmar
SELECT 4
USE GC_VND00 ORDER CODIGO
SELECT 5
USE ST_USERS ORDER NUMSOL
SELECT st_isrep
STORE SPACE(4) TO w_emiini,  ;
      w_emifin, w_garini,  ;
      w_garfin
STORE SPACE(5) TO w_usuini,  ;
      w_usufin
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
STORE DATE() TO w_fecini,  ;
      w_fecfin
@ 03, 2 TO 9, 77
DO esc_modo WITH 'S'
@ 04, 30 SAY SPACE(30)
DO WHILE .T.
     @ 07, 00 CLEAR TO 13, 77
     @ 03, 00 TO 10, 77
     @ 04, 30 SAY SPACE(30)
     @ 04, 05 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 01 SAY 'Del d¡a    :'  ;
       GET w_fecini
     @ 04, 38 SAY 'Al  d¡a    :'  ;
       GET w_fecfin RANGE  ;
       w_fecini
     @ 05, 01 SAY 'De Usuario :'  ;
       GET w_usuini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 05, 38 SAY 'A  Usuario :'  ;
       GET w_usufin RANGE  ;
       w_usuini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 06, 01 SAY 'De Emisor  :'  ;
       GET w_emiini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 06, 38 SAY 'A  Emisor  :'  ;
       GET w_emifin RANGE  ;
       w_emiini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 07, 01 SAY 'De Gt¡a.   :'  ;
       GET w_garini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 07, 38 SAY 'A  Gt¡a.   :'  ;
       GET w_garfin RANGE  ;
       w_garini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 08, 01 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'
     @ 09, 01 SAY  ;
       'Por Detalle/Res£men:' GET  ;
       tipo PICTURE  ;
       '@m Detalle,Res£men'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     CREATE CURSOR ing (numdoc C  ;
            (8), fecemi D, horemi  ;
            C (8), codemi C (4),  ;
            codmar C (4), codmod  ;
            C (15), numser C (20),  ;
            codent C (11), indori  ;
            C (4), indest C (4),  ;
            coddes C (1), codusu  ;
            C (5), feccom D)
     INDEX ON codusu +  ;
           DTOS(fecemi) TAG  ;
           codigo
     SELECT st_isrep
     SET NEAR ON
     SEEK dtoc2(w_fecini)
     SET NEAR OFF
     SCAN WHILE fecemi <=  ;
          w_fecfin .AND.  .NOT.  ;
          EOF()
          IF (st_isrep.codemi >=  ;
             w_emiini .AND.  ;
             st_isrep.codemi <=  ;
             w_emifin) .AND.  ;
             indest <> 'N' .AND.  ;
             (st_isrep.indori >=  ;
             w_garini .AND.  ;
             st_isrep.indori <=  ;
             w_garfin)
               SELECT st_users
               SEEK st_isrep.numdoc
               IF  .NOT. FOUND()
                    LOOP
               ENDIF
               IF st_users.codemp >=  ;
                  w_usuini .AND.  ;
                  st_users.codemp <=  ;
                  w_usufin
                    SELECT ing
                    APPEND BLANK
                    REPLACE numdoc  ;
                            WITH  ;
                            st_isrep.numdoc,  ;
                            fecemi  ;
                            WITH  ;
                            st_isrep.fecemi
                    REPLACE horemi  ;
                            WITH  ;
                            st_isrep.horemi,  ;
                            codmar  ;
                            WITH  ;
                            st_isrep.codmar
                    REPLACE codmod  ;
                            WITH  ;
                            st_isrep.codmod,  ;
                            numser  ;
                            WITH  ;
                            st_isrep.numser
                    REPLACE codemi  ;
                            WITH  ;
                            st_isrep.codemi,  ;
                            indest  ;
                            WITH  ;
                            st_isrep.indest
                    REPLACE indori  ;
                            WITH  ;
                            st_isrep.indori,  ;
                            coddes  ;
                            WITH  ;
                            st_isrep.coddes
                    REPLACE codent  ;
                            WITH  ;
                            st_isrep.codent,  ;
                            codusu  ;
                            WITH  ;
                            st_users.codemp
                    REPLACE feccom  ;
                            WITH  ;
                            st_isrep.feccom
               ENDIF
          ENDIF
          SELECT st_isrep
     ENDSCAN
     SELECT ing
     GOTO TOP
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          SET DEVICE TO PRINTER
          SET PRINTER ON
          ?? CHR(27) + CHR(15)
          IF tipo = 'Res£men'
               REPORT FORMAT  ;
                      porl0429  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      porl0429 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          SET PRINTER TO
          SET PRINTER OFF
          SET DEVICE TO SCREEN
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          filtxt = SYS(3) +  ;
                   '.TXT'
          IF tipo = 'Res£men'
               repo form porl0429 to file;
&filtxt noconsole summary
          ELSE
               repo form porl0429 to file;
&filtxt noconsole 
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &filtxt noedit window;
pantall 
          dele file &filtxt
          SET SYSMENU OFF
     ENDIF
ENDDO
DO sacawin
RETURN
*
FUNCTION valida
PARAMETER opc
DO CASE
     CASE VARREAD() = 'W_FECINI'  ;
          .OR. VARREAD() =  ;
          'W_FECFIN'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
     CASE opc = 'W_USUINI' .OR.  ;
          opc = 'W_USUFIN'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          SELECT gc_vnd00
          IF opc = 'W_USUINI'
               SEEK 'A' +  ;
                    w_usuini
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '*** Usuario No Existe ***'
                    RETURN .F.
               ELSE
                    @ 05, 20 SAY  ;
                      SUBSTR(vnd_nombre,  ;
                      1, 18)
               ENDIF
          ELSE
               SEEK 'A' +  ;
                    w_usufin
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '*** Usuario No Existe ***'
                    RETURN .F.
               ELSE
                    @ 05, 57 SAY  ;
                      SUBSTR(vnd_nombre,  ;
                      1, 17)
               ENDIF
          ENDIF
     CASE opc = 'W_EMIINI' .OR.  ;
          opc = 'W_EMIFIN'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          SELECT ge_tab0
          IF opc = 'W_EMIINI'
               SEEK 'EMIS' +  ;
                    w_emiini
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '*** C¢digo de Tabla No Existe ***'
                    RETURN .F.
               ELSE
                    @ 06, 20 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 18)
               ENDIF
          ELSE
               SEEK 'EMIS' +  ;
                    w_emifin
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '*** C¢digo de Tabla No Existe ***'
                    RETURN .F.
               ELSE
                    @ 06, 57 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 17)
               ENDIF
          ENDIF
     CASE opc = 'W_GARINI' .OR.  ;
          opc = 'W_GARFIN'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          SELECT ge_tab0
          IF opc = 'W_GARINI'
               SEEK 'INGA' +  ;
                    w_garini
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '*** C¢digo de Tabla No Existe ***'
                    RETURN .F.
               ELSE
                    @ 07, 20 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 18)
               ENDIF
          ELSE
               SEEK 'INGA' +  ;
                    w_garfin
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '*** C¢digo de Tabla No Existe ***'
                    RETURN .F.
               ELSE
                    @ 07, 57 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 17)
               ENDIF
          ENDIF
ENDCASE
RETURN
*
PROCEDURE ayuda
PARAMETER opc
DO CASE
     CASE VARREAD() = 'W_USUINI'  ;
          .OR. VARREAD() =  ;
          'W_USUFIN'
     CASE VARREAD() = 'W_EMIINI'  ;
          .OR. VARREAD() =  ;
          'W_EMIFIN'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'EMIS'
          GOTO TOP
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE EMISOR'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'ESC'
          SET FILTER TO
     CASE VARREAD() = 'W_GARINI'  ;
          .OR. VARREAD() =  ;
          'W_GARFIN'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'INGA'
          GOTO TOP
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE GARANTIA'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'ESC'
          SET FILTER TO
ENDCASE
RETURN
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 'W_USUINI' .OR.  ;
          opc = 'W_USUFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda
     CASE opc = 'W_EMIINI' .OR.  ;
          opc = 'W_EMIFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda
     CASE opc = 'W_GARINI' .OR.  ;
          opc = 'W_GARFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda
     CASE opc = 'OUTPUT'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6
ENDCASE
RETURN
*
FUNCTION usuario
PARAMETER opc2
SELECT gc_vnd00
SEEK 'A' + opc2
IF  .NOT. FOUND()
     RETURN SPACE(20)
ELSE
     RETURN vnd_nombre
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

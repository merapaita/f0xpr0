*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' PENDIENTES POR USUARIO '
SELECT 1
USE ST_USERS ORDER NUMSOL
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE GC_VND00 ORDER CODIGO
SELECT 4
USE ST_IOREP ORDER ORD_ESEM
SELECT 5
USE ST_ICLPR ORDER CODIGO
SELECT 6
USE ST_ISREP ORDER CODIGO
STORE SPACE(5) TO w_usuini,  ;
      w_usufin
STORE SPACE(4) TO w_estini,  ;
      w_estfin, w_emiini,  ;
      w_emifin, w_talini,  ;
      w_talfin, w_garini,  ;
      w_garfin
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE SPACE(8) TO w_numdoc
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
@ 3, 0 TO 11, 77
DO esc_modo WITH 'S'
@ 04, 30 SAY SPACE(30)
sigue = .T.
DO WHILE sigue
     @ 07, 01 CLEAR TO 13, 77
     @ 3, 0 TO 12, 77
     @ 04, 30 SAY SPACE(30)
     @ 04, 05 SAY SPACE(50)
     STORE 0 TO nco, w_reg
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 01 SAY 'De  Fecha   :'  ;
       GET w_fecini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 04, 39 SAY 'Al Fecha   :'  ;
       GET w_fecfin RANGE  ;
       w_fecini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 05, 01 SAY 'Del Usuario :'  ;
       GET w_usuini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 05, 39 SAY 'Al Usuario :'  ;
       GET w_usufin RANGE  ;
       w_usuini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 06, 01 SAY 'Del Estado  :'  ;
       GET w_estini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 06, 39 SAY 'Al Estado  :'  ;
       GET w_estfin RANGE  ;
       w_estini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 07, 01 SAY 'Del Emisor  :'  ;
       GET w_emiini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 07, 39 SAY 'Al Emisor  :'  ;
       GET w_emifin RANGE  ;
       w_emiini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 08, 01 SAY 'Del Taller  :'  ;
       GET w_talini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 08, 39 SAY 'Al Taller  :'  ;
       GET w_talfin RANGE  ;
       w_talini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 09, 01 SAY 'De  Gt¡a.   :'  ;
       GET w_garini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 09, 39 SAY 'A  Gt¡a.   :'  ;
       GET w_garfin RANGE  ;
       w_garini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 10, 01 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'  ;
       WHEN antes(2)
     @ 11, 01 SAY  ;
       'Por Detalle/Res£men:' GET  ;
       tipo PICTURE  ;
       '@m Detalle,Res£men'
     READ
     IF LASTKEY() = 27
          sigue = .F.
          LOOP
     ENDIF
     IF output = 'Impresora'
          nco = 1
          @ 09, 38 SAY  ;
            'Copias Nro. :' GET  ;
            nco PICTURE '99'  ;
            VALID nco >= 0
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     CREATE CURSOR PENUSU (numdoc  ;
            C (8), fecemi D,  ;
            codemi C (4), codent  ;
            C (11), codmar C (4),  ;
            indori C (4), estado  ;
            C (4), codmod C (15),  ;
            numser C (20), telcli  ;
            N (8), fecest D,  ;
            codtall C (4),  ;
            usuario C (5), feccom  ;
            D)
     INDEX ON usuario + estado +  ;
           DTOS(fecemi) TAG  ;
           codigo
     SELECT st_iorep
     SET NEAR ON
     SEEK w_estini
     SET NEAR OFF
     w_reg = 0
     SCAN WHILE auxest <=  ;
          w_estfin .AND.  .NOT.  ;
          EOF()
          IF indest <> 'N   '
               IF codemi >=  ;
                  w_emiini .AND.  ;
                  codemi <=  ;
                  w_emifin
                    IF codtall >=  ;
                       w_talini  ;
                       .AND.  ;
                       codtall <=  ;
                       w_talfin
                         IF fecemi >=  ;
                            w_fecini  ;
                            .AND.  ;
                            fecemi <=  ;
                            w_fecfin
                              IF indori >=  ;
                                 w_garini  ;
                                 .AND.  ;
                                 indori <=  ;
                                 w_garfin
                                   w_numdoc = numsol
                                   SELECT st_users
                                   SEEK w_numdoc
                                   IF FOUND()
                                        IF (codemp >= w_usuini .AND. codemp <= w_usufin)
                                             SELECT st_isrep
                                             SEEK w_numdoc
                                             IF FOUND()
                                                  w_feccom = feccom
                                             ENDIF
                                             SELECT penusu
                                             APPEND BLANK
                                             REPLACE numdoc WITH st_iorep.numdoc, fecemi WITH st_isrep.fecemi
                                             REPLACE codemi WITH st_iorep.codemi, indori WITH st_iorep.indori
                                             REPLACE codent WITH st_iorep.codent, codmar WITH st_iorep.codmar
                                             REPLACE codmod WITH st_iorep.codmod, estado WITH st_iorep.auxest
                                             REPLACE fecest WITH st_iorep.fecest, codtall WITH st_iorep.codtall
                                             REPLACE numser WITH st_iorep.numser, usuario WITH st_users.codemp
                                             REPLACE feccom WITH w_feccom
                                             w_reg = w_reg + 1
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SELECT penusu
     IF w_reg = 0
          DO error WITH  ;
             '*** No hay informaci¢n para el Reporte ***'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
     ELSE
          filidx = SYS(3)
          SELECT penusu
          GOTO TOP
          IF output = 'Impresora'
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               SET DEVICE TO PRINTER
               SET PRINTER ON
               ?? CHR(27) +  ;
                  CHR(15)
               FOR i = 1 TO nco
                    IF tipo =  ;
                       'Res£men'
                         REPORT FORMAT  ;
                                porl0428  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                porl0428  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDFOR
               ?? CHR(27) +  ;
                  CHR(18)
               SET PRINTER OFF
               SET DEVICE TO SCREEN
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ELSE
               DO mensa WITH  ;
                  '***** C o p i a n d o ... *****',  ;
                  'COLO'
               filtxt = SYS(3) +  ;
                        '.TXT'
               IF tipo =  ;
                  'Res£men'
                    repo form porl0428;
to file &FILTXT noconsole summary
               ELSE
                    repo form porl0428;
to file &FILTXT noconsole 
               ENDIF
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               modi comm &FILTXT;
 window pantall NOEDIT
               DELE FILE &FILTXT
               SET SYSMENU OFF
          ENDIF
     ENDIF
ENDDO
DO sacawin
CLOSE DATABASES
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
     CASE VARREAD() = 'W_USUINI'  ;
          .OR. VARREAD() =  ;
          'W_USUFIN'
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
     CASE VARREAD() = 'W_ESTINI'  ;
          .OR. VARREAD() =  ;
          'W_ESTFIN'
          SELECT ge_tab0
          IF VARREAD() =  ;
             'W_ESTINI'
               SEEK 'ESOR' +  ;
                    w_estini
          ELSE
               SEEK 'ESOR' +  ;
                    w_estfin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          IF VARREAD() =  ;
             'W_ESTINI'
               @ 06, 20 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 18)
          ELSE
               @ 06, 57 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 17)
          ENDIF
     CASE VARREAD() = 'W_EMIINI'  ;
          .OR. VARREAD() =  ;
          'W_EMIFIN'
          SELECT ge_tab0
          IF VARREAD() =  ;
             'W_EMIINI'
               SEEK 'EMIS' +  ;
                    w_emiini
          ELSE
               SEEK 'EMIS' +  ;
                    w_emifin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          IF VARREAD() =  ;
             'W_EMIINI'
               @ 07, 20 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 18)
          ELSE
               @ 07, 57 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 17)
          ENDIF
     CASE VARREAD() = 'W_TALINI'  ;
          .OR. VARREAD() =  ;
          'W_TALFIN'
          SELECT ge_tab0
          IF VARREAD() =  ;
             'W_TALINI'
               SEEK 'TALL' +  ;
                    w_talini
          ELSE
               SEEK 'TALL' +  ;
                    w_talfin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          IF VARREAD() =  ;
             'W_TALINI'
               @ 08, 20 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 18)
          ELSE
               @ 08, 57 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 17)
          ENDIF
     CASE VARREAD() = 'W_GARINI'  ;
          .OR. VARREAD() =  ;
          'W_GARFIN'
          SELECT ge_tab0
          IF VARREAD() =  ;
             'W_GARINI'
               SEEK 'INGA' +  ;
                    w_garini
          ELSE
               SEEK 'INGA' +  ;
                    w_garfin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          IF VARREAD() =  ;
             'W_GARINI'
               @ 09, 20 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 18)
          ELSE
               @ 09, 57 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 17)
          ENDIF
ENDCASE
RETURN
*
PROCEDURE ayuda
DO CASE
     CASE VARREAD() = 'W_USUINI'  ;
          .OR. VARREAD() =  ;
          'W_USUFIN'
     CASE VARREAD() = 'W_ESTINI'  ;
          .OR. VARREAD() =  ;
          'W_ESTFIN'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'ESOR'
          titulo = 'AYUDA DE TABLAS'
          campo = 'tab_codtab + "  " + tab_destab'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'ESC'
     CASE VARREAD() = 'W_EMIINI'  ;
          .OR. VARREAD() =  ;
          'W_EMIFIN'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'EMIS'
          titulo = 'AYUDA DE TABLAS'
          campo = 'tab_codtab + "  " + tab_destab'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'ESC'
     CASE VARREAD() = 'W_TALINI'  ;
          .OR. VARREAD() =  ;
          'W_TALFIN'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'TALL'
          titulo = 'AYUDA DE TABLAS'
          campo = 'tab_codtab + "  " + tab_destab'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'ESC'
     CASE VARREAD() = 'W_GARINI'  ;
          .OR. VARREAD() =  ;
          'W_GARFIN'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'INGA'
          titulo = 'AYUDA DE TABLAS'
          campo = 'tab_codtab + "  " + tab_destab'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
RETURN
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE VARREAD() = 'W_USUINI'  ;
          .OR. VARREAD() =  ;
          'W_USUFIN'
          ON KEY LABEL F6 DO ENTIDAD
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE VARREAD() = 'W_ESTINI'  ;
          .OR. VARREAD() =  ;
          'W_ESTFIN'
          ON KEY LABEL F6 DO AYUDA
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE VARREAD() = 'W_EMIINI'  ;
          .OR. VARREAD() =  ;
          'W_EMIFIN'
          ON KEY LABEL F6 DO AYUDA
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE VARREAD() = 'W_TALINI'  ;
          .OR. VARREAD() =  ;
          'W_TALFIN'
          ON KEY LABEL F6 DO AYUDA
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE VARREAD() = 'W_GARINI'  ;
          .OR. VARREAD() =  ;
          'W_GARFIN'
          ON KEY LABEL F6 DO AYUDA
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE VARREAD() = 'OUTPUT'
          ON KEY LABEL F6 DO AYUDA
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
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
FUNCTION entidad
PARAMETER w_var
ON KEY
DEFINE WINDOW cliente FROM 08, 20  ;
       TO 18, 63 TITLE  ;
       ' B£squeda de Vendedore/ADministrativos '  ;
       IN screen FOOTER  ;
       ' [F6] B£squeda R pida '  ;
       COLOR SCHEME 10
DEFINE WINDOW b_clien FROM 12, 30  ;
       TO 14, 53 TITLE ' Nombre '  ;
       IN screen
= ooopen('gc_vnd00',2)
SET ORDER TO 2
SET FILTER TO vnd_tpper = 'A'
GOTO TOP
ACTIVATE WINDOW cliente
ON KEY LABEL ENTER DO CARGA_COD
ON KEY LABEL F6 DO BUSCA_COD
BROWSE FIELDS vnd_code :R :H =  ;
       'C¢digo', vnd_nombre :R :H =  ;
       '  Nombre / Raz¢n Social  '  ;
       IN cliente
ON KEY
SET FILTER TO
RELEASE WINDOW cliente
RETURN w_var
*
FUNCTION carga_cod
ON KEY
w_var = vnd_code
RELEASE WINDOW cliente
SET ORDER TO 1
KEYBOARD CHR(13)
RETURN w_var
*
PROCEDURE busca_cod
ON KEY
ACTIVATE WINDOW b_clien
c_cli = SPACE(25)
@ 0, 0 GET c_cli PICTURE '@!'
READ
SET NEAR ON
SEEK c_cli
SET NEAR OFF
DEACTIVATE WINDOW b_clien
ACTIVATE WINDOW cliente
ON KEY LABEL F6 DO BUSCA_COD
ON KEY LABEL ENTER DO CARGA_COD
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

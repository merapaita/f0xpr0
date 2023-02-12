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
   ' INGRESO DE ORDENES POR TALLER '
wrk_dia = DATE()
SELECT 1
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 2
USE ST_IOREP ORDER ORD_FCHEMI
SELECT 3
USE SHARED ST_ICLPR ORDER CODIGO
SELECT 4
USE ST_ISERI ORDER SER_CODMAR
SELECT 2
STORE DATE() TO wrk_dia, wrk_dia2
wrk_copia = 1
STORE SPACE(4) TO wrk_des1,  ;
      wrk_des2, w_emiini,  ;
      w_emifin, w_garini,  ;
      w_garfin
output = 'Impresora'
STORE 'Detalle' TO tipo
ON KEY LABEL F6 DO AYUDA
DO WHILE .T.
     @ 7, 1 CLEAR TO 13, 77
     @ 4, 30 SAY SPACE(30)
     @ 3, 2 TO 10, 77
     @ 4, 5 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 04 SAY 'Del Emisor :'
     @ 04, 40 SAY 'Al Emisor :'
     @ 05, 04 SAY 'Del Taller :'
     @ 05, 40 SAY 'Al Taller :'
     @ 06, 04 SAY 'De  Gt¡a.  :'
     @ 06, 40 SAY 'A  Gt¡a.  :'
     @ 07, 04 SAY 'Del        :'
     @ 07, 40 SAY 'Al        :'
     @ 08, 04 SAY  ;
       'Por Detalle/Res£men   :'
     @ 09, 04 SAY  ;
       'Por Pantalla/Impresora:'
     SET CURSOR ON
     @ 04, 17 GET w_emiini  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 04, 52 GET w_emifin  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 05, 17 GET wrk_des1  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 05, 52 GET wrk_des2  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 06, 17 GET w_garini  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 06, 52 GET w_garfin  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 07, 17 GET wrk_dia WHEN  ;
       antes(VARREAD())
     @ 07, 52 GET wrk_dia2 RANGE  ;
       wrk_dia
     @ 08, 28 GET tipo PICTURE  ;
       '@m Detalle,Res£men'
     @ 09, 28 GET output PICTURE  ;
       '@m Pantalla,Impresora'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     IF output = 'Impresora'
          @ 09, 50 SAY 'Copias:'
          @ 09, 58 GET wrk_copia  ;
            PICTURE '99'
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
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
            codtall C (4), numsol  ;
            C (8), infor C  ;
            (120))
     INDEX ON codtall + codemi +  ;
           DTOS(fecemi) TO  ;
           codigo
     SELECT st_iorep
     SET NEAR ON
     SEEK dtoc2(wrk_dia)
     SET NEAR OFF
     SCAN WHILE fecemi <=  ;
          wrk_dia2 .AND.  .NOT.  ;
          EOF()
          IF (st_iorep.codemi >=  ;
             w_emiini .AND.  ;
             st_iorep.codemi <=  ;
             w_emifin) .AND.  ;
             indest <> 'N'
               IF st_iorep.codtall >=  ;
                  wrk_des1 .AND.  ;
                  st_iorep.codtall <=  ;
                  wrk_des2
                    IF st_iorep.indori >=  ;
                       w_garini  ;
                       .AND.  ;
                       st_iorep.indori <=  ;
                       w_garfin
                         SELECT ing
                         APPEND BLANK
                         REPLACE numdoc  ;
                                 WITH  ;
                                 st_iorep.numdoc,  ;
                                 fecemi  ;
                                 WITH  ;
                                 st_iorep.fecemi,  ;
                                 horemi  ;
                                 WITH  ;
                                 st_iorep.horemi,  ;
                                 codmar  ;
                                 WITH  ;
                                 st_iorep.codmar
                         REPLACE codmod  ;
                                 WITH  ;
                                 st_iorep.codmod,  ;
                                 numser  ;
                                 WITH  ;
                                 st_iorep.numser,  ;
                                 codemi  ;
                                 WITH  ;
                                 st_iorep.codemi,  ;
                                 indest  ;
                                 WITH  ;
                                 st_iorep.indest
                         REPLACE indori  ;
                                 WITH  ;
                                 st_iorep.indori,  ;
                                 codent  ;
                                 WITH  ;
                                 st_iorep.codent,  ;
                                 codtall  ;
                                 WITH  ;
                                 st_iorep.codtall,  ;
                                 infor  ;
                                 WITH  ;
                                 SUBSTR(st_iorep.observ,  ;
                                 1,  ;
                                 120)
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SELECT ing
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     COUNT TO wrk_valor
     IF wrk_valor = 0
          DO error WITH  ;
             'No Existen registros a Listar'
          LOOP
     ENDIF
     IF output = 'Impresora'
          ??? CHR(15)
          FOR a = 1 TO wrk_copia
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               IF tipo =  ;
                  'Detalle'
                    REPORT FORMAT  ;
                           porl4135  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           porl4135  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDFOR
          SET PRINTER TO
     ELSE
          IF tipo = 'Detalle'
               DO mensa WITH  ;
                  '*** C o p i a n d o ... ***',  ;
                  'COLO'
               REPORT FORMAT  ;
                      porl4135 TO  ;
                      FILE  ;
                      TEXT5.TXT  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      porl4135  ;
                      SUMMARY TO  ;
                      FILE  ;
                      TEXT5.TXT  ;
                      NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODIFY COMMAND  ;
                 text5.txt NOEDIT  ;
                 WINDOW pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
RETURN
*
FUNCTION valida
PARAMETER w_codtab
DO CASE
     CASE w_codtab = 'W_EMIINI'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + w_emiini
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Emisor No Existe ***'
               RETURN .F.
          ENDIF
          @ 04, 22 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
     CASE w_codtab = 'W_EMIFIN'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + w_emifin
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Emisor No Existe ***'
               RETURN .F.
          ENDIF
          @ 04, 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
     CASE w_codtab = 'WRK_DES1'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          SEEK 'TALL' + wrk_des1
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Taller No Existe ***'
               RETURN .F.
          ENDIF
          @ 05, 22 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
     CASE w_codtab = 'WRK_DES2'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          SEEK 'TALL' + wrk_des2
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Taller No Existe ***'
               RETURN .F.
          ENDIF
          @ 05, 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
     CASE w_codtab = 'W_GARINI'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          SELECT ge_tab0
          SEEK 'INGA' + w_garini
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** Tipo de Garant¡a No Existe ***'
               RETURN .F.
          ENDIF
          @ 06, 22 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
     CASE w_codtab = 'W_GARFIN'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          SEEK 'INGA' + w_garfin
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** Tipo de Garant¡a No Existe ***'
               RETURN .F.
          ENDIF
          @ 06, 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
ENDCASE
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT ge_tab0
DO CASE
     CASE opc = 1
          SET FILTER TO tab_codpre ==;
'TALL'
     CASE opc = 2
          SET FILTER TO tab_codpre ==;
'EMIS'
     CASE opc = 3
          SET FILTER TO tab_codpre ==;
'INGA'
ENDCASE
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE TABLAS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'ESC'
RETURN
*
PROCEDURE antes
PARAMETER w_opc
DO CASE
     CASE w_opc = 'W_EMIINI' .OR.  ;
          w_opc = 'W_EMIFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA WITH;
2
     CASE w_opc = 'WRK_DES1' .OR.  ;
          w_opc = 'WRK_DES2'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA WITH;
1
     CASE w_opc = 'W_GARINI' .OR.  ;
          w_opc = 'W_GARFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA WITH;
3
     CASE w_opc = 'WRK_DIA'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

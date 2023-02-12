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
   ' INGRESO DE ORDENES POR EMISOR-TALLER-FECHA '
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER  ;
    ord_fchemi
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
USE SHARED st_iseri ORDER  ;
    ser_codmar
SELECT st_iorep
STORE SPACE(4) TO emisor1,  ;
      emisor2, w_talini, w_talfin,  ;
      w_garini, w_garfin
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
STORE DATE() TO fecha1, fecha2
ON KEY LABEL f6 do ayuda
DO esc_modo WITH 'S'
@ 04, 30 SAY SPACE(30)
ON KEY
DO WHILE .T.
     @ 07, 00 CLEAR TO 10, 77
     @ 03, 00 TO 10, 77
     @ 04, 30 SAY SPACE(30)
     @ 04, 05 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 01 SAY  ;
       'Desde Fecha  :' GET  ;
       fecha1
     @ 04, 37 SAY  ;
       'Hasta fecha  :' GET  ;
       fecha2 RANGE fecha1
     @ 05, 01 SAY  ;
       'Desde Emisor :' GET  ;
       emisor1 PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 05, 37 SAY  ;
       'Hasta Emisor :' GET  ;
       emisor2 RANGE emisor1  ;
       PICTURE '@1' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 06, 01 SAY  ;
       'Desde Taller :' GET  ;
       w_talini VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 06, 37 SAY  ;
       'Hasta Taller :' GET  ;
       w_talfin RANGE w_talini  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 07, 01 SAY  ;
       'Desde Gtia.  :' GET  ;
       w_garini PICTURE '@!'  ;
       VALID valida(VARREAD())  ;
       WHEN antes(VARREAD())
     @ 07, 37 SAY  ;
       'Hasta Gtia.  :' GET  ;
       w_garfin RANGE w_garini  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
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
     SELECT 4
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
     INDEX ON codemi + codtall +  ;
           DTOS(fecemi) TO  ;
           codigo
     SELECT st_iorep
     SET NEAR ON
     SEEK dtoc2(fecha1)
     SET NEAR OFF
     SCAN WHILE fecemi <= fecha2  ;
          .AND.  .NOT. EOF()
          IF (st_iorep.codemi >=  ;
             emisor1 .AND.  ;
             st_iorep.codemi <=  ;
             emisor2) .AND.  ;
             indest <> 'N'
               IF st_iorep.codtall >=  ;
                  w_talini .AND.  ;
                  st_iorep.codtall <=  ;
                  w_talfin
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
                      porl6000  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      porl6000 TO  ;
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
               repo form porl6000 to file;
&filtxt noconsole summary
          ELSE
               repo form porl6000 to file;
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
PARAMETER w_codtab
DO CASE
     CASE w_codtab = 'EMISOR1'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + emisor1
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Emisor No Existe ***'
               RETURN .F.
          ENDIF
          @ 05, 21 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE w_codtab = 'EMISOR2'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + emisor2
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Emisor No Existe ***'
               RETURN .F.
          ENDIF
          @ 05, 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE w_codtab = 'W_TALINI'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          SEEK 'TALL' + w_talini
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Taller No Existe ***'
               RETURN .F.
          ENDIF
          @ 06, 21 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            16)
     CASE w_codtab = 'W_TALFIN'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          SEEK 'TALL' + w_talfin
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Taller No Existe ***'
               RETURN .F.
          ENDIF
          @ 06, 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            16)
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
          @ 07, 21 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
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
          @ 07, 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            16)
ENDCASE
RETURN
*
PROCEDURE ayuda
PARAMETER opc1
SELECT ge_tab0
DO CASE
     CASE opc1 = 1
          SET FILTER TO tab_codpre ==;
'EMIS'
     CASE opc1 = 2
          SET FILTER TO tab_codpre ==;
'TALL'
     CASE opc1 = 3
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
PARAMETER opc
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BUS', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
DO CASE
     CASE opc = 'EMISOR1' .OR.  ;
          opc = 'EMISOR2'
          ON KEY LABEL F6 DO AYUDA WITH;
1
     CASE opc = 'W_TALINI' .OR.  ;
          opc = 'W_TALFIN'
          ON KEY LABEL F6 DO AYUDA WITH;
2
     CASE opc = 'W_GARINI' .OR.  ;
          opc = 'W_GARFIN'
          ON KEY LABEL F6 DO AYUDA WITH;
3
     CASE opc = 'FECHA1' .OR. opc =  ;
          'FECHA2'
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

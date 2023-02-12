*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = ' REPORTE '
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' SALDO ACTUAL DE ARTICULOS '
CLOSE DATABASES
SELECT 1
USE ST_IOREP
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE ST_ICLPR ORDER CODIGO
STORE 1 TO w_opcion, w_salida,  ;
      w_copia, w_tipo
DO WHILE .T.
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 07, 01 CLEAR TO 15, 77
     @ 04, 01 SAY  ;
       'Tipo           :' SIZE 01,  ;
       16, 0
     @ 07, 01 SAY  ;
       'Tipo de Salida :' SIZE 01,  ;
       16, 0
     SET CURSOR ON
     @ 04, 18 GET w_tipo PICTURE  ;
       '@*RVN Res£men;Detalle'
     @ 07, 18 GET w_salida  ;
       PICTURE  ;
       '@*RVTN Pantalla;Impresora'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     IF w_salida = 2
          @ 08, 40 SAY  ;
            'Copias : ' GET  ;
            w_copia VALID  .NOT.  ;
            EMPTY(w_copia)
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT 4
     CREATE CURSOR saldos  ;
            (tnumord C (8),  ;
            tfecdoc D, tcodtal C  ;
            (4), tindori C (4),  ;
            tcodemi C (4),  ;
            tfecest D, tcodest C  ;
            (4), tindest C (1),  ;
            tcodtab C (1),  ;
            tcodmar C (4),  ;
            tcodmod C (15),  ;
            tnumser C (20),  ;
            tcodent C (11))
     INDEX ON tcodtab + tcodest +  ;
           tindori + tcodemi TAG  ;
           codigo
     a = 0
     b = 0
     SELECT st_iorep
     SCAN WHILE  .NOT. EOF()
          a = a + 1
          @ 00, 00 SAY a
          IF indest <> 'N   '
               IF codtall <  ;
                  '010 ' .OR.  ;
                  (codtall >  ;
                  '050 ' .AND.  ;
                  codtall <  ;
                  '060 ')
                    w_estado = auxest
                    SELECT ge_tab0
                    SEEK 'ESOR' +  ;
                         w_estado
                    IF SUBSTR(tab_parame,  ;
                       1) = 'T'  ;
                       .OR.  ;
                       SUBSTR(tab_parame,  ;
                       1) = 'A'
                         w_codtab =  ;
                          SUBSTR(tab_parame,  ;
                          1)
                         b = b +  ;
                             1
                         @ 00, 40  ;
                           SAY b
                         SELECT st_iorep
                         w_numord =  ;
                          numdoc
                         w_fecdoc =  ;
                          fecemi
                         w_codtal =  ;
                          codtall
                         w_indori =  ;
                          indori
                         w_codemi =  ;
                          codemi
                         w_fecest =  ;
                          fecest
                         w_codest =  ;
                          auxest
                         w_indest =  ;
                          indest
                         w_codmar =  ;
                          codmar
                         w_codmod =  ;
                          codmod
                         w_numser =  ;
                          numser
                         w_codent =  ;
                          codent
                         SELECT saldos
                         APPEND BLANK
                         REPLACE tnumord  ;
                                 WITH  ;
                                 w_numord,  ;
                                 tfecdoc  ;
                                 WITH  ;
                                 w_fecdoc
                         REPLACE tcodtal  ;
                                 WITH  ;
                                 w_codtal,  ;
                                 tindori  ;
                                 WITH  ;
                                 w_indori
                         REPLACE tcodemi  ;
                                 WITH  ;
                                 w_codemi,  ;
                                 tfecest  ;
                                 WITH  ;
                                 w_fecest
                         REPLACE tcodest  ;
                                 WITH  ;
                                 w_codest,  ;
                                 tindest  ;
                                 WITH  ;
                                 w_indest
                         REPLACE tcodtab  ;
                                 WITH  ;
                                 w_codtab,  ;
                                 tcodmar  ;
                                 WITH  ;
                                 w_codmar
                         REPLACE tcodmod  ;
                                 WITH  ;
                                 w_codmod,  ;
                                 tnumser  ;
                                 WITH  ;
                                 w_numser
                         REPLACE tcodent  ;
                                 WITH  ;
                                 w_codent
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SELECT saldos
     COUNT TO w_valor
     IF w_valor = 0
          DO mensa WITH  ;
             '***  Un momento, Por Favor ...  ***',  ;
             'SACA'
          DO error WITH  ;
             '***  No Existen registros a Listar  ***'
          LOOP
     ENDIF
     DO mensa WITH  ;
        '***  Un momento, Por Favor ...  ***',  ;
        'SACA'
     DO esc_indica WITH 2, 'BBB',  ;
        'VER', 'IMP', 'ESC'
     IF w_salida = 2
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          FOR a = 1 TO w_copia
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               ??? CHR(18)
               IF w_tipo = 1
                    REPORT FORMAT  ;
                           PORL4022  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           PORL4023  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
               SET PRINTER TO
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDFOR
     ENDIF
     IF w_salida = 1
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'COLO'
          w_file02 = SUBSTR(f_archivo(),  ;
                     1, 8) +  ;
                     '.TXT'
          IF w_tipo = 1
               REPO FORM PORL4022 TO FILE;
 &w_file02  SUMMARY NOCONSOLE
          ELSE
               REPO FORM PORL4023 TO FILE;
 &w_file02  NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODI COMM &w_file02 NOEDIT WINDOW;
PANTALL
          SET SYSMENU OFF
          DELE FILE &w_file02
     ENDIF
ENDDO
DO saca_win
RETURN
*
PROCEDURE antes
PARAMETER opc1
DO CASE
     CASE opc1 = 1
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          ON KEY LABEL F6 DO AYUDA WITH;
1
     CASE opc1 = 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
     CASE opc1 = 3
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          ON KEY LABEL F6 DO AYUDA WITH;
2
     CASE opc1 = 4
          ON KEY LABEL F6 DO AYUDA WITH;
3
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

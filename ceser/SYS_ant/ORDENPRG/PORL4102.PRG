*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
SET SYSMENU ON
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'PROMEDIO DE SERVICIO POR TECNICOS'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BUS', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
SELECT 1
USE ge_tab0 ORDER codigo
SELECT 2
USE ST_IOREP ORDER ORD_FECIND
SELECT 3
USE ST_ITECN ORDER CODIGO
@ 07, 01 CLEAR TO 13, 77
@ 03, 02 TO 10, 77
DO esc_modo WITH 'S'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BUS', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
wk_key = 0
SET CURSOR ON
STORE DATE() TO fecha1, fecha2
STORE SPACE(4) TO w_tecini,  ;
      w_tecfin, w_talini,  ;
      w_talfin, w_garini,  ;
      w_garfin
STORE 0 TO w_tecini, w_tecfin
STORE 'Detalle' TO w_tipo
STORE 'w_salida' TO w_salida
@ 04, 04 SAY 'Del        :'
@ 04, 40 SAY 'Al         :'
@ 05, 04 SAY 'Del Täcnico:'
@ 05, 40 SAY 'Al  Täcnico:'
@ 06, 04 SAY 'Del Taller :'
@ 06, 40 SAY 'Al  Taller :'
@ 07, 04 SAY 'De Garantça:'
@ 07, 40 SAY 'A  Garantça:'
@ 08, 04 SAY 'Tipo       :'
@ 08, 40 SAY 'Salida     :'
@ 04, 17 GET fecha1
@ 04, 53 GET fecha2 RANGE fecha1
@ 05, 17 GET w_tecini PICTURE  ;
  '@!' VALID valida(VARREAD())  ;
  WHEN antes(VARREAD())
@ 05, 53 GET w_tecfin RANGE  ;
  w_tecini PICTURE '999999999'  ;
  VALID valida(VARREAD()) WHEN  ;
  antes(VARREAD())
@ 06, 17 GET w_talini PICTURE  ;
  '999999999' VALID  ;
  valida(VARREAD()) WHEN  ;
  antes(VARREAD())
@ 06, 53 GET w_talfin RANGE  ;
  w_talini PICTURE '@!' VALID  ;
  valida(VARREAD()) WHEN  ;
  antes(VARREAD())
@ 07, 17 GET w_garini PICTURE  ;
  '@!' VALID valida(VARREAD())  ;
  WHEN antes(VARREAD())
@ 07, 53 GET w_garfin RANGE  ;
  w_garini PICTURE '@!' VALID  ;
  valida(VARREAD()) WHEN  ;
  antes(VARREAD())
@ 08, 17 GET w_tipo DEFAULT 1  ;
  SIZE 1, 11, 0 PICTURE  ;
  '@*RVTN Detalle;Resumen' WHEN  ;
  antes(VARREAD())
@ 08, 53 GET w_salida DEFAULT 1  ;
  SIZE 1, 11, 0 PICTURE  ;
  '@*RVTN Pantalla;Impresora'  ;
  WHEN antes(VARREAD())
READ
IF LASTKEY() = 27
     DO saca_win
     RETURN
ENDIF
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'COLO'
SELECT 10
CREATE CURSOR tmp (orden C (8),  ;
       rpto N (10, 2), mobra N  ;
       (10, 2), tecnico C (9),  ;
       gara C (4), emisor C (4),  ;
       taller C (4))
INDEX ON tecnico + taller + gara +  ;
      emisor TAG codigo
SET ORDER TO codigo
SET NEAR ON
SELECT st_iorep
SEEK DTOS(fecha1)
DO WHILE fecfin<=fecha2 .AND.   ;
   .NOT. EOF()
     IF (STR(w_tecini, 9) >=  ;
        codtec .AND. STR(w_tecfin,  ;
        9) <= codtec)
          w_numdoc = numdoc
          w_rpto = cosrep
          w_mobra = cosmob
          w_tecnico = codtec
          w_gara = indori
          w_emisor = codemi
          SELECT tmp
          APPEND BLANK
          REPLACE orden WITH  ;
                  w_numdoc
          REPLACE rpto WITH  ;
                  w_rpto
          REPLACE mobra WITH  ;
                  w_mobra
          REPLACE tecnico WITH  ;
                  w_tecnico
          REPLACE gara WITH  ;
                  w_gara
          REPLACE emisor WITH  ;
                  w_emisor
     ENDIF
     SELECT st_iorep
     SKIP
ENDDO
SELECT tmp
IF w_salida = 'Impresora'
     ??? CHR(15)
     IF w_tipo = ''
          REPORT FORMAT PORL4102  ;
                 TO PRINTER  ;
                 NOCONSOLE
     ELSE
          REPORT FORMAT PORL4102  ;
                 SUMMARY TO  ;
                 PRINTER  ;
                 NOCONSOLE
     ENDIF
ELSE
     SET SYSMENU ON
     IF w_tipo = ''
          REPORT FORMAT PORL4102  ;
                 TO FILE  ;
                 tecn.doc
          MODIFY COMMAND tecn.doc
     ELSE
          REPORT FORMAT PORL4102  ;
                 SUMMARY TO FILE  ;
                 tecn.doc
          MODIFY COMMAND tecn.doc
     ENDIF
     DELETE FILE tecn.doc
ENDIF
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'SACA'
CLOSE DATABASES
DO saca_win
RETURN
*
PROCEDURE antes
PARAMETER opc
IF VARREAD() = 'W_TECINI' .OR.  ;
   VARREAD() = 'W_TECFIN' .OR.  ;
   VARREAD() = 'W_TALINI' .OR.  ;
   VARREAD() = 'W_TALFIN' .OR.  ;
   VARREAD() = 'W_GARINI' .OR.  ;
   VARREAD() = 'W_GARFIN'
     ON KEY LABEL f6 do ayuda
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
ELSE
     ON KEY LABEL f6
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
ENDIF
RETURN
*
FUNCTION valida
PARAMETER opc
DO CASE
     CASE VARREAD() = 'W_TECINI'  ;
          .OR. VARREAD() =  ;
          'W_TECFIN'
          SELECT st_itecn
          IF VARREAD() =  ;
             'W_TECINI'
               SEEK STR(w_tecini,  ;
                    9)
          ELSE
               SEEK STR(w_tecfin,  ;
                    9)
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
ENDCASE
IF  .NOT. FOUND()
     DO error WITH  ;
        'C¢digo No Existe'
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE ayuda
PARAMETER var
DO CASE
     CASE VARREAD() = 'W_TALINI'  ;
          .OR. VARREAD() =  ;
          'W_TALFIN'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'TALL'
          titulo = 'AYUDA DE TALLER'
          campo = 'tab_codtab + "  " + tab_destab'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
     CASE VARREAD() = 'W_GARINI'  ;
          .OR. VARREAD() =  ;
          'W_GARFIN'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'INGA'
          titulo = 'AYUDA DE GARANTIA'
          campo = 'tab_codtab + "  " + tab_destab'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
     CASE VARREAD() = 'W_TECINI'  ;
          .OR. VARREAD() =  ;
          'W_TECFIN'
          SELECT st_itecn
          campoa = '" "+codent+" "+noment+" "+CODTEC'
          campob = '" "+noment+" "+codent+" "+CODTEC'
          titulo = 'AYUDA DE TECNICOS'
          DO ayuda2 WITH campoa,  ;
             campob, titulo,  ;
             'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
          SET ORDER TO codigo
ENDCASE
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'ESC'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER codigo
SELECT 2
USE SHARED st_mvord ORDER  ;
    mvo_tecnic
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'PLANILLA DE REPARADOS EN FECHA POR MARCA'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BUS', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
STORE DATE() TO fecha1, fecha2
STORE SPACE(4) TO w_marc
ON KEY LABEL F6 do ayuda
@ 05, 02 SAY 'Marca      :' GET  ;
  w_marc VALID valtab2('MARC', ;
  w_marc,30,30)
@ 06, 02 SAY 'Desde Fecha:' GET  ;
  fecha1
@ 07, 02 SAY 'Hasta Fecha:' GET  ;
  fecha2 RANGE fecha1
READ
IF LASTKEY() <> 27
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     CREATE CURSOR planrep  ;
            (numdoc C (8), fecemi  ;
            D, indori C (4),  ;
            codmod C (15), dia D,  ;
            estado C (4))
     FOR i = 1 TO 3
          DO CASE
               CASE i = 1
                    w_est = '010 '
               CASE i = 2
                    w_est = '021 '
               CASE i = 3
                    w_est = '026 '
          ENDCASE
          SELECT st_mvord
          SET NEAR ON
          SEEK w_est +  ;
               DTOS(fecha1)
          SET NEAR OFF
          SCAN WHILE estado =  ;
               w_est .AND. (dia >=  ;
               fecha1 .AND. dia <=  ;
               fecha2) .AND.   ;
               .NOT. EOF()
               SELECT st_iorep
               SEEK st_mvord.orden
               IF FOUND() .AND.  ;
                  ALLTRIM(codmar) =  ;
                  ALLTRIM(w_marc)
                    SELECT planrep
                    APPEND BLANK
                    REPLACE numdoc  ;
                            WITH  ;
                            st_iorep.numdoc
                    REPLACE fecemi  ;
                            WITH  ;
                            st_iorep.fecemi
                    REPLACE indori  ;
                            WITH  ;
                            st_iorep.indori
                    REPLACE codmod  ;
                            WITH  ;
                            st_iorep.codmod
                    REPLACE estado  ;
                            WITH  ;
                            st_mvord.estado
                    REPLACE dia  ;
                            WITH  ;
                            st_mvord.dia
               ENDIF
               SELECT st_mvord
          ENDSCAN
     ENDFOR
     SELECT planrep
     w_idx = f_indice()
     inde on estado+indori+codmod to &w_idx
     GOTO TOP
     REPORT FORMAT PORL4096 TO  ;
            PRINTER NOCONSOLE
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
ENDIF
DO sacawin
CLOSE DATABASES
ON KEY
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT 7
USE SHARED ge_tab0 ORDER codigo
SET FILTER TO tab_codpre == 'MARC'
titulo = 'AYUDA DE MARCAS'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
RETURN
*
PROCEDURE antiguo
SELECT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.indori,  ;
       st_iorep.codmod,  ;
       st_mvord.dia,  ;
       st_mvord.estado FROM  ;
       ST_IOREP, ST_MVORD WHERE  ;
       st_mvord.orden =  ;
       st_iorep.numdoc AND  ;
       (st_mvord.dia >= fecha1  ;
       AND st_mvord.dia <= fecha2  ;
       AND st_mvord.estado =  ;
       '010 ' OR st_mvord.estado =  ;
       '021 ' OR st_mvord.estado =  ;
       '026 ' AND  ;
       ALLTRIM(st_iorep.codmar) =  ;
       ALLTRIM(w_marc)) ORDER BY  ;
       st_mvord.estado,  ;
       st_iorep.indori,  ;
       st_iorep.codmod INTO  ;
       CURSOR QUERY
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

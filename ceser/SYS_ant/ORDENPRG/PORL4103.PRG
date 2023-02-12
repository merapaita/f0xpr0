*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'PROMEDIO SERVICIO POR TECNICOS'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BUS', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
STORE DATE() TO fecha1, fecha2
SELECT 7
USE SHARED ge_tab0 ORDER codigo
@ 07, 01 CLEAR TO 13, 77
@ 03, 02 TO 06, 77
DO esc_modo WITH 'S'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BUS', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
wk_key = 0
SET CURSOR ON
@ 04, 04 SAY 'Desde Fecha:' GET  ;
  fecha1
@ 04, 40 SAY 'Hasta Fecha:' GET  ;
  fecha2 RANGE fecha1
READ
IF LASTKEY() = 27
     DO saca_win
     RETURN
ENDIF
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'COLO'
SELECT st_mvord.dia,  ;
       st_mvord.orden,  ;
       st_mvord.tecnico,  ;
       st_mvord.estado,  ;
       st_iorep.codemi,  ;
       st_iorep.indori,  ;
       st_iorep.indest,  ;
       st_iorep.cosrep,  ;
       st_iorep.cosmob,  ;
       st_iorep.fecemi,  ;
       st_iorep.codent,  ;
       st_iorep.auxest,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iorep.numser,  ;
       st_iorep.numsol,  ;
       st_iorep.subtot,  ;
       st_iorep.codtall,  ;
       st_iorep.fecest,  ;
       st_itecn.codent,  ;
       st_itecn.noment,  ;
       st_itecn.codcla FROM  ;
       ST_MVORD, ST_IOREP,  ;
       ST_ITECN WHERE  ;
       st_iorep.numdoc =  ;
       st_mvord.orden AND  ;
       st_mvord.tecnico =  ;
       st_itecn.codent AND  ;
       st_mvord.dia >= fecha1 AND  ;
       st_mvord.dia <= fecha2 AND  ;
       st_mvord.estado = '010 '  ;
       AND st_iorep.indest <> 'N'  ;
       ORDER BY st_itecn.codcla,  ;
       st_itecn.noment,  ;
       st_iorep.indori,  ;
       st_iorep.codemi,  ;
       st_mvord.dia INTO CURSOR  ;
       QUERY
REPORT FORMAT PORL4103 TO FILE  ;
       text4.txt
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'SACA'
DO saca_win
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

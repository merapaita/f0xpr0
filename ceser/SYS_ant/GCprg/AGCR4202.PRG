*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW venta03 FROM 07, 00  ;
       TO 18, 40 COLOR SCHEME 20
ACTIVATE WINDOW venta03
CLOSE DATABASES
SET DATE TO dmy
STORE 0 TO venta, garan, fuera
CLEAR
SELECT 1
USE SHARED GC_HVE00 ORDER CODIGO
STORE DATE() TO fechai, fechal
STORE 'Pantalla ' TO impr
@ 00, 01 SAY 'Desde Fecha:' COLOR  ;
  W/N,N/W  GET fechai
@ 01, 01 SAY 'Hasta Fecha:' COLOR  ;
  W/N,N/W  GET fechal RANGE  ;
  fechai
@ 02, 01 SAY  ;
  'Por Pantalla/Impresora:' COLOR  ;
  W/N,N/W  GET impr PICTURE  ;
  '@m Pantalla ,Impresora'
READ
IF LASTKEY() = 27
     DEACTIVATE WINDOW venta03
     RETURN
ENDIF
@ 03, 09 SAY 'Procesando...'
IF impr = 'Impresora'
     SET DEVICE TO PRINTER
     SET PRINTER ON
ENDIF
SELECT gc_hve00
GOTO TOP
SUM FOR (hve_fecdoc >= fechai  ;
    .AND. hve_fecdoc <= fechal  ;
    .AND. (hve_indori = 'GARA'  ;
    .OR. hve_indori = 'GREC')  ;
    .AND. hve_estdoc <> 'A')  ;
    hve_totvta TO garan
GOTO TOP
SUM FOR (hve_fecdoc >= fechai  ;
    .AND. hve_fecdoc <= fechal  ;
    .AND. (hve_indori = 'FGAR'  ;
    .OR. hve_indori = 'FREC')  ;
    .AND. hve_estdoc <> 'A')  ;
    hve_totvta TO fuera
GOTO TOP
SUM FOR (hve_fecdoc >= fechai  ;
    .AND. hve_fecdoc <= fechal  ;
    .AND. hve_indori = 'VENT'  ;
    .AND. hve_estdoc <> 'A')  ;
    hve_totvta TO venta
@ 04, 08 SAY  ;
  'VENTAS TOTALES EN DOLARES'
@ 05, 01 SAY 'EN GARANTIA      :'
@ 05, 20 SAY garan PICTURE  ;
  '9,999,999,999.99'
@ 06, 01 SAY 'FUERA DE GARANTIA:'
@ 06, 20 SAY fuera PICTURE  ;
  '9,999,999,999.99'
@ 07, 01 SAY 'VENTAS           :'
@ 07, 20 SAY venta PICTURE  ;
  '9,999,999,999.99'
@ 08, 01 SAY 'TOTAL GENERAL    :'
@ 08, 20 SAY garan + fuera +  ;
  venta PICTURE  ;
  '9,999,999,999.99'
IF impr = 'Impresora'
     ?
     SET DEVICE TO SCREEN
     SET PRINTER OFF
     @ 05, 01 SAY  ;
       'EN GARANTIA      :'
     @ 05, 20 SAY garan PICTURE  ;
       '9,999,999,999.99'
     @ 06, 01 SAY  ;
       'FUERA DE GARANTIA:'
     @ 06, 20 SAY fuera PICTURE  ;
       '9,999,999,999.99'
     @ 07, 01 SAY  ;
       'VENTAS           :'
     @ 07, 20 SAY venta PICTURE  ;
       '9,999,999,999.99'
     @ 08, 01 SAY  ;
       'TOTAL GENERAL    :'
     @ 08, 20 SAY garan + fuera +  ;
       venta PICTURE  ;
       '9,999,999,999.99'
ENDIF
WAIT ''
DEACTIVATE WINDOW venta03
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

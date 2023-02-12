*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
@ 05, 05 TO 65, 20 DOUBLE
a = 0
b = 0
@ 08, 10 SAY  ;
  'TOTAL DE REGISTROS     :'
@ 10, 10 SAY  ;
  'REGISTROS ACTUALIZADOS :'
SELECT 1
USE ST_IOREP
SELECT 2
USE C:\MC\ST_ORDEN ORDER CODIGO
SELECT 1
GOTO TOP
SCAN WHILE YEAR(fecemi) = 1995  ;
     .AND.  .NOT. EOF()
     a = a + 1
     @ 08, 35 SAY a
     w_numdoc = numdoc
     w_observ = observ
     IF EMPTY(w_observ)
          SELECT st_orden
          SEEK w_numdoc
          IF FOUND()
               w_inftec = observ
               SELECT st_iorep
               REPLACE observ  ;
                       WITH  ;
                       w_inftec
               b = b + 1
               @ 10, 35 SAY b
          ENDIF
     ENDIF
     SELECT st_iorep
ENDSCAN
WAIT WINDOW 'PROCESO TERMINADO'
*
*** 
*** ReFox - retrace your steps ... 
***

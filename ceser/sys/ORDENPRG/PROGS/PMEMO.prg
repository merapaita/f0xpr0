*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
CLEAR
SET DELETED ON
SELECT 1
USE SHARED ST_ISREP ORDER CODIGO
SELECT 2
USE SHARED ST_IOREP ORDER CODIGO
SELECT 3
USE SHARED ST_ISPRE ORDER CODIGO
SELECT 4
USE SHARED ST_MVORD ORDER CODIGO
CLEAR
DO isrep
@ 10, 20 SAY ' TERMINE ST_ISREP'
READ
CLOSE DATABASES
SET MEMOWIDTH TO 50
RETURN
*
PROCEDURE isrep
SELECT st_isrep
GOTO TOP
n = 0
SET SYSMENU ON
SCAN WHILE  .NOT. EOF()
     SET MEMOWIDTH TO 45
     wrk_observ = observ
     wrk_lineas = MEMLINES(observ)
     wrk_resul = ''
     FOR i = 1 TO wrk_lineas
          wrk_resul = wrk_resul +  ;
                      MLINE(observ,  ;
                      i) +  ;
                      SPACE(45 -  ;
                      LEN(MLINE(observ,  ;
                      i)))
     ENDFOR
     n = n + 1
     REPLACE observ WITH  ;
             wrk_resul
     SET MEMOWIDTH TO 35
     wrk_observ = desace
     wrk_lineas = MEMLINES(desace)
     wrk_resul = ''
     FOR i = 1 TO wrk_lineas
          wrk_resul = wrk_resul +  ;
                      MLINE(desace,  ;
                      i) +  ;
                      SPACE(35 -  ;
                      LEN(MLINE(desace,  ;
                      i)))
     ENDFOR
     REPLACE desace WITH  ;
             wrk_resul
     @ 10, 20 SAY n
ENDSCAN
RETURN
*
PROCEDURE iorep
SELECT st_iorep
GOTO TOP
n = 0
SET MEMOWIDTH TO 38
SET SYSMENU ON
SEEK '    3588'
BROWSE
SCAN WHILE  .NOT. EOF()
     wrk_observ = observ
     wrk_lineas = MEMLINES(observ)
     wrk_resul = ''
     FOR i = 1 TO wrk_lineas
          wrk_resul = wrk_resul +  ;
                      MLINE(observ,  ;
                      i) +  ;
                      SPACE(38 -  ;
                      LEN(MLINE(observ,  ;
                      i)))
     ENDFOR
     n = n + 1
     REPLACE observ WITH  ;
             wrk_resul
     @ 10, 20 SAY n
ENDSCAN
RETURN
*
PROCEDURE iprep
SELECT st_ispre
GOTO TOP
n = 0
SET MEMOWIDTH TO 45
SET SYSMENU ON
SCAN WHILE  .NOT. EOF()
     wrk_observ = observ
     wrk_lineas = MEMLINES(observ)
     wrk_resul = ''
     FOR i = 1 TO wrk_lineas
          wrk_resul = wrk_resul +  ;
                      MLINE(observ,  ;
                      i) +  ;
                      SPACE(45 -  ;
                      LEN(MLINE(observ,  ;
                      i)))
     ENDFOR
     n = n + 1
     REPLACE observ WITH  ;
             wrk_resul
     @ 10, 20 SAY n
     BROWSE
ENDSCAN
RETURN
*
PROCEDURE mvord
SELECT st_mvord
GOTO TOP
n = 0
SET MEMOWIDTH TO 45
SCAN WHILE  .NOT. EOF()
     wrk_observ = inftec
     wrk_lineas = MEMLINES(inftec)
     wrk_resul = ''
     FOR i = 1 TO wrk_lineas
          wrk_resul = wrk_resul +  ;
                      MLINE(inftec,  ;
                      i) +  ;
                      SPACE(45 -  ;
                      LEN(MLINE(inftec,  ;
                      i)))
     ENDFOR
     n = n + 1
     REPLACE inftec WITH  ;
             wrk_resul
     @ 10, 20 SAY n
ENDSCAN
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

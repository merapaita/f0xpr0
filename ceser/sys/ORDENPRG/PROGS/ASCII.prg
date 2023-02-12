*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT 1
USE REP_400
SELECT 2
USE GC_PRO00 ORDER CODIGO
SELECT 3
USE GC_ALM00 ORDER CODIGO
SELECT 1
GOTO TOP
@ 10, 20 TO 20, 60 DOUBLE
@ 15, 25 SAY 'Registro No.'
a = 0
DO WHILE  .NOT. EOF()
     a = a + 1
     @ 15, 40 SAY a
     IF SUBSTR(rep_codant, 1, 2) =  ;
        '70'
          wrk_codpro = rep_codant
          wrk_proced = 'N'
          IF rep_prefob = 0
               wrk_prefob = rep_cprdol /  ;
                            1.35 
          ELSE
               wrk_prefob = rep_prefob
          ENDIF
     ELSE
          wrk_codpro = rep_codpro
          wrk_proced = 'I'
          IF rep_prefob = 0
               wrk_prefob = rep_cprdol /  ;
                            1.35 
          ELSE
               wrk_prefob = rep_prefob
          ENDIF
     ENDIF
     wrk_stkfis = rep_stkfis
     wrk_ubicac = rep_ubicac
     wrk_curvta = rep_curvta
     wrk_numpar = rep_codpro
     wrk_modelo = rep_modelo
     SELECT 2
     SEEK wrk_codpro
     IF  .NOT. FOUND()
          SELECT 1
          REPLACE rep_blanco WITH  ;
                  ''
     ELSE
          DO graba
     ENDIF
     SELECT 1
     SKIP
ENDDO
CLOSE DATABASES
WAIT WINDOW 'Proceso Terminado'
*
PROCEDURE graba
REPLACE pro_coremo WITH  ;
        wrk_prefob
REPLACE pro_clacom WITH  ;
        wrk_curvta
REPLACE pro_numpar WITH  ;
        wrk_numpar
REPLACE pro_proced WITH  ;
        wrk_proced
REPLACE pro_modelo WITH  ;
        wrk_modelo
SELECT 3
SEEK wrk_codpro + '0001'
IF FOUND()
     REPLACE alm_stkfis WITH  ;
             wrk_stkfis
     REPLACE alm_ubicac WITH  ;
             wrk_ubicac
ELSE
     APPEND BLANK
     REPLACE alm_codpro WITH  ;
             wrk_codpro
     REPLACE alm_stkfis WITH  ;
             wrk_stkfis
     REPLACE alm_codalm WITH  ;
             '0001'
     REPLACE alm_ubicac WITH  ;
             wrk_ubicac
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

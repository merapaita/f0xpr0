*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
SELECT 1
USE st_inv00 ORDER numsol
SELECT 2
USE st_iorep ORDER ord_numsol
SELECT 3
USE st_invfi
GOTO TOP
DO WHILE  .NOT. EOF()
     wrk_numsol = inv_numsol
     wrk_estinv = inv_auxest
     wrk_emiinv = inv_codemi
     wrk_talinv = inv_codtal
     wrk_codexi = inv_codexi
     SELECT 1
     SEEK wrk_numsol
     IF  .NOT. FOUND()
          APPEND BLANK
          REPLACE numdoc WITH  ;
                  wrk_numsol
     ENDIF
     REPLACE inv_fisest WITH  ;
             wrk_estinv
     REPLACE inv_fisemi WITH  ;
             wrk_emiinv
     REPLACE inv_fistal WITH  ;
             wrk_talinv
     REPLACE inv_codexi WITH  ;
             wrk_codexi
     SELECT 2
     SEEK wrk_numsol
     IF  .NOT. FOUND()
          SELECT 1
          REPLACE inv_codexi WITH  ;
                  '0'
     ELSE
          wrk_estact = auxest
          SELECT 1
          REPLACE inv_codexi WITH  ;
                  '1'
          REPLACE inv_estact WITH  ;
                  wrk_estact
     ENDIF
     SELECT 3
     SKIP
ENDDO
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
CLOSE DATABASES
SET ESCAPE OFF
@ 05, 05 TO 20, 74 DOUBLE
@ 05, 35 SAY  ;
  'DEPURACION DE ARCHIVOS'
@ 06, 20 SAY 'Reg. Borrados'
@ 06, 30 SAY 'Total de Reg.'
@ 06, 46 SAY 'Ult. Depuraci¢n'
@ 07, 07 SAY 'st_iorep : '
@ 08, 07 SAY 'st_isrep : '
@ 09, 07 SAY 'st_mvord : '
@ 10, 07 SAY 'st_iprep : '
@ 11, 07 SAY 'st_idped : '
@ 12, 07 SAY 'st_ispre : '
@ 13, 07 SAY 'st_idpre : '
@ 14, 07 SAY 'st_movca : '
@ 15, 07 SAY 'st_movso : '
@ 16, 07 SAY 'st_estad : '
@ 17, 07 SAY 'st_sicli : '
@ 18, 07 SAY 'st_iredo : '
@ 19, 07 SAY 'st_iscic : '
STORE 0 TO a01, a02, a03, a04,  ;
      a05, a06, a07, a08, a09,  ;
      a10, a11, a12, a13, az
SELECT 1
USE st_iorep
SELECT 2
USE st_isrep ORDER codigo
SELECT 3
USE st_mvord ORDER eor_nroord
SELECT 4
USE st_iprep ORDER rep_numord
SELECT 5
USE st_idped ORDER dre_numord
SELECT 6
USE st_ispre ORDER pre_numord
SELECT 7
USE st_idpre ORDER codigo
SELECT 8
USE st_movca ORDER numsol
SELECT 9
USE st_movso ORDER numsol
SELECT 10
USE st_estad ORDER est_numord
SELECT 11
USE st_sicli ORDER codigo
SELECT 12
USE st_iredo ORDER codigo
SELECT 13
USE st_iscic ORDER numord
SELECT st_iorep
GOTO TOP
DO WHILE YEAR(fecemi)<1999
     a01 = a01 + 1
     @ 07, 40 SAY a01
     w_numord = numdoc
     w_numsol = numsol
     IF (indest = 'F' .AND.  ;
        auxest = '100 ') .OR.  ;
        (indest = 'B' .AND.  ;
        auxest = '100 ') .OR.  ;
        indest = 'N' .OR. auxest =  ;
        '020 ' .OR. auxest =  ;
        '022 ' .OR. auxest =  ;
        '023 ' .OR. auxest =  ;
        '024 ' .OR. auxest =  ;
        '025 ' .OR. auxest =  ;
        '027 ' .OR. auxest =  ;
        '028 ' .OR. auxest =  ;
        '029 ' .OR. auxest =  ;
        '030 ' .OR. auxest =  ;
        '018 ' .OR. auxest =  ;
        '016 '
          SELECT st_isrep
          SEEK w_numsol
          IF FOUND()
               DELETE
               a02 = a02 + 1
               @ 08, 18 SAY a02
          ENDIF
          SELECT st_mvord
          SEEK w_numord
          IF FOUND()
               SCAN WHILE orden =  ;
                    w_numord
                    DELETE
                    a03 = a03 + 1
                    @ 09, 18 SAY  ;
                      a03
               ENDSCAN
          ENDIF
          SELECT st_iprep
          SEEK w_numord
          IF FOUND()
               SCAN WHILE numord =  ;
                    w_numord
                    DELETE
                    a04 = a04 + 1
                    @ 10, 18 SAY  ;
                      a04
               ENDSCAN
          ENDIF
          SELECT st_idped
          SEEK w_numord
          IF FOUND()
               SCAN WHILE numord =  ;
                    w_numord
                    DELETE
                    a05 = a05 + 1
                    @ 11, 18 SAY  ;
                      a05
               ENDSCAN
          ENDIF
          SELECT st_ispre
          SEEK w_numord
          IF FOUND()
               w_numpre = numdoc
               DELETE
               a06 = a06 + 1
               @ 12, 18 SAY a06
               SELECT st_idpre
               SET NEAR ON
               SEEK w_numpre +  ;
                    w_numord
               SCAN WHILE numord =  ;
                    w_numord
                    DELETE
                    a07 = a07 + 1
                    @ 13, 18 SAY  ;
                      a07
               ENDSCAN
               SET NEAR OFF
          ENDIF
          SELECT st_movca
          SEEK w_numsol
          IF FOUND()
               SCAN WHILE numsol =  ;
                    w_numsol
                    DELETE
                    a08 = a08 + 1
                    @ 14, 18 SAY  ;
                      a08
               ENDSCAN
          ENDIF
          SELECT st_movso
          SEEK w_numsol
          IF FOUND()
               SCAN WHILE numsol =  ;
                    w_numsol
                    DELETE
                    a09 = a09 + 1
                    @ 15, 18 SAY  ;
                      a09
               ENDSCAN
          ENDIF
          SELECT st_estad
          SEEK VAL(w_numord)
          IF FOUND()
               SCAN WHILE numord =  ;
                    VAL(w_numord)
                    DELETE
                    a10 = a10 + 1
                    @ 16, 18 SAY  ;
                      a10
               ENDSCAN
          ENDIF
          SELECT st_sicli
          SET NEAR ON
          SEEK w_numsol
          SCAN WHILE numdoc =  ;
               w_numsol
               DELETE
               a11 = a11 + 1
               @ 17, 18 SAY a11
          ENDSCAN
          SET NEAR OFF
          SELECT st_iredo
          SEEK 'ORD ' + w_numord
          IF FOUND()
               SCAN WHILE numodo =  ;
                    w_numord
                    DELETE
                    a12 = a12 + 1
                    @ 18, 18 SAY  ;
                      a12
               ENDSCAN
          ENDIF
          SEEK 'SSE ' + w_numsol
          IF FOUND()
               SCAN WHILE numodo =  ;
                    w_numsol
                    DELETE
                    a12 = a12 + 1
                    @ 18, 18 SAY  ;
                      a12
               ENDSCAN
          ENDIF
          SELECT st_iscic
          SEEK w_numord
          IF FOUND()
               SCAN WHILE numord =  ;
                    w_numord
                    DELETE
                    a13 = a13 + 1
                    @ 19, 18 SAY  ;
                      a13
               ENDSCAN
          ENDIF
          SELECT st_iorep
          DELETE
          az = az + 1
          @ 07, 18 SAY az
     ENDIF
     SKIP
ENDDO
CLOSE DATABASES
SELECT 1
USE ST_ISREP
SELECT 2
USE ST_IOREP ORDER ORD_NUMSOL
SELECT 3
USE ST_SICLI ORDER CODIGO
b = 0
c = 0
d = 0
SELECT st_isrep
DO WHILE YEAR(fecemi)<1999
     w_numdoc = numdoc
     d = d + 1
     @ 07, 50 SAY d
     SELECT st_iorep
     SEEK w_numdoc
     IF  .NOT. FOUND()
          SELECT st_isrep
          DELETE
          c = c + 1
          @ 07, 60 SAY c
     ENDIF
     SELECT st_sicli
     SET NEAR ON
     SEEK w_numdoc
     SCAN WHILE numdoc = w_numdoc
          DELETE
          b = b + 1
          @ 17, 60 SAY b
     ENDSCAN
     SET NEAR OFF
     SELECT st_isrep
     SKIP
ENDDO
CLOSE DATABASES
USE EXCLUSIVE st_iorep
PACK
USE
USE EXCLUSIVE st_isrep
PACK
USE
USE EXCLUSIVE st_mvord
PACK
USE
USE EXCLUSIVE st_iprep
PACK
USE
USE EXCLUSIVE st_idped
PACK
USE
USE EXCLUSIVE st_ispre
PACK
USE
USE EXCLUSIVE st_idpre
PACK
USE
USE EXCLUSIVE st_movca
PACK
USE
USE EXCLUSIVE st_movso
PACK
USE
USE EXCLUSIVE st_estad
PACK
USE
USE EXCLUSIVE st_iredo
PACK
USE
USE EXCLUSIVE st_iscic
PACK
USE
USE EXCLUSIVE st_sicli
PACK
USE
CLOSE DATABASES
WAIT WINDOW 'PROCESO TERMINADO'
*
*** 
*** ReFox - retrace your steps ... 
***

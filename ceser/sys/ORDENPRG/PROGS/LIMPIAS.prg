*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
USE SHARED st_iseri ORDER  ;
    ser_codmar
GOTO TOP
DO WHILE  .NOT. EOF()
     marca = codmar
     modelo = modelo
     serie = numser
     n = 1
     SCAN WHILE marca + modelo +  ;
          serie = codmar + modelo +  ;
          numser .AND.  .NOT.  ;
          EOF()
          DELETE
          n = n + 1
     ENDSCAN
     IF n > 1
          SKIP -1
          RECALL
          SKIP
     ENDIF
ENDDO
*
*** 
*** ReFox - retrace your steps ... 
***

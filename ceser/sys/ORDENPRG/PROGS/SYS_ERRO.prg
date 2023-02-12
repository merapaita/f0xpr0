*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER merror, mess, mprog
DEFINE WINDOW ayu3 FROM 0, 0 TO 9,  ;
       50 SHADOW TITLE  ;
       ' Mensaje de Error ' COLOR  ;
       N/R,W+/N,N/R,W/N,W/N,W+/N, ;
       W+/N,W+/N 
ACTIVATE WINDOW ayu3
@ 1, 2 SAY 'Numero del Error : ' +  ;
  LTRIM(STR(merror))
@ 2, 2 SAY 'Mensaje de Error : ' +  ;
  mess
@ 3, 2 SAY 'Nombre Programa  : ' +  ;
  mprog
FOR j = 1 TO 10
     ZOOM WINDOW ayu3 NORM FROM j,  ;
          0 TO 9 + j, 50
ENDFOR
FOR j = 1 TO 15
     ZOOM WINDOW ayu3 NORM FROM  ;
          10, j TO 19, 50 + j
ENDFOR
varx = 0
DO WHILE varx==0
     @ 7, 10 PROMPT '\<Reintenta'
     @ 7, 30 PROMPT '\<Cancela'
     MENU TO varx
ENDDO
DEACTIVATE WINDOW ayu3
IF varx = 1
     RETRY
ELSE
     CLOSE DATABASES
     CLEAR ALL
     CANCEL
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

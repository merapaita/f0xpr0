*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
DIMENSION a( 5)
a( 1) = 'System'
a( 2) = 'File'
a( 3) = 'Edit'
a( 4) = 'Database'
a( 5) = 'Program'
DEFINE POPUP foxpro FROM 10, 25  ;
       TITLE 'Foxpro 2.5' COLOR , ;
       W/N,,N/W 
FOR x = 1 TO ALEN(a, 1)
     DEFINE BAR x OF foxpro  ;
            PROMPT a(x)
ENDFOR
ACTIVATE POPUP foxpro
*
*** 
*** ReFox - retrace your steps ... 
***

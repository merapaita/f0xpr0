*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE POPUP menuxx FROM 07, 00  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menuxx PROMPT  ;
       'Par metros \<Generales'
DEFINE BAR 2 OF menuxx PROMPT  ;
       '\<Tablas Generales'
ON SELECTION POPUP menuxx DO MENU10 WITH;
BAR()
ACTIVATE POPUP menuxx
*
*** 
*** ReFox - retrace your steps ... 
***

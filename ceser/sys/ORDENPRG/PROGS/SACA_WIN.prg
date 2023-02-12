*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
FOR i = 38 TO 1 STEP -1
     ZOOM WINDOW indicar NORM  ;
          FROM 20, 0 TO 23, i *  ;
          2
ENDFOR
DEACTIVATE WINDOW indicar
ACTIVATE WINDOW trabajo
FOR i = 19 TO 1 STEP -1
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO 17, i * 4
ENDFOR
FOR i = 17 TO 1 STEP -1
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO i, 1
ENDFOR
FOR i = 11 TO 1 STEP -1
     ZOOM WINDOW trabajo NORM  ;
          FROM 23 - (i * 2), 0 TO  ;
          24 - (i * 2), 1
ENDFOR
FOR j = 11 TO 1 STEP -1
     ZOOM WINDOW trabajo NORM  ;
          FROM 23, 78 - (j * 7)  ;
          TO 24, 79 - (j * 7)
ENDFOR
DEACTIVATE WINDOW trabajo
DEACTIVATE WINDOW oculta
ACTIVATE SCREEN
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW unidad FROM 11, 02  ;
       TO 13, 28 IN screen COLOR  ;
       SCHEME 20
SAVE SCREEN TO antes
ACTIVATE WINDOW unidad
SET CURSOR ON
unidad = 'A'
@ 0, 0 SAY  ;
  ' DESTINO (A: ¢ B:) : ' GET  ;
  unidad PICTURE 'A' VALID unidad $  ;
  'AB'
READ
unidad = unidad + ':'
IF LASTKEY() <> 27
     RUN ECHO OFF
     !RESTORE  &UNIDAD J: /S
ENDIF
DEACTIVATE WINDOW unidad
ACTIVATE SCREEN
RESTORE SCREEN FROM antes
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

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
RUN C:
RUN CD\
RUN MD ARC_REP
RUN CD ARC_REP
RUN COPY J:\GC\G*.DBF 
unidad = unidad + ':'
bas = 'C:\ARC_REP\*.DBF'
IF LASTKEY() <> 27
     RUN ECHO OFF
     !BACKUP &BAS &UNIDAD
ENDIF
RUN I:
RUN CD\
RUN CD\GC\
DEACTIVATE WINDOW unidad
ACTIVATE SCREEN
RESTORE SCREEN FROM antes
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
SET TALK OFF
SET ECHO OFF
CLEAR ALL
DEFINE WINDOW sucu FROM 08, 20 TO  ;
       20, 60 COLOR SCHEME 24
ACTIVATE WINDOW sucu
STORE 'Arequipa  ' TO sucu
STORE SPACE(25) TO ruta
@ 03, 15 SAY 'CESER S.A.'
@ 05, 05 SAY 'Sucursal: '
SET CURSOR ON
@ 05, 15 GET sucu PICTURE  ;
  '@m Arequipa,Bre¤a     ,Trujillo  ,Chiclayo  ,Piura     ,Huacho    ,Pucallpa  '
READ
IF LASTKEY() = 27
     RETURN
ENDIF
RELEASE WINDOW sucu
DO CASE
     CASE sucu = 'Arequipa'
          ruta = 'CESER2\DATA:SUCU\CESER60'
     CASE sucu = 'Trujillo'
          ruta = 'CESER2\DATA:SUCU\CESER30'
     CASE sucu = 'Chiclayo'
          ruta = 'CESER2\DATA:SUCU\CESER40'
     CASE sucu = 'Bre¤a   '
          ruta = 'CESER2\DATA:SUCU\CESER21'
     CASE sucu = 'Huacho  '
          ruta = 'CESER2\DATA:SUCU\CESER26'
     CASE sucu = 'Piura   '
          ruta = 'CESER2\DATA:SUCU\CESER50'
     OTHERWISE
          ruta = 'CESER2\DATA:BASES'
ENDCASE
DO sistems
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

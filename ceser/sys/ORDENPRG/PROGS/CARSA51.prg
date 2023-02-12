*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET SYSMENU ON
w_sint = '    '
REPORT FORMAT carsa50 TO FILE  ;
       a:\carsa50.doc
*
FUNCTION sintoma
PARAMETER opc
SELECT 1
SET NEAR ON
SEEK opc
SET NEAR OFF
SCAN WHILE numdoc = opc
     w_sint = codsin
     IF SUBSTR(codsin, 2, 3) =  ;
        '561' .OR. SUBSTR(codsin,  ;
        2, 3) = '570'
          EXIT
     ENDIF
ENDSCAN
SELECT 2
RETURN w_sint
*
*** 
*** ReFox - retrace your steps ... 
***

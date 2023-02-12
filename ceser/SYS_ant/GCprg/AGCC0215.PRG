*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 07,  ;
       02 TO 20, 77 COLOR SCHEME  ;
       7
SELECT 1
USE CESER\APLIC:JVC\AIWA\AIWA
ACTIVATE WINDOW tablas
DO p_prestab WITH 'CONSULTAS',  ;
   'LISTA DE PRECIOS AIWA',  ;
   'AIWA'
@ 2, 63 SAY PROGRAM()
STORE SPACE(18) TO wrk_codigo
DO WHILE .T.
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     SET SYSMENU ON
     ACTIVATE WINDOW cabecera
     BROWSE FIELDS no_parte :R :  ;
            16 :H =  ;
            'Nro. de Parte',  ;
            descrip :R : 20 :H =  ;
            'Descripci¢n', modelo  ;
            :R : 10 :H = 'Modelo',  ;
            precio :R : 8 :H =  ;
            'Precio' :P =  ;
            '9,999.99', nla_code  ;
            :R : 4 :H = 'NLA' :P =  ;
            '99', spc_stock :R :  ;
            7 :H = 'Sps Stk' :P =  ;
            '999,999', aipc_stock  ;
            :R : 9 :H =  ;
            'Aipc Stk' :P =  ;
            '9,999,999',  ;
            substitute :R :H =  ;
            'Reemplazo' IN  ;
            cabecera COLOR SCHEME  ;
            7
     IF LASTKEY() = 27
          CLOSE DATABASES
          RELEASE WINDOW cabecera
          DEACTIVATE WINDOW  ;
                     tablas
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          SET SYSMENU OFF
          RETURN
     ENDIF
ENDDO
*
*** 
*** ReFox - retrace your steps ... 
***

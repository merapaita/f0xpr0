*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
CLOSE DATABASES
SET CENTURY ON
DEFINE WINDOW detalle FROM 04, 02  ;
       TO 21, 77 IN screen COLOR  ;
       SCHEME 24
DEFINE WINDOW ven2 FROM 18, 10 TO  ;
       20, 60 TITLE  ;
       'INGRESE EL TEXTO' IN  ;
       screen COLOR SCHEME 12
DEFINE WINDOW ven3 FROM 14, 10 TO  ;
       17, 30 TITLE 'SELECCIONE'  ;
       IN screen COLOR SCHEME 12
SELECT 1
USE GC_PRO00 ALIAS gc_pro00 ORDER  ;
    CODIGO
SELECT 2
USE GC_HLP00 ORDER CODIGO
SELECT 3
USE GC_DLP00 ALIAS gc_dlp00 ORDER  ;
    CODIGO
SELECT 4
USE GC_ALM00 ALIAS gc_alm00 ORDER  ;
    CODPRO
SELECT 5
USE GE_TAB0 ALIAS ge_tab0 ORDER  ;
    CODIGO
SELECT gc_pro00
GOTO TOP
SET RELATION TO 'PUBL' + pro_codpro INTO;
gc_dlp00, pro_codpro INTO gc_alm00
STORE 0 TO wrk_canti, stock,  ;
      precio, w_facigv, wrk_des
STORE SPACE(14) TO codigo, codi
CLEAR
ACTIVATE WINDOW tablas
DO p_prestab WITH  ;
   'OFERTAS DE PRODUCTOS',  ;
   'DE EXISTENCIAS EXCEDENTES E INACTIVOS',  ;
   'PROCESO'
@ 02, 63 SAY PROGRAM()
STORE .T. TO sigue
w_facigv = facigv()
IF w_facigv = 0
     sigue = .F.
ENDIF
SELECT gc_pro00
GOTO TOP
DO WHILE sigue
     ACTIVATE WINDOW detalle
     @ 04, 20 SAY  ;
       'Espere un Momento, Por Favor .....'
     DO p_footer WITH  ;
        '000010000000000110100110',  ;
        2
     ON KEY LABEL F6 DO BUSCAR
     ON KEY LABEL F9 DO CALTOT
     ON KEY LABEL HOME DO INICIO
     ON KEY LABEL END DO FIN
     ACTIVATE WINDOW detalle
     CLEAR
     SET FILTER TO (pro_nivcit = 'D';
.OR. pro_nivcit = 'E';
.OR. pro_nivcit = 'F');
.OR. (gc_alm00.alm_stkfis > pro_stkmax)
     BROWSE FIELDS pro_codpro :  ;
            15 :R :H = 'CODIGO',  ;
            pro_descri : 15 :R :H =  ;
            '  DESCRIPCION',  ;
            pro_rcom : 1 :R :H =  ;
            ' R COMPRA', stock =  ;
            valsd() : 8 :R :H =  ;
            '  STOCK', pro_stkmax  ;
            : 8 :R :H = 'ST_MAX.',  ;
            gc_dlp00.dlp_prsigv  ;
            :V = valpre() :P =  ;
            '999,999.99' :H =  ;
            'PRE_SIGV',  ;
            gc_dlp00.dlp_prcigv  ;
            :P = '999,999.99' :R  ;
            :H = 'PRE_CIGV',  ;
            pro_estope : 1 :P =  ;
            '@M A,O' :H =  ;
            'ESTADO', pro_coremo  ;
            :P = '999,999.99' :R  ;
            :H = 'REPOSICION' IN  ;
            detalle NOLGRID
     IF LASTKEY() = 27
          sigue = .F.
     ENDIF
ENDDO
CLOSE DATABASES
ON KEY
DEACTIVATE WINDOW tablas, codigo,  ;
           footer
DEACTIVATE WINDOW detalle
RELEASE WINDOW cabecera, detalle
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
ON KEY LABEL F9
ON KEY LABEL F6
ON KEY LABEL HOME
ON KEY LABEL END
RETURN
*
FUNCTION valsd
wrkvar = SELECT()
SELECT gc_alm00
SUM FOR gc_alm00.alm_codpro =  ;
    gc_pro00.pro_codpro  ;
    gc_alm00.alm_stkfis TO  ;
    wrk_canti
IF wrkvar = 1
     SELECT gc_pro00
ELSE
     SELECT gc_dlp00
ENDIF
RETURN wrk_canti
*
FUNCTION valpre
SELECT gc_dlp00
REPLACE gc_dlp00.dlp_prcigv WITH  ;
        ROUND(gc_dlp00.dlp_prsigv *  ;
        w_facigv, 2)
SELECT gc_pro00
RETURN .T.
*
PROCEDURE caltot
f1 = .T.
wrk_des = 0
DEFINE WINDOW panct FROM 20, 15  ;
       TO 22, 60 COLOR SCHEME 7
ACTIVATE WINDOW panct
@ 00, 04 SAY  ;
  'INGRESE EL DESCUENTO TOTAL :'  ;
  GET wrk_des RANGE 0,99.99   ;
  PICTURE '99.99'
READ
IF wrk_des <> 0
     wrk_des = 1 - (wrk_des /  ;
               100)
     CLEAR
     @ 00, 18 SAY  ;
       'Procesando .....'
     SELECT gc_pro00
     GOTO TOP
     REPLACE pro_estope WITH 'O',  ;
             gc_dlp00.dlp_prsigv  ;
             WITH  ;
             ROUND((gc_dlp00.dlp_prsigv *  ;
             wrk_des), 2),  ;
             gc_dlp00.dlp_prcigv  ;
             WITH  ;
             ROUND((gc_dlp00.dlp_prsigv *  ;
             w_facigv), 2) ALL  ;
             FOR (pro_nivcit =  ;
             'D' .OR. pro_nivcit =  ;
             'E' .OR. pro_nivcit =  ;
             'F') .OR. (valsd() >  ;
             pro_stkmax)
     GOTO TOP
ENDIF
DEACTIVATE WINDOW panct
RETURN
*
PROCEDURE inicio
GOTO TOP
RETURN
*
PROCEDURE fin
GOTO BOTTOM
RETURN
*
PROCEDURE buscar
STORE 0 TO opcion
ACTIVATE WINDOW ven3
@ 00, 00 GET opcion DEFAULT 1  ;
  SIZE 1, 10, 0 PICTURE  ;
  '@*+VN CODIGO;DESCRIPCION'  ;
  VALID bus1(opcion) COLOR SCHEME  ;
  10
READ CYCLE
IF LASTKEY() <> 27
     ACTIVATE WINDOW ven2
     @ 00, 00 GET codi PICTURE  ;
       '@!'
     READ
     SET NEAR ON
     SEEK codi
     SET NEAR OFF
ENDIF
DEACTIVATE WINDOW ven2, ven3
RETURN
*
PROCEDURE bus1
PARAMETER opc
DO CASE
     CASE opc = 1
          codi = SPACE(14)
          SELECT gc_pro00
          SET ORDER TO CODIGO
          CLEAR READ
     CASE opc = 2
          codi = SPACE(40)
          SELECT gc_pro00
          SET ORDER TO DESCRI
          CLEAR READ
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

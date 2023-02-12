*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
PUBLIC proveed, estades, pedido
STORE SPACE(30) TO proveed,  ;
      estades, pedido
SELECT 1
USE SHARED gc_hco00 ORDER codigo
SELECT 2
USE SHARED GC_CLI00 ORDER CODIGO
SELECT 3
USE SHARED ge_tab0 ORDER CODIGO
SELECT 4
USE SHARED gc_pro00 ORDER Codigo
SELECT 5
USE SHARED gc_dco00 ORDER CodigO
SET RELATION TO dco_nrodoc INTO gc_hco00,;
dco_codpro INTO gc_pro00
SELECT 1
SET NEAR ON
DEFINE WINDOW vent_1 FROM 00, 00  ;
       TO 21, 79 TITLE  ;
       'ÄÄÄÄÄÄÄÄÄÄÄÄÄ CONTROL DE PEDIDOS ÄÄÄÄÄÄÄÄÄÄÄÄÄ'  ;
       IN screen COLOR SCHEME 24
DEFINE WINDOW detalle FROM 06, 04  ;
       TO 15, 72 SHADOW TITLE  ;
       ' DETALLE ' IN screen  ;
       FOOTER  ;
       '  Presione Esc para Salir  '  ;
       COLOR SCHEME 19
DO p_footer WITH  ;
   '100000000000000110011110', 2
ACTIVATE WINDOW vent_1
GOTO TOP
DO dato
tmp_x = 0
RELEASE WINDOW vent_1, vent_2,  ;
        vent_3, vent_4
SET RELATION TO
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE dato
SELECT 1
CLEAR
@ 0, 0 SAY  ;
  '   Nro.Doc    Fec.Doc.   Fec.Conf.  Proveedor   Estado   Factura    Llegada'  ;
  COLOR W+/N 
COUNT ALL FOR ( .NOT. EOF(1)  ;
      .AND. (hco_nrodoc <>  ;
      hco_numfac)) TO ml
DIMENSION pedidos( ml)
lm = 1
x = 1
GOTO TOP
ON KEY LABEL ENTER DO DETALLE with left(pedido,10)
@ 8, 24 FILL TO 10, 51 COLOR W+/ ;
  BG 
@ 8, 24 TO 10, 51 COLOR W+/BG 
SCAN FOR ( .NOT. EOF(1) .AND.  ;
     (hco_nrodoc <> hco_numfac))
     por = x / ml
     @ 09, 25 SAY  ;
       'CARGANDO PEDIDOS  ' +  ;
       TRANSFORM((por * 100),  ;
       '999.99') + ' %' COLOR W+/ ;
       BG 
     x = x + 1
     DO estado WITH hco_indest
     SELECT 1
     DO provee WITH hco_codent
     SELECT 1
     pedidos( lm) = hco_nrodoc +  ;
            '³' +  ;
            DTOC(hco_fecdoc) +  ;
            '³' +  ;
            DTOC(hco_feccon) +  ;
            '³' + SUBSTR(proveed,  ;
            1, 11) + '³' +  ;
            estades + '³' +  ;
            hco_numfac + '³' +  ;
            DTOC(hco_feclle)
     lm = lm + 1
ENDSCAN
FOR xx = 1 TO 1000
     @ 09, 25 SAY  ;
       'CARGANDO PEDIDOS  ' +  ;
       TRANSFORM((por * 100),  ;
       '999.99') + ' %' COLOR W+/ ;
       BG 
ENDFOR
?? CHR(07)
@ 02, 03, 19, 77 BOX ( ;
  '     ±±±±')
@ 01, 01 GET pedido SIZE 18, 77  ;
  FROM pedidos
READ
SELECT 1
RETURN
*
FUNCTION estado
PARAMETER indicador
xxsele = SELECT()
SELECT 3
SEEK 'IMPO' + indicador
IF FOUND()
     estades = LEFT(tab_destab,  ;
               7)
ELSE
     estades = SPACE(30)
ENDIF
RETURN estades
SELECT (xxsele)
RETURN
*
FUNCTION provee
PARAMETER proveedor
xxsele = SELECT()
SELECT 2
SEEK 'P' + proveedor
IF FOUND()
     proveed = LEFT(cli_razsoc,  ;
               20)
ELSE
     proveedor = SPACE(30)
ENDIF
RETURN proveed
SELECT (xxsele)
RETURN
*
PROCEDURE centro
PARAMETER cadena, linea, xcolor
PRIVATE lcadena
STORE LEN(cadena) TO lcadena
@ linea, (80 - lcadena) / 2 SAY  ;
  cadena
RETURN
*
PROCEDURE detalle
PARAMETER orden
SELECT gc_dco00
SET FILTER TO dco_nrodoc = orden
ACTIVATE WINDOW detalle
ON KEY
BROWSE FIELDS dco_nrodoc :H =  ;
       'N§ DOC.', dco_codprp :H =  ;
       'CODIGO', dco_codpro :H =  ;
       'CODREEMPLA',  ;
       gc_pro00.pro_descri : 30  ;
       :H = 'DESCRIPCION',  ;
       dco_inorig : 1, dco_docref  ;
       :H = 'REFEREN', dco_numfac,  ;
       dco_indest : 1, dco_cansol  ;
       :H = 'SOLIC' :P = '9999',  ;
       dco_cancon :H = 'CONFI' :P =  ;
       '9999', dco_candes :H =  ;
       'DESPAC' :P = '9999',  ;
       dco_canbor :H = 'BACKOR'  ;
       :P = '9999', dco_cancel :H =  ;
       'CANCEL' :P = '9999',  ;
       dco_conpre :P =  ;
       '999,999.99' :H =  ;
       'P.FOB US$',  ;
       gc_pro00.pro_stksol : 4 :H =  ;
       'SOLI' :P = '9999',  ;
       gc_pro00.pro_stktra : 4 :H =  ;
       'TRAN' :P = '9999',  ;
       gc_pro00.pro_stkbor : 4 :H =  ;
       'B/OR' :P = '9999' NOEDIT  ;
       IN detalle
ON KEY LABEL ENTER DO DETALLE with left(pedido,10)
DEACTIVATE WINDOW detalle
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

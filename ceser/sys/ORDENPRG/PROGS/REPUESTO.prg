*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEACTIVATE WINDOW menu
tit1 = ALLTRIM(mod_descri)
SET COLOR TO W+/N
@ 1, 26, 3, 52 BOX '�Ŀ����� '
@ 2, 26 + ((26 - LEN(tit1)) / 2)  ;
  SAY tit1
SET PROCEDURE TO RGET0000
DO p_objetos
DO p_menupri
CLOSE DATABASES
DO p_tempos
DEACTIVATE WINDOW titulo
RETURN
*
PROCEDURE p_objetos
DEFINE MENU menu00 COLOR SCHEME  ;
       20
DEFINE PAD tabpad OF menu00  ;
       PROMPT '\<ACTUALIZACIONES'  ;
       AT 5, 01 SKIP FOR (xnnn =  ;
       'A6' .OR. xnnn = 'A8')
DEFINE PAD ordpad OF menu00  ;
       PROMPT '\<PROCESOS' AT 5,  ;
       21 SKIP FOR (xnnn = 'A6'  ;
       .OR. xnnn = 'A8')
DEFINE PAD pogpad OF menu00  ;
       PROMPT '\<CONSULTAS' AT 5,  ;
       35 SKIP FOR (xnnn = 'A6')
DEFINE PAD logpad OF menu00  ;
       PROMPT '\<REPORTES' AT 5,  ;
       50 SKIP FOR (xnnn = 'A1')
DEFINE PAD reppad OF menu00  ;
       PROMPT '\<ADMINISTRACION'  ;
       AT 5, 63 SKIP FOR (xnnn <>  ;
       'A7' .AND. xnnn <> 'A8'  ;
       .AND. xnnn <> 'A2')
ON PAD tabpad OF menu00 ACTIVATE POPUP;
menu01
ON PAD ordpad OF menu00 ACTIVATE POPUP;
menu02
ON PAD pogpad OF menu00 ACTIVATE POPUP;
menu03
ON PAD logpad OF menu00 ACTIVATE POPUP;
menu04
ON PAD reppad OF menu00 ACTIVATE POPUP;
menu05
DEFINE POPUP menu01 FROM 07, 00  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu01 PROMPT  ;
       'Pa\<r�metros Generales    '  ;
       SKIP FOR (xnnn <> 'A7'  ;
       .OR. xnnn = 'A1')
DEFINE BAR 2 OF menu01 PROMPT  ;
       '\<Tablas Generales        '  ;
       SKIP FOR (xnnn <> 'A7'  ;
       .OR. xnnn = 'A1')
DEFINE BAR 3 OF menu01 PROMPT  ;
       '\<Ficha de Productos      '  ;
       SKIP FOR (xnnn = 'A5' .OR.  ;
       xnnn = 'A1')
DEFINE BAR 4 OF menu01 PROMPT  ;
       '\<Unidades Alternativas   '  ;
       SKIP FOR (xnnn = 'A5' .OR.  ;
       xnnn = 'A1')
DEFINE BAR 5 OF menu01 PROMPT  ;
       '\<Productos Alternativos  '  ;
       SKIP FOR (xnnn = 'A5' .OR.  ;
       xnnn = 'A1')
DEFINE BAR 6 OF menu01 PROMPT  ;
       'F\<icha de Entidades      '
DEFINE BAR 7 OF menu01 PROMPT  ;
       'P\<aridades de Cambio     '  ;
       SKIP FOR (xnnn = 'A3' .OR.  ;
       xnnn = 'A1')
DEFINE BAR 8 OF menu01 PROMPT  ;
       'U\<bicaci�n de Productos  '  ;
       SKIP FOR (xnnn = 'A5' .OR.  ;
       xnnn = 'A1')
DEFINE BAR 9 OF menu01 PROMPT  ;
       'M�r\<genes Comerciales    '  ;
       SKIP FOR (xnnn = 'A5' .OR.  ;
       xnnn = 'A1')
DEFINE BAR 10 OF menu01 PROMPT  ;
       '\<M�nimos y M�ximos       '  ;
       SKIP FOR (xnnn = 'A5' .OR.  ;
       xnnn = 'A1')
DEFINE BAR 11 OF menu01 PROMPT  ;
       '\<Consumo de Repuestos    '  ;
       SKIP FOR (xnnn = 'A5' .OR.  ;
       xnnn = 'A1')
DEFINE BAR 12 OF menu01 PROMPT  ;
       '\<Valores de Importaci�n  '  ;
       SKIP FOR (xnnn = 'A5' .OR.  ;
       xnnn = 'A1')
ON SELECTION POPUP menu01 DO MENU10 WITH;
BAR()
DEFINE POPUP menu02 FROM 07, 18  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu02 PROMPT  ;
       '\<Ordenes de Requerimiento '  ;
       SKIP FOR (xnnn = 'A5' .OR.  ;
       xnnn = 'A1')
DEFINE BAR 2 OF menu02 PROMPT  ;
       '\<Control de Inventario    '  ;
       SKIP FOR (xnnn = 'A5' .OR.  ;
       xnnn = 'A1')
DEFINE BAR 3 OF menu02 PROMPT  ;
       '\<Gesti�n de Ventas        '
DEFINE BAR 4 OF menu02 PROMPT  ;
       '\<Orden de Reparaci�n      '  ;
       SKIP FOR (xnnn = 'A5' .OR.  ;
       xnnn = 'A1')
ON SELECTION POPUP menu02 DO MENU20 WITH;
BAR()
DEFINE POPUP menu021 FROM 09, 30  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu021 PROMPT  ;
       '\<Pedidos Sugerido seg�n Stock'
DEFINE BAR 2 OF menu021 PROMPT  ;
       '\<Mantenci�n de Pedidos'
DEFINE BAR 3 OF menu021 PROMPT  ;
       '\<Confirmaci�n de Pedidos'
DEFINE BAR 4 OF menu021 PROMPT  ;
       '\<Anulaci�n de Pedidos'
DEFINE BAR 5 OF menu021 PROMPT  ;
       'Pe\<didos de Sucursales'
ON SELECTION POPUP menu021 DO MENU021;
WITH BAR()
DEFINE POPUP menu022 FROM 10, 30  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu022 PROMPT  ;
       '\<Recepci�n de Productos     '
DEFINE BAR 2 OF menu022 PROMPT  ;
       '\<Despacho de Productos      '
DEFINE BAR 3 OF menu022 PROMPT  ;
       '\<Ajustes y Mermas           '
DEFINE BAR 4 OF menu022 PROMPT  ;
       '\<Traspaso Entre Bodegas     '
DEFINE BAR 5 OF menu022 PROMPT  ;
       '\<Consumos Internos          '
DEFINE BAR 6 OF menu022 PROMPT  ;
       '\<Inventario F�sico         '
ON SELECTION POPUP menu022 DO MENU022;
WITH BAR()
DEFINE POPUP menu023 FROM 11, 30  ;
       TO 21, 57 COLOR SCHEME 20
DEFINE BAR 1 OF menu023 PROMPT  ;
       'Fa\<cturaci�n Autom�tica    '  ;
       SKIP FOR (xnnn = 'A5')
DEFINE BAR 2 OF menu023 PROMPT  ;
       'Fac\<turaci�n en D�lares    '
DEFINE BAR 3 OF menu023 PROMPT  ;
       '\<Pagos a Cuenta            '  ;
       SKIP FOR (xnnn = 'A5')
DEFINE BAR 4 OF menu023 PROMPT  ;
       '\<Anulaci�n de Ventas       '
DEFINE BAR 5 OF menu023 PROMPT  ;
       '\<Facturaci�n - Manual    '
DEFINE BAR 6 OF menu023 PROMPT  ;
       'Pa\<go a Cuenta - Manual    '
DEFINE BAR 7 OF menu023 PROMPT  ;
       '\-'
DEFINE BAR 8 OF menu023 PROMPT  ;
       '\<Notas de Cr�dito         '  ;
       SKIP FOR (xnnn = 'A1' .OR.  ;
       xnnn = 'A2' .OR. xnnn =  ;
       'A3')
DEFINE BAR 9 OF menu023 PROMPT  ;
       'N\<otas de D�bito           '  ;
       SKIP FOR (xnnn = 'A1' .OR.  ;
       xnnn = 'A2' .OR. xnnn =  ;
       'A3')
DEFINE BAR 10 OF menu023 PROMPT  ;
       '\-'
DEFINE BAR 11 OF menu023 PROMPT  ;
       '\<Listas de Precio         '  ;
       SKIP FOR (xnnn = 'A1' .OR.  ;
       xnnn = 'A5')
ON SELECTION POPUP menu023 DO MENU023;
WITH BAR()
DEFINE POPUP menu024 FROM 12, 30  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu024 PROMPT  ;
       '\<Devoluci�n de Repuestos'
DEFINE BAR 2 OF menu024 PROMPT  ;
       '\<Anulaci�n de Repuestos'
ON SELECTION POPUP menu024 DO MENU024;
WITH BAR()
DEFINE POPUP menu0221 FROM 17, 43  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu0221 PROMPT  ;
       '\<Generaci�n y Modificaci�n '
DEFINE BAR 2 OF menu0221 PROMPT  ;
       '\<Proceso de Cierre'
ON SELECTION POPUP menu0221 DO MENU0221;
WITH BAR()
DEFINE POPUP menu0222 FROM 14, 43  ;
       TO 21, 73 COLOR SCHEME 20
DEFINE BAR 1 OF menu0222 PROMPT  ;
       '\<Emisi�n de Listados '
DEFINE BAR 2 OF menu0222 PROMPT  ;
       'Emisi�n de \<Stickers'
DEFINE BAR 3 OF menu0222 PROMPT  ;
       '\<Generaci�n de Archivos '
DEFINE BAR 4 OF menu0222 PROMPT  ;
       '\<Ingreso de Difer. de Inventario'
DEFINE BAR 5 OF menu0222 PROMPT  ;
       '\<Reporte de Difer. de Inventario'
DEFINE BAR 6 OF menu0222 PROMPT  ;
       'A\<ctualizaci�n de Toma Inventario'
DEFINE BAR 7 OF menu0222 PROMPT  ;
       'Reporte de Toma Inven. \<Valorizado'
ON SELECTION POPUP menu0222 DO MENU0222;
WITH BAR()
DEFINE POPUP menu0231 FROM 16, 50  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu0231 PROMPT  ;
       'Por \<Devoluci�n'
DEFINE BAR 2 OF menu0231 PROMPT  ;
       'Por \<Cambio Precio'
DEFINE BAR 3 OF menu0231 PROMPT  ;
       '\<Otras Razones'
ON SELECTION POPUP menu0231 DO MENU0231;
WITH BAR()
DEFINE POPUP menu0232 FROM 16, 50  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu0232 PROMPT  ;
       '\<Mantenci�n Listas'
DEFINE BAR 2 OF menu0232 PROMPT  ;
       '\<Generaci�n Autom�tica'
DEFINE BAR 3 OF menu0232 PROMPT  ;
       '\<Productos en Oferta'
ON SELECTION POPUP menu0232 DO MENU0232;
WITH BAR()
DEFINE POPUP menu03 FROM 07, 33  ;
       TO 21, 58 COLOR SCHEME 20
DEFINE BAR 1 OF menu03 PROMPT  ;
       '\<Tablas Generales       '
DEFINE BAR 2 OF menu03 PROMPT  ;
       '\<Stocks Actuales'
DEFINE BAR 3 OF menu03 PROMPT  ;
       '\<Documentos             '
DEFINE BAR 4 OF menu03 PROMPT  ;
       '\<Movimiento de Repuesto'
DEFINE BAR 5 OF menu03 PROMPT  ;
       'Modelos \<Utilizados'
DEFINE BAR 6 OF menu03 PROMPT  ;
       '\Productos Alternativos'
DEFINE BAR 7 OF menu03 PROMPT  ;
       '\<Niveles de Stock'
DEFINE BAR 8 OF menu03 PROMPT  ;
       '\<Lista de Precios'
DEFINE BAR 9 OF menu03 PROMPT  ;
       'Pr\<oductos por Lista'
DEFINE BAR 10 OF menu03 PROMPT  ;
       '\<Estad�sticas'
DEFINE BAR 11 OF menu03 PROMPT  ;
       '\<Importaciones          '
DEFINE BAR 12 OF menu03 PROMPT  ;
       'L. Precios SAMSUN\<G'  ;
       SKIP FOR (xnnn = 'A1')
DEFINE BAR 13 OF menu03 PROMPT  ;
       'L. Precios \<AIWA' SKIP  ;
       FOR (xnnn = 'A1')
DEFINE BAR 14 OF menu03 PROMPT  ;
       'Orden de \<Reparaci�n    '  ;
       SKIP FOR (xnnn = 'A1')
ON SELECTION POPUP menu03 DO MENU30 WITH;
BAR()
DEFINE POPUP menu031 FROM 11, 48  ;
       TO 20, 73 COLOR SCHEME 20
DEFINE BAR 1 OF menu031 PROMPT  ;
       '\<Recepci�n Productos'  ;
       SKIP FOR (xnnn = 'A1')
DEFINE BAR 2 OF menu031 PROMPT  ;
       '\<Despacho Productos'  ;
       SKIP FOR (xnnn = 'A1')
DEFINE BAR 3 OF menu031 PROMPT  ;
       '\<Ajustes y Mermas' SKIP  ;
       FOR (xnnn = 'A1')
DEFINE BAR 4 OF menu031 PROMPT  ;
       '\<Traspaso entre Bodegas'  ;
       SKIP FOR (xnnn = 'A1')
DEFINE BAR 5 OF menu031 PROMPT  ;
       '\<Consumos Internos' SKIP  ;
       FOR (xnnn = 'A1')
DEFINE BAR 6 OF menu031 PROMPT  ;
       '\<Pedidos de Clientes'  ;
       SKIP FOR (xnnn = 'A1')
DEFINE BAR 7 OF menu031 PROMPT  ;
       '\<Ordenes de Importaci�n'  ;
       SKIP FOR (xnnn = 'A1')
DEFINE BAR 8 OF menu031 PROMPT  ;
       'Documento de \<Venta'
DEFINE BAR 9 OF menu031 PROMPT  ;
       'N\<otas de Abono' SKIP  ;
       FOR (xnnn = 'A1')
ON SELECTION POPUP menu031 DO MENU301;
WITH BAR()
DEFINE POPUP menu037 FROM 15, 48  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu037 PROMPT  ;
       ' Compras '
DEFINE BAR 2 OF menu037 PROMPT  ;
       ' Ventas  '
DEFINE BAR 3 OF menu037 PROMPT  ;
       ' Compras/Ventas '
ON SELECTION POPUP menu037 DO AGCC0207;
WITH BAR()
DEFINE POPUP menu310a FROM 07, 20  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu310a PROMPT  ;
       'Tablas'
DEFINE BAR 2 OF menu310a PROMPT  ;
       'Indicadores'
ON SELECTION POPUP menu310a DO MENU401;
WITH BAR()
DEFINE POPUP menu0311 FROM 15, 50  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu0311 PROMPT  ;
       '\<Repuestos'
DEFINE BAR 2 OF menu0311 PROMPT  ;
       '\<Control de Pedidos'
DEFINE BAR 3 OF menu0311 PROMPT  ;
       'Pedidos \<por C�digo'
DEFINE BAR 4 OF menu0311 PROMPT  ;
       '\<Back Orders'
ON SELECTION POPUP menu0311 DO MENU311;
WITH BAR()
DEFINE POPUP menu0313 FROM 16, 57  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu0313 PROMPT  ;
       'Consumo de Repuestos'
DEFINE BAR 2 OF menu0313 PROMPT  ;
       'Solicitud de Servicio'
DEFINE BAR 3 OF menu0313 PROMPT  ;
       'Orden de Reparaci�n'
DEFINE BAR 4 OF menu0313 PROMPT  ;
       'Presupuestos'
ON SELECTION POPUP menu0313 DO MENU313;
WITH BAR()
DEFINE POPUP menu04 FROM 07, 47  ;
       TO 21, 73 COLOR SCHEME 20
DEFINE BAR 1 OF menu04 PROMPT  ;
       '\<Tablas Generales        '  ;
       SKIP FOR (xnnn = 'A2' .OR.  ;
       xnnn = 'A3' .OR. xnnn =  ;
       'A6' .OR. xnnn = 'A5' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 2 OF menu04 PROMPT  ;
       '\<Productos Alternativos  '  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A5' .OR. xnnn =  ;
       'A8')
DEFINE BAR 3 OF menu04 PROMPT  ;
       '\<Importaciones           '  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A5' .OR. xnnn =  ;
       'A8')
DEFINE BAR 4 OF menu04 PROMPT  ;
       '\<Stock por Centros' SKIP  ;
       FOR (xnnn = 'A6' .OR. xnnn =  ;
       'A8')
DEFINE BAR 5 OF menu04 PROMPT  ;
       '\<Existencias' SKIP FOR  ;
       (xnnn = 'A6' .OR. xnnn =  ;
       'A8')
DEFINE BAR 6 OF menu04 PROMPT  ;
       '\<Exist. C/Quiebre Simple'  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 7 OF menu04 PROMPT  ;
       '\<Mov. por Cod. Transac.'  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 8 OF menu04 PROMPT  ;
       '\<An�lisis de Existencias'  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 9 OF menu04 PROMPT  ;
       '\<Planilla de Ventas '  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 10 OF menu04 PROMPT  ;
       '\<Rotaci�n Inventario'  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 11 OF menu04 PROMPT  ;
       'Consumo de Rptos. en O/R'  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 12 OF menu04 PROMPT  ;
       'Co\<nsumos por Centro     '  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 13 OF menu04 PROMPT  ;
       '\<Listas de Precios' SKIP  ;
       FOR (xnnn = 'A6' .OR. xnnn =  ;
       'A5')
DEFINE BAR 14 OF menu04 PROMPT  ;
       'Prod. Pend\<ientes' SKIP  ;
       FOR (xnnn = 'A6' .OR. xnnn =  ;
       'A8')
DEFINE BAR 15 OF menu04 PROMPT  ;
       'Res\<�menes de Ventas'  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 16 OF menu04 PROMPT  ;
       'Gu�\<as no Facturadas'  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 17 OF menu04 PROMPT  ;
       'Productos no \<Vendidos'  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 18 OF menu04 PROMPT  ;
       'Estad�sticas' SKIP FOR  ;
       (xnnn = 'A6' .OR. xnnn =  ;
       'A8')
DEFINE BAR 19 OF menu04 PROMPT  ;
       'Ran\<king de Venta' SKIP  ;
       FOR (xnnn = 'A2' .OR. xnnn =  ;
       'A3' .OR. xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 20 OF menu04 PROMPT  ;
       'Li\<bro de Venta          '  ;
       SKIP FOR (xnnn = 'A2' .OR.  ;
       xnnn = 'A3' .OR. xnnn =  ;
       'A8')
DEFINE BAR 21 OF menu04 PROMPT  ;
       '  Contables  ' SKIP FOR  ;
       (xnnn = 'A6' .OR. xnnn =  ;
       'A8')
DEFINE BAR 22 OF menu04 PROMPT  ;
       'Costo de Ventas' SKIP FOR  ;
       (xnnn = 'A6' .OR. xnnn =  ;
       'A8')
DEFINE BAR 23 OF menu04 PROMPT  ;
       'Movimie\<nto de Almac�n'  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 24 OF menu04 PROMPT  ;
       'Compra,Consumo,\<Dev.Repuesto'  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A8')
DEFINE BAR 25 OF menu04 PROMPT  ;
       'Doc\<umentos              '  ;
       SKIP FOR (xnnn = 'A6' .OR.  ;
       xnnn = 'A5' .OR. xnnn =  ;
       'A8')
ON SELECTION POPUP menu04 DO MENU40 WITH;
BAR()
DEFINE POPUP menu09 FROM 07, 47  ;
       TO 21, 77 COLOR SCHEME 20
DEFINE BAR 1 OF menu09 PROMPT  ;
       '\<Totales x Doc. y Tipo de Mov.'
DEFINE BAR 2 OF menu09 PROMPT  ;
       '\<Detalle x Doc. y Tipo de Mov.'
DEFINE BAR 3 OF menu09 PROMPT  ;
       '\<Movimiento de Inventarios'
DEFINE BAR 4 OF menu09 PROMPT  ;
       'R\<egistro de Compras'
DEFINE BAR 5 OF menu09 PROMPT  ;
       'N\<uevo Kardex'
DEFINE BAR 6 OF menu09 PROMPT  ;
       '\<Cr�ditos del mes '
DEFINE BAR 7 OF menu09 PROMPT  ;
       'Re\<gistro de Ventas'
DEFINE BAR 8 OF menu09 PROMPT  ;
       'C\<osto de Ventas'
DEFINE BAR 09 OF menu09 PROMPT  ;
       '\<Stock Valorizado'
DEFINE BAR 10 OF menu09 PROMPT  ;
       'R\<ptos. Pendientes x Facturar'
DEFINE BAR 11 OF menu09 PROMPT  ;
       '\<Ajustes de Almac�n'
ON SELECTION POPUP menu09 DO MENU09 WITH;
BAR()
DEFINE POPUP menu091 FROM 16, 66  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu091 PROMPT  ;
       '\<Principal'
DEFINE BAR 2 OF menu091 PROMPT  ;
       '\<Sucursales'
ON SELECTION POPUP menu091 DO MENU091;
WITH BAR()
DEFINE POPUP menu310 FROM 07, 34  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu310 PROMPT  ;
       '\<Tablas'
DEFINE BAR 2 OF menu310 PROMPT  ;
       '\<Indicadores'
ON SELECTION POPUP menu310 DO MENU401;
WITH BAR()
DEFINE POPUP menu350 FROM 11, 12  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu350 PROMPT  ;
       '\<Valorizadas/CP/UC/CR'
DEFINE BAR 2 OF menu350 PROMPT  ;
       '\<Estado de Inventario'
ON SELECTION POPUP menu350 DO AGCR0205;
WITH BAR()
DEFINE POPUP menu370 FROM 11, 12  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu370 PROMPT  ;
       '\<Stock en Cero'
DEFINE BAR 2 OF menu370 PROMPT  ;
       'S\<tock Bajo el M�nimo'
DEFINE BAR 3 OF menu370 PROMPT  ;
       'St\<ock Sobre el M�ximo'
ON SELECTION POPUP menu370 DO AGCR0207;
WITH BAR()
DEFINE POPUP menu403 FROM 09, 24  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu403 PROMPT  ;
       '\<Pedidos por Proveedor'
DEFINE BAR 2 OF menu403 PROMPT  ;
       '\<Estado de Repuestos Pedidos'
DEFINE BAR 3 OF menu403 PROMPT  ;
       '\<Nivel de Servicio'
ON SELECTION POPUP menu403 DO MENU403;
WITH BAR()
DEFINE POPUP menu430 FROM 11, 12  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu430 PROMPT  ;
       'Por Vendedores'
DEFINE BAR 2 OF menu430 PROMPT  ;
       'Por Clientes '
DEFINE BAR 3 OF menu430 PROMPT  ;
       'Por Fechas/Emi./Venc'
ON SELECTION POPUP menu430 DO AGCR0213;
WITH BAR()
DEFINE POPUP menu470 FROM 11, 12  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu470 PROMPT  ;
       ' Compras '
DEFINE BAR 2 OF menu470 PROMPT  ;
       ' Ventas  '
DEFINE BAR 3 OF menu470 PROMPT  ;
       ' Compras/Ventas '
ON SELECTION POPUP menu470 DO AGCR0217;
WITH BAR()
DEFINE POPUP menu480 FROM 11, 12  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu480 PROMPT  ;
       'Por Producto'
DEFINE BAR 2 OF menu480 PROMPT  ;
       'Por Cliente'
DEFINE BAR 3 OF menu480 PROMPT  ;
       'Por Vendedor'
DEFINE BAR 4 OF menu480 PROMPT  ;
       'Por Proveedor'
ON SELECTION POPUP menu480 DO AGCR0218;
WITH BAR()
DEFINE POPUP menu490 FROM 14, 22  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu490 PROMPT  ;
       'Recepci�n de Productos'
DEFINE BAR 2 OF menu490 PROMPT  ;
       'Despacho de Productos'
DEFINE BAR 3 OF menu490 PROMPT  ;
       '\<Traspaso entre Bodegas'
DEFINE BAR 4 OF menu490 PROMPT  ;
       'Ajustes y Mermas'
DEFINE BAR 5 OF menu490 PROMPT  ;
       'Consumo Interno'
ON SELECTION POPUP menu490 DO MENU490;
WITH BAR()
DEFINE POPUP menu420 FROM 10, 30  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu420 PROMPT  ;
       'Registro de \<Ventas'
DEFINE BAR 2 OF menu420 PROMPT  ;
       '\<Total Ventas' SKIP FOR  ;
       (xnnn = 'A6')
DEFINE BAR 3 OF menu420 PROMPT  ;
       '\<Res�men Ventas' SKIP  ;
       FOR (xnnn = 'A6')
DEFINE BAR 4 OF menu420 PROMPT  ;
       'Venta de \<Sucursales'  ;
       SKIP FOR (xnnn = 'A6')
DEFINE BAR 5 OF menu420 PROMPT  ;
       '\<Margen Comercial de Principal'  ;
       SKIP FOR (xnnn = 'A6')
DEFINE BAR 6 OF menu420 PROMPT  ;
       'Margen Comercial de \<Sucursales'  ;
       SKIP FOR (xnnn = 'A6')
DEFINE BAR 7 OF menu420 PROMPT  ;
       '\<Proceso Ventas' SKIP  ;
       FOR (xnnn = 'A6')
DEFINE BAR 8 OF menu420 PROMPT  ;
       'Pe\<dido de Ventas' SKIP  ;
       FOR (xnnn = 'A6')
ON SELECTION POPUP menu420 DO MENU420;
WITH BAR()
DEFINE POPUP menu05 FROM 07, 57  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu05 PROMPT  ;
       '\<Optimizaci�n de Archivos'  ;
       SKIP FOR xnnn = 'A8' .OR.  ;
       xnnn = 'A2'
DEFINE BAR 2 OF menu05 PROMPT  ;
       '\<Admin. de Seguridad'  ;
       SKIP FOR xnnn = 'A8' .OR.  ;
       xnnn = 'A2'
DEFINE BAR 3 OF menu05 PROMPT  ;
       '\<Genera Archivo ASCII'  ;
       SKIP FOR xnnn = 'A8' .OR.  ;
       xnnn = 'A2'
DEFINE BAR 4 OF menu05 PROMPT  ;
       '\<Respaldo de Archivos'  ;
       SKIP FOR xnnn = 'A8' .OR.  ;
       xnnn = 'A2'
DEFINE BAR 5 OF menu05 PROMPT  ;
       'R\<estauraci�n de Archivos'  ;
       SKIP FOR xnnn = 'A8' .OR.  ;
       xnnn = 'A2'
DEFINE BAR 6 OF menu05 PROMPT  ;
       '\<Cierres Peri�dicos'  ;
       SKIP FOR xnnn = 'A8' .OR.  ;
       xnnn = 'A2'
DEFINE BAR 7 OF menu05 PROMPT  ;
       '\<Trasferencia de Datos   '
ON SELECTION POPUP menu05 DO MENU50 WITH;
BAR()
DEFINE POPUP menu052 FROM 09, 34  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu052 PROMPT  ;
       '\<Mantenci�n Accesos'
DEFINE BAR 2 OF menu052 PROMPT  ;
       '\<INF. PERFIL USUARIOS'
ON SELECTION POPUP menu052 DO MENU502;
WITH BAR()
DEFINE POPUP menu053 FROM 15, 35  ;
       COLOR SCHEME 20
DEFINE BAR 1 OF menu053 PROMPT  ;
       '\<Generar Ficha de Productos'
DEFINE BAR 2 OF menu053 PROMPT  ;
       '\<Actualizar Ficha de Productos'  ;
       SKIP FOR xnnn = 'A8' .OR.  ;
       xnnn = 'A2'
DEFINE BAR 3 OF menu053 PROMPT  ;
       'Generar \<Movimiento Sucursal'  ;
       SKIP FOR xnnn = 'A8' .OR.  ;
       xnnn = 'A2'
DEFINE BAR 4 OF menu053 PROMPT  ;
       'Actualizar Movimiento \<Sucursal'  ;
       SKIP FOR xnnn = 'A8' .OR.  ;
       xnnn = 'A2'
DEFINE BAR 5 OF menu053 PROMPT  ;
       'A\<ctualizar Movimiento de Almacen - Sucursal'
ON SELECTION POPUP menu053 DO MENU503;
WITH BAR()
RETURN
*
PROCEDURE p_menupri
DO WHILE .T.
     ACTIVATE WINDOW pantalla
     @ 4, 0 SAY  ;
       '������������������������������������������������������������������������������Ŀ'
     @ 5, 0 SAY  ;
       '� ACTUALIZACIONES �   PROCESOS   �  CONSULTAS  �   REPORTES   � ADMINISTRACION �'
     @ 6, 0 SAY  ;
       '��������������������������������������������������������������������������������'
     DO p_footer WITH  ;
        '100000000001011000001',  ;
        1
     ACTIVATE SCREEN
     ACTIVATE MENU menu00
     IF LASTKEY() = 27
          HIDE WINDOW footer
          rpt = f_yesno( ;
                'DESEA SALIR DE ALMACEN?' ;
                )
          IF rpt
               EXIT
          ENDIF
     ENDIF
ENDDO
RETURN
*
PROCEDURE menu10
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agca0201
     CASE bar = 2
          DO agca0202
     CASE bar = 3
          DO agca0203
     CASE bar = 4
          DO agca0204
     CASE bar = 5
          DO agca0205
     CASE bar = 6
          DO agca0262
     CASE bar = 7
          DO agca0207
     CASE bar = 8
          DO agca0208
     CASE bar = 9
          DO agca0210
     CASE bar = 10
          DO agca0211
     CASE bar = 11
          DO agca0212
     CASE bar = 12
          DO agca0213
ENDCASE
RETURN
*
PROCEDURE menu20
PARAMETER bar
DO CASE
     CASE bar = 1
          ACTIVATE POPUP menu021
     CASE bar = 2
          ACTIVATE POPUP menu022
     CASE bar = 3
          ACTIVATE POPUP menu023
     CASE bar = 4
          ACTIVATE POPUP menu024
ENDCASE
RETURN
*
PROCEDURE menu021
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcp2211
     CASE bar = 2
          DO agcp2212
     CASE bar = 3
          DO agcp2213
     CASE bar = 4
          DO agcp2214
     CASE bar = 5
          DO agcp2217
ENDCASE
RETURN
*
PROCEDURE menu022
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcp2221
     CASE bar = 2
          DO agcp2222
     CASE bar = 3
          DO agcp2223
     CASE bar = 4
          DO agcp2224
     CASE bar = 5
          DO agcp2225
     CASE bar = 6
          ACTIVATE POPUP menu0222
ENDCASE
RETURN
*
PROCEDURE menu0222
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcp0231
     CASE bar = 2
          DO agcp0232
     CASE bar = 3
          DO agcp0233
     CASE bar = 4
          DO agcp0234
     CASE bar = 5
          DO agcp0235
     CASE bar = 6
          DO agcp0236
     CASE bar = 7
          DO agcp0237
ENDCASE
RETURN
*
PROCEDURE menu023
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcp2240
     CASE bar = 2
          DO agcp2242
     CASE bar = 3
          DO agcp2243
     CASE bar = 4
          DO agcp2241 WITH 1
     CASE bar = 5
          DO agcp2233
     CASE bar = 6
          DO agcp2246
     CASE bar = 8
          ACTIVATE POPUP menu0231
     CASE bar = 9
          DO agcp2239
     CASE bar = 10
          ACTIVATE POPUP menu0232
ENDCASE
RETURN
*
PROCEDURE menu024
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcp0242
     CASE bar = 2
          DO agcp0243
ENDCASE
RETURN
*
PROCEDURE menu0231
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcp2361
     CASE bar = 2
          DO agcp2362
     CASE bar = 3
          DO agcp2363
ENDCASE
RETURN
*
PROCEDURE menu0232
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcp2237
     CASE bar = 2
          DO agcp2238
     CASE bar = 3
          DO agcp2244
ENDCASE
RETURN
*
PROCEDURE menu30
PARAMETER bar
DO CASE
     CASE bar = 1
          ACTIVATE POPUP menu310a
     CASE bar = 2
          DO agcc0202
     CASE bar = 3
          ACTIVATE POPUP menu031
     CASE bar = 4
          DO agcc0204
     CASE bar = 5
          DO agcc0205
     CASE bar = 6
          DO agcc0206
     CASE bar = 7
          DO agcc0207
     CASE bar = 8
          DO agcc0208
     CASE bar = 9
          DO agcc0209
     CASE bar = 10
          DO agcc0210
     CASE bar = 11
          ACTIVATE POPUP menu0311
     CASE bar = 12
          DO agcc0214
     CASE bar = 13
          DO agcc0215
     CASE bar = 14
          ACTIVATE POPUP menu0313
ENDCASE
RETURN
*
PROCEDURE menu301
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcc3021 WITH bar
     CASE bar = 2
          DO agcc2022 WITH bar
     CASE bar = 3
          DO agcc2023 WITH bar
     CASE bar = 4
          DO agcc2024 WITH bar
     CASE bar = 5
          DO agcc2025 WITH bar
     CASE bar = 6
          DO agcc2026 WITH bar
     CASE bar = 7
          DO agcc2027 WITH bar
     CASE bar = 8
          DO agcp2241 WITH 2
     CASE bar = 9
          DO agcc2029 WITH bar
ENDCASE
RETURN
*
PROCEDURE menu311
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcc2131
     CASE bar = 2
          DO agcc2132
     CASE bar = 3
          DO agcc2133
     CASE bar = 4
          DO agcc2134
ENDCASE
RETURN
*
PROCEDURE menu313
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcc0377
     CASE bar = 2
     CASE bar = 3
     CASE bar = 4
ENDCASE
RETURN
*
PROCEDURE menu40
PARAMETER bar
DO CASE
     CASE bar = 1
          ACTIVATE POPUP menu310
     CASE bar = 2
          DO agcr0202
     CASE bar = 3
          ACTIVATE POPUP menu403
     CASE bar = 4
          DO agcr0204
     CASE bar = 5
          DO agcr0205
     CASE bar = 6
          DO agcr0222
     CASE bar = 7
          DO agcr0206
     CASE bar = 8
          DO agcr0207
     CASE bar = 9
          DO agcr0208
     CASE bar = 10
          DO agcr0209
     CASE bar = 11
          DO agcr0210
     CASE bar = 12
          DO agcr0211
     CASE bar = 13
          DO agcr0212
     CASE bar = 14
          DO agcr0213
     CASE bar = 15
          DO agcr0214
     CASE bar = 16
          DO agcr0215
     CASE bar = 17
          DO agcr0216
     CASE bar = 18
          DO agcr0217
     CASE bar = 19
          DO agcr0218
     CASE bar = 20
          ACTIVATE POPUP menu420
     CASE bar = 21
          ACTIVATE POPUP menu09
     CASE bar = 22
          DO agcr2100
     CASE bar = 23
          DO agcr0231
     CASE bar = 24
          DO agcr0233
     CASE bar = 25
          ACTIVATE POPUP menu490
ENDCASE
CLOSE DATABASES
RETURN
*
PROCEDURE menu09
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcr0225
     CASE bar = 2
          DO agcr0224
     CASE bar = 3
          DO agcr0221
     CASE bar = 4
          DO agcr0243
     CASE bar = 5
          DO agcr0226
     CASE bar = 6
          DO agcr0229
     CASE bar = 7
          DO agcr0234
     CASE bar = 8
          ACTIVATE POPUP menu091
     CASE bar = 9
          DO agcr0237
     CASE bar = 10
          DO agcr0238
     CASE bar = 11
          DO agcr0242
ENDCASE
RETURN
*
PROCEDURE menu091
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcr0232
     CASE bar = 2
          DO agcr0235
ENDCASE
RETURN
*
PROCEDURE menu401
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcr2011
     CASE bar = 2
          DO agcr2012
ENDCASE
RETURN
*
PROCEDURE menu403
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcr2031
     CASE bar = 2
          DO agcr2032
     CASE bar = 3
          DO agcr2033
ENDCASE
RETURN
*
PROCEDURE menu420
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcr4201
     CASE bar = 2
          DO agcr4202
     CASE bar = 3
          DO agcr4203
     CASE bar = 4
          DO agcr4204
     CASE bar = 5
          DO agcr4205
     CASE bar = 6
          DO agcr4207
     CASE bar = 7
          DO agcr4206
     CASE bar = 8
          DO agcr4208
ENDCASE
RETURN
*
PROCEDURE menu490
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcr2501
     CASE bar = 2
     CASE bar = 3
          DO agcr2503
     CASE bar = 4
ENDCASE
RETURN
*
PROCEDURE menu50
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcs0201
     CASE bar = 2
          RETURN
          DO agcs0202
     CASE bar = 3
          RETURN
          DO agcs0203
     CASE bar = 4
          RETURN
          DO agcs0204
     CASE bar = 5
          RETURN
          DO agcs0205
     CASE bar = 6
          RETURN
          DO agcs0206
     CASE bar = 7
          ACTIVATE POPUP menu053
ENDCASE
RETURN
*
PROCEDURE menu502
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcs2011
     CASE bar = 2
          DO agcs2012
ENDCASE
RETURN
*
PROCEDURE menu503
PARAMETER bar
DO CASE
     CASE bar = 1
          DO agcs3011
     CASE bar = 2
          DO agcs3012
     CASE bar = 3
          DO agcs3013
     CASE bar = 4
          DO agcs3014
     CASE bar = 5
          DO agcs3015
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

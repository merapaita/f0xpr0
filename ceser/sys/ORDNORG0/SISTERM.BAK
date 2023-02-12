*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
SET PATH TO ..\..\BASES
CLEAR ALL
SET REPROCESS TO -1
SET PROCEDURE TO funcione
CLEAR MACROS
SET SAFETY OFF
SET ECHO OFF
SET TALK OFF
SET CURRENCY TO 'S/.'
SET NOTIFY OFF
SET BELL OFF
SET DATE briti
SET SCOREBOARD OFF
SET DELETED ON
SET STATUS OFF
SET ESCAPE OFF
SET EXCLUSIVE OFF
SET REFRESH TO 2
SET SYSMENU ON
SET CENTURY ON
PUBLIC sw_factura, rge_razsoc,  ;
       rge_abrev, rge_calle,  ;
       rge_distri, rge_provin,  ;
       rge_lispre
PUBLIC rge_monbas, rge_codalm,  ;
       rge_lptfac, rge_lptgui,  ;
       rge_lptped, rge_lptbol
PUBLIC rge_lptdes, rge_punemi,  ;
       rge_pormax, rge_vicodi,  ;
       rge_nroruc
mono = 'W/N,N/W,N'
set color to &mono
USE GC_PAR00
DO p_menpar
USE
SET COLOR OF SCHEME 5 TO N/W, N/W, N/W,;
W+/W, N/W, W/N, W+/W, N+/N, N/W, W+/W
SET COLOR OF SCHEME 6 TO N/W, N/W, N/W,;
W+/W, N/W, W/N, W+/W, N+/N, N/W, W+/W
SET COLOR OF SCHEME 7 TO W+/N, W+/N, W/N,;
W/N, N/W, N+/W, W+/N, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 8 TO N/W, N/W, N/W,;
N/W, W/N, W/N, N/W, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 9 TO W+/N, W+/N, W/N,;
W+/N, W/N, N/W, W/N, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 10 TO W+/N, N/W, N/W,;
W+/N, W+/N, N/W, N/W, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 12 TO W+/N, W+/N,;
W+/N, W+/N, W/N, N+/W, N+/W, N+/N, W+/N,;
W+/N
SET COLOR OF SCHEME 15 TO N/W, N/W, N/W,;
N/W, W/N, W/N, N/W, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 16 TO N/W, N/W, N/W,;
N/W, W/N, W/N, N/W, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 17 TO N/W, N/W, N/W,;
N/W, W/N, W/N, N/W, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 19 TO W+/N, W+/N,;
N+/W, W+/N, W+/N, N/W, W/N, W+/N, N/W
SET COLOR OF SCHEME 20 TO W+/N, W+/N,;
W+/N, W+/N, W+/N, N/W, W/N, N+/W, N+/W,;
W/N
SET COLOR OF SCHEME 23 TO W+/N, N+/W,;
N+/W, N+/W, W+/N, W+/N, N+/W, W+/N, N+/W,;
W+/N
SET COLOR OF SCHEME 21 TO W/N, W+/N, W/N,;
W/N, W/N, N/W, W/N, N+/N, W+/N, N+/N
DEFINE WINDOW pantall FROM 0, 0  ;
       TO 24, 222 FLOAT ZOOM  ;
       COLOR SCHEME 21
color1 = 'w/b'
color2 = 'b/w'
color3 = 'w+/b'
color4 = 'b*/w'
color5 = 'W/N, N+/W, W+/N, W+/N, W/N, U+/N, B+/W, -, W+/N, W/N'
color6 = 'b*/w'
chisof1 = 'DPTO.DE SISTEMAS LIMA-PERU'
chisof2 = 'Av. Rep. de Panam  4123'
PUBLIC empre1, empre2, empre3,  ;
       empre4, empre5, empre6,  ;
       empre7, empre8, empre9,  ;
       empre10, empre11, empre12,  ;
       empre13
USE SHARED st_iparg
empre1 = TRIM(sys_razsoc)
empre2 = TRIM(sys_nomcal)
empre3 = sys_tmpdem
empre4 = sys_codlpg
empre5 = sys_codlpf
empre6 = sys_codtal
empre7 = sys_fecgar
empre8 = sys_simmon
empre9 = sys_codimp
empre10 = sys_desmon
empre11 = sys_tmppre
empre12 = sys_garmax
empre13 = sys_simomb
USE
PUBLIC x_1, x_2, x_3, x_4, x_5,  ;
       x_6, x_7, x_8, x_9, x_10,  ;
       x_11, x_12, x_13, x_14
PUBLIC ppalqz, nivell
nivell = 'A0'
ppalqz = .T.
DO ingreso
IF nivell = 'A0'
     ppalqz = .F.
ENDIF
DO WHILE ppalqz
     DO menu
ENDDO
CLOSE DATABASES
CLEAR ALL
CLEAR
RUN del orden?.dbf > null
RUN del orden??.dbf > null
RUN del orden???.dbf > null
RUN del orden?.idx > null
RUN del orden??.idx > null
RUN del orden???.idx > null
RUN del orden?.fpt > null
RUN del orden??.fpt > null
RUN del orden???.fpt > null
RUN del *.tmp   > null
RETURN
*
FUNCTION menu
DEFINE WINDOW pantalla FROM 04,  ;
       00 TO 06, 79 IN screen  ;
       NONE
SET COLOR TO W+/N
tit1 = 'SISTEMA DE REPARACIONES'
@ 1, 26, 3, 56 BOX 'ÚÄ¿³ÙÄÀ³ '
@ 2, 26 + ((32 - LEN(tit1)) / 2)  ;
  SAY tit1 COLOR W+/N 
DEFINE MENU menu00 COLOR SCHEME  ;
       21
DEFINE PAD tabpad OF menu00  ;
       PROMPT 'ACTUALIZACIONES'  ;
       AT 5, 01 SKIP FOR (nivell =  ;
       'A0' .OR. nivell = 'A5'  ;
       .OR. nivell = 'A6')
DEFINE PAD ordpad OF menu00  ;
       PROMPT 'PROCESOS' AT 5, 21  ;
       SKIP FOR (nivell = 'A0'  ;
       .OR. nivell = 'A6')
DEFINE PAD pogpad OF menu00  ;
       PROMPT 'CONSULTAS' AT 5,  ;
       35 SKIP FOR nivell = 'A0'
DEFINE PAD logpad OF menu00  ;
       PROMPT 'REPORTES' AT 5, 50  ;
       SKIP FOR (nivell = 'A0')
DEFINE PAD reppad OF menu00  ;
       PROMPT 'ADMINISTRACION' AT  ;
       5, 63
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
       COLOR SCHEME 21
DEFINE BAR 1 OF menu01 PROMPT  ;
       'Par metros \<Generales'  ;
       SKIP FOR nivell <> 'A7'
DEFINE BAR 2 OF menu01 PROMPT  ;
       '\<Tablas Generales' SKIP  ;
       FOR nivell <> 'A7'
DEFINE BAR 3 OF menu01 PROMPT  ;
       'Registro de \<Modelos'  ;
       SKIP FOR ((nivell = 'A2'  ;
       .AND. SUBSTR(users, 1, 6) <>  ;
       'HOGPCH' .AND.  ;
       SUBSTR(users, 1, 6) <>  ;
       'TALMSR') .OR. nivell =  ;
       'A1 ')
DEFINE BAR 4 OF menu01 PROMPT  ;
       'Registro Cliente/\<Proveedor'  ;
       SKIP FOR ((nivell = 'A2'  ;
       .AND. SUBSTR(users, 1, 3) <>  ;
       'COM') .OR. nivell =  ;
       'A5')
DEFINE BAR 5 OF menu01 PROMPT  ;
       'Registro T‚\<cnicos' SKIP  ;
       FOR (nivell = 'A1' .OR.  ;
       nivell = 'A3' .OR. nivell =  ;
       'A8' .OR. nivell = 'A9'  ;
       .OR. nivell = 'A11')
DEFINE BAR 6 OF menu01 PROMPT  ;
       'Cat logo de \<S¡ntomas'  ;
       SKIP FOR nivell <> 'A7'
DEFINE BAR 7 OF menu01 PROMPT  ;
       'Registro de Mano de \<Obra'  ;
       SKIP FOR (nivell = 'A1'  ;
       .OR. nivell = 'A9' .OR.  ;
       nivell = 'A11')
DEFINE BAR 8 OF menu01 PROMPT  ;
       '\-'
DEFINE BAR 9 OF menu01 PROMPT  ;
       '\Exporta Registros de Clientes'  ;
       SKIP FOR nivell <> 'A7'
DEFINE BAR 10 OF menu01 PROMPT  ;
       'Exporta Estadist. AIWA'  ;
       SKIP FOR nivell <> 'A7'
DEFINE BAR 11 OF menu01 PROMPT  ;
       '\Exporta S/Servicio' SKIP  ;
       FOR nivell <> 'A7'
DEFINE BAR 12 OF menu01 PROMPT  ;
       '\Actualiza S/Servicio'  ;
       SKIP FOR nivell <> 'A7'
DEFINE BAR 13 OF menu01 PROMPT  ;
       '\-' SKIP FOR nivell <>  ;
       'A7'
DEFINE BAR 14 OF menu01 PROMPT  ;
       'T\<ipo de Cambio' SKIP  ;
       FOR nivell <> 'A7'
ON SELECTION POPUP menu01 DO MENU10 WITH;
BAR()
DEFINE POPUP menu017 FROM 15, 20  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu017 PROMPT  ;
       '\<Actualizaci¢n Manual'
DEFINE BAR 2 OF menu017 PROMPT  ;
       '\<Actualizaci¢n Autom tica'
ON SELECTION POPUP menu017 DO MENU017;
WITH BAR()
DEFINE POPUP menu02 FROM 07, 18  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu02 PROMPT  ;
       'Solicitud de \<Servicio    '  ;
       SKIP FOR (nivell = 'A2'  ;
       .OR. nivell = 'A5' .OR.  ;
       nivell = 'A8') .AND. users <>  ;
       'RECB¥Y' .AND.  ;
       SUBSTR(users, 1, 6) <>  ;
       'TALMSR' .AND. users <>  ;
       'TALVUM' .AND. users <>  ;
       'TALIGP' .AND.  ;
       SUBSTR(users, 1, 3) <>  ;
       'COM'
DEFINE BAR 2 OF menu02 PROMPT  ;
       'Solicitud de \<Presupuesto '  ;
       SKIP FOR (nivell = 'A1 '  ;
       .OR. nivell = 'A9')
DEFINE BAR 3 OF menu02 PROMPT  ;
       '\<Orden de Reparaci¢n      '
DEFINE BAR 4 OF menu02 PROMPT  ;
       'Pedido de \<Repuestos '  ;
       SKIP FOR (nivell = 'A1 '  ;
       .OR. nivell = 'A9')
DEFINE BAR 5 OF menu02 PROMPT  ;
       '\<Cierre de Orden de Rep.  '  ;
       SKIP FOR (nivell = 'A5'  ;
       .OR. nivell = 'A8')
DEFINE BAR 6 OF menu02 PROMPT  ;
       '\<Facturaci¢n              '  ;
       SKIP FOR ((nivell = 'A2'  ;
       .AND. (SUBSTR(users, 1, 3) <>  ;
       'CCA' .AND. users <>  ;
       'COMVCR' .AND. users <>  ;
       'TALVUM' .AND. users <>  ;
       'TALIGP')) .OR. nivell =  ;
       'A5' .OR. nivell = 'A8')
DEFINE BAR 7 OF menu02 PROMPT  ;
       '\<Inventario F¡sico        '  ;
       SKIP FOR (nivell = 'A6'  ;
       .OR. nivell = 'A8')
DEFINE BAR 8 OF menu02 PROMPT  ;
       'Sa\<lida de Art¡culos      '  ;
       SKIP FOR (nivell = 'A2'  ;
       .OR. nivell = 'A5')
DEFINE BAR 9 OF menu02 PROMPT  ;
       '\<Descuento de Art¡culos    '  ;
       SKIP FOR (nivell <> 'A7'  ;
       .AND. nivell <> 'A4')
DEFINE BAR 10 OF menu02 PROMPT  ;
       '\Cambios/Devoluciones      '  ;
       SKIP FOR (nivell <> 'A1'  ;
       .AND. nivell <> 'A7' .AND.  ;
       nivell <> 'A4')
ON SELECTION POPUP menu02 DO MENU20 WITH;
BAR()
DEFINE POPUP menu021 FROM 09, 30  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu021 PROMPT  ;
       '\<Generaci¢n de Solicitud'
DEFINE BAR 2 OF menu021 PROMPT  ;
       '\<Modificaci¢n de Solicitud'
DEFINE BAR 3 OF menu021 PROMPT  ;
       '\<Anulaci¢n de Solicitud'  ;
       SKIP FOR nivell <> 'A7'
DEFINE BAR 4 OF menu021 PROMPT  ;
       '\<Generaci¢n Manual' SKIP  ;
       FOR (nivell <> 'A7' .AND.  ;
       nivell <> 'A4')
ON SELECTION POPUP menu021 DO MENU021;
WITH BAR()
DEFINE POPUP menu022 FROM 10, 30  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu022 PROMPT  ;
       '\<Generaci¢n de Ppto.'
DEFINE BAR 2 OF menu022 PROMPT  ;
       '\<Modificaci¢n de Ppto.'  ;
       SKIP FOR (nivell = 'A5'  ;
       .OR. nivell = 'A8')
DEFINE BAR 3 OF menu022 PROMPT  ;
       '\<Anulaci¢n de Ppto.'  ;
       SKIP FOR (nivell = 'A5'  ;
       .OR. nivell = 'A8')
DEFINE BAR 4 OF menu022 PROMPT  ;
       'Actualizaci¢n de \<Ppto.'  ;
       SKIP FOR (nivell = 'A5'  ;
       .OR. nivell = 'A8' .OR.  ;
       nivell = 'A3' .OR. nivell =  ;
       'A11')
ON SELECTION POPUP menu022 DO MENU022 WITH BAR()
DEFINE POPUP menu023 FROM 11, 30 COLOR SCHEME 21
DEFINE BAR 1 OF menu023 PROMPT '\<Generaci¢n de Ordenes'   SKIP FOR (nivell = 'A1 ' .OR. nivell = 'A5' .OR. nivell = 'A8' .OR. nivell = 'A9')
DEFINE BAR 2 OF menu023 PROMPT '\<Cambio de Estado de O/R' SKIP FOR (nivell = 'A1 ' .OR. nivell = 'A9')
DEFINE BAR 3 OF menu023 PROMPT '\<Anulaci¢n de Ordenes'    SKIP FOR (nivell = 'A1 ' .OR. nivell = 'A5' .OR. nivell = 'A8' .OR. nivell = 'A9')
DEFINE BAR 4 OF menu023 PROMPT 'Cam\<bio de Tipo de Atenci¢n' SKIP FOR (nivell <> 'A1 ' .AND. nivell <> 'A7' .AND. nivell <> 'A9' .AND. nivell <> 'A4')
DEFINE BAR 5 OF menu023 PROMPT '\<Rechazado por Almac‚n Reparados' SKIP FOR (nivell <> 'A9' .AND. nivell <> 'A7' .AND. nivell <> 'A11 ')
ON SELECTION POPUP menu023 DO MENU023 WITH BAR()
DEFINE POPUP menu025 FROM 13, 30  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu025 PROMPT  ;
       '\<Cierre de O/R' SKIP FOR  ;
       (nivell <> 'A7')
DEFINE BAR 2 OF menu025 PROMPT  ;
       '\<Reingreso de O/R ' SKIP  ;
       FOR (nivell = 'A1  ' .OR.  ;
       nivell = 'A5' .OR. nivell =  ;
       'A9')
DEFINE BAR 3 OF menu025 PROMPT  ;
       'Cierre \<Presupuesto no Aprobado '  ;
       SKIP FOR (nivell = 'A5')
DEFINE BAR 4 OF menu025 PROMPT  ;
       '\<Ingreso de Flete ' SKIP  ;
       FOR (nivell = 'A1  ' .OR.  ;
       nivell = 'A5' .OR. nivell =  ;
       'A9')
ON SELECTION POPUP menu025 DO MENU025;
WITH BAR()
DEFINE POPUP menu026 FROM 07, 30  ;
       TO 18, 60 COLOR SCHEME 21
DEFINE BAR 1 OF menu026 PROMPT  ;
       'Ventas en \<L¡nea'
DEFINE BAR 2 OF menu026 PROMPT  ;
       'Ventas en \<Batch' SKIP  ;
       FOR (nivell = 'A1 ' .OR.  ;
       nivell = 'A3')
DEFINE BAR 3 OF menu026 PROMPT  ;
       '\<Pagos a Cuenta'
DEFINE BAR 4 OF menu026 PROMPT  ;
       '\<Anulaci¢n de Ventas'
DEFINE BAR 5 OF menu026 PROMPT  ;
       '\-'
DEFINE BAR 6 OF menu026 PROMPT  ;
       '\<Registro de Ventas - Manual'  ;
       SKIP FOR nivell = 'A1 '
DEFINE BAR 7 OF menu026 PROMPT  ;
       'Ventas en Batch    - \<Manual '  ;
       SKIP FOR nivell = 'A1 '  ;
       .OR. nivell = 'A3' .OR.  ;
       nivell = 'A9'
DEFINE BAR 8 OF menu026 PROMPT  ;
       'Pagos a \<Cuenta     - Manual '  ;
       SKIP FOR nivell = 'A1 '
DEFINE BAR 9 OF menu026 PROMPT  ;
       '\-'
DEFINE BAR 10 OF menu026 PROMPT  ;
       'Reg. de Ventas S\<ucursales'  ;
       SKIP FOR (nivell = 'A1 '  ;
       .OR. nivell = 'A2' .OR.  ;
       nivell = 'A4' .OR. nivell =  ;
       'A5' .OR. nivell = 'A8'  ;
       .OR. nivell = 'A9' .OR.  ;
       (nivell = 'A11' .AND.  ;
       users <> 'CAJA'))
DEFINE BAR 11 OF menu026 PROMPT  ;
       '\-'
DEFINE BAR 12 OF menu026 PROMPT  ;
       'Planilla Especial LG'  ;
       SKIP FOR nivell = 'A1 '  ;
       .OR. nivell = 'A3' .OR.  ;
       nivell = 'A9'
ON SELECTION POPUP menu026 DO MENU026;
WITH BAR()
DEFINE POPUP menu027 FROM 15, 30  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu027 PROMPT  ;
       '\<Creaci¢n de Archivo'
DEFINE BAR 2 OF menu027 PROMPT  ;
       '\<Reporte del Archivo'  ;
       SKIP FOR nivell = 'A5'
DEFINE BAR 3 OF menu027 PROMPT  ;
       'R\<egistro de Resultados'
DEFINE BAR 4 OF menu027 PROMPT  ;
       'Reporte de \<Diferencias'  ;
       SKIP FOR nivell = 'A5'
DEFINE BAR 5 OF menu027 PROMPT  ;
       'Eliminar \<Archivo '
ON SELECTION POPUP menu027 DO MENU027;
WITH BAR()            
DEFINE POPUP menu029 FROM 12, 30  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu029 PROMPT  ;
       'Salida a S\<ucursales '  ;
       SKIP FOR (nivell <> 'A8'  ;
       .AND. nivell <> 'A7' .AND.  ;
       nivell <> 'A11')
DEFINE BAR 2 OF menu029 PROMPT  ;
       'Salida en \<Cortes¡a  '  ;
       SKIP FOR (nivell <> 'A7'  ;
       .AND. nivell <> 'A4')
DEFINE BAR 3 OF menu029 PROMPT  ;
       'Sa\<lidas en General  '  ;
       SKIP FOR (nivell = 'A2'  ;
       .OR. nivell = 'A5' .OR.  ;
       nivell = 'A8')
DEFINE BAR 4 OF menu029 PROMPT  ;
       '\<Reingreso de Salida '  ;
       SKIP FOR (nivell = 'A2'  ;
       .OR. nivell = 'A5' .OR.  ;
       nivell = 'A8')
DEFINE BAR 5 OF menu029 PROMPT  ;
       '\<Gu¡as de Remisi¢n   '
ON SELECTION POPUP menu029 DO MENU029;
WITH BAR()
DEFINE POPUP menu03 FROM 07, 33  ;
       TO 19, 58 COLOR SCHEME 21
DEFINE BAR 1 OF menu03 PROMPT  ;
       'Tablas \<Generales' SKIP  ;
       FOR (nivell <> 'A7' .AND.  ;
       nivell <> 'A4')
DEFINE BAR 2 OF menu03 PROMPT  ;
       '\<Modelos'
DEFINE BAR 3 OF menu03 PROMPT  ;
       '\<Cliente/Proveedor'
DEFINE BAR 4 OF menu03 PROMPT  ;
       '\<Relaci¢n de T‚cnicos'  ;
       SKIP FOR (nivell = 'A1'  ;
       .OR. nivell = 'A9')
DEFINE BAR 5 OF menu03 PROMPT  ;
       '\<Tarifas de Mano de Obra'  ;
       SKIP FOR nivell = 'A1'
DEFINE BAR 6 OF menu03 PROMPT  ;
       'Ca\<t logo de S¡ntomas'  ;
       SKIP FOR nivell <> 'A7'
DEFINE BAR 7 OF menu03 PROMPT  ;
       '\<Documentos             '
DEFINE BAR 8 OF menu03 PROMPT  ;
       '\Estado de Ordenes'
DEFINE BAR 9 OF menu03 PROMPT  ;
       'Consulta de Repuest\<os'
DEFINE BAR 10 OF menu03 PROMPT  ;
       '\<Importaciones          '
DEFINE BAR 11 OF menu03 PROMPT  ;
       'In\<formaci¢n al Cliente'
ON SELECTION POPUP menu03 DO MENU30 WITH;
BAR()
DEFINE POPUP menu0701 FROM 11, 48  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu0701 PROMPT  ;
       '\<Solicitud de Servicio'
DEFINE BAR 2 OF menu0701 PROMPT  ;
       '\<Orden de Reparaci¢n'
DEFINE BAR 3 OF menu0701 PROMPT  ;
       'Ord\<enes y Pedidos'
DEFINE BAR 4 OF menu0701 PROMPT  ;
       '\<Presupuesto'
DEFINE BAR 5 OF menu0701 PROMPT  ;
       'Pedido de \<Repuesto'
DEFINE BAR 6 OF menu0701 PROMPT  ;
       '\<Documento de Venta'
DEFINE BAR 7 OF menu0701 PROMPT  ;
       '\<Consumo de Repuestos'
DEFINE BAR 8 OF menu0701 PROMPT  ;
       'Pendientes x \<T‚cnico'
ON SELECTION POPUP menu0701 DO MENU0701;
WITH BAR()
DEFINE POPUP menu0702 FROM 15, 48  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu0702 PROMPT  ;
       '\<Pedidos por Nø Factura'
DEFINE BAR 2 OF menu0702 PROMPT  ;
       'Pedidos por \<Detalle'
DEFINE BAR 3 OF menu0702 PROMPT  ;
       '\<Backorders' SKIP FOR  ;
       (nivell = 'A1')
DEFINE BAR 4 OF menu0702 PROMPT  ;
       'Pedidos por \<C¢digo'
ON SELECTION POPUP menu0702 DO MENU0702;
WITH BAR()
DEFINE POPUP menu04 FROM 07, 47  ;
       TO 20, 74 COLOR SCHEME 21
DEFINE BAR 1 OF menu04 PROMPT  ;
       'Tablas \<Generales' SKIP  ;
       FOR (nivell <> 'A7' .AND.  ;
       nivell <> 'A4')
DEFINE BAR 2 OF menu04 PROMPT  ;
       '\<Modelos' SKIP FOR  ;
       (nivell = 'A1' .OR. nivell =  ;
       'A3' .OR. nivell = 'A5')
DEFINE BAR 3 OF menu04 PROMPT  ;
       'Pro\<ceso GEX              '  ;
       SKIP FOR (nivell = 'A1'  ;
       .OR. nivell = 'A5')
DEFINE BAR 4 OF menu04 PROMPT  ;
       'Clasificaci¢n de \<T‚cnico'  ;
       SKIP FOR (nivell = 'A1'  ;
       .OR. nivell = 'A3' .OR.  ;
       nivell = 'A5' .OR. nivell =  ;
       'A8' .OR. nivell = 'A9'  ;
       .OR. nivell = 'A5' .OR.  ;
       nivell = 'A6')
DEFINE BAR 5 OF menu04 PROMPT  ;
       'Tarifa de Mano de \<Obra'  ;
       SKIP FOR (nivell = 'A1'  ;
       .OR. nivell = 'A3' .OR.  ;
       nivell = 'A5' .OR. nivell =  ;
       'A8' .OR. nivell = 'A9'  ;
       .OR. nivell = 'A5')
DEFINE BAR 6 OF menu04 PROMPT  ;
       'Atenc\<i¢n Telef¢nica      '  ;
       SKIP FOR nivell = 'A5'
DEFINE BAR 7 OF menu04 PROMPT  ;
       '\<Estado de Ordenes        '  ;
       SKIP FOR nivell = 'A5'
DEFINE BAR 8 OF menu04 PROMPT  ;
       'Servicios a \<Domicilio    '  ;
       SKIP FOR nivell = 'A5'
DEFINE BAR 9 OF menu04 PROMPT  ;
       '\<Ordenes de Reparaci¢n    '  ;
       SKIP FOR nivell = 'A5'
DEFINE BAR 10 OF menu04 PROMPT  ;
       '\<Producci¢n de T‚cnicos   '  ;
       SKIP FOR (nivell = 'A1 '  ;
       .OR. nivell = 'A3' .OR.  ;
       nivell = 'A9')
DEFINE BAR 11 OF menu04 PROMPT  ;
       'E\<stad¡sticas             '  ;
       SKIP FOR nivell = 'A5'
DEFINE BAR 12 OF menu04 PROMPT  ;
       'Cont\<ables                '  ;
       SKIP FOR (nivell = 'A9'  ;
       .OR. nivell = 'A5')
DEFINE BAR 13 OF menu04 PROMPT  ;
       'I\<nformes                 '  ;
       SKIP FOR nivell = 'A5'
DEFINE BAR 14 OF menu04 PROMPT  ;
       'Fact. en Ga\<rant¡a        '  ;
       SKIP FOR (nivell = 'A1 '  ;
       .OR. nivell = 'A8' .OR.  ;
       nivell = 'A5')
ON SELECTION POPUP menu04 DO MENU40 WITH;
BAR()
DEFINE POPUP menu403 FROM 13, 22  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu403 PROMPT  ;
       '\<Proceso Mensual'
DEFINE BAR 2 OF menu403 PROMPT  ;
       '\<Informe Ventas x Ingresos'
DEFINE BAR 3 OF menu403 PROMPT  ;
       'Informe Ingresos x Ventas'
DEFINE BAR 4 OF menu403 PROMPT  ;
       '\<Exportar Registros'
ON SELECTION POPUP menu403 DO MENU403;
WITH BAR()
DEFINE POPUP menu406 FROM 13, 22  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu406 PROMPT  ;
       '\<Control de Llamadas    '
DEFINE BAR 2 OF menu406 PROMPT  ;
       '\<Llamadas Comprometidas '
DEFINE BAR 3 OF menu406 PROMPT  ;
       '\<Pendientes por Usuario '
DEFINE BAR 4 OF menu406 PROMPT  ;
       '\<Servicios por Usuario '
ON SELECTION POPUP menu406 DO MENU406;
WITH BAR()
DEFINE POPUP menu407 FROM 14, 22 COLOR SCHEME 21
DEFINE BAR 1 OF menu407 PROMPT 'Estado de \<Ordenes por Fechas'
DEFINE BAR 2 OF menu407 PROMPT 'Estado de Ordenes por \<Fecha y Marca'
DEFINE BAR 3 OF menu407 PROMPT '\<Estado de Ordenes por Emisor'
DEFINE BAR 4 OF menu407 PROMPT '\<Resumen Estados de Ordenes '
DEFINE BAR 5 OF menu407 PROMPT '\<Saldos Actuales de Ordenes'
DEFINE BAR 6 OF menu407 PROMPT '\<Pendientes por Cliente'
DEFINE BAR 7 OF menu407 PROMPT 'Saldo actual de S/\<Domicilio'
DEFINE BAR 8 OF menu407 PROMPT 'Resumen de Ordenes - Estados'
ON SELECTION POPUP menu407 DO MENU407 WITH BAR()
DEFINE POPUP menu408 FROM 13, 22 COLOR SCHEME 21
DEFINE BAR 1 OF menu408 PROMPT '\<Recepcionados Por Zonas'
DEFINE BAR 2 OF menu408 PROMPT '\<Control de Servicio por T‚cnico'
ON SELECTION POPUP menu408 DO MENU408;
WITH BAR()
DEFINE POPUP menu409 FROM 11, 15  ;
       TO 19, 58 COLOR SCHEME 21
DEFINE BAR 1 OF menu409 PROMPT  ;
       'Ingreso de \<Ordenes por Taller'
DEFINE BAR 2 OF menu409 PROMPT  ;
       'Ingreso de Ordenes por \<Marca'
DEFINE BAR 3 OF menu409 PROMPT  ;
       'Ingreso de Ordenes por mod\<Elo'
DEFINE BAR 4 OF menu409 PROMPT  ;
       '\<Ingreso de Ordenes por emisor'
DEFINE BAR 5 OF menu409 PROMPT  ;
       'Ingreso de Ordenes por Tipo A\<Tenci¢n'
DEFINE BAR 6 OF menu409 PROMPT  ;
       'Ingreso de Ordenes por \<T‚cnico'
DEFINE BAR 7 OF menu409 PROMPT  ;
       'Ingreso de Ordenes por \<Distritos'
DEFINE BAR 8 OF menu409 PROMPT  ;
       'Ingreso de \<Art¡culos por Fechas'
DEFINE BAR 9 OF menu409 PROMPT  ;
       'Ingreso de art¡culos por Modelo y P¢li\<za'
DEFINE BAR 10 OF menu409 PROMPT  ;
       'Ingreso de art¡culos por Emi\<sor'
DEFINE BAR 11 OF menu409 PROMPT  ;
       'Ingreso de art¡culos por Tipo A\<Tenci¢n'
DEFINE BAR 12 OF menu409 PROMPT  ;
       'Ingreso de art¡culos por \<Distritos'
DEFINE BAR 13 OF menu409 PROMPT  ;
       'Ingreso de art¡culos por Lineas'
DEFINE BAR 14 OF menu409 PROMPT  ;
       '\<Salida de Art¡culos por Fechas'
DEFINE BAR 15 OF menu409 PROMPT  ;
       'Planilla de \<Ordenes Cerradas'
DEFINE BAR 16 OF menu409 PROMPT  ;
       'Planilla de \<Reparados por Fechas'
DEFINE BAR 17 OF menu409 PROMPT  ;
       'Planilla de Reparados en \<Fecha'
DEFINE BAR 18 OF menu409 PROMPT  ;
       'Ordenes no \<Cerradas '
DEFINE BAR 19 OF menu409 PROMPT  ;
       'Ordenes por Fecha \<Vencimiento'
DEFINE BAR 20 OF menu409 PROMPT  ;
       'Solicitudes Fact\<uradas'
DEFINE BAR 21 OF menu409 PROMPT  ;
       'Ordenes con Re\<puestos por Marca'  ;
       SKIP FOR nivell = 'A1'
DEFINE BAR 22 OF menu409 PROMPT  ;
       'Ordenes Cerradas x Tipo Serv'
ON SELECTION POPUP menu409 DO MENU409;
WITH BAR()
DEFINE POPUP menu410 FROM 14, 22  ;
       TO 19, 60 COLOR SCHEME 21
DEFINE BAR 1 OF menu410 PROMPT  ;
       'Pro\<ducci¢n Diaria por T‚cnico'
DEFINE BAR 2 OF menu410 PROMPT  ;
       'Producci¢n \<Mensual por t‚cnico'
DEFINE BAR 3 OF menu410 PROMPT  ;
       'Promedio \<Servicio por T‚cnico'
DEFINE BAR 4 OF menu410 PROMPT  ;
       'Producci¢n por T‚cnico y \<Emisor'
DEFINE BAR 5 OF menu410 PROMPT  ;
       'Producci¢n por \<T‚cnico Objetivo'
DEFINE BAR 6 OF menu410 PROMPT  ;
       'Control \<Servicio de T‚cnicos'
DEFINE BAR 7 OF menu410 PROMPT  ;
       '\<Reclamos por T‚cnico'
DEFINE BAR 8 OF menu410 PROMPT  ;
       'Recha\<zos por T‚cnico'
ON SELECTION POPUP menu410 DO MENU410;
WITH BAR()
DEFINE POPUP menu0411 FROM 16, 25  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu0411 PROMPT  ;
       'Por \<Modelo'
DEFINE BAR 2 OF menu0411 PROMPT  ;
       'M\<odelos por Marca'
DEFINE BAR 3 OF menu0411 PROMPT  ;
       '\<Tiempos de Atenci¢n     '
ON SELECTION POPUP menu0411 DO MENU0411;
WITH BAR()
DEFINE POPUP menu0412 FROM 17, 25  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu0412 PROMPT  ;
       '\<Salidas por Cortes¡a'  ;
       SKIP FOR (nivell <> 'A7'  ;
       .AND. nivell <> 'A4')
DEFINE BAR 2 OF menu0412 PROMPT  ;
       '\Devoluciones - N/Cambio y N/Cr‚dito'
DEFINE BAR 3 OF menu0412 PROMPT  ;
       '\<Planillas LG'
ON SELECTION POPUP menu0412 DO MENU0412;
WITH BAR()
DEFINE POPUP menu0413 FROM 13, 00  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu0413 PROMPT  ;
       'Desde \<Compra a Gen. S/S'
DEFINE BAR 2 OF menu0413 PROMPT  ;
       'Desde \<S/S a Cierre de O/R'
DEFINE BAR 3 OF menu0413 PROMPT  ;
       'Desde c\<Ierre de O/R a Cliente'
DEFINE BAR 4 OF menu0413 PROMPT  ;
       'Desde \<O/R hasta Cierre O/R'
DEFINE BAR 5 OF menu0413 PROMPT  ;
       'Desde S/S Hasta Cierre Por \<Emisor'
DEFINE BAR 6 OF menu0413 PROMPT  ;
       'Estado \<Actual con Movimiento'
DEFINE BAR 7 OF menu0413 PROMPT  ;
       'Desde \<O/R hasta Cierre O/R - T‚cnicos'
ON SELECTION POPUP menu0413 DO MENU0413;
WITH BAR()
DEFINE POPUP menu0414 FROM 16, 30  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu0414 PROMPT  ;
       '\<Informe T‚cnico'
DEFINE BAR 2 OF menu0414 PROMPT  ;
       '\<Duplicado de O/R' SKIP  ;
       FOR (nivell = 'A1' .OR.  ;
       nivell = 'A3' .OR. nivell =  ;
       'A5' .OR. nivell = 'A6'  ;
       .OR. nivell = 'A8' .OR.  ;
       nivell = 'A9')
DEFINE BAR 3 OF menu0414 PROMPT  ;
       'Con\<Solidado de O/R'  ;
       SKIP FOR (nivell = 'A3'  ;
       .OR. nivell = 'A5')
ON SELECTION POPUP menu0414 DO MENU0414;
WITH BAR() 
DEFINE POPUP menu0415 FROM 16, 20  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu0415 PROMPT  ;
       '\<Ordenes Fact. de Garant¡a'  ;
       SKIP FOR nivell = 'A1 '
DEFINE BAR 2 OF menu0415 PROMPT  ;
       '\<Consol. de O/R Fact. en Gtia'  ;
       SKIP FOR nivell = 'A1'
DEFINE BAR 3 OF menu0415 PROMPT  ;
       '\<Fact. en Garant¡a' SKIP  ;
       FOR nivell = 'A1'
DEFINE BAR 4 OF menu0415 PROMPT  ;
       'Ordenes Facturadas x \<Responsabilidad'  ;
       SKIP FOR nivell = 'A1 '
ON SELECTION POPUP menu0415 DO MENU0415;
WITH BAR()
DEFINE POPUP menu05 FROM 07, 57  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menu05 PROMPT  ;
       'Cambio de \<Password'
DEFINE BAR 2 OF menu05 PROMPT  ;
       '\<Optimizaci¢n de Archivos'  ;
       SKIP FOR nivell <> 'A7'
DEFINE BAR 3 OF menu05 PROMPT  ;
       '\<Generac. de Archivo ASCII'  ;
       SKIP FOR nivell <> 'A7'
DEFINE BAR 4 OF menu05 PROMPT  ;
       '\<Cierre Peri¢dico' SKIP  ;
       FOR nivell <> 'A7'
DEFINE BAR 5 OF menu05 PROMPT  ;
       '\<Registro de Usuarios'  ;
       SKIP FOR nivell <> 'A7'
ON SELECTION POPUP menu05 DO MENU50 WITH;
BAR()
DO WHILE .T.
     ACTIVATE WINDOW pantalla
     @ 0, 0 SAY 'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
     @ 1, 0 SAY '³ ACTUALIZACIONES ³   PROCESOS   ³  CONSULTAS  ³   REPORTES   ³ ADMINISTRACION ³'
     @ 2, 0 SAY 'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
     ACTIVATE SCREEN
     ACTIVATE MENU menu00
     IF LASTKEY() = 27
          rpt = f_yesno('DESEA SALIR DEL SISTEMA?')
          IF rpt
               ppalqz = .F.
               EXIT
          ENDIF
     ENDIF
ENDDO
RETURN ppalqz
*
PROCEDURE menu10
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porm0101
     CASE bar = 2
          DO tablas
     CASE bar = 3
          DO porm0103 WITH 1
     CASE bar = 4
          DO porm0105 WITH 1
     CASE bar = 5
          DO porm0106 WITH 1
     CASE bar = 6
          DO porm0108 WITH 1
     CASE bar = 7
          DO porm0109
     CASE bar = 9
          DO porm0111
     CASE bar = 10
          DO porm0112
     CASE bar = 11
          DO porm0113
     CASE bar = 12
          DO porm0114
     CASE bar = 14
          DO porm0115
ENDCASE
RETURN
*
PROCEDURE menu017
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porm0171 WITH 1
     CASE bar = 2
          DO porm0172 WITH 1
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
          DO porm0241 WITH 1
     CASE bar = 5
          ACTIVATE POPUP menu025
     CASE bar = 6
          ACTIVATE POPUP menu026
     CASE bar = 7
          ACTIVATE POPUP menu027
     CASE bar = 8
          ACTIVATE POPUP menu029
     CASE bar = 9
          DO porm0301
     CASE bar = 10
          DO porm0302
ENDCASE
RETURN
*
PROCEDURE menu021
PARAMETER bar
SET SYSMENU OFF
DO CASE
     CASE bar = 1
          DO porm0211
     CASE bar = 2
          DO porm0201 WITH 2
     CASE bar = 3
          DO porm0201 WITH 3
     CASE bar = 4
          DO porm0201 WITH 1
ENDCASE
RETURN
*
PROCEDURE menu022
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porm0202 WITH 1
     CASE bar = 2
          DO porm0202 WITH 2
     CASE bar = 3
          DO porm0202 WITH 3
     CASE bar = 4
          DO porm0204
ENDCASE
RETURN
*
PROCEDURE menu023
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porm0203 WITH 1
     CASE bar = 2
          DO porm0203 WITH 2
     CASE bar = 3
          DO porm0203 WITH 3
     CASE bar = 4
          DO porm2031
     CASE bar = 5
          DO porm2032
ENDCASE
RETURN
*
PROCEDURE menu025
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porm0251 WITH 1
     CASE bar = 2
          DO porm0251 WITH 2
     CASE bar = 3
          DO porm0252
     CASE bar = 4
          DO porm0253
ENDCASE
RETURN
*
PROCEDURE menu026
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porm0261 WITH 1
     CASE bar = 2
          DO porm0262 WITH 1
     CASE bar = 3
          DO porm0225 WITH 1
     CASE bar = 4
          DO porm0263
     CASE bar = 6
          DO porm0261 WITH 2
     CASE bar = 7
          DO porm0262 WITH 2
     CASE bar = 8
          DO porm0225 WITH 2
     CASE bar = 10
          DO porm0210
     CASE bar = 12
          DO porm0212 WITH 2
ENDCASE
RETURN
*
PROCEDURE menu027
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porm0271
     CASE bar = 2
          DO porm0272
     CASE bar = 3
          DO porm0273
     CASE bar = 4
          DO porm0274
     CASE bar = 5
          DO porm0275
ENDCASE
RETURN
*
PROCEDURE menu029
PARAMETER bar
DO CASE
     CASE bar = 1
          SET SYSMENU OFF
          DO porm0294
     CASE bar = 2
          SET SYSMENU OFF
          DO porm0295
     CASE bar = 3
          SET SYSMENU OFF
          DO porm0298
     CASE bar = 4
          SET SYSMENU OFF
          DO porm0297
     CASE bar = 5
          SET SYSMENU OFF
          DO porm0299
ENDCASE
RETURN
*
PROCEDURE menu30
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porm0102 WITH 2
     CASE bar = 2
          DO porm0103 WITH 2
     CASE bar = 3
          DO porm0105 WITH 2
     CASE bar = 4
          DO porm0106 WITH 2
     CASE bar = 5
          DO porm0104 WITH 2
     CASE bar = 6
          DO porm0108 WITH 2
     CASE bar = 7
          ACTIVATE POPUP menu0701
     CASE bar = 8
          DO porm0308
     CASE bar = 9
          DO agcc0202
     CASE bar = 10
          ACTIVATE POPUP menu0702
     CASE bar = 11
          DO porq0172
     CASE bar = 12
          DO carsa
ENDCASE
RETURN
*
PROCEDURE menu0701
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porq0371 WITH 1
     CASE bar = 2
          DO porq0372 WITH 2
     CASE bar = 3
          DO porq0376 WITH 3
     CASE bar = 4
          DO porq0371 WITH 3
     CASE bar = 5
          DO porq0374 WITH 2
     CASE bar = 6
          DO porq0375 WITH 1
     CASE bar = 7
          DO porq0377
     CASE bar = 8
          DO porq0379
ENDCASE
RETURN
*
PROCEDURE menu0702
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porq0312
     CASE bar = 2
          DO porq0313
     CASE bar = 3
          DO porq0314
     CASE bar = 4
          DO porq0315
ENDCASE
RETURN
*
PROCEDURE menu40
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl0401
     CASE bar = 2
          DO porl0402
     CASE bar = 3
          ACTIVATE POPUP menu403
     CASE bar = 4
          DO porl0404
     CASE bar = 5
          DO porl0415
     CASE bar = 6
          ACTIVATE POPUP menu406
     CASE bar = 7
          ACTIVATE POPUP menu407
     CASE bar = 8
          ACTIVATE POPUP menu408
     CASE bar = 9
          ACTIVATE POPUP menu409
     CASE bar = 10
          ACTIVATE POPUP menu410
     CASE bar = 11
          ACTIVATE POPUP menu0411
     CASE bar = 12
          ACTIVATE POPUP menu0412
     CASE bar = 13
          ACTIVATE POPUP menu0414
     CASE bar = 14
          ACTIVATE POPUP menu0415
ENDCASE
CLOSE DATABASES
RETURN
*
PROCEDURE menu403
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl0440
     CASE bar = 2
          DO porl0441
     CASE bar = 3
          DO porl0442
     CASE bar = 4
          DO porl0443
ENDCASE
RETURN
*
PROCEDURE menu406
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl0422 WITH 1
     CASE bar = 2
          DO porl0422 WITH 2
     CASE bar = 3
          DO porl0428
     CASE bar = 4
          DO porl0429
ENDCASE
RETURN
*
PROCEDURE menu407
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl4130
     CASE bar = 2
          DO porl4072
     CASE bar = 3
          DO porl4074
     CASE bar = 4
          DO porl4001
     CASE bar = 5
          DO porl4002
     CASE bar = 6
          DO porl4003
     CASE bar = 7
          DO porl4004
     CASE bar = 8
          DO porl4005
ENDCASE
RETURN
*
PROCEDURE menu408
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl4081
     CASE bar = 2
          DO porl4082
ENDCASE
RETURN
*
PROCEDURE menu409
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl4135
     CASE bar = 2
          DO porl4092
     CASE bar = 3
          DO porl4093
     CASE bar = 4
          DO porl4094
     CASE bar = 5
          DO porl4090
     CASE bar = 6
          DO porl4148
     CASE bar = 7
          DO porl4141
     CASE bar = 8
          DO porl4131
     CASE bar = 9
          DO porl4095
     CASE bar = 10
          DO porl4091
     CASE bar = 11
          DO porl4108
     CASE bar = 12
          DO porl4139
     CASE bar = 13
          DO porl4140
     CASE bar = 14
          DO porl4132
     CASE bar = 15
          DO porl4134
     CASE bar = 16
          DO porl4138
     CASE bar = 17
          DO porl4096
     CASE bar = 18
          DO porl4083
     CASE bar = 19
          DO porl4097
     CASE bar = 20
          DO porl4098
     CASE bar = 21
          DO porl4099
     CASE bar = 22
          DO porl0426
ENDCASE
RETURN
*
PROCEDURE menu410
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl4133
     CASE bar = 2
          DO porl4120
     CASE bar = 3
          DO porl4102
     CASE bar = 4
          DO porl4103
     CASE bar = 5
          DO porl4084
     CASE bar = 6
          DO porl4105
     CASE bar = 7
          DO porl4106
     CASE bar = 8
          DO porl4107
ENDCASE
RETURN
*
PROCEDURE menu0411
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl4116
     CASE bar = 2
          DO porl4113
     CASE bar = 3
          ACTIVATE POPUP menu0413
ENDCASE
RETURN
*
PROCEDURE menu0412
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl4012
     CASE bar = 2
          DO porl4013
     CASE bar = 3
          DO porl4014
ENDCASE
RETURN
*
PROCEDURE menu0413
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl4122
     CASE bar = 2
          DO porl4123
     CASE bar = 3
          DO porl4128
     CASE bar = 4
          DO porl4125
     CASE bar = 5
          DO porl4142
     CASE bar = 6
          DO porl4144
     CASE bar = 7
          DO porl4149
ENDCASE
RETURN
*
PROCEDURE menu0414
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl0413
     CASE bar = 2
          DO porl0414
     CASE bar = 3
          DO porl0420
ENDCASE
RETURN
*
PROCEDURE menu0415
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porl0417
     CASE bar = 2
          DO porl0419
     CASE bar = 3
          DO porl0425
     CASE bar = 4
          DO porl0427
ENDCASE
RETURN
*
PROCEDURE menu50
PARAMETER bar
DO CASE
     CASE bar = 1
          DO porm0502
     CASE bar = 2
          DO orad0501
     CASE bar = 3
          RETURN
          DO porm0503
     CASE bar = 4
          DO porm0506
     CASE bar = 5
          DO porm0507 WITH 1
ENDCASE
RETURN
*
FUNCTION f_yesno
PARAMETER mens, def
PRIVATE ALL
l = IIF(LEN(mens) < 18, 18,  ;
    LEN(mens))
sdef = IIF(def = .T.,  ;
       '\?\<Ok;\!\<Cancel',  ;
       '\?\<No;\!\<Si')
DEFINE WINDOW wyesno FROM 18, (80 -  ;
       l) / 2 - 5 TO 23, (80 + l) /  ;
       2 + 4 DOUBLE COLOR SCHEME  ;
       6
ACTIVATE WINDOW wyesno
@ 1, 4 SAY mens
@ 3, CEILING(l - 18) / 2 GET vok  ;
  DEFAULT 1 SIZE 1, 10, 4 PICTURE  ;
  '@*HT ' + sdef
READ CYCLE
RELEASE WINDOW wyesno
RETURN IIF(def = .T., IIF(vok = 1,  ;
       .T., .F.), IIF(vok = 1,  ;
       .F., .T.))
RETURN
*
PROCEDURE p_menpar
rge_nroruc = par_numruc
rge_razsoc = ALLTRIM(par_razsoc)
rge_abrev = ALLTRIM(par_abrev)
rge_calle = ALLTRIM(par_calle)
rge_distri = ALLTRIM(par_distri)
rge_provin = ALLTRIM(par_provin)
rge_lispre = par_codlis
rge_monbas = par_monbas
rge_codalm = par_codalm
rge_lptfac = par_lptfac
rge_lptgui = par_lptgui
rge_lptped = par_lptped
rge_lptbol = par_lptbol
rge_lptdes = par_lptbol
rge_punemi = par_punemi
rge_pormax = par_pormax
rge_vicodi = par_vicodi
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

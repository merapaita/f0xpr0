*ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*º Funci¢n   : PROGRAMA PRINCIPAL     º
*º Programa  : SISTEMA.PRG            º
*º Perfecci¢n: SISTEMAS CESER-PERU    º
*ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½ 
close data
*SET PATH TO &ruta
*set century on para cambiar poco a poco
set path to ..\bases
*set path to data:sys\bases
*set path to i:\sys\bases.prg
*SET PATH TO C:\bases
clear all
set reprocess to  -1
set procedure to funcione
clear macros
set safety off
set echo off
set talk off
set notify off
set bell off
set date briti
set score off
set dele on
SET STAT OFF
set escape off
SET EXCLUSIVE OFF
SET REFRESH TO 2
set sysmenu ON
set century on
public sw_factura, rge_razsoc,rge_abrev,rge_calle,rge_distri,rge_provin,rge_lispre 
PUBLIC rge_monbas, rge_codalm,rge_lptfac,rge_lptgui,rge_lptped,rge_lptbol 
PUBLIC rge_lptdes, rge_punemi,rge_pormax,rge_vicodi, rge_nroruc
MONO = "W/N,N/W,N"
set color to &mono
USE GC_PAR00
DO p_menpar
USE
SET COLOR OF SCHEME 5  TO N/W,N/W,N/W,W+/W,N/W,W/N,W+/W,N/N+,N/W,W+/W
SET COLOR OF SCHEME 6  TO N/W,N/W,N/W,W+/W,N/W,W/N,W+/W,N/N+,N/W,W+/W   
SET COLOR OF SCHEME 7  TO W+/N,W/N+,W/N,W/N,N/W ,N+/W,W+/N,N/N+,W+/N,N+/N
SET COLOR OF SCHEME 8  TO N/W,N/W,N/W,N/W,W/N,W/N,N/W,N/N+,W+/N,N+/N
SET COLOR OF SCHEME 9  TO W+/N,W+/N,W/N,W+/N,W/N,N/W,W/N,N/N+,W+/N,N+/N
SET COLOR OF SCHEME 10 TO W+/N,N/W,N/W,W+/N,W+/N,N/W,N/W,N/N+,W+/N,N+/N
SET COLOR OF SCHEME 12 TO W+/N,W+/N,W+/N+,W+/N,W/N,N+/W,N+/W,N+/N,W+/N,W/N+
SET COLOR OF SCHEME 15 TO N/W,N/W,N/W,N/W,W/N,W/N,N/W,N/N+,W+/N,N+/N
SET COLOR OF SCHEME 16 TO N/W,N/W,N/W,N/W,W/N,W/N,N/W,N/N+,W+/N,N+/N
SET COLOR OF SCHEME 17 TO N/W,N/W,N/W,N/W,W/N,W/N,N/W,N/N+,W+/N,N+/N
SET COLOR OF SCHEME 19 TO W+/N,W+/N ,N+/W ,W+/N, W/N+, N/W, W/N, W+/N, N/W 
SET COLOR OF SCHEME 20 TO W+/N,W+/N,W+/N,W+/N,W/N+,N/W,W/N,N+/W,N/W+,W/N
SET COLOR OF SCHEME 23 TO W+/N,N+/W ,N/W+,N+/W,W+/N,W+/N,N+/W,W+/N,N+/W,W+/N
SET COLOR OF SCHEME 21 TO W/N,W+/N,W/N,W/N,W/N,N/W,W/N,N/N+,W+/N,N+/N
DEFINE WINDOW PANTALL FROM 0,0 TO 24,222 COLOR SCHEME 21 ZOOM FLOAT
Color1 = 'w/b'
color2 = 'b/w'
color3 = 'w+/b'
color4 = 'b*/w'
color5 = 'W/N, N+/W, W+/N, W+/N, W/N, U+/N, B+/W, -, W+/N, W/N'
color6 = 'b*/w'
chisof1 = 'DPTO.DE SISTEMAS LIMA-PERU'
chisof2 = 'Av. Rep. de Panam  4123'
PUBLIC empre1, empre2, empre3, empre4, empre5, empre6, empre7, empre8, empre9, empre10, empre11, empre12,empre13
use st_iparg SHARE
empre1 = trim(sys_razsoc)
empre2 = trim(sys_nomcal)
empre3 = sys_tmpdem
empre4 = sys_codlpg
empre5 = sys_codlpf
empre6 = sys_codtal
empre7 = sys_fecgar
empre8 = sys_simmon
empre9 = sys_codimp
empre10= sys_desmon
empre11= sys_tmppre
empre12= sys_garmax
empre13= sys_simomb
use
public x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14
PUBLIC ppalqz,nivell
nivell='A0'
ppalqz = .t.
DO INGRESO 
if nivell='A0'
   ppalqz=.f.
endif   
do while ppalqz
   do menu
enddo
close data
clear all
clear
if file('orden1.dbf')
RUN del orden?.dbf
endif
if file('orden1.idx')
RUN del orden?.idx
endif
if file('orden2.dbf')
RUN del orden?.dbf
endif
if file('orden2.idx')
RUN del orden?.idx
endif
if file('tempo1.dbf')
RUN del tempo*.dbf
endif
if file('tempo1.idx')
RUN del tempo*.idx
endif
if file('orden1.cdx')
RUN del orden?.cdx
endif
* Quit
return

*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
PROCEDURE MENU
*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
DEFINE WIND PANTALLA FROM 04,00 TO 06,79 IN SCREEN NONE
   SET COLOR TO W+/N
   TIT1 = "SISTEMA DE REPARACIONES"
   @ 1,26,3,56 BOX "ÚÄ¿³ÙÄÀ³ " 
   @ 2,26+((32 - len(TIT1))/2) SAY TIT1 COLOR W+

******************************************
*  DEFINICION DE OBJETOS PARA EL SISTEMA *
******************************************
DEFINE MENU MENU00  COLOR SCHEME 21
DEFINE PAD TABPAD OF MENU00 PROMPT "ACTUALIZACIONES"  AT 5,01 SKIP FOR (NIVELL='A0' OR NIVELL='A5' OR NIVELL='A6')
DEFINE PAD ORDPAD OF MENU00 PROMPT "PROCESOS"         AT 5,21 SKIP FOR (NIVELL='A0' OR NIVELL='A6')
DEFINE PAD POGPAD OF MENU00 PROMPT "CONSULTAS"        AT 5,35 SKIP FOR  NIVELL='A0'
DEFINE PAD LOGPAD OF MENU00 PROMPT "REPORTES"         AT 5,50 SKIP FOR (NIVELL='A0')
DEFINE PAD REPPAD OF MENU00 PROMPT "ADMINISTRACION"   AT 5,63 

ON PAD TABPAD OF MENU00 ACTI POPUP MENU01  
ON PAD ORDPAD OF MENU00 ACTI POPUP MENU02  
ON PAD POGPAD OF MENU00 ACTI POPUP MENU03 
ON PAD LOGPAD OF MENU00 ACTI POPUP MENU04 
ON PAD REPPAD OF MENU00 ACTI POPUP MENU05  

*============= ACTUALIZACIONES

DEFINE POPUP MENU01 FROM 07,00 COLOR SCHEME 21 
DEFINE BAR 1  OF MENU01 PROMPT "Par metros \<Generales"          SKIP FOR NIVELL#'A7'          
DEFINE BAR 2  OF MENU01 PROMPT "\<Tablas Generales"              SKIP FOR NIVELL#'A7'           
DEFINE BAR 3  OF MENU01 PROMPT "Registro de \<Modelos"           SKIP FOR ((NIVELL='A2' and substr(users,1,6)#'HOGPCH' and substr(users,1,6)#'TALMSR') OR NIVELL='A1 ')
DEFINE BAR 4  OF MENU01 PROMPT "Registro Cliente/\<Proveedor"    SKIP FOR ((NIVELL='A2' and substr(users,1,3)#'COM') OR NIVELL='A5')
DEFINE BAR 5  OF MENU01 PROMPT "Registro T‚\<cnicos"             SKIP FOR (NIVELL='A1' OR NIVELL='A3' OR NIVELL='A8' OR NIVELL='A9' OR NIVELL='A11')
*DEFINE BAR 6  OF MENU01 PROMPT "\N£meros de Serie           "   SKIP FOR NIVELL#'A7'
DEFINE BAR 6  OF MENU01 PROMPT "Cat logo de \<S¡ntomas"          SKIP FOR NIVELL#'A7'
DEFINE BAR 7  OF MENU01 PROMPT "Registro de Mano de \<Obra"      SKIP FOR (NIVELL='A1' OR NIVELL='A9' OR NIVELL='A11')
DEFINE BAR 8  OF MENU01 PROMPT "\-"
DEFINE BAR 9  OF MENU01 PROMPT "\Exporta Registros de Clientes" SKIP FOR NIVELL#'A7'
DEFINE BAR 10 OF MENU01 PROMPT "\Exporta Estadist. AIWA"        SKIP FOR NIVELL#'A7'
DEFINE BAR 11 OF MENU01 PROMPT "\Exporta S/Servicio"            SKIP FOR NIVELL#'A7' 
DEFINE BAR 12 OF MENU01 PROMPT "\Actualiza S/Servicio"          SKIP FOR NIVELL#'A7' 
DEFINE BAR 13 OF MENU01 PROMPT "\-"                              SKIP FOR NIVELL#'A7' 
DEFINE BAR 14 OF MENU01 PROMPT "T\<ipo de Cambio"                SKIP FOR NIVELL#'A7' 
ON SELECTION POPUP MENU01 DO MENU10 WITH BAR()

DEFINE POPUP MENU017 FROM 15,20  COLOR SCHEME 21
DEFINE BAR 1 OF MENU017 PROMPT "\<Actualizaci¢n Manual" 
DEFINE BAR 2 OF MENU017 PROMPT "\<Actualizaci¢n Autom tica" 
ON SELECTION POPUP MENU017 DO MENU017 WITH BAR()

*============= PROCESOS

DEFINE POPUP MENU02 FROM 07,18  COLOR SCHEME 21
DEFINE BAR 1  OF MENU02 PROMPT "Solicitud de \<Servicio    " SKIP FOR (NIVELL='A2'  OR NIVELL='A5' OR NIVELL='A8') AND USERS#'RECB¥Y' and substr(users,1,6)#'TALMSR' AND USERS#'TALVUM' AND USERS#'TALIGP'AND substr(USERS,1,3)#'COM'
DEFINE BAR 2  OF MENU02 PROMPT "Solicitud de \<Presupuesto " SKIP FOR (NIVELL='A1 '  OR NIVELL='A9')
DEFINE BAR 3  OF MENU02 PROMPT "\<Orden de Reparaci¢n      " 
DEFINE BAR 4  OF MENU02 PROMPT "Pedido de \<Repuestos "       SKIP FOR ( NIVELL='A1 '  OR NIVELL='A9')
DEFINE BAR 5  OF MENU02 PROMPT "\<Cierre de Orden de Rep.  " SKIP FOR ( NIVELL='A5' OR NIVELL='A8')
DEFINE BAR 6  OF MENU02 PROMPT "\<Facturaci¢n              " SKIP FOR ((NIVELL='A2' AND (SUBSTR(USERS,1,3)#'CCA' AND USERS#'COMVCR' AND USERS#'TALVUM' AND USERS#'TALIGP')) OR NIVELL='A5' OR NIVELL='A8')
DEFINE BAR 7  OF MENU02 PROMPT "\<Inventario F¡sico        " SKIP FOR ( NIVELL='A6'  OR NIVELL='A8')
DEFINE BAR 8  OF MENU02 PROMPT "Sa\<lida de Art¡culos      " SKIP FOR ( NIVELL='A2'  OR NIVELL='A5')
DEFINE BAR 9  OF MENU02 PROMPT "\<Descuento de Art¡culos    " SKIP FOR ( NIVELL#'A7'  AND NIVELL#'A4')
DEFINE BAR 10 OF MENU02 PROMPT "\Cambios/Devoluciones      " SKIP FOR ( NIVELL#'A1'  AND NIVELL#'A7' AND NIVELL#'A4')
ON SELECTION POPUP MENU02 DO MENU20 WITH BAR()

*----
DEFINE POPUP MENU021 FROM 09,30  COLOR SCHEME 21
DEFINE BAR 1 OF MENU021 PROMPT "\<Generaci¢n de Solicitud"   
DEFINE BAR 2 OF MENU021 PROMPT "\<Modificaci¢n de Solicitud" 
DEFINE BAR 3 OF MENU021 PROMPT "\<Anulaci¢n de Solicitud"    SKIP FOR (NIVELL='A1' OR NIVELL='A4') AND substr(users,1,3)#'COM'
DEFINE BAR 4 OF MENU021 PROMPT "\<Generaci¢n Manual"         SKIP FOR (NIVELL#'A7' and NIVELL#'A4')
ON SELECTION POPUP MENU021 DO MENU021 WITH BAR()

DEFINE POPUP MENU022 FROM 10,30  COLOR SCHEME 21
DEFINE BAR 1 OF MENU022 PROMPT "\<Generaci¢n de Ppto."
DEFINE BAR 2 OF MENU022 PROMPT "\<Modificaci¢n de Ppto."    SKIP FOR (NIVELL='A5' OR NIVELL='A8')
DEFINE BAR 3 OF MENU022 PROMPT "\<Anulaci¢n de Ppto."       SKIP FOR (NIVELL='A5' OR NIVELL='A8')
DEFINE BAR 4 OF MENU022 PROMPT "Actualizaci¢n de \<Ppto."   SKIP FOR (NIVELL='A5' OR NIVELL='A8' or nivell='A3'or nivell='A11')
ON SELECTION POPUP MENU022 DO MENU022 WITH BAR()

DEFINE POPUP MENU023 FROM 11,30  COLOR SCHEME 21
DEFINE BAR 1 OF MENU023 PROMPT "\<Generaci¢n de Ordenes"           SKIP FOR (NIVELL='A1 ' OR  NIVELL='A5' OR NIVELL='A8' OR NIVELL='A9')
DEFINE BAR 2 OF MENU023 PROMPT "\<Cambio de Estado de O/R"         SKIP FOR (NIVELL='A1 ' OR  NIVELL='A9')
DEFINE BAR 3 OF MENU023 PROMPT "\<Anulaci¢n de Ordenes"            SKIP FOR (NIVELL='A1 ' OR  NIVELL='A5' OR  NIVELL='A8' OR  NIVELL='A9')
DEFINE BAR 4 OF MENU023 PROMPT "Cam\<bio de Tipo de Atenci¢n"      SKIP FOR (NIVELL#'A1 ' AND NIVELL#'A7' AND NIVELL#'A9' AND NIVELL#'A4')
DEFINE BAR 5 OF MENU023 PROMPT "\<Rechazado por Almac‚n Reparados" SKIP FOR (NIVELL#'A9' AND NIVELL#'A7' AND NIVELL#'A11 ')
ON SELECTION POPUP MENU023 DO MENU023 WITH BAR()

DEFINE POPUP MENU025 FROM 13,30  COLOR SCHEME 21
DEFINE BAR 1 OF MENU025 PROMPT "\<Cierre de O/R"                   SKIP FOR (NIVELL#'A7')
DEFINE BAR 2 OF MENU025 PROMPT "\<Reingreso de O/R "               SKIP FOR (NIVELL='A1  ' OR NIVELL='A5' OR NIVELL='A9')
DEFINE BAR 3 OF MENU025 PROMPT "Cierre \<Presupuesto no Aprobado " SKIP FOR (NIVELL='A5')
DEFINE BAR 4 OF MENU025 PROMPT "\<Ingreso de Flete "               SKIP FOR (NIVELL='A1  ' OR NIVELL='A5' OR NIVELL='A9')
ON SELECTION POPUP MENU025 DO MENU025 WITH BAR()

DEFINE POPUP MENU026 FROM 07,30 TO 18,60 COLOR SCHEME 21
DEFINE BAR  1 OF MENU026 PROMPT "Ventas en \<L¡nea"                 
DEFINE BAR  2 OF MENU026 PROMPT "Ventas en \<Batch"                  SKIP FOR (NIVELL='A1 ' OR NIVELL='A3')
DEFINE BAR  3 OF MENU026 PROMPT "\<Pagos a Cuenta"                  
*DEFINE BAR  4 OF MENU026 PROMPT "\<Facturaci¢n Preventa"            SKIP FOR (NIVELL='A1' OR NIVELL='A9')
*DEFINE BAR 10 OF MENU026 PROMPT "Fac\<turaci¢n Preventa-Manual "    SKIP FOR NIVELL='A1' OR NIVELL='A3' OR NIVELL='A2' OR NIVELL='A9'
DEFINE BAR  4 OF MENU026 PROMPT "\<Anulaci¢n de Ventas"
DEFINE BAR  5 OF MENU026 PROMPT "\-"                            
DEFINE BAR  6 OF MENU026 PROMPT "\<Registro de Ventas - Manual"     SKIP FOR NIVELL='A1 ' 
DEFINE BAR  7 OF MENU026 PROMPT "Ventas en Batch    - \<Manual "    SKIP FOR NIVELL='A1 ' OR nivell='A3' OR NIVELL='A9'
DEFINE BAR  8 OF MENU026 PROMPT "Pagos a \<Cuenta     - Manual "    SKIP FOR NIVELL='A1 ' 
DEFINE BAR  9 OF MENU026 PROMPT "\-"                               
DEFINE BAR 10 OF MENU026 PROMPT "Reg. de Ventas S\<ucursales"       SKIP FOR (NIVELL='A1 ' OR NIVELL='A2' OR NIVELL='A4' OR NIVELL='A5' OR NIVELL='A8' OR NIVELL='A9' or NIVELL='A11')
DEFINE BAR 11 OF MENU026 PROMPT "\-"                               
DEFINE BAR 12 OF MENU026 PROMPT "Planilla Especial LG"              SKIP FOR NIVELL='A1 ' OR nivell='A3' OR NIVELL='A9'
ON SELECTION POPUP MENU026 DO MENU026 WITH BAR()

DEFINE POPUP MENU027 FROM 15,30  COLOR SCHEME 21
DEFINE BAR 1 OF MENU027 PROMPT "\<Creaci¢n de Archivo"      
DEFINE BAR 2 OF MENU027 PROMPT "\<Reporte del Archivo"      SKIP FOR NIVELL='A5'
DEFINE BAR 3 OF MENU027 PROMPT "R\<egistro de Resultados"   
DEFINE BAR 4 OF MENU027 PROMPT "Reporte de \<Diferencias"   SKIP FOR NIVELL='A5'
DEFINE BAR 5 OF MENU027 PROMPT "Eliminar \<Archivo "
ON SELECTION POPUP MENU027 DO MENU027 WITH BAR()            

DEFINE POPUP MENU029 FROM 12,30  COLOR SCHEME 21
DEFINE BAR 1 OF MENU029 PROMPT "Salida a S\<ucursales "    SKIP FOR (NIVELL#'A8' AND NIVELL#'A7' AND NIVELL#'A11')
DEFINE BAR 2 OF MENU029 PROMPT "Salida en \<Cortes¡a  "    SKIP FOR (NIVELL#'A7' AND NIVELL#'A4')
DEFINE BAR 3 OF MENU029 PROMPT "Sa\<lidas en General  "    SKIP FOR (NIVELL='A2' OR NIVELL='A5' OR NIVELL='A8')
DEFINE BAR 4 OF MENU029 PROMPT "\<Reingreso de Salida "    SKIP FOR (NIVELL='A2' OR NIVELL='A5' OR NIVELL='A8')
DEFINE BAR 5 OF MENU029 PROMPT "\<Gu¡as de Remisi¢n   "    
ON SELECTION POPUP MENU029 DO MENU029 WITH BAR()

*============= CONSULTAS 

DEFINE POPUP MENU03 FROM 07,33 TO 19,58 COLOR SCHEME 21
DEFINE BAR  1  OF MENU03 PROMPT "Tablas \<Generales"         SKIP FOR (NIVELL#'A7' AND NIVELL#'A4')
DEFINE BAR  2  OF MENU03 PROMPT "\<Modelos"
DEFINE BAR  3  OF MENU03 PROMPT "\<Cliente/Proveedor" 
DEFINE BAR  4  OF MENU03 PROMPT "\<Relaci¢n de T‚cnicos"     SKIP FOR (NIVELL='A1' OR NIVELL='A9')
DEFINE BAR  5  OF MENU03 PROMPT "\<Tarifas de Mano de Obra"  SKIP FOR  NIVELL='A1'
DEFINE BAR  6  OF MENU03 PROMPT "Ca\<t logo de S¡ntomas"     SKIP FOR  NIVELL#'A7'
DEFINE BAR  7  OF MENU03 PROMPT "\<Documentos             "
DEFINE BAR  8  OF MENU03 PROMPT "\Estado de Ordenes"
DEFINE BAR  9  OF MENU03 PROMPT "Consulta de Repuest\<os"
DEFINE BAR  10 OF MENU03 PROMPT "\<Importaciones          " 
DEFINE BAR  11 OF MENU03 PROMPT "In\<formaci¢n al Cliente"
*DEFINE BAR  12 OF MENU03 PROMPT "G\<arant¡a CARSA"
ON SELECTION POPUP MENU03 DO MENU30 WITH BAR()

DEFINE POPUP MENU0701 FROM 11,48 COLOR SCHEME 21
DEFINE BAR  1 OF MENU0701 PROMPT "\<Solicitud de Servicio"
DEFINE BAR  2 OF MENU0701 PROMPT "\<Orden de Reparaci¢n"
DEFINE BAR  3 OF MENU0701 PROMPT "Ord\<enes y Pedidos"
DEFINE BAR  4 OF MENU0701 PROMPT "\<Presupuesto"
DEFINE BAR  5 OF MENU0701 PROMPT "Pedido de \<Repuesto"
DEFINE BAR  6 OF MENU0701 PROMPT "\<Documento de Venta"
DEFINE BAR  7 OF MENU0701 PROMPT "\<Consumo de Repuestos"
DEFINE BAR  8 OF MENU0701 PROMPT "Pendientes x \<T‚cnico"
ON SELECTION POPUP MENU0701 DO MENU0701 WITH BAR()

DEFINE POPUP MENU0702 FROM 15,48 COLOR SCHEME 21
DEFINE BAR  1 OF MENU0702 PROMPT "\<Pedidos por Nø Factura"
DEFINE BAR  2 OF MENU0702 PROMPT "Pedidos por \<Detalle"
DEFINE BAR  3 OF MENU0702 PROMPT "\<Backorders"              SKIP FOR (NIVELL='A1')
DEFINE BAR  4 OF MENU0702 PROMPT "Pedidos por \<C¢digo"
ON SELECTION POPUP MENU0702 DO MENU0702 WITH BAR()

*============= REPORTES

DEFINE POPUP MENU04 FROM 07,47 TO 20,74 COLOR SCHEME 21
DEFINE BAR  1 OF MENU04 PROMPT "Tablas \<Generales"           SKIP FOR (NIVELL#'A7' AND NIVELL#'A4')
DEFINE BAR  2 OF MENU04 PROMPT "\<Modelos"                    SKIP FOR (NIVELL='A1' OR  NIVELL='A3' OR NIVELL='A5')
DEFINE BAR  3 OF MENU04 PROMPT "\<Cliente / Proveedor"        SKIP FOR (NIVELL='A1' OR NIVELL='A5')
DEFINE BAR  4 OF MENU04 PROMPT "Clasificaci¢n de \<T‚cnico"   SKIP FOR (NIVELL='A1' OR NIVELL='A3' OR NIVELL='A5' OR NIVELL='A8' OR NIVELL='A9' OR NIVELL='A5' or nivell='A6')
DEFINE BAR  5 OF MENU04 PROMPT "Tarifa de Mano de \<Obra"     SKIP FOR (NIVELL='A1' OR NIVELL='A3' OR NIVELL='A5' OR NIVELL='A8' OR NIVELL='A9' OR NIVELL='A5')
DEFINE BAR  6 OF MENU04 PROMPT "Atenc\<i¢n Telef¢nica      " SKIP FOR NIVELL='A5'
DEFINE BAR  7 OF MENU04 PROMPT "\<Estado de Ordenes        " SKIP FOR NIVELL='A5'
DEFINE BAR  8 OF MENU04 PROMPT "Servicios a \<Domicilio    " SKIP FOR NIVELL='A5' 
DEFINE BAR  9 OF MENU04 PROMPT "\<Ordenes de Reparaci¢n    " SKIP FOR NIVELL='A5'
DEFINE BAR 10 OF MENU04 PROMPT "\<Producci¢n de T‚cnicos   " SKIP FOR (NIVELL='A1 ' OR NIVELL='A3' OR NIVELL='A9')
DEFINE BAR 11 OF MENU04 PROMPT "E\<stad¡sticas             " SKIP FOR NIVELL='A5'
DEFINE BAR 12 OF MENU04 PROMPT "Cont\<ables                " SKIP FOR (NIVELL='A9' OR NIVELL='A5')
DEFINE BAR 13 OF MENU04 PROMPT "I\<nformes                 " SKIP FOR NIVELL='A5'
DEFINE BAR 14 OF MENU04 PROMPT "Fact. en Ga\<rant¡a        " SKIP FOR (NIVELL='A1 ' OR NIVELL='A8' OR NIVELL='A5')
ON SELECTION POPUP MENU04 DO MENU40 WITH BAR()

DEFINE POPUP MENU406 FROM 13,22  COLOR SCHEME 21
DEFINE BAR 1 OF MENU406 PROMPT "\<Control de Llamadas    "        
DEFINE BAR 2 OF MENU406 PROMPT "\<Llamadas Comprometidas "     
DEFINE BAR 3 OF MENU406 PROMPT "\<Pendientes por Usuario "     
DEFINE BAR 4 OF MENU406 PROMPT "\<Servicios por Usuario "     
ON SELECTION POPUP MENU406 DO MENU406 WITH BAR()

DEFINE POPUP MENU407 FROM 14,22  COLOR SCHEME 21
DEFINE BAR 1 OF MENU407 PROMPT "Estado de \<Ordenes por Fechas"        
DEFINE BAR 2 OF MENU407 PROMPT "Estado de Ordenes por \<Fecha y Marca" 
DEFINE BAR 3 OF MENU407 PROMPT "\<Estado de Ordenes por Emisor"
DEFINE BAR 4 OF MENU407 PROMPT "\<Resumen Estados de Ordenes " 
DEFINE BAR 5 OF MENU407 PROMPT "\<Saldos Actuales de Ordenes" 
DEFINE BAR 6 OF MENU407 PROMPT "\<Pendientes por Cliente" 
DEFINE BAR 7 OF MENU407 PROMPT "Saldo actual de S/\<Domicilio" 
ON SELECTION POPUP MENU407 DO MENU407 WITH BAR()

DEFINE POPUP MENU408 FROM 13,22  COLOR SCHEME 21
DEFINE BAR 1 OF MENU408 PROMPT "\<Recepcionados Por Zonas"
DEFINE BAR 2 OF MENU408 PROMPT "\<Control de Servicio por T‚cnico"   
ON SELECTION POPUP MENU408 DO MENU408 WITH BAR()

DEFINE POPUP MENU409 FROM 11,15 TO 19,58 COLOR SCHEME 21
DEFINE BAR  1 OF MENU409 PROMPT "Ingreso de \<Ordenes por Taller"   
DEFINE BAR  2 OF MENU409 PROMPT "Ingreso de Ordenes por \<Marca"    
DEFINE BAR  3 OF MENU409 PROMPT "Ingreso de Ordenes por mod\<Elo"  
DEFINE BAR  4 OF MENU409 PROMPT "\<Ingreso de Ordenes por emisor"  
DEFINE BAR  5 OF MENU409 PROMPT "Ingreso de Ordenes por Tipo A\<Tenci¢n" 
DEFINE BAR  6 OF MENU409 PROMPT "Ingreso de Ordenes por \<T‚cnico"
DEFINE BAR  7 OF MENU409 PROMPT "Ingreso de Ordenes por \<Distritos"
DEFINE BAR  8 OF MENU409 PROMPT "Ingreso de \<Art¡culos por Fechas" 
DEFINE BAR  9 OF MENU409 PROMPT "Ingreso de art¡culos por Modelo y P¢li\<za"
DEFINE BAR 10 OF MENU409 PROMPT "Ingreso de art¡culos por Emi\<sor"      
DEFINE BAR 11 OF MENU409 PROMPT "Ingreso de art¡culos por Tipo A\<Tenci¢n" 
DEFINE BAR 12 OF MENU409 PROMPT "Ingreso de art¡culos por \<Distritos"
DEFINE BAR 13 OF MENU409 PROMPT "Ingreso de art¡culos por Lineas"
DEFINE BAR 14 OF MENU409 PROMPT "\<Salida de Art¡culos por Fechas"        
DEFINE BAR 15 OF MENU409 PROMPT "Planilla de \<Ordenes Cerradas" 
DEFINE BAR 16 OF MENU409 PROMPT "Planilla de \<Reparados por Fechas"
DEFINE BAR 17 OF MENU409 PROMPT "Planilla de Reparados en \<Fecha"  
DEFINE BAR 18 OF MENU409 PROMPT "Ordenes no \<Cerradas "
DEFINE BAR 19 OF MENU409 PROMPT "Ordenes por Fecha \<Vencimiento"   
DEFINE BAR 20 OF MENU409 PROMPT "Solicitudes Fact\<uradas"          
DEFINE BAR 21 OF MENU409 PROMPT "Ordenes con Re\<puestos por Marca" SKIP FOR NIVELL='A1' 
DEFINE BAR 22 OF MENU409 PROMPT "Ordenes Cerradas x Tipo Serv" 
ON SELECTION POPUP MENU409 DO MENU409 WITH BAR()

DEFINE POPUP MENU410 FROM 14,22 TO 19,60 COLOR SCHEME 21
DEFINE BAR 1 OF MENU410 PROMPT "Pro\<ducci¢n Diaria por T‚cnico"
DEFINE BAR 2 OF MENU410 PROMPT "Producci¢n \<Mensual por t‚cnico"
DEFINE BAR 3 OF MENU410 PROMPT "Promedio \<Servicio por T‚cnico"
DEFINE BAR 4 OF MENU410 PROMPT "Producci¢n por T‚cnico y \<Emisor"
DEFINE BAR 5 OF MENU410 PROMPT "Producci¢n por \<T‚cnico Objetivo"
DEFINE BAR 6 OF MENU410 PROMPT "Control \<Servicio de T‚cnicos"
DEFINE BAR 7 OF MENU410 PROMPT "\<Reclamos por T‚cnico"
DEFINE BAR 8 OF MENU410 PROMPT "Recha\<zos por T‚cnico"
ON SELECTION POPUP MENU410 DO MENU410 WITH BAR()

DEFINE POPUP MENU0411 FROM 16,25  COLOR SCHEME 21
DEFINE BAR 1 OF MENU0411 PROMPT "Por \<Modelo"
DEFINE BAR 2 OF MENU0411 PROMPT "M\<odelos por Marca" 
DEFINE BAR 3 OF MENU0411 PROMPT "\<Tiempos de Atenci¢n     "
ON SELECTION POPUP MENU0411 DO MENU0411 WITH BAR()

DEFINE POPUP MENU0412 FROM 17,25  COLOR SCHEME 21
DEFINE BAR 1  OF MENU0412 PROMPT "\<Salidas por Cortes¡a"                  SKIP FOR (NIVELL#'A7' AND NIVELL#'A4')
DEFINE BAR 2  OF MENU0412 PROMPT "\Devoluciones - N/Cambio y N/Cr‚dito"
DEFINE BAR 3  OF MENU0412 PROMPT "\<Planillas LG"
ON SELECTION POPUP MENU0412 DO MENU0412 WITH BAR()

DEFINE POPUP MENU0413 FROM 13,00  COLOR SCHEME 21
DEFINE BAR 1  OF MENU0413 PROMPT "Desde \<Compra a Gen. S/S"           
DEFINE BAR 2  OF MENU0413 PROMPT "Desde \<S/S a Cierre de O/R"         
DEFINE BAR 3  OF MENU0413 PROMPT "Desde c\<Ierre de O/R a Cliente"     
DEFINE BAR 4  OF MENU0413 PROMPT "Desde \<O/R hasta Cierre O/R"        
DEFINE BAR 5  OF MENU0413 PROMPT "Desde S/S Hasta Cierre Por \<Emisor"
DEFINE BAR 6  OF MENU0413 PROMPT "Estado \<Actual con Movimiento"
DEFINE BAR 7  OF MENU0413 PROMPT "Desde \<O/R hasta Cierre O/R - T‚cnicos"
ON SELECTION POPUP MENU0413 DO MENU0413 WITH BAR()

DEFINE POPUP MENU0414 FROM 16,30  COLOR SCHEME 21
DEFINE BAR 1 OF MENU0414 PROMPT "\<Informe T‚cnico"            
DEFINE BAR 2 OF MENU0414 PROMPT "\<Duplicado de O/R"           SKIP FOR (NIVELL='A1' OR NIVELL='A3' OR NIVELL='A5' OR NIVELL='A6' OR NIVELL='A8' OR NIVELL='A9')
DEFINE BAR 3 OF MENU0414 PROMPT "Con\<Solidado de O/R"         SKIP FOR (NIVELL='A3' OR NIVELL='A5')
ON SELECTION POPUP MENU0414 DO MENU0414 WITH BAR() 

DEFINE POPUP MENU0415 FROM 16,20  COLOR SCHEME 21
DEFINE BAR 1 OF MENU0415 PROMPT "\<Ordenes Fact. de Garant¡a"    SKIP FOR NIVELL='A1 '
DEFINE BAR 2 OF MENU0415 PROMPT "\<Consol. de O/R Fact. en Gtia" SKIP FOR NIVELL='A1'
DEFINE BAR 3 OF MENU0415 PROMPT "\<Fact. en Garant¡a"            SKIP FOR NIVELL='A1'
DEFINE BAR 4 OF MENU0415 PROMPT "Ordenes Facturadas x \<Responsabilidad" SKIP FOR NIVELL='A1 '
ON SELECTION POPUP MENU0415 DO MENU0415 WITH BAR()

*============= ADMINISTRACION
DEFINE POPUP MENU05 FROM 07,57  COLOR SCHEME 21
DEFINE BAR 1 OF MENU05 PROMPT "Cambio de \<Password"
DEFINE BAR 2 OF MENU05 PROMPT "\<Optimizaci¢n de Archivos"  SKIP FOR NIVELL#'A7'
DEFINE BAR 3 OF MENU05 PROMPT "\<Generac. de Archivo ASCII" SKIP FOR NIVELL#'A7'         
DEFINE BAR 4 OF MENU05 PROMPT "\<Cierre Peri¢dico"          SKIP FOR NIVELL#'A7'
DEFINE BAR 5 OF MENU05 PROMPT "\<Registro de Usuarios"      SKIP FOR NIVELL#'A7'
ON SELECTION POPUP MENU05 DO MENU50 WITH BAR()

*****************************************
*  PROCEDIMIENTOS DE APOYO AL SISTEMA   *
*****************************************
DO WHILE .T.
   ACTI WINDOW PANTALLA
   @ 0,0 SAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
   @ 1,0 SAY "³ ACTUALIZACIONES ³   PROCESOS   ³  CONSULTAS  ³   REPORTES   ³ ADMINISTRACION ³"
   @ 2,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
   ACTI SCREEN
   ACTI MENU MENU00
   IF LASTKEY()=27
      RPT=F_YESNO("DESEA SALIR DEL SISTEMA?")
      IF RPT
         ppalqz = .F.
         EXIT
      ENDIF   
   ENDIF
ENDDO   
RETURN ppalqz
                          && *****************************************
PROCEDURE MENU10          && ***** A C T U A L I Z A C I O N E S *****
****************          && *****************************************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORM0101                && Par metros Generales
   CASE BAR=2
        DO TABLAS
   CASE BAR=3
        DO PORM0103 WITH 1         && Maestro de Modelos
   CASE BAR=4
        DO PORM0105 WITH 1         && Registro de Clientes / Proveedores
   CASE BAR=5
        DO PORM0106 WITH 1         && REgistro de T‚cnicos
*   CASE BAR=6
*       ACTI POPUP MENU017          && N£meros de Serie
   CASE BAR=6
        DO PORM0108 WITH 1         && Cat logo de S¡ntomas
   CASE BAR=7
        DO PORM0109                && Registro de Mano de Obra
   CASE BAR=9
        DO PORM0111                && Exporta Registro de Clientes
   CASE BAR=10
        DO PORM0112                && Exporta Registro de AIWA
   CASE BAR=11
        DO PORM0113                && Exporta S/Servicio
   CASE BAR=12
        DO PORM0114                && Actualiza S/Servicio
   CASE BAR=14
        DO PORM0115                && Tipo de Cambio
ENDCASE
RETURN

PROCEDURE MENU017
*****************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORM0171 WITH 1         && Mantenci¢n Manual
   CASE BAR=2
        DO PORM0172 WITH 1         && Mantenci¢n Autom tica
ENDCASE
RETURN

                              &&  **************************************
PROCEDURE MENU20              &&  *****   P  R  O  C  E  S  O  S   *****
****************              &&  **************************************
PARAMETERS BAR                
DO CASE
   CASE BAR=1
        ACTI POPUP MENU021        && Solicitud de Servicio
   CASE BAR=2
        ACTI POPUP MENU022        && Solicitud de Presupuesto
   CASE BAR=3
        ACTI POPUP MENU023        && Ordenes de Reparaci¢n
   CASE BAR=4
        DO PORM0241 WITH 1        && Generaci¢n de Pedido de Repuestos
   CASE BAR=5
        ACTI POPUP MENU025        && Control de Calidad
   CASE BAR=6
        ACTI POPUP MENU026        && Ventas
   CASE BAR=7
        ACTI POPUP MENU027        && Inventario F¡sico
   CASE BAR=8
        ACTIVATE POPUP MENU029    && Salida de Articulos     
   CASE BAR=9
        DO PORM0301               && Descuento de Ordenes
   CASE BAR=10
        DO PORM0302               && Cambios y devoluciones
ENDCASE
RETU
                           
PROCEDURE MENU021             
*****************             
PARAMETERS BAR
set sysmenu off
DO CASE
   CASE BAR=1
        DO PORM0211                      && Generaci¢n Autom tica de S/S
   CASE BAR=2
        DO PORM0201 WITH 2               && Mantenci¢n de S/S
   CASE BAR=3
        DO PORM0201 WITH 3               && Anulaci¢n de S/S
   CASE BAR=4
        DO PORM0201 WITH 1               && Generaci¢n Manual
ENDCASE
RETURN   


PROCEDURE MENU022
*****************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORM0202 WITH 1               && Generaci¢n de S/PPTO
   CASE BAR=2
        DO PORM0202 WITH 2               && Mantenci¢n de S/PPTO
   CASE BAR=3
        DO PORM0202 WITH 3               && Anulaci¢n de S/PPTO
   CASE BAR=4
        DO PORM0204                      && Actualizaci¢n S/PPTO
ENDCASE
RETURN   

PROCEDURE MENU023
*****************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORM0203 WITH 1               && Generaci¢n de Orden
   CASE BAR=2
        DO PORM0203 WITH 2               && Mantenci¢n de Orden
   CASE BAR=3
        DO PORM0203 WITH 3               && Anulaci¢n de Orden
   CASE BAR=4
        DO PORM2031                      && Cambio de Garantia 
   CASE BAR=5
        DO PORM2032                      && Rechazado por Almac‚n de Reparados     
ENDCASE
RETURN   

PROCEDURE MENU025
*****************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORM0251 WITH 1               && Pasa Control de Calidad
   CASE BAR=2
        DO PORM0251 WITH 2               && Anular Control de Calidad / Regresa a Taller de Reparaci¢n
   CASE BAR=3
        DO PORM0252                      && Cierre Presupuesto no Aprobado
   CASE BAR=4
        DO PORM0253                      && Ingreso de Flete     
ENDCASE
RETURN   


PROCEDURE MENU026
*****************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORM0261 WITH 1               && Ventas en L¡nea
   CASE BAR=2
        DO PORM0262 WITH 1               && Ventas en Batch
   CASE BAR=3
        DO PORM0225  with 1              && Pagos a Cuenta
   CASE BAR=4
        DO PORM0263                      && Anulaci¢n de Ventas
   CASE BAR=6
        DO PORM0261 WITH 2               && Registro Manual de Ventas
   CASE BAR=7
        do porm0262 with 2               && Ventas en Batch Manual
   CASE BAR=8
        DO PORM0225 with 2 			     && Pagos a Cuenta Manual
   CASE BAR=10
        DO PORM0210     				 && Reg. de ventas de Sucursales
   CASE BAR=12
        DO PORM0212 WITH 2 				 && Planilla especial LG
 ENDCASE
RETURN   


PROCEDURE MENU027
*****************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORM0271                && Creaci¢n de archivos 
   CASE BAR=2
        DO PORM0272                && Reporte del archivo
   CASE BAR=3
        DO PORM0273                && Registro de Inventario
   CASE BAR=4
        DO PORM0274                && Reporte de Diferencias
   CASE BAR=5
        DO PORM0275                && Eliminaci¢n del Archivo     
ENDCASE
RETURN   

PROCEDURE MENU029
*****************
PARAMETERS BAR
DO CASE 
   CASE BAR=1
        set sysmenu off
        DO PORM0294               && Salida sucursales      
   CASE BAR=2
        set sysmenu off
        DO PORM0295               && Salida cortesia          
   CASE BAR=3
        set sysmenu off
        DO PORM0298               && Salidas en General    
   CASE BAR=4
        set sysmenu off
        DO PORM0297               && Reingreso de Salidas    
   CASE BAR=5
        set sysmenu off
        DO PORM0299               && Gu¡as de Remisi¢n
ENDCASE        
RETURN

                         &&  *****************************************
PROCEDURE MENU30         &&  *****   C  O  N  S  U  L  T  A  S   *****
****************         &&  *****************************************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORM0102 WITH 2        && Tablas Generales
   CASE BAR=2
        DO PORM0103 WITH 2        && Modelos
   CASE BAR=3
        DO PORM0105 WITH 2        && Clientes / Proveedores
   CASE BAR=4
        DO PORM0106 WITH 2        && T‚cnicos
   CASE BAR=5
        DO PORM0104 WITH 2        && Tarifas
   CASE BAR=6
        DO PORM0108 WITH 2        && Cat logo de S¡ntomas
   CASE BAR=7
        ACTI POPUP MENU0701       && Documentos
   CASE BAR=8
        DO PORM0308 
   CASE BAR=9
        DO AGCC0202               && Consulta de Repuestos
   CASE BAR=10
        ACTI POPUP MENU0702       && Importaciones
   CASE BAR=11
        DO porq0172               && Centro de Informaci¢n al cliente
   CASE BAR=12
        DO CARSA                  && GARANTIA CARSA - COLATINA (4 A¤os)
ENDCASE
RETURN

PROCEDURE MENU0701
******************
PARAMETER BAR
DO CASE
   CASE BAR=1
   		DO PORQ0371 WITH 1        && Solicitud de Servicio
   CASE BAR=2
	   	DO PORQ0372 WITH 2        && Ordenes de Reparaci¢n
   CASE BAR=3
   		DO PORQ0376 WITH 3        && Ordenes y Pedidos
   CASE BAR=4
   		DO PORQ0371 WITH 3        && Presupuesto
   CASE BAR=5
   		DO PORQ0374 WITH 2        && Pedido de Repuestos
   CASE BAR=6
   		DO PORQ0375 WITH 1        && Documento de Venta
   CASE BAR=7
   		DO PORQ0377               && Consumo de Repuestos
   CASE BAR=8
        DO PORQ0379               && Pendientes por T‚cnico
ENDCASE
RETURN

PROCEDURE MENU0702
******************
PARAMETER BAR
DO CASE
   CASE BAR=1
   		DO PORQ0312               && Consulta por # factura
   CASE BAR=2
	   	DO PORQ0313               && Consulta de Pedidos al detalle
   CASE BAR=3
   		DO PORQ0314               && Consulta de Backorders
   CASE BAR=4
   		DO PORQ0315               && Consulta de Pedidos por C¢digo
ENDCASE
RETURN
                             &&   **************************************
PROCEDURE MENU40             &&   *****   R  E  P  O  R  T  E  S   *****
****************             &&   **************************************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORL0401               && Tablas Generales
   CASE BAR=2
        DO PORL0402               && Modelos
   CASE BAR=3
        DO PORL0403               && Cliente / Proveedor
   CASE BAR=4
        DO PORL0404               && Clasificaci¢n de T‚cnicos
   CASE BAR=5
        DO PORL0415               && Mano de Obra
   CASE BAR=6
        ACTI POPUP MENU406        && Atenci¢n Tel‚fonica
*       DO PORL0406               
   CASE BAR=7
        ACTI POPUP MENU407        && Estado de Ordenes 
   CASE BAR=8
        ACTI POPUP MENU408        && Servicio a Domicilio
   CASE BAR=9
        ACTI POPUP MENU409        && Orden de Reparaci¢n
   CASE BAR=10     
        ACTI POPUP MENU410        && Producci¢n de T‚cnicos
   CASE BAR=11     
        ACTI POPUP MENU0411       && Estad¡sticas 
   CASE BAR=12     
        ACTI POPUP MENU0412       && Contables
   CASE BAR=13     
        ACTI POPUP MENU0414       && Informes
   CASE BAR=14     
        ACTI POPUP MENU0415       && Facturaci¢n en Garant¡a
ENDCASE
CLOSE DATA
RETURN  

PROCEDURE MENU406
*****************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORL0422 with 1  && Control de Llamadas
   CASE BAR=2
        DO PORL0422 with 2  && Llamadas Comprometidas
   CASE BAR=3
        DO PORL0428         && Pendientes por usuario
   CASE BAR=4
        DO PORL0429         && Servicios por usuario
ENDC
RETU


PROCEDURE MENU407
*****************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORL4130               && Estado de Ordenes por fechas
   CASE BAR=2
        DO PORL4072               && Estado de Ordenes por marca   
   CASE BAR=3
        DO PORL4074               && Estados de Ordenes por Emisor
   CASE BAR=4
        DO PORL4001               && Estados de Ordenes (inventario)
   CASE BAR=5
        DO PORL4002               && Saldos de inventario
   CASE BAR=6
        DO PORL4003               && Pendientes por Cliente
   CASE BAR=7
        DO PORL4004               && Pendientes por Cliente
ENDCASE
RETURN

PROCEDURE MENU408
*****************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORL4081         && Recepcionados por Zonas
   CASE BAR=2
        DO PORL4082         && Control de S/D por T‚cnico
ENDCASE
RETURN

PROCEDURE MENU409
******************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORL4135              &&Ingreso de Ordenes
   CASE BAR=2
        DO PORL4092              && Ingreso de Ordenes por Marca
   CASE BAR=3
*       do aa
        DO PORL4093              && Ingreso de Ordenes por Modelo
   CASE BAR=4 
        DO PORL4094              && Ingreso de Ordenes por Emisor
   CASE BAR=5
        DO PORL4090              && Ingreso de Ordenes por Tipo de Atenci¢n          
   CASE BAR=6
        DO PORL4148              && Ingreso de Ordenes por t‚cnico
   CASE BAR=7
        DO PORL4141              && Ingreso de Ordenes por Distrito 
   CASE BAR=8
        DO PORL4131              && Ingreso de art¡culos
   CASE BAR=9
        DO PORL4095              && Ingreso de art¡culo por Modelo     
   CASE BAR=10
        DO PORL4091              && Ingreso de art¡culo por Emisor     
   CASE BAR=11
        DO PORL4108              && Ingreso de Art¡culos por Tipo Atenci¢n     
   CASE BAR=12
        DO PORL4139              && Ingreso de Art¡culos por Distritos
   CASE BAR=13
        DO PORL4140              && Ingreso de Ordenes por L¡nea
   CASE BAR=14
        DO PORL4132              && Salida de Art¡culos
   CASE BAR=15 
        DO PORL4134              && Planilla de reparados
   CASE BAR=16
        DO PORL4138              && Planilla de Reparados por Fechas
   CASE BAR=17
        DO PORL4096              && Planilla de Reparados en fecha
   CASE BAR=18
        DO PORL4083              && Ordenes No cerradas         
   CASE BAR=19
        DO PORL4097              && Ordenes por Fecha de Vencimiento         
   CASE BAR=20
        DO PORL4098              && Ordenes Cerradas y Facturadas
   CASE BAR=21
        DO PORL4099              && Ordenes con Repuestos por Marca     
   CASE BAR=22
        DO PORL0426              && Ordenes Cerradas x Tipo de Servicio
ENDCASE
RETURN

PROCEDURE MENU410
*****************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORL4133               &&Producci¢n por T‚cnico
   CASE BAR=2
        DO PORL4120               &&Producci¢n Mesual     
   CASE BAR=3
        DO PORL4102               &&Promedio Servicio pot T‚cnico
   CASE BAR=4
        DO PORL4103               &&
   CASE BAR=5
        DO PORL4084               &&Producci¢n T‚cnico Objetivo
   CASE BAR=6
        DO PORL4105               &&Pendiente de T‚cnicos     
   CASE BAR=7
        DO PORL4106               &&Reclamos por T‚cnico     
   CASE BAR=8
        DO PORL4107               &&Rechazos por T‚cnico
ENDCASE
RETURN

PROCEDURE MENU0411
******************
PARAMETERS BAR
DO CASE
   CASE BAR=1 
        DO PORL4116               && Modelos por Proveedor de Gt¡a.
   CASE BAR=2
        DO PORL4113               && Modelos por Marca
   CASE BAR=3
        ACTI POPUP MENU0413       && Tiempos de Demora    
ENDCASE
RETURN

PROCEDURE MENU0412
******************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORL4012             && Salidas por Cortes¡a
   CASE BAR=2
        DO PORL4013             && Devoluciones - N/Cambio y N/Cr‚dito
   CASE BAR=3
        DO PORL4014             && Planillas x facturar LG
ENDCASE
RETURN

PROCEDURE MENU0413
******************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORL4122             && Tiempo desde compra a s/s
   CASE BAR=2
        DO PORL4123             && Tiempo desde s/s a cierre o/r     
   CASE BAR=3
        DO PORL4128             && Tiempo desde cierre o/r a cliente
   CASE BAR=4
        DO PORL4125             && Tiempo promedio de Atencion
  CASE BAR=5
       DO PORL4142              && Tiempo desde s/s a cierre por Emisor      
  CASE BAR=6
       DO PORL4144              && Estado Actual con Movimiento
  CASE BAR=7
       DO PORL4149              && Tiempo de Atencion desde O/R hasta Cierre - T‚cnicos
ENDCASE
RETURN        


PROCEDURE MENU0414
******************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORL0413               && Informe T‚cnico
   CASE BAR=2
        DO PORL0414               && Duplicado de O/R
   CASE BAR=3
        DO PORL0420               && Consolidado de O/R     
ENDCASE
RETURN        


PROCEDURE MENU0415
******************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORL0417               && Ordenes Facturadas en Garant¡a
   CASE BAR=2
        DO PORL0419               && Duplicado de O/R facturadas en Gtia.
   CASE BAR=3
        DO PORL0425               && Facturaci¢n en garant¡a.
   CASE BAR=4
        DO PORL0427               && Facturaci¢n en garant¡a.
ENDCASE
RETURN        

                            &&    *****************************************
PROCEDURE MENU50            &&    *****  A D M I N I S T R A C I O N  *****
****************            &&    *****************************************
PARAMETERS BAR
DO CASE
   CASE BAR=1
        DO PORM0502               && Cambio de Password
   CASE BAR=2
        DO ORAD0501
*       DO PORM0501               && Optimizaci¢n de Archivos
   CASE BAR=3
		RETU
		DO PORM0503               && Generaci¢n de Archivos ASCII
   CASE BAR=4
        DO PORM0506               && Cierres Peri¢dicos
   CASE BAR=5
        DO PORM0507 with 1        && Registro de Usuarios
ENDCASE
RETURN  

function f_yesno
****************
parameters mens,def
private all
l    =iif(len(mens)<18,18, len(mens))
sdef =iif(def=.t.,"\?\<Ok;\!\<Cancel","\?\<No;\!\<Si")
define window wyesno from 18,(80-l)/2-5 to 23,(80+l)/2+4  double color scheme 6
activate window wyesno
@ 1,4 say mens
@ 3,ceiling(l-18)/2 get vok picture "@*HT "+sdef size 1,10,4 default 1
read cycle
release window wyesno
return iif(def=.t., iif(vok=1,.t.,.f.), iif(vok=1,.f.,.t.) )
return

procedure p_menpar
******************
rge_nroruc = par_numruc
rge_razsoc = alltrim(par_razsoc)
rge_abrev  = alltrim(par_abrev)
rge_calle  = alltrim(par_calle)
rge_distri = alltrim(par_distri)
rge_provin = alltrim(par_provin)
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
return
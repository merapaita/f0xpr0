*:*****************************************************************************
*:
*:        Program: F:\BENE2000\ABASPRG\ABAS.PRG
*:
*:         System: MODULO DE ABASTECIMIENTOS
*:         Author: 
*:      Copyright (c) , 
*:  Last modified: 12/01/100     17:00
*:
*:      Documented 12/01/10 at 20:37               FoxDoc  version 2.10f
*:*****************************************************************************
** şÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄş
** º  Programa SubPrincipal : Abas.Prg  -Periodo 2001                    º
** º                                                                     º
** şÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄş
*
clear
close all
clear all

worker = iif( ltrim(upper(getenv('WORKER'))) = 'ON', .t.,.f. )
set procedure to sai_lib
do fox_ambi

DEFINE WINDOW _INVE    FROM 10,27 TO 15,77 DOUBLE FLOAT SHADOW
*
DO lColores

IF SelPer()
	*
ELSE
	RETURN
ENDIF

*ruta='..\DATA'
*ruta='h:\benedata\sicgdata\DATA2005'
*ruta='F:\BENE2005\DATA'
*repos='\bene2001\REPOS'
*rutapr='..\DATAPR'
rutaPr='F:\BENE2004\DATAPR'
*repospr='\bene2001\REPOS'
*
*SET PATH TO &ruta
desa = " SISTEMAS "
if worker
	if yesno("Datos de Prueba?")
		desa = " P R U E B A "
		SET PATH TO &rutapr
	endif
endif

*-- Clave de acceso
vuser_id = alltrim(left(sys(0),15))
IF !FILE('USUARIO.CDX')
   do standby with ' Error en INDEXA '
endif

public vcoddep,vconex,vmaq,vllave,vusurec,vflag,vnumdep,sistema,vsistema,SistCtrl

sistema='2'
SistCtrl = sistema
vconex = alltrim(left(sys(0),8))
vmaq   = alltrim(right(sys(0),2))
vllav  = alltrim(sys(2))

use usuario order tag usuario1  alias usua
IF !Clave()
	CLOSE DATA
	DO STANDBY WITH '!! Acceso Denegado !!',10,15
	IF Worker
		CANCEL
	ENDIF
	QUIT
ENDIF


vUser_ID = ALLTRIM(Usuario)

vnombre  = alltrim(nombre)
vcoddep  = coddep
vnumdep  = numdep
vflag    = flag
use

use iteusu order tag iteusu1   alias iteu
sele iteu
set filt to sistema='2'
vusucla = chrtran(vuser_id,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                           'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
vacceso = ' '

seek vusucla

scan while vusucla = alltrim(iteu.usucla)
	vacceso = vacceso + iteu.modulo
endscan
set filt to
use

=SAVCON(1)

**-- Definiciones para el Men£ --**
*----------------------------------*
use parmae order tag parmae1 alias parma
seek "EMPRES"
if !eof()
	cia  = alltrim(Descri)			&& abreviatura de la empresa
	detaw  = alltrim(Descriaux)	    && detalle del nombre de la empresa
	direw  = alltrim(Descriau2)   	&& detalle de la direccion de empresa
	telew  = Codigoaux		    	&& nro. de telefono de la empresa
	IF SEEK("RUCEMP01")
		rucw = ALLTRIM(DescriAux)
	 ELSE
		rucw = "           "
	ENDIF
else
*	=poperror( 'El Sistema ha encontrado UN ERROR.... NO EXISTE datos generales de la empresa' +;
*	    '               CONSULTE con su Administrador de Sistemas      Por Seguridad se ABANDORA del Sistema')
*	close data
*	return
endif
use
subc = " "
*desa = " SISTEMAS "
titu = "   Sistema de Abastecimientos "
vsistema = '1'
**-- Colores
escolor = iscolor()
if escolor
   set color to ,n/w,b
   c_panta = 8
   c_borde = 10
   c_popup = 4  &&15
   c_texto = "N/W"
   c_fondo = 1
else
   c_panta = 8
   c_borde = 7
   c_popup = 3
   c_texto = "N+/W"
   c_fondo = 1
endif

set message to 23 center

@  1, 0,23,79  box "°°°°°°°°°" color scheme c_fondo
@  1, 1,23,24  box "±±±±±±±±±" color scheme c_fondo


spac = (80 -(len(cia)+len(titu)+10))/2
rotulo1 = cia + space(spac) + titu + spac(spac) + space(11)

usuarios = '® USER: '+vnombre+' ¯'
fecha    = dtoc(date()) +space(2)
rotulo2  = padr(desa,17,' ')+padc(usuarios,46,' ')+padl(fecha,17,' ')
do logos with rotulo1,rotulo2,50

save screen to principal

* ERROR TRAPPING SYSTEM
if worker
	on error
else
	on error do fox_errs with program()
endif

save screen to pantalla

set clock on
store .t. to ven_sistema
store .f. to esc_tecla
= sys(2002,1)

USE PARMAE ORDER TAG Parmae1 ALIAS Parma
SEEK 'UNIEJE'
IF FOUND()
	vUEje = ALLTRIM(CodigoAux)
ENDIF

SEEK 'UNIGES'
IF FOUND()
	vUGes = ALLTRIM(Codigo)
ENDIF

*--- El men£ principal ---**
l_col  ="W+/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "

** şÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄş
** º      Abastecimiento       º
** şÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄş

*IF escolor
*   DEFINE POPUP menu FROM 5,2 TO 17,22 SHADOW COLOR &L_COL
*ELSE
*   DEFINE POPUP menu FROM 5,2 TO 17,22 COLOR SCHEME C_POPUP
*ENDIF

*DEFINE BAR 1  OF  menu PROMPT ' \<Programaci¢n   ' MESSAGE ''
*DEFINE BAR 2  OF  menu PROMPT ' \<Obtenciones    ' MESSAGE ''
*DEFINE BAR 3  OF  menu PROMPT ' a\<Lmacen        ' MESSAGE ''
*DEFINE BAR 4  OF  menu PROMPT ' ser\<Vicios      ' MESSAGE ''
*DEFINE BAR 5  OF  menu PROMPT ' \<Kardex         ' MESSAGE ''
*DEFINE BAR 6  OF  menu PROMPT ' \<Transportes    ' MESSAGE ''
*DEFINE BAR 7  OF  menu PROMPT ' \<Cat logos      ' MESSAGE ''
*DEFINE BAR 8  OF  menu PROMPT ' ta\<Blas         ' MESSAGE ''
*DEFINE BAR 9  OF  menu PROMPT ' \<Utilitarios    ' MESSAGE ''
*DEFINE BAR 10 OF  menu PROMPT ' co\<Nsultas      ' MESSAGE ''
*DEFINE BAR 11 OF  menu PROMPT ' \<Salida          '


if escolor
   define popup menu1 from 5,2 to 17,22 shadow color &l_col
else
   define popup menu1 from 5,2 to 17,22 color scheme c_popup
endif
*DEFINE BAR 1  OF  menu PROMPT ' \<Programaci¢n   ' MESSAGE ''
*DEFINE BAR 6  OF  menu PROMPT ' \<Transportes    ' MESSAGE ''
*DEFINE BAR 3  OF  menu PROMPT ' a\<Lmacen        ' MESSAGE ''

define bar 1  of  menu1 prompt ' \<Obtenciones    ' message 'Men£ de Opciones para OBTENCIONES : Pecosas, O/C '
define bar 2  of  menu1 prompt ' ser\<Vicios      ' message 'Men£ de Opciones para Procesos de SERVICIOS : Solicitud Servicio, O/S'
define bar 3  of  menu1 prompt ' \<Cat logos      ' message 'Men£ de Opciones para Actualizar : Proveedores, Art¡culos,Servicios'
define bar 4  of  menu1 prompt ' ta\<Blas         ' message 'Men£ de Opciones para Actualizar las TABLAS maestras del Sistema '
define bar 5  of  menu1 prompt ' \<Utilitarios    ' message 'Men£ de UTILITARIOS de Mantenimiento y Seguridad del Sistema'
define bar 6  of  menu1 prompt ' co\<Nsultas      ' message 'Men£ de Consultas de Documentaci¢n Administrativa '
define bar 7  of  menu1 prompt ' \<Despachos      ' message 'Registra y Actualiza los DESPACHOS DE PECOSAS DE STOCK'
define bar 8  of  menu1 prompt ' \<Polizas        ' message 'Actualiza Registro de POLIZAS DE ENTRADA Y SALIDA'
DEFINE BAR 9  OF  menu1 PROMPT ' \<Kardex         ' MESSAGE 'Permite Actualizar Saldo Inicial y Consultas de Stocks disponibles'
DEFINE BAR 10 OF  menu1 PROMPT ' \<Nota Ent. Alm. ' MESSAGE 'Permite ingresar Notas de Entrada a Almacen (Donaciones)'
define bar 11 of  menu1 prompt ' \<Salida        '  MESSAGE 'SALE DEL SISTEMA' 

*- Programaci¢n
if escolor
	define popup pop_02a from 5,26 shadow color &l_col
 else
	define popup pop_02a from 5,26 color scheme c_popup
endif
define bar 1  of  pop_02a prompt '\<Cuadro necesidades' message ''
define bar 2  of  pop_02a prompt '\<Verifica cuadro   ' message ''
define bar 3  of  pop_02a prompt 'c\<Onsolidaci¢n     ' message ''

*- Obtenciones
if escolor
   define popup pop_02b from 5,26 shadow color &l_col
else
   define popup pop_02b from 5,26 color scheme c_popup
endif

*-Obtenciones
define bar 1  of  pop_02b prompt '\<Registro pedidos-PECOSAS' message 'Registra y Genera PECOSAS de Stock y de Compra'
define bar 2  of  pop_02b prompt '\<Solicitud cotizaci¢n  ' message 'Elabora Solicitudes de COTIZACION PARA COMPRAS'
define bar 3  of  pop_02b prompt 'recepci¢n \<Cotizaciones' message ''
define bar 4  of  pop_02b prompt '\<Adjudicaci¢n          ' message ''
define bar 5  of  pop_02b prompt '\<Orden de compra       ' message 'Registra y Genera ORDENES DE COMPRA'

DEFINE BAR 6  OF  pop_02b PROMPT '\<Parte de anulaci¢n    ' MESSAGE ''
DEFINE BAR 7  OF  pop_02b PROMPT 'parte de re\<Baja       ' MESSAGE ''

*DEFINE BAR 8  OF  pop_02b PROMPT 'co\<Nsulta calendario   ' MESSAGE ''
*DEFINE BAR 9  OF  pop_02b PROMPT 'consul\<Ta saldos marco ' MESSAGE ''
*DEFINE BAR 10 OF  pop_02b PROMPT 'conci\<Liaci¢n mensual  ' MESSAGE ''
*DEFINE BAR 11 OF  pop_02b PROMPT 'conc\<Iliaci¢n diaria   ' MESSAGE ''
**DEFINE BAR 12 OF  pop_02b PROMPT 'conciliaci¢n ba\<Yovar  ' MESSAGE ''

*- P¢lizas
if escolor
   define popup pop_02cc from 12,26 shadow color &l_col
else
   define popup pop_02cc from 12,26 color scheme c_popup
endif

*-P¢lizas

define bar 1  of  pop_02cc prompt '\<Entrada        ' message 'Registra y Genera POLIZAS DE ENTRADA'
define bar 2  of  pop_02cc prompt '\<Salida         ' message 'Registra y Genera POLIZAS DE SALIDA'

*- Almacen
if escolor
   define popup pop_02c from 07,26 shadow color &l_col
else
   define popup pop_02c from 07,26 color scheme c_popup
endif

*-Almacen

define bar 1  of  pop_02c prompt '\<Gu¡a internamiento       ' message ''
define bar 2  of  pop_02c prompt '\<Despacho                 ' message ''
define bar 3  of  pop_02c prompt '\<Nota de entrada almac‚n  ' message ''
define bar 4  of  pop_02c prompt '\<P¢lizas                 ' message ''

if escolor
   define popup pop_02ca from 10,54 shadow color &l_col
else
   define popup pop_02ca from 10,54 color scheme c_popup
endif

*-Polizas
define bar 1 of pop_02ca prompt "\<Entrada  "
define bar 2 of pop_02ca prompt "\<Salida   "
*DEFINE BAR 3 OF pop_02ca PROMPT "en\<Trada Bayovar"


*- Servicios
if escolor
   define popup pop_02d from 06,26 shadow color &l_col
else
   define popup pop_02d from 06,26 color scheme c_popup
endif

*-Servicios

define bar 1  of  pop_02d prompt '\<Solicitud de servicio     ' message ''
define bar 2  of  pop_02d prompt 'so\<Licitud cotizaci¢n servicios' message ''
define bar 3  of  pop_02d prompt 'recepci¢n \<Cotizaciones    ' message ''
define bar 4  of  pop_02d prompt '\<Adjudicaci¢n              ' message ''
define bar 5  of  pop_02d prompt 'o\<Rden de Servicio         ' message ''
DEFINE BAR 6  OF  pop_02d PROMPT '\<Parte de anulaci¢n        ' MESSAGE ''
DEFINE BAR 7  OF  pop_02d PROMPT 'parte re\<Baja              ' MESSAGE ''


*DEFINE BAR 8  OF  pop_02d PROMPT 'co\<Nsulta calendario       ' MESSAGE ''
*DEFINE BAR 9  OF  pop_02d PROMPT 'consul\<Ta saldos marco     ' MESSAGE ''
*DEFINE BAR 10 OF  pop_02d PROMPT 'c\<Onciliaci¢n mensual      ' MESSAGE ''

*- Kardex
if escolor
   define popup pop_02e from  9,26 shadow color &l_col
else
   define popup pop_02e from  9,26 color scheme c_popup
endif

*-Kardex

define bar 1  of  pop_02e prompt '\<Existencias valoradas   ' message ''
define bar 2  of  pop_02e prompt '\<Control visible almacen ' message ''
define bar 3  of  pop_02e prompt '\<Stock                   ' message ''
define bar 4  of  pop_02e prompt 's\<Aldos inventario       ' message ''
define bar 5  of  pop_02e prompt 'sa\<ldos inicial          ' message ''
define bar 6  of  pop_02e prompt '\<Reporte Estado Inicial de Productos ' message ''

*- Transportes
if escolor
   define popup pop_02i from 10,26 shadow color &l_col
else
   define popup pop_02i from 10,26 color scheme c_popup
endif

*-Transportes

define bar 1  of  pop_02i prompt '\<Registro de vales  ' message ''
define bar 2  of  pop_02i prompt '\<Agrupa vales       ' message ''

*- Cat logos
if escolor
   define popup pop_02f from 07,26 shadow color &l_col
else
   define popup pop_02f from 07,26 color scheme c_popup
endif

*-Catalogos

define bar 1  of  pop_02f prompt '\<Proveedores    ' message ''
define bar 2  of  pop_02f prompt '\<Art¡culos      ' message ''
define bar 3  of  pop_02f prompt '\<Servicios      ' message ''
*DEFINE BAR 4  OF  pop_02f PROMPT 'p\<Recios        ' MESSAGE ''
*DEFINE BAR 5  OF  pop_02f PROMPT 'r\<Ecord         ' MESSAGE ''
*DEFINE BAR 6  OF  pop_02f PROMPT '\<Veh¡culos      ' MESSAGE ''
*DEFINE BAR 7  OF  pop_02f PROMPT 'c\<Hoferes       ' MESSAGE ''
*DEFINE BAR 8  OF  pop_02f PROMPT 'serv.ve\<H¡culos ' MESSAGE ''

*- Tablas
if escolor
   define popup pop_02g from 08,26 shadow color &l_col
else
   define popup pop_02g from 08,26 color scheme c_popup
endif

define bar 1  of  pop_02g prompt '\<Grupos gen‚ricos  ' message ''
define bar 2  of  pop_02g prompt '\<Dependencias      ' message ''
define bar 3  of  pop_02g prompt '\<Tipo de servicios ' message ''
define bar 4  of  pop_02g prompt '\<Procedencia       ' message ''
define bar 5  of  pop_02g prompt 'd\<Estino           ' message ''
define bar 6  of  pop_02g prompt '\<Unidad medida     ' message ''
define bar 7  of  pop_02g prompt '\<Jefaturas         ' message 'Actualiza la tabla de JEFES DE OFICINAS'

*- Utilitarios
if escolor
   define popup pop_02h from 09,26 margin  shadow color &l_col
else
   define popup pop_02h from 09,26 margin  color scheme c_popup
endif

define bar  1 of pop_02h prompt '\<Indexar archivos     '
define bar  2 of pop_02h prompt '\<Backup de los archivos'
define bar  3 of pop_02h prompt 'De\<puraci¢n de backups '
define bar  4 of pop_02h prompt '\<Correlativos         '
define bar  5 of pop_02h prompt '\-'
define bar  6 of pop_02h prompt '\<Seguridad         ' message '®® Acceso RESTRINGIDO ¯¯'
define bar  7 of pop_02h prompt '\<Seguridad Opciones' message '®® Acceso RESTRINGIDO ¯¯'
define bar  8 of pop_02h prompt '\<Usuario ABAS      '

*- Correlativos
if escolor
   define popup pop_02ha from 12,54 shadow color &l_col
else
   define popup pop_02ha from 12,54 color scheme c_popup
endif
define bar 1  of  pop_02ha prompt '\<Proveedores        ' message 'Actualiza el correlativo de REGISTRO DE PROVEEDORES'
define bar 2  of  pop_02ha prompt 'orden de \<Compra    ' message 'Actualiza el correlativo de ORDENES DE COMPRA '
define bar 3  of  pop_02ha prompt 'orden de \<Servicio  ' message 'Actualiza el correlativo de ORDENES DE SERVICIO'
define bar 4  of  pop_02ha prompt 'p\<E.co.sa           ' message 'Actualiza el correlativo de PECOSAS '
define bar 5  of  pop_02ha prompt 's\<Olicitud Servicio ' message 'Actualiza el correlativo de SOLICITUDES DE SERVICIO'
define bar 6  of  pop_02ha prompt 'c\<Uadro Comnparativo' message 'Actualiza el correlativo de CUADRO COMPARATIVO'
define bar 7  of  pop_02ha prompt 'P¢liza de En\<Trada  ' message "Actualiza el correlativo de P¢liza de ENTRADA "
define bar 8  of  pop_02ha prompt 'P¢liza de Sa\<Lida   ' message "Actualiza el correlativo de P¢liza de SALIDA "
define bar 9  of  pop_02ha prompt 'Cotizaci¢n de \<Bienes' message "Actualiza el correlativo de Solicitudes de Cotizaciones de BIENES"
*- Consultas
if escolor
   define popup pop_02j from 3,26 shadow color &l_col
else
   define popup pop_02j from 3,26 color scheme c_popup
endif
define bar 1  of  pop_02j prompt '\<a. Pe.co.sa.            '  message ''
define bar 2  of  pop_02j prompt '\<b. Orden de Compra      '  message ''
define bar 3  of  pop_02j prompt '\<c. Solicitud de Servicio'  message ''
define bar 4  of  pop_02j prompt '\<d. Orden de Servicio    '  message ''
define bar 5  of  pop_02j prompt '\<e. Proveedores          '  message ''
define bar 6  of  pop_02j prompt '\<f. Art¡culos            '  message ''
define bar 7  of  pop_02j prompt '\<g. P¢lizas de Entrada   '  message ''
define bar 8  of  pop_02j prompt '\<h. P¢lizas de Salida    '  message ''
define bar 9  of  pop_02j prompt '\<i. Parte Anulacion O/C  '  message ''
define bar 10 of  pop_02j prompt '\<j. Parte Anulacion O/S  '  message ''
define bar 11 of  pop_02j prompt '\<k. Parte Rebaja O/C     '  message ''
define bar 12 of  pop_02j prompt '\<l. Parte Rebaja O/S     '  message ''
define bar 13 of  pop_02j prompt '\-'
define bar 14 of  pop_02j prompt '\<m. Hojas de Control     '  message ''
define bar 15 of  pop_02j prompt '\<n. Comprobantes de Pago '  message ''
define bar 16 of  pop_02j prompt '\<o. Cheques              '  message ''
define bar 17 of  pop_02j prompt '\<p. Hojas Modificaci¢n   '  message ''
define bar 18 of  pop_02j prompt '\<q. Seguimiento Doc.Fte  '  message ''
* DEFINE BAR 19 OF  pop_02j PROMPT '\<r. Registro de H/C      '  MESSAGE ''

on selection popup menu1 do act_menus

IF !vFlag$'J*'
	DO ABREPAS
ENDIF

**- La sombra del principal
save screen to principal
activate popup menu1
do while  empty(prompt())
   activate popup menu1
enddo

*-Cerrar todo...
on key
return

*-- Fin  del Programa Principal



procedure act_menus
*------------------
choice=bar()
do case
    *    CASE choice = 1    && Programaci¢n
    *       ON SELECTION POPUP pop_02a  DO  menu_02a
    *       ACTIVATE POPUP pop_02a
case choice = 1    && Obtenciones
   on selection popup pop_02b  do  menu_02b
   activate popup pop_02b
   *    CASE choice = 3    && Almacen
   *         ON SELECTION POPUP pop_02c  DO  menu_02c
   *         ACTIVATE POPUP pop_02c
case choice = 2    && Servicios
   on selection popup pop_02d  do  menu_02d
   activate popup pop_02d
   *    CASE choice = 5    && Kardex
   *         ON SELECTION POPUP pop_02e  DO  menu_02e
   *         ACTIVATE POPUP pop_02e
   *    CASE choice = 6    && transportes
   *         ON SELECTION POPUP pop_02i  DO  menu_02i
   *         ACTIVATE POPUP pop_02i
case choice = 3    && Catalogos
   on selection popup pop_02f  do  menu_02f
   activate popup pop_02f
case choice = 4    && Tablas
	on selection popup pop_02g  do  menu_02g
	activate popup pop_02g
case choice = 5    && Utilitarios
   on selection popup pop_02h  do  menu_02h
   activate popup pop_02h
case choice = 6    && Consultas
   on selection popup pop_02j  do  menu_02j
   activate popup pop_02j
case choice = 7    && Despachos
   do pecsal with vsistema
case choice = 8    && P¢lizas
	on selection popup pop_02cc  do  menu_02cc
	activate popup pop_02cc
CASE choice = 9    && Kardex
	ON SELECTION POPUP pop_02e  DO  menu_02e
	ACTIVATE POPUP pop_02e
CASE choice = 10   && N.E.A.
	do notalm
case bar() =  11
   do salmenu
endcase

restore screen from principal
return

procedure menu_02a  && Programaci¢n
*-----------------
choice=bar()
do case
case choice = 1 && Cuadro de necesidades
   do regcdr
case choice = 2 && Verifica Cdro Necesidades
   do vercdr
case choice = 3 && Consolida Cdro necesidades
   do concdr
endcase
if !escolor
   restore screen from principal
   on selection popup pop_02a do menu_02a
endif
restore screen from principal
show popup pop_02a,menu1
hide window all
return



procedure menu_02b  && Obtenciones
*----------------
choice=bar()
do case
case choice = 1    && Registro PE.CO.SA
   do regpec with 2,' ',vsistema
case choice = 2    && Solicitud de Cotizaci¢n
   do solcot with 1
case choice = 3    && Registro de Cotizaci¢n
   do regcot with 1
case choice = 4    && Cuadro Comparativo Cotizaci¢n
   do adjcot with 1
case choice = 5    && Orden de Compra
   do ordcom with vsistema,1
case choice = 6    && Parte Anulaci¢n
   do anupar with 1
case choice = 7    && Parte Rebaja
   do rebpar with 1,vsistema
case choice = 8    && Consulta Calendario
   do consal
case choice = 9    && Consulta Calendario
   do conpre
case choice = 10   && Conciliacion Mensual
   do conord  with 'O/C'
case choice = 11   && Conciliacion Diaria
   do conord1 with 'O/C'
case choice = 12   && Conciliacion Bayovar
   do conordb with 'O/C' &&Conciliaci¢n Bayovar
endcase
if !escolor
   restore screen from principal
   on selection popup pop_02b do menu_02b
endif
restore screen from principal
show popup pop_02b,menu1
hide window all
return


procedure menu_02c   && Almacen
*----------------
choice=bar()
do case
case choice = 1    && Registro de Guias de internamiento
   do reggui with vsistema
case choice = 2    && Registro de Salidas
   do pecsal with vsistema
case choice = 3    && Nota de entrada alm.
   do notalm
case choice = 4    && POLIZAS DE ENTRADA Y SALIDA
   on selection popup pop_02ca  do  menu_02ca
   activate popup pop_02ca
endcase
if !escolor
   restore screen from principal
   on selection popup pop_02c do menu_02c
endif
restore screen from principal
show popup pop_02c,menu1
hide window all
return

procedure menu_02cc   &&  Opci¢n P¢lizas
*------------------
choice=bar()
do case
case choice = 1    && Entrada
   do polent1
case choice = 2    && Salida
   do polsal
endcase
if !escolor
   restore screen from principal
   on selection popup pop_02cc do menu_02cc
endif
restore screen from principal
show popup pop_02cc,menu1
hide window all
return


procedure menu_02ca   && P¢lizas de la Opci¢n Almac‚n
*-----------------
choice=bar()
do case
case choice = 1    && entrada
   *DO polent
   do polent1
case choice = 2    && salida
   do polsal
endcase
if !escolor
   restore screen from principal
   on selection popup pop_02ca do menu_02ca
endif
restore screen from principal
show popup pop_02ca,pop_02c,menu1
hide window all
return


procedure menu_02d   && Servicios
*-----------------
choice=bar()
do case
case choice = 1    && registro solicitud de servicio
	do regsol with vsistema
case choice = 2    && solicitud de cotizacion de servicio
	do solcot with 2
case choice = 3    && registro proveedores de servicio
	do regcot with 2
case choice = 4    && adjudicaci¢n proveedor
	do adjcot with 2
case choice = 5    && registro Orden de Servicio
	do ordser with vsistema,1
case choice = 6    && Parte Anulaci¢n
	do anupar with 2
case choice = 7    && Parte Rebaja
	do rebpar with 2,vsistema
case choice = 8    && Consultas
   do consal
case choice = 9    && Consulta Calendario
   do conpre
case choice = 10   && Conciliacion
   do conord with 'O/S'
case choice = 11   && Conciliaci¢n Bayovar Servicios
   do conordb with 'O/S'
endcase
if !escolor
   restore screen from principal
   on selection popup pop_02d do menu_02d
endif
restore screen from principal
show popup pop_02d,menu1
hide window all
return


procedure menu_02e   && Kardex
*-----------------
choice=bar()
do case
case choice = 1 && Existencias valoradas
   do regkar
case choice = 2 && Control visible alm.
   do regalm
case choice = 3 && Pedido comprobante salida
   *do stock
   DO PRESENTA
case choice = 4 && Pedido comprobante salida
   do stockin
case choice = 5 && Stock inicial de Cada Producto
    do Saldini
case choice = 6 && Reporte del Stock inicial de Todos los Productos
	DO REPTOTAL
endcase

if !escolor
   restore screen from principal
   on selection popup pop_02e do menu_02e
endif
restore screen from principal
show popup pop_02e,menu1
hide window all
return

procedure menu_02i   && Transportes
*-----------------
choice=bar()

do case
case choice = 1 && Existencias valoradas
   do regval
case choice = 2 && Control visible alm.
   do agrupa
endcase

if !escolor
   restore screen from principal
   on selection popup pop_02i do menu_02i
endif

restore screen from principal
show popup pop_02i,menu1
hide window all

return


procedure menu_02f   && Catalogos
*-----------------
choice=bar()
do case
case choice = 1    && Proveedores
   * do promae
   do promae2
case choice = 2    && Art¡culos
   do artmae
case choice = 3    && Servicios
   do sermae
   *    CASE choice = 4    && Precios
   *         do precio1
   *    CASE choice = 5    && Record
   *         do record
   *    CASE choice = 6    && Vehiculos
   *         do regveh
   *    CASE choice = 7    && CHOFERES Vehiculos
   *         DO manpar WITH "CODCHF","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"
   *    CASE choice = 8    && SERVICIO Vehiculos
   *         DO manpar WITH "CODSER","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"
endcase
if !escolor
   restore screen from principal
   on selection popup pop_02f do menu_02f
endif
restore screen from principal
show popup pop_02f,menu1
hide window all
return

procedure menu_02g   && Tablas
*-----------------
choice=bar()
do case
case choice = 1    && Grupos Gen‚ricos
   do manpar with "CODGEB","Grupo Gen‚rico","Grupos Gen‚ricos",1,"Valor siguiente"
case choice = 2    && DEPENDENCIAS
   do manpar with "CODDEP","Dependencia","Dependencias",1,"Valor siguiente"
case choice = 3    && Tipo de Servicio
   do manpar with "TIPSER","Tipo de Servicio","Tipos de Documentos"
case choice = 4    && Correlativo  Procedencias
   do manpar with "CODPRC","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"
case choice = 5    && Correlativo  DESTINOS
   do manpar with "CODDET","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"
case choice = 6    &&
   do manpar with "CODUNI","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"
case choice = 7	   && Jefaturas
   do manjef
endcase
if !escolor
   restore screen from principal
   on selection popup pop_02g do menu_02g
endif
restore screen from principal
show popup pop_02g,menu1
hide window all
return


procedure menu_02h  && Utilitarios
*----------------
choice=bar()
do case
case choice = 1    && Reindexado
   =poperror( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar el proceso de reordenamiento sin ning£n problema')
   do indexaba
case choice = 2    && Backup
   =poperror( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar la copia de seguridad sin ning£n problema')
   do backupx
   * DO BackUp
case choice = 3    && Depura Backup
   do restorex
   * DO Restore1
case choice = 4    && Correlativo
   on selection popup pop_02ha  do  menu_02ha
   activate popup pop_02ha
case choice = 6
	do segabas WITH vUser_Id
case choice = 7
	do segabOp WITH vUser_Id
case choice = 8
   do contusu
endcase
if !escolor
   restore screen from principal
   on selection popup pop_02h do menu_02h
endif
restore screen from principal
show popup pop_02h,menu1
hide window all
return

procedure menu_02ha  && Correlativos
*----------------
choice=bar()
do case
case choice = 1    && Correlativo Proveedores
   do manpar with "CORRELPROVEE","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
case choice = 2    && Correlativo O/C
   do manpar with "CORRELORDENC","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
case choice = 3    && Correlativo  O/S
   do manpar with "CORRELORDENS","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
case choice = 4    && Correlativo  PECOSA
   do manpar with "CORRELPECOSA","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
case choice = 5    && Correlativo Solicitud de Servicios
   do manpar with "CORRELSOLSER","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
case choice = 6    && Correlativo  CCC
   do manpar with "CORRELCCC",   "Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
case choice = 7    && Correlativo  P¢lizas de Entrada
   do manpar with "CORRELPOLENT",   "Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
case choice = 8    && Correlativo P¢lizas de Salida
   do manpar with "CORRELPOLSAL",   "Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
case choice = 9    && Correlativo de Cotizaciones de Bienes"
	do manpar with "CORRELSOLCOT",   "Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
endcase
if !escolor
   restore screen from principal
   on selection popup pop_02h do menu_02h
endif
restore screen from principal
show popup pop_02h,menu1
hide window all
return

procedure menu_02j  && Consultas
*-----------------
choice=bar()
do case
case choice = 1    && Pecosas
   do conpec
case choice = 2    && O/C
   do conoc
case choice = 3    && S/S
   do conss
case choice = 4    && O/S
   do conos
case choice = 5    && Proveedores
   do conprv
case choice = 6    && Articulos
   do conart
case choice = 7    && poliza ent
   do conpent
case choice = 8    && poliza sal
   do conpsal
case choice = 9    && parte anul o/c
   do conpan with 1
case choice = 10   && parte anul o/s
   do conpan with 2
case choice = 11   && parte reb o/c
   do conprb with 1
case choice = 12   && parte reb o/s
   do conprb with 2
case choice = 14   && hojas de control
   do conhc
case choice = 15   && comprobantes
   do concp
case choice = 16   && cheques
   do conch
case choice = 17   && hojas de modif
   do conhm
case choice = 18   && seguimiento de documentos
   do condoc
endcase
if !escolor
   restore screen from principal
   on selection popup pop_02j do menu_02j
endif
restore screen from principal
show popup pop_02j,menu1
hide window all
return


procedure salmenu
*----------------
store  yesno('¨ Estamos terminando la sesi¢n ?') to in_secoemsa
if in_secoemsa
   set color to
   close data
   hide popup all
   =SavCon(2)
   if worker
      clear
      set help on
      on key
      set exclu off
      cancel
   else
      restore screen from principal
      do clrscr
      quit
   endif
else
   return
endif



procedure salmenu2
*----------------
if yesno(' Estamos terminando la sesi¢n ')
   set color to
   close data
   close proc
   hide popup all
   restore screen from principal
   do clrscr1
   set help on
   deactivate window all
   if worker
      set exclu off
      clear all
      cancel
   endif
   quit
endif
return


procedure abrepas
*----------------
*-Programaci¢n
set skip of bar 1  of pop_02a !'A' $vacceso
set skip of bar 2  of pop_02a !'B' $vacceso
set skip of bar 3  of pop_02a !'C' $vacceso

*-Obtenciones
set skip of bar 1  of pop_02b !'D' $vacceso
set skip of bar 2  of pop_02b !'E' $vacceso
set skip of bar 3  of pop_02b !'F' $vacceso
set skip of bar 4  of pop_02b !'G' $vacceso
set skip of bar 5  of pop_02b !'H' $vacceso
set skip of bar 6  of pop_02b !'I' $vacceso
set skip of bar 7  of pop_02b !'J' $vacceso
set skip of bar 8  of pop_02b !'K' $vacceso
set skip of bar 9  of pop_02b !'L' $vacceso
set skip of bar 10 of pop_02b !'M' $vacceso
set skip of bar 11 of pop_02b !'N' $vacceso
set skip of bar 12 of pop_02b !'¥' $vacceso &&Bayovar

*-Almacen
set skip of bar 7  of MENU1 !'P' $vacceso
set skip of bar 10  of MENU1 !'P' $vacceso

*-Polizas
set skip of bar 1  of pop_02cc !'R' $vacceso
set skip of bar 2  of pop_02cc !'S' $vacceso


* O,Q,T,U DISPONIBLES

*-Servicios
set skip of bar 1  of pop_02d !'V' $vacceso
set skip of bar 2  of pop_02d !'W' $vacceso
set skip of bar 3  of pop_02d !'X' $vacceso
set skip of bar 4  of pop_02d !'Y' $vacceso
set skip of bar 5  of pop_02d !'Z' $vacceso
set skip of bar 6  of pop_02d !'a' $vacceso
set skip of bar 7  of pop_02d !'b' $vacceso
set skip of bar 8  of pop_02d !'c' $vacceso
set skip of bar 9  of pop_02d !'d' $vacceso
set skip of bar 10 of pop_02d !'e' $vacceso

* OJO LA "f" YA SE UTILIZà

*-Kardex
set skip of bar 1  of pop_02e !'g' $vacceso
set skip of bar 2  of pop_02e !'h' $vacceso
set skip of bar 3  of pop_02e !'i' $vacceso
set skip of bar 4  of pop_02e !'j' $vacceso
set skip of bar 4  of pop_02e !'j' $vacceso
set skip of bar 4  of pop_02e !'j' $vacceso

*-Catalogos
set skip of bar 1  of pop_02f !'k' $vacceso
set skip of bar 2  of pop_02f !'l' $vacceso
set skip of bar 3  of pop_02f !'m' $vacceso
set skip of bar 4  of pop_02f !'n' $vacceso
set skip of bar 5  of pop_02f !'¤' $vacceso
set skip of bar 6  of pop_02f !'o' $vacceso
set skip of bar 7  of pop_02f !'p' $vacceso
set skip of bar 8  of pop_02f !'q' $vacceso

*-Transortes
set skip of bar 1  of pop_02i !'r' $vacceso
set skip of bar 2  of pop_02i !'s' $vacceso

*-Tablas
set skip of bar 1  of pop_02g !'t' $vacceso
set skip of bar 2  of pop_02g !'u' $vacceso
set skip of bar 3  of pop_02g !'v' $vacceso
set skip of bar 4  of pop_02g !'w' $vacceso
set skip of bar 5  of pop_02g !'x' $vacceso
set skip of bar 6  of pop_02g !'y' $vacceso
set skip of bar 7  of pop_02g !'f' $vacceso

*-Utilitarios
set skip of bar 1  of pop_02h !'z' $vacceso
set skip of bar 2  of pop_02h !'1' $vacceso
set skip of bar 3  of pop_02h !'2' $vacceso
set skip of bar 4  of pop_02h !'3' $vacceso
set skip of bar 6  of pop_02h !'4' $vacceso
set skip of bar 7  of pop_02h !'5' $vacceso

*-
*set skip of bar 1  of pop_02ha !'6' $vacceso
*set skip of bar 2  of pop_02ha !'7' $vacceso
*set skip of bar 3  of pop_02ha !'8' $vacceso
*set skip of bar 4  of pop_02ha !'9' $vacceso
*set skip of bar 5  of pop_02ha !'0' $vacceso
*set skip of bar 6  of pop_02ha !'!' $vacceso

*-Consultas
set skip of bar 1  of pop_02j !'«' $vacceso
set skip of bar 2  of pop_02j !'¬' $vacceso
set skip of bar 3  of pop_02j !'@' $vacceso
set skip of bar 4  of pop_02j !'#' $vacceso
set skip of bar 5  of pop_02j !'$' $vacceso
set skip of bar 6  of pop_02j !'%' $vacceso
set skip of bar 7  of pop_02j !'^' $vacceso
set skip of bar 8  of pop_02j !'&' $vacceso
set skip of bar 9  of pop_02j !'*' $vacceso
set skip of bar 10 of pop_02j !'(' $vacceso
set skip of bar 11 of pop_02j !')' $vacceso
set skip of bar 12 of pop_02j !'-' $vacceso

set skip of bar 14 of pop_02j !'+' $vacceso
set skip of bar 15 of pop_02j !'=' $vacceso
set skip of bar 16 of pop_02j !'_' $vacceso
set skip of bar 17 of pop_02j !'/' $vacceso
set skip of bar 18 of pop_02j !'›' $vacceso

* No acceso a programacion
*vopcion = .t.
*for i = 1 to 3
*   if !skpbar('pop_02a',i)
*      vopcion = .f.
*      exit
*   endif
*endfor
*set skip of bar 1  of menu1 vopcion

* No acceso a obtenciones
vopcion = .t.
for i = 1 to 7
*for i = 1 to 12
	if !skpbar('pop_02b',i)
		vopcion = .f.
		exit
	endif
endfor
set skip of bar 1  of menu1 vopcion

* No acceso a almacen
*vopcion = .t.
*for i = 1 to 3
*   if !skpbar('pop_02c',i)
*      vopcion = .f.
*      exit
*   endif
*endfor
*set skip of bar 3 of menu1 vopcion

* No acceso a polizas
vopcion = .t.
for i = 1 to 2
*for i = 1 to 4
   if !skpbar('pop_02cc',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 8 of Menu1 vopcion

* No acceso a servicios
vopcion = .t.
for i = 1 to 7
*for i = 1 to 11
   if !skpbar('pop_02d',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 2 of menu1 vopcion
*
** No acceso a Kardex
*vopcion = .t.
*for i = 1 to 4
*   if !skpbar('pop_02e',i)
*      vopcion = .f.
*      exit
*   endif
*endfor
*set skip of bar 5 of menu1 vopcion
*
** No acceso a Transportes
*vopcion = .t.
*for i = 1 to 2
*   if !skpbar('pop_02i',i)
*      vopcion = .f.
*      exit
*   endif
*endfor
*set skip of bar 6 of menu1 vopcion
*
* No acceso a Catalogos
vopcion = .t.
for i = 1 to 3
*for i = 1 to 8
   if !skpbar('pop_02f',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 3 of menu1 vopcion

* No acceso a tablas
vopcion = .t.
for i = 1 to 6
   if !skpbar('pop_02g',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 4 of menu1 vopcion

* No acceso a Utilitarios
vopcion = .t.
for i = 1 to 7
   if !skpbar('pop_02h',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 5 of menu1 vopcion

** No acceso a Correlativos
*vopcion = .t.
*for i = 1 to 4
*   if !skpbar('pop_02ha',i)
*      vopcion = .f.
*      exit
*   endif
*endfor
*set skip of bar 4 of pop_02h vopcion
*
* No acceso a Consultas
vopcion = .t.
for i = 1 to 17
   if !skpbar('pop_02j',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 6 of menu1 vopcion

return


function lastday
*---------------
parameters zmes,zano
*- Devuelve una Fecha que corresponde al £ltimo dia del mes y a¤o
if parameters()=1
   zano = year(date())-1900
endif
vdia = '31'
vmes = padl(alltrim(str(zmes)),2,'0')
if !between(vmes,'01','12')
   return ctod('')
endif
vano = alltrim(str(zano))
vcentury = set("CENTURY")
if !between(vano,'00','99')
   set century on
endif
vret = ctod(vdia+'/'+vmes+'/'+vano)
do while empty(vret)
   vdia = str(val(vdia)-1,2)
   vret = ctod(vdia+'/'+vmes+'/'+vano)
enddo
set century on
return vret
*: EOF: ABAS.PRG

PROCEDURE lColores
*-----------------
SET COLOR OF SCHEME  1  TO W/N, N+/W, W+/N, W+/N, W/N, U+/N, W+/N, - , W+/N, W/N
 SET COLOR OF SCHEME 10  TO  G/N, GR+/B, G/N, G/N, GR+/N, W+/BG, BG+/B, -, W+/N, W/N
SET COLOR OF SCHEME 21  TO W/N, N+/W, W+/N, W+/N, W/N, U+/N, W+/N, -, W+/N, W/N

* 1 para fondo
* 2 para campos read
* 3 para bordes
* 4 titulos
* 5 titulos Desactivados

* 1 Otros registros
* 2 Campo actual
* 3 Borde
* 4 Color del t¡tulo cuando la ventana Examinar est  activa
* 5 Color del t¡tulo cuando hay otra ventana activa
* 6 Texto seleccionado
* 7 Registro actual
* 8 Sombra

RETURN
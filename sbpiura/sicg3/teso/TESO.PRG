********************************************************************
*:
*: Procedure file: G:\BENE2000\TESOPRG\TESO.PRG
*:
*:         System: MODULO DE TESORERIA
*:         Author: JACL
*:      Copyright (c) 1910, SOCIEDAD DE BENEFICENCIA
*:  Last modified: 10/01/100      9:58
*:
*:      Documented 12/01/10 at 21:07               FoxDoc  version 2.10f
*:*****************************************************************************
* ÖÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ·
* º  Programa SubPrincipal : TESO.Prg                                   º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
clear
close all
clear all
worker = iif( ltrim(upper(getenv('WORKER'))) = 'ON', .t.,.f. )
set procedure to tes_lib
do fox_ambi

DO lColores

public vcoddep,vnumdep,desuse,vuser_id,vconex,vmaq,vllav,vusurec,vflag,vFlag2,sistema,SistCtrl
sistema='3'
SistCtrl = sistema
vconex = alltrim(left(sys(0),8))
vmaq   = alltrim(right(sys(0),2))
vllav  = alltrim(sys(2))

*--------------------------
* RUTAS
*--------------------------

IF SelPer()
	*
ELSE
	RETURN
ENDIF

*ruta ='..\DATA'
*ruta ='F:\Bene2005\DATA'
*repos='\bene2000\REPOS'
rutapr='..\DATApr'
*rutapr='F:\BENE2005\DATApr'
repospr='\bene2000\REPOS'
*
*set path to (Ruta)
desa = "     SISTEMAS"
if worker
	if yesno("Datos de Prueba?")
		set path to (RutaPr)
		desa = "P r u e b a "
	endif
endif

if lastkey()#27
	**-- Definiciones para el Men£ --**
	*---------------------------------*
	
	cia  = " SB-PIURA"
	subc = "AREA  DE  "
*	desa = IIF(!Worker,"     SISTEMAS","P r u e b a ")
	titu = " Sistema de Tesorer¡a "
	desuse= "OFICINA DE TESORERIA"
	use usuario in 0 order usuario1 alias usuario
	sele usuario
	seek alltrim(substr(sys(0),1,10))
	
	IF FOUND()
		SPAC1  = LEN(ALLT(usuario.nombre))
		DESUSE = subst(Usuario.Nombre,1,spac1)
	 ELSE
		DESUSE =ALLTRIM(SUBSTR(SYS(0),1,10))
	ENDIF
	
	vcoddep  = coddep
	vnumdep  = numdep
	vflag    = flag
	use
	*-- Colores
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
	@ 1,0,23,79  box "°°°°°°°°°" color scheme c_fondo
	@ 1,1,23,24  box "±±±±±±±±±" color scheme c_fondo
	
	spac = (80 -(len(cia)+len(titu)+10))/2
	rotulo1 = cia + space(spac) + titu + spac(spac) + space(11)
	
	spac = (79 - (len(desa)+len(desuse)+10))/2
	rotulo2 = desa + space(spac)+'® '+desuse+' ¯'+spac(spac)+dtoc(date())
	
	do logos with rotulo1,rotulo2,50
	
*	@ 02,51 SAY " SISTEMA DE TESORERIA 1,999 " COLOR SCHEME 5
	
	save screen to principal
	
*	ERROR TRAPPING SYSTEM
	
	if worker
		on error
	 else
		on error do fox_errs with program()
	endif
	
	save screen to pantalla
	
	*-- Clave de acceso
	
	vuser_id = space(5)
	vuser_id = alltrim(left(sys(0),15))
	if !file( SET("PATH")+'\USUARIO.CDX')
		do standby with 'Error en INDEXA'
	endif
	
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
	vflag2   = flag
	use

    =SAVCON(1)

****
	use iteusu order tag iteusu1   alias iteu
	sele iteu
	set filt to sistema='3'
	vusucla = chrtran(vuser_id,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
							   'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~')
	vacceso = ''
	seek vusucla
	scan while vusucla = allt(iteu.usucla)
		vacceso = vacceso + allt(iteu.modulo)
	endscan
	set filt to
	use
	desa    = desa + vuser_id
	spac = 80 - len(desa) - 10
	rotulo2 = desa + space(spac) + dtoc(date()) + space(2)
	
	set clock on
	store .t. to ven_sistema
	store .f. to esc_tecla
	= sys(2002,1)        && Set cursor on
	*--- El men£ principal ---**
	
*	l_col  ="W+/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "
	L_COL =" W+/BG, N/W, N/W, GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "

*	ON KEY LABEL F1 ACTIVATE WINDOW calculator
*	ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*	º        Tesorer¡a          º
*	ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
	if escolor
		define popup menu from 5,2 to 15,20 shadow color &l_col
	 else
		define popup menu from 5,2 to 15,20 color scheme c_popup
	endif
	define bar  1  of  menu prompt ' \<Egresos        ' message ''
	define bar  2  of  menu prompt ' \<Ingresos       ' message ''
	define bar  3  of  menu prompt '\-' 				 message ''
	define bar  4  of  menu prompt ' Caja/\<Banco     ' message ''
	define bar  5  of  menu prompt ' \<Programaci¢n   ' message ''
	define bar  6  of  menu prompt ' \<Tablas         ' message ''
	define bar  7  of  menu prompt ' \<Cajero/Pagador ' message ''
	define bar  8  of  menu prompt ' \<Utilitarios    ' message ''
	define bar  9  of  menu prompt ' \<Salida          '
	
	
   
   *- Egresos
   if escolor
      define popup pop_01 from 06,26 shadow color &l_col
   else
      define popup pop_01 from 06,26 color scheme c_popup
   endif
   define bar 1  of  pop_01 prompt '\<Asigna Cta.Cte a H/C'		message 'Asigna cuenta corriente por Hoja de Control'
   *DEFINE BAR 2  OF  pop_01 PROMPT 'Asigna Cta.Cte a H/\<M'		MESSAGE 'Asigna cuenta corriente por Hoja de Modificaci¢n'
   *DEFINE BAR 3  OF  pop_01 PROMPT '\-'
   define bar 2  of  pop_01 prompt '\<Comprobante de Pago' 		message 'Ingreso de Comprobantes de Pago'
   define bar 3  of  pop_01 prompt '\<Hoja de Anulaci¢n de Cheque' message 'Ingreso de Partes de Anulaci¢n de C/P'
   define bar 4  of  pop_01 prompt '\-'
   **DEFINE BAR 6  OF  pop_01 PROMPT 'Hoja de Cargas \<Diversas' 	MESSAGE 'Registra las regularizaciones de C/P'
   *DEFINE BAR 6  OF  pop_01 PROMPT 'Actualiza \<Nueva H/C en C/P' 	MESSAGE 'Actualiza hojas de Control en C/P'
   *DEFINE BAR 7  OF  pop_01 PROMPT '\-'
   define bar 5  of  pop_01 prompt 'Cuadro de \<Retenciones  '		message 'Reporte de Retenciones'
   *DEFINE BAR 9  OF  pop_01 PROMPT 'Retenciones de \<Sunat   '		MESSAGE 'Retenciones para la Sunat'
   
   *- Ingresos
   if escolor
      define popup pop_02 from 06,26 shadow color &l_col
   else
      define popup pop_02 from 06,26 color scheme c_popup
   endif
   define bar 1  of  pop_02 prompt 'Recibos de \<Ingresos    ' message 'Registro de Recibos de ingreso'
   define bar 2  of  pop_02 prompt '\<Reversiones T - 6      ' message 'Reporte de T - 6'
   define bar 3  of  pop_02 prompt '\<Rebajas de Ingresos    ' message 'Reporte de Reversiones'
   define bar 4  of  pop_02 prompt '\<Facturas               ' message 'Registro de Facturas'
   *DEFINE BAR 4  OF  pop_02 PROMPT '\<Memo Resultado de Bases' MESSAGE 'Registro de Memorandum de Resultado de Bases'
   *DEFINE BAR 5  OF  pop_02 PROMPT '\-'
   *DEFINE BAR 6  OF  pop_02 PROMPT '\<Captaci¢n de Ingresos ' MESSAGE 'Reporte de Captaci¢n de Ingresos'
   *DEFINE BAR 7  OF  pop_02 PROMPT 'Captaci¢n por Fte.Fto.   ' MESSAGE 'Reporte de Ingreso por Fte.Fto.'
   *DEFINE BAR 8  OF  pop_02 PROMPT '\-'
   *DEFINE BAR 9  OF  pop_02 PROMPT '\<Solicitud de Chequeras ' MESSAGE 'Registra las solicitudes de chequeras'
   *DEFINE BAR 10 OF  pop_02 PROMPT '\<Control de Chequeras'    MESSAGE 'Control de Entrega de chequeras'
   *DEFINE BAR 11 OF  pop_02 PROMPT 'Cierre \<de Chequera'      MESSAGE 'Proceso de Cierre de Chequera'
   *DEFINE BAR 12 OF  pop_02 PROMPT '\-'
   *DEFINE BAR 13 OF  pop_02 PROMPT 'Cartas \<Fianzas         ' MESSAGE 'Registro y control de Documentos valorados'
   
   
   *- Autoriz. Giro
   if escolor
      define popup pop_03 from 07,26 shadow color &l_col
   else
      define popup pop_03 from 07,26 color scheme c_popup
   endif
   define bar  1  of  pop_03 prompt '\<Recepci¢n de Autorizaci¢n de giro '   message 'Registro Autorizaciones de Giro del MEF Lima'
   *DEFINE BAR  2  OF  pop_03 PROMPT '\<Distribuci¢n de Autorizaci¢n de giro' MESSAGE 'Registro Autorizaciones de Giro a cada Oficina Giradora'
   *DEFINE BAR  3  OF  pop_03 PROMPT 'Carta Orden de \<Transferencias ' 	  MESSAGE 'Emite Cartas Ordenes de Transferencias al Banco'
   *DEFINE BAR  4  OF  pop_03 PROMPT 'Reversi¢n de Saldos No \<Utilizados '   MESSAGE 'Emite Autorizaciones por CtaCte y para el Banco'
   *DEFINE BAR  5  OF  pop_03 PROMPT '\-'
   *DEFINE BAR  6  OF  pop_03 PROMPT 'Transferencia de \<Fondos -Mismo Banco'	 		  MESSAGE 'Oficio de Transferencia de fondos al banco'
   *DEFINE BAR  7  OF  pop_03 PROMPT 'Cheques de \<Gerencia -Otros Bancos'			      MESSAGE 'Oficio de Cheques de Gerencia al Banco'
   *DEFINE BAR  8  OF  pop_03 PROMPT 'Oficio de Autori\<Zaci¢n Simple'			          MESSAGE 'Oficio simple de autorizaci¢n a las oficinas giradoras'
   *DEFINE BAR  9  OF  pop_03 PROMPT 'Autorizaci¢n de Giro -\<Ing.Propios ' 			  MESSAGE 'Emisi¢n de Autorizaciones de giro ingresos propios'
   *DEFINE BAR  10 OF  pop_03 PROMPT '\-'
   *DEFINE BAR  11 OF  pop_03 PROMPT 'Reporte Anexo \<1 GR           ' 		  MESSAGE 'Reporte Regional Anexo 1gr'
   *DEFINE BAR  12 OF  pop_03 PROMPT '\<Ejecuci¢n Presupuestal   ' 			  MESSAGE 'Reporte de la Ejecuci¢n Presupuestal'
   define bar  2 of  pop_03 prompt 'Reporte \<AF-9                  ' 	  message 'Muestra el Movimiento de Fondos Por Cta.Cte.'
   define bar  3 of  pop_03 prompt 'Reporte \<Montos Pagados                  ' 	  message 'Muestra el Movimiento de Fondos Por Cta.Cte.'
   
   *- Caja/Banco
   if escolor
      define popup pop_04 from 09,26 shadow color &l_col
   else
      define popup pop_04 from 09,26 color scheme c_popup
   endif
   define bar 1  of  pop_04 prompt 'Movimientos de \<Banco      ' message 'Registro de N/C,N/A,B/D,R/I,etc'
   define bar 2  of  pop_04 prompt '\<Entrega de Cheques ' 	   message 'Entrega de Cheques a los Proveedores'
   define bar 3  of  pop_04 prompt '\-' 					       message ''
   define bar 4  of  pop_04 prompt '\<Extractos Bancarios '       message 'Registro de los Extractos Bancarios enviados por los Bancos'
   *DEFINE BAR 3  OF  pop_04 PROMPT 'c\<Heques Certificados'      MESSAGE 'Registro de cheques certificados pagados por los Bancos'
   define bar 5  of  pop_04 prompt '\-' 					       message ''
   define bar 6  of  pop_04 prompt '\<Libro Banco               ' message 'Reporte Libro Banco a nivel de CtaCte Central Piura'
   define bar 7  of  pop_04 prompt '\<Conciliaci¢n Bancaria     ' message 'Reporte de Conciliaci¢n'
   define bar 8  of  pop_04 prompt '\<Listado de Cheques        ' message 'Reporte de los cheques seg£n su estado'
   *DEFINE BAR 6  OF  pop_04 PROMPT '\<T4 Reporte de conciliaci¢n' MESSAGE 'Reporte Formato T-4 y Conciliaci¢n del Moviemiento Acumulado'
   *DEFINE BAR 7  OF  pop_04 PROMPT '\<Rebaja conciliaci¢n       ' MESSAGE 'Rebaja manual de conciliaci¢n'
   *DEFINE BAR 8  OF  pop_04 PROMPT 'Tipo de \<Gasto             ' MESSAGE 'Resumen del Tipo de Gasto'
   
   *-Programaci¢n
   if escolor
      define popup pop_05 from 10,26 shadow color &l_col
   else
      define popup pop_05 from 10,26 color scheme c_popup
   endif
   define bar 1  of  pop_05 prompt '\<Hojas de Trabajo  ' message 'Reporta Hoja de Trabajo Detallada'
   *DEFINE BAR 2  OF  pop_05 PROMPT 'Hojas de Trabajo \<SECTORES' MESSAGE 'Reporta Hoja de Trabajo Detallada'
   *DEFINE BAR 3  OF  pop_05 PROMPT 'h\<Oja Trabajo Resumida    ' MESSAGE 'Reporta Hoja de Trabajo Resumida '
   *DEFINE BAR 4  OF  pop_05 PROMPT '\<Reporte T2               ' MESSAGE 'Reporte Informe mensual del gasto T2'
   *DEFINE BAR 2  OF  pop_05 PROMPT '\<Saldos Ejecuci¢n x CtaCte' MESSAGE 'Reporte Informe mensual del gasto T2'
   *DEFINE BAR 3  OF  pop_05 PROMPT '\<Compromisos Pendientes    ' MESSAGE 'Reporte Informe Acumulado del gasto T2'
   *DEFINE BAR 4  OF  pop_05 PROMPT '\<Hojas de Control          ' MESSAGE 'Reporte Informe Acumulado del gasto T2'
   *DEFINE BAR 5  OF  pop_05 PROMPT '\<Conciliaci¢n de Cuentas   ' MESSAGE 'Reporte del Fondo garantia '
   *DEFINE BAR 5  OF  pop_05 PROMPT '\<Reporte T2               ' MESSAGE 'Reporte Informe mensual del gasto T2'
   
   *- Tablas
   if escolor
      define popup pop_06 from 05,26 color &l_col    &&SHADOW
   else
      define popup pop_06 from 05,26 color scheme c_popup
   endif
   define bar 1  of pop_06 prompt '\<Bancos'
   define bar 2  of pop_06 prompt '\<Cuentas Corrientes y Ahorros'
   *DEFINE BAR 3  OF pop_06 PROMPT 'Concepto \<Movimiento de Bancos'
   define bar 3  of pop_06 prompt '\<Tipos de Recibos de Ingreso'
   *DEFINE BAR 5  OF pop_06 PROMPT 'Tipos de Re\<versiones'
   define bar 4  of pop_06 prompt '\<Rubros de Retenciones'
   *DEFINE BAR 7  OF pop_06 PROMPT 'Clasificador por Objeto del \<Gasto'
   define bar 5  of pop_06 prompt 'Clasificador de \<Ingresos'
   *DEFINE BAR 9  OF pop_06 PROMPT '\<Obras Auxiliares'
   *DEFINE BAR 12 OF pop_06 PROMPT '\<Documentos '
   define bar 6 of pop_06 prompt 'Au\<xiliar OTros'
   *DEFINE BAR 14 OF pop_06 PROMPT 'M¢dulos del \<Sistema'
   
   *- Cajero/Pagador
   if escolor
      define popup pop_07 from 11,26 margin  shadow color &l_col
   else
      define popup pop_07 from 11,26 margin  color scheme c_popup
   endif
   
   *DEFINE BAR 1 OF  pop_07 PROMPT '\<Entrega de Cheques  ' MESSAGE 'Entrega de Cheques a Acreedores'
   *DEFINE BAR 1 OF  pop_07 PROMPT '\<Listado de Cheques  ' MESSAGE 'Entrega de Cheques a Acreedores'
   define bar 1 of  pop_07 prompt '\<Caja Chica            '
   *DEFINE BAR 3 OF  POP_07 PROMPT 'Rendici¢n de \<Fondos    ' MESSAGE 'Registra rendici¢n de fondos'
   *DEFINE BAR 4 OF  POP_07 PROMPT 'Ingreso de Facturas(COA) ' MESSAGE 'Registra facturas para la SUNAT'
   
   *- Informaci¢n Gerencial
   if escolor
      define popup pop_08 from 04,26 margin  shadow color &l_col
   else
      define popup pop_08 from 04,26 margin  color scheme c_popup
   endif
   
   define bar 1  of  pop_08 prompt 'Autorizaci¢n de Che\<ques'		       message 'Autorizaci¢n Director(a) Tesorer¡a'
   define bar 2  of  pop_08 prompt '\<Seguimiento de Cheques '		       message 'Consulta de Cheques.. Seguimiento'
   define bar 3  of  pop_08 prompt 'Seguimiento \<Doc. Fuente'		       message 'Res£men Completo de Documentos Fuente'
   define bar 4  of  pop_08 prompt '\<Reporte de Cheques'			       message 'Reporte de Cheques ..Estado Situacional'
   define bar 5  of  pop_08 prompt '\-'
   define bar 6  of  pop_08 prompt 'Saldos p\<or Cuenta Bancos'		   message 'Consulta de Saldos de Bancos'
   *DEFINE BAR 7  OF  POP_08 PROMPT 'Saldos Aut.de \<Giro'                 MESSAGE 'Consulta saldos de Autorizaci¢n de Giro'
   *DEFINE BAR 8  OF  POP_08 PROMPT 'Saldos por \<Banco'                   MESSAGE 'Consulta saldos de Autorizaci¢n de Giro'
   define bar 7  of  pop_08 prompt 'Captaci¢n de \<Ingresos' 			   message 'Consulta captaci¢n de Ingresos por mes'
   define bar 8 of  pop_08 prompt '\-'
   define bar 9 of  pop_08 prompt 'De \<Presupuesto       '	           message ''
   define bar 10 of  pop_08 prompt 'De \<Abastecimiento    '             message ''
   define bar 11 of  pop_08 prompt 'De \<Tesorer¡a         '             message ''
   define bar 12 of  pop_08 prompt 'De \<Cont. Presup.     '             message ''
   define bar 13 of  pop_08 prompt '\-'
   define bar 16 of  pop_08 prompt 'Seguimiento a Usuario'				   message 'Determina el usuario que ha registrado el Doc. Fte'
   
   *- Utilitarios
   if escolor
      define popup pop_09 from 05,26 margin  shadow color &l_col
   else
      define popup pop_09 from 05,26 margin  color scheme c_popup
   endif
   
   define bar  1 of pop_09  prompt '\<Indexar archivos             ' message 'Indexa uno o todos los archivos del Sistema'
   define bar  2 of pop_09  prompt '\<Backup de los archivos       ' message 'Genera un archivo de respaldo'
   define bar  3 of pop_09  prompt 'De\<puraci¢n de backups        ' message 'Elimina los archivos de respaldo'
   define bar  4 of pop_09  prompt 'Man\<Tenimiento Correlativos    '
   define bar  5 of pop_09  prompt 'Mantenimiento Pa\<R metros    '
   define bar  6 of pop_09  prompt 'impresi¢n de \<Archivos        '
   define bar  7 of pop_09  prompt '\-'
   define bar  8 of pop_09  prompt '\<Seguridad         ' 				message '®® Acceso RESTRINGIDO ¯¯'
   define bar  9 of pop_09  prompt '\<Seguridad Opciones   ' 			message '®® Acceso RESTRINGIDO ¯¯'
   define bar 10 of pop_09  prompt 'Control de Mensajes y \<Usuarios'
   define bar 11 of pop_09  prompt '\-'
   define bar 12 of pop_09  prompt 'Ir al D\<OS         ' message '®® Escriba EXIT para regresar ¯¯'
   *DEFINE BAR 12 OF Pop_09  PROMPT 'Ir al \<Word Perfect' MESSAGE '®® Escriba EXIT para regresar ¯¯'
   *DEFINE BAR 13 OF Pop_09  PROMPT 'Ir al \<QUATTRO PRO'  MESSAGE '®® Escriba EXIT para regresar ¯¯'
   *DEFINE BAR  7 OF Pop_09  PROMPT '\<Conectarse a la Print-Server  '
   *DEFINE BAR  8 OF Pop_09  PROMPT '\<Desconectarse de la Print-Server '
   
   *- De presupuesto
   if escolor
      define popup pop_11 from 13,56 shadow color &l_col
   else
      define popup pop_11 from 13,56 color scheme c_popup
   endif
   define bar 1  of  pop_11 prompt '\<Saldos calendario ' message ''
   define bar 2  of  pop_11 prompt 'Saldos del \<Marco  ' message ''
   
   
   *-- Documentos de Abastecimiento
   if escolor
      define popup pop_14 from 11,56  shadow color &l_col
   else
      define popup pop_14 from 11,56  color scheme c_popup
   endif
   
   define bar  01  of  pop_14 prompt '\<Pecosa                '         message ''
   define bar  02  of  pop_14 prompt 'Solicitud de \<Servicio '         message ''
   define bar  03  of  pop_14 prompt 'Orden de \<Compra       '         message ''
   define bar  04  of  pop_14 prompt '\<Orden de Servicio     '         message ''
   define bar  05  of  pop_14 prompt 'P¢liza de En\<Trada     '         message ''
   define bar  06  of  pop_14 prompt 'P¢liza de Sa\<Lida      '         message ''
   define bar  07  of  pop_14 prompt 'Parte de A\<Nulaci¢n    '         message ''
   define bar  08  of  pop_14 prompt 'Parte de \<Rebaja       '         message ''
   *-
   
   *-- Documentos de Tesorer¡a
   if escolor
      define popup pop_15 from 12,56  shadow color &l_col
   else
      define popup pop_15 from 12,56  color scheme c_popup
   endif
   
   define bar  01  of  pop_15 prompt '\<Comprobante de Pago   '         message ''
   define bar  02  of  pop_15 prompt '\<Recibo de Ingreso     '         message ''
   define bar  03  of  pop_15 prompt '\<Movimientos Bancarios '         message ''
   define bar  04  of  pop_15 prompt '\<Extractos Bancarios   '         message ''
   define bar  05  of  pop_15 prompt '\<Fondos de Caja Chica  '         message ''
   *DEFINE BAR  06  OF  pop_15 PROMPT '\<Distrib. Aut. Giro    '         MESSAGE ''
   *-
   
   *- Documentos de C.presupuestal
   if escolor
      define popup pop_16 from 13,56  shadow color &l_col
   else
      define popup pop_16 from 13,56  color scheme c_popup
   endif
   
   define bar  01  of  pop_16 prompt '\<Hoja de Control     '         message ''
   define bar  02  of  pop_16 prompt 'Saldo de \<Calendario '         message ''
   define bar  03  of  pop_16 prompt '\<Reporte de H/C      '         message ''
   
   
   if escolor
      define popup pop_16a from 11,56 shadow color &l_col
   else
      define popup pop_16a from 11,56 color scheme c_popup
   endif
   define bar 1  of  pop_16a prompt '\<Acumulados  ' message ''
   *DEFINE BAR 2  OF  pop_16a PROMPT '\<Mensualizado' MESSAGE ''
   
   on selection popup menu do act_menus
   
	if !vFlag$'J*'
		DO ABREPAS
	endif
*	if !vuser_id$'LCDJOSEJFGTRECARTE'
*		DO ABREPAS
*	endif
   **- La sombra del principal
   save screen to principal
   activate popup menu
   do while  empty(prompt())
      activate popup menu
   enddo
   *-Cerrar todo...
   on key
endif
=borcon()
return

*-- Fin  del Programa Principal


procedure act_menus
*------------------
do case
case bar() = 1
	on selection popup pop_01  do  menu_01
	activate popup pop_01
case bar() = 2
	on selection popup pop_02  do  menu_02
	activate popup pop_02
	*   CASE BAR() = 3
	*        ON SELECTION POPUP pop_03  DO  menu_03   && Autoriz. Giro
	*        ACTIVATE POPUP pop_03
case bar() = 4
	on selection popup pop_04  do  menu_04
	activate popup pop_04
case bar() = 5
	on selection popup pop_05  do  menu_05
	activate popup pop_05
case bar() = 6
	on selection popup pop_06  do  menu_06
	activate popup pop_06
case bar() = 7
	on selection popup pop_07  do  menu_07
	activate popup pop_07
	*   CASE BAR() = 8
	*        ON SELECTION POPUP pop_08  DO  menu_08
	*        ACTIVATE POPUP pop_08
case bar() = 8
	on selection popup pop_09  do  menu_09
	activate popup pop_09
case bar() = 9
	do salmenu
endcase

restore screen from principal
return


procedure menu_01   && Egresos
*----------------
choice=bar()
do case
case choice = 1    && Asigna Cta.Cte H/C
   do asigcchc
   *    CASE choice = 2    && Asigna Cta.Cte H/M
   *       DO AsigCCHM
case choice = 2    && Registro C/P
   do regcp1 with "1"
case choice = 3    && Registro Parte de Anulaci¢n
   do regha1 with sistema
   **   CASE choice = 6    && Hoja de Cargas Diversas
   **      DO RegCD
   *    CASE choice = 6    && Actualiza H/C en C/P
   *       DO act_cphc
case choice = 5     && Listado Retenciones
   do cuaret with sistema
   *    case choice = 9		&&& para la oficina de personal
   *    	do retper
endcase

if !escolor
   *RESTORE SCREEN FROM principal
   on selection popup pop_01  do menu_01
endif
restore screen from principal
show popup pop_01,menu
hide window all
return

PROCEDURE menu_02    && Ingresos
*----------------
private choice
choice=bar()
do case
case choice = 1     && Recibos de ingresos
	do regri with sistema
case choice = 2     && Reversiones
	do regrev
case choice = 3
	do regrevx		&& ReBAJAS comunes y corrientes para que no jodan en contabilidad
	
*	OJO NO BORRAR LA SGTE LINEA POR QUE SON REVERSIONES DEL T6
*	do regrev

case choice = 4     && Registro de facturas
	do regfac
	*    CASE choice = 4     && Memo Resultado de Bases
	*       DO MembasLi
	*    CASE choice = 6     &&  Fto E6
	*       DO HWCAPING
	*    CASE choice = 7     &&  Fto T5
	**       DO ftoT5
	*		DO RECIFTE
	*   CASE choice =  9    &&  Solicitud de Chequeras
	*       DO Ofichq
	*   CASE choice =  10   &&  Recibos de ingresos
	*       DO Contchq
	*   CASE choice =  11   &&  Recibos de ingresos
	*      DO Ciechq
	*    CASE choice = 13    && Control de Cartas Fianza
	*      DO CarFia
endcase

if !escolor
   *RESTORE SCREEN FROM principal
   on selection popup pop_02  do menu_02
endif
restore screen from principal
show popup pop_02,menu
hide window all
return

*ROCEDURE menu_02a   && Programaci¢n
*----------------
*PRIVATE choice
*  choice=BAR()
*  DO CASE
*    CASE choice = 1    && Listado de Hojas de Trabajo ingresos propios
*	  DO reGRI1
*    CASE choice = 2    && Res£men de Hojas de Trabajo
*      DO reGRI97
*  ENDCASE
*  IF !escolor
*   *RESTORE SCREEN FROM principal
*    ON SELECTION POPUP pop_02a  DO menu_02a
*  ENDIF
*  RESTORE SCREEN FROM PRINCIPAL
* SHOW POPUP pop_02,pop_02a,menu
*  HIDE WINDOW ALL
*  RETURN


*!*****************************************************************************
*!
*!      Procedure: MENU_03
*!
*!      Called by: MENU_03            (procedure in TESO.PRG)
*!
*!          Calls: BAR()              (function  in ?)
*!               : AUTGMEF.PRG
*!               : CCAF-9.PRG
*!               : MONPAG.PRG
*!               : MENU_03            (procedure in TESO.PRG)
*!
*!*****************************************************************************
procedure menu_03   && Integraci¢n
*----------------
choice=bar()
do case
case choice = 1    && Autorizaciones de giro Lima (MEF) - TRN
   do autgmef
   *		  DO PILI
   *   CASE choice = 2    && Autorizaciones de giro Regi¢n Grau  - TRN
   *DO AutGTRN
   *   CASE choice = 3    && Cartas Orden de Transferecias  -TRN
   *DO CarOrd
   *   CASE choice = 4    && Reversiones de Saldos No Utilizados -TRN
   *DO RevSnu
   *  CASE choice = 6    && Transferencia de Fondos -PRP
   *DO TransFon
   *  CASE choice = 7    && Cheques de Gerencia -PRP
   *DO ChqGer
   *  CASE choice = 8    && Autorizaciones Simples -PRP
   *DO AutSim
   *  CASE choice = 9   && Autorizaci¢n de Giro PRP
   *DO AutgPRP
   *  CASE choice = 11  && Reportes Anexo 1GR
   *DO Anex1gr
   *  CASE choice = 12  && Ejecuci¢n Presupuestal
   *DO ejepre
case choice = 2  && AF-9
   *        DO AF-9
   do ccaf-9
case choice = 3  && AF-9
   *        DO AF-9
   do monpag
endcase

if !escolor
   *RESTORE SCREEN FROM principal
   on selection popup pop_03  do menu_03
endif
restore screen from principal
show popup pop_03,menu
hide window all
return

procedure menu_04   && Caja/Banco
*----------------
private choice

choice=bar()
do case
case choice = 1    && Movimientos de banco
   do regmb
case choice = 2    && Entrega de Cheques
   do entchq
case choice = 4    && Ingreso Extracto de Cta Cte Bco
   do extbco
   *    CASE choice = 3    && Ingreso de cheques certificados
   *         DO Chqctf
case choice = 6    && Libro Bancos
   do libban3
   *         DO LibBan2
case choice = 7    && Conciliaci¢n
   do conban
   *   CASE choice = 6    && Reporte T4
   *         DO T4
   *    CASE choice = 7    && Rebaja manual de conciliaci¢n
   *         DO rebcon
   *   CASE choice = 8    && Consulta estado de conciliaci¢n
   *        DO tipgas
case choice = 8    && Listado de los Cheques seg£n su estado
   do repchq
   
endcase
if !escolor
   *RESTORE SCREEN FROM principal
   on selection popup pop_04  do menu_04
endif
restore screen from principal
show popup pop_04,menu
hide window all
return


*!*****************************************************************************
*!
*!      Procedure: MENU_05
*!
*!      Called by: ACT_MENUS          (procedure in TESO.PRG)
*!               : MENU_05            (procedure in TESO.PRG)
*!
*!          Calls: BAR()              (function  in ?)
*!               : LISHOJ.PRG
*!               : MENU_05            (procedure in TESO.PRG)
*!
*!*****************************************************************************
procedure menu_05   && Programaci¢n
*----------------
private choice
choice=bar()
do case
case choice = 1    && Listado de Hojas de Trabajo SEDE
   do lishoj with 1
   *    CASE choice = 2    && Listado de Hojas de Trabajo SECTORES
   *	  DO lishoj WITH 2
   *    CASE choice = 2    && Saldos ejecuci¢n
   *      DO ConSal
   *    CASE choice = 3    && Saldos acumulados
   *     DO ConPre
   *	DO COMPEN1
   *   CASE choice = 5    && Saldos acumulados
   *     DO FtoT2
   *    CASE choice = 4    && Hojas de Control
   *   DO rephoj
   *    CASE choice = 5    && Hojas de Control
   *   DO fdogar
   
endcase

if !escolor
   *RESTORE SCREEN FROM principal
   on selection popup pop_05  do menu_05
endif
restore screen from principal
show popup pop_05,menu
hide window all
return


*!*****************************************************************************
*!
*!      Procedure: MENU_06
*!
*!      Called by: ACT_MENUS          (procedure in TESO.PRG)
*!               : MENU_06            (procedure in TESO.PRG)
*!
*!          Calls: BAR()              (function  in ?)
*!               : MANPAR.PRG
*!               : CATCAJ.PRG
*!               : CATTIPRI.PRG
*!               : CATRET.PRG
*!               : INGRESO.PRG
*!               : MANAUX2.PRG
*!               : MENU_06            (procedure in TESO.PRG)
*!
*!*****************************************************************************
procedure menu_06   && Tablas
*----------------
private choice
choice=bar()
do case
case bar()=1  &&Bancos
   do manpar with 'BANCOS','Banco','Bancos'
case bar()=2  &&Cuenta de Bancos
   do catcaj
   *         CASE BAR()=3  &&Movimiento de Bancos
   *            DO CatMov
case bar()=3  &&Recibos de Ingreso
   do cattipri
   *         CASE BAR()=5  && Reversiones
   *            DO ManPar WITH 'CODREV','Reversi¢n','Reversiones'
case bar()=4  && Retenciones
   do catret
   *         CASE BAR()=7  &&Clasificador por Objeto del Gasto
   *            DO Clasific
case bar()=5  && Clasificador de Ingresos
   do ingreso
case bar()=6  && Auxiliar Otros
   do manaux2
   *         CASE BAR()=9  && Oficinas Giradoras del Tesoro
   **           DO ManPar WITH 'OFIGIR','Oficina Giradora','Oficinas Giradoras'
   *			do AUXIOBRA
   *         CASE BAR()=10 && Oficinas Giradoras de Ingresos Propios
   *            DO ManPar WITH 'OFGIIP','Oficina Giradora','Oficinas Giradoras'
   *         CASE BAR()=11 && Concursos de Licitaci¢n
   *            DO ConLic
   *	     CASE BAR()=12 && Documentos
   *            DO ManPar WITH 'DOCCAJ','Tipo de Documento','Tipos de Documentos'
   *         CASE BAR()=14  && M¢dulos del Sistema
   *            DO ManPar WITH 'MODTES','M¢dulo del Sistema','M¢dulos del Sistema'
endcase
if !escolor
   restore screen from principal
   on selection popup pop_06  do menu_06
endif
restore screen from principal
show popup pop_06,menu
hide window all
return


*!*****************************************************************************
*!
*!      Procedure: MENU_07
*!
*!      Called by: ACT_MENUS          (procedure in TESO.PRG)
*!               : MENU_07            (procedure in TESO.PRG)
*!
*!          Calls: BAR()              (function  in ?)
*!               : REGFP.PRG
*!               : MENU_07            (procedure in TESO.PRG)
*!
*!*****************************************************************************
procedure menu_07  && Cajero/Pagador
*----------------
choice=bar()
do case
   *    CASE choice = 1     && Entrega de cheque
   *      DO EntChq
   *    CASE choice = 1     && Reporte de cheque
   *       DO repchqca
case choice = 1    && Rendici¢n de Fondos
   do regfp with sistema
   *    CASE choice = 4    && Ingreso de facturas
   *       DO Factura
   
endcase
if !escolor
   restore screen from principal
   on selection popup pop_07  do menu_07
endif
restore screen from principal
show popup pop_07,menu
hide window all
return


*!*****************************************************************************
*!
*!      Procedure: MENU_08
*!
*!      Called by: MENU_08            (procedure in TESO.PRG)
*!
*!          Calls: BAR()              (function  in ?)
*!               : AUTCHQTE.PRG
*!               : SEGCHQ.PRG
*!               : SEGDOCFT.PRG
*!               : REPCHQ.PRG
*!               : SAL_BCO.PRG
*!               : CAP_ING.PRG
*!               : MENU_11            (procedure in TESO.PRG)
*!               : MENU_14            (procedure in TESO.PRG)
*!               : MENU_15            (procedure in TESO.PRG)
*!               : MENU_16            (procedure in TESO.PRG)
*!               : CONSEGTE.PRG
*!               : MENU_08            (procedure in TESO.PRG)
*!
*!*****************************************************************************
procedure menu_08  && Informaci¢n Gerencial
*----------------
choice=bar()
do case
case choice = 1     && Autoriza Cheque x Tesorer¡a
   do autchqte
case choice = 2     && Consulta de Cheques
   do segchq
case choice = 3     && Listado de Cheques
   do segdocft
case choice = 4     && Listado de Cheques
   do repchq
case choice = 6     && Saldos de Bancos
   do sal_bco
case choice = 7     && Saldos de Autorizaciones
   *    DO sal_aut
case choice = 8     && Estado de Conciliaci¢n
   *     DO sal_cta
case choice = 8     && Captaci¢n de Ingresos
   do cap_ing
case choice = 9    && Inf. de Presupuesto
   on selection popup pop_11  do  menu_11
   activate popup pop_11
case choice = 10    && Inf. de Abastecimientos
   on selection popup pop_14  do  menu_14
   activate popup pop_14
case choice = 11     && Inf. de Tesorer¡a
   on selection popup pop_15  do  menu_15
   activate popup pop_15
case choice = 12     && Inf. de C.presupuestal
   on selection popup pop_16  do  menu_16
   activate popup pop_16
case choice = 16    && Consulta Doc. por Usuario
   do consegte
endcase
if !escolor
   restore screen from principal
   on selection popup pop_08  do menu_08
endif
restore screen from principal
show popup pop_08,menu
hide window all
return


*!*****************************************************************************
*!
*!      Procedure: MENU_09
*!
*!      Called by: ACT_MENUS          (procedure in TESO.PRG)
*!               : MENU_09            (procedure in TESO.PRG)
*!
*!          Calls: BAR()              (function  in ?)
*!               : INDEXATE.PRG
*!               : BACKUP.PRG
*!               : DEPBK.PRG
*!               : MANPAR.PRG
*!               : IMPARC.PRG
*!               : SEGTESO.PRG
*!               : CONTUSU.PRG
*!               : MENU_09            (procedure in TESO.PRG)
*!
*!*****************************************************************************
procedure menu_09  && Utilitarios
*----------------
choice=bar()
do case
case choice = 1    && Reindexado
   do indexate
case choice = 2    && Backup
   do backup
case choice = 3    && Depura Backup
   do depbk
case choice = 4    && Correlativo
   do manpar with "CORREL","Correlativo","Correlativos",4,"Valor siguiente"
case choice = 5    && Par metros
   do manpar with "PARFAC","Valor Actual","Par metros",4,"Valor siguiente"
case bar() =  6
   do imparc
   *    CASE choice = 7
   *         RUN CAPTURE /Q=PRINTQ_0 /TI=0 /No Banner /No Formfeed
   *         RESTORE  SCREEN FROM Principal
   *    CASE choice = 8
   *         RUN ENDCAP
   *         RESTORE  SCREEN FROM Principal
case choice = 8    && Seguridad
	do segteso WITH vUser_Id
case choice = 9    && Seguridad
	do segtsOp WITH vUser_Id
case bar() =  10
		do contusu
case bar() = 12
   restore  screen from principal
   clear
   run foxswap command
   restore  screen from principal
   *    CASE BAR() = 12
   *        RESTORE  SCREEN FROM Principal
   *         CLEAR
   *        RUN FOXSWAP WP
   *         RESTORE  SCREEN FROM Principal
   *    CASE BAR() = 13
   *         RESTORE  SCREEN FROM Principal
   *         CLEAR
   *         RUN FOXSWAP Q
   *         RESTORE  SCREEN FROM Principal
endcase
if !escolor
   restore screen from principal
   on selection popup pop_09  do menu_09
endif
restore screen from principal
show popup pop_09,pop_09,menu
hide window all
return

*!*****************************************************************************
*!
*!      Procedure: MENU_11
*!
*!      Called by: MENU_08            (procedure in TESO.PRG)
*!               : MENU_11            (procedure in TESO.PRG)
*!
*!          Calls: BAR()              (function  in ?)
*!               : SALCAL.PRG
*!               : SALPRE.PRG
*!               : MENU_11            (procedure in TESO.PRG)
*!
*!*****************************************************************************
procedure menu_11  && De Presupuesto
*----------------
choice=bar()
do case
case choice = 1    &&  Indexa
   do salcal with 1
case choice = 2
   do salpre with '02'
endcase
if !escolor
   restore screen from principal
   on selection popup pop_11  do menu_11
endif
restore screen from principal
show popup pop_08,pop_11,menu
hide window all
return

*!*****************************************************************************
*!
*!      Procedure: MENU_14
*!
*!      Called by: MENU_08            (procedure in TESO.PRG)
*!               : MENU_14            (procedure in TESO.PRG)
*!
*!          Calls: BAR()              (function  in ?)
*!               : REGPEC.PRG
*!               : REGSOL.PRG
*!               : ORDCOM.PRG
*!               : ORDSER.PRG
*!               : MENU_14            (procedure in TESO.PRG)
*!
*!*****************************************************************************
procedure menu_14   && Documentos de Abastecimientos
*----------------
choice=bar()
do case
case choice = 1
   do regpec with 2,'F'    && Pecosa
case choice = 2
   do regsol     && Solicitud de servicios
case choice = 3
   *DO ConOc      && Orden de Compra
   do ordcom
case choice = 4
   *DO ConOs      && Orden de Servicio
   do ordser
case choice = 5
   *DO ConPent   && Poliza de Entrada
case choice = 6
   *DO ConPsal   && Poliza de Salida
case choice = 7
   *IF yesno("Partes de Anulaci¢n de O/C:[Si] o de O/S:[No] ? ")
   *   DO ConPan   WITH 1  && Parte anulaci¢n O/C
   *ELSE
   *   DO ConPan   WITH 2  && Parte anulaci¢n O/S
   *ENDIF
case choice = 8
   *IF yesno("Partes de Rebaja de O/C:[Si] o de O/S:[No] ? ")
   * 	DO ConPrb   WITH 1 && Parte rebaja DE o/c
   *ELSE
   *   DO ConPrb   WITH 2 && Parte rebaja DE o/S
   *ENDIF
endcase

if !escolor
   restore screen from principal
   on selection popup pop_14  do menu_14
endif
restore screen from principal
show popup pop_08,pop_14,menu
hide window all
return


*!*****************************************************************************
*!
*!      Procedure: MENU_15
*!
*!      Called by: MENU_08            (procedure in TESO.PRG)
*!               : MENU_15            (procedure in TESO.PRG)
*!
*!          Calls: BAR()              (function  in ?)
*!               : CONCP2.PRG
*!               : CONRI2.PRG
*!               : CONMB2.PRG
*!               : CONEXTB2.PRG
*!               : CONFP.PRG
*!               : MENU_15            (procedure in TESO.PRG)
*!
*!*****************************************************************************
procedure menu_15   && Documentos de Tesorer¡a
*----------------
choice=bar()
do case
case choice = 1      && Comprobante de Pago
   do concp2 with sistema
case choice = 2      && Recibo de Ingreso
   do conri2 with sistema
case choice = 3      && Movimiento de Bancos
   do conmb2
case choice = 4    && Extractos Bancarios
   do conextb2
   *   CASE choice = 5
   *     DO ConAutgM   && Autorizaciones MEF
case choice = 5
   do confp      && RENDICION DE FONDOS
endcase

if !escolor
   restore screen from principal
   on selection popup pop_15  do menu_15
endif
restore screen from principal
show popup pop_08,pop_15,menu
hide window all
return

*!*****************************************************************************
*!
*!      Procedure: MENU_16
*!
*!      Called by: MENU_08            (procedure in TESO.PRG)
*!               : MENU_16            (procedure in TESO.PRG)
*!
*!          Calls: BAR()              (function  in ?)
*!               : REGHC1.PRG
*!               : CONSAL1.PRG
*!               : CONHC.PRG
*!               : MENU_16            (procedure in TESO.PRG)
*!
*!*****************************************************************************
procedure menu_16   && Documentos de C.Presupuestal
*----------------
choice=bar()
do case
case choice = 1
   do reghc1 with 1
case choice = 2
   do consal1 with sistema
case choice = 3
   do conhc
endcase

if !escolor
   restore screen from principal
   on selection popup pop_16  do menu_16
endif
restore screen from principal
show popup pop_08,pop_16,menu
hide window all
return


*!*****************************************************************************
*!
*!      Procedure: MENU_16A
*!
*!      Called by: MENU_16A           (procedure in TESO.PRG)
*!
*!          Calls: BAR()              (function  in ?)
*!               : CONPRE.PRG
*!               : MENU_16A           (procedure in TESO.PRG)
*!
*!*****************************************************************************
procedure menu_16a  && Saldos de Presupuesto
*----------------
choice=bar()
do case
case choice = 1
   do conpre
case choice = 2
   *DO conpre1
endcase
if !escolor
   restore screen from principal
   on selection popup pop_16a  do menu_16a
endif
restore screen from principal
show popup pop_16a,menu
hide window all
return

*!*****************************************************************************
*!
*!      Procedure: SALMENU
*!
*!      Called by: ACT_MENUS          (procedure in TESO.PRG)
*!
*!          Calls: YESNO()            (function  in TES_LIB.PRG)
*!               : BORCON()           (function  in TES_LIB.PRG)
*!               : CLRSCR             (procedure in TES_LIB.PRG)
*!
*!*****************************************************************************
procedure salmenu
*----------------
store  yesno('¨ Estamos terminando la sesi¢n ?') to in_secoemsa
if in_secoemsa
	set color to
	*=ELICON()
	close data
	hide popup all
	
	=SavCon(2)
	if worker
		clear
		set help on
		on key
		set exclu off
		*=borcon()
		cancel
	 else
		restore screen from principal
		do clrscr
		=borcon()
	endif
	quit
 else
	return
endif

*!*****************************************************************************
*!
*!      Procedure: SALMENU2
*!
*!          Calls: YESNO()            (function  in TES_LIB.PRG)
*!               : CLRSCR1            (procedure in TES_LIB.PRG)
*!
*!*****************************************************************************
procedure salmenu2
*----------------
if yesno(' Estamos terminando la sesi¢n ')
   set color to
   *=ELICON()
   close data
   hide popup all
   restore screen from principal
   do clrscr1
   set help on
   deactivate window all
   if worker
      set exclu off
      cancel
   endif
   quit
endif
return

*!*****************************************************************************
*!
*!      Procedure: ABREPAS
*!
*!          Calls: SKPBAR()           (function  in ?)
*!
*!*****************************************************************************
procedure abrepas
*----------------
**- Accesos a las opciones; seg£n la seguridad especificada
* acceso a Egresos
set skip of bar 1  of pop_01 !'A' $vacceso
set skip of bar 2  of pop_01 !'B' $vacceso
set skip of bar 3  of pop_01 !'C' $vacceso
set skip of bar 5  of pop_01 !'D' $vacceso

* acceso a Ingresos
set skip of bar 1  of pop_02 !'E' $vacceso
set skip of bar 2  of pop_02 !'F' $vacceso
set skip of bar 3  of pop_02 !'G' $vacceso
set skip of bar 4  of pop_02 !'H' $vacceso

* acceso a Bancos
set skip of bar 1  of pop_04 !'I' $vacceso
set skip of bar 2  of pop_04 !'J' $vacceso
set skip of bar 4  of pop_04 !'K' $vacceso
set skip of bar 6  of pop_04 !'L' $vacceso
set skip of bar 7  of pop_04 !'M' $vacceso
SET SKIP OF BAR 8  OF pop_04 !'N' $vAcceso

* acceso a Programaci¢n
set skip of bar 1  of pop_05 !'O' $vacceso

* acceso a tablas
set skip of bar 1  of pop_06 !'P' $vacceso
set skip of bar 2  of pop_06 !'Q' $vacceso
set skip of bar 3  of pop_06 !'R' $vacceso
set skip of bar 4  of pop_06 !'S' $vacceso
set skip of bar 5  of pop_06 !'T' $vacceso
set skip of bar 6  of pop_06 !'U' $vacceso

* acceso a Cajero Pagador
set skip of bar 1  of pop_07 !'V' $vacceso

* acceso a Utilitarios
set skip of bar 1  of pop_09 !'W' $vacceso
set skip of bar 2  of pop_09 !'X' $vacceso
set skip of bar 3  of pop_09 !'Y' $vacceso
set skip of bar 4  of pop_09 !'Z' $vacceso
set skip of bar 5  of pop_09 !'a' $vacceso
set skip of bar 6  of pop_09 !'b' $vacceso
set skip of bar 8  of pop_09 !'c' $vacceso
set skip of bar 9  of pop_09 !'d' $vacceso
set skip of bar 11 of pop_09 !'e' $vacceso


* No acceso a Egresos
vopcion = .t.
for i = 1 to 5
   if !skpbar('pop_01',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 1  of menu vopcion

* No acceso a Ingresos
vopcion = .t.
for i = 1 to 4
   if !skpbar('pop_02',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 2  of menu  vopcion

* No acceso a Bancos
vopcion = .t.
for i = 1 to 7
   if !skpbar('pop_04',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 4  of menu vopcion


* No acceso a Programaci¢n
vopcion = .t.
for i = 1 to 1
   if !skpbar('pop_05',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 5  of menu vopcion


* No acceso a Tablas
vopcion = .t.
for i = 1 to 6
   if !skpbar('pop_06',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 6  of menu vopcion

* No acceso a Cajero /Pagador
vopcion = .t.
for i = 1 to 1
   if !skpbar('pop_07',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 7  of menu vopcion


* No acceso a Utilitarios
vopcion = .t.
for i = 1 to 11
   if !skpbar('Pop_09',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 8  of menu vopcion
return

*!*****************************************************************************
*: EOF: TESO.PRG
*!*****************************************************************************
*!
*!      Procedure: ABREPAS
*!
*!          Calls: SKPBAR()           (function  in ?)
*!
*!*****************************************************************************
procedure Xabrepas
*----------------
**- Accesos a las opciones; seg£n la seguridad especificada
* acceso a Egresos
set skip of bar 1  of pop_01 !'A' $vacceso
set skip of bar 2  of pop_01 !'B' $vacceso
set skip of bar 4  of pop_01 !'C' $vacceso
set skip of bar 5  of pop_01 !'D' $vacceso
set skip of bar 6  of pop_01 !'E' $vacceso
set skip of bar 7  of pop_01 !'=' $vacceso
set skip of bar 9  of pop_01 !'F' $vacceso

* acceso a Ingresos
set skip of bar 1  of pop_02 !'K' $vacceso
set skip of bar 2  of pop_02 !'L' $vacceso
set skip of bar 3  of pop_02 !'a' $vacceso
set skip of bar 4  of pop_02 !'G' $vacceso
set skip of bar 6  of pop_02 !'O' $vacceso
set skip of bar 7  of pop_02 !'P' $vacceso
set skip of bar 9  of pop_02 !'N' $vacceso
set skip of bar 10 of pop_02 !'X' $vacceso
set skip of bar 11 of pop_02 !'Y' $vacceso
set skip of bar 13 of pop_02 !'M' $vacceso

* acceso a Autoriz. Giro
set skip of bar 1  of pop_03 !'Q' $vacceso
*SET SKIP OF BAR 2  OF pop_03 !'R' $vAcceso
*SET SKIP OF BAR 3  OF pop_03 !'S' $vAcceso
*SET SKIP OF BAR 4  OF pop_03 !'T' $vAcceso
*SET SKIP OF BAR 6  OF pop_03 !'V' $vAcceso
*SET SKIP OF BAR 7  OF pop_03 !'U' $vAcceso
*SET SKIP OF BAR 8  OF pop_03 !'W' $vAcceso
*SET SKIP OF BAR 9  OF pop_03 !'Z' $vAcceso
*SET SKIP OF BAR 11 OF pop_03 !'b' $vAcceso
*SET SKIP OF BAR 12 OF pop_03 !'c' $vAcceso
*SET SKIP OF BAR 13 OF pop_03 !'d' $vAcceso


* acceso a Bancos
set skip of bar 1  of pop_04 !'e' $vacceso
set skip of bar 2  of pop_04 !'f' $vacceso
set skip of bar 3  of pop_04 !'g' $vacceso
set skip of bar 4  of pop_04 !'h' $vacceso
set skip of bar 5  of pop_04 !'i' $vacceso
set skip of bar 6  of pop_04 !'j' $vacceso
set skip of bar 7  of pop_04 !'I' $vacceso
*SET SKIP OF BAR 8  OF pop_04 !'J' $vAcceso

* acceso a Programaci¢n
set skip of bar 1  of pop_05 !'k' $vacceso
set skip of bar 2  of pop_05 !'k' $vacceso
set skip of bar 3  of pop_05 !'l' $vacceso
set skip of bar 4  of pop_05 !'m' $vacceso

* acceso a tablas
set skip of bar 1  of pop_06 !'p' $vacceso
set skip of bar 2  of pop_06 !'v' $vacceso
set skip of bar 3  of pop_06 !'w' $vacceso
set skip of bar 4  of pop_06 !'z' $vacceso
set skip of bar 6  of pop_06 !'x' $vacceso
set skip of bar 7  of pop_06 !'s' $vacceso
set skip of bar 8  of pop_06 !'1' $vacceso
set skip of bar 9  of pop_06 !'t' $vacceso
set skip of bar 10 of pop_06 !'u' $vacceso
set skip of bar 11 of pop_06 !'H' $vacceso
set skip of bar 12 of pop_06 !'r' $vacceso
set skip of bar 13 of pop_06 !'2' $vacceso
set skip of bar 14 of pop_06 !'q' $vacceso

* acceso a Cajero Pagador
set skip of bar 1  of pop_07 !'3' $vacceso
set skip of bar 2  of pop_07 !'4' $vacceso
set skip of bar 3  of pop_07 !'5' $vacceso
*SET SKIP OF BAR 3  OF pop_07 !'6' $vAcceso

* acceso a Informaci¢n Gerencial
set skip of bar 1  of pop_08 !'6' $vacceso
set skip of bar 2  of pop_08 !'7' $vacceso
set skip of bar 3  of pop_08 !'?' $vacceso
set skip of bar 4  of pop_08 !'8' $vacceso
set skip of bar 6  of pop_08 !'+' $vacceso
set skip of bar 7  of pop_08 !'9' $vacceso
set skip of bar 8  of pop_08 !'!' $vacceso
set skip of bar 9  of pop_08 !':' $vacceso
set skip of bar 11 of pop_08 !'>' $vacceso
set skip of bar 12 of pop_08 !'@' $vacceso
set skip of bar 13 of pop_08 !'#' $vacceso
set skip of bar 14 of pop_08 !'"' $vacceso
set skip of bar 16 of pop_08 !'<' $vacceso

* acceso a Utilitarios
set skip of bar 1  of pop_09 !'$' $vacceso
set skip of bar 2  of pop_09 !'%' $vacceso
set skip of bar 3  of pop_09 !'^' $vacceso
set skip of bar 4  of pop_09 !'&' $vacceso
set skip of bar 5  of pop_09 !'[' $vacceso
set skip of bar 8  of pop_09 !')' $vacceso
*SET SKIP OF BAR 6  OF pop_09 !'*' $vAcceso
*SET SKIP OF BAR 6  OF pop_09 ! ')' $vAcceso


* No acceso a Egresos
vopcion = .t.
for i = 1 to 9
   if !skpbar('pop_01',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 1  of menu vopcion

* No acceso a Ingresos
vopcion = .t.
for i = 1 to 4
*for i = 1 to 12
   if !skpbar('pop_02',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 2  of menu  vopcion

* No acceso a Autoriz. Giro
vopcion = .t.
for i = 1 to 3   &&13
   if !skpbar('pop_03',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 3  of menu vopcion

* No acceso a Bancos
vopcion = .t.
for i = 1 to 7
   if !skpbar('pop_04',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 4  of menu vopcion


* No acceso a Programaci¢n
vopcion = .t.
for i = 1 to 1
*for i = 1 to 4
   if !skpbar('pop_05',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 5  of menu vopcion


* No acceso a Tablas
vopcion = .t.
for i = 1 to 14
   if !skpbar('pop_06',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 6  of menu vopcion

* No acceso a Cajero /Pagador
vopcion = .t.
for i = 1 to 1
*for i = 1 to 3
   if !skpbar('pop_07',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 7  of menu vopcion



* No acceso a Inf. Gerencial
vopcion = .t.
for i = 1 to 13
   if !skpbar('Pop_08',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 8  of menu vopcion


* No acceso a Utilitarios
vopcion = .t.
for i = 1 to 8
   if !skpbar('Pop_09',i)
      vopcion = .f.
      exit
   endif
endfor
set skip of bar 9  of menu vopcion
return

*: EOF: TESO.PRG

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
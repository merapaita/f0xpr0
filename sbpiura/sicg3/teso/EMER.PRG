* ÖÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ·
* º  Programa SubPrincipal : EMER.Prg                                   º
* º    ®®  Sistema de Administraci¢n e Informaci¢n REGION GRAU ¯¯       º
* ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄº                                                               
* º  Empresa             : REGION GRAU  PIURA-PERU                      º
* º  Software            : Fox Pro 2.x                                  º
* º  Inicio              : Setiembre 1995                               º
* º  Subsistema          : Tesorer¡a                                    º
* º  Autores             : Ing. Federico Montero Valdiviezo.            º
* º                        A.S. Oswaldo Arturo Oliva Carl¡n.            º
* º  Inicio de Operaci¢n : 01/12/95                                     º
* º  Fecha Ultima Act.   : 17/04/97                                     º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
SET ESCAPE OFF
CLEAR
CLOSE ALL
CLEAR ALL
SET EXACT OFF
SET TALK OFF
SET ECHO OFF
worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO TES_lib
DO fox_ambi
public vCoddep,vNumdep,DESUSE,vuser_id,vConex,vMaq,vLlav,vUsurec,VFLAG,sistema

sistema='2'
vConex = ALLTRIM(LEFT(SYS(0),8))
vmaq   = ALLTRIM(RIGHT(SYS(0),2))
vllav  = ALLTRIM(sys(2))

*--------------------------
* NUEVA RUTA DE ACCESO

SET PATH TO H:\EMER98\DATA

*-----------------------------

=SAVCON()

IF WorKer
   IF YesNo("Datos de Prueba?")
	  SET PATH TO h:\SIS98\DATAPR
   ENDIF
ENDIF
IF  LASTKEY()#27 

**-- Definiciones para el Men£ --**
*----------------------------------*
*CIA  = " Regi¢n Grau "
*SUBC = "   CTAR      "
*DESA = "CTAR - RG"
CIA  = " CTAR PIURA "
SUBC = " CTAR PIURA "
DESA = "CTAR - PIURA"
TITU = " Sistema de Tesorer¡a " + "("+ALLTRIM(SUBSTR(SYS(0),1,10))+")"
USE USUARIO IN 0 ORDER USUARIO1 ALIAS USUARIO
SELE USUARIO
SEEK ALLTRIM(SUBSTR(SYS(0),1,10))
IF FOUND()
   SPAC1  = LEN(ALLT(usuario.nombre))  
   DESUSE = subst(Usuario.Nombre,1,spac1)
   
ELSE
   DESUSE =ALLTRIM(SUBSTR(SYS(0),1,10))
ENDIF
vCoddep  = coddep
vNumDep  = numdep
vflag    = flag
USE   

**-- Colores
escolor = iscolor()
IF escolor
   SET COLOR TO ,N/W,B
   c_panta = 8
   c_borde = 10
   c_popup = 4  &&15
   c_texto = "N/W"
   c_fondo = 1
ELSE
   c_panta = 8
   c_borde = 7
   c_popup = 3
   c_texto = "N+/W"
   c_fondo = 1
ENDIF
SET MESSAGE TO 23 CENTER

*IF escolor
 * @  1, 0 FILL TO 23,80 COLOR B/B,B/B
 * @  1, 1 FILL TO 23,24 COLOR BG/BG
*ELSE
   @  1, 0,23,79  BOX "°°°°°°°°°" COLOR SCHEME c_fondo
   @  1, 1,23,24  BOX "±±±±±±±±±" COLOR SCHEME c_fondo
*ENDIF

spac = (80 -(LEN(CIA)+LEN(TITU)+10))/2
Rotulo1 = CIA + SPACE(spac) + TITU + SPAC(spac) + space(11)

spac = (79 - (LEN(DESA)+LEN(DESUSE)+10))/2
Rotulo2 = DESA + SPACE(spac)+'® '+DESUSE+' ¯'+SPAC(spac)+DTOC(DATE())

DO Logos WITH Rotulo1,Rotulo2,50
@ 02,51 SAY " EMERGENCIA D.S 031-97 PCM " COLOR SCHEME 5

SAVE SCREEN TO Principal

* ERROR TRAPPING SYSTEM
IF WORKER
   ON ERROR
ELSE
   ON ERROR DO FOX_ERRS WITH PROGRAM()
ENDIF

SAVE SCREEN TO Pantalla
*-- Clave de acceso
vUser_ID = SPACE(5)
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
*vUser_ID = "ROSAM"

IF !FILE( '..\DATA\USUARIO.CDX')
  DO STANdBY WITH 'Error en INDEXA'
ENDIF
****
*USE Usuario ORDER TAG Usuario1  ALIAS Usua
*SELE usua
*IF !Clave(vUser_ID,19,30,23,55)
*   CLOSE DATA
*   DO STANDBY WITH '!! Acceso Denegado !!',10,15
*   IF Worker
*     CANCEL
*   ENDIF
*   QUIT
*ENDIF
*USE
****
USE IteUsu ORDER TAG IteUsu1   ALIAS IteU
SELE IteU
SET FILT TO sistema='3'
vUsuCla = CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
vAcceso = ''
SEEK vUsuCla
SCAN WHILE vUsuCla = ALLT(IteU.UsuCla)
  vAcceso = vAcceso + ALLT(IteU.Modulo)
ENDSCAN
SET FILT TO
USE

*DESA    = DESA + vUser_ID
*spac = 80 - LEN(DESA) - 10
*Rotulo2 = DESA + SPACE(spac) + DTOC(DATE()) + SPACE(2)

SET CLOCK ON
*SET CLOCK TO 00,67
STORE .t. TO vEn_sistema
STORE .f. TO esc_tecla
= SYS(2002,1)        && Set cursor on

*--- El men£ principal ---**
L_COL  ="W+/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "
*L_COL =" W+/BG, N/W, N/W, GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "

*ON KEY LABEL F1 ACTIVATE WINDOW calculator
* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º        Tesorer¡a          º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
IF escolor
   DEFINE POPUP menu FROM 5,2 TO 17,21 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP menu FROM 5,2 TO 17,21 COLOR SCHEME C_POPUP
ENDIF
DEFINE BAR  1  OF  menu PROMPT ' \<Egresos        ' MESSAGE ''
DEFINE BAR  2  OF  menu PROMPT ' \<Ingresos       ' MESSAGE ''
*DEFINE BAR  3  OF  menu PROMPT ' Int.\<Regional  ' MESSAGE ''
DEFINE BAR  3  OF  menu PROMPT ' Autoriz. Giro  ' MESSAGE ''
DEFINE BAR  4  OF  menu PROMPT ' Caja/\<Banco     ' MESSAGE ''
DEFINE BAR  5  OF  menu PROMPT ' \<Programaci¢n   ' MESSAGE ''
DEFINE BAR  6  OF  menu PROMPT ' \<Tablas         ' MESSAGE ''
DEFINE BAR  7  OF  menu PROMPT ' \<Cajero/Pagador ' MESSAGE ''
DEFINE BAR  8  OF  menu PROMPT ' Inf. \<Gerencial ' MESSAGE ''
DEFINE BAR  9  OF  menu PROMPT ' \<Utilitarios    ' MESSAGE ''
DEFINE BAR 10  OF  menu PROMPT ' \<Salida          '

*- Egresos
IF escolor
   DEFINE POPUP pop_01 FROM 06,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_01 FROM 06,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_01 PROMPT '\<Asigna Cta.Cte a H/C'		MESSAGE 'Asigna cuenta corriente por Hoja de Control'
DEFINE BAR 2  OF  pop_01 PROMPT 'Asigna Cta.Cte a H/\<M'		MESSAGE 'Asigna cuenta corriente por Hoja de Modificaci¢n'
DEFINE BAR 3  OF  pop_01 PROMPT '\-'
DEFINE BAR 4  OF  pop_01 PROMPT '\<Comprobante de Pago' 		MESSAGE 'Ingreso de Comprobantes de Pago'
DEFINE BAR 5  OF  pop_01 PROMPT '\<Hoja de Anulaci¢n de Cheque' MESSAGE 'Ingreso de Partes de Anulaci¢n de C/P'
DEFINE BAR 6  OF  pop_01 PROMPT 'Hoja de Cargas \<Diversas' 	MESSAGE 'Registra las regularizaciones de C/P'
DEFINE BAR 7  OF  pop_01 PROMPT 'Actualiza \<Nueva H/C en C/P' 	MESSAGE 'Actualiza hojas de Control en C/P'
DEFINE BAR 8  OF  pop_01 PROMPT '\-'
DEFINE BAR 9  OF  pop_01 PROMPT 'Cuadro de \<Retenciones  '		MESSAGE 'Reporte de Retenciones'

*- Ingresos
IF escolor
   DEFINE POPUP pop_02 FROM 06,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02 FROM 06,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02 PROMPT 'Recibos de \<Ingresos    ' MESSAGE 'Registro de Recibos de ingreso'
DEFINE BAR 2  OF  pop_02 PROMPT '\<Reversiones T - 6      ' MESSAGE 'Reporte de Reversiones'
DEFINE BAR 3  OF  pop_02 PROMPT '\<Facturas               ' MESSAGE 'Registro de Facturas'
DEFINE BAR 4  OF  pop_02 PROMPT '\<Memo Resultado de Bases' MESSAGE 'Registro de Memorandum de Resultado de Bases'
DEFINE BAR 5  OF  pop_02 PROMPT '\-'
DEFINE BAR 6  OF  pop_02 PROMPT 'Formato \<E6 Sede Central' MESSAGE 'Reporta el Formato E6'
DEFINE BAR 7  OF  pop_02 PROMPT 'Formato \<T5 Sede Central' MESSAGE 'Reporta el Formato T5 (A,B,C)'
DEFINE BAR 8  OF  pop_02 PROMPT '\-'
DEFINE BAR 9  OF  pop_02 PROMPT '\<Solicitud de Chequeras ' MESSAGE 'Registra las solicitudes de chequeras'
DEFINE BAR 10 OF  pop_02 PROMPT '\<Control de Chequeras'    MESSAGE 'Control de Entrega de chequeras'
DEFINE BAR 11 OF  pop_02 PROMPT 'Cierre \<de Chequera'      MESSAGE 'Proceso de Cierre de Chequera'
DEFINE BAR 12 OF  pop_02 PROMPT '\-'
DEFINE BAR 13 OF  pop_02 PROMPT 'Cartas \<Fianzas         ' MESSAGE 'Registro y control de Documentos valorados'


*- Integracion Regional
IF escolor
   DEFINE POPUP pop_03 FROM 07,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_03 FROM 07,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR  1  OF  pop_03 PROMPT '\<Recepci¢n de Autorizaci¢n de giro '   MESSAGE 'Registro Autorizaciones de Giro del MEF Lima'
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
DEFINE BAR  2 OF  pop_03 PROMPT 'Reporte \<AF-9                  ' 	  MESSAGE 'Muestra el Movimiento de Fondos Por Cta.Cte.'
DEFINE BAR  3 OF  pop_03 PROMPT 'Reporte Montos Pagados          ' 	  MESSAGE 'Muestra el Movimiento de Fondos Por Cta.Cte.'
*- Caja/Banco
IF escolor
   DEFINE POPUP pop_04 FROM 09,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_04 FROM 09,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_04 PROMPT 'Movimientos de \<Banco      ' MESSAGE 'Registro de N/C,N/A,B/D,R/I,etc'
DEFINE BAR 2  OF  pop_04 PROMPT '\<Extractos Bancarios ' MESSAGE 'Registro de los Extractos Bancarios enviados por los Bancos'
DEFINE BAR 3  OF  pop_04 PROMPT 'c\<Heques Certificados' MESSAGE 'Registro de cheques certificados pagados por los Bancos'
DEFINE BAR 4  OF  pop_04 PROMPT '\<Libro Banco               ' MESSAGE 'Reporte Libro Banco a nivel de CtaCte Central Piura'
DEFINE BAR 5  OF  pop_04 PROMPT '\<Conciliaci¢n Bancaria     ' MESSAGE 'Reporte de Conciliaci¢n'
DEFINE BAR 6  OF  pop_04 PROMPT '\<T4 Reporte de conciliaci¢n' MESSAGE 'Reporte de Conciliaci¢n del Movimiento de Fondos de Sub-Cuentas'
DEFINE BAR 7  OF  pop_04 PROMPT '\<Rebaja conciliaci¢n       ' MESSAGE 'Rebaja manual de conciliaci¢n'
DEFINE BAR 8  OF  pop_04 PROMPT 'Estado \<Actual de Conciliaci¢n' MESSAGE 'Reporte de Conciliaci¢n del Movimiento de Fondos de Sub-Cuentas'

*-Programaci¢n
IF escolor
   DEFINE POPUP pop_05 FROM 09,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_05 FROM 09,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_05 PROMPT '\<Hojas de Trabajo SEDE    ' MESSAGE 'Reporta Hoja de Trabajo Detallada'
DEFINE BAR 2  OF  pop_05 PROMPT 'Hojas de Trabajo \<SECTORES' MESSAGE 'Reporta Hoja de Trabajo Detallada'
DEFINE BAR 3  OF  pop_05 PROMPT 'h\<Oja Trabajo Resumida    ' MESSAGE 'Reporta Hoja de Trabajo Resumida '
*DEFINE BAR 4  OF  pop_05 PROMPT '\<Reporte T2               ' MESSAGE 'Reporte Informe mensual del gasto T2'
DEFINE BAR 4  OF  pop_05 PROMPT '\<Saldos Ejecuci¢n x CtaCte' MESSAGE 'Reporte Informe mensual del gasto T2'

*- Tablas
IF escolor
   DEFINE POPUP pop_06 FROM 05,26 COLOR &l_col    &&SHADOW
ELSE
   DEFINE POPUP pop_06 FROM 05,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF pop_06 PROMPT '\<Bancos'
DEFINE BAR 2  OF pop_06 PROMPT '\<Cuentas Corrientes y Ahorros'
DEFINE BAR 3  OF pop_06 PROMPT 'Concepto \<Movimiento de Bancos'
DEFINE BAR 4  OF pop_06 PROMPT '\<Tipos de Recibos de Ingreso'
DEFINE BAR 5  OF pop_06 PROMPT 'Tipos de Re\<versiones'
DEFINE BAR 6  OF pop_06 PROMPT '\<Rubros de Retenciones'
DEFINE BAR 7  OF pop_06 PROMPT 'Clasificador por Objeto del \<Gasto'
DEFINE BAR 8  OF pop_06 PROMPT 'Clasificador de \<Ingresos'
DEFINE BAR 9  OF pop_06 PROMPT '\<Oficinas giradoras Tesoro P£blico'
DEFINE BAR 10 OF pop_06 PROMPT 'Ofici\<nas giradoras Ingresos Propios'
DEFINE BAR 11 OF pop_06 PROMPT 'Concursos de \<Licitaci¢n/Otros'
DEFINE BAR 12 OF pop_06 PROMPT '\<Documentos '
DEFINE BAR 13 OF pop_06 PROMPT 'Au\<xiliar OTros'
DEFINE BAR 14 OF pop_06 PROMPT 'M¢dulos del \<Sistema'



*- Cajero/Pagador
IF escolor
   DEFINE POPUP pop_07 FROM 11,26 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_07 FROM 11,26 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF  pop_07 PROMPT '\<Entrega de Cheques  ' MESSAGE 'Entrega de Cheques a Acrredores'
DEFINE BAR 2 OF  pop_07 PROMPT '\<Listado de Cheques  ' MESSAGE 'Entrega de Cheques a Acrredores'
*DEFINE BAR 3 OF  POP_07 PROMPT '\<Caja Chica            '
DEFINE BAR 3 OF  POP_07 PROMPT 'Rendici¢n de \<Fondos   ' MESSAGE 'Registra rendici¢n de fondos'


*- Informaci¢n Gerencial
IF escolor
   DEFINE POPUP pop_08 FROM 04,26 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_08 FROM 04,26 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_08 PROMPT 'Autorizaci¢n de Che\<ques'		       MESSAGE 'Autorizaci¢n Director(a) Tesorer¡a'
DEFINE BAR 2  OF  POP_08 PROMPT '\<Seguimiento de Cheques '		       MESSAGE 'Consulta de Cheques.. Seguimiento'
DEFINE BAR 3  OF  POP_08 PROMPT 'Seguimiento \<Doc. Fuente'		       MESSAGE 'Res£men Completo de Documentos Fuente'	
DEFINE BAR 4  OF  POP_08 PROMPT '\<Reporte de Cheques'			       MESSAGE 'Reporte de Cheques ..Estado Situacional'	
DEFINE BAR 5  OF  POP_08 PROMPT '\-'
DEFINE BAR 6  OF  POP_08 PROMPT 'Saldos p\<or Cuenta Bancos'		   MESSAGE 'Consulta de Saldos de Bancos'	
DEFINE BAR 7  OF  POP_08 PROMPT 'Saldos Aut.de \<Giro'                 MESSAGE 'Consulta salods de Autorizaci¢n de Giro'	
DEFINE BAR 8  OF  POP_08 PROMPT 'Saldos por \<Banco'                   MESSAGE 'Consulta salods de Autorizaci¢n de Giro'	
DEFINE BAR 9  OF  POP_08 PROMPT 'Captaci¢n de \<Ingresos' 			   MESSAGE 'Consulta captaci¢n de Ingresos por mes'	
DEFINE BAR 10 OF  POP_08 PROMPT '\-'
DEFINE BAR 11 OF  POP_08 PROMPT 'De \<Presupuesto       '	           MESSAGE ''
DEFINE BAR 12 OF  pop_08 PROMPT 'De \<Abastecimiento    '             MESSAGE ''
DEFINE BAR 13 OF  pop_08 PROMPT 'De \<Tesorer¡a         '             MESSAGE ''
DEFINE BAR 14 OF  pop_08 PROMPT 'De \<Cont. Presup.     '             MESSAGE ''
DEFINE BAR 15 OF  POP_08 PROMPT '\-'
DEFINE BAR 16 OF  POP_08 PROMPT 'Seguimiento a Usuario'				   MESSAGE 'Determina el usuario que ha registrado el Doc. Fte'	

*- Utilitarios
IF escolor
   DEFINE POPUP pop_09 FROM 05,26 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_09 FROM 05,26 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_09  PROMPT '\<Indexar archivos             ' MESSAGE 'Indexa uno o todos los archivos del Sistema'
DEFINE BAR  2 OF Pop_09  PROMPT '\<Backup de los archivos       ' MESSAGE 'Genera un archivo de respaldo'
DEFINE BAR  3 OF Pop_09  PROMPT 'De\<puraci¢n de backups        ' MESSAGE 'Elimina los archivos de respaldo'
DEFINE BAR  4 OF Pop_09  PROMPT 'Man\<Tenimiento Correlativos    '
DEFINE BAR  5 OF Pop_09  PROMPT 'Mantenimiento Pa\<R metros    '
DEFINE BAR  6 OF Pop_09  PROMPT 'impresi¢n de \<Archivos        '
DEFINE BAR  7 OF Pop_09  PROMPT '\-'
DEFINE BAR  8 OF pop_09  PROMPT '\<Seguridad         ' 				MESSAGE '®® Acceso RESTRINGIDO ¯¯'
DEFINE BAR  9 OF Pop_09  PROMPT 'Control de Mensajes y \<Usuarios'
DEFINE BAR 10 OF Pop_09  PROMPT '\-'
DEFINE BAR 11 OF Pop_09  PROMPT 'Ir al D\<OS         ' MESSAGE '®® Escriba EXIT para regresar ¯¯'
DEFINE BAR 12 OF Pop_09  PROMPT 'Ir al \<Word Perfect' MESSAGE '®® Escriba EXIT para regresar ¯¯'
DEFINE BAR 13 OF Pop_09  PROMPT 'Ir al \<QUATTRO PRO'  MESSAGE '®® Escriba EXIT para regresar ¯¯'
*DEFINE BAR  7 OF Pop_09  PROMPT '\<Conectarse a la Print-Server  '
*DEFINE BAR  8 OF Pop_09  PROMPT '\<Desconectarse de la Print-Server '

*- De presupuesto
IF escolor
   DEFINE POPUP pop_11 FROM 13,56 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_11 FROM 13,56 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_11 PROMPT '\<Saldos calendario ' MESSAGE ''
DEFINE BAR 2  OF  pop_11 PROMPT 'Saldos del \<Marco  ' MESSAGE ''


*-- Documentos de Abastecimiento
  IF Escolor
     DEFINE POPUP pop_14 FROM 11,56  SHADOW COLOR &l_col
  ELSE
     DEFINE POPUP pop_14 FROM 11,56  COLOR SCHEME c_popup
  ENDIF
 
DEFINE BAR  01  OF  pop_14 PROMPT '\<Pecosa                '         MESSAGE ''
DEFINE BAR  02  OF  pop_14 PROMPT 'Solicitud de \<Servicio '         MESSAGE ''
DEFINE BAR  03  OF  pop_14 PROMPT 'Orden de \<Compra       '         MESSAGE ''
DEFINE BAR  04  OF  pop_14 PROMPT '\<Orden de Servicio     '         MESSAGE ''
DEFINE BAR  05  OF  pop_14 PROMPT 'P¢liza de En\<Trada     '         MESSAGE ''
DEFINE BAR  06  OF  pop_14 PROMPT 'P¢liza de Sa\<Lida      '         MESSAGE ''
DEFINE BAR  07  OF  pop_14 PROMPT 'Parte de A\<Nulaci¢n    '         MESSAGE ''
DEFINE BAR  08  OF  pop_14 PROMPT 'Parte de \<Rebaja       '         MESSAGE ''
*-

*-- Documentos de Tesorer¡a
  IF Escolor
     DEFINE POPUP pop_15 FROM 12,56  SHADOW COLOR &l_col
  ELSE
     DEFINE POPUP pop_15 FROM 12,56  COLOR SCHEME c_popup
  ENDIF

DEFINE BAR  01  OF  pop_15 PROMPT '\<Comprobante de Pago   '         MESSAGE ''
DEFINE BAR  02  OF  pop_15 PROMPT '\<Recibo de Ingreso     '         MESSAGE ''
DEFINE BAR  03  OF  pop_15 PROMPT '\<Movimientos Bancarios '         MESSAGE ''
DEFINE BAR  04  OF  pop_15 PROMPT '\<Extractos Bancarios   '         MESSAGE ''
DEFINE BAR  05  OF  pop_15 PROMPT '\<Autorizaciones MEF    '         MESSAGE ''
*DEFINE BAR  06  OF  pop_15 PROMPT '\<Distrib. Aut. Giro    '         MESSAGE ''
*-

*- Documentos de C.presupuestal
  IF Escolor
     DEFINE POPUP pop_16 FROM 13,56  SHADOW COLOR &l_col
  ELSE
     DEFINE POPUP pop_16 FROM 13,56  COLOR SCHEME c_popup
  ENDIF
DEFINE BAR  01  OF  pop_16 PROMPT '\<Hoja de Control     '         MESSAGE ''
DEFINE BAR  02  OF  pop_16 PROMPT 'Saldo de \<Calendario '         MESSAGE ''
DEFINE BAR  03  OF  pop_16 PROMPT '\<Reporte de H/C      '         MESSAGE ''

IF escolor
   DEFINE POPUP pop_16a FROM 11,56 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_16a FROM 11,56 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_16a PROMPT '\<Acumulados  ' MESSAGE ''
DEFINE BAR 2  OF  pop_16a PROMPT '\<Mensualizado' MESSAGE ''


ON SELECTION POPUP menu DO ACT_MENUS

IF !vUser_ID$'LCDJOSEJFG'
	DO ABREPAS
ENDIF	
**- La sombra del principal
SAVE SCREEN TO principal
ACTIVATE POPUP menu
DO WHILE  EMPTY(PROMPT())
   ACTIVATE POPUP menu
ENDDO
*-Cerrar todo...
ON KEY
endif
=BORCON()
RETURN

*-- Fin  del Programa Principal


PROCEDURE ACT_MENUS
*------------------
DO CASE
   CASE BAR() = 1
        ON SELECTION POPUP pop_01  DO  menu_01
        ACTIVATE POPUP pop_01
   CASE BAR() = 2
        ON SELECTION POPUP pop_02  DO  menu_02
        ACTIVATE POPUP pop_02
   CASE BAR() = 3
        ON SELECTION POPUP pop_03  DO  menu_03   && Integraci¢n Regional
        ACTIVATE POPUP pop_03
   CASE BAR() = 4
        ON SELECTION POPUP pop_04  DO  menu_04   
        ACTIVATE POPUP pop_04
   CASE BAR() = 5
        ON SELECTION POPUP pop_05  DO  menu_05
        ACTIVATE POPUP pop_05
   CASE BAR() = 6
        ON SELECTION POPUP pop_06  DO  menu_06
        ACTIVATE POPUP pop_06
   CASE BAR() = 7
        ON SELECTION POPUP pop_07  DO  menu_07
        ACTIVATE POPUP pop_07
   CASE BAR() = 8
        ON SELECTION POPUP pop_08  DO  menu_08
        ACTIVATE POPUP pop_08
   CASE BAR() = 9
        ON SELECTION POPUP pop_09  DO  menu_09
        ACTIVATE POPUP pop_09        
   CASE BAR() = 10
        DO Salmenu
ENDCASE

RESTORE SCREEN FROM principal
RETURN


PROCEDURE menu_01   && Egresos
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1    && Asigna Cta.Cte H/C
       DO AsigCCHC
    CASE choice = 2    && Asigna Cta.Cte H/M
       DO AsigCCHM
    CASE choice = 4    && Registro C/P
       DO RegCp WITH sistema
    CASE choice = 5    && Registro Parte de Anulaci¢n
       DO RegHA WITH sistema
    CASE choice = 6    && Hoja de Cargas Diversas
       DO RegCD
    CASE choice = 7    && Actualiza H/C en C/P
       DO act_cphc
    CASE choice = 9     && Listado Retenciones
       DO cuaret WITH sistema
  ENDCASE

  IF !escolor
   *RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_01  DO menu_01
  ENDIF
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_01,menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE menu_02    && Ingresos
*----------------
PRIVATE choice
  choice=BAR()
  DO CASE
    CASE choice = 1     && Recibos de ingresos
       DO RegRI WITH sistema
    CASE choice = 2     && Reversiones
       DO RegRev
    CASE choice = 3     && Registro de facturas
       DO RegFac
    CASE choice = 4     && Memo Resultado de Bases
       DO MembasLi
    CASE choice = 6     &&  Fto E6
       *DO ftoE6
    CASE choice = 7     &&  Fto T5
       *DO ftoT5
   CASE choice =  9    &&  Solicitud de Chequeras
       DO Ofichq
   CASE choice =  10   &&  Recibos de ingresos
       DO Contchq
   CASE choice =  11   &&  Recibos de ingresos
      DO Ciechq
    CASE choice = 13    && Control de Cartas Fianza
       DO CarFia
  ENDCASE

  IF !escolor
   *RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02  DO menu_02
  ENDIF
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_02,menu
  HIDE WINDOW ALL
  RETURN

PROCEDURE menu_02a   && Programaci¢n
*----------------
PRIVATE choice
  choice=BAR()
  DO CASE
    CASE choice = 1    && Listado de Hojas de Trabajo ingresos propios
	  DO reGRI1
    CASE choice = 2    && Res£men de Hojas de Trabajo
      DO reGRI97
  ENDCASE
  IF !escolor
   *RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02a  DO menu_02a
  ENDIF
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_02,pop_02a,menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE menu_03   && Integraci¢n Regional
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1    && Autorizaciones de giro Lima (MEF) - TRN
        DO AutGMEF
    CASE choice = 2    && Autorizaciones de giro Regi¢n Grau  - TRN
         *DO AutGTRN
    CASE choice = 3    && Cartas Orden de Transferecias  -TRN
         *DO CarOrd  
    CASE choice = 4    && Reversiones de Saldos No Utilizados -TRN
         *DO RevSnu  

    CASE choice = 6    && Transferencia de Fondos -PRP
        *DO TransFon
    CASE choice = 7    && Cheques de Gerencia -PRP
         *DO ChqGer
    CASE choice = 8    && Autorizaciones Simples -PRP
         *DO AutSim
    CASE choice = 9   && Autorizaci¢n de Giro PRP
        *DO AutgPRP
    CASE choice = 11  && Reportes Anexo 1GR
		 *DO Anex1gr
    CASE choice = 12  && Ejecuci¢n Presupuestal
         *DO ejepre
    CASE choice = 13  && AF-9
         *DO AF-9
         
  ENDCASE
  IF !escolor
   *RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_03  DO menu_03
  ENDIF
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_03,menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE menu_04   && Caja/Banco
*----------------
PRIVATE choice

  choice=BAR()
  DO CASE
    CASE choice = 1    && Movimientos de banco
		 DO RegMB  
    CASE choice = 2    && Ingreso Extracto de Cta Cte Bco
         DO Extbco
    CASE choice = 3    && Ingreso de cheques certificados
         DO Chqctf
    CASE choice = 4    && Libro Bancos
         DO LibBan
    CASE choice = 5    && Conciliaci¢n
         DO conban
    CASE choice = 6    && Reporte T4
         *DO RepT4
    CASE choice = 7    && Rebaja manual de conciliaci¢n
         DO rebcon
    CASE choice = 8    && Consulta estado de conciliaci¢n
         *DO con_conb
  ENDCASE
  IF !escolor
   *RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_04  DO menu_04
  ENDIF
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_04,menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE menu_05   && Programaci¢n
*----------------
PRIVATE choice
  choice=BAR()
  DO CASE
    CASE choice = 1    && Listado de Hojas de Trabajo SEDE
	  DO lishoj WITH 1
    CASE choice = 2    && Listado de Hojas de Trabajo SECTORES
	  DO lishoj WITH 2
    CASE choice = 3    && Resumen de Hojas de Trabajo
      *DO reshoj
    CASE choice = 4    && Saldos ejecuci¢n
      *DO Ftot2
      DO ConSal
  ENDCASE

  IF !escolor
   *RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_05  DO menu_05
  ENDIF
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_05,menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE menu_06   && Tablas
*----------------
PRIVATE choice
  choice=BAR()
  DO CASE
         CASE BAR()=1  &&Bancos
            DO ManPar WITH 'BANCOS','Banco','Bancos'
         CASE BAR()=2  &&Cuenta de Bancos
            DO CatCaj
         CASE BAR()=3  &&Movimiento de Bancos
            DO CatMov
         CASE BAR()=4  &&Recibos de Ingreso
            DO cattipri
         CASE BAR()=5  && Reversiones
            DO ManPar WITH 'CODREV','Reversi¢n','Reversiones'
         CASE BAR()=6  && Retenciones
            DO Catret 
         CASE BAR()=7  &&Clasificador por Objeto del Gasto
            DO Clasific
         CASE BAR()=8  && Clasificador de Ingresos
	        DO Ingreso  
         CASE BAR()=9  && Oficinas Giradoras del Tesoro
            DO ManPar WITH 'OFIGIR','Oficina Giradora','Oficinas Giradoras'
         CASE BAR()=10 && Oficinas Giradoras de Ingresos Propios    
            DO ManPar WITH 'OFGIIP','Oficina Giradora','Oficinas Giradoras'         
         CASE BAR()=11 && Concursos de Licitaci¢n
            DO ConLic  && Documentos
	     CASE BAR()=12
            DO ManPar WITH 'DOCCAJ','Tipo de Documento','Tipos de Documentos'
         CASE BAR()=13  && Auxiliar Otros
            DO manaux
         CASE BAR()=14  && M¢dulos del Sistema
            DO ManPar WITH 'MODTES','M¢dulo del Sistema','M¢dulos del Sistema'
  ENDCASE
  IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_06  DO menu_06
  ENDIF
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_06,menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE menu_07  && Cajero/Pagador
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1     && Entrega de cheque
       DO EntChq
    CASE choice = 2     && Reporte de cheque
       DO repchqca
    CASE choice = 3    && Rendici¢n de Fondos
       DO RegFP WITH sistema
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_07  DO menu_07
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_07,menu
 HIDE WINDOW ALL
 RETURN


PROCEDURE menu_08  && Informaci¢n Gerencial
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1     && Autoriza Cheque x Tesorer¡a
       DO autchqte
    CASE choice = 2     && Consulta de Cheques
       DO segchQ
    CASE choice = 3     && Listado de Cheques 
       DO segdocft
    CASE choice = 4     && Listado de Cheques 
       DO repchq
    CASE choice = 6     && Saldos de Bancos   
*      DO sal_bco
    CASE choice = 7     && Saldos de Autorizaciones
*      DO sal_aut
    CASE choice = 8     && Estado de Conciliaci¢n
*      DO sal_cta 
    CASE choice = 9     && Captaci¢n de Ingresos
*      DO cap_ing  
    CASE choice = 11    && Inf. de Presupuesto
       ON SELECTION POPUP pop_11  DO  menu_11
       ACTIVATE POPUP Pop_11
    CASE choice = 12    && Inf. de Abastecimientos
       ON SELECTION POPUP pop_14  DO  menu_14
       ACTIVATE POPUP Pop_14
    CASE choice = 13     && Inf. de Tesorer¡a
       ON SELECTION POPUP pop_15  DO  menu_15
       ACTIVATE POPUP Pop_15
    CASE choice = 14     && Inf. de C.presupuestal
       ON SELECTION POPUP pop_16  DO  menu_16
       ACTIVATE POPUP Pop_16
    CASE choice = 16    && Consulta Doc. por Usuario
*      DO consegte
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_08  DO menu_08
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_08,menu
 HIDE WINDOW ALL
 RETURN
 

PROCEDURE menu_09  && Utilitarios
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    && Reindexado
        DO Indexate
    CASE choice = 2    && Backup
         DO Backup
    CASE choice = 3    && Depura Backup
         DO DepBK    
    CASE choice = 4    && Correlativo
         DO manpar WITH "CORREL","Correlativo","Correlativos",4,"Valor siguiente"
    CASE choice = 5    && Par metros 
         DO manpar WITH "PARFAC","Valor Actual","Par metros",4,"Valor siguiente"
    CASE BAR() =  6
         DO ImpArc         
*    CASE choice = 7
*         RUN CAPTURE /Q=PRINTQ_0 /TI=0 /No Banner /No Formfeed
*         RESTORE  SCREEN FROM Principal    
*    CASE choice = 8
*         RUN ENDCAP
*         RESTORE  SCREEN FROM Principal    
    CASE choice = 8    && Seguridad
    	     DO segteso
    CASE BAR() =  9
         DO ContUsu         
    CASE BAR() = 11
         RESTORE  SCREEN FROM Principal
         CLEAR
         RUN FOXSWAP COMMAND
         RESTORE  SCREEN FROM Principal
    CASE BAR() = 12
         RESTORE  SCREEN FROM Principal
         CLEAR
         RUN FOXSWAP WP
         RESTORE  SCREEN FROM Principal
    CASE BAR() = 13
         RESTORE  SCREEN FROM Principal
         CLEAR
         RUN FOXSWAP Q
         RESTORE  SCREEN FROM Principal
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_09  DO menu_09
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP Pop_09,pop_09,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_11  && De Presupuesto
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  Indexa
    	*DO SalCal WITH 1
    CASE choice = 2
    	*DO Salpre WITH '02'
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_11  DO menu_11
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP Pop_08,pop_11,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_14   && Documentos de Abastecimientos
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
      DO REGPEC WITH 2,'F'    && Pecosa
    CASE choice = 2 
      DO regsol     && Solicitud de servicios
    CASE choice = 3
      *DO ConOc      && Orden de Compra
      DO OrdCom
    CASE choice = 4
      *DO ConOs      && Orden de Servicio
      DO OrdSer
    CASE choice = 5
      *DO ConPent   && Poliza de Entrada
    CASE choice = 6
      *DO ConPsal   && Poliza de Salida
    CASE choice = 7
      *IF yesno("Partes de Anulaci¢n de O/C:[Si] o de O/S:[No] ? ")
      *   DO ConPan   WITH 1  && Parte anulaci¢n O/C
      *ELSE 
      *   DO ConPan   WITH 2  && Parte anulaci¢n O/S
      *ENDIF   
    CASE choice = 8
     *IF yesno("Partes de Rebaja de O/C:[Si] o de O/S:[No] ? ")
     * 	DO ConPrb   WITH 1 && Parte rebaja DE o/c
     *ELSE
     *   DO ConPrb   WITH 2 && Parte rebaja DE o/S
     *ENDIF 	
  ENDCASE
  
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_14  DO menu_14
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP Pop_08,pop_14,menu
 HIDE WINDOW ALL
 RETURN


PROCEDURE menu_15   && Documentos de Tesorer¡a
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
*      DO ConCP       && Comprobante de Pago
    CASE choice = 2
*      DO ConRI       && Recibo de Ingreso
    CASE choice = 3
 *     DO Conmb       && Movimiento de Bancos
    CASE choice = 4
  *    DO ConExtBc    && Extractos Bancarios
    CASE choice = 5
   *   DO ConAutgM   && Autorizaciones MEF
    CASE choice = 6
    *  DO ConAutgT    && Distribuci¢n Aut. Giro.
  ENDCASE
  
   IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_15  DO menu_15
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP Pop_08,pop_15,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_16   && Documentos de C.Presupuestal
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
      DO RegHc1 WITH 1
    CASE choice = 2
      DO consal1 WITH SISTEMA
    CASE choice = 3
      DO CONHC
  ENDCASE
  
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_16  DO menu_16
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP Pop_08,pop_16,menu
 HIDE WINDOW ALL
 RETURN


PROCEDURE menu_16a  && Saldos de Presupuesto
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    
    	DO conpre
    CASE choice = 2
    	*DO conpre1
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_16a  DO menu_16a
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_16a,menu
 HIDE WINDOW ALL
RETURN

PROCEDURE salmenu
*----------------
  STORE  YESNO('¨ Estamos terminando la sesi¢n ?') TO in_secoemsa
  IF in_secoemsa
    SET COLOR TO
    =ELICON()
    CLOSE DATA
    HIDE POPUP ALL
    
    IF worker
      CLEAR
      SET HELP ON
      ON KEY
      SET EXCLU OFF
      =BORCON()
      CANCEL
    ELSE
      RESTORE SCREEN FROM Principal
      DO CLRSCR
      =BORCON()
    ENDIF
    QUIT
  ELSE
     RETURN
  ENDIF

PROCEDURE salmenu2
*----------------
  IF YESNO(' Estamos terminando la sesi¢n ')
     SET COLOR TO
    =ELICON()     
     CLOSE DATA
     HIDE POPUP ALL
     RESTORE SCREEN FROM Principal
     DO CLRSCR1
     SET HELP ON
     DEACTIVATE WINDOW ALL
     IF worker
        SET EXCLU OFF
        CANCEL
     ENDIF
     QUIT
  ENDIF
  RETURN

PROCEDURE ABREPAS
*----------------
**- Accesos a las opciones; seg£n la seguridad especificada
* acceso a Egresos
SET SKIP OF BAR 1  OF pop_01 !'A' $vAcceso
SET SKIP OF BAR 2  OF pop_01 !'B' $vAcceso
SET SKIP OF BAR 4  OF pop_01 !'C' $vAcceso
SET SKIP OF BAR 5  OF pop_01 !'D' $vAcceso
SET SKIP OF BAR 6  OF pop_01 !'E' $vAcceso
SET SKIP OF BAR 7  OF pop_01 !'=' $vAcceso
SET SKIP OF BAR 9  OF pop_01 !'F' $vAcceso

* acceso a Ingresos
SET SKIP OF BAR 1  OF pop_02 !'K' $vAcceso
SET SKIP OF BAR 2  OF pop_02 !'L' $vAcceso
SET SKIP OF BAR 3  OF pop_02 !'a' $vAcceso
SET SKIP OF BAR 4  OF pop_02 !'G' $vAcceso
SET SKIP OF BAR 6  OF pop_02 !'O' $vAcceso
SET SKIP OF BAR 7  OF pop_02 !'P' $vAcceso
SET SKIP OF BAR 9  OF pop_02 !'N' $vAcceso
SET SKIP OF BAR 10 OF pop_02 !'X' $vAcceso
SET SKIP OF BAR 11 OF pop_02 !'Y' $vAcceso
SET SKIP OF BAR 13 OF pop_02 !'M' $vAcceso

* acceso a tesorer¡a Regional
SET SKIP OF BAR 1  OF pop_03 !'Q' $vAcceso
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
SET SKIP OF BAR 1  OF pop_04 !'e' $vAcceso
SET SKIP OF BAR 2  OF pop_04 !'f' $vAcceso
SET SKIP OF BAR 3  OF pop_04 !'g' $vAcceso
SET SKIP OF BAR 4  OF pop_04 !'h' $vAcceso
SET SKIP OF BAR 5  OF pop_04 !'i' $vAcceso
SET SKIP OF BAR 6  OF pop_04 !'j' $vAcceso
SET SKIP OF BAR 7  OF pop_04 !'I' $vAcceso
SET SKIP OF BAR 8  OF pop_04 !'J' $vAcceso

* acceso a Programaci¢n
SET SKIP OF BAR 1  OF pop_05 !'k' $vAcceso
SET SKIP OF BAR 2  OF pop_05 !'k' $vAcceso
SET SKIP OF BAR 3  OF pop_05 !'l' $vAcceso
SET SKIP OF BAR 4  OF pop_05 !'m' $vAcceso

* acceso a tablas
SET SKIP OF BAR 1  OF pop_06 !'p' $vAcceso
SET SKIP OF BAR 2  OF pop_06 !'v' $vAcceso
SET SKIP OF BAR 3  OF pop_06 !'w' $vAcceso
SET SKIP OF BAR 4  OF pop_06 !'z' $vAcceso
SET SKIP OF BAR 6  OF pop_06 !'x' $vAcceso
SET SKIP OF BAR 7  OF pop_06 !'s' $vAcceso
SET SKIP OF BAR 8  OF pop_06 !'1' $vAcceso
SET SKIP OF BAR 9  OF pop_06 !'t' $vAcceso
SET SKIP OF BAR 10 OF pop_06 !'u' $vAcceso
SET SKIP OF BAR 11 OF pop_06 !'H' $vAcceso
SET SKIP OF BAR 12 OF pop_06 !'r' $vAcceso
SET SKIP OF BAR 13 OF pop_06 !'2' $vAcceso
SET SKIP OF BAR 14 OF pop_06 !'q' $vAcceso

* acceso a Cajero Pagador
SET SKIP OF BAR 1  OF pop_07 !'3' $vAcceso
SET SKIP OF BAR 2  OF pop_07 !'4' $vAcceso
SET SKIP OF BAR 3  OF pop_07 !'5' $vAcceso


* acceso a Informaci¢n Gerencial
SET SKIP OF BAR 1  OF pop_08 !'6' $vAcceso
SET SKIP OF BAR 2  OF pop_08 !'7' $vAcceso
SET SKIP OF BAR 3  OF pop_08 !'?' $vAcceso
SET SKIP OF BAR 4  OF pop_08 !'8' $vAcceso
SET SKIP OF BAR 6  OF pop_08 !'+' $vAcceso
SET SKIP OF BAR 7  OF pop_08 !'9' $vAcceso
SET SKIP OF BAR 8  OF pop_08 !'!' $vAcceso
SET SKIP OF BAR 9  OF pop_08 !':' $vAcceso
SET SKIP OF BAR 11 OF pop_08 !'>' $vAcceso
SET SKIP OF BAR 12 OF pop_08 !'@' $vAcceso
SET SKIP OF BAR 13 OF pop_08 !'#' $vAcceso
SET SKIP OF BAR 14 OF pop_08 !'"' $vAcceso
SET SKIP OF BAR 16 OF pop_08 !'<' $vAcceso

* acceso a Utilitarios
SET SKIP OF BAR 1  OF pop_09 !'$' $vAcceso
SET SKIP OF BAR 2  OF pop_09 !'%' $vAcceso
SET SKIP OF BAR 3  OF pop_09 !'^' $vAcceso
SET SKIP OF BAR 4  OF pop_09 !'&' $vAcceso
SET SKIP OF BAR 5  OF pop_09 !'[' $vAcceso
SET SKIP OF BAR 8  OF pop_09 !')' $vAcceso
*SET SKIP OF BAR 6  OF pop_09 !'*' $vAcceso
*SET SKIP OF BAR 6  OF pop_09 ! ')' $vAcceso


* No acceso a Egresos
vOpcion = .T.
FOR I = 1 TO 9
  IF !SKPBAR('pop_01',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 1  OF menu vOpcion

* No acceso a Ingresos
vOpcion = .T.
FOR I = 1 TO 12
  IF !SKPBAR('pop_02',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 2  OF menu  vOpcion

* No acceso a Int. Regional
vOpcion = .T.
FOR I = 1 TO 3
  IF !SKPBAR('pop_03',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 3  OF menu vOpcion

* No acceso a Bancos
vOpcion = .T.
FOR I = 1 TO 8
  IF !SKPBAR('pop_04',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 4  OF menu vOpcion


* No acceso a Programaci¢n
vOpcion = .T.
FOR I = 1 TO 3
  IF !SKPBAR('pop_05',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 5  OF menu vOpcion


* No acceso a Tablas
vOpcion = .T.
FOR I = 1 TO 14
  IF !SKPBAR('pop_06',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 6  OF menu vOpcion

* No acceso a Cajero /Pagador  
vOpcion = .T.
FOR I = 1 TO 3
  IF !SKPBAR('pop_07',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 7  OF menu vOpcion



* No acceso a Inf. Gerencial
vOpcion = .T.
FOR I = 1 TO 16
  IF !SKPBAR('Pop_08',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 8  OF menu vOpcion


* No acceso a Utilitarios   
vOpcion = .T.
FOR I = 1 TO 8
  IF !SKPBAR('Pop_09',I)
   vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 9  OF menu vOpcion
RETURN

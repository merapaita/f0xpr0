**-------------------------------------------------
** SISTEMA DE CONTABILIDAD PATRIMONIAL 
** EMPRESA : REGION GRAU
** UDEP    : Ing.  Juan Chong  AGOSTO 94 - AGOSTO 95 
** UDEP    : Ing.  Fernando Mu¤oz Cho  / Oswaldo Oliva Carlin SETIEMBRE - 95 
** UDEP    : Javier Frias Gomez   FEBRERO - 1996
** Version : 2.0D
**-------------------------------------------------
SET ESCAPE OFF
CLEAR
CLOSE ALL

CLEAR ALL

SET TALK OFF
SET ECHO OFF

worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )

SET PROCEDURE TO Con_lib

PUBLIC mbloqueo
PUBLIC festado,fmensaje,DESUSE,FinArchivo


DO fox_ambi
SET PATH TO H:\SISRG\DATA96
DESA = " CPT96 " 
IF WorKer
   IF YesNo("Datos de Prueba?")
      SET PATH TO H:\SISFTE\DATAPR                 &&& 
      DESA = " CPT96-PRUEBA " 
   ENDIF
ENDIF
=SavCon()

**-- Definiciones para el Men£  --**
*----------------------------------*
CIA  = " REGION GRAU "
TITU = " Sistema de Contabilidad Patrimonial "+ "("+ALLTRIM(LEFT(SYS(0),AT('#',SYS(0))-1))+")"

**-- Colores
escolor = iscolor()
IF escolor
   SET COLOR TO ,N/W,B
   c_panta = 8
   c_borde = 10
   c_popup = 15
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
@  1, 0,23,80  BOX "°°°°°°°°°" COLOR SCHEME c_fondo
@  1, 1,23,24  BOX "±±±±±±±±±" COLOR SCHEME c_fondo

USE USUARIO IN 0 ORDER USUARIO1
SEEK ALLTRIM(SUBSTR(SYS(0),1,10))
IF FOUND()
    SPAC1  = LEN(ALLT(usuario.nombre))  
    DESUSE = subst(Usuario.Nombre,1,spac1)
 ELSE
    DESUSE =ALLTRIM(SUBSTR(SYS(0),1,10))
ENDIF

spac = (80 -(LEN(CIA)+LEN(TITU)+10))/2
Rotulo1 = CIA + SPACE(spac) + TITU + SPAC(spac) + space(10)

spac = (80 - (LEN(DESA)+LEN(DESUSE)+10))/2
Rotulo2 = DESA + SPACE(spac)+' '+DESUSE+' '+SPAC(spac)+DTOC(DATE())
C    = " "

DO Logos WITH Rotulo1,Rotulo2,50
SAVE SCREEN TO Principal

IF WORKER
   ON ERROR
ENDIF

SAVE SCREEN TO Pantalla
STORE .t. TO vEn_sistema

*-- Clave de acceso
vUser_ID = SPACE(5)
vUser_ID = ALLTRIM(LEFT(SYS(0),15))

IF !FILE( '..\DATA\USUARIO.CDX')
  DO STANdBY WITH 'Error en INDEXA'
ENDIF

USE Usuario ORDER TAG Usuario1  ALIAS Usua
SELE usua
USE

USE IteUsu ORDER TAG IteUsu1   ALIAS IteU
SELE IteU
SET FILT TO sistema='5'
vUsuCla = CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
vAcceso = ''
SEEK vUsuCla
SCAN WHILE vUsuCla = ALLT(IteU.UsuCla)
  vAcceso = vAcceso + ALLT(IteU.Modulo)
ENDSCAN
SET FILT TO
USE


SET CLOCK ON
STORE .t. TO vEn_sistema
STORE .f. TO esc_tecla
= SYS(2002,1)        && Set cursor on

*--- El men£ principal ---**
L_COL  ="W+/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "

IF escolor
   DEFINE POPUP menu FROM 6,2 TO 16,21 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP menu FROM 6,2 TO 16,21        COLOR SCHEME C_POPUP
ENDIF

DEFINE BAR  1 OF menu PROMPT ' \<Documentos      '
DEFINE BAR  2 OF menu PROMPT ' \<Procesos        '
DEFINE BAR  3 OF menu PROMPT ' \<Reportes        '
DEFINE BAR  4 OF menu PROMPT ' \<Mantenimiento   '
DEFINE BAR  5 OF menu PROMPT ' \<Utilitarios     '
DEFINE BAR  6 OF menu PROMPT ' Datos \<Iniciales '
DEFINE BAR  7 OF menu PROMPT ' \<Especiales      '
DEFINE BAR  8 OF menu PROMPT ' \<Salida           '

*- Documentos
IF escolor
   DEFINE POPUP pop_01 FROM 5,24 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_01 FROM 5,24 COLOR SCHEME c_popup
ENDIF

DEFINE BAR  01  OF  pop_01 PROMPT '\<Documentos  Varios    '         MESSAGE ''
DEFINE BAR  02  OF  pop_01 PROMPT '\<Notas de Contabilidad '         MESSAGE ''
DEFINE BAR  03  OF  pop_01 PROMPT 'N/C  B. \<Constructivo  '         MESSAGE ''
DEFINE BAR  04  OF  pop_01 PROMPT '\<Listados de Control  '         MESSAGE ''
DEFINE BAR  05  OF  Pop_01 PROMPT '\-'
DEFINE BAR  06  OF  pop_01 PROMPT 'De \<Abastecimiento    '         MESSAGE ''
DEFINE BAR  07  OF  pop_01 PROMPT 'De \<Tesorer¡a         '         MESSAGE ''
DEFINE BAR  08  OF  pop_01 PROMPT 'De C. \<Presupuestal   '         MESSAGE ''
DEFINE BAR  09  OF  pop_01 PROMPT '\- '         MESSAGE ''
DEFINE BAR  10  OF  pop_01 PROMPT 'Hist¢rico de Conta\<Bilidad'       MESSAGE ''
DEFINE BAR  11  OF  pop_01 PROMPT 'Consulta Notas Contab\<ilidad'     MESSAGE 'Consultas de Notas de Contabilidad'

*- Listado de Errores 
  IF Escolor
     DEFINE POPUP pop_013 FROM 06,52  SHADOW COLOR &l_col
  ELSE
     DEFINE POPUP pop_013 FROM 06,52  to 12,75  COLOR SCHEME c_popup
  ENDIF
  
  DEFINE BAR  1 OF pop_013  PROMPT 'Consistencia \<General        ' MESSAGE ''
  DEFINE BAR  2 OF pop_013  PROMPT 'Consistencia \<Detallada      ' MESSAGE ''
  DEFINE BAR  3 OF pop_013  PROMPT '\<Listado de errores          ' MESSAGE ''
  DEFINE BAR  4 OF pop_013  PROMPT '\<Errores por tipo de auxiliar' MESSAGE ''
  DEFINE BAR  5 OF pop_013  PROMPT 'Errores por C¢digo de \<Obra  ' MESSAGE ''
  DEFINE BAR  6 OF pop_013  PROMPT 'Errores por Tipo \<Financiero ' MESSAGE ''
  DEFINE BAR  7 OF pop_013  PROMPT '\<N/C por Tipo de Documento   ' MESSAGE ''
  
*  DEFINE BAR  7 OF pop_013  PROMPT '\-'
*  DEFINE BAR  8 OF pop_013  PROMPT 'Listado de O\<peraciones      ' MESSAGE 'Movimientos de Tesorer¡a'



*-- Documentos de Abastecimiento
  IF Escolor
     DEFINE POPUP pop_014 FROM 07,52  SHADOW COLOR &l_col
  ELSE
     DEFINE POPUP pop_014 FROM 07,52  COLOR SCHEME c_popup
  ENDIF
 
DEFINE BAR  01  OF  pop_014 PROMPT '\<Pecosa                '         MESSAGE ''
DEFINE BAR  02  OF  pop_014 PROMPT '\<Orden de Compra       '         MESSAGE ''
DEFINE BAR  03  OF  pop_014 PROMPT 'Orden de \<Servicio     '         MESSAGE ''
DEFINE BAR  04  OF  pop_014 PROMPT 'P¢liza de En\<Trada     '         MESSAGE ''
DEFINE BAR  05  OF  pop_014 PROMPT 'P¢liza de Sa\<Lida      '         MESSAGE ''
DEFINE BAR  06  OF  pop_014 PROMPT 'Parte de A\<Nulaci¢n    '         MESSAGE ''
DEFINE BAR  07  OF  pop_014 PROMPT 'Parte de \<Rebaja       '         MESSAGE ''
*-   

*-- Documentos de Tesorer¡a
  IF Escolor
     DEFINE POPUP pop_015 FROM 07,52  SHADOW COLOR &l_col
  ELSE
     DEFINE POPUP pop_015 FROM 07,52  COLOR SCHEME c_popup
  ENDIF

DEFINE BAR  01  OF  pop_015 PROMPT '\<Comprobante de Pago   '         MESSAGE ''
DEFINE BAR  02  OF  pop_015 PROMPT '\<Recibo de Ingreso     '         MESSAGE ''
DEFINE BAR  03  OF  pop_015 PROMPT '\<Movimientos Bancarios '         MESSAGE ''
DEFINE BAR  04  OF  pop_015 PROMPT '\<Extractos Bancarios   '         MESSAGE ''
DEFINE BAR  05  OF  pop_015 PROMPT '\<Autorizaciones MEF    '         MESSAGE ''
DEFINE BAR  06  OF  pop_015 PROMPT '\<Distrib. Aut. Giro    '         MESSAGE ''
*-

*- Documentos de C.presupuestal
  IF Escolor
     DEFINE POPUP pop_016 FROM 11,52  SHADOW COLOR &l_col
  ELSE
     DEFINE POPUP pop_016 FROM 11,52  COLOR SCHEME c_popup
  ENDIF

DEFINE BAR  01  OF  pop_016 PROMPT 'Hoja de \<Control       '         MESSAGE ''
DEFINE BAR  02  OF  pop_016 PROMPT 'Hoja de \<Modificaci¢n  '         MESSAGE ''
*-


*- Procesos
IF escolor
   DEFINE POPUP pop_02 FROM 6,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02 FROM 6,26 TO 12,50        COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02 PROMPT '\<Proceso DOCS X Lotes ' MESSAGE ''
DEFINE BAR 2  OF  Pop_02 PROMPT '\-'
DEFINE BAR 3  OF  pop_02 PROMPT '\<Mayorizaci¢n          ' MESSAGE ''
DEFINE BAR 4  OF  pop_02 PROMPT 'Mayori\<Z./ Constructivo ' MESSAGE ''
DEFINE BAR 5  OF  pop_02 PROMPT 'Proceso de \<Ajuste ACM  ' MESSAGE ''
DEFINE BAR 6  OF  pop_02 PROMPT '\<Cierre del Mes        ' MESSAGE ''
DEFINE BAR 7  OF  pop_02 PROMPT '\<Re-procesos           ' MESSAGE ''
DEFINE BAR 8  OF  pop_02 PROMPT '\<Generaci¢n EE.FF.     ' MESSAGE ''
DEFINE BAR 9  OF  pop_02 PROMPT 'An \<Lisis de Cuenta    ' MESSAGE ''

IF escolor
   DEFINE POPUP pop_021 FROM 7,52 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_021 FROM 7,52 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_021 PROMPT '\<Hojas de Control     ' MESSAGE ''
DEFINE BAR 2  OF  pop_021 PROMPT '\<Recibos de Ingreso   ' MESSAGE ''
DEFINE BAR 3  OF  pop_021 PROMPT '\<Comprobantes / Pago  ' MESSAGE ''
DEFINE BAR 4  OF  pop_021 PROMPT '\<Boletas de Deposito  ' MESSAGE ''
DEFINE BAR 5  OF  pop_021 PROMPT '\<Notas de Cargo       ' MESSAGE ''
DEFINE BAR 6  OF  pop_021 PROMPT 'Notas de \<Abono       ' MESSAGE ''
DEFINE BAR 7  OF  pop_021 PROMPT 'NOC \<Tesorer¡a        ' MESSAGE ''
DEFINE BAR 8  OF  pop_021 PROMPT 'H.\<Anulaci¢n de Cheque' MESSAGE ''
DEFINE BAR 9  OF  pop_021 PROMPT 'Ordenes  de \<Servicio ' MESSAGE ''
DEFINE BAR 10 OF  pop_021 PROMPT '\<Facturas             ' MESSAGE ''

*- Reportes
IF escolor
   DEFINE POPUP pop_03 FROM 04,24 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_03 FROM 04,24 TO 20,48  COLOR SCHEME c_popup
ENDIF

DEFINE BAR 01  OF  pop_03 PROMPT '\<Diario                  ' MESSAGE ''
DEFINE BAR 02  OF  pop_03 PROMPT 'Diario \<Resumen          ' MESSAGE ''
DEFINE BAR 03  OF  pop_03 PROMPT 'Diari\<O Contabilidad     ' MESSAGE ''
DEFINE BAR 04  OF  pop_03 PROMPT '\<Mayor                   ' MESSAGE ''
DEFINE BAR 05  OF  pop_03 PROMPT '\<Balance                ' MESSAGE ''
DEFINE BAR 06  OF  pop_03 PROMPT 'Libro Ba\<Nco             ' MESSAGE ''
DEFINE BAR 07  OF  pop_03 PROMPT 'L\<Ibro Caja              ' MESSAGE ''
DEFINE BAR 08  OF  pop_03 PROMPT 'Conso\<Lidado Bancario    ' MESSAGE ''
DEFINE BAR 09  OF  pop_03 PROMPT 'Auxiliar E\<Standard     ' MESSAGE ''
DEFINE BAR 10  OF  pop_03 PROMPT '\<Analisis de cuentas    ' MESSAGE ''
DEFINE BAR 11  OF  pop_03 PROMPT 'In\<ventario F¡sico       ' MESSAGE ''
DEFINE BAR 12  OF  pop_03 PROMPT 'Estados \<Financieros    ' MESSAGE ''
DEFINE BAR 13  OF  pop_03 PROMPT 'Asiento Centrali\<Zaci¢n  ' MESSAGE ''
DEFINE BAR 14  OF  pop_03 PROMPT 'C‚d\<Ula de Ajuste EE.FF ' MESSAGE ''


*- An lisis de Cuentas 
IF escolor
     DEFINE POPUP pop_313 FROM 13,52 SHADOW COLOR &l_col
ELSE
    DEFINE POPUP pop_313 FROM 13,52  TO 23,75  COLOR SCHEME c_popup     
ENDIF
DEFINE BAR  1 OF pop_313  PROMPT 'Por \<Proveedor'  MESSAGE ''
DEFINE BAR  2 OF pop_313  PROMPT 'Por \<Obra     '  MESSAGE ''

*- Cedulas de Ajuste a los EE.FF
 IF escolor
     DEFINE POPUP pop_0318 FROM 10,52 SHADOW COLOR &l_col
 ELSE
     DEFINE POPUP pop_0318 FROM 10,52  TO 23,75  COLOR SCHEME c_popup     
 ENDIF
 DEFINE BAR  1 OF pop_0318  PROMPT 'Por \<Cuentas       '  MESSAGE ''
 DEFINE BAR  2 OF pop_0318  PROMPT 'Por \<Obra          '  MESSAGE ''
 DEFINE BAR  3 OF pop_0318  PROMPT '\<Anexos a los EE.FF'  MESSAGE ''

*- Estados Financieros

  IF escolor
     DEFINE POPUP pop_315 FROM 06,52 SHADOW COLOR &l_col
  ELSE
     DEFINE POPUP pop_315 FROM 06,52  TO 23,75  COLOR SCHEME c_popup     
  ENDIF

DEFINE BAR  1 OF pop_315  PROMPT 'AF1 \<Inversiones             '  MESSAGE 'Inversiones'
DEFINE BAR  2 OF pop_315  PROMPT 'AF2 Inmueble, \<Maq. y Equipo '  MESSAGE 'Inmueble, Maquinaria y Equipo'
DEFINE BAR  3 OF pop_315  PROMPT 'AF3 \<Depreciaci¢n Acumulada  '  MESSAGE 'Depreciaci¢n, Agotamiento y Amortizaci¢n Acumulada'
DEFINE BAR  4 OF pop_315  PROMPT 'AF4 \<Otras Cuentas Activo    '  MESSAGE 'Otras Cuentas del Activo'
DEFINE BAR  5 OF pop_315  PROMPT 'AF5 \<Provisi¢n Benef.Sociales'  MESSAGE 'Provisiones para Beneficios Sociales'
DEFINE BAR  6 OF pop_315  PROMPT 'AF6 In\<gresos Diferidos      '  MESSAGE 'Ingresos Diferidos'
DEFINE BAR  7 OF pop_315  PROMPT 'AF7 \<Hacienda Nac. Adicional '  MESSAGE 'Hacienda Nacional Adicional'
DEFINE BAR  8 OF pop_315  PROMPT 'AF8 Ana\<lisis.Cuentas Enlace '  MESSAGE 'An lisis de Cuenta de Enlace, Ingresos, Gtos. Extr. y de Ejerc. Anteriores' 
DEFINE BAR  9 OF pop_315  PROMPT 'F1  \<Balance General         '  MESSAGE 'Balance General'
DEFINE BAR 10 OF pop_315  PROMPT 'F2  \<Estado de Gesti¢n       '  MESSAGE 'Estado de Gesti¢n'
DEFINE BAR 11 OF pop_315  PROMPT 'F3  Estado Cambios Patr.\<Neto'  MESSAGE 'Estado de Cambios en el Patrimonio Neto'

*- Balances 
  IF Escolor
     DEFINE POPUP pop_034 FROM 04,52  SHADOW COLOR &l_col
  ELSE
     DEFINE POPUP pop_034 FROM 04,52  to 12,75  COLOR SCHEME c_popup
  ENDIF
  
  DEFINE BAR  1 OF pop_034  PROMPT '\<Comprobaci¢n       ' MESSAGE ''
  DEFINE BAR  2 OF pop_034  PROMPT 'C\<Omprobaci¢n 2     ' MESSAGE ''
  DEFINE BAR  3 OF pop_034  PROMPT '\<General            ' MESSAGE ''
  DEFINE BAR  4 OF pop_034  PROMPT 'Cons\<Tructivo       ' MESSAGE ''
 * DEFINE BAR  5 OF pop_034  PROMPT 'Const\<Ructivo 3     ' MESSAGE ''
  
*- Mantenimiento

IF escolor
   DEFINE POPUP pop_04 FROM 09,24 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_04 FROM 09,24 TO 15,53        COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_04 PROMPT '\<Plan Contable              '  MESSAGE ''
DEFINE BAR 2  OF  pop_04 PROMPT '\<Auxiliares                '  MESSAGE ''
DEFINE BAR 3  OF  pop_04 PROMPT '\<Tablas                    '  MESSAGE ''
DEFINE BAR 4  OF  pop_04 PROMPT '\<Fechas del Sistema         '  MESSAGE ''
DEFINE BAR 5  OF  Pop_04 PROMPT '\<Mantenimiento Correlativos ' MESSAGE ''

*-  Tablas
IF escolor
   DEFINE POPUP pop_04d FROM 03,55 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP pop_04d FROM 03,55 COLOR SCHEME C_POPUP
ENDIF

DEFINE BAR  1 OF pop_04d PROMPT 'Tipos \<Auxiliares         '
DEFINE BAR  2 OF pop_04d PROMPT '\<Par metros               '
DEFINE BAR  3 OF pop_04d PROMPT 'c\<Entros de Costo         '
DEFINE BAR  4 OF pop_04d PROMPT 'Tipos de \<Documentos      '
DEFINE BAR  5 OF pop_04d PROMPT 'Estructura \<Balance       '
DEFINE BAR  6 OF pop_04d PROMPT 'Estructura \<Gan.perd.     '
DEFINE BAR  7 OF pop_04d PROMPT '\<Clasificador de Gasto    '
DEFINE BAR  8 OF pop_04d PROMPT 'Tipos \<Financieros        '
DEFINE BAR  9 OF pop_04d PROMPT 'Tipos Financieros \<95     '
DEFINE BAR 10 OF pop_04d PROMPT 'Estru\<Tura  E.E.F.F.      '
DEFINE BAR 11 OF pop_04d PROMPT 'Tipos Financ. A\<Utom ticos'
DEFINE BAR 12 OF pop_04d PROMPT 'As\<Ientos Autom ticos     '
DEFINE BAR 13 OF pop_04d PROMPT 'A. Autom ticos x \<Retenci¢n'
DEFINE BAR 14 OF pop_04d PROMPT '\<Obras                    '
DEFINE BAR 15 OF pop_04d PROMPT 'Factores A.C.\<M.          '
DEFINE BAR 16 OF pop_04d PROMPT 'Asie\<ntos Aut. x Partidas '

*- Utilitarios
IF escolor
   DEFINE POPUP pop_05 FROM 09,24 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_05 FROM 09,24 COLOR SCHEME c_popup
ENDIF
DEFINE BAR  1  OF Pop_05 PROMPT '\<Indexar Archivos              '
DEFINE BAR  2  OF Pop_05 PROMPT '\<Backup de los Archivos        '
DEFINE BAR  3  OF Pop_05 PROMPT 'De\<Puraci¢n de Backups         '
DEFINE BAR  4  OF Pop_05 PROMPT '\<Conectarse  al   Print-Server  '
DEFINE BAR  5  OF Pop_05 PROMPT '\<Desconectarse del Print-Server '
DEFINE BAR  6  OF Pop_05 PROMPT 'Impresi¢n de \<Archivos         '
DEFINE BAR  7  OF Pop_05 PROMPT '\-'
DEFINE BAR  8  OF Pop_05 PROMPT '\<Usuarios CPT96                 '
DEFINE BAR  9  OF Pop_05 PROMPT '\<Seguridad                      ' MESSAGE '® Acceso Restringido ¯'

* DEFINE BAR  9  OF Pop_05 PROMPT '\-'
* DEFINE BAR  10 OF Pop_05 PROMPT 'Ir al D\<OS         ' MESSAGE '®® Escriba EXIT para regresar ¯¯'
* DEFINE BAR  11 OF Pop_05 PROMPT 'Ir al \<Word Perfect' MESSAGE ''

*- Datos Iniciales
IF escolor
   DEFINE POPUP Pop_06 FROM 11,26 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP Pop_06 FROM 11,26 TO 16,53 COLOR SCHEME C_POPUP
ENDIF

DEFINE BAR  1 OF Pop_06 PROMPT '\<Saldos y Sumas          '
DEFINE BAR  2 OF Pop_06 PROMPT 'Saldos de Apertura \<EE.FF.' 
DEFINE BAR  3 OF Pop_06 PROMPT 'Saldos de \<Auxiliar      '
DEFINE BAR  4 OF Pop_06 PROMPT 'Saldos de \<Obras         '
DEFINE BAR  5 OF Pop_06 PROMPT '\<Fin de A¤o              '
DEFINE BAR  6 OF Pop_06 PROMPT '\<Datos Referenciales EE.FF'

*- Consultas y Reportes Especiales
IF escolor
   DEFINE POPUP Pop_07 FROM 06,26 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP Pop_07 FROM 06,26 COLOR SCHEME C_POPUP
ENDIF

DEFINE BAR  1 OF Pop_07 PROMPT '\<Listado Hojas de Control'
DEFINE BAR  2 OF Pop_07 PROMPT 'Listado de \<Anticipos    '
DEFINE BAR  3 OF Pop_07 PROMPT '\<Movimiento de Cuentas'
DEFINE BAR  4 OF Pop_07 PROMPT 'Movimientos por \<Proveedor'
DEFINE BAR  5 OF Pop_07 PROMPT '\-'
DEFINE BAR  6 OF Pop_07 PROMPT '\<Comprobantes de Pago    '
DEFINE BAR  7 OF Pop_07 PROMPT '\<Hojas de Control        '
DEFINE BAR  8 OF Pop_07 PROMPT 'Ordenes de \<Servicio     '
DEFINE BAR  9 OF Pop_07 PROMPT 'O\<Rdenes de Compra       '
DEFINE BAR 10 OF Pop_07 PROMPT 'R\<Ecibos de Ingreso      '

ON SELECTION POPUP menu DO ACT_MENUS

IF !vUser_ID$'FMVLCDFMCJFG'
	DO ABREPAS
ENDIF	

IF  !escolor
  @ 04, 01, 20, 22 BOX "ÛÛÛÛÛÛÛÛÛ" COLOR SCHEME c_fondo
  @ 05, 02  FILL TO  21, 23    COLOR BR+/B
  @ 05, 02  FILL TO  20, 22    COLOR BR/B
ENDIF

**- La sombra del principal
**- SAVE SCREEN TO principal
ACTIVATE POPUP menu
DO WHILE  EMPTY(PROMPT())
   ACTIVATE POPUP menu
ENDDO
*-Cerrar todo...
ON KEY
=BorCon()
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
     ON SELECTION POPUP pop_03  DO  menu_03
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
     DO Salmenu
ENDCASE

RESTORE SCREEN FROM principal
RETURN


PROCEDURE menu_01   && Comprobantes
*----------------

  choice=BAR()
  DO CASE
    CASE choice = 1
      DO COMING3
    CASE choice = 2
      DO NOTCON3
    CASE choice = 3
      DO NOTCON3
    CASE choice = 4
      ON SELECTION POPUP pop_013 DO menu_013
      ACTIVATE POPUP pop_013
    CASE choice = 6  
      ON SELECTION POPUP pop_014 DO menu_014
      ACTIVATE POPUP pop_014
    CASE choice = 7  
      ON SELECTION POPUP pop_015 DO menu_015
      ACTIVATE POPUP pop_015
    CASE choice = 8  
      ON SELECTION POPUP pop_016 DO menu_016
      ACTIVATE POPUP pop_016
    CASE choice = 10   && Hist¢rico de Contabilidad 
      DO HComing  
    CASE choice = 11   && Hist¢rico de Contabilidad 
      DO CONS_NC 
      
  ENDCASE
    
  IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_01  DO menu_01
  ENDIF
  SHOW POPUP POP_01,menu
  HIDE WINDOW ALL
RETURN


PROCEDURE menu_013   && Listado de Errores
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
      DO ConCom
    CASE choice = 2
      *DO ConDet
      DO ConDet1
    CASE choice = 3
      DO CompErr      
    CASE choice = 4
      DO ErrAux
    CASE choice = 5
      DO ErrObr
    CASE choice = 6
      DO Errtfin
    CASE choice = 7
      DO nctipdoc
      
   *CASE choice = 8
   *DO Diaopesc
  ENDCASE
  
  IF !escolor
  	RESTORE SCREEN FROM principal
  ENDIF
  SHOW POPUP menu, pop_01,pop_013
  RETURN


PROCEDURE menu_014   && Documentos de Abastecimientos
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
      DO Con_Pec  WITH 1    && Pecosa
    CASE choice = 2
      DO Con_Ocom   && Orden de Compra
    CASE choice = 3
      DO Con_Oser   && Orden de Servicio
    CASE choice = 4
      DO Con_Pole   && Poliza de Entrada
    CASE choice = 5
      DO Con_Pols   && Poliza de Salida
    CASE choice = 6
      IF yesno("Partes de Anulaci¢n de O/C:[Si] o de O/S:[No] ? ")
         DO Con_ParA   WITH 1  && Parte anulaci¢n O/C
      ELSE 
         DO Con_ParA   WITH 2  && Parte anulaci¢n O/S
      ENDIF   
    CASE choice = 7
     IF yesno("Partes de Rebaja de O/C:[Si] o de O/S:[No] ? ")
      	DO Con_ParR   WITH 1 && Parte rebaja DE o/c
     ELSE
        DO Con_ParR   WITH 2 && Parte rebaja DE o/S
     ENDIF 	
      
  ENDCASE
  
  IF !escolor
  	RESTORE SCREEN FROM principal
  ENDIF
  SHOW POPUP menu, pop_01,pop_014
  RETURN


PROCEDURE menu_015   && Documentos de Tesorer¡a
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
      DO ConCPAU2    && Comprobante de Pago
    CASE choice = 2
      DO ConRIAu2    && Recibo de Ingreso
    CASE choice = 3
      DO ConMbAu2    && Movimiento de Bancos
    CASE choice = 4
      DO ConExtBc    && Extractos Bancarios
    CASE choice = 5
      DO ConAutg1    && Autorizaciones MEF
    CASE choice = 6
      DO ConAutg3    && Distribuci¢n Aut. Giro.
  ENDCASE
  
  IF !escolor
  	RESTORE SCREEN FROM principal
  ENDIF
  SHOW POPUP menu, pop_01,pop_015
  RETURN


PROCEDURE menu_016   && Documentos de C.Presupuestal
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
      DO Con_HojC WITH 1   && Hojas de Control Presupuestal
    CASE choice = 2
      DO Con_HojM          && Hojas de Modificaci¢n
  ENDCASE
  
  IF !escolor
  	RESTORE SCREEN FROM principal
  ENDIF
  SHOW POPUP menu, pop_01,pop_016
  RETURN

      
PROCEDURE menu_02   && Procesos
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
         ON SELECTION POPUP pop_021 DO menu_021
         ACTIVATE POPUP pop_021
    CASE choice = 3
      DO Mayorini
    CASE choice = 4
      do constru1
      * La £ltima version del Programa es MayoCons
    CASE choice = 5  && ACM
      DO Ajusteni
    CASE choice = 6
      DO CierrMES
    CASE choice = 7
      DO ReproCon
    CASE choice = 8
      DO EEFFINI3
    CASE choice = 9
      DO Genanal
  ENDCASE
  IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02  DO menu_02
  ENDIF
  SHOW POPUP pop_02,menu
  HIDE WINDOW ALL
  RETURN
  
  PROCEDURE menu_021  && Integra Lotes
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
         Do IntHC    && Integra Hojas de Control 
    CASE choice = 2
         Do IntRI    && Integra Recibos de Ingreso     
    CASE choice = 3
         DO IntCp1    && Integra Comprobantes de Pago
    CASE choice = 4
         DO IntMBD1    && Integra Movimientos Bancarios B/D 
    CASE choice = 5
         DO IntMNC1   && Integra Movimientos Bancarios  N/C
    CASE choice = 6
         DO IntMNA1   && Integra Movimientos Bancarios  N/A
    CASE choice = 7
         DO IntMNOC  && Integra Movimientos Bancarios  NOC
    CASE choice = 8
         DO IntHAC   && Integra HOJAS DE Anulacion de Cheque
    CASE choice = 9         
         DO IntOS    && Integra Ordenes de Servicio
    CASE choice = 10
         DO Intfact  && Integra facturas
  ENDCASE
  
  IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_021  DO menu_021
  ENDIF
  SHOW POPUP pop_021,menu
  HIDE WINDOW ALL
RETURN



PROCEDURE menu_03   && Reportes
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1    && Diario
      DO LibDiar
    CASE choice = 2    && Diario resumen
      *DO LibDir1
      * DO LibDir
      DO LibDir2
    CASE choice = 3   && Diario de Contabilidad
      DO DiarCont      
    CASE choice = 4    && Mayor
      *DO RepMay2    con este programa aparece un submenu 
      *DO MAYOR3
       DO LIBMAYOR
    CASE choice = 5    && Balance
         ON SELECTION POPUP pop_034 DO menu_034
         ACTIVATE POPUP pop_034
         
*    CASE choice = 5    && EE.GG.PP
*      DO RepEGP
*    CASE choice = 6    && Resumen de costos
*      DO GasCto
*    CASE choice = 7    && Balance de gastos
*      DO BalGas

    CASE choice = 6    && Libro Banco 
      * DO LibBan
      * DO LibBan1
      DO LibbanTs
    CASE choice = 7    && Libro Caja
      DO LibCaj
    CASE choice = 8   && Consolidado bancario
      DO ConBan
*    CASE choice = 11   && Registros oficiales
*      DO RegOfi
    CASE choice = 9   && Auxiliar estandard
      DO AuxSta
    CASE choice = 10   && Analisis de cuentas
         * DO ANACUE2
         * DO ANACUE3 FMC
         * DO ANALISIS
          ON SELECTION POPUP pop_313 DO menu_313
          ACTIVATE POPUP pop_313
    CASE choice = 11   && Inventario F¡sico
      DO InvFis
    CASE choice = 12   && Reportes AF's-F's
         ON SELECTION POPUP pop_315 DO menu_315
         ACTIVATE POPUP pop_315
    CASE choice = 13   && Asiento de Centralizaci¢n
      DO AsieCent
    CASE choice = 14   && Cedulas de Ajuste 
         ON SELECTION POPUP pop_0318 DO menu_0318
         ACTIVATE POPUP pop_0318
      * DO Ceduaju       && A los Estados Financieros  
  ENDCASE
  IF !escolor
     RESTORE SCREEN FROM principal
     ON SELECTION POPUP pop_03  DO menu_03
  ENDIF
  SHOW POPUP pop_03,menu
  HIDE WINDOW ALL
RETURN

PROCEDURE menu_034    && Balance
*----------------------
 STORE BAR() TO choice
 DO CASE
    CASE choice = 1
       DO REPBAL2
       * REPBAL2 ES LA ULTIMA VERSION CUANDO FUNCIONE LA MAYORIZACION
       * GENERAL 
       *--------------------------
    CASE choice = 2
        DO REPLANC2
       *DO REPLAJFG
       * REPLANC2 ES LA ULTIMA VERSION CUANDO FUNCIONE LA MAYORIZACION
       * GENERAL 
       
    CASE choice = 3
       DO Balgen       && Balance General
    CASE choice = 4
*       DO BalCon2       && Balance constructivo a dos  (02) Digitos
       * DO BalCon4      && Balance Constructivo FMC de 02   D¡gitos
       * DO BalCon7      && Balance Constructivo FMC de 02   D¡gitos
       DO RCONSTRU       && VERSION DE JFG
       * DO BalCon6
       * REPLANC2 ES LA ULTIMA VERSION CUANDO FUNCIONE LA MAYORIZACION
       * GENERAL 
*    CASE choice = 5
*      DO BalCon3      && Balance constructivo a tres (03) Digitos          
*       DO BalCon4    && Balance Constructivo FMC de A Nivel De Detalle 
 ENDCASE
  IF !escolor
    RESTORE SCREEN FROM principal
 ENDIF
 SHOW POPUP menu, pop_03,pop_034
RETURN

PROCEDURE menu_0318   && C‚dulas de Ajuste 
*----------------------
 STORE BAR() TO choice
 DO CASE
    CASE choice = 1
         DO Ceduaju          
    CASE choice = 2
         DO Cedajobr          
    CASE choice = 3
         DO ced_aju
 ENDCASE
  IF !escolor
    RESTORE SCREEN FROM principal
 ENDIF
 SHOW POPUP menu, pop_03,pop_0318
 RETURN

PROCEDURE menu_313  && An lisis de Cuentas 
*------------------
STORE BAR() TO choice
    DO CASE
       CASE choice = 1
         * DO Anacue   && Por Cuenta
         DO Analisis   && Por Cuenta
       CASE choice = 2
         DO Analisob   && Por Obra 
    ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
 ENDIF
 SHOW POPUP menu, pop_03,pop_313
RETURN


PROCEDURE menu_315    && Estados Financieros
*------------------

    STORE BAR() TO choice
    DO CASE
       CASE choice = 1
        DO RepEEFF WITH 'AF-1','REPAF1'
       CASE choice = 2
         DO RepEEFF WITH 'AF-2','REPAF2'       
       CASE choice = 3
         DO RepEEFF WITH 'AF-3','REPAF3'       
       CASE choice = 4
         DO RepEEFF WITH 'AF-4','REPAF4'       
       CASE choice = 5
         DO RepEEFF WITH 'AF-5','REPAF5'       
       CASE choice = 6
         DO RepEEFF WITH 'AF-6','REPAF6'       
       CASE choice = 7
         DO RepEEFF WITH 'AF-7','REPAF7M'       
       CASE choice = 8
         DO RepEEFF WITH 'AF-8','REPAF8'       
       CASE choice = 9
 		 DO RepEEFF WITH 'F-1','EF-F1'       
       CASE choice = 10
         SET DECIMAL TO 0
             DO RepEEFF WITH 'F-2','REPF2'       
         SET DECIMAL TO 2
       CASE choice = 11
			DO RepEEFF WITH 'F-3','REPF3'       
    ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
 ENDIF
 SHOW POPUP menu, pop_03,pop_315
RETURN

PROCEDURE menu_04   && Mantenimientos
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
       DO Cuentas
    CASE choice = 2
       * DO ManAux1
       DO ManAux2
    CASE choice = 3
       ON SELECTION POPUP pop_04d  DO  menu_04d
       ACTIVATE POPUP pop_04d
    CASE BAR() = 4
        DO MESANO          &&Mantenimiento de Fecha para el trabajo en Linea 
    CASE BAR() = 5
        DO CORREL          &&Mantenimiento de Correlativos 
     ENDCASE
  IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_04  DO menu_04
  ENDIF
  SHOW POPUP pop_04,menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE menu_04d
*-----------------
  DO CASE
    CASE BAR()=1
       DO TipAux
    CASE BAR()=2
       DO MANPAR1 WITH 'OPERAT',' Descripci¢n ','Par metros del Sistema',7
    CASE BAR()=3
       DO CtoCto
    CASE BAR()=4
       DO TipDoc
    CASE BAR()=5
       DO EstBal
    CASE BAR()=6
       DO EstEgp
    CASE BAR()=7
       DO Clasific
    CASE BAR()=8
       DO Tipfin
    CASE BAR()=9
       DO TFin95
    CASE BAR()=10
       DO EstFin
    CASE BAR()=11
       DO AutFin
    CASE BAR()=12
       DO AsiAut
    CASE BAR()=13
       DO Asiret
    CASE BAR()=14
       DO CptObra
    CASE BAR()=15
       DO Factores
    CASE BAR()=16
       DO AsiClasi
  ENDCASE
  IF !escolor
     RESTORE SCREEN FROM principal
     ON SELECTION POPUP pop_04d  DO menu_04d
  ENDIF
  SHOW POPUP pop_04d, pop_04, menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE menu_05   && Utilitarios
*----------------
 DO CASE
    CASE BAR() = 1
         DO Indexa
    CASE BAR() = 2
         DO Backup
    CASE BAR() = 3
         DO DepBK
    CASE BAR() = 4
         RUN CAPTURE /Q=PRINTQ_0 /TI=0 /No Banner /No Formfeed
         RESTORE  SCREEN FROM Principal
    CASE BAR() = 5
         RUN ENDCAP
         RESTORE  SCREEN FROM Principal
    CASE BAR() = 6
         DO ImpArc
    CASE BAR() = 8
         DO UsersCPT
    CASE BAR() = 9
         DO segCPT
  ENDCASE
  RETURN
  
PROCEDURE menu_06   && Datos Iniciales
*----------------
  DO CASE
     CASE BAR()=1
          DO Saldos
     CASE BAR()=2
	      OK=VE_PASSW('FELPA')
          IF OK
             DO Saldini 
          ENDIF
     CASE BAR()=3
          DO PSalAux
     CASE BAR()=4
          DO PSalObr
     CASE BAR()=5
          * DO Acumula
          DO Finano
     CASE BAR()=6
          DO RESEEFF
          
          
  ENDCASE
  IF !escolor
     RESTORE SCREEN FROM principal
     ON SELECTION POPUP pop_06  DO menu_06
  ENDIF
  SHOW POPUP pop_06,menu
  HIDE WINDOW ALL
RETURN


PROCEDURE menu_07   && Especiales
*----------------
  DO CASE
     CASE BAR()=1
          DO REP_HC
     CASE BAR()=2
          DO ANTICIP
     CASE BAR()=3
          DO seg_cta
     CASE BAR()=4
          DO LIQUIUSU
     CASE BAR()=6
          DO CE_COMPA
     CASE BAR()=7
          DO CE_HJCON   
     CASE BAR()=8
          DO CE_ORDSE
     CASE BAR()=9
          DO CE_ORDCO 
     CASE BAR()=10
          DO CE_REGRI
  ENDCASE
  IF !escolor
     RESTORE SCREEN FROM principal
     ON SELECTION POPUP pop_07  DO menu_07
  ENDIF
  SHOW POPUP pop_07,menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE salmenu
*----------------
  STORE  YESNO(' Estamos Terminando la sesi¢n ') TO in_regrau
  IF in_regrau
     CLOSE DATA
     CLEAR WINDOWS
     RELEASE ALL
     CLEAR
     IF !worker
        QUIT
     ENDIF
     =BorCon()
     CANCEL
  ELSE
     RETURN
  ENDIF


PROCEDURE ABREPAS
*----------------
**- Accesos a las opciones; seg£n la seguridad especificada
SET SKIP OF BAR 1  OF pop_01 !'A' $vAcceso
SET SKIP OF BAR 2  OF pop_01 !'B' $vAcceso
SET SKIP OF BAR 3  OF pop_01 !'C' $vAcceso
SET SKIP OF BAR 4  OF pop_01 !'D' $vAcceso
SET SKIP OF BAR 6  OF pop_01 !'E' $vAcceso
SET SKIP OF BAR 7  OF pop_01 !'F' $vAcceso
SET SKIP OF BAR 8  OF pop_01 !'G' $vAcceso
SET SKIP OF BAR 10 OF pop_01 !'H' $vAcceso
SET SKIP OF BAR 11 OF pop_01 !'\' $vAcceso

SET SKIP OF BAR 1  OF pop_02 !'I' $vAcceso
SET SKIP OF BAR 3  OF pop_02 !'J' $vAcceso
SET SKIP OF BAR 4  OF pop_02 !'K' $vAcceso
SET SKIP OF BAR 5  OF pop_02 !'L' $vAcceso
SET SKIP OF BAR 6  OF pop_02 !'M' $vAcceso
SET SKIP OF BAR 7  OF pop_02 !'N' $vAcceso
SET SKIP OF BAR 8  OF pop_02 !'O' $vAcceso
SET SKIP OF BAR 9  OF pop_02 !'P' $vAcceso

SET SKIP OF BAR 1  OF pop_03 !'Q' $vAcceso
SET SKIP OF BAR 2  OF pop_03 !'R' $vAcceso
SET SKIP OF BAR 3  OF pop_03 !'S' $vAcceso
SET SKIP OF BAR 4  OF pop_03 !'T' $vAcceso
SET SKIP OF BAR 5  OF pop_03 !'U' $vAcceso
SET SKIP OF BAR 6  OF pop_03 !'V' $vAcceso
SET SKIP OF BAR 7  OF pop_03 !'W' $vAcceso
SET SKIP OF BAR 8  OF pop_03 !'X' $vAcceso
SET SKIP OF BAR 9  OF pop_03 !'Y' $vAcceso
SET SKIP OF BAR 10 OF pop_03 !'Z' $vAcceso
SET SKIP OF BAR 11 OF pop_03 !'a' $vAcceso
SET SKIP OF BAR 12 OF pop_03 !'b' $vAcceso
SET SKIP OF BAR 13 OF pop_03 !'c' $vAcceso
SET SKIP OF BAR 14 OF pop_03 !'d' $vAcceso

SET SKIP OF BAR 1  OF pop_04 !'e' $vAcceso
SET SKIP OF BAR 2  OF pop_04 !'f' $vAcceso
SET SKIP OF BAR 3  OF pop_04 !'g' $vAcceso
SET SKIP OF BAR 4  OF pop_04 !'h' $vAcceso
SET SKIP OF BAR 5  OF pop_04 !'i' $vAcceso

SET SKIP OF BAR 1  OF pop_05 !'j' $vAcceso
SET SKIP OF BAR 2  OF pop_05 !'k' $vAcceso
SET SKIP OF BAR 3  OF pop_05 !'l' $vAcceso
SET SKIP OF BAR 4  OF pop_05 !'m' $vAcceso
SET SKIP OF BAR 5  OF pop_05 !'n' $vAcceso
SET SKIP OF BAR 6  OF pop_05 !'o' $vAcceso
SET SKIP OF BAR 8  OF pop_05 !'p' $vAcceso
SET SKIP OF BAR 9  OF pop_05 !'q' $vAcceso

SET SKIP OF BAR 1  OF pop_06 !'r' $vAcceso
SET SKIP OF BAR 2  OF pop_06 !'s' $vAcceso
SET SKIP OF BAR 3  OF pop_06 !'t' $vAcceso
SET SKIP OF BAR 4  OF pop_06 !'u' $vAcceso
SET SKIP OF BAR 5  OF pop_06 !'v' $vAcceso
SET SKIP OF BAR 6  OF pop_06 !'w' $vAcceso

SET SKIP OF BAR 1  OF pop_07 !'x' $vAcceso
SET SKIP OF BAR 2  OF pop_07 !'y' $vAcceso
SET SKIP OF BAR 3  OF pop_07 !'z' $vAcceso
SET SKIP OF BAR 4  OF pop_07 !' ' $vAcceso   && REVISAR!!!
SET SKIP OF BAR 6  OF pop_07 !'1' $vAcceso
SET SKIP OF BAR 7  OF pop_07 !'2' $vAcceso
SET SKIP OF BAR 8  OF pop_07 !'3' $vAcceso
SET SKIP OF BAR 9  OF pop_07 !'4' $vAcceso
SET SKIP OF BAR 10 OF pop_07 !'5' $vAcceso

SET SKIP OF BAR 1  OF pop_04d !'6' $vAcceso
SET SKIP OF BAR 2  OF pop_04d !'7' $vAcceso
SET SKIP OF BAR 3  OF pop_04d !'8' $vAcceso
SET SKIP OF BAR 4  OF pop_04d !'9' $vAcceso
SET SKIP OF BAR 5  OF pop_04d !'0' $vAcceso
SET SKIP OF BAR 6  OF pop_04d !'!' $vAcceso
SET SKIP OF BAR 7  OF pop_04d !'@' $vAcceso
SET SKIP OF BAR 8  OF pop_04d !'#' $vAcceso
SET SKIP OF BAR 9  OF pop_04d !'$' $vAcceso
SET SKIP OF BAR 10 OF pop_04d !'%' $vAcceso
SET SKIP OF BAR 11 OF pop_04d !'^' $vAcceso
SET SKIP OF BAR 12 OF pop_04d !'&' $vAcceso
SET SKIP OF BAR 13 OF pop_04d !'*' $vAcceso
SET SKIP OF BAR 14 OF pop_04d !'(' $vAcceso
SET SKIP OF BAR 15 OF pop_04d !')' $vAcceso
SET SKIP OF BAR 16 OF pop_04d !'|' $vAcceso

* No acceso a comprobantes
vOpcion = .T.
FOR I = 1 TO 11
  IF !SKPBAR('pop_01',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 1 OF menu vOpcion

* No acceso a Procesos
vOpcion = .T.
FOR I = 1 TO 9
  IF !SKPBAR('pop_02',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 2  OF menu  vOpcion

* No acceso a Reportes
vOpcion = .T.
FOR I = 1 TO 14
  IF !SKPBAR('pop_03',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 3  OF menu vOpcion

* No acceso a Tablas
vOpcion = .T.
FOR I = 1 TO 5
  IF !SKPBAR('pop_04',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 4  OF menu vopcion


* No acceso a Utilitarios
vOpcion = .T.
FOR I = 1 TO 9
  IF !SKPBAR('pop_05',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 5  OF menu vopcion

* No acceso a Datos iniciales
vOpcion = .T.
FOR I = 1 TO 6
  IF !SKPBAR('pop_06',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 6  OF menu vopcion

* No acceso a Especiales
vOpcion = .T.
FOR I = 1 TO 10
  IF !SKPBAR('pop_07',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 7  OF menu vopcion

* No acceso a Tablas
vOpcion = .T.
FOR I = 1 TO 16
  IF !SKPBAR('pop_04d',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 3  OF popup pop_03





RETURN

* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  Programa SubPrincipal : Abas.Prg                                   º
* º                                                                     º
* º  ®®  Sistema de Administraci¢n e Informaci¢n REGION GRAU ¯¯         º
* º                                                                     º
* º  Empresa : REGION GRAU  PIURA-PERU                                  º
* º                                                                     º
* º  SAIKI Consultores   Av. Arequipa 1130 Dpto 102 - Lima Per£         º
* º                      Tel-Fax 709612                                 º
* º                                                                     º
* º  Software : Fox Pro 2.x                                             º
* º                                                                     º
* º  Inicio   : Agosto 1994                                             º
* º                                                                     º
* º      M¢dulo  Abastecimientos  Ing Luis Castillo                     º
* º                                                                     º
* º  Inicio de Operaci¢n:                                               º
* º      Fecha: 02/01/95                                                º
* º                                                                     º
*  ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
SET EXCLUSIVE OFF
SET ESCAPE OFF
CLEAR
CLOSE ALL
CLEAR ALL

SET TALK OFF
SET ECHO OFF

worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO Sai_lib
DO fox_ambi

*SET PATH TO ..\DATA96
*IF WorKer
*   IF YesNo("Datos de Prueba?")
*      SET PATH TO ..\DATAPR
*   ENDIF
*ENDIF

SET PATH TO H:\SISRG\DATA96
IF WorKer
   IF YesNo("Datos de Prueba?")
      SET PATH TO H:\SISFTE\DATAPR
   ENDIF
ENDIF


*-- Clave de acceso
*vUser_ID = SPACE(5)
vUser_ID = ALLTRIM(LEFT(SYS(0),15))

* IF !FILE( 'H:\SISRG\DATA96\USUARIO.CDX')
*  DO INDEXA 
* ENDIF

public vCoddep

* IF !WorKer
*USE Usuario ORDER TAG Usuario1  ALIAS Usua
* IF !Clave(vUser_ID,19,30,23,55)
*   CLOSE DATA
*   DO STANDBY WITH '!! Acceso Denegado !!',10,15
*  IF Worker
*     CANCEL
*   ENDIF
*  QUIT
* ENDIF
* vNombre  = ALLTRIM(nombre)
* USE
* ENDIF

* USE IteUsu ORDER TAG IteUsu1   ALIAS IteU
* vUsuCla = CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
*           'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
* vAcceso = ''

* SEEK vUsuCla

* SCAN WHILE vUsuCla = ALLTRIM(IteU.UsuCla)
*     vAcceso = vAcceso + IteU.Modulo
* ENDSCAN

* USE


**-- Definiciones para el Men£ --**
*----------------------------------*
CIA  = "     CTAR    "
SUBC = " Regi¢n Grau "
DESA = " UDEP "
TITU = " Sistema de Abastecimientos " + "("+ALLTRIM(SUBSTR(SYS(0),1,2))+")"
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

spac = 80 - LEN(DESA) - 8
Rotulo2 = DESA + SPACE(spac) + DTOC(DATE()) + ' '

DO Logos WITH Rotulo1,Rotulo2,50

SAVE SCREEN TO Principal

* ERROR TRAPPING SYSTEM
IF WORKER
   ON ERROR
ELSE
   ON ERROR DO FOX_ERRS WITH PROGRAM()
ENDIF

SAVE SCREEN TO Pantalla

SET CLOCK ON
*SET CLOCK TO 00,67
STORE .t. TO vEn_sistema
STORE .f. TO esc_tecla
= SYS(2002,1)        && Set cursor on

*--- El men£ principal ---**
 L_COL =" B+/BG, N/W, N/W, GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "

* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º      Abastecimiento       º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

IF escolor
   DEFINE POPUP menu FROM 5,2 TO 16,22 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP menu FROM 5,2 TO 16,22 COLOR SCHEME C_POPUP
ENDIF

DEFINE BAR 1  OF  menu PROMPT ' \<Programaci¢n   ' MESSAGE ''
DEFINE BAR 2  OF  menu PROMPT ' \<Obtenciones    ' MESSAGE ''
DEFINE BAR 3  OF  menu PROMPT ' a\<Lmacen        ' MESSAGE ''
DEFINE BAR 4  OF  menu PROMPT ' ser\<Vicios      ' MESSAGE ''
DEFINE BAR 5  OF  menu PROMPT ' \<Kardex         ' MESSAGE ''
DEFINE BAR 6  OF  menu PROMPT ' \<Cat logos      ' MESSAGE ''
DEFINE BAR 7  OF  menu PROMPT ' \<Tablas         ' MESSAGE ''
DEFINE BAR 8  OF  menu PROMPT ' \<Utilitarios    ' MESSAGE ''
DEFINE BAR 9  OF  menu PROMPT ' \<Salida          '

*- Programaci¢n
IF escolor
   DEFINE POPUP pop_02a FROM 5,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02a FROM 5,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02a PROMPT '\<Cuadro necesidades' MESSAGE ''
DEFINE BAR 2  OF  pop_02a PROMPT '\<Verifica cuadro   ' MESSAGE ''
DEFINE BAR 3  OF  pop_02a PROMPT 'c\<Onsolidaci¢n     ' MESSAGE ''

*- Adquisici¢n
IF escolor
   DEFINE POPUP pop_02b FROM 6,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02b FROM 6,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02b PROMPT '\<Registro pe.co.sa     ' MESSAGE ''
DEFINE BAR 2  OF  pop_02b PROMPT '\<Solicitud cotizaci¢n  ' MESSAGE ''
DEFINE BAR 3  OF  pop_02b PROMPT 'recepci¢n \<Cotizaciones' MESSAGE ''
DEFINE BAR 4  OF  pop_02b PROMPT '\<Adjudicaci¢n          ' MESSAGE ''
DEFINE BAR 5  OF  pop_02b PROMPT '\<Orden de compra       ' MESSAGE ''
DEFINE BAR 6  OF  pop_02b PROMPT '\<Parte de anulaci¢n    ' MESSAGE ''
DEFINE BAR 7  OF  pop_02b PROMPT 'parte de re\<Baja       ' MESSAGE ''
DEFINE BAR 8  OF  pop_02b PROMPT 'co\<Nsulta calendario   ' MESSAGE ''
DEFINE BAR 9  OF  pop_02b PROMPT 'conci\<Liaci¢n mensual  ' MESSAGE ''
DEFINE BAR 10 OF  pop_02b PROMPT 'conc\<Iliaci¢n diaria   ' MESSAGE ''

*- Almacen
IF escolor
   DEFINE POPUP pop_02c FROM 07,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02c FROM 07,26 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_02c PROMPT '\<Gu¡a internamiento       ' MESSAGE ''
DEFINE BAR 2  OF  pop_02c PROMPT '\<Despacho                 ' MESSAGE ''
DEFINE BAR 3  OF  pop_02c PROMPT '\<Nota de entrada almac‚n  ' MESSAGE ''
DEFINE BAR 4  OF  pop_02c PROMPT '\<P¢lizas                 ' MESSAGE ''

IF escolor
   DEFINE POPUP pop_02ca FROM 09,54 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02ca FROM 09,54 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF pop_02ca PROMPT "\<Entrada  "
DEFINE BAR 2 OF pop_02ca PROMPT "\<Salida   "


*- Servicios
IF escolor
   DEFINE POPUP pop_02d FROM 08,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02d FROM 08,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02d PROMPT '\<Solicitud de servicio     ' MESSAGE ''
DEFINE BAR 2  OF  pop_02d PROMPT 'so\<Licitud cotizaci¢n servicios' MESSAGE ''
DEFINE BAR 3  OF  pop_02d PROMPT 'recepci¢n \<Cotizaciones    ' MESSAGE ''
DEFINE BAR 4  OF  pop_02d PROMPT '\<Adjudicaci¢n              ' MESSAGE ''
DEFINE BAR 5  OF  pop_02d PROMPT 'o\<Rden de Servicio         ' MESSAGE ''
DEFINE BAR 6  OF  pop_02d PROMPT '\<Parte de anulaci¢n        ' MESSAGE ''
DEFINE BAR 7  OF  pop_02d PROMPT 'parte re\<Baja              ' MESSAGE ''
DEFINE BAR 8  OF  pop_02d PROMPT 'co\<Nsulta calendario       ' MESSAGE ''
DEFINE BAR 9  OF  pop_02d PROMPT 'c\<Onciliaci¢n mensual      ' MESSAGE ''
*DEFINE BAR 10 OF  pop_02d PROMPT 'concil\<Iaci¢n diaria       ' MESSAGE ''
*- Kardex
IF escolor
   DEFINE POPUP pop_02e FROM  9,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02e FROM  9,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02e PROMPT '\<Existencias valoradas   ' MESSAGE ''
DEFINE BAR 2  OF  pop_02e PROMPT '\<Control visible almacen ' MESSAGE ''
DEFINE BAR 3  OF  pop_02e PROMPT '\<Stock                   ' MESSAGE ''
DEFINE BAR 4  OF  pop_02e PROMPT '\<Stock kardex             ' MESSAGE ''
*- Cat logos
IF escolor
   DEFINE POPUP pop_02f FROM 10,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02f FROM 10,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02f PROMPT '\<Proveedores ' MESSAGE ''
DEFINE BAR 2  OF  pop_02f PROMPT '\<Art¡culos   ' MESSAGE ''
DEFINE BAR 3  OF  pop_02f PROMPT '\<Servicios   ' MESSAGE ''
DEFINE BAR 4  OF  pop_02f PROMPT 'p\<Recios     ' MESSAGE ''
DEFINE BAR 5  OF  pop_02f PROMPT '\<Obras       ' MESSAGE ''
DEFINE BAR 6  OF  pop_02f PROMPT 'r\<Ecord      ' MESSAGE ''

*- Tablas
IF escolor
   DEFINE POPUP pop_02g FROM 11,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02g FROM 11,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02g PROMPT '\<Grupos gen‚ricos  ' MESSAGE ''
DEFINE BAR 2  OF  pop_02g PROMPT '\<Dependencias      ' MESSAGE ''
DEFINE BAR 3  OF  pop_02g PROMPT '\<Tipo de servicios ' MESSAGE ''
DEFINE BAR 4  OF  pop_02g PROMPT '\<Procedencia       ' MESSAGE ''
DEFINE BAR 5  OF  pop_02g PROMPT 'd\<Estino           ' MESSAGE ''
DEFINE BAR 6  OF  pop_02g PROMPT 'tipo de \<Obras     ' MESSAGE ''
DEFINE BAR 7  OF  pop_02g PROMPT '\<Unidad medida     ' MESSAGE ''

*DEFINE BAR 4  OF  pop_02g PROMPT 'Correlativo \<Proveedores  ' MESSAGE ''
*DEFINE BAR 5  OF  pop_02g PROMPT 'Correlativo \<O/C PRP      ' MESSAGE ''
*DEFINE BAR 6  OF  pop_02g PROMPT 'Correlativo \<O/C TRN      ' MESSAGE ''
*DEFINE BAR 7  OF  pop_02g PROMPT 'Correlativo \<O/S PRP      ' MESSAGE ''
*DEFINE BAR 8  OF  pop_02g PROMPT 'Correlativo \<O/S TRN      ' MESSAGE ''
*DEFINE BAR 9  OF  pop_02g PROMPT 'Correlativo peco\<Sa       ' MESSAGE ''
*DEFINE BAR 10 OF  pop_02g PROMPT 'Correlativo \<Destinos     ' MESSAGE ''
*DEFINE BAR 11 OF  pop_02g PROMPT 'Correlativo proce\<Dencias ' MESSAGE ''
*EFINE BAR 10 OF  pop_02g PROMPT 'Correlativo pt\<E anul O/S ' MESSAGE ''

*- Utilitarios
IF escolor
   DEFINE POPUP pop_02h FROM 12,26 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02h FROM 12,26 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_02h PROMPT '\<Indexar archivos           '
DEFINE BAR  2 OF pop_02h PROMPT '\<Backup de los archivos     '
DEFINE BAR  3 OF pop_02h PROMPT 'De\<puraci¢n de backups      '
*EFINE BAR  4 OF pop_02h PROMPT '\<Mantenimiento Correlativos    '
DEFINE BAR  4 OF pop_02H PROMPT 'De\<puraci¢n de backups      '
DEFINE BAR  5 OF pop_02h PROMPT '\-'
DEFINE BAR  6 OF pop_02h PROMPT 'Ir al D\<OS         ' MESSAGE '®® Escriba EXIT para regresar ¯¯'
*DEFINE BAR  7 OF pop_02h PROMPT 'Ir al \<Word Perfect' MESSAGE '®® Escriba EXIT para regresar ¯¯'
*DEFINE BAR  8 OF pop_02h PROMPT 'Ir al \<QUATTRO PRO'  MESSAGE '®® Escriba EXIT para regresar ¯¯'

ON SELECTION POPUP menu DO ACT_MENUS


**- La sombra del principal
SAVE SCREEN TO principal
ACTIVATE POPUP menu
DO WHILE  EMPTY(PROMPT())
   ACTIVATE POPUP menu
ENDDO
*-Cerrar todo...
ON KEY
RETURN

*-- Fin  del Programa Principal

PROCEDURE ACT_MENUS
*------------------
 choice=BAR()
 DO CASE
    CASE choice = 1    && Programaci¢n
         ON SELECTION POPUP pop_02a  DO  menu_02a
         ACTIVATE POPUP pop_02a
    CASE choice = 2    && Obtenciones
         ON SELECTION POPUP pop_02b  DO  menu_02b
         ACTIVATE POPUP pop_02b
    CASE choice = 3    && Almacen
         ON SELECTION POPUP pop_02c  DO  menu_02c
         ACTIVATE POPUP pop_02c
    CASE choice = 4    && Kardex
         ON SELECTION POPUP pop_02d  DO  menu_02d
         ACTIVATE POPUP pop_02d
    CASE choice = 5    && Servicios
         ON SELECTION POPUP pop_02e  DO  menu_02e
         ACTIVATE POPUP pop_02e
    CASE choice = 6    && Catalogos
         ON SELECTION POPUP pop_02f  DO  menu_02f
         ACTIVATE POPUP pop_02f
    CASE choice = 7    && Tablas
         ON SELECTION POPUP pop_02g  DO  menu_02g
         ACTIVATE POPUP pop_02g
    CASE choice = 8    && Utilitarios
         ON SELECTION POPUP pop_02h  DO  menu_02h
         ACTIVATE POPUP pop_02h
   CASE BAR() = 9
         DO Salmenu
 ENDCASE


RESTORE SCREEN FROM principal
RETURN



PROCEDURE menu_02a  && Programaci¢n
*-----------------
 choice=BAR()
 DO CASE
    CASE choice = 1 && Cuadro de necesidades
         DO REGCDR
    CASE choice = 2 && Verifica Cdro Necesidades
         DO VERCDR
    CASE choice = 3 && Consolida Cdro necesidades
         DO CONCDR
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02a DO menu_02a
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02a,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE Menu_02b  && Obtenciones
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    && Registro PE.CO.SA
         DO REGPEC
    CASE choice = 2    && Solicitud de Cotizaci¢n
         DO SOLCOT WITH 1
    CASE choice = 3    && Registro de Cotizaci¢n
         DO REGCOT WITH 1
    CASE choice = 4    && Cuadro Comparativo Cotizaci¢n
         DO ADJCOT WITH 1
    CASE choice = 5    && Orden de Compra
         DO ORDCOM
    CASE choice = 6    && Parte Anulaci¢n
         DO ANUPAR with 1
    CASE choice = 7    && Parte Rebaja
         DO rebPAR with 1
    CASE choice = 8    && Consulta Calendario
         DO SALcaL WITH 1
    CASE choice = 9    && Conciliacion
         DO conord  with 'O/C'
    CASE choice = 10   && Conciliacion
         DO conord1 with 'O/C'

 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02b DO menu_02b
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02b,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_02c   && Almacen
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    && Registro de Guias de internamiento
         IF !WORKER
         OK=VE_PASSW('TOPY')
         IF OK
            DO REGGUI
         ENDIF
         ELSE
            DO REGGUI
         ENDIF
    CASE choice = 2    && Registro de Salidas
         IF !WORKER
         OK=VE_PASSW('LUCH')
         IF OK
	        DO PECSAL
	     ENDIF
	     ELSE
	        DO PECSAL
	     ENDIF
    CASE choice = 3 && Nota de entrada alm.
         DO NOTALM
    CASE choice = 4    && POLIZAS DE ENTRADA Y SALIDA
         ON SELECTION POPUP pop_02ca  DO  menu_02ca
         ACTIVATE POPUP pop_02ca
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02c DO menu_02c
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02c,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_02ca   && P¢lizas
*-----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    && entrada
         DO polent
    CASE choice = 2    && salida
         DO polsal
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02ca DO menu_02ca
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02ca,pop_02c,menu
 HIDE WINDOW ALL
 RETURN


PROCEDURE menu_02d   && Servicios
*-----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    && registro solicitud de servicio
         DO REGSOL
    CASE choice = 2    && solicitud de cotizacion de servicio
         DO SOLCOT WITH 2
    CASE choice = 3    && registro proveedores de servicio
         DO REGCOT WITH 2
    CASE choice = 4    && adjudicaci¢n proveedor
         DO ADJCOT WITH 2
    CASE choice = 5    && registro Orden de Servicio
         DO ORDSER
    CASE choice = 6    && Parte Anulaci¢n
         DO ANUPAR with 2
    CASE choice = 7    && Parte Rebaja
         DO rebPAR with 2
    CASE choice = 8    && Consultas
         DO SALCAL WITH 2
    CASE choice = 9    && Conciliacion
         DO conord with 'O/S'
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02d DO menu_02d
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02d,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_02e   && Kardex
*-----------------
 choice=BAR()
 DO CASE
    CASE choice = 1 && Existencias valoradas
         DO KARDEX
    CASE choice = 2 && Control visible alm.
         DO COVIAL
    CASE choice = 3 && Pedido comprobante salida
         OK=VE_PASSW('LENY')
         if ok
		    DO STOCK2
		 endif
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02e DO menu_02e
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02e,menu
 HIDE WINDOW ALL
 RETURN


PROCEDURE menu_02f   && Catalogos
*-----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    && Proveedores
         do promae
    CASE choice = 2    && Art¡culos
         do artmae
    CASE choice = 3    && Servicios
         do sermae
    CASE choice = 4    && Precios
         do precio1
    CASE choice = 5    && Obras
         do obras
    CASE choice = 6    && Obras
         do record
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02f DO menu_02f
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02f,menu
 HIDE WINDOW ALL
 RETURN


PROCEDURE menu_02g   && Tablas
*-----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    && Grupos Gen‚ricos
         DO manpar WITH "CODGEB","Grupo Gen‚rico","Grupos Gen‚ricos",1,"Valor siguiente"
    CASE choice = 2    && DEPENDENCIAS
         DO manpar WITH "CODDEP","Dependencia","Dependencias",1,"Valor siguiente"
    CASE choice = 3    && Tipo de Servicio
         DO manpar WITH "TIPSER","Tipo de Servicio","Tipos de Documentos"
    CASE choice = 4    && Correlativo  Procedencias
         DO manpar WITH "CODPRC","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"
    CASE choice = 5    && Correlativo  DESTINOS
         DO manpar WITH "CODDET","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"
    CASE choice = 6    && Tipo Obras
         DO manpar WITH "TIPOBR","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"
    CASE choice = 7    &&
         DO manpar WITH "CODUNI","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"

*    CASE choice = 4    && Correlativo Proveedores
*         DO manpar WITH "CORRELPROVEE","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
*    CASE choice = 5    && Correlativo O/C ingresos propios
*        DO manpar WITH "CORRELORDPRP","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
*   CASE choice = 6    && Correlativo O/C tesoro publico
*        DO manpar WITH "CORRELORDTRN","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
*   CASE choice = 7    && Correlativo  O/S INGRESOS PROPIOS
*        DO manpar WITH "CORRELSERPRP","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
*   CASE choice = 8    && Correlativo  O/S TESORO PUBLICO
*        DO manpar WITH "CORRELSERTRN","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
*   CASE choice = 9    && Correlativo  PECOSA
*        DO manpar WITH "CORRELPECOSA","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
*   CASE choice = 10   && Correlativo  Pte Anul O/S
*        DO manpar WITH "CORRELANULOS","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
*   CASE choice = 10   && Correlativo  PROCEDENCIA
*        DO manpar WITH "CORRELPROCED","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"

 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02g DO menu_02g
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02g,menu
 HIDE WINDOW ALL
 RETURN



PROCEDURE menu_02h  && Utilitarios
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    && Reindexado
      =PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar el proceso de reordenamiento sin ning£n problema')
      Do Indexaba
    CASE choice = 2    && Backup
      =PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar la copia de seguridad sin ning£n problema')
      DO BackUp
    CASE choice = 3    && Depura Backup
      DO ResBK
    CASE choice = 4    && Correlativo
      DO DepBK
     *DO manpar WITH "CORREL","Correlativo","Correlativos",4,"Valor siguiente"
    CASE choice = 6
    CASE choice = 7
    CASE choice = 8
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02h DO menu_02h
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02h,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE salmenu
*----------------
  STORE  YESNO('¨ Estamos terminando la sesi¢n ?') TO in_secoemsa
  IF in_secoemsa
    SET COLOR TO
    CLOSE DATA
    HIDE POPUP ALL
    IF worker
      CLEAR
      SET HELP OFF
      ON KEY
      SET EXCLU OFF
      CANCEL
    ELSE
      RESTORE SCREEN FROM Principal
      DO CLRSCR
    ENDIF
    QUIT
  ELSE
     RETURN
  ENDIF

PROCEDURE salmenu2
*----------------
  IF YESNO(' Estamos terminando la sesi¢n ')
     SET COLOR TO
     CLOSE DATA
     CLOSE PROC
     HIDE POPUP ALL
     RESTORE SCREEN FROM Principal
     DO CLRSCR1
     SET HELP ON
     DEACTIVATE WINDOW ALL
     IF worker
        SET EXCLU OFF
        CLEAR ALL
        CANCEL
     ENDIF
     QUIT
  ENDIF
  RETURN


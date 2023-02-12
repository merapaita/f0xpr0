** şÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄş
** º  Programa SubPrincipal : Abas.Prg                                   º
** º                                                                     º
** º  ®®  Sistema de Administraci¢n e Informaci¢n REGION PIURA ¯¯        º
** º                                                                     º
** º  Empresa : REGION PIURA PIURA-PERU                                  º
** º                                                                     º
** º  Software : Fox Pro 2.x                                             º
** º                                                                     º
** º  Inicio   : Agosto 1994                                             º
** º                                                                     º
** º  M¢dulo   : Abastecimiento   Ing. Luis Castillo D                   º
** º                                                                     º
** º  Inicio de Operaci¢n: 02/01/95                                      º
** º                                                                     º
** º  Soporte y Actualizaci¢n : T‚c. Norberto Recarte Romero.            º
** şÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄş

SET EXCLUSIVE OFF
SET ESCAPE OFF
CLEAR
CLOSE ALL
CLEAR ALL

SET TALK OFF
SET ECHO OFF

RUTA='H:\SIS98\DATA'
REPOS='H:\SISrg\REPOS'

RUTApr='H:\SIS98\DATApr'
REPOSpr='H:\SISfte\REPOS'

worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO Sai_lib
DO fox_ambi 

SET PATH TO &ruta
IF WorKer
   IF YesNo("Datos de Prueba?")
      SET PATH TO &rutapr
   ENDIF
ENDIF

*-- Clave de acceso
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
*vUser_ID = "TRECARTE"
IF !FILE( '&ruta\USUARIO.CDX')
  DO STANDBY WITH ' Error en INDEXA '
ENDIF

public vCoddep,vConex,vMaq,vLlave,vUsurec,vflag,vNumdep,sistema,vsistema

sistema='1'
vConex = ALLTRIM(LEFT(SYS(0),8))
vmaq   = ALLTRIM(RIGHT(SYS(0),2))
vllav  = ALLTRIM(sys(2))


USE Usuario ORDER TAG Usuario1  ALIAS Usua
IF !Clave(vUser_ID,19,30,23,55)
   CLOSE DATA
   DO STANDBY WITH '!! Acceso Denegado !!',10,15
   IF Worker
      CANCEL
   ENDIF
   QUIT
ENDIF

vNombre  = ALLTRIM(nombre)
vCoddep  = coddep
vNumDep  = numdep
vflag = flag
USE

USE IteUsu ORDER TAG IteUsu1   ALIAS IteU
SELE Iteu
SET FILT TO sistema='2'
vUsuCla = CHRTRAN(vUser_ID,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{<>?~«¬@›_#')
vAcceso = ' '

SEEK vUsuCla

SCAN WHILE vUsuCla = ALLTRIM(IteU.UsuCla)
     vAcceso = vAcceso + IteU.Modulo
ENDSCAN
SET FILT TO
USE



**-- Definiciones para el Men£ --**
*----------------------------------*
CIA  = " CTAR PIURA  "
SUBC = "Nrr"
DESA = " INFORMATICA "
TITU = " Sistema de Abastecimiento 1998 " + "("+ALLTRIM(SUBSTR(SYS(0),1,10))+")"
VSISTEMA = '1'
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

@  1, 0,23,79  BOX "°°°°°°°°°" COLOR SCHEME c_fondo
@  1, 1,23,24  BOX "±±±±±±±±±" COLOR SCHEME c_fondo


spac = (80 -(LEN(CIA)+LEN(TITU)+10))/2
Rotulo1 = CIA + SPACE(spac) + TITU + SPAC(spac) + space(11)

usuarios = '® USER: '+vnombre+' ¯'
fecha    = DTOC(DATE()) +space(2)
Rotulo2  = PADR(Desa,17,' ')+PADC(Usuarios,46,' ')+PADL(Fecha,17,' ')

*spac = 80 - LEN(DESA) - 8
*Rotulo2 = DESA + SPACE(spac) + DTOC(DATE()) + ' '

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
STORE .t. TO vEn_sistema
STORE .f. TO esc_tecla
= SYS(2002,1)



*--- El men£ principal ---**
L_COL  ="W+/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "

** şÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄş
** º      Abastecimiento       º
** şÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄş

IF escolor
   DEFINE POPUP menu FROM 5,2 TO 17,22 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP menu FROM 5,2 TO 17,22 COLOR SCHEME C_POPUP
ENDIF

DEFINE BAR 1  OF  menu PROMPT ' \<Programaci¢n   ' MESSAGE ''
DEFINE BAR 2  OF  menu PROMPT ' \<Obtenciones    ' MESSAGE ''
DEFINE BAR 3  OF  menu PROMPT ' a\<Lmacen        ' MESSAGE ''
DEFINE BAR 4  OF  menu PROMPT ' ser\<Vicios      ' MESSAGE ''
DEFINE BAR 5  OF  menu PROMPT ' \<Kardex         ' MESSAGE ''
DEFINE BAR 6  OF  menu PROMPT ' \<Transportes    ' MESSAGE ''
DEFINE BAR 7  OF  menu PROMPT ' \<Cat logos      ' MESSAGE ''
DEFINE BAR 8  OF  menu PROMPT ' ta\<Blas         ' MESSAGE ''
DEFINE BAR 9  OF  menu PROMPT ' \<Utilitarios    ' MESSAGE ''
DEFINE BAR 10 OF  menu PROMPT ' co\<Nsultas      ' MESSAGE ''
DEFINE BAR 11 OF  menu PROMPT ' \<Salida          '

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
   DEFINE POPUP pop_02b FROM 6,27 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02b FROM 6,27 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02b PROMPT '\<Registro pe.co.sa     ' MESSAGE ''
DEFINE BAR 2  OF  pop_02b PROMPT '\<Solicitud cotizaci¢n  ' MESSAGE ''
DEFINE BAR 3  OF  pop_02b PROMPT 'recepci¢n \<Cotizaciones' MESSAGE ''
DEFINE BAR 4  OF  pop_02b PROMPT '\<Adjudicaci¢n          ' MESSAGE ''
DEFINE BAR 5  OF  pop_02b PROMPT '\<Orden de compra       ' MESSAGE ''
DEFINE BAR 6  OF  pop_02b PROMPT '\<Parte de anulaci¢n    ' MESSAGE ''
DEFINE BAR 7  OF  pop_02b PROMPT 'parte de re\<Baja       ' MESSAGE ''
DEFINE BAR 8  OF  pop_02b PROMPT 'co\<Nsulta calendario   ' MESSAGE ''
DEFINE BAR 9  OF  pop_02b PROMPT 'consul\<Ta saldos marco ' MESSAGE ''
DEFINE BAR 10 OF  pop_02b PROMPT 'conci\<Liaci¢n mensual  ' MESSAGE ''
DEFINE BAR 11 OF  pop_02b PROMPT 'conc\<Iliaci¢n diaria   ' MESSAGE ''
DEFINE BAR 12 OF  pop_02b PROMPT 'conciliaci¢n ba\<Yovar  ' MESSAGE ''

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
   DEFINE POPUP pop_02ca FROM 10,54 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02ca FROM 10,54 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF pop_02ca PROMPT "\<Entrada O/C "
DEFINE BAR 2 OF pop_02ca PROMPT "\<Entrada NEA "
DEFINE BAR 3 OF pop_02ca PROMPT "\<Salida   "
DEFINE BAR 4 OF pop_02ca PROMPT "en\<Trada Bayovar"
DEFINE BAR 5 OF pop_02ca PROMPT "sa\<Lida  Bayovar"


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
DEFINE BAR 9  OF  pop_02d PROMPT 'consul\<Ta saldos marco     ' MESSAGE ''
DEFINE BAR 10 OF  pop_02d PROMPT 'c\<Onciliaci¢n mensual      ' MESSAGE ''
DEFINE BAR 11 OF  pop_02d PROMPT 'concil\<Iaci¢n bayovar      ' MESSAGE ''

*- Kardex
IF escolor
   DEFINE POPUP pop_02e FROM  9,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02e FROM  9,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02e PROMPT '\<Existencias valoradas   ' MESSAGE ''
DEFINE BAR 2  OF  pop_02e PROMPT '\<Control visible almacen ' MESSAGE ''
DEFINE BAR 3  OF  pop_02e PROMPT '\<Stock                   ' MESSAGE ''
DEFINE BAR 4  OF  pop_02e PROMPT 's\<Aldos inventario       ' MESSAGE ''

*- Transportes
IF escolor
   DEFINE POPUP pop_02i FROM 10,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02i FROM 10,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02i PROMPT '\<Registro de vales  ' MESSAGE ''
DEFINE BAR 2  OF  pop_02i PROMPT '\<Agrupa vales       ' MESSAGE ''

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
DEFINE BAR 5  OF  pop_02f PROMPT 'r\<Ecord      ' MESSAGE ''
DEFINE BAR 6  OF  pop_02f PROMPT '\<Veh¡culos   ' MESSAGE ''
DEFINE BAR 7  OF  pop_02f PROMPT 'c\<Hoferes    ' MESSAGE ''
DEFINE BAR 8  OF  pop_02f PROMPT 'serv.ve\<H¡culos ' MESSAGE ''

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
DEFINE BAR 6  OF  pop_02g PROMPT '\<Unidad medida     ' MESSAGE ''

*- Utilitarios
IF escolor
   DEFINE POPUP pop_02h FROM 12,26 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02h FROM 12,26 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_02h PROMPT '\<Indexar archivos     '
DEFINE BAR  2 OF pop_02h PROMPT '\<Backup de los archivos'
DEFINE BAR  3 OF pop_02h PROMPT 'De\<puraci¢n de backups '
DEFINE BAR  4 OF pop_02H PROMPT '\<Correlativos         '
DEFINE BAR  5 OF pop_02h PROMPT '\-'
DEFINE BAR  6 OF pop_02h PROMPT '\<Seguridad         ' MESSAGE '®® Acceso RESTRINGIDO ¯¯'
DEFINE BAR  7 OF pop_02h PROMPT '\<Usuario ABAS      ' 

*- Correlativos
IF escolor
   DEFINE POPUP pop_02ha FROM 15,54 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02ha FROM 15,54 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_02ha PROMPT '\<Proveedores        ' MESSAGE ''
DEFINE BAR 2  OF  pop_02ha PROMPT 'orden de \<Compra    ' MESSAGE ''
DEFINE BAR 3  OF  pop_02ha PROMPT 'orden de \<Servicio  ' MESSAGE ''
DEFINE BAR 4  OF  pop_02ha PROMPT 'p\<E.co.sa           ' MESSAGE ''
DEFINE BAR 5  OF  pop_02ha PROMPT 's\<Olicitud Servicio ' MESSAGE ''
DEFINE BAR 6  OF  pop_02ha PROMPT 'c\<Uadro Comnparativo' MESSAGE ''

*- Consultas
IF escolor
   DEFINE POPUP pop_02j FROM 3,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02j FROM 3,26 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_02j PROMPT '\<a. Pe.co.sa.            '  MESSAGE ''
DEFINE BAR 2  OF  pop_02j PROMPT '\<b. Orden de Compra      '  MESSAGE ''
DEFINE BAR 3  OF  pop_02j PROMPT '\<c. Solicitud de Servicio'  MESSAGE ''
DEFINE BAR 4  OF  pop_02j PROMPT '\<d. Orden de Servicio    '  MESSAGE ''
DEFINE BAR 5  OF  pop_02j PROMPT '\<e. Proveedores          '  MESSAGE ''
DEFINE BAR 6  OF  pop_02j PROMPT '\<f. Art¡culos            '  MESSAGE ''
DEFINE BAR 7  OF  pop_02j PROMPT '\<g. P¢lizas de Entrada   '  MESSAGE ''
DEFINE BAR 8  OF  pop_02j PROMPT '\<h. P¢lizas de Salida    '  MESSAGE ''
DEFINE BAR 9  OF  pop_02j PROMPT '\<i. Parte Anulacion O/C  '  MESSAGE ''
DEFINE BAR 10 OF  pop_02j PROMPT '\<j. Parte Anulacion O/S  '  MESSAGE ''
DEFINE BAR 11 OF  pop_02j PROMPT '\<k. Parte Rebaja O/C     '  MESSAGE ''
DEFINE BAR 12 OF  pop_02j PROMPT '\<l. Parte Rebaja O/S     '  MESSAGE ''
DEFINE BAR 13 OF  pop_02j PROMPT '\-' 
DEFINE BAR 14 OF  pop_02j PROMPT '\<m. Hojas de Control     '  MESSAGE ''
DEFINE BAR 15 OF  pop_02j PROMPT '\<n. Comprobantes de Pago '  MESSAGE ''
DEFINE BAR 16 OF  pop_02j PROMPT '\<o. Cheques              '  MESSAGE ''
DEFINE BAR 17 OF  pop_02j PROMPT '\<p. Hojas Modificaci¢n   '  MESSAGE ''
DEFINE BAR 18 OF  pop_02j PROMPT '\<q. Seguimiento Doc.Fte  '  MESSAGE ''
DEFINE BAR 19 OF  pop_02j PROMPT '\<r. Registro de H/C      '  MESSAGE ''

ON SELECTION POPUP menu DO ACT_MENUS

IF !vFlag$'J*'
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
    CASE choice = 6    && transportes
         ON SELECTION POPUP pop_02i  DO  menu_02i
         ACTIVATE POPUP pop_02i
    CASE choice = 7    && Catalogos
         ON SELECTION POPUP pop_02f  DO  menu_02f
         ACTIVATE POPUP pop_02f
    CASE choice = 8    && Tablas
         ON SELECTION POPUP pop_02g  DO  menu_02g
         ACTIVATE POPUP pop_02g
    CASE choice = 9    && Utilitarios
         ON SELECTION POPUP pop_02h  DO  menu_02h
         ACTIVATE POPUP pop_02h
    CASE choice = 10    && Consultas
         ON SELECTION POPUP pop_02j  DO  menu_02j
         ACTIVATE POPUP pop_02j
    CASE BAR() = 11
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
         DO REGPEC with 2,' ',VSISTEMA
    CASE choice = 2    && Solicitud de Cotizaci¢n
         DO SOLCOT WITH 1
    CASE choice = 3    && Registro de Cotizaci¢n
         DO REGCOT WITH 1
    CASE choice = 4    && Cuadro Comparativo Cotizaci¢n
         DO ADJCOT WITH 1
    CASE choice = 5    && Orden de Compra
         DO ORDCOM WITH VSISTEMA,1
    CASE choice = 6    && Parte Anulaci¢n
         DO ANUPAR with 1
    CASE choice = 7    && Parte Rebaja
         DO rebPAR with 1,vsistema
    CASE choice = 8    && Consulta Calendario
    	 DO consal 
    CASE choice = 9    && Consulta Calendario
    	 DO conpre
    CASE choice = 10   && Conciliacion Mensual
         DO conord  with 'O/C'
    CASE choice = 11   && Conciliacion Diaria
         DO conord1 with 'O/C'
    CASE choice = 12   && Conciliacion Bayovar
         DO conordb with 'O/C' &&Conciliaci¢n Bayovar
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
         DO REGGUI WITH VSISTEMA
    CASE choice = 2    && Registro de Salidas
         DO PECSAL WITH VSISTEMA
    CASE choice = 3    && Nota de entrada alm.
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
    CASE choice = 1    && entrada o/c
         DO polent1
    CASE choice = 2    && entrada nea
         DO polent2
    CASE choice = 3    && salida
         DO polsal
    CASE choice = 4    && entrada Bayovar
         DO polbent
    CASE choice = 5    && salida  Bayovar
         DO polbsal
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
         DO REGSOL WITH VSISTEMA
    CASE choice = 2    && solicitud de cotizacion de servicio
         DO SOLCOT WITH 2
    CASE choice = 3    && registro proveedores de servicio
         DO REGCOT WITH 2
    CASE choice = 4    && adjudicaci¢n proveedor
         DO ADJCOT WITH 2
    CASE choice = 5    && registro Orden de Servicio
         DO ORDSER WITH VSISTEMA,1
    CASE choice = 6    && Parte Anulaci¢n
         DO ANUPAR with 2
    CASE choice = 7    && Parte Rebaja
         DO rebPAR with 2,vsistema
    CASE choice = 8    && Consultas
    	 DO CONSAL 
    CASE choice = 9    && Consulta Calendario
         DO CONPRE
    CASE choice = 10   && Conciliacion
         DO conord with 'O/S'
    CASE choice = 11   && Conciliaci¢n Bayovar Servicios
         DO conordb with 'O/S'
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
         DO REGKAR
    CASE choice = 2 && Control visible alm.
         DO REGALM
    CASE choice = 3 && Pedido comprobante salida
 		 IF !WORKER	   
         OK=VE_PASSW('DRGT')
         if ok
		    DO STOCK
		 endif
		 else
		   DO STOCK
		 endif
    CASE choice = 4 && Pedido comprobante salida		 
    	 do stoCKIN
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02e DO menu_02e
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02e,menu
 HIDE WINDOW ALL
 RETURN


PROCEDURE menu_02i   && Transportes
*-----------------
choice=BAR()

DO CASE
   CASE choice = 1 && Existencias valoradas
        DO regval
   CASE choice = 2 && Control visible alm.
        DO agrupa
ENDCASE

IF !escolor
   RESTORE SCREEN FROM principal
   ON SELECTION POPUP pop_02i DO menu_02i
ENDIF

RESTORE SCREEN FROM PRINCIPAL
SHOW POPUP pop_02i,menu
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
    CASE choice = 5    && Record
    	 * do record1
         do record
    CASE choice = 6    && Vehiculos
         do regveh
    CASE choice = 7    && CHOFERES Vehiculos
         DO manpar WITH "CODCHF","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"
    CASE choice = 8    && SERVICIO Vehiculos
         DO manpar WITH "CODSER","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"
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
    CASE choice = 6    &&
         DO manpar WITH "CODUNI","Tipo de Documento","Tipos de Documentos",1,"Valor siguiente"
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
      DO Restore1
    CASE choice = 4    && Correlativo
      ON SELECTION POPUP pop_02ha  DO  menu_02ha
      ACTIVATE POPUP pop_02ha
    CASE choice = 6
      IF LEFT(SYS(0),3)$'JOSE JFG TRECARTE'
         do segabas
      ELSE
        =PopError( 'Acceso denegado ,no lo vuelva a Intentar...')
      endif   
    CASE choice = 7
      do contusu      
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02h DO menu_02h
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02h,menu
 HIDE WINDOW ALL
 RETURN



PROCEDURE menu_02ha  && Correlativos
*----------------
 choice=BAR()
 DO CASE
   CASE choice = 1    && Correlativo Proveedores
        DO manpar WITH "CORRELPROVEE","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
   CASE choice = 2    && Correlativo O/C 
        DO manpar WITH "CORRELORDENC","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
   CASE choice = 3    && Correlativo  O/S 
        DO manpar WITH "CORRELORDENS","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
   CASE choice = 4    && Correlativo  PECOSA
        DO manpar WITH "CORRELPECOSA","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
   CASE choice = 5    && Correlativo Solicitud de Servicios
        DO manpar WITH "CORRELSOLSER","Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
   CASE choice = 6    && Correlativo  CCC
        DO manpar WITH "CORRELCCC",   "Tipo de Documento","Tipos de Documentos",8,"Valor siguiente"
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02h DO menu_02h
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02h,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_02j  && Consultas
*-----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    && Pecosas
         do conpec
    CASE choice = 2    && O/C
         do conoc
    CASE choice = 3    && S/S
         do conSS
    CASE choice = 4    && O/S
         do conos 
    CASE choice = 5    && Proveedores
         do conprv
    CASE choice = 6    && Articulos
         do conart
    CASE choice = 7    && poliza ent
         do conpent
    CASE choice = 8    && poliza sal
         do conpsal
    CASE choice = 9    && parte anul o/c
         do conpan with 1
    CASE choice = 10   && parte anul o/s
         do conpan with 2
    CASE choice = 11   && parte reb o/c
         do conprb with 1
    CASE choice = 12   && parte reb o/s
         do conprb with 2
    CASE choice = 14   && hojas de control
         do conhc
    CASE choice = 15   && comprobantes 
         do concp
    CASE choice = 16   && cheques
         do conch
    CASE choice = 17   && hojas de modif
         do conhm
    CASE choice = 18   && hojas de modif
         do conDOC
    CASE choice = 19   && consulta de h/c
         do reg_hc
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02j DO menu_02j
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02j,menu
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
      SET HELP ON
      ON KEY
      SET EXCLU OFF
      CANCEL
   ELSE
	  CANCEL
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
     *QUIT
  ENDIF
  RETURN


PROCEDURE ABREPAS
*----------------

SET SKIP OF BAR 1  OF pop_02a !'A' $vAcceso
SET SKIP OF BAR 2  OF pop_02a !'B' $vAcceso
SET SKIP OF BAR 3  OF pop_02a !'C' $vAcceso

SET SKIP OF BAR 1  OF pop_02b !'D' $vAcceso
SET SKIP OF BAR 2  OF pop_02b !'E' $vAcceso
SET SKIP OF BAR 3  OF pop_02b !'F' $vAcceso
SET SKIP OF BAR 4  OF pop_02b !'G' $vAcceso
SET SKIP OF BAR 5  OF pop_02b !'H' $vAcceso
SET SKIP OF BAR 6  OF pop_02b !'I' $vAcceso
SET SKIP OF BAR 7  OF pop_02b !'J' $vAcceso
SET SKIP OF BAR 8  OF pop_02b !'K' $vAcceso
SET SKIP OF BAR 9  OF pop_02b !'L' $vAcceso
SET SKIP OF BAR 10 OF pop_02b !'M' $vAcceso
SET SKIP OF BAR 11 OF pop_02b !'N' $vAcceso
SET SKIP OF BAR 12 OF pop_02b !'¥' $vAcceso &&Bayovar

SET SKIP OF BAR 1  OF pop_02c !'O' $vAcceso
SET SKIP OF BAR 2  OF pop_02c !'P' $vAcceso
SET SKIP OF BAR 3  OF pop_02c !'Q' $vAcceso

SET SKIP OF BAR 1  OF pop_02ca !'R' $vAcceso &&o/c
SET SKIP OF BAR 1  OF pop_02ca !'<' $vAcceso &&nea
SET SKIP OF BAR 2  OF pop_02ca !'S' $vAcceso 
SET SKIP OF BAR 3  OF pop_02ca !'T' $vAcceso &&Bayovar
SET SKIP OF BAR 4  OF pop_02ca !'U' $vAcceso &&Bayovar


SET SKIP OF BAR 1  OF pop_02d !'V' $vAcceso
SET SKIP OF BAR 2  OF pop_02d !'W' $vAcceso
SET SKIP OF BAR 3  OF pop_02d !'X' $vAcceso
SET SKIP OF BAR 4  OF pop_02d !'Y' $vAcceso
SET SKIP OF BAR 5  OF pop_02d !'Z' $vAcceso
SET SKIP OF BAR 6  OF pop_02d !'a' $vAcceso
SET SKIP OF BAR 7  OF pop_02d !'b' $vAcceso
SET SKIP OF BAR 8  OF pop_02d !'c' $vAcceso
SET SKIP OF BAR 9  OF pop_02d !'d' $vAcceso
SET SKIP OF BAR 10 OF pop_02d !'e' $vAcceso 
SET SKIP OF BAR 11 OF pop_02d !'f' $vAcceso &&Bayovar


SET SKIP OF BAR 1  OF pop_02e !'g' $vAcceso
SET SKIP OF BAR 2  OF pop_02e !'h' $vAcceso
SET SKIP OF BAR 3  OF pop_02e !'i' $vAcceso
SET SKIP OF BAR 4  OF pop_02e !'j' $vAcceso

SET SKIP OF BAR 1  OF pop_02f !'k' $vAcceso
SET SKIP OF BAR 2  OF pop_02f !'l' $vAcceso
SET SKIP OF BAR 3  OF pop_02f !'m' $vAcceso
SET SKIP OF BAR 4  OF pop_02f !'n' $vAcceso
SET SKIP OF BAR 5  OF pop_02f !'¤' $vAcceso
SET SKIP OF BAR 6  OF pop_02f !'o' $vAcceso
SET SKIP OF BAR 7  OF pop_02f !'p' $vAcceso
SET SKIP OF BAR 8  OF pop_02f !'q' $vAcceso

SET SKIP OF BAR 1  OF pop_02i !'r' $vAcceso
SET SKIP OF BAR 2  OF pop_02i !'s' $vAcceso

SET SKIP OF BAR 1  OF pop_02g !'t' $vAcceso
SET SKIP OF BAR 2  OF pop_02g !'u' $vAcceso
SET SKIP OF BAR 3  OF pop_02g !'v' $vAcceso
SET SKIP OF BAR 4  OF pop_02g !'w' $vAcceso
SET SKIP OF BAR 5  OF pop_02g !'x' $vAcceso
SET SKIP OF BAR 6  OF pop_02g !'y' $vAcceso

SET SKIP OF BAR 1  OF pop_02h !'z' $vAcceso
SET SKIP OF BAR 2  OF pop_02h !'1' $vAcceso
SET SKIP OF BAR 3  OF pop_02h !'2' $vAcceso
SET SKIP OF BAR 4  OF pop_02h !'3' $vAcceso
SET SKIP OF BAR 5  OF pop_02h !'4' $vAcceso
SET SKIP OF BAR 6  OF pop_02h !'5' $vAcceso

SET SKIP OF BAR 1  OF pop_02ha !'6' $vAcceso  
SET SKIP OF BAR 2  OF pop_02ha !'7' $vAcceso
SET SKIP OF BAR 3  OF pop_02ha !'8' $vAcceso
SET SKIP OF BAR 4  OF pop_02ha !'9' $vAcceso
SET SKIP OF BAR 5  OF pop_02ha !'0' $vAcceso
SET SKIP OF BAR 6  OF pop_02ha !'!' $vAcceso

SET SKIP OF BAR 1  OF pop_02j !'«' $vAcceso  
SET SKIP OF BAR 2  OF pop_02j !'¬' $vAcceso  
SET SKIP OF BAR 3  OF pop_02j !'@' $vAcceso
SET SKIP OF BAR 4  OF pop_02j !'#' $vAcceso
SET SKIP OF BAR 5  OF pop_02j !'$' $vAcceso
SET SKIP OF BAR 6  OF pop_02j !'%' $vAcceso
SET SKIP OF BAR 7  OF pop_02j !'^' $vAcceso
SET SKIP OF BAR 8  OF pop_02j !'&' $vAcceso  
SET SKIP OF BAR 9  OF pop_02j !'*' $vAcceso
SET SKIP OF BAR 10 OF pop_02j !'(' $vAcceso
SET SKIP OF BAR 11 OF pop_02j !')' $vAcceso
SET SKIP OF BAR 12 OF pop_02j !'-' $vAcceso
SET SKIP OF BAR 13 OF pop_02j !'+' $vAcceso
SET SKIP OF BAR 14 OF pop_02j !'=' $vAcceso
SET SKIP OF BAR 15 OF pop_02j !'_' $vAcceso
SET SKIP OF BAR 16 OF pop_02j !'/' $vAcceso
SET SKIP OF BAR 17 OF pop_02j !'›' $vAcceso 
SET SKIP OF BAR 18 OF pop_02j !'' $vAcceso 


* No acceso a programacion
vOpcion = .T.
FOR I = 1 TO 3
  IF !SKPBAR('pop_02a',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 1  OF menu vOpcion


* No acceso a obtenciones
vOpcion = .T.
FOR I = 1 TO 12
  IF !SKPBAR('pop_02b',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 2  OF menu vOpcion


* No acceso a almacen
vOpcion = .T.
FOR I = 1 TO 3
  IF !SKPBAR('pop_02c',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 3 OF menu vOpcion


* No acceso a polizas
vOpcion = .T.
FOR I = 1 TO 5
  IF !SKPBAR('pop_02ca',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 4 OF pop_02c vOpcion


* No acceso a servicios
vOpcion = .T.
FOR I = 1 TO 11
  IF !SKPBAR('pop_02d',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 4 OF menu vOpcion


* No acceso a Kardex
vOpcion = .T.
FOR I = 1 TO 4
  IF !SKPBAR('pop_02e',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 5 OF menu vOpcion


* No acceso a Catalogos
vOpcion = .T.
FOR I = 1 TO 8
  IF !SKPBAR('pop_02f',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 7 OF menu vOpcion


* No acceso a tablas
vOpcion = .T.
FOR I = 1 TO 6
  IF !SKPBAR('pop_02g',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 8 OF menu vOpcion


* No acceso a Utilitarios
vOpcion = .T.
FOR I = 1 TO 7
  IF !SKPBAR('pop_02h',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 9 OF menu vOpcion


* No acceso a Utilitarios
vOpcion = .T.
FOR I = 1 TO 4
  IF !SKPBAR('pop_02ha',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 4 OF pop_02h vOpcion


* No acceso a Consultas
vOpcion = .T.
FOR I = 1 TO 18
  IF !SKPBAR('pop_02j',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 10 OF menu vOpcion

RETURN


FUNCTION LastDay
*---------------
PARAMETERS Zmes,Zano
*- Devuelve una Fecha que corresponde al £ltimo dia del mes y a¤o
*- por J.F.G.
IF PARAMETERS()=1
   Zano = year(date())-1900
ENDIF   
vDia = '31'
vMes = PADL(ALLTRIM(STR(Zmes)),2,'0')
IF !BETWEEN(vMes,'01','12')
   RETURN CTOD('')
ENDIF   
vAno = ALLTRIM(STR(Zano))
vCentury = SET("CENTURY")
IF !BETWEEN(vano,'00','99')
   SET CENTURY ON
ENDIF   
vRet = CTOD(vDia+'/'+vMes+'/'+vAno)
DO WHILE EMPTY(vRet)
         vDia = STR(VAL(vDia)-1,2)
         vRet = CTOD(vDia+'/'+vMes+'/'+vAno)
ENDDO
SET CENTURY &vCentury        
RETURN vRet

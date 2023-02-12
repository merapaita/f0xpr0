* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  Programa SubPrincipal : Cpresu.Prg                                 º
* º                                                                     º
* º  ®®  Sistema de Administraci¢n e Informaci¢n REGION GRAU ¯¯         º
* º                                                                     º
* º  Software : Fox Pro 2.x                                             º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

CLEAR MEMO
SET ESCAPE OFF
CLEAR
CLOSE ALL
CLEAR ALL

SET TALK OFF
SET ECHO OFF

worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO Sai_lib
DO fox_ambi
public vCoddep,vNumdep
VCODDEP = '070100'
VNUMDEP = 6

* if !worker
* OK=VE_PASSW('FABER')
* IF !OK
*    RETURN
* ENDIF
* endif

RUTA='H:\SISRG\DATA96'
RUTApr='H:\SISFTE\DATApr'

SET PATH TO &RUTA
DESA = " CPRESU "
IF WorKer
   IF YesNo("Datos de Prueba?")
      DESA = " CPRESU - PRUEBA "
      SET PATH TO &RUTAPR
   ENDIF
ENDIF

*-- Clave de acceso
*vUser_ID = SPACE(5)
*vUser_ID = ALLTRIM(LEFT(SYS(0),15))

*IF !FILE( '&ruta\USUARIO.CDX')
*  DO STANDBY WITH ' Error en INDEXA '
*ENDIF

*public vCoddep

*USE Usuario ORDER TAG Usuario1  ALIAS Usua
*IF !Clave(vUser_ID,19,30,23,55)
*   CLOSE DATA
*   DO STANDBY WITH '!! Acceso Denegado !!',10,15
*   IF Worker
*      CANCEL
*    ENDIF
*   QUIT
*ENDIF
*vNombre  = ALLTRIM(nombre)
*USE

*USE IteUsu ORDER TAG IteUsu1   ALIAS IteU
*vUsuCla = CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
*vAcceso = ''

*SEEK vUsuCla

*SCAN WHILE vUsuCla = ALLTRIM(IteU.UsuCla)
*     vAcceso = vAcceso + IteU.Modulo
*ENDSCAN

*USE

**-- Definiciones para el Men£ --**
*----------------------------------*
CIA  = " Regi¢n Grau "


*TITU = IIF(!WORKER," Sistema de Contabilidad Presupuestal "," Sistema de Contabilidad Presupuestal [DATOS DE PRUEBA] ")
TITU = " Sistema de Contabilidad Presupuestal "+ "("+ALLTRIM(SUBSTR(SYS(0),1,10))+")"
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

*usuarios = '® USER: '+vnombre+' ¯'
*fecha    = DTOC(DATE()) +space(2)
*Rotulo2  = PADR(Desa,17,' ')+PADC(Usuarios,46,' ')+PADL(Fecha,17,' ')

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
SET MESSAGE TO 23 CENTER
SET CLOCK ON
*SET CLOCK TO 00,67
STORE .t. TO vEn_sistema
STORE .f. TO esc_tecla
= SYS(2002,1)        && Set cursor on

*--- El men£ principal ---**
L_COL =" W+/BG, N/W, N/W, GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "

* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º Contabilidad Presupuestal º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

IF escolor
   DEFINE POPUP menu FROM 5,2 TO 15,21 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP menu FROM 5,2 TO 15,21 COLOR SCHEME C_POPUP
ENDIF

DEFINE BAR 1  OF  menu PROMPT ' \<Afectaci¢n      ' MESSAGE ''
DEFINE BAR 2  OF  menu PROMPT ' \<Documentos     ' MESSAGE ''
DEFINE BAR 3  OF  menu PROMPT ' \<Consultas      ' MESSAGE ''
DEFINE BAR 4  OF  menu PROMPT ' \<Procesos       ' MESSAGE ''
DEFINE BAR 5  OF  menu PROMPT ' \<Tablas         ' MESSAGE ''
DEFINE BAR 6  OF  menu PROMPT ' \<Abastecimiento ' MESSAGE ''
DEFINE BAR 7  OF  menu PROMPT ' \<Utilitarios    ' MESSAGE ''
DEFINE BAR 8  OF  menu PROMPT ' \<Salida          '

*- Afectacion
IF escolor
   DEFINE POPUP pop_01 FROM  6,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_01 FROM  6,26 COLOR SCHEME c_popup
ENDIF

*DEFINE BAR 1  OF  pop_01 PROMPT '\<Autorizaci¢n H/C          ' MESSAGE ''
DEFINE BAR 1  OF  pop_01 PROMPT '\<Registro H/C: Sectores   ' MESSAGE ''
DEFINE BAR 2  OF  pop_01 PROMPT 'registro hoja \<Modificaci¢n ' MESSAGE ''
DEFINE BAR 3  OF  pop_01 PROMPT '\-' MESSAGE ''
DEFINE BAR 4  OF  pop_01 PROMPT '\<Parte anulaci¢n O/C ' MESSAGE ''
DEFINE BAR 5  OF  pop_01 PROMPT 'parte \<Rebaja    O/C ' MESSAGE ''
DEFINE BAR 6  OF  pop_01 PROMPT '\-' MESSAGE ''
DEFINE BAR 7  OF  pop_01 PROMPT '\<Parte anulaci¢n O/S ' MESSAGE ''
DEFINE BAR 8  OF  pop_01 PROMPT 'parte \<Rebaja    O/S ' MESSAGE ''
DEFINE BAR 9  OF  pop_01 PROMPT '\-' MESSAGE ''
DEFINE BAR 10 OF  pop_01 PROMPT '\<Orden de compra     ' MESSAGE ''
DEFINE BAR 11 OF  pop_01 PROMPT 'o\<Rden de servicio   ' MESSAGE ''
DEFINE BAR 12 OF  pop_01 PROMPT '\<Comprobante de Pago ' MESSAGE ''

*DEFINE BAR 1  OF  pop_01 PROMPT 'registro hoja \<Control     ' MESSAGE ''
*- Consultas

IF escolor
   DEFINE POPUP pop_02 FROM  7,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02 FROM  7,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02 PROMPT '\<Saldo presupuestal       ' MESSAGE ''
DEFINE BAR 2  OF  pop_02 PROMPT '\<Auxiliar de compromisos  ' MESSAGE ''
DEFINE BAR 3  OF  pop_02 PROMPT '\<Nota de contabilidad     ' MESSAGE ''
DEFINE BAR 4  OF  pop_02 PROMPT '\<Movimiento presupuestal  ' MESSAGE ''
DEFINE BAR 5  OF  pop_02 PROMPT '\<Consulta calendario     ' MESSAGE ''
DEFINE BAR 6  OF  pop_02 PROMPT 'saldo \<Ejecuci¢n          ' MESSAGE ''

*- Procesos
IF escolor
   DEFINE POPUP pop_03 FROM  8,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_03 FROM  8,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_03 PROMPT '\<Estado ejecuci¢n mensual E-5 ' MESSAGE ''
DEFINE BAR 2  OF  pop_03 PROMPT '\<Listado E-5 (Inversi¢n)      ' MESSAGE ''
DEFINE BAR 3  OF  pop_03 PROMPT '\<Cierre del mes       ' MESSAGE ''
DEFINE BAR 4  OF  pop_03 PROMPT '\<Reapertura del mes   ' MESSAGE ''

*- Tablas
IF escolor
   DEFINE POPUP pop_05 FROM 09,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_05 FROM 09,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_05 PROMPT '\<Plan contable    ' MESSAGE ''
DEFINE BAR 2  OF  pop_05 PROMPT '\<Tipo de Documento' MESSAGE ''
DEFINE BAR 3  OF  pop_05 PROMPT '\<Lista Proveedores' MESSAGE ''
DEFINE BAR 4  OF  pop_05 PROMPT '\<Correlativo H/C  ' MESSAGE ''
DEFINE BAR 5  OF  pop_05 PROMPT 't\<Ipo H/M         ' MESSAGE ''
DEFINE BAR 6  OF  pop_05 PROMPT 'tip\<O operaci¢n   ' MESSAGE ''
DEFINE BAR 7  OF  pop_05 PROMPT '\<Verif. H/C-C/P   ' MESSAGE ''

*- ABASTECIMIENTO
IF escolor
   DEFINE POPUP pop_06 FROM 10,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_06 FROM 10,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_06 PROMPT '\<Cuadro necesidades' MESSAGE ''
DEFINE BAR 2  OF  pop_06 PROMPT '\<Pe.co.sa.         ' MESSAGE ''
DEFINE BAR 3  OF  pop_06 PROMPT '\<Solicitud Servicio' MESSAGE ''


*- Utilitarios
IF escolor
   DEFINE POPUP pop_07 FROM 11,26 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_07 FROM 11,26 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_07 PROMPT '\<Indexar archivos             '
DEFINE BAR  2 OF pop_07 PROMPT '\<Backup de los archivos       '
DEFINE BAR  3 OF pop_07 PROMPT 'De\<puraci¢n de backups        '
DEFINE BAR  4 OF pop_07 PROMPT '\-'
DEFINE BAR  5 OF pop_07 PROMPT '\<Seguridad         ' MESSAGE '®® Acceso RESTRINGIDO ¯¯'

*DEFINE BAR  4 OF pop_07 PROMPT '\<Mantenimiento Correlativos    '
*DEFINE BAR  6 OF pop_07 PROMPT 'Ir al D\<OS         ' MESSAGE '®® Escriba EXIT para regresar ¯¯'
*EFINE BAR  7 OF pop_07 PROMPT 'Ir al \<Word Perfect' MESSAGE '®® Escriba EXIT para regresar ¯¯'
*EFINE BAR  8 OF pop_07 PROMPT 'Ir al \<QUATTRO PRO'  MESSAGE '®® Escriba EXIT para regresar ¯¯'

IF escolor
   DEFINE POPUP pop_02e FROM 11,56 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02e FROM 11,56 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02e PROMPT '\<Saldos calendario ' MESSAGE ''
DEFINE BAR 2  OF  pop_02e PROMPT '\<Auxliar Standard  ' MESSAGE ''
DEFINE BAR 3  OF  pop_02e PROMPT 'saldos asi\<Gnaci¢n ' MESSAGE ''

*DEFINE BAR 3  OF  pop_02e PROMPT '\<Aux.Standard/Gen‚rica  ' MESSAGE ' Por Partida Gen‚rica '
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
DO CASE
   CASE BAR() = 1
   
* DO REGHC WITH 1
* DO REGHC2 WITH 1

 DO REGHC3 WITH 1
 
 *- Es la ultima Versi¢n 
        * DO REGHC1 WITH 1
        * DO REGHC3 NEWEST
        *- NUEVA VERSION EN PRUEBA
   CASE BAR() = 2
        ON SELECTION POPUP pop_01  DO  menu_01
        ACTIVATE POPUP pop_01
   CASE BAR() = 3
        ON SELECTION POPUP pop_02  DO  menu_02
        ACTIVATE POPUP pop_02
   CASE BAR() = 4
        ON SELECTION POPUP pop_03  DO  menu_03
        ACTIVATE POPUP pop_03
*  CASE BAR() = 4
*        ON SELECTION POPUP pop_04  DO  menu_04
*        ACTIVATE POPUP pop_04
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


* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º Contabilidad Presupuestal º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

PROCEDURE menu_01   && Afectacion
*-----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
         *DO VERHJC
         * DO REGHC2 WITH 2
         DO REGHC3 WITH 2
         *-------------------------------------
         * Cambiar por la nueva Versi¢n REGHC1
         *-------------------------------------
    CASE choice = 2
         DO HOJMOD
         *-------------------------------------
         * Cambiar por la nueva Versi¢n hojmod1
         *-------------------------------------
         * DO HOJMOD1
    CASE choice = 4
         DO anupar WITH 1
    CASE choice = 5
         DO rebpar WITH 1
    CASE choice = 7
         DO anupar WITH 2
    CASE choice = 8
         DO rebpar WITH 2
    CASE choice = 10
         DO ordcom_c      
    CASE choice = 11
         DO ordser_c
    CASE choice = 12
         DO REGCP_c
  ENDCASE
  IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_01  DO menu_01
  ENDIF
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_01,menu
  HIDE WINDOW ALL
  RETURN



PROCEDURE menu_02   && Consultas
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
         DO SALPREt
    CASE choice = 2
         do acp
    CASE choice = 3
         Do NOTCON2
    CASE choice = 4
         * DO MOVPRESU
         DO MOVPREFM
    CASE choice = 5
         ON SELECTION POPUP pop_02e  DO  menu_02e
         ACTIVATE POPUP Pop_02e
    CASE choice = 6
         DO saleje
  ENDCASE
  IF !escolor
     RESTORE SCREEN FROM PRINCIPAL
     ON SELECTION POPUP pop_02  DO menu_02
  ENDIF
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_02,menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE menu_03   && Procesos
*-----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
         DO LIS_E5
    CASE choice = 2
         DO LIS_E5I             
    CASE choice = 3
         OK=VE_PASSW('ZXXZ')
         IF OK
            use parmae in 0 order tag parmae1 alias parma
            vmes = '  '
            ACTIVATE window STANDBY
            @ 1,1 SAY 'Indique el mes a Cerrar -> ' get vmes valid val_para(vmes,'FECMES','C')
            read
            deACTIVATE window STANDBY
            select parma
            seek 'HOJCON'+ALLTRIM(VMES)
            IF !FOUND()
               DO STANDBY WITH 'No se ha inicializado el Mes'
            ELSE
               IF CodigoAux='00'
                 DO STANDBY WITH 'Ya est  cerrado el Mes'
               ELSE
              	 IF RLOCK()
                	  REPLACE CodigoAux with '00'
               	 ENDIF
               ENDIF
            ENDIF   
         ENDIF 
         close data
    CASE choice = 4
         OK=VE_PASSW('ZXXZ')
         IF OK
            use parmae in 0 order tag parmae1 alias parma
            vmes = '  '
            ACTIVATE window STANDBY
            @ 1,1 SAY 'Indique el mes a Reaperturar -> ' get vmes valid val_para(vmes,'FECMES','C')
            read
            deACTIVATE window STANDBY
            select parma
            seek 'HOJCON'+ALLTRIM(VMES)
            IF !FOUND()
               DO STANDBY WITH 'No se ha inicializado el Mes'
            ELSE
               IF CodigoAux='  '
                 DO STANDBY WITH 'A£n no se ha cerrado el Mes'
               ELSE
	             IF RLOCK()
                    REPLACE CodigoAux with '  '
                 ENDIF
               endif
            ENDIF   
         ENDIF 
         close data
  ENDCASE
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_03,menu
  HIDE WINDOW ALL
  RETURN

PROCEDURE menu_04   && Consolidacion
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
    CASE choice = 2
    CASE choice = 3
  ENDCASE
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_04,menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE menu_05   && Consolidacion
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
      DO cuentas
    CASE choice = 2
      DO TipDoc
    CASE choice = 3
      DO ProMae      
    CASE choice = 4
      DO manpar WITH "HOJCON","Correlativo Hoja de Control","Correlaivos Hojas de Control",4,"Valor siguiente"
    CASE choice = 5
      DO manpar WITH "HOJMOD","Correlativo Hoja de Modificaci¢n","Tipos de Hoja de Modificaci¢n",1,"Valor siguiente"
    CASE choice = 6
      DO manpar WITH "OPERAC","Tipo Operaci¢n","Tipos de Operaci¢n",1,"Valor siguiente"
    CASE choice = 7
      DO VerHCCP  
      
  ENDCASE
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_05,menu
  HIDE WINDOW ALL
  RETURN

PROCEDURE menu_06   &&
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
         *DO regcdr
    CASE choice = 2
         *do regpec
    CASE choice = 3
         do regsol
  ENDCASE
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_06,menu
  HIDE WINDOW ALL
  RETURN

PROCEDURE menu_02e  && Saldos de Calendario
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  Indexa
    	DO SalCal
    CASE choice = 2
    	DO SalAxs
    CASE choice = 3
        DO SASINA
    	* DO SalAsgQ
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02e  DO menu_02e
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02e,menu
 HIDE WINDOW ALL
 RETURN




PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  DEACTIVATE WINDOW ALL
  DEACTIVATE MENU
  RETURN


PROCEDURE menu_07  && Utilitarios
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  Indexa
      =PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar el proceso de reordenamiento sin ning£n problema')
      DO IndexCP
    CASE choice = 2
      =PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar la copia de seguridad sin ning£n problema')
      DO BackUp
    CASE choice = 3
      DO ResBK
    CASE choice = 4
    CASE choice = 5
      DO SEGCP    
    CASE choice = 7
    CASE choice = 8
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_07  DO menu_07
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_07,menu
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
     HIDE POPUP ALL
     RESTORE SCREEN FROM Principal
     DO CLRSCR1
     SET HELP ON
     DEACTIVATE WINDOW ALL
     IF worker
        CANCEL
     ENDIF
     QUIT
  ENDIF
  RETURN


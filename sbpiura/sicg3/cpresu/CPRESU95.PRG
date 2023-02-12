* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  Programa SubPrincipal : Cpresu.Prg                                 º
* º                                                                     º
* º  ®®  Sistema de Administraci¢n e Informaci¢n REGION GRAU ¯¯         º
* º                                                                     º
* º  Empresa : REGION GRAU  PIURA-PERU                                  º
* º                                                                     º
* º  Software : Fox Pro 2.x                                             º
* º                                                                     º
* º  Inicio   : Agosto 1994                                             º
* º                                                                     º
* º      M¢dulo  Contabilidad Presupuestal Ing Luis Castillo            º
* º                                                                     º
* º  Inicio de Operaci¢n:                                               º
* º      Fecha: 02/01/95                                                º
* º                                                                     º
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

if !worker
OK=VE_PASSW('FABER')
IF !OK
   RETURN
ENDIF
endif
PUBLIC PASE
PASE = 0
SET PATH TO H:\SISRG\DATA
IF WorKer
   IF YesNo("Datos de Prueba?")
      SET PATH TO H:\SISFTE\DATAPR
   ENDIF
ENDIF


**-- Definiciones para el Men£ --**
*----------------------------------*
CIA  = " Regi¢n Grau "

DESA = " UDEP "
*TITU = IIF(!WORKER," Sistema de Contabilidad Presupuestal "," Sistema de Contabilidad Presupuestal [DATOS DE PRUEBA] ")
TITU = " Sistema de Contabilidad Presupuestal "+ "("+ALLTRIM(SUBSTR(SYS(0),1,2))+")"
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
DEFINE BAR 6  OF  menu PROMPT ' \<Utilitarios    ' MESSAGE ''
DEFINE BAR 7  OF  menu PROMPT ' \<Salida          '

*- Afectacion
IF escolor
   DEFINE POPUP pop_01 FROM  6,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_01 FROM  6,26 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_01 PROMPT '\<Autorizaci¢n H/C          ' MESSAGE ''
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
DEFINE BAR 5  OF  pop_02 PROMPT '\<Consulta de calendario  ' MESSAGE ''


*- Procesos
IF escolor
   DEFINE POPUP pop_03 FROM  8,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_03 FROM  8,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_03 PROMPT '\<Estado ejecuci¢n mensual E-5 ' MESSAGE ''
DEFINE BAR 2  OF  pop_03 PROMPT '\<Cierre del mes       ' MESSAGE ''
DEFINE BAR 3  OF  pop_03 PROMPT '\<Reapertura del mes   ' MESSAGE ''

*- Consolidacion
IF escolor
   DEFINE POPUP pop_04 FROM  9,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_04 FROM  9,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_04 PROMPT '\<Registro/recepcion  ' MESSAGE ''
DEFINE BAR 2  OF  pop_04 PROMPT '\<Informe consolidado ' MESSAGE ''

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
*DEFINE BAR 7  OF  pop_05 PROMPT '\<Obras            ' MESSAGE ''

*- Utilitarios
IF escolor
   DEFINE POPUP pop_06 FROM 10,26 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_06 FROM 10,26 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_06 PROMPT '\<Indexar archivos             '
DEFINE BAR  2 OF pop_06 PROMPT '\<Backup de los archivos       '
DEFINE BAR  3 OF pop_06 PROMPT 'De\<puraci¢n de backups        '
DEFINE BAR  4 OF pop_06 PROMPT '\<Mantenimiento Correlativos    '
DEFINE BAR  5 OF pop_06 PROMPT '\-'
DEFINE BAR  6 OF pop_06 PROMPT 'Ir al D\<OS         ' MESSAGE '®® Escriba EXIT para regresar ¯¯'
*EFINE BAR  7 OF pop_06 PROMPT 'Ir al \<Word Perfect' MESSAGE '®® Escriba EXIT para regresar ¯¯'
*EFINE BAR  8 OF pop_06 PROMPT 'Ir al \<QUATTRO PRO'  MESSAGE '®® Escriba EXIT para regresar ¯¯'

IF escolor
   DEFINE POPUP pop_07 FROM 11,56 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_07 FROM 11,56 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_07 PROMPT '\<Saldos calendario ' MESSAGE ''
DEFINE BAR 2  OF  pop_07 PROMPT '\<Auxiliar Standar  ' MESSAGE ''
DEFINE BAR 3  OF  pop_07 PROMPT 'saldos \<Asignaci¢n ' MESSAGE ''

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
        *DO REGHJC
        DO REGHC WITH 1
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
*        DO VERHJC
    CASE choice = 2
         DO HOJMOD
    CASE choice = 4
         DO anupar WITH 1
    CASE choice = 5
         DO rebparX WITH 1
    CASE choice = 7
         DO anupar WITH 2
    CASE choice = 8
         DO rebparX WITH 2
    CASE choice = 10
         DO ordcom_c      
    CASE choice = 11
         DO ordser_c
         
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
*        DO SALPRET
    CASE choice = 2
*        DO AUXCOM         
    CASE choice = 3
*        DO NOTCON
    CASE choice = 4
*        DO MOVPRE
    CASE choice = 5
         ON SELECTION POPUP pop_07  DO  menu_07
         ACTIVATE POPUP pop_07
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
         *DO REPO_E5
*        DO EJE_E5
    CASE choice = 2
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
    CASE choice = 3
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

PROCEDURE menu_07  && Saldos de Calendario
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  Indexa
    	DO SalCal
    CASE choice = 2
    	DO SalAxs
    CASE choice = 3
    	DO SalAsig
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_07  DO menu_07
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_07,menu
 HIDE WINDOW ALL
 RETURN




PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  DEACTIVATE WINDOW ALL
  DEACTIVATE MENU
  RETURN


PROCEDURE menu_06  && Utilitarios
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
      DO DepBK
    CASE choice = 6
    CASE choice = 7
    CASE choice = 8
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_06  DO menu_06
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_06,menu
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


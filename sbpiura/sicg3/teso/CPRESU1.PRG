*靈컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴?
*?  Programa SubPrincipal : Cp99.Prg                                   ?
*?                                                                     ?
*聃컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴?
SET ESCAPE OFF
CLEAR
CLOSE ALL
CLEAR ALL
SET EXACT OFF
SET TALK OFF
SET ECHO OFF
worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO SAI_LIB.PRG
DO fox_ambi
public vCoddep,vNumdep,DESUSE,vuser_id,vConex,vMaq,vLlav,vUsurec,VFLAG,sistema

sistema='1'
vConex = ALLTRIM(LEFT(SYS(0),8))
vmaq   = ALLTRIM(RIGHT(SYS(0),2))
vllav  = ALLTRIM(sys(2))

*--------------------------
* NUEVA RUTA DE ACCESO
*
*SET PATH TO H:\SISTEMA\DATA
SET PATH TO C:\BENE99\DATA
*=SAVCON()

IF WorKer
   IF YesNo("Datos de Prueba?")
      SET PATH TO c:\BENE99\DATAPR
   ENDIF
ENDIF
IF  LASTKEY()#27 

**-- Definiciones para el Men? --**
*----------------------------------*
CIA  = " SBP - PIURA"
SUBC = "AREA  DE  "
DESA = "     SISTEMAS"
TITU = " Sistema de Contabilidad Presupuestal " + "("+ALLTRIM(SUBSTR(SYS(0),1,10))+")"
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


**-- Definiciones para el Men? --**
*----------------------------------*
CIA  = "  SBP - PIURA"

*=SAVCON()
vflag = ' '
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

@  1, 0,23,79  BOX "같같같같?" COLOR SCHEME c_fondo
@  1, 1,23,24  BOX "굇굇굇굇?" COLOR SCHEME c_fondo

spac = (80 -(LEN(CIA)+LEN(TITU)+10))/2
Rotulo1 = CIA + SPACE(spac) + TITU + SPAC(spac) + space(11)

spac = (79 - (LEN(DESA)+LEN(DESUSE)+10))/2
Rotulo2 = DESA + SPACE(spac)+'? '+DESUSE+' ?'+SPAC(spac)+DTOC(DATE())

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
STORE .t. TO vEn_sistema
STORE .f. TO esc_tecla
= SYS(2002,1)        && Set cursor on

*--- El men? principal ---**
 L_COL  ="3/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "
* 靈컴컴컴컴컴컴컴컴컴컴컴컴컴?
* ? Contabilidad Presupuestal ?
* 聃컴컴컴컴컴컴컴컴컴컴컴컴컴?

IF escolor
   DEFINE POPUP menu FROM 5,2 TO 15,21 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP menu FROM 5,2 TO 15,21 COLOR SCHEME C_POPUP
ENDIF

DEFINE BAR 1  OF  menu PROMPT '\<Afectaci줻      ' MESSAGE ''  SKIP FOR !fmv 
DEFINE BAR 2  OF  menu PROMPT '\<Documentos     ' MESSAGE ''
DEFINE BAR 3  OF  menu PROMPT '\<Consultas      ' MESSAGE ''
DEFINE BAR 4  OF  menu PROMPT '\<Procesos       ' MESSAGE ''
DEFINE BAR 5  OF  menu PROMPT '\<Tablas         ' MESSAGE ''
DEFINE BAR 6  OF  menu PROMPT '\<Abastecimiento ' MESSAGE ''
DEFINE BAR 7  OF  menu PROMPT '\<Utilitarios    ' MESSAGE ''
DEFINE BAR 8  OF  menu PROMPT '\<Salida          '

*- Afectaci줻
IF escolor
   DEFINE POPUP pop_07 FROM  6,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_07 FROM  6,26 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_07 PROMPT '\<Normal        '  MESSAGE ''
DEFINE BAR 2  OF  pop_07 PROMPT 'De \<Emergencia '  MESSAGE ''

*- Documentos
IF escolor
   DEFINE POPUP pop_01 FROM  6,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_01 FROM  6,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_01 PROMPT 'Registro H/C: \<Sectores   ' MESSAGE ''
DEFINE BAR 2  OF  pop_01 PROMPT 'Registro hoja \<Modificaci줻 ' MESSAGE ''
DEFINE BAR 3  OF  pop_01 PROMPT '\-' MESSAGE ''
DEFINE BAR 4  OF  pop_01 PROMPT 'Parte \<Anulaci줻 O/C ' MESSAGE ''
DEFINE BAR 5  OF  pop_01 PROMPT 'parte \<Rebaja    O/C ' MESSAGE ''
DEFINE BAR 6  OF  pop_01 PROMPT '\-' MESSAGE ''
DEFINE BAR 7  OF  pop_01 PROMPT 'Parte anulaci줻 \<O/S ' MESSAGE ''
DEFINE BAR 8  OF  pop_01 PROMPT '\<Parte Rebaja    O/S ' MESSAGE ''
DEFINE BAR 9  OF  pop_01 PROMPT '\-' MESSAGE ''
DEFINE BAR 10 OF  pop_01 PROMPT 'Orden de \<Compra     ' MESSAGE ''
DEFINE BAR 11 OF  pop_01 PROMPT 'Orden de ser\<Vicio   ' MESSAGE ''
DEFINE BAR 12 OF  pop_01 PROMPT 'Comprobante de Pa\<Go ' MESSAGE ''
DEFINE BAR 13 OF  pop_01 PROMPT '\<Hoja De Control       ' MESSAGE ''
DEFINE BAR 14 OF  pop_01 PROMPT 'Hoja \<De Modificaci줻  ' MESSAGE ''
DEFINE BAR 15 OF  pop_01 PROMPT '\<Notas de Contabilidad ' MESSAGE ''

*- Consultas
IF escolor
   DEFINE POPUP pop_02 FROM  7,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02 FROM  7,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02 PROMPT '\<Saldo presupuestal       ' MESSAGE ''
DEFINE BAR 2  OF  pop_02 PROMPT '\<Auxiliar de compromisos  ' MESSAGE ''
DEFINE BAR 3  OF  pop_02 PROMPT 'Au\<x.Com. A nivel de meta ' MESSAGE ''
DEFINE BAR 4  OF  pop_02 PROMPT '\<N/C de enero a marzo     ' MESSAGE ''
DEFINE BAR 5  OF  pop_02 PROMPT 'N/C de abril a la \<Fecha  ' MESSAGE ''
DEFINE BAR 6  OF  pop_02 PROMPT '\<Movimiento presupuestal  ' MESSAGE ''
DEFINE BAR 7  OF  pop_02 PROMPT '\<Consulta calendario     ' MESSAGE ''
DEFINE BAR 8  OF  pop_02 PROMPT 'saldo \<Ejecuci줻          ' MESSAGE ''
DEFINE BAR 9  OF  pop_02 PROMPT '\<Rebajas meses anteriores ' MESSAGE ''
DEFINE BAR 10 OF  pop_02 PROMPT 'Reporte AP-\<1             ' MESSAGE ''
DEFINE BAR 11 OF  pop_02 PROMPT 'Reporte AP-\<2             ' MESSAGE ''
DEFINE BAR 12 OF  pop_02 PROMPT 'Cadenas \<Funcionales      ' MESSAGE ''

*- Procesos
IF escolor
   DEFINE POPUP pop_03 FROM  8,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_03 FROM  8,26 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_03 PROMPT '\<Estado ejecuci줻 mensual E-5 ' MESSAGE ''
DEFINE BAR 2  OF  pop_03 PROMPT '\<Listado E-5 (Inversi줻)      ' MESSAGE ''
DEFINE BAR 3  OF  pop_03 PROMPT '\<Cierre del mes        ' MESSAGE ''
DEFINE BAR 4  OF  pop_03 PROMPT '\<Reapertura del mes    ' MESSAGE ''
DEFINE BAR 5  OF  pop_03 PROMPT '\<Generaci줻 del AstPre ' MESSAGE ''
DEFINE BAR 6  OF  pop_03 PROMPT '\<Verifica Detalle de HC' MESSAGE ''

*- Tablas
IF escolor
   DEFINE POPUP pop_04 FROM 09,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_04 FROM 09,26 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_04 PROMPT '\<Plan contable    ' MESSAGE ''
DEFINE BAR 2  OF  pop_04 PROMPT '\<Tipo de Documento' MESSAGE ''
DEFINE BAR 3  OF  pop_04 PROMPT '\<Auxiliares       ' MESSAGE ''
DEFINE BAR 4  OF  pop_04 PROMPT '\<Correlativo H/C  ' MESSAGE ''
DEFINE BAR 5  OF  pop_04 PROMPT 't\<Ipo H/M         ' MESSAGE ''
DEFINE BAR 6  OF  pop_04 PROMPT 'tip\<O operaci줻   ' MESSAGE ''
DEFINE BAR 7  OF  pop_04 PROMPT '\<Verif. H/C-C/P   ' MESSAGE ''

*- Abastecimientos
IF escolor
   DEFINE POPUP pop_05 FROM 10,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_05 FROM 10,26 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_05 PROMPT '\<Cuadro necesidades' MESSAGE ''
DEFINE BAR 2  OF  pop_05 PROMPT '\<Pe.co.sa.         ' MESSAGE ''
DEFINE BAR 3  OF  pop_05 PROMPT '\<Solicitud Servicio' MESSAGE ''

*- Utilitarios
IF escolor
   DEFINE POPUP pop_06 FROM 11,26 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_06 FROM 11,26 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_06 PROMPT '\<Indexar archivos             '
DEFINE BAR  2 OF pop_06 PROMPT '\<Backup de los archivos       '
DEFINE BAR  3 OF pop_06 PROMPT 'De\<puraci줻 de backups        '
DEFINE BAR  4 OF pop_06 PROMPT '\-'
DEFINE BAR  5 OF pop_06 PROMPT '\<Seguridad         ' MESSAGE '?? Acceso RESTRINGIDO ??'
DEFINE BAR  6 OF pop_06 PROMPT '\<Usuarios CPRESU   ' 


IF escolor
   DEFINE POPUP pop_02e FROM 11,56 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02e FROM 11,56 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_02e PROMPT '\<Saldos calendario ' MESSAGE ''
DEFINE BAR 2  OF  pop_02e PROMPT '\<Saldos c. Anal죜. ' MESSAGE ''
DEFINE BAR 3  OF  pop_02e PROMPT '\<Auxliar Standard  ' MESSAGE ''
DEFINE BAR 4  OF  pop_02e PROMPT 'saldos asi\<Gnaci줻 ' MESSAGE ''
DEFINE BAR 5  OF  pop_02e PROMPT '\<Compromisos del mes' MESSAGE ''
DEFINE BAR 6  OF  pop_02e PROMPT '\<Compromisos x Componente' MESSAGE ''


IF escolor
   DEFINE POPUP pop_02f FROM 11,56 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02f FROM 11,56 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_02f PROMPT '\<Acumulados  ' MESSAGE ''
DEFINE BAR 2  OF  pop_02f PROMPT '\<Mensualizado' MESSAGE ''

*DEFINE BAR 3  OF  pop_02e PROMPT '\<Aux.Standard/Gen굍ica  ' MESSAGE ' Por Partida Gen굍ica '
ON SELECTION POPUP menu DO ACT_MENUS

IF !vUser_ID$'JFGAMERICA'
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
RETURN

*-- Fin  del Programa Principal

PROCEDURE ACT_MENUS
*------------------
DO CASE
   CASE BAR() = 1
         DO REGHC  WITH 1,'1'
   CASE BAR() = 2
        ON SELECTION POPUP pop_01  DO  menu_01
        ACTIVATE POPUP pop_01
   CASE BAR() = 3
        ON SELECTION POPUP pop_02  DO  menu_02
        ACTIVATE POPUP pop_02
   CASE BAR() = 4
        ON SELECTION POPUP pop_03  DO  menu_03
        ACTIVATE POPUP pop_03
   CASE BAR() = 5
        ON SELECTION POPUP pop_04  DO  menu_04
        ACTIVATE POPUP pop_04
   CASE BAR() = 6
        ON SELECTION POPUP pop_05  DO  menu_05
        ACTIVATE POPUP pop_05
   CASE BAR() = 7
        ON SELECTION POPUP pop_06  DO  menu_06
        ACTIVATE POPUP pop_06
   CASE BAR() = 8
        DO Salmenu
ENDCASE

RESTORE SCREEN FROM principal
RETURN


* 靈컴컴컴컴컴컴컴컴컴컴컴컴컴?
* ? Contabilidad Presupuestal ?
* 聃컴컴컴컴컴컴컴컴컴컴컴컴컴?

PROCEDURE menu_01   && Documentos
*-----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
         DO REGHC WITH 2,'1'
    CASE choice = 2
          DO HOJMOD1 WITH '1'
    CASE choice = 4
         DO anupar WITH 1,'1'
    CASE choice = 5
         DO rebpar WITH 1,'1'
    CASE choice = 7
         DO anupar WITH 2,'1'
    CASE choice = 8
         DO rebpar WITH 2,'1'
    CASE choice = 10
         DO con_ocom WITH '','1'
    CASE choice = 11
         DO con_oser WITH '','1'
    CASE choice = 12
         DO Concpau2
    CASE choice = 13
         DO Conhc
    CASE choice = 14
         DO Conhm
    case choice = 15
         DO cons_nc
  ENDCASE
  IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_01  DO menu_01
  ENDIF
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_01,menu
  HIDE WINDOW ALL
  RETURN


PROCEDURE menu_07
*----------------
choice=BAR()
DO CASE
   CASE CHOICE=1
        DO REGHC  WITH 1,'1'
   CASE CHOICE=2
        DO REGEHC WITH 1,'2'
ENDCASE
IF !escolor
   RESTORE SCREEN FROM PRINCIPAL
   ON SELECTION POPUP pop_07  DO menu_07
ENDIF
  
RESTORE SCREEN FROM PRINCIPAL
SHOW POPUP pop_07,menu
HIDE WINDOW ALL
RETURN
     


PROCEDURE menu_02   && Consultas
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
         ON SELECTION POPUP pop_02f  DO  menu_02f
         ACTIVATE POPUP Pop_02f
    CASE choice = 2
         do acp2
    CASE choice = 3
         do acp4
    CASE choice = 4
         Do NOTCON2
    CASE choice = 5
	     IF Yesno("Nota de Contabilidad [Si] Cuentas X Docs. [No]")
     	     Do NOTCON3
	     ELSE
	   	     DO NCPCHK     
	     ENDIF     
    CASE choice = 6
         DO MOVPREFM
    CASE choice = 7
         ON SELECTION POPUP pop_02e  DO  menu_02e
         ACTIVATE POPUP Pop_02e
    CASE choice = 8
         DO consal
    CASE choice = 9
         DO rebant
    CASE choice = 10
         DO STANDBY WITH "Reporte en Desarrollo"
         * DO RepAp1
    CASE choice = 11
         DO STANDBY WITH "Reporte en Desarrollo"
         * DO RepAp2
    case choice = 12
         do concad
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
                 DO STANDBY WITH 'Ya est? cerrado el Mes'
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
                 DO STANDBY WITH 'A즢 no se ha cerrado el Mes'
               ELSE
	             IF RLOCK()
                    REPLACE CodigoAux with '  '
                 ENDIF
               endif
            ENDIF   
         ENDIF 
         close data
   CASE  choice = 5        
         DO ASTPRCP1
  case   choice = 6
         DO VERIFDHC
  ENDCASE
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_03,menu
  HIDE WINDOW ALL
  RETURN

PROCEDURE menu_04   && Tablas
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
      DO cuentas
    CASE choice = 2
      DO TipDoc
    CASE choice = 3
      * DO ProMae      
      DO Manaux2
    CASE choice = 4
      DO manpar WITH "HOJCON","Correlativo Hoja de Control","Correlaivos Hojas de Control",4,"Valor siguiente"
    CASE choice = 5
      DO manpar WITH "HOJMOD","Correlativo Hoja de Modificaci줻","Tipos de Hoja de Modificaci줻",1,"Valor siguiente"
    CASE choice = 6
      DO manpar WITH "OPERAC","Tipo Operaci줻","Tipos de Operaci줻",1,"Valor siguiente"
    CASE choice = 7
      DO VerHCCP  
      
  ENDCASE
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_04,menu
  HIDE WINDOW ALL
  RETURN

PROCEDURE menu_05   &&Abastecimientos
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
         *DO regcdr
    CASE choice = 2
         *do regpec WITH 2,'F'
    CASE choice = 3
         do regsol
  ENDCASE
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_05,menu
  HIDE WINDOW ALL
  RETURN

PROCEDURE menu_06  && Utilitarios
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  Indexa
      =PopError( 'Aseg즦ese que no est굈 usando el sistema, para poder ' +;
      'realizar el proceso de reordenamiento sin ning즢 problema')
      DO IndexCP
    CASE choice = 2
      =PopError( 'Aseg즦ese que no est굈 usando el sistema, para poder ' +;
      'realizar la copia de seguridad sin ning즢 problema')
      DO BackUp
    CASE choice = 3
      DO ResBK
    CASE choice = 5
        IF LEFT(SYS(0),3)$'FMVJFG'
	       DO SEGCP    
	    ENDIF   
    CASE choice = 6
      do contusu
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_06  DO menu_06
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_06,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_02e  && Saldos de Calendario
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1   && saldos de calendario
    	DO consal1
    CASE choice = 2   && saldos de calendario analitico
    	DO consal2
    CASE choice = 3
    	DO SalAxs
    CASE choice = 4
        DO SASINA
    case choice = 5
        do concomp
    case choice = 6
        do concomp2
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02e  DO menu_02e
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02e,menu
 HIDE WINDOW ALL
RETURN

PROCEDURE menu_02f  && Saldos de Presupuesto
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  Indexa
    	DO conpre
    CASE choice = 2
    	DO conpre1
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02f  DO menu_02f
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02f,menu
 HIDE WINDOW ALL
RETURN

PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  DEACTIVATE WINDOW ALL
  DEACTIVATE MENU
RETURN



PROCEDURE salmenu
*----------------
  STORE  YESNO('? Estamos terminando la sesi줻 ?') TO in_secoemsa
  IF in_secoemsa
    SET COLOR TO  
    CLOSE DATA
    HIDE POPUP ALL
    IF worker
      CLEAR
      SET HELP ON
      ON KEY
      *=borcon()      
      CANCEL
    ELSE
      RESTORE SCREEN FROM Principal
      DO CLRSCR
      *=borcon()
    ENDIF
    QUIT
  ELSE
     RETURN
  ENDIF

PROCEDURE ABREPAS
*----------------
**- Accesos a las opciones; seg즢 la seguridad especificada
* Acceso a Documentos
SET SKIP OF BAR 1  OF pop_01 !'A' $vAcceso
SET SKIP OF BAR 2  OF pop_01 !'B' $vAcceso
SET SKIP OF BAR 4  OF pop_01 !'C' $vAcceso
SET SKIP OF BAR 5  OF pop_01 !'D' $vAcceso
SET SKIP OF BAR 7  OF pop_01 !'E' $vAcceso
SET SKIP OF BAR 8  OF pop_01 !'F' $vAcceso
SET SKIP OF BAR 10 OF pop_01 !'G' $vAcceso
SET SKIP OF BAR 11 OF pop_01 !'H' $vAcceso
SET SKIP OF BAR 12 OF pop_01 !'I' $vAcceso
SET SKIP OF BAR 13 OF pop_01 !'n' $vAcceso
SET SKIP OF BAR 14 OF pop_01 !'o' $vAcceso

* acceso a Consultas
SET SKIP OF BAR 1  OF pop_02 !'J' $vAcceso
SET SKIP OF BAR 2  OF pop_02 !'K' $vAcceso
SET SKIP OF BAR 3  OF pop_02 !'L' $vAcceso
SET SKIP OF BAR 4  OF pop_02 !'M' $vAcceso
SET SKIP OF BAR 5  OF pop_02 !'N' $vAcceso
SET SKIP OF BAR 6  OF pop_02 !'O' $vAcceso
SET SKIP OF BAR 7  OF pop_02 !'P' $vAcceso
SET SKIP OF BAR 8  OF pop_02 !'Q' $vAcceso
SET SKIP OF BAR 9  OF pop_02 !'R' $vAcceso
SET SKIP OF BAR 10 OF pop_02 !'S' $vAcceso

* acceso a Procesos
SET SKIP OF BAR 1  OF pop_03 !'T' $vAcceso
SET SKIP OF BAR 2  OF pop_03 !'U' $vAcceso
SET SKIP OF BAR 3  OF pop_03 !'V' $vAcceso
SET SKIP OF BAR 4  OF pop_03 !'W' $vAcceso
SET SKIP OF BAR 5  OF pop_03 !'X' $vAcceso

* acceso a Tablas
SET SKIP OF BAR 1  OF pop_04 !'Y' $vAcceso
SET SKIP OF BAR 2  OF pop_04 !'Z' $vAcceso
SET SKIP OF BAR 3  OF pop_04 !'a' $vAcceso
SET SKIP OF BAR 4  OF pop_04 !'b' $vAcceso
SET SKIP OF BAR 5  OF pop_04 !'c' $vAcceso
SET SKIP OF BAR 6  OF pop_04 !'d' $vAcceso
SET SKIP OF BAR 7  OF pop_04 !'e' $vAcceso

* acceso a Abastecimientos
SET SKIP OF BAR 1  OF pop_05 !'f' $vAcceso
SET SKIP OF BAR 2  OF pop_05 !'g' $vAcceso
SET SKIP OF BAR 3  OF pop_05 !'h' $vAcceso

* acceso a Utilitarios
SET SKIP OF BAR 1  OF pop_06 !'i' $vAcceso
SET SKIP OF BAR 2  OF pop_06 !'j' $vAcceso
SET SKIP OF BAR 3  OF pop_06 !'k' $vAcceso
SET SKIP OF BAR 5  OF pop_06 !'l' $vAcceso
SET SKIP OF BAR 6  OF pop_06 !'m' $vAcceso

* No acceso a Documentos
vOpcion = .T.
FOR I = 1 TO 14
  IF !SKPBAR('pop_01',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 2  OF menu vOpcion

* No acceso a Consultas
vOpcion = .T.
FOR I = 1 TO 10
  IF !SKPBAR('pop_02',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 3  OF menu  vOpcion

* No acceso a Procesos
vOpcion = .T.
FOR I = 1 TO 5
  IF !SKPBAR('pop_03',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 4  OF menu vOpcion

* No acceso a Tablas
vOpcion = .T.
FOR I = 1 TO 7
  IF !SKPBAR('pop_04',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 5  OF menu vOpcion

* No acceso a Abastecimientos
vOpcion = .T.
FOR I = 1 TO 3
  IF !SKPBAR('pop_05',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 6  OF menu vOpcion

* No acceso a Utilitarios
vOpcion = .T.
FOR I = 1 TO 6
  IF !SKPBAR('pop_06',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 7  OF menu vOpcion
RETURN


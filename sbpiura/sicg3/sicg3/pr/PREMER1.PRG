* -----------------------------------------------------
*
*   Programa SubPrincipal : Presu.prg                                  
*                                                                      
*   ®®  Sistema de Administraci¢n e Informaci¢n REGION GRAU ¯¯         
*                                                                      
*   Empresa : REGION GRAU  PIURA-PERU                                  
*                                                                      
*   Software : Fox Pro 2.x                                             
*                                                                      
*   Inicio   : Agosto 1994                                             
*                                                                      
*   	    M¢dulo  Presupuesto  Ing Luis Castillo                         
*                                                                      
*   Inicio de Operaci¢n:                                               
*	       Fecha: 02/01/95                                                
*	Upgrade		:	Enero 2000
* -----------------------------------------------------
*                                                                      
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

public vCoddep,vNumdep
VCODDEP = '040200'
VNUMDEP = 6

RUTA='H:\EMER97\DATA'
REPOS='H:\SISRG\REPOS'

RUTApr='H:\EMER97\DATApr'
REPOSpr='H:\SISFTE\REPOS'

SET PATH TO &ruta
IF WorKer
   IF YesNo("¨ Datos de Prueba ?")
      SET PATH TO &rutapr
   ENDIF
ENDIF
*=savcon()
*-- Clave de acceso
*vUser_ID = SPACE(5)

vUser_ID = ALLTRIM(LEFT(SYS(0),15))

IF !FILE( '&ruta\USUARIO.CDX')
  DO STANDBY WITH ' Error en INDEXA '
ENDIF

public vCoddep,vConex,vMaq,vLlav,vUsurec,vFlag


vConex = ALLTRIM(LEFT(SYS(0),8))
vmaq   = ALLTRIM(RIGHT(SYS(0),2))
vllav  = ALLTRIM(sys(2))

=SAVCON()

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
vFlag = flag
USE

USE IteUsu ORDER TAG IteUsu1   ALIAS IteU
SELE iteu
SET FILT TO sistema='1'
vUsuCla = CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
vAcceso = ''

SEEK vUsuCla

SCAN WHILE vUsuCla = ALLTRIM(IteU.UsuCla)
     vAcceso = vAcceso + IteU.Modulo
ENDSCAN
SET FILT  TO
USE

**-- Definiciones para el Men£ --**
*----------------------------------*
CIA  = " Regi¢n Grau "
SUBC = "    CTAR     "
DESA = " OINF-RG "
TITU = " Sistema de Planificaci¢n y Presupuesto " + "("+ALLTRIM(SUBSTR(SYS(0),1,10))+")"

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
 @  1, 1,23,30  BOX "±±±±±±±±±" COLOR SCHEME c_fondo

spac = (80 -(LEN(CIA)+LEN(TITU)+10))/2
Rotulo1 = CIA + SPACE(spac) + TITU + SPAC(spac) + space(11)

*spac = 80 - LEN(DESA) - 8
*Rotulo2 = DESA + SPACE(spac) + DTOC(DATE()) + ' '

usuarios = '® USER: '+vnombre+' ¯'
fecha    = DTOC(DATE()) +space(2)
Rotulo2  = PADR(Desa,17,' ')+PADC(Usuarios,46,' ')+PADL(Fecha,17,' ')

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
= SYS(2002,1)        && Set cursor on

*--- El men£ principal ---**
*L_COL =" W+/BG, N/W, N/W, GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "
L_COL  ="W+/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "
                                       && +     
IF escolor
   DEFINE POPUP menu FROM 5,2 TO 18,27 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP menu FROM 5,2 TO 18,27 COLOR SCHEME C_POPUP
ENDIF

* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º       Presupuesto         º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

DEFINE BAR 1  OF  menu PROMPT ' asignaci¢n \<Presupuestal' MESSAGE ''
DEFINE BAR 2  OF  menu PROMPT ' asignaci¢n \<Calendarios ' MESSAGE ''
DEFINE BAR 3  OF  menu PROMPT ' \<Modific. presupuestales' MESSAGE ''
DEFINE BAR 4  OF  menu PROMPT ' \<Ejecuci¢n             ' MESSAGE ''
DEFINE BAR 5  OF  menu PROMPT ' e\<Valuaci¢n            ' MESSAGE ''
DEFINE BAR 6  OF  menu PROMPT ' cat logos y ta\<Blas    ' MESSAGE ''
DEFINE BAR 7  OF  menu PROMPT ' \<Abastecimiento        ' MESSAGE ''
DEFINE BAR 8  OF  menu PROMPT ' \<Utilitarios           ' MESSAGE ''
DEFINE BAR 9  OF  menu PROMPT ' co\<Nsultas             ' MESSAGE ''
DEFINE BAR 10 OF  menu PROMPT ' \<S a l i d a            '

*- PRESUPUESTO
IF escolor
   DEFINE POPUP pop_07 FROM 05,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_07 FROM 05,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_07 PROMPT ' \<A n u a l  '
DEFINE BAR  2 OF pop_07 PROMPT ' \<Trimestral - mensual'

*- modificaciones presupuestales
IF escolor
   DEFINE POPUP pop_08 FROM 05,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_08 FROM 05,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_08 PROMPT ' \<Transferencias de partidas     '
DEFINE BAR  2 OF pop_08 PROMPT ' \<Cr‚ditos suplementarios        '
DEFINE BAR  3 OF pop_08 PROMPT ' cr‚d. \<Pres. y anul. (dentro UE)'
DEFINE BAR  4 OF pop_08 PROMPT ' cr‚d. pres. y \<Anul. (entre UE) '
DEFINE BAR  5 OF pop_08 PROMPT ' cr‚d. pres. y a\<Nul. (entre UG) '

*- CALENDARIOS
IF escolor
   DEFINE POPUP pop_09 FROM 05,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_09 FROM 05,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_09 PROMPT ' \<Asigna calendarios       '
DEFINE BAR  2 OF pop_09 PROMPT ' \<Trabaja con calendarios  '

IF escolor
   DEFINE POPUP pop_02 FROM 09,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02 FROM 09,32 MARGIN  COLOR SCHEME c_popup
ENDIF

*DEFINE BAR  1 OF pop_02 PROMPT ' E-\<1 '
*DEFINE BAR  2 OF pop_02 PROMPT ' E-\<2 '
DEFINE BAR  1 OF pop_02 PROMPT ' E-\<5 '
*DEFINE BAR  4 OF pop_02 PROMPT ' E-\<6 '

*- Evaluacion
IF escolor
   DEFINE POPUP pop_03 FROM 10,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_03 FROM 10,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_03 PROMPT ' EV-\<1 '

 IF escolor
   DEFINE POPUP pop_04 FROM 08,32  SHADOW COLOR &L_COL
 ELSE
   DEFINE POPUP pop_04 FROM 08,32  COLOR SCHEME c_popup
 ENDIF

 DEFINE BAR  1 OF pop_04  PROMPT ' \<Unidad gestora '
 DEFINE BAR  2 OF pop_04  PROMPT ' unidad \<Ejecutora'
 DEFINE BAR  3 OF pop_04  PROMPT ' \<Funci¢n        '
 DEFINE BAR  4 OF pop_04  PROMPT ' \<Programas      '
 DEFINE BAR  5 OF pop_04  PROMPT ' \<Subprogramas   '
 DEFINE BAR  6 OF pop_04  PROMPT ' \<Actividad/proyecto   '
 DEFINE BAR  7 OF pop_04  PROMPT ' \<Componente     '
 DEFINE BAR  8 OF pop_04  PROMPT ' ca\<Dena Funcional'
 DEFINE BAR  9 OF pop_04  PROMPT ' c\<Lasificador   '
 DEFINE BAR 10 OF pop_04  PROMPT ' \<Ingresos       '
 DEFINE BAR 11 OF pop_04  PROMPT ' f\<Uentes        '
 DEFINE BAR 12 OF pop_04  PROMPT ' \<Tipo libros    '
 DEFINE BAR 13 OF pop_04  PROMPT ' li\<Bros         '
 *-
IF escolor
   DEFINE POPUP pop_04a FROM 17,58 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_04a FROM 17,58 COLOR SCHEME c_popup
ENDIF

 DEFINE BAR  1 OF pop_04a  PROMPT ' \<Categ.del gasto '
 DEFINE BAR  2 OF pop_04a  PROMPT ' \<Grupo gen‚rico  '
 DEFINE BAR  3 OF pop_04a  PROMPT ' \<Mod. de aplicac.'
 DEFINE BAR  4 OF pop_04a  PROMPT ' \<Espec. del gasto'

IF escolor
   DEFINE POPUP pop_05 FROM 12,32 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_05 FROM 12,32 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1  OF  pop_05 PROMPT '\<Cuadro necesidades   ' MESSAGE ''
DEFINE BAR 2  OF  pop_05 PROMPT '\<Pe.co.sa             ' MESSAGE ''
DEFINE BAR 3  OF  pop_05 PROMPT '\<Solicitud servicios  ' MESSAGE ''

*- Utilitarios 
IF escolor
   DEFINE POPUP pop_01 FROM 13,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_01 FROM 13,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_01 PROMPT '\<Indexar archivos           '
DEFINE BAR  2 OF pop_01 PROMPT '\<Backup de los archivos     '
DEFINE BAR  3 OF pop_01 PROMPT 'De\<puraci¢n de backups      '
DEFINE BAR  4 OF pop_01 PROMPT '\-'
DEFINE BAR  5 OF pop_01 PROMPT '\<Seguridad         ' MESSAGE '®® Acceso RESTRINGIDO ¯¯'
DEFINE BAR  6 OF pop_01 PROMPT '\<Usuarios PRESU    ' 

*- Consultas 
IF escolor
   DEFINE POPUP pop_06 FROM 14,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_06 FROM 14,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_06 PROMPT 'saldo \<Calendario mensual'
DEFINE BAR  2 OF pop_06 PROMPT 'saldo \<Presupuestal      '
DEFINE BAR  3 OF pop_06 PROMPT '\<Marco Calendario  '
DEFINE BAR  4 OF pop_06 PROMPT 'marco \<Ejecuci¢n   '
DEFINE BAR  5 OF pop_06 PROMPT 'ejecuci¢n \<+ calendario'
DEFINE BAR  6 OF pop_06 PROMPT '\<Listado E-5  ' 
DEFINE BAR  7 OF pop_06 PROMPT 'Listado \<E-5 (Proyectos)' 

ON SELECTION POPUP menu DO ACT_MENUS
IF !vflag$'J*'
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
    CASE choice = 1    && Elabora Presupuesto
         *ON SELECTION POPUP pop_07  DO  menu_07
         *ACTIVATE POPUP pop_07
         DO REGPREA with '2'
    CASE choice = 2    && calendario
         ON SELECTION POPUP pop_09  DO  menu_09
         ACTIVATE POPUP pop_09
    CASE choice = 3    && Modificaciones Presupuestales
         ON SELECTION POPUP pop_08  DO  menu_08
         ACTIVATE POPUP pop_08
    CASE choice = 4
         ON SELECTION POPUP pop_02  DO  menu_02
         ACTIVATE POPUP pop_02
    CASE choice = 5
         ON SELECTION POPUP pop_03  DO  menu_03
         ACTIVATE POPUP pop_03
    CASE choice = 6    &&
         ON SELECTION POPUP pop_04  DO  menu_04
         ACTIVATE POPUP pop_04
    CASE choice = 7
         ON SELECTION POPUP pop_05  DO  menu_05
         ACTIVATE POPUP pop_05
    CASE choice = 8
         ON SELECTION POPUP pop_01  DO  menu_01
         ACTIVATE POPUP pop_01
    CASE choice = 9
         ON SELECTION POPUP pop_06  DO  menu_06
         ACTIVATE POPUP pop_06
    CASE choice = 10
         DO Salmenu
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_01  DO menu_01
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_07  && PRESUPUESTO
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  Indexa
        DO REGPREA
    CASE choice = 2    &&  Indexa
        DO REGPRET
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_07  DO menu_07
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_07,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_08  && modificaciones
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  transferencias de partidas
        DO transpA
    CASE choice = 2    &&  credito suplementario
        DO CRESU
    CASE choice = 3    &&  creditos pres. y anulaciones en la U.E
        DO tranDUE
    CASE choice = 4    &&  creditos pres. y anulaciones / ue
        DO transue
    CASE choice = 5    &&  creditos pres. y anulaciones / uG
        DO transug

 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_08  DO menu_08
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_08,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_09  && calendarios
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  asigna calendarios
        DO asical
    CASE choice = 2    &&  registro de calendarios
        DO regcal
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_09  DO menu_09
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_09,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_02  && Utilitarios
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  Indexa
        DO REGE51
*    CASE choice = 4    &&  Indexa
        *DO REGE6
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02  DO menu_02
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_02,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_03  && Utilitarios
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  Indexa
       *DO REPO_EV3
       *DO REPEV1A
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_03  DO menu_03
 ENDIF
RESTORE SCREEN FROM PRINCIPAL
SHOW POPUP pop_03,menu
HIDE WINDOW ALL
RETURN

PROCEDURE menu_04
*---------------
 choice=BAR()
 DO CASE
   CASE choice = 1
      DO manpar WITH "UNIGES","Unid. Gestora","UNIDADES GESTORAS",1,"Valor siguiente"
   CASE choice = 2
      DO manpar WITH "UNIEJE","Unid. Ejecut.","UNIDADES EJECUTORAS",7,"1"
   CASE choice = 3
      DO manpar WITH "CODFUN","Funci¢n ","FUNCIONES",1,"Valor siguiente"
   CASE choice = 4
      DO manpar WITH "CODPRG","Programa","PROGRAMAS",7,"2"
   CASE choice = 5
      DO manpar WITH "CODSPR","SubPrograma","SUBPROGRAMAS",7,"3"
   CASE choice = 6
      DO manpar WITH "ACTPRY","Act./Proy.","1 ACTIVIDADES 2 PROYECTOS",1,"Valor siguiente"
   CASE choice = 7
      DO manpar WITH "CODCOM","Componente","COMPONENTES",1,"Valor siguiente"
   CASE choice = 8
      DO CADFUN WITH "2"
   CASE choice = 9
     *DO CLASIFIC
      ON SELECTION POPUP pop_04a  DO  menu_04a
      ACTIVATE POPUP pop_04a
   CASE choice = 10
      *DO INGRESO
   CASE choice = 11
      DO manpar WITH "CODFTE","Fuente","Fuentes",1,"Valor siguiente"
   CASE choice = 12
      DO manpar WITH "PREDOC","Tipo de Documento","Tipos",1,"Valor siguiente"
   CASE choice = 13
      DO LIBROS         
 ENDCASE
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_04,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_04a
*---------------
 choice=BAR()
 DO CASE
   CASE choice = 1
      DO manpar WITH "CATGAS","Categ. del Gasto","Categor¡as del Gasto",1,"Valor siguiente"
   CASE choice = 2
      DO manpar WITH "GRUGEN","Grupo gen‚rico","Grupos gen‚ricos",1,"Valor siguiente"
   CASE choice = 3
      DO manpar WITH "MODAPL","Mod. de aplicaci¢n","Modalidad de la aplicaci¢n",1,"Valor siguiente"
   CASE choice = 4
      DO manpar WITH "ESPGAS","Espec. del gasto","Espec¡fica del gasto",1,"Valor siguiente"
 ENDCASE
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_04a,menu
 HIDE WINDOW ALL
 RETURN


PROCEDURE menu_05  &&
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  
        DO regcdr
    CASE choice = 2    &&  
        DO REGPEC with 2,' '
    CASE choice = 3    &&  
        DO regsol
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_05  DO menu_05
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_05,menu
 HIDE WINDOW ALL
 RETURN


PROCEDURE menu_01  && Utilitarios
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  Indexa
      =PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar el proceso de reordenamiento sin ning£n problema')
      DO IndexPre
    CASE choice = 2
      =PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar la copia de seguridad sin ning£n problema')
      DO BackUp
    CASE choice = 3
      DO DepBK
    CASE choice = 5
      do segPRE
    CASE choice = 6
      do contusu
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_01  DO menu_01
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_01,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_06
*---------------
choice=BAR()

DO CASE
   CASE choice = 1
      DO CONSAL
   CASE choice = 2
      DO CONPRE
   CASE choice = 3
      DO MARCAL
   CASE choice = 4
      *DO MAREJE
   CASE choice = 5
      DO EJECAL
   CASE choice = 6
      DO REGE51
   CASE choice = 7
      DO REGE52
ENDCASE
RESTORE SCREEN FROM PRINCIPAL
SHOW POPUP pop_06,menu
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
*     SET EXCLU ON
      =borcon()
      CANCEL
    ELSE
      RESTORE SCREEN FROM Principal
      DO CLRSCR
    ENDIF
    =borcon()
    QUIT
  ELSE
     RETURN
  ENDIF

PROCEDURE salmenu2
*----------------
  IF YESNO(' Estamos terminando la sesi¢n ')
     SET COLO TO
     =ELICON()
     CLOSE DATA
     HIDE POPUP ALL
     RESTORE SCREEN FROM Principal
     DO CLRSCR1
     SET HELP ON
     DEACTIVATE WINDOW ALL
*    SET EXCLU ON
     IF worker
        CANCEL
     ENDIF
     QUIT
  ENDIF
  RETURN

PROCEDURE ABREPAS
*----------------
SET SKIP OF BAR 1  OF menu !'A' $vAcceso
SET SKIP OF BAR 2  OF menu !'B' $vAcceso
SET SKIP OF BAR 3  OF menu !'C' $vAcceso
SET SKIP OF BAR 4  OF menu !'D' $vAcceso

SET SKIP OF BAR 1  OF pop_02 !'E' $vAcceso
SET SKIP OF BAR 2  OF pop_02 !'F' $vAcceso
SET SKIP OF BAR 3  OF pop_02 !'G' $vAcceso
SET SKIP OF BAR 4  OF pop_02 !'H' $vAcceso

SET SKIP OF BAR 1  OF pop_03 !'I' $vAcceso

SET SKIP OF BAR 1  OF pop_04 !'L' $vAcceso
SET SKIP OF BAR 2  OF pop_04 !'M' $vAcceso
SET SKIP OF BAR 3  OF pop_04 !'N' $vAcceso
SET SKIP OF BAR 4  OF pop_04 !'O' $vAcceso
SET SKIP OF BAR 5  OF pop_04 !'P' $vAcceso
SET SKIP OF BAR 6  OF pop_04 !'Q' $vAcceso
SET SKIP OF BAR 7  OF pop_04 !'Y' $vAcceso
SET SKIP OF BAR 8  OF pop_04 !'Z' $vAcceso

SET SKIP OF BAR 1  OF pop_05 !'R' $vAcceso
SET SKIP OF BAR 2  OF pop_05 !'S' $vAcceso
SET SKIP OF BAR 3  OF pop_05 !'T' $vAcceso

SET SKIP OF BAR 1  OF pop_01 !'U' $vAcceso
SET SKIP OF BAR 2  OF pop_01 !'V' $vAcceso
SET SKIP OF BAR 3  OF pop_01 !'W' $vAcceso
SET SKIP OF BAR 5  OF pop_01 !'X' $vAcceso
SET SKIP OF BAR 6  OF pop_01 !'5' $vAcceso

SET SKIP OF BAR 1  OF pop_06 !'a' $vAcceso
SET SKIP OF BAR 2  OF pop_06 !'b' $vAcceso
SET SKIP OF BAR 3  OF pop_06 !'c' $vAcceso
SET SKIP OF BAR 5  OF pop_06 !'d' $vAcceso
SET SKIP OF BAR 6  OF pop_06 !'e' $vAcceso
SET SKIP OF BAR 7  OF pop_06 !'f' $vAcceso

* No acceso a Ejecucion
vOpcion = .T.
FOR I = 1 TO 4
  IF !SKPBAR('pop_02',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 5  OF menu vOpcion

* No acceso a Evaluacion
vOpcion = .T.
FOR I = 1 TO 4
  IF !SKPBAR('pop_02',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 6  OF menu vOpcion

* No acceso a Catalogos
vOpcion = .T.
FOR I = 1 TO 8
  IF !SKPBAR('pop_04',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 7 OF menu vOpcion

* No acceso a abas
vOpcion = .T.
FOR I = 1 TO 3
  IF !SKPBAR('pop_05',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 8 OF menu vOpcion

* No acceso a utilitarios
vOpcion = .T.
FOR I = 1 TO 6
  IF !SKPBAR('pop_01',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 9 OF menu vOpcion

* No acceso a CONSULTAS
vOpcion = .T.
FOR I = 1 TO 7
  IF !SKPBAR('pop_06',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 10 OF menu vOpcion

RETURN


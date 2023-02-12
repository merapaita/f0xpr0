* ................................................................
*
*   Programa 	:	Presu.prg                                  
*   Descrip		:	Principal del M¢dulo de PRESUPUESTO                                                                   
*                                                                      
*	Upgrade		:	Diciembre de 1999
*
* ................................................................
*
CLEAR
CLOSE ALL
CLEAR ALL
worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO Sai_lib
DO fox_ambi

DO lColores

IF SelPer()
	*
ELSE
	RETURN
ENDIF

*RUTA='..\Data'
*RUTA='F:\Bene2005\Data'
*RutaBk='..\BACKUP'
RUTApr='..\DataPr'
*rutaPr='F:\BENE2002\DATAPR'

*SET PATH TO &ruta

IF WorKer
	IF YesNo("¨ Datos de Prueba ?")
		SET PATH TO &rutapr
	ENDIF
ENDIF

*-- Clave de acceso
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
IF !FILE('USUARIO.CDX')
	DO STANDBY WITH ' Error en INDEXA '
ENDIF

PUBLIC vConex,vMaq,vLlav,vSistema,vUsuario,vusucla,vUsurec

vConex = ALLTRIM(LEFT(SYS(0),8))
vmaq   = ALLTRIM(RIGHT(SYS(0),2))
vllav  = ALLTRIM(sys(2))

USE Parmae ORDER TAG Parmae1 ALIAS Parma
SEEK "SISTEM01"
vsistema  = ALLTRIM(Parma.Codigo)

USE Usuario ORDER TAG Usuario1  ALIAS Usua

IF !Clave()
	CLOSE DATA
	DO STANDBY WITH '!! Acceso Denegado !!',10,15
	IF Worker
		CANCEL
	ENDIF
	QUIT
ENDIF

vUsuario = ALLTRIM(Usuario)
vNombre  = ALLTRIM(nombre)
vFlag    = flag
USE

use iteusu order tag iteusu1   alias iteu
sele iteu
set filt to sistema='1'
vusucla = chrtran(vUsuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
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
	rucw   = NumEnt
else
*	=poperror( 'El Sistema ha encontrado UN ERROR.... NO EXISTE datos generales de la empresa' +;
*	    '               CONSULTE con su Administrador de Sistemas      Por Seguridad se ABANDORA del Sistema')
*	close data
*	return
endif
use

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

IF SET("Path")=Rutapr
	DESA = " P R U E B A "
 ELSE
	DESA = " Presu"
ENDIF

TITU = " Sistema de Planificaci¢n y Presupuesto " + vConex

spac = (80 - (LEN(CIA)+LEN(TITU)+10))/2
Rotulo1 = CIA + SPACE(spac) + TITU + SPAC(spac) + space(11)

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
STORE .t. TO vEn_SISTEMa
STORE .f. TO esc_tecla
= SYS(2002,1)        && Set cursor on

*--- El men£ principal ---**
*L_COL =" W+/BG, N/W, N/W, GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "
L_COL  ="W+/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "
                                       && +     
IF escolor
   DEFINE POPUP menu FROM 5,2 TO 15,30 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP menu FROM 5,2 TO 15,30 COLOR SCHEME C_POPUP
ENDIF

* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º       Presupuesto         º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

DEFINE BAR 1  OF  menu PROMPT ' asignaci¢n \<Presupuestal ' MESSAGE ''
DEFINE BAR 2  OF  menu PROMPT ' asignaci¢n \<Calendarios  ' MESSAGE ''
DEFINE BAR 3  OF  menu PROMPT ' \<Modific. presupuestales ' MESSAGE ''
DEFINE BAR 4  OF  menu PROMPT ' \<Ejecuci¢n               ' MESSAGE ''
DEFINE BAR 5  OF  menu PROMPT ' cat logos y ta\<Blas      ' MESSAGE ''
*DEFINE BAR 6  OF  menu PROMPT ' \<Abastecimiento          ' MESSAGE ''
DEFINE BAR 6  OF  menu PROMPT ' \<Utilitarios             ' MESSAGE ''
DEFINE BAR 7  OF  menu PROMPT ' co\<Nsultas               ' MESSAGE ''
DEFINE BAR 8  OF  menu PROMPT ' \<Salida                   '

*- Asignaci¢n Presupuestal
IF escolor
   DEFINE POPUP pop_07a FROM 05,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_07a FROM 05,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_07a PROMPT ' \<Presupuesto Egresos '
DEFINE BAR  2 OF pop_07a PROMPT ' \<Presupuesto Ingresos'

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
*DEFINE BAR  3 OF pop_08 PROMPT ' cr‚d. \<Pres. y anul. (dentro UE)'
*DEFINE BAR  4 OF pop_08 PROMPT ' cr‚d. pres. y \<Anul. (entre UE) '
*DEFINE BAR  5 OF pop_08 PROMPT ' cr‚d. pres. y a\<Nul. (entre UG) '

*- ASIGNACION DE CALENDARIOS
IF escolor
   DEFINE POPUP pop_09 FROM 05,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_09 FROM 05,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_09 PROMPT ' \<Asigna calendarios       '
DEFINE BAR  2 OF pop_09 PROMPT ' \<Trabaja con calendarios  '

*- EJECUCION
IF escolor
   DEFINE POPUP pop_02 FROM 09,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02 FROM 09,32 MARGIN  COLOR SCHEME c_popup
ENDIF
DEFINE BAR  1 OF pop_02 PROMPT ' E-\<5 '
DEFINE BAR  2 OF pop_02 PROMPT ' E-5 \<E '
DEFINE BAR  3 OF Pop_02 PROMPt ' \<Asigna al Mes ==> 1 '
DEFINE BAR  4 OF pop_02 PROMPT 'Actualiza Preupuesto '
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
 DEFINE BAR  9 OF pop_04  PROMPT ' c\<Lasificador         '
 DEFINE BAR 10 OF pop_04  PROMPT ' \<Ingresos       '
 DEFINE BAR 11 OF pop_04  PROMPT ' f\<Uentes        '
 DEFINE BAR 12 OF pop_04  PROMPT ' \<Registro Clasificador    '
 DEFINE BAR 13 OF pop_04  PROMPT ' Asi\<Gnaciones presupuestales'
* DEFINE BAR 12 OF pop_04  PROMPT ' \<Tipo libros    '
* DEFINE BAR 13 OF pop_04  PROMPT ' li\<Bros         '
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
         ON SELECTION POPUP pop_07a  DO  menu_07a
         ACTIVATE POPUP pop_07a
*         DO REGPREA
    CASE choice = 2    && calendario
         ON SELECTION POPUP pop_09  DO  menu_09
         ACTIVATE POPUP pop_09
    CASE choice = 3    && Modificaciones Presupuestales
         ON SELECTION POPUP pop_08  DO  menu_08
         ACTIVATE POPUP pop_08
    CASE choice = 4
         ON SELECTION POPUP pop_02  DO  menu_02
         ACTIVATE POPUP pop_02
    CASE choice = 5    &&
         ON SELECTION POPUP pop_04  DO  menu_04
         ACTIVATE POPUP pop_04
*    CASE choice = 6
*         ON SELECTION POPUP pop_05  DO  menu_05
*         ACTIVATE POPUP pop_05
    CASE choice = 6
         ON SELECTION POPUP pop_01  DO  menu_01
         ACTIVATE POPUP pop_01
    CASE choice = 7
         ON SELECTION POPUP pop_06  DO  menu_06
         ACTIVATE POPUP pop_06
    CASE choice = 8
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
    CASE choice = 1
        DO REGPREA
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_07  DO menu_07
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_07,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_07a  && PRESUPUESTO
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  P.Egresos
        DO REGPREA
    CASE choice = 2    &&  P.Ingresos
        DO REGPREAI
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_07a  DO menu_07a
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_07a,menu
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
*    CASE choice = 3    &&  creditos pres. y anulaciones en la U.E
*        DO tranDUE
*    CASE choice = 4    &&  creditos pres. y anulaciones / ue
*        DO transue
*    CASE choice = 5    &&  creditos pres. y anulaciones / uG
*        DO transug
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

PROCEDURE menu_02  && EJECUCION
*----------------
choice=BAR()
DO CASE
	CASE choice = 1    &&  E5
		DO REGE51
	CASE choice = 2    &&  
		DO REGE51IE
	CASE choice = 3    && 
		DO asiuno
	CASE choice = 4    &&  
		DO ActPre
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

PROCEDURE menu_04 &&CATALOGOS Y TABLAS
*---------------
choice=BAR()
DO CASE
	CASE choice = 1
		DO manpar1 WITH "UNIDADES GESTORAS","UNIGES","Unid. Gestora","","Descripci¢n"
	CASE choice = 2
		DO manpar1 WITH "UNIDADES EJECUTORA","UNIEJE","Unid. Gestora","Uni. Ejec.","Descripci¢n"
	CASE choice = 3
		DO manpar1 WITH "FUNCIONES","CODFUN","Funcion","","Descripci¢n"
	CASE choice = 4
		DO manpar1 WITH "PROGRAMAS","CODPRG","Funci¢n","Programa","Descripci¢n"
	CASE choice = 5
		DO manpar1 WITH "SUB PROGRAMAS","CODSPR","Programa","Sub Programa","Descripci¢n"
	CASE choice = 6
		DO manpar1 WITH "1 ACTIVIDADES 2 PROYECTOS","ACTPRY","Act./Proy.","","Descripci¢n"
	CASE choice = 7
		DO manpar1 WITH "COMPONENTES","CODCOM","Componente","","Descripci¢n"
	CASE choice = 8
		DO CADFUN WITH "1"
	CASE choice = 9
		ON SELECTION POPUP pop_04a  DO  menu_04a
		ACTIVATE POPUP pop_04a
	CASE choice = 10
		DO INGRESO
	CASE choice = 11
		DO manpar1 WITH "FUENTES DE FINANCIAMIENTO","CODFTE","Fte. Financiamiento","","Descripci¢n"
	CASE choice = 12
		DO CatAsiEn
*		DO CLASIFIC
*		DO CLASIFIC
	CASE choice = 13
		DO CatAsi
*		DO manpar WITH "PREDOC","Tipo de Documento","Tipos",1,"Valor siguiente"
*	CASE choice = 12
*		DO manpar WITH "PREDOC","Tipo de Documento","Tipos",1,"Valor siguiente"
*	CASE choice = 13
*		DO LIBROS
 ENDCASE
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_04,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_04a
*-----------------
choice=BAR()
DO CASE
	CASE choice = 1
		DO manpar1 WITH "CATEGORIAS DEL GASTO","CATGAS","Categ. del Gasto","","Descripci¢n"
	CASE choice = 2
		DO manpar1 WITH "GRUPO GENERICO","GRUGEN","Grupo Generico","","Descripci¢n"
	CASE choice = 3
		DO manpar1 WITH "MODALIDAD DE APLICACIàN","MODAPL","Mod. de Aplicaci¢n","","Descripci¢n"
	CASE choice = 4
		DO manpar1 WITH "ESPECÖFICA DEL GASTO","ESPGAS","Espec. del Gasto","","Descripci¢n"
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
*		=PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
		'realizar el proceso de reordenamiento sin ning£n problema')
		DO IndexPre
	CASE choice = 2
		=PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
		'realizar la copia de seguridad sin ning£n problema')
		DO BackUp
	CASE choice = 3
		DO DepBK
	CASE choice = 5
		do segPRE WITH vUsuario
		RESTORE  SCREEN FROM Principal
*		do Usuarios
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
      DO MAREJE 
   CASE choice = 5
      DO EJECAL 
   CASE choice = 6
      DO LISE51 
   CASE choice = 7
*      DO REGE52 
	   DO LISE51IE
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
*	=ELICON()
	CLOSE DATA
	HIDE POPUP ALL
	=SAVCON(2)
	IF worker
		CLEAR
		SET HELP ON
		ON KEY
**		SET EXCLU ON
*		=borcon()
		CANCEL
	 ELSE
		RESTORE SCREEN FROM Principal
		DO CLRSCR
	ENDIF
*	=borcon()
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


FUNCTION Salta
*-------------
PARAMETERS cBar
lBorra = .F.
IF !USED("IteU")
	USE IteUsu IN 0 ORDER TAG IteUsu1 ALIAS IteU
	lBorra = .T.
ENDIF
SELE IteU
mRet = !SEEK(vUsuario+vSistema+cBar)
IF FOUND()
	cClamod = ALLTRIM(ClaMod)
ENDIF
IF lBorra
	USE
ENDIF
RETURN mRet


PROCEDURE ABREPAS
*-----------------
SET SKIP OF BAR 1  OF pop_07a !'A' $vAcceso
SET SKIP OF BAR 2  OF pop_07a !'B' $vAcceso

* No acceso a Asignacion Presupuestal
vOpcion = .T.
FOR I = 1 TO 2
  IF !SKPBAR('pop_07a',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 1 OF menu vOpcion

SET SKIP OF BAR 1  OF pop_09 !'C' $vAcceso
SET SKIP OF BAR 2  OF pop_09 !'D' $vAcceso

* No acceso a Asignacion Calendarios
vOpcion = .T.
FOR I = 1 TO 2
  IF !SKPBAR('pop_09',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 2 OF menu vOpcion

SET SKIP OF BAR 1  OF pop_08 !'E' $vAcceso
SET SKIP OF BAR 2  OF pop_08 !'F' $vAcceso

* No acceso a Modificaci¢n Presuopuestal
vOpcion = .T.
FOR I = 1 TO 2
  IF !SKPBAR('pop_08',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 3 OF menu vOpcion

SET SKIP OF BAR 1  OF pop_02 !'G' $vAcceso
SET SKIP OF BAR 2  OF pop_02 !'H' $vAcceso
SET SKIP OF BAR 3  OF pop_02 !'I' $vAcceso
SET SKIP OF BAR 4  OF pop_02 !'J' $vAcceso

* No acceso a Ejecucion
vOpcion = .T.
FOR I = 1 TO 4
  IF !SKPBAR('pop_02',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 4 OF menu vOpcion

SET SKIP OF BAR 1  OF pop_04 !'K' $vAcceso
SET SKIP OF BAR 2  OF pop_04 !'L' $vAcceso
SET SKIP OF BAR 3  OF pop_04 !'M' $vAcceso
SET SKIP OF BAR 4  OF pop_04 !'N' $vAcceso
SET SKIP OF BAR 5  OF pop_04 !'O' $vAcceso
SET SKIP OF BAR 6  OF pop_04 !'P' $vAcceso
SET SKIP OF BAR 7  OF pop_04 !'Q' $vAcceso
SET SKIP OF BAR 8  OF pop_04 !'R' $vAcceso
SET SKIP OF BAR 9  OF pop_04 !'S' $vAcceso
SET SKIP OF BAR 10 OF pop_04 !'T' $vAcceso
SET SKIP OF BAR 11 OF pop_04 !'U' $vAcceso
SET SKIP OF BAR 12 OF pop_04 !'V' $vAcceso

* No acceso a Catalogos y Tablas
vOpcion = .T.
FOR I = 1 TO 12
  IF !SKPBAR('pop_04',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 5 OF menu vOpcion

SET SKIP OF BAR 1  OF pop_01 !'W' $vAcceso
SET SKIP OF BAR 2  OF pop_01 !'X' $vAcceso
SET SKIP OF BAR 3  OF pop_01 !'Y' $vAcceso
SET SKIP OF BAR 5  OF pop_01 !'Z' $vAcceso
SET SKIP OF BAR 6  OF pop_01 !'1' $vAcceso

* No acceso a Utilitarios
vOpcion = .T.
FOR I = 1 TO 6
  IF !SKPBAR('pop_01',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 6 OF menu vOpcion

SET SKIP OF BAR 1  OF pop_06 !'2' $vAcceso
SET SKIP OF BAR 2  OF pop_06 !'3' $vAcceso
SET SKIP OF BAR 3  OF pop_06 !'4' $vAcceso
SET SKIP OF BAR 4  OF pop_06 !'5' $vAcceso
SET SKIP OF BAR 5  OF pop_06 !'6' $vAcceso
SET SKIP OF BAR 6  OF pop_06 !'7' $vAcceso
SET SKIP OF BAR 7  OF pop_06 !'8' $vAcceso

* No acceso a Consultas
vOpcion = .T.
FOR I = 1 TO 7
  IF !SKPBAR('pop_06',I)
    vOpcion = .F.
    EXIT
  ENDIF
ENDFOR
SET SKIP OF BAR 7 OF menu vOpcion



* No acceso a Ejecucion
*vOpcion = .T.
*FOR I = 1 TO 4
*  IF !SKPBAR('pop_02',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 4 OF menu vOpcion

* No acceso a Evaluacion
*vOpcion = .T.
*FOR I = 1 TO 4
*  IF !SKPBAR('pop_02',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 6  OF menu vOpcion

* No acceso a Catalogos
*vOpcion = .T.
*FOR I = 1 TO 8
*  IF !SKPBAR('pop_04',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 7 OF menu vOpcion

* No acceso a abas
*vOpcion = .T.
*FOR I = 1 TO 3
*  IF !SKPBAR('pop_05',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 8 OF menu vOpcion

* No acceso a utilitarios
*vOpcion = .T.
*FOR I = 1 TO 6
*  IF !SKPBAR('pop_01',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 9 OF menu vOpcion

* No acceso a CONSULTAS
*vOpcion = .T.
*FOR I = 1 TO 7
*  IF !SKPBAR('pop_06',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 10 OF menu vOpcion

RETURN
PROCEDURE XABREPAS
*-----------------
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
*vOpcion = .T.
*FOR I = 1 TO 4
*  IF !SKPBAR('pop_02',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 5  OF menu vOpcion

* No acceso a Evaluacion
*vOpcion = .T.
*FOR I = 1 TO 4
*  IF !SKPBAR('pop_02',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 6  OF menu vOpcion

* No acceso a Catalogos
*vOpcion = .T.
*FOR I = 1 TO 8
*  IF !SKPBAR('pop_04',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 7 OF menu vOpcion

* No acceso a abas
*vOpcion = .T.
*FOR I = 1 TO 3
*  IF !SKPBAR('pop_05',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 8 OF menu vOpcion

* No acceso a utilitarios
*vOpcion = .T.
*FOR I = 1 TO 6
*  IF !SKPBAR('pop_01',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 9 OF menu vOpcion

* No acceso a CONSULTAS
*vOpcion = .T.
*FOR I = 1 TO 7
*  IF !SKPBAR('pop_06',I)
*    vOpcion = .F.
*    EXIT
*  ENDIF
*ENDFOR
*SET SKIP OF BAR 10 OF menu vOpcion

RETURN

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
* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  Programa SubPrincipal : Banco.prg                                  º
* º                                                                     º
* º  ®®  SISTEMA DE PLANILLAS (PRUEBA)                                  º
* º                                                                     º
* º  Empresa : BENEFICENCIA DE PIURA                                    º
* º                                                                     º
* º  Software : Fox Pro 2.6a                                            º
* º                                                                     º
* º  Inicio   : Marzo 1998                                              º
* º                                                                     º
* º  Hecho    : Segundo E. Correa Sandoval          					º
* º             Analista de Sistemas                                    º
* º  Corregido: Mra                                                     º
* º  Inicio de Operaci¢n:                                               º
* º      Fecha: 15/03/1999                                              º
* º                                                                     º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
CLEAR
CLOSE ALL
CLEAR ALL

worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO Sai_lib
DO fox_ambi

PUBLIC vCoddep,vNumdep,vConex,vMaq,vLlav,vUsurec,vFlag,Sistem

vCodDep = '040200'
vNumDep = 6

RUTA='..\DATA'
RUTApr='..\DATAPR'

SET PATH TO &ruta
IF WorKer
   IF YesNo("¨ Datos de Prueba ?")
      SET PATH TO &rutapr
    ELSE
*		QUIT
   ENDIF
ENDIF

vConex = ALLTRIM(LEFT(SYS(0),8))
vmaq   = ALLTRIM(RIGHT(SYS(0),2))
vllav  = ALLTRIM(sys(2))

*=savcon()
*-- Clave de acceso
*vUser_ID = SPACE(5)

*User_ID = ALLTRIM(LEFT(SYS(0),15))
vUser_ID = 'PERSO'
IF !FILE( '&ruta\USUARIO.CDX')
  DO STANDBY WITH ' Error en INDEXA '
ENDIF


*TODO ESTO

*=SAVCON()     && SECS
USE Usuario ORDER TAG Usuario1  ALIAS Usua
*IF !Clave(vUser_ID,19,30,23,55)
*   CLOSE DATA
*   DO STANDBY WITH '!! Acceso Denegado !!',10,15
*   IF Worker
*      CANCEL
*    ENDIF
*   QUIT
*ENDIF
SEEK 'PERSO'
vNombre  = ALLTRIM(nombre)
vFlag = flag

USE

USE IteUsu ORDER TAG IteUsu1   ALIAS IteU
SELE iteu
SET FILT TO SISTEMa='1'
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
CIA  = "SOCIEDAD DE BENEFICENCIA DE PIURA"
SUBC = "    SBP    "
DESA = " OINF-MRA "
TITU = " Sistema de Planillas " + "("+ALLTRIM(SUBSTR(SYS(0),1,10))+")"

IF SET('PATH')=RutaPr
	DESA = " P R U E B A   D E L   S I S T E M A "
ENDIF
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
STORE .t. TO vEn_SISTEMa
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
* º       Planillasto         º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

DO ActTSer			&& Actualiza el Tiempo de Servicio

DEFINE BAR 1  OF  menu PROMPT ' \<Sueldos               ' MESSAGE 'Ingreso de Monto de Planilla'
DEFINE BAR 2  OF  menu PROMPT ' cat logos y ta\<Blas    ' MESSAGE 'Archivos Maestros'
DEFINE BAR 3  OF  menu PROMPT ' \<Procesos              ' MESSAGE 'Procesos de Planillas'
DEFINE BAR 4  OF  menu PROMPT ' \<Utilitarios           ' MESSAGE 'Utilidades con Archivos'
DEFINE BAR 5  OF  menu PROMPT ' \<S a l i d a            ' MESSAGE 'Salir del Sistema'

* Sueldos
IF escolor
   DEFINE POPUP pop_00 FROM 06,30 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_00 FROM 06,30 COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_00  PROMPT '\<Sueldos  '
DEFINE BAR  2 OF pop_00  PROMPT '\<Observ. Planillas '

* Cat logos
IF escolor
   DEFINE POPUP pop_01 FROM 06,30 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_01 FROM 06,30 COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_01  PROMPT '\<Activos  '
DEFINE BAR  2 OF pop_01  PROMPT '\<Cesantes '
DEFINE BAR  3 OF pop_01  PROMPT '\<Niveles'
DEFINE BAR  4 OF pop_01  PROMPT 'ca\<Rgo  '
DEFINE BAR  5 OF pop_01  PROMPT '\-'
DEFINE BAR  6 OF pop_01  PROMPT 'aumentos de \<Ley      '
DEFINE BAR  7 OF pop_01  PROMPT '\<Descuentos de Ley    '
DEFINE BAR  8 OF pop_01  PROMPT 'Aumentos/Descuentos \<X Niveles'
DEFINE BAR  9 OF pop_01  PROMPT 'Aportaci¢n del \<Empleador '
DEFINE BAR 10 OF pop_01  PROMPT '\-'
DEFINE BAR 11 OF pop_01  PROMPT 'reg. de \<Pensiones'
DEFINE BAR 12 OF pop_01  PROMPT 'Cuentas y Partidas'
DEFINE BAR 13 OF pop_01  PROMPT '\<Estado de Plaza'
DEFINE BAR 14 OF pop_01  PROMPT '\-'
DEFINE BAR 15 OF pop_01  PROMPT '\<Mes de Planilla'


*- Procesos
IF escolor
   DEFINE POPUP pop_02 FROM 07,30 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_02 FROM 07,30 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_02 PROMPT 'Generaci¢n de \<Planillas '
DEFINE BAR  2 OF pop_02 PROMPT 'Crea Reporte '
DEFINE BAR  3 OF pop_02 PROMPT '\-'
DEFINE BAR  4 OF pop_02 PROMPT 'Reporte de \<Planillas '
DEFINE BAR  5 OF pop_02 PROMPT 'Reporte de \<Boletas   '
DEFINE BAR  6 OF pop_02 PROMPT '\-'
DEFINE BAR  7 OF pop_02 PROMPT 'Reporte x \<Cadenas    '
DEFINE BAR  8 OF pop_02 PROMPT '\-'
DEFINE BAR  9 OF pop_02 PROMPT '\<Planilla Vacaciones  '
DEFINE BAR  10 OF pop_02 PROMPT '\<Boletas  Vacaciones  '
DEFINE BAR  11 OF pop_02 PROMPT '\<Cadena  Vacaciones  '
DEFINE BAR  12 OF pop_02 PROMPT '\-'
DEFINE BAR  13 OF pop_02 PROMPT '\<Cerrar Mes de Proceso'

*- Utilitarios 
IF escolor
   DEFINE POPUP pop_03 FROM 09,30 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_03 FROM 09,30 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_03 PROMPT '\<Indexar archivos           '
DEFINE BAR  2 OF pop_03 PROMPT '\<Backup de los archivos     '
DEFINE BAR  3 OF pop_03 PROMPT 'De\<puraci¢n de backups      '
DEFINE BAR  4 OF pop_03 PROMPT '\-'
DEFINE BAR  5 OF pop_03 PROMPT '\<Crear Diskette al B.N.'

ON SELECTION POPUP menu DO ACT_MENUS

* SECS
*IF !vflag$'J*'
*	DO ABREPAS
*ENDIF	
*SECS

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
    CASE choice = 1    && Ingresa Sueldos
         ON SELECTION POPUP pop_00  DO  menu_00
         ACTIVATE POPUP pop_00
*         Do Sueldos
    CASE choice = 2    && Cat logos
         ON SELECTION POPUP pop_01  DO  menu_01
         ACTIVATE POPUP pop_01
    CASE choice = 3    && Procesos
         ON SELECTION POPUP pop_02  DO  menu_02
         ACTIVATE POPUP pop_02
    CASE choice = 4    && 
         ON SELECTION POPUP pop_03  DO  menu_03
         ACTIVATE POPUP pop_03
    CASE choice = 5
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

PROCEDURE menu_00
*----------------
choice=BAR()
DO CASE
	CASE choice = 1  && Activos
		DO Sueldos
	CASE choice = 2  && Pensionistas
		DO RegObsPl
ENDCASE

RESTORE SCREEN FROM PRINCIPAL
SHOW POPUP pop_00,menu
HIDE WINDOW ALL
RETURN

PROCEDURE menu_01
*---------------
choice=BAR()
DO CASE
   CASE choice = 1  && Activos
      DO PerMae 
   CASE choice = 2  && Pensionistas
      DO PerPen
   CASE choice = 3
      DO manpar1 WITH "NIVPER","Descripci¢n ","Niveles de la SBP",1
   CASE choice = 4
      DO manpar1 WITH "CARPER","Descripci¢n ","Cargos en la SBP",1
   CASE choice = 6
      DO manpar1 WITH "AUMLEY","Descripci¢n ","Aumentos de Ley",6,"Monto ","Porcentaje : "
   CASE choice = 7
      DO manpar1 WITH "DESLEY","Descripci¢n ","Descuentos de Ley",6,"Monto ","Porcentaje : "
   CASE choice = 8
      DO NIVELES
   CASE choice = 9
      DO manpar1 WITH "APORTA","Descripci¢n ","Aportaci¢n del Empleador",6,"Monto ","Porcentaje : "
   CASE choice =11
      DO manpar1 WITH "AFPPER","Descripci¢n ","Administradoras de Fondo de Pensiones",1,"Valor siguiente"
   CASE choice =12
      DO CtayPar
   CASE choice =13
      DO manpar1 WITH "ESTPLA","Descripci¢n ","Estados de Plazas",1,"Valor siguiente"
   CASE choice =15
      DO manpar1 WITH "FECSIS","Descripci¢n ","Fecha del Sistema",1,"Valor siguiente"

ENDCASE

RESTORE SCREEN FROM PRINCIPAL
SHOW POPUP pop_01,menu
HIDE WINDOW ALL
RETURN

PROCEDURE menu_02  && Procesos
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    &&  Genera Planillas
    	Do GenPla
*    	Do CreaPla
    CASE choice = 2    &&  Genera Crea Reporte
    	Do CreaPla
    CASE choice = 4    &&  Reporte de planillas
    	Do RepPla1 WITH 1
*    	Do RepPla
    CASE choice = 5    &&  Reporte de Boletas
    	Do RepBol1 WITH 1
*    	Do RepBol WITH 1
    CASE choice = 7    &&  Reporte por cadenas
    	Do RepxCad WITH 1
    CASE choice = 9    &&  Reporte de planillas de Vacaciones
    	Do RepPla1 WITH 2
*    	Do Plavac
    CASE choice = 10    &&  Reporte de planillas de Vacaciones
    	Do RepBol1 WITH 2
*    	Do Bolvac
    CASE choice = 11    &&  Reporte de Cadenas de Vacaciones
    	Do RepxCad WITH 2
*    	Do Bolvac
    CASE choice = 13    &&  Reporte de Cadenas de Vacaciones
    	Do CerrarPla
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_02 DO menu_02
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
      =PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar el proceso de reordenamiento sin ning£n problema')
      DO IndexaBc
    CASE choice = 2
      =PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar la copia de seguridad sin ning£n problema')
      DO BackUp
    CASE choice = 3
      DO DepBK
    CASE choice = 5
*      =PopError( 'Aseg£rese que el Diskette est‚ en la Unidad A:, para poder ' +;
      'realizar la copia de informaci¢n al BANCO DE LA NACION.')
      do creadis
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_01  DO menu_01
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_01,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE salmenu
*----------------
  STORE  YESNO('¨ Estamos terminando la sesi¢n ?') TO in_secoemsa
  IF in_secoemsa
    SET COLOR TO
    *=ELICON()
    CLOSE DATA
    HIDE POPUP ALL
    IF worker
      CLEAR
      SET HELP ON
      ON KEY
*     SET EXCLU ON
      *=borcon()
      CANCEL
    ELSE
      RESTORE SCREEN FROM Principal
      DO CLRSCR
    ENDIF
    *=borcon()
    QUIT
  ELSE
     RETURN
  ENDIF

PROCEDURE salmenu2
*----------------
  IF YESNO(' Estamos terminando la sesi¢n ')
     SET COLO TO
     *=ELICON()
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

PROCEDURE ActTSer
*----------------
* Actualiza el Tiempo de Servicio
* de Todos los Trabajadores
USE PERSONAL
SET FILTER TO !(ESTADO='03' OR ESTADO='04' OR ESTADO='08' OR  ESTADO='10' ) AND !EMPTY(Ape_Nom)
SCAN
	dfecact = date()
	dFecIng = Fec_Ing
*	IF "02876017"=personal.Lib_Ele
*		SUSP
*	ENDIF
	DO tSer WITH dFecAct,dFecIng
ENDSCAN
USE
RETURN

PROCEDURE CerrarPla
*------------------
USE Parmae IN 0 ALIAS Parma ORDER TAG Parmae1
USE Planilla IN 0 
SELE Parma
SEEK "FECSIS"
cFecha = ALLTRIM(Descri)
cMesActual = UPPER(cMes(VAL(LEFT(cFecha,2))))
cAno = UPPER(RIGHT(cFecha,4))
cMsg ='Eata opcion permite cerrar todas las planilla del mes Actual ( '+ cMesActual +' - ' +cAno+' ).'+;
	  CHR(13)+'Despues de Este Proceso Usted no podr  hacer modificaciones en en dichas planilla'

=PopError( cMsg )

IF YESNO("Esta Seguro de Cerrar Esta Planilla")
	SELE Planilla
	REPLACE EstPla WITH '20' FOR Mes_Sue=LEFT(Parma.Descri,2)
ENDIF

USE IN Parma
USE in Planilla

RETURN

PROCEDURE tSer
*-------------
PARAMETERS dFecAct,dFecIng
nDiaSus = Personal.Dia_Sus
nDia1 = DAY(dFecIng)
nMes1 = MONT(dFecIng)
nAno1 = YEAR(dFecIng)
nDia2 = DAY(dFecAct)
nMes2 = MONT(dFecAct)
nAno2 = YEAR(dFecAct)
tDias=0
tMeses=0
tAnos=0
*set step on
FOR i=nAno1 TO nAno2
	DO CASE
		CASE nAno1=nAno2
			FOR j = nMes1 TO nMes2
				uDiaMes = DAY(dFinMes(j,i))
				DO CASE
					CASE nMes1 = nMes2
						IF nDia2>uDiaMes
							tDias = uDiaMes - nDia1 + 1
						 ELSE
							tDias = nDia2 - nDia1 + 1
						ENDIF
					CASE j = nMes1 AND nMes1#nMes2
						tDias = tDias + uDiaMes
					CASE j = nMes2 AND nMes1#nMes2
						tDias = tDias + nDia2
					OTHERWISE
						tDias = tDias + uDiaMes
				ENDCASE
				IF tDias>=uDiaMes
					tDias = tDias - uDiaMes
					tMeses = tMeses+1
				ENDIF
			ENDFOR
		CASE i = nAno1 AND nAno1#nAno2
			FOR j = nMes1 TO 12
				uDiaMes = DAY(dFinMes(j,i))
				DO CASE
					CASE nano1=i AND nMes1=j
						tDias = uDiaMes - nDia1 + 1
					OTHERWISE
						tDias = tDias + uDiaMes
				ENDCASE
				IF tDias>=uDiaMes
					tDias = tDias - uDiaMes
					tMeses = tMeses+1
				ENDIF
			ENDFOR
		CASE i = nAno2 AND nAno1#nAno2
			FOR j = 1 TO nMes2
				uDiaMes = DAY(dFinMes(j,i))
				DO CASE
					CASE nano2=i AND nMes2=j
						tDias = tDias + nDia2
					OTHERWISE
						tDias = tDias + uDiaMes
				ENDCASE
				IF tDias>=uDiaMes
					tDias = tDias - uDiaMes
					tMeses = tMeses+1
				ENDIF
			ENDFOR
		OTHERWISE
			FOR j = 1 TO 12
				uDiaMes = DAY(dFinMes(j,i))
				tDias = tDias + uDiaMes
				IF tDias>=uDiaMes
					tDias = tDias - uDiaMes
					tMeses = tMeses+1
				ENDIF
			ENDFOR
	ENDCASE
	IF tMeses>=12
		tMeses = tMeses - 12
		tAnos  = tAnos+1
	ENDIF
ENDFOR

AnoS  = INT(nDiaSus/365)
xMesS = MOD(nDiaSus,365)
MesS  = INT(xMesS/30)
DiaS  = MOD(xMesS,30)

tDias = tDias-DiaS

IF tDias<0
	tDias  = 30 - ABS(tDias)
	tMeses = tMeses - 1
ENDIF
tMeses = tMeses-MesS
IF tMeses<0
	tMeses = 12 - ABS(tMeses)
	tAnos  = tAnos - 1
ENDIF
tAnos = Tanos - AnoS

m.a¤o_ser = STR(tAnos,2)+'A, '+STR(tmeses,2)+'M, '+STR(tDias,2)+'D.'
REPLACE A¤o_ser WITH m.A¤o_ser

RETURN
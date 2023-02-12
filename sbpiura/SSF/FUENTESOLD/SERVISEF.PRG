* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  Programa SubPrincipal : Presu.prg                                  º
* º                                                                     º
* º  ®®  SISTEMa de Servicio de Sepelio Familiar S.B.P ¯¯               º
* º                                                                     º
* º  Empresa : Beneficencia de  PIURA-PERU                              º
* º                                                                     º
* º  Software : Fox Pro 2.x                                             º
* º                                                                     º
* º  Inicio   : Agosto 1994                                             º
* º                                                                     º
* º      M¢dulo SERVISEF                                                º
* º                                                                     º
* º  Analista : Segundo E. Correa Sandoval                              º
* º                                                                     º
* º  Inicio de Operaci¢n:                                               º
* º      Fecha: 01/10/97                                                º
* º                                                                     º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
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
SET CENTURY ON
public vCoddep,vNumdep,SISTEM 
VCODDEP = '040200'
VNUMDEP = 6
SISTEM = '1'

Ruta   = '..\Data'
RutaPr = '..\Datapr'
RutaBk = '..\BackUp'

SET PATH TO &ruta
IF WorKer
   IF YesNo("¨ Datos de Prueba ?")
		SET PATH TO &RutaPr
   ENDIF
ENDIF
*=savcon()
*-- Clave de acceso
*vUser_ID = SPACE(5)

vUser_ID = ALLTRIM(LEFT(SYS(0),15))

*IF !FILE( '&ruta\USUARIO.CDX')
*  DO STANDBY WITH ' Error en INDEXA '
*ENDIF

public vCoddep,vConex,vMaq,vLlav,vUsurec,vFlag


vConex = ALLTRIM(LEFT(SYS(0),8))
vmaq   = ALLTRIM(RIGHT(SYS(0),2))
vllav  = ALLTRIM(sys(2))

*=SAVCON()

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
vNombre  = 'Analista: SEGUNDO E. CORREA S.'
*vFlag = flag
*USE

*USE IteUsu ORDER TAG IteUsu1   ALIAS IteU
*SELE iteu
*SET FILT TO SISTEMa='1'
*vUsuCla = CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
*          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
*vAcceso = ''

*SEEK vUsuCla

*SCAN WHILE vUsuCla = ALLTRIM(IteU.UsuCla)
*     vAcceso = vAcceso + IteU.Modulo
*ENDSCAN
*SET FILT  TO
*USE

**-- Definiciones para el Men£ --**
*----------------------------------*
CIA  = " S.B.P. "
SUBC = "    SERVISEF    "
DESA = SET('PATH')
TITU = " SERVICIO DE SEPELIO FAMILIAR" + "("+ALLTRIM(SUBSTR(SYS(0),1,10))+")"

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

usuarios = '®Hecho: '+vnombre+' ¯'
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
* º       SERVISEF            º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

DEFINE BAR 1  OF  menu PROMPT ' \<Cobranzas             ' MESSAGE ''
DEFINE BAR 2  OF  menu PROMPT ' \<Inscripci¢n de Socios  ' MESSAGE ''
DEFINE BAR 3  OF  menu PROMPT ' \<Estados de Cuenta     ' MESSAGE ''
DEFINE BAR 4  OF  menu PROMPT ' \<Gastos                 ' MESSAGE ''
DEFINE BAR 5  OF  menu PROMPT ' \<Ingresos               ' MESSAGE ''
DEFINE BAR 6  OF  menu PROMPT ' cat logos y ta\<Blas    ' MESSAGE ''
DEFINE BAR 7  OF  menu PROMPT ' \<Utilitarios           ' MESSAGE ''
DEFINE BAR 8  OF  menu PROMPT ' \<Reportes              ' MESSAGE ''
DEFINE BAR 9  OF  menu PROMPT ' \<S a l i d a            '

*- cobranza
IF escolor
   DEFINE POPUP pop_01 FROM 05,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_01 FROM 05,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_01 PROMPT ' \<Cobranza '
DEFINE BAR  2 OF pop_01 PROMPT ' \<Recibos de Cobranza'
DEFINE BAR  3 OF pop_01 PROMPT ' \<Servicios de Sepelio'
DEFINE BAR  4 OF pop_01 PROMPT '\-'
DEFINE BAR  5 OF pop_01 PROMPT ' \<Estadistica de Deudas'

*- Kardex
IF escolor
   DEFINE POPUP pop_05 FROM 05,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_05 FROM 05,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_05 PROMPT ' \<Kardex '
DEFINE BAR  2 OF pop_05 PROMPT ' \<Estados de Cuenta (Resumen)'
DEFINE BAR  3 OF pop_05 PROMPT '\-'
DEFINE BAR  4 OF pop_05 PROMPT ' \<Corrige Kardex'

*- catalogos

 IF escolor
   DEFINE POPUP pop_02 FROM 08,32  SHADOW COLOR &L_COL
 ELSE
   DEFINE POPUP pop_02 FROM 08,32  COLOR SCHEME c_popup
 ENDIF

 DEFINE BAR  1 OF pop_02  PROMPT ' \<Promotores     '
 DEFINE BAR  2 OF pop_02  PROMPT ' \<Cobradores     '
 DEFINE BAR  3 OF pop_02  PROMPT ' p\<Ersonal       '
 DEFINE BAR  4 OF pop_02  PROMPT '\-'
 DEFINE BAR  5 OF pop_02  PROMPT ' Tipo \<Gastos    '
 DEFINE BAR  6 OF pop_02  PROMPT ' Tipo \<Ingresos  '
 DEFINE BAR  7 OF pop_02  PROMPT ' pa\<Rentesco     '
 DEFINE BAR  8 OF pop_02  PROMPT ' Tipo \<Derechos  '
 DEFINE BAR  9 OF pop_02  PROMPT ' ins\<Tituciones  '


 *-

*- Utilitarios 
IF escolor
   DEFINE POPUP pop_03 FROM 13,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_03 FROM 13,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_03 PROMPT '\<Indexar archivos           '
DEFINE BAR  2 OF pop_03 PROMPT '\<Backup de los archivos     '
DEFINE BAR  3 OF pop_03 PROMPT 'De\<puraci¢n de backups      '
DEFINE BAR  4 OF pop_03 PROMPT 'Manten. de Correlativos      '

*- Reportes 
IF escolor
   DEFINE POPUP pop_04 FROM 14,32 MARGIN  SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_04 FROM 14,32 MARGIN  COLOR SCHEME c_popup
ENDIF

DEFINE BAR  1 OF pop_04 PROMPT '\<Cobranzas de Socios'
DEFINE BAR  2 OF pop_04 PROMPT '\<Morosos              '
DEFINE BAR  3 OF pop_04 PROMPT 'liquida   \<Promotores '
DEFINE BAR  4 OF pop_04 PROMPT 'liquida   \<Cobradores '
DEFINE BAR  5 OF pop_04 PROMPT 'cobranzas \<Realizadas '

ON SELECTION POPUP menu DO ACT_MENUS
*IF !vflag$'J*'
*	DO ABREPAS
*ENDIF	

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
    CASE choice = 1    
         ON SELECTION POPUP pop_01  DO  menu_01
         ACTIVATE POPUP pop_01
    CASE choice = 2
         DO inssoc
    CASE choice = 3
         ON SELECTION POPUP pop_05  DO  menu_05
         ACTIVATE POPUP pop_05
*		 do kardex1
*		 do EstCta1
*		 do kardex	
    CASE choice = 4
         do gastos
    CASE choice = 5
         DO INGRESO	
    CASE choice = 6    &&
		 ON SELECTION POPUP pop_02  DO  menu_02
         ACTIVATE POPUP pop_02
    CASE choice = 7
         ON SELECTION POPUP pop_03  DO  menu_03   &&Utilitarios
         ACTIVATE POPUP pop_03
    CASE choice = 8
         ON SELECTION POPUP pop_04  DO  menu_04
         ACTIVATE POPUP pop_04
    CASE choice = 9
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

PROCEDURE menu_01  && cobranza
*----------------
 choice=BAR()
 DO CASE
    CASE choice = 1    
        DO INGCOB
    CASE choice = 2 
        DO REccob
    CASE choice = 3 
        DO SerFun
    CASE choice = 5 
        DO EstDeu
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_01  DO menu_01
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_01,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_05  && cobranza
*----------------
choice=BAR()
DO CASE
	CASE choice = 1
		DO Kardex1
	CASE choice = 2
		DO EstCta1
	CASE choice = 4
		DO CorKar
ENDCASE
IF !escolor
	RESTORE SCREEN FROM principal
	ON SELECTION POPUP pop_05  DO menu_05
ENDIF
RESTORE SCREEN FROM PRINCIPAL
SHOW POPUP pop_05,menu
HIDE WINDOW ALL
RETURN


PROCEDURE menu_02
*---------------
 choice=BAR()
 DO CASE
   CASE choice = 1
      DO CATPRO
   CASE choice = 2
      DO CATCOB
   CASE choice = 3
      DO CATPER	
   CASE choice = 5
      DO manpar WITH "TIPGAS","Gasto","Gastos",1,"Valor siguiente"
   CASE choice = 6
      DO manpar WITH "TIPING","Ingreso","Ingresos",1,"Valor siguiente"
   CASE choice = 7
      DO manpar WITH "PARENT","Parentesco","Parentescos",1,"Valor siguiente"
   CASE choice = 8
      DO manpar WITH "TIPDER","Derecho","Derechos",1,"Valor siguiente"
   CASE choice = 9
      DO manpar WITH "INSTIT","Instituci¢n","Instituci¢n",1,"Valor siguiente"
 ENDCASE
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
      DO IndSERV
    CASE choice = 2
      =PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar la copia de seguridad sin ning£n problema')
      DO BackUp
    CASE choice = 3
      DO DepBK
    CASE choice = 4
      do Correl
    CASE choice = 6
    *  do contusu
 ENDCASE
 IF !escolor
    RESTORE SCREEN FROM principal
    ON SELECTION POPUP pop_03  DO menu_03
 ENDIF
 RESTORE SCREEN FROM PRINCIPAL
 SHOW POPUP pop_03,menu
 HIDE WINDOW ALL
 RETURN

PROCEDURE menu_04     && REPORTES
*---------------
choice=BAR()
DO CASE
   CASE choice = 1
      DO REPCOB
   CASE choice = 2
      DO REPMOR 
   CASE choice = 3
      DO LIQPRO
   CASE choice = 4
      DO LIQCOB 
   CASE choice = 5
      DO COBDIA
ENDCASE
RESTORE SCREEN FROM PRINCIPAL
SHOW POPUP pop_04,menu
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
     * =borcon()
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
   *  =ELICON()
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


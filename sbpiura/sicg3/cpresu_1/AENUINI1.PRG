* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  Programa    Principal : Maenuini.Prg                               º
* º                                                                     º
* º  ®® Control de Ayudant¡a y Asignaci¢n de Aulas para Pr cticas.      º
* º                                                                     º
* º  Realizado : Grupo 4                                                º
* º                                                                     º
* º  Software : Fox Pro 2.x                                             º
* º                                                                     º
* º  Inicio   : Agosto 1995                                             º
* º                                                                     º
* º                                                                     º
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
SET PROCEDURE TO Lib_ayuda
DO fox_ambi

OK=VE_PASSW('AYUDA')
IF !OK
   RETURN
ENDIF

SET PATH TO ..\DATA
IF WorKer
   IF YesNo("Datos de Prueba?")
      SET PATH TO ..\DATAPR
   ENDIF
ENDIF


**-- Definiciones para el Men£ --**
*----------------------------------*
CIA  = " Ingenieria  UdeP "

DESA = " Escuela Tecnol¢gica Superior Grupo 4 "
TITU = " Ayudant¡a y Asig. de Aulas para Pr cticas "+ "("+ALLTRIM(SUBSTR(SYS(0),1,2))+")"
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

DO Logos WITH Rotulo1,Rotulo2,30

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

IF escolor
   DEFINE POPUP menu FROM 5,2 TO 15,21 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP menu FROM 5,2 TO 15,21 COLOR SCHEME C_POPUP
ENDIF

DEFINE BAR 1  OF  menu PROMPT ' \<Asig. aulas     ' MESSAGE ''
DEFINE BAR 2  OF  menu PROMPT ' asig. a\<Yudantes ' MESSAGE ''
DEFINE BAR 3  OF  menu PROMPT ' \<Pagos           ' MESSAGE ''
DEFINE BAR 4  OF  menu PROMPT ' \<Reportes       ' MESSAGE ''
DEFINE BAR 5  OF  menu PROMPT ' \<Tablas         ' MESSAGE ''
DEFINE BAR 6  OF  menu PROMPT ' \<Utilitarios    ' MESSAGE ''
DEFINE BAR 7  OF  menu PROMPT ' \<Salida          '

*- Reportes
IF escolor
   DEFINE POPUP pop_04 FROM  9,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_04 FROM  9,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_04 PROMPT 'Ayudantes por \<Curso ' MESSAGE ''
DEFINE BAR 2  OF  pop_04 PROMPT 'Ayudantes por \<Semana' MESSAGE ''
DEFINE BAR 3  OF  pop_04 PROMPT '\<Cursos por semana   ' MESSAGE ''

*- Tablas
IF escolor
   DEFINE POPUP pop_05 FROM 09,26 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP pop_05 FROM 09,26 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_05 PROMPT '\<Ayudantes        ' MESSAGE ''
DEFINE BAR 2  OF  pop_05 PROMPT '\<Cursos           ' MESSAGE ''
DEFINE BAR 3  OF  pop_05 PROMPT 'au\<Las            ' MESSAGE ''
DEFINE BAR 4  OF  pop_05 PROMPT '\<Tipos Ayudantes  ' MESSAGE ''
DEFINE BAR 5  OF  pop_05 PROMPT '\<Grupos y cursos  ' MESSAGE ''
DEFINE BAR 6  OF  pop_05 PROMPT '\<Fecha de Ex menes' MESSAGE ''

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
        *DO AsiAula
   CASE BAR() = 2
        *DO AsiAyud
   CASE BAR() = 3
        *DO RegPago
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
        DO Salmenu
ENDCASE

RESTORE SCREEN FROM principal
RETURN

PROCEDURE menu_04   && Reportes
*----------------
  choice=BAR()
  DO CASE
    CASE choice = 1
    	*Do RepAcur
    CASE choice = 2
    	*Do RepAsem
    CASE choice = 3
    	*Do RepCuse
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
      
    CASE choice = 2
      
    CASE choice = 3
      
    CASE choice = 4
      *DO manpar WITH "HOJCON","Correlativo Hoja de Control","Correlaivos Hojas de Control",4,"Valor siguiente"
    CASE choice = 5
     
    CASE choice = 6
  ENDCASE
  RESTORE SCREEN FROM PRINCIPAL
  SHOW POPUP pop_05,menu
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
      *DO IndexCP
    CASE choice = 2
      =PopError( 'Aseg£rese que no est‚n usando el sistema, para poder ' +;
      'realizar la copia de seguridad sin ning£n problema')
      *DO BackUp
    CASE choice = 3
      *DO ResBK
    CASE choice = 4
      *DO DepBK
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
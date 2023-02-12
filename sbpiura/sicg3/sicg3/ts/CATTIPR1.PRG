* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ CatTipRi.PRG  02/07/96                              L: 0408  ³	
* ³ Cat logo de Conceptos de Recibos de Ingreso                  ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

USE Parmae   IN 1  ORDER TAG Parmae1  ALIAS Parma
USE Cuentas  IN 7   ORDER TAG Cuentas1 ALIAS cuenta
USE Ingreso  IN 10  ORDER TAG Ingreso1 		ALIAS IngR
*- Variables de trabajo (registro a trabajar)
SELECT Parma
SET FILT TO Tipo='TIPRI '
GO BOTTOM
SCATTER MEMVAR BLANK         && Crea variables en blanco
*- Mensajes de aviso al usuario
Vmens01 = ' ± Registro de Conceptos de Recibo de Ingresos ± '
Vmens02 = 'Revisi¢n de Rubros'
Vmens05 = 'No existe Rubro anterior'
Vmens06 = 'No existe Rubro siguiente'
Vmens08 = 'No hay registros para procesar'
*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista
PUBLIC vtipo, vclas
STORE SPACE(1) TO vtipo
STORE SPACE(9) TO vclas
*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO
DO Fin_opcion
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 23,79  ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_c1 FROM 00,00 TO 23,79 ;
 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @ 01,02 SAY "C¢digo del Concepto :"
 @ 03,02 SAY "        Descripci¢n :"
 @ 06,02 SAY " Asiento Autom tico :"
 @ 08,02 SAY " Clasificador [S/N] :"
 @ 10,02 SAY " Clasif. de Ingreso :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Parma
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR
 @ 01,24 SAY m.codigo
 @ 03,24 SAY m.Descri
 @ 06,24 SAY m.descriaux
 @ 06,36 SAY val_fun('Cuenta','Cuenta',"DescRi",LEFT(m.Descriaux,11))
 @ 08,24 SAY LEFT(ALLT(m.descriau2),1)
 @ 10,24 SAY IIF(LEFT(ALLT(m.descriau2),1)='S',SUBSTR(ALLT(m.descriau2),4,9),SPACE(30))
 RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE Parma
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 codigo  :H='Tipo R/I' ,;
 descri  :30 :H='Detalle' ,;
 descriaux :10 :H='Asiento',;
 X2=SUBSTR(ALLT(m.descriau2),4,9)  :9 :H='Clasificador'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 DO Vista
 RETURN

PROCEDURE Busca
*--------------
 SELECT Parma
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF
 Vtemp = RECNO()
 STORE SPACE(03) TO vGen
 ACTIVATE WINDOW standby
 @ 1,03 SAY "C¢digo del Concepto :" GET vGen
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vGen) .OR. LASTKEY()=27
    GOTO Vtemp
 ELSE
    SEEK 'TIPRI '+vGen
    IF !FOUND()
       DO standby WITH "Dicho Rubro no est  registrado"
       GOTO Vtemp
    ELSE
       DO Vista
    ENDIF
 ENDIF
 RETURN


PROCEDURE Anter
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF !BOF()
    SKIP -1
 ENDIF
 IF BOF()
    GO TOP
    DO standby WITH Vmens05
 ELSE
    DO Vista
 ENDIF
 RETURN


PROCEDURE Proxi
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH Vmens06
   GO BOTTOM
 ELSE
   DO Vista
 ENDIF
 RETURN


PROCEDURE Corri
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF

 ACTIVATE WINDOW Wind_0
 SELECT Parma
 DO pantalla
 SCATTER MEMVAR
 vTipo=LEFT(ALLT(m.descriau2),1)
 vclas=SUBS(ALLT(m.descriau2),4,9)
 vTipo=IIF(EMPTY(vtipo),SPACE(1),vtipo)
 vclas=IIF(EMPTY(vclas),SPACE(9),vclas)
 m.tipo='TIPRI '
 IF RLOCK() OR F_LOCK(1)
    @ 01,24 GET m.Codigo DISABLE
    @ 03,24 GET m.Descri PICTURE '@S36'
    @ 06,24 GET m.descriaux PICTURE "99999999999" 
    @ 08,24 GET vtipo FUNCTION 'M N,S'
    @ 10,24 GET vclas PICTURE "!.!.!.!!!"  VALID VAL_INGE(SUBSTR(vclas,11,2),LEFT(vclas,9),' ')  WHEN Vtipo='S'
    READ VALID Val_Read()
    IF LASTKEY() # 27          && Si se presion¢ Esc, no graba
       m.descriau2=vtipo+'--'+vclas
       SELECT Parma
       GATHER MEMVAR
    ENDIF
    DO Vista                    && Muestra nuevos datos
 ENDIF
 UNLOCK
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT Parma
 DO Pantalla
 SCATTER MEMVAR BLANK
* SEEK 'CORREL'+'TIPORI'
* m.codigo = PADL(NumEnt+1,4,'0')
 m.tipo='TIPRI '
 *@ 01,24 GET m.codigo disable 
  @ 01,24 GET m.Codigo PICTURE '999' VALID vret() 
  @ 03,24 GET m.Descri PICTURE '@S36'
 @ 06,24 GET m.descriaux PICTURE "99999999999" 
 @ 08,24 GET vtipo FUNCTION 'M N,S'
 @ 10,24 GET vclas PICTURE "!.!.!.!!!" VALID VAL_INGE(SUBSTR(vclas,11,2),LEFT(vclas,9),' ') WHEN vtipo='S' 
 READ VALID Val_Read()
 IF LASTKEY() # 27
       m.descriau2=vtipo+'--'+vclas
       IF F_Appd()
         GATHER MEMVAR
       ELSE
         GO BOTTOM
       ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado, no se graba nada'
   GO BOTTOM
 ENDIF
 UNLOCK ALL
 SELECT Parma
 DO Vista
 RETURN


PROCEDURE Elimi
*--------------
 SELECT Parma
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ELIMINAR FISICAMENTE ‚ste Rubro ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
   DELETE NEXT 1
   GO BOTTOM
   DO Vista
 ENDIF
 UNLOCK
 RETURN

PROCEDURE Lista
*--------------
SELECT Parma
Vind=SYS(3)+'.idx'
vtemp =RECNO()
IF EOF()
   DO standby WITH vmens08
   RETURN
ELSE
	IF escolor
		DEFINE POPUP xcot  FROM 18,55 SHADOW COLOR &l_col
	ELSE
		DEFINE POPUP xcot  FROM 18,55 COLOR SCHEME c_popup
	ENDIF
	DEFINE BAR 1 OF xcot PROMPT 'Por \<C¢digo     '
	DEFINE BAR 2 OF xcot PROMPT 'Por \<Descripci¢n'
	ON SELECTION POPUP xcot  DEACTIVATE POPUP
	ACTIVATE POPUP xcot
	DO CASE
	   CASE BAR() = 1
	        INDEX ON val(codigo) TO (Vind) FOR tipo='TIPRI ' 
		    DO reporte WITH 2,"LisTiprC",' Listado de Rubro de Retenciones '
		    SET INDEX TO
		    CLOSE INDEX
		    ERASE (vind)
	   CASE BAR() = 2
	        SET ORDER TO Parmae2
		    DO reporte WITH 2,"ListiprD",' Listado de Rubro de Retenciones '
			SET ORDER  TO Parmae1
	ENDCASE
ENDIF
GO vtemp
DO Vista
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN

FUNCTION vret
*------------
SELE Parma
SEEK m.tipo+m.codigo
IF FOUND()
   DO STANDBY WITH 'Concepto de R/I ya existe' 
   RETURN  .F.
ENDIF

RETURN .T.

FUNCTION correl
*--------------
SELE Parma
SEEK 'CORREL'+'TIPORI'
m.codigo = PADL(NumEnt+1,4,' ')

RETURN 



FUNCTION Val_IngE
*---------------
  PARAMETERS mValor, Filtro, mVaRiable, mCol, mLong
  PRiVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVaRiable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRi
  ENDCASE
  mAlias  = ALIAS()
  SELECT IngR
  SEEK Filtro+mValor

  IF !FOUND() AND !mVaRiable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
         SET FILTER TO CodIng >= Filtro
      ENDIF
      GO TOP
      IF EOF() 
         DO STANDBY WITH 'No existen partidas definidas'
         SET FILTER TO
         sele (malias)
         return &&.f.
      endif
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRiM(CODING)+'.'+ALLTRiM(SUBING)+' '+SUBSTR(DESING,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = IngR.CodIng 
 mDescr = SUBSTR( IngR.DesIng, 1, mLong )
 SET ORDE TO 1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVaRiable=' '   && En edici¢n
      @ ROW(),mCol+24  SAY mValor
      @ ROW(),mCol+37 SAY mDescr
      vclas    =mvalor
      RETURN .T.
    CASE mVaRiable='A'   && En edici¢n SOLO DESCRiPCION
      @ ROW(),mCol SAY mDescr
      RETURN
    CASE mVaRiable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVaRiable='D'   && En vista
      RETURN mDescr
    CASE mVaRiable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVaRiable='C'   && Solo codigo
      RETURN .T.
    CASE mVaRiable='T'
      &mVaRiable = mValor
      @ ROW(),mCol+7 SAY mDescr
      RETURN  mValor
    OTHERWISE   
      vclas= mvalor
      RETURN .T.
  ENDCASE
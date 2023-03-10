* 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
* ? Pendient.prg    12/06/96                            L: 0559  ?	
* ? Registro de Cheques pendientes                               ?
* ? Adecuaci줻 :   A.S. Oswaldo Arturo Oliva Carl죒.             ?
* 突袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴槁

set talk off
set excl off

SET PROC TO H:\SIS97\TESO\TES_LIB
SET PATH TO H:\SIS97\DATA

USE Parmae   IN 1   ORDER TAG Parmae1  ALIAS Parma
USE SalBcos  IN 2   ORDER TAG SalBcos1  ALIAS SalBcos
USE Cajas    IN 8 ORDER TAG Cajas1          ALIAS Caja

*- Variables de trabajo (registro a trabajar)
SELECT SalBcos
GO BOTTOM
SCATTER MEMVAR BLANK         && Crea variables en blanco
PUBLIC wtipoau,wctacte,vflag,Veli

*- Mensajes de aviso al usuario
Vmens01 = ' Registro de Cheques pendientes '
Vmens02 = 'Revisi줻 de Cheques'
Vmens04 = 'El Cheque no fue encontrado'
Vmens05 = 'No existe registro anterior'
Vmens06 = 'No existe registro siguiente'
Vmens07 = '? Desea ELIMINAR este registro ?'
Vmens08 = 'No hay registros para procesar'


*- Inicia proceso
DO Inicia                    && Define ventanas, men즧, t죜ulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men? mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

RETURN


PROCEDURE Inicia             && Crea ventanas, men즧 y t죜ulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
 *DO Logos WITH Rotulo1,vTempo

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


PROCEDURE Pantalla           && Pinta m쟳cara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @ 03,02 SAY "            Periodo :"
 @ 05,02 SAY "                Mes :"
 @ 07,02 SAY "   Cuenta Corriente :"
 @ 09,02 SAY "         Saldo Debe :"
 @ 11,02 SAY "        Saldo Haber :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT SalBcos
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR
 @ 03,24 SAY m.periodo
 @ 05,24 SAY m.nummes
 @ 07,24 SAY m.codctc
 @ 09,24 SAY m.saldeb   PICTURE '999,999,999.99'
 @ 11,24 SAY m.salhab   PICTURE '999,999,999.99'
 RETURN

PROCEDURE Revis              && Revisi줻 de BD en browse
*--------------
SELE SalBcos
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '같같같같같?Presione 췋10? para seleccionar  o  췊sc? para cancelar같같같같같같'
 *DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 periodo,;
 nummes,;
 codctc  :H='Cta Cte' ,;
 saldeb  :H='Debe' ,;
 salhab  :H='Haber' 
 vTempo = '같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같'
 *DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 DO Vista
 RETURN

PROCEDURE Busca
*--------------
return
 SELECT caja

 IF EOF()
    DO standby WITH "Archivo: vac죓. No hay registros para procesar."
    RETURN
 ENDIF

 Vtemp = RECNO()

 STORE SPACE(14) TO vGen

 ACTIVATE WINDOW standby
 @ 1,03 SAY "   Cuenta Corriente :" GET vGen
 READ
 DEACTIVATE WINDOW standby

 IF EMPTY(vGen) .OR. LASTKEY()=27
    GOTO Vtemp
 ELSE
    SEEK vGen
    IF !FOUND()
       DO standby WITH "Dicha Cuenta no est? registrada"
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
 SELECT SalBcos
 SCATTER MEMVAR
 IF RLOCK() OR F_LOCK(1)
 @ 03,24 GET m.periodo PICTURE '99'
 @ 05,24 GET m.nummes  PICTURE '99' VALID Val_Para(m.numMes,'FECMES',' ',24,9)
 @ 07,24 GET m.codctc  VALID Val_fun('Caja','CodCtc','codctc+descri',m.codctc,1,07,42,'LEFT(DESCRI,38)')
 @ 09,24 GET m.saldeb  PICTURE '999,999,999.99'
 @ 11,24 GET m.salhab   PICTURE '999,999,999.99'
 READ VALID Val_Read()

	 IF LASTKEY() # 27          && Si se presion? Esc, no graba
	    SELECT SalBcos
	    GATHER MEMVAR
	 ENDIF
	 DO Vista                    && Muestra nuevos datos
 ENDIF
 UNLOCK
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT SalBcos
 DO Pantalla
 SCATTER MEMVAR BLANK
 m.periodo='97'
 @ 03,24 GET m.periodo PICTURE '99'
 @ 05,24 GET m.nummes  PICTURE '99' VALID Val_Para(m.numMes,'FECMES',' ',24,9)
 @ 07,24 GET m.codctc  VALID Val_fun('Caja','CodCtc','codctc+descri',m.codctc,1,07,42,'LEFT(DESCRI,38)')
 @ 09,24 GET m.saldeb  PICTURE '999,999,999.99'
 @ 11,24 GET m.salhab   PICTURE '999,999,999.99'
 READ VALID Val_Read()

 IF LASTKEY() # 27
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
 SELECT Caja
 DO Vista
 RETURN


PROCEDURE Elimi
*--------------
 SELECT SalBcos
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 velimina = YESNO('? Desea ELIMINAR FISICAMENTE 굎ta registro?')
	 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
	   DELETE NEXT 1
	   GO BOTTOM
	   DO Vista
	 ENDIF
 UNLOCK
 RETURN

PROCEDURE Lista
*--------------
return
SELECT Caja 
Vind=SYS(3)+'.idx'
vtemp =RECNO()
IF EOF()
   DO standby WITH vmens08
   RETURN
ELSE
	IF escolor
		DEFINE POPUP xcot  FROM 15,55 SHADOW COLOR &l_col
	ELSE
		DEFINE POPUP xcot  FROM 15,55 COLOR SCHEME c_popup
	ENDIF
	DEFINE BAR 1 OF xcot PROMPT 'Por \<C줰igo     '
	DEFINE BAR 2 OF xcot PROMPT 'Por \<Banco      '
	DEFINE BAR 3 OF xcot PROMPT 'Por \<Tipo       '
	DEFINE BAR 4 OF xcot PROMPT 'Por \<Pertenencia'
	DEFINE BAR 5 OF xcot PROMPT 'Por \<Fuente     '
	ON SELECTION POPUP xcot  DEACTIVATE POPUP
	ACTIVATE POPUP xcot
	DO CASE
	   CASE BAR() = 1
		    do reporte with 2,"LisCaj",' Listado de Cuentas Corrientes '
	   CASE BAR() = 2
	        INDEX ON banco TO (Vind) 
		    DO reporte WITH 2,"LisCajB",' Listado de Cuentas Corrientes por Banco'
		    SET INDEX TO
		    CLOSE INDEX
		    ERASE (vind)
			SET ORDER  TO Cajas1
	   CASE BAR() = 3
	        INDEX ON clase TO (Vind) 
		    DO reporte WITH 2,"LiscajT",' Listado de Cuentas Corrientes por Tipo'
		    SET INDEX TO
		    CLOSE INDEX
		    ERASE (vind)
			SET ORDER  TO Cajas1
	   CASE BAR() = 4
	        INDEX ON tipo TO (Vind) 
		    DO reporte WITH 2,"LiscajP",' Listado de Cuentas Corrientes por Pertenencia'
		    SET INDEX TO
		    CLOSE INDEX
		    ERASE (vind)
			SET ORDER  TO Cajas1
	   CASE BAR() = 5
	        INDEX ON Codfte TO (Vind) 
		    DO reporte WITH 2,"LiscajF",' Listado de Cuentas Corrientes por Fuente de Fianciamiento'
		    SET INDEX TO
		    CLOSE INDEX
		    ERASE (vind)
			SET ORDER  TO Cajas1
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
  *RESTORE SCREEN FROM PRINCIPAL
  RETURN


PROCEDURE Corrauxi
*-------------------
SELE auxi
SEEK  wTipoau + m.Codctc
IF FOUND()
   IF RLOCK() OR F_LOCK(1)
	   REPLACE Tipo   WITH wtipoau,;
	           Codigo WITH m.codctc,;
	           descri WITH m.descri
   ENDIF
   UNLOCK
ENDIF
RETURN   
      
PROCEDURE Graauxi
*-------------------
SELECT Parma
SEEK 'CORREL'+'CTACTE'
wcorr=nument+1
wcorr = SUBSTR(STR(wcorr,4),3,2)
SELE auxi
IF F_Appd()
   REPLACE Tipo      WITH wtipoau,;
           Codigo    WITH m.codctc,;
           descri    WITH m.descri,;
           direccion WITH wcorr
ENDIF
UNLOCK ALL
SELECT Parma
SEEK 'CORREL'+'CTACTE'
REPLACE NumEnt WITH NumEnt + 1       
UNLOCK
RETURN

PROCEDURE Eliauxi
*-------------------
SELE auxi
SEEK  wTipoau + wctacte
IF FOUND()
   IF RLOCK() OR F_LOCK(1)
	   DELETE NEXT 1
	   GO BOTTOM
   ENDIF
   UNLOCK
ENDIF
RETURN   

FUNCTION Corcorre
*----------------
IF m.sector = 'N'
	DEFINE WINDOW Wind_3 FROM 18,14 TO 22,60 DOUBLE ;
 	TITLE "Correlativo" COLOR SCHEME 10
	ACTIVATE WINDOW Wind_3
	@ 1,1 SAY " Correlativo C/P : " GET m.corcp 
	READ VALID Val_read()
	REPLACE corcp WITH m.corcp
	DEACTIVATE WINDOW Wind_3
ELSE
	DEFINE WINDOW Wind_3 FROM 18,14 TO 22,60 DOUBLE ;
 	TITLE "Correlativo" COLOR SCHEME 10
	ACTIVATE WINDOW Wind_3
	@ 1,1 SAY " Correlativo C/P : " GET m.corcp 
	READ VALID Val_read()
	REPLACE corcp WITH m.corcp
	DEACTIVATE WINDOW Wind_3

	IF m.codfte='00'
		DEFINE WINDOW Wind_3 FROM 05,02 TO 18,77 DOUBLE ;
	 		TITLE "Correlativo" COLOR SCHEME 10
		SELECT Ctasec
		BROWSE WINDOW WIND_3 KEY m.codctc NOMENU NODELETE NOAPPEND FIELD ;
			Sector :H='Sector':R:W=.F.,;
			corsec
	ENDIF
	SELECT Caja
ENDIF
RETURN

FUNCTION vctc
*------------
SELE Caja
SEEK m.codctc
IF FOUND()
   DO STANDBY WITH 'Cuenta Corriente ya existe' 
   RETURN  .F.
ENDIF
RETURN .T.
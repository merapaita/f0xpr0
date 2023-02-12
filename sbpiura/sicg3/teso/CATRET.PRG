* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ CatRet.PRG  28/05/96                                L: 0395  ³	
* ³ Cat logo de Retenciones                                      ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

USE Parmae   IN 1  ORDER TAG Parmae1  ALIAS Parma
USE Auxil    IN 3  ORDER TAG Auxil1   ALIAS AuxiL
USE Cuentas  IN 7   ORDER TAG Cuentas1 ALIAS cuenta
*- Variables de trabajo (registro a trabajar)
SELECT Parma
SET FILT TO Tipo='CODRET'
GO BOTTOM
SCATTER MEMVAR BLANK         && Crea variables en blanco
*- Mensajes de aviso al usuario
Vmens01 = ' ± Registro de Rubro de Retenciones ± '
Vmens02 = 'Revisi¢n de Rubros'
Vmens05 = 'No existe Rubro anterior'
Vmens06 = 'No existe Rubro siguiente'
Vmens08 = 'No hay registros para procesar'
PUBLIC vtipo,Wtipoau
wtipoau='40'
vtipo=1
*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista
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
 @ 01,02 SAY "   C¢digo del Rubro :"
 @ 03,02 SAY "        Descripci¢n :"
 @ 06,02 SAY "               Tipo :"
 @ 08,02 SAY "     Tributo [RF G] :"
 @ 10,02 SAY "     Fecha Apertura :"
 @ 12,02 SAY " Asiento Autom tico :"
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
 Vtipo=IIF(ALLT(m.descriau2)='T',2,1)
 @ 06,24 GET vtipo FUNCTION '*RNH Ter\<Ceros;\<Tesoro' SIZE 1,10,6 COLOR SCHEME 11 WHEN .F.
* @ 08,24 SAY IIF(ALLT(m.codigoaux)='T','S','N') PICTURE "!"
 @ 08,24 SAY m.DESCRIAU2 PICTURE "!"
 @ 10,24 SAY m.Fecha
 @ 12,24 SAY m.descriaux
 @ 12,36 SAY val_fun('Cuenta','Cuenta',"DescRi",LEFT(m.Descriaux,11))
 READ
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
 codigo  :H='Rubro' ,;
 descri  :30 :H='Detalle' ,;
 x1=IIF(ALLT(descriau2)='T','TESORO','TERCEROS') :12 :H='A Favor',;
 descriaux :10 :H='Asiento',;
 X2=LEFT(DESCRIAU2,6) :5 :H='Tributo',;
 fecha   :H='fecha'
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
 STORE SPACE(04) TO vGen
 ACTIVATE WINDOW standby
 @ 1,03 SAY "   C¢digo del Rubro :" GET vGen
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vGen) .OR. LASTKEY()=27
    GOTO Vtemp
 ELSE
    SEEK 'CODRET'+vGen
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
 vtipo= IIF(ALLT(m.descriau2)='T',2,1)
 m.tipo='CODRET'
 IF RLOCK() OR F_LOCK(1)
    @ 01,24 GET m.Codigo DISABLE
    @ 03,24 GET m.Descri PICTURE '@S36'
    @ 06,24 GET vtipo FUNCTION '*RNH Ter\<Ceros;\<Tesoro' SIZE 1,10,6  COLOR SCHEME 11
*    @ 08,24 GET m.codigoaux FUNCTION '@M N,S'
    @ 08,24 GET m.DESCRIAU2 FUNCTION '@M R,F, ,G'
    @ 10,24 GET m.Fecha 
    @ 12,24 GET m.descriaux 
    READ VALID Val_Read()
    IF LASTKEY() # 27          && Si se presion¢ Esc, no graba
*      IF vtipo=2
*         m.descriau2='T'
*      ENDIF
*       IF ALLT(m.codigoaux)='S'
*          m.codigoaux='T'
*       ELSE    
*          m.codigoaux=' '
*       ENDIF   
       SELECT Parma
       GATHER MEMVAR
       DO corrauxi
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
 m.tipo='CODRET'
 @ 01,24 GET m.Codigo PICTURE '9999' VALID vret() 
 @ 03,24 GET m.Descri PICTURE '@S36'
 @ 06,24 GET vtipo FUNCTION '*RNH Ter\<Ceros;\<Tesoro' SIZE 1,10,6 COLOR SCHEME 11
* @ 08,24 GET m.codigoaux FUNCTION '@M N,S'
 @ 08,24 GET m.DESCRIAU2 FUNCTION '@M R,F, ,G'
 @ 10,24 GET m.Fecha 
 @ 12,24 GET m.descriaux PICTURE "99999999999" 
 READ VALID Val_Read()
 IF LASTKEY() # 27
*      IF vtipo=2
*         m.descriau2='T'
*      ENDIF   
*       IF ALLT(m.codigoaux)='S'
*          m.codigoaux='T'
*       ELSE    
*          m.codigoaux=' '
*       ENDIF   
       IF F_Appd()
         GATHER MEMVAR
         DO graauxi
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
   DO eliauxi
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
	        INDEX ON val(codigo) TO (Vind) FOR tipo='CODRET' 
		    DO reporte WITH 2,"LisRetC",' Listado de Rubro de Retenciones '
		    SET INDEX TO
		    CLOSE INDEX
		    ERASE (vind)
	   CASE BAR() = 2
	        SET ORDER TO Parmae2
		    DO reporte WITH 2,"CatRet",' Listado de Rubro de Retenciones '
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


PROCEDURE corrauxi
*-----------------
SELE Auxil
SEEK  wTipoau + m.Codigo
IF FOUND()
   IF RLOCK() OR F_LOCK(1)
      REPLACE Tipo   WITH wtipoau,;
              Codigo WITH m.codigo,;
              descri WITH m.descri
   ENDIF
   UNLOCK
ENDIF
RETURN   
      
PROCEDURE Graauxi
*-------------------
SELE Auxil
IF F_Appd()
   REPLACE Tipo   WITH Wtipoau,;
           Codigo WITH m.codigo,;
		   descri WITH m.descri
ENDIF
UNLOCK ALL
RETURN

procedure Eliauxi
*-------------------
SELE Auxil
SEEK  wTipoau + m.codigo
IF FOUND()
   IF RLOCK() OR F_LOCK(1)
      DELETE NEXT 1
      GO BOTTOM
   ENDIF
   UNLOCK
ENDIF
RETURN   

FUNCTION vret
*------------
SELE Parma
SEEK m.tipo+m.codigo
IF FOUND()
   DO STANDBY WITH 'Rubro de Retenci¢n ya existe' 
   RETURN  .F.
ENDIF
RETURN .T.
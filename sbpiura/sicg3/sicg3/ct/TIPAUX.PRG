*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08,Vmens09,Vmens10,Vmens11
Vmens01 = ' Tipos de Auxiliar '
Vmens02 = 'Revisi¢n de Tipos de Auxiliar'
Vmens03 = 'Digite c¢digo de Tipo de Auxiliar que desea :'
Vmens04 = 'Dicho Tipo de Auxiliar no fue encontrado.'
Vmens05 = 'No existe Tipo de Auxiliar anterior.'
Vmens06 = 'No existe Tipo de Auxiliar siguiente.'
Vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste Tipo de Auxiliar?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Debe primero eliminar todos sus Auxiliares'

USE Parmae  IN 1 ORDER TAG Parmae1  ALIAS parma
USE Cuentas IN 2 ORDER TAG Cuentas1 ALIAS Cuen
USE Auxil   IN 3 ORDER TAG Auxil1   ALIAS Auxi
SELECT parma
mtipo = "AUXIL "
SET FILTER TO TIPO = mtipo
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
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

 ACTIVATE SCREEN
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @  3, 2 SAY "  Tipo de Auxiliar:"
 @  5, 2 SAY "       Descripci¢n:"
 @  7, 2 SAY "            Cuenta:"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT parma
 SCATTER MEMVAR
 @  3,22 SAY m.Codigo      PICTURE '!!'
 @  5,22 SAY m.Descri
 @  7,22 SAY m.DescriAux   PICTURE '9999999999'
 @  7,33 SAY Val_cuen(SUBSTR(m.DescriAux,1,10))
 RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
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
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     Codigo   :H='C¢digo':P='!!',;
     Descri   :H='Tipo de Auxiliar',;
     DescriAux :H='Cuenta':P='9999999999',;
     Nombre   =Val_cuen(SUBSTR(DescriAux,1,10)) :H='Descripci¢n'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 ACTIVATE WINDOW standby
 STORE SPACE(2) TO Vbusca
 @ 1,3 SAY Vmens03 GET Vbusca PICT '!!' COLOR SCHEME 1
 READ
 DEACTIVATE WINDOW standby
 IF Vbusca = SPACE(2) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK mtipo + Vbusca
   IF .NOT. FOUND()
     DO standby WITH Vmens04
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
 Bloquea = .T.
 DO Fox_Lock WITH 1, Bloquea
 IF Bloquea
   @  3,22 SAY m.Codigo        PICTURE '!!'
   @  5,22 GET m.Descri
   @  7,22 GET m.DescriAux     PICTURE '9999999999' VALID Val_cuen(m.DescriAux,.T.,7,22)
   READ
   IF LASTKEY() <> 27          && Si se presion¢ Esc, no graba
     GATHER MEMVAR
   ENDIF
   DO Vista                    && Muestra nuevos datos
 ENDIF
 UNLOCK
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR BLANK
 @  3,22 GET m.Codigo   PICTURE '!!'
 READ
 SELECT parma
 IF LASTKEY() = 27 .OR. EMPTY(m.Codigo)
   DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
   GO BOTTOM
 ELSE
   SEEK mtipo + m.Codigo
   IF FOUND()
     DO standby WITH "Tipo de Auxiliar ya est  registrado. Proceda a corregir datos."
     DO Vista
     DO Corri
   ELSE
     @  5,22 CLEAR TO 23,78
     @  5,22 GET m.Descri
     @  7,22 GET m.DescriAux     PICTURE '9999999999' VALID Val_cuen(m.DescriAux,.T.,7,22)
     READ
     IF LASTKEY() <> 27
       Agrega = .T.
       DO Fox_appd WITH Agrega
       IF Agrega
         m.tipo = mtipo
         GATHER MEMVAR
       ENDIF
       UNLOCK
     ELSE
       GO BOTTOM
     ENDIF
   ENDIF
 ENDIF
 DO Vista
 RETURN


PROCEDURE Elimi
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 STORE YESNO(Vmens07) TO vElimina
 IF vElimina
   SELECT Auxi
   IF SEEK(TRIM(m.Codigo))
     DO Standby WITH vMens09
     SELECT Parma
     RETURN
   ENDIF
   SELECT Parma
   Bloquea = .T.
   DO Fox_Lock WITH 1, Bloquea
   IF Bloquea
     DELETE NEXT 1
     IF !BOF()
       SKIP -1
     ENDIF
   ENDIF
   UNLOCK
 ENDIF
 DO Vista
 RETURN


PROCEDURE Lista
*--------------
 SELECT parma
 DO REPORTE WITH 2,"TipAux"," Tipos de Auxiliar "
 SELECT parma
 GO BOTTOM
 DO Vista

 RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    MENU   mMenu
  CLOSE DATABASE
  DO Logos WITH Rotulo1, Rotulo2
  RETURN

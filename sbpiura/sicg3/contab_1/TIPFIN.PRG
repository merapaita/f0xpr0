** --------------------------------------
** TIPFIN.PRG
** Tabla de Mantenimiento de CORRELATIVOS
** AUTOR : UDEP - 1995.            
** --------------------------------------
*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08,Vmens09,Vmens10,Vmens11
Vmens01 = 'Relaci?n de Tipos Financieros'
Vmens02 = 'Revisi?n de Tipos Financieros'
Vmens03 = 'Digite c?digo del Tipos Financiero  que desea :'
Vmens04 = 'Dicho Tipos Financiero no fue encontrado.'
Vmens05 = 'No existe Tipos Financiero anterior.'
Vmens06 = 'No existe Tipos Financiero siguiente.'
Vmens07 = '?Est? seguro que desea ELIMINAR este Tipos Financiero?'
Vmens08 = 'No hay registros para procesar'

USE Parmae IN 1 ORDER TAG Parmae1 ALIAS parma
SELECT parma
mtipo = "TIPFIN"
SET FILTER TO TIPO = mtipo
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO Inicia                    && Define ventanas, men?s, t?tulos
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


PROCEDURE Inicia             && Crea ventanas, men?s y t?tulos
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


PROCEDURE Pantalla           && Pinta m?scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @  3, 2 SAY "            C?digo:"
 @  5, 2 SAY "C?digo del Reporte:"
 @  7, 2 SAY "       Descripci?n:"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT parma
 SCATTER MEMVAR
 @  3,22 SAY m.Codigo      PICTURE '!!!!!!'
 @  5,22 SAY m.Codigoaux   PICTURE '!!!!!!'
 @  7,22 SAY m.Descri      PICTURE '@S50'
 RETURN


PROCEDURE Revis              && Revisi?n de BD en browse
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     Codigo    :H='C?digo ',;
     Codigoaux :H='Reporte',;
     Descri    :H='Descripci?n'
 vTempo = '????????????????????????????????????????????????????????????????????????????????'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
    GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b?squeda directa
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 ACTIVATE WINDOW standby
 STORE SPACE(4) TO Vbusca
 @ 1,1 SAY Vmens03 GET Vbusca PICT '!!!!!!' COLOR SCHEME 5
 READ
 DEACTIVATE WINDOW standby
 IF Vbusca = SPACE(6) .OR. LASTKEY()=27
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
    @  3,22 SAY m.Codigo        PICTURE '!!!!!!'
    @  5,22 GET m.codigoaux     PICTURE '!!!!!!'
    @  7,22 GET m.Descri        PICTURE '@S50'
   READ
   IF LASTKEY() <> 27          && Si se presion? Esc, no graba
      GATHER MEMVAR
   ENDIF
   DO Vista                    && Muestra nuevos datos
 ENDIF
 UNLOCK
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 *- Aqu? coloca valores por omisi?n (default)
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR BLANK
 @  3,22 GET m.Codigo   PICTURE '!!!!!!'
 READ
 SELECT parma
 IF LASTKEY() = 27 .OR. m.Codigo=SPACE(3)
    DO standby WITH "Proceso cancelado. No se graba ning?n cambio."
    GO BOTTOM
 ELSE
    SEEK mtipo + m.Codigo
    IF FOUND()
       DO standby WITH "Tipos Financiero ya est? registrado. Proceda a Corregir datos."
       DO Vista
       DO Corri
   ELSE
       @  5,22 CLEAR TO 23,78
       @  5,22 GET m.Codigoaux    PICTURE '!!!!!!'
       @  7,22 GET m.Descri       PICTURE '@S50'
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

 indaux = SYS(3) 
 SELECT parma
 INDEX ON TIPO+CODIGOAUX+CODIGO TO (indaux)  FOR TIPO=mtipo
 DO REPORTE WITH 2,"Tipfin"," Tipos Financieros "
 SET INDEX TO
 ERASE (indaux)
 SELECT parma
 SET ORDER TO TAG Parmae1
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
  SET FILTER TO
  DO Logos WITH Rotulo1, Rotulo2
  RETURN

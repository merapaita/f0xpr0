**--------------------------------
** ManAux .PRG
** Mantenimiento de Auxiliar
** AUTOR: UDEP - 1994. JECHS
**--------------------------------

USE Parmae  IN 1 ORDER TAG Parmae1  ALIAS parma
USE Auxil   IN 2 ORDER TAG Auxil1   ALIAS Auxi

mtipo = "AUXIL "
SET FILTER TO mTipo = Parma.Tipo
DO Inicia
DO Salida
RETURN


PROCEDURE Inicia
*---------------

  IF escolor
     DEFINE POPUP Polman FROM 08,55 TO 17,75 PROMPT FIELD PROPER(Descri) ;
     SHADOW COLOR &L_COL
  ELSE
     DEFINE POPUP Polman FROM 08,55 TO 17,75 PROMPT FIELD PROPER(Descri) ;
     COLOR SCHEME C_POPUP
  ENDIF

  ON SELECTION POPUP Polman Do Auxilio WITH ALLTRIM(Descri), Codigo
  ACTIVATE POPUP Polman

RETURN


PROCEDURE Salida
*---------------
  RELEASE    POPUP  Polman
  ACTIVATE   SCREEN
  CLOSE DATABASE
RETURN

*----------------

PROCEDURE AUXILIO
*-------------------
PARAMETERS mDescri, xTipo
*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08,Vmens09,Vmens10,Vmens11
Vmens01 = ' Cat logo de '+ mDescri+ ' '
Vmens02 = 'Revisi¢n de '+ mDescri
Vmens03 = 'Digite c¢digo de Auxiliar que desea :'
Vmens04 = 'Dicho Auxiliar no fue encontrado.'
Vmens05 = 'No existe Registro anterior.'
Vmens06 = 'No existe Registro siguiente.'
Vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste Registro?'
Vmens08 = 'No hay registros para procesar'


xTipo = ALLTRIM(xTipo)

SELECT parma
SET FILTER TO

SELECT Auxi
SET FILTER TO  Auxi.Tipo=XTIPO
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
*- Inicia proceso
DO reInicia                    && Define ventanas, men£s, t¡tulos
DO rePantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion
RETURN


PROCEDURE reInicia             && Crea ventanas, men£s y t¡tulos
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


PROCEDURE rePantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @  2, 2 SAY "            C¢digo:"
 @  4, 2 SAY "       Descripci¢n:"
 @  6, 2 SAY "         Direcci¢n:"
 @  8, 2 SAY "        Tel‚fono 1:"
 @  8,40 SAY "Telefono 2:"
 @ 10, 2 SAY "           Anexo 1:"
 @ 10,40 SAY "   Anexo 2:"
 @ 12, 2 SAY "             R.U.C:"
 @ 14, 2 SAY "     Observaciones:"
 @ 16, 2 SAY " Auxiliar es Banco:"
 @ 18, 2 SAY "             Banco:"
 @ 05, 2 SAY "Auxiliar  / Codigo:"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT Auxi
 SCATTER MEMVAR
 @  2,22 SAY m.Codigo   PICTURE '!!!!!!!!!!'
 @  4,22 SAY m.Descri
 @ 05,22 SAY m.newTip 
 @ 05,25 SAY m.newcod                              

 @  6,22 SAY m.Direccion
 @  8,22 SAY m.Telf_1
 @  8,52 SAY m.Telf_2
 @ 10,22 SAY m.Anexo_1
 @ 10,52 SAY m.Anexo_2
 @ 12,22 SAY m.RUC
 @ 14,22 SAY m.Observ
 @ 16,22 SAY IIF(m.TipBan, 'Si', 'No')
 @ 18,22 SAY m.Banco
 @ 05,22 SAY m.newTip 
 @ 05,25 SAY m.newcod                              

 IF !EMPTY(m.Banco)
    @ 18,26 SAY Val_Para(m.Banco,'BANCOS')
 ENDIF   
 RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 ACTIVATE WINDOW STANDBY
 vopcion = 'Descripci¢n'
 @ 1,4 SAY 'Revisi¢n Ordenado por : ' GET vopcion PICTURE '@M Descripci¢n,C¢digo     '
 READ
 DEACTIVATE WINDOW STANDBY
 IF LASTKEY()=27
    DO vista
    RETURN
 ENDIF
 IF vOpcion='Descripci¢n'
    SET ORDER TO TAG AUXIL10
 ELSE
   SET ORDER TO TAG AUXIL1
 ENDIF
 GOTO vTemp        
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     Codigo   :H='C¢digo',;
     Descri   :H='Descripci¢n',;
     Direccion:H='Direcci¢n'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET ORDER TO TAG AUXIL1
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 STORE SPACE(10) TO vCodigo
 STORE SPACE(30) TO vDescri
 tb=yesno("Desea b£squeda por C¢digo (NO = por Descripci¢n)")
 IF LASTKEY()=27
    DO VISTA
    RETURN
 ENDIF
 ACTIVATE WINDOW standby
 @ 1,1 SAY IIF(tb,"     C¢digo : ","Descripci¢n : ")
 IF tb
    @ 1,15 GET vCodigo PICTURE "!!!!!!!!!!"
 ELSE
    @ 1,15 GET vDescri PICTURE REPLICATE("!",30)
 ENDIF   
 READ
 DEACTIVATE WINDOW standby
 IF tb
   SET ORDER TO TAG AUXIL1
   SEEK xTipo + ALLTRIM(vCodigo)
 ELSE
   SET ORDER TO TAG AUXIL10
   SEEK xTipo + ALLTRIM(vDescri)
 ENDIF
 SET ORDER TO TAG AUXIL1
 IF EOF()
     DO standby WITH "Dich" + IIF(tb, "o C¢digo ","a Descripci¢n ") + "no fu‚ encontrada."
     GOTO Vtemp
 ELSE
     DO Vista
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
   xTipBan = IIF(m.TipBan ,'Si', 'No')
   @  2,22 SAY m.Codigo        PICTURE '!!!!!!!!!!'
   @  4,22 GET m.Descri
   @ 05,22 GET m.newTip PICTURE '!!'
   @ 05,25 GET m.newcod PICTURE '!!!!!!!!!!!!'                             
 
   @  6,22 GET m.Direccion
   @  8,22 GET m.Telf_1
   @  8,52 GET m.Telf_2
   @ 10,22 GET m.Anexo_1
   @ 10,52 GET m.Anexo_2
   @ 12,22 GET m.RUC
   @ 14,22 GET m.Observ
   @ 16,22 GET xTipBan         PICTURE '@M Si,No'
   @ 18,22 GET m.Banco         PICTURE '!!'     VALID Val_para(m.Banco,'BANCOS',' ',26);
                               WHEN xTipBan = 'Si'
                               
*   @ 19,22 GET m.newTip PICTURE '!!'
*   @ 19,25 GET m.newcod PICTURE '!!!!!!!!!!!!'                             
   READ
   IF LASTKEY() <> 27          && Si se presion¢ Esc, no graba
     m.TipBan = IIF(xTipBan = 'Si', .T., .F.)
     IF !m.TipBan
       m.Banco  = SPACE(LEN(m.Banco))
     ENDIF
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
 GO BOTT
 m.codigo=PADL(ALLTRIM(STR(VAL(Auxi.Codigo)+1,6)),6,'0')
 
 @  2,22 GET m.Codigo   PICTURE '!!!!!!'
 READ
 SELECT Auxi
 IF LASTKEY() = 27 .OR. m.Codigo=SPACE(6)
   DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
   GO BOTTOM
 ELSE
   SEEK  xTipo + m.Codigo
   IF FOUND()
     DO standby WITH "Auxiliar ya est  registrado. Proceda a corregir datos."
     DO Vista
     DO Corri
   ELSE
     xTipBan = IIF(m.TipBan ,'Si', 'No')
     @ 04,22 CLEAR TO 23,78
     @ 04,22 GET m.Descri
     @ 05,22 GET m.newTip PICTURE '!!'
     @ 05,25 GET m.newcod PICTURE '!!!!!!!!!!!!'                             
     @ 06,22 GET m.Direccion
     @ 08,22 GET m.Telf_1
     @ 08,52 GET m.Telf_2
     @ 10,22 GET m.Anexo_1
     @ 10,52 GET m.Anexo_2
     @ 12,22 GET m.RUC
     @ 14,22 GET m.Observ
     @ 16,22 GET xTipBan         PICTURE '@M Si,No'
     @ 18,22 GET m.Banco         PICTURE '!!'     VALID Val_para(m.Banco,'BANCOS',' ',26);
                                 WHEN xTipBan = 'Si'
                                 
     READ
     IF LASTKEY() <> 27
       Agrega = .T.
       DO Fox_appd WITH Agrega
       IF Agrega
         m.Tipo = xTipo
         m.TipBan = IIF(xTipBan = 'Si', .T., .F.)
         IF !m.TipBan
           m.Banco  = SPACE(LEN(m.Banco))
         ENDIF
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
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 ACTIVATE WINDOW STANDBY
 vopcion = 'Descripci¢n'
 @ 1,4 SAY 'Listado Ordenado por : ' GET vopcion PICTURE '@M Descripci¢n,C¢digo     '
 READ
 SELECT Auxi
 nReg = RECNO()
 IF LASTKEY() # 27
   IF vopcion = 'Descripci¢n'
     SET ORDER TO TAG AUXIL10
     DO REPORTE WITH 2,"Auxil2"," Auxiliares "
   ELSE
     DO REPORTE WITH 2,"Auxil2"," Auxiliares "
   ENDIF
 ENDIF
 DEACTIVATE WINDOW STANDBY
 SELECT Auxi
 SET ORDER TO TAG AUXIL1
 GO nReg
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
  SELECT parma
  SET FILTER TO Tipo = mTipo
  DO Logos WITH Rotulo1, Rotulo2
RETURN

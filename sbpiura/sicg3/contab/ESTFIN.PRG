** ---------------------------------------------
** ESTFIN.PRG
** Tabla de Mantenimiento de tipos o Estados para identificar 
** montos o valores para el proceso de los 
** Estados Financieros
** AUTOR : UDEP - 1995.  OAOC
** ---------------------------------------------
*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08,Vmens09,Vmens10,Vmens11
Vmens01 = 'Relaci¢n de Tipos'
Vmens02 = 'Revisi¢n de Tipos'
Vmens03 = 'Digite c¢digo de Tipo de Reporte que Desea :'
Vmens04 = 'Dicho Tipo no fue encontrado.'
Vmens05 = 'No existe Tipo anterior.'
Vmens06 = 'No existe Tipo siguiente.'
Vmens07 = '¨Est  seguro que desea ELIMINAR este Registro?'
Vmens08 = 'No hay Registros para Procesar'

USE EstFin    IN 1 ORDER TAG EstFin1   ALIAS EstFin
USE Parmae    IN 2 ORDER TAG Parmae1   ALIAS Parma
USE Cuentas   IN 3 ORDER TAG Cuentas1  ALIAS Cuenta
SELECT EstFin
SET RELATION TO cuenta INTO cuenta

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
   @  3,2 SAY "C¢digo del Reporte  :"
   @  5,2 SAY "            Cuenta  :"
   @  6,2 SAY "Descripci¢n Cuenta  :"
   @  7,2 SAY "      Debe / Haber  :"    
   @  9,2 SAY "              Tipo  :"
   @ 11,2 SAY "              Fila  :"
   @ 13,2 SAY "           Columna  :" 
   @ 15,2 SAY " Tipo de Documento  :"   
   @ 17,2 SAY " Tipo Financiero 95 :"   
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT EstFin
 SCATTER MEMVAR
 @  3,24 SAY m.CodRep    PICTURE '!!!!!'
 @  3,29 SAY SPACE(40)
 @  3,29 SAY Val_para(m.CodRep,'CODREP','D',24,40)
 @  5,24 SAY m.Cuenta    PICTURE '!!!!!!!!!!'
 @  6,24 SAY IIF(!EOF('Cuenta'),cuenta.descri,PADR(' No existe en el Plan de Cuentas! ',50,'±'))
 @  7,24 SAY m.Debhab    PICTURE '!'  
 @  9,24 SAY m.Tipo      PICTURE '!!!!'
 @ 11,24 SAY m.Fila      PICTURE '!!!'  
 @ 13,24 SAY m.Columna   PICTURE '!!!'  
 @ 15,24 SAY m.TipDoc    PICTURE '!!!'
 @ 15,29 SAY SPACE(40)
 @ 17,24 SAY m.TFin95    PICTURE '!!!!'
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
     CodRep    :H='Reporte',;
     Cuenta    :H='   Cuenta   ',;
     TipDoc    :H='Documento',;
     debhab    :H='D/H',;          
     Tipo      :H='Tipo',;
     Fila      :H='Fila',;
     Columna   :H='Columna',;
     TFin95    :H='TFin95'
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
 STORE SPACE(5) TO Vbusca
 @ 1,3 SAY Vmens03 GET Vbusca PICT '!!!!!' COLOR SCHEME 7
 READ
 DEACTIVATE WINDOW standby
 IF Vbusca = SPACE(5) .OR. LASTKEY()=27
    GOTO Vtemp
 ELSE
    SEEK Vbusca
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
     @  3,24 SAY m.CodRep     PICTURE '!!!!!'
     @  5,24 SAY m.Cuenta     PICTURE '!!!!!!!!!!'
     * @  5,34 SAY Val_Fun("Cuenta","Cuenta","Cuenta+' '+Descri",m.Cuenta,1,5,22)
	 @  7,24 SAY m.Debhab     PICTURE '!' 
 	 @  9,24 GET m.Tipo      PICTURE '!!!!' VALID Val_para(m.Tipo,'TIPFIN',' ',24,40)
 	 * Validar con TIPFIN
	 @ 11,24 GET m.Fila      PICTURE '!!!'  
	 @ 13,24 GET m.Columna   PICTURE '!!!'  
	 @ 15,24 GET m.TipDoc    PICTURE '!!'
	 @ 17,24 GET m.TFin95    PICTURE '!!!!'
   READ VALID VAL_READ()
   @ 9,22 CLEAR TO 23,78
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
 DO PANTALLA
 SCATTER MEMVAR BLANK
 @  3,24 GET m.CodRep    PICTURE '!!!!!' VALID Val_para(m.CodRep,'CODREP',' ',24,40)
 @  5,24 GET m.Cuenta    PICTURE '!!!!!!!!!!' 
 * VALID Val_Fun("Cuenta","Cuenta","Cuenta+' '+Descri",m.Cuenta,1,5,22)
 @  7,24 GET m.Debhab    PICTURE '!' FUNCTION "M D,H"
 READ VALID VAL_READ()
 IF LASTKEY() = 27 .OR. m.CodRep=SPACE(5) .OR. m.TipDoc=SPACE(3)
    DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
    GO BOTTOM
 ELSE
    SEEK m.CodRep+m.TipDoc
    IF FOUND()
       DO standby WITH "Tipo ya est  registrado. Proceda a corregir datos."
       DO Vista
       DO Corri
    ELSE
        @  9,24 CLEAR TO 23,78
   	    @  9,24 GET m.Tipo      PICTURE '!!!!' VALID Val_para(m.Tipo,'TIPFIN',' ',24,40)
	    @ 11,24 GET m.Fila      PICTURE '!!!'  
	    @ 13,24 GET m.Columna   PICTURE '!!!'  
        @ 15,24 GET m.TipDoc    PICTURE '!!' 
        @ 17,24 GET m.TFin95    PICTURE '!!!!'
        * colocar el codigo Auxiliar
        * VALID Val_para(m.TipDoc,'TIPDOC',' ',22,40)

       READ VALID VAL_READ()
       @ 7,22 CLEAR TO 23,78
       IF LASTKEY() <> 27
          Agrega = .T.
          DO Fox_appd WITH Agrega
          IF Agrega
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
 DEFINE WINDOW vlis FROM 08,20 to 17,60 TITLE ' Lista ' COLOR SCHEME 5
 ACTIVATE WINDOW vlis
 vcodrep = space(4)
 @ 02,02 SAY '      Listado:' 
 @ 01,20 GET vtiplis FUNCTION '^ Un Tipo de EE.FF.; Todos' DEFAULT 1
 @ 04,02 SAY 'Codigo EE.FF.:'
 @ 04,20 GET vcodrep PICTURE '!!!!' valid val_para(vcodrep,'CODREP',' ',21,10) WHEN vtiplis=1
 @ 06,02 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,10
 READ
 DEACTIVATE WINDOW vlis
 IF LASTKEY()=27 OR okcancel=2
    RETURN
 ENDIF
 SELECT EstFin
 vreg = RECNO()
 IF vtiplis=1
    SET FILTER TO ALLTRIM(codrep)=ALLTRIM(vcodrep)
 ENDIF
 
 DO REPORTE WITH 2,"EstFin2"," ± Estructura Financiera ± "
 SET FILTER TO
 GO vreg
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
  CLOS DATA
  DO Logos WITH Rotulo1, Rotulo2
RETURN

FUNCTION LEETIP
*--------------
SELE Parma
SEEK 'TIPFIN'+Estfin.Tipo
IF FOUND()
   RETURN Parma.descri
ELSE 
   RETURN '   '
ENDIF   

RETURN

FUNCTION LEETIP1
*--------------
SELE Parma
SEEK 'TFIN95'+Estfin.TFin95
IF FOUND()
   RETURN Parma.descri
ELSE 
   RETURN '   '
ENDIF   

RETURN

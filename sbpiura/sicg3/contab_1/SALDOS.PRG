**-----------------------------------
** SALDOS.PRG
** Ingreso de Sumas y Saldos
**
** Autor : UDEP
**-----------------------------------

*- Mensajes de aviso al usuario
PUBLIC vNivel,vDescNiv

PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08,Vmens09,Vmens10,Vmens11
Vmens01 = ' Saldos Iniciales '
Vmens02 = 'Saldos Iniciales'
Vmens03 = 'Digite c¢digo de Cuenta que desea :'
Vmens04 = 'Dicha Cuenta no fue encontrado.'
Vmens05 = 'No existe Cuenta anterior.'
Vmens06 = 'No existe Cuenta siguiente.'
Vmens07 = '¨Est  seguro que desea ELIMINAR ‚sta Cuenta?'
Vmens08 = 'No hay registros para procesar'

Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
USE SaldoNa  IN 1 ORDER TAG SaldoNa1 ALIAS SalN
USE Cuentas  IN 3 ORDER TAG Cuentas1 ALIAS Cuen 
USE Parmae   IN 4 ORDER TAG Parmae1  ALIAS parma

SELE SalN
SET RELATION TO SalN.Cuenta INTO Cuen


SELECT parma
SEEK "MESANO"+"ACTUAL"
_actmes = LEFT(descri,6)
vmes = VAL(SUBSTR(_actmes,5,2))
vano = VAL(SUBSTR(_actmes,3,2))
 IF vmes = 1
   vano = vano - 1
   vmes = 12
 ELSE
   vmes = vmes - 1
 ENDIF

SELECT SalN
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
* DO Pregunta
* IF EMPTY(vNivel)
*   DO Fin_Opcion
*   RETURN
*ENDIF   

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

*PROCEDURE Pregunta
*-----------------
*vNivel   = LeeNivInt()
*IF !EMPTY(vNivel)
*   vDescNiv = ALLTRIM(parma.descri)
*   SELECT SalN
*   SET FILTER TO SalN.NivInt=vNivel
*ENDIF   
*RETURN



PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------

 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01+' °   Saldos y Sumas   ° ' COLOR SCHEME 10

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
 @  2, 2 SAY "            Cuenta:"
 @  3, 2 SAY "               Mes:"
 @  4, 2 SAY "               A¤o:"
 @  5, 2 SAY "  Movimiento  Debe:"
 @  6, 2 SAY "  Movimiento Haber:"
 @  7, 2 SAY "       Saldo  Debe:"
 @  8, 2 SAY "       Saldo Haber:"
 @  9, 2 SAY "Mov. Debe Ajustado:"
 @ 10, 2 SAY "Mov.Haber Ajustado:"
 @ 11, 2 SAY "Sal. Debe Ajustado:"
 @ 12, 2 SAY "Sal.Haber Ajustado:"
 @ 13, 2 SAY "Mto. Debe Ajustado:"
 @ 14, 2 SAY "Mto.Haber Ajustado:"
 @ 15, 2 SAY "    Valor  Mercado:"
 @ 16, 2 SAY "    Valor   Limite:"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT SalN
 SCATTER MEMVAR
 @  2,22 SAY m.Cuenta    PICTURE '9999999999'
 @  2,33 SAY IIF(!EMPTY(m.cuenta),Val_Cuen(m.Cuenta),SPACE(40))
 @  3,22 SAY m.Mes       PICTURE '99'
 @  3,25 SAY IIF(!EMPTY(m.mes), SUBSTR(Meses,m.Mes*9-8,9) , SPACE(15))
 @  4,22 SAY m.Ano       PICTURE '99'
 @  5,22 SAY m.DSumaNa   PICTURE '999,999,999,999.999'
 @  6,22 SAY m.HSumaNa   PICTURE '999,999,999,999.999'
 @  7,22 SAY m.DSaldoNa  PICTURE '999,999,999,999.999'
 @  8,22 SAY m.HSaldoNa  PICTURE '999,999,999,999.999'
 @  9,22 SAY m.DSumaju   PICTURE '999,999,999,999.999'
 @ 10,22 SAY m.HSumaju   PICTURE '999,999,999,999.999'
 @ 11,22 SAY m.DSaldoaju PICTURE '999,999,999,999.999'
 @ 12,22 SAY m.HSaldoaju PICTURE '999,999,999,999.999'
 @ 13,22 SAY m.Dajumes   PICTURE '999,999,999,999.999'
 @ 14,22 SAY m.Hajumes   PICTURE '999,999,999,999.999'
 @ 15,22 SAY m.ValMerc   PICTURE '999,999,999,999.999'
 @ 16,22 SAY m.Vallimi   PICTURE '999,999,999,999.999'
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
     Cuenta   :H='Cuenta',;
     Descri   =Val_Cuen(Cuenta) :H='Descripci¢n                            ',;
     Mes      :H='Mes',;
     ANo      :H='A¤o',;
     DSaldoNa :H= 'Saldo Debe S/.'  :P='999,999,999,999.99',;
     HSaldoNa :H= 'Saldo Haber S/.' :P='999,999,999,999.99' 
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
 STORE SPACE(10) TO Vbusca
 @ 1,3 SAY Vmens03 GET Vbusca PICT '@9999999999' COLOR SCHEME 1
 READ
 DEACTIVATE WINDOW standby
 IF Vbusca = SPACE(10) .OR. LASTKEY()=27
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
    @  2,22 SAY m.Cuenta    PICTURE '9999999999'
    @  2,33 SAY Val_Cuen(m.Cuenta)
    @  3,22 SAY m.Mes       PICTURE '99'
    @  3,25 SAY SUBSTR(Meses,m.Mes*9-8,9)
    @  4,22 SAY m.Ano       PICTURE '99'
    @  5,22 GET m.DSumaNa   PICTURE '999,999,999,999.999'
    @  6,22 GET m.HSumaNa   PICTURE '999,999,999,999.999'
    @  7,22 GET m.DSaldoNa  PICTURE '999,999,999,999.999'
    @  8,22 GET m.HSaldoNa  PICTURE '999,999,999,999.999'
    @  9,22 GET m.DSumaju   PICTURE '999,999,999,999.999'
    @ 10,22 GET m.HSumaju   PICTURE '999,999,999,999.999'
    @ 11,22 GET m.DSaldoaju PICTURE '999,999,999,999.999'
    @ 12,22 GET m.HSaldoaju PICTURE '999,999,999,999.999'
    @ 13,22 GET m.Dajumes   PICTURE '999,999,999,999.999'
    @ 14,22 GET m.Hajumes   PICTURE '999,999,999,999.999'
    @ 15,22 GET m.ValMerc   PICTURE '999,999,999,999.999'
    @ 16,22 GET m.Vallimi   PICTURE '999,999,999,999.999'
    
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
 m.Mes = vmes
 m.Ano = vano
 @  2,22 GET m.Cuenta   PICTURE '9999999999' VALID Val_Fun('Cuen','Cuenta',"TRIM(Cuenta) +' '+ Descri",m.Cuenta,1,3,22,'SUBSTR(Descri,1,14)') ERROR 'Cuenta no existe'
 @  3,22 GET m.Mes      PICTURE '99'
 @  3,25 SAY SUBSTR(Meses,m.Mes*9-8,9)
 @  4,22 GET m.Ano      PICTURE '99'
 READ
 SELECT SalN
 IF LASTKEY() = 27 .OR. m.Cuenta=SPACE(10)
    DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
    GO BOTTOM
 ELSE
    SEEK  m.Cuenta + STR(m.Mes,2) + STR(m.Ano,2)
    IF FOUND()
       DO standby WITH "Cuenta ya est  registrada. Proceda a corregir datos."
       DO Vista
       DO Corri
    ELSE    
       @  2,22 CLEAR TO 23,78
       @  2,22 SAY m.Cuenta    PICTURE '9999999999'
       @  2,33 SAY Val_Cuen(m.Cuenta)
       @  3,22 SAY m.Mes       PICTURE '99'
       @  3,25 SAY SUBSTR(Meses,m.Mes*9-8,9)
       @  4,22 SAY m.Ano       PICTURE '99'
       @  5,22 GET m.DSumaNa   PICTURE '999,999,999,999.99'
       @  6,22 GET m.HSumaNa   PICTURE '999,999,999,999.99'
       @  7,22 GET m.DSaldoNa  PICTURE '999,999,999,999.99'
       @  8,22 GET m.HSaldoNa  PICTURE '999,999,999,999.99'
       
       READ
       IF LASTKEY() <> 27 .AND. yesno("Confirme el ingreso")
          IF F_APPD()
             GATHER MEMVAR
             UNLOCK
          ENDIF
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
       SELECT SalN
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
DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE ' Listado de Saldos Iniciales ' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vLista
vmes = 0
vano = 0
IF LASTKEY()=27
   RETURN
ENDIF

@ 01,01 SAY "             Saldos: " 
@ 01,22 GET vLista FUNCTION '^ Todos los Saldos; De un Mes' 
@ 04,01 SAY "     Mes a Reportar: "
@ 04,22 GET vmes  WHEN vlista=2 PICTURE '99' VALID vmes>=1 AND vmes<=12
@ 06,01 SAY "     A¤o a Reportar: "
@ 06,22 GET vAno WHEN vlista=2 PICTURE '99' 
@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS

IF OKCANCEL = 1
   SELECT SalN
   IF vlista=2 
      SET FILTER TO SalN.mes=vmes AND SalN.ano=vano 
   ENDIF   
 GO TOP 
 IF EOF()
    DO STANDBY WITH "No hay registros para procesar."
 ELSE    
    DO REPORTE WITH 2,"Saldos"," Saldos Iniciales "
 ENDIF   
ENDIF

 SELECT SalN
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

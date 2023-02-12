* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ Clasific.PRG  12/06/96                              L: 0386  ³	
* ³ Cat logo de Clasificador de Gastos                           ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾
*- Abriendo Archivos
SET PROC TO TES_LIB
*- Mensajes de aviso al usuario
Vmens01 = ' Partidas : RECEPCION '
Vmens02 = 'Revisi¢n de Partidas'
Vmens04 = 'Dicha Partida no fue encontrada'
Vmens05 = 'No existe Partida anterior'
Vmens06 = 'No existe Partida siguiente'
Vmens07 = '¨ Desea ELIMINAR ‚sta Partida ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta Partida ha sido anulada'
Vmens10 = 'La Partida ya fue atendida'

USE clase   IN 1   order tag Clase1  ALIAS Clas
USE Cuentas IN 7   ORDER TAG Cuentas1 ALIAS cuenta
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
 @  2, 2 SAY "Partida Gen‚rica  :"
 @  4, 2 SAY "Partida Espec¡fica:"
 @  6, 2 SAY "       Descripci¢n:"
 @  8, 2 SAY "Asiento Autom tico:"

 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Clas
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR

 @  2,22 SAY m.codpart    PICTURE 'XX'
 @  4,22 SAY m.codsubp    picture 'XX'
 @  6,22 SAY m.despar
 @  7,22 SAY SPAC(60)
 @  8,22 SAY m.cuentaD    PICTURE "9999999999"
 @  8,36 SAY val_fun('Cuenta','Cuenta',"DescRi",m.cuentaD)
 RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE Clas
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
 codpart  :H='Generica' ,;
 codsubp  :H='Espec¡fica' ,;
 despar   :H='Descripci¢n',;
 CuentaD  :H='Asiento Autom tico'
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
 SELECT Clas

 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF

 Vtemp = RECNO()

 STORE SPACE(2) TO vGen,vEsp

 activate window standby
 @ 1,03 SAY "   Partida Gen‚rica :" GET vGen PICT '!!'
 @ 1,27 SAY "."
 @ 1,28 GET vEsp PICT '!!'
 READ
 deactivate window standby

 IF EMPTY(vGen) .OR. LASTKEY()=27
    GOTO Vtemp
 ELSE
    SEEK vGen+vEsp
    IF !FOUND()
       DO standby WITH "Dicho partida no est  registrada"
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
 SELECT Clas
 SCATTER MEMVAR
 IF RLOCK() OR F_LOCK(1)

    @  2,22 get m.codpart    PICTURE 'XX'
    @  4,22 get m.codsubp    picture 'XX'
    @  6,22 get m.despar
	@  8,22 GET m.cuentaD PICTURE "9999999999" VALID val_fun('Cuenta','Cuenta',"Cuenta+' '+DescRi",m.cuentaD,1)
    READ VALID Val_Read()
    IF LASTKEY() # 27          && Si se presion¢ Esc, no graba
       SELECT Clas
       GATHER MEMVAR
    ENDIF
    DO Vista                    && Muestra nuevos datos
 ENDIF
 UNLOCK
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT Clas
 DO Pantalla
 SCATTER MEMVAR BLANK
    @  2,22 get m.codpart    PICTURE 'XX'
    @  4,22 get m.codsubp    picture 'XX'
    @  6,22 get m.despar   &&PICT '@30'
	@  8,22 GET m.cuentaD PICTURE "9999999999" VALID val_fun('Cuenta','Cuenta',"Cuenta+' '+DescRi",m.cuentaD,1)
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
 SELECT Clas
 DO Vista
 RETURN


PROCEDURE Elimi
*--------------
 SELECT Clas
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ELIMINAR FISICAMENTE ‚sta partida ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
   DELETE NEXT 1
   GO BOTTOM
   DO Vista
 ENDIF
 UNLOCK
 RETURN

PROCEDURE Lista
*--------------
select Clas
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
    DO CLAS
  *  do reporte with 2,"CLASIF",' Listado de Clasificador por Objeto del Gasto '
Endif

SELE Clas
GO vtemp
do Vista
RETURN

PROCEDURE CLAS
*---------------
vTemo = RECNO()
DEFINE WINDOW LIS FROM 5,15 TO 19,65 FLOAT DOUBLE TITLE 'Listado de las Partidas' color SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro
vCli = SPAC(4)
@ 01,01 SAY "Todas las Partidas : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22)
@ 03,01 SAY "           Partida : "
@ 03,22 GET vCli    WHEN VTOCLI=2  PICTURE '!!!!' VALID ValClas()

@ 05,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ PartGen'

@ 08,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todas'

@ 12,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON Codpart+CodSubp TO (vInd) ;
      FOR IIF(vToCli=1,.T.,CodPart = vCli) .AND. IIF(vTipPro=1,.T.,'  ') &&iif(vTipPro=2,Estado = '00',Estado = '50' ))
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,' en General ','  ')  &&IIF(vTipPro=2,' Pendientes ',' Atendidos '))
   IF !EOF()
     DO REPORTE WITH 2,"CLASIF",' Listado de Clasificador por Objeto del Gasto '
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   CLOSE INDEX
   ERASE (VIND)
ENDIF

RETURN

PROCEDURE ValClas
*--------------
SELECT Clas
 vTem=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
* SET RELATION TO PERIODO+NUMOC INTO ITEOC
* SET SKIP TO ITEOC
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 codpart  :H='Generica' ,;
 codsubp  :H='Espec¡fica' ,;
 despar   :H='Descripci¢n' 
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 VCLI=CodPart
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION TO
 GOTO Vtemp
 RETURN .t.


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
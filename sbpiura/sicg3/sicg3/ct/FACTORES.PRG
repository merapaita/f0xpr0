**--------------------------------------------------
** Factores.PRG
** Mantenimiento Tabla de factores de ajuste
** Autor : Javier Fr¡as Gomez
** Fecha : 13/02/96
**--------------------------------------------------
PARAMETER flag
IF parameters()=0
   flag='No'
ENDIF   

PUBLIC vdescrip,mvalor,Valias,Per_Ini,DeCierre
Decierre=PROPER(flag)

*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08,Vmens09,Vmens10,Vmens11
Vmens01 = IIF(decierre='No','Factores para el ajuste mensual','Factores de cierre de ejercicio')
Vmens02 = 'Revisi¢n de '+IIF(decierre='No','Factores para el ajuste mensual','Factores de cierre de ejercicio')
Vmens03 = "Ya existe dicho Factor de Ajuste; Proceda a corregir datos."
Vmens04 = 'Dicho Factor de Ajuste no fue encontrado.'
Vmens05 = 'No existe Factor de Ajuste anterior.'
Vmens06 = 'No existe Factor de Ajuste siguiente.'
Vmens07 = '¨Est  seguro que desea ELIMINAR este Factor de Ajuste?'
Vmens08 = 'No hay registros para procesar'

Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"

USE Factores IN 1 ORDER TAG Factores1 ALIAS Factores
USE Parmae   IN 2 ORDER TAG Parmae1   ALIAS Parma

SELECT Factores
IF Decierre='Si'
   SET ORDER TO 3
ENDIF   
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
*-----------------
DO Inicia             && Define ventanas, men£s, t¡tulos
DO Pantalla           && Muestra pantalla inicial
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

 @  2, 2 SAY " Tipo                 :"
 @  4, 2 SAY " Per¡odo              :"
 @  6, 2 SAY " Mes                  :" 
 @  8, 2 SAY " Factor de Ajuste     :"
 @ 10, 2 SAY " Observaciones        :"
RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT Factores
    SCATTER MEMVAR
    @  2,28 SAY FTIPO(m.Tipo)
    @  4,28 SAY m.Periodo   PICTURE '!!!!'
    @  6,28 SAY m.mes       PICTURE '!!'
    @  8,28 SAY m.factor    PICTURE '99.99999'
    @ 10,28 SAY m.observa   PICTURE '@!'
    @ 8,28  SAY SPACE(IIF(m.factor=0,8,0))
    IF EMPTY(m.mes)
       @ 6,28 SAY SPACE(28)
    ELSE
       @ 6,28 SAY val_para(m.mes,'FECMES','D',28,20)
    ENDIF
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
		xTipo=FTIPO(tipo)                            :H=' Tipo   ' :8,;
		periodo                                      :H='Per¡odo '  ,;
		xMes=retmes(mes)                             :H='  Mes   ' :10, ;
		factor                                       :H='Factor de Ajuste',;
		observa                                      :H='Observaciones'
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
SCATTER MEMVAR BLANK
@ 00,02 SAY "Tipo                 : "
@ 01,02 SAY "Per¡odo              : "
@ 02,02 SAY "Mes                  : "
m.tipo="Mensual "
m.periodo = RIGHT(ALLTRIM(STR(YEAR(DATE()))),2)
@ 00,26 GET m.Tipo    FUNCTION "@M Mensual ,Anual   ,Promedio" 
@ 01,26 GET m.Periodo PICTURE "!!!!" DEFAULT "95"
@ 02,26 GET m.Mes     PICTURE "!!" VALID val_para(m.mes,'FECMES',' ',28,20) ;
        WHEN ALLTRIM(m.Tipo)="Mensual"

READ
DEACTIVATE WINDOW standby
IF LASTKEY()=27
    GOTO Vtemp
    DO VISTA
    RETURN
ENDIF
SEEK  ALLTRIM(m.Periodo) + LEFT(m.Tipo,1) + ALLTRIM(m.Mes)
IF EOF()
    DO standby WITH Vmens04
    GO vTemp
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
	DO PANTALLA
	SCATTER MEMVAR
	m.tipo=FTIPO(m.Tipo)
	@  2,28 SAY m.Tipo FUNCTION "@M Mensual ,Anual  ,Promedio" 
	@  4,28 SAY m.Periodo   PICTURE '!!!!'
	IF ALLTRIM(m.Tipo)="Mensual"
		@  6,28 SAY val_para(m.mes,'FECMES','D',28,20)
	 ELSE
		@  6,28 SAY SPACE(20)
	ENDIF
	
	@  8,28 GET m.factor    PICTURE '99.99999'
	@ 10,28 GET m.observa   PICTURE '@!'
	READ
	IF LASTKEY() = 27          && Si se presion¢ Esc, no graba
		DO StandBy WITH "Proceso Cancelado: No se modifica el registro"
	 ELSE
		GATHER MEMVAR
	ENDIF
	DO Vista                    && Muestra nuevos datos
ENDIF
UNLOCK
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*----------------
*- Aqu¡ coloca valores por omisi¢n (default)
DO PANTALLA
ACTIVATE WINDOW Wind_0
SCATTER MEMVAR BLANK
m.Tipo    = "Mensual "
m.periodo = STR(YEAR(DATE()),4)
@  2,28 GET m.Tipo FUNCTION "@M Mensual ,Anual  ,Promedio" 
@  4,28 GET m.Periodo   PICTURE '!!!!' 
@  6,28 GET m.mes       PICTURE '!!' VALID val_para(m.mes,'FECMES',' ',28,20) ;
        WHEN m.Tipo="Mensual"
@  8,28 GET m.factor    PICTURE '99.99999'
@ 10,28 GET m.observa   PICTURE '@!'
READ

Reg = RECNO() 
SELECT FACTORES
IF LASTKEY() = 27 
	DO standby WITH "Proceso Cancelado. No se graba ning£n cambio."
 ELSE
	SEEK ALLTRIM(m.Periodo) + LEFT(FTIPO(m.Tipo),1) + ALLTRIM(m.mes)
	IF FOUND()
		DO standby WITH Vmens03
		DO Vista
		DO Corri
	 ELSE
		IF LASTKEY() <> 27 .AND. yesno("Confirme el Ingreso")
			IF F_APPD()
				m.cierre=decierre
				GATHER MEMVAR
				UNLOCK
			ENDIF
		 ELSE
			GOTO Reg
		ENDIF
	ENDIF
ENDIF
SCATTER MEMVAR
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
       SELECT FACTORES
       IF !BOF()
          SKIP -1
       ENDIF
    ENDIF
    UNLOCK
 ENDIF
 DO Vista
 RETURN


PROCEDURE XLista
*--------------
 IF EOF()
    DO STANDBY WITH vMens08
    RETURN
 ENDIF    
 Reg    = RECNO()
 SELECT FACTORES 
 DO REPORTE WITH 2,"FACTORES"," °° FACTORES DE AJUSTE  °° "
 GO Reg
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


FUNCTION RetMes
*--------------
PARAMETERS xmes
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,VAL(xMes)*9-8,9)) 


FUNCTION fTipo
*-------------
PARAMETER xT
vRet=""
DO CASE
   CASE xT="A"
         vRet="Anual"
   CASE xT="M"
         vRet="Mensual"
   CAse xT="P"
         vRet="Promedio"
ENDCASE
RETURN PADR(vRet,8," ")


FUNCTION rFactor
*---------------
PARAMETER vPer,vTip,vMes
IF PARAMETERS()=2
   SEEK vPer+vTip
ELSE   
   SEEK vPer+vTip+vMes
ENDIF  
vRet = 0.00 
IF FOUND()
   vRet= factores.factor 
ENDIF
RETURN vRet


PROCEDURE Lista
*---------------
 IF EOF()
    DO STANDBY WITH vMens08
    RETURN
 ENDIF    
 Reg    = RECNO()
Per_Fin = STR( YEAR( DATE() ) , 4 )
Per_Ini = STR( VAL(Per_Fin)-4 , 4 )
DEFINE WINDOW LIS FROM 08,20 TO 15,60 FLOAT DOUBLE TITLE ' °°° Lista Factores de A.C.M. °°° ' COLOR SCHEME 5
ACTIVATE WINDOW LIS
@ 01,07 SAY "Per¡odo Desde : " GET Per_Ini PICTURE "9999" WHEN .F.
@ 02,07 SAY "        Hasta : " GET Per_Fin PICTURE "9999" VALID ACT_ini(Per_Fin)
@ 04,05 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,5
READ CYCLE
RELEASE WINDOWS LIS
IF LASTKEY()<>27 .AND. OKCANCEL=1
   DO REPORTE WITH 2,"FACT_AJU"," °° FACTORES DE AJUSTE  °° "
ENDIF   
GO Reg
DO Vista
RETURN


FUNCTION Act_Ini
*---------------
PARAMETER Fin
Per_ini = STR( VAL(Fin)-4 , 4 )
SHOW GET Per_Ini
RETURN .T.

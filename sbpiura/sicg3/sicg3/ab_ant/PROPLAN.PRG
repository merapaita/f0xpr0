**----------------------------------------------------------------------------------
** PROPLAN.PRG  Administra el Plan Anual de Adquisiciones
** Programa: Funcionamiento
** 07-06-99
** SISTEMA DEL CTAR PIURA 
** PIURA
** Autor: GUSWALEN
**---------------------------------------------------------------------------------------------
USE  planadq  IN 1   ORDER TAG plan1        ALIAS Plan
USE Parmae   IN 2   ORDER TAG Parmae1  ALIAS Parma
*USE  promae   IN 2   ORDER TAG promae1        ALIAS Promae
*USE  prvaux   IN 3   ORDER TAG prvaux1        ALIAS aux
*USE  artmae   IN 4   ORDER TAG artmae1        ALIAS produ
*USE  auxil    IN 5   ORDER TAG auxil1         ALIAS auxi
*USE  ordcom   IN 6   ORDER TAG ordcom4        ALIAS orden
*USE  ordser   IN 7   ORDER TAG ordser6        ALIAS ordser


select plan
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK

*-Define Programa
 IF escolor
    DEFINE POPUP INICIO FROM 15,50  SHADOW COLOR &L_COL
 ELSE
    DEFINE POPUP INICIO FROM 15,50  COLOR SCHEME C_POPUP
 ENDIF
 DEFINE BAR 1 OF INICIO PROMPT '\<Inversi¢n  '
 DEFINE BAR 2 OF INICIO PROMPT '\<Funcionamiento '
 ON SELECTION POPUP INICIO DEACTIVATE POPUP
 ACTIVATE POPUP INICIO
 DO CASE
     CASE BAR()=1
          VTIPSS = 'I'
     CASE BAR()=2
         VTIPSS = 'F'
 ENDCASE     
RELEASE POPUP INICIO


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

DO Termina

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo


 DEFINE WINDOW wind_cte1 FROM 04,10 TO 18,65 DOUBLE ;
 TITLE ' INGRESE MONTOS MENSUALES' COLOR SCHEME 1 

 DEFINE WINDOW wind_cte0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE ' PLAN ANUAL DE ADQUISICIONES ' COLOR SCHEME 10 

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina  '    AT 24,54
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
 ACTIVATE WINDOW wind_cte0

 @  2, 10 SAY "C¢digo"

 @  6, 10 SAY "    Enero : "
 @  8, 10 SAY "  Febrero : "
 @ 10, 10 SAY "    Marzo : "
 @ 12, 10 SAY "    Abril : "
 @ 14, 10 SAY "     Mayo : "
 @ 16, 10 SAY "    Junio : "
 
 @  6, 40 SAY "     Julio : "
 @  8, 40 SAY "    Agosto : "
 @ 10, 40 SAY " Setiembre : "
 @ 12, 40 SAY "   Octubre : "
 @ 14, 40 SAY " Noviembre : "
 @ 16, 40 SAY " Diciembre : "

RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW wind_cte0

 SELECT PLAN
SET ORDER TO 1
 IF EOF()
    DO Pantalla
    RETURN
 ENDIF

 SCATTER MEMVAR

 @  2,22 SAY m.CODADQ 

 @  2,32 SAY val_para(m.CodAdq,'CODADQ','A',32,40,7)

 @  6,22 SAY m.ene PICTURE '999,999.99'
 @  8,22 SAY m.feb PICTURE '999,999.99'
 @ 10,22 SAY m.Mar PICTURE '999,999.99'
 @ 12,22 SAY m.abr PICTURE '999,999.99'
 @ 14,22 SAY m.may PICTURE '999,999.99'
 @ 16,22 SAY m.jun PICTURE '999,999.99'
 
 @  6,52 SAY m.jul PICTURE '999,999.99'
 @  8,52 SAY m.ago PICTURE '999,999.99'
 @ 10,52 SAY m.Set PICTURE '999,999.99'
 @ 12,52 SAY m.oct PICTURE '999,999.99'
 @ 14,52 SAY m.nov PICTURE '999,999.99'
 @ 16,52 SAY m.dic PICTURE '999,999.99'
 
 RETURN


PROCEDURE Revis
*--------------
SELE PLAN
set orde to 1 

 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
   RETURN
 ENDIF

 vTemp = RECNO()

 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo

 ON KEY LABEL F10  KEYBOARD CHR(23)

 BROWSE WINDOW wind_cte0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
 CODADQ  :H='C¢difo' ,;
 Codadq  :H='Descripci¢n'
* rl_nom  :H='Representante Legal'

 ON KEY LABEL F10

 SET ORDER TO 1
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 IF LASTKEY()=27
   GOTO vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F3 
 DO Vista
 RETURN
 


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF

SELECT PLAN
SET ORDER TO 2

vtemp    = RECNO()
vNum_pec = '  '
ACTIVATE WINDOW standby
@ 1,05 SAY 'Digite Mes a Ubicar: ' &&GET M.Periodo PICTURE '!!'
@ 1,30 SAY '-' GET vNum_Pec PICTURE '!!' VALID vBusca()
READ

DEACTIVATE WINDOW standby
SEEK vNum_pec
   IF !FOUND()
	  DO standby WITH " Registro  No  Ubicado "	
      GOTO Vtemp
   ELSE
      DO Vista
   ENDIF

RETURN


PROCEDURE vBusca
*---------------
vnum_pec=padl(alltrim(vnum_pec),2,'0')
retur .t.


PROCEDURE Anter
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 SELECT PLAN
 IF !BOF()
   SKIP -1
 ENDIF
 IF BOF()
   GO TOP
   DO standby WITH "Inicio de archivo: no existe Registro anterior"
 ELSE
   DO Vista
 ENDIF
RETURN



PROCEDURE Proxi
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 SELECT PLAN
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH "Fin de archivo: no existe Registro Siguiente"
   GO BOTTOM
 ELSE
   DO Vista
 ENDIF
RETURN



PROCEDURE Corri
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF

 SCATTER MEMVAR
 vtem= recno() 
 ACTIVATE WINDOW wind_cte0
 DO PANTALLA

 @  2,20 GET m.Codadq
 @  2,50 SAY val_para(m.CodAdq,'CODADQ','A',22,40)
* @  2,50 SAY val_para(m.CodAdq,'CODADQ','A',22,50)
 @  6,22 GET m.Ene PICTURE '999,999.99'
 @  8,22 GET m.Feb PICTURE '999,999.99'
 @ 10,22 GET m.Mar PICTURE '999,999.99'
 @ 12,22 GET m.Abr PICTURE '999,999.99'
 @ 14,22 GET m.May PICTURE '999,999.99'
 @ 16,22 GET m.Jun PICTURE '999,999.99'
 
 @  6,52 GET m.Jul PICTURE '999,999.99'
 @  8,52 GET m.Ago PICTURE '999,999.99'
 @ 10,52 GET m.Set PICTURE '999,999.99'
 @ 12,52 GET m.Oct PICTURE '999,999.99'
 @ 14,52 GET m.Nov PICTURE '999,999.99'
 @ 16,52 GET m.Dic PICTURE '999,999.99' 

    
 READ VALID Val_Read()
       IF LASTKEY() # 27
          IF YESNO('¨ Conforme la modificaci¢n?')
			 GATHER MEMVAR
          ENDIF
       ELSE
       ENDIF

 UNLOCK
 SELECT Plan
 DO Vista                    && Muestra nuevos datos
 RETURN



PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 
 DO Pantalla
 SELECT PLAN
 SCATTER MEMVAR BLANK




 @  2,22 GET m.CodADQ	PICTURE '!!!!!!' VALID Val_PARA( m.CodADQ,'CODADQ',' ',22,40,7) &&AND VALATTE()
 
 @  6,22 GET m.Ene PICTURE '999,999.99'
 @  8,22 GET m.Feb PICTURE '999,999.99'
 @ 10,22 GET m.Mar PICTURE '999,999.99'
 @ 12,22 GET m.Abr PICTURE '999,999.99'
 @ 14,22 GET m.May PICTURE '999,999.99'
 @ 16,22 GET m.Jun PICTURE '999,999.99'
  
 @  6,52 GET m.Jul PICTURE '999,999.99'
 @  8,52 GET m.Ago PICTURE '999,999.99'
 @ 10,52 GET m.Set PICTURE '999,999.99'
 @ 12,52 GET m.Oct PICTURE '999,999.99'
 @ 14,52 GET m.Nov PICTURE '999,999.99'
 @ 16,52 GET m.Dic PICTURE '999,999.99'



 READ VALID Val_Read()
 
IF LASTKEY()# 27
   IF YESNO('¨ Confirme el ingreso ?')
    	APPEND BLANK
    	GATHER MEMVAR 
		REPLACE tippla WITH vtipss
		IF YESNO('¨ Ingresa Otro C¢digo ?')
		   do ingre
		endif
   ENDIF
ELSE
   DO STANDBY WITH 'Proceso cancelado'
   GO BOTTOM
ENDIF

SELECT PLAN
SET ORDER TO 1
DO VISTA
RETURN

PROCEDURE Elimi
*--------------

SELECT PLAN
IF EOF()
  DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
  RETURN
ENDIF

*PRIVATE OK
    velimina = YESNO('¨ Desea Eliminar ‚ste Registro ?')
    IF vElimina 
       SELECT Plan

       IF RLOCK()
          delete next 1
       ENDIF
       IF EOF()
          SKIP -1
       ENDIF   
    ENDIF    
DO VISTA
UNLOCK ALL
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN

PROCEDURE Termina
*----------------
  CLOSE DATA
  ON KEY LABEL F2
  RELEASE    WINDOW wind_cte0
  RELEASE    WINDOW wind_cte1
  ON KEY LABEL F12
  ON KEY LABEL F11
  RELEASE    MENU   mMenu
  ACTIVATE   SCREEN
  RESTORE    SCREEN FROM Principal
  RETURN

PROCEDURE Lista
*--------------
SELECT Plan
IF EOF()
   RETURN
ENDIF
VTEMP = RECNO()
go top
DO LisPla
SELECT plan
GO VTEMP
RETURN



PROCEDURE LisPLA
*---------------
IF escolor
   DEFINE POPUP Lismenu FROM 17,54 MARGIN SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP Lismenu FROM 17,54 MARGIN COLOR SCHEME C_POPUP
ENDIF
public ZZ
DEFINE BAR 1 OF Lismenu PROMPT ' \<Plan'
DEFINE BAR 2 OF Lismenu PROMPT ' \<Ejecuci¢n '


ON SELECTION POPUP Lismenu DEACTIVATE POPUP

ACTIVATE POPUP Lismenu
vTemp=recn()
DO CASE
   CASE BAR()=1
	     do reporte with 2,"Lisplaf",' Plan Anual ',1,.F.,.T.
   CASE BAR()=2
	     do reporte with 2,"Lisplac",' Plan Anual ',1,.F.,.T.
ENDCASE
RELEASE POPUP LisMenu
RETURN


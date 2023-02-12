**----------------------------------------------------------------------------------
** GENPLA.PRG  Administra las planillas
** 03/1999
** SISTEMA SBPP
** PIURA
** A.S. SEGUNDO CORREA SANDOVAL
**---------------------------------------------------------------------------------------------
USE  parmae   IN 1   ORDER TAG parmae1        ALIAS Parma
USE  personal IN 2   ORDER TAG persona1       ALIAS Permae
USE  iteper   IN 3   ORDER TAG iteper1        ALIAS Iteper
USE  maeniv   IN 4   ORDER TAG maeniv1        ALIAS Nivel
USE  iteniv   IN 5   ORDER TAG iteniv1        ALIAS Iteniv


SELECT PERMAE
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial

*- Activa men£ mientras vEn_accion es .T.
DO Termina

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' SOCIEDAD DE BENEFICENCIA DE PIURA '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW wind_1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE ' Generaci¢n de Planillas de la S.B.P.' COLOR SCHEME 10 ;
 FOOTER ''

 DEFINE WINDOW wind_2   FROM 10,16 TO 16,60 DOUBLE ;
 TITLE ' Procesando Planilla ' COLOR SCHEME 8 

 DEFINE WINDOW wind_3   FROM 18,12 TO 21,64 DOUBLE ;
 TITLE ' Procesando Planilla ' COLOR SCHEME 8 
 
 ACTIVATE WINDOWS wind_1
 
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW WIND_2
 store space(2) to vmes
 store space(4) to va¤o
 
 @  1, 2  SAY "  Mes : "
 @  2, 2  SAY "  A¤o : "
 
 @  1, 15 GET vmes PICTURE '99' VALID Val_para(vmes,'FECMES',' ',15,15)
 @  2, 15 GET vA¤o PICTURE '9999'

 READ VALID Val_Read()
 IF LASTKEY() # 27
    Do Genera
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 DEACTIVATE WINDOW WIND_2
 RETURN

PROCEDURE Genera
*---------------
 ACTIVATE WINDOW wind_3
 SELE PARMA
 SEEK 'FECSIS'
 IF LEFT(DESCRI,2) # ALLT(vMes) AND SUBSTR(DESCRI,4,4) # ALLT(vA¤o)
 	DEACTIVATE WINDOW wind_3
 	DO STANDBY WITH 'La Fecha de la planilla es diferente al mes del Sistema'
 	RETURN
 ENDIF
 SELECT Iteper
 SET FILTER TO Mes_Sue = ALLT(VMES) AND Periodo=vA¤o
 GO TOP
 IF !EOF()
 	IF YesNo("Planilla ya Generada. ¨Desea Reprocesarla?")
 		DELE FOR Mes_Sue = ALLT(VMES) AND Periodo=vA¤o
 	 ELSE
 		RETURN .T.
 	ENDIF
 ENDIF
 SET FILTER TO
 @  0, 6  SAY " Esperar un momento genrando Planilla del mes "+vmes COLOR SCHEME 15 

 IF allt(vmes)#'01'
 		xmes = val(vmes)-1
 	 	m.mes_sue = PADL(ALLT(STR(XMES)),2,'00')
 	 	m.periodo = SUBSTR(PARMA.DESCRI,4,4)
 else
 		m.mes_sue = '12'
 		m.periodo = PADL(VAL(SUBSTR(PARMA.DESCRI,4,4))-1,4,'0')
 endif
 SET FILTER TO mes_sue=m.mes_sue and periodo = m.periodo
 COPY TO plames
 SET FILTER TO
 USE plames in 0
 sele plames
 replace all mes_sue with allt(vmes)
 replace all periodo with allt(va¤o)
 SELECT iteper
 locate for mes_sue=allt(vmes)
 IF FOUND()
 	IF yesno("Se realiz¢ anteriormente la planilla ¨Deseas sobreescribirla?")
 		DELETE FOR mes_sue = allt(vmes)
 	ENDIF
 ENDIF
 APPEND FROM plames
 DEACTIVATE WINDOW wind_3
 DO STANDBY WITH 'Termin¢ el Proceso de Generaci¢n de Planillas'
 RETURN


PROCEDURE Termina
*----------------
  CLOSE DATA
  RELEASE    MENU   mMenu
  ACTIVATE   SCREEN
  RESTORE    SCREEN FROM Principal
  RETURN


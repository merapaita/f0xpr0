**----------------------------------------------------------------------------------
** PERMAE.PRG  Administra la lista de Personal
** 03/1999
** SISTEMA SBPP
** PIURA
** A.S. SEGUNDO CORREA SANDOVAL
**---------------------------------------------------------------------------------------------
USE  parmae   IN 1   ORDER TAG parmae1        ALIAS Parma
USE  personal IN 2   ORDER TAG persona1       ALIAS Permae

SELECT PERMAE
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK

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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW wind_cte0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE ' CATALOGO DEL PERSONAL DE LA S.B.P.' COLOR SCHEME 10 ;
 FOOTER ''

 DEFINE WINDOW wind_cte1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE ' Revisi¢n del Personal de la S.B.P' COLOR SCHEME 10

 DEFINE WINDOW wind_2P   FROM 12,10 TO 22,69 DOUBLE ;
 TITLE ' Detalle : Clasificaci¢n ' COLOR SCHEME 10 ;
 FOOTER '± ®Esc¯ Sale ±'

 DEFINE WINDOW wind_2Pa  FROM 14,04 TO 22,75 DOUBLE ;
 TITLE ' Clasificaci¢n del Proveedor ' COLOR SCHEME 10 ;

 DEFINE WINDOW wind_2Pb  FROM 07,02 TO 18,78 DOUBLE ;
 TITLE ' Detalle : Ordenes Compra ' COLOR SCHEME 10 ;
 FOOTER '± ®Esc¯ Sale ±'

 DEFINE WINDOW wind_2Pc  FROM 07,02 TO 18,78 DOUBLE ;
 TITLE ' Detalle : Ordenes Servicio ' COLOR SCHEME 10 ;
 FOOTER '± ®Esc¯ Sale ±'

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
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

 @  1, 2 SAY "  Lib. Electoral : "
 @  2, 2 SAY "Apell. y Nombres : "
 @  3, 2 SAY " Nro Cta. Ahorro : "
 @  4, 2 SAY "Fecha de Ingreso : "
 @  4,40 SAY "Sanc. / Lic.-SGH : "
 @  5, 2 SAY " Tiempo de Serv. : "
 @  6, 2 SAY "       Direcci¢n : "
 @  7, 2 SAY "        Tel‚fono : "
 @  8, 2 SAY "    Estado Civil : "
 @  9, 2 SAY " N£mero de Plaza : "
 @ 10, 2 SAY "       Condici¢n : "
 @ 11, 2 SAY "         Recurso : "
 @ 12, 2 SAY "           Nivel : "
 @ 13, 2 SAY "           Cargo : "
 @ 14, 2 SAY "  R‚g. Pensiones : "
 @ 16, 2 SAY " Estado de PLaza : "
 @ 18, 2 SAY "Mes - Vacaciones : "
 @ 20, 2 SAY "CADENA FUNCIONAL : "
 @ 21, 2 SAY "     Observaci¢n : "
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW wind_cte0
 SELECT Permae
 IF EOF()
    DO Pantalla
    RETURN
 ENDIF
 SCATTER MEMVAR
 @  1, 22 SAY m.Lib_ele
 @  2, 22 SAY m.Ape_Nom
 @  3, 22 SAY m.Nro_Cta
 @  4, 22 SAY m.Fec_ing
 @  4, 60 SAY m.dia_sus PICTURE '9999'
 @  5, 22 SAY m.A¤o_ser
 @  6, 22 SAY m.Dir_Per
 @  7, 22 SAY m.Tel_Per
 @  8, 22 SAY Val_para(m.Est_Civ,'ESTCIV','D',22)
 @  9, 22 SAY m.num_pla
 @ 10, 22 SAY Val_para(m.con_Per,'CONPER','D',22)
 @ 11, 22 SAY Val_para(m.rec_Per,'RECPER','D',22)
 @ 12, 22 SAY Val_para(m.niv_Per,'NIVPER','D',22)
 @ 13, 22 SAY Val_para(m.car_Per,'CARPER','D',22)
 @ 14, 22 SAY Val_para(m.afp_Per,'AFPPER','D',22)
 @ 16, 22 SAY Val_para(m.estado,'ESTPLA','D',22)
 @ 18, 22 SAY Val_para(m.mes_vac,'FECMES','D',22)
 @ 20, 22 SAY m.CODCAD
 @ 21, 22 SAY m.obs_per
 RETURN

PROCEDURE Revis
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
   RETURN
 ENDIF
 vTemp = RECNO()
 IF escolor
    DEFINE POPUP Busmenu FROM 15,50  SHADOW COLOR &L_COL
 ELSE
    DEFINE POPUP Busmenu FROM 15,50  COLOR SCHEME C_POPUP
 ENDIF
 DEFINE BAR 1 OF Busmenu PROMPT ' ordenado por:  \<L.E.    '
 DEFINE BAR 2 OF Busmenu PROMPT ' ordenado por:  \<N§ Cta. '
 DEFINE BAR 3 OF Busmenu PROMPT ' ordenado por:  \<Apellidos. '
 ON SELECTION POPUP Busmenu DEACTIVATE POPUP
 ACTIVATE POPUP Busmenu
 DO CASE
     CASE BAR()=1
          SET ORDER TO Persona1
     CASE BAR()=2
          SET ORDER TO Persona2
     CASE BAR()=3
          SET ORDER TO Persona3
 ENDCASE     
 IF LASTKEY()=27
	SET ORDER TO Persona2
    DO VISTA
    RETURN
 ENDIF     
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo

 ON KEY LABEL F10  KEYBOARD CHR(23)

 BROWSE WINDOW wind_cte1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
 	Lib_Ele :H='L.E.' ,;
 	Ape_Nom :H='Apellidos y Nombres',;
 	Nro_Cta :H='Cta. Aho.',;
 	Obs_Per :H='Observaciones.'

 ON KEY LABEL F10

 SET ORDER TO 2
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
 IF escolor
    DEFINE POPUP Busmenu FROM 15,54  SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP Busmenu FROM 15,54  COLOR SCHEME C_POPUP
  ENDIF
  
 DEFINE BAR 1 OF Busmenu PROMPT ' ordenado por:  \<L.E.    '
 DEFINE BAR 2 OF Busmenu PROMPT ' ordenado por:  \<N§ Cta. '
 DEFINE BAR 3 OF Busmenu PROMPT ' ordenado por:  \<Apelidos. '
  ON SELECTION POPUP Busmenu DEACTIVATE POPUP

  ACTIVATE POPUP Busmenu
  SELECT permae
  Vtemp = RECNO()
  DO CASE
     CASE BAR()=1
       ACTIVATE WINDOW standby
       STORE SPACE(8) TO Vbusca
       @ 1,2 SAY 'C¢digo: ' GET Vbusca PICT '99999999' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          SET ORDER TO Persona1
          vBusca = UPPER(ALLTRIM(vbusca))
       ENDIF
     CASE BAR() = 2
       ACTIVATE WINDOW standby
       STORE SPACE(10) TO Vbusca
       @ 1,1 SAY ' N§ Cta.: ' GET Vbusca PICTURE '9999999999' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          SET ORDER TO Persona2
          vBusca = TRIM(UPPER(vBusca))
       ENDIF
     CASE BAR() = 3
       ACTIVATE WINDOW standby
       STORE SPACE(30) TO Vbusca
       @ 1,1 SAY ' Nombre: ' GET Vbusca PICTURE '@!' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          SET ORDER TO Persona3
          vBusca = TRIM(UPPER(vBusca))
       ENDIF
     OTHERWISE
       RETURN
 ENDCASE

 IF EMPTY( Vbusca ) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK Vbusca
   IF .NOT. FOUND()
     DO standby WITH "Dicho Proveedor no fue encontrado"
     GOTO Vtemp
   ELSE
     DO Vista
   ENDIF
 ENDIF
 SET ORDER TO Persona2
 DO VISTA
 RETURN

PROCEDURE Anter
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 SELECT PERmae
 IF !BOF()
   SKIP -1
 ENDIF
 IF BOF()
   GO TOP
   DO standby WITH "Inicio de archivo: no existe Contratante anterior"
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
 SELECT Permae
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH "Fin de archivo: no existe Contratante siguiente"
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
 @  1, 22 SAY m.Lib_ele  
 @  2, 22 GET m.Ape_Nom  PICTURE '@S50'
 @  3, 22 SAY m.Nro_Cta  
 @  4, 22 GET m.Fec_ing  
 @  4, 60 get m.dia_sus  PICTURE '9999' valid tieser(m.Fec_ing,m.dia_sus)
 @  5, 22 say m.a¤o_ser  PICTURE '99'
 @  6, 22 GET m.Dir_Per  PICTURE '@S50'
 @  7, 22 GET m.Tel_Per  PICTURE '999999'
 @  8, 22 GET m.Est_Civ  PICTURE '!'  VALID Val_para(m.Est_Civ,'ESTCIV',' ',22)
 @  9, 22 GET m.num_pla  PICTURE '99'
 @ 10, 22 GET m.con_Per  PICTURE '!'  VALID Val_para(m.Con_Per,'CONPER',' ',22)
 @ 11, 22 GET m.rec_Per  PICTURE '!'  VALID Val_para(m.Rec_Per,'RECPER',' ',22)
 @ 12, 22 GET m.niv_Per  PICTURE '!!' VALID Val_para(m.niv_Per,'NIVPER',' ',22)
 @ 13, 22 GET m.car_Per  PICTURE '!!' VALID Val_para(m.car_Per,'CARPER',' ',22)
 @ 14, 22 GET m.afp_Per  PICTURE '!!' VALID Val_para(m.afp_Per,'AFPPER',' ',22)
 @ 16, 22 GET m.estado   PICTURE '!!' VALID Val_para(m.estado,'ESTPLA',' ',22)
 @ 18, 22 GET m.mes_vac  PICTURE '!!' VALID Val_para(m.mes_vac,'FECMES',' ',22)
 @ 20, 22 GET m.codcad  
 @ 21, 22 GET m.obs_per
 READ VALID Val_Read()
 
 IF LASTKEY() <> 27
    select permae
    GATHER MEMVAR
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    GO BOTTOM
 ENDIF
 UNLOCK
 SELECT Permae
 DO Vista                    && Muestra nuevos datos
 RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
*OK=VE_PASSW('MRA')    
*IF !OK
*   RETURN
*ENDIF   
 SELECT Permae
 SCATTER MEMVAR BLANK
 DO Pantalla
 STORE '00'  TO m.Estado
 m.fec_ing=date()
 m.Nro_Cta = '463163    '
 @  1, 22 GET m.Lib_ele  PICTURE '99999999'   Valid Val_busper (m.Lib_Ele,1)
 @  2, 22 GET m.Ape_Nom  PICTURE '@S50'
 @  3, 22 GET m.Nro_Cta  PICTURE '9999999999' Valid Val_busper (m.Nro_Cta,2)
 @  4, 22 GET m.Fec_ing  
 @  4, 60 get m.dia_sus  PICTURE '9999' valid tieser(m.Fec_ing,m.dia_sus)
 @  5, 22 say m.a¤o_ser  PICTURE '99'
 @  6, 22 GET m.Dir_Per  PICTURE '@S50'
 @  7, 22 GET m.Tel_Per  PICTURE '999999'
 @  8, 22 GET m.Est_Civ  PICTURE '!'  VALID Val_para(m.Est_Civ,'ESTCIV',' ',22)
 @  9, 22 GET m.num_pla  PICTURE '99'
 @ 10, 22 GET m.con_Per  PICTURE '!'  VALID Val_para(m.Con_Per,'CONPER',' ',22)
 @ 11, 22 GET m.rec_Per  PICTURE '!'  VALID Val_para(m.Rec_Per,'RECPER',' ',22)
 @ 12, 22 GET m.niv_Per  PICTURE '!!' VALID Val_para(m.niv_Per,'NIVPER',' ',22)
 @ 13, 22 GET m.car_Per  PICTURE '!!' VALID Val_para(m.car_Per,'CARPER',' ',22)
 @ 14, 22 GET m.afp_Per  PICTURE '!!' VALID Val_para(m.afp_Per,'AFPPER',' ',22)
 @ 16, 22 GET m.estado   PICTURE '!!' VALID Val_para(m.estado,'ESTPLA',' ',22)
 @ 18, 22 GET m.mes_vac  PICTURE '!!' VALID Val_para(m.mes_vac,'FECMES',' ',22)
 @ 20, 22 GET m.codcad  
 @ 21, 22 GET m.obs_per
 READ VALID Val_Read()
 IF LASTKEY() <> 27
    SELECT PERMAE
    IF F_appd()
		GATHER MEMVAR
    endif    
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    GO BOTTOM
 ENDIF
 SELECT PERMAE
 DO VISTA
 RETURN

PROCEDURE Elimi
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
   RETURN
 ENDIF
 DELETE NEXT 1
 UNLOCK
 DO Vista
 RETURN

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN

PROCEDURE Termina
*----------------
  CLOSE DATA
  RELEASE    MENU   mMenu
  ACTIVATE   SCREEN
  RESTORE    SCREEN FROM Principal
  RETURN

PROCEDURE Lista
*--------------
SELECT PERMAE
IF EOF()
   RETURN
ENDIF
VTEMP = RECNO()
go top
DO LisPER
SELECT pERmae
GO VTEMP
RETURN

PROCEDURE LisPER
*---------------
IF EOF()
   DO STANDBY WITH vMens08
   RETURN
ENDIF
vTemP = RECNO()

DEFINE WINDOW LIS FROM 2,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Proveedores' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1        TO vToCli,vOrden,vTipPro,vEstado,vTipLis
VCODPER = SPAC(8)

@ 01,01 SAY "   Total Personal : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22)
@ 03,01 SAY "         Personal : "
@ 03,22 GET VCODPER    WHEN VTOCLI=2  PICTURE '99999999' VALID Val_busper(vcodper,1)

@ 05,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ L.E.;Cta. Ahor.'  WHEN VTOCLI =  1

@ 08,01 SAY "           Estado : " GET vEstado  FUNCTION '^ Todos;Activos-Pensi¢n;Anulados' WHEN VTOCLI =  1

@ 17,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
  	IF vOrden=2
		SET ORDER TO persona2
 	else
 		SET ORDER TO persona1
  	endif
   IF VTOCLI = 1
	  DO CASE
	  	CASE vEstado=1
	  		SET FILTER TO ALLT(NRO_CTA)#'     ' AND ALLT(NRO_CTA)#'0000000000' AND !EMPTY(LIB_ELE) AND !EMPTY(APE_NOM)
      	CASE vEstado=2
      		SET FILTER TO estado#'02' and estado#'04'
        CASE vEstado=3
      		SET FILTER TO estado='99'
      ENDCASE                 
   ELSE 
   	  SET FILTER TO Lib_ele=vcodper
   ENDIF
   DEACTIVATE WINDOW STANDBY
   IF !EOF()
		do reporte  with 2, 'LISPER', ' Lista del Personal',1,.F.,.T.
   ELSE
      DO STANDBY WITH 'Archivo Vac¡o ...'
   ENDIF
   SET FILTER TO
ENDIF
SELECT PERMAE
SET ORDE TO persona2
GO VTEMP
RETURN

Function tieser
*--------------
* Actualiza el Tiempo de Servicio
* de Todos los Trabajadores
PARAMETER vfecing,vdiasus
vfecact=date()
*Calcula los d¡as
vtotdia=(vfecact-fec_ing)-dia_sus
*Total a¤os
vtotano=INT(vtotdia/365)
vresto =MOD(vtotdia,365)
*Total meses
vtotmes=INT(vresto/30)
vtotdia1=MOD(vresto,30)
m.a¤o_ser = STR(vtotano,2)+'A, '+STR(vtotmes,2)+'M, '+STR(vtotDia1,2)+'D.'
@ 5, 22 CLEAR TO 5,40
@ 5, 22 SAY m.A¤o_ser
RETURN .T.


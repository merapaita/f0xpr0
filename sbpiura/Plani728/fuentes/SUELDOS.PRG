*--------------------------------------------------------------------------
* Sueldos.Prg
* Registra los sueldos de los trabajadores
* Estado :
*   '00' Emitida   Este es el que se registra en la Solicitud de Cotizaci¢n
*   '20' Afectada
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor:
*--------------------------------------------------------------------------
*- Abriendo Archivos
PARAMETER sistema
USE Personal IN 1   order tag persona2  ALIAS Permae
USE Iteper   IN 2   order tag Iteper1   ALIAS Iteper
USE parmae   IN 3   order tag PARMAE1   alias parma
USE totapor  IN 4   order tag totapor1  alias totapor

PUBLIC vMes_Sue,vMes,vAumento,vDescuento,vEmp_01,vEmp_02, v26,vdescuentp,vSubsidios

*- Mensajes de aviso al usuario
*PRIVATE vTotOC
*xtotoc = 0
Vmens01 = ' Items de Sueldos'
Vmens02 = ' Registro de Sueldos de Trabajadores'
Vmens04 = 'Dicha persona no fue encontrado'
Vmens05 = 'No existe personal anterior'
Vmens06 = 'No existe personal siguiente'
Vmens08 = 'No hay registros para procesar'

PUBLIC GH, vmes_sue,vperiodo
SELE PARMA
SEEK 'FECSIS'
SELECT ITEPER
vMes_sue = LEFT(Parma.Descri,2)
vPeriodo = SUBS(parma.descri,4,4)
SET FILTER TO Mes_Sue = vMes_Sue AND Periodo = vPeriodo
SELECT Permae
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
HIDE POPUP ALL
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa           Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

* DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
* TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 13,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 14,00 TO 23,79 DOUBLE ;
 TITLE 'Sueldos del A¤o ' COLOR SCHEME 10

 DEFINE WINDOW wind_cte1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE ' Revisi¢n del Personal de la S.B.P' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 05,50 TO 14,78 ;
 TITLE 'TOTALES ' COLOR SCHEME 10
 
 DEFINE WINDOW Wind_4 FROM 20,48 TO 22,78 ;
 COLOR SCHEME 10

 DEFINE WINDOW Wind_5 FROM 16,01 TO 18,79 ;
 TITLE ' Destino '

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
*DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 *ON SELECTION PAD anula  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_1
CLEAR
 @  1, 2 SAY "  Lib. Electoral : "
 @  2, 2 SAY "Apell. y Nombres : "
 @  3, 2 SAY " Nro Cta. Ahorro : "
 
 @  5, 2 SAY "   Fecha Ingreso : "
 @  6, 2 SAY "       Direcci¢n : "
 @  7, 2 SAY "        Tel‚fono : "
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ON KEY LABEL F5
 ON KEY LABEL F8
 SELECT PerMae
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW WIND_1

 ON KEY LABEL F9 DO VISTA_DET

 SCATTER MEMVAR

* @  0,02 SAY IIF(m.Tipdoc='OK' AND m.estado#'5','O/C Visado','             ') 
* @  0,60 SAY vEstOc(m.Estado) COLOR SCHEME 02
 @  1, 22 SAY m.Lib_ele
 @  2, 22 SAY m.Ape_Nom
 @  3, 22 SAY m.Nro_Cta
 
 @  5, 22 SAY m.Fec_ing
 @  6, 22 SAY m.Dir_Per
 @  7, 22 SAY m.Tel_Per

 DO VISTA_HIJO
 DO TOTAL
 RETURN

PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
SELE totapor
SEEK m.lib_ele+vPeriodo+vmes_sue
@  0,0 SAY 'Ingresos :'
@  1,0 SAY 'S / A    :'
@  2,0 SAY 'Subsidios:'
@  3,0 SAY 'Egresos  :'
@ 4,14 SAY '----------'
@ 5,0 SAY 'Sueldo  :'
@ 6,0 SAY 'C.E.M.  :'
@ 7,0 SAY 'I.E.S.  :'

@  0,14 SAY tot_ing picture '9,999,999.99'
@  1,14 SAY tot_SA  picture '9,999,999.99'
@  2,14 SAY tot_Sub picture '9,999,999.99'
@  3,14 SAY tot_des picture '9,999,999.99'
@ 4,14 SAY '----------'
@ 5,14 SAY tot_ing+Tot_Sub+Tot_SA-tot_des picture '9,999,999.99'
@ 6,14 SAY apo_01 picture '9,999,999.99'
@ 7,14 SAY apo_02 picture '9,999,999.99'
SELE permae
RETURN

PROCEDURE VISTA_HIJO
*-------------------
hide popup all
SELECT iteper
SEEK m.lib_ele
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.lib_ele  TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   Mes_Sue     : H= 'Mes',;
   Tip_adn : H='+/-':P="@M '+,-'" :R,;
   cod_adn : H='Aum/Desc':R,;
   xx=Val_para(cod_adn,IIF(Tip_adn='+','AUMLEY','DESLEY'),'Z',15,15):H='Descripci¢n',;
   tot_adn : H='Valor':R,;
   porcent : H='Porcentaje':R
   
SELE permae
RETURN


PROCEDURE VISTA_DET
*------------------
hide popup all
SELECT iteper
SEEK m.lib_ele
GO TOP
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.lib_ele  ;
   WINDOW Wind_2 ;
   FIELDS;
   Estado=IIF(estado='00','Pendiente','Enviado  '),;
   Mes_Sue     : H= 'Mes',;
   Imp_Sue     : H= 'Importe' :P='99,999.99'
SELE permae
DO VISTA
ON KEY LABEL F9 DO VISTA_DET
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
ACTIVATE SCREE
IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
   RETURN
 ENDIF
 vTemp = RECNO()
 DEFINE POPUP Busmenu FROM 15,50  SHADOW COLOR &L_COL
 DEFINE BAR 1 OF Busmenu PROMPT ' ordenado por:  \<L.E.    '
 DEFINE BAR 2 OF Busmenu PROMPT ' ordenado por:  \<N§ Cta. '
 DEFINE BAR 3 OF Busmenu PROMPT ' ordenado por:  \<Apellido'
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
    SET ORDE TO Persona2
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
 	Nro_Cta :H='Cta. Aho.'

 ON KEY LABEL F10

 SET ORDER TO Persona2
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa           Listar  Termina '
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
ACTIVATE SCREE
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 DEFINE POPUP Busmenu FROM 15,54  SHADOW COLOR &L_COL
 DEFINE BAR 1 OF Busmenu PROMPT ' busca por:  \<L.E.    '
 DEFINE BAR 2 OF Busmenu PROMPT ' busca por:  \<N§ Cta. '
 DEFINE BAR 3 OF Busmenu PROMPT ' busca por:  \<Apellido'
 ON SELECTION POPUP Busmenu DEACTIVATE POPUP

  ACTIVATE POPUP Busmenu
  SELECT permae
  Vtemp = RECNO()
  DO CASE
     CASE BAR()=1
       ACTIVATE WINDOW standby
       STORE space(8) TO Vbusca
       @ 1,2 SAY 'C¢digo: ' GET Vbusca PICT '99999999' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          vBusca = UPPER(ALLTRIM(vbusca))
          SET ORDER TO persona1
       ENDIF
     CASE BAR() = 2
       ACTIVATE WINDOW standby
       STORE SPACE(10) TO Vbusca
       vbusca='463163    '
       @ 1,1 SAY ' N§ Cuenta: ' GET Vbusca PICTURE '9999999999' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          vBusca = TRIM(UPPER(vBusca))
          SET ORDER TO persona2
       ENDIF
     CASE BAR() = 3
       ACTIVATE WINDOW standby
       STORE SPACE(50) TO Vbusca
       @ 1,1 SAY ' Nombre: ' GET Vbusca PICTURE '@S30' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          vBusca = TRIM(UPPER(vBusca))
          SET ORDER TO persona3
       ENDIF
     OTHERWISE
       RETURN
 ENDCASE

 IF EMPTY( Vbusca ) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK Vbusca
   IF .NOT. FOUND()
     DO standby WITH "Dicha Persona no fue encontrado"
     GOTO Vtemp
   ELSE
     DO Vista
   ENDIF
 ENDIF
 SET ORDER TO persona2
 DO VISTA
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
 PRIVATE AD
 ON KEY LABEL F12
 ON KEY LABEL F4
 OQ=.T.
 IF Proc_Cerrado(vMes_Sue,vPeriodo)				&& Verifica el Estado de Proceso de las Planilla
	do standby with 'EL MES YA ESTA CERRADO NO PUEDE VOLVER A MODIFICAR'
	RETURN
 ENDIF
 AD = RECNO()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 DO CASE  
 	CASE Estado = '99'
 		 * Anulada
         DO STANDBY WITH Vmens09
         RETURN
 ENDCASE

 SELECT PerMae
 SCATTER MEMVAR

 ACTIVATE WINDOW Wind_1
 Or = TRABAJA_HJ()
 SELECT iteper
 SEEK m.lib_ele 
 vtotal = 0
 SCAN WHILE lib_ele = m.lib_ele
	vtotal = vtotal + Imp_sue
 ENDSCAN     
 m.Tot_Sue= vtotal
 SELECT permae
 REPLACE Tot_Sue WITH m.Tot_Sue
 DO VISTA
 RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ON KEY LABEL F12
 ON KEY LABEL F4
 IF Proc_Cerrado(vMes_Sue,vPeriodo)				&& Verifica el Estado de Proceso de las Planilla
	DO Standby WITH 'EL MES YA ESTA CERRADO NO PUEDE VOLVER A MODIFICAR'
	RETURN
 ENDIF
 DEACTIVATE WINDOW WIND_3
 SELECT permae
 REC=RECNO()
 AST=ORDER()
 Ok = TRABAJA_HJ()
 SELECT iteper
 SEEK m.Lib_Ele
 vTotal = 0
 SCAN WHILE Lib_Ele = m.Lib_Ele
	vtotal = vtotal + Imp_sue
 ENDSCAN
 m.Tot_Sue= vtotal
 SELECT PerMae
 REPLACE Tot_Sue WITH m.Tot_Sue
 DO VISTA
 RETURN


*PROCEDURE siprv
*--------------
parameter vfun
return
vfun = .t.
if !v_reg='VG'
   do standby with 'El Proveedor no esta Regularizado...Observaci¢n'
endif
return vfun


PROCEDURE TRABAJA_HJ
*-------------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO Agreg_ITEM
ON KEY LABEL F8  DO Elimi_ITEM
ON KEY LABEL F10 KEYBOARD CHR(23)
SELE iteper
SEEK m.lib_ele
IF !FOUND()
	DO Agreg_ITEM
ENDIF
BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2 key m.lib_ele  FIELD ;
	Mes_sue     : H= 'Mes ',;
	Niv_Per		: H= 'Nivel'   :V=Val_para(Niv_Per,'NIVPER',"Niv_Per",15,15),;
	JJ=Val_para(Niv_Per,'NIVPER','Z',15,15):H='Desc.',;
	Tip_adn : H='+/-',;
	cod_adn : H='Aum/Desc':V=Val_para(cod_adn,IIF(Tip_adn='+','AUMLEY','DESLEY'),"Cod_Adn") AND valores(),;
	xx=Val_para(cod_adn,IIF(Tip_adn='+','AUMLEY','DESLEY'),'Z',15,15):H='Descripci¢n',;
	tot_adn : H='Valor',;
	porcent : H='Porcentaje':R,;
	suJ_des : H='Suj. a Desc'
	
	
STORE 0 TO vaumento,vaumentop,vsuj_des,vsuj_des1,vsuj_desp,vdescuento,vdescuentp,V26,vSubsidios,v32,vSinAfec
SELE iteper
SEEK m.lib_ele

nAguin = 0

IF m.Lib_ele='02675895'
*	SET STEP ON
ENDIF

nAguin = 0

SCAN WHILE m.lib_ele=lib_ele
	
	IF Tip_adn='+'
		IF cod_adn = '02'		&&Aguinaldo
			nAguin = nAguin + tot_adn
		 ELSE
			nAguin = nAguin + 0
		ENDIF
		
		IF porcent='N'
			IF cod_adn <> '26' AND cod_adn <> '28' AND cod_adn <> '72' AND cod_adn <> '30'     && AND cod_adn <> '32'
				vaumento   = vaumento   + tot_adn
			 ELSE
			 	*IF cod_adn <>'32'
			 	*	V26 = V26 + tot_adn
			 	* ELSE
			 	*	V32 = V32 + tot_adn   && 32 No lo Coje por Estar ya Afectado
			 	*ENDIF
			ENDIF
			IF cod_adn = '30' OR cod_adn = '31' OR cod_adn = '64' OR cod_adn = '72' 		&&cod_adn = '38' OR  OR cod_adn = '79' OR cod_adn = '80' OR cod_adn = '32'
				vSubsidios = vSubsidios + tot_adn
			ENDIF
			IF cod_adn = '65' OR cod_adn = '79' OR cod_adn = '80' OR cod_adn = '83' OR cod_adn = '91' OR cod_adn = '94' OR cod_adn = '95'
				vSinAfec = vSinAfec + tot_adn
			ENDIF
			IF cod_adn = '06' OR cod_adn = '85' OR cod_adn = '93' OR cod_adn = '43' OR cod_adn = '44'
				vsuj_des = vsuj_des + tot_adn
			ENDIF
			IF suj_des = 'N' AND !(cod_adn = '02' OR cod_adn = '06' OR cod_adn = '85' OR cod_adn = '93' OR cod_adn = '43' OR cod_adn = '44' OR cod_adn = '65')
				vsuj_des1 = vsuj_des1 + tot_adn
			ENDIF
		 ELSE
			IF suj_des = 'N'
				vsuj_desp = vsuj_desp + tot_adn
			ENDIF
			vaumentop = vaumentoP + tot_adn
			IF cod_adn = '11'
				vAumento = vAumento + 93  && 10% RMV
			else
				vAumento = vAumento + vAumento*vaumentop/100
			ENDIF
			vAumentoP = 0
		ENDIF
	 ELSE 
		IF porcent='N'
			vdescuento = vdescuento + tot_adn
		 ELSE
			vdescuentp   = vdescuentp + tot_adn
*SET STEP ON
			vdescuento = vdescuento + ROUND((vaumento+vSubsidios+V26+v32-nAguin-vsuj_des-vsuj_des1-vSinAfec)*vdescuentp/100,2)
*			vdescuento = vdescuento + ROUND((vaumento+V26+v32-vsuj_des-vsuj_des1-vSinAfec-vSubsidios)*vdescuentp/100,2)
			vdescuentp   = 0
		ENDIF
	ENDIF
ENDSCAN

*vaumento   = vaumento + ROUND((vaumento*vaumentop),2)/100
*vdescuen   = ROUND((((vaumento+V26)-vsuj_des)*vdescuentp),2)/100
*vdescuento = vdescuen + vdescuento
STORE 0 TO vEmpl01, vEmpl02
SELECT PARMA
seek 'APORTA'
IF FOUND()
	SCAN WHILE tipo = 'APORTA'
		IF allt(codigo) = '01'
			vEmpl01 = vEmpl01 + numdec			&& PORCENTAJES
		 ELSE
			vEmpl02 = vEmpl02 + numdec			&& PORCENTAJES
		ENDIF
	ENDSCAN
	
	* SUBSIDIOS ESSALUD
	IF (vSubsidios>0)
		XX = ((vaumento+vSubsidios+V26+v32-nAguin)-vsuj_des-vsuj_des1-vSinAfec)
		vEmpl01 = xx*vEmpl01/100
		vEmpl02 = (((vaumento+vSubsidios+V26+V32-nAguin)-vsuj_des-vSubsidios-vSinAfec)*vEmpl02)/100
	ELSE
		uit = 4300
		mref = uit * 0.30
		XX = ((vaumento+vSubsidios+V26+v32-nAguin)-vsuj_des-vsuj_des1-vSinAfec)
		vEmpl01 = IIF(xx>mref AND m.Con_per='6',mref*vEmpl01/100,IIF(xx > 930.00,xx*vEmpl01/100,IIF(xx > 0.00,930.00*vEmpl01/100,0)))
		vEmpl02 = (((vaumento+V26+V32-nAguin)-vsuj_des-vSubsidios-vSinAfec)*vEmpl02)/100
	ENDIF
*           vaumento+vSubsidios+V26+v32       -vsuj_des-vsuj_des1-vSinAfec

*   30 % UIT = 1215    
*	vEmpl01 = IIF(xx>1215 AND m.Con_per='6',1215*vEmpl01/100,IIF(xx > 750.00,xx*vEmpl01/100,IIF(xx > 0.00,750.00*vEmpl01/100,0)))
*	vEmpl01 = IIF(vSubsidios>0,xx*vEmpl01/100,IIF(xx>1155 AND m.Con_per='6',1155*vEmpl01/100,IIF(xx > 750.00,xx*vEmpl01/100,IIF(xx > 0.00,750.00*vEmpl01/100,0))))
*	vEmpl01 = IIF(vSubsidios>0,xx*vEmpl01/100,IIF(xx > 750.00,xx*vEmpl01/100,IIF(xx > 0.00,750.00*vEmpl01/100,0)))
*	vEmpl01 = IIF(xx > 600.00,xx*vEmpl01/100,IIF(xx > 0.00,600.00*vEmpl01/100,0))
*	vEmpl01 = xx*vEmpl01/100
*	vEmpl01 = (((vaumento+V26+v32)-vsuj_des-vSubsidios-vSinAfec)*vEmpl01)/100
*	vEmpl01 = (((vaumento+V26+v32)-vsuj_des)*vEmpl01)/100
*	vEmpl02 = (((vaumento+V26+V32-nAguin)-vsuj_des-vSubsidios-vSinAfec)*vEmpl02)/100
*	vEmpl02 = (((vaumento+V26+V32-nAguin)-vsuj_des)*vEmpl02)/100
ENDIF
UNLOCK ALL
do mtotal
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
SELECT permae
RETURN

Procedure mTotal
*---------------
SELECT totapor
SEEK m.lib_ele+vPeriodo+vmes_sue
m.mes_sue = Vmes_sue
IF !FOUND()
	APPEND BLANK
	GATHER MEMVAR
ENDIF
REPLACE tot_ing WITH vaumento+v26		&&-vSubsidios-vSinAfec
REPLACE tot_Sub WITH vSubsidios
REPLACE tot_SA  WITH vSinAfec
REPLACE tot_des WITH vdescuento
REPLACE Periodo WITH PADL(YEAR(DATE()),4)
IF (PerMae.Con_Per="3".OR. PerMae.Con_Per="4")
	REPLACE apo_01  WITH 0 
	REPLACE apo_02  WITH 0
 ELSE
	REPLACE apo_01  WITH vEmpl01 
	REPLACE apo_02  WITH vEmpl02
ENDIF

RETURN

PROCEDURE Elimi_ITEM
*-------------------
PRIVATE VFUN
VFUN=.F.
SELEC iteper
delete next 1
VFUN=.T.
RETURN VFUN

PROCEDURE Agreg_Item
*-------------------
PRIVATE VFUN
VFUN=.F.
SELEC iteper
APPEND BLANK
REPLACE  mes_sue WITH vmes_sue
REPLACE  periodo WITH vperiodo
REPLACE  lib_ele WITH m.lib_ele
REPLACE  estado  WITH PerMae.Estado
*REPLACE  Num_Pla WITH PerMae.Num_Pla
REPLACE  Niv_Per  WITH Permae.Niv_Per
VFUN=.T.
RETURN VFUN


PROCEDURE Lista
*--------------
SELECT permae
SET RELATION TO lib_ele INTO iteper
IF EOF()
   RETURN
ENDIF
VTEMP = RECNO()
GO TOP
DO LisPer
SELECT PerMae
GO vTemp
RETURN

PROCEDURE LisPer
*---------------
IF EOF()
   DO STANDBY WITH vMens08
   RETURN
ENDIF
vTemP = RECNO()

DEFINE WINDOW LIS FROM 2,15 TO 23,65 FLOAT DOUBLE TITLE ' Listado de Personal ' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1 TO vToCli,vOrden,vTipPro,vEstado,vTipLis
VCODPER = SPAC(8)
Vmes    = SPAC(2)
vdato   = space(20)
SELECT PerMae
SET ORDE TO Persona2
@ 01,01 SAY "   Total Personal : " GET vToCli  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
*@ 03,01 SAY "         Personal : "
*@ 03,22 GET vCodPer    WHEN VToCli=2  PICTURE '99999999' VALID Val_busper(vcodper,1)
@ 04,01 SAY "              Mes : "
@ 04,22 GET Vmes  Valid Val_para(vmes,'FECMES',' ',22)

@ 06,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ L.E.;Cta. Ahor.;Ape_Nom'  WHEN VTOCLI =  1

@ 09,01 SAY "           Estado : " GET vEstado  FUNCTION '^ Todos;Pendientes;Anulados' WHEN VTOCLI =  1
@ 13,01 SAY "           T¡tulo : " GET vDato    

@ 17,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   SET RELATION TO LIB_ELE+allt(vmes) INTO TOTAPOR
   SET FILTER TO ALLT(NRO_CTA)#'     ' AND ALLT(NRO_CTA)#'0000000000' AND !EMPTY(LIB_ELE) AND !EMPTY(APE_NOM) AND LIB_ELE#'00000' AND REC_PER#'2' AND MES_VAC#ALLT(VMES)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   IF !EOF()
		do reporte  with 2, 'LISSUE', ' Lista del Personal',2,.F.,.T.
   ELSE
      DO STANDBY WITH 'Archivo Vac¡o ...'
   ENDIF
   SET FILTER TO
ENDIF
SELECT PERMAE
SET ORDE TO persona2
GO VTEMP
RETURN

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F2
  ON KEY LABEL F4
  ON KEY LABEL F9 
  HIDE WINDOW WIND_1
  DEACTIVATE MENU
  RETURN

FUNCTION valores
*---------------
IF parma.numdec>0
	REPLACE tot_adn with parma.numdec, porcent with 'S'
ELSE
	REPLACE tot_adn with parma.nument, porcent with 'N'
ENDIF
RETURN .T.

PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
*  RELEASE WINDOW wind_0
  RELEASE WINDOW wind_1
  RELEASE WINDOW wind_3
  RELEASE WINDOW wind_4
  RELEASE WINDOW wind_cte1
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN
  
*FUNCTION AgrDetalle
*------------------

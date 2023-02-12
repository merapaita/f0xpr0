*--------------------------------------------------------------------------
* RegSol.Prg
* Registra  Solicitud de Servicios que se emiten en cada dependencia
* Estado del Solicitud de Servicio:
*   '00' Emitida     ** Este es el que se registra en el 
*   '20' Con Correlativo
*   '50' Con O/S
*   '70' Devuelta
*   '99' Anulada
*--------------------------------------------------------------------------

*- Abriendo Archivos
parameter sistema
CLOS DATA

USE Parmae   IN 1   ORDER TAG Parmae1  ALIAS Parma
USE Solser   IN 2   ORDER TAG Solser1  ALIAS Solser
USE calen    IN 3   ORDER TAG calen1   ALIAS calen
IF sistema='1'
	USE maepre   IN 4  order tag maepre1  ALIAS maepre
ELSE
	USE maepre   IN 4  order tag maepre3  ALIAS maepre
ENDIF	
USE itepar   in 5   ORDER TAG itepar1  ALIAS ITEPAR          
USE OrdSer   IN 7   ORDER TAG OrdSer1  ALIAS Ordse
USE HojCon   IN 8   ORDER TAG HojCon1  ALIAS Hoja
USE Cheque   IN 9   ORDER TAG Cheque1  ALIAS Cheque
USE Compag   IN 10  ORDER TAG Compag1  ALIAS compag
USE Promae   IN 11  ORDER TAG Promae1  ALIAS Promae
USE Usuario  IN 12  ORDER TAG Usuario1 ALIAS USU

USE cdrnec   IN 13   ORDER TAG Cdrnec1  ALIAS cuadro

*- Mensajes de aviso al usuario
Vmens01 = ' Solicitud de Servicios : REGISTRO '
Vmens02 = 'Registro de Solicitud de Servicio'
Vmens04 = 'Dicho Solicitud de Servicio no fue encontrado'
Vmens05 = 'No existe Solicitud de Servicio anterior'
Vmens06 = 'No existe Solicitud de Servicio siguiente'
Vmens07 = '¨ Desea ELIMINAR ‚ste Solicitud de Servicio ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta Solicitud de Servicio ha sido anulada'
Vmens10 = 'El Solicitud de Servicio ya fue atendido'
Vmens11 = 'El Solicitud de Servicio ha sido devuelto'
Vmens12 = 'El Solicitud de Servicio ya tiene O/S'

PRIVATE VRUC,VDIREC
VNUMDEP=0
PUBLIC FOND
SET MEMOWIDTH TO 56

PUBLIC con,conx,cony,conz,GH,v_reg
SELECT Solser
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
on key label f2 do VisObs
on key label f4 do imprimIR
on key label f5 do asigprv

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista
HIDE POPUP ALL

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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular           Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Solicitud de Servicio' ;
 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD anula  OF mMenu DO anula
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @  1, 2 SAY "            Fecha :"
 @  1,40 SAY " N£mero Solicitud :"
 @  2, 2 SAY "               De :"
 @  3, 2 SAY "      Dependencia :"

 @  4, 2 SAY "      Cadena Fun. :"
 @  5, 2 SAY " Fte. Funcionami. :"

 @  6, 2 SAY "          Funci¢n :"
 @  7, 2 SAY "         Programa :"
 @  8, 2 SAY "      Subprograma :"
 @  9, 2 SAY "   Activ./Proyec. :"

 @ 10, 2 SAY "          Destino :"
 @ 11, 2 SAY "    Tipo Servicio :"

 @ 12, 2 SAY "        Ubicaci¢n :"
 @ 13, 2 SAY "          Usuario :"
 @ 14, 2 SAY "         Defectos :"
 @ 15, 2 SAY "            Causa :"
 @ 17, 2 SAY "      Descripci¢n :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Solser
IF EOF()
	DO Pantalla
	RETURN
ENDIF

ACTIVATE WINDOW Wind_0
ON KEY LABEL F7 DO Estado WITH 'SS','Solser.Periodo+Solser.Numos+Solser.Codfte'

SCATTER MEMVAR
Do Pantalla
=val_codcad(ALLT(m.codcad),m.periodo,'C')
IF USer_tp$'E' 
*	@  0,02 SAY IIF(m.TipSs='S','Solicitud Sede  ','Solicitud Bayovar ') COLOR SCHEME 02
	@  1,22 SAY m.FecSS
	@  1,60 SAY m.periodo
	@  1,63 SAY m.NumSs
	@  2,22 CLEAR TO 20,78
	DO STANDBY with 'Solicitud Elaborando por '+user
	IF !BOF()
		SKIP -1
		DO VISTA
	ENDIF   
 ELSE
	@  0,00 SAY IIF(M.USER_TP='I',IIF(EMPTY(m.User),space(50),'     Elaborado por: ')+user,IIF(EMPTY(m.User),space(50),'     Corregido por: ')+user)
*	@  0,02 SAY IIF(m.TipSs='S','Solicitud Sede  ','Solicitud Bayovar ') COLOR SCHEME 02
	@  0,60 SAY IIF( m.Estado= '00','Pendiente   ',IIF( m.Estado = '20','C/Sol.Cotiz.',IIF(m.Estado='99','Anulada     ',IIF(m.Estado='50','Atendido    ','Con O/S:'+NumOs))))
	@  1,22 SAY m.FecSS
	@  1,60 SAY m.periodo
	@  1,63 SAY m.NumSs
	
	@  2,22 SAY m.atte
	@  3,22 SAY val_para(m.CodDep,'CODDEP','D',22,50)
	
	@  4,22 SAY val_CodCad(m.Codcad,m.periodo,'D',22,40) 
	@  5,22 SAY val_para(m.codfte,'CODFTE','D',22,40)
	
	@  6,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
	@  7,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
	@  8,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
	@  9,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)
	
	@ 10,22 SAY m.Destino PICTURE '@S56'
	@ 11,22 SAY VAL_PARA(m.TipSer,'TIPSER','V',22,40)
	@ 12,22 SAY m.Ubicac
	@ 13,22 SAY m.Usuario
	@ 14,22 SAY m.Defect
	@ 15,22 SAY m.Causas
	@ 17,22 CLEAR TO 21,78 
	@ 17,22 SAY MLINE(Detalle,1)
	@ 18,22 SAY MLINE(Detalle,2)
	@ 19,22 SAY MLINE(Detalle,3)
	@ 20,22 SAY MLINE(Detalle,4)
	
	@ 21,00 SAY PADC('° ®F2¯ Detalle S/S ° ®F4¯ Imprime ° ®F5¯ Asigna Prv ° ®F7¯ Seguimiento °',79,' ') COLOR W+/B
ENDIF
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
ON KEY LABEL F7

SELECT Solser
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
BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
NumSs  :H='N§S/S',;
Est = IIF(Estado= '00','Pendte',IIF(Estado = '20','S/Cotz',IIF(Estado='99','Anulad',IIF(Estado='50','Atend.','('+NUMOS+')')))) :H=' O/S ' ,;
Codfte :H='Fte ' ,;
FecSs  :H='Fecha ' ,;
CodDep :H='DEP',;
XX=MLINE(DETALLE,1)  :H='Descripci¢n'
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
IF LASTKEY()=27
  GOTO Vtemp
ENDIF
SHOW MENU mMenu
ON KEY LABEL F10
SELE SOLSER
DO Vista
RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF

ON KEY LABEL F7 

vtemp    = RECNO()
vPeriodo = RIGHT(DTOC(DATE()),2)
vNumSs   = '    '
ACTIVATE WINDOW standby
@ 1,01 SAY 'Ingrese N£mero Solicitud : ' GET vPeriodo PICTURE '!!'
@ 1,30 SAY '-' GET vNumss PICTURE '!!!!' VALID vBusca()
READ

DEACTIVATE WINDOW standby
IF EMPTY(vNumSs) .or. LASTKEY()=27
   ON KEY LABEL F7 DO Estado WITH 'SS','Solser.Periodo+Solser.Numos+Solser.Codfte'
   RETURN
ELSE
   SEEK vPeriodo + vNumSs
   IF !FOUND()
      DO standby WITH Vmens04
      GOTO Vtemp
   ELSE
      DO Vista
   ENDIF
ENDIF
ON KEY LABEL F7 DO Estado WITH 'SS','Solser.Periodo+Solser.Numos+Solser.Codfte'
RETURN


PROCEDURE vBusca
*---------------
vNumss=Padl(alltrim(Vnumss),4,'0')
retur .t.


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
ON KEY LABEL F7 

PUBLIC vMes,vPart

 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 DO CASE  
    CASE Estado = '99'
 		 * Anulada
         DO STANDBY WITH Vmens09
         RETURN
	CASE Estado = '20'
         * La Orden de Servicio tiene S/Cotizaci¢n
         DO STANDBY WITH 'La O/S ya tiene Solicitud de Cotizaci¢n'
         RETURN
	CASE Estado = '50'
         * Ya tiene Orden de Sericio ?
         DO STANDBY WITH 'Ya tiene O/S'
         RETURN
	CASE Estado = '70'
         * La Orden de Servicio ha sido devuelta
         DO STANDBY WITH 'La O/S ha sido devuelta'
         RETURN
 ENDCASE

*IF escolor
*    DEFINE POPUP xcot  FROM 16,65 SHADOW COLOR &L_COL
*ELSE
*    DEFINE POPUP xcot  FROM 16,65 COLOR SCHEME c_popup
*ENDIF

*DEFINE BAR 1 OF xcot PROMPT ' \<Sede     '
*DEFINE BAR 2 OF xcot PROMPT ' \<Bayovar   '
 
*ON SELECTION POPUP xcot  DEACTIVATE POPUP
*ACTIVATE POPUP XCOT

*DO CASE
*    CASE BAR() = 1
*      vTipo = 'S'
*    CASE BAR() = 2
*      vTipo = 'B'
*    OTHERWISE
*ENDCASE
*RELEASE POPUP Xcot

*IF LASTKEY()=27 OR !vtipo$'SB'
*   DO STANDBY WITH 'Proceso cancelado'
*   DO VISTA
*   RETURN
*ENDIF


SELE Solser
SCATTER MEMVAR
Activate window wind_0
DO PANTALLA
=val_CODCAD(ALLT(m.codcad),m.periodo,'C')

*@  0,02 SAY IIF(m.TipSs='S','Solicitud Sede  ','Solicitud Bayovar ') COLOR SCHEME 02
@  1,22 GET m.FecSs     PICTURE '@D'  DISABLE
@  1,60 GET m.Periodo   PICTURE '!!'  DISABLE
@  1,62 SAY '-'
@  1,63 GET m.NumSs     PICTURE '!!!!' DISABLE

@  2,22 GET m.atte
@  3,22 GET m.CodDep    PICTURE '!!!!!!' VALID Val_para( m.CodDep,'CODDEP',' ',22,50)

@  4,22 GET m.CodCad    PICTURE '!!!!' valid val_codcad(m.codcad,m.periodo,' ',22,30)
@  5,22 GET m.CodFte    PICTURE '!!'   VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,40)
READ VALID val_read()
IF LASTKEY()=27 
   DO vista
   RETURN
ENDIF
@  6,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
@  7,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
@  8,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
@  9,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

@ 10,22 GET m.Destino   PICTURE '@S56'
@ 11,22 GET m.TipSer    PICTURE '!!' VALID VAL_PARA(m.TipSer,'TIPSER',' ',22,40)
@ 12,22 GET m.Ubicac    PICTURE '@S56'
@ 13,22 GET m.Usuario   PICTURE '@S56'
@ 14,22 GET m.Defect    PICTURE '@S56'
@ 15,22 GET m.Causas    PICTURE '@S56'
@ 17,22 SAY MLINE(Detalle,1)
@ 18,22 SAY MLINE(Detalle,2)
@ 19,22 SAY MLINE(Detalle,3)
@ 20,22 SAY MLINE(Detalle,4)
READ VALID Val_Read()
IF LASTKEY() # 27 && Si se presiona Esc, no graba
	DO WHILE .T.
	OK  = DETALLE()
    IF LASTKEY() # 27
       IF YESNO('¨ Conforme la correcci¢n ?')
          EXIT
       ENDIF
    ELSE
       IF YESNO('¨ Cancela la correcci¢n ?')
           ok = .F.
           EXIT
       ENDIF
    ENDIF
    ENDDO
   **** verifica siestan correctos
   codcad = m.codcad
   sele Solser
   seek m.periodo+m.numss
   if .T.
   		replace codcad with m.codcad, codfte with m.codfte, coddep with m.codfte
   endif
   if ok .and. lastkey() # 27
      sele solser
      m.user = sys(0)
      m.user_fc = date()
      m.user_TP = 'C'
   	  GATHER MEMVAR
   endif
ELSE
   DO STANDBY WITH 'Proceso cancelado'
ENDIF
SELECT Solser
DO Vista			&& Muestra los nuevos Datos
UNLOCK
RETURN


PROCEDURE Ingre     && Crea nuevo registro en BD
*--------------
PUBLIC AST,REC,VMES,VPART,VTIPO
ON KEY LABEL F7 
*IF escolor
*    DEFINE POPUP xcot  FROM 16,65 SHADOW COLOR &L_COL
*ELSE
*    DEFINE POPUP xcot  FROM 16,65 COLOR SCHEME c_popup
*ENDIF

*DEFINE BAR 1 OF xcot PROMPT ' \<Sede     '
*DEFINE BAR 2 OF xcot PROMPT ' \<Bayovar   '
 
*ON SELECTION POPUP xcot  DEACTIVATE POPUP
*ACTIVATE POPUP XCOT

*DO CASE
*    CASE BAR() = 1
*      vTipo = 'S'
*    CASE BAR() = 2
*      vTipo = 'B'
*    OTHERWISE
*ENDCASE
*RELEASE POPUP Xcot

*IF LASTKEY()=27 OR !vtipo$'SB'
*   DO STANDBY WITH 'Proceso cancelado'
*   DO VISTA
*   RETURN
*ENDIF


DO Pantalla
SELECT Solser
REC = RECNO()

SCATTER MEMVAR BLANK
m.periodo = SUBS(DTOC(m.fecsis),9,2)
m.NumSS   = REPASA()
m.FecSs   = m.fecsis
*m.Tipss  = vTipo

*@  0,02 SAY IIF(m.TipSs='S','Solicitud Sede  ','Solicitud Bayovar ') COLOR SCHEME 02
@  1,22 GET m.FecSs     PICTURE '@D' DISABLE
@  1,60 GET M.Periodo   PICTURE '!!' DISABLE
@  1,62 SAY '-'
@  1,63 GET m.NumSs     PICTURE '!!!!' DISABLE VALID Val_SS()
@  2,22 GET m.atte
@  3,22 GET m.CodDep    PICTURE '!!!!!!' VALID Val_DEP(m.CodDep,'CODDEP',' ',22,40) AND VALATTE()

@  4,22 GET m.Codcad   PICTURE '!!!!' VALID VAL_CODCAD(m.Codcad,m.periodo,' ',22,40)
@  5,22 GET m.CodFte   PICTURE '!!'   VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,40)

READ VALID val_read()
IF LASTKEY()=27
    SELE SOLSER
    GO REC
	DO vista
	RETURN
ENDIF

@  6,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
@  7,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
@  8,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
@  9,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

@ 10,22 GET m.Destino   PICTURE '@S56'
@ 11,22 GET m.TipSer    PICTURE '!!' VALID VAL_PARA(m.TipSer,'TIPSER',' ',22,40)
@ 12,22 GET m.Ubicac    PICTURE '@S56'
@ 13,22 GET m.Usuario   PICTURE '@S56'
@ 14,22 GET m.Defect    PICTURE '@S56'
@ 15,22 GET m.Causas    PICTURE '@S56'

*@ 17,22 SAY MLINE(Detalle,1)
*@ 18,22 SAY MLINE(Detalle,2)
*@ 19,22 SAY MLINE(Detalle,3)
*@ 20,22 SAY MLINE(Detalle,4)

READ VALID Val_Read()
IF LASTKEY() # 27 && verifica si estan correctos
	SELE SOLSER
	SEEK m.periodo + m.numss
	IF !FOUND()
		IF f_Appd()
			m.user = sys(0)  &&aqui
			m.user_fc = date()
			m.user_TP = 'C'
			m.estado  = '00'
			GATHER MEMVAR
			SELE Parma
			SEEK "CORRELSOLSER"
			IF found()
				REPLACE NumEnt WITH NumEnt+1
			ENDIF
		ENDIF
	ENDIF
	SELECT solser
	IF !empty(m.Numss)
		OK = DETALLE()
	ENDIF   
 ELSE
	GO REC
ENDIF
SELECT solser
DO Vista			&& Muestra los nuevos Datos
RETURN


PROCEDURE Val_SS
*---------------
PRIVATE AS
AS=RECNO()
SEEK m.Periodo+m.Numss

IF FOUND()
	DO Standby WITH 'La Solicitud ya esta registrada'
	RETURN .f.
ENDIF
RETURN .T.


PROCEDURE Anula
*--------------
SELECT SOLSER
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
IF Estado = '99'
	* ya pas¢
	DO STANDBY WITH Vmens09
	RETURN
ENDIF
IF Estado # '00'
	* ya pas¢
	DO STANDBY WITH Vmens10
	RETURN
ENDIF
velimina = YESNO('¨ Desea ANULAR ‚ste Solicitud?')
IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
*	DELETE NEXT 1
*	SKIP -1
	REPLACE ESTADO WITH '99' ,FECVER WITH DATE()
	DO Vista
ENDIF
UNLOCK
RETURN


PROCEDURE Lista
*--------------
select SOLSER
vtemp =recno()
IF LASTKEY()=27
   DO VISTA
   RETURN
ENDIF
if eof()
   do standby with vmens08
   return
else
*  Detrte with 2,"LisSs",' Solicitud de Servicio '
   ON KEY LABEL F7
   DO LISSER
endif
SELE SOLSER
GO vtemp
do Vista
RETURN


PROCEDURE lisSER
*---------------
vOrde = ORDER()
DEFINE WINDOW LIS FROM 2,15 TO 21,65 FLOAT DOUBLE TITLE 'Listado Solicitud de Servicios' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1   TO vToCLI,vORDEN,vTipPro,vListado
vCli = SPAC(4)
vAno = str(year(DATE())-1900,2)
@ 01,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ Resumido;Detallado' 

@ 05,01 SAY "        Total S/S : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22) when vlistado =2
@ 07,01 SAY "              S/S : "
@ 07,22 GET vAno    picture '!!'   WHEN VTOCLI=2 and vlistado=2
@ 07,25 SAY '-'
@ 07,26 GET vCli    WHEN VTOCLI=2 and vlistado=2 PICTURE '!!!!' VALID Bussol()

@ 09,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Dependencia;Emision' WHEN VTOCLI=1 

@ 12,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos' WHEN VTOCLI=1 

@ 16,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,Periodo+NumSs,IIF(vOrden=2,CodDep,DTOS(FECss))) TO (vInd) ;
         FOR IIF(vToCli=1,.T.,Periodo+NumSs = vAno+vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',Estado = '50' ))
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,' en General ',IIF(vTipPro=2,' Pendientes ',' Atendidos '))
   IF !EOF()
      if vlistado=2
         do reporte with 2,"LissERV",' Solicitud de Servicio '
      else
         do reporte with 2,"LisSol",' Solicitud de Servicios ',1,.F.,.T.      
      endif
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   CLOSE INDEX
   ERASE (VIND)
ENDIF
SELECT solser
SET ORDE TO (VORDE)
GO TOP
RETURN


PROCEDURE IMPRIMIR
*-----------------
PRIVATE VCON,XX
*XX = val_prv(m.codprv,.T.)
SELECT solser
DO BUSCAPRV
VCON = RECNO()
SCATTER MEMVAR
vNumss = m.Numss
set filt to Numss=vNumss
if eof()
   do standby with vmens08
   return
else
   do reporte with 2,"LissERV",' Solicitud de Servicio '
endif
set filter to
SELECT Solser
GO VCON
DO VISTA
RETURN


PROCEDURE IMPRIM_A
*-----------------
PRIVATE VCON
SELECT solser
VCON = RECNO()
SCATTER MEMVAR
IF m.estado # '30'
   do standby with 'A£n esta sin Orden de Servicio'
   do vista
   return
endif      
vNumss = m.Numss
set filt to Numss=vNumss
if eof()
   do standby with vmens08
   return
else
   do reporte with 2,"Lisss1_A",' Solicitud de Servicio '
endif
set filter to
SELECT Solser
GO VCON
DO VISTA
RETURN


PROCEDURE Termi
*--------------
 vEn_accion = .F.
 DEACTIVATE MENU
 ON KEY LABEL F2
 ON KEY LABEL F4 
 ON KEY LABEL F5  
 ON KEY LABEL F7 
 RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  ON KEY LABEL F7 
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


function valSs
*-----------------
parameter vnumss
private vfun
vfun = .t.
m.numss=padl(alltrim(str(vnumss,4)),4,'0')
if m.numss = '0000' or empty(m.numss)
   vfun = .f.
endif
return vfun


FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,vFun
vFun = .F.

XX = Val_Art(CodArt,.F.)
IF XX
  SELECT Itepec
  REPLACE ;
          coduni  WITH Produ.coduni,;
          preuni  WITH Produ.preuni
  vFun = .T.
ENDIF
RETURN vFun


PROCEDURE BusSol             && Revisi¢n de BD en browse
*--------------

SELECT Solser
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 vCli=Padl(alltrim(vCli),4,'0')
 seek vano+vcli
 if !found()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumSs  :H=' N§ ',;
 Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
 FecSs  :H='Fecha' ,;
 CodDep :H='DEP',;
 Codcad :H='Codigo Cadena' ,;
 DESSS  :H='Descripci¢n'

 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 endif
 vano=periodo
 vcli=numss
 ON KEY LABEL F10
 RETURN


PROCEDURE ASIGPRV
*----------------
vOrde = ORDER()
IF m.ESTADO = '00' OR m.estado # '00'
   IF EMPTY(M.ESTADO)
      m.codprv='0000'
   ENDIF   
DEFINE WINDOW LIS1 FROM 10,15 TO 14,65 FLOAT DOUBLE TITLE 'Asigna Proveedores de Servicios' COLOR SCHEME 5
ACTIVATE WINDOW LIS1
@ 01,01 SAY " C¢digo Proveedor : " GET m.codprv valid IIF(m.codprv='0000',.t.,Val_prv(m.CodPrv,.T.,1,27)  and siprv() and promae.estado="VG")
@ 02,01 SAY "   Total Servicio : " GET m.Valtot PICT '9,999,999.99'
read valid val_read()
if lastkey() # 27
   replace solser.codprv with m.codprv,solser.valtot with m.valtot 
endif 
DEACTIVATE WINDOW LIS1  
ELSE
  DO STANDBY WITH 'No esta pendiente...'
ENDIF
return


PROCEDURE buscprv
*----------------
PRIVATE XC
xc=alias()
VCODPRV = val_prv(m.codprv,.T.)
FOND = VCODPRV
sele (xc)
RETURN ' '


FUNCTION DETALLE
*---------------
vAlias = ALIAS()
SET MEMOWIDTH TO 56
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Detalle")
	DEFINE WINDOW Detalle FROM 03,12 TO 20,67 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Detalle")
	ACTIVATE WINDOW Detalle SAME
 ELSE
	ACTIVATE WINDOW Detalle NOSHOW
ENDIF

MODIFY MEMO DETALLE WINDOW Detalle

IF !WVISIBLE("Detalle")
	ACTIVATE WINDOW Detalle
ENDIF

RELEASE WINDOW Detalle
IF LASTKEY()=27
	DO STANDBY WITH 'Proceso cancelado. No graba el Detalle '
ENDIF
SELECT (vAlias)
RETURN .T.


FUNCTION VisObs
*--------------
ON KEY LABEL F2
vAlias = ALIAS()

IF !WEXIST("Detalle")
   DEFINE WINDOW Detalle FROM 03,12 TO 20,67 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle SAME
ELSE
   ACTIVATE WINDOW Detalle NOSHOW
ENDIF

MODIFY MEMO DETALLE NOEDIT WINDOW Detalle

IF !WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle
ENDIF
RELEASE WINDOW Detalle
ON KEY LABEL F2 do VisObs
RETURN .T.


FUNCTION val_DEP
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong , mdist
PRIVATE malias
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
		mdist = 6
	CASE PARAMETERS() = 5
		mdist = 6
ENDCASE
malias  = ALIAS()

SELECT parma
SEEK filtro+mvalor

IF !FOUND() .AND. !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	SET FILTER TO tipo = filtro AND CODIGO = substr(vcoddep,1,vnumdep)
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		SET FILTER TO
		IF !EMPTY( malias )
			SELECT (malias)
		ENDIF
		RETURN
	ENDIF
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = parma.codigo
mcuenta= parma.descriau2
mdescr = SUBSTR( parma.descri, 1, mlong )
mdescriaux = SUBSTR( parma.descriaux, 1, mlong)
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable=' '   && En edici¢n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable='D'   && En vista
		RETURN mdescr
	CASE mvariable='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE


Function repasa
*--------------
SELE Parma
SEEK "CORREL"+"SOLSER"
mRet = PADL(ALLTRIM(STR(Parma.NumEnt+1)),4,'0')
RETURN mRet

PROCEDURE VALATTE
*----------------
SELECT Cuadro
SEEK  m.Periodo+m.Coddep
if found()
   m.atte = Cuadro.Atte 	
   @  2,22 SAY m.atte
&& m.atte = IIF(EMPTY(m.atte) or m.atte='No se Registra Solicitante..,revise',Cuadro.Atte,m.atte)
else
   m.atte = 'No Existe Solicitante..,Revise'
   @  2,22 SAY m.atte
endif
SELECT SOLSER
return


PROCEDURE BUSCAPRV
*------------------
PRIVATE ALI,VKEY
ALI = ALIAS()
VKEY = CODPRV
SELE PROMAE
SEEK VKEY
SELE (ALI)
RETURN


PROCEDURE VUSUA
*---------------
PARAMETER CSYS
PRIVATE ALI
ALI = ALIAS()
VKEY = ALLTRIM(CSYS)
SELE USU
SEEK VKEY
VFUN = NOMBRE
SELE (ALI)
RETURN VFUN

PROCEDURE siprv
*--------------
parameter vfun
vfun = .t.
if !v_reg='VG'
   do standby with 'El Proveedor no esta Regularizado...Observaci¢n'
endif
return vfun   

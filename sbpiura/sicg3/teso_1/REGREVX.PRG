* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RegREV.PRG  30/05/96                             L: 0803     ³	
* ³ Registro de Reversiones                                      ³
* ³ AUTOR   : 													 ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

USE Parmae   IN 1  ORDER TAG parmae1	ALIAS Parma
USE Cajas    IN 2  ORDER TAG Cajas1		ALIAS Caja
*USE Reversio IN 3  ORDER TAG Rever1	ALIAS Rever
USE RevSNUTS IN 3  ORDER TAG RevsnuTS1	ALIAS Rever
USE AstPat   IN 4  ORDER TAG AstPat15	ALIAS AstPat
USE Cuentas  IN 5  ORDER TAG cuentas1	ALIAS cuenta
USE astpre   IN 9  ORDER TAG astpre5	ALIAS astpre  
*USE SubCtas  IN 11 ORDER TAG Subctas1	ALIAS Subcta
USE MaePre   IN 12 ORDER TAG MaePre1	ALIAS MaePre
USE ITERI    IN 13 ORDER TAG ITERI3		ALIAS ITERI
USE Movbco   IN 14 ORDER TAG Movbco11	ALIAS Movbco
USE Auxil    IN 15 ORDER TAG Auxil2		ALIAS Auxil
USE Ingreso  IN 16 ORDER TAG Ingreso1	ALIAS IngR
USE CatAsi   IN 19  ORDER TAG CatAsi3       ALIAS CatAsi
USE IteCla   IN 18  ORDER TAG IteCla1       ALIAS IteCla
USE ItePari  IN 25  ORDER TAG ItePari1      ALIAS ItePari          

STORE SPACE(2)  TO vMes,vMesT
vmens04 = 'Dicha Reversi¢n no fue encontrada'
vmens08 = 'No hay registros para procesar'
vmens10 = 'La reversi¢n ya est  anulada'
vmens02 = ' °®F2¯Observaciones     ®F4¯Imprime REV      ®F9¯Corrige Asientos.°'
SELECT Rever
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK
PUBLIC W_TIPCTC,deme,VUSER_ID,wuser_id,mdescr
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO Pantalla                  && Muestra pantalla inicial
DO Vista
ON KEY LABEL F2 DO VISOBS
ON KEY LABEL F4 DO imp_rev

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

 DEFINE WINDOW wind_6 FROM 10,10 TO 18,70 ;
 TITLE ' ASIENTOS PRESUPUESTALES' COLOR SCHEME 02

* DEFINE WINDOW wind_cte0 FROM 00,00 TO 24,79 DOUBLE ;
* TITLE ' REVERSIONES AL TESORO  ' FOOTER '±F2 observaciones ±  ±F4 Imprime Doc.   ±F9 Corrige Asientos±  ' COLOR SCHEME 10 

 DEFINE WINDOW wind_cte0 FROM 00,00 TO 14,79 DOUBLE ;
 TITLE ' REVERSIONES ' FOOTER '±F2 CONCEPTO ±  ±DETALLE   ' COLOR SCHEME 10 

 DEFINE WINDOW Wind_2 FROM 14,00 TO 23,79 DOUBLE ;
 TITLE 'F2 CONCEPTO ±  ±DETALLE ' ;
 COLOR SCHEME 10

 DEFINE WINDOW wind_3 FROM 00,00 TO 24,79 DOUBLE ;
 TITLE ' REVERSIONES AL TESORO  F10 PARA SELECCIONAR' COLOR SCHEME 10 


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
 ON SELECTION PAD elimi  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 ON KEY LABEL F4 DO imp_rev
 ON KEY LABEL F9 DO cor_as
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW wind_cte0
 CLEAR	
 @  0, 2   SAY "  N§ Reversi¢n : " 
 @  0, 35  SAY "       Periodo : " 
 @  1, 2   SAY "           Mes : " 
 @  1, 35  SAY "         Fecha : "
 @  2, 2   SAY "        Nombre : "
 @  3, 2   SAY "       Cta.SBP : "
 @  6, 2   SAY "      Cod.Fte. : "
 @  6, 35  SAY "     Tipo Aux. : "
 @  7, 2   SAY " Cad.Funcional : "
 @  7, 27  SAY " UG  UE  FN PRG SBPRG ACTPRY"
 @  9, 2  SAY "       Referencia : "
 @ 10, 2 SAY  "      Observaci¢n : "
 @ 11, 2 SAY "             Monto : "
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW wind_cte0
 SELECT REVER
 IF EOF()
    DO Pantalla
    RETURN
 ENDIF
 SCATTER MEMVAR
=val_codca1(ALLTRIM(m.codcad),m.periodo,'C')

 ON KEY LABEL F9 DO cor_as
 @  0,22 SAY m.NumRev
 @  0,55 SAY m.periodo
 @  1,22 SAY m.Nummes
 @  1,29 SAY Val_Para(m.NumMes,'FECMES','D',3,12) 
 @  1,55 SAY m.Fecha
 @  2,22 SAY m.Nombre 
 @  3,22 SAY m.CodCtc
 @  3,38 SAY IIF(!EMPTY(m.codctc),Val_Fun("Caja","Codctc","Descri",ALLTRIM(m.Codctc)) ,' ')
 @  6,22 SAY Val_para(m.CodFte,'CODFTE','V',22,10)
 @  6,55 SAY Val_Para(m.TipAux,"AUXIL ","V",55,30)
 @  7,22 SAY m.CodCad
 @  8,27 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
 @  9,22 SAY m.Refer
 @ 10,22 SAY m.Observa
 @ 11,22 SAY m.Monto   PICTURE "999,999,999,999.99"

 DO Vista_Hijo

 RETURN
 
PROCEDURE Vista_Hijo
*-------------------
SELECT IteRi
SEEK ALLTRiM(m.NumMes)+ALLTRiM(m.NumREV)
SET ORDER TO iteri3
BROWSE ;
	NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
	WINDOW Wind_2 KEY ALLTRiM(m.NumMes)+ALLTRiM(m.NumREV);
	FIELDS;
		CodPart :H= 'C¢d.Par',;
		aa = IIF(!EMPTY(CodPart),ValAsi1(ALIAS(),CodPart,"8","Descri",'R'),' ') :H='Descripci¢n' :40 :W=!EMPTY(CodPart),;
		ImpParc :H= 'Monto'

*CodPart    :H= 'Partida':V=VAL_ING2(SUBSTR(CodPart,10,3),LEFT(CodPart,9),'codpart'):F:W=Tipo='P',;
*aa = IIF(!EMPTY(CODPART),VAL_ING(SUBSTR(CodPart,10,3),LEFT(CodPart,9),'Z'),' ') :H='DescRipci¢n' :40 :W=!EMPTY(CodPart),;
		
SELECT REVER
RETURN


PROCEDURE Revis
*--------------
IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
	RETURN
ENDIF
vTemp = RECNO()
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10    KEYBOARD CHR(23)
BROWSE WINDOW wind_cte0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
	NumMes  :H='Mes' ,;
	NumRev  :H='N£mero',;
	X1=IIF(tipo='M','Menores Gastos','Partida de Ingresos') :15    :H='Tipo',;
	CodSCta :H='SubCta' ,;
	Fecha   :H='Fecha',;
	Nombre  :H='Nombre'

ON KEY LABEL F10
vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mMenu
DO Vista
RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
VTEMP=RECNO()
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
ACTIVATE WINDOW standby
@ 1,01 SAY 'Ingrese N£mero : ' GET vRE PICTURE '@!' DEFAULT SPACE(4)
@ 1,30 SAY 'Mes:' GET vMes
READ
DEACTIVATE WINDOW standby
IF EMPTY(vRE) .or. LASTKEY()=27
	RETURN
 ELSE
	vRE = PADL(ALLTRIM(vRE),4,'0')
	SEEK ALLTRIM(vMes)+ALLTRIM(vRE)
	IF !FOUND()
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
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF
SELECT REVER
IF !BOF()
	SKIP -1
ENDIF
IF BOF()
	GO TOP
	DO standby WITH "Inicio de archivo: no existe Reversi¢n anterior"
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
SELECT REVER
IF !EOF()
	SKIP
ENDIF
IF EOF()
	DO standby WITH "Fin de archivo: no existe Reversi¢n siguiente"
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

IF Estado = '99'
	DO StandBy WITH "El Documento esta Anulado"
	RETURN
ENDIF

SCATTER MEMVAR
ACTIVATE WINDOW wind_cte0
DO PANTALLA

@  0,22 SAY m.NumRev
@  0,55 GET m.periodo  PICTURE '!!'   
@  1,22 GET m.NumMes   PICTURE '!!'   VALID Val_Para(m.NUmMes,'FECMES',' ',22,9)  DISABLE
@  1,55 GET m.Fecha    VALID(MONTH(m.fecha)=VAL(m.nummes))
@  2,22 GET m.Nombre
@  3,22 GET m.CodCtc   PICTURE '@!'   VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",m.CodCTc,1,3,22)    
@  6,22 GET m.CodFte   PICTURE "@!"   VALID Val_Para (m.CodFte, 'CODFTE', ' ',22,22)
@  6,55 GET m.TipAux   PICTURE "!!"   VALID Val_ParaD(m.TipAux,"AUXIL "," ",55) 
@  7,22 GET m.CodCad   PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',22,47) 
@  9,22 GET m.Refer
@ 10,22 GET m.Observa  SIZE 1,40
@ 11,22 GET m.Monto    PICTURE "999,999,999,999.99"
    	 
READ VALID Val_Read()
m.usuario=wuser_id
SELE CAJA
SEEK m.codctc
w_tipctc=CAJA.Tipo
SELE Rever
IF LASTKEY() <> 27
	SS=TRA_HIJO()
	=concepto()
	DO Compre
	IF SS .AND. LASTKEY()#27
		SELECT REVER
		GATHER MEMVAR
	ENDIF
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
ENDIF
UNLOCK
SELECT REVER
ON KEY LABEL F2 DO VISOBS
ON KEY LABEL F4 DO imp_rev
DO Vista                    && Muestra nuevos datos
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
SELECT REVER
SCATTER MEMVAR BLANK
SELECT Parma
SEEK 'CORRELREVSNU'
m.NumRev = PADL(NumEnt+1,4,'0')
SELECT REVER
DO Pantalla
m.Periodo = RIGH(STR(YEAR(DATE()),4),2)
m.Fecha  = DATE()
m.FecBol = DATE()
m.tipdoc = 'REB'
m.Estado = '00'
@  0,22 GET m.NumRev	DISABLE
@  0,55 GET m.periodo
@  1,22 GET m.Nummes   PICTURE '!!'   VALID Val_Para(m.NUmMes,'FECMES',' ',22,9) AND VAL_REV(m.nummes,m.numREV)
@  1,55 GET m.Fecha    VALID(MONTH(m.fecha)=VAL(m.nummes))
@  2,22 GET m.Nombre
@  3,22 GET m.CodCtc   PICTURE '@!'   VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",m.CodCTc,1,3,22)
@  6,22 GET m.CodFte   PICTURE "@!"   VALID Val_Para(m.CodFte, 'CODFTE', ' ',22,12)
@  6,55 GET m.TipAux   PICTURE "!!"   VALID Val_ParaD(m.TipAux,"AUXIL "," ",55,12)
@  7,22 GET m.CodCad   PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',22,47)
@  9,22 GET m.Refer
@ 10,22 GET m.Observa  SIZE 1,40
@ 11,22 GET m.Monto    PICTURE "999,999,999,999.99"
READ VALID Val_Read()
m.usuario = wuser_id
SELE Caja
SEEK ALLTRiM(m.CodCtc)
IF FOUND()
	w_TipCtc=Caja.tipo
	wbanco = Caja.banco
	wclase = Caja.Clase
	vdeb   = Caja.CuentaH
ENDIF
SELE REVER
vElimina = .F.
IF LASTKEY() # 27
	IF F_Appd()
		GATHER MEMVAR
		UNLOCK
		IF ALLTRIM(wclase) = 'T'
			SELE REVER
			DO Agreg1
		 ELSE
			SELE REVER
			DO Agreg1
			SELECT ITERI
			SCATTER MEMVAR
		ENDIF
		DO WHILE .T.
			DO Tra_hijo
			=concepto()
			DO Compre
			IF LASTKEY() = 27
				IF YESNO( '¨ Cancela el Ingreso ?' )
					vElimina = .T.
					EXIT
				ENDIF
			 ELSE
				IF YESNO( '¨ Est n correctos los datos ?')
					EXIT
				ENDIF
			ENDIF
		ENDDO
		SELE Parma
		SEEK 'CORRELREVSNU'
		IF FOUND()
			REPLACE NumEnt WITH NumEnt+1
		ENDIF
	ENDIF
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
ENDIF
UNLOCK ALL
ON KEY LABEL F2 DO VisObs
ON KEY LABEL f4 DO imp_Ri
DO Vista
RETURN

PROCEDURE Agr_Enl
*----------------
PRIVATE cAlias
cAlias = ALIAS()

SELE IteRi
SEEK m.Periodo+m.Nummes+m.NumRI
PUBLIC DECLARE aCadEnl(1,9)
I = 0
SCAN WHILE Periodo+NumMes+NumRI = m.Periodo+m.Nummes+m.NumRI
	IF Iteri.Tipo = 'P'
		SELE IteCla
		IF SEEK(IteRi.CodPart+Iteri.CodCla)
			SCAN WHILE CodPart+CodCla = IteRi.CodPart+Iteri.CodCla
				IF TipDoc = 'R/I'
					I = I + 1
					PUBLIC DECLARE aCadEnl(i,9)
					aCadEnl(i,1) = "R"
					aCadEnl(i,2) = IteCla.CuentaD
					aCadEnl(i,3) = IteCla.CuentaH
					aCadEnl(i,4) = IteCla.DAstPre
					aCadEnl(i,5) = IteCla.HAstPre
					aCadEnl(i,6) = IteCla.DAstOrd
					aCadEnl(i,7) = IteCla.HAstOrd
					aCadEnl(i,8) = IteRI.ImpParc
				ENDIF
			ENDSCAN
		ENDIF
		IF SEEK(IteRi.CodPart)
			SCAN WHILE CodPart = IteRi.CodPart
				IF TipDoc = 'FAC'
					I = I + 1
					DECLARE aCadEnl(i,9)
					aCadEnl(i,1) = "F"
					aCadEnl(i,2) = IteCla.CuentaD
					aCadEnl(i,3) = IteCla.CuentaH
					aCadEnl(i,4) = IteCla.DAstPre
					aCadEnl(i,5) = IteCla.HAstPre
					aCadEnl(i,6) = IteCla.DAstOrd
					aCadEnl(i,7) = IteCla.HAstOrd
					aCadEnl(i,8) = IteRI.ImpParc
					aCadEnl(i,9) = IteCla.IGV
				ENDIF
			ENDSCAN
		ENDIF
	ENDIF
	IF Iteri.Tipo = 'O'
		SELE AsiAut
		IF SEEK("R/I"+m.TipRI)
			I = I + 1
			PUBLIC DECLARE aCadEnl(i,9)
			aCadEnl(i,1) = "R"
			aCadEnl(i,2) = AsiAut.DAstPat
			aCadEnl(i,3) = AsiAut.HAstPat
			aCadEnl(i,4) = ""					&&AsiAut.DAstPre
			aCadEnl(i,5) = ""					&&AsiAut.HAstPre
			aCadEnl(i,6) = ""					&&AsiAut.DAstOrd
			aCadEnl(i,7) = ""					&&AsiAut.HAstOrd
			aCadEnl(i,8) = IteRI.ImpParc
		ENDIF
		
		IF SEEK("FAC"+m.TipRI)
			I = I + 1
			DECLARE aCadEnl(i,9)
			aCadEnl(i,1) = "F"
			aCadEnl(i,2) = AsiAut.DAstPat
			aCadEnl(i,3) = AsiAut.HAstPat
			aCadEnl(i,4) = ""					&&AsiAut.DAstPre
			aCadEnl(i,5) = ""					&&AsiAut.HAstPre
			aCadEnl(i,6) = ""					&&AsiAut.DAstOrd
			aCadEnl(i,7) = ""					&&AsiAut.HAstOrd
			aCadEnl(i,8) = IteRI.ImpParc
		ENDIF
	ENDIF
ENDSCAN

RETURN

PROCEDURE ANULA
*--------------
SELECT REVER
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
IF Estado = '99'
	DO STANDBY WITH  vmens10
	RETURN
ENDIF
velimina = YESNO('¨ Desea ANULAR esta Reversi¢n ?')
IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
	REPLACE ESTADO WITH '99'
*	REPLACE usuario WITH wuser_id
	DELETE NEXT 1
	DO Vista
ENDIF
UNLOCK
sele astpat
SEEK ALLTRIM(m.nummes)+m.numREV+alltrim(m.codctc)
IF FOUND()
	scan while nummes=alltrim(m.nummes) and numref=m.numREV and codctc=m.codctc and tipdoc="REV"
		IF RLOCK() .OR. F_LOCK(1)
			DELETE NEXT 1
		ENDIF
		UNLOCK
	ENDSCAN
ENDIF
SELE REVER    
RETURN

PROCEDURE Lista  &&Reportes varios
*--------------
SELECT REVER
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
DEFINE WINDOW lis FROM 3,10 TO 18,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocta,vestado,vlista
STORE SPACE(14) TO vCta,vCuenta
STORE SPACE(2)  TO vAno,vMes
STORE SPACE(4)  TO vCli
VCTA=M.CODCTC
VANO=M.NUMMES
VCLI=M.NUMrev
STORE DATE() TO vfecini, vfecfin
@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento'
*;Menores Gastos;Partida Ingresos'
@ 05,01 SAY "     N§ Documento : "
@ 05,22 GET vano    WHEN vlista=1  PICTURE '!!'
@ 05,25 GET vcli    WHEN vlista=1  PICTURE '!!!!' 
@ 07,01 SAY "   Cta. Corriente : "
@ 07,22 GET vcta  WHEN  vlista=1 OR VLISTA=2 OR vlista=3 VALID val_fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,10,07)
@ 09,01 SAY " Fecha de Emisi¢n : "
@ 10,22 GET vfecini  PICTURE '@D' COLOR SCHEME 7   WHEN vlista>1 
@ 10,32 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7 WHEN vlista>1
@ 12,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS

IF OKCANCEL = 1
	ACTIVATE WINDOW STANDBY
	@ 01,04 SAY 'Espere un momento........'
	SELECT Rever
	GO TOP
	vInd = SYS(3) + '.IDX'
	vbas = SYS(3) + '.DBF'
	DEACTIVATE WINDOW STANDBY
	DO CASE
		CASE vLista=2 OR vLista=3
			IF vLista=2
				SET FILT TO Rever.Tipo='M' AND Rever.codctc=vcta AND BETWEEN(Rever.fecha,vfecini,vfecfin)
				IF EOF()
					DO StanDBY with VMENS08
					SELECT REVER
					SET FILT TO
					IF vTemp>0
						GO vTemp
					ENDIF
					DO vista
					RETURN
				ENDIF
				DO REPORTE WITH 2,"LisRev2",' Reporte de Reversiones por Menores Gastos'
			 ELSE
				SET FILT TO Rever.Tipo='P' AND Rever.codctc=vcta AND BETWEEN(Rever.fecha,vfecini,vfecfin)
				IF EOF()
					DO StanDBY with VMENS08
					SELECT REVER
					SET FILT TO
					IF vTemp>0
						GO vTemp
					ENDIF
					DO vista
					RETURN
				ENDIF
				DO REPORTE WITH 2,"LisRev2",' Reporte de Reversiones por Partida de Ingresos'
			ENDIF
		CASE vLista=1
			SEEK ALLT(vano)+vcli
			IF FOUND()
				COPY NEXT 1 TO (vbas)
				USE (Vbas) IN 0 ALIAS RE
				SELE RE
				INDEX ON RE.NumMes+RE.Numrev+RE.codctc TO (vind)
*				SET RELATION TO RE.NumMes+RE.Numrev+RE.codctc INTO AstPat
				SET MEMOWIDTH TO 75
				SET RELATION TO re.nummes+ re.numrev INTO ITERI ADDITIVE

				DO repPRG WITH "Rever"," Reporte de Reversiones ",2

*				DO REPORTE WITH 2,"Rever",' Reporte de Reversiones '
*				DO REPORTE WITH 2,"T-6",' Reporte de Reversiones '
				SET RELATION OFF INTO Iteri
*				SET RELATION OFF INTO astpat
				USE
				ERASE (vbas)
			 ELSE
				DO Standby WITH vmens08
			ENDIF
	ENDCASE
ENDIF
SELECT REVER
SET ORDER TO TAG REVSNUTS1
IF vTemp>0
	GO vTemp
ENDIF
DO vista
RETURN


PROCEDURE ingap   && Ingresa asientos patrimoniales
*--------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgrIte
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numrev+m.codctc
IF !FOUND()
	DO agrite
ENDIF
BROWSE NOAPPEND NODELETE NOMENU WINDOW WIND_2 KEY ALLTRIM(m.nummes)+m.numrev+m.codctc FIELDS ;
	codcta :H='Cuenta',;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999.99'

STORE 0 TO vdebe, vhaber ,vRet
SEEK ALLTRIM(m.nummes)+m.numrev+m.codctc
SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numrev=numrev AND m.codctc=codctc
IF IIF(TipCta='D',MtoDeb,MtoHab)=0
 	dele next 1
ELSE
	vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
	vhaber= vhaber + IIF(tipcta='H',mtohab,0)
ENDIF		
ENDSCAN

IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
ON KEY
SELECT REVER

ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5

UNLOCK
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
 
RETURN


PROCEDURE agrite
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes  WITH m.nummes,;
	        numref  WITH m.numrev,;
	        tipdoc  WITH 'REV',;
	        codctc  WITH m.codctc,;
	        fecha   WITH m.Fecha,;
	        TIPCTC WITH W_TIPCTC,;
	        Periodo WITH m.periodo
	        
ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT astpat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN


FUNCTION ValRes
*--------------
 SELECT REVER
 vTemp=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK vMes+vCli
 IF !FOUND()
*    SET ORDER TO Rever1 
    SET ORDER TO RevSNUTS1 
    HIDE MENU mMenu
    ACTIVATE SCREEN
    vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
    DO Logos WITH Rotulo1,vTempo    
    ON KEY LABEL F10 KEYBOARD CHR(23)    
    BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
    NumMes :H='Mes',;
    NumRev :H=' N§ Rev.' ,;
    Fecha  :H='Fecha' ,;
    Monto :H='Total Reversi¢n',;
    Observ :H='Observaci¢n',;
    CodCtc :H='CtaCte'

    vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
    DO Logos WITH Rotulo1,vTempo
    IF LASTKEY()=27
       GO Vtemp
    ENDIF

    SHOW MENU mMenu
    ON KEY LABEL F10
    SET RELATION TO
    GO Vtemp
 ENDIF
 vMes=NumMes
 vCli=NumRev

 RETURN .t.


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  ON KEY LABEL F2
  ON KEY LABEL F4
  ON KEY LABEL F9
  ON KEY
  RETURN


PROCEDURE Termina
*----------------
  CLOSE DATA
  ON KEY LABEL F2
  RELEASE    WINDOW wind_cte0
  RELEASE    WINDOW wind_cte1
  RELEASE    MENU   mMenu
  ACTIVATE   SCREEN
  RESTORE    SCREEN FROM Principal
  RETURN

PROCEDURE valfecha
*-----------------
an=RIGHT(STR(YEAR(DATE()),4),2)
vme = VAL(m.nummes)+1
me = PADL(ALLTRIM(STR(vme,2)),2,'0')
vfec = '01/&ME/&AN'
m.feccp = CTOD(vfec) - 1
RETURN .T.


*FUNCTION cheqscta
**---------------
SELE CAJA
SEEK m.codctc
IF FOUND()
	m.codscta=ALLT(Caja.CodScta)
ENDIF	
RETURN 


FUNCTION Vctc
**---------------
SELE CAJA
LOCATE FOR clase='T'
IF FOUND()
	m.ctates=ALLT(Caja.Codctc)
ENDIF	
RETURN 

FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9))

FUNCTION concepto
*---------------
vAlias = ALIAS()
SELECT Rever
SET MEMOWIDTH TO 50
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Concepto")
   DEFINE WINDOW Concepto FROM 03,14 TO 20,66 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF
IF WVISIBLE("Concepto")
   ACTIVATE WINDOW concepto SAME
ELSE
   ACTIVATE WINDOW concepto NOSHOW
ENDIF
MODIFY MEMO concepto WINDOW concepto
IF !WVISIBLE("Concepto")
   ACTIVATE WINDOW concepto
ENDIF
RELEASE WINDOW concepto
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba la Observaci¢n '
ENDIF
SELECT (vAlias)
RETURN .T.


FUNCTION VisObs
*--------------
vAlias = ALIAS()
SELECT Rever
IF !WEXIST("concepto")
   DEFINE WINDOW concepto FROM 03,14 TO 20,66 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("concepto")
   ACTIVATE WINDOW concepto SAME
ELSE
   ACTIVATE WINDOW concepto NOSHOW
ENDIF

MODIFY MEMO concepto NOEDIT WINDOW concepto

IF !WVISIBLE("concepto")
   ACTIVATE WINDOW concepto
ENDIF
RELEASE WINDOW concepto
RETURN .T.


PROCEDURE imp_REV
*---------------
   vInd = SYS(3) + '.IDX'
   vbas = SYS(3) + '.DBF'
   SEEK ALLT(m.nummes)+ALLT(m.numrev)
		  IF FOUND()
		      COPY NEXT 1 TO (vbas)
		      USE (Vbas) IN 0 ALIAS RE
		      SELE RE
		      INDEX ON RE.NumMes+RE.Numrev+RE.codctc TO (vind)
	       	  SET RELATION TO RE.NumMes+RE.Numrev+RE.codctc INTO AstPat
	       	  SET MEMOWIDTH TO 75
			  DO repPRG WITH "Rever"," Reporte de Reversiones ",2
			  
*		   	  DO REPORTE WITH 2,"Rever",' Reporte de Reversiones '
	    	  SET RELATION OFF INTO astpat
	  	      USE
	  	      ERASE (vbas)
	      ELSE
              DO Standby WITH vmens08
          ENDIF         
	SELECT REVER
*	SET ORDER TO TAG REVER1
    SET ORDER TO RevSNUTS1 
	DO vista
RETURN	


PROCEDURE cor_as  &&Corrige Asientos Patrimoniales
*---------------
IF m.tipo='M'
SELE Caja
SEEK ALLTRiM(m.CodCtc)
IF FOUND()
   w_TipCtc=CAJA.TIPO
ENDIF
SELE rever
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgrIte
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numrev+m.codctc
IF !FOUND()
	DO agrite
ENDIF
BROWSE NOAPPEND NODELETE NOMENU WINDOW WIND_2 KEY ALLTRIM(m.nummes)+m.numrev+m.codctc FIELDS ;
	codcta :H='Cuenta',;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999.99'

STORE 0 TO vdebe, vhaber ,vRet
SEEK ALLTRIM(m.nummes)+m.numrev+m.codctc
SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numrev=numrev AND m.codctc=codctc
    IF IIF(TipCta='D',MtoDeb,MtoHab)=0
    	dele next 1
    else
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
    ENDIF		
ENDSCAN

IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
ON KEY
ON KEY LABEL F4 DO imp_rev
SELECT REVER

ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5

UNLOCK
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
SELE Rever
ENDIF
DO vista
RETURN

&&&&&&&&&&&&&&&&&&&

PROCEDURE ingap   && Ingresa asientos patrimoniales
*--------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgRite
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT astpat
SEEK m.Periodo+ALLTRiM(m.nummes)+m.numREF+m.CodCtc
IF !FOUND()
	DO agRiaut
ENDIF

******* 
*IF m.numri <='0663'
*	SEEK m.Periodo+ALLTRiM(m.nummes)+m.numRi+m.CodCtc*
*	SCAN WHILE PERiODO=m.Periodo AND ALLTRiM(m.nummes)=nummes AND m.numRi=numRi AND m.CodCtc=codctc 
*    	dele next 1
*	ENDSCAN
*	DO agriaut
*ENDIF

*******

BROWSE FIELDS ;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+DescRi",codcta,2):F ,;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999,999.99';
	WINDOW wind_2 KEY m.Periodo+ALLTRiM(m.NumMes)+m.NumREF+m.CodCtc
STORE 0 TO vdebe, vhaber ,vRet
SEEK m.Periodo+ALLTRiM(m.nummes)+m.numREF+m.CodCtc
SCAN WHILE PERiODO=m.Periodo AND ALLTRiM(m.nummes)=nummes AND m.numREV=numREF AND m.CodCtc=codctc 
    IF IIF(TipCta='D',MtoDeb,MtoHab)=0
    	dele next 1
    else
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
    ENDIF		
ENDSCAN
IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
ON KEY
SELECT REVER
ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5
UNLOCK
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
RETURN

PROCEDURE Trabaja_Hijo
*---------------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SELE IteRi
 SEEK ALLTRiM(m.Periodo)+ALLTRiM(m.NumMes)+ALLTRiM(m.NumRi)
 IF !FOUND()
    DO Agreg_Item
 ENDIF
   BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_2;
   KEY m.Periodo+ALLTRiM(m.NumMes) + ALLTRiM(m.NumRi) FIELD ;
   Tipo       :H= 'T':P='@M O,P',;
	CodPart    :H= 'Partida':V=V_Part(CodPart) :F:W=Tipo='P',;
	aa = IIF(!EMPTY(CodPart),ValAsi1(ALIAS(),CodPart,'8','Descri','R'),' ') :H='Descripci¢n' :40 :W=!EMPTY(CodPart),;
   BolDep     :H= 'N§ Bol./Dep.': P='@!',;
   FecDep     :H= 'FechaDep',;
   Glosa      :H= 'Ingreso en': P='@!',;
   ImpParc    :H='Monto' : P='999,999,999,999.99',;
   DocRef     :H='B/D' :R :F
   
*	CodPart    :H= 'Partida':V=VAL_ING2(SUBSTR(CodPart,10,3),LEFT(CodPart,9),'codpart'):F:W=Tipo='P',;
*	aa = IIF(!EMPTY(CODPART),VAL_ING(SUBSTR(CodPart,10,3),LEFT(CodPart,9),'Z'),' ') :H='DescRipci¢n' :40 :W=!EMPTY(CodPart)
 SELECT IteRi
 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5
 UNLOCK ALL
 SELECT Ri
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 RETURN

PROCEDURE Agreg_Item
*-------------------
SELECT Parma
SEEK "CORREL"+"MOVBCO"
vNUMERO=PADL(NUMENT+1,4,'0')
SELECT IteRi
IF F_Appd()
   REPLACE Periodo WITH m.Periodo,;
   		   NumMes  WITH m.NumMes,;
   		   Estado  WITH m.Estado,;
   		   Fecdep  WITH m.fecHA,;
   		   NUMREV  WITH M.NUMREV
   		   
  * SELECT Parma
  * SEEK "CORREL"+"MOVBCO"
  * REPLACE NUMENT WITH NUMENT+1
   SELE ITERI
ENDIF
RETURN



PROCEDURE Elimi_Item
*-------------------
SELECT IteRi
if rlock()
   SELECT IteRi
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif

return

PROCEDURE Tra_Hijo
*---------------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO Agreg1
ON KEY LABEL F8  DO Elimi_Item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELE IteRi
SEEK ALLTRiM(m.NumMes)+ALLTRiM(m.NumREV)
IF !FOUND()
	DO Agreg1
ENDIF

BROWSE NOAPPEND NODELETE NOREFRESH NOMENU NOOPTIMIZE WINDOW Wind_2;
	KEY ALLTRiM(m.NumMes) + ALLTRiM(m.NumREV) FIELD ;
		Tipo       :H= 'T':P='@M O,P',;
		CodPart    :H= 'Partida':V=V_Part(CodPart) :F:W=Tipo='P',;
    	aa = IIF(!EMPTY(CodPart),ValAsi1(ALIAS(),CodPart,'8','Descri','R'),' ') :H='Descripci¢n' :40 :W=!EMPTY(CodPart),;
		Glosa      :H= 'Ingreso en': P='@!',;
		ImpParc    :H='Monto' : P='999,999,999,999.99'

*		CODCLA     : H= 'Cla.':V=Val_cla(codpart,CODPART+codcla,'codcla'):F :F:W=Tipo='P' AND !empty(CodPart),;
*		CodPart    :H= 'Partida':V=VAL_ING2(SUBSTR(CodPart,10,3),LEFT(CodPart,9),'codpart'):F:W=Tipo='P',;
*		aa = IIF(!EMPTY(CODPART),VAL_ING(SUBSTR(CodPart,10,3),LEFT(CodPart,9),'Z'),' ') :H='DescRipci¢n' :40 :W=!EMPTY(CodPart),;

   *NROCAR     :H='N/C' 
   *DocRef     :H= 'N§ Reversi¢n'  :R :F,;
   *FecDep     :H= 'FechaRev.',;

SELECT IteRi
SEEK ALLTRiM(m.NumMes) + ALLTRiM(m.NumREV)
IF FOUND()
	m.Monto = 0
	SCAN WHILE ALLTRiM(m.NumMes)=NumMes AND ALLTRiM(m.NumREV)=NumRev
		m.Monto = m.Monto + ImpParc
	ENDSCAN
ENDIF
SELE Rever
REPLACE Monto WITH m.Monto 
 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5
 UNLOCK ALL
 SELECT REVER
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 RETURN


PROCEDURE Agreg1
*-------------------
*SELECT Parma
*SEEK "CORREL"+"MOVBCO"
*vNUMERO=PADL(NUMENT+1,4,'0')
SELECT IteRi
IF F_Appd()
	REPLACE Periodo WITH m.Periodo,;
			NumMes  WITH m.NumMes ,;
			Estado  WITH m.Estado ,;
			Fecdep  WITH m.fecHA  ,;
			NUMREV  WITH M.NUMREV
ENDIF

*		NROCAR  WITH VNUMERO
*		SELECT Parma
*		SEEK "CORREL"+"MOVBCO"
*		REPLACE NUMENT WITH NUMENT+1

SELE ITERI
RETURN

PROCEDURE ingap2   && Ingresa asientos patRimoniales para Reversiones
*-------------------
*PARAMETERS vasiento
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgRite
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)

SELECT Astpat
xord = ORDER()
SET ORDER TO Astpat15

SELECT IteRi
SEEK m.Periodo+ALLTRiM(m.NumMes) + ALLTRiM(m.NumREV)
vpeRiodo = m.Periodo
vnummes  = m.nummes
vnumREV  = m.numREV
SCAN WHILE vPERiODO+ALLTRiM(vNumMes) + ALLTRiM(vNumREV) =PERiODO+ALLTRiM(NumMes) + ALLTRiM(NumREV)
	SELECT Rever
	SEEK ALLT(IteRi.nummes)+Allt(Iteri.NUMREV)
	*Docref)
	IF !FOUND()
		APPEND BLANK
		REPLACE PeRiodo 	WITH vpeRiodo ,;
				Nummes  	WITH vnummes  ,;
				numRev  	WITH VNUMREV  ,;
				Fecha   	WITH IteRi.fecdep ,;
				Tipo    	WITH 'M' ,;
				TipRev  	WITH wtiprev ,;
				Glosa   	WITH 'DEPOSITO R/I '+m.numRi ,;
				ctates  	WITH m.CodCtc ,;
				Monto   	WITH IteRi.impparc ,;
				Tipdoc  	WITH 'R/I',;
				perdoc  	WITH IteRi.peRiodo,;
				mesdoc      WITH IteRi.nummes ,;
				numdoc  	WITH IteRi.numRi ,;
				estado      WITH '00' ,;
				Codctc      WITH vctc,;
				Codscta     WITH vscta,;
				concepto    WITH '   RUBRO A: REVERSIONES POR MENORES GASTOS.-             '+Ri.observ,;
				flag        WITH 'I',;
				codofg      WITH vofig,;
				usuario     WITH wuser_id

	ELSE
		REPLACE PeRiodo 	WITH vpeRiodo ,;
				Fecha   	WITH IteRi.fecdep ,;
				Tipo    	WITH 'M' ,;
				TipRev  	WITH wtiprev ,;
				Glosa   	WITH 'DEPOSITO R/I '+m.numRi ,;
				ctates  	WITH m.CodCtc ,;
				Monto   	WITH IteRi.impparc ,;
				Tipdoc  	WITH 'R/I',;
				perdoc  	WITH IteRi.peRiodo,;
				mesdoc      WITH IteRi.nummes ,;
				numdoc  	WITH IteRi.numRi ,;
				estado      WITH '00' ,;
				codscta     WITH vscta,;
				Codctc      WITH vctc,;
				concepto    WITH '   RUBRO A: REVERSIONES POR MENORES GASTOS.-             '+Ri.observ,;
				flag        WITH 'I',;
				codofg      WITH vofig,;
				usuario     WITH wuser_id
	ENDIF		
	=concepto()
	SELECT Astpat
	SEEK ALLT(IteRi.Nummes)+Iteri.NUMREV+vctc
	IF !FOUND()
		APPEND BLANK
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'REV' ,; 
				fecha   WITH IteRi.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH Iteri.NUMREV ,;
				tipcta  WITH 'D',;
				codctc  WITH vctc ,;
				codcta  WITH SPACE(12),;
				Mtodeb  WITH IteRi.impparc ,;
				tipctc  WITH w_tipctc,;
				numdoc  WITH vscta,;
				doc     WITH vofig
		APPEND BLANK
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'REV' ,; 
				fecha   WITH IteRi.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH Iteri.NUMREV ,;
				tipcta  WITH 'H',;
				codctc  WITH vctc ,;
				codcta  WITH SPACE(12),;
				Mtohab  WITH IteRi.impparc ,;
				tipctc  WITH w_tipctc,;
				numdoc  WITH vscta,;
				doc     WITH vofig
	ELSE
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'REV' ,; 
				fecha   WITH IteRi.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH Iteri.NUMREV ,;
				tipcta  WITH 'D',;
				codctc  WITH vctc ,;
				codcta  WITH SPACE(12),;
				Mtodeb  WITH IteRi.impparc ,;
				tipctc  WITH w_tipctc,;
				numdoc  WITH vscta,;
				doc     WITH vofig
		SKIP
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'REV' ,; 
				fecha   WITH IteRi.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH Iteri.NUMREV ,;
				tipcta  WITH 'H',;
				codctc  WITH vctc ,;
				codcta  WITH SPACE(12),;
				Mtohab  WITH IteRi.impparc,;
				tipctc  WITH w_tipctc,;
				numdoc  WITH vscta,;
				doc     WITH vofig
	ENDIF
	SELECT IteRi
ENDSCAN	
SELECT Astpat
SET ORDER TO (xord)
SELE REVER
RETURN

FUNCTION PidApRev
*---------------
 DEFINE WINDOW Wind_7 FROM 10,10 TO 18,70  DOUBLE ;
 TITLE " Asientos para Reversiones " COLOR SCHEME 10
 vdeb = SPACE(12)
 vhab = SPACE(12)
 *vscta= SPACE(8)
 vofig= SPACE(02)
 *vctc = SPACE(14)
 vctc = '631018599'
 vscta= '00230-98'
 vofig= '01'
 ACTIVATE WINDOW Wind_7
 alia  = ALIAS()
 STORE SPACE(1) TO vgas
 SELE Ri
 vgas  = tipgas
 SELECT ITERI
 SEEK ALLTRIM(m.periodo)+ALLTRiM(m.nummes)+m.numRi
 IF FOUND()
 	vnroref = Iteri.DocRef &&Nro de la Boleta de Dep¢sito
 	SELECT ASTPAT
    Vord = ORDER()
    SET ORDER TO AstPat15
    SEEK m.nummes+ALLT(VnroRef)+m.codctc
    
    IF FOUND()
     IF m.numri <='0663'
       REPLACE codcta WITH vdeb
       Vscta =  ALLTRIM(numdoc)
       Vofig =  ALLTRIM(doc)
       Vctc  =  ALLTRIM(codctc)
       skip
       REPLACE codcta WITH vhab
     ELSE
       Vdeb  =  CodCta
       Vscta =  ALLTRIM(numdoc)
       Vofig =  ALLTRIM(doc)
       Vctc  =  ALLTRIM(codctc)
       SKIP
       Vhab=CodCta
     ENDIF
    ENDIF
    ON KEY LABEL ESC DO escap
	DEFINE WINDOW Wind_7a FROM 12,12 TO 16,68  DOUBLE ;
 	TITLE " Tipo de Gasto " COLOR SCHEME 10
	ACTIVATE WINDOW Wind_7a
	@ 1,05 SAY "Gasto            :" GET vgas  VALID Val_Para(vgas,'CATGAS',' ',24,20)
	READ VALID Val_read()
    DEACTIVATE WINDOW Wind_7a
    vdeb=IIF(ALLT(m.tipri)='04','302010101000',IIF(ALLTRIM(vgas)='5','751010101000','302010101000'))
    vhab  ='101010101000'
    SELE Ri
    REPLACE Tipgas WITH ALLTRIM(vgas)
	ON KEY LABEL ESC
    SELECT ASTPAT
    SET ORDER TO (Vord)   
 ENDIF 	
 @ 1, 05 SAY "Debe            :" GET vdeb  VALID val_fun('Cuenta','Cuenta',"Cuenta+' '+DescRi",vdeb,1)
 @ 2, 05 SAY "Haber           :" GET vhab  VALID val_fun('Cuenta','Cuenta',"Cuenta+' '+DescRi",vhab,1)
 @ 4, 05 SAY "Cuenta Corriente:" GET Vctc  VALID Val_Fun("Caja","Codctc","Codctc+' '+Descri",Vctc,1) 
 @ 5, 05 SAY "SubCuenta       :" GET vscta VALID Val_Fun("SubCta","Subcta","Subcta+' '+Descri",Vscta,1) 
 @ 6, 05 SAY "Oficina Giradora:" GET Vofig VALID Val_Para(vofig,'OFIGIR',' ',20,10)
 READ VALID Val_read()
 DEACTIVATE WINDOW Wind_7
 SELECT (alia)
 RETURN vdeb+vhab

PROCEDURE CAPTURA
*----------------
DEFINE WINDOW XXBusca FROM 10,12 TO 14,68 DOUBLE TITLE 'ASIENTOS PARA N/C';
COLOR SCHEME 5
ACTIVATE WINDOW XXBusca
VCTAD=SPACE(12)
VCTAH=SPACE(12)
 @ 0, 02  SAY 'Fecha de Bancos :' GET m.fecha 
 @ 1, 02  SAY 'DEBE' +SPACE(12)+'HABER' 
 @ 2, 02  GET VCTAD VALID val_fun('Cuenta','Cuenta',"Cuenta+' '+DescRi",VCTAD,1)
 @ 2, 17  GET VCTAH VALID val_fun('Cuenta','Cuenta',"Cuenta+' '+DescRi",VCTAH,1)
 READ VALID Val_Read()
 DEAC WIND XXBUSCA

SELECT astpat
xord = ORDER()
SET ORDE TO ASTPAT15
SEEK ALLT(M.Nummes)+ALLT(M.NUMREV)
IF !FOUND()
IF LASTKEY()#27
 vAnula = .F.
       APPEND BLANK
       *IF f_appd()
       REPLACE  Periodo WITH m.periodo,;
	            tipdoc  WITH 'N/C'    ,;
	            FECHA   WITH m.FECHA  ,;
	            NUMMES  WITH M.NUMMES ,;
	            numref  WITH m.numrev ,;  
                TIPCTA  WITH 'D'      ,;
	            codctc  WITH m.codctc ,;
	            MTODEB  WITH M.MONTO  ,;
	            CODCTC  WITH M.CODCTC ,;
   	            FECHC   WITH m.FECHA  ,;
   	            TIPCTC  WITH w_TipCtc ,;
	            CODCTA  WITH VCTAD   
	    APPEND BLANK
        REPLACE  Periodo WITH m.periodo,;
	            tipdoc  WITH 'N/C'    ,;
	            FECHA   WITH m.FECHA  ,;
	            NUMMES  WITH M.NUMMES ,;
	            numref  WITH m.numrev ,;
	            TIPCTA  WITH 'H'      ,;
	            codctc  WITH m.codctc ,;
	            MTOHAB  WITH M.MONTO  ,;
	            CODCTC  WITH M.CODCTC ,;
   	            FECHC   WITH m.FECHA  ,;
   	            TIPCTC  WITH w_TipCtc ,;
	            CODCTA  WITH VCTAH
 ENDIF
ELSE
    DO STANDBY WITH 'Proceso cancelado'
ENDIF    
=concepto()
SELECT Astpat
SET ORDER TO (XORD)
RETURN

FUNCTION Val_IngE
*---------------
  PARAMETERS mValor, Filtro, mVaRiable, mCol, mLong
  PRiVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVaRiable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRi
  ENDCASE
  mAlias  = ALIAS()
  SELECT IngR
  SEEK Filtro+mValor

  IF !FOUND() AND !mVaRiable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
         SET FILTER TO CodIng >= Filtro
      ENDIF
      GO TOP
      IF EOF() 
         DO STANDBY WITH 'No existen partidas definidas'
         SET FILTER TO
         sele (malias)
         return &&.f.
      endif
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRiM(CODING)+'.'+ALLTRiM(SUBING)+' '+SUBSTR(DESING,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = IngR.CodIng 
 mDescr = SUBSTR( IngR.DesIng, 1, mLong )

 SET ORDE TO 1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
*  DO CASE
*    CASE mVaRiable=' '   && En edici¢n
*      @ ROW(),mCol   SAY mValor
*      @ ROW(),mCol+7 SAY mDescr
*      RETURN .T.
*    CASE mVaRiable='A'   && En edici¢n SOLO DESCRiPCION
*      @ ROW(),mCol SAY mDescr
*      RETURN
*    CASE mVaRiable='V'   && En vista
*      @ ROW(),COL()  SAY mValor
*      RETURN mDescr
*    CASE mVaRiable='D'   && En vista
*      RETURN mDescr
*    CASE mVaRiable='Z'   && En vista SIN PINTAR
*      RETURN mDescr
*    CASE mVaRiable='C'   && Solo codigo
*      RETURN .T.
*    CASE mVaRiable='T'
*      &mVaRiable = mValor
*      @ ROW(),mCol+7 SAY mDescr
 *     RETURN  mValor
 *   OTHERWISE            && En browse de edici¢n
 mValor = IngR.CodIng 
 mDescr = SUBSTR( IngR.DesIng, 1, mLong )
 SELECT (mAlias)
 REPLACE Iteri.codpart  WITH mValor
 REPLACE Iteri.concep  WITH mdescr

 *     REPLACE codpart WITH mValor
*      replace concep with mdescr
*      CODPART=MVALOR
 *     wCodPart=MVALOR
      RETURN .t. 
*  ENDCASE
*RETURN

 FUNCTION Val_Ing2
*-----------------
  PARAMETERS mValor, Filtro, mVaRiable, mCol, mLong
  PRiVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVaRiable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRi
  ENDCASE
  mAlias  = ALIAS()
  SELECT IngR
  SEEK Filtro+mValor

  IF !FOUND() AND !mVaRiable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
         SET FILTER TO CodIng >= Filtro
      ENDIF
      GO TOP
      IF EOF() 
         DO STANDBY WITH 'No existen partidas definidas'
         SET FILTER TO
         sele (malias)
         return &&.f.
      endif
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRiM(CODING)+'.'+ALLTRiM(SUBING)+' '+SUBSTR(DESING,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = IngR.CodIng 
 mDescr = SUBSTR( IngR.DesIng, 1, mLong )

 SET ORDE TO 1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
 mValor = IngR.CodIng 
 mDescr = SUBSTR( IngR.DesIng, 1, mLong )
 SELECT (mAlias)
 REPLACE Iteri.codpart  WITH mValor
 REPLACE Iteri.concep  WITH mdescr

      RETURN .t. 
RETURN

function val_REV
*-----------------
PARAMETERS nmes,nnum
SELE REVER
SET ORDER TO REVSNUTS1
SEEK ALLTRIM(NMES)+ALLT(NNUM)
IF FOUND()
   WAIT "Error, N£mero de Documento ya existe" WINDOW
   RETURN .F.
ELSE
   RETURN .T.	    	   
ENDIF    

PROCEDURE ComPre
*---------------
AS = ALIAS()
cCodFte = IIF(m.CodFte='09','0201',IIF(m.CodFte='13',"0402",''))
vCtaDeb1 = PADR('8101'+cCodFte,15,"0")
vCtaDeb2 = PADR('8201'+cCodFte,15,"0")
vCtaHab1 = PADR("8201"+cCodFte,15,"0")
vCtaHab2 = PADR("8501"+cCodFte,15,'0')
SELE IteRI
SEEK ALLTRiM(m.NumMes)+ALLTRiM(m.NumREV)
nTot = 0
IF FOUND()
	SCAN WHILE Periodo = m.Periodo AND NumMes = m.NumMes AND NumRev = m.NumREV AND Tipo="P"
		nTot = nTot + ImpParc
	ENDSCAN
ENDIF

SELE AstPre
cOrd = ORDER()
SET ORDER TO astpre5
SEEK ALLTRiM(m.nummes)+ALLTRiM(m.numRev)+"REB"
IF !FOUND()
	FOR i= 1 TO 4
		IF f_appd()
			REPLACE Periodo WITH m.Periodo,;
					nummes  WITH m.nummes,;
					tipdoc  WITH 'REB',;
					numref  WITH m.numRev ,;
					cuenta  WITH IIF(i=1,vctadeb1,IIF(i=2,vCtaDeb2,IIF(i=3,vctaHab1,IIF(i=4,vCtaHab2,SPACE(10))))),;
					tipo    WITH IIF((i=1 OR i=2),'D','H'),;
					fecref  WITH Rever.Fecha,;
					CodCtc  WITH ALLTRiM(m.CodCtc),;
					CodCad  WITH m.CodCad,;
					ctadeb  WITH IIF(i=1,vctadeb1,IIF(i=2,vCtaDeb2,SPACE(10))),;
					ctahab  WITH IIF(i=3,vctaHab1,IIF(i=4,vCtaHab2,SPACE(10))),;
					valdeb  WITH IIF((i=1 OR i=2),nTot,0),;
					valhab  WITH IIF((i=3 OR i=4),nTot,0)
		ENDIF
		UNLOCK
	ENDFOR
 ELSE
	FOR i= 1 TO 4
		IF RLOCK() AND TipDoc='REV' Periodo = m.Periodo AND NumMes = m.NumMes AND NumRev = m.NumREV AND Tipo="P"
			REPLACE Periodo WITH m.Periodo,;
					nummes  WITH m.nummes,;
					tipdoc  WITH 'REB',;
					numref  WITH m.numRev,;
					cuenta  WITH IIF(i=1,vctadeb1,IIF(i=2,vCtaDeb2,IIF(i=3,vctaHab1,IIF(i=4,vCtaHab2,SPACE(10))))),;
					tipo    WITH IIF((i=1 OR i=2),'D','H'),;
					fecref  WITH Rever.Fecha,;
					CodCtc  WITH ALLTRiM(m.CodCtc),;
					CodCad  WITH m.CodCad,;
					ctadeb WITH IIF(i=1,vctadeb1,IIF(i=2,vCtaDeb2,SPACE(10))),;
					ctahab WITH IIF(i=3,vctaHab1,IIF(i=4,vCtaHab2,SPACE(10))),;
					valdeb WITH IIF((i=1 OR i=2),nTot,0),;
					valhab WITH IIF((i=3 OR i=4),nTot,0)
		 ELSE
			* No Modifica Nada
		ENDIF
		SKIP
		UNLOCK
	ENDFOR
ENDIF

SET ORDER TO (cOrd)
SELE (AS)
ACTIVATE WINDOW wind_6
	@ 00,08  SAY 'Cuentas '
	@ 00,22  SAY 'Debe '
	@ 00,38  SAY 'Haber '
	@ 01,04  SAY REPLICATE("=",45)
	@ 02,04  SAY vctadeb1 PICTURE '!!!!!!!!!!!!' 
	@ 02,18  SAY nTot  PICTURE '999,999,999.99' 
	@ 03,12  SAY vctahab1 PICTURE '!!!!!!!!!!!!' 
	@ 03,34  SAY nTot  PICTURE '999,999,999.99' 
	@ 04,04  SAY vctadeb2 PICTURE '!!!!!!!!!!!!' 
	@ 04,18  SAY nTot  PICTURE '999,999,999.99' 
	@ 05,12  SAY vctahab2 PICTURE '!!!!!!!!!!!!' 
	@ 05,34  SAY nTot  PICTURE '999,999,999.99' 
	WAIT ' '
	DEACTIVATE WINDOW wind_6
	vValdeb = nTot  
	vValhab = nTot 
RETURN

***************


PROCEDURE Rever                  &&Programa Reporte usa repprg
*--------------
PARAMETERS xcop
PRIVATE fila
DO CASE
	CASE _Dest="PANTALLA "
		SET DEVICE TO FILE (P_FIL)
	CASE _Dest="IMPRESORA"
		SET DEVICE TO PRINTER
	CASE _Dest="ARCHIVO"
		SET DEVICE TO FILE (P_FIL)
ENDCASE	

SELE Re
SET MEMOWIDTH TO 80
Vper=Re.Periodo
vMes=ALLTRIM(nummes)
vRev=numRev
vcta=ALLTRIM(codctc)
Vkey= ALLTRIM(Re.NumMes)+Re.NumRev+vcta
vfte=ALLT(Re.codfte)

@ 00,10 SAY CHR(27)+CHR(64)
@1,12  SAY CHR(15)
@1,12  SAY ALLTRIM(cia)
@1,114 SAY "P g.:"
@1,122 SAY ALLTRiM(STR(_PAGENO,8))
@2,29  SAY "Rever"
@4,20  SAY CHR(14)
@4,25  SAY "<< REBAJA DE INGRESOS N§ "+NumRev+'.'+NumMes+" >>"
@4,74  SAY CHR(15)
*@2,86  SAY IIF(ALLT(m.tipRi)='50','R E F E R E N C I A L',IIF(Ri.Estado='99','A N U L A D O',' A T E N D I D O'))
@5,59  SAY "Cuenta Corriente: "+vCta

fila=6
@fila,10   SAY "Ú"
@fila,11   SAY REPLICATE("Ä",92)
@fila,103  SAY "Â"
@fila,104  SAY REPLICATE("Ä",25)
@fila,129 SAY "¿"
fila=fila+1
@fila,10   SAY "³"
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,39  SAY "C O M C E P T O"
@fila,103  SAY "³"
@fila,112 SAY "IMPORTE"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "Ã"
@fila,11   SAY REPLICATE("Ä",92)
@fila,103  SAY "Å"
@fila,104  SAY REPLICATE("Ä",25)
@fila,129 SAY "´"
fila=fila+1
@fila,10   SAY "³"
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,13   SAY "SOCIEDAD DE BENEFICENCIA DE PIURA"
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,17   SAY "(A) REVISADO POR :"
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,21  SAY MLINE(concepto,1)
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,21  SAY MLINE(concepto,2)
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,21  SAY MLINE(concepto,3)
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,21  SAY MLINE(concepto,4)
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,21  SAY MLINE(concepto,5)
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,21  SAY MLINE(concepto,6)
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,21  SAY MLINE(concepto,7)
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,21  SAY MLINE(concepto,8)
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1

SELE Iteri
IF SEEK(Re.NumMes+Re.NumRev)
	SCAN WHILE Re.NumMes = NumMes AND Re.NumRev = Numrev
		@fila,10   SAY "³"
		@fila,24  SAY "Partida : " + CodPart
		@fila,49  SAY ImpParc
		@fila,103  SAY "³"
		@fila,129 SAY "³"
		fila=fila+1
	ENDSCAN
ENDIF

SELE Re
@fila,10   SAY "³"
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,21  SAY "Referencia : " + Refer
@fila,103  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "Ã"
@fila,11   SAY REPLICATE("Ä",92)
@fila,103  SAY "Å"
@fila,104  SAY REPLICATE("Ä",25)
@fila,129 SAY "´"
fila=fila+1
@fila,10   SAY "³"
@fila,70  SAY "TOTAL:"
@fila,103  SAY "³"
@fila,108  SAY Monto
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "À"
@fila,11   SAY REPLICATE("Ä",92)
@fila,103  SAY "Á"
@fila,104  SAY REPLICATE("Ä",25)
@fila,129 SAY "Ù"
fila=fila+1

@fila,10   SAY "Ú"
@fila,11   SAY REPLICATE("Ä",118)
@fila,129 SAY "¿"
fila = fila+1
@fila,10   SAY "³"
@fila,14   SAY "Observaciones :"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,30  SAY Observa
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "À"
@fila,11   SAY REPLICATE("Ä",118)
@fila,129 SAY "Ù"
fila=fila+1

@fila,10   SAY "Ú"
@fila,11   SAY REPLICATE("Ä",118)
@fila,129 SAY "¿"
fila = fila+1
@fila,10   SAY "³"
@fila,14   SAY "Lugar y Fecha :"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,25  SAY "Piura, "+ PADL(ALLTRIM(STR(DAY(fecha))),2,'0') + " de " + MFECHA(VAL(nummes)) + " de " + '20'+m.periodo
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "À"
@fila,11   SAY REPLICATE("Ä",118)
@fila,129 SAY "Ù"
fila=fila+1

@fila,10   SAY "Ú"
@fila,11   SAY REPLICATE("Ä",59)
@fila,70  SAY "Â"
@fila,71  SAY REPLICATE("Ä",58)
@fila,129 SAY "¿"
fila=fila+1
@fila,10   SAY "³"
@fila,15   SAY "Firma y sello del Tesorero o Depositante"
@fila,70  SAY "³"
@fila,85  SAY "Firma y Sello del Depositante"
@fila,129 SAY "³"
fila=fila+1

@fila,10   SAY "³"
@fila,70  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,70  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,70  SAY "³"
@fila,129 SAY "³"
fila=fila+1
@fila,10   SAY "³"
@fila,70  SAY "³"
@fila,129 SAY "³"
fila=fila+1

@fila,10   SAY "À"
@fila,11   SAY REPLICATE("Ä",59)
@fila,70  SAY "Á"
@fila,71  SAY REPLICATE("Ä",58)
@fila,129 SAY "Ù"
fila=fila+1

SET DEVICE TO SCREEN 
RETURN

FUNCTION ValPrIng
*----------------
PARAMETERS mValor, mVariable, TipRet
*PARAMETERS vAli, mValor, Nivel, mVariable, TipRet
PRIVATE cAlias,cOrd,vIdx,mRet

cAlias = ALIAS()
vIdx = SYS(3) + '.Idx'
mret = .T.

SELECT ItePari

*set step on
SEEK m.Periodo+'01001'+m.CodCad+m.CodFte+mValor

IF !FOUND() OR EMPTY(mValor)

    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    
    GO TOP
    
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_CAD FROM 04,07 TO 17,70  DOUBLE ;
		TITLE  ' ± CLASE ± ' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 15
	ACTIVATE WINDOWS wind_cad
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH ;
		WINDOW wind_cad  FIELD ;
		CODPart	:H='Partida',;
		aa = ValAsi1(ALIAS(),CodPart,"8","Descri",'R') :H='Descripci¢n'
		
    RELEASE WINDOWS wind_cad
    
*    IF !EMPTY( _OldWnd)
*       ACTIVATE WINDOW &_OldWnd
*    ENDIF
    
    RELEASE POPUP parametro
    SET FILTER TO
ENDIF

mRet = &mvariable

IF !EMPTY(cAlias)
	SELECT (cAlias)
ENDIF

DO CASE
    CASE TipRet=='C'   && Campo
      REPLACE &mVariable WITH mRet
*      RETURN .T.
*    CASE TipRet=='R'   && Retorna valor
*      RETURN mRet
ENDCASE

RETURN 


FUNCTION v_part
*--------------
PARAMETERS vPar
lValida = .F.
IF !EMPTY(vPar)
	IF SEEK(vPar,"CatAsi") AND CatAsi.Detalle = 'S'
		REPLACE Iteri.concep  WITH CatAsi.Descri
	ELSE
		lValida = .T.
	ENDIF
ELSE
	lValida = .T.
ENDIF	

IF lValida
	vPar = SPACE(14)
	= ValPrIng(CodPart,"CodPart",'C')
	vPar = IteParI.CodPart
	SELE CatAsi
	IF SEEK(vPar)
		REPLACE Iteri.concep  WITH CatAsi.Descri
	ELSE
		REPLACE Iteri.concep  WITH 'Error En Partida'
	ENDIF
ENDIF

RETURN .T.

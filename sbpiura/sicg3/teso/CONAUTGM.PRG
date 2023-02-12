* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ CONAUTGM.PRG                                          L:1177 ³	
* ³ Consulta Autorizaci¢n de Giro del MEF a la Regi¢n            ³
* ³ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

SAVE SCREE TO vpanta
*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '
Vmens01 = 'Registro de Autorizaci¢n de Giros del MEF'

*- Abre archivos

USE AutGiro  IN  1 ORDER TAG AutGiro1                 ALIAS AutG
USE IteAut   IN  2 ORDER TAG IteAut1                 ALIAS IteA
USE Clase    IN  3 ORDER TAG Clase1                  ALIAS Clase
USE Parmae   IN  4 ORDER TAG Parmae1                 ALIAS Parma
USE MaePre   IN  6 ORDER TAG MaePre1                 ALIAS MaePre
USE AstPat   IN  7 ORDER TAG AstPat6   				 ALIAS AstPat
USE cuentas  IN  8 ORDER TAG cuentas1                ALIAS cuenta
USE Cajas    IN  9 ORDER TAG Cajas1                  ALIAS Caja
USE MovBco   IN 10 ORDER TAG Movbco1         		 ALIAS Movb
USE SubCtas  IN 11 ORDER TAG Subctas1                ALIAS Subcta

*- Variables de trabajo (registro a trabajar)
PUBLIC vFun,m.fecha,vMes,mmonto,Sw,w_tipctc,w_ctaH

STORE .T. TO Agrega,vFun
m.fecha=DATE()
store 0 to mmonto,Sw
store space(10) to w_ctaH

*- Inicia proceso
SELECT AutG
GO BOTTOM
DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
   ACTIVATE SCREEN
   ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

SHOW POPUP Menu

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anula    Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 12,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_C2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C3 FROM 12,00 TO 23,38 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C4 FROM 00,00 TO 23,79 DOUBLE;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Autorizaci¢n de Giro MEF - Lima' ;
 COLOR SCHEME 10

DEFINE WINDOW wind_3 FROM 20,62 TO 22,78 ;
	TITLE ' TOTAL ' COLOR SCHEME 10  

DEFINE WINDOW wind_7 FROM 13,10 TO 17,70 ;
	TITLE ' CENTRALIZACION DE CAJA ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\Anula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO Lista
 ON SELECTION PAD termi  OF mMenu DO termi

 ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @ 00,02 SAY "      Numero:"  
 @ 00,40 SAY "      Estado:" &&NUAUGI
 @ 01,02 SAY "     Nro Doc:" &&NUMDOC
 @ 01,40 SAY "Nro. de Giro:" &&NUAUGI
 @ 02,02 SAY "       Fecha:" &&FECHA 
 @ 02,40 SAY "     Periodo:" &&PERIODO
 @ 03,02 SAY "         Mes:" &&NUMMES
 @ 04,02 SAY "     Fte.Fto:" &&CODFTE
 @ 05,02 SAY "       Nivel:" &&Tipo INVERSION O FUNCIONAMIENTO
 @ 06,02 SAY "     Sub_Cta:" &&SUBCTA
 @ 07,02 SAY "   Cta.Cte. :" &&CodCTc
 @ 08,02 SAY "  Compromiso:" &&TipComp
 @ 09,02 SAY "        Pago:" &&TipAgo
 @ 10,02 SAY "Tot. Ingreso:" &&TOTAL DE INGRESO
 @ 10,40 SAY "Fecha MovBco:" &&Fecha Movimiento de Bancos
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT AutG
 SCATTER MEMVAR
 @  0,16 SAY m.correl
 @  0,55 SAY IIF(EMPTY(m.nroabo),'Sin Mov.Bco. Autom tico','Con Mov.Bco. Autom tico') color scheme 2
 @  1,16 SAY m.NumDoc
 @  1,55 SAY m.NuAuGi
 @  2,16 SAY m.Fecha
 @  2,55 SAY m.Periodo
 @  3,16 SAY m.NumMes
 @  3,23 SAY Val_Para(m.NumMes,'FECMES','D',3,23)
 @  4,16 SAY m.CodFte
 @  4,20 SAY VAL_PARA(m.CodFte ,'CODFTE','D',7,20)
 @  5,16 SAY m.TipFun
 @  5,18 SAY VAL_PARA(m.tipfun,'TIPFUN','D',5,15)
 @  6,16 SAY m.CodScta
 @  6,32 SAY Val_Fun("SubCta","Subcta","Descri",ALLTRIM(m.CodScta))
 @  7,16 SAY m.CodCtc
 @  7,32 SAY Val_Fun("Caja","Codctc","Descri",ALLTRIM(m.Codctc))
 @  8,16 SAY m.TipComp
 @  9,16 SAY m.TiPAgo
 @ 10,16 SAY m.TotAsig  PICTURE "999,999,999,999.99"
 @ 10,55 SAY m.fecabo 
 DO Vista_Hijo
 DO total
RETURN


PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
 SELECT IteA
 SEEK ALLTRIM(m.nummes)+m.correl
 BROWSE;
   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
   WINDOW Wind_C2 KEY ALLTRIM(m.nummes)+m.Correl;
   FIELDS;
   CodPart  :H='Part'    :P='!!.!!',;
   xx=clase() :H='Descripci¢n' ,;
   MonAsig  :H='Monto'   :P='999,999,999,999.99'
  SELECT  AutG
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT AutG
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 vTemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   Nummes   :H= 'Mes' ,;
   Correl   		 ,;
   NuAuGi   :H= 'Nro.Giro',;
   CodCtc   :H= 'Cta_Cte',;
   CodScta  :H= 'Sub_Cuenta',;
   NumDoc   :H= 'Nro.Doc.',;
   Fecha    :H= 'Fecha',;
   TotAsig  :H='Asignado':P='999,999,999,999.99'
   vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
   DO Logos WITH Rotulo1,vTempo
   IF LASTKEY()=27
      GOTO Vtemp
   ENDIF
   SHOW MENU mMenu
   ON KEY LABEL F10
   DEACTIVATE WINDOW Wind_C4
   DO Vista
RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 SELECT AutG
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 DEFINE WINDOW WBusca FROM 10,12 TO 14,68 DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW WBusca
 STORE SPACE(02) TO vmesb
 STORE SPACE(04) TO Vbusca
 @ 0,43 say "."
 @ 0, 2 SAY 'N£mero de documento que desea buscar:'
 @ 0,41 GET Vmesb PICTURE '!!'
 @ 0,44 GET Vbusca PICTURE '!!!!'
 READ
 RELEASE WINDOW WBusca
 IF LASTKEY()=27
    SELECT AutG
    RETURN
 ENDIF
IF !SEEK(ALLTRIM(Vmesb)+Vbusca)
   DO standby WITH "Documento no encontrado."
   GOTO Vtemp
ELSE
   DO Vista
ENDIF
RETURN


PROCEDURE Anter              && Documento anterior
*--------------
 SELECT AutG
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF

 IF !BOF()
    SKIP -1
 ENDIF

 IF BOF()
    GO TOP
    DO standby WITH "No existe Documento anterior."
 ELSE
    DO Vista
 ENDIF

RETURN


PROCEDURE Proxi              && Documento siguiente
*--------------
 SELECT AutG
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF

 IF !EOF()
    SKIP
 ENDIF

 IF EOF()
    DO standby WITH "No existe Documento siguiente."
    GO BOTTOM
 ELSE
    DO Vista
 ENDIF
RETURN

FUNCTION Tra_Hijo
*----------------
 vFun=.t.
 HIDE MENU mMenu
 ACTIVATE SCREEN
 VTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO agreg_item
 ON KEY LABEL F8  DO elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
 STORE 0 TO m.MonAsig,AntCan,vSuma
 SELECT IteA
 GO TOP
 SEEK ALLTRIM(m.nummes)+m.correl
 IF !FOUND()
    DO Agreg_Item
 ENDIF

 BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
  KEY ALLTRIM(m.nummes)+m.correl;
  FIELDS;
   CodPart  : H='Part'    :V=VAL_PART(RIGHT(CodPart,2),LEFT(CodPart,2),'codpart'):F:P='!!.!!',;
   xx=clase() :50 :H='Descripci¢n',;
   MonAsig  : H='      Monto      ':P='999,999,999,999.99'
 vOrd=ORDER()
 SELECT IteA
 SET ORDER TO vOrd
 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5
 UNLOCK ALL
SELECT AutG
ACTIVATE SCREEN
vTempo = ' Ingresa  Elige  Busca  Anterior  Siguiente  Corrige  Anula  Lista Termina '
DO Logos WITH Rotulo1,vtempo
SHOW MENU mMenu
RETURN  vFun


PROCEDURE Agreg_item
*-------------------
SELECT IteA
IF F_Appd()
   REPLACE Nummes   WITH m.Nummes ,;
           Correl   WITH m.correl ,;
           NuAuGi   WITH m.NuAuGi ,;
           Fecha    WITH m.Fecha  ,;
           CodScta  WITH m.CodScta ,;
           CodCtc   WITH m.CodCtc
ENDIF
UNLOCK
RETURN


PROCEDURE Elimi_item
*-------------------
SELECT IteA
IF RLOCK()
   DELETE NEXT 1
ELSE
   DO StandBy WITH 'No puede eliminar este Item.'
ENDIF
RETURN


PROCEDURE regabo  
*---------------
* Registra Nota de Abono Autom tico a la SubCuenta
SELE movb
IF ingreso and sw=0
	IF F_Appd()
	   REPLACE periodo WITH m.periodo,;
	           nummes WITH PADL(MONTH(m.fecabo),2,'0'),;
	           numero WITH m.nroabo,;
	           codctc WITH m.codctc,;
	           codban WITH '002' ,;
			   transa WITH 'ABO',;
			   fecha WITH m.fecabo,;
			   motivo WITH '06',;
			   tipdoc WITH 'N/A',;
			   numref WITH m.numdoc,;
			   tipfun WITH m.tipfun,;
			   codfte WITH m.codfte,;
			   nombre WITH 'AUT. GIRO TESORO PUBLICO',;
			   monto WITH m.totasig,;
			   estado WITH '00',;
			   codcal WITH ALLTRIM(m.periodo)+m.nummes+m.codfte+'01001',;
			   nummesC WITH m.nummes,;
			   fechaC WITH m.fecha
	ENDIF
	UNLOCK ALL
ELSE	
    SEEK ALLTRIM(m.nummes+m.nroabo)
    IF FOUND()
	   REPLACE periodo WITH m.periodo,;
	   	       nummes WITH PADL(MONTH(m.fecabo),2,'0'),;
	   	       numero WITH m.nroabo,;
	   	       codctc WITH m.codctc,; 
	   	       codban WITH '002' ,;
	   		   transa WITH 'ABO',;
	   		   fecha WITH m.fecabo,;
	   		   motivo WITH '06',;
	   		   tipdoc WITH 'N/A',;
	   		   numref WITH m.numdoc,;
	   		   tipfun WITH m.tipfun,;
	   		   codfte WITH m.codfte,;
	   		   nombre WITH 'AUT. GIRO TESORO PUBLICO',;
	   		   monto WITH m.totasig,;
	   		   estado WITH '00',;
	   		   codcal WITH ALLTRIM(m.periodo)+m.nummes+m.codftE+'01001',;
	           nummesC WITH m.nummes,;
	           fechaC WITH m.fecha
	ENDIF   
ENDIF	
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
SEEK ALLT(m.Nummes)+ALLTRIM(m.Nroabo)+ALLTRIM(m.Codctc)
IF !FOUND()
	DO agrite4
ENDIF
BROWSE FIELDS ;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999,999,999.99';
	WINDOW wind_2 KEY allt(m.nummes)+ALLTRIM(m.Nroabo)+ALLTRIM(m.Codctc)
STORE 0 TO vdebe, vhaber ,vRet
SEEK allt(m.nummes)+m.Nroabo+m.Codctc
SCAN WHILE allt(m.nummes)=NumMes AND ALLTRIM(m.Nroabo)=NumREF AND ALLTRIM(m.Codctc)=CodCtC
     IF IIF(TipCta='D',MtoDeb,MtoHab)=0
    	DELE NEXT 1
     ELSE
	 	vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
     ENDIF
ENDSCAN
IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
ON KEY

SELECT AutG

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
	REPLACE nummes WITH allt(m.nummes),;
	        numref WITH m.nroabo,;
	        Periodo WITH m.periodo,;
	        tipdoc WITH 'N/A',;
	        codctc WITH m.codctc,;
	        fecha  WITH m.Fecha,;
	        tipctc WITH w_tipctc
ENDIF
RETURN

PROCEDURE agrite4
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH allt(m.nummes),;
	        numref WITH m.nroabo,;
	        Periodo WITH m.periodo,;
	        tipdoc WITH 'N/A',;
	        codctc WITH m.codctc,;
	        fecha  WITH m.Fecha,;
	        tipctc WITH w_tipctc,;
	        codcta WITH w_ctah,;
	        tipcta WITH 'D',;
	        mtodeb WITH m.totasig
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(m.nummes),;
	        numref WITH m.nroabo,;
	        Periodo WITH m.periodo,;
	        tipdoc WITH 'N/A',;
	        codctc WITH m.codctc,;
	        fecha  WITH m.Fecha,;
	        tipctc WITH w_tipctc,;
	        codcta WITH IIF(m.tipfun='I','3070101000','7510101000'),;
	        tipcta WITH 'H',;
	        mtoHab WITH m.totasig
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(m.nummes),;
	        numref WITH m.nroabo,;
	        Periodo WITH m.periodo,;
	        tipdoc WITH 'N/A',;
	        codctc WITH m.codctc,;
	        fecha  WITH m.Fecha,;
	        tipctc WITH w_tipctc,;
	        codcta WITH '1010101000',;
	        tipcta WITH 'D',;
	        mtodeb WITH m.totasig
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(m.nummes),;
	        numref WITH m.nroabo,;
	        Periodo WITH m.periodo,;
	        tipdoc WITH 'N/A',;
	        codctc WITH m.codctc,;
	        fecha  WITH m.Fecha,;
	        tipctc WITH w_tipctc,;
	        codcta WITH '1010101000',;
	        tipcta WITH 'H',;
	        mtohab WITH m.totasig
ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT astpat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN


PROCEDURE Termi
*--------------
 vEn_accion = .F.
 DEACTIVATE MENU
RETURN


PROCEDURE Fin_opcion
*-------------------
 DO Logos WITH Rotulo1,Rotulo2
 RELEASE    WINDOW Wind_C0
 RELEASE    WINDOW Wind_C2
 RELEASE    WINDOW wind_3
 RELEASE    MENU   mMenu
 CLOSE DATA
 RESTORE SCREEN FROM vpanta
RETURN


PROCEDURE Lista
*--------------
 SELECT AutG
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 STORE SPACE(2) TO vnummes
 STORE SPACE(4) TO vcorrel 
 rec = RECNO()
 SCATTER MEMVAR
 vnummes = m.nummes
 vcorrel = m.correl
 vcodscta= m.codscta
 vtiplis = 1
DEFINE WINDOW lis FROM 4,10 TO 19,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 00,01 SAY "     Tipo Listado : "  GET vtiplis FUNCTION '^ Documento;Cdro.Demost. X Subcta.;Cdro.Demos. de SubCta.'
@ 06,01 SAY "     N§ Documento : "
@ 06,24 SAY "."
@ 06,22 GET vnummes  WHEN vtiplis = 1 PICTURE "!!"
@ 06,25 GET vcorrel  WHEN vtiplis = 1 PICTURE "!!!!" VALID ValAut()
@ 08,01 SAY "       Sub Cuenta : " 
@ 08,22 GET vcodscta WHEN vtiplis = 3
@ 13,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
xmes=val(vnummes)
xAno =VAL(m.Periodo)
Xali = ALIAS()
vord1 = Order()
RELEASE WINDOW lis
IF okcancel = 1
   DO CASE
   	CASE vtiplis = 1
	   SELE AUTG
	   SET RELATION TO Allt(NumMes)+correl INTO itea
	   SET FILT TO allt(nummes)+Correl=ALLTRIM(vnummes)+vcorrel
	   SET SKIP TO ITEA
	   SEEK ALLTRIM(vnummes)+vcorrel
	   IF FOUND()
		  DO REPORTE WITH 2,"RepAutLi"," Recepci¢n de Giros del MEF ",1,.F.,.T.
	   ELSE
    	  DO STANDBY WITH vmens08	  
	   ENDIF
	   SET SKIP TO
	   SET FILT TO
	   SET RELA TO
	CASE vtiplis = 2
	   SELE ITEA
	   xOrd = ORDER()
	   SET ORDER TO Iteaut3
	   GO TOP
	   IF !EOF()
		  DO REPORTE WITH 2,"CuaAutLi"," Recepci¢n de Giros del MEF ",1,.F.,.T.
	   ELSE
    	  DO STANDBY WITH vmens08	  
	   ENDIF
	   SET ORDER TO (xord)
	CASE vtiplis = 3
	   SELE ITEA
	   xOrd = ORDER()
	   SET ORDER TO Iteaut3
	   SET FILT TO Codscta = ALLTRIM(vcodscta)
	   GO TOP
	   IF !EOF()
		  DO REPORTE WITH 2,"CuaAutLi"," Recepci¢n de Giros del MEF ",1,.F.,.T.
	   ELSE
    	  DO STANDBY WITH vmens08	  
	   ENDIF
	   USE itaure   IN 0  ALIAS Itaure
	   vind1 = SYS(3)+'.IDX'
	   SELECT Itaure
	   INDEX ON Ctcofg+nummes TO (vind1) FOR Codscta = ALLTRIM(vcodscta)
	   GO TOP
	   IF !EOF()
		  DO REPORTE WITH 2,"CuAuLiOf"," Recepci¢n de Giros del MEF ",1,.F.,.T.
	   ELSE
    	  DO STANDBY WITH vmens08	  
	   ENDIF
	   SET INDEX TO
	   USE
	   SELECT Itea
	   SET ORDER TO (xord)
    ENDCASE		
ENDIF      	   
SELECT (Xali)
SET ORDER TO (vord1)
IF rec > 0
   GO rec
ENDIF
DO vista
RETURN


FUNCTION Clase
*-------------
PUBLIC _y
STORE ' ' TO _y
vAli=ALIAS()
vOrd=ORDER()
vCodPart=LEFT(IteA.CodPart,2)
vCodSubP=RIGHT(IteA.CodPart,2)
SELECT Clase
SET ORDER TO Clase1
SEEK vCodPart+vCodSubP
IF FOUND()
   _y=LEFT(Clase.DesPar,50)
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN _y


FUNCTION Posi
*------------
vOrd=ORDER()
SELECT MEFC
SET RELATION TO 
SET ORDER TO MEFCal1
SEEK ALLTRIM(m.Periodo)+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(m.CodPrg)+ALLTRIM(m.TipFun)+ALLTRIM(m.CodPart)
IF FOUND()
   REPLACE MEFC.GirMEF WITH m.MonAsig
ENDIF   
SELECT IteA
SET ORDER TO vOrd
RETURN


FUNCTION Suma
*------------
IF MONTH(m.Fecha) # MONTH(DATE())
   DO StandBy WITH "Mes no invalido para la operaci¢n"
   RETURN
ENDIF
vOrd=ORDER()
SELECT Calen
IF IteA.MonAsig # AntCan
   IF EMPTY(IteA.CodAct)
      SET ORDER TO Calen7
      SEEK IteA.CodPrg+IteA.CodFte+IteA.CodSubpr+IteA.CodProy+IteA.CodSuPry+IteA.CodPart
      IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
         IF IteA.MonAsig > AntCan
            REPLACE Calen.GirMEF WITH Calen.GirMEF  + (IteA.MonAsig - AntCan)
            IF Calen.GirMEF < Calen.ValPart
               DO Standby WITH "El Monto es inferior al Aprobado"
            ELSE
               DO Standby WITH "ERROR!... El giro excede el Total Aprobado"
            ENDIF
            vSuma = vSuma + IteA.MonAsig
            IF vSuma > m.TotAsig
               SEEK IteA.CodPrg+IteA.CodFte+IteA.CodSubpr+IteA.CodProy+IteA.CodSuPry+IteA.CodPart
               IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
                  REPLACE Calen.GirMEF  WITH Calen.GirMEF - (IteA.MonAsig - AntCan)
                  REPLACE IteA.MonAsig   WITH 0
                  vSuma = vSuma - IteA.MonAsig
               ENDIF
               DO Standby WITH "ERROR!!!. Suma excede el Total del Credito"
            ENDIF
         ELSE
            REPLACE Calen.GirMEF  WITH Calen.GirMEF - (AntCan - IteA.MonAsig)
            IF Calen.GirMEF < Calen.ValPart
               DO Standby WITH "El Monto es inferior al Aprobado"
            ELSE
               DO Standby WITH "ERROR!... El giro excede el Total Aprobado"
            ENDIF
            vSuma = vSuma + IteA.MonAsig            
            IF vSuma > m.TotAsig
               SEEK IteA.CodPrg+IteA.CodFte+IteA.CodSubpr+IteA.CodProy+IteA.CodSuPry+IteA.CodPart
               IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
                  REPLACE Calen.GirMEF WITH Calen.GirMEF  + (AntCan - IteA.MonAsig)
                  REPLACE IteA.MonAsig WITH 0
                  vSuma = vSuma + IteA.MonAsig                  
               ENDIF
               DO Standby WITH "ERROR!!!. Suma excede el Total del Credito"
            ENDIF
         ENDIF
      ENDIF
   ELSE
      SET ORDER TO Calen8
      SEEK IteA.CodPrg+IteA.CodFte+IteA.CodSubpr+IteA.CodAct+IteA.CodPart
      IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
         IF IteA.MonAsig > AntCan
            REPLACE Calen.GirMEF WITH Calen.GirMEF + (IteA.MonAsig - AntCan)
            IF Calen.GirMEF < Calen.ValPart
               DO Standby WITH "El Monto es inferior al Aprobado"
            ELSE
               DO Standby WITH "ERROR!... El giro excede el Total Aprobado"
            ENDIF
            vSuma = vSuma + IteA.MonAsig            
            IF vSuma > m.TotAsig
               SEEK IteA.CodPrg+IteA.CodFte+IteA.CodSubpr+IteA.CodAct+IteA.CodPart
               IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
                  REPLACE Calen.GirMEF WITH Calen.GirMEF - (IteA.MonAsig - AntCan)
                  REPLACE IteA.MonAsig WITH 0
                  vSuma = vSuma - IteA.MonAsig                  
               ENDIF
               DO Standby WITH "ERROR!!!. Suma excede el Total del Credito"
            ENDIF
         ELSE
            REPLACE Calen.GirMEF WITH Calen.GirMEF - (AntCan - IteA.MonAsig)
            IF Calen.GirMEF < Calen.ValPart
               DO Standby WITH "El Monto es inferior al Aprobado"
            ELSE
               DO Standby WITH "ERROR!... El giro excede el Total Aprobado"
            ENDIF
            vSuma = vSuma + IteA.MonAsig            
            IF vSuma > m.TotAsig
               SEEK IteA.CodPrg+IteA.CodFte+IteA.CodSubpr+IteA.CodAct+IteA.CodPart
               IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
                  REPLACE Calen.GirMEF WITH Calen.GirMEF + (AntCan - IteA.MonAsig)
                  REPLACE IteA.MonAsig WITH 0                  
                  vSuma = vSuma - IteA.MonAsig                  
               ENDIF
               DO Standby WITH "ERROR!!!. Suma excede el Total del Credito"
            ENDIF
         ENDIF
      ENDIF
   ENDIF
ENDIF
SELECT IteA
SET ORDER TO vOrd
RETURN


FUNCTION Val_Cale1
*-----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong
  PRIVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
  ENDCASE
  mAlias  = ALIAS()
  SELECT Parma
  SEEK Filtro+mValor
  IF (!FOUND() OR EMPTY(mValor)) .AND. !EMPTY(Calen.CodPart)
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      SET FILTER TO Tipo+Codigo = Filtro
      GO TOP
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRIM(CODIGOAUX)+' '+SUBSTR(Descri,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = Parma.CodigoAux
 mDescr = SUBSTR( Parma.Descri, 1, mLong )
 SET ORDE TO 1
 IF !EMPTY( mAlias )
   SELECT (mAlias)
 ENDIF
 REPLACE &mVariable WITH mValor
 RETURN .T.


FUNCTION BuscPrg &&Buscar Actividad
*---------------
PRIVATE vKey
vKey=ALLTRIM(IteA.Codprg)+ALLTRIM(IteA.CodSubPr)+ALLTRIM(IteA.CodProy)
vAli=ALIAS()
vOrd=ORDER()
SELE MaePre
SET ORDER TO MaePre4
SEEK vKey
vProyecto = MaePre.Descri
SELECT (vAli)
SET ORDER TO TAG vOrd
RETURN vProyecto


FUNCTION Ubica
*-------------
vAli=ALIAS()
vOrd=ORDER()
SELECT Calen
SET ORDER TO Calen5
IF EMPTY(IteA.CodSuPry)
   SEEK IteA.CodPrg+IteA.CodFte+IteA.CodSubPr+IteA.CodProy+;
        IteA.CodSuPry+IteA.CodPart
   IF FOUND()
      RETURN Calen.UbiCac
   ENDIF
ELSE
   IF !EMPTY(IteA.codsubpr)
      SEEK IteA.CodPrg+IteA.CodFte+IteA.CodSubPr+IteA.CodProy+;
      IteA.CodSuPry+IteA.CodPart
      IF FOUND()
         RETURN Calen.ubicac
      ENDIF
   ENDIF
ENDIF
SELECT (vAli)
SET ORDER TO TAG vOrd


FUNCTION Valida && Suma el ingreso de todos los Items ingresados.
*--------------
vAli=ALIAS()
vOrd=ORDER()
SELECT IteA
vNumMES=ALLTRIM(IteA.NumMES)
vcorrel=ALLTRIM(IteA.Correl)
vAcum=0
SEEK vnummes+vcorrel
SCAN WHILE vNummes=ALLTRIM(IteA.Nummes) .AND. vcorrel=ALLTRIM(IteA.correl) 
     vAcum = vAcum + IteA.MonAsig
     IF vAcum > m.TotAsig
        DO StandBy WITH "ERROR!, la suma excede lo asignado. Proceda a Corregir"
     ENDIF
ENDSCAN
RETURN


FUNCTION REMBYS
*--------------
vAli=ALIAS()
vOrd=ORDER()
vSubCta=LEFT(AutG.CodSCta,1)
SELECT Parma
RETURN


FUNCTION Descri
*--------------
PUBLIC aa
aa = IIF(!EMPTY(CODPART),VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'D'),' ')
RETURN aa


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)


PROCEDURE VALFECHA
*-----------------
an=right(str(year(date()),4),2)
vMe = val(m.Nummes)+1
me = padl(alltrim(str(vMe,2)),2,'0')
vFec = '01/&ME/&AN'
m.Fecha = ctod(vFec) - 1
RETURN .T.

FUNCTION EXISTE
*--------------
vAli=ALIAS()
vOrd=ORDER()
SELECT AutG
SEEK ALLTRIM(NumDoc)+correl
IF FOUND()
   DO StandBy WITH "El documento ya EXISTE"
   SELECT (vAli)
   SET ORDER TO vOrd
   RETURN .F.
ELSE
   SELECT (vAli)
   SET ORDER TO vOrd
   RETURN
ENDIF

PROCEDURE ValAut
*--------------
 SELECT Autg
 vTemp=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK Vnummes+Vcorrel
 IF !FOUND()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   Nummes   :H= 'Mes' ,;
   Correl  			 ,;
   NuAuGi   :H= 'Nro.Giro',;
   CodScta  :H= 'Sub_Cuenta',;
   NumDoc   :H= 'Nro.Doc.',;
   Fecha    :H= 'Fecha',;
   TotAsig  :H='Asignado':P='999,999,999,999.99'
   vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
   DO Logos WITH Rotulo1,vTempo
   IF LASTKEY()=27
      GOTO Vtemp
   ENDIF
   SHOW MENU mMenu
   ON KEY LABEL F10
   DEACTIVATE WINDOW Wind_C4
   GO Vtemp
 ENDIF
 vnummes= NumMes
 vcorrel= correl
 vcodctc= codctc
 RETURN .T.

FUNCTION vctc
*------------
SELE subcta
m.codctc=subcta.ctarec
RETURN

PROCEDURE total
*--------------
 xtot=0
 SELE Itea
 SEEK ALLTRIM(m.nummes)+m.correl
 SCAN WHILE Itea.nummes=ALLT(m.nummes) AND Itea.correl=m.correl 
 	  xtot=xtot+Itea.monasig 
 ENDSCAN	  
 ACTIVATE WINDOW wind_3
 @ 0,1 SAY xtot PICTURE '999,999,999.99'
 RETURN
 
 FUNCTION wfec
*------------
m.nummes=PADL(MONTH(m.fecha),2,'0')
RETURN
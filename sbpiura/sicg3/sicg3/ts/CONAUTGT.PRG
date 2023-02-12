* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ ConAutgT.PRG                                        L: 1624  ³	
* ³ Consulta Autorizaci¢n de Giro de la Regi¢n                   ³
* ³ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Mensajes de aviso al usuario
PUBLIC Ingreso
PRIVATE Vmens01,Vmens02,Vmens09
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '
Vmens01 = 'Registro de Autorizaci¢nes de Giro a las Ofc. Giradoras' 

*- Abre archivos

USE AuGiRe   IN  1 ORDER TAG AuGiRe1                 ALIAS AutG
USE ItAuRe   IN  2 ORDER TAG ItAuRe1                 ALIAS IteA
USE Clase    IN  3 ORDER TAG Clase1                  ALIAS Clase
USE Parmae   IN  4 ORDER TAG Parmae1                 ALIAS Parma
USE Calen    IN  5 ORDER TAG Calen1                  ALIAS Calen
USE MaePre   IN  6 ORDER TAG MaePre1                 ALIAS MaePre
USE cuentas  IN  8 ORDER TAG cuentas1                ALIAS cuenta
USE Cajas    IN  9 ORDER TAG Cajas1                  ALIAS Caja
USE MovBco   IN 10 ORDER TAG Movbco11        		 ALIAS Movb
USE SubCtas  IN 11 ORDER TAG Subctas1                ALIAS Subcta

*- Variables de trabajo (registro a trabajar)
PUBLIC vFun,m.fecha,xMes,xAno,sw,W_tipctc,w_ctah,w_prog


SELECT Autg
SCATTER MEMVAR BLANK

STORE .T. TO Agrega,vFun
m.fecha=DATE()
store 0 to sw
STORE SPACE(10) TO w_ctah
STORE SPACE(2) TO w_prog
*- Mes y a¤o actual
SELECT parma
SEEK "MESANO"+"ACTUAL"
_actmes = LEFT(descri,6)
actmes = CTOD("01/"+SUBSTR(_actmes,5,2)+"/"+SUBSTR(_actmes,3,2) )

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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 12,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10 

 DEFINE WINDOW Wind_C2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C3 FROM 11,00 TO 23,38 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C4 FROM 00,00 TO 23,79 DOUBLE;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 11,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Autorizaci¢n de Giro Ofc.Giradora' ;
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
 DEFINE PAD elimi   OF mMenu PROMPT '\Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO LISTA
 ON SELECTION PAD termi  OF mMenu DO termi
*ON KEY LABEL f4 DO gen_movb
 ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @ 0, 1 SAY "     N§ Giro :"
 @ 0,40 SAY "     N§ Doc. :"
 @ 1, 1 SAY "     Periodo :"
 @ 1,40 SAY "       Fecha :"
 @ 2, 1 SAY "N§ Carta Ref :"
 @ 2,40 SAY "      Estado :"
 @ 3, 1 SAY "     Sub Cta :"
 @ 4, 1 SAY "Cta.Cte.Reg. :"
 @ 5, 1 SAY "Ofc.Giradora :"
 @ 6, 1 SAY "Ctc.Cte.Gir. :"
 @ 7, 1 SAY "     Fte.Fto :"
 @ 7,40 SAY "       Nivel :"
 @ 8, 1 SAY "  Compromiso :"
 @ 8,40 SAY "        Pago :"
 @ 9, 1 SAY "Tot. Ingreso :"
 @ 9,40 SAY "Fecha Abono  :"

RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT AutG
 SCATTER MEMVAR
 @  0,16 SAY m.nuaugi
 @  0,56 SAY m.NumDoc
 @  1,16 SAY m.Periodo
 @  1,20 SAY m.NumMes
 @  1,23 SAY Val_Para(m.NumMes,'FECMES','D',1,23)
 @  1,56 SAY m.Fecha
 @  2,16 SAY m.CarRef
 @  2,56 SAY IIF(m.estbco='û' AND !EMPTY(Fecabo),'OK Mov.Bco  Autom tico','Sin Mov.Bco Autom tico') COLOR SCHEME 2
 @  3,16 SAY m.CodScta
 @  3,32 SAY Val_Fun("SubCta","Subcta","Descri",ALLTRIM(m.CodScta))
 @  4,16 SAY m.Codctc
 @  4,32 SAY Val_Fun("Caja","CodCtc","Descri",m.Codctc)
 @  5,16 SAY m.CodOfg
 @  5,20 SAY Val_Para(m.CodOfG,'OFIGIR','D')
 @  6,16 SAY m.Ctcofg
 @  6,32 SAY Val_Fun("Caja","CodCtc","Descri",m.CtcOfg)
 @  7,16 SAY m.CodFte
 @  7,20 SAY VAL_PARA(m.CodFte ,'CODFTE','D',7,20)
 @  7,56 SAY m.TipFun
 @  7,58 SAY VAL_PARA(m.tipfun,'TIPFUN','D',7,15)
 @  8,16 SAY m.TipComp
 @  8,56 SAY m.TiPAgo
 @  9,16 SAY m.TotAsig  PICTURE "999,999,999,999.99"
 @  9,56 SAY m.Fecabo   PICTURE "@E"
 DO Vista_Hijo
 DO total
RETURN

PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
 SELECT IteA
 SEEK ALLTRIM(m.Nummes)+ALLTRIM(m.Numdoc)
 BROWSE;
   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
   WINDOW Wind_C2 KEY ALLTRIM(m.Nummes)+ALLTRIM(m.Numdoc);
   FIELDS;
   CodPrg   :H='Prg'     :P='!!',;
   CodPart  :H='Part'    :P='!!.!!',;
   xx=clase() :46 :H='Descripci¢n',;
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
   Nummes   :H= 'Mes',;
   Numdoc   :H= 'Nro.Doc.',;
   CodCtc   :H= 'Cta_Cte',;
   CodScta   :H= 'Sub_Cuenta',;
   CtcOfg   :H= 'Cta_Ofg',;
   TotAsig  :H='Asignado':P='999,999,999,999.99',;
   CarRef   :H='Carta. Ref.'
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
IF !SEEK(ALLTRIM(Vmesb)+ALLTRIM(Vbusca))
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
 SEEK ALLTRIM(m.NumMES)+ALLTRIM(m.Numdoc)
 IF !FOUND()
    DO Agreg_Item
 ENDIF

 BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
  KEY ALLTRIM(m.NumMES)+ALLTRIM(m.Numdoc);
  FIELDS;
   CodPart  : H='Part'    :V=VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'codpart') :F:P='!!.!!',;
   xx=clase() :50 :H='Descripci¢n',;
   MonAsig  : H='      Monto       ':P='999,999,999,999.99'

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
           Numdoc   WITH m.Numdoc ,;
           NuAuGi   WITH m.NuAuGi ,;
           Fecha    WITH m.Fecha  ,;
           CodScta  WITH m.CodScta,;
           CodOfg   WITH m.CodOfg ,;
           Codctcen WITH m.Codctcen,;
           Codctc   WITH m.Codctc,;
           Ctcofg   WITH m.Ctcofg ,;
           Codfte   WITH m.codfte,;
           codprg   WITH w_prog
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
 RESTORE SCREEN FROM PRINCIPAL
RETURN

PROCEDURE LISTA
*--------------
 SELECT AutG
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
vnumdoc = ALLT(m.numdoc)
vtiplis = 1
xAno = VAL(M.Periodo)
mmes  = m.nummes
vfecini = DATE()
vfecfin = DATE()
vcodctc = SPACE(14)

SET UDFPARMS TO REFERENCE 
DEFINE WINDOW lis FROM 4,10 TO 18,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 01,01 SAY "  Tipo Listado : "  GET vtiplis FUNCTION '^ Documento;Resumen x Cta.Cte.'
@ 05,01 SAY "  N§ Documento : "
@ 05,19 GET mmes     WHEN vtiplis = 1 VALID Val_Para(mMes,'FECMES',' ',24)
@ 05,26 GET vnumdoc  WHEN vtiplis = 1 PICTURE "!!!!" VALID ValAut(vnumdoc,mmes)
@ 07,01 SAY "      Cta.Cte. : "  GET vcodctc  WHEN vtiplis = 2  VALID ;
		Val_Fun('Caja','CodCtc','codctc+descri',vCodctc,1,8,19,'descri',27)
@ 09,01 SAY "  Rango Fechas : "  GET vfecini  WHEN vtiplis = 2 PICTURE "@D"
@ 09,27 SAY "  Al  " GET vfecfin  WHEN vtiplis = 2 PICTURE "@D"
@ 11,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW lis
 IF LASTKEY()=27
    RETURN
 ENDIF

IF okcancel = 1
 DO CASE
    CASE vTiplis=1
 		 xMes = VAL(mMes)
         SELECT AutG
         rec  = RECNO()
         SELECT IteA
         vord = ORDER()
         vrec = RECNO()
         SET ORDER TO TAG ItAuRe1
         SET FILTER TO numdoc = vnumdoc
         DO REPORTE WITH 2,"RepAutOF"," Autorizaciones de Giros a las Ofc. Giradoras ",1,.F.,.T.
         DO REPORTE WITH 2,"AuGireBc"," Autorizaciones de Giros al Banco de la Naci¢n "
		 SELECT Itea
		 SET FILTER TO 
		 SET ORDER TO (vord)
         SELECT Autg
         GO rec
     CASE vtiplis = 2
         SELECT Itea 
         vord = ORDER()
         vrec = RECNO()
         vind = SYS(3)+'.idx'
         INDEX ON ctcofg TO (vind)  FOR BETWEEN(Fecha,vfecini,vfecfin) AND  ;
         								ctcofg = vcodctc
         GO TOP
         DO REPORTE WITH 2,"ReAuGR"," Resumen de Autorizaciones de Giro a las Ofc. Giradoras ",1,.F.,.T.
         SELECT Itea 
         SET INDEX TO
		 SET ORDER TO (vord)
		 SELECT Autg
 ENDCASE 
ENDIF
 DO vista
RETURN


FUNCTION Valida && Suma el ingreso de todos los Items ingresados.
*--------------
vAli=ALIAS()
vOrd=ORDER()
SELECT IteA
vNumMES=ALLTRIM(IteA.NumMES)
vNuAuGi=ALLTRIM(IteA.NuAuGi)
vnuscta=ALLTRIM(Itea.Codscta)
vnumdoc=ALLTRIM(m.numdoc)
vAcum=0
SEEK vnummes+vnumdoc
SCAN WHILE vNummes=ALLTRIM(IteA.Nummes) .AND. vNumdoc=ALLTRIM(IteA.Numdoc) 
     vAcum = vAcum + IteA.MonAsig
     IF vAcum > m.TotAsig
        DO StandBy WITH "ERROR!, la suma excede lo asignado. Proceda a Corregir"
     ENDIF
ENDSCAN
RETURN


FUNCTION Descri
*--------------
PUBLIC aa
aa = IIF(!EMPTY(CODPART),VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'D'),' ')
RETURN aa

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
   _y=LEFT(Clase.DesPar,46)
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN _y

FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)

FUNCTION vctc
*------------
SELECT subcta
m.codctc   = subcta.ctaent
m.codctcen = subcta.ctarec
RETURN .T.


PROCEDURE ValAut
*--------------
 PARAMETERS vnum,xmes
 SELECT Autg
 vTemp=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET EXACT ON
 SEEK ALLT(xmes)+ALLT(vnum)
 IF !FOUND()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   Nummes   :H= 'Mes' ,;
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
 ENDIF
 GO Vtemp
 vnum = autg.Numdoc
 SET EXACT OFF
 RETURN .T.

PROCEDURE regabogi  
*---------------
* Registra Nota de Abono Autom tico a la Oficina Giradora
SELE movb
IF ingreso and sw=0
	IF F_Appd()
	   REPLACE periodo WITH m.periodo,;
	           nummes  WITH PADL(MONTH(m.fecabo),2,'0'),;
	           numero  WITH m.aboctcofg,;
	           codctc  WITH m.ctcofg,;
	           codban  WITH '002' ,;
			   transa  WITH 'ABO',;
			   fecha   WITH m.fecabo,;
			   motivo  WITH '09',;
			   tipdoc  WITH 'N/A',;
			   numref  WITH m.numdoc,;
			   tipfun  WITH m.tipfun,;
			   codfte  WITH m.codfte,;
			   nombre  WITH 'AUT.GIRO R.G :' + Val_Para(m.codofg,'OFIGIR','D'),;
			   monto   WITH m.totasig,;
			   estado  WITH '00',;
			   codcal  WITH ALLTRIM(m.periodo)+m.nummes+m.codfte+'01001',;
			   nummesC WITH m.nummes,;
			   fechaC  WITH m.fecha
	ENDIF
	UNLOCK ALL
ELSE	
    SEEK ALLTRIM(m.nummes+m.aboctcofg)
    IF FOUND()
	   REPLACE periodo WITH m.periodo,;
	   	       nummes WITH PADL(MONTH(m.fecabo),2,'0'),;
	   	       numero WITH m.aboctcofg,;
	   	       codctc WITH m.ctcofg,; 
	   	       codban WITH '002' ,;
	   		   transa WITH 'ABO',;
	   		   fecha WITH m.fecabo,;
	   		   motivo WITH '09',;
	   		   tipdoc WITH 'N/A',;
	   		   numref WITH m.numdoc,;
	   		   tipfun WITH m.tipfun,;
	   		   codfte WITH m.codfte,;
	   		   nombre WITH 'AUT.GIRO R.G :' + Val_Para(m.codofg,'OFIGIR','D'),;
	   		   monto WITH m.totasig,;
	   		   estado WITH '00',;
	   		   codcal WITH ALLTRIM(m.periodo)+m.nummes+m.codftE+'01001',;
	           nummesC WITH m.nummes,;
	           fechaC WITH m.fecha
	ENDIF   
ENDIF	
RETURN

PROCEDURE ingapgi   && Ingresa asientos patrimoniales
*--------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SELECT caja
SEEK ALLTRIM(m.ctcofg)
W_TIPCTC=CAJA.TIPO
W_CTAH=CAJA.CuentaH
SELE aUTg
ON KEY LABEL F5 DO AgrIte
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
USE AstPat   IN  18 ORDER TAG AstPat6	 ALIAS AstPat
SELECT astpat
SEEK ALLT(m.Nummes)+ALLTRIM(m.aboctcofg)+ALLTRIM(m.Ctcofg)
IF !FOUND()
	DO agrite4
ENDIF
BROWSE FIELDS ;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999,999,999.99';
	WINDOW wind_2 KEY allt(m.nummes)+ALLTRIM(m.aboctcofg)+ALLTRIM(m.Ctcofg)
STORE 0 TO vdebe, vhaber ,vRet
SEEK allt(m.nummes)+m.aboctcofg+m.Ctcofg
SCAN WHILE allt(m.nummes)=NumMes AND ALLTRIM(m.aboctcofg)=NumREF AND ALLTRIM(m.Ctcofg)=CodCtC
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
USE IN 18 
RETURN

PROCEDURE agrite
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH allt(m.nummes),;
	        numref WITH m.aboctcofg,;
	        Periodo WITH m.periodo,;
	        tipdoc WITH 'N/A',;
	        codctc WITH m.ctcofg,;
	        fecha  WITH m.Fecha,;
	        tipctc WITH w_tipctc
ENDIF
RETURN

PROCEDURE agrite4
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH allt(m.nummes),;
	        numref WITH m.aboctcofg,;
	        Periodo WITH m.periodo,;
	        tipdoc WITH 'N/A',;
	        codctc WITH m.ctcofg,;
	        fecha  WITH m.Fecha,;
	        tipctc WITH w_tipctc,;
	        codcta WITH w_ctaH,;
	        tipcta WITH 'D',;
	        mtodeb WITH m.totasig
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(m.nummes),;
	        numref WITH m.aboctcofg,;
	        Periodo WITH m.periodo,;
	        tipdoc WITH 'N/A',;
	        codctc WITH m.ctcofg,;
	        fecha  WITH m.Fecha,;
	        tipctc WITH w_tipctc,;
	        codcta WITH '3050101000',;
	        tipcta WITH 'H',;
	        mtoHab WITH m.totasig
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(m.nummes),;
	        numref WITH m.aboctcofg,;
	        Periodo WITH m.periodo,;
	        tipdoc WITH 'N/A',;
	        codctc WITH m.ctcofg,;
	        fecha  WITH m.Fecha,;
	        tipctc WITH w_tipctc,;
	        codcta WITH '1010101000',;
	        tipcta WITH 'D',;
	        mtodeb WITH m.totasig
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(m.nummes),;
	        numref WITH m.aboctcofg,;
	        Periodo WITH m.periodo,;
	        tipdoc WITH 'N/A',;
	        codctc WITH m.ctcofg,;
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





PROCEDURE Gen_movb
*-----------------
PRIVATE Xord,xrec
STORE .T. TO ingreso
ON KEY LABEL f10 KEYBOARD CHR(23)
DEFINE WINDOW Wind_12 FROM 06,11 TO 12,69 ;
    	 TITLE ' Generaci¢n de Mov.Bcos ' COLOR SCHEME 15
SELE AutG
WTOTAL=0
xord=ORDER()
xrec=RECNO()
wcarref=SPACE(3)
Wfecbco =DATE()
wfeccon =ctod('      ')     
SET ORDER TO AuGire3
ACTIVATE WINDOW wind_12
@ 1,1 SAY "Carta Orden    :" GET wcarref VALID Vcarta()
@ 2,1 SAY "Fecha Banco    :" GET wfecbco PICTURE "@E"
@ 3,1 SAY "Fecha Contable :" 
READ  
IF LASTKEY()=27
	SELE Autg
	SET ORDER TO XORD
	GO xrec
	RELEASE WINDOW Wind_12
*ON KEY LABEL f4 DO gen_movb
	DO vista
	RETURN			        
ENDIF   
SEEK ALLT(wcarref)
IF FOUND()
    SCAN WHILE ALLTRIM(Autg.CarRef) = ALLTRIM(wcarref)
	     wtotal = wtotal + Autg.totasig
	     westbco= Autg.EstBco
	     ingreso= IIF(westbco='û',.F.,.T.)
	ENDSCAN
	@1,40 SAY wtotal PICTURE "999,999,999.99"
    @ 3,18 SAY wfeccon PICTURE "@E"
	IF ingreso  &&Generar
	    SELE Parma
		SEEK 'CORREL'+'MOVBCO'
		wcarctcen  = PADL(NumEnt+1,4,'0')
	    waboctcre  = PADL(NumEnt+2,4,'0')
	    wcarctcre  = PADL(NumEnt+3,4,'0')
	    SELE AutG
		SEEK ALLT(wcarref)
		IF FOUND()
			wcodctcen = Autg.Codctcen
			wcodctc   = Autg.Codctc
			wperiodo  = Autg.Periodo
			wtipfun   = Autg.TipFun 
			wcodfte   = Autg.CodFte 
			wnummes   = Autg.Nummes
		    SCAN WHILE ALLTRIM(Autg.CarRef) = ALLTRIM(wcarref)
	    		 IF RLOCK()
		    	    REPLACE carctcen  WITH wcarctcen,;
		    	            aboctcre  WITH waboctcre,;	
	    		            carctcre  WITH wcarctcre,;
	   		    	        fecbco    WITH wfecbco,;
	   	    	    	    estbco    WITH 'û'
		    	 ENDIF
		    	 UNLOCK
	   		ENDSCAN
	        DO regcar1 &&Cargo Cta.Reg. Recepci¢n
	        DO regabo2  &&Abono Cta.Reg.REtiro
	   	    DO regcar2  &&Cargo Cta.Reg. Retiro
	        DO IngAp1
	        DO IngAp2
	   	    DO IngAp3
	        SELECT Parma
    	    SEEK 'CORREL'+'MOVBCO'
	   	    REPLACE NumEnt WITH NumEnt + 3
	    ENDIF    
	ELSE
	    SELE AutG
		SEEK ALLT(wcarref)
		IF FOUND()
			wcarctcen = Autg.Carctcen
		    waboctcre = Autg.aboctcre
	    	wcarctcre = Autg.Carctcre
			wcodctcen = Autg.Codctcen
			wcodctc   = Autg.Codctc
			wperiodo  = Autg.Periodo
			wtipfun   = Autg.TipFun 
			wcodfte   = Autg.CodFte 
			wnummes   = Autg.Nummes
	    	SCAN WHILE ALLTRIM(Autg.CarRef) = ALLTRIM(wcarref)
		    	 IF RLOCK()
	    		    REPLACE fecbco    WITH wfecbco,;
	   			            estbco    WITH 'û'
	    		 ENDIF
	    		 UNLOCK
		    ENDSCAN
	        DO regcar1 &&Cargo Cta.Reg. Recepci¢n
    	    DO regabo2  &&Abono Cta.Reg.REtiro
	       	DO regcar2  &&Cargo Cta.Reg. Retiro
	        DO IngAp1
	   	    DO IngAp2
	       	DO IngAp3
	    ENDIF    
	ENDIF		        
ELSE
	DO standby WITH "No existe Carta Orden" 
ENDIF
SELE Autg
SET ORDER TO XORD
GO xrec
RELEASE WINDOW Wind_12
*ON KEY LABEL f4 DO gen_movb
DO vista
RETURN			        
    	 
PROCEDURE regcar1
*---------------
* Registra Nota de Cargo Autom tico a la SubCuenta de Entrega
SELE movb
IF ingreso 
	IF F_Appd()
	   REPLACE periodo WITH wperiodo,;
	           nummes  WITH PADL(MONTH(wfecbco),2,'0'),;
	           nummesC WITH wnummes,;
	           numero  WITH wcarctcen,;
	           codctc  WITH wcodctcen,;
	           codban  WITH '002' ,;
			   transa  WITH 'CAR',;
			   fecha   WITH wfecbco,;
			   fechaC  WITH wfeccon,;
			   motivo  WITH '07',;
			   tipdoc  WITH 'N/C',;
			   numref  WITH 'C/O.'+wcarref  ,;
			   tipfun  WITH wtipfun,;
	           codfte  WITH wcodfte,;
	           nombre  WITH 'AUT.GIRO R.G :',;
	           monto   WITH WTOTAL,;
	           estado  WITH '00',;
	           codcal  WITH ALLTRIM(wperiodo)+PADL(MONTH(wfecbco),2,'0')+ALLT(wcodfte)+'01001'
	ENDIF
	UNLOCK ALL
ELSE	
    SEEK ALLTRIM(wnummes)+wcarctcen
    IF FOUND()
	   REPLACE periodo WITH wperiodo,;
	   		   nummes  WITH PADL(MONTH(wfecbco),2,'0'),;
	   		   nummesc WITH wnummes,;
	   		   numero  WITH wcarctcen,;
	   		   codctc  WITH wcodctcen,;
	   		   codban  WITH '002',; 
	           transa  WITH 'CAR',;
	           fecha   WITH wfecbco,;
			   fechaC  WITH wfeccon,;
	           motivo  WITH '07',;
	           tipdoc  WITH 'N/C',;
	           numref  WITH 'C/O.'+wcarref,;
	           tipfun  WITH wtipfun,;
	           codfte  WITH wcodfte,;
	           nombre  WITH 'AUT.GIRO R.G :',;
	           monto   WITH WTOTAL,;
	           estado  WITH '00',;
	   		   codcal  WITH ALLTRIM(wperiodo)+PADL(MONTH(wfecbco),2,'0')+ALLTRIM(wcodfte)+'01001'
	ENDIF   
ENDIF	
RETURN


PROCEDURE regabo2
*---------------
* Registra Nota de Abono Autom tico a la SubCuenta de Retiro
SELE movb
IF ingreso and sw=0
	IF F_Appd()
	   REPLACE periodo WITH wperiodo,;
	   		   nummes  WITH PADL(MONTH(wfecbco),2,'0'),;
	   		   nummesC WITH wnummes,;
	   		   numero  WITH waboctcre,;
	   		   codctc  WITH wcodctc,;
	   		   codban  WITH '002' ,;
	   		   transa  WITH 'ABO',;
	   		   fecha   WITH wfecbco,;
	   		   fechaC  WITH wfeccon,;
	   		   motivo  WITH '07',;
	   		   tipdoc  WITH 'N/A',;
	   		   numref  WITH 'C/O.'+wcarref,;
	   		   tipfun  WITH wtipfun,;
	   		   codfte  WITH wcodfte,;
	   		   nombre  WITH 'AUT. GIRO R.G :',;
	   		   monto   WITH WTOTAL,;
	   		   estado  WITH '00',;
	   		   codcal  WITH ALLTRIM(wperiodo)+PADL(MONTH(wfecbco),2,'0')+ALLTRIM(wcodfte)+'01001'
	ENDIF
	UNLOCK ALL
ELSE	
    SEEK ALLTRIM(wnummes)+waboctcre
    IF FOUND()
	   REPLACE periodo WITH wperiodo,;
	   		   nummes  WITH PADL(MONTH(wfecbco),2,'0'),;
	   		   nummesC WITH wnummes,;
	   		   numero  WITH waboctcre,;
	   		   codctc  WITH wcodctc,;
	   		   codban  WITH '002',; 
	   		   transa  WITH 'ABO',;
	   		   fecha   WITH wfecbco,;
	   		   fechaC  WITH wfeccon,;
	   		   motivo WITH '07',;
	   		   tipdoc WITH 'N/A',;
	   		   numref WITH 'C/O.'+wcarref,;
	   		   tipfun WITH wtipfun,;
	   		   codfte WITH wcodfte,;
	   		   nombre WITH 'AUT. GIRO R.G :',;
	   		   monto WITH WTOTAL,;
	   		   estado WITH '00',;
	   		   codcal WITH ALLTRIM(wperiodo)+PADL(MONTH(wfecbco),2,'0')+ALLTRIM(wcodfte)+'01001'
	ENDIF   
ENDIF	
RETURN


PROCEDURE regcar2
*---------------
* Registra Nota de Cargo Autom tico a la SubCuenta de Retiro
SELE movb
IF ingreso and sw=0
	IF F_Appd()
	   REPLACE periodo WITH wperiodo,;
	   		   nummes  WITH PADL(MONTH(wfecbco),2,'0'),;
	   		   nummesC WITH wnummes,;
	   		   numero  WITH wcarctcre,;
	   		   codctc  WITH wcodctc,;
	   		   codban  WITH '002' ,;
	           transa  WITH 'CAR',;
	           fecha   WITH wfecbco,;
	           fechaC  WITH wfeccon,;
	           motivo  WITH '08',; 
	           tipdoc WITH 'N/C',;
	           numref WITH 'C/O.'+wcarref,;
	           tipfun WITH wtipfun,;
	   		   codfte WITH wcodfte,;
	   		   nombre WITH 'AUT.GIRO R.G :',;
	   		   monto WITH WTOTAL,;
	   		   estado WITH '00',;
	   		   codcal WITH ALLTRIM(wperiodo)+PADL(MONTH(wfecbco),2,'0')+ALLTRIM(wcodfte)+'01001'
	ENDIF
	UNLOCK ALL
ELSE	
    SEEK ALLTRIM(wnummes)+wcarctcre
    IF FOUND()
	   REPLACE  periodo WITH wperiodo,;
	   			nummes  WITH PADL(MONTH(wfecbco),2,'0'),;
	   			nummesC WITH wnummes,;
	   			numero  WITH wcarctcre,;
	   			codctc  WITH wcodctc,;
	   			codban  WITH '002' ,;
	   			transa  WITH 'CAR',;
	   			fecha   WITH wfecbco,;
	   			motivo  WITH '08',;
	   			tipdoc  WITH 'N/C',;
	   			numref  WITH 'C/O.'+wcarref  ,;
	   			tipfun  WITH wtipfun,;
	   			codfte  WITH wcodfte,;
	   			nombre  WITH 'AUT. GIRO R.G :',;
	   			monto   WITH WTOTAL,;
	   			estado  WITH '00',;
	   			codcal  WITH ALLTRIM(wperiodo)+PADL(MONTH(wfecbco),2,'0')+ALLTRIM(wcodfte)+'01001'
	ENDIF   
ENDIF	
RETURN



PROCEDURE ingap1   && Ingresa asientos patrimoniales
*--------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
DEFINE WINDOW Wind_x FROM 11,00 TO 23,79 DOUBLE ;
 TITLE 'Asientos Patrimoniales : Nota de Cargo '+wcodctcen;
 COLOR SCHEME 10
ON KEY LABEL F5 DO AgrIte1
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELE CAJA
SEEK allt(wcodctcen)
w_ctaH=caja.CuentaH
USE AstPat   IN  18 ORDER TAG AstPat5	 ALIAS AstPat
swa=0
SELECT astpat
SEEK allt(wnummes)+ALLTRIM(wcarctcen)+ALLTRIM(wCodctcen)
IF !FOUND()
	DO agrite4a
ENDIF
BROWSE FIELDS ;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='D/H' :P='@M D,H' ,;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999,999,999.99';
	WINDOW wind_x KEY allt(wnummes)+ALLTRIM(wcarctcen)+ALLTRIM(wCodctcen)
STORE 0 TO vdebe, vhaber ,vRet
SEEK allt(wnummes)+wcarctcen+wCodctcen
SCAN WHILE allt(wnummes)=NumMes AND ALLTRIM(wcarctcen)=NumREF AND ALLTRIM(wCodctcen)=CodCtC
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
USE IN 18
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

PROCEDURE agrite1
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH wcarctcen,;
	        tipdoc WITH 'N/C',;
	        codctc WITH wcodctcen,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo
ENDIF
RETURN

PROCEDURE agrite4a
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH wcarctcen,;
	        tipdoc WITH 'N/C',;
	        codctc WITH wcodctcen,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH IIF(swa=0,'3050101000',w_ctaH),;
	        tipcta WITH IIF(swa=0,'D','H');
	        mtodeb WITH IIF(Astpat.tipcta='D',wtotal,0),;
	        mtohab WITH IIF(Astpat.tipcta='H',wtotal,0)
	        
ENDIF
swa=swa+1
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH wcarctcen,;
	        tipdoc WITH 'N/C',;
	        codctc WITH wcodctcen,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH IIF(swa=0,'3050101000',w_ctah),;
	        tipcta WITH IIF(swa=0,'D','H');
	        mtodeb WITH IIF(Astpat.tipcta='D',wtotal,0),;
	        mtohab WITH IIF(Astpat.tipcta='H',wtotal,0)
	        
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH wcarctcen,;
	        tipdoc WITH 'N/C',;
	        codctc WITH wcodctcen,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH '1010101000',;
	        tipcta WITH 'D';
	        mtodeb WITH wtotal
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH wcarctcen,;
	        tipdoc WITH 'N/C',;
	        codctc WITH wcodctcen,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH '1010101000',;
	        tipcta WITH 'H';
	        mtohab WITH wtotal
ENDIF
RETURN

PROCEDURE ingap2   && Ingresa asientos patrimoniales
*--------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
DEFINE WINDOW Wind_x FROM 11,00 TO 23,79 DOUBLE ;
 TITLE 'Asientos Patrimoniales : Nota de Abono '+wcodctc;
 COLOR SCHEME 10

ON KEY LABEL F5 DO AgrIte2
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELE CAJA
SEEK allt(wcodctc)
w_ctaH=Caja.CuentaH
USE AstPat   IN  18 ORDER TAG AstPat6	 ALIAS AstPat
swb=0
SELECT astpat
SEEK allt(wnummes)+ALLTRIM(waboctcre)+ALLTRIM(wCodctc)
IF !FOUND()
	DO agrite4b
ENDIF
BROWSE FIELDS ;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999,999,999.99';
	WINDOW wind_x KEY allt(wnummes)+ALLTRIM(waboctcre)+ALLTRIM(wCodctc)
STORE 0 TO vdebe, vhaber ,vRet
SEEK allt(wnummes)+waboctcre+wCodctc
SCAN WHILE allt(wnummes)=NumMes AND ALLTRIM(waboctcre)=NumREF AND ALLTRIM(wCodctc)=CodCtC
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
USE IN 18
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

PROCEDURE agrite2
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH waboctcre,;
	        tipdoc WITH 'N/A',;
	        codctc WITH wcodctc,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo
ENDIF
RETURN

PROCEDURE agrite4b
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH waboctcre,;
	        tipdoc WITH 'N/A',;
	        codctc WITH wcodctc,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH IIF(swb=0,w_ctah,'3050101000'),;
	        tipcta WITH IIF(swb=0,'D','H'),;
	        mtodeb WITH IIF(Astpat.tipcta='D',wtotal,0),;
	        mtohab WITH IIF(Astpat.tipcta='H',wtotal,0)
ENDIF
swb=swb+1
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH waboctcre,;
	        tipdoc WITH 'N/A',;
	        codctc WITH wcodctc,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH IIF(swb=0,w_ctaH,'3050101000'),;
	        tipcta WITH IIF(swb=0,'D','H'),;
	        mtodeb WITH IIF(Astpat.tipcta='D',wtotal,0),;
	        mtohab WITH IIF(Astpat.tipcta='H',wtotal,0)
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH waboctcre,;
	        tipdoc WITH 'N/A',;
	        codctc WITH wcodctc,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH '1010101000',;
	        tipcta WITH 'D',;
	        mtodeb WITH Wtotal
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH waboctcre,;
	        tipdoc WITH 'N/A',;
	        codctc WITH wcodctc,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH '1010101000',;
	        tipcta WITH 'H',;
	        mtohab WITH wtotal
ENDIF
RETURN


PROCEDURE ingap3   && Ingresa asientos patrimoniales
*--------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
DEFINE WINDOW Wind_x FROM 11,00 TO 23,79 DOUBLE ;
 TITLE 'Asientos Patrimoniales : Nota de Cargo '+wcodctc;
 COLOR SCHEME 10

ON KEY LABEL F5 DO AgrIte3
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELE CAJA
SEEK allt(wcodctc)
w_ctaH=Caja.CuentaH
USE AstPat   IN  18 ORDER TAG AstPat5	 ALIAS AstPat
swc=0
SELECT astpat
SEEK allt(wnummes)+ALLTRIM(wcarctcre)+ALLTRIM(wCodctc)
IF !FOUND()
	DO agrite4C
ENDIF
BROWSE FIELDS ;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999,999,999.99';
	WINDOW wind_x KEY allt(wnummes)+ALLTRIM(wcarctcre)+ALLTRIM(wCodctc)
STORE 0 TO vdebe, vhaber ,vRet
SEEK allt(wnummes)+wcarctcre+wCodctc
SCAN WHILE allt(wnummes)=NumMes AND ALLTRIM(wcarctcre)=NumREF AND ALLTRIM(wCodctc)=CodCtC
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
USE IN 18
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

PROCEDURE agrite3
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH wcarctcre,;
	        tipdoc WITH 'N/C',;
	        codctc WITH wcodctc,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo
ENDIF
RETURN

PROCEDURE agrite4C
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH wcarctcre,;
	        tipdoc WITH 'N/C',;
	        codctc WITH wcodctc,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH IIF(swc=0,'3050101000',w_ctaH),;
	        tipcta WITH IIF(swC=0,'D','H'),;
	        mtodeb WITH IIF(Astpat.tipcta='D',wtotal,0),;
	        mtohab WITH IIF(Astpat.tipcta='H',wtotal,0)
ENDIF
swc=swc+1
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH wcarctcre,;
	        tipdoc WITH 'N/C',;
	        codctc WITH wcodctc,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH IIF(swc=0,'3050101000',w_ctaH),;
	        tipcta WITH IIF(swC=0,'D','H'),;
	        mtodeb WITH IIF(Astpat.tipcta='D',wtotal,0),;
	        mtohab WITH IIF(Astpat.tipcta='H',wtotal,0)
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH wcarctcre,;
	        tipdoc WITH 'N/C',;
	        codctc WITH wcodctc,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH '1010101000',;
	        tipcta WITH 'D',;
	        mtodeb WITH wtotal
ENDIF
IF f_appd()
	REPLACE nummes WITH allt(wnummes),;
	        numref WITH wcarctcre,;
	        tipdoc WITH 'N/C',;
	        codctc WITH wcodctc,;
	        fecha  WITH wfeccon,;
	        tipctc WITH 'RE',;
	        periodo WITH wperiodo,;
	        codcta WITH '1010101000',;
	        tipcta WITH 'H',;
	        mtohab WITH wtotal
ENDIF
RETURN

PROCEDURE eliite
*---------------
SELECT astpat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN

FUNCTION vcarta
*--------------
SELE aUTG
SEEK ALLT(wcarref)
Wfeccon=Autg.Fecha
wfecbco=Autg.fecabo
RETURN

PROCEDURE total
*--------------
 xtot=0
 SELE Itea
 SEEK ALLTRIM(m.Nummes)+ALLTRIM(m.Numdoc)
 SCAN WHILE Itea.nummes=ALLT(m.nummes) AND Itea.numdoc=m.numdoc 
 	  xtot=xtot+Itea.monasig 
 ENDSCAN	  
 ACTIVATE WINDOW wind_3
 @ 0,1 SAY xtot PICTURE '999,999,999.99'
 RETURN

FUNCTION vofi
*------------
xali=ALIAS()
xord=ORDER()
SELE CAJA
LOCATE FOR Caja.Codscta=m.codscta AND Caja.CodOfg=Alltrim(m.Codofg)
IF FOUND()
   m.ctcofg=caja.codctc
ENDIF
SELE PARMA
SEEK 'OFIGIR'+ALLT(m.codofg)
w_prog=ALLT(Parma.CodigoAux)
SELE (xali)
SET ORDER TO (xord)
RETURN   
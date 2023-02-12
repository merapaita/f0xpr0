*PARAMETERS newsistem
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ AutGMEF.PRG                                          L: 0973 ³	
* ³ Autorizaci¢n de Giro del MEF a la Regi¢n                     ³
* ³ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.   06/1997   ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '
Vmens01 = 'Registro de Autorizaci¢n de Giros del MEF'
Vmens03 = 'F2 Proceso'

*- Abre archivos

USE AutGiro  IN  1 ORDER TAG AutGiro1                 ALIAS AutG
USE IteAut   IN  2 ORDER TAG IteAut1                 ALIAS IteA
USE Parmae   IN  4 ORDER TAG Parmae1                 ALIAS Parma
USE MaePre   IN  6 ORDER TAG MaePre1                 ALIAS MaePre
USE cuentas  IN  8 ORDER TAG cuentas1                ALIAS cuenta
USE Cajas    IN  9 ORDER TAG Cajas1                  ALIAS Caja
USE SubCtas  IN 11 ORDER TAG Subctas1                ALIAS Subcta
USE TOTAUT   IN 12 ORDER TAG Totaut1				 ALIAS Totaut

*- Variables de trabajo (registro a trabajar)
PUBLIC vFun,m.fecha,vMes,mmonto,Sw,w_tipctc,w_ctaH,VUSER_ID,wuser_id,colw,m.ejec
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
STORE .T. TO Agrega,vFun
m.fecha=DATE()
store 0 to mmonto,Sw,colw
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
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT 'A\<nula  '    AT 24,54
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
*
 ON KEY LABEL F2 DO PROCAUT
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
 @ 05,02 SAY "     Sub_Cta:" &&SUBCTA
 @ 06,02 SAY "   Cta.Cte. :" &&CodCTc
 @ 07,02 SAY "  Compromiso:" &&TipComp
 @ 08,02 SAY "        Pago:" &&TipAgo
 @ 09,02 SAY "Tot. Ingreso:" &&TOTAL DE INGRESO
 @ 09,40 SAY "Fecha MovBco:" &&Fecha Movimiento de Bancos
 @ 10,00 SAY Vmens03 COLOR W+/B
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT AutG
 SCATTER MEMVAR
 @  0,16 SAY m.correl
 @  1,16 SAY m.NumDoc
 @  1,55 SAY m.NuAuGi
 @  2,16 SAY m.Fecha
 @  2,55 SAY m.Periodo
 @  3,16 SAY m.NumMes
 @  3,23 SAY Val_Para(m.NumMes,'FECMES','D',3,23)
 @  4,16 SAY m.CodFte
 @  4,20 SAY VAL_PARA(m.CodFte ,'CODFTE','D',7,20)
 @  5,16 SAY m.CodScta
 @  5,32 SAY Val_Fun("SubCta","Subcta","Descri",ALLTRIM(m.CodScta))
 @  6,16 SAY m.CodCtc
 @  6,32 SAY Val_Fun("Caja","Codctc","Descri",ALLTRIM(m.Codctc))
 @  7,16 SAY m.TipComp
 @  8,16 SAY m.TiPAgo
 @  9,16 SAY m.TotAsig  PICTURE "999,999,999,999.99"
 @  9,55 SAY m.fecabo 
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
   CodPart  :H='Part'    :P='9',;
   xx=IIF(!EMPTY(codpart),Val_Para(codpart,'GRUGEN','D',3,23),'')   :H='Descripci¢n' ,;
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


PROCEDURE Corri              && Corrige el documento actual
*--------------
 SELECT AutG
 SCATTER MEMVAR
 IF EOF()
    DO standby WITH Vmens08
    UNLOCK ALL
    RETURN
 ENDIF
 ingreso=.F.
 IF !f_lock(1)
    RETURN
 ENDIF
 ACTIVATE WINDOW Wind_C0
 @ 00,16 SAY m.correl
 @ 01,16 GET m.NumDoc      PICTURE "!!!!!!!"
 @ 01,55 GET m.NuAuGi      PICTURE '!!!!'  WHEN ingreso
 @ 02,16 GET m.Fecha       PICTURE '@E' VALID wfec()
 @ 02,55 GET m.Periodo     PICTURE '!!' 
 @ 03,16 GET m.NumMes      PICTURE '!!'  VALID Val_Para(m.NUmMes  ,'FECMES',' ',16,9) .AND. ValFecha() WHEN ingreso
 @ 04,16 SAY m.CodFte 
 @ 05,16 GET m.CodScta     PICTURE "@!"  VALID Val_Fun("SubCta","Subcta","Subcta+' '+Descri",m.CodScta,1,6,31,'Descri')
 @ 06,16 GET m.CodCtc      PICTURE "@!"  VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",m.Codctc,1,7,31,'Descri')
 @ 07,16 GET m.TipComp     PICTURE '@M MES VIGENTE ,MES ANTERIOR'
 @ 08,16 GET m.TiPAgo      PICTURE '@M EFECTIVO,OTROS'
 @ 09,16 GET m.TotAsig     PICTURE "999,999,999,999.99"
 @ 09,55 GET m.Fecabo      PICTURE '@E'
 READ VALID Val_Read()
 m.usuario=wuser_id
 IF LASTKEY() # 27
	 SELECT caja
	 SEEK m.codctc 
	 w_tipctc=caja.Tipo
     w_ctaH  =Caja.CuentaH
	 SELECT autg
    GATHER MEMVAR
    SCATTER MEMVAR
    SS= Tra_Hijo()     && Corrige items
    IF SS .AND. LASTKEY()#27
       SELECT IteA
       GATHER MEMVAR
       =Valida()
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT AutG
 DO Vista
RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT AutG
 DO Pantalla
 SCATTER MEMVAR BLANK
 m.Fecha=DATE()
 m.Periodo=RIGHT(DTOC(DATE()),2)
 vMes=STR(MONTH(DATE()))
 ingreso=.T.
 SELECT Parma
 SEEK 'CORREL'+'AUTLIM'
 m.Correl = PADL(NumEnt+1,4,'0')
 m.codfte ='00'
 @ 00,16 SAY m.correl
 @ 01,16 GET m.NumDoc      PICTURE "!!!!!!!"
 @ 01,55 GET m.NuAuGi      PICTURE '!!!!'
 @ 02,16 GET m.Fecha       PICTURE '@E' VALID wfec()
 @ 02,55 GET m.Periodo     PICTURE '!!'
 @ 03,16 GET m.NumMes      PICTURE '!!'  VALID Val_Para(m.NUmMes  ,'FECMES',' ',16,9) .AND. ValFecha()
 @ 04,16 SAY m.CodFte      
 @ 05,16 GET m.CodScta     PICTURE "@!"  VALID Val_Fun("SubCta","Subcta","Subcta+' '+Descri",m.CodScta,1,6,31,'Descri') AND vctc()
 @ 06,16 GET m.CodCtc      PICTURE "@!"  VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",m.Codctc,1,7,31,'Descri')
 @ 07,16 GET m.TipComp     PICTURE '@M MES VIGENTE ,MES ANTERIOR'
 @ 08,16 GET m.TiPAgo      PICTURE '@M EFECTIVO,OTROS'
 @ 09,16 GET m.TotAsig     PICTURE "999,999,999,999.99"
 @ 09,55 GET m.Fecabo      PICTURE '@E'
 READ VALID Val_Read()
 m.usuario=wuser_id
  IF LASTKEY()#27
	 SELECT caja
	 SEEK ALLTRIM(m.codctc)
     W_TIPCTC=CAJA.TIPO
     w_ctaH  =Caja.CuentaH
     ***colocar Ofigir='S'
    vAnula = .F.
    SELECT AutG
    IF F_Appd()
       GATHER MEMVAR
       UNLOCK ALL
       SELECT Parma
       SEEK 'CORREL'+'AUTLIM'
       REPLACE NumEnt WITH NumEnt + 1       
       SELECT AutG
       SCATTER MEMVAR
       DO Agreg_item
       SELECT IteA
       DO WHILE .T.
          DO Tra_hijo
          IF LASTKEY() = 27
             IF YESNO( '¨ Cancela el Ingreso ?' )
                vAnula = .T.
                EXIT
             ENDIF
          ELSE
             IF YESNO( '¨ Est n correctos los datos ?')             
                =Valida()
                EXIT
             ENDIF
          ENDIF
          Sw=1
       ENDDO
       IF vAnula
          DO Anula
       ENDIF
    ELSE
       GO BOTTOM
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT AutG
 DO Vista
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
   CodPart  :H='Part'    :P='9' :V=Val_Para(codpart,'GRUGEN','C',3,23),;
   xx=IIF(!EMPTY(codpart),Val_Para(codpart,'GRUGEN','D',3,23),'')   :H='Descripci¢n' ,;
   MonAsig  :H='Monto'   :P='999,999,999,999.99'

 vOrd=ORDER()
 SELECT IteA
 SET ORDER TO vOrd
 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5
 ON KEY LABEL F2
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
           Periodo  WITH M.Periodo,;
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

PROCEDURE Anula
*--------------
* Anula el documento actual.
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
IF yesno("¨ANULA Documento?")
   vErdad=.T.
   SELECT AutG
   vNummes=Autg.Nummes
*  vNummes1=PADL(MONTH(Autg.fecabo),2,'0')
   vcorrel=ALLTRIM(AutG.Correl)
   vabono =ALLTRIM(AutG.Nroabo)
   vcodctc=ALLTRIM(Autg.Codctc)
   IF f_lock(1)
      DELETE
      *-Busca y elimina los Items
      SELECT IteA
      GO TOP
      SEEK ALLTRIM(vNummes)+vcorrel
      IF FOUND()
         SCAN WHILE vNummes = ALLTRIM(IteA.Nummes) .AND. vcorrel = IteA.Correl 
              IF f_lock(1)
                 DELETE NEXT 1
              ENDIF
         ENDSCAN
      ENDIF
      SELECT AutG
      IF !BOF()
         SKIP -1
      ELSE
         IF !EOF()
            SKIP
         ENDIF
      ENDIF
   ENDIF
ENDIF
UNLOCK ALL
DO Vista
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
 RELEASE    MENU   mMenu
 CLOSE DATA
 RESTORE SCREEN FROM PRINCIPAL
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
*	   USE itaure   IN 0  ALIAS Itaure
*	   vind1 = SYS(3)+'.IDX'
*	   SELECT Itaure
*	   INDEX ON Ctcofg+nummes TO (vind1) FOR Codscta = ALLTRIM(vcodscta)
*	   GO TOP
*	   IF !EOF()
*		  DO REPORTE WITH 2,"CuAuLiOf"," Recepci¢n de Giros del MEF ",1,.F.,.T.
*	   ELSE
*   	  DO STANDBY WITH vmens08	  
*	   ENDIF
*	   SET INDEX TO
*	   USE
	   SELECT Itea
	   SET ORDER TO (xord)
	   SET FILT TO	
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

PROCEDURE PROCAUT
*----------------
*
store space(14) to vcodctc
store space(2) to vnummes,vperiodo
*
DEFINE WINDOW _verproc FROM 14,35 TO 20,75 COLOR W+/B TITLE " Proceso F2 " DOUBLE

activate window _verproc
@ 1,1 Say 'Ingrese Periodo: ' GET vPeriodo PICTURE '!!'
@ 2,1 SAY 'Ingrese Mes    : ' GET vnummes PICTURE '!!' DEFAULT SPACE(2)
@ 3,1 SAY 'Ingrese CtaCte : ' GET vcodctc FUNCTION '!' VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",vcodctc,1,2,24)
READ

release window _verproc

IF LASTKEY()=27
	RETURN
Endif
*
SELE ITEA
SET ORDER TO ITEAUT4
*
GO TOP
seek vperiodo+vnummes+vcodctc
SCAN WHILE periodo=vperiodo and nummes=vnummes and codctc=vcodctc
	vcodpart=codpart
	vasigna=0
	DO WHILE codpart=vcodpart
		IF .NOT. DELETE()
			vasigna = vasigna + monasig
		ENDIF			
		SKIP
	ENDDO
	SKIP -1
	sele totaut
	SEEK vperiodo+vnummes+vcodctc+vcodpart
   	IF .NOT. EOF()
   		repl asigna with vasigna, SALDO WITH ASIGNA-EJECUT
	Else
		if f_appd()
			repla periodo with vperiodo, nummes with vnummes,;
			      codctc with vcodctc, codpart with vcodpart,;
			      asigna with vasigna, SALDO WITH ASIGNA-EJECUT
		endif
	endif									
	*
	sele ITEA
endscan
*
if yesno("¨Desea ver Saldos?")
	DEFINE WINDOW _versal FROM 12,20 TO 20,77 COLOR W+/B TITLE "Saldos de Aut.Giro" DOUBLE
	activate window _versal
	@ 0,0 SAY 'PART.'
	@ 0,8 SAY 'ASIG.'
	@ 0,22 SAY 'EJEC.'
	@ 0,41 SAY 'SALDO'
	colw=2
*	use \emer98\data\totaut IN 3 order tag \emer98\data\totaut1 alias totaut1
	sele totaut
	seek vperiodo+vnummes+vcodctc
	SCAN while periodo=vperiodo and nummes=vnummes and codctc=vcodctc
*		WKEY=vperiodo+vnummes+vcodctc+codpart
		*
*		sele totaut1
*		seek wkey
*		if .not. eof()
*			m.ejecut=ejecut
*		else
*			m.ejecut=0.00
*		endif
*		sele totaut						
		@ colw,0 SAY codpart pict '9'
		@ colw,2 SAY asigna  pict '999,999,999.99'
		@ colw,19 SAY ejecut pict '999,999,999.99'
		@ colw,36 SAY SALDO  pict '999,999,999.99'
		colw=colw+1
	ENDSCAN
	*
	USE  IN 3
	if yesno("¨Desea Salir?")
		release window _versal
	else
		wait ' '
		release window _versal
	endif		
	*
ENDIF		
SELE ITEA
SET ORDER TO ITEAUT1
return 

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
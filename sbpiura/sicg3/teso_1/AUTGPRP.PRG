* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ AutGPRP.PRG                                          L: 0966 ³	
* ³ Autorizaci¢n de Giro - Ingresos Propios                      ³
* ³ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Mensajes de aviso al usuario
PUBLIC Ingreso
PRIVATE Vmens01,Vmens02,Vmens09
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '
Vmens01 = 'Registro de Autorizaci¢nes de Giro Ingresos Propios' 

*- Abre archivos

USE AutgirP  IN  1 ORDER TAG AutgirP1                 ALIAS AutG
USE Iteaup   IN  2 ORDER TAG Iteaup1                 ALIAS IteA
USE Clase    IN  3 ORDER TAG Clase1                  ALIAS Clase
USE Parmae   IN  4 ORDER TAG Parmae1                 ALIAS Parma
USE Calen    IN  5 ORDER TAG Calen1                  ALIAS Calen
USE MaePre   IN  6 ORDER TAG MaePre1                 ALIAS MaePre
USE ItechqGe IN  7 ORDER TAG ItechqGe1               ALIAS IteCH
USE cuentas  IN  8 ORDER TAG cuentas1                ALIAS cuenta
USE Cajas    IN  9 ORDER TAG Cajas8                  ALIAS Caja
USE MovBco   IN 10 ORDER TAG Movbco11        		 ALIAS Movb
USE SubCtas  IN 11 ORDER TAG Subctas1                ALIAS Subcta
USE IteTf    IN 12 ORDER TAG IteTf1               	 ALIAS itef
USE Iteauts  IN 13 ORDER TAG IteAuts1           	 ALIAS iteS

*- Variables de trabajo (registro a trabajar)
PUBLIC vFun,m.fecha,xMes,xAno,sw,Vnummes,VUSER_ID,wuser_id,wbanco,wdesbco
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
SELECT Autg
SCATTER MEMVAR BLANK

STORE .T. TO Agrega,vFun
m.fecha=DATE()
store 0 to sw
STORE SPACE(2) TO vnummes
*- Mes y a¤o actual

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
 ON SELECTION PAD elimi  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO LISTA
 ON SELECTION PAD termi  OF mMenu DO termi
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
 @ 2, 1 SAY "   N§ Oficio :"
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
 @ 9,40 SAY "Observaciones:"

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
 @  1,23 SAY IIF(!EMPTY(m.nummes),Val_Para(m.NumMes,'FECMES','D',1,23),' ')
 @  1,56 SAY m.Fecha
 @  2,16 SAY m.CarRef
 @  2,56 SAY IIF(m.estbco='1','CHEQUE   DE   GERENCIA',IIF(m.estbco='2','TRANSFERENCIA DE FONDO','AUTORIZACION   SIMPLE ')) COLOR SCHEME 2
 @  3,16 SAY m.CodScta
 @  4,16 SAY IIF(m.estbco#'3',m.Codctc,' ')
 @  4,32 SAY IIF(!EMPTY(m.codctc) OR m.estbco='3',Val_Fun("Caja","CodCtc","Descri",m.Codctc),' ')
 @  5,16 SAY m.CodOfg
 @  5,20 SAY IIF(!EMPTY(m.codofg),Val_Para(m.CodOfG,'OFGIIP','D',5,20),' ')
 @  6,16 SAY m.Ctcofg
 @  6,32 SAY IIF(!EMPTY(m.ctcofg),Val_Fun("Caja","CodCtc","Descri",m.CtcOfg),' ')
 @  7,16 SAY m.CodFte
 @  7,20 SAY IIF(!EMPTY(m.codfte),VAL_PARA(m.CodFte ,'CODFTE','D',7,20),' ')
 @  7,56 SAY m.TipFun
 @  7,58 SAY IIF(!EMPTY(m.tipfun),VAL_PARA(m.tipfun,'TIPFUN','D',7,15),' ')
 @  8,16 SAY m.TipComp
 @  8,56 SAY m.TiPAgo
 @  9,16 SAY m.TotAsig  PICTURE "999,999,999,999.99"
 @  9,56 SAY m.observ PICTURE '@S20'
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
   CodPart  :H='Part'    :P='!!.!!',;
   xx=clase() :50 :H='Descripci¢n',;
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


PROCEDURE Corri              && Corrige el documento actual
*--------------
 SELECT AutG
 SCATTER MEMVAR
 IF EOF()
    DO standby WITH Vmens08
    UNLOCK ALL
    RETURN
 ENDIF
 IF !f_lock(1)
    RETURN
 ENDIF
 ACTIVATE WINDOW Wind_C0
 ingreso=.F.
 @ 00,16 GET m.NuAuGi      PICTURE '!!!!'
 @ 00,56 SAY m.NumDoc      PICTURE "!!!!"
 @ 01,16 GET m.Periodo     PICTURE '!!'
 @ 01,20 GET m.NumMes      PICTURE '!!' VALID Val_Para(m.NumMes,'FECMES','C')
 @ 01,56 GET m.Fecha       PICTURE '@E'
 @ 02,16 SAY m.CarRef      PICTURE '!!!!'
 @ 03,16 SAY m.CodScta 
 @ 04,16 SAY IIF(m.estbco#'3',m.Codctc,' ')
 @ 04,32 SAY IIF(!EMPTY(m.codctc) OR m.estbco='3',Val_Fun("Caja","CodCtc","Descri",m.Codctc),' ')
 @ 05,16 SAY m.CodOfg
 @ 05,20 SAY IIF(!EMPTY(m.codofg),Val_Para(m.CodOfG,'OFGIIP','D',5,20),' ')
 @ 06,16 SAY m.Ctcofg
 @ 06,32 SAY IIF(!EMPTY(m.ctcofg),Val_Fun("Caja","CodCtc","Descri",m.CtcOfg),' ')
 @ 07,16 SAY m.CodFte
 @ 07,20 SAY IIF(!EMPTY(m.codfte),VAL_PARA(m.CodFte ,'CODFTE','D',7,20),' ')
 @ 07,56 GET m.TipFun      PICTURE '!'   VALID VAL_PARA(m.tipfun,'TIPFUN',' ',56,15,2)
 @ 08,16 GET m.TipComp     PICTURE '@M MES VIGENTE ,MES ANTERIOR'
 @ 08,56 GET m.TiPAgo      PICTURE '@M EFECTIVO,OTROS'
 @ 09,16 SAY m.TotAsig     PICTURE "999,999,999,999.99" 
 @  9,56 GET m.observ PICTURE '@S20'
 m.usuario=wuser_id
 READ VALID Val_Read()
 IF LASTKEY() # 27
    SELECT Autg
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
 DO pantalla
 SCATTER MEMVAR BLANK
IF escolor
	DEFINE POPUP xcot  FROM 17,45 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP xcot  FROM 17,45 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF xcot PROMPT 'Captura de \<Cheque de Gerencia'
DEFINE BAR 2 OF xcot PROMPT 'Captura de\<Transferencia de Fondos'
DEFINE BAR 3 OF xcot PROMPT 'Captura de \<Oficio Simple de Aut.'
ON SELECTION POPUP xcot  DEACTIVATE POPUP
ACTIVATE POPUP xcot
DO CASE
   CASE BAR() =  1
		m.estbco = '1'
		oi=carcg()
      	IF LASTKEY()=27 OR !OI
		   SELECT AutG
		   DO vista
		   RETURN
		ENDIF
   CASE BAR() =  2
		m.estbco = '2'
		oi=cartf()
      	IF LASTKEY()=27 OR !OI
		   SELECT AutG
		   DO vista
		   RETURN
		ENDIF
   CASE BAR() =  3
		m.estbco = '3'
		oi=caras()
      	IF LASTKEY()=27 OR !OI
		   SELECT AutG
		   DO vista
		   RETURN
		ENDIF
ENDCASE		
 m.Fecha=DATE()
 m.Periodo=RIGHT(DTOC(DATE()),2)
 m.codscta='ING.PRP.'  
 m.codfte='PRP'
 ingreso=.t.
 SELECT Parma
 SEEK 'CORREL'+'AUTGIP'
 m.Numdoc = PADL(NumEnt+1,4,'0')
 @ 00,16 GET m.NuAuGi      PICTURE '!!!!' 
 @ 00,56 SAY m.NumDoc      PICTURE "!!!!"
 @ 01,16 SAY m.Periodo     PICTURE '!!'
 @ 01,20 GET m.NumMes      PICTURE '!!' VALID Val_Para(m.NumMes,'FECMES',' ',20,23) 
 @ 01,56 GET m.Fecha       PICTURE '@E'
 @ 02,16 SAY m.CarRef      PICTURE '!!!!' 
 @ 03,16 SAY m.CodScta 
 @ 04,16 SAY IIF(m.estbco#'3',m.Codctc,' ')
 @ 04,32 SAY IIF(!EMPTY(m.codctc) OR m.estbco='3',Val_Fun("Caja","CodCtc","Descri",m.Codctc),' ')
 @ 05,16 SAY m.CodOfg
 @ 05,20 SAY IIF(!EMPTY(m.codofg),Val_Para(m.CodOfG,'OFGIIP','D',5,20),' ')
 @ 06,16 SAY m.Ctcofg
 @ 06,32 SAY IIF(!EMPTY(m.ctcofg),Val_Fun("Caja","CodCtc","Descri",m.CtcOfg),' ')
 @ 07,16 SAY m.CodFte
 @ 07,20 SAY IIF(!EMPTY(m.codfte),VAL_PARA(m.CodFte ,'CODFTE','D',7,20),' ')
 @ 07,56 GET m.TipFun      PICTURE '!'   VALID VAL_PARA(m.tipfun,'TIPFUN',' ',56,15,2)
 @ 08,16 GET m.TipComp     PICTURE '@M MES VIGENTE ,MES ANTERIOR'
 @ 08,56 GET m.TiPAgo      PICTURE '@M EFECTIVO,OTROS'
 @ 09,16 SAY m.TotAsig     PICTURE "999,999,999,999.99"
 @  9,56 GET m.observ PICTURE '@S20'
 m.usuario=wuser_id
 READ VALID Val_Read()
 IF LASTKEY()#27
    vAnula = .F.
    SELECT AutG
    IF F_Appd()
       GATHER MEMVAR
       UNLOCK ALL
       SELE PARMA
       SEEK 'CORREL'+'AUTGIP'
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
                DO CASE
                   CASE m.estbco='1'
                        SELE itech
                        SET ORDER TO itechqge2
                        SEEK ALLT(m.mesref)+m.carref+m.ctcofg   
                        IF RLOCK()
	                        REPLACE estado WITH 'û'
	                    ENDIF
	                    UNLOCK
                   CASE m.estbco='2'
                        SELE iteF
                        SET ORDER TO itetf2
                        SEEK ALLT(m.mesref)+m.carref+m.ctcofg   
                        IF RLOCK()
	                        REPLACE estado WITH 'û'
	                    ENDIF
	                    UNLOCK
                   CASE m.estbco='3'
                        SELE iteS
                        SET ORDER TO iteAuts2
                        SEEK ALLT(m.mesref)+m.carref+m.ctcofg   
                        IF RLOCK()
	                        REPLACE estado WITH 'û'
	                    ENDIF
	                    UNLOCK
	            ENDCASE     
                EXIT
             ENDIF
          ENDIF
          sw=1
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
           Codctc   WITH m.Codctc,;
           Ctcofg   WITH m.Ctcofg ,;
           Codfte   WITH m.codfte
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
IF yesno("¨ANULA Documento?")
   vErdad=.T.
   vAli=ALIAS()
   vOrd=ORDER()
   SELECT AutG
   vnummes=ALLTRIM(Autg.nummes)
   vNumDoc=ALLTRIM(AutG.NumDoc)
   vNuAuGi=ALLTRIM(AutG.NuAuGi)
   vmesref=ALLTRIM(Autg.mesref)
   SELECT AutG
   IF f_lock(1)
      DELETE
      *-Busca y elimina los Items
      SELECT IteA
      GO TOP
      SEEK ALLTRIM(vNummes)+ALLTRIM(vNumdoc)
      IF FOUND()
         SCAN WHILE vNumDoc = ALLTRIM(IteA.NumDoc) .AND. vNummes = ALLTRIM(IteA.Nummes)
              IF f_lock(1)
                 DELETE NEXT 1
              ENDIF
         ENDSCAN
      ENDIF
      DO CASE
         CASE m.estbco='1'
              SELE itech
              SET ORDER TO itechqge2
              SEEK ALLT(m.mesref)+m.carref+m.ctcofg   
              IF RLOCK()
	             REPLACE estado WITH ' '
	          ENDIF
	          UNLOCK
         CASE m.estbco='2'
              SELE iteF
              SET ORDER TO itetf2
              SEEK ALLT(m.mesref)+m.carref+m.ctcofg   
              IF RLOCK()
	             REPLACE estado WITH ' '
	          ENDIF
	          UNLOCK
         CASE m.estbco='3'
              SELE iteS
              SET ORDER TO iteauts2
              SEEK ALLT(m.mesref)+m.carref+m.ctcofg   
              IF RLOCK()
	             REPLACE estado WITH ' '
	          ENDIF
	          UNLOCK
	  ENDCASE     
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
SELECT (vAli)
SET ORDER TO vOrd
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
wdesbco = SPACE(13)
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
         SET ORDER TO TAG Iteaup1
         SET FILTER TO numdoc = vnumdoc
         SELE caja
         IF SEEK (autg.ctcofg)
            wbanco=caja.banco
            SELE Parma
            IF SEEK ('BANCOS'+wbanco)
	           wdesbco=LEFT(Parma.descri,13)
	        ENDIF
	     ENDIF  
	     SELE Itea
         DO REPORTE WITH 2,"RepAutIP"," Autorizaciones de Giros a las Ofc. Giradoras ",1,.F.,.T.
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
         DO REPORTE WITH 2,"ReAuGI"," Resumen de Autorizaciones de Giro a las Ofc. Giradoras ",1,.F.,.T.
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

   
PROCEDURE carcg
*--------------
* Carga valores de Cheque Gerencia
vfun = .T.
OK   = FOUND()
SELECT IteCH
SET FILTER TO Estado#'û' AND tipo=1
GO TOP
IF EOF()
	DO standby WITH 'No existe Oficio de Cheque de Gerencia'
	SET FILT TO
	RETURN .F.
ELSE
  GO BOTTOM
** Cambio de POPUP con BROWSE
   ON KEY LABEL F10 KEYBOARD CHR(23)
   DEFINE WINDOW Elicg FROM 1,1 TO 18,79 TITLE " Elija el Oficio de Cheque de Gerencia con F10 "
   BROWSE NOED WINDOW Elicg COLOR SCHEME 10 FIELDS ;
     numero:H="Oficio",;
     nummes:H="Mes",;
     codctc,;
     importe,;
     codofg
ENDIF
IF LASTKEY()#27
    SELE Itech
	m.tipfun   = tipfun
	m.nummes   = nummes
	m.mesref   = nummes
    m.carref   = numero
	m.totasig  = importe
	m.ctcofg   = codctc
	m.codofg   = codofg
	SHOW GETS
	SET FILT TO
	USE chqger IN 0 ORDER chqger1 ALIAS chqG
    SELE chqG
    IF SEEK(ALLT(m.mesref)+m.carref)
       m.codctc = chqG.ctacar
    ENDIF   
    USE 
	SELECT AutG
ENDIF
RETURN .T.

PROCEDURE carTF
*--------------
* Carga valores de Transferencia de Fondos
vfun = .T.
OK   = FOUND()
SELECT Itef
SET FILTER TO Estado#'û' AND tipo=1
GO TOP
IF EOF()
	DO standby WITH 'No existe Oficio de Transferencia de Fondos'
	SET FILT TO
	RETURN .F.
ELSE
  GO BOTTOM
** Cambio de POPUP con BROWSE
   ON KEY LABEL F10 KEYBOARD CHR(23)
   DEFINE WINDOW Elicg FROM 1,1 TO 18,79 TITLE " Elija el Oficio de Transferencia de Fondos con F10 "
   BROWSE NOED WINDOW Elicg COLOR SCHEME 10 FIELDS ;
     numero:H="Oficio",;
     nummes:H="Mes",;
     codctc,;
     importe,;
     codofg
ENDIF
IF LASTKEY()#27
    SELE Itef
	m.tipfun   = tipfun
	m.nummes   = nummes
	m.mesref   = nummes
    m.carref   = numero
	m.totasig  = importe
	m.ctcofg   = codctc
	m.codofg   = codofg
	SHOW GETS
	SET FILT TO
	USE Transfon IN 0 ORDER transfon1 ALIAS tran
    SELE tran
    IF SEEK(ALLT(m.mesref)+m.carref)
       m.codctc = tran.ctacar
    ENDIF   
    USE
	SELECT AutG
ENDIF
RETURN .T.

PROCEDURE carAS
*--------------
* Carga valores de Oficio Simples
vfun = .T.
OK   = FOUND()
SELECT IteS
SET FILTER TO Estado#'û' AND tipo=1
GO TOP
IF EOF()
	DO standby WITH 'No existe Oficio Simple de Autorizaci¢n'
	SET FILT TO
	RETURN .F.
ELSE
  GO BOTTOM
** Cambio de POPUP con BROWSE
   ON KEY LABEL F10 KEYBOARD CHR(23)
   DEFINE WINDOW Elicg FROM 1,1 TO 18,79 TITLE " Elija el Oficio Simple de Autorizaci¢n con F10 "
   BROWSE NOED WINDOW Elicg COLOR SCHEME 10 FIELDS ;
     numero:H="Oficio",;
     nummes:H="Mes",;
     codctc,;
     importe,;
     codofg
ENDIF
IF LASTKEY()#27
    SELE IteS
	m.tipfun   = tipfun
	m.nummes   = nummes
	m.mesref   = nummes
    m.carref   = numero
	m.totasig  = importe
	m.ctcofg   = codctc
	m.codofg   = codofg
	SHOW GETS
	SET FILT TO
	SELECT AutG
ENDIF
RETURN .T.
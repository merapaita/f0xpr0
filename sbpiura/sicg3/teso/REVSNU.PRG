* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RevSnu.PRG        10/09/96                         L: 1314   ³	
* ³ Reversi¢n de Saldos No Utilizados                            ³
* ³ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09
Vmens01 = 'Revisi¢n de Reversiones de Saldos No Utilizados'
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '

*- Abre archivos

USE RevSnu   IN  1 ORDER TAG RevSnu1    ALIAS RevS
USE IteRevSn IN  2 ORDER TAG IteRevSn1  ALIAS IteR
USE Parmae   IN  4 ORDER TAG Parmae1    ALIAS Parma
USE AstPat   IN  7 ORDER TAG AstPat6   	ALIAS AstPat
USE Cuentas  IN  8 ORDER TAG Cuentas1   ALIAS Cuenta
USE Cajas    IN  9 ORDER TAG Cajas1     ALIAS Caja
USE MovBco   IN 10 ORDER TAG Movbco11   ALIAS Movb
USE SubCtas  IN 11 ORDER TAG Subctas1   ALIAS Subcta

SUSP
*- Variables de trabajo (registro a trabajar)
PUBLIC vFun,m.fecha,vMes,mmonto,Sw,vfecbco,VUSER_ID,wuser_id
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
vfecbco=DATE()

STORE   .T.	 TO 	Agrega,vFun
STORE DATE() TO 	m.fecha
STORE    0   TO		mmonto,Sw

*- Inicia proceso
SELECT RevS
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

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 10,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_C2 FROM 10,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C3 FROM 12,00 TO 23,38 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C4 FROM 00,00 TO 23,79 DOUBLE;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Reversi¢n de Saldos No Utilizados' ;

DEFINE WINDOW wind_3 FROM 20,64 TO 22,78 ;
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

 ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @ 00,02 SAY "     Numero :"  
 @ 01,02 SAY "      Fecha :" 
 @ 01,40 SAY "    Periodo :" 
 @ 02,02 SAY "        Mes :" 
 @ 03,02 SAY "     Nombre :" 
 @ 04,02 SAY "    Sub_Cta :" 
 @ 05,02 SAY " Referencia :" 
 @ 06,02 SAY " Cta.Tesoro :" 
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT RevS
 SCATTER MEMVAR
 @  0,16 SAY m.numrev
 @  1,16 SAY m.Fecha
 @  1,55 SAY m.Periodo
 @  2,16 SAY m.NumMes
 @  3,16 SAY m.nombre 
 @  4,16 SAY m.CodScta
 @  5,16 SAY m.refer
 @  6,16 SAY m.ctates
 DO Vista_Hijo
 DO total
RETURN


PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
 SELECT IteR
 SEEK ALLTRIM(m.nummes)+m.numrev
 BROWSE;
   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
   WINDOW Wind_C2 KEY ALLTRIM(m.nummes)+m.numrev;
   FIELDS;
   CodCtc    :H='Cta.Cte.' ,;
   xx=ofigir(1) :H='Of. Giradora',;
   Importe   :P='999,999,999,999.99'
  SELECT  RevS
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT RevS
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
   numrev   		 ,;
   CodScta  :H= 'Sub_Cuenta',;
   Fecha    :H= 'Fecha',;
   Nombre  
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
 SELECT RevS
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
    SELECT RevS
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
 SELECT RevS
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
 SELECT RevS
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
 SELECT RevS
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
 @ 00,16 SAY m.numrev
 @ 01,16 GET m.Fecha       PICTURE '@E' VALID wfec()
 @ 01,55 GET m.Periodo     PICTURE '!!' 
 @ 02,16 GET m.NumMes      PICTURE '!!'  VALID Val_Para(m.NUmMes  ,'FECMES',' ',27,9) .AND. ValFecha() WHEN ingreso
 @ 03,16 GET m.nombre      PICTURE '@!'
 @ 04,16 GET m.CodScta     PICTURE "@!"  VALID Val_Fun("SubCta","Subcta","Subcta+' '+Descri",m.CodScta,1,4,21,'SUBSTR(Descri,1,22)')
 @ 05,16 GET m.refer       PICTURE "@!"  VALID ctatp()
 @ 06,16 SAY SPACE(14)     
 m.usuario=wuser_id
 READ VALID Val_Read()
 IF LASTKEY() # 27
*	 SELECT caja
*	 SEEK m.codctc 
*	 w_tipctc=caja.Tipo
	 SELECT RevS
    GATHER MEMVAR
    SCATTER MEMVAR
    vAstpat = pidapnc()
    SS= Tra_Hijo()     && Corrige items
    SELE CAJA
	SEEK ALLTRiM(Iter.CodCtc)
	IF FOUND()
	   w_TipCtc=CAJA.TIPO
	   wbanco = CAJA.banco
	ENDIF
	DO IngAp1 WITH vastpat
    IF SS .AND. LASTKEY()#27
       SELECT IteR
       GATHER MEMVAR
*	   DO regabo
*	   DO IngAP              
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 DO total
 SELECT RevS
 DO Vista
RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT RevS
 DO pantalla
 SCATTER MEMVAR BLANK
 m.Fecha=DATE()
 m.Periodo=RIGHT(DTOC(DATE()),2)
 vMes=STR(MONTH(DATE()))
 ingreso=.T.
 SELECT Parma
 SEEK 'CORREL'+'REVSNU'
 m.numrev = PADL(NumEnt+1,4,'0')
 m.nombre='REYNALDO PETROZZI GIRON'+SPACE(27)
 @ 00,16 SAY m.numrev
 @ 01,16 GET m.Fecha       PICTURE '@E' VALID wfec()
 @ 01,55 GET m.Periodo     PICTURE '!!' 
 @ 02,16 GET m.NumMes      PICTURE '!!'  VALID Val_Para(m.NUmMes  ,'FECMES',' ',27,9) .AND. ValFecha() WHEN ingreso
 @ 03,16 GET m.nombre      PICTURE '@!'
 @ 04,16 GET m.CodScta     PICTURE "@!"  VALID Val_Fun("SubCta","Subcta","Subcta+' '+Descri",m.CodScta,1,4,21,'SUBSTR(Descri,1,22)')
 @ 05,16 GET m.refer       PICTURE "@!"  VALID ctatp()
 @ 06,16 SAY SPACE(14)     
 m.usuario=wuser_id
 READ VALID Val_Read()
  IF LASTKEY()#27
     vAnula = .F.
     SELECT RevS
     IF F_Appd()
        GATHER MEMVAR
        UNLOCK ALL
        SELECT Parma
        SEEK 'CORREL'+'REVSNU'
        REPLACE NumEnt WITH NumEnt + 1       
        SEEK "CORREL"+"MOVBCO"
		vNUMERO=PADL(NUMENT+1,4,'0')
        SELECT RevS
        SCATTER MEMVAR
        DO Agreg_item
        SELECT IteR
        DO WHILE .T.
   		  vAstpat = pidapnc()
          DO Tra_hijo
		 SELE CAJA
		 SEEK ALLTRiM(Iter.CodCtc)
		 IF FOUND()
		    w_TipCtc=CAJA.TIPO
		    wbanco = CAJA.banco
		 ENDIF
		  DO IngAp1 WITH vastpat
          IF LASTKEY() = 27
             IF YESNO( '¨ Cancela el Ingreso ?' )
                vAnula = .T.
                EXIT
             ENDIF
          ELSE
             IF YESNO( '¨ Est n correctos los datos ?')             
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
 DO total 
 UNLOCK ALL
 SELECT RevS
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
 STORE 0 TO m.Importe,AntCan,vSuma
 SELECT IteR
 GO TOP
 SEEK ALLTRIM(m.nummes)+m.numrev
 IF !FOUND()
    DO Agreg_Item
 ENDIF

 BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
  KEY ALLTRIM(m.nummes)+m.numrev;
  FIELDS;
   Codctc   : H='Cta.Cte'   :V=val_ctc("Caja","CodCtc","CodCtc+' '+Descri",Codctc,2) :f,;
   xx=ofigir(2) :H='Of. Giradora',;
   Importe  : H='      Monto      ':P='999,999,999,999.99',;
   NroCar   : H='N/C' :R :F
 vOrd=ORDER()
 SELECT IteR
 SET ORDER TO (vOrd)
 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5
 UNLOCK ALL
SELECT RevS
ACTIVATE SCREEN
vTempo = ' Ingresa  Elige  Busca  Anterior  Siguiente  Corrige  Anula  Lista Termina '
DO Logos WITH Rotulo1,vtempo
SHOW MENU mMenu
RETURN  vFun


PROCEDURE Agreg_item
*-------------------
SELECT Parma
SEEK "CORREL"+"MOVBCO"
vNUMERO=PADL(NUMENT+1,4,'0')
SELECT IteR
IF F_Appd()
   REPLACE Nummes   WITH m.Nummes ,;
           numrev   WITH m.numrev ,;
           periodo  WITH m.periodo,;  
           fecbco   WITH vfecbco,;
		   NroCar   WITH vnumero
   SELECT Parma
   SEEK "CORREL"+"MOVBCO"
   REPLACE NUMENT WITH NUMENT+1
   SELE Iter
ENDIF
UNLOCK
RETURN


PROCEDURE Elimi_item
*-------------------
SELECT IteR
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
	   REPLACE periodo WITH m.periodo, nummes WITH PADL(MONTH(m.fecabo),2,'0'), numero WITH m.nroabo, codctc WITH m.codctc, codban WITH '002' 
	   REPLACE transa WITH 'ABO', fecha WITH m.fecabo, motivo WITH '14', tipdoc WITH 'N/A', numref WITH m.numdoc, tipfun WITH m.tipfun,usuario WITH wuser_id
	   REPLACE codfte WITH m.codfte, nombre WITH 'AUT. GIRO TESORO PUBLICO', monto WITH m.importe, estado WITH '00', codcal WITH ALLTRIM(m.periodo)+m.nummes+m.codfte+'01001'
	ENDIF
	UNLOCK ALL
ELSE	
    SEEK ALLTRIM(m.nummes+m.nroabo)
    IF FOUND()
	   REPLACE periodo WITH m.periodo, nummes WITH PADL(MONTH(m.fecabo),2,'0'), numero WITH m.nroabo, codctc WITH m.codctc, codban WITH '002' 
	   REPLACE transa WITH 'ABO', fecha WITH m.fecabo, motivo WITH '14', tipdoc WITH 'N/A', numref WITH m.numdoc, tipfun WITH m.tipfun,usuario WITH wuser_id
	   REPLACE codfte WITH m.codfte, nombre WITH 'AUT. GIRO TESORO PUBLICO', monto WITH m.importe, estado WITH '00', codcal WITH ALLTRIM(m.periodo)+m.nummes+m.codftE+'01001'
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
SEEK PADL(MONTH(m.fecabo),2,'0')+ALLTRIM(m.Nroabo)+ALLTRIM(m.Codctc)
IF !FOUND()
	DO agrite
ENDIF
BROWSE FIELDS ;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999,999,999.99';
	WINDOW wind_2 KEY PADL(MONTH(m.fecabo),2,'0')+ALLTRIM(m.Nroabo)+ALLTRIM(m.Codctc)
STORE 0 TO vdebe, vhaber ,vRet
SEEK PADL(MONTH(m.fecabo),2,'0')+m.Nroabo+m.Codctc
mmonto=mtohab+mtodeb  &&Para la 101
SCAN WHILE PADL(MONTH(m.fecabo),2,'0')=NumMes AND ALLTRIM(m.Nroabo)=NumREF AND ALLTRIM(m.Codctc)=CodCtC
     IF IIF(TipCta='D',MtoDeb,MtoHab)=0
    	DELE NEXT 1
     ELSE
	 	vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
     ENDIF
ENDSCAN
IF ingreso and sw=0
	IF f_appd()
		REPLACE nummes WITH PADL(MONTH(m.fecabo),2,'0'), numref WITH m.nroabo, tipdoc WITH 'N/A',codctc WITH m.codctc, fecha WITH m.FecABO,TIPCTC WITH W_TIPCTC
		REPLACE codcta WITH '1010201000',Mtodeb WITH mmonto, Tipcta WITH 'D'
	ENDIF
	IF f_appd()
		REPLACE nummes WITH PADL(MONTH(m.fecabo),2,'0'), numref WITH m.nroabo, tipdoc WITH 'N/A',codctc WITH m.codctc, fecha WITH m.FecABO,TIPCTC WITH W_TIPCTC
		REPLACE codcta WITH '1010201000',Mtohab WITH mmonto, Tipcta WITH 'H'
	ENDIF
	DO vis_cen
   else 
	* si es corrije solo modifica
	SEEK PADL(MONTH(m.fecabo),2,'0')+m.nroabo+alltrim(m.codctc)
    mmonto=mtohab+mtodeb  &&Para la 101
	IF FOUND()
		SCAN WHILE PADL(MONTH(m.fecabo),2,'0')=nummes AND m.nroabo=numref AND m.codctc=alltrim(codctc)
		    IF codcta='1010201000' and tipcta='D'
	        	REPLA Mtodeb WITH mmonto
	        ENDIF
	        IF codcta='1010201000' and tipcta='H'
	        	REPLA Mtohab WITH mmonto
	        ENDIF
		ENDSCAN
    ELSE		
		IF f_appd()
			REPLACE nummes WITH PADL(MONTH(m.fecabo),2,'0'), numref WITH m.nroabo, tipdoc WITH 'N/A',codctc WITH m.codctc, fecha WITH m.FecABO,TIPCTC WITH W_TIPCTC
			REPLACE codcta WITH '1010201000',Mtodeb WITH mmonto, Tipcta WITH 'D'
		ENDIF
		IF f_appd()
			REPLACE nummes WITH PADL(MONTH(m.fecabo),2,'0'), numref WITH m.nroabo, tipdoc WITH 'N/A',codctc WITH m.codctc, fecha WITH m.FecABO,TIPCTC WITH W_TIPCTC
			REPLACE codcta WITH '1010201000',Mtohab WITH mmonto, Tipcta WITH 'H'
		ENDIF
	ENDIF	
	DO vis_cen
ENDIF
IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
ON KEY

SELECT RevS

ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5

UNLOCK
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
 
RETURN


PROCEDURE vis_cen &&Visualizacentralizaci¢n de caja
*----------------
ACTIVATE WINDOW wind_7
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY 'Debe '
@ 00,34  SAY 'Haber '
@ 01,04  SAY '1010201000'
@ 01,18  SAY mmonto PICTURE '999,999,999.99' 
@ 02,12  SAY '1010201000'
@ 02,34  SAY mmonto PICTURE '999,999,999.99' 
DO standby WITH 'Visualizando los Movimientos'
DEACTIVATE WINDOW wind_7
return

PROCEDURE agrite
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH PADL(MONTH(m.fecabo),2,'0'),;
	        numref WITH m.nroabo,;
	        Periodo WITH m.periodo,;
	        tipdoc WITH 'N/A',;
	        codctc WITH m.codctc,;
	        fecha  WITH m.FecABO,;
	        tipctc WITH w_tipctc
ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT astpat
IF RLOCK()
	DELETE NEXT 1
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
   SELECT RevS
   vNummes=RevS.Nummes
*   vNummes1=PADL(MONTH(RevS.fecabo),2,'0')
   vnumrev=ALLTRIM(RevS.numrev)
*   vabono =ALLTRIM(RevS.Nroabo)
*   vcodctc=ALLTRIM(RevS.Codctc)
   IF f_lock(1)
      DELETE
      *-Busca y elimina los Items
      SELECT IteR
      GO TOP
      SEEK ALLTRIM(vNummes)+vnumrev
      IF FOUND()
         SCAN WHILE vNummes = ALLTRIM(IteR.Nummes) .AND. vnumrev = IteR.numrev 
              IF f_lock(1)
                 DELETE NEXT 1
              ENDIF
         ENDSCAN
      ENDIF
      SELECT RevS
      IF !BOF()
         SKIP -1
      ELSE
         IF !EOF()
            SKIP
         ENDIF
      ENDIF
*      SELECT Movb
*      SEEK ALLTRIM(vNummes1)+vabono
*      IF FOUND()
*         IF f_lock(1)
*            DELETE NEXT 1
*         ENDIF
*      ENDIF
*      *-Busca y elimina los Asientos
*      SELECT astpat
*      GO TOP
*      SEEK ALLTRIM(vNumMes1)+ALLTRIM(vabono)+ALLTRIM(vcodctc)
*      IF FOUND()
*         SCAN WHILE vNumMES1 = ALLTRIM(Astpat.Nummes) .AND. vabono = ALLTRIM(Astpat.Numref) .AND. vcodctc= ALLTRIM(Astpat.codctc)
*              IF f_lock(1)
*                 DELETE NEXT 1
*              ENDIF
*         ENDSCAN
*      ENDIF
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
 SELECT RevS
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 STORE SPACE(2) TO vnummes
 STORE SPACE(4) TO vnumrev 
 rec = RECNO()
 SCATTER MEMVAR
 vnummes = m.nummes
 vnumrev = m.numrev
 vcodscta= m.codscta
 vtiplis = 1
DEFINE WINDOW lis FROM 4,10 TO 19,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 00,01 SAY "     Tipo Listado : "  GET vtiplis FUNCTION '^ Documento;Cdro.Demost. X Subcta.;Cdro.Demos. de SubCta.'
@ 06,01 SAY "     N§ Documento : "
@ 06,24 SAY "."
@ 06,22 GET vnummes  WHEN vtiplis = 1 PICTURE "!!"
@ 06,25 GET vnumrev  WHEN vtiplis = 1 PICTURE "!!!!" VALID ValAut()
@ 08,01 SAY "       Sub Cuenta : " 
@ 08,22 GET vcodscta WHEN vtiplis = 3
@ 13,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
xmes=val(vnummes)
xAno =VAL(m.Periodo)
RELEASE WINDOW lis
IF okcancel = 1
	   SELE RevS
	   SET RELATION TO Allt(NumMes)+numrev INTO IteR
	   SET FILT TO allt(nummes)+numrev=ALLTRIM(vnummes)+vnumrev
	   SET SKIP TO IteR
	   SEEK ALLTRIM(vnummes)+vnumrev
	   IF FOUND()
		  DO REPORTE WITH 2,"FtoRev"," Reversi¢n de Saldos poe Menores Utilizaciones",1,.F.,.T.
	   ELSE
    	  DO STANDBY WITH vmens08	  
	   ENDIF
	   SET SKIP TO
	   SET FILT TO
	   SET RELA TO
ENDIF      	   
SELECT RevS
IF rec > 0
   GO rec
ENDIF
DO vista
RETURN


FUNCTION ofigir
*-------------
PARAMETERS A
PUBLIC  _y, _x
STORE ' ' TO _y,_x
vAli=ALIAS()
vOrd=ORDER()
SELE CAJA
SEEK ALLT(IteR.Codctc)
_y=ALLT(Caja.CodOfg)
_x=Val_Para(_y,'OFIGIR','D')
SELECT (vAli)
SET ORDER TO (vOrd)
IF a=2
   REPLACE CodOfg WITH _y
ENDIF
RETURN _x


FUNCTION Posi
*------------
vOrd=ORDER()
SELECT MEFC
SET RELATION TO 
SET ORDER TO MEFCal1
SEEK ALLTRIM(m.Periodo)+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(m.CodPrg)+ALLTRIM(m.TipFun)+ALLTRIM(m.CodPart)
IF FOUND()
   REPLACE MEFC.GirMEF WITH m.Importe
ENDIF   
SELECT IteR
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
IF IteR.Importe # AntCan
   IF EMPTY(IteR.CodAct)
      SET ORDER TO Calen7
      SEEK IteR.CodPrg+IteR.CodFte+IteR.CodSubpr+IteR.CodProy+IteR.CodSuPry+IteR.CodPart
      IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
         IF IteR.Importe > AntCan
            REPLACE Calen.GirMEF WITH Calen.GirMEF  + (IteR.Importe - AntCan)
            IF Calen.GirMEF < Calen.ValPart
               DO Standby WITH "El Monto es inferior al Aprobado"
            ELSE
               DO Standby WITH "ERROR!... El giro excede el Total Aprobado"
            ENDIF
            vSuma = vSuma + IteR.Importe
            IF vSuma > m.importe
               SEEK IteR.CodPrg+IteR.CodFte+IteR.CodSubpr+IteR.CodProy+IteR.CodSuPry+IteR.CodPart
               IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
                  REPLACE Calen.GirMEF  WITH Calen.GirMEF - (IteR.Importe - AntCan)
                  REPLACE IteR.Importe   WITH 0
                  vSuma = vSuma - IteR.Importe
               ENDIF
               DO Standby WITH "ERROR!!!. Suma excede el Total del Credito"
            ENDIF
         ELSE
            REPLACE Calen.GirMEF  WITH Calen.GirMEF - (AntCan - IteR.Importe)
            IF Calen.GirMEF < Calen.ValPart
               DO Standby WITH "El Monto es inferior al Aprobado"
            ELSE
               DO Standby WITH "ERROR!... El giro excede el Total Aprobado"
            ENDIF
            vSuma = vSuma + IteR.Importe            
            IF vSuma > m.importe
               SEEK IteR.CodPrg+IteR.CodFte+IteR.CodSubpr+IteR.CodProy+IteR.CodSuPry+IteR.CodPart
               IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
                  REPLACE Calen.GirMEF WITH Calen.GirMEF  + (AntCan - IteR.Importe)
                  REPLACE IteR.Importe WITH 0
                  vSuma = vSuma + IteR.Importe                  
               ENDIF
               DO Standby WITH "ERROR!!!. Suma excede el Total del Credito"
            ENDIF
         ENDIF
      ENDIF
   ELSE
      SET ORDER TO Calen8
      SEEK IteR.CodPrg+IteR.CodFte+IteR.CodSubpr+IteR.CodAct+IteR.CodPart
      IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
         IF IteR.Importe > AntCan
            REPLACE Calen.GirMEF WITH Calen.GirMEF + (IteR.Importe - AntCan)
            IF Calen.GirMEF < Calen.ValPart
               DO Standby WITH "El Monto es inferior al Aprobado"
            ELSE
               DO Standby WITH "ERROR!... El giro excede el Total Aprobado"
            ENDIF
            vSuma = vSuma + IteR.Importe            
            IF vSuma > m.importe
               SEEK IteR.CodPrg+IteR.CodFte+IteR.CodSubpr+IteR.CodAct+IteR.CodPart
               IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
                  REPLACE Calen.GirMEF WITH Calen.GirMEF - (IteR.Importe - AntCan)
                  REPLACE IteR.Importe WITH 0
                  vSuma = vSuma - IteR.Importe                  
               ENDIF
               DO Standby WITH "ERROR!!!. Suma excede el Total del Credito"
            ENDIF
         ELSE
            REPLACE Calen.GirMEF WITH Calen.GirMEF - (AntCan - IteR.Importe)
            IF Calen.GirMEF < Calen.ValPart
               DO Standby WITH "El Monto es inferior al Aprobado"
            ELSE
               DO Standby WITH "ERROR!... El giro excede el Total Aprobado"
            ENDIF
            vSuma = vSuma + IteR.Importe            
            IF vSuma > m.importe
               SEEK IteR.CodPrg+IteR.CodFte+IteR.CodSubpr+IteR.CodAct+IteR.CodPart
               IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
                  REPLACE Calen.GirMEF WITH Calen.GirMEF + (AntCan - IteR.Importe)
                  REPLACE IteR.Importe WITH 0                  
                  vSuma = vSuma - IteR.Importe                  
               ENDIF
               DO Standby WITH "ERROR!!!. Suma excede el Total del Credito"
            ENDIF
         ENDIF
      ENDIF
   ENDIF
ENDIF
SELECT IteR
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
vKey=ALLTRIM(IteR.Codprg)+ALLTRIM(IteR.CodSubPr)+ALLTRIM(IteR.CodProy)
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
IF EMPTY(IteR.CodSuPry)
   SEEK IteR.CodPrg+IteR.CodFte+IteR.CodSubPr+IteR.CodProy+;
        IteR.CodSuPry+IteR.CodPart
   IF FOUND()
      RETURN Calen.UbiCac
   ENDIF
ELSE
   IF !EMPTY(IteR.codsubpr)
      SEEK IteR.CodPrg+IteR.CodFte+IteR.CodSubPr+IteR.CodProy+;
      IteR.CodSuPry+IteR.CodPart
      IF FOUND()
         RETURN Calen.ubicac
      ENDIF
   ENDIF
ENDIF
SELECT (vAli)
SET ORDER TO TAG vOrd


FUNCTION REMBYS
*--------------
vAli=ALIAS()
vOrd=ORDER()
vSubCta=LEFT(RevS.CodSCta,1)
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
SELECT RevS
SEEK ALLTRIM(NumDoc)+numrev
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
 SELECT RevS
 vTemp=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK Vnummes+Vnumrev
 IF !FOUND()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   Nummes   :H= 'Mes' ,;
   numrev  			 ,;
   CodScta  :H= 'Sub_Cuenta',;
   Fecha    :H= 'Fecha',;
   Nombre  
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
 vnumrev= numrev
 vcodSCTA= codSCTA
 RETURN .T.

FUNCTION vctc
*------------
SELE subcta
m.codctc=subcta.ctarec
RETURN

FUNCTION CtaTp
*-------------
xAli=ALIAS()
xOrd=ORDER()
SELE CAJA
wOrd=ORDER()
aind=SYS(3)+'.IDX'
INDEX ON clase TO (aind)
SEEK 'T'
m.ctates=ALLT(CAJA.codctc)
SET INDEX TO
CLOSE INDEX
ERASE (aind)
@ 06,16 SAY m.CtaTes      
SET ORDER TO (wOrd)
SELECT (xAli)
SET ORDER TO (xOrd)
RETURN

FUNCTION Val_ctc
*----------------
PARAMETER v__al, v__dev, v__bus, v__cod, v__tipo, v__x, v__y, v__ver, v__Long
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWnd,_campo, mVali, mRec, v__x, v__y, v__lon
* Alias :     v__al  " "
* Var.Devol.: v__dev " "
* Var.Mostr.: v__bus ( en el popup ) " expr"
* Var.Modif.: v__cod   variable
* Tipo      : v__tipo : 1=variable    2=campo   3=solo valida
* Posiciones: v__x, v__y   para el v__bus
* A ver     : v__Ver     Usado para visualizar, si no se pone, usa v__bus
* Long.A ver: v__Long    Usado para mostrar el tama¤o de v__Ver

mEdita = (parameters()>=5) .and.  (v__tipo # 3)
v__Ver = IIF(PARAMETERS()>=8,v__ver,v__bus)
mRec   = IIF(EOF(),-1,RECNO())
mAlias = ALIAS()
_campo = TRIM(mAlias)+'.'+VARREAD()
SELECT &v__al
SET FILT TO codscta=m.codscta AND ofigir='S'
_OldWnd = WOUTPUT()
IF EMPTY(&_campo)
   GO TOP
   IF EOF()
	  DO standby WITH "²²²²²²²² NO HAY VALORES PARA ELEGIR ²²²²²²²²"
	  v_fun = .F.
   ELSE
	  _ln = LEN(&v__bus)
	  _ln = IIF(_ln>65,65,_ln)
	  ACTIVATE SCREEN
	  DEFINE POPUP v__xx FROM 1,80-_ln TO 12,79 PROMPT FIELD &v__bus
	  ON SELECTION POPUP v__xx DEACTIVATE POPUP
	  ACTIVATE POPUP v__xx
	  RELEASE POPUP v__xx
	  IF !EMPTY(_OldWnd)
	      ACTIVATE WINDOW &_OldWnd
	  ENDIF
	  IF Lastkey()=27
	     v_fun = .f.
	  ELSE
	     v__cod = &v__dev
         IF !EMPTY(mAlias)
		    SELECT (mAlias)
	        IF mRec>0
				     GO mRec
					 REPLACE &_campo WITH v__cod
					 v_fun = .T.
			ELSE
					 DO standby WITH " El archivo est  vac¡o "
					 v_fun = .F.
			ENDIF
	     ENDIF	   
	  ENDIF
   ENDIF 
ELSE
 	  SEEK &_campo
	  v_fun = FOUND()
ENDIF
SELE &v__al
SET FILT TO
IF EMPTY(mAlias)
   SELECT 0
ELSE
   SELECT (mAlias)
ENDIF

IF !EMPTY(ALIAS())
   IF mRec>0
	  GO mRec
   ELSE
 	  GO BOTTOM
   ENDIF
ENDIF
RETURN v_fun


PROCEDURE total
*--------------
 xtot=0
 SELE Iter
 SEEK ALLT(m.nummes)+m.numrev
 SCAN WHILE Iter.nummes=ALLT(m.nummes) AND Iter.numrev=m.numrev 
 	  xtot=xtot+Iter.importe 
 ENDSCAN	  
 ACTIVATE WINDOW wind_3
 @ 0,1 SAY xtot PICTURE '9,999,999.99'
 RETURN



FUNCTION PidApNc
*---------------
 DEFINE WINDOW Wind_7 FROM 10,20 TO 16,60  DOUBLE ;
 TITLE " Asientos para N/C " COLOR SCHEME 10
 vdeb = SPACE(10)
 vhab = SPACE(10)
 ACTIVATE WINDOW Wind_7
 alia  = ALIAS()
 SELECT ITER
 SEEK ALLTRiM(m.nummes)+m.numRev
 IF FOUND()
 	vnrocar = Iter.NroCar &&Nro de la Nota de Cargo
 	SELECT ASTPAT
    Vord = ORDER()
    SET ORDER TO AstPat5
    SEEK ALLTRIM(m.nummes)+vnrocar+ALLT(Iter.CodCtc)
    IF FOUND()
       Vdeb=CodCta
       SKIP
       Vhab=CodCta
    ENDIF
    SELECT ASTPAT
    SET ORDER TO (Vord)   
 ENDIF 	
 vfecbco=DATE()
 @ 1, 10 say "Fecha de Bancos" GET vfecbco PICTURE '@E'
 @ 2, 10 SAY "Debe"
 @ 2, 25 SAY "Haber"
 @ 3, 10 GET vdeb 
 @ 3, 25 GET vhab 
 READ VALID Val_read()
 DEACTIVATE WINDOW Wind_7
 SELECT (alia)
 RETURN vdeb+vhab


PROCEDURE ingap1   && Ingresa asientos patRimoniales para Notas de Cargo
*-------------------
PARAMETERS vasiento
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgRite
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)

SELE ASTPAT
xord = ORDER()
SET ORDER TO ASTPAT5

SELECT IteR
SEEK ALLTRiM(m.NumMes)+m.NumRev
Vperiodo = m.periodo
vnummes  = m.nummes
vnumRev  = m.numRev
SCAN WHILE ALLTRiM(vNumMes) + ALLTRiM(vNumRev) =ALLTRiM(NumMes) + ALLTRiM(NumRev)
	SELECT Movb
	SEEK PADL(MONTH(Iter.fecbco),2,'0')+Iter.NroCar
	IF !FOUND()
		APPEND BLANK
		REPLACE PeRiodo 	WITH vpeRiodo ,;
				Nummes  	WITH PADL(MONTH(Iter.fecbco),2,'0'),;
				Nummesc 	WITH PADL(MONTH(Iter.fecbco),2,'0'),;
				numero  	WITH Iter.NroCar ,;
				Transa  	WITH 'CAR' ,;
				Fecha   	WITH Iter.fecbco ,;
				FechaC  	WITH Iter.fecbco ,;
				Tipdoc  	WITH 'N/C' ,;
				Numref  	WITH IteR.numrev ,;
				codfte  	WITH 'TRN',;
				codctc  	WITH Iter.CodCtc ,;
				nombre  	WITH 'REVERSION DE SALDOS NO UTILIZADOS '+m.numREV ,;
				forma   	WITH 'EF' ,;
				Monto   	WITH IteR.importe ,;
				estado      WITH '00' ,;
				motivo      WITH '23',;				
				codban   	WITH wbanco,;
				usuario     WITH wuser_id
	ELSE
		REPLACE PeRiodo WITH IteR.peRiodo ,;
				Transa  	WITH 'CAR' ,;
				Fecha   	WITH Iter.fecbco ,;
				FechaC  	WITH Iter.fecbco ,;
				Tipdoc  	WITH 'N/C' ,;
				Numref  	WITH IteR.numrev ,;
				codfte  	WITH 'TRN',;
				codctc  	WITH Iter.CodCtc ,;
				nombre  	WITH 'REVERSION DE SALDOS NO UTILIZADOS '+m.numREV ,;
				forma   	WITH 'EF' ,;
				Monto   	WITH IteR.importe ,;
				estado      WITH '00' ,;
				motivo      WITH '23',;				
				codban   	WITH wbanco,;
				usuario     WITH wuser_id
				
	ENDIF		
	SELECT Astpat
	SEEK PADL(MONTH(Iter.fecbco),2,'0')+Iter.NroCar+Iter.CodCtc
	IF !FOUND()
		APPEND BLANK
		REPLACE PeRiodo WITH IteR.peRiodo ,;
				tipdoc  WITH 'N/C' ,; 
				fecha   WITH IteR.fecbco ,;
				Nummes  WITH PADL(MONTH(Iter.fecbco),2,'0'),;
				Numref  WITH Iter.NroCar;
				tipcta  WITH 'D',;
				codctc  WITH IteR.CodcTc ,;
				codcta  WITH LEFT(vasiento,10),;
				Mtodeb  WITH IteR.importe ,;
				tipctc  WITH w_tipctc
		APPEND BLANK
		REPLACE PeRiodo WITH IteR.peRiodo ,;
				tipdoc  WITH 'N/C' ,; 
				fecha   WITH IteR.fecbco ,;
				Nummes  WITH PADL(MONTH(Iter.fecbco),2,'0'),;
				Numref  WITH Iter.NroCar;
				tipcta  WITH 'H',;
				codctc  WITH IteR.CodcTc ,;
				codcta  WITH RIGHT(vasiento,10),;
				Mtodeb  WITH IteR.importe ,;
				tipctc  WITH w_tipctc
	ELSE
		REPLACE PeRiodo WITH IteR.peRiodo ,;
				tipdoc  WITH 'N/C' ,; 
				fecha   WITH IteR.fecbco ,;
				Nummes  WITH PADL(MONTH(Iter.fecbco),2,'0'),;
				Numref  WITH Iter.NroCar;
				tipcta  WITH 'D',;
				codctc  WITH IteR.CodcTc ,;
				codcta  WITH LEFT(vasiento,10),;
				Mtodeb  WITH IteR.importe ,;
				tipctc  WITH w_tipctc
		SKIP
		REPLACE PeRiodo WITH IteR.peRiodo ,;
				tipdoc  WITH 'N/C' ,; 
				fecha   WITH IteR.fecbco ,;
				Nummes  WITH PADL(MONTH(Iter.fecbco),2,'0'),;
				Numref  WITH Iter.NroCar;
				tipcta  WITH 'H',;
				codctc  WITH IteR.CodcTc ,;
				codcta  WITH RIGHT(vasiento,10),;
				Mtodeb  WITH IteR.importe ,;
				tipctc  WITH w_tipctc
	ENDIF
	SELECT IteR
ENDSCAN	
SELECT Astpat
SET ORDER TO (xord)
SELE Revs
RETURN


FUNCTION wfec
*------------
m.nummes=PADL(MONTH(m.fecha),2,'0')
RETURN
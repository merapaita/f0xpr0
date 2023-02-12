* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ CarOrd.PRG       10/09/96                           L: 0798  ³	
* ³ Registro y Emisi¢n de Cartas Orden al Banco de la Naci¢n     ³
* ³ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09
Vmens01 = 'Revisi¢n de Cartas Orden de Transferencias'
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '

*- Abre archivos

USE CarOrd   IN  1  ORDER TAG CarOrd1                 ALIAS CarO
USE IteCaOrd IN  2  ORDER TAG IteCaOrd1               ALIAS IteC
USE Augire   IN  13 ORDER TAG Augire1                 ALIAS AutG
USE Parmae   IN  4  ORDER TAG Parmae1                 ALIAS Parma
USE Cuentas  IN  8  ORDER TAG Cuentas1                ALIAS Cuenta
USE Cajas    IN  9  ORDER TAG Cajas1                  ALIAS Caja
USE SubCtas  IN 11  ORDER TAG Subctas1                ALIAS Subcta

*- Variables de trabajo (registro a trabajar)
PUBLIC vFun,m.fecha,vMes,mmonto,Sw,vfecbco,wdesc,vconf,VUSER_ID,wuser_id
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
vfecbco=DATE()

STORE   .T.	 TO 	Agrega,vFun
STORE DATE() TO 	m.fecha
STORE    0   TO		mmonto,Sw

*- Inicia proceso
SELECT CarO
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

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 08,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_C2 FROM 08,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C3 FROM 12,00 TO 23,38 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C4 FROM 00,00 TO 23,79 DOUBLE;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Carta Orden de TRansferencias' ;

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
 @ 00,02 SAY "Carta Orden :"  
 @ 01,02 SAY "      Fecha :" 
 @ 01,40 SAY "    Periodo :" 
 @ 02,02 SAY "        Mes :" 
 @ 03,02 SAY "     Nombre :" 
 @ 04,02 SAY "    Sub_Cta :" 
 @ 05,02 SAY " Cta.Matriz :" 
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT CarO
 SCATTER MEMVAR
 @  0,16 SAY m.numcar
 @  1,16 SAY m.Fecha
 @  1,55 SAY m.Periodo
 @  2,16 SAY m.NumMes
 @  3,16 SAY m.nombre 
 @  4,16 SAY m.CodScta
 @  5,16 SAY m.codctc
 DO Vista_Hijo
 DO total
RETURN


PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
 SELECT IteC
 SEEK ALLTRIM(m.nummes)+m.numcar
 BROWSE;
   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
   WINDOW Wind_C2 KEY ALLTRIM(m.nummes)+m.numcar;
   FIELDS;
   CodCtc    :H='Cta.Cte.' ,;
   xx=ofigir(1) :H='Of. Giradora',;
   Importe   :P='999,999,999,999.99'
  SELECT  CarO
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT CarO
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
   numcar   :H= 'Carta Orden' ,;
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
 SELECT CarO
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
    SELECT CarO
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
 SELECT CarO
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
 SELECT CarO
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
 SELECT CarO
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
 =vscta(2)
 @ 00,16 SAY m.numcar
 @ 01,16 SAY m.Fecha       PICTURE '@E'  
 @ 01,55 SAY m.Periodo     PICTURE '!!' 
 @ 02,16 SAY m.NumMes      PICTURE '!!'  
 @ 03,16 GET m.nombre      PICTURE '@!'
 @ 04,16 SAY m.CodScta     PICTURE "@!"  
 @ 04,31 SAY wdesc
 @ 05,16 SAY m.Codctc      
 READ VALID Val_Read()
 m.usuario=wuser_id
 IF LASTKEY() # 27
    SELECT CarO
    GATHER MEMVAR
    SCATTER MEMVAR
    ss= Tra_Hijo()     && Corrige items
    IF SS .AND. LASTKEY()#27
       SELECT IteC
       GATHER MEMVAR
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 DO total
 SELECT CarO
 DO Vista
RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT CarO
 DO pantalla
 SCATTER MEMVAR BLANK
 m.Fecha=DATE()
 m.Periodo=RIGHT(DTOC(DATE()),2)
 vMes=STR(MONTH(DATE()))
 ingreso=.T.
 SELECT Parma
 SEEK 'CORREL'+'CARORD'
 m.numcar = PADL(NumEnt+1,3,'0')
 m.nombre='ROMAN MORALES ROCHA'+SPACE(27)
 @ 00,16 SAY m.numcar
 = vscta(1)
 @ 01,16 SAY m.Fecha       PICTURE '@E' 
 @ 01,55 SAY m.Periodo     PICTURE '!!' 
 @ 02,16 SAY m.NumMes      
 @ 03,16 GET m.nombre      PICTURE '@!' 
 @ 04,16 SAY m.CodScta     PICTURE "@!"  
 @ 04,31 SAY wdesc
 @ 05,16 SAY m.Codctc      
 m.usuario=wuser_id
 READ VALID Val_Read()
  IF LASTKEY()#27 
     vAnula = .F.
     SELECT CarO
     IF F_Appd()
        GATHER MEMVAR
        UNLOCK ALL
        SELECT Parma
        SEEK 'CORREL'+'CARORD'
        REPLACE NumEnt WITH NumEnt + 1       
        SELECT CarO
        SCATTER MEMVAR
        SELECT IteC
        DO WHILE .T.
          DO Tra_hijo
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
 SELECT CarO
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
 SELECT IteC
 GO TOP
 SEEK ALLTRIM(m.nummes)+m.numcar
 IF !FOUND()
    DO captu_Item
 ENDIF

 BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
  KEY ALLTRIM(m.nummes)+m.numcar;
  FIELDS;
   Codctc   : H='Cta.Cte'   :V=val_ctc("Caja","CodCtc","CodCtc+' '+Descri",Codctc,2) :f,;
   xx=ofigir(2) :H='Of. Giradora',;
   Importe  : H='      Monto      ':P='999,999,999,999.99'
 vOrd=ORDER()
 SELECT IteC
 SET ORDER TO (vOrd)
 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5
 UNLOCK ALL
SELECT CarO
ACTIVATE SCREEN
vTempo = ' Ingresa  Elige  Busca  Anterior  Siguiente  Corrige  Anula  Lista Termina '
DO Logos WITH Rotulo1,vtempo
SHOW MENU mMenu
RETURN  vFun


PROCEDURE Agreg_item
*-------------------
SELECT IteC
IF F_Appd()
   REPLACE Nummes   WITH m.Nummes ,;
           numcar   WITH m.numcar ,;
           periodo  WITH m.periodo  
ENDIF
UNLOCK
RETURN


PROCEDURE Elimi_item
*-------------------
SELECT IteC
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
   SELECT CarO
   vNummes=CarO.Nummes
   vnumcar=ALLTRIM(CarO.numcar)
   IF f_lock(1)
      DELETE
      *-Busca y elimina los Items
      SELECT IteC
      GO TOP
      SEEK ALLTRIM(vNummes)+vnumcar
      IF FOUND()
         SCAN WHILE vNummes = ALLTRIM(IteC.Nummes) .AND. vnumcar = IteC.numcar 
              IF f_lock(1)
                 DELETE NEXT 1
              ENDIF
         ENDSCAN
      ENDIF
      SELECT CarO
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
 SELECT CarO
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 STORE SPACE(2) TO vnummes
 STORE SPACE(4) TO vnumcar 
 rec = RECNO()
 SCATTER MEMVAR
 vnummes = m.nummes
 vnumcar = m.numcar
 vcodscta= m.codscta
 vtiplis = 1
DEFINE WINDOW lis FROM 4,10 TO 19,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 00,01 SAY "     Tipo Listado : "  GET vtiplis FUNCTION '^ Documento'
@ 06,01 SAY "     N§ Documento : "
@ 06,24 SAY "."
@ 06,22 GET vnummes  WHEN vtiplis = 1 PICTURE "!!"
@ 06,25 GET vnumcar  WHEN vtiplis = 1 PICTURE "!!!!" VALID ValAut()
@ 08,01 SAY "       Sub Cuenta : " 
@ 08,22 GET vcodscta WHEN vtiplis = 3
@ 13,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
xmes=val(vnummes)
xAno =VAL(m.Periodo)
RELEASE WINDOW lis
IF okcancel = 1
	   SELE CarO
	   SET RELATION TO Allt(NumMes)+numcar INTO IteC
	   SET FILT TO allt(nummes)+numcar=ALLTRIM(vnummes)+vnumcar
	   SET SKIP TO IteC
	   SEEK ALLTRIM(vnummes)+vnumcar
	   IF FOUND()
		  DO REPORTE WITH 2,"FtoCaOrd"," Carta Orden para Transferencias",1,.F.,.T.
	   ELSE
    	  DO STANDBY WITH vmens08	  
	   ENDIF
	   SET SKIP TO
	   SET FILT TO
	   SET RELA TO
ENDIF      	   
SELECT CarO
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
SEEK ALLT(IteC.Codctc)
_y=ALLT(Caja.CodOfg)
_x=Val_Para(_y,'OFIGIR','D')
SELECT (vAli)
SET ORDER TO (vOrd)
IF a=2
   REPLACE CodOfg WITH _y
ENDIF
RETURN _x


FUNCTION REMBYS
*--------------
vAli=ALIAS()
vOrd=ORDER()
vSubCta=LEFT(CarO.CodSCta,1)
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
SELECT CarO
SEEK ALLTRIM(NumDoc)+numcar
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
 SELECT CarO
 vTemp=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK Vnummes+Vnumcar
 IF !FOUND()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   Nummes   :H= 'Mes' ,;
   numcar  			 ,;
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
 vnumcar= numcar
 vcodSCTA= codSCTA
 RETURN .T.



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
 SELE IteC
 SEEK ALLT(m.nummes)+m.numcar
 SCAN WHILE IteC.nummes=ALLT(m.nummes) AND IteC.numcar=m.numcar 
 	  xtot=xtot+IteC.importe 
 ENDSCAN	  
 ACTIVATE WINDOW wind_3
 @ 0,1 SAY xtot PICTURE '9,999,999.99'
 RETURN


PROCEDURE Captu_Item
*-------------------
 wali = ALIAS()
 word = ORDER()
 SELE Autg
 SET ORDER TO AUGIRE3
 SEEK m.numcar
 SCAN WHILE Autg.carref=m.numcar 
      SELE IteC
	  IF f_appd()
	     REPLACE periodo WITH m.periodo,;
	     		 nummes  WITH m.nummes,;
	     		 numcar  WITH m.numcar,;
	     		 codctc  WITH Autg.CtcOfg,;
	     		 codofg  WITH Autg.CodOfg,; 
	     		 importe WITH Autg.totasig
	  ENDIF   		 
	  SELE Autg
 ENDSCAN	  
 SELE AutG
 SET ORDER TO Augire1
 SELE (wali)
 SET ORDER TO (word)
 RETURN


FUNCTION vscta
*--------------
PARAMETERS a
yali = ALIAS()
yord = ORDER()
IF a=1
    SELE AUtG
	SET ORDER TO Augire3
	SEEK m.numcar
	IF FOUND()
	   m.codscta=Autg.Codscta
	   m.fecha  =Autg.Fecha
	   m.nummes=PADL(MONTH(m.fecha),2,'0')
	   m.codctc=ALLT(Autg.CodCtc)
	   SELE Subcta
	   SEEK m.codscta
	   wdesc   =ALLT(Subcta.Descri)
	ELSE
	   DO STANDBY WITH 'Carta de Referencia sin giros, Esc para salir' 
		SELE (Yali)
		SET ORDER TO (Yord)
	   RETURN 
	ENDIF      
	SELE AutG
	SET ORDER TO Augire1
ELSE	
   SELE Subcta
   SEEK m.codscta
   wdesc   =ALLT(Subcta.Descri)
ENDIF   
SELE (Yali)
SET ORDER TO (Yord)
RETURN
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ Transfon.PRG  09/07/96                            L: 1248    ³	
* ³ Transferencia de Fondos                                      ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09
Vmens01 = 'Revisi¢n de Transferencia de Fondos'
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '

*- Abre archivos

USE TransFon IN  1 ORDER TAG TransFon1               ALIAS Tran
USE IteTf    IN  2 ORDER TAG IteTf1               	 ALIAS itef
USE Parmae   IN  4 ORDER TAG Parmae1                 ALIAS Parma
USE AstPat   IN  7 ORDER TAG AstPat6   				 ALIAS AstPat
USE Cuentas  IN  8 ORDER TAG Cuentas1                ALIAS Cuenta
USE Cajas    IN  9 ORDER TAG Cajas8                  ALIAS Caja
USE MovBco   IN 10 ORDER TAG Movbco11        		 ALIAS Movb
*- Variables de trabajo (registro a trabajar)
PUBLIC vFun,m.fecha,vMes,mmonto,Sw,vfecbco,wdesc,vconf,W_ctaH,w_tipctc,wbanco,xtot,VUSER_ID,wuser_id,wbanco
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
vfecbco=DATE()

STORE   .T.	 TO 	Agrega,vFun
STORE DATE() TO 	m.fecha
STORE    0   TO		mmonto,Sw

*- Inicia proceso
SELECT Tran
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
 TITLE 'Detalle: Cheques de Gerencia' ;

DEFINE WINDOW wind_3 FROM 20,61 TO 22,78 ;
	TITLE ' TOTAL ' COLOR SCHEME 10  


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
 @ 00,02 SAY "  N§ Oficio :"  
 @ 01,02 SAY "      Fecha :" 
 @ 01,40 SAY "    Periodo :" 
 @ 02,02 SAY "        Mes :" 
 @ 03,02 SAY "     Cuenta :" 
 @ 04,02 SAY "     Nombre :" 
 @ 05,02 SAY "       Tipo :" 
 @ 06,02 SAY "   N§ Cargo :" 
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT Tran
 SCATTER MEMVAR
 @  0,16 SAY m.numero
 @  1,16 SAY m.Fecha
 @  1,55 SAY m.Periodo
 @  2,16 SAY m.NumMes
 @  3,16 SAY m.CtaCar
 @  4,16 SAY m.nombre 
 @  5,16 GET m.tipo FUNCTION '*RNH \<Autorizaciones de Giro;\<Transferencia de Fondos'  SIZE 1,10,2 COLOR SCHEME 11 WHEN .F.
 @  6,16 SAY m.nrocar
 READ
 DO Vista_Hijo
 DO total
RETURN


PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
 SELECT itef
 SEEK ALLTRIM(m.nummes)+m.numero
 DO CASE
    CASE m.tipo=1
	   BROWSE;
	   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
	   WINDOW Wind_C2 KEY ALLTRIM(m.nummes)+m.numero;
	   FIELDS;
	   CodCtc    :H='Cta.Aho.' ,;
	   xx=ofigir(1) :36 :H='Of. Giradora',;
	   Importe   :P='999,999,999,999.99',;
	   nroabo   : 4 :R,;
	   nroauto  : 6 :R
	   SELECT  Tran
	CASE m.tipo=2
	   BROWSE;
	   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
	   WINDOW Wind_C2 KEY ALLTRIM(m.nummes)+m.numero;
	   FIELDS;
	   CodCtc    :H='Cta.Aho.' ,;
	   xx=vdcta() :36 :H='Descripci¢n',;
	   Importe   :P='999,999,999,999.99',;
	   nroabo   : 4 :R ,;
	   nroauto  : 6 :R
	   SELECT  Tran
ENDCASE 
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT Tran
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
   numero   :H= 'Oficio' ,;
   CtaCar   :H= 'Cta.Ahorro',;
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
 SELECT Tran
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
 @ 0,41 GET Vmesb PICTURE '99'
 @ 0,44 GET Vbusca PICTURE '9999'
 READ
 RELEASE WINDOW WBusca
 IF LASTKEY()=27
    SELECT Tran
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
 SELECT Tran
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
 SELECT Tran
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
 SELECT Tran
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
 @  0,16 GET m.numero   WHEN .F.
 @  1,16 GET m.Fecha  	PICTURE '@E' 
 @  1,55 GET m.Periodo  PICTURE "!!"
 @  2,16 GET m.NumMes   WHEN .F.
 @  3,16 GET m.CtaCar   WHEN .F. VALID vnom()
 @  4,16 GET m.nombre   PICTURE '@!' 
 @  5,16 GET m.tipo     FUNCTION '*RNH \<Autorizaciones de Giro;\<Transferencia de Fondos'  SIZE 1,10,2 COLOR SCHEME 11 
 @  6,16 SAY m.nrocar
 m.usuario=wuser_id
 READ VALID Val_Read()
 SELE CAJA
 SEEK ALLTRIM(m.ctacar)
 wbanco=caja.banco
 w_tipctc=caja.tipo
 w_ctaH=caja.CuentaH
 SELE Tran
 IF LASTKEY() # 27
    SELECT Tran
    GATHER MEMVAR
    SCATTER MEMVAR
    ss= Tra_Hijo()     && Corrige items
    IF SS .AND. LASTKEY()#27
       SELECT itef
       GATHER MEMVAR
    ENDIF
 UNLOCK ALL
 DO total
 SELE Tran
 IF SEEK( ALLT(m.nummes)+m.numero)
    REPLACE monto WITH Xtot
 ENDIF   
 DO ingap
 IF m.tipo=2
    DO ingap1
 ENDIF    
 SELECT Tran
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 DO Vista
RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT Tran
 DO pantalla
 SCATTER MEMVAR BLANK
 m.Fecha=DATE()
 m.Periodo=RIGHT(DTOC(DATE()),2)
 vMes=STR(MONTH(DATE()))
 ingreso=.T.
 SELECT Parma
 SEEK 'CORREL'+'TRANSF'
 m.numero  = PADL(NumEnt+1,4,'0')
 SEEK 'CORREL'+'MOVBCO'
 m.nrocar  = PADL(NumEnt+1,4,'0')
 m.portador='LETICIA GARCIA BACA'+SPACE(27)
 @ 00,16 GET m.numero      PICTURE "9999" 
 @ 01,16 GET m.Fecha       PICTURE '@E'  VALID vfec()
 @ 01,55 GET m.Periodo     PICTURE '!!' 
 @ 02,16 GET m.NumMes      VALID val_para(m.nummes  ,'FECMES',' ',29,9) AND vexiste()
 @ 03,16 GET m.CtaCar      PICTURE "@!"  VALID Val_fun('Caja','CodCtc','codctc+descri',m.Ctacar,1,03,16)  AND vnom()
 @ 04,16 GET m.nombre      PICTURE '@!' 
 @ 05,16 GET m.tipo        FUNCTION '*RNH \<Autorizaciones de Giro;\<Transferencia de Fondos'  SIZE 1,10,2 COLOR SCHEME 11 
 @ 06,16 SAY m.nrocar
 m.usuario=wuser_id
 READ VALID Val_Read()
 SELE CAJA
 SEEK ALLTRIM(m.ctacar)
 wbanco  = caja.banco
 w_tipctc= caja.tipo
 w_ctaH  = caja.CuentaH
 SELE Tran
  IF LASTKEY()#27 
     vAnula = .F.
     SELECT Tran
     IF F_Appd()
        GATHER MEMVAR
        UNLOCK ALL
        SELECT Parma
        SEEK 'CORREL'+'TRANSF'
        REPLACE NumEnt WITH NumEnt + 1       
        SEEK 'CORREL'+'MOVBCO'
        REPLACE NumEnt WITH NumEnt + 1       
        SELECT Tran
        SCATTER MEMVAR
        SELECT itef
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
	    		DO total 
			    SELE Tran
			    IF SEEK( ALLT(m.nummes)+m.numero)
			      REPLACE monto WITH Xtot
			    ENDIF   
			    DO ingap
		        DO ingap1
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
    UNLOCK ALL
    SELECT Tran
ELSE
    DO STANDBY WITH 'Proceso cancelado'
ENDIF
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
 SELECT itef
 GO TOP
 SEEK ALLTRIM(m.nummes)+m.numero
 IF !FOUND()
    DO Agreg_Item
 ENDIF
 DO CASE
    CASE m.tipo=1
	  BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
	  KEY ALLTRIM(m.nummes)+m.numero;
	  FIELDS;
	   Codctc   : H='Cod.Cta'   :V=val_ctc("Caja","CodCtc","CodCtc+' '+Descri",Codctc,2) :f,;
	   xx=ofigir(2) :H='Of. Giradora' :36 :R,;
	   Importe  : H='      Monto      ':P='999,999,999,999.99',;
	   nroabo   : 4  :R
    CASE m.tipo=2
	  BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
	  KEY ALLTRIM(m.nummes)+m.numero;
	  FIELDS;
	   Codctc   : H='Cod.Cta'   :V=val_Ctc1("Caja","CodCtc","CodCtc+' '+Descri",Codctc,2) :f,;
	   xx=vdcta() :H='Descripci¢n' :36 :R,;
	   Importe  : H='      Monto      ':P='999,999,999,999.99',;
	   nroabo   : 4 :R

ENDCASE  
 vOrd=ORDER()
 SELECT itef
 SET ORDER TO (vOrd)
 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5
 UNLOCK ALL
SELECT Tran
ACTIVATE SCREEN
vTempo = ' Ingresa  Elige  Busca  Anterior  Siguiente  Corrige  Anula  Lista Termina '
DO Logos WITH Rotulo1,vtempo
SHOW MENU mMenu
RETURN  vFun


PROCEDURE Agreg_item
*-------------------
SELE PARMA
SEEK 'CORREL'+'MOVBCO'
m.nroabo  = PADL(NumEnt+1,4,'0')
SELE itef
IF F_Appd()
   REPLACE Nummes   WITH m.Nummes ,;
           numero   WITH m.numero ,;
           periodo  WITH m.periodo,;
           nroabo   WITH m.nroabo,;
           Tipo     WITH m.tipo    
ENDIF
SELE parma
SEEK 'CORREL'+'MOVBCO'
REPLACE NumEnt WITH NumEnt + 1       
SELE itef        
UNLOCK
RETURN


PROCEDURE Elimi_item
*-------------------
SELECT itef
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
   SELECT Tran
   vNummes=Tran.Nummes
   vnumero=ALLTRIM(Tran.numero)
   IF f_lock(1)
      DELETE
      *-Busca y elimina los Items
      SELECT itef
      GO TOP
      SEEK ALLTRIM(vNummes)+vnumero
      IF FOUND()
         SCAN WHILE vNummes = ALLTRIM(itef.Nummes) .AND. vnumero = itef.numero 
              IF f_lock(1)
                 DELETE NEXT 1
              ENDIF
         ENDSCAN
      ENDIF
      SELECT Tran
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
 SELECT Tran
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 STORE SPACE(2) TO vnummes
 STORE SPACE(4) TO vnumero 
 rec = RECNO()
 SCATTER MEMVAR
 vnummes = m.nummes
 vnumero = m.numero
 vcodctc = m.ctacar
 vtiplis = 1
DEFINE WINDOW lis FROM 4,10 TO 19,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 00,01 SAY "     Tipo Listado : "  GET vtiplis FUNCTION '^ Documento'
@ 06,01 SAY "     N§ Documento : "
@ 06,24 SAY "."
@ 06,22 GET vnummes  WHEN vtiplis = 1 PICTURE "!!"
@ 06,25 GET vnumero  WHEN vtiplis = 1 PICTURE "!!!!" VALID ValAut()
@ 13,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
SELE CAJA
IF SEEK (ALLTRIM(vcodctc))
   wbanco=Caja.Banco
   SELE Parma
   SEEK 'BANCOS'+ALLTRIM(wbanco)
   wdbco=Parma.Descri
ENDIF
SELE Tran   
xmes=val(vnummes)
xAno =VAL(m.Periodo)
RELEASE WINDOW lis
IF okcancel = 1
	   SELE Tran
	   SET RELATION TO Allt(NumMes)+numero INTO itef
	   SET FILT TO allt(nummes)+numero=ALLTRIM(vnummes)+vnumero
	   SET SKIP TO itef
	   SEEK ALLTRIM(vnummes)+vnumero
	   IF FOUND()
		  DO REPORTE WITH 2,"FtoTranF"," Cheques de Gerencia",1,.F.,.T.
	   ELSE
    	  DO STANDBY WITH vmens08	  
	   ENDIF
	   SET SKIP TO
	   SET FILT TO
	   SET RELA TO
ENDIF      	   
SELECT Tran
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
SEEK ALLT(itef.Codctc)
_y=ALLT(Caja.CodOfg)
_x=Val_Para(_y,'OFGIIP','D')
SELECT (vAli)
SET ORDER TO (vOrd)
IF a=2
   REPLACE CodOfg WITH _y
ENDIF
RETURN _x



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


PROCEDURE ValAut
*--------------
 SELECT Tran
 vTemp=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK Vnummes+Vnumero
 IF !FOUND()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   Nummes   :H= 'Mes' ,;
   numero  			 ,;
   CtaCar  :H= 'Cta.Ahorro',;
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
 vnumero= numero
 vcodctc= ctacar
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
SET FILT TO codscta='ING.PRP.' AND ofigir='S' AND banco=ALLTRIM(wbanco)
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


FUNCTION Val_ctc1
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
SET FILT TO banco=ALLTRIM(wbanco)
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
 SELE itef
 SEEK ALLT(m.nummes)+m.numero
 SCAN WHILE itef.nummes=ALLT(m.nummes) AND itef.numero=m.numero 
 	  xtot=xtot+itef.importe 
 ENDSCAN	  
 ACTIVATE WINDOW wind_3
 @ 0,1 SAY xtot PICTURE '999,999,999.99'
 RETURN


FUNCTION vdcta
*--------------
yali = ALIAS()
yord = ORDER()
SELE Caja
SEEK ALLT(itef.codctc)
IF FOUND()
   vdes=LEFT(descri,40)
ELSE
   vdes=SPACE(40)   
ENDIF   
SELE (Yali)
SET ORDER TO (Yord)
RETURN vdes

PROCEDURE ingap   && Ingresa asientos patRimoniales (N/C)
*-------------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
DEFINE WINDOW Wind_x FROM 10,00 TO 20,79 DOUBLE ;
 TITLE 'Asientos Patrimoniales : Nota de Cargo '+m.ctacar;
 COLOR SCHEME 10
SELE ASTPAT
xord = ORDER()
SET ORDER TO ASTPAT5
SELECT Movb
SEEK ALLT(m.nummes)+m.nrocar
IF !FOUND()
	APPEND BLANK
		REPLACE PeRiodo 	WITH m.peRiodo ,;
				Nummes  	WITH m.nummes ,;
				NummesC 	WITH m.nummes ,;
				numero  	WITH m.nrocar ,;
				Transa  	WITH 'CAR' ,;
				Fecha   	WITH m.fecha,;
				FechaC  	WITH m.fecha,;
				Tipdoc  	WITH 'N/C' ,;
				codfte  	WITH 'PRP',;
				codctc  	WITH m.Ctacar ,;
				nombre  	WITH 'TRANSFERENCIA DE FONDOS AUTOMATICO '+m.numero+'.'+m.nummes ,;
				forma   	WITH 'BD' ,;
				Monto   	WITH xtot,;
				tipfun  	WITH 'F',;
				codcal  	WITH ALLTRIM(m.periodo)+ALLTRIM(m.nummes)+'PRP01001' ,;
				estado      WITH '00' ,;
				motivo      WITH IIF(m.tipo=1,'05','04'),;				
				codban   	WITH wbanco,;
				usuario     WITH wuser_id
ELSE
	REPLACE PeRiodo 	WITH m.peRiodo ,;
				NummesC 	WITH m.nummes ,;
				Transa  	WITH 'CAR' ,;
				Fecha   	WITH m.fecha,;
				FechaC  	WITH m.fecha,;
				Tipdoc  	WITH 'N/C' ,;
				codfte  	WITH 'PRP',;
				codctc  	WITH m.Ctacar ,;
				nombre  	WITH 'TRANSFERENCIA DE FONDOS AUTOMATICO '+m.numero+'.'+m.nummes ,;
				forma   	WITH 'BD' ,;
				Monto   	WITH xtot,;
				tipfun  	WITH 'F',;
				codcal  	WITH ALLTRIM(m.periodo)+ALLTRIM(m.nummes)+'PRP01001' ,;
				estado      WITH '00' ,;
				motivo      WITH IIF(m.tipo=1,'05','04'),;				
				codban   	WITH wbanco,;
				usuario     WITH wuser_id
ENDIF		
IF m.tipo=2
	SELECT Astpat
	SEEK ALLT(m.Nummes)+m.nrocar+ALLT(m.Ctacar)
	IF !FOUND()
			APPEND BLANK
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar;
					tipcta  WITH 'D',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH '1010101000',;
					Mtodeb  WITH xtot ,;
					tipctc  WITH w_tipctc
			APPEND BLANK
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar ,;
					tipcta  WITH 'H',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH w_ctaH,;
					Mtohab  WITH xtot,;
					tipctc  WITH w_tipctc
	ELSE
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar;
					tipcta  WITH 'D',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH '1010101000',;
					Mtodeb  WITH xtot ,;
					tipctc  WITH w_tipctc
			SKIP
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar ,;
					tipcta  WITH 'H',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH w_ctaH,;
					Mtohab  WITH xtot,;
					tipctc  WITH w_tipctc
	ENDIF
ELSE
	SELECT Astpat
		SEEK ALLT(m.Nummes)+m.nrocar+ALLT(m.Ctacar)
		IF !FOUND()
			APPEND BLANK
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar;
					tipcta  WITH 'D',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH '7150101000',;
					Mtodeb  WITH xtot ,;
					tipctc  WITH w_tipctc
			APPEND BLANK
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar ,;
					tipcta  WITH 'H',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH w_ctaH,;
					Mtohab  WITH xtot,;
					tipctc  WITH w_tipctc
			APPEND BLANK
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar;
					tipcta  WITH 'D',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH '1010101000',;
					Mtodeb  WITH xtot ,;
					tipctc  WITH w_tipctc
			APPEND BLANK
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar ,;
					tipcta  WITH 'H',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH '1010101000',;
					Mtohab  WITH xtot,;
					tipctc  WITH w_tipctc
		ELSE
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar;
					tipcta  WITH 'D',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH '7150101000',;
					Mtodeb  WITH xtot ,;
					tipctc  WITH w_tipctc
			SKIP
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar ,;
					tipcta  WITH 'H',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH w_ctaH,;
					Mtohab  WITH xtot,;
					tipctc  WITH w_tipctc
			SKIP		
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar;
					tipcta  WITH 'D',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH '1010101000',;
					Mtodeb  WITH xtot ,;
					tipctc  WITH w_tipctc
			SKIP
			REPLACE PeRiodo WITH m.peRiodo ,;
					tipdoc  WITH 'N/C' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH m.Nummes ,;
					Numref  WITH m.nrocar ,;
					tipcta  WITH 'H',;
					codctc  WITH m.Ctacar ,;
					codcta  WITH '1010101000',;
					Mtohab  WITH xtot,;
					tipctc  WITH w_tipctc
		ENDIF
	
ENDIF
SELECT Astpat
BROWSE FIELDS ;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999,999,999.99';
	WINDOW wind_x KEY allt(m.nummes)+ALLTRIM(m.nrocar)+ALLTRIM(m.Ctacar)
SET ORDER TO (xord)
SELE Tran
RETURN




PROCEDURE ingap1   && Ingresa asientos patRimoniales (N/A)
*-------------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SELE ASTPAT
xord = ORDER()
SET ORDER TO ASTPAT6
SELECT itef
SEEK ALLTRiM(m.NumMes)+m.numero 
vnummes  = itef.nummes
vnumero  = itef.numero
vnroabo  = itef.NroAbo
vcodctc  = itef.Codctc
vnroauto = itef.nroauto
SCAN WHILE ALLTRiM(vNumMes)+vnumero=ALLTRiM(itef.NumMes)+itef.Numero 
    IF (m.tipo=1 AND ALLT(ITEC.codofg)='01') OR m.tipo=2
		vnummes  = itef.nummes
		vnumero  = itef.numero
		vnroabo  = itef.NroAbo
		vcodctc  = itef.Codctc
		vnroauto = itef.nroauto
		SELE Caja
		SEEK ALLTRIM(vcodctc)
		wbanco  = caja.banco
		w_tipctc= caja.tipo
		w_ctaH  = caja.CuentaH
		SELECT Movb
		SEEK ALLT(Vnummes)+Vnroabo
		IF !FOUND()
			APPEND BLANK
			REPLACE PeRiodo 	WITH itef.peRiodo ,;
				Nummes  	WITH vnummes ,;
				NummesC 	WITH vnummes ,;
				numero  	WITH vnroabo,;
				Transa  	WITH 'ABO' ,;
				Fecha   	WITH m.fecha,;
				FechaC  	WITH m.fecha,;
				Tipdoc  	WITH 'N/A' ,;
				Numref  	WITH 'TRA.FON.'+vnroauto ,;
				codfte  	WITH 'PRP',;
				codctc  	WITH VCodCtc ,;
				nombre  	WITH 'TRANSFERENCIA DE FONDOS AUTOMATICA '+m.numero+'.'+m.nummes ,;
				forma   	WITH 'BD' ,;
				Monto   	WITH itef.importe ,;
				tipfun  	WITH 'F',;
				codcal  	WITH ALLTRIM(m.periodo)+ALLTRIM(m.nummes)+'PRP01001' ,;
				estado      WITH '00' ,;
				motivo      WITH '04',;				
				codban   	WITH wbanco,;
				usuario     WITH wuser_id
		ELSE
			REPLACE PeRiodo 	WITH itef.peRiodo ,;
				Transa  	WITH 'ABO' ,;
				Fecha   	WITH m.fecha,;
				FechaC  	WITH m.fecha,;
				Tipdoc  	WITH 'N/A' ,;
				Numref  	WITH 'TRA.FON.'+vnroauto ,;
				codfte  	WITH 'PRP',;
				codctc  	WITH VCodCtc ,;
				nombre  	WITH 'TRANSFERENCIA DE FONDOS AUTOMATICA '+m.numero+'.'+m.nummes ,;
				forma   	WITH 'BD' ,;
				Monto   	WITH itef.importe ,;
				tipfun  	WITH 'F',;
				codcal  	WITH ALLTRIM(m.periodo)+ALLTRIM(m.nummes)+'PRP01001' ,;
				estado      WITH '00' ,;
				motivo      WITH '04',;				
				codban   	WITH wbanco,;
				usuario     WITH wuser_id
		ENDIF		
		SELECT Astpat
		SEEK ALLT(vNummes)+vnroabo+ALLT(vcodctc)
		IF !FOUND()
				APPEND BLANK
				REPLACE PeRiodo WITH itef.peRiodo ,;
					tipdoc  WITH 'N/A' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH vnummes ,;
					Numref  WITH vnroabo;
					tipcta  WITH 'D',;
					codctc  WITH vcodctc,;
					codcta  WITH w_ctaH,;
					Mtodeb  WITH itef.Importe ,;
					tipctc  WITH w_tipctc
				APPEND BLANK
				REPLACE PeRiodo WITH itef.peRiodo ,;
					tipdoc  WITH 'N/A' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH vnummes ,;
					Numref  WITH vnroabo;
					tipcta  WITH 'H',;
					codctc  WITH vcodctc,;
					codcta  WITH '1010101000',;
					Mtohab  WITH itef.Importe ,;
					tipctc  WITH w_tipctc
		ELSE
				REPLACE PeRiodo WITH itef.peRiodo ,;
					tipdoc  WITH 'N/A' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH vnummes ,;
					Numref  WITH vnroabo;
					tipcta  WITH 'D',;
					codctc  WITH vcodctc,;
					codcta  WITH w_ctaH,;
					Mtodeb  WITH itef.Importe ,;
					tipctc  WITH w_tipctc
				SKIP
				REPLACE PeRiodo WITH itef.peRiodo ,;
					tipdoc  WITH 'N/A' ,; 
					fecha   WITH m.fecha ,;
					Nummes  WITH vnummes ,;
					Numref  WITH vnroabo;
					tipcta  WITH 'H',;
					codctc  WITH vcodctc,;
					codcta  WITH '1010101000',;
					Mtohab  WITH itef.Importe ,;
					tipctc  WITH w_tipctc
		ENDIF
		SELE itef
	ENDIF	
ENDSCAN	
SELE ASTPAT
SET ORDER TO (xord)
SELE Tran
RETURN

FUNCTION Vexiste
*---------------
SELE Tran
SEEK ALLT(m.nummes)+m.numero
IF FOUND()
    DO STANDBY WITH "El N§ de oficio ya existe"
	RETURN .F.
ENDIF
RETURN .T.	

FUNCTION vfec
*------------
m.nummes     = MONTH(m.fecha)
IF m.nummes<10
	m.nummes  ='0'+ALLTRIM(STR(MONTH(m.fecha)))
ELSE
    m.nummes=STR(m.nummes,2)   
ENDIF   
RETURN

FUNCTION vnom
*------------
Vali=ALIAS()
vord=ORDER()
SELE Caja
SEEK m.ctacar
IF FOUND()
   wbanco=caja.banco
   SELE Parma
   SEEK 'BANCOS'+wbanco
   IF FOUND()
      m.nombre=parma.descriaux
   ENDIF   
ENDIF
SELE (vali)
SET ORDER TO (vord)
RETURN	
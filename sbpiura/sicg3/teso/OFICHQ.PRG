* 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
* ? Ofichq.PRG  09/10/96                              L: 0741    ?	
* ? Solicitud de Chequeras                                       ?
* ? AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ?
* 突袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴槁

*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09
Vmens01 = 'Revisi줻 de Solicitud de Chequeras'
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '

*- Abre archivos

USE OfiCheq  IN  1 ORDER TAG OfiCheq1                 ALIAS OfiC
USE IteOfich IN  2 ORDER TAG IteOfiCh1               ALIAS IteO
USE Parmae   IN  3 ORDER TAG Parmae1                 ALIAS Parma
USE Cajas    IN  4 ORDER TAG Cajas2                  ALIAS Caja
*- Variables de trabajo (registro a trabajar)
PUBLIC vFun,m.fecha,vMes,mmonto,Sw,vfecbco,wdesc,vconf,W_ctaH,w_tipctc,wbanco,xtot,Vuser_id,wuser_id,wbanco
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
vfecbco=DATE()

STORE   .T.	 TO 	Agrega,vFun
STORE DATE() TO 	m.fecha
STORE    0   TO		mmonto,Sw

*- Inicia proceso
SELECT OfiC
GO BOTTOM
DO Inicia                    && Define ventanas, men즧, t죜ulos
HIDE POPUP ALL
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men? mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
   ACTIVATE SCREEN
   ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

SHOW POPUP Menu

RETURN


PROCEDURE Inicia             && Crea ventanas, men즧 y t죜ulos
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
 TITLE 'Detalle: Solicitud de Chequeras' ;

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


PROCEDURE Pantalla           && Pinta m쟳cara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @ 00,02 SAY "  N? Oficio :"  
 @ 01,02 SAY "      Fecha :" 
 @ 01,40 SAY "    Periodo :" 
 @ 02,02 SAY "        Mes :" 
 @ 03,02 SAY "      Banco :" 
 @ 04,02 SAY "     Nombre :" 
 @ 05,02 SAY "Responsable :" 
 @ 06,02 SAY "       L.E. :" 
 @ 07,02 SAY "Total-Letras:" 
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT OfiC
 SCATTER MEMVAR
 @  0,16 SAY m.nroofi
 @  1,16 SAY m.Fecha
 @  1,55 SAY m.Periodo
 @  2,16 SAY m.nummes
 @  3,16 SAY m.banco
 @  4,16 SAY m.nombre 
 @  5,16 SAY m.respons
 @  6,16 SAY m.leresp PICTURE "99999999"
 @  7,16 SAY m.totlet
 DO Vista_Hijo
 DO total
RETURN


PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
 SELECT IteO
 SEEK ALLTRIM(m.nummes)+m.nroofi
 BROWSE;
 NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
 WINDOW Wind_C2 KEY ALLTRIM(m.nummes)+m.nroofi;
 FIELDS;
 CodCtc     :H='Cta.Cte.' ,;
 xx=vdcta() :36 :H='Descripci줻',;
 Nrochq     :H='N? Chequeras' :P='999',;
 unichq     :H='Chq. C/U' :P='999'
 SELECT  OfiC
RETURN


PROCEDURE Revis              && Revisi줻 de BD en browse
*--------------
 SELECT OfiC
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 vTemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '같같같같같?Presione 췋10? para seleccionar  o  췊sc? para cancelar같같같같같같'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   periodo,;
   Nummes   :H= 'Mes' ,;
   nroofi   :H= 'Oficio' ,;
   x2=Val_Para(banco,"BANCOS",'V',22,40) :H='Banco' :15,;
   Fecha    :H= 'Fecha',;
   totchq,;
   nombre 
   vTempo = '같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같'
   DO Logos WITH Rotulo1,vTempo
   IF LASTKEY()=27
      GOTO Vtemp
   ENDIF
   SHOW MENU mMenu
   ON KEY LABEL F10
   DEACTIVATE WINDOW Wind_C4
   DO Vista
RETURN


PROCEDURE Busca              && Realiza b즧queda directa
*--------------
 SELECT OfiC
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
 @ 0, 2 SAY 'N즡ero de documento que desea buscar:'
 @ 0,41 GET Vmesb PICTURE '99'
 @ 0,44 GET Vbusca PICTURE '9999'
 READ
 RELEASE WINDOW WBusca
 IF LASTKEY()=27
    SELECT OfiC
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
 SELECT OfiC
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
 SELECT OfiC
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
 SELECT OfiC
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
 @  0,16 GET m.nroofi   WHEN .F.
 @  1,16 GET m.Fecha  	PICTURE '@E' 
 @  1,55 GET m.Periodo  PICTURE "!!"
 @  2,16 GET m.NumMes   WHEN .F.
 @  3,16 GET m.banco    WHEN .F. VALID vnom()
 @  4,16 GET m.nombre   PICTURE '@!' 
 @  5,16 GET m.respons  PICTURE '@!' 
 @  6,16 GET m.leresp   PICTURE "99999999"
 @  7,16 GET m.totlet   PICTURE '@!' 
 m.usuario=wuser_id
 READ VALID Val_Read()
 SELE OfiC
 IF LASTKEY() # 27
    SELECT OfiC
    GATHER MEMVAR
    SCATTER MEMVAR
    ss= Tra_Hijo()     && Corrige items
    IF SS .AND. LASTKEY()#27
       SELECT IteO
       GATHER MEMVAR
    ENDIF
 UNLOCK ALL
 DO total
 SELE OfiC
 IF SEEK( ALLT(m.nummes)+m.nroofi)
    REPLACE totchq WITH Xtot
 ENDIF   
 SELECT OfiC
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 DO Vista
RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT OfiC
 DO pantalla
 SCATTER MEMVAR BLANK
 m.Fecha=DATE()
 m.Periodo=RIGHT(DTOC(DATE()),2)
 vMes=STR(MONTH(DATE()))
 ingreso=.T.
 SELECT Parma
 SEEK 'CORREL'+'OFICHQ'
 m.nroofi  = PADL(NumEnt+1,4,'0')
 m.respons ='IRMA MACEDA HERRERA'+SPACE(27)
 m.leresp  ='02603237'
 @ 00,16 GET m.nroofi      PICTURE "9999" 
 @ 01,16 GET m.Fecha       PICTURE '@E'  VALID vfec()
 @ 01,55 GET m.Periodo     PICTURE '!!' 
 @ 02,16 GET m.NumMes      VALID val_para(m.nummes  ,'FECMES',' ',29,9) AND vexiste()
 @ 03,16 GET m.banco       PICTURE "!!!"    VALID Val_Para(m.banco,"BANCOS"," ",22,25) AND vnom()
 @ 04,16 GET m.nombre      PICTURE '@!' 
 @ 05,16 GET m.respons 	   PICTURE '@!' 	
 @ 06,16 GET m.leresp      PICTURE "99999999"
 @ 07,16 GET m.totlet   PICTURE '@!' 
 m.usuario = wuser_id
 READ VALID Val_Read()
 SELE OfiC
  IF LASTKEY()#27 
     vAnula = .F.
     SELECT OfiC
     IF F_Appd()
        GATHER MEMVAR
        UNLOCK ALL
        SELECT Parma
        SEEK 'CORREL'+'OFICHQ'
        REPLACE NumEnt WITH NumEnt + 1       
        SELECT OfiC
        SCATTER MEMVAR
        SELECT IteO
        DO WHILE .T.
          DO Tra_hijo
          IF LASTKEY() = 27
             IF YESNO( '? Cancela el Ingreso ?' )
                vAnula = .T.
                EXIT
             ENDIF
          ELSE
             IF YESNO( '? Est쟮 correctos los datos ?')             
			    DO total 
			    SELE OfiC
			    IF SEEK( ALLT(m.nummes)+m.nroofi)
			       REPLACE Totchq WITH Xtot
			    ENDIF   
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
    UNLOCK ALL
    SELECT OfiC
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
 VTempo = '같같같같F5->Agregar같같같같같같같F8->Eliminar같같같같같같같F10->Terminar같같같같'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO agreg_item
 ON KEY LABEL F8  DO elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
 STORE 0 TO m.Importe,AntCan,vSuma
 SELECT IteO
 GO TOP
 SEEK ALLTRIM(m.nummes)+m.nroofi
 IF !FOUND()
    DO Agreg_Item
 ENDIF
 BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
 KEY ALLTRIM(m.nummes)+m.nroofi;
 FIELDS;
 Codctc   : H='Cod.Cta'   :V=val_ctc("Caja","CodCtc","CodCtc+' '+Descri",Codctc,2) :f,;
 xx=vdcta() :36 :H='Descripci줻',;
 Nrochq     :H='N? Chequeras' :P='999'  :v=nrochq>0 ,;
 unichq     :H='Chq. C/U' :P='999':v=unichq>0
 vOrd=ORDER()
 SELECT IteO
 SET ORDER TO (vOrd)
 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5
 UNLOCK ALL
SELECT OfiC
ACTIVATE SCREEN
vTempo = ' Ingresa  Elige  Busca  Anterior  Siguiente  Corrige  Anula  Lista Termina '
DO Logos WITH Rotulo1,vtempo
SHOW MENU mMenu
RETURN  vFun


PROCEDURE Agreg_item
*-------------------
SELE IteO
IF F_Appd()
   REPLACE Nummes   WITH m.Nummes ,;
           nroofi   WITH m.nroofi ,;
           periodo  WITH m.periodo,;
           estado   WITH '1'
ENDIF
UNLOCK
RETURN


PROCEDURE Elimi_item
*-------------------
SELECT IteO
IF RLOCK()
   DELETE NEXT 1
ELSE
   DO StandBy WITH 'No puede eliminar este Item.'
ENDIF
RETURN


PROCEDURE Anula
*--------------
* Anula el documento actual.
IF yesno("쭭NULA Documento?")
   vErdad=.T.
   SELECT OfiC
   vNummes=OfiC.Nummes
   vnumero=ALLTRIM(OfiC.nroofi)
   IF f_lock(1)
      DELETE
      *-Busca y elimina los Items
      SELECT IteO
      GO TOP
      SEEK ALLTRIM(vNummes)+vnumero
      IF FOUND()
         SCAN WHILE vNummes = ALLTRIM(IteO.Nummes) .AND. vnumero = IteO.nroofi 
              IF f_lock(1)
                 DELETE NEXT 1
              ENDIF
         ENDSCAN
      ENDIF
      SELECT OfiC
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
 SELECT OfiC
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 STORE SPACE(2) TO vnummes
 STORE SPACE(4) TO vnumero 
 rec = RECNO()
 SCATTER MEMVAR
 vnummes = m.nummes
 vnumero = m.nroofi
 vtiplis = 1
DEFINE WINDOW lis FROM 4,10 TO 19,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 00,01 SAY "     Tipo Listado : "  GET vtiplis FUNCTION '^ Documento'
@ 06,01 SAY "     N? Documento : "
@ 06,24 SAY "."
@ 06,22 GET vnummes  WHEN vtiplis = 1 PICTURE "!!"
@ 06,25 GET vnumero  WHEN vtiplis = 1 PICTURE "!!!!" VALID valofchq()
@ 13,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
xmes=val(vnummes)
xAno =VAL(m.Periodo)
RELEASE WINDOW lis
IF okcancel = 1
   DO CASE
   	CASE vtiplis = 1
	   SELE OfiC
	   SET RELATION TO Allt(NumMes)+nroofi INTO IteO
	   SET FILT TO allt(nummes)+nroofi=ALLTRIM(vnummes)+vnumero
	   SET SKIP TO IteO
	   SEEK ALLTRIM(vnummes)+vnumero
	   IF FOUND()
		  DO REPORTE WITH 2,"solchq"," Cheques de Gerencia",1,.F.,.T.
	   ELSE
    	  DO STANDBY WITH vmens08	  
	   ENDIF
	   SET SKIP TO
	   SET FILT TO
	   SET RELA TO
    ENDCASE		
ENDIF      	   
SELECT OfiC
IF rec > 0
   GO rec
ENDIF
DO vista
RETURN

FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)


PROCEDURE Valfecha
*-----------------
an=right(str(year(date()),4),2)
vMe = val(m.Nummes)+1
me = padl(alltrim(str(vMe,2)),2,'0')
vFec = '01/&ME/&AN'
m.Fecha = ctod(vFec) - 1
RETURN .T.


PROCEDURE Valofchq
*-----------------
 SELECT OfiC
 vTemp=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK Vnummes+Vnumero
 IF !FOUND()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '같같같같같?Presione 췋10? para seleccionar  o  췊sc? para cancelar같같같같같같'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   Nummes   :H= 'Mes' ,;
   nroofi  			 ,;
   x2=Val_Para(banco,"BANCOS",'V',22,40) :H='Banco' :12,;
   Fecha    :H= 'Fecha',;
   Nombre  
   vTempo = '같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같'
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
 vnumero= nroofi
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
* Long.A ver: v__Long    Usado para mostrar el tama쨚 de v__Ver

mEdita = (parameters()>=5) .and.  (v__tipo # 3)
v__Ver = IIF(PARAMETERS()>=8,v__ver,v__bus)
mRec   = IIF(EOF(),-1,RECNO())
mAlias = ALIAS()
_campo = TRIM(mAlias)+'.'+VARREAD()
SELECT &v__al
SET FILT TO banco=ALLT(m.banco)
_OldWnd = WOUTPUT()
IF EMPTY(&_campo)
   GO TOP
   IF EOF()
	  DO standby WITH "께께께께 NO HAY VALORES PARA ELEGIR 께께께께"
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
					 DO standby WITH " El archivo est? vac죓 "
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
 SELE IteO
 SEEK ALLT(m.nummes)+m.nroofi
 SCAN WHILE IteO.nummes=ALLT(m.nummes) AND IteO.nroofi=m.nroofi 
 	  xtot=xtot+IteO.nrochq 
 ENDSCAN	  
 ACTIVATE WINDOW wind_3
 @ 0,1 SAY xtot PICTURE '999,999,999.99'
 RETURN


FUNCTION vdcta
*--------------
yali = ALIAS()
yord = ORDER()
SELE Caja
SEEK ALLT(IteO.codctc)
IF FOUND()
   vdes=LEFT(descri,40)
ELSE
   vdes=SPACE(40)   
ENDIF   
SELE (Yali)
SET ORDER TO (Yord)
RETURN vdes


FUNCTION Vexiste
*---------------
SELE OfiC
SEEK ALLT(m.nummes)+m.nroofi
IF FOUND()
    DO STANDBY WITH "El N? de oficio ya existe"
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
SELE Parma
SEEK 'BANCOS'+m.banco
IF FOUND()
   m.nombre=parma.descriaux
ENDIF   
SELE (vali)
SET ORDER TO (vord)
RETURN	
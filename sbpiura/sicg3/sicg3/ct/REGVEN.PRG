* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RegVen.Prg  23/04/98                                L:  848  ³	
* ³ Registro de Ventas						                     ³
* ³ AUTOR   : ANAL. Segundo Edgar Correa Sandoval 				 ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾
*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '
Vmens01 = 'Registro de Ventas'

*- Abre archivos

USE Regven  IN  1 ORDER TAG RegVen1     ALIAS Venta
USE Iteven  IN  2 ORDER TAG IteVen1	    ALIAS Iteven
USE ParMae  IN  3 ORDER TAG ParMae1     ALIAS Parma
*- Variables de trabajo (registro a trabajar)
PUBLIC vFun, m.fecha, vMes, m.Item, XDBF,vDVan,vHVan

STORE .T. TO Agrega,vFun
STORE SPACE(12) TO m.TipPre
STORE 0 TO m.Item
m.fecha=DATE()
*- Inicia proceso
SELECT Venta
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

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 11,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_C2 FROM 11,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C3 FROM 11,00 TO 23,38 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C4 FROM 00,00 TO 23,79 DOUBLE;
 TITLE Vmens02 COLOR SCHEME 10

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
 ON SELECTION PAD lista  OF mMenu DO Lista
 ON SELECTION PAD termi  OF mMenu DO termi

 ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @ 01,02 SAY "     Nro Doc :"  
 @ 01,40 SAY "         Mes :"  
 @ 03,02 SAY "    Gravable :" 
 @ 05,02 SAY "      I.G.V. :"
 @ 07,02 SAY "   Exonerado :"
 @ 09,02 SAY "       Total :"
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT Venta
 SCATTER MEMVAR
 @  1,16 SAY m.NumVen
 @  1,54 SAY m.NumMes 
 @  3,22 SAY m.Total - m.totigv  PICTURE '999,999,999,999.99' COLOR SCHEME 02
 @  5,22 SAY m.totigv PICTURE '999,999,999,999.99' COLOR SCHEME 02
 @  7,22 SAY m.totexo PICTURE '999,999,999,999.99' COLOR SCHEME 05
 @  9,22 SAY m.total+m.totexo  PICTURE '999,999,999,999.99' COLOR SCHEME 05
 DO Vista_Hijo
RETURN


PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
 hide popup all
 ACTIVATE SCREEN
 SELECT ITEVen
 SEEK ALLTRIM(m.NumVen)+ALLTRIM(m.NumMes)
 BROWSE;
   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
   WINDOW Wind_C2 KEY ALLTRIM(m.NumVen)+ALLTRIM(m.NumMes);
   FIELDS;
   Fecdoc   :H='Fecha',;
   TIPDOC   :H='Doc.',;
   NumDoc   :H='N§ Doc',;
   RUCCLI   :H='R.U.C.',;
   Descri   :H='Descripci¢n':20,;
   Import   :H='Monto'   :P='99,999.99',;
   totigv   :H='I.G.V.'  :P='99,999.99',;
   Numri	:H='R/I',;
   Mesri	:H='Mes'
 SET ORDER TO iteVen1
 SELECT  Venta
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT Venta
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
   NumVen   :H= 'Nro.Doc.',;
   Nummes   :H= 'Nummes',;
   total	:H= 'Total',;
   TotIGV	:H= 'I.G.V.'
   
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
 SELECT Venta
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF

 IF LASTKEY()=27
    RETURN
 ENDIF

 Vtemp = RECNO()

 DEFINE WINDOW WBusca FROM 10,12 TO 14,68 DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW WBusca
 STORE SPACE(07) TO Vbusca
 @ 0, 2 SAY 'N£mero de documento que desea buscar:' GET Vbusca PICTURE '!!!!!!!'
 READ

 RELEASE WINDOW WBusca

 IF LASTKEY()=27
    SELECT Venta
    RETURN
 ENDIF
 IF !SEEK(Vbusca)
    DO standby WITH "Documento no encontrado."
    GOTO Vtemp
 ELSE
    DO Vista
 ENDIF
RETURN


PROCEDURE Anter              && Documento anterior
*--------------
 SELECT Venta
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
 SELECT Venta
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
 SELECT Venta
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
 
 @ 01,16 SAY m.numVen
 @ 01,54 SAY Val_Para(m.NumMes,'FECMES','D',54,9)
 @ 03,22 SAY m.Total - m.totigv PICTURE '999,999,999,999.99' COLOR SCHEME 02
 @ 05,22 SAY m.totigv  			PICTURE '999,999,999,999.99' COLOR SCHEME 02
 @ 07,22 SAY m.totexo  			PICTURE '999,999,999,999.99' COLOR SCHEME 02
 @ 09,22 SAY m.total+m.totexo   PICTURE '999,999,999,999.99' COLOR SCHEME 02
 IF LASTKEY() # 27
    SELECT Venta
    GATHER MEMVAR
    SS= Tra_Hijo()     && Corrige items
    IF SS .AND. LASTKEY()#27
       SELECT ITEVen
       =Valida()
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF

 UNLOCK ALL
 SELECT Venta
 DO Vista
RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT Venta
 DO pantalla
 SCATTER MEMVAR BLANK
 m.Fecha=DATE()
 m.Periodo=RIGHT(DTOC(DATE()),2)
 vMes=STR(MONTH(DATE()))
 SELECT Parma
 SEEK 'CORREL'+'REGVEN'
 m.numVen = PADL(NumEnt+1,4,'0')
 @  7,55 SAY SPACE(20)
 @  8,55 SAY SPACE(20)
 @ 01,16 SAY m.numVen    PICTURE "!!!!"
 @ 01,54 GET m.NumMes    PICTURE '!!'  VALID Val_Para(m.NumMes  ,'FECMES',' ',54,9) .AND. ValFecha()  
 @ 03,22 SAY m.Total - m.totigv PICTURE '999,999,999,999.99' COLOR SCHEME 02
 @ 05,22 SAY m.totigv  			PICTURE '999,999,999,999.99' COLOR SCHEME 02
 @ 07,22 SAY m.totexo  			PICTURE '999,999,999,999.99' COLOR SCHEME 02
 @ 09,22 SAY m.total+m.totexo   PICTURE '999,999,999,999.99' COLOR SCHEME 02
 READ VALID Val_Read()
 IF LASTKEY()#27
    vAnula = .F.
    SELECT Venta
    IF F_Appd()
       GATHER MEMVAR
       UNLOCK ALL
       SELECT Parma
   	   SEEK 'CORREL'+'REGVEN'
       REPLACE NumEnt WITH NumEnt + 1
       SELECT Venta
       SCATTER MEMVAR
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
		        SELE Venta
                EXIT
             ENDIF
          ENDIF
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
 SELECT Venta
 DO Vista
 RETURN



FUNCTION Tra_Hijo
*----------------
 vFun=.t.
 HIDE MENU mMenu
 ACTIVATE SCREEN
 VTempo = '°° ®F4¯ I.G.V. °°F5->Agregar°°F6->Repetir°°F8->Eliminar°°F10->Terminar°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F4  DO Agreg_igv
 ON KEY LABEL F5  DO agreg_item
 ON KEY LABEL F6  DO Repit_item
 ON KEY LABEL F8  DO elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
 STORE 0 TO m.import
 SELECT ITEVen
 SEEK ALLTRIM(m.NumVen)+ALLTRIM(m.NumMes)
 IF !FOUND()
    DO Agreg_Item
 ENDIF
 BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
  KEY ALLTRIM(m.NumVen)+ALLTRIM(m.NumMes);
  FIELDS;
   Fecdoc   :H='Fecha',;
   NumDoc   :H='N§Doc'  :F,;
   TipDoc   :H='DOC' :V=val_Para(TipDoc,"DOCCAJ","Tipdoc"):F:P='!!!',;
   RUCCLI   :H='R.U.C.' :V=IIF(TIPDOC='01',val_clie(ruccli),.T.):F,;
   Descri   :H='Descripcion':20:P='@!':F,;
   TOTigv	:H= 'I.G.V.':P='99,999.99':w=.f.,;
   Import   :H='Importe':P='99,999.99',;
   Numri	:H='R/I',;
   Mesri	:H='Mes'
   
 vOrd=ORDER()
*  ss=val_Para(TipDoc,"DOCCAJ") :H="Detalle" :20,;   

 SELECT ITEVen
 SET ORDER TO vOrd

 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5

 UNLOCK ALL

SELECT Venta
ACTIVATE SCREEN
vTempo = ' Ingresa  Elige  Busca  Anterior  Siguiente  Corrige  Anula  Lista Termina '
DO Logos WITH Rotulo1,vtempo
SHOW MENU mMenu
RETURN  vFun


PROCEDURE Agreg_item
*-------------------
SELECT IteVen
IF F_Appd()
    REPLACE NumVen  WITH Venta.numVen,;
            NumMes  WITH Venta.NumMes
ENDIF   
UNLOCK
RETURN

PROCEDURE repit_item
*-------------------
SELECT ItevEN
vfec=iteVen.fecdoc
vtip=iteVen.tipdoc
vdoc=iteVen.numdoc
vdes=iteVen.descri
vimp=iteVen.import
vigv=iteVen.totigv
IF F_Appd()
    REPLACE NumVen   WITH m.numVen,;
            NumMes  WITH m.NumMes,;
            FecDoc  WITH vfec,;
            tipDoc  WITH vtip,;
            numDoc  WITH vdoc,;
            descri  WITH vdes,;
            import  WITH vimp,;
            totigv  WITH vigv
ENDIF   
UNLOCK
RETURN


PROCEDURE Elimi_item
*-------------------
SELECT ItevEN
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
   vAli=ALIAS()
   vOrd=ORDER()
   SELECT VENTA
   vnumfp=ALLTRIM(VENTA.numVEN)
   SELECT ITEVEN
   GO TOP
   SEEK ALLTRIM(vnumfp)

   SELECT (vAli)
   SET ORDER TO vOrd
   SELECT vENTA
   IF f_lock(1)
      DELETE
      *-Busca y elimina los Items
      SELECT ITEVEN
      GO TOP
      SEEK ALLTRIM(vnumfp)
      IF FOUND()
         SCAN WHILE vnumfp = ALLTRIM(ITEVEN.numVEN)
              IF f_lock(1)
                 DELETE NEXT 1
              ENDIF
         ENDSCAN
      ENDIF
      SELECT Fonp
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
 RELEASE    WINDOW Wind_C3
 RELEASE    WINDOW Wind_C4
 RELEASE    MENU   mMenu
 CLOSE DATA
RETURN


*-Imprime documento

PROCEDURE lista
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF

 IF LASTKEY()=27
    RETURN
 ENDIF
  
 SELECT Venta
 SCATTER Memvar
 vRec=RECNO()
 
 vind = SYS(3) + '.idx'
 xMes = MONTH(DATE())
 xAno = VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))
 SELECT ITeVen
 SET RELATION TO NumVen+NumMes INTO Venta

 DEFINE WINDOW lis FROM 3,5 TO 19,75 FLOAT DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW lis
 STORE 1  TO vtocta,vestado,vlista
 STORE SPACE(14) TO vcta
 STORE SPACE(2)  TO vano
 STORE SPACE(4)  TO vcli
 vcli=ALLT(m.numVen)
 vano=ALLT(m.nummes)
 STORE DATE() TO vfecini, vfecfin
 @ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento '
 @ 04,01 SAY "     N§ Documento : "
 @ 04,22 GET vano    WHEN vlista=1  PICTURE '!!'
 @ 04,25 GET vcli    WHEN vlista=1  PICTURE '!!!!' VALID val_fp()
 *@ 07,01 SAY " Fecha de Emisi¢n : "
 *@ 07,22 GET vfecini  PICTURE '@D' COLOR SCHEME 7   WHEN vlista=2
 *@ 07,32 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7 WHEN vlista=2

 @ 12,18 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
 READ CYCLE

 RELEASE WINDOW lis
 IF okcancel = 1
	ACTIVATE WINDOW standby
	xcolor = '*' + SET('COLOR')
	@ 01,02 SAY 'Espere un momento...Reporte en proceso' COLOR &xcolor
	DO CASE
	   CASE VLISTA=1
	      SET FILT TO numVEN=ALLT(vcli) and nummes=ALLT(vano)
	      VDVAN = 0
	      VHVAN = 0
	      VEXO  = 0
	      VTOT  = 0
		  IF FOUND()
	       	  DEACTIVATE WINDOW standby
              DO REPORTE WITH 2,"RepVen"," Fondo de Pagos en Efectivo "
        	  SELECT ITEVen
	      ELSE
              DO Standby WITH vmens08
	       	  DEACTIVATE WINDOW standby
          ENDIF
    ENDCASE
 ENDIF
 SELECT ITEVen
 SET FILT TO
 SELECT Venta
 IF vRec > 0
    GO vRec
 ENDIF
 DO vista
RETURN


FUNCTION val_fp             && Revisi¢n de BD en browse
*--------------
AS=ALIAS()
SELE Venta
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
SEEK vcli+vano
IF !FOUND()
	vtemp = RECNO()
	HIDE MENU mmenu
	ACTIVATE SCREEN
	vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	ON KEY LABEL f10 KEYBOARD CHR(23)
	BROWSE WINDOW wind_C0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
  		x1=numVen+'.'+nummes  :H='N§ Venta' ,;
	    total	:H= 'Total',;
   		TotIGV	:H= 'I.G.V.'

	vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	SHOW MENU mmenu
	IF LASTKEY()=27
		GOTO BOTT
	ENDIF
ENDIF
vAno = NumMes
vCli = NumVen

ON KEY LABEL f10
SELE (AS)
RETURN

FUNCTION V_Num  &&Valida que el documento no exista
*-------------
PRIVATE __fun
vOrd=ORDER()

  _rec = IIF(EOF(),-1,RECNO())

  __fun = !SEEK(m.numfp+m.numfp)

 _msg2 = "Documento YA EXISTE"

 IF _rec>0
    GO _rec
 ELSE
    GO BOTTOM
 ENDIF
RETURN __fun

FUNCTION Valida && Suma el ingreso de todos los Items ingresados.
*--------------
PRIVATE Vacum
vAcum=0
vAcumIGV=0
vAcuexo=0
SELE IteVen
SEEK ALLT(m.numVen)+ALLTRiM(m.nummes)
SCAN WHILE Iteven.numven=ALLT(M.numven) AND Iteven.nummes=ALLTRIM(M.NUMMES)
	 IF Iteven.TOTIGV<>0
     	vAcum = vAcum + Iteven.import
     	vAcumIGV = vAcumIGV + Iteven.TOTIGV
     ELSE
     	vAcuexo = vAcuexo + Iteven.import
     ENDIF
ENDSCAN
SELE Venta
REPLACE Venta.total  WITH vAcum
REPLACE Venta.TOTIGV WITH vAcumIGV
REPLACE Venta.TOTexo WITH vAcuexo
@  3,22 SAY Venta.total - Venta.totigv PICTURE '999,999,999,999.99' COLOR SCHEME 02
@  5,22 SAY Venta.TOTIGV PICTURE '999,999,999,999.99' COLOR SCHEME 02
@  7,22 SAY Venta.TOTEXO PICTURE '999,999,999,999.99' COLOR SCHEME 05
@  9,22 SAY Venta.TOTal+Venta.Totexo PICTURE '999,999,999,999.99' COLOR SCHEME 05
RETURN

FUNCTION ValFecha
*----------------
an=right(str(year(date()),4),2)
vMe = val(m.Nummes)+1
me = padl(alltrim(str(vMe,2)),2,'0')
vFec = '01/&ME/&AN'
m.Fecha = ctod(vFec) - 1
return .t.


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)

PROCEDURE Agreg_iGV
*-----------------
private as
AS=ALIAS()
sele iteVEN
vValtot=import
SELE PARMA
SEEK 'IMPTOS01'
vIgv=NUMDEC
SELE (AS)
IF TOTIGV>0
	REPLACE totigv with 0
ELSE
	REPLACE totigv WITH ROUND(((vValtot*vigv)/(1+vigv)),2)
ENDIF
RETURN	

FUNCTION Fin
*-----------
IF !EOF()
   SKIP
   IF EOF()
      SKIP - 1
      RETURN .T.
   ENDIF
   SKIP - 1
   RETURN .F.
ENDIF
RETURN .T.

FUNCTION VSUMA
*--------------
vDVan =  vDVan + IIF(iteven.totigv>0,ITEven.IMPORT-ITEven.TOTIGV,0)   && GRAVABLE
vHVan =  vHVan + ITEVEN.TOTIGV
vEXO  =  vEXO  + IIF(iteven.totigv>0,0,ITEven.IMPORT)   && EXONERADO
VTOT  =  VDVAN+VHVAN+VEXO
RETURN ' '

FUNCTION val_cliE
*---------------
PARAMETER vruccli
VRECNO = RECNO()
LOCATE FOR RUCCLI=vruccli
IF FOUND()
	VDESCRI = DESCRI
	GO VRECNO
	REPLACE DESCRI WITH VDESCRI
ELSE
	GO VRECNO
ENDIF
RETURN

	
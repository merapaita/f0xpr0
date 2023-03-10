* 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
* ? RegFP.Prg  27/06/96                                 L:  848  ?	
* ? Rendici줻 de Fondos de Pago en Efectivo                      ?
* ? AUTOR   : ANAL. Segundo Edgar Correa Sandoval 				 ?
* 突袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴槁
*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '
Vmens01 = 'Rendici줻 del Fondo de Pago en Efectivo'

*- Abre archivos

USE Fonpag  IN  1 ORDER TAG FonPag1     ALIAS FonP
USE IteFp   IN  2 ORDER TAG IteFp1	    ALIAS IteFp
USE ParMae  IN  3 ORDER TAG ParMae1     ALIAS Parma
USE MaePre  IN  4 ORDER TAG MaePre1     ALIAS MaePre
USE Clase   IN  5 ORDER TAG Clase1      ALIAS Clase
USE Compag  IN  6 ORDER TAG Compag1     ALIAS Compag
USE Cajas   IN  7 ORDER TAG Cajas2      ALIAS Caja
USE itepar  IN  8 ORDER TAG itepar3		ALIAS itepar
*- Variables de trabajo (registro a trabajar)
PUBLIC vFun, m.fecha, vMes, m.Item, XDBF,vDVan,vHVan

STORE .T. TO Agrega,vFun
STORE SPACE(12) TO m.TipPre
STORE 0 TO m.Item
m.fecha=DATE()
*- Inicia proceso
SELECT FonP
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 10,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_C2 FROM 10,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C3 FROM 10,00 TO 23,38 DOUBLE ;
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


PROCEDURE Pantalla           && Pinta m쟳cara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @ 01,02 SAY "     Nro Doc :"  
 @ 01,40 SAY "         Mes :"  
 @ 02,02 SAY " Cadena Funcional :"
 @ 02,27 SAY " UG  UE  FN PRG SBPRG ACTPRY"
 @ 04,02 SAY "       Fecha :"  
 @ 05,02 SAY " Tipo Presup :"  
 @ 05,40 SAY "Saldo Inicial:"
 @ 06,02 SAY "         C/P :"  
 @ 06,40 SAY "    Reembolso:" 
 @ 07,02 SAY "     Cod.Ctc.:"  
 @ 07,40 SAY " Total Gastos:" 
 @ 08,02 SAY " Total I.G.V.:"
 @ 08,40 SAY "        Saldo:"
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT FonP
 SCATTER MEMVAR
 =val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
 @  1,16 SAY m.NumFp
 @  1,54 SAY m.NumMes 
 DO VIS_CALEN
 @  4,16 SAY m.Fecha
 @  5,16 SAY m.TipPre             PICTURE '@!'
 @  5,55 SAY m.SalIni             PICTURE '999,999,999,999.99'
 @  6,16 SAY m.nummescp+'.'+m.numcp COLOR SCHEME 02
 @  6,55 SAY m.ReemBols           PICTURE '999,999,999,999.99' 
 @  7,16 SAY m.codctc COLOR SCHEME 02
 @  7,55 SAY m.MonEje             PICTURE '999,999,999,999.99'
 IF m.SalIni + m.ReemBols - m.MonEje  >= 0
	 @  8,55 SAY m.SalIni + m.ReemBols - m.MonEje  PICTURE '999,999,999,999.99' COLOR SCHEME 02
	 @  8,22 SAY m.totigv PICTURE '999,999,999,999.99' COLOR SCHEME 02
 ELSE	 
	 @  8,55 SAY m.SalIni + m.ReemBols - m.MonEje  PICTURE '999,999,999,999.99' COLOR SCHEME 05
	 @  8,22 SAY m.totigv PICTURE '999,999,999,999.99' COLOR SCHEME 05
 ENDIF	 
 DO Vista_Hijo
RETURN


PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
 SELECT ITEFP
 SEEK ALLTRIM(m.NumFp)+ALLTRIM(m.NumMes)
* SET ORDER TO itefp2
 BROWSE;
   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
   WINDOW Wind_C2 KEY ALLTRIM(m.NumFp)+ALLTRIM(m.NumMes);
   FIELDS;
   Fecdoc   :H='Fecha'     :P='!!!!!!!!',;
   TipDoc   :H='Doc.'    :P='!!!!!!',;
   NumDoc   :H='N? Doc'     :P='!!!!!!!!!!',;
   Descri   :H='Descripci줻' :20,;
   Import   :H='Monto'   :P='99,999.99',;
   totigv   :H='I.G.V.'  :P='99,999.99',;
   CodPart  :H='Partida'
 SET ORDER TO itefp1
 SELECT  Fonp
RETURN


PROCEDURE Revis              && Revisi줻 de BD en browse
*--------------
 SELECT Fonp
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
   NumFp    :H= 'Nro.Fp.',;
   Nummes   :H= 'Nummes',;
   Codcad   :H= 'Cadena',;
   TipPre   :H= 'Presupuesto',;
   Fecha    :H='Fecha ',;
   x1=nummescp+'.'+numcp  :H='C/P',;
   codctc
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
 SELECT Fonp
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
 @ 0, 2 SAY 'N즡ero de documento que desea buscar:' GET Vbusca PICTURE '!!!!!!!'
 READ

 RELEASE WINDOW WBusca

 IF LASTKEY()=27
    SELECT Fonp
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
 SELECT Fonp
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
 SELECT Fonp
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
 SELECT Fonp
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
 
 @ 01,16 SAY m.numfp    
 @ 01,54 GET m.NumMes    PICTURE '!!'  VALID Val_Para(m.NumMes  ,'FECMES',' ',54,9) .AND. ValFecha()  WHEN .F.
 =val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
 @ 2,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  
 @ 04,16 GET m.fecha     PICTURE '@E'
 @ 05,16 SAY m.tippre    PICTURE '@!'
 @ 05,55 GET m.SalIni    PICTURE '999,999,999,999.99'
 @  6,16 SAY m.nummescp+'.'+m.numcp COLOR SCHEME 02
 @ 06,55 GET m.ReemBols  PICTURE '999,999,999,999.99'
 @  7,16 SAY m.codctc COLOR SCHEME 02
 READ VALID Val_Read()
 IF !EMPTY(m.CodCad) 
 	=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
    DO vis_calen
	vcadena=maepre.uniges+maepre.unieje+maepre.Codfun+maepre.CodPrg+maepre.CodSpr+maepre.ActPry
 ENDIF
 IF LASTKEY() # 27
    SELECT Fonp
    GATHER MEMVAR
    SS= Tra_Hijo()     && Corrige items
    IF SS .AND. LASTKEY()#27
       SELECT ITEFP
       *GATHER MEMVAR
       =Valida()
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF

 UNLOCK ALL
 SELECT Fonp
 DO Vista
RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT Fonp
 DO pantalla
 SCATTER MEMVAR BLANK
 m.Fecha=DATE()
 m.Periodo=RIGHT(DTOC(DATE()),2)
 vMes=STR(MONTH(DATE()))
 SELECT Parma
 SEEK 'CORREL'+'FONPAG'
 m.numfp = PADL(NumEnt+1,4,'0')
 @  7,55 SAY SPACE(20)
 @  8,55 SAY SPACE(20)
 oi=carcp()
 @ 01,16 SAY m.numfp     PICTURE "!!!!"
 @ 01,54 GET m.NumMes    PICTURE '!!'  VALID Val_Para(m.NumMes  ,'FECMES',' ',54,9) .AND. ValFecha()  
 =val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
 @ 2,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  
 @ 04,16 GET m.fecha     PICTURE '@E'
 @ 05,16 SAY m.tippre    PICTURE '@M DE OPERACION,DE INVERSION'
 =calcsdo()
 @ 05,55 GET m.SalIni    PICTURE '999,999,999,999.99' 
 @ 06,55 GET m.ReemBols  PICTURE '999,999,999,999.99'
 READ VALID Val_Read()
 IF LASTKEY()#27
    vAnula = .F.
    SELECT Fonp
    IF F_Appd()
       GATHER MEMVAR
       UNLOCK ALL
       SELECT Parma
   	   SEEK 'CORREL'+'FONPAG'
       REPLACE NumEnt WITH NumEnt + 1
       SELECT Fonp
       *DO Agreg_item
       SELECT ITEFP
       SCATTER MEMVAR
       
       DO WHILE .T.
          DO Tra_hijo
          IF LASTKEY() = 27
             IF YESNO( '? Cancela el Ingreso ?' )
                vAnula = .T.
                EXIT
             ENDIF
          ELSE
             IF YESNO( '? Est쟮 correctos los datos ?')
               =Valida()
		       SELE fonP
		       SCATTER MEMVAR
	    	   SELE compag
			   SEEK ALLTRIM(m.nummescp)+m.numcp+ALLT(m.codctc)
			   IF RLOCK()  AND FOUND()
				  REPLACE estfon   WITH '?'
		       ENDIF
		       UNLOCK
		       SELE fonp
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
 SELECT Fonp
 DO Vista
 RETURN



FUNCTION Tra_Hijo
*----------------
 vFun=.t.
 HIDE MENU mMenu
 ACTIVATE SCREEN
 VTempo = '같 췋4? I.G.V. 같F5->Agregar같F6->Repetir같F8->Eliminar같F10->Terminar같'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F4  DO Agreg_igv
 ON KEY LABEL F5  DO agreg_item
 ON KEY LABEL F6  DO Repit_item
 ON KEY LABEL F8  DO elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
 STORE 0 TO m.import
 SELECT ITEFP
 SEEK ALLTRIM(m.NumFp)+ALLTRIM(m.NumMes)
 IF !FOUND()
    DO Agreg_Item
 ENDIF
 BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
  KEY ALLTRIM(m.NumFp)+ALLTRIM(m.NumMes);
  FIELDS;
   Fecdoc   :H='Fecha',;
   NumDoc   :H='N쬎oc'    :F :P='!!!!!!!!!!',;
   Descri   :H='Descripcion':P='@!':F,;
   TipDoc   :H='TipDoc' :V=val_Para(TipDoc,"DOCCAJ","Tipdoc"):F:P='!!!',;
   Import   :H='   Importe    ':P='999,999,999,999.99',;
   TOTigv	:H= 'I.G.V.':w=.f.,;
   codpart  :H= 'Partid':V=Val_Partida(codpart):F,;
   AA=IIF(EMPTY(CODPART)OR LEN(ALLT(codpart))<6,' ',val_para(right(ITEFP.CodPart,2),'ESPGAS','D',22,40)):H='Descripci줻':40 :W=!EMPTY(CODPART)
     
 vOrd=ORDER()
*  ss=val_Para(TipDoc,"DOCCAJ") :H="Detalle" :20,;   

 SELECT ITEFP
 SET ORDER TO vOrd

 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5

 UNLOCK ALL

SELECT Fonp
ACTIVATE SCREEN
vTempo = ' Ingresa  Elige  Busca  Anterior  Siguiente  Corrige  Anula  Lista Termina '
DO Logos WITH Rotulo1,vtempo
SHOW MENU mMenu
RETURN  vFun


PROCEDURE Agreg_item
*-------------------
SELECT IteFp
IF F_Appd()
    REPLACE NumFP   WITH FonP.numfp,;
            NumMes  WITH FonP.NumMes,;
            FecDoc  WITH FonP.fecha 
ENDIF   
UNLOCK
RETURN

PROCEDURE repit_item
*-------------------
SELECT IteFp
vfec=itefp.fecdoc
vtip=itefp.tipdoc
vdoc=itefp.numdoc
vdes=itefp.descri
vimp=itefp.import
vpar=itefp.codpart
vigv=itefp.totigv
IF F_Appd()
    REPLACE NumFP   WITH m.numfp,;
            NumMes  WITH m.NumMes,;
            FecDoc  WITH vfec,;
            tipDoc  WITH vtip,;
            numDoc  WITH vdoc,;
            descri  WITH vdes,;
            import  WITH vimp,;
            totigv  WITH vigv,;
            codpart WITH vpar
ENDIF   
UNLOCK
RETURN


PROCEDURE Elimi_item
*-------------------
SELECT IteFp
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
IF yesno("쭭NULA Documento?")
   vErdad=.T.
   vAli=ALIAS()
   vOrd=ORDER()
   SELECT Fonp
   vnumfp=ALLTRIM(Fonp.numfp)
   SELECT ITEFP
   GO TOP
   SEEK ALLTRIM(vnumfp)

   SELECT (vAli)
   SET ORDER TO vOrd
   SELECT Fonp
   IF f_lock(1)
      DELETE
      *-Busca y elimina los Items
      SELECT ITEFP
      GO TOP
      SEEK ALLTRIM(vnumfp)
      IF FOUND()
         SCAN WHILE vnumfp = ALLTRIM(ITEFP.numfp)
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
  
 SELECT Fonp
 SCATTER Memvar
 vRec=RECNO()
 
 vind = SYS(3) + '.idx'
 xMes = MONTH(DATE())
 xAno = VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))
 SELECT ITEFP
 SET RELATION TO NumFP+NumMes INTO FONP

 DEFINE WINDOW lis FROM 3,5 TO 19,75 FLOAT DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW lis
 STORE 1  TO vtocta,vestado,vlista
 STORE SPACE(14) TO vcta
 STORE SPACE(2)  TO vano
 STORE SPACE(4)  TO vcli
 vcli=ALLT(m.numfp)
 vano=ALLT(m.nummes)
 STORE DATE() TO vfecini, vfecfin
 @ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento '
 @ 04,01 SAY "     N? Documento : "
 @ 04,22 GET vano    WHEN vlista=1  PICTURE '!!'
 @ 04,25 GET vcli    WHEN vlista=1  PICTURE '!!!!' VALID val_fp()
 *@ 07,01 SAY " Fecha de Emisi줻 : "
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
	      SET FILT TO numfp=ALLT(vcli) and nummes=ALLT(vano)
	      VDVAN = 0
	      VHVAN = 0
		  IF FOUND()
	       	  DEACTIVATE WINDOW standby
              DO REPORTE WITH 2,"RepFonP"," Fondo de Pagos en Efectivo "
	   	  	  SET ORDE TO ITEFP2
	   		  do resumen
			  SELECT FONP
			  SEEK vcli+vano
			  SELECT RESUMEN
	   		  if !eof()
				 DEACTIVATE WINDOW standby
            	 DO REPORTE WITH 2,"RepFonR"," Fondo de Pagos en Efectivo "	    	  	   	
        	  endif
        	  SELECT RESUMEN
        	  USE
        	  ERASE(xDBF)
        	  SELECT ITEFP
        	  SET ORDE TO ITEFP1
	      ELSE
              DO Standby WITH vmens08
	       	  DEACTIVATE WINDOW standby
          ENDIF
    ENDCASE
 ENDIF
 SELECT ITEFP
 SET FILT TO
 SELECT Fonp
 IF vRec > 0
    GO vRec
 ENDIF
 DO vista
RETURN


FUNCTION val_fp             && Revisi줻 de BD en browse
*--------------
AS=ALIAS()
SELE Fonp
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
SEEK vcli+vano
IF !FOUND()
	vtemp = RECNO()
	HIDE MENU mmenu
	ACTIVATE SCREEN
	vtempo = '같같같같같?Presione 췋10? para seleccionar  o  췊sc? para cancelar같같같같같같'
	DO logos WITH rotulo1,vtempo
	ON KEY LABEL f10 KEYBOARD CHR(23)
	BROWSE WINDOW wind_C0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
  		x1=numfp+'.'+nummes  :H=' N? FP' ,;
        codcad   :H= 'Cadena',;
        TipPre   :H= 'Presupuesto',;
        Fecha    :H='Fecha '
	vtempo = '같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같'
	DO logos WITH rotulo1,vtempo
	SHOW MENU mmenu
	IF LASTKEY()=27
		GOTO BOTT
	ENDIF
ENDIF
vAno = NumMes
vCli = NumFP

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
SELE Itefp
SEEK ALLT(m.numfp)+ALLTRiM(m.nummes)
SCAN WHILE Itefp.numfp=ALLT(numfp) AND Itefp.nummes=ALLTRIM(M.NUMMES)
     vAcum = vAcum + Itefp.import
     vAcumIGV = vAcumIGV + Itefp.TOTIGV
ENDSCAN
SELE FonP
IF vAcum > Fonp.SalIni + Fonp.ReemBols
   DO StandBy WITH 'Existe sobregiro'
ENDIF   
IF F_LOCK(1)
   REPLACE FonP.MonEje WITH vAcum
   REPLACE FonP.TOTIGV WITH vAcumIGV
   IF FonP.SalIni + FonP.ReemBols - FonP.MonEje >= 0
	   @  8,55 SAY FonP.SalIni + FonP.ReemBols - FonP.MonEje PICTURE '999,999,999,999.99' COLOR SCHEME 02
	   @  8,22 SAY FonP.TOTIGV PICTURE '999,999,999,999.99' COLOR SCHEME 02
   ELSE	   
	   @  8,55 SAY FonP.SalIni + FonP.ReemBols - FonP.MonEje PICTURE '999,999,999,999.99' COLOR SCHEME 05
	   @  8,22 SAY FonP.TOTIGV PICTURE '999,999,999,999.99' COLOR SCHEME 05
   ENDIF	   
ENDIF
RETURN

PROCEDURE carcp  &&Captura C/P
*--------------
vfun = .T.
OK = FOUND()
SELECT ComPag
*SET FILT TO flagcon='F' AND ESTADO<>'99' AND PRES<>'*' AND estfon<>'?'
SET FILT TO DOCREF='RNF' AND ESTADO<>'99' AND estfon<>'?'
GO TOP
IF EOF()
	DO standby WITH 'No existe Comprobantes de Pago'
	SET FILT TO
	RETURN .F.
ENDIF
m.nummescp ='00'
m.numcp=SPACE(4)
vcc=SPACE(14)
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N? C/P: " GET m.nummescp DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 say '.'
@ 1,17 GET m.numcp DEFAULT SPACE(4)
@ 2, 4 SAY "Cta.Cte.:" GET vcc FUNCTION '!' VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",vcc,1,2,24)
READ
SELE COMPAG
deactivate WINDOW standby
IF LASTKEY()=27
   SET FILTER TO
   RETURN .F.
ENDIF
m.nummescp = PADL(ALLTRIM(m.nummescp),2,'0')
m.numcp = PADL(ALLTRIM(m.numcp),4,'0')
m.codctc=ALLT(m.codctc)
IF !SEEK(alltrim(m.nummescp)+m.numcp+alltrim(m.codctc))
  GO BOTTOM
  ON KEY LABEL F10 KEYBOARD CHR(23)
  DEFINE WINDOW EliHC FROM 1,1 TO 18,79 TITLE " Elija el C/P con F10 "
  BROWSE NOED WINDOW EliHC COLOR SCHEME 10 FIELDS ;
     numcp :H="C/P",;
     nummes :H="Mes",;
     XX=DOCREF+' '+NUMREF+' '+REFER:H='DOCREF',;
     tipprv,;
     codemp :H="CodEmp",;
     x2=TRANSF(import,'99,999,999.99'):H="Monto",;
     fecCP :H="fecha",;
     x3=codcaD+' '+codctc :H="Cal.CTC"
ENDIF
IF LASTKEY()#27
    SELE compag
	m.nummescp = nummes
	m.numcp    = numcp
	m.codctc   = codctc
	m.reembols = import
	m.nummes   = nummes
	m.fecha    = feccp
	m.codcad   = codcad
	m.tippre   = IIF(tipfun='00','DE OPERACION','DE INVERSION')
	SHOW GETS
	SELECT COMPAG
	SET FILT TO 
	SELECT fonp
	 @  6,16 SAY m.nummescp+'.'+m.numcp COLOR SCHEME 02
	 @  7,16 SAY m.codctc COLOR SCHEME 02
ENDIF
RETURN .T.


FUNCTION REMBYS
*----------------
vAli=ALIAS()
vOrd=ORDER()
vSubCta=LEFT(Fonp.CodSCta,1)
SELECT Parma
RETURN


FUNCTION Descri
*--------------
PUBLIC aa
aa = IIF(!EMPTY(CODPART),VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'D'),' ')
RETURN aa


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

FUNCTION calcsdo  &&Calcula Saldo inicial
*---------------
SELE fonp
*SET ORDER TO FonPag1
IF !BOF()
	SKIP -1
	IF fonp.codctc=m.codctc
	   m.salini=fonp.salini+fonp.reembols-fonp.moneje
	ENDIF
	SKIP
ENDIF	
*SET ORDER TO FonPag1
RETURN   

PROCEDURE vis_calen   &&Vista Codificaci줻 Funcional Program쟴ica
*-------------------
@ 02,22 SAY m.codcad
@ 02,27 SAY " UG  UE  FN PRG SBPRG ACTPRY"
@ 03,28 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
RETURN

PROCEDURE Agreg_iGV
*-----------------
private as
AS=ALIAS()
sele itefp
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

PROCEDURE RESUMEN
*----------------
xDBF=SYS(3)+".DBF"
SELE itefp
COPY STRUC TO (xDBF)
USE (xDBF) IN 0 ALIAS resumen
SELECT ITEFP
GO TOP
SCAN
	vCodPart1 = codpart
	vCodPart2 = codpart
	vTotPar   = 0
	vTotIgv   = 0
	DO WHILE vCodPart1 = Vcodpart2 AND !EOF()
		vTotPar = vTotPar + import
		vTotIgv = vTotIgv + TotIgv
		SKIP
		vCodPart2 = codpart
	ENDDO
	SKIP -1
	SCATTER MEMVAR
	SELECT resumen
	APPEND BLANK
	GATHER MEMVAR
	REPLACE import with vTotPar
	REPLACE totigv with vTotIgv	
	SELECT itefp
ENDSCAN
SELECT resumen
GO TOP
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
vDVan =  vDVan + ITEFP.IMPORT
vHVan =  vHVan + ITEFP.TOTIGV
RETURN ' '

FUNCTION val_partida
*-----------------
PARAMETER vcodpart
HIDE MENU mMenu
ACTIVATE SCREEN
valias = Alias()
SELECT itepar
SET FILTER TO codcad='0001' AND codfte='09'
GO TOP
SEEK m.PERIODO+'000109'+vcodpart
IF !FOUND()
	DO standby WITH 'Partida No PresupuEstada'
 	DEFINE WINDOW parti FROM 5,10 TO 18,70 DOUBLE ;
 	TITLE 'Partidas Presupuestadas' COLOR SCHEME 10
 	ACTIVATE WINDOW parti
 	GO TOP
 	BROWSE NOEDIT NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE IN WINDOW parti;
	FIELDS;	
   		codpart  :H= 'Partid',;
		BB=val_para(right(itepar.CodPart,2),'ESPGAS','D',22,40) :H='Descripci줻':40

	release WINDOWS parti
	vcodpart = ITEPAR.codpart
ENDIF
set filter to
SELECT (valias)
REPLACE codpart with VCODPART
return .t.
PARAMETERS NEWSISTEM
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RegFP.Prg  27/06/96                                 L:  848  ³	
* ³ Rendici¢n de Fondos de Pago en Efectivo                      ³
* ³ AUTOR   :  PILAR PACHERRE NAVARRO                            ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾
*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '
Vmens01 = 'Rendici¢n del Fondo de Pago en Efectivo'

*- Abre archivos

USE Fonpag   IN  1 ORDER TAG FonPag1   ALIAS FonP
USE IteFp    IN  2 ORDER TAG IteFp1	   ALIAS IteFp
USE ParMae   IN  3 ORDER TAG ParMae1   ALIAS Parma
USE MaePre   IN  4 ORDER TAG MaePre1   ALIAS MaePre
USE Clase    IN  5 ORDER TAG Clase2    ALIAS Clase
USE Compag   IN  6 ORDER TAG Compag1   ALIAS Compag
USE Cajas    IN  7 ORDER TAG Cajas2	   ALIAS Caja
USE ITECLA   IN  8 ORDER TAG ITECLA1
USE Auxil 	 IN  9 order TAG Auxil1    ALIAS Auxi
USE IteUsuOp IN 10 ORDER TAG IteUsuOp1 ALIAS SubOp
USE CatAsi   IN 11 ORDER TAG CatAsi4   ALIAS CatAsi
USE Cuentas  IN 12 order tag Cuentas6  ALIAS Cuenta

*- Variables de trabajo (registro a trabajar)
PUBLIC vFun, m.fecha, vMes, m.Item,VCTA,VNCP,vDVan,vHVan

STORE .T. TO Agrega,vFun
STORE SPACE(2) TO m.TipPre
STORE 0 TO m.Item,vdVan,vHVan
m.fecha=DATE()
*- Inicia proceso
SELECT FonP
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


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @ 00,02 SAY "  Responsable :"  
 @ 00,40 SAY "      Mes H/C :"  
 @ 01,02 SAY "      Nro Doc :"  
 @ 01,40 SAY "          Mes :"  
 @ 02,02 SAY " Cad.Funcional:" 
 @ 02,19 SAY " UG  UE  FN PRG SBPRG ACTPRY"
 @ 04,02 SAY "        Fecha :"  
 @ 05,02 SAY "  Tipo Presup.:" 
 @ 05,40 SAY "Saldo Inicial :"  
 @ 06,02 SAY "          C/P :"
 @ 06,40 SAY "    Reembolso :" 
 @ 07,02 SAY "      Cod.Ctc.:"  
 @ 07,40 SAY "  Total Gastos:" 
 @ 08,02 SAY "    Total IGV :"
 @ 08,40 SAY "        Saldo :"
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 m.Periodo=RIGHT(DTOC(DATE()),2)
 SELECT FonP
 SCATTER MEMVAR
 @  0,18 SAY Val_aux(m.CodEmp,'03',' ',18) COLOR SCHEME 02 		&&SIZE 1,19
* @  0,18 SAY m.Persona  SIZE 1,19 COLOR SCHEME 02 
 @  0,55 SAY m.NumMesHc +'.'+NumHc COLOR SCHEME 02
 @  1,18 SAY m.NumFp    COLOR SCHEME 02
 @  1,55 SAY m.Fecha
 @  2,18 SAY val_codca1(ALLTRIM(m.codcad),m.periodo,'D',21,40)
 @  3,21 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
 @  4,18 SAY m.Fecha
 @  5,18 SAY TipPre
 @  5,55 SAY m.SalIni       PICTURE '999,999,999,999.99'
 @  6,18 SAY m.NumCp   
 @  6,55 SAY m.ReemBols     PICTURE '999,999,999,999.99' 
 @  7,18 SAY m.CodCtc  
 @  7,55 SAY m.MonEje       PICTURE '999,999,999,999.99'
 @  8,18 SAY m.TotIgv       PICTURE '999,999,999,999.99'
* @  7,55 SAY m.MonEje      PICTURE '999,999,999,999.99'
 IF m.SalIni + m.ReemBols - m.MonEje  >= 0
	 @  8,55 SAY m.SalIni + m.ReemBols - m.MonEje  PICTURE '999,999,999,999.99' COLOR SCHEME 02
 ELSE	 
	 @  8,55 SAY m.SalIni + m.ReemBols - m.MonEje  PICTURE '999,999,999,999.99' COLOR SCHEME 05
 ENDIF	 
 DO Vista_Hijo
 IF !vFlag2$'J*'
	DO SubOpc
ENDIF

RETURN


PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
 SELECT ITEFP
 SEEK ALLTRIM(m.NumFp)+ALLTRIM(m.NumMes)
 SET ORDER TO itefp2
 BROWSE;
   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
   WINDOW Wind_C2 KEY ALLTRIM(m.NumFp)+ALLTRIM(m.NumMes);
   FIELDS;
   Fecdoc   :H='Fecha'    :P='!!!!!!!!',;
   TipDoc   :H='Doc.'     :P='!!!!!!',;
   NumDoc   :H='N§ Doc'     :P='!!!!!!!!!!',;
   Descri   :H='Descripci¢n' :20,;
   Import   :H='Monto'   :P='99,999,999.99',;
   TotIgv   :H='IGV'     :P='99,999.99',;
   CodPart  :H='Partida'    :P='!!!!!!!!'
 SET ORDER TO itefp1
 SELECT  Fonp
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT Fonp
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
   NumFp    :H= 'Nro.Fp.',;
   Nummes   :H= 'Nummes',;
   TipPre   :H= 'Presupuesto',;
   Fecha    :H='Fecha ',;
   x1=nummescp+'.'+numcp  :H='C/P',;
   codctc
   vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
   *CodPrg   :H= 'Prog',;
   *Codsubpr :H= 'Subp',;
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
 STORE SPACE(04) TO VNUME
 STORE SPACE(02) TO VMES
* Vbusca
 @ 0, 2 SAY 'N£mero de documento que desea buscar:' GET VNUME PICTURE '!!!!'
 @ 0, 44 SAY '.'
 @ 0, 45 GET VMES PICT '!!'
 READ
 VBUSCA=VNUME+VMES
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
 m.Periodo=RIGHT(DTOC(DATE()),2)
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
 @ 00,18 GET m.CodEmp VALID Val_aux(m.CodEmp,'03',' ',18)
 @ 00,24 GET m.persona  WHEN ValPer() SIZE 1,24

* @ 00,18 GET m.persona  SIZE 1,24
 @ 00,55 SAY m.nummeshc +'.'+m.numhc      COLOR SCHEME 02
 @ 01,18 SAY m.numfp    
 @ 01,55 GET m.NumMes    PICTURE '!!'  VALID Val_Para(m.NumMes  ,'FECMES',' ',54,9) .AND. ValFecha()  WHEN .F.
 @ 02,18 GET m.CodCad    PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',21,47) 	  
 @ 04,18 GET m.fecha     PICTURE '@E'
 @ 05,18 GET m.tippre    
 *PICTURE '!!'  VALID Val_Para(m.tippre ,'CODFTE',' ',19,10)
 @ 05,55 GET m.SalIni    PICTURE '999,999,999,999.99'
 @ 06,18 SAY m.nummescp+'.'+m.numcp COLOR SCHEME 02
 @ 06,55 GET m.ReemBols  PICTURE '999,999,999,999.99'
 @ 07,18 SAY m.codctc    COLOR SCHEME 02
 @ 07,55 GET m.MonEje    PICTURE '999,999,999,999.99'
 READ VALID Val_Read()
 IF ((m.SalIni + m.ReemBols) - m.MonEje)  >= 0
	 @  8,55 SAY (m.SalIni + m.ReemBols) - m.MonEje  PICTURE '999,999,999,999.99' COLOR SCHEME 02
 ELSE	 
	 @  8,55 SAY (m.SalIni + m.ReemBols) - m.MonEje  PICTURE '999,999,999,999.99' COLOR SCHEME 05
 ENDIF	 
 IF LASTKEY() # 27
	GATHER MEMVAR
    SS= Tra_Hijo()     && Corrige items
    IF SS .AND. LASTKEY()#27
       SELECT ITEFP
*       GATHER MEMVAR
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
*=CAJAFP() &&Captura C/P
*aa=CARCP() &&Captura C/P
*   	IF LASTKEY()=27 OR !aa
*	   SELECT FonP
*	   DO vista
*	   RETURN
*	ENDIF
 ACTIVATE WINDOW Wind_C0
 SCATTER MEMVAR BLANK
 m.Fecha=DATE()
 m.Estado = '00'
* m.salini=0.00
* m.reembols=0.00
 m.Periodo=RIGHT(DTOC(DATE()),2)
* m.numfp=space(4)
* m.codcad=space(4)
* vMes=STR(MONTH(DATE()))
 SELECT Parma
 SEEK 'CORREL'+'FONPAG'
 m.numfp = PADL(NumEnt+1,4,'0')
* @  7,55 SAY SPACE(20)
* @  8,55 SAY SPACE(20)
=CAJAFP()			&&Captura C/P
 SELE FonP
 @ 00,18 GET m.CodEmp VALID Val_aux(m.CodEmp,'03',' ',18) 		&&SIZE 1,19
 @ 00,24 GET m.persona  WHEN ValPer() SIZE 1,19
* @ 00,55 SAY m.nummeshc+'.'+m.numhc
* @ 00,55 GET m.nummeshc
* @ 00,57 SAY '.'
* @ 00,58 GET m.numhc    
 @ 01,18 SAY m.numfp    
*@ 01,55 GET m.NumMes 
@ 02,18 GET m.CodCad  PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',21,47) 	  
 @ 04,18 GET m.fecha 
 @ 05,18 GET m.tippre  
 *Val_Para(m.tippre ,'CODFTE',' ',19,10)
=calcsdo()
 @ 05,55 GET m.SalIni    PICTURE '999,999,999,999.99'
* @ 06,18 say m.nummescp+'.'+m.numcp
*@ 06,18 GET m.nummescp
* @ 06,22 say '.'
* @ 06,23 GET m.numcp 
 @ 06,55 SAY m.ReemBols  PICTURE '999,999,999,999.99'
 @ 06,55 GET m.ReemBols  PICTURE '999,999,999,999.99'
* @ 07,18 GET m.codctc    COLOR SCHEME 02
 @ 07,55 GET m.MonEje    PICTURE '999,999,999,999.99'
 @ 08,18 GET m.TotIgv    PICTURE '999,999,999,999.99'
 READ VALID Val_Read()
 IF ((m.SalIni + m.ReemBols) - m.MonEje)  >= 0
	 @  8,55 SAY (m.SalIni + m.ReemBols) - m.MonEje  PICTURE '999,999,999,999.99' COLOR SCHEME 02
 ELSE	 
	 @  8,55 SAY (m.SalIni + m.ReemBols) - m.MonEje  PICTURE '999,999,999,999.99' COLOR SCHEME 05
 ENDIF	 

IF LASTKEY()#27
	vAnula = .F.
	SELECT Fonp
	IF F_Appd()
		GATHER MEMVAR
		UNLOCK ALL
*		SELECT Parma
*		SEEK 'CORREL'+'FONPAG'
*		REPLACE NumEnt WITH NumEnt + 1
		SELECT Fonp
		DO Agreg_item
		SELECT ITEFP
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
					SELE fonP
					SCATTER MEMVAR
					SELE compag
					SEEK ALLTRIM(m.nummescp)+m.numcp		&&+ALLT(m.codctc)
					IF RLOCK()  AND FOUND()
					
						REPLACE estfon   WITH 'û'
					ENDIF
					UNLOCK
					SELE fonp
					EXIT
				ENDIF
			ENDIF
		ENDDO
		IF vAnula
			DO Anula
		 ELSE
			SELECT Parma
			* SEEK "CORREL"+ alltrim("MOVBCO")
			SEEK "CORREL"+ "FONPAG"
			IF RLOCK()
				REPLACE NumEnt WITH NumEnt + 1
				UNLOCK
			ENDIF
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

FUNCTION ValPer
*--------------
m.Persona = ALLTRIM(Auxi.Descri)

FUNCTION Tra_Hijo
*----------------
vFun=.t.
HIDE MENU mMenu
ACTIVATE SCREEN
VTempo = '°°°°°°F5->Agregar°°°°°°F6->Repetir°°°°°F8->Eliminar°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
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
BROWSE NOAPPEND NODELETE NOMENU NOCLEAR SAVE NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
	KEY ALLTRIM(m.NumFp)+ALLTRIM(m.NumMes);
	FIELDS;
	Fecdoc   :H='Fecha',;
	TipDoc   :H='TipDoc' :V=val_Para(TipDoc,"DOCCAJ","Tipdoc"):F:P='!!!',;
	NumDoc   :H='N§Doc'    :F :P='!!!!!!!!!!',;
	DecJur   :H='Dec. Jur.'    :F ,;
	Descri   :H='Descripcion':P='@!':F,;
	Import   :H='Monto':P='999,999,999,999.99' :V=CALC_IGV() :F,;
	TotIgv   :H='IGV'  :P='@K 999,999,999,999.99',;
	CodPart  :H= 'Partida':V=ValAsi1(ALIAS(),CodPart,'8','codpart','C') AND Val_Niv() :F,;
	CODCLA   : H= 'Cla.':V=Val_cla(codpart,CODPART+codcla,'codcla'):F 
*	aa = IIF(EMPTY(CodPart),' ',IIF(LEN(ALLTRIM(CodPart))=6,val_para(SUBSTR(codpart,5,2),'ESPGAS','D'),VAL_PARA1(SUBSTR(CodPart,7,2),'SUBESP'+SUBSTR(CodPart,5,2),'D'))) :H='Descripci¢n':40 :W=.F.

*	CodPart  :H='Partida':P='!!!!!!!!' :V=IIF(EMPTY(codpart),.T.,val_part2(CodPart,"CodPart")) :F,;
*   aa = IIF(EMPTY(CodPart),' ',val_para(right(codpart,2),'ESPGAS','D',28,40)) :H='Descripci¢n':40 :W=.F.     
*   aa = IIF(EMPTY(CodPart),' ',val_para(right(codpart,2),'ESPGAS','D',28,40)) :H='Descripci¢n':40 :W=.F.
*   CodPart  :H='Partida':P='!!!!!!!!' :V=val_part1(right(codpart,2),LEFT(codpart,4),'CodPart') :F,;


*V=IIF(EMPTY(codpart),.T.,val_part())

   vOrd=ORDER()
	
*	SEEK ALLTRIM(m.NumFp)+ALLTRIM(m.NumMes)
*	IF FOUND()
*		m.TotIGV = 0
*		SCAN WHILE ALLTRIM(m.NumFp)+ALLTRIM(m.NumMes)=ALLTRIM(NumFp)+ALLTRIM(NumMes)
*			m.TotIgv = m.TotIGV + TotIGV
*		ENDSCAN
*		SELE FonP
*		SEEK ALLTRIM(m.NumFp)+ALLTRIM(m.NumMes)
*		IF FOUND()
*			REPLACE TotIGV WITH m.TotIGV
*		ENDIF
*	ENDIF
		

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


FUNCTION CALC_IGV
*-----------------
IF TipDoc='01'
	nIgv = IIF(DTOC(FecDoc,1)>"20110228",1.18,1.19)
	REPLACE TOTIGV WITH ROUND(Import-(Import/nIgv),2)
 ELSE
	REPLACE TOTIGV WITH 0
ENDIF
RETURN .T.


PROCEDURE Agreg_item
*-------------------
SELECT IteFp
IF F_Appd()
    REPLACE NumFP   WITH m.numfp,;
            NumMes  WITH m.NumMes,;
            FecDoc  WITH m.fecha 
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
vglo=itefp.glosa
*vren=itefp.rendic
vigv=itefp.totigv
*vcad=itefp.codcal
IF F_Appd()
    REPLACE NumFP   WITH m.numfp,;
            NumMes  WITH m.NumMes,;
            FecDoc  WITH vfec,;
            tipDoc  WITH vtip,;
            numDoc  WITH vdoc,;
            descri  WITH vdes,;
            import  WITH vimp,;
            codpart WITH vpar,;
            glosa   WITH vglo,;
            totigv  with vigv

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
IF yesno("¨ANULA Documento?")
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
 SET RELATION TO CodPart+CodCla INTO IteCla ADDITIVE

 DEFINE WINDOW lis FROM 2,5 TO 18,75 FLOAT DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW lis
 STORE 1  TO vtocta,vestado,vlista
 STORE SPACE(14) TO vcta
 STORE SPACE(2)  TO vano
 STORE SPACE(4)  TO vcli
 STORE SPACE(3)  TO vrendi
 STORE 0 TO m.Item,vdVan,vHVan
 vcli=ALLT(m.numfp)
 vano=ALLT(m.nummes)
 STORE DATE() TO vfecini, vfecfin
* @ 02,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento   ;Res£men'
 @ 02,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento  '
 @ 06,01 SAY "     N§ Resoluci¢n: "
 @ 06,22 GET vano    WHEN vlista=1  PICTURE '!!'
 @ 06,25 GET vcli    WHEN vlista=1  PICTURE '!!!!' VALID val_fp()
 @ 08,01 say "     N§ Rendici¢n : "
*@ 08,25 GET vrendi  WHEN vlista=1  PICTURE '!!!' 
*@ 09,01 SAY " Fecha de Emisi¢n : "
*@ 09,22 GET vfecini  PICTURE '@D' COLOR SCHEME 7   WHEN vlista=2
*@ 09,32 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7 WHEN vlista=2
 @ 12,18 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
 READ CYCLE

 RELEASE WINDOW lis
 IF okcancel = 1
	ACTIVATE WINDOW standby
	xcolor = '*' + SET('COLOR')
*	@ 01,02 SAY 'Espere un momento...Reporte en proceso' COLOR &xcolor
	DO CASE
		CASE VLISTA=1
			SELE ITEFP
*			INDEX ON ALLTRIM(NUMFP)+ALLTRIM(NUMMES)+RENDIC+CODCAL+DTOC(FECDOC,1) TO VIND FOR numfp=ALLT(vcli) and nummes=ALLT(vano) AND TIPDOC<>'REC'
			@ 01,02 SAY 'Espere un momento...Reporte en proceso' COLOR &xcolor
			SET FILT TO numfp=ALLT(vcli) and nummes=ALLT(vano) AND TIPDOC<>'REC' && and rendiC=vrendi
			IF FOUND()
				DEACTIVATE WINDOW standby
				DO REPORTE WITH 2,"RepFonP"," Fondo de Pagos en Efectivo ",2,.F.,.T.
				vIdx = SYS(3)+".Idx"
				INDEX ON CodPart+Codcla to (vIdx)
				DO REPORTE WITH 2, "REPFONR"," FPE",2,.F.,.T. 
				SET INDEX TO
				SET ORDER TO IteFp1
				ERASE (vIdx)
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

FUNCTION vsuma
*-------------
vDVan = vDVan + itefp.import
vHVan = vHVan + itefp.TotIgv
RETURN 0

FUNCTION fin
*-----------
return eof()



FUNCTION val_fp             && Revisi¢n de BD en browse
*--------------
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
	vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	ON KEY LABEL f10 KEYBOARD CHR(23)
	BROWSE WINDOW wind_C0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
  		x1=numfp+'.'+nummes  :H=' N§ FP' ,;
        Codsubpr :H= 'Subp',;
        TipPre   :H= 'Presupuesto',;
        Fecha    :H='Fecha '
	vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	*CodPrg   :H= 'Prog',;
	DO logos WITH rotulo1,vtempo
	SHOW MENU mmenu
	IF LASTKEY()=27
		GOTO BOTT
	ENDIF
ENDIF
vAno = NumMes
vCli = NumFP

ON KEY LABEL f10
SELE Fonp
RETURN


FUNCTION Mayor
*-------------
vAli=ALIAS()
vOrd=ORDER()
SELECT ITEFP
IF ITEFP.import > m.TotAsig .OR. ITEFP.import <= 0
   DO StandBy WITH "ERROR! el monto excede lo asignado, es NEGATIVO o es CERO. Proceda a Corregir"
   RETURN .F.
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN .T.


FUNCTION W_Can
*-------------
vAli=ALIAS()
vOrd=ORDER()
SELECT ITEFP
AntCan = ITEFP.import
SELECT (vAli)
SET ORDER TO vOrd
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


FUNCTION BuscPrg &&Buscar Actividad
*---------------
PRIVATE vKey
vKey=ALLTRIM(ITEFP.Codprg)+ALLTRIM(ITEFP.CodSubPr)
vAli=ALIAS()
vOrd=ORDER()
SELE MaePre
SET ORDER TO MaePre4
SEEK vKey
vProyecto = MaePre.Descri
SELECT (vAli)
SET ORDER TO TAG vOrd
RETURN vProyecto


FUNCTION Valida && Suma el ingreso de todos los Items ingresados.
*--------------
PRIVATE Vacum, VSREC
vAcum=0
VSREC=0
vTotIGV = 0
SELE Itefp
IF FOUND()
	SEEK ALLT(m.numfp)+ALLTRiM(m.nummes)
	SCAN WHILE numfp=ALLT(m.numfp) AND nummes=ALLTRIM(M.NUMMES)
		IF TIPDOC='REC'
			VSREC=VSREC+ITEFP.IMPORT
		 else
			vAcum = vAcum + Itefp.import
		endif
		vTotIGV = vTotIGV + IteFP.TotIGV
	ENDSCAN
ENDIF

SELE FonP
IF vacum+vsrec > Fonp.SalIni + Fonp.ReemBols
   DO StandBy WITH 'Existe sobregiro'
ENDIF   
IF F_LOCK(1)
	REPLACE FonP.MonEje WITH VACUM
	REPLACE TotIGV WITH vTotIGV
*   REPLACE FONP.MONSREC WITH VSREC
	IF (FonP.SalIni + FonP.ReemBols) - FonP.MonEje >= 0
		@  8,55 SAY (FonP.SalIni + FonP.ReemBols) - FonP.MonEje PICTURE '999,999,999,999.99' COLOR SCHEME 02
	 ELSE	   
		@  8,55 SAY (FonP.SalIni + FonP.ReemBols) - FonP.MonEje PICTURE '999,999,999,999.99' COLOR SCHEME 05
	ENDIF	   
ENDIF
RETURN

PROCEDURE carcp  &&Captura C/P
*--------------
vfun = .T.
OK = FOUND()
SELECT ComPag
SET FILT TO flagcon='F' AND ESTADO<>'99' AND PRES<>'*' AND estfon<>'û'
GO TOP
IF EOF()
	DO standby WITH 'No existe Comprobantes de Pago'
	SET FILT TO
	RETURN .F.
ENDIF
m.nummescp ='00'
m.numcp=SPACE(4)
vcc=SPACE(14)
*ACTIVATE WINDOW Standby
@ 06,16 GET m.nummescp DEFAULT PADL(MONTH(DATE()),2,'0')
@ 06,18 say '.'
@ 06,19 GET m.numcp DEFAULT SPACE(4)
@ 07,16 GET vcc FUNCTION '!' VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",vcc,1,2,24)
READ
SELE COMPAG
*deactivate WINDOW standby
IF LASTKEY()=27
   SET FILTER TO
   RETURN .F.
ENDIF

m.nummescp = PADL(ALLTRIM(m.nummescp),2,'0')
m.numcp = PADL(ALLTRIM(m.numcp),4,'0')
m.codctc=ALLT(VCC)
IF !SEEK(alltrim(m.nummescp)+m.numcp+alltrim(m.codctc))
  GO BOTTOM
  ON KEY LABEL F10 KEYBOARD CHR(23)
  DEFINE WINDOW EliHC FROM 1,1 TO 18,79 TITLE " Elija el C/P con F10 "
  BROWSE NOED WINDOW EliHC COLOR SCHEME 10 FIELDS ;
     numcp :H="C/P",;
     nummes :H="Mes",;
     tipdoc,;
     tipprv,;
     codemp :H="CodEmp",;
     x2=TRANSF(import,'99,999,999.99'):H="Monto",;
     fecCP :H="fecha",;
    x3=codcal+' '+codctc :H="Cal.CTC"
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

*	IF newsistem='1'
*		=val_codcad(ALLTRIM(m.codcal),m.periodo+'01001','C')
*	ELSE
*			=val_codca1(ALLTRIM(m.codcal),m.periodo,'C')
*	ENDIF
*m.codprg   = maepre.codprg
*m.codsubpr = maepre.codspr
*m.tippre  = IIF(tipfun='F','DE OPERACION','DE INVERSION')
	SHOW GETS
	SELECT COMPAG
	SET FILT TO 
	SELECT fonp
*	 @  6,16 SAY m.nummescp+'.'+m.numcp COLOR SCHEME 02
*	 @  7,16 SAY m.codctc COLOR SCHEME 02
ENDIF
RETURN .T.


FUNCTION REMBYS
*----------------
vAli=ALIAS()
vOrd=ORDER()
vSubCta=LEFT(Fonp.CodSCta,1)
SELECT Parma
RETURN


*FUNCTION Descri
*--------------
PUBLIC aa
*aa = IIF(!EMPTY(CODPART),VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'D'),' ')
RETURN aa

function val_num
*-----------------
PARAMETERS m.numfp,m.nummes
SELE fonp
SEEK ALLTRIM(m.numfp)+alltrim(m.nummes)
IF FOUND()
   WAIT "Error, N£mero de Documento ya existe" WINDOW
*   SET ORDER TO MovBco11
   RETURN .F.
ELSE
*   SET ORDER TO MovBco11
   RETURN .T.	    	   
ENDIF    

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
SET ORDER TO FonPag1
*SET ORDER TO FonPag2
IF !BOF()
	SKIP -1
	IF fonp.codctc=m.codctc
	   m.salini=fonp.salini+fonp.reembols-fonp.moneje
	ENDIF
	SKIP
ENDIF	
SET ORDER TO FonPag1
RETURN   

PROCEDURE Cajafp
*---------------
vfun = .T.
m.Periodo=RIGHT(DTOC(DATE()),2)
OK = FOUND()
SELECT ComPag
SET FILT TO ESTADO<>'99' AND PRES<>'*' AND estfon<>'û' AND DOCREF="RNF"
*SET FILT TO flagcon='F' AND ESTADO<>'99' AND PRES<>'*' AND estfon<>'û'
GO TOP
IF EOF()
	DO standby WITH 'No existe Comprobantes de Pago'
	SET FILT TO
	RETURN .F.
ENDIF

DEFINE WINDOW WBusca FROM 10,12 TO 14,68 DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW WBusca

STORE SPACE(04) TO VCP
STORE SPACE(02) TO VMES
STORE SPACE(17) TO VCTA
vMes ='00'
vCP  ='    '

@ 0, 02  SAY 'N§ C/P :' GET VMES PICT '!!'
@ 0, 14  SAY '.'
@ 0, 15  GET VCP PICTURE '!!!!' 
@ 1, 02  SAY 'Cta. Cte.:' GET VCTA FUNCTION '!' VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",vCTA,1,2,24)
READ

SELECT CompaG
SET ORDER TO Compag1
GO TOP
RELEASE WINDOW WBusca

M.NUMMESCP = PADL(ALLTRIM(vMes),2,'0')
M.NUMCP    = PADL(ALLTRIM(vCP),4,'0')
M.CODCTC   = ALLTRIM(vCTA)

*IF !SEEK(vMes+vCP+ALLTRIM(vCTA))
IF !SEEK(M.NUMMESCP+M.NUMCP+ALLTRIM(M.CODCTC))
	*SEEK vMes+vCP+ALLTRIM(vCTA) 
	*IF !FOUND()
	GO BOTTOM
	** Cambio de POPUP con BROWSE
	ON KEY LABEL F10 KEYBOARD CHR(23)
	DEFINE WINDOW EliHC FROM 1,1 TO 18,79 TITLE " Elija el C/P con F10 "
	BROWSE NOEDIT WINDOW EliHC COLOR SCHEME 10 FIELDS ;
		numcp :H="C/P",;
		nummes:H="Mes",;
		FecCp :H='Fecha',;
		Import:H="Monto",;
		codCtc:H="Cta.Cte"
ENDIF
IF LASTKEY()#27
	SELE Compag
	m.nummeshc = COMPAG.nummeshc
	m.fecha    = COMPAG.feccp
	m.numhc    = COMPAG.numhc
	m.nummes   = COMPAG.nummes
	m.nummesCP = COMPAG.nummes
	m.CodCad   = COMPAG.codcad
	m.Reembols = COMPAG.import
	m.numcp    = COMPAG.numcp
	m.codctc   = COMPAG.codctc
ENDIF

SHOW GETS
SELECT COMPAG
SET FILT TO
SELECT fonp

@ 00,55 SAY m.nummeshc+'.'+m.numhc
@ 01,55 SAY m.NumMes
@ 02,18 SAY Val_CodCa1(m.codcad,m.periodo,' ',18,47)
@ 06,18 SAY m.nummescp+'.'+m.numcp COLOR SCHEME 02
@ 07,18 SAY m.codctc COLOR SCHEME 02

* SELECT Compag
* SET ORDER TO (vvord)
SELECT FonP

RETURN .T.

FUNCTION TotImp
*--------------
PARAMETERS cPartida,cCla
ntotal = 0
nReg = RECNO()
SCAN WHILE CodPart=cPartida AND CodCla = cCla
	ntotal = ntotal + Import
ENDSCAN
GO nreg
RETURN ntotal

FUNCTION TotIGV
*--------------
PARAMETERS cPartida,cCla
ntotal = 0
nReg = RECNO()
SCAN WHILE CodPart=cPartida AND CodCla = cCla
	ntotal = ntotal + TotIGV
ENDSCAN
GO nreg
RETURN ntotal

PROCEDURE SubOpc
*---------------
PRIVATE cAlias,cMod
cAlias = ALIAS()
SELE SubOp
cCtrlOp = ''
cMod = "04"

SEEK vUsuCla+PADL(SistCtrl,2,'0')+cMod

IF FOUND()
	SCAN WHILE vUsuCla+PADL(SistCtrl,2,'0')+cMod = ALLTRIM(SubOp.User)+SubOp.Sistema+SubOp.Modulo
		cCtrlOp = cCtrlOp + SubOp.Opcion
	ENDSCAN
ENDIF

set skip of PAD Revis of mMenu !'A' $cCtrlOp
set skip of PAD Busca of mMenu !'B' $cCtrlOp
set skip of PAD Anter of mMenu !'C' $cCtrlOp
set skip of PAD Proxi of mMenu !'D' $cCtrlOp
set skip of PAD Corri of mMenu !'E' $cCtrlOp
set skip of PAD Ingre of mMenu !'F' $cCtrlOp
set skip of PAD Elimi of mMenu !'G' $cCtrlOp
set skip of PAD Lista of mMenu !'H' $cCtrlOp

SELE (cAlias)

RETURN

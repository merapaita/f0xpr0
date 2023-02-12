* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RegCp.Prg  19/11/96                              L: 3935     ³	
* ³ Registro de Comprobantes de Pago                             ³
* ³ AUTORES : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ³                Julio Cruz Ortiz  (Pago de Retenciones)       ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.     05/1997 ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾
PARAMETERS  key_docfte

IF PARAMETERS()=0
   key_docfte = ''
ENDIF

CLOSE DATA
USE ParMae   IN  1  ORDER TAG ParMae1      ALIAS ParMa 
USE ComPag   IN  3  ORDER TAG ComPag1      ALIAS ComPag 
USE IteCp    IN  4  ORDER TAG IteCp1       ALIAS IteCp  
USE HojCon   IN  6  ORDER TAG HojCon1      ALIAS Hoja   
USE IteHc    IN  7  ORDER TAG IteHc1       ALIAS IteHc 
USE Cajas    IN 10  ORDER TAG Cajas2       ALIAS Caja
USE Cheque   IN 11  ORDER TAG Cheque1      ALIAS Cheque
USE Auxil    IN 15  ORDER TAG Auxil1       ALIAS Auxil
USE MaePre   IN 16  ORDER tag MaePre1      ALIAS MaePre
USE ItePar   IN 25  ORDER tag ItePar1      ALIAS ItePar          

SELECT ComPag
GO BOTTOM


*- Mensajes de aviso al usuario

vmens01 = ' Comprobantes de Pago : REVISION '
vmens02 = '°   ®F3¯ Visualiza Retenciones    ®F4¯Imprime C/P °'
vmens04 = 'Dicho C/P no fue encontrado'
vmens05 = 'No existe C/P anterior'
vmens06 = 'No existe C/P siguiente'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este C/P ha sido anulado'
vmens13 = ' °  Retenciones  ° '

SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Variables de trabajo (registro a trabajar)

PUBLIC vFecha,vKey,mmonto,vpar,vComPag,zz,yy,lseek,x_partret,Vrec,Wmtoret,wchq,ztotal,vref,vref1,wuser_id,Vglosa2,vsector

PUBLIC vNumMes,vCodCtc,vCta,vdes,w_tipctc,vret,w_ctad,w_part,w_ctah,vtitulo,vncheque,vimport,vvpartret,vcadena,docbusc

* vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID = CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
STORE SPACE(14)  TO vcodctc,vncheque
STORE SPACE(18)  TO wchq
STORE SPACE(5)   TO w_part,yy
STORE SPACE(11)  TO w_ctad,w_ctah
STORE 0          TO mmonto,vmondeb,vret
STORE SPACE(20)  TO zz,Vref,vref1
STORE .T.        TO vflag
STORE SPACE(1)   TO m.prestamo
STORE DATE()     TO vfecha
STORE '  '       TO vnummes,vKey
STORE SPACE(180)  TO vglosa2

DocBusc = ''
IF !empty(key_docfte)
   DocBusc = key_docfte
ENDIF

IF !EMPTY(DocBusc)
   SEEK DocBusc
   IF !FOUND()
      DO STANDBY WITH 'El C/P no fue Encontrado!'
      GO BOTTOM
   ENDIF
ENDIF   


*- Inicia proceso
DO inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO pantalla                  && Muestra pantalla inicial
DO vista
*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO ven_accion
DO WHILE ven_accion
	ACTIVATE SCREEN
	ACTIVATE MENU mMenu1
ENDDO
DO fin_opcion
RETURN


PROCEDURE inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular   Listar  Termina '
DO logos WITH rotulo1,vtempo

DEFINE WINDOW wind_0 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10 

DEFINE WINDOW wind_1 FROM 00,00 TO 13,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 14,00 TO 23,79 DOUBLE ;
	TITLE '®F9¯: Corrige Asientos    Comprobante de Pago : Detalle   ' COLOR SCHEME 10

DEFINE WINDOW wind_3 FROM 20,64 TO 22,78 ;
	TITLE ' TOTAL ' COLOR SCHEME 10

DEFINE WINDOW wind_10 FROM 20,64 TO 22,78 ;
	TITLE ' TOTAL ' COLOR SCHEME 10

DEFINE WINDOW wind_4 FROM 08,01 TO 15,78 DOUBLE ;
	TITLE '** GIRO DE Cheque->  ®F5¯ Agregar   ®F8¯ Eliminar   ®F10¯ Terminar **' ;
	COLOR SCHEME 10

DEFINE WINDOW WIND_5 FROM 08,02 TO 15,77 DOUBLE ;
	TITLE ' Retenciones    ®F10¯ Continua ' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_6 FROM 13,10 TO 17,70 ;
	TITLE ' COMPROMISO PRESUPUESTAL ' COLOR SCHEME  15

DEFINE WINDOW wind_7 FROM 13,10 TO 17,70 ;
	TITLE ' CENTRALIZACION DE Caja ' COLOR SCHEME 10

DEFINE WINDOW Wind_8 FROM 14,01 TO 16,79 ;
 TITLE ' Destino ' 

DEFINE WINDOW wind_9 FROM 05,00 TO 23,79 DOUBLE ;
	TITLE vmens13 COLOR SCHEME 2

DEFINE WINDOW Wind_11 FROM 16,01 TO 18,79 ;
	 TITLE ' Pr‚stamos ' COLOR SCHEME 5
	 
DEFINE WINDOW Wind_12 FROM 14,11 TO 16,69 ;
TITLE ' Retenci¢n ' 

DEFINE WINDOW Wind_13 FROM 13,10 TO 17,70  ;
    TITLE 'Sectores'  COLOR SCHEME 10

DEFINE MENU mMenu1 COLOR SCHEME 3
DEFINE PAD revis   OF mMenu1 PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF mMenu1 PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF mMenu1 PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF mMenu1 PROMPT '\<Siguiente'  AT 24,25
DEFINE PAD corri   OF mMenu1 PROMPT '\Corrige'    AT 24,36
DEFINE PAD ingre   OF mMenu1 PROMPT '\Ingresa'    AT 24,45
DEFINE PAD anula   OF mMenu1 PROMPT '\aNular '    AT 24,54
DEFINE PAD lista   OF mMenu1 PROMPT '\<Listar '    AT 24,63
DEFINE PAD termi   OF mMenu1 PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mMenu1 DO revis
ON SELECTION PAD busca  OF mMenu1 DO busca
ON SELECTION PAD anter  OF mMenu1 DO anter
ON SELECTION PAD proxi  OF mMenu1 DO proxi
ON SELECTION PAD lista  OF mMenu1 DO lista
ON SELECTION PAD termi  OF mMenu1 DO termi
ON KEY LABEL F2 DO gircheq
ON KEY LABEL F3 DO regret
ON KEY LABEL F4 DO imp_cp
ON KEY LABEL F9 DO cor_as
RETURN


PROCEDURE pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW wind_1
CLEAR
@ 00,02 SAY "         Pr‚stamo :" 
@ 00,40 SAY "Incidencia Presup.:" 
@ 01,02 SAY "           N§ C/P :"
@ 01,40 SAY "       Fecha  C/P :"
@ 02,02 SAY " Cuenta Corriente :"
@ 03,02 SAY "        Proveedor :"
@ 04,02 SAY "  Doc. Referencia :"
@ 04,48 SAY " Fecha Digitaci¢n :"
@ 05,02 SAY "         Fte.Fto. :"
@ 06,02 SAY " Cadena Funcional :"
@ 06,27 SAY " UG  UE  FN PRG SBPRG ACTPRY"
@ 08,02 SAY "            Glosa :"
@ 09,02 SAY "    Observaciones :"
@ 11,00 SAY PADC(ALLTRIM(vmens02),77,' ') COLOR W+/B
RETURN


PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT ComPag
SET RELATION TO NumMesHC+NumHC 		INTO Hoja
SET RELATION TO NumMes+NumCP+Codctc INTO Cheque  ADDI
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
=Val_Codc1(ALLTRIM(m.codcad),m.periodo+'01001','C')
ON KEY LABEL F9 DO cor_as
@  0,0  SAY SPACE(80)
@  0,45 SAY 'Estado     :'
**
@  0,60 SAY IIF(m.pres='*','  Con H/A  ',IIF(M.tipdoc='IN','Inutilizado',IIF(Estado='99','  Anulado  ','     OK û   '))) COLOR SCHEME 2
@  1,22 SAY m.numcp
@  1,26 SAY '.'
@  1,27 SAY m.nummes
@  1,60 SAY m.feccp
@  2,22 SAY m.codctc
@  2,60 SAY SPACE(18)
@  2,60 SAY Verestch(Cheque.estado) COLOR SCHEME 2
SET RELA TO

DO CASE

	CASE m.TipDoc$'RESRRURS'
    	@ 0,01 SAY IIF(m.tipdoc='RE','Retenci¢n',IIF(m.tipdoc='SR','Ret.Sector',IIF(m.tipdoc='RU','Ret.Subsid','Ret.Sec.Sub'))) COLOR SCHEME 05
	    @ 3,22 SAY VAL_PARA(m.codret,'CODRET','V',22,40)

	CASE m.TipDoc$'MESUHCSS'
	    @ 0,01 SAY IIF(m.tipdoc='ME','Memorandum',IIF(m.tipdoc='HC','Hoja Control',IIF(m.tipdoc='SU','Subsidios','Sect.Sub'))) COLOR SCHEME 05
	    @ 3,22 SAY SPACE(60)
	    =pres_pro()
       	
	CASE m.TipDoc$'SESH'  
	    @ 0,01 SAY 'Sector.H/C' COLOR SCHEME 05
	    @ 3,22 SAY SPACE(60)
	    =pres_pro()
		
	CASE m.TipDoc$'HMRGIN'
	    @ 0,01 SAY IIF(m.tipdoc='HM','Hoja Mod.',IIF(m.tipdoc='RG','Regularizac. H/C','Inutilizado')) COLOR SCHEME 05
	    @ 3,22 SAY SPACE(60)
    	@ 3,22 SAY m.nompre     

ENDCASE

DO CASE

   CASE m.tipdoc$'HCRG'
		@  4,22 SAY m.numhc+'.'+m.nummeshc+' H/C'
		
   CASE m.tipdoc='HM'
		@  4,22 SAY m.numhm+'.'+m.nummeshm+' H/M'
		
   OTHER	
		@  4,22 SAY m.docref
		@  4,26 SAY m.numref
		@  4,34 SAY m.refer
		
ENDCASE

IF  m.tipdoc#'IN'
    @  4,68 SAY m.fecref 
	@  5,22 SAY val_para(m.codfte,'CODFTE','V',26,50) 
 	@  6,22 SAY m.codcad
    DO vis_calen
	@ 08,22 SAY m.glosa PICTURE '@S56' 
	@ 09,22 SAY SUBSTR(m.observ,1,56)
	IF m.Reten>0
	   @10,02 SAY '        Retenci¢n=>' COLOR SCHEME 05
	   @10,22 SAY m.Reten
	ELSE
	   @10,00 SAY SPACE(79)
	ENDIF      
	DO vista_hijo
	DO Total
ENDIF	
RETURN


PROCEDURE vista_hijo
*-------------------
HIDE POPUP ALL
SELECT IteCp
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
IF FOUND()
  IF m.codcad<>'0000'
	BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT  KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)  TIMEOUT 0.001 ; 
	WINDOW wind_2 ;
	FIELDS ;
        CodCom     :H= 'Componente',;
        CodMet     :H= 'Meta',;
		CodPart    :H= 'Partida' ,;
		aa = IIF(EMPTY(CodPart),' ',val_para(right(codpart,2),'ESPGAS','D',28,30)) :H='Descripci¢n':38,;
		impparc    :H= 'Monto'   :F :p='9,999,999.99' 
  ELSE
	BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT  KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)  TIMEOUT 0.001 ; 
	WINDOW wind_2 ;
	FIELDS ;
		CodPart    :H= 'Partida'  ,;
		aa = IIF(EMPTY(CodPart),' ',val_para(right(codpart,2),'ESPGAS','D',28,30)) :H='Descripci¢n':38,;
		impparc    :H= 'Monto'   :F :p='9,999,999.99' 
  ENDIF
ELSE
	DO standby WITH 'No tiene detalle'
ENDIF
SELE ComPag
SET RELATION OFF INTO Hoja
RETURN


PROCEDURE Total
*--------------
ACTIVATE WINDOW wind_3
@ 0,1 SAY m.import PICTURE '9,999,999.99'
RETURN


PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE ComPag
SET RELATION TO nummes+numcp+codctc  INTO Cheque ADDI
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mMenu1
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	x1=numcp+'.'+nummes  :H=' N§ CP' ,;
	feccp,;
	codctc :H='CtaCte',;
	tipprv,;
	X4=muespro(compag.tipprv,compag.codotr,compag.tipdoc) :H="Proveedor" ,;
	codfte :H='Fte.Fin.',;
	import :H='Total CP',;
	x3=NumHC+'.'+NumMesHC :H=' N§ H/C',;
	x2=NumHM+'.'+NumMesHM :H=' N§ H/M',;
	codcad :H='Cad. Func.',;
	Estado :H='Estado'
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
   GOTO vtemp
ENDIF
SHOW MENU mMenu1
ON KEY LABEL f10
SELE ComPag
SET RELATION OFF INTO Cheque
DO vista
RETURN


PROCEDURE busca              
*--------------
vtemp=RECNO()
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
STORE SPACE(4) TO VComPag
ACTIVATE WINDOW standby
@ 0,01 SAY '          Ingrese Mes: ' GET vnummes PICTURE '!!' DEFAULT SPACE(2)
@ 1,01 SAY 'Ingrese N£mero ComPag: ' GET vComPag PICTURE '@!' DEFAULT SPACE(4)
@ 2,01 SAY '       Ingrese CtaCte: ' GET vcodctc FUNCTION '!' VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",vcodctc,1,2,24)
READ
DEACTIVATE WINDOW standby
IF EMPTY(vComPag) .OR. LASTKEY()=27
	RETURN
ELSE
	vnummes = PADL(ALLTRIM(vnummes),2,'0')
	vComPag = PADL(ALLTRIM(vComPag),4,'0')
	SEEK vnummes+vComPag+ALLTRIM(vcodctc)
	IF !FOUND()
		DO standby WITH vmens04
		GOTO vtemp
	ELSE
		DO vista
	ENDIF
ENDIF
RETURN


PROCEDURE anter
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !BOF()
	SKIP -1
ENDIF
IF BOF()
	GO TOP
	DO standby WITH vmens05
ELSE
	DO vista
ENDIF
RETURN


PROCEDURE proxi
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !EOF()
	SKIP
ENDIF
IF EOF()
	DO standby WITH vmens06
	GO BOTTOM
ELSE
	DO vista
ENDIF
RETURN

PROCEDURE carhm
*--------------
* Carga valores de HM
vfun = .T.
OK   = FOUND()
SELECT hojm
SET FILTER TO Operac$'TC' AND Estado='20'
GO TOP
IF EOF()
	DO standby WITH 'No existe documentos H/M '
	SET FILT TO
	RETURN .F.
ENDIF
* Trata de ingresar directamente
vMes ='00'
vHm  ='    '
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N§ H/M: " GET vMes DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 SAY '.'
@ 1,17 GET vHm DEFAULT SPACE(4)
READ
deactivate WINDOW standby
IF LASTKEY()=27
   SET FILT TO
   RETURN .F.
ENDIF
vMes = PADL(ALLTRIM(vMes),2,'0')
vHm  = PADL(ALLTRIM(vHm),4,'0')
IF !SEEK(ALLTRIM(vMes)+vHm)
  GO BOTTOM
** Cambio de POPUP con BROWSE
   ON KEY LABEL F10 KEYBOARD CHR(23)
   DEFINE WINDOW EliHC FROM 1,1 TO 18,79 TITLE " Elija la HM con F10 "
   BROWSE NOED WINDOW EliHC COLOR SCHEME 10 FIELDS ;
     numhm :H="HM",;
     nummes:H="Mes",;
     tipprv,;
     codprv :H="CodPro",;
     codemp :H="CodEmp",;
     x1=IIF(TIPPRV="O" AND EMPTY(codotr),NOMBRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),IIF(tipprv="E",LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codotr), 20)))) :H="Nombre",;
     x2=TRANSF(imptot,'99,999,999.99'):H="Monto",;
     fechm :H="fecha",;
     x3=codcal+' '+codctc :H="Cal.ctc"
ENDIF
IF LASTKEY()#27
    SELE Hojm
	m.tipfun   = tipfun
	m.nummeshm = nummes
	m.numhm    = numhm
	m.nummes   = nummes
	m.nummeshc = nummeshc
	m.numhc    = numhc
	m.import   = imptot
	m.codprv   = codprv
	m.codemp   = codemp
	m.codotr   = codotr
	m.codctc   = codctc
	m.CodPart  = CodPart
	m.tipdoc   = tipdoc
	m.tipprv   = tipprv
	m.periodo  = periodo
	m.nompre   = nombre
	m.destino  = Destino
	m.codfte   = IIF(codfte='TRN','PRP','TRN')
    m.CodCal   = STUFF(codcal, 5, 3, m.codfte)	
	m.numref   = Numhm+"."+Nummes
	SELE Hojm
	SHOW GETS
	SELECT Caja
	SEEK ALLTRIM(m.codctc)
	IF !FOUND()
		DO standby WITH 'No se tiene inscrito la cuenta ' + m.codctc
	ELSE
		m.numcp =  PADL(corcp+1,4,'0')
		w_ctah  =  Caja.CuentaH
	ENDIF
	SELECT hojm
	SET FILT TO
	SELECT ComPag
ENDIF
RETURN .T.


PROCEDURE carSHC
*---------------
PARAMETERS vsec
* Carga valores de HC Sectorial
vfun = .T.
OK   = FOUND()

SELECT hoja
vvord= ORDER()

DO CASE
   CASE vsec='A' 	
   		SET ORDER TO Hojcon6
   CASE vsec='E' 	
   		SET ORDER TO Hojcon7
   CASE vsec='I' 	
   		SET ORDER TO Hojcon8
   CASE vsec='T'
   		SET ORDER TO Hojcon9
   CASE vsec='P' 	
   		SET ORDER TO Hojcon10
ENDCASE
   		
GO TOP
IF EOF()
	DO standby WITH 'No existe documentos H/C para sectores'
	SET ORDER TO (vvord)
	SELE compag
	RETURN .F.
ENDIF
* Trata de ingresar directamente
vMes ='00'
vHc  ='    '
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N§ H/C: " GET vMes DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 SAY '.'
@ 1,17 GET vHC DEFAULT SPACE(4)
READ
DEACTIVATE WINDOW standby
IF LASTKEY()=27
   SET ORDER TO (vvord)
   SELE compag
   RETURN .F.
ENDIF
vMes = PADL(ALLTRIM(vMes),2,'0')
vHC  = PADL(ALLTRIM(vHC),4,'0')
IF !SEEK(ALLTRIM(vMes)+vHC)
  GO BOTTOM
** Cambio de POPUP con BROWSE
   ON KEY LABEL F10 KEYBOARD CHR(23)
   DEFINE WINDOW EliHC FROM 1,1 TO 18,79 TITLE " Elija la HC con F10 "
   BROWSE NOEDIT WINDOW EliHC COLOR SCHEME 10 FIELDS ;
     numhc :H="HC",;
     nummes:H="Mes",;
     tipprv,;
     codprv :H="CodPro",;
     codemp :H="CodEmp",;
     x1=IIF(TIPPRV="O" AND EMPTY(codotr),NOMBRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),IIF(tipprv="E",LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codotr), 20)))) :H="Nombre",;
     x2=TRANSF(imptot,'99,999,999.99'):H="Monto",;
     fechc :H="Fecha",;
     x3=codcad+' '+codctc :H="Cad.ctc"

ENDIF
IF LASTKEY()#27
    SELE HOJA
	m.nummes   = nummes
	m.nummeshc = nummes
	m.numhc    = numhc
	m.import   = imptot
	m.codprv   = codprv
	m.codemp   = codemp
	m.codotr   = codotr
	m.CodCad   = codcad
*	m.codctc   = codctc
	m.CodPart  = CodPart
	m.tipdoc   = tipdoc
	m.nombre   = nompre
	m.tipprv   = tipprv
	m.periodo  = periodo
	m.nompre   = nombre
	m.destino  = Destino
	m.codobra  = codobra
	m.codfte   = codfte
	m.numref   = Numhc+"."+Nummes
	m.partret  = partret
**

	SHOW GETS
	SET ORDER TO (vvord)
	SELECT ComPag
ENDIF
RETURN .T.


PROCEDURE IngFec
*---------------
m.periodo = RIGHT(DTOC(m.feccp),2)
RETURN .T.


PROCEDURE busCaja
*----------------
SELECT Caja
SEEK m.codctc
IF !FOUND()
	DO standby WITH 'No se tiene inscrita la cuenta ' + m.codctc
ELSE
	m.numcp  =  PADL(corcp+1,4,'0')
    *m.tipfun =  IIF(Caja.Tipfun#'A' AND !EMPTY(Caja.tipfun),Caja.Tipfun,' ')
	*m.codfte =  ALLT(Caja.codfte)   
	w_ctah   =  Caja.CuentaH
ENDIF
RETURN


PROCEDURE busCaja1
*-----------------
SELECT Caja
SEEK m.codctc
IF !FOUND()
	DO standby WITH 'No se tiene inscrita la cuenta ' + m.codctc
ELSE
    *m.tipfun =  IIF(Caja.Tipfun#'A' AND !EMPTY(Caja.tipfun),Caja.Tipfun,' ')
	*m.codfte =  ALLT(Caja.codfte)   
	w_ctah   =  Caja.CuentaH
ENDIF
RETURN


PROCEDURE valfecha
*-----------------
m.feccp = DATE()
RETURN .T.

PROCEDURE vis_cen &&Visualiza centralizaci¢n de Caja
*-----------------
ACTIVATE WINDOW wind_7
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY 'Debe '
@ 00,34  SAY 'Haber '
@ 01,04  SAY '10101010100'
@ 01,18  SAY mmonto PICTURE '999,999,999.99' 
@ 02,12  SAY '10101010100'
@ 02,34  SAY mmonto PICTURE '999,999,999.99' 
DO standby WITH 'Visualizando los Movimientos'
DEACTIVATE WINDOW wind_7
RETURN


FUNCTION Asig
*------------
vMonDeb=mtodeb
RETURN

PROCEDURE verap   && F5
*--------------
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+m.codctc
IF !FOUND()
	DO standby WITH 'No tiene detalle'
ELSE
	BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY ALLTRIM(m.nummes)+m.numcp   TIMEOUT 0.001 ;
		WINDOW wind_2 ;
		FIELDS;
		codcta :H='CtaCte',;
		tipcta :H='Tp',;
		mtodeb :H='Monto Debe' :p='999,999,999.99',;
		mtohab :H='Monto Haber'  :p='999,999,999.99',;
		ret    :H='Ret?' :p='!'
ENDIF
USE IN 09
SELECT ComPag
RETURN

PROCEDURE lista
*--------------
USE IN  6 
USE IN  7
USE IN  8
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
SELECT ComPag
VRECNO=RECNO()
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
DEFINE WINDOW lis FROM 3,10 TO 23,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1  TO vtocta,vlista,vesta,Vsino
STORE SPACE(14) TO vCta,vCuenta
STORE SPACE(2)  TO vAno,vMes
STORE SPACE(4)  TO vCli,Vinicp,vfincp
STORE SPACE(212) TO vobs
vcta=m.codctc
Vano=m.nummes
Vcli=m.numcp
STORE DATE() TO vfecini, vfecfin
*@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento;Resumen x Cta.Cte;Pr‚stamos;Anticipos'
@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento;Resumen x Cta.Cte'
@ 05,01 SAY "     N§ Documento : "
@ 05,22 GET vano    WHEN vlista=1  PICTURE '!!'
@ 05,25 GET vcli    WHEN vlista=1  PICTURE '!!!!' VALID val_CP()
@ 07,01 SAY "   Cta. Corriente : "
@ 07,22 GET vcta    VALID val_fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,08,07)
@ 09,01 SAY "           Estado : "  GET vesta   FUNCTION '*RNH Pendientes;General' WHEN vlista=3 
@ 11,01 SAY "          Destino :  " GET vsino   FUNCTION '*RNH Presidencia;Cajero/Pagador' WHEN vlista=2
@ 13,01 SAY " Fecha / Correlat.: "
@ 13,22 GET vinicp   PICTURE '!!!!' COLOR SCHEME 10 WHEN vsino=2
@ 13,32 GET vfincp   PICTURE '!!!!' COLOR SCHEME 10 WHEN vsino=2
@ 13,22 GET vfecini  PICTURE '@D'   COLOR SCHEME 7 WHEN (vlista=2 AND vsino=1) OR VLISTA=3 OR VLISTA=4
@ 13,32 GET vfecfin  PICTURE '@D'   COLOR SCHEME 7 VALID (vfecfin >= vfecini)  WHEN (vlista=2 AND vsino=1) OR VLISTA=3 OR VLISTA=4
@ 15,01 SAY "    Observaciones : "  GET vobs     PICTURE '@S28' WHEN vlista=2 FUNCTION "!"
@ 17,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
xind=SYS(3)+".IDX"
DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
	DO CASE 
	   CASE vLista = 1
		    vkey=ALLTRIM(VANO)+VCLI+ALLTRIM(VCTA)
    		SELE ComPag
    		SEEK vkey
    		DO repPRG WITH "LisCP"," Listado Comprobante de Pago ",2
	   CASE vLista = 2
 		    ACTIVATE WINDOW Xwait
		    @0,0 SAY " Procesando reporte...." COLOR W+/BR*
		    SELE ComPag
			SET RELATION TO nummes+numcp+codctc  INTO Cheque ADDI
			IF vsino=1
				INDEX ON numcp TO (xind) FOR codctc=vcta AND BETWEEN(ComPag.fecref,vfecini,vfecfin) 
			ELSE	
				INDEX ON numcp TO (xind) FOR codctc=vcta AND BETWEEN(ComPag.numcp,vinicp,vfincp) 
			ENDIF	
			DEACTIVATE WINDOW xwait
            DO reporte WITH 2,"LisCpCta"," Resumen de Comprobantes de Pago Registradas por Cta.Cte. ",2,.F.,.T.                                  	       
	        SELE ComPag
	        SET RELA OFF INTO Cheque
	        SET INDEX TO
   	        SELE ComPag
   	        SET ORDER TO ComPag1
	  CASE vLista = 3
		    ACTIVATE WINDOW Xwait
		    @0,0 SAY " Procesando reporte...." COLOR W+/BR*
			USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
			SELE Astpat
			SELECT ComPag
			vrec1=RECNO()
			vord1=ORDER()
			IF vesta=1
			   INDEX ON NUMMES+NUMCP+CODCTC TO (XIND) FOR codctc=vcta AND BETWEEN(fecCP,vfecini,vfecfin) AND Prestamo='S' AND EMPTY(ConAAFF)
			ELSE   
			   INDEX ON NUMMES+NUMCP+CODCTC TO (XIND) FOR codctc=vcta AND BETWEEN(fecCP,vfecini,vfecfin)	AND Prestamo='S'
			ENDIF   
            SET RELATION TO nummes+numcp+codctc  INTO Astpat ADDI
   	        SET RELATION TO nummes+numcp+codctc  INTO Cheque ADDI
   	        DEACTIVATE WINDOW xwait
   	  	    IF EOF()
	           DO STANDBY WITH "Registros no existen"
			    SET INDEX TO
	     	    USE IN 9
	     	    SELE ComPag
    	 	    SET ORDER TO (vord1)
	     	    GO vrec1
	     	    RETURN
	     	 ENDIF   
	         Vtitulo = 'PRESTAMOS'
	         DO reporte WITH 2,"LisCpCon"," Resumen de Comprobantes de Pago de Prestamos por Cta.Cte. "
    	     USE IN 9
        	 SET INDEX TO
   	         SELE ComPag
   	         SET ORDER TO (vord1)
  	         GO vrec1
	  CASE vLista = 4
		    ACTIVATE WINDOW Xwait
		    @0,0 SAY " Procesando reporte...." COLOR W+/BR*
			USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
			SELECT astpat
			INDEX ON NUMMES+NUMREF+CODCTC TO (XIND) FOR codctc=vcta AND BETWEEN(fecHA,vfecini,vfecfin)	AND LEFT(CODCTA,3)='384' AND TIPDOC='C/P'
            SET RELATION TO nummes+numref+codctc  INTO ComPag ADDI
            SET RELATION TO nummes+numref+codctc  INTO Cheque ADDI
            DEACTIVATE WINDOW xwait
			      	IF EOF()
		              DO STANDBY WITH "Registros no existen"
		     	      SET INDEX TO
		     	      USE IN 9
		     	      SELE ComPag
		     	      GO BOTT
		     	      RETURN
		     	   ENDIF   
      	           VTITULO = 'ANTICIPOS'
                   DO reporte WITH 2,"LisCpCon"," Resumen de Comprobantes de Pago de Anticipos"                                  	       
                   SET INDEX TO
                   USE IN 9
    ENDCASE
	SELE ComPag
ENDIF
USE IN 9
USE IN 14
USE HojCon   IN  6  ORDER TAG HojCon1      ALIAS hoja   &&H/C
USE IteHc    IN  7  ORDER TAG IteHc1       ALIAS IteHc 
USE Clase    IN  8  ORDER TAG Clase1       ALIAS Clase   
SELECT ComPag
GO VRECNO
DO VISTA
RETURN


PROCEDURE val_cp             && Revisi¢n de BD en browse
*---------------
SELE ComPag
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
SEEK vano+vcli
IF !FOUND()
	vtemp = RECNO()
	HIDE MENU mMenu1
	ACTIVATE SCREEN
	vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	ON KEY LABEL f10 KEYBOARD CHR(23)
	BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	x1=numcp+'.'+nummes  :H=' N§ CP' ,;
	feccp,;
	codctc :H='CtaCte',;
	tipprv,;
    x4=IIF((TIPPRV="O" .OR. EMPTY(TIPPRV)),NOMPRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20))) :H="Nombre",;
	codfte :H='Fte.Fin.',;
	IMPORT :H='Total CP',;
	x3=NumHC+'.'+NumMesHC :H=' N§ H/C',;
	codcal :H='Calendario',;
	Estado :H='Estado'
	vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	SHOW MENU mMenu1
	IF LASTKEY()=27
		GOTO BOTT
	ENDIF
ENDIF
vano = nummes
vcli = numcp
vcta = codctc

ON KEY LABEL f10
SELE ComPag
RETURN


PROCEDURE LisCP  &&Programa Reporte usa repprg 
*--------------
PARAMETERS xcop
PRIVATE fila,xval,tval,vpar,lval,x_sw1,vref
STORE 0 TO X_sw1,xval,tval,fila
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELE ComPag
vcad=codcad
vper=periodo
vano=ALLTRIM(nummes)
vcli=numcp
vcta=codctc
Vkey=ComPag.Nummes+ComPag.Numcp+ComPag.Codctc
Vref=ComPag.refer
LVAL=0
@0,0 SAY CHR(18)
@1,3   SAY ALLTRIM(CIA)
@1,68  SAY "PAG:"
@1,76  SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "LisCp"
@2,68 SAY "FECHA:"
@2,76 SAY DATE()           
IF TIPDOC#'IN'
	@5,14 SAY CHR(14)
	@5,15 SAY "<< COMPROBANTE DE PAGO >>"
	@5,44 SAY CHR(27)+CHR(18)
	@07,03 SAY "      N§ C/P :"  
	@07,20 SAY NUMCP+'.'+NUMMES
	@07,68 SAY "Estado :"  
	@07,76 SAY IIF(Estado='00','Emitido',IIF(Estado='20','Girado',IIF(Estado='99','Anulado',IIF(Estado='50','Pagado',IIF(Estado='92','Inutilizado',IIF(Estado="10","Girado",' -  '))))))
	@08,03 SAY "   Fecha C/P :"  
	@08,20 SAY FECCP picture "@D"
	@09,03 SAY "         Son :"  
	@09,20 SAY LETRAS(IMPORT-RETEN,'SOLES')
	@10,03 SAY "Raz¢n Social :"  
	@10,20 SAY IIF(tipdoc$"RESRRURS",codret,IIF((TIPPRV='O' OR EMPTY(TIPPRV) OR TIPDOC='RG' OR tipdoc='HM')," ",IIF((TIPPRV='P'and tipdoc<>'RG' AND tipdoc<>'HM'),Codprv,IIF(TIPDOC<>'RG' AND tipdoc<>'HM',Codemp,' '))))
	@10,27 SAY IIF(tipdoc$"RESRRURS",VAL_PARA(Codret,'CODRET','D',28,40),;
	       IIF((tipprv='O' AND EMPTY(codotr) OR EMPTY(tipprv) OR tipdoc='RG' OR tipdoc='HM'),ComPag.nompre,;
	       IIF((tipprv='P' AND Tipdoc<>'RG' AND tipdoc<>'HM'),val_auxi(ALLT(codprv),'20','D',40),;
	       IIF(tipdoc<>'RG' AND tipdoc<>'HM' AND tipprv='E', VAL_AUXI(allt(Codemp),'30','D',40),;
	       IIF(tipdoc<>'RG' AND tipdoc<>'HM' AND tipprv='O', VAL_AUXI(allt(Codotr),'09','D',40),;
   	       IIF(tipprv='R' AND !EMPTY(codotr) AND tipdoc='ME',VAL_PARA(ALLTRIM(Codotr),'CODRET','D',28,40),' '))))))
	@11,03 SAY "  Referencia :"  
	@11,20 SAY docref
	@11,24 SAY numref+' '+refer
	@12,03 SAY "     Cta.Cte.:"  
	@12,20 SAY codctc
	@12,35 SAY val_fun('Caja','codctc','Descri',codctc)
	@13,03 SAY "     Fte.Fto.:"
	@13,20 SAY codfte
	*@13,25 SAY ALLTRIM(val_para(CODFTE,'CODFTE','D',26,50))+' - '+ALLTRIM(val_para(tipfun,'TIPFUN','D',26,50))
	@13,25 SAY ALLTRIM(val_para(CODFTE,'CODFTE','D',26,50))
	IF incpres='S'
		=Val_Codc1(ALLTRIM(vcad),vper+'01001','C')
		@15,04 SAY "Cadena Func.:   UG  UE  FN PRG SBPRG ACTPRY"
		@16,20 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
		
		
	ENDIF
	@18,08 SAY IIF(EMPTY(DESTINO),' ','Destino :')
	@18,19 SAY CHR(15)
	@18,22 SAY DESTINO
	@19,31 SAY CHR(18)
	@19,40 SAY "C O N C E P T O" 
	@20,01 SAY CHR(15)
	@20,03 SAY SUBSTR(GLOSA,1,130)
	@21,03 SAY SUBSTR(GLOSA,131,70)
	@22,32 SAY CHR(18)
	@22,40 SAY "OBSERVACIONES" 
	@23,01 SAY CHR(15)
	@23,03 SAY SUBSTR(OBSERV,1,100)
	@23,24 SAY CHR(18)
	@24,25 SAY "<< CONTABILIDAD PRESUPUESTAL >>"
	@25,10 SAY "Cuentas"
	@25,52 SAY "Debe"
	@25,72 SAY "Haber"
	@26,01 SAY CTADEB
	@26,13 SAY Val_Fun('Cuenta','Cuenta','SUBSTR(Descri,1,26)',substr(CtaDeb,1,3))
	@26,40 SAY VALDEB PICTURE '@Z 999,999,999,999.99'
	@27,01 SAY CTAHAB
	@27,13 SAY Val_Fun('Cuenta','Cuenta','SUBSTR(Descri,1,26)',substr(CtaHAB,1,3))
	@27,60 SAY VALHAB PICTURE '@Z 999,999,999,999.99'
	@29,20 SAY "<< ESTADISTICA DIARIA OBJETO DEL GASTO >>"
	@30,03 SAY "     Componente      Meta      Partida            Parcial               Total"
	SELE IteCp
	SEEK  vkey
	@31,06 SAY '    '+IteCp.CodCom+'         '+IteCp.CodMet   &&+'        '+ALLTRIM(IteCp.CodPart)
	@31,34 SAY ALLTRIM(IteCp.CodPart)
	FILA=31
	SCAN WHILE NUMMES=ALLTRIM(VANO) AND NUMCP=VCLI AND CODCTC=ALLTRIM(VCTA)
    	 IF fila>=32
        	IF IteCp.codpart <> vPar
           	   FILA=FILA-1 
		       @fila,62 SAY xval PICTURE '@Z 999,999,999,999.99'
		       FILA=FILA+1
	    	   xval=0
	        ENDIF
	     ENDIF           
	    IF fila>=32 
			@fila,06 SAY '    '+IteCp.CodCom+'         '+IteCp.CodMet   &&+'        '+ALLTRIM(IteCp.CodPart)
			@fila,34 SAY ALLTRIM(IteCp.CodPart)
		ENDIF
	     @FILA,40 SAY IteCp.IMPPARC PICTURE '@Z 999,999,999,999.99'	
	     vpar=IteCp.codpart
	     xval=xval+IteCp.impparc
	     tval=tval+IteCp.impparc
	     FILA=FILA+1
	ENDSCAN     
	SELE ComPag
	FILA=FILA-1
	@fila,62 SAY xval PICTURE '@Z 999,999,999,999.99'
	FILA=FILA+2
	@FILA,40 SAY "      TOTAL :"    
	@fila,62 SAY Tval PICTURE '@Z 999,999,999,999.99'
	FILA=FILA+1	
	@FILA,40 SAY "DEDUCCIONES :"    
	@FILA,62 SAY Reten  PICTURE '@Z 999,999,999,999.99'
	FILA=FILA+1
	@FILA,40 SAY "    LIQUIDO :"    
	@FILA,62 SAY IMPORT-reten  PICTURE '@Z 999,999,999,999.99'
	FILA=FILA+2
	@FILA,25 SAY "<< CONTABILIDAD PATRIMONIAL >>"
	FILA=FILA+1
	@FILA,08 SAY "Cuenta D"
	@FILA,32 SAY "Importe"
	@FILA,46 SAY "Cuenta H"
	@FILA,72 SAY "Importe"
	FILA=FILA+1
	SELE ASTPAT
	SEEK vkey
	SCAN WHILE ASTPAT.NUMMES=ALLTRIM(VANO) AND ASTPAT.NUMREF=VCLI AND ASTPAT.CODCTC=ALLTRIM(VCTA)  
	IF ASTPAT.RET='N'	
		IF ASTPAT.TIPCTA="D" 
		   @FILA,06 SAY ASTPAT.CODCTA
	       @FILA,20 SAY IIF(LEFT(ALLT(ASTPAT.CODCTA),2)='10',ASTPAT.MTODEB,ASTPAT.MTODEB-ComPag.RETEN)  PICTURE "@z 999,999,999,999.99"             
	   	ELSE
	   	   @FILA,44 SAY ASTPAT.CODCTA
	   	   @FILA,60 SAY ASTPAT.MTOHAB  PICTURE "@z 999,999,999,999.99"             
	   ENDIF
	   FILA=FILA+1
   ENDIF	   
   ENDSCAN

	FILA=FILA+1

	@FILA,3 SAY "Ú"
	@FILA,4 SAY REPLICATE("Ä",30)
	@FILA,34 SAY "¿" 
	@FILA,37 SAY "Ú"
	@FILA,38 SAY REPLICATE("Ä",40)
	@FILA,78 SAY "¿" 

	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,10 SAY " RECIBI CONFORME"
	@FILA,34 SAY "³"
	@FILA,37 SAY "³"
	@FILA,78 SAY "³"

	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,34 SAY "³"
	
	@FILA,37 SAY "³"
	@FILA,38 SAY " -----------------"
	@FILA,59 SAY " -----------------"
	@FILA,78 SAY "³"

	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,34 SAY "³"

	@FILA,37 SAY "³"
	@FILA,39 SAY "CONTROL INTERNO "
	@FILA,61 SAY "CAJERO GENERAL"
	@FILA,78 SAY "³"

	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,6 SAY " ------------------------"
	@FILA,34 SAY "³"

	@FILA,37 SAY "³"
	@FILA,78 SAY "³"

	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,34 SAY "³"
	
	@FILA,37 SAY "³"
	@FILA,78 SAY "³"

	FILA=FILA+1
    
	@FILA,3 SAY "³"
	@FILA,4 SAY " L.E. :"
	@FILA,34 SAY "³"
	
	@FILA,37 SAY "³"
	@FILA,38 SAY " -----------------"
	@FILA,59 SAY " -----------------"
	@FILA,78 SAY "³"


	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,4 SAY " FECHA :"
	@FILA,12  SAY "  /  /"         
	@FILA,34 SAY "³"

	@FILA,37 SAY "³"
	@FILA,39 SAY "VISACION CONTADOR"
	@FILA,61 SAY "AUTORIZACION"
	@FILA,78 SAY "³"

	FILA=FILA+1

	@FILA,3 SAY "À"
	@FILA,4 SAY REPLICATE("Ä",30) 
	@FILA,34 SAY "Ù"

	@FILA,37 SAY "À"
	@FILA,38 SAY REPLICATE("Ä",40)
	@FILA,78 SAY "Ù" 

	FILA=FILA+1
		
	@FILA,12 SAY "N§ Cheque :"     
	SELE Cheque
	SEEK vkey
	@FILA,24 SAY iif(empty(Cheque.numchq),' Sin Cheque',Cheque.numchq)
	SELE ComPag
	@FILA,40 SAY "LOGIN:"
	@FILA,50 SAY disp_usu()
    @fila+1,40 SAY "Fecha:"
    @fila+1,50 SAY Compag.FecDig
    @fila+2,40 SAY "Hora :"
    @fila+2,50 SAY Compag.HorDig
	*IF TIPDOC$'SRRURS' AND ESTADO#'99'
	IF TIPDOC$'RESRRURS' AND ESTADO#'99'
		USE reten  IN 12  ORDER TAG reten5  ALIAS reten
		X_SW1=1
	ELSE	
		USE reten  IN 12  ORDER TAG reten1  ALIAS reten
	ENDIF	
	SELECT ComPag
	vrec=RECNO()
	
	SELECT RETEN	
	SEEK vkey
	IF FOUND()	
		IF _DEST1=2
	    	EJECT
		ENDIF

		DO cab_ret
		DO CASE
		   CASE X_SW1=1 
		       YZ=1
				SCAN WHILE MESCPPG=VANO AND NUMCPPG=VCLI AND CODCTC=VCTA AND CONPAGO='û'
			    	 @FILA,03 SAY RETEN.NUMMES PICTURE '@J'
			    	 @FILA,05 SAY '.'
			    	 @FILA,06 SAY RETEN.NUMCP  PICTURE '@J'
			    	 @FILA,15 SAY dispglo(yZ) 
				     @FILA,53 SAY RETEN.VALRET PICTURE '@Z 999,999,999.99'
				     FILA=FILA+1
				     YZ=YZ+1
			    	 @FILA,15 SAY dispglo(YZ) 
				     FILA=FILA+1
				     YZ=YZ+1
			    	 @FILA,15 SAY dispglo(YZ) 
			    	 FILA=FILA+1
			    	 YZ=1
			    	 LVAL=LVAL+RETEN.VALRET
				ENDSCAN     
		   CASE X_SW1=0	
				SCAN WHILE NUMMES=VANO AND NUMCP=VCLI AND CODCTC=VCTA
		    		 @FILA,03 SAY RETEN.CODRET PICTURE '@J'
			    	 @FILA,15 SAY RETEN.NOMRET
				     @FILA,53 SAY RETEN.VALRET PICTURE '@Z 999,999,999.99'
				     FILA=FILA+1
			    	 LVAL=LVAL+RETEN.VALRET
				ENDSCAN     
		ENDCASE		
		FILA=FILA+1
		@FILA,01 SAY REPLICATE("-",71)
		FILA=FILA+1
		@FILA,20 SAY 'T O T A L  ---->'
	    @fila,53 SAY Lval PICTURE '@Z 999,999,999.99'
		FILA=FILA+1
		@FILA,01 SAY REPLICATE("-",71)
		IF vref='**********'
		   DO cab_ret1
		   LVAL=0
		   SELECT RETEN	
		   SET ORDER TO RETEN1
		   SEEK vkey
	   	   SCAN WHILE NUMMES=VANO AND NUMCP=VCLI AND CODCTC=VCTA
		    	 @FILA,03 SAY RETEN.CODRET PICTURE '@J'
		    	 @FILA,15 SAY RETEN.NOMRET
			     @FILA,53 SAY RETEN.VALRET PICTURE '@Z 999,999,999.99'
			     FILA=FILA+1
		    	 LVAL=LVAL+RETEN.VALRET
			ENDSCAN     
		FILA=FILA+1
			@FILA,01 SAY REPLICATE("-",71)
			FILA=FILA+1
			@FILA,20 SAY 'T O T A L  ---->'
		    @fila,53 SAY Lval PICTURE '@Z 999,999,999.99'
			FILA=FILA+1
			@FILA,01 SAY REPLICATE("-",71)
		ENDIF	
	ENDIF
	USE IN 12
	SELE ComPag
	GO vrec
ELSE
	@5,14 SAY CHR(14)
	@5,15 SAY "<< COMPROBANTE DE PAGO >>"
	@5,44 SAY CHR(27)+CHR(18)
	@07,03 SAY "      N§ C/P :"  
	@07,20 SAY NUMCP+'.'+NUMMES
	@08,03 SAY "   Fecha C/P :"  
	@08,20 SAY FECCP picture "@D"
	@09,03 SAY "     Cta cte :"  
	@09,20 SAY codctc
	@09,35 SAY val_fun('Caja','codctc','Descri',codctc)
	@14,14 SAY CHR(14)
	@14,18 SAY "I N U T I L I Z A D O"
	@15,01 SAY CHR(27)+CHR(18)
ENDIF
IF _DEST1=2
   	EJECT
ENDIF

SET DEVICE TO SCREEN 
RETURN


FUNCTION dispglo  &&Displaya Glosa de C/P.Retenci¢n
*--------------
parameters ZY
PRIVATE valias
valias=ALIAS()
SELE ComPag
SEEK ALLTRIM(reten.nummes)+reten.numcp+allt(reten.codctc)
IF FOUND()
   vglosa=IIF(ZY=1,SUBST(ComPag.glosa,21,36),IIF(ZY=2,SUBST(ComPag.glosa,58,36),IIF(ZY=3,SUBST(ComPag.glosa,95,36),' ')))
ELSE
   STORE SPACE(33) TO vglosa
ENDIF
SELE (valias)
RETURN vglosa

PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN

PROCEDURE fin_opcion
*-------------------
ON KEY
CLOSE DATA
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_1
RELEASE    WINDOW wind_2
RELEASE    WINDOW wind_3
RELEASE    WINDOW wind_4
RELEASE    WINDOW wind_c1
RELEASE    MENU   mMenu1
RESTORE SCREEN FROM principal
RETURN



FUNCTION Fecha
*-------------
vFecha = FecRet
RETURN



PROCEDURE VALE
*-------------
IF EMPTY(m.codctc) 
   DO STANDBY with 'No esta asignada la Cuenta Corriente'
*  DELE NEXT 1
   return .F.
else
   return .t.
endif



PROCEDURE imp_cp
*---------------
PUBLIC NUM_C
NUM_C=1
SET ESCAPE ON
ON ESCAPE STORE .F. TO PRINTING
_DEST = 'Impresora'
P_FIL = SPACE(8)
_Dest1 = 2
P_FIL = sys(3)+".LST"
IMPRE = (_DEST='Impresora')
PRINTING = .T.
DEFINE WINDOW Numcop FROM 20,50 TO 22,79 DOUBLE
ACTIVATE WINDOW Numcop
@ 0,3 SAY "N§ de Copias : " GET NUM_C PICTURE "99" VALID NUM_C>0 AND NUM_C<99
READ
RELEASE WINDOW Numcop
IF LASTKEY()=27
   RETUR
ENDIF 
IF IMPRE
   IF !EMPTY(LEFT(SYS(0),15))
     IF !YESNO("¨Imprime en impresora local?  <NO = IMPRESORA DE RED>")
       **-- Impresora de red.
       SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
       SET PRINTER TO \\SPOOLER\NB
     ENDIF
   ENDIF
   IF !READY2PR()
       PRINTING = .F.
   ENDIF
ENDIF   
IF PRINTING
  IF IMPRE
   	 SET DEVICE TO PRINT
     SET PRINTER TO &p_fil
     DO LISCP WITH NUM_C
     SET PRINTER TO
     IF READY2PR() 
     	FOR I=1 TO NUM_C
        	!TYPE &P_FIL >PRN
        ENDFOR	
     ENDIF
     ERASE &p_fil
   ENDIF
 ELSE
   DO STANDBY WITH 'EL REPORTE HA SIDO CANCELADO.'
 ENDIF
 SET DEVICE TO SCREEN
 SELE ComPag
RETURN


FUNCTION valsec  &&Displaya Nro C/P Sectorial
*--------------
PUBLIC vinicp
IF vsector#'A'
	vinicp=LEFT(m.numhc,1)
ELSE
	vinicp='A'
ENDIF 
IF m.codfte<>'00'
	SELECT Caja
	SEEK ALLTRIM(m.codctc)
	IF !FOUND()
		DO standby WITH 'No se tiene inscrita la cuenta ' + m.codctc
	ELSE
		m.numcp =  vinicp+PADL(corcp+1,3,'0')
		w_ctah  =  Caja.CuentaH
	ENDIF
ELSE
	USE CtaSec IN 0  ORDER CtaSec1 ALIAS CtaSec
	SELE CtaSec
	SEEK m.codctc+vinicp
	m.numcp =  PADL(CtaSec.corsec+1,3,'0')
	USE
	vinicp=vinicp+RIGHT(m.numcp,3)
	SELE ComPag
	@ 1,22 SAY vinicp PICTURE "!!!!"     
	m.numcp=vinicp
ENDIF

RETURN

FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)


FUNCTION Vexiste
*---------------
SELE ComPag
SEEK ALLT(m.nummes)+m.numcp+ALLT(m.codctc)
IF FOUND()
    DO STANDBY WITH "El Comprobante de Pago ya existe"
	RETURN .F.
ENDIF
RETURN .T.	

FUNCTION DISPDES  &&Displaya nombre del Acreedor
*---------------
DO CASE
   CASE ComPag.tipdoc$'HCMESUSHSS'
        vdes=IIF(ComPag.tipprv='P',val_auxi(ALLT(ComPag.codprv),'20','D'),IIF(ComPag.tipprv='E',val_Auxi(ALLT(ComPag.codemp),'30','D'),IIF(ComPag.tipprv='O' AND !EMPTY(m.codotr),val_Auxi(ALLT(ComPag.codotr),'09','D'),ComPag.nompre)))
   CASE ComPag.tipdoc$'RESRRURS' 
       vdes =VAL_PARA(ComPag.codret,'CODRET','D',22,40)
   OTHER     
	   vdes	=ComPag.nompre         		
ENDCASE       
RETURN vdes


FUNCTION Disppart  &&Displaya partida
*---------------
private ypart
ypart=0
IF ComPag.tipdoc$'HCRGSHSRSEHM'
   SELE IteCp
   SET ORDER TO Itecp1
   SEEK ALLT(ComPag.nummes)+ComPag.numcp+allt(ComPag.codctc)
   IF FOUND()
   	  IF Compag.tipfun='I'
   	     xpart=IteCp.CodPart
   	  ELSE
   	     ypart=val(IteCp.codanal)
   	     SCAN WHILE ALLT(IteCp.nummes)+IteCp.NumCP+ALLT(IteCp.codctc)=ALLT(ComPag.nummes)+ComPag.numcp+allt(ComPag.codctc) 
			 IF val(IteCp.codanal)>=ypart
		  	    ypart=val(IteCp.codanal)
			 ENDIF
		ENDSCAN
		Xpart=IIF(ypart<10,'0'+ALLT(STR(ypart,5,2)),ALLT(STR(ypart,5,2)))
	 ENDIF
	ELSE
	   DO STANDBY WITH "Sin partida"  			       	     
	ENDIF
ENDIF
RETURN Xpart	   

PROCEDURE cab_ret  &&Cabecera de Retenciones
*----------------
		@0,0 SAY CHR(18)
		@1,3   SAY ALLTRIM(CIA)
		@1,68  SAY "PAG:"
		@1,76  SAY ALLTRIM(STR(_PAGENO,8))
		@2,3   SAY "LisRetCP"
		@2,68 SAY "FECHA:"
		@2,76 SAY DATE()           
		@4,20 SAY CHR(14)
		@4,13 SAY IIF(X_SW1=1,"RETENCIONES CANCELADAS","DEDUCCIONES Y/O RETENCIONES")
		@5,0 SAY CHR(27)+CHR(18)
		@5,34 SAY 'C/P: '+VCLI+'.'+ALLTRIM(VANO)
		@6,1 SAY "Ú"
		@6,2 SAY REPLICATE("Ä",70)
		@6,72 SAY "¿" 
		@7,1 SAY "³"
		@7,3 SAY IIF(X_SW1=1," C/P              DESCRIPCION                              MONTO"," RUBRO            DESCRIPCION                              MONTO")
		@7,72 SAY "³"
		@8,1 SAY "À"
		@8,2 SAY REPLICATE("Ä",70) 
		@8,72 SAY "Ù"
	    FILA=10
RETURN

PROCEDURE cab_ret1  &&Cabecera de Retenciones
*----------------
		@0,0 SAY CHR(18)
		@1,3   SAY ALLTRIM(CIA)
		@1,68  SAY "PAG:"
		@1,76  SAY ALLTRIM(STR(_PAGENO,8))
		@2,3   SAY "LisRetCP"
		@2,68 SAY "FECHA:"
		@2,76 SAY DATE()           
		@4,20 SAY CHR(14)
		@4,13 SAY "DEDUCCIONES Y/O RETENCIONES"
		@5,0 SAY CHR(27)+CHR(18)
		@5,34 SAY 'C/P: '+VCLI+'.'+ALLTRIM(VANO)
		@6,1 SAY "Ú"
		@6,2 SAY REPLICATE("Ä",70)
		@6,72 SAY "¿" 
		@7,1 SAY "³"
		@7,3 SAY IIF(X_SW1=1," C/P              DESCRIPCION                              MONTO"," RUBRO            DESCRIPCION                              MONTO")
		@7,72 SAY "³"
		@8,1 SAY "À"
		@8,2 SAY REPLICATE("Ä",70) 
		@8,72 SAY "Ù"
	    FILA=10
RETURN

FUNCTION wdbco  &&Descripci¢n de Banco para el Repo
*-------------
PRIVATE wdbc,wcbc
SELE Caja
SEEK ComPag.codctc
IF FOUND()
   wcbc=Caja.banco
   wdbc=Val_Para(wcbc,"BANCOS","D",22,25)   
ELSE
   wdbc=SPACE(10)
ENDIF
RETURN wdbc      


FUNCTION Disp_usu
*----------------
PRIVATE xuser_id,xdesu
vali=ALIAS()
vord=ORDER()
xuser_id=SPACE(08)
xdesu=SPACE(30)
xuser_ID = CHRTRAN(Compag.Usuario,'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?','ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
USE USUARIO IN 0 ORDER USUARIO1 ALIAS USUARIO
SELE USUARIO
SEEK ALLT(xuser_id)
IF FOUND()
   xdesu  = ALLT(usuario.nombre)
ENDIF
USE 
SELE (Vali)
SET ORDER  TO (vord)
RETURN xdesu     


PROCEDURE LIMPIA
*---------------
@ 3,22 SAY SPAC(56)
RETURN

FUNCTION pres_pro        &&Displaya descripci¢n del proveedor
*----------------
DO CASE
   CASE m.tipprv="E"
        @ 3,22 SAY val_Auxi(ALLT(m.Codemp),'30','V')
   CASE m.tipprv="P"
        @ 3,22 SAY val_Auxi(ALLT(m.Codprv),'20','V')
   CASE m.tipprv="O" AND !EMPTY(m.codotr)
        @ 3,22 SAY val_Auxi(ALLT(m.Codotr),'09','V')
   CASE m.tipprv="R" AND !EMPTY(m.codotr)
   		@ 3,22 SAY VAL_PARA(m.codotr,'CODRET','V',22,40)
   OTHERWISE
        @ 3,22 SAY m.nompre
ENDCASE
RETURN

FUNCTION muespro
*---------------
PARAMETERS xtipprv,xcodotr,xtipdoc
PRIVATE xnompre
DO CASE
    CASE xtipprv="E"
         xnompre=val_Auxi(ALLT(Compag.Codemp),'30','V')
    CASE xtipprv="P"
         xnompre=val_Auxi(ALLT(Compag.Codprv),'20','V')
    CASE xtipprv="O" AND !EMPTY(xcodotr)
         xnompre=val_Auxi(ALLT(xCodotr),'09','V')
    CASE xtipdoc$"RESRRU"
		 xnompre=VAL_PARA(Compag.codret,'CODRET','V',22,40)	
	OTHER  	   
	     xnompre=compag.nompre
ENDCASE
RETURN xnompre


FUNCTION Val_rete
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias
  mCol = 0
  mLong = 40
  mDist = 6
  mAlias  = ALIAS()
  SELECT Parma
  SEEK Filtro+mValor
  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO Tipo = Filtro
    SET ORDER TO PARMAE2
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       SELECT (mAlias)
	   RETURN 
    ENDIF
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(Descri,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF
    RELEASE POPUP parametro
    SELE PARMA
    SET FILTER TO
    SET ORDER TO PARMAE1
  ENDIF
  mValor = Parma.Codigo
  mCuenta= Parma.DescriAu2
  mDescr = SUBSTR( Parma.Descri, 1, mLong )
  mDescriAux = LEFT(Parma.descriau2, 1)
  SELECT (mAlias)
  REPLACE Reten.codret  WITH mValor
  REPLACE Reten.nomret  WITH mdescr
  REPLACE reten.tributo WITH mdescriaux
RETURN .T.


PROCEDURE vis_calen   &&Vista Codificaci¢n Funcional Program tica
*-------------------
@ 06,22 SAY m.codcad
@ 06,27 SAY " UG  UE  FN PRG SBPRG ACTPRY"
@ 07,28 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
RETURN


FUNCTION val_ff
*--------------
m.fecdig=DATE()
m.hordig=TIME()
RETURN

PROCEDURE regret
*---------------
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL f10 KEYBOARD CHR(23)
USE reten    IN 12  ORDER TAG reten1       ALIAS reten
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT itecp
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
vvcompon=Itecp.codcom

SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
STORE 0 TO vdebe, vhaber
SCAN WHILE nummes=ALLTRIM(m.nummes) and numref=m.numcp and codctc=ALLTRIM(m.codctc)
	 IF RET='S' && suma solo valores del haber para comparar con valor de retencion
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
	 ENDIF
ENDSCAN


SELECT reten
SEEK ALLT(m.nummes)+m.numcp+ALLT(m.codctc)
vale=0
IF !FOUND()
   DO STANDBY WITH 'No existen retenciones registradas'
ENDIF
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_5 KEY ALLT(m.nummes)+m.numcp+ALLT(m.codctc) FIELDS;
	estado     :H= 'Es' :W=.F. :R ,;
	fecret     :H= 'Fecha':V=Fecha() ,;
	codret     :v=val_rete(Codret,"CODRET","Codret")  ,;
    nomret     :H= 'Retenido a':p='@S39':R ,;
	Valret     :H= 'Monto' :p='99,999,999.99' 
SELE reten	
GO TOP
SEEK ALLT(m.nummes)+m.numcp+ALLT(m.codctc)
totalq = 0

SELE ComPag
USE IN 09  
USE IN 12
DO vista
RETURN


FUNCTION Val_Codc1
*------------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
      mDist = 6
    CASE PARAMETERS() = 5
      mDist = 6
  ENDCASE
  mAlias  = ALIAS()

  SELECT maepre
  SET ORDER TO maepre1
  SEEK Filtro+mValor

  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO periodo+uniges+unieje = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   return	
    ENDIF
    SET ORDER TO maepre4
    ON KEY LABEL f2  DO Completo
    ON KEY LABEL f3  DO PorCompon

    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_CAD FROM 02,01 TO 23,78  DOUBLE ;
		TITLE '[F2] Orden completo [F3] Orden x Act/Pry+Comp.  [F10] seleccionar' COLOR SCHEME 15
		ACTIVATE WINDOWS wind_cad
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad  FIELD ;
		CODCAD	: H='CodCad',;
    	CODFUN	: H='Fn',;
    	CODPRG	: H='Prg',;
    	CODSPR	: H='SPrg',;
    	ACTPRY	: H='Act/Pry',;
    	CODCOM	: H='CodComp',;
    	CODMET	: H='Meta',;
    	Descri	: H='Descripci¢n':40
	
    RELEASE WINDOWS wind_cad
    ON KEY LABEL f2  
    ON KEY LABEL f3  
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    SET FILTER TO
  ENDIF
  mValor = maepre.codcad
  mDescr = SUBSTR( maepre.Descri, 1, mLong )
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n S
      RETURN mCodAux
*     @ ROW(),mCol SAY mDescr
*     RETURN ' '
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE
*-------

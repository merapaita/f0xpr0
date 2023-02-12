*--------------------------------------------------------------------------
* AnuPar.Prg
* Anula Orden de Compra
* Estado :
*   '00' Emitida   Este es el que se registra en la Orden de Compra
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: 
*--------------------------------------------------------------------------
parameter vOpcion,sistema
*- Abriendo Archivos
if vOpcion = 1
   USE PteAnu   IN 1   order tag PteAnu1  ALIAS AnuPA
   USE OrdCom   IN 2   order tag OrdCom1  ALIAS Orden
   USE IteOc    IN 3   order tag IteOc1   ALIAS IteOc
   USE IteOc1   IN 15  order tag IteOc11  ALIAS IteOc1
else
   USE PteAnu   IN 1   order tag PteAnu2  ALIAS AnuPA
   USE OrdSer   IN 2   order tag OrdSer1  ALIAS Ordse
   USE Solser   IN 3   order tag Solser1  ALIAS Solser
   USE IteOs1   IN 15  order tag IteOs11  ALIAS IteOs1   
endif
USE Parmae   IN 4   order tag Parmae1  ALIAS Parma
USE Artmae   IN 5   order tag Artmae1  ALIAS Produ
USE Itepec   IN 6   order tag ItePec4  ALIAS Itepec
USE AuxCot   IN 7   order tag AuxCot1  ALIAS Auxcot
USE Promae   IN 8   order tag Promae1  ALIAS Promae
USE Calen    IN 9   order tag calen1   ALIAS calen
USE maepre   IN 10  order tag maepre1  ALIAS maepre
USE itepar   in 11  order tag itepar1  ALIAS ITEPAR          
USE HOJCON   in 12  order tag hojcon1  ALIAS hoja
USE Cheque   IN 13  ORDER TAG Cheque1  ALIAS Cheque
USE AstOrd	 IN 16	ORDER TAG AstOrd1  ALIAS AstOrd

*- Mensajes de aviso al usuario

PUBLIC VMENSL
vmensL= Iif(Vopcion=1,' PARTE ANULACION O/C',' PARTE ANULACION O/S')
if vOpcion = 1
	Vmens01 = ' Parte Anulaci¢n Orden de Compra : REVISION '
	Vmens02 = ' Registro de Parte Anulaci¢n Ordenes de Compra '
	Vmens04 = 'Dicho Orden de Compra no fue encontrado'
	Vmens05 = 'No existe Orden de Compra anterior'
	Vmens06 = 'No existe Orden de Compra siguiente'
	Vmens07 = '¨ Desea ANULAR ‚ste Orden de Compra ?'
	Vmens08 = 'No hay registros para procesar'
	Vmens09 = 'Este Orden de Compra ha sido anulado'
	Vmens10 = 'Este Orden de Compra ya fue atendido'
	Vmens11 = 'Este Orden de Compra ha sido devuelto'
else
	Vmens01 = ' Parte Anulaci¢n Orden de Servicio: REVISION '
	Vmens02 = ' Registro de Parte Anulaci¢n Ordenes de Servicio '
	Vmens04 = 'Dicho Orden de Servicio no fue encontrado'
	Vmens05 = 'No existe Orden de Servicio anterior'
	Vmens06 = 'No existe Orden de Servicio siguiente'
	Vmens07 = '¨ Desea ANULAR ‚ste Orden de Servicio ?'
	Vmens08 = 'No hay registros para procesar'
	Vmens09 = 'Este Orden de Servicio ha sido anulado'
	Vmens10 = 'Este Orden de Servicio ya fue atendido'
	Vmens11 = 'Este Orden de Servicio ha sido devuelto'
endif

SELECT AnuPA
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
HIDE POPUP ALL
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
	ACTIVATE SCREEN
	ACTIVATE MENU mMenu
ENDDO
DO Fin_opcion
RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD anula  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_0
CLEAR
@  1, 2 SAY "       N£mero P/A :"
@  2, 2 SAY "        Fecha P/A :"
IF vOpcion =1
	@  1,40 SAY "       N£mero O/C :"
	@  2,40 SAY "        Fecha O/C :"
 else
	@  1,40 SAY "       N£mero O/S :"
	@  2,40 SAY "        Fecha O/S :"
endif
@  4, 2 SAY "        Proveedor :"
@  5, 2 SAY "         Importe  :"

@  6, 2 SAY "      Cadena Fun. :"
@  7, 2 SAY " Fte. Financiami. :"

@  8, 2 SAY "          Funci¢n :"
@  9, 2 SAY "         Programa :"
@ 10, 2 SAY "      Subprograma :"
@ 11, 2 SAY "   Activ./Proyec. :"

@ 13, 2 SAY "           Motivo :"

@ 15, 2 SAY "       Numero H/C :"
@ 16, 2 SAY "       Numero C/P :"
@ 17, 2 SAY "    Numero Cheque :"

@ 19, 2 SAY "    Observaciones :"
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
ACTIVATE WINDOW Wind_0
SELECT AnuPA

IF EOF()
	DO Pantalla
	RETURN
ENDIF

SCATTER MEMVAR
=val_codcad(allt(m.codcad),m.periodo,'C')

@  0,60 SAY IIF( m.Estado= '00','Pendiente   ',IIF( m.Estado = '20','En Cont.Pres.',IIF(m.Estado='99','Anulada      ',IIF(m.Estado='50','Atendido    ','             '))))

@  1,22 SAY m.NumPa

@  1,60 SAY m.Periodo 
@  1,63 SAY m.Codfte  
@  1,67 SAY m.NumRef  

@  2,22 SAY m.FecPa
@  2,60 SAY m.FecRef

@  4,22 SAY val_prv(m.Codprv)

@  5,22 SAY m.ValTot PICTURE '999,999.99'

@  6,22 SAY val_codcad(m.codcad,m.periodo,'D',22,30)
@  7,22 SAY val_para(m.codfte,'CODFTE','D',22,30)

@  8,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
@  9,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
@ 10,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
@ 11,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

@ 13,22 SAY m.Motivo PICTURE '@S56'

@ 15,22 SAY m.NumHC 
@ 15,26 SAY '.'
@ 15,27 SAY m.PerHC  

@ 16,22 SAY m.NumCP  
@ 16,26 SAY '.'
@ 16,27 SAY m.PerCp  
@ 17,22 SAY m.numchq

@ 19,22 SAY m.Observa
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT ANUPA
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 numpa  :H='N§Dc' ,;
 fecpa  :H='Fecha' ,;
 TipDoc :H='Doc' ,;
 numref :H='N§' ,;
 Fecref :H='Fecha' ,;
 ess=IIF( Estado= '00','Pend',IIF( Estado = '20','Aten',IIF(Estado='99','Anul',IIF(Estado='50','Liqu','Aten')))) :H='Estd',;
 codprv :H='Prv' ,;
 observa :H='Observaci¢n'

vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
IF LASTKEY()=27
	GOTO Vtemp
ENDIF
SHOW MENU mMenu
ON KEY LABEL F10
DO Vista
RETURN

PROCEDURE Busca              && Realiza b£squeda directa
*--------------
SELECT ANUPA
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vPeriodo = RIGHT(DTOC(DATE()),2)
vNum_Oc  = 0
ACTIVATE WINDOW standby
if vopcion=1
	@ 1,01 SAY 'Ingrese N£mero O/C : ' &&GET vPeriodo PICTURE '!!'
 else
	@ 1,01 SAY 'Ingrese N£mero O/S : ' &&GET vPeriodo PICTURE '!!'
endif
@ 1,23 GET vNum_Oc PICTURE '9999' VALID vBusca()
READ

DEACTIVATE WINDOW standby
IF EMPTY(vNum_Oc) .or. LASTKEY()=27
	RETURN
 ELSE
	SEEK vNum_Oc
	IF !FOUND()
		DO standby WITH Vmens04
		GOTO Vtemp
	 ELSE
	 	DO Vista
	ENDIF
ENDIF
RETURN

PROCEDURE vBusca
*---------------
vNum_Oc=Padl(alltrim(str(vNum_Oc,4)),4,'0')
retur .t.

PROCEDURE Anter
*--------------
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
IF !BOF()
	SKIP -1
ENDIF
IF BOF()
	GO TOP
	DO standby WITH Vmens05
 ELSE
	DO Vista
ENDIF
RETURN

PROCEDURE Proxi
*--------------
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
IF !EOF()
	SKIP
ENDIF
IF EOF()
	DO standby WITH Vmens06
	GO BOTTOM
 ELSE
	DO Vista
ENDIF
RETURN

PROCEDURE Corri
*--------------
DO PANTALLA
SELECT AnuPA
SCATTER MEMVAR
=val_codcad(allt(m.codcad),m.periodo,'C')
IF ESTADO="20"
	DO Standby with "El Parte de Anulaci¢n ya fue Contabilizado"
	RETURN
 else    
	@  1,22 GET m.NumPa   DISABLE
	@  2,22 GET m.FecPa		VALID Val_Ano()
	
	@  1,60 GET m.Periodo DISABLE
	@  1,63 GET m.Codfte  DISABLE
	@  1,67 GET m.NumRef  DISABLE
	
	@  2,60 GET m.FecRef  DISABLE
	
	@  4,22 SAY val_prv(m.Codprv)
	@  5,22 SAY m.ValTot  PICTURE '999,999.99'
	
	@  6,22 GET m.codcad PICTURE '!!!!' VALID VAL_CODCAD(m.codcad,m.periodo,' ',22,30)
	@  7,22 GET m.codfte PICTURE '!!'   VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,30)
endif

m.Mespa  = PADL(ALLTRIM(STR(MONT(m.FecPA))),2,'0')
 
IF LASTKEY() # 27
	SELECT AnuPA
	GATHER MEMVAR
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
ENDIF

@  8,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
@  9,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
@ 10,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
@ 11,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

@ 13,22 GET m.Motivo PICTURE '@S56'

@ 15,22 SAY m.NumHC 
@ 15,26 SAY '.'
@ 15,27 SAY m.PerHC  

@ 16,22 SAY m.NumCP  
@ 16,26 SAY '.'
@ 16,27 SAY m.PerCp  

@ 17,22 SAY m.NumChq 

@ 19,22 GET m.Observa

READ VALID Val_Read()

IF LASTKEY() # 27
	SELECT AnuPA
	GATHER MEMVAR
	do ASIORD
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL
DO PANTALLA
DO Vista
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
DO PANTALLA

SCATTER MEMVAR BLANK

m.NumRef  = space(4)
m.codfte  = space(2)
m.FecPa   = date()
m.Tipo    = 'A'
m.periodo = PADL(ALLTRIM(str(year(date()) - 2000)),2,'0')

m.Estado  = '00'

SELECT AnuPA

@  1,60 GET m.Periodo picture '!!'   valid !empty(m.Periodo)
@  1,63 GET m.Codfte  PICTURE '!!'   valid val_para(m.Codfte,'CODFTE','C')
@  1,67 GET m.NumRef  PICTURE '!!!!' VALID VAL_REV()
READ

IF LASTKEY()=27
	SELECT AnuPA
	DO VISTA
	RETURN
ENDIF

SELECT Parma
if vOpcion = 1
	SEEK 'CORREL'+'ANULOC'
 else
	SEEK 'CORREL'+'ANULOS'
endif
= valPA ( Parma.NumEnt + 1 )
SELECT AnuPA

IF vOpcion = 1
	m.CodFte = orden.CodFte
	m.TipDoc =  'O/C'
	m.NumRef = orden.NumOc
	m.Mespa  = PADL(ALLTRIM(STR(MONT(m.FecPA))),2,'0')
	m.FecRef = orden.fecOc
	m.CodPrv = orden.CodPrv
	m.Codcad = orden.codcad
	m.valtot = orden.Valtot
 ELSE
	m.CodFte = ordse.CodFte
	m.TipDoc =  'O/S'
	m.NumRef = ordse.NumOs
	m.Mespa  = PADL(ALLTRIM(STR(MONT(m.FecPA))),2,'0')
	m.FecRef = ordse.fecOs
	m.CodPrv = ordse.CodPrv
	m.Codcad = ordse.codcad
	m.valtot = ordse.Valtot
ENDIF
m.motivo  = space(254)
m.observa = space(40)

@  1,22 GET m.NumPa   DISABLE
@  2,22 GET m.FecPa		VALID Val_Ano()

@  1,60 GET m.Periodo DISABLE
@  1,63 GET m.Codfte  DISABLE
@  1,67 GET m.NumRef  DISABLE

@  2,60 GET m.FecREf  DISABLE

@  4,22 SAY val_prv(m.Codprv)
@  5,22 SAY m.ValTot  PICTURE '999,999.99'

@  6,22 SAY VAL_CODCAD(m.codcad,m.periodo,' ',22,30)
@  7,22 SAY VAL_PARA(m.CodFte,'CODFTE',' ',22,30)

@  8,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
@  9,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
@ 10,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
@ 11,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

@ 13,22 GET m.Motivo PICTURE '@S56'

@ 15,22 SAY m.NumHC 
@ 15,26 SAY '.'
@ 15,27 SAY m.PerHC  

@ 16,22 SAY m.NumCP  
@ 16,26 SAY '.'
@ 16,27 SAY m.PerCp  

@ 17,22 SAY m.NumChq 

@ 19,22 GET m.Observa

READ VALID Val_Read()

m.Mespa  = PADL(ALLTRIM(STR(MONT(m.FecPA))),2,'0')

IF LASTKEY() # 27
	vcod = m.numpa
	IF F_Appd()
		*- Aumento el correlativo
		IF vOPCION=1						&&Para las O/C
			SELE ORDEN
			IF RLOCK()
				REPLACE ANULTOT WITH m.ValTot,TIPALT WITH 'A',NUMANU WITH M.NUMPA,ORDEN.ESTADO WITH '21',ITEOC.ESTADO WITH '21'
			ENDIF
			IF !empty(m.numhc)
				sele hoja
				SEEK m.perhc+m.numhc
				IF found()
					IF rlock()
						replace numanu with m.NumPa,valanu with m.valtot
					ENDIF
				ENDIF
			ENDIF
			SELE ITEOC
			SCAN WHILE iteoc.numoc+iteoc.codfte = orden.numoc + orden.codfte
				vArt = iteoc.codart
				vCad = iteoc.codcad
				vPrv = orden.codprv
				vkey = iteoc.periodo+iteoc.numoc+iteoc.codfte
				vfun = .t.
				SELE ITEPEC
				SET ORDE TO ItePec7
				vTemp=Recno()
				vkey = iteoc.periodo+iteoc.numoc+iteoc.codfte+iteoc.codart
				SEEK VKEY
				IF FOUND()
					IF RLOCK()
						IF !EMPTY(NUMSC)
							REPLACE orden with ' ',estado with '20' , NUMOC WITH SPACE(4)
						 ELSE
							REPLACE orden with ' ',estado with '10',  NUMOC WITH SPACE(4)
						ENDIF
					 ELSE
						do standby with 'Advertencia:La Pecosa no se habilit¢'
						vFun=.f.
					ENDIF
					UNLOCK
				 ELSE
					DO STANDBY WITH 'Advertencia:No es ubicado la Pecosa,Revise'
					vFun = .F.
				ENDIF
				SELECT IteOC
				IF RLOCK() and vfun
					REPLACE ESTADO WITH '95'
				ENDIF
				UNLOCK
			ENDSCAN
		 ELSE
			if !empty(m.numhc)
				select Hoja
				seek m.perhc+m.numhc
				if found()
					if rlock()
						replace numanu with m.NumPa,valanu with m.valtot
					endif
				endif
			endif
			SELE ORDSE
			IF RLOCK()
				REPLACE ANULTOT WITH m.ValTot,TIPALT WITH ' ',NUMANU WITH M.NUMPA ORDSE.ESTADO WITH '21'
			ENDIF
			SELECT SOLSER
			SEEK m.Periodo + ordse.numss
			If FOUND()
				IF RLOCK()
					***POR GUSWALEN 12/07/99
					REPLACE peros with '  ',estado with '10',;
							NUMOs WITH SPACE(4)
				 ELSE
					do standby with 'Advertencia:La S/S no se habilit¢'
				ENDIF
			 else
				do standby with 'Advertencia:No es ubicado la S/S,Revise'
			ENDif
		ENDIF
		UNLOCK ALL
		m.numpa = vcod
		m.tipo = 'A'
		SELECT AnuPA
		GATHER MEMVAR
		DO ASIORD
		SELECT Parma
		if vOpcion = 1
			SEEK 'CORREL'+'ANULOC'
		 else
			SEEK 'CORREL'+'ANULOS'
		endif
		= valPA ( Parma.NumEnt + 1 )
		REPLACE NumEnt WITH NumEnt + 1
		SELECT AnuPA
	ENDIF
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
	SELECT AnuPA
ENDIF
DO Vista
RETURN

FUNCTION Val_Ano
*---------------
IF YEAR(m.FecPA) = YEAR(m.FecSis)
	mRet = .T.
ELSE
	DO StandBy WITH "Error en la Fecha... Revise."
	mRet = .F.
ENDIF
RETURN mRet

PROCEDURE AsiOrd
*---------------
USE AsiAut IN 0 ORDER TAG AsiAut1
SELE AsiAut
IF vOpcion = 1
	vTip = "AOC"
 ELSE
	vTip = "AOS"
ENDIF
SEEK vTip+"   "+"ASTORD"
IF !FOUND()
	DO StandBy WITH "PARAMETRO DE CTAS. DE ORDEN INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
	RETURN
 ELSE
	cCtaD = DCuenta
	cCtaH = HCuenta
ENDIF
SELE AstOrd
SEEK m.Periodo+m.MesPA+m.NumPA+vTip
IF FOUND()
	FOR i = 1 TO 2
		IF f_Lock(1) OR RLOCK()
			REPLACE Periodo WITH m.Periodo ,;
					NUMMES	WITH m.MesPA   ,;
					NUMREF	WITH m.NumPA   ,;
					TIPDOC	WITH vTip      ,;
					FECHA	WITH m.FecPA ,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,m.ValTot,0),;
					MTOHAB	WITH IIF(i=2,m.ValTot,0)
			UNLOCK
			SKIP
		ENDIF
	ENDFOR
 ELSE
	FOR i = 1 TO 2
		IF f_Appd()
			REPLACE Periodo WITH m.Periodo ,;
					NUMMES	WITH m.MesPA   ,;
					NUMREF	WITH m.NumPA   ,;
					TIPDOC	WITH vTip      ,;
					FECHA	WITH m.FecPA ,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,m.ValTot,0),;
					MTOHAB	WITH IIF(i=2,m.ValTot,0)
			UNLOCK
		ENDIF
	ENDFOR
ENDIF

USE IN AsiAut
DEFINE WINDOW wAstOrd FROM 10,10 TO 15,70 ;
	 TITLE ' ASIENTOS DE ORDEN' COLOR SCHEME 02
ACTIVATE WINDOW wAstOrd
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY '        Debe '
@ 00,34  SAY '        Haber '
@ 01,04  SAY cCtaD PICTURE '!!!!!!!!!!!'
@ 01,18  SAY m.ValTot PICTURE '99,999,999.99'
@ 02,12  SAY cCtaH PICTURE '!!!!!!!!!!!'
@ 02,34  SAY m.ValTot PICTURE '99,999,999.99'
WAIT " "
DEACTIVATE WINDOW wAstOrd
RELEASE WIND wAstOrd
RETURN

PROCEDURE VAL_REV              && Revisi¢n de BD en browse
*----------------
PRIVATE VTEMP

IF vOpcion = 1  && ************ PARTE DE ANULACION O/C *******************
	AS=ALIAS()
	SELECT ORDEN
	SET FILT TO ESTADO#'5'
	IF EOF()
		DO standby WITH Vmens08
		RETURN
	ENDIF
	SET RELATION TO PERIODO+NUMOC+CODFTE INTO ITEOC
	SET SKIP TO ITEOC
	SET FILTER TO ESTADO#'90'AND ESTADO#'99'
	GO TOP
	SEEK m.Periodo+m.Numref+alltrim(m.Codfte)
	IF !FOUND()
		vTemp = RECNO()
		HIDE MENU mMenu
		ACTIVATE SCREEN
		vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
		DO Logos WITH Rotulo1,vTempo
		ON KEY LABEL F10 KEYBOARD CHR(23)
		BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
			NumOc  :H=' N§ ' ,;
			FecOc  :H='Fecha' ,;
			ess=IIF( Estado= '00','Pend',IIF( Estado = '20','Afec.',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
			iteOc.descri :H='Articulo ' :36 ,;
			iteOc.coduni :H='Unid' ,;
			iteOc.Canreq :H='Cantid'
		
		vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
		DO Logos WITH Rotulo1,vTempo
		SHOW MENU mMenu
		ON KEY LABEL F10
		m.NumHc  = orden.NumHc
		m.PerHc  = orden.PerHc
		
		SELECT Hoja
		vOrd = ORDER()
		SET ORDER TO hojcon1
		SEEK m.PerHc+m.numhc
		
		IF FOUND() 
			IF !EMPTY(HOJA.NumCp)
				m.NumCp  = Hoja.NumCp
				m.percp  = Hoja.NumMesCp
				vCta    = Hoja.CodCtc
			 else
				m.Numcp  =''
				m.percp  =''
				vCta    =''
			endif
		 ELSE
			m.Numcp  =''
			m.Percp  =''
			vCta    =''
		ENDIF
		SET ORDER TO (vOrd)
		
		SELECT Cheque
		vord = ORDER()
		SET ORDER TO Cheque1
		SEEK m.percp+m.NumCp+vCta
		IF FOUND() 
			m.NumChq = Cheque.NumChq
		 ELSE
			m.NumChq =''
		ENDIF
		SET ORDER TO (vOrd)
		m.codfte = iteoc.codfte
	 ELSE
		m.NumHc  = orden.NumHc
		m.PerHc  = orden.PerHc
		
		SELECT Hoja
		vOrd = ORDER()
		SET ORDER TO hojcon1
		SEEK m.PerHc+m.numhc
		
		IF FOUND() 
			IF !EMPTY(HOJA.NumCp)
				m.NumCp  = Hoja.NumCp
				m.percp  = Hoja.NumMesCp
				vCta    = Hoja.CodCtc
			 else
				m.Numcp  =''
				m.percp  =''
				vCta    =''
			endif 	
		 ELSE
			m.Numcp  =''
			m.Percp  =''
			vCta    =''
		ENDIF
		SET ORDER TO (vOrd)
		
		SELECT Cheque
		vord = ORDER()
		SET ORDER TO Cheque1
		SEEK m.percp+m.NumCp+vCta
		IF FOUND() 
			m.NumChq = Cheque.NumChq
		 ELSE
			m.NumChq =''
		ENDIF
		SET ORDER TO (vOrd)
		m.codfte = iteoc.codfte
	ENDIF
*	set filter to
*	SET RELATION TO
	SELE (AS)
*	DO Vista
 ELSE  && ******************* PARTE DE ANULACION DE O/S ********************
	AS=ALIAS()
	SELECT OrdSe
	SET FILT TO ESTADO#'5'
	IF EOF()
		DO standby WITH Vmens08
		RETURN
	ENDIF
	SET FILTER TO ESTADO#'90'AND ESTADO#'99'
	GO  TOP
	SEEK m.Periodo+m.Numref+alltrim(m.Codfte)
	IF !found()
		Vtemp = RECNO()
		HIDE MENU mMenu
		ACTIVATE SCREEN
		vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
		DO Logos WITH Rotulo1,vTempo
		ON KEY LABEL F10 KEYBOARD CHR(23)
		BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
				NumOs  :H=' N§ ' ,;
				Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
				FecOs  :H='Fecha' ,;
				CodDep :H='DEP',;
				DESOS  :H='Descripci¢n'
				
				* Codcal :H='Calendario' ,;
		vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
		DO Logos WITH Rotulo1,vTempo
		m.codfte = ordse.codfte
		IF LASTKEY()=27
			GO TOP
*			GOTO Vtemp
		ENDIF
		SHOW MENU mMenu
		ON KEY LABEL F10
		m.NumHc  = ordSe.NumHc
		m.PerHc  = ordSe.PerHc
		
		SELECT Hoja
		vOrd = ORDER()
		SET ORDER TO hojcon1
		SEEK m.PerHc+m.numhc
		IF FOUND()
			IF !EMPTY(HOJA.NumCp)
				m.NumCp  = Hoja.NumCp
				m.percp  = Hoja.NumMesCp    
				vCta    = Hoja.CodCtc
			 else
				m.Numcp  =''
				m.percp  =''
				vCta    =''
			endif
		 ELSE
			m.Numcp  =''
			m.Percp  =''
			vCta    =''
		ENDIF
		SET ORDER TO (vOrd)
		
		SELECT Cheque
		vord = ORDER()
		SET ORDER TO Cheque1
		SEEK m.percp+m.NumCp+vCta
		IF FOUND()
			m.NumChq = Cheque.NumChq
		 ELSE
			m.NumChq =''
		ENDIF
		SET ORDER TO (vOrd)
		m.codfte = Ordse.codfte
	 ELSE
		m.NumHc  = ordse.NumHc
		m.PerHc  = ordse.PerHc
		
		SELECT Hoja
		vOrd = ORDER()
		SET ORDER TO hojcon1
		SEEK m.PerHc+m.numhc
		
		IF FOUND() 
			IF !EMPTY(HOJA.NumCp)
				m.NumCp  = Hoja.NumCp
				m.percp  = Hoja.NumMesCp
				vCta    = Hoja.CodCtc
			 else
				m.Numcp  =''
				m.percp  =''
				vCta    =''
			endif
		 ELSE
			m.Numcp  =''
			m.Percp  =''
			vCta    =''
		ENDIF
		SET ORDER TO (vOrd)
		
		SELECT Cheque
		vord = ORDER()
		SET ORDER TO Cheque1
		SEEK m.percp+m.NumCp+vCta
		IF FOUND()
			m.NumChq = Cheque.NumChq
		 ELSE
			m.NumChq =''
		ENDIF
		SET ORDER TO (vOrd)
		m.codfte = ordse.codfte
	ENDIF
	SELE (AS)
*	DO Vista
ENDIF

RETURN

function valprv
*--------------
private xx, vfun
vfun = .f.
codprv = iif( empty(codprv),codprv,padl(alltrim(codprv),4,'0'))
xx = val_prv( codprv,.t.)
if xx
   replace codprv with promae.codprv ,  nompro with promae.nompro
   return .t.
endif
return vfun

PROCEDURE Anula
*--------------
 SELECT AnuPA
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR esta Parte ?')
 IF vElimina
    IF VOPCION=1
       SELE ORDEN
       IF RLOCK()
         REPLACE ORDEN.TIPALT WITH ' ',ORDEN.NUMANU WITH '    ' &&ORDEN.ESTADO WITH '20'
       ENDIF
       if !empty(m.numhc)
              select Hoja
              seek m.perhc+m.numhc
              if found()
                 if rlock()
                    replace numanu with '    ',valanu with 0
                 endif   
              endif
       endif         
    ELSE
       SELE ORDSE
       IF RLOCK()
         REPLACE ORDSE.TIPALT WITH ' ',ORDSE.NUMANU WITH '    ' &&ORDSE.ESTADO WITH '20'
       ENDIF
          if !empty(m.numhc)
              select Hoja
              seek m.perhc+m.numhc
              if found()
                 if rlock()
                    replace numanu with '    ',valanu with 0
                 endif   
              endif
          endif         
    ENDIF
    
    SELECT AnuPA
    IF  RLOCK()
       REPLACE ESTADO WITH '99' ,FECVER WITH DATE()
    ENDIF
    DO Vista
 ENDIF
 UNLOCK
 RETURN

PROCEDURE Agreg_Item
*-------------------
vOrd=ORDER()
Sele iteOc
IF F_appd()
  REPLACE NumOc  WITH m.NumOc ,;
          Periodo WITH m.Periodo ,;
          CodArt WITH Itepec.CodArt ,;
          CodCad WITH Itepec.CodCad ,;
          CanReq WITH Itepec.Canreq ,;
          CodUni WITH Itepec.Coduni ,;
          DesCri WITH Itepec.Descri ,;
          Preuni WITH Itepec.Preuni
  RETURN .T.
ENDIF
SET ORDER TO vOrd
RETURN .F.


PROCEDURE Marca
*-----------------
vTemp = Recno()
vOrde = Itepec.Orden
vCodCal=ItePec.CodCad+ItePec.CodPrv
SET FILTER TO ItePec.CodCad+ItePec.CodPrv=vCodCad
GO TOP
SCAN
   DO CASE
      CASE vOrde='*'
           if rlock()
              REPLACE orden WITH '*' , estado with '30'
           ENDIF
      CASE vOrde=' '
           if rlock()
              REPLACE orden WITH ' ' , estado with '20'
           ENDIF
   ENDCASE
ENDSCAN
SET FILTER TO
go vTemp
KEYBOARD CHR(23)
RETURN .T.

PROCEDURE Lista
*--------------
SELECT AnuPA
vtemp =recno()

if eof()
   do standby with vmens08
   return
else
  ACTIVATE WINDOW STANDBY
  vnumpa = m.numpa
  vperio = m.periodo
  @ 1,1 say 'Ingrese N§ Pte Anulaci¢n : ' get vperio
  @ 1,32 get vnumpa
  read
  DEACTIVATE WINDOW STANDBY
  IF VOPCION = 1 
     SET ORDER TO PTEANU1
  ELSE
     SET ORDER TO PTEANU2
  ENDIF      
  SET FILTER TO PERIODO = VPERIO AND NUMPA=VNUMPA
  if vOpcion = 1
     do reporte with 2,"LisPAC1",vmensL,2,.F.,.F.
  else   
     do reporte with 2,"LisPAs1",vmensL,2,.F.,.F.
  endif    
  SET FILT TO
endif
select AnuPA
go vtemp
DO VISTA
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F2
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE WINDOW wind_0
  RELEASE WINDOW wind_1
  RELEASE WINDOW wind_c1
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


function valprv
*--------------
private xx, vfun
vfun = .f.
m.codpr = iif( empty(m.codprv),m.codprv,padl(alltrim(m.codprv),4,'0'))
xx = val_prv( m.codprv,.t.)
if xx
   return .t.
endif
return vfun

function valPA
*-----------------
parameter vnumPA
private vfun
vfun = .t.
m.numPA =padl(alltrim(str(vnumPA,4)),4,'0')
if m.numPA  = '0000' or empty(m.numPA)
   vfun = .f.
endif
return vfun

FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,vFun
vFun = .F.

XX = Val_Art(CodArt,.F.)
IF XX
  SELECT Itepec
  REPLACE ;
          coduni  WITH Produ.coduni,;
          preuni  WITH Produ.preuni
  vFun = .T.
ENDIF
RETURN vFun

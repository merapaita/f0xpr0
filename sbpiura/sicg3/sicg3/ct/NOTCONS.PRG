**--------------------------------------
** NOTCON1.PRG
** Programa de Ingreso
** de Notas de Contabilidad y otros Doc
** Reingenieria de Procesos
** AUTOR : A.S. Marco Rodriguez Abad - 08/11/2000
** Estados : 00 Pendiente de Cancelaci¢n
**           50 Cancelado
**--------------------------------------

*- Guarda el menu y la pantalla anterior
PUBLIC vIndPar,filaux,DtotalDoc,HtotalDoc

SAVE SCREEN TO pta_comp
HIDE POPUP pop_01,menu
*- Mensajes de aviso al usuario
mverifd='    '
PRIVATE Vmens01,Vmens02,Vmens09,vMens20
Vmens01 = ' Notas de Contabilidad '
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = ' °Detalle° '
Vmens20 = 'Documento Ya Procesado en el Balance.'

filaux  = SYS(3)+'.Dbf'
vIndPar = SYS(3)+'.Idx'
*- Abre archivos
USE Compro  IN  1 ORDER TAG Compro7  ALIAS Comp
USE Diario  IN  2 ORDER TAG Diario11 ALIAS Diar
USE Cuentas IN  3 ORDER TAG Cuentas1 ALIAS Cuen
USE Parmae  IN  4 ORDER TAG Parmae1  ALIAS Parma
USE Auxil   IN  6 ORDER TAG Auxil1   ALIAS Auxi
USE Cajas   IN 10 ORDER TAG Cajas1   ALIAS Caja

SELECT DIAR
COPY STRUCTURE TO (filaux)
USE (filaux) IN 8 EXCLUSIVE                  ALIAS diax

DO Indexa_Par

SELECT parma
IF blo_sis(1)
   DO STANDBY WITH "Sistema Bloquedo, Consulte al Area de Sistemas"
   CLOS DATA
   RETURN 
ENDIF

vUsuario = ALLTRIM(SUBSTR(SYS(0),1,6))

*- Mes y a¤o actual
SELECT parma

SEEK "MESANO"+"ACTUAL"
_actmes = LEFT(descri,6)
actmes = CTOD("01/"+SUBSTR(_actmes,5,2)+"/"+SUBSTR(_actmes,1,4) )
fmes = SUBSTR(_actmes,5,2)

*- Si obliga a cuadre y al total de suma de cuentas
*- en el documento
SEEK "OPERAT"+"SUMAS"
v_sumas = (LEFT(descri,2)="SI")
SEEK "OPERAT"+"CUADRE"
v_cuadre = (LEFT(descri,2)="SI")
SEEK "CONTAB"+"CIERRE"
MesCerrado = VAL(SUBSTR(Descri,5,2))
AnoCerrado = VAL(SUBSTR(Descri,1,4))

*- Inicia proceso
SELECT Comp
GO BOTTOM
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

RESTORE SCREEN FROM pta_comp
SHOW POPUP Menu

RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN

DEFINE WINDOW Wind_C0 FROM 00,00 TO 12,79 DOUBLE ;
TITLE Vmens01 FOOTER PADC(" ®F4¯ Imprime ÍÍÍÍÍÍÍÍÍÍÍÍÍÍ  ®F9¯ Detalle ",78,'Í') COLOR SCHEME 10

DEFINE WINDOW Wind_C2 FROM 13,00 TO 23,79 DOUBLE ;
TITLE Vmens09 COLOR SCHEME 10

DEFINE WINDOW Wind_C3 FROM 04,00 TO 23,58 DOUBLE ;
TITLE Vmens09 COLOR SCHEME 10

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
ON SELECTION PAD elimi  OF mMenu DO Elimina
ON SELECTION PAD lista  OF mMenu DO Lista
ON SELECTION PAD termi  OF mMenu DO termi
ACTIVATE SCREEN
 
RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_C0
CLEAR
@  0,02 SAY "   Documento:"
@  0,34 SAY "-"
@  0,41 SAY "Fecha:"
@  0,60 SAY "Estado:"
@ 01,02 SAY "       Glosa:" 
@ 02,02 SAY "       Glosa:" 
@ 03,02 SAY "       Glosa:" 
@ 04,02 SAY "       Glosa:"    
@ 05,02 SAY "Cadena Func.:"
@ 05,37 SAY "   FTE. FTO.:"
@ 06,02 SAY "C. Corriente:"

@ 08,02 SAY "  Total Debe:"

@ 08,41 SAY "Total Haber:"
RETURN

PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
ACTIVATE WINDOW Wind_C0
SELECT COMP
SCATTER MEMVAR
@ 0,16 SAY m.TipDoc
@ 0,19 SAY m.Comprob
@ 0,35 SAY m.ComAdi
@ 0,48 SAY m.Fecha
@ 0,68 SAY IIF(m.Estado="20","Contabilizado.","Pendiente")
@ 1,16 SAY SUBSTR(m.Glosa1,1,60)
@ 2,16 SAY SUBSTR(m.Glosa2,1,60)
@ 3,16 SAY SUBSTR(m.Glosa3,1,60)
@ 4,16 SAY SUBSTR(m.Glosa4,1,60) 
@ 5,16 SAY m.CodCad
@ 5,47 SAY m.codfte
@ 5,52 SAY val_para(m.CodFte,'CODFTE','Z',52,15)
@ 6,16 SAY m.CodCtc

@  8,16 SAY m.TotDeb  PICTURE "999,999,999.99"
@  8,54 SAY m.TotHab  PICTURE "999,999,999.99"
DO Vista_Hijo
RETURN

PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
ON KEY LABEL F9 DO Vista_Det
ON KEY LABEL F4 DO Lista
SCATTER MEMVAR
SELECT Diar
BROWSE;
	NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
	WINDOW Wind_C2 KEY m.Comprob + m.ComAdi + m.CodFte ;
	FIELDS;
		fecemi   :H='Emisi¢n',;
		Orden    :H='Orden',;
		coddep   :H='Depen',;
		cuenta   :H='  Cuenta',;
		vDebe    = IIF(tipo="D",montos,0) :H=" Debe M.Nac." :P="@Z 99,999,999.99",;
		vHaber   = IIF(tipo="H",montos,0) :H="Haber M.Nac." :P="@Z 99,999,999.99",;
		vProvee  = rAuxil( diar.tipaux,diar.registro ) :H="Proveedor" :30,;
		registro :H='C¢d.Aux.'
		
SELECT Comp
ON KEY LABEL F9 DO Vista_Det
ON KEY LABEL F4 DO Lista

RETURN

PROCEDURE  Vista_Det
*-------------------
HIDE POPUP ALL
ON KEY LABEL F9
ON KEY LABEL F4
SCATTER MEMVAR
SELECT Diar
BROWSE;
	NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT ;
	WINDOW Wind_C2 KEY m.Comprob + m.ComAdi ;
	FIELDS;
		item     :H='Item'       :R,;
		fecemi   :H='Emisi¢n'    :R,;
		cuenta   :H=' Cuenta'    :R,;
		tipo     :H='D/H'        :R,;
		montos   :H=' Importe S/. ':P='99,999,999.99':R,;
		vProvee  = rAuxil( diaR.tipaux,diaR.registro ) :H="Proveedor" :30,;
		diaR.tipaux   :H='TipAux.'    :R,;
		diaR.registro :H='C¢d.Aux. '  :R,;
		CodArt   :H='Art¡culo' :R,;
		Descri   :H='Descripci¢n' :R,;
		Codpart  :H='Partida' :R
		
SELECT Comp
DO VISTA
RETURN

FUNCTION rAuxil
*--------------
PARAMETERS vtipaux,vregistro
* Retorna la descripci¢n del proveedor,
* y a la vez actualiza una variable l¢gica AuxErr
* seg£n haya o no error en codificaci¢n del proveedor.

IF EMPTY(vtipaux) OR EMPTY(vRegistro)
*	STORE .T. TO auxerr
ENDIF   
AreaActual  = SELECT()
vret = SPACE(0)
SELECT Auxi
SEEK vTipAux+vRegistro
Existe      = FOUND()
*auxerr      = !FOUND()
IF Existe
	vRet   = Auxi.Descri
ENDIF
SELECT(AreaActual)
RETURN vRet

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
ON KEY LABEL F9
ON KEY LABEL F4

DEFINE WINDOW Wind_C4 FROM 00,00 TO 23,79 DOUBLE;
TITLE Vmens02 FOOTER PADC('Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar',78,'°') COLOR SCHEME 10
ACTIVATE WINDOWS Wind_C4

SELECT Comp
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
Vtemp = RECNO()
HIDE MENU mMenu

ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE IN WINDOW Wind_C4;
	NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
	TipDoc   :H= 'Tipo doc.',;
	Comprob  :H= 'Documento',;
	ComAdi   :H= 'CompAdi',;
	Glosa1   :H= '              Glosa',;
	Glosa2   :H= '              Glosa',;
	Totdeb   :H= '  Debe M.Nac.' :P= '999,999,999.99',;
	Tothab   :H= ' Haber M.Nac.' :P= '999,999,999.99'
	
IF LASTKEY()=27
	GO Vtemp
ENDIF
ON KEY LABEL F10
DEACTIVATE WINDOW Wind_C4
DO Vista
RETURN

PROCEDURE Busca              && Realiza b£squeda directa
*---------------
ON KEY LABEL F9
ON KEY LABEL F4
SELECT Comp
IF EOF()
	DO standby WITH VMens08
	RETURN
ENDIF
IF LASTKEY()=27
	RETURN
ENDIF

Vtemp = RECNO()
DEFINE WINDOW WBusca FROM 10,12 TO 16,68 DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW WBusca
STORE FMes      TO vM
STORE SPACE(02) TO vT
STORE SPACE(5)  TO Vbusca
STORE SPACE(02) TO Vbusca1
STORE SPACE(03) TO Vbusca2
@ 0, 0 SAY '                     Mes  : ' GET vM      PICTURE '!!' VALID val_para(vm,'FECMES',' ',29,25)
@ 1, 0 SAY '       Tipo de Documento  : ' GET vT      PICTURE '!!' VALID Val_para7(vT,'TIPDOC',' ',29,20)
@ 2, 0 SAY '    N£mero  de documento  : ' GET Vbusca  PICTURE REPLICATE('!',5)
@ 3, 0 SAY '        N£mero adicional  : ' GET Vbusca1 PICTURE '!!'  
READ 
RELEASE WINDOW WBusca
IF LASTKEY()=27
	SELECT comp
	RETURN
ENDIF

xComprob = PADR(ALLTRIM(vT) + ALLTRIM(Vbusca) + ALLTRIM(vM),14,' ')
xComAdi  = PADR(ALLTRIM(vBusca1),2,' ')
DocB     = ALLTRIM(xComprob+xComAdi)
IF EMPTY(vBusca) AND EMPTY(vBusca1) AND EMPTY(vBusca2)
	DocB = ALLTRIM(vT)
ENDIF

IF !SEEK(DocB)
	DO standby WITH "Dicho Documento No Fue Encontrado."
	GOTO Vtemp
 ELSE
	DO Vista
ENDIF
RETURN

PROCEDURE Anter              && Documento anterior
*--------------
SELECT Comp
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
SELECT Comp
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
ON KEY LABEL F4
ON KEY LABEL F9
SELECT Comp
IF EOF()
	DO standby WITH Vmens08
	UNLOCK ALL
	RETURN
ENDIF

ACTIVATE WINDOW Wind_C0
SCATTER MEMVAR
IF m.estado = '20'
	DO StandBy WITH 'El Documento ya ha sido procesado.'
	UNLOCK ALL
	RETURN
ENDIF
CLEAR
DO Pantalla
IF !f_lock(1)
	RETURN
ENDIF
DO WHILE .T.
	tcambia=.f.
	ON KEY LABEL F3 DO CorCom
	=v_tdoc()
	
	@  0,16 SAY m.TipDoc
	@  0,19 SAY m.Comprob
	@  0,35 SAY m.ComAdi
	@  0,48 GET m.Fecha
	@ 01,16 GET m.Glosa1 PICTURE "@S60"
	@ 02,16 GET m.Glosa2 PICTURE "@S60"
	@ 03,16 GET m.Glosa3 PICTURE "@S60"
	@ 04,16 GET m.Glosa4 PICTURE "@S60"
	@ 05,16 GET m.CodCaD 
	@ 05,50 GET m.CodFte PICTURE "99"  VALID VAL_PARA(m.CODFTE,'CODFTE',' ',50,15)
	READ
	
	xTipDoc = m.TipDoc
	
	IF YESNO("Tiene Cta.Cte")
		@ 06,16 GET m.CodCtc  PICTURE "@!" VALID Val_Fun("Caja","CodCTc","Descri",m.CodCTc,1,6,34)
		READ
	 ELSE
		m.codctc=SPACE(14)
		@ 06,16 SAY m.codctc
	ENDIF
	
	ON KEY LABEL F3
	
	IF LASTKEY()=27
		DO vista
		RETURN
	ENDIF
	* N/C normales
	m.estado="  "
	IF yesno("Confirme datos")
		EXIT
	ENDIF
ENDDO

IF tcambia=.F.
	SS= Tra_Hijo(1)     && Corrige items
	IF SS .AND. yesno("Confirme Datos Ingresados")
		SELECT parma
		IF !SEEK('OPERATMAYOR')
			DO Standby WITH 'Par metro de Mayor no existe'
			RETURN
		ENDIF
		IF !f_lock(1)
			RETURN
		ENDIF
		
		SELECT Parma          && Mayorizaci¢n no efectuada
		SEEK 'OPERATMAYOR'
		REPLACE Descri WITH IIF(MONTH(m.fecha) = MONTH(actmes);
				.AND. YEAR(m.fecha) = YEAR(actmes),'No',parma.Descri)
		
		*- Graba items del documento.
		SELECT Diax
		
		GO TOP
		
		SELECT Diar
		vOrd=ORDER()
		SEEK Diax.Comprob + Diax.ComAdi + Diax.Codfte
		SCAN WHILE Diax.Comprob + Diax.ComAdi + Diax.CodFte = Diar.Comprob + Diar.ComAdi + Diar.Codfte
			IF !f_lock(1)
				UNLOCK ALL
				RETURN
			ENDIF
			SELECT Diar
			IF RLOCK()
				DELETE NEXT 1
			ENDIF
			SELECT Diar
		ENDSCAN
		
		* Se actualiza
		SELECT diax
		DO WHILE !EOF()
			SCATTER MEMVAR
			SELECT Diar
			IF f_appd()
				GATHER MEMVAR
			ENDIF
			SELECT Diax
			SKIP
		ENDDO
		
		SELECT Comp
		m.TipDoc = xTipDoc
		GATHER MEMVAR
	 ELSE
		DO standby WITH "Error en digitaci¢n"
	ENDIF
	
	SELECT Diax
	ZAP
	*----------------
	*SCATTER MEMVAR BLANK
	*-------------------
ENDIF

SELECT Comp
UNLOCK ALL
DO Vista
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
ON KEY LABEL F4
ON KEY LABEL F9
ACTIVATE WINDOW Wind_C0
vreg = RECNO()  
SELECT Comp
SCATTER MEMVAR BLANK
m.fecha=ActMes
DO WHILE .T.
	_msg2  = ""
	@ 08,16 SAY REPLICATE(' ',20)
	@ 08,54 SAY REPLICATE(' ',20)
	@  0,16 SAY REPLICATE(" ",22)
	@  0,16 GET m.TipDoc      VALID v_Tdoc() .AND. Asigna()
	@  0,19 GET m.Comprob     VALID v_comp();
								MESSAGE 'El N£mero del Comprobante  debe Termiar en el N£mero de Mes a Procesar' ;
								ERROR _msg2
	@  0,35 GET m.ComAdi      VALID No_Dupli(m.comprob,m.comadi) ERROR _Msg2
	@  0,38 GET m.Fecha       VALID vfecAct() .AND. v_comp1() .AND. (!m.fecha < m.actmes) ERROR _msg2
	@ 01,16 GET m.Glosa1      PICTURE "@S60"
	@ 02,16 GET m.Glosa2      PICTURE "@S60"
	@ 03,16 GET m.Glosa3      PICTURE "@S60"
	@ 04,16 GET m.Glosa4      PICTURE "@S60"
	@ 05,16 GET m.CodCal      VALID !EMPTY(m.CodCal)
	@ 05,50 GET m.CodFte PICTURE "99"  VALID VAL_PARA(m.CODFTE,'CODFTE',' ',50,15)
	
	@ 00,37 SAY REPLICATE(" ",43)
	@ 06,22 SAY REPLICATE(' ',54)
	@ 07,22 SAY REPLICATE(' ',54)
	@ 08,16 SAY REPLICATE(' ',10)
	@ 08,54 SAY REPLICATE(' ',10)
	@ 09,22 SAY REPLICATE(' ',54)

	READ
	
	IF LASTKEY()=27
		DO STANDBY WITH "Proceso Cancelado. No se graba el Documento"
		GOTO vreg
		DO Vista
		RETURN
	ENDIF
	
	IF YESNO("Tiene Cta.Cte")
		@ 06,16 GET m.CodCtc  PICTURE "@!" VALID Val_Fun("Caja","CodCTc","Descri",m.CodCTc,1,6,34)
		READ
	 ELSE
		m.codctc=SPACE(14)
		@ 06,16 SAY m.codctc
	ENDIF

	IF LASTKEY()=27
		DO STANDBY WITH "Proceso Cancelado. No se graba el Documento"
		GOTO vreg
		DO Vista
		RETURN
	ENDIF
	
	* Notas/Contabilidad Normales
	m.estado="  "
	
	IF yesno("Confirme datos del documento")
		EXIT
	ENDIF
ENDDO

SELECT diax
m.Item = 0
redonerr  = .F.
_msg1     = " "
_msg2     = " "

DO agreg_item
SS= Tra_Hijo(2)     && 2 indica que es documento nuevo.
lAppd = .F. 

*aqui me quedo

IF SS .AND. yesno("¨Graba la Informaci¢n?")
	SELECT Parma
	IF !SEEK('OPERATMAYOR')
		DO Standby WITH 'No existe Par metro de Mayor'
		RETURN
	ENDIF
	IF !f_lock(1)
		RETURN
	ENDIF
	SELECT Comp
	IF f_appd()
		SELECT Parma
		SEEK 'CORREL' + 'NOTCON'
		REPLACE NumEnt WITH NumEnt + 1
		SEEK 'OPERATMAYOR' &&=Si &&AJUSTE POR DIFERENCIA
		REPLACE Descri WITH IIF(MONTH(m.fecha) = MONTH(actmes);
				.AND. YEAR(m.fecha) = YEAR(actmes),'No',parma.Descri)

		SELECT Comp
		
		GATHER MEMVAR
		REPLACE FecOpe  WITH DATE()
*		REPLACE Usuario WITH LOGIN()
		
		SELECT Diax
		GO TOP
		
		SCAN
			SCATTER MEMVAR
			SELECT Diar
			IF f_appd()
				GATHER MEMVAR
				lAppd=.T.
			ENDIF
			SELECT Diax
		ENDSCAN
	ENDIF
	
	SELECT Diar
	SEEK Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.CodCal,5,3)
	
 ELSE
	DO standby WITH "Error en digitaci¢n"
ENDIF

SELECT Diax
ZAP
*------------
SCATTER MEMVAR BLANK
*-----------
SELECT Diar
UNLOCK ALL

SELE Diar
SET ORDER TO Diario11

SELE COMP
SET ORDER TO Compro7

DO vista
RETURN



PROCEDURE CorCom
*---------------
* Permite corregir el Tipo de documento.

ON KEY LABEL F3
_msg2    = ""
vTipDoc  = m.TipDoc
vComprob = m.Comprob
vComAdi  = m.ComAdi
vCodCal  = m.CodCal 
vReg     = RECNO()

DEFINE WINDOW WCambia FROM 10,12 TO 15,68 DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW WCambia
@ 0,2 SAY 'Tipo de Documento :' GET m.TipDoc      VALID Val_para7(m.Tipdoc,'TIPDOC',' ',22,15);
                               .AND. Asigna1()
@ 1,2 SAY '        Documento :' GET m.Comprob     VALID v_comp();
                         ERROR _msg2
@ 2,2 SAY '        Adicional :' GET m.ComAdi
@ 3,2 SAY ' Codigo  Proyecto :' GET m.CodCal

READ
RELEASE WINDOW WCambia
IF LASTKEY() # 27
	SELECT Comp
	GO vReg
	REPLACE TipDoc   WITH m.TipDoc
	REPLACE Comprob  WITH m.Comprob
	REPLACE ComAdi   WITH m.ComAdi
	REPLACE Codcal  WITH m.CodCal
	SELECT Diar
	DO WHILE .T.
		IF SEEK(vComprob+vComAdi)
			REPLACE Diar.Comprob WITH m.Comprob
			REPLACE Diar.ComAdi  WITH m.ComAdi
			REPLACE Diar.Codfte  WITH m.CodFte
		 ELSE
			EXIT
		ENDIF
	ENDDO
	tcambia=.T.
	SELECT Diar
	SEEK m.Comprob + m.ComAdi + m.CodFte
	SELECT Comp
 ELSE
	SELECT Comp
	GO vReg
ENDIF
RETURN

FUNCTION Asigna1
*---------------
IF m.TipDoc # vTipDoc
	xcomprob=SUBSTR(m.Comprob,3,14)
	m.Comprob = alltrim(m.tipdoc)+ xcomprob
ENDIF
RETURN .T.

FUNCTION v_comp
*--------------
IF LEFT(m.comprob,2)<>ALLTRIM(m.tipdoc)
	_MSG2 = "Digitos iniciales INCORRECTOS"
	RETURN .f.
ENDIF

IF LEN(ALLTRIM(m.comprob)) <= 8
	_MSG2 = "N£mero de Documento errado"
	RETURN .f.
ENDIF

_rec = IIF(EOF(),-1,RECNO())

IF _rec>0
	GO _rec
 ELSE
	GO BOTTOM
ENDIF

RETURN


FUNCTION Tra_Hijo
*----------------
PARAMETER cor_ing       && cor_ing = 1 correccion carga data antigua
PUBLIC valido
HIDE MENU mMenu

Tit = "²² F2:Edita  F5:Agrega Item  F7:TipFin  F8:Borra   F10:Sale ²²"

ON KEY LABEL F2  DO Edicion
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F10 DO sal_hijo

IF cor_ing = 1    && Correccion, carga data antigua
	m.Item = 0
	SELECT Diar
	SEEK m.Comprob + m.ComAdi + m.Codfte
	SCAN WHILE Diar.Comprob + Diar.ComAdi + Diar.Codfte = m.Comprob + m.ComAdi + m.Codfte
		SCATTER MEMVAR
		SELECT Diax
		APPEND BLANK
		GATHER MEMVAR
		Sele Diar
	ENDSCAN
ENDIF

SELECT diax

BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
	TITLE Tit FIELDS;
	Orden    :H='ORDEN':R,;
	item     :H='Item'       :R,;
	fecemi   :H='Emisi¢n'    :R,;
	cuenta   :H=' Cuenta'    :R,;
	tipo     :H='D/H'        :R,;
	montos   :H=' Importe S/. ':P='99,999,999.99':R,;
	vProvee  = rAuxil( diaX.tipaux,diaX.registro ) :H="Proveedor" :30,;
	diax.tipaux   :H='TipAux.'    :R,;
	diax.registro :H='C¢d.Aux. '  :R,;
	codfte   :H='codfte' :R,;
	CodArt   :H='Art¡culo' :R,;
	Descri   :H='Descripci¢n' :R,;
	Codpart  :H='Partida' :R
	
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10

IF LASTKEY()=27
	DO standby WITH " Cancela Ingreso "
	RETURN .F.
ENDIF
SELECT comp
SHOW MENU mMenu
RETURN valido

FUNCTION Edicion
*---------------
ACTIVATE WINDOW Wind_C3
*- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10

SCATTER MEMVAR

@  0,1  SAY '    Item:' + STR(m.Item,4)
@  1,1  SAY ' Fec.Emi:' GET m.FecEmi    WHEN .F.
@  2,1  SAY '  Cuenta:' GET m.Cuenta    VALID   v_cta() ERROR   _msg1 PICTURE '!!!!!!!!!!'
@  3,1  SAY '     D/H:' GET m.Tipo      PICTURE '@M D,H' 
@  4,1  SAY ' Imp.S/.:' GET m.Montos    PICTURE '999,999,999,999.99'
@  5,1  SAY '   Orden:' GET m.orden		VALID   v_Ord()
@  6,1  SAY 'Tip.Aux.:' GET m.TipAux    VALID   v_aux1()  ERROR   "Dato errado!!!"
@  7,1  SAY 'C¢d.Aux.:' GET m.Registro  VALID   V_AUX()
@  8,1  SAY '  DocRef:'
@  8,21 SAY '-'
@  8,11 GET m.Tipdcr  PICTURE "!!" VALID vv_Tdoc()
@  8,14 GET m.DocRef    PICTURE "!!!!!!!"  WHEN !EMPTY(m.TipDcr)
@  8,22 GET m.DocAdi    VALID Valida() PICTURE "!!" WHEN !EMPTY(m.TipDcr)
@  9,1  SAY 'Mes-Per. DocRef: ' GET  m.Nummesref    PICTURE "!!" WHEN !EMPTY(m.TipDcr)
@  9,21 SAY '-'
@  9,22 GET  Periodoref   PICTURE "!!" WHEN !EMPTY(m.TipDcr)
@ 10,1  SAY '  CtaCte:' GET m.CodCtc
@ 11,1  SAY 'N.Cheque:' GET m.NumChq    PICTURE "!!!!!!!!!!!!!!"
@ 12,1  SAY 'Cod.Dep.:'
@ 12,11 GET m.CodDep PICTURE '!!!!!!' VALID Val_para(m.CodDep,'CODDEP','W',11,30,7)
READ VALID VAL_READ()

SELECT Diax
IF LASTKEY() = 27
	DO standby WITH " Cancela Ingreso "
 ELSE
	GATHER MEMVAR FIELDS EXCEPT ITEM,TIPDOC
ENDIF

DO regreg

DEACTIVATE WINDOW WIND_C3

ON KEY LABEL F2  DO Edicion
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F10 DO sal_hijo
RETURN .F.


FUNCTION v_Tdoc
*--------------
IF EMPTY(m.TipDoc)
	vAli=ALIAS()
	SELECT Parma
	vOrd=ORDER()
	SET INDEX TO (vIndPar)
	DEFINE POPUP parametro FROM 0,40 PROMPT FIELD ALLTRIM(CodigoAUX)+' '+Descri TITLE ' Tipos de Documento '
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	RELEASE POPUP parametro
	SET ORDER TO vOrd
	m.TipDoc=ALLTRIM(Parma.CodigoAux) &&SEGUNDO CODIGO DE PARMAE
	SELECT (vAli)
 ELSE
	vAli=ALIAS()
	SELECT Parma
	SET FILTER TO Tipo='TIPDOC'
	LOCATE FOR TIPO='TIPDOC' AND ALLTRIM(CODIGOAUX)=ALLTRIM(m.Tipdoc) 
	IF !FOUND()
		SET FILTER TO
		SELECT (vAli)
		RETURN .F.
	ENDIF
	SET FILTER TO
	SELECT (vAli)
ENDIF
RETURN


PROCEDURE Indexa_Par
*-------------------
SELECT parma
SET ORDER TO
INDEX ON CodigoAux TO (vIndPar) FOR parma.tipo='TIPDOC' AND !EMPTY(parma.codigoaux)
SET INDEX TO
SET ORDER TO Parmae1
RETURN .T.

FUNCTION v_cta
*-------------
* Muestra el tipo de auxiliar posible.
* Verifica que la cuenta sea de detalle y no sea autom tica.
* Si la cuenta no existe permite agregarla en el plan de cuentas.
*----------------------------------------------
PRIVATE mAlias
mAlias = ALIAS()
SELECT Cuen
_f_ = SEEK(m.Cuenta) .AND. !EMPTY(m.Cuenta)
@ 2,22 SAY SUBSTR(Cuen.Descri,1,35)
IF _f_ .OR. EMPTY(m.Cuenta)
	IF EMPTY(m.Cuenta)
		SELECT Cuen
		SET ORDER TO TAG Cuentas4
		_f_ = Val_Fun('Cuen','Cuenta',"Cuenta+' '+Descri",m.Cuenta,1,2,22,'SUBSTR(Descri,1,35)')
		SET ORDER TO TAG Cuentas1
		SELECT (mAlias)
		IF !_f_
			_msg1 = "Indique el n£mero de cuenta!!!"
			RETURN .F.
		ENDIF
	ENDIF
	
	SELECT (mAlias)
	IF UPPER(Cuen.Detalle)  # 'S'
		_msg1 = "Cuenta no es de Detalle"
		RETURN .F.
	ENDIF
	
ENDIF

RETURN _f_

FUNCTION v_aux1
*--------------
vreturn = .F.
IF !EMPTY(m.Tipaux)
	IF SEEK('AUXIL ' + m.TipAux,'Parma')
		@ 6,14 SAY SUBSTR(Parma.Descri,1,30)
		vreturn = .T.
	 ELSE
		vreturn = Val_para(m.Tipaux,'AUXIL ',' ',8,30)
	ENDIF
 ELSE
	vreturn = Val_para(m.Tipaux,'AUXIL ',' ',8,30)
ENDIF

RETURN (vreturn)


FUNCTION v_aux
*-------------
resx    = .F.

IF !EMPTY(m.TipAux)
	IF !EMPTY(m.registro)
		IF SEEK(m.TipAux + m.registro,'Auxi')
			@ 7,17 SAY SUBSTR(Auxi.Descri,1,30)
			resx = .T.
		 ELSE
			resx = VAL_AUX(m.registro,m.TipAux,'  ',11,30)
			IF !EMPTY(m.registro)
				@ 7,17 SAY SUBSTR(Auxi.Descri,1,30)
				resx = .T.
			 ELSE
				@ 7,17 SAY SPACE(30)
				resx = .F.
			ENDIF
		ENDIF
	 ELSE
		resx = VAL_AUX(m.registro,m.TipAux,'  ',11,30)
	ENDIF
ENDIF
RETURN (resx)

FUNCTION vv_Tdoc
*----------------
IF !EMPTY(m.TipDcr)
	vAli=ALIAS()
	SELECT Parma
	SET INDEX TO (vIndPar) 
	SEEK ALLTRIM(m.Tipdcr)
	IF !FOUND()
		DEFINE POPUP parametro FROM 0,40 PROMPT FIELD ALLTRIM(CodigoAUX)+' '+Descri TITLE ' Tipos de Documento '
		ON SELECTION POPUP parametro DEACTIVATE POPUP
		ACTIVATE POPUP parametro
		RELEASE POPUP parametro
		m.TipDcr=ALLTRIM(Parma.CodigoAux)	&&SEGUNDO CODIGO DE PARMAE
	ENDIF
	SET ORDER TO PARMAE1
	SELECT (vAli)
ENDIF

mverifd=ALLTRIM(Parma.descriau2)
RETURN

* Valida la existencia de la cuenta.
* VERIFICAR Y ANALIZAR PARA QUE SIRVE ESTA FUNCIàN
FUNCTION Valida
*--------------
 PUBLIC vMonD,vMonH
 STORE 0 TO vMonD,vMonH
 vRec=RECNO()
 vOrd=ORDER()
 vAli=ALIAS()
 SELECT Diar
 vRec11=RECNO()
 vOrd11=ORDER()
 vAli11=ALIAS()
 vComprob =Diax.DocRef
 vComAdi  =Diax.DocAdi
 vCuenta  =Diax.Cuenta
 vTipAux  =Diax.TipAux
 vRegistro=Diax.Registro
 SET ORDER TO TAG Diario9
 SEEK vComprob+vComAdi+vCuenta+vTipAux+vRegistro
 IF FOUND() .AND. Diar.Tipo#Diax.Tipo
    IF Diar.Estado = '50'
       DO StandBy WITH "EL documento ya fue !! Cancelado !!"
    ENDIF     
    IF Diar.SalDPen > 0 .OR. Diar.SalDPen = 0
       IF Diar.SalDPen > 0
          IF Diar.Montos > 0 .OR. Diar.Montos < 0
             REPLACE Diar.SalDPen WITH Diar.Montos - (Diax.Montos + Diar.SalDPen)
             IF Diar.SalDPen=0
                REPLACE Diar.Estado WITH '50'
             ENDIF
          ENDIF
          IF Diar.Montos = 0
             REPLACE Diar.SalDPen WITH Diar.SalDPen - Diax.Montos
             IF Diar.SalDPen=0
                REPLACE Diar.Estado WITH '50'
             ENDIF
          ENDIF
       ELSE
          IF Diar.SalDPen = 0
             IF Diar.Montos = 0
                REPLACE Diar.SalDPen WITH 0
             ELSE
                REPLACE Diar.SalDPen WITH Diar.Montos - Diax.Montos
                IF Diar.SalDPen=0
                   REPLACE Diar.Estado WITH '50'
                ENDIF
             ENDIF
          ENDIF
       ENDIF
    ELSE
       IF Diar.SalDPen < 0
          IF Diar.Montos=0
             REPLACE Diar.SalDPen WITH Diar.Montos - Diax.Montos
             IF Diar.SalDPen=0
                REPLACE Diar.Estado WITH '50'
             ENDIF
          ELSE
             REPLACE Diar.SalDPen WITH Diar.Montos - Diax.Montos  &&Saldo Debe
             IF Diar.SalDPen=0
                REPLACE Diar.Estado WITH '50'
             ENDIF
          ENDIF
       ENDIF
       IF Diar.Montos=Diax.Montos
          REPLACE Diar.Estado WITH '50'   &&Asiento Cancelado
          REPLACE Diar.SalDPen WITH 0
       ENDIF
    ENDIF
    IF Diar.SalDPen > 0 .OR. Diar.SalDPen < 0
       REPLACE Diar.Estado WITH '00'   &&Doc.Pendiente de cancelaci¢n
    ELSE
       REPLACE Diar.Estado WITH '50'   &&Asiento Cancelado
    ENDIF
 ENDIF
 SELECT (vAli11)
 SET ORDER TO TAG vOrd11
 SELECT Diax
 SET ORDER TO TAG vOrd
 GO vRec
 RETURN

PROCEDURE regreg
*------------------
IF yesno("Desea Actualizar Todos los Items con el mismo Auxiliar ?")  
	* Se Actualiza el auxiliar    
	SELE Diax
	REPLACE ALL diax.tipaux   WITH m.tipaux
	REPLACE ALL diax.registro WITH m.registro
*	REPLACE ALL diax.docref   WITH wdocref
*	REPLACE ALL diax.docadi   WITH wdocadi
*	REPLACE ALL diax.nummesref  WITH wnummesref
*	REPLACE ALL diax.Periodoref WITH wperiodoref
ENDIF  
RETURN

PROCEDURE Agreg_item
*-------------------

 SELECT Diax
 ***************
 m.EstCon = "55"
 ***************
 m.FecEmi = m.Fecha
 m.Cuenta = SPACE(10)
 m.Item   = m.Item + 1
 m.Tipo   = 'D'
 m.CodDep = '112000'
 m.Montos = 0.00
 IF f_Appd()
 	GATHER MEMVAR
 ENDIF
 DO Edicion
RETURN


PROCEDURE Elimi_item
*-------------------
* Elimina el registro
* En caso que el registro tenga una provisi¢n verifica que ‚sta
* no tenga cancelaciones (sea en el mismo o en otro documento).
* Si se borra el registro debe recalcular el correlativo de registro
* para el documento.

IF yesno("Confirme ELIMINACION de Item")
	* Recalcula totales del documento
	SELECT Diax
	IF diax.tipo='D'
		m.TotDeb = m.TotDeb - Diax.Montos
	 ELSE
		m.TotHab = m.TotHab - Diax.Montos
	ENDIF
	
	* Borra el registro y recalcula el correlativo.
	SELECT Diax
	m.Item = Diax.Item - 1
	DELETE
	SKIP
	vreg = IIF(!EOF(),RECNO(),0)
	DO WHILE !EOF()
		m.Item = m.Item + 1
		REPLACE Item WITH m.Item
		SKIP
	ENDDO
	IF vreg # 0
		GO vreg
	 ELSE
		GO BOTTOM
	ENDIF
ENDIF
RETURN

PROCEDURE sal_hijo
*-----------------
* Valida los totales para salir:
* la suma de cuentas
* los cargos con los abonos
* Verifica que no existan descuadres por redondeos; si lo hubieran
* permite ajustar estos redondeos regresando a la rutina Tra_Hijo.

valido    = .T.      && utilizada por tra_hijo.
redonerr  = .F.      && indica si existe error por redondeo.
rec       = IIF(EOF(),-1,RECNO())
STORE 0 TO m.totdeb, m.tothab
STORE 0 TO xtotdeb,xtothab

SELECT Diax
idxaux = SYS(3) + '.IDX'
INDEX ON STR(nummov,3)+STR(item,4) TO (idxaux)
DEFINE WINDOW _yy FROM 10,15 TO 16,65 COLOR SCHEME 10 TITLE " Valida montos por movimientos "

xnummov = nummov
STORE 0 TO xtotdeb, xtothab
vReg    = RECNO()
SUM diax.montos FOR diax.tipo ="H"  TO xtothab
GO TOP

SUM diax.montos FOR diax.tipo ="D"  TO xtotdeb
GO TOP
IF xtotdeb # xtothab
	ACTIVATE WINDOW _yy
	@ 1,1  SAY "Mov. "+STR(xnummov,3) + " no cuadra "
	WAIT ''
ENDIF
RELEA  WINDOW _yy
SET INDEX TO
ERASE (idxaux)
IF redonerr
	RETURN
ENDIF
m.totdeb = xtotdeb
m.tothab = xtothab
IF rec<0
	GO BOTTOM
 ELSE
	GO rec
ENDIF
IF m.totdeb # m.tothab
	IF !v_cuadre
		SET CONSOLE ON
		SET BELL TO 130,5
		_WRAP = .F.
		? CHR(7)
		_WRAP = .T.
		SET BELL TO 512,2
		SET CONSOLE OFF
		DO standby WITH "No CUADRAN los cargos con los abonos"
		valido = .T.
	 ELSE
		SET CONSOLE ON
		SET BELL TO 130,5
		_WRAP = .F.
		? CHR(7)
		_WRAP = .T.
		SET BELL TO 512,2
		SET CONSOLE OFF
		DO standby WITH "No CUADRAN los cargos con los abonos"
		valido = .F.
	ENDIF
	IF !valido
		IF !YesNo('¨Acepta el Error? <NO> = Corrige')
			RETURN
		ENDIF
		valido = !valido
	ENDIF
ENDIF

KEYBOARD CHR(23)
RETURN

FUNCTION no_dupli
*----------------
*- Esta funci¢n no permite la duplicidad en el n£mero de comprobante
PARAMETERS v_Com, vAdi
vMens = "N£mero de Documento YA EXISTENTE."
vAli  = ALIAS()
vExpr = PADR(ALLTRIM(v_com),14,' ') + PADR(ALLTRIM(vAdi),2,' ')
SELECT comp
SEEK vexpr
enc   = FOUND()
SELECT(vAli)
IF FOUND()
   * DO STANDBY WITH vMens
   _msg2 = vMens
ENDIF
RETURN !FOUND()


FUNCTION vFecAct
*---------------
IF !EMPTY(m.fecha)
	vret = (MONTH(m.Fecha)>MesCerrado AND YEAR(m.Fecha)=AnoCerrado) OR YEAR(m.Fecha)>AnoCerrado
	IF !vret
		_msg2 = "­­­ Dicho mes ya est  cerrado !!!" 
	ENDIF
	RETURN vret
ENDIF
RETURN .T.

FUNCTION v_comp1
*---------------
PRIVATE __fun,__fun1
SET ORDER TO TAG Compro7

  _rec = IIF(EOF(),-1,RECNO())

  __fun = !SEEK(m.comprob+m.comadi+ALLTRIM(STR(MONTH(m.fecha)))+ALLTRIM(m.CodCal))
 
 _msg2 = "Documento YA EXISTE"

 __fun1= !m.fecha < m.actmes
 IF !__fun1
    _msg2 = "El a¤o/mes no es el correcto"
 ENDIF
 
 If RIGHT(ALLTRIM(m.comprob),2) <> PADL(ALLTRIM(STR(MONTH(m.fecha),2)),2,'0')
    _msg2 = "El mes de la fecha no coincide con la numeraci¢n final del comprobante"
    RETURN .F.
 ENDIF
 
  
 IF _rec>0
    GO _rec
 ELSE
    GO BOTTOM
 ENDIF
 
SET ORDER TO
m.codcal=SUBSTR(STR(YEAR(m.fecha),4),3,2)+PADL(ALLTRIM(STR(MONTH(m.fecha),2)),2,'0')+'RDR'+SPACE(12)
RETURN __fun


FUNCTION Asigna
*--------------
vlen      = LEN(m.Comprob)
*xcomprob=SUBSTR(m.Comprob,3,14)
SELECT Parma
SEEK 'CORREL'+'NOTCON'
xcomprob=PADL(ALLTRIM(STR(Parma.NumEnt+1,5)),5,'0')
m.Comprob = m.tipdoc + xcomprob+fmes
SELECT  Comp 
*WAIT WINDOW 'El N£mero del Comprobante  debe Terminar en el N£mero de Mes a Procesar' 
RETURN .T.

PROCEDURE Elimina
*----------------
* Anula el documento actual.
* Solo se puede anular si no tiene provisiones que se cancelen
* en otros documento o si el documento no se ha cerrado.

IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF

IF Comp.fecha < m.actmes
	DO standby WITH "Documento no es del mes actual"
	RETURN
ENDIF

IF Comp.estado = '20'
	DO standby WITH vMens20
	UNLOCK ALL
	RETURN
ENDIF


IF yesno("¨ANULA Documento?")
	vErdad=.T.
	vOrd=ORDER()
	SELECT Comp
	IF f_lock(1)
		*-Busca y elimina los Items
		SELECT Diar
		SEEK Comp.Comprob+Comp.ComAdi+Comp.CodFte
		IF FOUND()
			SCAN WHILE Comp.Comprob+Comp.ComAdi+Comp.CodFte = Diar.Comprob+Diar.ComAdi+Diar.CodFte
				IF f_lock(1)
					DELETE
				ENDIF
			ENDSCAN
		ENDIF
		SELECT Comp
		DELETE
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


PROCEDURE Lista
*--------------
*- Listado de Notas de Contabilidad
DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE 'Listado de Notas de Contabilidad' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vLista
vtipo = SPACE(2)
STORE DATE() TO xf1, xf2
SELECT Comp
rec = RECNO()
IF LASTKEY()=27
	RETURN
ENDIF

@ 01,01 SAY "         Documentos: "
@ 01,22 GET vLista FUNCTION '^ Este Documento;Por Tipo de Doc.;Todos'
@ 04,01 SAY "  Tipo de Documento: "
@ 04,22 GET vtipo  WHEN vlista=2 PICTURE '!!' VALID Val_para7(vTipo,'TIPDOC',' ',22,20)
@ 06,01 SAY "        Fecha Desde: "
@ 06,22 GET xf1 WHEN vlista<>1
@ 08,01 SAY "        Fecha Hasta: "
@ 08,22 GET xf2 WHEN vlista<>1 VALID(xf2>=xf1) ERROR "Debe ser >= al 1ro"
@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
	vDbf = SYS(3)+'.Dbf'
	vIdx = SYS(3)+'.Idx'
	vDbf1 = SYS(3)+'.Dbf'
	vIdx1 = SYS(3)+'.Idx'
	SELECT comp
	COPY STRUCTURE TO (vDbf)
	SELECT 0
	USE (vDbf) ALIAS t_com EXCL
	SELECT Diar
	COPY STRUCTURE TO (vDbf1)
	SELECT 0
	USE (vDbf1) ALIAS t_Dia EXCL
	INDEX ON Comprob + comadi + codfte + cuenta + tipo TO (vIdx1)
	
	SELECT Comp
	rec = RECNO()
	SET RELATION TO Comp.Comprob+Comp.ComAdi+Comp.CodFTE INTO Diar
	SET SKIP TO Diar
	
	DO CASE
		CASE vlista=1                        && Este documento
			SELECT comp
			SCATTER MEMVAR
			SELECT t_com
			=f_appd()
			GATHER MEMVAR
			SELECT diar
			vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
			SCAN WHILE vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
				SCATTER MEMVAR
				SELECT T_dia
				SEEK Diar.Comprob + Diar.comadi + Diar.codfte + LEFT(diar.cuenta,3)+diar.tipo
				IF !FOUND()
					=f_appd()
					GATHER MEMVAR
				 ELSE
					REPLACE t_dia.Montos    WITH t_dia.montos + m.montos
				ENDIF
				SELECT diar
				vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
			ENDSCAN
			
			SELECT t_dia
			SET INDEX TO
			INDEX ON COMPROB+COMADI+CODFTE+tipo+CUENTA TO (vIdx1)
			GO TOP
			SELECT T_Com
			SET RELATION TO Comprob+ComAdi+CodFTE INTO t_Dia
			GO TOP
			SELECT t_dia
			IF !EOF()
				DO REPORTE WITH 2,"NOTA1"," Listado Nota de Contabilidad "
			 ELSE
				DO STANDBY WITH 'No hay registros para procesar.'
			ENDIF
		CASE vlista=2                        && Rango x Fechas y Tipo
			DO ESPERA WITH 1,'Notas de Contabilidad en Proceso ... '   
			SELECT comp
			INDEX ON Comp.Comprob+Comp.ComAdi+Comp.codfte TO (vidx) FOR SUBSTR(Comp.Comprob,1,2)=ALLTRIM(vtipo) AND BETWEEN(Comp.Fecha,xf1,xf2) .AND. ESTADO#"55" .AND. ESTADO#"99"
			
			*- Se trasladan estos documentos a los archivos temporales:
			SELECT comp
			GO TOP
			nr = 0
			DO WHILE !EOF()
				SCATTER MEMVAR
				nr=nr+1
				@ 0,0 SAY alltrim(str(nr))
				SELECT t_com
				=f_appd()
				GATHER MEMVAR
				
				SELECT diar
				vDoc = Comp.COMPROB+Comp.COMADI+CodFte
				SCAN FOR vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
					SCATTER MEMVAR
					SELECT T_dia
					SEEK Diar.Comprob + Diar.Comadi + Diar.CodFte + LEFT(diar.cuenta,3)+diar.tipo
					IF !FOUND()
						=f_appd()
						GATHER MEMVAR
					 ELSE
						REPLACE t_dia.Montos    WITH t_dia.montos + m.montos
					ENDIF
					SELECT diar
					vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
				ENDSCAN
				
				SELECT comp
				SKIP
			ENDDO
			DO ESPERA WITH 2
			
			SELECT t_dia
			SET INDEX TO
			INDEX ON COMPROB+COMADI+CODFTE+t_dia.tipo+t_DIA.CUENTA TO (vIdx1)
			
			SELECT T_Com
			INDEX ON COMPROB+COMADI+CodFte TO (vIdx)
			
			SELECT t_dia
			SET RELATION TO Comprob+ComAdi+Codfte INTO T_com
			set skip to t_com
			GO TOP
			IF !EOF()
			SUSP
				DO REPORTE WITH 2,"NOTA1","Notas de Contabilidad por Tipo de Documento"
			 ELSE
				DO STANDBY WITH 'No hay registros para procesar.'
			ENDIF
			
		CASE vlista=3
			DO ESPERA WITH 1,' Espere un momento; Procesando Informaci¢n  '   
			INDEX ON Comp.Comprob+Comp.ComAdi+Comp.codfte TO (vIdx) FOR BETWEEN(Comp.Fecha,xf1,xf2)
			
			*- Se trasladan estos documentos a los archivos temporales:
			SELECT comp
			GO TOP
			nr = 0
			DO WHILE !EOF('comp')
				SCATTER MEMVAR
				nr=nr+1
				@ 0,0 SAY alltrim(str(nr))
				SELECT t_com
				=f_appd()
				GATHER MEMVAR
				
				SELECT diar 
				vDoc = Comp.COMPROB+Comp.COMADI+CodFte
				SCAN FOR vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
					SCATTER MEMVAR
					SELECT T_dia
					SEEK Diar.Comprob + Diar.Comadi + Diar.CodFte + diar.cuenta+diar.tipo
					IF !FOUND()
						=f_appd()
						GATHER MEMVAR
					 ELSE
						REPLACE t_dia.Montos    WITH t_dia.montos + diar.montos
					ENDIF
					SELECT diar
					vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
				ENDSCAN
				
				SELECT comp
				SKIP
			ENDDO
			DO ESPERA WITH 2
			
			SELECT t_dia
			SET INDEX TO
			INDEX ON COMPROB+COMADI+CODFTE+t_dia.tipo+t_DIA.CUENTA TO (vIdx1)
			
			SELECT T_Com
			INDEX ON COMPROB+COMADI+subs(codcal,5,3) TO (vIdx)
			
			SELECT t_dia
			SET RELATION TO Comprob+ComAdi+Codfte INTO T_com
			set skip to t_com
			GO TOP
			IF !EOF()
				DO REPORTE WITH 2,"NOTA1","Notas de Contabilidad"
			 ELSE
				DO STANDBY WITH 'No hay registros para procesar.'
			ENDIF
			
	ENDCASE
	SELECT comp
	SET INDEX TO
	USE IN t_Dia
	USE IN t_Com
	ERASE (vDbf)
	ERASE (vIdx)
	ERASE (vDbf1)
	ERASE (vIdx1)
ENDIF

SELECT COMP
SET RELATION OFF INTO DIAR
SELE Comp
SET ORDER TO tag Compro7
SELE Diar
SET ORDER TO tag Diario11
SELECT COMP
GO rec
DO vista
RETURN

FUNCTION Inic_tot
*-----------------
*- Inicializa las variables para los totales por documento
STORE 0 TO DtotalDoc,HtotalDoc
RETURN ' '

FUNCTION busca_cta
*----------------
PARAMETERS vcuenta 
vAlias = SELECT()
SELECT Cuen
SEEK &vcuenta 
IF FOUND()
   SELE(VAlias)
   RETURN  ALLTRIM(Cuen.descri) 
ELSE
   SELE(VAlias)
   RETURN  'Cuenta no Existe'    
ENDIF
RETURN 

FUNCTION t_deb
*-------------
area = SELECT()
SELECT T_Dia
vDoc  = t_dia.comprob + t_dia.comadi + t_dia.CodFte
vCuen = LEFT(t_dia.cuenta,2)
vTipo = 'D'
vRegi = RECNO()
SUM Montos TO vtoD FOR  (vDoc  = t_dia.comprob + t_dia.comadi + t_dia.codfte)		&& AND (t_dia.tipo=vTipo) AND (LEFT(t_dia.cuenta,2)=vCuen)
SELECT(area)
GOTO vRegi
RETURN vtoD


FUNCTION t_Hab
*-------------
area = SELECT()
SELECT T_Dia
vDoc  = t_dia.comprob + t_dia.comadi + t_dia.CodFte
vCuen = LEFT(t_dia.cuenta,2)
vTipo = 'H'
vRegi = RECNO()
SUM Montos TO vtoH FOR  (vDoc  = t_dia.comprob + t_dia.comadi + t_dia.codfte) AND (t_dia.tipo=vTipo) AND (LEFT(t_dia.cuenta,2)=vCuen)
SELECT(area)
GOTO vRegi
RETURN vtoH
FUNCTION Cuenta
*--------------
IF SUBSTR(Cuenta,1,2)='01' OR SUBSTR(Cuenta,1,2)='90'
	DO CASE
		CASE SUBSTR(Cuenta,3,8)='00000000'
		   mRet = LEFT(Cuenta,2)
		CASE SUBSTR(Cuenta,5,6)='000000'
		   mRet = LEFT(Cuenta,4)
		CASE SUBSTR(Cuenta,7,4)='0000'
		   mRet = LEFT(Cuenta,6)
		CASE SUBSTR(Cuenta,9,2)='00'
		   mRet = LEFT(Cuenta,8)
	 	OTHERWISE
		   mRet = Cuenta
	ENDCASE
  ELSE
	DO CASE
		CASE SUBSTR(Cuenta,3,8)='00000000'
		   mRet = LEFT(Cuenta,2)
		CASE SUBSTR(Cuenta,4,7)='0000000'
		   mRet = LEFT(Cuenta,3)
		CASE SUBSTR(Cuenta,6,5)='00000'
		   mRet = LEFT(Cuenta,5)
		CASE SUBSTR(Cuenta,8,3)='000'
		   mRet = LEFT(Cuenta,7)
	 	OTHERWISE
		   mRet = Cuenta
	ENDCASE
ENDIF
RETURN mRet

FUNCTION suma_doc
*----------------
*- Suma el total del debe y haber por documento
DtotalDoc = DtotalDoc+IIF(t_Dia.tipo='D',t_dia.montos,0)
HtotalDoc = HtotalDoc+IIF(t_Dia.tipo='H',t_dia.montos,0)
RETURN ' '

FUNCTION Glosas
*--------------
r  = ""
gl = ALLTRIM( UPPER ( T_Com.Glosa1 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+gl
ENDIF
gl = ALLTRIM( UPPER ( T_Com.Glosa2 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF
gl = ALLTRIM( UPPER ( T_Com.Glosa3 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF
gl = ALLTRIM( UPPER ( T_Com.Glosa4 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF
RETURN IIF(!Empty(r),ALLTRIM(r),'No se encontr¢ detalle/glosa para este Documento.')


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
 RELEASE    MENU   mMenu
 ON KEY LABEL F4
 ON KEY LABEL F9
 
 CLOSE DATA
 ERASE (filaux)
 ERASE (vIndPar)
RETURN

FUNCTION v_Ord
*-------------
mRet = .F.
cAlias = ALIAS()
USE Glosas IN 0 ORDER TAG Glosas1
SELE Glosas
SEEK STR(YEAR(ActMes),4)+LEFT(m.Comprob,2)+m.Orden
IF !FOUND()
	SET FILTER TO STR(YEAR(ActMes),4)+LEFT(m.Comprob,2)=Ano+Tipo
	GO TOP
	IF !EOF()
		ON KEY LABEL F10 KEYBOARD(CHR(23))
		DEFINE WINDOW w_Ord FROM 03,10 TO 18,70  DOUBLE ;
			TITLE  ' ± Ordenes de Glosas ± ' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 15
		ACTIVATE WINDOWS w_Ord
		BROWSE ;
			NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Ordenes de Glosas ';
			IN WINDOW w_Ord  FIELD ;
			Tipo	: H='Tipo',;
			Orden	: H='Orden',;
			Glosa	: H='Glosa'
		RELEASE WINDOWS w_Ord
		ON KEY LABEL F10 KEYBOARD(CHR(23))
		m.Orden = Glosas.Orden
		mRet = .T.
	 ELSE
		DO StandBy WITH "No existen Glosas para este tipo de documento. Por favor revise"
		mRet = .F.
	ENDIF
	SET FILTER TO
 ELSE
	mRet = .T.
ENDIF
USE IN Glosas
SELE (cAlias)
RETURN mRet

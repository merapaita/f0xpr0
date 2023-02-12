** --------------------------------------
** COMING.PRG
** Ingreso de Documentos de Diarios
** AUTOR : UDEP - 1995. FMC - OOC Versi¢n 6.0
** Modif.  : M.R.A.  20/09/2000
** ---------------------------------------
CLOS DATA
*- Guarda el menu y la pantalla anterior
SAVE SCREEN TO pta_comp
HIDE POPUP pop_01,menu
*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens09
PUBLIC tip,xtipobr,xCodObra
Vmens01 = 'Í  Documentos  Varios ÍÍ F9:Det ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = ' Detalle  '
xPos    = INT((80 - (LEN(ALLTRIM(cia)) + 2))/2)
Vmens01 = SUBSTR(Vmens01, 1, xPos+1) + ' ' + cia + ' ' + SUBSTR(Vmens01,xPos + LEN(ALLTRIM(cia)) + 3,80)

*- Copia estructura del diario a temporal

filaux = SYS(3) + '.DBF'
SELECT 8
CREATE TABLE (filaux);
  (Comprob   C(014),;
   ComAdi    C(002),;
   Item      N(004),;
   NumMov    N(003),;
   FecEmi    D(008),;
   Cuenta    C(010),;
   Tipo      C(001),;
   Montos    N(18,3),;
   TipAux    C(002),;
   Registro  C(006),;
   DocRef    C(007),;
   DocAdi    C(002),;
   CodCtc    C(014),;
   NumChq    C(014),;
   CCosto    C(005),;
   CodDep    C(006),;
   TipDcr    C(002),;
   Estado    C(002),;
   FecVen    D(008),;
   SaldPen   N(18,3),;
   CodFte    C(002),;
   Nummesref C(002),;   
   Periodoref C(002),;
   Tipfun    C(001),;
   CodArt    C(011),;
   Descri    C(135),;
   Codpart   C(010),;
   Codanal   C(010),;
   TipFin    C(004),;
   TFin95    C(004),;
   TipObr    C(001),;
   CodObra   C(004) )

PUBLIC IDXAUXI   
DO BASES

IF blo_sis(1)
   DO STANDBY WITH "Sistema Bloquedo, Consulte al Area de Sistemas"
   CLOS DATA
   RETURN 
ENDIF   

*- Variables de trabajo (registro a trabajar)
PUBLIC m.comprob, m.comadi, m.TipDoc, m.tipdcr
PUBLIC m.docref, m.docadi, m.fecref, m.numchq , m.tipfun , m.codart, m.descri, m.codobra, m.tipobr
PUBLIC m.fecha, m.actmes, vLleno, vcatego,vmodali
PUBLIC m.OldDebe, m.OldHaber
PUBLIC v_sumas, v_cuadre, m.estado, m.glosa1, m.glosa2, vCon, vCon1, vMto
PUBLIC m.totdeb, m.tothab, m.codctc, m.codprv, m.glosa3, m.glosa4
PUBLIC xmov, _msg1,_msg2,;
       xfecemi, xcuenta, xTipAux, xRegistro, xmontos, m.CodCtc, xcodfte, xTipfun,xCodart,xDescri
PUBLIC xCodFte, xCodanal,xCodpart,xNumhm   
PUBLIC vErdad
PUBLIC wnummov,wfecemi,wmontos,wTipAux,wregistro,wDocRef,wdocadi,wnumchq ,wCCosto,wCodDep,wtipdcr,wcodctc,wtipo,wdestino,kcuenta,wmonto 
PUBLIC vtipfin,vtipfin95,vAli,vDHCuentas,VDESTINO,VCODARTI,TIENE, TOTIGVD, totigvH,Vcodcomp
vtipfin=SPACE(08)
vtipfin95=SPACE(08)
vali=SPACE(08)
wdestino=SPACE(55)
vDHCuentas=SPACE(20)
VDESTINO=SPACE(1)
*VCODARTI=SPACE(11)
totigvH = 0
totigvd = 0
xTipAux   = SPACE(02)
xRegistro = SPACE(06)
wTipAux   = SPACE(02)
wRegistro = SPACE(06)
wtipo     = SPACE(02)
fmes      = SPACE(02)
Vcodcomp  = SPACE(14)

Agrega  = .T.
STORE 0 TO m.item, vLleno, vItem
STORE SPACE(4) TO vNumCP
*- Mes y a¤o actual
 DEFINE WINDOW WParalelo FROM 10,12 TO 15,68 DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW WParalelo
 SELECT parma
 SEEK "MESANO"+"ACTUAL"
 _actmes = LEFT(descri,6)
 actmes = CTOD("01/"+SUBSTR(_actmes,5,2)+"/"+SUBSTR(_actmes,1,4) )
 fmes = SUBSTR(_actmes,5,2)
 fAno = SUBSTR(_actmes,1,4)
 @ 1, 12 SAY 'Mes a Trabajar :' GET Fmes PICTURE "!!" VALID val_para(Fmes,'FECMES',' ',31,25)
 @ 1, 35 SAY mfecha(val(fmes),year(actmes))
 READ 
 RELEASE WINDOW WParalelo
*- Si obliga a cuadre y al total de suma de cuentas
*- en el documento
SEEK "OPERAT"+"SUMAS"
v_sumas = (LEFT(descri,2)="SI")
SEEK "OPERAT"+"CUADRE"
v_cuadre = (LEFT(descri,2)="SI")
SEEK "CONTAB"+"CIERRE"
MesCerrado = VAL(SUBSTR(Descri,5,2))
AnoCerrado = VAL(SUBSTR(Descri,1,4))

IF !((VAL(fMes)>MesCerrado AND VAL(fAno)=AnoCerrado) OR VAL(fAno)>AnoCerrado)
	DO StandBy WITH "­­­ Dicho mes ya est  cerrado !!!"
	CLOS DATABASE
	ERASE (FilAux)
	RETURN .F.
ENDIF

*- Inicia proceso

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
*SET INDEX TO
ERASE (idxauxi)
ERASE (filaux)
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior   Siguiente         Ingresa  Elimina         Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 10,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_C2 FROM 10,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C3 FROM 04,00 TO 23,58 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C4 FROM 00,00 TO 23,79 DOUBLE;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO anula
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
 @ 01,02 SAY "       Glosa:" 
 @ 02,02 SAY "       Glosa:" 
 @ 03,02 SAY "       Glosa:" 
 @ 04,02 SAY "       Glosa:"    
 @ 05,02 SAY "  Cad. Func.:"
 @ 05,37 SAY "Fte. Fto.:"
 @ 06,02 SAY "C. Corriente:"
 @ 08,02 SAY "  Total Debe:"
 @ 08,41 SAY "Total Haber:"
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 ON KEY LABEL F9 DO Vista_Det
 SELECT Comp
 SCATTER MEMVAR
 wtipo=m.Tipdoc
 @  0,16 SAY m.TipDoc
 @  0,19 SAY m.Comprob
 @  0,35 SAY m.ComAdi
 @  0,48 SAY m.Fecha
 @  1,16 SAY SUBSTR(m.Glosa1,1,60)
 @  2,16 SAY SUBSTR(m.Glosa2,1,60)
 @  3,16 SAY SUBSTR(m.Glosa3,1,60)
 @  4,16 SAY SUBSTR(m.Glosa4,1,60) 
 @  5,16 SAY SPACE(20)
 @  5,16 SAY m.CodCad
 @  5,47 SAY m.codfte
 @  5,52 SAY val_para(m.codfte,'CODFTE','Z',52,25)
 @  6,16 SAY m.CodCtc
 
*** No se tien Obras
*** @  7,16 SAY m.Tipobr
*** @  7,18 SAY m.Codobra
***
 @  7,23 SAY wdestino
 @  8,16 SAY m.TotDeb  PICTURE "999,999,999,999.99"
 @  8,54 SAY m.TotHab  PICTURE "999,999,999,999.99"
 DO Vista_Hijo
RETURN


PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
** SCATTER MEMVAR
 SELECT COMP
 SET RELATION TO COMPROB+COMADI INTO DIAR
 SELECT Diar
 vOrd=ORDER()
 SET ORDER TO TAG Diario11
 seek m.Comprob + m.ComAdi
 BROWSE;
   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
   WINDOW Wind_C2 KEY m.Comprob + m.ComAdi;   
   FIELDS;
   nummov   :H="Mov",;
   ORDEN    :H="ORD",;
   fecemi   :H='Emisi¢n',;
   coddep   :H='Depen',;
   cuenta   :H='  Cuenta',;
   nivel    = Val_Fun('Cuen','Cuenta','nivel',Diar.Cuenta):P="99":H="Nv",;
   x1 = IIF(tipo="D",montos,0) :P="99,999,999.99" :H=" Debe M.Nac.",;
   x2 = IIF(tipo="D",0,montos) :P="99,999,999.99" :H="Haber M.Nac.",;
   ccosto   :H='Cen.Cto',;
   registro :H='C¢d.Aux.',;
   tipfin   :H='Tipo Fin.'
   
 SET ORDER TO TAG vOrd
 SELECT Comp
RETURN


PROCEDURE  Vista_Det
*-------------------
 HIDE POPUP ALL 
 ON KEY LABEL F9
 SCATTER MEMVAR
 SELECT Diar
 vOrd=ORDER()
 SET ORDER TO TAG Diario11
 BROWSE;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT ;
   WINDOW Wind_C2 KEY m.Comprob + m.ComAdi + m.Codfte;   
   FIELDS;
   nummov   :H="Mov",;
   ORDEN    :H="ORD",;
   fecemi   :H='Emisi¢n',;
   coddep   :H='Depen',;
   cuenta   :H='  Cuenta',;
   nivel    = Val_Fun('Cuen','Cuenta','nivel',Diar.Cuenta):P="99":H="Nv",;
   x1 = IIF(tipo="D",montos,0) :P="99,999,999.99" :H=" Debe M.Nac.",;
   x2 = IIF(tipo="D",0,montos) :P="99,999,999.99" :H="Haber M.Nac.",;
   ccosto   :H='Cen.Cto',;
   registro :H='C¢d.Aux.',;
   tipfin   :H='Tipo Fin.',;
   tfin95   :H='TipFin 95'
 SET ORDER TO TAG vOrd
 SELECT Comp
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT Comp
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 ACTIVATE SCREEN
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   TipDoc   :H= 'Tipo doc.',;
   Comprob  :H= 'Documento',;
   ComAdi   :H= 'CompAdi',;
   Fecha    :H= 'Fecha' ,;
   CodCaD   :H= 'CAD.FUN.' ,;
   Glosa1   :H= '              Glosa',;
   Glosa2   :H= '              Glosa',;
   Totdeb   :H= '  Debe M.Nac.',;
   Tothab   :H= ' Haber M.Nac.'
  vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
   ON KEY LABEL F10
* DEACTIVATE WINDOW Wind_C4
 DO Vista
RETURN

PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 SELECT Comp
 VORDER=ORDER()
 SET ORDER TO COMPRO4
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF

 IF LASTKEY()=27
    RETURN
 ENDIF

 Vtemp = RECNO()

 DEFINE WINDOW WBusca FROM 10,12 TO 15,68 DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW WBusca
 STORE SPACE(14) TO Vbusca
 @ 2, 1 SAY 'N£mero  de documento que desea buscar:' GET Vbusca PICTURE '!!!!!!!!!!!!!!'
 READ
 RELEASE WINDOW WBusca
 IF LASTKEY()=27
    SELECT comp
	SET ORDER TO (vorder)
    RETURN
 ENDIF
 
* IF !SEEK(Vbusca + Vbusca1 + ALLTRIM(Vbusca2))
SEEK VBUSCA
IF !FOUND()
    DO standby WITH "Documento No Encontrado."
    GOTO Vtemp
 ELSE
    DO Vista
 ENDIF
SELE COMP
SET ORDER TO (vorder)
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

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
ACTIVATE WINDOW Wind_C0
SELECT Comp
VRECNO = RECNO()

SCATTER MEMVAR BLANK

DO WHILE .T.
	vcol ="w+/w,n/w,W/W,w/W,,w+/b"
	IF Escolor
		DEFINE POPUP RepT FROM 03,23  to 23,54  SHADOW	COLOR &vcol
	 ELSE
		DEFINE POPUP RepT FROM 03,23  to 23,54  COLOR SCHEME c_popup
	ENDIF
	
	DEFINE BAR  1 OF RepT  PROMPT ' \<a.Nota de Entrada a Almacen  '
	DEFINE BAR  2 OF RepT  PROMPT ' \<b.Orden de Servicio          '
	DEFINE BAR  3 OF RepT  PROMPT ' \<c.Hoja de Control            '
	DEFINE BAR  4 OF RepT  PROMPT ' \<d.Comprobante de Pago        '
	DEFINE BAR  5 OF RepT  PROMPT ' \<e.Recibo de Ingreso          '
	DEFINE BAR  6 OF RepT  PROMPT ' \<f.Boleta de Dep¢sito         '
	DEFINE BAR  7 OF RepT  PROMPT ' \<h.Nota de Cargo              '
	DEFINE BAR  8 OF RepT  PROMPT ' \<i.Nota de Abono              '
	DEFINE BAR  9 OF RepT  PROMPT ' \<j.Facturas                   '
	DEFINE BAR 10 OF RepT  PROMPT ' \<k.Hojas de Modificaciones    '
	DEFINE BAR 11 OF RepT  PROMPT ' \<l.Hoja de Anulaci¢n de Cheque'
	DEFINE BAR 12 OF RepT  PROMPT ' \<m.Ordenes de Compra          '
	DEFINE BAR 13 OF RepT  PROMPT ' \<n.Cheques Girados            '
	DEFINE BAR 14 OF RepT  PROMPT ' \<o.Cheques Entregados         '
	DEFINE BAR 15 OF RepT  PROMPT ' \<p.Ordenes de Serv. con C/P   '
	DEFINE BAR 16 OF RepT  PROMPT ' \<q.Reversiones T - 6          '
	DEFINE BAR 17 OF RepT  PROMPT ' \<r.Rebajas Ingresos           '
	DEFINE BAR 18 OF RepT  PROMPT ' \<s.Parte de Reb. y Anul OC, OS'
	DEFINE BAR 19 OF RepT  PROMPT ' \<t.PE. CO. SA.                '
	ON SELECTION POPUP RepT DO SelPop
	ACTIVATE POPUP RepT
	
	SELECT COMP
	VRECNO1 = RECNO()
	UNLOCK ALL
	CLOSE DATA
	DO BASES
	GO VRECNO1
	IF error()=5
		go vRacno1
	endif
	DO vista
	RETURN
ENDDO
RETURN  

PROCEDURE Verif
*--------------
PARAMETER merror, mens, mens1, mprog, mn£ml¡n

RETURN

FUNCTION Tra_Hijo
*----------------
PARAMETER cor_ing       && cor_ing = 1 correccion carga data antigua
PUBLIC valido
HIDE MENU mMenu
ACTIVATE SCREEN

 redonerr  = .F.
 xmov      = 0
 xfecemi   = CTOD('  /  /  ')
 _msg1     = " "
 _msg2     = " "
* xTipAux   = SPACE(2)
* xRegistro = SPACE(6)
 
Tit = "²² F2:Edita    F5:Agrega Item    F7:TipFin    F8:Borra    F10:Sale ²²"

ON KEY LABEL F2  DO Edicion
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F7  DO TipFin
*ON KEY LABEL F9  DO TFin95
ON KEY LABEL F10 DO sal_hijo

IF cor_ing = 1    && Correccion, carga data antigua
   m.Item = 0
   SELECT Diar
   vOrd=ORDER()
   SET ORDER TO TAG Diario11
   SEEK m.Comprob + m.ComAdi + xCodfte
   SCAN WHILE Diar.Comprob + Diar.ComAdi + Diar.Codfte = m.Comprob + m.ComAdi + xCodfte
	SCATTER TO datos
	SELECT Diax
	APPEND BLANK
	GATHER FROM datos        
	m.Item   = Diax.Item
	wtipaux  = Diax.tipaux
	wregistro= Diax.registro              
	Sele Diar
   ENDSCAN
   
   SELECT Diax
   REPLACE ALL TipObr  WITH xTipObr
   REPLACE ALL CodObra WITH xCodObra
   SELE Diar
   SET ORDER TO TAG vOrd
ENDIF
SELECT diax
BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
  TITLE Tit FIELDS;
  item     :H='Item'       :R,;  
  nummov   :H='Mov.'       :R,;
  fecemi   :H='Emisi¢n'    :R,;
  cuenta   :H=' Cuenta'    :R,;
  tipo     :H='D/H'        :R,;
  montos   :H=' Importe S/. ':P='99,999,999.99':R,;
  tipaux   :H='TipAux.'    :R,;
  registro :H='C¢d.Aux. '  :R,;
  ccosto   :H='C.Cto':R,;
  Tipfun   :H='Tipfun' :R,;
  CodArt   :H='Art¡culo' :R,;
  Descri   :H='Descripci¢n' :R,;
  Codpart  :H='Partida' :R,;
  Codanal  :H='Analitica' :R,;
  TipFin   :H='TipFin' :R
  *TFin95   :H='TipFin 95':R
  
  ON KEY LABEL F10
  ON KEY LABEL F5
  ON KEY LABEL F8
  ON KEY LABEL F2

IF LASTKEY()=27
   DO standby WITH " Cancela Ingreso "
   RETURN .F.
ENDIF

SELECT comp
ACTIVATE SCREEN
vTempo = ' Revisa  Busca  Anterior  Siguiente         Ingresa  Elimina         Termina '
DO Logos WITH Rotulo1,vtempo
SHOW MENU mMenu

RETURN valido


PROCEDURE Agreg_item
*-------------------
 SELECT Diax
 APPEND BLANK
 m.Item = m.Item + 1
 REPLACE Comprob  WITH m.Comprob
 REPLACE ComAdi   WITH m.ComAdi
 REPLACE Item     WITH m.Item
 REPLACE Tipo     WITH 'D'
 REPLACE CodCtc   WITH m.CodCtc
 REPLACE CodFte   WITH m.codfte
 REPLACE TipObr   WITH xtipObr
 REPLACE CodObra  WITH xCodObra
 REPLACE Coddep   WITH '112000'
* REPLACE TipFin   WITH m.TipFin            
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
      REPLACE Comp.TotDeb WITH Comp.TotDeb - Diax.Montos
   ELSE
      m.TotHab = m.TotHab - Diax.Montos
      REPLACE Comp.TotHab WITH Comp.TotHab - Diax.Montos      
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


PROCEDURE Anula
*--------------
* Anula el documento actual.
* Solo se puede anular si no tiene provisiones que se cancelen
* en otros documento o si el documento no se ha cerrado.

IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
* IF m.fecha < m.actmes
*   DO standby WITH "Documento no es del mes actual"
*   RETURN
* ENDIF
IF m.estado $ '12'
   DO standby WITH "Documento cerrado"
   UNLOCK ALL
   RETURN
ENDIF
IF yesno("¨ANULA Documento?")
   vErdad=.T.
   vOrd=ORDER()
   SELECT Comp
   IF f_lock(1)
      REPLACE Estado WITH '99'
      DELETE
      *-Busca y elimina los Items
      SELECT Diar
      SET ORDER TO TAG Diario11
      SEEK Comp.Comprob+Comp.ComAdi
      IF FOUND()
	 SCAN WHILE Comp.Comprob+Comp.ComAdi+Comp.CodCtc= Diar.Comprob+Diar.ComAdi
	      IF vErdad
		 =Rebaja()
	      ENDIF
	      IF f_lock(1)
		 REPLACE Estado WITH '99'
		 DELETE
	      ENDIF
	 ENDSCAN
      ENDIF
      SET ORDER TO TAG Diario1
      SELECT Comp
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
 ERASE (filaux)
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
vali = .T.
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

  IF !vali .AND. !valido
     IF !yesno("¨Cancela ingreso? <NO> = Corrige")
	RETURN
     ENDIF
  ENDIF
ENDIF
valido = valido .AND. vali

KEYBOARD CHR(23)
RETURN


*- Funcion de Edici¢n

FUNCTION Edicion
*---------------

ACTIVATE WINDOW Wind_C3
*- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2  DO nada
ON KEY LABEL F3
ON KEY LABEL F5  DO nada
ON KEY LABEL F7  DO nada
ON KEY LABEL F8  DO nada
ON KEY LABEL F9  DO nada
ON KEY LABEL F10 DO nada
xTipfun =Tipfun
XCodart =Codart
xDescri =Descri
xCodpart=Codpart
xCodanal=Codanal
xtipObr =TipObr
xCodObra=CodObra

REPLACE NumMov   WITH 1
REPLACE tipaux   WITH wtipaux
REPLACE registro WITH wregistro      
REPLACE codctc   WITH m.Codctc

@  0,1  SAY '    Item:' + STR(Item,4)
@  1,1  SAY '    Mov.:' GET NumMov    VALID   (v_mov() .OR. cor_ing=1);
					      .AND. !EMPTY(nummov)

@  2,1  SAY ' Fec.Emi:' GET FecEmi    VALID   Val_Fec();
				      ERROR   "Mes igual o menor al del documento"
tip=.F.
@  3,1  SAY '  Cuenta:' GET Cuenta    VALID   v_cta();
				      ERROR   _msg1;
				      PICTURE '9999999999'

@  4,1  SAY '     D/H:' GET Tipo      VALID   Tipo$'DH';
				      ERROR   'Debe o Haber';
				      PICTURE '@M D,H' && .AND. Lleno()

@  5,1  SAY ' Imp.S/.:' GET Montos    PICTURE '999,999,999,999.99'

@  6,1  SAY 'Tip.Aux.:' GET TipAux    VALID   v_aux1();
				       ERROR   "Dato errado!!!"

@  7,1  SAY 'C¢d.Aux.:' GET Registro  VALID   V_Aux()

@  8,1  SAY '  DocRef:' GET DocRef    PICTURE "!!!!!!!"

@  8,18 SAY '-'

@  8,19                 GET DocAdi    VALID Valida() PICTURE "!!"

@  9,1  SAY '  CtaCte:' GET CodCtc    &&VALID CtaCte()

@ 10,1  SAY 'N.Cheque:' GET NumChq    PICTURE "@!"

@ 11,1  SAY 'C. Costo:' GET cCosto    VALID   v_cto();
				      ERROR   "Cto.Cto. errado"
@ 12,1   SAY 'Cod.Dep.:' GET CodDep    PICTURE "!!!!!!"
@ 13,1   SAY 'Mes-Per. DocRef: ' GET  Nummesref    PICTURE "!!"
@ 13,21  SAY '-'
@ 13,22                          GET  periodoref   PICTURE "!!"
@ 14,1   SAY 'Articulo: '+ALLTRIM(xTipfun)
@ 14,12  SAY '-'
@ 14,13  SAY ALLTRIM(XCodart)
@ 14,25  SAY ALLTRIM(xdescri)
@ 15,1   SAY 'Partida :'+ALLTRIM(xCodpart)+' '+ALLTRIM(xCodanal)
READ 

IF tip
    =Aut_Fin (SUBSTR(Comp.Comprob,1,2),Diax.Cuenta)    
    IF !EMPTY(vTipfin)
	REPLACE TipFin WITH SUBSTR(vTipfin,1,4)
		*REPLACE TFin95 WITH SUBSTR(vTipfin,5,8)
	ENDIF   
	@ 16,1   SAY 'Tip Fin.:' GET TipFin  VALID Val_Tipo(TipFin,'TIPFIN','TipFin')
	*@ 17,1   SAY 'TFin 95 :' GET TFin95  VALID Val_Tipo(TFin95,'TFIN95','TFin95')
	READ 
ENDIF
*DO Pantalla
*vreturn = Val_para(Diax.Tipaux,'AUXIL ','Tipaux',' ',6,18)

SELECT Diax
IF LASTKEY() = 27
   DO standby WITH " Cancela Ingreso "
ELSE
   Tipfun=xTipfun
   Codart=xCodart
   Descri=xDescri
   Codpart=xcodpart
   Codanal=xcodanal
   tipObr =xTipObr
   CodObra=xCodObra
   GATHER MEMVAR
ENDIF
*--------
*Actualiza Registro Auxiliar
*-------
DO regreg
*-----------
DEACTIVATE WINDOW WIND_C3

ON KEY LABEL F2  DO Edicion
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F7  DO TipFin
*ON KEY LABEL F9  DO TFin95
ON KEY LABEL F10 DO sal_hijo
RETURN .F.


*- Funci¢n para seleccionar Tipo Financiero
FUNCTION TipFin
*------------------
ON KEY LABEL F2  DO nada
ON KEY LABEL F3
ON KEY LABEL F5  DO nada
ON KEY LABEL F7  DO nada
ON KEY LABEL F8  DO nada
ON KEY LABEL F9  DO nada
ON KEY LABEL F10 DO nada

tip=.F.
SELE EstFin
IF SEEK(LEFT(Diax.Cuenta,3))
	tip=.T.
ENDIF
SELECT Diax

IF tip=.T.
	DEFINE WINDOW TFin FROM 10,15 TO 13,60 FOOTER '[ESC]:Cancela  [F10]:Confirma'
	ACTIVATE WINDOW TFin
	tf=Diax.TipFin
	* ON KEY LABEL F10 CLEAR READ 
	@0,7 SAY 'Tipo Financiero : ' GET TipFin VALID Val_Tipo(TipFin,'TIPFIN','TipFin')   
	READ  
	IF LASTKEY()=27 
		REPLACE TipFin WITH tf
	ENDIF
	DEACTIVATE WINDOW TFin
ELSE
	WAIT WINDOW 'Cuenta no se encuentra en la Estructura Financiera' NOWAIT
ENDIF
* ON KEY LABEL F10
ON KEY LABEL F2  DO Edicion
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F7  DO TipFin
*ON KEY LABEL F9  DO TFin95
ON KEY LABEL F10 DO sal_hijo
IF LASTKEY() = 27
   DO standby WITH " Cancela Ingreso "
ENDIF
RETURN .F.


*- Funciones de validacion
FUNCTION Asigna
*--------------
IF EMPTY(m.TipDoc)
   vlen      = LEN(m.Comprob)
   m.Comprob = m.tipdoc + SPACE(vlen - LEN(m.tipdoc))
   RETURN .T.
ENDIF
RETURN


FUNCTION Asigna1
*---------------

 IF m.TipDoc # vTipDoc
    vlen      = LEN(m.Comprob)
    m.Comprob = m.TipDoc + SPACE(vlen - LEN(m.TipDoc))
 ENDIF
 RETURN .T.


FUNCTION Asigna2
*---------------
 vlen        = LEN(m.DocRef)
 m.DocRef = m.tipdcr + SPACE(vlen - LEN(m.tipdcr))
 RETURN .T.

FUNCTION v_comp
*--------------
 IF LEFT(m.comprob,2)#ALLTRIM(m.tipdoc)
    _MSG2 = "Digitos iniciales INCORRECTOS"
    RETURN .f.
 ENDIF

 IF LEN(ALLTRIM(m.comprob)) = 2
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


FUNCTION v_comp1
*---------------
PRIVATE __fun,__fun1
vAli=ALIAS()
vOrd=ORDER()
SELECT Comp
SET ORDER TO TAG Compro7

  _rec = IIF(EOF(),-1,RECNO())

  __fun = !SEEK(m.comprob+m.comadi+ALLTRIM(STR(MONTH(m.fecha))))

 _msg2 = "Documento YA EXISTE"

 __fun1= !m.fecha < m.actmes
 IF !__fun1
    _msg2 = "El a¤o/mes no es el correcto"
 ENDIF

 IF _rec>0
    GO _rec
 ELSE
    GO BOTTOM
 ENDIF
SELECT (vAli)
SET ORDER TO
RETURN __fun


*- 2. Valida Fecha del item
FUNCTION Val_Fec
*---------------
IF MONTH(fecemi) = MONTH(m.fecha) .AND. YEAR(fecemi) = YEAR(m.fecha)
   RETURN .T.
ENDIF
IF PADL(YEAR(fecemi),4,'0') + PADL(MONTH(fecemi),2,'0') < ;
   PADL(YEAR(m.Fecha),4,'0') + PADL(MONTH(m.Fecha),2,'0')
   IF YesNo("Mes distinto al del DOCUMENTO. ¨Continua?")
      RETURN .T.
   ENDIF
ENDIF
RETURN .F.


*- 4 Tipo de auxiliar
FUNCTION v_aux1
*--------------
vAlias  = ALIAS()
vreturn = .F.
IF EMPTY(diax.Tipaux)
   vreturn = Val_para(Diax.Tipaux,'AUXIL ','Tipaux',' ',6,18)
ENDIF   

SELECT Parma
SEEK 'AUXIL ' + Diax.TipAux
IF FOUND()
   REPLACE Diax.Tipaux WITH Parma.Codigo
   @ 6,15 SAY SUBSTR(Parma.Descri,1,18)
   vreturn=.t.
ELSE 
   vreturn=.f.
ENDIF   

SELECT (vAlias)
RETURN (vreturn)


*- 5 Codigo auxiliar

FUNCTION v_aux
*-------------
 _ALI_   = ALIAS()
 resx    = .F.
 vReturn = .F.

 IF EMPTY(Diax.registro)
       xaux = SPACE(30)  && Temporalmente guarda la descripci¢n
       SELECT Auxi
       vidx = SYS(3) + '.idx'
       INDEX ON Descri TO &vidx FOR Diax.Tipaux = Tipo
       resx = val_fun('Auxi','Descri','descri',xaux,1)
       SELECT Auxi
       SEEK xAux
       xaux = Auxi.Codigo
       SET INDEX TO
       SET ORDER TO TAG AUXIL1
       ERASE (vidx)
       SELECT Diax
       IF resx
	  REPLACE Diax.registro WITH xaux
       ENDIF
       SELECT Auxi
       IF !SEEK(Diax.tipaux + Diax.registro)
	  resx = .F.
       ENDIF       
ELSE
 SELECT auxi
 SET ORDER TO TAG AUXIL1
 SEEK diax.tipaux + SUBSTR(diax.registro,1,6)
  IF FOUND()
     @ 7,18 SAY SUBSTR(auxi.descri,1,30)
     wtipaux=diax.tipaux
     wregistro=diax.registro      
     resx = .T.  
  ELSE   
    resx = .F.  
  ENDIF
ENDIF

SELECT auxi
 SET ORDER TO TAG AUXIL1
SEEK diax.tipaux + SUBSTR(diax.registro,1,6)
IF FOUND()
   @ 7,18 SAY SUBSTR(auxi.descri,1,30)
     wtipaux=diax.tipaux
     wregistro=diax.registro      
     resx = .T.  
 ELSE
     resx = .F.  
ENDIF  
SELECT (_ali_)
RETURN resx


PROCEDURE regreg
*------------------
IF yesno("Desea Actualizar Todos los Items con el mismo Auxiliar ?")  
    * Se Actualiza el auxiliar    
    SELE Diax
    REPLACE ALL diax.tipaux   WITH wtipaux
    REPLACE ALL diax.registro WITH wregistro
  ENDIF  
RETURN

*- 7. La cuenta
FUNCTION v_cta
*-------------
* Muestra el tipo de auxiliar posible.
* Verifica que la cuenta sea de detalle y no sea autom tica.
* Si la cuenta no existe permite agregarla en el plan de cuentas.

 SELECT Cuen
 _f_ = SEEK(Diax.Cuenta) .AND. !EMPTY(Diax.Cuenta)
 SELECT Diax
 @ 3,22 SAY SUBSTR(Cuen.Descri,1,14)

 IF _f_ .OR. EMPTY(Diax.Cuenta)
    IF EMPTY(Diax.Cuenta)
       vcuenta = SPACE(LEN(Diax.Cuenta))
       SELECT Cuen
       SET ORDER TO TAG Cuentas4
       _f_ = Val_Fun1('Cuen','Cuenta',"TRIM(Cuenta) +' '+ Descri",VCuenta,1,3,22,'SUBSTR(Descri,1,14)','Cuentas4','Cuentas5')
       SET ORDER TO TAG Cuentas1
       SELECT Diax
       IF !_f_
	  _msg1 = "Indique el n£mero de cuenta!!!"
	  RETURN .F.
       ELSE
	  REPLACE Diax.Cuenta  WITH vCuenta
	  @ 3, 11 SAY Diax.Cuenta
       ENDIF
    ENDIF


   SELE EstFin
   IF SEEK(LEFT(Diax.Cuenta,3))
		tip=.T.
   ENDIF
   SELECT Diax

 ENDIF

RETURN _f_


* 8 . Numero de Movimiento
FUNCTION v_mov
*-------------
* Guarda todos los valores que un mismo movimiento debe tener.
* Tambi‚n, guarda los anteriores valores de los documentos antes
* de ser modificados.

 xTipAux    = TipAux
 xRegistro  = Registro
 xmov       = nummov
 xfecemi    = fecemi
 xcuenta    = cuenta
 xmontos    = montos
 xCodCtc    = CodCtc
 IF EMPTY(xfecemi)
    vreg = RECNO()
    SELECT Diax
    GO TOP
    SCAN
       IF xmov = nummov .AND. vreg # RECNO()
	  xfecemi    = fecemi
	  EXIT
       ENDIF
    ENDSCAN
    GO vreg
    REPLACE fecemi WITH xfecemi
    @  2,11 SAY  fecemi
    @  3,11 SAY  cuenta     PICTURE '9999999999'
 ENDIF
 RETURN .T.


*  9. Desactiva las teclas F2, F5, F8, F10
PROCEDURE Nada
*-------------
RETURN


* 10. Agrega una nueva cuenta.
PROCEDURE ap_cta
*---------------
PARAMETER xxcta
*- apertura de ctas. en linea
 DEFINE WINDOW _ctas FROM 3,5 TO 20,75 TITLE " APERTURA DE CUENTAS " COLOR SCHEME 10
 ACTIVATE WINDOW _ctas
 CLEAR

 PRIVATE M.CUENTA, M.DESCRI,  M.NIVEL,   M.DETALLE
 PRIVATE M.TIPO,   M.DCONTRA, M.HCONTRA
 PRIVATE M.DBHB,   xxalias

 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW _ctas

 SELECT Cuen
 SCATTER MEMVAR BLANK
 m.Cuenta = xxcta
 xxalias = ALIAS()

 vreg = IIF(!EOF(),RECNO(),0)
 xDetalle = IIF(m.Detalle='S','Si','No')
 XTipo    = IIF(m.Tipo   ='A','Activo   ','Pasivo   ')
 XDbHb    = IIF(m.DbHb   ='D','Deudora  ','Acreedora')
 m.Nivel  = LEN(ALLTRIM(m.Cuenta))

 @  3,2 SAY "            Cuenta:" GET m.Cuenta   WHEN (.F.) PICTURE '9999999999'
 @  4,2 SAY "       Descripci¢n:" GET m.Descri   PICTURE "@!"
 @  5,2 SAY "             NIVEL:" GET m.Nivel    WHEN (.F.) PICTURE '99'
 @  6,2 SAY "           Detalle:" GET xDetalle   PICTURE "@M Si,No"
 @  7,2 SAY "              Tipo:" GET xTipo      PICTURE "@M Activo   ,Pasivo   ,Resultado"
 @  8,2 SAY "Contra cuenta Debe:" GET m.DContra  PICTURE '9999999999' VALID EMPTY(m.DContra) .OR. Val_Cuen(m.DContra,.T.,13,23)
 @  9,2 SAY "Contr.cuenta Haber:" GET m.HContra  PICTURE '9999999999' VALID EMPTY(m.HContra) .OR. Val_Cuen(m.HContra,.T.,15,23)
 @ 10,2 SAY "        Debe/Haber:" GET XDbHb      PICTURE '@M Deudora  ,Acreedora'
 READ 
 m.Detalle = IIF(xDetalle $ 'Si','S','N')
 m.Tipo    = IIF(xTipo    $ 'Activo   ','A',IIF(xTipo $ 'Pasivo   ','P','R'))
 m.DbHb    = IIF(xDbHb    $ 'Deudora  ','D','H')
 IF vreg # 0 .AND. vreg <= RECCOUNT()
    GO vreg
 ENDIF

 IF LASTKEY() <> 27 .AND. yesno("Confirme el ingreso")
    IF f_appd()
       GATHER MEMVAR
    ENDIF
    UNLOCK
 ELSE
    RELEASE WINDOW _ctas
    SELECT diax
    RETURN .F.
 ENDIF

 RELEASE WINDOW _ctas
 SELECT diax

RETURN .T.


* 11. Centro de Costo
FUNCTION v_cto
*-------------
_F = .T.
IF !EMPTY(ccosto)
   xcto   = Diax.ccosto
   vAlias = ALIAS()
   SELECT Ctoc
   IF !SEEK(xcto)
      xcto = SPACE(LEN(Diax.ccosto))
      _F   = Val_Fun('Ctoc','ctocto','descri',xcto,1)
      IF LASTKEY()=27 .OR. !_F
	 IF yesno("Centro de Costo no existe. ¨Crea uno?")
	    _f = ap_cto(2)
	    IF _f
	       @ 10, 17 SAY SUBSTR(Ctoc.Descri,1,19)
	    ENDIF
	 ELSE
	    SELECT Diax
	    REPLACE CCosto WITH SPACE(LEN(Diax.CCosto))
	    @ 10,17 SAY SPACE(19)
	    RETURN .T.
	 ENDIF
      ELSE
	SELECT Diax
	REPLACE CCosto   WITH xcto
	@ 10, 17 SAY SUBSTR(Ctoc.Descri,1,19)
      ENDIF
   ENDIF
   SELECT (vAlias)
ELSE
   IF SUBSTR(Diax.Cuenta,1,1) = '9' .AND.;
      !YesNo("Falta indicar Centro de Costo. ¨Continua?")
      _f = .F.
   ENDIF
ENDIF

RETURN _f


* 12. Crea nuevo centro de costo.
FUNCTION ap_cto
*--------------
PARAMETER _tt

 DEFINE WINDOW _ctos FROM 3,5 TO 23,75 TITLE " APERTURA DE CTOS. DE CTOS. " COLOR SCHEME 10
 ACTIVATE WINDOW _ctos
 CLEAR

 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW _ctos

 SELECT Ctoc

 IF _tt = 1
    SCATTER MEMVAR
    XCuenta1 = SUBSTR(m.Cuentas,1,10)
    XCuenta2 = SUBSTR(m.Cuentas,11,10)
    XCuenta3 = SUBSTR(m.Cuentas,21,10)
    XCuenta4 = SUBSTR(m.Cuentas,31,10)
    XCuenta5 = SUBSTR(m.Cuentas,41,10)
    XCuenta6 = SUBSTR(m.Cuentas,51,10)
    XCuenta7 = SUBSTR(m.Cuentas,61,10)
    XCuenta8 = SUBSTR(m.Cuentas,71,10)
    XCuenta9 = SUBSTR(m.Cuentas,81,10)
    XCuent10 = SUBSTR(m.Cuentas,91,10)
 ELSE
    SCATTER MEMVAR BLANK
    m.ctocto = diax.ccosto
    STORE SPACE(10) TO XCuenta1, XCuenta2, XCuenta3, XCuenta4, XCuenta5, XCuenta6,;
		   XCuenta7, XCuenta8, XCuenta9, XCuent10
 ENDIF

 @  3, 2 SAY "   Centro de Costo: " GET m.ctocto WHEN (.F.)
 @  5, 2 SAY "       Descripci¢n: "  GET m.Descri PICTURE "@!"
 @  7, 2 SAY "           Cuentas: "
 @  7,22 GET XCuenta1   PICTURE '9999999999' VALID EMPTY(XCuenta1)  .OR. Val_Cuen(XCuenta1,.T.,7,22)
 @  8,22 GET XCuenta2   PICTURE '9999999999' VALID EMPTY(XCuenta2)  .OR. Val_Cuen(XCuenta2,.T.,8,22)
 @  9,22 GET XCuenta3   PICTURE '9999999999' VALID EMPTY(XCuenta3)  .OR. Val_Cuen(XCuenta3,.T.,9,22)
 @ 10,22 GET XCuenta4   PICTURE '9999999999' VALID EMPTY(XCuenta4)  .OR. Val_Cuen(XCuenta4,.T.,10,22)
 @ 11,22 GET XCuenta5   PICTURE '9999999999' VALID EMPTY(XCuenta5)  .OR. Val_Cuen(XCuenta5,.T.,11,22)
 @ 12,22 GET XCuenta6   PICTURE '9999999999' VALID EMPTY(XCuenta6)  .OR. Val_Cuen(XCuenta6,.T.,12,22)
 @ 13,22 GET XCuenta7   PICTURE '9999999999' VALID EMPTY(XCuenta7)  .OR. Val_Cuen(XCuenta7,.T.,13,22)
 @ 14,22 GET XCuenta8   PICTURE '9999999999' VALID EMPTY(XCuenta8)  .OR. Val_Cuen(XCuenta8,.T.,14,22)
 @ 15,22 GET XCuenta9   PICTURE '9999999999' VALID EMPTY(XCuenta9)  .OR. Val_Cuen(XCuenta9,.T.,15,22)
 @ 16,22 GET XCuent10   PICTURE '9999999999' VALID EMPTY(XCuent10)  .OR. Val_Cuen(XCuent10,.T.,16,22)
 READ 
 RELEASE WINDOW _ctos

 IF LASTKEY() <> 27 .AND. yesno("Confirme el ingreso")
    m.Cuentas = XCuenta1 + XCuenta2 + XCuenta3 + XCuenta4 + XCuenta5 + XCuenta6 +;
		XCuenta7 + XCuenta8 + XCuenta9 + XCuent10
    IF _tt = 2
       Agrega = F_appd()
    ELSE
       Agrega = F_Lock(1)
    ENDIF

    IF Agrega
       GATHER MEMVAR
    ENDIF

    UNLOCK
 ELSE
    GO BOTTOM
    SELECT diax

    RETURN .f.
 ENDIF
 SELECT diax
RETURN .T.


* 13.- Apertura de Auxiliares
FUNCTION ap_aux
*--------------
PARAMETER xxcta
*- apertura de auxiliares en l¡nea
 DEFINE WINDOW ap_aux FROM 3,5 TO 20,75 TITLE " APERTURA DE AUXILIARES " COLOR SCHEME 10
 ACTIVATE WINDOW ap_aux
 CLEAR

 PRIVATE m.Tipo,     m.Descri,  m.Direccion, m.Codigo
 PRIVATE m.Telefono, m.Ruc      m.Observ

 *- Aqu¡ coloca valores por omisi¢n (default)

 SELECT Auxi
 SCATTER MEMVAR BLANK
 m.Tipo   = Diax.TipAux
 m.Codigo = Diax.Registro
 xxalias = ALIAS()

 vreg = IIF(!EOF(),RECNO(),0)

 @  3, 2 SAY "            C¢digo:" + m.Codigo
 @  5, 2 SAY "       Descripci¢n:" GET m.Descri
 @  7, 2 SAY "         Direcci¢n:" GET m.Direccion
 @  9, 2 SAY "          Tel‚fono:" GET m.Telefono
 @ 11, 2 SAY "             R.U.C:" GET m.Ruc
 @ 13, 2 SAY "     Observaciones:" GET m.Observ

 READ 

 IF vreg # 0 .AND. vreg <= RECCOUNT()
    GO vreg
 ENDIF

 IF LASTKEY() <> 27 .AND. yesno("Confirme el ingreso")
    IF f_appd()
       GATHER MEMVAR
    ENDIF
    UNLOCK
 ELSE
    RELEASE WINDOW ap_aux
    SELECT diax
    RETURN .F.
 ENDIF

 RELEASE WINDOW ap_aux
 SELECT diax

RETURN .T.


* 14.- Tipo de Documento de Referencia
FUNCTION V_TipDoc
*----------------
IF LASTKEY() = -2    && F3
   REPLACE TipDcr    WITH TipDoc
   REPLACE DocRef    WITH Comprob
   REPLACE DocAdi    WITH ComAdi
   RETURN .T.
ENDIF
IF EMPTY(TipDcr)
   vAlias = ALIAS()
   SELECT Cuen
   SEEK Diax.Cuenta
   SELECT (vAlias)
   IF Cuen.RefObl .AND. !YesNo("Falta indicar el T.Documento. ¨Continua?")
      RETURN .F.
   ENDIF
   RETURN .T.
ENDIF
IF Val_Para2(TipDcr,'TIPDOC','TipDcr')
   RETURN .T.
ENDIF
RETURN .F.


* 15 Pinta la Cuenta Corriente
FUNCTION CtaCte
*--------------
vAli=ALIAS()
vOrd=ORDER()

IF EMPTY(Diax.CodCtc)
   SELECT Caja
*   DEFINE POPUP parametro FROM 0,40 TO 12,75 PROMPT FIELD CodCtc+' '+Descri TITLE ' Cuentas Corrientes '
*   ON SELECTION POPUP parametro DEACTIVATE POPUP
*   ACTIVATE POPUP parametro

    vcta = SPACE(LEN(Diax.CodCtc))
    SET ORDER TO TAG CAJAS1
    _f_ = Val_Fun('Caja','CodCtc',"CodCtc +' '+ Descri",VCta,1,3,22,'SUBSTR(Descri,1,14)')

   REPLACE Diax.CodCtc WITH VCta

   @ 9,22 SAY LEFT(Caja.Descri,10)

   RELEASE POPUP parametro

ENDIF
SELECT Diax

RETURN


* Imprime documento
PROCEDURE LisComp
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE 'Listado de Notas de Contabilidad' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vLista
vtipo = SPACE(2)
STORE DATE() TO xf1, xf2
IF LASTKEY()=27
   RETURN
ENDIF
rec = RECNO()
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
   idxaux = SYS(3) + '.IDX'
   Sele Diar
   INDEX ON Diar.Comprob+Diar.ComAdi+Diar.CodFte+Diar.Tipo+SUBSTR(Diar.Cuenta,1,2)  TO (idxaux) 
 
   SELECT Comp
   rec = RECNO()
   SET RELATION TO Comp.Comprob+Comp.ComAdi INTO Diar
   SET SKIP TO Diar
   
   idxaux = SYS(3) + '.IDX'
   DO CASE 
   CASE vlista=1                        && Este documento
     INDEX ON Diar.Comprob+Diar.ComAdi+Diar.CodFte  TO (idxaux) FOR Diar.Comprob+Diar.ComAdi+Diar.Codfte = m.Comprob+m.ComAdi+xCodfte
     IF EOF()
	DO STANDBY WITH "No hay registros para procesar"
     ELSE
	  DO REPORTE WITH 2,"voucher1"," Reporte de Vouchers Digitados ",2,.F.,.T.
     ENDIF              
   CASE vlista=2                        && Rango x Fechas y Tipo
     INDEX ON Comp.Comprob+Comp.ComAdi TO (idxaux) FOR SUBSTR(Comp.Comprob,1,2)=ALLTRIM(vtipo) AND BETWEEN(Comp.Fecha,xf1,xf2) 
     IF EOF()
	     DO STANDBY WITH "No hay registros para procesar"
     ELSE
	 DO REPORTE WITH 2,"voucher1"," Reporte de Vouchers Digitados ",2,.F.,.T.
	 ENDIF
   OTHERWISE
     INDEX ON Comp.Comprob+Comp.ComAdi TO (idxaux) FOR BETWEEN(Comp.Fecha,xf1,xf2) &&Comp.fecha>=xf1 .AND. Comp.fecha<=xf2
     IF EOF()
		DO STANDBY WITH "No hay registros para procesar"
	 ELSE
	DO REPORTE WITH 2,"voucher1"," Reporte de Vouchers Digitados " ,2,.F.,.T.
	 ENDIF
 ENDCASE
 SET INDEX TO
 ERASE (idxaux)
* SET INDEX TO
* ERASE (idxaux1)
 ENDIF
 
 SET RELATION OFF INTO COMP
 SELE Comp  
 SET ORDER TO tag Compro7                 
 SELE Diar 
 SET ORDER TO tag Diario11                 
 SELECT COMP
 IF rec > 0
    GO rec
 ENDIF
 DO vista
RETURN


FUNCTION Tdoc
*------------
vAli=ALIAS()
vOrd=ORDER()
SELECT Parma
DEFINE POPUP parametro FROM 0,40 TO 15,75 PROMPT FIELD ALLTRIM(Codigo)+' '+Descri TITLE ' Tipo de Documentos '
ON SELECTION POPUP parametro DEACTIVATE POPUP
ACTIVATE POPUP parametro

m.TipDoc=ALLTRIM(Parma.Codigo)
@ 0,20 SAY ALLTRIM(Parma.Descri)

RELEASE POPUP parametro
SET FILTER TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN


FUNCTION Rebaja
*--------------
 vComprob =Diar.DocRef
 vComAdi  =Diar.DocAdi
 vCuenta  =Diar.Cuenta
 vTipAux  =Diar.TipAux
 vRegistro=Diar.Registro
 vMontos  =Diar.Montos
 vRec=RECNO()
 vOrd=ORDER()
 SET ORDER TO TAG Diario9
 SEEK vComprob+vComAdi+vCuenta+vTipAux+vRegistro
 IF FOUND()
    REPLACE Diar.Estado  WITH SPACE(2)
    REPLACE Diar.SalDPen WITH 0
 ENDIF
 SET ORDER TO TAG vOrd
 GO vRec
 RETURN


*-Valida la existencia de la cuenta.
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


FUNCTION v_Tdoc
*--------------
IF EMPTY(m.TipDoc)
 vAli=ALIAS()
 vOrd=ORDER()
 SELECT Parma
 SET FILTER TO Tipo='TIPDOC'
 DEFINE POPUP parametro FROM 0,40 TO 15,75 PROMPT FIELD ALLTRIM(Codigo)+' '+Descri TITLE ' Tipo de Documentos '
 ON SELECTION POPUP parametro DEACTIVATE POPUP
 ACTIVATE POPUP parametro

 m.TipDoc=ALLTRIM(Parma.CodigoAux) &&SEGUNDO CODIGO DE PARMAE
 @ 0,20 SAY ALLTRIM(Parma.Descri)

 RELEASE POPUP parametro
 SET FILTER TO
 SELECT (vAli)
 SET ORDER TO vOrd
ENDIF
RETURN


PROCEDURE SelPop
*---------------
STORE BAR() TO choice
      DO CASE
	 CASE choice = 1
		  DO NEA
	 CASE choice = 2
	      DO ORDSER
	 CASE choice = 3
	      DO HOJCON
	 CASE choice = 4
	      DO COMPAG
	 CASE choice = 5
	      DO RECING
	 CASE choice = 6
	      DO BOLDEP         
	 CASE choice = 7
	      DO NOTCAR
	 CASE choice = 8
	      DO NOTABO
	 CASE choice = 9
	      DO FACTURA
	 CASE choice = 10
	      DO HojMod
	 CASE choice = 11
	      DO HojAnuCh
	 CASE choice = 12
	      DO Ordcom
	 CASE choice = 13
	      DO CheGir
	 CASE choice = 14
		  DO CheEnt
	 CASE choice = 15
*		  DO OrdSerCP
	 CASE choice = 16
		  DO Rever
	 CASE choice = 17
		  DO ReverTs
	 CASE choice = 18
		  DO RevyAnu
	 CASE choice = 19
		  DO Pecosa
	ENDCASE

DEACTIVATE POPUP RepT
RELEASE POPUP  Rept

RETURN

*---------------
PROCEDURE ORDSER
*---------------
USE OrdSer IN 0 ORDER TAG OrdSer1 ALIAS OrdSer
USE AstOrd IN 0 ORDER TAG AstOrd1 ALIAS AstOrd
USE Promae IN 0 ORDER TAG Promae1 Alias Promae
SELECT OrdSer

GO TOP
   
IF EOF()
	DO STANDBY WITH 'No Existe Documentos ORDEN/SERVICIO'
	USE IN OrdSer
	USE IN AstOrd
	USE IN Promae
	RETURN .F.
ENDIF
   
SELE OrdSer
SET FILTER TO MONTH(FECOS)=VAL(FMES)
GO TOP
IF EOF()
	DO standby WITH 'No existen Ordenes de Servicio del Mes '+FMES
	USE IN OrdSer
	USE IN AstOrd
	USE IN Promae
	RETURN .F.
ENDIF
DEFINE POPUP EliOS PROMPT FIELD ;
	NumOS+' '+DTOC(FecOS)+' '+CodCaD+' '+TRANSF(ValTot,'999,999.99');
	+' '+LEFT(Val_Fun('ProMae', 'NomPro', 'NomPro', CodPrv), 20)

ON SELECTION POPUP EliOS DEACTIVATE POPUP

ACTIVATE POPUP EliOS

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Periodo+' '+NumOS+' '+ Codfte)
		SELE Parma
		SEEK 'TIPDOC'+'ORS'
		IF !FOUND()
			=STANDBY('Parametro de Ordenes de Servicio no Inicializados')
			USE IN OrdSer
			USE IN AstOrd
			USE IN Promae
			RETURN
		ENDIF
		vTip   = ALLTRIM(Parma.CodigoAux)
		cTip   = ALLTRIM(Parma.Codigo)
		cClave = vTip+PADL(OrdSer.NumOS,5,'0')+OrdSer.NumMes+'  '
		IF !Hay_Doc(cClave,cTip)			&& Se verifica si la Prden de Servicio ya ha sido integrada
			m.Item = 0
			m.Periodo = OrdSer.Periodo
			m.Codfte  = OrdSer.CodFte
			m.fecEmi  = OrdSer.FecOS
			m.TipDoc  = vTip
			m.Comprob = ALLTRIM(vTip)+PADL(OrdSer.NumOS,5,'0')+fMes
			m.ComAdi  = ''
			m.Fecha   = OrdSer.FecOS
			m.Glosa1  = 'O/S Emitida N§ '+OrdSer.Periodo+'.'+OrdSer.NumOS+' H/C N§: '+OrdSer.numhc+'.'+OrdSer.perhc
			m.Glosa2  = 'Desde fecha '+DTOC(m.Fecha) +'  Fuente : '+m.codfte
			m.Glosa3  = OrdSer.docref+' Destino: '+ALLTRIM(OrdSer.Destino)
			m.Glosa4  = OrdSer.observa
			m.CodCad  = OrdSer.codcad
			m.TotDeb  = ROUND(OrdSer.valtot,2)
			m.TotHab  = ROUND(OrdSer.valtot,2)
			m.Periodofte = OrdSer.Periodo
			m.Numdocfte  = OrdSer.NumOS
			m.Docref  = OrdSer.Numhc
			m.Tipdcr    = IIF(!EMPTY(OrdSer.NumHc),'03','  ')
			m.Nummesref = OrdSer.Perhc
			
			xTotDeb = 0
			xTotHab = 0
			
			SELECT Comp
			IF f_appd()
				GATHER MEMVAR
			ENDIF
			SELE AstOrd
			SEEK OrdSer.Periodo+OrdSer.NumMes+OrdSer.NumOS+'O/S'
			
			IF FOUND()
				SCAN WHILE Periodo=OrdSer.Periodo AND NumMes=OrdSer.NumMes AND NumRef=OrdSer.NumOS AND TipDoc='O/S'
					m.Item  = m.Item + 1
					m.Orden = '01'
					m.NumMov  = 1
					m.FecEmi  = OrdSer.FecOS
					m.TipAux  = '20'
					m.Registro = OrdSer.codprv
					m.Coddep  = '111000'
					m.codfte  = OrdSer.CodFte
					m.Docref  = OrdSer.Numhc
					m.Tipdcr  = IIF(!EMPTY(OrdSer.NumHc),'03','  ')
					m.Nummesref = OrdSer.Perhc
					m.Tipo    = AstOrd.TipCta
					m.Cuenta  = AstOrd.CodCta
					m.Montos  = IIF(m.Tipo='D',AstOrd.MtoDeb,AstOrd.MtoHab)
					
					SELECT Diar
					IF f_appd()
						GATHER MEMVAR
						UNLOCK
					ENDIF
					
					xTotDeb = xTotDeb+IIF(m.Tipo='D',AstOrd.MtoDeb,0)
					xTotHab = xTotHab+IIF(m.Tipo='H',AstOrd.MtoHab,0)
					
					SELE AstOrd
				ENDSCAN
				
			ENDIF
			
			
			SELE Comp
			
			SEEK m.Comprob+m.ComAdi
			
			IF f_Lock(1)
				REPLACE TotDeb WITH xTotDeb
				REPLACE TotHab WITH xTotHab
				UNLOCK
			ENDIF
		ENDIF
 	ENDIF
ENDIF
USE IN OrdSer
USE IN AstOrd

RETURN

PROCEDURE ORDSERCP
*-----------------
USE OrdSer IN 0 ORDER TAG OrdSer1 ALIAS Orden
USE HojCon IN 0 ORDER TAG Hojcon1 ALIAS hoja
USE ComPag IN 0 ORDER TAG ComPag1 ALIAS ComPag
USE AstOrd IN 0 ORDER TAG AstOrd2 ALIAS AstOrd
USE Promae IN 0 ORDER TAG Promae1 Alias Promae

SELECT ComPag

GO TOP

IF EOF()
	DO STANDBY WITH 'No Existe Documentos ORDEN/SERVICIO CON CP'
	USE IN Orden
	USE IN ComPag
	USE IN Hoja
	USE IN AstOrd
	USE IN Promae
	RETURN .F.
ENDIF
SELE Orden
SET RELATION TO PerHC+NumHC INTO Hoja
SELE Hoja
SET RELATION TO NumMesCP+NumCP INTO ComPag
SELE Orden
SET FILTER TO ComPag.NumMes=fMes and Estado#'99' AND ComPag.estado#'99' AND !EMPTY(Hoja.NumMesCP) AND !EMPTY(Hoja.NumCP)
GO TOP
IF EOF()
	DO standby WITH 'No existen Ordenes de Servicio Pagadas en el Mes '+FMES
	USE IN Orden
	USE IN ComPag
	USE IN Hoja
	USE IN AstOrd
	USE IN Promae
	RETURN .F.
ENDIF

DEFINE POPUP EliOSCP PROMPT FIELD ;
	Orden.NumOS+' '+DTOC(Orden.FecOS)+' '+STR(Orden.ValTot,12,2)+' '+ComPag.CodCaD+' '+Hoja.NumHC+' '+Hoja.CodPart+' '+'C/P '+ComPag.NumCP+' '+STR(ComPag.Import,13,2)
*	+' '+LEFT(Val_Fun('ProMae', 'NomPro', 'NomPro', CodPrv), 20)

ON SELECTION POPUP EliOSCP DEACTIVATE POPUP

ACTIVATE POPUP EliOSCP
RELEASE  POPUP EliOSCP

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Periodo+' '+NumOS+' '+ Codfte)
		SELE Parma
		SEEK 'TIPDOC'+'OSC'
		IF !FOUND()
			=STANDBY('Parametro de Ordenes de Servicio Pagadas no Inicializados')
			USE IN Orden
			USE IN ComPag
			USE IN Hoja
			USE IN AstOrd
			USE IN Promae
			RETURN
		ENDIF
		vTip   = ALLTRIM(Parma.CodigoAux)
		cClave = PADR(vTip+PADL(Orden.NumOS,5,'0')+ComPag.NumMes,9,' ')+PADL(DAY(ComPag.FecCP),2,'0')
		IF !Hay_Doc(cClave,'OSC')			&& Se verifica si la O/S Pagadas ya ha sido integradas
			IF Hoja.TipDoc#'O/S'
				LOOP
			ENDIF
			m.TipDoc     = vTip
			m.Comprob    = vTip+PADL(Orden.NumOS,5,'0')+fMes
			m.ComAdi     = PADL(DAY(ComPag.FecCP),2,'0')
			m.Fecha      = ComPag.FecCP
			m.Glosa1     = 'O/S N§: '+Orden.numOS+'.'+PADL(MONTH(Orden.fecOS),2,'0')+' Afectada con H/C: '+Hoja.numhc+'.'+Hoja.NumMes
			m.Glosa2     = 'Pagada con C/P: '+ComPag.Numcp+'.'+ComPag.nummes+' por el Importe de: '+STR(ComPag.Import,13,2)
			m.Glosa3     = ComPag.Glosa
			m.Glosa4     = ComPag.observ
			m.CodCad     = ComPag.codcad
			m.codfte     = ComPag.codfte
			m.TotDeb	 = 0
			m.TotHab	 = 0
			m.Periodofte = Hoja.PerRef
			m.Numdocfte  = Hoja.NumRef
			m.Docref     = ComPag.Numcp
			m.Tipdcr     = Val_para6('C/P','TIPDOC')
			m.Nummesref  = ComPag.NumCP
			
			SELECT Comp
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			
			xTotDeb = 0
			xTotHab = 0
			m.Item  = 0
			
			SELE AstOrd
			SEEK ComPag.Periodo+ComPag.NumMes+Orden.NumOS+'OSC'+ComPag.NumCP
			
			SCAN WHILE Periodo=ComPag.Periodo AND NumMes=ComPag.NumMes AND NumRef=Orden.NumOS AND TipDoc='OSC' AND NumCP=Compag.NumCP
				m.Item      = m.Item + 1
				m.orden     = '01'
				m.NumMov    = 1
				m.FecEmi    = m.fecha
				m.TipAux    = IIF(!EMPTY(Hoja.CodPrv),'20',IIF(!EMPTY(Hoja.CodEmp),'30','09'))
				m.Registro  = IIF(!EMPTY(Hoja.CodPrv),Hoja.CodPrv,IIF(!EMPTY(Hoja.CodEmp),Hoja.CodEmp,Hoja.CodOtr))
				m.Coddep    = '111000'
				m.codfte    = ComPag.CodFte
				m.Docref    = ComPag.Numcp
				m.Nummesref = ComPag.NUMMES
				m.Tipo      = AstOrd.TipCta
				m.Cuenta    = AstOrd.Codcta
				m.Montos    = IIF(m.Tipo='D',MtoDeb,MtoHab)
				
				xtotdeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
				xtothab = xTotHab + IIF(m.Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF F_APPD()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstOrd
			ENDSCAN
		ENDIF
 	ENDIF
ENDIF
USE IN Orden
USE IN ComPag
USE IN Hoja
USE IN AstOrd
USE IN Promae

RETURN


*----------------
PROCEDURE POLIZA
*----------------
* Carga valores de la Poliza de Entrada/Salida
*---------------------------------------------

USE Poliza IN 0 ORDER TAG Poliza3 ALIAS Poliza
USE ItePol IN 0 ORDER TAG ItePol3 ALIAS ItePol
USE AstOrd IN 0 ORDER TAG AstOrd1 ALIAS AstOrd

SELE Poliza

IF EOF()
	DO STANDBY WITH ' No existe documentos Polizas '
	USE IN Poliza
	USE IN ItePol
	USE IN AstOrd
    RETURN .F.
ENDIF
   
SET FILTER TO PADL(MONT(fecemi),2,'0')=ALLTRIM(fMes)
GO TOP
IF EOF()
	DO standby WITH 'No existen Polizas del Mes '+FMES
	USE IN Poliza
	USE IN ItePol
	USE IN AstOrd
	RETURN .F.
ENDIF

DEFINE POPUP EliPES PROMPT FIELD ;
	Poliza.NumPol+' '+Poliza.Tippol+' '+Poliza.Analisis+' '+Poliza.Codfte+' '+Poliza.Periodo+' '+DTOC(Poliza.Fecini)+' '+DTOC(Poliza.FecFin)+' '+TRANSF(Poliza.ValTot,'999,999,999.99')
	
ON SELECTION POPUP EliPES DEACTIVATE POPUP
ACTIVATE POPUP EliPES
RELEASE  POPUP EliPES

SELE POLIZA

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Periodo+' '+NumPol+' '+ TipPol+' '+Codfte)
		SELE Parma
		SEEK 'TIPDOC'+IIF(Poliza.TipPol='E','P/E','P/S')
		IF !FOUND()
			=STANDBY('Parametro de Polizas de Entrada o Salida no Inicializados')
			USE IN Poliza
			USE IN ItePol
			USE IN AstOrd
			RETURN
		ENDIF
		SELE Poliza
		vTip   = ALLTRIM(Parma.CodigoAux)
		cTip   = ALLTRIM(Parma.Codigo)
		vOrden = IIF(TipPol='E','01','01')
		cClave = vTip+PADL(Poliza.NumPol,5,'0')+fMes+'  '
		IF !Hay_Doc(cClave,cTip)			&& Se verifica si la Poliza ya ha sido integrada
			m.TipDoc  = vTip
			m.Comprob = vTip+PADL(Poliza.NumPol,5,'0')+fMes
			m.ComAdi  = ''
			m.Fecha   = Poliza.FecEmi
			m.Glosa1  = IIF(Itepol.Tippol="E",'Poliza de Entrada','Poliza de Salida')+' '+Poliza.Periodo+' '+Poliza.Numpol+' '+IIF(Poliza.tippol='E','Numero de O/C:','Numero Pecosa:')+' '+itepol.TipRef+ItePol.NumRef+IIF(ITEPOL.IGV>0,'  Con IGV.',' ')
			m.Glosa2  = 'Desde '+DTOC(Poliza.FecIni)+'  Hasta '+DTOC(Poliza.FecFin)+'  Fuente : '+Poliza.codfte
			m.Glosa3  = IIF(!EMPTY(Itepol.NummesHc),'H/C: '+ItePol.NumMesHC+' '+Itepol.NumHC,' ')+' '+ALLTRIM(Itepol.Destino)
			m.Glosa4  = ''
			m.CodCad  = Itepol.CodCad
			m.codfte  = Poliza.codfte
			m.Periodofte = Itepol.Perref
			m.Numdocfte  = Itepol.NUMPOL
			m.Docref     = Itepol.Numhc
			m.Tipdcr     = IIF(!EMPTY(Itepol.NummesHc),'03','  ')
			m.Nummesref  = Itepol.Nummeshc
			
			SELECT Comp
			IF f_appd()
				GATHER MEMVAR
			ENDIF
			m.Item  = 0
			xTotDeb = 0
			xTotHab = 0
			
			SELE Itepol
			SEEK Poliza.Periodo+Poliza.Numpol+Poliza.Tippol
			SCAN WHILE Itepol.Periodo=Poliza.Periodo AND Itepol.Numpol=Poliza.Numpol AND Itepol.Tippol=Poliza.Tippol
				xNum = IIF(EMPTY(ItePol.IGV),2,3)
				xNum = IIF(EMPTY(dCuenta) AND EMPTY(hCuenta),0,xNum)
				xNum = IIF(ItePol.TipPol='E',xNum,2)
				FOR i = 1 TO xNum
					m.Item      = Item + 1
					m.tipdcr    = Val_para6(Itepol.TipRef,'TIPDOC')
					m.Orden     = vorden
					m.NumMov    = 1
					m.FecEmi    = m.fecha
					m.TipAux    = IIF(!EMPTY(ItePol.codprv),'20','  ')
					m.Registro  = IIF(!EMPTY(ItePol.codprv),ItePol.codprv,ItePol.Coddep)
					m.Coddep    = '111000'
					m.codfte    = Poliza.Codfte
					m.Codart    = ItePol.CodGen+' '+ItePol.CodCla+' '+ItePol.CodDet
					m.Descri    = ALLTRIM(ItePol.Descri)
					m.Docref    = ItePol.NumRef
					m.Tipdcr    = ItePol.TipRef
					
					DO CASE
						CASE i = 1
							m.Tipo   = 'D'
							m.Cuenta = ItePol.DCuenta
							m.Montos = ItePol.ValTot-ItePol.IGV
						CASE i = 2 AND xNum=3
							m.Tipo   = 'D'
							m.Cuenta = '4010401000'
							m.Montos = ItePol.Igv
						CASE (i = 2 AND xNum=2) OR (i = 3 AND xNum=3)
							m.Tipo   = 'H'
							m.Cuenta = ItePol.HCuenta
							IF ItePol.TipPol='S'
								m.Montos = ItePol.ValTot-ItePol.IGV
							 ELSE
								m.Montos = ItePol.ValTot
							ENDIF
					ENDCASE
					SELECT Diar
					IF f_appd()
						GATHER MEMVAR
						UNLOCK
					ENDIF
					xTotDeb = xTotDeb+IIF(m.Tipo='D',m.Montos,0)
					xTotHab = xTotHab+IIF(m.Tipo='H',m.Montos,0)
					SELE Itepol
				ENDFOR
			ENDSCAN
			SELE AstOrd
			SEEK Poliza.Periodo+fMes+Poliza.NumPol+IIF(Poliza.Tippol='E',"P/E","")
			IF Poliza.TipPol='E'
				SEEK Poliza.Periodo+fMes+Poliza.Numpol+'P/E'
				SCAN WHILE AstOrd.Periodo=Poliza.Periodo AND astOrd.NumMes=fMes AND AstOrd.NumRef=Poliza.NumPol AND AstOrd.TipDoc='P/E'
					m.Item      = Item + 1
					m.Orden      = '02'
					m.NumMov     = 1
					m.TipAux     = ''
					m.Registro   = ''
					m.Coddep     = '111000'
					m.Codart     = ''
					m.Descri     = ''
					m.Docref     = ''
					m.Tipo       = AstOrd.TipCta
					m.Cuenta     = AstOrd.CodCta
					m.Montos     = IIF(m.Tipo='D',AstOrd.MtoDeb,AstOrd.MtoHab)
					
					SELECT Diar
					IF f_appd()
						GATHER MEMVAR
						UNLOCK
					ENDIF
					
					xTotDeb = xTotDeb+IIF(m.Tipo='D',m.Montos,0)
					xTotHab = xTotHab+IIF(m.Tipo='H',m.Montos,0)
					SELE AstOrd
				ENDSCAN
			ENDIF
			
			Sele Comp
			SEEK ALLTRIM(m.comprob)
			IF RLOCK()
				REPLACE Comp.totdeb     WITH xtotdeb
				REPLACE Comp.tothab     WITH xtothab
				UNLOCK
				xtotdeb = 0
				xtothab = 0
				m.Item  = 0
			ENDIF
		ENDIF   
	ENDIF
ENDIF && de Repro
 
USE IN Poliza
USE IN ItePol
USE IN AstOrd

RETURN

*---------------
PROCEDURE HOJCON
*---------------
*- Carga valores de Hoja de Control

SET MEMOWIDTH TO 50
     

USE HojCon IN 0 ORDER TAG HojCon1 ALIAS Hoja
USE IteHC  IN 0 ORDER TAG IteHC1  ALIAS IteHC   
USE IteCla IN 0 ORDER TAG IteCla1 ALIAS IteCla
USE AstPre IN 0 ORDER TAG AstPre5 ALIAS AstPre
USE OrdSer IN 0 ORDER TAG OrdSer1 ALIAS OrdSer		&& Todo lo Refer. a OrdSer debe Salir
USE Promae IN 0 ORDER TAG Promae1 Alias Promae
SELECT Hoja
SET RELATION TO NumMes+NumHC INTO IteHC
GO TOP

IF EOF()
	DO STANDBY WITH ' No existe documentos H/C '
	USE IN Hoja
	USE IN IteHC
	USE IN IteCla
	USE IN AstPre
	USE IN OrdSer
	USE IN Promae
    RETURN .F.
ENDIF
   
SET FILTER TO NUMMES = ALLTRIM(Fmes)
GO TOP
IF EOF()
	DO standby WITH 'No existen H/C del Mes '+FMES
	USE IN Hoja
	USE IN IteHC
	USE IN IteCla
	USE IN AstPre
	USE IN OrdSer
	USE IN Promae
	RETURN .F.
ENDIF
   
DEFINE POPUP EliHC PROMPT FIELD ;
	NumHC+' '+NumMes+' '+TipDoc+' '+NumRef+' '+DTOC(FecRef)+;
	' ' + TRANSF(ImpTot,'999,999.99') + ' ' +CodPart+' '+LEFT(Val_Fun('ProMae', 'NomPro', 'NomPro', CodPrv), 20)

ON SELECTION POPUP EliHC DEACTIVATE POPUP
ACTIVATE POPUP EliHC
RELEASE  POPUP EliHC

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Periodo+' '+Numhc+' '+ NummeS+' '+Codfte)
		SELE Parma
		SEEK 'TIPDOC'+'H/C'
		IF !FOUND()
			=STANDBY('Parametro de Hoja de Control no Inicializado')
			USE IN Hoja
			USE IN IteHC
			USE IN IteCla
			USE IN AstPre
			USE IN OrdSer
			USE IN Promae
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cClave = vTip+PADL(Hoja.NumHC,5,'0')+Hoja.NumMes+'  '
		IF !Hay_Doc(cClave,'H/C')			&& Se verifica si a H/C ya ha sido integrada
			m.TipDoc     = VTIP
			m.Comprob    = VTIP+'0'+Hoja.NumHC+Fmes
			m.comadi     = ''
			m.fecha      = Hoja.FecHC
			m.Glosa1     = ALLTRIM(Hoja.DesRef)+' '+Hoja.Destino
			m.Glosa2     = Hoja.Observa
			m.Glosa3     = MLINE(Hoja.Observ,1)+MLINE(Hoja.Observ,2)+MLINE(Hoja.Observ,3)+MLINE(Hoja.Observ,4)
			m.Glosa4     = MLINE(Hoja.Observ,5)+MLINE(Hoja.Observ,6)+MLINE(Hoja.Observ,7)+MLINE(Hoja.Observ,8)
			m.CodCtc     = Hoja.CodCtC
			m.CodCad     = Hoja.CodCad
			m.codfte     = Hoja.codfte
			m.tipObr     = Hoja.tipObr
			m.CodObra    = Hoja.CodObra
			m.Periodofte = Hoja.Periodo
			m.Nummesfte  = Hoja.Nummes
			m.Numdocfte  = hoja.NumHC
			m.Periodoref = Hoja.Perref
			m.DocRef     = Hoja.NumRef
			m.NummesRef  = SUBSTR(DTOC(Hoja.FecRef),4,2)
			m.TipDcr     = Val_para6(Hoja.TipDoc,'TIPDOC')
			
			SELECT Comp
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			*-Se generan los Asientos Presupuestales por cada una de las partidas
			*-ya sea generica o anal¡tica
			
			m.Item = 0
			xtotdeb= 0
			xtothab= 0
			
			SELE AstPre
			SEEK Hoja.NumMes + Hoja.NumHC + 'H/C'
			SCAN WHILE IteHC.NumHC = AstPre.NumRef .AND. IteHC.NumMes = AstPre.NumMes AND AstPre.TipDoc='H/C'
				m.Item = m.Item + 1
				m.Orden 	 = '10'
				m.NumMov     = 1
				m.FecEmi     = m.Fecha
				m.TipAux     = IIF(!EMPTY(Hoja.CodPrv),'20',IIF(!EMPTY(Hoja.CodEmp),'03','09'))
				m.Registro   = IIF(!EMPTY(Hoja.CodPrv),Hoja.CodPrv,IIF(!EMPTY(Hoja.CodEmp),Hoja.CodEmp,Hoja.CodOtr))
				m.DocRef     = Hoja.NumRef
				m.TipDcr     = m.tipdcr
				m.CodCtc     = Hoja.CodCtC
				m.Coddep     = '112000'
				m.Codpart    = IteHC.Codpart
				m.Periodoref = Hoja.Perref
				m.Tipo		 = AstPre.Tipo
				m.Cuenta     = IIF(AstPre.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
				m.Montos     = IIF(AstPre.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
				
				xtotdeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
				xtothab = xTotHab + IIF(m.Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF F_APPD()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstPre
			ENDSCAN
			
			*- Genera asientos patrimoniales con las partidas del clasificador
			*- por objeto del gasto.
			
			SELE IteHC
			SEEK Hoja.nuMmes+Hoja.numhc
			SCAN WHILE IteHC.NumHC = Hoja.NumHC .AND. IteHC.NumMes = Hoja.NumMes AND IteHC.Estado#'92'
				SELE IteCla
				SEEK IteHc.CodPart+IteHC.CodCla
				SELE IteHC
				IF !EMPTY(IteCla.CuentaD) AND !EMPTY(IteCla.CuentaH)
					xigv=0
					
					xnum=IIF(IteHC.Igv="S",3,2)
					xNum = IIF(IteHC.TipDoc='RNF',IIF(IteHC.IIgv>0,3,xnum),xNum)
					xigv=IteHC.IIgv
					
					FOR vCon=1 TO xnum
						m.Item = m.Item + 1
						DO CASE
							CASE IteHC.TipDoc='O/C'
								m.Orden = '07'
							CASE IteHC.TipDoc='RNF'
								m.Orden = '06'
							CASE (IteHC.Codpart='531124' AND IteHC.CodCla='06') OR (IteHC.Codpart='531171' AND IteHC.CodCla='06')
								m.Orden = '04'
							CASE (IteHC.Codpart='651124' AND IteHC.CodCla='01')
								m.Orden = '04'
							CASE LEFT(IteHC.Codpart,2)='65' AND IteHC.CodCad#'0026'
								m.Orden = '02'
							CASE itehc.TipDoc = 'PLA' OR IteHC.CodPart='210101020103' OR IteHC.CodPart='210101020101' OR IteHC.CodPart='2101100101' OR IteHC.CodPart='2101100102' OR IteHC.CodPart='2103010105'
*							CASE itehc.TipDoc = 'PLA'
								m.Orden = '03'
							OTHERWISE
								m.Orden = '01'
						ENDCASE
						m.NumMov     = 1
						m.FecEmi     = m.fecha
						m.DocRef     = Hoja.NumRef
						m.TipDcr     = vtip
						m.CodCtc     = Hoja.CodCtC
						m.CodFte     = ITEHC.codfte
						m.Coddep     = '112000'
						m.Codpart    = Itehc.Codpart
						m.Periodoref = Hoja.perref
						m.TipDoc     = itehc.TipDoc
						m.Montos     = IteHC.ValPart
						
						DO CASE
							CASE vCon = 1
								m.Tipo   = 'D'
								m.Cuenta = PADR(ALLTRIM(IteCla.CuentaD),15,'0')
								m.Montos = IteHC.ValPart-xIgv
							CASE vCon = 2 AND xNum=3
								m.Tipo   = 'D'
								m.Cuenta = '210101050100000'
								m.Montos = xIgv
							CASE (vCon = 2 AND xNum=2) OR (vCon = 3 AND xNum=3)
								m.Tipo   = 'H'
								m.Cuenta = PADR(ALLTRIM(IteCla.CuentaH),15,'0')
								m.Montos = IteHC.ValPart
						ENDCASE
						
						xtotdeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
						xtothab = xTotHab + IIF(m.Tipo='H',m.Montos,0)
						
						SELE Diar
						IF F_APPD()
							GATHER MEMVAR
							UNLOCK
						ENDIF
					ENDFOR
				ENDIF
				SELE IteHC
			ENDSCAN
			Sele Comp
			IF RLOCK()
				REPLACE Comp.totdeb WITH xtotdeb
				REPLACE Comp.tothab WITH xtothab
			ENDIF
			
			SELE Hoja
			SET RELATION OFF INTO IteHC
		ENDIF
	ENDIF
ENDIF

USE IN Hoja
USE IN IteHC
USE IN IteCla
USE IN AstPre
USE IN OrdSer
USE IN Promae
RETURN


FUNCTION Hay_Doc
*----------------
PARAMETERS cClave,cTip
* Se Verifica la existencia del Documentos a Integrar
calias = ALIAS()
SELE Comp
SET ORDER TO Compro13
SEEK cClave
mRet=.F.

IF FOUND()
	IF yesno('Documento '+cTip + ' '+ SUBSTR(cClave,4,4)+'-'+SUBSTR(cClave,8,2)+' Ya integrado, Reprocesa?')
		DO ESPERA WITH 1,'Eliminando Asientos de '+ cTip +' '+SUBSTR(cClave,4,4)+' procesado(a).'
		
		SELE Diar
		SET ORDER  TO tag Diario20
		SEEK cClave
		SCAN WHILE ALLTRIM(Comprob)+ComAdi = cClave
			IF !f_lock(1)
				UNLOCK ALL
				RETURN
			ENDIF
			SELECT Diar
			IF RLOCK()
				DELETE NEXT 1
				UNLOCK
			ENDIF   
		ENDSCAN
		
		SELECT Comp
		SEEK cClave
		SCAN WHILE ALLTRIM(Comprob)+ComAdi = cClave
			IF !f_lock(1)
				UNLOCK ALL
				RETURN
			ENDIF
			
			SELECT Comp
			IF RLOCK()
				DELETE NEXT 1
				UNLOCK
			ENDIF
			SELECT Comp
		ENDSCAN
		DO Espera WITH 2
		mRet=.F.
	 ELSE
		mRet=.T.
	ENDIF
ENDIF
SELE (cAlias)

RETURN mret


PROCEDURE COMPAG
*------------------------------------
* Carga valores de COMPROBANTES DE PAGO
* ESTADO 20 CHEQUE GIRADO
*        10 RETENCIONES
* Tipaux : 20 cuando es Proveedor
*          30 cuando es Empleado
*          08 cuando es Retenci¢n 
*----------------------------------------------
xtotdeb=0
xtothab=0

USE ComPag IN 0 ORDER TAG ComPag1  ALIAS ComPag
USE AstPat IN 0 ORDER TAG AstPat34 ALIAS AstPat
USE AstPre IN 0 ORDER TAG AstPre26 ALIAS AstPre
      
SELECT ComPAG
GO TOP  
IF EOF()
	DO STANDBY WITH 'No existe Documentos C/P'
	USE IN ComPag
	USE IN AstPat
	USE IN AstPre
   RETURN .F.
ENDIF

SELECT CompAG
SET RELATION TO NumMes+NumCP        INTO AstPat 
SET RELATION TO NumMes+NumCP        INTO AstPre ADDITIVE
   
SELE Compag
SET FILTER TO NumMes = ALLTRIM(FMES)
GO TOP
IF EOF()
	DO standby WITH 'No existen Comprobantes del Mes '+FMES
	USE IN ComPag
	USE IN AstPat
	USE IN AstPre
	RETURN .F.
ENDIF

DEFINE POPUP EliCP PROMPT FIELD ;
	NumCP+'.'+NumMes+' '+DTOC(FecCP)+' '+CodCad+' '+ALLTRIM(CodCTC)+' '+TRANSF(Import,'999,999,999.99') ;
	+' '+DocRef+' '+NumRef

ON SELECTION POPUP EliCP DEACTIVATE POPUP
ACTIVATE POPUP EliCP
RELEASE  POPUP EliCP

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Periodo+' '+NumCP+' '+ NuMmes+' '+CodCtc)
		SELE Parma
		SEEK 'TIPDOC'+'C/P'
		IF !FOUND()
			=STANDBY('Parametro de Comprobante de pago no icializado')
			USE IN ComPag
			USE IN AstPat
			USE IN AstPre
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cClave = vTip+PADL(ComPag.NumCP,5,'0')+ComPag.NumMes+'  '
		
		IF !Hay_Doc(cClave,'C/P')		&& Se verifica si el C/P ya ha sido integrada
			
			m.TipDoc     = VTip
			m.Comprob    = VTip+PADL(ComPag.NumCP,5,'0')+fMes
			m.Comadi     = '' 
			m.Fecha      = Compag.FecCP
			m.Glosa1     = SUBSTR(ComPag.Glosa,1,100)
			m.Glosa2     = RIGHT(ComPag.Glosa,100)
			m.Glosa3     = ComPag.Observ
			m.Glosa4     = ALLTRIM(ComPag.Nompre)+' '+Compag.Codctc
			m.CodCtc     = ComPag.CodCtc
			m.CodCaD     = ComPag.CodCaD
			m.CodFTE     = ComPag.CodFTE
			m.tipobr     = ''
			m.CodObra    = ''
			m.nummesfte  = Compag.Nummes
			m.periodofte = Compag.Periodo
			m.numdocfte  = Compag.Numcp
			m.Nummesref  = Compag.NummesHC
			m.Docref     = Compag.Numref
			m.Tipdcr     = Val_para6(Compag.Docref,'TIPDOC')
			
			SELECT Comp
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			
			xtotdeb = 0
			xtothab = 0
			m.Item  = 0
			
			*-Cuentas Patrimoniales
			SELECT AstPat
			SEEK Compag.NumMes+Compag.NumCP       &&  +vCodCtc
			SCAN WHILE  ComPag.NumCP = AstPat.NumRef .AND. ComPag.NumMes = AstPat.NumMes    && .AND. vCodCtc = AstPat.CodCtc
				m.Item      = m.Item + 1
				m.NumMov    = 1
				m.FecEmi    = Compag.FecCP
				m.Cuenta    = AstPat.CodCta
				m.Tipo      = AstPat.TipCta
				m.TipAux    = IIF(!EMPTY(ComPag.CodPrv),'20',IIF(!EMPTY(ComPag.CodEmp),'03',IIF(!EMPTY(Compag.CodRet),'08',IIF(!EMPTY(Compag.CodOtr),'09','  '))))
				m.Registro  = IIF(!EMPTY(ComPag.CodPrv),Compag.Codprv,IIF(!EMPTY(ComPag.CodEmp),ComPag.CodEmp,IIF(!EMPTY(Compag.CodRet),ComPag.CodRet,IIF(!EMPTY(Compag.CodOtr),Compag.CodOtr,'  '))))
				m.CodCtc    = Compag.CodCtc
				m.CodFte    = Compag.CodFTE
				m.CodDep    = '113000'
				m.tipobr    = ''
				m.CodObra   = ''
				m.Reten     = AstPat.Ret
				m.Montos    = IIF(m.Tipo='D',AstPat.MtoDeb,AstPat.MtoHab)
				
				xtotDeb=xtotDeb+IIF(m.Tipo='D',m.Montos,0)
				xtotHab=xtotHab+IIF(m.Tipo='H',m.Montos,0)
				
				IF m.Tipo='H'
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','58','57')
				 ELSE
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','57','58')
				ENDIF	
				IF RETEN='S'
					m.orden = '56'
				ENDIF
				
				SELECT Diar
				IF F_APPD()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstPat
			ENDSCAN
			
			*-Cuentas Presupuestales de ejecuci¢n
			SELECT AstPre
			SEEK fMes+ComPag.NumCP      &&  +vCodCtc
			SCAN WHILE ComPag.NumCP = AstPre.NumRef .AND. ComPag.NumMes = AstPre.NumMes    && .AND. vCodCtc = AstPre.CodCtc
				m.Item = vItem + 1
				m.ORDEN     = '32'
				m.Comadi    = ''
				m.NumMov    = 1
				m.Tipo      = AstPre.Tipo
				m.CodCtc    = Compag.CodCtc
				m.Codpart   = Astpre.codpart
				m.Montos   = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
				m.Cuenta   = IIF(m.Tipo='D',Astpre.CtaDeb,Astpre.CtaHab)
				
				SELECT Diar
				IF f_appd()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				
				xtotdeb = xtotdeb + IIF(m.Tipo='D',m.montos,0)
				xtotHab = xtotHab + IIF(m.Tipo='H',m.montos,0)
				SELE AstPre
			ENDSCAN
			
			Sele Comp
			IF RLOCK()
				REPLACE Comp.totdeb     WITH xtotdeb
				REPLACE Comp.tothab     WITH xtothab
				UNLOCK
			ENDIF
			SELE Compag
			SET RELATION OFF INTO AstPat
			SET RELATION OFF INTO AstPre
		ENDIF
	ENDIF
ENDIF
      
USE IN ComPag
USE IN AstPat
USE IN AstPre
   
RETURN

PROCEDURE BOLDEP 			&&BOLETA DE DEPOSITO
*----------------
USE AstPat IN 0 ORDER TAG AstPat7 ALIAS AstPat    
USE MovBco IN 0 ORDER TAG MovBco7 ALIAS MovBco
SELECT MovBco

SET FILTER TO MONTH(FECHA)=VAL(ALLTR(Fmes))
GO TOP
IF EOF()
	DO STANDBY WITH 'No existe Documentos B/D'
	USE IN AstPat
	USE IN MovBco
	RETURN .F.
ENDIF

DEFINE POPUP EliCP PROMPT FIELD ;
	Numero+' '+Nummes+' '+DTOC(Fecha)+'  '+CodCtc+'  '+TRANSF(Monto,'999,999,999.99')

ON SELECTION POPUP EliCP DEACTIVATE POPUP
ACTIVATE POPUP EliCP

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Numero+'-'+Nummes+' Cta.Cte :'+CodCtc)
		SELE Parma
		SEEK 'TIPDOC'+'B/D'
		IF !FOUND()
			=STANDBY('Parametro de Boletas de Dep¢sito no icializado')
			USE IN AstPat
			USE IN MovBco
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cClave = vTip+PADL(MovBco.Numero,5,'0')+MovBco.NumMes+'  '
		
		IF !Hay_Doc(cClave,'B/D')		&& Se verifica si la B/D ya ha sido integrada
			m.TipDoc     = VTIP
			m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+fMes
			m.ComAdi     = ''
			m.Fecha      = MovBco.Fecha
			m.Glosa1     = ALLT(MovBco.Concept)+'  '+Movbco.NumRef
			m.Glosa2     = ALLT(MovBco.Nombre) +'  '+Movbco.transa
			m.Glosa3     = Movbco.Codctc+' Mes:'+Movbco.Nummes
			m.Glosa4     = ''
			m.Codctc     = Movbco.Codctc
			m.CodCaD     = Movbco.CodCad
			m.CODFTE     = Movbco.Codfte
			m.DocRef     = MovBco.Numero
			m.Nummesref  = Movbco.Nummes
			m.Periodoref = MovBco.Periodo
			m.Nummesfte  = Movbco.Nummes
			m.Periodofte = MovBco.Periodo
			m.Numdocfte  = Movbco.Numero
			m.TipDcr     = Val_para6(Movbco.Tipdoc,'TIPDOC')
			
			SELECT Comp
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			
			m.Item  = 0
			xTotDeb = 0
			xTotHab = 0
			
			SELECT AstPat
			SEEK Movbco.NumMes+Movbco.Numero		&&+Movbco.Codctc
			SCAN WHILE  MovBco.Nummes=Astpat.Nummes .AND. MovBco.Numero = Astpat.Numref
				m.Item       = m.Item + 1
				m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+fMes
				m.NumMov     = 1
				m.FecEmi     = MovBco.Fecha
				m.Tipo       = AstPat.TipCta
				m.NumChq     = ALLTRIM(MovBco.NumREF)
				m.Codctc     = Movbco.Codctc
				m.TipAux     = '01'
				m.Registro   = MovBco.Codban
				m.Codfte     = MovBco.Codfte
				m.Nummesref  = Movbco.Nummes
				m.Periodoref = MovBco.Periodo
				m.Coddep     = '113000'
				m.Docref     = Movbco.NumRI
				m.Nummesref  = Movbco.NummesRI
				m.PeriodoRef = Movbco.PerRi
				m.Tipdcr     = '25'
				m.Cuenta     = AstPat.CodCta
				m.Montos     = IIF(m.Tipo='D',AstPat.MToDeb,AstPat.MToHab)
				
				IF m.Tipo='H'
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','58','57')
				 ELSE
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','57','58')
				ENDIF
				
				xtotDeb = xtotDeb + IIF(m.Tipo='D',m.Montos,0)
				xtotHab = xtotHab + IIF(m.Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF f_appd()
					GATHER MEMVAR
					UNLOCK
				ENDIF
			ENDSCAN
			
			SELE Comp
			IF RLOCK()
				REPLACE Comp.totdeb     WITH xtotdeb
				REPLACE Comp.tothab     WITH xtothab
			ENDIF
		ENDIF
	ENDIF
ENDIF

USE IN AstPat
USE IN MovBco

RETURN


PROCEDURE NOTCAR  &&NOTA DE CARGO
*---------------
* Carga valores de NOTA DE CARGO
USE AstPat IN 0 ORDER TAG AstPat5 ALIAS AstPat
USE MovBco IN 0 ORDER TAG MovBco8 ALIAS MovBco
USE AstPre IN 0 ORDER TAG AstPre9 ALIAS AstPre

SELECT MovBco
SET RELATION TO NumMes+Numero INTO AstPat
GO TOP

IF EOF()
	DO STANDBY WITH 'No existe Documentos N/C'
	RETURN .F.
ENDIF

SELE MovBco
SET FILTER TO NumMes =ALLTRIM(FMES)
GO TOP
IF EOF()
	DO standby WITH 'No existen Notas de Cargo del Mes '+FMES
	USE IN AstPat
	USE IN MovBco
	USE IN AstPre
	RETURN .F.
ENDIF


DEFINE POPUP EliCP PROMPT FIELD ;
	Numero+' '+Nummes+' '+DTOC(Fecha)+'  '+CodCtc+'  '+TRANSF(Monto,'999,999,999.99')

ON SELECTION POPUP EliCP DEACTIVATE POPUP
ACTIVATE POPUP EliCP

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Numero+'-'+Nummes+' Cta.Cte :'+CodCtc)
	*   Esto debe Quedar cuando se estandarice la Informacion
	*	SELE Parma
	*	SEEK 'TIPDOC'+'N/C'
	*	IF !FOUND()
	*		=STANDBY('Parametro de Notas de Cargo no icializado')
	*		USE IN AstPat
	*		USE IN MovBco
	*		USE IN AstPre
	*		RETURN
	*	ENDIF
	*	vTip = ALLTRIM(Parma.CodigoAux)
		vTip = '50'
		
		cClave = vTip+PADL(MovBco.Numero,5,'0')+MovBco.NumMes+'  '
		
		IF !Hay_Doc(cClave,'N/C')		&& Se verifica si la N/C ya ha sido integrada
			m.TipDoc     = VTIP
			m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+fMes
			m.ComAdi     = ''
			m.Fecha      = MovBco.Fecha
			m.Glosa1     = ALLT(MovBco.Concept)+'  '+Movbco.NumRef
			m.Glosa2     = ALLT(MovBco.Nombre) +'  '+Movbco.transa
			m.Glosa3     = Movbco.Codctc+' Mes:'+Movbco.Nummes
			m.Glosa4     = ''
			m.Codctc     = Movbco.Codctc
			m.CodCaD     = Movbco.CodCad
			m.CODFTE     = Movbco.Codfte
			m.DocRef     = MovBco.Numero
			m.Nummesref  = Movbco.Nummes
			m.Periodoref = MovBco.Periodo
			m.Nummesfte  = Movbco.Nummes
			m.Periodofte = MovBco.Periodo
			m.Numdocfte  = Movbco.Numero
			m.TipDcr     = Val_para6(Movbco.Tipdoc,'TIPDOC')
			
			SELECT Comp
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			
			m.Item  = 0
			xTotDeb = 0
			xTotHab = 0
			
			SELECT AstPat
			SEEK Movbco.NumMes+Movbco.Numero				&&+Movbco.Codctc 
			SCAN WHILE  MovBco.Nummes=Astpat.Nummes .AND. MovBco.Numero = Astpat.Numref
				m.Item = m.Item + 1
				m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+fMes
				m.NumMov     = 1
				m.FecEmi     = MovBco.Fecha
				m.Tipo       = AstPat.TipCta
				m.NumChq     = ALLTRIM(MovBco.NumREF)
				m.Codctc     = Movbco.Codctc
				m.TipAux     = '01'
				m.Registro   = MovBco.Codban
				m.Codfte     = MovBco.Codfte
				m.Nummesref  = Movbco.Nummes
				m.Periodoref = MovBco.Periodo
				m.Coddep     = '113000'
				m.Docref     = ALLTRIM(Movbco.Numero)
				m.Nummesref  = Movbco.Nummes
				m.PeriodoRef = Movbco.Periodo
				m.Cuenta     = AstPat.CodCta
				m.Montos     = IIF(m.Tipo='D',AstPat.MToDeb,AstPat.MToHab)
				m.Codctc     = Movbco.Codctc
				
				IF Tipo='H'
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','58','57')
				 ELSE
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','57','58')
				ENDIF
				
				xtotDeb = xtotDeb + IIF(m.Tipo='D',m.Montos,0)
				xtotHab = xtotHab + IIF(m.Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF f_appd()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstPat
			ENDSCAN
			
			*-Cuentas Presupuestales de ejecuci¢n
			
			SELECT AstPre
			SEEK Movbco.NumMes+Movbco.Numero				&&+Movbco.Codctc
			SCAN WHILE   Movbco.NumMes=AstPre.NumMes .and. Movbco.Numero = AstPre.NumRef			&& .AND. Movbco.Codctc = AstPre.CodCtc
				m.Item     = m.Item + 1
				m.orden    = '32'
				m.Comadi   = ''
				m.NumMov   = 1
				m.Tipo     = AstPre.Tipo
				m.TipAux   = '01'
				m.Registro = MovBco.Codban
				m.CodCtc   = MovBco.CodCtc
				m.DocRef   = MovBco.NumRef
				m.Codpart  = Astpre.codpart
				m.Cuenta   = IIF(m.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
				m.Montos   = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
				
				xtotDeb = xtotDeb + IIF(m.Tipo='D',m.Montos,0)
				xtotHab = xtotHab + IIF(m.Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF f_appd()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstPre
			ENDSCAN
			
			SELE Comp
			IF RLOCK()
				REPLACE Comp.totdeb WITH xtotdeb
				REPLACE Comp.tothab WITH xtothab
				UNLOCK
			ENDIF
			
			SELE MovBco
			SET RELATION OFF INTO AstPat    
		ENDIF
	ENDIF
ENDIF
USE IN AstPat
USE IN MovBco
USE IN AstPre

RETURN


PROCEDURE NOTABO  &&NOTA DE ABONO
*---------------
*-Carga valores de COMPROBANTES DE PAGO
USE MovBco IN 0 ORDER TAG MovBco9  ALIAS MovBco
USE AstPre IN 0 ORDER TAG AstPre11 ALIAS AstPre
USE AstPat IN 0 ORDER TAG AstPat6  ALIAS AstPat

SELECT MovBco
SET RELATION TO NumMes+Numero INTO AstPat
GO TOP
IF EOF()
	DO STANDBY WITH 'No existe Documentos N/A'
	USE IN MovBco
	USE IN AstPre
	USE IN AstPat
	RETURN .F.
ENDIF

SELE MovBco
SET FILTER TO NumMes = ALLTRIM(FMES)
GO TOP
IF EOF()
	DO standby WITH 'No existen Notas de Abono del Mes '+FMES
	USE IN MovBco
	USE IN AstPre
	USE IN AstPat
	RETURN .F.
ENDIF

DEFINE POPUP EliCP PROMPT FIELD ;
	Numero+' '+Nummes+' '+DTOC(Fecha)+'  '+CodCtc+'  '+TRANSF(Monto,'999,999,999.99')

ON SELECTION POPUP EliCP DEACTIVATE POPUP
ACTIVATE POPUP EliCP

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Numero+'-'+Nummes+' Cta.Cte :'+CodCtc)
		SELE Parma
		SEEK 'TIPDOC'+'N/A'
		IF !FOUND()
			=STANDBY('Parametro de Notas de Abono no icializado')
			USE IN MovBco
			USE IN AstPre
			USE IN AstPat
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cClave = vTip+PADL(MovBco.Numero,5,'0')+MovBco.NumMes+'  '
		
		IF !Hay_Doc(cClave,'N/A')		&& Se verifica si el C/P ya ha sido integrada
			m.TipDoc     = VTIP
			m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+fmes
			m.ComAdi     = ''
			m.Fecha      = MovBco.Fecha
			m.Glosa1     = ALLT(MovBco.Concept)+'  '+Movbco.NumRef
			m.Glosa2     = ALLT(MovBco.Nombre) +'  '+Movbco.transa
			m.Glosa3     = Movbco.Codctc+' Mes:'+Movbco.Nummes
			m.Glosa4     = ''
			m.Codctc     = Movbco.Codctc
			m.CodCaD     = Movbco.CodCad
			m.CODFTE     = Movbco.Codfte
			m.DocRef     = MovBco.Numero
			m.Nummesref  = Movbco.Nummes
			m.Periodoref = MovBco.Periodo
			m.Nummesfte  = Movbco.Nummes
			m.Periodofte = MovBco.Periodo
			m.Numdocfte  = Movbco.Numero
			m.TipDcr     = Val_para6(Movbco.Tipdoc,'TIPDOC')
			
			SELECT Comp
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			
			m.Item  = 0
			xTotDeb = 0
			xTotHab = 0
			
			SELECT AstPat
			SEEK Movbco.NumMes+Movbco.Numero				&&+Movbco.Codctc
			SCAN WHILE  MovBco.Nummes=Astpat.Nummes .AND. MovBco.Numero = Astpat.Numref
				m.Item = m.Item + 1
				m.Comprob    = VTIP+PADL(MovBco.Numero,5,'0')+fMes
				m.NumMov     = 1
				m.FecEmi     = MovBco.Fecha
				m.Tipo       = AstPat.TipCta
				m.NumChq     = ALLTRIM(MovBco.NumREF)
				m.Codctc     = Movbco.Codctc
				m.TipAux     = '01'
				m.Registro   = MovBco.Codban
				m.Codfte     = MovBco.Codfte
				m.Nummesref  = Movbco.Nummes
				m.Periodoref = MovBco.Periodo
				m.Coddep     = '113000'
				m.Docref     = ALLTRIM(Movbco.Numero)
				m.Nummesref  = Movbco.Nummes
				m.PeriodoRef = Movbco.Periodo
				m.Cuenta     = AstPat.CodCta
				m.Montos     = IIF(m.Tipo='D',AstPat.MToDeb,AstPat.MToHab)
				m.Codctc     = Movbco.Codctc
				
				IF Tipo='H'
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','58','57')
				 ELSE
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','57','58')
				ENDIF
				
				xtotDeb = xtotDeb + IIF(m.Tipo='D',m.Montos,0)
				xtotHab = xtotHab + IIF(m.Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF f_appd()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstPat
			ENDSCAN
			
			*-Cuentas Presupuestales de ejecuci¢n
			
			SELECT AstPre
			SEEK Movbco.NumMes+Movbco.Numero				&&+Movbco.Codctc
			SCAN WHILE   Movbco.NumMes=AstPre.NumMes .and. Movbco.Numero = AstPre.NumRef			&& .AND. Movbco.Codctc = AstPre.CodCtc
				m.Item     = m.Item + 1
				m.orden    = '01'
				m.Comadi   = ''
				m.NumMov   = 1
				m.Tipo     = AstPre.Tipo
				m.TipAux   = '01'
				m.Registro = MovBco.Codban
				m.CodCtc   = MovBco.CodCtc
				m.DocRef   = MovBco.NumRef
				m.Codpart  = Astpre.codpart
				m.Cuenta   = IIF(m.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
				m.Montos   = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
				
				xtotDeb = xtotDeb + IIF(m.Tipo='D',m.Montos,0)
				xtotHab = xtotHab + IIF(m.Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF f_appd()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstPre
			ENDSCAN
				
			SELE Comp
			IF RLOCK()
				REPLACE Comp.totdeb WITH xtotdeb
				REPLACE Comp.tothab WITH xtothab
				UNLOCK
			ENDIF
			SELE MovBco
			SET RELATION OFF INTO AstPat    
		ENDIF
	ENDIF
ENDIF

USE IN MovBco
USE IN AstPre
USE IN AstPat

RETURN


PROCEDURE RECING
*---------------
*-Carga valores de RECIBOS DE INGRESO

SET MEMOWIDTH TO 50

USE AstOrd IN 0 ORDER TAG AstOrd1	ALIAS AstOrd
USE AstPat IN 0 ORDER TAG AstPat14 ALIAS AstPat
USE AstPre IN 0 ORDER TAG AstPre20 ALIAS AstPre
USE RecIng IN 0 ORDER TAG RecIng1  ALIAS RecIng

SELECT RecIng
GO TOP

IF EOF()
	DO Standby WITH 'No existe documentos Recibo de Ingreso'
	USE IN AstOrd
	USE IN AstPat
	USE IN AstPre
	USE IN RecIng
	RETURN .F.
ENDIF

SELE Recing
SET FILTER TO NumMes = ALLTRIM(FMES)
GO TOP
IF EOF()
	DO standby WITH 'No existen R/I del Mes '+FMES
	USE IN AstOrd
	USE IN AstPat
	USE IN AstPre
	USE IN RecIng
	RETURN .F.
ENDIF

DEFINE POPUP EliRI PROMPT FIELD ;
	NumRI+' '+'Periodo'+' '+NumMes+' '+DTOC(FecRI)+' '+CodCaD+' '+TRANSF(CanRI,'999,999,999.99') ;
	+' '+BolDep+' '+DTOC(FecDep)

ON SELECTION POPUP EliRI DEACTIVATE POPUP
ACTIVATE POPUP EliRI
RELEASE  POPUP EliRI

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+NumRI+' '+Nummes)
		SELE Parma
		SEEK 'TIPDOC'+'R/I'
		IF !FOUND()
			=STANDBY('Parametro de Hoja de Control no Inicializado')
			USE IN AstOrd
			USE IN AstPat
			USE IN AstPre
			USE IN RecIng
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cClave = vTip+PADL(RecIng.NumRI,5,'0')+RecIng.NumMes+'  '
		IF !Hay_Doc(cClave,'R/I')			&& Se verifica si a H/C ya ha sido integrada
			m.TipDoc     = vTip
			m.Comprob    = vTip+PADL(RecIng.NumRI,5,'0')+Fmes
			m.ComAdi     = ''
			m.Fecha      = RecIng.FecRI
			m.Glosa1     = Val_Para(Recing.TipRi,"TIPRI ","V") + 'Referencia : '+RecIng.DocRef
			m.Glosa2     = MLINE(RecIng.Observ,2)+MLINE(RecIng.Observ,3)
			m.Glosa3     = MLINE(RecIng.Observ,4)+MLINE(RecIng.Observ,5)+MLINE(RecIng.Observ,6)+MLINE(RecIng.Observ,7)
			m.Glosa4     = MLINE(RecIng.Observ,8)+MLINE(RecIng.Observ,9)+MLINE(RecIng.Observ,10)+MLINE(RecIng.Observ,11)
			m.CodCtc     = RecIng.CodCtc
			m.CodCaD     = RecIng.CodCaD
			m.CodFTE     = RecIng.CodFTE
			m.Nummesfte  = Recing.Nummes
			m.Numdocfte  = Recing.NumRI
			m.Docref     = ALLTRIM(Recing.Boldep)
			m.Periodofte = Recing.Periodo
			m.Numdocfte  = Recing.NumRI
			m.Docref     = RecIng.NumRI
			
			SELECT Comp
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			
			xtotdeb = 0
			xtothab = 0
			m.Item  = 0
			
			*-Cuentas Patrimoniales            
			
			SELECT AstPat
			SEEK RecIng.Periodo+fMes+RecIng.NumRI
			SCAN WHILE  Recing.Periodo=Astpat.Periodo .AND. RecIng.NumMes = AstPat.NumMes AND RecIng.NumRI = AstPat.NumRef
				m.Item	   = m.Item + 1
				m.NumMov   = 1
				m.FecEmi   = RecIng.FecRI
				m.Cuenta   = AstPat.CodCta
				m.Tipo     = AstPat.TipCta
				m.TipAux   = Recing.Tipaux
				m.Registro = ALLTRIM(Recing.codprv)
				m.CodCtc   = AstPat.CodCtc
				m.CodDep   = '113000'
				m.CodFte   = RecIng.Codfte
				m.Montos = IIF(m.Tipo='D',AstPat.MtoDeb,AstPat.MtoHaB)
				
				IF m.Tipo='D'
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','57','58')
				 ELSE
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','58','57')
				ENDIF
				
				xtotdeb = xtotdeb + IIF(m.Tipo='D',montos,0)
				xtothab = xtothab + IIF(m.Tipo='H',montos,0)
				
				SELECT Diar
				IF F_APPD()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstPat
			ENDSCAN   && AstPat
			
			*-Cuentas Presupuestales de ejecuci¢n
			
			SELECT AstPre
			SEEK RecIng.Periodo+fMes+RecIng.NumRI
			SCAN WHILE  RecIng.Periodo=AstPre.Periodo AND RecIng.NumRI = AstPre.NumRef .AND. fMes = AstPre.NumMes
				m.Item     = m.Item + 1
				m.Orden    = '30'
				m.NumMov   = 1
				m.Item     = m.Item
				m.FecEmi   = RecIng.FecRI
				m.Tipo     = AstPre.Tipo
				m.TipAux   = Recing.Tipaux
				m.Registro = ALLTRIM(Recing.codprv)
				m.CodCtc   = AstPre.CodCtc
				m.CodDep   = '113000'
				m.CodFte   = RecIng.Codfte
				m.Montos   = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
				m.Cuenta   = IIF(m.Tipo='D',Astpre.CtaDeb,Astpre.CtaHab)
				
				xtotdeb = xtotdeb + IIF(m.Tipo='D',montos,0)
				xtotHab = xtotHab + IIF(m.Tipo='H',montos,0)
				
				SELECT Diar
				IF F_APPD()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstPre
			ENDSCAN 			&&Astpre
			
			SELE AstOrd
			SEEK RecIng.Periodo+RecIng.NumMes+RecIng.NumRI+'R/I'
			
			SCAN WHILE Periodo=RecIng.Periodo AND NumMes=RecIng.NumMes AND NumRef=RecIng.NumRI AND TipDoc='R/I'
				m.Item  = m.Item + 1
				m.Orden = '31'
				m.NumMov  = 1
				m.FecEmi   = RecIng.FecRI
				m.Tipo     = AstOrd.TipCta
				m.TipAux   = Recing.Tipaux
				m.Registro = ALLTRIM(Recing.codprv)
				m.CodCtc   = ''
				m.CodDep   = '113000'
				m.CodFte   = RecIng.Codfte
				m.Cuenta   = AstOrd.CodCta
				m.Montos   = IIF(m.Tipo='D',AstOrd.MtoDeb,AstOrd.MtoHab)
				
				SELECT Diar
				IF f_appd()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				
				xTotDeb = xTotDeb+IIF(m.Tipo='D',m.Montos,0)
				xTotHab = xTotHab+IIF(m.Tipo='H',m.Montos,0)
				
				SELE AstOrd
			ENDSCAN
			
			SELE Comp
			SEEK m.Comprob+m.ComAdi
			IF f_Lock(1)
				REPLACE TotDeb WITH xTotDeb
				REPLACE TotHab WITH xTotHab
				UNLOCK
			ENDIF
			xtotdeb = 0
			xtothab = 0
			m.Item  = 0
		ENDIF
	ENDIF
ENDIF

SELE COMP
SET RELATION TO comprob+comadi INTO Diar

USE IN AstOrd
USE IN AstPat
USE IN AstPre
USE IN RecIng
RETURN

PROCEDURE CHEGIR
*---------------
*-Carga valores de CHEQUE GIRADOS

USE Cheque	IN 0 ORDER TAG Cheque3	ALIAS Cheque
*USE AstOrd  IN 0 ORDER TAG AstOrd1	ALIAS AstOrd
USE Compag  IN 0 ORDER TAG Compag1  ALIAS Compag

SELECT Cheque
SET RELATION TO NumMes+NumCP INTO Compag

GO TOP

IF EOF()
	DO Standby WITH 'No existe documentos Cheques Girados'
	USE IN Cheque
*	USE IN AstOrd
	USE IN ComPag
	RETURN .F.
ENDIF

SELE Cheque
SET FILTER TO NumMes = ALLTRIM(FMES) AND BETWEEN(ASC(LEFT(Cheque.Numchq,1)),48,57)

GO TOP
IF EOF()
	DO standby WITH 'No existen Cheques del Mes '+FMES
	USE IN Cheque
*	USE IN AstOrd
	USE IN ComPag
	RETURN .F.
ENDIF

DEFINE POPUP EliChq PROMPT FIELD ;
	NumCP+' '+ NumMes+' '+NumChq+' '+CodCtc+' '+TRANSF(ValChq,'999,999,999.99') ;
	+' '+DTOC(FecChq)+' '+NomGir

ON SELECTION POPUP EliChq DEACTIVATE POPUP
ACTIVATE POPUP EliChq
RELEASE  POPUP EliChq

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+NumCP+' '+Nummes)
		SELE Parma
		SEEK 'TIPDOC'+'CHG'
		IF !FOUND()
			=STANDBY('Parametro de Cheques Girados no Inicializado')
			USE IN Cheque
*			USE IN AstOrd
			USE IN ComPag
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cTip   = ALLTRIM(Parma.Codigo)
		cClave = vTip+PADL(Cheque.NumCP,5,'0')+Cheque.NumMes
		vNumCP = Cheque.NumCP
		m.ComAdi = '00'
		SELE Cheque
		IF !Hay_Doc(cClave,'CHG')			&& Se verifica si CHG ya ha sido integrada
			SCAN WHILE Cheque.NumCP=vNumCP
				xTotDeb = 0
				xTotHab = 0
				IF Cheque.NumCP#vNumCP
					m.ComAdi = '01'
				 ELSE
					m.ComAdi = PADL(ALLTRIM(STR(VAL(m.ComAdi)+1)),2,'0')
				ENDIF
				
				vNumCP = Cheque.NumCP
				
				m.Item = 0
				m.Periodo = Cheque.Periodo
				m.fecEmi  = Cheque.FecChq
				m.TipDoc  = vTip
				m.Comprob = ALLTRIM(vTip)+PADL(Cheque.NumCP,5,'0')+fMes
				m.Fecha   = Cheque.FecChq
				m.Glosa1  = 'Cheque Emitido # ' + Cheque.NumChq + ' con C/P N§ '+Cheque.NumCP +'-'+Cheque.periodo
				m.Glosa2  = 'Girado: '+DTOC(m.Fecha)
				m.Glosa3  = ''
				m.Glosa4  = ''
				m.CodCad  = Cheque.codcad
				m.TotDeb  = ROUND(Cheque.valChq,2)
				m.TotHab  = ROUND(Cheque.valChq,2)
				m.Periodofte = Cheque.Periodo
				m.Numdocfte  = Cheque.NumChq
				m.Docref  = Cheque.NumCP
				m.Tipdcr  = IIF(!EMPTY(Cheque.NumCP),'26','  ')
				m.Nummesref = Cheque.NumMes
				
				SELECT Comp
				IF f_appd()
					GATHER MEMVAR
				ENDIF

				FOR i = 1 TO 2
					m.Item  = m.Item + 1
					m.Orden = '01'
					m.NumMov  = 1
					m.FecEmi  = Cheque.FecCHQ
					m.TipAux    = IIF(!EMPTY(ComPag.CodPrv),'20',IIF(!EMPTY(ComPag.CodEmp),'03',IIF(!EMPTY(Compag.CodRet),'08',IIF(!EMPTY(Compag.CodOtr),'09','  '))))
					m.Registro  = IIF(!EMPTY(ComPag.CodPrv),Compag.Codprv,IIF(!EMPTY(ComPag.CodEmp),ComPag.CodEmp,IIF(!EMPTY(Compag.CodRet),ComPag.CodRet,IIF(!EMPTY(Compag.CodOtr),Compag.CodOtr,'  '))))
					m.Coddep  = '111000'
					m.Docref  = Cheque.NumCP
					m.Tipdcr  = IIF(!EMPTY(Cheque.NumCP),'26','  ')
					m.Nummesref = Cheque.Periodo
					m.Tipo    = IIF(i=1,'D','H')
					m.Cuenta  = IIF(i=1,'910308000000000','910408000000000')
					m.Montos  = Cheque.ValChq
					
					SELECT Diar
					
					IF f_appd()
						GATHER MEMVAR
						UNLOCK
					ENDIF
					
					xTotDeb = xTotDeb+IIF(m.Tipo='D',m.Montos,0)
					xTotHeb = xTotHab+IIF(m.Tipo='H',m.Montos,0)
				ENDFOR
				
				SELE Comp
				
				SEEK m.Comprob+m.ComAdi
				
				IF f_Lock(1)
					REPLACE TotDeb WITH xTotDeb
					REPLACE TotHab WITH xTotHab
					UNLOCK
				ENDIF
				SELE Cheque
			ENDSCAN
		ENDIF
	ENDIF
ENDIF

USE IN Cheque
*USE IN AstOrd
USE IN ComPag
RETURN

PROCEDURE CHEENT
*---------------
*-Carga valores de CHEQUE Entregados

USE Cheque	IN 0 ORDER TAG Cheque3	ALIAS Cheque
*USE AstOrd  IN 0 ORDER TAG AstOrd1	ALIAS AstOrd
USE Compag  IN 0 ORDER TAG Compag1  ALIAS Compag

SELECT Cheque
SET RELATION TO NumMes+NumCP INTO Compag

GO TOP

IF EOF()
	DO Standby WITH 'No existe documentos cheques Entregados'
	USE IN Cheque
*	USE IN AstOrd
	USE IN ComPag
	RETURN .F.
ENDIF

SELE Cheque
SET FILTER TO padl(alltrim(str(mont(fecent))),2,'0') = ALLTRIM(FMES) AND Estado='40' AND BETWEEN(ASC(LEFT(Cheque.Numchq,1)),48,57)
GO TOP
IF EOF()
	DO standby WITH 'No existen Cheques del Mes '+FMES
	USE IN Cheque
*	USE IN AstOrd
	USE IN ComPag
	RETURN .F.
ENDIF

DEFINE POPUP EliChq PROMPT FIELD ;
	NumCP+' '+ NumMes+' '+NumChq+' '+CodCtc+' '+TRANSF(ValChq,'999,999,999.99') ;
	+' '+DTOC(FecChq)+' '+NomGir

ON SELECTION POPUP EliChq DEACTIVATE POPUP
ACTIVATE POPUP EliChq
RELEASE  POPUP EliChq

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+NumCP+' '+Nummes)
		SELE Parma
		SEEK 'TIPDOC'+'CHE'
		IF !FOUND()
			=STANDBY('Parametro de Cheques Entregados no Inicializado')
			USE IN Cheque
*			USE IN AstOrd
			USE IN ComPag
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cTip   = ALLTRIM(Parma.Codigo)
		cClave = vTip+PADL(Cheque.NumCP,5,'0')+padl(mont(Cheque.FecEnt),2,'0')
*		cClave = vTip+PADL(Cheque.NumCP,5,'0')+Cheque.NumMes
		vNumCP = Cheque.NumCP
		m.ComAdi = '00'
		SELE Cheque
		IF !Hay_Doc(cClave,'CHE')			&& Se verifica si CHE ya ha sido integrada
			SCAN WHILE Cheque.NumCP=vNumCP
				xTotDeb = 0
				xTotHab = 0
				IF Cheque.NumCP#vNumCP
					m.ComAdi = '01'
				 ELSE
					m.ComAdi = PADL(ALLTRIM(STR(VAL(m.ComAdi)+1)),2,'0')
				ENDIF
				
				vNumCP = Cheque.NumCP
				
				m.Item = 0
				m.Periodo = Cheque.Periodo
				m.fecEmi  = Cheque.FecEnt
				m.TipDoc  = vTip
				m.Comprob = ALLTRIM(vTip)+PADL(Cheque.NumCP,5,'0')+fMes
				m.Fecha   = Cheque.FecEnt
				m.Glosa1  = 'Cheque Estregado # ' + Cheque.NumChq + ' con C/P N§ '+Cheque.NumCP +'-'+Cheque.periodo 
				m.Glosa2  = 'con Fecha '+ DTOC(Cheque.FecChq)
				m.Glosa3  = ''
				m.Glosa4  = ''
				m.CodCad  = Cheque.codcad
				m.TotDeb  = ROUND(Cheque.valChq,2)
				m.TotHab  = ROUND(Cheque.valChq,2)
				m.Periodofte = Cheque.Periodo
				m.Numdocfte  = Cheque.NumChq
				m.Docref  = Cheque.NumCP
				m.Tipdcr  = IIF(!EMPTY(Cheque.NumCP),'26','  ')
				m.Nummesref = Cheque.NumMes
				
				SELECT Comp
				IF f_appd()
					GATHER MEMVAR
				ENDIF
				
				FOR i = 1 TO 2
					m.Item  = m.Item + 1
					m.Orden = '01'
					m.NumMov  = 1
					m.FecEmi  = Cheque.FecEnt
					m.TipAux    = IIF(!EMPTY(ComPag.CodPrv),'20',IIF(!EMPTY(ComPag.CodEmp),'03',IIF(!EMPTY(Compag.CodRet),'08',IIF(!EMPTY(Compag.CodOtr),'09','  '))))
					m.Registro  = IIF(!EMPTY(ComPag.CodPrv),Compag.Codprv,IIF(!EMPTY(ComPag.CodEmp),ComPag.CodEmp,IIF(!EMPTY(Compag.CodRet),ComPag.CodRet,IIF(!EMPTY(Compag.CodOtr),Compag.CodOtr,'  '))))
					m.Coddep  = '111000'
					m.Docref  = Cheque.NumCP
					m.Tipdcr  = IIF(!EMPTY(Cheque.NumCP),'26','  ')
					m.Nummesref = Cheque.Periodo
					m.Tipo    = IIF(i=1,'D','H')
					m.Cuenta  = IIF(i=1,'910408000000000','910308000000000')
					m.Montos  = Cheque.ValChq
					
					SELECT Diar
					IF f_appd()
						GATHER MEMVAR
						UNLOCK
					ENDIF
					
					xTotDeb = xTotDeb+IIF(m.Tipo='D',m.Montos,0)
					xTotHab = xTotHab+IIF(m.Tipo='H',m.Montos,0)
				ENDFOR
				
				SELE Comp
				
				SEEK m.Comprob+m.ComAdi
				
				IF f_Lock(1)
					REPLACE TotDeb WITH xTotDeb
					REPLACE TotHab WITH xTotHab
					UNLOCK
				ENDIF
				SELE Cheque
			ENDSCAN
		ENDIF
	ENDIF
ENDIF

SELE COMP
SET RELATION TO comprob+comadi INTO Diar

USE IN Cheque
*USE IN AstOrd
USE IN ComPag

RETURN

PROCEDURE HOJMOD
*---------------
*- Carga valores de Hoja de Modificaci¢n 

USE HojMod IN 0 ORDER TAG HojMod1 ALIAS HojMod
USE IteHC  IN 0 ORDER TAG IteHC3  ALIAS IteHC   
USE IteCla IN 0 ORDER TAG IteCla1 ALIAS IteCla
USE AstPre IN 0 ORDER TAG AstPre5 ALIAS AstPre
USE OrdSer IN 0 ORDER TAG OrdSer1 ALIAS OrdSer		&& Todo lo Refer. a OrdSer debe Salir
USE Promae IN 0 ORDER TAG Promae1 Alias Promae

SELECT HojMod
SET FILTER TO HojMod.NumMes=fMes
SET RELATION TO NumMesHC+NumHC INTO IteHC

SELECT IteHC
SET RELATION TO codpart+CodCla                     INTO IteCla ADDITIVE
SET RELATION TO HojMod.NumMesHC+HojMod.NumHC+'H/M' INTO AstPre ADDITIVE

SELE HojMod
GO TOP

IF EOF()
	DO STANDBY WITH ' No existe documentos H/M '
	USE IN HojMod
	USE IN IteHC
	USE IN IteCla
	USE IN AstPre
	USE IN OrdSer
	RETURN .F.
ENDIF


DEFINE POPUP EliHM PROMPT FIELD ;
	NumHM+' '+NumMes+' '+Tiphm+' '+' '+NumMesHC+' '+Numhc+' '+DTOC(FecRef)+' '+CodPrv+' '+LEFT(Val_Fun('ProMae','NomPro','NomPro',CodPrv), 20)+;
	' ' + TRANSF(ImpTot,'99,999,999.99') + ' ' +CodPart+' '+CodCtc+' '+ ALLTRIM(TipFun)

ON SELECTION POPUP EliHM DEACTIVATE POPUP

ACTIVATE POPUP EliHM
RELEASE POPUP  EliHM

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Periodo+' '+Numhm+' '+ NummeS+' '+Codfte)
		SELE Parma
		SEEK 'TIPDOC'+'H/M'
		IF !FOUND()
			=STANDBY('Parametro de Hoja de Modificacion no Inicializado')
			USE IN HojMod
			USE IN IteHC
			USE IN IteCla
			USE IN AstPre
			USE IN OrdSer
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cClave = vTip+PADL(HojMod.NumHM,5,'0')+HojMod.NumMes+'  '
		IF !Hay_Doc(cClave,'H/M')			&& Se verifica si a H/M ya ha sido integrada
			m.fecha=HojMod.FecHm
			
			IF PADL(ALLTRIM(STR(MONTH(HojMod.FecHm),2)),2,'0')<>fMes
				* se asigna el primer dia del mes
				m.fecha=CTOD('01/'+fMes+'/'+VAnoHM)
			ENDIF
			
			m.TipDoc     = VTIP
			m.Comprob    = VTIP+PADL(HojMod.NumHm,5,'0')+fMes
			m.Fecha      = HojMod.FecHm
			m.Glosa1     = ALLTRIM(HojMod.DesRef)+' '+HojMod.Destino
			m.Glosa2     = MLINE(HojMod.OBSERVA,1)+MLINE(HojMod.OBSERVA,2)+MLINE(HojMod.OBSERVA,3)+MLINE(HojMod.OBSERVA,4)
			m.Glosa3     = MLINE(HojMod.OBSERVA,5)+MLINE(HojMod.OBSERVA,6)+MLINE(HojMod.OBSERVA,7)+MLINE(HojMod.OBSERVA,8)
			m.Glosa4     = ''
			m.CodCtc     = HojMod.CodCtC
			m.CodCad     = HojMod.CodCad
			m.codfte     = HojMod.codfte
			m.Periodofte = HojMod.Periodo
			m.Nummesfte  = HojMod.Nummes
			m.Numdocfte  = HojMod.NumHm
			m.Periodoref = HojMod.Perref
			m.DocRef     = HojMod.NumHC
			m.NummesRef  = SUBSTR(DTOC(HojMod.Fechc),4,2)
			m.TipDcr     = vtip
			
			SELECT Comp
			IF F_APPD()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			
			m.Item = 0
			xtotdeb= 0
			xtothab= 0
			
			*-Se generan los asientos Presupuestales por cada una de las partidas
			*-ya sea generica o anal¡tica
			
			SELECT AstPre
			SEEK HojMod.NumMes+HojMod.NumHM+'H/M'
			SCAN WHILE AstPre.NumMes = HojMod.NumMes AND AstPre.NumRef = HojMod.NumHM AND TIPDOC='H/M'
				m.Item = m.Item + 1
				m.orden      = '01'
				m.NumMov     = 1
				m.FecEmi     = m.Fecha
				m.TipAux     = IIF(!EMPTY(HojMod.CodPrv),'20',IIF(!EMPTY(HojMod.CodEmp),'03','09'))
				m.Registro   = IIF(!EMPTY(HojMod.CodPrv),HojMod.CodPrv,IIF(!EMPTY(HojMod.CodEmp),HojMod.CodEmp,HojMod.CodOtr))
				m.CodCtc     = HojMod.CodCtC
				m.Coddep     = '112000'
				m.Codpart    = IteHC.CodPart
				m.Periodoref = HojMod.Perref
				m.Tipo       = AstPre.Tipo
				m.CodCtc     = AstPre.CodCtC
				m.Cuenta     = IIF(m.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
				m.Montos     = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab) 
				
				xtotdeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
				xtothab = xTotHab + IIF(m.Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF F_APPD()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstPre
			ENDSCAN
			
			*- Genera asientos patrimoniales con las partidas del clasificador
			*- por objeto del gasto.
			SELE IteHC
			SEEK HojMod.NumMesHC+HojMod.NumHC
			IF FOUND()
				SCAN WHILE IteHC.NumHC = HojMod.NumHC .AND. IteHC.NumMes = HojMod.NumMesHC AND IteHC.Estado='92'
					mGenera=.T.
					IF mGenera
						SELECT IteHC
						IF !EMPTY(IteCla.CuentaD) AND !EMPTY(IteCla.CuentaH)
							
							FOR vCon=1 TO 2
		     					m.orden      = '02'
			     				m.NumMov     = 1
								m.Item       = m.Item+1
			     				m.DocRef     = HojMod.NumRef
	    		 				m.TipDcr     = vtip
		    		 			m.CodCtc     = HojMod.CodCtC
		     					m.Coddep     = '112000'
	    	 					m.Codpart    = Itehc.Codpart              
	     						m.Periodoref = HojMod.perref
			     				m.TipDoc     = itehc.TipDoc
			     				
								IF IteHC.TipOpe='-'
								 	DO CASE
										CASE vCon = 1
											m.Tipo   = 'H'
											m.Cuenta = PADR(ALLTRIM(IteCla.CuentaD),15,'0')
											m.Montos = IteHC.ValPart
										CASE vCon = 2
											m.Tipo   = 'D'
											m.Cuenta = PADR(ALLTRIM(IteCla.CuentaH),15,'0')
											m.Montos = IteHC.ValPart
									ENDCASE
								 ELSE
									DO CASE
										CASE vCon = 1
											m.Tipo   = 'D'
											m.Cuenta = PADR(ALLTRIM(IteCla.CuentaD),15,'0')
											m.Montos = IteHC.ValPart
										CASE vCon = 2 AND xNum=2
											m.Tipo   = 'H'
											m.Cuenta = PADR(ALLTRIM(IteCla.CuentaH),15,'0')
											m.Montos = IteHC.ValPart
									ENDCASE
								ENDIF
								
								
								xtotdeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
								xtothab = xTotHab + IIF(m.Tipo='H',m.Montos,0)
								SELE Diar
								IF F_APPD()
									GATHER MEMVAR
									UNLOCK
								ENDIF
								
							ENDFOR
						ENDIF
						SELE IteHC
					ENDIF
				ENDSCAN
			ENDIF
			
			Sele Comp
			IF F_Lock(1)
				REPLACE Comp.totdeb     WITH xtotdeb
				REPLACE Comp.tothab     WITH xtothab
				xtotdeb=0
				xtothab=0
				vitem=0
				UNLOCK
			ENDIF
   	    ENDIF
	ENDIF
ENDIF
DEACTIVATE POPUP RepT
RELEASE POPUP RepT
SET RELATION OFF INTO IteHC      
SELECT Comp
RETURN


PROCEDURE FACTURA
*----------------
USE RecIng IN 0 ORDER TAG RecIng1
USE AstPat IN 0 ORDER TAG AstPat36
USE AstPre IN 0 ORDER TAG AstPre5
USE AstOrd IN 0 ORDER TAG AstOrd1

SELE Recing
IF EOF()
	DO STANDBY WITH ' No existe documentos H/C '
	USE IN RecIng
	USE IN AstPat
	USE IN AstPre
	USE IN AstOrd
	RETURN .F.
ENDIF

SET FILTER TO NUMMES = ALLTRIM(Fmes)
GO TOP
IF EOF()
	DO standby WITH 'No existen Facturas del Mes '+FMES
	USE IN RecIng
	USE IN AstPat
	USE IN AstPre
	USE IN AstOrd
	RETURN .F.
ENDIF

DEFINE POPUP EliFac PROMPT FIELD ;
	RecIng.NumRi+RecIng.Periodo+' '+RecIng.NumMes+' '+' '+DTOC(RecIng.fecRi)+' '+TRANSF(RecIng.CanRi,'999,999,999.99')

ON SELECTION POPUP EliFac DEACTIVATE POPUP
ACTIVATE POPUP EliFac

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+NumRI+' '+Nummes)
		SELE Parma
		SEEK 'TIPDOC'+'FAC'
		IF !FOUND()
			=STANDBY('Parametro de Facturas no Inicializado')
			USE IN RecIng
			USE IN AstPat
			USE IN AstPre
			USE IN AstOrd
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cClave = vTip+PADL(RecIng.NumRI,5,'0')+RecIng.NumMes+'  '
		IF !Hay_Doc(cClave,'FAC')			&& Se verifica si a FAC ya ha sido integrada
			IF !EOF("AstPat") OR !EOF("AstPre") OR !EOF("AstOrd")
				*- Para el archivo compro.dbf
				m.tipdoc  = vTip
				m.comprob = vTip+PADL(RecIng.NumRi,5,"0")+fMes
				m.codfte    = RecIng.codfte
				m.comadi  = ''
				m.codCad  = RecIng.codCad
				m.codcal  = SUBS(DTOC(RecIng.fecRI),9,2) + fMes  + RecIng.codfte
				m.fecha   = RecIng.FecRI
				m.fecemi  = m.fecha
				m.glosa1  = "Fact. por ingreso # "+RecIng.NumRi
				m.glosa2  = MLINE(RecIng.Observ,1)
				m.glosa3  = MLINE(RecIng.Observ,2)
				m.glosa4  = MLINE(RecIng.Observ,3)
				m.estado  = ''
				m.tipdcr  = ''
				m.docref  = RecIng.numri
				m.codctc  = ''
				m.numchq  = ''
				m.fecope  = date()
				m.tipobr  = ''
				m.codobra = ''
*				m.usuario = login()
				m.fecope  = date()
				m.totdeb  = 0
				m.tothab  = 0
				
				SELECT comp
				IF f_appd()
					GATHER MEMVAR
				ENDIF
				
				@ 05,13 say 'Asiento de Fact. del R/I N§ '+RecIng.NumRi+' '+RecIng.codfte
				
				*-Para el diario
				
				m.Item = 0
				
				SELE AstPat
				SEEK RecIng.Periodo+RecIng.NumMes+RecIng.NumRI+'FAC'
				SCAN WHILE AstPat.NumRef=RecIng.NumRi AND TipDoc='FAC'
					m.item      = m.Item + 1
					m.orden     = IIF(RecIng.TipRI#'51','06','45')
					m.nummov    = 1
					m.tipdes    = '1'
					m.cuenta    = AstPat.CodCta
					m.tipo      = AstPat.TipCta
					m.montos    = IIF(m.Tipo='D',AstPat.MtoDeb,AstPat.MtoHab)
					m.tipaux    = RecIng.tipaux
					m.docref    = RecIng.NumRi
					m.codctc    = ''
					m.coddep    = '000001'
					m.tipdcr    = ''
					m.codfte    = RecIng.codfte
					m.fecemi    = m.fecha
					m.registro  = RecIng.codprv
					
					m.totdeb = m.totdeb+IIF(m.Tipo='D',m.Montos,0)
					m.totHab = m.totHab+IIF(m.Tipo='H',m.Montos,0)
					
					SELECT diar
					IF f_appd()
						GATHER MEMVAR
					ENDIF
					SELE AstPat
				ENDSCAN
				
				SELE AstPre
				SEEK RecIng.NumMes+RecIng.NumRI+'FAC'
				SCAN WHILE AstPre.NumRef=RecIng.NumRi AND TipDoc='FAC'
					m.item      = m.Item + 1
					m.nummov    = 1
					m.tipdes    = '1'
					m.orden     = '08'
					m.tipo      = AstPre.Tipo
					m.cuenta    = IIF(m.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
					m.montos    = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
					m.tipaux    = RecIng.tipaux
					m.docref    = RecIng.NumRi
					m.codctc    = ''
					
					m.totdeb = m.totdeb+IIF(m.Tipo='D',m.Montos,0)
					m.totHab = m.totHab+IIF(m.Tipo='H',m.Montos,0)
					
					SELECT diar
					IF f_appd()
						GATHER MEMVAR
					ENDIF
					SELE AstPre
				ENDSCAN
				
				SELE AstOrd
				SEEK RecIng.Periodo+RecIng.NumMes+RecIng.NumRI+'FAC'
				SCAN WHILE AstOrd.NumRef=RecIng.NumRi AND TipDoc='FAC'
					m.item      = m.Item + 1
					m.nummov    = 1
					m.tipdes    = '1'
					m.orden     = '07'
					m.tipo      = AstOrd.TipCta
					m.cuenta    = AstOrd.CodCta
					m.montos    = IIF(m.Tipo='D',AstOrd.MtoDeb,AstOrd.MtoHab)
					m.tipaux    = RecIng.tipaux
					m.docref    = RecIng.NumRi
					m.codctc    = ''
					
					m.totdeb = m.totdeb+IIF(m.Tipo='D',m.Montos,0)
					m.totHab = m.totHab+IIF(m.Tipo='H',m.Montos,0)
					
					SELECT diar
					IF f_appd()
						GATHER MEMVAR
					ENDIF
					SELE AstOrd
				ENDSCAN
				SELE Comp
				SEEK m.Comprob
				IF FOUND()
					REPLACE TotDeb WITH m.TotDeb
					REPLACE TotHab WITH m.TotHab
				ENDIF
			 ELSE
				* En el caso de No Exixtir Cuentas
				LOOP
			ENDIF
		ENDIF
	ENDIF
ENDIF

USE IN RecIng
USE IN AstPat
USE IN AstPre
USE IN AstOrd

RETURN

PROCEDURE NEA
*------------

DO Espera WITH 1,'Preparando Informacion...'

USE NotAlm IN 0 ORDER TAG NotAlm1 ALIAS NotAlm
USE AstPat IN 0 ORDER TAG AstPat36

SELE NotAlm

SET FILTER TO NUMMES = ALLTRIM(Fmes)
SET RELATION TO Periodo+fMes+NumAlm+'NEA' INTO AstPat ADDITIVE

IF EOF()
	DO STANDBY WITH ' No existe documentos PEC '
	USE IN NotAlm
	USE IN AstPat
	RETURN .F.
ENDIF

GO TOP
IF EOF()
	DO standby WITH 'No existen Pecosas del Mes '+FMES
	USE IN NotAlm
	USE IN AstPat
	RETURN .F.
ENDIF

DO Espera WITH 2

DEFINE POPUP EliPec PROMPT FIELD ;
	NotAlm.NumAlm+' '+NotAlm.Periodo+' '+fMes+' '+' '+DTOC(NotAlm.fecAlm)

ON SELECTION POPUP EliPec DEACTIVATE POPUP
ACTIVATE POPUP EliPec

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+NumAlm+' de fecha '+DTOC(FecAlm))
		SELE Parma
		SEEK 'TIPDOC'+'NEA'
		IF !FOUND()
			=STANDBY('Parametro de Neas no Inicializado')
			USE IN NotAlm
			USE IN AstPat
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cDia = PADL(DAY(NotAlm.fecAlm),2,'0')
		cClave = vTip+PADL(NotAlm.NumAlm,5,'0')+fMes				&&Pecosa1.NumPec+'  '
		SELE NotAlm
		IF !Hay_Doc(cClave,'NEA')			&& Se verifica si a NEA ya ha sido integrada
			IF !EOF("AstPat")
				*- Para el archivo compro.dbf
				m.tipdoc  = vTip
				m.comprob = vTip+PADL(NotAlm.NumAlm,5,"0")+fMes
				m.comadi  = ""
				m.codfte  = NotAlm.codfte
				m.codCad  = NotAlm.codCad
				m.codcal  = ""
				m.fecha   = NotAlm.FecAlm
				m.glosa1  = "Nota de Entrada a Almacen # "+NotAlm.NumAlm
				m.glosa2  = NotAlm.Destino
				m.glosa3  = NotAlm.Segun
				m.glosa4  = NotAlm.Observa
				m.estado  = ''
				m.tipdcr  = ''
				m.docref  = NotAlm.numAlm
				m.codctc  = ''
				m.numchq  = ''
				m.fecope  = date()
				m.tipobr  = ''
				m.codobra = ''
*				m.usuario = login()
				m.fecope  = date()
				m.totdeb  = 0
				m.tothab  = 0

				SELECT comp
				IF f_appd()
					GATHER MEMVAR
				ENDIF
				
				@ 05,13 say 'Asiento de Nota Ent. almacen N§ '+NotAlm.NumAlm+' '+NotAlm.codfte
				
				*-Para el diario
				
				m.Item = 0
				
				SELE AstPat
				SEEK NotAlm.Periodo+fMes+NotAlm.NumAlm+'NEA'
				SCAN WHILE AstPat.NumRef=NotAlm.NumAlm AND TipDoc='NEA' AND NumMes=fMes
					m.item      = m.Item + 1
					m.orden     = '01'
					m.nummov    = 1
					m.tipdes    = '1'
					m.cuenta    = AstPat.CodCta
					m.tipo      = AstPat.TipCta
					m.montos    = IIF(m.Tipo='D',AstPat.MtoDeb,AstPat.MtoHab)
					m.tipaux    = ''
					m.docref    = NotAlm.NumAlm
					m.codctc    = ''
					m.coddep    = '000001'
					m.tipdcr    = ''
					m.codfte    = NotAlm.codfte
					m.fecemi    = NotAlm.FecAlm
					m.registro  = ''
					
					m.totdeb = m.totdeb+IIF(m.Tipo='D',m.Montos,0)
					m.totHab = m.totHab+IIF(m.Tipo='H',m.Montos,0)
					SELECT diar
					IF f_appd()
						GATHER MEMVAR
					ENDIF
					
					SELE AstPat
				ENDSCAN
				
				SELE Comp
				SEEK m.Comprob+m.ComAdi
				IF FOUND()
					REPLACE TotDeb WITH m.TotDeb
					REPLACE TotHab WITH m.TotHab
				ENDIF
			 ELSE
				* En el caso de No Exixtir Cuentas
			*	LOOP
			ENDIF
		ENDIF
	ENDIF
ENDIF

USE IN NotAlm
USE IN AstPat

RETURN


PROCEDURE PECOSA
*----------------

DO Espera WITH 1,'Preparando Informacion...'

USE AstPat IN 0 ORDER TAG AstPat36

SELECT DISTINCT PECOSA.PERIODO, PECOSA.NUMPEC, PECOSA.CODCAD,;
  PECOSA.FECPEC, PECOSA.ESTADO, PECOSA.CODDEP, PECOSA.CODFTE, PECOSA.CODMAY,;
  PECOSA.FECEMI, PECOSA.FECVER, PECOSA.OBSERVA, PECOSA.ATTE, PECOSA.DESTINO,;
  PECOSA.TIPPEC, PECOSA.USER, PECOSA.USER_CR, PECOSA.USER_FC, PECOSA.USER_TP,;
  PECOSA.DESTINAT, PECOSA.CARDESTI, PECOSA.OBSALMA, ITEPEC.FECDESP;
 FROM PECOSA, ITEPEC;
 WHERE ITEPEC.PERIODO = PECOSA.PERIODO;
   AND ITEPEC.NUMPEC = PECOSA.NUMPEC;
   AND PADL(MONTH((ITEPEC.FECDESP)),2,"0") = fMes;
 ORDER BY PECOSA.PERIODO, PECOSA.NUMPEC;
 INTO CURSOR PeCoSa1

SELE Pecosa1

SET RELATION TO Periodo+fMes+NumPec+'PEC' INTO AstPat ADDITIVE

IF EOF()
	DO STANDBY WITH ' No existe documentos PEC '
	USE IN Pecosa
	USE IN Pecosa1
	USE IN ItePec
	USE IN AstPat
	RETURN .F.
ENDIF

GO TOP
IF EOF()
	DO standby WITH 'No existen Pecosas del Mes '+FMES
	USE IN Pecosa
	USE IN Pecosa1
	USE IN ItePec
	USE IN AstPat
	RETURN .F.
ENDIF

DO Espera WITH 2

DEFINE POPUP EliPec PROMPT FIELD ;
	Pecosa1.NumPec+' '+Pecosa1.Periodo+' '+fMes+' '+' '+DTOC(Pecosa1.fecPec)+' '+DTOC(Pecosa1.fecDesp)

ON SELECTION POPUP EliPec DEACTIVATE POPUP
ACTIVATE POPUP EliPec

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+NumPec+' con desp. '+DTOC(FecDesp))
		SELE Parma
		SEEK 'TIPDOC'+'PEC'
		IF !FOUND()
			=STANDBY('Parametro de Pecosas no Inicializado')
			USE IN Pecosa
			USE IN Pecosa1
			USE IN ItePec
			USE IN AstPat
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cDia = PADL(DAY(Pecosa1.fecDesp),2,'0')
		cClave = vTip+PADL(Pecosa1.NumPec,5,'0')+fMes+cDia				&&Pecosa1.NumPec+'  '
		SELE Pecosa1
		IF !Hay_Doc(cClave,'PEC')			&& Se verifica si a PEC ya ha sido integrada
			IF !EOF("AstPat")
				*- Para el archivo compro.dbf
				m.tipdoc  = vTip
				m.comprob = vTip+PADL(Pecosa1.NumPec,5,"0")+fMes
				m.comadi  = cDia
				m.codfte  = Pecosa1.codfte
				m.codCad  = Pecosa1.codCad
				m.codcal  = SUBS(DTOC(Pecosa1.fecPec),9,2) + fMes  + Pecosa1.codfte
				m.fecha   = Pecosa1.Fecdesp
				m.glosa1  = "Pecosa # "+Pecosa1.NumPec
				m.glosa2  = SUBSTR(ObsAlma,1,100)
				m.glosa3  = SUBSTR(ObsAlma,101,100)
				m.glosa4  = "Fecha de Despacho: "+DTOC(Pecosa1.FecDesp)
				m.estado  = ''
				m.tipdcr  = ''
				m.docref  = Pecosa1.numPec
				m.codctc  = ''
				m.numchq  = ''
				m.fecope  = date()
				m.tipobr  = ''
				m.codobra = ''
*				m.usuario = login()
				m.fecope  = date()
				m.totdeb  = 0
				m.tothab  = 0
				
				SELECT comp
				IF f_appd()
					GATHER MEMVAR
				ENDIF
				
				@ 05,13 say 'Asiento de PE. CO. SA. N§ '+Pecosa1.NumPec+' '+Pecosa1.codfte
				
				*-Para el diario
				
				m.Item = 0
				
				SELE AstPat
				SEEK Pecosa1.Periodo+fMes+Pecosa1.NumPec+'PEC'
				SCAN WHILE AstPat.NumRef=Pecosa1.NumPec AND TipDoc='PEC' AND NumMes=fMes
					IF Fecha=Pecosa1.FecDesp
						m.item      = m.Item + 1
						m.orden     = '01'
						m.nummov    = 1
						m.tipdes    = '1'
						m.cuenta    = AstPat.CodCta
						m.tipo      = AstPat.TipCta
						m.montos    = IIF(m.Tipo='D',AstPat.MtoDeb,AstPat.MtoHab)
						m.tipaux    = ''
						m.docref    = Pecosa1.NumPec
						m.codctc    = ''
						m.coddep    = '000001'
						m.tipdcr    = ''
						m.codfte    = Pecosa1.codfte
						m.fecemi    = Pecosa1.Fecdesp
						m.registro  = ''
						
						m.totdeb = m.totdeb+IIF(m.Tipo='D',m.Montos,0)
						m.totHab = m.totHab+IIF(m.Tipo='H',m.Montos,0)
						SELECT diar
						IF f_appd()
							GATHER MEMVAR
						ENDIF
					 ELSE
						LOOP
					ENDIF
					
					SELE AstPat
				ENDSCAN
				
				SELE Comp
				SEEK m.Comprob+m.ComAdi
				IF FOUND()
					REPLACE TotDeb WITH m.TotDeb
					REPLACE TotHab WITH m.TotHab
				ENDIF
			 ELSE
				* En el caso de No Exixtir Cuentas
			*	LOOP
			ENDIF
		ENDIF
	ENDIF
ENDIF

USE IN Pecosa
USE IN Pecosa1
USE IN ItePec
USE IN AstPat

RETURN

*PROCEDURE PECOSA
*----------------
USE Pecosa IN 0 ORDER TAG Pecosa1
USE ItePec IN 0 ORDER TAG ItePec1
USE AstPat IN 0 ORDER TAG AstPat36

SELE Pecosa
IF EOF()
	DO STANDBY WITH ' No existe documentos H/C '
	USE IN Pecosa
	USE IN ItePec
	USE IN AstPat
    RETURN .F.
ENDIF

SET RELATION TO Periodo+NumPec INTO ItePec
SET FILTER TO PADL(ALLTRIM(STR(MONT(ItePec.FecDesp))),2,'0')=ALLTRIM(Fmes)

GO TOP
IF EOF()
	DO standby WITH 'No existen Pecosas del Mes '+FMES
	USE IN Pecosa
	USE IN ItePec
	USE IN AstPat
	RETURN .F.
ENDIF

DEFINE POPUP EliPec PROMPT FIELD ;
	Pecosa.NumPec+' '+Pecosa.Periodo+' '+fMes+' '+' '+DTOC(Pecosa.fecPec)

ON SELECTION POPUP EliPec DEACTIVATE POPUP
ACTIVATE POPUP EliPec

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+NumPec+' '+Nummes)
		SELE Parma
		SEEK 'TIPDOC'+'PEC'
		IF !FOUND()
			=STANDBY('Parametro de Pecosas no Inicializado')
			USE IN Pecosa
			USE IN ItePec
			USE IN AstPat
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cClave = vTip+PADL(Pecosa.NumPec,5,'0')+Pecosa.NumPec+'  '
		IF !Hay_Doc(cClave,'PEC')			&& Se verifica si a PEC ya ha sido integrada
			IF !EOF("AstPat")
				*- Para el archivo compro.dbf
				m.tipdoc  = vTip
				m.comprob = vTip+PADL(Pecosa.NumPec,5,"0")+fMes
				m.comadi  = ''
				m.codfte  = Pecosa.codfte
				m.codCad  = Pecosa.codCad
				m.codcal  = SUBS(DTOC(Pecosa.fecPec),9,2) + fMes  + Pecosa.codfte
				m.fecha   = Pecosa.FecPec
				m.glosa1  = "Pecosa # "+Pecosa.NumPec
				m.glosa2  = SUBSTR(ObsAlma,1,100)
				m.glosa3  = SUBSTR(ObsAlma,101,100)
				m.glosa4  = ''
				m.estado  = ''
				m.tipdcr  = ''
				m.docref  = Pecosa.numPec
				m.codctc  = ''
				m.numchq  = ''
				m.fecope  = date()
				m.tipobr  = ''
				m.codobra = ''
*				m.usuario = login()
				m.fecope  = date()
				m.totdeb  = 0
				m.tothab  = 0
				
				SELECT comp
				IF f_appd()
					GATHER MEMVAR
				ENDIF
				
				@ 05,13 say 'Asiento de PE. CO. SA. N§ '+Pecosa.NumPec+' '+Pecosa.codfte
				
				*-Para el diario
				
				m.Item = 0
				
				SELE AstPat
				SEEK Pecosa.Periodo+fMes+Pecosa.NumPec+'PEC'
				SCAN WHILE AstPat.NumRef=Pecosa.NumPec AND TipDoc='PEC'
					m.item      = m.Item + 1
					m.orden     = '01'
					m.nummov    = 1
					m.tipdes    = '1'
					m.cuenta    = AstPat.CodCta
					m.tipo      = AstPat.TipCta
					m.montos    = IIF(m.Tipo='D',AstPat.MtoDeb,AstPat.MtoHab)
					m.tipaux    = ''
					m.docref    = Pecosa.NumPec
					m.codctc    = ''
					m.coddep    = '000001'
					m.tipdcr    = ''
					m.codfte    = Pecosa.codfte
					m.fecemi    = m.fecha
					m.registro  = ''
					
					m.totdeb = m.totdeb+IIF(m.Tipo='D',m.Montos,0)
					m.totHab = m.totHab+IIF(m.Tipo='H',m.Montos,0)
					
					SELECT diar
					IF f_appd()
						GATHER MEMVAR
					ENDIF
					SELE AstPat
				ENDSCAN
				
				SELE Comp
				SEEK m.Comprob
				IF FOUND()
					REPLACE TotDeb WITH m.TotDeb
					REPLACE TotHab WITH m.TotHab
				ENDIF
			 ELSE
				* En el caso de No Exixtir Cuentas
				LOOP
			ENDIF
		ENDIF
	ENDIF
ENDIF

USE IN Pecosa
USE IN ItePec
USE IN AstPat

RETURN



PROCEDURE HojAnuCh		&&PARTE DE ANULACION DE CHEQUE
*-----------------
*-Carga los partes de Anulacion de Cheques
USE Hojanu IN 0 ORDER TAG hojanu1  ALIAS HojAnu
USE AstPat IN 0 ORDER TAG AstPat18 ALIAS AstPat
USE AstPre IN 0 ORDER TAG AstPre19 ALIAS AstPre
USE AstOrd IN 0 ORDER TAG AstOrd1  ALIAS AstOrd

SELE HojAnu
IF EOF()
	DO Standby WITH 'No existe Documentos Parte de Anulaci¢n de Cheque'
	USE IN Hojanu
	USE IN AstPat
	USE IN AstPre
	USE IN AstOrd
	Return .F.
ENDIF


SET FILTER TO NUMMES = ALLTRIM(Fmes)
GO TOP
IF EOF()
	DO standby WITH 'No existen Parte de Anulaci¢n de Cheque del Mes '+FMES
	USE IN Hojanu
	USE IN AstPat
	USE IN AstPre
	USE IN AstOrd
	RETURN .F.
ENDIF

DEFINE POPUP EliHACh PROMPT FIELD ;
	NumHA+' '+NumMes+' '+NumCP+'.'+NummesCP+' '+DTOC(FecHan)+' '+CodFte+' '+CodCTC+' '+TRANSF(Import,'9,999,999.99')

ON SELECTION POPUP EliHACh DEACTIVATE POPUP
ACTIVATE POPUP EliHACh

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Periodo+' '+NumHA+' '+ NuMmeS+' '+CodFte+' '+CodCtc)
		SELE Parma
		SEEK 'TIPDOC'+'H/A'
		IF !FOUND()
			=STANDBY('Parametro de Facturas no Inicializado')
			USE IN Hojanu
			USE IN AstPat
			USE IN AstPre
			USE IN AstOrd
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cClave = vTip+PADL(HojAnu.NumHA,5,'0')+HojAnu.NumMes+'  '
		IF !Hay_Doc(cClave,'H/A')			&& Se verifica si a HAC ya ha sido integrada
			m.TipDoc     = vtip
			m.Comprob    = vtip+'0'+Hojanu.NumHa+Fmes
			m.Fecha      = Hojanu.FecHan
			m.Glosa1     = ALLTRIM(Hojanu.ccNombre)
			m.Glosa2     = 'C/P: '+Hojanu.NumCp+'.'+Hojanu.NummesCP+' H/C: '+Hojanu.NUmHC+'.'+Hojanu.NummesHC
			m.Glosa3     = ALLTRIM(Hojanu.Justif)
			m.Glosa4     = Hojanu.Destino
			m.CodCtc     = Hojanu.CodCtc
			m.CodCaD     = Hojanu.CodCaD
			m.CODFTE     = Hojanu.CODFTE
			m.tipobr     = Hojanu.Tipobr
			m.CodObra    = Hojanu.CodObra
			m.nummesfte  = Hojanu.Nummes
			m.periodofte = Hojanu.Periodo
			m.numdocfte  = Hojanu.NumHA
			m.Nummesref  = Hojanu.NummesCP
			m.Docref     = Hojanu.Numref
			m.Tipdcr     = Val_para6(Hojanu.Docref,'TIPDOC')
			
			SELECT comp
			IF f_appd()
				GATHER MEMVAR
			ENDIF
			xTotDeb = 0
			xTotHab = 0
			m.Item  = 0
			
  			*-Cuentas Patrimoniales
			SELECT AstPat
			SEEK fMes+HojAnu.NumHA+HojAnu.CodCtc
			SCAN WHILE  HojAnu.NumHA = AstPat.NumRef .AND. fMes = AstPat.NumMes .AND. HojAnu.CodCtc = AstPat.CodCtc
				m.Comprob   = vtip+'0'+Hojanu.Numha+Fmes
				m.NumMov    = 1
				m.Item      = m.Item +1
				m.FecEmi    = Hojanu.Fechan
				m.Cuenta    = AstPat.CodCta
				m.Tipo      = AstPat.TipCta
				m.TipAux    = IIF(!EMPTY(HojAnu.CodPrv),'20',IIF(!EMPTY(HojAnu.CodEmp),'30',IIF(!EMPTY(HojAnu.CodRet),'08',IIF(!EMPTY(HojAnu.CodOtr),'09','  '))))
				m.Registro  = IIF(!EMPTY(HojAnu.CodPrv),HojAnu.Codprv,IIF(!EMPTY(HojAnu.CodEmp),HojAnu.CodEmp,IIF(!EMPTY(HojAnu.CodRet),HojAnu.CodRet,IIF(!EMPTY(HojAnu.CodOtr),HojAnu.CodOtr,'  '))))
				m.CodCtc    = Hojanu.CodCtc
				m.CodFte    = Hojanu.CodFte
				m.Nummesref = Hojanu.NummesCP
				m.DocRef    = Hojanu.Numref
				m.CodDep    = '113000'
				m.Montos = IIF(m.Tipo='D',AstPat.MtoDeb,AstPat.MtoHab)
				
				IF m.Tipo='D'
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','57','58')
				 ELSE
					m.orden = IIF(left(cuenta,6)='110101' OR left(cuenta,6)='110102','58','57')
				ENDIF
				
				xtotdeb=xtotdeb+IIF(m.Tipo='D',m.Montos,0)
				xtotHab=xtotHab+IIF(m.Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF F_APPD()
					GATHER MEMVAR
				ENDIF
				SELE AstPat
			ENDSCAN
			
			*-Cuentas Presupuestales de ejecuci¢n
			
*			SELECT AstPre
*			SEEK fMes+HojAnu.NumHA+HojAnu.CodCtc
*			SCAN WHILE  HojAnu.Numha = AstPre.NumRef .AND. fMes = AstPre.NumMes  .AND.  HojAnu.CodCtc = AstPre.CodCtc
*				m.Item = m.Item + 1
*				m.Comprob   = vtip+'0'+hojanu.Numha+Fmes
*				m.ORDEN	   = '01'
*				m.NumMov     = 1
*				m.FecEmi     = Hojanu.Fechan
*				m.Tipo       = AstPre.Tipo
*				m.TipAux    = IIF(!EMPTY(HojAnu.CodPrv),'20',IIF(!EMPTY(HojAnu.CodEmp),'30',IIF(!EMPTY(HojAnu.CodRet),'08',IIF(!EMPTY(HojAnu.CodOtr),'09','  '))))
*				m.Registro  = IIF(!EMPTY(HojAnu.CodPrv),HojAnu.Codprv,IIF(!EMPTY(HojAnu.CodEmp),HojAnu.CodEmp,IIF(!EMPTY(HojAnu.CodRet),HojAnu.CodRet,IIF(!EMPTY(HojAnu.CodOtr),HojAnu.CodOtr,'  '))))
*				m.CodCtc     = Hojanu.CodCtc
*				m.CodFte     = Hojanu.CodFte
*				m.Nummesref  = Hojanu.NummesCP
*				m.TipDcr     = wtipdcr
*				m.DocRef     = Hojanu.Numref
*				m.Codpart    = Hojanu.Codpart
*				m.CodDep     = '112000'
*				m.Cuenta = IIF(m.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
*				m.Montos = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
				
*				xtotdeb=xtotdeb+IIF(m.Tipo='D',m.Montos,0)
*				xtothab=xtothab+IIF(m.Tipo='H',m.Montos,0)
				
*				SELECT Diar
*				IF F_APPD()
*					GATHER MEMVAR
*				ENDIF
*				SELE AstPre
*			ENDSCAN
			
			SELE AstOrd
			
			SEEK HojAnu.Periodo+HojAnu.NumMes+HojAnu.NumHA+'H/A'
			SCAN WHILE  HojAnu.Numha = AstOrd.NumRef .AND. fMes = AstOrd.NumMes AND AstOrd.TipDoc='H/A'
				m.Comprob  = vtip+'0'+hojanu.Numha+Fmes
				m.ORDEN	   = '01'
				m.NumMov   = 1
				m.Item     = m.Item+1
				m.FecEmi   = Hojanu.Fechan
				m.TipAux    = IIF(!EMPTY(HojAnu.CodPrv),'20',IIF(!EMPTY(HojAnu.CodEmp),'30',IIF(!EMPTY(HojAnu.CodRet),'08',IIF(!EMPTY(HojAnu.CodOtr),'09','  '))))
				m.Registro  = IIF(!EMPTY(HojAnu.CodPrv),HojAnu.Codprv,IIF(!EMPTY(HojAnu.CodEmp),HojAnu.CodEmp,IIF(!EMPTY(HojAnu.CodRet),HojAnu.CodRet,IIF(!EMPTY(HojAnu.CodOtr),HojAnu.CodOtr,'  '))))
				m.CodCtc   = Hojanu.CodCtc
				m.CodFte   = Hojanu.CodFte
				m.Nummesref= Hojanu.NummesCP
				m.DocRef   = Hojanu.Numref
				m.Codpart  = Hojanu.Codpart
				m.Tipo     = AstOrd.TipCta
				m.Cuenta  = AstOrd.CodCta
				m.MONTOS   = IIF(m.Tipo='D',AstOrd.MtoDeb,AstOrd.MtoHab)
				m.CodDep   = '112000'
				
				xtotdeb=xtotdeb+IIF(m.Tipo='D',m.Montos,0)
				xtothab=xtothab+IIF(m.Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF F_APPD()
					GATHER MEMVAR
				ENDIF
				SELE AstOrd
			ENDSCAN
			Sele Comp
			IF RLOCK()
				REPLACE Comp.totdeb     WITH xtotdeb
				REPLACE Comp.tothab     WITH xtothab
			ENDIF
		ENDIF
	ENDIF
ENDIF
USE IN Hojanu
USE IN AstPat
USE IN AstPre
USE IN AstOrd
   
RETURN
   

FUNCTION val_para6
*----------------
PARAMETERS mvalor, filtro
SELECT parma
SEEK filtro+mvalor
IF FOUND()
   RETURN  ALLTRIM(parma.codigoaux)
ELSE
   RETURN  '  '    
ENDIF
RETURN 

FUNCTION leedir
*----------------
PARAMETERS mcuenta 
* CASO Especial para el adicional de ctuentas corrientes
SELECT Auxi
SEEK '11'+ALLTRIM(mcuenta)
IF FOUND()
   RETURN  ALLTRIM(Auxi.direccion) 
ELSE
  * selec Valias
   RETURN  '  '    
ENDIF
RETURN 

FUNCTION busca_cta
*----------------
PARAMETERS vcuenta 
SELECT Cuen
SEEK &vcuenta 
IF FOUND()
   * SELE VAlias  
   RETURN  ALLTRIM(Cuen.descri) 
ELSE
   * SELE VAlias  
   RETURN  'Cuenta no Existe'    
ENDIF
RETURN 

FUNCTION SUMDEB
*----------------
PRIVATE VREC
wmonto=0
wspacio=SPACE(20)
Kcuenta=SUBSTR(diar.Cuenta,1,2)
SELE Diar
vrec=recno()
SUM Diar.Montos to wmonto FOR SUBSTR(diar.Cuenta,1,2)=Kcuenta and Diar.tipo='D' AND Diar.Comprob+Diar.ComAdi+Diar.Codfte = Comp.Comprob+Comp.ComAdi+Comp.CodFTE
GO vrec
RETURN wmonto
   

FUNCTION SUMHAB
*----------------
PRIVATE VREC
wmonto=0
wspacio=SPACE(20)
Kcuenta=SUBSTR(diar.Cuenta,1,2)
Sele Diar
vrec=recno()
SUM Diar.Montos TO wmonto FOR SUBSTR(diar.Cuenta,1,2)=Kcuenta and Diar.tipo='H' AND Diar.Comprob+Diar.ComAdi+Diar.Codfte = Comp.Comprob+Comp.ComAdi+Comp.CodFTE
GO vrec
RETURN wmonto

FUNCTION Val_Tipo
*----------------
  PARAMETERS mValor, Filtro, mVariable    
  PRIVATE mAlias, mLong
  mLong = 40               && Longitud campo DESCRI
  mAlias  = ALIAS()

  SELECT Parma
  SEEK Filtro+mValor

  IF !FOUND()   
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO Tipo = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
	  SELECT (mAlias)
       ENDIF
	   RETURN       
    ENDIF
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELDS;
	   Codigo+' '+CodigoAux+' '+SUBSTR(Descri,1,40)
	   ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    RELEASE POPUP parametro
    SET FILTER TO
  ENDIF
  mValor = Parma.Codigo
  mCuenta= Parma.DescriAu2
  mDescr = SUBSTR( Parma.Descri, 1, mLong )
  mDescriAux = SUBSTR( Parma.DescriAux, 1, mLong)
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
IF LASTKEY()#27
      REPLACE &mVariable WITH mValor
      RETURN .T.
ENDIF

FUNCTION Aut_Fin
*-----------------------------------------------------
* Funci¢n que Permite validar los tipos financieros en
* Forma autom tica
* _vtipdoc : Variable que recibe el tipo de documento
* _vCuenta : Variable que recibe el tipo de documento
*-----------------------------------------------------
PARAMETER  _vtipdoc,_vCuenta 
vAli=ALIAS()
SELE AutFin
SEEK _vtipdoc+_vCuenta 
IF FOUND()
   vTipfin  =Autfin.Tipfin+Autfin.Tipfin95
ENDIF
SELE (vAli)
RETURN vTipfin


FUNCTION Asi_Aut
*-----------------------------------------------------
* Funci¢n que Permite Asignar Asientos Autom ticos 
* cuando se hace el proceso de integraci¢n de Poliza 
* Forma autom tica
* _vtipdoc : Variable que recibe el tipo de documento
* _vCuenta : Variable que recibe el tipo de documento
*-----------------------------------------------------
PARAMETER  _vtipdoc,_vDestino,_vTipfun,_vCodart
vAli=ALIAS()
SELE AsiAut
* SEEK _vtipdoc+_vDestino+_vTipfun+_vCodart
* SEEK _vCodart+ALLTRIM(_vtipdoc)+ALLTRIM(_vTipfun)
SEEK _vCodart+ALLTRIM(_vtipdoc)+ALLTRIM(_vTipfun)+_vDestino
IF FOUND()
   vDHCuentas=AsiAut.Dcuenta+AsiAut.HCuenta
ELSE
   vDHCuentas=SPACE(20)
ENDIF
SELE (vAli)
RETURN (vDHCuentas)


PROCEDURE BASES
*/-------------
USE Compro    IN  1 ORDER TAG Compro7       ALIAS Comp
USE Diario    IN  2 ORDER TAG Diario11      ALIAS Diar
USE Cuentas   IN  3 ORDER TAG Cuentas1      ALIAS Cuen
USE CtoCto    IN  5 ORDER TAG CtoCto1       ALIAS CtoC
USE AutFin   IN  9 ORDER TAG AutFin1       ALIAS AutFin
USE Cajas     IN 10 ORDER TAG CAJAS1        ALIAS Caja
USE EstFin    IN 11 ORDER TAG EstFin1       ALIAS EstFin
*USE Auxil     IN 18 ORDER TAG Auxil1        ALIAS Auxi
USE (filaux)  IN  8 EXCLUSIVE                         ALIAS diax
USE Parmae    IN  4 ORDER TAG Parmae1                 ALIAS Parma
idxauxi = SYS(3) + '.IDX'
SELE EstFin
INDEX ON cuenta FOR !EMPTY(Tipo) TO (idxauxi) UNIQUE
SELECT COMP
RETURN

PROCEDURE OrdCom
*----------------
USE OrdCom IN 0 ORDER TAG OrdCom1 ALIAS Orden
USE AstOrd IN 0 ORDER TAG AstOrd1 ALIAS AstOrd
USE Promae IN 0 ORDER TAG Promae1 Alias Promae

SELECT Orden
GO TOP

IF EOF()
	DO STANDBY WITH ' No existe documentos O/C '
	USE IN Orden
	USE IN AstOrd
    RETURN .F.
ENDIF
   
SET FILTER TO NUMMES = ALLTRIM(Fmes) AND estado<>'99'
GO TOP
IF EOF()
	DO standby WITH 'No existen O/C del Mes '+FMES
	USE IN Orden
	USE IN AstOrd
	RETURN .F.
ENDIF

DEFINE POPUP EliOC PROMPT FIELD ;
	NumOC+' '+DTOC(FecOC)+' '+CodCaD+' '+DocRef+' '+TRANSF(ValTot,'999,999,999.99');
	+' '+LEFT(Val_Fun('ProMae', 'NomPro', 'NomPro', CodPrv), 20)

ON SELECTION POPUP EliOC DEACTIVATE POPUP

ACTIVATE POPUP EliOC
RELEASE  POPUP EliOC

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Periodo+' '+NumOC+' '+Codfte)
		SELE PARMA
		SET ORDER TO Parmae1
		SEEK 'TIPDOC'+'ORC'
		IF !FOUND()
			=STANDBY('Parametro de Ordenes de Compra no Inicializado')
			USE IN Orden
			USE IN AstOrd
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		cClave = vTip+PADL(Orden.NumOC,5,'0')+Orden.NumMes+'  '
		IF !Hay_Doc(cClave,'O/C')			&& Se verifica si a O/C ya ha sido integrada
			m.Item = 0
			m.Periodo = Orden.Periodo
			m.Codfte  = Orden.CodFte
			m.fecEmi  = Orden.Fecoc
			m.TipDoc  = vTip
			m.Comprob = ALLTRIM(vTip)+PADL(Orden.Numoc,5,'0')+fMes
			m.ComAdi  = ''
			m.Fecha   = Orden.FecOC
			m.Glosa1  = 'O/C Emitida N§ '+Orden.Periodo+'.'+Orden.Numoc+' H/C N§: '+Orden.numhc+'.'+Orden.perhc
			m.Glosa2  = 'Desde fecha '+DTOC(m.Fecha) +'  Fuente : '+m.codfte
			m.Glosa3  = Orden.docref+' Destino: '+ALLTRIM(Orden.Destino)
			m.Glosa4  = Orden.observa
			m.CodCad  = Orden.codcad
			m.TotDeb  = ROUND(Orden.valtot,2)
			m.TotHab  = ROUND(Orden.valtot,2)
			m.Periodofte = Orden.Periodo
			m.Numdocfte  = Orden.NUMoc
			m.Docref  = Orden.Numhc
			m.Tipdcr    = IIF(!EMPTY(Orden.NumHc),'03','  ')
			m.Nummesref = Orden.Perhc
			
			xTotDeb = 0
			xTotHab = 0
			
			SELECT Comp
			IF f_appd()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			SELE AstOrd
			SEEK Orden.Periodo+Orden.NumMes+Orden.NumOC+'O/C'
			IF FOUND()
				SCAN WHILE Periodo=Orden.Periodo AND NumMes=Orden.NumMes AND NumRef=Orden.NumOC AND TipDoc='O/C'
					m.Item  = m.Item + 1
					m.Orden = '01'
					m.NumMov  = 1
					m.FecEmi  = Orden.FecOC
					m.TipAux  = '20'
					m.Registro = Orden.codprv
					m.Coddep  = '111000'
					m.codfte  = Orden.CodFte
					m.Docref  = Orden.Numhc
					m.Tipdcr  = IIF(!EMPTY(Orden.NumHc),'03','  ')
					m.Nummesref = Orden.Perhc
					m.Tipo    = AstOrd.TipCta
					m.Cuenta  = AstOrd.CodCta
					m.Montos  = IIF(m.Tipo='D',AstOrd.MtoDeb,AstOrd.MtoHab)
					
					SELECT Diar
					IF f_appd()
						GATHER MEMVAR
						UNLOCK
					ENDIF
					
					xTotDeb = xTotDeb+IIF(m.Tipo='D',AstOrd.MtoDeb,0)
					xTotHab = xTotHab+IIF(m.Tipo='H',AstOrd.MtoHab,0)
					
					SELE AstOrd
				ENDSCAN
			ENDIF
			
			SELE Comp
			SEEK m.Comprob+m.ComAdi
				
			IF f_Lock(1)
				REPLACE TotDeb WITH xTotDeb
				REPLACE TotHab WITH xTotHab
				UNLOCK
			ENDIF
		ENDIF
	ENDIF
ENDIF

USE IN Orden
USE IN AstOrd

RETURN

PROCEDURE Rever
*--------------
USE Revsnu IN 0 ORDER TAG RevsNu1 ALIAS revs
USE Iteri  IN 0 ORDER TAG Iteri3  ALIAS Iteri
USE AstPre IN 0 ORDER TAG AstPre5 ALIAS AstPre
USE Promae IN 0 ORDER TAG Promae1 Alias Promae

SELECT Revs
GO TOP

IF EOF()
	DO STANDBY WITH ' No existe documentos Reversiones'
	USE IN Revs
	USE IN Iteri
	USE IN AstPre
	USE IN Promae
    RETURN .F.
ENDIF
   
SET FILTER TO NUMMES = ALLTRIM(Fmes) AND estado<>'99'
GO TOP
IF EOF()
	DO standby WITH 'No existen Reverssiones del Mes '+FMES
	USE IN Revs
	USE IN Iteri
	USE IN AstPre
	USE IN Promae
	RETURN .F.
ENDIF

DEFINE POPUP EliOC PROMPT FIELD ;
	NumRev+' '+DTOC(Fecha)+' '+CodCaD+' '+TRANSF(Monto,'999,999,999.99');
	+' '+LEFT(Val_Fun('ProMae', 'NomPro', 'NomPro', CodPrv), 20)

ON SELECTION POPUP EliOC DEACTIVATE POPUP

ACTIVATE POPUP EliOC
RELEASE  POPUP EliOC

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Periodo+' '+NumMes+' '+NumRev+' '+Codfte)
		SELE Parma
		SET ORDER TO Parmae1
		SEEK 'TIPDOC'+'REV'
		IF !FOUND()
			=STANDBY('Parametro de Reversiones no Inicializado')
			USE IN Revs
			USE IN Iteri
			USE IN AstPre
			USE IN Promae
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		vMesRev = Revs.NumMes
		cClave = vTip+PADL(RevS.NumRev,5,'0')+vMesRev+'  '
		IF !Hay_Doc(cClave,'REV')			&& Se verifica si REV ya ha sido integrada
			m.TipDoc     = VTIP
			m.Comprob    = VTIP+PADL(revs.NumRev,5,'0')+revs.NumMes
			m.Fecha      = revs.fecha
			m.Glosa1     = 'Realizado por : '+ALLTRIM(revs.nombre)
			m.Glosa2     = MLINE(revs.OBSERVA,1)+MLINE(revs.OBSERVA,2)
			m.Glosa3     = MLINE(revs.OBSERVA,3)+MLINE(revs.OBSERVA,4)+MLINE(revs.OBSERVA,5)+MLINE(revs.OBSERVA,6)
			m.Glosa4     = MLINE(revs.OBSERVA,7)+MLINE(revs.OBSERVA,8)+MLINE(revs.OBSERVA,9)+MLINE(revs.OBSERVA,10)
			m.CodCtc     = revs.CodCtC
			m.CodCad     = revs.CodCad
			m.codfte     = revs.codfte
			m.Periodofte = revs.Periodo
			m.Nummesfte  = revs.Nummes
			m.Numdocfte  = revs.NumREV
			m.TipDcr     = vtip
			
			SELECT Comp
			IF F_Appd()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			
			m.Item  = 0
			xtotdeb = 0
			xtothab = 0
			
			*-Se generan los asientos Presupuestales por cada una de las partidas
			*-las Patrimoniales las ingresa en el comprobante al momento de Jirar Cheque
			
			SELECT AstPre
			
			SEEK vMesRev+Revs.NumRev+'REV'
			
			SCAN WHILE AstPre.NumRef = Revs.NumRev .AND. AstPre.NumMes = Revs.NumMes
				m.Item     = m.Item + 1
				m.ORDEN    = '01'
				m.NumMov   = 1
				m.FecEmi   = m.fecha
				m.TipAux   = revs.TIPAUX
				m.CodCtc   = AstPre.CodCtC
				m.Registro = revs.CodPrv
				m.TipDcr   = wtipdcr
				m.CodFte   = REVS.codfte
				m.CodCtc   = revs.CodCtC
				m.Coddep   = '113000'
				m.Codpart  = iteri.Codpart
				m.Tipo     = AstPre.Tipo
				m.Cuenta   = IIF(m.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
				m.Montos   = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
				
				xtotdeb=xtotdeb+IIF(Tipo='D',m.Montos,0)
				xtotHab=xtotHab+IIF(Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF F_APPD()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstPre
			ENDSCAN
			
			Sele Comp
			IF RLOCK()
				REPLACE Comp.totdeb     WITH xtotdeb
				REPLACE Comp.tothab     WITH xtothab
				xtotdeb=0
				xtothab=0
				vitem=0
			ENDIF
		ENDIF
	ENDIF
ENDIF

USE IN Revs
USE IN Iteri
USE IN AstPre
USE IN Promae

RETURN

PROCEDURE ReverTs
*--------------
USE RevsnuTs IN 0 ORDER TAG RevsNuTS1 ALIAS revs
USE Iteri    IN 0 ORDER TAG Iteri3    ALIAS Iteri
USE AstPre   IN 0 ORDER TAG AstPre5   ALIAS AstPre
USE Promae   IN 0 ORDER TAG Promae1   Alias Promae

SELECT Revs
GO TOP

IF EOF()
	DO STANDBY WITH ' No existe documentos Reversiones'
	USE IN Revs
	USE IN Iteri
	USE IN AstPre
	USE IN Promae
    RETURN .F.
ENDIF
   
SET FILTER TO NUMMES = ALLTRIM(Fmes) AND estado<>'99'
GO TOP
IF EOF()
	DO standby WITH 'No existen Reverssiones del Mes '+FMES
	USE IN Revsnu
	USE IN Iteri
	USE IN AstPre
	USE IN Promae
	RETURN .F.
ENDIF

DEFINE POPUP EliOC PROMPT FIELD ;
	NumRev+' '+DTOC(Fecha)+' '+CodCaD+' '+TRANSF(Monto,'999,999,999.99');
	+' '+LEFT(Val_Fun('ProMae', 'NomPro', 'NomPro', CodPrv), 20)

ON SELECTION POPUP EliOC DEACTIVATE POPUP

ACTIVATE POPUP EliOC
RELEASE  POPUP EliOC

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Periodo+' '+NumMes+' '+NumRev+' '+Codfte)
		SELE Parma
		SET ORDER TO Parmae1
		SEEK 'TIPDOC'+'REB'
		IF !FOUND()
			=STANDBY('Parametro de Reversiones no Inicializado')
			USE IN Revs
			USE IN Iteri
			USE IN AstPre
			USE IN Promae
			RETURN
		ENDIF
		vTip = ALLTRIM(Parma.CodigoAux)
		vMesRev = Revs.NumMes
		cClave = vTip+PADL(RevS.NumRev,5,'0')+vMesRev+'  '
		IF !Hay_Doc(cClave,'REB')			&& Se verifica si REB ya ha sido integrada
			m.TipDoc     = VTIP
			m.Comprob    = VTIP+PADL(revs.NumRev,5,'0')+VMesRev
			m.Fecha      = revs.fecha
			m.Glosa1     = 'Realizado por : '+ALLTRIM(revs.nombre)
			m.Glosa2     = MLINE(revs.OBSERVA,1)+MLINE(revs.OBSERVA,2)
			m.Glosa3     = MLINE(revs.OBSERVA,3)+MLINE(revs.OBSERVA,4)+MLINE(revs.OBSERVA,5)+MLINE(revs.OBSERVA,6)
			m.Glosa4     = MLINE(revs.OBSERVA,7)+MLINE(revs.OBSERVA,8)+MLINE(revs.OBSERVA,9)+MLINE(revs.OBSERVA,10)
			m.CodCtc     = revs.CodCtC
			m.CodCad     = revs.CodCad
			m.codfte     = revs.codfte
			m.Periodofte = revs.Periodo
			m.Nummesfte  = revs.Nummes
			m.Numdocfte  = revs.NumREV
			m.TipDcr     = vtip
			
			SELECT Comp
			IF F_Appd()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			
			m.Item = 0
			xtotdeb= 0
			xtothab= 0
			
			*-Se generan los asientos Presupuestales por cada una de las partidas
			*-las Patrimoniales las ingresa en el comprobante al momento de Jirar Cheque
			
			SELECT AstPre
			
			SEEK vMesRev+Revs.NumRev+'REB'
			
			SCAN WHILE AstPre.NumRef = Revs.NumRev .AND. AstPre.NumMes = Revs.NumMes AND TipDoc="REB"
				m.Item     = m.Item + 1
				m.ORDEN    = '01'
				m.NumMov   = 1
				m.FecEmi   = m.fecha
				m.TipAux   = revs.TIPAUX
				m.CodCtc   = AstPre.CodCtC
				m.Registro = revs.CodPrv
				m.TipDcr   = wtipdcr
				m.CodFte   = REVS.codfte
				m.CodCtc   = revs.CodCtC
				m.Coddep   = '113000'
				m.Codpart  = iteri.Codpart
				m.Tipo     = AstPre.Tipo
				m.Cuenta   = IIF(m.Tipo='D',AstPre.CtaDeb,AstPre.CtaHab)
				m.Montos   = IIF(m.Tipo='D',AstPre.ValDeb,AstPre.ValHab)
				
				xtotdeb=xtotdeb+IIF(Tipo='D',m.Montos,0)
				xtotHab=xtotHab+IIF(Tipo='H',m.Montos,0)
				
				SELECT Diar
				IF F_APPD()
					GATHER MEMVAR
					UNLOCK
				ENDIF
				SELE AstPre
			ENDSCAN
			
			Sele Comp
			IF RLOCK()
				REPLACE Comp.totdeb     WITH xtotdeb
				REPLACE Comp.tothab     WITH xtothab
				xtotdeb=0
				xtothab=0
				vitem=0
			ENDIF
		ENDIF
	ENDIF
ENDIF

USE IN Revs
USE IN Iteri
USE IN AstPre
USE IN Promae

RETURN

PROCEDURE RevyAnu
*----------------
USE PteAnu	IN 0 ORDER TAG PteAnu3	ALIAS PteAnu
USE AstOrd  IN 0 ORDER TAG AstOrd1	ALIAS AstOrd
USE Promae  IN 0 ORDER TAG Promae1  Alias Promae

SELECT PteAnu
GO TOP

IF EOF()
	DO STANDBY WITH ' No existe documentos Reversiones'
	USE IN PteAnu
	USE IN AstOrd
	USE IN Promae
    RETURN .F.
ENDIF
   
vIdx = SYS(3)+".Idx"
INDEX ON Periodo+NumPA TO (vIdx) FOR MONTH(FecPA)=VAL(Fmes) and periodo=SUBSTR(FANO,3,2) AND estado<>'99'
SET FILTER TO PADL(MONT(FecPA),2,'0') = ALLTRIM(Fmes) AND estado<>'99'

SET RELATION TO Periodo+MesPA+NumPA+IIF(Tipo='A' AND TipDoc='O/C','AOC',IIF(Tipo='A' AND TipDoc='O/S',"AOS",IIF(Tipo='R' AND TipDoc='O/C',"ROC","ROS"))) INTO AstOrd ADDITIVE

GO TOP
IF EOF()
	DO standby WITH 'No existen Reverssiones del Mes '+FMES
	USE IN PteAnu
	USE IN AstOrd
	USE IN Promae
	ERASE (vIdx)
	RETURN .F.
ENDIF

DEFINE POPUP EliOC PROMPT FIELD ;
	Tipo+' '+MesPA+' '+NumPA+' '+TipDoc+' '+NumRef+' '+TRANSF(ValTot,'999,999.99');
	+' '+LEFT(Val_Fun('ProMae', 'NomPro', 'NomPro', CodPrv), 20)

ON SELECTION POPUP EliOC DEACTIVATE POPUP

ACTIVATE POPUP EliOC
RELEASE  POPUP EliOC

IF LASTKEY()#27
	IF yesno("Confirma Documento a Procesar "+Periodo+' '+MesPA+' '+NumPA+' '+Codfte)
		SELE Parma
		SET ORDER TO Parmae1
		SEEK 'TIPDOC'+'PRA'
		IF !FOUND()
			=STANDBY('Parametro de Reversiones no Inicializado')
			USE IN PteAnu
			USE IN AstOrd
			USE IN Promae
			ERASE (vIdx)
			RETURN
		ENDIF
		
		vTip = ALLTRIM(Parma.CodigoAux)
		
		SELE PteAnu
		
		vMesPA = PADL(MONT(FecPA),2,'0')
		
		DO CASE
			CASE Tipo='A' AND TipDoc='O/C'
				vTip1 = "AOC"
				vDoc  = 'Anulaci¢n de OC'
				vAdic = '01'
				vOrd  = '01'
			CASE Tipo='A' AND TipDoc='O/S'
				vTip1 = "AOS"
				vDoc  = "Anulaci¢n de OS"
				vAdic = '01'
				vOrd  = '02'
			CASE Tipo='R' AND TipDoc='O/C'
				vTip1 = "ROC"
				vDoc  = "Rebaja de OC"
				vAdic = '02'
				vOrd  = '03'
			CASE Tipo='R' AND TipDoc='O/S'
				vTip1 = "ROS"
				vDoc  = "Rebaja de OS"
				vAdic = '02'
				vOrd  = '04'
		ENDCASE
		
		cClave = vTip+PADL(PteAnu.NumPA,5,'0')+vMesPA+vAdic
		IF !Hay_Doc(cClave,'PRA')			&& Se verifica si REB ya ha sido integrada
			m.Item = 0
			m.Periodo = PteAnu.Periodo
			m.Codfte  = PteAnu.CodFte
			m.fecEmi  = PteAnu.FecPA
			m.TipDoc  = vTip
			m.Comprob = ALLTRIM(vTip)+PADL(PteAnu.NumPA,5,'0')+vMesPA
			m.ComAdi  = vAdic
			m.Fecha   = PteAnu.FecPA
			m.Glosa1  = vDoc +" de " + PteAnu.TipDoc + ": " + PteAnu.NumRef + " de Fecha " + DTOC(PteAnu.FecRef) + " y con fuente de financiamiento: " + PteAnu.CodFte
			m.Glosa2  = LEFT(PteAnu.Motivo,100)
			m.Glosa3  = SUBSTR(PteAnu.Motivo,101,100)
			m.Glosa4  = PteAnu.Observa
			m.CodCad  = PteAnu.codcad
			m.TotDeb  = ROUND(PteAnu.valtot,2)
			m.TotHab  = ROUND(PteAnu.valtot,2)
			m.Periodofte = PteAnu.Periodo
			m.Numdocfte  = PteAnu.NUMRef
			m.Docref  = PteAnu.TipDoc
			m.Nummesref = PADL(ALLTRIM(STR(MONT(PteAnu.FecRef))),2,"0")
			
			SELECT Comp
			IF F_Appd()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			
			m.Item = 0
			xtotdeb= 0
			xtothab= 0
			
			SELECT AstOrd
			
			SEEK m.Periodo + vMesPA + PteAnu.NumPA+vTip1
			
			IF FOUND()
				SCAN WHILE Periodo=PteAnu.Periodo AND NumMes=PteAnu.MesPA AND NumRef=PteAnu.NumPA AND TipDoc=vTip1
					m.Item  = m.Item + 1
					m.Orden = vOrd
					m.NumMov  = 1
					m.FecEmi  = PteAnu.FecPA
					m.TipAux  = '20'
					m.Registro = PteAnu.codprv
					m.Coddep  = '111000'
					m.codfte  = PteAnu.CodFte
					m.Docref  = PteAnu.NumRef
					m.Tipdcr  = "09"
					m.Nummesref = PADL(ALLTRIM(STR(MONT(PteAnu.FecRef))),2,"0")
					m.Tipo    = AstOrd.TipCta
					m.Cuenta  = AstOrd.CodCta
					m.Montos  = IIF(m.Tipo='D',AstOrd.MtoDeb,AstOrd.MtoHab)
					
					xtotdeb=xtotdeb+IIF(Tipo='D',m.Montos,0)
					xtotHab=xtotHab+IIF(Tipo='H',m.Montos,0)
					
					SELECT Diar
					
					IF F_APPD()
						GATHER MEMVAR
						UNLOCK
					ENDIF
					SELE AstOrd
				ENDSCAN
				
				Sele Comp
				IF RLOCK()
					REPLACE Comp.totdeb     WITH xtotdeb
					REPLACE Comp.tothab     WITH xtothab
					xtotdeb = 0
					xtothab = 0
					vitem   = 0
				ENDIF
			ENDIF
		ENDIF
	ENDIF
ENDIF
USE IN PteAnu
USE IN AstOrd
USE IN Promae
ERASE (vIdx)
RETURN

FUNCTION repetir
*---------------
PARAMETER vnumref,vcodpart
valias = ALIAS()
vorder = ORDER()
USE Itefp  IN 7 ORDER TAG Itefp2  ALIAS itefon
SELECT 	itefon
vTotigv= 0
SCAN FOR numfp=vnumref and codpart=vcodpart
 	vTotigv = vTotigv + Totigv
ENDSCAN
SELECT (valias)
SET ORDER TO (vorder)
RETURN vTotigv

*5505

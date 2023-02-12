**********************************
*  fOrdPed.Prg
*  Ordnes de Pedidos
*  Autor : Marco Rodriguez Abad
**********************************

CLOSE DATABASES
USE IN 1 FOrdPed ALIAS ordped   ORDER FOrdPed1
USE IN 2 FIteOP  ALIAS iteop    ORDER FIteOP1
USE IN 3 ArtMae  ALIAS artmae   ORDER ArtMae1
USE IN 4 IteArt  ALIAS iteart   ORDER IteArt2
USE IN 5 KardexV ALIAS kardex   ORDER Kardexv1
USE IN 6 StkAlmV ALIAS stkalmv  ORDER StkAlmv1
USE IN 7 Parmae  ALIAS parma    ORDER Parmae1
USE IN 8 Promae  ALIAS promae   ORDER ProMae1

vmens01 = 'Registro de Ordenes de Pedido de Famacia'
vmens02 = 'Ordenes de Pedido de Famacia : REVISION '
vmens04 = 'Dicha Orden no fue encontrada'
vmens05 = 'No existe Orden de Pedido anterior'
vmens06 = 'No existe Orden de Pedido siguiente'
vmens07 = '¨ Desea Anular esta Orden de Pedido ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Orden de Pedido ha sido anulada'
vmens10 = 'Edici¢n'

pigv = 1.18 
SELECT ordped
GOTO BOTTOM
SCATTER BLANK MEMVAR
DO inicia
HIDE POPUP ALL
DO pantalla
DO vista
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     ACTIVATE MENU mmenu
ENDDO
DO fin_opcion
RETURN
*
PROCEDURE inicia
*---------------
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anula  Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  TO 11, 79 TITLE vmens01  ;
       FOOTER '[F9] Detalle : Item' DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 12, 00  TO 23, 79 TITLE 'Detalle: Orden de Pedido de farmacia'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 00, 00  TO 23, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE MENU mmenu COLOR SCHEME 3

DEFINE PAD revis OF mmenu PROMPT '\<Revisa' AT 24, 00
DEFINE PAD busca OF mmenu PROMPT '\<Busca' AT 24, 08
DEFINE PAD anter OF mmenu PROMPT '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu PROMPT '\<Siguiente' AT 24, 25
DEFINE PAD corri OF mmenu PROMPT '\<Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT '\<Ingresa' AT 24, 45
DEFINE PAD anula OF mmenu PROMPT 'A\<nula  ' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT '\<Listar ' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO corri
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD anula OF mmenu DO anula
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
*-----------------
ACTIVATE WINDOW wind_0
CLEAR
@ 0, 02 SAY '              C/P:'
@ 0, 50 SAY '    Estado :'
@ 2, 2 SAY ' Orden de Pedido :'
@ 2, 50 SAY ' Fecha O/P :'
@ 3, 2 SAY '     Tipo Pedido :'
@ 4, 2 SAY '         Cliente :'
@ 5, 2 SAY '         Observa :'
@ 6, 2 SAY '     Valor Venta :'
@ 7, 2 SAY '             IGV :'
@ 8, 2 SAY '    Precio Venta :'
*@ 7, 37 SAY '       Redondeo :'
@ 8, 37 SAY 'Total Facturado :'
RETURN
*
PROCEDURE vista
*--------------
SELECT ordped
ON KEY LABEL F9 DO VISTA_DET
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 0, 12 SAY val_tipdoc() COLOR SCHEME 1
@ 0, 65 SAY val_est()    COLOR SCHEME 1
@ 2, 22 SAY m.periodo
@ 2, 25 SAY m.numped
@ 2, 65 SAY m.fecped
@ 3, 22 SAY val_para(m.tipped,'TIPPED','V',22,30)
@ 4, 22 SAY m.cliente
@ 5, 22 SAY LEFT(m.observa, 50)
@ 6, 22 SAY m.valped   PICTURE '99,999.99'
@ 7, 22 SAY m.igv      PICTURE '99,999.99'
@ 8, 22 SAY m.totped   PICTURE '99,999.99'
*@ 7, 54 SAY m.redondeo PICTURE '99,999.99'
@ 8, 54 SAY m.totfac   PICTURE '99,999.99'
DO vista_hijo
IF !vflag $ 'J*'
     DO subopc
ENDIF
RETURN
*
PROCEDURE vista_hijo
*-------------------
HIDE POPUP ALL
SELECT iteop
GOTO TOP
SEEK m.periodo + m.numped
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            item   :H = 'ITEM',  ;
            codart :H = 'C¢digo',  ;
            desart :H = 'Descripci¢n' : 40,  ;
            totcan :H = 'Cantidad' :P = '9,999',;
            total  :H = 'Total'    :P = '9,999.999',;
            cantidad :H = 'Unid.' :P =  '9,999',;
            fraccion :H = 'Frac.',;
            preven :H =   'P.U.',;
            prevenfr :H = 'P.U.Fr.';
            NOMENU NOAPPEND NODELETE NOCLEAR WINDOW wind_1  ;
            KEY m.periodo + m.numped TIMEOUT 0.0001  NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 4, 25 SAY 'No hay Detalles de este Documento'
     ACTIVATE WINDOW wind_0
ENDIF
SELECT ordped
RETURN
*
FUNCTION val_est
*---------------
PRIVATE mret
DO CASE
     CASE m.estado = '00'
          mret = 'Emitido  '
     CASE m.estado = '20'
          mret = 'Cancelado'
     CASE m.estado = '99'
          mret = 'Anulado  '
ENDCASE
RETURN mret
*
FUNCTION val_tipdoc
*------------------
PRIVATE mret
DO CASE
     CASE m.tipcp = 'B/V'
          mret = 'B/V N§ : ' + m.bolvta
     CASE m.tipcp = 'FAC'
          mret = 'FAC N§ : ' + m.factura
     OTHERWISE
          mret = SPACE(19)
ENDCASE
RETURN mret
*
PROCEDURE vista_det
*------------------
SELECT iteop
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
GOTO TOP
SEEK m.periodo + m.numped
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            item   :H = 'ITEM',  ;
            codart :H = 'C¢digo',  ;
            desart :H = 'Descripci¢n' : 40,  ;
            totcan :H = 'Cantidad' :P = '9,999', ;
            total  :H = 'Total'    :P = '9,999.999',;
            cantidad  :H = 'Unid.' :P = '9,999', ;
            fraccion  :H = 'Frac.',;
            preven :H = 'P.U.',;
            prevenfr :H = 'P.U. Fr.';
            NOMENU NOAPPEND NOEDIT NODELETE NOCLEAR  ;
            WINDOW wind_1 KEY m.periodo + m.numped  ;
            TITLE vtempo NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 4, 25 SAY 'No hay Detalles de este Ingreso'
ENDIF
ON KEY LABEL F9 DO VISTA_DET
SHOW MENU mmenu
SELECT ordped
DO vista
RETURN
*
PROCEDURE revis
*--------------
SELECT ordped
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SET RELATION TO periodo + numped INTO;
iteop
SET SKIP TO iteop
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE FIELDS periodo :H = 'Periodo', ;
			numped :H = 'O/P',;
			iteop.item :H = 'ITEM',;
			iteop.codart :H = 'C¢digo',;
			iteop.desart :H = 'Descripci¢n', ;
			iteop.cantidad :H = 'Unid.',;
			iteop.fraccion :H = 'Fracc.',;
			iteop.fraccion :H = 'Cantidad',  ;
			iteop.preven   :H = 'P.U.',  ;
			iteop.prevenfr :H = 'P.U. Fr.',;
			iteop.total :H = 'Total';
			NOMENU NOAPPEND NOEDIT NODELETE WINDOW wind_2
			
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELECT ordped
SET RELATION TO
DO vista
RETURN
*
PROCEDURE busca
*--------------
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = RIGHT(DTOC(m.fecsis), 2)
vnum_op = '    '
ACTIVATE WINDOW standby
@ 1, 01 SAY 'Ingrese N£mero O/P : ' GET vperiodo PICTURE '!!'
@ 1, 26 SAY '-' GET vnum_op PICTURE '!!!!' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_op) .OR. LASTKEY() = 27
     RETURN
ELSE
     SEEK vperiodo + vnum_op
     IF !FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
RETURN
*
FUNCTION vbusca
*--------------
vnum_op = PADL(ALLTRIM(vnum_op), 4, '0')
RETURN .T.
*
PROCEDURE anter
*--------------
SELECT ordped
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF !BOF()
     SKIP -1
ENDIF
IF BOF()
     GOTO TOP
     DO standby WITH vmens05
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE proxi
*--------------
SELECT ordped
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF !EOF()
     SKIP
ENDIF
IF EOF()
     DO standby WITH vmens06
     GOTO BOTTOM
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE corri
*--------------
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado = '99'
     DO standby WITH 'El Documento esta ya anulado'
     RETURN
ENDIF
IF estado = '20'
     DO standby WITH 'El Documento esta fue cancelado'
     RETURN
ENDIF
SELECT kardex
GOTO TOP
IF estado = '20'
     DO standby WITH 'El periodo ya esta cerrado no se puede hacer modificaciones'
     SELECT ordped
     DO vista
     RETURN
ENDIF
cmodo = 'C'
SELECT ordped
SCATTER MEMVAR
DO pantalla
@ 0, 65 GET m.estado WHEN .F.
@ 2, 22 GET m.periodo WHEN .F.
@ 2, 25 GET m.numped WHEN .F.
@ 2, 65 GET m.fecped WHEN .F.
@ 3, 22 GET m.tipped WHEN .F.
@ 4, 22 GET m.cliente WHEN .F.
@ 5, 22 GET m.observa FUNCTION 'S50' WHEN .F.
@ 6, 22 GET m.valped  PICTURE  '99,999.99' WHEN .F.
@ 7, 22 GET m.igv     PICTURE '99,999.99' WHEN .F.
@ 8, 22 GET m.totped  PICTURE '99,999.99' WHEN .F.
READ
IF LASTKEY() <> 27
     ok = trabaja_hi(cmodo)
     IF ok
          ok = grabapedid(cmodo)
          ok = cliente()
     ENDIF
     IF ok .AND. LASTKEY() <> 27
          SELECT ordped
          IF f_lock(1)
               m.tuser = 'C'
               m.usercr = vuser_id
               m.dusercr = DATE()
               m.husercr = TIME()
               GATHER MEMVAR
          ENDIF
     ENDIF
ELSE
     DO standby WITH 'Proceso cancelado'
ENDIF
UNLOCK ALL
FLUSH
SELECT ordped
DO vista
RETURN
*
PROCEDURE ingre
*--------------
SELECT stkalmv
GOTO TOP
IF EOF()
     DO standby WITH 'No existe Inventario Inicial en el sistema. por favor revise'
     SELECT ordped
     DO vista
     RETURN
ENDIF
SELECT kardex
GOTO TOP
IF estado = '20'
     DO standby WITH 'El periodo ya esta cerrado no se puede hacer modificaciones'
     SELECT ordped
     DO vista
     RETURN
ENDIF
ON KEY LABEL F9
SELECT iteop
vdbf = SYS(3) + '.Dbf'
vidx = SYS(3) + '.Idx'
COPY TO (vdbf) STRUCTURE
USE IN 0 (vdbf) ALIAS iteopx
SELECT iteopx
INDEX ON periodo + numped + item + codart TO (vidx)
SELECT ordped
nreg = IIF(!EOF(), RECNO(), - 1)
DO pantalla
SCATTER BLANK MEMVAR
m.estado = '00'
m.periodo = RIGHT(STR(YEAR(m.fecsis),4),2)
m.fecped = m.fecsis
cmodo = 'A'
SELECT ordped
@ 0, 65 GET m.estado WHEN .F.
@ 2, 22 GET m.periodo WHEN .F.
@ 2, 25 GET m.numped WHEN .F.
@ 2, 65 GET m.fecped WHEN .F.
@ 3, 22 GET m.tipped WHEN .F.
@ 4, 22 GET m.cliente WHEN .F.
@ 5, 22 GET m.observa FUNCTION 'S50' WHEN .F.
@ 6, 22 GET m.valped  PICTURE  '99,999.99' WHEN .F.
@ 7, 22 GET m.igv     PICTURE  '99,999.99' WHEN .F.
@ 8, 22 GET m.totped  PICTURE  '99,999.99' WHEN .F.
IF LASTKEY() <> 27
	ok = trabaja_hi(cmodo)
	IF ok
		ok = grabapedid(cmodo)
		ok = cliente()
	ENDIF
	IF ok .AND. LASTKEY() <> 27
		SELECT ordped
		IF f_appd()
			m.tuser = 'I'
			m.user = vuser_id
			m.duser = DATE()
			m.huser = TIME()
			GATHER MEMVAR
			SELECT parma
			SEEK 'CORRELFORDP '
			IF FOUND()
				IF f_lock(1)
					REPLACE nument  WITH nument + 1
				ENDIF
			ENDIF
		ENDIF
	ELSE
		IF nreg <> -1
			GOTO nreg
		ENDIF
		SELECT iteop
		IF SEEK(m.periodo + m.numped)
			DELETE WHILE periodo = m.periodo AND numped = m.numped
		ENDIF
		SELECT kardex
		SET ORDER TO KardexV2
		DO WHILE SEEK(m.periodo+'OPF'+m.numped)
			cperiodo = periodo
			ccodart = codart
			ccorrel = correl
			DELETE NEXT 1
			DO recalpr WITH cperiodo, ccodart, ccorrel
		ENDDO
		SET ORDER TO KardexV1
		DO standby WITH 'Proceso cancelado'
	ENDIF
ELSE
	IF nreg <> -1
		GOTO nreg
	ENDIF
	DO standby WITH 'Proceso cancelado'
ENDIF
UNLOCK ALL
FLUSH
USE IN iteopx
ERASE (vdbf)
ERASE (vidx)
SELECT ordped
DO vista
RETURN
*
FUNCTION grabapedid
*------------------
PARAMETER cmodo
IF cmodo = 'A'
	SELECT parma
	SEEK 'CORRELFORDP '
	IF FOUND()
		m.numped = PADL(ALLTRIM(STR(nument + 1)), 4, '0')
		IF SEEK(m.periodo + m.numped, 'OrdPed')
			DO standby WITH 'El Correlativo Generado ya Existe.'
			SELECT ordped
			DO vista
			RETURN
		ENDIF
	ELSE
		DO standby WITH 'El Parametro de Correlativos no existe. por favor consulte al area de Sistemas'
		DO vista
		RETURN
	ENDIF
	SELECT iteopx
	IF f_lock(1)
		REPLACE numped WITH m.numped FOR EMPTY(numped)
	ENDIF
	SELECT iteop
	APPEND FROM (vdbf)
ELSE
	SELECT iteop
ENDIF
SEEK m.periodo + m.numped
SCAN WHILE periodo + numped = m.periodo + m.numped
	SCATTER MEMVAR
	ak = actkarpr('OPF',m.numped,m.fecped)
ENDSCAN
IF LASTKEY() <> 27
	mret = .T.
ELSE
	mret = .F.
ENDIF
RETURN mret
*
FUNCTION cliente
*---------------
PRIVATE mret
DEFINE WINDOW cliente FROM 06, 00 TO 18, 79 TITLE 'CLIENTE'  ;
		FOOTER '' DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW cliente
@ 0, 0 SAY '  Tipo Venta : ' GET m.tipped VALID val_para(m.tipped,'TIPPED',' ',17,30)
@ 1, 0 SAY '      Cliente: ' GET m.cliente
@ 2, 0 SAY 'Observaciones: ' GET m.observa PICTURE '@S60'
@ 3, 0 SAY '  Valor Pedido : ' GET m.valped PICTURE '99,999.99' WHEN .F.
@ 4, 0 SAY '         I.G.V.: ' GET m.igv PICTURE '99,999.99'    WHEN .F.
@ 5, 0 SAY '   Total Pedido: ' GET m.totped PICTURE '99,999.99' WHEN .F.
*@ 6, 0 SAY '       Redondeo: ' GET m.redondeo PICTURE '99,999.99'
@ 6, 0 SAY 'Total Facturado: ' GET m.totfac PICTURE '99,999.99' WHEN vtotfac()
READ
DEACTIVATE WINDOW cliente
IF LASTKEY() <> 27
	mret = .T.
ELSE
	mret = .F.
ENDIF
RETURN mret
*
FUNCTION vtotfac
*---------------
m.totfac = m.totped		&& + m.redondeo
RETURN .F.
*
FUNCTION trabaja_hi
*------------------
PARAMETER cmodo
vsun = .T.
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°F2->Edita°°°°°°°°°F5->Agregar°°°°°°°°°F8->Eliminar°°°°°°°°F10->Terminar°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F2 DO Edita_item WITH cModo
ON KEY LABEL F5 DO agr_item   WITH cModo
ON KEY LABEL F6 DO agr_item1  WITH cModo
ON KEY LABEL F8 DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
IF cmodo == 'A'
	SELECT iteopx
ELSE
	SELECT iteop
	SET ORDER TO FiteOP1
ENDIF
SEEK m.periodo + m.numped
IF !FOUND()
	DO agr_item WITH cmodo
ENDIF
BROWSE NOOPTIMIZE NOMENU NOAPPEND NOEDIT NODELETE NOCLEAR;
	FIELDS ;
		item :H = 'Item',;
		codart :H = 'Codigo',;
		desart :H = 'Descripci¢n' : 40, ;
		totcan :H = 'Cantidad' :P = '9,999',;
		total  :H = 'Total' :P = '9,999.999',  ;
		cantidad :H = 'Unid.' :P = '9,999',;
		lfracc :H = 'Fracc.', ;
		unidad :H = 'Unidad', ;
		unifrac :H = 'U.fracc.', ;
		fraccion :H = 'Frac.',;
		preven :H = 'P.U.',;
		prevenfr :H = 'P.U.Frc.';
		WINDOW wind_1 KEY m.periodo + m.numped WHEN .F. NOREFRESH
m.totped = 0
SEEK m.periodo + m.numped
IF FOUND()
	SCAN WHILE periodo + numped = m.periodo + m.numped
		m.totped = m.totped + total
	ENDSCAN
	m.totped = ROUND(m.totped,2)
ENDIF
IF m.totped > 0
	m.igv = m.totped-ROUND(m.totped/pigv,2)
	m.valped = m.totped-m.igv
ENDIF
IF LASTKEY() <> 27
	vsun = .T.
ELSE
	vsun = .F.
ENDIF
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F6
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SELECT ordped
RETURN vsun
*
PROCEDURE agr_item
*-----------------
PARAMETER cmodo
PRIVATE m.tuser, m.user, m.huser, m.duser
PRIVATE kf2, kf5, kf6, kf8
lcant = .F.
lfrac = .F.
DEFINE WINDOW wind_3 FROM 02, 04 TO 20, 71 TITLE  ;
	'Pedido de Medicamentos' DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW wind_3
kf2 = ON('KEY','F2')
kf5 = ON('KEY','F5')
kf6 = ON('KEY','F6')
kf8 = ON('KEY','F8')
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F6
ON KEY LABEL F8
ON KEY LABEL F10
IF cmodo = 'A'
	SELECT iteopx
ELSE
	SELECT iteop
ENDIF
IF SEEK(m.periodo + m.numped)
	SCAN WHILE periodo + numped = m.periodo + m.numped
		m.item = item
	ENDSCAN
	m.item = PADL(ALLTRIM(STR(VAL(m.item) + 1)), 4, '0')
ELSE
	m.item = '0001'
ENDIF
m.codart = SPACE(12)
m.desart = SPACE(60)
m.lfracc = SPACE(1)
m.lmenart = SPACE(1)
m.unidad = SPACE(10)
m.unifrac = SPACE(10)
m.cantidad = 0
m.cantidadx = SPACE(10)
m.fraccion = 0
m.fracart = 0
m.totcan = 0
m.preven = 0
m.prevenfr = 0
m.preven2 = 0
m.preven2fr = 0
m.codpre = 1
xpreven = 0
xprevenfr = 0
xtotal = 0
m.preuni = 0
m.valuni = 0
m.valunifr = 0
m.total = 0
m.tuser = 'I'
m.user = vuser_id
m.huser = TIME()
m.duser = DATE()
m.lRecibo = ''
lRec = 2
lprecio1 = 0
lprecio2 = 0
@ 0, 1 SAY '             Item: ' + m.item
@ 1, 1 SAY '       Num. Orden: ' + m.periodo + '.' + m.numped
@ 2, 1 SAY '           Codigo:' GET m.codart VALID art_alm(m.codart) .AND. vcodlab() 
@ 3, 1 SAY '      Laboratorio: '
@ 4, 1 SAY '         Medicina:' GET m.desart FUNCTION 'S40' WHEN .F.
@ 5, 1 SAY '    Menudeo Art.?:' GET m.lmenart WHEN .F.
@ 6, 1 SAY '    Unidad (Lote):' GET m.unidad WHEN .F.
@ 7, 1 SAY '  Unidad Fraccion:' GET m.unifrac WHEN .F.
@ 8, 1 SAY '  Fracc. Articulo:' GET m.fracart WHEN .F.
@ 6, 35 SAY '           Pedido:'GET m.cantidadx		VALID Val_Can()
*@ 7, 35 SAY '     Recibo?:'GET lRec SIZE 1,5,1 FUNCTION '*RNH \<Si;\<No'	VALID Val_lRec()
*@ 8, 35 SAY '         Precio 1:'GET lprecio1 FUNCTION '*C' WHEN lRec=2 VALID vvalpre(1) AND val_can()
*@ 9, 35 SAY '         Precio 2:'GET lprecio2 FUNCTION '*C' WHEN wlprecio() AND lRec=1 VALID vvalpre(2) AND val_can()
@ 11, 35 SAY '         Cantidad:' GET m.cantidad VALID val_can() WHEN lcant
@ 12, 35 SAY '         Fraccion:' GET m.fraccion VALID val_can() WHEN lfrac
@ 13, 35 SAY '   Total Cantidad:' GET m.totcan WHEN .F.
@ 11, 1 SAY '    Precio Venta :'  GET m.preven PICTURE '999,999.99' DISABLE
@ 12, 1 SAY '  Prec. Vta. Frc.:'  GET m.prevenfr PICTURE '999,999.99' DISABLE
@ 13, 1 SAY '            Total:'  GET m.total PICTURE '999,999.99'  WHEN .F.
READ VALID val_read() .AND. valing()
IF LASTKEY() <> 27
	ak = .T.
	IF ak
		IF f_appd()
*       	m.preven = xpreven
*           m.prevenfr = xprevenfr
*           m.total = xtotal
			GATHER MEMVAR
		ENDIF
	ENDIF
ELSE
	DO standby WITH 'Cancela Informacion'
ENDIF
DEACTIVATE WINDOW wind_3
RELEASE WINDOW wind_3
ON KEY LABEL F2 &kf2
ON KEY LABEL F5 &kf5
ON KEY LABEL F6 &kf6
ON KEY LABEL F8 &kf8
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN

PROCEDURE agr_item1
*------------------
PARAMETER cmodo
PRIVATE m.tuser, m.user, m.huser, m.duser
PRIVATE kf2, kf5, kF6, kf8
lcant = .F.
lfrac = .F.
DEFINE WINDOW wind_3 FROM 02, 04 TO 20, 71 TITLE  ;
	'Pedido de Medicamentos - Precio Especial' DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW wind_3
kf2 = ON('KEY','F2')
kf5 = ON('KEY','F5')
kf6 = ON('KEY','F6')
kf8 = ON('KEY','F8')
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F6
ON KEY LABEL F8
ON KEY LABEL F10
IF cmodo = 'A'
	SELECT iteopx
ELSE
	SELECT iteop
ENDIF
IF SEEK(m.periodo + m.numped)
	SCAN WHILE periodo + numped = m.periodo + m.numped
		m.item = item
	ENDSCAN
	m.item = PADL(ALLTRIM(STR(VAL(m.item) + 1)), 4, '0')
ELSE
	m.item = '0001'
ENDIF
m.codart = SPACE(12)
m.desart = SPACE(60)
m.lfracc = SPACE(1)
m.lmenart = SPACE(1)
m.unidad = SPACE(10)
m.unifrac = SPACE(10)
m.cantidad = 0
m.cantidadx = SPACE(10)
m.fraccion = 0
m.fracart = 0
m.totcan = 0
m.preven = 0
m.prevenfr = 0
m.preven2 = 0
m.preven2fr = 0
m.codpre = 2
xpreven = 0
xprevenfr = 0
xtotal = 0
m.preuni = 0
m.valuni = 0
m.valunifr = 0
m.total = 0
m.tuser = 'I'
m.user = vuser_id
m.huser = TIME()
m.duser = DATE()
m.lRecibo = ''
lRec = 2
lprecio1 = 0
lprecio2 = 0
@ 0, 1 SAY '             Item: ' + m.item
@ 1, 1 SAY '       Num. Orden: ' + m.periodo + '.' + m.numped
@ 2, 1 SAY '           Codigo:' GET m.codart VALID art_alm(m.codart) .AND. vcodlab() 
@ 3, 1 SAY '      Laboratorio: '
@ 4, 1 SAY '         Medicina:' GET m.desart FUNCTION 'S40' WHEN .F.
@ 5, 1 SAY '    Menudeo Art.?:' GET m.lmenart WHEN .F.
@ 6, 1 SAY '    Unidad (Lote):' GET m.unidad WHEN .F.
@ 7, 1 SAY '  Unidad Fraccion:' GET m.unifrac WHEN .F.
@ 8, 1 SAY '  Fracc. Articulo:' GET m.fracart WHEN .F.
@ 6, 35 SAY '           Pedido:'GET m.cantidadx		VALID Val_Can()
*@ 7, 35 SAY '     Recibo?:'GET lRec SIZE 1,5,1 FUNCTION '*RNH \<Si;\<No'	VALID Val_lRec()
*@ 8, 35 SAY '         Precio 1:'GET lprecio1 FUNCTION '*C' WHEN lRec=2 VALID vvalpre(1) AND val_can()
*@ 9, 35 SAY '         Precio 2:'GET lprecio2 FUNCTION '*C' WHEN wlprecio() AND lRec=1 VALID vvalpre(2) AND val_can()
@ 11, 35 SAY '         Cantidad:' GET m.cantidad VALID val_can() WHEN lcant
@ 12, 35 SAY '         Fraccion:' GET m.fraccion VALID val_can() WHEN lfrac
@ 13, 35 SAY '   Total Cantidad:' GET m.totcan WHEN .F.
@ 11, 1 SAY '    Precio Venta :'  GET m.preven PICTURE '999,999.99' DISABLE
@ 12, 1 SAY '  Prec. Vta. Frc.:'  GET m.prevenfr PICTURE '999,999.99' DISABLE
@ 13, 1 SAY '            Total:'  GET m.total PICTURE '999,999.99'  WHEN .F.
READ VALID val_read() .AND. valing()
IF LASTKEY() <> 27
	ak = .T.
	IF ak
		IF f_appd()
*       	m.preven = xpreven
*           m.prevenfr = xprevenfr
*           m.total = xtotal
			GATHER MEMVAR
		ENDIF
	ENDIF
ELSE
	DO standby WITH 'Cancela Informacion'
ENDIF
DEACTIVATE WINDOW wind_3
RELEASE WINDOW wind_3
ON KEY LABEL F2 &kf2
ON KEY LABEL F5 &kf5
ON KEY LABEL F6 &kf6
ON KEY LABEL F8 &kf8
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN

FUNCTION Val_lRec
*----------------
m.lRecibo = IIF(lRec=1,'S','')
*lPrecio1 = 0
*lPrecio2 = 0
*SHOW GET lPrecio1
*SHOW GET lPrecio2
RETURN

*
FUNCTION wlprecio
*----------------
mret = .F.
mret = IIF(vuser_id='ADMIN',.T.,.F.)
RETURN mret
*
FUNCTION vcodlab
*---------------
xlab = 'B' + LEFT(m.codart, 6)
= val_fun('ArtMae',xlab,'Descri', xlab,1,3,20,'Descri')
RETURN .T.
*
FUNCTION valing
*--------------
PRIVATE mret
mret = .T.
IF LASTKEY() <> 27
     DO CASE
          CASE m.totcan = 0
               DO standby WITH 'La cantidad Ingrasada no debe Ser 0'
               mret = .F.
          CASE m.Total = 0
               DO standby WITH 'Monto no debe Ser 0'
               mret = .F.
          OTHERWISE
               mret = .T.
     ENDCASE
ENDIF
RETURN mret
*
PROCEDURE edita_item
*-------------------
PARAMETER cmodo
PRIVATE m.tuser, m.usercr, m.husercr, m.dusercr
PRIVATE kf2, kf5, kf6, kf8
lcant = .F.
lfrac = .F.
DEFINE WINDOW wind_3 FROM 02, 04 TO 20, 71 TITLE 'Edici¢n de Pedido Medicamentos'  ;
       DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW wind_3
kf2 = ON('KEY', 'F2')
kf5 = ON('KEY', 'F5')
kf6 = ON('KEY', 'F6')
kf8 = ON('KEY', 'F8')
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F6
ON KEY LABEL F8
ON KEY LABEL F10
IF cmodo = 'A'
     SELECT iteopx
ELSE
     SELECT iteop
ENDIF
SCATTER MEMVAR
m.lmenart = SPACE(1)
m.fracart = 0
xpreven = m.preven
xprevenfr = m.prevenfr
xtotal = m.total
lprecio1 = IIF(m.codpre = '1', 1, 0)
lprecio2 = IIF(m.codpre = '2', 1, 0)
m.tuser = 'C'
m.usercr = vuser_id
m.husercr = TIME()
m.dusercr = DATE()
= val_art(m.codart)
@ 0, 1 SAY '             Item: ' + m.item
@ 1, 1 SAY '       Num. Orden: ' + m.periodo + '.' + m.numped
@ 2, 1 SAY '           Codigo:'  GET m.codart WHEN vcodlab() AND .F.
@ 3, 1 SAY '      Laboratorio: '
@ 4, 1 SAY '         Medicina:'  GET m.desart FUNCTION 'S40' WHEN .F.
@ 5, 1 SAY '    Menudeo Art.?:'  GET m.lmenart WHEN .F.
@ 6, 1 SAY '    Unidad (Lote):'  GET m.unidad WHEN .F.
@ 7, 1 SAY '  Unidad Fraccion:'  GET m.unifrac WHEN .F.
@ 8, 1 SAY '  Fracc. Articulo:'  GET m.fracart WHEN .F.
@ 6, 35 SAY '           Pedido:' GET m.cantidadx	VALID val_can()
*@ 8, 35 SAY '         Precio 1:' GET lprecio1 FUNCTION '*C' VALID vvalpre(1) AND val_can()
*@ 9, 35 SAY '         Precio 2:' GET lprecio2 FUNCTION '*C' VALID vvalpre(2) AND val_can() WHEN wlprecio()
@ 11, 35 SAY '         Cantidad:' GET m.cantidad VALID val_can() WHEN lcant
@ 12, 35 SAY '         Fraccion:' GET m.fraccion VALID val_can() WHEN lfrac
@ 13, 35 SAY '   Total Cantidad:' GET m.totcan WHEN .F.
@ 11, 01 SAY '    Precio Venta :' GET m.preven PICTURE '999,999.99' DISABLE
@ 12, 01 SAY '  Prec. Vta. Frc.:' GET m.prevenfr PICTURE '999,999.99' DISABLE
@ 13, 01 SAY '            Total:' GET m.total PICTURE '999,999.99' WHEN .F.
READ VALID val_read()
IF LASTKEY() <> 27
     ak = .T.
     IF ak
          IF f_lock(1)
*               m.preven = xpreven
*               m.prevenfr = xprevenfr
*               m.total = xtotal
               GATHER MEMVAR
          ENDIF
     ENDIF
ELSE
     DO standby WITH 'Cancela Informacion'
ENDIF
DEACTIVATE WINDOW wind_3
RELEASE WINDOW wind_3
ON KEY LABEL F2 &kf2
ON KEY LABEL F5 &kf5
ON KEY LABEL F6 &kf6
ON KEY LABEL F8 &kf8
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN
*
PROCEDURE val_fracc
*------------------
PARAMETER cart
PRIVATE cali, cord
cali = ALIAS()
SELECT iteart
cord = ORDER()
SET ORDER TO IteArt1
IF SEEK('B' + cart)
     IF lfracc = 'S'
          IF m.lfracc = 'S'
               m.cantidad = 1
               lcant = .F.
               lfrac = .T.
          ELSE
               m.fraccion = fraccion
               lcant = .T.
               lfrac = .F.
          ENDIF
     ELSE
          IF m.lfracc = 'S'
               m.lfracc = 'N'
          ENDIF
          m.fraccion = fraccion
          lcant = .T.
          lfrac = .F.
     ENDIF
ELSE
     DO standby WITH 'Error en catalogo de Articulos'
ENDIF
SHOW GET m.cantidad
SHOW GET m.fraccion
SET ORDER TO (cord)
SELECT (cali)
RETURN
*
FUNCTION vvalpre
*---------------
PARAMETER xnum
PRIVATE calias
IF lprecio1 = 0 AND lprecio2 = 0
	m.codpre = ''
	xpreven = 0
	xprevenfr = 0
ELSE
	DO CASE
    	CASE xnum = 1
    		IF lprecio1 = 1
    			lprecio2 = 0
    			m.codpre = '1'
    			calias = ALIAS()
    			SELECT stkalmv
    			IF SEEK(m.periodo + m.codart)
    				xpreven = preven
    				xprevenfr = prevenfr
    			ENDIF
    			SELECT (calias)
    		ENDIF
		CASE xnum = 2
			IF lprecio2 = 1
				lprecio1 = 0
    			m.codpre = '2'
    			calias = ALIAS()
    			SELECT stkalmv
    			IF SEEK(m.periodo + m.codart)
    				xpreven = preven2
    				xprevenfr = preven2fr
    			ENDIF
    			SELECT (calias)
    		ENDIF
    ENDCASE
ENDIF

SHOW GET lprecio1
SHOW GET lprecio2
RETURN .T.
*
FUNCTION val_can
*---------------
PRIVATE mret
mret = .T.
IF m.lmenart = 'S'
	IF LEN(m.cantidadx) > 0
		IF VAL(m.cantidadx) = 0
			IF SUBSTR(m.cantidadx,1, 1) = 'F'
				m.lfracc = 'S'
			ELSE
				mret = .F.
			ENDIF
			IF TYPE(SUBSTR(m.cantidadx,2)) = 'N'
				m.fraccion = VAL(SUBSTR(m.cantidadx,2))
			ELSE
				mret = .F.
			ENDIF
			IF mret
				m.cantidad = INT(m.fraccion / m.fracart)
				m.totcan = m.fraccion
			ENDIF
		ELSE
			m.lfracc = 'N'
			m.cantidad = VAL(m.cantidadx)
			m.fraccion = m.fracart
			m.totcan = m.fraccion * m.cantidad
		ENDIF
	ELSE
		mret = .F.
	ENDIF
ELSE
	IF LEN(m.cantidadx) > 0
		IF VAL(m.cantidadx) = 0
			mret = .F.
		ENDIF
	ENDIF
	IF mret
		m.lfracc = 'N'
		m.cantidad = VAL(m.cantidadx)
		m.fraccion = m.fracart
		m.totcan = m.fraccion * m.cantidad
	ENDIF
ENDIF
IF mret
	SHOW GET m.cantidad
	SHOW GET m.fraccion
	SHOW GET m.totcan
ELSE
	DO standby WITH 'Error en cantidad ingresada'
ENDIF
m.total = IIF(m.lfracc='S',m.totcan*m.prevenfr,IIF(m.lfracc='N',m.cantidad*m.preven,0))
IF m.totcan > stkalmv.salfrac
	DO standby WITH 'La Cantidad Ingresada esta Excediendo al Stock. Revise'
ENDIF
SHOW GET m.totcan
SHOW GET m.preven
SHOW GET m.prevenfr
SHOW GET m.total
RETURN mret
*
PROCEDURE elimi_item
*-------------------
SELECT iteop
IF yesno('Estas seguro de Eliminar este Item')
     SELECT kardex
     SET ORDER TO KardexV2
     IF SEEK(iteop.periodo+'OPF'+iteop.numped+iteop.item+iteop.codart)
          IF RLOCK()
               cperiodo = periodo
               ccodart = codart
               ccorrel = correl
               DELETE NEXT 1
               DO recalpr WITH cperiodo,ccodart,ccorrel
          ENDIF
     ENDIF
     SELECT iteop
     IF RLOCK()
          DELETE NEXT 1
     ELSE
          DO standby WITH 'No puede eliminar este Item.'
     ENDIF
ENDIF
UNLOCK
RETURN
*
PROCEDURE lista
*--------------
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
nreg = RECNO()
DEFINE WINDOW wlista FROM 3, 15 TO 20, 70 FLOAT TITLE  ;
       'Listado de Pedidos' DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
vtope = 1
lelec = 1
ltped = 1
mfecha1 = CTOD('01/01/'+STR(YEAR(m.fecsis),4))
mfecha2 = DATE()
@ 01, 01 SAY '  Todos los Pedidos : ' GET vtope SIZE 1,10,2 FUNCTION '*RNH \<Si;\<No'
@ 03, 01 SAY '              Pedido: ' GET m.periodo WHEN vtope = 2
@ 03, 27 GET m.numped WHEN vtope = 2
@ 05, 20 GET lelec FUNCTION '^ Resumen;Detallado' WHEN vtope = 1
@ 08, 01 SAY '       Tipo Pedido :   ' GET ltped FUNCTION '^ Todos;Ventas Contado;Ventas Credito;Otros' WHEN vtope = 1
@ 12, 01 SAY '      Rango de Fechas: ' GET mfecha1 WHEN vtope = 1
@ 12, 36 GET mfecha2 WHEN vtope = 1
@ 15, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
IF LASTKEY() <> 27 .AND. okcancel <> 2
     IF vtope = 2
          IF SEEK(m.periodo + m.numped)
               SCATTER MEMVAR
               vdbf = SYS(3) + '.Dbf'
               COPY TO (vdbf) STRUCTURE
               USE IN 0 (vdbf) ALIAS xordped
               SELECT xordped
               APPEND BLANK
               GATHER MEMVAR
               SET RELATION TO periodo + numped INTO iteop ADDITIVE
               SET SKIP TO iteop
               DO reporte WITH 2,'OrdPed',' Orden de Pedido.',2,.F.,.T.
               USE IN xordped
               ERASE (vdbf)
               SELECT ordped
          ENDIF
     ELSE
          SELECT ordped
          DO CASE
               CASE ltped = 1
                    lfil = .T.
               CASE ltped = 2
                    lfil = "m.TipPed = '01'"
               CASE ltped = 3
                    lfil = "m.TipPed = '02'"
               CASE ltped = 4
                    lfil = "m.TipPed = '03'"
          ENDCASE
          SET FILTER TO &lFil AND BETW(FecPed,mFecha1,mFecha2)
          IF lelec = 1
               DO reporte WITH 2,'Pedidos','Reporte de Pedidos Resumen',2, .F., .T.
          ELSE
               SET RELATION TO periodo + numped INTO iteop
               SET SKIP TO iteop
               DO reporte WITH 2,'Pedidos2','Reporte de Pedidos Detallado',2,.F.,.T.
               SET RELATION TO
          ENDIF
          SET FILTER TO
     ENDIF
ENDIF
SELECT ordped
GOTO nreg
DO vista
RETURN
*
PROCEDURE anula
*--------------
PRIVATE nreg, lanula
lanula = .F.
IF yesno('Esta seguro de Anular este Documento')
     lanula = .T.
ENDIF
IF lanula
     DO CASE
          CASE estado = '99'
               DO standby WITH 'El Docuemnto ya esta Anulado'
               lanula = .F.
          CASE estado = '20'
               DO standby WITH 'EL Documento ya esta cancelado no se puede anular.'
               lanula = .F.
          OTHERWISE
               lanula = .T.
     ENDCASE
ENDIF
IF lanula
     SELECT iteop
     IF SEEK(m.periodo+m.numped)
          SCAN WHILE periodo+numped = m.periodo+m.numped
          	IF f_lock(1)
               REPLACE estado WITH '99'
          	ENDIF
          ENDSCAN
     ENDIF
     SELECT kardex
     SET ORDER TO KardexV2
     IF SEEK(m.periodo + 'OPF' + m.numped)
          SCAN WHILE periodo+tipdoc+numdoc = m.periodo+'OPF'+m.numped
          	IF f_lock(1)
               REPLACE estado WITH '99'
               nreg = RECNO()
               cperiodo = periodo
               ccodart = codart
               ccorrel = correl
               DO recalpr WITH cperiodo,ccodart,ccorrel
               GOTO nreg
          	ENDIF
          ENDSCAN
     ENDIF
     SET ORDER TO KardexV1
     SELECT ordped
     IF SEEK(m.periodo+m.numped)
     	IF f_lock(1)
          REPLACE estado WITH '99'
     	ENDIF
     ENDIF
ENDIF
SELECT ordped
DO vista
RETURN
*
PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
*-------------------
CLOSE DATABASES
ON KEY LABEL F7
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
PROCEDURE subopc
*---------------
PRIVATE calias
calias = ALIAS()
USE IN 0 IteUsuOp ORDER IteUsuOp2
SET SKIP OF PAD revis OF mmenu NOT SEEK(vusucla+'0027','IteUsuOp')
SET SKIP OF PAD busca OF mmenu NOT SEEK(vusucla+'0028','IteUsuOp')
SET SKIP OF PAD anter OF mmenu NOT SEEK(vusucla+'0029','IteUsuOp')
SET SKIP OF PAD proxi OF mmenu NOT SEEK(vusucla+'0030','IteUsuOp')
SET SKIP OF PAD corri OF mmenu NOT SEEK(vusucla+'0031','IteUsuOp')
SET SKIP OF PAD ingre OF mmenu NOT SEEK(vusucla+'0032','IteUsuOp')
SET SKIP OF PAD anula OF mmenu NOT SEEK(vusucla+'0033','IteUsuOp')
SET SKIP OF PAD lista OF mmenu NOT SEEK(vusucla+'0034','IteUsuOp')
USE IN iteusuop
SELECT (calias)
RETURN
*

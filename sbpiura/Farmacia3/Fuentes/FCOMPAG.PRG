**********************************
* fCompag.Prg
* Comprobantes de Pago
* A.S. : Marco E. Rodriguez Abad
**********************************

CLOSE DATABASES
USE IN  1 FComPag ALIAS compag  ORDER FComPag1
USE IN  2 FIteCP  ALIAS itecp   ORDER FIteCP1
USE IN  3 FOrdPed ALIAS ordped  ORDER FOrdPed1
USE IN  4 FIteOP  ALIAS iteop   ORDER FIteOP1
USE IN  5 ArtMae  ALIAS artmae  ORDER ArtMae1
USE IN  6 IteArt  ALIAS iteart  ORDER IteArt2
USE IN  7 KardexV ALIAS kardex  ORDER Kardexv2
USE IN  8 StkAlmV ALIAS stkalmv ORDER StkAlmv1
USE IN  9 Parmae  ALIAS parma   ORDER Parmae1
USE IN 10 Promae   ALIAS promae  ORDER ProMae1
USE IN 11 Caja     ALIAS caja    ORDER Caja1
USE IN 12 Clientes ALIAS Clien   ORDER Clientes1

vmens01 = 'Registro de Comprobantes de Pago'
vmens02 = 'Comprobantes de Pago : REVISION '
vmens04 = 'Dicho C/P no fue encontrado'
vmens05 = 'No existe Comprobante anterior'
vmens06 = 'No existe Comprobante siguiente'
vmens07 = '? Desea Anular este Comprobante ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Comprobante ha sido anulado'
vmens10 = 'Edici?n'
pigv = 1.18 

SELECT compag
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
vtempo = ' Revisa  Busca  Anterior  Siguiente           Ingresa  Anula  Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00 TO 11, 79 TITLE vmens01 FOOTER '[F9] Detalle : Item'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 12, 00 TO 23, 79 TITLE 'Detalle: Orden de Pedido de farmacia'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 00, 00 TO 23, 79 TITLE vmens02 DOUBLE COLOR SCHEME 10

DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis OF mmenu PROMPT '\<Revisa' AT 24, 00
DEFINE PAD busca OF mmenu PROMPT '\<Busca' AT 24, 08
DEFINE PAD anter OF mmenu PROMPT '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu PROMPT '\<Siguiente' AT 24, 25
DEFINE PAD ingre OF mmenu PROMPT '\<Ingresa' AT 24, 45
DEFINE PAD anula OF mmenu PROMPT 'A\<nula  ' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT '\<Listar ' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD anula OF mmenu DO anula
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
*------------------
ACTIVATE WINDOW wind_0
CLEAR
@ 0, 59 SAY 'Estado :'
@ 1, 2 SAY '             C/P :'
@ 1, 50 SAY ' Fecha C/P :'
@ 3, 2 SAY '    Pedidos Ref. :'
@ 4, 2 SAY '         Cliente :'
@ 5, 2 SAY '         Observa :'
@ 6, 2 SAY '     Valor Venta :'
@ 7, 2 SAY '             IGV :'
@ 8, 2 SAY '        Redondeo :'
@ 6,33 SAY '    Valor Compra :'
@ 7,33 SAY '  Total Recibido :'
@ 8,33 SAY '          Vuelto :'
RETURN
*
PROCEDURE vista
*--------------
SELECT compag
ON KEY LABEL F9 DO VISTA_DET
ON KEY LABEL F4 DO Imprimir
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 0, 12 SAY val_tipdoc()	COLOR	SCHEME 1
@ 0, 65 SAY val_est()		COLOR	SCHEME 1
@ 1, 22 SAY m.periodo
@ 1, 25 SAY m.numcp
@ 1, 65 SAY m.feccp
@ 3, 22 SAY m.pedidos
@ 4, 22 SAY m.ruccli
@ 4, 35 SAY m.cliente
@ 5, 22 SAY LEFT(m.observa, 50)
@ 6, 22 SAY m.valcom	PICTURE '9,999.99'
@ 7, 22 SAY m.igv		PICTURE '9,999.99'
@ 8, 22 SAY m.Redondeo	PICTURE '9,999.99'
@ 6, 52 SAY m.totcom	PICTURE '9,999.99'
@ 7, 52 SAY m.TotRec	PICTURE '9,999.99'
@ 8, 52 SAY m.Vuelto	PICTURE '9,999.99'
DO vista_hijo
IF !vflag $ 'J*'
	DO subopc
ENDIF
RETURN
*
PROCEDURE vista_hijo
*-------------------
HIDE POPUP ALL
SELECT itecp
GOTO TOP
SEEK m.periodo + m.numcp
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS item :H = 'ITEM', desart :H =  ;
            'Descripci?n' : 50,  ;
            totcan :H =  ;
            'Cantidad' :P =  ;
            '9,999', total :H =  ;
            'Total' :P =  ;
            '9,999.999', cantidad  ;
            :H = 'Unid.',  ;
            fraccion :H = 'Frac.',  ;
            preven :H = 'P.U.',  ;
            prevenfr :H =  ;
            'P.U.Fr.' NOMENU  ;
            NOAPPEND NODELETE  ;
            NOCLEAR WINDOW wind_1  ;
            KEY m.periodo +  ;
            m.numcp TIMEOUT  ;
            0.0001  NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 4, 25 SAY  ;
       'No hay Detalles de este Documento'
     ACTIVATE WINDOW wind_0
ENDIF
SELECT compag
RETURN
*
FUNCTION val_est
PRIVATE mret
DO CASE
     CASE m.estado = '00'
          mret = 'Emitido  '
     CASE m.estado = '99'
          mret = 'Anulado  '
ENDCASE
RETURN mret
*
FUNCTION val_tipdoc
PRIVATE mret
DO CASE
     CASE m.tipcp = 'B/V'
          mret = 'B/V N? : ' +  ;
                 m.bolvta
     CASE m.tipcp = 'FAC'
          mret = 'FAC N? : ' +  ;
                 m.factura
     CASE m.tipcp = 'REC'
          mret = 'REC N? : ' +  ;
                 m.recibo
     OTHERWISE
          mret = SPACE(19)
ENDCASE
RETURN mret
*
PROCEDURE vista_det
SELECT itecp
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
ON KEY LABEL F4
HIDE POPUP ALL
GOTO TOP
SEEK m.periodo + m.numcp
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            item :H = 'ITEM',  ;
            desart :H =  ;
            'Descripci?n' : 50,  ;
            totcan :H =  ;
            'Cantidad' :P =  ;
            '9,999', total :H =  ;
            'Total' :P =  ;
            '9,999.999', cantidad  ;
            :H = 'Unid.',  ;
            fraccion :H = 'Frac.',  ;
            preven :H = 'P.U.',  ;
            prevenfr :H =  ;
            'P.U. Fr.' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_1 KEY  ;
            m.periodo + m.numcp  ;
            TITLE vtempo  ;
            NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 4, 25 SAY  ;
       'No hay Detalles de este Documento'
ENDIF
SHOW MENU mmenu
SELECT compag
DO vista
RETURN
*
PROCEDURE revis
ON KEY LABEL F9
ON KEY LABEL F4
SELECT compag
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
DO logos WITH rotulo1, vtempo
SET RELATION TO periodo + numcp INTO itecp
SET SKIP TO itecp
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE FIELDS periodo :H =  ;
       'Periodo', numcp :H =  ;
       'C/P', itecp.item :H =  ;
       'ITEM', itecp.desart :H =  ;
       'Descripci?n' : 50,  ;
       itecp.cantidad :H =  ;
       'Unid.', itecp.fraccion :H =  ;
       'Fracc.', itecp.fraccion  ;
       :H = 'Cantidad',  ;
       itecp.preven :H = 'P.U.',  ;
       itecp.prevenfr :H =  ;
       'P.U. Fr.', itecp.total :H =  ;
       'Total' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_2
vtempo = '????????????????????????????????????????????????????????????????????????????????'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELECT compag
SET RELATION TO
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
*
PROCEDURE busca
*--------------
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
DEFINE POPUP plista FROM 19, 40 COLOR SCHEME c_popup
DEFINE BAR 1 OF plista PROMPT '\<a. Por Codigo    '
DEFINE BAR 2 OF plista PROMPT '\<b. Por B/V       '
DEFINE BAR 3 OF plista PROMPT '\<b. Por Factura   '
ON SELECTION POPUP plista DEACTIVATE POPUP
ACTIVATE POPUP plista
cord1 = ORDER()
DO CASE
     CASE BAR() = 1
          vper = RIGHT(STR(YEAR(m.fecsis),4), 2)
          vbusca = SPACE(4)
          vnombre = 'Codigo :'
          SET ORDER TO FComPag1
     CASE BAR() = 2
          vbusca = SPACE(10)
          vnombre = 'B/V    : '
          SET ORDER TO FComPag2
     CASE BAR() = 3
          vbusca = SPACE(10)
          vnombre = 'FACTURA: '
          SET ORDER TO FComPag3
     OTHERWISE
          vbusca = ''
          vnombre = ''
          SET ORDER TO
ENDCASE
IF LASTKEY() # 27
     DEFINE WINDOW lista FROM 09,12 TO 16, 68 FLOAT TITLE ' ?? B?squeda ?? '  ;
            DOUBLE COLOR SCHEME 5
     ACTIVATE WINDOW lista
     IF BAR() = 1
          @ 2, 2 SAY 'Periodo:' GET vper
     ENDIF
     @ 3, 2 SAY vnombre GET vbusca
     READ VALID val_read()
     DEACTIVATE WINDOW lista
ENDIF
IF EMPTY(vbusca) OR LASTKEY() = 27
ELSE
     DO CASE
          CASE BAR() = 1
               SEEK vper + ALLTRIM(vbusca)
          CASE BAR() = 2
               SEEK ALLTRIM(vbusca)
     ENDCASE
     IF  !FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
SET ORDER TO &cOrd1
RETURN
*
FUNCTION vbusca
*--------------
vnum_cp = PADL(ALLTRIM(vnum_cp), 4, '0')
RETURN .T.
*
PROCEDURE anter
*--------------
SELECT compag
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  !BOF()
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
SELECT compag
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  !EOF()
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
SELECT compag
SCATTER MEMVAR
DO pantalla
@ 0, 05 SAY 'Tipo de C/P'
@ 0, 65 GET m.estado DISABLE
@ 1, 22 GET m.periodo DISABLE
@ 1, 25 GET m.numcp DISABLE
@ 1, 65 GET m.feccp WHEN .F.
@ 4, 22 SAY m.ruccli
@ 4, 35 GET m.cliente
@ 5, 22 GET m.observa FUNCTION 'S50'
@ 6, 22 GET m.valcom DISABLE
@ 7, 22 GET m.igv DISABLE
@ 8, 22 GET m.totcom DISABLE
READ VALID val_read()
IF LASTKEY() # 27
     ok = trabaja_hi()
     IF ok AND LASTKEY() # 27
          SELECT compag
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
SELECT compag
DO vista
RETURN
*
PROCEDURE ingre
*--------------
PUBLIC m.Pretot
SELECT stkalmv
GOTO TOP
IF EOF()
	DO standby WITH 'No existe Inventario Inicial en el sistema. por favor revise'
	SELECT compag
	DO vista
	RETURN
ENDIF
SELECT kardex
GOTO TOP
IF estado = '20'
	DO standby WITH 'El Periodo ya est? cerrado no se puede hacer modificaciones'
	SELECT compag
	DO vista
	RETURN
ENDIF
ON KEY LABEL F9
ON KEY LABEL F4
SELECT compag
nreg = IIF(!EOF(),RECNO(),-1)
DO pantalla
SCATTER BLANK MEMVAR
m.estado  = '00'
m.lRecibo = ''
m.TotRec  = 00
m.Vuelto  = 00
m.periodo = RIGHT(STR(YEAR(m.fecsis), 4), 2)
m.PreTot  = 00
m.feccp   = m.fecsis
SELECT parma
SEEK 'CORRELFCOMP '
IF FOUND()
	m.numcp = PADL(ALLTRIM(STR(nument + 1)), 4, '0')
	IF SEEK(m.periodo + m.numcp, 'ComPag')
		DO standby WITH 'El Correlativo Generado ya Existe.'
		SELECT compag
		DO vista
		RETURN
	ENDIF
ELSE
	DO standby WITH 'El Parametro de Correlativos no existe. por favor consulte al area de Sistemas'
	DO vista
	RETURN
ENDIF
SELECT compag
@ 0, 05 SAY 'Tipo de C/P'
@ 0, 65 GET m.estado DISABLE
@ 1, 22 GET m.periodo DISABLE
@ 1, 25 GET m.numcp DISABLE
@ 1, 65 GET m.feccp WHEN .F.
@ 4, 22 GET m.ruccli WHEN .F.
@ 4, 35 GET m.cliente WHEN .F.
@ 5, 22 GET m.observa FUNCTION 'S50' WHEN .F.
@ 6, 22 GET m.valcom DISABLE	PICTURE '9,999.99'
@ 7, 22 GET m.igv DISABLE		PICTURE '9,999.99'
@ 8, 22 GET m.totcom DISABLE	PICTURE '9,999.99'

IF LASTKEY() # 27
	ok = trabaja_hi()
*	IF ok AND LASTKEY() # 27
*		ok = comp_sunat()
*	ENDIF
	IF ok AND LASTKEY() # 27
		ok = act_caja()
	ENDIF
	IF ok AND LASTKEY() # 27
		SELECT compag
		IF f_appd()
			m.tuser = 'I'
			m.user = vuser_id
			m.duser = DATE()
			m.huser = TIME()
			GATHER MEMVAR
			SELECT parma
			SEEK 'CORRELFCOMP '
			IF FOUND()
				IF f_lock(1)
					REPLACE nument WITH nument + 1
				ENDIF
			ENDIF
		ENDIF
	ELSE
		IF nreg # -1
			GOTO nreg
		ENDIF
		DO standby WITH 'Proceso cancelado'
	ENDIF
ELSE
	IF nreg # -1
		GOTO nreg
	ENDIF
	DO standby WITH 'Proceso cancelado'
ENDIF
UNLOCK ALL
FLUSH
SELECT compag
DO vista
RETURN
*
FUNCTION trabaja_hi
*------------------
PRIVATE xok
vsun = .T.
xok = .F.
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '?????F2->Edita?????????F5->Agregar?????????F8->Eliminar????????F10->Terminar????'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO agrega_Ped
ON KEY LABEL F8 DO elimi_Ped
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itecp
SET ORDER TO FiteCP1
SEEK m.periodo + m.numcp
IF  !FOUND()
	agr = agrega_ped()
ELSE
	agr = .T.
ENDIF
IF agr
	BROWSE NOOPTIMIZE FIELDS  ;
			item   :H = 'Item',  ;
			desart :H = 'Descripci?n' : 50,  ;
			totcan :H = 'Cantidad' :P = '9,999',;
			total  :H = 'Total' :P = '9,999.999',;
			lfracc :H = 'Fracc.';
			NOMENU NOAPPEND NOEDIT NODELETE NOCLEAR  ;
			WINDOW wind_1 KEY m.periodo + m.numcp WHEN .F. NOREFRESH
			
	IF LASTKEY() # 27
		m.totcom = 0
		m.PreTot = 0
		SEEK m.periodo + m.numcp
		IF FOUND()
			SCAN WHILE periodo + numcp = m.periodo + m.numcp
				m.PreTot = m.PreTot + total
				m.totcom = m.totcom + total
			ENDSCAN
		ENDIF
		IF m.totcom > 0
			m.igv = m.totcom - ROUND(m.totcom / pigv, 2)
			m.valcom = m.totcom - m.igv
		ENDIF
*		set step on
*		xok = .t.			&& comp_sunat()
		xok = comp_sunat()
	ELSE
		xok = .F.
	ENDIF
	
	IF xok
	ELSE
		SEEK m.periodo + m.numcp
		IF FOUND()
			SCAN WHILE periodo = m.periodo AND numcp = m.numcp
				SELECT iteop
				IF SEEK(itecp.periodo + itecp.numped + itecp.item + itecp.codart)
					IF f_lock(1)
						REPLACE estado WITH '00', numcp WITH ''
					ENDIF
				ENDIF
				SELECT ordped
				IF SEEK(itecp.periodo + itecp.numped)
					IF f_lock(1)
						REPLACE estado WITH '00', numcp WITH ''
					ENDIF
				ENDIF
				SELECT itecp
				IF f_lock(1)
					DELETE NEXT 1
					UNLOCK
				ENDIF
			ENDSCAN
		ENDIF
	ENDIF
ELSE
	DO standby WITH 'No Existen detalles. Por favor Revise.'
	vsun = .F.
ENDIF
IF LASTKEY() # 27 AND vsun
	vsun = .T.
ELSE
	vsun = .F.
ENDIF
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '????????????????????????????????????????????????????????????????????????????????'
DO logos WITH rotulo1, vtempo
SELECT compag
RETURN vsun
*
FUNCTION agrega_ped
*------------------
PRIVATE calias, mret
calias = ALIAS()
SELECT ordped
SET FILTER TO estado = '00'
SELECT iteop
SET FILTER TO estado = '00'
SELECT ordped
GOTO TOP
IF EOF()
	DO standby WITH 'No Existen Pedidos por Procesar'
	SET FILTER TO
	SELECT iteop
	SET FILTER TO
	SELECT (calias)
	RETURN .F.
ENDIF
DEFINE WINDOW wventana FROM 05,05 TO 10,60
ACTIVATE WINDOW wventana
vperiodo = RIGHT(STR(YEAR(fecsis),4), 2)
vnumped  = SPACE(4)
@ 01, 01 SAY 'Ingrese N? Pedido:' GET vnumped VALID fval_ped()
READ
m.cliente = ordped.cliente
m.lRecibo = ordped.lRecibo
m.observa = ordped.observa
DEACTIVATE WINDOW wventana
RELEASE WINDOW wventana
IF LASTKEY() # 27
	SELECT iteop
	SEEK vperiodo + vnumped
	IF FOUND()
		SCAN WHILE periodo = vperiodo AND numped = vnumped
			IF f_lock(1)
				REPLACE estado WITH '20', numcp WITH m.numcp
			ENDIF
			SCATTER MEMVAR
			SELECT itecp
			IF f_appd()
				m.tuser = 'I'
				m.user = vuser_id
				m.duser = DATE()
				m.huser = TIME()
				m.estado = '00'
				GATHER MEMVAR
			ENDIF
			UNLOCK
			SELECT iteop
		ENDSCAN
	ENDIF
	SELECT ordped
	SEEK vperiodo + vnumped
	IF FOUND()
		IF f_lock(1)
			REPLACE estado WITH '20', numcp WITH m.numcp
		ENDIF
	ENDIF
	SET FILTER TO
	SELECT iteop
	SET FILTER TO
	SEEK vperiodo + vnumped
	m.pedidos = IIF(EMPTY(m.pedidos),vnumped,ALLTRIM(m.pedidos)+', '+vnumped)
	mret = .T.
ELSE
	mret = .F.
ENDIF
SELECT (calias)
RETURN mret
*
FUNCTION fval_ped
*----------------
IF !SEEK(vperiodo+vnumped) AND EMPTY(vnumped)
     DEFINE POPUP v__xx FROM 1,10 TO 12, 79 PROMPT FIELDS numped +  ;
            LEFT(cliente, 50) + STR(totped, 13, 4)
     ON SELECTION POPUP v__xx DEACTIVATE POPUP
     ACTIVATE POPUP v__xx
     RELEASE POPUP v__xx
ENDIF
IF LASTKEY() # 27
     vnumped = numped
     mret = .T.
ELSE
     mret = .F.
ENDIF
RETURN mret
*
PROCEDURE actComSunat
*--------------------
IF LASTKEY() # 27
	DO CASE
		CASE m.tipcp = 'B/V'
			SELECT compag
			IF f_lock(1)
				REPLACE bolvta WITH m.bolvta, tipcp  WITH m.tipcp, cliente WITH m.cliente
			ENDIF
			
			SELECT itecp
			SEEK m.periodo + m.numcp
			SCAN WHILE periodo + numcp = m.periodo + m.numcp
				IF f_lock(1)
					REPLACE tipcp WITH m.tipcp, bolvta WITH m.bolvta
				ENDIF
				
				SELECT iteop
				IF SEEK(itecp.periodo + itecp.numped + itecp.item + itecp.codart)
					IF f_lock(1)
						REPLACE tipcp WITH m.tipcp,	bolvta WITH m.bolvta
					ENDIF
					
					SELECT kardex
					IF SEEK(iteop.periodo+'OPF'+iteop.numped+iteop.item+iteop.codart)
						IF f_lock(1)
							REPLACE boleta WITH iteop.bolvta
						ENDIF
					ENDIF
				ENDIF
				SELECT ordped
				IF SEEK(itecp.periodo+itecp.numped)
					IF f_lock(1)
                         REPLACE tipcp WITH m.tipcp, bolvta WITH m.bolvta
					ENDIF
				ENDIF
				SELECT itecp
			ENDSCAN
			SELECT parma
			SEEK 'CORRELFBV   '
			IF f_lock(1)
				REPLACE nument WITH nument + 1
			ENDIF
			mret = .T.
		CASE m.tipcp = 'FAC'
			SELECT compag
			IF f_lock(1)
				REPLACE factura WITH m.factura, tipcp   WITH m.tipcp,  ;
						ruccli  WITH m.ruccli,  cliente WITH m.cliente
			ENDIF
			SELECT itecp
			SEEK m.periodo + m.numcp
			SCAN WHILE periodo + numcp = m.periodo + m.numcp
				IF f_lock(1)
					REPLACE tipcp WITH m.tipcp, factura WITH m.factura
				ENDIF
				SELECT iteop
				IF SEEK(itecp.periodo + itecp.numped + itecp.item + itecp.codart)
					IF f_lock(1)
						REPLACE tipcp WITH m.tipcp, factura WITH m.factura
					ENDIF
					SELECT kardex
					IF SEEK(iteop.periodo+'OPF'+iteop.numped+iteop.item+iteop.codart)
						IF f_lock(1)
							REPLACE factura WITH iteop.factura
						ENDIF
					ENDIF
				ENDIF
				SELECT ordped
				IF SEEK(itecp.periodo+itecp.numped)
					IF f_lock(1)
						REPLACE tipcp WITH m.tipcp, factura WITH m.factura
					ENDIF
				ENDIF
				SELECT itecp
			ENDSCAN
			SELECT parma
			SEEK 'CORRELFFAC  '
			IF f_lock(1)
				REPLACE nument WITH nument + 1
			ENDIF
			mret = .T.
		CASE m.tipcp = 'REC'
			SELECT compag
			IF f_lock(1)
				REPLACE recibo WITH m.recibo, tipcp WITH m.tipcp,  ;
    	                   ruccli WITH m.ruccli, cliente WITH m.cliente
			ENDIF
			SELECT itecp
			SEEK m.periodo + m.numcp
			SCAN WHILE periodo + numcp = m.periodo + m.numcp
				IF f_lock(1)
					REPLACE tipcp WITH m.tipcp, recibo WITH m.recibo
				ENDIF
				SELECT iteop
				IF SEEK(itecp.periodo+itecp.numped+itecp.item+itecp.codart)
					IF f_lock(1)
						REPLACE tipcp WITH m.tipcp, recibo WITH m.recibo
					ENDIF
					SELECT kardex
					IF SEEK(iteop.periodo + 'OPF' + iteop.numped + iteop.item + iteop.codart)
						IF f_lock(1)
							REPLACE recibo WITH iteop.recibo
						ENDIF
					ENDIF
				ENDIF
				SELECT ordped
				IF SEEK(itecp.periodo + itecp.numped)
					IF f_lock(1)
						REPLACE tipcp WITH m.tipcp, recibo WITH m.recibo
					ENDIF
				ENDIF
				SELECT itecp
			ENDSCAN
			SELECT parma
			SEEK 'CORRELFREC  '
			IF f_lock(1)
				REPLACE nument WITH nument + 1
			ENDIF
			mret = .T.
	ENDCASE
ELSE
     mret = .F.
ENDIF

FUNCTION act_caja
*----------------
PRIVATE calias
calias = ALIAS()
DIMENSION matrizuno( 1)
mret = .T.
IF m.totcom > 0
     xpedidos = m.pedidos
     DO WHILE LEN(xpedidos)>0
          xnumped = LEFT(xpedidos,4)
          m.tipcaj = '1'
          m.fecreg = m.feccp
          m.coring = xnumped
          SELECT ordped
          IF SEEK(m.periodo + m.coring)
               m.importe = totped
               xuser = user
               xusercr = usercr
               xduser = duser
               xdusercr = dusercr
               xhuser = huser
               xhusercr = husercr
          ELSE
               DO standby WITH 'Error. No se ubic? Pedido'
               mret = .F.
          ENDIF
          IF mret
               SELECT caja
               IF f_appd()
                    GATHER MEMVAR
                    REPLACE estado WITH '00'
                    UNLOCK
               ENDIF
          ENDIF
          IF AT(',', xpedidos) > 0
               xpedidos = ALLTRIM(SUBSTR(xpedidos,(AT(',',xpedidos) + 1)))
          ELSE
               xpedidos = ''
          ENDIF
     ENDDO
ELSE
     mret = .F.
ENDIF
SELECT (calias)
RETURN mret
*
FUNCTION comp_sunat
*------------------
PRIVATE calias
calias = ALIAS()
DEFINE WINDOW wventana FROM 05,05 TO 15, 60
ACTIVATE WINDOW wventana
IF m.lRecibo # 'S'
	@ 00, 01 SAY '   Bol/Fac:' GET m.tipcp PICTURE '@M B/V,FAC' VALID val_tc()
ELSE
	@ 00, 01 SAY '   Bol/Fac:' GET m.tipcp PICTURE '@M REC' VALID val_tc()
ENDIF
@ 00, 22 SAY IIF(m.tipcp = 'B/V',m.bolvta, IIF(m.tipcp = 'FAC',m.factura, IIF(m.tipcp = 'DON',m.donacion, IIF(m.tipcp = 'REC',m.recibo, ''))))
@ 01, 01 SAY '           RUC :' GET m.ruccli PICTURE '!!!!!!!!!!!' WHEN m.tipcp = 'FAC' VALID val_Ruc() AND Ruc_Blanco()
@ 02, 01 SAY '        Nombre :' GET m.cliente	WHEN m.tipcp#'FAC'
@ 03, 01 SAY ' Observaciones :' GET m.observa	PICTURE "@S30"
@ 04, 01 SAY '         Total :'	GET m.PreTot	PICTURE '999.99'	WHEN .F.
@ 05, 01 SAY '      Redondeo :'	GET m.Redondeo	PICTURE '999.99'	VALID vRedondeo()
@ 06, 01 SAY ' Total a pagar :'	GET m.TotCom	PICTURE '999.99'	WHEN .F.
@ 07, 01 SAY 'Total recibido :'	GET m.TotRec	PICTURE '999.99'	VALID vTotRec()
@ 08, 01 SAY '        Vuelto :'	GET m.Vuelto	PICTURE '999.99'

READ VALID vTotRec()
DEACTIVATE WINDOW wventana
RELEASE WINDOW wventana
*SET STEP ON
IF LASTKEY() # 27 AND vTotRec()
	DO CASE
		CASE m.tipcp = 'B/V'
			SELECT itecp
			SEEK m.periodo + m.numcp
			SCAN WHILE periodo + numcp = m.periodo + m.numcp
				IF f_lock(1)
					REPLACE tipcp WITH m.tipcp, bolvta WITH m.bolvta
				ENDIF
				
				SELECT iteop
				IF SEEK(itecp.periodo + itecp.numped + itecp.item + itecp.codart)
					IF f_lock(1)
						REPLACE tipcp WITH m.tipcp,	bolvta WITH m.bolvta
					ENDIF
					
					SELECT kardex
					IF SEEK(iteop.periodo+'OPF'+iteop.numped+iteop.item+iteop.codart)
						IF f_lock(1)
							REPLACE boleta WITH iteop.bolvta
						ENDIF
					ENDIF
				ENDIF
				SELECT ordped
				IF SEEK(itecp.periodo+itecp.numped)
					IF f_lock(1)
						REPLACE tipcp WITH m.tipcp, bolvta WITH m.bolvta
					ENDIF
				ENDIF
				SELECT itecp
			ENDSCAN
			SELECT parma
			SEEK 'CORRELFBV   '
			IF f_lock(1)
				REPLACE nument WITH nument + 1
			ENDIF
			mret = .T.
		CASE m.tipcp = 'FAC'
			SELECT itecp
			SEEK m.periodo + m.numcp
			SCAN WHILE periodo + numcp = m.periodo + m.numcp
				IF f_lock(1)
					REPLACE tipcp WITH m.tipcp, factura WITH m.factura
				ENDIF
				SELECT iteop
				IF SEEK(itecp.periodo + itecp.numped + itecp.item + itecp.codart)
					IF f_lock(1)
						REPLACE tipcp WITH m.tipcp, factura WITH m.factura
					ENDIF
					SELECT kardex
					IF SEEK(iteop.periodo+'OPF'+iteop.numped+iteop.item+iteop.codart)
						IF f_lock(1)
							REPLACE factura WITH iteop.factura
						ENDIF
					ENDIF
				ENDIF
				SELECT ordped
				IF SEEK(itecp.periodo+itecp.numped)
					IF f_lock(1)
						REPLACE tipcp WITH m.tipcp, factura WITH m.factura
					ENDIF
				ENDIF
				SELECT itecp
			ENDSCAN
			SELECT parma
			SEEK 'CORRELFFAC  '
			IF f_lock(1)
				REPLACE nument WITH nument + 1
			ENDIF
			mret = .T.
*          CASE m.tipcp = 'REC'
*               SELECT compag
*               REPLACE recibo WITH m.recibo, tipcp WITH m.tipcp,  ;
*                       ruccli WITH m.ruccli, cliente WITH m.cliente
*               SELECT itecp
*               SEEK m.periodo + m.numcp
*               SCAN WHILE periodo + numcp = m.periodo + m.numcp
*                    REPLACE tipcp WITH m.tipcp, recibo WITH m.recibo
*                    SELECT iteop
*                    IF SEEK(itecp.periodo+itecp.numped+itecp.item+itecp.codart)
*                         REPLACE tipcp WITH m.tipcp, recibo WITH m.recibo
*                         SELECT kardex
*                         IF SEEK(iteop.periodo + 'OPF' + iteop.numped + iteop.item + iteop.codart)
*                              REPLACE recibo WITH iteop.recibo
*                         ENDIF
*                    ENDIF
*                    SELECT ordped
*                    IF SEEK(itecp.periodo + itecp.numped)
*                         REPLACE tipcp WITH m.tipcp, recibo WITH m.recibo
*                    ENDIF
*                    SELECT itecp
*               ENDSCAN
*               SELECT parma
*               SEEK 'CORRELFREC  '
*               REPLACE nument WITH nument + 1
*               mret = .T.
     ENDCASE
     mret = .T.
ELSE
     mret = .F.
ENDIF
SELECT (calias)
RETURN mret

*FUNCTION comp_sunat
*------------------
PRIVATE calias
calias = ALIAS()
DEFINE WINDOW wventana FROM 05,05 TO 15, 60
ACTIVATE WINDOW wventana
IF m.lRecibo # 'S'
	@ 00, 01 SAY '   Bol/Fac:' GET m.tipcp PICTURE '@M B/V,FAC' VALID val_tc()
ELSE
	@ 00, 01 SAY '   Bol/Fac:' GET m.tipcp PICTURE '@M REC' VALID val_tc()
ENDIF
@ 00, 22 SAY IIF(m.tipcp = 'B/V',m.bolvta, IIF(m.tipcp = 'FAC',m.factura, IIF(m.tipcp = 'DON',m.donacion, IIF(m.tipcp = 'REC',m.recibo, ''))))
@ 01, 01 SAY '           RUC :' GET m.ruccli PICTURE '!!!!!!!!!!!' WHEN m.tipcp = 'FAC' VALID val_Ruc() AND Ruc_Blanco()
@ 02, 01 SAY '        Nombre :' GET m.cliente	WHEN m.tipcp#'FAC'
@ 03, 01 SAY ' Observaciones :' GET m.observa	PICTURE "@S30"
@ 04, 01 SAY '         Total :'	GET m.PreTot	PICTURE '999.99'	WHEN .F.
@ 05, 01 SAY '      Redondeo :'	GET m.Redondeo	PICTURE '999.99'	VALID vRedondeo()
@ 06, 01 SAY ' Total a pagar :'	GET m.TotCom	PICTURE '999.99'	WHEN .F.
@ 07, 01 SAY 'Total recibido :'	GET m.TotRec	PICTURE '999.99'	VALID vTotRec()
@ 08, 01 SAY '        Vuelto :'	GET m.Vuelto	PICTURE '999.99'

READ
DEACTIVATE WINDOW wventana
RELEASE WINDOW wventana
*SET STEP ON
IF LASTKEY() # 27 AND vTotRec()
	DO CASE
		CASE m.tipcp = 'B/V'
*			SELECT compag
*			REPLACE bolvta WITH m.bolvta, tipcp  WITH m.tipcp, cliente WITH m.cliente
*			
			SELECT itecp
			SEEK m.periodo + m.numcp
			SCAN WHILE periodo + numcp = m.periodo + m.numcp
				IF f_lock(1)
					REPLACE tipcp WITH m.tipcp, bolvta WITH m.bolvta
				ENDIF
				
				SELECT iteop
				IF SEEK(itecp.periodo + itecp.numped + itecp.item + itecp.codart)
					IF f_lock(1)
						REPLACE tipcp WITH m.tipcp,	bolvta WITH m.bolvta
					ENDIF
					
					SELECT kardex
					IF SEEK(iteop.periodo+'OPF'+iteop.numped+iteop.item+iteop.codart)
						IF f_lock(1)
							REPLACE boleta WITH iteop.bolvta
						ENDIF
					ENDIF
					
				ENDIF
				SELECT ordped
				IF SEEK(itecp.periodo+itecp.numped)
					IF f_lock(1)
						REPLACE tipcp WITH m.tipcp, bolvta WITH m.bolvta
					ENDIF
				ENDIF
				SELECT itecp
			ENDSCAN
			SELECT parma
			SEEK 'CORRELFBV   '
			IF f_lock(1)
				REPLACE nument WITH nument + 1
			ENDIF
			mret = .T.
		CASE m.tipcp = 'FAC'
*               SELECT compag
*               REPLACE factura WITH m.factura, tipcp   WITH m.tipcp,  ;
*                       ruccli  WITH m.ruccli,  cliente WITH m.cliente
			SELECT itecp
			SEEK m.periodo + m.numcp
			SCAN WHILE periodo + numcp = m.periodo + m.numcp
				IF f_lock(1)
					REPLACE tipcp WITH m.tipcp, factura WITH m.factura
				ENDIF
				SELECT iteop
					IF SEEK(itecp.periodo + itecp.numped + itecp.item + itecp.codart)
						IF f_lock(1)
							REPLACE tipcp WITH m.tipcp, factura WITH m.factura
						ENDIF
						SELECT kardex
						IF SEEK(iteop.periodo+'OPF'+iteop.numped+iteop.item+iteop.codart)
							IF f_lock(1)
								REPLACE factura WITH iteop.factura
							ENDIF
						ENDIF
					ENDIF
                    SELECT ordped
                    IF SEEK(itecp.periodo+itecp.numped)
                    	IF f_lock(1)
                    		REPLACE tipcp WITH m.tipcp, factura WITH m.factura
                    	ENDIF
                    ENDIF
                    SELECT itecp
               ENDSCAN
               SELECT parma
               SEEK 'CORRELFFAC  '
               IF f_lock(1)
	               REPLACE nument WITH nument + 1
               ENDIF
               mret = .T.
*          CASE m.tipcp = 'REC'
*               SELECT compag
*               REPLACE recibo WITH m.recibo, tipcp WITH m.tipcp,  ;
*                       ruccli WITH m.ruccli, cliente WITH m.cliente
*               SELECT itecp
*               SEEK m.periodo + m.numcp
*               SCAN WHILE periodo + numcp = m.periodo + m.numcp
*                    REPLACE tipcp WITH m.tipcp, recibo WITH m.recibo
*                    SELECT iteop
*                    IF SEEK(itecp.periodo+itecp.numped+itecp.item+itecp.codart)
*                         REPLACE tipcp WITH m.tipcp, recibo WITH m.recibo
*                         SELECT kardex
*                         IF SEEK(iteop.periodo + 'OPF' + iteop.numped + iteop.item + iteop.codart)
*                              REPLACE recibo WITH iteop.recibo
*                         ENDIF
*                    ENDIF
*                    SELECT ordped
*                    IF SEEK(itecp.periodo + itecp.numped)
*                         REPLACE tipcp WITH m.tipcp, recibo WITH m.recibo
*                    ENDIF
*                    SELECT itecp
*               ENDSCAN
*               SELECT parma
*               SEEK 'CORRELFREC  '
*               REPLACE nument WITH nument + 1
*               mret = .T.
     ENDCASE
     mret = .T.
ELSE
     mret = .F.
ENDIF
SELECT (calias)
RETURN mret
*

FUNCTION Ruc_Blanco
*------------------
PRIVATE mRet
IF EMPTY(m.RucCli)
	DO StandBy WITH "El Ruc no debe ser Vacio"
	mRet = .F.
ELSE
	mRet = .T.
ENDIF
RETURN mRet

FUNCTION vRedondeo
*-----------------
IF m.Redondeo > 0.00
	m.TotCom = m.PreTot + m.Redondeo
	SHOW GET m.TotCom
ENDIF
RETURN .T.

FUNCTION vTotRec
*--------------
PRIVATE mRet
IF m.TotCom > m.TotRec
	DO StandBy WITH 'Total Recibido no debe ser menor que la Compra'
	mRet = .F.
ELSE
	m.Vuelto = m.TotRec - m.TotCom
	SHOW GET m.Vuelto
	mRet = .T.
ENDIF
RETURN mRet

FUNCTION val_tc
*--------------
mret = .F.
DO CASE
	CASE m.tipcp = 'B/V'
		cserie = IIF(SEEK('CORRELFSBV  ','Parma'),PADL(ALLTRIM(STR(parma.nument)),3,'0'),'000')
		cbolvta = IIF(SEEK('CORRELFBV   ','Parma'),PADL(ALLTRIM(STR(parma.nument+1)),6,'0'),'000000')
		IF cserie = '000' OR cbolvta = '000000'
			DO standby WITH 'Error en Correlativo de B/V. Consulte al Area de Sistemas'
		ELSE
			m.bolvta = cserie + '-' + cbolvta
			@ 00, 22 SAY m.bolvta
			mret = .T.
		ENDIF
	CASE m.tipcp = 'FAC'
		cserie = IIF(SEEK('CORRELFSFAC','Parma'),PADL(ALLTRIM(STR(parma.nument)),3,'0'),'000')
		cfactura = IIF(SEEK('CORRELFFAC','Parma'),PADL(ALLTRIM(STR(parma.nument+1)),6,'0'),'000000')
		IF cserie = '000' OR cfactura = '000000'
			DO standby WITH 'Error en Correlativo de Facturas. Consulte al Area de Sistemas'
		ELSE
			m.factura = cserie + '-' + cfactura
			@ 00, 22 SAY m.factura
			mret = .T.
		ENDIF
	CASE m.tipcp = 'REC'
		cserie = IIF(SEEK('CORRELFSREC','Parma'),PADL(ALLTRIM(STR(parma.nument)),3,'0'),'000')
		crecibo = IIF(SEEK('CORRELFREC','Parma'),PADL(ALLTRIM(STR(parma.nument+1)), 6, '0'),'000000')
		IF cserie = '000' OR crecibo = '000000'
			DO standby WITH 'Error en Correlativo de Recibos. Consulte al Area de Sistemas'
		ELSE
			m.recibo = cserie + '-' + crecibo
			@ 00, 22 SAY m.recibo
			mret = .T.
		ENDIF
	CASE m.tipcp = 'DON'
		cserie = IIF(SEEK('CORRELFSDON','Parma'),PADL(ALLTRIM(STR(parma.nument)),2,'0'),'00')
		cdon = IIF(SEEK('CORRELFDON','Parma'),PADL(ALLTRIM(STR(parma.nument+1)), 6,'0'),'000000')
		IF cserie = '000' OR cdon = '000000'
			DO standby WITH 'Error en Correlativo de Donaciones. Consulte al Area de Sistemas'
		ELSE
			m.donacion = cserie + '-' + cdon
			@ 00, 22 SAY m.donacion
			mret = .T.
		ENDIF
ENDCASE
RETURN mret
*
PROCEDURE elimi_ped
*
PROCEDURE imprimir
*-----------------
SELECT compag
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
SCATTER MEMVAR
vdbf = SYS(3) + '.Dbf'
COPY TO (vdbf) STRUCTURE
USE IN 0 (vdbf) ALIAS xcompag
SELECT xcompag
APPEND BLANK
GATHER MEMVAR
SET RELATION TO periodo + numcp INTO itecp ADDITIVE
SET SKIP TO itecp

DO CASE
	CASE m.TipCp = 'B/V'
*		SET DEVICE TO PRINTER
*		??CHR(27)+CHR(64)
*		??CHR(27)+CHR(77)
*		??CHR(27)+CHR(67)+chr(24)
		DO reporte WITH 2, 'FBoleta',' Comprobante de Pago.', 2,.F., .T.
	CASE m.TipCp = 'FAC'
		DO reporte WITH 2, 'FFactura',' Comprobante de Pago.', 2,.F., .T.
*		do RepPrg  with "fFactura",' Facturas '
*	CASE m.TipCp = 'REC'
*		DO reporte WITH 2, 'FComPag',' Comprobante de Pago.', 2,.F., .T.
ENDCASE

USE IN xcompag
ERASE (vdbf)
SELECT compag
RETURN
*
PROCEDURE fBoleta  			&&Programa Reporte de boleta
*----------------
PARAMETER _desti
IF _desti=2
	SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINT
Impri    = .F.
_PLENGTH = 21

STORE 0 TO pagina,linea,xVal	&&,X_SW1,lVal

SELE IteCP
SCAN WHILE IteCP.NumCP = ComPag.NumCP
	IF pagina = 0 .OR. linea > 19
		DO TitBoleta
	ENDIF
	DO CASE
		CASE linea < 17
			@ LINEA,05  SAY IteCP.Item
			@ LINEA,09  SAY PADL(ALLTRIM(IteCP.Cantidadx),7,' ')
			@ LINEA,16  SAY IteCP.DesArt
			@ LINEA,23  SAY IteCP.Total
			
			xval=xval+IteCp.Total
			linea = linea + 1
		CASE linea >= 17
			SKIP -1
			xReg = RECNO()
*			@ linea,1 say Letras(xcompag.totcom,"Nuevos Soles.")
			DO SUMBoleta
			DO TITBoleta
			SELE IteCP
			GO xReg
		OTHER
	ENDCASE
ENDSCAN

DO SUMBoleta

SELE ComPag
SET DEVICE TO SCREEN
SET PRINTER TO

RETURN

PROCEDURE fFactura  			&&Programa Reporte de boleta
*-----------------
PARAMETER _desti
IF _desti=2
	SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINT
Impri    = .F.

STORE 0 TO pagina,linea,xVal	&&,X_SW1,lVal

SELE IteCP
SCAN WHILE IteCP.NumCP = ComPag.NumCP
	IF pagina = 0 .OR. linea > 28
		DO TitFactura
	ENDIF
	DO CASE
		CASE linea < 24
			@ LINEA,05  SAY IteCP.Item
			@ LINEA,09  SAY PADL(ALLTRIM(IteCP.Cantidadx),7,' ')
			@ LINEA,19  SAY IteCP.DesArt
			@ LINEA,50  SAY IteCP.Total
			
			xval=xval+IteCp.Total
			linea = linea + 1
		CASE linea >= 24
			SKIP -1
			xReg = RECNO()
*			@ linea,1 say Letras(xcompag.totcom,"Nuevos Soles.")
*			@ linea + 1 ,1 say xcompag.totcom
*			@ linea + 2 ,1 say xcompag.IGV
*			@ linea + 3 ,1 say xcompag.totcom - xcompag.IGV
			DO SumFactura
			DO TITFactura
			SELE IteCP
			GO xReg
		OTHER
	ENDCASE
ENDSCAN

DO SUMFactura

SELE ComPag
SET DEVICE TO SCREEN
SET PRINTER TO

RETURN

PROCEDURE TitBoleta
*------------------
Pagina = Pagina + 1
@0,00 SAY CHR(27)+CHR(64)
@0,02 SAY CHR(15)
@3,07 SAY xcompag.cliente
@7,39 SAY xcompag.feccp
@7,54 SAY IIF(tipcp='FAC','Fac.:'+factura,IIF(tipcp='B/V','B/V:'+bolvta,IIF(tipcp='REC','Rec.:'+recibo,'')))
linea = 8
RETURN

PROCEDURE TitFactura
*-------------------
Pagina = Pagina + 1
@00,00 SAY CHR(27)+CHR(64)
@00,02 SAY CHR(15)
@06,07 SAY xcompag.cliente
@08,07 SAY xcompag.RucCli
@08,39 SAY xcompag.feccp
@08,54 SAY IIF(tipcp='FAC','Fac.:'+factura,IIF(tipcp='B/V','B/V:'+bolvta,IIF(tipcp='REC','Rec.:'+recibo,'')))
linea = 9
RETURN

PROCEDURE SumBoleta
*------------------
Linea = 17
@ linea,1 say Letras(xcompag.totcom,"Nuevos Soles.")
Linea = Linea + 1
@ linea,50 say xcompag.totcom
RETURN

PROCEDURE SumFactura
*-------------------
Linea = 25
@ linea,1 say Letras(xcompag.totcom,"Nuevos Soles.")
Linea = Linea + 1
@ linea,50 say xcompag.totcom - xcompag.IGV
Linea = Linea + 1
@ linea,50 say xcompag.IGV
Linea = Linea + 1
@ linea,50 say xcompag.totcom
RETURN

PROCEDURE lista
*--------------
DO standby WITH 'Opci?n todavia en desarrollo'
RETURN
*
PROCEDURE anula
*--------------
lanula = .F.
IF yesno('Esta seguro de Anular este Documento')
     lanula = .T.
ENDIF
IF lanula
     DO CASE
          CASE estado = '99'
               DO standby WITH 'El Docuemnto ya esta Anulado'
               lanula = .F.
          OTHERWISE
               lanula = .T.
     ENDCASE
ENDIF
IF lanula
     SELECT itecp
     IF SEEK(m.periodo + m.numcp)
          SCAN WHILE periodo + numcp = m.periodo + m.numcp
               SELECT iteop
               IF SEEK(itecp.periodo + itecp.numped + itecp.item)
                    SELECT ordped
                    IF SEEK(iteop.periodo + iteop.numped)
                         IF f_lock(1)
                              REPLACE estado WITH '00',;
                              		numcp WITH '',  ;
                              		tipcp WITH '',  ;
                              		bolvta WITH '',  ;
                              		factura WITH '', ;
                              		recibo  WITH ''
                         ENDIF
                    ENDIF
                    SELECT kardex
                    IF SEEK(iteop.periodo + 'OPF' + iteop.numped + iteop.item)
                         IF f_lock(1)
                              REPLACE boleta WITH  '', factura WITH '', recibo WITH ''
                         ENDIF
                    ENDIF
                    SELECT caja
                    IF SEEK('1'+iteop.numped)
                    	IF f_lock(1)
	                         REPLACE estado WITH '99'
                    	ENDIF
                    ENDIF
                    SELECT iteop
                    IF f_lock(1)
                         REPLACE estado WITH '00',  ;
                                 numcp  WITH '',  ;
                                 tipcp  WITH '',  ;
                                 bolvta WITH '',  ;
                                 factura WITH '',  ;
                                 recibo WITH ''
                    ENDIF
               ENDIF
               SELECT itecp
               IF f_lock(1)
                    REPLACE estado WITH '99', numped WITH ''
               ENDIF
          ENDSCAN
          SELECT compag
          IF f_lock(1)
               REPLACE estado WITH '99', pedidos WITH ''
          ENDIF
     ENDIF
     SELECT compag
ENDIF
SELECT compag
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
SET SKIP OF PAD revis OF mmenu !SEEK(vusucla + '0035', 'IteUsuOp')
SET SKIP OF PAD busca OF mmenu !SEEK(vusucla + '0036', 'IteUsuOp')
SET SKIP OF PAD anter OF mmenu !SEEK(vusucla + '0037', 'IteUsuOp')
SET SKIP OF PAD proxi OF mmenu !SEEK(vusucla + '0038', 'IteUsuOp')
SET SKIP OF PAD corri OF mmenu !SEEK(vusucla + '0039', 'IteUsuOp')
SET SKIP OF PAD ingre OF mmenu !SEEK(vusucla + '0040', 'IteUsuOp')
SET SKIP OF PAD anula OF mmenu !SEEK(vusucla + '0041', 'IteUsuOp')
SET SKIP OF PAD lista OF mmenu !SEEK(vusucla + '0042', 'IteUsuOp')
USE IN iteusuop
SELECT (calias)
RETURN
*

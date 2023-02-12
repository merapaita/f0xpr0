CLOSE DATABASES
USE IN 1 FOrdCom ALIAS ordcom ORDER FOrdCom1
USE IN 2 FIteOC ALIAS iteoc ORDER FIteOC1
USE IN 3 ArtMae ALIAS artmae  ORDER ArtMae1
USE IN 4 IteArt ALIAS iteart  ORDER IteArt2
USE IN 5 KardexV ALIAS kardex ORDER Kardexv1
USE IN 6 StkAlmV ALIAS stkalmv ORDER StkAlmv1
USE IN 7 Parmae ALIAS parma ORDER Parmae1
USE IN 8 Promae ALIAS promae ORDER ProMae1
vmens01 = 'Registro de Ordenes de Compra de Famacia'
vmens02 = 'Ordenes de Compra de Famacia : REVISION '
vmens04 = 'Dicha Orden no fue encontrada'
vmens05 = 'No existe Orden de Compra anterior'
vmens06 = 'No existe Orden de Compra siguiente'
vmens07 = '¨ Desea Anular esta Orden de Compra ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Orden de Compra ha sido anulada'
vmens10 = 'Edici¢n'
SELECT ordcom
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
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anula  Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00 TO 11, 79 TITLE vmens01  ;
       FOOTER '[F9] Detalle : Item' DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 12, 00 TO 23, 79 TITLE  ;
       'Detalle: Orden de Compra de farmacia'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 00, 00 TO 23, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis OF mmenu PROMPT '\<Revisa' AT 24, 00
DEFINE PAD busca OF mmenu PROMPT '\<Busca' AT 24, 08
DEFINE PAD anter OF mmenu PROMPT '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu PROMPT '\<Siguiente' AT 24, 25
DEFINE PAD corri OF mmenu PROMPT '\<Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT '\<Ingresa' AT 24, 45
DEFINE PAD anula OF mmenu PROMPT ' \<Anula ' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT '\<Listar' AT 24, 63
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
ACTIVATE WINDOW wind_0
CLEAR
@ 0, 59 SAY 'Estado :'
@ 1, 2 SAY ' Orden de Compra :'
@ 1, 50 SAY ' Fecha O/C :'
@ 2, 2 SAY ' Doc. referencia :'
@ 3, 2 SAY ' Num. Referencia :'
@ 3, 50 SAY 'Fecha Ref. :'
@ 4, 2 SAY '       Proveedor :'
@ 5, 2 SAY '         Factura :'
@ 6, 2 SAY ' Boleta de Venta :'
@ 7, 2 SAY '   Observaciones :'
@ 8, 2 SAY '           Total :'
RETURN
*
PROCEDURE vista
SELECT ordcom
ON KEY LABEL F9 DO VISTA_DET
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 0, 68 SAY m.estado
@ 1, 22 SAY m.periodo
@ 1, 25 SAY m.numoc
@ 1, 65 SAY m.fecoc
@ 2, 22 SAY m.tipdoc
@ 3, 22 SAY m.numref
@ 3, 65 SAY m.fecref
@ 4, 22 SAY val_prv(m.codprv)
@ 5, 22 SAY m.factura
@ 6, 22 SAY m.bolvta
@ 7, 22 SAY LEFT(m.observa, 55)
@ 8, 22 SAY m.totoc
DO vista_hijo
IF !vflag $ 'J*'
     DO subopc
ENDIF
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT iteoc
GOTO TOP
SEEK m.periodo + m.numoc
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            item :H = 'ITEM',  ;
            codart :H = 'C¢digo',  ;
            desart :H =  ;
            'Descripci¢n' : 40,  ;
            totcan :H =  ;
            'Cantidad' :P =  ;
            '9,999', precom :H =  ;
            'Total' :P =  ;
            '9,999.999', cantidad  ;
            :H = 'Cantidad',  ;
            fraccion :H =  ;
            'Fracci¢n', preuni :H =  ;
            'P.U.', precom :H =  ;
            'Total' NOMENU  ;
            NOAPPEND NODELETE  ;
            NOCLEAR WINDOW wind_1  ;
            KEY m.periodo +  ;
            m.numoc TIMEOUT  ;
            0.0001  NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 4, 25 SAY  ;
       'No hay Detalles de este Documento'
     ACTIVATE WINDOW wind_0
ENDIF
SELECT ordcom
RETURN
*
PROCEDURE vista_det
SELECT iteoc
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
GOTO TOP
SEEK m.periodo + m.numoc
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            item :H = 'ITEM',  ;
            codart :H = 'C¢digo',  ;
            desart :H =  ;
            'Descripci¢n' : 40,  ;
            totcan :H =  ;
            'Cantidad' :P =  ;
            '9,999', precom :H =  ;
            'Total' :P =  ;
            '9,999.999', cantidad  ;
            :H = 'Cantidad',  ;
            fraccion :H =  ;
            'Fracci¢n', preuni :H =  ;
            'P.U.', precom :H =  ;
            'Total' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_1 KEY  ;
            m.periodo + m.numoc  ;
            TITLE vtempo  ;
            NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 4, 25 SAY  ;
       'No hay Detalles de este Ingreso'
ENDIF
ON KEY LABEL F9 DO VISTA_DET
SHOW MENU mmenu
SELECT ordcom
DO vista
RETURN
*
PROCEDURE revis
SELECT ordcom
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SET RELATION TO periodo + numoc INTO iteoc
SET SKIP TO iteoc
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE FIELDS periodo :H =  ;
       'Periodo', numoc :H =  ;
       'O/C', iteoc.item :H =  ;
       'ITEM', iteoc.codart :H =  ;
       'C¢digo', iteoc.desart :H =  ;
       'Descripci¢n',  ;
       iteoc.cantidad :H =  ;
       'Cantidad', iteoc.fraccion  ;
       :H = 'Fracci¢n',  ;
       iteoc.preuni :H = 'P.U.',  ;
       iteoc.precom :H = 'Total'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW wind_2
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELECT ordcom
SET RELATION TO
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = RIGHT(DTOC(m.fecsis),  ;
           2)
vnum_oc = '    '
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero O/C : ' GET  ;
  vperiodo PICTURE '!!'
@ 1, 26 SAY '-' GET vnum_oc  ;
  PICTURE '!!!!' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_oc) OR LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vperiodo + vnum_oc
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
vnum_oc = PADL(ALLTRIM(vnum_oc),  ;
          4, '0')
RETURN .T.
*
PROCEDURE anter
SELECT ordcom
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
SELECT ordcom
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
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado = '99'
     DO standby WITH  ;
        'El Ducumento esta Anulado'
     RETURN
ENDIF
SELECT kardex
GOTO TOP
IF estado = '20'
     DO standby WITH  ;
        'El Periodo ya esta Cerrado no se pueden hacer modificaciones'
     SELECT ordcom
     DO vista
     RETURN
ENDIF
SELECT parma
SEEK 'UTILID01'
IF FOUND()
     putil = numdec
ELSE
     DO standby WITH  ;
        'El Parametro de Utilidad de Ventas en Farmacia no Existe. por favor consulte al area de Sistemas'
     DO vista
     RETURN
ENDIF
SELECT parma
SEEK 'UTILID02'
IF FOUND()
     putil2 = numdec
ELSE
     DO standby WITH  ;
        'El Parametro de Utilidad de Ventas en Farmacia no Existe. por favor consulte al area de Sistemas'
     DO vista
     RETURN
ENDIF
SELECT ordcom
SCATTER MEMVAR
DO pantalla
@ 0, 68 GET m.estado DISABLE
@ 1, 22 GET m.periodo DISABLE
@ 1, 25 GET m.numoc DISABLE
@ 1, 65 GET m.fecoc WHEN .F.
@ 2, 22 GET m.tipdoc VALID  ;
  val_para(m.tipdoc,'TIPDOC',' ', ;
  22,20)
@ 3, 22 GET m.numref
@ 3, 65 GET m.fecref
@ 4, 22 GET m.codprv VALID  ;
  val_prv(m.codprv,.T.,04,27)
@ 5, 22 GET m.factura
@ 6, 22 GET m.bolvta
@ 7, 22 GET m.observa FUNCTION  ;
  'S50'
@ 8, 22 GET m.totoc WHEN .F.
READ VALID val_read()
IF LASTKEY() <> 27
     ok = trabaja_hi()
     IF ok AND LASTKEY() <> 27
          SELECT ordcom
          IF f_lock(1)
               m.tuser = 'C'
               m.usercr = vuser_id
               m.dusercr = DATE()
               m.husercr = TIME()
               GATHER MEMVAR
          ENDIF
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT ordcom
DO vista
RETURN
*
PROCEDURE ingre
SELECT stkalmv
GOTO TOP
IF EOF()
     DO standby WITH  ;
        'No existe Inventario Inicial en el sistema. por favor revise'
     SELECT ordcom
     DO vista
     RETURN
ENDIF
SELECT kardex
GOTO TOP
IF estado = '20'
     DO standby WITH  ;
        'El Periodo ya esta Cerrado no se pueden hacer modificaciones'
     SELECT ordcom
     DO vista
     RETURN
ENDIF
ON KEY LABEL F9
SELECT ordcom
nreg = IIF(!EOF(), RECNO(), - ;
       1)
DO pantalla
SCATTER BLANK MEMVAR
m.estado = '00'
m.periodo = RIGHT(STR(YEAR(m.fecsis),  ;
            4), 2)
m.fecoc = m.fecsis
m.fecref = m.fecsis
SELECT parma
SEEK 'CORRELFORDC '
IF FOUND()
     m.numoc = PADL(ALLTRIM(STR(nument +  ;
               1)), 4, '0')
     IF SEEK(m.periodo + m.numoc,  ;
        'OrdCom')
          DO standby WITH  ;
             'El Correlativo Generado ya Existe.'
          SELECT ordcom
          DO vista
          RETURN
     ENDIF
ELSE
     DO standby WITH  ;
        'El Parametro de Correlativos no existe. por favor consulte al area de Sistemas'
     DO vista
     RETURN
ENDIF
SELECT parma
SEEK 'UTILID01'
IF FOUND()
     putil = numdec
ELSE
     DO standby WITH  ;
        'El Parametro de Utilidad de Ventas en Farmacia no Existe. por favor consulte al area de Sistemas'
     DO vista
     RETURN
ENDIF
SELECT parma
SEEK 'UTILID02'
IF FOUND()
     putil2 = numdec
ELSE
     DO standby WITH  ;
        'El Parametro de Utilidad de Ventas en Farmacia no Existe. por favor consulte al area de Sistemas'
     DO vista
     RETURN
ENDIF
SELECT ordcom
@ 0, 68 GET m.estado DISABLE
@ 1, 22 GET m.periodo DISABLE
@ 1, 25 GET m.numoc DISABLE
@ 1, 65 GET m.fecoc WHEN .F.
@ 2, 22 GET m.tipdoc VALID  ;
  val_para(m.tipdoc,'TIPDOC',' ', ;
  22,20)
@ 3, 22 GET m.numref
@ 3, 65 GET m.fecref
@ 4, 22 GET m.codprv VALID  ;
  val_prv(m.codprv,.T.,04,27)
@ 5, 22 GET m.factura
@ 6, 22 GET m.bolvta
@ 7, 22 GET m.observa FUNCTION  ;
  'S50'
@ 8, 22 GET m.totoc WHEN .F.
READ VALID val_read()
IF LASTKEY() <> 27
     ok = trabaja_hi()
     IF ok AND LASTKEY() <> 27
          SELECT ordcom
          IF f_appd()
               m.tuser = 'I'
               m.user = vuser_id
               m.duser = DATE()
               m.huser = TIME()
               GATHER MEMVAR
               SELECT parma
               SEEK 'CORRELFORDC '
               IF FOUND()
                    IF f_lock(1)
                         REPLACE nument WITH nument + 1
                    ENDIF
               ENDIF
          ENDIF
     ELSE
          IF nreg <> -1
               GOTO nreg
          ENDIF
          SELECT iteoc
          IF SEEK(m.periodo +  ;
             m.numoc)
               DELETE WHILE  ;
                      periodo =  ;
                      m.periodo  ;
                      AND numoc =  ;
                      m.numoc
          ENDIF
          SELECT kardex
          SET ORDER TO KardexV2
          DO WHILE SEEK(m.periodo+ ;
             'OCF'+m.numoc)
               cperiodo = periodo
               ccodart = codart
               ccorrel = correl
               DELETE NEXT 1
               DO recalpr WITH  ;
                  cperiodo,  ;
                  ccodart,  ;
                  ccorrel
          ENDDO
          SET ORDER TO KardexV1
          DO standby WITH  ;
             'Proceso cancelado'
     ENDIF
ELSE
     IF nreg <> -1
          GOTO nreg
     ENDIF
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT ordcom
DO vista
RETURN
*
FUNCTION trabaja_hi
vsun = .T.
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°F2->Edita°°°°°°°°°F5->Agregar°°°°°°°°°F8->Eliminar°°°°°°°°F10->Terminar°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F2 DO Edita_item
ON KEY LABEL F5 DO agrega_item
ON KEY LABEL F8 DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteoc
SET ORDER TO FiteOC1
SEEK m.periodo + m.numoc
IF !FOUND()
     DO agrega_ite
ENDIF
BROWSE NOOPTIMIZE FIELDS item :H =  ;
       'Item', codart :H =  ;
       'Codigo', desart :H =  ;
       'Descripci¢n' : 40, totcan  ;
       :H = 'Cantidad' :P =  ;
       '9,999', precom :H =  ;
       'Total' :P = '9,999.999',  ;
       lfracc :H = 'Frac.',  ;
       unidad :H = 'Unidad',  ;
       unifrac :H = 'U.fracc.',  ;
       cantidad :H = 'Cantidad',  ;
       fraccion :H = 'Fraccion',  ;
       preuni :H = 'P.U.', valuni  ;
       :H = 'V.U.', valunifr :H =  ;
       'V.U.Fr.', valcom :H =  ;
       'Val. Tot.', precom :H =  ;
       'Pre. Tot.' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       NOCLEAR WINDOW wind_1 KEY  ;
       m.periodo + m.numoc WHEN  ;
       .F. NOREFRESH
m.totoc = 0
SEEK m.periodo + m.numoc
IF FOUND()
     SCAN WHILE periodo + numoc =  ;
          m.periodo + m.numoc
          m.totoc = m.totoc +  ;
                    precom
     ENDSCAN
ENDIF
IF LASTKEY() <> 27
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
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SELECT ordcom
RETURN vsun
*
PROCEDURE agrega_ite
PRIVATE m.tuser, m.user, m.huser,  ;
        m.duser
lcant = .F.
lfrac = .F.
DEFINE WINDOW wind_3 FROM 01, 04  ;
       TO 21, 71 TITLE  ;
       'Adici¢n de Medicamentos'  ;
       DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW wind_3
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SELECT iteoc
IF SEEK(m.periodo + m.numoc)
     SCAN WHILE periodo + numoc =  ;
          m.periodo + m.numoc
          m.item = item
     ENDSCAN
     m.item = PADL(ALLTRIM(STR(VAL(m.item) +  ;
              1)), 4, '0')
ELSE
     m.item = '0001'
ENDIF
m.codart = SPACE(12)
m.desart = SPACE(60)
m.lmenart = SPACE(1)
m.lfracc = SPACE(1)
m.unidad = SPACE(10)
m.unifrac = SPACE(10)
m.cantidad = 0
m.cantidadx = SPACE(10)
m.fraccion = 0
m.fracart = 0
m.totcan = 0
m.preuni = 0
m.igv = 0
m.valuni = 0
m.valunifr = 0
m.valcom = 0
m.precom = 0
m.porvta = putil
m.porvta2 = putil2
m.preven = 0
m.prevenfr = 0
m.preven2 = 0
m.preven2fr = 0
m.tuser = 'I'
m.user = vuser_id
m.huser = TIME()
m.duser = DATE()
@ 0, 1 SAY '             Item: ' +  ;
  m.item
@ 1, 1 SAY '       Num. Orden: ' +  ;
  m.periodo + '.' + m.numoc
@ 2, 1 SAY '           Codigo:'  ;
  GET m.codart VALID  ;
  val_art(m.codart) AND  ;
  vcodlab()
@ 3, 1 SAY '      Laboratorio: '
@ 4, 1 SAY '         Medicina:'  ;
  GET m.desart FUNCTION 'S40'  ;
  WHEN .F.
@ 5, 1 SAY '    Menudeo Art.?:'  ;
  GET m.lmenart WHEN .F.
@ 6, 1 SAY '    Unidad (Lote):'  ;
  GET m.unidad WHEN .F.
@ 7, 1 SAY '  Unidad Fraccion:'  ;
  GET m.unifrac WHEN .F.
@ 8, 1 SAY '  Fracc. Articulo:'  ;
  GET m.fracart WHEN .F.
@ 6, 35 SAY '       Ingreso:' GET  ;
  m.cantidadx VALID val_can()
@ 8, 35 SAY '      Cantidad:' GET  ;
  m.cantidad WHEN .F.
@ 9, 35 SAY '      Fraccion:' GET  ;
  m.fraccion WHEN .F.
@ 10, 35 SAY 'Total Cantidad:'  ;
  GET m.totcan WHEN .F.
@ 12, 1 SAY '     Precio Total:'  ;
  GET m.precom PICTURE  ;
  '999,999.99' VALID  ;
  val_pu(m.codart)
@ 13, 1 SAY '      P.U. (Lote):'  ;
  GET m.preuni PICTURE  ;
  '999,999.99' WHEN .F.
@ 14, 1 SAY '          I.G.V. :'  ;
  GET m.igv PICTURE '999,999.99'  ;
  WHEN .F.
@ 12, 35 SAY '    Valor Total:'  ;
  GET m.valcom PICTURE  ;
  '999,999.99' WHEN .F.
@ 13, 35 SAY 'Valor U. (Lote):'  ;
  GET m.valuni PICTURE  ;
  '999,999.99' WHEN .F.
@ 14, 35 SAY ' Val. U. Fracc.:'  ;
  GET m.valunifr PICTURE  ;
  '999,999.99' WHEN .F.
@ 16, 09 SAY ' % Vta.(Lote)'
@ 16, 27 SAY 'Prec. Vta.(Lote)'
@ 16, 46 SAY 'Prec. Vta. Fr'
@ 17, 11 SAY '1'
@ 17, 13 GET m.porvta PICTURE  ;
  '999.99'
@ 17, 20 SAY '%'
@ 17, 30 GET m.preven PICTURE  ;
  '999,999.99' WHEN val_pv()
@ 17, 47 GET m.prevenfr PICTURE  ;
  '999,999.99' WHEN  ;
  val_pvfr(m.codart)
@ 18, 11 SAY '2'
@ 18, 13 GET m.porvta2 PICTURE  ;
  '999.99'
@ 18, 20 SAY '%'
@ 18, 30 GET m.preven2 PICTURE  ;
  '999,999.99' WHEN val_pv2()
@ 18, 47 GET m.preven2fr PICTURE  ;
  '999,999.99' WHEN  ;
  val_pv2fr(m.codart)
READ VALID val_read() AND  ;
     valing()
IF LASTKEY() <> 27
     ak = actkarpr('OCF',m.numoc, ;
          m.fecoc)
     IF ak
          IF f_appd()
               GATHER MEMVAR
          ENDIF
     ENDIF
ELSE
     DO standby WITH  ;
        'Cancela Informacion'
ENDIF
DEACTIVATE WINDOW wind_3
RELEASE WINDOW wind_3
ON KEY LABEL F2 DO Edita_item
ON KEY LABEL F5 DO agrega_item
ON KEY LABEL F8 DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN
*
FUNCTION vcodlab
xlab = 'B' + LEFT(m.codart, 6)
= val_fun('ArtMae',xlab,'Descri', ;
  xlab,1,3,20,'Descri')
RETURN .T.
*
PROCEDURE val_fracc
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
     WAIT WINDOW 'KK'
     DO standby WITH  ;
        'Error en catalogo de Articulos'
ENDIF
SHOW GET m.cantidad
SHOW GET m.fraccion
SET ORDER TO (cord)
SELECT (cali)
RETURN
*
FUNCTION valing
PRIVATE mret
mret = .T.
IF LASTKEY() <> 27
     DO CASE
          CASE m.totcan = 0
               DO standby WITH  ;
                  'La cantidad Ingrasada no debe Ser 0'
               mret = .F.
          CASE m.precom = 0
               DO standby WITH  ;
                  'El Precio de compra Ingresado no debe Ser 0'
               mret = .F.
          CASE m.preven = 0
               DO standby WITH  ;
                  'El Precio de Venta al Publico no debe Ser 0'
               mret = .F.
          CASE m.prevenfr = 0
               DO standby WITH  ;
                  'El Precio de Venta Fraccionado no debe Ser 0'
               mret = .F.
          OTHERWISE
               mret = .T.
     ENDCASE
ENDIF
RETURN mret
*
PROCEDURE val_igv
m.igv = ROUND(m.preuni - m.preuni /  ;
        1.19 , 2)
m.valuni = ROUND(m.preuni - m.igv,  ;
           2)
SHOW GET m.valuni
SHOW GET m.igv
RETURN
*
PROCEDURE val_pu
PARAMETER cart
PRIVATE cali, cord
m.preuni = ROUND(m.precom /  ;
           m.totcan, 2)
cali = ALIAS()
SELECT iteart
cord = ORDER()
SET ORDER TO IteArt1
IF SEEK('B' + cart)
     m.preuni = ROUND(m.preuni *  ;
                iteart.fraccion,  ;
                2)
     SHOW GET m.preuni
ELSE
     DO standby WITH  ;
        'Error en catalogo de Articulos'
ENDIF
SET ORDER TO (cord)
SELECT (cali)
DO val_igv
DO val_pre WITH cart
RETURN
*
PROCEDURE val_pre
PARAMETER cart
PRIVATE cali, cord
cali = ALIAS()
SELECT iteart
cord = ORDER()
SET ORDER TO IteArt1
IF SEEK('B' + cart)
     m.valunifr = ROUND(m.valuni /  ;
                  iteart.fraccion,  ;
                  2)
     m.valcom = ROUND(m.valunifr *  ;
                m.totcan, 2)
     SHOW GET m.valunifr
     SHOW GET m.valcom
ELSE
     DO standby WITH  ;
        'Error en catalogo de Articulos'
ENDIF
SET ORDER TO (cord)
SELECT (cali)
RETURN
*
PROCEDURE val_pre1
PARAMETER cart
PRIVATE cali, cord
cali = ALIAS()
SELECT iteart
cord = ORDER()
SET ORDER TO IteArt1
IF SEEK('B' + cart)
     m.valcom = m.valunifr *  ;
                m.totcan
     m.precom = (m.preuni /  ;
                iteart.fraccion) *  ;
                m.totcan
     SHOW GET m.valcom
     SHOW GET m.precom
ELSE
     DO standby WITH  ;
        'Error en catalogo de Articulos'
ENDIF
SET ORDER TO (cord)
SELECT (cali)
RETURN
*
FUNCTION val_can
PRIVATE mret
mret = .T.
IF m.lmenart = 'S'
     IF LEN(m.cantidadx) > 0
          IF VAL(m.cantidadx) = 0
               IF SUBSTR(m.cantidadx,  ;
                  1, 1) = 'F'
                    m.lfracc = 'S'
               ELSE
                    mret = .F.
               ENDIF
               IF TYPE(SUBSTR(m.cantidadx,  ;
                  2)) = 'N'
                    m.fraccion = VAL(SUBSTR(m.cantidadx,  ;
                                 2))
               ELSE
                    mret = .F.
               ENDIF
               IF mret
                    m.cantidad = INT(m.fraccion /  ;
                                 m.fracart)
                    m.totcan = m.fraccion
               ENDIF
          ELSE
               m.lfracc = 'N'
               m.cantidad = VAL(m.cantidadx)
               m.fraccion = m.fracart
               m.totcan = m.fraccion *  ;
                          m.cantidad
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
          m.totcan = m.fraccion *  ;
                     m.cantidad
     ENDIF
ENDIF
IF mret
     SHOW GET m.cantidad
     SHOW GET m.fraccion
     SHOW GET m.totcan
ELSE
     DO standby WITH  ;
        'Error en cantidad ingresada'
ENDIF
RETURN mret
m.totcan = m.fraccion *  ;
           m.cantidad
SHOW GET m.totcan
RETURN
*
PROCEDURE edita_item
PRIVATE m.tuser, m.usercr,  ;
        m.husercr, m.dusercr
lcant = .F.
lfrac = .F.
DEFINE WINDOW wind_3 FROM 01, 04  ;
       TO 21, 71 TITLE  ;
       'Edici¢n de Articulos'  ;
       DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW wind_3
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SELECT iteoc
SCATTER MEMVAR
m.tuser = 'C'
m.usercr = vuser_id
m.husercr = TIME()
m.dusercr = DATE()
m.lmenart = ''
m.fracart = 0
= val_art(m.codart)
@ 0, 1 SAY '             Item: ' +  ;
  m.item
@ 1, 1 SAY '       Num. Orden: ' +  ;
  m.periodo + '.' + m.numoc
@ 2, 1 SAY '           Codigo:'  ;
  GET m.codart WHEN vcodlab()  ;
  AND .F.
@ 3, 1 SAY '      Laboratorio: '
@ 4, 1 SAY '         Medicina:'  ;
  GET m.desart FUNCTION 'S40'  ;
  WHEN .F.
@ 5, 1 SAY '    Menudeo Art.?:'  ;
  GET m.lmenart WHEN .F.
@ 6, 1 SAY '    Unidad (Lote):'  ;
  GET m.unidad WHEN .F.
@ 7, 1 SAY '  Unidad Fraccion:'  ;
  GET m.unifrac WHEN .F.
@ 8, 1 SAY '  Fracc. Articulo:'  ;
  GET m.fracart WHEN .F.
@ 6, 35 SAY '       Ingreso:' GET  ;
  m.cantidadx VALID val_can()
@ 8, 35 SAY '      Cantidad:' GET  ;
  m.cantidad WHEN .F.
@ 9, 35 SAY '      Fraccion:' GET  ;
  m.fraccion WHEN .F.
@ 10, 35 SAY 'Total Cantidad:'  ;
  GET m.totcan WHEN .F.
@ 12, 1 SAY '     Precio Total:'  ;
  GET m.precom PICTURE  ;
  '999,999.99' WHEN .F.
@ 13, 1 SAY '      P.U. (Lote):'  ;
  GET m.preuni PICTURE  ;
  '999,999.99' WHEN .F.
@ 14, 1 SAY '          I.G.V. :'  ;
  GET m.igv PICTURE '999,999.99'  ;
  WHEN .F.
@ 12, 35 SAY '    Valor Total:'  ;
  GET m.valcom PICTURE  ;
  '999,999.99' WHEN .F.
@ 13, 35 SAY 'Valor U. (Lote):'  ;
  GET m.valuni PICTURE  ;
  '999,999.99' WHEN .F.
@ 14, 35 SAY ' Val. U. Fracc.:'  ;
  GET m.valunifr PICTURE  ;
  '999,999.99' WHEN .F.
@ 16, 09 SAY ' % Vta.(Lote)'
@ 16, 27 SAY 'Prec. Vta.(Lote)'
@ 16, 46 SAY 'Prec. Vta. Fr'
@ 17, 11 SAY '1'
@ 17, 13 GET m.porvta PICTURE  ;
  '999.99'
@ 17, 20 SAY '%'
@ 17, 30 GET m.preven PICTURE  ;
  '999,999.99' WHEN val_pv()
@ 17, 47 GET m.prevenfr PICTURE  ;
  '999,999.99' WHEN  ;
  val_pvfr(m.codart)
@ 18, 11 SAY '2'
@ 18, 13 GET m.porvta2 PICTURE  ;
  '999.99'
@ 18, 20 SAY '%'
@ 18, 30 GET m.preven2 PICTURE  ;
  '999,999.99' WHEN val_pv2()
@ 18, 47 GET m.preven2fr PICTURE  ;
  '999,999.99' WHEN  ;
  val_pv2fr(m.codart)
READ VALID val_read() AND  ;
     valing()
IF LASTKEY() <> 27
     ak = actkarpr('OCF',numoc, ;
          m.fecoc)
     IF ak
          IF f_lock(1)
               GATHER MEMVAR
          ENDIF
     ENDIF
ELSE
     DO standby WITH  ;
        'Cancela Informacion'
ENDIF
DEACTIVATE WINDOW wind_3
RELEASE WINDOW wind_3
ON KEY LABEL F2 DO Edita_item
ON KEY LABEL F5 DO agrega_item
ON KEY LABEL F8 DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN
*
PROCEDURE val_pv
m.preven = ROUND(m.preuni * (1 +  ;
           m.porvta), 2)
SHOW GET m.preven
RETURN
*
PROCEDURE val_pv2
m.preven2 = ROUND(m.preuni * (1 +  ;
            m.porvta2), 2)
SHOW GET m.preven2
RETURN
*
PROCEDURE val_pvfr
PARAMETER cart
PRIVATE cali, cord
cali = ALIAS()
SELECT iteart
cord = ORDER()
SET ORDER TO IteArt1
IF SEEK('B' + cart)
     m.prevenfr = ROUND(m.preven /  ;
                  iteart.fraccion,  ;
                  2)
     SHOW GET m.prevenfr
ELSE
     DO standby WITH  ;
        'Error en catalogo de Articulos'
ENDIF
SET ORDER TO (cord)
SELECT (cali)
RETURN
*
PROCEDURE val_pv2fr
PARAMETER cart
PRIVATE cali, cord
cali = ALIAS()
SELECT iteart
cord = ORDER()
SET ORDER TO IteArt1
IF SEEK('B' + cart)
     m.preven2fr = ROUND(m.preven2 /  ;
                   iteart.fraccion,  ;
                   2)
     SHOW GET m.preven2fr
ELSE
     DO standby WITH  ;
        'Error en catalogo de Articulos'
ENDIF
SET ORDER TO (cord)
SELECT (cali)
RETURN
*
PROCEDURE elimi_item
SELECT ordcom
IF yesno( ;
   'Estas seguro de Eliminar este Item' ;
   )
     SELECT kardex
     SET ORDER TO KardexV2
     IF SEEK(iteoc.periodo +  ;
        'OCF' + iteoc.numoc +  ;
        iteoc.item +  ;
        iteoc.codart)
          IF RLOCK()
               cperiodo = periodo
               ccodart = codart
               ccorrel = correl
               DELETE NEXT 1
               DO recalpr WITH  ;
                  cperiodo,  ;
                  ccodart,  ;
                  ccorrel
          ENDIF
     ENDIF
     SELECT iteoc
     IF RLOCK()
          DELETE NEXT 1
     ELSE
          DO standby WITH  ;
             'No puede eliminar este Item.'
     ENDIF
ENDIF
UNLOCK
RETURN
*
PROCEDURE lista
DO standby WITH  ;
   'Opci¢n todavia en proceso. Crear reporte si es necesario'
RETURN
SELECT ordcom
SCATTER MEMVAR
vdbf = SYS(3) + '.Dbf'
COPY TO (vdbf) STRUCTURE
USE IN 0 (vdbf) ALIAS xord
SELECT xord
APPEND BLANK
GATHER MEMVAR
SET RELATION TO periodo + numoc INTO iteoc;
ADDITIVE
SET SKIP TO iteoc
DO reporte WITH 2, 'FOrdCom', 'Guia de Internamiento', 2, .F., .T.
USE IN xord
ERASE (vdbf)
SELECT ordcom
RETURN
*
PROCEDURE anula
DO standby WITH  ;
   'Opci¢n todavia en proceso. Crear reporte si es necesario'
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
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
PRIVATE calias
calias = ALIAS()
USE IN 0 IteUsuOp ORDER IteUsuOp2
SET SKIP OF PAD revis OF mmenu;
!SEEK(vusucla + '0019', 'IteUsuOp')
SET SKIP OF PAD busca OF mmenu;
!SEEK(vusucla + '0020', 'IteUsuOp')
SET SKIP OF PAD anter OF mmenu;
!SEEK(vusucla + '0021', 'IteUsuOp')
SET SKIP OF PAD proxi OF mmenu;
!SEEK(vusucla + '0022', 'IteUsuOp')
SET SKIP OF PAD corri OF mmenu;
!SEEK(vusucla + '0023', 'IteUsuOp')
SET SKIP OF PAD ingre OF mmenu;
!SEEK(vusucla + '0024', 'IteUsuOp')
SET SKIP OF PAD anula OF mmenu;
!SEEK(vusucla + '0025', 'IteUsuOp')
SET SKIP OF PAD lista OF mmenu;
!SEEK(vusucla + '0026', 'IteUsuOp')
USE IN iteusuop
SELECT (calias)
RETURN
*

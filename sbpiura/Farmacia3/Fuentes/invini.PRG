CLOSE DATABASES
USE IN 1 InvIni ALIAS invini  ;
    ORDER InvIni1
USE IN 2 IteII ALIAS iteii ORDER  ;
    IteII1
USE IN 3 ArtMae ALIAS artmae  ;
    ORDER ArtMae1
USE IN 4 IteArt ALIAS iteart  ;
    ORDER IteArt2
USE IN 5 KardexV ALIAS kardex  ;
    ORDER Kardexv1
USE IN 6 StkAlmV ALIAS stkalmv  ;
    ORDER StkAlmv1
USE IN 7 Parmae ALIAS parma ORDER  ;
    Parmae1
vmens01 = 'Registro de Inventario Inicial'
vmens02 = 'Inventario Inicial : REVISION '
vmens04 = 'Dicho Inventario no fue encontrado'
vmens05 = 'No existe Inventario anterior'
vmens06 = 'No existe Inventario siguiente'
vmens07 = '¨ Desea Anular ‚sta Inventario ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Inventario ha sido anulada'
vmens10 = 'Edici¢n'
SELECT invini
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
vtempo = ' Revisa         Anterior  Siguiente  Corrige  Ingresa           Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 11, 79 TITLE vmens01  ;
       FOOTER  ;
       '[F9] Detalle : Item'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle: Inventario'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 00, 00  ;
       TO 23, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis OF mmenu PROMPT  ;
       '\<Revisa' AT 24, 00
DEFINE PAD anter OF mmenu PROMPT  ;
       '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu PROMPT  ;
       '\<Siguiente' AT 24, 25
DEFINE PAD corri OF mmenu PROMPT  ;
       '\<Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT  ;
       '\<Ingresa' AT 24, 45
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Listar ' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO corri
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_0
CLEAR
@ 1, 59 SAY 'Estado :'
@ 2, 2 SAY '         Periodo :'
@ 3, 2 SAY ' Num. Inventario :'
@ 4, 2 SAY '   Fecha Ingreso :'
@ 5, 2 SAY ' Doc. referencia :'
@ 6, 2 SAY ' Num. Referencia :'
@ 7, 2 SAY 'Fecha Referencia :'
@ 8, 2 SAY '   Observaciones :'
RETURN
*
PROCEDURE vista
SELECT invini
ON KEY LABEL F9 DO VISTA_DET
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 1, 68 SAY m.estado
@ 2, 22 SAY m.periodo
@ 3, 22 SAY m.numii
@ 4, 22 SAY m.fecii
@ 5, 22 SAY m.docref
@ 6, 22 SAY m.numref
@ 7, 22 SAY m.fecref
@ 8, 22 SAY m.observ
DO vista_hijo
IF  .NOT. vflag $ 'J*'
     DO subopc
ENDIF
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT iteii
GOTO TOP
SEEK m.periodo + m.numii
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            item :H = 'ITEM',  ;
            desart :H =  ;
            'Descripci¢n' : 40,  ;
            totcan :H =  ;
            'Cantidad' :P =  ;
            '9,999', preuni :H =  ;
            'P.U.+IGV', precom :H =  ;
            'Total' :P =  ;
            '9,999.999', cantidad  ;
            :H = 'Cantidad',  ;
            fraccion :H =  ;
            'Fracci¢n', valcom :H =  ;
            'Valor' NOMENU  ;
            NOAPPEND NODELETE  ;
            NOCLEAR WINDOW wind_1  ;
            KEY m.periodo +  ;
            m.numii TIMEOUT  ;
            0.0001  NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 4, 25 SAY  ;
       'No hay Detalles de este Inventario'
     ACTIVATE WINDOW wind_0
ENDIF
SELECT invini
RETURN
*
PROCEDURE vista_det
SELECT iteii
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
GOTO TOP
SEEK m.periodo + m.numii
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            item :H = 'ITEM',  ;
            desart :H =  ;
            'Descripci¢n' : 40,  ;
            totcan :H =  ;
            'Cantidad' :P =  ;
            '9,999', preuni :H =  ;
            'P.U.+IGV', precom :H =  ;
            'Total' :P =  ;
            '9,999.999', totcan  ;
            :H = 'Cantidad' :P =  ;
            '9,999', cantidad :H =  ;
            'Cantidad', fraccion  ;
            :H = 'Fracci¢n',  ;
            valcom :H = 'Valor'  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_1  ;
            KEY m.periodo +  ;
            m.numii TITLE vtempo  ;
            NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 4, 25 SAY  ;
       'No hay Detalles de este Ingreso'
ENDIF
ON KEY LABEL F9 DO VISTA_DET
SHOW MENU mmenu
SELECT invini
DO vista
RETURN
*
FUNCTION revis
SELECT invini
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SET RELATION TO periodo + numii INTO iteii
SET SKIP TO iteii
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE FIELDS periodo :H =  ;
       'Periodo', numii :H =  ;
       'Inv.', iteii.item :H =  ;
       'ITEM', iteii.codart :H =  ;
       'C¢digo', iteii.desart :H =  ;
       'Descripci¢n',  ;
       iteii.cantidad :H =  ;
       'Cantidad', iteii.fraccion  ;
       :H = 'Fracci¢n',  ;
       iteii.preuni :H =  ;
       'P.U.+IGV', iteii.valcom  ;
       :H = 'Valor', iteii.precom  ;
       :H = 'Precio' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_2
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELECT invini
SET RELATION TO
DO vista
RETURN
RETURN .T.
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vbusca = SPACE(20)
vnombre = 'Codigo Medicamento:'
IF LASTKEY() <> 27
     DEFINE WINDOW lista FROM 09,  ;
            12 TO 16, 68 FLOAT  ;
            TITLE  ;
            ' °° B£squeda °° '  ;
            DOUBLE COLOR SCHEME  ;
            5
     ACTIVATE WINDOW lista
     @ 3, 2 SAY vnombre GET  ;
       vbusca
     READ VALID val_read()
     DEACTIVATE WINDOW lista
ENDIF
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
ELSE
     SEEK ALLTRIM(vbusca)
     IF  .NOT. FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
RETURN
*
PROCEDURE anter
SELECT invini
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  .NOT. BOF()
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
SELECT invini
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  .NOT. EOF()
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
SELECT kardex
GOTO TOP
IF estado = '20'
     DO standby WITH  ;
        'El periodo ya esta cerrado no se puede hacer modificaciones'
     SELECT invini
     DO vista
     RETURN
ENDIF
SELECT parma
SEEK 'UTILID01'
IF FOUND()
     putil = numdec
ELSE
     DO standby WITH  ;
        'El Parametro de Utilidad de Ventas en Farmacia no Existe. por favor consulte al area de Sistemas.'
     DO vista
     RETURN
ENDIF
SELECT parma
SEEK 'UTILID02'
IF FOUND()
     putil2 = numdec
ELSE
     DO standby WITH  ;
        'El Parametro de Utilidad de Ventas en Farmacia no Existe. por favor consulte al area de Sistemas.'
     DO vista
     RETURN
ENDIF
ON KEY LABEL F9
SELECT invini
SCATTER MEMVAR
DO pantalla
@ 1, 68 GET m.estado DISABLE
@ 2, 22 GET m.periodo DISABLE
@ 3, 22 GET m.numii DISABLE
@ 4, 22 GET m.fecii WHEN .F.
@ 5, 22 GET m.docref VALID  ;
  val_para(m.docref,'TIPDOC',' ', ;
  22,20)
@ 6, 22 GET m.numref
@ 7, 22 GET m.fecref
@ 8, 22 GET m.observ FUNCTION  ;
  'S50'
READ VALID val_read()
IF LASTKEY() <> 27
     ok = trabaja_hi()
     IF ok .AND. LASTKEY() <> 27
          SELECT invini
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
FLUSH
SELECT invini
DO vista
RETURN
*
PROCEDURE ingre
*--------------
SELECT stkalmv
GOTO TOP
IF  .NOT. EOF()
     DO standby WITH 'Solo puede ingresar un Inventario Inicial.'
     SELECT invini
     DO vista
     RETURN
ENDIF
ON KEY LABEL F9
SELECT invini
nreg = IIF( .NOT. EOF(), RECNO(), -1)
DO pantalla
SCATTER BLANK MEMVAR
m.estado = '00'
m.periodo = RIGHT(STR(YEAR(m.fecsis),  ;
            4), 2)
m.fecii = m.fecsis
m.fecref = m.fecsis
SELECT parma
SEEK 'CORRELINVINI'
IF FOUND()
     m.numii = PADL(ALLTRIM(STR(nument +  ;
               1)), 4, '0')
     IF SEEK(m.periodo + m.numii,  ;
        'InvIni')
          DO standby WITH  ;
             'El Correlativo Generado ya Existe.'
          SELECT invini
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
        'El Parametro de Utilidad de Ventas en Farmacia no Existe. por favor consulte al area de Sistemas.'
     DO vista
     RETURN
ENDIF
SELECT invini
@ 1, 68 GET m.estado DISABLE
@ 2, 22 GET m.periodo DISABLE
@ 3, 22 GET m.numii DISABLE
@ 4, 22 GET m.fecii WHEN .F.
@ 5, 22 GET m.docref VALID  ;
  val_para(m.docref,'TIPDOC',' ', ;
  22,20)
@ 6, 22 GET m.numref
@ 7, 22 GET m.fecref
@ 8, 22 GET m.observ FUNCTION  ;
  'S50'
READ VALID val_read()
IF LASTKEY() <> 27
     ok = trabaja_hi()
     IF ok .AND. LASTKEY() <> 27
          SELECT invini
          IF f_appd()
               m.tuser = 'I'
               m.user = vuser_id
               m.duser = DATE()
               m.huser = TIME()
               GATHER MEMVAR
               SELECT parma
               SEEK 'CORRELINVINI'
               IF FOUND()
                    IF f_lock(1)
                         REPLACE nument  ;
                                 WITH  ;
                                 nument +  ;
                                 1
                    ENDIF
               ENDIF
          ENDIF
     ELSE
          IF nreg <> -1
               GOTO nreg
          ENDIF
          SELECT iteii
          IF SEEK(m.periodo +  ;
             m.numii)
               DELETE WHILE  ;
                      periodo =  ;
                      m.periodo  ;
                      .AND. numii =  ;
                      m.numii
          ENDIF
          SELECT kardex
          SET ORDER TO KardexV2
          DO WHILE SEEK(m.periodo+ ;
             'INV'+m.numii)
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
FLUSH
SELECT invini
DO vista
RETURN
*
FUNCTION trabaja_hi
*------------------
vsun = .T.
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°F2->Edita°°°°°°°°°F5->Agregar°°°°°°°°°F8->Eliminar°°°°°°°°F10->Terminar°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F2 DO Edita_item
ON KEY LABEL F5 DO agrega_item
ON KEY LABEL F8 DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteii
SET ORDER TO iteII1
SEEK m.periodo + m.numii
IF  .NOT. FOUND()
     DO agrega_ite
ENDIF
BROWSE NOOPTIMIZE FIELDS item :H =  ;
       'Item', codart :H =  ;
       'Codigo', desart :H =  ;
       'Descripci¢n', totcan :H =  ;
       'Cantidad' :P = '9,999',  ;
       preuni :H = 'P.U.+IGV',  ;
       precom :H = 'Total' :P =  ;
       '9,999.999', lfracc :H =  ;
       'Fracc.', unidad :H =  ;
       'Unidad', unifrac :H =  ;
       'U.fracc.', cantidad :H =  ;
       'Cantidad', preuni :H =  ;
       'P.U.+IGV', fraccion :H =  ;
       'Fraccion', valcom :H =  ;
       'Valor' NOMENU NOAPPEND  ;
       NOEDIT NODELETE NOCLEAR  ;
       WINDOW wind_1 KEY  ;
       m.periodo + m.numii WHEN  ;
       .F.
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
SELECT invini
RETURN vsun
*
PROCEDURE agrega_ite
*-------------------
PRIVATE m.tuser, m.user, m.huser, m.duser
lcant = .F.
lfrac = .F.
DEFINE WINDOW wind_3 FROM 01, 04  ;
       TO 21, 71 TITLE  ;
       'Adici¢n de Items' DOUBLE  ;
       COLOR SCHEME 10
ACTIVATE WINDOW wind_3
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SELECT iteii
SELECT iteii
IF SEEK(m.periodo + m.numii)
     SCAN WHILE periodo + numii = m.periodo + m.numii
          m.item = item
     ENDSCAN
     m.item = PADL(ALLTRIM(STR(VAL(m.item) + 1)), 4, '0')
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
xlab = SPACE(7)
@ 0, 1 SAY '             Item: ' +  m.item
@ 1, 1 SAY '        Num. Inv.: ' +  m.periodo + '.' + m.numii
@ 2, 1 SAY '           Codigo:'  GET m.codart VALID val_art(m.codart) .AND. vcodlab()
@ 3, 1 SAY '      Laboratorio: '
@ 4, 1 SAY '         Medicina:' GET m.desart FUNCTION 'S40' WHEN .F.
@ 5, 1 SAY '    Menudeo Art.?:' GET m.lmenart WHEN .F.
@ 6, 1 SAY '    Unidad (Lote):' GET m.unidad WHEN .F.
@ 7, 1 SAY '  Unidad Fraccion:' GET m.unifrac WHEN .F.
@ 8, 1 SAY '  Fracc. Articulo:' GET m.fracart WHEN .F.
@ 6, 35 SAY '       Ingreso:' GET m.cantidadx VALID val_can()
@ 8, 35 SAY '      Cantidad:' GET m.cantidad WHEN .F.
@ 9, 35 SAY '      Fraccion:' GET m.fraccion WHEN .F.
@ 10, 35 SAY 'Total Cantidad:'GET m.totcan WHEN .F.
@ 12, 1 SAY '     Precio Total:' GET m.precom PICTURE '999,999.99' VALID val_pu(m.codart)
@ 13, 1 SAY '      P.U. (Lote):' GET m.preuni PICTURE '999,999.99' WHEN .F.
@ 14, 1 SAY '          I.G.V. :' GET m.igv PICTURE '999,999.99' WHEN .F.
@ 12, 35 SAY '    Valor Total:'  GET m.valcom PICTURE '999,999.99' WHEN .F.
@ 13, 35 SAY 'Valor U. (Lote):'  GET m.valuni PICTURE '999,999.99' WHEN .F.
@ 14, 35 SAY ' Val. U. Fracc.:'  GET m.valunifr PICTURE '999,999.99' WHEN .F.
@ 16, 09 SAY ' % Vta.(Lote)'
@ 16, 27 SAY 'Prec. Vta.(Lote)'
@ 16, 46 SAY 'Prec. Vta. Fr'
@ 17, 11 SAY '1'
@ 17, 13 GET m.porvta PICTURE '999.99'
@ 17, 20 SAY '%'
@ 17, 30 GET m.preven PICTURE '999,999.99' WHEN val_pv()
@ 17, 47 GET m.prevenfr PICTURE '999,999.99' WHEN val_pvfr(m.codart)
@ 18, 11 SAY '2'
@ 18, 13 GET m.porvta2 PICTURE '999.99'
@ 18, 20 SAY '%'
@ 18, 30 GET m.preven2 PICTURE '999,999.99' WHEN val_pv2()
@ 18, 47 GET m.preven2fr PICTURE '999,999.99' WHEN val_pv2fr(m.codart)
READ VALID val_read() .AND. valing()
IF LASTKEY() <> 27
     ak = actkarpr('INV',m.numii,m.fecii)
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
                  'El Precio de Venta al publico no debe Ser 0'
               mret = .F.
          CASE m.prevenfr = 0
               DO standby WITH  ;
                  'El Precio de Venta fraccionado no debe Ser 0'
               mret = .F.
     ENDCASE
ENDIF
RETURN mret
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
     m.prevenfr = ROUND((m.preven /  ;
                  iteart.fraccion),  ;
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
     m.preven2fr = ROUND((m.preven2 /  ;
                   iteart.fraccion),  ;
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
FUNCTION val_can
*---------------
mret = .T.
IF m.lmenart = 'S'
     IF LEN(m.cantidadx) > 0
          IF VAL(m.cantidadx) = 0
               IF SUBSTR(m.cantidadx, 1, 1) = 'F'
                    m.lfracc = 'S'
               ELSE
                    mret = .F.
               ENDIF
               IF TYPE(SUBSTR(m.cantidadx, 2)) = 'N'
                    m.fraccion = VAL(SUBSTR(m.cantidadx, 2))
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
     DO standby WITH  ;
        'Error en cantidad ingresada'
ENDIF
RETURN mret
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
     DO standby WITH  ;
        'Error en catalogo de Articulos'
ENDIF
SHOW GET m.cantidad
SHOW GET m.fraccion
SET ORDER TO (cord)
SELECT (cali)
RETURN
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
m.preuni = ROUND(m.precom /  m.totcan, 2)
cali = ALIAS()
SELECT iteart
cord = ORDER()
SET ORDER TO IteArt1
IF SEEK('B' + cart)
     m.preuni = m.preuni * iteart.fraccion
     SHOW GET m.preuni
ELSE
     DO standby WITH 'Error en catalogo de Articulos'
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
     m.valunifr = ROUND(m.valuni / iteart.fraccion, 2)
     m.valcom = ROUND(m.valunifr * m.totcan, 2)
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
PROCEDURE edita_item
PRIVATE m.tuser, m.usercr,  ;
        m.husercr, m.dusercr
lcant = .F.
lfrac = .F.
DEFINE WINDOW wind_3 FROM 01, 04  ;
       TO 21, 71 TITLE  ;
       'Edici¢n de Items' DOUBLE  ;
       COLOR SCHEME 10
ACTIVATE WINDOW wind_3
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SELECT iteii
SCATTER MEMVAR
m.tuser = 'C'
m.usercr = vuser_id
m.husercr = TIME()
m.dusercr = DATE()
m.lmenart = ''
m.fracart = 0
xlab = 'B' + LEFT(m.codart, 6)
= val_art(m.codart)
@ 0, 1 SAY '             Item: ' +  ;
  m.item
@ 1, 1 SAY '        Num. Inv.: ' +  ;
  m.periodo + '.' + m.numii
@ 2, 1 SAY '           Codigo:'  ;
  GET m.codart WHEN vcodlab()  ;
  .AND. .F.
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
READ VALID val_read() .AND.  ;
     valing()
IF LASTKEY() <> 27
     ak = actkarpr('INV',numii, ;
          m.fecii)
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
PROCEDURE elimi_item
SELECT iteii
IF yesno( ;
   'Estas seguro de Eliminar este Item' ;
   )
     SELECT kardex
     SET ORDER TO KardexV2
     IF SEEK(iteii.periodo +  ;
        'INV' + iteii.numii +  ;
        iteii.item +  ;
        iteii.codart)
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
     SELECT iteii
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
PRIVATE cord
SELECT iteii
SET RELATION TO 'B' + LEFT(iteii.codart,;
6) INTO artmae ADDITIVE
cord = ORDER()
SET ORDER TO IteII2
SELECT invini
SCATTER MEMVAR
vdbf = SYS(3) + '.Dbf'
COPY TO (vdbf) STRUCTURE
USE IN 0 (vdbf) ALIAS xinv
SELECT xinv
APPEND BLANK
GATHER MEMVAR
SET RELATION TO periodo + numii INTO iteii;
ADDITIVE
SET SKIP TO iteii
DO reporte WITH 2, 'Invent',  ;
   'Invetario', 2, .F., .T.
USE IN xinv
ERASE (vdbf)
SELECT iteii
SET RELATION TO INTO artmae
SET ORDER TO &cOrd
SELECT invini
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
 .NOT. SEEK(vusucla + '0051', 'IteUsuOp')
SET SKIP OF PAD anter OF mmenu;
 .NOT. SEEK(vusucla + '0053', 'IteUsuOp')
SET SKIP OF PAD proxi OF mmenu;
 .NOT. SEEK(vusucla + '0054', 'IteUsuOp')
SET SKIP OF PAD corri OF mmenu;
 .NOT. SEEK(vusucla + '0055', 'IteUsuOp')
SET SKIP OF PAD ingre OF mmenu;
 .NOT. SEEK(vusucla + '0056', 'IteUsuOp')
SET SKIP OF PAD anula OF mmenu;
 .NOT. SEEK(vusucla + '0057', 'IteUsuOp')
SET SKIP OF PAD lista OF mmenu;
 .NOT. SEEK(vusucla + '0058', 'IteUsuOp')
USE IN iteusuop
SELECT (calias)
RETURN
*

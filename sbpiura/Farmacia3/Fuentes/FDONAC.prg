CLOSE DATABASES
USE IN 1 FDonac ALIAS donac ORDER  ;
    FDonac1
USE IN 2 FIteDon ALIAS itedon  ;
    ORDER FIteDon1
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
USE IN 8 Promae ALIAS promae  ;
    ORDER ProMae1
vmens01 = 'Registro de Donaciones de Famacia'
vmens02 = 'Donaciones de Famacia : REVISION '
vmens04 = 'Dicha Donaci�n no fue encontrada'
vmens05 = 'No existe Donaci�n anterior'
vmens06 = 'No existe Donaci�n siguiente'
vmens07 = '� Desea Anular esta Donaci�n ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Donaci�n ha sido anulada'
vmens10 = 'Edici�n'
SELECT donac
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
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 11, 79 TITLE vmens01  ;
       FOOTER  ;
       '[F9] Detalle : Item'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle: Donaciones de Farmacia'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 00, 00  ;
       TO 23, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis OF mmenu PROMPT  ;
       '\<Revisa' AT 24, 00
DEFINE PAD busca OF mmenu PROMPT  ;
       '\<Busca' AT 24, 08
DEFINE PAD anter OF mmenu PROMPT  ;
       '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu PROMPT  ;
       '\<Siguiente' AT 24, 25
DEFINE PAD corri OF mmenu PROMPT  ;
       '\<Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT  ;
       '\<Ingresa' AT 24, 45
DEFINE PAD anula OF mmenu PROMPT  ;
       ' \<Anula ' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Listar ' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
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
@ 0, 50 SAY '      Estado :'
@ 1, 2 SAY '        Donacion :'
@ 1, 50 SAY ' Fec. Donac. :'
@ 2, 2 SAY '   Tipo Donacion :'
@ 4, 2 SAY '       Indigente :'
@ 5, 2 SAY '         Observa :'
@ 6, 2 SAY '  Valor Donacion :'
@ 8, 2 SAY ' Precio Donacion :'
@ 7, 37 SAY '       Redondeo :'
@ 8, 37 SAY 'Total Facturado :'
RETURN
*
PROCEDURE vista
SELECT donac
ON KEY LABEL F9 DO VISTA_DET
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 0, 65 SAY val_est() COLOR  ;
  SCHEME 1
@ 1, 22 SAY m.periodo
@ 1, 25 SAY m.numdon
@ 1, 65 SAY m.fecdon
@ 2, 22 SAY val_para(m.tipdon, ;
  'TIPDON','V',22,30)
@ 4, 22 SAY m.indigente
@ 5, 22 SAY LEFT(m.observa, 50)
@ 6, 22 SAY m.valdon PICTURE  ;
  '99,999.99'
@ 8, 22 SAY m.totdon PICTURE  ;
  '99,999.99'
@ 7, 54 SAY m.redondeo PICTURE  ;
  '99,999.99'
@ 8, 54 SAY m.totfac PICTURE  ;
  '99,999.99'
DO vista_hijo
IF  .NOT. vflag $ 'J*'
     DO subopc
ENDIF
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT itedon
GOTO TOP
SEEK m.periodo + m.numdon
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            item :H = 'ITEM',  ;
            codart :H = 'C�digo',  ;
            desart :H =  ;
            'Descripci�n' : 40,  ;
            totcan :H =  ;
            'Cantidad' :P =  ;
            '9,999', total :H =  ;
            'Total' :P =  ;
            '9,999.999', cantidad  ;
            :H = 'Unid.' :P =  ;
            '9,999', fraccion :H =  ;
            'Frac.', preven :H =  ;
            'P.U.', prevenfr :H =  ;
            'P.U.Fr.' NOMENU  ;
            NOAPPEND NODELETE  ;
            NOCLEAR WINDOW wind_1  ;
            KEY m.periodo +  ;
            m.numdon TIMEOUT  ;
            0.0001  NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 4, 25 SAY  ;
       'No hay Detalles de este Documento'
     ACTIVATE WINDOW wind_0
ENDIF
SELECT donac
RETURN
*
FUNCTION val_est
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
PROCEDURE vista_det
SELECT itedon
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
GOTO TOP
SEEK m.periodo + m.numdon
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            item :H = 'ITEM',  ;
            codart :H = 'C�digo',  ;
            desart :H =  ;
            'Descripci�n' : 40,  ;
            totcan :H =  ;
            'Cantidad' :P =  ;
            '9,999', total :H =  ;
            'Total' :P =  ;
            '9,999.999', cantidad  ;
            :H = 'Unid.' :P =  ;
            '9,999', fraccion :H =  ;
            'Frac.', preven :H =  ;
            'P.U.', prevenfr :H =  ;
            'P.U. Fr.' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_1 KEY  ;
            m.periodo + m.numdon  ;
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
SELECT donac
DO vista
RETURN
*
PROCEDURE revis
SELECT donac
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '�����������Presione �F10� para seleccionar  o  �Esc� para cancelar������������'
DO logos WITH rotulo1, vtempo
SET RELATION TO periodo + numdon INTO;
itedon
SET SKIP TO itedon
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE FIELDS periodo :H =  ;
       'Periodo', numdon :H =  ;
       'Don', itedon.item :H =  ;
       'ITEM', itedon.codart :H =  ;
       'C�digo', itedon.desart :H =  ;
       'Descripci�n',  ;
       itedon.cantidad :H =  ;
       'Unid.', itedon.fraccion  ;
       :H = 'Fracc.',  ;
       itedon.fraccion :H =  ;
       'Cantidad', itedon.preven  ;
       :H = 'P.U.',  ;
       itedon.prevenfr :H =  ;
       'P.U. Fr.', itedon.total  ;
       :H = 'Total' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_2
vtempo = '��������������������������������������������������������������������������������'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELECT donac
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
vnum_don = '    '
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N�mero Donacion : '  ;
  GET vperiodo PICTURE '!!'
@ 1, 30 SAY '-' GET vnum_don  ;
  PICTURE '!!!!' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_don) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vperiodo + vnum_don
     IF  .NOT. FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
RETURN
*
FUNCTION vbusca
vnum_don = PADL(ALLTRIM(vnum_don),  ;
           4, '0')
RETURN .T.
*
PROCEDURE anter
SELECT donac
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
SELECT donac
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
     SELECT donac
     DO vista
     RETURN
ENDIF
SELECT donac
SCATTER MEMVAR
DO pantalla
@ 0, 65 GET m.estado WHEN .F.
@ 1, 22 GET m.periodo WHEN .F.
@ 1, 25 GET m.numdon WHEN .F.
@ 1, 65 GET m.fecdon WHEN .F.
@ 2, 22 GET m.tipdon WHEN .F.
@ 4, 22 GET m.indigente WHEN .F.
@ 5, 22 GET m.observa FUNCTION  ;
  'S50' WHEN .F.
@ 6, 22 GET m.valdon PICTURE  ;
  '99,999.99' WHEN .F.
@ 8, 22 GET m.totdon PICTURE  ;
  '99,999.99' WHEN .F.
READ
IF LASTKEY() <> 27
     ok = trabaja_hi()
     IF ok
          ok = indigente()
     ENDIF
     IF ok .AND. LASTKEY() <> 27
          SELECT donac
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
SELECT donac
DO vista
RETURN
*
PROCEDURE ingre
SELECT stkalmv
GOTO TOP
IF EOF()
     DO standby WITH  ;
        'No existe Inventario Inicial en el sistema. por favor revise'
     SELECT donac
     DO vista
     RETURN
ENDIF
SELECT kardex
GOTO TOP
IF estado = '20'
     DO standby WITH  ;
        'El periodo ya esta cerrado no se puede hacer modificaciones'
     SELECT donac
     DO vista
     RETURN
ENDIF
ON KEY LABEL F9
SELECT donac
nreg = IIF( .NOT. EOF(), RECNO(), - ;
       1)
DO pantalla
SCATTER BLANK MEMVAR
m.estado = '00'
m.periodo = RIGHT(STR(YEAR(m.fecsis),  ;
            4), 2)
m.fecdon = m.fecsis
SELECT parma
SEEK 'CORRELFDON'
IF FOUND()
     m.numdon = PADL(ALLTRIM(STR(nument +  ;
                1)), 4, '0')
     IF SEEK(m.periodo + m.numdon,  ;
        'Donac')
          DO standby WITH  ;
             'El Correlativo Generado ya Existe.'
          SELECT donac
          DO vista
          RETURN
     ENDIF
ELSE
     DO standby WITH  ;
        'El Parametro de Correlativos no existe. por favor consulte al area de Sistemas'
     DO vista
     RETURN
ENDIF
SELECT donac
@ 0, 65 GET m.estado WHEN .F.
@ 1, 22 GET m.periodo WHEN .F.
@ 1, 25 GET m.numdon WHEN .F.
@ 1, 65 GET m.fecdon WHEN .F.
@ 2, 22 GET m.tipdon WHEN .F.
@ 4, 22 GET m.indigente WHEN .F.
@ 5, 22 GET m.observa FUNCTION  ;
  'S50' WHEN .F.
@ 6, 22 GET m.valdon PICTURE  ;
  '99,999.99' WHEN .F.
@ 8, 22 GET m.totdon PICTURE  ;
  '99,999.99' WHEN .F.
IF LASTKEY() <> 27
     ok = trabaja_hi()
     IF ok
          ok = indigente()
     ENDIF
     IF ok .AND. LASTKEY() <> 27
          SELECT donac
          IF f_appd()
               m.tuser = 'I'
               m.user = vuser_id
               m.duser = DATE()
               m.huser = TIME()
               GATHER MEMVAR
               SELECT parma
               SEEK 'CORRELFDON'
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
          SELECT itedon
          IF SEEK(m.periodo +  ;
             m.numdon)
               DELETE WHILE  ;
                      periodo =  ;
                      m.periodo  ;
                      .AND.  ;
                      numdon =  ;
                      m.numdon
          ENDIF
          SELECT kardex
          SET ORDER TO KardexV2
          DO WHILE SEEK(m.periodo+ ;
             'DON'+m.numdon)
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
SELECT donac
DO vista
RETURN
*
FUNCTION indigente
PRIVATE mret
DEFINE WINDOW indigente FROM 06,  ;
       00 TO 18, 79 TITLE  ;
       'CLIENTE' FOOTER '' DOUBLE  ;
       COLOR SCHEME 10
ACTIVATE WINDOW indigente
@ 0, 0 SAY 'Tipo Donacion : ' GET  ;
  m.tipdon VALID  ;
  val_para(m.tipdon,'TIPDON',' ', ;
  17,30)
@ 1, 0 SAY '    Indigente : ' GET  ;
  m.indigente
@ 2, 0 SAY ' Observaciones: ' GET  ;
  m.observa PICTURE '@S73'
@ 4, 0 SAY '   Valor Donac.: '  ;
  GET m.valdon PICTURE  ;
  '99,999.99' WHEN .F.
@ 6, 0 SAY '   Total Donac.: '  ;
  GET m.totdon PICTURE  ;
  '99,999.99' WHEN .F.
@ 7, 0 SAY '       Redondeo: '  ;
  GET m.redondeo PICTURE  ;
  '99,999.99'
@ 8, 0 SAY 'Total Facturado: '  ;
  GET m.totfac PICTURE  ;
  '99,999.99' WHEN vtotfac()
READ
DEACTIVATE WINDOW indigente
IF LASTKEY() <> 27
     mret = .T.
ELSE
     mret = .F.
ENDIF
RETURN mret
*
FUNCTION vtotfac
m.totfac = m.totdon + m.redondeo
RETURN .F.
*
FUNCTION trabaja_hi
vsun = .T.
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '�����F2->Edita���������F5->Agregar���������F8->Eliminar��������F10->Terminar����'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F2 DO Edita_item
ON KEY LABEL F5 DO agrega_item
ON KEY LABEL F8 DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itedon
SET ORDER TO FiteDon1
SEEK m.periodo + m.numdon
IF  .NOT. FOUND()
     DO agrega_ite
ENDIF
BROWSE NOOPTIMIZE FIELDS item :H =  ;
       'Item', codart :H =  ;
       'Codigo', desart :H =  ;
       'Descripci�n' : 40, totcan  ;
       :H = 'Cantidad' :P =  ;
       '9,999', total :H =  ;
       'Total' :P = '9,999.999',  ;
       cantidad :H = 'Unid.' :P =  ;
       '9,999', lfracc :H =  ;
       'Fracc.', unidad :H =  ;
       'Unidad', unifrac :H =  ;
       'U.fracc.', fraccion :H =  ;
       'Frac.', preven :H =  ;
       'P.U.', prevenfr :H =  ;
       'P.U.Frc.' NOMENU NOAPPEND  ;
       NOEDIT NODELETE NOCLEAR  ;
       WINDOW wind_1 KEY  ;
       m.periodo + m.numdon WHEN  ;
       .F. NOREFRESH
m.totdon = 0
SEEK m.periodo + m.numdon
IF FOUND()
     SCAN WHILE periodo + numdon =  ;
          m.periodo + m.numdon
          m.totdon = m.totdon +  ;
                     total
     ENDSCAN
     m.totdon = ROUND(m.totdon,  ;
                2)
ENDIF
IF m.totdon > 0
     m.valdon = m.totdon
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
vtempo = '��������������������������������������������������������������������������������'
DO logos WITH rotulo1, vtempo
SELECT donac
RETURN vsun
*
PROCEDURE agrega_ite
PRIVATE m.tuser, m.user, m.huser,  ;
        m.duser
lcant = .F.
lfrac = .F.
DEFINE WINDOW wind_3 FROM 02, 04  ;
       TO 20, 71 TITLE  ;
       'Donaci�n de Medicamentos'  ;
       DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW wind_3
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SELECT itedon
IF SEEK(m.periodo + m.numdon)
     SCAN WHILE periodo + numdon =  ;
          m.periodo + m.numdon
          m.item = item
     ENDSCAN
     m.item = PADL(ALLTRIM(STR(VAL(m.item) +  ;
              1)), 4, '0')
ELSE
     m.item = '0001'
ENDIF
m.codart = SPACE(12)
m.desart = SPACE(60)
m.lfracc = SPACE(1)
m.unidad = SPACE(10)
m.unifrac = SPACE(10)
m.cantidad = 0
m.fraccion = 0
m.totcan = 0
m.preven = 0
m.prevenfr = 0
m.preuni = 0
m.valuni = 0
m.valunifr = 0
m.total = 0
m.tuser = 'I'
m.user = vuser_id
m.huser = TIME()
m.duser = DATE()
@ 0, 1 SAY '             Item: ' +  ;
  m.item
@ 1, 1 SAY '    Num. Donacion: ' +  ;
  m.periodo + '.' + m.numdon
@ 2, 1 SAY '           Codigo:'  ;
  GET m.codart VALID  ;
  art_alm(m.codart) .AND.  ;
  vcodlab()
@ 3, 1 SAY '      Laboratorio: '
@ 4, 1 SAY '         Medicina:'  ;
  GET m.desart FUNCTION 'S40'  ;
  WHEN .F.
@ 5, 1 SAY 'Prod.Fraccionado?:'  ;
  GET m.lfracc PICTURE '@M S,N,'  ;
  VALID val_fracc(m.codart)
@ 6, 1 SAY '    Unidad (Lote):'  ;
  GET m.unidad WHEN .F.
@ 7, 1 SAY '  Unidad Fraccion:'  ;
  GET m.unifrac WHEN .F.
@ 8, 35 SAY '         Cantidad:'  ;
  GET m.cantidad VALID val_can()  ;
  WHEN lcant
@ 9, 35 SAY '         Fraccion:'  ;
  GET m.fraccion VALID val_can()  ;
  WHEN lfrac
@ 10, 35 SAY '   Total Cantidad:'  ;
  GET m.totcan WHEN .F.
@ 11, 1 SAY '    Precio Venta :'  ;
  GET m.preven PICTURE  ;
  '999,999.9999' DISABLE
@ 12, 1 SAY '  Prec. Vta. Frc.:'  ;
  GET m.prevenfr PICTURE  ;
  '999,999.9999' DISABLE
@ 13, 1 SAY '            Total:'  ;
  GET m.total PICTURE  ;
  '999,999.9999' WHEN .F.
READ VALID val_read() .AND.  ;
     valing()
IF LASTKEY() <> 27
     ak = actkarpr('DON',m.numdon, ;
          m.fecdon)
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
          OTHERWISE
               mret = .T.
     ENDCASE
ENDIF
RETURN mret
*
PROCEDURE edita_item
PRIVATE m.tuser, m.usercr,  ;
        m.husercr, m.dusercr
lcant = .F.
lfrac = .F.
DEFINE WINDOW wind_3 FROM 04, 00  ;
       TO 23, 75 TITLE  ;
       'Edici�n de Articulos'  ;
       DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW wind_3
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SELECT itedon
SCATTER MEMVAR
m.tuser = 'C'
m.usercr = vuser_id
m.husercr = TIME()
m.dusercr = DATE()
@ 0, 1 SAY '             Item: ' +  ;
  m.item
@ 1, 1 SAY '    Num. Donacion: ' +  ;
  m.periodo + '.' + m.numdon
@ 2, 1 SAY '           Codigo:'  ;
  GET m.codart WHEN vcodlab()  ;
  .AND. .F.
@ 3, 1 SAY '      Laboratorio: '
@ 4, 1 SAY '         Medicina:'  ;
  GET m.desart FUNCTION 'S40'  ;
  WHEN .F.
@ 5, 1 SAY 'Prod.Fraccionado?:'  ;
  GET m.lfracc PICTURE '@M S,N'  ;
  VALID val_fracc(m.codart)
@ 6, 1 SAY '    Unidad (Lote):'  ;
  GET m.unidad WHEN .F.
@ 7, 1 SAY '  Unidad Fraccion:'  ;
  GET m.unifrac WHEN .F.
@ 8, 1 SAY '         Cantidad:'  ;
  GET m.cantidad VALID val_can()  ;
  WHEN lcant
@ 9, 1 SAY '         Fraccion:'  ;
  GET m.fraccion VALID val_can()  ;
  WHEN lfrac
@ 10, 1 SAY '   Total Cantidad:'  ;
  GET m.totcan WHEN .F.
@ 11, 1 SAY '    Precio Venta :'  ;
  GET m.preven PICTURE  ;
  '999,999.9999' DISABLE
@ 12, 1 SAY '  Prec. Vta. Frc.:'  ;
  GET m.prevenfr PICTURE  ;
  '999,999.9999' DISABLE
@ 13, 1 SAY '            Total:'  ;
  GET m.total PICTURE  ;
  '999,999.9999' WHEN .F.
READ VALID val_read()
IF LASTKEY() <> 27
     ak = actkarpr('DON',numdon, ;
          m.fecdon)
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
PROCEDURE val_can
m.totcan = m.fraccion *  ;
           m.cantidad
m.total = IIF(m.lfracc = 'S',  ;
          m.totcan * m.prevenfr,  ;
          IIF(m.lfracc = 'N',  ;
          m.cantidad * m.preven,  ;
          0))
IF m.totcan > stkalmv.salfrac
     DO standby WITH  ;
        'La Cantidad Ingresada esta Excediendo al Stock. Revise'
ENDIF
SHOW GET m.totcan
SHOW GET m.total
RETURN
*
PROCEDURE elimi_item
SELECT itedon
IF yesno( ;
   'Estas seguro de Eliminar este Item' ;
   )
     SELECT kardex
     SET ORDER TO KardexV2
     IF SEEK(itedon.periodo +  ;
        'DON' + itedon.numdon +  ;
        itedon.item +  ;
        itedon.codart)
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
     SELECT itedon
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
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
nreg = RECNO()
DEFINE WINDOW wlista FROM 3, 15  ;
       TO 20, 70 FLOAT TITLE  ;
       'Listado de Pedidos'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
vtodn = 1
lelec = 1
ltdon = 1
mfecha1 = CTOD('01/01/' +  ;
          STR(YEAR(m.fecsis),  ;
          4))
mfecha2 = DATE()
@ 01, 01 SAY  ;
  'Todos las Donaciones : ' GET  ;
  vtodn SIZE 1, 10, 2 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 03, 01 SAY  ;
  '            Donacion : ' GET  ;
  m.periodo WHEN vtodn = 2
@ 03, 27 GET m.numdon WHEN vtodn =  ;
  2
@ 05, 01 SAY  ;
  '                Modo : ' GET  ;
  lelec FUNCTION  ;
  '^ Resumen;Detallado' WHEN  ;
  vtodn = 1
@ 08, 01 SAY  ;
  '       Tipo Donacion : ' GET  ;
  ltdon FUNCTION  ;
  '^ Todos;Servicio Social;Campa�as M�dicas;Botiquines'  ;
  WHEN vtodn = 1
@ 12, 01 SAY  ;
  '      Rango de Fechas: ' GET  ;
  mfecha1 WHEN vtodn = 1
@ 12, 36 GET mfecha2 WHEN vtodn =  ;
  1
@ 15, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
IF LASTKEY() <> 27 .AND. okcancel <>  ;
   2
     IF vtodn = 2
          SCATTER MEMVAR
          vdbf = SYS(3) + '.Dbf'
          COPY TO (vdbf)  ;
               STRUCTURE
          USE IN 0 (vdbf) ALIAS  ;
              xdonac
          SELECT xdonac
          APPEND BLANK
          GATHER MEMVAR
          SET RELATION TO periodo + numdon;
INTO itedon ADDITIVE
          SET SKIP TO itedon
          DO reporte WITH 2,  ;
             'Donac',  ;
             ' Donaciones.', 2,  ;
             .F., .T.
          USE IN xdonac
          ERASE (vdbf)
          SELECT donac
     ELSE
          SELECT donac
          DO CASE
               CASE ltdon = 1
                    lfil = .T.
               CASE ltdon = 2
                    lfil = "m.TipDon = '01'"
               CASE ltdon = 3
                    lfil = "m.TipDon = '02'"
               CASE ltdon = 4
                    lfil = "m.TipDon = '03'"
          ENDCASE
          SET FILTER TO &lFil AND BETW(FecDon,mFecha1,mFecha2)
          IF lelec = 1
               DO reporte WITH 2,  ;
                  'Donaci',  ;
                  'Reporte de Donaciones Resumen',  ;
                  2, .F., .T.
          ELSE
               SET RELATION TO periodo;
+ numdon INTO itedon
               SET SKIP TO itedon
               DO reporte WITH 2,  ;
                  'Donaci2',  ;
                  'Reporte de Donaciones Detallado',  ;
                  2, .F., .T.
               SET RELATION TO
          ENDIF
          SET FILTER TO
     ENDIF
ENDIF
SELECT donac
GOTO nreg
DO vista
RETURN
*
PROCEDURE anula
PRIVATE nreg, lanula
lanula = .F.
IF yesno( ;
   'Esta seguro de Anular este Documento' ;
   )
     lanula = .T.
ENDIF
IF lanula
     DO CASE
          CASE estado = '99'
               DO standby WITH  ;
                  'El Docuemnto ya esta Anulado'
               lanula = .F.
          CASE estado = '20'
               DO standby WITH  ;
                  'EL Documento ya esta cancelado no se puede anular.'
               lanula = .F.
          OTHERWISE
               lanula = .T.
     ENDCASE
ENDIF
IF lanula
     SELECT itedon
     IF SEEK(m.periodo +  ;
        m.numdon)
          SCAN WHILE periodo +  ;
               numdon = m.periodo +  ;
               m.numdon
               REPLACE estado  ;
                       WITH '99'
          ENDSCAN
     ENDIF
     SELECT kardex
     SET ORDER TO KardexV2
     IF SEEK(m.periodo + 'DON' +  ;
        m.numdon)
          SCAN WHILE periodo +  ;
               tipdoc + numdoc =  ;
               m.periodo + 'DON' +  ;
               m.numdon
               REPLACE estado  ;
                       WITH '99'
               nreg = RECNO()
               cperiodo = periodo
               ccodart = codart
               ccorrel = correl
               DO recalpr WITH  ;
                  cperiodo,  ;
                  ccodart,  ;
                  ccorrel
               GOTO nreg
          ENDSCAN
     ENDIF
     SET ORDER TO KardexV1
     SELECT donac
     IF SEEK(m.periodo +  ;
        m.numdon)
          REPLACE estado WITH  ;
                  '99'
     ENDIF
ENDIF
SELECT donac
DO vista
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
 .NOT. SEEK(vusucla + '0043', 'IteUsuOp')
SET SKIP OF PAD busca OF mmenu;
 .NOT. SEEK(vusucla + '0044', 'IteUsuOp')
SET SKIP OF PAD anter OF mmenu;
 .NOT. SEEK(vusucla + '0045', 'IteUsuOp')
SET SKIP OF PAD proxi OF mmenu;
 .NOT. SEEK(vusucla + '0046', 'IteUsuOp')
SET SKIP OF PAD corri OF mmenu;
 .NOT. SEEK(vusucla + '0047', 'IteUsuOp')
SET SKIP OF PAD ingre OF mmenu;
 .NOT. SEEK(vusucla + '0048', 'IteUsuOp')
SET SKIP OF PAD anula OF mmenu;
 .NOT. SEEK(vusucla + '0049', 'IteUsuOp')
SET SKIP OF PAD lista OF mmenu;
 .NOT. SEEK(vusucla + '0050', 'IteUsuOp')
USE IN iteusuop
SELECT (calias)
RETURN
*

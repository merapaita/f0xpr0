PARAMETER vopcion
IF vopcion = 1
     USE IN 1 PteAnu ALIAS anupa  ;
         ORDER PteAnu3
     USE IN 2 OrdCom ALIAS orden  ;
         ORDER OrdCom1
     USE IN 3 IteOc ALIAS iteoc  ;
         ORDER IteOc1
     USE IN 15 Iteoc1 ALIAS  ;
         iteoc1 ORDER IteOc11
ELSE
     USE IN 1 PteAnu ALIAS anupa  ;
         ORDER PteAnu4
     USE IN 2 OrdSer ALIAS ordse  ;
         ORDER OrdSer1
     USE IN 15 Iteos1 ALIAS  ;
         iteos1 ORDER IteOs11
ENDIF
USE IN 4 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 5 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 6 Itepec ALIAS itepec  ;
    ORDER ItePec1
USE IN 7 AuxCot ALIAS auxcot  ;
    ORDER AuxCot1
USE IN 8 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 9 Calen ALIAS calen ORDER  ;
    calen1
USE IN 10 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 11 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 12 HOJCON ALIAS hoja ORDER  ;
    hojcon1
USE IN 13 COMPAG ALIAS compag  ;
    ORDER compag1
USE IN 14 Cheque ALIAS cheque  ;
    ORDER Cheque1
IF vopcion = 1
     vmens01 = ' Parte Rebaja Orden de Compra : REVISION '
     vmens02 = ' Registro de Parte Rebaja Ordenes de Compra '
     vmens04 = 'Dicho Orden de Compra no fue encontrado'
     vmens05 = 'No existe Orden de Compra anterior'
     vmens06 = 'No existe Orden de Compra siguiente'
     vmens07 = '¨ Desea ModificaR ‚ste Orden de Compra ?'
     vmens08 = 'No hay registros para procesar'
     vmens09 = 'Este Orden de Compra ha sido Modificado'
     vmens10 = 'Este Orden de Compra ya fue atendido'
     vmens11 = 'Este Orden de Compra ha sido devuelto'
ELSE
     vmens01 = ' Parte Rebaja Orden de Servicio: REVISION '
     vmens02 = ' Registro de Parte Rebaja Ordenes de Servicio '
     vmens04 = 'Dicho Orden de Servicio no fue encontrado'
     vmens05 = 'No existe Orden de Servicio anterior'
     vmens06 = 'No existe Orden de Servicio siguiente'
     vmens07 = '¨ Desea Modificar ‚ste Orden de Servicio ?'
     vmens08 = 'No hay registros para procesar'
     vmens09 = 'Este Orden de Servicio ha sido Modificado'
     vmens10 = 'Este Orden de Servicio ya fue atendido'
     vmens11 = 'Este Orden de Servicio ha sido devuelto'
ENDIF
SELECT anupa
GOTO BOTTOM
SCATTER BLANK MEMVAR
HIDE POPUP ALL
DO inicia
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
vtempo = ' Revisa  Busca  Anterior  Siguiente                                     Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 11, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       ' Detalle O/C:            ®F12¯ Habilita Item'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2a FROM 10, 04  ;
       TO 16, 75 TITLE  ;
       '®F5¯ Agrega  ° ®F8¯ Eliminar  ° ®F10¯ Terminar '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 12, 04  ;
       TO 18, 75 TITLE  ;
       '®F5¯ Agrega  ° ®F8¯ Eliminar  ° ®F10¯ Terminar '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_4 FROM 10, 04  ;
       TO 16, 75 TITLE  ;
       '° ®F10¯ Terminar °'  ;
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
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_0
CLEAR
@ 1, 2 SAY '       N£mero P/R :'
@ 2, 2 SAY '        Fecha P/R :'
IF vopcion = 1
     @ 1, 40 SAY  ;
       '       N£mero O/C :'
     @ 2, 40 SAY  ;
       '        Fecha O/C :'
ELSE
     @ 1, 40 SAY  ;
       '       N£mero O/S :'
     @ 2, 40 SAY  ;
       '        Fecha O/S :'
ENDIF
@ 4, 2 SAY '        Proveedor :'
@ 5, 2 SAY '   Importe Rebaja :'
@ 6, 2 SAY '       Cadena Fun :'
@ 7, 2 SAY ' Fte. Financiami. :'
@ 8, 2 SAY '          Funci¢n :'
@ 9, 2 SAY '         Programa :'
@ 10, 2 SAY '      Subprograma :'
@ 11, 2 SAY '   Activ./Proyec. :'
@ 13, 2 SAY '           Motivo :'
@ 15, 2 SAY '       Numero H/C :'
@ 16, 2 SAY '       Numero C/P :'
@ 17, 2 SAY '    Numero Cheque :'
@ 19, 2 SAY '    Observaciones :'
RETURN
*
PROCEDURE vista
ACTIVATE WINDOW wind_0
SELECT anupa
IF EOF()
     DO pantalla
     RETURN
ENDIF
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
@ 0, 60 SAY IIF(m.estado = '00',  ;
  'Pendiente    ', IIF(m.estado =  ;
  '20', 'En Cont.Pres',  ;
  IIF(m.estado = '99',  ;
  'Anulada  ', IIF(m.estado =  ;
  '50', 'Atendido ',  ;
  '         '))))
@ 1, 22 SAY m.numpa
@ 1, 60 SAY m.periodo
@ 1, 63 SAY m.codfte
@ 1, 67 SAY m.numref
@ 2, 22 SAY m.fecpa
@ 2, 60 SAY m.fecref
@ 4, 22 SAY val_prv(m.codprv)
@ 5, 22 SAY m.valtot PICTURE  ;
  '999,999.99'
@ 6, 22 SAY val_codcad(m.codcad, ;
  m.periodo,'D',22,30)
@ 7, 22 SAY val_para(m.codfte, ;
  'CODFTE','D',22,30)
@ 8, 22 SAY  ;
  val_para(maepre.codfun,'CODFUN', ;
  'V',22,40)
@ 9, 22 SAY  ;
  val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'V',22, ;
  40)
@ 10, 22 SAY  ;
  val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'V',22, ;
  40)
@ 11, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 13, 22 SAY m.motivo PICTURE  ;
  '@S56'
@ 15, 22 SAY m.numhc
@ 15, 26 SAY '.'
@ 15, 27 SAY m.perhc
@ 16, 22 SAY m.numcp
@ 16, 26 SAY '.'
@ 16, 27 SAY m.percp
@ 17, 22 SAY m.numchq
@ 19, 22 SAY m.observa
RETURN
*
PROCEDURE revis
SELECT anupa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS numpa :H = 'N§Dc',  ;
       fecpa :H = 'Fecha', tipdoc  ;
       :H = 'Doc', numref :H =  ;
       'N§', fecref :H = 'Fecha',  ;
       ess = IIF(estado = '00',  ;
       'Pend', IIF(estado = '20',  ;
       'Afec', IIF(estado = '9',  ;
       'Anul', IIF(estado = '40',  ;
       'Aten', 'Liqu')))) :H =  ;
       'Estd', codprv :H = 'Prv',  ;
       observa :H = 'Observaci¢n'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
DO vista
RETURN
*
PROCEDURE busca
SELECT anupa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnum_oc = 0
ACTIVATE WINDOW standby
IF vopcion = 1
     @ 1, 01 SAY  ;
       'Ingrese N£mero O/C : '
ELSE
     @ 1, 01 SAY  ;
       'Ingrese N£mero O/S : '
ENDIF
@ 1, 23 GET vnum_oc PICTURE  ;
  '9999' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_oc) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vnum_oc
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
vnum_oc = PADL(ALLTRIM(STR(vnum_oc,  ;
          4)), 4, '0')
RETURN .T.
*
PROCEDURE anter
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
FUNCTION verfi_s
IF m.valtot > vmax
     DO standby WITH  ;
        'Se est  exediendo en '+ ;
        STR(m.valtot-vmax, 5)
     RETURN .F.
ELSE
     RETURN .T.
ENDIF
*
PROCEDURE desmarca
IF yesno( ;
   'Est  seguro de habilitar este Item?' ;
   )
     IF RLOCK()
          IF  .NOT. EMPTY(numsc)
               REPLACE orden WITH  ;
                       ' ',  ;
                       estado  ;
                       WITH '20',  ;
                       numoc WITH  ;
                       SPACE(4)
          ELSE
               REPLACE orden WITH  ;
                       ' ',  ;
                       estado  ;
                       WITH '00',  ;
                       numoc WITH  ;
                       SPACE(4)
          ENDIF
     ENDIF
     UNLOCK
ENDIF
RETURN
*
PROCEDURE val_rev
PRIVATE vtemp
IF vopcion = 1
     as = ALIAS()
     SELECT orden
     vtemp = RECNO()
     IF EOF()
          DO standby WITH vmens08
          RETURN
     ENDIF
     SET RELATION TO periodo + numoc +;
codfte INTO iteoc
     SET SKIP TO iteoc
     SET FILTER TO estado <> '90';
.AND. estado <> '99'
     GOTO TOP
     SEEK m.periodo + m.numref
     IF  .NOT. FOUND()
          vtemp = RECNO()
          HIDE MENU mmenu
          ACTIVATE SCREEN
          vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          ON KEY LABEL F10 KEYBOARD CHR(23)
          BROWSE FIELDS numoc :H =  ;
                 ' N§ ', fecoc :H =  ;
                 'Fecha', ess =  ;
                 IIF(estado =  ;
                 '00', 'Pend',  ;
                 IIF(estado =  ;
                 '20', 'Afec',  ;
                 IIF(estado = '9',  ;
                 'Anul',  ;
                 IIF(estado =  ;
                 '40', 'Aten',  ;
                 'Liqu')))) :H =  ;
                 'Estd',  ;
                 iteoc.descri :H =  ;
                 'Articulo ' : 36,  ;
                 iteoc.coduni :H =  ;
                 'Unid',  ;
                 iteoc.canreq :H =  ;
                 'Cantid' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 wind_0
          vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          SHOW MENU mmenu
          ON KEY LABEL F10
          m.numhc = orden.numhc
          m.perhc = orden.perhc
          SELECT hoja
          vord = ORDER()
          SET ORDER TO hojcon1
          SEEK m.perhc + m.numhc
          IF FOUND()
               IF  .NOT.  ;
                   EMPTY(hoja.numcp)
                    m.numcp = hoja.numcp
                    m.percp = hoja.nummescp
                    vcta = hoja.codctc
               ELSE
                    m.numcp = ''
                    m.percp = ''
                    vcta = ''
               ENDIF
          ELSE
               m.numcp = ''
               m.percp = ''
               vcta = ''
          ENDIF
          SET ORDER TO (vord)
          SELECT cheque
          vord = ORDER()
          SET ORDER TO Cheque1
          SEEK m.percp + m.numcp +  ;
               vcta
          IF FOUND()
               m.numchq = cheque.numchq
          ELSE
               m.numchq = ''
          ENDIF
          SET ORDER TO (vord)
          m.codfte = iteoc.codfte
     ELSE
          m.numhc = orden.numhc
          m.perhc = orden.perhc
          SELECT hoja
          vord = ORDER()
          SET ORDER TO hojcon1
          SEEK m.perhc + m.numhc
          IF FOUND()
               IF  .NOT.  ;
                   EMPTY(hoja.numcp)
                    m.numcp = hoja.numcp
                    m.percp = hoja.nummescp
                    vcta = hoja.codctc
               ELSE
                    m.numcp = ''
                    m.percp = ''
                    vcta = ''
               ENDIF
          ELSE
               m.numcp = ''
               m.percp = ''
               vcta = ''
          ENDIF
          SET ORDER TO (vord)
          SELECT cheque
          vord = ORDER()
          SET ORDER TO Cheque1
          SEEK m.percp + m.numcp +  ;
               vcta
          IF FOUND()
               m.numchq = cheque.numchq
          ELSE
               m.numchq = ''
          ENDIF
          SET ORDER TO (vord)
          m.codfte = iteoc.codfte
     ENDIF
     SELECT (as)
ELSE
     as = ALIAS()
     SELECT ordse
     IF EOF()
          DO standby WITH vmens08
          RETURN
     ENDIF
     SET FILTER TO estado <> '90';
.AND. estado <> '99'
     GOTO TOP
     SEEK m.periodo + m.numref +  ;
          ALLTRIM(m.codfte)
     IF  .NOT. FOUND()
          vtemp = RECNO()
          HIDE MENU mmenu
          ACTIVATE SCREEN
          vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          ON KEY LABEL F10 KEYBOARD CHR(23)
          BROWSE FIELDS numos :H =  ;
                 ' N§ ', est =  ;
                 IIF(estado =  ;
                 '00', 'Pend',  ;
                 IIF(estado =  ;
                 '20', 'S/Ct',  ;
                 IIF(estado =  ;
                 '99', 'Anul',  ;
                 IIF(estado =  ;
                 '50', 'Aten',  ;
                 ' -  ')))) :H =  ;
                 'ESTD', fecos :H =  ;
                 'Fecha', coddep  ;
                 :H = 'DEP',  ;
                 desos :H =  ;
                 'Descripci¢n'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 WINDOW wind_0
          vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          m.codfte = ordse.codfte
          SHOW MENU mmenu
          ON KEY LABEL F10
          m.numhc = ordse.numhc
          m.perhc = ordse.perhc
          SELECT hoja
          vord = ORDER()
          SET ORDER TO hojcon1
          SEEK m.perhc + m.numhc
          IF FOUND()
               IF  .NOT.  ;
                   EMPTY(hoja.numcp)
                    m.numcp = hoja.numcp
                    m.percp = hoja.nummescp
                    vcta = hoja.codctc
               ELSE
                    m.numcp = ''
                    m.percp = ''
                    vcta = ''
               ENDIF
          ELSE
               m.numcp = ''
               m.percp = ''
               vcta = ''
          ENDIF
          SET ORDER TO (vord)
          SELECT cheque
          vord = ORDER()
          SET ORDER TO Cheque1
          SEEK m.percp + m.numcp +  ;
               vcta
          IF FOUND()
               m.numchq = cheque.numchq
          ELSE
               m.numchq = ''
          ENDIF
          SET ORDER TO (vord)
          m.codfte = ordse.codfte
     ELSE
          m.numhc = ordse.numhc
          m.perhc = ordse.perhc
          SELECT hoja
          vord = ORDER()
          SET ORDER TO hojcon1
          SEEK m.perhc + m.numhc
          IF FOUND()
               IF  .NOT.  ;
                   EMPTY(hoja.numcp)
                    m.numcp = hoja.numcp
                    m.percp = hoja.nummescp
                    vcta = hoja.codctc
               ELSE
                    m.numcp = ''
                    m.percp = ''
                    vcta = ''
               ENDIF
          ELSE
               m.numcp = ''
               m.percp = ''
               vcta = ''
          ENDIF
          SET ORDER TO (vord)
          SELECT cheque
          vord = ORDER()
          SET ORDER TO Cheque1
          SEEK m.percp + m.numcp +  ;
               vcta
          IF FOUND()
               m.numchq = cheque.numchq
          ELSE
               m.numchq = ''
          ENDIF
          SET ORDER TO (vord)
          m.codfte = ordse.codfte
     ENDIF
     SELECT (as)
ENDIF
RETURN
*
FUNCTION valprv
PRIVATE xx, vfun
vfun = .F.
codprv = IIF(EMPTY(codprv),  ;
         codprv,  ;
         PADL(ALLTRIM(codprv), 4,  ;
         '0'))
xx = val_prv(codprv,.T.)
IF xx
     REPLACE codprv WITH  ;
             promae.codprv,  ;
             nompro WITH  ;
             promae.nompro
     RETURN .T.
ENDIF
RETURN vfun
*
PROCEDURE anula
SELECT anupa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado <> '00'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ANULAR esta Parte ?' ;
           )
IF velimina
     IF vopcion = 1
          SELECT orden
          IF RLOCK()
               REPLACE orden.tipalt  ;
                       WITH ' ',  ;
                       orden.numreb  ;
                       WITH  ;
                       '    '
          ENDIF
          IF  .NOT.  ;
              EMPTY(m.numhc)
               SELECT hoja
               SEEK m.perhc +  ;
                    m.numhc
               IF FOUND()
                    IF RLOCK()
                         REPLACE numreb  ;
                                 WITH  ;
                                 '    ',  ;
                                 valreb  ;
                                 WITH  ;
                                 0
                    ENDIF
               ENDIF
          ENDIF
     ELSE
          SELECT ordse
          IF RLOCK()
               REPLACE ordse.tipalt  ;
                       WITH ' ',  ;
                       ordse.numreb  ;
                       WITH  ;
                       '    '
          ENDIF
          IF  .NOT.  ;
              EMPTY(m.numhc)
               SELECT hoja
               SEEK m.perhc +  ;
                    m.numhc
               IF FOUND()
                    IF RLOCK()
                         REPLACE numreb  ;
                                 WITH  ;
                                 '    ',  ;
                                 valreb  ;
                                 WITH  ;
                                 0
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
     SELECT anupa
     IF RLOCK()
          REPLACE estado WITH  ;
                  '99', fecver  ;
                  WITH DATE()
     ENDIF
     DO vista
ENDIF
UNLOCK
RETURN
*
FUNCTION agreg_item
vord = ORDER()
SELECT iteoc
IF f_appd()
     REPLACE numoc WITH m.numoc,  ;
             periodo WITH  ;
             m.periodo, codart  ;
             WITH itepec.codart,  ;
             codcad WITH  ;
             itepec.codcad,  ;
             canreq WITH  ;
             itepec.canreq,  ;
             coduni WITH  ;
             itepec.coduni,  ;
             descri WITH  ;
             itepec.descri,  ;
             preuni WITH  ;
             itepec.preuni
     RETURN .T.
ENDIF
SET ORDER TO vOrd
RETURN .F.
*
FUNCTION marca
vtemp = RECNO()
vorde = itepec.orden
vcodcad = itepec.codcad +  ;
          itepec.codprv
SET FILTER TO itepec.codcad + itepec.codprv;
= vcodcad
GOTO TOP
SCAN
     DO CASE
          CASE vorde = '*'
               IF RLOCK()
                    REPLACE orden  ;
                            WITH  ;
                            '*',  ;
                            estado  ;
                            WITH  ;
                            '30'
               ENDIF
          CASE vorde = ' '
               IF RLOCK()
                    REPLACE orden  ;
                            WITH  ;
                            ' ',  ;
                            estado  ;
                            WITH  ;
                            '20'
               ENDIF
     ENDCASE
ENDSCAN
SET FILTER TO
GOTO vtemp
KEYBOARD CHR(23)
RETURN .T.
*
PROCEDURE lista
SELECT anupa
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     vmensl = IIF(vopcion = 1,  ;
              ' PARTE ANULACION O/C',  ;
              ' PARTE ANULACION O/S' ;
              )
     IF vopcion = 1
          SET ORDER TO 3
     ELSE
          SET ORDER TO 4
     ENDIF
     ACTIVATE WINDOW standby
     vnumpa = m.numpa
     vperio = m.periodo
     @ 1, 1 SAY  ;
       'Ingrese N§ Pte Rebaja : '  ;
       GET vperio
     @ 1, 29 GET vnumpa
     READ
     DEACTIVATE WINDOW standby
     IF LASTKEY() = 27
          RETURN
     ENDIF
     SET FILTER TO periodo = vperio;
.AND. numpa = vnumpa
     IF vopcion = 1
          vnumref = busc_oc()
          DO reporte WITH 2,  ;
             'LisPRC1', vmensl,  ;
             2
     ELSE
          vnumref = busc_os()
          DO reporte WITH 2,  ;
             'LisPRs1', vmensl,  ;
             2
     ENDIF
     SET FILTER TO
ENDIF
SELECT anupa
GOTO vtemp
DO vista
RETURN
*
PROCEDURE termi
ven_accion = .F.
ON KEY LABEL F2
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_c1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION valprv
PRIVATE xx, vfun
vfun = .F.
m.codpr = IIF(EMPTY(m.codprv),  ;
          m.codprv,  ;
          PADL(ALLTRIM(m.codprv),  ;
          4, '0'))
xx = val_prv(m.codprv,.T.)
IF xx
     RETURN .T.
ENDIF
RETURN vfun
*
FUNCTION valpa
PARAMETER vnumpa
PRIVATE vfun
vfun = .T.
m.numpa = PADL(ALLTRIM(STR(vnumpa,  ;
          4)), 4, '0')
IF m.numpa = '0000' .OR.  ;
   EMPTY(m.numpa)
     vfun = .F.
ENDIF
RETURN vfun
*
FUNCTION valart
PARAMETER _cod
PRIVATE xx, vfun
vfun = .F.
xx = val_art(codart,.F.)
IF xx
     SELECT itepec
     REPLACE coduni WITH  ;
             produ.coduni, preuni  ;
             WITH produ.preuni
     vfun = .T.
ENDIF
RETURN vfun
*
PROCEDURE lisser
vorde = ORDER()
DEFINE WINDOW lis FROM 1, 15 TO  ;
       23, 65 FLOAT TITLE  ;
       'Listado Solicitud de Servicios'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro, vtofue
vcli = SPACE(4)
vano = '95'
vfte = '   '
vcodfte = '   '
@ 02, 01 SAY  ;
  '        Total O/C : ' GET  ;
  vtocli SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,7,22)
@ 04, 01 SAY  ;
  '              O/C : '
@ 04, 22 GET vfte PICTURE '!!!'  ;
  VALID val_para(vfte,'CODFTE', ;
  'C') WHEN vtocli = 2
@ 04, 26 GET vano PICTURE '!!'  ;
  WHEN vtocli = 2
@ 04, 28 SAY '-'
@ 04, 29 GET vcli PICTURE '!!!!'  ;
  VALID vo() .AND. valord() WHEN  ;
  vtocli = 2
@ 06, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,11,22)
@ 08, 01 SAY  ;
  '           Fuente : '
@ 08, 22 GET vcodfte PICTURE  ;
  '!!!' VALID val_para(vcodfte, ;
  'CODFTE','C') WHEN vtofue = 2
@ 17, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     vind = SYS(3) + '.IDX'
     SET FILTER TO IIF(vtocli = 1,;
.T., periodo + numoc + codfte = vano +;
vcli + ALLTRIM(vfte))
     SET INDEX TO (vind)
     COUNT ALL TO vtotos
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtippro = 1,  ;
               'Listado Orden Compra',  ;
               IIF(vtippro = 2,  ;
               'Listado Orden de Servicio Pendientes',  ;
               IIF(vtippro = 3,  ;
               'Listado Orden de Servicios Afectados',  ;
               IIF(vtippro = 4,  ;
               'Listado Orden de Servicios Anulados',  ;
               'Listado Orden de Compra Liquidados' ;
               ))))
     IF  .NOT. EOF()
          IF .F.
               DO CASE
                    CASE vlistado =  ;
                         1
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOs1',  ;
                            ' Ordenes de Compra ',  ;
                            2
                    CASE vlistado =  ;
                         2
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrds',  ;
                            ' Ordenes de Compra ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE vlistado =  ;
                         3
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrsX',  ;
                            ' Ordenes de Compra ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
               ENDCASE
          ENDIF
          DO reporte WITH 2,  ;
             'LisPRC1', vmensl
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     CLOSE INDEX
     ERASE (vind)
ENDIF
SELECT ordse
SET ORDER TO 1
RETURN
*
FUNCTION vo
vcli = PADL(ALLTRIM(vcli), 4,  ;
       '0')
RETURN .T.
*
FUNCTION valord
SELECT orden
DEFINE WINDOW windo_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
vtem = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
SEEK vano + vcli + ALLTRIM(vfte)
IF  .NOT. FOUND()
     HIDE MENU mmenu
     ACTIVATE SCREEN
     vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     ON KEY LABEL F10 KEYBOARD CHR(23)
     BROWSE FIELDS numoc :H =  ;
            ' N§ ', est =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'S/Ct',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            ' -  ')))) :H =  ;
            'ESTD', fecoc :H =  ;
            'Fecha', codfte :H =  ;
            'FTE ', observa :H =  ;
            'Descripci¢n' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            windo_0
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     IF LASTKEY() = 27
          GOTO vtemp
     ENDIF
     SHOW MENU mmenu
     ON KEY LABEL F10
ENDIF
vcli = numoc
GOTO vtemp
RETURN .T.
*
FUNCTION busc_oc
PRIVATE qw
qw = ALIAS()
vck = anupa.periodo +  ;
      anupa.numref +  ;
      anupa.codfte
SELECT orden
tot = 0
SEEK vck
IF FOUND()
     tot = orden.valtot
ENDIF
SELECT (qw)
RETURN tot
*
FUNCTION busc_os
PRIVATE qw
qw = ALIAS()
vck = anupa.periodo +  ;
      anupa.numref +  ;
      anupa.codfte
SELECT ordse
tot = 0
SEEK vck
IF FOUND()
     tot = ordse.valtot
ENDIF
SELECT (qw)
RETURN tot
*
PROCEDURE bus
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
IF escolor
     DEFINE POPUP Busmenu FROM 15,54;
 SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP busmenu FROM 15,  ;
            54 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF busmenu PROMPT  ;
       ' por \<Por Total O/C'
DEFINE BAR 2 OF busmenu PROMPT  ;
       ' por \<Por Componente'
ON SELECTION POPUP busmenu DEACTIVATE;
POPUP
ACTIVATE POPUP busmenu
SELECT promae
vtemp = RECNO()
DO CASE
     CASE BAR() = 1
          ACTIVATE WINDOW standby
          STORE '0000' TO vbusca
          @ 1, 2 SAY 'C¢digo: '  ;
            GET vbusca PICTURE  ;
            '!!!!'
          READ
          DEACTIVATE WINDOW  ;
                     standby
          IF LASTKEY() <> 27
               vbusca = UPPER(ALLTRIM(vbusca))
          ENDIF
     CASE BAR() = 2
          ACTIVATE WINDOW standby
          STORE SPACE(40) TO  ;
                vbusca
          @ 1, 1 SAY ' Nombre: '  ;
            GET vbusca PICTURE  ;
            '@S30'
          READ
          DEACTIVATE WINDOW  ;
                     standby
          IF LASTKEY() <> 27
               vbusca = TRIM(UPPER(vbusca))
               SET ORDER TO 2
          ENDIF
     OTHERWISE
          RETURN
ENDCASE
*
FUNCTION corri_hj
vbase = ALIAS()
vregi = RECNO()
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°F10->Terminar°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F8 DO Elimi_Ic
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteoc
SET ORDER TO iteoc1
SEEK m.periodo + m.numref
BROWSE FIELDS codart :H =  ;
       'C¢digo' :V =  ;
       val_artc(codart,.F.) :F :W =  ;
       EMPTY(codart), descri :H =  ;
       'Descripci¢n' : 29 :W =  ;
       .F., canreq :H =  ;
       'Cantidad' :P =  ;
       '99,999.999' :R, coduni :H =  ;
       'Uni' :W = .F. : 3, preuni  ;
       :H = 'PreUni' :P =  ;
       '99,999.999' :R, prereb :H =  ;
       'PreReb' :P = '99,999.999',  ;
       x = ROUND(canreq * preuni,  ;
       3) :H = 'Total' :P =  ;
       '999,999.99' :W = .F., y =  ;
       ROUND(canreq * prereb, 3)  ;
       :H = 'Rebaja' :P =  ;
       '99,999.999' NOMENU  ;
       NOAPPEND NODELETE NOCLEAR  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.numref
SELECT iteoc
SEEK m.periodo + m.numref
vtothoc = 0
vtotreb = 0
SCAN WHILE periodo + numoc =  ;
     m.periodo + m.numref
     REPLACE anttot WITH canreq *  ;
             prereb
     vtothoc = vtothoc + (valtot -  ;
               anttot)
     vtotreb = vtotreb + anttot
     IF  .NOT. EMPTY(numpec)
          = agre_itoc1(m.periodo +  ;
            numpec,codart)
     ENDIF
ENDSCAN
m.valtot = vtotreb
SELECT iteoc
SEEK m.periodo + m.numref
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT orden
IF LASTKEY() = 27
     RETURN .F.
ENDIF
SET RELATION TO
DO trab_hijo
SELECT (vbase)
GOTO vregi
RETURN
*
FUNCTION trab_hijo
as = ALIAS()
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°F5->Agregar°°°°°°°°°°F8->Eliminar°°°°°°°°°°°F10->Terminar°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteoc1
SET ORDER TO ITEOC11
SEEK m.periodo + m.numref
BROWSE FIELDS codcom :H =  ;
       'Componente.' :R, codmet  ;
       :H = 'Meta' :R, codpart :H =  ;
       'Partida' :F, aa =  ;
       IIF(EMPTY(codpart), ' ',  ;
       val_para(RIGHT(codpart, 2), ;
       'ESPGAS','D',28,40)) :H =  ;
       'Descripci¢n' : 40,  ;
       valpart :H = 'Monto ' :P =  ;
       '99,999,999.99' :R, valreb  ;
       :H = 'Rebaja' :P =  ;
       '99,999,999.99' NOMENU  ;
       NOAPPEND NODELETE WINDOW  ;
       wind_2a KEY m.periodo +  ;
       m.numref
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SELECT iteoc1
SEEK m.periodo + m.numref
vtotcom = 0
SCAN WHILE periodo + numref =  ;
     m.periodo + m.numref
     vtotcom = vtotcom + (valpart -  ;
               valreb)
     IF valpart = 0
          DELETE NEXT 1
     ENDIF
ENDSCAN
IF vtotcom <> vtothoc
     DO standby WITH ( ;
        'Total Compon. es diferente al total de  O/C...Revise...' ;
        )
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SET FILTER TO
SELECT (as)
IF LASTKEY() = 27
     RETURN .F.
ENDIF
RETURN .T.
*
PROCEDURE agre_itoc1
PARAMETER vkey, vcodart
vrecno = RECNO()
valias = ALIAS()
SELECT itepec
SEEK vkey
IF FOUND()
     SCAN WHILE periodo + numpec =  ;
          vkey
          IF codart = vcodart
               = val_codcad(ALLTRIM(itepec.codcad), ;
                 m.periodo,'C')
               vuniges = maepre.uniges
               vunieje = maepre.unieje
               vcodcom = maepre.codcom
               vcodmet = maepre.codmet
               SELECT iteoc1
               SET ORDER TO iteoc13
               SEEK m.periodo +  ;
                    m.numref +  ;
                    itepec.codcad +  ;
                    vcodcom +  ;
                    vcodmet
               IF FOUND()
                    REPLACE valreb  ;
                            WITH  ;
                            valreb +  ;
                            iteoc.anttot
               ELSE
                    IF f_appd()
                         REPLACE periodo  ;
                                 WITH  ;
                                 m.periodo,  ;
                                 numoc  ;
                                 WITH  ;
                                 m.numref,  ;
                                 codcad  ;
                                 WITH  ;
                                 itepec.codcad,  ;
                                 uniges  ;
                                 WITH  ;
                                 vuniges,  ;
                                 unieje  ;
                                 WITH  ;
                                 vunieje,  ;
                                 codcom  ;
                                 WITH  ;
                                 vcodcom,  ;
                                 codmet  ;
                                 WITH  ;
                                 vcodmet,  ;
                                 valpart  ;
                                 WITH  ;
                                 iteoc.anttot
                    ENDIF
               ENDIF
          ENDIF
     ENDSCAN
ENDIF
SELECT (valias)
GOTO vrecno
RETURN
*
PROCEDURE elimi_ic
PRIVATE vfun
vfun = .F.
SELECT itepec
SET ORDER TO ITEPEC7
SEEK iteoc.periodo + iteoc.numoc +  ;
     iteoc.codfte + iteoc.codart +  ;
     iteoc.numord
IF FOUND()
     IF RLOCK()
          REPLACE orden WITH ' ',  ;
                  estado WITH  ;
                  '20', numoc  ;
                  WITH '    '
          vfun = .T.
     ENDIF
     UNLOCK
ELSE
     IF m.tipo = 'M'
          vfun = .T.
     ENDIF
ENDIF
SELECT iteoc
IF vfun
     IF RLOCK()
          DELETE NEXT 1
     ENDIF
     UNLOCK
ELSE
     DO standby WITH  ;
        'No se ubic¢ el Pecosa ...'
ENDIF
RETURN
*

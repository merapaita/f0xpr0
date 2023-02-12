PARAMETER vopcion
IF vopcion = 1
     USE IN 1 PteAnu ALIAS anupa  ;
         ORDER PteAnu1
     USE IN 2 OrdCom ALIAS orden  ;
         ORDER OrdCom1
     USE IN 3 IteOc ALIAS iteoc  ;
         ORDER IteOc1
     USE IN 15 IteOc1 ALIAS  ;
         iteoc1 ORDER IteOc11
ELSE
     USE IN 1 PteAnu ALIAS anupa  ;
         ORDER PteAnu2
     USE IN 2 OrdSer ALIAS ordse  ;
         ORDER OrdSer1
     USE IN 3 Solser ALIAS solser  ;
         ORDER Solser1
     USE IN 15 IteOs1 ALIAS  ;
         iteos1 ORDER IteOs11
ENDIF
USE IN 4 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 5 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 6 Itepec ALIAS itepec  ;
    ORDER ItePec4
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
USE IN 13 Cheque ALIAS cheque  ;
    ORDER Cheque1
PUBLIC vmensl
vmensl = IIF(vopcion = 1,  ;
         ' PARTE ANULACION O/C',  ;
         ' PARTE ANULACION O/S')
IF vopcion = 1
     vmens01 = ' Parte Anulaci¢n Orden de Compra : REVISION '
     vmens02 = ' Registro de Parte Anulaci¢n Ordenes de Compra '
     vmens04 = 'Dicho Orden de Compra no fue encontrado'
     vmens05 = 'No existe Orden de Compra anterior'
     vmens06 = 'No existe Orden de Compra siguiente'
     vmens07 = '¨ Desea ANULAR ‚ste Orden de Compra ?'
     vmens08 = 'No hay registros para procesar'
     vmens09 = 'Este Orden de Compra ha sido anulado'
     vmens10 = 'Este Orden de Compra ya fue atendido'
     vmens11 = 'Este Orden de Compra ha sido devuelto'
ELSE
     vmens01 = ' Parte Anulaci¢n Orden de Servicio: REVISION '
     vmens02 = ' Registro de Parte Anulaci¢n Ordenes de Servicio '
     vmens04 = 'Dicho Orden de Servicio no fue encontrado'
     vmens05 = 'No existe Orden de Servicio anterior'
     vmens06 = 'No existe Orden de Servicio siguiente'
     vmens07 = '¨ Desea ANULAR ‚ste Orden de Servicio ?'
     vmens08 = 'No hay registros para procesar'
     vmens09 = 'Este Orden de Servicio ha sido anulado'
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
       ' Detalle: ' DOUBLE COLOR  ;
       SCHEME 10
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
@ 1, 2 SAY '       N£mero P/A :'
@ 2, 2 SAY '        Fecha P/A :'
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
@ 5, 2 SAY '         Importe  :'
@ 6, 2 SAY '      Cadena Fun. :'
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
  'Pendiente   ', IIF(m.estado =  ;
  '20', 'En Cont.Pres.',  ;
  IIF(m.estado = '99',  ;
  'Anulada      ', IIF(m.estado =  ;
  '50', 'Atendido    ',  ;
  '             '))))
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
       'Aten', IIF(estado = '99',  ;
       'Anul', IIF(estado = '50',  ;
       'Liqu', 'Aten')))) :H =  ;
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
PROCEDURE val_rev
PRIVATE vtemp
IF vopcion = 1
     as = ALIAS()
     SELECT orden
     SET FILTER TO estado <> '5'
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
          BROWSE FIELDS numoc :H =  ;
                 ' N§ ', fecoc :H =  ;
                 'Fecha', ess =  ;
                 IIF(estado =  ;
                 '00', 'Pend',  ;
                 IIF(estado =  ;
                 '20', 'Afec.',  ;
                 IIF(estado =  ;
                 '99', 'Anul',  ;
                 IIF(estado =  ;
                 '50', 'Aten',  ;
                 '    ')))) :H =  ;
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
     SET FILTER TO estado <> '5'
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
          IF LASTKEY() = 27
               GOTO TOP
          ENDIF
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
     DO vista
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
FUNCTION marca
vtemp = RECNO()
vorde = itepec.orden
vcodcal = itepec.codcad +  ;
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
     ACTIVATE WINDOW standby
     vnumpa = m.numpa
     vperio = m.periodo
     @ 1, 1 SAY  ;
       'Ingrese N§ Pte Anulaci¢n : '  ;
       GET vperio
     @ 1, 32 GET vnumpa
     READ
     DEACTIVATE WINDOW standby
     IF vopcion = 1
          SET ORDER TO PTEANU1
     ELSE
          SET ORDER TO PTEANU2
     ENDIF
     SET FILTER TO periodo = vperio;
.AND. numpa = vnumpa
     IF vopcion = 1
          DO reporte WITH 2,  ;
             'LisPAC1', vmensl, 2,  ;
             .F., .F.
     ELSE
          DO reporte WITH 2,  ;
             'LisPAs1', vmensl, 2,  ;
             .F., .F.
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

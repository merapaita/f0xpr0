PARAMETER sistema
SET EXCLUSIVE OFF
USE IN 1 OrdCom ALIAS orden ORDER  ;
    OrdCom1
USE IN 2 Iteoc ALIAS iteoc ORDER  ;
    IteOc1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 4 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 5 Pecosa ALIAS pecosa  ;
    ORDER Pecosa1
USE IN 6 Itepec ALIAS itepec  ;
    ORDER ItePec4
USE IN 7 KARDEX ALIAS kardex  ;
    ORDER KARDEX2
USE IN 8 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 9 calen ALIAS cale ORDER  ;
    calen1
USE IN 10 IteArt ALIAS iteart  ;
    ORDER Iteart3
IF sistema = '1'
     USE IN 11 maepre ALIAS  ;
         maepre ORDER maepre1
ELSE
     USE IN 11 maepre ALIAS  ;
         maepre ORDER maepre3
ENDIF
USE IN 12 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 13 IteOC1 ALIAS iteoc1  ;
    ORDER IteOC11
PUBLIC sumatot
vmens01 = ' Gu¡as de Internamiento : REVISION '
vmens02 = ' Registro de Gu¡as de Internamiento '
vmens04 = 'Dicho Gu¡as de Internamiento no fue encontrado'
vmens05 = 'No existe Gu¡as de Internamiento anterior'
vmens06 = 'No existe Gu¡as de Internamiento siguiente'
vmens07 = '¨ Desea ANULAR ‚ste Gu¡as de Internamiento ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Gu¡as de Internamiento ha sido anulado'
vmens10 = 'Este Gu¡as de Internamiento ya fue Contabilizado'
vmens11 = 'Este Gu¡as de Internamiento ha sido devuelto'
SELECT orden
GOTO BOTTOM
PUBLIC vtoprv
SCATTER BLANK MEMVAR
ON KEY LABEL F12 DO CORRI
ON KEY LABEL F6 DO CORRI1
ON KEY LABEL F2 DO FECREP
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  liQuida  Ingresa           Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 13, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 14, 00  ;
       TO 23, 79 TITLE  ;
       ' Detalle: Gu¡as de Internamiento                   ®F9¯ Detalle '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 20, 63  ;
       TO 22, 78 TITLE 'TOTAL '  ;
       COLOR SCHEME 10
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
       'li\<Quida' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT  ;
       '\<Ingresa' AT 24, 45
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Listar' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO liquida
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_1
CLEAR
@ 1, 2 SAY '       N£mero O/C :'
@ 1, 40 SAY 'Fecha Liquidaci¢n :'
@ 2, 2 SAY '        Fecha O/C :'
@ 2, 40 SAY '              H/C :'
@ 3, 2 SAY '        Proveedor :'
@ 4, 2 SAY '      Cod. Cadena :'
@ 5, 2 SAY '         Fte.Fto. :'
@ 6, 2 SAY '          Funci¢n :'
@ 7, 2 SAY '         Programa :'
@ 8, 2 SAY '      Subprograma :'
@ 9, 2 SAY '  Activ./Proyect. :'
@ 10, 2 SAY '          Destino :'
@ 11, 2 SAY '    Observaciones :'
RETURN
*
PROCEDURE vista
SELECT orden
ON KEY LABEL F9 DO VISTA_DET
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
@ 0, 60 SAY vestoc(m.estado)
@ 1, 22 SAY m.periodo
@ 1, 25 SAY m.numoc
@ 1, 60 SAY m.fecdesp
@ 2, 22 SAY m.fecoc
@ 2, 60 SAY m.perhc + ' ' +  ;
  m.numhc
@ 3, 22 SAY val_prv(m.codprv)
@ 4, 22 SAY val_codcad(m.codcad, ;
  m.periodo,'D',22,30)
@ 5, 22 SAY val_para(m.codfte, ;
  'CODFTE','D',22,30)
@ 6, 22 SAY  ;
  val_para(maepre.codfun,'CODFUN', ;
  'V',22,40)
@ 7, 22 SAY  ;
  val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'V',22, ;
  40)
@ 8, 22 SAY  ;
  val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'V',22, ;
  40)
@ 9, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 10, 22 SAY m.destino PICTURE  ;
  '@S56'
@ 11, 22 SAY m.observa
DO vista_hijo
DO total
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT iteoc
BROWSE NOOPTIMIZE FIELDS numpec  ;
       :H = 'Pecs' :W = .F.,  ;
       coduni :H = 'Uni' :W = .F.  ;
       : 2, descri :H =  ;
       'Descripci¢n' : 20 :W =  ;
       .F., preuni :H = 'PreUni'  ;
       :P = '99,999.99' :W = .F.,  ;
       x = ROUND(canreq * preuni,  ;
       5) :H = 'Total' :P =  ;
       '9,999,999.99' :W = .F.,  ;
       canreq :H = 'Cantd' :P =  ;
       '99,999.99', candesp :H =  ;
       'Despd' :P = '99,999.99',  ;
       xx = IIF(EMPTY(fecdesp),  ;
       'Sin Ingr', fecdesp) :H =  ;
       'Atendido' NOMENU NOAPPEND  ;
       NOEDIT NODELETE NOCLEAR  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.numoc  ;
       TIMEOUT 0.001  NOREFRESH
SELECT orden
RETURN
*
PROCEDURE vista_det
HIDE POPUP ALL
SELECT iteoc
BROWSE NOOPTIMIZE FIELDS numpec  ;
       :H = 'Pecs' :W = .F.,  ;
       coduni :H = 'Uni' :W = .F.  ;
       : 2, descri :H =  ;
       'Descripci¢n' : 20, preuni  ;
       :H = 'PreUni' :P =  ;
       '99,999.99' :W = .F., x =  ;
       ROUND(canreq * preuni, 5)  ;
       :H = 'Total' :P =  ;
       '9.999,999.99' :W = .F.,  ;
       canreq :H = 'Cantd' :P =  ;
       '99,999.99', candesp :H =  ;
       'Despd' :P = '99,999.99',  ;
       fecdesp :H = 'Atendido'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_2 KEY m.periodo +  ;
       m.numoc +  ;
       ALLTRIM(m.codfte)  ;
       NOREFRESH
SELECT orden
RETURN
*
PROCEDURE ingre
IF estado = '99'
     DO standby WITH  ;
        'La O/C ya est  anulada'
     DO vista
     RETURN
ENDIF
IF estado = '00'
     DO standby WITH  ;
        'La O/C no esta afectada'
     DO vista
     RETURN
ENDIF
IF estado = '50'
     DO standby WITH  ;
        'La O/C ya est  Liquidada'
     DO vista
     RETURN
ENDIF
DEACTIVATE WINDOW wind_3
SELECT orden
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
vfecdesp = DATE()
IF LASTKEY() <> 27
     DO WHILE .T.
          ok = ingresa_hi()
          IF LASTKEY() <> 27  ;
             .AND. ok
               IF yesno( ;
                  '¨ Confirme el ingreso ?' ;
                  )
                    EXIT
               ENDIF
          ELSE
               IF yesno( ;
                  '¨ Cancela el ingreso ?' ;
                  )
                    ok = .F.
                    EXIT
               ENDIF
          ENDIF
     ENDDO
     IF ok .AND. LASTKEY() <> 27
          SELECT iteoc
          SET ORDER TO ITEOC2
          SEEK m.periodo +  ;
               m.numoc
          vig = 0
          ves = 0
          vfec = iteoc.fecdesp
          entro = .T.
          SCAN WHILE periodo +  ;
               numoc = m.periodo +  ;
               m.numoc
               IF iteoc.candesp1 =  ;
                  0
                    ves = ves + 1
               ELSE
                    SELECT kardex
                    SEEK iteoc.periodo +  ;
                         iteoc.numoc +  ;
                         iteoc.codfte +  ;
                         iteoc.codart
                    IF FOUND()
                         IF RLOCK()
                              REPLACE  ;
                               tipdoc  ;
                               WITH  ;
                               'O/C',  ;
                               numdoc  ;
                               WITH  ;
                               m.numoc,  ;
                               fecdoc  ;
                               WITH  ;
                               m.fecoc,  ;
                               codart  ;
                               WITH  ;
                               iteoc.codart,  ;
                               fecent  ;
                               WITH  ;
                               iteoc.fecdesp,  ;
                               coddep  ;
                               WITH  ;
                               iteoc.coddep,  ;
                               codcad  ;
                               WITH  ;
                               iteoc.codcad,  ;
                               entcan  ;
                               WITH  ;
                               iteoc.candesp1,  ;
                               preunie  ;
                               WITH  ;
                               iteoc.preuni,  ;
                               entimp  ;
                               WITH  ;
                               iteoc.preuni *  ;
                               kardex.entcan,  ;
                               horent  ;
                               WITH  ;
                               TIME(),  ;
                               periodo  ;
                               WITH  ;
                               m.periodo,  ;
                               codfte  ;
                               WITH  ;
                               m.codfte,  ;
                               codcad  ;
                               WITH  ;
                               m.codcad,  ;
                               estado  ;
                               WITH  ;
                               '  '
                         ENDIF
                    ELSE
                         IF f_appd()
                              REPLACE  ;
                               tipdoc  ;
                               WITH  ;
                               'O/C',  ;
                               numdoc  ;
                               WITH  ;
                               m.numoc,  ;
                               fecdoc  ;
                               WITH  ;
                               m.fecoc,  ;
                               codart  ;
                               WITH  ;
                               iteoc.codart,  ;
                               fecent  ;
                               WITH  ;
                               iteoc.fecdesp,  ;
                               coddep  ;
                               WITH  ;
                               iteoc.coddep,  ;
                               codcad  ;
                               WITH  ;
                               iteoc.codcad,  ;
                               entcan  ;
                               WITH  ;
                               iteoc.candesp1,  ;
                               preunie  ;
                               WITH  ;
                               iteoc.preuni,  ;
                               entimp  ;
                               WITH  ;
                               iteoc.preuni *  ;
                               kardex.entcan,  ;
                               horent  ;
                               WITH  ;
                               TIME(),  ;
                               periodo  ;
                               WITH  ;
                               m.periodo,  ;
                               codfte  ;
                               WITH  ;
                               m.codfte,  ;
                               codcad  ;
                               WITH  ;
                               m.codcad
                         ENDIF
                         UNLOCK
                    ENDIF
                    SELECT iteoc
                    IF RLOCK()
                         REPLACE candesp  ;
                                 WITH  ;
                                 candesp +  ;
                                 candesp1,  ;
                                 estado  ;
                                 WITH  ;
                                 IIF(candesp =  ;
                                 canreq,  ;
                                 '50',  ;
                                 '00'),  ;
                                 candesp1  ;
                                 WITH  ;
                                 0
                    ENDIF
                    IF (candesp <  ;
                       canreq)
                         vig = vig +  ;
                               1
                    ENDIF
                    IF iteoc.fecdesp >=  ;
                       vfec
                    ENDIF
               ENDIF
               entro = .T.
          ENDSCAN
          SELECT orden
          IF entro
               IF vig = 0 .AND.  ;
                  ves = 0
                    m.estado = IIF(m.estado =  ;
                               '51',  ;
                               '51',  ;
                               '40')
               ENDIF
               GATHER MEMVAR
          ELSE
               DO standby WITH  ;
                  ' La O/C no se pudo atender '
          ENDIF
          IF sistema = '1'
               USE IN 11 maepre  ;
                   ALIAS maepre  ;
                   ORDER maepre1
          ELSE
               USE IN 11 maepre  ;
                   ALIAS maepre  ;
                   ORDER maepre3
          ENDIF
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT iteoc
SET ORDER TO 1
SELECT orden
DO pantalla
DO vista
RETURN
*
PROCEDURE ingresa_hi
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°Presione ®F10¯ para salir grabando o  ®Esc¯ para cancelar°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
HIDE POPUP ALL
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteoc
SET ORDER TO 1
SET FILTER TO estado = '00';
.OR. estado = '95'
IF EOF()
     DO standby WITH  ;
        'La O/C ya est  atendida'
     SET ORDER TO 1
     DO vista
     RETURN
ENDIF
GOTO TOP
BROWSE NOOPTIMIZE FIELDS coduni  ;
       :H = 'Uni' :W = .F. : 3,  ;
       descri :H = 'Descripci¢n'  ;
       : 22 :W = .F., preuni :H =  ;
       'PreUni' :P = '99,999.99'  ;
       :W = .F., x = ROUND(canreq *  ;
       preuni, 5) :H = 'Total' :P =  ;
       '9,999,999.99' :W = .F.,  ;
       canreq :H = 'Cantd' :P =  ;
       '99,999.99' :W = .F.,  ;
       candesp1 :H = 'Despd' :P =  ;
       '99,999.99' :V = val_can()  ;
       :F, fecdesp :H =  ;
       'Atendido' :W = candesp1 >  ;
       0 NOMENU NOAPPEND NODELETE  ;
       NOCLEAR WINDOW wind_2 KEY  ;
       m.periodo + m.numoc  ;
       NOREFRESH
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SET FILTER TO
SELECT orden
ON KEY LABEL F10
RETURN
*
FUNCTION val_can
PRIVATE vfun
vfun = .T.
vresta = canreq - candesp
IF candesp1 > vresta
     DO standby WITH  ;
        'Se esta exediendo en '+ ;
        ALLTRIM(STR(candesp1- ;
        vresta, 10))
     vfun = .F.
ENDIF
RETURN vfun
*
PROCEDURE liquida
SELECT orden
vre = RECNO()
SCATTER MEMVAR
DO CASE
     CASE m.estado = '99'
          DO standby WITH  ;
             'La O/C ya est  anulada'
          DO vista
          RETURN
     CASE m.estado = '50'
          DO standby WITH  ;
             'La O/C ya est  liquidada'
          DO vista
          RETURN
     CASE m.estado = '51'
          DO standby WITH  ;
             'La O/C ya est  contabilizada'
          DO vista
          RETURN
     CASE m.estado = '00'
          DO standby WITH  ;
             'Todav¡a no esta atendida'
          DO vista
          RETURN
     CASE m.estado = '20'
          DO standby WITH  ;
             'Todav¡a no esta despachada'
          DO vista
          RETURN
ENDCASE
DEACTIVATE WINDOW wind_3
@ 1, 60 GET m.fecdesp
READ VALID val_read()
IF LASTKEY() <> 27
     m.estado = IIF(m.estado =  ;
                '51', '51',  ;
                '50')
     m.tipdoc = 'OK'
     GATHER MEMVAR
ENDIF
DO vista
RETURN
*
PROCEDURE revis
SELECT orden
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SET RELATION TO periodo + numoc + codfte;
INTO iteoc
SET SKIP TO iteoc
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS numoc :H = ' N§ ',  ;
       codfte :H = 'Fte', fecoc  ;
       :H = 'Fecha', ess =  ;
       vestoc(iteoc.estado) : 5  ;
       :H = 'Estd', iteoc.descri  ;
       :H = 'Articulo ' : 30,  ;
       iteoc.coduni :H = 'Unid',  ;
       iteoc.canreq :H = 'Cantid',  ;
       iteoc.candesp :H =  ;
       'Despac' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SET RELATION TO
DO vista
RETURN
*
FUNCTION val_fec
IF EMPTY(fecdesp)
     IF RLOCK()
          REPLACE fecdesp WITH  ;
                  DATE()
     ELSE
          RETURN .F.
     ENDIF
ENDIF
RETURN .T.
*
PROCEDURE total
ACTIVATE WINDOW wind_3
IF  .NOT. EMPTY(m.numanu) .OR.   ;
    .NOT. EMPTY(m.numreb)
     @ 0, 0 SAY m.anultot PICTURE  ;
       '9,999,999.99'
ELSE
     @ 0, 0 SAY m.valtot PICTURE  ;
       '9,999,999.99'
ENDIF
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnum_oc = SPACE(4)
vfte = SPACE(2)
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero O/C : ' GET  ;
  vperiodo PICTURE '!!'
@ 1, 26 SAY '-' GET vnum_oc  ;
  PICTURE '!!!!' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_oc) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vperiodo + vnum_oc
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
vnum_oc = PADL(ALLTRIM(vnum_oc),  ;
          4, '0')
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
PROCEDURE corri
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SELECT orden
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@ 1, 60 GET m.fecdesp
READ VALID val_read()
IF LASTKEY() <> 27
     SELECT orden
     GATHER MEMVAR
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT orden
DO pantalla
DO vista
RETURN
*
FUNCTION avalprv
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
PROCEDURE lista
SELECT orden
SET RELATION TO periodo + numoc + codfte;
INTO iteoc
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lisord
ENDIF
SELECT orden
SET RELATION TO
SET ORDER TO 1
GOTO vtemp
DO vista
RETURN
*
PROCEDURE lisord
vorde = ORDER()
DEFINE WINDOW lis FROM 0, 15 TO  ;
       24, 65 FLOAT TITLE  ;
       'Listado Ordenes de Compra'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro, vlistado, vtofue,  ;
      vtoprv
vcli = SPACE(4)
vano = '99'
vfte = '  '
vcodfte = '  '
vcodprv = '    '
vfecini = DATE()
vfecfin = DATE()
@ 01, 01 SAY  ;
  '     Tipo Listado : ' GET  ;
  vlistado FUNCTION  ;
  '^ Detallado;Resumido'
@ 05, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,07,22)
@ 07, 01 SAY  ;
  '           Fuente : '
@ 07, 22 GET vcodfte PICTURE '!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  'C') WHEN vtofue = 2
@ 09, 01 SAY  ;
  'Todos los Proveed : ' GET  ;
  vtoprv SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtoprv,11,22)
@ 11, 01 SAY  ;
  '      Proveedores : '
@ 11, 22 GET vcodprv PICTURE  ;
  '!!!!' VALID valprv() WHEN  ;
  vtoprv = 2
@ 13, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Numero;Proveedor;Emision;Fuente'
@ 16, 01 SAY  ;
  '           Estado : ' GET  ;
  vtippro FUNCTION  ;
  '^ Todos;Pendientes;Atendidos;Afectados;Anulados;Liquidados'
@ 20, 01 SAY  ;
  '           Fechas : ' GET  ;
  vfecini PICTURE '@D' WHEN  ;
  vtippro = 6
@ 20, 32 GET vfecfin PICTURE '@D'  ;
  WHEN vtippro = 6
@ 22, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     vind = SYS(3) + '.IDX'
     INDEX ON IIF(vorden = 1,  ;
           numoc, IIF(vorden = 2,  ;
           codprv, IIF(vorden = 3,  ;
           DTOS(fecemi), codfte +  ;
           numoc))) TO (vind) FOR  ;
           IIF(vtippro = 1, .T.,  ;
           IIF(vtippro = 2,  ;
           estado = '00',  ;
           IIF(vtippro = 3,  ;
           estado = '40',  ;
           IIF(vtippro = 4,  ;
           estado = '20',  ;
           IIF(vtippro = 5,  ;
           estado = '99', estado =  ;
           '50'))))) .AND.  ;
           IIF(vtofue = 1, .T.,  ;
           codfte =  ;
           ALLTRIM(vcodfte))
     SET FILTER TO IIF(vtoprv = 1,;
.T., codprv = ALLTRIM(vcodprv));
.AND. IIF(vtippro = 6, BETWEEN(fecdesp,;
vfecini, vfecfin),;
.T.)
     SET INDEX TO (vind)
     COUNT ALL TO vtotoc
     SUM valtot - anultot TO  ;
         sumatot
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtippro = 1,  ;
               'Listado Orden Compra',  ;
               IIF(vtippro = 2,  ;
               'Listado Orden de Compra Pendientes',  ;
               IIF(vtippro = 3,  ;
               'Listado de Ordenes de Compra Atendidas',  ;
               IIF(vtippro = 4,  ;
               'Listado Orden de Compra Afectadas',  ;
               IIF(vtippro = 4,  ;
               'Listado Orden de Compra Anuladas',  ;
               'Listado Orden de Compra Liquidadas' ;
               )))))
     SET MEMOWIDTH TO 43
     IF  .NOT. EOF()
          SET SKIP TO iteoc
          IF vtippro <> 6
               DO CASE
                    CASE vlistado =  ;
                         1
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrdc1',  ;
                            ' Ordenes de Compra ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE vlistado =  ;
                         2
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrdX1',  ;
                            ' Ordenes de Compra ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
               ENDCASE
          ELSE
               DO CASE
                    CASE vlistado =  ;
                         1
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrdc',  ;
                            ' Ordenes de Compra ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE vlistado =  ;
                         2
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrdX',  ;
                            ' Ordenes de Compra ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
               ENDCASE
          ENDIF
          SELECT orden
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     CLOSE INDEX
     ERASE (vind)
ENDIF
RETURN
*
PROCEDURE alista
SELECT orden
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     SET RELATION TO periodo + numoc INTO;
iteoc
     SET SKIP TO iteoc
     DO reporte WITH 2, 'LisOc',  ;
        ' Gu¡as de Internamiento '
     SET RELATION TO
ENDIF
SELECT orden
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
ON KEY LABEL F9
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RELEASE WINDOW wind_3
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION valprv
PRIVATE xx
vfun = .F.
vcodprv = IIF(EMPTY(vcodprv),  ;
          vcodprv,  ;
          PADL(ALLTRIM(vcodprv),  ;
          4, '0'))
xx = val_prv(vcodprv,.T.)
IF xx
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION valoc
PARAMETER vnumoc
PRIVATE vfun
vfun = .T.
m.numoc = PADL(ALLTRIM(STR(vnumoc,  ;
          4)), 4, '0')
IF m.numoc = '0000' .OR.  ;
   EMPTY(m.numoc)
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
FUNCTION fecrep
SET FUNCTION F3 TO DTOC(fecdesp)
RETURN fecdesp
*
PROCEDURE corri1
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SELECT orden
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@ 5, 22 GET m.codfte
READ VALID val_read()
IF LASTKEY() <> 27
     SELECT orden
     GATHER MEMVAR
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
SELECT iteoc
SEEK m.periodo + m.numoc
SET FILTER TO periodo = m.periodo;
.AND. numoc = m.numoc
GOTO TOP
SCAN WHILE periodo + numoc =  ;
     m.periodo + m.numoc
     REPLACE codfte WITH m.codfte  ;
             ALL
ENDSCAN
UNLOCK ALL
SELECT orden
DO pantalla
DO vista
RETURN
*

SET EXCLUSIVE OFF
USE IN 1 OrdCom ALIAS orden ORDER  ;
    OrdCom1
USE IN 2 IteOc ALIAS iteoc ORDER  ;
    IteOc1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 4 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 5 NOTALM ALIAS nota ORDER  ;
    NOTALM1
USE IN 6 Itepec ALIAS itepec  ;
    ORDER ItePec1
USE IN 7 AuxCot ALIAS auxcot  ;
    ORDER AuxCot1
USE IN 8 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 9 ITEALM ALIAS itealm  ;
    ORDER ITEALM1
USE IN 10 Iteart ALIAS iteart  ;
    ORDER iteart1
USE IN 11 Poliza ALIAS poliza  ;
    ORDER poliza1
USE IN 12 Itepol ALIAS itepol  ;
    ORDER itepol1
USE IN 13 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 14 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 15 Asiaut ALIAS asig ORDER  ;
    asiaut1
USE IN 16 Parkar ALIAS parkar  ;
    ORDER parkar1
USE IN 19 AstOrd ALIAS astord  ;
    ORDER AstOrd1
USE IN 20 Cuentas ALIAS cuen  ;
    ORDER Cuentas1
USE IN 0 IteUsuOp ALIAS subop  ;
    ORDER IteUsuOp1
vmens01 = ' P¢lizas de Entrada: REVISION '
vmens02 = 'Registro de P¢liza de Entrada'
vmens04 = 'Dicho P¢liza no fue encontrado'
vmens05 = 'No existe P¢liza anterior'
vmens06 = 'No existe P¢liza siguiente'
vmens07 = '¨ Desea Anular ‚sta P¢liza ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta P¢liza ha sido anulado'
vmens10 = 'La P¢liza ya est  Atendido'
vmens11 = 'La P¢liza ha sido devuelto'
vmens12 = 'La P¢liza ya tiene O/C'
SELECT poliza
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Elimina Ingresa            Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 09, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 10, 00  ;
       TO 23, 79 TITLE  ;
       ' Detalle: P¢liza         ®F9¯ Detalle : Item '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 20, 65  ;
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
DEFINE PAD elimi OF mmenu PROMPT  ;
       '\<Elimina' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT  ;
       '\<Ingresa' AT 24, 45
DEFINE PAD anula OF mmenu PROMPT  ;
       'An\<ula  ' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Listar ' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD elimi OF mmenu DO elimi
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD anula OF mmenu DO Anula
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_1
CLEAR
@ 1, 2 SAY '        * Periodo :'
@ 2, 2 SAY '          Emisi¢n :'
@ 3, 2 SAY '    N£mero P¢liza :'
@ 4, 2 SAY ' F.Financiamiento :'
@ 5, 2 SAY '   N£mero de Gu¡a :'
@ 6, 2 SAY '         Vigencia :'
@ 7, 2 SAY '         Analisis :'
RETURN
*
PROCEDURE vista
SELECT poliza
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
ON KEY LABEL F9 DO vista_det
SCATTER MEMVAR
@ 0, 60 SAY vestpol(m.estado)
@ 1, 22 SAY m.periodo
@ 2, 22 SAY m.fecemi
@ 3, 22 SAY m.numpol
@ 4, 22 SAY val_para(m.codfte, ;
  'CODFTE','V',22,30)
@ 5, 22 SAY m.numgui
@ 6, 22 SAY m.fecini
@ 6, 36 SAY m.fecfin
@ 7, 22 SAY val_para(m.analisis, ;
  'ANALIS','V',22,15,3)
DO vista_hijo
DO total
IF  .NOT. vflag $ 'J*'
     DO subopc
ENDIF
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT itepol
GOTO TOP
DO CASE
     CASE ALLTRIM(m.analisis) =  ;
          'G'
          BROWSE NOOPTIMIZE  ;
                 FIELDS despro :H =  ;
                 'P' : 1, tipref  ;
                 :H = 'REF',  ;
                 perref :H = 'Pr',  ;
                 numref :H =  ;
                 'N§Rf', fecref  ;
                 :H = 'FecRef',  ;
                 codgen :H = 'GG',  ;
                 descri :H =  ;
                 'Definici¢n' :  ;
                 36, igv :H =  ;
                 'IGV' :P =  ;
                 '99,999.99',  ;
                 valtot :H =  ;
                 'Total' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE NOCLEAR  ;
                 WINDOW wind_2  ;
                 KEY m.periodo +  ;
                 m.numpol TIMEOUT  ;
                 0.001   ;
                 NOREFRESH
     CASE ALLTRIM(m.analisis) =  ;
          'C'
          BROWSE NOOPTIMIZE  ;
                 FIELDS despro :H =  ;
                 'P' : 1, tipref  ;
                 :H = 'REF',  ;
                 perref :H = 'Pr',  ;
                 numref :H =  ;
                 'N§Rf', fecref  ;
                 :H = 'FecRef',  ;
                 codgen :H = 'GG',  ;
                 codcla :H =  ;
                 'Clf', descri :H =  ;
                 'Definici¢n' :  ;
                 33, igv :H =  ;
                 'IGV' :P =  ;
                 '99,999.99',  ;
                 valtot :H =  ;
                 'Total' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE NOCLEAR  ;
                 WINDOW wind_2  ;
                 KEY m.periodo +  ;
                 m.numpol TIMEOUT  ;
                 0.001   ;
                 NOREFRESH
     CASE ALLTRIM(m.analisis) =  ;
          'A'
          BROWSE NOOPTIMIZE  ;
                 FIELDS despro :H =  ;
                 'P' : 1, tipref  ;
                 :H = 'REF',  ;
                 perref :H = 'Pr',  ;
                 numref :H =  ;
                 'N§Rf', codcad  ;
                 :H = 'Cadena',  ;
                 fecref :H =  ;
                 'FecRef', codgen  ;
                 :H = 'GG',  ;
                 codcla :H =  ;
                 'Clf', coddet :H =  ;
                 'Det', descri :H =  ;
                 'Definici¢n' :  ;
                 29, igv :H =  ;
                 'IGV' :P =  ;
                 '99,999.99',  ;
                 valtot :H =  ;
                 'Total' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE NOCLEAR  ;
                 WINDOW wind_2  ;
                 KEY m.periodo +  ;
                 m.numpol TIMEOUT  ;
                 0.001   ;
                 NOREFRESH
ENDCASE
SELECT poliza
RETURN
*
PROCEDURE total
ACTIVATE WINDOW wind_3
@ 0, 0 SAY m.valtot PICTURE  ;
  '9,999,999.99'
RETURN
*
PROCEDURE vista_det
HIDE POPUP ALL
ON KEY LABEL F4 DO AgrIGV
ON KEY LABEL F9
ON KEY LABEL F10 KEYBOARD(CHR(23))
SELECT itepol
GOTO TOP
BROWSE NOOPTIMIZE FIELDS despro  ;
       :H = 'P' : 1, tipref :H =  ;
       'REF', perref :H = 'Pr',  ;
       numref :H = 'N§Rf', fecref  ;
       :H = 'FecRef', codgen :H =  ;
       'GG', descri :H =  ;
       'Definici¢n' : 36, igv :H =  ;
       'IGV' :P = '99,999.99',  ;
       valtot :H = 'Total',  ;
       dcuenta :H = 'Al Debe' :V =  ;
       val_cta('D') :F, hcuenta  ;
       :H = 'Al Haber' :V =  ;
       val_cta('H') :F NOMENU  ;
       NOAPPEND NODELETE NOCLEAR  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.numpol  ;
       NOREFRESH
SELECT poliza
ON KEY LABEL F9 DO vista_det   
ON KEY LABEL F10
SELECT itepol
SEEK m.periodo + m.numpol
IF FOUND()
     m.valtot = 0
     SCAN WHILE m.periodo =  ;
          periodo .AND. m.numpol =  ;
          numpol
          m.valtot = m.valtot +  ;
                     valtot
     ENDSCAN
ENDIF
SELECT poliza
REPLACE valtot WITH m.valtot
DO asiord
ON KEY LABEL F4
RETURN
*
PROCEDURE revis
SELECT poliza
PRIVATE vtemp
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF LASTKEY() = 27
     SELECT poliza
     DO vista
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS numpol :H = ' N§ ',  ;
       fecemi :H = 'Fecha', est =  ;
       IIF(estado = '00', 'Pend',  ;
       IIF(estado = '51', 'Cont',  ;
       '    ')) :H = 'Estd',  ;
       codfte :H = 'Fte ', fecini  ;
       :H = 'Del :', fecfin :H =  ;
       ' al :', fecemi :H =  ;
       'Emision', valtot :H =  ;
       'Total' :P =  ;
       '9,999,999.99' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELECT poliza
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnum_pol = '0000'
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero P¢liza: ' GET  ;
  vperiodo PICTURE '!!'
@ 1, 27 SAY '-' GET vnum_pol  ;
  PICTURE '!!!!'
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_pol) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vperiodo +  ;
          ALLTRIM(vnum_pol)
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
SELECT poliza
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
SELECT poliza
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
PROCEDURE ingre
PRIVATE vre
DO pantalla
SELECT poliza
vre = RECNO()
SCATTER BLANK MEMVAR
STORE DATE() TO m.fecini,  ;
      m.fecfin, m.fecemi
m.periodo = RIGHT(DTOC(DATE()),  ;
            2)
= repasa()
@ 1, 22 SAY m.periodo
@ 2, 22 GET m.fecemi
@ 3, 22 GET m.numpol DISABLE  ;
  VALID vnumpol()
@ 3, 22 SAY m.numpol COLOR  ;
  'GBR+/R'
@ 4, 22 GET m.codfte PICTURE '!!'  ;
  VALID val_para(m.codfte, ;
  'CODFTE',' ',22,30)
@ 5, 22 GET m.numgui
@ 6, 22 GET m.fecini
@ 6, 36 GET m.fecfin
@ 7, 22 GET m.analisis PICTURE  ;
  '!' VALID val_para(m.analisis, ;
  'ANALIS',' ',22,15,3)
READ VALID val_read()
IF LASTKEY() <> 27
     SELECT parma
     SEEK 'CORRELPOLENT'
     correl = nument + 1
     REPLACE nument WITH correl
ENDIF
SELECT poliza
IF LASTKEY() <> 27
     DO WHILE .T.
          ok = ingresa_h()
          IF LASTKEY() <> 27  ;
             .AND. ok
               IF yesno( ;
                  '¨ Confirme el ingreso ?' ;
                  )
                    EXIT
               ENDIF
          ELSE
               DO standby WITH  ;
                  ' Cancelado el Ingreso ...'
               ok = .F.
               EXIT
          ENDIF
     ENDDO
     IF ok .AND. LASTKEY() <> 27
          SELECT itepol
          SEEK m.periodo +  ;
               m.numpol
          vtotalz = 0
          SCAN WHILE  ;
               itepol.periodo +  ;
               itepol.numpol =  ;
               m.periodo +  ;
               m.numpol
               vtotalz = vtotalz +  ;
                         valtot
               SELECT orden
               SEEK m.periodo +  ;
                    itepol.numref +  ;
                    m.codfte
               IF RLOCK()
               ENDIF
               UNLOCK
          ENDSCAN
          SELECT poliza
          m.valtot = vtotalz
          m.tippol = 'E'
          m.estado = '00'
          IF f_appd()
               GATHER MEMVAR
          ENDIF
          SELECT itepol
          SEEK m.periodo +  ;
               m.numpol
          IF FOUND()
               xnumr = itepol.numref
               SELECT orden
               SEEK m.periodo +  ;
                    xnumr
               REPLACE docref  ;
                       WITH  ;
                       'Pol: ' +  ;
                       m.numpol
          ENDIF
          DO asiord
     ELSE
          SELECT itepol
          SEEK m.periodo +  ;
               m.numpol
          IF FOUND()
               SCAN WHILE  ;
                    itepol.periodo +  ;
                    itepol.numpol =  ;
                    m.periodo +  ;
                    m.numpol
                    IF RLOCK()
                         DELETE NEXT  ;
                                1
                    ENDIF
               ENDSCAN
               UNLOCK
          ENDIF
          SELECT poliza
          DO standby WITH  ;
             ' No se pudo crear la Poliza '
          IF  .NOT. EOF()
               GOTO vre
          ENDIF
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
     SELECT poliza
     GOTO vre
ENDIF
SELECT poliza
DO vista
RETURN
*
FUNCTION ingresa_h
PRIVATE ok
ON KEY LABEL F4 DO AgrIGV
ACTIVATE SCREEN
HIDE MENU mmenu
ACTIVATE WINDOW standby
xcolor = '*' + SET('COLOR')
@ 01,06 SAY 'Espere un momento ... Reporte en proceso';
COLOR &xcolor
ok = pasa()
DEACTIVATE WINDOW standby
vtempo = '°°°°°°°°°°Presione ®F10¯ para salir grabando o  ®Esc¯ para cancelar°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
IF (ok)
     SELECT itepol
     SEEK m.periodo + m.numpol
     DO CASE
          CASE ALLTRIM(m.analisis) =  ;
               'G'
               BROWSE FIELDS  ;
                      despro :H =  ;
                      IIF(m.tippol =  ;
                      'E', 'Des',  ;
                      'Prc'),  ;
                      tipref :H =  ;
                      'Ref' :W =  ;
                      .F., perref  ;
                      :H = 'Mes'  ;
                      :W = .F.,  ;
                      numref :H =  ;
                      'N§O/C' :W =  ;
                      .F., codcad  ;
                      :H =  ;
                      'Cadena' :W =  ;
                      .F., codgen  ;
                      :H = 'GG'  ;
                      :W = .F.,  ;
                      descri :H =  ;
                      'Descripci¢n'  ;
                      :W = .F. :  ;
                      35, igv :H =  ;
                      'IGV' :P =  ;
                      '99,999.99',  ;
                      valtot :H =  ;
                      'Total',  ;
                      dcuenta :H =  ;
                      'Al Debe'  ;
                      :V =  ;
                      val_cta('D')  ;
                      :F, hcuenta  ;
                      :H =  ;
                      'Al Haber'  ;
                      :V =  ;
                      val_cta('H')  ;
                      :F NOMENU  ;
                      NOAPPEND  ;
                      NODELETE  ;
                      WINDOW  ;
                      wind_2 KEY  ;
                      m.periodo +  ;
                      m.numpol
          CASE ALLTRIM(m.analisis) =  ;
               'C'
               BROWSE FIELDS  ;
                      despro :H =  ;
                      IIF(m.tippol =  ;
                      'E', 'Des',  ;
                      'Prc'),  ;
                      tipref :H =  ;
                      'Ref' :W =  ;
                      .F., perref  ;
                      :H = 'Mes'  ;
                      :W = .F.,  ;
                      numref :H =  ;
                      'N§O/C' :W =  ;
                      .F., codcad  ;
                      :H =  ;
                      'Cadena' :W =  ;
                      .F., codgen  ;
                      :H = 'GG'  ;
                      :W = .F.,  ;
                      codcla :H =  ;
                      'Clf' :W =  ;
                      .F., descri  ;
                      :H =  ;
                      'Descripci¢n'  ;
                      :W = .F. :  ;
                      32, igv :H =  ;
                      'IGV' :P =  ;
                      '99,999.99',  ;
                      valtot :H =  ;
                      'Total',  ;
                      dcuenta :H =  ;
                      'Al Debe'  ;
                      :V =  ;
                      val_cta('D')  ;
                      :F, hcuenta  ;
                      :H =  ;
                      'Al Haber'  ;
                      :V =  ;
                      val_cta('H')  ;
                      :F NOMENU  ;
                      NOAPPEND  ;
                      NODELETE  ;
                      WINDOW  ;
                      wind_2 KEY  ;
                      m.periodo +  ;
                      m.numpol
          CASE ALLTRIM(m.analisis) =  ;
               'A'
               BROWSE FIELDS  ;
                      despro :H =  ;
                      IIF(m.tippol =  ;
                      'E', 'Des',  ;
                      'Prc'),  ;
                      tipref :H =  ;
                      'Ref' :W =  ;
                      .F., perref  ;
                      :H = 'Mes'  ;
                      :W = .F.,  ;
                      numref :H =  ;
                      'N§O/C' :W =  ;
                      .F., codcad  ;
                      :H =  ;
                      'Cadena' :W =  ;
                      .F., codgen  ;
                      :H = 'GG'  ;
                      :W = .F.,  ;
                      codcla :H =  ;
                      'Clf' :W =  ;
                      .F., coddet  ;
                      :H = 'Det'  ;
                      :W = .F.,  ;
                      descri :H =  ;
                      'Descripci¢n'  ;
                      :W = .F. :  ;
                      29, igv :H =  ;
                      'IGV' :P =  ;
                      '99,999.99',  ;
                      valtot :H =  ;
                      'Total',  ;
                      dcuenta :H =  ;
                      'Al Debe'  ;
                      :V =  ;
                      val_cta('D')  ;
                      :F, hcuenta  ;
                      :H =  ;
                      'Al Haber'  ;
                      :V =  ;
                      val_cta('H')  ;
                      :F NOMENU  ;
                      NOAPPEND  ;
                      NODELETE  ;
                      WINDOW  ;
                      wind_2 KEY  ;
                      m.periodo +  ;
                      m.numpol
     ENDCASE
     ON KEY LABEL F10
     ACTIVATE SCREEN
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     SHOW MENU mmenu
     SELECT poliza
ELSE
     DO standby WITH  ;
        ' No existe Registros a Procesar ...'
     ok = .F.
ENDIF
ON KEY LABEL F4
RETURN (ok)
*
PROCEDURE agrigv
IF EMPTY(igv)
     REPLACE igv WITH valtot -  ;
             ROUND(valtot / 1.18 ,  ;
             2)
ELSE
     REPLACE igv WITH 0
ENDIF
RETURN
*
FUNCTION pasa
PRIVATE vfun, malias
malias = ALIAS()
SELECT iteoc
sdx = SYS(3) + '.IDX'
SET RELATION TO periodo + numoc + codfte;
INTO orden
INDEX ON periodo + numoc + codfte +  ;
      codart TO (sdx) FOR codfte =  ;
      ALLTRIM(m.codfte) .AND.  ;
      BETWEEN(orden.fecvis,  ;
      m.fecini, m.fecfin) .AND.  ;
      EMPTY(orden.docref) .AND.  ;
      perpec <> 'LI'
GOTO TOP
IF EOF()
     vfun = .F.
ELSE
     DO CASE
          CASE ALLTRIM(m.analisis) =  ;
               'G'
               SCAN
                    vtot = 0
                    vtot = vtot +  ;
                           candesp *  ;
                           preuni
                    DO agreg_pol  ;
                       WITH  ;
                       'O/C'
                    SELECT iteoc
               ENDSCAN
               vfun = .T.
          CASE ALLTRIM(m.analisis) =  ;
               'C'
               SCAN
                    vtot = 0
                    vtot = vtot +  ;
                           candesp *  ;
                           preuni
                    DO agreg_pol  ;
                       WITH  ;
                       'O/C'
                    SELECT iteoc
               ENDSCAN
               vfun = .T.
          CASE ALLTRIM(m.analisis) =  ;
               'A'
               SCAN
                    vtot = 0
                    IF estado =  ;
                       '50'
                         vtot = vtot +  ;
                                valtot
                    ELSE
                         vtot = vtot +  ;
                                valtot
                    ENDIF
                    DO agreg_pol  ;
                       WITH  ;
                       'O/C'
                    SELECT iteoc
               ENDSCAN
               vfun = .T.
     ENDCASE
ENDIF
SELECT (malias)
RETURN vfun
*
FUNCTION agreg_pol
PARAMETER vtipdoc
PRIVATE as
as = ALIAS()
SELECT itepol
IF vtipdoc = 'O/C'
     IF f_appd()
          REPLACE periodo WITH  ;
                  m.periodo,  ;
                  numpol WITH  ;
                  m.numpol,  ;
                  tippol WITH 'E',  ;
                  tipref WITH  ;
                  vtipdoc, codcad  ;
                  WITH  ;
                  ALLTRIM(iteoc.codcad),  ;
                  perref WITH  ;
                  orden.nummes,  ;
                  numref WITH  ;
                  iteoc.numoc,  ;
                  codgen WITH  ;
                  LEFT(iteoc.codart,  ;
                  2), codcla WITH  ;
                  SUBSTR(iteoc.codart,  ;
                  4, 3), coddet  ;
                  WITH  ;
                  SUBSTR(iteoc.codart,  ;
                  8, 3), valtot  ;
                  WITH vtot,  ;
                  nummeshc WITH  ;
                  orden.perhc,  ;
                  numhc WITH  ;
                  orden.numhc,  ;
                  codprv WITH  ;
                  orden.codprv,  ;
                  destino WITH  ;
                  orden.destino,  ;
                  estado WITH  ;
                  '00', fecref  ;
                  WITH  ;
                  orden.fecdesp,  ;
                  despro WITH '1',  ;
                  numpec WITH  ;
                  iteoc.numpec,  ;
                  descri WITH  ;
                  IIF(ALLTRIM(m.analisis) =  ;
                  'G',  ;
                  SUBSTR(val_para('B' +  ;
                  ALLTRIM(LEFT(iteoc.codart,  ;
                  2)),'CODGEB', ;
                  'D',22,60), 4,  ;
                  100),  ;
                  IIF(ALLTRIM(m.analisis) =  ;
                  'C', buscart(),  ;
                  val_artc('B' +  ;
                  iteoc.codart)))
          DO ubicta
     ELSE
          SELE &AS
          RETURN .T.
     ENDIF
ENDIF
UNLOCK
SELE &AS
RETURN .T.
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
FUNCTION vnumpol
m.numpol = PADL(ALLTRIM(m.numpol),  ;
           4, '0')
SEEK m.periodo + m.numpol
IF  .NOT. FOUND()
     RETURN .T.
ELSE
     DO standby WITH  ;
        'La poliza ya se encuentra registrada..!'
     RETURN .F.
ENDIF
*
FUNCTION vnumpol1
IF  .NOT. EMPTY(vnumpol)
     vnumpol = PADL(ALLTRIM(vnumpol),  ;
               4, '0')
     SEEK m.periodo + vnumpol
     IF FOUND()
          RETURN .T.
     ELSE
          DO standby WITH  ;
             'La poliza NO se encuentra registrada'
          RETURN .F.
     ENDIF
ELSE
     vnumpol = m.numpol
     RETURN .T.
ENDIF
*
PROCEDURE elimi
SELECT poliza
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado = '50'
     DO standby WITH  ;
        'La P¢liza ya est  Liquidada'
     RETURN
ELSE
     IF estado = '51'
          DO standby WITH  ;
             'La P¢liza ya est  Contabilizada'
          RETURN
     ENDIF
ENDIF
PRIVATE ok
ok = ve_passw('CHINA')
IF ok
     velimina = yesno( ;
                '¨ Desea Eliminar ‚sta P¢liza ?' ;
                )
     IF velimina
          SELECT itepol
          SEEK m.periodo +  ;
               m.numpol
          SCAN WHILE m.periodo =  ;
               periodo .AND.  ;
               m.numpol = numpol
               SELECT orden
               SEEK m.periodo +  ;
                    itepol.numref
               REPLACE tipdoc  ;
                       WITH '  ',  ;
                       docref  ;
                       WITH  ;
                       '          ',  ;
                       fecvis  ;
                       WITH {}
               SELECT itepol
               IF RLOCK()
                    DELETE NEXT 1
               ENDIF
          ENDSCAN
          SELECT poliza
          IF RLOCK()
               DELETE NEXT 1
          ENDIF
          IF EOF()
               SKIP -1
          ENDIF
     ENDIF
     DO vista
     UNLOCK ALL
ENDIF
RETURN
*
PROCEDURE anula
nreg = RECNO()
IF estado = '99'
     DO standby WITH  ;
        'La poliza ya esta Anulada'
     DO vista
     RETURN
ENDIF
IF yesno(vmens07)
     SELECT itepol
     SEEK poliza.periodo +  ;
          poliza.numpol
     IF FOUND()
          REPLACE estado WITH  ;
                  '99' WHILE  ;
                  poliza.periodo +  ;
                  poliza.numpol =  ;
                  itepol.periodo +  ;
                  itepol.numpol
     ENDIF
     SEEK poliza.periodo +  ;
          poliza.numpol
     SELECT poliza
     REPLACE estado WITH '99'
     SELECT orden
     SEEK m.periodo +  ;
          itepol.numref
     REPLACE tipdoc WITH '  ',  ;
             docref WITH  ;
             '          ', fecvis  ;
             WITH {}
     SELECT poliza
     DO vista
     DO standby WITH vmens09
ENDIF
RETURN
*
PROCEDURE liquida
SELECT poliza
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
DO CASE
     CASE estado = '50'
          DO standby WITH  ;
             'La P¢liza ya est  Liquidada'
          RETURN
     CASE estado = '51'
          DO standby WITH  ;
             'La P¢liza ya est  Contabilizada'
          RETURN
ENDCASE
vliquida = yesno( ;
           '¨ Seguro de LIQUIDAR ‚sta P¢liza ?' ;
           )
IF vliquida
     ACTIVATE WINDOW wind_1
     SELECT itepol
     SEEK m.periodo + m.numpol
     SCAN WHILE m.periodo =  ;
          periodo .AND. m.numpol =  ;
          numpol
          IF RLOCK()
               REPLACE estado  ;
                       WITH '50'
          ENDIF
     ENDSCAN
     SELECT orden
     SEEK m.periodo +  ;
          itepol.numref +  ;
          m.codfte
     IF RLOCK()
          REPLACE orden.estado  ;
                  WITH '50'
     ENDIF
     SELECT poliza
     IF RLOCK()
          REPLACE estado WITH  ;
                  '50'
     ENDIF
ENDIF
DO vista
UNLOCK ALL
RETURN
*
PROCEDURE lista
USE IN 5
USE IN 7
USE IN 9
USE IN 10
USE IN 14
USE IN 15
USE IN 16
USE IN 17 itehc ALIAS itehc ORDER  ;
    itehc1
USE IN 18 HOJCON ALIAS hoja ORDER  ;
    HOJCON1
SELECT poliza
SET FILTER TO
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lispol
ENDIF
USE IN 5 NOTALM ALIAS nota ORDER  ;
    NOTALM1
USE IN 7 AuxCot ALIAS auxcot  ;
    ORDER AuxCot1
USE IN 9 ITEALM ALIAS itealm  ;
    ORDER ITEALM1
USE IN 10 Iteart ALIAS iteart  ;
    ORDER iteart1
USE IN 14 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 15 Asiaut ALIAS asig ORDER  ;
    asiaut1
USE IN 16 Parkar ALIAS parkar  ;
    ORDER parkar1
USE IN 17
USE IN 18
SELECT poliza
SET RELATION TO
SET FILTER TO
DO vista
RETURN
*
PROCEDURE lispol
PRIVATE vorde
vorde = ORDER()
vrec = RECNO()
DEFINE WINDOW lis FROM 3, 15 TO  ;
       20, 65 FLOAT TITLE  ;
       'Listado Polizas' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtopol, vtomes, vtofue,  ;
      vtodep, vorden, vtiplis
vnumpol = SPACE(4)
vfte = SPACE(2)
vcodmes = SPACE(2)
vcoddep = SPACE(6)
vcodfte = SPACE(3)
@ 01, 01 SAY  ;
  'Todas las Polizas : ' GET  ;
  vtopol SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtopol,3,22)
@ 03, 01 SAY  ;
  '           Poliza : '
@ 03, 22 GET vnumpol PICTURE  ;
  '!!!!' VALID vnumpol1() WHEN  ;
  vtopol = 2
@ 05, 01 SAY  ;
  '  Todos las Meses : ' GET  ;
  vtomes SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtomes,6,22) WHEN vtopol =  ;
  1
@ 06, 01 SAY  ;
  '              Mes : '
@ 06, 22 GET vcodmes PICTURE '!!'  ;
  VALID val_para(vcodmes,'FECMES', ;
  'C') WHEN vtopol = 1 .AND.  ;
  vtomes = 2
@ 08, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtofue,9,22) WHEN vtopol =  ;
  1
@ 09, 01 SAY  ;
  '           Fuente : '
@ 09, 22 GET vcodfte PICTURE '!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  'C') WHEN vtopol = 1 .AND.  ;
  vtofue = 2
@ 11, 01 SAY  ;
  '           Estado : ' GET  ;
  vtiplis FUNCTION  ;
  '^ Todos;Pendientes;Atendidos'  ;
  WHEN vtopol = 1
@ 14, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1 .AND. LASTKEY() <>  ;
   27
     SET RELATION TO periodo + numpol;
INTO itepol
     SET SKIP TO itepol
     SELECT itepol
     SET RELATION TO nummeshc + numhc;
INTO hoja ADDITIVE
     SELECT poliza
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     vind = SYS(3) + '.IDX'
     INDEX ON periodo +  ;
           itepol.numref +  ;
           itepol.codgen TO  ;
           (vind) FOR IIF(vtopol =  ;
           1, .T., numpol +  ;
           codfte = vnumpol +  ;
           ALLTRIM(vfte)) .AND.  ;
           IIF(vtiplis = 1, .T.,  ;
           IIF(vtiplis = 2,  ;
           estado = '00', estado =  ;
           '50')) .AND.  ;
           IIF(vtomes = 1, .T.,  ;
           MONTH(fecemi) =  ;
           VAL(vcodmes)) .AND.  ;
           IIF(vtofue = 1, .T.,  ;
           codfte =  ;
           ALLTRIM(vcodfte))
     SET FILTER TO tippol = 'E'
     SET INDEX TO (vind)
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtiplis = 1,  ;
               ' en General ',  ;
               IIF(vtiplis = 2,  ;
               ' Pendientes ',  ;
               ' Atendidos '))
     IF  .NOT. EOF()
          IF vtopol = 2
               DO CASE
                    CASE ALLTRIM(m.analisis) =  ;
                         'G'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'polent1',  ;
                            ' Poliza de Entrada',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE ALLTRIM(m.analisis) =  ;
                         'C'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'polent_c',  ;
                            ' Poliza de Entrada',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE ALLTRIM(m.analisis) =  ;
                         'A'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'polent1',  ;
                            ' Poliza de Entrada',  ;
                            1,  ;
                            .F.,  ;
                            .T.
               ENDCASE
          ELSE
               DO reporte WITH 2,  ;
                  'Polent',  ;
                  ' Poliza de Entrada ',  ;
                  1, .F., .T.
          ENDIF
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     CLOSE INDEX
     ERASE (vind)
ENDIF
SELECT poliza
SET ORDER TO (vorde)
GOTO TOP
GOTO vrec
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RELEASE WINDOW wind_3
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION valpec
PARAMETER vnumpec
PRIVATE vfun
vfun = .T.
m.numpec = PADL(ALLTRIM(STR(vnumpec,  ;
           4)), 4, '0')
IF m.numpec = '0000' .OR.  ;
   EMPTY(m.numpec)
     vfun = .F.
ENDIF
RETURN vfun
*
FUNCTION buscart
PRIVATE vfun
as = ALIAS()
SELECT produ
SET ORDER TO 1
SEEK 'B' + LEFT(iteoc.codart, 6)
IF  .NOT. FOUND()
     vfun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
ELSE
     vfun = UPPER(produ.descri)
ENDIF
SELECT (as)
RETURN vfun
*
FUNCTION val_artc
PARAMETER xcod
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
_campo = VARREAD()
malias = ALIAS()
SELECT iteart
GOTO TOP
_oldwnd = WOUTPUT()
v_fun = '*'
SEEK xcod
v_fun = IIF(FOUND(), descri, '*')
SELECT itepol
IF RLOCK()
     REPLACE descri WITH  ;
             iteart.descri
ENDIF
SELECT (malias)
RETURN v_fun
*
PROCEDURE ubicta
PRIVATE as
as = ALIAS()
SELECT asig
SEEK 'P/E' + '1  ' + 'B' +  ;
     LEFT(iteoc.codart, 6)
SELECT itepol
IF FOUND()
     REPLACE itepol.dcuenta WITH  ;
             asig.dcuenta,  ;
             itepol.hcuenta WITH  ;
             asig.hcuenta
ELSE
     REPLACE itepol.dcuenta WITH  ;
             '         ',  ;
             itepol.hcuenta WITH  ;
             '         '
ENDIF
SELECT (as)
RETURN
*
FUNCTION valanul
PRIVATE as
vh = itepol.periodo +  ;
     itepol.numref
as = ALIAS()
SELECT orden
SEEK vh
IF FOUND()
     vtot = orden.anultot
ELSE
     vtot = 0
ENDIF
SELECT (as)
RETURN vtot
*
FUNCTION buscatg
PARAMETER clavehc
valias = SELECT()
SELECT itehc
IF SEEK(clavehc)
     vcatgas = LEFT(itehc.codpart,  ;
               1)
ELSE
     vcatgas = ' '
ENDIF
SELECT (valias)
RETURN vcatgas
*
PROCEDURE asiord
SELECT asig
SEEK 'P/E' + '   ' + 'ASTORD'
IF  .NOT. FOUND()
     DO standby WITH  ;
        'PARAMETRO DE CTAS. DE ORDEN INICIALIZADO, CONSULTE AL AREA DE SISTEMAS'
     RETURN
ELSE
     cctad = dcuenta
     cctah = hcuenta
ENDIF
SELECT astord
cnummes = PADL(MONTH(m.fecini), 2,  ;
          '0')
SEEK m.periodo + cnummes +  ;
     m.numpol + 'P/E'
IF FOUND()
     FOR i = 1 TO 2
          IF f_lock(1) .OR.  ;
             RLOCK()
               REPLACE periodo  ;
                       WITH  ;
                       m.periodo,  ;
                       nummes  ;
                       WITH  ;
                       cnummes,  ;
                       numref  ;
                       WITH  ;
                       m.numpol,  ;
                       tipdoc  ;
                       WITH 'P/E',  ;
                       fecha WITH  ;
                       m.fecini,  ;
                       codcta  ;
                       WITH IIF(i =  ;
                       1, cctad,  ;
                       cctah),  ;
                       tipcta  ;
                       WITH IIF(i =  ;
                       1, 'D',  ;
                       'H'),  ;
                       mtodeb  ;
                       WITH IIF(i =  ;
                       1,  ;
                       m.valtot,  ;
                       0), mtohab  ;
                       WITH IIF(i =  ;
                       2,  ;
                       m.valtot,  ;
                       0)
               UNLOCK
               SKIP
          ENDIF
     ENDFOR
ELSE
     FOR i = 1 TO 2
          IF f_appd()
               REPLACE periodo  ;
                       WITH  ;
                       m.periodo,  ;
                       nummes  ;
                       WITH  ;
                       cnummes,  ;
                       numref  ;
                       WITH  ;
                       m.numpol,  ;
                       tipdoc  ;
                       WITH 'P/E',  ;
                       fecha WITH  ;
                       m.fecini,  ;
                       codcta  ;
                       WITH IIF(i =  ;
                       1, cctad,  ;
                       cctah),  ;
                       tipcta  ;
                       WITH IIF(i =  ;
                       1, 'D',  ;
                       'H'),  ;
                       mtodeb  ;
                       WITH IIF(i =  ;
                       1,  ;
                       m.valtot,  ;
                       0), mtohab  ;
                       WITH IIF(i =  ;
                       2,  ;
                       m.valtot,  ;
                       0)
               UNLOCK
          ENDIF
     ENDFOR
ENDIF
DEFINE WINDOW wastord FROM 10, 10  ;
       TO 15, 70 TITLE  ;
       ' ASIENTOS DE ORDEN' COLOR  ;
       SCHEME 02
ACTIVATE WINDOW wastord
@ 00, 08 SAY 'Cuentas '
@ 00, 18 SAY '        Debe '
@ 00, 34 SAY '        Haber '
@ 01, 04 SAY cctad PICTURE  ;
  '!!!!!!!!!!!'
@ 01, 18 SAY m.valtot PICTURE  ;
  '99,999,999.99'
@ 02, 12 SAY cctah PICTURE  ;
  '!!!!!!!!!!!'
@ 02, 34 SAY m.valtot PICTURE  ;
  '99,999,999.99'
WAIT ' '
DEACTIVATE WINDOW wastord
RELEASE WINDOW wastord
RETURN
*
FUNCTION repasa
vfun = .T.
vrec = RECNO()
vali = ALIAS()
SELECT parma
SEEK 'CORRELPOLENT'
newpol = nument + 1
m.numpol = PADL(ALLTRIM(STR(newpol)),  ;
           4, '0')
SELECT (vali)
RETURN vfun
*
FUNCTION val_cta
PARAMETER ctipo
xfield = IIF(ctipo = 'D',  ;
         'DCuenta', 'HCuenta')
IF !EMPTY(&xField)
     IF seek(&xField,'Cuen')
     ELSE
          REPLACE &xField WITH SPACE(10)
     ENDIF
     xCta = Val_Fun('Cuen','Cuenta',"Cuenta+' '+Descri",&xField,2)
     IF cuen.detalle <> 'S'
          = standby( ;
            'La Cuenta No es de Detalle. Por Favor Intente de Nuevo' ;
            )
          RETURN .F.
     ENDIF
ENDIF
RETURN .T.
*
PROCEDURE subopc
PRIVATE calias, cmod
calias = ALIAS()
SELECT subop
cctrlop = ''
cmod = '03'
SEEK vusucla + PADL(sistctrl, 2,  ;
     '0') + cmod
IF FOUND()
     SCAN WHILE vusucla +  ;
          PADL(sistctrl, 2, '0') +  ;
          cmod =  ;
          ALLTRIM(subop.user) +  ;
          subop.sistema +  ;
          subop.modulo
          cctrlop = cctrlop +  ;
                    subop.opcion
     ENDSCAN
ENDIF
SET SKIP OF PAD revis OF mmenu;
 .NOT. 'A' $ cctrlop
SET SKIP OF PAD busca OF mmenu;
 .NOT. 'B' $ cctrlop
SET SKIP OF PAD anter OF mmenu;
 .NOT. 'C' $ cctrlop
SET SKIP OF PAD proxi OF mmenu;
 .NOT. 'D' $ cctrlop
SET SKIP OF PAD elimi OF mmenu;
 .NOT. 'E' $ cctrlop
SET SKIP OF PAD ingre OF mmenu;
 .NOT. 'F' $ cctrlop
SET SKIP OF PAD anula OF mmenu;
 .NOT. 'G' $ cctrlop
SET SKIP OF PAD lista OF mmenu;
 .NOT. 'H' $ cctrlop
SELECT (calias)
RETURN
*

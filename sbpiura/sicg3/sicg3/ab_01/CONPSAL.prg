SET EXCLUSIVE OFF
USE IN 1 OrdCom ALIAS orden ORDER  ;
    OrdCom2
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 4 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 5 Pecosa ALIAS pecosa  ;
    ORDER Pecosa1
USE IN 6 Itepec ALIAS itepec  ;
    ORDER ItePec1
USE IN 8 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 9 calen ALIAS calen ORDER  ;
    calen1
USE IN 10 Iteart ALIAS iteart  ;
    ORDER iteart1
USE IN 11 Poliza ALIAS poliza  ;
    ORDER poliza2
USE IN 12 Itepol ALIAS itepol  ;
    ORDER itepol2
USE IN 13 Asiaut ALIAS asig ORDER  ;
    asiaut1
USE IN 14 Parkar ALIAS parkar  ;
    ORDER parkar1
vmens01 = ' P¢lizas de Salida : REVISION '
vmens02 = 'Registro de P¢liza de Salida'
vmens04 = 'Dicho P¢liza no fue encontrado'
vmens05 = 'No existe P¢liza anterior'
vmens06 = 'No existe P¢liza siguiente'
vmens07 = '¨ Desea Anular ‚sta P¢liza ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta P¢liza ha sido anulado'
vmens10 = 'La P¢liza ya est  Atendido'
vmens11 = 'La P¢liza ha sido devuelto'
vmens12 = 'La P¢liza ya tiene O/C'
ON KEY LABEL F7 DO VISTA_COR
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Elimina  Ingresa  Anular   Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 09, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 10, 00  ;
       TO 23, 79 TITLE  ;
       ' Detalle: P¢liza    ®F9¯ Detalle : Item  ®F7¯ Verifica Precio'  ;
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
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Listar ' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_1
CLEAR
@ 1, 2 SAY '          Periodo :'
@ 2, 2 SAY '          Emisi¢n :'
@ 3, 2 SAY '    N£mero P¢liza :'
@ 4, 2 SAY '         Fte.Fto. :'
@ 5, 2 SAY '         Vigencia :'
@ 6, 2 SAY '         Analisis :'
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
  'CODFTE','V',22,20,4)
@ 5, 22 SAY m.fecini
@ 5, 38 SAY m.fecfin
@ 6, 22 SAY val_para(m.analisis, ;
  'ANALIS','V',22,15,3)
DO vista_hijo
DO total
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
                 'D' : 1, tipref  ;
                 :H = 'REF',  ;
                 perref :H = 'Pr',  ;
                 numref :H =  ;
                 'N§Rf', fecref  ;
                 :H = 'FecRef',  ;
                 codgen :H = 'GG',  ;
                 descri :H =  ;
                 'Definici¢n' :  ;
                 36, valtot :H =  ;
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
                 33, valtot :H =  ;
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
          SEEK m.periodo +  ;
               m.numpol
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
                 29, valtot :H =  ;
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
PROCEDURE vista_cor
HIDE POPUP ALL
SELECT itepol
ON KEY LABEL F7
GOTO TOP
DO CASE
     CASE ALLTRIM(m.analisis) =  ;
          'G'
          BROWSE NOOPTIMIZE  ;
                 FIELDS despro :H =  ;
                 'D' : 1 :W = .F.,  ;
                 tipref :H =  ;
                 'REF' :W = .F.,  ;
                 perref :H = 'Pr'  ;
                 :W = .F., numref  ;
                 :H = 'N§Rf' :W =  ;
                 .F., fecref :H =  ;
                 'FecRef' :W =  ;
                 .F., codgen :H =  ;
                 'GG' :W = .F.,  ;
                 descri :H =  ;
                 'Definici¢n' :  ;
                 36 :W = .F.,  ;
                 valtot :H =  ;
                 'Total' NOMENU  ;
                 NOAPPEND  ;
                 NODELETE NOCLEAR  ;
                 WINDOW wind_2  ;
                 KEY m.periodo +  ;
                 m.numpol  ;
                 NOREFRESH
     CASE ALLTRIM(m.analisis) =  ;
          'C'
          BROWSE NOOPTIMIZE  ;
                 FIELDS despro :H =  ;
                 'P' : 1 :W = .F.,  ;
                 tipref :H =  ;
                 'REF' :W = .F.,  ;
                 perref :H = 'Pr'  ;
                 :W = .F., numref  ;
                 :H = 'N§Rf' :W =  ;
                 .F., fecref :H =  ;
                 'FecRef' :W =  ;
                 .F., codgen :H =  ;
                 'GG' :W = .F.,  ;
                 codcla :H =  ;
                 'Clf' :W = .F.,  ;
                 descri :H =  ;
                 'Definici¢n' :  ;
                 33 :W = .F.,  ;
                 valtot :H =  ;
                 'Total' NOMENU  ;
                 NOAPPEND  ;
                 NODELETE NOCLEAR  ;
                 WINDOW wind_2  ;
                 KEY m.periodo +  ;
                 m.numpol  ;
                 NOREFRESH
     CASE ALLTRIM(m.analisis) =  ;
          'A'
          BROWSE NOOPTIMIZE  ;
                 FIELDS despro :H =  ;
                 'P' : 1 :W = .F.,  ;
                 tipref :H =  ;
                 'REF' :W = .F.,  ;
                 perref :H = 'Pr'  ;
                 :W = .F., numref  ;
                 :H = 'N§Rf' :W =  ;
                 .F., fecref :H =  ;
                 'FecRef' :W =  ;
                 .F., codgen :H =  ;
                 'GG' :W = .F.,  ;
                 codcla :H =  ;
                 'Clf' :W = .F.,  ;
                 coddet :H =  ;
                 'Det' :W = .F.,  ;
                 descri :H =  ;
                 'Definici¢n' :  ;
                 29 :W = .F.,  ;
                 valtot :H =  ;
                 'Total' NOMENU  ;
                 NOAPPEND  ;
                 NODELETE NOCLEAR  ;
                 WINDOW wind_2  ;
                 KEY m.periodo +  ;
                 m.numpol  ;
                 NOREFRESH
ENDCASE
SELECT itepol
SEEK m.periodo + m.numpol
vtotalz = 0
SCAN WHILE itepol.periodo +  ;
     itepol.numpol = m.periodo +  ;
     m.numpol
     vtotalz = vtotalz + valtot
ENDSCAN
SELECT poliza
REPLACE valtot WITH vtotalz
ON KEY LABEL F7 DO VISTA_COR
DO vista
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
ON KEY LABEL F9
SELECT itepol
GOTO TOP
BROWSE NOOPTIMIZE FIELDS despro  ;
       :H = 'D' : 1, tipref :H =  ;
       'REF', perref :H = 'Pr',  ;
       numref :H = 'N§Rf', fecref  ;
       :H = 'FecRef', codgen :H =  ;
       'GG', descri :H =  ;
       'Definici¢n' : 36, valtot  ;
       :H = 'Total' NOMENU  ;
       NOAPPEND NODELETE NOCLEAR  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.numpol  ;
       NOREFRESH
SELECT poliza
ON KEY LABEL F9 DO vista_det   
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
PROCEDURE lista
PRIVATE vtemp
SELECT poliza
vtemp = RECNO()
SET RELATION TO periodo + numpol INTO;
itepol
SET SKIP TO itepol
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lispol WITH vtemp
ENDIF
SELECT poliza
SET RELATION TO
SET FILTER TO
GOTO TOP
DO vista
RETURN
*
PROCEDURE lispol
PARAMETER vreg
PRIVATE vorde
vorde = ORDER()
vrec = RECNO()
DEFINE WINDOW lis FROM 2, 15 TO  ;
       23, 65 FLOAT TITLE  ;
       'Listado Polizas' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtopol, vtomes, vtofue,  ;
      vtodep, vorden, vtiplis
vnumpol = SPACE(4)
vfte = SPACE(2)
vcodmes = SPACE(2)
vcoddep = SPACE(6)
vcodfte = SPACE(2)
vtiprep = 1
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
  '      Pecosa tipo : ' GET  ;
  vtiprep FUNCTION  ;
  '^ Todos;Compra;Stock' WHEN  ;
  vtopol = 2
@ 08, 01 SAY  ;
  '  Todos las Meses : ' GET  ;
  vtomes SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtomes,6,22) WHEN vtopol =  ;
  1
@ 09, 01 SAY  ;
  '              Mes : '
@ 09, 22 GET vcodmes PICTURE '!!'  ;
  VALID val_para(vcodmes,'FECMES', ;
  'C') WHEN vtopol = 1 .AND.  ;
  vtomes = 2
@ 11, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtofue,9,22) WHEN vtopol =  ;
  1
@ 12, 01 SAY  ;
  '           Fuente : '
@ 12, 22 GET vcodfte PICTURE '!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  'C') WHEN vtopol = 1 .AND.  ;
  vtofue = 2
@ 14, 01 SAY  ;
  '           Estado : ' GET  ;
  vtiplis FUNCTION  ;
  '^ Todos;Pendientes;Atendidos'  ;
  WHEN vtopol = 1
@ 18, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1 .AND. LASTKEY() <>  ;
   27
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
           VAL(vcodmes))
     SET INDEX TO (vind)
     SET FILTER TO tippol = 'S';
.AND. IIF(vtofue = 1,;
.T., codfte = ALLTRIM(vcodfte));
.AND. IIF(vtiprep = 1,;
.T., IIF(vtiprep = 2, itepol.tippec =;
'O', itepol.tippec = 'S'))
     SELECT itepol
     GOTO TOP
     SELECT poliza
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtiplis = 1,  ;
               ' en General ',  ;
               IIF(vtiplis = 2,  ;
               ' Pendientes ',  ;
               ' Atendidos '))
     IF  .NOT. EOF()
          IF vtopol = 2
               DO CASE
                    CASE ALLTRIM(analisis) =  ;
                         'G'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'polsal1',  ;
                            ' Poliza de Salida',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE ALLTRIM(analisis) =  ;
                         'C'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'polsal_c',  ;
                            ' Poliza de Salida',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE ALLTRIM(analisis) =  ;
                         'A'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'polsal_a',  ;
                            ' Poliza de Salida',  ;
                            1,  ;
                            .F.,  ;
                            .T.
               ENDCASE
          ELSE
               DO reporte WITH 2,  ;
                  'Polsal',  ;
                  ' Poliza de Salida ',  ;
                  1, .F., .T.
          ENDIF
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     CLOSE INDEX
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
SELECT (malias)
RETURN v_fun
*
FUNCTION buscart
PRIVATE vfun
as = ALIAS()
SELECT produ
SET ORDER TO 1
SEEK 'B' + LEFT(itepec.codart, 6)
IF  .NOT. FOUND()
     vfun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
ELSE
     vfun = UPPER(produ.descri)
ENDIF
SELECT (as)
RETURN vfun
*
PROCEDURE ubicta
PRIVATE as
as = ALIAS()
SELECT asig
SEEK '08' + '1  ' + 'B' +  ;
     LEFT(itepec.codart, 6)
IF FOUND()
     REPLACE itepol.dcuenta WITH  ;
             asig.dcuenta,  ;
             itepol.hcuenta WITH  ;
             asig.hcuenta
ELSE
     REPLACE itepol.dcuenta WITH  ;
             '     *    ',  ;
             itepol.hcuenta WITH  ;
             '     *    '
ENDIF
SELECT (as)
RETURN
*
FUNCTION buscoc
PRIVATE ali, vfun
ali = ALIAS()
vk = itepol.numoc
SELECT orden
SEEK vk
IF FOUND()
     vfun = vk + '.' +  ;
            PADL(ALLTRIM(STR(MONTH(fecdesp),  ;
            2)), 2, '0')
ELSE
     vfun = vk + '.' + '??'
ENDIF
RETURN vfun
*

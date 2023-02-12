PARAMETER key_docfte, sistema
IF PARAMETERS() = 0
     key_docfte = ''
ENDIF
USE IN 1 OrdCom ALIAS orden ORDER  ;
    OrdCom1
USE IN 2 IteOc ALIAS iteoc ORDER  ;
    IteOc1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 4 IteArt ALIAS iteart  ;
    ORDER Iteart3
USE IN 5 Pecosa ALIAS pecosa  ;
    ORDER Pecosa1
USE IN 6 Itepec ALIAS itepec  ;
    ORDER ItePec1
USE IN 7 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 8 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 13 IteOc1 ALIAS iteoc1  ;
    ORDER Iteoc11
USE IN 20 USUARIO ALIAS usu ORDER  ;
    USUARIO1
ON KEY LABEL F4 do imprimir
PUBLIC vmes, vpart, vnumoc, oq,  ;
       vms, vfecvis, v_reg,  ;
       vtothoc, vcadena, docbusc
PRIVATE vtotoc
xtotoc = 0
vmens01 = ' Orden de Compra : REVISION '
vmens02 = ' Registro de Ordenes de Compra '
vmens04 = 'Dicha Orden de Compra no fue encontrado'
vmens05 = 'No existe Orden de Compra anterior'
vmens06 = 'No existe Orden de Compra siguiente'
vmens07 = '¨ Desea ANULAR ‚ste Orden de Compra ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Orden de Compra ha sido anulado'
vmens10 = 'Este Orden de Compra ya fue atendido'
vmens11 = 'Este Orden de Compra ha sido devuelto'
docbusc = ''
IF  .NOT. EMPTY(key_docfte)
     docbusc = key_docfte
ENDIF
PUBLIC gh
SELECT orden
GOTO BOTTOM
IF  .NOT. EMPTY(docbusc)
     SEEK docbusc
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'La O/C no fue Encontrada!'
          GOTO BOTTOM
     ENDIF
ENDIF
SCATTER BLANK MEMVAR
HIDE POPUP ALL
DO inicia
DO pantalla
DO vista
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     ACTIVATE MENU mmenu1
ENDDO
DO fin_opcion
RETURN
*
PROCEDURE inicia
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 13, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 14, 00  ;
       TO 23, 79 TITLE  ;
       '®F4¯ Imprime  ° ®F7¯ Seguimiento  ° ®F9¯ Detalle :Item  '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2a FROM 10, 04  ;
       TO 16, 75 TITLE  ;
       '®F5¯ Agrega  ° ®F8¯ Eliminar  ° ®F10¯ Terminar '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 20, 63  ;
       TO 22, 78 TITLE 'TOTAL '  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_4 FROM 20, 48  ;
       TO 22, 78 COLOR SCHEME 10
DEFINE WINDOW wind_5 FROM 16, 01  ;
       TO 18, 79 TITLE  ;
       ' Destino '
DEFINE MENU mmenu1 COLOR SCHEME 3
DEFINE PAD revis OF mmenu1 PROMPT  ;
       '\<Revisa' AT 24, 00
DEFINE PAD busca OF mmenu1 PROMPT  ;
       '\<Busca' AT 24, 08
DEFINE PAD anter OF mmenu1 PROMPT  ;
       '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu1 PROMPT  ;
       '\<Siguiente' AT 24, 25
DEFINE PAD corri OF mmenu1 PROMPT  ;
       '\Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu1 PROMPT  ;
       '\Ingresa' AT 24, 45
DEFINE PAD anula OF mmenu1 PROMPT  ;
       '\aNula  ' AT 24, 54
DEFINE PAD lista OF mmenu1 PROMPT  ;
       '\<Listar' AT 24, 63
DEFINE PAD termi OF mmenu1 PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu1 DO revis
ON SELECTION PAD busca OF mmenu1 DO busca
ON SELECTION PAD anter OF mmenu1 DO anter
ON SELECTION PAD proxi OF mmenu1 DO proxi
ON SELECTION PAD corri OF mmenu1 DO corri
ON SELECTION PAD ingre OF mmenu1 DO ingre
ON SELECTION PAD anula OF mmenu1 DO anula
ON SELECTION PAD lista OF mmenu1 DO lista
ON SELECTION PAD termi OF mmenu1 DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_1
CLEAR
@ 1, 2 SAY '       N£mero O/C:'
@ 1, 40 SAY '            Fecha:'
@ 2, 2 SAY '        Proveedor:'
@ 4, 2 SAY '      Cadena Fun.:'
@ 5, 2 SAY '  Fte. Financiam.:'
@ 6, 2 SAY '          Funci¢n:'
@ 7, 2 SAY '         Programa:'
@ 8, 2 SAY '      Subprograma:'
@ 9, 2 SAY ' Activid./Proyec.:'
@ 10, 2 SAY '       Calendario:'
@ 10, 40 SAY '              H/C:'
@ 11, 2 SAY '    Observaciones:'
RETURN
*
PROCEDURE vista
ON KEY LABEL F5
ON KEY LABEL F8
SELECT orden
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
ON KEY LABEL F9 DO VISTA_DET
ON KEY LABEL F7 DO PREstado 
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
@ 0, 02 SAY IIF(m.tipdoc = 'OK'  ;
  .AND. m.estado <> '5',  ;
  'O/C Visado', '             ')
@ 0, 60 SAY vestoc(m.estado)  ;
  COLOR SCHEME 02
@ 1, 22 SAY m.periodo
@ 1, 25 SAY m.numoc
@ 1, 58 SAY m.fecoc
@ 1, 68 SAY IIF( .NOT.  ;
  EMPTY(m.fecdesp), '<' +  ;
  DTOC(m.fecdesp) + '>',  ;
  SPACE(10))
@ 2, 2 SAY '        Proveedor:'
@ 2, 22 SAY IIF(EMPTY(m.codprv),  ;
  'Sin Codigo' + SPACE(40),  ;
  val_prv(m.codprv))
IF m.tipo = 'M'
     @ 3, 2 SAY  ;
       '      Documento :'
     @ 3, 22 SAY m.memoran
ELSE
     @ 3, 2 SAY SPACE(79)
ENDIF
@ 4, 22 SAY val_codcad(m.codcad, ;
  m.periodo,'V',22,30)
@ 5, 22 SAY val_para(m.codfte, ;
  'CODFTE','V',22,30)
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
@ 10, 22 SAY val_para(m.nummes, ;
  'FECMES','V',20,10)
@ 10, 60 SAY m.perhc + ' ' +  ;
  m.numhc
@ 11, 22 SAY m.observa PICTURE  ;
  '@S54'
DO vista_hijo
DO total
RETURN
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
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT iteoc
SEEK m.periodo + m.numoc
BROWSE NOOPTIMIZE FIELDS codart  ;
       :H = 'C¢digo', canreq :H =  ;
       'Cantidad' :P =  ;
       '99,999.99', coduni :H =  ;
       'Uni' :W = .F. : 3, descri  ;
       :H = 'Descripci¢n' : 26 :W =  ;
       .F., numpec :H = 'Pecs' :W =  ;
       .F., preuni :H = 'PreUni'  ;
       :P = '99,999.999' :W = .F.,  ;
       x = ROUND(canreq * preuni,  ;
       2) :H = 'Total' :P =  ;
       '99,999.99' :W = .F.  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_2 KEY m.periodo +  ;
       m.numoc TIMEOUT 0.001   ;
       NOREFRESH
SELECT orden
RETURN
*
PROCEDURE vista_det
HIDE POPUP ALL
SELECT iteoc
SEEK m.periodo + m.numoc
ON KEY LABEL F9 DO OBSERVA
ON KEY LABEL F7
GOTO TOP
BROWSE NOOPTIMIZE FIELDS codart  ;
       :H = 'C¢digo', canreq :H =  ;
       'Cantidad' :P =  ;
       '99,999.99', coduni :H =  ;
       'Uni' :W = .F. : 3, descri  ;
       :H = 'Descripci¢n' : 26 :W =  ;
       .F., numpec :H = 'Pecs' :W =  ;
       .F., preuni :H = 'PreUni'  ;
       :P = '99,999.999' :W = .F.,  ;
       x = ROUND(canreq * preuni,  ;
       2) :H = 'Total' :P =  ;
       '99,999.99' :W = .F.  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_2 KEY m.periodo +  ;
       m.numoc NOREFRESH
SELECT orden
DO vista
ON KEY LABEL F7 DO PREstado 
ON KEY LABEL F9 DO VISTA_DET
RETURN
*
PROCEDURE revis
SELECT orden
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SET RELATION TO periodo + numoc INTO iteoc
SET SKIP TO iteoc
vtemp = RECNO()
HIDE MENU mmenu1
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS numoc :H = ' N§ ',  ;
       codfte :H = 'Fte', fecoc  ;
       :H = 'Fecha', cc =  ;
       vestoc(orden.estado) :H =  ;
       'Estd' : 4, iteoc.descri  ;
       :H = 'Articulo ' : 36,  ;
       iteoc.coduni :H = 'Unid',  ;
       iteoc.canreq :H = 'Cantid'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu1
ON KEY LABEL F10
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
vperiodo = RIGHT(DTOC(DATE()), 2)
vnum_oc = '    '
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
PROCEDURE buspec
ACTIVATE WINDOW wind_4
vnumpec = '0000'
@ 0, 1 SAY  ;
  'Ingrese N§ Pecosa => ' GET  ;
  vnumpec PICTURE '!!!!'
READ
DEACTIVATE WINDOW wind_4
SEEK m.periodo + ALLTRIM(vnumpec)
RETURN
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
      vtomes
vcli = SPACE(4)
vano = '00'
vfte = '  '
vcodfte = '  '
vmes = '  '
@ 01, 01 SAY  ;
  '     Tipo Listado : ' GET  ;
  vlistado FUNCTION  ;
  '^ por Documento;Detallado;Resumido;Control'
@ 05, 01 SAY  ;
  '        Total O/C : ' GET  ;
  vtocli SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,7,22) .AND.  ;
  assig() WHEN vlistado = 1
@ 07, 01 SAY  ;
  '              O/C : '
@ 07, 22 GET vano PICTURE '!!'  ;
  WHEN vtocli = 2 .AND. vlistado =  ;
  1
@ 07, 24 SAY '-'
@ 07, 25 GET vcli PICTURE '!!!!'  ;
  VALID vo() .AND. valord() WHEN  ;
  vtocli = 2 .AND. vlistado = 1
@ 09, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtofue,11,22) WHEN  ;
  (vlistado = 2 .OR. vlistado = 3  ;
  .OR. vlistado = 4)
@ 11, 01 SAY  ;
  '           Fuente : '
@ 11, 22 GET vcodfte PICTURE '!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  'C') WHEN vtofue = 2 .AND.  ;
  (vlistado = 2 .OR. vlistado =  ;
  3)
@ 13, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Numero;Proveedor;Emision;Fuente'  ;
  WHEN vtocli = 1 .AND. (vlistado =  ;
  2 .OR. vlistado = 3 .OR.  ;
  vlistado = 4)
@ 16, 01 SAY  ;
  '           Estado : ' GET  ;
  vtippro FUNCTION  ;
  '^ Todos;Pendientes;Atendidos;Afectados;Anulados;Liquidados'  ;
  WHEN vtocli = 1 .AND. (vlistado =  ;
  2 .OR. vlistado = 3 .OR.  ;
  vlistado = 4)
@ 19, 01 SAY  ;
  '  Todos los Meses : ' GET  ;
  vtomes SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtomes,11,22) WHEN  ;
  (vlistado = 2 .OR. vlistado = 3  ;
  .OR. vlistado = 4)
@ 20, 01 SAY  ;
  '              Mes : '
@ 20, 22 GET vmes PICTURE '!!'  ;
  VALID val_para(vmes,'FECMES', ;
  'C') WHEN vtomes = 2 .AND.  ;
  (vlistado = 2 .OR. vlistado = 3  ;
  .OR. vlistado = 4)
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
     IF vlistado <> 4
          INDEX ON IIF(vorden = 1,  ;
                numoc, IIF(vorden =  ;
                2, codprv,  ;
                IIF(vorden = 3,  ;
                DTOS(fecemi),  ;
                codfte + numoc)))  ;
                TO (vind) FOR  ;
                IIF(vtocli = 1,  ;
                .T., periodo +  ;
                numoc = vano +  ;
                vcli) .AND.  ;
                IIF(vtippro = 1,  ;
                .T., IIF(vtippro =  ;
                2, estado = '00',  ;
                IIF(vtippro = 3,  ;
                estado = '40',  ;
                IIF(vtippro = 4,  ;
                estado = '20',  ;
                IIF(vtippro = 5,  ;
                estado = '99',  ;
                estado =  ;
                '50')))))
          SET FILTER TO IIF(vtofue = 1,;
.T., codfte = ALLTRIM(vcodfte));
.AND. IIF(vtomes = 1,;
.T., VAL(vmes) = MONTH(fecoc))
     ELSE
          INDEX ON IIF(vorden = 1,  ;
                numoc, IIF(vorden =  ;
                2, codprv,  ;
                IIF(vorden = 3,  ;
                DTOS(fecemi),  ;
                codfte + numoc)))  ;
                TO (vind) FOR  ;
                IIF(vtocli = 1,  ;
                .T., periodo +  ;
                numoc = vano +  ;
                vcli) .AND.  ;
                IIF(vtofue = 1,  ;
                .T., codfte =  ;
                ALLTRIM(vcodfte))  ;
                .AND. IIF(vtomes =  ;
                1, .T., VAL(vmes) =  ;
                MONTH(fecoc))  ;
                .AND. busprv()
     ENDIF
     SET INDEX TO (vind)
     COUNT ALL TO vtotoc
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
          DO CASE
               CASE vlistado = 1
                    DO repprg  ;
                       WITH  ;
                       'LisOc3',  ;
                       ' Ordenes de Compra',  ;
                       2
               CASE vlistado = 2
                    DO reporte  ;
                       WITH 2,  ;
                       'LisOrdc',  ;
                       ' Ordenes de Compra ',  ;
                       1, .F.,  ;
                       .T.
               CASE vlistado = 3
                    DO reporte  ;
                       WITH 2,  ;
                       'LisOrdY',  ;
                       ' Ordenes de Compra ',  ;
                       1, .F.,  ;
                       .T.
               CASE vlistado = 4
                    DO reporte  ;
                       WITH 2,  ;
                       'ordcont',  ;
                       ' Ordenes de Compra ',  ;
                       1, .F.,  ;
                       .T.
          ENDCASE
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
FUNCTION assig
vcli = orden.numoc
vano = orden.periodo
vfte = orden.codfte
RETURN .T.
*
FUNCTION vo
vcli = PADL(ALLTRIM(vcli), 4,  ;
       '0')
RETURN .T.
*
PROCEDURE termi
ven_accion = .F.
ON KEY LABEL F2
ON KEY LABEL F4
ON KEY LABEL F9
ON KEY LABEL F7
HIDE WINDOW wind_1
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_3
RELEASE WINDOW wind_4
RELEASE MENU mmenu1
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION valprv
PRIVATE xx, vfun
vfun = .F.
m.codprv = IIF(EMPTY(m.codprv),  ;
           m.codprv,  ;
           PADL(ALLTRIM(m.codprv),  ;
           4, '0'))
xx = val_prv(m.codprv,.T.,2,26)
IF xx
     RETURN .T.
ENDIF
RETURN vfun
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
FUNCTION observa
valias = ALIAS()
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
IF  .NOT. WEXIST('Observa')
     DEFINE WINDOW observa FROM  ;
            05, 18 TO 18, 61  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Detalle O/C ±'  ;
            FOOTER  ;
            ' ° ®F10¯ Graba ° '  ;
            DOUBLE COLOR SCHEME  ;
            1
ENDIF
IF WVISIBLE('Observa')
     ACTIVATE WINDOW SAME observa
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              observa
ENDIF
MODIFY MEMO observa WINDOW  ;
       observa
IF  .NOT. WVISIBLE('Observa')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado. No graba la Observaci¢n '
ENDIF
RETURN .T.
*
FUNCTION visobs
valias = ALIAS()
IF  .NOT. WEXIST('Observa')
     DEFINE WINDOW observa FROM  ;
            03, 18 TO 20, 61  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Detalle O/C ±'  ;
            FOOTER  ;
            ' ° ®Esc¯ Sale ° '  ;
            DOUBLE COLOR SCHEME  ;
            1
ENDIF
IF WVISIBLE('Observa')
     ACTIVATE WINDOW SAME observa
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              observa
ENDIF
MODIFY MEMO observa NOEDIT WINDOW  ;
       observa
IF  .NOT. WVISIBLE('Observa')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
RETURN .T.
*
FUNCTION itep
as = ALIAS()
SELECT pecosa
SEEK iteoc.periodo + iteoc.numpec +  ;
     iteoc.codfte
ok1 = pecosa.destino
SELECT (as)
RETURN (ok1)
*
PROCEDURE destino
PRIVATE az
az = ALIAS()
SELECT iteoc
SEEK m.periodo + m.numoc
SELECT pecosa
SEEK iteoc.periodo + iteoc.numpec +  ;
     iteoc.codfte
m.destino = IIF(EMPTY(m.destino),  ;
            pecosa.destino,  ;
            m.destino)
SELECT (az)
ACTIVATE WINDOW wind_5
@ 0, 0 SAY 'Destino: ' GET  ;
  m.destino PICTURE '@S73'
READ
DEACTIVATE WINDOW wind_5
RETURN
*
PROCEDURE alan
RETURN
ax = ALIAS()
SELECT alan
SEEK ALLTRIM(m.codfte) +  ;
     ALLTRIM(m.numoc)
IF FOUND()
     m.nummes = alan.nummes
ELSE
     m.nummes = '00'
ENDIF
SELECT (ax)
RETURN
*
FUNCTION val_artc
PARAMETER xcod, _tipo, _x, _y
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) .AND.  ;
       _tipo
_campo = VARREAD()
ord = ORDER()
malias = ALIAS()
SELECT iteart
GOTO TOP
_oldwnd = WOUTPUT()
v_fun = .F.
IF  .NOT. medita
     SEEK xcod
     v_fun = IIF(FOUND(), descri,  ;
             '')
ELSE
     IF EMPTY(xcod)
          SET ORDER TO 2
          ACTIVATE SCREEN
          ON KEY LABEL F10 KEYBOARD CHR(23)
          ON KEY LABEL F8 DO BorrDet
          ON KEY LABEL F5 DO Agr2Det
          ON KEY LABEL F2 DO FunBusDet
          DEFINE WINDOW _busart  ;
                 FROM 2, 01 TO 22,  ;
                 78
          BROWSE FIELDS codart :H =  ;
                 'C¢digo' :W =  ;
                 .F., descri :H =  ;
                 'Nombre' : 70,  ;
                 coduni :H =  ;
                 'Unidad' : 7  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 WINDOW _busart  ;
                 TITLE  ;
                 '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²'  ;
                 NOLGRID
          vord = RECNO()
          GOTO TOP
          SCAN WHILE  ;
               EMPTY(descri)
               IF RLOCK()
                    DELETE NEXT 1
               ENDIF
          ENDSCAN
          GOTO TOP
          GOTO vord
          ON KEY LABEL F10
          ON KEY LABEL F8
          ON KEY LABEL F5
          ON KEY LABEL F2
          RELEASE WINDOW _busart
          SET ORDER TO 1
          IF LASTKEY() = 27
               v_fun = .F.
          ELSE
               xcod = codart
               IF mmsg
                    @ _x, _y SAY  ;
                      descri
               ENDIF
               SELECT (malias)
               IF  .NOT. _tipo
                    REPLACE &_campo WITH;
 xcod
               ENDIF
               v_fun = .T.
          ENDIF
     ENDIF
ENDIF
IF v_fun
     SELECT iteoc
     IF RLOCK()
          REPLACE coduni WITH  ;
                  iteart.coduni,  ;
                  preuni WITH  ;
                  iteart.preuni,  ;
                  descri WITH  ;
                  iteart.descri
     ENDIF
ENDIF
SELECT (malias)
SET ORDER TO (ord)
ON KEY LABEL F5 DO Agreg_iC
ON KEY LABEL F8 DO Elimi_iC
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
RETURN v_fun
*
PROCEDURE imprimir
PRIVATE vcon
SELECT orden
vcon = RECNO()
SCATTER MEMVAR
SET MEMOWIDTH TO 43
vnumoc = m.periodo + m.numoc
SET RELATION TO periodo + numoc + codfte;
INTO iteoc
SET FILTER TO periodo + numoc = vnumoc
SET SKIP TO iteoc
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO repprg WITH 'LisOc3',  ;
        ' Ordenes de Compra', 2
ENDIF
SET SKIP TO
SET FILTER TO
SET RELATION TO
SELECT orden
GOTO vcon
DO vista
RETURN
*
PROCEDURE titulo
pagina = pagina + 1
vtitulo = ' ORDEN DE COMPRA '
@ 0, 1 SAY CHR(18) + CHR(14)
@ 0, 15 SAY vtitulo + CHR(18)
@ 1, 2 SAY cia
@ 1, 70 SAY 'P g.'
@ 1, 74 SAY pagina PICTURE  ;
  '##,###'
@ 2, 70 SAY DATE()
@ 3, 04 SAY  ;
  '                  ÚÄÄÄÄÄÄÄÄÄ¿'
@ 4, 04 SAY  ;
  'ORDEN DE COMPRA : ³ ' +  ;
  orden.numoc + '.' +  ;
  orden.nummes + ' ³'
@ 4, 62 SAY 'Estado : ' +  ;
  IIF(orden.estado = '00',  ;
  'Emitido  ', IIF(orden.estado =  ;
  '20', 'Afectado ',  ;
  IIF(orden.estado = '99',  ;
  'Anulado', IIF(orden.estado =  ;
  '50', 'Liquidado',  ;
  ' Atendido'))))
@ 5, 04 SAY  ;
  '                  ÀÄÄÄÄÄÄÄÄÄÙ'
@ 5, 59 SAY 'Fecha O/C:' +  ;
  DTOC(orden.fecoc)
@ 6, 66 SAY IIF( .NOT.  ;
  EMPTY(orden.numhc), 'H/C :',  ;
  '  ') + IIF( .NOT.  ;
  EMPTY(orden.numhc), orden.perhc +  ;
  '.' + orden.numhc, ' ')
@ 7, 02 SAY ' SE¥OR(ES) : ' +  ;
  CHR(27) + 'G' +  ;
  val_prv(orden.codprv) + CHR(27) +  ;
  'H'
@ 8, 02 SAY '     R.U.C.: ' +  ;
  promae.numruc
@ 9, 02 SAY ' DIRECCION : ' +  ;
  val_fun('ProMae','CodPrv', ;
  'Dirpro',orden.codprv)
@ 10, 02 SAY  ;
  ' -Le agradecemos enviar a nuestro Almac‚n : Jr. Lima Nro. 878 - Piura '
@ 11, 1 SAY CHR(15)
@ 11, 06 SAY  ;
  IIF(SUBSTR(orden.observa, 1, 4) <>  ;
  SPACE(4), 'OBS.:' + CHR(15) +  ;
  orden.observa + CHR(18), ' ') +  ;
  CHR(18)
@ 12, 02 SAY  ;
  ' -Facturar a nombre de : SOCIEDAD DE BENEFICENCIA DE PIURA - RUC NRO. 14708286 ' +  ;
  CHR(15)
@ 13, 02 SAY IIF(orden.tipo = 'M',  ;
  'REFERENCIA : ' + memoran,  ;
  CHR(18) + valccc() + CHR(15))
@ 14, 05 SAY  ;
  'ÚÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 15, 05 SAY  ;
  '³Pecosa ³  C¢digo   ³Unidad³                Art¡culo                                     ³Cantidad³Pre.Uni.³%    ³Dscto   ³ I.G.V. ³       Total  ³'
@ 16, 05 SAY  ;
  'ÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
linea = 17
RETURN
*
PROCEDURE lisoc3
PARAMETER _desti
IF _desti = 2
     SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINTER
STORE 0 TO pagina, linea
impri = .F.
xcolumna = SPACE(7)
GOTO TOP
vsuma = 0
vsumigv = 0
SCAN
     IF pagina = 0 .OR. linea >  ;
        60
          DO titulo
     ENDIF
     DO CASE
          CASE linea < 34
               @ linea, 1 SAY  ;
                 CHR(15)
               @ linea, 08 SAY  ;
                 iteoc.numpec
               @ linea, 15 SAY  ;
                 iteoc.codart
               @ linea, 27 SAY  ;
                 iteoc.coduni
               @ linea, 35 SAY  ;
                 ALLTRIM(iteoc.descri) +  ;
                 IIF(EMPTY(MLINE(iteoc.observa,  ;
                 1)), ' ', ' - ' +  ;
                 ALLTRIM(MLINE(iteoc.observa,  ;
                 1)))
               @ linea, 93 SAY  ;
                 iteoc.canreq  ;
                 PICTURE  ;
                 '999.999'
               @ linea, 100 SAY  ;
                 iteoc.preuni  ;
                 PICTURE  ;
                 '9,999.999'
               @ linea, 112 SAY  ;
                 iteoc.pctdto  ;
                 PICTURE '99.99'
               xpctdto = (iteoc.preuni *  ;
                         iteoc.canreq) *  ;
                         (iteoc.pctdto) /  ;
                         100
               @ linea, 119 SAY  ;
                 xpctdto PICTURE  ;
                 '99.99'
               xpctigv = (iteoc.preuni *  ;
                         iteoc.canreq) *  ;
                         (100 -  ;
                         iteoc.pctdto) /  ;
                         100 *  ;
                         (iteoc.pctigv) /  ;
                         100
               @ linea, 125 SAY  ;
                 xpctigv PICTURE  ;
                 '99.99'
               @ linea, 130 SAY  ;
                 iteoc.valtot  ;
                 PICTURE  ;
                 '999,999.99'
               vsuma = vsuma +  ;
                       iteoc.valtot
               vsumigv = vsumigv +  ;
                         xpctigv
               linea = linea + 1
          CASE linea >= 34
               @ linea, 1 SAY  ;
                 CHR(18)
               @ linea, 05 SAY  ;
                 REPLICATE('-',  ;
                 80)
               linea = linea + 1
               @ linea, 50 SAY  ;
                 'S U B T O T A L   S/.'
               @ linea, 71 SAY  ;
                 vsuma PICTURE  ;
                 '999,999.99'
               linea = linea + 2
               SELECT 2
               cc = RECNO()
               SELECT 1
               DO sumario
               DO titulo
               SELECT 2
               GOTO cc - 1
               SELECT 1
               @ linea, 01 SAY  ;
                 CHR(18)
               @ linea, 52 SAY  ;
                 'V I E N E N  S/.'
               @ linea, 71 SAY  ;
                 vsuma PICTURE  ;
                 '999,999.99'
               linea = linea + 1
          OTHERWISE
     ENDCASE
     IF  .NOT.  ;
         EMPTY(iteoc.observa)
          FOR xx = 2 TO  ;
              MEMLINES(iteoc.observa)
               @ linea, 37 SAY  ;
                 MLINE(iteoc.observa,  ;
                 xx)
               linea = linea + 1
               IF linea >= 34
                    @ linea, 1  ;
                      SAY  ;
                      CHR(18)
                    @ linea, 05  ;
                      SAY  ;
                      REPLICATE( ;
                      '-', 80)
                    linea = linea +  ;
                            1
                    @ linea, 56  ;
                      SAY  ;
                      '    V A N   S/.'
                    @ linea, 71  ;
                      SAY vsuma  ;
                      PICTURE  ;
                      '999,999.99'
                    linea = linea +  ;
                            2
                    SELECT 2
                    cc = RECNO()
                    SELECT 1
                    DO sumario
                    DO titulo
                    SELECT 2
                    GOTO cc
                    SELECT 1
                    @ linea, 01  ;
                      SAY  ;
                      CHR(18)
                    @ linea, 52  ;
                      SAY  ;
                      'V I E N E N  S/.'
                    @ linea, 71  ;
                      SAY vsuma  ;
                      PICTURE  ;
                      '999,999.99'
                    @ linea, 79  ;
                      SAY  ;
                      CHR(15)
                    linea = linea +  ;
                            1
               ENDIF
          ENDFOR
     ENDIF
ENDSCAN
@ linea, 1 SAY CHR(18)
@ linea, 05 SAY REPLICATE('Í',  ;
  80)
linea = linea + 1
IF vsumigv > 0
     @ linea, 30 SAY  ;
       'I.G.V.  S/.'
     @ linea, 42 SAY vsumigv  ;
       PICTURE '9,999.99'
ENDIF
@ linea, 56 SAY 'T O T A L   S/.'
@ linea, 71 SAY vsuma PICTURE  ;
  '999,999.99'
linea = linea + 1
@ linea, 71 SAY 'ÍÍÍÍÍÍÍÍÍÍÍÍ'
linea = linea + 1
GOTO TOP
DO codific
DO sumario
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE codific
valias = ALIAS()
vorder = ORDER()
vrecno = RECNO()
@ linea, 04 SAY '         SON :' +  ;
  letras1(orden.valtot, ;
  'NUEVOS SOLES')
linea = linea + 2
@ linea, 1 SAY CHR(15)
IF  .NOT. EMPTY(orden.destino)
     @ linea, 04 SAY  ;
       '     Destino : ' +  ;
       ALLTRIM(orden.destino)
     linea = linea + 1
ENDIF
@ linea, 04 SAY '  Calendario : ' +  ;
  nummes + '  ' +  ;
  val_para(orden.nummes,'FECMES', ;
  'D',26,50)
linea = linea + 1
@ linea, 04 SAY '  Cadena Fun.: ' +  ;
  codcad + '   ' +  ;
  ALLTRIM(val_codcad(m.codcad, ;
  m.periodo,'D',26,60))
linea = linea + 1
@ linea, 04 SAY 'Fte.Financia.: ' +  ;
  codfte + '     ' +  ;
  ALLTRIM(val_para(m.codfte, ;
  'CODFTE','D',26,60))
linea = linea + 1
@ linea, 04 SAY '     Funci¢n : ' +  ;
  ALLTRIM(maepre.codfun) +  ;
  '     ' +  ;
  ALLTRIM(val_para(maepre.codfun, ;
  'CODFUN','D',26,60))
linea = linea + 1
@ linea, 04 SAY '    Programa : ' +  ;
  ALLTRIM(maepre.codprg) + '    ' +  ;
  ALLTRIM(val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'D',26, ;
  60))
linea = linea + 1
@ linea, 04 SAY ' SubPrograma : ' +  ;
  ALLTRIM(maepre.codspr) + '   ' +  ;
  ALLTRIM(val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'D',26, ;
  60))
linea = linea + 1
@ linea, 04 SAY 'Activ./Proye.: ' +  ;
  ALLTRIM(maepre.actpry) + ' ' +  ;
  ALLTRIM(val_para(maepre.actpry, ;
  'ACTPRY','D',26,60))
linea = linea + 1
SELECT iteoc1
SET FILTER TO periodo + numoc = orden.periodo;
+ orden.numoc
GOTO TOP
SCAN
     @ linea, 1 SAY CHR(15)
     @ linea, 04 SAY  ;
       '   Componente : ' +  ;
       codcom
     @ linea, 26 SAY 'Meta : ' +  ;
       codmet
     @ linea, 105 SAY valpart  ;
       PICTURE '9,999,999.99'
     linea = linea + 1
     vrecno1 = RECNO()
     DO CASE
          CASE linea >= 36
               SELECT 1
               DO sumario
               DO titulo
               @ linea, 52 SAY  ;
                 'V I E N E N '
               linea = linea + 2
               SELECT iteoc1
               GOTO vrecno1
     ENDCASE
ENDSCAN
SET FILTER TO
SELECT (valias)
SET ORDER TO (vorder)
GOTO vrecno
RETURN
*
PROCEDURE sumario
@ 46, 1 SAY CHR(18)
@ 47, 07 SAY  ;
  'ÚÄ¿                                       ÚÄ¿'
@ 48, 07 SAY  ;
  '³1³                                       ³3³'
@ 49, 07 SAY  ;
  'ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-        ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-'
@ 50, 07 SAY  ;
  '     Director de Abastecimiento                   Jefe de Almacen '
@ 53, 07 SAY  ;
  'ÚÄ¿                                                RECIBI CONFORME:  '
@ 54, 07 SAY  ;
  '³2³                                              ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ '
@ 55, 07 SAY  ;
  'ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-               ³ Fecha:   /  /   ³ '
@ 56, 07 SAY  ;
  '       Jefe de Adquisiciones                     ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ '
@ 57, 56 SAY CHR(15) +  ;
  'Elaborado por: ' +  ;
  vusua(user)
@ 59, 1 SAY CHR(15) +  ;
  'Nota: Esta Orden es Nula sin la firma mancomunada del Director de Abastecimientos y el Jefe de Adquisiciones .Nos reservamos el derecho de'
@ 60, 1 SAY CHR(15) +  ;
  '      devolver la mercader¡a que no est‚ de acuerdo con nuestras especificaciones.' +  ;
  CHR(18) + CHR(12)
linea = 1
RETURN
*
PROCEDURE prestado
USE IN 11
USE IN 12
USE IN 13 Cheque ALIAS cheque  ;
    ORDER Cheque1
USE IN 14 Compag ALIAS compag  ;
    ORDER Compag1
DO estado WITH 'OC',  ;
   'm.perhc+m.numhc'
USE IN 13
USE IN 14
USE IN 11 Itehc ALIAS itehc ORDER  ;
    Itehc1
USE IN 12 HOJMOD ALIAS hojmod  ;
    ORDER HOJMOD1
RETURN
*
FUNCTION valccc
PRIVATE as, vnumccc
USE IN 10 SOLCOT ALIAS solcot  ;
    ORDER Solcot1
USE IN 11 IteSC ALIAS itesc ORDER  ;
    Itesc1
USE IN 12
as = ALIAS()
vcc = iteoc.numpec + iteoc.codart +  ;
      ALLTRIM(iteoc.numord)
SELECT itesc
SET RELATION TO periodo + numsc INTO solcot
SET ORDER TO ITESC1
SEEK vcc
vnumccc = IIF(FOUND(),  ;
          '   CUADRO COMPARATIVO : ' +  ;
          solcot.numccc, '   ')
SET RELATION TO
SELECT (as)
USE IN 10 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 11 Itehc ALIAS itehc ORDER  ;
    Itehc1
USE IN 12 HOJMOD ALIAS hojmod  ;
    ORDER HOJMOD1
RETURN vnumccc
*
FUNCTION busprv
PRIVATE ali, vkey
ali = ALIAS()
vkey = codprv
SELECT promae
SEEK vkey
IF FOUND()
     IF estado = 'VG'
          vfun = .F.
     ELSE
          vfun = .T.
     ENDIF
ELSE
     vfun = .T.
ENDIF
SELECT (ali)
RETURN vfun
*
FUNCTION vusua
PARAMETER csys
PRIVATE ali
ali = ALIAS()
vkey = ALLTRIM(csys)
SELECT usu
SEEK vkey
vfun = nombre
SELECT (ali)
RETURN vfun
*

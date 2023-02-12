USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 2 vale ALIAS vale ORDER  ;
    vales1
USE IN 3 Pecosa ALIAS pecosa  ;
    ORDER Pecosa1
USE IN 4 ItePec ALIAS itepec  ;
    ORDER ItePec1
USE IN 5 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 6 IteArt ALIAS iteart  ;
    ORDER IteArt5
USE IN 7 cdrnec ALIAS cuadro  ;
    ORDER Cdrnec1
USE IN 8 itecn ALIAS itecn ORDER  ;
    itecn3
USE IN 9 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 10 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 11 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 12 Regveh ALIAS vehic  ;
    ORDER RegVeh1
ON KEY LABEL F4 do imprimir
vmens01 = ' Vales : REVISION '
vmens02 = 'Registro de Vale'
vmens04 = 'Dicho Vale no fue encontrado'
vmens05 = 'No existe Vale anterior'
vmens06 = 'No existe Vale siguiente'
vmens07 = '¨ Desea Anular ‚ste Vale ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Vale ha sido anulado'
vmens10 = 'El Vale ya est  Atendido'
vmens11 = 'El Vale ha sido devuelto'
vmens12 = 'El Vale ya tiene Pecosa'
SELECT vale
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular   Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 11, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_4 FROM 04, 01  ;
       TO 21, 77 TITLE  ;
       'Relaci¢n de Art¡culos'  ;
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
       'a\<Nular ' AT 24, 54
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
@ 1, 2 SAY '          Periodo :'
@ 1, 40 SAY '              Mes :'
@ 2, 2 SAY '       Fecha Vale :'
@ 3, 2 SAY '      N£mero Vale :'
@ 4, 2 SAY '        Proveedor :'
@ 5, 2 SAY '         Veh¡culo :'
@ 6, 2 SAY '     Tipo de Vale :'
@ 7, 2 SAY '  Cadena de Gasto :'
@ 8, 2 SAY ' F.Financiamiento :'
@ 11, 2 SAY '   Detalle Pedido :'
@ 12, 2 SAY '            Valor :'
@ 13, 2 SAY '           Chofer :'
@ 14, 2 SAY '          Oficina :'
@ 16, 2 SAY '     Pe.Co.Sa. N§ :'
@ 17, 2 SAY '       Factura N§ :'
@ 18, 2 SAY '          Destino :'
@ 19, 2 SAY '   Doc.Referencia :'
@ 20, 2 SAY '    Observaciones :'
RETURN
*
PROCEDURE vista
SELECT vale
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
@ 1, 22 SAY m.periodo
@ 1, 60 SAY val_para(m.nummes, ;
  'FECMES','D',22,20)
@ 2, 22 SAY m.fecval
@ 3, 22 SAY m.codval
@ 4, 22 SAY IIF(m.codprv = '0000',  ;
  m.nomprv, val_prv(m.codprv)) +  ;
  SPACE(5)
@ 5, 22 SAY m.codpla
@ 6, 22 SAY val_para(m.tipval, ;
  'TIPVAL','D',22,30)
@ 7, 22 SAY val_codcad(m.codcad, ;
  m.periodo,'D',22,40)
@ 8, 22 SAY val_para(m.codfte, ;
  'CODFTE','D',22,40)
IF ALLTRIM(m.tipval) $ 'PC'
     @ 9, 2 SAY  ;
       '  Cantidad Pedido :'
     @ 10, 2 SAY  ;
       '    C¢digo Pedido :'
     @ 10, 40 SAY  ;
       '    Unidad Medida :'
     @ 9, 22 CLEAR TO 9, 78
     @ 9, 22 SAY m.canreq PICTURE  ;
       '9,999,999.999'
     @ 10, 22 SAY m.codart
     @ 10, 60 SAY m.unimed
ELSE
     @ 9, 2 SAY  ;
       '         Servicio :'
     @ 9, 22 SAY  ;
       val_para(m.codser,'CODSER', ;
       'D',22,50)
     @ 10, 02 CLEAR TO 10, 78
ENDIF
@ 11, 22 SAY m.descri PICTURE  ;
  '@S56'
@ 12, 22 SAY m.valtot
@ 13, 22 SAY val_para(m.codchf, ;
  'CODCHF','D',22,50)
@ 14, 22 SAY val_para(m.coddep, ;
  'CODDEP','D',22,50)
@ 16, 22 SAY m.numpec
@ 17, 22 SAY m.serfac + '-' +  ;
  m.numfac
@ 18, 22 SAY m.destino PICTURE  ;
  '@S56'
@ 19, 22 SAY m.docref PICTURE  ;
  '@S56'
@ 20, 22 SAY m.observa PICTURE  ;
  '@S56'
@ 21, 00 SAY PADC( ;
  ' ° ®F4¯ Imprime ° ', 79, ' ')  ;
  COLOR W+/B 
RETURN
*
PROCEDURE revis
SELECT vale
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
BROWSE FIELDS codval :H = ' N§ ',  ;
       fecval :H = 'Fecha', prv =  ;
       val_prv(codprv) :H =  ;
       'Proveedor' : 32, codpla  ;
       :H = 'Placa', coddep :H =  ;
       'Oficina', canreq :H =  ;
       'Cantid' :P = '99,999.99',  ;
       vart = val_artq(codart) :H =  ;
       'Detalle ' : 25, valtot :H =  ;
       'Valor' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_0
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
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnummes = SPACE(2)
vnumval = SPACE(4)
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero Vale: ' GET  ;
  vperiodo PICTURE '!!'
@ 1, 26 SAY '-' GET vnummes  ;
  PICTURE '!!'
@ 1, 31 SAY '-' GET vnumval  ;
  PICTURE '!!!!' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnumval) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vperiodo + vnummes +  ;
          vnumval
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
vnumval = PADL(ALLTRIM(vnumval),  ;
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
IF estado = '99'
     DO standby WITH vmens09
     RETURN
ENDIF
IF estado = '50'
     DO standby WITH vmens12
     RETURN
ENDIF
SELECT vale
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
ACTIVATE WINDOW wind_0
DO pantalla
@ 1, 22 GET m.periodo DISABLE
@ 1, 60 GET m.nummes DISABLE
@ 2, 22 GET m.fecval
@ 3, 22 GET m.codval DISABLE
@ 4, 22 GET m.codprv PICTURE  ;
  '!!!!' VALID IIF(m.codprv =  ;
  '0000', .T., val_prv(m.codprv, ;
  .T.))
@ 4, 27 GET m.nomprv PICTURE  ;
  '@S50'
@ 5, 22 GET m.codpla PICTURE  ;
  '!!!!!!!' VALID  ;
  val_pla(m.codpla,'C')
@ 6, 22 GET m.tipval PICTURE '!'  ;
  VALID val_para(m.tipval, ;
  'TIPVAL',' ',22,15)
@ 7, 22 GET m.codcad PICTURE  ;
  '!!!!' VALID  ;
  val_codcad(m.codcad,m.periodo, ;
  ' ',22,30)
@ 8, 22 GET m.codfte PICTURE '!!'  ;
  VALID val_para(m.codfte, ;
  'CODFTE',' ',22,30)
READ
IF ALLTRIM(m.tipval) $ 'PC'
     @ 9, 2 SAY  ;
       '  Cantidad Pedido :'
     @ 10, 2 SAY  ;
       '    C¢digo Pedido :'
     @ 10, 40 SAY  ;
       '    Unidad Medida :'
     @ 9, 22 GET m.canreq PICTURE  ;
       '9,999,999.999'
     @ 10, 22 GET m.codart  ;
       PICTURE '!!!!!!!!!!' VALID  ;
       val_artq(m.codart,.F.)  ;
       WHEN  .NOT.  ;
       ALLTRIM(m.tipval) $ 'S'
     @ 10, 60 GET m.unimed WHEN   ;
       .NOT. ALLTRIM(m.tipval) $  ;
       'S'
ELSE
     @ 9, 2 SAY  ;
       '         Servicio :'
     @ 9, 22 GET m.codser PICTURE  ;
       '!!!!!' VALID  ;
       val_para(m.codser,'CODSER', ;
       ' ',22,50,6) .AND. asser()  ;
       WHEN ALLTRIM(m.tipval) $  ;
       'S'
     @ 10, 02 CLEAR TO 10, 78
ENDIF
@ 11, 22 GET m.descri PICTURE  ;
  '@S56'
@ 12, 22 GET m.valtot
@ 13, 22 GET m.codchf PICTURE  ;
  '!!!' VALID val_para(m.codchf, ;
  'CODCHF',' ',22,50)
@ 14, 22 GET m.coddep PICTURE  ;
  '!!!!!!' VALID  ;
  val_para(m.coddep,'CODDEP',' ', ;
  22,40,7)
@ 16, 22 GET m.numpec
@ 17, 22 GET m.serfac
@ 17, 24 SAY '-'
@ 17, 25 GET m.numfac
@ 18, 22 GET m.destino PICTURE  ;
  '@S56'
@ 19, 22 GET m.docref PICTURE  ;
  '@S56'
@ 20, 22 GET m.observa PICTURE  ;
  '@S56'
READ VALID val_read()
IF LASTKEY() <> 27
     GATHER MEMVAR
ELSE
     SELECT vale
ENDIF
DO vista
UNLOCK
RETURN
*
FUNCTION asser
RETURN .T.
*
PROCEDURE ingre
SELECT vale
vtemp = RECNO()
DO pantalla
SCATTER BLANK MEMVAR
m.periodo = RIGHT(DTOC(DATE()),  ;
            2)
m.fecval = DATE()
m.codprv = SPACE(4)
m.codcad = SPACE(4)
m.codfte = SPACE(2)
@ 1, 22 GET m.periodo
@ 1, 60 GET m.nummes PICTURE '!!'  ;
  VALID val_para(m.nummes, ;
  'FECMES',' ',60,10,3)
@ 2, 22 GET m.fecval VALID  ;
  asig_val()
@ 3, 22 GET m.codval VALID  ;
  val_val()
@ 4, 22 GET m.codprv PICTURE  ;
  '!!!!' VALID IIF(m.codprv =  ;
  '0000', .T., val_prv(m.codprv, ;
  .T.) .AND. asprv())
@ 4, 27 GET m.nomprv PICTURE  ;
  '@S50'
@ 5, 22 GET m.codpla PICTURE  ;
  '!!!!!!!' VALID  ;
  val_pla(m.codpla,'C')
@ 6, 22 GET m.tipval PICTURE '!'  ;
  VALID val_para(m.tipval, ;
  'TIPVAL',' ',22,15)
@ 7, 22 GET m.codcad PICTURE  ;
  '!!!!' VALID  ;
  val_codcad(m.codcad,m.periodo, ;
  ' ',22,30)
@ 8, 22 GET m.codfte PICTURE '!!'  ;
  VALID val_para(m.codfte, ;
  'CODFTE',' ',22,30)
READ
IF ALLTRIM(m.tipval) $ 'PC'
     @ 9, 2 SAY  ;
       '  Cantidad Pedido :'
     @ 10, 2 SAY  ;
       '    C¢digo Pedido :'
     @ 10, 40 SAY  ;
       '    Unidad Medida :'
     @ 9, 22 GET m.canreq PICTURE  ;
       '9,999,999.999'
     @ 10, 22 GET m.codart  ;
       PICTURE '!!!!!!!!!!' VALID  ;
       val_artq(m.codart,.F.)  ;
       WHEN  .NOT.  ;
       ALLTRIM(m.tipval) $ 'S'
     @ 10, 60 GET m.unimed WHEN   ;
       .NOT. ALLTRIM(m.tipval) $  ;
       'S'
ELSE
     @ 9, 2 SAY  ;
       '         Servicio :'
     @ 9, 22 GET m.codser PICTURE  ;
       '!!!!!' VALID  ;
       val_para(m.codser,'CODSER', ;
       ' ',22,50,6) .AND. asser()  ;
       WHEN ALLTRIM(m.tipval) $  ;
       'S'
     @ 10, 02 CLEAR TO 10, 78
ENDIF
@ 11, 22 GET m.descri PICTURE  ;
  '@S56'
@ 12, 22 GET m.valtot
@ 13, 22 GET m.codchf PICTURE  ;
  '!!!' VALID val_para(m.codchf, ;
  'CODCHF',' ',22,50)
@ 14, 22 GET m.coddep PICTURE  ;
  '!!!!!!' VALID  ;
  val_para(m.coddep,'CODDEP',' ', ;
  22,40,7)
@ 18, 22 GET m.destino PICTURE  ;
  '@S56'
@ 19, 22 GET m.docref PICTURE  ;
  '@S56'
@ 20, 22 GET m.observa PICTURE  ;
  '@S56'
READ VALID val_read()
IF LASTKEY() <> 27
     SELECT vale
     IF f_appd()
          m.estado = '00'
          m.candesp = m.canreq
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
     GOTO vtemp
     SELECT vale
ENDIF
DO vista
RETURN
*
FUNCTION asprv
m.nomprv = promae.nompro
RETURN .T.
*
PROCEDURE asig_val
SET FILTER TO nummes = ALLTRIM(m.nummes)
GOTO BOTTOM
v1 = VAL(codval)
m.codval = PADL(ALLTRIM(STR(v1 +  ;
           1, 4)), 4, '0')
SET FILTER TO
RETURN
*
FUNCTION val_val
m.codval = PADL(ALLTRIM(m.codval),  ;
           4, '0')
SEEK m.periodo + m.nummes +  ;
     m.codval
IF FOUND()
     DO standby WITH  ;
        'N£mero ya registrado'
     RETURN .F.
ENDIF
RETURN .T.
*
PROCEDURE anula
SELECT pecosa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado <> '00'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ANULAR ‚ste Vale ?' ;
           )
vestado = .T.
SELECT itepec
SEEK m.periodo + m.numpec
SCAN WHILE m.periodo = periodo  ;
     .AND. m.numpec = numpec  ;
     .AND. ALLTRIM(codfte) =  ;
     codfte
     IF estado = '30' .OR. estado =  ;
        '20'
          DO standby WITH  ;
             'La Pecosa ya tiene generada O/C o S/C,no se puede anular'
          vestado = .F.
          EXIT
     ENDIF
ENDSCAN
IF vestado
     IF velimina
          SELECT itepec
          SEEK m.periodo +  ;
               m.numpec
          SCAN WHILE m.periodo =  ;
               periodo .AND.  ;
               m.numpec = numpec  ;
               .AND.  ;
               ALLTRIM(codfte) =  ;
               codfte
               IF RLOCK()
               ENDIF
          ENDSCAN
          SELECT pecosa
     ENDIF
ENDIF
SELECT pecosa
DO vista
UNLOCK
RETURN
*
PROCEDURE imprimir
PRIVATE vcon
SELECT vale
vcon = RECNO()
SCATTER MEMVAR
vnumval = m.periodo + m.nummes +  ;
          m.codval
SEEK vnumval
IF  .NOT. FOUND()
     DO standby WITH vmens08
ELSE
     SET FILTER TO periodo = m.periodo;
.AND. codval = m.codval;
.AND. nummes = ALLTRIM(m.nummes)
     IF m.tipval = 'S'
          DO reporte WITH 2,  ;
             'Regvals',  ;
             ' Pe.co.sa ', 1
     ELSE
          DO reporte WITH 2,  ;
             'Regval',  ;
             ' Pe.co.sa ', 1
     ENDIF
     SET FILTER TO
ENDIF
SELECT vale
GOTO vcon
DO vista
RETURN
*
FUNCTION agreg_item
IF f_appd()
     REPLACE periodo WITH  ;
             m.periodo, numpec  ;
             WITH m.numpec,  ;
             estado WITH '00',  ;
             codcal WITH m.codcal,  ;
             codfte WITH m.codfte,  ;
             tipfun WITH  ;
             m.tipfun
     RETURN .T.
ENDIF
RETURN .F.
*
PROCEDURE elimi_item
SELECT itepec
IF RLOCK()
     DELETE NEXT 1
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
RETURN
*
FUNCTION corri_item
REPLACE codcad WITH m.codcad
RETURN .T.
*
PROCEDURE lista
ON KEY LABEL F7
ON KEY LABEL F9
SELECT vale
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lisval
ENDIF
SELECT vale
DO vista
RETURN
*
PROCEDURE lisval
vorde = ORDER()
vrec = RECNO()
DEFINE WINDOW lis FROM 0, 15 TO  ;
       24, 65 FLOAT TITLE  ;
       'Listado Vales' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtopec, vtomes, vtoart,  ;
      vtodep, vorden, vtiplis,  ;
      vtiprep, vtotip, vtopla,  ;
      vrepo, vtocho
vcodmes = SPACE(2)
vcoddep = SPACE(6)
vcodart = SPACE(10)
vcodtip = SPACE(1)
vcodpla = SPACE(7)
vcodcho = SPACE(3)
@ 01, 01 SAY  ;
  '     Tipo Reporte : ' GET  ;
  vrepo FUNCTION  ;
  '^ General;Control;Equipo Mec'
@ 04, 01 SAY  ;
  ' Todos las Meses? : ' GET  ;
  vtomes SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtomes,5,22)
@ 05, 01 SAY  ;
  '              Mes : '
@ 05, 22 GET vcodmes PICTURE '!!'  ;
  VALID val_para(vcodmes,'FECMES', ;
  'C') WHEN vtomes = 2
@ 06, 01 SAY  ;
  '  Todas Oficinas? : ' GET  ;
  vtodep SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtodep,7,22)
@ 07, 01 SAY  ;
  '          Oficina : '
@ 07, 22 GET vcoddep PICTURE  ;
  '!!!!!!' VALID val_para(vcoddep, ;
  'CODDEP','C') WHEN vtodep = 2
@ 08, 01 SAY  ;
  ' Todos Articulos? : ' GET  ;
  vtoart SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtoart,9,22)
@ 09, 01 SAY  ;
  '         Art¡culo : '
@ 09, 22 GET vcodart PICTURE  ;
  '!!!!!!!!!!' VALID fbut() WHEN  ;
  vtoart = 2
@ 10, 01 SAY  ;
  ' Todos los Tipos? : ' GET  ;
  vtotip SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtotip,11,22)
@ 11, 01 SAY  ;
  '             Tipo : '
@ 11, 22 GET vcodtip PICTURE '!'  ;
  VALID val_para(vcodtip,'TIPVAL', ;
  ' ',22,15) WHEN vtotip = 2
@ 12, 01 SAY  ;
  'Todas las Placas? : ' GET  ;
  vtopla SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtopla,13,22)
@ 13, 01 SAY  ;
  '            Placa : '
@ 13, 22 GET vcodpla PICTURE  ;
  '!!!!!!!' VALID val_pla(vcodpla, ;
  'C') WHEN vtopla = 2
@ 14, 01 SAY  ;
  '  Todos Choferes? : ' GET  ;
  vtocho SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocho,15,22)
@ 15, 01 SAY  ;
  '           Chofer : '
@ 15, 22 GET vcodcho PICTURE  ;
  '!!!' VALID val_para(vcodcho, ;
  'CODCHF',' ',22,50) WHEN vtocho =  ;
  2
@ 16, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Vale;Dependencia;Placa;Fecha'
@ 19, 01 SAY  ;
  '           Estado : ' GET  ;
  vtiplis FUNCTION  ;
  '^ Todos;Pendientes;Atendidos'
@ 22, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1 .AND. LASTKEY() <>  ;
   27
     SELECT vale
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     vind = SYS(3) + '.IDX'
     INDEX ON IIF(vorden = 1,  ;
           periodo + codval,  ;
           IIF(vorden = 2, coddep,  ;
           IIF(vorden = 3, codpla,  ;
           fecval))) TO (vind)  ;
           FOR IIF(vtiplis = 1,  ;
           .T., IIF(vtiplis = 2,  ;
           estado = '00', estado =  ;
           '50')) .AND.  ;
           IIF(vtopla = 1, .T.,  ;
           codpla =  ;
           ALLTRIM(vcodpla))
     SET FILTER TO IIF(vtoart = 1,;
.T., codart = ALLTRIM(vcodart));
.AND. IIF(vtodep = 1,;
.T., coddep = ALLTRIM(vcoddep));
.AND. IIF(vtomes = 1,;
.T., MONTH(fecval) = VAL(vcodmes));
.AND. IIF(vtotip = 1,;
.T., tipval = ALLTRIM(vcodtip));
.AND. IIF(vtocho = 1,;
.T., codchf = ALLTRIM(vcodcho))
     SET INDEX TO (vind)
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtiplis = 1,  ;
               'Vales en General ',  ;
               IIF(vtiplis = 2,  ;
               ' Vales Pendientes ',  ;
               ' Vales Atendidos ' ;
               ))
     vmes = IIF(vtomes = 1, ' ',  ;
            'Mes de :' +  ;
            ALLTRIM(val_para(ALLTRIM(vcodmes), ;
            'FECMES','D',22, ;
            60)))
     IF  .NOT. EOF()
          DO CASE
               CASE vrepo = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'LisVal',  ;
                       ' Vales '
               CASE vrepo = 2
                    DO reporte  ;
                       WITH 2,  ;
                       'LisVal1',  ;
                       ' Vales '
               CASE vrepo = 3
          ENDCASE
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     CLOSE INDEX
     ERASE (vind)
ENDIF
SELECT vale
GOTO TOP
GOTO vrec
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
ON KEY LABEL F7
ON KEY LABEL F9
ON KEY LABEL F4
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
ON KEY LABEL F7
ON KEY LABEL F9
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_c1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION validar
PRIVATE vfun
vali = ALIAS()
vrec = RECNO()
vkey = 'B' + m.codart
SELECT iteart
SEEK vkey
vfun = .T.
IF FOUND()
     DO standby WITH  ;
        'El C¢digo ya Existe..'
     vfun = .F.
ENDIF
SELECT vali
RETURN vfun
*
FUNCTION val_artq
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
v_ent = .F.
IF  .NOT. medita
     SET ORDER TO ITEART5
     SEEK xcod
     v_fun = IIF(FOUND(), descri,  ;
             '')
     v_ent = FOUND()
ELSE
     IF EMPTY(xcod)
          SET ORDER TO ITEART6
          GOTO TOP
          ACTIVATE SCREEN
          ON KEY LABEL F10 KEYBOARD CHR(23)
          ON KEY LABEL F2 DO FunBusDet
          ON KEY LABEL F5
          ON KEY LABEL F8
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
                 '²²²² [F10] Selecciona   [F2] Buscar ²²²²'  ;
                 NOLGRID
          ON KEY LABEL F10
          ON KEY LABEL F2
          RELEASE WINDOW _busart
          SET ORDER TO 3
          IF LASTKEY() = 27
               v_fun = .F.
               v_ent = .F.
          ELSE
               xcod = codart
               xdes = descri
               xuni = coduni
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
               v_ent = .T.
          ENDIF
     ELSE
          SET ORDER TO ITEART5
          SEEK xcod
          v_ent = FOUND()
          v_fun = FOUND()
     ENDIF
ENDIF
SELECT vale
m.codart = iteart.codart
m.descri = iteart.descri
m.unimed = iteart.coduni
SELECT (malias)
SET ORDER TO (ord)
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
RETURN v_fun
*
FUNCTION fbut
SELECT iteart
SET ORDER TO iteart3
SEEK vcodart
IF  .NOT. FOUND() .OR.  ;
    EMPTY(vcodart)
     SET ORDER TO iteart2
     DO funbusdet
     BROWSE FIELDS iteart.codart  ;
            :H = 'Articulo' : 10,  ;
            iteart.descri :H =  ;
            'Descripci¢n' : 35,  ;
            produ.codart :H =  ;
            'Det' : 7,  ;
            produ.descri :H =  ;
            'Detalle' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_4
     SET ORDER TO iteart1
     SET RELATION TO
ENDIF
vcodart = iteart.codart
RETURN .T.
*

USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 2 Factura ALIAS factura  ;
    ORDER Factura1
USE IN 3 vale ALIAS vale ORDER  ;
    vales1
USE IN 4 Regveh ALIAS vehi ORDER  ;
    RegVeh1
USE IN 5 calen ALIAS calen ORDER  ;
    calen3
USE IN 6 Pecosa ALIAS pecosa  ;
    ORDER Pecosa1
USE IN 7 ItePec ALIAS itepec  ;
    ORDER ItePec1
USE IN 8 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 9 IteArt ALIAS iteart  ;
    ORDER IteArt1
USE IN 10 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 11 Maepre ALIAS maepre  ;
    ORDER Maepre1
ON KEY LABEL F4 do imprimir
ON KEY LABEL F9 DO VISTA_DET
vmens01 = ' Fichas : REVISION '
vmens02 = 'Registro de fichas'
vmens04 = 'Dicho ficha no fue encontrado'
vmens05 = 'No existe ficha anterior'
vmens06 = 'No existe ficha siguiente'
vmens07 = '¨ Desea Anular ‚sta ficha ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta ficha ha sido anulada'
vmens10 = 'La ficha ya est  Atendida'
vmens11 = 'La ficha ha sido devuelta'
vmens12 = 'El ficha ya tiene Pecosa'
SELECT factura
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular           Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 12, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 13, 00  ;
       TO 23, 79 TITLE  ;
       ' Detalle: Vales         ®F9¯ Detalle : Item         ®F4¯ Imprime '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 20, 66  ;
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
       '\<Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT  ;
       '\<Ingresa' AT 24, 45
DEFINE PAD anula OF mmenu PROMPT  ;
       'a\<Nular ' AT 24, 54
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO corri
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD anula OF mmenu DO anula
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_1
CLEAR
@ 1, 2 SAY '              Mes :'
@ 1, 40 SAY '            Fecha :'
@ 2, 2 SAY '           Fuente :'
@ 3, 2 SAY '       Tipo Gasto :'
@ 4, 2 SAY '      Dependencia :'
@ 5, 2 SAY '           C¢digo :'
@ 6, 2 SAY '         Producto :'
@ 7, 2 SAY '              Del :'
@ 7, 40 SAY '               Al :'
@ 8, 2 SAY '     Documento N§ :'
@ 8, 40 SAY '      Total Vales :'
@ 9, 2 SAY '  Total Requerido :'
@ 9, 40 SAY '  Precio Unitario :'
@ 10, 2 SAY '          Destino :'
RETURN
*
PROCEDURE vista
SELECT factura
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
@ 1, 22 SAY m.nummes
@ 1, 60 SAY m.fecfac
@ 2, 22 SAY val_para(m.codfte, ;
  'CODFTE','V',22,40)
@ 4, 22 SAY val_para(m.coddep, ;
  'CODDEP','V',22,49,7)
@ 5, 22 SAY m.codart
@ 6, 22 SAY m.descri
@ 7, 22 SAY m.fecini
@ 7, 60 SAY m.fecfin
@ 8, 22 SAY m.numdoc
@ 8, 60 SAY m.totvales PICTURE  ;
  '999'
@ 9, 22 SAY m.canreq PICTURE  ;
  '999,999.999'
@ 9, 60 SAY m.preuni PICTURE  ;
  '999,999.999'
@ 10, 22 SAY m.destino
DO vista_hijo
RETURN
*
PROCEDURE total
IF m.valtot <> 0
     ACTIVATE WINDOW wind_3
     @ 0, 0 SAY m.valtot PICTURE  ;
       '999,999.999'
ELSE
     DEACTIVATE WINDOW wind_3
ENDIF
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT vale
SET ORDER TO VALES1
GOTO TOP
BROWSE NOOPTIMIZE NOMENU NOAPPEND  ;
       NOEDIT NODELETE NOCLEAR  ;
       KEY ALLTRIM(m.nummes)  ;
       NOREFRESH
SELECT factura
RETURN
*
PROCEDURE vista_det
HIDE POPUP ALL
ON KEY LABEL F9
SELECT vale
SET ORDER TO VALES1
GOTO TOP
BROWSE NOOPTIMIZE FIELDS codval  ;
       :H = 'Vale', fecval :H =  ;
       'Fecha', codprv :H =  ;
       'Provd', codpla :H =  ;
       'Placa', canreq :H =  ;
       'Canreq' :P = '9,999.999',  ;
       valtot :H = 'Total' :P =  ;
       '999,999.99', xx =  ;
       val_para(codchf,'CODCHF', ;
       'D',22,60) :H = 'Chofer'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_2 KEY m.nummes +  ;
       m.codart + m.coddep  ;
       NOREFRESH
SELECT factura
ON KEY LABEL F9 DO VISTA_DET
DO vista
RETURN
*
PROCEDURE revis
SELECT factura
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SET RELATION TO nummes + codart + coddep;
INTO vale
SET SKIP TO vale
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS fecfac :H = 'Fecha',  ;
       coddep :H = 'DEP', codart  ;
       :H = 'Art¡culo',  ;
       vale.codval :H = 'Vale',  ;
       vale.fecval :H = 'Fecha',  ;
       vale.codprv :H = 'Provd',  ;
       vale.codpla :H = 'Placa',  ;
       vale.canreq :H = 'Canreq'  ;
       :P = '9,999.999',  ;
       vale.valtot :H = 'Total'  ;
       :P = '999,999.99', xx =  ;
       val_para(vale.codchf, ;
       'CODCHF','D',22,60) :H =  ;
       'Chofer' : 25, valtot :H =  ;
       'Total' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELECT factura
SET RELATION TO
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
ON KEY LABEL F7
ON KEY LABEL F9
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnum_pec = '    '
vcod_fte = '  '
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero Pecosa: ' GET  ;
  vperiodo PICTURE '!!'
@ 1, 27 SAY '-' GET vnum_pec  ;
  PICTURE '!!!!' VALID vbusca()
@ 1, 33 SAY '-' GET vcod_fte  ;
  PICTURE '!!' VALID  ;
  val_para(vcod_fte,'CODFTE','C', ;
  33,20)
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_pec) .OR. LASTKEY() =  ;
   27
     ON KEY LABEL F9 DO vista_det
     RETURN
ELSE
     SEEK vperiodo + vnum_pec
     IF  .NOT. FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
ON KEY LABEL F9 DO vista_det
RETURN
*
FUNCTION vbusca
vnum_pec = PADL(ALLTRIM(vnum_pec),  ;
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
DEACTIVATE WINDOW wind_3
SELECT factura
vtemp = RECNO()
DO pantalla
SCATTER MEMVAR
@ 1, 22 GET m.nummes PICTURE '!!'  ;
  DISABLE VALID val_para(m.nummes, ;
  'FECMES',' ',22,50) .AND.  ;
  ing_fec()
@ 1, 60 GET m.fecfac
@ 4, 22 GET m.coddep PICTURE  ;
  '!!!!!!' DISABLE VALID  ;
  val_para(m.coddep,'CODDEP',' ', ;
  22,49,7)
@ 5, 22 GET m.codart PICTURE  ;
  '!!!!!!!!!!' VALID  ;
  val_art(m.codart,.T.)
@ 6, 22 GET m.descri PICTURE  ;
  '@S56'
@ 7, 22 GET m.fecini
@ 7, 60 GET m.fecfin
@ 8, 22 GET m.numdoc
@ 9, 60 GET m.preuni PICTURE  ;
  '999,999.999'
@ 10, 22 GET m.destino PICTURE  ;
  '@S56'
READ VALID val_read()
IF LASTKEY() <> 27
     DO WHILE .T.
          ok = corrije_hj()
          IF LASTKEY() <> 27
               IF yesno( ;
                  '¨ Conforme la correcci¢n ?' ;
                  )
                    EXIT
               ENDIF
          ELSE
               IF yesno( ;
                  '¨ Cancela la correcci¢n ?' ;
                  )
                    ok = .F.
                    EXIT
               ENDIF
          ENDIF
     ENDDO
     IF ok .AND. LASTKEY() <> 27
          SELECT vale
          SET ORDER TO VALEs2
          SEEK ALLTRIM(m.nummes) +  ;
               ALLTRIM(m.codart) +  ;
               ALLTRIM(m.coddep)
          vtotal = 0
          vreq = 0
          ving = 0
          SCAN FOR nummes =  ;
               ALLTRIM(m.nummes)  ;
               .AND. codart =  ;
               ALLTRIM(m.codart)  ;
               .AND. coddep =  ;
               ALLTRIM(m.coddep)
               IF flag = 'û'
                    IF RLOCK()
                         REPLACE preuni  ;
                                 WITH  ;
                                 m.preuni,  ;
                                 valtot  ;
                                 WITH  ;
                                 preuni *  ;
                                 candesp,  ;
                                 estado  ;
                                 WITH  ;
                                 '50'
                         ving = ving +  ;
                                1
                    ENDIF
                    vtotal = vtotal +  ;
                             vale.valtot
                    vreq = vreq +  ;
                           vale.candesp
               ENDIF
          ENDSCAN
          SET ORDER TO VALES1
          SELECT factura
          m.valtot = vreq *  ;
                     m.preuni
          m.canreq = vreq
          m.totvales = ving
          m.user = SYS(0)
          m.user_fc = DATE()
          m.user_tp = 'C'
          IF ving > 0
               GATHER MEMVAR
          ENDIF
     ELSE
          SELECT factura
     ENDIF
ENDIF
DO vista
UNLOCK
RETURN
*
PROCEDURE ingre
DEACTIVATE WINDOW wind_3
SELECT factura
vtemp = RECNO()
DO pantalla
SCATTER BLANK MEMVAR
m.fecfac = DATE()
@ 1, 22 GET m.nummes PICTURE '!!'  ;
  VALID val_para(m.nummes, ;
  'FECMES',' ',22,50) .AND.  ;
  ing_fec()
@ 1, 60 GET m.fecfac
@ 2, 22 GET m.codfte PICTURE '!!'  ;
  VALID val_para(m.codfte, ;
  'CODFTE',' ',22,30)
@ 4, 22 GET m.coddep PICTURE  ;
  '!!!!!!' VALID  ;
  val_para(m.coddep,'CODDEP',' ', ;
  22,49,7)
READ VALID val_read()
IF LASTKEY() <> 27
     DO WHILE .T.
          ok = trabaja_hj()
          IF LASTKEY() <> 27
               IF yesno( ;
                  '¨ Confirme el ingreso ?' ;
                  )
                    EXIT
               ENDIF
          ELSE
               DO standby WITH  ;
                  ' Cancelado el Ingreso ..'
               ok = .F.
               EXIT
          ENDIF
     ENDDO
     IF ok .AND. LASTKEY() <> 27
          SELECT vale
          SET ORDER TO VALEs2
          SEEK ALLTRIM(m.nummes) +  ;
               ALLTRIM(m.codart) +  ;
               ALLTRIM(m.coddep)
          vtotal = 0
          vreq = 0
          ving = 0
          SCAN FOR nummes =  ;
               ALLTRIM(m.nummes)  ;
               .AND. codart =  ;
               ALLTRIM(m.codart)  ;
               .AND. coddep =  ;
               ALLTRIM(m.coddep)
               IF flag = 'û'
                    IF RLOCK()
                         REPLACE estado  ;
                                 WITH  ;
                                 '50',  ;
                                 preuni  ;
                                 WITH  ;
                                 m.preuni,  ;
                                 valtot  ;
                                 WITH  ;
                                 preuni *  ;
                                 candesp
                         ving = ving +  ;
                                1
                    ENDIF
                    vtotal = vtotal +  ;
                             vale.valtot
                    vreq = vreq +  ;
                           vale.candesp
               ENDIF
          ENDSCAN
          SET ORDER TO VALES1
          SELECT factura
          m.valtot = vreq *  ;
                     m.preuni
          m.canreq = vreq
          m.estado = '00'
          m.totvales = ving
          m.user = SYS(0)
          m.user_fc = DATE()
          m.user_tp = 'I'
          IF ving > 0
               IF f_appd()
                    GATHER MEMVAR
               ENDIF
          ENDIF
     ELSE
          DO standby WITH  ;
             'Proceso cancelado'
          SELECT factura
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
     SELECT factura
ENDIF
DO vista
RETURN
*
PROCEDURE ing_fec
xmes = ALLTRIM(m.nummes)
ymes = PADL(ALLTRIM(STR((VAL(m.nummes) +  ;
       1), 2)), 2, '0')
m.fecini = ctod('01-&xmes-97')
m.fecfin = ctod('01-&ymes-97') - 1
RETURN
*
PROCEDURE trabaja_hj
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F11->Marca°°°°°°°°°°°°°°F12->Desmarca°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F11 DO Marca
ON KEY LABEL F12 DO Desmarca
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT vale
SET ORDER TO vales1
GOTO TOP
SET FILTER TO BETWEEN(fecval, m.fecini,;
m.fecfin)
BROWSE FIELDS flag :H = 'OK' :W =  ;
       .F., codval :H = ' N§ ' :W =  ;
       .F., fecval :H = 'Fecha'  ;
       :W = .F., codpla :H =  ;
       'Placa' :W = .F., canreq  ;
       :H = 'Cantid' :P =  ;
       '99,999.99' :W = .F.,  ;
       candesp :H = 'Despac' :P =  ;
       '99,999.99', valtot :H =  ;
       'Valor', prv =  ;
       val_prv(codprv) :H =  ;
       'Proveedor' : 32 :W = .F.  ;
       NOMENU NOAPPEND NODELETE  ;
       WINDOW wind_2 KEY  ;
       ALLTRIM(m.nummes) +  ;
       ALLTRIM(m.codart) +  ;
       ALLTRIM(m.coddep)  ;
       NOREFRESH
SET FILTER TO
ON KEY LABEL F11
ON KEY LABEL F12
ON KEY LABEL F10
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT vale
SET ORDER TO vales1
RETURN
*
PROCEDURE corrije_hj
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agrega°°°°°°°°°°°°°°F12->Desmarca°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL f5 do AGREG_VAL
ON KEY LABEL f12 do desmarca
SELECT vale
SET ORDER TO vales1
GOTO TOP
SET FILTER TO BETWEEN(fecval, m.fecini,;
m.fecfin)
BROWSE FIELDS flag :H = 'OK' :W =  ;
       .F., codval :H = ' N§ ' :W =  ;
       .F., fecval :H = 'Fecha'  ;
       :W = .F., codpla :H =  ;
       'Placa' :W = .F., canreq  ;
       :H = 'Cantid' :P =  ;
       '99,999.99' :W = .F.,  ;
       candesp :H = 'Despac' :P =  ;
       '99,999.99', valtot :H =  ;
       'Valor', prv =  ;
       val_prv(codprv) :H =  ;
       'Proveedor' : 32 :W = .F.  ;
       NOMENU NOAPPEND NODELETE  ;
       WINDOW wind_2 KEY  ;
       ALLTRIM(m.nummes) +  ;
       ALLTRIM(m.codart) +  ;
       ALLTRIM(m.coddep)  ;
       NOREFRESH
SET FILTER TO
ON KEY LABEL F10
ON KEY LABEL F12
ON KEY LABEL F5
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT vale
SET ORDER TO vales1
RETURN
*
PROCEDURE agreg_val
oq = trabaja_hj()
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°[Enter] Continua°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
RETURN
*
PROCEDURE anula
SELECT factura
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado <> '00'
     DO standby WITH vmens10
     RETURN
ENDIF
IF yesno( ;
   '¨ Desea ANULAR ‚sta Ficha ?' ;
   )
     SELECT vale
     SET ORDER TO VALEs1
     SEEK ALLTRIM(m.nummes) +  ;
          ALLTRIM(m.codart) +  ;
          ALLTRIM(m.coddep)
     vtotal = 0
     vreq = 0
     ving = 0
     SCAN FOR nummes =  ;
          ALLTRIM(m.nummes) .AND.  ;
          codart =  ;
          ALLTRIM(m.codart) .AND.  ;
          coddep =  ;
          ALLTRIM(m.coddep)
          IF RLOCK()
               REPLACE preuni  ;
                       WITH 0,  ;
                       valtot  ;
                       WITH 0,  ;
                       estado  ;
                       WITH '00',  ;
                       flag WITH  ;
                       ' '
          ENDIF
     ENDSCAN
     SET ORDER TO VALES1
     SELECT factura
     IF RLOCK()
          DELETE NEXT 1
     ENDIF
ENDIF
SELECT factura
DO vista
UNLOCK
RETURN
*
PROCEDURE imprimir
PRIVATE vcon
SELECT vale
SET ORDER TO VALES5
SELECT factura
vcon = RECNO()
SCATTER MEMVAR
vnumc = ALLTRIM(m.nummes) +  ;
        ALLTRIM(m.codart) +  ;
        ALLTRIM(m.coddep)
SET RELATION TO nummes + codart + coddep;
INTO vale
SET FILTER TO nummes + codart + coddep;
= vnumc
SET SKIP TO vale
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     IF  .NOT. yesno( ;
         '¨Reporte detallado?')
          DO reporte WITH 2,  ;
             'agrupa',  ;
             ' Agrupa Vales ', 1
     ELSE
          DO reporte WITH 2,  ;
             'agrupat',  ;
             ' Agrupa Vales Detallado ',  ;
             1
     ENDIF
ENDIF
SET SKIP TO
SET FILTER TO
SET RELATION TO
SELECT factura
GOTO vcon
DO vista
RETURN
*
PROCEDURE lista
ON KEY LABEL F7
ON KEY LABEL F9
SELECT pecosa
vtemp = RECNO()
SET RELATION TO periodo + numpec + codfte;
INTO itepec
SET SKIP TO itepec
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lispec
ENDIF
SELECT pecosa
SET RELATION TO
SET FILTER TO
DO vista
RETURN
*
PROCEDURE lispec
vorde = ORDER()
vrec = RECNO()
DEFINE WINDOW lis FROM 0, 15 TO  ;
       24, 65 FLOAT TITLE  ;
       'Listado Pecosas' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtopec, vtomes, vtofue,  ;
      vtodep, vorden, vtiplis,  ;
      vtiprep
vnumpec = SPACE(4)
vfte = SPACE(2)
vcodmes = SPACE(2)
vperiod = SPACE(2)
vcoddep = SPACE(6)
vcodfte = SPACE(2)
@ 01, 01 SAY  ;
  'Todas las Pecosas : ' GET  ;
  vtopec SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtopec,2,22) .AND.  ;
  assig()
@ 02, 01 SAY  ;
  '           Pecosa : '
@ 02, 22 GET vnumpec PICTURE  ;
  '!!!!' WHEN vtopec = 2
@ 02, 27 GET vperiod PICTURE '!!'  ;
  WHEN vtopec = 2
@ 02, 30 SAY '-'
@ 02, 31 GET vfte PICTURE '!!'  ;
  VALID val_para(vfte,'CODFTE', ;
  'C') .AND. valpeco() WHEN  ;
  vtopec = 2
@ 04, 01 SAY  ;
  '  Todos las Meses : ' GET  ;
  vtomes SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtomes,5,22) WHEN vtopec =  ;
  1
@ 05, 01 SAY  ;
  '              Mes : '
@ 05, 22 GET vcodmes PICTURE '!!'  ;
  VALID val_para(vcodmes,'FECMES', ;
  'C') WHEN vtopec = 1 .AND.  ;
  vtomes = 2
@ 07, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtofue,8,22) WHEN vtopec =  ;
  1
@ 08, 01 SAY  ;
  '           Fuente : '
@ 08, 22 GET vcodfte PICTURE '!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  'C') WHEN vtopec = 1 .AND.  ;
  vtofue = 2
@ 10, 01 SAY  ;
  'Todas las Dependc : ' GET  ;
  vtodep SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtodep,11,22) WHEN  ;
  vtopec = 1
@ 11, 01 SAY  ;
  '      Dependencia : '
@ 11, 22 GET vcoddep PICTURE  ;
  '!!!!!!' VALID val_para(vcoddep, ;
  'CODDEP','C') WHEN vtopec = 1  ;
  .AND. vtodep = 2
@ 13, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Numero;Dependencia;Emision'  ;
  WHEN vtopec = 1
@ 16, 01 SAY  ;
  '           Estado : ' GET  ;
  vtiplis FUNCTION  ;
  '^ Todos;Pendientes;Atendidos;Liquidados'  ;
  WHEN vtopec = 1
@ 19, 01 SAY  ;
  '           Estado : ' GET  ;
  vtiprep FUNCTION  ;
  '^ Resumido;Detallado' WHEN  ;
  vtopec = 1
@ 22, 10 GET okcancel DEFAULT 1  ;
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
     INDEX ON IIF(vorden = 1,  ;
           periodo + numpec,  ;
           IIF(vorden = 2, coddep,  ;
           DTOS(fecemi))) TO  ;
           (vind) FOR IIF(vtopec =  ;
           1, .T., periodo +  ;
           numpec + codfte =  ;
           vperiod + vnumpec +  ;
           vfte) .AND.  ;
           IIF(vtiplis = 1, .T.,  ;
           IIF(vtiplis = 2,  ;
           estado = '00',  ;
           IIF(vtiplis = 3,  ;
           estado = '40', estado =  ;
           '50')))
     SET FILTER TO IIF(vtomes = 1,;
.T., IIF(vtiplis = 4, MONTH(fecdesp),;
MONTH(fecpec)) = VAL(vcodmes));
.AND. IIF(vtomes = 1,;
.T., MONTH(fecpec) = VAL(vcodmes));
.AND. IIF(vtofue = 1,;
.T., codfte = ALLTRIM(vcodfte));
.AND. IIF(vtodep = 1,;
.T., coddep = ALLTRIM(vcoddep))
     SET INDEX TO (vind)
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtiplis = 1,  ;
               ' en General ',  ;
               IIF(vtiplis = 2,  ;
               ' Pendientes ',  ;
               ' Atendidos '))
     IF  .NOT. EOF()
          IF vtopec = 1
               IF vtiprep = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'LisPecX',  ;
                       ' Pe.co.sa ',  ;
                       1, .F.,  ;
                       .T.
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'LisPec',  ;
                       ' Pe.co.sa ',  ;
                       1, .F.,  ;
                       .T.
               ENDIF
          ELSE
               DO reporte WITH 2,  ;
                  'LisPec1',  ;
                  ' Pe.co.sa '
          ENDIF
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     CLOSE INDEX
     ERASE (vind)
ENDIF
SELECT pecosa
SET ORDER TO (vorde)
GOTO TOP
GOTO vrec
RETURN
*
FUNCTION assig
vnumpec = pecosa.numpec
vperiod = pecosa.periodo
vfte = pecosa.codfte
RETURN .T.
*
PROCEDURE valpeco
SELECT pecosa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vnumpec = PADL(ALLTRIM(vnumpec),  ;
          4, '0')
SEEK '97' + vnumpec
IF  .NOT. FOUND()
     SET FILTER TO codfte = ALLTRIM(vfte)
     SET RELATION TO periodo + numpec;
INTO itepec
     SET SKIP TO itepec
     vtemp = RECNO()
     HIDE MENU mmenu
     ACTIVATE SCREEN
     vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     ON KEY LABEL F10 KEYBOARD CHR(23)
     BROWSE FIELDS numpec :H =  ;
            ' N§ ', est =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'S/Ct',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            ' -  ')))) :H =  ;
            'ESTD', codcal :H =  ;
            'Calendario', fecpec  ;
            :H = 'Fecha', coddep  ;
            :H = 'DEP',  ;
            itepec.canreq :H =  ;
            'Cantidad' :P =  ;
            '99,999',  ;
            itepec.descri :H =  ;
            'Detalle ' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_0
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     SELECT pecosa
     SET RELATION TO
     SET FILTER TO
ENDIF
vnumpec = numpec
vfte = codfte
SHOW MENU mmenu
ON KEY LABEL F10
SELECT pecosa
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
RELEASE WINDOW wind_2
RELEASE WINDOW wind_3
RELEASE WINDOW wind_4
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION valpec
PARAMETER vnumpec, alis
alis = ALIAS()
PRIVATE vfun
vfun = .T.
m.numpec = PADL(ALLTRIM(STR(vnumpec,  ;
           4)), 4, '0')
IF m.numpec = '0000' .OR.  ;
   EMPTY(m.numpec)
     vfun = .F.
ENDIF
SELECT parma
SEEK 'CORREL' + 'PECOSA'
REPLACE nument WITH nument + 1
SELECT (alis)
RETURN vfun
*
PROCEDURE valult
PARAMETER vrec
vrec = RECNO()
SELECT pecosa
SET ORDER TO pecosa1
GOTO BOTTOM
vnumpec = VAL(numpec) + 1
m.numpec = PADL(ALLTRIM(STR(vnumpec,  ;
           4)), 4, '0')
IF m.numpec = '0000' .OR.  ;
   EMPTY(m.numpec)
     vfun = .F.
ENDIF
*
FUNCTION repasa
PARAMETER vrec, vfun, vali, as
vfun = .T.
vrec = RECNO()
vali = ALIAS()
SELECT pecosa
SET ORDER TO pecosa1
SEEK '970482'
as = RECNO()
GOTO as
numr = 0482
DO WHILE .T.
     IF VAL(numpec) = numr
          numr = numr + 1
          SKIP
          LOOP
     ELSE
          EXIT
     ENDIF
ENDDO
m.numpec = PADL(ALLTRIM(STR(numr,  ;
           4)), 4, '0')
IF m.numpec = '0000' .OR.  ;
   EMPTY(m.numpec)
     vfun = .F.
ELSE
     IF f_appd()
          REPLACE periodo WITH  ;
                  m.periodo,  ;
                  numpec WITH  ;
                  m.numpec
          gh = RECNO()
     ENDIF
     UNLOCK
ENDIF
SELECT parma
SEEK 'CORREL' + 'PECOSA'
REPLACE nument WITH numr
SELECT (vali)
RETURN vfun
*
FUNCTION xvalart
PARAMETER _cod
dc = ALIAS()
PRIVATE xx, yy, zz, vfun
vfun = .F.
SELECT iteart
vtemp = RECNO()
SEEK 'B' + ALLTRIM(itepec.codart)
IF FOUND() .AND.  .NOT.  ;
   EMPTY(itepec.codart)
     SELECT itecn
     SEEK m.periodo +  ;
          ALLTRIM(m.coddep) +  ;
          itepec.codart
     IF FOUND()
          SELECT itepec
          IF RLOCK()
               REPLACE coduni  ;
                       WITH  ;
                       iteart.coduni,  ;
                       preuni  ;
                       WITH  ;
                       iteart.preuni,  ;
                       descri  ;
                       WITH  ;
                       iteart.descri,  ;
                       tipcdr  ;
                       WITH 'S'
          ENDIF
          vfun = .T.
     ELSE
          DO standby WITH  ;
             'Este producto no est  registrado en el Cuadro de Necesidades'
          SELECT itepec
          IF RLOCK()
               REPLACE tipcdr  ;
                       WITH 'N'
          ENDIF
          vfun = .T.
     ENDIF
ELSE
     SELECT itepec
     zz = val_para(codart, ;
          'CODGEB','C')
     IF LASTKEY() = 27
          RETURN .T.
     ENDIF
     IF zz
          xx = val_art(_cod,.F.)
          IF xx
               yy = val_artdet(SUBSTR(ALLTRIM(produ.codart),  ;
                    2, 6),.F.)
               IF yy .AND.  ;
                  m.tipdoc = 'S'
                    SELECT itecn
                    SEEK m.periodo +  ;
                         ALLTRIM(m.coddep) +  ;
                         itepec.codart
                    IF  .NOT.  ;
                        FOUND()
                         DO standby  ;
                            WITH  ;
                            'Este producto no est  registrado en el Cuadro de Necesidades'
                         SELECT itepec
                         IF RLOCK()
                              REPLACE  ;
                               coduni  ;
                               WITH  ;
                               iteart.coduni,  ;
                               preuni  ;
                               WITH  ;
                               iteart.preuni,  ;
                               descri  ;
                               WITH  ;
                               iteart.descri,  ;
                               tipcdr  ;
                               WITH  ;
                               'N'
                         ENDIF
                         vfun = .T.
                    ELSE
                         SELECT itepec
                         IF RLOCK()
                              REPLACE  ;
                               coduni  ;
                               WITH  ;
                               iteart.coduni,  ;
                               preuni  ;
                               WITH  ;
                               iteart.preuni,  ;
                               descri  ;
                               WITH  ;
                               iteart.descri,  ;
                               tipcdr  ;
                               WITH  ;
                               'S'
                         ENDIF
                         vfun = .T.
                    ENDIF
               ELSE
                    IF f_lock(1)
                         REPLACE itepec.codart  ;
                                 WITH  ;
                                 SPACE(11)
                    ENDIF
                    vfun = .F.
               ENDIF
          ELSE
               IF f_lock(1)
                    REPLACE itepec.codart  ;
                            WITH  ;
                            SPACE(11)
               ENDIF
               vfun = .F.
          ENDIF
     ELSE
          IF f_lock(1)
               REPLACE itepec.codart  ;
                       WITH  ;
                       SPACE(11)
          ENDIF
          vfun = .F.
     ENDIF
ENDIF
ON KEY
ON KEY LABEL F5 DO Agreg_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itepec
RETURN vfun
*
FUNCTION trimestre
PARAMETER vfecha
DO CASE
     CASE MONTH(vfecha) = 1 .OR.  ;
          MONTH(vfecha) = 2 .OR.  ;
          MONTH(vfecha) = 3
          vtrim = '1'
     CASE MONTH(vfecha) = 4 .OR.  ;
          MONTH(vfecha) = 5 .OR.  ;
          MONTH(vfecha) = 6
          vtrim = '2'
     CASE MONTH(vfecha) = 7 .OR.  ;
          MONTH(vfecha) = 8 .OR.  ;
          MONTH(vfecha) = 9
          vtrim = '3'
     CASE MONTH(vfecha) = 10 .OR.  ;
          MONTH(vfecha) = 11 .OR.  ;
          MONTH(vfecha) = 12
          vtrim = '4'
ENDCASE
RETURN vtrim
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
            '± Detalle Pecosa ±'  ;
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
            '± Detalle Pecosa ±'  ;
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
FUNCTION val_art
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
PROCEDURE marca
REPLACE flag WITH 'û'
RETURN
*
PROCEDURE desmarca
REPLACE flag WITH ' ', estado  ;
        WITH '00'
RETURN
*

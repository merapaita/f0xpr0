USE IN 1 CdrNec ALIAS cuadro  ;
    ORDER CdrNec1
USE IN 2 IteCn ALIAS itecn ORDER  ;
    IteCn1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 4 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 5 IteArt ALIAS iteart  ;
    ORDER IteArt3
USE IN 6 Calen ALIAS calen ORDER  ;
    calen1
vmens01 = 'Registro de Cuadro de Necesidades'
vmens02 = ' REGISTRO : Cuadro de Necesidades '
vmens04 = 'Dicho Cuadro de Necesidades no fue encontrado'
vmens05 = 'No existe Cuadro de Necesidades anterior'
vmens06 = 'No existe Cuadro de Necesidades siguiente'
vmens07 = '¨ Desea ELIMINAR ‚ste Cuadro de Necesidades ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Cuadro de Necesidades ha sido anulado'
vmens10 = 'El Cuadro de Necesidades ya fue atendido o est  en Abastecimientos'
vmens11 = 'El Cuadro de Necesidades ha sido devuelto'
SELECT cuadro
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 10, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 11, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle : Cuadro de Necesidades     ®F9¯ Detalle Item'  ;
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
DEFINE PAD elimi OF mmenu PROMPT  ;
       '\<Elimina' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Listar' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO corri
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD elimi OF mmenu DO elimi
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_1
CLEAR
@ 1, 2 SAY '            Fecha :'
@ 3, 2 SAY '          Periodo :'
@ 4, 2 SAY '      Dependencia :'
@ 5, 2 SAY '     Recepcionista:'
@ 6, 2 SAY 'N£mero Trabajador :'
@ 8, 2 SAY '    Observaciones :'
RETURN
*
PROCEDURE vista
SELECT cuadro
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
ON KEY LABEL F9 DO VISTA_DET
SCATTER MEMVAR
@ 0, 60 SAY IIF(m.estado = '00',  ;
  'Pendiente', IIF(m.estado =  ;
  '30', 'En Abastc.',  ;
  IIF(m.estado = '99', 'Anulada ',  ;
  IIF(m.estado = '50', 'Atendido',  ;
  '        '))))
@ 1, 22 SAY m.feccn
@ 3, 22 SAY m.periodo
@ 4, 22 SAY val_para(m.coddep, ;
  'CODDEP','D',22,48)
@ 5, 22 SAY m.atte
@ 6, 22 SAY m.numtra PICTURE  ;
  '9,999'
@ 8, 22 SAY m.observa
DO vista_hijo
RETURN
*
PROCEDURE vista_hijo
SELECT itecn
BROWSE NOOPTIMIZE FIELDS codart  ;
       :H = 'C¢digo', descri :H =  ;
       'Descripci¢n' : 24, coduni  ;
       :H = 'Uni' : 4, nec_1 :H =  ;
       '1§ Trim' :P = '99,999',  ;
       nec_2 :H = '2§ Trim' :P =  ;
       '99,999', nec_3 :H =  ;
       '3§ Trim' :P = '99,999',  ;
       nec_4 :H = '4§ Trim' :P =  ;
       '99,999' NOMENU NOAPPEND  ;
       NOEDIT NODELETE NOCLEAR  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.coddep  ;
       TIMEOUT 0.001  NOREFRESH
SELECT cuadro
RETURN
*
PROCEDURE vista_det
SELECT itecn
BROWSE NOOPTIMIZE FIELDS codart  ;
       :H = 'C¢digo', descri :H =  ;
       'Descripci¢n' : 24, coduni  ;
       :H = 'Uni' : 4, nec_1 :H =  ;
       '1§ Trim' :P = '99,999',  ;
       nec_2 :H = '2§ Trim' :P =  ;
       '99,999', nec_3 :H =  ;
       '3§ Trim' :P = '99,999',  ;
       nec_4 :H = '4§ Trim' :P =  ;
       '99,999' NOMENU NOAPPEND  ;
       NOEDIT NODELETE NOCLEAR  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.coddep  ;
       NOREFRESH
SELECT cuadro
*
PROCEDURE revis
SELECT cuadro
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SET RELATION TO periodo + coddep INTO;
itecn
SET SKIP TO itecn
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS periodo :H = 'Pr',  ;
       coddep :H = 'DEP', numtra  ;
       :H = 'Trabj', itecn.cannec  ;
       :H = 'Cant.' :P = '99,999',  ;
       itecn.descri :H =  ;
       'Articulo', observa :H =  ;
       'Observaciones' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
ON KEY LABEL F10
SET RELATION TO
SELECT cuadro
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = SPACE(2)
vnum_sol = SPACE(6)
ACTIVATE WINDOW standby
@ 1, 1 SAY  ;
  '            Periodo : ' GET  ;
  vperiodo PICTURE 'XX'
@ 2, 1 SAY  ;
  'Ingrese Dependencia : ' GET  ;
  vnum_sol PICTURE 'XXXXXX' VALID  ;
  val_para(vnum_sol,'CODDEP','C', ;
  24,40,7)
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_sol) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vperiodo + vnum_sol
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
IF estado = '70'
     DO standby WITH vmens11
     RETURN
ENDIF
IF estado = '50'
     DO standby WITH  ;
        'El Cuadro ya esta verificado'
     RETURN
ENDIF
IF estado = '30'
     DO standby WITH  ;
        'El Cuadro ya esta en Abastecimientos'
     RETURN
ENDIF
SELECT cuadro
SCATTER MEMVAR
ACTIVATE WINDOW wind_1
DO pantalla
IF RLOCK() .OR. f_lock(1)
     @ 1, 22 GET m.feccn
     @ 3, 22 GET m.periodo
     @ 4, 22 GET m.coddep PICTURE  ;
       '!!!!!!' VALID  ;
       val_para(m.coddep,'CODDEP', ;
       ' ',22,50,7)
     @ 5, 22 GET m.atte
     @ 6, 22 GET m.numtra PICTURE  ;
       '9,999'
     @ 8, 22 GET m.observa
     READ VALID val_read()
     IF LASTKEY() <> 27
          DO WHILE .T.
               ok = trabaja_hj()
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
          SELECT cuadro
          IF ok .AND. LASTKEY() <>  ;
             27
               GATHER MEMVAR
          ELSE
               SELECT cuadro
          ENDIF
     ELSE
          DO standby WITH  ;
             'Proceso cancelado'
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
DO vista
UNLOCK
RETURN
*
PROCEDURE ingre
SELECT cuadro
vtemp = RECNO()
DO pantalla
SCATTER BLANK MEMVAR
m.periodo = '99'
m.estado = '00'
m.feccn = DATE()
@ 1, 22 GET m.feccn
@ 3, 22 GET m.periodo PICTURE  ;
  '!!' VALID  .NOT.  ;
  EMPTY(m.periodo)
@ 4, 22 GET m.coddep PICTURE  ;
  '!!!!!!' VALID  ;
  val_para(m.coddep,'CODDEP',' ', ;
  22,40,7) WHEN  .NOT.  ;
  EMPTY(m.periodo)
READ
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
SEEK m.periodo +  ;
     ALLTRIM(m.coddep)
IF FOUND()
     DO standby WITH  ;
        'La Dependencia ya est  registrada'
     DO vista
     RETURN
ENDIF
@ 5, 22 GET m.atte
@ 6, 22 GET m.numtra PICTURE  ;
  '9,999'
@ 8, 22 GET m.observa
READ VALID val_read()
IF LASTKEY() <> 27
     IF f_appd()
          SELECT itecn
          DO agreg_item
          DO WHILE .T.
               ok = trabaja_hj()
               IF LASTKEY() <> 27
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
          IF ok .AND. LASTKEY() <>  ;
             27
               SELECT cuadro
               GATHER MEMVAR
          ELSE
               SELECT itecn
               DO elimi
               GOTO BOTTOM
          ENDIF
     ELSE
          SELECT cuadro
          GOTO BOTTOM
     ENDIF
     UNLOCK ALL
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
     GOTO vtemp
ENDIF
SELECT cuadro
DO vista
RETURN
*
PROCEDURE trabaja_hj
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO Agreg_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itecn
BROWSE FIELDS codart :H =  ;
       'C¢digo' :V =  ;
       val_artc(codart,.F.) :F :W =  ;
       EMPTY(codart), descri :H =  ;
       'Descripci¢n' : 25 :W =  ;
       .F., coduni :H = 'Uni' :W =  ;
       .F. : 4, nec_1 :H =  ;
       '1§ Trim' :P = '99,999' :W =   ;
       .NOT. EMPTY(codart), nec_2  ;
       :H = '2§ Trim' :P =  ;
       '99,999' :W =  .NOT.  ;
       EMPTY(codart), nec_3 :H =  ;
       '3§ Trim' :P = '99,999' :W =   ;
       .NOT. EMPTY(codart), nec_4  ;
       :H = '4§ Trim' :P =  ;
       '99,999' :W =  .NOT.  ;
       EMPTY(codart) NOMENU  ;
       NOAPPEND NODELETE NOCLEAR  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.coddep
SELECT itecn
SEEK m.periodo + m.coddep
SCAN WHILE m.periodo = periodo  ;
     .AND. m.coddep = coddep
     IF f_lock(1)
          IF EMPTY(codart) .OR.  ;
             nec_1 + nec_2 +  ;
             nec_3 + nec_4 = 0
               DELETE NEXT 1
          ELSE
               REPLACE cannec  ;
                       WITH nec_1 +  ;
                       nec_2 +  ;
                       nec_3 +  ;
                       nec_4
          ENDIF
     ENDIF
ENDSCAN
UNLOCK ALL
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT cuadro
RETURN
*
FUNCTION antr
vant = itecn.nec_1
RETURN .T.
*
FUNCTION agreg_item
IF f_appd()
     REPLACE periodo WITH  ;
             m.periodo, coddep  ;
             WITH m.coddep,  ;
             estado WITH '00'
ENDIF
RETURN .T.
*
PROCEDURE elimi_item
SELECT itecn
IF RLOCK()
     DELETE NEXT 1
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
RETURN
*
PROCEDURE elimi
SELECT cuadro
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado = '30' .OR. estado =  ;
   '50'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ELIMINAR FISICAMENTE ‚ste Cuadro ?' ;
           )
IF velimina .AND. (RLOCK() .OR.  ;
   f_lock(1))
     SELECT itecn
     SEEK m.periodo + m.coddep
     SCAN WHILE m.periodo =  ;
          periodo .AND. m.coddep =  ;
          coddep
          IF f_lock(1)
               DELETE NEXT 1
          ENDIF
     ENDSCAN
     SELECT cuadro
     DELETE NEXT 1
     GOTO BOTTOM
     DO vista
ENDIF
UNLOCK
RETURN
*
PROCEDURE lista
SELECT cuadro
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     SET RELATION TO periodo + coddep;
INTO itecn
     SET SKIP TO itecn
     DO liscn
     SET RELATION TO
ENDIF
SELECT cuadro
GOTO vtemp
DO vista
RETURN
*
PROCEDURE liscn
vtemo = RECNO()
DEFINE WINDOW lis FROM 5, 15 TO  ;
       19, 65 FLOAT TITLE  ;
       'Listado Cuadro de Necesidades'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro
vcli = SPACE(4)
@ 01, 01 SAY  ;
  '    Todas las C/N : ' GET  ;
  vtocli SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,3,22)
@ 03, 01 SAY  ;
  '              C/N : '
@ 03, 22 GET vcli PICTURE '!!!!'  ;
  VALID valcn() WHEN vtocli = 2
@ 05, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Dependenc;Emision' WHEN  ;
  vtocli = 1
@ 08, 01 SAY  ;
  '           Estado : ' GET  ;
  vtippro FUNCTION  ;
  '^ Todos;Pendientes;En Abastec'  ;
  WHEN vtocli = 1
@ 12, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento .... '
     vind = SYS(3) + '.IDX'
     INDEX ON IIF(vorden = 1,  ;
           coddep, feccn) TO  ;
           (vind) FOR IIF(vtocli =  ;
           1, .T., coddep = vcli)  ;
           .AND. IIF(vtippro = 1,  ;
           .T., IIF(vtippro = 2,  ;
           estado = '00', estado =  ;
           '30'))
     SET INDEX TO (vind)
     GOTO TOP
     SET RELATION TO periodo + coddep;
INTO itecn
     SET SKIP TO itecn
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtippro = 1,  ;
               ' en General ',  ;
               IIF(vtippro = 2,  ;
               ' Pendientes ',  ;
               ' Atendidos '))
     IF  .NOT. EOF()
          DO reporte WITH 2,  ;
             'LisCdr',  ;
             ' Cuadro de Necesidades ',  ;
             1, .F., .T.
     ELSE
          DO standby WITH vmens08
     ENDIF
     CLOSE INDEX
     ERASE (vind)
ENDIF
RETURN
*
FUNCTION valcn
SELECT cuadro
vtem = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SET RELATION TO periodo + coddep INTO;
itecn
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS feccn :H = 'FecCdr',  ;
       periodo :H = 'Pr', coddep  ;
       :H = 'C¢digo', xx =  ;
       val_para(coddep,'CODDEP', ;
       'D') :H = 'Dependencia',  ;
       numtra :H = 'Trabj',  ;
       observa :H =  ;
       'Observaciones' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
vcli = coddep
SHOW MENU mmenu
ON KEY LABEL F10
SET RELATION TO
GOTO vtemp
RETURN .T.
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
ON KEY LABEL F9
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_c1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION xvalartd
PARAMETER _cod
PRIVATE xx, vfun
vfun = .F.
xx = val_artdet(_cod,.F.)
IF xx
     SELECT itecn
     REPLACE coduni WITH  ;
             iteart.coduni,  ;
             preuni WITH  ;
             iteart.preuni,  ;
             codart WITH  ;
             iteart.codart,  ;
             descri WITH  ;
             iteart.descri
     vfun = .T.
ENDIF
RETURN vfun
*
FUNCTION valartd
PARAMETER _cod
dc = ALIAS()
PRIVATE xx, yy, zz, vfun
vfun = .F.
SELECT iteart
vtemp = RECNO()
SEEK ALLTRIM(itecn.codart)
IF FOUND() .AND.  .NOT.  ;
   EMPTY(itecn.codart)
     SELECT (dc)
     IF RLOCK()
          REPLACE coduni WITH  ;
                  iteart.coduni,  ;
                  preuni WITH  ;
                  iteart.preuni,  ;
                  descri WITH  ;
                  iteart.descri
     ENDIF
     UNLOCK
     vfun = .T.
ELSE
     GOTO vtemp
     SELECT (dc)
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
               IF yy
                    SELECT itecn
                    IF RLOCK()
                         REPLACE coduni  ;
                                 WITH  ;
                                 iteart.coduni,  ;
                                 preuni  ;
                                 WITH  ;
                                 iteart.preuni,  ;
                                 descri  ;
                                 WITH  ;
                                 iteart.descri
                    ENDIF
                    vfun = .T.
               ELSE
                    IF f_lock(1)
                         REPLACE itecn.codart  ;
                                 WITH  ;
                                 SPACE(11)
                    ENDIF
                    UNLOCK
                    vfun = .F.
               ENDIF
          ELSE
               IF f_lock(1)
                    REPLACE itecn.codart  ;
                            WITH  ;
                            SPACE(11)
               ENDIF
               UNLOCK
               vfun = .F.
          ENDIF
     ELSE
          IF f_lock(1)
               REPLACE itecn.codart  ;
                       WITH  ;
                       SPACE(11)
          ENDIF
          UNLOCK
          vfun = .F.
     ENDIF
ENDIF
UNLOCK ALL
ON KEY LABEL F5 DO Agreg_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
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
RETURN .T.
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
     x_fun = IIF(FOUND(), descri,  ;
             '')
     v_fun = IIF(FOUND(), .T.,  ;
             .F.)
ELSE
     IF EMPTY(xcod)
          SET ORDER TO ITEART2
          ACTIVATE SCREEN
          ON KEY LABEL F10 KEYBOARD CHR(23)
          ON KEY LABEL F8
          ON KEY LABEL F5
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
                 '²²²² [F10] Selecciona    [F2] Buscar ²²²²'  ;
                 NOLGRID
          vord = RECNO()
          GOTO TOP
          SCAN WHILE  ;
               EMPTY(descri)
               IF RLOCK()
                    DELETE NEXT 1
               ENDIF
               UNLOCK
          ENDSCAN
          GOTO TOP
          GOTO vord
          ON KEY LABEL F10
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
     ELSE
          SEEK xcod
          x_fun = IIF(FOUND(),  ;
                  descri, '')
          v_fun = IIF(FOUND(),  ;
                  .T., .F.)
     ENDIF
ENDIF
IF v_fun
     SELECT itecn
     IF RLOCK()
          REPLACE coduni WITH  ;
                  iteart.coduni,  ;
                  preuni WITH  ;
                  iteart.preuni,  ;
                  descri WITH  ;
                  iteart.descri
          IF iteart.codart =  ;
             '00.000.000'
               REPLACE canreq  ;
                       WITH -1
          ENDIF
     ENDIF
     UNLOCK
ENDIF
SELECT (malias)
SET ORDER TO (ord)
ON KEY LABEL F5 DO Agreg_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN v_fun
*

USE IN 1 CdrNec ALIAS cuadro  ;
    ORDER CdrNec1
USE IN 2 IteCn ALIAS itecn ORDER  ;
    IteCn1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 4 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 5 IteArt ALIAS iteart  ;
    ORDER IteArt1
USE IN 6 Calen ALIAS calen ORDER  ;
    calen1
USE IN 7 Itepec ALIAS itepec  ;
    ORDER Itepec13
USE IN 8 Pecosa ALIAS pecosa  ;
    ORDER Pecosa1
vmens01 = 'Verificaci¢n del Cuadro de Necesidades'
vmens02 = ' VERIFICACION : Cuadro de Necesidades '
vmens04 = 'Dicho Cuadro de Necesidades no fue encontrado'
vmens05 = 'No existe Cuadro de Necesidades anterior'
vmens06 = 'No existe Cuadro de Necesidades siguiente'
vmens07 = '¨ Desea ELIMINAR ‚ste Cuadro de Necesidades ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Cuadro de Necesidades ha sido anulado'
vmens10 = 'El Cuadro de Necesidades ya fue atendido'
vmens11 = 'El Cuadro de Necesidades ha sido devuelto'
SELECT cuadro
GOTO BOTTOM
ON KEY LABEL F4 DO IMPRIME
ON KEY LABEL F11 DO ACTUALIZA
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Verifica Elimina  Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 10, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 11, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle : CN °  ®F9¯ Detalle Item   ®F4¯ Imprime  ®F11¯ Actualiza CN'  ;
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
       '\<Verifica' AT 24, 45
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
@ 5, 2 SAY '    Recepcionista :'
@ 6, 2 SAY 'N£mero Trabajador :'
@ 8, 2 SAY '    Observaciones :'
RETURN
*
PROCEDURE imprime
SELECT cuadro
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     SET RELATION TO periodo + coddep;
INTO itecn
     SET SKIP TO itecn
     DO liscn WITH 2
     SET RELATION TO
ENDIF
SELECT cuadro
GOTO vtemp
DO vista
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
@ 4, 22 SAY m.coddep
@ 4, 29 SAY val_para(m.coddep, ;
  'CODDEP','D',29,40)
@ 5, 22 SAY m.atte
@ 6, 22 SAY m.numtra PICTURE  ;
  '9,999'
@ 8, 22 SAY m.observa
DO vista_hijo
RETURN
*
PROCEDURE vista_hijo
SELECT itecn
BROWSE NOOPTIMIZE FIELDS descri  ;
       :H = 'Descripci¢n' : 28,  ;
       coduni :H = 'Uni' : 4,  ;
       cannec :H = 'Pedido' :P =  ;
       '9,999', nec_1 :H = '1§ T'  ;
       :P = '9,999', nec_2 :H =  ;
       '2§ T' :P = '9,999', nec_3  ;
       :H = '3§ T' :P = '9,999',  ;
       nec_4 :H = '4§ T' :P =  ;
       '9,999', ajucan :H =  ;
       'Ajuste' :P = '9,999', xx =  ;
       ajucan - cannec :H = 'Dif'  ;
       :P = '9,999' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       NOCLEAR WINDOW wind_2 KEY  ;
       m.periodo + m.coddep  ;
       TIMEOUT 0.001  NOREFRESH
SELECT cuadro
RETURN
*
PROCEDURE vista_det
SELECT itecn
BROWSE NOOPTIMIZE FIELDS descri  ;
       :H = 'Descripci¢n' : 28,  ;
       coduni :H = 'Uni' : 4,  ;
       cannec :H = 'Pedido' :P =  ;
       '9,999', nec_1 :H = '1§ T'  ;
       :P = '99,999', nec_2 :H =  ;
       '2§ T' :P = '99,999',  ;
       nec_3 :H = '3§ T' :P =  ;
       '99,999', nec_4 :H =  ;
       '4§ T' :P = '99,999',  ;
       ajucan :H = 'Ajuste' :P =  ;
       '9,999', xx = ajucan -  ;
       cannec :H = 'Dif' :P =  ;
       '9,999' NOMENU NOAPPEND  ;
       NOEDIT NODELETE NOCLEAR  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.coddep  ;
       NOREFRESH
SELECT cuadro
RETURN
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
BROWSE FIELDS feccn :H = 'FecCdr',  ;
       periodo :H = 'Pr', coddep  ;
       :H = 'DEP', numtra :H =  ;
       'Trabj', itecn.cannec :H =  ;
       'Cant.' :P = '99,999',  ;
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
  22,40,7)
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_sol) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SET ORDER TO 1
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
SELECT cuadro
SCATTER MEMVAR
ACTIVATE WINDOW wind_1
@ 5, 22 GET m.atte
@ 6, 22 GET m.numtra PICTURE  ;
  '9,999'
@ 8, 22 GET m.observa
READ VALID val_read()
SELECT itecn
ok = trabaja_hi()
IF ok .AND. LASTKEY() <> 27
     GATHER MEMVAR
ENDIF
SELECT cuadro
DO vista
UNLOCK
RETURN
*
PROCEDURE ingre
SELECT cuadro
PUBLIC vestado
SCATTER MEMVAR
IF m.estado = '50'
     DO standby WITH  ;
        'El Cuadro ya esta Verificado'
ENDIF
IF m.estado = '99'
     DO standby WITH vmens09
     RETURN
ENDIF
IF m.estado = '70'
     DO standby WITH vmens11
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
@ 1, 22 GET m.feccn DISABLE
@ 3, 22 GET m.periodo DISABLE
@ 5, 22 GET m.atte
@ 6, 22 GET m.numtra PICTURE  ;
  '9,999'
@ 8, 22 GET m.observa
READ
SELECT itecn
ok = trabaja_hi()
IF ok .AND. LASTKEY() <> 27
     SELECT cuadro
     REPLACE cuadro.estado WITH  ;
             '30'
ENDIF
SELECT cuadro
DO vista
RETURN
*
FUNCTION trabaja_hi
SET RELATION TO 'B' + codart INTO iteart
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO Agreg_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS iteart.marca :H =  ;
       'Marca' :P = '@S15' :V =  ;
       marc(), descri :H =  ;
       'Descripci¢n' : 42 :W =  ;
       .F., coduni :H = 'Uni' :W =  ;
       .F. : 5, cannec :H =  ;
       'Tot.Nec' :P = '9,999' :W =  ;
       .F., nec_1 :H = '1§ T' :P =  ;
       '99,999' :W =  .NOT.  ;
       EMPTY(codart), nec_2 :H =  ;
       '2§ T' :P = '99,999' :W =   ;
       .NOT. EMPTY(codart), nec_3  ;
       :H = '3§ T' :P = '99,999'  ;
       :W =  .NOT. EMPTY(codart),  ;
       nec_4 :H = '4§ T' :P =  ;
       '99,999' :W =  .NOT.  ;
       EMPTY(codart) :V = sumar()  ;
       :F, ajucan :H = 'Ajuste'  ;
       :P = '9,999' :W =  ;
       EMPTY(codart),  ;
       iteart.preuni :H =  ;
       'Precio' :V = sumcdr() :F  ;
       :P = '99,999,999.99'  ;
       NOMENU NOAPPEND NODELETE  ;
       NOCLEAR WINDOW wind_2 KEY  ;
       m.periodo + m.coddep
SELECT itecn
vestado = 1
SEEK m.periodo + m.coddep
SCAN WHILE m.periodo = periodo  ;
     .AND. m.coddep = coddep
     IF f_lock(1)
          IF EMPTY(codart) .OR.  ;
             nec_1 + nec_2 +  ;
             nec_3 + nec_4 = 0
               DELETE NEXT 1
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
RETURN .T.
*
FUNCTION sumar
IF RLOCK()
     REPLACE ajucan WITH nec_1 +  ;
             nec_2 + nec_3 +  ;
             nec_4
ENDIF
UNLOCK
RETURN .T.
*
FUNCTION marc
IF RLOCK()
     REPLACE marca WITH  ;
             iteart.marca
ENDIF
UNLOCK
RETURN .T.
*
FUNCTION sumcdr
IF RLOCK()
     REPLACE preuni WITH  ;
             iteart.preuni,  ;
             estado WITH '00'
ENDIF
UNLOCK
RETURN .T.
*
FUNCTION agreg_item
IF f_appd()
     REPLACE periodo WITH  ;
             m.periodo, coddep  ;
             WITH m.coddep
     RETURN .T.
ENDIF
RETURN .F.
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
IF estado <> '00'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ELIMINAR FISICAMENTE ‚str Cuadro ?' ;
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
     DO liscn WITH 1
     SET RELATION TO
ENDIF
SELECT cuadro
GOTO vtemp
DO vista
RETURN
*
PROCEDURE liscn
PARAMETER vtipo
vtemo = RECNO()
DEFINE WINDOW lis FROM 5, 15 TO  ;
       19, 65 FLOAT TITLE  ;
       'Listado Ordenes de Compra'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro
vcli = cuadro.coddep
@ 01, 01 SAY  ;
  '    Todas las C/N : ' GET  ;
  vtocli SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,3,22)
@ 03, 01 SAY  ;
  '              C/N : '
@ 03, 22 GET vcli PICTURE  ;
  '!!!!!!' VALID valcn() WHEN  ;
  vtocli = 2
@ 05, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Dependenc;Emision' WHEN  ;
  vtocli = 1
@ 08, 01 SAY  ;
  '           Estado : ' GET  ;
  vtippro FUNCTION  ;
  '^ Todos;Pendientes;Atendidos'  ;
  WHEN vtocli = 1
@ 12, 10 GET okcancel DEFAULT 1  ;
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
           coddep, feccn) TO  ;
           (vind) FOR IIF(vtocli =  ;
           1, .T., coddep = vcli)  ;
           .AND. IIF(vtippro = 1,  ;
           .T., IIF(vtippro = 2,  ;
           estado = '00', estado =  ;
           '50'))
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
          IF vtipo = 1
               DO reporte WITH 2,  ;
                  'LisCdrA',  ;
                  '  Cuadro de Necesidades ',  ;
                  1, .F., .T.
          ELSE
               DO reporte WITH 2,  ;
                  'LisCdrT',  ;
                  '  Cuadro de Necesidades ',  ;
                  1, .F., .T.
          ENDIF
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
FUNCTION valartd
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
PROCEDURE actualiza
ok = ve_passw('PITIN')
IF  .NOT. ok
     RETURN
ENDIF
PRIVATE alias
alias = ALIAS()
ACTIVATE WINDOW standby
@ 1, 10 SAY  ;
  'Espere un Momento...' COLOR W+/ ;
  RB* 
SELECT itepec
SET RELATION TO periodo + numpec INTO;
pecosa
SELECT itecn
SET ORDER TO ITECN3
GOTO TOP
SCAN
     vkey = itecn.periodo +  ;
            itecn.coddep +  ;
            itecn.codart
     SELECT itepec
     p_1 = 0
     p_2 = 0
     p_3 = 0
     p_4 = 0
     SEEK vkey
     IF FOUND()
          SCAN WHILE  ;
               itepec.periodo +  ;
               itepec.coddep +  ;
               itepec.codart =  ;
               vkey
               vfec = trimestre(pecosa.fecpec)
               P_&vfec = P_&vfec + itepec.canreq
          ENDSCAN
          SELECT itecn
          REPLACE ped_1 WITH p_1,  ;
                  ped_2 WITH p_2,  ;
                  ped_3 WITH p_3,  ;
                  ped_4 WITH p_4
     ELSE
          SELECT itecn
     ENDIF
ENDSCAN
SELECT itepec
SET RELATION TO
SELECT itecn
SET ORDER TO ITECN1
SELECT (alias)
DEACTIVATE WINDOW standby
RETURN
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

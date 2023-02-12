USE IN 1 Notalm ALIAS nota ORDER  ;
    Notalm1
USE IN 2 Itealm ALIAS itealm  ;
    ORDER Itealm1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 4 IteArt ALIAS iteart  ;
    ORDER Iteart3
USE IN 8 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 14 ArtMae ALIAS produ  ;
    ORDER ArtMae1
USE IN 7 KARDEX ALIAS kardex  ;
    ORDER KARDEX5
PUBLIC vmes, vpart
vmens01 = ' Nota de Entrada de Almac‚n : REVISION '
vmens02 = ' Registro de Nota de Entrada de Almac‚n '
vmens04 = 'Dicha Nota de Entrada de Almac‚n no fue encontrado'
vmens05 = 'No existe Nota de Entrada de Almac‚n anterior'
vmens06 = 'No existe Nota de Entrada de Almac‚n siguiente'
vmens07 = '¨ Desea ANULAR ‚ste Nota de Entrada de Almac‚n ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Nota de Entrada de Almac‚n ha sido anulado'
vmens10 = 'Este Nota de Entrada de Almac‚n ya fue atendido'
vmens11 = 'Este Nota de Entrada de Almac‚n ha sido devuelto'
SELECT nota
GOTO BOTTOM
ON KEY LABEL F7 DO LIQUIDA
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula            Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00 TO 23, 79 TITLE vmens01 DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00 TO 12, 79 TITLE vmens02 DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 13, 00 TO 23, 79 TITLE ' Detalle: Nota de Entrada de Almac‚n        ®F9¯ Detalle : Item ' DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 20, 68 TO 22, 78 TITLE 'TOTAL ' COLOR SCHEME 10
DEFINE WINDOW wind_4 FROM 20, 68 TO 22, 78 TITLE 'N§PECOSA:' COLOR SCHEME 10
DEFINE WINDOW wind_5 FROM 14, 01 TO 16, 79 TITLE ' Destino '
DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis OF mmenu PROMPT '\<Revisa' AT 24, 00
DEFINE PAD busca OF mmenu PROMPT '\<Busca' AT 24, 08
DEFINE PAD anter OF mmenu PROMPT '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu PROMPT '\<Siguiente' AT 24, 25
DEFINE PAD corri OF mmenu PROMPT '\<Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT '\<Ingresa' AT 24, 45
DEFINE PAD anula OF mmenu PROMPT 'a\<Nula  ' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT '\<Listar' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT '\<Termina' AT 24, 71
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
ACTIVATE WINDOW wind_1
CLEAR
@ 1, 2 SAY '          Periodo :'
@ 2, 2 SAY '       N£mero NEA :'
@ 2, 40 SAY '            Fecha :'
@ 3, 2 SAY '      Dependencia :'
@ 4, 2 SAY 'F. Financiamiento :'
@ 5, 2 SAY '      Procedencia :'
@ 6, 2 SAY '    Con destino a :'
@ 7, 2 SAY '      Liquidaci¢n :'
@ 8, 2 SAY '            Seg£n :'
@ 9, 2 SAY '    Observaciones :'
@ 10, 00 SAY PADC( ;
  ' ° ®F7¯ Liquida ° ', 79, ' ')  ;
  COLOR W+/B 
RETURN
*
PROCEDURE vista
SELECT nota
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@ 0, 60 SAY vestna(m.estado)
@ 1, 22 SAY m.periodo
@ 2, 22 SAY m.numalm
@ 2, 60 SAY m.fecalm
@ 3, 22 SAY val_para(m.coddep, ;
  'CODDEP','D',22,50)
@ 4, 22 SAY val_para(m.codfte, ;
  'CODFTE','V',22,50)
@ 5, 22 SAY val_para(m.codprc, ;
  'CODPRC','D',22,50)
@ 6, 22 SAY m.destino PICTURE  ;
  '@S50'
@ 7, 22 SAY m.fecliq
@ 8, 22 SAY m.segun PICTURE  ;
  '@S56'
@ 9, 22 SAY m.observa PICTURE  ;
  '@S56'
DO vista_hijo
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
ON KEY LABEL F9 DO VISTA_DET
SELECT itealm
GOTO TOP
BROWSE NOOPTIMIZE FIELDS canreq  ;
       :H = 'Cantidad' :P =  ;
       '9,999.99', codart :H =  ;
       'C¢digo', coduni :H =  ;
       'Uni' :W = .F. : 3, descri  ;
       :H = 'Descripci¢n' : 31 :W =  ;
       .F., preuni :H = 'PreUni'  ;
       :P = '99,999.9999' :W =  ;
       .F., x = ROUND(canreq *  ;
       preuni, 2) :H = 'Total' :P =  ;
       '99,999.99' :W = .F.  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_2 KEY m.periodo +  ;
       m.numalm TIMEOUT 0.001   ;
       NOREFRESH
SELECT nota
RETURN
*
PROCEDURE vista_det
HIDE POPUP ALL
ON KEY LABEL F9 DO observa
SELECT itealm
GOTO TOP
BROWSE NOOPTIMIZE FIELDS canreq  ;
       :H = 'Cantidad' :P =  ;
       '9,999.99', codart :H =  ;
       'C¢digo', coduni :H =  ;
       'Uni' :W = .F. : 3, descri  ;
       :H = 'Descripci¢n' : 31 :W =  ;
       .F., preuni :H = 'PreUni'  ;
       :P = '99,999.999' :W = .F.,  ;
       x = ROUND(canreq * preuni,  ;
       2) :H = 'Total' :P =  ;
       '99,999.99' :W = .F.  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_2 KEY m.periodo +  ;
       m.numalm NOREFRESH
SELECT nota
DO vista
ON KEY LABEL F9 DO VISTA_det
RETURN
*
PROCEDURE revis
SELECT nota
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SET RELATION TO periodo + numalm INTO;
itealm
SET SKIP TO itealm
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS numalm :H = ' N§ ',  ;
       codfte :H = 'Fte', fecalm  ;
       :H = 'Fecha',  ;
       itealm.descri :H =  ;
       'Articulo ' : 36,  ;
       itealm.coduni :H = 'Unid',  ;
       itealm.canreq :H =  ;
       'Cantid' NOMENU NOAPPEND  ;
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
SELECT nota
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
vnum_nea = '    '
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero NEA : ' GET  ;
  vperiodo PICTURE '!!'
@ 1, 26 SAY '-' GET vnum_nea  ;
  PICTURE '!!!!' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_nea) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vperiodo + vnum_nea
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
vnum_nea = PADL(ALLTRIM(vnum_nea),  ;
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
*--------------
IF EOF()
    DO standby WITH vmens08
    RETURN
ENDIF
DO CASE
     CASE estado = '99'
          DO standby WITH vmens09
          RETURN
     CASE estado = '40'
          DO standby WITH  ;
             'La NEA ya est  atendida'
          RETURN
     CASE estado = '5'
          DO standby WITH  ;
             'La NEA ya est  liquidada/Contabilizada'
          RETURN
ENDCASE
SELECT nota
SCATTER MEMVAR
ACTIVATE WINDOW wind_1
DO pantalla
@ 1, 22 GET m.periodo PICTURE  ;
  '!!'
@ 2, 22 GET m.numalm
@ 2, 60 GET m.fecalm
@ 3, 22 GET m.coddep PICTURE  ;
  '!!!!!!' VALID  ;
  val_para(m.coddep,'CODDEP',' ', ;
  22,20)
@ 4, 22 GET m.codfte PICTURE '!!'  ;
  VALID val_para(m.codfte, ;
  'CODFTE',' ',22,20)
@ 5, 22 GET m.codprc PICTURE  ;
  '!!!' VALID val_para(m.codprc, ;
  'CODPRC',' ',22,20)
@ 6, 22 GET m.destino PICTURE  ;
  '@S56'
@ 8, 22 GET m.segun PICTURE  ;
  '@S56'
@ 9, 22 GET m.observa PICTURE  ;
  '@S56'
READ VALID val_read()
IF LASTKEY() <> 27
     DO WHILE .T.
          ok = trabaja_hi()
          IF LASTKEY() <> 27
               IF yesno( ;
                  '¨ Conforme la modificaci¢n ?' ;
                  )
                    EXIT
               ENDIF
          ELSE
               IF yesno( ;
                  '¨ Cancela la modificaci¢n ?' ;
                  )
                    EXIT
               ENDIF
          ENDIF
     ENDDO
     IF ok .AND. LASTKEY() <> 27
          SELECT itealm
          SEEK m.periodo +  ;
               m.numalm
          vtotal = 0
          SCAN WHILE periodo =  ;
               m.periodo .AND.  ;
               numalm = m.numalm
               IF RLOCK()
                    REPLACE valtot  ;
                            WITH  ;
                            preuni *  ;
                            canreq
               ENDIF
               UNLOCK
               vtotal = vtotal +  ;
                        valtot
          ENDSCAN
          m.nummes = PADL(ALLTRIM(STR(MONTH(m.fecalm),  ;
                     2)), 2,  ;
                     '0')
          m.valtot = vtotal
          SELECT nota
          GATHER MEMVAR
          DO ingap
     ELSE
          SELECT nota
     ENDIF
     UNLOCK ALL
ENDIF
SELECT nota
DO vista
RETURN
*
PROCEDURE ingre
SELECT nota
DO pantalla
SCATTER BLANK MEMVAR
m.fecalm = DATE()
m.periodo = RIGHT(STR(YEAR(DATE()),  ;
            4), 2)
SELECT parma
IF SEEK('CORRELNEA')
     m.numalm = PADL(ALLTRIM(STR(nument +  ;
                1)), 4, '0')
ELSE
     DO standby WITH  ;
        'Parametro de Correlativo no Existe. Favor llamar al area de Sistema'
     RETURN
ENDIF
SELECT nota
@ 1, 22 GET m.periodo PICTURE  ;
  '!!'
@ 2, 22 GET m.numalm VALID  .NOT.  ;
  EMPTY(m.numalm) .AND.  ;
  val_alm()
@ 2, 60 GET m.fecalm
@ 3, 22 GET m.coddep PICTURE  ;
  '!!!!!!' VALID  ;
  val_para(m.coddep,'CODDEP',' ', ;
  22,20)
@ 4, 22 GET m.codfte PICTURE '!!'  ;
  VALID val_para(m.codfte, ;
  'CODFTE',' ',22,20)
@ 5, 22 GET m.codprc PICTURE '!'  ;
  VALID val_para(m.codprc, ;
  'CODPRC',' ',22,20)
@ 6, 22 GET m.destino PICTURE  ;
  '@S56'
@ 8, 22 GET m.segun PICTURE  ;
  '@S56'
@ 9, 22 GET m.observa PICTURE  ;
  '@S56'
READ VALID val_read()
IF LASTKEY() <> 27
     DO WHILE .T.
          ok = trabaja_hi()
          IF LASTKEY() <> 27  ;
             .AND. ok
               IF yesno( ;
                  '¨ Confirme el ingreso ?' ;
                  )
                    EXIT
               ENDIF
          ELSE
               DO standby WITH  ;
                  ' Cancelado el Ingreso '
               ok = .F.
               EXIT
          ENDIF
     ENDDO
     IF ok .AND. LASTKEY() <> 27
          SELECT parma
          IF SEEK('CORRELNEA')
               REPLACE nument  ;
                       WITH  ;
                       nument +  ;
                       1
          ENDIF
          SELECT itealm
          SEEK m.periodo +  ;
               m.numalm
          vtotal = 0
          SCAN WHILE periodo =  ;
               m.periodo .AND.  ;
               numalm = m.numalm
               vtotal = vtotal +  ;
                        valtot
          ENDSCAN
          SELECT nota
          m.valtot = vtotal
          m.estado = '00'
          m.nummes = PADL(ALLTRIM(STR(MONTH(m.fecalm),  ;
                     2)), 2,  ;
                     '0')
          IF f_appd()
               GATHER MEMVAR
          ENDIF
          DO ingap
     ELSE
          DO standby WITH  ;
             'Proceso cancelado'
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
SELECT nota
GOTO BOTTOM
DO vista
RETURN
*
PROCEDURE ingap
PRIVATE cnum
USE IN 0 cuentas ALIAS cuenta  ;
    ORDER cuentas1
USE IN 0 astpat ALIAS astpat  ;
    ORDER Astpat36
SELECT astpat
SEEK m.periodo + m.nummes +  ;
     m.numalm + 'NEA'
IF  .NOT. FOUND()
     DO agrite
ENDIF
ON KEY LABEL F5 DO agrite
ON KEY LABEL F8 DO eliite
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS fecha :H = 'Fecha'  ;
       :F, codcta :H = 'Cuenta'  ;
       :V = val_fun('Cuenta', ;
       'Cuenta', ;
       "Cuenta+' '+Descri",codcta, ;
       2) :F, tipcta :H = 'Tp' :P =  ;
       '@M D,H', mtodeb :H =  ;
       'Monto Debe' :W = tipcta =  ;
       'D' :P = '999,999,999.99',  ;
       mtohab :H = 'Monto Haber'  ;
       :W = tipcta = 'H' :P =  ;
       '999,999,999.99' NOMENU  ;
       NOAPPEND NODELETE WINDOW  ;
       wind_2 KEY m.periodo +  ;
       m.nummes + m.numalm +  ;
       'NEA'
SEEK m.periodo + m.nummes +  ;
     m.numalm + 'NEA'
STORE 0 TO vdebe, vhaber
SCAN WHILE periodo = m.periodo  ;
     .AND. nummes = m.nummes  ;
     .AND. numref = m.numalm  ;
     .AND. tipdoc = 'NEA'
     vdebe = vdebe + IIF(tipcta =  ;
             'D', mtodeb, 0)
     vhaber = vhaber + IIF(tipcta =  ;
              'H', mtohab, 0)
ENDSCAN
IF vdebe <> vhaber
     DO standby WITH  ;
        'Ojo: No cuadra debe con haber'
ENDIF
USE IN cuenta
USE IN astpat
DEACTIVATE WINDOW wind_12
RETURN
*
PROCEDURE agrite
SELECT astpat
FOR i = 1 TO 2
     IF f_appd()
          REPLACE periodo WITH  ;
                  m.periodo,  ;
                  nummes WITH  ;
                  m.nummes,  ;
                  numref WITH  ;
                  m.numalm,  ;
                  tipdoc WITH  ;
                  'NEA', fecha  ;
                  WITH m.fecalm,  ;
                  tipcta WITH  ;
                  IIF(i = 1, 'D',  ;
                  'H')
     ENDIF
ENDFOR
RETURN
*
PROCEDURE eliite
SELECT astpat
IF RLOCK()
     DELETE NEXT 1
ENDIF
RETURN
*
FUNCTION val_alm
m.numalm = PADL(ALLTRIM(m.numalm),  ;
           4, '0')
SEEK m.periodo + m.numalm
IF  .NOT. FOUND()
     RETURN .T.
ELSE
     DO standby WITH  ;
        'La NEA se encuentra registrada'
     RETURN .F.
ENDIF
*
FUNCTION numera
SELECT nota
SEEK m.periodo + m.numoc +  ;
     ALLTRIM(m.codfte)
IF FOUND()
     DO standby WITH  ;
        'La O/C ya ha sido generada'
     RETURN .F.
ELSE
     RETURN .T.
ENDIF
*
FUNCTION trabaja_hi
PRIVATE vfun
vfun = .T.
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO Agreg_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F9 DO VisObs
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itealm
SEEK m.periodo + m.numalm
IF  .NOT. FOUND()
     DO agreg_item
ENDIF
BROWSE FIELDS codart :H =  ;
       'C¢digo' :V =  ;
       val_artc(codart,.F.) :F :W =  ;
       EMPTY(codart), descri :H =  ;
       'Descripci¢n' : 40 :W =  ;
       .F., coduni :H = 'Uni' :W =  ;
       .F. : 3, canreq :H =  ;
       'Cantd' :P = '99,999.99'  ;
       :V = canreq > 0 :W =   ;
       .NOT. EMPTY(codart),  ;
       preuni :H = 'PreUni' :P =  ;
       '99,999.9999', x =  ;
       ROUND(canreq * preuni, 2)  ;
       :H = 'Total' :P =  ;
       '99,999.99' :W = .F.  ;
       NOMENU NOAPPEND NODELETE  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.numalm  ;
       NOREFRESH
UNLOCK ALL
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F9
ON KEY LABEL F10
IF LASTKEY() = 27
     vfun = .F.
ENDIF
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT nota
RETURN vfun
*
PROCEDURE liquida
ON KEY LABEL F7
SELECT nota
SCATTER MEMVAR
ACTIVATE WINDOW wind_1
DO pantalla
@ 1, 22 GET m.periodo PICTURE  ;
  '!!' DISABLE
@ 2, 22 GET m.numalm DISABLE
@ 2, 60 GET m.fecalm DISABLE
@ 3, 22 GET m.coddep PICTURE  ;
  '!!!!!!' DISABLE VALID  ;
  val_para(m.coddep,'CODDEP',' ', ;
  22,20)
@ 4, 22 GET m.codfte PICTURE '!!'  ;
  DISABLE VALID val_para(m.codfte, ;
  'CODFTE',' ',22,20)
@ 5, 22 GET m.codprc PICTURE  ;
  '!!!' DISABLE VALID  ;
  val_para(m.codprc,'CODPRC',' ', ;
  22,20)
@ 6, 22 GET m.destino PICTURE  ;
  '@S56' DISABLE
@ 7, 22 GET m.fecliq
@ 8, 22 GET m.segun PICTURE  ;
  '@S56' DISABLE
@ 9, 22 GET m.observa PICTURE  ;
  '@S56'
READ VALID val_read()
IF LASTKEY() <> 27
     SELECT itealm
     SEEK m.periodo + m.numalm
     SCAN WHILE periodo + numalm =  ;
          m.periodo + m.numalm
          SELECT kardex
          SEEK itealm.periodo +  ;
               itealm.numalm +  ;
               itealm.codfte +  ;
               itealm.codart
          IF FOUND()
               IF RLOCK()
                    REPLACE tipdoc  ;
                            WITH  ;
                            'NEA',  ;
                            numdoc  ;
                            WITH  ;
                            m.numalm,  ;
                            fecdoc  ;
                            WITH  ;
                            m.fecalm,  ;
                            codart  ;
                            WITH  ;
                            itealm.codart,  ;
                            tipord  ;
                            WITH  ;
                            '3',  ;
                            fecent  ;
                            WITH  ;
                            m.fecliq,  ;
                            coddep  ;
                            WITH  ;
                            nota.coddep,  ;
                            entcan  ;
                            WITH  ;
                            itealm.canreq,  ;
                            preunie  ;
                            WITH  ;
                            itealm.preuni,  ;
                            entimp  ;
                            WITH  ;
                            itealm.preuni *  ;
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
                            estado  ;
                            WITH  ;
                            '  '
               ENDIF
          ELSE
               IF f_appd()
                    REPLACE tipdoc  ;
                            WITH  ;
                            'NEA',  ;
                            numdoc  ;
                            WITH  ;
                            m.numalm,  ;
                            fecdoc  ;
                            WITH  ;
                            m.fecalm,  ;
                            codart  ;
                            WITH  ;
                            itealm.codart,  ;
                            tipord  ;
                            WITH  ;
                            '3',  ;
                            fecent  ;
                            WITH  ;
                            m.fecliq,  ;
                            coddep  ;
                            WITH  ;
                            nota.coddep,  ;
                            entcan  ;
                            WITH  ;
                            itealm.canreq,  ;
                            preunie  ;
                            WITH  ;
                            itealm.preuni,  ;
                            entimp  ;
                            WITH  ;
                            itealm.preuni *  ;
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
                            estado  ;
                            WITH  ;
                            '  '
               ENDIF
               UNLOCK
          ENDIF
          SELECT itealm
          IF RLOCK()
               REPLACE estado  ;
                       WITH '50'
          ENDIF
     ENDSCAN
     SELECT nota
     m.estado = '50'
     GATHER MEMVAR
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
SELECT nota
GOTO BOTTOM
DO vista
ON KEY LABEL F7 DO LIQUIDA
RETURN
*
PROCEDURE anula
SELECT nota
PRIVATE vfun, vkey
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado <> '00'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ANULAR ‚sta Nota de Entrada de Almac‚n ?' ;
           )
IF velimina
     SELECT itealm
     SCAN WHILE periodo + numalm =  ;
          nota.periodo +  ;
          nota.numalm
          IF RLOCK()
               REPLACE estado  ;
                       WITH '99'
          ENDIF
          UNLOCK
     ENDSCAN
     SELECT nota
     IF RLOCK()
          REPLACE estado WITH  ;
                  '99', fecver  ;
                  WITH DATE()
     ENDIF
     UNLOCK
     DO vista
ENDIF
RETURN
*
FUNCTION agreg_item
PRIVATE vfun
vfun = .F.
SELECT itealm
IF f_appd()
     REPLACE numalm WITH m.numalm,  ;
             periodo WITH  ;
             m.periodo, codfte  ;
             WITH m.codfte,  ;
             estado WITH '00'
     vfun = .T.
ENDIF
UNLOCK
RETURN vfun
*
PROCEDURE elimi_item
SELECT itealm
IF RLOCK()
     DELETE NEXT 1
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
UNLOCK
RETURN
*
PROCEDURE lista
SELECT nota
SET RELATION TO periodo + numalm INTO;
itealm
vcli = numalm
SET SKIP TO itealm
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     vind = SYS(3) + '.IDX'
     INDEX ON numalm TO (vind)  ;
           FOR numalm = vcli
     SET INDEX TO (vind)
     DEACTIVATE WINDOW standby
     IF  .NOT. EOF()
          DO reporte WITH 2,  ;
             'Notalm',  ;
             ' Nota de Almacen ',  ;
             1
     ENDIF
ENDIF
SELECT nota
SET RELATION TO
SET ORDER TO NotAlm1
GOTO vtemp
DO vista
RETURN
*
PROCEDURE lisord
vorde = ORDER()
DEFINE WINDOW lis FROM 1, 15 TO  ;
       23, 65 FLOAT TITLE  ;
       'Listado Notaes de Compra'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vnota, vtippro,  ;
      vlistado, vtofue
vcli = SPACE(4)
vano = '95'
vfte = '   '
vcodfte = '   '
@ 01, 01 SAY  ;
  '     Tipo Listado : ' GET  ;
  vlistado FUNCTION  ;
  '^ por Documento;Detallado;Resumido'
@ 05, 01 SAY  ;
  '        Total NEA : ' GET  ;
  vtocli SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,7,22) WHEN  ;
  vlistado = 1
@ 07, 01 SAY  ;
  '              NEA : '
@ 07, 22 GET vcli PICTURE '!!!!'  ;
  VALID vo() .AND. valord() WHEN  ;
  vtocli = 2 .AND. vlistado = 1
@ 10, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     vind = SYS(3) + '.IDX'
     INDEX ON numalm TO (vind)  ;
           FOR IIF(vtocli = 1,  ;
           .T., numalm = vcli)
     SET INDEX TO (vind)
     COUNT ALL TO vtotoc
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtippro = 1,  ;
               'Listado Nota Compra',  ;
               IIF(vtippro = 2,  ;
               'Listado Nota de Entrada de Almac‚n Pendientes',  ;
               IIF(vtippro = 3,  ;
               'Listado de Notaes de Compra Afectadas',  ;
               IIF(vtippro = 4,  ;
               'Listado Nota de Entrada de Almac‚n Anuladas',  ;
               'Listado Nota de Entrada de Almac‚n Liquidadas' ;
               ))))
     SET MEMOWIDTH TO 43
     IF  .NOT. EOF()
          SET SKIP TO itealm
          DO CASE
               CASE vlistado = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'LisOc1',  ;
                       ' Notaes de Compra',  ;
                       1
               CASE vlistado = 2
                    DO reporte  ;
                       WITH 2,  ;
                       'LisOrdc',  ;
                       ' Notaes de Compra ',  ;
                       1, .F.,  ;
                       .T.
               CASE vlistado = 3
                    DO reporte  ;
                       WITH 2,  ;
                       'LisOrdX',  ;
                       ' Notaes de Compra ',  ;
                       1, .F.,  ;
                       .T.
          ENDCASE
          SELECT nota
     ELSE
          DO standby WITH vmens08
     ENDIF
     CLOSE INDEX
     ERASE (vind)
ENDIF
RETURN
*
FUNCTION vo
vcli = PADL(ALLTRIM(vcli), 4,  ;
       '0')
RETURN .T.
*
PROCEDURE valord
SELECT nota
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SEEK vano + vcli + ALLTRIM(vfte)
IF  .NOT. FOUND()
     SET SKIP TO itealm
     GOTO TOP
     HIDE MENU mmenu
     ACTIVATE SCREEN
     vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     ON KEY LABEL F10 KEYBOARD CHR(23)
     BROWSE FIELDS numoc :H =  ;
            ' N§ ', fecoc :H =  ;
            'Fecha', ess =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'C/c ',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            '    ')))) :H =  ;
            'Estd', itealm.descri  ;
            :H = 'Articulo ' : 36,  ;
            itealm.coduni :H =  ;
            'Unid', itealm.canreq  ;
            :H = 'Cantid' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_0
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     IF LASTKEY() = 27
          SELECT nota
          SET RELATION TO
     ENDIF
     SHOW MENU mmenu
     ON KEY LABEL F10
     SELECT nota
ENDIF
vano = nota.periodo
vcli = nota.numoc
vfte = nota.codfte
RETURN
*
PROCEDURE termi
ven_accion = .F.
ON KEY LABEL F2
ON KEY LABEL F9
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
RELEASE MENU mmenu
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
ON KEY LABEL F9
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
IF  .NOT. WEXIST('Observa')
     DEFINE WINDOW observa FROM  ;
            05, 18 TO 18, 61  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Detalle NEA ±'  ;
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
MODIFY MEMO observa NOEDIT WINDOW  ;
       observa
IF  .NOT. WVISIBLE('Observa')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado. No graba la Observaci¢n '
ENDIF
ON KEY LABEL F9 DO OBSERVA
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
SEEK itealm.periodo +  ;
     itealm.numpec +  ;
     itealm.codfte
ok1 = pecosa.destino
SELECT (as)
RETURN (ok1)
*
PROCEDURE itepec
PRIVATE az
SELECT itealm
SEEK m.periodo + m.numoc +  ;
     ALLTRIM(m.codfte)
az = ALIAS()
SELECT pecosa
SEEK itealm.periodo +  ;
     itealm.numpec +  ;
     itealm.codfte
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
*----------------
PARAMETER xcod, _tipo, _x, _y
PRIVATE medita, mmsg, malias, v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) .AND. _tipo
_campo = VARREAD()
ord = ORDER()
malias = ALIAS()
SELECT iteart
GOTO TOP
_oldwnd = WOUTPUT()
v_fun = .F.
IF  .NOT. medita
     SEEK xcod
     v_fun = IIF(FOUND(), descri, '')
ELSE
     IF EMPTY(xcod)
          SET ORDER TO IteArt2
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
     SELECT itealm
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
ON KEY LABEL F5 DO Agreg_Item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
RETURN v_fun
*
FUNCTION visobs
ON KEY LABEL F9
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
MODIFY MEMO observa WINDOW  ;
       observa
IF  .NOT. WVISIBLE('Observa')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
ON KEY LABEL F9 DO VisObs
RETURN .T.
*

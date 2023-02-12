SAVE SCREEN TO pta_comp
HIDE POPUP pop_01, menu
PRIVATE vmens01, vmens02, vmens09
vmens01 = 'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
vmens02 = 'Documentos'
vmens04 = 'Dicho Trancferencias no fue encontrado'
vmens08 = 'No hay registros para procesar'
vmens09 = '  Detalle  '
xpos = INT((80 -  ;
       (LEN(ALLTRIM(cia)) + 2)) /  ;
       2)
vmens01 = SUBSTR(vmens01, 1, xpos +  ;
          1) + ' ' +  ;
          'Transferencias' + ' ' +  ;
          SUBSTR(vmens01, xpos +  ;
          LEN(ALLTRIM(cia)) + 3,  ;
          80)
USE IN 1 TraPar ALIAS trapar  ;
    ORDER TraPar1
USE IN 2 IteTra ALIAS itetra  ;
    ORDER IteTra1
USE IN 4 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 5 ItePar ALIAS itepar  ;
    ORDER ItePar1
USE IN 6 MaePre ALIAS maepre  ;
    ORDER MaePre1
USE IN 7 MaePar ALIAS presu ORDER  ;
    MaePar1
USE IN 14 CatAsi ALIAS catasi  ;
    ORDER CatAsi4
PUBLIC tiping, vmonacu
ON KEY LABEL F7 DO vis_det
ON KEY LABEL F9 DO PENDIENTE
STORE .T. TO agrega
STORE 0 TO vmontrai, vmontras,  ;
      m.montra
m.fecha = DATE()
SELECT parma
SEEK 'MESANO' + 'ACTUAL'
_actmes = LEFT(descri, 6)
actmes = CTOD('01/' +  ;
         SUBSTR(_actmes, 5, 2) +  ;
         '/' + SUBSTR(_actmes, 3,  ;
         2))
SELECT trapar
GOTO TOP
DO inicia
DO pantalla
DO vista
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     ACTIVATE MENU mmenu
ENDDO
DO fin_opcion
RESTORE SCREEN FROM pta_comp
SHOW POPUP menu
RETURN
*
PROCEDURE inicia
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa           Lista   Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_c0 FROM 00, 00  ;
       TO 12, 79 TITLE vmens01  ;
       FOOTER  ;
       '° ®F7¯Detalle:Item       ®F9¯ [Aprobado/Solicitado] °'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_c2 FROM 13, 00  ;
       TO 23, 79 TITLE vmens09  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_c3 FROM 10, 00  ;
       TO 23, 38 TITLE vmens09  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_c4 FROM 00, 00  ;
       TO 23, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 03, 40  ;
       TO 15, 79 TITLE  ;
       '° ®F10¯ Sale °' DOUBLE
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
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Lista ' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO corri
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD lista OF mmenu DO impri
ON SELECTION PAD termi OF mmenu DO termi
ACTIVATE SCREEN
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_c0
CLEAR
@ 00, 02 SAY  ;
  '          Periodo :'
@ 01, 02 SAY  ;
  '   Tipo Documento :'
@ 02, 02 SAY  ;
  '     N§ Documento :'
@ 03, 02 SAY  ;
  '            Fecha :'
@ 02, 40 SAY  ;
  '   Unidad Gestora :'
@ 03, 40 SAY  ;
  ' Unidad Ejecutora :'
@ 04, 02 SAY  ;
  '    Modalidad Doc :'
@ 07, 02 SAY  ;
  '      Descripci¢n :'
@ 08, 02 SAY  ;
  '         Asignado :'
@ 09, 02 SAY  ;
  '    Monto Ingreso :'
@ 10, 02 SAY  ;
  '     Monto Salida :'
RETURN
*
PROCEDURE vista
ACTIVATE WINDOW wind_c0
SELECT trapar
SCATTER MEMVAR
@ 0, 60 SAY IIF(m.estado = '00',  ;
  'Pendiente', 'Aprobado ')
@ 0, 22 SAY m.periodo
@ 1, 22 SAY val_para(m.tipdoc, ;
  'TIPDOC','V',22,40)
@ 2, 22 SAY m.numdoc
@ 3, 22 SAY m.fecha
@ 2, 60 SAY m.uniges
@ 3, 60 SAY m.unieje
@ 4, 22 SAY val_para(m.moddoc, ;
  'MODDOC','V',22,40)
@ 7, 22 SAY SUBSTR(m.descri, 1,  ;
  60)
@ 8, 22 SAY m.totasig PICTURE  ;
  '999,999,999,999.99'
@ 9, 22 SAY m.montrai PICTURE  ;
  '999,999,999,999.99'
@ 10, 22 SAY m.montras PICTURE  ;
  '999,999,999,999.99'
DO vista_hijo
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SET EXACT OFF
SELECT itetra
SET ORDER TO ITETRA1
SEEK m.periodo + m.codope +  ;
     ALLTRIM(m.tipdoc) +  ;
     ALLTRIM(m.numdoc)
BROWSE NOOPTIMIZE FIELDS tipope  ;
       :H = 'T' :P = '!', codcad  ;
       :H = 'Cad. Fun.', codfte  ;
       :H = 'Cod. Fte.', codpart  ;
       :H = 'Partida', valpart :H =  ;
       'Presup.Inicial' :P =  ;
       '999,999,999,999.99',  ;
       montra :H =  ;
       'Transferencia' :P =  ;
       '999,999,999,999.99'  ;
       NOMENU NOAPPEND NODELETE  ;
       NOCLEAR WINDOW wind_c2 KEY  ;
       m.periodo + m.codope +  ;
       ALLTRIM(m.tipdoc) +  ;
       ALLTRIM(m.numdoc) TIMEOUT  ;
       0.0001  NOREFRESH
SELECT trapar
HIDE WINDOW wind_c2
RETURN
*
PROCEDURE vis_det
ON KEY LABEL f7
SELECT itetra
SET ORDER TO ITETRA1
SEEK m.periodo + m.codope +  ;
     ALLTRIM(m.tipdoc) +  ;
     ALLTRIM(m.numdoc)
BROWSE NOOPTIMIZE FIELDS tipope  ;
       :H = 'T' :P = '!', codcad  ;
       :H = 'Cad. Fun.', codfte  ;
       :H = 'Cod. Fte.', codpart  ;
       :H = 'Partida', valpart :H =  ;
       'Presup.Inicial' :P =  ;
       '999,999,999,999.99',  ;
       montra :H =  ;
       'Transferencia' :P =  ;
       '999,999,999,999.99'  ;
       NOEDIT NOCLEAR WINDOW  ;
       wind_c2 KEY m.periodo +  ;
       m.codope +  ;
       ALLTRIM(m.tipdoc) +  ;
       ALLTRIM(m.numdoc)  ;
       NOREFRESH
ON KEY LABEL F7 DO vis_det
SELECT trapar
RETURN
*
PROCEDURE revis
SELECT trapar
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
ACTIVATE SCREEN
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS tipdoc :H =  ;
       'Tipo doc.', numdoc :H =  ;
       'Documento', fecha :H =  ;
       'CompAdi', descri :H =  ;
       'Descrip.', montrai :H =  ;
       'Ingreso Trans.' :P =  ;
       '999,999,999,999.99',  ;
       montras :H =  ;
       'Salida Trans. ' :P =  ;
       '999,999,999,999.99'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW wind_c4
ON KEY LABEL F10
DEACTIVATE WINDOW wind_c4
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vper = RIGHT(DTOC(DATE()), 2)
as = ORDER()
vnum = '00000'
vdoc = '   '
vtip = ' '
DEFINE WINDOW lis FROM 08, 12 TO  ;
       15, 68 FLOAT TITLE  ;
       ' °° B£squeda °° ' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 1, 2 SAY ' Periodo : ' GET vper  ;
  PICTURE '!!'
@ 2, 2 SAY 'Tip.Doc. : ' GET vdoc  ;
  PICTURE '!!!' VALID  ;
  val_para(vdoc,'TIPDOC','C',14, ;
  30)
@ 3, 2 SAY 'Num.Doc. : ' GET vnum  ;
  PICTURE '!!!!!'
@ 3, 19 SAY '-'
@ 3, 20 GET vper PICTURE '!!'
READ
DEACTIVATE WINDOW lis
IF EMPTY(vnum) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     vkey = vper + '001' +  ;
            ALLTRIM(vdoc) +  ;
            ALLTRIM(vnum) + '-' +  ;
            ALLTRIM(vper)
     SEEK ALLTRIM(vkey)
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Transferencia no encontrado'
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
RETURN
*
PROCEDURE anter
SELECT trapar
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  .NOT. BOF()
     SKIP -1
ENDIF
IF BOF()
     GOTO TOP
     DO standby WITH  ;
        'No existe Documento anterior.'
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE proxi
SELECT trapar
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  .NOT. EOF()
     SKIP
ENDIF
IF EOF()
     DO standby WITH  ;
        'No existe Documento siguiente.'
     GOTO BOTTOM
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE corri
tiping = 'C'
IF EOF()
     DO standby WITH vmens08
     UNLOCK ALL
     RETURN
ENDIF
IF m.fecha < m.actmes
ENDIF
IF  .NOT. f_lock(1)
     RETURN
ENDIF
SELECT trapar
SCATTER MEMVAR
ACTIVATE WINDOW wind_c0
m.codope = '003'
@ 00, 22 GET m.periodo
@ 01, 22 SAY val_para(m.tipdoc, ;
  'TIPDOC',' ',22,40)
@ 02, 22 SAY m.numdoc
@ 03, 22 GET m.fecha
@ 02, 60 GET m.uniges PICTURE  ;
  '!!' VALID val_para(m.uniges, ;
  'UNIGES',' ',60,15,3)
@ 03, 60 GET m.unieje PICTURE  ;
  '!!!' VALID val_para1(m.unieje, ;
  'UNIEJE' + ALLTRIM(m.uniges), ;
  ' ',60,12)
@ 04, 22 GET m.moddoc PICTURE  ;
  '!!!' DISABLE VALID  ;
  val_para(m.moddoc,'MODDOC',' ', ;
  22,20)
@ 07, 22 GET m.descri PICTURE  ;
  '@S56'
@ 08, 22 SAY m.totasig PICTURE  ;
  '999,999,999,999.99'
@ 09, 22 SAY m.montrai PICTURE  ;
  '999,999,999,999.99'
@ 10, 22 SAY m.montras PICTURE  ;
  '999,999,999,999.99'
READ VALID val_read()
IF LASTKEY() <> 27
     DO tra_hijo
     = valida()
     SELECT trapar
     GATHER MEMVAR
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT trapar
DO vista
RETURN
*
PROCEDURE ingre
tiping = 'I'
ACTIVATE WINDOW wind_c0
SELECT trapar
SCATTER BLANK MEMVAR
m.codope = '003'
@ 00, 22 GET m.periodo
@ 01, 22 GET m.tipdoc PICTURE  ;
  '!!!' VALID val_para(m.tipdoc, ;
  'TIPDOC',' ',22,20)
@ 02, 22 GET m.numdoc PICTURE  ;
  '!!!!!-!!'
@ 03, 22 GET m.fecha
@ 02, 60 GET m.uniges PICTURE  ;
  '!!' VALID val_para(m.uniges, ;
  'UNIGES',' ',60,15,3)
@ 03, 60 GET m.unieje PICTURE  ;
  '!!!' VALID val_para1(m.unieje, ;
  'UNIEJE' + ALLTRIM(m.uniges), ;
  ' ',60,12) .AND. v_num()
@ 07, 22 GET m.descri PICTURE  ;
  '@S56'
@ 08, 22 SAY m.totasig PICTURE  ;
  '999,999,999,999.99'
@ 09, 22 SAY m.montrai PICTURE  ;
  '999,999,999,999.99'
@ 10, 22 SAY m.montras PICTURE  ;
  '999,999,999,999.99'
READ VALID val_read()
IF LASTKEY() <> 27
     vanula = .F.
     SELECT itetra
     DO WHILE .T.
          ok = tra_hijo()
          IF LASTKEY() = 27 .AND.  ;
             ok
               IF yesno( ;
                  '¨ Cancela el Ingreso ?' ;
                  )
                    vanula = .T.
                    EXIT
               ENDIF
          ELSE
               IF yesno( ;
                  '¨ Est n correctos los datos ?' ;
                  )
                    EXIT
               ENDIF
          ENDIF
     ENDDO
     IF  .NOT. vanula
          = valida()
          SELECT trapar
          m.estado = '00'
          IF f_appd()
               GATHER MEMVAR
          ENDIF
     ELSE
          DO anula
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
STORE 0 TO vmontrai, vmontras
UNLOCK ALL
SELECT trapar
DO vista
RETURN
*
FUNCTION tra_hijo
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO agreg_item
ON KEY LABEL F8 DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itetra
SEEK ALLTRIM(m.periodo) +  ;
     m.codope + ALLTRIM(m.tipdoc) +  ;
     ALLTRIM(m.numdoc)
IF  .NOT. FOUND()
     DO agreg_item
ENDIF
STORE 0 TO m.montra, m.montrai,  ;
      m.montras, vmontos,  ;
      vmontoi
BROWSE NOOPTIMIZE FIELDS tipope  ;
       :H = 'T', codcad :H =  ;
       'Cad. Fun.' :V =  ;
       val_codcad(codcad, ;
       m.periodo +  ;
       ALLTRIM(m.uniges) +  ;
       ALLTRIM(m.unieje), ;
       'codcad') :F, codfte :H =  ;
       'Fte' :V =  ;
       val_parabr(codfte,'CODFTE', ;
       'codfte') :F, codpart :H =  ;
       'Part' :V =  ;
       val_part1(codpart,periodo +  ;
       ALLTRIM(m.uniges) +  ;
       ALLTRIM(m.unieje) + codcad +  ;
       codfte,'codpart','C')  ;
       .AND. posi() :F, valpart  ;
       :H = 'Asignado' :P =  ;
       '999,999,999,999.99' :R,  ;
       montra :H = 'Transfiere'  ;
       :P = '999,999,999,999.99'  ;
       NOMENU NOAPPEND NODELETE  ;
       WINDOW wind_c2 KEY  ;
       ALLTRIM(m.periodo) +  ;
       m.codope +  ;
       ALLTRIM(m.tipdoc) +  ;
       ALLTRIM(m.numdoc)  ;
       NOREFRESH
vmonacu = 0
vmoning = 0
vmonsal = 0
SELECT itetra
SEEK ALLTRIM(m.periodo) +  ;
     m.codope + ALLTRIM(m.tipdoc) +  ;
     ALLTRIM(m.numdoc)
SCAN WHILE periodo + codope +  ;
     ALLTRIM(tipdoc) +  ;
     ALLTRIM(numdoc) =  ;
     ALLTRIM(m.periodo) +  ;
     m.codope + ALLTRIM(m.tipdoc) +  ;
     ALLTRIM(m.numdoc)
     IF itetra.montra = 0
          IF RLOCK()
               DELETE NEXT 1
          ENDIF
     ELSE
          REPLACE fecha WITH  ;
                  m.fecha
          vmonacu = vmonacu +  ;
                    montra *  ;
                    IIF(tipope =  ;
                    '-', -1, 1)
          vmonsal = vmonsal +  ;
                    IIF(tipope =  ;
                    '-', montra,  ;
                    0)
          vmoning = vmoning +  ;
                    IIF(tipope =  ;
                    '+', montra,  ;
                    0)
     ENDIF
     UNLOCK
ENDSCAN
m.totasig = vmonacu
m.montrai = vmoning
m.montras = vmonsal
ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5
UNLOCK ALL
ACTIVATE SCREEN
vtempo = '                                                                           '
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT trapar
RETURN .T.
*
PROCEDURE agreg_item
SELECT itetra
IF f_appd()
     REPLACE periodo WITH  ;
             m.periodo, codope  ;
             WITH m.codope,  ;
             tipdoc WITH m.tipdoc,  ;
             numdoc WITH m.numdoc,  ;
             uniges WITH  ;
             ALLTRIM(m.uniges),  ;
             unieje WITH  ;
             ALLTRIM(m.unieje),  ;
             fecha WITH m.fecha
     IF ALLTRIM(m.moddoc) = 'HTA'
          REPLACE tipope WITH '-'
     ELSE
          REPLACE tipope WITH '+'
     ENDIF
ENDIF
UNLOCK
RETURN
*
PROCEDURE elimi_item
SELECT itetra
vord = ORDER()
IF RLOCK()
     SELECT itepar
     SEEK itetra.periodo +  ;
          ALLTRIM(m.uniges) +  ;
          ALLTRIM(m.unieje) +  ;
          itetra.codcad +  ;
          itetra.codfte +  ;
          itetra.codpart
     IF FOUND() .AND. RLOCK()
          IF itetra.tipope = '-'
               REPLACE itepar.tra001  ;
                       WITH  ;
                       itepar.tra001 +  ;
                       itetra.montra
               REPLACE trapar.montras  ;
                       WITH  ;
                       trapar.montras -  ;
                       itetra.montra
               SELECT itetra
               DELETE NEXT 1
          ENDIF
          IF itetra.tipope = '+'
               REPLACE itepar.tra001  ;
                       WITH  ;
                       itepar.tra001 -  ;
                       itetra.montra
               REPLACE trapar.montrai  ;
                       WITH  ;
                       trapar.montrai -  ;
                       itetra.montra
               SELECT itetra
               DELETE NEXT 1
          ENDIF
     ENDIF
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
SELECT itetra
SET ORDER TO vOrd
RETURN
*
PROCEDURE anula
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF yesno('¨ANULA Documento?')
     vali = ALIAS()
     vord = ORDER()
     SELECT trapar
     vperiodo = ALLTRIM(trapar.periodo)
     vtipdoc = ALLTRIM(trapar.tipdoc)
     vcodope = ALLTRIM(trapar.codope)
     vnumdoc = ALLTRIM(trapar.numdoc)
     vgeseje = trapar.uniges +  ;
               trapar.unieje
     SELECT itetra
     GOTO TOP
     SCAN WHILE vperiodo =  ;
          periodo .AND. vcodope =  ;
          codope .AND. vtipdoc =  ;
          ALLTRIM(tipdoc) .AND.  ;
          vnumdoc =  ;
          ALLTRIM(numdoc) .AND.  ;
          vgeseje = uniges +  ;
          unieje
          SELECT itepar
          SEEK itetra.periodo +  ;
               ALLTRIM(m.uniges) +  ;
               ALLTRIM(m.unieje) +  ;
               itetra.codcad +  ;
               itetra.codfte +  ;
               itetra.codpart
          IF FOUND() .AND.  ;
             (RLOCK() .OR.  ;
             f_lock(1))
               IF itetra.tipope =  ;
                  '-'
                    REPLACE itepar.valpart  ;
                            WITH  ;
                            itepar.tra001 +  ;
                            itetra.montra
               ELSE
                    REPLACE itepar.valpart  ;
                            WITH  ;
                            itepar.tra001 -  ;
                            itetra.montra
               ENDIF
          ENDIF
          SELECT itetra
     ENDSCAN
     SELECT (vali)
     SET ORDER TO vOrd
     IF f_lock(1)
          DELETE
          SELECT itetra
          SEEK vperiodo + vtipdoc +  ;
               vcodope + vnumdoc +  ;
               vgeseje
          IF FOUND()
               SCAN WHILE  ;
                    vperiodo =  ;
                    periodo .AND.  ;
                    vcodope =  ;
                    codope .AND.  ;
                    vtipdoc =  ;
                    ALLTRIM(tipdoc)  ;
                    .AND. vnumdoc =  ;
                    ALLTRIM(numdoc)  ;
                    .AND. vgeseje =  ;
                    uniges +  ;
                    unieje
                    IF f_lock(1)
                         DELETE NEXT  ;
                                1
                    ENDIF
               ENDSCAN
          ENDIF
          SELECT trapar
          IF  .NOT. BOF()
               SKIP -1
          ELSE
               IF  .NOT. EOF()
                    SKIP
               ENDIF
          ENDIF
     ENDIF
ENDIF
UNLOCK ALL
DO vista
RETURN
*
PROCEDURE valida
PARAMETER vali, vtp
vali = ALIAS()
SELECT itetra
SEEK m.periodo + m.codope +  ;
     ALLTRIM(m.tipdoc) +  ;
     ALLTRIM(m.numdoc) +  ;
     ALLTRIM(m.uniges) +  ;
     ALLTRIM(m.unieje)
SCAN WHILE periodo = m.periodo  ;
     .AND. codope = m.codope  ;
     .AND. ALLTRIM(tipdoc) =  ;
     ALLTRIM(m.tipdoc) .AND.  ;
     ALLTRIM(numdoc) =  ;
     ALLTRIM(m.numdoc)
     SELECT itepar
     SEEK itetra.periodo +  ;
          ALLTRIM(m.uniges) +  ;
          ALLTRIM(m.unieje) +  ;
          itetra.codcad +  ;
          itetra.codfte +  ;
          itetra.codpart
     IF FOUND()
          vantcan = IIF(tiping =  ;
                    'C',  ;
                    itepar.tra001,  ;
                    0)
          IF RLOCK()
               REPLACE itepar.tra001  ;
                       WITH  ;
                       roltra(),  ;
                       itepar.numtra  ;
                       WITH  ;
                       itetra.numdoc
          ENDIF
     ENDIF
     UNLOCK ALL
     SELECT itetra
ENDSCAN
SELECT (vali)
RETURN
*
FUNCTION roltra
PRIVATE alis, vkeyf, vkeyi, vtp,  ;
        vtotal
alis = ALIAS()
SELECT itetra
vtp = RECNO()
vkeyi = periodo + codcad + codfte +  ;
        codpart + codope + numdoc +  ;
        uniges + unieje
vkeyf = periodo + codcad + codfte +  ;
        codpart + codope + numdoc +  ;
        uniges + unieje
GOTO TOP
vtotal = 0
SCAN FOR periodo + codcad +  ;
     codfte + codpart + codope +  ;
     numdoc + uniges + unieje =  ;
     vkeyi
     vtotal = vtotal + montra *  ;
              IIF(tipope = '-', - ;
              1, 1)
ENDSCAN
GOTO vtp
SELECT (alis)
RETURN vtotal
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
DO logos WITH rotulo1, rotulo2
RELEASE WINDOW wind_c0
RELEASE WINDOW wind_c2
RELEASE WINDOW wind_3
RELEASE MENU mmenu
CLOSE DATABASES
RETURN
*
PROCEDURE impri
SELECT trapar
SCATTER MEMVAR
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF LASTKEY() = 27
     RETURN
ENDIF
vind = SYS(3) + '.idx'
rec = RECNO()
vkey = m.periodo +  ;
       ALLTRIM(m.tipdoc) +  ;
       ALLTRIM(m.numdoc)
SELECT itetra
SET RELATION TO m.periodo + ALLTRIM(m.tipdoc);
+ ALLTRIM(m.numdoc) INTO trapar
SET FILTER TO periodo + ALLTRIM(tipdoc);
+ ALLTRIM(numdoc) = vkey
DO reporte WITH 2, 'Transf_I',  ;
   ' Reporte de Transferencias '
DO reporte WITH 2, 'Transf_F',  ;
   ' Reporte de Transferencias '
SET SKIP TO
SET FILTER TO
SET RELATION TO
SELECT trapar
GOTO rec
DO vista
RETURN
*
PROCEDURE posi
vord = ORDER()
SELECT itepar
SEEK itetra.periodo +  ;
     ALLTRIM(m.uniges) +  ;
     ALLTRIM(m.unieje) +  ;
     itetra.codcad +  ;
     itetra.codfte +  ;
     itetra.codpart
IF FOUND() .AND. (RLOCK() .OR.  ;
   f_lock(1))
     IF EMPTY(itetra.valpart)
          REPLACE itetra.valpart  ;
                  WITH  ;
                  itepar.valpart
     ENDIF
ELSE
     DO standby WITH  ;
        'No se tiene registrado esta partida'
ENDIF
SELECT itetra
RETURN
*
PROCEDURE v_num
vord = ORDER()
SEEK m.periodo + m.codope +  ;
     ALLTRIM(m.tipdoc) +  ;
     ALLTRIM(m.numdoc) +  ;
     ALLTRIM(m.uniges) +  ;
     ALLTRIM(m.unieje)
IF FOUND()
     DO standby WITH  ;
        'Documento Existe'
ENDIF
SET ORDER TO vOrd
RETURN
*
PROCEDURE pendiente
DO CASE
     CASE estado = '00'
          REPLACE estado WITH  ;
                  '10'
     CASE estado = '10'
          REPLACE estado WITH  ;
                  '00'
ENDCASE
DO vista
RETURN
*

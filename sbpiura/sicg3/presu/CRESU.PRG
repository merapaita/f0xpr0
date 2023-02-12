PUBLIC xtipfun
PRIVATE vmens01, vmens02, vmens09
vmens01 = ' Cr‚dito Suplementarios '
vmens03 = '° ®F7¯Detalle:Item       ®F9¯ [Aprobado/Solicitado] °'
vmens02 = ' Documentos '
vmens08 = 'No hay registros para procesar'
vmens09 = ' Detalle: Cr‚dito Suplementario '
USE IN 1 CreSup ALIAS cresup  ;
    ORDER CreSup1
USE IN 2 IteCre ALIAS itecre  ;
    ORDER IteCre1
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
PUBLIC vfun
ON KEY LABEL F7 DO vis_det
ON KEY LABEL F9 DO PENDIENTE
STORE .T. TO agrega, vfun
m.fecha = DATE()
SELECT parma
SEEK 'MESANO' + 'ACTUAL'
_actmes = LEFT(descri, 6)
actmes = CTOD('01/' +  ;
         SUBSTR(_actmes, 5, 2) +  ;
         '/' + SUBSTR(_actmes, 3,  ;
         2))
SELECT cresup
GOTO BOTTOM
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa           Lista   Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_c0 FROM 00, 00  ;
       TO 12, 79 TITLE vmens01  ;
       FOOTER vmens03 DOUBLE  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_c2 FROM 13, 00  ;
       TO 23, 79 TITLE vmens09  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_c2t FROM 00,  ;
       00 TO 23, 79 TITLE vmens09  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_c3 FROM 09, 00  ;
       TO 23, 38 TITLE vmens09  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_c4 FROM 00, 00  ;
       TO 24, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 03, 40  ;
       TO 15, 79 TITLE  ;
       '° ®F10¯ Sale °' PANEL
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
@ 01, 02 SAY  '          Periodo :'
@ 02, 02 SAY  '   Tipo Documento :'
@ 03, 02 SAY  '     N§ Documento :'
@ 04, 02 SAY  '            Fecha :'
@ 05, 02 SAY  '      Descripci¢n :'
@ 06, 02 SAY  '   Unidad Gestora :'
@ 07, 02 SAY  ' Unidad Ejecutora :'
@ 08, 02 SAY  '         Asignado :'
@ 09, 02 SAY  '    Total Ingreso :'
RETURN
*
PROCEDURE vista
SELECT cresup
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_c0
SCATTER MEMVAR
@ 00, 60 SAY IIF(m.estado = '00',  'Pendiente', 'Aprobado ')
@ 01, 22 SAY m.periodo
@ 02, 22 SAY val_para(m.tipdoc, 'TIPDOC','D',22,40)
@ 03, 22 SAY m.numdoc
@ 04, 22 SAY m.fecha
@ 05, 22 SAY SUBSTR(m.descri, 1,  56)
@ 06, 22 SAY val_para(m.uniges, 'UNIGES','V',22,40)
@ 07, 22 SAY val_para1(m.unieje, 'UNIEJE' + m.uniges,'V',22,40)
@ 08, 22 SAY m.totasig PICTURE  '999,999,999.99'
@ 09, 22 SAY m.moncrei PICTURE  '999,999,999.99'
DO vista_hijo
RETURN
*
PROCEDURE vista_hijo
SELECT itecre
SEEK m.periodo + m.codope +  ALLTRIM(m.tipdoc) +  ALLTRIM(m.numdoc)
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            codcad :H = 'Cad. Fun.', ;
            codfte :H = 'Fte.',;
            codpart :H = 'Partida',;
            valpart :H = 'Marco Asignado' :P = '999,999,999.99',;
            monasig :H = 'Cr‚dito Asignado' :P = '999,999,999.99';
            NOEDIT NOCLEAR WINDOW wind_c2 KEY m.periodo + m.codope + ALLTRIM(m.tipdoc) + ALLTRIM(m.numdoc)  ;
            TIMEOUT 0.005 NOREFRESH
     SELECT cresup

*            xx = valasi1('ItePar', itepar.codpart,'6', 'Descri','R') :H='Descripci¢n':20,;

ENDIF
RETURN
*
PROCEDURE vis_det
ON KEY LABEL F7
SCATTER MEMVAR
SELECT itecre
BROWSE NOOPTIMIZE FIELDS codcad  ;
       :H = 'Cad. Fun.', codfte  ;
       :H = 'Fte.', codpart :H =  ;
       'Partida', xx =  ;
       valasi1('ItePar', ;
       itepar.codpart,'6', ;
       'Descri','R') :H =  ;
       'Descripci¢n' : 20,  ;
       valpart :H =  ;
       'Marco Asignado' :P =  ;
       '999,999,999.99', monasig  ;
       :H = 'Cr‚dito Asignado' :P =  ;
       '999,999,999.99' NOEDIT  ;
       NOCLEAR WINDOW wind_c2 KEY  ;
       m.periodo + m.codope +  ;
       ALLTRIM(m.tipdoc) +  ;
       ALLTRIM(m.numdoc)  ;
       NOREFRESH
SELECT cresup
ON KEY LABEL F7 DO vis_det
RETURN
*
PROCEDURE revis
SELECT cresup
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
ACTIVATE SCREEN
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS tipdoc :H =  ;
       'TipDoc', uniges :H =  ;
       'Gestora', unieje :H =  ;
       'Ejecutora', codcad :H =  ;
       'Cad.Fun.', codfte :H =  ;
       'FTE.FTO.', numdoc :H =  ;
       'Documento', fecha :H =  ;
       'CompAdi', descri :H =  ;
       'Descrip.', totasig :H =  ;
       'Asignado' :P =  ;
       '999,999,999,999.99'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW wind_c4
ON KEY LABEL F10
DEACTIVATE WINDOW wind_c4
DO vista
RETURN
*
PROCEDURE busca
PRIVATE vkey, vtemp, as
SELECT cresup
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vper = RIGHT(DTOC(DATE()), 2)
as = ORDER()
vnum_mes = 0
vnum = '00000'
vdoc = '   '
vcodcad = '    '
vcodfte = '  '
DEFINE WINDOW lis FROM 08, 12 TO  ;
       15, 68 FLOAT TITLE  ;
       ' °° B£squeda °° ' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 1, 2 SAY ' Periodo : ' GET vper  ;
  PICTURE '!!'
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
     vkey = vper + '002' +  ;
            ALLTRIM(vnum) + '-' +  ;
            ALLTRIM(vper)
     SEEK ALLTRIM(vkey)
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Cr‚dito no encontrado'
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
RETURN
*
PROCEDURE anter
SELECT cresup
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
SELECT cresup
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
SELECT cresup
SCATTER MEMVAR
IF EOF()
     DO standby WITH vmens08
     UNLOCK ALL
     RETURN
ENDIF
IF m.fecha < m.actmes
     DO standby WITH  ;
        'Documento no es del mes actual'
     RETURN
ENDIF
IF  .NOT. f_lock(1)
     RETURN
ENDIF
ACTIVATE WINDOW wind_c0
m.codope = '002'
@ 01, 22 GET m.periodo
@ 02, 22 SAY val_para(m.tipdoc, ;
  'TIPDOC','V',22,40)
@ 03, 22 SAY m.numdoc
@ 04, 22 GET m.fecha VALID  .NOT.  ;
  m.fecha < m.actmes
@ 05, 22 GET m.descri PICTURE  ;
  '@S50'
@ 06, 22 SAY val_para(m.uniges, ;
  'UNIGES','V',22,40)
@ 07, 22 SAY val_para1(m.unieje, ;
  'UNIEJE' + ALLTRIM(m.uniges), ;
  'V',22,40)
@ 08, 22 GET m.totasig PICTURE  ;
  '999,999,999.99' VALID  ;
  m.totasig > 0
@ 09, 22 SAY m.moncrei PICTURE  ;
  '999,999,999.99'
READ VALID val_read()
IF LASTKEY() <> 27
     xfecha = m.fecha
     m.uniges = ALLTRIM(m.uniges)
     m.unieje = ALLTRIM(m.unieje)
     ok = tra_hijo()
     m.fecha = xfecha
     SELECT cresup
     GATHER MEMVAR
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT cresup
DO vista
RETURN
*
FUNCTION vper
vper = m.periodo
RETURN .T.
*
PROCEDURE ingre
PRIVATE vnum, vper
ACTIVATE WINDOW wind_c0
SELECT cresup
SCATTER BLANK MEMVAR
DO pantalla
vnum = '00000'
vper = '  '
m.codope = '002'
@ 01, 22 GET m.periodo PICTURE  ;
  '!!' VALID  .NOT.  ;
  EMPTY(m.periodo) .AND. vper()
@ 02, 22 GET m.tipdoc VALID  ;
  val_para(m.tipdoc,'TIPDOC',' ', ;
  22,40,3)
@ 03, 22 GET vnum PICTURE '!!!!!'  ;
  VALID VAL(vnum) > 0
@ 03, 28 GET vper PICTURE '!!'
@ 04, 22 GET m.fecha VALID  .NOT.  ;
  m.fecha < m.actmes
@ 05, 22 GET m.descri PICTURE  ;
  '@S50'
@ 06, 22 GET m.uniges PICTURE  ;
  '!!' VALID val_para(m.uniges, ;
  'UNIGES',' ',22,40)
@ 07, 22 GET m.unieje PICTURE  ;
  '!!!' VALID val_para1(m.unieje, ;
  'UNIEJE' + ALLTRIM(m.uniges), ;
  ' ',22,40) .AND. v_num()
@ 08, 22 GET m.totasig PICTURE  ;
  '999,999,999.99' VALID  ;
  m.totasig > 0
@ 09, 22 SAY m.moncrei PICTURE  ;
  '999,999,999.99'
READ VALID val_read()
IF LASTKEY() <> 27
     m.numdoc = vnum + '-' + vper
     m.uniges = ALLTRIM(m.uniges)
     m.unieje = ALLTRIM(m.unieje)
     ok = tra_hijo()
     SELECT cresup
     m.estado = '00'
     IF f_appd()
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT cresup
DO vista
RETURN
*
FUNCTION tra_hijo
vfun = .T.
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO agreg_item
ON KEY LABEL F8 DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
STORE 0 TO m.monasig, antcan
SELECT itecre
GOTO TOP
SEEK m.periodo + m.codope +  ;
     ALLTRIM(m.tipdoc) +  ;
     ALLTRIM(m.numdoc)
IF  .NOT. FOUND()
     DO agreg_item
ELSE
     SCAN WHILE periodo =  ;
          m.periodo .AND. codope =  ;
          m.codope .AND.  ;
          ALLTRIM(tipdoc) =  ;
          ALLTRIM(m.tipdoc) .AND.  ;
          ALLTRIM(numdoc) =  ;
          ALLTRIM(m.numdoc)
          IF RLOCK()
               REPLACE antcan  ;
                       WITH  ;
                       monasig
          ENDIF
     ENDSCAN
     GOTO TOP
     SEEK m.periodo + m.codope +  ;
          ALLTRIM(m.tipdoc) +  ;
          ALLTRIM(m.numdoc)
ENDIF
SET CONFIRM OFF
BROWSE NOOPTIMIZE FIELDS codcad  ;
       :H = 'Cad. Fun.' :V =  ;
       val_codcad(codcad, ;
       m.periodo + m.uniges +  ;
       m.unieje,'codcad') :F,  ;
       codfte :H = 'Fte' :V =  ;
       val_parabr(codfte,'CODFTE', ;
       'codfte') .AND.  ;
       ver_pres(periodo +  ;
       m.uniges + m.unieje +  ;
       codcad + codfte) :F,  ;
       codpart :H = 'Part' :V =  ;
       val_part1(codpart,periodo +  ;
       m.uniges + m.unieje +  ;
       codcad + codfte,'codpart', ;
       'C') .AND. posi() :F,  ;
       valpart :H = 'Asignado' :P =  ;
       '999,999,999.99' :R,  ;
       monasig :H =  ;
       'Cr‚dito Asignado' :P =  ;
       '999,999,999.99' :V =  ;
       mayor() :F NOMENU NOAPPEND  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_c2 KEY m.periodo +  ;
       m.codope +  ;
       ALLTRIM(m.tipdoc) +  ;
       ALLTRIM(m.numdoc)  ;
       NOREFRESH
SET CONFIRM ON
vmoncre = 0
SELECT itecre
SEEK m.periodo + m.codope +  ;
     ALLTRIM(m.tipdoc) +  ;
     ALLTRIM(m.numdoc)
SCAN WHILE periodo = m.periodo  ;
     .AND. codope = m.codope  ;
     .AND. ALLTRIM(tipdoc) =  ;
     ALLTRIM(m.tipdoc) .AND.  ;
     ALLTRIM(numdoc) =  ;
     ALLTRIM(m.numdoc)
     IF RLOCK()
          IF monasig <> 0
               REPLACE fecha WITH  ;
                       m.fecha
               SELECT itepar
               SEEK itecre.periodo +  ;
                    m.uniges +  ;
                    m.unieje +  ;
                    itecre.codcad +  ;
                    itecre.codfte +  ;
                    itecre.codpart
               IF FOUND()
                    REPLACE cresup  ;
                            WITH  ;
                            cresup +  ;
                            (itecre.monasig -  ;
                            itecre.antcan)
               ENDIF
               SELECT itecre
               vmoncre = vmoncre +  ;
                         monasig
          ELSE
               DELETE NEXT 1
          ENDIF
     ENDIF
ENDSCAN
GOTO TOP
m.moncrei = vmoncre
ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5
UNLOCK ALL
SELECT cresup
ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
IF LASTKEY() = 27
     vfun = .F.
ENDIF
RETURN vfun
*
FUNCTION asiact
REPLACE codact WITH vcodact
RETURN .T.
*
FUNCTION asipry
REPLACE codproy WITH vproyec
RETURN .T.
*
PROCEDURE agreg_item
SELECT itecre
IF f_appd()
     REPLACE periodo WITH  ;
             m.periodo, codope  ;
             WITH m.codope,  ;
             tipdoc WITH m.tipdoc,  ;
             numdoc WITH m.numdoc,  ;
             uniges WITH m.uniges,  ;
             unieje WITH m.unieje,  ;
             fecha WITH m.fecha
ENDIF
UNLOCK
RETURN
*
PROCEDURE elimi_item
SELECT itecre
IF RLOCK()
     = suma()
     DELETE NEXT 1
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
RETURN
*
PROCEDURE anula
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF yesno('¨ANULA Documento?')
     verdad = .T.
     vali = ALIAS()
     vord = ORDER()
     SELECT cresup
     vperiodo = cresup.periodo
     vuniges = cresup.uniges
     vunieje = cresup.unieje
     vtipdoc = ALLTRIM(cresup.tipdoc)
     vcodope = cresup.codope
     vnumdoc = ALLTRIM(cresup.numdoc)
     SELECT itecre
     GOTO TOP
     SCAN WHILE vperiodo +  ;
          vcodope + vtipdoc +  ;
          vnumdoc =  ;
          itecre.periodo +  ;
          itecre.codope +  ;
          ALLTRIM(itecre.tipdoc) +  ;
          ALLTRIM(itecre.numdoc)
          SELECT itepar
          SEEK itecre.periodo +  ;
               vuniges + vunieje +  ;
               itecre.codcad +  ;
               itecre.codfte +  ;
               itecre.codpart
          IF FOUND()
               IF f_lock(1)
                    REPLACE itepar.cresup  ;
                            WITH  ;
                            itepar.cresup -  ;
                            itecre.monasig
               ENDIF
          ENDIF
          SELECT itecre
     ENDSCAN
     SELECT (vali)
     SET ORDER TO vOrd
     SELECT cresup
     IF f_lock(1)
          DELETE
          SELECT itecre
          GOTO TOP
          SEEK vperiodo + vcodope +  ;
               ALLTRIM(vtipdoc) +  ;
               ALLTRIM(vnumdoc)
          IF FOUND()
               SCAN WHILE  ;
                    vperiodo +  ;
                    vcodope +  ;
                    vtipdoc +  ;
                    vnumdoc =  ;
                    itecre.periodo +  ;
                    itecre.codope +  ;
                    ALLTRIM(itecre.tipdoc) +  ;
                    ALLTRIM(itecre.numdoc)
                    IF f_lock(1)
                         DELETE NEXT  ;
                                1
                    ENDIF
               ENDSCAN
          ENDIF
          SELECT cresup
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
SELECT cresup
SET RELATION TO periodo + codope + ALLTRIM(tipdoc);
+ ALLTRIM(numdoc) INTO itecre
SET SKIP TO itecre
vrecno = RECNO()
SCATTER MEMVAR
IF EOF()
     DO standby WITH vmens08
     GOTO vrecno
     DO vista
     RETURN
ENDIF
STORE 0 TO vtipo
STORE SPACE(2) TO vperiodo,  ;
      vcodfte
vcodcad = SPACE(4)
vnumdoc = SPACE(5)
vuniges = SPACE(2)
vunieje = SPACE(3)
DEFINE WINDOW lis_1 FROM 05, 13  ;
       TO 18, 67 FLOAT TITLE  ;
       ' °°  Creditos Suplementarios  °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis_1
@ 1, 2 SAY '     Periodo : ' GET  ;
  vperiodo PICTURE '!!' VALID   ;
  .NOT. EMPTY(vperiodo)
@ 3, 2 SAY ' Por Docume. : ' GET  ;
  vtipo SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 5, 2 SAY '  U. Gestora : ' GET  ;
  vuniges PICTURE '!!' VALID  ;
  val_para(vuniges,'UNIGES',' ', ;
  18,30)
@ 6, 2 SAY 'U. Ejecutora : ' GET  ;
  vunieje PICTURE '!!!' VALID  ;
  val_para1(vunieje,'UNIEJE' +  ;
  vuniges,' ',18,30)
@ 8, 2 SAY '    Num.Doc. : ' GET  ;
  vnumdoc PICTURE '!!!!!' WHEN  ;
  vtipo = 1
@ 10, 2 SAY '   Fte. Fto. : ' GET  ;
  vcodfte PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vcodfte),  ;
  val_para(vcodfte,'CODFTE',' ', ;
  18,30), .T.)
READ VALID val_read()
DEACTIVATE WINDOW lis_1
IF LASTKEY() = 27
     SELECT cresup
     GOTO vrecno
     DO vista
     RETURN
ENDIF
IF EOF()
     DO standby WITH vmens08
ELSE
     rec = RECNO()
     SELECT itecre
     SET FILTER TO periodo = ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), cresup.uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), cresup.unieje;
= ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vnumdoc)), ALLTRIM(numdoc);
= vnumdoc + '-' + vperiodo,;
.T.)
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existe Registros para procesar'
     ELSE
          DO reporte WITH 2,  ;
             'CreSup_I',  ;
             ' Creditos Suplementarios '
     ENDIF
ENDIF
SET FILTER TO
SET RELATION TO
SELECT cresup
GOTO vrecno
DO vista
RETURN
*
PROCEDURE posi
ad = ALIAS()
SELECT itepar
SEEK itecre.periodo + m.uniges +  ;
     m.unieje + itecre.codcad +  ;
     itecre.codfte +  ;
     itecre.codpart
IF FOUND() .AND. (RLOCK() .OR.  ;
   f_lock(1))
     IF EMPTY(itecre.valpart)
          SELECT itecre
          REPLACE itecre.valpart  ;
                  WITH  ;
                  itepar.valpart +  ;
                  itepar.cresup +  ;
                  itepar.tra001 +  ;
                  itepar.tra003 +  ;
                  itepar.tra004 +  ;
                  itepar.tra005
     ENDIF
ELSE
     DO standby WITH  ;
        'No se tiene registrado esta partida'
ENDIF
SELECT (ad)
RETURN
*
FUNCTION mayor
IF itecre.monasig > m.totasig
     DO standby WITH  ;
        'ERROR! el monto excede lo asignado, es NEGATIVO o es CERO. Proceda a Corregir'
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION w_can
RETURN .T.
*
PROCEDURE suma
vord = ORDER()
SELECT itepar
IF itecre.monasig <> antcan
     SEEK itecre.periodo +  ;
          m.uniges + m.unieje +  ;
          itecre.codcad +  ;
          itecre.codfte +  ;
          itecre.codpart
     IF FOUND() .AND. (RLOCK()  ;
        .OR. f_lock(1))
          IF itecre.monasig >  ;
             antcan
               REPLACE itepar.cresup  ;
                       WITH  ;
                       itepar.cresup +  ;
                       (itecre.monasig -  ;
                       antcan)
               REPLACE cresup.moncrei  ;
                       WITH  ;
                       cresup.moncrei +  ;
                       (itecre.monasig -  ;
                       antcan)
               IF cresup.moncrei >  ;
                  m.totasig
                    SEEK itecre.periodo +  ;
                         m.uniges +  ;
                         m.unieje +  ;
                         itecre.codcad +  ;
                         itecre.codfte +  ;
                         itecre.codpart
                    IF FOUND()  ;
                       .AND.  ;
                       (RLOCK()  ;
                       .OR.  ;
                       f_lock(1))
                         REPLACE itepar.cresup  ;
                                 WITH  ;
                                 itepar.cresup -  ;
                                 (itecre.monasig -  ;
                                 antcan)
                         REPLACE cresup.moncrei  ;
                                 WITH  ;
                                 cresup.moncrei -  ;
                                 (itecre.monasig -  ;
                                 antcan)
                         REPLACE itecre.monasig  ;
                                 WITH  ;
                                 0
                    ENDIF
                    DO standby  ;
                       WITH  ;
                       'ERROR!!!. Suma excede el Total del Credito'
               ENDIF
          ELSE
               REPLACE itepar.cresup  ;
                       WITH  ;
                       itepar.cresup -  ;
                       (antcan -  ;
                       itecre.monasig)
               REPLACE cresup.moncrei  ;
                       WITH  ;
                       cresup.moncrei -  ;
                       (antcan -  ;
                       itecre.monasig)
               IF cresup.moncrei >  ;
                  m.totasig
                    SEEK itecre.periodo +  ;
                         m.uniges +  ;
                         m.unieje +  ;
                         itecre.codcad +  ;
                         itecre.codfte +  ;
                         itecre.codpart
                    IF FOUND()  ;
                       .AND.  ;
                       (RLOCK()  ;
                       .OR.  ;
                       f_lock(1))
                         REPLACE itepar.cresup  ;
                                 WITH  ;
                                 itepar.cresup +  ;
                                 (antcan -  ;
                                 itecre.monasig)
                         REPLACE cresup.moncrei  ;
                                 WITH  ;
                                 cresup.moncrei +  ;
                                 (antcan -  ;
                                 itecre.monasig)
                         REPLACE itecre.monasig  ;
                                 WITH  ;
                                 0
                    ENDIF
                    DO standby  ;
                       WITH  ;
                       'ERROR!!!. Suma excede el Total del Credito'
               ENDIF
          ENDIF
     ENDIF
ENDIF
SELECT itecre
SET ORDER TO vOrd
RETURN
*
FUNCTION v_num
PRIVATE vfun
vord = ORDER()
vfun = .T.
m.numdoc = PADL(ALLTRIM(vnum), 5,  ;
           '0') + '-' + vper
SEEK m.periodo +  ;
     ALLTRIM(m.tipdoc) +  ;
     ALLTRIM(m.numdoc)
IF FOUND()
     DO standby WITH  ;
        'El Documento ya Existe'
     vfun = .F.
ENDIF
@ 03, 22 GET m.numdoc DISABLE
RETURN vfun
*
PROCEDURE valida
vali = ALIAS()
SELECT itecre
m.moncrei = 0
SEEK m.periodo + m.codope +  ;
     ALLTRIM(m.tipdoc) +  ;
     ALLTRIM(m.numdoc)
SCAN WHILE periodo + codope =  ;
     m.periodo + m.codope .AND.  ;
     ALLTRIM(tipdoc) =  ;
     ALLTRIM(m.tipdoc) .AND.  ;
     ALLTRIM(numdoc) =  ;
     ALLTRIM(m.numdoc)
     m.moncrei = m.moncrei +  ;
                 itecre.monasig
     SCATTER MEMVAR
     SELECT itepar
     SEEK itecre.periodo +  ;
          itecre.codcad +  ;
          itecre.codfte +  ;
          itecre.codpart
     vnum = itecre.numdoc
     IF FOUND()
          IF RLOCK()
               REPLACE itepar.cresup  ;
                       WITH  ;
                       rolcre(),  ;
                       itepar.numcre  ;
                       WITH vnum
          ENDIF
     ELSE
          m.cresup = itecre.monasig
          IF f_appd()
               GATHER MEMVAR
          ENDIF
     ENDIF
     UNLOCK ALL
     SELECT itecre
ENDSCAN
SELECT cresup
RETURN
*
FUNCTION rolcre
PRIVATE alis, vkeyf, vkeyi, vtp,  ;
        vtotal
alis = ALIAS()
SELECT itecre
vtp = RECNO()
vkeyi = itecre.periodo +  ;
        itecre.codcad +  ;
        itecre.codfte +  ;
        itecre.codpart
vkeyf = itecre.periodo +  ;
        itecre.codcad +  ;
        itecre.codfte +  ;
        itecre.codpart
GOTO TOP
vtotal = 0
SCAN FOR itecre.periodo +  ;
     itecre.codcad +  ;
     itecre.codfte +  ;
     itecre.codpart = vkeyi
     vtotal = vtotal + monasig
ENDSCAN
GOTO vtp
SELECT (alis)
RETURN vtotal
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

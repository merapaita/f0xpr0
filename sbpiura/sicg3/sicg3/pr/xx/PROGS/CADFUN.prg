PARAMETER vsistema
CLOSE DATABASES
USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 2 Maepre ALIAS maepre  ;
    ORDER Maepre1
vmens01 = 'Registro de Presupuesto'
vmens02 = ' Presupuesto : REVISION '
vmens04 = 'Dicho Presupuesto no fue encontrado'
vmens05 = 'No existe Presupuesto anterior'
vmens06 = 'No existe Presupuesto siguiente'
vmens07 = '¨ Desea Anular ‚ste Presupuesto ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Presupuesto ha sido anulado'
vmens10 = 'El Presupuesto ya est  Atendido'
vmens11 = 'El Presupuesto ha sido devuelto'
SELECT maepre
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
       TO 11, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle: Presupuesto'  ;
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
       '\<Eliminar' AT 24, 54
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
ON SELECTION PAD elimi OF mmenu DO elimi
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_0
CLEAR
@ 1, 2 SAY '          Periodo :'
@ 1, 40 SAY 'Corr. cadena Fun. :'
@ 3, 2 SAY '   Unidad Gestora :'
@ 4, 2 SAY ' Unidad Ejecutora :'
@ 5, 2 SAY '          Funci¢n :'
@ 6, 2 SAY '         Programa :'
@ 7, 2 SAY '      SubPrograma :'
@ 8, 2 SAY '  Activ./Proyecto :'
@ 9, 2 SAY '       Componente :'
@ 11, 2 SAY '             Meta :'
@ 12, 2 SAY '        Finalidad :'
@ 13, 2 SAY '      Descripci¢n :'
IF vsistema = '2'
     @ 16, 2 SAY  ;
       ' C¢digo Actividad :'
ENDIF
RETURN
*
PROCEDURE vista
SELECT maepre
STORE 0 TO vasigna
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 1, 22 SAY m.periodo
@ 1, 60 SAY m.codcad
@ 3, 22 SAY val_para(m.uniges, ;
  'UNIGES','V',22,40)
@ 4, 22 SAY val_para1(m.unieje, ;
  'UNIEJE' + ALLTRIM(m.uniges), ;
  'V',22,40)
@ 5, 22 SAY val_para(m.codfun, ;
  'CODFUN','V',22,40)
@ 6, 22 SAY val_para1(m.codprg, ;
  'CODPRG' + ALLTRIM(m.codfun), ;
  'V',22,40)
@ 7, 22 SAY val_para1(m.codspr, ;
  'CODSPR' + ALLTRIM(m.codprg), ;
  'V',22,40)
@ 8, 22 SAY val_para(m.actpry, ;
  'ACTPRY','V',22,40)
@ 9, 22 SAY val_para(m.codcom, ;
  'CODCOM','V',22,40)
@ 11, 22 SAY m.codmet
@ 12, 22 SAY m.codfin
@ 13, 22 SAY SUBSTR(m.descri, 1,  ;
  55)
@ 14, 22 SAY SUBSTR(m.descri, 56,  ;
  55)
IF vsistema = '2'
     @ 16, 22 SAY m.cod_act
ENDIF
RETURN
*
PROCEDURE revis
SELECT maepre
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
BROWSE FIELDS codcad :H =  ;
       'Cad.Fun.', uniges :H =  ;
       'UG', unieje :H = 'UE',  ;
       codfun :H = 'Fn', codprg  ;
       :H = 'Prg', codspr :H =  ;
       'SubPrg', actpry :H =  ;
       'Act/Pry', codcom :H =  ;
       'Compon.', codmet :H =  ;
       'Meta', codfin :H =  ;
       'Finalid.', xx =  ;
       IIF(vsistema = '2',  ;
       cod_act, ' ') :H =  ;
       'Actividad', descri :H =  ;
       'Descripci¢n' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELECT maepre
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
vcodfte = '  '
vcodcad = '    '
vuniges = '  '
vunieje = '   '
DEFINE WINDOW lista FROM 09, 12  ;
       TO 16, 68 FLOAT TITLE  ;
       ' °° B£squeda °° ' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW lista
@ 1, 2 SAY ' Periodo : ' GET  ;
  vperiodo PICTURE '!!'
@ 2, 2 SAY 'UNI.GES. : ' GET  ;
  vuniges PICTURE '!!' VALID  ;
  val_para(vuniges,'UNIGES',' ', ;
  14,30)
@ 3, 2 SAY 'UNI.EJE. : ' GET  ;
  vunieje PICTURE '!!!' VALID  ;
  val_para1(vunieje,'UNIEJE' +  ;
  vuniges,' ',14,30)
@ 4, 2 SAY '  Cadena : ' GET  ;
  vcodcad PICTURE '!!!!' VALID  ;
  val_codcad(vcodcad,vperiodo +  ;
  ALLTRIM(vuniges) +  ;
  ALLTRIM(vunieje),' ',14,30)
READ VALID val_read()
DEACTIVATE WINDOW lista
IF EMPTY(vperiodo) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SET ORDER TO 1
     SEEK vperiodo +  ;
          ALLTRIM(vuniges) +  ;
          ALLTRIM(vunieje) +  ;
          ALLTRIM(vcodcad)
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
     DO standby WITH vmens12
     RETURN
ENDIF
SELECT maepre
SCATTER MEMVAR
vdescri1 = SUBSTR(m.descri, 1,  ;
           55)
vdescri2 = SUBSTR(m.descri, 56,  ;
           55)
ACTIVATE WINDOW wind_0
@ 1, 22 SAY m.periodo
@ 1, 60 SAY m.codcad
@ 3, 22 GET m.uniges PICTURE '!!'  ;
  VALID val_para(m.uniges, ;
  'UNIGES',' ',22,40)
@ 4, 22 GET m.unieje PICTURE  ;
  '!!!' VALID val_para1(m.unieje, ;
  'UNIEJE' + ALLTRIM(m.uniges), ;
  ' ',22,40)
@ 5, 22 GET m.codfun PICTURE '!!'  ;
  VALID val_para(m.codfun, ;
  'CODFUN',' ',22,40)
@ 6, 22 GET m.codprg PICTURE  ;
  '!!!' VALID val_para1(m.codprg, ;
  'CODPRG' + ALLTRIM(m.codfun), ;
  ' ',22,40)
@ 7, 22 GET m.codspr PICTURE  ;
  '!!!!' VALID val_para1(m.codspr, ;
  'CODSPR' + ALLTRIM(m.codprg), ;
  ' ',22,40)
@ 8, 22 GET m.actpry PICTURE  ;
  '!!!!!!' VALID  ;
  val_para(m.actpry,'ACTPRY',' ', ;
  22,40)
@ 9, 22 GET m.codcom PICTURE  ;
  '!!!!!' VALID val_para(m.codcom, ;
  'CODCOM',' ',22,40)
@ 11, 22 GET m.codmet
@ 12, 22 GET m.codfin
@ 13, 22 GET vdescri1
@ 14, 22 GET vdescri2
IF vsistema = '2'
     @ 16, 22 GET m.cod_act  ;
       PICTURE '!!!!!!!!!'
ENDIF
READ VALID val_read()
IF LASTKEY() <> 27
     m.estado = '00'
     m.fecemi = DATE()
     m.descri = vdescri1 +  ;
                vdescri2
     IF LASTKEY() <> 27
          SELECT maepre
          GATHER MEMVAR
     ELSE
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT maepre
DO pantalla
DO vista
RETURN
*
PROCEDURE ingre
SELECT maepre
v = RECNO()
DO pantalla
SCATTER BLANK MEMVAR
m.periodo = PADL(ALLTRIM(STR(YEAR(DATE()) -  ;
            2000)), 2, '0')
STORE SPACE(55) TO vdescri1,  ;
      vdescri2
@ 1, 22 GET m.periodo PICTURE  ;
  '!!'
@ 1, 60 GET m.codcad PICTURE  ;
  '!!!!'
@ 3, 22 GET m.uniges PICTURE '!!'  ;
  VALID val_para(m.uniges, ;
  'UNIGES',' ',22,40)
@ 4, 22 GET m.unieje PICTURE  ;
  '!!!' VALID val_para1(m.unieje, ;
  'UNIEJE' + ALLTRIM(m.uniges), ;
  ' ',22,40)
@ 5, 22 GET m.codfun PICTURE '!!'  ;
  VALID val_para(m.codfun, ;
  'CODFUN',' ',22,40)
@ 6, 22 GET m.codprg PICTURE  ;
  '!!!' VALID val_para1(m.codprg, ;
  'CODPRG' + ALLTRIM(m.codfun), ;
  ' ',22,40)
@ 7, 22 GET m.codspr PICTURE  ;
  '!!!!' VALID val_para1(m.codspr, ;
  'CODSPR' + ALLTRIM(m.codprg), ;
  ' ',22,40)
@ 8, 22 GET m.actpry PICTURE  ;
  '!!!!!!' VALID  ;
  val_para(m.actpry,'ACTPRY',' ', ;
  22,40)
@ 9, 22 GET m.codcom PICTURE  ;
  '!!!!!' VALID val_para(m.codcom, ;
  'CODCOM',' ',22,40)
@ 11, 22 GET m.codmet
@ 12, 22 GET m.codfin
@ 13, 22 GET vdescri1
@ 14, 22 GET vdescri2
IF vsistema = '2'
     @ 16, 22 GET m.cod_act  ;
       PICTURE '!!!!!!!!!' VALID  ;
       val_codact(m.cod_act)
ENDIF
READ VALID val_read()
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado'
     GOTO v
     DO vista
     RETURN
ELSE
     SEEK m.periodo + m.codcad +  ;
          m.uniges + m.unieje
     IF FOUND()
          DO standby WITH  ;
             'Ya est  registrado.'
          GOTO v
          DO pantalla
          DO vista
          RETURN
     ENDIF
     IF f_appd()
          m.estado = '00'
          m.fecemi = DATE()
          m.descri = vdescri1 +  ;
                     vdescri2
          IF LASTKEY() <> 27
               SELECT maepre
               GATHER MEMVAR
          ELSE
          ENDIF
     ELSE
          SELECT maepre
          GOTO BOTTOM
     ENDIF
ENDIF
UNLOCK ALL
SELECT maepre
DO vista
RETURN
*
PROCEDURE actcodcad
valias = ALIAS()
SELECT parma
SEEK 'CORRELCODCAD'
IF FOUND()
     m.codcad = PADL(nument + 1,  ;
                4, '0')
ELSE
     DO standby WITH  ;
        'No est  inicializado el correlativo de cadena'
     m.codcad = '    '
ENDIF
SELECT (valias)
RETURN
*
PROCEDURE elimi
SELECT maepre
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  .NOT. estado <> '  00'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ELIMINAR ‚ste Presupuesto ?' ;
           )
IF velimina .AND. (RLOCK() .OR.  ;
   f_lock(1))
     DELETE NEXT 1
     SKIP
     IF EOF()
          GOTO BOTTOM
     ENDIF
     DO vista
ENDIF
UNLOCK ALL
RETURN
*
PROCEDURE lista
SELECT maepre
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO cadfun
ENDIF
SELECT maepre
GOTO vtemp
DO vista
RETURN
*
PROCEDURE cadfun
vtemo = RECNO()
DEFINE WINDOW lis FROM 4, 15 TO  ;
       20, 65 FLOAT TITLE  ;
       'Listado Actividad / Proyecto'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro
vcodcad = SPACE(4)
vperiodo = '  '
STORE SPACE(2) TO vuniges
STORE SPACE(3) TO vunieje
@ 00, 01 SAY  ;
  '          Per¡odo : ' GET  ;
  vperiodo
@ 02, 01 SAY  ;
  '   Unidad Gestora : ' GET  ;
  vuniges PICTURE '!!' VALID  ;
  val_para(vuniges,'UNIGES',' ', ;
  22,20)
@ 04, 01 SAY  ;
  ' Unidad Ejecutora : ' GET  ;
  vunieje PICTURE '!!!' VALID  ;
  val_para1(vunieje,'UNIEJE' +  ;
  vuniges,' ',22,20)
@ 06, 01 SAY  ;
  ' Cadena Funcional : '
@ 06, 22 GET vcodcad PICTURE  ;
  '!!!!' VALID IIF( .NOT.  ;
  EMPTY(vcodcad),  ;
  val_codcad(vcodcad,vperiodo +  ;
  ALLTRIM(vuniges) +  ;
  ALLTRIM(vunieje),' ',22,20),  ;
  .T.)
@ 08, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     vtitulo = ' En General '
     IF  .NOT. EMPTY(vcodcad)
          SET FILTER TO periodo + uniges;
+ unieje + codcad = vperiodo + ALLTRIM(vuniges);
+ ALLTRIM(vunieje) + ALLTRIM(vcodcad)
     ELSE
          SET FILTER TO periodo + uniges;
+ unieje = vperiodo + ALLTRIM(vuniges);
+ ALLTRIM(vunieje)
     ENDIF
     IF  .NOT. EOF()
          DO reporte WITH 2,  ;
             'CADFUN',  ;
             'Cadenas Funcionales'
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
ENDIF
GOTO vtemo
DO vista
RETURN
*
FUNCTION valcad
SELECT presu
vtem = RECNO()
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
BROWSE FIELDS codcad :H =  ;
       'Cad.Fun.', uniges :H =  ;
       'UG', unieje :H = 'UE',  ;
       codfun :H = 'Fn', codprg  ;
       :H = 'Prg', codspr :H =  ;
       'SubPrg', actpry :H =  ;
       'Act/Pry', codcom :H =  ;
       'Compon.', codmet :H =  ;
       'Meta', codfin :H =  ;
       'Finalid.', descri :H =  ;
       'Describci¢n' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
vcli = codcad
SHOW MENU mmenu
ON KEY LABEL F10
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
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_c1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
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
RETURN .T.
*
FUNCTION val_codact
PARAMETER vcodigo
vorder = ORDER()
valias = ALIAS()
vrecno = RECNO()
SELECT maepre
SET ORDER TO maepre5
SEEK vcodigo
IF FOUND()
     DO standby WITH  ;
        'Ya existe esta actividad'
     RETURN .F.
ENDIF
RETURN .T.
*

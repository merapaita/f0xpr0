vmens01 = ' Catalogo de Asignaciones Presupuestales: REGISTRO '
vmens02 = 'Revisi¢n de Asignaciones Presupuestales'
vmens04 = 'Dicha Asignacion no fue encontrada'
vmens05 = 'No existe Asignacion anterior'
vmens06 = 'No existe Asignaci¢n siguiente'
vmens07 = '¨ Desea ELIMINAR ‚sta Asignacion ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Asignaci¢n ha sido anulado'
vmens10 = 'La Asignaci¢n ya fue atendido'
CLOSE DATABASES
USE IN 1 iteCla ALIAS itecla  ;
    ORDER IteCla1
USE IN 2 CatAsi ALIAS catasi  ;
    ORDER CatAsi5
USE IN 3 cuentas ALIAS cuenta  ;
    ORDER cuentas4
USE IN 4 Parmae ALIAS parma ORDER  ;
    Parmae1
GOTO BOTTOM
SCATTER BLANK MEMVAR
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 23, 79 TITLE vmens02  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_c1 FROM 00, 00  ;
       TO 23, 79 COLOR SCHEME 10
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
ACTIVATE WINDOW wind_0
CLEAR
@ 01, 2 SAY 'Asignaci¢n Presup.:'
@ 02, 2 SAY 'Tipo de Documento :'
@ 04, 2 SAY '           Enlaca :'
@ 06, 2 SAY '      Descripci¢n :'
@ 08, 2 SAY '     Patrim. Debe :'
@ 10, 2 SAY '    Patrim. Haber :'
@ 12, 2 SAY '     Presup. Debe :'
@ 14, 2 SAY '    Presup. Haber :'
@ 16, 2 SAY '       Orden Debe :'
@ 18, 2 SAY '      Orden Haber :'
RETURN
*
PROCEDURE vista
SELECT itecla
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 01, 22 SAY m.codpart
@ 02, 22 SAY val_para(m.tipdoc, ;
  'DOCENL','V',22,40)
@ 04, 22 SAY m.codcla
@ 06, 22 SAY m.descri
@ 08, 22 SAY m.cuentad
@ 08, 22 SAY val_cta(m.cuentad,8, ;
  22)
@ 10, 22 SAY m.cuentah
@ 10, 22 SAY val_cta(m.cuentah,10, ;
  22)
@ 12, 22 SAY m.dastpre
@ 12, 22 SAY val_cta(m.dastpre,12, ;
  22)
@ 14, 22 SAY m.hastpre
@ 14, 22 SAY val_cta(m.hastpre,14, ;
  22)
@ 16, 22 SAY m.dastord
@ 16, 22 SAY val_cta(m.dastord,16, ;
  22)
@ 18, 22 SAY m.hastord
@ 18, 22 SAY val_cta(m.hastord,18, ;
  22)
RETURN
*
PROCEDURE revis
SELECT itecla
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
BROWSE FIELDS codpart :H =  ;
       'Partida', codcla :H =  ;
       'Enl', tipdoc :H = 'Doc',  ;
       descri :H = 'Descripci¢n'  ;
       : 30, cuentad :H = 'Debe',  ;
       cuentah :H = 'Haber'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW wind_1
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
SELECT itecla
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
vtemp = RECNO()
STORE SPACE(9) TO vcoding
STORE SPACE(2) TO vsubing
ACTIVATE WINDOW standby
@ 1, 03 SAY '       Codigo :' GET  ;
  vcoding PICTURE '@!'
READ
DEACTIVATE WINDOW standby
IF EMPTY(vcoding) .OR. LASTKEY() =  ;
   27
     GOTO vtemp
ELSE
     SEEK vcoding + vsubing
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Dicho codigo no est  registrado'
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
ACTIVATE WINDOW wind_0
SELECT itecla
SCATTER MEMVAR
IF RLOCK() .OR. f_lock(1)
     @ 01, 22 GET m.codpart  ;
       DISABLE
     @ 02, 22 GET m.tipdoc VALID  ;
       val_para(m.tipdoc,'DOCENL', ;
       ' ',22,40)
     @ 04, 22 GET m.codcla  ;
       DISABLE
     @ 06, 22 GET m.descri  ;
       FUNCTION '!S30'
     @ 08, 22 GET m.cuentad  ;
       PICTURE '999999999999999'  ;
       VALID val_cta(m.cuentad,8, ;
       22)
     @ 10, 22 GET m.cuentah  ;
       PICTURE '999999999999999'  ;
       VALID val_cta(m.cuentah,10, ;
       22)
     @ 12, 22 GET m.dastpre  ;
       PICTURE '999999999999999'  ;
       VALID val_cta(m.dastpre,12, ;
       22)
     @ 14, 22 GET m.hastpre  ;
       PICTURE '999999999999999'  ;
       VALID val_cta(m.hastpre,14, ;
       22)
     @ 16, 22 GET m.dastord  ;
       PICTURE '999999999999999'  ;
       VALID val_cta(m.dastord,16, ;
       22)
     @ 18, 22 GET m.hastord  ;
       PICTURE '999999999999999'  ;
       VALID val_cta(m.hastord,18, ;
       22)
     READ VALID val_read()
     IF LASTKEY() <> 27
          SELECT itecla
          GATHER MEMVAR
     ENDIF
     DO vista
ENDIF
UNLOCK
RETURN
*
PROCEDURE ingre
SELECT itecla
DO pantalla
SCATTER BLANK MEMVAR
@ 01, 22 GET m.codpart PICTURE  ;
  '99999999999999' VALID  ;
  v_part(m.codpart)
@ 02, 22 GET m.tipdoc VALID  ;
  val_para(m.tipdoc,'DOCENL',' ', ;
  22,40) .AND.  ;
  correnl(m.codpart)
@ 04, 22 GET m.codcla PICTURE  ;
  '99' WHEN .F.
@ 06, 22 GET m.descri FUNCTION  ;
  '!S30'
@ 08, 22 GET m.cuentad PICTURE  ;
  '999999999999999' VALID  ;
  val_cta(m.cuentad,8,22)
@ 10, 22 GET m.cuentah PICTURE  ;
  '999999999999999' VALID  ;
  val_cta(m.cuentah,10,22)
@ 12, 22 GET m.dastpre PICTURE  ;
  '999999999999999' VALID  ;
  val_cta(m.dastpre,12,22)
@ 14, 22 GET m.hastpre PICTURE  ;
  '999999999999999' VALID  ;
  val_cta(m.hastpre,14,22)
@ 16, 22 GET m.dastord PICTURE  ;
  '999999999999999' VALID  ;
  val_cta(m.dastord,16,22)
@ 18, 22 GET m.hastord PICTURE  ;
  '999999999999999' VALID  ;
  val_cta(m.hastord,18,22)
READ VALID val_read() .AND.  ;
     val_nvo()
IF LASTKEY() <> 27
     IF f_appd()
          GATHER MEMVAR
     ELSE
          GOTO BOTTOM
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado, no se graba nada'
     GOTO BOTTOM
ENDIF
UNLOCK ALL
SELECT itecla
DO vista
RETURN
*
FUNCTION val_cta
PARAMETER xcta, xfil, xcol
PRIVATE calias
calias = ALIAS()
IF EMPTY(xcta)
ELSE
     SELECT cuenta
     IF  .NOT. SEEK(xcta)
          xcta = SPACE(15)
     ENDIF
     = val_fun('Cuenta','Cuenta', ;
       "Cuenta+' '+Descri",xcta,1, ;
       xfil,xcol)
ENDIF
SELECT (calias)
RETURN .T.
*
FUNCTION correnl
PARAMETER xpart
SET FILTER TO tipdoc = m.tipdoc
IF SEEK(xpart)
     SCAN WHILE codpart = xpart
          m.codcla = codcla
     ENDSCAN
     m.codcla = PADL(ALLTRIM(STR(VAL(m.codcla) +  ;
                1)), 2, '0')
ELSE
     m.codcla = '01'
ENDIF
SET FILTER TO
SHOW GET m.codcla
RETURN .T.
*
FUNCTION v_part
PARAMETER vpar
IF SEEK(vpar, 'CatAsi')
ELSE
     vpar = SPACE(14)
     = valasi1(ALIAS(),vpar,'8', ;
       'CodPart','R')
     vpar = catasi.codpart
     vdespar = catasi.descri
ENDIF
RETURN .T.
*
FUNCTION val_nvo
PRIVATE nreg, mret
nreg = RECNO()
mret = .T.
IF LASTKEY() <> 27
     IF  .NOT. EMPTY(m.codpart)  ;
         .AND.  .NOT.  ;
         EMPTY(m.codcla) .AND.   ;
         .NOT. EMPTY(m.tipdoc)
          IF  .NOT.  ;
              SEEK(m.codpart +  ;
              m.codcla + tipdoc)
               mret = .T.
          ELSE
               DO standby WITH  ;
                  'Este Enlace ya fue Ingresada'
               mret = .F.
          ENDIF
     ELSE
          DO standby WITH  ;
             'Faltan Datos para procesar la Informaci¢n'
          mret = .F.
     ENDIF
ELSE
     mret = .T.
ENDIF
RETURN mret
*
PROCEDURE elimi
SELECT itecla
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ELIMINAR FISICAMENTE ‚ste ingreso ?' ;
           )
IF velimina .AND. (RLOCK() .OR.  ;
   f_lock(1))
     DELETE NEXT 1
     DO vista
ENDIF
UNLOCK
RETURN
*
PROCEDURE lista
SELECT itecla
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO reporte WITH 2,  ;
        'EnCatAsi',  ;
        ' Codigos de Ingresos '
ENDIF
SELECT itecla
GOTO vtemp
DO vista
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
RELEASE WINDOW wind_c1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*

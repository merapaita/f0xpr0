PARAMETER v_us
vmens01 = 'ÍÍ Seguridad ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ' +  ;
          cia +  ;
          'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
vmens02 = 'Revisi¢n de Seguridad de ' +  ;
          cia
vmens03 = 'Digite C¢digo de Usuario que desea :'
vmens04 = 'Dicho Usuario no fue encontrado'
vmens05 = 'No existe Usuario anterior'
vmens06 = 'No existe Usuario siguiente'
vmens07 = '¨ Est  seguro que desea ANULAR este Usuario ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'M¢dulos'
vmens10 = '¨ Est  seguro que desea ANULAR este m¢dulo ?'
vmens11 = 'EDICION'
USE IN 1 Usuario ALIAS usua ORDER  ;
    Usuario1
USE IN 2 IteUsuOp ALIAS iteu  ;
    ORDER IteUsuoP1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
SAVE SCREEN TO pantalla
HIDE POPUP ALL
SELECT iteu
SELECT usua
SELECT usua
IF v_us <> 'ADMIN'
     SET FILTER TO usuario = v_us
ENDIF
GOTO BOTTOM
SCATTER BLANK MEMVAR
DO inicia
DO pantalla
DO vista
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     ACTIVATE MENU fmenu
ENDDO
DO fin_opcion
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_f0
RELEASE WINDOW wind_f1
RELEASE WINDOW wind_f2
RELEASE WINDOW wind_f3
RELEASE MENU fmenu
RESTORE SCREEN FROM pantalla
RETURN
*
PROCEDURE inicia
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_f0 FROM 00, 00  ;
       TO 08, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_f1 FROM 00, 00  ;
       TO 23, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_f2 FROM 08, 00  ;
       TO 23, 79 TITLE vmens09  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_f3 FROM 03, 00  ;
       TO 15, 50 TITLE vmens11  ;
       DOUBLE COLOR SCHEME 10
DEFINE MENU fmenu COLOR SCHEME 3
DEFINE PAD revis OF fmenu PROMPT  ;
       '\<Revisa' AT 24, 00
DEFINE PAD busca OF fmenu PROMPT  ;
       '\<Busca' AT 24, 08
DEFINE PAD anter OF fmenu PROMPT  ;
       '\<Anterior' AT 24, 15
DEFINE PAD proxi OF fmenu PROMPT  ;
       '\<Siguiente' AT 24, 25
DEFINE PAD corri OF fmenu PROMPT  ;
       '\<Corrige' AT 24, 36 SKIP  ;
       FOR v_us <> 'ADMIN'
DEFINE PAD ingre OF fmenu PROMPT  ;
       '\<Ingresa' AT 24, 45 SKIP  ;
       FOR v_us <> 'ADMIN'
DEFINE PAD elimi OF fmenu PROMPT  ;
       '\<Elimina' AT 24, 54
DEFINE PAD lista OF fmenu PROMPT  ;
       '\<Lista ' AT 24, 63
DEFINE PAD termi OF fmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF fmenu DO revis
ON SELECTION PAD busca OF fmenu DO busca
ON SELECTION PAD anter OF fmenu DO anter
ON SELECTION PAD proxi OF fmenu DO proxi
ON SELECTION PAD corri OF fmenu DO corri
ON SELECTION PAD ingre OF fmenu DO ingre
ON SELECTION PAD elimi OF fmenu DO elimi
ON SELECTION PAD lista OF fmenu DO lista
ON SELECTION PAD termi OF fmenu DO termi
ACTIVATE SCREEN
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_f0
CLEAR
@ 1, 2 SAY '          Usuario :'
@ 3, 2 SAY '           Nombre :'
@ 4, 2 SAY '      Dependencia :'
@ 5, 2 SAY '            Nivel :'
@ 6, 2 SAY '    Observaciones :'
RETURN
*
PROCEDURE vista
ACTIVATE WINDOW wind_f0
ON KEY LABEL F3 DO PerCla
ON KEY LABEL F9 DO Vista_Det
SELECT usua
SCATTER MEMVAR
@ 1, 22 SAY m.usuario
@ 3, 22 SAY m.nombre
@ 4, 22 SAY val_para(m.coddep, ;
  'CODDEP','A',22,50,7)
@ 5, 22 SAY m.numdep PICTURE '99'
@ 6, 22 SAY m.observ
DO vista_hijo
RETURN
*
PROCEDURE percla
SELECT usua
cclave1 = 'A'
cclave2 = SPACE(10)
cclave3 = SPACE(10)
DEFINE WINDOW pass FROM 05, 10 TO  ;
       15, 50 TITLE  ;
       'Clave de Opciones del Sistema'  ;
       DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW pass
@ 01, 03 SAY ' Usuario : '
@ 01, 20 SAY m.usuario
IF LASTKEY() <> 27
     DO WHILE  .NOT.  ;
        (ALLTRIM(m.clave)== ;
        ALLTRIM(cclave1))
          @ 03, 03 SAY IIF( .NOT.  ;
            EMPTY(m.clave),  ;
            'Ingrese su clave anterior :',  ;
            SPACE(10))
          @ 03, 30 SAY SPACE(10)
          cclave1 = IIF( .NOT.  ;
                    EMPTY(m.clave),  ;
                    aster(3,30),  ;
                    '')
          IF LASTKEY() = 27
               DEACTIVATE WINDOW  ;
                          pass
               RELEASE WINDOW  ;
                       pass
               RETURN
          ENDIF
          cclave1 = CHRTRAN(cclave1, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
          IF  .NOT.  ;
              (ALLTRIM(m.clave) ==  ;
              ALLTRIM(cclave1))
               DO standby WITH  ;
                  'Error al digitar su clave. Intente de nuevo'
               LOOP
          ENDIF
     ENDDO
     cclave2 = 'A'
     cclave3 = 'B'
     DO WHILE  .NOT. (cclave2== ;
        cclave3)
          @ 05, 03 SAY  ;
            'Ingrese su Nueva Clave :'
          @ 05, 30 SAY SPACE(10)
          @ 07, 30 SAY SPACE(10)
          cclave2 = aster(5,30)
          @ 07, 03 SAY  ;
            'Repita su Nueva Clave: '
          @ 07, 30 SAY SPACE(10)
          cclave3 = aster(7,30)
          IF  .NOT. (cclave2 ==  ;
              cclave3)
               DO standby WITH  ;
                  'Clave Incorrecta Intente de nuevo'
          ENDIF
     ENDDO
     cclave3 = CHRTRAN(cclave3, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
     IF LASTKEY() <> 27
          REPLACE usua.clave WITH  ;
                  cclave3
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso Cancelado'
ENDIF
DEACTIVATE WINDOW pass
RELEASE WINDOW pass
RETURN
*
FUNCTION aster
PARAMETER xfil, xcol
m.key = 0
xpass = ''
IF LASTKEY() <> 27
     @ xfil, xcol SAY ''
     DO WHILE m.key<>13 .AND.  ;
        m.key<>27
          m.key = INKEY(0)
          DO CASE
               CASE BETWEEN(m.key,  ;
                    65, 90) .OR.  ;
                    BETWEEN(m.key,  ;
                    97, 122) .OR.  ;
                    BETWEEN(m.key,  ;
                    48, 57) .OR.  ;
                    BETWEEN(m.key,  ;
                    164, 165)
                    xpass = xpass +  ;
                            CHR(m.key)
               CASE m.key = 19  ;
                    .OR. m.key =  ;
                    127 .OR.  ;
                    m.key = 7
                    @ ROW(),  ;
                      COL() - 1  ;
                      SAY ' '
                    @ ROW(),  ;
                      COL() - 1  ;
                      SAY ''
                    xpass = SUBSTR(xpass,  ;
                            1,  ;
                            LEN(xpass) -  ;
                            1)
               OTHERWISE
          ENDCASE
          IF LASTKEY() <> 27
               @ xfil, xcol SAY  ;
                 REPLICATE('',  ;
                 LEN(xpass))
          ENDIF
     ENDDO
ENDIF
RETURN xpass
*
PROCEDURE vista_hijo
SELECT iteu
vusucla = CHRTRAN(usua.usuario, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
SEEK ALLTRIM(vusucla)
IF FOUND()
     BROWSE FIELDS xsis = IIF(  ;
            .NOT. EMPTY(sistema),  ;
            val_para(sistema, ;
            'SISTEM','D'),  ;
            sistema) : 30 :H =  ;
            'Sistema', xmod =  ;
            IIF( .NOT.  ;
            EMPTY(modulo),  ;
            val_para1(modulo, ;
            'MODULO' + sistema, ;
            'D'), modulo) : 30 :H =  ;
            'M¢dulo', xopc = IIF(  ;
            .NOT. EMPTY(opcion),  ;
            val_para(opcion, ;
            'OPCION','D'),  ;
            opcion) : 10 :H =  ;
            'Opci¢n' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_f2 KEY  ;
            ALLTRIM(vusucla)  ;
            TIMEOUT 0.001 
ELSE
     ACTIVATE WINDOW wind_f2
     CLEAR
     @ 5, 10 SAY  ;
       'USUARIO NO TIENE DETALLE...'
ENDIF
RETURN
*
PROCEDURE vista_det
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteu
vusucla = CHRTRAN(usua.usuario, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
SEEK ALLTRIM(vusucla)
IF FOUND()
     ACTIVATE WINDOW wind_f2
     BROWSE FIELDS xsis = IIF(  ;
            .NOT. EMPTY(sistema),  ;
            val_para(sistema, ;
            'SISTEM','D'),  ;
            sistema) : 30 :H =  ;
            'Sistema', xmod =  ;
            IIF( .NOT.  ;
            EMPTY(modulo),  ;
            val_para1(modulo, ;
            'MODULO' + sistema, ;
            'D'), modulo) : 30 :H =  ;
            'M¢dulo', xopc = IIF(  ;
            .NOT. EMPTY(opcion),  ;
            val_para(opcion, ;
            'OPCION','D'),  ;
            opcion) : 10 :H =  ;
            'Opci¢n' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_f2 KEY  ;
            ALLTRIM(vusucla)
ELSE
     ACTIVATE WINDOW wind_f2
     CLEAR
     @ 5, 10 SAY  ;
       'USUARIO NO TIENE DETALLE...'
ENDIF
ON KEY LABEL F10
RETURN
*
PROCEDURE modcla
SELECT usua
ccla1 = SPACE(10)
ccla2 = SPACE(10)
DEFINE WINDOW pass FROM 05, 10 TO  ;
       15, 50 TITLE  ;
       'Clave de Usuarios del Sistema'  ;
       DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW pass
@ 01, 03 SAY '         Clave : '
@ 02, 20 GET ccla1 COLOR ,N 
@ 03, 03 SAY ' Repetir Clave : '
@ 04, 20 GET ccla2 COLOR ,N 
READ VALID val_cla()
IF LASTKEY() <> 27
     ccla2 = CHRTRAN(ccla2, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
     REPLACE usua.clave WITH  ;
             ccla2
ELSE
     DO standby WITH  ;
        'Proceso Cancelado'
ENDIF
DEACTIVATE WINDOW pass
RELEASE WINDOW pass
*
FUNCTION val_cla
IF ccla1 <> ccla2
     DO standby WITH  ;
        'Clave Ingresada es Incorrecta. Intente de nuevo.'
     ccla1 = SPACE(10)
     ccla2 = SPACE(10)
     _CUROBJ = OBJNUM(ccla1)
     SHOW GETS
     RETURN .F.
ELSE
     RETURN val_read()
ENDIF
RETURN .T.
*
PROCEDURE revis
ON KEY LABEL F3
SELECT usua
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU fmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS usuario :H =  ;
       'Usuario', nombre :H =  ;
       'Nombre', observ :H =  ;
       'Observaciones' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_f1
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ELSE
     SCATTER MEMVAR
ENDIF
SHOW MENU fmenu
ON KEY LABEL F10
DO vista
RETURN
*
PROCEDURE busca
ON KEY LABEL F3
SELECT usua
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
ACTIVATE WINDOW standby
STORE SPACE(LEN(m.usuario)) TO  ;
      vbusca
@ 1, 3 SAY vmens03 COLOR SCHEME 1  ;
  GET vbusca
READ
DEACTIVATE WINDOW standby
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
     GOTO vtemp
ELSE
     IF  .NOT.  ;
         SEEK(ALLTRIM(vbusca))
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          SCATTER MEMVAR
          DO vista
     ENDIF
ENDIF
RETURN
*
PROCEDURE anter
SELECT usua
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
SELECT usua
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
ON KEY LABEL F3
SELECT usua
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SELECT usua
IF f_lock(1)
     ACTIVATE WINDOW wind_f0
     @ 4, 22 CLEAR TO 4, 79
     @ 1, 22 SAY m.usuario
     @ 3, 22 GET m.nombre PICTURE  ;
       '@!'
     @ 4, 22 GET m.coddep PICTURE  ;
       '!!!!!!' VALID  ;
       val_para(m.coddep,'CODDEP', ;
       ' ',22,40,8)
     @ 5, 22 GET m.numdep PICTURE  ;
       '99'
     @ 6, 22 GET m.observ PICTURE  ;
       '@!'
     READ
     IF LASTKEY() <> 27
          SELECT iteu
          ok = trabaja_hi()
          IF ok
               SELECT usua
               GATHER MEMVAR
          ENDIF
     ELSE
          SELECT usua
          SCATTER MEMVAR
     ENDIF
     DO vista
ENDIF
UNLOCK ALL
RETURN
*
PROCEDURE ingre
ON KEY LABEL F3
ACTIVATE WINDOW wind_f0
SELECT usua
vlastkey = 0
SCATTER BLANK MEMVAR
@ 4, 22 CLEAR TO 4, 79
@ 1, 22 GET m.usuario PICTURE  ;
  'NNNNNNNNNN'
READ
m.usuario = UPPER(m.usuario)
IF LASTKEY() = 27 .OR.  ;
   EMPTY(m.usuario)
     DO standby WITH  ;
        'Proceso cancelado. No se graba ning£n cambio.'
     GOTO BOTTOM
ELSE
     SELECT usua
     IF SEEK(ALLTRIM(m.usuario))
          DO standby WITH  ;
             'El C¢digo del Usuario ya est  registrado. Proceda a corregir datos.'
          DO vista
          DO corri
     ELSE
          @ 1, 22 SAY m.usuario
          @ 3, 22 GET m.nombre  ;
            PICTURE '@!'
          @ 4, 22 GET m.coddep  ;
            PICTURE '!!!!!!'  ;
            VALID  ;
            val_para(m.coddep, ;
            'CODDEP',' ',22,40, ;
            8)
          @ 5, 22 GET m.numdep  ;
            PICTURE '99'
          @ 6, 22 GET m.observ  ;
            PICTURE '@!'
          m.flag = IIF(m.usuario =  ;
                   'ADMIN', '*',  ;
                   '')
          READ
          IF LASTKEY() <> 27
               SELECT iteu
               ok = trabaja_hi()
               SELECT usua
               IF ok
                    IF f_appd()
                         GATHER MEMVAR
                    ENDIF
               ENDIF
          ELSE
               GOTO BOTTOM
          ENDIF
     ENDIF
ENDIF
UNLOCK ALL
DO vista
RETURN
*
FUNCTION trabaja_hi
PRIVATE vfun
vfun = .T.
HIDE MENU fmenu
ACTIVATE SCREEN
DO logos WITH rotulo1,  ;
   '°°°°F5->Agregar°°°°°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°°°°°F10->Terminar°°°°'
ON KEY LABEL F2 DO Edita_item
ON KEY LABEL F5 DO Agreg_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteu
vmens = ''
vusucla = CHRTRAN(m.usuario, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
SEEK ALLTRIM(vusucla)
IF  .NOT. FOUND()
     DO agreg_item
ENDIF
BROWSE FIELDS xsis = IIF( .NOT.  ;
       EMPTY(sistema),  ;
       val_para(sistema,'SISTEM', ;
       'D'), sistema) : 30 :H =  ;
       'Sistema', xmod = IIF(  ;
       .NOT. EMPTY(modulo),  ;
       val_para1(modulo,'MODULO' +  ;
       sistema,'D'), modulo) : 30  ;
       :H = 'M¢dulo', xopc = IIF(  ;
       .NOT. EMPTY(opcion),  ;
       val_para(opcion,'OPCION', ;
       'D'), opcion) : 10 :H =  ;
       'Opci¢n' NOMENU NOAPPEND  ;
       NODELETE WINDOW wind_f2  ;
       KEY ALLTRIM(vusucla)
SELECT iteu
SEEK ALLTRIM(vusucla)
SCAN WHILE vusucla = iteu.user
     IF EMPTY(iteu.user) .OR.  ;
        EMPTY(iteu.sistema) .OR.  ;
        EMPTY(iteu.modulo) .OR.  ;
        EMPTY(iteu.opcion)
          DELETE NEXT 1
     ENDIF
ENDSCAN
SEEK ALLTRIM(vusucla)
FLUSH
ACTIVATE SCREEN
DO logos WITH rotulo1,  ;
   ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
SHOW MENU fmenu
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
RETURN vfun
*
PROCEDURE agreg_item
SELECT iteu
IF f_appd()
     REPLACE user WITH vusucla
ENDIF
DO edita_item
RETURN
*
PROCEDURE edita_item
ACTIVATE WINDOW wind_f3
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SCATTER MEMVAR
@ 1, 1 SAY '  Usuario: ' +  ;
  usua.nombre
@ 3, 1 SAY '  Sistema:' GET  ;
  m.sistema PICTURE '!!' VALID  ;
  val_para(m.sistema,'SISTEM',' ', ;
  12)
@ 5, 1 SAY '   Modulo:' GET  ;
  m.modulo PICTURE '!!' VALID  ;
  val_para1(m.modulo,'MODULO' +  ;
  m.sistema,' ',12)
@ 7, 1 SAY '   Opci¢n:' GET  ;
  m.opcion PICTURE '!' VALID  ;
  val_para(m.opcion,'OPCION',' ', ;
  12) .AND. vopcion()
READ VALID val_read()
IF LASTKEY() <> 27
     IF f_lock(1)
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH  ;
        'Cancela Informacion'
ENDIF
DEACTIVATE WINDOW wind_f3
ON KEY LABEL F2 DO Edita_item
ON KEY LABEL F5 DO agreg_item
ON KEY LABEL F8 DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN
*
PROCEDURE elimi_item
IF f_lock(1)
     STORE yesno(vmens10) TO  ;
           velimina
     IF velimina
          SELECT iteu
          DELETE NEXT 1
     ENDIF
ENDIF
RETURN
*
FUNCTION vopcion
PRIVATE vsistema
vmens = ''
vretorno = .F.
vsistema = ALLTRIM(m.sistema)
vmodulo = ALLTRIM(m.modulo)
vopcion = ALLTRIM(m.opcion)
vlastkey = LASTKEY()
IF vlastkey = -7
     vretorno = .T.
ELSE
     vreg = RECNO()
     IF SEEK(ALLTRIM(vusucla) +  ;
        vsistema + vmodulo +  ;
        vopcion)
          IF vreg <> RECNO()
               vretorno = .F.
               vmens = 'M¢dulo ya seleccionado'
               DO standby WITH  ;
                  vmens
               GOTO vreg
          ELSE
               vretorno = .T.
          ENDIF
     ELSE
          GOTO vreg
          vretorno = .T.
     ENDIF
ENDIF
RETURN vretorno
*
FUNCTION opcdescri
DO CASE
     CASE iteu.modulo == 'A'
          RETURN ' Revisa '
     CASE iteu.modulo == 'B'
          RETURN ' Busca'
     CASE iteu.modulo == 'C'
          RETURN ' Anterior'
     CASE iteu.modulo == 'D'
          RETURN ' Siguiente'
     CASE iteu.modulo == 'E'
          RETURN ' Corrige'
     CASE iteu.modulo == 'F'
          RETURN ' Ingresa'
     CASE iteu.modulo == 'G'
          RETURN ' Anula'
     CASE iteu.modulo == 'H'
          RETURN ' Lista'
ENDCASE
RETURN ''
*
PROCEDURE elimi
ON KEY LABEL F3
STORE yesno(vmens07) TO velimina
IF  .NOT. velimina
     RETURN
ENDIF
IF f_lock(1)
     vusucla = CHRTRAN(usua.usuario, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
     SELECT iteu
     SEEK ALLTRIM(vusucla)
     SCAN WHILE vusucla =  ;
          iteu.usucla
          IF f_lock(1)
               DELETE
          ENDIF
     ENDSCAN
     SELECT usua
     DELETE
     UNLOCK ALL
ENDIF
SELECT usua
IF  .NOT. BOF()
     SKIP -1
ELSE
     GOTO BOTTOM
ENDIF
DO vista
RETURN
*
PROCEDURE lista
ON KEY LABEL F3
ACTIVATE WINDOW standby
SELECT usua
vreg = RECNO()
GOTO TOP
numini = usua.usuario
GOTO BOTTOM
numfin = usua.usuario
@ 0, 1 SAY 'Usuario Inicial:' GET  ;
  numini VALID numini <= numfin  ;
  ERROR  ;
  'Usuario inicial mayor que el final'
@ 1, 1 SAY '          Final:' GET  ;
  numfin VALID numini <= numfin  ;
  ERROR  ;
  'Usuario inicial mayor que el final'
READ
DEACTIVATE WINDOW standby
DO repprg WITH 'ImpUsu',  ;
   ' Relaci¢n de Usuarios '
SELECT usua
GOTO vreg
DO vista
RETURN
*
PROCEDURE impusu
PARAMETER _desti
IF _desti = 2
     SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINTER
STORE 0 TO lineas
SELECT usua
SEEK numini
SCAN WHILE numini <= usua.usuario  ;
     .AND. numfin >=  ;
     usua.usuario
     SCATTER MEMVAR
     IF lineas > 50 .OR. lineas =  ;
        0
          lineas = 0
          DO imptit WITH lineas,  ;
             .T.
     ELSE
          DO imptit WITH lineas,  ;
             .F.
     ENDIF
     vusucla = CHRTRAN(usua.usuario, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
     SELECT iteu
     SEEK ALLTRIM(vusucla)
     SCAN WHILE vusucla =  ;
          iteu.usucla
          IF lineas > 50 .OR.  ;
             lineas = 0
               lineas = 0
               DO imptit WITH  ;
                  lineas, .T.
          ENDIF
          SELECT iteu
          @ lineas, 0 SAY  ;
            iteu.modulo
          @ lineas, 10 SAY  ;
            xdescri()
          lineas = lineas + 1
     ENDSCAN
     SELECT usua
ENDSCAN
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE imptit
PARAMETER vlineas, cabecera
IF vlineas = 0
     vtitulo = 'Relaci¢n de Usuarios'
     @ 1, 0 SAY SUBSTR(cia, 1,  ;
       20)
     @ 1, INT((80 - LEN(vtitulo)) /  ;
       2) SAY vtitulo
     @ 1, 73 SAY DATE()
     vlineas = 2
ENDIF
vlineas = vlineas + 1
@ vlineas, 0 SAY '   Usuario: ' +  ;
  m.usuario
vlineas = vlineas + 1
@ vlineas, 0 SAY '    Nombre: ' +  ;
  m.nombre
vlineas = vlineas + 1
xclave = CHRTRAN(m.clave, 'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#',;
'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789')
@ vlineas, 0 SAY '     Clave: ' +  ;
  xclave
vlineas = vlineas + 1
IF cabecera
     @ vlineas, 00 SAY  ;
       REPLICATE('-', 80)
     vlineas = vlineas + 1
     @ vlineas, 00 SAY 'M¢dulo'
     @ vlineas, 10 SAY  ;
       '    Descripci¢n     '
     vlineas = vlineas + 1
     @ vlineas, 00 SAY  ;
       REPLICATE('-', 80)
     vlineas = vlineas + 1
     SELECT iteu
ELSE
     vlineas = vlineas + 1
ENDIF
RETURN
*
FUNCTION valida
PARAMETER valor, compara
valor = UPPER(valor)
compara = UPPER(compara)
IF valor <> compara
     valor = SPACE(LEN(valor))
     @ 4, 22 SAY valor PICTURE  ;
       '!!!!!!!!!!'
     RETURN -1
ENDIF
RETURN 1
*

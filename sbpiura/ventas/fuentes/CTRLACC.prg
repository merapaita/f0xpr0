PARAMETER v_us
vmens01 = '様 Seguridad 様様様様様様様様?' +  ;
          cia +  ;
          '様様様様様様様様様様様様様様様様様様様様様'
vmens02 = 'Revisi?n de Seguridad de ' +  ;
          cia
vmens03 = 'Digite C?digo de Usuario que desea :'
vmens04 = 'Dicho Usuario no fue encontrado'
vmens05 = 'No existe Usuario anterior'
vmens06 = 'No existe Usuario siguiente'
vmens07 = '? Est? seguro que desea ANULAR este Usuario ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'M?dulos'
vmens10 = '? Est? seguro que desea ANULAR este m?dulo ?'
USE IN 1 Usuario ALIAS usua ORDER  ;
    Usuario1
USE IN 2 IteUsuOp ALIAS iteu  ;
    ORDER IteUsuOp1
USE IN 3 MaeAcc ALIAS maeacc  ;
    ORDER MaeAcc1
USE IN 4 Parmae ALIAS parma ORDER  ;
    Parmae1
SAVE SCREEN TO pantalla
HIDE POPUP ALL
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
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_f0
RELEASE WINDOW wind_f1
RELEASE WINDOW wind_f2
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
DEFINE PAD termi OF fmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF fmenu DO revis
ON SELECTION PAD busca OF fmenu DO busca
ON SELECTION PAD anter OF fmenu DO anter
ON SELECTION PAD proxi OF fmenu DO proxi
ON SELECTION PAD corri OF fmenu DO corri
ON SELECTION PAD ingre OF fmenu DO ingre
ON SELECTION PAD termi OF fmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_f0
CLEAR
@ 1, 2 SAY '          Usuario :'
@ 3, 2 SAY '           Nombre :'
@ 6, 2 SAY '    Observaciones :'
RETURN
*
PROCEDURE vista
ON KEY LABEL F3 DO PerCla
ON KEY LABEL F9 DO Vista_Det
ACTIVATE WINDOW wind_f0
SELECT usua
SCATTER MEMVAR
@ 1, 22 SAY m.usuario
@ 3, 22 SAY m.nombre
@ 6, 22 SAY m.observ
DO vista_hijo
RETURN
*
PROCEDURE vista_hijo
SELECT iteu
vusucla = CHRTRAN(usua.usuario, 'ABCDEFGHIJKLMN?OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~??@?_#')
SEEK ALLTRIM(vusucla)
BROWSE FIELDS xsis = IIF( .NOT.  ;
       EMPTY(sistema),  ;
       IIF(SEEK(sistema,  ;
       'Maeacc'), maeacc.dessist,  ;
       ''), '') : 30 :H =  ;
       'Sistema', xmod = IIF(  ;
       .NOT. EMPTY(modulo),  ;
       IIF(SEEK(sistema + modulo,  ;
       'MaeAcc'), maeacc.desmod,  ;
       ''), '') : 30 :H =  ;
       'M?dulo', xopc = IIF(  ;
       .NOT. EMPTY(opcion),  ;
       IIF(SEEK(sistema + modulo +  ;
       opcion, 'MaeAcc'),  ;
       maeacc.desopc, ''),  ;
       opcion) : 10 :H = 'Opci?n'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_f2 KEY  ;
       ALLTRIM(vusucla) TIMEOUT  ;
       0.001 
RETURN
*
PROCEDURE vista_det
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteu
vusucla = CHRTRAN(usua.usuario, 'ABCDEFGHIJKLMN?OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~??@?_#')
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
            'M?dulo', xopc = IIF(  ;
            .NOT. EMPTY(opcion),  ;
            val_para(opcion, ;
            'OPCION','D'),  ;
            opcion) : 10 :H =  ;
            'Opci?n' NOMENU  ;
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
PROCEDURE revis
ON KEY LABEL F3
ON KEY LABEL F9
SELECT usua
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU fmenu
ACTIVATE SCREEN
vtempo = '旭旭旭旭旭?Presione ?F10? para seleccionar  o  ?Esc? para cancelar旭旭旭旭旭旭'
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
ON KEY LABEL F9
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
ON KEY LABEL F9
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
ON KEY LABEL F9
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
        'Proceso cancelado. No se graba ning?n cambio.'
     GOTO BOTTOM
ELSE
     SELECT usua
     IF SEEK(ALLTRIM(m.usuario))
          DO standby WITH  ;
             'El C?digo del Usuario ya est? registrado. Proceda a corregir datos.'
          DO vista
          DO corri
     ELSE
          @ 1, 22 SAY m.usuario
          @ 3, 22 GET m.nombre  ;
            PICTURE '@!'
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
   '旭旭F5->Agregar旭旭旭旭旭旭旭旭旭F8->Eliminar旭旭旭旭旭旭旭旭旭F10->Terminar旭旭'
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT maeacc
SET ORDER TO MaeAcc2
REPLACE acceso WITH '' ALL
SELECT iteu
vmens = ''
vusucla = CHRTRAN(m.usuario, 'ABCDEFGHIJKLMN?OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~??@?_#')
SEEK ALLTRIM(vusucla)
IF FOUND()
     SCAN WHILE user =  ;
          ALLTRIM(vusucla)
          SELECT maeacc
          SEEK iteu.codacc
          IF FOUND()
               REPLACE acceso  ;
                       WITH 'S'
          ELSE
               DO standby WITH  ;
                  'Error Opci?n no definida'
          ENDIF
          SELECT iteu
     ENDSCAN
ENDIF
SELECT maeacc
SET ORDER TO MaeAcc1
BROWSE FIELDS dessist : 25 :H =  ;
       'Sistema' :R, desmod : 25  ;
       :H = 'M?dulo' :R, desopc :  ;
       10 :H = 'Opci?n' :R,  ;
       acceso :H = 'Acceso' :P =  ;
       '@M S, ' NOMENU NOAPPEND  ;
       NODELETE WINDOW wind_f2
SELECT iteu
SET ORDER TO IteUsuOp2
DELETE FOR user =  ;
       ALLTRIM(vusucla)
SELECT maeacc
GOTO TOP
SCAN
     IF UPPER(maeacc.acceso) =  ;
        'S'
          SELECT iteu
          SEEK ALLTRIM(vusucla) +  ;
               maeacc.codacc
          IF  .NOT. FOUND()
               IF f_appd()
                    REPLACE user  ;
                            WITH  ;
                            ALLTRIM(vusucla),  ;
                            sistema  ;
                            WITH  ;
                            maeacc.sistema,  ;
                            modulo  ;
                            WITH  ;
                            maeacc.modulo,  ;
                            opcion  ;
                            WITH  ;
                            maeacc.opcion,  ;
                            codacc  ;
                            WITH  ;
                            maeacc.codacc
                    UNLOCK
               ENDIF
          ENDIF
     ENDIF
ENDSCAN
SELECT iteu
SET ORDER TO IteUsuOp1
SEEK ALLTRIM(vusucla)
FLUSH
ACTIVATE SCREEN
DO logos WITH rotulo1,  ;
   ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
SHOW MENU fmenu
ON KEY LABEL F10
RETURN vfun
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
PROCEDURE lista
ON KEY LABEL F3
ON KEY LABEL F9
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
   ' Relaci?n de Usuarios '
SELECT usua
GOTO vreg
DO vista
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
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
          cclave1 = CHRTRAN(cclave1, 'ABCDEFGHIJKLMN?OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~??@?_#')
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
     cclave3 = CHRTRAN(cclave3, 'ABCDEFGHIJKLMN?OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~??@?_#')
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
xpass = UPPER(xpass)
RETURN xpass
*

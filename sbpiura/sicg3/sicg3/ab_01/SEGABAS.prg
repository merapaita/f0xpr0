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
USE IN 1 Usuario ALIAS usua ORDER  ;
    Usuario1
USE IN 2 IteUsu ALIAS iteu ORDER  ;
    IteUsu1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
SAVE SCREEN TO pantalla
HIDE POPUP ALL
SELECT iteu
SET FILTER TO sistema = '2'
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
DEFINE PAD elimi OF fmenu PROMPT  ;
       '\<Elimina' AT 24, 54 SKIP  ;
       FOR v_us <> 'ADMIN'
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
DEFINE POPUP modulos FROM 1, 50  ;
       TO 22, 80 COLOR SCHEME 3
DEFINE BAR 1 OF modulos PROMPT  ;
       '\<A. Cuadro necesidades '
DEFINE BAR 2 OF modulos PROMPT  ;
       '\<B. Verificaci¢n C/N'
DEFINE BAR 3 OF modulos PROMPT  ;
       '\<C. Consolidaci¢n C/N'
DEFINE BAR 4 OF modulos PROMPT  ;
       '\<<. Consulta Saldos'
DEFINE BAR 5 OF modulos PROMPT  ;
       '\<\. Programa Pecosa'
DEFINE BAR 6 OF modulos PROMPT  ;
       '\<§. Programa Solicitud'
DEFINE BAR 7 OF modulos PROMPT  ;
       '\<D. Registro Pe.co.sa    '
DEFINE BAR 8 OF modulos PROMPT  ;
       '\<E. Solicitud Cotizaci¢n Compras'
DEFINE BAR 9 OF modulos PROMPT  ;
       '\<F. Recepci¢n Cotizacion Compras'
DEFINE BAR 10 OF modulos PROMPT  ;
       '\<G. Adjudicaci¢n Compras  '
DEFINE BAR 11 OF modulos PROMPT  ;
       '\<H. Orden de Compra       '
DEFINE BAR 12 OF modulos PROMPT  ;
       '\<I. Parte de Anulaci¢n  O/C  '
DEFINE BAR 13 OF modulos PROMPT  ;
       '\<J. Parte de ReBaja     O/C  '
DEFINE BAR 14 OF modulos PROMPT  ;
       '\<K. Consulta calendario O/C  '
DEFINE BAR 15 OF modulos PROMPT  ;
       '\<L. Consulta Marco Compras '
DEFINE BAR 16 OF modulos PROMPT  ;
       '\<M. Conciliaci¢n mensual O/C '
DEFINE BAR 17 OF modulos PROMPT  ;
       '\<N. Conciliaci¢n diaria  O/C '
DEFINE BAR 18 OF modulos PROMPT  ;
       '\<¥. Conciliaci¢n Bayovar O/C '
DEFINE BAR 19 OF modulos PROMPT  ;
       '\<O. Gu¡a Internamiento       '
DEFINE BAR 20 OF modulos PROMPT  ;
       '\<P. Despacho                 '
DEFINE BAR 21 OF modulos PROMPT  ;
       '\<Q. Nota de Entrada almac‚n  '
DEFINE BAR 22 OF modulos PROMPT  ;
       '\<R. Poliza de Entrada O/C '
DEFINE BAR 23 OF modulos PROMPT  ;
       '\<S. Poliza de Salida   '
DEFINE BAR 26 OF modulos PROMPT  ;
       '\<V. Solicitud de servicio     '
DEFINE BAR 27 OF modulos PROMPT  ;
       '\<W. Solicitud cotizaci¢n servicios'
DEFINE BAR 28 OF modulos PROMPT  ;
       '\<X. Recepci¢n Cotizaciones servicios '
DEFINE BAR 29 OF modulos PROMPT  ;
       '\<Y. Adjudicaci¢n Servicios         '
DEFINE BAR 30 OF modulos PROMPT  ;
       '\<Z. Orden de Servicio         '
DEFINE BAR 31 OF modulos PROMPT  ;
       '\<a. Parte de Anulaci¢n  O/S      '
DEFINE BAR 32 OF modulos PROMPT  ;
       '\<b. Parte Rebaja        O/S      '
DEFINE BAR 33 OF modulos PROMPT  ;
       '\<c. Consulta calendario O/S      '
DEFINE BAR 34 OF modulos PROMPT  ;
       '\<d. Consulta Marco Servicios'
DEFINE BAR 35 OF modulos PROMPT  ;
       '\<e. Conciliaci¢n mensual O/S  '
DEFINE BAR 37 OF modulos PROMPT  ;
       '\<g. Existencias Valoradas   '
DEFINE BAR 38 OF modulos PROMPT  ;
       '\<h. Control Visible Almacen '
DEFINE BAR 39 OF modulos PROMPT  ;
       '\<i. Stock                   '
DEFINE BAR 40 OF modulos PROMPT  ;
       '\<j. Stock Kardex             '
DEFINE BAR 41 OF modulos PROMPT  ;
       '\<k. Proveedores '
DEFINE BAR 42 OF modulos PROMPT  ;
       '\<l. Art¡culos   '
DEFINE BAR 43 OF modulos PROMPT  ;
       '\<m. Servicios   '
DEFINE BAR 44 OF modulos PROMPT  ;
       '\<n. Precios     '
DEFINE BAR 45 OF modulos PROMPT  ;
       '\<¤. Record      '
DEFINE BAR 46 OF modulos PROMPT  ;
       '\<o. Vehiculos   '
DEFINE BAR 47 OF modulos PROMPT  ;
       '\<p. Choferes    '
DEFINE BAR 48 OF modulos PROMPT  ;
       '\<q. Serv. Chof  '
DEFINE BAR 49 OF modulos PROMPT  ;
       '\<r. Vales       '
DEFINE BAR 50 OF modulos PROMPT  ;
       '\<s. Agrupa      '
DEFINE BAR 51 OF modulos PROMPT  ;
       '\<t. Grupos Gen‚ricos  '
DEFINE BAR 52 OF modulos PROMPT  ;
       '\<u. Dependencias      '
DEFINE BAR 53 OF modulos PROMPT  ;
       '\<v. Tipo de Servicios '
DEFINE BAR 54 OF modulos PROMPT  ;
       '\<w. Procedencia       '
DEFINE BAR 55 OF modulos PROMPT  ;
       '\<x. Destino           '
DEFINE BAR 56 OF modulos PROMPT  ;
       '\<y. Unidad medida     '
DEFINE BAR 57 OF modulos PROMPT  ;
       '\<f. Jefaturas  '
DEFINE BAR 58 OF modulos PROMPT  ;
       '\<z. Indexar archivos '
DEFINE BAR 59 OF modulos PROMPT  ;
       '\<1. Backup de los archivos'
DEFINE BAR 60 OF modulos PROMPT  ;
       '\<2. Depuraci¢n de backups '
DEFINE BAR 61 OF modulos PROMPT  ;
       '\<3. Correlativo           '
DEFINE BAR 62 OF modulos PROMPT  ;
       '\<4. Seguridad         '
DEFINE BAR 63 OF modulos PROMPT  ;
       '\<5. Usuarios ABAS     '
DEFINE BAR 64 OF modulos PROMPT  ;
       '\<6. Correlativo Proveedores'
DEFINE BAR 65 OF modulos PROMPT  ;
       '\<7. Correlativo O/C'
DEFINE BAR 66 OF modulos PROMPT  ;
       '\<8. Correlativo O/S'
DEFINE BAR 67 OF modulos PROMPT  ;
       '\<9. Correlativo Pe.co.sa '
DEFINE BAR 68 OF modulos PROMPT  ;
       '\<0. Correlativo S/S'
DEFINE BAR 69 OF modulos PROMPT  ;
       '\<!. Correlativo CC/C'
DEFINE BAR 70 OF modulos PROMPT  ;
       '\<«. Consulta:Pe.co.sa.          '  ;
       MESSAGE ''
DEFINE BAR 71 OF modulos PROMPT  ;
       '\<¬. Consulta:Orden de Compra    '  ;
       MESSAGE ''
DEFINE BAR 72 OF modulos PROMPT  ;
       '\<@. Consulta:Solicitud Servicio '  ;
       MESSAGE ''
DEFINE BAR 73 OF modulos PROMPT  ;
       '\<#. Consulta:Orden de Servicio  '  ;
       MESSAGE ''
DEFINE BAR 74 OF modulos PROMPT  ;
       '\<$. Consulta:Proveedores        '  ;
       MESSAGE ''
DEFINE BAR 75 OF modulos PROMPT  ;
       '\<%. Consulta:Art¡culos          '  ;
       MESSAGE ''
DEFINE BAR 76 OF modulos PROMPT  ;
       '\<^. Consulta:P¢l.Entrada        '  ;
       MESSAGE ''
DEFINE BAR 77 OF modulos PROMPT '\<&. Consulta:P¢l Salida         ';
MESSAGE ''
DEFINE BAR 78 OF modulos PROMPT  ;
       '\<*. Consulta:P/Anul O/C         '  ;
       MESSAGE ''
DEFINE BAR 79 OF modulos PROMPT  ;
       '\<(. Consulta:P/Anul O/S         '  ;
       MESSAGE ''
DEFINE BAR 80 OF modulos PROMPT  ;
       '\<). Consulta:P/Rebaja O/C       '  ;
       MESSAGE ''
DEFINE BAR 81 OF modulos PROMPT  ;
       '\<-. Consulta:P/Rebaja O/S       '  ;
       MESSAGE ''
DEFINE BAR 82 OF modulos PROMPT  ;
       '\<+. Consulta:Hojas de Control      '  ;
       MESSAGE ''
DEFINE BAR 83 OF modulos PROMPT  ;
       '\<=. Consulta:Comp.Pago             '  ;
       MESSAGE ''
DEFINE BAR 84 OF modulos PROMPT  ;
       '\<_. Consulta:Cheques               '  ;
       MESSAGE ''
DEFINE BAR 85 OF modulos PROMPT  ;
       '\</. Consulta:Hojas Modif.          '  ;
       MESSAGE ''
DEFINE BAR 86 OF modulos PROMPT  ;
       '\<›. Consulta:Seguimiento Doc.Fte.  '  ;
       MESSAGE ''
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
ON KEY LABEL F3 DO PerCla
ACTIVATE WINDOW wind_f0
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
BROWSE FIELDS modulo :H =  ;
       'M¢dulo', descri =  ;
       xdescri() :H =  ;
       '      Descripci¢n       '  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_f2 KEY  ;
       ALLTRIM(vusucla) TIMEOUT  ;
       0.001 
RETURN
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
BROWSE FIELDS modulo :H =  ;
       'M¢dulo' :P = 'N' :V =  ;
       modulos() :F :E = vmens,  ;
       descri = xdescri() :H =  ;
       '      Descripci¢n        '  ;
       NOMENU NOAPPEND NODELETE  ;
       WINDOW wind_f2 KEY  ;
       ALLTRIM(vusucla)
SELECT iteu
SEEK ALLTRIM(vusucla)
FLUSH
ACTIVATE SCREEN
DO logos WITH rotulo1,  ;
   ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
SHOW MENU fmenu
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
RETURN vfun
*
PROCEDURE agreg_item
SELECT iteu
IF f_appd()
     REPLACE usucla WITH vusucla
     REPLACE sistema WITH '2'
ENDIF
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
FUNCTION modulos
vmens = ''
vretorno = .F.
vmodulo = iteu.modulo
vlastkey = LASTKEY()
IF vlastkey = -7
     vretorno = .T.
ELSE
     IF ;
.NOT. vmodulo $ 'ABC<\§DEFGHIJKLMN¥OPQRSTUVWXYZabcdefghijklmn¤opqrstuvwxyz1234567890*/-!@#$%^&*()+=/«¬@›_';
.OR. EMPTY(vmodulo)
          ON SELECTION POPUP modulos DEACTIVATE;
POPUP Modulos
          ACTIVATE POPUP modulos
          IF  .NOT.  ;
              EMPTY(PROMPT())
               vmodulo = SUBSTR(PROMPT(),  ;
                         1, 1)
               vretorno = .T.
          ELSE
               vmens = 'Seleccione un m¢dulo'
               vmodulo = SPACE(1)
          ENDIF
     ELSE
          vretorno = .T.
     ENDIF
     vreg = RECNO()
     IF SEEK(vusucla + vmodulo)
          IF vreg <> RECNO()
               vretorno = .F.
               vmens = 'M¢dulo ya seleccionado'
               GOTO vreg
               IF  .NOT.  ;
                   EMPTY(iteu.modulo)
                    IF f_lock(1)
                         REPLACE modulo  ;
                                 WITH  ;
                                 SPACE(1)
                    ENDIF
               ENDIF
          ELSE
               IF EMPTY(iteu.modulo)
                    IF f_lock(1)
                         REPLACE modulo  ;
                                 WITH  ;
                                 vmodulo
                    ENDIF
               ENDIF
          ENDIF
     ELSE
          GOTO vreg
          IF EMPTY(iteu.modulo)
               IF f_lock(1)
                    REPLACE modulo  ;
                            WITH  ;
                            vmodulo
               ENDIF
          ENDIF
     ENDIF
ENDIF
RETURN vretorno
*
FUNCTION xdescri
DO CASE
     CASE iteu.modulo == 'A'
          RETURN ' Cuadro necesidades '
     CASE iteu.modulo == 'B'
          RETURN ' Verificaci¢n C/N'
     CASE iteu.modulo == 'C'
          RETURN ' Consolidaci¢n C/N'
     CASE iteu.modulo == '<'
          RETURN ' Consulta Saldos'
     CASE iteu.modulo == '\'
          RETURN ' Programa Pecosa'
     CASE iteu.modulo == '§'
          RETURN ' Programa Solicitud'
     CASE iteu.modulo == 'D'
          RETURN ' Registro Pe.co.sa    '
     CASE iteu.modulo == 'E'
          RETURN ' Solicitud Cotizaci¢n Compras'
     CASE iteu.modulo == 'F'
          RETURN ' Recepci¢n Cotizacion Compras'
     CASE iteu.modulo == 'G'
          RETURN ' Adjudicaci¢n Compras  '
     CASE iteu.modulo == 'H'
          RETURN ' Orden de Compra   '
     CASE iteu.modulo == 'I'
          RETURN ' Parte de Anulaci¢n  O/C'
     CASE iteu.modulo == 'J'
          RETURN ' Parte de ReBaja   O/C '
     CASE iteu.modulo == 'K'
          RETURN ' Consulta calendario O/C  '
     CASE iteu.modulo == 'L'
          RETURN ' Consulta Marco Compras  '
     CASE iteu.modulo == 'M'
          RETURN ' Conciliaci¢n mensual O/C '
     CASE iteu.modulo == 'N'
          RETURN ' Conciliaci¢n diaria O/C  '
     CASE iteu.modulo == '¥'
          RETURN ' Conciliaci¢n de Bayovar O/C  '
     CASE iteu.modulo == 'O'
          RETURN ' Gu¡a Internamiento       '
     CASE iteu.modulo == 'P'
          RETURN ' Despacho                 '
     CASE iteu.modulo == 'Q'
          RETURN ' Nota de Entrada almac‚n  '
     CASE iteu.modulo == 'R'
          RETURN ' Poliza de Entrada O/C '
     CASE iteu.modulo == '<'
          RETURN ' Poliza de Entrada NEA '
     CASE iteu.modulo == 'S'
          RETURN ' Poliza de Salida   '
     CASE iteu.modulo == 'T'
          RETURN ' Poliza de Entrada Bayovar '
     CASE iteu.modulo == 'U'
          RETURN ' Poliza de Salida Bayovar  '
     CASE iteu.modulo == 'V'
          RETURN ' Solicitud de servicio '
     CASE iteu.modulo == 'W'
          RETURN ' Solicitud cotizaci¢n servicios'
     CASE iteu.modulo == 'X'
          RETURN ' Recepci¢n Cotizaciones servicios '
     CASE iteu.modulo == 'Y'
          RETURN ' Adjudicaci¢n Servicios         '
     CASE iteu.modulo == 'Z'
          RETURN ' Orden de Servicio         '
     CASE iteu.modulo == 'a'
          RETURN ' Parte de Anulaci¢n   O/S '
     CASE iteu.modulo == 'b'
          RETURN ' Parte Rebaja         O/S  '
     CASE iteu.modulo == 'c'
          RETURN ' Consulta calendario   O/S    '
     CASE iteu.modulo == 'd'
          RETURN ' Consulta Marco Servicios '
     CASE iteu.modulo == 'e'
          RETURN ' Conciliaci¢n mensual O/S  '
     CASE iteu.modulo == 'g'
          RETURN ' Existencias Valoradas   '
     CASE iteu.modulo == 'h'
          RETURN ' Control Visible Almacen '
     CASE iteu.modulo == 'i'
          RETURN ' Stock                   '
     CASE iteu.modulo == 'j'
          RETURN ' Stock Kardex            '
     CASE iteu.modulo == 'k'
          RETURN ' Proveedores '
     CASE iteu.modulo == 'l'
          RETURN ' Art¡culos   '
     CASE iteu.modulo == 'm'
          RETURN ' Servicios   '
     CASE iteu.modulo == 'n'
          RETURN ' Precios     '
     CASE iteu.modulo == '¤'
          RETURN ' Record      '
     CASE iteu.modulo == 'o'
          RETURN ' Vehiculos   '
     CASE iteu.modulo == 'p'
          RETURN ' Choferes    '
     CASE iteu.modulo == 'q'
          RETURN ' Serv.Veh¡culos'
     CASE iteu.modulo == 'r'
          RETURN ' Vales       '
     CASE iteu.modulo == 's'
          RETURN ' Agrupa vales'
     CASE iteu.modulo == 't'
          RETURN ' Grupos Gen‚ricos  '
     CASE iteu.modulo == 'u'
          RETURN ' Dependencias      '
     CASE iteu.modulo == 'v'
          RETURN ' Tipo de Servicios '
     CASE iteu.modulo == 'w'
          RETURN ' Procedencia       '
     CASE iteu.modulo == 'x'
          RETURN ' Destino           '
     CASE iteu.modulo == 'y'
          RETURN ' Unidad medida     '
     CASE iteu.modulo == 'f'
          RETURN ' Jefaturas '
     CASE iteu.modulo == 'z'
          RETURN ' Indexar archivos '
     CASE iteu.modulo == '1'
          RETURN ' Backup de los archivos'
     CASE iteu.modulo == '2'
          RETURN ' Depuraci¢n de backups '
     CASE iteu.modulo == '3'
          RETURN ' Correlativos      '
     CASE iteu.modulo == '4'
          RETURN ' Seguridad         '
     CASE iteu.modulo == '5'
          RETURN ' Usuarios          '
     CASE iteu.modulo == '6'
          RETURN ' Correlativo Proveedores'
     CASE iteu.modulo == '7'
          RETURN ' Correlativo O/C'
     CASE iteu.modulo == '8'
          RETURN ' Correlativo O/S'
     CASE iteu.modulo == '9'
          RETURN ' Correlativo Pe.co.sa '
     CASE iteu.modulo == '0'
          RETURN ' Correlativo S/S'
     CASE iteu.modulo == '!'
          RETURN ' Correlativo CCC'
     CASE iteu.modulo == '«'
          RETURN ' Consulta: Pe.co.sa. '
     CASE iteu.modulo == '¬'
          RETURN ' Consulta: O/C      '
     CASE iteu.modulo == '@'
          RETURN ' Consulta: Solicitud Servicio'
     CASE iteu.modulo == '#'
          RETURN ' Consulta: O/S     '
     CASE iteu.modulo == '$'
          RETURN ' Consulta: Proveedores'
     CASE iteu.modulo == '%'
          RETURN ' Consulta: Art¡culos'
     CASE iteu.modulo == '^'
          RETURN ' Consulta Poliza Entrada'
     CASE iteu.modulo == '&'
          RETURN ' Consulta Poliza Salida'
     CASE iteu.modulo == '*'
          RETURN ' Consulta P/A O/C '
     CASE iteu.modulo == '('
          RETURN ' Consulta P/A O/S '
     CASE iteu.modulo == ')'
          RETURN ' Consulta P/R O/C '
     CASE iteu.modulo == '-'
          RETURN ' Consulta P/R O/S '
     CASE iteu.modulo == '+'
          RETURN ' Consulta H/C'
     CASE iteu.modulo == '='
          RETURN ' Consulta C/P'
     CASE iteu.modulo == '_'
          RETURN ' Consulta Cheque'
     CASE iteu.modulo == '/'
          RETURN ' Consulta H/Mod '
     CASE iteu.modulo == '›'
          RETURN ' Consulta Seguimiento Doc.Fte.'
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

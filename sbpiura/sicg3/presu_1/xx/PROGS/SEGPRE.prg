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
SET FILTER TO sistema = '1'
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
       TO 09, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_f1 FROM 00, 00  ;
       TO 23, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_f2 FROM 10, 00  ;
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
DEFINE POPUP modulos FROM 1, 50  ;
       TO 22, 78 COLOR SCHEME 3
DEFINE BAR 1 OF modulos PROMPT  ;
       '\<A. Presupuesto de Egresos'
DEFINE BAR 2 OF modulos PROMPT  ;
       '\<B. Presupuesto de Ingresos'
DEFINE BAR 3 OF modulos PROMPT  ;
       '\<C. Asigna Calendario'
DEFINE BAR 4 OF modulos PROMPT  ;
       '\<D. Trabaja con Calendario'
DEFINE BAR 5 OF modulos PROMPT  ;
       '\<E. Transferencia de Partidas '
DEFINE BAR 6 OF modulos PROMPT  ;
       '\<F. Credito Suplementario '
DEFINE BAR 7 OF modulos PROMPT  ;
       '\<G. E-5 '
DEFINE BAR 8 OF modulos PROMPT  ;
       '\<H. E-5 E '
DEFINE BAR 9 OF modulos PROMPT  ;
       '\<I. Asigna al Mes ===> 1 '
DEFINE BAR 10 OF modulos PROMPT  ;
       '\<J. Actualiza presupuesto '
DEFINE BAR 11 OF modulos PROMPT  ;
       '\<K. Catalogos Unidad Gestora '
DEFINE BAR 12 OF modulos PROMPT  ;
       '\<L. Catalogos Unidad Ejecutora'
DEFINE BAR 13 OF modulos PROMPT  ;
       '\<M. Catalogos Funcion'
DEFINE BAR 14 OF modulos PROMPT  ;
       '\<N. Catalogos Programas'
DEFINE BAR 15 OF modulos PROMPT  ;
       '\<O. Catalogos Subprograma'
DEFINE BAR 16 OF modulos PROMPT  ;
       '\<P. Catalogos Act/Proy'
DEFINE BAR 17 OF modulos PROMPT  ;
       '\<Q. Catalogos Componente'
DEFINE BAR 18 OF modulos PROMPT  ;
       '\<R. Cadena Funcional'
DEFINE BAR 19 OF modulos PROMPT  ;
       '\<S. Clasificador de Gasto'
DEFINE BAR 20 OF modulos PROMPT  ;
       '\<T. Clasificador de Ingreso'
DEFINE BAR 21 OF modulos PROMPT  ;
       '\<U. Fuente de Financiamiento'
DEFINE BAR 22 OF modulos PROMPT  ;
       '\<V. Clasificador'
DEFINE BAR 23 OF modulos PROMPT  ;
       '\<W. Indexar archivos '
DEFINE BAR 24 OF modulos PROMPT  ;
       '\<X. Backup de los archivos'
DEFINE BAR 25 OF modulos PROMPT  ;
       '\<Y. Depuraci¢n de backups '
DEFINE BAR 26 OF modulos PROMPT  ;
       '\<Z. Seguridad         '
DEFINE BAR 27 OF modulos PROMPT  ;
       '\<1. Usuarios          '
DEFINE BAR 28 OF modulos PROMPT  ;
       '\<2. Saldo Calendario mensual'
DEFINE BAR 29 OF modulos PROMPT  ;
       '\<3. Saldo Presupuestal '
DEFINE BAR 30 OF modulos PROMPT  ;
       '\<4. Marco Calendario  '
DEFINE BAR 31 OF modulos PROMPT  ;
       '\<5. Marco Ejecuci¢n   '
DEFINE BAR 32 OF modulos PROMPT  ;
       '\<6. Ejecuci¢n + Calendario'
DEFINE BAR 33 OF modulos PROMPT  ;
       '\<7. Listado E-5  '
DEFINE BAR 34 OF modulos PROMPT  ;
       '\<8. Listado E-5 (Proyecto) '
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
@ 7, 2 SAY '    Observaciones :'
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
  'CODDEP','D',22,50,7)
@ 5, 22 SAY m.numdep PICTURE '99'
@ 7, 22 SAY m.observ
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
     @ 7, 22 GET m.observ PICTURE  ;
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
          @ 7, 22 GET m.observ  ;
            PICTURE '@!'
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
vusucla = CHRTRAN(usua.usuario, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
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
ON KEY
RETURN vfun
*
PROCEDURE agreg_item
SELECT iteu
IF f_appd()
     REPLACE usucla WITH vusucla
     REPLACE sistema WITH '1'
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
     IF  .NOT. vmodulo $  ;
         'ABCDEFGHIJKLMN¥OPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890*/-'  ;
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
     CASE iteu.modulo = 'A'
          RETURN ' Presupuesto de Egresos '
     CASE iteu.modulo = 'B'
          RETURN ' Presupuesto de Ingresos '
     CASE iteu.modulo = 'C'
          RETURN ' Asigna Calendario'
     CASE iteu.modulo = 'D'
          RETURN ' Trabaja con Calendario '
     CASE iteu.modulo = 'E'
          RETURN ' Transferencia de Partida'
     CASE iteu.modulo = 'F'
          RETURN ' Cr‚ditos Suplementarios'
     CASE iteu.modulo = 'G'
          RETURN ' E-5'
     CASE iteu.modulo = 'H'
          RETURN ' E-5 E'
     CASE iteu.modulo = 'I'
          RETURN ' Asigna al Mes ===> 1 '
     CASE iteu.modulo = 'J'
          RETURN ' Actualiza Presupuesto '
     CASE iteu.modulo = 'K'
          RETURN ' Catalogos: Unidad Gestora '
     CASE iteu.modulo = 'L'
          RETURN ' catalogos: Unidad Ejecutora'
     CASE iteu.modulo = 'M'
          RETURN ' Catalogos: Funcion '
     CASE iteu.modulo = 'N'
          RETURN ' Catalogos: Programas'
     CASE iteu.modulo = 'O'
          RETURN ' Catalogos: Sub - Programas'
     CASE iteu.modulo = 'P'
          RETURN ' Catalogos: Act/Pry'
     CASE iteu.modulo = 'Q'
          RETURN ' Catalogos: Componente'
     CASE iteu.modulo = 'R'
          RETURN ' Cadena Funcional'
     CASE iteu.modulo = 'S'
          RETURN ' Clasificador de Gastos'
     CASE iteu.modulo = 'T'
          RETURN ' Calsificador de Ingresos'
     CASE iteu.modulo = 'U'
          RETURN ' Fuentes de Financiamiento'
     CASE iteu.modulo = 'V'
          RETURN ' Clasificador'
     CASE iteu.modulo = 'W'
          RETURN ' Indexar Archivos '
     CASE iteu.modulo = 'X'
          RETURN ' BackUp de Archivos '
     CASE iteu.modulo = 'Y'
          RETURN " Depuracion de BackUp's "
     CASE iteu.modulo = 'Z'
          RETURN ' Seguridad '
     CASE iteu.modulo = '1'
          RETURN ' Usuarios Presu '
     CASE iteu.modulo = '2'
          RETURN ' Saldo Calend. Mens.'
     CASE iteu.modulo = '3'
          RETURN ' Saldo Presupuestal '
     CASE iteu.modulo = '4'
          RETURN ' Marco Calendario   '
     CASE iteu.modulo = '5'
          RETURN ' Marco Ejecuci¢n    '
     CASE iteu.modulo = '6'
          RETURN ' Ejecuci¢n + Calendario '
     CASE iteu.modulo = '7'
          RETURN ' Listado E-5'
     CASE iteu.modulo = '8'
          RETURN ' Listado E-5 (Proyectos)'
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
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~')
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
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~')
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
xclave = CHRTRAN(m.clave, 'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~',;
'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789')
@ vlineas, 0 SAY '   Oficina: ' +  ;
  val_para(m.coddep,'CODDEP','D', ;
  22,60)
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

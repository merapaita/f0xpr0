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
USE IN 2 IteUsu ALIAS iteu ORDER  ;
    IteUsu1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
HIDE POPUP ALL
SELECT iteu
SET FILTER TO sistema = '4'
SELECT usua
IF v_us <> 'ADMIN'
     SET FILTER TO usuario = v_us
ENDIF
SET EXACT OFF
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
RELEASE MENU mmenu
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
       '\<Corrige' AT 24, 36 SKIP  ;
       FOR v_us <> 'ADMIN'
DEFINE PAD ingre OF mmenu PROMPT  ;
       '\<Ingresa' AT 24, 45 SKIP  ;
       FOR v_us <> 'ADMIN'
DEFINE PAD elimi OF mmenu PROMPT  ;
       '\<Elimina' AT 24, 54
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
ON SELECTION PAD elimi OF mmenu DO elimi
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
DEFINE POPUP modulos FROM 1, 50  ;
       TO 22, 78 COLOR SCHEME 3
DEFINE BAR 1 OF modulos PROMPT  ;
       '\<A. Registro H/Control'
DEFINE BAR 2 OF modulos PROMPT  ;
       '\<B. Registro H/Modificaci?n'
DEFINE BAR 3 OF modulos PROMPT  ;
       '\<C. Orden de Compra'
DEFINE BAR 4 OF modulos PROMPT  ;
       '\<D. Orden de Servicio'
DEFINE BAR 5 OF modulos PROMPT  ;
       '\<E. Comprobantes de Pago'
DEFINE BAR 6 OF modulos PROMPT  ;
       '\<F. Relacion de H/C '
DEFINE BAR 7 OF modulos PROMPT  ;
       '\<G. Saldo Presupuestal'
DEFINE BAR 8 OF modulos PROMPT  ;
       '\<H. Saldo Calendario '
DEFINE BAR 9 OF modulos PROMPT  ;
       '\<I. Calendario Vs. Ejecuci?n '
DEFINE BAR 10 OF modulos PROMPT  ;
       '\<J. Estado Ejecuci?n Mensual E-5'
DEFINE BAR 11 OF modulos PROMPT  ;
       '\<K. Listado E-5 (Inversi?n)'
DEFINE BAR 12 OF modulos PROMPT  ;
       '\<L. Tablas Tipo de Documento'
DEFINE BAR 13 OF modulos PROMPT  ;
       '\<M. Tablas Auxiliares'
DEFINE BAR 14 OF modulos PROMPT  ;
       '\<N. Tablas Correlativo H/C'
DEFINE BAR 15 OF modulos PROMPT  ;
       '\<O. Indexar Archivos'
DEFINE BAR 16 OF modulos PROMPT  ;
       "\<P. Backup's de Archivos"
DEFINE BAR 17 OF modulos PROMPT  ;
       "\<Q. Depuraci?n de Backup's"
DEFINE BAR 18 OF modulos PROMPT  ;
       '\<R. Seguridad'
DEFINE BAR 19 OF modulos PROMPT  ;
       '\<S. Usuarios cPresu'
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
       '\<Y. Depuraci?n de backups '
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
       '\<5. Marco Ejecuci?n   '
DEFINE BAR 32 OF modulos PROMPT  ;
       '\<6. Ejecuci?n + Calendario'
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
RETURN xpass
*
PROCEDURE vista_hijo
SELECT iteu
vusucla = CHRTRAN(usua.usuario, 'ABCDEFGHIJKLMN?OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~')
SEEK ALLTRIM(vusucla)
BROWSE FIELDS modulo :H =  ;
       'M?dulo', descri =  ;
       xdescri() : 40 :H =  ;
       '      Descripci?n       '  ;
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
HIDE MENU mmenu
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
SHOW MENU mmenu
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
          @ 4, 22 GET m.coddep  ;
            PICTURE '!!!!!!'  ;
            VALID  ;
            val_para(m.coddep, ;
            'CODDEP',' ',22,40, ;
            8)
          @ 6, 22 GET m.observ  ;
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
HIDE MENU mmenu
ACTIVATE SCREEN
DO logos WITH rotulo1,  ;
   '旭旭F5->Agregar旭旭旭旭旭旭旭旭旭F8->Eliminar旭旭旭旭旭旭旭旭旭F10->Terminar旭旭'
ON KEY LABEL F5 DO Agreg_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteu
vmens = ''
vusucla = CHRTRAN(m.usuario, 'ABCDEFGHIJKLMN?OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~')
SEEK ALLTRIM(vusucla)
IF  .NOT. FOUND()
     DO agreg_item
ENDIF
BROWSE FIELDS modulo :H =  ;
       'M?dulo' :P = 'N' :V =  ;
       modulos() :F :E = vmens,  ;
       descri = xdescri() : 40 :H =  ;
       '      Descripci?n        '  ;
       NOMENU NOAPPEND NODELETE  ;
       WINDOW wind_f2 KEY  ;
       ALLTRIM(vusucla)
SELECT iteu
SEEK ALLTRIM(vusucla)
FLUSH
ACTIVATE SCREEN
DO logos WITH rotulo1,  ;
   ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
SHOW MENU mmenu
ON KEY
RETURN vfun
*
PROCEDURE agreg_item
SELECT iteu
IF f_appd()
     REPLACE usucla WITH vusucla
     REPLACE sistema WITH '4'
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
PROCEDURE elimi
ON KEY LABEL F3
STORE yesno(vmens07) TO velimina
IF  .NOT. velimina
     RETURN
ENDIF
IF f_lock(1)
     vusucla = CHRTRAN(usua.usuario, 'ABCDEFGHIJKLMN?OPQRSTUVWXYZ0123456789',;
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
   ' Relaci?n de Usuarios '
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
     vusucla = CHRTRAN(usua.usuario, 'ABCDEFGHIJKLMN?OPQRSTUVWXYZ0123456789',;
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
FUNCTION modulos
vmens = ''
vretorno = .F.
vmodulo = iteu.modulo
vlastkey = LASTKEY()
IF vlastkey = -7
     vretorno = .T.
ELSE
     IF  .NOT. vmodulo $  ;
         'ABCDEFGHIJKLMN?OPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890*/-~'  ;
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
               vmens = 'Seleccione un m?dulo'
               vmodulo = SPACE(1)
          ENDIF
     ELSE
          vretorno = .T.
     ENDIF
     vreg = RECNO()
     IF SEEK(vusucla + vmodulo)
          IF vreg <> RECNO()
               vretorno = .F.
               vmens = 'M?dulo ya seleccionado'
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
          RETURN ' Registro H/Control '
     CASE iteu.modulo = 'B'
          RETURN ' Registro H/Modificaci?n'
     CASE iteu.modulo = 'C'
          RETURN ' Orden de Compra'
     CASE iteu.modulo = 'D'
          RETURN ' Orden de Servicio '
     CASE iteu.modulo = 'E'
          RETURN ' Comprobantes de Pago'
     CASE iteu.modulo = 'F'
          RETURN ' Relacion de H/C'
     CASE iteu.modulo = 'G'
          RETURN ' Saldo Presupuestal'
     CASE iteu.modulo = 'H'
          RETURN ' Saldo Calendario'
     CASE iteu.modulo = 'I'
          RETURN ' Calendario Vs. Ejecuci?n '
     CASE iteu.modulo = 'J'
          RETURN ' Estado Ejecuci?n Mensual E-5 '
     CASE iteu.modulo = 'K'
          RETURN ' Listado E-5 (Inversiones) '
     CASE iteu.modulo = 'L'
          RETURN ' Tablas: Tipo de Documentos'
     CASE iteu.modulo = 'M'
          RETURN ' Tablas: Auxiliares '
     CASE iteu.modulo = 'N'
          RETURN ' Tablas: Correlativo H/C '
     CASE iteu.modulo = 'O'
          RETURN ' Indexar Archivos'
     CASE iteu.modulo = 'P'
          RETURN " BackUp's de Archivos"
     CASE iteu.modulo = 'Q'
          RETURN " Depuraci?n de backup's"
     CASE iteu.modulo = 'R'
          RETURN ' Seguridad'
     CASE iteu.modulo = 'S'
          RETURN ' Usuarios CPresu'
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
          RETURN ' Marco Ejecuci?n    '
     CASE iteu.modulo = '6'
          RETURN ' Ejecuci?n + Calendario '
     CASE iteu.modulo = '7'
          RETURN ' Listado E-5'
     CASE iteu.modulo = '8'
          RETURN ' Listado E-5 (Proyectos)'
ENDCASE
RETURN ''
*
FUNCTION xxdescri
vali = ALIAS()
vord = ORDER()
PRIVATE vdes, vmod
vdes = SPACE(40)
vmod = ALLTRIM(iteu.modulo)
vdes = val_para(vmod,'MODCPR', ;
       'D')
SELECT (vali)
SET ORDER TO (vord)
RETURN vdes
*
PROCEDURE imptit
PARAMETER vlineas, cabecera
IF vlineas = 0
     vtitulo = 'Relaci?n de Usuarios'
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
'ABCDEFGHIJKLMN?OPQRSTUVWXYZ0123456789')
@ vlineas, 0 SAY '     Clave: ' +  ;
  xclave
vlineas = vlineas + 1
IF cabecera
     @ vlineas, 00 SAY  ;
       REPLICATE('-', 80)
     vlineas = vlineas + 1
     @ vlineas, 00 SAY 'M?dulo'
     @ vlineas, 10 SAY  ;
       '    Descripci?n     '
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

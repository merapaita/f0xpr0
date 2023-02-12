USE IN 1 parmae ALIAS parma ORDER  ;
    parmae1
USE IN 2 promae ALIAS promae  ;
    ORDER promae1
USE IN 3 prvaux ALIAS aux ORDER  ;
    prvaux1
USE IN 4 artmae ALIAS produ ORDER  ;
    artmae1
USE IN 5 auxil ALIAS auxil ORDER  ;
    auxil1
USE IN 6 ordcom ALIAS orden ORDER  ;
    ordcom4
USE IN 7 ordser ALIAS ordser  ;
    ORDER ordser6
SELECT promae
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
DO termina
RETURN
*
PROCEDURE inicia
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente                                     Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_cte0 FROM 00,  ;
       00 TO 23, 79 TITLE  ;
       ' CATALOGO DE PROVEEDORES '  ;
       FOOTER  ;
       '± ®F2¯ Muestra Clasificaci¢n ± ®F11¯ O/C ± ®F12¯ O/S ±'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_cte1 FROM 00,  ;
       00 TO 23, 79 TITLE  ;
       ' Revisi¢n de Proveedores '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2p FROM 12, 10  ;
       TO 22, 69 TITLE  ;
       ' Detalle : Clasificaci¢n '  ;
       FOOTER '± ®Esc¯ Sale ±'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2pa FROM 14,  ;
       04 TO 22, 75 TITLE  ;
       ' Clasificaci¢n del Proveedor '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2pb FROM 07,  ;
       02 TO 18, 78 TITLE  ;
       ' Detalle : Ordenes Compra '  ;
       FOOTER '± ®Esc¯ Sale ±'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2pc FROM 07,  ;
       02 TO 18, 78 TITLE  ;
       ' Detalle : Ordenes Servicio '  ;
       FOOTER '± ®Esc¯ Sale ±'  ;
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
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_cte0
@ 1, 2 SAY '          C¢digo : '
@ 1, 23 SAY '-'
@ 1, 40 SAY '   Fecha Ingreso : '
@ 3, 2 SAY '          Nombre : '
@ 4, 2 SAY '       Direcci¢n : '
@ 5, 2 SAY '        Tel‚fono : '
@ 5, 40 SAY '             Fax : '
@ 6, 2 SAY '           R.U.C.: '
@ 8, 2 SAY '   Lic.Municipal : '
@ 8, 40 SAY '   Reg.Comercial : '
@ 9, 2 SAY '   Reg.Unificado : '
@ 9, 40 SAY '  Reg.Industrial : '
@ 10, 2 SAY '      Testimonio : '
@ 10, 40 SAY  ;
  '      C.Apertura : '
@ 12, 2 SAY '  Nombre R.Legal : '
@ 13, 2 SAY '  Direcc.R.Legal : '
@ 14, 2 SAY '     RUC R.Legal : '
@ 15, 2 SAY '    L.E. R.Legal : '
@ 16, 2 SAY '  Telef. R.Legal : '
@ 18, 2 SAY '  Tipo Operaci¢n : '
@ 19, 2 SAY '   Act.Econ¢mica : '
@ 20, 2 SAY '          Ambito : '
@ 21, 2 SAY '          M‚rito : '
@ 21, 40 SAY  ;
  'Doc. Legalizados : '
RETURN
*
PROCEDURE vista
ACTIVATE WINDOW wind_cte0
ON KEY LABEL F12 DO SERVI
ON KEY LABEL F11 DO ORDEN
SELECT promae
IF EOF()
     DO pantalla
     RETURN
ENDIF
ON KEY LABEL F2 DO VISTA_PRO
SCATTER MEMVAR
@ 0, 60 SAY verest(m.estado)
@ 1, 22 SAY m.tippro
@ 1, 24 SAY m.codprv
@ 1, 60 SAY m.fecing
@ 3, 22 SAY m.nompro
@ 4, 22 SAY m.dirpro
@ 5, 22 SAY m.telpro
@ 5, 60 SAY m.faxpro
@ 6, 22 SAY m.numruc
@ 8, 22 SAY m.licmun
@ 8, 60 SAY m.regcom
@ 9, 22 SAY m.regunf
@ 9, 60 SAY m.regind
@ 10, 22 SAY m.test
@ 10, 60 SAY m.apert
@ 12, 22 SAY m.rl_nom
@ 13, 22 SAY m.rl_dir
@ 14, 22 SAY m.rl_ruc
@ 15, 22 SAY m.rl_le
@ 16, 22 SAY m.rl_tel
@ 18, 22 SAY val_para(m.tipope, ;
  'TIPOPE','V')
@ 19, 22 SAY val_para(m.acteco, ;
  'ACTECO','V')
@ 20, 22 SAY val_para(m.ambito, ;
  'AMBITO','V')
@ 21, 22 SAY m.clasif
@ 21, 60 SAY m.legali
RETURN
*
PROCEDURE revis
ON KEY LABEL F2
ON KEY LABEL F3 DO BUSCA_P
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar'
     RETURN
ENDIF
vtemp = RECNO()
IF escolor
     DEFINE POPUP Busmenu FROM 15,50;
 SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP busmenu FROM 15,  ;
            50 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF busmenu PROMPT  ;
       ' ordenado por:  \<C¢digo  '
DEFINE BAR 2 OF busmenu PROMPT  ;
       ' ordenado por:  \<Nombre  '
DEFINE BAR 3 OF busmenu PROMPT  ;
       ' ordenado por:  \<R.U.C   '
DEFINE BAR 4 OF busmenu PROMPT  ;
       ' ordenado por:  representante \<Legal '
ON SELECTION POPUP busmenu DEACTIVATE;
POPUP
ACTIVATE POPUP busmenu
DO CASE
     CASE BAR() = 1
          SET ORDER TO 1
     CASE BAR() = 2
          SET ORDER TO 2
     CASE BAR() = 3
          SET ORDER TO 3
     CASE BAR() = 4
          SET ORDER TO 4
ENDCASE
IF LASTKEY() = 27
     SET ORDER TO 1
     DO vista
     RETURN
ENDIF
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS tippro :H = 'T',  ;
       codprv :H = 'C¢digo',  ;
       nompro :H = 'Nombre',  ;
       dirpro :H = 'Direcci¢n',  ;
       numruc :H = 'RUC', rl_nom  ;
       :H = 'Representante Legal'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW wind_cte1
ON KEY LABEL F10
SET ORDER TO 1
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F3
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
IF escolor
     DEFINE POPUP Busmenu FROM 15,54;
 SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP busmenu FROM 15,  ;
            54 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF busmenu PROMPT  ;
       ' por \<C¢digo  '
DEFINE BAR 2 OF busmenu PROMPT  ;
       ' por \<Nombre  '
DEFINE BAR 3 OF busmenu PROMPT  ;
       ' por \<R.U.C   '
DEFINE BAR 4 OF busmenu PROMPT  ;
       ' por repres. \<Leg '
ON SELECTION POPUP busmenu DEACTIVATE;
POPUP
ACTIVATE POPUP busmenu
SELECT promae
vtemp = RECNO()
DO CASE
     CASE BAR() = 1
          ACTIVATE WINDOW standby
          STORE '0000' TO vbusca
          @ 1, 2 SAY 'C¢digo: '  ;
            GET vbusca PICTURE  ;
            '!!!!'
          READ
          DEACTIVATE WINDOW  ;
                     standby
          IF LASTKEY() <> 27
               vbusca = UPPER(ALLTRIM(vbusca))
          ENDIF
     CASE BAR() = 2
          ACTIVATE WINDOW standby
          STORE SPACE(40) TO  ;
                vbusca
          @ 1, 1 SAY ' Nombre: '  ;
            GET vbusca PICTURE  ;
            '@S30'
          READ
          DEACTIVATE WINDOW  ;
                     standby
          IF LASTKEY() <> 27
               vbusca = TRIM(UPPER(vbusca))
               SET ORDER TO 2
          ENDIF
     CASE BAR() = 3
          ACTIVATE WINDOW standby
          STORE SPACE(8) TO  ;
                vbusca
          @ 1, 1 SAY ' N§ RUC: '  ;
            GET vbusca PICTURE  ;
            '!!!!!!!!'
          READ
          DEACTIVATE WINDOW  ;
                     standby
          IF LASTKEY() <> 27
               vbusca = TRIM(UPPER(vbusca))
               SET ORDER TO 3
          ENDIF
     CASE BAR() = 4
          ACTIVATE WINDOW standby
          STORE SPACE(40) TO  ;
                vbusca
          @ 1, 1 SAY  ;
            ' Nombre Rep.Legal: '  ;
            GET vbusca PICTURE  ;
            '@S20'
          READ
          DEACTIVATE WINDOW  ;
                     standby
          IF LASTKEY() <> 27
               vbusca = TRIM(UPPER(vbusca))
               SET ORDER TO 4
          ENDIF
     OTHERWISE
          RETURN
ENDCASE
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
     GOTO vtemp
ELSE
     SEEK vbusca
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Dicho Proveedor no fue encontrado'
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
SET ORDER TO 1
DO vista
RETURN
*
PROCEDURE busca_p
as = ORDER()
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
IF escolor
     DEFINE POPUP Busmenu FROM 17,54;
 SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP busmenu FROM 17,  ;
            54 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF busmenu PROMPT  ;
       '  \<C¢digo  '
DEFINE BAR 2 OF busmenu PROMPT  ;
       '  \<Nombre  '
DEFINE BAR 3 OF busmenu PROMPT  ;
       '  \<R.U.C   '
ON SELECTION POPUP busmenu DEACTIVATE;
POPUP
ACTIVATE POPUP busmenu
SELECT promae
vtemp = RECNO()
DO CASE
     CASE BAR() = 1
          SET ORDER TO 1
          ACTIVATE WINDOW standby
          STORE '0000' TO vbusca
          @ 1, 2 SAY 'C¢digo: '  ;
            GET vbusca PICTURE  ;
            '!!!!'
          READ
          DEACTIVATE WINDOW  ;
                     standby
          IF LASTKEY() <> 27
               vbusca = UPPER(ALLTRIM(vbusca))
          ENDIF
     CASE BAR() = 2
          ACTIVATE WINDOW standby
          STORE SPACE(40) TO  ;
                vbusca
          @ 1, 1 SAY ' Nombre: '  ;
            GET vbusca PICTURE  ;
            '@S30'
          READ
          DEACTIVATE WINDOW  ;
                     standby
          IF LASTKEY() <> 27
               vbusca = TRIM(UPPER(vbusca))
               SET ORDER TO 2
          ENDIF
     CASE BAR() = 3
          ACTIVATE WINDOW standby
          STORE SPACE(8) TO  ;
                vbusca
          @ 1, 1 SAY ' N§ RUC: '  ;
            GET vbusca PICTURE  ;
            '!!!!!!!!'
          READ
          DEACTIVATE WINDOW  ;
                     standby
          IF LASTKEY() <> 27
               vbusca = TRIM(UPPER(vbusca))
               SET ORDER TO 3
          ENDIF
     OTHERWISE
          RETURN
ENDCASE
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
     GOTO vtemp
ELSE
     SEEK vbusca
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Dicho Proveedor no fue encontrado'
          GOTO vtemp
     ENDIF
ENDIF
SET ORDER TO AS
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
RETURN
*
PROCEDURE anter
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
SELECT promae
IF  .NOT. BOF()
     SKIP -1
ENDIF
IF BOF()
     GOTO TOP
     DO standby WITH  ;
        'Inicio de archivo: no existe Contratante anterior'
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE proxi
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
SELECT promae
IF  .NOT. EOF()
     SKIP
ENDIF
IF EOF()
     DO standby WITH  ;
        'Fin de archivo: no existe Contratante siguiente'
     GOTO BOTTOM
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE orden
PRIVATE ali, vkey
ali = ALIAS()
vkey = '99' + promae.codprv
SELECT orden
SEEK vkey
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No registra Ordenes de Compra'
ELSE
     BROWSE NOOPTIMIZE FIELDS  ;
            numoc :H = ' O/C',  ;
            codfte :H = 'Fte',  ;
            fecoc :H = 'FecOc',  ;
            fecdesp :H = 'Liquid',  ;
            valtot :H = 'Total ',  ;
            user :H = 'Ingreso',  ;
            user_fc :H = 'FecIng'  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW  ;
            wind_2pb KEY vkey  ;
            NOREFRESH
ENDIF
SELECT (ali)
DO vista
RETURN
*
PROCEDURE servi
PRIVATE ali, vkey
ali = ALIAS()
vkey = '99' + promae.codprv
SELECT ordser
SEEK vkey
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No registra Ordenes de Servicios'
ELSE
     BROWSE NOOPTIMIZE FIELDS  ;
            numos :H = ' O/S',  ;
            codfte :H = 'Fte',  ;
            fecos :H = 'FecOs',  ;
            fecliq :H = 'Liquid',  ;
            valtot :H = 'Total ',  ;
            user :H = 'Ingreso',  ;
            user_fc :H = 'FecIng'  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW  ;
            wind_2pc KEY vkey  ;
            NOREFRESH
ENDIF
SELECT (ali)
DO vista
RETURN
*
FUNCTION numruc
PRIVATE vfun
ax = RECNO()
vfun = .T.
SET ORDER TO 3
SEEK m.numruc
IF ax <> RECNO()
     IF FOUND() .OR.  ;
        EMPTY(m.numruc) .OR.  ;
        LEN(ALLTRIM(m.numruc)) <  ;
        8
          DO standby WITH  ;
             'El proveedor ya est  registrado,Tiene el mismo RUC,o est  en Blanco/Nulo'
          vfun = .F.
     ENDIF
ENDIF
SET ORDER TO 1
RETURN vfun
*
PROCEDURE trabaja_pr
as = ALIAS()
ACTIVATE SCREEN
HIDE MENU mmenu
ACTIVATE WINDOW wind_2pa
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO Agreg_pro
ON KEY LABEL F8 DO Elimi_pro
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT aux
SET ORDER TO PRVAUX1
SEEK m.codprv
IF  .NOT. FOUND()
     DO agreg_pro
ENDIF
BROWSE FIELDS codcla :H = 'Clase'  ;
       :V = valart(codcla) :F :W =  ;
       EMPTY(codcla), xdescri =  ;
       val_fun('Produ','CodArt', ;
       'Descri',codcla) :H =  ;
       'Descripci¢n' : 60 :W =  ;
       .F. NOMENU NOAPPEND  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_2pa KEY m.codprv
DEACTIVATE WINDOW wind_2pa
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SCAN WHILE m.codprv = codprv
     IF RLOCK()
          IF EMPTY(codcla)
               DELETE NEXT 1
          ENDIF
     ENDIF
ENDSCAN
UNLOCK ALL
SHOW MENU mmenu
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SELECT (as)
RETURN
*
PROCEDURE vista_pro
SELECT aux
SET ORDER TO 1
SEEK m.codprv
IF  .NOT. FOUND()
     DO standby WITH  ;
        'Este proveedor no tiene Clasificaci¢n'
ELSE
     ACTIVATE WINDOW wind_2p
     HIDE MENU mmenu
     BROWSE FIELDS codcla :H =  ;
            'Clase', xdescri =  ;
            val_fun('Produ', ;
            'CodArt','Descri', ;
            codcla) :H =  ;
            'Descripci¢n' : 44 :W =  ;
            .F., xtipprv =  ;
            IIF(LEFT(codcla, 1) =  ;
            'B', 'Bien', 'Serv')  ;
            :H = 'Tipo' :W = .F.  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR KEY  ;
            m.codprv
ENDIF
DEACTIVATE WINDOW wind_2p
SHOW MENU mmenu
SELECT promae
RETURN
*
PROCEDURE elimi
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar'
     RETURN
ENDIF
IF estado = 'AN'
     DO standby WITH  ;
        'El proveedor ya est  anulado'
ENDIF
IF escolor
     DEFINE POPUP Elimenu FROM 17,54;
 SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP elimenu FROM 17,  ;
            54 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF elimenu PROMPT  ;
       ' \<Anulaci¢n  '
DEFINE BAR 2 OF elimenu PROMPT  ;
       ' \<Rehabilita '
ON SELECTION POPUP elimenu DEACTIVATE;
POPUP
ACTIVATE POPUP elimenu
DO CASE
     CASE BAR() = 1
          IF RLOCK()
               REPLACE estado  ;
                       WITH 'AN',  ;
                       fecanu  ;
                       WITH  ;
                       DATE()
          ENDIF
     CASE BAR() = 2
          IF estado <> 'AN'
               DO standby WITH  ;
                  'El proveedor NO est  anulado'
          ELSE
               IF RLOCK()
                    REPLACE estado  ;
                            WITH  ;
                            'PD',  ;
                            fecanu  ;
                            WITH  ;
                            CTOD( ;
                            '  -   -  ' ;
                            )
               ENDIF
          ENDIF
ENDCASE
RELEASE POPUP elimenu
UNLOCK
DO vista
RETURN
*
FUNCTION agreg_pro
IF f_appd()
     REPLACE codprv WITH m.codprv
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION agreg_aux
SELECT auxil
IF f_appd()
     REPLACE codigo WITH m.codprv,  ;
             tipo WITH '20',  ;
             descri WITH m.nompro,  ;
             direccion WITH  ;
             m.dirpro
     RETURN .T.
ENDIF
RETURN .F.
*
PROCEDURE elimi_pro
SELECT aux
IF RLOCK()
     DELETE NEXT 1
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE termina
CLOSE DATABASES
ON KEY LABEL F2
RELEASE WINDOW wind_cte0
RELEASE WINDOW wind_cte1
ON KEY LABEL F12
ON KEY LABEL F11
RELEASE MENU mmenu
ACTIVATE SCREEN
RESTORE SCREEN FROM principal
RETURN
*
PROCEDURE lista
SELECT promae
IF EOF()
     RETURN
ENDIF
vtemp = RECNO()
GOTO TOP
DO lispro
SELECT promae
GOTO vtemp
ON KEY LABEL F2 DO VISTA_PRO
RETURN
*
FUNCTION valdir
m.dir_cob = m.dir_cte
RETURN .T.
*
FUNCTION valprv
PRIVATE xx, vfun
vfun = .F.
vcodprv = IIF(EMPTY(vcodprv),  ;
          vcodprv,  ;
          PADL(ALLTRIM(vcodprv),  ;
          4, '0'))
zz = val_prv(vcodprv,.T.)
IF zz
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION xvalprv
PARAMETER vcli
PRIVATE xx, vfun
vfun = .F.
vcli = IIF(EMPTY(vcli), vcli,  ;
       PADL(ALLTRIM(vcli), 4,  ;
       '0'))
xx = val_prv(vcli,.T.,3,1)
IF  .NOT. EMPTY(xx)
     RETURN .T.
ENDIF
RETURN vfun
*
FUNCTION valpro
PARAMETER vcodprv
PRIVATE vfun
vfun = .T.
m.codprv = PADL(ALLTRIM(STR(vcodprv,  ;
           4)), 4, '0')
IF m.codprv = '0000' .OR.  ;
   EMPTY(m.codprv)
     vfun = .F.
ENDIF
RETURN vfun
*
FUNCTION valart
PARAMETER _cod
PRIVATE xx, yy, zz, vfun
vfun = .T.
as = ALIAS()
IF EMPTY(_cod)
     zz = val_para(_cod,'CODGE', ;
          'C')
     IF LASTKEY() = 27
          RETURN .T.
     ENDIF
     IF zz
          xx = val_art(codcla, ;
               .F.)
          IF xx
               SELECT aux
               REPLACE codcla  ;
                       WITH  ;
                       produ.codart
               vfun = .T.
          ENDIF
     ENDIF
ELSE
     vfun = .T.
ENDIF
SELECT (as)
UNLOCK ALL
ON KEY LABEL F2 DO VISTA_PRO
ON KEY LABEL F5 DO Agreg_pro
ON KEY LABEL F8 DO Elimi_pro
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN vfun
*
FUNCTION vallis
PARAMETER _cod
PRIVATE xx, yy, zz, vfun
vfun = .T.
as = ALIAS()
zz = val_para(_cod,'CODGEB','C')
IF zz
     xx = val_art(codcla,.T.)
     IF xx
          vfun = .T.
     ENDIF
ENDIF
SELECT (as)
RETURN vfun
*
PROCEDURE lispro1
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemo = RECNO()
DEFINE WINDOW lis FROM 2, 15 TO  ;
       23, 65 FLOAT TITLE  ;
       'Listado Proveedores'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro, vestado, vtiplis
vcodprv = SPACE(4)
@ 01, 01 SAY  ;
  '  Total Proveedor : ' GET  ;
  vtocli SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,3,22)
@ 03, 01 SAY  ;
  '        Proveedor : '
@ 03, 22 GET vcodprv PICTURE  ;
  '!!!!' VALID valprv() WHEN  ;
  vtocli = 2
@ 05, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Nombre;C¢digo;Ingreso;RUC'  ;
  WHEN vtocli = 1
@ 08, 01 SAY  ;
  '           Estado : ' GET  ;
  vestado FUNCTION  ;
  '^ Todos;Aptos;No aptos;Pendientes;Anulados'  ;
  WHEN vtocli = 1
@ 11, 01 SAY  ;
  '   Tipo Proveedor : ' GET  ;
  vtippro FUNCTION  ;
  '^ Todos;Bienes;Servicios' WHEN  ;
  vtocli = 1
@ 14, 01 SAY  ;
  '     Tipo Listado : ' GET  ;
  vtiplis FUNCTION  ;
  '^ General;Resumido;INAP;Control O/C;Control O/S'  ;
  WHEN vtocli = 1
@ 17, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     vind = SYS(3) + '.IDX'
     DO CASE
          CASE vtiplis = 1 .OR.  ;
               vtiplis = 3
               DO CASE
                    CASE vestado =  ;
                         1
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               IIF(vorden =  ;
                               3,  ;
                               DTOS(fecing),  ;
                               numruc)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               IIF(vtocli =  ;
                               1,  ;
                               .T.,  ;
                               codprv =  ;
                               vcodprv)  ;
                               .AND.  ;
                               IIF(vtippro =  ;
                               1,  ;
                               .T.,  ;
                               IIF(vtippro =  ;
                               2,  ;
                               tippro =  ;
                               'B',  ;
                               tippro =  ;
                               'S'))
                    CASE vestado =  ;
                         2
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               IIF(vorden =  ;
                               3,  ;
                               DTOS(fecing),  ;
                               numruc)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               IIF(vtocli =  ;
                               1,  ;
                               .T.,  ;
                               codprv =  ;
                               vcodprv)  ;
                               .AND.  ;
                               IIF(vtippro =  ;
                               1,  ;
                               .T.,  ;
                               IIF(vtippro =  ;
                               2,  ;
                               tippro =  ;
                               'B',  ;
                               tippro =  ;
                               'S'))  ;
                               .AND.  ;
                               estado =  ;
                               'VG'  ;
                               .AND.  ;
                               legali =  ;
                               'û'
                    CASE vestado =  ;
                         3
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               IIF(vorden =  ;
                               3,  ;
                               DTOS(fecing),  ;
                               numruc)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               IIF(vtocli =  ;
                               1,  ;
                               .T.,  ;
                               codprv =  ;
                               vcodprv)  ;
                               .AND.  ;
                               IIF(vtippro =  ;
                               1,  ;
                               .T.,  ;
                               IIF(vtippro =  ;
                               2,  ;
                               tippro =  ;
                               'B',  ;
                               tippro =  ;
                               'S'))  ;
                               .AND.  ;
                               estado =  ;
                               'RG'
                    CASE vestado =  ;
                         4
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               IIF(vorden =  ;
                               3,  ;
                               DTOS(fecing),  ;
                               numruc)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               estado =  ;
                               'PD'
                    CASE vestado =  ;
                         5
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               IIF(vorden =  ;
                               3,  ;
                               DTOS(fecing),  ;
                               numruc)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               estado =  ;
                               'AN'
               ENDCASE
          CASE vtiplis = 2
               DO CASE
                    CASE vestado =  ;
                         1
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               IIF(vorden =  ;
                               3,  ;
                               DTOS(fecing),  ;
                               numruc)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               IIF(vtocli =  ;
                               1,  ;
                               .T.,  ;
                               codprv =  ;
                               vcodprv)  ;
                               .AND.  ;
                               IIF(vtippro =  ;
                               1,  ;
                               .T.,  ;
                               IIF(vtippro =  ;
                               2,  ;
                               tippro =  ;
                               'B',  ;
                               tippro =  ;
                               'S'))
                    CASE vestado =  ;
                         2
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               IIF(vorden =  ;
                               3,  ;
                               DTOS(fecing),  ;
                               numruc)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               IIF(vtocli =  ;
                               1,  ;
                               .T.,  ;
                               codprv =  ;
                               vcodprv)  ;
                               .AND.  ;
                               IIF(vtippro =  ;
                               1,  ;
                               .T.,  ;
                               IIF(vtippro =  ;
                               2,  ;
                               tippro =  ;
                               'B',  ;
                               tippro =  ;
                               'S'))  ;
                               .AND.  ;
                               estado =  ;
                               'VG'  ;
                               .AND.  ;
                               legali =  ;
                               'û'
                    CASE vestado =  ;
                         3
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               IIF(vorden =  ;
                               3,  ;
                               DTOS(fecing),  ;
                               numruc)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               IIF(vtocli =  ;
                               1,  ;
                               .T.,  ;
                               codprv =  ;
                               vcodprv)  ;
                               .AND.  ;
                               IIF(vtippro =  ;
                               1,  ;
                               .T.,  ;
                               IIF(vtippro =  ;
                               2,  ;
                               tippro =  ;
                               'B',  ;
                               tippro =  ;
                               'S'))  ;
                               .AND.  ;
                               estado =  ;
                               'RG'
                    CASE vestado =  ;
                         4
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               IIF(vorden =  ;
                               3,  ;
                               DTOS(fecing),  ;
                               numruc)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               estado =  ;
                               'PD'
                    CASE vestado =  ;
                         5
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               IIF(vorden =  ;
                               3,  ;
                               DTOS(fecing),  ;
                               numruc)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               estado =  ;
                               'AN'
               ENDCASE
          CASE vtiplis = 4
               INDEX ON  ;
                     IIF(vorden =  ;
                     1,  ;
                     promae.nompro,  ;
                     IIF(vorden =  ;
                     2, codprv,  ;
                     IIF(vorden =  ;
                     3,  ;
                     DTOS(fecing),  ;
                     numruc))) TO  ;
                     (vind) FOR  ;
                     IIF(vtocli =  ;
                     1, .T.,  ;
                     codprv =  ;
                     vcodprv)  ;
                     .AND.  ;
                     IIF(vtippro =  ;
                     1, .T.,  ;
                     IIF(vtippro =  ;
                     2, tippro =  ;
                     'B', tippro =  ;
                     'S')) .AND.  ;
                     estado <>  ;
                     'VG' .AND.  ;
                     busprvoc()
          CASE vtiplis = 5
               INDEX ON  ;
                     IIF(vorden =  ;
                     1,  ;
                     promae.nompro,  ;
                     IIF(vorden =  ;
                     2, codprv,  ;
                     IIF(vorden =  ;
                     3,  ;
                     DTOS(fecing),  ;
                     numruc))) TO  ;
                     (vind) FOR  ;
                     IIF(vtocli =  ;
                     1, .T.,  ;
                     codprv =  ;
                     vcodprv)  ;
                     .AND.  ;
                     IIF(vtippro =  ;
                     1, .T.,  ;
                     IIF(vtippro =  ;
                     2, tippro =  ;
                     'B', tippro =  ;
                     'S')) .AND.  ;
                     estado <>  ;
                     'VG' .AND.  ;
                     busprvos()
     ENDCASE
     SET INDEX TO (vind)
     GOTO TOP
     DEACTIVATE WINDOW standby
     IF  .NOT. EOF()
          IF vtiplis = 1
               vtitul = IIF(vtippro =  ;
                        1,  ;
                        ' en General ',  ;
                        IIF(vtippro =  ;
                        2,  ;
                        ' por Bienes ',  ;
                        ' por Servicio ' ;
                        ))
               DO CASE
                    CASE vestado =  ;
                         1
                         vtitulo =  ;
                          ' Proveedores en General '
                    CASE vestado =  ;
                         2
                         vtitulo =  ;
                          ' Proveedores Aptos '
                    CASE vestado =  ;
                         3
                         vtitulo =  ;
                          ' Proveedores no Aptos '
                    CASE vestado =  ;
                         4
                         vtitulo =  ;
                          ' Proveedores Pendientes '
                    CASE vestado =  ;
                         5
                         vtitulo =  ;
                          ' Proveedores Anulados '
               ENDCASE
               DO reporte WITH 2,  ;
                  'LISPRO',  ;
                  ' Lista de Proveedores ',  ;
                  1, .F., .T.
          ELSE
               IF vtiplis = 2
                    DO CASE
                         CASE vestado =  ;
                              1
                              vtitulo =  ;
                               ' Proveedores en General '
                         CASE vestado =  ;
                              2
                              vtitulo =  ;
                               ' Proveedores Aptos '
                         CASE vestado =  ;
                              3
                              vtitulo =  ;
                               ' Proveedores no Aptos '
                         CASE vestado =  ;
                              4
                              vtitulo =  ;
                               ' Proveedores Pendientes '
                         CASE vestado =  ;
                              5
                              vtitulo =  ;
                               ' Proveedores Anulados '
                    ENDCASE
                    DO reporte  ;
                       WITH 2,  ;
                       'LISPRO2',  ;
                       ' Lista de Proveedores ',  ;
                       1, .F.,  ;
                       .T.
               ELSE
                    IF vtiplis =  ;
                       3
                         vtitulo =  ;
                          IIF(vtippro =  ;
                          1,  ;
                          ' en General ',  ;
                          IIF(vtippro =  ;
                          2,  ;
                          ' por Bienes ',  ;
                          ' por Servicio ' ;
                          ))
                         DO CASE
                              CASE  ;
                               vestado =  ;
                               1
                                   vtitu = ' Proveedores en General '
                              CASE  ;
                               vestado =  ;
                               2
                                   vtitu = ' Proveedores Aptos '
                              CASE  ;
                               vestado =  ;
                               3
                                   vtitu = ' Proveedores no Aptos '
                              CASE  ;
                               vestado =  ;
                               4
                                   vtitu = ' Proveedores Pendientes '
                              CASE  ;
                               vestado =  ;
                               5
                                   vtitu = ' Proveedores Anulados '
                         ENDCASE
                         ACTIVATE  ;
                          WINDOW  ;
                          standby
                         an = STR(YEAR(DATE()),  ;
                              4)
                         mes = '00'
                         @ 1, 10  ;
                           SAY  ;
                           'Ingrese Per¡odo -> '  ;
                           GET an  ;
                           PICTURE  ;
                           '!!!!'
                         @ 1, 35  ;
                           GET  ;
                           mes  ;
                           PICTURE  ;
                           '!!'
                         READ
                         DEACTIVATE  ;
                          WINDOW  ;
                          standby
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LISPROX',  ;
                            ' Lista de Proveedores ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    ELSE
                         IF vtiplis =  ;
                            4
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'prvoc',  ;
                                 ' Lista de Proveedores ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ELSE
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'prvos',  ;
                                 ' Lista de Proveedores ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ELSE
          DO standby WITH  ;
             'Archivo Vac¡o ...'
     ENDIF
     SET FILTER TO
     CLOSE INDEX
     ERASE (vind)
ENDIF
SELECT promae
SET ORDER TO 1
GOTO vtemp
RETURN
*
PROCEDURE lispro
IF escolor
     DEFINE POPUP Lismenu FROM 17,54 MARGIN;
SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP lismenu FROM 17,  ;
            54 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
PUBLIC zz
DEFINE BAR 1 OF lismenu PROMPT  ;
       ' \<Gen‚rico     '
DEFINE BAR 2 OF lismenu PROMPT  ;
       ' por \<Cat logo '
DEFINE BAR 3 OF lismenu PROMPT  ;
       ' por \<Proveedor'
ON SELECTION POPUP lismenu DEACTIVATE;
POPUP
ACTIVATE POPUP lismenu
vtemp = RECNO()
DO CASE
     CASE BAR() = 1
          DO lispro1
     CASE BAR() = 2
          IF escolor
               DEFINE POPUP Lismenu FROM;
17,54 MARGIN SHADOW COLOR &L_COL
          ELSE
               DEFINE POPUP  ;
                      lismenu  ;
                      FROM 17, 54  ;
                      MARGIN  ;
                      COLOR  ;
                      SCHEME  ;
                      c_popup
          ENDIF
          DEFINE BAR 1 OF lismenu  ;
                 PROMPT  ;
                 '\<Grupo gen‚rico'
          DEFINE BAR 2 OF lismenu  ;
                 PROMPT  ;
                 '\<Clase         '
          ON SELECTION POPUP lismenu DEACTIVATE;
POPUP
          ACTIVATE POPUP lismenu
          DO CASE
               CASE BAR() = 1
                    SELECT promae
                    SET FILTER TO legali;
= 'û';
.AND. estado = 'VG'
                    COUNT ALL TO  ;
                          vcd
                    SELECT aux
                    SET RELATION TO codprv;
INTO promae
                    SET SKIP TO promae
                    vtem = RECNO()
                    SET UNIQUE ON
                    vind = SYS(3) +  ;
                           '.IDX'
                    INDEX ON  ;
                          LEFT(codcla,  ;
                          3) +  ;
                          codprv  ;
                          TO  ;
                          (vind)  ;
                          FOR   ;
                          .NOT.  ;
                          EMPTY(promae.codprv)
                    SET INDEX TO (vind)
                    IF  .NOT.  ;
                        yesno( ;
                        ' Todos los Grupos Gen‚ricos ?' ;
                        )
                         ACTIVATE  ;
                          WINDOW  ;
                          standby
                         _cod = '   '
                         @ 1, 1  ;
                           SAY  ;
                           'Ingrese C¢digo :'  ;
                           GET  ;
                           _cod  ;
                           PICTURE  ;
                           '!!!'  ;
                           VALID  ;
                           val_para(_cod, ;
                           'CODGE', ;
                           'C')
                         READ
                         DEACTIVATE  ;
                          WINDOW  ;
                          standby
                         cod = parma.codigo
                         SET FILTER TO;
codcla = ALLTRIM(cod);
.AND. promae.legali = 'û';
.AND. promae.estado = 'VG'
                         COUNT ALL  ;
                               TO  ;
                               vcd
                    ENDIF
                    GOTO TOP
                    IF  .NOT.  ;
                        EOF()
                         vtitulo =  ;
                          'Por Grupo Gen‚rico'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'GenPrv1',  ;
                            ' Lista por Grupo Gen‚rico ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    ELSE
                         DO standby  ;
                            WITH  ;
                            'No se tiene existencias de esta l¡nea'
                    ENDIF
                    SET UNIQUE OFF
                    SET FILTER TO
                    CLOSE INDEX
                    SELECT promae
                    SET FILTER TO
                    SELECT aux
                    SET ORDER TO 1
                    GOTO BOTTOM
                    ERASE (vind)
               CASE BAR() = 2
                    SELECT promae
                    SET FILTER TO legali;
= 'û';
.AND. estado = 'VG'
                    COUNT ALL TO  ;
                          vcd
                    SELECT aux
                    SET RELATION TO codprv;
INTO promae
                    SET ORDER TO 2
                    SET SKIP TO promae
                    IF  .NOT.  ;
                        yesno( ;
                        ' Toda la Clasificaci¢n ?' ;
                        )
                         ACTIVATE  ;
                          WINDOW  ;
                          standby
                         _cod = '       '
                         @ 1, 1  ;
                           SAY  ;
                           'Ingrese C¢digo :'  ;
                           GET  ;
                           _cod  ;
                           PICTURE  ;
                           '!!!.!!!'  ;
                           VALID  ;
                           val_art1(_cod, ;
                           .T.)
                         READ
                         DEACTIVATE  ;
                          WINDOW  ;
                          standby
                         SET FILTER TO;
codcla = ALLTRIM(_cod);
.AND. promae.legali = 'û';
.AND. promae.estado = 'VG'
                         COUNT ALL  ;
                               TO  ;
                               vcd
                    ENDIF
                    GOTO TOP
                    IF  .NOT.  ;
                        EOF()
                         vtitulo =  ;
                          'Por Clase'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'GenPrv',  ;
                            ' Lista por Catalogos ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    ELSE
                         DO standby  ;
                            WITH  ;
                            'No se tiene existencias de esta l¡nea'
                    ENDIF
                    SET FILTER TO
                    SET ORDER TO 1
                    SELECT promae
                    SET FILTER TO
          ENDCASE
     CASE BAR() = 3
          DEFINE WINDOW lis FROM  ;
                 3, 15 TO 21, 65  ;
                 FLOAT TITLE  ;
                 'Listado Proveedores'  ;
                 DOUBLE COLOR  ;
                 SCHEME 5
          ACTIVATE WINDOW lis
          STORE 1 TO vtocli,  ;
                vorden, vtippro,  ;
                vestado, vtippro
          vcodprv = SPACE(4)
          @ 01, 01 SAY  ;
            '  Total Proveedor : '  ;
            GET vtocli SIZE 1, 10,  ;
            6 FUNCTION  ;
            '*RNH \<Si;\<No'  ;
            VALID valtod(vtocli,3, ;
            22)
          @ 03, 01 SAY  ;
            '        Proveedor : '
          @ 03, 22 GET vcodprv  ;
            PICTURE '!!!!' VALID  ;
            valprv() WHEN vtocli =  ;
            2
          @ 05, 01 SAY  ;
            '     Ordenado por : '  ;
            GET vorden FUNCTION  ;
            '^ Nombre;C¢digo;Ingreso;RUC'  ;
            WHEN vtocli = 1
          @ 08, 01 SAY  ;
            '           Estado : '  ;
            GET vestado FUNCTION  ;
            '^ Aptos;No aptos'  ;
            WHEN vtocli = 1
          @ 11, 01 SAY  ;
            '   Tipo Proveedor : '  ;
            GET vtippro FUNCTION  ;
            '^ Todos;Bienes;Servicios'  ;
            WHEN vtocli = 1
          @ 14, 10 GET okcancel  ;
            DEFAULT 1 SIZE 1, 11,  ;
            8 FUNCTION  ;
            '*TH \!\<OK;\?\<Cancela'
          READ CYCLE
          RELEASE WINDOW lis
          IF okcancel = 1
               ACTIVATE WINDOW  ;
                        standby
               @ 01, 04 SAY  ;
                 'Espere un momento........'
               vind = SYS(3) +  ;
                      '.IDX'
               IF vtocli = 2
                    vcd = 1
                    SELECT aux
                    SET ORDER TO PRVAUX1
                    SET FILTER TO codprv;
= vcodprv
                    SET RELATION TO codprv;
INTO promae
                    SET SKIP TO promae
               ELSE
                    SELECT promae
                    SET FILTER TO legali;
= 'û';
.AND. estado = 'VG'
                    COUNT ALL TO  ;
                          vcd
                    SELECT aux
                    SET ORDER TO PRVAUX1
                    SET RELATION TO codprv;
INTO promae
                    SET SKIP TO promae
               ENDIF
               GOTO TOP
               IF vtocli = 1
                    IF vestado =  ;
                       1
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               DTOS(promae.fecing)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               IIF(vtocli =  ;
                               1,  ;
                               .T.,  ;
                               codprv =  ;
                               vcodprv)  ;
                               .AND.  ;
                               IIF(vtippro =  ;
                               1,  ;
                               .T.,  ;
                               IIF(vtippro =  ;
                               2,  ;
                               promae.tippro =  ;
                               'B',  ;
                               promae.tippro =  ;
                               'S'))
                         SET FILTER TO;
IIF(SUBSTR(promae.acteco, 1, 2) = 'PJ',;
( .NOT. EMPTY(promae.test);
.AND. ;
.NOT. EMPTY(promae.rl_le);
.AND. ;
.NOT. EMPTY(promae.regunf);
.AND. ;
.NOT. EMPTY(promae.numruc)), (;
.NOT. EMPTY(promae.rl_le);
.AND. ;
.NOT. EMPTY(promae.regunf);
.AND. ;
.NOT. EMPTY(promae.numruc)));
.AND. ;
.NOT. EMPTY(promae.giro);
.AND. IIF(vtocli = 1, promae.legali =;
'û', .T.) .AND. IIF(vtocli = 1, promae.estado = 'VG', .T.)
                    ELSE
                         INDEX ON  ;
                               IIF(vorden =  ;
                               1,  ;
                               promae.nompro,  ;
                               IIF(vorden =  ;
                               2,  ;
                               codprv,  ;
                               DTOS(promae.fecing)))  ;
                               TO  ;
                               (vind)  ;
                               FOR  ;
                               IIF(vtocli =  ;
                               1,  ;
                               .T.,  ;
                               codprv =  ;
                               vcodprv)  ;
                               .AND.  ;
                               IIF(vtippro =  ;
                               1,  ;
                               .T.,  ;
                               IIF(vtippro =  ;
                               2,  ;
                               promae.tippro =  ;
                               'B',  ;
                               promae.tippro =  ;
                               'S'))
                         SET FILTER TO;
IIF(SUBSTR(promae.acteco, 1, 2) = 'PJ',;
(EMPTY(promae.test);
.OR. EMPTY(promae.rl_le);
.OR. EMPTY(promae.regunf);
.OR. EMPTY(promae.numruc)), (EMPTY(promae.rl_le);
.OR. EMPTY(promae.regunf);
.OR. EMPTY(promae.numruc)));
.OR. EMPTY(promae.giro);
.AND. promae.legali <> 'û';
.AND. promae.estado <> 'VG'
                    ENDIF
                    SET INDEX TO (vind)
                    GOTO TOP
               ENDIF
               DEACTIVATE WINDOW  ;
                          standby
               IF  .NOT. EOF()
                    DO reporte  ;
                       WITH 2,  ;
                       'GenPrv2A',  ;
                       ' Lista por Proveedor ',  ;
                       1, .F.,  ;
                       .T.
               ELSE
                    DO standby  ;
                       WITH  ;
                       'No se tiene existencias de esta l¡nea'
               ENDIF
               SET RELATION TO
               SET FILTER TO
               CLOSE INDEX
               ERASE (vind)
          ENDIF
          SELECT promae
          SET ORDER TO 1
     OTHERWISE
ENDCASE
RELEASE POPUP lismenu
RETURN
*
FUNCTION val_art1
PARAMETER xcod, _tipo, _x, _y
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) .AND.  ;
       _tipo
_campo = VARREAD()
malias = ALIAS()
SELECT produ
_oldwnd = WOUTPUT()
IF  .NOT. medita
     SEEK xcod
     v_fun = IIF(FOUND(), descri,  ;
             '')
ELSE
     IF EMPTY(xcod)
          SET ORDER TO 1
          ACTIVATE WINDOW standby
          @ 1, 14 SAY  ;
            'Espere un Momento ....'  ;
            COLOR W/N* 
          GOTO TOP
          IF EOF()
               DEACTIVATE WINDOW  ;
                          standby
               ACTIVATE SCREEN
               SET FILTER TO
               v_fun = .F.
          ELSE
               DEACTIVATE WINDOW  ;
                          standby
               ACTIVATE SCREEN
               ON KEY LABEL F10 KEYBOARD;
CHR(23)
               ON KEY LABEL F2 DO FunBus
               DEFINE WINDOW  ;
                      _busart  ;
                      FROM 2, 02  ;
                      TO 22, 77
               BROWSE FIELDS  ;
                      codart :H =  ;
                      'C¢digo' :W =  ;
                      EMPTY(SUBSTR(codart,  ;
                      5, 3)),  ;
                      descri :H =  ;
                      'Nombre' :  ;
                      60 :W =  ;
                      EMPTY(descri)  ;
                      NOMENU  ;
                      NOAPPEND  ;
                      NODELETE  ;
                      WINDOW  ;
                      _busart  ;
                      TITLE  ;
                      '²²²² [F10] Selecciona   [F2] Buscar ²²²²'  ;
                      NOLGRID
               ON KEY LABEL F10
               ON KEY LABEL F2
               RELEASE WINDOW  ;
                       _busart
               SET FILTER TO
               IF  .NOT.  ;
                   EMPTY(_oldwnd)
                    ACTIVATE WINDOW &_OldWnd
               ENDIF
               IF LASTKEY() = 27
                    v_fun = .F.
               ELSE
                    xcod = codart
                    IF mmsg
                         @ _x, _y  ;
                           SAY  ;
                           descri
                    ENDIF
                    SELECT (malias)
                    IF  .NOT.  ;
                        _tipo
                         REPLACE &_campo;
WITH  xcod
                    ENDIF
                    v_fun = .T.
               ENDIF
          ENDIF
     ELSE
          SEEK xcod
          IF mmsg .AND. FOUND()
               @ _x, _y SAY  ;
                 descri
          ENDIF
          v_fun = FOUND()
     ENDIF
ENDIF
SET ORDER TO 1
SELECT (malias)
RETURN v_fun
*
FUNCTION buscart
PARAMETER vcodigo
as = ALIAS()
SELECT produ
SET ORDER TO 1
SEEK ALLTRIM(vcodigo)
IF  .NOT. FOUND()
     vfun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
ELSE
     vfun = produ.descri
ENDIF
SELECT (as)
RETURN vfun
*
FUNCTION busprvoc
PRIVATE ali, vkey
ali = ALIAS()
vkey = '96' + promae.codprv
SELECT orden
SEEK vkey
IF FOUND()
     vfun = .T.
ELSE
     vfun = .F.
ENDIF
SELECT (ali)
RETURN vfun
*
FUNCTION busprvos
PRIVATE ali, vkey
ali = ALIAS()
vkey = '96' + promae.codprv
SELECT ordser
SEEK vkey
IF FOUND()
     vfun = .T.
ELSE
     vfun = .F.
ENDIF
SELECT (ali)
RETURN vfun
*

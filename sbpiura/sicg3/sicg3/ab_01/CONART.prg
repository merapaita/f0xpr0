USE IN 1 ParMae ALIAS parma ORDER  ;
    ParMae1
USE IN 2 ArtMae ALIAS produ ORDER  ;
    ArtMae1
USE IN 3 IteArt ALIAS iteart  ;
    ORDER IteArt1
PRIVATE vmens01, vmens02, vmens03,  ;
        vmens04, vmens05, vmens06,  ;
        vmens07, vmens08
vmens01 = ' Cat?logo de Existencias '
vmens02 = 'Revisi?n de Art?culos'
vmens03 = 'C?digo del Art?culo: '
vmens04 = 'Dicho Art?culo no fue encontrado.'
vmens05 = 'No existe Art?culo anterior.'
vmens06 = 'No existe Art?culo siguiente.'
vmens07 = '?Est? seguro que desea ELIMINAR ?ste Art?culo?'
vmens08 = 'No hay registros para procesar'
SELECT produ
GOTO TOP
SCATTER BLANK MEMVAR
HIDE POPUP ALL
DO inicia
DO pantalla
DO vista
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     ACTIVATE MENU nmenu
ENDDO
DO fin_opcion
RETURN
*
PROCEDURE inicia
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente                             Lista   Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 08, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 09, 00  ;
       TO 23, 79 TITLE vmens02  ;
       FOOTER '?F5? Avanza'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 00, 00  ;
       TO 23, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 09, 00  ;
       TO 23, 79 TITLE vmens02 +  ;
       '       ?Esc? Escoge'  ;
       DOUBLE COLOR SCHEME 10
DEFINE MENU nmenu COLOR SCHEME 3
DEFINE PAD revis OF nmenu PROMPT  ;
       '\<Revisa' AT 24, 00
DEFINE PAD busca OF nmenu PROMPT  ;
       '\<Busca' AT 24, 08
DEFINE PAD anter OF nmenu PROMPT  ;
       '\<Anterior' AT 24, 15
DEFINE PAD proxi OF nmenu PROMPT  ;
       '\<Siguiente' AT 24, 25
DEFINE PAD lista OF nmenu PROMPT  ;
       '\<Lista ' AT 24, 63
DEFINE PAD termi OF nmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF nmenu DO revis
ON SELECTION PAD busca OF nmenu DO busca
ON SELECTION PAD anter OF nmenu DO anter
ON SELECTION PAD proxi OF nmenu DO proxi
ON SELECTION PAD lista OF nmenu DO lista
ON SELECTION PAD termi OF nmenu DO termi
ACTIVATE SCREEN
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_0
CLEAR
@ 2, 2 SAY '         Cat?logo :'
@ 4, 2 SAY '      Descripci?n :'
RETURN
*
PROCEDURE vista
ACTIVATE WINDOW wind_0
SELECT produ
SCATTER MEMVAR
@ 2, 22 SAY m.codart
@ 4, 22 SAY SUBSTR(m.descri, 1,  ;
  54)
@ 5, 22 SAY SUBSTR(m.descri, 55,  ;
  10)
DO vista_hijo
RETURN
*
PROCEDURE vista_hijo
ACTIVATE WINDOW wind_1
SELECT iteart
GOTO TOP
SEEK LEFT(m.codart, 7)
IF FOUND()
     ON KEY LABEL F5 DO VISTA_PRO
     BROWSE NOOPTIMIZE FIELDS xx =  ;
            SUBSTR(codart, 8, 3)  ;
            :H = 'Cod', descri :H =  ;
            'Descripci?n' : 64,  ;
            coduni :H = 'Unidad'  ;
            : 8 NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_1  ;
            KEY LEFT(m.codart, 7)  ;
            TIMEOUT 0.001   ;
            NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 5, 33 SAY 'Sin Detalle'
ENDIF
SELECT produ
RETURN
*
PROCEDURE vista_pro
SELECT iteart
SEEK LEFT(m.codart, 7)
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS xx =  ;
            SUBSTR(codart, 8, 3)  ;
            :H = 'Cod', descri :H =  ;
            'Descripci?n' : 64,  ;
            coduni :H = 'Uni' : 8  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_1  ;
            KEY LEFT(m.codart, 7)  ;
            NOREFRESH
ENDIF
SELECT produ
RETURN
*
PROCEDURE revis
SELECT produ
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
IF yesno( ;
   '? En Orden Alfab?tico ? [ NO = Por C?digo]' ;
   )
     SET ORDER TO 2
ENDIF
HIDE MENU nmenu
ACTIVATE SCREEN
vtempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SET RELATION TO LEFT(codart, 7) INTO iteart
SET SKIP TO iteart
BROWSE FIELDS xq = SUBSTR(codart,  ;
       1, 7) :H = 'Articulo',  ;
       descri :H = 'Descripci?n'  ;
       : 30, xx =  ;
       SUBSTR(iteart.codart, 8,  ;
       3) :H = 'Det',  ;
       iteart.descri :H =  ;
       'Detalle' : 60,  ;
       iteart.preuni :H =  ;
       'Precio', iteart.codpart  ;
       :H = 'Partida' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_2
vtempo = '????????????????????????????????????????????????????????????????????????????????'
DO logos WITH rotulo1, vtempo
SET ORDER TO 1
IF LASTKEY() = 27
     SET RELATION TO
     GOTO vtemp
ENDIF
SHOW MENU nmenu
ON KEY LABEL F10
SET RELATION TO
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
IF  .NOT. yesno('?Por Art?culo?')
     DO funbus
ELSE
     SELECT iteart
     SET RELATION TO 'B' + LEFT(codart,;
6) INTO produ
     DO funbusdet
     SET ORDER TO 2
     BROWSE FIELDS iteart.codart  ;
            :H = 'Articulo' : 10,  ;
            iteart.descri :H =  ;
            'Descripci?n' : 35,  ;
            produ.codart :H =  ;
            'Det' : 7,  ;
            produ.descri :H =  ;
            'Detalle' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_3
     SET ORDER TO 1
     SET RELATION TO
     SELECT produ
ENDIF
IF EOF()
     DO standby WITH vmens04
     GOTO vtemp
ELSE
     DO vista
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
PROCEDURE lista
SELECT produ
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vreg = RECNO()
DO lisexi
GOTO vreg
RETURN
*
PROCEDURE termi
ven_accion = .F.
ON KEY LABEL F5
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE MENU nmenu
RETURN
*
PROCEDURE lisexi
DEACTIVATE WINDOW wind_1
IF escolor
     DEFINE POPUP Lismenu FROM 17,54 MARGIN;
SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP lismenu FROM 17,  ;
            54 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF lismenu PROMPT  ;
       ' \<Grupo gen?rico   '
DEFINE BAR 2 OF lismenu PROMPT  ;
       ' g\<Rupo espec?fico '
DEFINE BAR 3 OF lismenu PROMPT  ;
       ' por \<Producto     '
ON SELECTION POPUP lismenu DEACTIVATE;
POPUP
ACTIVATE POPUP lismenu
DO CASE
     CASE BAR() = 1
          SELECT parma
          SET FILTER TO tipo = 'CODGEB'
          GOTO TOP
          DO reporte WITH 2,  ;
             'Artprv3',  ;
             ' Grupos Gen?ricos ',  ;
             1, .F., .T.
          SET FILTER TO
     CASE BAR() = 2
          IF  .NOT. yesno( ;
              '? Reporte por 1 Grupo Gen?rico ?' ;
              )
               SET FILTER TO LEFT(codart,;
1) = 'B'
               DO reporte WITH 2,  ;
                  'Artprv1',  ;
                  ' Art?culos ',  ;
                  1, .F., .T.
          ELSE
               ACTIVATE WINDOW  ;
                        standby
               vcod = '   '
               @ 01, 02 SAY  ;
                 'C?digo: ' GET  ;
                 vcod PICTURE  ;
                 '!!!' VALID  ;
                 val_para(vcod, ;
                 'CODGEB',' ',10, ;
                 10)
               READ
               DEACTIVATE WINDOW  ;
                          standby
               IF LASTKEY() <> 27
                    SELECT produ
                    SET FILTER TO LEFT(codart,;
3) = ALLTRIM(vcod);
.AND. LEFT(codart, 1) = 'B'
                    DO reporte  ;
                       WITH 2,  ;
                       'ArtPrv1',  ;
                       ' Existencias por L?neas ',  ;
                       1, .F.,  ;
                       .T.
               ELSE
                    DO standby  ;
                       WITH  ;
                       ' No se tiene proveedores en este cat?logo '
               ENDIF
               SET FILTER TO
          ENDIF
     CASE BAR() = 3
          SELECT iteart
          IF yesno( ;
             '? Reporte por 1 Grupo Espec?fico ?' ;
             )
               ACTIVATE WINDOW  ;
                        standby
               cod = '       '
               @ 1, 1 SAY  ;
                 'Ingrese C?digo :'  ;
                 GET cod VALID  ;
                 valart(cod)
               READ
               DEACTIVATE WINDOW  ;
                          standby
               SET FILTER TO 'B' + SUBSTR(codart,;
1, 6) = ALLTRIM(cod)
          ENDIF
          GOTO TOP
          IF  .NOT. EOF()
               DO reporte WITH 2,  ;
                  'Artprv2',  ;
                  ' Lista por Productos ',  ;
                  1, .F., .T.
          ELSE
               DO standby WITH  ;
                  'No se tiene existencias de esta l?nea'
          ENDIF
          SET FILTER TO
          SET ORDER TO 1
     OTHERWISE
ENDCASE
SELECT produ
RELEASE POPUP lismenu
DO vista
RETURN
*
FUNCTION buscart
as = ALIAS()
SELECT produ
SET ORDER TO 1
SEEK 'B' + LEFT(iteart.codart, 6)
IF  .NOT. FOUND()
     vfun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
ELSE
     vfun = UPPER(produ.descri)
ENDIF
SELECT (as)
RETURN vfun
*
FUNCTION sicto
vreturn = .F.
valias = ALIAS()
SELECT parma
SEEK 'TIPEXI' + m.tipexi
IF TRIM(UPPER(parma.descriaux)) =  ;
   'P.TERMINADO'
     vreturn = .T.
ENDIF
SELECT (valias)
RETURN vreturn
*
FUNCTION valarta
PARAMETER _cod
PRIVATE xx, vfun
vfun = .F.
xx = val_art(_cod,.T.,1,18)
IF xx
     vcod = produ.codart
     vfun = .T.
ENDIF
RETURN vfun
*
FUNCTION valart
PARAMETER _cod
PRIVATE xx, yy, zz, vfun
vfun = .T.
as = ALIAS()
IF EMPTY(_cod)
     zz = val_para(_cod,'CODGEB', ;
          'C')
     IF LASTKEY() = 27
          RETURN .T.
     ENDIF
     IF zz
          xx = val_art1(cod,.F.)
          IF xx
               _cod = produ.codart
               vfun = .T.
          ENDIF
     ENDIF
ELSE
     vfun = .T.
ENDIF
SELECT (as)
UNLOCK ALL
RETURN vfun
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
SET ORDER TO 1
ACTIVATE WINDOW standby
@ 1, 14 SAY  ;
  'Espere un Momento ....' COLOR  ;
  W/N* 
GOTO TOP
IF EOF()
     DEACTIVATE WINDOW standby
     ACTIVATE SCREEN
     SET FILTER TO
     v_fun = .F.
ELSE
     SET ORDER TO 1
     SET FILTER TO SUBSTR(codart, 2, 2);
= SUBSTR(ALLTRIM(parma.codigo), 2, 2)
     ACTIVATE WINDOW standby
     @ 1, 14 SAY  ;
       'Espere un Momento ....'  ;
       COLOR W/N* 
     GOTO TOP
     DEACTIVATE WINDOW standby
     ACTIVATE SCREEN
     ON KEY LABEL F10 KEYBOARD CHR(23)
     ON KEY LABEL F2 DO FunBus
     DEFINE WINDOW _busart FROM 2,  ;
            02 TO 22, 77
     BROWSE FIELDS codart :H =  ;
            'C?digo' :W =  ;
            EMPTY(SUBSTR(codart,  ;
            5, 3)), descri :H =  ;
            'Nombre' : 60 :W =  ;
            EMPTY(descri) NOMENU  ;
            NOAPPEND NODELETE  ;
            WINDOW _busart TITLE  ;
            '???? [F10] Selecciona   [F2] Buscar ????'  ;
            NOLGRID
     ON KEY LABEL F10
     ON KEY LABEL F2
     RELEASE WINDOW _busart
     SET FILTER TO
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     IF LASTKEY() = 27
          v_fun = .F.
     ELSE
          xcod = codart
          IF mmsg
               @ _x, _y SAY  ;
                 descri
          ENDIF
          SELECT (malias)
          IF  .NOT. _tipo
               &_CAMPO = ALLTRIM(XCOD)
          ENDIF
          v_fun = .T.
     ENDIF
ENDIF
SET ORDER TO 1
SELECT (malias)
RETURN v_fun
*

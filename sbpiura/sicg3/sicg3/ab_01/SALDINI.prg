USE IN 1 IteArt ALIAS iteart  ;
    ORDER IteArt2
USE IN 2 STOCK ALIAS stock ORDER  ;
    STOCKX
SELECT iteart
mmaa = '0000'
vfte = '  '
ON KEY LABEL F8 do INVENTAR
ON KEY LABEL F2 do Busca
vcampo = mmaa + vfte
DEFINE WINDOW _inven FROM 10, 27  ;
       TO 15, 77 FLOAT SHADOW  ;
       DOUBLE
DEFINE WINDOW _invfec FROM 02, 27  ;
       TO 05, 77 FLOAT SHADOW  ;
       DOUBLE
DEACTIVATE WINDOW standby
IF LASTKEY() = 27
     CLOSE DATABASES
     RETURN
ENDIF
GOTO TOP
PRIVATE vmens01, vmens02, vmens03,  ;
        vmens04, vmens05, vmens06,  ;
        vmens07, vmens08
vmens02 = ' Cat logo de Existencias '
vmens01 = 'Actualizaci¢n de Stock de Art¡culos - A¤o: ' +  ;
          SUBSTR(DTOC(DATE()), 7,  ;
          4)
vmens03 = 'C¢digo del Art¡culo: '
vmens04 = 'Dicho Art¡culo no fue encontrado.'
vmens05 = 'No existe Art¡culo anterior.'
vmens06 = 'No existe Art¡culo siguiente.'
vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste Art¡culo?'
vmens08 = 'No hay registros para procesar'
HIDE POPUP ALL
DO invfinal
IF LASTKEY() = 27
     RETURN
ENDIF
DO inicia
DO vista
DEACTIVATE WINDOW wind_0
ACTIVATE SCREEN
DO fin_opcion
RETURN
*
PROCEDURE invfinal
PUBLIC fecini
fecini = SPACE(10)
ACTIVATE WINDOW _invfec
@ 01, 02 SAY 'Fecha Inventario: '  ;
  GET fecini PICTURE '@D' VALID   ;
  .NOT. EMPTY(fecini)
READ
DEACTIVATE WINDOW _invfec
*
PROCEDURE inicia
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW wind_0
RETURN
*
PROCEDURE vista
ACTIVATE SCREEN
vtempo = '°°°°°° ®F2¯ Busca      ®F8¯ Agregar Stock inicial  ®Esc¯ para salir   °°°°°°°'
DO logos WITH rotulo1, vtempo
SELECT iteart
BROWSE NOOPTIMIZE FIELDS codart  ;
       :H = 'Codigo' :P =  ;
       '!!!!!!!!!!' :W = .F.,  ;
       descri :H = 'Descripci¢n'  ;
       : 44 :W = .F., coduni :H =  ;
       'Unidad ' :W = .F.,  ;
       cantini :H = 'Cantidad' :W =  ;
       .F., preuni :H = 'Precio'  ;
       :W = .F. NOMENU NOAPPEND  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_0 NOREFRESH
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
DO funbux
IF EOF()
     DO standby WITH vmens04
     GOTO vtemp
ELSE
ENDIF
RETURN
*
PROCEDURE termi
ven_accion = .F.
ON KEY LABEL F5
ON KEY LABEL F2
ON KEY LABEL F8
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
ON KEY LABEL F5
ON KEY LABEL F2
DEACTIVATE MENU
CLOSE DATABASES
RELEASE WINDOW wind_0
RELEASE MENU nmenu
RETURN
*
FUNCTION buscart
as = ALIAS()
SELECT produ
SET ORDER TO 1
SEEK 'B' + LEFT(iteart.codart,  ;
     10)
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
            'C¢digo' :W =  ;
            EMPTY(SUBSTR(codart,  ;
            5, 3)), descri :H =  ;
            'Nombre' : 60 :W =  ;
            EMPTY(descri) NOMENU  ;
            NOAPPEND NODELETE  ;
            WINDOW _busart TITLE  ;
            '²²²² [F10] Selecciona   [F2] Buscar ²²²²'  ;
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
PROCEDURE funbux
ON KEY LABEL F2
IF escolor
     DEFINE POPUP _mm FROM 16,54 SHADOW;
COLOR &l_col
ELSE
     DEFINE POPUP _mm FROM 16, 54  ;
            COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF _mm PROMPT  ;
       ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT  ;
       ' Busqueda por \<Nombre '
ON SELECTION POPUP _mm DEACTIVATE POPUP
PRIVATE orden
orden = ORDER()
_cod = SPACE(11)
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
     CASE BAR() = 1
          ACTIVATE WINDOW _funbus
          @ 01, 02 SAY 'C¢digo: '  ;
            GET _cod PICTURE  ;
            '!!.!!!.!!!'
          READ
          DEACTIVATE WINDOW  ;
                     _funbus
          IF LASTKEY() <> 27
               SET ORDER TO ITEART3
               SEEK ALLTRIM(_cod)
          ENDIF
     CASE BAR() = 2
          ACTIVATE WINDOW _funbus
          _cod = SPACE(40)
          @ 01, 02 SAY 'Nombre: '  ;
            GET _cod PICTURE  ;
            '@S30'
          READ
          DEACTIVATE WINDOW  ;
                     _funbus
          IF LASTKEY() <> 27
               SET ORDER TO ITEART2
               SEEK UPPER(ALLTRIM(_cod))
          ENDIF
     OTHERWISE
          RELEASE POPUP _mm
          SET ORDER TO (orden)
          RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
ON KEY LABEL F2 do Busca
RETURN
*
PROCEDURE inventar
ACTIVATE WINDOW _inven
STORE 14 TO cai
STORE 14 TO pri
cai = iteart.cantini
pri = iteart.preuni
@ 02, 02 SAY 'Cantidad : ' GET  ;
  cai PICTURE '99999.999'
@ 03, 02 SAY 'Precio   : ' GET  ;
  pri PICTURE '99999.999'
READ
IF yesno( ;
   '¨ Esta Correcto el ingreso ?' ;
   )
     SELECT iteart
     codii = iteart.codart
     REPLACE iteart.preuni WITH  ;
             pri
     REPLACE iteart.cantini WITH  ;
             cai
     SELECT stock
     DELETE FOR stock.codart =  ;
            codii .AND.  ;
            stock.tipdoc = 'INV'
     APPEND BLANK
     REPLACE periodo WITH  ;
             SUBSTR(fecini, 9,  ;
             2)
     REPLACE tipdoc WITH 'INV'
     REPLACE numdoc WITH 'INIC'
     REPLACE codart WITH codii
     REPLACE fuente WITH '09'
     REPLACE tipomov WITH 'I'
     REPLACE fechamov WITH  ;
             CTOD(fecini)
     REPLACE cantidad WITH cai
     REPLACE cosmed WITH pri
ELSE
     RETURN
ENDIF
SELECT iteart
DEACTIVATE WINDOW _inven
*

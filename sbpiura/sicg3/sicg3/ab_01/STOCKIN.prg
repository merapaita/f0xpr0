USE IN 1 ParMae ALIAS parma ORDER  ;
    ParMae1
USE IN 2 ArtMae ALIAS produ ORDER  ;
    ArtMae1
USE IN 3 IteArt ALIAS iteart  ;
    ORDER IteArt2
USE IN 4 ItePec ALIAS itepec  ;
    ORDER ItePec10
USE IN 5 Pecosa ALIAS pecosa  ;
    ORDER Pecosa1
SET DATE TO DMY
SELECT iteart
STORE SPACE(2) TO mm
STORE SPACE(4) TO aa1
vfte = '  '
ON KEY LABEL F2 do Busca
ON KEY LABEL F11 do PECOSAS
ACTIVATE WINDOW standby
@ 0, 00 SAY  ;
  'Inventario ===> [Mes] : ' GET  ;
  mm PICTURE '!!' VALID  ;
  val_regmes(mm,'FECMES',' ',22, ;
  15)
@ 1, 00 SAY  ;
  '                [A¤o] : ' GET  ;
  aa1 PICTURE '9999'
@ 2, 00 SAY  ;
  '             [Fuente] : ' GET  ;
  vfte PICTURE '!!' VALID  ;
  val_para(vfte,'CODFTE','C',33, ;
  20)
READ
IF LASTKEY() <> 27
     WAIT WINDOW NOWAIT  ;
          'Espere un Momento Por Favor, Procesando...'
ENDIF
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
vmens01 = 'Revisi¢n de Stock Art¡culos - Semestre: '
vmens03 = 'C¢digo del Art¡culo: '
vmens04 = 'Dicho Art¡culo no fue encontrado.'
vmens05 = 'No existe Art¡culo anterior.'
vmens06 = 'No existe Art¡culo siguiente.'
vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste Art¡culo?'
vmens08 = 'No hay registros para procesar'
HIDE POPUP ALL
DO inicia
DO vista
DEACTIVATE WINDOW wind_0
ACTIVATE SCREEN
DO fin_opcion
RETURN
*
PROCEDURE inicia
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 05, 02  ;
       TO 19, 77 TITLE  ;
       ' Listado Pecosas ' DOUBLE  ;
       COLOR SCHEME 10
ACTIVATE WINDOW wind_0
RETURN
*
PROCEDURE vista
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°° ®F2¯ Busca      ®F11¯ Pecosas      ®Esc¯ para salir   °°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SELECT iteart
ye = ALLTRIM(aa1)
mm = ALLTRIM(mm)
SET FILTER TO SUBSTR(DTOC(fecreg), 4,;
2) = mm;
.AND. SUBSTR(DTOC(fecreg), 7, 4) = ye
COUNT FOR SUBSTR(DTOC(fecreg), 4,  ;
      2) = mm .AND.  ;
      SUBSTR(DTOC(fecreg), 7, 4) =  ;
      ye TO conte
WAIT WINDOW NOWAIT  ;
     'Informaci¢n Solicitada en Pantalla, VERIFIQUE...'
DO alarma
IF conte <> 0
     GOTO TOP
     BROWSE NOOPTIMIZE FIELDS  ;
            codart :H = 'Codigo'  ;
            :P = '!!!!!!!!!!' :W =  ;
            .T., descri :H =  ;
            'Descripci¢n' : 44 :W =  ;
            .T., coduni :H =  ;
            'Unidad' : 10,  ;
            codpart :H =  ;
            'Partida' :W = .F.,  ;
            codgen :H =  ;
            'Codigo GG' :P = '!!'  ;
            :W = .F., codcla :H =  ;
            'Cls' :P = '!!!' :W =  ;
            .F., coddet :H =  ;
            'Det' :P = '!!!' :W =  ;
            .F., fecreg :H =  ;
            'Fecha' NOMENU  ;
            NOAPPEND NODELETE  ;
            NOCLEAR WINDOW wind_0  ;
            NOREFRESH
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
ELSE
     WAIT WINDOW NOWAIT  ;
          'No Existen Articulos de Tal Fecha...'
ENDIF
RETURN
*
PROCEDURE pecosas
PRIVATE malias
malias = ALIAS()
vcod = ALLTRIM(iteart.codart)
ON KEY LABEL F11
SELECT itepec
SET RELATION TO periodo + numpec + codfte;
INTO pecosa
BROWSE NOOPTIMIZE FIELDS codfte  ;
       :H = 'FTE', numpec :H =  ;
       'N§PC', canreq :H =  ;
       'Pedido' :P = '99,999.99',  ;
       coduni :H = 'Unid' : 4,  ;
       preuni :H = 'PreUni' :P =  ;
       '99,999.999', xx = preuni *  ;
       canreq :H = 'Total' :P =  ;
       '99,999.99', numoc :H =  ;
       'NumOc', ww =  ;
       IIF(EMPTY(pecosa.coddep),  ;
       '** ?? **',  ;
       val_para(pecosa.coddep, ;
       'CODDEP','D',22,60)) :H =  ;
       'Dependencia' : 40 NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       NOCLEAR WINDOW wind_1 KEY  ;
       vcod NOREFRESH
SELECT (malias)
SET RELATION TO
ON KEY LABEL F11 do PECOSAS
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
SEEK LEFT(iteart.codart, 10)
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
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
     CASE BAR() = 1
          ACTIVATE WINDOW _funbus
          _cod = '00.000.000'
          @ 01, 02 SAY 'C¢digo: '  ;
            GET _cod PICTURE  ;
            '!!.!!!.!!!'
          READ
          DEACTIVATE WINDOW  ;
                     _funbus
          IF LASTKEY() <> 27
               SET ORDER TO 1
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
               SET ORDER TO 2
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
FUNCTION val_regmes
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong,  ;
          mdist
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = ' '
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 3
          mcol = 0
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 4
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 5
          mdist = 6
ENDCASE
malias = ALIAS()
SELECT parma
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO tipo = filtro
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen Registros para Procesar'
          SET FILTER TO
          IF  .NOT. EMPTY(malias)
               SELECT (malias)
          ENDIF
          RETURN
     ENDIF
     SET CONFIRM ON
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            SUBSTR(descri, 1,  ;
            40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_oldwnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = ALLTRIM(parma.codigo)
mcuenta = parma.descriau2
mdescr = SUBSTR(parma.descri, 1,  ;
         mlong)
mdescriaux = SUBSTR(parma.descriaux,  ;
             1, mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable == 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN ' '
     CASE mvariable == 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescr
     CASE mvariable == 'D'
          RETURN mdescr
     CASE mvariable == 'Z'
          RETURN mdescr
     CASE mvariable == 'C'
          RETURN .T.
     OTHERWISE
          REPLACE &mvariable WITH mvalor
          RETURN .T.
ENDCASE
*

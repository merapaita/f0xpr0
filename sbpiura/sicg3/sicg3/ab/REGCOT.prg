PARAMETER vopcion
SET EXCLUSIVE OFF
USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
IF vopcion = 1
     USE IN 2 Solcot ALIAS solcot  ;
         ORDER Solcot1
     USE IN 7 AuxCot ALIAS auxcot  ;
         ORDER Auxcot1
ELSE
     USE IN 2 Solcot ALIAS solcot  ;
         ORDER Solcot2
     USE IN 7 AuxCot ALIAS auxcot  ;
         ORDER Auxcot2
ENDIF
USE IN 3 Itesc ALIAS itesc ORDER  ;
    Itesc1
USE IN 4 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 5 Itepec ALIAS itepec  ;
    ORDER ItePec2
USE IN 6 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 8 Iteart ALIAS iteart  ;
    ORDER Iteart1
vmens01 = ' Solicitud de Cotizaci¢n - Proveedor : REVISION '
vmens02 = ' Recepci¢n de Cotizaci¢n '
vmens04 = 'Dicho Solicitud de Cotizaci¢n no fue encontrada'
vmens05 = 'No existe Solicitud de Cotizaci¢n anterior'
vmens06 = 'No existe Solicitud de Cotizaci¢n siguiente'
vmens07 = '¨ Desea ELIMINAR ‚sta Solicitud de Cotizaci¢n ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Solicitud de Cotizaci¢n ha sido anulada'
vmens10 = 'La Solicitud de Cotizaci¢n ya fue atendida'
vmens11 = 'La Solicitud de Cotizaci¢n ha sido devuelta'
SELECT solcot
GOTO BOTTOM
SCATTER BLANK MEMVAR
ON KEY LABEL F5 DO VISTA_PROV
HIDE POPUP ALL
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
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 11, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       ' Detalle: Cotizaci¢n '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_9 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       ' ®Cuadro Comparativo de Cotizaciones¯ '  ;
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
ACTIVATE WINDOW wind_1
CLEAR
@ 1, 2 SAY ' N£mero Solicitud :'
@ 1, 40 SAY '            Fecha :'
RETURN
*
PROCEDURE vista
SELECT solcot
DO pantalla
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@ 0, 60 SAY IIF(m.estado = '00',  ;
  'Pendiente', IIF(m.estado =  ;
  '20', 'Con Cotiz', IIF(m.estado =  ;
  '99', 'Anulada  ', IIF(m.estado =  ;
  '50', 'Atendido ',  ;
  '         '))))
@ 1, 22 SAY m.periodo
@ 1, 24 SAY '-'
@ 1, 25 SAY m.numsc
@ 1, 60 SAY m.fecsc
@ 2, 40 SAY IIF(EMPTY(m.numccc),  ;
  '                   ',  ;
  'Cuadro Comparativo:')
@ 2, 60 SAY IIF(EMPTY(m.numccc),  ;
  '    ', m.numccc)
IF  .NOT. EMPTY(m.codprv1)
     @ 3, 2 SAY  ;
       '   Proveedor N§ 1 :  '
     @ 3, 22 SAY m.codprv1
     @ 3, 27 SAY  ;
       val_prv(m.codprv1)
ELSE
     @ 3, 2 SAY SPACE(77)
ENDIF
IF  .NOT. EMPTY(m.codprv2)
     @ 4, 2 SAY  ;
       '   Proveedor N§ 2 :  '
     @ 4, 22 SAY m.codprv2
     @ 4, 27 SAY  ;
       val_prv(m.codprv2)
ELSE
     @ 4, 2 SAY SPACE(77)
ENDIF
IF  .NOT. EMPTY(m.codprv3)
     @ 5, 2 SAY  ;
       '   Proveedor N§ 3 :  '
     @ 5, 22 SAY m.codprv3
     @ 5, 27 SAY  ;
       val_prv(m.codprv3)
ELSE
     @ 5, 2 SAY SPACE(77)
ENDIF
IF  .NOT. EMPTY(m.codprv4)
     @ 6, 2 SAY  ;
       '   Proveedor N§ 4 :  '
     @ 6, 22 SAY m.codprv4
     @ 6, 27 SAY  ;
       val_prv(m.codprv4)
ELSE
     @ 6, 2 SAY SPACE(77)
ENDIF
IF  .NOT. EMPTY(m.codprv5)
     @ 7, 2 SAY  ;
       '   Proveedor N§ 5 :  '
     @ 7, 22 SAY m.codprv5
     @ 7, 27 SAY  ;
       val_prv(m.codprv5)
ELSE
     @ 7, 2 SAY SPACE(77)
ENDIF
IF  .NOT. EMPTY(m.codprv6)
     @ 8, 2 SAY  ;
       '   Proveedor N§ 6 :  '
     @ 8, 22 SAY m.codprv6
     @ 8, 27 SAY  ;
       val_prv(m.codprv6)
ELSE
     @ 8, 2 SAY SPACE(77)
ENDIF
DO vista_hijo
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT itesc
IF vopcion = 1
     BROWSE NOOPTIMIZE FIELDS xx =  ;
            '-', desped :H =  ;
            'Descripci¢n' : 57 :W =  ;
            .F., cansol :H =  ;
            'Cantid' :P =  ;
            '9,999.999', unimed  ;
            :H = 'Uni' :W = .F. :  ;
            6 NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.periodo +  ;
            m.numsc TIMEOUT 0.001   ;
            NOREFRESH
ELSE
     BROWSE NOOPTIMIZE FIELDS xx =  ;
            '-', aa =  ;
            MLINE(detalle, 1) :H =  ;
            'Descripci¢n' : 79 :W =  ;
            .F. NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.periodo +  ;
            m.numsc TIMEOUT 0.001   ;
            NOREFRESH
ENDIF
SELECT solcot
RETURN
*
PROCEDURE revis
SELECT solcot
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SET RELATION TO periodo + numsc INTO auxcot
SET SKIP TO auxcot
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS numsc :H = ' N§ ',  ;
       fecsc :H = 'Fecha', tipdoc  ;
       :H = 'DOC', ess =  ;
       IIF(estado = '00', 'Pend',  ;
       IIF(estado = '20', 'C/c ',  ;
       IIF(estado = '99', 'Anul',  ;
       IIF(estado = '50', 'Aten',  ;
       '    ')))) :H = 'Estd',  ;
       auxcot.codprv :H =  ;
       'CodPro', auxcot.nompro :H =  ;
       'Proveedor' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SET RELATION TO
SELECT solcot
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnum_sc = 0
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero Solicitud : '  ;
  GET vperiodo PICTURE '!!'
@ 1, 32 SAY '-' GET vnum_sc  ;
  PICTURE '9999' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_sc) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vperiodo + vnum_sc
     IF  .NOT. FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
RETURN
*
FUNCTION vbusca
vnum_sc = PADL(ALLTRIM(STR(vnum_sc,  ;
          4)), 4, '0')
RETURN .T.
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
     DO pantalla
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
SCATTER MEMVAR
DO CASE
     CASE m.estado = '00'
          DO standby WITH  ;
             ' La Solicitud NO tiene Cotizaci¢n registrada '
          RETURN
     CASE m.estado = '70'
          DO standby WITH  ;
             ' La Solicitud ha sido Devuelta '
          RETURN
     CASE m.estado = '99'
          DO standby WITH  ;
             ' La Solicitud est  Anulada '
          RETURN
ENDCASE
ACTIVATE WINDOW wind_1
DO pantalla
@ 1, 22 GET m.periodo DISABLE
@ 1, 24 SAY '-'
@ 1, 25 GET m.numsc DISABLE
@ 1, 60 GET m.fecsc DISABLE
SELECT auxcot
SET FILTER TO periodo = m.periodo;
.AND. numsc = m.numsc
GOTO TOP
IF EOF()
     DO standby WITH  ;
        'No se tiene registrado a ningun Proveedor '
ENDIF
vcod = 1
SCAN
     vn = STR(vcod, 1)
     m.Codprv&vN = auxcot.codprv
     @ vCod+2, 2 SAY "   Proveedor N§ &vN :  "
     @ vCod+2,22 GET m.Codprv&vN VALID;
valprv()
     READ
     IF LASTKEY() <> 27
          DO WHILE .T.
               ok = trabaja_hi()
               IF LASTKEY() <> 27
                    IF yesno( ;
                       '¨ Conforme ?' ;
                       )
                         ok = .T.
                         EXIT
                    ENDIF
               ELSE
                    IF yesno( ;
                       '¨ Cancela ?' ;
                       )
                         ok = .F.
                         EXIT
                    ENDIF
               ENDIF
          ENDDO
          IF ok .AND. LASTKEY() <>  ;
             27
               SELECT solcot
               GATHER MEMVAR
          ELSE
               SELECT solcot
               EXIT
          ENDIF
     ELSE
          DO standby WITH  ;
             ' Siguiente Proveedor '
     ENDIF
     SELECT auxcot
     vcod = vcod + 1
ENDSCAN
SET FILTER TO
UNLOCK ALL
SELECT solcot
DO vista
RETURN
*
PROCEDURE ingre
SELECT parma
SEEK 'CORRELSOLCOT'
SELECT solcot
SCATTER MEMVAR
DO CASE
     CASE m.estado = '20'
          DO standby WITH  ;
             ' La Solicitud ya tiene Cotizaci¢n registrada '
          RETURN
     CASE m.estado = '70'
          DO standby WITH  ;
             ' La Solicitud ha sido Devuelta '
          RETURN
     CASE m.estado = '99'
          DO standby WITH  ;
             ' La Solicitud est  Anulada '
          RETURN
ENDCASE
DO pantalla
@ 1, 22 GET m.periodo DISABLE
@ 1, 24 SAY '-'
@ 1, 25 GET m.numsc DISABLE
@ 1, 60 GET m.fecsc DISABLE
SELECT auxcot
SET FILTER TO periodo = m.periodo;
.AND. numsc = m.numsc
GOTO TOP
IF EOF()
     DO standby WITH  ;
        'No se tiene registrado a ningun Proveedor '
ENDIF
vcod = 1
SCAN
     vn = STR(vcod, 1)
     m.Codprv&vN = auxcot.codprv
     @ vCod+2, 2 SAY "   Proveedor N§ &vN :  "
     @ vCod+2,22 GET m.Codprv&vN VALID;
valprv()
     READ
     IF LASTKEY() <> 27
          DO WHILE .T.
               ok = trabaja_hi()
               IF LASTKEY() <> 27
                    IF yesno( ;
                       '¨ Conforme los precios ?' ;
                       )
                         EXIT
                    ENDIF
               ELSE
                    IF yesno( ;
                       '¨ Cancela el ingreso ?' ;
                       )
                         ok = .F.
                         EXIT
                    ENDIF
               ENDIF
          ENDDO
          IF ok .AND. LASTKEY() <>  ;
             27
               m.estado = '20'
               SELECT solcot
               GATHER MEMVAR
          ELSE
               SELECT solcot
               EXIT
          ENDIF
     ELSE
          DO standby WITH  ;
             'Proveedor cancelado'
     ENDIF
     SELECT auxcot
     vcod = vcod + 1
ENDSCAN
SET FILTER TO
UNLOCK ALL
SELECT solcot
DO vista
RETURN
*
PROCEDURE trabaja_hi
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°° [*] Selecciona°°°°° [Space Bar] Desselecciona °°°°°° F10->Terminar°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F9 DO VISOBS
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itesc
IF vopcion = 1
     BROWSE  NOAPPEND NODELETE NOMENU;
NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo;
+ m.numsc  WINDOW Wind_9  FIELDS Desped;
     : H= 'Descripci¢n' :39 :W=.F.;
, Cansol      : H= 'Cantid' :P='9,999.999';
:W=.f., unimed      : H= 'Uni';
     :W=.F. :6;
, preuni&vN   : H= 'Pre_&vN'  :P='9,999,999.999';
:V=COSTO():F, valtot&vN   : H= 'Tot_&vN';
 :P='9,999,999.99' :W=.F.
ELSE
     BROWSE  NOAPPEND NODELETE NOMENU;
NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo;
+ m.numsc  WINDOW Wind_9  FIELDS XX=MLINE(DETALLE,1);
    : H= 'Descripci¢n' :64 :W=.F.;
, valtot&vN   : H= 'Tot_&vN' :V=ACT():F;
:P='9,999,999.99'
ENDIF
ON KEY LABEL F10
ON KEY LABEL F9
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT solcot
RETURN
*
FUNCTION act
IF RLOCK()
     replace PREUNI&vN with valtot&vN,cansol;
with 1
ENDIF
RETURN .T.
*
FUNCTION costo
IF RLOCK()
     replace valtot&vN with cansol*preuni&vN
ENDIF
RETURN .T.
*
PROCEDURE elimi
SELECT solcot
DO CASE
     CASE m.estado = '70'
          DO standby WITH  ;
             ' La Solicitud ha sido Devuelta '
          RETURN
     CASE m.estado = '99'
          DO standby WITH  ;
             ' La Solicitud est  Anulada '
          RETURN
ENDCASE
velimina = yesno( ;
           '¨ Desea ELIMINAR los proveedores de esta solicitud ?' ;
           )
IF velimina .AND. (RLOCK() .OR.  ;
   f_lock(1))
     SELECT itesc
     SET FILTER TO itesc.numsc = solcot.numsc
     GOTO TOP
     SCAN
          FOR vcod = 1 TO 6
               vn = STR(vcod, 1)
               replace Preuni&vN with;
0 , Valtot&vN with 0
          ENDFOR
     ENDSCAN
     SET FILTER TO
     SELECT solcot
     vcod = 1
     FOR vcod = 1 TO 6
          vn = STR(vcod, 1)
          replace Codprv&vN with space(4)
     ENDFOR
     REPLACE estado WITH '00'
ENDIF
DO vista
UNLOCK
RETURN
*
FUNCTION agreg_item
SELECT itesc
IF f_appd()
     REPLACE numsc WITH m.numsc,  ;
             codart WITH  ;
             itepec.codart,  ;
             codcad WITH  ;
             itepec.codcad,  ;
             cansol WITH vcanreq,  ;
             unimed WITH  ;
             itepec.coduni,  ;
             desped WITH  ;
             itepec.descri
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION marca
DO CASE
     CASE llave = '*'
          IF RLOCK()
               REPLACE llave WITH  ;
                       '*',  ;
                       estado  ;
                       WITH '20'
          ENDIF
          RETURN .T.
     CASE llave = ' '
          IF RLOCK()
               REPLACE llave WITH  ;
                       ' ',  ;
                       estado  ;
                       WITH '  '
          ENDIF
          RETURN .T.
ENDCASE
RETURN .F.
*
PROCEDURE lista
SELECT solcot
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     vnum_sc = solcot.numsc
     vperiodo = RIGHT(DTOC(DATE()),  ;
                2)
     ACTIVATE WINDOW standby
     @ 1, 01 SAY  ;
       'Ingrese N£mero Solicitud : '  ;
       GET vperiodo PICTURE '!!'
     @ 1, 32 SAY '-' GET vnum_sc  ;
       PICTURE '!!!!'
     READ
     DEACTIVATE WINDOW standby
     IF LASTKEY() = 27
          DO vista
          RETURN
     ENDIF
     SEEK vperiodo + vnum_sc
     IF  .NOT. FOUND()
          GOTO vtemp
          DO revis
     ENDIF
     av = numsc
     bv = periodo
     SET RELATION TO periodo + numsc INTO;
itesc
     SET RELATION TO periodo + numsc INTO;
auxcot ADDITIVE
     SET SKIP TO itesc
     SET FILTER TO solcot.numsc = av;
.AND. solcot.periodo = bv
     SET MEMOWIDTH TO 112
     IF vopcion = 1
          DO reporte WITH 2,  ;
             'cUcOCO',  ;
             ' Cuadro Comparativo de Cotizaciones '
     ELSE
          DO reporte WITH 2,  ;
             'cUcOCOS',  ;
             ' Cuadro Comparativo de Cotizaciones '
     ENDIF
     SET FILTER TO
     SET RELATION TO
     SET MEMOWIDTH TO 56
ENDIF
SELECT solcot
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
FUNCTION valprv
PRIVATE xx, vfun
vfun = .F.
m.codprv&vN = iif( empty(m.codprv&vN),m.codprv&vN,padl(alltrim(m.codprv&vN),4,'0'))
xx = val_prv( m.codprv&vN,.t.,vCod+2,27)
IF xx
     RETURN .T.
ENDIF
RETURN vfun
*
FUNCTION valsc
PARAMETER vnumsc
PRIVATE vfun
vfun = .T.
m.numsc = PADL(ALLTRIM(STR(vnumsc,  ;
          4)), 4, '0')
IF m.numsc = '0000' .OR.  ;
   EMPTY(m.numsc)
     vfun = .F.
ENDIF
RETURN vfun
*
FUNCTION valart
PARAMETER _cod
PRIVATE xx, vfun
vfun = .F.
xx = val_art(codart,.F.)
IF xx
     SELECT itepec
     REPLACE coduni WITH  ;
             produ.coduni, preuni  ;
             WITH produ.preuni
     vfun = .T.
ENDIF
RETURN vfun
*
FUNCTION observa
valias = ALIAS()
SELECT itesc
SET MEMOWIDTH TO 56
ON KEY LABEL F10 KEYBOARD CHR(23)
IF  .NOT. WEXIST('OBSERVA')
     DEFINE WINDOW observa FROM  ;
            05, 18 TO 18, 61  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Detalle Pecosa ±'  ;
            FOOTER  ;
            ' ° ®F10¯ Graba ° '  ;
            DOUBLE COLOR SCHEME  ;
            1
ENDIF
IF WVISIBLE('OBSERVA')
     ACTIVATE WINDOW SAME observa
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              observa
ENDIF
MODIFY MEMO detalle WINDOW  ;
       observa
IF  .NOT. WVISIBLE('OBSERVA')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado. No graba el OBSERVA '
ENDIF
SELECT (valias)
RETURN .T.
*
FUNCTION visobs
valias = ALIAS()
SELECT itesc
IF  .NOT. WEXIST('OBSERVA')
     DEFINE WINDOW observa FROM  ;
            05, 18 TO 18, 61  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Detalle Pecosa ±'  ;
            FOOTER  ;
            ' ° ®F10¯ Graba ° '  ;
            DOUBLE COLOR SCHEME  ;
            1
ENDIF
IF WVISIBLE('OBSERVA')
     ACTIVATE WINDOW SAME observa
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              observa
ENDIF
MODIFY MEMO detalle NOEDIT WINDOW  ;
       observa
IF  .NOT. WVISIBLE('OBSERVA')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
RETURN .T.
*

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
         ORDER SolCot2
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
USE IN 7 AuxCot ALIAS auxcot  ;
    ORDER Auxcot1
USE IN 8 Solser ALIAS solser  ;
    ORDER Solser2
USE IN 0 USUARIO ALIAS usu ORDER  ;
    USUARIO1
SET MEMOWIDTH TO 112
PUBLIC just1, just2, just3, just4,  ;
       just5, just6, obs1, obs2,  ;
       obs3, obs4, obs5, obs6
vmens01 = ' Cuadro Comparativo de Cotizaciones: REGISTRO '
vmens02 = ' Registro del Cuadro Comparativo de Cotizaciones '
vmens04 = 'Dicho Solicitud de Cotizaci¢n no fue encontrada'
vmens05 = 'No existe Solicitud de Cotizaci¢n anterior'
vmens06 = 'No existe Solicitud de Cotizaci¢n siguiente'
vmens07 = '¨ Desea ELIMINAR ‚ste Cuadro Comparativo de Cotizaciones ?'
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
       'Detalle: Precios Netos '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_9 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       ' ®Cuadro Comparativo de Cotizaci¢nes¯       ®F7¯ Justificaci¢n/Obs '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_8 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       ' ®Cuadro Comparativo de Cotiz.¯     ®F7¯ Justificaci¢n,Observaci¢n'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW jusobs FROM 10, 02  ;
       TO 15, 77 TITLE  ;
       ' Acta de Otorgamiento  '  ;
       DOUBLE
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
@ 1, 2 SAY ' N£mero Solicitud : '
@ 1, 40 SAY  ;
  '            Fecha : '
RETURN
*
PROCEDURE vista
SELECT solcot
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@ 0, 60 SAY IIF(m.estado = '00',  ;
  'Pendiente', IIF(m.estado =  ;
  '20', 'CON COTIZ.',  ;
  IIF(m.estado = '99', 'Anulada ',  ;
  IIF(m.estado = '50', 'Atendido',  ;
  '        '))))
@ 1, 22 SAY m.numsc
@ 1, 60 SAY m.fecsc
@ 2, 40 SAY IIF(EMPTY(m.numccc),  ;
  '                   ',  ;
  'Cuadro Comparativo:')
@ 2, 60 SAY IIF(EMPTY(m.numccc),  ;
  '    ', m.numccc)
DO proveedor
DO vista_hijo
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT itesc
IF vopcion = 1
     BROWSE NOOPTIMIZE FIELDS  ;
            codart :H = 'C¢digo'  ;
            :W = .F., numord :H =  ;
            'OR' :W = .F., numpec  ;
            :H = 'Pecs' :W = .F.,  ;
            desped :H =  ;
            'Descripci¢n' : 40 :W =  ;
            .F., unimed :H =  ;
            'UniMd' :W = .F. : 4,  ;
            cansol :H = 'Cant' :P =  ;
            '9,999.999' :W = .F.,  ;
            codprvx :H = 'Prov'  ;
            :W = .F. NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_2 KEY  ;
            m.periodo + m.numsc  ;
            TIMEOUT 0.001   ;
            NOREFRESH
ELSE
     BROWSE NOOPTIMIZE FIELDS zz =  ;
            MLINE(detalle, 1) :H =  ;
            'Descripci¢n' : 70 :W =  ;
            .F., codprvx :H =  ;
            'Prov' :W = .F.  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.periodo +  ;
            m.numsc TIMEOUT 0.001   ;
            NOREFRESH
ENDIF
SELECT solcot
RETURN
*
PROCEDURE vista_prov
DO CASE
     CASE estado = '70'
          DO standby WITH  ;
             ' La Solicitud ha sido Devuelta '
          RETURN
     CASE estado = '99'
          DO standby WITH  ;
             ' La Solicitud est  Anulada '
          RETURN
ENDCASE
HIDE POPUP ALL
SELECT itesc
IF vopcion = 1
     BROWSE FIELDS codart :H =  ;
            'C¢digo', desped :H =  ;
            'Descripci¢n' : 46,  ;
            unimed :H = 'UniMd' :  ;
            5, cansol :H = 'Cant'  ;
            :P = '9,999.999',  ;
            codprvx :H = 'Prov'  ;
            :W = .F., a = '³' :H =  ;
            '*' :W = .F., preuni1  ;
            :H = 'Unitario1' :P =  ;
            '9,999,999.999',  ;
            valtot1 :H =  ;
            'Total 1' :P =  ;
            '9,999,999.99', b =  ;
            '³' :H = '*' :W = .F.,  ;
            preuni2 :H =  ;
            'Unitario2' :P =  ;
            '9,999,999.999',  ;
            valtot2 :H =  ;
            'Total 2' :P =  ;
            '9,999,999.999', c =  ;
            '³' :H = '*' :W = .F.,  ;
            preuni3 :H =  ;
            'Unitario3' :P =  ;
            '9,999,999.999',  ;
            valtot3 :H =  ;
            'Total 3' :P =  ;
            '9,999,999.999', d =  ;
            '³' :H = '*' :W = .F.,  ;
            preuni4 :H =  ;
            'Unitario4' :P =  ;
            '9,999,999.999',  ;
            valtot4 :H =  ;
            'Total 4' :P =  ;
            '9,999,999.999', e =  ;
            '³' :H = '*' :W = .F.,  ;
            preuni5 :H =  ;
            'Unitario5' :P =  ;
            '9,999,999.999',  ;
            valtot5 :H =  ;
            'Total 5' :P =  ;
            '9,999,999.999', f =  ;
            '³' :H = '*' :W = .F.,  ;
            preuni6 :H =  ;
            'Unitario6' :P =  ;
            '9,999,999.999',  ;
            valtot6 :H =  ;
            'Total 6' :P =  ;
            '9,999,999.999'  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_9  ;
            KEY m.periodo +  ;
            m.numsc
ELSE
     BROWSE FIELDS codart :H =  ;
            'C¢digo', dessc :H =  ;
            'Descripci¢n' : 46,  ;
            unimed :H = 'UniMd' :  ;
            5, cansol :H = 'Cant'  ;
            :P = '9,999.999',  ;
            codprvx :H = 'Prov'  ;
            :W = .F., a = '³' :H =  ;
            '*' :W = .F., preuni1  ;
            :H = 'Unitario1' :P =  ;
            '9,999,999.999',  ;
            valtot1 :H =  ;
            'Total 1' :P =  ;
            '9,999,999.99', b =  ;
            '³' :H = '*' :W = .F.,  ;
            preuni2 :H =  ;
            'Unitario2' :P =  ;
            '9,999,999.999',  ;
            valtot2 :H =  ;
            'Total 2' :P =  ;
            '9,999,999.99', c =  ;
            '³' :H = '*' :W = .F.,  ;
            preuni3 :H =  ;
            'Unitario3' :P =  ;
            '9,999,999.999',  ;
            valtot3 :H =  ;
            'Total 3' :P =  ;
            '9,999,999.99', d =  ;
            '³' :H = '*' :W = .F.,  ;
            preuni4 :H =  ;
            'Unitario4' :P =  ;
            '9,999,999.999',  ;
            valtot4 :H =  ;
            'Total 4' :P =  ;
            '9,999,999.99', e =  ;
            '³' :H = '*' :W = .F.,  ;
            preuni5 :H =  ;
            'Unitario5' :P =  ;
            '9,999,999.999',  ;
            valtot5 :H =  ;
            'Total 5' :P =  ;
            '9,999,999.99', f =  ;
            '³' :H = '*' :W = .F.,  ;
            preuni6 :H =  ;
            'Unitario6' :P =  ;
            '9,999,999.999',  ;
            valtot6 :H =  ;
            'Total 6' :P =  ;
            '9,999,999.99' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_9 KEY  ;
            m.periodo + m.numsc
ENDIF
SELECT solcot
DO vista
RETURN
*
PROCEDURE revis
SELECT solcot
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SET RELATION TO periodo + numsc INTO itesc
SET SKIP TO itesc
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
IF vopcion = 1
     BROWSE FIELDS numsc :H =  ;
            ' N§ ', fecsc :H =  ;
            'Fecha', gg =  ;
            IIF(EMPTY(itesc.codprvx),  ;
            '....',  ;
            itesc.codprvx) :H =  ;
            'PROVE', hh =  ;
            IIF(EMPTY(itesc.codprvx),  ;
            '....',  ;
            itesc.preunix) :H =  ;
            'PreUni' :P =  ;
            '9,999,999.999',  ;
            itesc.cansol :H =  ;
            'Cantid' :P =  ;
            '9,999.999',  ;
            itesc.desped :H =  ;
            'Articulo ' : 35,  ;
            itesc.unimed :H =  ;
            'Unid' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_0
ELSE
     BROWSE FIELDS numsc :H =  ;
            ' N§ ', fecsc :H =  ;
            'Fecha', gg =  ;
            IIF(EMPTY(itesc.codprvx),  ;
            '....',  ;
            itesc.codprvx) :H =  ;
            'PROVE', hh =  ;
            IIF(EMPTY(itesc.codprvx),  ;
            '....',  ;
            itesc.preunix) :H =  ;
            'PreUni' :P =  ;
            '9,999,999.999', qq =  ;
            MLINE(itesc.detalle,  ;
            1) :H = 'Servicio ' :  ;
            56 NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            WINDOW wind_0
ENDIF
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
SET RELATION TO
ON KEY LABEL F10
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
     DO pantalla
     DO vista
ENDIF
RETURN
*
PROCEDURE corri
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SELECT solcot
DO CASE
     CASE estado = '00'
          DO standby WITH  ;
             ' La Solicitud aun no Registra Proveedores '
          RETURN
     CASE estado = '20'
          DO standby WITH  ;
             ' La Solicitud no esta Adjudicada '
          RETURN
     CASE estado = '70'
          DO standby WITH  ;
             ' La Solicitud ha sido Devuelta '
          RETURN
     CASE estado = '99'
          DO standby WITH  ;
             ' La Solicitud est  Anulada '
          RETURN
ENDCASE
DO pantalla
SCATTER MEMVAR
@ 1, 22 GET m.numsc DISABLE
@ 1, 60 GET m.fecsc DISABLE
DO proveedor
DO WHILE .T.
     ok = trabaja_hi()
     IF LASTKEY() <> 27
          IF yesno( ;
             '¨ Confirme el ingreso ?' ;
             )
               DO agrjusyobs
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
IF ok .AND. LASTKEY() <> 27
     m.estado = '50'
     ACTIVATE WINDOW standby
     @ 1, 1 SAY  ;
       ' N§ Cuadro Comparativo -> : '  ;
       GET m.numccc DISABLE
     READ
     = INKEY(0)
     DEACTIVATE WINDOW standby
     SELECT solcot
     m.user = vuser_id
     m.user_fc = DATE()
     m.user_tp = 'C'
     GATHER MEMVAR
ENDIF
UNLOCK ALL
SELECT solcot
DO vista
RETURN
*
PROCEDURE ingre
SELECT solcot
DO CASE
     CASE estado = '00'
          DO standby WITH  ;
             ' La Solicitud aun no Registra Proveedores '
          RETURN
     CASE estado = '50'
          DO standby WITH  ;
             ' La Solicitud ha sido Atendida '
          RETURN
     CASE estado = '70'
          DO standby WITH  ;
             ' La Solicitud ha sido Devuelta '
          RETURN
     CASE estado = '99'
          DO standby WITH  ;
             ' La Solicitud est  Anulada '
          RETURN
ENDCASE
DO pantalla
SCATTER MEMVAR
@ 1, 22 GET m.numsc DISABLE
@ 1, 60 GET m.fecsc DISABLE
DO proveedor
DO WHILE .T.
     ok = trabaja_hi()
     IF LASTKEY() <> 27
          IF yesno( ;
             '¨ Confirme el ingreso ?' ;
             )
               DO agrjusyobs
               EXIT
          ENDIF
     ELSE
          DO standby WITH  ;
             ' Cancelado el Ingreso ..'
          ok = .F.
          EXIT
     ENDIF
ENDDO
IF ok .AND. LASTKEY() <> 27
     m.estado = '50'
     SELECT parma
     SEEK 'CORRELCCC'
     = valccc(parma.nument + 1)
     ACTIVATE WINDOW standby
     @ 1, 1 SAY  ;
       ' N§ Cuadro Comparativo -> : '  ;
       GET m.numccc DISABLE
     READ
     = INKEY(0)
     DEACTIVATE WINDOW standby
     SELECT solcot
     SEEK m.periodo + m.numsc
     m.user = vuser_id
     m.user_fc = DATE()
     m.user_tp = 'I'
     IF FOUND()
          GATHER MEMVAR
     ELSE
          IF f_appd()
               GATHER MEMVAR
          ENDIF
          UNLOCK
     ENDIF
     SELECT parma
     SEEK 'CORRELCCC'
     REPLACE nument WITH nument +  ;
             1
     SELECT solcot
ENDIF
UNLOCK ALL
SELECT solcot
DO vista
RETURN
*
PROCEDURE agrjusyobs
SET CONFIRM OFF
DEFINE WINDOW w_just FROM 03, 10  ;
       TO 22, 69 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW w_just
@ 01, 05 SAY  ;
  'Ingrese Justificaci¢n'
@ 02, 01 GET m.just1
@ 03, 01 GET m.just2
@ 04, 01 GET m.just3
@ 05, 01 GET m.just4
@ 06, 01 GET m.just5
@ 07, 01 GET m.just6
@ 09, 05 SAY  ;
  'Ingrese Observaci¢n'
@ 10, 01 GET m.obs1 FUNCTION  ;
  'S40'
@ 11, 01 GET m.obs2 FUNCTION  ;
  'S40'
@ 12, 01 GET m.obs3 FUNCTION  ;
  'S40'
@ 13, 01 GET m.obs4 FUNCTION  ;
  'S40'
@ 14, 01 GET m.obs5 FUNCTION  ;
  'S40'
@ 15, 01 GET m.obs6 FUNCTION  ;
  'S40'
READ VALID val_read()
DEACTIVATE WINDOW w_just
RELEASE WINDOW w_just
SET CONFIRM ON
RETURN
*
PROCEDURE agrobs
RETURN
*
FUNCTION trabaja_hi
PRIVATE vfun
ACTIVATE SCREEN
vfun = .T.
PUBLIC vprv
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° F10->Terminar°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itesc
IF vopcion = 1
     BROWSE NOOPTIMIZE FIELDS  ;
            codart :H = 'C¢digo'  ;
            :W = .F., numord :H =  ;
            'OR' :W = .F., numpec  ;
            :H = 'Pecs' :W = .F.,  ;
            desped :H =  ;
            'Descripci¢n' : 42 :W =  ;
            .F., unimed :H =  ;
            'UniMd' : 4 :W = .F.,  ;
            cansol :H = 'Cant' :P =  ;
            '9,999.999' :W = .F.,  ;
            numprv :H = 'P' :P =  ;
            'X' :V = escoge() :F,  ;
            nomprox =  ;
            val_prv(codprvx) :F  ;
            :H = ' PROVEEDOR ' :  ;
            30 :W = .F., a = '³'  ;
            :H = '*' :W = .F.,  ;
            preuni1 :H =  ;
            'Unitario1' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot1 :H =  ;
            'Total 1' :P =  ;
            '9,999,999.99' :W =  ;
            .F., b = '³' :H = '*'  ;
            :W = .F., preuni2 :H =  ;
            'Unitario2' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot2 :H =  ;
            'Total 2' :P =  ;
            '9,999,999.99' :W =  ;
            .F., c = '³' :H = '*'  ;
            :W = .F., preuni3 :H =  ;
            'Unitario3' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot3 :H =  ;
            'Total 3' :P =  ;
            '9,999,999.99' :W =  ;
            .F., d = '³' :H = '*'  ;
            :W = .F., preuni4 :H =  ;
            'Unitario4' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot4 :H =  ;
            'Total 4' :P =  ;
            '9,999,999.99' :W =  ;
            .F., e = '³' :H = '*'  ;
            :W = .F., preuni5 :H =  ;
            'Unitario5' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot5 :H =  ;
            'Total 5' :P =  ;
            '9,999,999.99' :W =  ;
            .F., f = '³' :H = '*'  ;
            :W = .F., preuni6 :H =  ;
            'Unitario6' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot6 :H =  ;
            'Total 6' :P =  ;
            '9,999,999.99' :W =  ;
            .F. NOMENU NOAPPEND  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_9 KEY  ;
            m.periodo + m.numsc  ;
            NOREFRESH
ELSE
     BROWSE NOOPTIMIZE FIELDS ss =  ;
            MLINE(detalle, 1) :H =  ;
            'Descripci¢n' : 70 :W =  ;
            .F., numprv :H = 'P'  ;
            :P = 'X' :V =  ;
            escoge() :F, nomprox =  ;
            val_prv(codprvx) :F  ;
            :H = ' PROVEEDOR ' :  ;
            30 :W = .F., a = '³'  ;
            :H = '*' :W = .F.,  ;
            preuni1 :H =  ;
            'Unitario1' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot1 :H =  ;
            'Total 1' :P =  ;
            '9,999,999.99' :W =  ;
            .F., b = '³' :H = '*'  ;
            :W = .F., preuni2 :H =  ;
            'Unitario2' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot2 :H =  ;
            'Total 2' :P =  ;
            '9,999,999.99' :W =  ;
            .F., c = '³' :H = '*'  ;
            :W = .F., preuni3 :H =  ;
            'Unitario3' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot3 :H =  ;
            'Total 3' :P =  ;
            '9,999,999.99' :W =  ;
            .F., d = '³' :H = '*'  ;
            :W = .F., preuni4 :H =  ;
            'Unitario4' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot4 :H =  ;
            'Total 4' :P =  ;
            '9,999,999.99' :W =  ;
            .F., e = '³' :H = '*'  ;
            :W = .F., preuni5 :H =  ;
            'Unitario5' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot5 :H =  ;
            'Total 5' :P =  ;
            '9,999,999.99' :W =  ;
            .F., f = '³' :H = '*'  ;
            :W = .F., preuni6 :H =  ;
            'Unitario6' :P =  ;
            '9,999,999.999' :W =  ;
            .F., valtot6 :H =  ;
            'Total 6' :P =  ;
            '9,999,999.99' :W =  ;
            .F. NOMENU NOAPPEND  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_9 KEY  ;
            m.periodo + m.numsc  ;
            NOREFRESH
ENDIF
ON KEY LABEL F10
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT solcot
RETURN vfun
*
FUNCTION escoge
PRIVATE vprv, vpre, vkei
vprv = 'SOLCOT.codprv' + numprv
vpre = 'Preuni' + numprv
vkei = itesc.periodo +  ;
       itesc.numsc + itesc.codfte +  ;
       itesc.codart +  ;
       itesc.numord +  ;
       itesc.numpec
IF NUMPRV $ '12345' AND &vPrv#'    '
     REPLACE CodPrvX WITH &vPrv,;
 PreUniX WITH &vPre
ELSE
     REPLACE codprvx WITH '    ',  ;
             numprv WITH ' ',  ;
             preunix WITH 0
ENDIF
IF vopcion = 1
     SELECT itepec
     SET ORDER TO ITEPEC11
     SEEK vkei
     IF FOUND()
          IF RLOCK()
               REPLACE codprv  ;
                       WITH  ;
                       itesc.codprvx,  ;
                       preuni  ;
                       WITH  ;
                       itesc.preunix
          ENDIF
     ELSE
          IF  .NOT.  ;
              EMPTY(itesc.numprv)
               DO standby WITH  ;
                  'Error, no grab¢ en Pecosa..'
          ENDIF
     ENDIF
     SET ORDER TO ITEPEC2
ELSE
     SELECT solser
     SEEK m.periodo + itesc.numsc
     IF FOUND()
          IF RLOCK()
               REPLACE codprv  ;
                       WITH  ;
                       itesc.codprvx,  ;
                       valtot  ;
                       WITH  ;
                       itesc.preunix
          ENDIF
          UNLOCK
     ELSE
          DO standby WITH  ;
             'Error..'
     ENDIF
ENDIF
SELECT itesc
ON KEY LABEL F7 DO JUSOBS
RETURN .T.
*
PROCEDURE jusobs
IF  .NOT. EMPTY(itesc.numprv)
     vnum = ALLTRIM(itesc.numprv)
     ACTIVATE WINDOW jusobs
     @ 0, 1 SAY 'PROVEEDOR N§ ' +  ;
       vnum + ':'
     @ 1,1 SAY 'Justificaci¢n :' get m.just&vNum;
PICTURE '@S56'
     @ 2,1 SAY '  Observaci¢n :' get m.obs&vNum;
 PICTURE '@S56'
     READ
     DEACTIVATE WINDOW jusobs
ENDIF
ON KEY LABEL F7
RETURN
*
FUNCTION xactualiza
PRIVATE zx
zx = ALIAS()
IF vopcion = 1
     SELECT itepec
     SET ORDER TO ITEPEC11
     SEEK itesc.periodo +  ;
          itesc.numsc +  ;
          itesc.codfte +  ;
          itesc.codart +  ;
          ALLTRIM(itesc.numord)
     IF FOUND()
          IF RLOCK()
               REPLACE codprv  ;
                       WITH  ;
                       itesc.codprvx,  ;
                       preuni  ;
                       WITH  ;
                       itesc.preunix
          ENDIF
          SET ORDER TO ITEPEC2
     ELSE
          IF  .NOT.  ;
              EMPTY(itesc.numprv)
               DO standby WITH  ;
                  'Error, no grab¢ en Pecosa..'
          ENDIF
     ENDIF
ELSE
     SELECT solser
     SEEK m.periodo + itesc.numsc
     IF FOUND()
          IF RLOCK()
               REPLACE codprv  ;
                       WITH  ;
                       itesc.codprvx,  ;
                       valtot  ;
                       WITH  ;
                       itesc.preunix
          ENDIF
          UNLOCK
     ELSE
          DO standby WITH  ;
             'Error..'
     ENDIF
ENDIF
sele &zx
RETURN .T.
*
FUNCTION borrapec
SELECT itepec
SET FILTER TO itepec.codart = itesc.codart;
.AND. itepec.numsc = itesc.numsc
SET ORDER TO 1
GOTO TOP
SCAN
     IF RLOCK()
          REPLACE codprv WITH  ;
                  '    ', preuni  ;
                  WITH 0
     ENDIF
ENDSCAN
SET ORDER TO 2
RETURN .T.
*
FUNCTION agregaoc
zx = ALIAS()
SELECT parma
SEEK 'CORREL' + 'ORDCOM'
vcorr = parma.nument + 1
SELECT itepec
SET FILTER TO itepec.codart = itesc.codart;
.AND. itepec.numsc = itesc.numsc
SET ORDER TO 1
GOTO TOP
SCAN
     vcanreq = 0
     vca1 = codart + codcad
     vca2 = codart + codcad
     DO WHILE vca1=vca2
          vcanreq = vcanreq +  ;
                    canreq
          SKIP
          vca1 = codart + codcad
     ENDDO
     SKIP -1
     DO agreg_item
ENDSCAN
SCAN
     IF RLOCK()
          REPLACE codprv WITH  ;
                  itesc.codprvx,  ;
                  preuni WITH  ;
                  itesc.preunix
     ENDIF
ENDSCAN
SET ORDER TO 2
sele &zx
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
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
DO CASE
     CASE estado = '00'
          DO standby WITH  ;
             ' La Solicitud aun no Registra Proveedores '
          RETURN
     CASE estado = '20'
          DO standby WITH  ;
             ' La Solicitud NO ha sido Atendida '
          RETURN
     CASE estado = '70'
          DO standby WITH  ;
             ' La Solicitud ha sido Devuelta '
          RETURN
     CASE estado = '99'
          DO standby WITH  ;
             ' La Solicitud est  Anulada '
          RETURN
ENDCASE
velimina = yesno( ;
           '¨ Desea ELIMINAR Adjudicaci¢n de ‚sta Solicitud ?' ;
           )
IF velimina .AND. (RLOCK() .OR.  ;
   f_lock(1))
     DO borrapec
     SELECT itesc
     SCAN FOR itesc.numsc =  ;
          solcot.numsc
          REPLACE numprv WITH ' ',  ;
                  codprvx WITH  ;
                  '    ', preunix  ;
                  WITH 0,  ;
                  solcot.estado  ;
                  WITH '20'
     ENDSCAN
     SELECT solcot
     DO vista
ENDIF
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
PROCEDURE lista
SELECT solcot
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lisadj
ENDIF
SELECT solcot
GOTO vtemp
DO vista
RETURN
*
PROCEDURE lisadj
vtemo = RECNO()
DEFINE WINDOW lis FROM 5, 15 TO  ;
       19, 65 FLOAT TITLE  ;
       'Listado Adjudicaci¢n'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtiplis
vcli = SPACE(4)
van = SPACE(2)
@ 01, 01 SAY  ;
  'Total Solicitudes : ' GET  ;
  vtocli SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,3,22) .AND.  ;
  IIF(vtocli = 2, assig(), .T.)
@ 03, 01 SAY  ;
  '        Solicitud : '
@ 03, 22 GET vcli PICTURE '!!!!'  ;
  WHEN vtocli = 2
@ 03, 26 SAY '.'
@ 03, 27 GET van PICTURE '!!'  ;
  VALID valsol() WHEN vtocli = 2
@ 06, 01 SAY  ;
  '      Listado por : ' GET  ;
  vtiplis FUNCTION  ;
  '^ Art¡culo;Otorgamiento'
@ 12, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     SET RELATION TO periodo + numsc INTO;
itesc
     SET SKIP TO itesc
     SET FILTER TO estado = '50';
.AND. IIF(vtocli = 1,;
.T., numsc = vcli)
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtiplis = 1,  ;
               ' por Art¡culo ',  ;
               ' por Acta de Otorgamiento ' ;
               )
     IF  .NOT. EOF()
          IF vopcion = 1
               IF vtiplis = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'LisAdj',  ;
                       ' Proveedores Adjudicados '
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'LisAdj1',  ;
                       ' Acta de Otorgamiento '
               ENDIF
          ELSE
               IF vtiplis = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'LisAdjS',  ;
                       ' Proveedores Adjudicados '
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'LisAdj2',  ;
                       ' Acta de Otorgamiento '
               ENDIF
          ENDIF
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     SELECT solcot
     SET RELATION TO
ENDIF
RETURN
*
FUNCTION assig
vcli = solcot.numsc
van = solcot.periodo
RETURN .T.
*
PROCEDURE valsol
SELECT solcot
SET RELATION TO periodo + numsc INTO itesc
SET SKIP TO itesc
vtemp = RECNO()
ON KEY LABEL F10 KEYBOARD CHR(23)
SEEK van + vcli
IF  .NOT. FOUND()
     IF vopcion = 1
          BROWSE FIELDS numsc :H =  ;
                 ' N§ ', fecsc :H =  ;
                 'Fecha', tipdoc  ;
                 :H = 'DOC', ess =  ;
                 IIF(estado =  ;
                 '00', 'Pend',  ;
                 IIF(estado =  ;
                 '20', 'C/c ',  ;
                 IIF(estado =  ;
                 '99', 'Anul',  ;
                 IIF(estado =  ;
                 '50', 'Aten',  ;
                 '    ')))) :H =  ;
                 'Estd',  ;
                 itesc.desped :H =  ;
                 'Articulo ' : 36,  ;
                 itesc.unimed :H =  ;
                 'Unid',  ;
                 itesc.cansol :H =  ;
                 'Cantid' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 wind_0
     ELSE
          BROWSE FIELDS numsc :H =  ;
                 ' N§ ', fecsc :H =  ;
                 'Fecha', tipdoc  ;
                 :H = 'DOC', ess =  ;
                 IIF(estado =  ;
                 '00', 'Pend',  ;
                 IIF(estado =  ;
                 '20', 'C/c ',  ;
                 IIF(estado =  ;
                 '99', 'Anul',  ;
                 IIF(estado =  ;
                 '50', 'Aten',  ;
                 '    ')))) :H =  ;
                 'Estd', dd =  ;
                 MLINE(itesc.detalle,  ;
                 1) :H =  ;
                 'Servicio ' : 36,  ;
                 itesc.unimed :H =  ;
                 'Unid',  ;
                 itesc.cansol :H =  ;
                 'Cantid' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 wind_0
     ENDIF
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     IF LASTKEY() = 27
          GOTO vtemp
     ENDIF
ENDIF
vcli = numsc
van = periodo
SELECT solcot
ON KEY LABEL F10
SET RELATION TO
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
PROCEDURE proveedor
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
RETURN
*
FUNCTION buscar
RETURN .T.
vnum = ALLTRIM(itesc.numprv)
just&vNum=Itesc.actjusx
obs&vNum=Itesc.observx
RETURN ' '
*
FUNCTION valccc
PARAMETER vcodccc
PRIVATE vfun
vfun = .T.
m.numccc = PADL(ALLTRIM(STR(vcodccc,  ;
           4)), 4, '0')
IF m.numccc = '0000' .OR.  ;
   EMPTY(m.numccc)
     vfun = .F.
ENDIF
RETURN vfun
*
FUNCTION vusua
PARAMETER csys
PRIVATE ali
ali = ALIAS()
vkey = ALLTRIM(csys)
vfun = ' '
IF  .NOT. EMPTY(vkey)
     SELECT usu
     SEEK vkey
     vfun = IIF(FOUND(), nombre,  ;
            '   ')
     SELECT (ali)
ENDIF
RETURN vfun
*

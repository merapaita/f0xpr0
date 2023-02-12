PARAMETER vopcion
SET EXCLUSIVE OFF
USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
IF vopcion = 1
     USE IN 2 Solcot ALIAS solcot  ;
         ORDER Solcot1
     USE IN 6 AuxCot ALIAS auxcot  ;
         ORDER Auxcot1
ELSE
     USE IN 2 Solcot ALIAS solcot  ;
         ORDER SolCot2
     USE IN 6 AuxCot ALIAS auxcot  ;
         ORDER Auxcot2
ENDIF
USE IN 3 Itesc ALIAS itesc ORDER  ;
    Itesc1
USE IN 4 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 5 Itepec ALIAS itepec  ;
    ORDER ItePec1
USE IN 7 Promae ALIAS promae  ;
    ORDER promae1
USE IN 8 Solser ALIAS solser  ;
    ORDER Solser1
USE IN 9 IteArt ALIAS iteart  ;
    ORDER Iteart3
USE IN 10 Pecosa ALIAS pecosa  ;
    ORDER Pecosa1
USE IN 19 USUARIO ALIAS usu ORDER  ;
    USUARIO1
SET MEMOWIDTH TO 56
vmens01 = ' Solicitud de Cotizaciones : REVISION '
vmens02 = 'Registro de Solicitud de Cotizaci¢n'
vmens04 = 'Dicho Solicitud de Cotizaci¢n no fue encontrada'
vmens05 = 'No existe Solicitud de Cotizaci¢n anterior'
vmens06 = 'No existe Solicitud de Cotizaci¢n siguiente'
vmens07 = '¨ Desea ANULAR ‚sta Solicitud de Cotizaci¢n ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Solicitud de Cotizaci¢n ha sido anulada'
vmens10 = 'La Solicitud de Cotizaci¢n ya fue atendida'
vmens11 = 'La Solicitud de Cotizaci¢n ha sido devuelta'
vesc = SET('ESCAPE')
SET ESCAPE OFF
SELECT solcot
GOTO BOTTOM
IF vopcion = 2
     SET FILTER TO IIF(vflag = '*',;
.T., coddep = SUBSTR(vcoddep, 1, vnumdep))
ENDIF
SCATTER BLANK MEMVAR
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
PROCEDURE vista_deta
IF vopcion = 1
     DO vis_det
ELSE
     DO vis_det1
ENDIF
*
PROCEDURE inicia
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 09, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 10, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle:S/C    ®F2¯ Muestra Proveedores   ®F9¯ Detalle Item '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2a FROM 10, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle:S/C    ®F9¯ Agregar Detalle   ®Esc¯ Sale '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 11, 05  ;
       TO 21, 74 TITLE  ;
       ' Registro de Proveedores '  ;
       FOOTER  ;
       '®F2¯ Muestra Proveedores'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_4 FROM 20, 50  ;
       TO 22, 78 COLOR SCHEME 10
DEFINE WINDOW wind_5 FROM 10, 15  ;
       TO 23, 79 TITLE  ;
       'Detalle:S/C    ' DOUBLE  ;
       COLOR SCHEME 10
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
DEFINE PAD anula OF mmenu PROMPT  ;
       'a\<Nula  ' AT 24, 54
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
ON SELECTION PAD anula OF mmenu DO anula
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_1
CLEAR
@ 1, 2 SAY ' N£mero Solicitud :'
@ 1, 40 SAY '            Fecha :'
@ 3, 2 SAY '       Referencia :'
@ 4, 2 SAY '          Destino :'
@ 5, 2 SAY '    Observaciones :'
RETURN
*
PROCEDURE vista
SELECT solcot
IF EOF()
     DO pantalla
     RETURN
ENDIF
ON KEY LABEL F2 DO vista_aux
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
IF vopcion = 2
ENDIF
@ 3, 22 SAY m.observa
@ 4, 22 SAY m.lugar
@ 5, 22 SAY m.obssc1
@ 6, 22 SAY m.obssc2
IF vopcion = 1
     DO vis_hijo
ELSE
     DO vis_hijo1
ENDIF
RETURN
*
PROCEDURE vis_hijo
HIDE POPUP ALL
SELECT itesc
ON KEY LABEL F9 DO VISTA_DETA
IF vopcion = 1
     BROWSE NOOPTIMIZE FIELDS  ;
            numpec :H = 'Pecs',  ;
            codfte :H = 'Fte',  ;
            codart :H = 'C¢digo',  ;
            cansol :H = 'Cant.'  ;
            :P = '999,999.999',  ;
            unimed :H = 'Uni' :W =  ;
            .F. : 5, desped :H =  ;
            'Descripci¢n' : 58 :W =  ;
            .F. NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.periodo +  ;
            m.numsc TIMEOUT 0.001   ;
            NOREFRESH
ELSE
     BROWSE NOOPTIMIZE FIELDS  ;
            numpec :H = 'Pecs',  ;
            codfte :H = 'Fte',  ;
            codart :H = 'C¢digo',  ;
            cansol :H = 'Cant.'  ;
            :P = '999,999.999',  ;
            unimed :H = 'Uni' :W =  ;
            .F. : 5, xx =  ;
            MLINE(detalle, 1) :H =  ;
            'Descripci¢n' : 58 :W =  ;
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
PROCEDURE vis_hijo1
HIDE POPUP ALL
ON KEY LABEL F2 DO vista_aux
SELECT itesc
ON KEY LABEL F9 DO VISTA_DETA
IF vopcion = 1
     BROWSE NOOPTIMIZE FIELDS  ;
            numss :H = 'Sol.Ser.',  ;
            codfte :H = 'Fte',  ;
            codcad :H =  ;
            'Cod.Cadena' :W = .F.,  ;
            desped :H =  ;
            'Descripci¢n' :W =  ;
            .F. NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.periodo +  ;
            m.numsc TIMEOUT 0.001   ;
            NOREFRESH
ELSE
     BROWSE NOOPTIMIZE FIELDS  ;
            numss :H = 'Sol.Ser.',  ;
            codfte :H = 'Fte',  ;
            codcad :H =  ;
            'Cod.Cadena' :W = .F.,  ;
            xx = MLINE(detalle,  ;
            1) :H = 'Descripci¢n'  ;
            : 60 :W = .F. NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_2 KEY  ;
            m.periodo + m.numsc  ;
            TIMEOUT 0.001   ;
            NOREFRESH
ENDIF
SELECT solcot
RETURN
*
PROCEDURE vis_det
HIDE POPUP ALL
SELECT itesc
ON KEY LABEL F9 DO OBSERVA
IF vopcion = 1
     BROWSE NOOPTIMIZE FIELDS  ;
            numpec :H = 'Pecs',  ;
            codfte :H = 'Fte',  ;
            codart :H = 'C¢digo',  ;
            cansol :H = 'Cant.'  ;
            :P = '999,999.999',  ;
            unimed :H = 'Uni' :W =  ;
            .F. : 5, desped :H =  ;
            'Descripci¢n' : 58 :W =  ;
            .F. NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW  ;
            wind_2a KEY m.periodo +  ;
            m.numsc NOREFRESH
ELSE
     BROWSE NOOPTIMIZE FIELDS  ;
            numpec :H = 'Pecs',  ;
            codfte :H = 'Fte',  ;
            codart :H = 'C¢digo',  ;
            cansol :H = 'Cant.'  ;
            :P = '999,999.999',  ;
            unimed :H = 'Uni' :W =  ;
            .F. : 5, xx =  ;
            MLINE(detalle, 1) :H =  ;
            'Descripci¢n' : 58 :W =  ;
            .F. NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.periodo +  ;
            m.numsc NOREFRESH
ENDIF
SELECT solcot
ON KEY LABEL F9 DO vista_det
DO vista
RETURN
*
PROCEDURE vis_det1
HIDE POPUP ALL
ON KEY LABEL F2 DO vista_aux
SELECT itesc
ON KEY LABEL F9 DO OBSERVA
IF vopcion = 1
     BROWSE NOOPTIMIZE FIELDS  ;
            numss :H =  ;
            'Sol.Serv.', codfte  ;
            :H = 'Fte', codcad :H =  ;
            'Cod.Cadena' :W = .F.,  ;
            desped :H =  ;
            'Descripci¢n' :W =  ;
            .F. NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.periodo +  ;
            m.numsc NOREFRESH
ELSE
     BROWSE NOOPTIMIZE FIELDS  ;
            numss :H =  ;
            'Sol.Serv.', codfte  ;
            :H = 'Fte', codcad :H =  ;
            'Cod.Cadena' :W = .F.,  ;
            xx = MLINE(detalle,  ;
            1) :H = 'Descripci¢n'  ;
            : 60 :W = .F. NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_2 KEY  ;
            m.periodo + m.numsc  ;
            NOREFRESH
ENDIF
SELECT solcot
ON KEY LABEL F9 DO vista_det
RETURN
*
PROCEDURE revis
ON KEY LABEL F2
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
            'Fecha', tipdoc :H =  ;
            'DOC', ess =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'C/c ',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            '    ')))) :H =  ;
            'Estd', itesc.desped  ;
            :H = IIF(vopcion = 1,  ;
            'Articulo ',  ;
            'Servicio ') : 36,  ;
            itesc.unimed :H =  ;
            'Unid', itesc.cansol  ;
            :H = 'Cantid' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_0
ELSE
     BROWSE FIELDS numsc :H =  ;
            ' N§ ', fecsc :H =  ;
            'Fecha', tipdoc :H =  ;
            'DOC', ess =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'C/c ',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            '    ')))) :H =  ;
            'Estd', cc =  ;
            MLINE(itesc.detalle,  ;
            1) :H = IIF(vopcion =  ;
            1, 'Articulo ',  ;
            'Servicio ') : 36,  ;
            itesc.unimed :H =  ;
            'Unid', itesc.cansol  ;
            :H = 'Cantid' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_0
ENDIF
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SET RELATION TO
DO vista
RETURN
*
PROCEDURE busca
ON KEY LABEL F2
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnum_sc = '    '
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero Solicitud : '  ;
  GET vperiodo PICTURE '!!'
@ 1, 32 SAY '-' GET vnum_sc  ;
  PICTURE '!!!!' VALID vbusca()
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
vnum_sc = PADL(ALLTRIM(vnum_sc),  ;
          4, '0')
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
ON KEY LABEL F2
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado = '99'
     DO standby WITH vmens09
     RETURN
ENDIF
IF estado = '70'
     DO standby WITH vmens11
     RETURN
ENDIF
IF estado = '50'
     DO standby WITH vmens10
ENDIF
SELECT solcot
SCATTER MEMVAR
ACTIVATE WINDOW wind_1
DO pantalla
@ 1, 22 GET m.periodo PICTURE  ;
  '!!' DISABLE
@ 1, 24 SAY '-'
@ 1, 25 GET m.numsc PICTURE  ;
  '!!!!' DISABLE
@ 1, 60 GET m.fecsc PICTURE '@D'
@ 3, 22 GET m.observa PICTURE  ;
  '@S40'
@ 4, 22 GET m.lugar PICTURE  ;
  '@S40'
@ 5, 22 GET m.obssc1 PICTURE  ;
  '@S40'
@ 6, 22 GET m.obssc2 PICTURE  ;
  '@S40'
READ VALID val_read()
IF LASTKEY() <> 27
     DO WHILE .T.
          ok = corrije_hi()
          DO WHILE .T.
               oh = auxcot()
               IF  .NOT. yesno( ;
                   '¨ Adiciona Proveedores?' ;
                   )
                    EXIT
               ENDIF
          ENDDO
          IF LASTKEY() <> 27
               IF yesno( ;
                  '¨ Conforme la modificaci¢n ?' ;
                  )
                    EXIT
               ENDIF
          ELSE
               IF yesno( ;
                  '¨ Cancela la modificaci¢n ?' ;
                  )
                    ok = .F.
                    EXIT
               ENDIF
          ENDIF
     ENDDO
     IF ok .AND. LASTKEY() <> 27
          IF vopcion = 1
               SELECT itepec
               SET ORDER TO ITEPEC11
               SEEK m.periodo +  ;
                    m.numsc
               SCAN WHILE llave +  ;
                    periodo +  ;
                    numsc = 'û' +  ;
                    m.periodo +  ;
                    m.numsc
                    IF RLOCK()
                         REPLACE numsc  ;
                                 WITH  ;
                                 m.numsc,  ;
                                 estado  ;
                                 WITH  ;
                                 '20'
                    ENDIF
               ENDSCAN
               SET ORDER TO ITEPEC1
          ELSE
               SELECT solser
               IF RLOCK()
                    REPLACE numsc  ;
                            WITH  ;
                            m.numsc,  ;
                            estado  ;
                            WITH  ;
                            '20'
               ENDIF
          ENDIF
          UNLOCK
          SELECT itesc
          IF vopcion = 2
               oh = detalle()
          ENDIF
          SELECT solcot
          m.user = vuser_id
          m.user_fc = DATE()
          m.user_tp = 'C'
          GATHER MEMVAR
     ELSE
          SELECT solcot
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT solcot
DO vista
RETURN
*
PROCEDURE ingre
PUBLIC ast, rec, vmes, vpart,  ;
       vtipo
ON KEY LABEL F2
DO pantalla
SELECT solcot
rec = RECNO()
ast = ORDER()
SCATTER BLANK MEMVAR
m.periodo = RIGHT(DTOC(DATE()),  ;
            2)
= repasa()
m.fecsc = DATE()
m.estado = '00'
@ 1, 22 GET m.periodo PICTURE  ;
  '!!' DISABLE
@ 1, 24 SAY '-'
@ 1, 25 GET m.numsc PICTURE  ;
  '!!!!' DISABLE
@ 1, 60 GET m.fecsc PICTURE '@D'  ;
  DISABLE
@ 3, 22 GET m.observa PICTURE  ;
  '@S40'
@ 4, 22 GET m.lugar PICTURE  ;
  '@S40'
@ 5, 22 GET m.obssc1 PICTURE  ;
  '@S40'
@ 6, 22 GET m.obssc2 PICTURE  ;
  '@S40'
READ VALID val_read()
IF LASTKEY() = 27
     SELECT parma
     SEEK 'CORRELSOLCOT'
     REPLACE nument WITH (nument -  ;
             1)
     SELECT solcot
     IF numsc = ALLTRIM(m.numsc)
          DELETE NEXT 1
     ENDIF
     GOTO rec
     DO vista
     RETURN
ENDIF
IF LASTKEY() <> 27
     DO WHILE .T.
          ACTIVATE WINDOW standby
          @ 1, 14 SAY  ;
            'Espere un momento ... '  ;
            COLOR W/N* 
          ok = trabaja_hj()
          IF LASTKEY() <> 27  ;
             .AND. ok
               IF yesno( ;
                  '¨ Confirme el ingreso ?' ;
                  )
                    IF vopcion =  ;
                       2
                         DO agreg_ser
                    ENDIF
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
          SELECT solcot
          IF vopcion = 1
               SELECT itepec
               SET ORDER TO ITEPEC11
               SEEK m.periodo +  ;
                    m.numsc
               SCAN WHILE llave +  ;
                    periodo +  ;
                    numsc = 'û' +  ;
                    m.periodo +  ;
                    m.numsc .AND.  ;
                    estado =  ;
                    '00'
                    IF RLOCK()
                         REPLACE numsc  ;
                                 WITH  ;
                                 m.numsc,  ;
                                 estado  ;
                                 WITH  ;
                                 '20'
                    ENDIF
               ENDSCAN
               SET ORDER TO ITEPEC1
               m.tipdoc = 'PEC'
          ELSE
               SELECT solser
               IF RLOCK()
                    REPLACE numsc  ;
                            WITH  ;
                            m.numsc,  ;
                            estado  ;
                            WITH  ;
                            '20'
               ENDIF
               SET ORDER TO SOLSER1
               m.tipdoc = 'S/S'
          ENDIF
          UNLOCK
          SELECT solcot
          m.tipact = IIF(vopcion =  ;
                     1, 'B',  ;
                     'S')
          SELECT itesc
          IF vopcion = 2
               oh = detalle()
          ENDIF
          SELECT solcot
          IF  .NOT.  ;
              EMPTY(m.numsc)
               m.user = vuser_id
               m.user_fc = DATE()
               m.user_tp = 'I'
               GATHER MEMVAR
          ENDIF
          DO WHILE .T.
               oh = auxcot()
               IF  .NOT. yesno( ;
                   '¨ Adiciona Proveedores?' ;
                   )
                    EXIT
               ENDIF
          ENDDO
          SELECT solcot
     ELSE
          SELECT itesc
          SEEK m.periodo +  ;
               m.numsc
          SCAN WHILE  ;
               itesc.periodo =  ;
               m.periodo .AND.  ;
               itesc.numsc =  ;
               m.numsc
               IF RLOCK()
                    DELETE NEXT 1
               ENDIF
          ENDSCAN
          UNLOCK
          IF vopcion = 1
               SELECT itepec
               SET ORDER TO 11
               SCAN WHILE periodo +  ;
                    numsc =  ;
                    m.periodo +  ;
                    m.numsc
                    IF RLOCK()
                         REPLACE llave  ;
                                 WITH  ;
                                 ' ',  ;
                                 numsc  ;
                                 WITH  ;
                                 SPACE(4)
                    ENDIF
               ENDSCAN
               UNLOCK
               SET ORDER TO 1
          ELSE
               SELECT solser
               SEEK m.periodo +  ;
                    m.numsc
               UNLOCK
          ENDIF
          SELECT solcot
          IF numsc =  ;
             ALLTRIM(m.numsc)
               DELETE NEXT 1
          ENDIF
          GOTO rec
          SELECT parma
          SEEK 'CORRELSOLCOT'
          REPLACE nument WITH  ;
                  (nument - 1)
     ENDIF
ENDIF
UNLOCK ALL
SELECT solcot
IF vopcion = 2
     SET FILTER TO IIF(vflag = '*',;
.T., coddep = SUBSTR(vcoddep, 1, vnumdep))
ENDIF
DO vista
RETURN
*
FUNCTION trabaja_hj
PRIVATE vfun
vfun = .T.
ACTIVATE SCREEN
HIDE MENU mmenu
ON KEY LABEL F10 KEYBOARD CHR(23)
DO CASE
     CASE vopcion = 1
          ON KEY LABEL F2 DO BUSPEC;
   
          ON KEY LABEL F11 DO MARCA
          ON KEY LABEL F12 DO DESMARCA
          vtempo = '°°°°° ®F2¯ Busca Pec °°° ®F11¯ Marca û °°° ®F12¯ Desmarca û °°° ®F10¯ Sale °°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          SELECT itepec
          SET ORDER TO ITEPEC1
          SET FILTER TO estado = '00';
.AND. tippec <> 'S'
          SET RELATION TO periodo + numpec;
INTO pecosa
          DEACTIVATE WINDOW  ;
                     standby
          BROWSE FIELDS numord :H =  ;
                 'Od' :W = .F.,  ;
                 llave :H = 'û'  ;
                 :W = .F., codart  ;
                 :H = 'C¢digo' :W =  ;
                 .F., numpec :H =  ;
                 'Doc' :W = .F.,  ;
                 codfte :H =  ;
                 'Fte' :W = .F.,  ;
                 descri :H =  ;
                 'Descripci¢n' :  ;
                 30 :W = .F.,  ;
                 coduni :H =  ;
                 'Uni' :W = .F. :  ;
                 5, canreq :H =  ;
                 'Cantidad' :P =  ;
                 '9,999.999' :W =  ;
                 .F., x =  ;
                 ROUND(canreq *  ;
                 preuni, 5) :H =  ;
                 'Total' :P =  ;
                 '999,999.99' :W =  ;
                 .F. NOMENU  ;
                 NOAPPEND  ;
                 NODELETE NOCLEAR  ;
                 WINDOW wind_2
          IF LASTKEY() <> 27
               SET ORDER TO ITEPEC11
               SEEK m.periodo +  ;
                    m.numsc
               SCAN WHILE llave +  ;
                    periodo +  ;
                    numsc = 'û' +  ;
                    m.periodo +  ;
                    m.numsc .AND.  ;
                    estado = '00'  ;
                    .AND. tippec <>  ;
                    'S'
                    DO agreg_item
                    SELECT itepec
               ENDSCAN
               SET ORDER TO itepec1
          ENDIF
          SET RELATION TO
          SET FILTER TO
     CASE vopcion = 2
          vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          SELECT solser
          vord = ORDER()
          SET ORDER TO Solser3
          GOTO TOP
          IF  .NOT. EOF()
               DEACTIVATE WINDOW  ;
                          standby
               BROWSE FIELDS  ;
                      numss :H =  ;
                      'N§ S/S' :W =  ;
                      .F., fecss  ;
                      :H =  ;
                      'Fecha' :W =  ;
                      .F., atte  ;
                      :H =  ;
                      'Con Atencion a:'  ;
                      :W = .F.,  ;
                      coddep =  ;
                      val_para(coddep, ;
                      'CODDEP', ;
                      'D') : 15  ;
                      :H =  ;
                      'Dependencia'  ;
                      :W = .F.,  ;
                      ss =  ;
                      MLINE(destino,  ;
                      1) :H =  ;
                      'Destino'  ;
                      :W = .F.  ;
                      NOMENU  ;
                      NOAPPEND  ;
                      NODELETE  ;
                      NOCLEAR  ;
                      WINDOW  ;
                      wind_2
          ELSE
               DEACTIVATE WINDOW  ;
                          standby
               DO standby WITH  ;
                  'No existe Solicitudes a Cotizar'
               vfun = .F.
          ENDIF
          SELECT solser
          SET ORDER TO (vord)
ENDCASE
IF LASTKEY() = 27
     vfun = .F.
ENDIF
ON KEY LABEL F2
ON KEY LABEL F10
ON KEY LABEL F11
ON KEY LABEL F12
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT solcot
RETURN vfun
*
PROCEDURE buspec
ACTIVATE WINDOW wind_4
m.numpec = '0000'
@ 0, 1 SAY 'Ingrese N§ Pecosa =>'  ;
  GET m.numpec PICTURE '!!!!'
READ
DEACTIVATE WINDOW wind_4
SELECT itepec
SET ORDER TO ITEPEC1
GOTO TOP
SEEK m.periodo +  ;
     ALLTRIM(m.numpec)
IF EOF()
     WAIT WINDOW 'NO EXISTE ' +  ;
          m.periodo +  ;
          ALLTRIM(m.numpec)
ELSE
ENDIF
RETURN
*
FUNCTION val_sc
PRIVATE ads, ord
ads = RECNO()
ord = ORDER()
SET ORDER TO solcot1
SEEK m.periodo + m.numsc
ok1 = FOUND()
SET ORDER TO solcot2
SEEK m.periodo + m.numsc
ok2 = FOUND()
SET ORDER TO ord
GOTO ads
IF ok1 .OR. ok2
     RETURN .F.
ENDIF
RETURN .T.
*
PROCEDURE marca
REPLACE llave WITH 'û', numsc  ;
        WITH m.numsc
RETURN
*
PROCEDURE desmarca
REPLACE llave WITH ' ', numsc  ;
        WITH SPACE(4)
RETURN
*
PROCEDURE todesmar
recs = RECNO()
GOTO TOP
SCAN
     REPLACE llave WITH ' ',  ;
             numsc WITH SPACE(4)
ENDSCAN
GOTO TOP
GOTO recs
RETURN
*
PROCEDURE todmarca
vperpec = periodo + numpec
recx = RECNO()
GOTO TOP
SCAN FOR periodo + numpec =  ;
     vperpec
     REPLACE llave WITH 'û',  ;
             numsc WITH m.numsc
     vperpec = periodo + numpec
ENDSCAN
GOTO TOP
GOTO recx
RETURN
*
FUNCTION corrije_hi
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO Agreg_c
ON KEY LABEL F8 DO Elimi_C
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itesc
IF vopcion = 1
     BROWSE FIELDS numord :H =  ;
            'Od' :W = .F., codart  ;
            :H = 'C¢digo' :W =  ;
            .F., desped :H =  ;
            'Descripci¢n' : 50 :W =  ;
            .F., unimed :H =  ;
            'Uni' :W = .F. : 3,  ;
            cansol :H =  ;
            'Cantidad' :P =  ;
            '9,999.999' :W = .F.  ;
            NOMENU NOAPPEND  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_2 KEY  ;
            m.periodo + m.numsc
ELSE
     BROWSE FIELDS codcad :H =  ;
            'Cadena Fun.' :W =  ;
            .F., desped :H =  ;
            'Descripci¢n' :W =  ;
            .F. NOMENU NOAPPEND  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_2 KEY  ;
            m.periodo + m.numsc
ENDIF
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT solcot
IF LASTKEY() = 27
     RETURN .F.
ENDIF
RETURN
*
FUNCTION agreg_c
oq = trabaja_hj()
IF vopcion = 2
     DO agreg_ser
ENDIF
ON KEY LABEL F10 KEYBOARD CHR(23)
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
RETURN oq
*
PROCEDURE elimi_c
PRIVATE vfun, vkey, valias
valias = ALIAS()
vfun = .T.
IF vopcion = 1
     SELECT itepec
     SET ORDER TO ITEPEC11
     GOTO TOP
     SEEK itesc.periodo +  ;
          itesc.numsc +  ;
          itesc.codfte +  ;
          itesc.codart
     IF FOUND()
          IF RLOCK()
               REPLACE llave WITH  ;
                       ' ',  ;
                       estado  ;
                       WITH '00',  ;
                       numsc WITH  ;
                       SPACE(4)
          ENDIF
     ELSE
          DO standby WITH  ;
             'Advertencia:No es ubicado la Pecosa,Revise'
          vfun = .F.
     ENDIF
     SET ORDER TO Itepec1
ELSE
     SELECT solser
     SEEK itesc.periodo +  ;
          itesc.numsc
     IF FOUND()
          IF RLOCK()
               REPLACE estado  ;
                       WITH '00',  ;
                       numsc WITH  ;
                       SPACE(4)
          ENDIF
     ELSE
          DO standby WITH  ;
             'Advertencia:No es ubicado la S/S,Revise'
          vfun = .F.
     ENDIF
     UNLOCK
ENDIF
SELECT itesc
IF RLOCK() .AND. vfun
     DELETE NEXT 1
ENDIF
SELECT (valias)
RETURN
*
PROCEDURE auxcot
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°F10->Continua °°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO Agreg_Cot
ON KEY LABEL F8 DO Elimi_Cot
ON KEY LABEL F10 KEYBOARD CHR(23)
cc = ALIAS()
SELECT auxcot
SEEK m.periodo + m.numsc
IF  .NOT. FOUND()
     DO agreg_cot
ENDIF
BROWSE FIELDS codprv :H =  ;
       'C¢digo' :V = valprv() :F,  ;
       nompro :H = 'Proveeedor'  ;
       :W = .F. NOMENU NOAPPEND  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_3 KEY m.periodo +  ;
       m.numsc
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
sele &cc
RETURN
*
PROCEDURE vista_aux
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°F10->Continua °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
as = ALIAS()
SELECT auxcot
SEEK m.periodo + m.numsc
IF FOUND()
     BROWSE FIELDS codprv :H =  ;
            'C¢digo' :W = .F.,  ;
            nompro :H =  ;
            'Proveeedor' :W = .F.  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_3  ;
            KEY m.periodo +  ;
            m.numsc
ELSE
     DO standby WITH  ;
        'No se encuentra Registrado Proveedores'
ENDIF
ON KEY LABEL F8
ON KEY LABEL F10
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SELECT &AS
DO vista
RETURN
*
PROCEDURE vista_aux1
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°F10->Continua °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
ON KEY LABEL F10 KEYBOARD CHR(23)
cc = ALIAS()
SELECT auxcot
SEEK m.periodo + m.numsc
ACTIVATE WINDOW wind_3
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            codprv :H = 'C¢digo'  ;
            :W = .F., nompro :H =  ;
            'Proveeedor' :W = .F.  ;
            NOEDIT NOCLEAR KEY  ;
            m.periodo + m.numsc  ;
            NOREFRESH
ELSE
     DO standby WITH  ;
        ' No  tiene registrado Proveedores '
ENDIF
ON KEY LABEL F10
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
sele &cc
DEACTIVATE WINDOW wind_3
RETURN
*
FUNCTION agreg_cot
IF f_appd()
     REPLACE numsc WITH m.numsc,  ;
             periodo WITH  ;
             m.periodo, tipact  ;
             WITH IIF(vopcion = 1,  ;
             'B', 'S')
     RETURN .T.
ENDIF
RETURN .F.
*
PROCEDURE elimi_cot
IF RLOCK()
     DELETE NEXT 1
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
RETURN
*
FUNCTION valprv
PRIVATE xx, vfun
vfun = .F.
codprv = IIF(EMPTY(codprv),  ;
         codprv,  ;
         PADL(ALLTRIM(codprv), 4,  ;
         '0'))
xx = val_cot(codprv,.T.)
ON KEY LABEL F10 KEYBOARD CHR(23)
IF xx
     REPLACE codprv WITH  ;
             promae.codprv,  ;
             nompro WITH  ;
             promae.nompro
     RETURN .T.
ENDIF
RETURN vfun
*
PROCEDURE anula
SELECT solcot
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado <> '00'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ANULAR ‚sta SOLICITUD ?' ;
           )
IF velimina .AND. (RLOCK() .OR.  ;
   f_lock(1))
     SELECT itepec
     SET ORDER TO ITEPEC11
     GOTO TOP
     vkey = itesc.periodo +  ;
            itesc.numsc +  ;
            itesc.codfte
     SEEK vkey
     IF FOUND()
          SCAN WHILE  ;
               itepec.periodo +  ;
               itepec.numsc +  ;
               itepec.codfte =  ;
               vkey
               IF RLOCK()
                    REPLACE llave  ;
                            WITH  ;
                            ' ',  ;
                            estado  ;
                            WITH  ;
                            '00'
               ENDIF
               UNLOCK
          ENDSCAN
     ELSE
          DO standby WITH  ;
             'Advertencia:No es ubicado la Pecosa,Revise'
          vfun = .F.
     ENDIF
     SELECT solcot
     IF RLOCK()
          REPLACE estado WITH  ;
                  '99', fecver  ;
                  WITH DATE()
     ENDIF
     DO vista
ENDIF
UNLOCK
RETURN
*
FUNCTION agreg_item
as = ALIAS()
SELECT itesc
IF f_appd()
     IF vopcion = 1
          REPLACE numsc WITH  ;
                  m.numsc,  ;
                  periodo WITH  ;
                  m.periodo,  ;
                  codart WITH  ;
                  itepec.codart,  ;
                  codcad WITH  ;
                  itepec.codcad,  ;
                  cansol WITH  ;
                  itepec.canreq,  ;
                  unimed WITH  ;
                  itepec.coduni,  ;
                  desped WITH  ;
                  itepec.descri,  ;
                  perpec WITH  ;
                  itepec.periodo,  ;
                  numpec WITH  ;
                  itepec.numpec,  ;
                  numord WITH  ;
                  itepec.numord,  ;
                  codfte WITH  ;
                  itepec.codfte,  ;
                  tipdoc WITH  ;
                  'PEC', detalle  ;
                  WITH  ;
                  itepec.observa
     ELSE
          REPLACE numsc WITH  ;
                  m.numsc,  ;
                  periodo WITH  ;
                  m.periodo,  ;
                  codart WITH  ;
                  itepec.codart,  ;
                  codcad WITH  ;
                  itepec.codcad,  ;
                  cansol WITH  ;
                  vcanreq, unimed  ;
                  WITH  ;
                  itepec.coduni,  ;
                  dessc WITH  ;
                  itepec.descri,  ;
                  perpec WITH  ;
                  itepec.periodo,  ;
                  numpec WITH  ;
                  itepec.numpec,  ;
                  codfte WITH  ;
                  itepec.codfte,  ;
                  tipdoc WITH  ;
                  'S/S'
     ENDIF
     UNLOCK
ENDIF
SELE &AS
RETURN .F.
*
FUNCTION agreg_ser
SELECT itesc
IF f_appd()
     REPLACE numsc WITH m.numsc,  ;
             periodo WITH  ;
             m.periodo, codcad  ;
             WITH solser.codcad,  ;
             numss WITH  ;
             solser.numss, perss  ;
             WITH solser.periodo,  ;
             codfte WITH  ;
             solser.codfte,  ;
             detalle WITH  ;
             solser.detalle,  ;
             tipdoc WITH 'S/S'
     UNLOCK
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION xmarca
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
ON KEY LABEL F2
SELECT solcot
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lissol
ENDIF
SELECT solcot
GOTO vtemp
DO vista
RETURN
*
PROCEDURE lissol
DEFINE WINDOW lis FROM 5, 15 TO  ;
       18, 65 FLOAT TITLE  ;
       'Listado Solicitud de Cotizaciones'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE SPACE(25) TO voferta,  ;
      vfopag, vplaent, vgaran
vcli = SPACE(4)
van = SPACE(2)
@ 01, 01 SAY  ;
  '     Solicitud N§ : '
@ 01, 22 GET vcli PICTURE '!!!!'  ;
  VALID assig()
@ 01, 26 SAY '.'
@ 01, 27 GET van PICTURE '!!'  ;
  VALID valsol()
@ 03, 01 SAY  ;
  '         Condiciones : '
@ 04, 01 SAY  ;
  'Validez de la Oferta : ' GET  ;
  voferta PICTURE '@S20'
@ 05, 01 SAY  ;
  '       Forma de Pago : ' GET  ;
  vfopag PICTURE '@S20'
@ 06, 01 SAY  ;
  '    Plazo de Entrega : ' GET  ;
  vplaent PICTURE '@S20'
@ 07, 01 SAY  ;
  '            Garant¡a : ' GET  ;
  vgaran PICTURE '@S20'
@ 11, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     SELECT auxcot
     SET FILTER TO numsc = vcli;
.AND. periodo = van;
.AND. IIF(vopcion = 1, tipact = 'B', tipact;
= 'S')
     SET RELATION TO periodo + numsc INTO;
solcot
     SET RELATION TO periodo + numsc INTO;
itesc ADDITIVE
     DEACTIVATE WINDOW standby
     SET SKIP TO itesc
     SET MEMOWIDTH TO 60
     GOTO TOP
     IF  .NOT. EOF()
          IF vopcion = 1
               DO reporte WITH 2,  ;
                  'LisScB',  ;
                  ' Solicitud de Cotizaciones Bienes'
          ELSE
               DO reporte WITH 2,  ;
                  'LisScoS',  ;
                  ' Solicitud de Cotizaciones Servicios'
          ENDIF
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     SET RELATION TO
     SELECT solcot
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
                 IIF(vopcion = 1,  ;
                 'Articulo ',  ;
                 'Servicio ') :  ;
                 36, itesc.unimed  ;
                 :H = 'Unid',  ;
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
                 'Estd', xx =  ;
                 MLINE(itesc.detalle,  ;
                 1) :H =  ;
                 IIF(vopcion = 1,  ;
                 'Articulo ',  ;
                 'Servicio ') :  ;
                 36, itesc.unimed  ;
                 :H = 'Unid',  ;
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
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F9
ON KEY LABEL F10
SET ESCAPE &vesc
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_c1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
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
ELSE
     SELECT solcot
     APPEND BLANK
     IF f_appd()
          REPLACE periodo WITH  ;
                  m.periodo,  ;
                  numsc WITH  ;
                  numsc, estado  ;
                  WITH m.estado
     ENDIF
     UNLOCK
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
FUNCTION xtrabaja_h
vfun = .T.
ACTIVATE SCREEN
HIDE MENU mmenu
ON KEY LABEL F10 KEYBOARD CHR(23)
DO CASE
     CASE vopcion = 1
          vtempo = '°°°°°°° [*] Selecciona°°°°° [Space Bar] Desselecciona °°°°°° F10->Terminar°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          SELECT itepec
          BROWSE FIELDS llave :H =  ;
                 'C' :V = marca()  ;
                 .AND. llave $  ;
                 '* ', codart :H =  ;
                 'C¢digo' :W =  ;
                 .F., numpec :H =  ;
                 'Doc' :W = .F.,  ;
                 descri :H =  ;
                 'Descripci¢n' :  ;
                 25 :W = .F.,  ;
                 coduni :H =  ;
                 'Uni' :W = .F. :  ;
                 3, canreq :H =  ;
                 'Cantidad' :P =  ;
                 '99,999.999' :W =  ;
                 .F., preuni :H =  ;
                 'Costo' :P =  ;
                 '9,999,999.999'  ;
                 :W = .F., x =  ;
                 ROUND(canreq *  ;
                 preuni, 5) :H =  ;
                 'Total' :P =  ;
                 '9,999,999.99'  ;
                 :W = .F. NOMENU  ;
                 NOAPPEND  ;
                 NODELETE NOCLEAR  ;
                 WINDOW wind_2
          SET FILTER TO llave = '*'
          GOTO TOP
          SCAN
               vcanreq = 0
               vca1 = codart
               vca2 = codart
               DO WHILE vca1=vca2
                    vcanreq = vcanreq +  ;
                              canreq
                    SKIP
                    vca1 = codart
               ENDDO
               SKIP -1
               DO agreg_item
               SELECT itepec
          ENDSCAN
          SET FILTER TO
     CASE vopcion = 2
          vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          SELECT solser
          SET FILTER TO estado = '00'
          GOTO TOP
          IF  .NOT. EOF()
               BROWSE FIELDS  ;
                      numss :H =  ;
                      'N§ S/S' :W =  ;
                      .F., fecss  ;
                      :H =  ;
                      'Fecha' :W =  ;
                      .F., codcad  ;
                      :H =  ;
                      'Cod.Cadena'  ;
                      :W = .F.,  ;
                      xcoddep =  ;
                      val_para(coddep, ;
                      'CODDEP', ;
                      'D') : 15  ;
                      :H =  ;
                      'Dependencia'  ;
                      :W = .F.,  ;
                      desss :H =  ;
                      'Descripci¢n'  ;
                      :W = .F.  ;
                      NOMENU  ;
                      NOAPPEND  ;
                      NODELETE  ;
                      NOCLEAR  ;
                      WINDOW  ;
                      wind_2
          ELSE
               DO standby WITH  ;
                  'No existe Solicitudes a Cotizar'
               vfun = .F.
          ENDIF
          SELECT solser
          SET FILTER TO
ENDCASE
ON KEY LABEL F10
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT solcot
RETURN vfun
*
FUNCTION detalle
valias = ALIAS()
SELECT itesc
SET MEMOWIDTH TO 56
ON KEY LABEL F10 KEYBOARD CHR(23)
IF  .NOT. WEXIST('OBSERVA')
     DEFINE WINDOW observa FROM  ;
            05, 18 TO 18, 61  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Detalle Solicitud Servicio ±'  ;
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
            '± Detalle Solicitud de Servicio ±'  ;
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
FUNCTION val_cot
PARAMETER xcod, _tipo, _x, _y
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) .AND.  ;
       _tipo
_campo = VARREAD()
malias = ALIAS()
SELECT promae
_oldwnd = WOUTPUT()
ON KEY LABEL F10
IF  .NOT. medita
     SEEK xcod
     v_fun = IIF(FOUND(), nompro,  ;
             '')
ELSE
     IF EMPTY(xcod)
          SET ORDER TO PROMAE2
          ON KEY LABEL ENTER KEYBOARD;
CHR(23)
          ON KEY LABEL f2 DO prvbusC
          DEFINE WINDOW _xx FROM  ;
                 3, 3 TO 22, 77
          BROWSE FIELDS codprv :H =  ;
                 'C¢digo', nompro  ;
                 :H = 'Nombre' :  ;
                 45, dirpro :H =  ;
                 'Direccci¢n' :  ;
                 25 NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 _xx TITLE  ;
                 ' ®Enter¯  Selecciona    ®F2¯ Busca   '  ;
                 NOLGRID
          ON KEY LABEL f10
          ON KEY LABEL f2
          RELEASE WINDOW _xx
          SET ORDER TO promae1
          IF  .NOT.  ;
              EMPTY(_oldwnd)
               ACTIVATE WINDOW &_oldwnd
          ENDIF
          IF LASTKEY() = 27
               v_fun = .F.
          ELSE
               xcod = codprv
               IF mmsg
                    @ _x, _y SAY  ;
                      nompro
               ENDIF
               SELECT (malias)
               IF  .NOT. _tipo
                    REPLACE &_campo WITH;
 xcod
               ENDIF
               v_fun = .T.
          ENDIF
     ELSE
          SEEK xcod
          IF mmsg .AND. FOUND()
               @ _x, _y SAY  ;
                 nompro
          ENDIF
          v_fun = FOUND()
     ENDIF
ENDIF
SELECT (malias)
IF v_fun
     SELECT auxcot
     IF RLOCK()
          REPLACE codprv WITH  ;
                  promae.codprv,  ;
                  nompro WITH  ;
                  promae.nompro
     ENDIF
ENDIF
ON KEY LABEL ENTER
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN v_fun
*
PROCEDURE prvbusc
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
DEFINE BAR 3 OF _mm PROMPT  ;
       ' Busqueda por \<RUC    '
ON SELECTION POPUP _mm DEACTIVATE POPUP
orden = ORDER()
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
     CASE BAR() = 1
          ACTIVATE WINDOW _funbus
          _cod = '0000'
          @ 01, 02 SAY 'C¢digo: '  ;
            GET _cod PICTURE  ;
            '!!!!'
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
     CASE BAR() = 3
          ACTIVATE WINDOW _funbus
          _cod = SPACE(8)
          @ 01, 02 SAY 'RUC: '  ;
            GET _cod PICTURE  ;
            '@S8'
          READ
          DEACTIVATE WINDOW  ;
                     _funbus
          IF LASTKEY() <> 27
               SET ORDER TO 3
               SEEK ALLTRIM(_cod)
          ENDIF
     OTHERWISE
          RELEASE POPUP _mm
          SET ORDER TO (orden)
          RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
IF  .NOT. FOUND()
     DO standby WITH  ;
        'Proveedor no encontrado'
ENDIF
RETURN
*
FUNCTION vusua
PARAMETER csys
PRIVATE ali
ali = ALIAS()
vkey = ALLTRIM(csys)
SELECT usu
SEEK vkey
vfun = nombre
SELECT (ali)
RETURN vfun
*
FUNCTION observa
valias = ALIAS()
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
IF  .NOT. WEXIST('Observa')
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
IF WVISIBLE('Observa')
     ACTIVATE WINDOW SAME observa
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              observa
ENDIF
MODIFY MEMO detalle WINDOW  ;
       observa
IF  .NOT. WVISIBLE('Observa')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado. No graba la Observaci¢n '
ENDIF
RETURN .T.
*
FUNCTION visobs
valias = ALIAS()
IF  .NOT. WEXIST('Observa')
     DEFINE WINDOW observa FROM  ;
            03, 18 TO 20, 61  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Detalle Pecosa ±'  ;
            FOOTER  ;
            ' ° ®Esc¯ Sale ° '  ;
            DOUBLE COLOR SCHEME  ;
            1
ENDIF
IF WVISIBLE('Observa')
     ACTIVATE WINDOW SAME observa
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              observa
ENDIF
MODIFY MEMO detalle NOEDIT WINDOW  ;
       observa
IF  .NOT. WVISIBLE('Observa')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
RETURN .T.
*
FUNCTION sihay
PRIVATE as
as = RECNO()
SEEK m.periodo + m.numsc
IF FOUND() .AND. RECNO() <> as
     DO standby WITH  ;
        'La Solicitud de Cotiz. ya esta registrada'
     RETURN .F.
ENDIF
RETURN
*
FUNCTION repasa
vfun = .T.
vrec = RECNO()
vali = ALIAS()
SELECT solcot
IF tipdoc = 'PEC'
     SET ORDER TO SOLCOT1
ELSE
     IF tipdoc = 'S/S'
          SET ORDER TO SOLCOT2
     ENDIF
ENDIF
as = RECNO()
IF FOUND()
     GOTO as
ENDIF
SELECT parma
SEEK 'CORRELSOLCOT'
numr = nument + 1
SELECT solcot
DO WHILE .T.
     IF VAL(numsc) = numr
          numr = numr + 1
          SKIP
          LOOP
     ELSE
          EXIT
     ENDIF
ENDDO
IF vopcion = 1
     SET ORDER TO Solcot1
ELSE
     SET ORDER TO SolCot2
ENDIF
m.numsc = PADL(ALLTRIM(STR(numr,  ;
          4)), 4, '0')
IF m.numsc = '0000' .OR.  ;
   EMPTY(m.numsc)
     vfun = .F.
ELSE
     SELECT solcot
     IF f_appd()
          REPLACE periodo WITH  ;
                  m.periodo,  ;
                  numsc WITH  ;
                  m.numsc, estado  ;
                  WITH '00',  ;
                  user_tp WITH  ;
                  'E', user WITH  ;
                  vuser_id
     ENDIF
     UNLOCK
ENDIF
SELECT parma
SEEK 'CORRELSOLCOT'
IF LASTKEY() <> 27
     REPLACE nument WITH numr
ENDIF
SELECT (vali)
RETURN vfun
*

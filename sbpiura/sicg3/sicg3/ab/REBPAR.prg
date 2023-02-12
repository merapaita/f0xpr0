PARAMETER vopcion, sistema
IF vopcion = 1
     USE IN 1 PteAnu ALIAS anupa  ;
         ORDER PteAnu3
     USE IN 2 OrdCom ALIAS orden  ;
         ORDER OrdCom1
     USE IN 3 IteOc ALIAS iteoc  ;
         ORDER IteOc1
     USE IN 15 Iteoc1 ALIAS  ;
         iteoc1 ORDER IteOc11
ELSE
     USE IN 1 PteAnu ALIAS anupa  ;
         ORDER PteAnu4
     USE IN 2 OrdSer ALIAS ordse  ;
         ORDER OrdSer1
     USE IN 15 Iteos1 ALIAS  ;
         iteos1 ORDER IteOs11
ENDIF
USE IN 4 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 5 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 6 Itepec ALIAS itepec  ;
    ORDER ItePec1
USE IN 7 AuxCot ALIAS auxcot  ;
    ORDER AuxCot1
USE IN 8 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 9 Calen ALIAS calen ORDER  ;
    calen1
USE IN 16 AstOrd ALIAS astord  ;
    ORDER AstOrd1
IF sistema = '1'
     USE IN 10 maepre ALIAS  ;
         maepre ORDER maepre1
ELSE
     USE IN 10 maepre ALIAS  ;
         maepre ORDER maepre3
ENDIF
USE IN 11 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 12 HOJCON ALIAS hoja ORDER  ;
    hojcon1
USE IN 13 COMPAG ALIAS compag  ;
    ORDER compag1
USE IN 14 Cheque ALIAS cheque  ;
    ORDER Cheque1
USE IN 25 CatAsi ALIAS catasi  ;
    ORDER CatAsi4
PUBLIC vingre, v_reg, vvalr,  ;
       vtotcom
IF vopcion = 1
     vmens01 = ' Parte Rebaja Orden de Compra : REVISION '
     vmens02 = ' Registro de Parte Rebaja Ordenes de Compra '
     vmens04 = 'Dicho Orden de Compra no fue encontrado'
     vmens05 = 'No existe Orden de Compra anterior'
     vmens06 = 'No existe Orden de Compra siguiente'
     vmens07 = '¨ Desea ModificaR ‚ste Orden de Compra ?'
     vmens08 = 'No hay registros para procesar'
     vmens09 = 'Este Orden de Compra ha sido Modificado'
     vmens10 = 'Este Orden de Compra ya fue atendido'
     vmens11 = 'Este Orden de Compra ha sido devuelto'
ELSE
     vmens01 = ' Parte Rebaja Orden de Servicio: REVISION '
     vmens02 = ' Registro de Parte Rebaja Ordenes de Servicio '
     vmens04 = 'Dicho Orden de Servicio no fue encontrado'
     vmens05 = 'No existe Orden de Servicio anterior'
     vmens06 = 'No existe Orden de Servicio siguiente'
     vmens07 = '¨ Desea Modificar ‚ste Orden de Servicio ?'
     vmens08 = 'No hay registros para procesar'
     vmens09 = 'Este Orden de Servicio ha sido Modificado'
     vmens10 = 'Este Orden de Servicio ya fue atendido'
     vmens11 = 'Este Orden de Servicio ha sido devuelto'
ENDIF
SELECT anupa
GOTO BOTTOM
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
PROCEDURE inicia
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 11, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       ' Detalle O/C:            ®F12¯ Habilita Item'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2a FROM 10, 04  ;
       TO 16, 75 TITLE  ;
       '®F5¯ Agrega  ° ®F8¯ Eliminar  ° ®F10¯ Terminar '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 12, 04  ;
       TO 18, 75 TITLE  ;
       '®F5¯ Agrega  ° ®F8¯ Eliminar  ° ®F10¯ Terminar '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_4 FROM 10, 04  ;
       TO 16, 75 TITLE  ;
       '° ®F10¯ Terminar °'  ;
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
ACTIVATE WINDOW wind_0
CLEAR
@ 1, 2 SAY '       N£mero P/R :'
@ 2, 2 SAY '        Fecha P/R :'
IF vopcion = 1
     @ 1, 40 SAY  ;
       '       N£mero O/C :'
     @ 2, 40 SAY  ;
       '        Fecha O/C :'
ELSE
     @ 1, 40 SAY  ;
       '       N£mero O/S :'
     @ 2, 40 SAY  ;
       '        Fecha O/S :'
ENDIF
@ 4, 1 SAY '       Contratista :'
@ 5, 1 SAY 'Importe con Rebaja :'
@ 6, 2 SAY '       Cadena Fun :'
@ 7, 2 SAY ' Fte. Financiami. :'
@ 8, 2 SAY '          Funci¢n :'
@ 9, 2 SAY '         Programa :'
@ 10, 2 SAY '      Subprograma :'
@ 11, 2 SAY '   Activ./Proyec. :'
@ 13, 2 SAY '           Motivo :'
@ 15, 2 SAY '       Numero H/C :'
@ 16, 2 SAY '       Numero C/P :'
@ 17, 2 SAY '    Numero Cheque :'
@ 19, 2 SAY '    Observaciones :'
RETURN
*
PROCEDURE vista
ACTIVATE WINDOW wind_0
SELECT anupa
IF EOF()
     DO pantalla
     RETURN
ENDIF
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
@ 0, 60 SAY IIF(m.estado = '00',  ;
  'Pendiente    ', IIF(m.estado =  ;
  '20', 'En Cont.Pres',  ;
  IIF(m.estado = '99',  ;
  'Anulada  ', IIF(m.estado =  ;
  '50', 'Atendido ',  ;
  '         '))))
@ 1, 22 SAY m.numpa
@ 1, 60 SAY m.periodo
@ 1, 63 SAY m.codfte
@ 1, 67 SAY m.numref
@ 2, 22 SAY m.fecpa
@ 2, 60 SAY m.fecref
@ 4, 22 SAY val_prv(m.codprv)
@ 5, 22 SAY m.valtot PICTURE  ;
  '999,999.99'
@ 6, 22 SAY val_codcad(m.codcad, ;
  m.periodo,'D',22,30)
@ 7, 22 SAY val_para(m.codfte, ;
  'CODFTE','D',22,30)
@ 8, 22 SAY  ;
  val_para(maepre.codfun,'CODFUN', ;
  'V',22,40)
@ 9, 22 SAY  ;
  val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'V',22, ;
  40)
@ 10, 22 SAY  ;
  val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'V',22, ;
  40)
@ 11, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 13, 22 SAY m.motivo PICTURE  ;
  '@S56'
@ 15, 22 SAY m.numhc
@ 15, 26 SAY '.'
@ 15, 27 SAY m.perhc
@ 16, 22 SAY m.numcp
@ 16, 26 SAY '.'
@ 16, 27 SAY m.percp
@ 17, 22 SAY m.numchq
@ 19, 22 SAY m.observa
SET RELATION TO
RETURN
*
PROCEDURE revis
SELECT anupa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS numpa :H = 'N§Dc',  ;
       fecpa :H = 'Fecha', tipdoc  ;
       :H = 'Doc', numref :H =  ;
       'N§', fecref :H = 'Fecha',  ;
       ess = IIF(estado = '00',  ;
       'Pend', IIF(estado = '20',  ;
       'Afec', IIF(estado = '9',  ;
       'Anul', IIF(estado = '40',  ;
       'Aten', 'Liqu')))) :H =  ;
       'Estd', codprv :H = 'Prv',  ;
       observa :H = 'Observaci¢n'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
DO vista
RETURN
*
PROCEDURE busca
SELECT anupa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnum_oc = 0
ACTIVATE WINDOW standby
IF vopcion = 1
     @ 1, 01 SAY  ;
       'Ingrese N£mero O/C : '
ELSE
     @ 1, 01 SAY  ;
       'Ingrese N£mero O/S : '
ENDIF
@ 1, 23 GET vnum_oc PICTURE  ;
  '9999' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_oc) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vnum_oc
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
vnum_oc = PADL(ALLTRIM(STR(vnum_oc,  ;
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
DO pantalla
SELECT anupa
vingre = .F.
vvalr = 0
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
IF estado = '20'
     DO standby WITH  ;
        'El Parte de Rebaja ya fue Contabilizado'
     RETURN
ELSE
     @ 1, 22 GET m.numpa DISABLE
     @ 2, 22 GET m.fecpa VALID  ;
       val_ano()
     @ 1, 60 GET m.periodo  ;
       DISABLE
     @ 1, 63 GET m.codfte DISABLE
     @ 1, 67 GET m.numref DISABLE
     @ 2, 60 GET m.fecref DISABLE
     @ 4, 22 SAY  ;
       val_prv(m.codprv)
     @ 5, 22 GET m.valtot PICTURE  ;
       '999,999.99'
     @ 6, 22 SAY  ;
       val_codcad(m.codcad, ;
       m.periodo,' ',22,30)
     @ 7, 22 SAY  ;
       val_para(m.codfte,'CODFTE', ;
       ' ',22,30)
ENDIF
m.mespa = PADL(ALLTRIM(STR(MONTH(m.fecpa))),  ;
          2, '0')
IF LASTKEY() <> 27
     SELECT anupa
     GATHER MEMVAR
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
@ 8, 22 SAY  ;
  val_para(maepre.codfun,'CODFUN', ;
  'V',22,40)
@ 9, 22 SAY  ;
  val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'V',22, ;
  40)
@ 10, 22 SAY  ;
  val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'V',22, ;
  40)
@ 11, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 13, 22 GET m.motivo PICTURE  ;
  '@S56'
@ 15, 22 SAY m.numhc
@ 15, 26 SAY '.'
@ 15, 27 SAY m.perhc
@ 16, 22 SAY m.numcp
@ 16, 26 SAY '.'
@ 16, 27 SAY m.percp
@ 17, 22 SAY m.numchq
@ 19, 22 GET m.observa
READ VALID val_read()
IF LASTKEY() <> 27
     IF vopcion = 1
          DO corri_hj
          IF RLOCK()
               REPLACE anultot  ;
                       WITH  ;
                       m.valtot,  ;
                       orden.tipalt  ;
                       WITH 'R',  ;
                       orden.numreb  ;
                       WITH  ;
                       m.numpa,  ;
                       orden.estado  ;
                       WITH '22'
          ENDIF
          IF  .NOT.  ;
              EMPTY(m.numhc)
               SELECT hoja
               SEEK m.perhc +  ;
                    m.numhc
               IF FOUND()
                    IF RLOCK()
                         REPLACE numreb  ;
                                 WITH  ;
                                 m.numpa,  ;
                                 valreb  ;
                                 WITH  ;
                                 m.valtot
                    ENDIF
               ENDIF
          ENDIF
          SELECT iteoc
          SCAN WHILE iteoc.numoc +  ;
               iteoc.codfte =  ;
               orden.numoc +  ;
               orden.codfte
               IF RLOCK()
                    REPLACE estado  ;
                            WITH  ;
                            '95'
               ENDIF
               UNLOCK
          ENDSCAN
     ELSE
          IF  .NOT.  ;
              EMPTY(m.numhc)
               SELECT hoja
               SEEK m.perhc +  ;
                    m.numhc
               IF FOUND()
                    IF RLOCK()
                         REPLACE numreb  ;
                                 WITH  ;
                                 m.numpa,  ;
                                 valreb  ;
                                 WITH  ;
                                 m.valtot
                    ENDIF
               ENDIF
          ENDIF
          SELECT ordse
          SEEK m.periodo +  ;
               m.numref
          IF FOUND()
               IF RLOCK()
                    REPLACE anultot  ;
                            WITH  ;
                            m.valtot,  ;
                            ordse.tipalt  ;
                            WITH  ;
                            'R',  ;
                            ordse.numreb  ;
                            WITH  ;
                            m.numpa
               ENDIF
          ENDIF
          vtotcom = ordse.valtot -  ;
                    m.valtot
          SELECT iteos1
          SET ORDER TO iteos11
          SEEK m.periodo +  ;
               m.numref
          IF FOUND()
               IF RLOCK()
                    REPLACE iteos1.valreb  ;
                            WITH  ;
                            m.valtot
               ENDIF
          ENDIF
     ENDIF
     UNLOCK ALL
     m.codcad = IIF(vopcion = 1,  ;
                orden.codcad,  ;
                ordse.codcad)
     m.tipo = 'R'
     m.valtot = ROUND(m.valtot,  ;
                2)
     SELECT anupa
     GATHER MEMVAR
     DO asiord
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
DO pantalla
DO vista
RETURN
*
FUNCTION val_ano
IF YEAR(m.fecpa) = YEAR(m.fecsis)
     mret = .T.
ELSE
     DO standby WITH  ;
        'Error en la Fecha... Revise.'
     mret = .F.
ENDIF
RETURN mret
*
FUNCTION verfi_s
IF m.valtot > vmax
     DO standby WITH  ;
        'Se est  exediendo en '+ ;
        STR(m.valtot-vmax, 5)
     RETURN .F.
ELSE
     RETURN .T.
ENDIF
*
PROCEDURE ingre
DO pantalla
vingre = .T.
SCATTER BLANK MEMVAR
m.tipo = 'R'
m.estado = '00'
m.numref = SPACE(4)
m.fecpa = DATE()
m.periodo = PADL(ALLTRIM(STR(YEAR(DATE()) -  ;
            2000)), 2, '0')
SELECT anupa
@ 1, 60 GET m.periodo PICTURE  ;
  '!!' VALID  .NOT.  ;
  EMPTY(m.periodo)
@ 1, 63 GET m.codfte VALID  ;
  val_para(m.codfte,'CODFTE', ;
  'C')
@ 1, 67 GET m.numref PICTURE  ;
  '!!!!' VALID val_rev()
READ
IF LASTKEY() = 27
     SELECT anupa
     DO vista
     RETURN
ENDIF
SELECT parma
IF vopcion = 1
     SEEK 'CORREL' + 'ANULOC'
ELSE
     SEEK 'CORREL' + 'ANULOS'
ENDIF
= valpa(parma.nument + 1)
SELECT anupa
IF vopcion = 1
     m.codfte = orden.codfte
     m.tipdoc = 'O/C'
     m.numref = orden.numoc
     m.mespa = PADL(ALLTRIM(STR(MONTH(m.fecpa))),  ;
               2, '0')
     m.fecref = orden.fecoc
     m.codprv = orden.codprv
     m.codcad = orden.codcad
     m.valtot = 0
     vmax = orden.valtot
     SELECT anupa
ELSE
     m.codfte = ordse.codfte
     m.tipdoc = 'O/S'
     m.numref = ordse.numos
     m.mespa = PADL(ALLTRIM(STR(MONTH(m.fecpa))),  ;
               2, '0')
     m.fecref = ordse.fecos
     m.codprv = ordse.codprv
     m.codcad = ordse.codcad
     m.valtot = 0
     vmax = ordse.valtot
ENDIF
m.motivo = SPACE(254)
m.observa = SPACE(40)
@ 1, 22 GET m.numpa
@ 2, 22 GET m.fecpa VALID  ;
  val_ano()
@ 1, 60 GET m.periodo DISABLE
@ 1, 63 GET m.codfte DISABLE
@ 1, 67 GET m.numref DISABLE
@ 2, 60 GET m.fecref DISABLE
@ 4, 22 SAY val_prv(m.codprv,.T., ;
  4,22) .AND. siprv() .AND.  ;
  promae.estado = 'VG'
@ 5, 22 GET m.valtot PICTURE  ;
  '999,999.99'
@ 6, 22 SAY val_codcad(m.codcad, ;
  m.periodo,' ',22,30)
@ 7, 22 SAY val_para(m.codfte, ;
  'CODFTE',' ',22,30)
@ 8, 22 SAY  ;
  val_para(maepre.codfun,'CODFUN', ;
  'V',22,40)
@ 9, 22 SAY  ;
  val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'V',22, ;
  40)
@ 10, 22 SAY  ;
  val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'V',22, ;
  40)
@ 11, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 13, 22 GET m.motivo PICTURE  ;
  '@S56'
@ 15, 22 SAY m.numhc
@ 15, 26 SAY '.'
@ 15, 27 SAY m.perhc
@ 16, 22 SAY m.numcp
@ 16, 26 SAY '.'
@ 16, 27 SAY m.percp
@ 17, 22 SAY m.numchq
@ 19, 22 GET m.observa
READ VALID val_read()
m.mespa = PADL(ALLTRIM(STR(MONTH(m.fecpa))),  ;
          2, '0')
IF LASTKEY() <> 27
     vcod = m.numpa
     IF f_appd()
          IF vopcion = 1
               SELECT orden
               DO corri_hj
               IF RLOCK()
                    REPLACE anultot  ;
                            WITH  ;
                            m.valtot,  ;
                            orden.tipalt  ;
                            WITH  ;
                            'R',  ;
                            orden.numreb  ;
                            WITH  ;
                            m.numpa,  ;
                            orden.estado  ;
                            WITH  ;
                            '22'
               ENDIF
               IF  .NOT.  ;
                   EMPTY(m.numhc)
                    SELECT hoja
                    SEEK m.perhc +  ;
                         m.numhc
                    IF FOUND()
                         IF RLOCK()
                              REPLACE  ;
                               numreb  ;
                               WITH  ;
                               m.numpa,  ;
                               valreb  ;
                               WITH  ;
                               m.valtot
                         ENDIF
                    ENDIF
               ENDIF
               SELECT iteoc
               SCAN WHILE  ;
                    iteoc.numoc +  ;
                    iteoc.codfte =  ;
                    orden.numoc +  ;
                    orden.codfte
                    IF RLOCK()
                         REPLACE estado  ;
                                 WITH  ;
                                 '95'
                    ENDIF
                    UNLOCK
               ENDSCAN
               UNLOCK ALL
          ELSE
               IF  .NOT.  ;
                   EMPTY(m.numhc)
                    SELECT hoja
                    SEEK m.perhc +  ;
                         m.numhc
                    IF FOUND()
                         IF RLOCK()
                              REPLACE  ;
                               numreb  ;
                               WITH  ;
                               m.numpa,  ;
                               valreb  ;
                               WITH  ;
                               m.valtot
                         ENDIF
                    ENDIF
               ENDIF
               SELECT ordse
               SEEK m.periodo +  ;
                    m.numref
               IF RLOCK()
                    REPLACE anultot  ;
                            WITH  ;
                            m.valtot,  ;
                            ordse.tipalt  ;
                            WITH  ;
                            'R',  ;
                            ordse.numreb  ;
                            WITH  ;
                            m.numpa
               ENDIF
               SELECT iteos1
               SET ORDER TO iteos11
               SEEK m.periodo +  ;
                    m.numref
               IF RLOCK()
                    REPLACE iteos1.valreb  ;
                            WITH  ;
                            m.valtot
               ENDIF
               vtotcom = ordse.valtot -  ;
                         m.valtot
               UNLOCK ALL
               m.codcad = ordse.codcad
               m.numpa = vcod
               m.tipo = 'R'
          ENDIF
          SELECT anupa
          GATHER MEMVAR
          DO asiord
          SELECT parma
          IF vopcion = 1
               SEEK 'CORREL' +  ;
                    'ANULOC'
          ELSE
               SEEK 'CORREL' +  ;
                    'ANULOS'
          ENDIF
          IF sistema = '1'
               USE IN 10 maepre  ;
                   ALIAS maepre  ;
                   ORDER maepre1
          ELSE
               USE IN 10 maepre  ;
                   ALIAS maepre  ;
                   ORDER maepre3
          ENDIF
          = valpa(parma.nument +  ;
            1)
          REPLACE nument WITH  ;
                  nument + 1
          SELECT anupa
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
     SELECT anupa
ENDIF
DO vista
RETURN
*
PROCEDURE desmarca
IF yesno( ;
   'Est  seguro de habilitar este Item?' ;
   )
     IF RLOCK()
          IF  .NOT. EMPTY(numsc)
               REPLACE orden WITH  ;
                       ' ',  ;
                       estado  ;
                       WITH '20',  ;
                       numoc WITH  ;
                       SPACE(4)
          ELSE
               REPLACE orden WITH  ;
                       ' ',  ;
                       estado  ;
                       WITH '00',  ;
                       numoc WITH  ;
                       SPACE(4)
          ENDIF
     ENDIF
     UNLOCK
ENDIF
RETURN
*
PROCEDURE val_rev
PRIVATE vtemp
IF vopcion = 1
     as = ALIAS()
     SELECT orden
     vtemp = RECNO()
     IF EOF()
          DO standby WITH vmens08
          RETURN
     ENDIF
     SET RELATION TO periodo + numoc +;
codfte INTO iteoc
     SET SKIP TO iteoc
     SET FILTER TO estado <> '90';
.AND. estado <> '99'
     GOTO TOP
     SEEK m.periodo + m.numref
     IF  .NOT. FOUND()
          vtemp = RECNO()
          HIDE MENU mmenu
          ACTIVATE SCREEN
          vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          ON KEY LABEL F10 KEYBOARD CHR(23)
          BROWSE FIELDS numoc :H =  ;
                 ' N§ ', fecoc :H =  ;
                 'Fecha', ess =  ;
                 IIF(estado =  ;
                 '00', 'Pend',  ;
                 IIF(estado =  ;
                 '20', 'Afec',  ;
                 IIF(estado = '9',  ;
                 'Anul',  ;
                 IIF(estado =  ;
                 '40', 'Aten',  ;
                 'Liqu')))) :H =  ;
                 'Estd',  ;
                 iteoc.descri :H =  ;
                 'Articulo ' : 36,  ;
                 iteoc.coduni :H =  ;
                 'Unid',  ;
                 iteoc.canreq :H =  ;
                 'Cantid' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 wind_0
          vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          SHOW MENU mmenu
          ON KEY LABEL F10
          m.numhc = orden.numhc
          m.perhc = orden.perhc
          SELECT hoja
          vord = ORDER()
          SET ORDER TO hojcon1
          SEEK m.perhc + m.numhc
          IF FOUND()
               IF  .NOT.  ;
                   EMPTY(hoja.numcp)
                    m.numcp = hoja.numcp
                    m.percp = hoja.nummescp
                    vcta = hoja.codctc
               ELSE
                    m.numcp = ''
                    m.percp = ''
                    vcta = ''
               ENDIF
          ELSE
               m.numcp = ''
               m.percp = ''
               vcta = ''
          ENDIF
          SET ORDER TO (vord)
          SELECT cheque
          vord = ORDER()
          SET ORDER TO Cheque1
          SEEK m.percp + m.numcp +  ;
               vcta
          IF FOUND()
               m.numchq = cheque.numchq
          ELSE
               m.numchq = ''
          ENDIF
          SET ORDER TO (vord)
          m.codfte = iteoc.codfte
     ELSE
          m.numhc = orden.numhc
          m.perhc = orden.perhc
          SELECT hoja
          vord = ORDER()
          SET ORDER TO hojcon1
          SEEK m.perhc + m.numhc
          IF FOUND()
               IF  .NOT.  ;
                   EMPTY(hoja.numcp)
                    m.numcp = hoja.numcp
                    m.percp = hoja.nummescp
                    vcta = hoja.codctc
               ELSE
                    m.numcp = ''
                    m.percp = ''
                    vcta = ''
               ENDIF
          ELSE
               m.numcp = ''
               m.percp = ''
               vcta = ''
          ENDIF
          SET ORDER TO (vord)
          SELECT cheque
          vord = ORDER()
          SET ORDER TO Cheque1
          SEEK m.percp + m.numcp +  ;
               vcta
          IF FOUND()
               m.numchq = cheque.numchq
          ELSE
               m.numchq = ''
          ENDIF
          SET ORDER TO (vord)
          m.codfte = iteoc.codfte
     ENDIF
     SELECT (as)
ELSE
     as = ALIAS()
     SELECT ordse
     IF EOF()
          DO standby WITH vmens08
          RETURN
     ENDIF
     SET FILTER TO estado <> '90';
.AND. estado <> '99'
     GOTO TOP
     SEEK m.periodo + m.numref +  ;
          ALLTRIM(m.codfte)
     IF  .NOT. FOUND()
          vtemp = RECNO()
          HIDE MENU mmenu
          ACTIVATE SCREEN
          vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          ON KEY LABEL F10 KEYBOARD CHR(23)
          BROWSE FIELDS numos :H =  ;
                 ' N§ ', est =  ;
                 IIF(estado =  ;
                 '00', 'Pend',  ;
                 IIF(estado =  ;
                 '20', 'S/Ct',  ;
                 IIF(estado =  ;
                 '99', 'Anul',  ;
                 IIF(estado =  ;
                 '50', 'Aten',  ;
                 ' -  ')))) :H =  ;
                 'ESTD', fecos :H =  ;
                 'Fecha', coddep  ;
                 :H = 'DEP',  ;
                 desos :H =  ;
                 'Descripci¢n'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 WINDOW wind_0
          vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          m.codfte = ordse.codfte
          SHOW MENU mmenu
          ON KEY LABEL F10
          m.numhc = ordse.numhc
          m.perhc = ordse.perhc
          SELECT hoja
          vord = ORDER()
          SET ORDER TO hojcon1
          SEEK m.perhc + m.numhc
          IF FOUND()
               IF  .NOT.  ;
                   EMPTY(hoja.numcp)
                    m.numcp = hoja.numcp
                    m.percp = hoja.nummescp
                    vcta = hoja.codctc
               ELSE
                    m.numcp = ''
                    m.percp = ''
                    vcta = ''
               ENDIF
          ELSE
               m.numcp = ''
               m.percp = ''
               vcta = ''
          ENDIF
          SET ORDER TO (vord)
          SELECT cheque
          vord = ORDER()
          SET ORDER TO Cheque1
          SEEK m.percp + m.numcp +  ;
               vcta
          IF FOUND()
               m.numchq = cheque.numchq
          ELSE
               m.numchq = ''
          ENDIF
          SET ORDER TO (vord)
          m.codfte = ordse.codfte
     ELSE
          m.numhc = ordse.numhc
          m.perhc = ordse.perhc
          SELECT hoja
          vord = ORDER()
          SET ORDER TO hojcon1
          SEEK m.perhc + m.numhc
          IF FOUND()
               IF  .NOT.  ;
                   EMPTY(hoja.numcp)
                    m.numcp = hoja.numcp
                    m.percp = hoja.nummescp
                    vcta = hoja.codctc
               ELSE
                    m.numcp = ''
                    m.percp = ''
                    vcta = ''
               ENDIF
          ELSE
               m.numcp = ''
               m.percp = ''
               vcta = ''
          ENDIF
          SET ORDER TO (vord)
          SELECT cheque
          vord = ORDER()
          SET ORDER TO Cheque1
          SEEK m.percp + m.numcp +  ;
               vcta
          IF FOUND()
               m.numchq = cheque.numchq
          ELSE
               m.numchq = ''
          ENDIF
          SET ORDER TO (vord)
          m.codfte = ordse.codfte
     ENDIF
     SELECT (as)
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
xx = val_prv(codprv,.T.)
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
SELECT anupa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado <> '00'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ANULAR esta Parte ?' ;
           )
IF velimina
     IF vopcion = 1
          SELECT orden
          IF RLOCK()
               REPLACE orden.tipalt  ;
                       WITH ' ',  ;
                       orden.numreb  ;
                       WITH  ;
                       '    '
          ENDIF
          IF  .NOT.  ;
              EMPTY(m.numhc)
               SELECT hoja
               SEEK m.perhc +  ;
                    m.numhc
               IF FOUND()
                    IF RLOCK()
                         REPLACE numreb  ;
                                 WITH  ;
                                 '    ',  ;
                                 valreb  ;
                                 WITH  ;
                                 0
                    ENDIF
               ENDIF
          ENDIF
     ELSE
          SELECT ordse
          IF RLOCK()
               REPLACE ordse.tipalt  ;
                       WITH ' ',  ;
                       ordse.numreb  ;
                       WITH  ;
                       '    '
          ENDIF
          IF  .NOT.  ;
              EMPTY(m.numhc)
               SELECT hoja
               SEEK m.perhc +  ;
                    m.numhc
               IF FOUND()
                    IF RLOCK()
                         REPLACE numreb  ;
                                 WITH  ;
                                 '    ',  ;
                                 valreb  ;
                                 WITH  ;
                                 0
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
     SELECT anupa
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
vord = ORDER()
SELECT iteoc
IF f_appd()
     REPLACE numoc WITH m.numoc,  ;
             periodo WITH  ;
             m.periodo, codart  ;
             WITH itepec.codart,  ;
             codcad WITH  ;
             itepec.codcad,  ;
             canreq WITH  ;
             itepec.canreq,  ;
             coduni WITH  ;
             itepec.coduni,  ;
             descri WITH  ;
             itepec.descri,  ;
             preuni WITH  ;
             itepec.preuni
     RETURN .T.
ENDIF
SET ORDER TO vOrd
RETURN .F.
*
FUNCTION marca
vtemp = RECNO()
vorde = itepec.orden
vcodcad = itepec.codcad +  ;
          itepec.codprv
SET FILTER TO itepec.codcad + itepec.codprv;
= vcodcad
GOTO TOP
SCAN
     DO CASE
          CASE vorde = '*'
               IF RLOCK()
                    REPLACE orden  ;
                            WITH  ;
                            '*',  ;
                            estado  ;
                            WITH  ;
                            '30'
               ENDIF
          CASE vorde = ' '
               IF RLOCK()
                    REPLACE orden  ;
                            WITH  ;
                            ' ',  ;
                            estado  ;
                            WITH  ;
                            '20'
               ENDIF
     ENDCASE
ENDSCAN
SET FILTER TO
GOTO vtemp
KEYBOARD CHR(23)
RETURN .T.
*
PROCEDURE lista
SELECT anupa
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     vmensl = IIF(vopcion = 1,  ;
              ' PARTE ANULACION O/C',  ;
              ' PARTE ANULACION O/S' ;
              )
     IF vopcion = 1
          SET ORDER TO PteAnu3
     ELSE
          SET ORDER TO PteAnu4
     ENDIF
     ACTIVATE WINDOW standby
     vnumpa = m.numpa
     vperio = m.periodo
     @ 1, 1 SAY  ;
       'Ingrese N§ Pte Rebaja : '  ;
       GET vperio
     @ 1, 29 GET vnumpa
     READ
     DEACTIVATE WINDOW standby
     IF LASTKEY() = 27
          RETURN
     ENDIF
     IF vopcion = 1
          SET RELATION TO periodo + numref;
INTO iteoc
          SET SKIP TO iteoc
          SET FILTER TO periodo = vperio;
.AND. numpa = vnumpa;
.AND. (;
.NOT. EMPTY(iteoc.canreb);
.OR. ;
.NOT. EMPTY(iteoc.prereb);
.OR. iteoc.perpec = 'LI')
          vnumref = busc_oc()
          DO reporte WITH 2,  ;
             'LisPRC1', vmensl,  ;
             2
          SET FILTER TO
          SET RELATION TO
     ELSE
          SET FILTER TO periodo = vperio;
.AND. numpa = vnumpa
          vnumref = busc_os()
          DO reporte WITH 2,  ;
             'LisPRs1', vmensl,  ;
             2
     ENDIF
ENDIF
SELECT anupa
GOTO vtemp
DO vista
RETURN
*
PROCEDURE termi
ven_accion = .F.
ON KEY LABEL F2
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
m.codpr = IIF(EMPTY(m.codprv),  ;
          m.codprv,  ;
          PADL(ALLTRIM(m.codprv),  ;
          4, '0'))
xx = val_prv(m.codprv,.T.)
IF xx
     RETURN .T.
ENDIF
RETURN vfun
*
FUNCTION valpa
PARAMETER vnumpa
PRIVATE vfun
vfun = .T.
m.numpa = PADL(ALLTRIM(STR(vnumpa,  ;
          4)), 4, '0')
IF m.numpa = '0000' .OR.  ;
   EMPTY(m.numpa)
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
PROCEDURE lisser
vorde = ORDER()
DEFINE WINDOW lis FROM 1, 15 TO  ;
       23, 65 FLOAT TITLE  ;
       'Listado Solicitud de Servicios'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro, vtofue
vcli = SPACE(4)
vano = '95'
vfte = '   '
vcodfte = '   '
@ 02, 01 SAY  ;
  '        Total O/C : ' GET  ;
  vtocli SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,7,22)
@ 04, 01 SAY  ;
  '              O/C : '
@ 04, 22 GET vfte PICTURE '!!!'  ;
  VALID val_para(vfte,'CODFTE', ;
  'C') WHEN vtocli = 2
@ 04, 26 GET vano PICTURE '!!'  ;
  WHEN vtocli = 2
@ 04, 28 SAY '-'
@ 04, 29 GET vcli PICTURE '!!!!'  ;
  VALID vo() .AND. valord() WHEN  ;
  vtocli = 2
@ 06, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,11,22)
@ 08, 01 SAY  ;
  '           Fuente : '
@ 08, 22 GET vcodfte PICTURE  ;
  '!!!' VALID val_para(vcodfte, ;
  'CODFTE','C') WHEN vtofue = 2
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
     SET FILTER TO IIF(vtocli = 1,;
.T., periodo + numoc + codfte = vano +;
vcli + ALLTRIM(vfte))
     SET INDEX TO (vind)
     COUNT ALL TO vtotos
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtippro = 1,  ;
               'Listado Orden Compra',  ;
               IIF(vtippro = 2,  ;
               'Listado Orden de Servicio Pendientes',  ;
               IIF(vtippro = 3,  ;
               'Listado Orden de Servicios Afectados',  ;
               IIF(vtippro = 4,  ;
               'Listado Orden de Servicios Anulados',  ;
               'Listado Orden de Compra Liquidados' ;
               ))))
     IF  .NOT. EOF()
          IF .F.
               DO CASE
                    CASE vlistado =  ;
                         1
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOs1',  ;
                            ' Ordenes de Compra ',  ;
                            2
                    CASE vlistado =  ;
                         2
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrds',  ;
                            ' Ordenes de Compra ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE vlistado =  ;
                         3
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrsX',  ;
                            ' Ordenes de Compra ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
               ENDCASE
          ENDIF
          DO reporte WITH 2,  ;
             'LisPRC1', vmensl
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     CLOSE INDEX
     ERASE (vind)
ENDIF
SELECT ordse
SET ORDER TO 1
RETURN
*
FUNCTION vo
vcli = PADL(ALLTRIM(vcli), 4,  ;
       '0')
RETURN .T.
*
FUNCTION valord
SELECT orden
DEFINE WINDOW windo_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
vtem = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
SEEK vano + vcli + ALLTRIM(vfte)
IF  .NOT. FOUND()
     HIDE MENU mmenu
     ACTIVATE SCREEN
     vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     ON KEY LABEL F10 KEYBOARD CHR(23)
     BROWSE FIELDS numoc :H =  ;
            ' N§ ', est =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'S/Ct',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            ' -  ')))) :H =  ;
            'ESTD', fecoc :H =  ;
            'Fecha', codfte :H =  ;
            'FTE ', observa :H =  ;
            'Descripci¢n' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            windo_0
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     IF LASTKEY() = 27
          GOTO vtemp
     ENDIF
     SHOW MENU mmenu
     ON KEY LABEL F10
ENDIF
vcli = numoc
GOTO vtemp
RETURN .T.
*
FUNCTION busc_oc
PRIVATE qw
qw = ALIAS()
vck = anupa.periodo +  ;
      anupa.numref +  ;
      anupa.codfte
SELECT orden
tot = 0
SEEK vck
IF FOUND()
     tot = orden.valtot
ENDIF
SELECT (qw)
RETURN tot
*
FUNCTION busc_os
PRIVATE qw
qw = ALIAS()
vck = anupa.periodo +  ;
      anupa.numref +  ;
      anupa.codfte
SELECT ordse
tot = 0
SEEK vck
IF FOUND()
     tot = ordse.valtot
ENDIF
SELECT (qw)
RETURN tot
*
PROCEDURE bus
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
       ' por \<Por Total O/C'
DEFINE BAR 2 OF busmenu PROMPT  ;
       ' por \<Por Componente'
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
     OTHERWISE
          RETURN
ENDCASE
*
FUNCTION corri_hj
SELECT orden
SEEK m.periodo + m.numref
vbase = ALIAS()
vregi = RECNO()
IF  .NOT. vingre
     vvalr = 0
ENDIF
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°°°°°F8->Liberar °°°°°°°°°°°°°°°°°°°°°°°°°F10->Terminar°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F8 DO Elimi_Ic
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itepec
SET ORDER TO ITEPEC16
SELECT iteoc
SET ORDER TO iteoc1
SEEK m.periodo + m.numref
BROWSE FIELDS codart :H =  ;
       'C¢digo' :V =  ;
       val_artc(codart,.F.) :F :W =  ;
       EMPTY(codart), descri :H =  ;
       'Descripci¢n' : 29 :W =  ;
       .F., canreq :H =  ;
       'Cantidad' :P =  ;
       '99,999.999' :R, canreb :H =  ;
       'Cant/Reb' :P =  ;
       '99,999.999', coduni :H =  ;
       'Uni' :W = .F. : 3, preuni  ;
       :H = 'PreUni' :P =  ;
       '99,999.999' :R, prereb :H =  ;
       'PreReb' :P =  ;
       '99,999.99999', x =  ;
       ROUND(canreq * preuni, 3)  ;
       :H = 'Total' :P =  ;
       '999,999.99' :W = .F., y =  ;
       ROUND(IIF(canreb > 0,  ;
       canreb * IIF(prereb > 0,  ;
       prereb, preuni), canreq *  ;
       IIF(prereb > 0, prereb,  ;
       preuni)), 5) :H = 'Rebaja'  ;
       :P = '99,999.999' :W = .F.,  ;
       z = IIF(perpec = 'LI',  ;
       'Liberado', '       ') :H =  ;
       'Modo' :R NOMENU NOAPPEND  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_2 KEY m.periodo +  ;
       m.numref
SELECT iteoc
SEEK m.periodo + m.numref
vtothoc = 0
vtotreb = 0
SCAN WHILE periodo + numoc =  ;
     m.periodo + m.numref
     IF iteoc.perpec <> 'LI'
          IF canreb > 0
               IF prereb > 0
                    REPLACE anttot  ;
                            WITH  ;
                            canreb *  ;
                            prereb
               ELSE
                    REPLACE anttot  ;
                            WITH  ;
                            canreb *  ;
                            preuni
               ENDIF
          ELSE
               IF prereb > 0
                    REPLACE anttot  ;
                            WITH  ;
                            canreq *  ;
                            prereb
               ELSE
                    REPLACE anttot  ;
                            WITH  ;
                            canreq *  ;
                            preuni
               ENDIF
          ENDIF
          SELECT itepec
          SEEK iteoc.periodo +  ;
               iteoc.numpec +  ;
               iteoc.codfte +  ;
               iteoc.codart
          SCAN WHILE  ;
               iteoc.periodo +  ;
               iteoc.numpec +  ;
               iteoc.codfte +  ;
               iteoc.codart =  ;
               periodo + numpec +  ;
               codfte + codart
               IF preuni =  ;
                  iteoc.preuni
                    REPLACE preuni  ;
                            WITH  ;
                            IIF(iteoc.prereb >  ;
                            0,  ;
                            iteoc.prereb,  ;
                            preuni)
               ENDIF
          ENDSCAN
          SELECT iteoc
          vtothoc = vtothoc +  ;
                    (valtot -  ;
                    anttot)
          vtotreb = vtotreb +  ;
                    anttot
          IF  .NOT. EMPTY(numpec)
               = agre_itoc1(m.periodo +  ;
                 numpec,codart)
          ENDIF
     ELSE
          SELECT iteoc
     ENDIF
ENDSCAN
m.valtot = vtotreb
SELECT itepec
SET ORDER TO ITEPEC1
SELECT iteoc
SEEK m.periodo + m.numref
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT orden
IF LASTKEY() = 27
     RETURN .F.
ENDIF
SET RELATION TO
DO trab_hijo
SELECT (vbase)
GOTO vregi
RETURN
*
FUNCTION trab_hijo
as = ALIAS()
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°F5->Agregar°°°°°°°°°°F8->Eliminar°°°°°°°°°°°F10->Terminar°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteoc1
SET ORDER TO ITEOC11
SEEK m.periodo + m.numref
BROWSE FIELDS codcad :H =  ;
       'Cadena' :V =  ;
       val_codcad(ALLTRIM(codcad), ;
       periodo,'CodCad') :F,  ;
       codcom :H = 'Componente'  ;
       :V = val_comp(periodo +  ;
       maepre.uniges +  ;
       maepre.unieje + codcad, ;
       codcom,'codcom') :F,  ;
       codmet :H = 'Meta' :V =  ;
       val_meta(periodo +  ;
       maepre.uniges +  ;
       maepre.unieje + codcad, ;
       codcom + codmet,'codmet')  ;
       :F, codpart :H = 'Partida'  ;
       :V = val_cale(codpart, ;
       periodo + maepre.uniges +  ;
       maepre.unieje +  ;
       maepre.codfun +  ;
       maepre.codprg +  ;
       maepre.codspr +  ;
       maepre.actpry + codcom +  ;
       codmet + ALLTRIM(codfte) +  ;
       PADL(MONTH(m.fecref), 2,  ;
       '0'),'codpart') :F,  ;
       valpart :H = 'Monto' :P =  ;
       '99,999,999.99', valreb :H =  ;
       'Rebaja' :P =  ;
       '99,999,999.99' NOMENU  ;
       NOAPPEND NODELETE WINDOW  ;
       wind_2a KEY m.periodo +  ;
       m.numref
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SELECT iteoc1
SEEK m.periodo + m.numref
vtotcom = 0
SCAN WHILE periodo + numoc =  ;
     m.periodo + m.numref
     vtotcom = vtotcom + (valpart -  ;
               valreb)
     IF valpart = 0
          DELETE NEXT 1
     ENDIF
ENDSCAN
SHOW MENU mmenu
ON KEY LABEL F10
SET FILTER TO
SELECT (as)
IF LASTKEY() = 27
     RETURN .F.
ENDIF
RETURN .T.
*
PROCEDURE agre_itoc1
PARAMETER vkey, vcodart
vrecno = RECNO()
valias = ALIAS()
SELECT iteoc
= val_codcad(ALLTRIM(iteoc.codcad), ;
  m.periodo,'C')
vuniges = maepre.uniges
vunieje = maepre.unieje
vcodcom = maepre.codcom
vcodmet = maepre.codmet
SELECT iteoc1
SET ORDER TO iteoc13
SEEK m.periodo + m.numref +  ;
     iteoc.codcad + vcodcom +  ;
     vcodmet
IF FOUND()
     IF vingre
          REPLACE valreb WITH  ;
                  valreb +  ;
                  iteoc.anttot
     ELSE
          REPLACE valreb WITH  ;
                  vvalr +  ;
                  iteoc.anttot
          vvalr = valreb
     ENDIF
ELSE
     IF f_appd()
          REPLACE periodo WITH  ;
                  m.periodo,  ;
                  numoc WITH  ;
                  m.numref,  ;
                  codcad WITH  ;
                  iteoc.codcad,  ;
                  uniges WITH  ;
                  vuniges, unieje  ;
                  WITH vunieje,  ;
                  codcom WITH  ;
                  vcodcom, codmet  ;
                  WITH vcodmet,  ;
                  valpart WITH  ;
                  iteoc.anttot
     ENDIF
ENDIF
SELECT (valias)
GOTO vrecno
RETURN
*
PROCEDURE elimi_ic
PRIVATE vfun
vfun = .F.
SELECT itepec
vorder = ORDER()
SET ORDER TO ITEPEC7
SEEK iteoc.periodo + iteoc.numoc +  ;
     iteoc.codfte + iteoc.codart +  ;
     iteoc.numord
IF FOUND()
     IF RLOCK()
          IF  .NOT. EMPTY(numsc)
               REPLACE orden WITH  ;
                       ' ',  ;
                       estado  ;
                       WITH '20',  ;
                       numoc WITH  ;
                       SPACE(4)
          ELSE
               REPLACE orden WITH  ;
                       ' ',  ;
                       estado  ;
                       WITH '00',  ;
                       numoc WITH  ;
                       SPACE(4)
          ENDIF
          vfun = .T.
     ENDIF
     UNLOCK
ELSE
     IF m.tipo = 'M'
          vfun = .T.
     ENDIF
ENDIF
SELECT itepec
SET ORDER TO (vorder)
SELECT iteoc
IF vfun
     IF RLOCK()
          REPLACE perpec WITH  ;
                  'LI'
     ENDIF
     UNLOCK
ELSE
     DO standby WITH  ;
        'No se ubic¢ el Pecosa ...'
ENDIF
RETURN
*
PROCEDURE rev_ic
PRIVATE vfun
vfun = .F.
SELECT itepec
vorder = ORDER()
SET ORDER TO ITEPEC7
SEEK iteoc.periodo + iteoc.numpec +  ;
     iteoc.codfte + iteoc.codart +  ;
     iteoc.numord
IF FOUND()
     IF RLOCK()
          IF  .NOT. EMPTY(numsc)
               REPLACE orden WITH  ;
                       'û',  ;
                       estado  ;
                       WITH '30',  ;
                       numoc WITH  ;
                       iteoc.numoc
          ENDIF
          vfun = .T.
     ENDIF
     UNLOCK
ELSE
     IF m.tipo = 'M'
          vfun = .T.
     ENDIF
ENDIF
SELECT itepec
SET ORDER TO (vorder)
SELECT iteoc
IF vfun
     IF RLOCK()
          REPLACE perpec WITH  ;
                  m.periodo
     ENDIF
     UNLOCK
ELSE
     DO standby WITH  ;
        'No se ubic¢ el Pecosa ...'
ENDIF
RETURN
*
FUNCTION siprv
PARAMETER vfun
vfun = .T.
IF  .NOT. v_reg = 'VG'
     DO standby WITH  ;
        'El Contratista no esta Regularizado...Observaci¢n'
ENDIF
RETURN vfun
*
PROCEDURE asiord
USE IN 0 AsiAut ORDER AsiAut1
SELECT asiaut
IF vopcion = 1
     vtip = 'ROC'
ELSE
     vtip = 'ROS'
ENDIF
SEEK vtip + '   ' + 'ASTORD'
IF  .NOT. FOUND()
     DO standby WITH  ;
        'PARAMETRO DE CTAS. DE ORDEN INICIALIZADO, CONSULTE AL AREA DE SISTEMAS'
     RETURN
ELSE
     cctad = dcuenta
     cctah = hcuenta
ENDIF
SELECT astord
SEEK m.periodo + m.mespa +  ;
     m.numpa + vtip
IF FOUND()
     FOR i = 1 TO 2
          IF f_lock(1) .OR.  ;
             RLOCK()
               REPLACE periodo  ;
                       WITH  ;
                       m.periodo,  ;
                       nummes  ;
                       WITH  ;
                       m.mespa,  ;
                       numref  ;
                       WITH  ;
                       m.numpa,  ;
                       tipdoc  ;
                       WITH vtip,  ;
                       fecha WITH  ;
                       m.fecpa,  ;
                       codcta  ;
                       WITH IIF(i =  ;
                       1, cctad,  ;
                       cctah),  ;
                       tipcta  ;
                       WITH IIF(i =  ;
                       1, 'D',  ;
                       'H'),  ;
                       mtodeb  ;
                       WITH IIF(i =  ;
                       1, vtotcom,  ;
                       0), mtohab  ;
                       WITH IIF(i =  ;
                       2, vtotcom,  ;
                       0)
               UNLOCK
               SKIP
          ENDIF
     ENDFOR
ELSE
     FOR i = 1 TO 2
          IF f_appd()
               REPLACE periodo  ;
                       WITH  ;
                       m.periodo,  ;
                       nummes  ;
                       WITH  ;
                       m.mespa,  ;
                       numref  ;
                       WITH  ;
                       m.numpa,  ;
                       tipdoc  ;
                       WITH vtip,  ;
                       fecha WITH  ;
                       m.fecpa,  ;
                       codcta  ;
                       WITH IIF(i =  ;
                       1, cctad,  ;
                       cctah),  ;
                       tipcta  ;
                       WITH IIF(i =  ;
                       1, 'D',  ;
                       'H'),  ;
                       mtodeb  ;
                       WITH IIF(i =  ;
                       1, vtotcom,  ;
                       0), mtohab  ;
                       WITH IIF(i =  ;
                       2, vtotcom,  ;
                       0)
               UNLOCK
          ENDIF
     ENDFOR
ENDIF
USE IN asiaut
DEFINE WINDOW wastord FROM 10, 10  ;
       TO 15, 70 TITLE  ;
       ' ASIENTOS DE ORDEN' COLOR  ;
       SCHEME 02
ACTIVATE WINDOW wastord
@ 00, 08 SAY 'Cuentas '
@ 00, 18 SAY '        Debe '
@ 00, 34 SAY '        Haber '
@ 01, 04 SAY cctad PICTURE  ;
  '!!!!!!!!!!!'
@ 01, 18 SAY vtotcom PICTURE  ;
  '99,999,999.99'
@ 02, 12 SAY cctah PICTURE  ;
  '!!!!!!!!!!!'
@ 02, 34 SAY vtotcom PICTURE  ;
  '99,999,999.99'
WAIT ' '
DEACTIVATE WINDOW wastord
RELEASE WINDOW wastord
RETURN
*

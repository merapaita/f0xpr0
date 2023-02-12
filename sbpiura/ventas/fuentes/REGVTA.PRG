CLOSE DATABASES
USE IN 1 Ventas   ALIAS ventas   ORDER Ventas1
USE IN 2 Parmae   ALIAS parma    ORDER Parmae1
USE IN 3 Clientes ALIAS clien    ORDER Clientes1
USE IN 5 Creditos ALIAS creditos ORDER Creditos1
USE IN 6 Caja     ALIAS caja     ORDER Caja1
USE IN 7 Subvenc  ALIAS subvenc  ORDER Subvenc1
USE IN 8 Donac    ALIAS donac    ORDER Donac1
USE IN 9 KarCre   ALIAS karcre   ORDER KarCre1
vmens01 = 'Registro de Ventas'
vmens02 = ' Ventas : REVISION '
vmens04 = 'Dicha Venta no fue encontrada'
vmens05 = 'No existe Venta anterior'
vmens06 = 'No existe Venta siguiente'
vmens07 = '¨ Desea Eliminar ‚sta Venta ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Venta ha sido anulado'
vmens10 = 'La Venta ya est  Atendido'
vmens11 = 'La Venta ha sido devuelto'
SELECT ventas
GOTO BOTTOM
SCATTER BLANK MEMVAR
DO inicia
HIDE POPUP ALL
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anula    Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
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
       'A\<nula  ' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Listar ' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO corri
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD elimi OF mmenu DO anula
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_0
CLEAR
@ 00, 50 SAY ' Estado :'
@ 01, 5 SAY  ;
  '             Venta :'
@ 02, 5 SAY  ;
  '     Tipo de Venta :'
@ 03, 5 SAY  ;
  '         Condicion :'
@ 04, 5 SAY  ;
  '               C/P :'
@ 05, 5 SAY  ;
  '           Cliente :'
@ 06, 5 SAY  ;
  '              Aval :'
@ 07, 5 SAY  ;
  'Nomb. Beneficiario :'
@ 08, 5 SAY  ;
  '        Cementerio :'
@ 09, 5 SAY  ;
  '           Cuartel :'
@ 10, 5 SAY  ;
  '             Nicho :'
@ 12, 5 SAY  ;
  'Subvencionado (S/N):'
@ 13, 5 SAY  ;
  '     Donacion (S/N):'
@ 11, 38 SAY ' Cod. Credito :'
@ 12, 38 SAY 'Cod. Subvenc. :'
@ 13, 38 SAY 'Cod. Donaci¢n :'
@ 14, 5 SAY  ;
  '     Importe Total :'
@ 15, 5 SAY  ;
  ' Mto Subvencionado :'
@ 16, 5 SAY  ;
  '        Mto Donado :'
@ 17, 5 SAY  ;
  '     Mto Cancelado :'
@ 18, 5 SAY  ;
  '           Credito :'
@ 19, 5 SAY  ;
  '     Observaciones :'
RETURN
*
PROCEDURE vista
SELECT ventas
ON KEY LABEL F4 DO Imprimir
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
IF m.camnom
     @ 00, 10 SAY  ;
       'Solo para Tramite Administradivo'  ;
       COLOR SCHEME 5
ELSE
     @ 00, 10 SAY SPACE(32)
ENDIF
@ 00, 60 SAY estvta()
@ 01, 26 SAY m.codvta
@ 01, 54 SAY m.fecvta
@ 02, 26 SAY m.tipvta
@ 02, 32 SAY val_para(m.tipvta, ;
  'TIPVTA','A',32,20)
@ 03, 26 SAY m.convta
@ 03, 32 SAY val_para(m.convta, ;
  'CONVTA','A',32,10)
@ 04, 26 SAY m.compag
@ 04, 32 SAY m.codcp
@ 04, 54 SAY m.feccp
@ 05, 26 SAY IIF( .NOT. m.camnom,  ;
  m.codcli, m.newcli)
@ 05, 32 SAY IIF( .NOT. m.camnom,  ;
  val_cli(m.codcli,5,33),  ;
  val_cli(m.newcli,5,33))
@ 06, 26 SAY m.codavl
@ 06, 32 SAY IIF(convta = '2',  ;
  val_avl(m.codavl,6,33),  ;
  SPACE(30))
@ 07, 26 SAY LEFT(m.nomben, 40)
@ 08, 26 SAY m.codcem
@ 08, 26 SAY val_para(m.codcem, ;
  'CODCEM','A',32,30)
@ 09, 26 SAY m.cuartel
@ 10, 26 SAY m.nicho
@ 12, 26 SAY m.subvenc
@ 13, 26 SAY m.donacion
@ 11, 54 SAY m.codcre
@ 12, 54 SAY m.codsub
@ 13, 54 SAY m.coddon
@ 14, 26 SAY m.mtovta PICTURE  ;
  '99,999.99'
@ 15, 26 SAY m.mtosub PICTURE  ;
  '99,999.99'
@ 16, 26 SAY m.mtodon PICTURE  ;
  '99,999.99'
@ 17, 26 SAY m.mtocan PICTURE  ;
  '99,999.99'
@ 18, 26 SAY m.mtocre PICTURE  ;
  '99,999.99'
@ 19, 26 SAY LEFT(m.observ, 50)
@ 20, 26 SAY SUBSTR(m.observ, 51,  ;
  50)
IF  .NOT. vflag $ 'J*'
     DO subopc
ENDIF
RETURN
*
PROCEDURE revis
ON KEY LABEL F4
SELECT ventas
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
DEFINE POPUP previs FROM 20, 40  ;
       COLOR SCHEME c_popup
DEFINE BAR 1 OF previs PROMPT  ;
       '\<a. Por Codigo Venta '
DEFINE BAR 2 OF previs PROMPT  ;
       '\<a. Por Codigo Cliente '
ON SELECTION POPUP previs DEACTIVATE POPUP
ACTIVATE POPUP previs
cord1 = ORDER()
DO CASE
     CASE BAR() = 1
          SET ORDER TO Ventas1
     CASE BAR() = 2
          SET ORDER TO Ventas2
ENDCASE
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL F10 KEYBOARD CHR(23)
SET RELATION TO codcli INTO clien
BROWSE FIELDS codvta :H =  ;
       'C¢digo', codcli :H =  ;
       'Cliente', clien.nomcli :H =  ;
       'Nombre', compag :H =  ;
       'Comprobante', codcp :H =  ;
       'N§' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_0
SET RELATION TO
SET ORDER TO &cOrd1
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELECT ventas
DO vista
RETURN
*
PROCEDURE busca
ON KEY LABEL F4
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
DEFINE POPUP plista FROM 20, 40  ;
       COLOR SCHEME c_popup
DEFINE BAR 1 OF plista PROMPT  ;
       '\<a. Por Codigo  '
ON SELECTION POPUP plista DEACTIVATE POPUP
ACTIVATE POPUP plista
cord1 = ORDER()
DO CASE
     CASE BAR() = 1
          vcodcem = SPACE(2)
          vbusca = SPACE(7)
          vnombre = 'Codigo :'
          SET ORDER TO Ventas1
     OTHERWISE
          vcodcem = ''
          vbusca = ''
          vnombre = ''
          SET ORDER TO
ENDCASE
IF LASTKEY() <> 27
     DEFINE WINDOW lista FROM 09,  ;
            12 TO 16, 68 FLOAT  ;
            TITLE  ;
            ' °° B£squeda °° '  ;
            DOUBLE COLOR SCHEME  ;
            5
     ACTIVATE WINDOW lista
     @ 3, 2 SAY vnombre GET  ;
       vbusca
     READ VALID val_read()
     DEACTIVATE WINDOW lista
     IF EMPTY(vbusca) .OR.  ;
        LASTKEY() = 27
     ELSE
          SEEK ALLTRIM(vbusca)
          IF  .NOT. FOUND()
               DO standby WITH  ;
                  vmens04
               GOTO vtemp
          ENDIF
     ENDIF
ENDIF
DO vista
SET ORDER TO &cOrd1
RETURN
*
PROCEDURE anter
SELECT ventas
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
SELECT ventas
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
ON KEY LABEL F4
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SELECT ventas
SCATTER MEMVAR
DO pantalla
@ 00, 60 SAY val_para(m.estado, ;
  'ESTVTA',' ',60,10)
@ 01, 26 GET m.codvta WHEN .F.
@ 01, 54 GET m.fecvta WHEN .F.
@ 02, 26 GET m.tipvta WHEN .F.
@ 02, 32 SAY val_para(m.tipvta, ;
  'TIPVTA','A',32,20)
@ 03, 26 GET m.convta WHEN .F.
@ 03, 32 SAY val_para(m.convta, ;
  'CONVTA','A',32,10)
@ 04, 26 GET m.compag WHEN .F.
@ 04, 32 GET m.codcp
@ 04, 54 GET m.feccp WHEN .F.
@ 05, 26 SAY IIF( .NOT. m.camnom,  ;
  m.codcli, m.newcli)
@ 05, 32 SAY IIF( .NOT. m.camnom,  ;
  val_cli(m.codcli,5,33),  ;
  val_cli(m.newcli,5,33))
@ 06, 26 GET m.codavl WHEN .F.
@ 06, 32 SAY IIF(m.convta = '2',  ;
  val_avl(m.codavl,6,33),  ;
  SPACE(30))
@ 07, 26 GET m.nomben PICTURE  ;
  '@!S30'
@ 08, 26 GET m.codcem VALID  ;
  val_para(m.codcem,'CODCEM',' ', ;
  26,30)
@ 09, 26 GET m.cuartel FUNCTION  ;
  '!'
@ 10, 26 GET m.nicho FUNCTION '!'
@ 12, 26 GET m.subvenc WHEN .F.
@ 13, 26 GET m.donacion WHEN .F.
@ 11, 54 GET m.codcre WHEN .F.
@ 12, 54 GET m.codsub WHEN .F.
@ 13, 54 GET m.coddon WHEN .F.
@ 14, 26 GET m.mtovta PICTURE  ;
  '99,999.99' WHEN .F.
@ 15, 26 GET m.mtosub PICTURE  ;
  '99,999.99' WHEN .F.
@ 16, 26 GET m.mtodon PICTURE  ;
  '99,999.99' WHEN .F.
@ 17, 26 GET m.mtocan PICTURE  ;
  '99,999.99' WHEN .F.
@ 18, 26 GET m.mtocre PICTURE  ;
  '99,999.99' WHEN .F.
@ 19, 26 GET m.observ PICTURE  ;
  '@S50'
READ VALID val_read()
IF LASTKEY() <> 27
     IF f_lock(1)
          SELECT ventas
          m.tuser = 'C'
          m.usercr = vuser_id
          m.dusercr = DATE()
          m.husercr = TIME()
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
FLUSH
SELECT ventas
DO vista
RETURN
*
PROCEDURE ingre
*--------------
ON KEY LABEL F4
SELECT ventas
vtemp = IIF(EOF(), -1, RECNO())
SCATTER BLANK MEMVAR
m.codent = SPACE(2)
DO pantalla
m.estado = '00'
STORE DATE() TO m.docref, m.numdoc, m.exped
STORE m.fecsis TO m.fecvta, m.feccp, m.fecsub, m.fecdon
= corvta()
@ 00, 60 SAY val_para(m.estado, 'ESTVTA',' ',60,10)
@ 01, 26 GET m.codvta WHEN .F.
@ 01, 54 GET m.fecvta WHEN .F.
@ 02, 26 GET m.tipvta VALID val_para(m.tipvta,'TIPVTA',' ', 26,20)
@ 03, 26 GET m.convta VALID val_para(m.convta,'CONVTA',' ', 26,10)
@ 04, 26 GET m.compag VALID val_para(m.compag,'COMPAG','C', 26,10)
@ 04, 32 GET m.codcp
@ 04, 54 GET m.feccp
@ 05, 26 GET m.codcli VALID val_cli(m.codcli,5,33) .AND. valestcli() .AND. val_ruc()
@ 06, 26 GET m.codavl VALID val_avl(m.codavl,6,33) .AND.  ;
  valestavl() WHEN m.convta = '2'
@ 07, 26 GET m.nomben FUNCTION '!' PICTURE '@S30'
@ 08, 26 GET m.codcem VALID val_para(m.codcem,'CODCEM',' ', 26,30)
@ 09, 26 GET m.cuartel FUNCTION '!'
@ 10, 26 GET m.nicho FUNCTION '!' WHEN (m.tipvta = '01' .OR. m.tipvta = '03' .OR. m.tipvta = '04')
@ 12, 26 GET m.subvenc PICTURE '@M N,S'
@ 13, 26 GET m.donacion PICTURE '@M N,S'
@ 11, 54 GET m.codcre WHEN wcodcre()
@ 12, 54 GET m.codsub WHEN IIF(m.subvenc = 'S', wcodsub(), .F.)
@ 13, 54 GET m.coddon WHEN IIF(m.donacion = 'S', wcoddon(), .F.)
@ 14, 26 GET m.mtovta PICTURE '99,999.99' VALID vmtovta()
@ 15, 26 GET m.mtosub PICTURE '99,999.99' WHEN .F.
@ 16, 26 GET m.mtodon PICTURE '99,999.99' WHEN .F.
@ 17, 26 GET m.mtocan PICTURE '99,999.99' VALID vmtocan()
@ 18, 26 GET m.mtocre PICTURE '99,999.99' WHEN .F.
@ 19, 26 GET m.observ PICTURE '@S50'
READ VALID val_read() .AND. valvta()
SELECT ventas
mact = .T.
IF LASTKEY() <> 27
     m.tuser = 'I'
     m.user = vuser_id
     m.duser = DATE()
     m.huser = TIME()
     DO CASE
          CASE m.mtocre = 0
               m.estado = '10'
          CASE m.mtocre > 0
               m.estado = '20'
     ENDCASE
     IF m.mtocan > 0
          SELECT caja
          m.tipcaj = '1'
          m.fecreg = m.fecvta
          m.coring = m.codvta
          m.importe = m.mtocan
          IF f_appd()
               GATHER MEMVAR
               REPLACE estado WITH '00'
               UNLOCK
          ENDIF
     ENDIF
     IF m.mtocre > 0
          m.feccre = m.fecvta
          m.codmov = m.codvta
          m.fultpag = m.fecvta
          xx = actkarcre('CRE', m.codmov, m.feccre)
          SELECT clien
          IF f_lock(1)
               REPLACE estado WITH '10'
               UNLOCK
          ENDIF
          cmes = PADL(ALLTRIM(STR(MONTH(m.feccre))), 2, '0')
          cano = RIGHT(STR(YEAR(m.feccre), 4), 2)
          SELECT parma
          IF  .NOT. SEEK('CREDIT' + cano + cmes)
               IF f_appd()
                    REPLACE tipo WITH 'CREDIT',  ;
                            codigo  ;
                            WITH  ;
                            cano,  ;
                            codigoaux  ;
                            WITH  ;
                            cmes,  ;
                            descri  ;
                            WITH  ;
                            'CORRELATIVO DE CREDITOS: ' +  ;
                            cmes +  ;
                            '/' +  ;
                            cano,  ;
                            nument  ;
                            WITH  ;
                            1
               ENDIF
          ELSE
               IF f_lock(1)
                    REPLACE nument  ;
                            WITH  ;
                            parma.nument +  ;
                            1
               ENDIF
          ENDIF
          UNLOCK
     ENDIF
     IF m.subvenc = 'S'
          SELECT subvenc
          IF f_appd()
               GATHER MEMVAR
               REPLACE estado  ;
                       WITH '20'
               UNLOCK
          ENDIF
          SELECT parma
          cmes = PADL(ALLTRIM(STR(MONTH(m.fecsub))),  ;
                 2, '0')
          cano = RIGHT(STR(YEAR(m.fecsub),  ;
                 4), 2)
          IF  .NOT. SEEK('CORSUB' +  ;
              cano + cmes)
               IF f_appd()
                    REPLACE tipo  ;
                            WITH  ;
                            'CORSUB',  ;
                            codigo  ;
                            WITH  ;
                            cano,  ;
                            codigoaux  ;
                            WITH  ;
                            cmes,  ;
                            descri  ;
                            WITH  ;
                            'CORRELATIVO DE SUBVENCIONES: ' +  ;
                            cmes +  ;
                            '/' +  ;
                            cano
                    UNLOCK
               ENDIF
          ENDIF
          IF f_lock(1)
               REPLACE nument  ;
                       WITH  ;
                       parma.nument +  ;
                       1
          ENDIF
          UNLOCK
     ENDIF
     IF m.donacion = 'S'
          SELECT donac
          IF f_appd()
               GATHER MEMVAR
               REPLACE estado  ;
                       WITH '20'
               UNLOCK
          ENDIF
          SELECT parma
          cmes = PADL(ALLTRIM(STR(MONTH(m.fecdon))),  ;
                 2, '0')
          cano = RIGHT(STR(YEAR(m.fecdon),  ;
                 4), 2)
          IF  .NOT. SEEK('CORDON' +  ;
              cano + cmes)
               IF f_appd()
                    REPLACE tipo  ;
                            WITH  ;
                            'CORDON',  ;
                            codigo  ;
                            WITH  ;
                            cano,  ;
                            codigoaux  ;
                            WITH  ;
                            cmes,  ;
                            descri  ;
                            WITH  ;
                            'CORRELATIVO DE DONACIONES: ' +  ;
                            cmes +  ;
                            '/' +  ;
                            cano
                    UNLOCK
               ENDIF
          ENDIF
          IF f_lock(1)
               REPLACE nument  ;
                       WITH  ;
                       parma.nument +  ;
                       1
          ENDIF
          UNLOCK
     ENDIF
     IF mact
          SELECT ventas
          IF f_appd()
               GATHER MEMVAR
               REPLACE estado  ;
                       WITH  ;
                       m.estado
          ENDIF
          SELECT parma
          cmes = PADL(ALLTRIM(STR(MONTH(m.fecvta))),  ;
                 2, '0')
          cano = RIGHT(STR(YEAR(m.fecvta),  ;
                 4), 2)
          IF  .NOT. SEEK('VENTA ' +  ;
              cano + cmes)
               IF f_appd()
                    REPLACE tipo  ;
                            WITH  ;
                            'VENTA ',  ;
                            codigo  ;
                            WITH  ;
                            cano,  ;
                            codigoaux  ;
                            WITH  ;
                            cmes,  ;
                            descri  ;
                            WITH  ;
                            'CORRELATIVO DE VENTAS: ' +  ;
                            cmes +  ;
                            '/' +  ;
                            cano
                    UNLOCK
               ENDIF
          ENDIF
          IF f_lock(1)
               REPLACE nument  ;
                       WITH  ;
                       parma.nument +  ;
                       1
          ENDIF
          UNLOCK ALL
     ENDIF
ELSE
     IF EOF()
          GOTO BOTTOM
     ELSE
          GOTO vtemp
     ENDIF
ENDIF
SELECT ventas
FLUSH
DO vista
RETURN
*
FUNCTION corvta
PRIVATE cmes, cano, calias, mret
calias = ALIAS()
SELE &cAlias
mret = .T.
cmes = PADL(ALLTRIM(STR(MONTH(m.fecvta))),  ;
       2, '0')
cano = RIGHT(STR(YEAR(m.fecvta),  ;
       4), 2)
IF SEEK('VENTA ' + cano + cmes,  ;
   'Parma')
     m.codvta = cano + cmes +  ;
                PADL(ALLTRIM(STR(parma.nument +  ;
                1)), 3, '0')
     SHOW GET m.codvta
ELSE
     m.codvta = cano + cmes +  ;
                PADL(ALLTRIM(STR(parma.nument +  ;
                1)), 3, '0')
     SHOW GET m.codvta
ENDIF
SELECT (calias)
IF EMPTY(m.codvta)
     DO standby WITH  ;
        'El C¢digo esta vacio'
     mret = .F.
ELSE
     nreg = RECNO()
     IF SEEK(m.codvta)
          DO standby WITH  ;
             'Ya esta Registrado esta Venta'
          mret = .F.
     ENDIF
ENDIF
RETURN mret
*
FUNCTION wcorsub
PRIVATE calias, cmes, cano
cmes = PADL(ALLTRIM(STR(MONTH(m.fecvta))),  ;
       2, '0')
cano = RIGHT(STR(YEAR(m.fecvta),  ;
       4), 2)
calias = ALIAS()
IF SEEK('CORSUB' + cano + cmes,  ;
   'Parma')
     m.codsub = cano + cmes +  ;
                PADL(ALLTRIM(STR(parma.nument +  ;
                1)), 3, '0')
ELSE
     m.codsub = cano + cmes +  ;
                PADL(ALLTRIM(STR(1)),  ;
                3, '0')
ENDIF
SHOW GET m.codsub
RETURN .F.
*
FUNCTION wcordon
PRIVATE calias, cmes, cano
cmes = PADL(ALLTRIM(STR(MONTH(m.fecvta))),  ;
       2, '0')
cano = RIGHT(STR(YEAR(m.fecvta),  ;
       4), 2)
calias = ALIAS()
IF SEEK('CORDON' + cano + cmes,  ;
   'Parma')
     m.coddon = cano + cmes +  ;
                PADL(ALLTRIM(STR(parma.nument +  ;
                1)), 3, '0')
ELSE
     m.coddon = cano + cmes +  ;
                PADL(ALLTRIM(STR(1)),  ;
                3, '0')
ENDIF
SHOW GET m.coddon
RETURN .F.
*
FUNCTION wcodcre
PRIVATE calias, cmes, cano
cmes = PADL(ALLTRIM(STR(MONTH(m.fecvta))),  ;
       2, '0')
cano = RIGHT(STR(YEAR(m.fecvta),  ;
       4), 2)
calias = ALIAS()
IF m.convta = '2'
     IF SEEK('CREDIT' + cano +  ;
        cmes, 'Parma')
          m.codcre = cano + cmes +  ;
                     PADL(ALLTRIM(STR(parma.nument +  ;
                     1)), 3,  ;
                     '0')
     ELSE
          m.codcre = cano + cmes +  ;
                     PADL(ALLTRIM(STR(1)),  ;
                     3, '0')
     ENDIF
ELSE
     m.codcre = SPACE(7)
ENDIF
SHOW GET m.codcre
RETURN .F.
*
FUNCTION vmtocan
m.mtocre = m.mtovta - m.mtocan -  ;
           m.mtosub - m.mtodon
mret = .T.
SHOW GET m.mtocre
RETURN mret
*
FUNCTION valvta
mret = .T.
IF LASTKEY() <> 27
     IF m.mtocre < 0
          DO standby WITH  ;
             'El Mto del Credito no debe ser menor que 0'
          mret = .F.
     ENDIF
     IF m.convta = '1' .AND.  ;
        m.mtocre > 0
          DO standby WITH  ;
             'La venta es al contado; por lo tanto no hay credito'
          mret = .F.
     ENDIF
     IF m.convta = '2' .AND.  ;
        m.mtocre = 0
          DO standby WITH  ;
             'El Credito debe ser mayor que cero, de lo contrario es una venta al contado'
          mret = .F.
     ENDIF
     IF m.subvenc = 'S' .AND.  ;
        m.mtosub = 0
          DO standby WITH  ;
             'La Subvenci¢n debe ser mayor que cero, de lo contrario no hay Subvenci¢n'
          mret = .F.
     ENDIF
     IF m.donacion = 'S' .AND.  ;
        m.mtodon = 0
          DO standby WITH  ;
             'La Donaci¢n debe ser mayor que cero, de lo contrario no hay Donaci¢n'
          mret = .F.
     ENDIF
ENDIF
RETURN mret
*
FUNCTION vmtovta
IF m.mtovta < m.mtosub
     DO standby WITH  ;
        'El Importe de la Venta no debe ser menor a la Subvencion'
     mret = .F.
ELSE
     mret = .T.
ENDIF
RETURN mret
*
FUNCTION val_ruc
PRIVATE calias
calias = ALIAS()
IF SEEK(m.codcli, 'Clien')
     m.ruccli = clien.ruccli
ELSE
     m.ruccli = SPACE(11)
ENDIF
SHOW GET m.ruccli
RETURN .T.
*
FUNCTION wcodsub
mret = .T.
malias = ALIAS()
SELECT subvenc
DEFINE WINDOW w_sub FROM 05, 05  ;
       TO 20, 70 TITLE  ;
       'Ingresando Detalles de Subvencion'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW w_sub
m.codsub = SPACE(7)
m.fecsub = m.fecvta
m.docref = SPACE(3)
m.numdoc = SPACE(15)
m.exped = SPACE(15)
m.mtosub = 0
@ 01, 01 SAY  ;
  '     Subvencion N§ :' GET  ;
  m.codsub WHEN wcorsub()
@ 02, 01 SAY  ;
  '             Fecha :' GET  ;
  m.fecsub WHEN .F.
@ 03, 01 SAY  ;
  ' Ent. Beneficiaria :' GET  ;
  m.codent PICTURE '@S40' VALID  ;
  val_para(m.codent,'ENTSUB',' ', ;
  22,40)
@ 04, 01 SAY  ;
  '   Doc. Referencia :' GET  ;
  m.docref VALID  ;
  val_para(m.docref,'DOCREF',' ', ;
  22,40)
@ 05, 01 SAY  ;
  '    Num. Documento :' GET  ;
  m.numdoc
@ 06, 01 SAY  ;
  '        Expediente :' GET  ;
  m.exped
@ 07, 01 SAY  ;
  '      C¢digo Venta :' GET  ;
  m.codvta WHEN .F.
@ 09, 01 SAY  ;
  '    C¢digo Cliente :' GET  ;
  m.codcli WHEN .F.
@ 11, 01 SAY  ;
  '      Beneficiario :' GET  ;
  m.nomben WHEN .F.
@ 13, 01 SAY  ;
  '  Monto Subvenci¢n :' GET  ;
  m.mtosub
READ VALID val_read()
DEACTIVATE WINDOW w_sub
RELEASE WINDOW w_sub
SHOW GET m.codsub
SHOW GET m.mtosub
mret = .F.
SELECT (malias)
RETURN mret
*
FUNCTION wcoddon
mret = .T.
malias = ALIAS()
SELECT donac
DEFINE WINDOW w_don FROM 05, 05  ;
       TO 20, 70 TITLE  ;
       'Ingresando Detalles de Donaci¢n'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW w_don
m.coddon = SPACE(7)
m.fecdon = m.fecvta
m.mtodon = 0
@ 01, 01 SAY '     Donaci¢n N§ :'  ;
  GET m.coddon WHEN wcordon()
@ 03, 01 SAY '           Fecha :'  ;
  GET m.fecdon WHEN .F.
@ 07, 01 SAY '    C¢digo Venta :'  ;
  GET m.codvta WHEN .F.
@ 09, 01 SAY '  C¢digo Cliente :'  ;
  GET m.codcli WHEN .F.
@ 11, 01 SAY '    Beneficiario :'  ;
  GET m.nomben WHEN .F.
@ 13, 01 SAY '  Monto Donaci¢n :'  ;
  GET m.mtodon
READ VALID val_read()
DEACTIVATE WINDOW w_don
RELEASE WINDOW w_don
SHOW GET m.coddon
SHOW GET m.mtodon
mret = .F.
SELECT (malias)
RETURN mret
*
PROCEDURE imprimir
*-----------------
PRIVATE vcon
SELECT ventas
vcon = RECNO()
SCATTER MEMVAR
m.fecrec = m.feccp
m.impcan = letras(m.mtocan,'SOLES')
m.cement = val_para(m.codcem, 'CODCEM','V',32,30)
IF !EMPTY(m.codcli)
	IF SEEK(m.codcli, 'Clien')
		m.nomcli = clien.nomcli
		m.dni = clien.dnicli
		m.dircli = clien.dircli
		m.ruccli = clien.ruccli
	ENDIF
ENDIF
IF !EMPTY(m.newcli)
	IF SEEK(m.newcli, 'Clien')
		m.nomcli = clien.nomcli
		m.dni = clien.dnicli
		m.dircli = clien.dircli
		m.ruccli = clien.ruccli
	ENDIF
ENDIF
IF !EMPTY(m.codavl)
	IF SEEK(m.codavl, 'Clien')
		m.nomavl = clien.nomcli
		m.diravl = clien.dircli
		m.ruccli = clien.ruccli
	ENDIF
ENDIF
vdbf = SYS(3) + '.Dbf'
CREATE CURSOR Recibo (codvta C (7), tipvta C (3), convta C (1), ;
					  fecrec D, compag C (3), codcp C (20), ;
					  codcli C (6), nomcli C (70), dircli C (70), ;
					  dni C (8), ruccli C (11), codavl C (6), ;
					  nomavl C (70), diravl C (70), nomben C (70), ;
					  cement C (30), cuartel C (30), nicho C (5), ;
					  subvenc C (1), donacion C (1), codsub C (7), ;
					  coddon C (7), codcre C (7), estado C (2), ;
					  mtovta N (10, 2), mtosub N (10, 2), mtodon N (10, 2), ;
					  mtocan N (10, 2), mtocre N (10, 2), impcan C (200),;
					  observ C (100))
SELECT recibo
APPEND BLANK
GATHER MEMVAR
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO reporte WITH 2, 'Recibo', ' Orden de Servicios '
ENDIF
SET FILTER TO
SELECT ventas
GOTO vcon
DO vista
RETURN
*
PROCEDURE anula
PRIVATE lanula
lanula = .T.
IF yesno( ;
   'Desea Anular este registro')
     IF  .NOT. ve_passw('ELIDIF')
          DO standby WITH  ;
             'Proceso Cancelado'
          lanula = .F.
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso Cancelado'
     lanula = .F.
ENDIF
IF lanula
     IF m.estado = '99'
          DO standby WITH  ;
             'La Venta ya ha sido anulado.'
          lanula = .F.
     ENDIF
ENDIF
IF lanula
     SELECT karcre
     xcre = m.codcre
     IF SEEK(m.codcre)
          xcon = 0
          SCAN WHILE codcre =  ;
               m.codcre
               xcon = xcon + 1
          ENDSCAN
          IF xcon > 1
               DO standby WITH  ;
                  'Este Credito no se puede anular por que ya tiene pagos a cuenta. Avise a Area de Sistemas'
               lanula = .F.
          ELSE
               lanula = .T.
          ENDIF
     ENDIF
ENDIF
IF lanula
     IF m.mtocan > 0
          SELECT caja
          IF SEEK('1' + m.codvta)
               IF f_lock(1)
                    REPLACE estado  ;
                            WITH  ;
                            '99'
                    UNLOCK
               ENDIF
          ELSE
               lanula = .F.
          ENDIF
     ENDIF
     IF lanula
          IF m.mtocre > 0
               SELECT karcre
               IF SEEK(m.codcre)
                    IF f_lock(1)
                         REPLACE estado  ;
                                 WITH  ;
                                 '99'
                         UNLOCK
                    ENDIF
               ELSE
                    lanula = .F.
               ENDIF
               SELECT creditos
               IF SEEK(m.codcre)
                    IF f_lock(1)
                         REPLACE estado  ;
                                 WITH  ;
                                 '99'
                         UNLOCK
                    ENDIF
               ELSE
                    lanula = .F.
               ENDIF
          ENDIF
     ENDIF
     IF lanula
          IF m.subvenc = 'S'
               SELECT subvenc
               IF SEEK(m.codsub)
                    IF f_lock(1)
                         REPLACE estado  ;
                                 WITH  ;
                                 '99'
                         UNLOCK
                    ENDIF
               ELSE
                    lanula = .F.
               ENDIF
          ENDIF
     ENDIF
     IF lanula
          IF m.donacion = 'S'
               SELECT donac
               IF SEEK(m.coddon)
                    IF f_lock(1)
                         REPLACE estado  ;
                                 WITH  ;
                                 '99'
                         UNLOCK
                    ENDIF
               ELSE
                    lanula = .F.
               ENDIF
          ENDIF
     ENDIF
     IF lanula
          SELECT ventas
          IF f_lock(1)
               REPLACE estado  ;
                       WITH '99'
               UNLOCK ALL
          ENDIF
     ELSE
          DO standby WITH  ;
             'Ocurrio un Error. Avise a Area de Sistemas'
     ENDIF
ENDIF
DO vista
RETURN
*
PROCEDURE lista
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
nreg = RECNO()
SELECT ventas
SET RELATION TO codcli INTO clien
DEFINE WINDOW wlista FROM 3, 15 TO 20, 70 FLOAT TITLE  ;
       'Listado de Ventas' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW wlista
vtove = 1
vtoti = 1
ltcon = 1
mfecha1 = CTOD('01/01/' +  STR(YEAR(m.fecsis), 4))
mfecha2 = DATE()
mtipvta = '  '
@ 01, 01 SAY '  Todos las Ventas : ' GET vtove SIZE 1, 10, 2 FUNCTION '*RNH \<Si;\<No'
@ 03, 01 SAY '              Venta: ' GET m.codvta WHEN vtove = 2
@ 05, 01 SAY '   Todos los Tipos : ' GET vtoti SIZE 1, 10, 2 FUNCTION '*RNH \<Si;\<No' WHEN vtove = 1
@ 07, 01 SAY '      Tipo de Venta: ' GET mtipvta VALID val_para(mtipvta, 'TIPVTA',' ',23,20) WHEN vtoti = 2
@ 09, 01 SAY '  Modalid. de Pago : ' GET ltcon FUNCTION '^ Todos;Ventas Contado;Ventas Credito;Donac/Subvenc' WHEN vtove = 1
@ 13, 01 SAY '    Rango de Fechas: ' GET mfecha1 WHEN vtove = 1
@ 13, 36 GET mfecha2 WHEN vtove = 1
@ 15, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
IF LASTKEY() <> 27 .AND. okcancel <> 2
     IF vtove = 2
          IF SEEK(m.codvta)
               SET FILTER TO codvta = m.codvta
               DO reporte WITH 2, 'Ventas', 'Listado de Ventas'
               SET FILTER TO
          ELSE
               DO standby WITH 'Venta no encontrada'
          ENDIF
     ELSE
          lfil = '.T.'
          IF vtoti = 2
               lfil = lfil + ' AND TipVta = mTipVta'
          ENDIF
          DO CASE
               CASE ltcon = 1
                    lfil = lfil + ' AND .T.'
               CASE ltcon = 2
                    lfil = lfil + " AND ConVta = '1'"
               CASE ltcon = 3
                    lfil = lfil + " AND ConVta = '2'"
               CASE ltcon = 4
                    lfil = lfil + " AND (Donacion = 'S' OR subvenc = 'S')"
          ENDCASE
          SET FILTER TO &lFil AND BETW(FecVta,mFecha1,mFecha2) AND !CamNom AND Estado#'99'
          DO reporte WITH 2, 'Ventas',  ;
             'Listado de Ventas'
          SET FILTER TO
     ENDIF
ENDIF
SELECT ventas
SET RELATION TO
GOTO nreg
DO vista
RETURN
*
FUNCTION rettv
PARAMETER mtv
PRIVATE mret
IF SEEK('TIPVTA' + mtv, 'Parma')
     mret = parma.descri
ELSE
     mret = 'Sin Descripci¢n'
ENDIF
RETURN mret
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
ERASE &vDbf
RELEASE WINDOW wind_0
RELEASE MENU mmenu
RETURN
*
PROCEDURE estvta
DO CASE
     CASE m.estado = '10'
          @ ROW(), COL() SAY  ;
            'CANCELADO' COLOR  ;
            SCHEME 5
     CASE m.estado = '20'
          @ ROW(), COL() SAY  ;
            'CON SALDO' COLOR  ;
            SCHEME 5
     CASE m.estado = '99'
          @ ROW(), COL() SAY  ;
            'ANULADO' COLOR  ;
            SCHEME 5
ENDCASE
RETURN
*
PROCEDURE val_tipvt
PARAMETER vfil, vcol
PRIVATE mbus, mimp, calias
STORE .F. TO mbus, mimp
calias = ALIAS()
IF EMPTY(m.tipri)
     mbus = .T.
ELSE
     IF SEEK('VTA' + m.tipvta,  ;
        'AsiAut')
          mimp = .T.
     ELSE
          mbus = .T.
     ENDIF
ENDIF
IF mbus
     SELECT asiaut
     SET ORDER TO AsiAutRI2
     DEFINE POPUP v__xx FROM 1,  ;
            30 TO 12, 79 PROMPT  ;
            FIELDS descri
     ON SELECTION POPUP v__xx DEACTIVATE;
POPUP
     ACTIVATE POPUP v__xx
     RELEASE POPUP v__xx
     m.tipri = codigo
     SET ORDER TO AsiAutRi1
     mimp = .T.
ENDIF
IF mimp
     @ vfil, vcol SAY  ;
       asiaut.descri
ENDIF
SELECT (calias)
RETURN
*
PROCEDURE subopc
PRIVATE calias
calias = ALIAS()
USE IN 0 IteUsuOp ORDER IteUsuOp2
SET SKIP OF PAD revis OF mmenu;
 .NOT. SEEK(vusucla + '0002', 'IteUsuOp')
SET SKIP OF PAD busca OF mmenu;
 .NOT. SEEK(vusucla + '0003', 'IteUsuOp')
SET SKIP OF PAD anter OF mmenu;
 .NOT. SEEK(vusucla + '0004', 'IteUsuOp')
SET SKIP OF PAD proxi OF mmenu;
 .NOT. SEEK(vusucla + '0005', 'IteUsuOp')
SET SKIP OF PAD corri OF mmenu;
 .NOT. SEEK(vusucla + '0006', 'IteUsuOp')
SET SKIP OF PAD ingre OF mmenu;
 .NOT. SEEK(vusucla + '0007', 'IteUsuOp')
SET SKIP OF PAD elimi OF mmenu;
 .NOT. SEEK(vusucla + '0008', 'IteUsuOp')
SET SKIP OF PAD lista OF mmenu;
 .NOT. SEEK(vusucla + '0009', 'IteUsuOp')
USE IN iteusuop
SELECT (calias)
RETURN
*

FUNCTION Derecho
*---------------
cDerecho = ''
if tipvta='01'
	cDerecho = cDerecho + 'nicho'
endif
if tipvta='02'
	cDerecho = cDerecho + 'mausoleo'
endif
if tipvta='03'
	cDerecho = cDerecho + 'nicho para ocupacion futura'
endif
if tipvta='04'
	cDerecho = cDerecho + 'nicho para traslado'
endif
if tipvta='05'
	cDerecho = cDerecho + ' ataud'
endif
if tipvta='06'
	cDerecho = cDerecho + ' urna para cenizas'
endif

if ConVta='1'
	cDerecho = cDerecho + ' AL CONTADO'
endif
if ConVta='2'
	cDerecho = cDerecho + ' AL CREDITO'
endif

return cDerecho
CLOSE DATABASES
USE IN 1 PagDon ALIAS pagdon  ;
    ORDER PagDon1
USE IN 2 ItePD ALIAS itepd ORDER  ;
    ItePD1
USE IN 3 Donac ALIAS donac ORDER  ;
    Donac1
USE IN 4 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 5 Clientes ALIAS clien  ;
    ORDER Clientes1
USE IN 6 Caja ALIAS caja ORDER  ;
    Caja1
vmens01 = 'Registro de Pago de Donaciones'
vmens02 = 'Pago de Donaciones : REVISION '
vmens04 = 'Dicho Pago no fue encontrado'
vmens05 = 'No existe Pago anterior'
vmens06 = 'No existe Pago siguiente'
vmens07 = '¨ Desea Anular este Pago ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Pago ha sido anulado'
vmens10 = 'Edici¢n'
SELECT pagdon
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anula  Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 11, 79 TITLE vmens01  ;
       FOOTER  ;
       '[F9] Detalle : Item'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle: Pago Donaciones'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 00, 00  ;
       TO 23, 79 TITLE vmens02  ;
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
       ' A\<nula ' AT 24, 54
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
@ 0, 59 SAY 'Estado :'
@ 1, 2 SAY ' Pago Donaciones :'
@ 1, 50 SAY ' Fecha P/D :'
@ 2, 2 SAY '  Ent. q cancela :'
@ 3, 2 SAY '  Factura Girada :'
@ 4, 2 SAY '   Cheque Girado :'
@ 5, 2 SAY '     Monto Total :'
@ 7, 2 SAY '   Observaciones :'
RETURN
*
PROCEDURE vista
SELECT pagdon
ON KEY LABEL F9 DO VISTA_DET
ON KEY LABEL F4 DO Imprimir
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 0, 68 SAY m.estado
@ 1, 22 SAY m.codpd
@ 1, 65 SAY m.fecpd
@ 2, 22 SAY m.codent
@ 2, 22 SAY val_para(m.codent, ;
  'ENTDON',' ',22,30)
@ 3, 22 SAY m.codcp
@ 4, 22 SAY m.cheque
@ 5, 22 SAY m.mtotot
@ 7, 22 SAY LEFT(m.observ, 55)
DO vista_hijo
IF  .NOT. vflag $ 'J*'
     DO subopc
ENDIF
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT itepd
GOTO TOP
SEEK m.codpd
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            coddon :H = 'Donac',  ;
            x = retcli(codcli) :H =  ;
            'Cliente', mtodon :H =  ;
            'Monto' NOMENU  ;
            NOAPPEND NODELETE  ;
            NOCLEAR WINDOW wind_1  ;
            KEY m.codpd TIMEOUT  ;
            0.0001  NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 4, 25 SAY  ;
       'No hay Detalles de este Documento'
     ACTIVATE WINDOW wind_0
ENDIF
SELECT pagdon
RETURN
*
PROCEDURE vista_det
SELECT itepd
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
GOTO TOP
SEEK m.codpd
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            coddon :H = 'Donac',  ;
            x = retcli(codcli) :H =  ;
            'Cliente', mtodon :H =  ;
            'Monto' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_1 KEY  ;
            m.codpd TITLE vtempo  ;
            NOREFRESH
ELSE
     ACTIVATE WINDOW wind_1
     CLEAR
     @ 4, 25 SAY  ;
       'No hay Detalles de este Ingreso'
ENDIF
ON KEY LABEL F9 DO VISTA_DET
SHOW MENU mmenu
SELECT pagdon
DO vista
RETURN
*
PROCEDURE revis
SELECT pagdon
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SET RELATION TO codpd INTO itepd
SET SKIP TO itepd
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE FIELDS codpd :H = 'Pago',  ;
       fecpd :H = 'Fecha', codent  ;
       :H = 'Entidad',  ;
       itepd.coddon :H = 'Donac.',  ;
       itepd.codcli :H =  ;
       'Cliente', itepd.mtodon :H =  ;
       'Monto' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_2
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELECT pagdon
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
vnum_pd = SPACE(4)
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese Codigo Pago : ' GET  ;
  vnum_pd PICTURE '!!!!' VALID  ;
  vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_pd) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vnum_pd
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
vnum_pd = PADL(ALLTRIM(vnum_pd),  ;
          4, '0')
RETURN .T.
*
PROCEDURE anter
SELECT pagdon
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
SELECT pagdon
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
ON KEY LABEL F9
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SELECT pagdon
SCATTER MEMVAR
DO pantalla
@ 0, 68 GET m.estado DISABLE
@ 1, 22 GET m.codpd DISABLE
@ 1, 65 GET m.fecpd
@ 2, 22 GET m.codent VALID  ;
  val_para(m.codent,'ENTDON',' ', ;
  22,30)
@ 3, 22 GET m.codcp
@ 4, 22 GET m.cheque
@ 5, 22 GET m.mtotot WHEN .F.
@ 7, 22 GET m.observ FUNCTION  ;
  'S50'
READ VALID val_read()
IF LASTKEY() <> 27
     ok = trabaja_hi()
     IF ok .AND. LASTKEY() <> 27
          SELECT pagdon
          IF f_lock(1)
               m.tuser = 'C'
               m.usercr = vuser_id
               m.dusercr = DATE()
               m.husercr = TIME()
               GATHER MEMVAR
          ENDIF
     ELSE
          DO standby WITH  ;
             'El proceso ha sido cancelado'
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT pagdon
DO vista
RETURN
*
PROCEDURE ingre
ON KEY LABEL F9
SELECT pagdon
nreg = IIF( .NOT. EOF(), RECNO(), - ;
       1)
DO pantalla
SCATTER BLANK MEMVAR
m.estado = '00'
m.fecpd = m.fecsis
= corpd()
SELECT pagdon
@ 0, 68 GET m.estado DISABLE
@ 1, 22 GET m.codpd DISABLE
@ 1, 65 GET m.fecpd
@ 2, 22 GET m.codent VALID  ;
  val_para(m.codent,'ENTDON',' ', ;
  22,30)
@ 3, 22 GET m.codcp
@ 4, 22 GET m.cheque
@ 5, 22 GET m.mtotot WHEN .F.
@ 7, 22 GET m.observ FUNCTION  ;
  'S50'
READ VALID val_read()
IF LASTKEY() <> 27
     ok = trabaja_hi()
     IF ok .AND. LASTKEY() <> 27
          SELECT pagdon
          IF f_appd()
               m.tuser = 'I'
               m.user = vuser_id
               m.duser = DATE()
               m.huser = TIME()
               GATHER MEMVAR
          ENDIF
          SELECT parma
          cano = RIGHT(STR(YEAR(m.fecpd),  ;
                 4), 2)
          IF  .NOT. SEEK('CORPDN' +  ;
              cano, 'Parma')
               IF f_appd()
                    REPLACE tipo  ;
                            WITH  ;
                            'CORPDN',  ;
                            codigo  ;
                            WITH  ;
                            cano,  ;
                            descri  ;
                            WITH  ;
                            'CORRELATIVO DE Pago de Donaciones A¤o :' +  ;
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
     ELSE
          IF nreg <> -1
               GOTO nreg
          ENDIF
          SELECT itepd
          IF SEEK(m.codpd)
               DELETE WHILE codpd =  ;
                      m.codpd
          ENDIF
          DO standby WITH  ;
             'Proceso cancelado'
     ENDIF
ELSE
     IF nreg <> -1
          GOTO nreg
     ENDIF
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT pagdon
DO vista
RETURN
*
FUNCTION corpd
PRIVATE cano, calias, mret, nreg
calias = ALIAS()
SELE &cAlias
mret = .T.
cano = RIGHT(STR(YEAR(m.fecpd),  ;
       4), 2)
IF SEEK('CORPDN' + cano, 'Parma')
     m.codpd = cano +  ;
               PADL(ALLTRIM(STR(parma.nument +  ;
               1)), 2, '0')
     SHOW GET m.codpd
ELSE
     m.codpd = cano +  ;
               PADL(ALLTRIM(STR(parma.nument +  ;
               1)), 2, '0')
     SHOW GET m.codpd
ENDIF
SELECT (calias)
IF EMPTY(m.codpd)
     DO standby WITH  ;
        'El C¢digo esta vacio'
     mret = .F.
ELSE
     nreg = RECNO()
     IF SEEK(m.codpd)
          DO standby WITH  ;
             'Ya esta Registrado este Pago'
          mret = .F.
     ENDIF
ENDIF
RETURN mret
*
FUNCTION trabaja_hi
vsun = .T.
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°°°°F5->Agregar°°°°°°°°°F8->Eliminar°°°°°°°°F10->Terminar°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO agrega_Don
ON KEY LABEL F8 DO elimi_Don
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itepd
SET ORDER TO itePD1
SEEK m.codpd
IF  .NOT. FOUND()
     vsun = agrega_don()
ELSE
     vsun = .T.
ENDIF
IF vsun
     BROWSE NOOPTIMIZE FIELDS  ;
            coddon :H = 'Donac',  ;
            x = retcli(codcli) :H =  ;
            'Cliente' : 30,  ;
            mtodon :H = 'Monto'  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_1  ;
            KEY m.codpd WHEN .F.
     IF LASTKEY() <> 27
          m.mtotot = 0
          SEEK m.codpd
          IF FOUND()
               SCAN WHILE codpd =  ;
                    m.codpd
                    m.mtotot = m.mtotot +  ;
                               mtodon
               ENDSCAN
          ENDIF
          vsun = .T.
     ELSE
          vsun = .F.
     ENDIF
ENDIF
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SELECT pagdon
RETURN vsun
*
FUNCTION retcli
PARAMETER mcli
PRIVATE mret
IF SEEK(mcli, 'Clien')
     mret = clien.nomcli
ELSE
     mret = 'Sin Descripci¢n'
ENDIF
RETURN mret
*
FUNCTION agrega_don
PRIVATE calias, mret
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F11 DO Liquida
ON KEY LABEL F12 DO QuitLiq
calias = ALIAS()
SELECT caja
SET FILTER TO estado <> '99'
SELECT donac
SET FILTER TO estado = '20'
GOTO TOP
DEFINE WINDOW wind_3 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       '°°°°[F11] Liquida°°°°°°°°[F12] Quita Liquidacion°°°°'  ;
       DOUBLE COLOR SCHEME 10
IF  .NOT. EOF()
     BROWSE FIELDS pagado :H =  ;
            '*' :W = .F., coddon  ;
            :H = 'Donac', fecdon  ;
            :H = 'Fecha', codcli  ;
            :H = 'Cliente',  ;
            nomben :H = 'Benef.',  ;
            mtodon :H = 'Monto'  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_3  ;
            WHEN .F.
     IF LASTKEY() <> 27
          SET FILTER TO pagado = 'x';
.AND. estado = '20'
          GOTO TOP
          IF  .NOT. EOF()
               SCAN FOR pagado =  ;
                    'x' .AND.  ;
                    estado <>  ;
                    '10'
                    xcoddon = coddon
                    xfecdon = fecdon
                    xcodcli = codcli
                    xmtodon = mtodon
                    xestado = '00'
                    SELECT caja
                    IF  .NOT.  ;
                        SEEK('4' +  ;
                        xcoddon)
                         m.tipcaj =  ;
                          '4'
                         m.fecreg =  ;
                          m.fecpd
                         m.coring =  ;
                          donac.coddon
                         m.importe =  ;
                          xmtodon
                         IF f_appd()
                              GATHER  ;
                               MEMVAR
                              REPLACE  ;
                               estado  ;
                               WITH  ;
                               '00',  ;
                               codcli  ;
                               WITH  ;
                               xcodcli,  ;
                               tuser  ;
                               WITH  ;
                               'I',  ;
                               user  ;
                               WITH  ;
                               vuser_id,  ;
                               duser  ;
                               WITH  ;
                               DATE(),  ;
                               huser  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                         ENDIF
                         mret = .T.
                    ELSE
                         DO standby  ;
                            WITH  ;
                            'eSTE rEGISTRO YA ESTA EN vNTAS'
                         mret = .F.
                    ENDIF
                    IF mret
                         SELECT donac
                         REPLACE estado  ;
                                 WITH  ;
                                 '10'
                         SELECT itepd
                         IF f_appd()
                              REPLACE  ;
                               codpd  ;
                               WITH  ;
                               m.codpd,  ;
                               fecpd  ;
                               WITH  ;
                               m.fecpd,  ;
                               codent  ;
                               WITH  ;
                               m.codent,  ;
                               coddon  ;
                               WITH  ;
                               xcoddon,  ;
                               fecdon  ;
                               WITH  ;
                               xfecdon,  ;
                               codcli  ;
                               WITH  ;
                               xcodcli,  ;
                               mtodon  ;
                               WITH  ;
                               xmtodon,  ;
                               estado  ;
                               WITH  ;
                               xestado,  ;
                               tuser  ;
                               WITH  ;
                               'I',  ;
                               user  ;
                               WITH  ;
                               vuser_id,  ;
                               duser  ;
                               WITH  ;
                               DATE(),  ;
                               huser  ;
                               WITH  ;
                               TIME()
                         ENDIF
                    ENDIF
               ENDSCAN
               mret = .T.
          ELSE
               DO standby WITH  ;
                  'No ha Seleccionado Ninguna Donacion'
               mret = .F.
          ENDIF
     ELSE
          DO standby WITH  ;
             'Cancela Informacion'
          mret = .F.
     ENDIF
ELSE
     DO standby WITH  ;
        'No Existe Informacion para Procesar'
     mret = .F.
ENDIF
DEACTIVATE WINDOW wind_3
RELEASE WINDOW wind_3
SELECT donac
SET FILTER TO
SELECT caja
SET FILTER TO
SELE &cAlias
ON KEY LABEL F11
ON KEY LABEL F12
ON KEY LABEL F5 DO agrega_Don
ON KEY LABEL F8 DO elimi_Don
RETURN mret
*
FUNCTION liquida
REPLACE pagado WITH 'x', codpd  ;
        WITH m.codpd
RETURN .T.
*
FUNCTION quitliq
REPLACE pagado WITH ' ', codpd  ;
        WITH ''
RETURN .T.
*
PROCEDURE elimi_don
PRIVATE mret
mret = .F.
IF yesno( ;
   'Estas seguro de Eliminar este Item' ;
   )
     mret = .T.
ENDIF
IF mret
     SELECT caja
     SET FILTER TO estado <> '99'
     IF SEEK('3' + itepd.coddon)
          REPLACE estado WITH  ;
                  '99', tuser  ;
                  WITH 'A',  ;
                  usercr WITH  ;
                  vuser_id,  ;
                  husercr WITH  ;
                  TIME(), dusercr  ;
                  WITH DATE()
          mret = .T.
     ELSE
          IF yesno( ;
             'Donacion no Registrada en caja. Desea Continuar?' ;
             )
               mret = .T.
          ELSE
               mret = .F.
          ENDIF
     ENDIF
     SET FILTER TO
ENDIF
IF mret
     SELECT donac
     IF SEEK(itepd.coddon)
          REPLACE estado WITH  ;
                  '20', pagado  ;
                  WITH ' ', codpd  ;
                  WITH '', tuser  ;
                  WITH 'C',  ;
                  usercr WITH  ;
                  vuser_id,  ;
                  husercr WITH  ;
                  TIME(), dusercr  ;
                  WITH DATE()
          mret = .T.
     ELSE
          IF yesno( ;
             'Donacion no Registrada. Desea Continuar?' ;
             )
               mret = .T.
          ELSE
               mret = .F.
          ENDIF
     ENDIF
ENDIF
IF mret
     SELECT itepd
     DELETE NEXT 1
     DO standby WITH  ;
        'Donacion Eliminada del Pago'
ENDIF
RETURN
*
PROCEDURE lista
PRIVATE nreg
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
nreg = RECNO()
DEFINE WINDOW wlista FROM 3, 15  ;
       TO 20, 70 FLOAT TITLE  ;
       'Listado de Pedidos'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
vtopa = 1
mfecha1 = CTOD('01/01/' +  ;
          STR(YEAR(m.fecsis),  ;
          4))
mfecha2 = DATE()
@ 01, 01 SAY  ;
  '    Todos los Pagos : ' GET  ;
  vtopa SIZE 1, 10, 2 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 03, 01 SAY  ;
  '         Donacion : ' GET  ;
  m.codpd WHEN vtopa = 2
@ 12, 01 SAY  ;
  '     Rango de Fechas: ' GET  ;
  mfecha1 WHEN vtopa = 1
@ 12, 36 GET mfecha2 WHEN vtopa =  ;
  1
@ 15, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
lfil = .T.
IF LASTKEY() <> 27 .AND. okcancel <>  ;
   2
     IF vtopa = 2
          lfil = 'CodPD = m.CodPD'
     ENDIF
     SET FILTER TO &lFil AND BETW(FecPD,mFecha1,mFecha2)
     SET RELATION TO codpd INTO itepd;
ADDITIVE
     SET SKIP TO itepd
     DO reporte WITH 2, 'PagDon',  ;
        'Pago de Donaciones', 2,  ;
        .F., .T.
     SET RELATION TO
     SET FILTER TO
ENDIF
SELECT pagdon
GOTO nreg
DO vista
RETURN
*
PROCEDURE imprimir
PRIVATE vcon
SELECT pagdon
vcon = RECNO()
SCATTER MEMVAR
m.fecrec = m.fecpd
m.impcan = letras(m.mtotot, ;
           'SOLES')
SELECT parma
IF SEEK('ENTDON' + m.codent)
     m.nomcli = parma.descri
     m.dircli = ''
ENDIF
vdbf = SYS(3) + '.Dbf'
CREATE CURSOR Recibo (codvta C  ;
       (7), tipvta C (3), convta  ;
       C (1), fecrec D, compag C  ;
       (3), codcp C (20), codcli  ;
       C (6), nomcli C (50),  ;
       dircli C (50), ruccli C  ;
       (11), codavl C (6), nomavl  ;
       C (50), diravl C (50),  ;
       nomben C (50), cement C  ;
       (30), cuartel C (30),  ;
       nicho C (5), subvenc C (1),  ;
       donacion C (1), codsub C  ;
       (7), coddon C (7), codcre  ;
       C (7), estado C (2),  ;
       mtovta N (10, 2), mtosub N  ;
       (10, 2), mtodon N (10, 2),  ;
       mtocan N (10, 2), mtocre N  ;
       (10, 2), impcan C (200),  ;
       observ C (100))
SELECT recibo
APPEND BLANK
GATHER MEMVAR
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO reporte WITH 2, 'RecDon',  ;
        ' Recibo por Donaciones '
ENDIF
SET FILTER TO
SELECT pagdon
GOTO vcon
DO vista
RETURN
*
PROCEDURE anula
DO standby WITH  ;
   'Opci¢n todavia en proceso. Crear reporte si es necesario'
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
ON KEY LABEL F7
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
PROCEDURE subopc
PRIVATE calias
calias = ALIAS()
USE IN 0 IteUsuOp ORDER IteUsuOp2
SET SKIP OF PAD revis OF mmenu;
 .NOT. SEEK(vusucla + '0020', 'IteUsuOp')
SET SKIP OF PAD busca OF mmenu;
 .NOT. SEEK(vusucla + '0021', 'IteUsuOp')
SET SKIP OF PAD anter OF mmenu;
 .NOT. SEEK(vusucla + '0022', 'IteUsuOp')
SET SKIP OF PAD proxi OF mmenu;
 .NOT. SEEK(vusucla + '0023', 'IteUsuOp')
SET SKIP OF PAD corri OF mmenu;
 .NOT. SEEK(vusucla + '0024', 'IteUsuOp')
SET SKIP OF PAD ingre OF mmenu;
 .NOT. SEEK(vusucla + '0025', 'IteUsuOp')
SET SKIP OF PAD anula OF mmenu;
 .NOT. SEEK(vusucla + '0026', 'IteUsuOp')
SET SKIP OF PAD lista OF mmenu;
 .NOT. SEEK(vusucla + '0027', 'IteUsuOp')
USE IN iteusuop
SELECT (calias)
RETURN
*

PARAMETER key_docfte
IF PARAMETERS() = 0
     key_docfte = ''
ENDIF
CLOSE DATABASES
USE IN 1 ParMae ALIAS parma ORDER  ;
    ParMae1
USE IN 3 ComPag ALIAS compag  ;
    ORDER ComPag1
USE IN 4 IteCp ALIAS itecp ORDER  ;
    IteCp1
USE IN 6 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 7 IteHc ALIAS itehc ORDER  ;
    IteHc1
USE IN 10 Cajas ALIAS caja ORDER  ;
    Cajas2
USE IN 11 Cheque ALIAS cheque  ;
    ORDER Cheque1
USE IN 15 Auxil ALIAS auxil ORDER  ;
    Auxil1
USE IN 16 MaePre ALIAS maepre  ;
    ORDER MaePre1
USE IN 25 ItePar ALIAS itepar  ;
    ORDER ItePar1
SELECT compag
GOTO BOTTOM
vmens01 = ' Comprobantes de Pago : REVISION '
vmens02 = '°   ®F3¯ Visualiza Retenciones    ®F4¯Imprime C/P °'
vmens04 = 'Dicho C/P no fue encontrado'
vmens05 = 'No existe C/P anterior'
vmens06 = 'No existe C/P siguiente'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este C/P ha sido anulado'
vmens13 = ' °  Retenciones  ° '
SCATTER BLANK MEMVAR
PUBLIC vfecha, vkey, mmonto, vpar,  ;
       vcompag, zz, yy, lseek,  ;
       x_partret, vrec, wmtoret,  ;
       wchq, ztotal, vref, vref1,  ;
       wuser_id, vglosa2,  ;
       vsector
PUBLIC vnummes, vcodctc, vcta,  ;
       vdes, w_tipctc, vret,  ;
       w_ctad, w_part, w_ctah,  ;
       vtitulo, vncheque, vimport,  ;
       vvpartret, vcadena,  ;
       docbusc
wuser_id = CHRTRAN(vuser_id, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
STORE SPACE(14) TO vcodctc,  ;
      vncheque
STORE SPACE(18) TO wchq
STORE SPACE(5) TO w_part, yy
STORE SPACE(11) TO w_ctad, w_ctah
STORE 0 TO mmonto, vmondeb, vret
STORE SPACE(20) TO zz, vref,  ;
      vref1
STORE .T. TO vflag
STORE SPACE(1) TO m.prestamo
STORE DATE() TO vfecha
STORE '  ' TO vnummes, vkey
STORE SPACE(180) TO vglosa2
docbusc = ''
IF  .NOT. EMPTY(key_docfte)
     docbusc = key_docfte
ENDIF
IF  .NOT. EMPTY(docbusc)
     SEEK docbusc
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'El C/P no fue Encontrado!'
          GOTO BOTTOM
     ENDIF
ENDIF
DO inicia
HIDE POPUP ALL
DO pantalla
DO vista
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     ACTIVATE MENU mmenu1
ENDDO
DO fin_opcion
RETURN
*
PROCEDURE inicia
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente                             Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 13, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 14, 00  ;
       TO 23, 79 TITLE  ;
       '®F9¯: Corrige Asientos    Comprobante de Pago : Detalle   '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 20, 64  ;
       TO 22, 78 TITLE ' TOTAL '  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_10 FROM 20, 64  ;
       TO 22, 78 TITLE ' TOTAL '  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_4 FROM 08, 01  ;
       TO 15, 78 TITLE  ;
       '** GIRO DE Cheque->  ®F5¯ Agregar   ®F8¯ Eliminar   ®F10¯ Terminar **'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_5 FROM 08, 02  ;
       TO 15, 77 TITLE  ;
       ' Retenciones    ®F10¯ Continua '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_6 FROM 13, 10  ;
       TO 17, 70 TITLE  ;
       ' COMPROMISO PRESUPUESTAL '  ;
       COLOR SCHEME 15
DEFINE WINDOW wind_7 FROM 13, 10  ;
       TO 17, 70 TITLE  ;
       ' CENTRALIZACION DE Caja '  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_8 FROM 14, 01  ;
       TO 16, 79 TITLE  ;
       ' Destino '
DEFINE WINDOW wind_9 FROM 05, 00  ;
       TO 23, 79 TITLE vmens13  ;
       DOUBLE COLOR SCHEME 2
DEFINE WINDOW wind_11 FROM 16, 01  ;
       TO 18, 79 TITLE  ;
       ' Pr‚stamos ' COLOR SCHEME  ;
       5
DEFINE WINDOW wind_12 FROM 14, 11  ;
       TO 16, 69 TITLE  ;
       ' Retenci¢n '
DEFINE WINDOW wind_13 FROM 13, 10  ;
       TO 17, 70 TITLE 'Sectores'  ;
       COLOR SCHEME 10
DEFINE MENU mmenu1 COLOR SCHEME 3
DEFINE PAD revis OF mmenu1 PROMPT  ;
       '\<Revisa' AT 24, 00
DEFINE PAD busca OF mmenu1 PROMPT  ;
       '\<Busca' AT 24, 08
DEFINE PAD anter OF mmenu1 PROMPT  ;
       '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu1 PROMPT  ;
       '\<Siguiente' AT 24, 25
DEFINE PAD lista OF mmenu1 PROMPT  ;
       '\<Listar ' AT 24, 63
DEFINE PAD termi OF mmenu1 PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu1 DO revis
ON SELECTION PAD busca OF mmenu1 DO busca
ON SELECTION PAD anter OF mmenu1 DO anter
ON SELECTION PAD proxi OF mmenu1 DO proxi
ON SELECTION PAD lista OF mmenu1 DO lista
ON SELECTION PAD termi OF mmenu1 DO termi
ON KEY LABEL F2 DO gircheq
ON KEY LABEL F3 DO regret
ON KEY LABEL F4 DO imp_cp
ON KEY LABEL F9 DO cor_as
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_1
CLEAR
@ 00, 02 SAY  ;
  '         Pr‚stamo :'
@ 00, 40 SAY  ;
  'Incidencia Presup.:'
@ 01, 02 SAY  ;
  '           N§ C/P :'
@ 01, 40 SAY  ;
  '       Fecha  C/P :'
@ 02, 02 SAY  ;
  ' Cuenta Corriente :'
@ 03, 02 SAY  ;
  '        Proveedor :'
@ 04, 02 SAY  ;
  '  Doc. Referencia :'
@ 04, 48 SAY  ;
  ' Fecha Digitaci¢n :'
@ 05, 02 SAY  ;
  '         Fte.Fto. :'
@ 06, 02 SAY  ;
  ' Cadena Funcional :'
@ 06, 27 SAY  ;
  ' UG  UE  FN PRG SBPRG ACTPRY'
@ 08, 02 SAY  ;
  '            Glosa :'
@ 09, 02 SAY  ;
  '    Observaciones :'
@ 11, 00 SAY  ;
  PADC(ALLTRIM(vmens02), 77, ' ')  ;
  COLOR W+/B 
RETURN
*
PROCEDURE vista
SELECT compag
SET RELATION TO nummeshc + numhc INTO;
hoja
SET RELATION TO nummes + numcp + codctc;
INTO cheque ADDITIVE
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
= val_codc1(ALLTRIM(m.codcad), ;
  m.periodo + '01001','C')
ON KEY LABEL F9 DO cor_as
@ 0, 0 SAY SPACE(80)
@ 0, 45 SAY 'Estado     :'
@ 0, 60 SAY IIF(m.pres = '*',  ;
  '  Con H/A  ', IIF(m.tipdoc =  ;
  'IN', 'Inutilizado', IIF(estado =  ;
  '99', '  Anulado  ',  ;
  '     OK û   '))) COLOR SCHEME  ;
  2
@ 1, 22 SAY m.numcp
@ 1, 26 SAY '.'
@ 1, 27 SAY m.nummes
@ 1, 60 SAY m.feccp
@ 2, 22 SAY m.codctc
@ 2, 60 SAY SPACE(18)
@ 2, 60 SAY  ;
  verestch(cheque.estado) COLOR  ;
  SCHEME 2
SET RELATION TO
DO CASE
     CASE m.tipdoc $ 'RESRRURS'
          @ 0, 01 SAY  ;
            IIF(m.tipdoc = 'RE',  ;
            'Retenci¢n',  ;
            IIF(m.tipdoc = 'SR',  ;
            'Ret.Sector',  ;
            IIF(m.tipdoc = 'RU',  ;
            'Ret.Subsid',  ;
            'Ret.Sec.Sub')))  ;
            COLOR SCHEME 05
          @ 3, 22 SAY  ;
            val_para(m.codret, ;
            'CODRET','V',22,40)
     CASE m.tipdoc $ 'MESUHCSS'
          @ 0, 01 SAY  ;
            IIF(m.tipdoc = 'ME',  ;
            'Memorandum',  ;
            IIF(m.tipdoc = 'HC',  ;
            'Hoja Control',  ;
            IIF(m.tipdoc = 'SU',  ;
            'Subsidios',  ;
            'Sect.Sub'))) COLOR  ;
            SCHEME 05
          @ 3, 22 SAY SPACE(60)
          = pres_pro()
     CASE m.tipdoc $ 'SESH'
          @ 0, 01 SAY  ;
            'Sector.H/C' COLOR  ;
            SCHEME 05
          @ 3, 22 SAY SPACE(60)
          = pres_pro()
     CASE m.tipdoc $ 'HMRGIN'
          @ 0, 01 SAY  ;
            IIF(m.tipdoc = 'HM',  ;
            'Hoja Mod.',  ;
            IIF(m.tipdoc = 'RG',  ;
            'Regularizac. H/C',  ;
            'Inutilizado')) COLOR  ;
            SCHEME 05
          @ 3, 22 SAY SPACE(60)
          @ 3, 22 SAY m.nompre
ENDCASE
DO CASE
     CASE m.tipdoc $ 'HCRG'
          @ 4, 22 SAY m.numhc +  ;
            '.' + m.nummeshc +  ;
            ' H/C'
     CASE m.tipdoc = 'HM'
          @ 4, 22 SAY m.numhm +  ;
            '.' + m.nummeshm +  ;
            ' H/M'
     OTHERWISE
          @ 4, 22 SAY m.docref
          @ 4, 26 SAY m.numref
          @ 4, 34 SAY m.refer
ENDCASE
IF m.tipdoc <> 'IN'
     @ 4, 68 SAY m.fecref
     @ 5, 22 SAY  ;
       val_para(m.codfte,'CODFTE', ;
       'V',26,50)
     @ 6, 22 SAY m.codcad
     DO vis_calen
     @ 08, 22 SAY m.glosa PICTURE  ;
       '@S56'
     @ 09, 22 SAY SUBSTR(m.observ,  ;
       1, 56)
     IF m.reten > 0
          @ 10, 02 SAY  ;
            '        Retenci¢n=>'  ;
            COLOR SCHEME 05
          @ 10, 22 SAY m.reten
     ELSE
          @ 10, 00 SAY SPACE(79)
     ENDIF
     DO vista_hijo
     DO total
ENDIF
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT itecp
SEEK ALLTRIM(m.nummes) + m.numcp +  ;
     ALLTRIM(m.codctc)
IF FOUND()
     IF m.codcad <> '0000'
          BROWSE NOOPTIMIZE  ;
                 FIELDS codcom :H =  ;
                 'Componente',  ;
                 codmet :H =  ;
                 'Meta', codpart  ;
                 :H = 'Partida',  ;
                 aa =  ;
                 IIF(EMPTY(codpart),  ;
                 ' ',  ;
                 val_para(RIGHT(codpart,  ;
                 2),'ESPGAS','D', ;
                 28,30)) :H =  ;
                 'Descripci¢n' :  ;
                 38, impparc :H =  ;
                 'Monto' :F :P =  ;
                 '9,999,999.99'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 NOCLEAR WINDOW  ;
                 wind_2 KEY  ;
                 ALLTRIM(m.nummes) +  ;
                 m.numcp +  ;
                 ALLTRIM(m.codctc)  ;
                 TIMEOUT 0.001   ;
                 NOREFRESH
     ELSE
          BROWSE NOOPTIMIZE  ;
                 FIELDS codpart  ;
                 :H = 'Partida',  ;
                 aa =  ;
                 IIF(EMPTY(codpart),  ;
                 ' ',  ;
                 val_para(RIGHT(codpart,  ;
                 2),'ESPGAS','D', ;
                 28,30)) :H =  ;
                 'Descripci¢n' :  ;
                 38, impparc :H =  ;
                 'Monto' :F :P =  ;
                 '9,999,999.99'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 NOCLEAR WINDOW  ;
                 wind_2 KEY  ;
                 ALLTRIM(m.nummes) +  ;
                 m.numcp +  ;
                 ALLTRIM(m.codctc)  ;
                 TIMEOUT 0.001   ;
                 NOREFRESH
     ENDIF
ELSE
     DO standby WITH  ;
        'No tiene detalle'
ENDIF
SELECT compag
SET RELATION OFF INTO hoja
RETURN
*
PROCEDURE total
ACTIVATE WINDOW wind_3
@ 0, 1 SAY m.import PICTURE  ;
  '9,999,999.99'
RETURN
*
PROCEDURE revis
SELECT compag
SET RELATION TO nummes + numcp + codctc;
INTO cheque ADDITIVE
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu1
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE FIELDS x1 = numcp + '.' +  ;
       nummes :H = ' N§ CP',  ;
       feccp, codctc :H =  ;
       'CtaCte', tipprv, x4 =  ;
       muespro(compag.tipprv, ;
       compag.codotr, ;
       compag.tipdoc) :H =  ;
       'Proveedor', codfte :H =  ;
       'Fte.Fin.', import :H =  ;
       'Total CP', x3 = numhc +  ;
       '.' + nummeshc :H =  ;
       ' N§ H/C', x2 = numhm +  ;
       '.' + nummeshm :H =  ;
       ' N§ H/M', codcad :H =  ;
       'Cad. Func.', estado :H =  ;
       'Estado' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu1
ON KEY LABEL f10
SELECT compag
SET RELATION OFF INTO cheque
DO vista
RETURN
*
PROCEDURE busca
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
STORE SPACE(4) TO vcompag
ACTIVATE WINDOW standby
@ 0, 01 SAY  ;
  '          Ingrese Mes: ' GET  ;
  vnummes DEFAULT SPACE(2)  ;
  PICTURE '!!'
@ 1, 01 SAY  ;
  'Ingrese N£mero ComPag: ' GET  ;
  vcompag DEFAULT SPACE(4)  ;
  PICTURE '@!'
@ 2, 01 SAY  ;
  '       Ingrese CtaCte: ' GET  ;
  vcodctc FUNCTION '!' VALID  ;
  val_fun('Caja','Codctc', ;
  "CodCtc+' '+Descri",vcodctc,1,2, ;
  24)
READ
DEACTIVATE WINDOW standby
IF EMPTY(vcompag) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     vnummes = PADL(ALLTRIM(vnummes),  ;
               2, '0')
     vcompag = PADL(ALLTRIM(vcompag),  ;
               4, '0')
     SEEK vnummes + vcompag +  ;
          ALLTRIM(vcodctc)
     IF  .NOT. FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
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
FUNCTION carhm
vfun = .T.
ok = FOUND()
SELECT hojm
SET FILTER TO operac $ 'TC';
.AND. estado = '20'
GOTO TOP
IF EOF()
     DO standby WITH  ;
        'No existe documentos H/M '
     SET FILTER TO
     RETURN .F.
ENDIF
vmes = '00'
vhm = '    '
ACTIVATE WINDOW standby
@ 1, 4 SAY ' N§ H/M: ' GET vmes  ;
  DEFAULT PADL(MONTH(DATE()), 2,  ;
  '0')
@ 1, 16 SAY '.'
@ 1, 17 GET vhm DEFAULT SPACE(4)
READ
DEACTIVATE WINDOW standby
IF LASTKEY() = 27
     SET FILTER TO
     RETURN .F.
ENDIF
vmes = PADL(ALLTRIM(vmes), 2,  ;
       '0')
vhm = PADL(ALLTRIM(vhm), 4, '0')
IF  .NOT. SEEK(ALLTRIM(vmes) +  ;
    vhm)
     GOTO BOTTOM
     ON KEY LABEL F10 KEYBOARD CHR(23)
     DEFINE WINDOW elihc FROM 1,  ;
            1 TO 18, 79 TITLE  ;
            ' Elija la HM con F10 '
     BROWSE FIELDS numhm :H =  ;
            'HM', nummes :H =  ;
            'Mes', tipprv, codprv  ;
            :H = 'CodPro', codemp  ;
            :H = 'CodEmp', x1 =  ;
            IIF(tipprv = 'O'  ;
            .AND. EMPTY(codotr),  ;
            nombre, IIF(tipprv =  ;
            'P',  ;
            LEFT(val_fun('Auxil', ;
            'descri','descri', ;
            codprv), 20),  ;
            IIF(tipprv = 'E',  ;
            LEFT(val_fun('Auxil', ;
            'descri','descri', ;
            codemp), 20),  ;
            LEFT(val_fun('Auxil', ;
            'descri','descri', ;
            codotr), 20)))) :H =  ;
            'Nombre', x2 =  ;
            TRANSFORM(imptot,  ;
            '99,999,999.99') :H =  ;
            'Monto', fechm :H =  ;
            'fecha', x3 = codcal +  ;
            ' ' + codctc :H =  ;
            'Cal.ctc' NOEDIT  ;
            WINDOW elihc COLOR  ;
            SCHEME 10
ENDIF
IF LASTKEY() <> 27
     SELECT hojm
     m.tipfun = tipfun
     m.nummeshm = nummes
     m.numhm = numhm
     m.nummes = nummes
     m.nummeshc = nummeshc
     m.numhc = numhc
     m.import = imptot
     m.codprv = codprv
     m.codemp = codemp
     m.codotr = codotr
     m.codctc = codctc
     m.codpart = codpart
     m.tipdoc = tipdoc
     m.tipprv = tipprv
     m.periodo = periodo
     m.nompre = nombre
     m.destino = destino
     m.codfte = IIF(codfte =  ;
                'TRN', 'PRP',  ;
                'TRN')
     m.codcal = STUFF(codcal, 5,  ;
                3, m.codfte)
     m.numref = numhm + '.' +  ;
                nummes
     SELECT hojm
     SHOW GETS
     SELECT caja
     SEEK ALLTRIM(m.codctc)
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'No se tiene inscrito la cuenta '+ ;
             m.codctc
     ELSE
          m.numcp = PADL(corcp +  ;
                    1, 4, '0')
          w_ctah = caja.cuentah
     ENDIF
     SELECT hojm
     SET FILTER TO
     SELECT compag
ENDIF
RETURN .T.
*
FUNCTION carshc
PARAMETER vsec
vfun = .T.
ok = FOUND()
SELECT hoja
vvord = ORDER()
DO CASE
     CASE vsec = 'A'
          SET ORDER TO Hojcon6
     CASE vsec = 'E'
          SET ORDER TO Hojcon7
     CASE vsec = 'I'
          SET ORDER TO Hojcon8
     CASE vsec = 'T'
          SET ORDER TO Hojcon9
     CASE vsec = 'P'
          SET ORDER TO Hojcon10
ENDCASE
GOTO TOP
IF EOF()
     DO standby WITH  ;
        'No existe documentos H/C para sectores'
     SET ORDER TO (vvord)
     SELECT compag
     RETURN .F.
ENDIF
vmes = '00'
vhc = '    '
ACTIVATE WINDOW standby
@ 1, 4 SAY ' N§ H/C: ' GET vmes  ;
  DEFAULT PADL(MONTH(DATE()), 2,  ;
  '0')
@ 1, 16 SAY '.'
@ 1, 17 GET vhc DEFAULT SPACE(4)
READ
DEACTIVATE WINDOW standby
IF LASTKEY() = 27
     SET ORDER TO (vvord)
     SELECT compag
     RETURN .F.
ENDIF
vmes = PADL(ALLTRIM(vmes), 2,  ;
       '0')
vhc = PADL(ALLTRIM(vhc), 4, '0')
IF  .NOT. SEEK(ALLTRIM(vmes) +  ;
    vhc)
     GOTO BOTTOM
     ON KEY LABEL F10 KEYBOARD CHR(23)
     DEFINE WINDOW elihc FROM 1,  ;
            1 TO 18, 79 TITLE  ;
            ' Elija la HC con F10 '
     BROWSE FIELDS numhc :H =  ;
            'HC', nummes :H =  ;
            'Mes', tipprv, codprv  ;
            :H = 'CodPro', codemp  ;
            :H = 'CodEmp', x1 =  ;
            IIF(tipprv = 'O'  ;
            .AND. EMPTY(codotr),  ;
            nombre, IIF(tipprv =  ;
            'P',  ;
            LEFT(val_fun('Auxil', ;
            'descri','descri', ;
            codprv), 20),  ;
            IIF(tipprv = 'E',  ;
            LEFT(val_fun('Auxil', ;
            'descri','descri', ;
            codemp), 20),  ;
            LEFT(val_fun('Auxil', ;
            'descri','descri', ;
            codotr), 20)))) :H =  ;
            'Nombre', x2 =  ;
            TRANSFORM(imptot,  ;
            '99,999,999.99') :H =  ;
            'Monto', fechc :H =  ;
            'Fecha', x3 = codcad +  ;
            ' ' + codctc :H =  ;
            'Cad.ctc' NOEDIT  ;
            WINDOW elihc COLOR  ;
            SCHEME 10
ENDIF
IF LASTKEY() <> 27
     SELECT hoja
     m.nummes = nummes
     m.nummeshc = nummes
     m.numhc = numhc
     m.import = imptot
     m.codprv = codprv
     m.codemp = codemp
     m.codotr = codotr
     m.codcad = codcad
     m.codpart = codpart
     m.tipdoc = tipdoc
     m.nombre = nompre
     m.tipprv = tipprv
     m.periodo = periodo
     m.nompre = nombre
     m.destino = destino
     m.codobra = codobra
     m.codfte = codfte
     m.numref = numhc + '.' +  ;
                nummes
     m.partret = partret
     SHOW GETS
     SET ORDER TO (vvord)
     SELECT compag
ENDIF
RETURN .T.
*
FUNCTION ingfec
m.periodo = RIGHT(DTOC(m.feccp),  ;
            2)
RETURN .T.
*
PROCEDURE buscaja
SELECT caja
SEEK m.codctc
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No se tiene inscrita la cuenta '+ ;
        m.codctc
ELSE
     m.numcp = PADL(corcp + 1, 4,  ;
               '0')
     w_ctah = caja.cuentah
ENDIF
RETURN
*
PROCEDURE buscaja1
SELECT caja
SEEK m.codctc
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No se tiene inscrita la cuenta '+ ;
        m.codctc
ELSE
     w_ctah = caja.cuentah
ENDIF
RETURN
*
FUNCTION valfecha
m.feccp = DATE()
RETURN .T.
*
PROCEDURE vis_cen
ACTIVATE WINDOW wind_7
@ 00, 08 SAY 'Cuentas '
@ 00, 18 SAY 'Debe '
@ 00, 34 SAY 'Haber '
@ 01, 04 SAY '10101010100'
@ 01, 18 SAY mmonto PICTURE  ;
  '999,999,999.99'
@ 02, 12 SAY '10101010100'
@ 02, 34 SAY mmonto PICTURE  ;
  '999,999,999.99'
DO standby WITH  ;
   'Visualizando los Movimientos'
DEACTIVATE WINDOW wind_7
RETURN
*
PROCEDURE asig
vmondeb = mtodeb
RETURN
*
PROCEDURE verap
USE IN 9 astpat ALIAS astpat  ;
    ORDER astpat3
SELECT astpat
SEEK ALLTRIM(m.nummes) + m.numcp +  ;
     m.codctc
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No tiene detalle'
ELSE
     BROWSE NOOPTIMIZE FIELDS  ;
            codcta :H = 'CtaCte',  ;
            tipcta :H = 'Tp',  ;
            mtodeb :H =  ;
            'Monto Debe' :P =  ;
            '999,999,999.99',  ;
            mtohab :H =  ;
            'Monto Haber' :P =  ;
            '999,999,999.99', ret  ;
            :H = 'Ret?' :P = '!'  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY ALLTRIM(m.nummes) +  ;
            m.numcp TIMEOUT 0.001   ;
            NOREFRESH
ENDIF
USE IN 9
SELECT compag
RETURN
*
PROCEDURE lista
USE IN 6
USE IN 7
USE IN 8
USE IN 9 astpat ALIAS astpat  ;
    ORDER astpat3
USE IN 14 cuentas ALIAS cuenta  ;
    ORDER cuentas1
SELECT compag
vrecno = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
DEFINE WINDOW lis FROM 3, 10 TO  ;
       23, 70 FLOAT DOUBLE COLOR  ;
       SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocta, vlista, vesta,  ;
      vsino
STORE SPACE(14) TO vcta, vcuenta
STORE SPACE(2) TO vano, vmes
STORE SPACE(4) TO vcli, vinicp,  ;
      vfincp
STORE SPACE(212) TO vobs
vcta = m.codctc
vano = m.nummes
vcli = m.numcp
STORE DATE() TO vfecini, vfecfin
@ 01, 01 SAY  ;
  '     Tipo Listado : ' GET  ;
  vlista FUNCTION  ;
  '^ Documento;Resumen x Cta.Cte'
@ 05, 01 SAY  ;
  '     N§ Documento : '
@ 05, 22 GET vano PICTURE '!!'  ;
  WHEN vlista = 1
@ 05, 25 GET vcli PICTURE '!!!!'  ;
  VALID val_cp() WHEN vlista = 1
@ 07, 01 SAY  ;
  '   Cta. Corriente : '
@ 07, 22 GET vcta VALID  ;
  val_fun('Caja','CodCtc', ;
  "CodCtC+' '+Descri",vcta,1,08, ;
  07)
@ 09, 01 SAY  ;
  '           Estado : ' GET  ;
  vesta FUNCTION  ;
  '*RNH Pendientes;General' WHEN  ;
  vlista = 3
@ 11, 01 SAY  ;
  '          Destino :  ' GET  ;
  vsino FUNCTION  ;
  '*RNH Presidencia;Cajero/Pagador'  ;
  WHEN vlista = 2
@ 13, 01 SAY  ;
  ' Fecha / Correlat.: '
@ 13, 22 GET vinicp PICTURE  ;
  '!!!!' WHEN vsino = 2 COLOR  ;
  SCHEME 10
@ 13, 32 GET vfincp PICTURE  ;
  '!!!!' WHEN vsino = 2 COLOR  ;
  SCHEME 10
@ 13, 22 GET vfecini PICTURE '@D'  ;
  WHEN (vlista = 2 .AND. vsino =  ;
  1) .OR. vlista = 3 .OR. vlista =  ;
  4 COLOR SCHEME 7
@ 13, 32 GET vfecfin PICTURE '@D'  ;
  VALID (vfecfin >= vfecini) WHEN  ;
  (vlista = 2 .AND. vsino = 1)  ;
  .OR. vlista = 3 .OR. vlista = 4  ;
  COLOR SCHEME 7
@ 15, 01 SAY  ;
  '    Observaciones : ' GET vobs  ;
  FUNCTION '!' PICTURE '@S28'  ;
  WHEN vlista = 2
@ 17, 15 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     xind = SYS(3) + '.IDX'
     DEFINE WINDOW xwait FROM 21,  ;
            35 TO 23, 75 COLOR  ;
            SCHEME 5
     DO CASE
          CASE vlista = 1
               vkey = ALLTRIM(vano) +  ;
                      vcli +  ;
                      ALLTRIM(vcta)
               SELECT compag
               SEEK vkey
               DO repprg WITH  ;
                  'LisCP',  ;
                  ' Listado Comprobante de Pago ',  ;
                  2
          CASE vlista = 2
               ACTIVATE WINDOW  ;
                        xwait
               @ 0, 0 SAY  ;
                 ' Procesando reporte....'  ;
                 COLOR W+/RB* 
               SELECT compag
               SET RELATION TO nummes;
+ numcp + codctc INTO cheque ADDITIVE
               IF vsino = 1
                    INDEX ON  ;
                          numcp  ;
                          TO  ;
                          (xind)  ;
                          FOR  ;
                          codctc =  ;
                          vcta  ;
                          .AND.  ;
                          BETWEEN(compag.fecref,  ;
                          vfecini,  ;
                          vfecfin)
               ELSE
                    INDEX ON  ;
                          numcp  ;
                          TO  ;
                          (xind)  ;
                          FOR  ;
                          codctc =  ;
                          vcta  ;
                          .AND.  ;
                          BETWEEN(compag.numcp,  ;
                          vinicp,  ;
                          vfincp)
               ENDIF
               DEACTIVATE WINDOW  ;
                          xwait
               DO reporte WITH 2,  ;
                  'LisCpCta',  ;
                  ' Resumen de Comprobantes de Pago Registradas por Cta.Cte. ',  ;
                  2, .F., .T.
               SELECT compag
               SET RELATION OFF INTO cheque
               SET INDEX TO
               SELECT compag
               SET ORDER TO ComPag1
          CASE vlista = 3
               ACTIVATE WINDOW  ;
                        xwait
               @ 0, 0 SAY  ;
                 ' Procesando reporte....'  ;
                 COLOR W+/RB* 
               USE IN 9 astpat  ;
                   ALIAS astpat  ;
                   ORDER astpat3
               SELECT astpat
               SELECT compag
               vrec1 = RECNO()
               vord1 = ORDER()
               IF vesta = 1
                    INDEX ON  ;
                          nummes +  ;
                          numcp +  ;
                          codctc  ;
                          TO  ;
                          (xind)  ;
                          FOR  ;
                          codctc =  ;
                          vcta  ;
                          .AND.  ;
                          BETWEEN(feccp,  ;
                          vfecini,  ;
                          vfecfin)  ;
                          .AND.  ;
                          prestamo =  ;
                          'S'  ;
                          .AND.  ;
                          EMPTY(conaaff)
               ELSE
                    INDEX ON  ;
                          nummes +  ;
                          numcp +  ;
                          codctc  ;
                          TO  ;
                          (xind)  ;
                          FOR  ;
                          codctc =  ;
                          vcta  ;
                          .AND.  ;
                          BETWEEN(feccp,  ;
                          vfecini,  ;
                          vfecfin)  ;
                          .AND.  ;
                          prestamo =  ;
                          'S'
               ENDIF
               SET RELATION TO nummes;
+ numcp + codctc INTO astpat ADDITIVE
               SET RELATION TO nummes;
+ numcp + codctc INTO cheque ADDITIVE
               DEACTIVATE WINDOW  ;
                          xwait
               IF EOF()
                    DO standby  ;
                       WITH  ;
                       'Registros no existen'
                    SET INDEX TO
                    USE IN 9
                    SELECT compag
                    SET ORDER TO (vord1)
                    GOTO vrec1
                    RETURN
               ENDIF
               vtitulo = 'PRESTAMOS'
               DO reporte WITH 2,  ;
                  'LisCpCon',  ;
                  ' Resumen de Comprobantes de Pago de Prestamos por Cta.Cte. '
               USE IN 9
               SET INDEX TO
               SELECT compag
               SET ORDER TO (vord1)
               GOTO vrec1
          CASE vlista = 4
               ACTIVATE WINDOW  ;
                        xwait
               @ 0, 0 SAY  ;
                 ' Procesando reporte....'  ;
                 COLOR W+/RB* 
               USE IN 9 astpat  ;
                   ALIAS astpat  ;
                   ORDER astpat3
               SELECT astpat
               INDEX ON nummes +  ;
                     numref +  ;
                     codctc TO  ;
                     (xind) FOR  ;
                     codctc =  ;
                     vcta .AND.  ;
                     BETWEEN(fecha,  ;
                     vfecini,  ;
                     vfecfin)  ;
                     .AND.  ;
                     LEFT(codcta,  ;
                     3) = '384'  ;
                     .AND. tipdoc =  ;
                     'C/P'
               SET RELATION TO nummes;
+ numref + codctc INTO compag ADDITIVE
               SET RELATION TO nummes;
+ numref + codctc INTO cheque ADDITIVE
               DEACTIVATE WINDOW  ;
                          xwait
               IF EOF()
                    DO standby  ;
                       WITH  ;
                       'Registros no existen'
                    SET INDEX TO
                    USE IN 9
                    SELECT compag
                    GOTO BOTTOM
                    RETURN
               ENDIF
               vtitulo = 'ANTICIPOS'
               DO reporte WITH 2,  ;
                  'LisCpCon',  ;
                  ' Resumen de Comprobantes de Pago de Anticipos'
               SET INDEX TO
               USE IN 9
     ENDCASE
     SELECT compag
ENDIF
USE IN 9
USE IN 14
USE IN 6 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 7 IteHc ALIAS itehc ORDER  ;
    IteHc1
USE IN 8 Clase ALIAS clase ORDER  ;
    Clase1
SELECT compag
GOTO vrecno
DO vista
RETURN
*
PROCEDURE val_cp
SELECT compag
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SEEK vano + vcli
IF  .NOT. FOUND()
     vtemp = RECNO()
     HIDE MENU mmenu1
     ACTIVATE SCREEN
     vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     ON KEY LABEL f10 KEYBOARD CHR(23)
     BROWSE FIELDS x1 = numcp +  ;
            '.' + nummes :H =  ;
            ' N§ CP', feccp,  ;
            codctc :H = 'CtaCte',  ;
            tipprv, x4 =  ;
            IIF((tipprv = 'O'  ;
            .OR. EMPTY(tipprv)),  ;
            nompre, IIF(tipprv =  ;
            'P',  ;
            LEFT(val_fun('Auxil', ;
            'descri','descri', ;
            codprv), 20),  ;
            LEFT(val_fun('Auxil', ;
            'descri','descri', ;
            codemp), 20))) :H =  ;
            'Nombre', codfte :H =  ;
            'Fte.Fin.', import :H =  ;
            'Total CP', x3 =  ;
            numhc + '.' +  ;
            nummeshc :H =  ;
            ' N§ H/C', codcal :H =  ;
            'Calendario', estado  ;
            :H = 'Estado' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_0
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     SHOW MENU mmenu1
     IF LASTKEY() = 27
          GOTO BOTTOM
     ENDIF
ENDIF
vano = nummes
vcli = numcp
vcta = codctc
ON KEY LABEL f10
SELECT compag
RETURN
*
PROCEDURE liscp
PARAMETER xcop
PRIVATE fila, xval, tval, vpar,  ;
        lval, x_sw1, vref
STORE 0 TO x_sw1, xval, tval,  ;
      fila
DO CASE
     CASE _dest1 = 1
          SET DEVICE TO FILE (p_fil)
     CASE _dest1 = 2
          SET DEVICE TO PRINTER
     CASE _dest1 = 3
          SET DEVICE TO FILE (p_fil)
ENDCASE
USE IN 14 cuentas ALIAS cuenta  ;
    ORDER cuentas1
USE IN 9 astpat ALIAS astpat  ;
    ORDER astpat3
SELECT compag
vcad = codcad
vper = periodo
vano = ALLTRIM(nummes)
vcli = numcp
vcta = codctc
vkey = compag.nummes +  ;
       compag.numcp +  ;
       compag.codctc
vref = compag.refer
lval = 0
@ 0, 0 SAY CHR(18)
@ 1, 3 SAY ALLTRIM(cia)
@ 1, 68 SAY 'PAG:'
@ 1, 76 SAY ALLTRIM(STR(_PAGENO,  ;
  8))
@ 2, 3 SAY 'LisCp'
@ 2, 68 SAY 'FECHA:'
@ 2, 76 SAY DATE()
IF tipdoc <> 'IN'
     @ 5, 14 SAY CHR(14)
     @ 5, 15 SAY  ;
       '<< COMPROBANTE DE PAGO >>'
     @ 5, 44 SAY CHR(27) +  ;
       CHR(18)
     @ 07, 03 SAY  ;
       '      N§ C/P :'
     @ 07, 20 SAY numcp + '.' +  ;
       nummes
     @ 07, 68 SAY 'Estado :'
     @ 07, 76 SAY IIF(estado =  ;
       '00', 'Emitido',  ;
       IIF(estado = '20',  ;
       'Girado', IIF(estado =  ;
       '99', 'Anulado',  ;
       IIF(estado = '50',  ;
       'Pagado', IIF(estado =  ;
       '92', 'Inutilizado',  ;
       IIF(estado = '10',  ;
       'Girado', ' -  '))))))
     @ 08, 03 SAY  ;
       '   Fecha C/P :'
     @ 08, 20 SAY feccp PICTURE  ;
       '@D'
     @ 09, 03 SAY  ;
       '         Son :'
     @ 09, 20 SAY letras(import -  ;
       reten,'SOLES')
     @ 10, 03 SAY  ;
       'Raz¢n Social :'
     @ 10, 20 SAY IIF(tipdoc $  ;
       'RESRRURS', codret,  ;
       IIF((tipprv = 'O' .OR.  ;
       EMPTY(tipprv) .OR. tipdoc =  ;
       'RG' .OR. tipdoc = 'HM'),  ;
       ' ', IIF((tipprv = 'P'  ;
       .AND. tipdoc <> 'RG' .AND.  ;
       tipdoc <> 'HM'), codprv,  ;
       IIF(tipdoc <> 'RG' .AND.  ;
       tipdoc <> 'HM', codemp,  ;
       ' '))))
     @ 10, 27 SAY IIF(tipdoc $  ;
       'RESRRURS',  ;
       val_para(codret,'CODRET', ;
       'D',28,40), IIF((tipprv =  ;
       'O' .AND. EMPTY(codotr)  ;
       .OR. EMPTY(tipprv) .OR.  ;
       tipdoc = 'RG' .OR. tipdoc =  ;
       'HM'), compag.nompre,  ;
       IIF((tipprv = 'P' .AND.  ;
       tipdoc <> 'RG' .AND.  ;
       tipdoc <> 'HM'),  ;
       val_auxi(ALLTRIM(codprv), ;
       '20','D',40), IIF(tipdoc <>  ;
       'RG' .AND. tipdoc <> 'HM'  ;
       .AND. tipprv = 'E',  ;
       val_auxi(ALLTRIM(codemp), ;
       '30','D',40), IIF(tipdoc <>  ;
       'RG' .AND. tipdoc <> 'HM'  ;
       .AND. tipprv = 'O',  ;
       val_auxi(ALLTRIM(codotr), ;
       '09','D',40), IIF(tipprv =  ;
       'R' .AND.  .NOT.  ;
       EMPTY(codotr) .AND. tipdoc =  ;
       'ME',  ;
       val_para(ALLTRIM(codotr), ;
       'CODRET','D',28,40),  ;
       ' '))))))
     @ 11, 03 SAY  ;
       '  Referencia :'
     @ 11, 20 SAY docref
     @ 11, 24 SAY numref + ' ' +  ;
       refer
     @ 12, 03 SAY  ;
       '     Cta.Cte.:'
     @ 12, 20 SAY codctc
     @ 12, 35 SAY val_fun('Caja', ;
       'codctc','Descri',codctc)
     @ 13, 03 SAY  ;
       '     Fte.Fto.:'
     @ 13, 20 SAY codfte
     @ 13, 25 SAY  ;
       ALLTRIM(val_para(codfte, ;
       'CODFTE','D',26,50))
     IF incpres = 'S'
          = val_codc1(ALLTRIM(vcad), ;
            vper + '01001','C')
          @ 15, 04 SAY  ;
            'Cadena Func.:   UG  UE  FN PRG SBPRG ACTPRY'
          @ 16, 20 SAY  ;
            maepre.uniges + '  ' +  ;
            maepre.unieje + ' ' +  ;
            maepre.codfun + ' ' +  ;
            maepre.codprg + ' ' +  ;
            maepre.codspr + '  ' +  ;
            maepre.actpry
     ENDIF
     @ 18, 08 SAY  ;
       IIF(EMPTY(destino), ' ',  ;
       'Destino :')
     @ 18, 19 SAY CHR(15)
     @ 18, 22 SAY destino
     @ 19, 31 SAY CHR(18)
     @ 19, 40 SAY  ;
       'C O N C E P T O'
     @ 20, 01 SAY CHR(15)
     @ 20, 03 SAY SUBSTR(glosa, 1,  ;
       130)
     @ 21, 03 SAY SUBSTR(glosa,  ;
       131, 70)
     @ 22, 32 SAY CHR(18)
     @ 22, 40 SAY 'OBSERVACIONES'
     @ 23, 01 SAY CHR(15)
     @ 23, 03 SAY SUBSTR(observ,  ;
       1, 100)
     @ 23, 24 SAY CHR(18)
     @ 24, 25 SAY  ;
       '<< CONTABILIDAD PRESUPUESTAL >>'
     @ 25, 10 SAY 'Cuentas'
     @ 25, 52 SAY 'Debe'
     @ 25, 72 SAY 'Haber'
     @ 26, 01 SAY ctadeb
     @ 26, 13 SAY  ;
       val_fun('Cuenta','Cuenta', ;
       'SUBSTR(Descri,1,26)', ;
       SUBSTR(ctadeb, 1, 3))
     @ 26, 40 SAY valdeb PICTURE  ;
       '@Z 999,999,999,999.99'
     @ 27, 01 SAY ctahab
     @ 27, 13 SAY  ;
       val_fun('Cuenta','Cuenta', ;
       'SUBSTR(Descri,1,26)', ;
       SUBSTR(ctahab, 1, 3))
     @ 27, 60 SAY valhab PICTURE  ;
       '@Z 999,999,999,999.99'
     @ 29, 20 SAY  ;
       '<< ESTADISTICA DIARIA OBJETO DEL GASTO >>'
     @ 30, 03 SAY  ;
       '     Componente      Meta      Partida            Parcial               Total'
     SELECT itecp
     SEEK vkey
     @ 31, 06 SAY '    ' +  ;
       itecp.codcom + '         ' +  ;
       itecp.codmet
     @ 31, 34 SAY  ;
       ALLTRIM(itecp.codpart)
     fila = 31
     SCAN WHILE nummes =  ;
          ALLTRIM(vano) .AND.  ;
          numcp = vcli .AND.  ;
          codctc = ALLTRIM(vcta)
          IF fila >= 32
               IF itecp.codpart <>  ;
                  vpar
                    fila = fila -  ;
                           1
                    @ fila, 62  ;
                      SAY xval  ;
                      PICTURE  ;
                      '@Z 999,999,999,999.99'
                    fila = fila +  ;
                           1
                    xval = 0
               ENDIF
          ENDIF
          IF fila >= 32
               @ fila, 06 SAY  ;
                 '    ' +  ;
                 itecp.codcom +  ;
                 '         ' +  ;
                 itecp.codmet
               @ fila, 34 SAY  ;
                 ALLTRIM(itecp.codpart)
          ENDIF
          @ fila, 40 SAY  ;
            itecp.impparc PICTURE  ;
            '@Z 999,999,999,999.99'
          vpar = itecp.codpart
          xval = xval +  ;
                 itecp.impparc
          tval = tval +  ;
                 itecp.impparc
          fila = fila + 1
     ENDSCAN
     SELECT compag
     fila = fila - 1
     @ fila, 62 SAY xval PICTURE  ;
       '@Z 999,999,999,999.99'
     fila = fila + 2
     @ fila, 40 SAY  ;
       '      TOTAL :'
     @ fila, 62 SAY tval PICTURE  ;
       '@Z 999,999,999,999.99'
     fila = fila + 1
     @ fila, 40 SAY  ;
       'DEDUCCIONES :'
     @ fila, 62 SAY reten PICTURE  ;
       '@Z 999,999,999,999.99'
     fila = fila + 1
     @ fila, 40 SAY  ;
       '    LIQUIDO :'
     @ fila, 62 SAY import -  ;
       reten PICTURE  ;
       '@Z 999,999,999,999.99'
     fila = fila + 2
     @ fila, 25 SAY  ;
       '<< CONTABILIDAD PATRIMONIAL >>'
     fila = fila + 1
     @ fila, 08 SAY 'Cuenta D'
     @ fila, 32 SAY 'Importe'
     @ fila, 46 SAY 'Cuenta H'
     @ fila, 72 SAY 'Importe'
     fila = fila + 1
     SELECT astpat
     SEEK vkey
     SCAN WHILE astpat.nummes =  ;
          ALLTRIM(vano) .AND.  ;
          astpat.numref = vcli  ;
          .AND. astpat.codctc =  ;
          ALLTRIM(vcta)
          IF astpat.ret = 'N'
               IF astpat.tipcta =  ;
                  'D'
                    @ fila, 06  ;
                      SAY  ;
                      astpat.codcta
                    @ fila, 20  ;
                      SAY  ;
                      IIF(LEFT(ALLTRIM(astpat.codcta),  ;
                      2) = '10',  ;
                      astpat.mtodeb,  ;
                      astpat.mtodeb -  ;
                      compag.reten)  ;
                      PICTURE  ;
                      '@z 999,999,999,999.99'
               ELSE
                    @ fila, 44  ;
                      SAY  ;
                      astpat.codcta
                    @ fila, 60  ;
                      SAY  ;
                      astpat.mtohab  ;
                      PICTURE  ;
                      '@z 999,999,999,999.99'
               ENDIF
               fila = fila + 1
          ENDIF
     ENDSCAN
     fila = fila + 1
     @ fila, 3 SAY 'Ú'
     @ fila, 4 SAY REPLICATE('Ä',  ;
       30)
     @ fila, 34 SAY '¿'
     @ fila, 37 SAY 'Ú'
     @ fila, 38 SAY REPLICATE('Ä',  ;
       40)
     @ fila, 78 SAY '¿'
     fila = fila + 1
     @ fila, 3 SAY '³'
     @ fila, 10 SAY  ;
       ' RECIBI CONFORME'
     @ fila, 34 SAY '³'
     @ fila, 37 SAY '³'
     @ fila, 78 SAY '³'
     fila = fila + 1
     @ fila, 3 SAY '³'
     @ fila, 34 SAY '³'
     @ fila, 37 SAY '³'
     @ fila, 38 SAY  ;
       ' -----------------'
     @ fila, 59 SAY  ;
       ' -----------------'
     @ fila, 78 SAY '³'
     fila = fila + 1
     @ fila, 3 SAY '³'
     @ fila, 34 SAY '³'
     @ fila, 37 SAY '³'
     @ fila, 39 SAY  ;
       'CONTROL INTERNO '
     @ fila, 61 SAY  ;
       'CAJERO GENERAL'
     @ fila, 78 SAY '³'
     fila = fila + 1
     @ fila, 3 SAY '³'
     @ fila, 6 SAY  ;
       ' ------------------------'
     @ fila, 34 SAY '³'
     @ fila, 37 SAY '³'
     @ fila, 78 SAY '³'
     fila = fila + 1
     @ fila, 3 SAY '³'
     @ fila, 34 SAY '³'
     @ fila, 37 SAY '³'
     @ fila, 78 SAY '³'
     fila = fila + 1
     @ fila, 3 SAY '³'
     @ fila, 4 SAY ' L.E. :'
     @ fila, 34 SAY '³'
     @ fila, 37 SAY '³'
     @ fila, 38 SAY  ;
       ' -----------------'
     @ fila, 59 SAY  ;
       ' -----------------'
     @ fila, 78 SAY '³'
     fila = fila + 1
     @ fila, 3 SAY '³'
     @ fila, 4 SAY ' FECHA :'
     @ fila, 12 SAY '  /  /'
     @ fila, 34 SAY '³'
     @ fila, 37 SAY '³'
     @ fila, 39 SAY  ;
       'VISACION CONTADOR'
     @ fila, 61 SAY  ;
       'AUTORIZACION'
     @ fila, 78 SAY '³'
     fila = fila + 1
     @ fila, 3 SAY 'À'
     @ fila, 4 SAY REPLICATE('Ä',  ;
       30)
     @ fila, 34 SAY 'Ù'
     @ fila, 37 SAY 'À'
     @ fila, 38 SAY REPLICATE('Ä',  ;
       40)
     @ fila, 78 SAY 'Ù'
     fila = fila + 1
     @ fila, 12 SAY 'N§ Cheque :'
     SELECT cheque
     SEEK vkey
     @ fila, 24 SAY  ;
       IIF(EMPTY(cheque.numchq),  ;
       ' Sin Cheque',  ;
       cheque.numchq)
     SELECT compag
     @ fila, 40 SAY 'LOGIN:'
     @ fila, 50 SAY disp_usu()
     @ fila + 1, 40 SAY 'Fecha:'
     @ fila + 1, 50 SAY  ;
       compag.fecdig
     @ fila + 2, 40 SAY 'Hora :'
     @ fila + 2, 50 SAY  ;
       compag.hordig
     IF tipdoc $ 'RESRRURS' .AND.  ;
        estado <> '99'
          USE IN 12 reten ALIAS  ;
              reten ORDER reten5
          x_sw1 = 1
     ELSE
          USE IN 12 reten ALIAS  ;
              reten ORDER reten1
     ENDIF
     SELECT compag
     vrec = RECNO()
     SELECT reten
     SEEK vkey
     IF FOUND()
          IF _dest1 = 2
               EJECT
          ENDIF
          DO cab_ret
          DO CASE
               CASE x_sw1 = 1
                    yz = 1
                    SCAN WHILE  ;
                         mescppg =  ;
                         vano  ;
                         .AND.  ;
                         numcppg =  ;
                         vcli  ;
                         .AND.  ;
                         codctc =  ;
                         vcta  ;
                         .AND.  ;
                         conpago =  ;
                         'û'
                         @ fila,  ;
                           03 SAY  ;
                           reten.nummes  ;
                           PICTURE  ;
                           '@J'
                         @ fila,  ;
                           05 SAY  ;
                           '.'
                         @ fila,  ;
                           06 SAY  ;
                           reten.numcp  ;
                           PICTURE  ;
                           '@J'
                         @ fila,  ;
                           15 SAY  ;
                           dispglo(yz)
                         @ fila,  ;
                           53 SAY  ;
                           reten.valret  ;
                           PICTURE  ;
                           '@Z 999,999,999.99'
                         fila = fila +  ;
                                1
                         yz = yz +  ;
                              1
                         @ fila,  ;
                           15 SAY  ;
                           dispglo(yz)
                         fila = fila +  ;
                                1
                         yz = yz +  ;
                              1
                         @ fila,  ;
                           15 SAY  ;
                           dispglo(yz)
                         fila = fila +  ;
                                1
                         yz = 1
                         lval = lval +  ;
                                reten.valret
                    ENDSCAN
               CASE x_sw1 = 0
                    SCAN WHILE  ;
                         nummes =  ;
                         vano  ;
                         .AND.  ;
                         numcp =  ;
                         vcli  ;
                         .AND.  ;
                         codctc =  ;
                         vcta
                         @ fila,  ;
                           03 SAY  ;
                           reten.codret  ;
                           PICTURE  ;
                           '@J'
                         @ fila,  ;
                           15 SAY  ;
                           reten.nomret
                         @ fila,  ;
                           53 SAY  ;
                           reten.valret  ;
                           PICTURE  ;
                           '@Z 999,999,999.99'
                         fila = fila +  ;
                                1
                         lval = lval +  ;
                                reten.valret
                    ENDSCAN
          ENDCASE
          fila = fila + 1
          @ fila, 01 SAY  ;
            REPLICATE('-', 71)
          fila = fila + 1
          @ fila, 20 SAY  ;
            'T O T A L  ---->'
          @ fila, 53 SAY lval  ;
            PICTURE  ;
            '@Z 999,999,999.99'
          fila = fila + 1
          @ fila, 01 SAY  ;
            REPLICATE('-', 71)
          IF vref = '**********'
               DO cab_ret1
               lval = 0
               SELECT reten
               SET ORDER TO RETEN1
               SEEK vkey
               SCAN WHILE nummes =  ;
                    vano .AND.  ;
                    numcp = vcli  ;
                    .AND. codctc =  ;
                    vcta
                    @ fila, 03  ;
                      SAY  ;
                      reten.codret  ;
                      PICTURE  ;
                      '@J'
                    @ fila, 15  ;
                      SAY  ;
                      reten.nomret
                    @ fila, 53  ;
                      SAY  ;
                      reten.valret  ;
                      PICTURE  ;
                      '@Z 999,999,999.99'
                    fila = fila +  ;
                           1
                    lval = lval +  ;
                           reten.valret
               ENDSCAN
               fila = fila + 1
               @ fila, 01 SAY  ;
                 REPLICATE('-',  ;
                 71)
               fila = fila + 1
               @ fila, 20 SAY  ;
                 'T O T A L  ---->'
               @ fila, 53 SAY  ;
                 lval PICTURE  ;
                 '@Z 999,999,999.99'
               fila = fila + 1
               @ fila, 01 SAY  ;
                 REPLICATE('-',  ;
                 71)
          ENDIF
     ENDIF
     USE IN 12
     SELECT compag
     GOTO vrec
ELSE
     @ 5, 14 SAY CHR(14)
     @ 5, 15 SAY  ;
       '<< COMPROBANTE DE PAGO >>'
     @ 5, 44 SAY CHR(27) +  ;
       CHR(18)
     @ 07, 03 SAY  ;
       '      N§ C/P :'
     @ 07, 20 SAY numcp + '.' +  ;
       nummes
     @ 08, 03 SAY  ;
       '   Fecha C/P :'
     @ 08, 20 SAY feccp PICTURE  ;
       '@D'
     @ 09, 03 SAY  ;
       '     Cta cte :'
     @ 09, 20 SAY codctc
     @ 09, 35 SAY val_fun('Caja', ;
       'codctc','Descri',codctc)
     @ 14, 14 SAY CHR(14)
     @ 14, 18 SAY  ;
       'I N U T I L I Z A D O'
     @ 15, 01 SAY CHR(27) +  ;
       CHR(18)
ENDIF
IF _dest1 = 2
     EJECT
ENDIF
SET DEVICE TO SCREEN
RETURN
*
FUNCTION dispglo
PARAMETER zy
PRIVATE valias
valias = ALIAS()
SELECT compag
SEEK ALLTRIM(reten.nummes) +  ;
     reten.numcp +  ;
     ALLTRIM(reten.codctc)
IF FOUND()
     vglosa = IIF(zy = 1,  ;
              SUBSTR(compag.glosa,  ;
              21, 36), IIF(zy = 2,  ;
              SUBSTR(compag.glosa,  ;
              58, 36), IIF(zy = 3,  ;
              SUBSTR(compag.glosa,  ;
              95, 36), ' ')))
ELSE
     STORE SPACE(33) TO vglosa
ENDIF
SELECT (valias)
RETURN vglosa
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
ON KEY
CLOSE DATABASES
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RELEASE WINDOW wind_3
RELEASE WINDOW wind_4
RELEASE WINDOW wind_c1
RELEASE MENU mmenu1
RESTORE SCREEN FROM principal
RETURN
*
PROCEDURE fecha
vfecha = fecret
RETURN
*
FUNCTION vale
IF EMPTY(m.codctc)
     DO standby WITH  ;
        'No esta asignada la Cuenta Corriente'
     RETURN .F.
ELSE
     RETURN .T.
ENDIF
*
PROCEDURE imp_cp
PUBLIC num_c
num_c = 1
SET ESCAPE ON
ON ESCAPE STORE .F. TO PRINTING
_dest = 'Impresora'
p_fil = SPACE(8)
_dest1 = 2
p_fil = SYS(3) + '.LST'
impre = (_dest = 'Impresora')
printing = .T.
DEFINE WINDOW numcop FROM 20, 50  ;
       TO 22, 79 DOUBLE
ACTIVATE WINDOW numcop
@ 0, 3 SAY 'N§ de Copias : ' GET  ;
  num_c PICTURE '99' VALID num_c >  ;
  0 .AND. num_c < 99
READ
RELEASE WINDOW numcop
IF LASTKEY() = 27
     RETURN
ENDIF
IF impre
     IF  .NOT. EMPTY(LEFT(SYS(0),  ;
         15))
          IF  .NOT.  ;
              yesno( ;
              '¨Imprime en impresora local?  <NO = IMPRESORA DE RED>' ;
              )
               SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
               SET PRINTER TO \\SPOOLER\NB
          ENDIF
     ENDIF
     IF  .NOT. ready2pr()
          printing = .F.
     ENDIF
ENDIF
IF printing
     IF impre
          SET DEVICE TO PRINTER
          SET PRINTER TO &p_fil
          DO liscp WITH num_c
          SET PRINTER TO
          IF ready2pr()
               FOR i = 1 TO num_c
                    !TYPE &P_FIL >PRN
               ENDFOR
          ENDIF
          ERASE &p_fil
     ENDIF
ELSE
     DO standby WITH  ;
        'EL REPORTE HA SIDO CANCELADO.'
ENDIF
SET DEVICE TO SCREEN
SELECT compag
RETURN
*
PROCEDURE valsec
PUBLIC vinicp
IF vsector <> 'A'
     vinicp = LEFT(m.numhc, 1)
ELSE
     vinicp = 'A'
ENDIF
IF m.codfte <> '00'
     SELECT caja
     SEEK ALLTRIM(m.codctc)
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'No se tiene inscrita la cuenta '+ ;
             m.codctc
     ELSE
          m.numcp = vinicp +  ;
                    PADL(corcp +  ;
                    1, 3, '0')
          w_ctah = caja.cuentah
     ENDIF
ELSE
     USE IN 0 CtaSec ALIAS ctasec  ;
         ORDER CtaSec1
     SELECT ctasec
     SEEK m.codctc + vinicp
     m.numcp = PADL(ctasec.corsec +  ;
               1, 3, '0')
     USE
     vinicp = vinicp +  ;
              RIGHT(m.numcp, 3)
     SELECT compag
     @ 1, 22 SAY vinicp PICTURE  ;
       '!!!!'
     m.numcp = vinicp
ENDIF
RETURN
*
FUNCTION mfecha
PARAMETER xmes, xano
meses = 'ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE'
RETURN ALLTRIM(SUBSTR(meses, xmes *  ;
       9 - 8, 9)) + ' ' +  ;
       STR(xano, 2)
*
FUNCTION vexiste
SELECT compag
SEEK ALLTRIM(m.nummes) + m.numcp +  ;
     ALLTRIM(m.codctc)
IF FOUND()
     DO standby WITH  ;
        'El Comprobante de Pago ya existe'
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION dispdes
DO CASE
     CASE compag.tipdoc $  ;
          'HCMESUSHSS'
          vdes = IIF(compag.tipprv =  ;
                 'P',  ;
                 val_auxi(ALLTRIM(compag.codprv), ;
                 '20','D'),  ;
                 IIF(compag.tipprv =  ;
                 'E',  ;
                 val_auxi(ALLTRIM(compag.codemp), ;
                 '30','D'),  ;
                 IIF(compag.tipprv =  ;
                 'O' .AND.  .NOT.  ;
                 EMPTY(m.codotr),  ;
                 val_auxi(ALLTRIM(compag.codotr), ;
                 '09','D'),  ;
                 compag.nompre)))
     CASE compag.tipdoc $  ;
          'RESRRURS'
          vdes = val_para(compag.codret, ;
                 'CODRET','D',22, ;
                 40)
     OTHERWISE
          vdes = compag.nompre
ENDCASE
RETURN vdes
*
FUNCTION disppart
PRIVATE ypart
ypart = 0
IF compag.tipdoc $ 'HCRGSHSRSEHM'
     SELECT itecp
     SET ORDER TO Itecp1
     SEEK ALLTRIM(compag.nummes) +  ;
          compag.numcp +  ;
          ALLTRIM(compag.codctc)
     IF FOUND()
          IF compag.tipfun = 'I'
               xpart = itecp.codpart
          ELSE
               ypart = VAL(itecp.codanal)
               SCAN WHILE  ;
                    ALLTRIM(itecp.nummes) +  ;
                    itecp.numcp +  ;
                    ALLTRIM(itecp.codctc) =  ;
                    ALLTRIM(compag.nummes) +  ;
                    compag.numcp +  ;
                    ALLTRIM(compag.codctc)
                    IF VAL(itecp.codanal) >=  ;
                       ypart
                         ypart = VAL(itecp.codanal)
                    ENDIF
               ENDSCAN
               xpart = IIF(ypart <  ;
                       10, '0' +  ;
                       ALLTRIM(STR(ypart,  ;
                       5, 2)),  ;
                       ALLTRIM(STR(ypart,  ;
                       5, 2)))
          ENDIF
     ELSE
          DO standby WITH  ;
             'Sin partida'
     ENDIF
ENDIF
RETURN xpart
*
PROCEDURE cab_ret
@ 0, 0 SAY CHR(18)
@ 1, 3 SAY ALLTRIM(cia)
@ 1, 68 SAY 'PAG:'
@ 1, 76 SAY ALLTRIM(STR(_PAGENO,  ;
  8))
@ 2, 3 SAY 'LisRetCP'
@ 2, 68 SAY 'FECHA:'
@ 2, 76 SAY DATE()
@ 4, 20 SAY CHR(14)
@ 4, 13 SAY IIF(x_sw1 = 1,  ;
  'RETENCIONES CANCELADAS',  ;
  'DEDUCCIONES Y/O RETENCIONES')
@ 5, 0 SAY CHR(27) + CHR(18)
@ 5, 34 SAY 'C/P: ' + vcli + '.' +  ;
  ALLTRIM(vano)
@ 6, 1 SAY 'Ú'
@ 6, 2 SAY REPLICATE('Ä', 70)
@ 6, 72 SAY '¿'
@ 7, 1 SAY '³'
@ 7, 3 SAY IIF(x_sw1 = 1,  ;
  ' C/P              DESCRIPCION                              MONTO',  ;
  ' RUBRO            DESCRIPCION                              MONTO' ;
  )
@ 7, 72 SAY '³'
@ 8, 1 SAY 'À'
@ 8, 2 SAY REPLICATE('Ä', 70)
@ 8, 72 SAY 'Ù'
fila = 10
RETURN
*
PROCEDURE cab_ret1
@ 0, 0 SAY CHR(18)
@ 1, 3 SAY ALLTRIM(cia)
@ 1, 68 SAY 'PAG:'
@ 1, 76 SAY ALLTRIM(STR(_PAGENO,  ;
  8))
@ 2, 3 SAY 'LisRetCP'
@ 2, 68 SAY 'FECHA:'
@ 2, 76 SAY DATE()
@ 4, 20 SAY CHR(14)
@ 4, 13 SAY  ;
  'DEDUCCIONES Y/O RETENCIONES'
@ 5, 0 SAY CHR(27) + CHR(18)
@ 5, 34 SAY 'C/P: ' + vcli + '.' +  ;
  ALLTRIM(vano)
@ 6, 1 SAY 'Ú'
@ 6, 2 SAY REPLICATE('Ä', 70)
@ 6, 72 SAY '¿'
@ 7, 1 SAY '³'
@ 7, 3 SAY IIF(x_sw1 = 1,  ;
  ' C/P              DESCRIPCION                              MONTO',  ;
  ' RUBRO            DESCRIPCION                              MONTO' ;
  )
@ 7, 72 SAY '³'
@ 8, 1 SAY 'À'
@ 8, 2 SAY REPLICATE('Ä', 70)
@ 8, 72 SAY 'Ù'
fila = 10
RETURN
*
FUNCTION wdbco
PRIVATE wdbc, wcbc
SELECT caja
SEEK compag.codctc
IF FOUND()
     wcbc = caja.banco
     wdbc = val_para(wcbc, ;
            'BANCOS','D',22,25)
ELSE
     wdbc = SPACE(10)
ENDIF
RETURN wdbc
*
FUNCTION disp_usu
PRIVATE xuser_id, xdesu
vali = ALIAS()
vord = ORDER()
xuser_id = SPACE(08)
xdesu = SPACE(30)
xuser_id = CHRTRAN(compag.usuario, 'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?',;
'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
USE IN 0 USUARIO ALIAS usuario  ;
    ORDER USUARIO1
SELECT usuario
SEEK ALLTRIM(xuser_id)
IF FOUND()
     xdesu = ALLTRIM(usuario.nombre)
ENDIF
USE
SELECT (vali)
SET ORDER TO (vord)
RETURN xdesu
*
PROCEDURE limpia
@ 3, 22 SAY SPACE(56)
RETURN
*
PROCEDURE pres_pro
DO CASE
     CASE m.tipprv = 'E'
          @ 3, 22 SAY  ;
            val_auxi(ALLTRIM(m.codemp), ;
            '30','V')
     CASE m.tipprv = 'P'
          @ 3, 22 SAY  ;
            val_auxi(ALLTRIM(m.codprv), ;
            '20','V')
     CASE m.tipprv = 'O' .AND.   ;
          .NOT. EMPTY(m.codotr)
          @ 3, 22 SAY  ;
            val_auxi(ALLTRIM(m.codotr), ;
            '09','V')
     CASE m.tipprv = 'R' .AND.   ;
          .NOT. EMPTY(m.codotr)
          @ 3, 22 SAY  ;
            val_para(m.codotr, ;
            'CODRET','V',22,40)
     OTHERWISE
          @ 3, 22 SAY m.nompre
ENDCASE
RETURN
*
FUNCTION muespro
PARAMETER xtipprv, xcodotr,  ;
          xtipdoc
PRIVATE xnompre
DO CASE
     CASE xtipprv = 'E'
          xnompre = val_auxi(ALLTRIM(compag.codemp), ;
                    '30','V')
     CASE xtipprv = 'P'
          xnompre = val_auxi(ALLTRIM(compag.codprv), ;
                    '20','V')
     CASE xtipprv = 'O' .AND.   ;
          .NOT. EMPTY(xcodotr)
          xnompre = val_auxi(ALLTRIM(xcodotr), ;
                    '09','V')
     CASE xtipdoc $ 'RESRRU'
          xnompre = val_para(compag.codret, ;
                    'CODRET','V', ;
                    22,40)
     OTHERWISE
          xnompre = compag.nompre
ENDCASE
RETURN xnompre
*
FUNCTION val_rete
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong,  ;
          mdist
PRIVATE malias
mcol = 0
mlong = 40
mdist = 6
malias = ALIAS()
SELECT parma
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO tipo = filtro
     SET ORDER TO PARMAE2
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen Registros para Procesar'
          SET FILTER TO
          SELECT (malias)
          RETURN
     ENDIF
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            SUBSTR(descri, 1,  ;
            40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     RELEASE POPUP parametro
     SELECT parma
     SET FILTER TO
     SET ORDER TO PARMAE1
ENDIF
mvalor = parma.codigo
mcuenta = parma.descriau2
mdescr = SUBSTR(parma.descri, 1,  ;
         mlong)
mdescriaux = LEFT(parma.descriau2,  ;
             1)
SELECT (malias)
REPLACE reten.codret WITH mvalor
REPLACE reten.nomret WITH mdescr
REPLACE reten.tributo WITH  ;
        mdescriaux
RETURN .T.
*
PROCEDURE vis_calen
@ 06, 22 SAY m.codcad
@ 06, 27 SAY  ;
  ' UG  UE  FN PRG SBPRG ACTPRY'
@ 07, 28 SAY maepre.uniges + '  ' +  ;
  maepre.unieje + ' ' +  ;
  maepre.codfun + ' ' +  ;
  maepre.codprg + ' ' +  ;
  maepre.codspr + '  ' +  ;
  maepre.actpry
RETURN
*
PROCEDURE val_ff
m.fecdig = DATE()
m.hordig = TIME()
RETURN
*
PROCEDURE regret
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL f10 KEYBOARD CHR(23)
USE IN 12 reten ALIAS reten ORDER  ;
    reten1
USE IN 9 astpat ALIAS astpat  ;
    ORDER astpat3
SELECT itecp
SEEK ALLTRIM(m.nummes) + m.numcp +  ;
     ALLTRIM(m.codctc)
vvcompon = itecp.codcom
SELECT astpat
SEEK ALLTRIM(m.nummes) + m.numcp +  ;
     ALLTRIM(m.codctc)
STORE 0 TO vdebe, vhaber
SCAN WHILE nummes =  ;
     ALLTRIM(m.nummes) .AND.  ;
     numref = m.numcp .AND.  ;
     codctc = ALLTRIM(m.codctc)
     IF ret = 'S'
          vdebe = vdebe +  ;
                  IIF(tipcta =  ;
                  'D', mtodeb,  ;
                  0)
          vhaber = vhaber +  ;
                   IIF(tipcta =  ;
                   'H', mtohab,  ;
                   0)
     ENDIF
ENDSCAN
SELECT reten
SEEK ALLTRIM(m.nummes) + m.numcp +  ;
     ALLTRIM(m.codctc)
vale = 0
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No existen retenciones registradas'
ENDIF
BROWSE FIELDS estado :H = 'Es' :W =  ;
       .F. :R, fecret :H =  ;
       'Fecha' :V = fecha(),  ;
       codret :V =  ;
       val_rete(codret,'CODRET', ;
       'Codret'), nomret :H =  ;
       'Retenido a' :P = '@S39'  ;
       :R, valret :H = 'Monto' :P =  ;
       '99,999,999.99' NOMENU  ;
       NOAPPEND NODELETE WINDOW  ;
       wind_5 KEY  ;
       ALLTRIM(m.nummes) +  ;
       m.numcp +  ;
       ALLTRIM(m.codctc)
SELECT reten
GOTO TOP
SEEK ALLTRIM(m.nummes) + m.numcp +  ;
     ALLTRIM(m.codctc)
totalq = 0
SELECT compag
USE IN 9
USE IN 12
DO vista
RETURN
*
FUNCTION val_codc1
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
SELECT maepre
SET ORDER TO maepre1
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO periodo + uniges +;
unieje = filtro
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
     SET ORDER TO maepre4
     ON KEY LABEL f2 DO Completo
     ON KEY LABEL f3 DO PorCompon
     ON KEY LABEL f10 KEYBOARD CHR(23)
     DEFINE WINDOW wind_cad FROM  ;
            02, 01 TO 23, 78  ;
            TITLE  ;
            '[F2] Orden completo [F3] Orden x Act/Pry+Comp.  [F10] seleccionar'  ;
            DOUBLE COLOR SCHEME  ;
            15
     ACTIVATE WINDOW wind_cad
     BROWSE NOOPTIMIZE FIELDS  ;
            codcad :H = 'CodCad',  ;
            codfun :H = 'Fn',  ;
            codprg :H = 'Prg',  ;
            codspr :H = 'SPrg',  ;
            actpry :H = 'Act/Pry',  ;
            codcom :H = 'CodComp',  ;
            codmet :H = 'Meta',  ;
            descri :H =  ;
            'Descripci¢n' : 40  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR TITLE  ;
            'Relaci¢n de Cadenas Funcionales '  ;
            IN wind_cad  ;
            NOREFRESH
     RELEASE WINDOW wind_cad
     ON KEY LABEL f2
     ON KEY LABEL f3
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     SET FILTER TO
ENDIF
mvalor = maepre.codcad
mdescr = SUBSTR(maepre.descri, 1,  ;
         mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable = 'A'
          RETURN mcodaux
     CASE mvariable = 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescr
     CASE mvariable = 'D'
          RETURN mdescr
     CASE mvariable = 'Z'
          RETURN mdescr
     CASE mvariable = 'C'
          RETURN .T.
     OTHERWISE
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
*

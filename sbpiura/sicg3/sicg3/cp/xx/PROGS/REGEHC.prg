PARAMETER vopcion, sistema
CLOSE DATABASES
rutaactual = SET('PATH')
SET PATH TO H:\EMER98\DATA   
USE IN 1 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 2 Itehc ALIAS itehc ORDER  ;
    Itehc1
USE IN 3 PARMAE ALIAS parma ORDER  ;
    Parmae1
USE IN 4 maepre ALIAS maepre  ;
    ORDER maepre3
USE IN 5 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 6 Calen ALIAS calen ORDER  ;
    calen4
USE IN 12 Auxil ALIAS auxi ORDER  ;
    Auxil1
USE IN 14 Cuentas ALIAS cuenta  ;
    ORDER Cuentas6
USE IN 15 AstPre ALIAS astpre  ;
    ORDER Astpre1
ON KEY LABEL F4 DO IMPRIMIR
ON KEY LABEL F9 DO VISTA_DET
PUBLIC valcs, vvalpart, vtotafe,  ;
       vtotoc, vtotos, vpart,  ;
       vtomes, vmesx
PUBLIC vcodcom, vcodmet, vcodpar,  ;
       vcadena, vcodfte, vvalpart,  ;
       vtotafe, vnummes,  ;
       pathactual
STORE SPACE(5) TO vcodcom,  ;
      vcodmet
STORE SPACE(6) TO vcodpar
pathactual = rutaactual
valcs = .T.
vmens01 = ' Hoja de Afectaci¢n : REVISION '
vmens02 = IIF(vopcion = 1,  ;
          ' Registro de Hoja de Afectaci¢n [EMERGENCIA]',  ;
          ' Registro de Hoja de Afectaci¢n:SECTORES ' ;
          )
vmens04 = 'Dicho Hoja de Afectaci¢n no fue encontrado'
vmens05 = 'No existe Hoja de afectaci¢n anterior'
vmens06 = 'No existe Hoja de afectaci¢n siguiente'
vmens07 = '¨ Desea ANULAR ‚ste Hoja de afectaci¢n ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Hoja de Afectaci¢n ha sido anulada'
vmens10 = 'Este Hoja de afectaci¢n ya fue atendida'
vmens11 = 'Este Hoja de Afectaci¢n ha sido devuelto'
vmens12 = '°®F2¯ Observaciones °®F4¯ Imprime H/C °®F7¯ Buscar  °®F9¯ Detalle °'
SELECT hoja
GOTO BOTTOM
SCATTER BLANK MEMVAR
DO inicia
HIDE POPUP ALL
DO pantalla
DO vista
ON KEY LABEL F7 DO buscarhoja
ON KEY LABEL F2 DO VISOBS
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
DEFINE WINDOW wind_7 FROM 03, 03  ;
       TO 20, 77 TITLE  ;
       ' °   Documentos:  ®F10¯ Selecciona  ° '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 13, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1h FROM 00, 00  ;
       TO 13, 79 TITLE  ;
       'Registro Hoja Modificacion'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 14, 00  ;
       TO 23, 79 TITLE  ;
       ' Estad¡stica Diaria por Objeto del Gasto '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2a FROM 14, 41  ;
       TO 23, 79 TITLE  ;
       'Est. Diaria por Objeto del Gasto'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2b FROM 14, 00  ;
       TO 23, 40 TITLE  ;
       ' Detalle: ' DOUBLE COLOR  ;
       SCHEME 10
DEFINE WINDOW wind_3 FROM 20, 64  ;
       TO 22, 78 TITLE ' TOTAL '  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_4 FROM 20, 63  ;
       TO 22, 77 TITLE  ;
       ' PARTIDA ' COLOR SCHEME  ;
       10
DEFINE WINDOW wind_5 FROM 13, 10  ;
       TO 17, 70 TITLE  ;
       ' COMPROMISO PRESUPUESTAL '  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_6 FROM 14, 01  ;
       TO 16, 79 TITLE  ;
       ' Destino '
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
       'a\<Nula   ' AT 24, 54
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
@ 0, 2 SAY '          Periodo :'
@ 1, 2 SAY '       N£mero H/C :'
@ 1, 40 SAY '        Fecha H/C :'
@ 2, 2 SAY '   Tipo Documento :'
@ 2, 40 SAY '         Fte.Fto. :'
@ 3, 2 SAY ' N£mero Documento :'
@ 3, 26 SAY '.'
@ 3, 40 SAY '  Fecha Documento :'
@ 4, 2 SAY '        Proveedor :'
@ 5, 2 SAY 'Corr. cadena Fun. :'
@ 6, 2 SAY '          Funci¢n :'
@ 7, 2 SAY '         Programa :'
@ 8, 2 SAY '      SubPrograma :'
@ 9, 2 SAY '  Activ./Proyecto :'
RETURN
*
PROCEDURE vista
SELECT hoja
DO pantalla
IF EOF()
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
@ 0, 22 SAY m.periodo
@ 0, 55 SAY veresthc(m.estado)  ;
  COLOR SCHEME 02
@ 1, 22 SAY m.nummes
@ 1, 24 SAY '.'
@ 1, 25 SAY m.numhc
@ 1, 60 SAY m.fechc
@ 2, 22 SAY m.tipdoc
@ 2, 28 SAY val_para(m.tipdoc, ;
  'TIPDOC','D',28,20)
@ 2, 60 SAY m.codfte
@ 2, 63 SAY val_para(m.codfte, ;
  'CODFTE','D',63,15,2)
@ 3, 26 SAY '.'
@ 3, 22 SAY SPACE(55)
@ 3, 22 SAY IIF(m.tipdoc = 'O/',  ;
  m.numref, m.desref)
@ 3, 60 SAY m.fecref
@ 4, 22 SAY IIF(m.tipprv = 'O',  ;
  IIF(EMPTY(m.codotr), m.nombre,  ;
  val_aux(m.codotr,'09','D',24)),  ;
  IIF(m.tipprv = 'P',  ;
  val_aux(m.codprv,'20','D',24),  ;
  val_aux(m.codemp,'30','D', ;
  24)))
@ 5, 22 SAY m.codcad
@ 6, 22 SAY  ;
  val_para(maepre.codfun,'CODFUN', ;
  'V',22,40)
@ 7, 22 SAY  ;
  val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'V',22, ;
  40)
@ 8, 22 SAY  ;
  val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'V',22, ;
  40)
@ 9, 22 SAY maepre.actpry
@ 9, 29 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'D',23,40)
@ 10, 2 SAY IIF( .NOT.  ;
  EMPTY(m.destino),  ;
  '          Destino :',  ;
  '                     ')
@ 10, 22 SAY m.destino PICTURE  ;
  '@S56'
@ 11, 00 SAY PADC(vmens12, 79,  ;
  ' ') COLOR '7+/1'
DO vista_hijo
DO total
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
ACTIVATE WINDOW wind_2
SELECT itehc
SET ORDER TO itehc1
SEEK m.nummes + m.numhc
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            tipope :H = 'ä' :W =  ;
            .F., codcom :H =  ;
            'Comp.', codmet :H =  ;
            'Meta', codpart :H =  ;
            'Partid', aa =  ;
            val_para(RIGHT(codpart,  ;
            2),'ESPGAS','D',22, ;
            40) :H =  ;
            'Descripci¢n' : 40,  ;
            valpart :H =  ;
            '  Parcial' :P =  ;
            '99,999,999.99', xx =  ;
            IIF( .NOT.  ;
            itehc.estado = '92',  ;
            '       ', 'H/M:' +  ;
            numhm) :H = IIF(  ;
            .NOT. estado = '90',  ;
            '          ',  ;
            'Hoja Modificac') :  ;
            15 NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.nummes +  ;
            m.numhc TIMEOUT 0.001   ;
            NOREFRESH
ELSE
     CLEAR
     @ 2, 25 SAY  ;
       'No existe detalle, Revise..'
ENDIF
SELECT hoja
RETURN
*
PROCEDURE vista_det
HIDE POPUP ALL
ON KEY LABEL F9
ACTIVATE WINDOW wind_2
SELECT itehc
SEEK m.nummes + m.numhc
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            tipope :H = 'ä' :W =  ;
            .F., codcom :H =  ;
            'COMP.', codmet :H =  ;
            'META', codpart :H =  ;
            'Anlt.', aa =  ;
            val_para(RIGHT(codpart,  ;
            2),'ESPGAS','D',22, ;
            50) :H =  ;
            'Descripci¢n' : 37,  ;
            valpart :H =  ;
            '  Parcial' :P =  ;
            '99,999,999.99', xx =  ;
            IIF( .NOT.  ;
            itehc.estado = '92',  ;
            '       ', 'H/M:' +  ;
            numhm) :H = IIF(  ;
            .NOT. estado = '90',  ;
            '          ',  ;
            'Hoja Modificac') :  ;
            15 NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.nummes +  ;
            m.numhc NOREFRESH
ELSE
     CLEAR
     @ 2, 25 SAY  ;
       'No existe detalle, Revise..'
ENDIF
SELECT hoja
ON KEY LABEL F9 DO VISTA_DET
DO vista
RETURN
*
PROCEDURE total
ACTIVATE WINDOW wind_3
@ 0, 0 SAY m.imptot PICTURE  ;
  '99,999,999.99'
RETURN
*
PROCEDURE revis
PRIVATE vtemp
SELECT hoja
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SET RELATION TO nummes + numhc INTO itehc
SET SKIP TO itehc
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS nummes :H = 'Mes',  ;
       numhc :H = 'H/C ', tipdoc  ;
       :H = 'Doc', numref :H =  ;
       'N§', ess = IIF(estado =  ;
       '00', 'Pend', IIF(estado =  ;
       '20', 'C/c ', IIF(estado =  ;
       '99', 'Anul', IIF(estado =  ;
       '50', 'Aten', '    '))))  ;
       :H = 'Estd', codprv :H =  ;
       'Prv', codcad :H =  ;
       'Cod. Cadena',  ;
       itehc.codcom :H = 'COMP.',  ;
       itehc.codmet :H = 'META',  ;
       itehc.codpart :H =  ;
       'Partida', itehc.valpart  ;
       :H = 'Parcial' :P =  ;
       '9,999,999.99', imptot :H =  ;
       'TOTAL' :P =  ;
       '9,999,999.99' NOMENU  ;
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
DO vista
RETURN
*
PROCEDURE busca
PRIVATE vtemp
SELECT hoja
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnum_mes = '00'
vnum_hc = '0000'
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero H/C : '
@ 1, 23 GET vnum_mes PICTURE '!!'
@ 1, 25 SAY '.'
@ 1, 26 GET vnum_hc PICTURE  ;
  '!!!!'
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_hc) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK ALLTRIM(vnum_mes) +  ;
          vnum_hc
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
SELECT hoja
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
SELECT hoja
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
PRIVATE vtep
valcs = .T.
SELECT hoja
vtep = RECNO()
DO CASE
     CASE estado = '9'
          DO standby WITH  ;
             'La H/C ya est  anulada'
          DO vista
          RETURN
     CASE estado = '50'
          DO standby WITH  ;
             'La H/C ya est  Pagada'
          DO vista
          RETURN
     CASE estado = '51'
          DO standby WITH  ;
             'La H/C ya est  contabilizada'
          DO vista
          RETURN
ENDCASE
DO pantalla
SCATTER MEMVAR
vcodcad = m.codcad
vcodfte = m.codfte
m.partret = SPACE(5)
@ 0, 22 GET m.periodo PICTURE  ;
  '!!' DISABLE
@ 1, 22 GET m.nummes DISABLE
@ 1, 24 SAY '.'
@ 1, 25 GET m.numhc PICTURE  ;
  '!!!!' DISABLE VALID  .NOT.  ;
  EMPTY(m.numhc)
@ 1, 60 GET m.fechc VALID  .NOT.  ;
  EMPTY(m.fechc)
DO CASE
     CASE m.tipdoc = 'O/'
          @ 2, 22 SAY  ;
            val_para(m.tipdoc, ;
            'TIPDOC','V',22,20)
          @ 2, 60 GET m.codfte  ;
            PICTURE '!!' VALID  ;
            val_para(m.codfte, ;
            'CODFTE',' ',60,16, ;
            2)
          @ 3, 22 GET m.numref
          @ 3, 26 SAY '.'
          @ 3, 27 GET m.perref
     CASE m.tipdoc <> 'O/'
          @ 2, 22 GET m.tipdoc  ;
            VALID  ;
            val_para(m.tipdoc, ;
            'TIPDOC',' ',22,20)
          @ 2, 60 GET m.codfte  ;
            PICTURE '!!' VALID  ;
            val_para(m.codfte, ;
            'CODFTE',' ',60,16, ;
            2)
          @ 3, 22 GET m.desref
ENDCASE
@ 3, 60 SAY m.fecref
@ 4, 22 GET m.tipprv PICTURE  ;
  '@M P,E,O' VALID limpia()
@ 4, 24 GET m.codprv PICTURE  ;
  '!!!!' VALID val_prv1(m.codprv, ;
  '20',' ',24) WHEN m.tipprv =  ;
  'P'
@ 4, 24 GET m.codemp PICTURE  ;
  '!!!!!' VALID val_prv1(m.codemp, ;
  '30',' ',24) WHEN m.tipprv =  ;
  'E'
@ 4, 24 GET m.codotr PICTURE  ;
  '!!!!!!' VALID  ;
  val_prv1(m.codotr,'09',' ',24)  ;
  WHEN m.tipprv = 'O'
@ 5, 22 GET m.codcad PICTURE  ;
  '!!!!' VALID  ;
  val_codcad(m.codcad,m.periodo, ;
  ' ',22,40)
READ VALID ve_cad() .AND.  ;
     val_read()
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado'
     UNLOCK ALL
     CLOSE DATABASES
     DO abre
     SELECT hoja
     GOTO TOP
     GOTO vtep
     DO vista
     ON KEY LABEL F2 DO VISOBS
     RETURN
ENDIF
m.nombre = auxi.descri
USE IN 8
USE IN 9
USE IN 10
USE IN 11
IF LASTKEY() <> 27
     vcadena = maepre.uniges +  ;
               maepre.unieje +  ;
               maepre.codfun +  ;
               maepre.codprg +  ;
               maepre.codspr +  ;
               maepre.actpry
     vmesant = m.nummes
     m.nummes = vmesant
     ok = trab_hijo()
     IF ok .AND. LASTKEY() <> 27
          IF m.tipdoc = 'O/'  ;
             .AND. valcs
               vok = trab_hijo()
          ENDIF
          SELECT itehc
          SET ORDER TO ITEHC1
          SEEK ALLTRIM(m.nummes) +  ;
               m.numhc
          vtotal = 0
          SCAN WHILE nummes =  ;
               ALLTRIM(m.nummes)  ;
               .AND. numhc =  ;
               m.numhc .AND.  ;
               hoja.estado <>  ;
               '92'
               vtotal = vtotal +  ;
                        valpart
               IF valpart = 0
                    IF RLOCK()
                         DELETE NEXT  ;
                                1
                    ENDIF
                    UNLOCK
               ELSE
                    IF RLOCK()
                         REPLACE codfte  ;
                                 WITH  ;
                                 m.codfte,  ;
                                 codcad  ;
                                 WITH  ;
                                 m.codcad
                    ENDIF
                    UNLOCK
               ENDIF
          ENDSCAN
          GOTO TOP
          SELECT hoja
          m.imptot = vtotal
          vseek = m.periodo +  ;
                  m.numref +  ;
                  ALLTRIM(m.codfte)
          IF LEFT(m.tipdoc, 2) =  ;
             'O/'
               DO CASE
                    CASE ALLTRIM(m.tipdoc) =  ;
                         'O/C'
                         USE IN  ;
                             11
                         USE IN 8  ;
                             OrdCom  ;
                             ALIAS  ;
                             orden  ;
                             ORDER  ;
                             OrdCom1
                         USE IN 9  ;
                             IteOc  ;
                             ALIAS  ;
                             iteoc  ;
                             ORDER  ;
                             IteOc1
                         USE IN  ;
                             10  ;
                             Iteart  ;
                             ALIAS  ;
                             iteart  ;
                             ORDER  ;
                             Iteart3
                         SELECT orden
                         SET ORDER TO;
ORDCOM1
                    CASE ALLTRIM(m.tipdoc) =  ;
                         'O/S'
                         USE IN 8
                         USE IN 9
                         USE IN  ;
                             10
                         USE IN  ;
                             11  ;
                             Ordser  ;
                             ALIAS  ;
                             ordser  ;
                             ORDER  ;
                             OrdSer1
                         SELECT ordser
                         SET ORDER TO;
ORDSER1
               ENDCASE
               SEEK ALLTRIM(vseek)
               IF FOUND()
                    IF RLOCK()
                         REPLACE numhc  ;
                                 WITH  ;
                                 m.numhc,  ;
                                 perhc  ;
                                 WITH  ;
                                 m.nummes,  ;
                                 estado  ;
                                 WITH  ;
                                 IIF(estado =  ;
                                 '5',  ;
                                 estado,  ;
                                 '20')
                    ENDIF
                    UNLOCK
                    DO v_dest
               ELSE
                    DO standby  ;
                       WITH 'La '+ ;
                       IIF(ALLTRIM(m.tipdoc)= ;
                       'O/S',  ;
                       'O/S',  ;
                       'O/C')+ ;
                       ' no est  conectada a Abastecimentos'
               ENDIF
          ELSE
               ACTIVATE WINDOW  ;
                        wind_6
               @ 0, 0 SAY  ;
                 'Destino: ' GET  ;
                 m.destino  ;
                 PICTURE '@S73'
               READ
               DEACTIVATE WINDOW  ;
                          wind_6
          ENDIF
          USE IN 8
          USE IN 9
          USE IN 10
          USE IN 11
          DO compre
          SELECT hoja
          m.user = SYS(0)
          m.user_fc = DATE()
          m.user_tp = 'C'
          = observa()
          SELECT hoja
          GATHER MEMVAR
     ELSE
          DO standby WITH  ;
             'Proceso cancelado'
     ENDIF
ENDIF
UNLOCK ALL
DO abre
SELECT hoja
GOTO TOP
GOTO vtep
DO vista
ON KEY LABEL F2 DO VISOBS
RETURN
*
PROCEDURE ingre
PRIVATE vtempw
valcs = .T.
SELECT hoja
xs = ORDER()
vtep = RECNO()
DO pantalla
SCATTER BLANK MEMVAR
vtempw = RECNO()
m.numref = SPACE(4)
m.partret = SPACE(5)
m.periodo = RIGHT(STR(YEAR(DATE()),  ;
            4), 2)
STORE DATE() TO m.fechc, m.fecref
@ 0, 22 GET m.periodo PICTURE  ;
  '!!' VALID  .NOT.  ;
  EMPTY(m.periodo)
@ 1, 22 GET m.nummes PICTURE '!!'  ;
  VALID val_para(m.nummes, ;
  'FECMES','C',22,30) .AND.  ;
  val_mes()
@ 1, 24 SAY '.'
@ 1, 25 GET m.numhc PICTURE  ;
  '!!!!' VALID valer() .AND.   ;
  .NOT. EMPTY(m.numhc)
@ 1, 60 GET m.fechc
@ 2, 22 GET m.tipdoc PICTURE  ;
  '!!!' VALID val_para(m.tipdoc, ;
  'TIPDOC',' ',22,20)
@ 2, 60 GET m.codfte PICTURE '!!'  ;
  VALID limpia1() .AND.  ;
  val_para(m.codfte,'CODFTE',' ', ;
  60,14,3)
@ 3, 22 GET m.numref PICTURE  ;
  '!!!!' VALID valrf() .AND.  ;
  buscar() WHEN m.tipdoc = 'O/'
@ 3, 27 GET m.perref PICTURE '!!'  ;
  WHEN m.tipdoc = 'O/'
@ 3, 22 GET m.desref PICTURE  ;
  '@S15' WHEN m.tipdoc <> 'O/'
@ 3, 60 GET m.fecref
@ 4, 22 GET m.tipprv PICTURE  ;
  '@M P,E,O' VALID limpia()
@ 4, 24 GET m.codprv PICTURE  ;
  '!!!!' VALID val_prv1(m.codprv, ;
  '20',' ',24) WHEN m.tipprv =  ;
  'P'
@ 4, 24 GET m.codemp PICTURE  ;
  '!!!!!' VALID val_prv1(m.codemp, ;
  '30',' ',24) WHEN m.tipprv =  ;
  'E'
@ 4, 24 GET m.codotr PICTURE  ;
  '!!!!!!' VALID  ;
  val_prv1(m.codotr,'09',' ',24)  ;
  WHEN m.tipprv = 'O'
@ 5, 22 GET m.codcad PICTURE  ;
  '!!!!' VALID  ;
  val_codcad(m.codcad,m.periodo, ;
  ' ',22,40)
READ VALID ve_cad() .AND.  ;
     val_read()
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado'
     UNLOCK ALL
     DO borra_hc
     CLOSE DATABASES
     DO abre
     SELECT hoja
     GOTO TOP
     IF  .NOT. EOF()
          GOTO vtep
     ENDIF
     DO vista
     ON KEY LABEL F2 DO VISOBS
     RETURN
ENDIF
m.nombre = auxi.descri
IF LASTKEY() <> 27
     vcadena = maepre.uniges +  ;
               maepre.unieje +  ;
               maepre.codfun +  ;
               maepre.codprg +  ;
               maepre.codspr +  ;
               maepre.actpry
     DO CASE
          CASE m.tipdoc = 'O/S'
               USE IN 8
               USE IN 9
               USE IN 10
          CASE m.tipdoc = 'O/C'
               USE IN 12
     ENDCASE
     vmesant = m.nummes
     m.nummes = vmesant
     ok = IIF(m.tipdoc = 'O/'  ;
          .AND. valcs, escoge_o(),  ;
          trab_hijo())
     IF ok .AND. LASTKEY() <> 27
          IF m.tipdoc = 'O/'  ;
             .AND. valcs
               vok = trab_hijo()
          ENDIF
          SELECT itehc
          SEEK ALLTRIM(m.nummes) +  ;
               m.numhc
          vtotal = 0
          SCAN WHILE nummes =  ;
               ALLTRIM(m.nummes)  ;
               .AND. numhc =  ;
               m.numhc
               vtotal = vtotal +  ;
                        valpart
               IF valpart = 0
                    IF RLOCK()
                         DELETE NEXT  ;
                                1
                    ENDIF
                    UNLOCK
               ELSE
                    IF RLOCK()
                         REPLACE codfte  ;
                                 WITH  ;
                                 m.codfte,  ;
                                 codcad  ;
                                 WITH  ;
                                 m.codcad
                         SELECT itehc
                    ENDIF
                    UNLOCK
               ENDIF
          ENDSCAN
          GOTO TOP
          SELECT hoja
          m.imptot = vtotal
          vimpr = m.periodo +  ;
                  ALLTRIM(m.nummes) +  ;
                  m.numhc
          vseek = m.periodo +  ;
                  m.numref
          IF vopcion = 1
               SELECT parma
               SEEK 'HOJCON' +  ;
                    ALLTRIM(m.nummes)
               REPLACE nument  ;
                       WITH  ;
                       nument +  ;
                       1
          ENDIF
          IF LEFT(m.tipdoc, 2) =  ;
             'O/'
               DO CASE
                    CASE ALLTRIM(m.tipdoc) =  ;
                         'O/C'
                         SELECT orden
                         SET ORDER TO;
ordCOM1
                    CASE ALLTRIM(m.tipdoc) =  ;
                         'O/S'
                         USE IN  ;
                             11
                         USE IN  ;
                             11  ;
                             Ordser  ;
                             ALIAS  ;
                             ordser  ;
                             ORDER  ;
                             OrdSer1
                         SELECT ordser
               ENDCASE
               SEEK ALLTRIM(vseek)
               IF FOUND()
                    IF RLOCK()
                         REPLACE numhc  ;
                                 WITH  ;
                                 m.numhc,  ;
                                 perhc  ;
                                 WITH  ;
                                 m.nummes,  ;
                                 estado  ;
                                 WITH  ;
                                 IIF(estado =  ;
                                 '5',  ;
                                 estado,  ;
                                 '20')
                    ENDIF
                    UNLOCK
                    DO v_dest
               ELSE
                    DO standby  ;
                       WITH 'La '+ ;
                       IIF(ALLTRIM(m.tipdoc)= ;
                       'O/S',  ;
                       'O/S',  ;
                       'O/C')+ ;
                       ' no est  conectada a Abastecimentos'
               ENDIF
          ELSE
               ACTIVATE WINDOW  ;
                        wind_6
               @ 0, 0 SAY  ;
                 'Destino: ' GET  ;
                 m.destino  ;
                 PICTURE '@S73'
               READ
               DEACTIVATE WINDOW  ;
                          wind_6
          ENDIF
          USE IN 8
          USE IN 9
          USE IN 10
          USE IN 11
          DO compre
          SELECT hoja
          m.user = SYS(0)
          m.user_fc = DATE()
          m.user_tp = 'I'
          m.estado = '00'
          IF f_appd()
               GATHER MEMVAR
          ENDIF
          vtempw = RECNO()
          = observa()
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
DO abre
SELECT hoja
GOTO TOP
IF  .NOT. BOF() .AND.  .NOT.  ;
    EOF()
     GOTO vtempw
ENDIF
DO vista
ON KEY LABEL F2 DO VISOBS
RETURN
*
FUNCTION afdest
m.destino = 'OBRA ' + m.codobra +  ;
            ALLTRIM(m.tipobr) +  ;
            ' : ' +  ;
            ALLTRIM(val_para(m.tipobr, ;
            'TIPOBR','D',22,40)) +  ;
            ' - ' +  ;
            SUBSTR(obra.descri, 1,  ;
            100)
RETURN .T.
*
PROCEDURE limpia
@ 4, 22 SAY SPACE(56)
RETURN
*
PROCEDURE limpia1
vrow = ROW()
vcol = COL()
@ 3, 22 SAY SPACE(20)
@ vrow, vcol SAY ''
RETURN
*
FUNCTION escoge_o
PRIVATE vtemp
as = ALIAS()
PRIVATE vfun
vfun = .F.
vind = SYS(3) + '.IDX'
DO CASE
     CASE ALLTRIM(m.tipdoc) =  ;
          'O/C'
          USE IN 11
          USE IN 8 OrdCom ALIAS  ;
              orden ORDER  ;
              OrdCom1
          USE IN 9 IteOc1 ALIAS  ;
              iteoc ORDER  ;
              IteOc11
          USE IN 10 Iteart ALIAS  ;
              iteart ORDER  ;
              Iteart3
          SELECT iteoc
          SET ORDER TO ITEOC11
     CASE ALLTRIM(m.tipdoc) =  ;
          'O/S'
          USE IN 8
          USE IN 9
          USE IN 10
          USE IN 11 ITEOS1 ALIAS  ;
              iteos ORDER  ;
              Iteos11
          SELECT iteos
          SET FILTER TO numos = m.numref;
.AND. periodo = m.periodo;
.AND. codfte = ALLTRIM(m.codfte)
          GOTO BOTTOM
     OTHERWISE
          SELE &AS
          RETURN vfun
ENDCASE
IF EOF()
     DO standby WITH  ;
        ' Documento no en Linea '
     SET FILTER TO
     SET RELATION TO
     SELECT hoja
     vfun = trab_hijo()
     RETURN vfun
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°° ®F10¯ para confirmar      ®Esc¯ Cancela °°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
DO CASE
     CASE ALLTRIM(m.tipdoc) =  ;
          'O/C'
          BROWSE FIELDS codcom :H =  ;
                 'Comp.' :V =  ;
                 val_comp(m.periodo +  ;
                 maepre.uniges +  ;
                 maepre.unieje +  ;
                 m.codcad,codcom, ;
                 'codcom') :F,  ;
                 codmet :H =  ;
                 'Meta' :V =  ;
                 val_meta(m.periodo +  ;
                 maepre.uniges +  ;
                 maepre.unieje +  ;
                 m.codcad,codcom +  ;
                 codmet,'codmet')  ;
                 :F, codpart :H =  ;
                 'Partid' :F, aa =  ;
                 val_para(RIGHT(codpart,  ;
                 2),'ESPGAS','D', ;
                 22,40) :H =  ;
                 'Descripci¢n' :  ;
                 40 :W = .F.,  ;
                 valpart :H =  ;
                 'Total' :P =  ;
                 '999,999.99' :W =  ;
                 .F. :V =  ;
                 val_afe(valpart, ;
                 valtot,vpart) :F  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 WINDOW wind_2  ;
                 KEY m.periodo +  ;
                 m.numref
          vind = SYS(3) + '.IDX'
          SELECT iteoc
     CASE ALLTRIM(m.tipdoc) =  ;
          'O/S'
          BROWSE FIELDS codcom :H =  ;
                 'Comp.' :V =  ;
                 val_comp(m.periodo +  ;
                 maepre.uniges +  ;
                 maepre.unieje +  ;
                 m.codcad,codcom, ;
                 'codcom') :F,  ;
                 codmet :H =  ;
                 'Meta' :V =  ;
                 val_meta(m.periodo +  ;
                 maepre.uniges +  ;
                 maepre.unieje +  ;
                 m.codcad,codcom +  ;
                 codmet,'codmet')  ;
                 :F, codpart :H =  ;
                 'Partid' :F, aa =  ;
                 val_para(RIGHT(codpart,  ;
                 2),'ESPGAS','D', ;
                 22,40) :H =  ;
                 'Descripci¢n' :  ;
                 40 :W = .F.,  ;
                 valpart :H =  ;
                 'Total' :P =  ;
                 '999,999.99' :W =  ;
                 .F. :V =  ;
                 val_afe(valpart, ;
                 valtot,vpart) :F  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 WINDOW wind_2  ;
                 KEY m.periodo +  ;
                 m.numref
          vind = SYS(3) + '.IDX'
          SELECT iteos
ENDCASE
vorde = RECNO()
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
IF LASTKEY() <> 27
     DO agrega_ite
     vfun = .T.
ELSE
     vfun = .F.
ENDIF
ON KEY LABEL F10
SET FILTER TO
CLOSE INDEX
SET RELATION TO
SELECT hoja
RETURN vfun
*
FUNCTION trab_hijo
as = ALIAS()
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°F5->Agregar°°°°°F8->Eliminar°°°°°°F10->Terminar°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO agreg_item
ON KEY LABEL F8 DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itehc
SET ORDER TO ITEHC1
SEEK ALLTRIM(m.nummes) + m.numhc
SET FILTER TO estado <> '92'
IF  .NOT. FOUND()
     DO agreg_item
ELSE
ENDIF
BROWSE FIELDS tipope :H = 'ä' :V =  ;
       tipope $ '-+' :F :W = .F.,  ;
       codcom :H = 'Comp.' :V =  ;
       val_comp(m.periodo +  ;
       uniges + unieje + m.codcad, ;
       codcom,'codcom') :F,  ;
       codmet :H = 'Meta' :V =  ;
       val_meta(m.periodo +  ;
       uniges + unieje + m.codcad, ;
       codcom + codmet,'codmet')  ;
       :F, codpart :H = 'Partid'  ;
       :F, aa =  ;
       IIF(EMPTY(codpart), ' ',  ;
       val_para(RIGHT(codpart, 2), ;
       'ESPGAS','D',28,40)) :H =  ;
       'Descripci¢n' : 40 :W =  ;
       .F., valpart :H = 'Total'  ;
       :P = '99,999,999.99' :F  ;
       NOMENU NOAPPEND NODELETE  ;
       WINDOW wind_2 KEY  ;
       ALLTRIM(m.nummes) +  ;
       m.numhc
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SET FILTER TO
SELECT hoja
IF LASTKEY() = 27
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION asignp
REPLACE codpart WITH m.codpart
vpart = ordser.valtot
RETURN .T.
*
FUNCTION asigpar
REPLACE iteoc.codpart WITH  ;
        itehc.codpart
vpart = iteoc.valtot
RETURN .T.
*
FUNCTION dond
vpart = itehc.valpart
RETURN .T.
*
FUNCTION pone
m.codpart = itehc.codpart
vpart = itehc.valpart
RETURN .T.
*
FUNCTION agreg_item
SELECT itehc
vp = codpart
IF f_appd()
     REPLACE numhc WITH m.numhc,  ;
             nummes WITH m.nummes,  ;
             codpart WITH vp,  ;
             estado WITH '00',  ;
             codfte WITH m.codfte,  ;
             codcad WITH m.codcad,  ;
             tipdoc WITH m.tipdoc,  ;
             tipope WITH '*',  ;
             numpa WITH numanu,  ;
             numpr WITH numreb,  ;
             codpart WITH vcodpar,  ;
             codcom WITH vcodcom,  ;
             codmet WITH vcodmet,  ;
             uniges WITH  ;
             maepre.uniges,  ;
             unieje WITH  ;
             maepre.unieje
ENDIF
RETURN .T.
*
PROCEDURE elimi_item
SELECT itehc
IF RLOCK()
     REPLACE valpart WITH 0
     DELETE NEXT 1
ENDIF
RETURN
*
PROCEDURE borra_hc
ax = ALIAS()
SELECT itehc
SEEK ALLTRIM(m.nummes) + m.numhc
IF RLOCK() .AND. FOUND()
     SCAN WHILE nummes =  ;
          ALLTRIM(m.nummes) .AND.  ;
          numhc = m.numhc
          SELECT calen
          SEEK m.periodo +  ;
               vcadena +  ;
               itehc.codcom +  ;
               itehc.codmet +  ;
               ALLTRIM(m.codfte) +  ;
               ALLTRIM(m.nummes) +  ;
               itehc.codpart
          REPLACE totafe WITH  ;
                  totafe -  ;
                  itehc.valpart
          SELECT itehc
          DELETE NEXT 1
     ENDSCAN
ENDIF
SELECT (ax)
RETURN
*
PROCEDURE anula
PRIVATE vfun
vfun = .F.
or = .F.
SELECT hoja
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF LEFT(estado, 1) = '9'
     DO standby WITH  ;
        'La H/C ya esta anulada o con H/M'
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ANULAR esta Hoja de Control ?' ;
           )
IF velimina
     DO CASE
          CASE ALLTRIM(m.tipdoc) =  ;
               'O/C'
               USE IN 11
               USE IN 8 OrdCom  ;
                   ALIAS orden  ;
                   ORDER OrdCom1
               USE IN 9 IteOc  ;
                   ALIAS iteoc  ;
                   ORDER IteOc1
               USE IN 10 Iteart  ;
                   ALIAS iteart  ;
                   ORDER Iteart3
               SELECT orden
               SEEK m.periodo +  ;
                    m.numref +  ;
                    ALLTRIM(m.codfte)
               IF FOUND()
                    IF RLOCK()
                         REPLACE orden.estado  ;
                                 WITH  ;
                                 '00',  ;
                                 orden.perhc  ;
                                 WITH  ;
                                 '  ',  ;
                                 orden.numhc  ;
                                 WITH  ;
                                 '    '
                    ENDIF
               ELSE
                    DO standby  ;
                       WITH  ;
                       'No se pudo anular ...'
               ENDIF
          CASE ALLTRIM(m.tipdoc) =  ;
               'O/S'
               USE IN 8
               USE IN 9
               USE IN 10
               USE IN 11 Ordser  ;
                   ALIAS ordser  ;
                   ORDER OrdSer1
               SELECT ordser
               SEEK m.periodo +  ;
                    m.numref +  ;
                    ALLTRIM(m.codfte)
               IF FOUND()
                    IF RLOCK()
                         REPLACE ordser.estado  ;
                                 WITH  ;
                                 '00',  ;
                                 ordser.perhc  ;
                                 WITH  ;
                                 '  ',  ;
                                 ordser.numhc  ;
                                 WITH  ;
                                 '    '
                    ENDIF
               ELSE
                    DO standby  ;
                       WITH  ;
                       'No se pudo anular ...'
               ENDIF
     ENDCASE
     UNLOCK ALL
     SELECT hoja
     IF RLOCK()
          REPLACE hoja.estado  ;
                  WITH '99'
     ENDIF
     SELECT calen
     REPLACE totafe WITH 0 ALL
     SELECT itehc
     SEEK ALLTRIM(m.nummes) +  ;
          m.numhc
     SCAN WHILE nummes =  ;
          ALLTRIM(m.nummes) .AND.  ;
          numhc = m.numhc
          IF RLOCK()
               REPLACE itehc.estado  ;
                       WITH '99'
          ENDIF
     ENDSCAN
     GOTO TOP
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
SELECT hoja
DO vista
UNLOCK ALL
RETURN
*
PROCEDURE imprimir
PRIVATE vcon
SELECT hoja
vcon = RECNO()
SCATTER MEMVAR
vnumoc = m.nummes + m.numhc
SET RELATION TO nummes + numhc INTO itehc
SET FILTER TO nummes + numhc = vnumoc
SET SKIP TO itehc
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO reporte WITH 2, 'LisHc1',  ;
        ' Hojas de Control '
ENDIF
SET SKIP TO
SET FILTER TO
SET RELATION TO
SELECT hoja
GOTO vcon
DO vista
RETURN
*
PROCEDURE lista
SELECT hoja
PRIVATE vtemp1
vtemp1 = RECNO()
CLOSE DATABASES
USE IN 1 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 2 Itehc ALIAS itehc ORDER  ;
    Itehc1
USE IN 3 PARMAE ALIAS parma ORDER  ;
    Parmae1
USE IN 4 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 5 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 6 Calen ALIAS calen ORDER  ;
    calen1
USE IN 10 Iteart ALIAS iteart  ;
    ORDER Iteart3
USE IN 22 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 23 Personal ALIAS personal  ;
    ORDER Personal2
USE IN 12 Auxil ALIAS auxi ORDER  ;
    AUXIL1
SELECT hoja
SET RELATION TO nummes + numhc INTO itehc
SET SKIP TO itehc
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lishjc
ENDIF
SELECT hoja
SET RELATION TO
CLOSE DATABASES
DO abre
SELECT hoja
GOTO vtemp1
DO vista
RETURN
*
PROCEDURE lishjc
DEFINE WINDOW lis FROM 0, 15 TO  ;
       24, 65 FLOAT TITLE  ;
       'Listado Hojas de Control'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro, vlista, vtofue,  ;
      vtomes
vcli = SPACE(4)
vfte = SPACE(3)
vmes = SPACE(2)
vmesx = SPACE(2)
@ 01, 01 SAY  ;
  '        Total H/C : ' GET  ;
  vtocli SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,3,22)
@ 03, 01 SAY  ;
  '              H/C : '
@ 03, 22 GET vmes PICTURE '!!'  ;
  WHEN vtocli = 2
@ 03, 25 GET vcli PICTURE '!!!!'  ;
  VALID val_hc() WHEN vtocli = 2
@ 05, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,7,22) WHEN vtocli =  ;
  1
@ 07, 01 SAY  ;
  '           Fuente : '
@ 07, 22 GET vfte PICTURE '!!!'  ;
  VALID val_para(vfte,'CODFTE', ;
  'C') WHEN vtofue = 2 .AND.  ;
  vtocli = 1
@ 09, 01 SAY  ;
  '  Todos los Meses : ' GET  ;
  vtomes SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,11,22) WHEN  ;
  vtocli = 1
@ 11, 01 SAY  ;
  '              Mes : '
@ 11, 22 GET vmesx PICTURE '!!'  ;
  VALID val_para(vmesx,'FECMES', ;
  'C') WHEN vtomes = 2 .AND.  ;
  vtocli = 1
@ 13, 01 SAY  ;
  '      Listado por : ' GET  ;
  vlista FUNCTION  ;
  '^ Documento;Res£men;Res.Det.1;Res.Det.2'  ;
  WHEN vtocli = 1
@ 16, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Numero;Proveedor;Emisi¢n'  ;
  WHEN vtocli = 1
@ 19, 01 SAY  ;
  '           Estado : ' GET  ;
  vtippro FUNCTION  ;
  '^ Todos;Pendientes;Atendidos'  ;
  WHEN vtocli = 1
@ 22, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     vind = SYS(3) + '.IDX'
     INDEX ON IIF(vorden = 1,  ;
           nummes + numhc,  ;
           IIF(vorden = 2, codprv,  ;
           DTOS(fechc))) +  ;
           itehc.codpart TO  ;
           (vind) FOR IIF(vtocli =  ;
           1, .T., nummes + numhc =  ;
           ALLTRIM(vmes) + vcli)  ;
           .AND. IIF(vtippro = 1,  ;
           .T., IIF(vtippro = 2,  ;
           estado = '00', estado =  ;
           '50')) .AND.  ;
           IIF(vtofue = 1, .T.,  ;
           codfte =  ;
           ALLTRIM(vfte)) .AND.  ;
           IIF(vtomes = 1, .T.,  ;
           nummes =  ;
           ALLTRIM(vmesx))
     GOTO TOP
     DEACTIVATE WINDOW standby
     SCATTER MEMVAR
     SET RELATION TO nummes + numhc INTO;
itehc
     SET SKIP TO itehc
     vtitulo = IIF(vtippro = 1,  ;
               ' en General ',  ;
               IIF(vtippro = 2,  ;
               ' Pendientes ',  ;
               ' Atendidos '))
     IF  .NOT. EOF()
          SET MEMOWIDTH TO 34
          IF  .NOT. EMPTY(vmes)  ;
              .AND.  .NOT.  ;
              EMPTY(vcli)
               m.nummes = vmes
               m.numhc = vcli
               DO imprimir
          ELSE
               DO CASE
                    CASE vlista =  ;
                         1
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisHc1',  ;
                            ' Hojas de Control '
                    CASE vlista =  ;
                         2
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisHc3',  ;
                            ' Hojas de Control '
                    CASE vlista =  ;
                         3
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisHc2',  ;
                            ' Hojas de Control '
                    CASE vlista =  ;
                         4
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisHc4',  ;
                            ' Hojas de Control '
               ENDCASE
          ENDIF
     ELSE
          DO standby WITH vmens08
     ENDIF
     CLOSE INDEX
     ERASE (vind)
ENDIF
RETURN
*
PROCEDURE imp
PARAMETER vmes, vcli
IF  .NOT. EMPTY(vmes) .AND.   ;
    .NOT. EMPTY(vcli)
     m.nummes = vmes
     m.numhc = vcli
     DO imprimir
ENDIF
RETURN
*
PROCEDURE termi
ven_accion = .F.
ON KEY LABEL F2
ON KEY LABEL F7
ON KEY LABEL F9
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
ON KEY LABEL F2
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RELEASE WINDOW wind_2a
RELEASE WINDOW wind_2b
RELEASE WINDOW wind_3
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
SET PATH TO &pathActual
RETURN
*
FUNCTION valhc
PARAMETER vnumhc
PRIVATE vfun
vfun = .T.
vnumhc = PADL(ALLTRIM(STR(vnumhc,  ;
         4)), 4, '0')
RETURN vnumhc
*
FUNCTION valrf
m.numref = PADL(ALLTRIM(m.numref),  ;
           4, '0')
RETURN .T.
*
FUNCTION observa
valias = ALIAS()
SET MEMOWIDTH TO 34
ON KEY LABEL F10 KEYBOARD CHR(23)
IF  .NOT. WEXIST('Observa')
     DEFINE WINDOW observa FROM  ;
            03, 22 TO 20, 57  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Observaciones ±'  ;
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
MODIFY MEMO observ WINDOW observa
IF  .NOT. WVISIBLE('Observa')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado. No graba la Observaci¢n '
ENDIF
SELECT (valias)
RETURN .T.
*
FUNCTION visobs
valias = ALIAS()
IF  .NOT. WEXIST('Observa')
     DEFINE WINDOW observa FROM  ;
            03, 22 TO 20, 57  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Observaciones ±'  ;
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
MODIFY MEMO observ NOEDIT WINDOW  ;
       observa
IF  .NOT. WVISIBLE('Observa')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
RETURN .T.
*
PROCEDURE compre
ax = ALIAS()
m.valdeb = m.imptot
m.valhab = m.imptot
m.ctadeb = IIF(ALLTRIM(m.codfte) =  ;
           '00', '90101010101',  ;
           '90101010103')
m.ctahab = IIF(ALLTRIM(m.codfte) =  ;
           '00', '90301010101',  ;
           '90301010103')
SELECT cuenta
ACTIVATE WINDOW wind_5
@ 00, 08 SAY 'Cuentas '
@ 00, 18 SAY 'Debe '
@ 00, 34 SAY 'Haber '
@ 01, 04 GET m.ctadeb PICTURE  ;
  '!!!!!!!!!!!' VALID  ;
  val_fun('Cuenta','Cuenta', ;
  "LEFT(Cuenta,10)+' '+Descri", ;
  m.ctadeb,1)
@ 01, 18 GET m.valdeb PICTURE  ;
  '99,999,999.99' VALID  ;
  valedeb()
READ
@ 02, 12 GET m.ctahab PICTURE  ;
  '!!!!!!!!!!!' VALID  ;
  val_fun('Cuenta','Cuenta', ;
  "LEFT(Cuenta,10)+' '+Descri", ;
  m.ctahab,1)
@ 02, 34 GET m.valhab PICTURE  ;
  '99,999,999.99' VALID  ;
  valehab()
READ
DEACTIVATE WINDOW wind_5
RETURN
*
PROCEDURE valedeb
as = ALIAS()
SELECT astpre
SEEK 'D' + ALLTRIM(m.nummes) +  ;
     ALLTRIM(m.numhc)
IF  .NOT. FOUND()
     IF f_appd()
          REPLACE nummes WITH  ;
                  m.nummes,  ;
                  tipdoc WITH  ;
                  'H/C', numref  ;
                  WITH m.numhc,  ;
                  cuenta WITH  ;
                  m.ctadeb, tipo  ;
                  WITH 'D',  ;
                  fecref WITH  ;
                  m.fecref,  ;
                  codpart WITH  ;
                  m.codpart,  ;
                  periodo WITH  ;
                  m.periodo
     ENDIF
     UNLOCK
ENDIF
IF RLOCK()
     REPLACE ctadeb WITH m.ctadeb,  ;
             ctahab WITH  ;
             SPACE(10), valdeb  ;
             WITH m.valdeb,  ;
             valhab WITH 0,  ;
             codcad WITH m.codcad,  ;
             periodo WITH  ;
             m.periodo
ENDIF
SELECT (as)
RETURN
*
PROCEDURE valehab
as = ALIAS()
SELECT astpre
SEEK 'H' + ALLTRIM(m.nummes) +  ;
     ALLTRIM(m.numhc)
IF  .NOT. FOUND()
     IF f_appd()
          REPLACE nummes WITH  ;
                  m.nummes,  ;
                  tipdoc WITH  ;
                  'H/C', numref  ;
                  WITH m.numhc,  ;
                  cuenta WITH  ;
                  m.ctahab, tipo  ;
                  WITH 'H',  ;
                  fecref WITH  ;
                  m.fecref,  ;
                  codpart WITH  ;
                  m.codpart,  ;
                  periodo WITH  ;
                  m.periodo
     ENDIF
     UNLOCK
ENDIF
IF RLOCK()
     REPLACE ctadeb WITH  ;
             SPACE(10), ctahab  ;
             WITH m.ctahab,  ;
             valdeb WITH 0,  ;
             valhab WITH m.valhab,  ;
             codcad WITH m.codcad,  ;
             periodo WITH  ;
             m.periodo
ENDIF
SELECT (as)
RETURN
*
PROCEDURE val_hc
SELECT hoja
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SEEK vmes + vcli
IF  .NOT. FOUND()
     SET RELATION TO nummes + numhc INTO;
itehc
     SET SKIP TO itehc
     vtemp = RECNO()
     HIDE MENU mmenu
     ACTIVATE SCREEN
     vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
     ON KEY LABEL F10 KEYBOARD CHR(23)
     BROWSE FIELDS nummes :H =  ;
            'Mes', numhc :H =  ;
            'H/C ', tipdoc :H =  ;
            'Doc', numref :H =  ;
            'N§', ess =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'C/c ',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            '    ')))) :H =  ;
            'Estd', codprv :H =  ;
            'Prv', codcad :H =  ;
            'Cod. Cad.',  ;
            itehc.codpart :H =  ;
            'Partida',  ;
            itehc.valpart :H =  ;
            'Parcial', imptot :H =  ;
            'TOTAL' :P =  ;
            '99,999.99' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_0
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     IF LASTKEY() = 27
          GOTO BOTTOM
     ENDIF
ENDIF
vmes = nummes
vcli = numhc
ON KEY LABEL F10
SELECT hoja
SET RELATION TO
RETURN
*
FUNCTION valprv
PRIVATE xx, vfun
vfun = .F.
m.codprv = IIF(EMPTY(m.codprv),  ;
           m.codprv,  ;
           PADL(ALLTRIM(m.codprv),  ;
           4, '0'))
xx = val_prv(m.codprv,.T.,4,30)
IF xx
     m.codemp = '     '
     RETURN .T.
ENDIF
RETURN vfun
*
FUNCTION val_pro
PARAMETER xcod, _tipo, _x, _y
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) .AND.  ;
       _tipo
_campo = VARREAD()
malias = ALIAS()
SELECT personal
_oldwnd = WOUTPUT()
IF  .NOT. medita
     SET ORDER TO 1
     SEEK xcod
     v_fun = IIF(FOUND(), descri,  ;
             '')
ELSE
     IF EMPTY(xcod)
          SET ORDER TO 2
          ON KEY LABEL ENTER keyboard;
chr(23)
          DEFINE WINDOW _xx FROM  ;
                 3, 22 TO 22, 77
          BROWSE FIELDS codigo :H =  ;
                 'C¢digo', descri  ;
                 :H = 'Nombre'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 WINDOW _xx TITLE  ;
                 ' ®Enter¯  Selecciona   '  ;
                 NOLGRID
          ON KEY LABEL ENTER
          RELEASE WINDOW _xx
          SET ORDER TO 2
          IF  .NOT.  ;
              EMPTY(_oldwnd)
               activate window &_oldwnd
          ENDIF
          IF LASTKEY() = 27
               v_fun = .F.
          ELSE
               xcod = codigo
               IF mmsg
                    @ _x, _y SAY  ;
                      descri
               ENDIF
               SELECT (malias)
               IF  .NOT. _tipo
                    replace &_campo with;
 xcod
               ENDIF
               v_fun = .T.
          ENDIF
     ELSE
          SET ORDER TO 1
          SEEK xcod
          IF mmsg .AND. FOUND()
               @ _x, _y SAY  ;
                 descri
          ENDIF
          v_fun = FOUND()
     ENDIF
ENDIF
m.codprv = '    '
SELECT (malias)
RETURN v_fun
*
FUNCTION val_mes
IF vopcion = 1
     SELECT parma
     SET ORDER TO PARMAE1
     m.fechc = DATE()
     SEEK 'HOJCON' +  ;
          ALLTRIM(m.nummes)
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'El Correlativo del Mes no est  Inicializado'
          SELECT hoja
          RETURN .F.
     ELSE
          IF codigoaux = '00'
               DO standby WITH  ;
                  'El Calendario del Mes '+ ;
                  ALLTRIM(m.nummes)+ ;
                  ' ya est  cerrado'
               SELECT hoja
               RETURN .F.
          ELSE
               m.numhc = valhc(parma.nument +  ;
                         1)
               xx = IIF(m.nummes =  ;
                    '12', '01',  ;
                    PADL(ALLTRIM(STR((VAL(m.nummes) +  ;
                    1), 2)), 2,  ;
                    '0'))
               IF  .NOT.  ;
                   VAL(m.nummes) =  ;
                   MONTH(DATE())
                    m.fechc = IIF(VAL(m.nummes) <  ;
                              IIF(xx =  ;
                              '01',  ;
                              MONTH(DATE()) +  ;
                              12,  ;
                              MONTH(DATE())),  ;
                              CTOD( ;
                              '01/' +  ;
                              xx +  ;
                              '/' +  ;
                              ALLTRIM(STR(YEAR(m.fechc) -  ;
                              1900,  ;
                              4))) -  ;
                              1,  ;
                              DATE())
               ENDIF
               SELECT hoja
               RETURN .T.
          ENDIF
     ENDIF
ELSE
     RETURN .T.
ENDIF
*
FUNCTION xval_mes
IF vopcion = 1
     SELECT parma
     SET ORDER TO PARMAE1
     m.fechc = DATE()
     SEEK 'CORRELHOJCON'
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'El Correlativo no est  Inicializado'
          SELECT hoja
          RETURN .F.
     ELSE
          m.numhc = valhc(parma.nument +  ;
                    1)
          xx = IIF(m.nummes =  ;
               '12', '01',  ;
               PADL(ALLTRIM(STR((VAL(m.nummes) +  ;
               1), 2)), 2, '0'))
          m.fechc = IIF(VAL(m.nummes) <  ;
                    IIF(xx = '01',  ;
                    MONTH(DATE()) +  ;
                    12,  ;
                    MONTH(DATE())),  ;
                    CTOD('01/' +  ;
                    xx + '/' +  ;
                    ALLTRIM(STR(YEAR(m.fechc) -  ;
                    1900, 4))) -  ;
                    1, DATE())
          SELECT hoja
          RETURN .T.
     ENDIF
ELSE
     RETURN .T.
ENDIF
*
FUNCTION buscar
IF m.tipdoc = 'O/' .AND. m.numref <>  ;
   'B' .AND. m.numref <> 'A'
     IF ALLTRIM(m.tipdoc) = 'O/C'
          USE IN 11
          USE IN 8 OrdCom ALIAS  ;
              orden ORDER  ;
              OrdCom1
          USE IN 9 IteOc ALIAS  ;
              iteoc ORDER IteOc1
          USE IN 10 Iteart ALIAS  ;
              iteart ORDER  ;
              Iteart3
          SELECT orden
     ELSE
          USE IN 8
          USE IN 9
          USE IN 10
          USE IN 11 Ordser ALIAS  ;
              ordser ORDER  ;
              OrdSer1
          SELECT ordser
     ENDIF
     SET FILTER TO codfte = ALLTRIM(m.codfte);
.AND. estado = '00'
     GOTO TOP
     IF  .NOT. EOF()
          SEEK m.periodo +  ;
               m.numref +  ;
               ALLTRIM(m.codfte)
          IF  .NOT. FOUND()
               GOTO TOP
               DO CASE
                    CASE ALLTRIM(m.tipdoc) =  ;
                         'O/C'
                         ON KEY LABEL;
F10 KEYBOARD CHR(23)
                         SET RELATION;
TO periodo + numoc INTO iteoc
                         SET SKIP TO iteoc
                         BROWSE FIELDS  ;
                                numoc  ;
                                :H =  ;
                                'O/C',  ;
                                fecoc  ;
                                :H =  ;
                                'Fecha',  ;
                                prv =  ;
                                val_aux(codprv, ;
                                '20', ;
                                'D', ;
                                24)  ;
                                :H =  ;
                                'Raz¢n Social'  ;
                                :  ;
                                30,  ;
                                iteoc.descri  ;
                                :H =  ;
                                'Articulo '  ;
                                :  ;
                                32,  ;
                                iteoc.coduni  ;
                                :H =  ;
                                'Unid'  ;
                                :  ;
                                5,  ;
                                iteoc.canreq  ;
                                :H =  ;
                                'Cantidad'  ;
                                :P =  ;
                                '9,999,999.99',  ;
                                ess =  ;
                                IIF(estado =  ;
                                '00',  ;
                                'Pend',  ;
                                IIF(estado =  ;
                                '20',  ;
                                'C/c ',  ;
                                IIF(estado =  ;
                                '99',  ;
                                'Anul',  ;
                                IIF(estado =  ;
                                '50',  ;
                                'Aten',  ;
                                '    ' ;
                                ))))  ;
                                :H =  ;
                                'Estd'  ;
                                NOMENU  ;
                                NOAPPEND  ;
                                NOEDIT  ;
                                NODELETE  ;
                                WINDOW  ;
                                wind_7
                         vtempo =  ;
                          '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
                         IF LASTKEY() =  ;
                            27
                              RETURN  ;
                               .F.
                         ENDIF
                         m.numref =  ;
                          numoc
                         m.fecref =  ;
                          fecoc
                         ON KEY LABEL;
F10
                         SET RELATION;
TO
                    CASE ALLTRIM(m.tipdoc) =  ;
                         'O/S'
                         ON KEY LABEL;
F10 KEYBOARD CHR(23)
                         BROWSE FIELDS  ;
                                numos  ;
                                :H =  ;
                                'O/S',  ;
                                fecos  ;
                                :H =  ;
                                'Fecha',  ;
                                coddep  ;
                                :H =  ;
                                'Depend',  ;
                                codfte  ;
                                :H =  ;
                                'Fte',  ;
                                valtot  ;
                                :H =  ;
                                'Total'  ;
                                :P =  ;
                                '9,999,999.99',  ;
                                prv =  ;
                                val_aux(codprv, ;
                                '20', ;
                                'D', ;
                                24)  ;
                                :H =  ;
                                'Raz¢n Social'  ;
                                :  ;
                                30,  ;
                                est =  ;
                                IIF(estado =  ;
                                '00',  ;
                                'Pend',  ;
                                IIF(estado =  ;
                                '20',  ;
                                'S/Ct',  ;
                                IIF(estado =  ;
                                '99',  ;
                                'Anul',  ;
                                IIF(estado =  ;
                                '50',  ;
                                'Aten',  ;
                                ' -  ' ;
                                ))))  ;
                                :H =  ;
                                'Estd',  ;
                                desos  ;
                                :H =  ;
                                'Descripci¢n del Servicio'  ;
                                NOMENU  ;
                                NOAPPEND  ;
                                NOEDIT  ;
                                NODELETE  ;
                                WINDOW  ;
                                wind_7
                         vtempo =  ;
                          '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
                         IF LASTKEY() =  ;
                            27
                              RETURN  ;
                               .F.
                         ENDIF
                         ON KEY LABEL;
F10
                         m.numref =  ;
                          numos
                         m.fecref =  ;
                          fecos
               ENDCASE
          ELSE
               m.numref = IIF(ALLTRIM(m.tipdoc) =  ;
                          'O/C',  ;
                          numoc,  ;
                          numos)
               m.fecref = IIF(ALLTRIM(m.tipdoc) =  ;
                          'O/C',  ;
                          fecoc,  ;
                          fecos)
          ENDIF
          m.perref = periodo
          m.codfte = codfte
          m.codcad = codcad
          m.codpart = codpart
          m.codprv = codprv
          m.destino = destino
          vnummes = nummes
          IF ALLTRIM(m.nummes) <>  ;
             vnummes
               IF yesno( ;
                  'No coincide con Mes ' +  ;
                  vnummes +  ;
                  ' Inicial.. ')
                    SET FILTER TO
                    RETURN .F.
               ENDIF
          ENDIF
          SET FILTER TO
     ELSE
          DO standby WITH vmens08
          RETURN .F.
     ENDIF
ELSE
     valcs = .F.
     RETURN .T.
ENDIF
RETURN
*
PROCEDURE agrega_ite
DO CASE
     CASE ALLTRIM(m.tipdoc) =  ;
          'O/C'
          SET ORDER TO ITEOC13
          SEEK m.periodo +  ;
               m.numref
          IF FOUND()
               SCAN WHILE numoc =  ;
                    m.numref  ;
                    .AND. periodo =  ;
                    m.periodo
                    vpart1 = iteoc.numoc +  ;
                             iteoc.codcad +  ;
                             iteoc.codcom +  ;
                             iteoc.codmet +  ;
                             iteoc.codpart
                    vpart2 = iteoc.numoc +  ;
                             iteoc.codcad +  ;
                             iteoc.codcom +  ;
                             iteoc.codmet +  ;
                             iteoc.codpart
                    vtot = 0
                    DO WHILE  ;
                       vpart1= ;
                       vpart2
                         vtot = vtot +  ;
                                valpart
                         vcodcom =  ;
                          iteoc.codcom
                         vcodmet =  ;
                          iteoc.codmet
                         vcodpar =  ;
                          iteoc.codpart
                         SKIP
                         vpart1 =  ;
                          iteoc.numoc +  ;
                          iteoc.codcad +  ;
                          iteoc.codcom +  ;
                          iteoc.codmet +  ;
                          iteoc.codpart
                    ENDDO
                    SKIP -1
                    DO agreg_hc
                    SELECT iteoc
               ENDSCAN
          ELSE
               DO standby WITH  ;
                  'Error.'
          ENDIF
     CASE ALLTRIM(m.tipdoc) =  ;
          'O/S'
          SET ORDER TO ITEOS13
          SEEK m.periodo +  ;
               m.numref
          IF FOUND()
               SCAN WHILE numos =  ;
                    m.numref  ;
                    .AND. periodo =  ;
                    m.periodo
                    vpart1 = iteos.numos +  ;
                             iteos.codcad +  ;
                             iteos.codcom +  ;
                             iteos.codmet +  ;
                             iteos.codpart
                    vpart2 = iteos.numos +  ;
                             iteos.codcad +  ;
                             iteos.codcom +  ;
                             iteos.codmet +  ;
                             iteos.codpart
                    vtot = 0
                    DO WHILE  ;
                       vpart1= ;
                       vpart2
                         vtot = vtot +  ;
                                valpart
                         vcodcom =  ;
                          iteos.codcom
                         vcodmet =  ;
                          iteos.codmet
                         vcodpar =  ;
                          iteos.codpart
                         SKIP
                         vpart1 =  ;
                          iteos.numos +  ;
                          iteos.codcad +  ;
                          iteos.codcom +  ;
                          iteos.codmet +  ;
                          iteos.codpart
                    ENDDO
                    SKIP -1
                    DO agreg_hc
                    SELECT iteos
               ENDSCAN
          ELSE
               DO standby WITH  ;
                  'Error.'
          ENDIF
ENDCASE
SET FILTER TO
RETURN
*
FUNCTION agreg_hc
as = ALIAS()
SELECT itehc
IF f_appd()
     DO CASE
          CASE ALLTRIM(m.tipdoc) =  ;
               'O/C'
               REPLACE itehc.numhc  ;
                       WITH  ;
                       m.numhc,  ;
                       itehc.nummes  ;
                       WITH  ;
                       m.nummes,  ;
                       itehc.valpart  ;
                       WITH vtot,  ;
                       itehc.codcad  ;
                       WITH  ;
                       m.codcad,  ;
                       itehc.estado  ;
                       WITH '00',  ;
                       itehc.codfte  ;
                       WITH  ;
                       m.codfte,  ;
                       itehc.tipdoc  ;
                       WITH  ;
                       m.tipdoc,  ;
                       itehc.tipope  ;
                       WITH 'è',  ;
                       itehc.codcom  ;
                       WITH  ;
                       vcodcom,  ;
                       itehc.codmet  ;
                       WITH  ;
                       vcodmet,  ;
                       itehc.codpart  ;
                       WITH  ;
                       vcodpar,  ;
                       itehc.uniges  ;
                       WITH '01',  ;
                       itehc.unieje  ;
                       WITH  ;
                       '001'
          CASE ALLTRIM(m.tipdoc) =  ;
               'O/S'
               REPLACE itehc.numhc  ;
                       WITH  ;
                       m.numhc,  ;
                       itehc.nummes  ;
                       WITH  ;
                       m.nummes,  ;
                       itehc.valpart  ;
                       WITH vtot,  ;
                       itehc.codcad  ;
                       WITH  ;
                       m.codcad,  ;
                       itehc.estado  ;
                       WITH '00',  ;
                       itehc.codfte  ;
                       WITH  ;
                       m.codfte,  ;
                       itehc.tipdoc  ;
                       WITH  ;
                       m.tipdoc,  ;
                       itehc.tipope  ;
                       WITH 'è',  ;
                       itehc.codcom  ;
                       WITH  ;
                       vcodcom,  ;
                       itehc.codmet  ;
                       WITH  ;
                       vcodmet,  ;
                       itehc.codpart  ;
                       WITH  ;
                       vcodpar,  ;
                       itehc.uniges  ;
                       WITH '01',  ;
                       itehc.unieje  ;
                       WITH  ;
                       '001'
     ENDCASE
     SELE &AS
     RETURN .T.
ENDIF
SELE &AS
RETURN .F.
*
PROCEDURE abre
USE IN 1 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 2 Itehc ALIAS itehc ORDER  ;
    Itehc1
USE IN 3 PARMAE ALIAS parma ORDER  ;
    Parmae1
USE IN 4 maepre ALIAS maepre  ;
    ORDER maepre3
USE IN 5 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 6 Calen ALIAS calen ORDER  ;
    calen4
USE IN 15 AstPre ALIAS astpre  ;
    ORDER Astpre1
USE IN 12 Auxil ALIAS auxi ORDER  ;
    Auxil1
USE IN 14 H:\SIS97\DATA\Cuentas  ;
    ALIAS cuenta ORDER Cuentas6
RETURN
*
PROCEDURE listar
CLOSE DATABASES
USE IN 1 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 2 Itehc ALIAS itehc ORDER  ;
    Itehc1
USE IN 3 PARMAE ALIAS parma ORDER  ;
    Parmae1
USE IN 4 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 5 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 6 Calen ALIAS calen ORDER  ;
    calen4
USE IN 10 Iteart ALIAS iteart  ;
    ORDER Iteart3
USE IN 12 Auxil ALIAS auxi ORDER  ;
    Auxil1
USE IN 15 AstPre ALIAS astpre  ;
    ORDER Astpre1
USE IN 22 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 23 Personal ALIAS personal  ;
    ORDER Personal2
USE IN 14 H:\SIS97\DATA\Cuentas  ;
    ALIAS cuenta ORDER Cuentas6
SELECT hoja
SET RELATION TO nummes + numhc INTO itehc
SET SKIP TO itehc
SET MEMOWIDTH TO 34
vind = SYS(3) + '.IDX'
INDEX ON nummes + numhc TO (vind)  ;
      FOR periodo + nummes +  ;
      numhc = vimpr
SET INDEX TO (vind)
GOTO TOP
SCATTER MEMVAR
DO reporte WITH 2, 'LisHc1',  ;
   ' Hojas de Control '
CLOSE INDEX
CLOSE DATABASES
DO abre
RETURN
*
FUNCTION valer
SELECT hoja
SEEK ALLTRIM(m.nummes) + m.numhc
IF FOUND()
     DO standby WITH  ;
        'La H/C ya ha sido generada'
     RETURN .F.
ELSE
     RETURN .T.
ENDIF
*
PROCEDURE v_dest
PRIVATE az
m.destino = IIF(EMPTY(m.destino),  ;
            IIF(ALLTRIM(m.tipdoc) =  ;
            'O/C', orden.destino,  ;
            ordser.destino),  ;
            m.destino)
ACTIVATE WINDOW wind_6
@ 0, 0 SAY 'Destino: ' GET  ;
  m.destino PICTURE '@S73'
READ
DEACTIVATE WINDOW wind_6
RETURN
*
PROCEDURE pasa_it
SELECT iteoc1
SEEK m.periodo + m.numref +  ;
     ALLTRIM(m.codfte)
SCAN WHILE periodo = m.periodo  ;
     .AND. numoc = m.numref .AND.  ;
     codfte = ALLTRIM(m.codfte)
     vkf = iteoc.codart
     SELECT iteart
     SEEK vkf
     IF FOUND()
          IF RLOCK()
               REPLACE codpart  ;
                       WITH  ;
                       iteoc.codpart
          ENDIF
          UNLOCK
     ELSE
          DO standby WITH  ;
             'Error...'
     ENDIF
     SELECT iteoc
ENDSCAN
RETURN
*
FUNCTION mfecha
PARAMETER vmes
meses = 'ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE'
RETURN ALLTRIM(SUBSTR(meses,  ;
       VAL(vmes) * 9 - 8, 9))
*
PROCEDURE val_cale1
valias = ALIAS()
vord = maepre.uniges +  ;
       maepre.unieje +  ;
       maepre.codfun +  ;
       maepre.codprg +  ;
       maepre.codspr +  ;
       maepre.actpry +  ;
       maepre.codcom +  ;
       maepre.codmet
vv = val_cale(' ',m.periodo +  ;
     vord + ALLTRIM(codfte) +  ;
     ALLTRIM(m.nummes),'C')
SELECT (valias)
RETURN
*
FUNCTION ve_cad
IF LASTKEY() <> 27
     @ 6, 22 SAY  ;
       val_para(maepre.codfun, ;
       'CODFUN','V',22,40)
     @ 7, 22 SAY  ;
       val_para1(maepre.codprg, ;
       'CODPRG' + maepre.codfun, ;
       'V',22,40)
     @ 8, 22 SAY  ;
       val_para1(maepre.codspr, ;
       'CODSPR' + maepre.codprg, ;
       'V',22,40)
     @ 9, 22 SAY maepre.actpry
     @ 9, 29 SAY  ;
       val_para(maepre.actpry, ;
       'ACTPRY','D',23,40)
ENDIF
RETURN .T.
*
FUNCTION val_monto
vsaldo = vvalpart - vtotafe
IF vsaldo < itehc.valpart
     DO standby WITH  ;
        'Supera el calendario en : '+ ;
        STR(itehc.valpart- ;
        vsaldo)
     REPLACE itehc.valpart WITH 0
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION asig_techo
valias = ALIAS()
DEFINE WINDOW techo FROM 8, 20 TO  ;
       14, 60 FLOAT TITLE  ;
       'Periodo de Afectaci¢n'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW techo
SELECT hoja
GOTO BOTTOM
STORE SPACE(2) TO vperiodo,  ;
      vnummes
vperiodo = hoja.periodo
vnummes = hoja.nummes
@ 1, 2 SAY 'Periodo : ' GET  ;
  vperiodo PICTURE '!!'
@ 3, 2 SAY '    Mes : ' GET  ;
  vnummes PICTURE '!!' VALID  ;
  val_para(vnummes,'FECMES',' ', ;
  13,20)
READ VALID val_read()
RELEASE WINDOW techo
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado'
     DO termi
     DO fin_opcion
     RETURN .F.
ENDIF
= val_cale(vperiodo, ;
  ALLTRIM(vnummes))
SELECT (valias)
RETURN .T.
*
FUNCTION val_cal
PARAMETER mvariable, mfiltro
valias = ALIAS()
SELECT calen1
SEEK mfiltro + mvariable
IF FOUND()
     vvalpart = valpart
     vtotafe = totafe
ELSE
     DO standby WITH  ;
        'No existen techos...'
     vvalpart = 0
     vtotafe = 0
ENDIF
SELECT (valias)
RETURN .T.
*
PROCEDURE cruce_hco
IF codpart <> ALLTRIM(codart)  ;
   .AND. m.tipdoc = 'O/C'
     USE IN 9 IteOc1 ALIAS iteoc  ;
         ORDER IteOc11
     SELECT iteoc1
     SET ORDER TO iteoc13
     SEEK m.periodo + m.numref +  ;
          itehc.codcad +  ;
          itehc.codcom +  ;
          itehc.codmet +  ;
          itehc.codart
     IF FOUND()
          REPLACE codpart WITH  ;
                  itehc.codpart
     ENDIF
     USE
ELSE
     IF codpart <>  ;
        ALLTRIM(codart) .AND.  ;
        m.tipdoc = 'O/S'
          USE IN 9 IteOS1 ALIAS  ;
              iteos ORDER  ;
              IteOS11
          SELECT iteos1
          SET ORDER TO iteoS13
          SEEK m.periodo +  ;
               m.numref +  ;
               itehc.codcad +  ;
               itehc.codcom +  ;
               itehc.codmet +  ;
               itehc.codart
          IF FOUND()
               REPLACE codpart  ;
                       WITH  ;
                       itehc.codpart
          ENDIF
          USE
     ENDIF
ENDIF
SELECT itehc
RETURN
*
PROCEDURE buscarhoja
actual = RECNO()
DEFINE WINDOW buspr FROM  ;
       INT((SROWS() - 20) / 2),  ;
       INT((SCOLS() - 75) / 2) TO  ;
       INT((SROWS() - 20) / 2) +  ;
       19, INT((SCOLS() - 75) /  ;
       2) + 74 NOFLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       '  Busqueda de Hojas de Control  '  ;
       NOMINIMIZE DOUBLE COLOR  ;
       SCHEME 1
DO WHILE .T.
     IF WVISIBLE('buspr')
          ACTIVATE WINDOW SAME  ;
                   buspr
     ELSE
          ACTIVATE WINDOW NOSHOW  ;
                   buspr
     ENDIF
     CLEAR
     SELECT hoja
     SCATTER BLANK MEMVAR
     m.tipprv = hoja.tipprv
     m.codprv = hoja.codprv
     m.codemp = hoja.codemp
     m.codotr = hoja.codotr
     @ 1, 2 SAY 'Raz¢n Social:'  ;
       SIZE 1, 13, 0
     @ 1, 17 GET m.tipprv PICTURE  ;
       '@M P,E,O'
     @ 1, 19 GET m.codprv PICTURE  ;
       '!!!!' VALID  ;
       val_prv1(m.codprv,'20',' ', ;
       19) WHEN m.tipprv = 'P'
     @ 1, 19 GET m.codemp PICTURE  ;
       '!!!!!' VALID  ;
       val_prv1(m.codemp,'30',' ', ;
       19) WHEN m.tipprv = 'E'
     @ 1, 19 GET m.codotr PICTURE  ;
       '!!!!!!' VALID  ;
       val_prv1(m.codotr,'09',' ', ;
       19) WHEN m.tipprv = 'O'
     READ VALID val_read()
     IF LASTKEY() = 27
          GOTO actual
          EXIT
     ENDIF
     SELECT hoja
     SET ORDER TO 11
     SEEK m.tipprv + IIF(m.tipprv =  ;
          'P', ALLTRIM(m.codprv),  ;
          IIF(m.tipprv = 'E',  ;
          ALLTRIM(m.codemp),  ;
          ALLTRIM(m.codotr)))
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'El Proveedor no tiene H/C asignadas. Seleccione otro...'
          LOOP
     ENDIF
     SET FILTER TO hoja.tipprv = ALLTRIM(m.tipprv);
.AND. IIF(m.tipprv = 'P', hoja.codprv;
= ALLTRIM(m.codprv), IIF(m.tipprv = 'E',;
hoja.codemp = ALLTRIM(m.codemp), hoja.codotr;
= ALLTRIM(m.codotr)))
     DEFINE POPUP pop_hc PROMPT  ;
            FIELDS hoja.nummes +  ;
            '.' + hoja.numhc +  ;
            '³' + hoja.tipdoc +  ;
            '³' +  ;
            TRANSFORM(hoja.imptot,  ;
            '999,999,999.99')  ;
            MARK '' MARGIN  ;
            SCROLL
     @ 4,2 GET vLisHC  PICTURE "@&N";
 POPUP POP_HC  SIZE 12,34  DEFAULT " ";
 WHEN InfoDeHC()  COLOR SCHEME 2
     @ 3, 2 SAY  ;
       'Hojas de Control:' SIZE 1,  ;
       17, 0
     @ 3, 38 SAY  ;
       'Informaci¢n de la H/C:'  ;
       SIZE 1, 22, 0
     @ 4, 38 TO 12, 71
     @ 5, 43 SAY 'Tip.Doc:' SIZE  ;
       1, 8, 0
     @ 6, 39 SAY 'Descripci¢n:'  ;
       SIZE 1, 12, 0
     @ 7, 44 SAY 'Fuente:' SIZE 1,  ;
       7, 0
     @ 8, 45 SAY 'Total:' SIZE 1,  ;
       6, 0
     @ 9, 43 SAY 'Cta.Cte:' SIZE  ;
       1, 8, 0
     @ 10, 43 SAY 'Num.C/P:' SIZE  ;
       1, 8, 0
     @ 11, 44 SAY 'Estado:' SIZE  ;
       1, 7, 0
     @ 17, 20 GET okcancel  ;
       DEFAULT 1 SIZE 1, 10, 5  ;
       PICTURE  ;
       '@*HT \!Aceptar;\<Otro;\<Cancelar'
     IF  .NOT. WVISIBLE('buspr')
          ACTIVATE WINDOW buspr
     ENDIF
     READ CYCLE
     DO CASE
          CASE okcancel = 1
               SELECT hoja
               m.nummes = hoja.nummes
               m.numhc = hoja.numhc
               SET FILTER TO
               SET ORDER TO 1
               SEEK m.nummes +  ;
                    m.numhc
               EXIT
          CASE okcancel = 2
               SELECT hoja
               SET FILTER TO
               LOOP
          CASE okcancel = 3
               SELECT hoja
               SCATTER MEMVAR
               SET FILTER TO
               SET ORDER TO 1
               GOTO actual
               EXIT
     ENDCASE
ENDDO
RELEASE WINDOW buspr
RELEASE POPUP pop_hc
DO vista
RETURN
*
PROCEDURE infodehc
SELECT hoja
@ 05, 52 SAY hoja.tipdoc COLOR  ;
  '3+/1'
@ 06, 52 SAY LEFT(hoja.desref,  ;
  19) COLOR '3+/1'
@ 07, 52 SAY  ;
  LEFT(val_para(hoja.codfte, ;
  'CODFTE','D',20,20), 19) COLOR  ;
  '3+/1'
@ 08, 54 SAY  ;
  TRANSFORM(hoja.imptot,  ;
  '999,999,999.99') COLOR '3+/1'
@ 09, 52 SAY hoja.codctc COLOR  ;
  '3+/1'
@ 10, 52 SAY hoja.nummescp + '.' +  ;
  hoja.numcp COLOR '3+/1'
@ 11, 52 SAY esthc(hoja.estado)  ;
  COLOR '0/7'
RETURN
*
FUNCTION esthc
PARAMETER vest
PRIVATE vfun
vfun = SPACE(10)
DO CASE
     CASE vest = '00' .OR. vest =  ;
          '  '
          vfun = 'Emitido     '
     CASE vest = '20'
          vfun = 'Con Cta.Cte.'
     CASE vest = '52'
          vfun = 'Regularizado'
     CASE vest = '54'
          vfun = 'Con N/C     '
     CASE vest = '50'
          vfun = 'Pagado      '
     CASE vest = '70'
          vfun = 'Tiene P/A   '
     CASE vest = '80'
          vfun = 'Tiene P/R   '
     CASE vest = '90'
          vfun = 'Tiene H/M   '
     CASE vest = '99'
          vfun = 'Anulado     '
ENDCASE
RETURN vfun
*

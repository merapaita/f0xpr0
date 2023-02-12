PARAMETER vopcion, sistema
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
USE IN 12 Auxil ALIAS auxil ORDER  ;
    Auxil1
USE IN 14 Cuentas ALIAS cuenta  ;
    ORDER Cuentas6
USE IN 15 AstPre ALIAS astpre  ;
    ORDER Astpre5
USE IN 16 ITECLA ORDER ITECLA1
USE IN 0 IteUsuOp ALIAS subop  ;
    ORDER IteUsuOp1
USE IN 19 CatAsi ALIAS catasi  ;
    ORDER CatAsi4
ON KEY LABEL F4 DO IMPRIMIR
ON KEY LABEL F9 DO VISTA_DET
PUBLIC valcs, vvalpart, vtotafe,  ;
       vtotoc, vtotos, vpart,  ;
       vtomes, vmesx
PUBLIC vcodcom, vcodmet, vcodpar,  ;
       vcadena, vcodfte, vvalpart,  ;
       vtotafe, vnummes
STORE SPACE(5) TO vcodcom,  ;
      vcodmet
STORE SPACE(6) TO vcodpar
valcs = .T.
vmens01 = ' Hoja de Afectaci¢n : REVISION '
vmens02 = IIF(vopcion = 1,  ;
          ' Registro de Hoja de Afectaci¢n ',  ;
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
vmens12 = '°®F2¯ Observaciones °®F4¯ Imprime H/C °®F9¯ Detalle °®F7 Busca ¯'
SELECT hoja
GOTO BOTTOM
SCATTER BLANK MEMVAR
DO inicia
HIDE POPUP ALL
DO pantalla
DO vista
ON KEY LABEL F2 DO VISOBS
ON KEY LABEL F7 DO buscarhoja
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     ACTIVATE MENU mmenu
ENDDO
DO fin_opcion
RETURN
*
PROCEDURE cor_despue
DO trab_hijo
DO compre
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
       TO 23, 79 TITLE  ;
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
ON KEY LABEL F4 DO IMPRIMIR
ON KEY LABEL F9 DO VISTA_DET
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
@ 0, 51 SAY veresthc(m.estado)  ;
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
@ 3, 22 SAY SPACE(45)
@ 3, 22 SAY IIF((m.tipdoc = 'O/'  ;
  .OR. m.tipdoc = 'RNF'),  ;
  m.numref, SUBSTR(m.desref, 1,  ;
  30))
@ 3, 60 SAY m.fecref
@ 4, 22 SAY IIF(m.tipprv = 'O',  ;
  IIF(EMPTY(m.codotr), m.nombre,  ;
  val_aux(m.codotr,'09','D',24)),  ;
  IIF(m.tipprv = 'P',  ;
  val_aux(m.codprv,'20','D',24),  ;
  val_aux(m.codemp,'03','D', ;
  24)))
= pres_pro()
@ 5, 22 SAY m.codcad
@ 5, 30 SAY val_codcad(m.codcad, ;
  m.periodo,' ',22,40)
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
IF  .NOT. vflag $ 'J*'
     DO subopc
ENDIF
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
            'Meta', operac :H =  ;
            'Oblig.', codpart :H =  ;
            'Partid', codcla :H =  ;
            'Cla.', aa =  ;
            valasi1(ALIAS(), ;
            codpart,'8','Descri', ;
            'R') :H =  ;
            'Descripci¢n' : 30,  ;
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
            valasi1(ALIAS(), ;
            codpart,'8','Descri', ;
            'R') :H =  ;
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
          @ 3, 22 GET m.desref  ;
            PICTURE '@S15'
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
  '03',' ',24) WHEN m.tipprv =  ;
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
m.nombre = auxil.descri
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
                                 m.codfte
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
          m.user = vuser_id
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
  val_mes() .AND. valer()
@ 1, 24 SAY '.'
@ 1, 25 GET m.numhc PICTURE  ;
  '!!!!' WHEN .F.
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
  buscar() WHEN m.tipdoc = 'O/'  ;
  .OR. m.tipdoc = 'RNF'
@ 3, 27 GET m.perref PICTURE '!!'  ;
  WHEN m.tipdoc = 'O/'
@ 3, 22 GET m.desref PICTURE  ;
  '@S15' WHEN m.tipdoc <> 'O/'  ;
  .AND. m.tipdoc <> 'RNF'
@ 3, 60 GET m.fecref
@ 4, 22 GET m.tipprv PICTURE  ;
  '@M P,E,O' VALID limpia()
@ 4, 24 GET m.codprv PICTURE  ;
  '!!!!' VALID val_prv1(m.codprv, ;
  '20',' ',24) WHEN m.tipprv =  ;
  'P'
@ 4, 24 GET m.codemp PICTURE  ;
  '!!!!!' VALID val_prv1(m.codemp, ;
  '03',' ',24) WHEN m.tipprv =  ;
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
m.nummes = ALLTRIM(m.nummes)
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado'
     UNLOCK ALL
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
m.nombre = auxil.descri
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
     DO CASE
          CASE (m.tipdoc = 'O/'  ;
               .OR. m.tipdoc =  ;
               'RNF') .AND.  ;
               valcs
               ok = escoge_o()
          OTHERWISE
               ok = trab_hijo()
     ENDCASE
     IF ok .AND. LASTKEY() <> 27
          IF (m.tipdoc = 'O/'  ;
             .AND. valcs) .OR.  ;
             m.tipdoc = 'RNF'
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
                                 m.codfte
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
          DO CASE
               CASE LEFT(m.tipdoc,  ;
                    2) = 'O/'
                    DO CASE
                         CASE ALLTRIM(m.tipdoc) =  ;
                              'O/C'
                              SELECT  ;
                               orden
                              SET ORDER;
TO ordCOM1
                         CASE ALLTRIM(m.tipdoc) =  ;
                              'O/S'
                              USE  ;
                               IN  ;
                               11
                              USE  ;
                               IN  ;
                               11  ;
                               Ordser  ;
                               ALIAS  ;
                               ordser  ;
                               ORDER  ;
                               OrdSer1
                              SELECT  ;
                               ordser
                    ENDCASE
                    SEEK ALLTRIM(vseek)
                    IF FOUND()
                         IF RLOCK()
                              REPLACE  ;
                               numhc  ;
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
                              mobs =  ;
                               IIF(m.tipdoc =  ;
                               'O/S',  ;
                               detalle,  ;
                               '')
                         ENDIF
                         UNLOCK
                         DO v_dest
                    ELSE
                         DO standby  ;
                            WITH  ;
                            'La '+ ;
                            IIF(ALLTRIM(m.tipdoc)= ;
                            'O/S',  ;
                            'O/S',  ;
                            'O/C' ;
                            )+ ;
                            ' no est  conectada a Abastecimentos'
                    ENDIF
               CASE ALLTRIM(m.tipdoc) =  ;
                    'RNF'
                    SELECT fonpag
                    SEEK SUBSTR(vseek,  ;
                         3)
                    IF FOUND()
                         IF f_lock(1)
                              REPLACE  ;
                               numhc  ;
                               WITH  ;
                               m.numhc,  ;
                               nummeshc  ;
                               WITH  ;
                               m.nummes,  ;
                               estado  ;
                               WITH  ;
                               '20'
                         ENDIF
                         UNLOCK
                         DO v_dest
                    ELSE
                         DO standby  ;
                            WITH  ;
                            'La Rendici¢n no est  conectada a Tesoreria'
                    ENDIF
               OTHERWISE
                    ACTIVATE WINDOW  ;
                             wind_6
                    @ 0, 0 SAY  ;
                      'Destino: '  ;
                      GET  ;
                      m.destino  ;
                      PICTURE  ;
                      '@S73'
                    READ
                    DEACTIVATE WINDOW  ;
                               wind_6
          ENDCASE
          USE IN 8
          USE IN 9
          USE IN 10
          USE IN 11
          USE IN 17
          USE IN 18
          DO compre
          SELECT hoja
          m.user = vuser_id
          m.user_fc = DATE()
          m.user_tp = 'I'
          m.estado = '00'
          m.uniges = '01'
          m.uniges = '001'
          IF f_appd()
               REPLACE hoja.observ  ;
                       WITH  ;
                       'AFECTACION PRESUPUESTAL ' +  ;
                       IIF(ALLTRIM(m.tipdoc) =  ;
                       'O/S',  ;
                       mobs,  ;
                       'POR CONCEPTO DE :' ;
                       )
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
DO CASE
     CASE m.tipprv = 'P'
          m.codemp = SPACE(4)
          m.codotr = SPACE(4)
     CASE m.tipprv = 'E'
          m.codprv = SPACE(4)
          m.codotr = SPACE(4)
     CASE m.tipprv = 'O'
          m.codprv = SPACE(4)
          m.codemp = SPACE(4)
ENDCASE
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
ON KEY LABEL F4
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
     CASE ALLTRIM(m.tipdoc) =  ;
          'RNF'
          SELECT itefp
          SET FILTER TO LEFT(m.numref,;
4) = ALLTRIM(numfp)
          GOTO TOP
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
vtempo = '°°°°°°°° ®F10¯ para confirmar     ®Esc¯ Cancela °°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
DO CASE
     CASE ALLTRIM(m.tipdoc) =  ;
          'O/C'
          SEEK m.periodo +  ;
               m.numref
          SET RELATION TO periodo + uniges;
+ unieje + codcad INTO maepre
          BROWSE FIELDS codcad :H =  ;
                 'Cadena' :V =  ;
                 val_codcad(ALLTRIM(codcad), ;
                 periodo, ;
                 'CodCad') :F,  ;
                 codcom :H =  ;
                 'Comp.' :V =  ;
                 val_comp(periodo +  ;
                 maepre.uniges +  ;
                 maepre.unieje +  ;
                 codcad,codcom, ;
                 'codcom') :F,  ;
                 codmet :H =  ;
                 'Meta' :V =  ;
                 val_meta(periodo +  ;
                 maepre.uniges +  ;
                 maepre.unieje +  ;
                 codcad,codcom +  ;
                 ALLTRIM(codmet), ;
                 'codmet') :F,  ;
                 codpart :H =  ;
                 'Partid' :F, igv  ;
                 :H = 'IGV' :P =  ;
                 '@M S,N', aa =  ;
                 IIF(EMPTY(codpart),  ;
                 .T.,  ;
                 valasi1(ALIAS(), ;
                 codpart,'8', ;
                 'Descri','R'))  ;
                 :H =  ;
                 'Descripci¢n' :  ;
                 40 :W = .F.,  ;
                 valpart :H =  ;
                 'Total' :P =  ;
                 '999,999.99' :W =  ;
                 .F., valreb :H =  ;
                 'Tot Reb.' :P =  ;
                 '999,999.99' :W =  ;
                 .F. NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 wind_2 KEY  ;
                 m.periodo +  ;
                 m.numref
          SET RELATION TO
          vind = SYS(3) + '.IDX'
          SELECT iteoc
     CASE ALLTRIM(m.tipdoc) =  ;
          'O/S'
          SEEK m.periodo +  ;
               m.numref
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
                 'Partid' :F, igv  ;
                 :H = 'IGV' :P =  ;
                 '@M S,N', aa =  ;
                 IIF(EMPTY(codpart),  ;
                 .T.,  ;
                 valasi1(ALIAS(), ;
                 codpart,'8', ;
                 'Descri','R'))  ;
                 :H =  ;
                 'Descripci¢n' :  ;
                 40 :W = .F.,  ;
                 valpart :H =  ;
                 'Total' :P =  ;
                 '999,999.99' :W =  ;
                 .F., valreb :H =  ;
                 'Tot. Reb' :P =  ;
                 '999,999.99' :W =  ;
                 .F. NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 wind_2 KEY  ;
                 m.periodo +  ;
                 m.numref
          vind = SYS(3) + '.IDX'
          SELECT iteos
     CASE ALLTRIM(m.tipdoc) =  ;
          'RNF'
          SEEK m.numref
          BROWSE FIELDS codpart  ;
                 :H = 'Partid' :F,  ;
                 aa =  ;
                 IIF(EMPTY(codpart),  ;
                 .T.,  ;
                 valasi1(ALIAS(), ;
                 codpart,'8', ;
                 'Descri','R'))  ;
                 :H =  ;
                 'Descripci¢n' :  ;
                 40 :W = .F.,  ;
                 import :H =  ;
                 'Total' :P =  ;
                 '999,999.99' :W =  ;
                 .F., totigv :H =  ;
                 'IGV' :P =  ;
                 '99,999.99'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 WINDOW wind_2  ;
                 KEY m.numref
          GOTO TOP
          SCAN
               cpart = codpart
               ccla = codcla
               DO agreg_hc
               SCAN WHILE codpart =  ;
                    cpart .AND.  ;
                    codcla =  ;
                    ccla
                    REPLACE itehc.codpart  ;
                            WITH  ;
                            itefp.codpart
                    REPLACE itehc.codcla  ;
                            WITH  ;
                            itefp.codcla
                    REPLACE itehc.valpart  ;
                            WITH  ;
                            itehc.valpart +  ;
                            itefp.import
                    REPLACE itehc.iigv  ;
                            WITH  ;
                            itehc.iigv +  ;
                            itefp.totigv
                    SELECT itefp
               ENDSCAN
               SKIP -1
          ENDSCAN
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
ON KEY LABEL F4 DO IMPRIMIR
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
ON KEY LABEL F4
ON KEY LABEL F5 DO agreg_item
ON KEY LABEL F8 DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itehc
SET ORDER TO ITEHC1
SEEK ALLTRIM(m.nummes) + m.numhc
SET FILTER TO estado <> '92'
vcodcom = maepre.codcom
vcodmet = maepre.codmet
IF  .NOT. FOUND()
     DO agreg_item
ENDIF
BROWSE FIELDS tipope :H = 'ä' :V =  ;
       tipope $ '-+' :F :W = .F.,  ;
       codcad :H = 'Cadena' :V =  ;
       val_codcad(ALLTRIM(codcad), ;
       periodo,'CodCad') .AND.  ;
       valcym() :F, codcom :H =  ;
       'Comp.' :V =  ;
       val_comp(periodo + uniges +  ;
       unieje + codcad,codcom, ;
       'codcom') :F, codmet :H =  ;
       'Meta' :V =  ;
       val_meta(periodo + uniges +  ;
       unieje + codcad,codcom +  ;
       codmet,'codmet') :F,  ;
       operac :H = 'Oblig.',  ;
       codpart :H = 'Partid' :V =  ;
       val_cale1(codpart, ;
       m.periodo + maepre.uniges +  ;
       maepre.unieje +  ;
       maepre.codfun +  ;
       maepre.codprg +  ;
       maepre.codspr +  ;
       maepre.actpry + codcom +  ;
       codmet + ALLTRIM(codfte) +  ;
       ALLTRIM(m.nummes), ;
       'codpart') :F, codcla :H =  ;
       'Cla.' :V =  ;
       val_cla(codpart,codpart +  ;
       codcla,'codcla') :F, aa =  ;
       IIF(EMPTY(codpart), ' ',  ;
       valasi1(ALIAS(),codpart, ;
       '6','Descri','R')) :H =  ;
       'Descripci¢n' : 40 :W =  ;
       .F., valpart :H = 'Total'  ;
       :P = '99,999,999.99' :F,  ;
       igv :H = 'IGV' :P =  ;
       '@M S,N', iigv :H =  ;
       'Imp. IGV' :P =  ;
       '99,999.99' NOMENU  ;
       NOAPPEND NODELETE WINDOW  ;
       wind_2 KEY  ;
       ALLTRIM(m.nummes) +  ;
       m.numhc
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ON KEY LABEL F4 DO IMPRIMIR
SET FILTER TO
SELECT hoja
IF LASTKEY() = 27
     DO standby WITH  ;
        'Cancelado el registro de la H/C'
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION valcym
IF SEEK(m.periodo + itehc.uniges +  ;
   itehc.unieje + itehc.codcad,  ;
   'MaePre')
     REPLACE codcom WITH  ;
             maepre.codcom,  ;
             codmet WITH  ;
             maepre.codmet
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
             uniges WITH '01',  ;
             unieje WITH '001'
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
SET MEMOWIDTH TO 60
SELECT maepre
cord = ORDER()
SET ORDER TO Maepre3
SELECT itehc
SET RELATION TO codpart + codcla INTO;
itecla ADDITIVE
SELECT hoja
vcon = RECNO()
SCATTER MEMVAR
vnumoc = m.nummes + m.numhc
SET RELATION TO nummes + numhc INTO itehc;
ADDITIVE
SET FILTER TO nummes + numhc = vnumoc
SET SKIP TO itehc
SELECT itehc
SET RELATION TO hoja.periodo + codcad;
INTO maepre ADDITIVE
IF EOF()
     DO standby WITH vmens08
ELSE
     _LMARGIN = 5
     DO repprg WITH 'HojConRep',  ;
        ' HOJA DE AFECTACIàN'
ENDIF
SELECT hoja
SET SKIP TO
SET FILTER TO
SET RELATION TO
SELECT itehc
SET RELATION OFF INTO itecla
SET RELATION OFF INTO maepre
SELECT maepre
SET ORDER TO (cord)
SELECT hoja
GOTO vcon
DO vista
RETURN
*
PROCEDURE hojconrep
PARAMETER _desti
PRIVATE nreg, cnum
IF _desti = 2
     SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINTER
impri = .F.
xcolumna = SPACE(7)
GOTO TOP
@ 00, 00 SAY CHR(27) + CHR(64)
STORE 0 TO pagina, linea
nreg = RECNO()
SELECT itehc
SELECT hoja
SEEK m.nummes + m.numhc
xtot = 0
xigv = 0
SCAN WHILE hoja.numhc =  ;
     itehc.numhc
     IF pagina = 0 .OR. linea >  ;
        60
          DO titulo
          @ linea, 01 SAY  ;
            '                                            ®   C A D E N A   F U N C I O N A L   ¯  '
          linea = linea + 1
          @ linea, 00 SAY  ;
            'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
          linea = linea + 1
          @ linea, 00 SAY  ;
            '³ Cadena      UG UE FN PRG SPRG Act/PRY                          Componente                Partida           Debe       Haber            Parcial³'
          linea = linea + 1
          @ linea, 00 SAY  ;
            'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
          linea = linea + 1
     ENDIF
     DO CASE
          CASE linea < 49
               @ linea, 00 SAY  ;
                 CHR(15)
               @ linea, 05 SAY  ;
                 itehc.codcad
               @ linea, 10 SAY  ;
                 itehc.uniges
               @ linea, 13 SAY  ;
                 itehc.unieje
               @ linea, 17 SAY  ;
                 maepre.codfun
               @ linea, 20 SAY  ;
                 maepre.codprg
               @ linea, 25 SAY  ;
                 maepre.codspr
               @ linea, 30 SAY  ;
                 maepre.actpry
               @ linea, 37 SAY  ;
                 IIF( .NOT.  ;
                 EMPTY(itehc.numhm),  ;
                 'H/M: ' +  ;
                 itehc.numhm,  ;
                 '')
               @ linea, 47 SAY  ;
                 itehc.codcom
               @ linea, 53 SAY  ;
                 LEFT(val_para(itehc.codcom, ;
                 'CODCOM','D'),  ;
                 40)
               @ linea, 93 SAY  ;
                 fpartida(itehc.codpart)
               @ linea, 108 SAY  ;
                 LEFT(itecla.cuentad,  ;
                 12)
               @ linea, 121 SAY  ;
                 LEFT(itecla.cuentah,  ;
                 12)
               @ linea, 136 SAY  ;
                 itehc.valpart  ;
                 PICTURE  ;
                 '999,999.99'
               xtot = xtot +  ;
                      itehc.valpart
               xigv = xigv +  ;
                      itehc.iigv
               linea = linea + 1
          CASE linea >= 49
               @ linea, 1 SAY  ;
                 CHR(18)
               @ linea, 05 SAY  ;
                 REPLICATE('-',  ;
                 80)
               linea = linea + 1
               DO sumario
               DO titulo
               @ linea, 01 SAY  ;
                 '                                            ®   C A D E N A   F U N C I O N A L   ¯  '
               linea = linea + 1
               @ linea, 00 SAY  ;
                 'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
               linea = linea + 1
               @ linea, 00 SAY  ;
                 '³ Cadena      UG UE FN PRG SPRG Act/PRY                          Componente                        Partida       Debe     Haber    Parcial³'
               linea = linea + 1
               @ linea, 00 SAY  ;
                 'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
               SKIP -1 IN itehc
               @ linea, 01 SAY  ;
                 CHR(18)
          OTHERWISE
     ENDCASE
ENDSCAN
IF linea >= 49
     @ linea, 05 SAY REPLICATE( ;
       '-', 80)
     linea = linea + 1
     DO sumario
     DO titulo
     @ linea, 01 SAY  ;
       '                                            ®   C A D E N A   F U N C I O N A L   ¯  '
     linea = linea + 1
     @ linea, 00 SAY  ;
       'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
     linea = linea + 1
     @ linea, 00 SAY  ;
       '³ Cadena      UG UE FN PRG SPRG Act/PRY                          Componente                        Partida       Debe     Haber    Parcial³'
     linea = linea + 1
     @ linea, 00 SAY  ;
       'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
     linea = linea + 1
ENDIF
@ linea, 0 SAY CHR(15)
@ linea, 05 SAY REPLICATE('-',  ;
  141)
linea = linea + 1
@ linea, 90 SAY 'IGV : '
@ linea, 96 SAY xigv PICTURE  ;
  '@Z 9,999,999.99'
@ linea, 128 SAY xtot PICTURE  ;
  '@Z 999,999,999.99'
SELECT hoja
SEEK m.nummes + m.numhc
linea = linea + 1
@ linea, 0 SAY CHR(18)
@ linea, 04 SAY PADC( ;
  'COMPROMISO PRESUPUESTAL', 47)
linea = linea + 1
@ linea, 04 SAY PADC('CUENTAS',  ;
  10)
@ linea, 25 SAY PADC('DEBE', 13)
@ linea, 41 SAY PADC('HABER', 13)
linea = linea + 1
@ linea, 04 SAY REPLICATE('~',  ;
  47)
linea = linea + 1
SELECT astpre
SEEK hoja.nummes + hoja.numhc +  ;
     'H/C'
IF FOUND()
     IF linea >= 49
          linea = linea + 1
          DO sumario
          DO titulo
          @ linea, 0 SAY CHR(18)
          @ linea, 04 SAY  ;
            PADC( ;
            'COMPROMISO PRESUPUESTAL',  ;
            47)
          linea = linea + 1
          @ linea, 04 SAY  ;
            PADC('CUENTAS', 10)
          @ linea, 18 SAY  ;
            PADC('DEBE', 13)
          @ linea, 34 SAY  ;
            PADC('HABER', 13)
          linea = linea + 1
          @ linea, 04 SAY  ;
            REPLICATE('~', 47)
          linea = linea + 1
     ENDIF
     xdeb = 0
     xhab = 0
     SCAN WHILE nummes =  ;
          hoja.nummes .AND.  ;
          numref = hoja.numhc  ;
          .AND. tipdoc = 'H/C'
          IF linea > 60
               DO titulo
          ENDIF
          DO CASE
               CASE linea < 49
                    @ linea, 04  ;
                      SAY  ;
                      IIF(tipo =  ;
                      'D', ctadeb,  ;
                      '') PICTURE  ;
                      '!!!!!!!!!!!!'
                    @ linea, 14  ;
                      SAY  ;
                      IIF(tipo =  ;
                      'H', ctahab,  ;
                      '') PICTURE  ;
                      '!!!!!!!!!!!!'
                    @ linea, 27  ;
                      SAY  ;
                      IIF(tipo =  ;
                      'D', valdeb,  ;
                      '') PICTURE  ;
                      '99,999,999.99'
                    @ linea, 41  ;
                      SAY  ;
                      IIF(tipo =  ;
                      'H', valhab,  ;
                      '') PICTURE  ;
                      '99,999,999.99'
                    xdeb = xdeb +  ;
                           IIF(tipo =  ;
                           'D',  ;
                           valdeb,  ;
                           0)
                    xhab = xhab +  ;
                           IIF(tipo =  ;
                           'H',  ;
                           valhab,  ;
                           0)
                    linea = linea +  ;
                            1
               CASE linea >= 49
                    @ linea, 05  ;
                      SAY  ;
                      REPLICATE( ;
                      '-', 80)
                    linea = linea +  ;
                            1
                    DO sumario
                    DO titulo
                    @ linea, 0  ;
                      SAY  ;
                      CHR(18)
                    @ linea, 04  ;
                      SAY  ;
                      PADC( ;
                      'COMPROMISO PRESUPUESTAL',  ;
                      47)
                    linea = linea +  ;
                            1
                    @ linea, 04  ;
                      SAY  ;
                      PADC('CUENTAS',  ;
                      10)
                    @ linea, 18  ;
                      SAY  ;
                      PADC('DEBE',  ;
                      13)
                    @ linea, 34  ;
                      SAY  ;
                      PADC('HABER',  ;
                      13)
                    linea = linea +  ;
                            1
                    @ linea, 04  ;
                      SAY  ;
                      REPLICATE('~',  ;
                      47)
                    linea = linea +  ;
                            1
                    SKIP -1
          ENDCASE
     ENDSCAN
ENDIF
IF linea >= 49
     @ linea, 05 SAY REPLICATE( ;
       '-', 80)
     linea = linea + 1
     DO sumario
     DO titulo
     @ linea, 0 SAY CHR(18)
     @ linea, 04 SAY  ;
       PADC( ;
       'COMPROMISO PRESUPUESTAL',  ;
       47)
     linea = linea + 1
     @ linea, 04 SAY  ;
       PADC('CUENTAS', 10)
     @ linea, 18 SAY PADC('DEBE',  ;
       13)
     @ linea, 34 SAY PADC('HABER',  ;
       13)
     linea = linea + 1
     @ linea, 04 SAY  ;
       REPLICATE('~', 47)
     linea = linea + 1
ENDIF
@ linea, 05 SAY REPLICATE('-',  ;
  80)
linea = linea + 1
@ linea, 27 SAY xdeb PICTURE  ;
  '99,999,999.99'
@ linea, 41 SAY xhab PICTURE  ;
  '99,999,999.99'
linea = linea + 1
SELECT hoja
GOTO TOP
linea = linea + 1
@ linea, 00 SAY CHR(18)
linea = linea + 1
@ linea, 01 SAY  ;
  'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
linea = linea + 1
@ linea, 01 SAY '³' +  ;
  PADC('OBSERVACIONES', 79) +  ;
  '³'
linea = linea + 1
@ linea, 01 SAY  ;
  'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
linea = linea + 1
IF  .NOT. EMPTY(hoja.observ)
     FOR xx = 1 TO  ;
         MEMLINES(hoja.observ)
          @ linea, 13 SAY  ;
            MLINE(hoja.observ,  ;
            xx)
          linea = linea + 1
          IF linea >= 49
               @ linea, 1 SAY  ;
                 CHR(18)
               @ linea, 05 SAY  ;
                 REPLICATE('-',  ;
                 80)
               linea = linea + 1
               @ linea, 56 SAY  ;
                 '    V A N...   .'
               linea = linea + 2
               DO sumario
               DO titulo
               @ linea, 01 SAY  ;
                 CHR(18)
               @ linea, 52 SAY  ;
                 'V I E N E N  '
               @ linea, 79 SAY  ;
                 CHR(15)
               linea = linea + 1
          ENDIF
     ENDFOR
ENDIF
DO sumario
SELECT hoja
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE titulo
pagina = pagina + 1
vtitulo = ' PROCESO DE AFECTACIàN PRESUPUESTAL '
@ 00, 01 SAY CHR(18)
@ 00, 02 SAY ALLTRIM(cia)
@ 00, 70 SAY 'P g.'
@ 00, 75 SAY pagina PICTURE  ;
  '####'
@ 01, 01 SAY CHR(18) + CHR(14)
@ 01, 03 SAY vtitulo + CHR(27) +  ;
  'H' + CHR(18)
@ 03, 00 SAY CHR(18)
@ 03, 01 SAY '       N§ H/C : ' +  ;
  numhc + '.' + nummes
@ 03, 50 SAY '       Estado : ' +  ;
  veresthc(estado)
@ 04, 01 SAY '    Fecha H/C : ' +  ;
  DTOC(hoja.fechc)
@ 05, 01 SAY ' Raz¢n Social : ' +  ;
  IIF(m.tipprv = 'P',  ;
  val_prv1(m.codprv,'20','D',40),  ;
  IIF(m.tipprv = 'E',  ;
  val_prv1(m.codemp,'03','D',40),  ;
  val_prv1(m.codotr,'09','D', ;
  40)))
@ 06, 01 SAY '    Documento : ' +  ;
  hoja.tipdoc + ' ' +  ;
  IIF(hoja.tipdoc $  ;
  'O/C O/S P/E P/S', hoja.numref,  ;
  hoja.desref)
@ 07, 01 SAY '      Fte Fto : ' +  ;
  hoja.codfte +  ;
  val_para(hoja.codfte,'CODFTE', ;
  'D',26,50)
@ 08, 01 SAY '      Destino : ' +  ;
  CHR(15) + hoja.destino
linea = 10
RETURN
*
PROCEDURE sumario
@ 56, 05 SAY  ;
  'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ           ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ '
@ 57, 05 SAY  ;
  '          Visado                                 Autorizado'
@ 59, 60 SAY CHR(15) +  ;
  IIF(m.user_tp = 'I',  ;
  'Elaborado por: ',  ;
  IIF(m.user_tp = 'C',  ;
  'Corregido por: ', '')) +  ;
  ALLTRIM(vusua(m.user))
linea = 1
@ 60, 1 SAY CHR(12)
RETURN
*
FUNCTION vusua
PARAMETER csys
PRIVATE ali
ali = ALIAS()
vkey = ALLTRIM(csys)
USE IN 0 USUARIO ALIAS usu ORDER  ;
    USUARIO1
SELECT usu
SEEK vkey
vfun = nombre
USE IN usu
SELECT (ali)
RETURN vfun
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
USE IN 16 ITECLA ORDER ITECLA1
USE IN 19 CatAsi ALIAS catasi  ;
    ORDER CatAsi4
USE IN 12 Auxil ALIAS auxil ORDER  ;
    Auxil1
USE IN 22 Promae ALIAS promae  ;
    ORDER Promae1
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
            03, 12 TO 20, 67  ;
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
            03, 12 TO 20, 67  ;
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
PRIVATE ntot
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO AgRite
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itehc
SEEK m.nummes + m.numhc
IF FOUND()
     acadmon = ''
     DIMENSION acadmon( 1, 1)
     i = 0
     SCAN WHILE itehc.nummes +  ;
          itehc.numhc = m.nummes +  ;
          m.numhc
          i = i + 1
          DIMENSION acadmon( i,  ;
                    2)
          acadmon( i, 1) =  ;
                 itehc.codcad
          acadmon( i, 2) =  ;
                 itehc.valpart
     ENDSCAN
ENDIF
SELECT astpre
DO agrpre
BROWSE FIELDS tipo :H = 'D/H' :P =  ;
       '@M D,H', ctadeb :H =  ;
       'Cuenta' :W = tipo = 'D'  ;
       :V = val_fun('Cuenta', ;
       'Cuenta', ;
       "Cuenta+' '+DescRi",ctadeb, ;
       2) .AND. actcta(ctadeb) :F,  ;
       ctahab :H = 'Cuenta' :W =  ;
       tipo = 'H' :V =  ;
       val_fun('Cuenta','Cuenta', ;
       "Cuenta+' '+DescRi",ctahab, ;
       2) .AND. actcta(ctahab) :F,  ;
       valdeb :H = 'Monto Debe'  ;
       :W = tipo = 'D' :P =  ;
       '999,999,999.99', valhab  ;
       :H = 'Monto Haber' :W =  ;
       tipo = 'H' :P =  ;
       '999,999,999.99' WINDOW  ;
       wind_2b KEY m.nummes +  ;
       m.numhc + 'H/C'
STORE 0 TO vdebe, vhaber, vret
SEEK m.nummes + m.numhc + 'H/C'
SCAN WHILE periodo = m.periodo  ;
     .AND. nummes =  ;
     ALLTRIM(m.nummes) .AND.  ;
     numref = m.numhc .AND.  ;
     tipdoc = 'H/C'
     IF IIF(tipo = 'D', valdeb,  ;
        valhab) = 0
          DELETE NEXT 1
     ELSE
          vdebe = vdebe +  ;
                  IIF(tipo = 'D',  ;
                  valdeb, 0)
          vhaber = vhaber +  ;
                   IIF(tipo = 'H',  ;
                   valhab, 0)
     ENDIF
ENDSCAN
IF vdebe <> vhaber
     DO standby WITH  ;
        'Ojo: No cuadra debe con haber'
ENDIF
ON KEY
SELECT hoja
ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5
UNLOCK
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
RETURN
*
PROCEDURE actcta
PARAMETER xcta
REPLACE cuenta WITH xcta
REPLACE codcad WITH SUBSTR(cuenta,  ;
        5, 4)
RETURN
*
PROCEDURE agrpre
PRIVATE nfilas, i, xcadena, xord
SELECT astpre
xord = ORDER()
SET ORDER TO AstPre1
nfilas = IIF(TYPE('acadmon') <>  ;
         'U', ALEN(acadmon, 1),  ;
         0)
i = 0
IF nfilas > 0
     FOR i = 1 TO nfilas
          xcadena = acadmon(i,1)
          ximp = acadmon(i,2)
          lsalta = .F.
          m.valdeb = ximp
          m.valhab = ximp
          m.ctadeb = PADR('8301' +  ;
                     IIF(m.codfte =  ;
                     '09', '0201',  ;
                     '0402') +  ;
                     RIGHT(xcadena,  ;
                     2), 15,  ;
                     '0')
          m.ctahab = PADR('8401' +  ;
                     IIF(m.codfte =  ;
                     '09', '0201',  ;
                     '0402') +  ;
                     RIGHT(xcadena,  ;
                     2), 15,  ;
                     '0')
          SELECT astpre
          SEEK 'D' + m.nummes +  ;
               m.numhc +  ;
               m.ctadeb
          IF FOUND()
               xseek = 'D' +  ;
                       m.nummes +  ;
                       m.numhc +  ;
                       m.ctadeb
               SCAN WHILE 'D' +  ;
                    nummes +  ;
                    numhc +  ;
                    ctadeb =  ;
                    xseek
                    IF valdeb ==  ;
                       ximp
                         lsalta =  ;
                          .T.
                    ENDIF
               ENDSCAN
          ENDIF
          IF  .NOT. lsalta
               IF f_appd()
                    REPLACE periodo  ;
                            WITH  ;
                            m.periodo,  ;
                            nummes  ;
                            WITH  ;
                            m.nummes,  ;
                            numref  ;
                            WITH  ;
                            m.numhc,  ;
                            tipdoc  ;
                            WITH  ;
                            'H/C',  ;
                            codctc  ;
                            WITH  ;
                            m.codctc,  ;
                            fecref  ;
                            WITH  ;
                            m.fecref,  ;
                            codcad  ;
                            WITH  ;
                            xcadena,  ;
                            tipo  ;
                            WITH  ;
                            'D',  ;
                            ctadeb  ;
                            WITH  ;
                            m.ctadeb,  ;
                            cuenta  ;
                            WITH  ;
                            m.ctadeb,  ;
                            valdeb  ;
                            WITH  ;
                            m.valdeb
               ENDIF
               IF f_appd()
                    REPLACE periodo  ;
                            WITH  ;
                            m.periodo,  ;
                            nummes  ;
                            WITH  ;
                            m.nummes,  ;
                            numref  ;
                            WITH  ;
                            m.numhc,  ;
                            tipdoc  ;
                            WITH  ;
                            'H/C',  ;
                            codctc  ;
                            WITH  ;
                            m.codctc,  ;
                            fecref  ;
                            WITH  ;
                            m.fecref,  ;
                            codcad  ;
                            WITH  ;
                            xcadena,  ;
                            tipo  ;
                            WITH  ;
                            'H',  ;
                            ctahab  ;
                            WITH  ;
                            m.ctahab,  ;
                            cuenta  ;
                            WITH  ;
                            m.ctahab,  ;
                            valhab  ;
                            WITH  ;
                            m.valhab
               ENDIF
          ENDIF
     ENDFOR
ENDIF
SELECT astpre
SET ORDER TO (xord)
RETURN
*
PROCEDURE agrite
SELECT astpre
IF f_appd()
     REPLACE periodo WITH  ;
             m.periodo, nummes  ;
             WITH m.nummes,  ;
             numref WITH m.numhc,  ;
             tipdoc WITH 'H/C',  ;
             codctc WITH m.codctc,  ;
             fecref WITH m.fecref,  ;
             codcad WITH m.codcad,  ;
             tipo WITH 'D'
ENDIF
UNLOCK
RETURN
*
PROCEDURE eliite
SELECT astpre
IF RLOCK()
     DELETE NEXT 1
ENDIF
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
     REPLACE cuenta WITH m.ctadeb,  ;
             ctadeb WITH m.ctadeb,  ;
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
     REPLACE cuenta WITH m.ctahab,  ;
             ctadeb WITH  ;
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
               IF VAL(m.nummes) <>  ;
                  MONTH(DATE())
                    dd = PADL(ALLTRIM(STR(IIF(VAL(m.nummes) <>  ;
                         MONTH(DATE()),  ;
                         1,  ;
                         MONTH(DATE())))),  ;
                         2, '0')
                    mm = m.nummes
                    aa = STR(YEAR(DATE()),  ;
                         4)
                    m.fechc = CTOD(dd +  ;
                              '/' +  ;
                              mm +  ;
                              '/' +  ;
                              aa)
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
               m.fechc = IIF(VAL(m.nummes) <  ;
                         IIF(xx =  ;
                         '01',  ;
                         MONTH(DATE()) +  ;
                         12,  ;
                         MONTH(DATE())),  ;
                         CTOD( ;
                         '01/' +  ;
                         xx + '/' +  ;
                         ALLTRIM(STR(YEAR(m.fechc) -  ;
                         1900,  ;
                         4))) - 1,  ;
                         DATE())
               SELECT hoja
               RETURN .T.
          ENDIF
     ENDIF
ELSE
     RETURN .T.
ENDIF
*
FUNCTION buscar
DO CASE
     CASE m.tipdoc = 'O/'
          IF ALLTRIM(m.tipdoc) =  ;
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
          ELSE
               USE IN 8
               USE IN 9
               USE IN 10
               USE IN 11 Ordser  ;
                   ALIAS ordser  ;
                   ORDER OrdSer1
               SELECT ordser
          ENDIF
          SET FILTER TO codfte = ALLTRIM(m.codfte);
.AND. (estado = '00';
.OR. estado = '22')
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
                              SET SKIP;
TO iteoc
                              BROWSE  ;
                               FIELDS  ;
                               numoc  ;
                               :H =  ;
                               'O/C',  ;
                               fecoc  ;
                               :H =  ;
                               'Fecha',  ;
                               prv =  ;
                               val_auxi(codprv, ;
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
                               vestoc(iteoc.estado)  ;
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
                                   RETURN .F.
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
                              BROWSE  ;
                               FIELDS  ;
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
                               val_auxi(codprv, ;
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
                                   RETURN .F.
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
                       ' Inicial.. ' ;
                       )
                         SET FILTER TO
                         RETURN .F.
                    ENDIF
               ENDIF
               SET FILTER TO
          ELSE
               DO standby WITH  ;
                  vmens08
               RETURN .F.
          ENDIF
     CASE m.tipdoc = 'RNF'
          USE IN 17 fonpag ALIAS  ;
              fonpag ORDER  ;
              fonpag1
          USE IN 18 itefp ALIAS  ;
              itefp ORDER itefp2
          SELECT fonpag
          SET FILTER TO codfte = ALLTRIM(m.codfte);
.AND. (estado = '00')
          GOTO TOP
          IF  .NOT. EOF()
               SEEK m.numref
               IF  .NOT. FOUND()
                    GOTO TOP
                    ON KEY LABEL F10 KEYBOARD;
CHR(23)
                    BROWSE FIELDS  ;
                           numfp  ;
                           :H =  ;
                           'RNF',  ;
                           fecha  ;
                           :H =  ;
                           'Fecha',  ;
                           emp =  ;
                           val_aux(codemp, ;
                           '03', ;
                           'D', ;
                           24) :H =  ;
                           'Empleado'  ;
                           : 30,  ;
                           salini  ;
                           :H =  ;
                           'S. Inicial'  ;
                           :P =  ;
                           '9,999,999.99',  ;
                           moneje  ;
                           :H =  ;
                           'Ejecuci¢n'  ;
                           :P =  ;
                           '9,999,999.99'  ;
                           NOMENU  ;
                           NOAPPEND  ;
                           NOEDIT  ;
                           NODELETE  ;
                           WINDOW  ;
                           wind_7
                    vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
                    IF LASTKEY() =  ;
                       27
                         RETURN .F.
                    ENDIF
                    m.numref = numfp
                    m.fecref = fecha
                    ON KEY LABEL F10
               ENDIF
               m.perref = periodo
               m.codcad = fonpag.codcad
               m.tipprv = 'E'
               m.codemp = fonpag.codemp
               m.destino = persona
               vnummes = PADL(MONTH(fonpag.fecha),  ;
                         2, '0')
          ELSE
               DO standby WITH  ;
                  vmens08
               RETURN .F.
          ENDIF
          SELECT hoja
     OTHERWISE
          valcs = .F.
          RETURN .T.
ENDCASE
SHOW GET m.numref
RETURN
*
PROCEDURE agrega_ite
PRIVATE nigv
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
                                IIF(  ;
                                .NOT.  ;
                                EMPTY(valreb),  ;
                                valreb,  ;
                                valpart)
                         vcodcad =  ;
                          iteoc.codcad
                         vcodcom =  ;
                          iteoc.codcom
                         vcodmet =  ;
                          iteoc.codmet
                         vcodpar =  ;
                          iteoc.codpart
                         vigv = iteoc.igv
                         nigv = IIF(DTOC(m.fecref,  ;
                                1) >  ;
                                '20110228',  ;
                                1.18 ,  ;
                                1.19  ;
                                )
                         viigv = IIF(vigv =  ;
                                 'S',  ;
                                 ROUND(vtot -  ;
                                 (vtot /  ;
                                 nigv),  ;
                                 2),  ;
                                 0)
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
                    viigv = 0
                    DO WHILE  ;
                       vpart1= ;
                       vpart2
                         vtot = vtot +  ;
                                IIF(  ;
                                .NOT.  ;
                                EMPTY(valreb),  ;
                                valreb,  ;
                                valpart)
                         vcodcom =  ;
                          iteos.codcom
                         vcodmet =  ;
                          iteos.codmet
                         vcodpar =  ;
                          iteos.codpart
                         vigv = iteos.igv
                         nigv = IIF(DTOC(m.fecref,  ;
                                1) >  ;
                                '20110228',  ;
                                1.18 ,  ;
                                1.19  ;
                                )
                         xigv = IIF(vigv =  ;
                                'S',  ;
                                ROUND(iteos.valpart -  ;
                                (iteos.valpart /  ;
                                nigv),  ;
                                2),  ;
                                0)
                         viigv = viigv +  ;
                                 xigv
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
APPEND BLANK
DO CASE
     CASE ALLTRIM(m.tipdoc) =  ;
          'O/C'
          REPLACE itehc.numhc  ;
                  WITH m.numhc,  ;
                  itehc.nummes  ;
                  WITH m.nummes,  ;
                  itehc.valpart  ;
                  WITH vtot,  ;
                  itehc.iigv WITH  ;
                  viigv,  ;
                  itehc.codcad  ;
                  WITH vcodcad,  ;
                  itehc.estado  ;
                  WITH '00',  ;
                  itehc.codfte  ;
                  WITH m.codfte,  ;
                  itehc.tipdoc  ;
                  WITH m.tipdoc,  ;
                  itehc.tipope  ;
                  WITH 'è',  ;
                  itehc.codcom  ;
                  WITH vcodcom,  ;
                  itehc.codmet  ;
                  WITH vcodmet,  ;
                  itehc.codpart  ;
                  WITH vcodpar,  ;
                  itehc.igv WITH  ;
                  vigv,  ;
                  itehc.uniges  ;
                  WITH '01',  ;
                  itehc.unieje  ;
                  WITH '001'
     CASE ALLTRIM(m.tipdoc) =  ;
          'O/S'
          REPLACE itehc.numhc  ;
                  WITH m.numhc,  ;
                  itehc.nummes  ;
                  WITH m.nummes,  ;
                  itehc.valpart  ;
                  WITH vtot,  ;
                  itehc.iigv WITH  ;
                  viigv,  ;
                  itehc.codcad  ;
                  WITH m.codcad,  ;
                  itehc.estado  ;
                  WITH '00',  ;
                  itehc.codfte  ;
                  WITH m.codfte,  ;
                  itehc.tipdoc  ;
                  WITH m.tipdoc,  ;
                  itehc.tipope  ;
                  WITH 'è',  ;
                  itehc.codcom  ;
                  WITH vcodcom,  ;
                  itehc.codmet  ;
                  WITH vcodmet,  ;
                  itehc.codpart  ;
                  WITH vcodpar,  ;
                  itehc.igv WITH  ;
                  vigv,  ;
                  itehc.uniges  ;
                  WITH '01',  ;
                  itehc.unieje  ;
                  WITH '001'
     CASE ALLTRIM(m.tipdoc) =  ;
          'RNF'
          REPLACE itehc.numhc  ;
                  WITH m.numhc,  ;
                  itehc.nummes  ;
                  WITH m.nummes,  ;
                  itehc.codcad  ;
                  WITH m.codcad,  ;
                  itehc.estado  ;
                  WITH '00',  ;
                  itehc.codfte  ;
                  WITH m.codfte,  ;
                  itehc.tipdoc  ;
                  WITH m.tipdoc,  ;
                  itehc.tipope  ;
                  WITH '*',  ;
                  itehc.codcom  ;
                  WITH  ;
                  maepre.codcom,  ;
                  itehc.codmet  ;
                  WITH  ;
                  maepre.codmet,  ;
                  itehc.codpart  ;
                  WITH vcodpar,  ;
                  itehc.uniges  ;
                  WITH '01',  ;
                  itehc.unieje  ;
                  WITH '001'
ENDCASE
SELE &AS
RETURN .T.
*
PROCEDURE abre
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
USE IN 12 Auxil ALIAS auxil ORDER  ;
    Auxil1
USE IN 14 Cuentas ALIAS cuenta  ;
    ORDER Cuentas6
USE IN 15 AstPre ALIAS astpre  ;
    ORDER Astpre5
USE IN 22 PROMAE ALIAS promae  ;
    ORDER Promae1
USE IN 16 ITECLA ORDER ITECLA1
USE IN 19 CatAsi ALIAS catasi  ;
    ORDER CatAsi4
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
USE IN 12 Auxil ALIAS auxil ORDER  ;
    Auxil1
USE IN 14 Cuentas ALIAS cuenta  ;
    ORDER Cuentas6
USE IN 15 AstPre ALIAS astpre  ;
    ORDER Astpre5
USE IN 16 ITECLA ORDER ITECLA1
USE IN 19 CatAsi ALIAS catasi  ;
    ORDER CatAsi4
USE IN 22 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 23 Personal ALIAS personal  ;
    ORDER Personal2
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
     SHOW GET m.numhc
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
PROCEDURE pres_pro
DO CASE
     CASE m.tipprv = 'E'
          @ 4, 22 SAY  ;
            val_auxi(ALLTRIM(m.codemp), ;
            '03','V')
     CASE m.tipprv = 'P'
          @ 4, 22 SAY  ;
            val_auxi(ALLTRIM(m.codprv), ;
            '20','V')
     CASE m.tipprv = 'O' .AND.   ;
          .NOT. EMPTY(m.codotr)
          @ 4, 22 SAY  ;
            val_auxi(ALLTRIM(m.codotr), ;
            '09','V')
     CASE m.tipprv = 'I'
          @ 4, 22 SAY  ;
            val_auxi(ALLTRIM(m.codpre), ;
            '80','V')
     CASE m.tipprv = 'R' .AND.   ;
          .NOT. EMPTY(m.codotr)
          @ 4, 22 SAY  ;
            val_para(m.codotr, ;
            'CODRET','V',22,40)
     OTHERWISE
          @ 4, 22 SAY m.nombre
ENDCASE
RETURN
*
PROCEDURE subopc
PRIVATE calias, cmod
calias = ALIAS()
SELECT subop
cctrlop = ''
cmod = '01'
SEEK vusucla + PADL(sistctrl, 2,  ;
     '0') + cmod
IF FOUND()
     SCAN WHILE vusucla +  ;
          PADL(sistctrl, 2, '0') +  ;
          cmod =  ;
          ALLTRIM(subop.user) +  ;
          subop.sistema +  ;
          subop.modulo
          cctrlop = cctrlop +  ;
                    subop.opcion
     ENDSCAN
ENDIF
SET SKIP OF PAD revis OF mmenu;
 .NOT. 'A' $ cctrlop
SET SKIP OF PAD busca OF mmenu;
 .NOT. 'B' $ cctrlop
SET SKIP OF PAD anter OF mmenu;
 .NOT. 'C' $ cctrlop
SET SKIP OF PAD proxi OF mmenu;
 .NOT. 'D' $ cctrlop
SET SKIP OF PAD corri OF mmenu;
 .NOT. 'E' $ cctrlop
SET SKIP OF PAD ingre OF mmenu;
 .NOT. 'F' $ cctrlop
SET SKIP OF PAD anula OF mmenu;
 .NOT. 'G' $ cctrlop
SET SKIP OF PAD lista OF mmenu;
 .NOT. 'H' $ cctrlop
SELECT (calias)
RETURN
*

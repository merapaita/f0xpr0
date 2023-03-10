PARAMETER vsistema
USE IN 1 hojmod ALIAS hojmod  ;
    ORDER hojmod1
USE IN 3 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 4 Itehc ALIAS itehc ORDER  ;
    Itehc1
USE IN 5 PARMAE ALIAS parma ORDER  ;
    Parmae1
USE IN 6 maepre ALIAS maepre  ;
    ORDER maepre3
USE IN 7 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 8 Calen ALIAS calen ORDER  ;
    calen1
USE IN 10 Auxil ALIAS auxil ORDER  ;
    Auxil1
USE IN 12 Cuentas ALIAS cuenta  ;
    ORDER Cuentas6
USE IN 16 ITECLA ORDER ITECLA1
PUBLIC valcs, vcodprg, vcodsub,  ;
       vproyec, vcodact, vsubpry,  ;
       vtotahc, vresto, vcadena,  ;
       vperiodo
sistema = vsistema
vperiod = '  '
valcs = .T.
vmens01 = ' Hoja de Modificación : REVISION '
vmens02 = ' Registro de Hoja de Modificación '
vmens04 = 'Dicho Hoja de Modificación no fue encontrado'
vmens05 = 'No existe Hoja de Modificación anterior'
vmens06 = 'No existe Hoja de Modificación siguiente'
vmens07 = 'Ę Desea ANULAR éste Hoja de Modificación ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Hoja de Modificación ha sido anulada'
vmens10 = 'Este Hoja de Modificación ya fue atendida'
vmens11 = 'Este Hoja de Modificación ha sido devuelto'
vmens12 = '░   «F2» Justificación   ░ '
SELECT itehc
SET RELATION TO hoja.nummes + hoja.numhc;
INTO hoja
SET RELATION TO hoja.periodo + itehc.codcad;
+ itehc.codcom INTO maepre
SELECT hojmod
GOTO BOTTOM
SCATTER BLANK MEMVAR
ON KEY LABEL F2 DO VISOBS
ON KEY LABEL F7 DO vis_ap
ON KEY LABEL F4 DO imp_hm
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 14, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1h FROM 00, 00  ;
       TO 14, 79 TITLE  ;
       'Registro Hoja Modificacion'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 15, 00  ;
       TO 23, 79 TITLE  ;
       'Estadística Diaria por Objeto del Gasto'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2a FROM 13, 41  ;
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
DEFINE WINDOW wind_5 FROM 07, 08  ;
       TO 17, 72 TITLE  ;
       ' COMPROMISO PRESUPUESTAL    [«F5»Agrega  «F8»Borra]'  ;
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
@ 1, 2 SAY '       Número H/M :'
@ 1, 40 SAY '        Fecha H/M :'
@ 2, 2 SAY '       Incidencia :'
@ 2, 40 SAY '        Operación :'
@ 4, 2 SAY '       Número H/C :'
@ 4, 26 SAY '.'
@ 4, 40 SAY '        Fecha H/C :'
@ 5, 2 SAY '        Proveedor :'
@ 6, 2 SAY 'Corr. cadena Fun. :'
@ 7, 2 SAY '          Función :'
@ 8, 2 SAY '         Programa :'
@ 9, 2 SAY '      SubPrograma :'
@ 10, 2 SAY '  Activ./Proyecto :'
@ 11, 2 SAY '         Fte.Fto. :'
@ 12, 0 SAY PADC( ;
  '▓▒  «F2» Observaciones   «F4» Imprime   «F7» Asiento   «F9» Item  ▒▓',  ;
  77, ' ') COLOR '7+/1'
RETURN
*
PROCEDURE vista
SELECT hojmod
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
ON KEY LABEL F9 DO vista_det
SCATTER MEMVAR
vperiodo = m.periodo
= val_cadhm(ALLTRIM(m.codcad), ;
  m.periodo,'C')
@ 0, 60 SAY veresthm(m.estado)  ;
  COLOR W+/B 
@ 1, 22 SAY m.nummes
@ 1, 24 SAY '.'
@ 1, 25 SAY m.numhm
@ 1, 60 SAY m.fechm
@ 2, 22 SAY val_para(m.tiphm, ;
  'HOJMOD','D',22,18)
@ 2, 60 SAY val_para(m.operac, ;
  'OPERAC','D',22,18)
@ 4, 22 SAY m.nummeshc
@ 4, 24 SAY '.'
@ 4, 25 SAY m.numhc
@ 4, 60 SAY m.fechc
@ 5, 22 SAY SPACE(60)
= pres_pro()
@ 6, 22 SAY m.codcad
= ve_cad()
@ 11, 22 SAY val_para(m.codfte, ;
  'CODFTE','V',43)
DO vista_hijo
RETURN
*
PROCEDURE vista_hijo
HIDE POPUP ALL
ACTIVATE WINDOW wind_2
SELECT maepre
SET ORDER TO MAEPRE3
SELECT itehc
SET RELATION TO vperiodo + itehc.codcad;
+ itehc.codcom INTO maepre
GOTO TOP
SEEK m.nummeshc + m.numhc
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            tipope :H = 'ń' :V =  ;
            tipope $ '-+' :W =   ;
            .NOT. itehc.tipope $  ;
            'Ŕ*', codfte :H =  ;
            'Fte' :P = '!!' :W =   ;
            .NOT. itehc.tipope $  ;
            'Ŕ*', codcad :H =  ;
            'Cadena',  ;
            maepre.codfun :H =  ;
            'Fn' :W = .F.,  ;
            maepre.codprg :H =  ;
            'Prg' :W = .F.,  ;
            maepre.codspr :H =  ;
            'Spr' :W = .F.,  ;
            maepre.actpry :H =  ;
            'ActPry' :W = .F.,  ;
            codcom :H = 'Comp.'  ;
            :W =  .NOT.  ;
            itehc.tipope $ 'Ŕ*',  ;
            codmet :H = 'Meta' :W =   ;
            .NOT. itehc.tipope $  ;
            'Ŕ*', codpart :H =  ;
            'Partid' :W =  .NOT.  ;
            itehc.tipope $ 'Ŕ*',  ;
            codcla :H = 'Cla.',  ;
            valpart :H = 'Total'  ;
            :P = '99,999,999.99'  ;
            :W =  .NOT.  ;
            itehc.tipope $ 'Ŕ*',  ;
            xx = IIF( .NOT.  ;
            itehc.estado = '92',  ;
            '       ', 'H/M:' +  ;
            numhm) :H = IIF(  ;
            .NOT. estado = '90',  ;
            '          ',  ;
            'Hoja Modificac') :W =  ;
            .F. NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.nummeshc +  ;
            m.numhc TIMEOUT 0.001   ;
            NOREFRESH
ELSE
     CLEAR
     @ 2, 25 SAY  ;
       'No existe detalle, Revise..'
ENDIF
SELECT hojmod
RETURN
*
PROCEDURE vista_det
HIDE POPUP ALL
ACTIVATE WINDOW wind_2
ON KEY LABEL F9
SELECT maepre
SET ORDER TO MAEPRE3
SELECT itehc
SET RELATION TO hoja.periodo + itehc.codcad;
+ itehc.codcom INTO maepre
SELECT itehc
GOTO TOP
SEEK m.nummeshc + m.numhc
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            tipope :H = 'ń' :V =  ;
            tipope $ '-+' :W =   ;
            .NOT. itehc.tipope $  ;
            'Ŕ*', codfte :H =  ;
            'Fte' :V =  ;
            val_para(codfte, ;
            'CODFTE','codfte') :F  ;
            :P = '!!' :W =  .NOT.  ;
            itehc.tipope $ 'Ŕ*',  ;
            codcad :H = 'Cadena'  ;
            :F, maepre.codfun :H =  ;
            'Fn' :W = .F.,  ;
            maepre.codprg :H =  ;
            'Prg' :W = .F.,  ;
            maepre.codspr :H =  ;
            'SPrg' :W = .F.,  ;
            maepre.actpry :H =  ;
            'ActPry' :W = .F.,  ;
            codcom :H = 'Comp.'  ;
            :F :W =  .NOT.  ;
            itehc.tipope $ 'Ŕ*',  ;
            codmet :H = 'Meta' :F  ;
            :W =  .NOT.  ;
            itehc.tipope $ 'Ŕ*',  ;
            codpart :H = 'Partid'  ;
            :W =  .NOT.  ;
            itehc.tipope $ 'Ŕ*',  ;
            valpart :H = 'Total'  ;
            :P = '99,999,999.99'  ;
            :W =  .NOT.  ;
            itehc.tipope $ 'Ŕ*',  ;
            xx = IIF( .NOT.  ;
            itehc.estado = '92',  ;
            '       ', 'H/M:' +  ;
            numhm) :H = IIF(  ;
            .NOT. estado = '90',  ;
            '          ',  ;
            'Hoja Modificac') :W =  ;
            .F. NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.nummeshc +  ;
            m.numhc NOREFRESH
ELSE
     CLEAR
     @ 2, 25 SAY  ;
       'No existe detalle, Revise..'
ENDIF
SELECT hojmod
ON KEY LABEL F9 DO vista_det
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
SELECT hojmod
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SET RELATION TO nummeshc + numhc INTO;
itehc
SET SKIP TO itehc
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '░░░░░░░░░░░Presione «F10» para seleccionar  o  «Esc» para cancelar░░░░░░░░░░░░'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS nummes :H = 'Mes',  ;
       numhm :H = 'H/M ',  ;
       nummeshc :H = 'MHc', numhc  ;
       :H = 'H/C', codprv :H =  ;
       'Prv', codcad :H =  ;
       'Cod. Cadena',  ;
       itehc.codpart :H =  ;
       'Partida', itehc.valpart  ;
       :H = 'Parcial' :P =  ;
       '9,999,999.99', imptot :H =  ;
       'TOTAL' :P =  ;
       '9,999,999.99' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_0
vtempo = '░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SET RELATION TO
SELECT hojmod
DO vista
RETURN
*
PROCEDURE busca
PRIVATE vtemp
SELECT hojmod
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
  'Ingrese Número H/M : '
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
FUNCTION vbusca
vnum_oc = PADL(ALLTRIM(STR(vnum_oc,  ;
          4)), 4, '0')
RETURN .T.
*
PROCEDURE anter
SELECT hojmod
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
SELECT hojmod
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
PRIVATE vtem
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtem = RECNO()
PUBLIC vcodprg, vcodsub, vproyec,  ;
       vcodact, vsubpry, vcodprg,  ;
       vcodsub, vproyec, vcodact,  ;
       vsubpry, vtotalh, vtotsig
SELECT hojmod
SCATTER MEMVAR
ACTIVATE WINDOW wind_1
@ 2, 22 GET m.tiphm PICTURE '!!!'  ;
  VALID val_para(m.tiphm,'HOJMOD', ;
  'C',22,10,4)
@ 2, 60 GET m.operac PICTURE '!'  ;
  VALID val_para(m.operac, ;
  'OPERAC','C',60,10,3)
READ VALID val_read()
IF LASTKEY() <> 27
     vantpart = m.codpart
     ok = trab_hijo()
     IF ok .AND. LASTKEY() <> 27
          SELECT itehc
          SET FILTER TO estado = '92'
          GOTO TOP
          SEEK ALLTRIM(m.nummeshc) +  ;
               m.numhc
          vtotal = 0
          SCAN WHILE nummes =  ;
               ALLTRIM(m.nummeshc)  ;
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
                         IF nummeshm =  ;
                            ALLTRIM(m.nummes)  ;
                            .AND.  ;
                            numhm =  ;
                            ALLTRIM(m.numhm)
                              REPLACE  ;
                               operac  ;
                               WITH  ;
                               m.operac,  ;
                               tipcom  ;
                               WITH  ;
                               m.tiphm
                         ENDIF
                    ENDIF
                    UNLOCK
               ENDIF
          ENDSCAN
          SET FILTER TO
          GOTO TOP
          SELECT hojmod
          m.imptot = vtotal
          m.codpart = vantpart
          vimpr = m.periodo +  ;
                  ALLTRIM(m.nummes) +  ;
                  m.numhm
          vhm = ALLTRIM(m.nummeshc) +  ;
                m.numhc
          USE IN 8
          USE IN 9
          DO compre1
          SELECT hojmod
          GATHER MEMVAR
          = observa()
          SELECT hoja
          SEEK vhm
          IF FOUND()
               IF RLOCK()
                    REPLACE numhm  ;
                            WITH  ;
                            hojmod.numhm,  ;
                            fechm  ;
                            WITH  ;
                            hojmod.fechm
               ENDIF
               UNLOCK
          ELSE
               DO standby WITH  ;
                  'error!'
          ENDIF
     ENDIF
     SELECT hojmod
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
     SELECT hojmod
ENDIF
CLOSE DATABASES
DO abre
SELECT hojmod
GOTO vtem
DO vista
RETURN
*
PROCEDURE ingre
PRIVATE vtemp1
vtemp1 = RECNO()
PUBLIC vcodprg, vcodsub, vproyec,  ;
       vcodact, vsubpry, vcodprg,  ;
       vcodsub, vproyec, vcodact,  ;
       vsubpry, vtotalh, vtotsig
DO pantalla
SELECT hojmod
SCATTER BLANK MEMVAR
m.periodo = RIGHT(ALLTRIM(STR(YEAR(DATE()))),  ;
            2)
m.fechm = DATE()
@ 4, 22 GET m.nummeshc
@ 4, 24 SAY '.'
@ 4, 25 GET m.numhc VALID v_hc()
@ 1, 22 GET m.nummes PICTURE '!!'  ;
  VALID val_para(m.nummes, ;
  'FECMES','C',22,30) .AND.  ;
  vale_mes() .AND. vcorhm()
@ 1, 24 SAY '.'
@ 1, 25 GET m.numhm PICTURE  ;
  '!!!!' VALID valer() .AND.   ;
  .NOT. EMPTY(m.numhm) WHEN .F.
@ 1, 60 GET m.fechm
@ 2, 22 GET m.tiphm PICTURE '!!!'  ;
  VALID val_para(m.tiphm,'HOJMOD', ;
  ' ',22,10,4) .AND. asig_fec()
@ 2, 60 GET m.operac PICTURE '!'  ;
  VALID val_para(m.operac, ;
  'OPERAC',' ',60,10,3)
@ 4, 60 GET m.fechc
@ 5, 22 GET m.tipprv PICTURE  ;
  '@M P,E,O'
@ 5, 24 GET m.codprv PICTURE  ;
  '!!!!' VALID val_prv1(m.codprv, ;
  '20',' ',24) WHEN m.tipprv =  ;
  'P'
@ 5, 24 GET m.codemp PICTURE  ;
  '!!!!!' VALID val_prv1(m.codemp, ;
  '03',' ',24) WHEN m.tipprv =  ;
  'E'
@ 5, 24 GET m.codotr PICTURE  ;
  '!!!!!!' VALID  ;
  val_prv1(m.codotr,'09',' ',24)  ;
  WHEN m.tipprv = 'O'
@ 6, 22 GET m.codcad PICTURE  ;
  '!!!!' VALID val_cadhm(m.codcad, ;
  m.periodo,' ',22,40) .AND.  ;
  ve_cad()
@ 11, 22 GET m.codfte PICTURE  ;
  '!!' VALID val_para(m.codfte, ;
  'CODFTE',' ',22,25,4)
READ VALID val_read()
vcadena = maepre.uniges +  ;
          maepre.unieje +  ;
          maepre.codfun +  ;
          maepre.codprg +  ;
          maepre.codspr +  ;
          maepre.actpry
IF LASTKEY() <> 27
     vantpart = m.codpart
     ok = trab_hijo()
     IF ok .AND. LASTKEY() <> 27
          USE IN 0 COMPAG ALIAS  ;
              compag ORDER  ;
              COMPAG4
          SELECT compag
          SEEK ALLTRIM(m.nummeshc) +  ;
               m.numhc
          IF FOUND()
               m.numcp = compag.numcp
               m.nummescp = compag.nummes
          ENDIF
          USE
          SELECT itehc
          SET FILTER TO estado = '92'
          SEEK ALLTRIM(m.nummeshc) +  ;
               m.numhc
          vtotal = 0
          vtotalh = 0
          vtotsig = 0
          SCAN FOR nummes =  ;
               ALLTRIM(m.nummeshc)  ;
               .AND. numhc =  ;
               m.numhc
               vtotal = vtotal +  ;
                        valpart
               vtotalh = vtotalh +  ;
                         valpart
               vtotsig = vtotsig +  ;
                         IIF(tipope =  ;
                         '-', -1 *  ;
                         valpart,  ;
                         valpart)
               IF valpart = 0
                    IF RLOCK()
                         DELETE NEXT  ;
                                1
                    ENDIF
                    UNLOCK
               ENDIF
          ENDSCAN
          SET FILTER TO
          GOTO TOP
          SELECT hojmod
          m.imptot = vtotal
          m.codcal = ALLTRIM(STR(YEAR(m.fechc) -  ;
                     1900)) +  ;
                     ALLTRIM(m.nummes) +  ;
                     ALLTRIM(m.codfte)
          m.codpart = vantpart
          vimpr = m.periodo +  ;
                  ALLTRIM(m.nummes) +  ;
                  m.numhm
          vhm = ALLTRIM(m.nummeshc) +  ;
                m.numhc
          USE IN 8
          USE IN 9
          DO compre1
          SELECT hojmod
          m.estado = '00'
          IF f_appd()
               GATHER MEMVAR
          ENDIF
          = observa()
          SELECT hoja
          SEEK vhm
          IF FOUND()
               IF RLOCK()
                    REPLACE numhm  ;
                            WITH  ;
                            hojmod.numhm,  ;
                            fechm  ;
                            WITH  ;
                            hojmod.fechm
               ENDIF
               UNLOCK
          ELSE
               DO standby WITH  ;
                  'error!'
          ENDIF
     ENDIF
     SELECT hojmod
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
     SELECT hojmod
ENDIF
CLOSE DATABASES
DO abre
SELECT hojmod
GOTO BOTTOM
DO vista
ON KEY LABEL F2 DO VISOBS
RETURN
*
FUNCTION asig_fec
m.periodo = PADL(ALLTRIM(STR(YEAR(m.fechm) -  ;
            2000)), 2, '0')
RETURN .T.
*
FUNCTION trab_hijo
as = ALIAS()
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '░░░░░░░░F5->Agregar░░░░░░░░░░░░░░F8->Eliminar░░░░░░░░░░░░░░F10->Terminar░░░░░░░░'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO agreg_HM
ON KEY LABEL F8 DO elimi_HM
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT maepre
SET ORDER TO MAEPRE3
SELECT itehc
SET RELATION TO hoja.periodo + itehc.codcad;
+ itehc.codcom INTO maepre
SELECT itehc
SEEK ALLTRIM(m.nummeshc) +  ;
     m.numhc
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No tiene registros'
ENDIF
BROWSE FIELDS tipope :H = 'ń' :V =  ;
       tipope $ '-+', codfte :H =  ;
       'Fte' :V = val_para(codfte, ;
       'CODFTE','codfte') :F :P =  ;
       '!!', codcad :H = 'Cadena'  ;
       :V =  ;
       val_cadhm(itehc.codcad, ;
       hoja.periodo,'R',22,40)  ;
       .AND. sel3() :F,  ;
       maepre.codfun :H = 'Fn' :W =  ;
       .F., maepre.codprg :H =  ;
       'Prg' :W = .F.,  ;
       maepre.codspr :H = 'SPrg'  ;
       :W = .F., maepre.actpry :H =  ;
       'ActPry' :W = .F., codcom  ;
       :H = 'Comp.' :V =  ;
       val_comhm(m.periodo +  ;
       '01001' + itehc.codcad, ;
       itehc.codcom,'codcom')  ;
       .AND. sel3() :F :W =   ;
       .NOT. itehc.tipope $ 'Ŕ*',  ;
       codmet :H = 'Meta' :V =  ;
       val_methm(m.periodo +  ;
       '01001' + itehc.codcad, ;
       itehc.codcom +  ;
       itehc.codmet,'codmet')  ;
       .AND. sel3() :F :W =   ;
       .NOT. itehc.tipope $ 'Ŕ*',  ;
       codpart :H = 'Partid' :W =   ;
       .NOT. itehc.tipope $ 'Ŕ*',  ;
       codcla :H = 'Cla.' :V =  ;
       val_cla(codpart,codpart +  ;
       codcla,'codcla') :F,  ;
       valpart :H = 'Total' :P =  ;
       '99,999,999.99' :W =   ;
       .NOT. itehc.tipope $ 'Ŕ*',  ;
       xx = IIF( .NOT.  ;
       itehc.estado = '92',  ;
       '       ', 'H/M:' + numhm)  ;
       :H = IIF( .NOT. estado =  ;
       '90', '          ',  ;
       'Hoja Modificac') :W = .F.  ;
       NOMENU NOAPPEND NODELETE  ;
       WINDOW wind_2 KEY  ;
       ALLTRIM(m.nummeshc) +  ;
       m.numhc
vtempo = '░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SET FILTER TO
SELECT hojmod
IF LASTKEY() = 27
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION agreg_hm
SELECT itehc
vp = codpart
IF f_appd()
     REPLACE numhc WITH m.numhc,  ;
             nummes WITH  ;
             m.nummeshc, numhm  ;
             WITH m.numhm,  ;
             nummeshm WITH  ;
             m.nummes, codpart  ;
             WITH vp, tipdoc WITH  ;
             'H/M', tipope WITH  ;
             '-', estado WITH  ;
             '92', codfte WITH  ;
             m.codfte, operac  ;
             WITH m.operac,  ;
             tipcom WITH m.tiphm,  ;
             codcad WITH  ;
             maepre.codcad,  ;
             uniges WITH '01',  ;
             unieje WITH '001'
ENDIF
UNLOCK
RETURN .T.
*
FUNCTION elimi_hm
IF itehc.tipope $ 'Ŕ*'
     RETURN .T.
ENDIF
IF RLOCK()
     DELETE NEXT 1
ENDIF
UNLOCK
RETURN .T.
*
FUNCTION v_hc
PRIVATE vtemp2
SELECT hoja
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
m.numhc = PADL(ALLTRIM(m.numhc),  ;
          4, '0')
SEEK ALLTRIM(m.nummeshc) +  ;
     m.numhc
IF  .NOT. FOUND()
     vtemp2 = RECNO()
     HIDE MENU mmenu
     ACTIVATE SCREEN
     vtempo = '░░░░░░░░░░░Presione «F10» para seleccionar  o  «Esc» para cancelar░░░░░░░░░░░░'
     DO logos WITH rotulo1,  ;
        vtempo
     ON KEY LABEL F10 KEYBOARD CHR(23)
     BROWSE FIELDS nummes :H =  ;
            'Mes', numhc :H =  ;
            'H/C ', tipdoc :H =  ;
            'Doc', numref :H =  ;
            'Nž', ess =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'C/c ',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            '    ')))) :H =  ;
            'Estd', imptot :H =  ;
            'TOTAL' :P =  ;
            '9,999,999.99' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_0
     DO logos WITH rotulo1,  ;
        vtempo
     IF LASTKEY() = 27
          SELECT hoja
          GOTO BOTTOM
     ENDIF
     SHOW MENU mmenu
     ON KEY LABEL F10
ENDIF
IF hoja.estado = '99'
     DO standby WITH  ;
        'La Hoja ya esta anulada..'
     RETURN .F.
ENDIF
m.fechc = hoja.fechc
m.tipprv = hoja.tipprv
m.codprv = hoja.codprv
m.codemp = hoja.codemp
m.codotr = hoja.codotr
m.nombre = hoja.nombre
m.codfte = hoja.codfte
m.codpart = hoja.codpart
m.numhc = hoja.numhc
m.nummeshc = hoja.nummes
m.codcad = hoja.codcad
SELECT hoja
RETURN
*
FUNCTION t_h
as = ALIAS()
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '░░░░░░░░F5->Agregar░░░░░░░░░░░░░░F8->Eliminar░░░░░░░░░░░░░░F10->Terminar░░░░░░░░'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itehc
SEEK ALLTRIM(m.nummes) + m.numhc
SET FILTER TO estado <> '92'
IF  .NOT. FOUND()
     SET FILTER TO
     RETURN .F.
ENDIF
DO CASE
     CASE ALLTRIM(m.tipfun) = 'I'
          BROWSE FIELDS tipope :H =  ;
                 'ń' :V = tipope $  ;
                 '-+' :F :W = .F.,  ;
                 codpart :H =  ;
                 'Partida' :V =  ;
                 val_part(SUBSTR(codpart,  ;
                 4, 2), ;
                 LEFT(codpart, 2), ;
                 'codpart') .AND.  ;
                 pone() :F :W =  ;
                 .F., codanal :H =  ;
                 'Analitc' :V =  ;
                 val_part(SUBSTR(codanal,  ;
                 4, 2), ;
                 LEFT(codanal, 2), ;
                 'codanal') :F :W =  ;
                 .F., aa = IIF(  ;
                 .NOT.  ;
                 EMPTY(codanal),  ;
                 val_part(SUBSTR(codanal,  ;
                 4, 2), ;
                 LEFT(codanal, 2), ;
                 'D'), ' ') :H =  ;
                 'Descripción' :  ;
                 40 :W = .F.,  ;
                 valpart :H =  ;
                 'Total' :P =  ;
                 '99,999,999.99'  ;
                 :V = cale() :F  ;
                 :W = .F. NOMENU  ;
                 NOAPPEND  ;
                 NODELETE WINDOW  ;
                 wind_2 KEY  ;
                 ALLTRIM(m.nummes) +  ;
                 m.numhc
     CASE ALLTRIM(m.tipfun) = 'F'
          BROWSE FIELDS tipope :H =  ;
                 'ń' :V = tipope $  ;
                 '-+' :F, codanal  ;
                 :H = 'Analitc'  ;
                 :V =  ;
                 val_part(SUBSTR(codanal,  ;
                 4, 2), ;
                 LEFT(codanal, 2), ;
                 'codanal') :F :W =  ;
                 .F., aa = IIF(  ;
                 .NOT.  ;
                 EMPTY(codanal),  ;
                 val_part(SUBSTR(codanal,  ;
                 4, 2), ;
                 LEFT(codanal, 2), ;
                 'D'), ' ') :H =  ;
                 'Descripción' :  ;
                 40 :W = .F.,  ;
                 valpart :H =  ;
                 'Total' :P =  ;
                 '99,999,999.99'  ;
                 :V = cale() :F  ;
                 :W = .F. NOMENU  ;
                 NOAPPEND  ;
                 NODELETE WINDOW  ;
                 wind_2 KEY  ;
                 ALLTRIM(m.nummes) +  ;
                 m.numhc
     OTHERWISE
ENDCASE
vtempo = '░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
ON KEY LABEL F10
SET FILTER TO
IF LASTKEY() = 27
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION cale
REPLACE codfte WITH  ;
        SUBSTR(m.codcal, 5, 3),  ;
        codprg WITH  ;
        SUBSTR(m.codcal, 8, 2),  ;
        codsubpr WITH  ;
        SUBSTR(m.codcal, 10, 3)
RETURN .T.
*
FUNCTION pone
IF ALLTRIM(m.tipfun) = 'I'
     m.codpart = itehc.codpart
ENDIF
RETURN .T.
*
PROCEDURE borra_oc
ax = ALIAS()
SELECT itehc
SEEK ALLTRIM(m.nummes) + m.numhc
IF RLOCK()
     SCAN WHILE nummes =  ;
          ALLTRIM(m.nummes) .AND.  ;
          numhc = m.numhc
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
SELECT hojmod
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF LEFT(estado, 1) = '9'
     DO standby WITH  ;
        'La H/C ya esta anulada o con H/M'
     RETURN
ENDIF
IF yesno( ;
   'Ę Desea ANULAR esta Hoja de Modificación ?' ;
   )
     ACTIVATE WINDOW standby
     @ 1, 14 SAY  ;
       'Espere un Momento ....'  ;
       COLOR W/N* 
     SELECT itehc
     SET FILTER TO estado = '92'
     SEEK ALLTRIM(m.nummeshc) +  ;
          m.numhc
     SCAN FOR nummes =  ;
          ALLTRIM(m.nummeshc)  ;
          .AND. numhc = m.numhc  ;
          .AND. nummeshm =  ;
          m.nummes .AND. numhm =  ;
          m.numhm
          IF RLOCK()
               REPLACE estado  ;
                       WITH '99'
          ENDIF
          UNLOCK
     ENDSCAN
     SET FILTER TO
     SELECT hojmod
     IF RLOCK()
          REPLACE estado WITH  ;
                  '99'
     ENDIF
     UNLOCK
     DEACTIVATE WINDOW standby
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
SELECT hojmod
DO vista
UNLOCK ALL
RETURN
*
PROCEDURE lista
SET MEMOWIDTH TO 75
SELECT hojmod
vtemp = RECNO()
SELECT hojmod
SET RELATION TO nummeshc + numhc INTO;
itehc
SET SKIP TO itehc
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lishjm
ENDIF
SELECT hojmod
SET RELATION TO
SELECT hojmod
GOTO vtemp
DO vista
RETURN
*
PROCEDURE lishjm
DEFINE WINDOW lis FROM 5, 15 TO  ;
       21, 65 FLOAT TITLE  ;
       'Listado Hojas de Modificación'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro, vlista
vcli = m.numhm
vmes = m.nummes
vperi = SPACE(2)
@ 01, 01 SAY  ;
  '      Listado por : ' GET  ;
  vlista FUNCTION  ;
  '^ Documento;Resumen' WHEN  ;
  vtocli = 1
@ 04, 01 SAY  ;
  '              H/M : '
@ 04, 22 GET vmes PICTURE '!!'
@ 04, 25 GET vcli PICTURE '!!!!'  ;
  VALID val_hc() WHEN vlista = 1
@ 04, 31 SAY '    Periodo: ' GET  ;
  vperi WHEN vlista = 2
@ 06, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Numero;Proveedor;Emisión'  ;
  WHEN vlista = 2
@ 10, 01 SAY  ;
  '           Estado : ' GET  ;
  vtippro FUNCTION  ;
  '^ Todos;Pendientes;Atendidos'  ;
  WHEN vlista = 2
@ 14, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     vind = SYS(3) + '.IDX'
     SELECT itehc
     SELECT hojmod
     IF vlista = 1
          INDEX ON IIF(vorden = 1,  ;
                nummes + numhm,  ;
                IIF(vorden = 2,  ;
                codprv,  ;
                DTOS(fechc))) TO  ;
                (vind) FOR  ;
                (nummes + numhm =  ;
                vmes + vcli)  ;
                .AND. IIF(vtippro =  ;
                1, .T.,  ;
                IIF(vtippro = 2,  ;
                estado = '00',  ;
                estado = '50'))
     ELSE
          INDEX ON IIF(vorden = 1,  ;
                nummes + numhm,  ;
                IIF(vorden = 2,  ;
                codprv,  ;
                DTOS(fechc))) TO  ;
                (vind) FOR  ;
                (nummes = vmes)  ;
                .AND. (periodo =  ;
                vperi) .AND.  ;
                IIF(vtippro = 1,  ;
                .T., IIF(vtippro =  ;
                2, estado = '00',  ;
                estado = '50'))
     ENDIF
     SET INDEX TO (vind)
     GOTO TOP
     SET RELATION TO nummeshc + numhc;
INTO itehc
     SET SKIP TO itehc
     DEACTIVATE WINDOW standby
     SCATTER MEMVAR
     vtitulo = IIF(vtippro = 1,  ;
               ' en General ',  ;
               IIF(vtippro = 2,  ;
               ' Pendientes ',  ;
               ' Atendidos '))
     IF  .NOT. EOF()
          IF vlista = 1
               DO reporte WITH 2,  ;
                  'LisHm1',  ;
                  ' Hojas de Modificación '
          ELSE
               DO reporte WITH 2,  ;
                  'LisHm2',  ;
                  ' Hojas de Modificación '
          ENDIF
     ELSE
          DO standby WITH vmens08
     ENDIF
     CLOSE INDEX
     ERASE (vind)
ENDIF
SELECT itehc
SET FILTER TO
SELECT hojmod
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
SET MEMOWIDTH TO 75
ON KEY LABEL F10 KEYBOARD CHR(23)
IF  .NOT. WEXIST('Observa')
     DEFINE WINDOW observa FROM  ;
            03, 02 TO 12, 78  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '▒ Justificaciones ▒'  ;
            FOOTER  ;
            ' ░ «F10» Graba ░ '  ;
            DOUBLE COLOR SCHEME  ;
            1
ENDIF
IF WVISIBLE('Observa')
     ACTIVATE WINDOW SAME observa
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              observa
ENDIF
MODIFY MEMO observa WINDOW  ;
       observa
IF  .NOT. WVISIBLE('Observa')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado. No graba la Observación '
ENDIF
SELECT (valias)
RETURN .T.
*
FUNCTION visobs
valias = ALIAS()
IF  .NOT. WEXIST('Observa')
     DEFINE WINDOW observa FROM  ;
            03, 02 TO 12, 78  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '▒ Justificaciones ▒'  ;
            FOOTER  ;
            ' ░ «Esc» Sale ░ '  ;
            DOUBLE COLOR SCHEME  ;
            1
ENDIF
IF WVISIBLE('Observa')
     ACTIVATE WINDOW SAME observa
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              observa
ENDIF
MODIFY MEMO observa NOEDIT WINDOW  ;
       observa
IF  .NOT. WVISIBLE('Observa')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
RETURN .T.
*
FUNCTION compre1
PRIVATE as, vtemp
as = ALIAS()
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '░░░░░░░░F5->Agregar░░░░░░░░░░░░░░F8->Eliminar░░░░░░░░░░░░░░F10->Terminar░░░░░░░░'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO agreg_ap
ON KEY LABEL F8 DO elimi_ap
ON KEY LABEL F10 KEYBOARD CHR(23)
USE IN 13 AstPre ALIAS astpre  ;
    ORDER Astpre5
SELECT astpre
SET ORDER TO Astpre5
SEEK ALLTRIM(m.nummes) + m.numhm +  ;
     'H/M'
IF  .NOT. FOUND()
     DO agreg_ap
ENDIF
SET ORDER TO ASTPRE5
BROWSE FIELDS tipo :H = '' :V =  ;
       tipo $ 'DH' .AND.  ;
       valtipo() :F, ctadeb :H =  ;
       'Cta.Debe' :V =  ;
       val_fun('Cuenta','Cuenta', ;
       "LEFT(Cuenta,11)+' '+Descri", ;
       ctadeb,2) :F :W = IIF(tipo =  ;
       'D', .T., .F.), ctahab :H =  ;
       'Cta.Haber' :V =  ;
       val_fun('Cuenta','Cuenta', ;
       "LEFT(Cuenta,11)+' '+Descri", ;
       ctahab,2) :F :W = IIF(tipo =  ;
       'H', .T., .F.), valdeb :H =  ;
       ' al Debe ' :W = IIF(tipo =  ;
       'D', .T., .F.), valhab :H =  ;
       ' al Haber' :W = IIF(tipo =  ;
       'H', .T., .F.) NOMENU  ;
       NOAPPEND NODELETE WINDOW  ;
       wind_5 KEY  ;
       ALLTRIM(m.nummes) +  ;
       m.numhm + 'H/M'
SEEK ALLTRIM(m.nummes) + m.numhm +  ;
     'H/M'
SCAN WHILE nummes =  ;
     ALLTRIM(m.nummes) .AND.  ;
     numref = m.numhm .AND.  ;
     tipdoc = 'H/M'
     DO CASE
          CASE tipo = '*' .OR.  ;
               tipo = ' '
               IF RLOCK()
                    DELETE NEXT 1
               ENDIF
               UNLOCK
          OTHERWISE
               IF RLOCK()
                    REPLACE cuenta  ;
                            WITH  ;
                            IIF(tipo =  ;
                            'D',  ;
                            ctadeb,  ;
                            ctahab)
               ENDIF
               UNLOCK
     ENDCASE
ENDSCAN
vtempo = '░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
ON KEY LABEL F10
IF LASTKEY() = 27
     RETURN .F.
ENDIF
RETURN .T.
*
PROCEDURE vis_ap
PRIVATE ad
ad = ALIAS()
USE IN 13 AstPre ALIAS astpre  ;
    ORDER Astpre5
SELECT astpre
SET ORDER TO astpre5
SEEK ALLTRIM(m.nummes) + m.numhm +  ;
     'H/M'
ON KEY LABEL F7
BROWSE FIELDS tipo :H = '' :V =  ;
       tipo $ 'DH' .AND.  ;
       valtipo() :F, ctadeb :H =  ;
       'Cta.Debe' :V =  ;
       val_fun('Cuenta','Cuenta', ;
       "LEFT(Cuenta,10)+' '+Descri", ;
       ctadeb,2) :F :W = IIF(tipo =  ;
       'D', .T., .F.), ctahab :H =  ;
       'Cta.Haber' :V =  ;
       val_fun('Cuenta','Cuenta', ;
       "LEFT(Cuenta,10)+' '+Descri", ;
       ctahab,2) :F :W = IIF(tipo =  ;
       'H', .T., .F.), valdeb :H =  ;
       ' al Debe ' :W = IIF(tipo =  ;
       'D', .T., .F.), valhab :H =  ;
       ' al Haber' :W = IIF(tipo =  ;
       'H', .T., .F.) NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_5 KEY  ;
       ALLTRIM(m.nummes) +  ;
       m.numhm + 'H/M'
USE IN 13
SELECT (ad)
ON KEY LABEL F7 do vis_ap       
RETURN
*
PROCEDURE agreg_ap
IF f_appd()
     REPLACE nummes WITH m.nummes,  ;
             tipdoc WITH 'H/M',  ;
             numref WITH m.numhm,  ;
             fecref WITH m.fecref,  ;
             codpart WITH  ;
             m.codpart
     REPLACE periodo WITH  ;
             m.periodo
ELSE
     DO standby WITH  ;
        'No se pudo Agregar'
ENDIF
UNLOCK
RETURN
*
FUNCTION elimi_ap
SELECT astpre
vfun = .T.
IF RLOCK()
     DELETE NEXT 1
ELSE
     vfun = .F.
ENDIF
RETURN .T.
*
FUNCTION valtipo
DO CASE
     CASE tipo = 'D'
          REPLACE ctahab WITH  ;
                  SPACE(10),  ;
                  valhab WITH 0
     CASE tipo = 'H'
          REPLACE ctadeb WITH  ;
                  SPACE(10),  ;
                  valdeb WITH 0
ENDCASE
RETURN .T.
*
PROCEDURE xcompre1
PARAMETER sig
ax = ALIAS()
m.valdeb = 0
m.valhab = 0
IF sig = '-'
ELSE
ENDIF
SELECT cuenta
ACTIVATE WINDOW wind_5
@ 00, 08 SAY 'Cuentas '
@ 00, 18 SAY 'Debe '
@ 00, 34 SAY 'Haber '
@ 01, 04 GET m.ctadeb PICTURE  ;
  '!!!!!!!!!!' VALID  ;
  val_fun('Cuenta','Cuenta', ;
  "LEFT(Cuenta,10)+' '+Descri", ;
  m.ctadeb,1)
@ 01, 18 GET m.valdeb PICTURE  ;
  '99,999,999.99' VALID valdb1()
READ
@ 02, 12 GET m.ctahab PICTURE  ;
  '!!!!!!!!!!' VALID  ;
  val_fun('Cuenta','Cuenta', ;
  "LEFT(Cuenta,10)+' '+Descri", ;
  m.ctahab,1)
@ 02, 34 GET m.valhab PICTURE  ;
  '99,999,999.99' VALID valhb1()
READ
DEACTIVATE WINDOW wind_5
RETURN
*
PROCEDURE valdb1
as = ALIAS()
SELECT astpre
SET ORDER TO 3
SEEK 'D' + ALLTRIM(m.nummes) +  ;
     ALLTRIM(m.numhm)
IF  .NOT. FOUND()
     IF f_appd()
          REPLACE nummes WITH  ;
                  m.nummes,  ;
                  tipdoc WITH  ;
                  'H/M', numref  ;
                  WITH m.numhm,  ;
                  cuenta WITH  ;
                  m.ctadeb, tipo  ;
                  WITH 'D',  ;
                  fecref WITH  ;
                  m.fecref,  ;
                  codpart WITH  ;
                  m.codpart,  ;
                  codcal WITH  ;
                  m.periodo +  ;
                  ALLTRIM(m.nummes) +  ;
                  ALLTRIM(m.codfte) +  ;
                  ALLTRIM(vcodprg) +  ;
                  ALLTRIM(vcodsub) +  ;
                  IIF(ALLTRIM(m.tipfun) =  ;
                  'I',  ;
                  ALLTRIM(vproyec) +  ;
                  ALLTRIM(vsubpry),  ;
                  ALLTRIM(vcodact))
     ENDIF
     UNLOCK
ENDIF
IF RLOCK()
     REPLACE ctadeb WITH m.ctadeb,  ;
             ctahab WITH  ;
             SPACE(10), valdeb  ;
             WITH m.valdeb,  ;
             valhab WITH 0,  ;
             codcal WITH m.codcal,  ;
             codcal WITH  ;
             m.periodo +  ;
             ALLTRIM(m.nummes) +  ;
             ALLTRIM(m.codfte) +  ;
             ALLTRIM(vcodprg) +  ;
             ALLTRIM(vcodsub) +  ;
             IIF(ALLTRIM(m.tipfun) =  ;
             'I',  ;
             ALLTRIM(vproyec) +  ;
             ALLTRIM(vsubpry),  ;
             ALLTRIM(vcodact))
ENDIF
SELECT (as)
RETURN
*
PROCEDURE valhb1
as = ALIAS()
SELECT astpre
SET ORDER TO 3
SEEK 'H' + ALLTRIM(m.nummes) +  ;
     ALLTRIM(m.numhm)
IF  .NOT. FOUND()
     IF f_appd()
          REPLACE nummes WITH  ;
                  m.nummes,  ;
                  tipdoc WITH  ;
                  'H/M', numref  ;
                  WITH m.numhm,  ;
                  cuenta WITH  ;
                  m.ctahab, tipo  ;
                  WITH 'H',  ;
                  fecref WITH  ;
                  m.fecref,  ;
                  codpart WITH  ;
                  m.codpart,  ;
                  codcal WITH  ;
                  m.periodo +  ;
                  ALLTRIM(m.nummes) +  ;
                  ALLTRIM(m.codfte) +  ;
                  ALLTRIM(vcodprg) +  ;
                  ALLTRIM(vcodsub) +  ;
                  IIF(ALLTRIM(m.tipfun) =  ;
                  'I',  ;
                  ALLTRIM(vproyec) +  ;
                  ALLTRIM(vsubpry),  ;
                  ALLTRIM(vcodact))
     ENDIF
     UNLOCK
ENDIF
IF RLOCK()
     REPLACE ctadeb WITH  ;
             SPACE(10), ctahab  ;
             WITH m.ctahab,  ;
             valdeb WITH 0,  ;
             valhab WITH m.valhab,  ;
             codcal WITH m.codcal,  ;
             codcal WITH  ;
             m.periodo +  ;
             ALLTRIM(m.nummes) +  ;
             ALLTRIM(m.codfte) +  ;
             ALLTRIM(vcodprg) +  ;
             ALLTRIM(vcodsub) +  ;
             IIF(ALLTRIM(m.tipfun) =  ;
             'I',  ;
             ALLTRIM(vproyec) +  ;
             ALLTRIM(vsubpry),  ;
             ALLTRIM(vcodact))
ENDIF
SELECT (as)
RETURN
*
PROCEDURE val_hc
SELECT hojmod
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SEEK vmes + vcli
IF  .NOT. FOUND()
     SET RELATION TO nummeshc + numhc;
INTO itehc
     SET SKIP TO itehc
     vtemp = RECNO()
     HIDE MENU mmenu
     ACTIVATE SCREEN
     vtempo = '░░░░░░░░░░░Presione «F10» para seleccionar  o  «Esc» para cancelar░░░░░░░░░░░░'
     DO logos WITH rotulo1,  ;
        vtempo
     ON KEY LABEL F10 KEYBOARD CHR(23)
     BROWSE FIELDS nummes :H =  ;
            'Mes', numhc :H =  ;
            'H/C ', tipdoc :H =  ;
            'Doc', numref :H =  ;
            'Nž', ess =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'C/c ',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            '    ')))) :H =  ;
            'Estd', codprv :H =  ;
            'Prv', codcal :H =  ;
            'Calendario',  ;
            itehc.codpart :H =  ;
            'Partida',  ;
            itehc.valpart :H =  ;
            'Parcial', imptot :H =  ;
            'TOTAL' :P =  ;
            '99,999.99' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_0
     vtempo = '░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░'
     DO logos WITH rotulo1,  ;
        vtempo
     IF LASTKEY() = 27
          GOTO BOTTOM
     ENDIF
ENDIF
vmes = nummes
vcli = numhm
vresto = 0
SELECT itehc
SEEK hojmod.nummeshc +  ;
     hojmod.numhc
IF FOUND()
     DO WHILE nummes= ;
        hojmod.nummeshc .AND.  ;
        numhc=hojmod.numhc
          IF tipope = '-' .AND.  ;
             hojmod.operac = 'R'
               vresto = vresto +  ;
                        itehc.valpart
          ENDIF
          SKIP
          IF EOF()
               EXIT
          ENDIF
     ENDDO
ENDIF
SELECT hoja
SEEK hojmod.nummeshc +  ;
     hojmod.numhc
vtotahc = imptot
ON KEY LABEL F10
SELECT hojmod
SET RELATION TO
RETURN
*
FUNCTION vtotales
vresto = 0
vtotahc = 0
SELECT itehc
mreg = RECNO()
SEEK hojmod.nummeshc +  ;
     hojmod.numhc
IF FOUND()
     SCAN FOR itehc.nummes =  ;
          hojmod.nummeshc .AND.  ;
          itehc.numhc =  ;
          hojmod.numhc
          IF tipope = '-' .AND.  ;
             hojmod.operac = 'R'
               vresto = vresto +  ;
                        itehc.valpart
          ENDIF
          SELECT itehc
     ENDSCAN
ENDIF
GOTO mreg
SELECT hoja
SEEK hojmod.nummeshc +  ;
     hojmod.numhc
vtotahc = imptot - vresto
SELECT hojmod
RETURN vtotahc
*
FUNCTION valprv
PRIVATE xx, vfun
vfun = .F.
malias = ALIAS()
m.codprv = IIF(EMPTY(m.codprv),  ;
           m.codprv,  ;
           PADL(ALLTRIM(m.codprv),  ;
           4, '0'))
xx = val_prv(m.codprv,.T.,4,30)
IF xx
     m.codemp = '     '
     SELECT (malias)
     RETURN .T.
ENDIF
SELECT (malias)
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
                 'Código', descri  ;
                 :H = 'Nombre'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 WINDOW _xx TITLE  ;
                 ' «Enter»  Selecciona   '  ;
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
FUNCTION vale_mes
xx = PADL(ALLTRIM(STR((VAL(m.nummes)),  ;
     2)), 2, '0')
m.nummes = xx
m.fechm = IIF(VAL(m.nummes) <  ;
          MONTH(DATE()), CTOD( ;
          '01/' + xx + '/' +  ;
          STR(YEAR(DATE()), 4)),  ;
          DATE())
RETURN .T.
*
FUNCTION val_mes
SELECT parma
SEEK 'HOJCON' + ALLTRIM(m.nummes)
IF  .NOT. FOUND()
     DO standby WITH  ;
        'El Correlativo del Mes no está Inicializado'
     SELECT hoja
     RETURN .F.
ELSE
     IF codigoaux = '00'
          DO standby WITH  ;
             'El Calendario del Mes '+ ;
             ALLTRIM(m.nummes)+ ;
             ' ya está cerrado'
          SELECT hoja
          RETURN .F.
     ELSE
          m.numhc = valhc(parma.nument +  ;
                    1)
          xx = PADL(ALLTRIM(STR((VAL(m.nummes) +  ;
               1), 2)), 2, '0')
          m.fechc = IIF(VAL(m.nummes) <  ;
                    MONTH(DATE()),  ;
                    CTOD('01/' +  ;
                    xx + '/95') -  ;
                    1, DATE())
          SELECT hoja
          RETURN .T.
     ENDIF
ENDIF
*
PROCEDURE abre
USE IN 1 hojmod ALIAS hojmod  ;
    ORDER hojmod1
USE IN 3 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 4 Itehc ALIAS itehc ORDER  ;
    Itehc1
USE IN 5 PARMAE ALIAS parma ORDER  ;
    Parmae1
USE IN 6 maepre ALIAS maepre  ;
    ORDER maepre3
USE IN 7 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 8 Calen ALIAS calen ORDER  ;
    calen1
USE IN 9 Clase ALIAS clase ORDER  ;
    Clase1
USE IN 10 Auxil ALIAS auxil ORDER  ;
    Auxil1
USE IN 12 Cuentas ALIAS cuenta  ;
    ORDER Cuentas6
USE IN 13 AstPre ALIAS astpre  ;
    ORDER Astpre5
USE IN 16 ITECLA ORDER ITECLA1
ON KEY LABEL F2 DO VISOBS
RETURN
*
PROCEDURE listar
CLOSE DATABASES
USE IN 1 hojmod ALIAS hojmod  ;
    ORDER hojmod1
USE IN 3 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 4 Itehc ALIAS itehc ORDER  ;
    Itehc1
USE IN 5 PARMAE ALIAS parma ORDER  ;
    Parmae1
USE IN 6 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 7 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 8 Calen ALIAS calen ORDER  ;
    calen1
USE IN 9 Clase ALIAS clase ORDER  ;
    Clase1
USE IN 10 Auxil ALIAS auxil ORDER  ;
    Auxil1
USE IN 12 Cuentas ALIAS cuenta  ;
    ORDER Cuentas6
USE IN 13 AstPre ALIAS astpre  ;
    ORDER Astpre5
SELECT hoja
SET RELATION TO nummeshc + numhc INTO;
itehc
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
FUNCTION vcorhm
SELECT hojmod
m.nummes = ALLTRIM(m.nummes)
SEEK m.nummes
xnumhm = '0'
SCAN WHILE nummes = m.nummes
     xnumhm = numhm
ENDSCAN
m.numhm = PADL(ALLTRIM(STR(VAL(xnumhm) +  ;
          1)), 4, '0')
SHOW GETS
RETURN .T.
*
FUNCTION valer
SELECT hojmod
m.numhm = PADL(ALLTRIM(m.numhm),  ;
          4, '0')
SEEK ALLTRIM(m.nummes) + m.numhm
IF FOUND()
     DO standby WITH  ;
        'La H/M ya ha sido generada'
     BROWSE
     RETURN .F.
ELSE
     RETURN .T.
ENDIF
*
FUNCTION asiact
REPLACE codact WITH vcodact
RETURN .T.
*
FUNCTION asipry
REPLACE codproy WITH vproyec
RETURN .T.
*
FUNCTION valpyac
PARAMETER nvalor, nfiltro,  ;
          nvariable, ncol, nlong,  ;
          nancho
PRIVATE nalias
DO CASE
     CASE PARAMETERS() = 2
          ncol = 0
          nvariable = ' '
          nlong = 40
          nancho = 6
     CASE PARAMETERS() = 3
          ncol = 0
          nlong = 40
          nancho = 6
     CASE PARAMETERS() = 4
          nlong = 40
          nancho = 6
     CASE PARAMETERS() = 5
          nancho = 6
ENDCASE
nalias = ALIAS()
SELECT maepre
SET ORDER TO IIF(ALLTRIM(m.tipfun)='I',6,7)
SEEK ALLTRIM(itehc.tipfun) +  ;
     nfiltro + nvalor
IF  .NOT. FOUND() .OR.  .NOT.  ;
    nvariable $ 'V'
     SET FILTER TO periodo + codprg +;
codsubpr = nfiltro
     GOTO TOP
     IF  .NOT. EOF()
          IF  .NOT. EMPTY(nvalor)
               SEEK ALLTRIM(itehc.tipfun) +  ;
                    nfiltro +  ;
                    nvalor
               IF  .NOT. FOUND()
                    DO yrolea
               ENDIF
          ELSE
               DO yrolea
          ENDIF
     ELSE
          DO standby WITH  ;
             'Error en Codificación programática'
          SET FILTER TO
          IF  .NOT. EMPTY(nalias)
               SELECT (nalias)
          ENDIF
          RETURN .F.
     ENDIF
ENDIF
nvalor = IIF(ALLTRIM(itehc.tipfun) =  ;
         'I', maepre.codproy,  ;
         maepre.codact)
ndescr = SUBSTR(maepre.descri, 1,  ;
         nlong)
SET FILTER TO
IF  .NOT. EMPTY(nalias)
     SELECT (nalias)
ENDIF
DO CASE
     CASE nvariable = ' '
          @ ROW(), ncol SAY  ;
            PADL(nvalor, nancho,  ;
            ' ')
          @ ROW(), ncol SAY  ;
            ndescr
          RETURN .T.
     CASE nvariable = 'A'
          @ ROW(), ncol SAY  ;
            ndescr
          RETURN
     CASE nvariable = 'V'
          @ ROW(), COL() SAY  ;
            PADR(nvalor, nancho,  ;
            ' ')
          RETURN ndescr
     CASE nvariable = 'D'
          RETURN ndescr
     CASE nvariable = 'Z'
          RETURN ndescr
     CASE nvariable = 'C'
          RETURN .T.
     OTHERWISE
          &nvariable = nvalor
          RETURN .T.
ENDCASE
*
PROCEDURE yrolea
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _xx FROM 06, 10 TO  ;
       17, 69 FLOAT SHADOW DOUBLE  ;
       COLOR SCHEME 10
GOTO TOP
DO CASE
     CASE ALLTRIM(itehc.tipfun) =  ;
          'I' .AND.  .NOT. EOF()  ;
          .AND.  .NOT.  ;
          EMPTY(codproy)
          BROWSE FIELDS codproy  ;
                 :H = 'Pry',  ;
                 descri :H =  ;
                 'Detalle' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 _xx TITLE  ;
                 ' PROYECTOS :  «Enter»  Selecciona  '  ;
                 NOLGRID
     CASE ALLTRIM(itehc.tipfun) =  ;
          'F' .AND.  .NOT. EOF()  ;
          .AND.  .NOT.  ;
          EMPTY(codact)
          BROWSE FIELDS codact :H =  ;
                 'Act', descri :H =  ;
                 'Detalle' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 _xx TITLE  ;
                 ' ACTIVIDAD :  «Enter»  Selecciona  '  ;
                 NOLGRID
     OTHERWISE
          IF  .NOT. EMPTY(nvalor)
               DO standby WITH  ;
                  'No se tiene '+ ;
                  IIF(ALLTRIM(hoja.tipfun)= ;
                  'F',  ;
                  'Actividad',  ;
                  'Proyecto')+ ;
                  ' en referencia ---> '+ ;
                  m.tipfun
          ENDIF
ENDCASE
ON KEY LABEL ENTER
RELEASE WINDOW _xx
IF  .NOT. EMPTY(_oldwnd)
     ACTIVATE WINDOW &_oldwnd
ENDIF
RETURN
*
FUNCTION sigue
SELECT hojmod
IF EOF()
     RETURN .T.
ELSE
     SKIP
ENDIF
RETURN .T.
*
FUNCTION ve_cad
IF LASTKEY() <> 27
     @ 7, 22 SAY  ;
       val_para(maepre.codfun, ;
       'CODFUN','V',22,40)
     @ 8, 22 SAY  ;
       val_para1(maepre.codprg, ;
       'CODPRG' + maepre.codfun, ;
       'V',22,40)
     @ 9, 22 SAY  ;
       val_para1(maepre.codspr, ;
       'CODSPR' + maepre.codprg, ;
       'V',22,40)
     @ 10, 22 SAY maepre.actpry
     @ 10, 29 SAY  ;
       val_para(maepre.actpry, ;
       'ACTPRY','D',23,40)
ENDIF
RETURN .T.
*
FUNCTION val_cadhm
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
IF sistema = '1'
     SET FILTER TO periodo + '01001' =;
filtro + '01001'
     SEEK filtro + '01001' +  ;
          mvalor
ELSE
     SET FILTER TO LEFT(codcad, 1) = 'C'
     SEEK filtro + mvalor
ENDIF
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd1 = WOUTPUT()
     ACTIVATE SCREEN
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen Registros para Procesar'
          SET FILTER TO
          SET ORDER TO maepre3
          IF  .NOT. EMPTY(malias)
               SELECT (malias)
          ENDIF
          RETURN
     ENDIF
     ON KEY LABEL f2 DO busCodCad
     ON KEY LABEL f3 DO busDesCad
     ON KEY LABEL f10 KEYBOARD CHR(23)
     DEFINE WINDOW wind_cad FROM  ;
            02, 01 TO 23, 78  ;
            TITLE  ;
            '[F2]Cadena   [F3]Componente   [F10] seleccionar'  ;
            DOUBLE COLOR SCHEME  ;
            15
     ACTIVATE WINDOW wind_cad
     BROWSE NOOPTIMIZE FIELDS  ;
            codcad :H = 'CodCad',  ;
            uniges :H = 'GEST.',  ;
            unieje :H = 'EJEC.',  ;
            codfun :H = 'Fn',  ;
            codprg :H = 'Prg',  ;
            codspr :H = 'SPrg',  ;
            actpry :H = 'Act/Pry',  ;
            codcom :H = 'CodComp',  ;
            xx = val_para(codcom, ;
            'CODCOM','D') :H =  ;
            'Descripción' : 25,  ;
            codmet :H = 'Meta',  ;
            descri :H =  ;
            'Descripción' : 20  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR TITLE  ;
            'Relación de Cadenas Funcionales '  ;
            IN wind_cad  ;
            NOREFRESH
     RELEASE WINDOW wind_cad
     ON KEY LABEL f2
     ON KEY LABEL f3
     IF  .NOT. EMPTY(_oldwnd1)
          ACTIVATE WINDOW &_OldWnd1
     ENDIF
     SET ORDER TO maepre3
ENDIF
SET FILTER TO
SELECT maepre
SET ORDER TO maepre3
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
     CASE mvariable = 'R'
          REPLACE itehc.codcad  ;
                  WITH  ;
                  maepre.codcad
     OTHERWISE
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
RETURN
*
FUNCTION val_comhm
PARAMETER vfiltro, vbusca,  ;
          mvariable
valias = ALIAS()
SELECT maepre
SET ORDER TO MAEPRE1
vrecno = RECNO()
vorder = ORDER()
IF sistema = '2'
     SET ORDER TO MAEPRE1
ENDIF
SEEK vfiltro
vbusca1 = periodo + uniges +  ;
          unieje + codfun +  ;
          codprg + codspr +  ;
          actpry
SET ORDER TO maepre4
SEEK vbusca1 + vbusca
IF  .NOT. FOUND()
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SEEK vbusca1
     vkey = periodo + uniges +  ;
            unieje + codfun +  ;
            codprg + codspr +  ;
            actpry
     SET FILTER TO periodo + uniges +;
unieje + codfun + codprg + codspr + actpry;
= vkey
     GOTO TOP
     ON KEY LABEL f10 KEYBOARD CHR(23)
     DEFINE WINDOW wind_cad FROM  ;
            05, 20 TO 15, 60  ;
            TITLE  ;
            ' ▒ Componentes ▒ '  ;
            FOOTER  ;
            ' ░ «F10» Graba ░ '  ;
            DOUBLE COLOR SCHEME  ;
            15
     ACTIVATE WINDOW wind_cad
     BROWSE NOOPTIMIZE FIELDS  ;
            codcom :H = 'CodComp',  ;
            xx = val_para(codcom, ;
            'CODCOM','D') :H =  ;
            'Descripción' : 25  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR TITLE  ;
            'Relación de Cadenas Funcionales '  ;
            IN wind_cad  ;
            NOREFRESH
     RELEASE WINDOW wind_cad
     SET FILTER TO
ENDIF
mvalor = maepre.codcom
mdescr = val_para(maepre.codcom, ;
         'CODCOM','D')
SELECT maepre
IF sistema = '2'
     SET ORDER TO maepre3
ELSE
     SET ORDER TO maepre1
ENDIF
SELECT maepre
SET ORDER TO MAEPRE3
IF  .NOT. EMPTY(valias)
     SELECT (valias)
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
RETURN
*
FUNCTION val_methm
PARAMETER vfiltro, vbusca,  ;
          mvariable
valias = ALIAS()
SELECT maepre
SET ORDER TO MAEPRE1
vrecno = RECNO()
SEEK vfiltro
vbusca1 = periodo + uniges +  ;
          unieje + codfun +  ;
          codprg + codspr +  ;
          actpry
SET ORDER TO maepre4
SEEK vbusca1 + vbusca
IF  .NOT. FOUND()
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SEEK vbusca1
     vord1 = periodo + uniges +  ;
             unieje + codfun +  ;
             codprg + codspr +  ;
             actpry + codcom
     SET FILTER TO periodo + uniges +;
unieje + codfun + codprg + codspr + actpry;
+ codcom = vord1
     GOTO TOP
     ON KEY LABEL f10 KEYBOARD CHR(23)
     DEFINE WINDOW wind_cad1 FROM  ;
            05, 20 TO 15, 60  ;
            TITLE ' ▒ Metas ▒ '  ;
            FOOTER  ;
            ' ░ «F10» Graba ░ '  ;
            DOUBLE COLOR SCHEME  ;
            15
     ACTIVATE WINDOW wind_cad1
     BROWSE NOOPTIMIZE FIELDS  ;
            codmet :H = 'META',  ;
            descri :H =  ;
            'Descripción' : 30  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR TITLE  ;
            'Relación de Cadenas Funcionales '  ;
            IN wind_cad1  ;
            NOREFRESH
     RELEASE WINDOW wind_cad1
     SET FILTER TO
ENDIF
mvalor = maepre.codmet
mdescr = maepre.descri
SELECT maepre
SET ORDER TO MAEPRE3
IF  .NOT. EMPTY(valias)
     SELECT (valias)
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
RETURN
*
FUNCTION sel3
vx = SELECT()
SELECT maepre
SET ORDER TO MAEPRE3
SELECT (vx)
RETURN .T.
*
PROCEDURE imp_hm
SET MEMOWIDTH TO 75
SELECT hojmod
vtemp = RECNO()
SELECT hojmod
SET RELATION TO nummeshc + numhc INTO;
itehc
SET SKIP TO itehc
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lishjm1
ENDIF
SELECT hojmod
SET RELATION TO
SELECT hojmod
GOTO vtemp
DO vista
RETURN
*
PROCEDURE lishjm1
STORE 1 TO vtocli, vorden,  ;
      vtippro, vlista
vcli = m.numhm
vmes = m.nummes
vperi = SPACE(2)
vlista = 1
vorden = 1
vtippro = 1
RELEASE WINDOW lis
ACTIVATE WINDOW standby
@ 01, 04 SAY  ;
  'Espere un momento........'
vind = SYS(3) + '.IDX'
SELECT hojmod
IF vlista = 1
     INDEX ON IIF(vorden = 1,  ;
           nummes + numhm,  ;
           IIF(vorden = 2, codprv,  ;
           DTOS(fechc))) TO  ;
           (vind) FOR (nummes +  ;
           numhm = vmes + vcli)  ;
           .AND. IIF(vtippro = 1,  ;
           .T., IIF(vtippro = 2,  ;
           estado = '00', estado =  ;
           '50'))
ELSE
     INDEX ON IIF(vorden = 1,  ;
           nummes + numhm,  ;
           IIF(vorden = 2, codprv,  ;
           DTOS(fechc))) TO  ;
           (vind) FOR (nummes =  ;
           vmes) .AND. (periodo =  ;
           vperi) .AND.  ;
           IIF(vtippro = 1, .T.,  ;
           IIF(vtippro = 2,  ;
           estado = '00', estado =  ;
           '50'))
ENDIF
SET INDEX TO (vind)
GOTO TOP
SET RELATION TO nummeshc + numhc INTO;
itehc
SET SKIP TO itehc
DEACTIVATE WINDOW standby
SCATTER MEMVAR
SET RELATION TO nummeshc + numhc INTO;
hoja ADDITIVE
DO reporte WITH 2, 'LisHm1',  ;
   ' Hojas de Modificación '
SELECT hojmod
SET RELATION OFF INTO hoja
CLOSE INDEX
ERASE (vind)
SELECT itehc
SET FILTER TO
SELECT hojmod
RETURN
*
PROCEDURE pres_pro
DO CASE
     CASE m.tipprv = 'E'
          @ 5, 22 SAY  ;
            val_auxi(ALLTRIM(m.codemp), ;
            '30','V')
     CASE m.tipprv = 'P'
          @ 5, 22 SAY  ;
            val_auxi(ALLTRIM(m.codprv), ;
            '20','V')
     CASE m.tipprv = 'O' .AND.   ;
          .NOT. EMPTY(m.codotr)
          @ 5, 22 SAY  ;
            val_auxi(ALLTRIM(m.codotr), ;
            '09','V')
     CASE m.tipprv = 'I'
          @ 5, 22 SAY  ;
            val_auxi(ALLTRIM(m.codpre), ;
            '80','V')
     CASE m.tipprv = 'R' .AND.   ;
          .NOT. EMPTY(m.codotr)
          @ 5, 22 SAY  ;
            val_para(m.codotr, ;
            'CODRET','V',22,40)
     OTHERWISE
          @ 5, 22 SAY m.nombre
ENDCASE
RETURN
*

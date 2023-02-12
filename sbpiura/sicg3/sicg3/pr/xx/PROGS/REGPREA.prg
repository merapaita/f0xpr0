CLOSE DATABASES
USE IN 1 parmae ALIAS parma ORDER  ;
    parmae1
USE IN 2 maepar ALIAS presu ORDER  ;
    maepar1
USE IN 3 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 5 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 7 Calen ALIAS calen ORDER  ;
    Calen2
USE IN 9 IteHc ALIAS itehc ORDER  ;
    Itehc1
USE IN 13 traPAR ALIAS trapar  ;
    ORDER traPAR1
USE IN 10 Itetra ALIAS itetra  ;
    ORDER Itetra1
USE IN 4 CRESUP ALIAS cresup  ;
    ORDER CRESUP1
USE IN 11 Itecre ALIAS itecre  ;
    ORDER Itecre1
USE IN 12 HOJMOD ALIAS hojmod  ;
    ORDER hojmod1
USE IN 14 CatAsi ALIAS catasi  ;
    ORDER CatAsi4
PUBLIC vcodsub, vcodact, vproyec,  ;
       vsubpry, vcalend, vnewing
vmens01 = 'Registro de Presupuesto'
vmens02 = ' Presupuesto : REVISION '
vmens04 = 'Dicho Presupuesto no fue encontrado'
vmens05 = 'No existe Presupuesto anterior'
vmens06 = 'No existe Presupuesto siguiente'
vmens07 = '¨ Desea Anular ‚ste Presupuesto ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Presupuesto ha sido anulado'
vmens10 = 'El Presupuesto ya est  Atendido'
vmens11 = 'El Presupuesto ha sido devuelto'
SELECT presu
GOTO TOP
ON KEY LABEL F9 DO VISTA_DET
ON KEY LABEL F7 DO asign_tri
SCATTER BLANK MEMVAR
vnewing = .F.
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 11, 79 TITLE vmens02  ;
       FOOTER  ;
       '[F7] Asig. Trimestral'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle: Presupuesto         ®F9¯ Detalle : Item '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2a FROM 01, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle: Presupuesto         ®F10¯ Salir '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 09, 55  ;
       TO 23, 78 TITLE  ;
       '° Totales °' PANEL
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
       '\<Eliminar' AT 24, 54
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
ON SELECTION PAD elimi OF mmenu DO elimi
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_1
CLEAR
@ 0, 2 SAY '          Periodo :'
@ 0, 40 SAY 'Corr. cadena Fun. :'
@ 1, 2 SAY '   Unidad Gestora :'
@ 2, 2 SAY ' Unidad Ejecutora :'
@ 3, 2 SAY '          Funci¢n :'
@ 4, 2 SAY '         Programa :'
@ 5, 2 SAY '      SubPrograma :'
@ 6, 2 SAY '  Activ./Proyecto :'
@ 7, 2 SAY '       Componente :'
@ 8, 2 SAY '             Meta :'
@ 8, 40 SAY '        Finalidad :'
@ 9, 2 SAY '      Descripci¢n :'
@ 9, 40 SAY '         Fte.Fto. :'
RETURN
*
PROCEDURE vista
SELECT presu
SET FILTER TO
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo + m.uniges + m.unieje, ;
  'C')
@ 0, 22 SAY m.periodo
@ 0, 60 SAY m.codcad
@ 1, 22 SAY  ;
  val_para(maepre.uniges,'UNIGES', ;
  'V',22,40)
@ 2, 22 SAY  ;
  val_para1(maepre.unieje, ;
  'UNIEJE' + maepre.uniges,'V',22, ;
  40)
@ 3, 22 SAY  ;
  val_para(maepre.codfun,'CODFUN', ;
  'V',22,40)
@ 4, 22 SAY  ;
  val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'V',22, ;
  40)
@ 5, 22 SAY  ;
  val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'V',22, ;
  40)
@ 6, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 7, 22 SAY  ;
  val_para(maepre.codcom,'CODCOM', ;
  'V',22,40)
@ 8, 22 SAY maepre.codmet
@ 8, 60 SAY maepre.codfin
@ 9, 22 SAY SUBSTR(maepre.descri,  ;
  1, 23)
@ 9, 60 SAY val_para(m.codfte, ;
  'CODFTE','V',60,18)
DO vista_hijo
RETURN
*
PROCEDURE vista_hijo
SELECT itepar
SET ORDER TO ITEPAR1
SET FILTER TO
HIDE POPUP ALL
GOTO TOP
SEEK m.periodo + m.uniges +  ;
     m.unieje + m.codcad +  ;
     ALLTRIM(m.codfte)
IF FOUND()
     DO CASE
          CASE LEFT(maepre.actpry,  ;
               1) = '1'
               BROWSE NOOPTIMIZE  ;
                      FIELDS  ;
                      tippre :H =  ;
                      'T',  ;
                      generic :H =  ;
                      'G', sgn1  ;
                      :H = 'S1',  ;
                      sgn2 :H =  ;
                      'S2', espn1  ;
                      :H = 'E1',  ;
                      espn2 :H =  ;
                      'E2', espn3  ;
                      :H = 'E3',  ;
                      espn4 :H =  ;
                      'E4', pp =  ;
                      val_presu()  ;
                      :H =  ;
                      'Marco+/-'  ;
                      :P =  ;
                      '99,999,999'  ;
                      :F, valpart  ;
                      :H =  ;
                      'Asignaci¢n'  ;
                      :P =  ;
                      '99,999,999',  ;
                      ss =  ;
                      val_saldo()  ;
                      :H =  ;
                      'Saldo' :P =  ;
                      '99,999,999'  ;
                      :F, cresup  ;
                      :H =  ;
                      'Cr‚ditos  '  ;
                      :P =  ;
                      '99,999,999',  ;
                      tra001 :H =  ;
                      'Tra. Pliego'  ;
                      :P =  ;
                      '99,999,999',  ;
                      tra003 :H =  ;
                      'Cr. Pre-Anu'  ;
                      :P =  ;
                      '99,999,999',  ;
                      tra004 :H =  ;
                      'Cr. P/A UE'  ;
                      :P =  ;
                      '99,999,999',  ;
                      tra005 :H =  ;
                      'Cr. P/A UG'  ;
                      :P =  ;
                      '99,999,999'  ;
                      NOMENU  ;
                      NOAPPEND  ;
                      NODELETE  ;
                      NOCLEAR  ;
                      WINDOW  ;
                      wind_2 KEY  ;
                      m.periodo +  ;
                      m.uniges +  ;
                      m.unieje +  ;
                      m.codcad +  ;
                      ALLTRIM(m.codfte)  ;
                      TIMEOUT  ;
                      0.0001   ;
                      NOREFRESH
          CASE LEFT(maepre.actpry,  ;
               1) = '2'
               BROWSE NOOPTIMIZE  ;
                      FIELDS  ;
                      tippre :H =  ;
                      'T',  ;
                      generic :H =  ;
                      'G', sgn1  ;
                      :H = 'S1',  ;
                      sgn2 :H =  ;
                      'S2', espn1  ;
                      :H = 'E1',  ;
                      espn2 :H =  ;
                      'E2', espn3  ;
                      :H = 'E3',  ;
                      espn4 :H =  ;
                      'E4', pp =  ;
                      val_presu()  ;
                      :H =  ;
                      'Marco+/-'  ;
                      :P =  ;
                      '99,999,999'  ;
                      :F, valpart  ;
                      :H =  ;
                      'Asignaci¢n'  ;
                      :P =  ;
                      '99,999,999',  ;
                      ss =  ;
                      val_saldo()  ;
                      :H =  ;
                      'Saldo' :P =  ;
                      '99,999,999'  ;
                      :F, cresup  ;
                      :H =  ;
                      'Cr‚ditos  '  ;
                      :P =  ;
                      '99,999,999',  ;
                      tra001 :H =  ;
                      'Tra. Pliego'  ;
                      :P =  ;
                      '99,999,999',  ;
                      tra003 :H =  ;
                      'Cr. Pre-Anu'  ;
                      :P =  ;
                      '99,999,999',  ;
                      tra004 :H =  ;
                      'Cr. P/A UE'  ;
                      :P =  ;
                      '99,999,999',  ;
                      tra005 :H =  ;
                      'Cr. P/A UG'  ;
                      :P =  ;
                      '99,999,999',  ;
                      ubicac :H =  ;
                      'Ubicaci¢n'  ;
                      :W =  ;
                      itepar.valpart <>  ;
                      0, modeje  ;
                      :H = 'MEj'  ;
                      :W =  ;
                      valpart <>  ;
                      0, coddep  ;
                      :H =  ;
                      'UniEje' :V =  ;
                      val_para(coddep, ;
                      'CODDEP', ;
                      'coddep')  ;
                      :F :W =  ;
                      (ALLTRIM(m.tipfun) =  ;
                      'I') .AND.  ;
                      valpart <>  ;
                      0, metas :H =  ;
                      'Metas F¡sicas'  ;
                      :W =  ;
                      valpart <>  ;
                      0 NOMENU  ;
                      NOAPPEND  ;
                      NODELETE  ;
                      NOCLEAR  ;
                      WINDOW  ;
                      wind_2 KEY  ;
                      m.periodo +  ;
                      m.uniges +  ;
                      m.unieje +  ;
                      ALLTRIM(m.codcad) +  ;
                      ALLTRIM(m.codfte)  ;
                      TIMEOUT  ;
                      0.0001   ;
                      NOREFRESH
     ENDCASE
ELSE
     ACTIVATE WINDOW wind_2
     CLEAR
     @ 4, 25 SAY  ;
       'No hay asignaci¢n de presupuesto'
ENDIF
SELECT presu
RETURN
*
PROCEDURE vista_det
SELECT itepar
SET ORDER TO ITEPAR1
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
GOTO TOP
SEEK m.periodo + m.uniges +  ;
     m.unieje + ALLTRIM(m.codcad) +  ;
     ALLTRIM(m.codfte)
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            codpart :H =  ;
            'Partida', xx =  ;
            valasi1('ItePar', ;
            itepar.codpart,'8', ;
            'Descri','R') :H =  ;
            'Descripci¢n', pp =  ;
            val_presu() :H =  ;
            'Marco+/-' :P =  ;
            '99,999,999' :F,  ;
            valpart :H =  ;
            'Asignaci¢n' :P =  ;
            '99,999,999', cresup  ;
            :H = 'Cr‚ditos  ' :P =  ;
            '99,999,999', tra001  ;
            :H = 'Tra. Pliego' :P =  ;
            '99,999,999', tra003  ;
            :H = 'Cr. Pre-Anu' :P =  ;
            '99,999,999', tra004  ;
            :H = 'Cr. P/A UE' :P =  ;
            '99,999,999', tra005  ;
            :H = 'Cr. P/A UG' :P =  ;
            '99,999,999', m_01 :H =  ;
            'Ene.' :P =  ;
            '99,999,999', m_02 :H =  ;
            'Feb.' :P =  ;
            '99,999,999', m_03 :H =  ;
            'Mar.' :P =  ;
            '99,999,999', m_04 :H =  ;
            'Abr.' :P =  ;
            '99,999,999', m_05 :H =  ;
            'May.' :P =  ;
            '99,999,999', m_06 :H =  ;
            'Jun.' :P =  ;
            '99,999,999', m_07 :H =  ;
            'Jul.' :P =  ;
            '99,999,999', m_08 :H =  ;
            'Ago.' :P =  ;
            '99,999,999', m_09 :H =  ;
            'Set.' :P =  ;
            '99,999,999', m_10 :H =  ;
            'Oct.' :P =  ;
            '99,999,999', m_11 :H =  ;
            'Nov.' :P =  ;
            '99,999,999', m_12 :H =  ;
            'Dic.' :P =  ;
            '99,999,999', ss =  ;
            val_saldo() :H =  ;
            'Saldo' :P =  ;
            '99,999,999' :F LOCK  ;
            1 NOMENU NOAPPEND  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_2a KEY  ;
            m.periodo + m.uniges +  ;
            m.unieje +  ;
            ALLTRIM(m.codcad) +  ;
            ALLTRIM(m.codfte)  ;
            TITLE vtempo  ;
            NOREFRESH
ELSE
     ACTIVATE WINDOW wind_2a
     CLEAR
     @ 12, 25 SAY  ;
       'No hay asignaci¢n de presupuesto'
ENDIF
ON KEY LABEL F9 DO VISTA_DET
SHOW MENU mmenu
SELECT presu
DO vista
RETURN
*
PROCEDURE revis
SELECT presu
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SET RELATION TO periodo + uniges + unieje;
+ codcad + codfte INTO itepar
SET SKIP TO itepar
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE FIELDS presu.periodo :H =  ;
       'Per', presu.uniges :H =  ;
       'UNIGES', presu.unieje :H =  ;
       'UNIEJE', presu.codcad :H =  ;
       'CodCad', presu.codfte :H =  ;
       'Fte', itepar.tippre :H =  ;
       'T', itepar.generic :H =  ;
       'G', itepar.sgn1 :H = 'S1',  ;
       itepar.sgn2 :H = 'S2',  ;
       itepar.espn1 :H = 'E1',  ;
       itepar.espn2 :H = 'E2',  ;
       itepar.espn3 :H = 'E3',  ;
       itepar.espn4 :H = 'E4', xx =  ;
       valasi1('ItePar', ;
       itepar.codpart,'8', ;
       'Descri','R') :H =  ;
       'Descripci¢n',  ;
       itepar.valpart :H =  ;
       'Asignaci¢n' :P =  ;
       '99,999,999.99' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SET RELATION TO
SELECT presu
SET ORDER TO MaePar1
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
vcodfte = '  '
vcodcad = '    '
vuniges = '  '
vunieje = '   '
DEFINE WINDOW lista FROM 09, 12  ;
       TO 16, 68 FLOAT TITLE  ;
       ' °° B£squeda °° ' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW lista
@ 1, 2 SAY ' Periodo : ' GET  ;
  vperiodo PICTURE '!!'
@ 2, 2 SAY 'UNI.GES. : ' GET  ;
  vuniges PICTURE '!!' VALID  ;
  val_para(vuniges,'UNIGES',' ', ;
  14,30)
@ 3, 2 SAY 'UNI.EJE. : ' GET  ;
  vunieje PICTURE '!!!' VALID  ;
  val_para1(vunieje,'UNIEJE' +  ;
  vuniges,' ',14,30)
@ 4, 2 SAY '  Cadena : ' GET  ;
  vcodcad PICTURE '!!!!' VALID  ;
  val_codcad(vcodcad,vperiodo +  ;
  ALLTRIM(vuniges) +  ;
  ALLTRIM(vunieje),' ',14,30)
@ 5, 2 SAY '  Fuente : ' GET  ;
  vcodfte PICTURE '!!' VALID  ;
  val_para(vcodfte,'CODFTE',' ', ;
  14,30)
READ VALID val_read()
DEACTIVATE WINDOW lista
IF EMPTY(vperiodo) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SET ORDER TO MaePar1
     SEEK vperiodo +  ;
          ALLTRIM(vuniges) +  ;
          ALLTRIM(vunieje) +  ;
          ALLTRIM(vcodcad) +  ;
          ALLTRIM(vcodfte)
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
SELECT presu
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
SELECT presu
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
IF estado = '99'
     DO standby WITH vmens09
     RETURN
ENDIF
IF estado = '70'
     DO standby WITH vmens11
     RETURN
ENDIF
SELECT presu
SCATTER MEMVAR
DO pantalla
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo + m.uniges + m.unieje, ;
  'C')
@ 0, 22 GET m.periodo PICTURE  ;
  '!!' DISABLE
@ 0, 60 GET m.codcad PICTURE  ;
  '!!!!' DISABLE
@ 1, 22 SAY  ;
  val_para(maepre.uniges,'UNIGES', ;
  'V',22,40)
@ 2, 22 SAY  ;
  val_para1(maepre.unieje, ;
  'UNIEJE' + maepre.uniges,'V',22, ;
  40)
@ 3, 22 SAY  ;
  val_para(maepre.codfun,'CODFUN', ;
  'V',22,40)
@ 4, 22 SAY  ;
  val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'V',22, ;
  40)
@ 5, 22 SAY  ;
  val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'V',22, ;
  40)
@ 6, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 7, 22 SAY  ;
  val_para(maepre.codcom,'CODCOM', ;
  'V',22,40)
@ 8, 22 SAY maepre.codmet
@ 8, 60 SAY maepre.codfin
@ 9, 22 SAY SUBSTR(maepre.descri,  ;
  1, 23)
@ 9, 60 SAY val_para(m.codfte, ;
  'CODFTE','V',60,18,2)
IF LASTKEY() <> 27
     m.estfun = maepre.uniges +  ;
                maepre.unieje +  ;
                maepre.codfun +  ;
                maepre.codprg +  ;
                maepre.codspr +  ;
                maepre.actpry +  ;
                maepre.codcom +  ;
                maepre.codmet +  ;
                maepre.codfin
     ok = trabaja_hi()
     IF ok .AND. LASTKEY() <> 27
          SELECT presu
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT presu
DO vista
RETURN
*
PROCEDURE ingre
SELECT presu
op = ORDER()
vtemp = RECNO()
DO pantalla
vperiodo = PADL(ALLTRIM(STR(YEAR(DATE()) -  ;
           2000)), 2, '0')
STORE SPACE(2) TO vcodfte,  ;
      vuniges
STORE SPACE(3) TO vunieje
STORE SPACE(4) TO vcodcad
vtemp = RECNO()
@ 0, 22 GET vperiodo PICTURE '!!'
@ 1, 22 GET vuniges PICTURE '!!'  ;
  VALID val_para(vuniges,'UNIGES', ;
  ' ',22,40)
@ 2, 22 GET vunieje PICTURE '!!!'  ;
  VALID val_para1(vunieje, ;
  'UNIEJE' + vuniges,' ',22,40)
@ 0, 60 GET vcodcad PICTURE  ;
  '!!!!' VALID val_codcad(vcodcad, ;
  vperiodo + ALLTRIM(vuniges) +  ;
  ALLTRIM(vunieje),'C',60)
READ VALID val_read()
IF LASTKEY() = 27
     GOTO vtemp
     DO vista
     RETURN
ENDIF
@ 3, 22 SAY  ;
  val_para(maepre.codfun,'CODFUN', ;
  'V',22,40)
@ 4, 22 SAY  ;
  val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'V',22, ;
  40)
@ 5, 22 SAY  ;
  val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'V',22, ;
  40)
@ 6, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 7, 22 SAY  ;
  val_para(maepre.codcom,'CODCOM', ;
  'V',22,40)
@ 8, 22 SAY maepre.codmet
@ 8, 60 SAY maepre.codfin
@ 9, 22 SAY SUBSTR(maepre.descri,  ;
  1, 23)
@ 9, 60 GET vcodfte PICTURE '!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  ' ',60,18,3)
READ VALID val_read()
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
vkey = ALLTRIM(vperiodo) +  ;
       maepre.uniges +  ;
       maepre.unieje +  ;
       ALLTRIM(vcodcad) +  ;
       ALLTRIM(vcodfte)
SEEK vkey
IF  .NOT. FOUND()
     SCATTER BLANK MEMVAR
ELSE
     DO standby WITH  ;
        'Ya esta Registrado el Programa'
     DO vista
     RETURN
ENDIF
IF LASTKEY() <> 27
     m.estado = '00'
     m.fecemi = DATE()
     m.codfte = ALLTRIM(vcodfte)
     m.uniges = maepre.uniges
     m.unieje = maepre.unieje
     m.codcad = ALLTRIM(vcodcad)
     m.periodo = vperiodo
     m.estfun = maepre.uniges +  ;
                maepre.unieje +  ;
                maepre.codfun +  ;
                maepre.codprg +  ;
                maepre.codspr +  ;
                maepre.actpry +  ;
                maepre.codcom +  ;
                maepre.codmet +  ;
                maepre.codfin
     DO WHILE .T.
          ok = trabaja_hi()
          IF LASTKEY() <> 27  ;
             .AND. ok
               IF yesno( ;
                  '¨ Confirme el ingreso ?' ;
                  )
                    ok = .T.
                    EXIT
               ENDIF
          ELSE
               IF yesno( ;
                  '¨ Cancela el ingreso ?' ;
                  )
                    DO standby  ;
                       WITH  ;
                       ' Cancelado el Ingreso ..'
                    ok = .F.
                    EXIT
               ELSE
                    LOOP
               ENDIF
          ENDIF
     ENDDO
     IF ok .AND. LASTKEY() <> 27
          SELECT presu
          IF f_appd()
               GATHER MEMVAR
          ENDIF
     ELSE
          SELECT itepar
          SEEK m.periodo +  ;
               ALLTRIM(m.codcad) +  ;
               ALLTRIM(m.codfte)
          SCAN WHILE periodo =  ;
               m.periodo .AND.  ;
               codcad =  ;
               ALLTRIM(m.codcad)  ;
               .AND. codfte =  ;
               ALLTRIM(m.codfte)
               IF RLOCK()
                    DELETE NEXT 1
               ENDIF
          ENDSCAN
          GOTO vtemp
     ENDIF
ENDIF
UNLOCK ALL
SELECT presu
SET ORDER TO (op)
DO vista
RETURN
*
FUNCTION trabaja_hi
vsun = .T.
PUBLIC ak
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL f5 DO agreg_item
ON KEY LABEL f8 DO elimi_item
ON KEY LABEL f10 KEYBOARD CHR(23)
SELECT itepar
SET ORDER TO itepar1
SEEK m.periodo + m.uniges +  ;
     m.unieje + ALLTRIM(m.codcad) +  ;
     ALLTRIM(m.codfte)
IF  .NOT. FOUND()
     DO agreg_item
ENDIF
DO CASE
     CASE LEFT(maepre.actpry, 1) =  ;
          '1'
          BROWSE NOOPTIMIZE  ;
                 FIELDS tippre :H =  ;
                 'T' :V =  ;
                 valasi(tippre,'', ;
                 '1','TipPre', ;
                 'C') :F, generic  ;
                 :H = 'G' :V =  ;
                 valasi(generic, ;
                 tippre,'2', ;
                 'Generic','C')  ;
                 :F, sgn1 :H =  ;
                 'S1' :V =  ;
                 valasi(sgn1, ;
                 tippre + generic, ;
                 '3','SGN1','C')  ;
                 :F, sgn2 :H =  ;
                 'S2' :V =  ;
                 valasi(sgn2, ;
                 tippre + generic +  ;
                 sgn1,'4','SGN2', ;
                 'C') :F, espn1  ;
                 :H = 'E1' :V =  ;
                 valasi(espn1, ;
                 tippre + generic +  ;
                 sgn1 + sgn2,'5', ;
                 'EspN1','C') :F,  ;
                 espn2 :H = 'E2'  ;
                 :V =  ;
                 valasi(espn2, ;
                 tippre + generic +  ;
                 sgn1 + sgn2 +  ;
                 espn1,'6', ;
                 'EspN2','C') :F,  ;
                 espn3 :H = 'E3'  ;
                 :V =  ;
                 valasi(espn3, ;
                 tippre + generic +  ;
                 sgn1 + sgn2 +  ;
                 espn1 + espn2, ;
                 '7','EspN3','C')  ;
                 :F, espn4 :H =  ;
                 'E4' :V =  ;
                 valasi(espn4, ;
                 tippre + generic +  ;
                 sgn1 + sgn2 +  ;
                 espn1 + espn2 +  ;
                 espn3,'8', ;
                 'EspN4','C')  ;
                 .AND. chequeo()  ;
                 :F, valpart :H =  ;
                 'Asignaci¢n' :P =  ;
                 '99,999,999' :F,  ;
                 cresup :H =  ;
                 'Cr‚ditos  ' :P =  ;
                 '99,999,999' :R,  ;
                 m_01 :H = 'Ene.'  ;
                 :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_01')  ;
                 :F, m_02 :H =  ;
                 'Feb.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_02')  ;
                 :F, m_03 :H =  ;
                 'Mar.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_03')  ;
                 :F, m_04 :H =  ;
                 'Abr.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_04')  ;
                 :F, m_05 :H =  ;
                 'May.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_05')  ;
                 :F, m_06 :H =  ;
                 'Jun.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_06')  ;
                 :F, m_07 :H =  ;
                 'Jul.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_07')  ;
                 :F, m_08 :H =  ;
                 'Ago.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_08')  ;
                 :F, ss =  ;
                 val_saldo() :H =  ;
                 'Saldo' :P =  ;
                 '99,999,999' :F  ;
                 LOCK 4 NOMENU  ;
                 NOAPPEND  ;
                 NODELETE NOCLEAR  ;
                 WINDOW wind_2  ;
                 KEY m.periodo +  ;
                 m.uniges +  ;
                 m.unieje +  ;
                 ALLTRIM(m.codcad) +  ;
                 ALLTRIM(m.codfte)  ;
                 NOREFRESH
     CASE LEFT(maepre.actpry, 1) =  ;
          '2'
          BROWSE NOOPTIMIZE  ;
                 FIELDS tippre :H =  ;
                 'T' :V =  ;
                 valasi(tippre,'', ;
                 '1','TipPre', ;
                 'C') :F, generic  ;
                 :H = 'G' :V =  ;
                 valasi(generic, ;
                 tippre,'2', ;
                 'Generic','C')  ;
                 :F, sgn1 :H =  ;
                 'S1' :V =  ;
                 valasi(sgn1, ;
                 tippre + generic, ;
                 '3','SGN1','C')  ;
                 :F, sgn2 :H =  ;
                 'S2' :V =  ;
                 valasi(sgn2, ;
                 tippre + generic +  ;
                 sgn1,'4','SGN2', ;
                 'C') :F, espn1  ;
                 :H = 'E1' :V =  ;
                 valasi(espn1, ;
                 tippre + generic +  ;
                 sgn1 + sgn2,'5', ;
                 'EspN1','C') :F,  ;
                 espn2 :H = 'E2'  ;
                 :V =  ;
                 valasi(espn2, ;
                 tippre + generic +  ;
                 sgn1 + sgn2 +  ;
                 espn1,'6', ;
                 'EspN2','C') :F,  ;
                 espn3 :H = 'E3'  ;
                 :V =  ;
                 valasi(espn3, ;
                 tippre + generic +  ;
                 sgn1 + sgn2 +  ;
                 espn1 + espn2, ;
                 '7','EspN3','C')  ;
                 :F, espn4 :H =  ;
                 'E4' :V =  ;
                 valasi(espn4, ;
                 tippre + generic +  ;
                 sgn1 + sgn2 +  ;
                 espn1 + espn2 +  ;
                 espn3,'8', ;
                 'EspN4','C')  ;
                 .AND. chequeo()  ;
                 :F, valpart :H =  ;
                 'Asignaci¢n' :P =  ;
                 '99,999,999' :F,  ;
                 cresup :H =  ;
                 'Cr‚ditos  ' :P =  ;
                 '99,999,999' :R,  ;
                 m_01 :H = 'Ene.'  ;
                 :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_01')  ;
                 :F, m_02 :H =  ;
                 'Feb.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_02')  ;
                 :F, m_03 :H =  ;
                 'Mar.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_03')  ;
                 :F, m_04 :H =  ;
                 'Abr.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_04')  ;
                 :F, m_05 :H =  ;
                 'May.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_05')  ;
                 :F, m_06 :H =  ;
                 'Jun.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_06')  ;
                 :F, m_07 :H =  ;
                 'Jul.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_07')  ;
                 :F, m_08 :H =  ;
                 'Ago.' :P =  ;
                 '99,999,999' :V =  ;
                 chequeoa('M_08')  ;
                 :F, ss =  ;
                 val_saldo() :H =  ;
                 'Saldo' :P =  ;
                 '99,999,999' :F  ;
                 LOCK 4 NOMENU  ;
                 NOAPPEND  ;
                 NODELETE NOCLEAR  ;
                 WINDOW wind_2  ;
                 KEY m.periodo +  ;
                 m.uniges +  ;
                 m.unieje +  ;
                 ALLTRIM(m.codcad) +  ;
                 ALLTRIM(m.codfte)  ;
                 NOREFRESH
ENDCASE
IF LASTKEY() = 27
     vsun = .F.
ENDIF
STORE 0 TO vm_01, vm_02, vm_03,  ;
      vm_04, vm_05, vm_06, vm_07,  ;
      vm_08, vm_09, vm_10, vm_11,  ;
      vm_12
SELECT itepar
SEEK m.periodo + m.uniges +  ;
     m.unieje + ALLTRIM(m.codcad) +  ;
     ALLTRIM(m.codfte)
SCAN WHILE m.periodo = periodo  ;
     .AND. m.uniges + m.unieje +  ;
     ALLTRIM(m.codcad) = uniges +  ;
     unieje + codcad .AND.  ;
     ALLTRIM(m.codfte) = codfte
     IF EMPTY(codcad) .OR.  ;
        EMPTY(codfte) .OR.  ;
        EMPTY(codpart) .OR.  ;
        LEN(ALLTRIM(codpart)) <  ;
        6
          IF RLOCK()
               DELETE NEXT 1
          ENDIF
          UNLOCK
     ELSE
          REPLACE estfun WITH  ;
                  m.estfun
     ENDIF
     vm_01 = vm_01 + m_01
     vm_02 = vm_02 + m_02
     vm_03 = vm_03 + m_03
     vm_04 = vm_04 + m_04
     vm_05 = vm_05 + m_05
     vm_06 = vm_06 + m_06
     vm_07 = vm_07 + m_07
     vm_08 = vm_08 + m_08
     vm_09 = vm_09 + m_09
     vm_10 = vm_10 + m_10
     vm_11 = vm_11 + m_11
     vm_12 = vm_12 + m_12
ENDSCAN
m.estado = '00'
ACTIVATE WINDOW wind_3
@ 1, 1 SAY 'ENE : '
@ 2, 1 SAY 'FEB : '
@ 3, 1 SAY 'MAR : '
@ 4, 1 SAY 'ABR : '
@ 5, 1 SAY 'MAY : '
@ 6, 1 SAY 'JUN : '
@ 7, 1 SAY 'JUL : '
@ 8, 1 SAY 'AGO : '
@ 9, 1 SAY 'SET : '
@ 10, 1 SAY 'OCT : '
@ 11, 1 SAY 'NOV : '
@ 12, 1 SAY 'DIC : '
@ 1, 8 SAY vm_01 PICTURE  ;
  '99,999,999.99'
@ 2, 8 SAY vm_02 PICTURE  ;
  '99,999,999.99'
@ 3, 8 SAY vm_03 PICTURE  ;
  '99,999,999.99'
@ 4, 8 SAY vm_04 PICTURE  ;
  '99,999,999.99'
@ 5, 8 SAY vm_05 PICTURE  ;
  '99,999,999.99'
@ 6, 8 SAY vm_06 PICTURE  ;
  '99,999,999.99'
@ 7, 8 SAY vm_07 PICTURE  ;
  '99,999,999.99'
@ 8, 8 SAY vm_08 PICTURE  ;
  '99,999,999.99'
@ 9, 8 SAY vm_09 PICTURE  ;
  '99,999,999.99'
@ 10, 8 SAY vm_10 PICTURE  ;
  '99,999,999.99'
@ 11, 8 SAY vm_11 PICTURE  ;
  '99,999,999.99'
@ 12, 8 SAY vm_12 PICTURE  ;
  '99,999,999.99'
DO standby WITH  ;
   'Presione Tecla para Continuar',  ;
   19, 2
DEACTIVATE WINDOW wind_3
UNLOCK ALL
SET FILTER TO
SET ORDER TO 1
ON KEY LABEL f5
ON KEY LABEL f8
ON KEY LABEL f10
ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SELECT presu
RETURN vsun
*
PROCEDURE elimi
SELECT presu
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  .NOT. estado <> '  00'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ELIMINAR ‚ste Presupuesto ?' ;
           )
IF velimina .AND. f_lock(1)
     DELETE NEXT 1
     SELECT itepar
     SEEK m.periodo + m.uniges +  ;
          m.unieje +  ;
          ALLTRIM(m.codcad) +  ;
          ALLTRIM(m.codfte)
     IF FOUND()
          SCAN WHILE m.periodo =  ;
               itepar.periodo  ;
               .AND. m.uniges +  ;
               m.unieje +  ;
               ALLTRIM(m.codcad) =  ;
               itepar.uniges +  ;
               itepar.unieje +  ;
               itepar.codcad  ;
               .AND.  ;
               ALLTRIM(m.codfte) =  ;
               itepar.codfte
               IF f_lock(1)
                    DELETE NEXT 1
               ENDIF
          ENDSCAN
     ENDIF
     SELECT presu
     IF  .NOT. BOF()
          SKIP -1
     ELSE
          IF  .NOT. EOF()
               SKIP
          ENDIF
     ENDIF
ENDIF
UNLOCK ALL
DO vista
RETURN
*
PROCEDURE lista
DEFINE WINDOW lis_1 FROM 06, 26  ;
       TO 18, 55 FLOAT TITLE  ;
       ' Opciones ' DOUBLE COLOR  ;
       SCHEME 5
ACTIVATE WINDOW lis_1
opcion = 0
@ 1, 4 PROMPT  ;
  '  Consolidado  Anual  '  ;
  MESSAGE  ;
  'Presupuesto Autorizado Consolidado'
@ 2, 4 PROMPT  ;
  '      Anal¡tico       '  ;
  MESSAGE  ;
  'Presupuesto Anal¡tico'
@ 3, 4 PROMPT  ;
  '  Saldo Presupuestal  '  ;
  MESSAGE  ;
  'Lista Saldos Presupuestales a nivel anal¡tico '
@ 4, 4 PROMPT  ;
  '      Gen‚rico        '  ;
  MESSAGE  ;
  'Lista Saldos Presupuestales a nivel gen‚rico'
@ 6, 4 PROMPT  ;
  'Consolidado   Mensual '  ;
  MESSAGE  ;
  'Consolidado Mensualizado'
@ 7, 4 PROMPT  ;
  'Consolidado Trimestral'  ;
  MESSAGE  ;
  'Consolidado Trimestral'
@ 8, 4 PROMPT  ;
  ' Ejecuci¢n del Gasto  '  ;
  MESSAGE 'Ejecuci¢n'
MENU TO opcion
RELEASE WINDOW lis_1
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
USE IN 6 repopre ALIAS repo
SELECT repo
vdbf = SYS(3) + '.dbf'
COPY TO (vdbf) STRUCTURE
USE IN 6 EXCLUSIVE (vdbf) ALIAS  ;
    repo
SELECT repo
LIST STRUCTURE TO FILE xx
ZAP
PRIVATE vtexp
SELECT presu
ord = ORDER()
vtexp = RECNO()
STORE 0 TO vtotal, vtipo
STORE SPACE(2) TO vperiodo,  ;
      vcodfte, vcalend, vcodfun,  ;
      vuniges
STORE SPACE(3) TO vcodprg,  ;
      vunieje
STORE SPACE(4) TO vcodcad,  ;
      vcodspr
STORE SPACE(5) TO vcodcom,  ;
      vcodmet
STORE SPACE(6) TO vactpry
STORE SPACE(10) TO vcodpart
vperiodo = RIGHT(DTOC(DATE()), 2)
vforma = 'Partida'
DO CASE
     CASE opcion = 1
          DEFINE WINDOW lis_1  ;
                 FROM 4, 10 TO 20,  ;
                 70 FLOAT TITLE  ;
                 ' °°  Consolidado Presupuesto Anual  °° '  ;
                 DOUBLE COLOR  ;
                 SCHEME 5
          ACTIVATE WINDOW lis_1
          @ 0, 2 SAY  ;
            '     Periodo : ' GET  ;
            vperiodo PICTURE '!!'  ;
            VALID  .NOT.  ;
            EMPTY(vperiodo)
          @ 1, 2 SAY  ;
            '  Por Cadena : ' GET  ;
            vtotal SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 2, 2 SAY  ;
            '  Espec¡fico : ' GET  ;
            vtipo SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 3, 2 SAY  ;
            '  U. Gestora : ' GET  ;
            vuniges PICTURE '!!'  ;
            VALID  ;
            val_para(vuniges, ;
            'UNIGES',' ',18,30)
          @ 4, 2 SAY  ;
            'U. Ejecutora : ' GET  ;
            vunieje PICTURE '!!!'  ;
            VALID  ;
            val_para1(vunieje, ;
            'UNIEJE' + vuniges, ;
            ' ',18,30)
          @ 6, 2 SAY  ;
            'Cad. Funcion.: ' GET  ;
            vcodcad PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcad),  ;
            val_codcad(vcodcad, ;
            vperiodo +  ;
            ALLTRIM(vuniges) +  ;
            ALLTRIM(vunieje),' ', ;
            18,30), .T.) WHEN  ;
            vtotal = 1
          @ 8, 2 SAY  ;
            '     Funci¢n : ' GET  ;
            vcodfun PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfun),  ;
            val_para(vcodfun, ;
            'CODFUN',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 9, 2 SAY  ;
            '    Programa : ' GET  ;
            vcodprg PICTURE '!!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodprg),  ;
            val_para1(vcodprg, ;
            'CODPRG' + vcodfun, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 10, 2 SAY  ;
            ' SubPrograma : ' GET  ;
            vcodspr PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodspr),  ;
            val_para1(vcodspr, ;
            'CODSPR' + vcodprg, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 11, 2 SAY  ;
            'Activ/Proyec : ' GET  ;
            vactpry PICTURE  ;
            '!!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vactpry),  ;
            val_para(vactpry, ;
            'ACTPRY',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 12, 2 SAY  ;
            '  Componente : ' GET  ;
            vcodcom PICTURE  ;
            '!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcom),  ;
            val_para(vcodcom, ;
            'CODCOM',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 14, 2 SAY  ;
            '   Fte. Fto. : ' GET  ;
            vcodfte PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfte),  ;
            val_para(vcodfte, ;
            'CODFTE',' ',18,30),  ;
            .T.)
          READ VALID val_read()
          DEACTIVATE WINDOW lis_1
          IF LASTKEY() = 27
               DO vista
               RETURN
          ENDIF
          SELECT itepar
          IF EOF()
               DO standby WITH  ;
                  vmens08
          ELSE
               ACTIVATE WINDOW  ;
                        standby
               @ 1, 14 SAY  ;
                 'Espere un momento ...'  ;
                 COLOR W+/RB* 
               SELECT repo
               vind = SYS(3) +  ;
                      '.IDX'
               INDEX ON  ;
                     LEFT(estfun,  ;
                     5) + codcad +  ;
                     codfte +  ;
                     codpart TO  ;
                     (vind)
               SET INDEX TO (vind)
               IF vtotal = 1
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.)
               ELSE
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), SUBSTR(estfun,;
1, 2) = ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), SUBSTR(estfun,;
3, 3) = ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfun)), SUBSTR(estfun,;
6, 2) = ALLTRIM(vcodfun),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodprg)), SUBSTR(estfun,;
8, 3) = ALLTRIM(vcodprg),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodspr)), SUBSTR(estfun,;
11, 4) = ALLTRIM(vcodspr),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vactpry)), SUBSTR(estfun,;
15, 6) = ALLTRIM(vactpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcom)), SUBSTR(estfun,;
21, 5) = ALLTRIM(vcodcom),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.)
               ENDIF
               GOTO TOP
               SCAN
                    SCATTER MEMVAR
                    SELECT repo
                    SEEK LEFT(itepar.estfun,  ;
                         5) +  ;
                         itepar.codcad +  ;
                         itepar.codfte +  ;
                         itepar.codpart
                    vcod = 'FTE' +  ;
                           ALLTRIM(m.codfte)
                    m.transf = m.tra001 +  ;
                               m.tra003 +  ;
                               m.tra004 +  ;
                               m.tra005
                    m.totcal = 0
                    IF  .NOT.  ;
                        FOUND()
                         m.&vcod = m.valpart+m.cresup+m.transf
                         APPEND BLANK
                         GATHER MEMVAR
                         m.&vcod=0
                    ELSE
                         IF RLOCK()
                              REPLACE;
&vcod WITH &vcod + m.valpart+m.cresup+m.traNSF
                         ENDIF
                         UNLOCK
                         &vcod = 0
                    ENDIF
                    SELECT itepar
               ENDSCAN
               DEACTIVATE WINDOW  ;
                          standby
               SELECT repo
               GOTO TOP
               IF EOF()
                    DO standby  ;
                       WITH  ;
                       'No existe Registros para procesar'
               ELSE
                    IF vtotal = 1
                         IF vtipo =  ;
                            1
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LisPreA1',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ELSE
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LisPreA2',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ENDIF
                    ELSE
                         IF vtipo =  ;
                            1
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LISPRAG1',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ELSE
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LisPrAG2',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     CASE opcion = 2
          DEFINE WINDOW lis_1  ;
                 FROM 4, 10 TO 20,  ;
                 70 FLOAT TITLE  ;
                 ' °°   x Partidas Presupuestales     °° '  ;
                 DOUBLE COLOR  ;
                 SCHEME 5
          ACTIVATE WINDOW lis_1
          @ 0, 2 SAY  ;
            '     Periodo : ' GET  ;
            vperiodo PICTURE '!!'  ;
            VALID  .NOT.  ;
            EMPTY(vperiodo)
          @ 1, 2 SAY  ;
            '  Por Cadena : ' GET  ;
            vtotal SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 2, 2 SAY  ;
            '  Espec¡fico : ' GET  ;
            vtipo SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 4, 2 SAY  ;
            '  U. Gestora : ' GET  ;
            vuniges PICTURE '!!'  ;
            VALID  ;
            val_para(vuniges, ;
            'UNIGES',' ',18,30)
          @ 5, 2 SAY  ;
            'U. Ejecutora : ' GET  ;
            vunieje PICTURE '!!!'  ;
            VALID  ;
            val_para1(vunieje, ;
            'UNIEJE' + vuniges, ;
            ' ',18,30)
          @ 7, 2 SAY  ;
            'Cad. Funcion.: ' GET  ;
            vcodcad PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcad),  ;
            val_codcad(vcodcad, ;
            vperiodo +  ;
            ALLTRIM(vuniges) +  ;
            ALLTRIM(vunieje),' ', ;
            18,30), .T.) WHEN  ;
            vtotal = 1
          @ 9, 2 SAY  ;
            '     Funci¢n : ' GET  ;
            vcodfun PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfun),  ;
            val_para(vcodfun, ;
            'CODFUN',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 10, 2 SAY  ;
            '    Programa : ' GET  ;
            vcodprg PICTURE '!!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodprg),  ;
            val_para1(vcodprg, ;
            'CODPRG' + vcodfun, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 11, 2 SAY  ;
            ' SubPrograma : ' GET  ;
            vcodspr PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodspr),  ;
            val_para1(vcodspr, ;
            'CODSPR' + vcodprg, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 12, 2 SAY  ;
            'Activ/Proyec : ' GET  ;
            vactpry PICTURE  ;
            '!!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vactpry),  ;
            val_para(vactpry, ;
            'ACTPRY',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 13, 2 SAY  ;
            '  Componente : ' GET  ;
            vcodcom PICTURE  ;
            '!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcom),  ;
            val_para(vcodcom, ;
            'CODCOM',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          READ VALID val_read()
          DEACTIVATE WINDOW lis_1
          IF LASTKEY() = 27
               DO vista
               RETURN
          ENDIF
          SELECT itepar
          IF EOF()
               DO standby WITH  ;
                  vmens08
          ELSE
               ACTIVATE WINDOW  ;
                        standby
               @ 1, 14 SAY  ;
                 'Espere un momento ...'  ;
                 COLOR W+/RB* 
               SELECT repo
               vind = SYS(3) +  ;
                      '.IDX'
               INDEX ON  ;
                     LEFT(estfun,  ;
                     5) + codcad +  ;
                     codpart TO  ;
                     (vind)
               SET INDEX TO (vind)
               IF vtotal = 1
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.)
               ELSE
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), SUBSTR(estfun,;
1, 2) = ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), SUBSTR(estfun,;
3, 3) = ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfun)), SUBSTR(estfun,;
6, 2) = ALLTRIM(vcodfun),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodprg)), SUBSTR(estfun,;
8, 3) = ALLTRIM(vcodprg),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodspr)), SUBSTR(estfun,;
11, 4) = ALLTRIM(vcodspr),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vactpry)), SUBSTR(estfun,;
15, 6) = ALLTRIM(vactpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcom)), SUBSTR(estfun,;
21, 5) = ALLTRIM(vcodcom),;
.T.)
               ENDIF
               GOTO TOP
               SCAN
                    SCATTER MEMVAR
                    SELECT repo
                    SEEK LEFT(itepar.estfun,  ;
                         5) +  ;
                         itepar.codcad +  ;
                         itepar.codpart
                    vcod = 'FTE' +  ;
                           ALLTRIM(m.codfte)
                    m.transf = m.tra001 +  ;
                               m.tra003 +  ;
                               m.tra004 +  ;
                               m.tra005
                    m.totcal = 0
                    IF  .NOT.  ;
                        FOUND()
                         m.&vcod = m.valpart+m.cresup+m.transf
                         APPEND BLANK
                         GATHER MEMVAR
                         m.&vcod=0
                    ELSE
                         IF RLOCK()
                              REPLACE;
&vcod WITH &vcod + m.valpart+m.cresup+m.traNSF
                         ENDIF
                         UNLOCK
                         &vcod = 0
                    ENDIF
                    SELECT itepar
               ENDSCAN
               DEACTIVATE WINDOW  ;
                          standby
               SELECT repo
               GOTO TOP
               IF EOF()
                    DO standby  ;
                       WITH  ;
                       'No existe Registros para procesar'
               ELSE
                    IF vtotal = 1
                         IF vtipo =  ;
                            1
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LisPrCA1',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ELSE
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LisPrCA2',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ENDIF
                    ELSE
                         IF vtipo =  ;
                            1
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LISPRCG1',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ELSE
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LisPrCG2',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     CASE opcion = 3
          DEFINE WINDOW lis_1  ;
                 FROM 4, 10 TO 20,  ;
                 70 FLOAT TITLE  ;
                 ' °°  Saldo Presupuestal °° '  ;
                 DOUBLE COLOR  ;
                 SCHEME 5
          ACTIVATE WINDOW lis_1
          @ 0, 2 SAY  ;
            '     Periodo : ' GET  ;
            vperiodo PICTURE '!!'  ;
            VALID  .NOT.  ;
            EMPTY(vperiodo)
          @ 1, 2 SAY  ;
            '  Calendario : ' GET  ;
            vcalend PICTURE '!!'  ;
            VALID  ;
            val_para(vcalend, ;
            'FECMES',' ',18,25)
          @ 2, 2 SAY  ;
            '  Por Cadena : ' GET  ;
            vtotal SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 3, 2 SAY  ;
            '  Espec¡fico : ' GET  ;
            vtipo SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 4, 2 SAY  ;
            '  U. Gestora : ' GET  ;
            vuniges PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vuniges),  ;
            val_para(vuniges, ;
            'UNIGES',' ',18,30),  ;
            .T.)
          @ 5, 2 SAY  ;
            'U. Ejecutora : ' GET  ;
            vunieje PICTURE '!!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vunieje),  ;
            val_para1(vunieje, ;
            'UNIEJE' + vuniges, ;
            ' ',18,30), .T.)
          @ 6, 2 SAY  ;
            'Cad. Funcion.: ' GET  ;
            vcodcad PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcad),  ;
            val_codcad(vcodcad, ;
            vperiodo +  ;
            ALLTRIM(vuniges) +  ;
            ALLTRIM(vunieje),' ', ;
            18,30), .T.) WHEN  ;
            vtotal = 1
          @ 8, 2 SAY  ;
            '     Funci¢n : ' GET  ;
            vcodfun PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfun),  ;
            val_para(vcodfun, ;
            'CODFUN',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 9, 2 SAY  ;
            '    Programa : ' GET  ;
            vcodprg PICTURE '!!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodprg),  ;
            val_para1(vcodprg, ;
            'CODPRG' + vcodfun, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 10, 2 SAY  ;
            ' SubPrograma : ' GET  ;
            vcodspr PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodspr),  ;
            val_para1(vcodspr, ;
            'CODSPR' + vcodprg, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 11, 2 SAY  ;
            'Activ/Proyec : ' GET  ;
            vactpry PICTURE  ;
            '!!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vactpry),  ;
            val_para(vactpry, ;
            'ACTPRY',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 12, 2 SAY  ;
            '  Componente : ' GET  ;
            vcodcom PICTURE  ;
            '!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcom),  ;
            val_para(vcodcom, ;
            'CODCOM',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 14, 2 SAY  ;
            '   Fte. Fto. : ' GET  ;
            vcodfte PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfte),  ;
            val_para(vcodfte, ;
            'CODFTE',' ',18,30),  ;
            .T.)
          READ VALID val_read()
          DEACTIVATE WINDOW lis_1
          IF LASTKEY() = 27
               DO vista
               RETURN
          ENDIF
          SELECT itepar
          IF EOF()
               DO standby WITH  ;
                  vmens08
          ELSE
               ACTIVATE WINDOW  ;
                        standby
               @ 1, 14 SAY  ;
                 'Espere un momento ...'  ;
                 COLOR W+/RB* 
               SELECT repo
               vind = SYS(3) +  ;
                      '.IDX'
               INDEX ON  ;
                     LEFT(estfun,  ;
                     5) + codcad +  ;
                     codfte +  ;
                     codpart TO  ;
                     (vind)
               SET INDEX TO (vind)
               IF vtotal = 1
                    SELECT calen
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. nummes <= ALLTRIM(vcalend);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.)
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.)
               ELSE
                    SELECT calen
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), SUBSTR(estfun,;
1, 2) = ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), SUBSTR(estfun,;
3, 3) = ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfun)), SUBSTR(estfun,;
6, 2) = ALLTRIM(vcodfun),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodprg)), SUBSTR(estfun,;
8, 3) = ALLTRIM(vcodprg),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodspr)), SUBSTR(estfun,;
11, 4) = ALLTRIM(vcodspr),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vactpry)), SUBSTR(estfun,;
15, 6) = ALLTRIM(vactpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcom)), SUBSTR(estfun,;
21, 5) = ALLTRIM(vcodcom),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. nummes <= ALLTRIM(vcalend)
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), SUBSTR(estfun,;
1, 2) = ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), SUBSTR(estfun,;
3, 3) = ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfun)), SUBSTR(estfun,;
6, 2) = ALLTRIM(vcodfun),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodprg)), SUBSTR(estfun,;
8, 3) = ALLTRIM(vcodprg),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodspr)), SUBSTR(estfun,;
11, 4) = ALLTRIM(vcodspr),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vactpry)), SUBSTR(estfun,;
15, 6) = ALLTRIM(vactpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcom)), SUBSTR(estfun,;
21, 5) = ALLTRIM(vcodcom),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.)
               ENDIF
               GOTO TOP
               SCAN
                    SCATTER MEMVAR
                    m.cresup = getcre()
                    m.transf = gettra()
                    SELECT repo
                    SEEK LEFT(itepar.estfun,  ;
                         5) +  ;
                         itepar.codcad +  ;
                         itepar.codfte +  ;
                         itepar.codpart
                    vcod = 'FTE' +  ;
                           ALLTRIM(m.codfte)
                    m.totcal = 0
                    IF  .NOT.  ;
                        FOUND()
                         m.&vcod = m.valpart+m.cresup+m.transf
                         APPEND BLANK
                         GATHER MEMVAR
                         m.&vcod=0
                    ELSE
                         IF RLOCK()
                              REPLACE;
&vcod WITH &vcod + m.valpart+m.cresup+m.transf
                         ENDIF
                         UNLOCK
                         &vcod = 0
                    ENDIF
                    SELECT calen
                    vfilt = itepar.periodo +  ;
                            LEFT(itepar.estfun,  ;
                            5) +  ;
                            itepar.codcad +  ;
                            itepar.codfte +  ;
                            itepar.codpart
                    SEEK vfilt
                    IF FOUND()
                         vtotcal =  ;
                          0
                         vtotafe =  ;
                          0
                         vkey1 = periodo +  ;
                                 LEFT(estfun,  ;
                                 5) +  ;
                                 codcad +  ;
                                 codfte +  ;
                                 codpart
                         vkey2 = periodo +  ;
                                 LEFT(estfun,  ;
                                 5) +  ;
                                 codcad +  ;
                                 codfte +  ;
                                 codpart
                         DO WHILE  ;
                            vkey1= ;
                            vkey2  ;
                            .AND.   ;
                            .NOT.  ;
                            EOF()
                              vtotcal =  ;
                               vtotcal +  ;
                               valpart +  ;
                               ampliar
                              SKIP
                              vkey2 =  ;
                               periodo +  ;
                               LEFT(estfun,  ;
                               5) +  ;
                               codcad +  ;
                               codfte +  ;
                               codpart
                         ENDDO
                         SELECT repo
                         REPLACE totcal  ;
                                 WITH  ;
                                 totcal +  ;
                                 vtotcal
                         REPLACE totafe  ;
                                 WITH  ;
                                 totafe +  ;
                                 vtotafe
                    ENDIF
                    SELECT itepar
               ENDSCAN
               SELECT repo
               zind = SYS(3) +  ;
                      '.IDX'
               INDEX ON  ;
                     LEFT(estfun,  ;
                     30) + codfte +  ;
                     codpart TO  ;
                     (zind)
               SELECT itehc
               SET FILTER TO nummes <=;
ALLTRIM(vcalend);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpa), mespr <> nummes,;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpr), mespr = nummes,;
.T.)
               GOTO TOP
               SCAN
                    SELECT maepre
                    SEEK vperiodo +  ;
                         itehc.uniges +  ;
                         itehc.unieje +  ;
                         itehc.codcad
                    vkey = uniges +  ;
                           unieje +  ;
                           codfun +  ;
                           codprg +  ;
                           codspr +  ;
                           actpry +  ;
                           itehc.codcom +  ;
                           itehc.codmet +  ;
                           itehc.codfte +  ;
                           itehc.codpart
                    vkey1 = uniges +  ;
                            unieje +  ;
                            codfun +  ;
                            codprg +  ;
                            codspr +  ;
                            actpry +  ;
                            itehc.codcom +  ;
                            itehc.codmet +  ;
                            itehc.codfte
                    vmes = 'M_' +  ;
                           ALLTRIM(itehc.nummes)
                    SELECT repo
                    SEEK vkey
                    IF FOUND()
                         REPLACE totafe  ;
                                 WITH  ;
                                 totafe +  ;
                                 IIF(itehc.tipope =  ;
                                 '-',  ;
                                 itehc.valpart * - ;
                                 1,  ;
                                 itehc.valpart)
                    ELSE
                         GOTO TOP
                         LOCATE FOR  ;
                                LEFT(estfun,  ;
                                30) +  ;
                                codfte =  ;
                                vkey1
                         IF FOUND()
                              APPEND  ;
                               BLANK
                              REPLACE;
CODPART WITH ITEHC.CODPART, PERIODO WITH;
VPERIODO, CODCAD  WITH ITEHC.CODCAD, CODFTE;
 WITH ITEHC.CODFTE, ESTFUN  WITH LEFT(VKEY,30),;
&vmes   WITH IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
                         ENDIF
                    ENDIF
                    SELECT itehc
               ENDSCAN
               DEACTIVATE WINDOW  ;
                          standby
               SELECT repo
               GOTO TOP
               SCAN
                    vkey = periodo +  ;
                           LEFT(estfun,  ;
                           30) +  ;
                           codfte
                    SELECT itepar
                    SET ORDER TO ITEPAR4
                    SEEK vkey
                    IF FOUND()
                         REPLACE repo.codcad  ;
                                 WITH  ;
                                 itepar.codcad
                    ENDIF
                    SELECT repo
               ENDSCAN
               GOTO TOP
               IF EOF()
                    DO standby  ;
                       WITH  ;
                       'No existe Registros para procesar'
               ELSE
                    IF vtotal = 1
                         IF vtipo =  ;
                            1
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'SalPre1',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ELSE
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'SalPre2',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ENDIF
                    ELSE
                         IF vtipo =  ;
                            1
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'SalPreG1',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ELSE
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'SalPreG2',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     CASE opcion = 4
          DEFINE WINDOW lis_1  ;
                 FROM 4, 10 TO 20,  ;
                 70 FLOAT TITLE  ;
                 ' °°  Presupuesto Gen‚rico  °° '  ;
                 DOUBLE COLOR  ;
                 SCHEME 5
          ACTIVATE WINDOW lis_1
          @ 2, 2 SAY  ;
            '     Periodo : ' GET  ;
            vperiodo PICTURE '!!'  ;
            VALID  .NOT.  ;
            EMPTY(vperiodo)
          @ 4, 2 SAY  ;
            '  U. Gestora : ' GET  ;
            vuniges PICTURE '!!'  ;
            VALID  ;
            val_para(vuniges, ;
            'UNIGES',' ',18,30)
          @ 5, 2 SAY  ;
            'U. Ejecutora : ' GET  ;
            vunieje PICTURE '!!!'  ;
            VALID  ;
            val_para1(vunieje, ;
            'UNIEJE' + vuniges, ;
            ' ',18,30)
          @ 6, 2 SAY  ;
            '     Funci¢n : ' GET  ;
            vcodfun PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfun),  ;
            val_para(vcodfun, ;
            'CODFUN',' ',18,30),  ;
            .T.)
          @ 7, 2 SAY  ;
            '    Programa : ' GET  ;
            vcodprg PICTURE '!!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodprg),  ;
            val_para1(vcodprg, ;
            'CODPRG' + vcodfun, ;
            ' ',18,30), .T.)
          @ 8, 2 SAY  ;
            ' SubPrograma : ' GET  ;
            vcodspr PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodspr),  ;
            val_para1(vcodspr, ;
            'CODSPR' + vcodprg, ;
            ' ',18,30), .T.)
          @ 9, 2 SAY  ;
            'Activ/Proyec : ' GET  ;
            vactpry PICTURE  ;
            '!!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vactpry),  ;
            val_para(vactpry, ;
            'ACTPRY',' ',18,30),  ;
            .T.)
          @ 10, 2 SAY  ;
            '  Componente : ' GET  ;
            vcodcom PICTURE  ;
            '!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcom),  ;
            val_para(vcodcom, ;
            'CODCOM',' ',18,30),  ;
            .T.)
          @ 12, 2 SAY  ;
            '   Fte. Fto. : ' GET  ;
            vcodfte PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfte),  ;
            val_para(vcodfte, ;
            'CODFTE',' ',18,30),  ;
            .T.)
          READ VALID val_read()
          DEACTIVATE WINDOW lis_1
          IF LASTKEY() = 27
               DO vista
               RETURN
          ENDIF
          SELECT itepar
          IF EOF()
               DO standby WITH  ;
                  vmens08
          ELSE
               ACTIVATE WINDOW  ;
                        standby
               @ 1, 14 SAY  ;
                 'Espere un momento ...'  ;
                 COLOR W+/RB* 
               SELECT repo
               vind = SYS(3) +  ;
                      '.IDX'
               INDEX ON periodo +  ;
                     codcad +  ;
                     codfte +  ;
                     codpart TO  ;
                     (vind)
               SET INDEX TO (vind)
               SELECT itepar
               SET FILTER TO periodo =;
ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), SUBSTR(estfun,;
1, 2) = ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), SUBSTR(estfun,;
3, 3) = ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfun)), SUBSTR(estfun,;
6, 2) = ALLTRIM(vcodfun),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodprg)), SUBSTR(estfun,;
8, 3) = ALLTRIM(vcodprg),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodspr)), SUBSTR(estfun,;
11, 4) = ALLTRIM(vcodspr),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vactpry)), SUBSTR(estfun,;
15, 6) = ALLTRIM(vactpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcom)), SUBSTR(estfun,;
21, 5) = ALLTRIM(vcodcom),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.)
               GOTO TOP
               SCAN
                    SCATTER MEMVAR
                    SELECT repo
                    SEEK itepar.periodo +  ;
                         itepar.codcad +  ;
                         itepar.codfte +  ;
                         itepar.codpart
                    vcod = 'FTE' +  ;
                           ALLTRIM(m.codfte)
                    m.transf = m.tra001 +  ;
                               m.tra003 +  ;
                               m.tra004 +  ;
                               m.tra005
                    m.totcal = 0
                    IF  .NOT.  ;
                        FOUND()
                         m.&vcod = m.valpart+m.cresup+m.traNSF
                         APPEND BLANK
                         GATHER MEMVAR
                         m.&vcod=0
                    ELSE
                         IF RLOCK()
                              REPLACE;
&vcod WITH &vcod + m.valpart+m.cresup+m.traNSF
                         ENDIF
                         UNLOCK
                         &vcod = 0
                    ENDIF
                    SELECT itepar
               ENDSCAN
               DEACTIVATE WINDOW  ;
                          standby
               SELECT repo
               GOTO TOP
               IF EOF()
                    DO standby  ;
                       WITH  ;
                       'No existe Registros para procesar'
               ELSE
                    REPLACE valpart  ;
                            WITH  ;
                            fte00 +  ;
                            fte01 +  ;
                            fte09  ;
                            ALL
                    vind = SYS(3) +  ;
                           '.IDX'
                    INDEX ON  ;
                          periodo +  ;
                          LEFT(estfun,  ;
                          10) +  ;
                          codfte +  ;
                          codpart  ;
                          TO  ;
                          (vind)
                    GOTO TOP
                    DO reporte  ;
                       WITH 2,  ;
                       'LisPreg1',  ;
                       ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                       1, .F.,  ;
                       .T.
               ENDIF
          ENDIF
     CASE opcion = 5
          DEFINE WINDOW lis_1  ;
                 FROM 4, 10 TO 20,  ;
                 70 FLOAT TITLE  ;
                 ' °°  Consolidado Presupuesto Anual  °° '  ;
                 DOUBLE COLOR  ;
                 SCHEME 5
          ACTIVATE WINDOW lis_1
          @ 0, 2 SAY  ;
            '     Periodo : ' GET  ;
            vperiodo PICTURE '!!'  ;
            VALID  .NOT.  ;
            EMPTY(vperiodo)
          @ 1, 2 SAY  ;
            '  Por Cadena : ' GET  ;
            vtotal SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 2, 2 SAY  ;
            '  Espec¡fico : ' GET  ;
            vtipo SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 3, 2 SAY  ;
            '  U. Gestora : ' GET  ;
            vuniges PICTURE '!!'  ;
            VALID  ;
            val_para(vuniges, ;
            'UNIGES',' ',18,30)
          @ 4, 2 SAY  ;
            'U. Ejecutora : ' GET  ;
            vunieje PICTURE '!!!'  ;
            VALID  ;
            val_para1(vunieje, ;
            'UNIEJE' + vuniges, ;
            ' ',18,30)
          @ 6, 2 SAY  ;
            'Cad. Funcion.: ' GET  ;
            vcodcad PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcad),  ;
            val_codcad(vcodcad, ;
            vperiodo +  ;
            ALLTRIM(vuniges) +  ;
            ALLTRIM(vunieje),' ', ;
            18,30), .T.) WHEN  ;
            vtotal = 1
          @ 8, 2 SAY  ;
            '     Funci¢n : ' GET  ;
            vcodfun PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfun),  ;
            val_para(vcodfun, ;
            'CODFUN',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 9, 2 SAY  ;
            '    Programa : ' GET  ;
            vcodprg PICTURE '!!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodprg),  ;
            val_para1(vcodprg, ;
            'CODPRG' + vcodfun, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 10, 2 SAY  ;
            ' SubPrograma : ' GET  ;
            vcodspr PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodspr),  ;
            val_para1(vcodspr, ;
            'CODSPR' + vcodprg, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 11, 2 SAY  ;
            'Activ/Proyec : ' GET  ;
            vactpry PICTURE  ;
            '!!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vactpry),  ;
            val_para(vactpry, ;
            'ACTPRY',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 12, 2 SAY  ;
            '  Componente : ' GET  ;
            vcodcom PICTURE  ;
            '!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcom),  ;
            val_para(vcodcom, ;
            'CODCOM',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 14, 2 SAY  ;
            '   Fte. Fto. : ' GET  ;
            vcodfte PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfte),  ;
            val_para(vcodfte, ;
            'CODFTE',' ',18,30),  ;
            .T.)
          READ VALID val_read()
          DEACTIVATE WINDOW lis_1
          IF LASTKEY() = 27
               DO vista
               RETURN
          ENDIF
          SELECT itepar
          IF EOF()
               DO standby WITH  ;
                  vmens08
          ELSE
               ACTIVATE WINDOW  ;
                        standby
               @ 1, 14 SAY  ;
                 'Espere un momento ...'  ;
                 COLOR W+/RB* 
               SELECT repo
               vind = SYS(3) +  ;
                      '.IDX'
               INDEX ON  ;
                     LEFT(estfun,  ;
                     5) + codcad +  ;
                     codfte +  ;
                     codpart TO  ;
                     (vind)
               SET INDEX TO (vind)
               IF vtotal = 1
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.)
               ELSE
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), SUBSTR(estfun,;
1, 2) = ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), SUBSTR(estfun,;
3, 3) = ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfun)), SUBSTR(estfun,;
6, 2) = ALLTRIM(vcodfun),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodprg)), SUBSTR(estfun,;
8, 3) = ALLTRIM(vcodprg),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodspr)), SUBSTR(estfun,;
11, 4) = ALLTRIM(vcodspr),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vactpry)), SUBSTR(estfun,;
15, 6) = ALLTRIM(vactpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcom)), SUBSTR(estfun,;
21, 5) = ALLTRIM(vcodcom),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.)
               ENDIF
               GOTO TOP
               SCAN
                    SCATTER MEMVAR
                    SELECT repo
                    SEEK LEFT(itepar.estfun,  ;
                         5) +  ;
                         itepar.codcad +  ;
                         itepar.codfte +  ;
                         itepar.codpart
                    vcod = 'FTE' +  ;
                           ALLTRIM(m.codfte)
                    m.transf = m.tra001 +  ;
                               m.tra003 +  ;
                               m.tra004 +  ;
                               m.tra005
                    m.totcal = 0
                    IF  .NOT.  ;
                        FOUND()
                         m.&vcod = m.valpart+m.cresup+m.transf
                         APPEND BLANK
                         GATHER MEMVAR
                         m.&vcod=0
                    ELSE
                         IF RLOCK()
                              REPLACE;
&vcod WITH &vcod + m.valpart+m.cresup+m.traNSF
                         ENDIF
                         UNLOCK
                         &vcod = 0
                    ENDIF
                    SELECT itepar
               ENDSCAN
               DEACTIVATE WINDOW  ;
                          standby
               SELECT repo
               GOTO TOP
               IF EOF()
                    DO standby  ;
                       WITH  ;
                       'No existe Registros para procesar'
               ELSE
                    IF vtotal = 1
                         IF vtipo =  ;
                            1
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LisPrem1',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ELSE
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LisPrem2',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ENDIF
                    ELSE
                         IF vtipo =  ;
                            1
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LISPRMG1',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ELSE
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LisPrMG2',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     CASE opcion = 6
          vtrimes = ' '
          DEFINE WINDOW lis_1  ;
                 FROM 4, 10 TO 20,  ;
                 70 FLOAT TITLE  ;
                 ' °°  Consolidado Presupuesto Anual  °° '  ;
                 DOUBLE COLOR  ;
                 SCHEME 5
          ACTIVATE WINDOW lis_1
          @ 0, 2 SAY  ;
            '     Periodo : ' GET  ;
            vperiodo PICTURE '!!'  ;
            VALID  .NOT.  ;
            EMPTY(vperiodo)
          @ 1, 2 SAY  ;
            '  Por Cadena : ' GET  ;
            vtotal SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 2, 2 SAY  ;
            '  Espec¡fico : ' GET  ;
            vtipo SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 3, 2 SAY  ;
            '  U. Gestora : ' GET  ;
            vuniges PICTURE '!!'  ;
            VALID  ;
            val_para(vuniges, ;
            'UNIGES',' ',18,30)
          @ 4, 2 SAY  ;
            'U. Ejecutora : ' GET  ;
            vunieje PICTURE '!!!'  ;
            VALID  ;
            val_para1(vunieje, ;
            'UNIEJE' + vuniges, ;
            ' ',18,30)
          @ 6, 2 SAY  ;
            'Cad. Funcion.: ' GET  ;
            vcodcad PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcad),  ;
            val_codcad(vcodcad, ;
            vperiodo +  ;
            ALLTRIM(vuniges) +  ;
            ALLTRIM(vunieje),' ', ;
            18,30), .T.) WHEN  ;
            vtotal = 1
          @ 8, 2 SAY  ;
            '     Funci¢n : ' GET  ;
            vcodfun PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfun),  ;
            val_para(vcodfun, ;
            'CODFUN',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 9, 2 SAY  ;
            '    Programa : ' GET  ;
            vcodprg PICTURE '!!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodprg),  ;
            val_para1(vcodprg, ;
            'CODPRG' + vcodfun, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 10, 2 SAY  ;
            ' SubPrograma : ' GET  ;
            vcodspr PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodspr),  ;
            val_para1(vcodspr, ;
            'CODSPR' + vcodprg, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 11, 2 SAY  ;
            'Activ/Proyec : ' GET  ;
            vactpry PICTURE  ;
            '!!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vactpry),  ;
            val_para(vactpry, ;
            'ACTPRY',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 12, 2 SAY  ;
            '  Componente : ' GET  ;
            vcodcom PICTURE  ;
            '!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcom),  ;
            val_para(vcodcom, ;
            'CODCOM',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 13, 2 SAY  ;
            '   Fte. Fto. : ' GET  ;
            vcodfte PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfte),  ;
            val_para(vcodfte, ;
            'CODFTE',' ',18,30),  ;
            .T.)
          @ 14, 2 SAY  ;
            ' N§Trimestre : ' GET  ;
            vtrimes PICTURE  ;
            '@M 1,2,3,4,T'
          READ VALID val_read()
          DEACTIVATE WINDOW lis_1
          IF LASTKEY() = 27
               DO vista
               RETURN
          ENDIF
          SELECT itepar
          IF EOF()
               DO standby WITH  ;
                  vmens08
          ELSE
               ACTIVATE WINDOW  ;
                        standby
               @ 1, 14 SAY  ;
                 'Espere un momento ...'  ;
                 COLOR W+/RB* 
               USE IN 6
               USE IN 6 repcal1
               SELECT 6
               vind = SYS(3) +  ;
                      '.DBF'
               COPY TO (vind)  ;
                    STRUCTURE
               USE IN 6 EXCLUSIVE  ;
                   (vind) ALIAS  ;
                   repo
               SELECT repo
               ZAP
               vind = SYS(3) +  ;
                      '.IDX'
               INDEX ON  ;
                     LEFT(estfun,  ;
                     5) + codcad +  ;
                     codfte +  ;
                     codpart TO  ;
                     (vind)
               SET INDEX TO (vind)
               IF vtotal = 1
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.)
               ELSE
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), SUBSTR(estfun,;
1, 2) = ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), SUBSTR(estfun,;
3, 3) = ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfun)), SUBSTR(estfun,;
6, 2) = ALLTRIM(vcodfun),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodprg)), SUBSTR(estfun,;
8, 3) = ALLTRIM(vcodprg),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodspr)), SUBSTR(estfun,;
11, 4) = ALLTRIM(vcodspr),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vactpry)), SUBSTR(estfun,;
15, 6) = ALLTRIM(vactpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcom)), SUBSTR(estfun,;
21, 5) = ALLTRIM(vcodcom),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.)
               ENDIF
               GOTO TOP
               SCAN
                    SCATTER MEMVAR
                    SELECT repo
                    SEEK LEFT(itepar.estfun,  ;
                         5) +  ;
                         itepar.codcad +  ;
                         itepar.codfte +  ;
                         itepar.codpart
                    IF  .NOT.  ;
                        FOUND()
                         m.valpres =  ;
                          m.valpart +  ;
                          m.cresup +  ;
                          m.tra001 +  ;
                          m.tra003 +  ;
                          m.tra004 +  ;
                          m.tra005
                         APPEND BLANK
                         GATHER MEMVAR
                    ELSE
                         IF RLOCK()
                              REPLACE  ;
                               valpres  ;
                               WITH  ;
                               valpres +  ;
                               m.valpart +  ;
                               m.cresup +  ;
                               m.tra001 +  ;
                               m.tra003 +  ;
                               m.tra004 +  ;
                               m.tra005
                         ENDIF
                         UNLOCK
                    ENDIF
                    SELECT calen
                    vkey = itepar.periodo +  ;
                           itepar.uniges +  ;
                           itepar.unieje +  ;
                           itepar.codcad +  ;
                           itepar.codfte +  ;
                           itepar.codpart
                    vkey0 = itepar.periodo +  ;
                            itepar.uniges +  ;
                            itepar.unieje +  ;
                            itepar.codcad +  ;
                            itepar.codfte +  ;
                            itepar.codpart
                    SEEK vkey
                    IF FOUND()
                         DO WHILE  ;
                            vkey= ;
                            vkey0  ;
                            .AND.   ;
                            .NOT.  ;
                            EOF()
                              vtotcal =  ;
                               0
                              vkey1 =  ;
                               periodo +  ;
                               uniges +  ;
                               unieje +  ;
                               codcad +  ;
                               codfte +  ;
                               codpart +  ;
                               nummes
                              vkey2 =  ;
                               periodo +  ;
                               uniges +  ;
                               unieje +  ;
                               codcad +  ;
                               codfte +  ;
                               codpart +  ;
                               nummes
                              vcod1 =  ;
                               'C_' +  ;
                               ALLTRIM(nummes)
                              DO WHILE  ;
                                 vkey1= ;
                                 vkey2
                                   vtotcal = vtotcal + valpart + ampliar
                                   SKIP
                                   vkey2 = periodo + uniges + unieje + codcad + codfte + codpart + nummes
                              ENDDO
                              SELECT  ;
                               repo
                              REPLACE;
&vcod1 WITH &vcod1 + vTotCal
                              SELECT  ;
                               calen
                              vkey0 =  ;
                               periodo +  ;
                               uniges +  ;
                               unieje +  ;
                               codcad +  ;
                               codfte +  ;
                               codpart
                         ENDDO
                    ENDIF
                    SELECT itepar
               ENDSCAN
               DEACTIVATE WINDOW  ;
                          standby
               SELECT repo
               GOTO TOP
               IF EOF()
                    DO standby  ;
                       WITH  ;
                       'No existe Registros para procesar'
               ELSE
                    IF vtotal = 1
                         IF vtipo =  ;
                            1
                              IF vtrimes =  ;
                                 'T'
                                   DO reporte WITH 2, 'LisPrT1', ' Presupuesto a nivel Trimestral(01) '
                                   DO reporte WITH 2, 'LisPrT2', ' Presupuesto a nivel Trimestral(02) '
                                   DO reporte WITH 2, 'LisPrT3', ' Presupuesto a nivel Trimestral(03) '
                                   DO reporte WITH 2, 'LisPrT4', ' Presupuesto a nivel Trimestral(04) '
                              ELSE
                                   vrepo = 'LisPrt' + vtrimes
                                   DO reporte WITH 2, vrepo, ' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
                              ENDIF
                         ELSE
                              IF vtrimes =  ;
                                 'T'
                                   DO reporte WITH 2, 'LisPrT11', ' Presupuesto a nivel Trimestral(01) '
                                   DO reporte WITH 2, 'LisPrT22', ' Presupuesto a nivel Trimestral(02) '
                                   DO reporte WITH 2, 'LisPrT33', ' Presupuesto a nivel Trimestral(03) '
                                   DO reporte WITH 2, 'LisPrT44', ' Presupuesto a nivel Trimestral(04) '
                              ELSE
                                   vrepo = 'LisPrT' + vtrimes + vtrimes
                                   DO reporte WITH 2, vrepo, ' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
                              ENDIF
                         ENDIF
                    ELSE
                         IF vtipo =  ;
                            1
                              IF vtrimes =  ;
                                 'T'
                                   DO reporte WITH 2, 'LisPGT1', ' Presupuesto a nivel Trimestral(01) '
                                   DO reporte WITH 2, 'LisPGT2', ' Presupuesto a nivel Trimestral(02) '
                                   DO reporte WITH 2, 'LisPGT3', ' Presupuesto a nivel Trimestral(03) '
                                   DO reporte WITH 2, 'LisPGT4', ' Presupuesto a nivel Trimestral(04) '
                              ELSE
                                   vrepo = 'LisPGt' + vtrimes
                                   DO reporte WITH 2, vrepo, ' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
                              ENDIF
                         ELSE
                              IF vtrimes =  ;
                                 'T'
                                   DO reporte WITH 2, 'LisPGT11', ' Presupuesto a nivel Trimestral(01) '
                                   DO reporte WITH 2, 'LisPGT22', ' Presupuesto a nivel Trimestral(02) '
                                   DO reporte WITH 2, 'LisPGT33', ' Presupuesto a nivel Trimestral(03) '
                                   DO reporte WITH 2, 'LisPGT44', ' Presupuesto a nivel Trimestral(04) '
                              ELSE
                                   vrepo = 'LisPGT' + vtrimes + vtrimes
                                   DO reporte WITH 2, vrepo, ' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     CASE opcion = 7
          STORE 0 TO cm_01, cm_02,  ;
                cm_03, cm_04,  ;
                cm_05, cm_06,  ;
                cm_07, cm_08,  ;
                cm_09, cm_10,  ;
                cm_11, cm_12
          STORE 0 TO cgm_01,  ;
                cgm_02, cgm_03,  ;
                cgm_04, cgm_05,  ;
                cgm_06, cgm_07,  ;
                cgm_08, cgm_09,  ;
                cgm_10, cgm_11,  ;
                cgm_12
          STORE 0 TO sc, cvalpres,  ;
                ccresup, ctransf,  ;
                tceje
          STORE 0 TO scg,  ;
                cgvalpres,  ;
                cgcresup,  ;
                cgtransf, tcgeje
          STORE 0 TO cfm_01,  ;
                cfm_02, cfm_03,  ;
                cfm_04, cfm_05,  ;
                cfm_06, cfm_07,  ;
                cfm_08, cfm_09,  ;
                cfm_10, cfm_11,  ;
                cfm_12
          STORE 0 TO ggm_01,  ;
                ggm_02, ggm_03,  ;
                ggm_04, ggm_05,  ;
                ggm_06, ggm_07,  ;
                ggm_08, ggm_09,  ;
                ggm_10, ggm_11,  ;
                ggm_12
          STORE 0 TO scf,  ;
                cfvalpres,  ;
                cfcresup,  ;
                cftransf, tcfeje
          STORE 0 TO sgg,  ;
                ggvalpres,  ;
                ggcresup,  ;
                ggtransf, tggeje
          vtrimes = ' '
          ltrim = 1
          vmes1 = SPACE(2)
          vmes2 = SPACE(2)
          DEFINE WINDOW lis_1  ;
                 FROM 2, 10 TO 22,  ;
                 70 FLOAT TITLE  ;
                 ' °°  Consolidado Presupuesto Anual  °° '  ;
                 DOUBLE COLOR  ;
                 SCHEME 5
          ACTIVATE WINDOW lis_1
          @ 00, 02 SAY  ;
            '     Periodo : ' GET  ;
            vperiodo PICTURE '!!'  ;
            VALID  .NOT.  ;
            EMPTY(vperiodo)
          @ 01, 02 SAY  ;
            '  Trimestral : ' GET  ;
            ltrim SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 03, 02 SAY  ;
            ' N§Trimestre : ' GET  ;
            vtrimes PICTURE  ;
            '@M 1,2,3,4,T' WHEN  ;
            ltrim = 1
          @ 04, 02 SAY  ;
            ' Rang. Meses : ' GET  ;
            vmes1 PICTURE '!!'  ;
            VALID val_para(vmes1, ;
            'FECMES',' ',18,25)  ;
            WHEN ltrim = 2
          @ 04, 33 GET vmes2  ;
            PICTURE '!!' VALID  ;
            val_para(vmes2, ;
            'FECMES',' ',33,25)  ;
            WHEN ltrim = 2
          @ 06, 02 SAY  ;
            '  Por Cadena : ' GET  ;
            vtotal SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 07, 02 SAY  ;
            '  Espec¡fico : ' GET  ;
            vtipo SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 08, 02 SAY  ;
            '  U. Gestora : ' GET  ;
            vuniges PICTURE '!!'  ;
            VALID  ;
            val_para(vuniges, ;
            'UNIGES',' ',18,30)
          @ 09, 02 SAY  ;
            'U. Ejecutora : ' GET  ;
            vunieje PICTURE '!!!'  ;
            VALID  ;
            val_para1(vunieje, ;
            'UNIEJE' + vuniges, ;
            ' ',18,30)
          @ 10, 02 SAY  ;
            'Cad. Funcion.: ' GET  ;
            vcodcad PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcad),  ;
            val_codcad(vcodcad, ;
            vperiodo +  ;
            ALLTRIM(vuniges) +  ;
            ALLTRIM(vunieje),' ', ;
            18,30), .T.) WHEN  ;
            vtotal = 1
          @ 11, 02 SAY  ;
            '     Funci¢n : ' GET  ;
            vcodfun PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfun),  ;
            val_para(vcodfun, ;
            'CODFUN',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 12, 02 SAY  ;
            '    Programa : ' GET  ;
            vcodprg PICTURE '!!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodprg),  ;
            val_para1(vcodprg, ;
            'CODPRG' + vcodfun, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 13, 02 SAY  ;
            ' SubPrograma : ' GET  ;
            vcodspr PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodspr),  ;
            val_para1(vcodspr, ;
            'CODSPR' + vcodprg, ;
            ' ',18,30), .T.) WHEN  ;
            vtotal = 2
          @ 14, 02 SAY  ;
            'Activ/Proyec : ' GET  ;
            vactpry PICTURE  ;
            '!!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vactpry),  ;
            val_para(vactpry, ;
            'ACTPRY',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 15, 02 SAY  ;
            '  Componente : ' GET  ;
            vcodcom PICTURE  ;
            '!!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcom),  ;
            val_para(vcodcom, ;
            'CODCOM',' ',18,30),  ;
            .T.) WHEN vtotal = 2
          @ 16, 02 SAY  ;
            '   Fte. Fto. : ' GET  ;
            vcodfte PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfte),  ;
            val_para(vcodfte, ;
            'CODFTE',' ',18,30),  ;
            .T.)
          READ VALID val_read()
          DEACTIVATE WINDOW lis_1
          IF LASTKEY() = 27
               DO vista
               RETURN
          ENDIF
          SELECT itepar
          IF EOF()
               DO standby WITH  ;
                  vmens08
          ELSE
               ACTIVATE WINDOW  ;
                        standby
               @ 1, 14 SAY  ;
                 'Espere un momento ...'  ;
                 COLOR W+/RB* 
               USE IN 6
               USE IN 6 repcal1
               SELECT 6
               vind = SYS(3) +  ;
                      '.DBF'
               COPY TO (vind)  ;
                    STRUCTURE
               USE IN 6 EXCLUSIVE  ;
                   (vind) ALIAS  ;
                   repo
               SELECT repo
               ZAP
               vind = SYS(3) +  ;
                      '.IDX'
               INDEX ON  ;
                     LEFT(estfun,  ;
                     5) + codcad +  ;
                     codfte +  ;
                     codpart TO  ;
                     (vind)
               SET INDEX TO (vind)
               GOTO TOP
               IF vtotal = 1
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.)
               ELSE
                    SELECT itepar
                    SET FILTER TO periodo;
= ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), SUBSTR(estfun,;
1, 2) = ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), SUBSTR(estfun,;
3, 3) = ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfun)), SUBSTR(estfun,;
6, 2) = ALLTRIM(vcodfun),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodprg)), SUBSTR(estfun,;
8, 3) = ALLTRIM(vcodprg),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodspr)), SUBSTR(estfun,;
11, 4) = ALLTRIM(vcodspr),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vactpry)), SUBSTR(estfun,;
15, 6) = ALLTRIM(vactpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcom)), SUBSTR(estfun,;
21, 5) = ALLTRIM(vcodcom),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.)
               ENDIF
               GOTO TOP
               SCAN
                    SCATTER MEMVAR
                    m.metas = getcad()
                    m.cresup = getcre()
                    m.transf = gettra()
                    SELECT repo
                    SEEK LEFT(itepar.estfun,  ;
                         5) +  ;
                         itepar.codcad +  ;
                         itepar.codfte +  ;
                         itepar.codpart
                    STORE 0 TO  ;
                          m.m_01,  ;
                          m.m_02,  ;
                          m.m_03,  ;
                          m.m_04,  ;
                          m.m_05,  ;
                          m.m_06
                    STORE 0 TO  ;
                          m.m_07,  ;
                          m.m_08,  ;
                          m.m_09,  ;
                          m.m_10,  ;
                          m.m_11,  ;
                          m.m_12
                    IF  .NOT.  ;
                        FOUND()
                         m.valpres =  ;
                          m.valpart
                         APPEND BLANK
                         GATHER MEMVAR
                    ELSE
                         IF RLOCK()
                              REPLACE  ;
                               valpres  ;
                               WITH  ;
                               valpres +  ;
                               m.valpart
                              REPLACE  ;
                               cresup  ;
                               WITH  ;
                               cresup +  ;
                               m.cresup
                              REPLACE  ;
                               transf  ;
                               WITH  ;
                               cresup +  ;
                               m.transf
                         ENDIF
                         UNLOCK
                    ENDIF
                    SELECT calen
                    vkey = itepar.periodo +  ;
                           itepar.uniges +  ;
                           itepar.unieje +  ;
                           itepar.codcad +  ;
                           itepar.codfte +  ;
                           itepar.codpart
                    vkey0 = itepar.periodo +  ;
                            itepar.uniges +  ;
                            itepar.unieje +  ;
                            itepar.codcad +  ;
                            itepar.codfte +  ;
                            itepar.codpart
                    SEEK vkey
                    IF FOUND()
                         DO WHILE  ;
                            vkey= ;
                            vkey0  ;
                            .AND.   ;
                            .NOT.  ;
                            EOF()
                              vtotcal =  ;
                               0
                              vtotafe =  ;
                               0
                              vkey1 =  ;
                               periodo +  ;
                               uniges +  ;
                               unieje +  ;
                               codcad +  ;
                               codfte +  ;
                               codpart +  ;
                               nummes
                              vkey2 =  ;
                               periodo +  ;
                               uniges +  ;
                               unieje +  ;
                               codcad +  ;
                               codfte +  ;
                               codpart +  ;
                               nummes
                              vcod1 =  ;
                               'C_' +  ;
                               ALLTRIM(nummes)
                              DO WHILE  ;
                                 vkey1= ;
                                 vkey2
                                   vtotcal = vtotcal + valpart + ampliar
                                   SKIP
                                   vkey2 = periodo + uniges + unieje + codcad + codfte + codpart + nummes
                              ENDDO
                              SELECT  ;
                               repo
                              REPLACE;
&vcod1 WITH &vcod1 + vTotCal
                              SELECT  ;
                               calen
                              vkey0 =  ;
                               periodo +  ;
                               uniges +  ;
                               unieje +  ;
                               codcad +  ;
                               codfte +  ;
                               codpart
                         ENDDO
                    ENDIF
                    SELECT itepar
               ENDSCAN
               SELECT repo
               zind = SYS(3) +  ;
                      '.IDX'
               INDEX ON  ;
                     LEFT(estfun,  ;
                     30) + codfte +  ;
                     codpart TO  ;
                     (zind)
               SELECT itehc
               IF ltrim = 1
                    DO CASE
                         CASE vtrimes =  ;
                              '1'
                              SET FILTER;
TO nummes <= '03';
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpa), mespr <> nummes,;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpr), mespr = nummes,;
.T.)
                         CASE vtrimes =  ;
                              '2'
                              SET FILTER;
TO nummes <= '06';
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpa), mespr <> nummes,;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpr), mespr = nummes,;
.T.)
                         CASE vtrimes =  ;
                              '3'
                              SET FILTER;
TO nummes <= '09';
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpa), mespr <> nummes,;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpr), mespr = nummes,;
.T.)
                         OTHERWISE
                              SET FILTER;
TO IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpa), mespr <> nummes,;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpr), mespr = nummes,;
.T.)
                    ENDCASE
               ELSE
                    SET FILTER TO IIF(estado;
<> '92', BETWEEN(VAL(nummes), VAL(vmes1),;
VAL(vmes2)), BETWEEN(VAL(nummeshm), VAL(vmes1),;
VAL(vmes2)));
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. estado <> '99';
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpa), mespr <> nummes,;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpr), mespr = nummes,;
.T.)
               ENDIF
               GOTO TOP
               SCAN
                    SELECT maepre
                    SEEK vperiodo +  ;
                         itehc.uniges +  ;
                         itehc.unieje +  ;
                         itehc.codcad
                    vkey = uniges +  ;
                           unieje +  ;
                           codfun +  ;
                           codprg +  ;
                           codspr +  ;
                           actpry +  ;
                           itehc.codcom +  ;
                           itehc.codmet +  ;
                           itehc.codfte +  ;
                           itehc.codpart
                    vkey1 = uniges +  ;
                            unieje +  ;
                            codfun +  ;
                            codprg +  ;
                            codspr +  ;
                            actpry +  ;
                            itehc.codcom +  ;
                            itehc.codmet +  ;
                            itehc.codfte
                    vmes = IIF(itehc.estado <>  ;
                           '92',  ;
                           'M_' +  ;
                           ALLTRIM(itehc.nummes),  ;
                           'M_' +  ;
                           ALLTRIM(itehc.nummeshm))
                    IF VAL(itehc.nummeshm) >  ;
                       VAL(vmes2)
                         SELECT itehc
                         LOOP
                    ENDIF
                    SELECT repo
                    SEEK vkey
                    IF FOUND()
                         REPLACE &vmes;
WITH &vmes  + IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
                    ELSE
                         GOTO TOP
                         LOCATE FOR  ;
                                LEFT(estfun,  ;
                                30) +  ;
                                codfte =  ;
                                vkey1
                         IF FOUND()
                              APPEND  ;
                               BLANK
                              REPLACE;
CODPART WITH ITEHC.CODPART, PERIODO WITH;
VPERIODO, CODCAD  WITH ITEHC.CODCAD, CODFTE;
 WITH ITEHC.CODFTE, ESTFUN  WITH LEFT(VKEY,30),;
&vmes   WITH IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
                         ENDIF
                    ENDIF
                    SELECT itehc
               ENDSCAN
               DEACTIVATE WINDOW  ;
                          standby
               SELECT repo
               GOTO TOP
               SCAN
                    vkey = periodo +  ;
                           LEFT(estfun,  ;
                           30) +  ;
                           codfte
                    SELECT itepar
                    SET ORDER TO ITEPAR4
                    SEEK vkey
                    IF FOUND()
                         REPLACE repo.codcad  ;
                                 WITH  ;
                                 itepar.codcad
                    ENDIF
                    SELECT repo
               ENDSCAN
               a = VAL(vmes1)
               b = VAL(vmes2)
               cadcam = ''
               FOR i = a TO b
                    cadcam = cadcam +  ;
                             'M_' +  ;
                             PADL(ALLTRIM(STR(i)),  ;
                             2,  ;
                             '0') +  ;
                             '+'
               ENDFOR
               cadcam = LEFT(cadcam,  ;
                        LEN(cadcam) -  ;
                        1)
               SELECT repo
               zind1 = SYS(3) +  ;
                       '.IDX'
               INDEX ON codcad +  ;
                     codfte +  ;
                     codpart TO  ;
                     (zind1)
               GOTO TOP
               IF EOF()
                    DO standby  ;
                       WITH  ;
                       'No existe Registros para procesar'
               ELSE
                    IF ltrim = 1
                         IF vtotal =  ;
                            1
                              IF vtipo =  ;
                                 1
                                   IF vtrimes = 'T'
                                        DO reporte WITH 2, 'Liscal1', ' Presupuesto a nivel Trimestral(01) '
                                        DO reporte WITH 2, 'Liscal2', ' Presupuesto a nivel Trimestral(02) '
                                        DO reporte WITH 2, 'Liscal3', ' Presupuesto a nivel Trimestral(03) '
                                        DO reporte WITH 2, 'Liscal4', ' Presupuesto a nivel Trimestral(04) '
                                   ELSE
                                        vrepo = 'Liscal' + vtrimes
                                        DO reporte WITH 2, vrepo, ' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
                                   ENDIF
                              ELSE
                                   IF vtrimes = 'T'
                                        DO reporte WITH 2, 'LisCal11', ' Presupuesto a nivel Trimestral(01) '
                                        DO reporte WITH 2, 'LisCal22', ' Presupuesto a nivel Trimestral(02) '
                                        DO reporte WITH 2, 'LisCal33', ' Presupuesto a nivel Trimestral(03) '
                                        DO reporte WITH 2, 'LisCal44', ' Presupuesto a nivel Trimestral(04) '
                                   ELSE
                                        vrepo = 'LisCal' + vtrimes + vtrimes
                                        DO reporte WITH 2, vrepo, ' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
                                   ENDIF
                              ENDIF
                         ELSE
                              IF vtipo =  ;
                                 1
                                   IF vtrimes = 'T'
                                        DO reporte WITH 2, 'LisCaG1', ' Presupuesto a nivel Trimestral(01) '
                                        DO reporte WITH 2, 'LisCaG2', ' Presupuesto a nivel Trimestral(02) '
                                        DO reporte WITH 2, 'LisCaG3', ' Presupuesto a nivel Trimestral(03) '
                                        DO reporte WITH 2, 'LisCaG4', ' Presupuesto a nivel Trimestral(04) '
                                   ELSE
                                        vrepo = 'LisCaG' + vtrimes
                                        DO reporte WITH 2, vrepo, ' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
                                   ENDIF
                              ELSE
                                   IF vtrimes = 'T'
                                        DO reporte WITH 2, 'LisCaG11', ' Presupuesto a nivel Trimestral(01) '
                                        DO reporte WITH 2, 'LisCaG22', ' Presupuesto a nivel Trimestral(02) '
                                        DO reporte WITH 2, 'LisCaG33', ' Presupuesto a nivel Trimestral(03) '
                                        DO reporte WITH 2, 'LisCaG44', ' Presupuesto a nivel Trimestral(04) '
                                   ELSE
                                        vrepo = 'LisCaG' + vtrimes + vtrimes
                                        DO reporte WITH 2, vrepo, ' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
                                   ENDIF
                              ENDIF
                         ENDIF
                    ELSE
                         SELECT repo
                         SET RELATION;
TO periodo + uniges + unieje + codcad;
INTO maepre
                         vxls = 'EM' +  ;
                                vmes2 +  ;
                                vperiodo +  ;
                                vcodfte +  ;
                                '.Dbf'
                         COPY TO  ;
                              (vxls)
                         !COPY &vXls D:\XX
                         ERASE (vxls)
                         DEFINE WINDOW  ;
                                w_rep  ;
                                FROM  ;
                                7,  ;
                                10  ;
                                TO  ;
                                13,  ;
                                50  ;
                                FLOAT  ;
                                TITLE  ;
                                ' °°  Seleccionar Reporte  °° '  ;
                                DOUBLE  ;
                                COLOR  ;
                                SCHEME  ;
                                5
                         ACTIVATE  ;
                          WINDOW  ;
                          w_rep
                         vrep = 0
                         @ 02, 02  ;
                           GET  ;
                           vrep  ;
                           FUNCTION  ;
                           '^ Detallado;Nivel Especifico;Nivel Sub Generica'
                         READ
                         DEACTIVATE  ;
                          WINDOW  ;
                          w_rep
                         IF LASTKEY() <>  ;
                            27
                              DO CASE
                                   CASE vrep = 1
                                        DO reporte WITH 2, 'LisEje1', ' Presupuesto a nivel Ejecuciones ', 2, .F., .T.
                                   CASE vrep = 2
                                        DO reporte WITH 2, 'LisEje2', ' Presupuesto a nivel Ejecuciones ', 2, .F., .T.
                                   CASE vrep = 3
                                        DO reporte WITH 2, 'LisEje3', ' Presupuesto a nivel Ejecuciones ', 2, .F., .T.
                              ENDCASE
                         ENDIF
                         IF yesno( ;
                            'Desea Imprimir Consolidado' ;
                            )
                              zind1 =  ;
                               SYS(3) +  ;
                               '.IDX'
                              INDEX  ;
                               ON  ;
                               codfte +  ;
                               codpart  ;
                               TO  ;
                               (zind1)
                              GOTO  ;
                               TOP
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'LisEje1C',  ;
                                 ' Presupuesto a nivel Ejecuciones ',  ;
                                 2,  ;
                                 .F.,  ;
                                 .T.
                         ENDIF
                         SET RELATION;
TO
                    ENDIF
               ENDIF
               SET INDEX TO
               ERASE (zind)
               ERASE (zind1)
          ENDIF
ENDCASE
SELECT repo
USE
ERASE (vind)
ERASE (vdbf)
USE IN 6 EXCLUSIVE REPOPRE ALIAS  ;
    repo
SELECT presu
SET ORDER TO 1
GOTO vtexp
DO vista
RETURN
*
FUNCTION val_saldo
vsaldo1 = valpart + cresup +  ;
          tra001 + tra003 +  ;
          tra004 + tra005
vsaldo2 = m_01 + m_02 + m_03 +  ;
          m_04 + m_05 + m_06 +  ;
          m_07 + m_08 + m_09 +  ;
          m_10 + m_11 + m_12
vsaldo = vsaldo1 - vsaldo2
RETURN vsaldo
*
FUNCTION val_presu
vmarco = valpart + cresup +  ;
         tra001 + tra003 + tra004 +  ;
         tra005
RETURN vmarco
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
RELEASE WINDOW wind_2a
RELEASE WINDOW wind_3
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION des_par
PARAMETER xcpart
PRIVATE calias
calias = ALIAS()
SELECT catasi
IF SEEK(xcpart)
     IF detalle = 'S'
          mret = descri
     ELSE
          mret = 'Error en Detalle de Partida'
     ENDIF
ELSE
     mret = 'Error en Partida'
ENDIF
SELECT (calias)
RETURN mret
*
FUNCTION chequeo
ord = ORDER()
vc = RECNO()
vcodpart = tippre + generic +  ;
           sgn1 + sgn2 + espn1 +  ;
           espn2 + espn3 + espn4
vacpya = itepar.periodo +  ;
         itepar.uniges +  ;
         itepar.unieje +  ;
         itepar.codcad +  ;
         itepar.codfte +  ;
         vcodpart
SET ORDER TO itepar1
SEEK vacpya
IF FOUND() .AND. vnewing
     vubicac = itepar.ubicac
     vmodeje = itepar.modeje
     vcoddep = itepar.coddep
     vmeta = itepar.metas
     DO standby WITH  ;
        'Ya est  registrada esta partida '
     SET ORDER TO (ord)
     GOTO vc
     IF RLOCK()
          REPLACE ubicac WITH  ;
                  vubicac, modeje  ;
                  WITH vmodeje,  ;
                  coddep WITH  ;
                  vcoddep, metas  ;
                  WITH vmeta,  ;
                  codpart WITH  ;
                  vcodpart
     ENDIF
     UNLOCK
     vnewing = .F.
     RETURN .T.
ELSE
     SET ORDER TO (ord)
     GOTO vc
     REPLACE itepar.codpart WITH  ;
             vcodpart
     vnewing = .F.
ENDIF
SET ORDER TO (ord)
GOTO vc
RETURN .T.
*
FUNCTION chequeoa
PARAMETER vmes1
vtotmes = m_01 + m_02 + m_03 +  ;
          m_04 + m_05 + m_06 +  ;
          m_07 + m_08 + m_09 +  ;
          m_10 + m_11 + m_12
vvalpart = valpart + cresup +  ;
           tra001 + tra003 +  ;
           tra004 + tra005
IF vvalpart < vtotmes
     vsaldo = vtotmes - vvalpart
ENDIF
REPLACE tri_01 WITH m_01 + m_02 +  ;
        m_03
REPLACE tri_02 WITH m_04 + m_05 +  ;
        m_06
REPLACE tri_03 WITH m_07 + m_08 +  ;
        m_09
REPLACE tri_04 WITH m_10 + m_11 +  ;
        m_12
RETURN .T.
*
FUNCTION summef
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrecno = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro fte09 + fte13 TO  ;
         suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro fte09 +  ;
         fte13 TO suma
ENDIF
GOTO vrecno
RETURN suma
*
FUNCTION summef09
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrecno = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro fte09 TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro fte09  ;
         TO suma
ENDIF
GOTO vrecno
RETURN suma
*
FUNCTION summef13
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrecno = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro fte13 TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro fte13  ;
         TO suma
ENDIF
GOTO vrecno
RETURN suma
*
FUNCTION summef00
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrecno = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro fte00 TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro fte00  ;
         TO suma
ENDIF
GOTO vrecno
RETURN suma
*
FUNCTION sumpre2
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM FTE00+FTE01+FTE09 TO suma FOR &vFiltro=;
vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumcal1
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM totcal TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumafe1
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM totafe TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumsal1
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM (FTE00+FTE01+FTE09)-TOTCAL TO suma;
FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumcal
PARAMETER vfiltro, vnivel
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro totcal TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro totcal  ;
         TO suma
ENDIF
GOTO vrec
RETURN suma
*
FUNCTION sumafe
PARAMETER vfiltro, vnivel
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro totafe TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro totafe  ;
         TO suma
ENDIF
GOTO vrec
RETURN suma
*
FUNCTION sumsal
PARAMETER vfiltro, vnivel
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro (fte00 + fte01 +  ;
         fte09) - totcal TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro (fte00 +  ;
         fte01 + fte09) - totcal  ;
         TO suma
ENDIF
GOTO vrec
RETURN suma
*
FUNCTION sumpre1
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '0'
          vfiltro = 'LEFT(ESTFUN,5)'
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM FTE00+FTE01+FTE09+FTE13 TO suma FOR;
&vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION summes
PARAMETER vcalen, part, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '0'
          vfiltro = 'LEFT(ESTFUN,5)'
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM &part TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumar
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '0'
          vfiltro = 'LEFT(ESTFUN,5)'
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12;
 TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION salprg
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '0'
          vfiltro = 'LEFT(ESTFUN,5)'
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM FTE00+FTE01+FTE09+FTE13-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12);
TO sumA FOR &vFiltro = vCalen
GOTO vrec
RETURN suma
*
FUNCTION summes1
PARAMETER vfiltro, part
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
IF vtipo <= 25
     SUM &part TO suma FOR LEFT(ESTFUN,vtipo);
= vFiltro
ELSE
     SUM &part TO suma FOR LEFT(ESTFUN,25)+codfte;
= vFiltro
ENDIF
GOTO vrec
RETURN suma
*
FUNCTION sumar1
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
DO CASE
     CASE vtipo <= 25
          SUM FOR LEFT(estfun,  ;
              vtipo) = vfiltro  ;
              m_01 + m_02 + m_03 +  ;
              m_04 + m_05 + m_06 +  ;
              m_07 + m_08 + m_09 +  ;
              m_10 + m_11 + m_12  ;
              TO suma
     CASE vtipo > 25 .AND. vtipo <  ;
          28
          SUM FOR LEFT(estfun,  ;
              25) + codfte =  ;
              vfiltro m_01 + m_02 +  ;
              m_03 + m_04 + m_05 +  ;
              m_06 + m_07 + m_08 +  ;
              m_09 + m_10 + m_11 +  ;
              m_12 TO suma
     CASE vtipo > 28 .AND. vtipo <  ;
          30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              LEFT(codpart, 2) =  ;
              vfiltro m_01 + m_02 +  ;
              m_03 + m_04 + m_05 +  ;
              m_06 + m_07 + m_08 +  ;
              m_09 + m_10 + m_11 +  ;
              m_12 TO suma
     CASE vtipo > 30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              codpart = vfiltro  ;
              m_01 + m_02 + m_03 +  ;
              m_04 + m_05 + m_06 +  ;
              m_07 + m_08 + m_09 +  ;
              m_10 + m_11 + m_12  ;
              TO suma
ENDCASE
GOTO vrec
RETURN suma
*
FUNCTION salprg1
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
DO CASE
     CASE vtipo <= 25
          SUM FOR LEFT(estfun,  ;
              vtipo) = vfiltro  ;
              fte00 + fte01 +  ;
              fte09 - (m_01 +  ;
              m_02 + m_03 + m_04 +  ;
              m_05 + m_06 + m_07 +  ;
              m_08 + m_09 + m_10 +  ;
              m_11 + m_12) TO  ;
              suma
     CASE vtipo > 25 .AND. vtipo <  ;
          28
          SUM FOR LEFT(estfun,  ;
              25) + codfte =  ;
              vfiltro fte00 +  ;
              fte01 + fte09 -  ;
              (m_01 + m_02 + m_03 +  ;
              m_04 + m_05 + m_06 +  ;
              m_07 + m_08 + m_09 +  ;
              m_10 + m_11 + m_12)  ;
              TO suma
     CASE vtipo > 28 .AND. vtipo <  ;
          30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              LEFT(codpart, 2) =  ;
              vfiltro fte00 +  ;
              fte01 + fte09 -  ;
              (m_01 + m_02 + m_03 +  ;
              m_04 + m_05 + m_06 +  ;
              m_07 + m_08 + m_09 +  ;
              m_10 + m_11 + m_12)  ;
              TO suma
     CASE vtipo > 30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              codpart = vfiltro  ;
              fte00 + fte01 +  ;
              fte09 - (m_01 +  ;
              m_02 + m_03 + m_04 +  ;
              m_05 + m_06 + m_07 +  ;
              m_08 + m_09 + m_10 +  ;
              m_11 + m_12) TO  ;
              suma
ENDCASE
GOTO vrec
RETURN suma
*
FUNCTION sumpre
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
SUM VALPRES TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumar_prg
PARAMETER vcalen, part, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = 1
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = 2
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
SUM &part TO sumsg FOR &vFiltro= vCalen
GOTO vrec
RETURN sumsg
*
FUNCTION totalprg
PARAMETER vcalen, vnivel, vtrim
totalt = 0
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
vtipo = '1'
IF vtipo = '1'
     DO CASE
          CASE vtrim = '1'
               SUM  M_01+M_02+M_03 TO;
totalm FOR &vFiltro= vCalen
          CASE vtrim = '2'
               SUM  M_04+M_05+M_06 TO;
totalm FOR &vFiltro= vCalen
          CASE vtrim = '3'
               SUM  M_07+M_08+M_09 TO;
totalm FOR &vFiltro= vCalen
          CASE vtrim = '4'
               SUM  M_10+M_11+M_12 TO;
totalm FOR &vFiltro= vCalen
     ENDCASE
ELSE
     DO CASE
          CASE vtrim = '1'
               SUM C_01+C_02+C_03 TO totalm;
FOR &vFiltro= vCalen
          CASE vtrim = '2'
               SUM C_04+C_05+C_06 TO totalm;
FOR &vFiltro= vCalen
          CASE vtrim = '3'
               SUM C_07+C_08+C_09 TO totalm;
FOR &vFiltro= vCalen
          CASE vtrim = '4'
               SUM C_10+C_11+C_12 TO totalm;
FOR &vFiltro= vCalen
     ENDCASE
ENDIF
totalt = totalt + totalm
GOTO TOP
GOTO vrec
RETURN totalt
*
FUNCTION totalprg1
PARAMETER vcalen, vnivel, vtipo,  ;
          vtrim
totalt = 0
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
IF vtipo = '1'
     DO CASE
          CASE vtrim = '1'
               SUM  M_01+M_02+M_03 TO;
totalm FOR &vFiltro= vCalen
          CASE vtrim = '2'
               SUM  M_04+M_05+M_06 TO;
totalm FOR &vFiltro= vCalen
          CASE vtrim = '3'
               SUM  M_07+M_08+M_09 TO;
totalm FOR &vFiltro= vCalen
          CASE vtrim = '4'
               SUM  M_10+M_11+M_12 TO;
totalm FOR &vFiltro= vCalen
     ENDCASE
ELSE
     DO CASE
          CASE vtrim = '1'
               SUM C_01+C_02+C_03 TO totalm;
FOR &vFiltro= vCalen
          CASE vtrim = '2'
               SUM C_04+C_05+C_06 TO totalm;
FOR &vFiltro= vCalen
          CASE vtrim = '3'
               SUM C_07+C_08+C_09 TO totalm;
FOR &vFiltro= vCalen
          CASE vtrim = '4'
               SUM C_10+C_11+C_12 TO totalm;
FOR &vFiltro= vCalen
     ENDCASE
ENDIF
totalt = totalt + totalm
GOTO TOP
GOTO vrec
RETURN totalt
*
FUNCTION summef1
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrecno = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro valpres TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro valpres  ;
         TO suma
ENDIF
GOTO vrecno
RETURN suma
*
FUNCTION getcad
PRIVATE calias
calias = ALIAS()
SELECT maepre
SEEK itepar.periodo +  ;
     itepar.uniges +  ;
     itepar.unieje +  ;
     itepar.codcad
IF FOUND()
     mret = descri
ENDIF
SELECT (calias)
RETURN mret
*
FUNCTION sumcad
PARAMETER xcadena
PRIVATE nreg
nreg = RECNO()
cvalpres = 0
ccresup = 0
ctransf = 0
cm_01 = 0
cm_02 = 0
cm_03 = 0
cm_04 = 0
cm_05 = 0
cm_06 = 0
cm_07 = 0
cm_08 = 0
cm_09 = 0
cm_10 = 0
cm_11 = 0
cm_12 = 0
SCAN WHILE codcad = xcadena
     cvalpres = cvalpres +  ;
                valpres
     ccresup = ccresup + cresup
     ctransf = ctransf + transf
     cm_01 = cm_01 + m_01
     cm_02 = cm_02 + m_02
     cm_03 = cm_03 + m_03
     cm_04 = cm_04 + m_04
     cm_05 = cm_05 + m_05
     cm_06 = cm_06 + m_06
     cm_07 = cm_07 + m_07
     cm_08 = cm_08 + m_08
     cm_09 = cm_09 + m_09
     cm_10 = cm_10 + m_10
     cm_11 = cm_11 + m_11
     cm_12 = cm_12 + m_12
ENDSCAN
GOTO nreg
tceje = cm_01 + cm_02 + cm_03 +  ;
        cm_04 + cm_05 + cm_06 +  ;
        cm_07 + cm_08 + cm_09 +  ;
        cm_10 + cm_11 + cm_12
sc = cvalpres + ccresup + ctransf -  ;
     tceje
RETURN 0
*
FUNCTION sumcadfte
PARAMETER xcadena, xfte
PRIVATE nreg
nreg = RECNO()
cfvalpres = 0
cfcresup = 0
cftransf = 0
cfm_01 = 0
cfm_02 = 0
cfm_03 = 0
cfm_04 = 0
cfm_05 = 0
cfm_06 = 0
cfm_07 = 0
cfm_08 = 0
cfm_09 = 0
cfm_10 = 0
cfm_11 = 0
cfm_12 = 0
SCAN WHILE codcad + codfte =  ;
     xcadena + xfte
     cfvalpres = cfvalpres +  ;
                 valpres
     cfcresup = cfcresup + cresup
     cftransf = cftransf + transf
     cfm_01 = cfm_01 + m_01
     cfm_02 = cfm_02 + m_02
     cfm_03 = cfm_03 + m_03
     cfm_04 = cfm_04 + m_04
     cfm_05 = cfm_05 + m_05
     cfm_06 = cfm_06 + m_06
     cfm_07 = cfm_07 + m_07
     cfm_08 = cfm_08 + m_08
     cfm_09 = cfm_09 + m_09
     cfm_10 = cfm_10 + m_10
     cfm_11 = cfm_11 + m_11
     cfm_12 = cfm_12 + m_12
ENDSCAN
GOTO nreg
tcfeje = cfm_01 + cfm_02 + cfm_03 +  ;
         cfm_04 + cfm_05 + cfm_06 +  ;
         cfm_07 + cfm_08 + cfm_09 +  ;
         cfm_10 + cfm_11 +  ;
         cfm_12
scf = cfvalpres + cfcresup +  ;
      cftransf - tcfeje
RETURN 0
*
FUNCTION sumcg
PARAMETER xcg
PRIVATE nreg
nreg = RECNO()
cgvalpres = 0
cgcresup = 0
cgtransf = 0
cgm_01 = 0
cgm_02 = 0
cgm_03 = 0
cgm_04 = 0
cgm_05 = 0
cgm_06 = 0
cgm_07 = 0
cgm_08 = 0
cgm_09 = 0
cgm_10 = 0
cgm_11 = 0
cgm_12 = 0
SCAN WHILE codpart = xcg
     cgvalpres = cgvalpres +  ;
                 valpres
     cgcresup = cgcresup + cresup
     cgtransf = cgtransf + transf
     cgm_01 = cgm_01 + m_01
     cgm_02 = cgm_02 + m_02
     cgm_03 = cgm_03 + m_03
     cgm_04 = cgm_04 + m_04
     cgm_05 = cgm_05 + m_05
     cgm_06 = cgm_06 + m_06
     cgm_07 = cgm_07 + m_07
     cgm_08 = cgm_08 + m_08
     cgm_09 = cgm_09 + m_09
     cgm_10 = cgm_10 + m_10
     cgm_11 = cgm_11 + m_11
     cgm_12 = cgm_12 + m_12
ENDSCAN
GOTO nreg
tcgeje = cgm_01 + cgm_02 + cgm_03 +  ;
         cgm_04 + cgm_05 + cgm_06 +  ;
         cgm_07 + cgm_08 + cgm_09 +  ;
         cgm_10 + cgm_11 +  ;
         cgm_12
scg = cgvalpres + cgcresup +  ;
      cgtransf - tcgeje
RETURN 0
*
FUNCTION sumcg_gg
PARAMETER xcg, xgg
PRIVATE nreg
nreg = RECNO()
ggvalpres = 0
ggcresup = 0
ggtransf = 0
ggm_01 = 0
ggm_02 = 0
ggm_03 = 0
ggm_04 = 0
ggm_05 = 0
ggm_06 = 0
ggm_07 = 0
ggm_08 = 0
ggm_09 = 0
ggm_10 = 0
ggm_11 = 0
ggm_12 = 0
SCAN WHILE codpart = xcg + xgg
     ggvalpres = ggvalpres +  ;
                 valpres
     ggcresup = ggcresup + cresup
     ggtransf = ggtransf + transf
     ggm_01 = ggm_01 + m_01
     ggm_02 = ggm_02 + m_02
     ggm_03 = ggm_03 + m_03
     ggm_04 = ggm_04 + m_04
     ggm_05 = ggm_05 + m_05
     ggm_06 = ggm_06 + m_06
     ggm_07 = ggm_07 + m_07
     ggm_08 = ggm_08 + m_08
     ggm_09 = ggm_09 + m_09
     ggm_10 = ggm_10 + m_10
     ggm_11 = ggm_11 + m_11
     ggm_12 = ggm_12 + m_12
ENDSCAN
GOTO nreg
tggeje = ggm_01 + ggm_02 + ggm_03 +  ;
         ggm_04 + ggm_05 + ggm_06 +  ;
         ggm_07 + ggm_08 + ggm_09 +  ;
         ggm_10 + ggm_11 +  ;
         ggm_12
sgg = ggvalpres + ggcresup +  ;
      ggtransf - tggeje
RETURN 0
*
FUNCTION sumcre
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM CRESUP TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumtra
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM TRANSF TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
PROCEDURE asign_tri
valias = ALIAS()
vorder = ORDER()
vrecno = RECNO()
SET EXCLUSIVE OFF
IF yesno( ;
   '¨ Seguro de realizar la asignaci¢n trimestral ?' ;
   )
     ACTIVATE WINDOW standby
     @ 1, 2 SAY  ;
       ' Realizando Proceso de Asignaci¢n trimestral '  ;
       COLOR W+/RB* 
     SELECT itepar
     GOTO TOP
     SCAN
          SCATTER MEMVAR
          REPLACE tri_01 WITH  ;
                  m.m_01 + m.m_02 +  ;
                  m.m_03, tri_02  ;
                  WITH m.m_04 +  ;
                  m.m_05 + m.m_06,  ;
                  tri_03 WITH  ;
                  m.m_07 + m.m_08 +  ;
                  m.m_09, tri_04  ;
                  WITH m.m_10 +  ;
                  m.m_11 +  ;
                  m.m_12
     ENDSCAN
     RELEASE WINDOW standby
ENDIF
SELECT (valias)
SET ORDER TO (vorder)
GOTO vrecno
SET EXCLUSIVE ON
DO vista
RETURN
*
FUNCTION agreg_item
SELECT itepar
IF f_appd()
     REPLACE periodo WITH  ;
             m.periodo, uniges  ;
             WITH m.uniges,  ;
             unieje WITH m.unieje,  ;
             codcad WITH  ;
             ALLTRIM(m.codcad),  ;
             codfte WITH  ;
             ALLTRIM(m.codfte)
     vnewing = .T.
     RETURN .T.
ENDIF
RETURN .F.
*
PROCEDURE elimi_item
SELECT itepar
IF RLOCK()
     DELETE NEXT 1
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
UNLOCK
RETURN
*

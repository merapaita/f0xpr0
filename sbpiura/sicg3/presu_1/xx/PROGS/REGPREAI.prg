CLOSE DATABASES
USE IN 1 parmae ALIAS parma ORDER  ;
    parmae1
USE IN 2 maeparI ALIAS presu  ;
    ORDER maeparI1
USE IN 3 iteparI ALIAS itepar  ;
    ORDER iteparI1
USE IN 5 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 7 Calen ALIAS calen ORDER  ;
    Calen2
USE IN 8 recing ALIAS recing  ;
    ORDER recing1
USE IN 9 Iteri ALIAS iteri ORDER  ;
    Iteri1
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
USE IN 13 INGRESO ALIAS ingr  ;
    ORDER Ingreso1
USE IN 14 CatAsi ALIAS catasi  ;
    ORDER CatAsi3
PUBLIC vcodsub, vcodact, vproyec,  ;
       vsubpry, vcalend, vnewing,  ;
       mdescr
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
*
PROCEDURE vista_hijo
SELECT itepar
SET ORDER TO ITEPARI1
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
                      itepar.codpart  ;
                      :H =  ;
                      'Partida',  ;
                      xx =  ;
                      valasi1('ItePar', ;
                      codpart,'8', ;
                      'Descri', ;
                      'R') : 40  ;
                      :H =  ;
                      'Descripci¢n',  ;
                      valpart :H =  ;
                      'Asignaci¢n'  ;
                      :P =  ;
                      '99,999,999',  ;
                      meseje :H =  ;
                      'Mes',  ;
                      numcre :H =  ;
                      'Cr‚dito',  ;
                      numtra :H =  ;
                      'Transf.'  ;
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
                      itepar.codpart  ;
                      :H =  ;
                      'Partida',  ;
                      xx =  ;
                      valasi1('ItePar', ;
                      codpart,'8', ;
                      'Descri', ;
                      'R') : 40  ;
                      :H =  ;
                      'Descripci¢n',  ;
                      valpart :H =  ;
                      'Asignaci¢n'  ;
                      :P =  ;
                      '99,999,999',  ;
                      meseje :H =  ;
                      'Mes',  ;
                      numcre :H =  ;
                      'Cr‚dito',  ;
                      numtra :H =  ;
                      'Transf.',  ;
                      modeje :H =  ;
                      'MEj' :W =  ;
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
SET ORDER TO ITEPARI1
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
GOTO TOP
SEEK m.periodo + m.uniges +  ;
     m.unieje + ALLTRIM(m.codcad) +  ;
     ALLTRIM(m.codfte)
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            itepar.codpart :H =  ;
            'Partida', xx =  ;
            valasi1('ItePar', ;
            codpart,'8','Descri', ;
            'R') : 40 :H =  ;
            'Descripci¢n',  ;
            valpart :H =  ;
            'Asignaci¢n' :P =  ;
            '99,999,999', meseje  ;
            :H = 'Mes', numcre :H =  ;
            'Cr‚dito', numtra :H =  ;
            'Transf.' LOCK 1  ;
            NOMENU NOAPPEND  ;
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
       'Fte', itepar.codpart :H =  ;
       'Partida', itepar.codpart  ;
       :H = 'Partida', xx =  ;
       valasi1('Repo', ;
       itepar.codpart,'8', ;
       'Descri','R') : 40 :H =  ;
       'Descripci¢n' NOMENU  ;
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
SET ORDER TO 1
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
     SET ORDER TO 1
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
IF estado = '50'
     DO standby WITH vmens12
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
STORE SPACE(2) TO vcodfte,  ;
      vuniges
STORE SPACE(3) TO vunieje
STORE SPACE(4) TO vcodcad
vperiodo = RIGHT(DTOC(DATE()), 2)
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
PROCEDURE trabaja_hi
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
SET ORDER TO iteparI1
SEEK m.periodo + m.uniges +  ;
     m.unieje + ALLTRIM(m.codcad) +  ;
     ALLTRIM(m.codfte)
IF  .NOT. FOUND()
     DO agreg_item
ENDIF
BROWSE NOOPTIMIZE FIELDS tippre  ;
       :H = 'T' :V =  ;
       valasi(tippre,'','1', ;
       'TipPre','C') :F, generic  ;
       :H = 'G' :V =  ;
       valasi(generic,tippre,'2', ;
       'Generic','C') :F, sgn1 :H =  ;
       'S1' :V = valasi(sgn1, ;
       tippre + generic,'3', ;
       'SGN1','C') :F, sgn2 :H =  ;
       'S2' :V = valasi(sgn2, ;
       tippre + generic + sgn1, ;
       '4','SGN2','C') :F, espn1  ;
       :H = 'E1' :V =  ;
       valasi(espn1,tippre +  ;
       generic + sgn1 + sgn2,'5', ;
       'EspN1','C') :F, espn2 :H =  ;
       'E2' :V = valasi(espn2, ;
       tippre + generic + sgn1 +  ;
       sgn2 + espn1,'6','EspN2', ;
       'C') :F, espn3 :H = 'E3'  ;
       :V = valasi(espn3,tippre +  ;
       generic + sgn1 + sgn2 +  ;
       espn1 + espn2,'7','EspN3', ;
       'C') :F, espn4 :H = 'E4'  ;
       :V = valasi(espn4,tippre +  ;
       generic + sgn1 + sgn2 +  ;
       espn1 + espn2 + espn3,'8', ;
       'EspN4','C') .AND.  ;
       chequeo() :F, valpart :H =  ;
       'Asignaci¢n' :P =  ;
       '99,999,999', meseje :H =  ;
       'Mes', numcre :H =  ;
       'Cr‚dito', numtra :H =  ;
       'Transf.' LOCK 4 NOMENU  ;
       NOAPPEND NODELETE NOCLEAR  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.uniges +  ;
       m.unieje +  ;
       ALLTRIM(m.codcad) +  ;
       ALLTRIM(m.codfte)  ;
       NOREFRESH
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
DO standby WITH  ;
   'Presione Tecla para Continuar',  ;
   19, 2
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
SET ORDER TO iteparI1
SEEK vacpya
IF FOUND() .AND. vnewing
     vmodeje = itepar.modeje
     vcoddep = itepar.coddep
     vmeta = itepar.metas
     DO standby WITH  ;
        'Ya est  registrada esta partida '
     SET ORDER TO (ord)
     GOTO vc
     IF RLOCK()
          REPLACE modeje WITH  ;
                  vmodeje, coddep  ;
                  WITH vcoddep,  ;
                  metas WITH  ;
                  vmeta, codpart  ;
                  WITH vcodpart
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
  'Ejecuci¢n Mensualizada'  ;
  MESSAGE 'Ejecuci¢n'
MENU TO opcion
RELEASE WINDOW lis_1
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
USE IN 6 repopreI ALIAS repo
SELECT repo
vdbf = SYS(3) + '.dbf'
COPY TO (vdbf) STRUCTURE
USE IN 6 EXCLUSIVE (vdbf) ALIAS  ;
    repo
SELECT repo
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
STORE SPACE(6) TO vcodpart,  ;
      vactpry
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
          vrep = 1
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
            'Cad. Funcion.: ' GET  ;
            vcodcad PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcad),  ;
            val_codcad(vcodcad, ;
            vperiodo +  ;
            ALLTRIM(vuniges) +  ;
            ALLTRIM(vunieje),' ', ;
            18,30), .T.)
          @ 8, 2 SAY  ;
            '   Fte. Fto. : ' GET  ;
            vcodfte PICTURE '!!'  ;
            VALID IIF( .NOT.  ;
            EMPTY(vcodfte),  ;
            val_para(vcodfte, ;
            'CODFTE',' ',18,30),  ;
            .T.)
          @ 10, 2 SAY  ;
            '    Reporte? : ' GET  ;
            vrep FUNCTION  ;
            '^ Normal;Con Porcentaje'
          READ VALID val_read()
          DEACTIVATE WINDOW lis_1
          xcodfte = vcodfte
          xcodcad = vcodcad
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
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
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
.AND. IIF(EMPTY(ALLTRIM(meseje)),;
.T., meseje <= ALLTRIM(vcalend))
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
                    m.totcal = 0
                    IF  .NOT.  ;
                        FOUND()
                         m.&vcod = m.valparT
                         APPEND BLANK
                         GATHER MEMVAR
                         SELECT maepre
                         SEEK m.periodo +  ;
                              m.uniges +  ;
                              m.unieje +  ;
                              m.codcad
                         cmes = ''
                         IF FOUND()
                              cmet =  ;
                               maepre.descri
                         ENDIF
                         SELECT repo
                         REPLACE metas  ;
                                 WITH  ;
                                 cmet
                         IF  .NOT.  ;
                             EMPTY(itepar.meseje)  ;
                             .AND.   ;
                             .NOT.  ;
                             EMPTY(itepar.numcre)
                              IF BETWEEN(VAL(itepar.meseje),  ;
                                 1,  ;
                                 12)
                              ELSE
                                   DO standby WITH 'Exixte Un Error en Una Ampliacion Presupuestal. Revise'
                              ENDIF
                         ENDIF
                         m.&vcod=0
                    ELSE
                         IF RLOCK()
                              REPLACE  ;
                               valpart  ;
                               WITH  ;
                               itepar.valpart +  ;
                               valpart
                              IF   ;
                               .NOT.  ;
                               EMPTY(itepar.meseje)  ;
                               .AND.   ;
                               .NOT.  ;
                               EMPTY(itepar.numcre)
                                   IF BETWEEN(VAL(itepar.meseje), 1, 12)
                                   ELSE
                                        DO standby WITH 'Exixte Un Error en Una Ampliacion Presupuestal. Revise'
                                   ENDIF
                              ENDIF
                         ENDIF
                         UNLOCK
                         &vcod = 0
                    ENDIF
                    SELECT itepar
               ENDSCAN
               SELECT repo
               zind = SYS(3) +  ;
                      '.IDX'
               INDEX ON codcad +  ;
                     codfte +  ;
                     codpart TO  ;
                     (zind)
               IF  .NOT.  ;
                   USED('Rever')
                    USE IN 0  ;
                        RevSNu  ;
                        ALIAS  ;
                        rever  ;
                        ORDER  ;
                        RevSNu1
                    USE IN 0  ;
                        RevSNutS  ;
                        ALIAS  ;
                        reverts  ;
                        ORDER  ;
                        RevSNuTS1
                    borra = .T.
                    SELECT iteri
                    SET RELATION TO nummes;
+ numrev INTO rever ADDITIVE
                    SET RELATION TO nummes;
+ numrev INTO reverts ADDITIVE
               ENDIF
               SELECT iteri
               SET RELATION TO periodo;
+ nummes + numri INTO recing ADDITIVE
               SET FILTER TO nummes <=;
ALLTRIM(vcalend);
.AND. tipo = 'P';
.AND. iteri.estado <> '99';
.AND. IIF(;
.NOT. EMPTY(numri);
.AND. EMPTY(numrev), (recing.codfte =;
ALLTRIM(xcodfte);
.AND. recing.codcad = ALLTRIM(xcodcad)),;
(rever.codfte = ALLTRIM(xcodfte);
.AND. rever.codcad = ALLTRIM(xcodcad));
.OR. (reverts.codfte = ALLTRIM(xcodfte);
.AND. reverts.codcad = ALLTRIM(xcodcad)))
               GOTO TOP
               SCAN
                    DO CASE
                         CASE  .NOT.  ;
                               EMPTY(numri)
                              ccadena =  ;
                               recing.codcad
                              cfuente =  ;
                               recing.codfte
                              nimporte =  ;
                               impparc
                         CASE  .NOT.  ;
                               EMPTY(numrev)  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF('Rever')
                              ccadena =  ;
                               rever.codcad
                              cfuente =  ;
                               rever.codfte
                              nimporte =  ;
                               impparc * - ;
                               1
                         CASE  .NOT.  ;
                               EMPTY(numrev)  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF('ReverTS')
                              ccadena =  ;
                               reverts.codcad
                              cfuente =  ;
                               reverts.codfte
                              nimporte =  ;
                               impparc * - ;
                               1
                    ENDCASE
                    SELECT maepre
                    IF  .NOT.  ;
                        EMPTY(iteri.numri)
                         SEEK vperiodo +  ;
                              '01001' +  ;
                              recing.codcad
                    ELSE
                         SEEK vperiodo +  ;
                              '01001' +  ;
                              ccadena
                    ENDIF
                    vestfun = uniges +  ;
                              unieje +  ;
                              codfun +  ;
                              codprg +  ;
                              codspr +  ;
                              actpry +  ;
                              codcom +  ;
                              codmet
                    IF  .NOT.  ;
                        EMPTY(iteri.numri)
                         vkey = recing.codcad +  ;
                                recing.codfte +  ;
                                iteri.codpart
                         vcodcad =  ;
                          recing.codcad
                         vcodfte =  ;
                          recing.codfte
                    ELSE
                         vkey = ccadena +  ;
                                cfuente +  ;
                                iteri.codpart
                         vcodcad =  ;
                          ccadena
                         vcodfte =  ;
                          cfuente
                    ENDIF
                    vmes = 'MES_' +  ;
                           ALLTRIM(iteri.nummes)
                    SELECT repo
                    SEEK vkey
                    IF FOUND()
                         REPLACE totafe  ;
                                 WITH  ;
                                 totafe +  ;
                                 nimporte
                         REPLACE;
 &vmes WITH &vmes+nImporte
                    ELSE
                         APPEND BLANK
                         REPLACE CODPART;
WITH ITEri.CODPART, UNIGES  WITH MAEPRE.UNIGES,;
UNIEJE  WITH MAEPRE.UNIEJE, Metas;
   WITH MAEPRE.Descri, PERIODO WITH VPERIODO,;
CODCAD  WITH VCODCAD, CODFTE  WITH VCODFTE,;
&vmes   WITH nImporte, ESTFUN  WITH VESTFUN,;
TOTAFE  WITH nImporte
                    ENDIF
                    IF iteri.nummes =  ;
                       ALLTRIM(vcalend)
                         REPLACE totcal  ;
                                 WITH  ;
                                 totcal +  ;
                                 nimporte
                    ENDIF
                    SELECT iteri
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
                    IF vrep = 1
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'SalPrei1',  ;
                            ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    ELSE
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'SalPrIQ1',  ;
                            ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    ENDIF
               ENDIF
               IF yesno( ;
                  'Imprime Consolidado' ;
                  )
                    xdbf = SYS(3) +  ;
                           '.DBF'
                    xind = SYS(3) +  ;
                           '.IDX'
                    COPY TO  ;
                         (xdbf)  ;
                         STRUCTURE
                    INDEX ON  ;
                          codfte +  ;
                          codpart  ;
                          TO  ;
                          (xind)
                    USE IN 0  ;
                        (xdbf)  ;
                        ALIAS  ;
                        consol
                    SELECT repo
                    GOTO TOP
                    SCAN
                         vcodpart1 =  ;
                          codfte +  ;
                          codpart
                         vcodpart2 =  ;
                          codfte +  ;
                          codpart
                         vtotafe =  ;
                          0
                         vtotcal =  ;
                          0
                         vpresu =  ;
                          0
                         SCATTER MEMVAR
                         DO WHILE  ;
                            vcodpart1= ;
                            vcodpart2  ;
                            .AND.   ;
                            .NOT.  ;
                            EOF()
                              vpresu =  ;
                               vpresu +  ;
                               valpart
                              vtotcal =  ;
                               vtotcal +  ;
                               totcal
                              vtotafe =  ;
                               vtotafe +  ;
                               totafe
                              SKIP
                              vcodpart2 =  ;
                               codfte +  ;
                               codpart
                         ENDDO
                         m.valpart =  ;
                          vpresu
                         m.totcal =  ;
                          vtotcal
                         m.totafe =  ;
                          vtotafe
                         SELECT consol
                         APPEND BLANK
                         GATHER MEMVAR
                         SELECT repo
                         SKIP -1
                    ENDSCAN
                    SELECT consol
                    REPLACE codcad  ;
                            WITH  ;
                            '0001'  ;
                            ALL
                    GOTO TOP
                    IF vrep = 1
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisE5ci',  ;
                            ' Consolidado de la Ejecucion ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    ELSE
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisE5cQi',  ;
                            ' Consolidado de la Ejecucion con Porcentaje ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    ENDIF
                    USE
                    ERASE (xdbf)
               ENDIF
               SELECT iteri
               SET RELATION OFF INTO recing
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
          PRIVATE cmes1, cmes2
          cmes1 = SPACE(2)
          cmes2 = SPACE(2)
          DEFINE WINDOW lis_1  ;
                 FROM 4, 10 TO 20,  ;
                 70 FLOAT TITLE  ;
                 ' °°  Ejecuci¢n Mensualizada °° '  ;
                 DOUBLE COLOR  ;
                 SCHEME 5
          ACTIVATE WINDOW lis_1
          @ 0, 2 SAY  ;
            '     Periodo : ' GET  ;
            vperiodo PICTURE '!!'  ;
            VALID  .NOT.  ;
            EMPTY(vperiodo)
          @ 1, 2 SAY  ;
            '       Meses : ' GET  ;
            cmes1 PICTURE '!!'  ;
            VALID val_para(cmes1, ;
            'FECMES',' ',18,12)
          @ 1, 35 GET cmes2  ;
            PICTURE '!!' VALID  ;
            val_para(cmes2, ;
            'FECMES',' ',38,12)
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
            'Cad. Funcion.: ' GET  ;
            vcodcad PICTURE  ;
            '!!!!' VALID IIF(  ;
            .NOT. EMPTY(vcodcad),  ;
            val_codcad(vcodcad, ;
            vperiodo +  ;
            ALLTRIM(vuniges) +  ;
            ALLTRIM(vunieje),' ', ;
            18,30), .T.)
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
          xcodfte = vcodfte
          xcodcad = vcodcad
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
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
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
.AND. (BETWEEN(VAL(meseje), VAL(cmes1),;
VAL(cmes2));
.OR. EMPTY(meseje))
               GOTO TOP
               SCAN
                    SCATTER MEMVAR
                    IF codpart =  ;
                       '4.2.1.001'
                    ENDIF
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
                         m.&vcod = m.valparT
                         APPEND BLANK
                         GATHER MEMVAR
                         SELECT maepre
                         SEEK m.periodo +  ;
                              m.uniges +  ;
                              m.unieje +  ;
                              m.codcad
                         cmes = ''
                         IF FOUND()
                              cmet =  ;
                               maepre.descri
                         ENDIF
                         SELECT repo
                         REPLACE metas  ;
                                 WITH  ;
                                 cmet
                         IF  .NOT.  ;
                             EMPTY(itepar.meseje)  ;
                             .AND.   ;
                             .NOT.  ;
                             EMPTY(itepar.numcre)
                              IF BETWEEN(VAL(itepar.meseje),  ;
                                 1,  ;
                                 12)
                                   IF codpart <> '3.1.2.030'
                                   ENDIF
                              ELSE
                                   DO standby WITH 'Exixte Un Error en Una Ampliacion Presupuestal. Revise'
                              ENDIF
                         ENDIF
                         m.&vcod=0
                    ELSE
                         IF RLOCK()
                              REPLACE  ;
                               valpart  ;
                               WITH  ;
                               itepar.valpart +  ;
                               valpart
                              IF   ;
                               .NOT.  ;
                               EMPTY(itepar.meseje)  ;
                               .AND.   ;
                               .NOT.  ;
                               EMPTY(itepar.numcre)
                                   IF BETWEEN(VAL(itepar.meseje), 1, 12)
                                        IF codpart <> '3.1.2.030'
                                        ENDIF
                                   ELSE
                                        DO standby WITH 'Exixte Un Error en Una Ampliacion Presupuestal. Revise'
                                   ENDIF
                              ENDIF
                         ENDIF
                         UNLOCK
                         &vcod = 0
                    ENDIF
                    SELECT itepar
               ENDSCAN
               SELECT repo
               zind = SYS(3) +  ;
                      '.IDX'
               INDEX ON codcad +  ;
                     codfte +  ;
                     codpart TO  ;
                     (zind)
               IF  .NOT.  ;
                   USED('Rever')
                    USE IN 0  ;
                        RevSNu  ;
                        ALIAS  ;
                        rever  ;
                        ORDER  ;
                        RevSNu1
                    USE IN 0  ;
                        RevSNutS  ;
                        ALIAS  ;
                        reverts  ;
                        ORDER  ;
                        RevSNuTS1
                    borra = .T.
                    SELECT iteri
                    SET RELATION TO nummes;
+ numrev INTO rever ADDITIVE
                    SET RELATION TO nummes;
+ numrev INTO reverts ADDITIVE
               ENDIF
               SELECT iteri
               SET RELATION TO periodo;
+ nummes + numri INTO recing ADDITIVE
               SET FILTER TO BETWEEN(VAL(nummes),;
VAL(cmes1), VAL(cmes2));
.AND. tipo = 'P';
.AND. iteri.estado <> '99';
.AND. IIF(;
.NOT. EMPTY(numri);
.AND. EMPTY(numrev), (recing.codfte =;
ALLTRIM(xcodfte);
.AND. recing.codcad = ALLTRIM(xcodcad)),;
(rever.codfte = ALLTRIM(xcodfte);
.AND. rever.codcad = ALLTRIM(xcodcad));
.OR. (reverts.codfte = ALLTRIM(xcodfte);
.AND. reverts.codcad = ALLTRIM(xcodcad)))
               GOTO TOP
               SCAN
                    DO CASE
                         CASE  .NOT.  ;
                               EMPTY(numri)
                              ccadena =  ;
                               recing.codcad
                              cfuente =  ;
                               recing.codfte
                              nimporte =  ;
                               impparc
                         CASE  .NOT.  ;
                               EMPTY(numrev)  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF('Rever')
                              ccadena =  ;
                               rever.codcad
                              cfuente =  ;
                               rever.codfte
                              nimporte =  ;
                               impparc * - ;
                               1
                         CASE  .NOT.  ;
                               EMPTY(numrev)  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF('ReverTS')
                              ccadena =  ;
                               reverts.codcad
                              cfuente =  ;
                               reverts.codfte
                              nimporte =  ;
                               impparc * - ;
                               1
                    ENDCASE
                    SELECT maepre
                    IF  .NOT.  ;
                        EMPTY(iteri.numri)
                         SEEK vperiodo +  ;
                              '01001' +  ;
                              recing.codcad
                    ELSE
                         SEEK vperiodo +  ;
                              '01001' +  ;
                              ccadena
                    ENDIF
                    vestfun = uniges +  ;
                              unieje +  ;
                              codfun +  ;
                              codprg +  ;
                              codspr +  ;
                              actpry +  ;
                              codcom +  ;
                              codmet
                    IF  .NOT.  ;
                        EMPTY(iteri.numri)
                         vkey = recing.codcad +  ;
                                recing.codfte +  ;
                                iteri.codpart
                         vcodcad =  ;
                          recing.codcad
                         vcodfte =  ;
                          recing.codfte
                    ELSE
                         vkey = ccadena +  ;
                                cfuente +  ;
                                iteri.codpart
                         vcodcad =  ;
                          ccadena
                         vcodfte =  ;
                          cfuente
                    ENDIF
                    vmes = 'MES_' +  ;
                           ALLTRIM(iteri.nummes)
                    SELECT repo
                    SEEK vkey
                    IF FOUND()
                         REPLACE totafe  ;
                                 WITH  ;
                                 totafe +  ;
                                 nimporte
                         REPLACE;
 &vmes WITH &vmes+nImporte
                    ELSE
                         APPEND BLANK
                         REPLACE CODPART;
WITH ITEri.CODPART, UNIGES  WITH MAEPRE.UNIGES,;
UNIEJE  WITH MAEPRE.UNIEJE, Metas;
   WITH MAEPRE.Descri, PERIODO WITH VPERIODO,;
CODCAD  WITH VCODCAD, CODFTE  WITH VCODFTE,;
&vmes   WITH nImporte, ESTFUN  WITH VESTFUN,;
TOTAFE  WITH nImporte
                    ENDIF
                    IF BETWEEN(iteri.nummes,  ;
                       cmes1,  ;
                       cmes2)
                         REPLACE totcal  ;
                                 WITH  ;
                                 totcal +  ;
                                 nimporte
                    ENDIF
                    SELECT iteri
               ENDSCAN
               DEACTIVATE WINDOW  ;
                          standby
               SELECT repo
               GOTO TOP
               vxls = 'EM' +  ;
                      cmes2 +  ;
                      vperiodo +  ;
                      vcodfte +  ;
                      '.Dbf'
               COPY TO (vxls)
               !COPY &vXls D:\XX
               ERASE (vxls)
               GOTO TOP
               IF EOF()
                    DO standby  ;
                       WITH  ;
                       'No existe Registros para procesar'
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'SalPreM1',  ;
                       ' Ejecuci¢n Presupuestal Mensualizado ',  ;
                       1, .F.,  ;
                       .T.
               ENDIF
               SELECT iteri
               SET RELATION OFF INTO recing
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
FUNCTION sumpre2
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM VALPART TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumcal
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM totcal TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumafe
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM totafe TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumsal
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM VALPART-TOTAFE TO suma FOR &vFiltro=;
vCalen
GOTO vrec
RETURN suma
*
FUNCTION summes
PARAMETER vcalen, part, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM &part TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*

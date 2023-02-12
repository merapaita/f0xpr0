CLOSE DATABASES
SET EXCLUSIVE OFF
USE IN 1 parmae ALIAS parma ORDER  ;
    parmae1
USE IN 2 calen ALIAS calen ORDER  ;
    calen2
USE IN 3 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 4 maepar ALIAS presu ORDER  ;
    maepar1
USE IN 5 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 8 IteHc ALIAS itehc ORDER  ;
    Itehc1
USE IN 10 Itetra ALIAS itetra  ;
    ORDER Itetra1
USE IN 11 Itecre ALIAS itecre  ;
    ORDER Itecre1
USE IN 12 HOJMOD ALIAS hojmod  ;
    ORDER hojmod1
USE IN 13 cresup ALIAS cresup  ;
    ORDER cresup1
USE IN 14 Trapar ALIAS trapar  ;
    ORDER trapar1
USE IN 15 CatAsi ALIAS catasi  ;
    ORDER CatAsi4
vmens01 = 'Registro de Calendario'
vmens02 = ' Calendario : REVISION '
vmens04 = 'Dicho Calendario no fue encontrado'
vmens05 = 'No existe Calendario anterior'
vmens06 = 'No existe Calendario siguiente'
vmens07 = '¨ Desea Anular ‚ste Calendario ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Calendario ha sido anulado'
vmens10 = 'El Calendario ya est  Atendido'
vmens11 = 'El Calendario ha sido devuelto'
ON KEY LABEL F9 DO VISTA_DET
SELECT calen
SET RELATION TO periodo + uniges + unieje;
+ codcad + codfte + codpart INTO itepar
SET SKIP TO itepar
SELECT presu
GOTO TOP
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
vtempo = ' Revisa  Busca  Anterior  Siguiente           Ingresa  aNula    Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 11, 79 TITLE vmens02  ;
       FOOTER '[F9] Detalle'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle: Calendarios'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2a FROM 01, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle: Calendarios'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 02, 02  ;
       TO 22, 77 TITLE  ;
       '² ®F10¯ Escoge ²' DOUBLE  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_4 FROM 02, 02  ;
       TO 22, 77 TITLE  ;
       '² ®F10¯ Escoge ²' DOUBLE  ;
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
DEFINE PAD ingre OF mmenu PROMPT  ;
       '\<Ingresa' AT 24, 45
DEFINE PAD anula OF mmenu PROMPT  ;
       'a\<Nula   ' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Listar ' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
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
SELECT calen
SET ORDER TO calen2
HIDE POPUP ALL
GOTO TOP
SEEK m.periodo + m.uniges +  ;
     m.unieje + ALLTRIM(m.codcad) +  ;
     ALLTRIM(m.codfte)
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            nummes :H = 'Mes',  ;
            codpart :H = 'P.E.',  ;
            xx = IIF( .NOT.  ;
            EMPTY(ALLTRIM(codpart)),  ;
            valasi1('Calen', ;
            codpart,'6','Descri', ;
            'R'), '') :H =  ;
            'Descripci¢n' : 30,  ;
            yy = IIF(nummes <  ;
            '04', itepar.tri_01,  ;
            IIF(nummes < '07',  ;
            itepar.tri_02,  ;
            IIF(nummes < '10',  ;
            itepar.tri_03,  ;
            itepar.tri_04))) :H =  ;
            'Asig.Trim.' :P =  ;
            '99,999,999' :R,  ;
            valpart :H =  ;
            'Cal.Mes' :P =  ;
            '99,999,999', ampliar  ;
            :H = 'Ampliar' :P =  ;
            '99,999,999', saldo =  ;
            (valpart + ampliar)  ;
            :H = 'Tot. Cal.' :P =  ;
            '99,999,999' :F,  ;
            saldo1 =  ;
            val_saldo(codpart, ;
            nummes) :H =  ;
            'Util. Trim.' :P =  ;
            '99,999,999' :F  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_2  ;
            KEY m.periodo +  ;
            m.uniges + m.unieje +  ;
            ALLTRIM(m.codcad) +  ;
            ALLTRIM(m.codfte)  ;
            TIMEOUT 0.0001   ;
            NOREFRESH
ELSE
     ACTIVATE WINDOW wind_2
     CLEAR
     @ 4, 25 SAY  ;
       'No hay asignaci¢n de calendario'
ENDIF
SELECT presu
RETURN
*
PROCEDURE vista_det
SELECT calen
SET ORDER TO calen2
HIDE POPUP ALL
vtempo = '°°°°°°[F2]->Ord. x Mes°°°°°°°°°[F3]->Ord x Part °°°°°°°°°°°°[ESC]Terminar°°°°°°'
ON KEY LABEL F9
ON KEY LABEL F2 Do Ord_mes
ON KEY LABEL F3 Do Ord_par
GOTO TOP
SEEK m.periodo + m.uniges +  ;
     m.unieje + ALLTRIM(m.codcad) +  ;
     ALLTRIM(m.codfte)
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            nummes :H = 'Mes',  ;
            codpart :H =  ;
            'Partida', xx = IIF(  ;
            .NOT.  ;
            EMPTY(itepar.codpart),  ;
            IIF( .NOT.  ;
            EMPTY(itepar.subesp),  ;
            val_para1(itepar.subesp, ;
            'SUBESP' +  ;
            itepar.espgas,'D'),  ;
            val_para(itepar.espgas, ;
            'ESPGAS','D',28,50)),  ;
            ' ') :H =  ;
            'Descripci¢n' : 30,  ;
            yy = IIF(nummes <  ;
            '04', itepar.tri_01,  ;
            IIF(nummes < '07',  ;
            itepar.tri_02,  ;
            IIF(nummes < '10',  ;
            itepar.tri_03,  ;
            itepar.tri_04))) :H =  ;
            'Asig.Trim.' :P =  ;
            '99,999,999' :R,  ;
            valpart :H =  ;
            'Cal.Mes' :P =  ;
            '99,999,999', ampliar  ;
            :H = 'Ampliar' :P =  ;
            '99,999,999', saldo =  ;
            (valpart + ampliar)  ;
            :H = 'Tot. Cal.' :P =  ;
            '99,999,999' :F,  ;
            saldo1 =  ;
            val_saldo(codpart, ;
            nummes) :H =  ;
            'Util. Trim.' :P =  ;
            '99,999,999' :F  ;
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
       'No hay asignaci¢n de calendario'
ENDIF
SET ORDER TO CALEN2
ON KEY LABEL F9 DO VISTA_DET
ON KEY LABEL F2
ON KEY LABEL F3
SHOW MENU mmenu
SELECT presu
DO vista
RETURN
*
FUNCTION ord_mes
v = RECNO()
SET ORDER TO CALEN1
GOTO TOP
GOTO v
RETURN .T.
*
FUNCTION ord_par
v = RECNO()
SET ORDER TO CALEN2
GOTO TOP
GOTO v
RETURN .T.
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
ON KEY LABEL f10 KEYBOARD CHR(23)
SET RELATION TO periodo + uniges + unieje;
+ codcad + codfte INTO calen
SET SKIP TO calen
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE FIELDS presu.periodo :H =  ;
       'Per', presu.codcad :H =  ;
       'CodCad', presu.codfte :H =  ;
       'Fte', calen.nummes :H =  ;
       'Mes', calen.codpart :H =  ;
       'Partida', xx = IIF( .NOT.  ;
       EMPTY(calen.codpart),  ;
       IIF(LEN(ALLTRIM(calen.codpart)) >  ;
       6,  ;
       val_para1(RIGHT(calen.codpart,  ;
       2),'SUBESP' +  ;
       SUBSTR(calen.codpart, 5,  ;
       2),'D'),  ;
       val_para(SUBSTR(calen.codpart,  ;
       5, 2),'ESPGAS','D',28,50)),  ;
       ' ') :H = 'Descripci¢n' :  ;
       30, calen.valpart :H =  ;
       'Asignaci¢n' :P =  ;
       '99,999,999' NOMENU  ;
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
PRIVATE vck
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
vck = RECNO()
SCATTER MEMVAR
DO pantalla
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
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
     ok = trabaja_hi()
     IF ok .AND. LASTKEY() <> 27
          SELECT presu
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
FUNCTION trabaja_hi
vsun = .T.
PUBLIC ak, vcanant
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°F4->Corrige°°°F5->Agregar°°°F6->Ampliaci¢n°°° F8->Elimina °°° F10->Terminar°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL f4 DO corri_item
ON KEY LABEL f5 DO agreg_item
ON KEY LABEL f6 DO ampli_item
ON KEY LABEL f8 DO elimi_item
ON KEY LABEL f10 KEYBOARD CHR(23)
SELECT calen
SET ORDER TO calen2
SEEK m.periodo + m.uniges +  ;
     m.unieje + ALLTRIM(m.codcad) +  ;
     ALLTRIM(m.codfte)
IF  .NOT. FOUND()
     DO agreg_item
ENDIF
BROWSE NOOPTIMIZE FIELDS nummes  ;
       :H = 'Mes' :R, codpart :H =  ;
       'Partida' :R, xx = IIF(  ;
       .NOT.  ;
       EMPTY(ALLTRIM(codpart)),  ;
       IIF(LEN(ALLTRIM(codpart)) >  ;
       6, val_para1(RIGHT(codpart,  ;
       2),'SUBESP' +  ;
       SUBSTR(codpart, 5, 2),'D'),  ;
       val_para(SUBSTR(codpart, 5,  ;
       2),'ESPGAS','D')), ' ') :H =  ;
       'Descripci¢n' : 30, yy =  ;
       IIF(nummes < '04',  ;
       itepar.tri_01, IIF(nummes <  ;
       '07', itepar.tri_02,  ;
       IIF(nummes < '10',  ;
       itepar.tri_03,  ;
       itepar.tri_04))) :H =  ;
       'Asig.Trim.' :P =  ;
       '99,999,999' :R, valpart  ;
       :H = 'Cal.Mes' :P =  ;
       '99,999,999' :R, ampliar  ;
       :H = 'Ampliar' :P =  ;
       '99,999,999' :R, saldo =  ;
       (valpart + ampliar) :H =  ;
       'Tot. Cal.' :P =  ;
       '99,999,999' :F, saldo1 =  ;
       val_saldo(codpart,nummes)  ;
       :H = 'Util. Trim.' :P =  ;
       '99,999,999' :F NOMENU  ;
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
SELECT calen
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
        LEN(ALLTRIM(codpart)) < 6  ;
        .OR. valpart + ampliar =  ;
        0
          IF RLOCK()
               DELETE NEXT 1
          ENDIF
          UNLOCK
     ELSE
          REPLACE estfun WITH  ;
                  m.estfun
     ENDIF
ENDSCAN
m.estado = '00'
UNLOCK ALL
SET ORDER TO calen2
ON KEY LABEL f4
ON KEY LABEL f5
ON KEY LABEL f6
ON KEY LABEL f10
ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SELECT presu
RETURN vsun
*
FUNCTION corri_item
SELECT calen
DEFINE WINDOW lis FROM 10, 12 TO  ;
       15, 68 FLOAT TITLE  ;
       ' °° Corrige el Calendario °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
vcodpart = calen.codpart
vmes = calen.nummes
vcanant = calen.valpart
vmonto = calen.valpart
@ 1, 2 SAY '       Mes : ' GET  ;
  vmes PICTURE '!!' VALID  ;
  val_para(vmes,'FECMES',' ',16, ;
  30)
@ 2, 2 SAY '   Partida : ' GET  ;
  vcodpart PICTURE '!!!!!!' VALID  ;
  val_part(vcodpart,m.periodo +  ;
  m.uniges + m.unieje +  ;
  ALLTRIM(codcad) +  ;
  ALLTRIM(codfte),' ',16,30)
READ VALID val_lee()
SEEK m.periodo + m.uniges +  ;
     m.unieje + m.codcad +  ;
     ALLTRIM(m.codfte) +  ;
     ALLTRIM(vcodpart) +  ;
     ALLTRIM(vmes)
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No ha sido ingresado...'
     RETURN .F.
ENDIF
vrecno = RECNO()
@ 3, 2 SAY 'Calendario : ' GET  ;
  vmonto PICTURE '99,999,999'
READ
RELEASE WINDOW lis
REPLACE periodo WITH m.periodo,  ;
        uniges WITH m.uniges,  ;
        unieje WITH m.unieje,  ;
        nummes WITH vmes, codcad  ;
        WITH ALLTRIM(m.codcad),  ;
        codfte WITH  ;
        ALLTRIM(m.codfte),  ;
        codpart WITH vcodpart,  ;
        valpart WITH vmonto
GOTO TOP
SEEK m.periodo + m.uniges +  ;
     m.unieje + m.codcad +  ;
     ALLTRIM(m.codfte) +  ;
     ALLTRIM(vcodpart)
vacu = 0
SCAN WHILE periodo + uniges +  ;
     unieje + codcad + codfte +  ;
     codpart = m.periodo +  ;
     m.uniges + m.unieje +  ;
     m.codcad + ALLTRIM(m.codfte) +  ;
     ALLTRIM(vcodpart)
     REPLACE itepar.totcal WITH  ;
             vacu + calen.valpart +  ;
             calen.ampliar
     IF nummes < '04'
          REPLACE calen.valpre  ;
                  WITH  ;
                  itepar.totcal
     ELSE
          IF nummes < '07'
               REPLACE calen.valpre  ;
                       WITH  ;
                       itepar.totcal -  ;
                       itepar.tri_01
          ELSE
               IF nummes < '10'
                    REPLACE calen.valpre  ;
                            WITH  ;
                            itepar.totcal -  ;
                            (itepar.tri_01 +  ;
                            itepar.tri_02)
               ELSE
                    REPLACE calen.valpre  ;
                            WITH  ;
                            itepar.totcal -  ;
                            (itepar.tri_01 +  ;
                            itepar.tri_02 +  ;
                            itepar.tri_03)
               ENDIF
          ENDIF
     ENDIF
     vacu = vacu + calen.valpart
ENDSCAN
GOTO vrecno
ok2 = chemon(vcodpart,vmonto +  ;
      calen.ampliar)
RETURN .T.
*
FUNCTION ampli_item
SELECT calen
DEFINE WINDOW lis FROM 10, 12 TO  ;
       15, 68 FLOAT TITLE  ;
       ' °° Ampliaci¢n de Calendario °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
vrecno = RECNO()
vcodpart = calen.codpart
vmes = calen.nummes
vcanant = calen.valpart
vmonto = calen.ampliar
@ 1, 2 SAY '       Mes : ' GET  ;
  vmes PICTURE '!!' VALID  ;
  val_para(vmes,'FECMES',' ',16, ;
  30)
@ 2, 2 SAY '   Partida : ' GET  ;
  vcodpart PICTURE '!!!!!!' VALID  ;
  val_part(vcodpart,m.periodo +  ;
  m.uniges + m.unieje +  ;
  ALLTRIM(codcad) +  ;
  ALLTRIM(codfte),' ',16,30)
READ VALID val_lee()
SEEK m.periodo + m.uniges +  ;
     m.unieje + m.codcad +  ;
     ALLTRIM(m.codfte) +  ;
     ALLTRIM(vcodpart) +  ;
     ALLTRIM(vmes)
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No ha sido ingresado...'
     RETURN .F.
ENDIF
vrecno = RECNO()
@ 3, 2 SAY 'Ampliaci¢n : ' GET  ;
  vmonto PICTURE '99,999,999'
READ
RELEASE WINDOW lis
REPLACE periodo WITH m.periodo,  ;
        uniges WITH m.uniges,  ;
        unieje WITH m.unieje,  ;
        codcad WITH  ;
        ALLTRIM(m.codcad), codfte  ;
        WITH ALLTRIM(m.codfte),  ;
        codpart WITH vcodpart,  ;
        ampliar WITH vmonto
GOTO TOP
SEEK m.periodo + m.uniges +  ;
     m.unieje + m.codcad +  ;
     ALLTRIM(m.codfte) +  ;
     ALLTRIM(vcodpart)
vacu = 0
SCAN WHILE periodo + uniges +  ;
     unieje + codcad + codfte +  ;
     codpart = m.periodo +  ;
     m.uniges + m.unieje +  ;
     m.codcad + ALLTRIM(m.codfte) +  ;
     ALLTRIM(vcodpart)
     REPLACE itepar.totcal WITH  ;
             vacu + calen.valpart +  ;
             calen.ampliar
     IF nummes < '04'
          REPLACE calen.valpre  ;
                  WITH  ;
                  itepar.totcal
     ELSE
          IF nummes < '07'
               REPLACE calen.valpre  ;
                       WITH  ;
                       itepar.totcal -  ;
                       itepar.tri_01
          ELSE
               IF nummes < '10'
                    REPLACE calen.valpre  ;
                            WITH  ;
                            itepar.totcal -  ;
                            (itepar.tri_01 +  ;
                            itepar.tri_02)
               ELSE
                    REPLACE calen.valpre  ;
                            WITH  ;
                            itepar.totcal -  ;
                            (itepar.tri_01 +  ;
                            itepar.tri_02 +  ;
                            itepar.tri_03)
               ENDIF
          ENDIF
     ENDIF
     vacu = vacu + calen.valpart
ENDSCAN
GOTO vrecno
ok2 = chemon(vcodpart,vmonto +  ;
      calen.valpart)
RETURN .T.
*
FUNCTION agreg_item
SELECT calen
DEFINE WINDOW lis FROM 10, 12 TO  ;
       15, 68 FLOAT TITLE  ;
       ' °° Ingreso de Calendario °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
vmes = SPACE(2)
vcodpart = SPACE(6)
vmonto = 0
@ 1, 2 SAY '       Mes : ' GET  ;
  vmes PICTURE '!!' VALID  ;
  val_para(vmes,'FECMES',' ',16, ;
  30)
@ 2, 2 SAY '   Partida : ' GET  ;
  vcodpart PICTURE '!!!!!!' VALID  ;
  val_part(vcodpart,m.periodo +  ;
  m.uniges + m.unieje + m.codcad +  ;
  ALLTRIM(m.codfte),' ',16,25)
@ 3, 2 SAY 'Calendario : ' GET  ;
  vmonto PICTURE '99,999,999'
READ VALID val_lee()
RELEASE WINDOW lis
ok1 = chequeo(ALLTRIM(vcodpart) +  ;
      ALLTRIM(vmes))
IF ok1
     IF f_appd()
          REPLACE periodo WITH  ;
                  m.periodo,  ;
                  nummes WITH  ;
                  ALLTRIM(vmes),  ;
                  uniges WITH  ;
                  m.uniges,  ;
                  unieje WITH  ;
                  m.unieje,  ;
                  codcad WITH  ;
                  ALLTRIM(m.codcad),  ;
                  codfte WITH  ;
                  ALLTRIM(m.codfte),  ;
                  codpart WITH  ;
                  vcodpart,  ;
                  valpart WITH  ;
                  vmonto
          vrecno = RECNO()
          SEEK m.periodo +  ;
               m.uniges +  ;
               m.unieje +  ;
               m.codcad +  ;
               ALLTRIM(m.codfte) +  ;
               ALLTRIM(vcodpart)
          vacu = 0
          SCAN WHILE periodo +  ;
               uniges + unieje +  ;
               codcad + codfte +  ;
               codpart =  ;
               m.periodo +  ;
               m.uniges +  ;
               m.unieje +  ;
               m.codcad +  ;
               ALLTRIM(m.codfte) +  ;
               ALLTRIM(vcodpart)
               REPLACE itepar.totcal  ;
                       WITH vacu +  ;
                       calen.valpart +  ;
                       calen.ampliar
               IF vmes < '04'
                    REPLACE calen.valpre  ;
                            WITH  ;
                            itepar.totcal
               ELSE
                    IF vmes <  ;
                       '07'
                         REPLACE calen.valpre  ;
                                 WITH  ;
                                 itepar.totcal -  ;
                                 itepar.tri_01
                    ELSE
                         IF vmes <  ;
                            '10'
                              REPLACE  ;
                               calen.valpre  ;
                               WITH  ;
                               itepar.totcal -  ;
                               (itepar.tri_01 +  ;
                               itepar.tri_02)
                         ELSE
                              REPLACE  ;
                               calen.valpre  ;
                               WITH  ;
                               itepar.totcal -  ;
                               (itepar.tri_01 +  ;
                               itepar.tri_02 +  ;
                               itepar.tri_03)
                         ENDIF
                    ENDIF
               ENDIF
               vacu = vacu +  ;
                      calen.valpart
          ENDSCAN
          GOTO vrecno
          RETURN
     ENDIF
     ok2 = chemon(vcodpart, ;
           vmonto)
ENDIF
RETURN .F.
*
PROCEDURE ingre
PRIVATE vck
SELECT presu
vck = RECNO()
DO pantalla
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
PROCEDURE anula
PRIVATE vtemp
SELECT calen
vtemp = RECNO()
as = ORDER()
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado = '20'
     DO standby WITH  ;
        'Ya est  anulado..'
     RETURN
ENDIF
IF  .NOT. estado <> '  00'
     DO standby WITH vmens10
     RETURN
ENDIF
IF yesno( ;
   '¨ Desea anular ‚ste Calendario ?' ;
   )
     vcanant = valpart
     m.valpart = 0
     SELECT itepar
     IF (valpart + cresup +  ;
        tra001 + tra003 + tra004 +  ;
        tra005 - itepar.totcal -  ;
        (valpart - vcanant)) < 0
          DO standby WITH  ;
             'No existe marco presupuestal para asignar.... Se exede en '+ ;
             STR(valpart+cresup+ ;
             tra001+tra003+tra004+ ;
             tra005-itepar.totcal- ;
             m.valpart+vcanant,  ;
             8)
          SELECT calen
          SET ORDER TO (as)
          GOTO vtemp
          DO vista
          RETURN
     ENDIF
     SELECT calen
     GOTO vtemp
     m.valpre = m.valpre +  ;
                vcanant
     m.estado = '20'
     GATHER MEMVAR
     DO vista
ENDIF
UNLOCK ALL
RETURN
*
PROCEDURE elimi_item
IF RLOCK()
     DELETE NEXT 1
     SKIP 1
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
UNLOCK
RETURN
*
PROCEDURE lista
USE IN 18 repocal
USE IN 19 repcal1
SELECT 18
vind = SYS(3) + '.DBF'
COPY TO (vind) STRUCTURE
USE IN 18 EXCLUSIVE (vind) ALIAS  ;
    repo
SELECT 19
vind = SYS(3) + '.DBF'
COPY TO (vind) STRUCTURE
USE IN 19 EXCLUSIVE (vind) ALIAS  ;
    rep1
SELECT calen
vrecno = RECNO()
vorder = ORDER()
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
DEFINE WINDOW lis_1 FROM 05, 26  ;
       TO 15, 55 FLOAT TITLE  ;
       ' Opciones ' DOUBLE COLOR  ;
       SCHEME 5
ACTIVATE WINDOW lis_1
opcion = 0
SET MESSAGE TO 23
@ 1, 4 PROMPT  ;
  '  Marco Calendario  ' MESSAGE  ;
  PADC( ;
  'Imprime el Marco del Calendario',  ;
  79, ' ')
@ 3, 4 PROMPT  ;
  'Saldos de Calendario' MESSAGE  ;
  PADC( ;
  'Lista el  de Calendario por partida especifica',  ;
  79, ' ')
@ 5, 4 PROMPT  ;
  '   Marco Ejecuci¢n  ' MESSAGE  ;
  PADC( ;
  'Lista el Marco de Ejecuci¢n ',  ;
  79, ' ')
@ 7, 4 PROMPT  ;
  '   Ampliaciones     ' MESSAGE  ;
  PADC('Lista de Ampliaciones',  ;
  79, ' ')
MENU TO opcion
RELEASE WINDOW lis_1
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
DO CASE
     CASE opcion = 1
          DEFINE WINDOW lis_1  ;
                 FROM 4, 10 TO 20,  ;
                 70 FLOAT TITLE  ;
                 ' °° Calendario Marco Presupuestal °° '  ;
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
            '   Al mes de : ' GET  ;
            vcalend PICTURE '!!'  ;
            VALID  ;
            val_para(vcalend, ;
            'FECMES',' ',18,30)
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
          ACTIVATE WINDOW standby
          @ 1, 14 SAY  ;
            'Espere un momento ...'  ;
            COLOR W+/RB* 
          SELECT rep1
          ZAP
          vind = SYS(3) + '.IDX'
          zind = SYS(3) + '.IDX'
          INDEX ON LEFT(estfun,  ;
                5) + codcad +  ;
                codfte + codpart  ;
                TO (vind)
          IF vtotal = 1
               SELECT itepar.codpart,  ;
                      itepar.valpart,  ;
                      itepar.periodo,  ;
                      itepar.codcad,  ;
                      itepar.cresup,  ;
                      itepar.tra001,  ;
                      itepar.tra003,  ;
                      itepar.tra004,  ;
                      itepar.tra005,  ;
                      itepar.codfte,  ;
                      itepar.totafe,  ;
                      itepar.totcal,  ;
                      itepar.ubicac,  ;
                      itepar.estfun  ;
                      FROM ITEPAR  ;
                      WHERE  ;
                      periodo =  ;
                      ALLTRIM(vperiodo)  ;
                      AND IIF(  ;
                      NOT  ;
                      EMPTY(ALLTRIM(vcodcad)),  ;
                      codcad =  ;
                      ALLTRIM(vcodcad),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodfte)),  ;
                      codfte =  ;
                      ALLTRIM(vcodfte),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vuniges)),  ;
                      uniges =  ;
                      ALLTRIM(vuniges),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vunieje)),  ;
                      unieje =  ;
                      ALLTRIM(vunieje),  ;
                      .T.) INTO  ;
                      CURSOR  ;
                      PRESUX
          ELSE
               SELECT itepar.codpart,  ;
                      itepar.valpart,  ;
                      itepar.periodo,  ;
                      itepar.codcad,  ;
                      itepar.cresup,  ;
                      itepar.tra001,  ;
                      itepar.tra003,  ;
                      itepar.tra004,  ;
                      itepar.tra005,  ;
                      itepar.codfte,  ;
                      itepar.totafe,  ;
                      itepar.totcal,  ;
                      itepar.ubicac,  ;
                      itepar.estfun  ;
                      FROM ITEPAR  ;
                      WHERE  ;
                      periodo =  ;
                      ALLTRIM(vperiodo)  ;
                      AND IIF(  ;
                      NOT  ;
                      EMPTY(ALLTRIM(vuniges)),  ;
                      SUBSTR(estfun,  ;
                      1, 2) =  ;
                      ALLTRIM(vuniges),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vunieje)),  ;
                      SUBSTR(estfun,  ;
                      3, 3) =  ;
                      ALLTRIM(vunieje),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodfun)),  ;
                      SUBSTR(estfun,  ;
                      6, 2) =  ;
                      ALLTRIM(vcodfun),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodprg)),  ;
                      SUBSTR(estfun,  ;
                      8, 3) =  ;
                      ALLTRIM(vcodprg),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodspr)),  ;
                      SUBSTR(estfun,  ;
                      11, 4) =  ;
                      ALLTRIM(vcodspr),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vactpry)),  ;
                      SUBSTR(estfun,  ;
                      15, 6) =  ;
                      ALLTRIM(vactpry),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodcom)),  ;
                      SUBSTR(estfun,  ;
                      21, 5) =  ;
                      ALLTRIM(vcodcom),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodfte)),  ;
                      codfte =  ;
                      ALLTRIM(vcodfte),  ;
                      .T.) INTO  ;
                      CURSOR  ;
                      PRESUX
          ENDIF
          GOTO TOP
          vind = SYS(3) + '.DBF'
          COPY TO (vind)
          USE IN 17 EXCLUSIVE  ;
              (vind) ALIAS  ;
              presu1
          SELECT presu1
          INDEX ON LEFT(estfun,  ;
                5) + codcad +  ;
                codfte + codpart  ;
                TO (zind)
          IF vtotal = 1
               SELECT calen.nummes,  ;
                      calen.codpart,  ;
                      calen.valpart,  ;
                      calen.periodo,  ;
                      calen.codcad,  ;
                      calen.codfte,  ;
                      calen.totafe,  ;
                      calen.ampliar,  ;
                      calen.totoc,  ;
                      calen.totos,  ;
                      calen.estfun  ;
                      FROM CALEN  ;
                      WHERE  ;
                      periodo =  ;
                      ALLTRIM(vperiodo)  ;
                      AND IIF(  ;
                      NOT  ;
                      EMPTY(ALLTRIM(vcodcad)),  ;
                      codcad =  ;
                      ALLTRIM(vcodcad),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodfte)),  ;
                      codfte =  ;
                      ALLTRIM(vcodfte),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vuniges)),  ;
                      uniges =  ;
                      ALLTRIM(vuniges),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vunieje)),  ;
                      unieje =  ;
                      ALLTRIM(vunieje),  ;
                      .T.) AND  ;
                      nummes <=  ;
                      ALLTRIM(vcalend)  ;
                      INTO CURSOR  ;
                      CalenX
          ELSE
               SELECT calen.nummes,  ;
                      calen.codpart,  ;
                      calen.valpart,  ;
                      calen.periodo,  ;
                      calen.codcad,  ;
                      calen.codfte,  ;
                      calen.totafe,  ;
                      calen.ampliar,  ;
                      calen.totoc,  ;
                      calen.totos,  ;
                      calen.estfun  ;
                      FROM CALEN  ;
                      WHERE  ;
                      periodo =  ;
                      ALLTRIM(vperiodo)  ;
                      AND IIF(  ;
                      NOT  ;
                      EMPTY(ALLTRIM(vuniges)),  ;
                      SUBSTR(estfun,  ;
                      1, 2) =  ;
                      ALLTRIM(vuniges),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vunieje)),  ;
                      SUBSTR(estfun,  ;
                      3, 3) =  ;
                      ALLTRIM(vunieje),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodfun)),  ;
                      SUBSTR(estfun,  ;
                      6, 2) =  ;
                      ALLTRIM(vcodfun),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodprg)),  ;
                      SUBSTR(estfun,  ;
                      8, 3) =  ;
                      ALLTRIM(vcodprg),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodspr)),  ;
                      SUBSTR(estfun,  ;
                      11, 4) =  ;
                      ALLTRIM(vcodspr),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vactpry)),  ;
                      SUBSTR(estfun,  ;
                      15, 6) =  ;
                      ALLTRIM(vactpry),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodcom)),  ;
                      SUBSTR(estfun,  ;
                      21, 5) =  ;
                      ALLTRIM(vcodcom),  ;
                      .T.) AND  ;
                      IIF( NOT  ;
                      EMPTY(ALLTRIM(vcodfte)),  ;
                      codfte =  ;
                      ALLTRIM(vcodfte),  ;
                      .T.) AND  ;
                      nummes <=  ;
                      ALLTRIM(vcalend)  ;
                      INTO CURSOR  ;
                      CalenX
          ENDIF
          SELECT presu1
          GOTO TOP
          SCAN
               SCATTER MEMVAR
               m.valpres = m.valpart
               m.cresup = getcre()
               m.transf = gettra()
               SELECT rep1
               IF f_appd()
                    GATHER MEMVAR
               ENDIF
               SELECT presu1
          ENDSCAN
          SELECT calenx
          GOTO TOP
          SCAN
               SCATTER MEMVAR
               vrep2 = LEFT(m.estfun,  ;
                       5) +  ;
                       m.codcad +  ;
                       m.codfte +  ;
                       m.codpart
               STORE 0 TO m.c_01,  ;
                     m.c_02,  ;
                     m.c_03,  ;
                     m.c_04,  ;
                     m.c_05,  ;
                     m.c_06,  ;
                     m.c_07,  ;
                     m.c_08,  ;
                     m.c_09,  ;
                     m.c_10,  ;
                     m.c_11,  ;
                     m.c_12
               SELECT rep1
               SEEK ALLTRIM(vrep2)
               IF  .NOT. FOUND()
                    vcod = 'C_' +  ;
                           ALLTRIM(m.nummes)
                    m.&vcod=m.valpart;
+ m.Ampliar
                    APPEND BLANK
                    GATHER MEMVAR
                    vkey = LEFT(m.estfun,  ;
                           5) +  ;
                           m.codcad +  ;
                           m.codfte +  ;
                           m.codpart
                    SELECT presu1
                    GOTO TOP
                    SEEK vkey
                    IF FOUND()
                         vtransf =  ;
                          presu1.tra001 +  ;
                          presu1.tra003 +  ;
                          presu1.tra004 +  ;
                          presu1.tra005
                         SELECT rep1
                         REPLACE valpres  ;
                                 WITH  ;
                                 presu1.valpart,  ;
                                 cresup  ;
                                 WITH  ;
                                 presu1.cresup,  ;
                                 transf  ;
                                 WITH  ;
                                 vtransf
                    ENDIF
                    m.valpart = 0
               ELSE
                    vcod = 'C_' +  ;
                           ALLTRIM(m.nummes)
                    REPLACE &vcod WITH;
&vcod + CALENX.VALPART + CALENx.ampliar
                    m.valpart = 0
               ENDIF
               SELECT calenx
          ENDSCAN
          USE IN 17
          SELECT rep1
          DEACTIVATE WINDOW  ;
                     standby
          GOTO TOP
          IF vtotal = 1
               IF vtipo = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'MarCal1',  ;
                       ' A nivel de Marco Presupuestal'
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'MarCal11',  ;
                       ' A nivel de Marco Presupuestal'
               ENDIF
          ELSE
               IF vtipo = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'MarCaG1',  ;
                       ' A nivel de Marco Presupuestal'
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'MarCaG11',  ;
                       ' A nivel de Marco Presupuestal'
               ENDIF
          ENDIF
          CLOSE INDEX
          SET FILTER TO
          SELECT 13
          SET RELATION TO
          SELECT 14
          SET RELATION TO
          SELECT calen
          SET ORDER TO CALEN2
     CASE opcion = 2
          DEFINE WINDOW lis_1  ;
                 FROM 4, 10 TO 20,  ;
                 70 FLOAT TITLE  ;
                 ' °° SALDO DE CALENDARIOS °° '  ;
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
            '   Al mes de : ' GET  ;
            vcalend PICTURE '!!'  ;
            VALID  ;
            val_para(vcalend, ;
            'FECMES',' ',18,30)
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
          IF LASTKEY() = 27
               DO vista
               RETURN
          ENDIF
          ACTIVATE WINDOW standby
          @ 1, 14 SAY  ;
            'Espere un momento ...'  ;
            COLOR W+/RB* 
          SELECT calen
          GOTO TOP
          IF EOF()
               DO standby WITH  ;
                  vmens08
               RETURN
          ELSE
               IF vtotal = 1
                    SELECT calen.nummes,  ;
                           calen.codpart,  ;
                           calen.valpart,  ;
                           calen.periodo,  ;
                           calen.codcad,  ;
                           calen.codfte,  ;
                           calen.totafe,  ;
                           calen.ampliar,  ;
                           calen.totoc,  ;
                           calen.totos,  ;
                           calen.estfun  ;
                           FROM  ;
                           CALEN  ;
                           WHERE  ;
                           periodo =  ;
                           ALLTRIM(vperiodo)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vcodcad)),  ;
                           codcad =  ;
                           ALLTRIM(vcodcad),  ;
                           .T.)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vcodfte)),  ;
                           codfte =  ;
                           ALLTRIM(vcodfte),  ;
                           .T.)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vuniges)),  ;
                           uniges =  ;
                           ALLTRIM(vuniges),  ;
                           .T.)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vunieje)),  ;
                           unieje =  ;
                           ALLTRIM(vunieje),  ;
                           .T.)  ;
                           AND  ;
                           nummes =  ;
                           ALLTRIM(vcalend)  ;
                           INTO  ;
                           CURSOR  ;
                           CalenX
               ELSE
                    SELECT calen.nummes,  ;
                           calen.codpart,  ;
                           calen.valpart,  ;
                           calen.periodo,  ;
                           calen.codcad,  ;
                           calen.codfte,  ;
                           calen.totafe,  ;
                           calen.ampliar,  ;
                           calen.totoc,  ;
                           calen.totos,  ;
                           calen.estfun  ;
                           FROM  ;
                           CALEN  ;
                           WHERE  ;
                           periodo =  ;
                           ALLTRIM(vperiodo)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vuniges)),  ;
                           SUBSTR(estfun,  ;
                           1, 2) =  ;
                           ALLTRIM(vuniges),  ;
                           .T.)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vunieje)),  ;
                           SUBSTR(estfun,  ;
                           3, 3) =  ;
                           ALLTRIM(vunieje),  ;
                           .T.)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vcodfun)),  ;
                           SUBSTR(estfun,  ;
                           6, 2) =  ;
                           ALLTRIM(vcodfun),  ;
                           .T.)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vcodprg)),  ;
                           SUBSTR(estfun,  ;
                           8, 3) =  ;
                           ALLTRIM(vcodprg),  ;
                           .T.)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vcodspr)),  ;
                           SUBSTR(estfun,  ;
                           11, 4) =  ;
                           ALLTRIM(vcodspr),  ;
                           .T.)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vactpry)),  ;
                           SUBSTR(estfun,  ;
                           15, 6) =  ;
                           ALLTRIM(vactpry),  ;
                           .T.)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vcodcom)),  ;
                           SUBSTR(estfun,  ;
                           21, 5) =  ;
                           ALLTRIM(vcodcom),  ;
                           .T.)  ;
                           AND  ;
                           IIF(  ;
                           NOT  ;
                           EMPTY(ALLTRIM(vcodfte)),  ;
                           codfte =  ;
                           ALLTRIM(vcodfte),  ;
                           .T.)  ;
                           AND  ;
                           nummes =  ;
                           ALLTRIM(vcalend)  ;
                           INTO  ;
                           CURSOR  ;
                           CalenX
               ENDIF
          ENDIF
          vind = SYS(3) + '.DBF'
          COPY TO (vind)
          USE IN 0 EXCLUSIVE  ;
              (vind) ALIAS  ;
              calen1
          zind = SYS(3) + '.IDX'
          SELECT calen1
          INDEX ON LEFT(estfun,  ;
                30) + codfte +  ;
                codpart TO  ;
                (zind)
          SELECT itehc
          SET FILTER TO nummes = ALLTRIM(vcalend);
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
               SELECT calen1
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
                         APPEND BLANK
                         REPLACE codpart  ;
                                 WITH  ;
                                 itehc.codpart,  ;
                                 periodo  ;
                                 WITH  ;
                                 vperiodo,  ;
                                 codcad  ;
                                 WITH  ;
                                 itehc.codcad,  ;
                                 codfte  ;
                                 WITH  ;
                                 itehc.codfte,  ;
                                 estfun  ;
                                 WITH  ;
                                 LEFT(vkey,  ;
                                 30),  ;
                                 totafe  ;
                                 WITH  ;
                                 IIF(itehc.tipope =  ;
                                 '-',  ;
                                 itehc.valpart * - ;
                                 1,  ;
                                 itehc.valpart)
                    ENDIF
               ENDIF
               SELECT itehc
          ENDSCAN
          IF vsistema = '01'
               SELECT 8
               USE
               USE IN 8  ;
                   \EMER97\DATA\IteHc  ;
                   ALIAS itehce  ;
                   ORDER Itehc1
               SELECT 3
               USE
               USE IN 3  ;
                   \EMER97\DATA\maepre  ;
                   ALIAS maepree  ;
                   ORDER maepre1
               SELECT itehce
               SET FILTER TO nummes =;
ALLTRIM(vcalend);
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
                    SELECT maepree
                    SEEK vperiodo +  ;
                         itehce.uniges +  ;
                         itehce.unieje +  ;
                         itehce.codcad
                    vkey = uniges +  ;
                           unieje +  ;
                           codfun +  ;
                           codprg +  ;
                           codspr +  ;
                           actpry +  ;
                           itehce.codcom +  ;
                           '00001' +  ;
                           itehce.codfte +  ;
                           itehce.codpart
                    vkey1 = uniges +  ;
                            unieje +  ;
                            codfun +  ;
                            codprg +  ;
                            codspr +  ;
                            actpry +  ;
                            itehce.codcom +  ;
                            '00001' +  ;
                            itehce.codfte
                    SELECT calen1
                    SEEK vkey
                    IF FOUND()
                         REPLACE totafe  ;
                                 WITH  ;
                                 totafe +  ;
                                 IIF(itehce.tipope =  ;
                                 '-',  ;
                                 itehce.valpart * - ;
                                 1,  ;
                                 itehce.valpart)
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
                              REPLACE  ;
                               codpart  ;
                               WITH  ;
                               itehce.codpart,  ;
                               periodo  ;
                               WITH  ;
                               vperiodo,  ;
                               codcad  ;
                               WITH  ;
                               itehce.codcad,  ;
                               codfte  ;
                               WITH  ;
                               itehce.codfte,  ;
                               estfun  ;
                               WITH  ;
                               LEFT(vkey,  ;
                               30),  ;
                               totafe  ;
                               WITH  ;
                               IIF(itehce.tipope =  ;
                               '-',  ;
                               itehce.valpart * - ;
                               1,  ;
                               itehce.valpart)
                         ENDIF
                    ENDIF
                    SELECT itehce
               ENDSCAN
               SELECT 8
               USE
               USE IN 8 IteHc  ;
                   ALIAS itehc  ;
                   ORDER Itehc1
               SELECT 3
               USE
               USE IN 3 maepre  ;
                   ALIAS maepre  ;
                   ORDER maepre1
          ENDIF
          SELECT calen1
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
                    REPLACE calen1.codcad  ;
                            WITH  ;
                            itepar.codcad
               ENDIF
               SELECT calen1
          ENDSCAN
          GOTO TOP
          DEACTIVATE WINDOW  ;
                     standby
          IF vtotal = 1
               IF vtipo = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'SalCal1',  ;
                       ' A nivel de Marco Presupuestal'
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'SalCal11',  ;
                       ' A nivel de Marco Presupuestal'
               ENDIF
          ELSE
               IF vtipo = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'SalCaG1',  ;
                       ' A nivel de Marco Presupuestal'
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'SalCaG11',  ;
                       ' A nivel de Marco Presupuestal'
               ENDIF
          ENDIF
          USE
          ERASE (vind)
          SELECT calen
          SET ORDER TO CALEN2
     CASE opcion = 3
          DEFINE WINDOW lis_1  ;
                 FROM 4, 10 TO 20,  ;
                 70 FLOAT TITLE  ;
                 ' °°  Marco de Ejecuci¢n °° '  ;
                 DOUBLE COLOR  ;
                 SCHEME 5
          ACTIVATE WINDOW lis_1
          @ 0, 2 SAY  ;
            '     Periodo : ' GET  ;
            vperiodo PICTURE '!!'  ;
            VALID  .NOT.  ;
            EMPTY(vperiodo)
          @ 1, 2 SAY  ;
            '   Al Mes de : ' GET  ;
            vcalend PICTURE '!!'  ;
            VALID  ;
            val_para(vcalend, ;
            'FECMES',' ',18,25)
          @ 3, 2 SAY  ;
            '  Por Cadena : ' GET  ;
            vtotal SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 4, 2 SAY  ;
            '  Espec¡fico : ' GET  ;
            vtipo SIZE 1, 10, 6  ;
            FUNCTION  ;
            '*RNH \<Si;\<No'
          @ 5, 2 SAY  ;
            '  U. Gestora : ' GET  ;
            vuniges PICTURE '!!'  ;
            VALID  ;
            val_para(vuniges, ;
            'UNIGES',' ',18,30)
          @ 6, 2 SAY  ;
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
               RETURN
          ENDIF
          SELECT itepar
          IF EOF()
               DO standby WITH  ;
                  vmens08
          ELSE
               DEFINE WINDOW  ;
                      xwait FROM  ;
                      20, 06 TO  ;
                      22, 78  ;
                      COLOR  ;
                      SCHEME 05
               ACTIVATE WINDOW  ;
                        xwait
               @ 0, 10 SAY  ;
                 ' Espere un Momento...Procesando Marco de Ejecuci¢n !'  ;
                 COLOR W+/RB* 
               SELECT rep1
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
                    m.cresup = getcre()
                    m.transf = gettra()
                    SELECT rep1
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
                               m.valpart
                              REPLACE  ;
                               cresup  ;
                               WITH  ;
                               m.cresup
                              REPLACE  ;
                               transf  ;
                               WITH  ;
                               m.transf
                         ENDIF
                         UNLOCK
                    ENDIF
                    SELECT itepar
               ENDSCAN
               SELECT rep1
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
.AND. estado <> '99';
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
                    SELECT rep1
                    SEEK vkey
                    IF FOUND()
                         REPLACE &vmes;
WITH &vmes+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
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
               IF sistem = '1'
                    SELECT 8
                    USE
                    USE IN 8  ;
                        \EMER97\DATA\IteHc  ;
                        ALIAS  ;
                        itehce  ;
                        ORDER  ;
                        Itehc1
                    SELECT 3
                    USE
                    USE IN 3  ;
                        \EMER97\DATA\maepre  ;
                        ALIAS  ;
                        maepree  ;
                        ORDER  ;
                        maepre1
                    SELECT itehce
                    SET FILTER TO nummes;
<= ALLTRIM(vcalend);
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
.AND. estado <> '99';
.AND. IIF(;
.NOT. EMPTY(numpa), mespr <> nummes,;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpr), mespr = nummes,;
.T.)
                    GOTO TOP
                    SCAN
                         SELECT maepree
                         SEEK vperiodo +  ;
                              itehce.uniges +  ;
                              itehce.unieje +  ;
                              itehce.codcad
                         vkey = uniges +  ;
                                unieje +  ;
                                codfun +  ;
                                codprg +  ;
                                codspr +  ;
                                actpry +  ;
                                itehce.codcom +  ;
                                '00001' +  ;
                                itehce.codfte +  ;
                                itehce.codpart
                         vkey1 = uniges +  ;
                                 unieje +  ;
                                 codfun +  ;
                                 codprg +  ;
                                 codspr +  ;
                                 actpry +  ;
                                 itehce.codcom +  ;
                                 '00001' +  ;
                                 itehce.codfte
                         vmes = 'M_' +  ;
                                ALLTRIM(itehce.nummes)
                         SELECT rep1
                         SEEK vkey
                         IF FOUND()
                              REPLACE;
&vmes WITH &vmes+IIF(IteHce.TIPOPE='-',IteHce.VALPART*-1,IteHce.VALPART)
                         ELSE
                              GOTO  ;
                               TOP
                              LOCATE  ;
                               FOR  ;
                               LEFT(estfun,  ;
                               30) +  ;
                               codfte =  ;
                               vkey1
                              IF FOUND()
                                   APPEND BLANK
                                   REPLACE CODPART WITH IteHce.CODPART, PERIODO WITH VPERIODO, CODCAD  WITH IteHce.CODCAD, CODFTE  WITH IteHce.CODFTE, ESTFUN  WITH LEFT(VKEY,30), &vmes   WITH IIF(IteHce.TIPOPE='-',IteHce.VALPART*-1,IteHce.VALPART)
                              ENDIF
                         ENDIF
                         SELECT itehce
                    ENDSCAN
                    SELECT 8
                    USE
                    USE IN 8  ;
                        IteHc  ;
                        ALIAS  ;
                        itehc  ;
                        ORDER  ;
                        Itehc1
                    SELECT 3
                    USE
                    USE IN 3  ;
                        maepre  ;
                        ALIAS  ;
                        maepre  ;
                        ORDER  ;
                        maepre1
               ENDIF
               SELECT itepar
               SET ORDER TO ITEPAR4
               SELECT rep1
               GOTO TOP
               SCAN
                    vkey = periodo +  ;
                           LEFT(estfun,  ;
                           30) +  ;
                           codfte
                    SELECT itepar
                    SEEK vkey
                    IF FOUND()
                         REPLACE rep1.codcad  ;
                                 WITH  ;
                                 itepar.codcad
                    ENDIF
                    SELECT rep1
               ENDSCAN
               SELECT rep1
               vind = SYS(3) +  ;
                      '.IDX'
               INDEX ON  ;
                     LEFT(estfun,  ;
                     5) + codcad +  ;
                     codfte +  ;
                     codpart TO  ;
                     (vind)
               SET INDEX TO (vind)
               RELEASE WINDOW  ;
                       xwait
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
                                 'SalPrem1',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ELSE
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'SalPrem2',  ;
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
                                 'SalPrmG1',  ;
                                 ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                                 1,  ;
                                 .F.,  ;
                                 .T.
                         ELSE
                              DO reporte  ;
                                 WITH  ;
                                 2,  ;
                                 'SalPrmG2',  ;
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
                 FROM 6, 10 TO 18,  ;
                 70 FLOAT TITLE  ;
                 ' °°  Marco de Ampliaci¢n °° '  ;
                 DOUBLE COLOR  ;
                 SCHEME 5
          ACTIVATE WINDOW lis_1
          @ 1, 2 SAY  ;
            '     Periodo : ' GET  ;
            vperiodo PICTURE '!!'  ;
            VALID  .NOT.  ;
            EMPTY(vperiodo)
          @ 3, 2 SAY  ;
            '   Al Mes de : ' GET  ;
            vcalend PICTURE '!!'  ;
            VALID  ;
            val_para(vcalend, ;
            'FECMES',' ',18,25)
          @ 5, 2 SAY  ;
            '  U. Gestora : ' GET  ;
            vuniges PICTURE '!!'  ;
            VALID  ;
            val_para(vuniges, ;
            'UNIGES',' ',18,30)
          @ 7, 2 SAY  ;
            'U. Ejecutora : ' GET  ;
            vunieje PICTURE '!!!'  ;
            VALID  ;
            val_para1(vunieje, ;
            'UNIEJE' + vuniges, ;
            ' ',18,30)
          @ 9, 2 SAY  ;
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
               RETURN
          ENDIF
          DEFINE WINDOW xwait  ;
                 FROM 20, 06 TO  ;
                 22, 78 COLOR  ;
                 SCHEME 05
          ACTIVATE WINDOW xwait
          @ 0, 10 SAY  ;
            ' Espere un Momento...Procesando Ampliaciones del mes !'  ;
            COLOR W+/RB* 
          vdbf = SYS(3) + '.DBF'
          vind = SYS(3) + '.IDX'
          SELECT calen
          COPY TO (vdbf) FOR  ;
               nummes =  ;
               ALLTRIM(vcalend)  ;
               .AND. periodo =  ;
               ALLTRIM(vperiodo)  ;
               .AND. IIF( .NOT.  ;
               EMPTY(ALLTRIM(vuniges)),  ;
               SUBSTR(estfun, 1,  ;
               2) =  ;
               ALLTRIM(vuniges),  ;
               .T.) .AND. IIF(  ;
               .NOT.  ;
               EMPTY(ALLTRIM(vunieje)),  ;
               SUBSTR(estfun, 3,  ;
               3) =  ;
               ALLTRIM(vunieje),  ;
               .T.) .AND. IIF(  ;
               .NOT.  ;
               EMPTY(ALLTRIM(vcodfte)),  ;
               codfte =  ;
               ALLTRIM(vcodfte),  ;
               .T.) .AND. ampliar >  ;
               0
          RELEASE WINDOW xwait
          USE IN 0 (vdbf) ALIAS  ;
              amplia
          SELECT amplia
          IF EOF()
               RELEASE WINDOW  ;
                       xwait
               DO standby WITH  ;
                  vmens08
          ELSE
               INDEX ON codfte +  ;
                     codcad +  ;
                     codpart TO  ;
                     (vind)
               GOTO TOP
               DO reporte WITH 2,  ;
                  'Ampliar',  ;
                  ' Ampliaciones ',  ;
                  1, .F., .T.
               USE
          ENDIF
          ERASE (vdbf)
          ERASE (vind)
          SELECT calen
          SET FILTER TO
          SET ORDER TO CALEN2
ENDCASE
USE IN 18 repocal
USE IN 19 repcal1
SELECT calen
SET ORDER TO CALEN2
SET RELATION TO periodo + uniges + unieje;
+ codcad + codfte + codpart INTO itepar
SET SKIP TO itepar
GOTO vrecno
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
RELEASE WINDOW wind_0
RELEASE WINDOW wind_2
RELEASE WINDOW wind_2a
RELEASE WINDOW wind_1
RELEASE WINDOW wind_c1
RELEASE MENU mmenu
ON KEY LABEL F9
RESTORE SCREEN FROM principal
RETURN
*
PROCEDURE pendiente
DO CASE
     CASE estado = '00'
          REPLACE estado WITH  ;
                  '10'
     CASE estado = '10'
          REPLACE estado WITH  ;
                  '20'
     CASE estado = '20'
          REPLACE estado WITH  ;
                  '00'
ENDCASE
DO vista
RETURN
*
FUNCTION sum00
PARAMETER vkey, vllave
vrec = RECNO()
GOTO TOP
SUM FTE00 TO suma FOR &vLLave = vkey 
GOTO vrec
RETURN suma
*
FUNCTION sum01
PARAMETER vkey, vllave
vrec = RECNO()
GOTO TOP
SUM FTE01 TO suma FOR &vLLave = vkey 
GOTO vrec
RETURN suma
*
FUNCTION sum09
PARAMETER vkey, vllave
vrec = RECNO()
GOTO TOP
SUM FTE09 TO suma FOR &vLLave = vkey 
GOTO vrec
RETURN suma
*
FUNCTION sumcal
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
SUM valpart+ampliar TO suma FOR &vFiltro=;
vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumcal1
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrecno = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro valpart +  ;
         ampliar TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro valpart +  ;
         ampliar TO suma
ENDIF
GOTO vrecno
RETURN suma
*
FUNCTION sumcom
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
SUM TOTAFE TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumcom1
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrecno = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro totafe TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro totafe  ;
         TO suma
ENDIF
GOTO vrecno
RETURN suma
*
FUNCTION sumar
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
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+left(codpart,2)'
ENDCASE
SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12;
 TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION summes
PARAMETER vcalen, part, vnivel
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
SUM &part TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION totprg
PARAMETER tota
vkey = codcad
vrec = RECNO()
GOTO TOP
SUM FOR codcad = vkey valpres +  ;
    IIF(tota = 1, 0, cresup +  ;
    transf) TO suma
GOTO vrec
RETURN suma
*
FUNCTION salprg
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
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+left(codpart,2)'
ENDCASE
SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12);
TO sumA FOR &vFiltro = vCalen
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
SUM valpRES+cresup+TRANSF TO suma FOR;
&vFiltro = vCalen
GOTO vrec
RETURN suma
*
FUNCTION salspr
vkey = codcad + codfte
vrec = RECNO()
GOTO TOP
SUM FOR codcad + codfte = vkey  ;
    valpres + cresup + transf -  ;
    (c_01 + c_02 + c_03 + c_04 +  ;
    c_05 + c_06 + c_07 + c_08 +  ;
    c_09 + c_10 + c_11 + c_12) TO  ;
    suma
GOTO vrec
RETURN suma
*
FUNCTION chequeo
PARAMETER vcodpart
ord = ORDER()
vc = RECNO()
vpartida = m.periodo + m.uniges +  ;
           m.unieje + m.codcad +  ;
           m.codfte + vcodpart
SEEK vpartida
IF FOUND()
     DO standby WITH  ;
        'Ya est  registrada esta partida '
     GOTO vc
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION chemon
PARAMETER vcodpart, vcantidad
valias = ALIAS()
ord = ORDER()
SELECT itepar
SEEK m.periodo + m.uniges +  ;
     m.unieje + m.codcad +  ;
     ALLTRIM(m.codfte) +  ;
     vcodpart
vresto = (itepar.totcal -  ;
         (calen.valpart +  ;
         calen.ampliar)) +  ;
         vcantidad
IF (valpart + cresup + tra001 +  ;
   tra003 + tra004 + tra005 -  ;
   vresto) < 0
     DO standby WITH  ;
        'No existe marco presupuestal para asignar.... Se excede en '+ ;
        STR(valpart+cresup+tra001+ ;
        tra003+tra004+tra005- ;
        (itepar.totcal+ ;
        calen.valpart+ ;
        calen.ampliar), 8)
     SELECT (valias)
     RETURN .F.
ENDIF
SELECT (valias)
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
         vfiltro valpres + cresup +  ;
         transf TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro valpres +  ;
         cresup + transf TO suma
ENDIF
GOTO vrecno
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
              c_01 + c_02 + c_03 +  ;
              c_04 + c_05 + c_06 +  ;
              c_07 + c_08 + c_09 +  ;
              c_10 + c_11 + c_12  ;
              TO suma
     CASE vtipo > 25 .AND. vtipo <  ;
          28
          SUM FOR LEFT(estfun,  ;
              25) + codfte =  ;
              vfiltro c_01 + c_02 +  ;
              c_03 + c_04 + c_05 +  ;
              c_06 + c_07 + c_08 +  ;
              c_09 + c_10 + c_11 +  ;
              c_12 TO suma
     CASE vtipo > 28 .AND. vtipo <  ;
          30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              LEFT(codpart, 2) =  ;
              vfiltro c_01 + c_02 +  ;
              c_03 + c_04 + c_05 +  ;
              c_06 + c_07 + c_08 +  ;
              c_09 + c_10 + c_11 +  ;
              c_12 TO suma
     CASE vtipo > 30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              codpart = vfiltro  ;
              c_01 + c_02 + c_03 +  ;
              c_04 + c_05 + c_06 +  ;
              c_07 + c_08 + c_09 +  ;
              c_10 + c_11 + c_12  ;
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
              valpres + cresup +  ;
              transf - (c_01 +  ;
              c_02 + c_03 + c_04 +  ;
              c_05 + c_06 + c_07 +  ;
              c_08 + c_09 + c_10 +  ;
              c_11 + c_12) TO  ;
              suma
     CASE vtipo > 25 .AND. vtipo <  ;
          28
          SUM FOR LEFT(estfun,  ;
              25) + codfte =  ;
              vfiltro valpres +  ;
              cresup + transf -  ;
              (c_01 + c_02 + c_03 +  ;
              c_04 + c_05 + c_06 +  ;
              c_07 + c_08 + c_09 +  ;
              c_10 + c_11 + c_12)  ;
              TO suma
     CASE vtipo > 28 .AND. vtipo <  ;
          30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              LEFT(codpart, 2) =  ;
              vfiltro valpres +  ;
              cresup + transf -  ;
              (c_01 + c_02 + c_03 +  ;
              c_04 + c_05 + c_06 +  ;
              c_07 + c_08 + c_09 +  ;
              c_10 + c_11 + c_12)  ;
              TO suma
     CASE vtipo > 30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              codpart = vfiltro  ;
              valpres + cresup +  ;
              transf - (c_01 +  ;
              c_02 + c_03 + c_04 +  ;
              c_05 + c_06 + c_07 +  ;
              c_08 + c_09 + c_10 +  ;
              c_11 + c_12) TO  ;
              suma
ENDCASE
GOTO vrec
RETURN suma
*
FUNCTION sumpre1
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
SUM valpres+cresup+transf TO suma FOR;
&vFiltro= vCalen
GOTO vrec
RETURN suma
*
PROCEDURE case2
DEFINE WINDOW lis_1 FROM 08, 13  ;
       TO 15, 67 FLOAT TITLE  ;
       ' °° Listado Calendario °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis_1
@ 1, 2 SAY '   Periodo : ' GET  ;
  vperiodo PICTURE '!!' VALID   ;
  .NOT. EMPTY(vperiodo)
@ 2, 2 SAY '       Mes : ' GET  ;
  vcalend PICTURE '!!' VALID  ;
  val_para(vcalend,'FECMES',' ', ;
  16,25)
@ 3, 2 SAY 'Cad. FunC. : ' GET  ;
  vcodcad PICTURE '!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodcad),  ;
  val_codcad(vcodcad,vperiodo,' ', ;
  16,25), .T.)
@ 4, 2 SAY ' Fte. Fto. : ' GET  ;
  vcodfte PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vcodfte),  ;
  val_para(vcodfte,'CODFTE',' ', ;
  16,30), .T.)
READ VALID val_read()
DEACTIVATE WINDOW lis_1
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
SELECT calen
GOTO TOP
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     SELECT repo
     yind = SYS(3) + '.IDX'
     INDEX ON periodo + nummes +  ;
           codcad + codpart TO  ;
           (yind)
     SET INDEX TO (yind)
     SELECT calen
     SET FILTER TO nummes = ALLTRIM(vcalend);
.AND. periodo = ALLTRIM(periodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.)
     GOTO TOP
     SCAN
          SCATTER MEMVAR
          vrep2 = m.periodo +  ;
                  m.nummes +  ;
                  m.codcad +  ;
                  m.codpart
          SELECT repo
          SEEK ALLTRIM(vrep2)
          IF  .NOT. FOUND()
               vcod = 'FTE' +  ;
                      m.codfte
               APPEND BLANK
               m.&vcod=m.valpart
               GATHER MEMVAR
               m.&vcod=0
          ELSE
               vcod = 'FTE' +  ;
                      m.codfte
               IF RLOCK()
                    REPLACE &vcod WITH;
&vcod+m.valpart
               ENDIF
               UNLOCK
          ENDIF
          SELECT calen
     ENDSCAN
     SELECT repo
     GOTO TOP
     DO reporte WITH 2,  ;
        'LisCalIN',  ;
        'Listado de Calendarios '
ENDIF
*
FUNCTION val_saldo
PARAMETER vcodpart, vmes
vrecno = RECNO()
vtotal = 0
SEEK m.periodo + m.uniges +  ;
     m.unieje + ALLTRIM(m.codcad) +  ;
     ALLTRIM(m.codfte) + vcodpart +  ;
     vmes
vtipo1 = m.periodo + m.uniges +  ;
         m.unieje +  ;
         ALLTRIM(m.codcad) +  ;
         ALLTRIM(m.codfte) +  ;
         vcodpart
DO CASE
     CASE vmes < '04'
          DO WHILE nummes<=vmes  ;
             .AND. periodo+uniges+ ;
             unieje+codcad+codfte+ ;
             codpart=vtipo1 .AND.   ;
             .NOT. BOF()
               vtotal = vtotal +  ;
                        valpart
               SKIP -1
          ENDDO
     CASE vmes > '03' .AND. vmes <  ;
          '07'
          DO WHILE nummes>'03'  ;
             .AND. nummes<=vmes  ;
             .AND. periodo+uniges+ ;
             unieje+codcad+codfte+ ;
             codpart=vtipo1 .AND.   ;
             .NOT. BOF()
               vtotal = vtotal +  ;
                        valpart
               SKIP -1
          ENDDO
     CASE vmes > '06' .AND. vmes <  ;
          '10'
          DO WHILE nummes>'06'  ;
             .AND. nummes<=vmes  ;
             .AND. periodo+uniges+ ;
             unieje+codcad+codfte+ ;
             codpart=vtipo1 .AND.   ;
             .NOT. BOF()
               vtotal = vtotal +  ;
                        valpart
               SKIP -1
          ENDDO
     CASE vmes > '09'
          DO WHILE nummes>'09'  ;
             .AND. nummes<=vmes  ;
             .AND. periodo+uniges+ ;
             unieje+codcad+codfte+ ;
             codpart=vtipo1 .AND.   ;
             .NOT. BOF()
               vtotal = vtotal +  ;
                        valpart
               SKIP -1
          ENDDO
ENDCASE
GOTO vrecno
RETURN vtotal
*

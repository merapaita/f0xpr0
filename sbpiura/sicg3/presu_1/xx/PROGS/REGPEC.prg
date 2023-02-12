PARAMETER vopcion, vtipfun
SET EXCLUSIVE OFF
USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 2 calen ALIAS calen ORDER  ;
    calen1
USE IN 3 Pecosa ALIAS pecosa  ;
    ORDER Pecosa1
USE IN 4 ItePec ALIAS itepec  ;
    ORDER ItePec1
USE IN 5 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 6 IteArt ALIAS iteart  ;
    ORDER IteArt1
USE IN 7 cdrnec ALIAS cuadro  ;
    ORDER Cdrnec1
USE IN 8 itecn ALIAS itecn ORDER  ;
    itecn1
USE IN 10 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 11 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 13 OrdCom ALIAS orden  ;
    ORDER Ordcom1
USE IN 20 USUARIO ALIAS usu ORDER  ;
    USUARIO1
vmens01 = ' Pecosas : REVISION '
vmens02 = 'Registro de Pecosa'
vmens04 = 'Dicho Pecosa no fue encontrado'
vmens05 = 'No existe Pecosa anterior'
vmens06 = 'No existe Pecosa siguiente'
vmens07 = '� Desea Anular �ste Pecosa ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Pecosa ha sido anulado'
vmens10 = 'El Pecosa ya est� Atendido'
vmens11 = 'El Pecosa ha sido devuelto'
vmens12 = 'El Pecosa ya tiene O/C'
PUBLIC vant, gh, vnp
SELECT pecosa
GOTO BOTTOM
IF vopcion = 2
     SET FILTER TO IIF(vflag = '*',;
.T., coddep = SUBSTR(vcoddep, 1, vnumdep))
ENDIF
ON KEY LABEL F4 do imprimir
ON KEY LABEL F5 do lprecioS
ON KEY LABEL F6 do lprecioX
SCATTER BLANK MEMVAR
DO inicia
DO pantalla
DO vista
HIDE POPUP ALL
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular   Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 13, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 14, 00  ;
       TO 23, 79 TITLE  ;
       ' Detalle :Pecosa   �F9� Detalle :Item     �F4�Imprime  �F5�Art.Reg '  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 14, 00  ;
       TO 23, 79 TITLE  ;
       ' �F7�Seguimiento   �F9�Detalle Item   �Esc� Sale   �F3�Cdro Nec.'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_4 FROM 08, 00  ;
       TO 10, 79 TITLE  ;
       ' Detalle: Cuadro Necesidades '  ;
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
       'a\<Nular ' AT 24, 54
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
ON SELECTION PAD anula OF mmenu DO anula
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_1
CLEAR
@ 1, 2 SAY '            Fecha :'
@ 1, 38 SAY '    N�mero Pecosa :'
@ 2, 2 SAY '      Dependencia :'
@ 3, 2 SAY '             Atte :'
@ 4, 2 SAY '      Cadena Fun. :'
@ 5, 2 SAY ' Fte. Funcionami. :'
@ 6, 2 SAY '          Funci�n :'
@ 7, 2 SAY '         Programa :'
@ 8, 2 SAY '      SubPrograma :'
@ 9, 2 SAY '   Activ./Proyec. :'
@ 10, 2 SAY '          Destino :'
@ 11, 2 SAY '    Observaciones :'
RETURN
*
PROCEDURE vista
SELECT pecosa
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
ON KEY LABEL F9 DO vista_det
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo + '01001','C')
IF user_tp $ 'E'
     @ 1, 22 SAY m.fecpec
     @ 1, 60 SAY m.periodo
     @ 1, 63 SAY m.numpec
     DO standby WITH  ;
        'Pecosa Elaborando por '+ ;
        user
     IF  .NOT. BOF()
          SKIP -1
          DO vista
     ENDIF
ELSE
     @ 0, 00 SAY IIF(m.user_tp =  ;
       'C', PADC( ;
       '�Corregido por ' + IIF(  ;
       .NOT. EMPTY(m.user_cr),  ;
       ALLTRIM(m.user_cr),  ;
       ALLTRIM(m.user)) + '�', 79,  ;
       ' '), PADC( ;
       '�Elaborado por ' +  ;
       ALLTRIM(m.user) + '�', 79,  ;
       ' '))
     @ 0, 02 SAY IIF(m.tippec =  ;
       'S', 'Pecosa Stock     ',  ;
       IIF(m.tippec = 'O',  ;
       'Pecosa Compra    ',  ;
       IIF(m.tippec = 'R',  ;
       'Pecosa Referencia',  ;
       IIF(m.tippec = 'T',  ;
       'Pecosa Transporte',  ;
       'Pecosa Caja Chica'))))  ;
       COLOR SCHEME 02
     @ 0, 60 SAY  ;
       vestpec(m.estado) COLOR  ;
       SCHEME 02
     @ 1, 22 SAY m.fecpec
     @ 1, 63 SAY m.numpec
     @ 2, 22 SAY  ;
       val_para(m.coddep,'CODDEP', ;
       'A',22,50)
     @ 3, 22 SAY m.atte
     @ 4, 22 SAY  ;
       val_codcad(m.codcad, ;
       m.periodo + '01001','V',22, ;
       40)
     @ 5, 22 SAY  ;
       val_para(m.codfte,'CODFTE', ;
       'V',22,40)
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
     @ 9, 22 SAY  ;
       val_para(maepre.actpry, ;
       'ACTPRY','V',22,40)
     @ 10, 22 SAY m.destino  ;
       PICTURE '@S56'
     @ 11, 22 SAY m.observa
     DO vista_hijo
ENDIF
RETURN
*
PROCEDURE vista_hijo
PRIVATE vest
HIDE POPUP ALL
SELECT itepec
SEEK m.periodo + m.numpec
vest00 = 0
vest20 = 0
vest30 = 0
vest50 = 0
vest99 = 0
vfte = 0
SCAN WHILE itepec.periodo +  ;
     itepec.numpec = m.periodo +  ;
     m.numpec
     IF itepec.codfte <>  ;
        ALLTRIM(m.codfte)
          vfte = vfte + 1
     ENDIF
     DO CASE
          CASE itepec.estado =  ;
               '00'
               vest00 = vest00 +  ;
                        1
          CASE itepec.estado =  ;
               '20'
               vest20 = vest20 +  ;
                        1
          CASE itepec.estado =  ;
               '30'
               vest30 = vest30 +  ;
                        1
          CASE itepec.estado =  ;
               '50'
               vest50 = vest50 +  ;
                        1
          CASE itepec.estado =  ;
               '99'
               vest99 = vest99 +  ;
                        1
     ENDCASE
ENDSCAN
IF vfte <> 0
     DO standby WITH  ;
        'Cambio de Fte. de Financto ... Revise'
ENDIF
IF m.estado <> '50'
     DO CASE
          CASE vest00 <> 0
               SELECT pecosa
               IF RLOCK()
                    REPLACE pecosa.estado  ;
                            WITH  ;
                            '00'
               ENDIF
               UNLOCK
          CASE vest20 <> 0 .AND.  ;
               vest30 = 0
               SELECT pecosa
               IF RLOCK()
                    REPLACE pecosa.estado  ;
                            WITH  ;
                            '20'
               ENDIF
               UNLOCK
          CASE vest30 <> 0 .AND.  ;
               vest50 = 0
               SELECT pecosa
               IF RLOCK()
                    REPLACE pecosa.estado  ;
                            WITH  ;
                            '30'
               ENDIF
               UNLOCK
     ENDCASE
ENDIF
SELECT itepec
SEEK m.periodo + m.numpec
BROWSE NOOPTIMIZE FIELDS xx =  ;
       IIF(estado = '50', '�',  ;
       '') :H = '�' :W = .F.,  ;
       numord :H = 'Od' :W = .F.,  ;
       codart :H = 'C�digo' :V =  ;
       valart() :F :W =  ;
       EMPTY(codart), descri :H =  ;
       'Descripci�n' : 35 :W =  ;
       .F., coduni :H = 'Uni' :W =  ;
       .F. : 3, canreq :H =  ;
       'Cantd' :P = '99,999.99'  ;
       :W =  .NOT. EMPTY(codart),  ;
       codfte :H = 'Fte' :P =  ;
       '!!' :W = .F., ess =  ;
       IIF(estado = '00',  ;
       'Pendte', IIF(estado =  ;
       '20', 'SC' + numsc,  ;
       IIF(estado = '99',  ;
       'Anulad', IIF(estado =  ;
       '50' .AND. m.tippec = 'S',  ;
       'Atendido', 'OC' +  ;
       numoc)))) :H = 'Estado' :W =  ;
       .F., preuni :H = 'Precio '  ;
       :P = '99,999.99' :W = .F.  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE NOCLEAR WINDOW  ;
       wind_2 KEY m.periodo +  ;
       m.numpec TIMEOUT 0.001   ;
       NOREFRESH
SELECT pecosa
RETURN
*
PROCEDURE consult
USE IN 5
USE IN 6
USE IN 7
USE IN 8
USE IN 14 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 15 Cheque ALIAS cheque  ;
    ORDER Cheque1
USE IN 16 Compag ALIAS compag  ;
    ORDER Compag1
DO estado WITH 'PE',  ;
   'ItePec.Periodo+Itepec.Numoc+Itepec.Codfte'
USE IN 14
USE IN 15
USE IN 16
USE IN 5 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 6 IteArt ALIAS iteart  ;
    ORDER IteArt1
USE IN 7 cdrnec ALIAS cuadro  ;
    ORDER Cdrnec1
USE IN 8 itecn ALIAS itecn ORDER  ;
    itecn3
USE IN 14 HojCon ALIAS hoja ORDER  ;
    HojCon1
RETURN
*
PROCEDURE vista_det
HIDE POPUP ALL
ON KEY LABEL F9 DO OBSERVA
ON KEY LABEL F7 DO CONSULT 
ON KEY LABEL F3 DO MUSCDR
SELECT itepec
SEEK m.periodo + m.numpec
BROWSE NOOPTIMIZE FIELDS codart  ;
       :H = 'C�digo' :V =  ;
       valart() :F :W =  ;
       EMPTY(codart), artcod :H =  ;
       'Cd', descri :H =  ;
       'Descripci�n' : 38 :W =  ;
       .F., coduni :H = 'Uni' :W =  ;
       .F. : 3, canreq :H =  ;
       'Cantd' :P = '99,999.99'  ;
       :W =  .NOT. EMPTY(codart),  ;
       codfte :H = 'Fte' :P =  ;
       '!!' :W = .F., ess =  ;
       IIF(estado = '00',  ;
       'Pendte', IIF(estado =  ;
       '20', 'SC' + numsc,  ;
       IIF(estado = '99',  ;
       'Anulad', IIF(estado =  ;
       '50', 'Atendi', 'OC' +  ;
       numoc)))) :H = 'Estado' :W =  ;
       .F., preuni :H = 'Precio '  ;
       :P = '99,999.99' :W = .F.  ;
       NOMENU NOAPPEND NODELETE  ;
       NOCLEAR WINDOW wind_3 KEY  ;
       m.periodo + m.numpec  ;
       NOREFRESH
ON KEY LABEL F9 DO vista_det
ON KEY LABEL F7
ON KEY LABEL F3
SELECT pecosa
DO vista
RETURN
*
PROCEDURE revis
ON KEY LABEL F7
ON KEY LABEL F9
SELECT pecosa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SET RELATION TO periodo + numpec INTO;
itepec
SET SKIP TO itepec
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '�����������Presione �F10� para seleccionar  o  �Esc� para cancelar������������'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS numpec :H = ' N� ',  ;
       est = IIF(itepec.estado =  ;
       '00', 'Pend',  ;
       IIF(itepec.estado = '30',  ;
       itepec.numoc, IIF(estado =  ;
       '99', 'Anul',  ;
       IIF(itepec.estado = '50',  ;
       'Aten', 'S/Ct')))) :H =  ;
       'Estd', codfte :H = 'Fte ',  ;
       fecpec :H = 'Fecha',  ;
       coddep :H = 'DEP',  ;
       itepec.canreq :H =  ;
       'Cantid' :P = '99,999.99',  ;
       itepec.descri :H =  ;
       'Detalle ' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_0
vtempo = '��������������������������������������������������������������������������������'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SET RELATION TO
SELECT pecosa
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
ON KEY LABEL F7
ON KEY LABEL F9
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnum_pec = '    '
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N�mero Pecosa: ' GET  ;
  m.periodo PICTURE '!!'
@ 1, 27 SAY '-' GET vnum_pec  ;
  PICTURE '!!!!' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_pec) .OR. LASTKEY() =  ;
   27
     ON KEY LABEL F9 DO vista_det
     RETURN
ELSE
     SEEK vperiodo + vnum_pec
     IF  .NOT. FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
ON KEY LABEL F9 DO vista_det
RETURN
*
FUNCTION vbusca
vnum_pec = PADL(ALLTRIM(vnum_pec),  ;
           4, '0')
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
ON KEY LABEL F7
ON KEY LABEL F9
PUBLIC vmes, vpart
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
IF estado = '30'
     DO standby WITH  ;
        'La pecosa ya tiene O/C'
     RETURN
ENDIF
IF estado = '40'
     DO standby WITH  ;
        'La pecosa ya est� despachado '
     RETURN
ENDIF
SELECT pecosa
IF escolor
     DEFINE POPUP xcot  FROM 16,65 SHADOW;
COLOR &L_COL
ELSE
     DEFINE POPUP xcot FROM 16,  ;
            65 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF xcot PROMPT  ;
       ' \<Stock     '
DEFINE BAR 2 OF xcot PROMPT  ;
       ' \<Compra    '
DEFINE BAR 3 OF xcot PROMPT  ;
       ' \<Transporte'
DEFINE BAR 4 OF xcot PROMPT  ;
       ' \<Referencia'
ON SELECTION POPUP xcot DEACTIVATE POPUP
ACTIVATE POPUP xcot
DO CASE
     CASE BAR() = 1
          vtipo = 'S'
     CASE BAR() = 2
          vtipo = 'O'
     CASE BAR() = 3
          vtipo = 'T'
     CASE BAR() = 4
          vtipo = 'R'
     OTHERWISE
ENDCASE
RELEASE POPUP xcot
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado'
     DO vista
     RETURN
ENDIF
SCATTER MEMVAR
m.tippec = vtipo
ACTIVATE WINDOW wind_1
DO pantalla
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo + '01001','C')
@ 0, 02 SAY IIF(m.tippec = 'S',  ;
  'Pecosa Stock     ',  ;
  IIF(m.tippec = 'O',  ;
  'Pecosa Compra    ',  ;
  IIF(m.tippec = 'R',  ;
  'Pecosa Referencia',  ;
  IIF(m.tippec = 'T',  ;
  'Pecosa Transporte',  ;
  'Pecosa Caja Chica')))) COLOR  ;
  SCHEME 02
@ 1, 22 GET m.fecpec PICTURE '@D'
@ 1, 60 GET m.periodo PICTURE  ;
  '!!' DISABLE
@ 1, 62 SAY '-'
@ 1, 63 GET m.numpec PICTURE  ;
  '!!!!' DISABLE
@ 2, 22 GET m.coddep PICTURE  ;
  '!!!!!!' VALID val_dep(m.coddep, ;
  'CODDEP',' ',22,40) .AND.  ;
  valatte()
@ 3, 22 GET m.atte VALID  .NOT.  ;
  EMPTY(m.coddep)
@ 4, 22 GET m.codcad PICTURE  ;
  '!!!!' VALID  ;
  val_codcad(m.codcad,m.periodo +  ;
  '01001',' ',22,40)
@ 5, 22 GET m.codfte PICTURE '!!'  ;
  VALID val_para(m.codfte, ;
  'CODFTE',' ',22,40)
READ VALID val_read()
IF LASTKEY() = 27
     DO vista
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
@ 9, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 10, 22 GET m.destino PICTURE  ;
  '@S49'
@ 11, 22 GET m.observa PICTURE  ;
  '@S56'
READ VALID val_read()
IF LASTKEY() <> 27
     DO WHILE .T.
          ok = trabaja_hj()
          IF LASTKEY() <> 27
               IF yesno( ;
                  '� Conforme la correcci�n ?' ;
                  )
                    EXIT
               ENDIF
          ELSE
               IF yesno( ;
                  '� Cancela la correcci�n ?' ;
                  )
                    ok = .F.
                    EXIT
               ENDIF
          ENDIF
     ENDDO
     IF ok .AND. LASTKEY() <> 27
          SELECT itepec
          SEEK m.periodo +  ;
               m.numpec
          vorer = 1
          SCAN WHILE m.periodo =  ;
               periodo .AND.  ;
               m.numpec = numpec
               IF RLOCK()
                    IF EMPTY(codart)  ;
                       .OR.  ;
                       EMPTY(descri)  ;
                       .OR.  ;
                       canreq =  ;
                       0
                         DELETE NEXT  ;
                                1
                    ELSE
                         REPLACE codcad  ;
                                 WITH  ;
                                 m.codcad,  ;
                                 tippec  ;
                                 WITH  ;
                                 m.tippec,  ;
                                 codfte  ;
                                 WITH  ;
                                 m.codfte,  ;
                                 coddep  ;
                                 WITH  ;
                                 m.coddep
                         IF EMPTY(numord)
                              REPLACE  ;
                               numord  ;
                               WITH  ;
                               PADL(vorer,  ;
                               2,  ;
                               '0')
                         ENDIF
                         vorer = vorer +  ;
                                 1
                    ENDIF
               ENDIF
          ENDSCAN
          IF ok .AND. LASTKEY() <>  ;
             27
               SELECT pecosa
               m.user_cr = SYS(0)
               m.user_fc = DATE()
               m.user_tp = 'C'
               GATHER MEMVAR
          ELSE
               SELECT itepec
          ENDIF
     ELSE
          SELECT pecosa
     ENDIF
ENDIF
DO vista
UNLOCK
RETURN
*
PROCEDURE ingre
PUBLIC ast, rec, vmes, vpart,  ;
       vtipo
ON KEY LABEL F7
ON KEY LABEL F9
IF escolor
     DEFINE POPUP xcot  FROM 16,65 SHADOW;
COLOR &L_COL
ELSE
     DEFINE POPUP xcot FROM 16,  ;
            65 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF xcot PROMPT  ;
       ' \<Stock     '
DEFINE BAR 2 OF xcot PROMPT  ;
       ' \<Compra    '
DEFINE BAR 3 OF xcot PROMPT  ;
       ' \<Transporte'
DEFINE BAR 4 OF xcot PROMPT  ;
       ' \<Referencia'
DEFINE BAR 5 OF xcot PROMPT  ;
       ' c\<Aja Chica'
ON SELECTION POPUP xcot DEACTIVATE POPUP
ACTIVATE POPUP xcot
DO CASE
     CASE BAR() = 1
          vtipo = 'S'
     CASE BAR() = 2
          vtipo = 'O'
     CASE BAR() = 3
          vtipo = 'T'
     CASE BAR() = 4
          vtipo = 'R'
     CASE BAR() = 5
          vtipo = 'C'
     OTHERWISE
ENDCASE
RELEASE POPUP xcot
IF LASTKEY() = 27 .OR.  .NOT.  ;
   vtipo $ 'SOTR'
     DO standby WITH  ;
        'Proceso cancelado'
     DO vista
     RETURN
ENDIF
DO pantalla
SELECT pecosa
rec = RECNO()
ast = ORDER()
SCATTER BLANK MEMVAR
m.periodo = STR(YEAR(DATE()) -  ;
            1900, 2)
= repasa()
m.fecpec = DATE()
m.tippec = vtipo
SET CONFIRM OFF
@ 0, 02 SAY IIF(m.tippec = 'S',  ;
  'Pecosa Stock     ',  ;
  IIF(m.tippec = 'O',  ;
  'Pecosa Compra    ',  ;
  IIF(m.tippec = 'R',  ;
  'Pecosa Referencia',  ;
  IIF(m.tippec = 'T',  ;
  'Pecosa Transporte',  ;
  'Pecosa Caja Chica')))) COLOR  ;
  SCHEME 02
@ 1, 22 GET m.fecpec PICTURE '@D'
@ 1, 60 GET m.periodo DISABLE
@ 1, 62 SAY '-'
@ 1, 63 GET m.numpec PICTURE  ;
  '!!!!' DISABLE
@ 2, 22 GET m.coddep PICTURE  ;
  '!!!!!!' VALID val_dep(m.coddep, ;
  'CODDEP',' ',22,40,7) .AND.  ;
  valatte()
@ 3, 22 GET m.atte VALID  .NOT.  ;
  EMPTY(m.coddep)
@ 4, 22 GET m.codcad PICTURE  ;
  '!!!!' VALID  ;
  val_codcad(m.codcad,m.periodo +  ;
  '01001',' ',22,30)
@ 5, 22 GET m.codfte PICTURE '!!'  ;
  VALID val_para(m.codfte, ;
  'CODFTE',' ',22,30)
READ VALID val_read()
IF LASTKEY() = 27
     SELECT pecosa
     GOTO gh
     IF numpec =  ;
        ALLTRIM(m.numpec)
          DELETE NEXT 1
     ENDIF
     GOTO rec
     DO vista
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
@ 9, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 10, 22 GET m.destino PICTURE  ;
  '@S49'
@ 11, 22 GET m.observa PICTURE  ;
  '@S56'
READ VALID val_read()
SET CONFIRM ON
IF LASTKEY() <> 27
     DO WHILE .T.
          ok = trabaja_hj()
          IF LASTKEY() <> 27
               IF yesno( ;
                  '� Confirme el ingreso ?' ;
                  )
                    EXIT
               ENDIF
          ELSE
               DO standby WITH  ;
                  ' Cancelado el Ingreso ...'
               ok = .F.
               EXIT
          ENDIF
     ENDDO
     IF ok .AND. LASTKEY() <> 27
          SELECT itepec
          SEEK m.periodo +  ;
               m.numpec
          vnumord = 0
          SCAN WHILE m.periodo =  ;
               periodo .AND.  ;
               m.numpec = numpec
               IF f_lock(1)
                    IF EMPTY(codart)  ;
                       .OR.  ;
                       EMPTY(descri)  ;
                       .OR.  ;
                       canreq =  ;
                       0
                         DELETE NEXT  ;
                                1
                    ELSE
                         vnumord =  ;
                          vnumord +  ;
                          1
                         REPLACE codcad  ;
                                 WITH  ;
                                 m.codcad,  ;
                                 tippec  ;
                                 WITH  ;
                                 m.tippec,  ;
                                 numord  ;
                                 WITH  ;
                                 PADL(ALLTRIM(STR(vnumord,  ;
                                 2)),  ;
                                 2,  ;
                                 '0'),  ;
                                 coddep  ;
                                 WITH  ;
                                 m.coddep
                    ENDIF
               ENDIF
          ENDSCAN
          SELECT pecosa
          GOTO gh
          IF  .NOT.  ;
              EMPTY(m.numpec)
               m.user = SYS(0)
               m.user_fc = DATE()
               m.user_tp = 'I'
               m.estado = '00'
               GATHER MEMVAR
          ENDIF
     ELSE
          SELECT itepec
          SEEK m.periodo +  ;
               m.numpec
          SCAN WHILE m.periodo =  ;
               periodo .AND.  ;
               m.numpec = numpec
               IF f_lock(1)
                    DELETE NEXT 1
               ENDIF
          ENDSCAN
          UNLOCK
          SELECT pecosa
          GOTO gh
          IF numpec =  ;
             ALLTRIM(m.numpec)
               DELETE NEXT 1
          ENDIF
          GOTO rec
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
     SELECT pecosa
     GOTO gh
     IF numpec =  ;
        ALLTRIM(m.numpec)
          DELETE NEXT 1
     ENDIF
     GOTO rec
ENDIF
SELECT pecosa
IF vopcion = 2
     SET FILTER TO IIF(vflag = '*',;
.T., coddep = SUBSTR(vcoddep, 1, vnumdep))
ENDIF
DO vista
RETURN
*
PROCEDURE lprecios
PRIVATE as
as = ALIAS()
SELECT itepec
od = ORDER()
SET ORDER TO itepec10
SET FILTER TO estado <> '9'
GOTO TOP
DO reporte WITH 2, 'lprecio',  ;
   ' Lista por Productos ', 1,  ;
   .F., .T.
SET FILTER TO
SET ORDER TO (od)
IF vopcion = 2
     SET FILTER TO IIF(vflag = '*',;
.T., coddep = SUBSTR(vcoddep, 1, vnumdep))
ENDIF
SELECT (as)
RETURN
*
PROCEDURE lpreciox
PRIVATE as
as = ALIAS()
SELECT itepec
od = ORDER()
SET ORDER TO itepec10
SET FILTER TO estado <> '9'
GOTO TOP
DO reporte WITH 2, 'lprecioX',  ;
   ' Lista por Productos ', 1,  ;
   .F., .T.
SET FILTER TO
SET ORDER TO (od)
IF vopcion = 2
     SET FILTER TO IIF(vflag = '*',;
.T., coddep = SUBSTR(vcoddep, 1, vnumdep))
ENDIF
SELECT (as)
RETURN
*
PROCEDURE val_pe
m.numpec = PADL(ALLTRIM(vnumpec),  ;
           4, '0')
RETURN
*
FUNCTION sihay
PRIVATE as
as = RECNO()
SEEK m.periodo + m.numpec
IF FOUND() .AND. RECNO() <> as
     DO standby WITH  ;
        'El Pecosa ya esta registrado'
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE valatte
SELECT cuadro
SEEK m.periodo + m.coddep
IF FOUND()
     m.atte = IIF(EMPTY(m.atte)  ;
              .OR. m.atte =  ;
              'No se Registra Solicitante..,revise',  ;
              cuadro.atte,  ;
              m.atte)
ELSE
     m.atte = 'No se Registra Solicitante..,revise'
ENDIF
SELECT pecosa
RETURN
*
FUNCTION trabaja_hj
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '��������F5->Agregar��������������F8->Eliminar��������������F10->Terminar��������'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO Agreg_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itepec
SEEK m.periodo + m.numpec
IF  .NOT. FOUND()
     DO agreg_item
ENDIF
BROWSE FIELDS codart :H =  ;
       'C�digo' :V =  ;
       val_artc(codart) .AND.  ;
       antr() :F, descri :H =  ;
       'Descripci�n' : 43 :W =  ;
       .F., coduni :H = 'Uni' :W =  ;
       .F. : 5, canreq :H =  ;
       'Cantd' :P = '99,999.99'  ;
       :V = IIF(itepec.tipcdr =  ;
       'S', valor(), .T.) :F :W =   ;
       .NOT. EMPTY(codart), ess =  ;
       IIF(estado = '00',  ;
       'Pendte', IIF(estado =  ;
       '20', 'SC' + numsc,  ;
       IIF(estado = '99',  ;
       'Anulad', IIF(estado =  ;
       '50', 'Atendi', 'OC' +  ;
       numoc)))) :H = 'Estado' :W =  ;
       .F. NOMENU NOAPPEND  ;
       NODELETE WINDOW wind_2 KEY  ;
       m.periodo + m.numpec  ;
       NOREFRESH
UNLOCK ALL
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE SCREEN
vtempo = '��������������������������������������������������������������������������������'
DO logos WITH rotulo1, vtempo
SHOW MENU mmenu
SELECT pecosa
RETURN .T.
*
FUNCTION antr
vant = itepec.canreq
RETURN .T.
*
FUNCTION valor
SELECT itecn
SEEK m.periodo +  ;
     ALLTRIM(m.coddep) +  ;
     ALLTRIM(itepec.codart)
vper = trimestre(m.fecpec)
vDispon = Itecn.Nec_&vPer - Itecn.Ped_&vPer
IF itepec.canreq > vdispon + vant
     DO standby WITH IIF(vdispon+ ;
        vant=0,  ;
        'Ya no Existe Art�culos para este Trimestre',  ;
        'Ud. s�lo puede pedir m�ximo '+ ;
        ALLTRIM(STR(vdispon+vant,  ;
        5))+ ;
        ' para este Trimestre')
ENDIF
IF RLOCK()
     replace Ped_&vper with Ped_&vper;
+ ItePec.Canreq - vant
ENDIF
SELECT itepec
RETURN .T.
*
PROCEDURE anula
SELECT pecosa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado <> '00'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '� Desea ANULAR �ste Pecosa ?' ;
           )
vestado = .T.
SELECT itepec
SEEK m.periodo + m.numpec
SCAN WHILE m.periodo = periodo  ;
     .AND. m.numpec = numpec  ;
     .AND. ALLTRIM(codfte) =  ;
     codfte
     IF estado = '30' .OR. estado =  ;
        '20'
          DO standby WITH  ;
             'La Pecosa ya tiene generada O/C o S/C,no se puede anular'
          vestado = .F.
          EXIT
     ENDIF
ENDSCAN
IF vestado
     IF velimina
          SELECT itepec
          SEEK m.periodo +  ;
               m.numpec
          SCAN WHILE m.periodo =  ;
               periodo .AND.  ;
               m.numpec = numpec  ;
               .AND.  ;
               ALLTRIM(codfte) =  ;
               codfte
               IF RLOCK()
                    REPLACE estado  ;
                            WITH  ;
                            '99'
               ENDIF
          ENDSCAN
          SELECT pecosa
          REPLACE estado WITH  ;
                  '99', fecver  ;
                  WITH DATE()
     ENDIF
ENDIF
SELECT pecosa
DO vista
UNLOCK
RETURN
*
PROCEDURE imprimir
PRIVATE vcon
SELECT pecosa
vcon = RECNO()
SCATTER MEMVAR
fdbf = SYS(3) + '*.DBF'
fidx = SYS(3) + '*.IDX'
USE IN 5
USE IN 6
USE IN 7
COPY TO (fdbf) STRUCTURE
USE IN 5 EXCLUSIVE (fdbf) ALIAS  ;
    def
SELECT def
INDEX ON periodo + numpec TO  ;
      (fidx)
APPEND BLANK
GATHER MEMVAR
SET RELATION TO periodo + numpec INTO;
itepec
SET SKIP TO itepec
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO reporte WITH 2, 'Pecosa',  ;
        ' Pe.co.sa ', 2
ENDIF
SET RELATION TO
USE IN 5
USE IN 5 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 6 IteArt ALIAS iteart  ;
    ORDER IteArt1
USE IN 7 cdrnec ALIAS cuadro  ;
    ORDER Cdrnec1
USE IN 8 itecn ALIAS itecn ORDER  ;
    itecn3
SELECT pecosa
IF vopcion = 2
     SET FILTER TO IIF(vflag = '*',;
.T., coddep = SUBSTR(vcoddep, 1, vnumdep))
ENDIF
GOTO vcon
DO vista
RETURN
*
FUNCTION agreg_item
IF f_appd()
     REPLACE periodo WITH  ;
             m.periodo, numpec  ;
             WITH m.numpec,  ;
             estado WITH '00',  ;
             codcad WITH m.codcad,  ;
             codfte WITH  ;
             m.codfte
     RETURN .T.
ENDIF
RETURN .F.
*
PROCEDURE elimi_item
SELECT itepec
IF RLOCK()
     DELETE NEXT 1
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
RETURN
*
FUNCTION corri_item
REPLACE codcad WITH vcodcad
RETURN .T.
*
PROCEDURE lista
ON KEY LABEL F7
ON KEY LABEL F9
SELECT pecosa
vtemp = RECNO()
SET RELATION TO periodo + numpec + codfte;
INTO itepec
SET SKIP TO itepec
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lispec
ENDIF
SELECT pecosa
SET RELATION TO
IF vopcion = 2
     SET FILTER TO IIF(vflag = '*',;
.T., coddep = SUBSTR(vcoddep, 1, vnumdep))
ENDIF
DO vista
RETURN
*
PROCEDURE lispec
vorde = ORDER()
vrec = RECNO()
DEFINE WINDOW lis FROM 0, 15 TO  ;
       24, 65 FLOAT TITLE  ;
       'Listado Pecosas' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtopec, vtomes, vtofue,  ;
      vtodep, vorden, vtiplis,  ;
      vtiprep
vnp = SPACE(4)
vfte = SPACE(2)
vcodmes = SPACE(2)
vperiod = SPACE(2)
vcoddep = SPACE(6)
vcodfte = SPACE(2)
@ 01, 01 SAY  ;
  'Todas las Pecosas : ' GET  ;
  vtopec SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtopec,2,22) .AND.  ;
  assig()
@ 02, 01 SAY  ;
  '           Pecosa : '
@ 02, 22 GET vnp PICTURE '!!!!'  ;
  WHEN vtopec = 2
@ 02, 27 GET vperiod PICTURE '!!'  ;
  WHEN vtopec = 2
@ 02, 30 SAY '-'
@ 02, 31 GET vfte PICTURE '!!'  ;
  VALID val_para(vfte,'CODFTE', ;
  'C') .AND. valpeco() WHEN  ;
  vtopec = 2
@ 04, 01 SAY  ;
  '  Todos las Meses : ' GET  ;
  vtomes SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtomes,5,22) WHEN vtopec =  ;
  1
@ 05, 01 SAY  ;
  '              Mes : '
@ 05, 22 GET vcodmes PICTURE '!!'  ;
  VALID val_para(vcodmes,'FECMES', ;
  'C') WHEN vtopec = 1 .AND.  ;
  vtomes = 2
@ 07, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtofue,8,22) WHEN vtopec =  ;
  1
@ 08, 01 SAY  ;
  '           Fuente : '
@ 08, 22 GET vcodfte PICTURE '!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  'C') WHEN vtopec = 1 .AND.  ;
  vtofue = 2
@ 10, 01 SAY  ;
  'Todas las Dependc : ' GET  ;
  vtodep SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtodep,11,22) WHEN  ;
  vtopec = 1
@ 11, 01 SAY  ;
  '      Dependencia : '
@ 11, 22 GET vcoddep PICTURE  ;
  '!!!!!!' VALID val_para(vcoddep, ;
  'CODDEP','C') WHEN vtopec = 1  ;
  .AND. vtodep = 2
@ 13, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Numero;Dependencia;Emision'  ;
  WHEN vtopec = 1
@ 16, 01 SAY  ;
  '           Estado : ' GET  ;
  vtiplis FUNCTION  ;
  '^ Todos;Pendientes;Atendidos;Liquidados'  ;
  WHEN vtopec = 1
@ 19, 01 SAY  ;
  '           Estado : ' GET  ;
  vtiprep FUNCTION  ;
  '^ Resumido;Detallado' WHEN  ;
  vtopec = 1
@ 22, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1 .AND. LASTKEY() <>  ;
   27
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento ... Gracias ...'
     vind = SYS(3) + '.IDX'
     INDEX ON IIF(vorden = 1,  ;
           periodo + numpec,  ;
           IIF(vorden = 2, coddep,  ;
           DTOS(fecemi))) TO  ;
           (vind) FOR IIF(vtopec =  ;
           1, .T., periodo +  ;
           numpec + codfte =  ;
           vperiod + vnp + vfte)  ;
           .AND. IIF(vtiplis = 1,  ;
           .T., IIF(vtiplis = 2,  ;
           estado = '00',  ;
           IIF(vtiplis = 3,  ;
           estado = '40', estado =  ;
           '50')))
     SET FILTER TO IIF(vtomes = 1,;
.T., IIF(vtiplis = 4, MONTH(fecdesp),;
MONTH(fecpec)) = VAL(vcodmes));
.AND. IIF(vtomes = 1,;
.T., MONTH(fecpec) = VAL(vcodmes));
.AND. IIF(vtofue = 1,;
.T., codfte = ALLTRIM(vcodfte));
.AND. IIF(vtodep = 1,;
.T., coddep = ALLTRIM(vcoddep))
     SET INDEX TO (vind)
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtiplis = 1,  ;
               ' en General ',  ;
               IIF(vtiplis = 2,  ;
               ' Pendientes ',  ;
               ' Atendidos '))
     IF  .NOT. EOF()
          IF vtopec = 1
               IF vtiprep = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'LisPecX',  ;
                       ' Pe.co.sa ',  ;
                       1, .F.,  ;
                       .T.
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'LisPec',  ;
                       ' Pe.co.sa ',  ;
                       1, .F.,  ;
                       .T.
               ENDIF
          ELSE
               DO reporte WITH 2,  ;
                  'LisPec1',  ;
                  ' Pe.co.sa '
          ENDIF
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     CLOSE INDEX
     ERASE (vind)
ENDIF
SELECT pecosa
SET ORDER TO (vorde)
IF vopcion = 2
     SET FILTER TO IIF(vflag = '*',;
.T., coddep = SUBSTR(vcoddep, 1, vnumdep))
ENDIF
GOTO TOP
GOTO vrec
RETURN
*
FUNCTION assig
vnp = pecosa.numpec
vperiod = pecosa.periodo
vfte = pecosa.codfte
RETURN .T.
*
PROCEDURE valpeco
SELECT pecosa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vnp = PADL(ALLTRIM(vnp), 4, '0')
SEEK '97' + vnp
IF  .NOT. FOUND()
     SET FILTER TO codfte = ALLTRIM(vfte)
     SET RELATION TO periodo + numpec;
INTO itepec
     SET SKIP TO itepec
     vtemp = RECNO()
     HIDE MENU mmenu
     ACTIVATE SCREEN
     vtempo = '�����������Presione �F10� para seleccionar  o  �Esc� para cancelar������������'
     DO logos WITH rotulo1,  ;
        vtempo
     ON KEY LABEL F10 KEYBOARD CHR(23)
     BROWSE FIELDS numpec :H =  ;
            ' N� ', est =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'S/Ct',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            ' -  ')))) :H =  ;
            'ESTD', codcad :H =  ;
            'Cadena Fun.', fecpec  ;
            :H = 'Fecha', coddep  ;
            :H = 'DEP',  ;
            itepec.canreq :H =  ;
            'Cantidad' :P =  ;
            '99,999',  ;
            itepec.descri :H =  ;
            'Detalle ' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_0
     vtempo = '��������������������������������������������������������������������������������'
     DO logos WITH rotulo1,  ;
        vtempo
     SELECT pecosa
     SET RELATION TO
     SET FILTER TO
ENDIF
vnumpec = numpec
vfte = codfte
SHOW MENU mmenu
ON KEY LABEL F10
SELECT pecosa
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
ON KEY LABEL F7
ON KEY LABEL F9
ON KEY LABEL F4
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
ON KEY LABEL F7
ON KEY LABEL F9
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_c1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION valpec
PARAMETER vnumpec, alis
alis = ALIAS()
PRIVATE vfun
vfun = .T.
m.numpec = PADL(ALLTRIM(STR(vnumpecp,  ;
           4)), 4, '0')
IF m.numpec = '0000' .OR.  ;
   EMPTY(m.numpec)
     vfun = .F.
ENDIF
SELECT parma
SEEK 'CORREL' + 'PECOSA'
REPLACE nument WITH nument + 1
SELECT (alis)
RETURN vfun
*
PROCEDURE valult
PARAMETER vrec
vrec = RECNO()
SELECT pecosa
SET ORDER TO pecosa1
GOTO BOTTOM
vnp = VAL(numpec) + 1
m.numpec = PADL(ALLTRIM(STR(vnp,  ;
           4)), 4, '0')
IF m.numpec = '0000' .OR.  ;
   EMPTY(m.numpec)
     vfun = .F.
ENDIF
*
FUNCTION repasa
vfun = .T.
vrec = RECNO()
vali = ALIAS()
SELECT pecosa
SET ORDER TO PECOSA1
SET FILTER TO
SEEK '971179'
as = RECNO()
IF FOUND()
     GOTO as
ENDIF
numr = 1179
DO WHILE .T.
     IF VAL(numpec) = numr
          numr = numr + 1
          SKIP
          LOOP
     ELSE
          EXIT
     ENDIF
ENDDO
m.numpec = PADL(ALLTRIM(STR(numr,  ;
           4)), 4, '0')
IF m.numpec = '0000' .OR.  ;
   EMPTY(m.numpec)
     vfun = .F.
ELSE
     SELECT pecosa
     IF f_appd()
          REPLACE periodo WITH  ;
                  m.periodo,  ;
                  numpec WITH  ;
                  m.numpec,  ;
                  estado WITH  ;
                  '00', user_tp  ;
                  WITH 'E', user  ;
                  WITH SYS(0)
          gh = RECNO()
     ENDIF
     UNLOCK
ENDIF
SELECT parma
SEEK 'CORRELPECOSA'
REPLACE nument WITH numr
SELECT (vali)
RETURN vfun
*
FUNCTION xvalart
PARAMETER _cod
dc = ALIAS()
PRIVATE xx, yy, zz, vfun
vfun = .F.
SELECT iteart
vtemp = RECNO()
SEEK 'B' + ALLTRIM(itepec.codart)
IF FOUND() .AND.  .NOT.  ;
   EMPTY(itepec.codart)
     SELECT itecn
     SEEK m.periodo +  ;
          ALLTRIM(m.coddep) +  ;
          itepec.codart
     IF FOUND()
          SELECT itepec
          IF RLOCK()
               REPLACE coduni  ;
                       WITH  ;
                       iteart.coduni,  ;
                       preuni  ;
                       WITH  ;
                       iteart.preuni,  ;
                       descri  ;
                       WITH  ;
                       iteart.descri,  ;
                       tipcdr  ;
                       WITH 'S'
          ENDIF
          vfun = .T.
     ELSE
          DO standby WITH  ;
             'Este producto no est� registrado en el Cuadro de Necesidades'
          SELECT itepec
          IF RLOCK()
               REPLACE tipcdr  ;
                       WITH 'N'
          ENDIF
          vfun = .T.
     ENDIF
ELSE
     SELECT itepec
     zz = val_para(codart, ;
          'CODGEB','C')
     IF LASTKEY() = 27
          RETURN .T.
     ENDIF
     IF zz
          xx = val_art(_cod,.F.)
          IF xx
               yy = val_artdet(SUBSTR(ALLTRIM(produ.codart),  ;
                    2, 6),.F.)
               IF yy .AND.  ;
                  m.tipdoc = 'S'
                    SELECT itecn
                    SEEK m.periodo +  ;
                         ALLTRIM(m.coddep) +  ;
                         itepec.codart
                    IF  .NOT.  ;
                        FOUND()
                         DO standby  ;
                            WITH  ;
                            'Este producto no est� registrado en el Cuadro de Necesidades'
                         SELECT itepec
                         IF RLOCK()
                              REPLACE  ;
                               coduni  ;
                               WITH  ;
                               iteart.coduni,  ;
                               preuni  ;
                               WITH  ;
                               iteart.preuni,  ;
                               descri  ;
                               WITH  ;
                               iteart.descri,  ;
                               tipcdr  ;
                               WITH  ;
                               'N'
                         ENDIF
                         vfun = .T.
                    ELSE
                         SELECT itepec
                         IF RLOCK()
                              REPLACE  ;
                               coduni  ;
                               WITH  ;
                               iteart.coduni,  ;
                               preuni  ;
                               WITH  ;
                               iteart.preuni,  ;
                               descri  ;
                               WITH  ;
                               iteart.descri,  ;
                               tipcdr  ;
                               WITH  ;
                               'S'
                         ENDIF
                         vfun = .T.
                    ENDIF
               ELSE
                    IF f_lock(1)
                         REPLACE itepec.codart  ;
                                 WITH  ;
                                 SPACE(11)
                    ENDIF
                    vfun = .F.
               ENDIF
          ELSE
               IF f_lock(1)
                    REPLACE itepec.codart  ;
                            WITH  ;
                            SPACE(11)
               ENDIF
               vfun = .F.
          ENDIF
     ELSE
          IF f_lock(1)
               REPLACE itepec.codart  ;
                       WITH  ;
                       SPACE(11)
          ENDIF
          vfun = .F.
     ENDIF
ENDIF
ON KEY
ON KEY LABEL F5 DO Agreg_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itepec
RETURN vfun
*
FUNCTION trimestre
PARAMETER vfecha
DO CASE
     CASE MONTH(vfecha) = 1 .OR.  ;
          MONTH(vfecha) = 2 .OR.  ;
          MONTH(vfecha) = 3
          vtrim = '1'
     CASE MONTH(vfecha) = 4 .OR.  ;
          MONTH(vfecha) = 5 .OR.  ;
          MONTH(vfecha) = 6
          vtrim = '2'
     CASE MONTH(vfecha) = 7 .OR.  ;
          MONTH(vfecha) = 8 .OR.  ;
          MONTH(vfecha) = 9
          vtrim = '3'
     CASE MONTH(vfecha) = 10 .OR.  ;
          MONTH(vfecha) = 11 .OR.  ;
          MONTH(vfecha) = 12
          vtrim = '4'
ENDCASE
RETURN vtrim
*
FUNCTION observa
valias = ALIAS()
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
IF  .NOT. WEXIST('Observa')
     DEFINE WINDOW observa FROM  ;
            05, 18 TO 18, 61  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '� Detalle Pecosa �'  ;
            FOOTER  ;
            ' � �F10� Graba � '  ;
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
        'Proceso cancelado. No graba la Observaci�n '
ENDIF
RETURN .T.
*
FUNCTION visobs
valias = ALIAS()
IF  .NOT. WEXIST('Observa')
     DEFINE WINDOW observa FROM  ;
            03, 18 TO 20, 61  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '� Detalle Pecosa �'  ;
            FOOTER  ;
            ' � �Esc� Sale � '  ;
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
FUNCTION vusua
PARAMETER csys
PRIVATE ali
ali = ALIAS()
vkey = ALLTRIM(csys)
SELECT usu
SEEK vkey
vfun = nombre
SELECT (ali)
RETURN vfun
*
FUNCTION buscart
as = ALIAS()
SELECT produ
SET ORDER TO 1
SEEK 'B' + LEFT(itepec.codart, 6)
IF  .NOT. FOUND()
     vfun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
ELSE
     vfun = UPPER(produ.descri)
ENDIF
SELECT (as)
RETURN vfun
*
FUNCTION val_dep
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
SELECT parma
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO tipo = filtro;
.AND. codigo = SUBSTR(vcoddep, 1, vnumdep)
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
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            SUBSTR(descri, 1,  ;
            40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_oldwnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = parma.codigo
mcuenta = parma.descriau2
mdescr = SUBSTR(parma.descri, 1,  ;
         mlong)
mdescriaux = SUBSTR(parma.descriaux,  ;
             1, mlong)
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
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN ' '
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
          REPLACE &mvariable WITH mvalor
          RETURN .T.
ENDCASE
*
FUNCTION val_artc
PARAMETER xcod, _tipo, _x, _y
ON KEY LABEL F5
ON KEY LABEL F8
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) .AND.  ;
       _tipo
_campo = VARREAD()
ord = ORDER()
malias = ALIAS()
SELECT iteart
GOTO TOP
_oldwnd = WOUTPUT()
v_fun = .F.
v_ent = .F.
IF  .NOT. medita
     SET ORDER TO ITEART3
     SEEK xcod
     v_fun = IIF(FOUND(), descri,  ;
             '')
     v_ent = FOUND()
ENDIF
IF EMPTY(xcod) .OR.  .NOT. v_ent
     SET ORDER TO ITEART2
     GOTO TOP
     ACTIVATE SCREEN
     ON KEY LABEL F10 KEYBOARD CHR(23)
     ON KEY LABEL F2 DO FunBusDet
     ON KEY LABEL F5
     ON KEY LABEL F8
     DEFINE WINDOW _busart FROM 2,  ;
            01 TO 22, 78
     BROWSE FIELDS codart :H =  ;
            'C�digo', descri :H =  ;
            'Nombre' : 60, coduni  ;
            :H = 'Unidad' : 7  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            WINDOW _busart TITLE  ;
            '���� [F10] Selecciona   [F2] Buscar ����'  ;
            NOLGRID
     ON KEY LABEL F10
     ON KEY LABEL F2
     RELEASE WINDOW _busart
     SET ORDER TO ITEART1
     IF LASTKEY() = 27
          v_fun = .F.
          v_ent = .F.
     ELSE
          xcod = codart
          xdes = descri
          xuni = coduni
          IF mmsg
               @ _x, _y SAY  ;
                 descri
          ENDIF
          SELECT (malias)
          IF  .NOT. _tipo
               REPLACE &_campo WITH;
 xcod 
          ENDIF
          v_fun = .T.
          v_ent = .T.
     ENDIF
ENDIF
vtipcdr = 'N'
IF v_ent .AND. m.tippec = 'S'  ;
   .AND. itepec.canreq = 0
     SELECT itecn
     SEEK m.periodo +  ;
          ALLTRIM(m.coddep) +  ;
          ALLTRIM(xcod)
     IF  .NOT. FOUND()
          vtipcdr = 'N'
     ELSE
          DO muestra WITH  ;
             'ITEART.DESCRI'
          vtipcdr = 'S'
     ENDIF
ENDIF
SELECT itepec
IF RLOCK()
     REPLACE coduni WITH  ;
             iteart.coduni,  ;
             preuni WITH  ;
             iteart.preuni,  ;
             descri WITH  ;
             iteart.descri,  ;
             tipcdr WITH vtipcdr
ENDIF
SELECT (malias)
SET ORDER TO (ord)
ON KEY LABEL F5 DO Agreg_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
IF  .NOT. v_ent
     RETURN v_fun
ELSE
     RETURN v_ent
ENDIF
*
PROCEDURE muscdr
PRIVATE ed
ed = ALIAS()
IF m.tippec = 'S'
     SELECT itecn
     SEEK m.periodo +  ;
          ALLTRIM(m.coddep) +  ;
          ALLTRIM(itepec.codart)
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'No tiene Cuadro de Necesidades'
     ELSE
          DO muestra WITH  ;
             'ITEPEC.DESCRI'
     ENDIF
ENDIF
SELECT (ed)
RETURN
*
PROCEDURE muestra
PARAMETER uy
PRIVATE uy, vtri
vtri = trimestre(m.fecpec)
uy = iteart.descri
ua = 'SCHEME 10'
ub = 'W+/BG, W+/BG, GR+/W*, W+/B, R+/B, W+/GR, GR+/RB, N+/N, GR+/B, R+/B'
DEFINE WINDOW Wind_T FROM 12,00 TO 24,79;
DOUBLE  TITLE "Art�culo: "+ALLTRIM(&UY);
COLOR SCHEME 10
IF vtri = '1'
     DEFINE WINDOW Wind_1T FROM 14,21;
TO 16,59 DOUBLE  TITLE ' Movimiento :1� Trimestre ';
COLOR &UB
ELSE
     DEFINE WINDOW Wind_1T FROM 14,21;
TO 16,59 DOUBLE  TITLE ' Movimiento :1� Trimestre ';
COLOR &UA
ENDIF
IF vtri = '2'
     DEFINE WINDOW Wind_2T FROM 17,01;
TO 19,39 DOUBLE  TITLE ' Movimiento :2� Trimestre ';
COLOR &UB
ELSE
     DEFINE WINDOW Wind_2T FROM 17,01;
TO 19,39 DOUBLE  TITLE ' Movimiento :2� Trimestre ';
COLOR &UA
ENDIF
IF vtri = '3'
     DEFINE WINDOW Wind_3T FROM 17,41;
TO 19,78 DOUBLE  TITLE ' Movimiento :3� Trimestre ';
COLOR &UB
ELSE
     DEFINE WINDOW Wind_3T FROM 17,41;
TO 19,78 DOUBLE  TITLE ' Movimiento :3� Trimestre ';
COLOR &UA
ENDIF
IF vtri = '4'
     DEFINE WINDOW Wind_4T FROM 20,21;
TO 22,59 DOUBLE  TITLE ' Movimiento :4� Trimestre ';
COLOR &UB
ELSE
     DEFINE WINDOW Wind_4T FROM 20,21;
TO 22,59 DOUBLE  TITLE ' Movimiento :4� Trimestre ';
COLOR &UA
ENDIF
DO calcdr WITH uy
ACTIVATE WINDOW wind_t
ACTIVATE WINDOW wind_1t
@ 0, 0 SAY 'Pedido :'
@ 0, 8 SAY nec_1 PICTURE '9,999'
@ 0, 20 SAY 'Atendido :'
@ 0, 30 SAY ped_1 PICTURE '9,999'
ACTIVATE WINDOW wind_2t
@ 0, 0 SAY 'Pedido :'
@ 0, 8 SAY nec_2 PICTURE '9,999'
@ 0, 20 SAY 'Atendido :'
@ 0, 30 SAY ped_2 PICTURE '9,999'
ACTIVATE WINDOW wind_3t
@ 0, 0 SAY 'Pedido :'
@ 0, 8 SAY nec_3 PICTURE '9,999'
@ 0, 20 SAY 'Atendido :'
@ 0, 30 SAY ped_3 PICTURE '9,999'
ACTIVATE WINDOW wind_4t
@ 0, 0 SAY 'Pedido :'
@ 0, 8 SAY nec_4 PICTURE '9,999'
@ 0, 20 SAY 'Atendido :'
@ 0, 30 SAY ped_4 PICTURE '9,999'
ss = INKEY(0)
DEACTIVATE WINDOW wind_1t,  ;
           wind_2t, wind_3t,  ;
           wind_4t, wind_t
RETURN
*
PROCEDURE calcdr
PARAMETER uya
PRIVATE alias, vkey
vkey = m.periodo +  ;
       ALLTRIM(m.coddep) +  ;
       ALLTRIM(uya)
alias = ALIAS()
SELECT itepec
orden = ORDER()
SET ORDER TO ITEPEC13
SET FILTER TO estado <> '99'
SEEK vkey
IF FOUND()
     SCAN WHILE vkey = periodo +  ;
          coddep + codart
     ENDSCAN
ENDIF
*

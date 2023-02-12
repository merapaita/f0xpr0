PARAMETER sistema
CLOSE DATABASES
USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 2 Solser ALIAS solser  ;
    ORDER Solser1
USE IN 3 calen ALIAS calen ORDER  ;
    calen1
IF sistema = '1'
     USE IN 4 maepre ALIAS maepre  ;
         ORDER maepre1
ELSE
     USE IN 4 maepre ALIAS maepre  ;
         ORDER maepre3
ENDIF
USE IN 5 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 7 OrdSer ALIAS ordse ORDER  ;
    OrdSer1
USE IN 8 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 9 Cheque ALIAS cheque  ;
    ORDER Cheque1
USE IN 10 Compag ALIAS compag  ;
    ORDER Compag1
USE IN 11 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 12 Usuario ALIAS usu ORDER  ;
    Usuario1
USE IN 13 cdrnec ALIAS cuadro  ;
    ORDER Cdrnec1
vmens01 = ' Solicitud de Servicios : REGISTRO '
vmens02 = 'Registro de Solicitud de Servicio'
vmens04 = 'Dicho Solicitud de Servicio no fue encontrado'
vmens05 = 'No existe Solicitud de Servicio anterior'
vmens06 = 'No existe Solicitud de Servicio siguiente'
vmens07 = '¨ Desea ELIMINAR ‚ste Solicitud de Servicio ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Solicitud de Servicio ha sido anulada'
vmens10 = 'El Solicitud de Servicio ya fue atendido'
vmens11 = 'El Solicitud de Servicio ha sido devuelto'
vmens12 = 'El Solicitud de Servicio ya tiene O/S'
PRIVATE vruc, vdirec
vnumdep = 0
PUBLIC fond
SET MEMOWIDTH TO 56
PUBLIC con, conx, cony, conz, gh,  ;
       v_reg
SELECT solser
GOTO BOTTOM
SCATTER BLANK MEMVAR
ON KEY LABEL f2 do VisObs
ON KEY LABEL f4 do imprimIR
ON KEY LABEL f5 do asigprv
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular           Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 11, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 12, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle: Solicitud de Servicio'  ;
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
       'a\<Nula  ' AT 24, 54
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO corri
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD anula OF mmenu DO anula
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_0
CLEAR
@ 1, 2 SAY '            Fecha :'
@ 1, 40 SAY ' N£mero Solicitud :'
@ 2, 2 SAY '               De :'
@ 3, 2 SAY '      Dependencia :'
@ 4, 2 SAY '      Cadena Fun. :'
@ 5, 2 SAY ' Fte. Funcionami. :'
@ 6, 2 SAY '          Funci¢n :'
@ 7, 2 SAY '         Programa :'
@ 8, 2 SAY '      Subprograma :'
@ 9, 2 SAY '   Activ./Proyec. :'
@ 10, 2 SAY '          Destino :'
@ 11, 2 SAY '    Tipo Servicio :'
@ 12, 2 SAY '        Ubicaci¢n :'
@ 13, 2 SAY '          Usuario :'
@ 14, 2 SAY '         Defectos :'
@ 15, 2 SAY '            Causa :'
@ 17, 2 SAY '      Descripci¢n :'
RETURN
*
PROCEDURE vista
SELECT solser
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
ON KEY LABEL F7 DO Estado WITH 'SS','Solser.Periodo+Solser.Numos+Solser.Codfte'
SCATTER MEMVAR
DO pantalla
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
IF user_tp $ 'E'
     @ 1, 22 SAY m.fecss
     @ 1, 60 SAY m.periodo
     @ 1, 63 SAY m.numss
     @ 2, 22 CLEAR TO 20, 78
     DO standby WITH  ;
        'Solicitud Elaborando por '+ ;
        user
     IF  .NOT. BOF()
          SKIP -1
          DO vista
     ENDIF
ELSE
     @ 0, 00 SAY IIF(m.user_tp =  ;
       'I', IIF(EMPTY(m.user),  ;
       SPACE(50),  ;
       '     Elaborado por: ') +  ;
       user, IIF(EMPTY(m.user),  ;
       SPACE(50),  ;
       '     Corregido por: ') +  ;
       user)
     @ 0, 60 SAY IIF(m.estado =  ;
       '00', 'Pendiente   ',  ;
       IIF(m.estado = '20',  ;
       'C/Sol.Cotiz.',  ;
       IIF(m.estado = '99',  ;
       'Anulada     ',  ;
       IIF(m.estado = '50',  ;
       'Atendido    ', 'Con O/S:' +  ;
       numos))))
     @ 1, 22 SAY m.fecss
     @ 1, 60 SAY m.periodo
     @ 1, 63 SAY m.numss
     @ 2, 22 SAY m.atte
     @ 3, 22 SAY  ;
       val_para(m.coddep,'CODDEP', ;
       'D',22,50)
     @ 4, 22 SAY  ;
       val_codcad(m.codcad, ;
       m.periodo,'D',22,40)
     @ 5, 22 SAY  ;
       val_para(m.codfte,'CODFTE', ;
       'D',22,40)
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
     @ 11, 22 SAY  ;
       val_para(m.tipser,'TIPSER', ;
       'V',22,40)
     @ 12, 22 SAY m.ubicac
     @ 13, 22 SAY m.usuario
     @ 14, 22 SAY m.defect
     @ 15, 22 SAY m.causas
     @ 17, 22 CLEAR TO 21, 78
     @ 17, 22 SAY MLINE(detalle,  ;
       1)
     @ 18, 22 SAY MLINE(detalle,  ;
       2)
     @ 19, 22 SAY MLINE(detalle,  ;
       3)
     @ 20, 22 SAY MLINE(detalle,  ;
       4)
     @ 21, 00 SAY PADC( ;
       '° ®F2¯ Detalle S/S ° ®F4¯ Imprime ° ®F5¯ Asigna Prv ° ®F7¯ Seguimiento °',  ;
       79, ' ') COLOR W+/B 
ENDIF
RETURN
*
PROCEDURE revis
ON KEY LABEL F7
SELECT solser
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS numss :H = 'N§S/S',  ;
       est = IIF(estado = '00',  ;
       'Pendte', IIF(estado =  ;
       '20', 'S/Cotz', IIF(estado =  ;
       '99', 'Anulad', IIF(estado =  ;
       '50', 'Atend.', '(' +  ;
       numos + ')')))) :H =  ;
       ' O/S ', codfte :H =  ;
       'Fte ', fecss :H =  ;
       'Fecha ', coddep :H =  ;
       'DEP', xx = MLINE(detalle,  ;
       1) :H = 'Descripci¢n'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELECT solser
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
ON KEY LABEL F7
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnumss = '    '
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero Solicitud : '  ;
  GET vperiodo PICTURE '!!'
@ 1, 30 SAY '-' GET vnumss  ;
  PICTURE '!!!!' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnumss) .OR. LASTKEY() =  ;
   27
     ON KEY LABEL F7 DO Estado WITH 'SS','Solser.Periodo+Solser.Numos+Solser.Codfte'
     RETURN
ELSE
     SEEK vperiodo + vnumss
     IF  .NOT. FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
ON KEY LABEL F7 DO Estado WITH 'SS','Solser.Periodo+Solser.Numos+Solser.Codfte'
RETURN
*
FUNCTION vbusca
vnumss = PADL(ALLTRIM(vnumss), 4,  ;
         '0')
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
PUBLIC vmes, vpart
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
DO CASE
     CASE estado = '99'
          DO standby WITH vmens09
          RETURN
     CASE estado = '20'
          DO standby WITH  ;
             'La O/S ya tiene Solicitud de Cotizaci¢n'
          RETURN
     CASE estado = '50'
          DO standby WITH  ;
             'Ya tiene O/S'
          RETURN
     CASE estado = '70'
          DO standby WITH  ;
             'La O/S ha sido devuelta'
          RETURN
ENDCASE
SELECT solser
SCATTER MEMVAR
ACTIVATE WINDOW wind_0
DO pantalla
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
@ 1, 22 GET m.fecss PICTURE '@D'  ;
  DISABLE
@ 1, 60 GET m.periodo PICTURE  ;
  '!!' DISABLE
@ 1, 62 SAY '-'
@ 1, 63 GET m.numss PICTURE  ;
  '!!!!' DISABLE
@ 2, 22 GET m.atte
@ 3, 22 GET m.coddep PICTURE  ;
  '!!!!!!' VALID  ;
  val_para(m.coddep,'CODDEP',' ', ;
  22,50)
@ 4, 22 GET m.codcad PICTURE  ;
  '!!!!' VALID  ;
  val_codcad(m.codcad,m.periodo, ;
  ' ',22,30)
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
  '@S56'
@ 11, 22 GET m.tipser PICTURE  ;
  '!!' VALID val_para(m.tipser, ;
  'TIPSER',' ',22,40)
@ 12, 22 GET m.ubicac PICTURE  ;
  '@S56'
@ 13, 22 GET m.usuario PICTURE  ;
  '@S56'
@ 14, 22 GET m.defect PICTURE  ;
  '@S56'
@ 15, 22 GET m.causas PICTURE  ;
  '@S56'
@ 17, 22 SAY MLINE(detalle, 1)
@ 18, 22 SAY MLINE(detalle, 2)
@ 19, 22 SAY MLINE(detalle, 3)
@ 20, 22 SAY MLINE(detalle, 4)
READ VALID val_read()
IF LASTKEY() <> 27
     DO WHILE .T.
          ok = detalle()
          IF LASTKEY() <> 27
               IF yesno( ;
                  '¨ Conforme la correcci¢n ?' ;
                  )
                    EXIT
               ENDIF
          ELSE
               IF yesno( ;
                  '¨ Cancela la correcci¢n ?' ;
                  )
                    ok = .F.
                    EXIT
               ENDIF
          ENDIF
     ENDDO
     codcad = m.codcad
     SELECT solser
     SEEK m.periodo + m.numss
     IF .T.
          REPLACE codcad WITH  ;
                  m.codcad,  ;
                  codfte WITH  ;
                  m.codfte,  ;
                  coddep WITH  ;
                  m.codfte
     ENDIF
     IF ok .AND. LASTKEY() <> 27
          SELECT solser
          m.user = SYS(0)
          m.user_fc = DATE()
          m.user_tp = 'C'
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
SELECT solser
DO vista
UNLOCK
RETURN
*
PROCEDURE ingre
PUBLIC ast, rec, vmes, vpart,  ;
       vtipo
ON KEY LABEL F7
DO pantalla
SELECT solser
rec = RECNO()
SCATTER BLANK MEMVAR
m.periodo = SUBSTR(DTOC(m.fecsis), 9, 2)
m.numss = repasa()
m.fecss = m.fecsis
@ 1, 22 GET m.fecss PICTURE '@D' DISABLE
@ 1, 60 GET m.periodo PICTURE '!!' DISABLE
@ 1, 62 SAY '-'
@ 1, 63 GET m.numss PICTURE '!!!!' DISABLE VALID val_ss()
@ 2, 22 GET m.atte
@ 3, 22 GET m.coddep PICTURE  ;
  '!!!!!!' VALID val_dep(m.coddep, ;
  'CODDEP',' ',22,40) .AND.  ;
  valatte()
@ 4, 22 GET m.codcad PICTURE  ;
  '!!!!' VALID  ;
  val_codcad(m.codcad,m.periodo, ;
  ' ',22,40)
@ 5, 22 GET m.codfte PICTURE '!!'  ;
  VALID val_para(m.codfte, ;
  'CODFTE',' ',22,40)
READ VALID val_read()
IF LASTKEY() = 27
     SELECT solser
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
  '@S56'
@ 11, 22 GET m.tipser PICTURE  ;
  '!!' VALID val_para(m.tipser, ;
  'TIPSER',' ',22,40)
@ 12, 22 GET m.ubicac PICTURE  ;
  '@S56'
@ 13, 22 GET m.usuario PICTURE  ;
  '@S56'
@ 14, 22 GET m.defect PICTURE  ;
  '@S56'
@ 15, 22 GET m.causas PICTURE  ;
  '@S56'
READ VALID val_read()
IF LASTKEY() <> 27
     SELECT solser
     SEEK m.periodo + m.numss
     IF  .NOT. FOUND()
          IF f_appd()
               m.user = SYS(0)
               m.user_fc = DATE()
               m.user_tp = 'C'
               m.estado = '00'
               GATHER MEMVAR
               SELECT parma
               SEEK 'CORRELSOLSER'
               IF FOUND()
                    REPLACE nument  ;
                            WITH  ;
                            nument +  ;
                            1
               ENDIF
          ENDIF
     ENDIF
     SELECT solser
     IF  .NOT. EMPTY(m.numss)
          ok = detalle()
     ENDIF
ELSE
     GOTO rec
ENDIF
SELECT solser
DO vista
RETURN
*
FUNCTION val_ss
PRIVATE as
as = RECNO()
SEEK m.periodo + m.numss
IF FOUND()
     DO standby WITH  ;
        'La Solicitud ya esta registrada'
     RETURN .F.
ENDIF
RETURN .T.
*
PROCEDURE anula
SELECT solser
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado = '99'
     DO standby WITH vmens09
     RETURN
ENDIF
IF estado <> '00'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ANULAR ‚ste Solicitud?' ;
           )
IF velimina .AND. (RLOCK() .OR.  ;
   f_lock(1))
     REPLACE estado WITH '99',  ;
             fecver WITH DATE()
     DO vista
ENDIF
UNLOCK
RETURN
*
PROCEDURE lista
SELECT solser
vtemp = RECNO()
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     ON KEY LABEL F7
     DO lisser
ENDIF
SELECT solser
GOTO vtemp
DO vista
RETURN
*
PROCEDURE lisser
vorde = ORDER()
DEFINE WINDOW lis FROM 2, 15 TO  ;
       21, 65 FLOAT TITLE  ;
       'Listado Solicitud de Servicios'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro, vlistado
vcli = SPACE(4)
vano = STR(YEAR(DATE()) - 1900,  ;
       2)
@ 01, 01 SAY  ;
  '     Tipo Listado : ' GET  ;
  vlistado FUNCTION  ;
  '^ Resumido;Detallado'
@ 05, 01 SAY  ;
  '        Total S/S : ' GET  ;
  vtocli SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,3,22) WHEN  ;
  vlistado = 2
@ 07, 01 SAY  ;
  '              S/S : '
@ 07, 22 GET vano PICTURE '!!'  ;
  WHEN vtocli = 2 .AND. vlistado =  ;
  2
@ 07, 25 SAY '-'
@ 07, 26 GET vcli PICTURE '!!!!'  ;
  VALID bussol() WHEN vtocli = 2  ;
  .AND. vlistado = 2
@ 09, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Numero;Dependencia;Emision'  ;
  WHEN vtocli = 1
@ 12, 01 SAY  ;
  '           Estado : ' GET  ;
  vtippro FUNCTION  ;
  '^ Todos;Pendientes;Atendidos'  ;
  WHEN vtocli = 1
@ 16, 10 GET okcancel DEFAULT 1  ;
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
           periodo + numss,  ;
           IIF(vorden = 2, coddep,  ;
           DTOS(fecss))) TO  ;
           (vind) FOR IIF(vtocli =  ;
           1, .T., periodo +  ;
           numss = vano + vcli)  ;
           .AND. IIF(vtippro = 1,  ;
           .T., IIF(vtippro = 2,  ;
           estado = '00', estado =  ;
           '50'))
     SET INDEX TO (vind)
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtippro = 1,  ;
               ' en General ',  ;
               IIF(vtippro = 2,  ;
               ' Pendientes ',  ;
               ' Atendidos '))
     IF  .NOT. EOF()
          IF vlistado = 2
               DO reporte WITH 2,  ;
                  'LissERV',  ;
                  ' Solicitud de Servicio '
          ELSE
               DO reporte WITH 2,  ;
                  'LisSol',  ;
                  ' Solicitud de Servicios ',  ;
                  1, .F., .T.
          ENDIF
     ELSE
          DO standby WITH vmens08
     ENDIF
     CLOSE INDEX
     ERASE (vind)
ENDIF
SELECT solser
SET ORDER TO (vorde)
GOTO TOP
RETURN
*
PROCEDURE imprimir
PRIVATE vcon, xx
SELECT solser
DO buscaprv
vcon = RECNO()
SCATTER MEMVAR
vnumss = m.numss
SET FILTER TO numss = vnumss
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO reporte WITH 2, 'LissERV',  ;
        ' Solicitud de Servicio '
ENDIF
SET FILTER TO
SELECT solser
GOTO vcon
DO vista
RETURN
*
PROCEDURE imprim_a
PRIVATE vcon
SELECT solser
vcon = RECNO()
SCATTER MEMVAR
IF m.estado <> '30'
     DO standby WITH  ;
        'A£n esta sin Orden de Servicio'
     DO vista
     RETURN
ENDIF
vnumss = m.numss
SET FILTER TO numss = vnumss
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO reporte WITH 2,  ;
        'Lisss1_A',  ;
        ' Solicitud de Servicio '
ENDIF
SET FILTER TO
SELECT solser
GOTO vcon
DO vista
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
ON KEY LABEL F2
ON KEY LABEL F4
ON KEY LABEL F5
ON KEY LABEL F7
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
ON KEY LABEL F7
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_c1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION valss
PARAMETER vnumss
PRIVATE vfun
vfun = .T.
m.numss = PADL(ALLTRIM(STR(vnumss,  ;
          4)), 4, '0')
IF m.numss = '0000' .OR.  ;
   EMPTY(m.numss)
     vfun = .F.
ENDIF
RETURN vfun
*
FUNCTION valart
PARAMETER _cod
PRIVATE xx, vfun
vfun = .F.
xx = val_art(codart,.F.)
IF xx
     SELECT itepec
     REPLACE coduni WITH  ;
             produ.coduni, preuni  ;
             WITH produ.preuni
     vfun = .T.
ENDIF
RETURN vfun
*
PROCEDURE bussol
SELECT solser
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vcli = PADL(ALLTRIM(vcli), 4,  ;
       '0')
SEEK vano + vcli
IF  .NOT. FOUND()
     HIDE MENU mmenu
     ACTIVATE SCREEN
     vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     ON KEY LABEL F10 KEYBOARD CHR(23)
     BROWSE FIELDS numss :H =  ;
            ' N§ ', est =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'S/Ct',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            ' -  ')))) :H =  ;
            'ESTD', fecss :H =  ;
            'Fecha', coddep :H =  ;
            'DEP', codcad :H =  ;
            'Codigo Cadena',  ;
            desss :H =  ;
            'Descripci¢n' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_0
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     SHOW MENU mmenu
ENDIF
vano = periodo
vcli = numss
ON KEY LABEL F10
RETURN
*
PROCEDURE asigprv
vorde = ORDER()
IF m.estado = '00' .OR. m.estado <>  ;
   '00'
     IF EMPTY(m.estado)
          m.codprv = '0000'
     ENDIF
     DEFINE WINDOW lis1 FROM 10,  ;
            15 TO 14, 65 FLOAT  ;
            TITLE  ;
            'Asigna Proveedores de Servicios'  ;
            DOUBLE COLOR SCHEME  ;
            5
     ACTIVATE WINDOW lis1
     @ 01, 01 SAY  ;
       ' C¢digo Proveedor : ' GET  ;
       m.codprv VALID  ;
       IIF(m.codprv = '0000', .T.,  ;
       val_prv(m.codprv,.T.,1,27)  ;
       .AND. siprv() .AND.  ;
       promae.estado = 'VG')
     @ 02, 01 SAY  ;
       '   Total Servicio : ' GET  ;
       m.valtot PICTURE  ;
       '9,999,999.99'
     READ VALID val_read()
     IF LASTKEY() <> 27
          REPLACE solser.codprv  ;
                  WITH m.codprv,  ;
                  solser.valtot  ;
                  WITH m.valtot
     ENDIF
     DEACTIVATE WINDOW lis1
ELSE
     DO standby WITH  ;
        'No esta pendiente...'
ENDIF
RETURN
*
FUNCTION buscprv
PRIVATE xc
xc = ALIAS()
vcodprv = val_prv(m.codprv,.T.)
fond = vcodprv
SELECT (xc)
RETURN ' '
*
FUNCTION detalle
valias = ALIAS()
SET MEMOWIDTH TO 56
ON KEY LABEL F10 KEYBOARD CHR(23)
IF  .NOT. WEXIST('Detalle')
     DEFINE WINDOW detalle FROM  ;
            03, 12 TO 20, 67  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Observaciones ±'  ;
            FOOTER  ;
            ' ° ®F10¯ Graba ° '  ;
            DOUBLE COLOR SCHEME  ;
            1
ENDIF
IF WVISIBLE('Detalle')
     ACTIVATE WINDOW SAME detalle
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              detalle
ENDIF
MODIFY MEMO detalle WINDOW  ;
       detalle
IF  .NOT. WVISIBLE('Detalle')
     ACTIVATE WINDOW detalle
ENDIF
RELEASE WINDOW detalle
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado. No graba el Detalle '
ENDIF
SELECT (valias)
RETURN .T.
*
FUNCTION visobs
ON KEY LABEL F2
valias = ALIAS()
IF  .NOT. WEXIST('Detalle')
     DEFINE WINDOW detalle FROM  ;
            03, 12 TO 20, 67  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE '± Detalle ±'  ;
            FOOTER  ;
            ' ° ®Esc¯ Sale ° '  ;
            DOUBLE COLOR SCHEME  ;
            1
ENDIF
IF WVISIBLE('Detalle')
     ACTIVATE WINDOW SAME detalle
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              detalle
ENDIF
MODIFY MEMO detalle NOEDIT WINDOW  ;
       detalle
IF  .NOT. WVISIBLE('Detalle')
     ACTIVATE WINDOW detalle
ENDIF
RELEASE WINDOW detalle
ON KEY LABEL F2 do VisObs
RETURN .T.
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
FUNCTION repasa
SELECT parma
SEEK 'CORREL' + 'SOLSER'
mret = PADL(ALLTRIM(STR(parma.nument +  ;
       1)), 4, '0')
RETURN mret
*
PROCEDURE valatte
SELECT cuadro
SEEK m.periodo + m.coddep
IF FOUND()
     m.atte = cuadro.atte
     @ 2, 22 SAY m.atte
ELSE
     m.atte = 'No Existe Solicitante..,Revise'
     @ 2, 22 SAY m.atte
ENDIF
SELECT solser
RETURN
*
PROCEDURE buscaprv
PRIVATE ali, vkey
ali = ALIAS()
vkey = codprv
SELECT promae
SEEK vkey
SELECT (ali)
RETURN
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
FUNCTION siprv
PARAMETER vfun
vfun = .T.
IF  .NOT. v_reg = 'VG'
     DO standby WITH  ;
        'El Proveedor no esta Regularizado...Observaci¢n'
ENDIF
RETURN vfun
*

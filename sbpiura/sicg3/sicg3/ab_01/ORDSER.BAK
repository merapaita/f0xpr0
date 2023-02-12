PARAMETER sistema, vopcion
USE IN 1 OrdSer ALIAS ordse ORDER  ;
    OrdSer1
USE IN 2 Solser ALIAS solser  ;
    ORDER Solser1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 4 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 5 Calen ALIAS calen ORDER  ;
    calen4
USE IN 0 AsiAut ORDER AsiAut1
IF sistema = '1'
     USE IN 10 maepre ALIAS  ;
         maepre ORDER maepre1
ELSE
     USE IN 10 maepre ALIAS  ;
         maepre ORDER maepre3
ENDIF
USE IN 11 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 12 Cheque ALIAS cheque  ;
    ORDER Cheque1
USE IN 13 Compag ALIAS compag  ;
    ORDER Compag1
USE IN 14 ITEHC ALIAS itehc ORDER  ;
    ITEHC1
USE IN 16 ITEOS1 ALIAS iteos1  ;
    ORDER ITEOS11
USE IN 15 AstOrd ALIAS astord  ;
    ORDER AstOrd1
USE IN 20 USUARIO ALIAS usu ORDER  ;
    USUARIO1
USE IN 0 IteUsuOp ALIAS subop  ;
    ORDER IteUsuOp1
USE IN 0 CatAsi ALIAS catasi  ;
    ORDER CatAsi4
PUBLIC vmes, con, cony, conx, vms,  ;
       v_reg, vcadena, gh,  ;
       vtipord
vtipord = ' '
vmens01 = ' Orden de Servicio : REVISION '
vmens02 = ' Registro de Ordenes de Servicio '
vmens04 = 'Dicho Orden de Servicio no fue encontrado'
vmens05 = 'No existe Orden de Servicio anterior'
vmens06 = 'No existe Orden de Servicio siguiente'
vmens07 = '¨ Desea ANULAR ‚ste Orden de Servicio ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Orden de Servicio ha sido anulado'
vmens10 = 'Este Orden de Servicio ya fue atendido'
vmens11 = 'Este Orden de Servicio ha sido devuelto'
ON KEY LABEL F2 do VisObs
ON KEY LABEL F4 do imprimir
ON KEY LABEL F6 do liquidar
ON KEY LABEL F12 DO CORRI1
SELECT ordse
GOTO BOTTOM
SCATTER BLANK MEMVAR
HIDE POPUP ALL
DO inicia
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
DEFINE WINDOW windo_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 14, 01  ;
       TO 16, 79 TITLE  ;
       ' Destino '
DEFINE WINDOW wind_2 FROM 12, 04  ;
       TO 18, 75 TITLE  ;
       '®F5¯ Agrega  ° ®F8¯ Eliminar  ° ®F10¯ Terminar '  ;
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
ACTIVATE WINDOW windo_0
CLEAR
@ 1, 2 SAY '       Numero O/S :'
@ 1, 40 SAY '       N£mero S/S :'
@ 2, 2 SAY '        Fecha O/S :'
@ 3, 2 SAY '              H/C :'
@ 4, 2 SAY '        Proveedor :'
@ 5, 2 SAY '        Cad.Func. :'
@ 6, 2 SAY ' Fte. Financiami. :'
@ 7, 2 SAY '          Funci¢n :'
@ 8, 2 SAY '         Programa :'
@ 9, 2 SAY '      Subprograma :'
@ 10, 2 SAY '   Activ./Proyec. :'
@ 11, 2 SAY '       Calendario :'
@ 12, 2 SAY '      Dependencia :'
@ 13, 2 SAY '          Destino :'
@ 14, 2 SAY '      Descripci¢n :'
@ 19, 2 SAY '          Importe :'
@ 19, 40 SAY  ;
  '         Contrato :'
@ 20, 2 SAY  ;
  '             IGV? :   '
RETURN
*
PROCEDURE vista
ON KEY LABEL F6 DO LIQUIDAR
ON KEY LABEL F7 DO ESTADOS
SELECT ordse
SET MEMOWIDTH TO 45
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW windo_0
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
@ 0, 02 SAY IIF(EMPTY(m.user),  ;
  SPACE(50),  ;
  '     Elaborado por: ') + user  ;
  COLOR SCHEME 02
@ 0, 60 SAY vestoc(m.estado)  ;
  COLOR SCHEME 02
@ 1, 40 SAY SPACE(40)
@ 1, 22 SAY m.periodo
@ 1, 24 SAY '-'
@ 1, 25 SAY m.numos
@ 1, 40 SAY IIF(EMPTY(m.memoran),  ;
  '       N£mero S/S :',  ;
  '       Memorandum :')
@ 1, 60 SAY IIF(EMPTY(m.memoran),  ;
  m.periodo + '.' + m.numss,  ;
  SUBSTR(m.memoran, 1, 18))
@ 2, 22 SAY m.fecos
@ 2, 60 SAY IIF(m.estado = '50',  ;
  fecliq, '        ')
@ 3, 22 SAY m.perhc + ' ' +  ;
  m.numhc
@ 4, 22 SAY SPACE(56)
@ 4, 22 SAY m.codprv
@ 4, 27 SAY val_prv(m.codprv,.T., ;
  4,27)
@ 5, 22 SAY val_codcad(m.codcad, ;
  m.periodo,'D',22,30)
@ 6, 22 SAY val_para(m.codfte, ;
  'CODFTE','D',22,30)
@ 7, 22 SAY  ;
  val_para(maepre.codfun,'CODFUN', ;
  'V',22,40)
@ 8, 22 SAY  ;
  val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'V',22, ;
  40)
@ 9, 22 SAY  ;
  val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'V',22, ;
  40)
@ 10, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 11, 22 SAY val_para(m.nummes, ;
  'FECMES','D',22,40)
@ 12, 22 SAY val_para(m.coddep, ;
  'CODDEP','A',22,56)
@ 13, 22 SAY m.destino PICTURE  ;
  '@s56'
@ 14, 22 CLEAR TO 18, 79
@ 14, 22 SAY MLINE(detalle, 1)
@ 15, 22 SAY MLINE(detalle, 2)
@ 16, 22 SAY MLINE(detalle, 3)
@ 17, 22 SAY MLINE(detalle, 4)
@ 18, 22 SAY MLINE(detalle, 5)
@ 19, 2 SAY '          Importe :'
@ 19, 40 SAY  ;
  '         Contrato :'
@ 20, 2 SAY  ;
  '             IGV? :  '
@ 19, 22 SAY m.valtot PICTURE  ;
  '999,999.99'
@ 19, 60 SAY IIF(EMPTY(m.concon),  ;
  '       ', m.numcon)
@ 20, 24 SAY m.igv PICTURE  ;
  '9999.99'
@ 21, 00 SAY PADC( ;
  '° ®F2¯ Detalle O/S °  ®F4¯ Imprime ° ®F6¯  Liquida °  ®F7¯ Seguimiento °',  ;
  79, ' ') COLOR W+/B 
IF  .NOT. vflag $ 'J*'
     DO subopc
ENDIF
RETURN
*
PROCEDURE revis
SELECT ordse
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
BROWSE FIELDS numos :H = ' N§ ',  ;
       codfte :H = 'Fte', aa =  ;
       vestoc(estado) :H = 'Estd'  ;
       : 4, fecos :H = 'Fecha',  ;
       coddep :H = 'DEP', valtot  ;
       :H = 'Total' :P =  ;
       '999,999.99', zz =  ;
       MLINE(detalle, 1) :H =  ;
       'Descripci¢n' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW windo_0
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
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
vperiodo = RIGHT(DTOC(DATE()), 2)
vnum_os = '    '
vfte = SPACE(2)
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero O/S : ' GET  ;
  vperiodo PICTURE '!!'
@ 1, 26 SAY '-' GET vnum_os  ;
  PICTURE '!!!!' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_os) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vperiodo + vnum_os
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
vnum_os = PADL(ALLTRIM(vnum_os),  ;
          4, '0')
RETURN .T.
*
PROCEDURE anter
SELECT ordse
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
SELECT ordse
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
PRIVATE vnum, nigv
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
             'El O/S ya esta Afectado'
          RETURN
     CASE estado = '50'
          DO standby WITH  ;
             'El O/S ya est  liquidado'
          RETURN
     CASE estado = '51'
          DO standby WITH  ;
             'El O/S ya est  Contabilizado'
          RETURN
ENDCASE
vigv = 'S'
SELECT ordse
SCATTER MEMVAR
IF EMPTY(m.igv)
     vigv = 'N'
ELSE
     vigv = 'S'
ENDIF
vnum = RECNO()
ACTIVATE WINDOW windo_0
DO pantalla
vsino = IIF( .NOT.  ;
        EMPTY(m.numcon), 'S',  ;
        'N')
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
vcodanal = m.codanal
SELECT ordse
IF RLOCK() .OR. f_lock(1)
     @ 1, 22 GET m.periodo  ;
       PICTURE '!!' DISABLE
     @ 1, 24 SAY '-'
     @ 1, 25 GET m.numos PICTURE  ;
       '!!!!' DISABLE
     @ 1, 60 SAY  ;
       IIF(EMPTY(m.memoran),  ;
       m.periodo + '.' + m.numss,  ;
       SUBSTR(m.memoran, 1, 18))
     @ 2, 22 GET m.fecos PICTURE  ;
       '@D' VALID val_ano()
     @ 4, 22 GET m.codprv PICTURE  ;
       '!!!!' VALID  ;
       val_prv(m.codprv,.T.,4, ;
       27)
     @ 5, 22 GET m.codcad PICTURE  ;
       '!!!!' VALID  ;
       val_codcad(m.codcad, ;
       m.periodo,' ',22,30)
     @ 6, 22 GET m.codfte PICTURE  ;
       '!!' VALID  ;
       val_para(m.codfte,'CODFTE', ;
       ' ',22,30)
     READ VALID val_read()
     IF LASTKEY() = 27
          DO vista
          RETURN
     ENDIF
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
     @ 10, 22 SAY  ;
       val_para(maepre.actpry, ;
       'ACTPRY','V',22,40)
     @ 11, 22 GET m.nummes  ;
       PICTURE '!!' VALID  ;
       val_para(m.nummes,'FECMES', ;
       ' ',22,20)
     @ 12, 22 GET m.coddep  ;
       PICTURE '!!!!!!' VALID  ;
       val_para(m.coddep,'CODDEP', ;
       ' ',22,30,7)
     @ 19, 22 GET m.valtot  ;
       PICTURE '999,999.99'
     @ 19, 60 GET vsino PICTURE  ;
       '!' VALID vsino $ 'NS'
     @ 19, 62 GET m.numcon WHEN  ;
       vsino = 'S'
     @ 20, 22 GET vigv PICTURE  ;
       '!' VALID vigv $ 'SN'
     READ
     IF vigv = 'S'
          nigv = IIF(DTOC(m.fecos,  ;
                 1) > '20110228',  ;
                 1.18 , 1.19 )
          m.igv = ROUND(m.valtot -  ;
                  (m.valtot /  ;
                  nigv), 2)
     ELSE
          m.igv = 0
     ENDIF
     @ 20, 24 GET m.igv PICTURE  ;
       '9999.99' WHEN vigv = 'S'
     READ VALID val_read()
     SELECT ordse
     vimpr = m.periodo + m.numos +  ;
             ALLTRIM(m.codfte)
     IF LASTKEY() <> 27
          vcadena = maepre.uniges +  ;
                    maepre.unieje +  ;
                    maepre.codfun +  ;
                    maepre.codprg +  ;
                    maepre.codspr +  ;
                    maepre.actpry
          IF vsino = 'N'
               m.numcon = SPACE(10)
          ELSE
               SELECT parma
               SEEK 'CORREL' +  ;
                    'NUMCON'
               REPLACE nument  ;
                       WITH  ;
                       nument +  ;
                       1
          ENDIF
          DO iteser
          SELECT ordse
          GOTO vnum
          m.user_cr = vuser_id
          m.user_fc = DATE()
          m.user_tp = 'C'
          m.estado = '00'
          GATHER MEMVAR
          ok = detalle()
     ELSE
          DO standby WITH  ;
             'Proceso cancelado'
          SELECT ordse
          GOTO TOP
          GOTO vnum
     ENDIF
ENDIF
UNLOCK ALL
SELECT ordse
DO vista
RETURN
*
PROCEDURE ingre
sincot = .T.
vtipss = ' '
IF escolor
     DEFINE POPUP xcot  FROM 17,55 SHADOW;
COLOR &L_COL
ELSE
     DEFINE POPUP xcot FROM 17,  ;
            55 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF xcot PROMPT  ;
       ' \<Sin Cotizaci¢n '
DEFINE BAR 2 OF xcot PROMPT  ;
       ' \<Con Cotizaci¢n '
DEFINE BAR 3 OF xcot PROMPT  ;
       ' c\<On otro docmto'
ON SELECTION POPUP xcot DEACTIVATE POPUP
ACTIVATE POPUP xcot
DO CASE
     CASE BAR() = 1
          sincot = .T.
          conmem = .F.
     CASE BAR() = 2
          sincot = .F.
          conmem = .F.
     CASE BAR() = 3
          sincot = .F.
          conmem = .T.
ENDCASE
RELEASE POPUP xcot
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado'
     DO vista
     RETURN
ENDIF
DO pantalla
SELECT ordse
SCATTER BLANK MEMVAR
asd = RECNO()
m.fecos = m.fecsis
m.periodo = SUBSTR(DTOC(m.fecsis),  ;
            9, 2)
vnumss = '    '
vano = SUBSTR(DTOC(m.fecsis), 9,  ;
       2)
vsino = 'N'
vnumcon = SPACE(6)
vigv = 'N'
@ 13, 22 CLEAR TO 17, 79
= v_numos() .AND. validar()
@ 1, 22 GET m.periodo PICTURE  ;
  '!!' DISABLE
@ 1, 24 SAY '-'
@ 1, 25 GET m.numos PICTURE  ;
  '!!!!' DISABLE
IF conmem
     @ 1, 40 SAY  ;
       ' Documento Fuente :' GET  ;
       m.memoran PICTURE '@S18'
ELSE
     @ 1, 40 SAY  ;
       '       N£mero S/S :' GET  ;
       vano
     @ 1, 62 SAY '.'
     @ 1, 63 GET vnumss PICTURE  ;
       '!!!!' VALID v_ser()
ENDIF
@ 2, 22 GET m.fecos PICTURE '@D'  ;
  VALID val_ano()
@ 4, 22 GET m.codprv PICTURE  ;
  '!!!!' VALID val_prv(m.codprv, ;
  .T.,4,27)
@ 5, 22 GET m.codcad PICTURE  ;
  '!!!!' VALID  ;
  val_codcad(m.codcad,m.periodo, ;
  ' ',22,30)
@ 6, 22 GET m.codfte PICTURE '!!'  ;
  VALID val_para(m.codfte, ;
  'CODFTE',' ',22,30)
READ VALID val_read()
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
@ 7, 22 SAY  ;
  val_para(maepre.codfun,'CODFUN', ;
  'V',22,40)
@ 8, 22 SAY  ;
  val_para1(maepre.codprg, ;
  'CODPRG' + maepre.codfun,'V',22, ;
  40)
@ 9, 22 SAY  ;
  val_para1(maepre.codspr, ;
  'CODSPR' + maepre.codprg,'V',22, ;
  40)
@ 10, 22 SAY  ;
  val_para(maepre.actpry,'ACTPRY', ;
  'V',22,40)
@ 11, 22 GET m.nummes PICTURE  ;
  '!!' VALID val_para(m.nummes, ;
  'FECMES',' ',22,20) .AND.  ;
  val_mes()
@ 12, 22 GET m.coddep PICTURE  ;
  '!!!!!!' VALID  ;
  val_para(m.coddep,'CODDEP',' ', ;
  22,40)
@ 19, 22 GET m.valtot PICTURE  ;
  '999,999.99'
@ 19, 60 GET vsino PICTURE '!'  ;
  VALID vsino $ 'NS'
@ 19, 62 GET vnumcon WHEN vsino =  ;
  'S'
@ 20, 22 GET vigv PICTURE '!'  ;
  VALID vigv $ 'SN'
READ
IF vigv = 'S'
     nigv = IIF(DTOC(m.fecos, 1) >  ;
            '20110228', 1.18 ,  ;
            1.19 )
     m.igv = ROUND(m.valtot -  ;
             (m.valtot / nigv),  ;
             2)
ENDIF
@ 20, 24 GET m.igv PICTURE  ;
  '9999.99' WHEN vigv = 'S'
READ VALID val_read()
SELECT ordse
IF LASTKEY() <> 27
     vcadena = maepre.uniges +  ;
               maepre.unieje +  ;
               maepre.codfun +  ;
               maepre.codprg +  ;
               maepre.codspr +  ;
               maepre.actpry
     m.fecemi = DATE()
     m.concon = vsino
     m.numcon = IIF(vsino = 'N',  ;
                '     ',  ;
                vnumcon)
     m.estado = '00'
     vimpr = m.periodo + m.numos +  ;
             ALLTRIM(m.codfte)
     DO iteser
     SELECT ordse
     vss = m.periodo + m.numss
     m.user = vuser_id
     m.user_fc = DATE()
     m.user_tp = 'I'
     IF f_appd()
          GATHER MEMVAR
          IF  .NOT. conmem
               SELECT solser
               SEEK vano + vnumss
               SELECT ordse
               REPLACE detalle  ;
                       WITH  ;
                       solser.detalle
          ENDIF
     ENDIF
     ok = detalle()
     SELECT parma
     SEEK 'CORREL' + 'ORDENS'
     REPLACE nument WITH nument +  ;
             1
     IF vsino = 'S'
          SEEK 'CORREL' +  ;
               'NUMCON'
          REPLACE nument WITH  ;
                  nument + 1
     ENDIF
     SELECT solser
     SEEK vss
     IF FOUND()
          IF f_lock(1)
               REPLACE estado  ;
                       WITH '30',  ;
                       numos WITH  ;
                       ordse.numos,  ;
                       peros WITH  ;
                       ordse.periodo
          ENDIF
          UNLOCK
     ENDIF
     SELECT iteos1
     SEEK m.periodo + m.numos
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
     SELECT ordse
     GOTO asd
ENDIF
UNLOCK ALL
SELECT ordse
DO vista
RETURN
*
FUNCTION val_ano
IF YEAR(m.fecos) = YEAR(m.fecsis)
     mret = .T.
ELSE
     DO standby WITH  ;
        'Error en la Fecha... Revise.'
     mret = .F.
ENDIF
RETURN mret
*
PROCEDURE v_numos
SELECT parma
SEEK 'CORRELORDENS'
m.numos = PADL(ALLTRIM(STR(parma.nument +  ;
          1)), 4, '0')
SELECT ordse
RETURN
*
PROCEDURE valmes
IF ALLTRIM(m.nummes) <> vmespro
     DO standby WITH  ;
        'No Coincide con Mes del Calendario'
ENDIF
RETURN
*
FUNCTION v_ser
PRIVATE as, vfun
vfun = .T.
as = ALIAS()
SELECT solser
vtemp = RECNO()
vnumss = PADL(ALLTRIM(vnumss), 4,  ;
         '0')
SEEK vano + vnumss
IF FOUND()
     IF  .NOT. conmem
          IF solser.estado = '30'
               DO standby WITH  ;
                  'La Solicitud de Servicio ya tiene ORDEN '
               RETURN .F.
          ENDIF
          m.detalle = solser.detalle
     ENDIF
ELSE
     SET FILTER TO EMPTY(numss);
.AND. estado = '00'
     vtemp = RECNO()
     vnumss = PADL(ALLTRIM(vnumss),  ;
              4, '0')
     SEEK vano + vnumss
     IF  .NOT. FOUND()
          HIDE MENU mmenu
          ACTIVATE SCREEN
          vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
          DO logos WITH rotulo1,  ;
             vtempo
          ON KEY LABEL F10 KEYBOARD CHR(23)
          IF sincot
               SET FILTER TO EMPTY(numos);
.AND. estado <> '99';
.AND. estado = '00'
          ELSE
               SET FILTER TO EMPTY(numos);
.AND. estado <> '99';
.AND. estado = '20'
          ENDIF
          GOTO TOP
          IF  .NOT. EOF()
               BROWSE FIELDS  ;
                      numss :H =  ;
                      ' N§ ',  ;
                      periodo :H =  ;
                      'A¤', est =  ;
                      IIF(estado =  ;
                      '00',  ;
                      'Pend',  ;
                      IIF(estado =  ;
                      '10',  ;
                      'Progra',  ;
                      IIF(estado =  ;
                      '20',  ;
                      'S/Ct',  ;
                      IIF(estado =  ;
                      '99',  ;
                      'Anul',  ;
                      IIF(estado =  ;
                      '50',  ;
                      'Aten',  ;
                      ' -  ')))))  ;
                      :H = 'ESTD',  ;
                      fecss :H =  ;
                      'Fecha',  ;
                      coddep :H =  ;
                      'DEP.',  ;
                      codfte :H =  ;
                      'FTE.', xx =  ;
                      MLINE(detalle,  ;
                      1) :H =  ;
                      'Descripci¢n'  ;
                      NOMENU  ;
                      NOAPPEND  ;
                      NOEDIT  ;
                      NODELETE  ;
                      WINDOW  ;
                      windo_0
               vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
               DO logos WITH  ;
                  rotulo1,  ;
                  vtempo
               IF LASTKEY() = 27
                    GOTO vtemp
               ENDIF
          ELSE
               DO standby WITH  ;
                  'No existe Solicitud de Servicio a Procesar '
               RETURN .F.
          ENDIF
          SHOW MENU mmenu
          SET FILTER TO
          ON KEY LABEL F10
     ENDIF
ENDIF
vano = solser.periodo
vnumss = solser.numss
m.codcad = solser.codcad
m.numss = solser.numss
m.codprv = solser.codprv
m.codfte = solser.codfte
m.coddep = solser.coddep
m.desos = solser.desss
m.desos1 = solser.desss1
m.desos2 = solser.desss2
m.desos3 = solser.desss3
m.valtot = solser.valtot
m.destino = solser.destino
SELECT (as)
RETURN
*
FUNCTION valprv
PRIVATE xx, vfun
vfun = .F.
codprv = IIF(EMPTY(codprv),  ;
         codprv,  ;
         PADL(ALLTRIM(codprv), 4,  ;
         '0'))
xx = val_prv(codprv,.T.)
IF xx
     REPLACE codprv WITH  ;
             promae.codprv,  ;
             nompro WITH  ;
             promae.nompro
     RETURN .T.
ENDIF
RETURN vfun
*
PROCEDURE anula
SELECT ordse
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF estado <> '00'
     DO standby WITH vmens10
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ANULAR ‚sta Orden de Servicio ?' ;
           )
IF velimina
     SELECT solser
     SEEK m.periodo + m.numss
     IF FOUND() .AND. RLOCK()
          REPLACE solser.estado  ;
                  WITH '00',  ;
                  numos WITH  ;
                  '    ', peros  ;
                  WITH '  '
     ENDIF
     SELECT calen
     REPLACE totos WITH 0 ALL
     SELECT ordse
     IF RLOCK()
          REPLACE ordse.estado  ;
                  WITH '99',  ;
                  fecver WITH  ;
                  DATE(), user  ;
                  WITH SYS(0),  ;
                  user_fc WITH  ;
                  DATE(), user_tp  ;
                  WITH 'A'
     ENDIF
     DO vista
ENDIF
UNLOCK ALL
RETURN
*
PROCEDURE lista
SELECT ordse
SCATTER MEMVAR
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lisser
ENDIF
SELECT ordse
GOTO vtemp
DO vista
RETURN
*
PROCEDURE imprimir
*-----------------
PRIVATE vcon
SELECT ordse
vcon = RECNO()
SCATTER MEMVAR
vnumos = m.nummes + m.numos
SET FILTER TO nummes + numos = vnumos
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO repprg WITH 'LisOS', ' ORDEN DE COMPRA'
ENDIF
SET FILTER TO
SELECT ordse
GOTO vcon
DO vista
RETURN
*
PROCEDURE lisos
*--------------
PARAMETER _desti
PRIVATE nreg
IF _desti = 2
     SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINTER
SET MEMOWIDTH TO 80
impri = .F.
xcolumna = SPACE(7)
GOTO TOP
SCAN
     STORE 0 TO pagina, linea, vsuma, vsumigv
     nreg = RECNO()
     SELECT ordse
     IF pagina = 0 .OR. linea > 60
          DO titulo
     ENDIF
     FOR xx = 1 TO MEMLINES(ordse.detalle)
          @ linea, 4 SAY MLINE(ordse.detalle,xx)
          linea = linea + 1
          IF linea >= 44
               @ linea, 05 SAY REPLICATE('-', 80)
               linea = linea + 1
               DO sumario
               DO titulo
          ENDIF
     ENDFOR
     IF linea >= 43
          DO sumario
          DO titulo
     ENDIF
     @ linea + 1, 05 SAY 'VALOR VTA.===>'
     @ linea + 1, 20 SAY valtot - igv PICTURE '99,999.99'
     @ linea + 2, 05 SAY '    I.G.V.===>'
     @ linea + 2, 20 SAY igv  PICTURE '99,999.99'
     @ linea + 3, 20 SAY '------------'
     @ linea + 4, 05 SAY '     TOTAL===>'
     @ linea + 4, 20 SAY valtot PICTURE '99,999.99'
     @ linea + 5, 05 SAY REPLICATE('Í', 80)
     @ linea + 6, 49 SAY 'TOTAL S/.   '
     @ linea + 6, 66 SAY CHR(27) + 'G'
     @ linea + 6, 68 SAY valtot PICTURE '99,999.99'
     @ linea + 6, 81 SAY CHR(27) + 'H'
     @ linea + 7, 70 SAY REPLICATE('Í', 14)
     @ linea + 8, 05 SAY IIF(EMPTY(destino), ' ', 'Destino :')
     @ linea + 8, 15 SAY ALLTRIM(destino)
     linea = linea + 9
     IF linea >= 43
          DO sumario
          DO titulo
     ENDIF
     @ linea, 10 SAY 'Cadena F. P. SP. A/P   Com. Fte Partida        Importe '
     linea = linea + 1
     SELECT iteos1
     IF SEEK(ordse.periodo + ordse.numos)
          SCAN WHILE iteos1.periodo + iteos1.numos = ordse.periodo + ordse.numos
               @ linea, 10 SAY iteos1.codcad
               @ linea, 16 SAY maepre.codfun
               @ linea, 19 SAY maepre.codprg
               @ linea, 23 SAY maepre.codspr
               @ linea, 28 SAY maepre.actpry
               @ linea, 35 SAY iteos1.codcom
               @ linea, 43 SAY ordse.codfte
               @ linea, 49 SAY LEFT(iteos1.codpart, 1) + '.' + SUBSTR(iteos1.codpart, 2, 1) + '.' + SUBSTR(iteos1.codpart, 3, 2) + '.' + SUBSTR(iteos1.codpart, 5, 2) + '.' + SUBSTR(iteos1.codpart, 7, 2) + '.' + SUBSTR(iteos1.codpart, 9, 2)
               @ linea, 65 SAY iteos1.valpart PICTURE '9,999,999.99'
               @ linea, 86 SAY CHR(27) + 'H' + CHR(15)
               IF  .NOT. EOF()
                    linea = linea + 1
                    vrecno1 = RECNO()
                    DO CASE
                         CASE linea >= 47
                              SELECT 1
                              DO sumario
                              DO titulo
                              @ linea, 52 SAY 'V I E N E N '
                              linea = linea + 2
                              SELECT iteos1
                    ENDCASE
               ENDIF
          ENDSCAN
     ENDIF
     DO sumario
     SELECT ordse
ENDSCAN
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE titulo
pagina = pagina + 1
vtitulo = ' ORDEN DE SERVICIO '
@ 0, 1 SAY CHR(18) + CHR(14)
@ 0, 15 SAY vtitulo + CHR(18)
@ 1, 2 SAY cia
@ 1, 70 SAY 'P g.'
@ 1, 74 SAY pagina PICTURE '##,###'
@ 3, 04 SAY '                   ÚÄÄÄÄÄÄÄÄÄ¿'
@ 4, 04 SAY 'ORDEN DE SERVICIO: ³ ' + ordse.numos + '.' + ordse.nummes + ' ³'
@ 4, 62 SAY 'Estado : ' + vestoc(ordse.estado)
@ 5, 04 SAY '                   ÀÄÄÄÄÄÄÄÄÄÙ'
@ 5, 59 SAY 'Fecha O/S:' + DTOC(ordse.fecos)
@ 6, 66 SAY IIF( .NOT. EMPTY(ordse.numhc), 'H/C :', '  ') + IIF( .NOT. EMPTY(ordse.numhc), ordse.perhc + '.' + ordse.numhc, ' ')
@ 7, 02 SAY ' SE¥OR(ES) : ' + CHR(27) + 'G' + val_prv(ordse.codprv) + CHR(27) + 'H'
@ 8, 02 SAY '     R.U.C.: ' + promae.numruc
@ 9, 02 SAY ' DIRECCION : ' + val_fun('ProMae','CodPrv', 'Dirpro',ordse.codprv)
@ 10, 02 SAY 'REFERENCIA : ' + IIF(EMPTY(numss), 'DOC.:', 'S/S ')
@ 10, 20 SAY IIF(EMPTY(numss), ordse.memoran, ordse.numss + '  ' + valccc())
@ 11, 02 SAY 'SOLICITANTE: ' + ALLTRIM(val_para(coddep,'CODDEP','D',22,60))
@ 12, 02 SAY ' -Facturar a nombre de :' + detaw + ' RUC NRO. : ' + rucw + CHR(15)
@ 13, 05 SAY IIF( .NOT. EMPTY(ordse.numcon), 'CONTRATO :', ' ')
@ 13, 16 SAY IIF( .NOT. EMPTY(ordse.numcon), ordse.numcon, ' ')
@ 14, 01 SAY CHR(18)
@ 14, 05 SAY '           DESCRIPCION DEL SERVICIO'
@ 15, 05 SAY REPLICATE('Í', 80)
linea = 17
RETURN
*
PROCEDURE sumario
@ 46, 1 SAY CHR(18)
@ 47, 07 SAY 'ÚÄ¿                                       ÚÄ¿'
@ 48, 07 SAY '³1³                                       ³3³'
@ 49, 07 SAY 'ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-        ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-'
@ 50, 07 SAY '     Director de Abastecimiento                   Jefe de Almacen '
@ 53, 07 SAY 'ÚÄ¿                                                RECIBI CONFORME:  '
@ 54, 07 SAY '³2³                                              ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ '
@ 55, 07 SAY 'ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-               ³ Fecha:   /  /   ³ '
@ 56, 07 SAY '       Jefe de Adquisiciones                     ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ '
@ 57, 07 SAY CHR(15) + 'Elaborado por: ' + vusua(m.user)
@ 57, 56 SAY CHR(15) + 'Corregido por: ' + IIF( .NOT. EMPTY(m.user_cr), vusua(m.user_cr), '')
@ 58, 1 SAY CHR(15) +  'Nota: Esta Orden es Nula sin la firma del Jefe de Abastecimiento. Nos reservamos el derecho de'
@ 59, 1 SAY CHR(15) +  '      devolver la mercader¡a que no est‚ de acuerdo con nuestras especificaciones.' + CHR(18) + CHR(12)
linea = 1
RETURN
*
PROCEDURE lisser
vorde = ORDER()
DEFINE WINDOW lis FROM 0, 15 TO  ;
       24, 65 FLOAT TITLE  ;
       'Listado Ordenes de Servicio'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro, vlistado, vtofue,  ;
      vtoprv, vtri
vcli = SPACE(4)
vano = '99'
vfte = '  '
vtri1 = '  '
vtri2 = '  '
vcodfte = '  '
vcodprv = '    '
vfecini = DATE()
vfecfin = DATE()
@ 01, 01 SAY  ;
  '     Tipo Listado : ' GET  ;
  vlistado FUNCTION  ;
  '^ Detallado;Resumido'
@ 04, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,07,22)
@ 06, 01 SAY  ;
  '           Fuente : '
@ 06, 22 GET vcodfte PICTURE '!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  'C') WHEN vtofue = 2
@ 08, 01 SAY  ;
  'Todos los Proveed : ' GET  ;
  vtoprv SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtoprv,11,22)
@ 10, 01 SAY  ;
  '      Proveedores : '
@ 10, 22 GET vcodprv PICTURE  ;
  '!!!!' VALID valprv() WHEN  ;
  vtoprv = 2
@ 12, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Numero;Proveedor;Emision;Fuente'
@ 15, 01 SAY  ;
  '           Estado : ' GET  ;
  vtippro FUNCTION  ;
  '^ Todos;Pendientes;Atendidos;Afectados;Menor Cuant¡a;Liquidados'
@ 18, 01 SAY  ;
  '           Fechas : ' GET  ;
  vtri1 PICTURE '!!'
@ 18, 32 GET vtri2 PICTURE '!!'
@ 20, 01 SAY  ;
  '           Fechas : ' GET  ;
  vfecini PICTURE '@D' WHEN  ;
  vtippro = 6
@ 20, 32 GET vfecfin PICTURE '@D'  ;
  WHEN vtippro = 6
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
           numos, IIF(vorden = 2,  ;
           codprv, IIF(vorden = 3,  ;
           DTOS(fecemi), codfte +  ;
           numos))) TO (vind) FOR  ;
           IIF(vtippro = 1, .T.,  ;
           IIF(vtippro = 2,  ;
           estado = '00',  ;
           IIF(vtippro = 3,  ;
           estado = '40',  ;
           IIF(vtippro = 4,  ;
           estado = '20',  ;
           IIF(vtippro = 5,  ;
           estado = '50', estado =  ;
           '50'))))) .AND.  ;
           IIF(vtofue = 1, .T.,  ;
           codfte =  ;
           ALLTRIM(vcodfte))
     SET FILTER TO IIF(vtoprv = 1,;
.T., codprv = ALLTRIM(vcodprv));
.AND. IIF(vtippro = 6, BETWEEN(fecliq,;
vfecini, vfecfin), IIF(EMPTY(vtri1);
.OR. EMPTY(vtri2),;
.T., BETWEEN(MONTH(fecos), VAL(vtri1),;
VAL(vtri2))))
     SET INDEX TO (vind)
     COUNT ALL TO vtotos
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtippro = 1,  ;
               'Listado de Ordenes de Servicio',  ;
               IIF(vtippro = 2,  ;
               'Listado de Ordenenes de Servicio Pendientes',  ;
               IIF(vtippro = 3,  ;
               'Listado de Ordenes de Servicio Atendidas',  ;
               IIF(vtippro = 4,  ;
               'Listado de Ordenes de Servicio Afectadas',  ;
               IIF(vtippro = 5,  ;
               'Listado de Ordenes de Servicio Anuladas',  ;
               'Listado de Ordenes de Servicio Liquidadas' ;
               )))))
     SET MEMOWIDTH TO 80
     IF  .NOT. EOF()
          SET RELATION TO periodo + m.numos;
INTO iteos1
          SET SKIP TO iteos1
          IF vtippro <> 6
               DO CASE
                    CASE vlistado =  ;
                         1
                         SET MEMOWIDTH;
TO 100
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrdS',  ;
                            ' Ordenes de Servicio ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE vlistado =  ;
                         2
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrsX',  ;
                            ' Ordenes de Servicio ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
               ENDCASE
          ELSE
               DO CASE
                    CASE vlistado =  ;
                         1
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrdS',  ;
                            ' Ordenes de Servicio ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE vlistado =  ;
                         2
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisOrsX',  ;
                            ' Ordenes de Servicio ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
               ENDCASE
          ENDIF
          SELECT ordse
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     CLOSE INDEX
     ERASE (vind)
ENDIF
RETURN
*
FUNCTION busprv
PRIVATE ali, vkey
ali = ALIAS()
vkey = codprv
SELECT promae
SEEK vkey
IF FOUND()
     IF estado = 'VG'
          vfun = .F.
     ELSE
          vfun = .T.
     ENDIF
ELSE
     vfun = .T.
ENDIF
SELECT (ali)
RETURN vfun
*
FUNCTION vo
vcli = PADL(ALLTRIM(vcli), 4,  ;
       '0')
RETURN .T.
*
FUNCTION valord
SELECT ordse
vtem = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
SEEK vano + vcli + ALLTRIM(vfte)
IF  .NOT. FOUND()
     HIDE MENU mmenu
     ACTIVATE SCREEN
     vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     ON KEY LABEL F10 KEYBOARD CHR(23)
     BROWSE FIELDS numos :H =  ;
            ' N§ ', est =  ;
            IIF(estado = '00',  ;
            'Pend', IIF(estado =  ;
            '20', 'S/Ct',  ;
            IIF(estado = '99',  ;
            'Anul', IIF(estado =  ;
            '50', 'Aten',  ;
            ' -  ')))) :H =  ;
            'ESTD', fecos :H =  ;
            'Fecha', coddep :H =  ;
            'DEP.', codfte :H =  ;
            'FTE.', desos :H =  ;
            'Descripci¢n' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            windo_0
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     IF LASTKEY() = 27
          GOTO vtemp
     ENDIF
     SHOW MENU mmenu
     ON KEY LABEL F10
ENDIF
vcli = numos
GOTO vtemp
RETURN .T.
*
PROCEDURE termi
ven_accion = .F.
ON KEY LABEL F2
ON KEY LABEL F6
ON KEY LABEL F7
ON KEY LABEL F4
DEACTIVATE MENU
RETURN
*
PROCEDURE estados
USE IN 2
USE IN 4
USE IN 5
USE IN 6
USE IN 12 Cheque ALIAS cheque  ;
    ORDER Cheque1
USE IN 13 Compag ALIAS compag  ;
    ORDER Compag1
DO estado WITH 'OS',  ;
   'm.perhc+m.numhc'
USE IN 2 Solser ALIAS solser  ;
    ORDER Solser1
USE IN 4 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 5 Calen ALIAS calen ORDER  ;
    calen1
USE IN 6 Clase ALIAS clase ORDER  ;
    clase1
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW windo_0
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION valprv
PRIVATE xx, vfun
vfun = .F.
vcodprv = IIF(EMPTY(vcodprv),  ;
          vcodprv,  ;
          PADL(ALLTRIM(vcodprv),  ;
          4, '0'))
xx = val_prv(vcodprv,.T.)
IF xx
     RETURN .T.
ENDIF
RETURN vfun
PRIVATE xx, vfun
vfun = .F.
m.codpr = IIF(EMPTY(m.codprv),  ;
          m.codprv,  ;
          PADL(ALLTRIM(m.codprv),  ;
          4, '0'))
xx = val_prv(m.codprv,.T.)
IF xx
     RETURN .T.
ENDIF
RETURN vfun
*
FUNCTION valct
PARAMETER vnumct
PRIVATE vfun
vfun = .T.
m.numcon = PADL(ALLTRIM(STR(vnumct,  ;
           4)), 4, '0')
IF m.numcon = '0000' .OR.  ;
   EMPTY(m.numcon)
     vfun = .F.
ENDIF
RETURN vfun
*
FUNCTION validar
as = RECNO()
SEEK m.periodo + m.numos
IF FOUND()
     DO standby WITH  ;
        'El O/S ya esta registrado'
     GOTO as
     RETURN .F.
ENDIF
IF  .NOT. EOF()
     GOTO as
ENDIF
RETURN .T.
*
PROCEDURE liquidar
PRIVATE ok
DO CASE
     CASE estado = '00'
          DO standby WITH  ;
             'El Documento a£n no est  afectado'
          DO vista
          RETURN
     CASE estado = '50'
          DO standby WITH  ;
             'El Documento ya est  liquidado'
          DO vista
          RETURN
     CASE estado = '99'
          DO standby WITH  ;
             'El Documento ya est  Anulado'
          DO vista
          RETURN
     CASE estado = '20'
          DO standby WITH  ;
             'El Documento ya est  Afectado'
ENDCASE
IF yesno( ;
   'Est  seguro de Liquidar este O/S' ;
   )
     vfecliq = DATE()
     vprv = m.codprv
     ACTIVATE WINDOW standby
     @ 1, 1 SAY  ;
       'Fecha Liquidaci¢n : ' GET  ;
       vfecliq
     READ
     DEACTIVATE WINDOW standby
     IF RLOCK()
          REPLACE estado WITH  ;
                  '50', fecliq  ;
                  WITH vfecliq,  ;
                  tipdoc WITH  ;
                  'OK'
     ELSE
          DO standby WITH  ;
             'Proceso Cancelado'
     ENDIF
ENDIF
DO vista
RETURN
*
PROCEDURE cont
IF yesno( ;
   'Continua descripci¢n 2?')
     con = .T.
ELSE
     con = .F.
ENDIF
RETURN
*
PROCEDURE conx
IF yesno( ;
   'Continua descripci¢n 3?')
     conx = .T.
ELSE
     conx = .F.
ENDIF
RETURN
*
PROCEDURE cony
IF yesno( ;
   'Continua descripci¢n 4?')
     cony = .T.
ELSE
     cony = .F.
ENDIF
RETURN
*
PROCEDURE iteser
PRIVATE az
az = ALIAS()
SELECT solser
SEEK ordse.periodo + ordse.numss
m.destino = IIF(EMPTY(m.destino),  ;
            solser.destino,  ;
            m.destino)
SELECT (az)
ACTIVATE WINDOW wind_1
@ 0, 0 SAY 'Destino: ' GET  ;
  m.destino PICTURE '@S73'
READ
DEACTIVATE WINDOW wind_1
DO trab_hijo
RETURN
*
FUNCTION detalle
valias = ALIAS()
ON KEY LABEL F10 KEYBOARD CHR(23)
IF  .NOT. WEXIST('Detalle')
     DEFINE WINDOW detalle FROM  ;
            03, 12 TO 20, 67  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Descripci¢n del O/S ±'  ;
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
valias = ALIAS()
ON KEY LABEL F10 KEYBOARD CHR(23);
 
IF  .NOT. WEXIST('Detalle')
     DEFINE WINDOW detalle FROM  ;
            03, 12 TO 20, 67  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Descripci¢n del O/S ±'  ;
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
SELECT (valias)
RETURN .T.
*
FUNCTION valccc
PRIVATE as, vnumccc
USE IN 13 SOLCOT ALIAS solcot  ;
    ORDER Solcot2
USE IN 14 IteSC ALIAS itesc ORDER  ;
    Itesc1
as = ALIAS()
vcc = ordse.numss
SELECT itesc
SET RELATION TO periodo + numsc INTO solcot
SET ORDER TO ITESC1
SEEK vcc
vnumccc = IIF(FOUND(), ' CC/C :' +  ;
          solcot.numccc, '   ')
SET RELATION TO
SELECT (as)
USE IN 13 Compag ALIAS compag  ;
    ORDER Compag1
RETURN vnumccc
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
IF v_reg = 'RG'
     DO standby WITH  ;
        'El Proveedor no esta Regularizado...Observaci¢n'
ENDIF
RETURN vfun
*
FUNCTION trab_hijo
as = ALIAS()
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°F5->Agregar°°°°°°°°°°F8->Eliminar°°°°°°°°°°°F10->Terminar°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F5 DO Agre_itos
ON KEY LABEL F8 DO elim_itos
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteos1
SET ORDER TO ITEOS11
SEEK m.periodo + m.numos
SET RELATION TO periodo + uniges + unieje;
+ codcad INTO maepre ADDITIVE
IF  .NOT. FOUND()
     DO agre_itos
ELSE
     DO borra_os
ENDIF
BROWSE FIELDS codcad :H =  ;
       'Cadena' :V =  ;
       val_codcad(ALLTRIM(codcad), ;
       periodo,'CodCad') :F,  ;
       codcom :H = 'Componente.'  ;
       :V = val_comp(periodo +  ;
       maepre.uniges +  ;
       maepre.unieje + codcad, ;
       codcom,'codcom') :F,  ;
       codmet :H = 'Meta' :V =  ;
       val_meta(periodo +  ;
       maepre.uniges +  ;
       maepre.unieje + codcad, ;
       codcom + codmet,'codmet')  ;
       :F, igv :H = 'IGV' :P =  ;
       '@M S,N', codpart :H =  ;
       'Partida' :V =  ;
       val_cale(codpart,periodo +  ;
       maepre.uniges +  ;
       maepre.unieje +  ;
       maepre.codfun +  ;
       maepre.codprg +  ;
       maepre.codspr +  ;
       maepre.actpry + codcom +  ;
       codmet + ALLTRIM(codfte) +  ;
       ALLTRIM(m.nummes), ;
       'codpart') :F, valpart :H =  ;
       'Monto' :P =  ;
       '99,999,999.99' NOMENU  ;
       NOAPPEND NODELETE WINDOW  ;
       wind_2 KEY m.periodo +  ;
       m.numos
SET RELATION TO
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
SELECT iteos1
SEEK m.periodo + m.numos
vtotcom = 0
SCAN WHILE periodo + numos =  ;
     m.periodo + m.numos
     vtotcom = vtotcom + valpart
     IF valpart = 0
          DELETE NEXT 1
     ENDIF
ENDSCAN
IF vtotcom <> m.valtot
     DO standby WITH ( ;
        'Total Compon. es diferente al total de  O/S...Revise...' ;
        )
ENDIF
DO asiord
SHOW MENU mmenu
ON KEY LABEL F5
ON KEY LABEL F7
ON KEY LABEL F10
SET FILTER TO
SELECT (as)
IF LASTKEY() = 27
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION agre_itos
SELECT iteos1
vp = codpart
vuniges = vuges
vunieje = vueje
IF f_appd()
     REPLACE periodo WITH  ;
             m.periodo, numos  ;
             WITH m.numos, codcad  ;
             WITH m.codcad,  ;
             uniges WITH vuniges,  ;
             unieje WITH vunieje
     UNLOCK
ENDIF
vcodcom = maepre.codcom
vcodmet = maepre.codmet
IF f_lock(1)
     REPLACE codcom WITH vcodcom,  ;
             codmet WITH vcodmet,  ;
             codpart WITH vp, igv  ;
             WITH vigv, valpart  ;
             WITH m.valtot
ENDIF
RETURN .T.
*
PROCEDURE elim_itos
SELECT iteos1
IF RLOCK()
     DELETE NEXT 1
ENDIF
RETURN
*
PROCEDURE borra_os
ax = ALIAS()
SELECT iteos1
SEEK m.periodo + m.numos
IF RLOCK()
     SCAN WHILE periodo + numos =  ;
          m.periodo + m.numos
          SELECT calen
          SEEK m.periodo +  ;
               vcadena +  ;
               iteos1.codcom +  ;
               iteos1.codmet +  ;
               ALLTRIM(m.codfte) +  ;
               ALLTRIM(m.nummes) +  ;
               iteos1.codpart
          REPLACE totos WITH  ;
                  totos -  ;
                  iteos1.valpart
          SELECT iteos1
     ENDSCAN
ENDIF
SELECT (ax)
RETURN
*
FUNCTION val_mes
IF vopcion = 1
     SELECT parma
     SET ORDER TO PARMAE1
     m.fecoc = DATE()
     SEEK 'CORRELORDENC'
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'El Correlativo del Mes no est  Inicializado'
          SELECT orden
          RETURN .F.
     ELSE
          IF codigoaux = '00'
               DO standby WITH  ;
                  'El Calendario del Mes '+ ;
                  ALLTRIM(m.nummes)+ ;
                  ' ya est  cerrado'
               SELECT orden
               RETURN .F.
          ELSE
               xx = IIF(m.nummes =  ;
                    '12', '01',  ;
                    PADL(ALLTRIM(STR((VAL(m.nummes) +  ;
                    1), 2)), 2,  ;
                    '0'))
               IF  .NOT.  ;
                   VAL(m.nummes) =  ;
                   MONTH(DATE())
                    m.fecos = IIF(VAL(m.nummes) <  ;
                              IIF(xx =  ;
                              '01',  ;
                              MONTH(DATE()) +  ;
                              12,  ;
                              MONTH(DATE())),  ;
                              CTOD( ;
                              '01/' +  ;
                              xx +  ;
                              '/' +  ;
                              ALLTRIM(STR(YEAR(m.fecoc) -  ;
                              2000,  ;
                              4))) -  ;
                              1,  ;
                              DATE())
               ENDIF
               SELECT ordse
               RETURN .T.
          ENDIF
     ENDIF
ELSE
     RETURN .T.
ENDIF
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
PROCEDURE corri1
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SELECT ordse
ACTIVATE WINDOW windo_0
SCATTER MEMVAR
@ 2, 55 GET m.codpart
READ VALID val_read()
IF LASTKEY() <> 27
     SELECT ordse
     GATHER MEMVAR
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
SELECT iteos1
SEEK m.periodo + m.numos
SET FILTER TO periodo = m.periodo;
.AND. numos = m.numos
GOTO TOP
SCAN WHILE periodo + numos =  ;
     m.periodo + m.numos
     REPLACE codpart WITH  ;
             m.codpart ALL
ENDSCAN
UNLOCK ALL
SELECT ordse
DO pantalla
DO vista
RETURN
*
PROCEDURE asiord
SELECT asiaut
SEEK 'O/S' + '   ' + 'ASTORD'
IF  .NOT. FOUND()
     DO standby WITH  ;
        'PARAMETRO DE CTAS. DE ORDEN INICIALIZADO, CONSULTE AL AREA DE SISTEMAS'
     RETURN
ELSE
     cctad = dcuenta
     cctah = hcuenta
ENDIF
SELECT astord
SEEK m.periodo + m.nummes +  ;
     m.numos + 'O/S'
IF FOUND()
     FOR i = 1 TO 2
          IF f_lock(1) .OR.  ;
             RLOCK()
               REPLACE periodo  ;
                       WITH  ;
                       m.periodo,  ;
                       nummes  ;
                       WITH  ;
                       m.nummes,  ;
                       numref  ;
                       WITH  ;
                       m.numos,  ;
                       tipdoc  ;
                       WITH 'O/S',  ;
                       fecha WITH  ;
                       m.fecemi,  ;
                       codcta  ;
                       WITH IIF(i =  ;
                       1, cctad,  ;
                       cctah),  ;
                       tipcta  ;
                       WITH IIF(i =  ;
                       1, 'D',  ;
                       'H'),  ;
                       mtodeb  ;
                       WITH IIF(i =  ;
                       1,  ;
                       m.valtot,  ;
                       0), mtohab  ;
                       WITH IIF(i =  ;
                       2,  ;
                       m.valtot,  ;
                       0)
               UNLOCK
               SKIP
          ENDIF
     ENDFOR
ELSE
     FOR i = 1 TO 2
          IF f_appd()
               REPLACE periodo  ;
                       WITH  ;
                       m.periodo,  ;
                       nummes  ;
                       WITH  ;
                       m.nummes,  ;
                       numref  ;
                       WITH  ;
                       m.numos,  ;
                       tipdoc  ;
                       WITH 'O/S',  ;
                       fecha WITH  ;
                       m.fecemi,  ;
                       codcta  ;
                       WITH IIF(i =  ;
                       1, cctad,  ;
                       cctah),  ;
                       tipcta  ;
                       WITH IIF(i =  ;
                       1, 'D',  ;
                       'H'),  ;
                       mtodeb  ;
                       WITH IIF(i =  ;
                       1,  ;
                       m.valtot,  ;
                       0), mtohab  ;
                       WITH IIF(i =  ;
                       2,  ;
                       m.valtot,  ;
                       0)
               UNLOCK
          ENDIF
     ENDFOR
ENDIF
DEFINE WINDOW wastord FROM 10, 10  ;
       TO 15, 70 TITLE  ;
       ' ASIENTOS DE ORDEN' COLOR  ;
       SCHEME 02
ACTIVATE WINDOW wastord
@ 00, 08 SAY 'Cuentas '
@ 00, 18 SAY '        Debe '
@ 00, 34 SAY '        Haber '
@ 01, 04 SAY cctad PICTURE  ;
  '!!!!!!!!!!!'
@ 01, 18 SAY m.valtot PICTURE  ;
  '99,999,999.99'
@ 02, 12 SAY cctah PICTURE  ;
  '!!!!!!!!!!!'
@ 02, 34 SAY m.valtot PICTURE  ;
  '99,999,999.99'
WAIT ' '
DEACTIVATE WINDOW wastord
RELEASE WINDOW wastord
RETURN
*
FUNCTION val_partid
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
     SET FILTER TO tipo = filtro
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
     SET CONFIRM ON
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
mvalor = ALLTRIM(parma.codigo)
mcuenta = parma.descriau2
mdescr = SUBSTR(parma.descri, 1,  ;
         mlong)
mdescriaux = SUBSTR(parma.descriaux,  ;
             1, mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable == 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN ' '
     CASE mvariable == 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescr
     CASE mvariable == 'D'
          RETURN mdescr
     CASE mvariable == 'Z'
          RETURN mdescr
     CASE mvariable == 'C'
          RETURN .T.
     OTHERWISE
          REPLACE &mvariable WITH mvalor
          RETURN .T.
ENDCASE
*
PROCEDURE subopc
PRIVATE calias, cmod
calias = ALIAS()
SELECT subop
cctrlop = ''
cmod = '02'
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

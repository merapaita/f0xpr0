PARAMETER key_docfte, sistema
IF PARAMETERS() = 0
     key_docfte = ''
ENDIF
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
USE IN 10 maepre ALIAS maepre  ;
    ORDER maepre1
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
USE IN 20 USUARIO ALIAS usu ORDER  ;
    USUARIO1
PUBLIC vmes, con, cony, conx, vms,  ;
       v_reg, vcadena, gh
SET MEMOWIDTH TO 56
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
ON KEY LABEL f2 do VisObs
ON KEY LABEL F4 do imprimir
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
       '\Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT  ;
       '\Ingresa' AT 24, 45
DEFINE PAD anula OF mmenu PROMPT  ;
       '\aNula  ' AT 24, 54
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
@ 20, 2 SAY '	            IGV :'
@ 20, 40 SAY  ;
  '         Contrato :'
RETURN
*
PROCEDURE vista
ON KEY LABEL F7 DO ESTADOS
SELECT ordse
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
@ 4, 27 SAY val_prv(m.codprv)
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
@ 19, 22 SAY m.valtot PICTURE  ;
  '999,999.99'
@ 20, 2 SAY '              IGV :'
@ 20, 22 SAY m.igv PICTURE  ;
  '9,999.99'
@ 20, 40 SAY  ;
  '         Contrato :'
@ 20, 60 SAY IIF(EMPTY(m.concon),  ;
  '       ', m.numcon)
@ 21, 00 SAY PADC( ;
  '° ®F2¯ Detalle O/S °  ®F4¯ Imprime ° ®F7¯ Seguimiento °',  ;
  79, ' ') COLOR W+/B 
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
vnum_os = 0
vnumos = '0000'
vfte = '  '
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero O/S : ' GET  ;
  vperiodo PICTURE '!!'
@ 1, 26 SAY '-' GET vnumos  ;
  PICTURE '!!!!'
vnum_os = ALLTRIM(vnumos)
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_os) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     SEEK vperiodo + vnumos
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
vnum_os = PADL(ALLTRIM(STR(vnum_os,  ;
          4)), 4, '0')
RETURN .T.
*
FUNCTION vbusca1
vnumos = PADL(ALLTRIM(vnumos), 4,  ;
         '0')
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
PRIVATE vcon
SELECT ordse
vcon = RECNO()
SCATTER MEMVAR
vnumos = m.nummes + m.numos
SET RELATION TO periodo + m.numos INTO;
iteos1
SET FILTER TO nummes + numos = vnumos
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO reporte WITH 2, 'LisOsX',  ;
        ' Orden de Servicios ',  ;
        2
ENDIF
SET FILTER TO
SELECT ordse
GOTO vcon
DO vista
RETURN
*
PROCEDURE lisser
vorde = ORDER()
DEFINE WINDOW lis FROM 1, 15 TO  ;
       23, 65 FLOAT TITLE  ;
       'Listado Solicitud de Servicios'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vtocli, vorden,  ;
      vtippro, vlistado, vtofue,  ;
      vtoprv
vcli = SPACE(4)
vano = '97'
vfte = '  '
vcodfte = '  '
vprv = '    '
@ 01, 01 SAY  ;
  '     Tipo Listado : ' GET  ;
  vlistado FUNCTION  ;
  '^ por Documento;en Detalle;Resumido;Control'
@ 04, 01 SAY  ;
  '        Total O/S : ' GET  ;
  vtocli SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,5,22) WHEN  ;
  vlistado = 1
@ 05, 01 SAY  ;
  '              O/S : '
@ 05, 22 GET vfte PICTURE '!!'  ;
  VALID val_para(vfte,'CODFTE', ;
  'C') WHEN vtocli = 2 .AND.  ;
  vlistado = 1
@ 05, 26 GET vano PICTURE '!!'  ;
  WHEN vtocli = 2 .AND. vlistado =  ;
  1
@ 05, 28 SAY '-'
@ 05, 29 GET vcli PICTURE '!!!!'  ;
  VALID vo() .AND. valord() WHEN  ;
  vtocli = 2 .AND. vlistado = 1
@ 06, 01 SAY  ;
  'Todos Proveedores : ' GET  ;
  vtoprv SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,7,22) WHEN  ;
  (vlistado <> 1 .AND. vlistado <>  ;
  4)
@ 07, 01 SAY  ;
  '    Proveedor O/S : '
@ 07, 22 GET vprv PICTURE '!!!!'  ;
  VALID val_prv(vprv,.T.) WHEN  ;
  vtoprv = 2
@ 09, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtocli,11,22) WHEN  ;
  (vlistado = 2 .OR. vlistado = 3  ;
  .OR. vlistado = 4)
@ 11, 01 SAY  ;
  '           Fuente : '
@ 11, 22 GET vcodfte PICTURE '!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  'C') WHEN vtofue = 2 .AND.  ;
  (vlistado = 2 .OR. vlistado = 3  ;
  .OR. vlistado = 4)
@ 13, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Numero;Proveedor;Emision;Fuente'  ;
  WHEN vtocli = 1 .AND. (vlistado =  ;
  2 .OR. vlistado = 3 .OR.  ;
  vlistado = 4)
@ 16, 01 SAY  ;
  '           Estado : ' GET  ;
  vtippro FUNCTION  ;
  '^ Todos;Pendientes;Atendidos;Afectados;Anulados;Liquidados'  ;
  WHEN vtocli = 1 .AND. (vlistado =  ;
  2 .OR. vlistado = 3 .OR.  ;
  vlistado = 4)
@ 20, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     vind = SYS(3) + '.IDX'
     IF vlistado <> 4
          INDEX ON IIF(vorden = 1,  ;
                numos, IIF(vorden =  ;
                2, codprv,  ;
                IIF(vorden = 3,  ;
                DTOS(fecemi),  ;
                codfte + numos)))  ;
                TO (vind) FOR  ;
                IIF(vtofue = 1,  ;
                .T., codfte =  ;
                ALLTRIM(vcodfte))  ;
                .AND. IIF(vtippro =  ;
                1, .T.,  ;
                IIF(vtippro = 2,  ;
                estado = '00',  ;
                IIF(vtippro = 3,  ;
                estado = '40',  ;
                IIF(vtippro = 4,  ;
                estado = '20',  ;
                IIF(vtippro = 5,  ;
                estado = '99',  ;
                estado =  ;
                '50')))))
          SET FILTER TO IIF(vtocli = 1,;
.T., periodo + numos + codfte = vano +;
vcli + ALLTRIM(vfte));
.AND. IIF(vtoprv = 2, codprv = vprv,;
.T.)
     ELSE
          INDEX ON IIF(vorden = 1,  ;
                numos, IIF(vorden =  ;
                2, codprv,  ;
                IIF(vorden = 3,  ;
                DTOS(fecemi),  ;
                codfte + numos)))  ;
                TO (vind) FOR  ;
                IIF(vtofue = 1,  ;
                .T., codfte =  ;
                ALLTRIM(vcodfte))  ;
                .AND. busprv()
     ENDIF
     SET INDEX TO (vind)
     COUNT ALL TO vtotos
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtippro = 1,  ;
               'Listado Orden Servicio',  ;
               IIF(vtippro = 2,  ;
               'Listado Orden de Servicio Pendientes',  ;
               IIF(vtippro = 3,  ;
               'Listado Orden de Servicios Afectados',  ;
               IIF(vtippro = 4,  ;
               'Listado Orden de Servicios Anulados',  ;
               'Listado Orden de Servicios Liquidados' ;
               ))))
     IF  .NOT. EOF()
          DO CASE
               CASE vlistado = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'LisOsX',  ;
                       ' Ordenes de Servicios ',  ;
                       2
               CASE vlistado = 2
                    DO reporte  ;
                       WITH 2,  ;
                       'LisOrds',  ;
                       ' Ordenes de Servicios ',  ;
                       1, .F.,  ;
                       .T.
               CASE vlistado = 3
                    DO reporte  ;
                       WITH 2,  ;
                       'LisOrsX',  ;
                       ' Ordenes de Servicios ',  ;
                       1, .F.,  ;
                       .T.
               CASE vlistado = 4
                    DO reporte  ;
                       WITH 2,  ;
                       'Sercont',  ;
                       ' Ordenes de Servicios ',  ;
                       1, .F.,  ;
                       .T.
          ENDCASE
     ELSE
          DO standby WITH vmens08
     ENDIF
     SET FILTER TO
     CLOSE INDEX
     ERASE (vind)
ENDIF
SELECT ordse
SET ORDER TO 1
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
FUNCTION valos
PARAMETER vnumos
PRIVATE vfun
vfun = .T.
m.numos = PADL(ALLTRIM(STR(vnumos,  ;
          4)), 4, '0')
IF m.numos = '0000' .OR.  ;
   EMPTY(m.numos)
     vfun = .F.
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
FUNCTION visobs
valias = ALIAS()
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
USE IN 15
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

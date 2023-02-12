PARAMETER sistema
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
USE IN 7 KARDEX ALIAS kardex  ;
    ORDER KARDEX1
USE IN 8 cdrnec ALIAS cuadro  ;
    ORDER Cdrnec1
USE IN 9 itecn ALIAS itecn ORDER  ;
    itecn3
USE IN 12 astpat ALIAS astpat  ;
    ORDER Astpat37
IF sistema = '1'
     USE IN 10 maepre ALIAS  ;
         maepre ORDER maepre1
ELSE
     USE IN 10 maepre ALIAS  ;
         maepre ORDER maepre3
ENDIF
USE IN 11 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 13 iteoc ALIAS iteoc ORDER  ;
    iteoc9
USE IN 20 USUARIO ALIAS usu ORDER  ;
    usuario1
USE IN 21 STOCK ALIAS stockali  ;
    ORDER STOCK1
USE IN 0 IteUsuOp ALIAS subop  ;
    ORDER IteUsuOp1
vmens01 = ' Pecosas : REVISION '
vmens02 = 'Registro de Pecosa'
vmens04 = 'Dicho Pecosa no fue encontrado'
vmens05 = 'No existe Pecosa anterior'
vmens06 = 'No existe Pecosa siguiente'
vmens07 = '¨ Desea Anular ‚ste Pecosa ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Pecosa ha sido anulado'
vmens10 = 'El Pecosa ya est  Atendido'
vmens11 = 'El Pecosa ha sido devuelto'
vmens12 = 'El Pecosa ya tiene O/C'
ON KEY LABEL F11 DO CORRI
SELECT pecosa
GOTO BOTTOM
ON KEY LABEL F2 DO FECREP
ON KEY LABEL F7 DO VISTA_ING   
PUBLIC veces
SCATTER BLANK MEMVAR
PUBLIC vtotp
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  liQuida  Ingresa           Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 13, 79 TITLE vmens02  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_2 FROM 14, 00  ;
       TO 23, 79 TITLE  ;
       'Detalle: Pecosa     ± ®F9¯ Detalle:Item ± ®F7¯ Coloca precio ± ®F4¯ Imprime'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 20, 63  ;
       TO 22, 78 TITLE 'TOTAL '  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_5 FROM 14, 00  ;
       TO 23, 79 TITLE  ;
       '± ®F11¯ Liquidaci¢n Parcial ± ®F12¯ Habilitaci¢n Parcial ±'  ;
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
       'li\<Quida' AT 24, 36
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
ON SELECTION PAD corri OF mmenu DO liquida
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
@ 1, 40 SAY '    N£mero Pecosa :'
@ 2, 2 SAY '      Dependencia :'
@ 3, 2 SAY '      Liquidaci¢n :'
@ 4, 2 SAY '       Cadena Fun.:'
@ 5, 2 SAY ' F.Financiamiento :'
@ 6, 2 SAY '          Funci¢n :'
@ 7, 2 SAY '         Programa :'
@ 8, 2 SAY '      Subprograma :'
@ 9, 2 SAY '  Activ./Proyect. :'
@ 10, 2 SAY '          Destino :'
@ 11, 2 SAY '     Obs. Almacen :'
RETURN
*
PROCEDURE vista
SELECT pecosa
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
ON KEY LABEL F4 DO Imprimir
ON KEY LABEL F9 DO vista_det
SCATTER MEMVAR
= val_codcad(ALLTRIM(m.codcad), ;
  m.periodo,'C')
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
       '®Corregido por ' + IIF(  ;
       .NOT. EMPTY(m.user_cr),  ;
       ALLTRIM(m.user_cr),  ;
       ALLTRIM(m.user)) + '¯', 79,  ;
       ' '), PADC( ;
       '®Elaborado por ' +  ;
       ALLTRIM(m.user) + '¯', 79,  ;
       ' '))
     @ 0, 02 SAY IIF(m.tippec =  ;
       'S', 'Pecosa Stock     ',  ;
       IIF(m.tippec = 'O',  ;
       'Pecosa Compra    ',  ;
       IIF(m.tippec = 'B',  ;
       'Pecosa Bayovar',  ;
       IIF(m.tippec = 'T',  ;
       'Pecosa Transporte',  ;
       'Pecosa Caja Chica'))))  ;
       COLOR SCHEME 02
     @ 0, 60 SAY  ;
       vestpec(m.estado) COLOR  ;
       SCHEME 02
     @ 1, 22 SAY m.fecpec
     @ 1, 60 SAY m.periodo
     @ 1, 63 SAY m.numpec
     @ 2, 22 SAY  ;
       val_para(m.coddep,'CODDEP', ;
       'A',22,40,7)
     @ 3, 22 SAY m.fecdesp
     @ 4, 22 SAY  ;
       val_codcad(m.codcad, ;
       m.periodo,'D',22,30)
     @ 5, 22 SAY  ;
       val_para(m.codfte,'CODFTE', ;
       'D',22,30)
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
     @ 11, 22 SAY m.obsalma  ;
       PICTURE '@S50'
     DO vista_hijo
     IF m.estado = '50'
          DO total
     ELSE
          DEACTIVATE WINDOW  ;
                     wind_3
     ENDIF
ENDIF
IF  .NOT. vflag $ 'J*'
     DO subopc
ENDIF
RETURN
*
FUNCTION calcula
SELECT itepec
SEEK m.periodo + m.numpec
vtotal = 0
SCAN WHILE itepec.periodo =  ;
     m.periodo .AND.  ;
     itepec.numpec = m.numpec
     vtotal = vtotal +  ;
              itepec.preuni *  ;
              itepec.candesp *  ;
              IIF(itepec.estado =  ;
              '50' .OR.  .NOT.  ;
              EMPTY(fecdesp), 1,  ;
              0)
ENDSCAN
SEEK m.periodo + m.numpec
SELECT pecosa
RETURN vtotal
*
FUNCTION calcula_s
SELECT itepec
SEEK m.periodo + m.numpec
vtotal = 0
SCAN WHILE itepec.periodo =  ;
     m.periodo .AND.  ;
     itepec.numpec = m.numpec
     vtotal = vtotal +  ;
              itepec.cosmed *  ;
              itepec.candesp *  ;
              IIF(itepec.estado =  ;
              '50' .OR.  .NOT.  ;
              EMPTY(fecdesp), 1,  ;
              0)
ENDSCAN
SEEK m.periodo + m.numpec
SELECT pecosa
RETURN vtotal
*
PROCEDURE vista_hijo
HIDE POPUP ALL
SELECT itepec
veces = 1
BROWSE NOOPTIMIZE FIELDS esx =  ;
       buscoc() :H = ' ' :W = .F.,  ;
       ess = IIF(tippec = 'S',  ;
       'Stock', verestdo()) :H =  ;
       'Estado' :W = .F., descri  ;
       :H = 'Descripci¢n' : 33 :W =  ;
       .F., coduni :H = 'Uni' :W =  ;
       .F. : 3, canreq :H =  ;
       'Pedido' :P = '99,999.99'  ;
       :W =  .NOT. EMPTY(codart),  ;
       candesp :H = 'Atendido' :P =  ;
       '99,999.99', aa =  ;
       IIF(preuni = 0, cosmed,  ;
       preuni) :H = 'Precio ' :P =  ;
       '9,999,999.9999', xx =  ;
       IIF(EMPTY(fecdesp),  ;
       '        ', fecdesp) :H =  ;
       'Fec Aten' NOMENU NOAPPEND  ;
       NOEDIT NODELETE NOCLEAR  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.numpec  ;
       TIMEOUT 0.001  NOREFRESH
SELECT pecosa
RETURN
*
FUNCTION buscoc
PRIVATE vmens
veces = 1
vmens = '*'
DO CASE
     CASE estado = '50' .AND.  ;
          tippec = 'S'
          vmens = 'û'
     CASE estado = '30'
          alis = ALIAS()
          vkey = periodo + numpec +  ;
                 newfte + codart +  ;
                 numord
          SELECT iteoc
          SEEK vkey
          vmens = IIF(FOUND(),  ;
                  IIF(estado =  ;
                  '50', 'û',  ;
                  IIF(estado =  ;
                  '95', 'R',  ;
                  '')), 'î')
          SELECT (alis)
     CASE estado = '50' .AND.  ;
          tippec <> 'S'
          alis = ALIAS()
          vkey = periodo + numpec +  ;
                 newfte + codart +  ;
                 numord
          SELECT iteoc
          SEEK vkey
          IF FOUND()
               IF estado <> '50'
                    IF estado =  ;
                       '9'
                    ELSE
                    ENDIF
                    vmens = 'R'
               ELSE
                    vmens = 'û'
               ENDIF
          ELSE
               vmens = '?'
          ENDIF
          SELECT (alis)
     CASE estado = '99'
          vmens = ''
     CASE estado = '00'
          vmens = ''
ENDCASE
RETURN vmens
*
FUNCTION verestdo
PRIVATE vmens
vmens = '---'
DO CASE
     CASE estado = '00'
          vmens = 'Pendte'
     CASE estado = '10'
          vmens = 'Progra'
     CASE estado = '20'
          vmens = 'SC' + numsc
     CASE estado = '30'
          vmens = 'OC' + numoc
     CASE estado = '50'
          vmens = 'OC' + numoc
     CASE estado = '99'
          vmens = 'Anulad'
ENDCASE
RETURN vmens
*
PROCEDURE imprimir
PRIVATE vcon
SELECT pecosa
vcon = RECNO()
SCATTER MEMVAR
fdbf = SYS(3) + '.DBF'
fidx = SYS(3) + '.IDX'
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
     DIMENSION afecha( 200)
     afecha = {}
     i = 0
     SCAN
          vfecha = itepec.fecdesp
          ldescarta = .F.
          FOR j = 1 TO i
               IF afecha(j) =  ;
                  vfecha
                    ldescarta = .T.
                    EXIT
               ENDIF
          ENDFOR
          IF  .NOT. ldescarta
               i = i + 1
               afecha( i) =  ;
                     vfecha
          ENDIF
     ENDSCAN
     DEFINE WINDOW w_fecha FROM  ;
            05, 10 TO 15, 60  ;
            TITLE vmens01 DOUBLE  ;
            COLOR SCHEME 10
     ACTIVATE WINDOW w_fecha
     nfecha = 1
     @ 01, 01 SAY  ;
       'Selecciones Fecha :'  ;
       COLOR SCHEME 5 GET nfecha  ;
       DEFAULT afecha(1) SIZE 6,  ;
       14 FROM afecha
     READ
     RELEASE WINDOW w_fecha
     SELECT itepec
     SET FILTER TO itepec.fecdesp = afecha(nfecha)
     GOTO TOP
     SELECT astpat
     SET FILTER TO periodo = def.periodo;
.AND. nummes = PADL(ALLTRIM(STR(MONTH(afecha(nfecha)))),;
2, '0');
.AND. numref = numpec;
.AND. fecha = afecha(nfecha);
.AND. tipdoc = 'PEC'
     SELECT def
     DO repprg WITH 'PECOSARep',  ;
        ' ORDEN DE COMPRA'
ENDIF
SET RELATION TO
SELECT astpat
SET FILTER TO
SELECT itepec
SET FILTER TO
SELECT pecosa
GOTO vcon
DO vista
RETURN
*
PROCEDURE pecosarep
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
STORE 0 TO pagina, linea, vsuma,  ;
      vnum
nreg = RECNO()
SELECT itepec
SCAN WHILE def.numpec =  ;
     itepec.numpec
     IF pagina = 0 .OR. linea >  ;
        60
          DO titulo
     ENDIF
     DO CASE
          CASE linea < 39
               vnum = vnum + 1
               @ linea, 00 SAY  ;
                 CHR(15)
               @ linea, 01 SAY  ;
                 PADL(ALLTRIM(STR(vnum,  ;
                 2)), 2, '0')
               @ linea, 04 SAY  ;
                 CHR(179)
               @ linea, 05 SAY  ;
                 itepec.codart
               @ linea, 15 SAY  ;
                 CHR(179)
               @ linea, 16 SAY  ;
                 itepec.canreq
               @ linea, 25 SAY  ;
                 CHR(179)
               @ linea, 26 SAY  ;
                 LEFT(ALLTRIM(itepec.descri),  ;
                 50)
               @ linea, 70 SAY  ;
                 CHR(179)
               @ linea, 71 SAY  ;
                 CHR(179)
               @ linea, 73 SAY  ;
                 itepec.numoc
               @ linea, 78 SAY  ;
                 CHR(179)
               @ linea, 79 SAY  ;
                 itepec.candesp  ;
                 PICTURE  ;
                 '@Z 99,999.99'
               @ linea, 88 SAY  ;
                 CHR(179)
               @ linea, 89 SAY  ;
                 itepec.fecdesp
               @ linea, 99 SAY  ;
                 CHR(179)
               @ linea, 100 SAY  ;
                 itepec.coduni  ;
                 PICTURE '@TI'
               @ linea, 111 SAY  ;
                 CHR(179)
               @ linea, 123 SAY  ;
                 CHR(179)
               @ linea, 137 SAY  ;
                 CHR(179)
               vsuma = vsuma +  ;
                       ROUND(itepec.preuni *  ;
                       itepec.candesp,  ;
                       3)
               linea = linea + 1
          CASE linea >= 39
               @ linea, 1 SAY  ;
                 CHR(18)
               @ linea, 05 SAY  ;
                 REPLICATE('-',  ;
                 80)
               linea = linea + 1
               @ linea, 50 SAY  ;
                 'S U B T O T A L   S/.'
               linea = linea + 2
               DO sumario
               DO titulo
               SKIP -1 IN itepec
               @ linea, 01 SAY  ;
                 CHR(18)
               @ linea, 52 SAY  ;
                 'V I E N E N  S/.'
               linea = linea + 1
          OTHERWISE
     ENDCASE
     IF  .NOT.  ;
         EMPTY(ALLTRIM(RIGHT(itepec.descri,  ;
         50)))
          @ linea, 26 SAY  ;
            SUBSTR(itepec.descri,  ;
            51, 50)
          linea = linea + 1
     ENDIF
     IF  .NOT.  ;
         EMPTY(itepec.observa)
          FOR xx = 1 TO  ;
              MEMLINES(itepec.observa)
               @ linea, 26 SAY  ;
                 MLINE(itepec.observa,  ;
                 xx)
               linea = linea + 1
               IF linea >= 39
                    @ linea, 1  ;
                      SAY  ;
                      CHR(18)
                    @ linea, 05  ;
                      SAY  ;
                      REPLICATE( ;
                      '-', 80)
                    linea = linea +  ;
                            1
                    @ linea, 56  ;
                      SAY  ;
                      '    V A N   S/.'
                    linea = linea +  ;
                            2
                    DO sumario
                    DO titulo
                    @ linea, 01  ;
                      SAY  ;
                      CHR(18)
                    @ linea, 52  ;
                      SAY  ;
                      'V I E N E N  S/.'
                    @ linea, 79  ;
                      SAY  ;
                      CHR(15)
                    linea = linea +  ;
                            1
               ENDIF
          ENDFOR
     ENDIF
ENDSCAN
@ linea, 1 SAY CHR(18)
@ linea, 05 SAY REPLICATE('Í',  ;
  80)
linea = linea + 1
@ linea, 56 SAY 'T O T A L   S/.'
linea = linea + 1
@ linea, 71 SAY 'ÍÍÍÍÍÍÍÍÍÍÍÍ'
linea = linea + 1
IF linea >= 37
     @ linea, 05 SAY REPLICATE( ;
       '-', 80)
     linea = linea + 1
     DO sumario
     DO titulo
     linea = linea + 1
ENDIF
SELECT astpat
GOTO TOP
linea = linea + 2
@ linea, 0 SAY CHR(18)
@ linea, 04 SAY PADC( ;
  'CUENTAS PATRIMONIALES', 47)
linea = linea + 1
@ linea, 04 SAY PADC('CUENTAS',  ;
  10)
@ linea, 18 SAY PADC('DEBE', 13)
@ linea, 34 SAY PADC('HABER', 13)
linea = linea + 1
@ linea, 04 SAY REPLICATE('~',  ;
  47)
linea = linea + 1
SCAN
     IF linea > 60
          DO titulo
     ENDIF
     DO CASE
          CASE linea < 42
               @ linea, 04 SAY  ;
                 codcta PICTURE  ;
                 '!!!!!!!!!!'
               linea = linea + 1
          CASE linea >= 42
               linea = linea + 1
               DO sumario
               DO titulo
               linea = linea + 1
               SKIP -1
          OTHERWISE
     ENDCASE
ENDSCAN
GOTO nreg
DO sumario
SELECT def
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE titulo
pagina = pagina + 1
vtitulo = ' PEDIDO - COMPROBANTE DE SALIDA '
@ 0, 01 SAY CHR(15)
@ 0, 02 SAY ALLTRIM(cia)
@ 0, 62 SAY CHR(18) + CHR(14)
@ 0, 66 SAY 'PEDIDO N§ ' +  ;
  def.numpec + '.' + def.periodo +  ;
  CHR(27) + 'H' + CHR(15)
@ 0, 93 SAY 'P g.'
@ 0, 98 SAY pagina PICTURE '####'
@ 1, 01 SAY CHR(18) + CHR(14)
@ 1, 04 SAY vtitulo + CHR(27) +  ;
  'H' + CHR(18) + CHR(15)
@ 1, 40 SAY 'Tipo:' +  ;
  IIF(def.tippec = 'S', 'Stock',  ;
  IIF(def.tippec = 'O', 'Compra',  ;
  IIF(def.tippec = 'T',  ;
  'Transporte', 'Referencia')))
@ 1, 53 SAY  ;
  PADL(ALLTRIM(STR(MONTH(DATE()))),  ;
  2, '0') + '/' +  ;
  ALLTRIM(STR(YEAR(DATE())))
@ 2, 01 SAY  ;
  'Dependencia Solicitante:'
@ 2, 26 SAY val_para(def.coddep, ;
  'CODDEP','D',22,60)
@ 2, 77 SAY 'Lugar y Fecha:'
@ 2, 92 SAY fecha(def.fecpec)
@ 3, 0 SAY  ;
  'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 4, 00 SAY  ;
  '³Solicito entregar a :' +  ;
  def.atte
@ 4, 110 SAY 'DEPENDENCIA :' +  ;
  def.coddep
@ 4, 132 SAY '³'
@ 5, 00 SAY '³CodDestino a:' +  ;
  ALLTRIM(def.destino) + ' ' +  ;
  ',los siguientes art¡culos:'
@ 5, 132 SAY '³'
@ 6, 00 SAY  ;
  'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
@ 7, 0 SAY  ;
  'ÚÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 8, 0 SAY  ;
  '³Rgl³ C¢digo   ³Cantidad ³              Descripci¢n                   ³³N§ O/C³DesPacho ³   Fecha  ³  Unidad   ³    P.U.   ³  T O T A L  ³'
@ 9, 0 SAY  ;
  'ÀÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
linea = 10
RETURN
*
PROCEDURE sumario
@ 43, 0 SAY CHR(15)
@ 43, 1 SAY  ;
  'Fte. Financiamiento:'
@ 43, 22 SAY  ;
  ALLTRIM(val_para(pecosa.codfte, ;
  'CODFTE','D',22,60))
@ 44, 06 SAY IIF( .NOT.  ;
  EMPTY(observa),  ;
  'OBSERVACIONES :', ' ')
@ 44, 22 SAY LEFT(obsalma, 67)
@ 44, 99 SAY  ;
  'ÚÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿'
@ 45, 22 SAY SUBSTR(obsalma, 68,  ;
  67)
@ 45, 99 SAY  ;
  '³Cadena³Func.³Prog.³Subpr.³Act./Proy.³'
@ 46, 22 SAY SUBSTR(obsalma, 135,  ;
  67)
@ 46, 99 SAY  ;
  'ÃÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´'
@ 47, 99 SAY CHR(179)
@ 47, 101 SAY codcad
@ 47, 106 SAY CHR(179)
@ 47, 108 SAY  ;
  ALLTRIM(maepre.codfun)
@ 47, 112 SAY CHR(179)
@ 47, 114 SAY  ;
  ALLTRIM(maepre.codprg)
@ 47, 118 SAY CHR(179)
@ 47, 120 SAY  ;
  ALLTRIM(maepre.codspr)
@ 47, 125 SAY CHR(179)
@ 47, 127 SAY  ;
  ALLTRIM(maepre.actpry)
@ 47, 136 SAY CHR(179)
@ 48, 06 SAY 'Elaborado por: ' +  ;
  ALLTRIM(vusua(IIF(user_tp = 'I',  ;
  user, user_cr)))
@ 48, 99 SAY  ;
  'ÀÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ'
@ 49, 01 SAY  ;
  '__ Equipo de Oficina ........ S/.          __ Gastos de Operaci¢n ......... S/.            __ Bienes en Deposito ..... S/.'
@ 50, 01 SAY  ;
  '__ Equipo de Transporte ..... S/.          __ Reconstrucci¢n de Equipos.... S/.            __ Pedido en Transito ..... S/.'
@ 51, 01 SAY  ;
  '__ Maquinaria y Equipo ...... S/.          __ Construcciones en Curso...... S/.            __                  ....... S/.'
@ 54, 03 SAY  ;
  'ÚÄ¿                            ÚÄ¿                             ÚÄ¿                             ÚÄ¿'
@ 55, 03 SAY  ;
  '³1³                            ³2³                             ³3³                             ³4³'
@ 56, 03 SAY  ;
  'ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ    ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ     ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ     ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ'
@ 57, 03 SAY  ;
  '         Solicitante               Jefe de Abastecimiento             Jefe de Almacen           Recib¡ comforme:    /  /'
@ 59, 1 SAY CHR(15) +  ;
  val_boy(PADL(ALLTRIM(STR(MONTH(pecosa.fecpec),  ;
  2)), 2, '0'),'DERNI¥','D')
linea = 1
@ 60, 1 SAY CHR(12)
RETURN
*
PROCEDURE total
ACTIVATE WINDOW wind_3
IF m.tippec = 'S'
     @ 0, 0 SAY calcula_s()  ;
       PICTURE '9,999,999.99'
ELSE
     @ 0, 0 SAY calcula() PICTURE  ;
       '9,999,999.99'
ENDIF
RETURN
*
PROCEDURE vista_ing
IF estado <> '50' .OR. tippec <>  ;
   'S'
     DO standby WITH  ;
        'El Pe.Co.Sa. no esta liquidada o no es de Stock'
     DO vista
     RETURN
ENDIF
HIDE POPUP ALL
valor = 0
ON KEY LABEL F7
SELECT itepec
BROWSE FIELDS esx = IIF(estado =  ;
       '00', ' ', IIF(estado =  ;
       '20', ' ', IIF(estado =  ;
       '99', 'x', IIF(estado =  ;
       '50', 'û', ' ')))) :H =  ;
       ' ' :W = .F., ess =  ;
       IIF(tippec = 'S', 'Stock',  ;
       IIF(estado = '00',  ;
       'Pendte', IIF(estado =  ;
       '10', 'Progra', IIF(estado =  ;
       '20', 'SC' + numsc,  ;
       IIF(estado = '99',  ;
       'Anulad', IIF(estado =  ;
       '50', 'OC' + numoc, 'OC' +  ;
       numoc)))))) :H = 'Estado'  ;
       :W = .F., descri :H =  ;
       'Descripci¢n' : 33 :W =  ;
       .F., coduni :H = 'Uni' :W =  ;
       .F. : 3, canreq :H =  ;
       'Pedido' :P = '99,999.99'  ;
       :W = .F., candesp :H =  ;
       'Atendido' :P =  ;
       '99,999.99' :W = .F.,  ;
       costot :H = 'Total' :P =  ;
       '99,999.99' :V = divide()  ;
       :F, cosmed :H = 'Precio '  ;
       :P = '99,999.999' :V =  ;
       multipli() :F, xx =  ;
       IIF(EMPTY(fecdesp),  ;
       '        ', fecdesp) :H =  ;
       'Fec Aten' :W = .F. WINDOW  ;
       wind_2 KEY m.periodo +  ;
       m.numpec
SELECT pecosa
ON KEY LABEL F7 DO VISTA_ING   
DO vista
RETURN
*
FUNCTION divide
IF candesp <> 0
     REPLACE cosmed WITH costot /  ;
             candesp
ELSE
     REPLACE cosmed WITH 0
ENDIF
RETURN .T.
*
FUNCTION multipli
IF candesp <> 0
     REPLACE costot WITH cosmed *  ;
             candesp
ELSE
     REPLACE costot WITH 0
ENDIF
RETURN .T.
*
PROCEDURE vista_det
HIDE POPUP ALL
ON KEY LABEL F9 DO OBSERVA
SELECT itepec
BROWSE NOOPTIMIZE FIELDS esx =  ;
       IIF(estado = '00', ' ',  ;
       IIF(estado = '20', ' ',  ;
       IIF(estado = '99', 'x',  ;
       IIF(estado = '50', 'û',  ;
       ' ')))) :H = ' ' :W = .F.,  ;
       ess = IIF(tippec = 'S',  ;
       'Stock', IIF(estado = '00',  ;
       'Pendte', IIF(estado =  ;
       '10', 'Progra', IIF(estado =  ;
       '20', 'SC' + numsc,  ;
       IIF(estado = '99',  ;
       'Anulad', IIF(estado =  ;
       '50', 'OC' + numoc, 'OC' +  ;
       numoc)))))) :H = 'Estado'  ;
       :W = .F., descri :H =  ;
       'Descripci¢n' : 34 :W =  ;
       .F., coduni :H = 'Uni' :W =  ;
       .F. : 3, canreq :H =  ;
       'Pedido' :P = '99,999.99'  ;
       :W =  .NOT. EMPTY(codart),  ;
       candesp :H = 'Atendido' :P =  ;
       '99,999.99', qq =  ;
       IIF(tippec = 'S', cosmed,  ;
       preuni) :H = 'Preuni' :P =  ;
       '99,999.99', aa =  ;
       IIF(preuni = 0, cosmed *  ;
       candesp, preuni * candesp)  ;
       :H = 'Precio Total ' :P =  ;
       '9,999,999.9999', xx =  ;
       IIF(EMPTY(fecdesp),  ;
       '        ', fecdesp) :H =  ;
       'Fec Aten' NOMENU NOAPPEND  ;
       NOEDIT NODELETE NOCLEAR  ;
       WINDOW wind_2 KEY  ;
       m.periodo + m.numpec  ;
       NOREFRESH
SELECT pecosa
ON KEY LABEL F9 DO vista_det   
DO vista
RETURN
*
FUNCTION observa
valias = ALIAS()
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F9
IF  .NOT. WEXIST('Observa')
     DEFINE WINDOW observa FROM  ;
            05, 18 TO 18, 61  ;
            FLOAT NOCLOSE SHADOW  ;
            TITLE  ;
            '± Detalle Pecosa ±'  ;
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
MODIFY MEMO observa NOEDIT WINDOW  ;
       observa
IF  .NOT. WVISIBLE('Observa')
     ACTIVATE WINDOW observa
ENDIF
RELEASE WINDOW observa
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado. No graba la Observaci¢n '
ENDIF
ON KEY LABEL F9 DO Observa
RETURN .T.
*
PROCEDURE revis
SELECT pecosa
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SET RELATION TO periodo + numpec INTO;
itepec
SET SKIP TO itepec
IF escolor
     DEFINE POPUP Busmenu FROM 15,50;
 SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP busmenu FROM 15,  ;
            50 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF busmenu PROMPT  ;
       ' ordenado por:  \<N£mero      '
DEFINE BAR 2 OF busmenu PROMPT  ;
       ' ordenado por:  \<Dependencia '
ON SELECTION POPUP busmenu DEACTIVATE;
POPUP
ACTIVATE POPUP busmenu
DO CASE
     CASE BAR() = 1
          SET ORDER TO pecosa1
     CASE BAR() = 2
          SET ORDER TO pecosa3
ENDCASE
IF LASTKEY() = 27
     SELECT pecosa
     SET ORDER TO PECOSA1
     DO vista
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS numpec :H = ' N§ ',  ;
       est = IIF(itepec.estado =  ;
       '00', 'Pend',  ;
       IIF(itepec.estado = '10',  ;
       'Progra',  ;
       IIF(itepec.estado = '30',  ;
       itepec.numoc, IIF(estado =  ;
       '99', 'Anul',  ;
       IIF(itepec.estado = '50',  ;
       'Aten', 'S/Ct'))))) :H =  ;
       'Estd', codfte :H = 'Fte ',  ;
       fecpec :H = 'Fecha',  ;
       coddep :H = 'Dep',  ;
       itepec.canreq :H =  ;
       'Cantid' :P = '99,999.99',  ;
       itepec.descri :H =  ;
       'Detalle ' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELECT pecosa
SET ORDER TO PECOSA1
SET RELATION TO
DO vista
RETURN
*
PROCEDURE xrevis
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
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS numpec :H = ' N§ ',  ;
       codfte :H = 'Fte', est =  ;
       vestoc(estado) :H = 'ESTD'  ;
       : 4, fecpec :H = 'Fecha',  ;
       coddep :H = 'DEP',  ;
       itepec.canreq :H =  ;
       'Cantidad' :P = '99,999',  ;
       itepec.descri :H =  ;
       'Detalle ' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELECT pecosa
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
vnum_pec = '    '
vcod_fte = '   '
ACTIVATE WINDOW standby
@ 1, 01 SAY  ;
  'Ingrese N£mero Pecosa: ' GET  ;
  vperiodo PICTURE '!!'
@ 1, 27 SAY '-' GET vnum_pec  ;
  PICTURE '!!!!' VALID vbusca()
READ
DEACTIVATE WINDOW standby
IF EMPTY(vnum_pec) .OR. LASTKEY() =  ;
   27
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
RETURN
*
FUNCTION vbusca
vnum_pec = PADL(ALLTRIM(vnum_pec),  ;
           4, '0')
RETURN .T.
*
PROCEDURE anter
SELECT pecosa
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
SELECT pecosa
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
PROCEDURE liquida
SELECT pecosa
vre = RECNO()
SCATTER MEMVAR
IF m.estado = '50'
     DO standby WITH  ;
        'El Pe.co.sa. ya est  liquidada'
     DO vista
     RETURN
ENDIF
IF m.estado <> '40'
     DO standby WITH  ;
        'Todav¡a no esta atendido'
     DO vista
     RETURN
ENDIF
ACTIVATE WINDOW standby
@ 1, 5 SAY 'Fecha de despacho:'  ;
  GET m.fecdesp
READ VALID val_read()
DEACTIVATE WINDOW standby
IF LASTKEY() <> 27
     m.estado = IIF(m.estado =  ;
                '51', '51',  ;
                '50')
     GATHER MEMVAR
ENDIF
DO vista
RETURN
*
PROCEDURE corri
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SELECT pecosa
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@ 3, 22 GET m.fecdesp
READ VALID val_read()
IF LASTKEY() <> 27
     SELECT pecosa
     GATHER MEMVAR
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
SELECT pecosa
DO pantalla
DO vista
RETURN
*
PROCEDURE ingre
SELECT pecosa
IF estado = '50'
     DO standby WITH  ;
        'El Pe.co.sa ya est  liquidada'
     DO vista
     RETURN
ENDIF
DO CASE
     CASE tippec = 'O' .AND.  ;
          estado = '10'
          DO standby WITH  ;
             'El Pe.co.sa no est  atendido'
          RETURN
     CASE tippec = 'B' .AND.  ;
          estado = '10'
          DO standby WITH  ;
             'El Pe.co.sa no est  atendido'
          RETURN
ENDCASE
ACTIVATE WINDOW wind_1
vre = RECNO()
SCATTER MEMVAR
@ 11, 22 GET m.obsalma FUNCTION  ;
  'S50'
READ
IF LASTKEY() <> 27
     DO WHILE .T.
          ok = trabaja_hi()
          IF LASTKEY() <> 27  ;
             .AND. ok
               IF yesno( ;
                  '¨ Confirme el ingreso ?' ;
                  )
                    DO wstockx
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
          SET FILTER TO estado <> '99'
          SET RELATION TO periodo + numpec;
INTO pecosa
          SEEK m.periodo +  ;
               m.numpec
          vig = 0
          ves = 0
          vfec = itepec.fecdesp
          entro = .F.
          SCAN WHILE periodo +  ;
               numpec = m.periodo +  ;
               m.numpec
               IF itepec.candesp1 =  ;
                  0
                    ves = ves + 1
               ELSE
                    SELECT itepec
                    IF RLOCK()
                         REPLACE candesp  ;
                                 WITH  ;
                                 candesp +  ;
                                 candesp1,  ;
                                 estado  ;
                                 WITH  ;
                                 IIF(candesp =  ;
                                 canreq,  ;
                                 '50',  ;
                                 '00'),  ;
                                 candesp1  ;
                                 WITH  ;
                                 0
                    ENDIF
                    IF (candesp <  ;
                       canreq)
                         IF estado <>  ;
                            '50'
                              vig =  ;
                               vig +  ;
                               1
                         ENDIF
                    ENDIF
               ENDIF
               entro = .T.
          ENDSCAN
          SELECT pecosa
          entro = .T.
          SEEK m.periodo +  ;
               m.numpec
          IF entro
               IF vig = 0 .AND.  ;
                  ves = 0
                    m.estado = IIF(m.estado =  ;
                               '51',  ;
                               '51',  ;
                               '40')
               ENDIF
               m.user_tp = 'C'
               m.user_cr = vuser_id
               m.user_fc = m.fecha
               GATHER MEMVAR
          ELSE
               DO standby WITH  ;
                  ' No se pudo Atender este Pe.co.sa. '
          ENDIF
     ENDIF
     SELECT itepec
     SEEK m.periodo + m.numpec
     DIMENSION afec( 200, 2)
     afec = ''
     i = 0
     SCAN WHILE periodo + numpec =  ;
          m.periodo + m.numpec
          vfecha = itepec.fecdesp
          vmonto = 0
          ldescarta = .F.
          FOR j = 1 TO i
               IF afec(j,1) =  ;
                  vfecha
                    afec( j, 2) =  ;
                        afec(j,2) +  ;
                        itepec.candesp *  ;
                        itepec.preuni
                    ldescarta = .T.
                    EXIT
               ENDIF
          ENDFOR
          IF  .NOT. ldescarta  ;
              .AND.  .NOT.  ;
              EMPTY(vfecha)
               i = i + 1
               afec( i, 1) =  ;
                   vfecha
               afec( i, 2) =  ;
                   itepec.candesp *  ;
                   itepec.preuni
          ENDIF
     ENDSCAN
     ntot = i
     i = 0
     FOR i = 1 TO ntot
          gfec = afec(i,1)
          iigv = IIF(DTOC(gfec,  ;
                 1) > '20110228',  ;
                 1.18 , 1.19 )
          afec( i, 2) =  ;
              ROUND((afec(i,2)) /  ;
              iigv, 2)
     ENDFOR
     DIMENSION afec( i, 2)
     i = 0
     j = 0
     xtmp = {}
     FOR i = 1 TO ntot - 1
          FOR j = i + 1 TO ntot
               IF afec(i,1) >  ;
                  afec(j,1)
                    xtmp = afec(i, ;
                           1)
                    afec( i, 1) =  ;
                        afec(j, ;
                        1)
                    afec( j, 1) =  ;
                        xtmp
                    xtmp = afec(i, ;
                           2)
                    afec( i, 2) =  ;
                        afec(j, ;
                        2)
                    afec( j, 2) =  ;
                        xtmp
               ENDIF
          ENDFOR
     ENDFOR
     FOR i = 1 TO ntot
          SELECT astpat
          IF  .NOT.  ;
              SEEK(m.periodo +  ;
              PADL(ALLTRIM(STR(MONTH(afec(i, ;
              1)))), 2, '0') +  ;
              m.numpec + 'PEC' +  ;
              DTOC(afec(i,1),  ;
              1))
               DO agrite1 WITH  ;
                  afec(i,1),  ;
                  afec(i,2)
          ENDIF
     ENDFOR
     SELECT itepec
     SEEK m.periodo + m.numpec
     DO ingap
     SELECT itepec
     SET ORDER TO ITEPEC1
     SET RELATION TO
     SET FILTER TO
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT pecosa
GOTO vre
DO pantalla
DO vista
RETURN
*
FUNCTION trabaja_hi
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°Presione ®F10¯ para salir grabando o  ®Esc¯ para cancelar°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F9 DO Obs_Item
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F11 do liqparc
ON KEY LABEL F12 do nliqparc
SELECT itepec
SET RELATION TO periodo + numpec INTO;
pecosa
SET ORDER TO ITEPEC1
SET FILTER TO estado <> '99'
SEEK m.periodo + m.numpec
IF FOUND()
     IF pecosa.tippec = 'S'
          BROWSE FIELDS esx =  ;
                 IIF(estado =  ;
                 '00', ' ',  ;
                 IIF(estado =  ;
                 '20', ' ',  ;
                 IIF(estado =  ;
                 '99', 'x',  ;
                 IIF(estado =  ;
                 '50', 'û',  ;
                 ' ')))) :H = ' '  ;
                 :W = .F., descri  ;
                 :H =  ;
                 'Descripci¢n' :  ;
                 35 :W = .F.,  ;
                 coduni :H =  ;
                 'Uni' :W = .F. :  ;
                 3, canreq :H =  ;
                 'Pedido' :P =  ;
                 '99,999.999' :W =  ;
                 .F., candesp :H =  ;
                 'Desp.' :P =  ;
                 '99,999.999' :W =  ;
                 estado <> '50'  ;
                 :V = val_can()  ;
                 :F, fecdesp :H =  ;
                 'Atendido' :W =  ;
                 estado <> '50'  ;
                 .AND. candesp >  ;
                 0, preuni :H =  ;
                 'PreUni' :W =  ;
                 estado <> '50'  ;
                 :P =  ;
                 '9,999,999.9999'  ;
                 NOMENU NOAPPEND  ;
                 NODELETE WINDOW  ;
                 wind_5 KEY  ;
                 m.periodo +  ;
                 m.numpec
     ELSE
          BROWSE FIELDS esx =  ;
                 IIF(estado =  ;
                 '00', ' ',  ;
                 IIF(estado =  ;
                 '20', ' ',  ;
                 IIF(estado =  ;
                 '99', 'x',  ;
                 IIF(estado =  ;
                 '50', 'û',  ;
                 ' ')))) :H = ' ',  ;
                 ess = IIF(estado =  ;
                 '00', 'Pendte',  ;
                 IIF(estado =  ;
                 '10', 'Progra',  ;
                 IIF(estado =  ;
                 '20', 'SC' +  ;
                 numsc,  ;
                 IIF(estado =  ;
                 '99', 'Anulad',  ;
                 IIF(estado =  ;
                 '50', 'Atendi',  ;
                 'OC' +  ;
                 numoc))))) :H =  ;
                 'Estado' :W =  ;
                 .F., descri :H =  ;
                 'Descripci¢n' :  ;
                 35 :W = .F.,  ;
                 coduni :H =  ;
                 'Uni' :W = .F. :  ;
                 3, canreq :H =  ;
                 'Pedido' :P =  ;
                 '99,999.999' :W =  ;
                 .F., candesp :H =  ;
                 'Desp.' :P =  ;
                 '99,999.999' :W =  ;
                 estado <> '50'  ;
                 :V = val_can()  ;
                 :F, fecdesp :H =  ;
                 'Atendido' :W =  ;
                 estado <> '50'  ;
                 .AND. candesp >  ;
                 0, preuni :H =  ;
                 'PreUni' :W =  ;
                 estado <> '50'  ;
                 :P =  ;
                 '9,999,999.9999'  ;
                 NOMENU NOAPPEND  ;
                 NODELETE WINDOW  ;
                 wind_5 KEY  ;
                 m.periodo +  ;
                 m.numpec
     ENDIF
     ON KEY LABEL F9
     ON KEY LABEL F10
     ON KEY LABEL F11
     ON KEY LABEL F12
     SET FUNCTION F12 TO
     SET FILTER TO
     ACTIVATE SCREEN
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     SHOW MENU mmenu
     SELECT pecosa
     RETURN .T.
ELSE
     DO standby WITH  ;
        'No se tiene articulos con Orden de Compra'
     SET FILTER TO
     ACTIVATE SCREEN
     vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO logos WITH rotulo1,  ;
        vtempo
     SHOW MENU mmenu
     SELECT pecosa
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE ingap
PRIVATE cnum
USE IN 0 cuentas ALIAS cuenta  ;
    ORDER cuentas1
SELECT astpat
SET FILTER TO periodo + numref + tipdoc;
= m.periodo + m.numpec + 'PEC'
ON KEY LABEL F5 DO agrite
ON KEY LABEL F8 DO eliite
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS periodo, fecha :H =  ;
       'Desp.' :V = valf() :F,  ;
       numref :F, codcta :H =  ;
       'Cuenta' :V =  ;
       val_fun('Cuenta','Cuenta', ;
       "Cuenta+' '+Descri",codcta, ;
       2) :F, tipcta :H = 'Tp' :P =  ;
       '@M D,H', mtodeb :H =  ;
       'Monto Debe' :W = tipcta =  ;
       'D' :P = '99,999,999.99',  ;
       mtohab :H = 'Monto Haber'  ;
       :W = tipcta = 'H' :P =  ;
       '99,999,999.99' NOMENU  ;
       NOAPPEND NODELETE WINDOW  ;
       wind_2
GOTO TOP
STORE 0 TO vdebe, vhaber
SCAN WHILE periodo = m.periodo  ;
     .AND. numref = m.numpec  ;
     .AND. tipdoc = 'PEC'
     vdebe = vdebe + IIF(tipcta =  ;
             'D', mtodeb, 0)
     vhaber = vhaber + IIF(tipcta =  ;
              'H', mtohab, 0)
ENDSCAN
SET FILTER TO
IF vdebe <> vhaber
     DO standby WITH  ;
        'Ojo: No cuadra debe con haber'
ENDIF
USE IN cuenta
DEACTIVATE WINDOW wind_12
RETURN
*
FUNCTION valf
REPLACE nummes WITH  ;
        PADL(MONTH(fecha), 2,  ;
        '0')
RETURN .T.
*
PROCEDURE valnm
REPLACE nummes WITH  ;
        PADL(MONTH(fecha), 2,  ;
        '0')
RETURN
*
PROCEDURE agrite
SELECT astpat
FOR i = 1 TO 2
     IF f_appd()
          REPLACE periodo WITH  ;
                  m.periodo,  ;
                  numref WITH  ;
                  m.numpec,  ;
                  tipdoc WITH  ;
                  'PEC', tipcta  ;
                  WITH IIF(i = 1,  ;
                  'D', 'H')
     ENDIF
ENDFOR
RETURN
*
PROCEDURE agrite1
PARAMETER xfec, xmto
PRIVATE i
SELECT astpat
FOR i = 1 TO 2
     IF f_appd()
          REPLACE periodo WITH  ;
                  m.periodo,  ;
                  nummes WITH  ;
                  PADL(MONTH(xfec),  ;
                  2, '0'), numref  ;
                  WITH m.numpec,  ;
                  tipdoc WITH  ;
                  'PEC', fecha  ;
                  WITH xfec,  ;
                  tipcta WITH  ;
                  IIF(i = 1, 'D',  ;
                  'H'), mtodeb  ;
                  WITH IIF(i = 1,  ;
                  xmto, 0),  ;
                  mtohab WITH  ;
                  IIF(i = 2, xmto,  ;
                  0)
     ENDIF
ENDFOR
RETURN
*
PROCEDURE eliite
SELECT astpat
IF RLOCK()
     DELETE NEXT 1
ENDIF
RETURN
*
FUNCTION obs_item
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F9
DEFINE WINDOW detalle FROM 03, 12  ;
       TO 20, 67 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       ALLTRIM(descri) FOOTER  ;
       ' ° ®Esc¯ Sale ° ' DOUBLE  ;
       COLOR SCHEME 1
IF WVISIBLE('Detalle')
     ACTIVATE WINDOW SAME detalle
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              detalle
ENDIF
MODIFY MEMO observa WINDOW  ;
       detalle
IF  .NOT. WVISIBLE('Detalle')
     ACTIVATE WINDOW detalle
ENDIF
RELEASE WINDOW detalle
ON KEY LABEL F9 DO Obs_Item
RETURN .T.
*
FUNCTION confirma
SELECT itepec
alis = ALIAS()
vkey = ALLTRIM(periodo + numpec +  ;
       codart)
SELECT iteoc
SEEK vkey
IF FOUND() .AND. estado = '50'
     vok = .T.
ELSE
     DO standby WITH  ;
        'La O/C no est  liquidada..'
     vok = .F.
ENDIF
SELECT (alis)
RETURN vok
*
FUNCTION fecrep
SET FUNCTION F3 TO DTOC(fecdesp)
RETURN fecdesp
*
PROCEDURE liqparc
IF itepec.candesp = itepec.canreq
     REPLACE estado WITH '50'
ENDIF
*
PROCEDURE nliqparc
DO CASE
     CASE orden = 'û'
          REPLACE estado WITH  ;
                  '30'
     CASE orden = ' ' .AND. llave =  ;
          'û'
          REPLACE estado WITH  ;
                  '20'
     OTHERWISE
          REPLACE estado WITH  ;
                  '00'
ENDCASE
*
FUNCTION val_can
PRIVATE vfun
vfun = .T.
IF candesp > canreq
     DO standby WITH  ;
        'Se esta exediendo en '+ ;
        ALLTRIM(STR(candesp- ;
        vresta, 10))
     vfun = .F.
ENDIF
RETURN vfun
*
FUNCTION val_fec
IF EMPTY(fecdesp)
     IF RLOCK()
          REPLACE fecdesp WITH  ;
                  DATE()
     ELSE
          RETURN .F.
     ENDIF
ENDIF
RETURN .T.
*
FUNCTION val_fec
IF EMPTY(fecdesp)
     IF RLOCK()
          REPLACE fecdesp WITH  ;
                  DATE()
     ELSE
          RETURN .F.
     ENDIF
ENDIF
RETURN .T.
*
FUNCTION agreg_item
IF f_appd()
     REPLACE periodo WITH  ;
             m.periodo, numpec  ;
             WITH m.numpec,  ;
             estado WITH  ;
             m.estado
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
REPLACE codcad WITH m.codcad
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
           '¨ Desea ANULAR ‚ste Pecosa ?' ;
           )
vestado = .T.
SELECT itepec
SEEK m.periodo + m.numpec
SCAN WHILE m.periodo = periodo  ;
     .AND. m.numpec = numpec
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
               m.numpec = numpec
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
PROCEDURE lista
SELECT pecosa
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO lispec
ENDIF
SELECT pecosa
SET RELATION TO
SET FILTER TO
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
      vtippec, vtop
vnumpec = SPACE(4)
vfte = SPACE(2)
vcodmes = SPACE(2)
vcoddep = SPACE(6)
vcodfte = SPACE(2)
vfecini = DATE()
vfecfin = DATE()
vpp = SPACE(2)
@ 01, 01 SAY  ;
  'Todas las Pecosas : ' GET  ;
  vtopec SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtopec,2,22)
@ 02, 01 SAY  ;
  '           Pecosa : '
@ 02, 22 GET vnumpec PICTURE  ;
  '!!!!' WHEN vtopec = 2
@ 02, 27 SAY '-'
@ 02, 29 GET vfte PICTURE '!!'  ;
  VALID val_para(vfte,'CODFTE', ;
  'C') WHEN vtopec = 2
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
@ 06, 01 SAY  ;
  'Todas las Fuentes : ' GET  ;
  vtofue SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtofue,7,22) WHEN vtopec =  ;
  1
@ 07, 01 SAY  ;
  '           Fuente : '
@ 07, 22 GET vcodfte PICTURE '!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  'C') WHEN vtopec = 1 .AND.  ;
  vtofue = 2
@ 08, 01 SAY  ;
  'Todas las Dependc : ' GET  ;
  vtodep SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  valtod(vtodep,9,22) WHEN vtopec =  ;
  1
@ 09, 01 SAY  ;
  '      Dependencia : '
@ 09, 22 GET vcoddep PICTURE  ;
  '!!!!!!' VALID val_para(vcoddep, ;
  'CODDEP','C') WHEN vtopec = 1  ;
  .AND. vtodep = 2
@ 11, 01 SAY  ;
  '     Ordenado por : ' GET  ;
  vorden FUNCTION  ;
  '^ Numero;Dependencia;Emision;Fuente'  ;
  WHEN vtopec = 1
@ 14, 01 SAY  ;
  '           Estado : ' GET  ;
  vtiplis FUNCTION  ;
  '^ Todos;Emitidos;Atendidos;Liquidados;Anulados'  ;
  WHEN vtopec = 1
@ 17, 01 SAY  ;
  '      Tipo Pecosa : ' GET  ;
  vtippec FUNCTION  ;
  '^ Todos;Compra;Stock;Transporte;Referencial;C.Chica'
@ 20, 01 SAY  ;
  '           Fechas : ' GET  ;
  vfecini PICTURE '@D' WHEN  ;
  vtiplis = 4
@ 20, 32 GET vfecfin PICTURE '@D'  ;
  WHEN vtiplis = 4
@ 21, 01 SAY  ;
  '       Mes Propym : ' GET vpp  ;
  PICTURE '@D' WHEN vtiplis = 5
@ 22, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1 .AND. LASTKEY() <>  ;
   27
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento........'
     vind = SYS(3) + '.IDX'
     SET RELATION TO periodo + numpec;
INTO itepec
     INDEX ON IIF(vorden = 1,  ;
           itepec.periodo +  ;
           itepec.numpec +  ;
           itepec.codfte,  ;
           IIF(vorden = 2, coddep,  ;
           IIF(vorden = 3,  ;
           DTOS(fecemi),  ;
           codfte))) TO (vind)  ;
           FOR IIF(vtopec = 1,  ;
           .T., numpec + codfte =  ;
           vnumpec +  ;
           ALLTRIM(vfte)) .AND.  ;
           IIF(vtiplis = 1, .T.,  ;
           IIF(vtiplis = 2,  ;
           estado = '00',  ;
           IIF(vtiplis = 3,  ;
           estado = '30' .OR.  ;
           estado = '40',  ;
           IIF(vtiplis = 4,  ;
           estado = '50', estado =  ;
           '99'))))
     SET FILTER TO IIF(vtomes = 1,;
.T., MONTH(fecpec) = VAL(vcodmes));
.AND. IIF(vtop = 1,;
.T., itepec.mes = VAL(vpp));
.AND. IIF(vtofue = 1,;
.T., itepec.codfte = ALLTRIM(vcodfte));
.AND. IIF(vtodep = 1,;
.T., coddep = ALLTRIM(vcoddep));
.AND. IIF(vtiplis = 4, BETWEEN(fecdesp,;
vfecini, vfecfin),;
.T.);
.AND. IIF(vtippec = 1,;
.T., IIF(vtippec = 2, tippec = 'O', IIF(vtippec;
= 3, tippec = 'S', IIF(vtippec = 4, tippec;
= 'R', tippec = 'C'))))
     SET INDEX TO (vind)
     COUNT ALL TO vtotp
     SET SKIP TO itepec
     GOTO TOP
     DEACTIVATE WINDOW standby
     vtitulo = IIF(vtiplis = 1,  ;
               ' en General ',  ;
               IIF(vtiplis = 2,  ;
               ' Pendientes ',  ;
               ' Atendidos '))
     vtippo = IIF(vtippec = 1,  ;
              ' ', IIF(vtippec =  ;
              2,  ;
              'Pecosa por Compra',  ;
              IIF(vtippec = 3,  ;
              'Pecosa por Stock',  ;
              'Pesosa por Transporte' ;
              )))
     IF  .NOT. EOF()
          IF vtopec = 2
               DO reporte WITH 2,  ;
                  'Pecsal1',  ;
                  ' Pe.co.sa '
          ELSE
               DO CASE
                    CASE vtiplis =  ;
                         4
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'Pecsalx',  ;
                            ' Pe.co.sa ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE vtiplis =  ;
                         5
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'PROPYM',  ;
                            ' Pe.co.sa ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
                    CASE vtiplis <>  ;
                         4
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'Pecsal',  ;
                            ' Pe.co.sa ',  ;
                            1,  ;
                            .F.,  ;
                            .T.
               ENDCASE
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
GOTO TOP
GOTO vrec
RETURN
*
PROCEDURE qlista
SELECT pecosa
vtemp = RECNO()
SET RELATION TO periodo + numpec INTO;
itepec
SET SKIP TO itepec
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  .NOT. EOF()
     DO reporte WITH 2, 'LisPec',  ;
        ' Pe.co.sa s '
ELSE
     DO standby WITH  ;
        'Archivo Vac¡o '
ENDIF
SELECT pecosa
SET RELATION TO
GOTO vtemp
DO vista
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
ON KEY LABEL F12
ON KEY LABEL F3
ON KEY LABEL F2
ON KEY LABEL F4
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_c1
RELEASE WINDOW wind_3
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION valpec
PARAMETER vnumpec
PRIVATE vfun
vfun = .T.
m.numpec = PADL(ALLTRIM(STR(vnumpec,  ;
           4)), 4, '0')
IF m.numpec = '0000' .OR.  ;
   EMPTY(m.numpec)
     vfun = .F.
ENDIF
RETURN vfun
*
FUNCTION valart
PARAMETER _cod
PRIVATE xx, vfun
vfun = .F.
xx = val_artdet(_cod,.F.)
IF xx
     SELECT itepec
     REPLACE coduni WITH  ;
             iteart.coduni,  ;
             preuni WITH  ;
             iteart.preuni,  ;
             descri WITH  ;
             iteart.descri
     vfun = .T.
ENDIF
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
PROCEDURE vmes
m = IIF(mes = '03', 'MARZO',  ;
    IIF(mes = '04', 'ABRIL',  ;
    IIF(mes = '05', 'MAYO',  ;
    IIF(mes = '06', 'JUNIO',  ;
    IIF(mes = '07', 'JULIO',  ;
    IIF(mes = '08', 'AGOSTO',  ;
    IIF(mes = '09', 'SETIEMBRE',  ;
    IIF(mes = '10', 'OCTUBRE',  ;
    IIF(mes = '11', 'NOVIEMBRE',  ;
    'DICIEMBRE')))))))))
*
PROCEDURE wstockx
savali = ALIAS()
buspec = numpec
vfecvis = fecpec
SELECT itepec
SELECT stockali
SEEK m.periodo + 'DESP' + buspec
IF  .NOT. FOUND()
     SELECT itepec
     SEEK m.periodo + buspec
     DO WHILE  .NOT. EOF() .AND.  ;
        periodo=m.periodo .AND.  ;
        numpec=buspec
          periw = periodo
          codaw = codart
          numow = numpec
          codfw = codfte
          canrw = canreq
          preuw = preuni
          fecow = fecoc
          SELECT stockali
          APPEND BLANK
          REPLACE periodo WITH  ;
                  periw
          REPLACE codart WITH  ;
                  codaw
          REPLACE tipdoc WITH  ;
                  'DESP'
          REPLACE numdoc WITH  ;
                  numow
          REPLACE fuente WITH  ;
                  codfw
          REPLACE cantidad WITH  ;
                  canrw
          REPLACE cosmed WITH  ;
                  preuw
          REPLACE fechamov WITH  ;
                  vfecvis
          REPLACE tipomov WITH  ;
                  'S'
          SELECT itepec
          SKIP
     ENDDO
ELSE
     SCAN WHILE periodo =  ;
          m.periodo .AND. tipdoc =  ;
          'DESP' .AND. numdoc =  ;
          buspec
          IF RLOCK()
               DELETE NEXT 1
          ENDIF
     ENDSCAN
     SELECT itepec
     SEEK m.periodo + buspec
     DO WHILE  .NOT. EOF() .AND.  ;
        periodo=m.periodo .AND.  ;
        numpec=buspec
          periw = periodo
          codaw = codart
          numow = numpec
          codfw = codfte
          canrw = canreq
          preuw = preuni
          fecow = fecoc
          SELECT stockali
          APPEND BLANK
          REPLACE periodo WITH  ;
                  periw
          REPLACE codart WITH  ;
                  codaw
          REPLACE tipdoc WITH  ;
                  'DESP'
          REPLACE numdoc WITH  ;
                  numow
          REPLACE fuente WITH  ;
                  codfw
          REPLACE cantidad WITH  ;
                  canrw
          REPLACE cosmed WITH  ;
                  preuw
          REPLACE fechamov WITH  ;
                  vfecvis
          REPLACE tipomov WITH  ;
                  'S'
          SELECT itepec
          SKIP
     ENDDO
ENDIF
SELECT (savali)
RETURN
*
PROCEDURE subopc
PRIVATE calias, cmod
calias = ALIAS()
SELECT subop
cctrlop = ''
cmod = '05'
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

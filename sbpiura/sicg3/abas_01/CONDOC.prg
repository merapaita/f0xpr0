USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 2 Compag ALIAS compag  ;
    ORDER Compag1
USE IN 3 Hojcon ALIAS hoja ORDER  ;
    Hojcon1
USE IN 4 OrdCom ALIAS orden ORDER  ;
    OrdCom1
USE IN 5 ItePec ALIAS itepec  ;
    ORDER ItePec12
USE IN 6 Cajas ALIAS caja ORDER  ;
    Cajas1
USE IN 7 SolSer ALIAS solser  ;
    ORDER SolSer4
USE IN 8 Ordser ALIAS ordser  ;
    ORDER OrdSer1
USE IN 9 Cheque ALIAS cheque  ;
    ORDER Cheque1
USE IN 10 Pecosa ALIAS pecosa  ;
    ORDER Pecosa1
USE IN 15 Auxil ALIAS auxil ORDER  ;
    Auxil1
USE IN 17 Promae ALIAS promae  ;
    ORDER Promae1
DEFINE WINDOW wind_1 FROM 10, 20  ;
       TO 14, 60 FLOAT TITLE  ;
       ' Periodo de Consulta '  ;
       DOUBLE COLOR SCHEME 5
DEFINE WINDOW wind_4 FROM 17, 16  ;
       TO 21, 64 FLOAT TITLE  ;
       ' ±± ESC = SALIR ±± '  ;
       DOUBLE COLOR SCHEME 05
DEFINE WINDOW wind_2 FROM 00, 01  ;
       TO 23, 79 FLOAT DOUBLE  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_3 FROM 02, 16  ;
       TO 22, 64 FLOAT TITLE  ;
       '±± F2=BUSCAR ±±  POR NOMBRE  ±± F10=SALIR ±±'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_8 FROM 06, 26  ;
       TO 16, 79 FLOAT TITLE  ;
       ' Seguimiento de Documento Fuente '  ;
       DOUBLE COLOR SCHEME 05
PRIVATE vnummes, vnumcp, vcodctc,  ;
        vfeccp, vnummesss,  ;
        vnumss
DIMENSION vsw[ 6]
FOR i = 1 TO 6
     vsw = 0
ENDFOR
STORE SPACE(14) TO vcodctc
STORE SPACE(04) TO vnumcp
DO WHILE .T.
     vdbf = SYS(3) + '.DBF'
     SELECT 12
     CREATE DBF (vdbf) (nummes C  ;
            (2), numcp C (4),  ;
            codctc C (14), numhc  ;
            C (4), nummeshc C (2),  ;
            tipdoc C (2), feccp D  ;
            (8), glosa C (200),  ;
            usuario C (8), fecref  ;
            D (8), import N (18,  ;
            2), reten N (18, 2))
     USE
     USE IN 12 EXCLUSIVE (vdbf)  ;
         ALIAS temp
     FOR i = 1 TO 6
          vsw[ i] = 0
     ENDFOR
     STORE SPACE(14) TO vnumchq
     STORE SPACE(04) TO vnumhc,  ;
           vnumoc
     STORE SPACE(02) TO vnummes,  ;
           vnummeshc, vperhc,  ;
           vnummesoc, vnummesss
     STORE SPACE(03) TO vtipdoc,  ;
           vfte
     STORE SPACE(33) TO vnombre
     STORE 0 TO vimport, vreten,  ;
           vliquido, vtiempo
     vnummes = '0' +  ;
               ALLTRIM(STR(MONTH(DATE())))
     vcodprv = '    '
     okcancel = 1
     vbus = 1
     DO prov
     IF LASTKEY() = 27
          CLOSE DATABASES
          RETURN
     ENDIF
ENDDO
RELEASE WINDOW wind_2
RELEASE WINDOW wind_8
CLOSE DATABASES
RETURN
*
PROCEDURE prov
SET CONFIRM OFF
vnom = SPACE(1)
vfecini = DATE()
vfecfin = DATE()
SELECT 1
ACTIVATE WINDOW standby
@ 01, 01 SAY 'Proveedor ->' GET  ;
  vcodprv PICTURE '!!!!' VALID  ;
  val_prv(vcodprv,.T.,2,27)
READ
DEACTIVATE WINDOW standby
IF LASTKEY() = 27
     RETURN
ENDIF
vdescri = promae.nompro
DEFINE WINDOW wind_6 FROM 06, 26  ;
       TO 19, 79 FLOAT TITLE ' ' +  ;
       ALLTRIM(vdescri) + ' '  ;
       FOOTER  ;
       ' [F10] SELECCIONAR '  ;
       DOUBLE COLOR SCHEME 02
ACTIVATE WINDOW wind_1
@ 1, 1 SAY ' Desde : ' GET  ;
  vfecini
@ 1, 18 SAY '    al : ' GET  ;
  vfecfin
READ
DEACTIVATE WINDOW wind_1
IF LASTKEY() = 27
     RETURN
ENDIF
DEFINE WINDOW xwait FROM 21, 50  ;
       TO 23, 75 COLOR SCHEME 05
ACTIVATE WINDOW xwait
@ 0, 0 SAY  ;
  ' Consulta en Proceso....'  ;
  COLOR W+/N* 
SELECT compag
SET ORDER TO Compag5
SEEK ALLTRIM(vcodprv)
IF FOUND()
     SELECT compag
     SCAN WHILE codprv =  ;
          ALLTRIM(vcodprv)
          IF BETWEEN(feccp,  ;
             vfecini, vfecfin)
               SELECT temp
               APPEND BLANK
               REPLACE nummes  ;
                       WITH  ;
                       compag.nummes
               REPLACE numcp WITH  ;
                       compag.numcp
               REPLACE codctc  ;
                       WITH  ;
                       compag.codctc
               REPLACE nummeshc  ;
                       WITH  ;
                       compag.nummeshc
               REPLACE numhc WITH  ;
                       compag.numhc
               REPLACE tipdoc  ;
                       WITH  ;
                       compag.tipdoc
               REPLACE feccp WITH  ;
                       compag.feccp
               REPLACE fecref  ;
                       WITH  ;
                       compag.fecref
               REPLACE glosa WITH  ;
                       compag.glosa
               REPLACE usuario  ;
                       WITH  ;
                       compag.usuario
               REPLACE import  ;
                       WITH  ;
                       compag.import
               REPLACE reten WITH  ;
                       compag.reten
          ENDIF
          SELECT compag
     ENDSCAN
ELSE
     RELEASE WINDOW xwait
     DO standby WITH  ;
        ' No hay cheques para dicho personal/proveedor/otro'
     RETURN
ENDIF
SELECT compag
SET ORDER TO Compag1
SELECT temp
vind1 = SYS(3) + '.IDX'
INDEX ON nummes + numcp + codctc  ;
      TO (vind1)
DEACTIVATE WINDOW xwait
ACTIVATE WINDOW wind_6
BROWSE FIELDS numcp, nummes,  ;
       codctc : 12, feccp, x1 =  ;
       import - reten : 15 :H =  ;
       'Liquido' NOMENU NOAPPEND  ;
       NOEDIT NODELETE IN wind_6
USE
DEACTIVATE WINDOW wind_6
RETURN
*
PROCEDURE seg_doc
PARAMETER vnummes, vnumcp,  ;
          vcodctc
HIDE WINDOW wind_6
IF LASTKEY() = 27 .OR. okcancel =  ;
   2
     DEACTIVATE WINDOW wind_2
     EXIT
ENDIF
SELECT temp
SEEK ALLTRIM(vnummes) + vnumcp +  ;
     ALLTRIM(vcodctc)
vnumhc = temp.numhc
vnummeshc = temp.nummeshc
vfeccp = temp.feccp
vregcp = temp.fecref
vimporte = temp.import
vreten = temp.reten
vliquido = vimporte - vreten
vtipo = temp.tipdoc
vregcp = temp.fecref
vconc1 = LEFT(temp.glosa, 37)
vconc2 = SUBSTR(temp.glosa, 37,  ;
         37)
vconc3 = SUBSTR(temp.glosa, 74,  ;
         37)
vconc4 = SUBSTR(temp.glosa, 111,  ;
         37)
vconc5 = SUBSTR(temp.glosa, 148,  ;
         37)
vconc6 = SUBSTR(temp.glosa, 185,  ;
         37)
vusucp = CHRTRAN(temp.usuario, 'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?',;
'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
SELECT cheque
SEEK ALLTRIM(vnummes) + vnumcp +  ;
     ALLTRIM(vcodctc)
IF FOUND()
     vnumchq = cheque.numchq
     vnombre = LEFT(cheque.nomgir,  ;
               30)
     vfecchq = cheque.fecchq
     vtiempo = vfecchq - vregcp +  ;
               1
     vsw[ 1] = 1
     SELECT hoja
     SEEK ALLTRIM(vnummeshc) +  ;
          vnumhc
     IF FOUND()
          vtipdoc = hoja.tipdoc
          vperhc = hoja.periodo
          vnumref = hoja.numref
          vfte = hoja.codfte
          vfechc = hoja.fechc
          vreghc = hoja.user_fc
          vusuhc = hoja.user
          vtiempo = vfecchq -  ;
                    vreghc + 1
          vsw[ 2] = 1
          DO CASE
               CASE ALLTRIM(vtipdoc) =  ;
                    'O/C'
                    SELECT orden
                    SEEK vperhc +  ;
                         vnumref +  ;
                         ALLTRIM(vfte)
                    IF FOUND()
                         vnumoc =  ;
                          orden.numoc
                         vnummesoc =  ;
                          SUBSTR(orden.codcal,  ;
                          3, 2)
                         vfecoc =  ;
                          orden.fecoc
                         vregoc =  ;
                          orden.user_fc
                         vtiempo =  ;
                          vfecchq -  ;
                          vregoc +  ;
                          1
                         vusuoc =  ;
                          orden.user
                         vsw[ 3] =  ;
                            1
                         SELECT itepec
                         SEEK vnumoc +  ;
                              ALLTRIM(vnummesoc)
                         IF FOUND()
                              vnumpe =  ;
                               itepec.numpec
                              vnummespe =  ;
                               SUBSTR(itepec.codcal,  ;
                               3,  ;
                               2)
                              vperpe =  ;
                               itepec.periodo
                              vsw[  ;
                                 4] =  ;
                                 1
                              SELECT  ;
                               pecosa
                              SEEK  ;
                               vperpe +  ;
                               vnumpe +  ;
                               ALLTRIM(vfte)
                              IF FOUND()
                                   vfecpe = pecosa.fecemi
                                   vregpe = pecosa.user_fc
                                   vusupe = pecosa.user
                                   vtiempo = vfecchq - vregpe + 1
                              ENDIF
                         ENDIF
                    ENDIF
               CASE ALLTRIM(vtipdoc) =  ;
                    'O/S'
                    SELECT ordser
                    SEEK vperhc +  ;
                         vnumref +  ;
                         ALLTRIM(vfte)
                    IF FOUND()
                         vnumos =  ;
                          ordser.numos
                         vnummesos =  ;
                          SUBSTR(DTOC(ordser.fecos),  ;
                          4, 2)
                         vfecos =  ;
                          ordser.fecos
                         vregos =  ;
                          ordser.user_fc
                         vusuos =  ;
                          ordser.user
                         vtiempo =  ;
                          vfecchq -  ;
                          vregos +  ;
                          1
                         vfecliq =  ;
                          ordser.fecliq
                         vsw[ 5] =  ;
                            1
                         SELECT solser
                         SEEK vnumos +  ;
                              ALLTRIM(vnummesos)
                         IF FOUND()
                              vnumss =  ;
                               solser.numss
                              vnummesss =  ;
                               SUBSTR(DTOC(solser.fecss),  ;
                               4,  ;
                               2)
                              vfecss =  ;
                               solser.fecss
                              vregss =  ;
                               solser.user_fc
                              vususs =  ;
                               solser.user
                              vtiempo =  ;
                               vfecchq -  ;
                               vregss +  ;
                               1
                              vsw[  ;
                                 6] =  ;
                                 1
                         ENDIF
                    ENDIF
          ENDCASE
     ENDIF
ENDIF
DEACTIVATE WINDOW wind_8
ACTIVATE WINDOW wind_2
@ 0, 08 SAY 'Doc. Fte'
@ 0, 37 SAY 'Fec.Doc.'
@ 0, 50 SAY 'Fec.Ope.'
@ 0, 63 SAY 'Usuario'
@ 1, 08 SAY REPLICATE('=', 8)
@ 1, 37 SAY REPLICATE('=', 8)
@ 1, 50 SAY REPLICATE('=', 8)
@ 1, 63 SAY REPLICATE('=', 6)
DO CASE
     CASE ALLTRIM(vtipdoc) =  ;
          'O/S'
          @ 3, 01 SAY  ;
            'N§ S/S      : ' +  ;
            ALLTRIM(vnummesss) +  ;
            '.' + vnumss
          @ 3, 38 SAY IIF(vsw(5) =  ;
            1, vfecss, ' ')
          @ 3, 51 SAY IIF(vsw(5) =  ;
            1, vregss, ' ')
          @ 3, 64 SAY IIF(vsw(5) =  ;
            1, vususs, ' ')
          @ 4, 04 SAY '' COLOR  ;
            RGB(255,0,0,255,255, ;
            225)
          @ 5, 01 SAY  ;
            'N§ O/S      : ' +  ;
            ALLTRIM(vnummesos) +  ;
            '.' + vnumos + '.' +  ;
            vfte
          @ 5, 38 SAY IIF(vsw(6) =  ;
            1, vfecos, ' ')
          @ 5, 51 SAY IIF(vsw(6) =  ;
            1, vregos, ' ')
          @ 5, 64 SAY IIF(vsw(6) =  ;
            1, vusuos, ' ')
          @ 6, 04 SAY '' COLOR  ;
            RGB(255,0,0,255,255, ;
            225)
     CASE ALLTRIM(vtipdoc) =  ;
          'O/C'
          @ 3, 01 SAY  ;
            'N§ PECOSA   : ' +  ;
            ALLTRIM(vnummespe) +  ;
            '.' + vnumpe
          @ 3, 38 SAY IIF(vsw(3) =  ;
            1, vfecpe, ' ')
          @ 3, 51 SAY IIF(vsw(3) =  ;
            1, vregpe, ' ')
          @ 3, 64 SAY IIF(vsw(3) =  ;
            1, vusupe, ' ')
          @ 4, 04 SAY '' COLOR  ;
            RGB(255,0,0,255,255, ;
            225)
          @ 5, 01 SAY  ;
            'N§ O/C      : ' +  ;
            ALLTRIM(vnummesoc) +  ;
            '.' + vnumoc + '.' +  ;
            vfte
          @ 5, 38 SAY IIF(vsw(4) =  ;
            1, vfecoc, ' ')
          @ 5, 51 SAY IIF(vsw(4) =  ;
            1, vregoc, ' ')
          @ 5, 64 SAY IIF(vsw(4) =  ;
            1, vusuoc, ' ')
          @ 6, 04 SAY '' COLOR  ;
            RGB(255,0,0,255,255, ;
            225)
ENDCASE
IF vsw(2) = 1
     @ 7, 01 SAY 'N§ H/C      : ' +  ;
       ALLTRIM(vnummeshc) + '.' +  ;
       vnumhc + '  ' + IIF(vsw(3) =  ;
       1 .OR. vsw(5) = 1, ' ',  ;
       ALLTRIM(vtipdoc))
     @ 7, 38 SAY vfechc
     @ 7, 51 SAY vreghc
     @ 7, 64 SAY vusuhc
     @ 8, 04 SAY '' COLOR  ;
       RGB(255,0,0,255,255,225)
ENDIF
@ 9, 01 SAY 'N§ C/P      : ' +  ;
  ALLTRIM(vnummes) + '.' + vnumcp +  ;
  '.' + ALLTRIM(vcodctc)
@ 9, 38 SAY vfeccp PICTURE '@D'
@ 9, 51 SAY vregcp
@ 9, 64 SAY vusucp
@ 10, 04 SAY '' COLOR RGB(255,0, ;
  0,255,255,225)
@ 11, 01 SAY 'N§ Cheque   : ' +  ;
  ALLTRIM(vnumchq)
@ 11, 38 SAY IIF(vsw(1) = 1,  ;
  vfecchq, ' ')
@ 11, 51 SAY vregcp
@ 11, 64 SAY vusucp
@ 12, 04 SAY '' COLOR RGB(255,0, ;
  0,255,255,225)
@ 13, 01 SAY 'Girado a    : ' +  ;
  vnombre
@ 13, 46 SAY 'Monto Bruto :'
@ 13, 62 SAY vimporte PICTURE  ;
  '@Z 9,999,999.99'
@ 14, 02 TO 21, 41
@ 15, 03 SAY vconc1
@ 15, 46 SAY 'Retenci¢n   :'
@ 15, 62 SAY vreten PICTURE  ;
  '9,999,999.99'
@ 16, 03 SAY vconc2
@ 17, 03 SAY vconc3
@ 17, 46 SAY 'Liquido     :'
@ 17, 62 SAY vliquido PICTURE  ;
  '@Z 9,999,999.99' COLOR RGB(1,0, ;
  0,200,111,115)
@ 18, 03 SAY vconc4
@ 19, 03 SAY vconc5
@ 20, 03 SAY vconc6
@ 19, 46 SAY 'Duraci¢n    :'
@ 19, 65 SAY vtiempo PICTURE  ;
  '999' COLOR RGB(1,0,0,200,111, ;
  115)
@ 19, 68 SAY IIF(vtiempo = 1,  ;
  '  D¡a', '  D¡as') COLOR RGB(1, ;
  0,0,200,111,115)
@ 21, 62 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK'
READ
CLEAR
DEACTIVATE WINDOW wind_2
RETURN
*
FUNCTION salir
PARAMETER vsal
IF LASTKEY() = -9
     vsal = .T.
ENDIF
RETURN .T.
*
FUNCTION exi
IF LASTKEY() = 27 .OR. okcancel =  ;
   1 .OR. okcancel = 2
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION disppro
PARAMETER xtipprv, xcodotr,  ;
          xtipdoc
PRIVATE xnompre
DO CASE
     CASE xtipprv = 'P'
          xnompre = val_auxi(ALLTRIM(compag.codprv), ;
                    '20','V')
     CASE xtipdoc $ 'RESR'
          xnompre = val_para(compag.codret, ;
                    'CODRET','V', ;
                    22,40)
     OTHERWISE
          xnompre = compag.nompre
ENDCASE
RETURN xnompre
*
PROCEDURE busca
SET EXACT OFF
ACTIVATE WINDOW wind_4
vnom = SPACE(20)
@ 1, 1 SAY ' Nombre : ' GET vnom
READ
DEACTIVATE WINDOW wind_4
vreg = RECNO()
SEEK ALLTRIM(vnom)
IF  .NOT. FOUND()
     GOTO vreg
ENDIF
SET EXACT ON
RETURN
*

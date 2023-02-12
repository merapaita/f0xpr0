*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' DEVOLUCIONES - N/CAMBIO Y CREDITO '
wrk_dia = DATE()
SELECT 1
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 2
USE SHARED ST_ISREP ORDER CODIGO
SELECT 3
USE SHARED ST_ICLPR ORDER CODIGO
SELECT 4
USE ST_CAMBI ORDER CODIGO
SELECT 2
STORE DATE() TO w_fecini,  ;
      w_fecfin
wrk_copia = 1
STORE SPACE(4) TO w_movini,  ;
      w_movfin, w_tdaini,  ;
      w_tdafin, w_motini,  ;
      w_motfin
w_output = 'Impresora'
STORE 'Detalle' TO w_tipo
STORE 'Todos     ' TO w_resum
DO WHILE .T.
     @ 7, 1 CLEAR TO 13, 77
     @ 4, 30 SAY SPACE(30)
     @ 3, 0 TO 11, 77
     @ 4, 5 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 02 SAY 'Del       :'
     @ 04, 40 SAY 'Al       :'
     @ 05, 02 SAY 'Del Movim.:'
     @ 05, 40 SAY 'Al Movim.:'
     @ 06, 02 SAY 'De Tienda :'
     @ 06, 40 SAY 'A Tienda :'
     @ 07, 02 SAY 'Del Motivo:'
     @ 07, 40 SAY 'Al Motivo:'
     @ 08, 02 SAY  ;
       'Por Todos/Pendientes  :'
     @ 09, 02 SAY  ;
       'Por Detalle/Res£men   :'
     @ 10, 02 SAY  ;
       'Por Pantalla/Impresora:'
     SET CURSOR ON
     @ 04, 14 GET w_fecini WHEN  ;
       antes(VARREAD())
     @ 04, 51 GET w_fecfin RANGE  ;
       w_fecini
     @ 05, 14 GET w_movini  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 05, 51 GET w_movfin  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 06, 14 GET w_tdaini  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 06, 51 GET w_tdafin  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 07, 14 GET w_motini  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 07, 51 GET w_motfin  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 08, 26 GET w_resum PICTURE  ;
       '@m Todos,Pendientes'
     @ 09, 26 GET w_tipo PICTURE  ;
       '@m Detalle,Res£men'
     @ 10, 26 GET w_output  ;
       PICTURE  ;
       '@m Pantalla,Impresora'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT 10
     CREATE DBF cambios (mtipdoc  ;
            C (04), mnrodoc C  ;
            (10), mfecdoc D,  ;
            mfecrep D, mtienda C  ;
            (04), mmotivo C (04),  ;
            mnroaut C (10),  ;
            mtipref C (4),  ;
            mdocref C (10),  ;
            mcodart C (06),  ;
            marticulo C (04),  ;
            mmarca C (04),  ;
            mmodelo C (15),  ;
            mserie C (15),  ;
            mcliente C (9),  ;
            mnumsol C (8),  ;
            mnumord C (8),  ;
            mobserv C (60),  ;
            mdocsal C (4),  ;
            mnrosal C (10),  ;
            mfecsal D)
     INDEX ON mtipdoc + mtienda +  ;
           DTOC(mfecdoc) TAG  ;
           codigo
     SET ORDER TO CODIGO
     SET NEAR ON
     SELECT st_cambi
     SET ORDER TO FECHA
     SEEK w_fecini
     SET NEAR OFF
     SCAN WHILE fecdoc <=  ;
          w_fecfin
          w_tipdoc = tipdoc
          w_nrodoc = nrodoc
          w_fecdoc = fecdoc
          w_tienda = tienda
          w_motivo = motivo
          w_nroaut = nroaut
          w_tipref = tipref
          w_docref = docref
          w_codart = codart
          w_numsol = numsol
          w_numord = numord
          w_docsal = docsal
          w_nrosal = nrosal
          w_fecsal = fecsal
          w_observ = observ
          SELECT st_isrep
          SEEK w_numsol
          IF FOUND()
               w_marca = codmar
               w_modelo = codmod
               w_serie = numser
               w_codcli = codent
          ENDIF
          SELECT cambios
          APPEND BLANK
          REPLACE mtipdoc WITH  ;
                  w_tipdoc,  ;
                  mnrodoc WITH  ;
                  w_nrodoc,  ;
                  mfecdoc WITH  ;
                  w_fecdoc
          REPLACE mtienda WITH  ;
                  w_tienda,  ;
                  mmotivo WITH  ;
                  w_motivo,  ;
                  mnroaut WITH  ;
                  w_nroaut
          REPLACE mtipref WITH  ;
                  w_tipref,  ;
                  mdocref WITH  ;
                  w_docref,  ;
                  mcodart WITH  ;
                  w_codart
          REPLACE mnumsol WITH  ;
                  w_numsol,  ;
                  mnumord WITH  ;
                  w_numord,  ;
                  mobserv WITH  ;
                  w_observ
          REPLACE mmarca WITH  ;
                  w_marca,  ;
                  mmodelo WITH  ;
                  w_modelo,  ;
                  mserie WITH  ;
                  w_serie
          REPLACE mcliente WITH  ;
                  w_codcli,  ;
                  mdocsal WITH  ;
                  w_docsal,  ;
                  mnrosal WITH  ;
                  w_nrosal
          REPLACE mfecsal WITH  ;
                  w_fecsal
          SELECT st_cambi
     ENDSCAN
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     SELECT cambios
     COUNT TO wrk_valor
     IF wrk_valor = 0
          DO error WITH  ;
             'No Existen registros a Listar'
          LOOP
     ENDIF
     IF w_output = 'Impresora'
          @ 10, 50 SAY 'Copias:'
          @ 10, 58 GET wrk_copia  ;
            PICTURE '99'
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     IF w_output = 'Impresora'
          ??? CHR(15)
          FOR a = 1 TO wrk_copia
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               IF w_tipo =  ;
                  'Detalle'
                    IF w_resum =  ;
                       'Pendientes'
                         REPORT FORMAT  ;
                                porl4013  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE  ;
                                FOR  ;
                                mfecsal =  ;
                                {}
                    ELSE
                         REPORT FORMAT  ;
                                porl4013  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ELSE
                    IF w_resum =  ;
                       'Pendientes'
                         REPORT FORMAT  ;
                                porl4013  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE  ;
                                FOR  ;
                                mfecsal =  ;
                                {}
                    ELSE
                         REPORT FORMAT  ;
                                porl4013  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDIF
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDFOR
          SET PRINTER TO
     ELSE
          IF w_tipo = 'Detalle'
               IF w_resum =  ;
                  'Pendientes'
                    REPORT FORMAT  ;
                           porl4013  ;
                           TO  ;
                           FILE  ;
                           TEXT5.TXT  ;
                           NOCONSOLE  ;
                           FOR  ;
                           mfecsal =  ;
                           {}
               ELSE
                    REPORT FORMAT  ;
                           porl4013  ;
                           TO  ;
                           FILE  ;
                           TEXT5.TXT  ;
                           NOCONSOLE
               ENDIF
          ELSE
               IF w_resum =  ;
                  'Pendientes'
                    REPORT FORMAT  ;
                           porl4013  ;
                           TO  ;
                           FILE  ;
                           TEXT5.TXT  ;
                           NOCONSOLE  ;
                           FOR  ;
                           mfecsal =  ;
                           {}
               ELSE
                    REPORT FORMAT  ;
                           porl4013  ;
                           SUMMARY  ;
                           TO  ;
                           FILE  ;
                           TEXT5.TXT  ;
                           NOCONSOLE
               ENDIF
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODIFY COMMAND  ;
                 text5.txt NOEDIT  ;
                 WINDOW pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
RETURN
*
FUNCTION valida
PARAMETER w_codtab
DO CASE
     CASE w_codtab = 'W_MOVINI'  ;
          .OR. w_codtab =  ;
          'W_MOVFIN'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          IF w_codtab =  ;
             'W_MOVINI'
               SEEK 'DOCU' +  ;
                    w_movini
          ELSE
               SEEK 'DOCU' +  ;
                    w_movfin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Documento No Existe ***'
               RETURN .F.
          ENDIF
          IF w_codtab =  ;
             'W_MOVINI'
               @ 05, 19 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 17)
          ELSE
               @ 05, 56 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 17)
          ENDIF
     CASE w_codtab = 'W_TDAINI'  ;
          .OR. w_codtab =  ;
          'W_TDAFIN'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          IF w_codtab =  ;
             'W_TDAINI'
               SEEK 'TDAS' +  ;
                    w_tdaini
          ELSE
               SEEK 'TDAS' +  ;
                    w_tdafin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Documento No Existe ***'
               RETURN .F.
          ENDIF
          IF w_codtab =  ;
             'W_TDAINI'
               @ 06, 19 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 17)
          ELSE
               @ 06, 56 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 17)
          ENDIF
     CASE w_codtab = 'W_MOTINI'  ;
          .OR. w_codtab =  ;
          'W_MOTFIN'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          IF w_codtab =  ;
             'W_MOTINI'
               SEEK 'MCAM' +  ;
                    w_motini
          ELSE
               SEEK 'MCAM' +  ;
                    w_motfin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Documento No Existe ***'
               RETURN .F.
          ENDIF
          IF w_codtab =  ;
             'W_MOTINI'
               @ 07, 19 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 17)
          ELSE
               @ 07, 56 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 17)
          ENDIF
ENDCASE
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT ge_tab0
DO CASE
     CASE opc = 1
          SET FILTER TO tab_codpre ==;
'DOCU'
     CASE opc = 2
          SET FILTER TO tab_codpre ==;
'TDAS'
     CASE opc = 3
          SET FILTER TO tab_codpre ==;
'MCAM'
ENDCASE
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE TABLAS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'ESC'
RETURN
*
PROCEDURE antes
PARAMETER w_opc
DO CASE
     CASE w_opc = 'W_MOVINI' .OR.  ;
          w_opc = 'W_MOVFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA WITH;
1
     CASE w_opc = 'W_TDAINI' .OR.  ;
          w_opc = 'W_TDAFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA WITH;
2
     CASE w_opc = 'W_MOTINI' .OR.  ;
          w_opc = 'W_MOTFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA WITH;
3
     CASE w_opc = 'W_TIPO'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
tit_prg = ' REPORTE '
wrk_progra = PROGRAM()
DO crea_win
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' PLANILLAS X FACTURAR LG '
CLOSE DATABASES
SELECT 1
USE ST_IOREP ORDER CODIGO
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE ST_PLANI ORDER FECHA
SELECT 4
USE ST_ICLPR ORDER CODIGO
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE 1 TO w_opcion, w_salida,  ;
      w_copia
DO WHILE .T.
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 07, 01 CLEAR TO 15, 77
     @ 04, 00 TO 10, 77
     @ 05, 01 SAY 'Per¡odo :'  ;
       SIZE 01, 10, 0
     @ 05, 12 SAY 'Del' SIZE 01,  ;
       10, 0
     @ 06, 12 SAY ' Al' SIZE 01,  ;
       10, 0
     @ 08, 01 SAY 'Estado   :'  ;
       SIZE 01, 10, 0
     @ 08, 30 SAY 'Destino  :'  ;
       SIZE 01, 10, 0
     @ 05, 16 GET w_fecini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD())
     @ 06, 16 GET w_fecfin RANGE  ;
       w_fecini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 08, 12 GET w_tipo DEFAULT  ;
       1 SIZE 1, 10, 0 PICTURE  ;
       '@*RVN X Facturar;Total'  ;
       WHEN oowhen(VARREAD())
     @ 08, 41 GET w_salida  ;
       DEFAULT 1 SIZE 1, 10, 0  ;
       PICTURE  ;
       '@*RVTN Pantalla;Impresora'  ;
       VALID oovalid(VARREAD())
     READ CYCLE
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF w_salida = 2
          @ 09, 55 SAY  ;
            'Copias  :' GET  ;
            w_copia RANGE 1,10  ;
            PICTURE '99' VALID   ;
            .NOT. EMPTY(w_copia)
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     CREATE CURSOR file (morden C  ;
            (8), mnumpla C (10),  ;
            mfecpla D, mfecord D,  ;
            mcliente C (20),  ;
            mmodelo C (15),  ;
            mserie C (15), mrptos  ;
            N (8, 2), mmobra N (8,  ;
            2), mflete N (8, 2),  ;
            mtotal N (8, 2),  ;
            mestado N (1),  ;
            mtipdoc C (4),  ;
            mnrodoc C (10))
     INDEX ON mnumpla +  ;
           DTOS(mfecpla) TAG  ;
           codigo
     SELECT st_plani
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     SCAN WHILE fecpla <=  ;
          w_fecfin .AND.  .NOT.  ;
          EOF()
          w_orden = orden
          w_nropla = nropla
          w_fecpla = fecpla
          w_estado = estado
          w_tipdoc = tipdoc
          w_nrodoc = nrodoc
          w_fecdoc = fecdoc
          SELECT st_iorep
          SEEK w_orden
          IF FOUND()
               w_fecord = fecemi
               w_codcli = codent
               w_modelo = codmod
               w_serie = numser
               w_totrep = cosrep
               w_totmob = cosmob
               w_totfle = flete
               w_total = subtot
               w_cliente = ' '
               SELECT st_iclpr
               SEEK 'C' +  ;
                    w_codcli
               IF FOUND()
                    w_cliente = noment
               ENDIF
          ENDIF
          SELECT file
          APPEND BLANK
          REPLACE morden WITH  ;
                  w_orden
          REPLACE mnumpla WITH  ;
                  w_nropla
          REPLACE mfecpla WITH  ;
                  w_fecpla
          REPLACE mtipdoc WITH  ;
                  w_tipdoc
          REPLACE mnrodoc WITH  ;
                  w_nrodoc
          REPLACE mfecord WITH  ;
                  w_fecord
          REPLACE mcliente WITH  ;
                  w_cliente
          REPLACE mmodelo WITH  ;
                  w_modelo
          REPLACE mserie WITH  ;
                  w_serie
          REPLACE mrptos WITH  ;
                  w_totrep
          REPLACE mmobra WITH  ;
                  w_totmob
          REPLACE mflete WITH  ;
                  w_totfle
          REPLACE mtotal WITH  ;
                  w_total
          REPLACE mestado WITH  ;
                  w_estado
          SELECT st_plani
     ENDSCAN
     SELECT file
     COUNT TO w_valor
     IF w_valor = 0
          DO error WITH  ;
             '***  No Existen registros a Listar  ***'
          LOOP
     ENDIF
     IF w_salida = 2
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          SET DEVICE TO PRINTER
          SET PRINTER ON
          ?? CHR(27) + CHR(15)
          FOR a = 1 TO w_copia
               IF w_tipo = 1
                    REPORT FORMAT  ;
                           PORL4014  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE  ;
                           FOR  ;
                           mnrodoc =  ;
                           SPACE(10)
               ELSE
                    REPORT FORMAT  ;
                           PORL4014  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
          ENDFOR
          ?? CHR(27) + CHR(18)
          SET PRINTER OFF
          SET DEVICE TO SCREEN
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'COLO'
          w_fildoc = SUBSTR(f_archivo(),  ;
                     1, 8) +  ;
                     '.DOC'
          IF w_tipo = 1
               REPO FORM PORL4014 TO FILE;
 &w_fildoc  FOR mnrodoc = SPACE(10) NOCONSOLE
          ELSE
               REPO FORM PORL4014 TO FILE;
 &w_fildoc  NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODI COMM &w_fildoc NOEDIT WIND;
PANTALL
          SET SYSMENU OFF
          DELE FILE &w_fildoc
     ENDIF
ENDDO
CLOSE DATABASES
DO sacawin
RETURN
*
PROCEDURE oowhen
PARAMETER opc
DO CASE
     CASE opc = 'W_FECFIN' .OR.  ;
          opc = 'W_TIPO'
          ON KEY
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER opc
DO CASE
     CASE opc = 'W_FECINI'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(w_fecini)
               DO error WITH  ;
                  '***  No se permiten Blancos  ***'
               RETURN .F.
          ENDIF
     CASE opc = 'W_FECFIN'
          IF EMPTY(w_fecfin)
               DO error WITH  ;
                  '***  No se permiten Blancos  ***'
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

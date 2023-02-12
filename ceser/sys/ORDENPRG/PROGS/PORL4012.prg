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
   ' SALIDAS POR CORTESIA '
CLOSE DATABASES
SELECT 1
USE SHARED ST_IOREP ORDER  ;
    ORD_ENTIND
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 3
USE SHARED GC_PRO00 ORDER CODIGO
SELECT 4
USE SHARED GC_HVE00 ORDER CODIGO
SELECT 5
USE SHARED GC_DVE00 ORDER CODIGO
SET RELATION TO dve_propar INTO gc_pro00
SELECT 6
USE SHARED ST_ICLPR ORDER codigo
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE 1 TO w_opcion, w_salida,  ;
      w_copia
STORE 'P' TO w_proini, w_profin
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
     @ 08, 01 SAY 'Tipo    :'  ;
       SIZE 01, 10, 0
     @ 08, 30 SAY 'Destino  :'  ;
       SIZE 01, 10, 0
     @ 05, 30 SAY 'Propiedad:'
     @ 05, 41 SAY 'Del'
     @ 06, 42 SAY 'Al '
     @ 05, 16 GET w_fecini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD())
     @ 06, 16 GET w_fecfin RANGE  ;
       w_fecini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 05, 45 GET w_proini SIZE 1,  ;
       4, 0 FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 06, 45 GET w_profin SIZE 1,  ;
       4, 0 RANGE w_proini  ;
       FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 08, 12 GET w_tipo DEFAULT  ;
       2 SIZE 1, 10, 0 PICTURE  ;
       '@*RVN Detalle;Resumen'  ;
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
     CREATE CURSOR file2 (orden C  ;
            (8), tipgar C (4),  ;
            fecent D, cliente C  ;
            (9), codpro C (14),  ;
            cantid N (8), preuni  ;
            N (8, 2), totite N (8,  ;
            2), mobra N (8, 2),  ;
            cpsol N (8, 2), cpdol  ;
            N (8, 2))
     INDEX ON DTOS(fecent) +  ;
           orden TAG codigo2
     SELECT st_iorep
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     SCAN WHILE fecent <=  ;
          w_fecfin .AND.  .NOT.  ;
          EOF()
          IF auxest = '025 '  ;
             .AND. indest <> 'N'
               w_numdoc = f_ceros(numdoc, ;
                          10,2)
               SELECT gc_hve00
               SEEK 'ORDE' +  ;
                    w_numdoc
               IF FOUND()
                    SELECT gc_dve00
                    SEEK 'ORDE' +  ;
                         w_numdoc
                    IF FOUND()
                         SCAN WHILE  ;
                              dve_tipdoc =  ;
                              'ORDE'  ;
                              .AND.  ;
                              dve_nrodoc =  ;
                              w_numdoc  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              IF gc_pro00.pro_propie >=  ;
                                 SUBSTR(w_proini,  ;
                                 1,  ;
                                 1)  ;
                                 .AND.  ;
                                 gc_pro00.pro_propie <=  ;
                                 SUBSTR(w_profin,  ;
                                 1,  ;
                                 1)
                                   DO graba
                                   REPLACE codpro WITH gc_dve00.dve_propar
                                   REPLACE cantid WITH gc_dve00.dve_cantid
                                   REPLACE preuni WITH gc_dve00.dve_import
                                   REPLACE totite WITH gc_dve00.dve_total
                                   REPLACE cpsol WITH gc_dve00.dve_coprmb
                                   REPLACE cpdol WITH gc_dve00.dve_coprmo
                                   SELECT gc_dve00
                              ENDIF
                         ENDSCAN
                    ENDIF
               ELSE
                    DO graba
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SELECT file2
     COUNT TO w_valor
     IF w_valor = 0
          DO error WITH  ;
             '***  No Existen registros a Listar  ***'
          LOOP
     ELSE
          SET RELATION TO codpro INTO;
gc_pro00, 'C' + cliente INTO st_iclpr
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
                           PORL4012  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           PORL412A  ;
                           SUMMARY  ;
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
               REPO FORM PORL4012 TO FILE;
&w_fildoc  NOCONSOLE
          ELSE
               REPO FORM PORL412A SUMMARY;
TO FILE  &w_fildoc  NOCONSOLE
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
PROCEDURE graba
SELECT file2
APPEND BLANK
REPLACE orden WITH  ;
        st_iorep.numdoc
REPLACE tipgar WITH  ;
        st_iorep.indori
REPLACE fecent WITH  ;
        st_iorep.fecent
REPLACE cliente WITH  ;
        st_iorep.codent
REPLACE mobra WITH  ;
        st_iorep.cosmob
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
     CASE opc = 'W_PROINI'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_PROFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda10
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
     CASE opc = 'W_PROINI' .OR.  ;
          opc = 'W_PROFIN'
          SELECT ge_tab0
          IF opc = 'W_PROINI'
               SEEK 'PROP' +  ;
                    w_proini
          ELSE
               SEEK 'PROP' +  ;
                    w_profin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '***  C¢digo no Existe  ***'
               RETURN .F.
          ENDIF
          IF opc = 'W_PROFIN'
               IF w_proini >  ;
                  w_profin
                    DO error WITH  ;
                       '***  C¢digo Menor al Inicial  ***'
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 50 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
ENDCASE
RETURN
*
PROCEDURE ayuda10
DO CASE
     CASE VARREAD() = 'W_PROINI'  ;
          .OR. VARREAD() =  ;
          'W_PROFIN'
          SELECT ge_tab0
          SET ORDER TO codigo
          SET FILTER TO tab_codpre = 'PROP'
          campo = 'tab_codtab+"  "+tab_destab'
          titulo = 'AYUDA DE PROPIEDAD'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
          SET ORDER TO codigo
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE POPUP menux01 FROM 17, 40  ;
       COLOR SCHEME 21
DEFINE BAR 1 OF menux01 PROMPT  ;
       '\<Solicitudes Facturadas'
DEFINE BAR 2 OF menux01 PROMPT  ;
       'Solicitudes \<Facturadas por Marca'
ON SELECTION POPUP menux01 DO INDICA WITH;
BAR()
ACTIVATE POPUP menux01
DEACTIVATE POPUP menux01
IF LASTKEY() = 27
     RETURN
ENDIF
RETURN
*
PROCEDURE indica
PARAMETER opc
IF opc = 2
     DO porl4100
     RETURN
ENDIF
ON KEY
tit_prg = ' REPORTE '
wrk_progra = PROGRAM()
DO crea_win
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' SOLICITUDES FACTURADAS '
CLOSE DATABASES
w_progra = 'PORL4098'
SELECT 1
USE ST_IOREP ORDER ORD_FECFAC
SELECT 2
USE ST_ISREP ORDER CODIGO
SELECT 3
USE GE_TAB0 ORDER CODIGO
SELECT 4
USE ST_IMODE ORDER CODIGO
SELECT 5
USE ST_ISERI ORDER SER_CODMAR
SELECT 6
USE ST_MOVSO ORDER NUMSOL
w_file = SUBSTR(da_nombre(), 1,  ;
         8) + '.DBF'
SELECT 7
CREATE TABLE &w_file (solic  C(08),;
 orden  C(08),   indori C(04),;
  fecemi D, marca  C(14),  modelo C(15),;
  serie  C(20),   docvta C(15), soluc;
 C(04),  cosmob N(10,2), cosrep N(10,2),;
flete  N(10,2), linea  C(04),  emisor;
C(04),   fecfac D,       fecent D, fecfin;
d,codtall c(4),codent c(9), horent C(08))
STORE 1 TO w_tipo, w_salida,  ;
      w_copia, w_orden
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE CTOD('  /  /  ') TO  ;
      w_fecemi, w_fecent
STORE SPACE(04) TO w_linini,  ;
      w_linfin, w_garini,  ;
      w_garfin, w_emiini,  ;
      w_emifin, w_codmar,  ;
      w_indori, w_codemi,  ;
      w_codlin, w_codsol
DO WHILE .T.
     STORE 0 TO w_cosmob,  ;
           w_cosrep, w_flete
     STORE SPACE(08) TO w_numsol,  ;
           w_numord, w_horent
     STORE SPACE(15) TO w_docvta,  ;
           w_codmod
     STORE SPACE(20) TO w_numser
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 07, 01 CLEAR TO 14, 77
     @ 03, 00 TO 03, 77
     @ 04, 01 SAY 'Per¡odo     :'
     @ 05, 09 SAY 'Del'
     @ 06, 09 SAY ' Al'
     @ 04, 40 SAY 'L¡nea   :'
     @ 05, 45 SAY 'Del'
     @ 06, 45 SAY ' Al'
     @ 07, 01 SAY 'T. Atenci¢n :'
     @ 08, 09 SAY 'Del'
     @ 09, 09 SAY ' Al'
     @ 07, 40 SAY 'Emisor  :'
     @ 08, 45 SAY 'Del'
     @ 09, 45 SAY ' Al'
     @ 10, 01 SAY 'Tipo Informe:'
     @ 10, 40 SAY 'Orden   :'
     @ 14, 01 SAY  ;
       'Tipo de Salida:'
     SET CURSOR ON
     @ 05, 13 GET w_fecini  ;
       PICTURE '@D' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 06, 13 GET w_fecfin RANGE  ;
       w_fecini PICTURE '@D'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 05, 49 GET w_linini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 06, 49 GET w_linfin RANGE  ;
       w_linini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 08, 13 GET w_garini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 09, 13 GET w_garfin RANGE  ;
       w_garini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 08, 49 GET w_emiini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 09, 49 GET w_emifin RANGE  ;
       w_emiini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 11, 13 GET w_tipo DEFAULT  ;
       1 PICTURE  ;
       '@*RVN Detalle;Resumen'  ;
       WHEN oowhen(VARREAD())
     @ 11, 49 GET w_orden DEFAULT  ;
       1 PICTURE  ;
       '@*RVN Emisor+L¡nea;L¡nea+Modelo'
     IF nivell = 'A4' .OR. nivell =  ;
        'A7'
          @ 13, 01 SAY  ;
            'Exportar datos:' GET  ;
            w_disket DEFAULT 1  ;
            PICTURE  ;
            '@*RHN No     ;A     ;B     ;C      '
     ENDIF
     @ 14, 17 GET w_salida  ;
       DEFAULT 1 PICTURE  ;
       '@*RHTN Pantalla;Impresora'
     READ CYCLE
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     IF w_salida = 2
          @ 14, 45 SAY  ;
            'Copias  :' GET  ;
            w_copia RANGE 1,10  ;
            PICTURE '99'
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT 7
     IF w_orden = 1
          INDEX ON emisor +  ;
                indori + linea +  ;
                modelo + soluc +  ;
                solic TAG  ;
                codigo2
     ELSE
          INDEX ON linea + modelo +  ;
                soluc + solic TAG  ;
                codigo2
     ENDIF
     SET ORDER TO codigo2
     SELECT st_iorep
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     SCAN WHILE fecfabo <=  ;
          w_fecfin .AND.  .NOT.  ;
          EOF()
          IF indest = 'F   ' .OR.  ;
             indest = 'B   '
               IF indori >=  ;
                  w_garini .AND.  ;
                  indori <=  ;
                  w_garfin
                    IF codemi >=  ;
                       w_emiini  ;
                       .AND.  ;
                       codemi <=  ;
                       w_emifin
                         w_codmar =  ;
                          codmar
                         w_codmod =  ;
                          codmod
                         SELECT st_imode
                         SEEK w_codmar +  ;
                              w_codmod
                         IF FOUND()
                              w_codlin =  ;
                               linea
                         ELSE
                              w_codlin =  ;
                               SPACE(4)
                         ENDIF
                         IF w_codlin >=  ;
                            w_linini  ;
                            .AND.  ;
                            w_codlin <=  ;
                            w_linfin
                              SELECT  ;
                               st_iorep
                              w_numord =  ;
                               numdoc
                              w_numsol =  ;
                               numsol
                              w_indori =  ;
                               indori
                              w_fecfac =  ;
                               fecfabo
                              w_numser =  ;
                               numser
                              w_codemi =  ;
                               codemi
                              w_cosmob =  ;
                               cosmob
                              w_cosrep =  ;
                               cosrep
                              w_flete =  ;
                               flete
                              w_fecent =  ;
                               fecent
                              w_horent =  ;
                               horent
                              SELECT  ;
                               st_isrep
                              SEEK  ;
                               w_numsol
                              IF FOUND()
                                   w_fecemi = fecemi
                              ENDIF
                              SELECT  ;
                               st_movso
                              SEEK  ;
                               w_numsol
                              IF FOUND()
                                   w_codsol = codsol
                              ELSE
                                   w_codsol = SPACE(4)
                              ENDIF
                              w_docvta =  ;
                               SPACE(15)
                              IF w_indori =  ;
                                 'GARA'  ;
                                 .OR.  ;
                                 w_indori =  ;
                                 'GREC'
                                   SELECT st_iseri
                                   SEEK w_codmar + w_codmod + w_numser
                                   IF FOUND()
                                        w_docvta = docgar
                                   ENDIF
                              ENDIF
                              SELECT  ;
                               7
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               solic  ;
                               WITH  ;
                               w_numsol
                              REPLACE  ;
                               orden  ;
                               WITH  ;
                               w_numord
                              REPLACE  ;
                               indori  ;
                               WITH  ;
                               w_indori
                              REPLACE  ;
                               fecemi  ;
                               WITH  ;
                               w_fecemi
                              REPLACE  ;
                               fecfac  ;
                               WITH  ;
                               w_fecfac
                              REPLACE  ;
                               marca  ;
                               WITH  ;
                               w_codmar
                              REPLACE  ;
                               modelo  ;
                               WITH  ;
                               w_codmod
                              REPLACE  ;
                               serie  ;
                               WITH  ;
                               w_numser
                              REPLACE  ;
                               docvta  ;
                               WITH  ;
                               w_docvta
                              REPLACE  ;
                               soluc  ;
                               WITH  ;
                               w_codsol
                              REPLACE  ;
                               cosmob  ;
                               WITH  ;
                               w_cosmob
                              REPLACE  ;
                               cosrep  ;
                               WITH  ;
                               w_cosrep
                              REPLACE  ;
                               flete  ;
                               WITH  ;
                               w_flete
                              REPLACE  ;
                               linea  ;
                               WITH  ;
                               w_codlin
                              REPLACE  ;
                               emisor  ;
                               WITH  ;
                               w_codemi
                              REPLACE  ;
                               fecent  ;
                               WITH  ;
                               w_fecent
                              REPLACE  ;
                               horent  ;
                               WITH  ;
                               w_horent
                              REPLACE  ;
                               fecfin  ;
                               WITH  ;
                               st_iorep.fecfin
                              REPLACE  ;
                               codtall  ;
                               WITH  ;
                               st_iorep.codtall
                              REPLACE  ;
                               codent  ;
                               WITH  ;
                               st_iorep.codent
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SELECT 7
     COUNT TO w_valor
     DO mensa WITH  ;
        '***  Un momento, Por Favor ...  ***',  ;
        'SACA'
     IF w_valor = 0
          DO error WITH  ;
             '***  No Existen registros a Listar  ***'
          LOOP
     ENDIF
     DO mensa WITH  ;
        '***  Exportando Datos ...  ***',  ;
        'COLO'
     IF w_disket <> 1
          IF w_disket = 2
               COPY TO a:porl4098
          ELSE
               IF w_disket = 3
                    COPY TO  ;
                         b:porl4098
               ELSE
                    COPY TO  ;
                         c:porl4098
               ENDIF
          ENDIF
     ENDIF
     GOTO TOP
     IF w_salida = 2
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          SET DEVICE TO PRINTER
          SET PRINTER ON
          @ PROW(), PCOL() SAY  ;
            CHR(15)
          FOR a = 1 TO w_copia
               IF w_tipo = 1
                    IF w_orden =  ;
                       1
                         REPORT FORMAT  ;
                                PORL4098  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                PORL0498  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ELSE
                    IF w_orden =  ;
                       1
                         REPORT FORMAT  ;
                                PORL498A  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                PORL498E  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDIF
               SET PRINTER TO
          ENDFOR
          SET PRINTER TO
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
               IF w_orden = 1
                    REPO FORM PORL4098;
TO FILE  &w_fildoc  NOCONSOLE
               ELSE
                    REPO FORM PORL0498;
TO FILE  &w_fildoc  NOCONSOLE
               ENDIF
          ELSE
               IF w_orden = 1
                    REPO FORM PORL498A;
SUMMARY TO FILE  &w_fildoc  NOCONSOLE
               ELSE
                    REPO FORM PORL498E;
SUMMARY TO FILE  &w_fildoc  NOCONSOLE
               ENDIF
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
     SELECT 7
     ZAP
ENDDO
CLOSE DATABASES
dele file &w_file
DO sacawin
RETURN
*
PROCEDURE oowhen
PARAMETER opc
DO CASE
     CASE opc = 'W_FECINI'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 'W_FECFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY
     CASE opc = 'W_LININI'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_LINFIN'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_GARINI'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_GARFIN'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_EMIINI'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_EMIFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_TIPO'
          ON KEY
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
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
     CASE opc = 'W_LININI' .OR.  ;
          opc = 'W_LINFIN'
          SELECT ge_tab0
          IF opc = 'W_LININI'
               SEEK 'LINE' +  ;
                    w_linini
          ELSE
               SEEK 'LINE' +  ;
                    w_linfin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '***  C¢digo de L¡nea no Existe  ***'
               RETURN .F.
          ENDIF
          IF opc = 'W_LINFIN'
               IF w_linini >  ;
                  w_linfin
                    DO error WITH  ;
                       '***  C¢digo Menor al Inicial  ***'
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 54 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     CASE opc = 'W_GARINI' .OR.  ;
          opc = 'W_GARFIN'
          SELECT ge_tab0
          IF opc = 'W_GARINI'
               SEEK 'INGA' +  ;
                    w_garini
          ELSE
               SEEK 'INGA' +  ;
                    w_garfin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '***  C¢digo de Garant¡a no Existe  ***'
               RETURN .F.
          ENDIF
          IF opc = 'W_GARFIN'
               IF w_garini >  ;
                  w_garfin
                    DO error WITH  ;
                       '***  C¢digo Menor al Inicial  ***'
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 18 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     CASE opc = 'W_EMIINI' .OR.  ;
          opc = 'W_EMIFIN'
          SELECT ge_tab0
          IF opc = 'W_EMIINI'
               SEEK 'EMIS' +  ;
                    w_emiini
          ELSE
               SEEK 'EMIS' +  ;
                    w_emifin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '***  C¢digo de Emisor no Existe  ***'
               RETURN .F.
          ENDIF
          IF opc = 'W_EMIFIN'
               IF w_emiini >  ;
                  w_emifin
                    DO error WITH  ;
                       '***  C¢digo Menor al Inicial  ***'
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 54 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
ENDCASE
RETURN
*
PROCEDURE ayuda10
DO CASE
     CASE VARREAD() = 'W_LININI'  ;
          .OR. VARREAD() =  ;
          'W_LINFIN'
          SELECT ge_tab0
          SET ORDER TO codigo
          SET FILTER TO tab_codpre = 'LINE'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE LINEAS'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
          SET ORDER TO codigo
     CASE VARREAD() = 'W_GARINI'  ;
          .OR. VARREAD() =  ;
          'W_GARFIN'
          SELECT ge_tab0
          SET FILTER TO tab_codpre = 'INGA'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE GARANTIAS'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
          SET ORDER TO codigo
     CASE VARREAD() = 'W_EMIINI'  ;
          .OR. VARREAD() =  ;
          'W_EMIFIN'
          SELECT ge_tab0
          SET FILTER TO tab_codpre = 'EMIS'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE EMISORES'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
          SET ORDER TO codigo
ENDCASE
*
*** 
*** ReFox - retrace your steps ... 
***

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
   ' SOLICITUDES FACTURADAS POR MARCA '
CLOSE DATABASES
w_progra = PROGRAM()
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
w_file = SUBSTR(da_nombre(), 1,  ;
         8) + '.DBF'
SELECT 6
CREATE TABLE &w_file (solic  C(08),;
 orden  C(08), indori C(04),   fecemi;
D, marca  C(04),  modelo C(15), serie;
 C(20),   docvta C(15), inform C(250),;
codtal C(04), emisor C(04),  fecfac D,;
fecent D,       fecfin D,    codtall C(4),;
codent c(11), horent C(08))
INDEX ON marca + modelo + solic  ;
      TAG codigo2
SET ORDER TO codigo2
STORE 1 TO w_tipo, w_salida,  ;
      w_copia, w_orden
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE CTOD('  /  /  ') TO  ;
      w_fecemi, w_fecent
STORE SPACE(04) TO w_talini,  ;
      w_talfin, w_garini,  ;
      w_garfin, w_emiini,  ;
      w_emifin, w_codmar,  ;
      w_indori, w_codemi,  ;
      w_codsol, w_marini,  ;
      w_marfin
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
     @ 04, 40 SAY 'Taller  :'
     @ 05, 45 SAY 'Del'
     @ 06, 45 SAY ' Al'
     @ 07, 01 SAY 'T. Atenci¢n :'
     @ 08, 09 SAY 'Del'
     @ 09, 09 SAY ' Al'
     @ 07, 40 SAY 'Emisor  :'
     @ 08, 45 SAY 'Del'
     @ 09, 45 SAY ' Al'
     @ 10, 01 SAY 'Marca   :'
     @ 11, 09 SAY 'Del'
     @ 12, 09 SAY ' Al'
     @ 10, 40 SAY 'Tipo Informe:'
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
     @ 05, 49 GET w_talini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 06, 49 GET w_talfin RANGE  ;
       w_talini PICTURE '@!'  ;
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
     @ 11, 13 GET w_marini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 12, 13 GET w_marfin RANGE  ;
       w_marini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 12, 49 GET w_tipo DEFAULT  ;
       1 PICTURE  ;
       '@*RVN Detalle;Resumen'  ;
       WHEN oowhen(VARREAD())
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
                         IF codtall >=  ;
                            w_talini  ;
                            .AND.  ;
                            codtall <=  ;
                            w_talfin
                              IF codmar >=  ;
                                 w_marini  ;
                                 .AND.  ;
                                 codmar <=  ;
                                 w_marfin
                                   w_codmar = codmar
                                   w_codmod = codmod
                                   w_numord = numdoc
                                   w_numsol = numsol
                                   w_indori = indori
                                   w_fecfac = fecfabo
                                   w_numser = numser
                                   w_codemi = codemi
                                   w_inform = observ
                                   w_codtal = codtall
                                   w_fecent = fecent
                                   w_horent = horent
                                   SELECT st_isrep
                                   SEEK w_numsol
                                   IF FOUND()
                                        w_fecemi = fecemi
                                   ENDIF
                                   w_docvta = SPACE(15)
                                   IF w_indori = 'GARA' .OR. w_indori = 'GREC'
                                        SELECT st_iseri
                                        SEEK w_codmar + w_codmod + w_numser
                                        IF FOUND()
                                             w_docvta = docgar
                                        ENDIF
                                   ENDIF
                                   SELECT 6
                                   APPEND BLANK
                                   REPLACE solic WITH w_numsol
                                   REPLACE orden WITH w_numord
                                   REPLACE indori WITH w_indori
                                   REPLACE fecemi WITH w_fecemi
                                   REPLACE fecfac WITH w_fecfac
                                   REPLACE marca WITH w_codmar
                                   REPLACE modelo WITH w_codmod
                                   REPLACE serie WITH w_numser
                                   REPLACE docvta WITH w_docvta
                                   REPLACE codtal WITH w_codtal
                                   REPLACE inform WITH w_inform
                                   REPLACE emisor WITH w_codemi
                                   REPLACE fecent WITH w_fecent
                                   REPLACE horent WITH w_horent
                                   REPLACE fecfin WITH st_iorep.fecfin
                                   REPLACE codtall WITH st_iorep.codtall
                                   REPLACE codent WITH st_iorep.codent
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SELECT 6
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
                    REPORT FORMAT  ;
                           PORL4100  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           PORL4100  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
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
               REPO FORM PORL4100 TO FILE;
 &w_fildoc  NOCONSOLE
          ELSE
               REPO FORM PORL4100 SUMMARY;
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
     SELECT 6
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
     CASE opc = 'W_TALINI'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_TALFIN'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_GARINI'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_GARFIN'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_EMIINI'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_EMIFIN'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_MARINI'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_MARFIN'
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
     CASE opc = 'W_TALINI' .OR.  ;
          opc = 'W_TALFIN'
          SELECT ge_tab0
          IF opc = 'W_TALINI'
               SEEK 'TALL' +  ;
                    w_talini
          ELSE
               SEEK 'TALL' +  ;
                    w_talfin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '***  C¢digo de Taller no Existe  ***'
               RETURN .F.
          ENDIF
          IF opc = 'W_TALFIN'
               IF w_talini >  ;
                  w_talfin
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
     CASE opc = 'W_MARINI' .OR.  ;
          opc = 'W_MARFIN'
          SELECT ge_tab0
          IF opc = 'W_MARINI'
               SEEK 'MARC' +  ;
                    w_marini
          ELSE
               SEEK 'MARC' +  ;
                    w_marfin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '***  C¢digo de Marca no Existe  ***'
               RETURN .F.
          ENDIF
          IF opc = 'W_MARFIN'
               IF w_marini >  ;
                  w_marfin
                    DO error WITH  ;
                       '***  C¢digo Menor al Inicial  ***'
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 18 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
ENDCASE
RETURN
*
PROCEDURE ayuda10
DO CASE
     CASE VARREAD() = 'W_TALINI'  ;
          .OR. VARREAD() =  ;
          'W_TALFIN'
          SELECT ge_tab0
          SET ORDER TO codigo
          SET FILTER TO tab_codpre = 'TALL'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE TALLERES'
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
     CASE VARREAD() = 'W_MARINI'  ;
          .OR. VARREAD() =  ;
          'W_MARFIN'
          SELECT ge_tab0
          SET FILTER TO tab_codpre = 'MARC'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE MARCAS'
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

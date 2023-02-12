*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER w_opcion
ON KEY
SET SYSMENU ON
SET DECIMALS TO 4
tit_prg = ' REPORTE '
wrk_progra = PROGRAM()
DO crea_win
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
IF w_opcion = 1
     DO saycenter WITH 2,  ;
        ' CONTROL DE LLAMADAS '
ELSE
     DO saycenter WITH 2,  ;
        'LLAMADAS COMPROMETIDAS'
ENDIF
CLOSE DATABASES
SELECT 1
USE SHARED st_isrep ORDER codigo
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 4
USE SHARED st_iclpr ORDER codigo
IF w_opcion = 1
     SELECT 5
     USE SHARED st_iscic ORDER  ;
         fecini
ELSE
     SELECT 5
     USE SHARED st_iscic ORDER  ;
         fecuse
ENDIF
SELECT 6
USE SHARED password ORDER usuario
STORE SPACE(04) TO w_tipini,  ;
      w_tipfin, w_emiini,  ;
      w_emifin
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE 1 TO w_tipo, w_salida,  ;
      w_copia, w_orden
STORE SPACE(10) TO w_usuini,  ;
      w_usufin
DO WHILE .T.
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 07, 01 CLEAR TO 14, 77
     @ 03, 00 TO 03, 77
     @ 04, 01 SAY 'Per¡odo    :'
     @ 07, 01 SAY 'Emisor     :'
     @ 10, 01 SAY 'Orden      :'
     @ 14, 01 SAY 'Destino    :'
     @ 05, 09 SAY 'Del'
     @ 06, 09 SAY ' Al'
     @ 08, 09 SAY 'Del'
     @ 09, 09 SAY ' Al'
     @ 04, 31 SAY  ;
       'Tipo Atenci¢n:'
     @ 07, 31 SAY  ;
       'Usuario      :'
     @ 10, 31 SAY  ;
       'Tipo         :'
     @ 05, 41 SAY 'Del'
     @ 06, 41 SAY ' Al'
     @ 08, 57 SAY 'Del'
     @ 09, 57 SAY ' Al'
     SET CURSOR ON
     @ 05, 13 GET w_fecini  ;
       PICTURE '@D' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 06, 13 GET w_fecfin RANGE  ;
       w_fecini PICTURE '@D'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 05, 45 GET w_tipini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 06, 45 GET w_tipfin RANGE  ;
       w_tipini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 08, 13 GET w_emiini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 09, 13 GET w_emifin RANGE  ;
       w_emiini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 08, 45 GET w_user DEFAULT  ;
       1 PICTURE  ;
       '@*RVN Todos;Usuario'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 11, 13 GET w_orden DEFAULT  ;
       1 PICTURE  ;
       '@*RVN Fecha;Usuario' WHEN  ;
       oowhen(VARREAD())
     @ 11, 45 GET w_tipo DEFAULT  ;
       1 PICTURE  ;
       '@*RVN Detalle;Resumen'
     @ 14, 13 GET w_salida  ;
       DEFAULT 1 PICTURE  ;
       '@*RHTN Pantalla;Impresora'
     READ CYCLE
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     IF w_salida = 2
          @ 14, 39 SAY  ;
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
     CREATE CURSOR archivo  ;
            (feclla D, horini C  ;
            (08), horfin C (08),  ;
            usuario C (08),  ;
            feccom D, horcom C  ;
            (08), numsol C (08),  ;
            numord C (08), marca  ;
            C (04), modelo C (15),  ;
            serie C (15), cliente  ;
            C (30), telef1 N (08),  ;
            telef2 N (08),  ;
            informe C (60))
     IF w_orden = 1
          INDEX ON DTOS(feclla) +  ;
                usuario TAG  ;
                codigo2
     ELSE
          INDEX ON usuario +  ;
                DTOS(feclla) TAG  ;
                codigo2
     ENDIF
     SELECT st_iscic
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     IF w_opcion = 1
          DO pro01
          titulo = 'C O N T R O L      D E     L L A M A D A S'
     ELSE
          DO pro02
          titulo = 'L L A M A D A S  C O M P R O M E T I D A S'
     ENDIF
     DO mensa WITH  ;
        '***  Un momento, Por Favor ...  ***',  ;
        'SACA'
     SELECT archivo
     GOTO TOP
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
          FOR a = 1 TO w_copia
               ??? CHR(15)
               IF w_tipo = 1
                    REPORT FORMAT  ;
                           porl0422  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           porl422A  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
               SET PRINTER TO
          ENDFOR
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
               repo form porl0422 to file;
&w_fildoc noconsole
          ELSE
               repo form porl422A summary;
to file &w_fildoc noconsole
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &w_fildoc noedit wind;
pantall
          SET SYSMENU OFF
          dele file &w_fildoc
     ENDIF
ENDDO
CLOSE DATABASES
SET DECIMALS TO 2
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
     CASE opc = 'W_TIPINI'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_TIPFIN'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_EMIINI'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_EMIFIN'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_USUINI'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_USUFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda10
     CASE opc = 'W_ORDEN'
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
          IF w_fecfin < w_fecini
               DO error WITH  ;
                  '***  Fecha Menor del inicio  ***'
               RETURN .F.
          ENDIF
     CASE opc = 'W_TIPINI' .OR.  ;
          opc = 'W_TIPFIN'
          SELECT ge_tab0
          IF opc = 'W_TIPINI'
               SEEK 'INGA' +  ;
                    w_tipini
          ELSE
               SEEK 'INGA' +  ;
                    w_tipfin
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '***  C¢digo no Existe  ***'
               RETURN .F.
          ENDIF
          IF opc = 'W_TIPFIN'
               IF w_tipini >  ;
                  w_tipfin
                    DO error WITH  ;
                       '***  C¢digo Menor al Inicial  ***'
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 50 SAY  ;
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
          @ ROW(), 18 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     CASE opc = 'W_USER'
          IF w_user = 2
               @ 08, 60 GET  ;
                 w_usuini PICTURE  ;
                 '@!' VALID  ;
                 oovali2(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())
               @ 09, 60 GET  ;
                 w_usufin RANGE  ;
                 w_usuini PICTURE  ;
                 '@!' VALID  ;
                 oovali2(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())
               READ
               IF LASTKEY() = 27
                    RETURN .F.
               ENDIF
          ENDIF
ENDCASE
RETURN
*
FUNCTION oovali2
PARAMETER opc
IF opc = 'W_USUINI' .OR. opc =  ;
   'W_USUFIN'
     IF opc = 'W_USUFIN'
          IF w_usuini > w_usufin
               DO error WITH  ;
                  '***  C¢digo Menor al Inicial  ***'
               RETURN .F.
          ENDIF
     ENDIF
     SELECT password
     IF opc = 'W_USUINI'
          SEEK w_usuini
     ELSE
          SEEK w_usufin
     ENDIF
     IF  .NOT. FOUND()
          DO error WITH  ;
             '***  C¢digo de Usuario no Existe  ***'
          RETURN .F.
     ENDIF
     @ ROW(), 71 SAY  ;
       SUBSTR(nombre, 1, 04)
ENDIF
RETURN
*
PROCEDURE ayuda10
DO CASE
     CASE VARREAD() = 'W_TIPINI'  ;
          .OR. VARREAD() =  ;
          'W_TIPFIN'
          SELECT ge_tab0
          SET ORDER TO codigo
          SET FILTER TO tab_codpre = 'INGA'
          campo = 'tab_codtab+"  "+tab_destab'
          titulo = 'AYUDA DE LINEAS'
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
          campo = 'tab_codtab+"  "+tab_destab'
          titulo = 'AYUDA DE EMISORES'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
          SET ORDER TO codigo
     CASE VARREAD() = 'W_USUINI'  ;
          .OR. VARREAD() =  ;
          'W_USUFIN'
          SELECT password
          titulo = 'AYUDA DE USUARIOS'
          IF VARREAD() =  ;
             'W_USUINI'
               w_codusu = w_usuini
          ELSE
               w_codusu = w_usufin
          ENDIF
          DO pormusua WITH  ;
             SELECT(), w_codusu,  ;
             VARREAD()
          SET ORDER TO usuario
ENDCASE
RETURN
*
PROCEDURE pro01
SCAN WHILE fecini <= w_fecfin  ;
     .AND.  .NOT. EOF()
     w_sigue = .T.
     IF w_user = 2
          IF SUBSTR(user, 1, 8) >=  ;
             SUBSTR(w_usuini, 1,  ;
             8) .AND. SUBSTR(user,  ;
             1, 8) <=  ;
             SUBSTR(w_usufin, 1,  ;
             8)
               w_sigue = .T.
          ELSE
               w_sigue = .F.
          ENDIF
     ENDIF
     IF w_sigue
          SELECT st_isrep
          SEEK st_iscic.numsol
          IF FOUND() .AND.  ;
             st_isrep.indest <>  ;
             'N'
               IF st_isrep.indori >=  ;
                  w_tipini .AND.  ;
                  st_isrep.indori <=  ;
                  w_tipfin
                    IF st_isrep.codemi >=  ;
                       w_emiini  ;
                       .AND.  ;
                       st_isrep.codemi <=  ;
                       w_emifin
                         SELECT st_iclpr
                         SEEK 'C' +  ;
                              st_isrep.codent
                         IF FOUND()
                              SELECT  ;
                               archivo
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               feclla  ;
                               WITH  ;
                               st_iscic.fecini
                              REPLACE  ;
                               horini  ;
                               WITH  ;
                               st_iscic.horini
                              REPLACE  ;
                               horfin  ;
                               WITH  ;
                               st_iscic.horfin
                              REPLACE  ;
                               usuario  ;
                               WITH  ;
                               st_iscic.user
                              REPLACE  ;
                               feccom  ;
                               WITH  ;
                               st_iscic.feccom
                              REPLACE  ;
                               horcom  ;
                               WITH  ;
                               st_iscic.horcom
                              REPLACE  ;
                               numsol  ;
                               WITH  ;
                               st_iscic.numsol
                              REPLACE  ;
                               numord  ;
                               WITH  ;
                               st_iscic.numord
                              REPLACE  ;
                               marca  ;
                               WITH  ;
                               st_isrep.codmar
                              REPLACE  ;
                               modelo  ;
                               WITH  ;
                               st_isrep.codmod
                              REPLACE  ;
                               serie  ;
                               WITH  ;
                               st_isrep.numser
                              REPLACE  ;
                               cliente  ;
                               WITH  ;
                               st_iclpr.noment
                              REPLACE  ;
                               telef1  ;
                               WITH  ;
                               st_iclpr.numte1
                              REPLACE  ;
                               telef2  ;
                               WITH  ;
                               st_iclpr.numte2
                              REPLACE  ;
                               informe  ;
                               WITH  ;
                               st_iscic.inform
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
     SELECT st_iscic
ENDSCAN
RETURN
*
PROCEDURE pro02
SELECT 20
USE SHARED st_iorep ORDER  ;
    ord_numsol
SELECT 21
USE SHARED gc_hve00 ORDER nrdore
SELECT st_iscic
SCAN WHILE feccom <= w_fecfin  ;
     .AND.  .NOT. EOF()
     w_sigue = .T.
     IF w_user = 2
          IF SUBSTR(user, 1, 8) >=  ;
             SUBSTR(w_usuini, 1,  ;
             8) .AND. SUBSTR(user,  ;
             1, 8) <=  ;
             SUBSTR(w_usufin, 1,  ;
             8)
               w_sigue = .T.
          ELSE
               w_sigue = .F.
          ENDIF
     ENDIF
     IF w_sigue
          SELECT st_isrep
          SEEK st_iscic.numsol
          IF FOUND() .AND. indest <>  ;
             'N'
               SELECT st_iorep
               SEEK st_iscic.numsol
               IF FOUND()
                    IF auxest <>  ;
                       '100 '  ;
                       .AND.  ;
                       auxest <>  ;
                       '025 '  ;
                       .AND.  ;
                       auxest <>  ;
                       '024 '
                         IF indest <>  ;
                            'P'  ;
                            .AND.  ;
                            indest <>  ;
                            'V'
                              IF (auxest =  ;
                                 '029 '  ;
                                 .OR.  ;
                                 auxest =  ;
                                 '028 '  ;
                                 .OR.  ;
                                 auxest =  ;
                                 '023 '  ;
                                 .OR.  ;
                                 auxest =  ;
                                 '022 ' ;
                                 )  ;
                                 .AND.  ;
                                 SUBSTR(indori,  ;
                                 2,  ;
                                 1) =  ;
                                 'R'  ;
                                 .AND.  ;
                                 cosrep =  ;
                                 0
                                   w_sigue = .F.
                              ELSE
                                   IF ((auxest = '028 ' .OR. auxest = '023 ') .AND. indori = 'FGAR' .AND. cosrep = 0)
                                        w_sigue = .F.
                                   ELSE
                                        IF ((auxest = '029 ' .OR. auxest = '028 ' .OR. auxest = '023 ' .OR. auxest = '022 ') .AND. SUBSTR(indori, 1, 1) = 'F' .AND. cosrep > 0)
                                             STORE 0 TO w_impor
                                             SELECT gc_hve00
                                             SEEK st_iorep.numsol
                                             IF FOUND()
                                                  SCAN WHILE hve_nrdore = st_iorep.numsol .AND.  .NOT. EOF()
                                                       IF hve_estdoc <> 'A'
                                                            w_impor = w_impor + hve_totgen
                                                       ENDIF
                                                  ENDSCAN
                                             ENDIF
                                             IF w_impor >= st_iorep.totbru
                                                  w_sigue = .F.
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                         IF w_sigue
                              SELECT  ;
                               st_isrep
                              IF st_isrep.indori >=  ;
                                 w_tipini  ;
                                 .AND.  ;
                                 st_isrep.indori <=  ;
                                 w_tipfin
                                   IF st_isrep.codemi >= w_emiini .AND. st_isrep.codemi <= w_emifin
                                        SELECT st_iclpr
                                        SEEK 'C' + st_isrep.codent
                                        IF FOUND()
                                             SELECT archivo
                                             APPEND BLANK
                                             REPLACE feclla WITH st_iscic.feccom
                                             REPLACE horini WITH st_iscic.horini
                                             REPLACE horfin WITH st_iscic.horfin
                                             REPLACE usuario WITH st_iscic.user
                                             REPLACE feccom WITH st_iscic.fecini
                                             REPLACE horcom WITH st_iscic.horcom
                                             REPLACE numsol WITH st_iscic.numsol
                                             REPLACE numord WITH st_iscic.numord
                                             REPLACE marca WITH st_isrep.codmar
                                             REPLACE modelo WITH st_isrep.codmod
                                             REPLACE serie WITH st_isrep.numser
                                             REPLACE cliente WITH st_iclpr.noment
                                             REPLACE telef1 WITH st_iclpr.numte1
                                             REPLACE telef2 WITH st_iclpr.numte2
                                             REPLACE informe WITH st_iscic.inform
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
     SELECT st_iscic
ENDSCAN
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

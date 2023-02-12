*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ARTICULOS POR TIPO DE GARANTIA '
SELECT 1
USE SHARED GE_TAB0 ORDER CODIGO
DO WHILE .T.
     @ 07, 01 CLEAR TO 13, 77
     @ 04, 30 SAY SPACE(30)
     @ 03, 01 TO 11, 77
     @ 04, 05 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     STORE MONTH(DATE()) TO  ;
           wrk_fecini,  ;
           wrk_fecfin
     STORE 1 TO wrk_copia
     STORE SPACE(4) TO wrk_emisor,  ;
           wrk_tipgar
     STORE 'D' TO wrk_detres
     @ 04, 03 SAY  ;
       'Emisor       :'
     @ 06, 03 SAY  ;
       'Mes Inicial  :'
     @ 06, 55 SAY 'Mes Final :'
     @ 08, 03 SAY  ;
       'Tipo de Gt¡a.:'
     @ 08, 55 SAY 'Copias    :'
     @ 10, 03 SAY  ;
       'Detalle/Resumen :  etalle'
     STORE 1 TO wrk_opc
     @ 04, 18 GET wrk_emisor  ;
       PICTURE '@!' VALID  ;
       valida(wrk_emisor,1,1)  ;
       WHEN antes(1)
     @ 06, 20 GET wrk_fecini  ;
       RANGE 1,12 PICTURE '99'  ;
       WHEN antes(2)
     @ 06, 67 GET wrk_fecfin  ;
       RANGE 1,12 PICTURE '99'
     @ 08, 18 GET wrk_tipgar  ;
       PICTURE '@!' VALID  ;
       valida(wrk_tipgar,1,4)  ;
       WHEN antes(4)
     @ 08, 67 GET wrk_copia RANGE  ;
       1,99 PICTURE '99' WHEN  ;
       antes(5)
     @ 10, 21 GET wrk_detres  ;
       PICTURE '@!' VALID  ;
       (wrk_detres $ 'DR')
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     IF wrk_detres = 'R'
          @ 10, 22 SAY 'esumen '
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_iorep.numdoc,  ;
            st_iorep.fecemi,  ;
            st_iorep.codemi,  ;
            st_iorep.codent,  ;
            st_iorep.indori,  ;
            st_iorep.indest,  ;
            st_iorep.auxest,  ;
            st_iorep.codmar,  ;
            st_iorep.codmod,  ;
            st_iorep.numser,  ;
            st_iorep.codtec,  ;
            st_iorep.observ,  ;
            st_iorep.numsol,  ;
            st_iorep.fecest FROM  ;
            ST_IOREP WHERE  ;
            BETWEEN(MONTH(st_iorep.fecemi),  ;
            wrk_fecini,  ;
            wrk_fecfin) AND  ;
            st_iorep.indest <>  ;
            'N' AND  ;
            st_iorep.codemi =  ;
            wrk_emisor AND  ;
            st_iorep.indori =  ;
            wrk_tipgar ORDER BY  ;
            st_iorep.fecemi,  ;
            st_iorep.numdoc INTO  ;
            CURSOR GARA
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'IMP', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'VER', 'BBB', 'ESC'
     DO WHILE LASTKEY()<>-6 .AND.  ;
        LASTKEY()<>27
          wk_key = INKEY(0, 'H')
     ENDDO
     IF wk_key = -6
          FOR a = 1 TO wrk_copia
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               IF wrk_detres =  ;
                  'R'
                    REPORT FORMAT  ;
                           PORL4101  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           PORL4101  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDFOR
          SET PRINTER TO
     ENDIF
ENDDO
DO saca_win
RETURN
*
FUNCTION valida
PARAMETER wrk_codtab, opc, val
DO CASE
     CASE val = 1
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF EMPTY(wrk_codtab)
               DO error WITH  ;
                  ' *** No se Permiten Blancos ***'
               RETURN .F.
          ENDIF
          SELECT 1
          IF opc = 1
               SEEK 'EMIS' +  ;
                    wrk_codtab
          ELSE
               SEEK 'INGA' +  ;
                    wrk_tipgar
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          IF opc = 1
               @ 04, 25 SAY  ;
                 tab_destab
          ELSE
               @ 08, 25 SAY  ;
                 tab_destab
          ENDIF
     CASE val = 2
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
ENDCASE
*
PROCEDURE ayuda
PARAMETER opc
SELECT 1
IF opc = 1
     SET FILTER TO tab_codpre == 'EMIS'
ELSE
     SET FILTER TO tab_codpre == 'INGA'
ENDIF
titulo = 'AYUDA DE ESTADOS'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE ESTADOS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
          ON KEY LABEL F6 DO AYUDA WITH;
1
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 3
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 4
          ON KEY LABEL F6 DO AYUDA WITH;
2
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 5
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
*
*** 
*** ReFox - retrace your steps ... 
***

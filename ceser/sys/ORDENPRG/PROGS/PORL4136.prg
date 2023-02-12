*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL4136>'
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' PLANILLA DE SIN REPARAR '
SELECT 1
USE SHARED ST_MVORD ORDER CODIGO
SELECT 2
USE SHARED ST_ICLPR ORDER CODIGO
SELECT 1
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
STORE DATE() TO wrk_dia, wrk_dia2
STORE 'R' TO wrk_destin
DO WHILE .T.
     @ 7, 1 CLEAR TO 13, 77
     @ 4, 30 SAY SPACE(30)
     @ 3, 2 TO 8, 77
     @ 4, 5 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wrk_copia = 1
     @ 04, 05 SAY 'Desde Fecha:'
     @ 05, 05 SAY 'Hasta Fecha:'
     @ 05, 34 SAY  ;
       'Destino (D/R) :'
     @ 05, 54 SAY 'Copias :'
     @ 06, 05 SAY  ;
       'Por Impresora/Pantalla:'
     @ 07, 05 SAY  ;
       'Por Detalle/Res£men:'
     SET CURSOR ON
     @ 04, 17 GET wrk_dia PICTURE  ;
       '@D' VALID valida(1)
     @ 05, 17 GET wrk_dia2 RANGE  ;
       wrk_dia VALID valida(1)
     @ 05, 51 GET wrk_destin  ;
       PICTURE '@!' VALID  ;
       wrk_destin $ 'RD'
     @ 05, 63 GET wrk_copia  ;
       PICTURE '999' VALID  ;
       valida(2)
     @ 06, 28 GET output PICTURE  ;
       '@m Pantalla,Impresora'
     @ 07, 28 GET tipo PICTURE  ;
       '@m Detalle,Res£men'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT DISTINCT st_mvord.dia,  ;
            st_mvord.tecnico,  ;
            st_mvord.estado,  ;
            st_mvord.orden,  ;
            st_iorep.codemi,  ;
            st_iorep.codent,  ;
            st_iorep.indest,  ;
            st_iorep.codmar,  ;
            st_iorep.codmod,  ;
            st_iorep.numser,  ;
            st_iorep.codtec,  ;
            st_iorep.codcca,  ;
            st_iorep.observ,  ;
            st_iorep.codtall,  ;
            st_iorep.indori,  ;
            st_iorep.numsol,  ;
            st_isrep.coddes FROM  ;
            ST_MVORD, ST_IOREP,  ;
            ST_ISREP WHERE  ;
            st_iorep.numdoc =  ;
            st_mvord.orden AND  ;
            st_isrep.numdoc =  ;
            st_iorep.numsol AND  ;
            (st_mvord.dia >=  ;
            wrk_dia AND  ;
            st_mvord.dia <=  ;
            wrk_dia2 AND  ;
            st_mvord.estado =  ;
            '021 ' AND  ;
            st_isrep.coddes =  ;
            wrk_destin) ORDER BY  ;
            st_mvord.dia,  ;
            st_iorep.codemi,  ;
            st_iorep.indori,  ;
            st_mvord.orden INTO  ;
            CURSOR REPAR
     COUNT TO wrk_valor
     IF wrk_valor = 0
          DO error WITH  ;
             'No Existen registros a Listar'
          LOOP
     ENDIF
     IF output = 'Impresora'
          FOR a = 1 TO wrk_copia
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               ??? CHR(15)
               IF tipo =  ;
                  'Detalle'
                    REPORT FORMAT  ;
                           porl4136  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           porl4136  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDFOR
          SET PRINTER TO
     ELSE
          IF tipo = 'Detalle'
               REPORT FORMAT  ;
                      porl4136 TO  ;
                      FILE  ;
                      text4.txt  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      porl4136  ;
                      SUMMARY TO  ;
                      FILE  ;
                      text4.txt  ;
                      NOCONSOLE
          ENDIF
          SET SYSMENU ON
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          MODIFY FILE text4.txt  ;
                 WINDOW pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
RETURN
*
FUNCTION valida
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
     CASE opc = 2
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
ENDCASE
*
*** 
*** ReFox - retrace your steps ... 
***

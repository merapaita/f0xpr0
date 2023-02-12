*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2, ' ESTADO DE ARTICULOS (HISTORICO) '
ppas = .T.
CLOSE DATABASES
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
STORE 'Pantalla ' TO output
STORE SPACE(4) TO wrk_estado
STORE SPACE(1) TO wrk_destin
STORE 1 TO wrk_copia
STORE DATE() TO fecha1, fecha2
STORE 'Res£men' TO tipo
STORE '001 ' TO taller
ON KEY LABEL f6 do ayuda with col()
DO WHILE ppas
     @ 07, 01 CLEAR TO 13, 77
     @ 03, 02 TO 08, 77
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU', 'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB', 'BBB', 'BBB', 'ESC'
     wk_key = 0
     SET CURSOR ON
     @ 04, 04 SAY 'C¢digo de Estado :'
     @ 05, 04 SAY 'Desde:'
     @ 05, 20 SAY 'Hasta:'
     @ 04, 23 GET wrk_estado PICTURE '@!' VALID valida(1)
     @ 04, 50 SAY 'Taller:' GET taller VALID va(1)
     @ 05, 10 GET fecha1
     @ 05, 29 GET fecha2 RANGE fecha1
     @ 06, 04 SAY 'Por Impresora/Pantalla:' GET output PICTURE '@m Pantalla ,Impresora'
     @ 07, 04 SAY 'Por Res£men/Detalle:' GET tipo PICTURE '@m Res£men,Detalle'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH '** Un momento, Por Favor ... **', 'COLO'
     IF wrk_estado = '012 ' .OR.  ;
        wrk_estado = '013 ' .OR.  ;
        wrk_estado = '014 ' .OR.  ;
        wrk_estado = '015 ' .OR.  ;
        wrk_estado = '016 ' .OR.  ;
        wrk_estado = '018 '
          op = 1
          SELECT DISTINCT  ;
                 st_iorep.numdoc, st_iorep.fecemi,  ;
                 st_iorep.codemi, st_iorep.codent,  ;
                 st_iorep.indori, st_iorep.indest,  ;
                 st_iorep.codmar, st_iorep.codmod,  ;
                 st_iorep.numser, st_iorep.codtec,  ;
                 st_iorep.observ, st_iorep.numsol,  ;
                 st_iorep.codtall, st_mvord.dia,  ;
                 st_mvord.destado  ;
                 FROM ST_IOREP, ST_MVORD 
                 WHERE st_mvord.orden = st_iorep.numdoc AND  ;
                 (st_iorep.indest <> 'N' AND st_mvord.estado =  ;
                 wrk_estado AND st_mvord.dia >= fecha1 AND  ;
                 st_mvord.dia <= fecha2) 
                 ORDER BY st_mvord.destado, st_mvord.dia,  ;
                 st_iorep.numdoc  ;
                 INTO CURSOR  ;
                 QUERY
          COUNT TO wrk_valor
     ELSE
          op = 2
          SELECT DISTINCT  ;
                 st_iorep.numdoc,  ;
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
                 st_iorep.codtall,  ;
                 st_iorep.fecest,  ;
                 st_iorep.cosrep,  ;
                 st_iorep.cosmob,  ;
                 st_iorep.totdes,  ;
                 st_mvord.dia,  ;
                 st_mvord.estado  ;
                 FROM ST_IOREP,  ;
                 ST_MVORD WHERE  ;
                 st_iorep.numdoc =  ;
                 st_mvord.orden  ;
                 AND  ;
                 st_iorep.indest <>  ;
                 'N' AND  ;
                 (st_mvord.estado =  ;
                 wrk_estado AND  ;
                 st_mvord.dia >=  ;
                 fecha1 AND  ;
                 st_mvord.dia <=  ;
                 fecha2) AND  ;
                 st_iorep.codtall =  ;
                 taller ORDER BY  ;
                 st_mvord.estado,  ;
                 st_mvord.dia,  ;
                 st_iorep.numdoc  ;
                 INTO CURSOR  ;
                 QUERY
     ENDIF
     COUNT TO wrk_valor
     IF wrk_valor = 0
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO error WITH  ;
             'No Existen registros a Listar'
          LOOP
     ENDIF
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          ??? CHR(15)
          IF tipo = 'Res£men'
               IF op = 1
                    REPORT FORMAT  ;
                           PORL4071  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    IF wrk_estado =  ;
                       '020 '
                         REPORT FORMAT  ;
                                POR40712  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                PORL4130  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDIF
          ELSE
               IF op = 1
                    REPORT FORMAT  ;
                           PORL4071  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    IF wrk_estado =  ;
                       '020 '
                         REPORT FORMAT  ;
                                POR40712  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                PORL4130  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDIF
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDIF
     ELSE
          IF tipo = 'Res£men'
               IF op = 1
                    REPORT FORMAT  ;
                           PORL4071  ;
                           SUMMARY  ;
                           TO  ;
                           FILE  ;
                           TEXT3.TXT  ;
                           NOCONSOLE
               ELSE
                    IF wrk_estado =  ;
                       '020 '
                         REPORT FORMAT  ;
                                POR40712  ;
                                SUMMARY  ;
                                TO  ;
                                FILE  ;
                                TEXT3.TXT  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                PORL4130  ;
                                SUMMARY  ;
                                TO  ;
                                FILE  ;
                                TEXT3.TXT  ;
                                NOCONSOLE
                    ENDIF
               ENDIF
          ELSE
               IF op = 1
                    REPORT FORMAT  ;
                           PORL4071  ;
                           TO  ;
                           FILE  ;
                           TEXT3.TXT  ;
                           NOCONSOLE
               ELSE
                    IF wrk_estado =  ;
                       '020 '
                         REPORT FORMAT  ;
                                POR40712  ;
                                TO  ;
                                FILE  ;
                                TEXT3.TXT  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                PORL4130  ;
                                TO  ;
                                FILE  ;
                                TEXT3.TXT  ;
                                NOCONSOLE
                    ENDIF
               ENDIF
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODIFY COMMAND  ;
                 TEXT3.TXT NOEDIT  ;
                 WINDOW pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT 2
IF opc < 40
     SET FILTER TO tab_codpre == 'ESOR'
     titulo = 'AYUDA DE ESTADOS'
ELSE
     SET FILTER TO tab_codpre == 'TALL'
     titulo = 'AYUDA DE TALLERES'
ENDIF
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
RETURN
*
PROCEDURE antes
ON KEY LABEL F6 DO AYUDA
RETURN
*
FUNCTION valida
PARAMETER opc
SELECT 2
IF LASTKEY() = 5 .OR. LASTKEY() = 19
     RETURN .F.
ENDIF
IF EMPTY(wrk_estado)
     DO error WITH 'No se Permiten Blancos'
     RETURN .F.
ENDIF
SEEK 'ESOR' + wrk_estado
IF  .NOT. FOUND()
     DO error WITH 'C¢digo de Estado No Existe'
     RETURN .F.
ENDIF
@ 04, 29 SAY SUBSTR(tab_destab, 1, 20)
RETURN
*
FUNCTION va
PARAMETER opc
SELECT 2
IF EMPTY(taller)
     DO error WITH 'No se Permiten Ceros'
     RETURN .F.
ENDIF
SEEK 'TALL' + taller
IF  .NOT. FOUND()
     DO error WITH 'C¢digo de Estado No Existe'
     RETURN .F.
ENDIF
@ 04, 63 SAY SUBSTR(tab_destab, 1,  ;
  10)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

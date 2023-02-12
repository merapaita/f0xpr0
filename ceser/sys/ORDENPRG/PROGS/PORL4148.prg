*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INGRESO DE ORDENES POR TECNICO '
SELECT 1
USE SHARED ST_ITECN ORDER CODIGO
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 3
USE SHARED ST_ICLPR ORDER CODIGO
SELECT 4
USE SHARED ST_IOREP ORDER  ;
    ORD_FECDOC
SELECT 5
USE SHARED ST_ISREP ORDER CODIGO
STORE 0 TO tecni1, tecni2
STORE SPACE(4) TO taller1,  ;
      taller2, emisor1, emisor2
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
STORE DATE() TO fecha1, fecha2
@ 3, 2 TO 9, 77
DO esc_modo WITH 'S'
@ 04, 30 SAY SPACE(30)
sigue = .T.
DO WHILE sigue
     @ 07, 01 CLEAR TO 14, 77
     @ 3, 2 TO 11, 77
     @ 04, 30 SAY SPACE(30)
     @ 04, 05 SAY SPACE(50)
     STORE 0 TO nco, w_reg
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 03 SAY 'Del Fecha  :'  ;
       GET fecha1
     @ 04, 39 SAY 'Al  Fecha  :'  ;
       GET fecha2 RANGE fecha1
     @ 05, 03 SAY 'Del T‚cnico:'  ;
       GET tecni1 PICTURE  ;
       '999999999' VALID  ;
       valida(tecni1,1,30) WHEN  ;
       antes(1)
     @ 06, 03 SAY 'Al  T‚cnico:'  ;
       GET tecni2 RANGE tecni1  ;
       PICTURE '999999999' VALID  ;
       valida(tecni2,1,30) WHEN  ;
       antes(1)
     @ 07, 03 SAY 'Del Taller :'  ;
       GET taller1 PICTURE '@!'  ;
       VALID valida(taller1,2,21)  ;
       WHEN antes(2)
     @ 07, 39 SAY 'Al  Taller :'  ;
       GET taller2 RANGE taller1  ;
       PICTURE '@!' VALID  ;
       valida(taller2,2,56) WHEN  ;
       antes(2)
     @ 08, 03 SAY 'Del Emisor :'  ;
       GET emisor1 PICTURE '@!'  ;
       VALID valida(emisor1,3,21)  ;
       WHEN antes(3)
     @ 08, 39 SAY 'Al  Emisor :'  ;
       GET emisor2 RANGE emisor1  ;
       PICTURE '@!' VALID  ;
       valida(emisor2,3,56) WHEN  ;
       antes(3)
     @ 09, 03 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'  ;
       WHEN antes(2)
     @ 10, 03 SAY  ;
       'Por Detalle/Res£men:' GET  ;
       tipo PICTURE  ;
       '@m Detalle,Res£men'
     READ
     IF LASTKEY() = 27
          sigue = .F.
          LOOP
     ENDIF
     IF output = 'Impresora'
          nco = 1
          @ 09, 40 SAY  ;
            'Copias Nro.:' GET  ;
            nco PICTURE '99'  ;
            VALID nco >= 0
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     CREATE CURSOR DOMI2 (numdoc  ;
            C (8), fecemi D,  ;
            codemi C (4), codent  ;
            C (11), codmar C (4),  ;
            indori C (4), indest  ;
            C (4), codmod C (15),  ;
            codtec C (9), observ  ;
            MEMO, nomcli C (30),  ;
            telcli N (8), numsol  ;
            C (8), fecsol D,  ;
            destec C (30), auxest  ;
            C (4), fecest D,  ;
            codtall C (4))
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_iorep
     SET NEAR ON
     SEEK DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE (fecemi >= fecha1  ;
          .AND. fecemi <= fecha2)  ;
          .AND.  .NOT. EOF()
          IF (codtec >=  ;
             STR(tecni1, 9) .AND.  ;
             codtec <= STR(tecni2,  ;
             9)) .AND. indest <>  ;
             'N   ' .AND.  ;
             (codtall >= taller1  ;
             .AND. codtall <=  ;
             taller2) .AND.  ;
             (codemi >= emisor1  ;
             .AND. codemi <=  ;
             emisor2)
               w_reg = w_reg + 1
               SELECT domi2
               APPEND BLANK
               REPLACE numdoc  ;
                       WITH  ;
                       st_iorep.numdoc,  ;
                       fecemi  ;
                       WITH  ;
                       st_iorep.fecemi
               REPLACE codemi  ;
                       WITH  ;
                       st_iorep.codemi,  ;
                       indori  ;
                       WITH  ;
                       st_iorep.indori
               REPLACE codent  ;
                       WITH  ;
                       st_iorep.codent,  ;
                       codmar  ;
                       WITH  ;
                       st_iorep.codmar
               REPLACE indest  ;
                       WITH  ;
                       st_iorep.indest,  ;
                       codtec  ;
                       WITH  ;
                       st_iorep.codtec
               REPLACE codmod  ;
                       WITH  ;
                       st_iorep.codmod,  ;
                       observ  ;
                       WITH  ;
                       st_iorep.observ
               REPLACE auxest  ;
                       WITH  ;
                       st_iorep.auxest,  ;
                       numsol  ;
                       WITH  ;
                       st_iorep.numsol
               REPLACE fecest  ;
                       WITH  ;
                       st_iorep.fecest,  ;
                       codtall  ;
                       WITH  ;
                       st_iorep.codtall
               SELECT st_isrep
               SEEK domi2.numsol
               IF FOUND()
                    SELECT domi2
                    REPLACE fecsol  ;
                            WITH  ;
                            st_isrep.fecemi
               ENDIF
               SELECT st_iclpr
               SEEK 'C' +  ;
                    domi2.codent
               IF FOUND()
                    SELECT domi2
                    REPLACE nomcli  ;
                            WITH  ;
                            st_iclpr.noment
                    REPLACE telcli  ;
                            WITH  ;
                            st_iclpr.numte1
               ENDIF
               SELECT st_itecn
               SEEK domi2.codtec
               IF FOUND()
                    SELECT domi2
                    REPLACE destec  ;
                            WITH  ;
                            st_itecn.noment
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     IF w_reg = 0
          DO error WITH  ;
             '*** No hay informaci¢n para el Reporte ***'
     ELSE
          filidx = SYS(3)
          SELECT domi2
          GOTO TOP
          index on codtec+dtoc(fecsol)+numsol+numdoc+dtoc(fecemi);
to &filidx  
          GOTO TOP
          IF output = 'Impresora'
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               SET DEVICE TO PRINTER
               SET PRINTER ON
               ?? CHR(27) +  ;
                  CHR(15)
               FOR i = 1 TO nco
                    IF tipo =  ;
                       'Res£men'
                         REPORT FORMAT  ;
                                porl4148  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                porl4148  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDFOR
               ?? CHR(27) +  ;
                  CHR(18)
               SET PRINTER OFF
               SET DEVICE TO SCREEN
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ELSE
               filtxt = SYS(3) +  ;
                        '.TXT'
               IF tipo =  ;
                  'Res£men'
                    repo form porl4148;
to file &FILTXT noconsole summary
               ELSE
                    repo form porl4148;
to file &FILTXT noconsole 
               ENDIF
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               modi comm &FILTXT;
 window pantall NOEDIT
               DELE FILE &FILTXT
               SET SYSMENU OFF
          ENDIF
     ENDIF
ENDDO
DO sacawin
CLOSE DATABASES
RETURN
*
FUNCTION valida
PARAMETER wrk_codtab, val, col
IF EMPTY(wrk_codtab)
     DO error WITH  ;
        ' *** No se Permiten Blancos ***'
     RETURN .F.
ENDIF
DO CASE
     CASE val = 1
          SELECT st_itecn
          SEEK STR(wrk_codtab, 9)
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de T‚cnico No Existe ***'
               RETURN .F.
          ENDIF
          @ ROW(), col SAY noment
     CASE val = 2
          SELECT ge_tab0
          SEEK 'TALL' +  ;
               wrk_codtab
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          @ ROW(), col SAY  ;
            SUBSTR(tab_destab, 1,  ;
            18)
     CASE val = 3
          SELECT ge_tab0
          SEEK 'EMIS' +  ;
               wrk_codtab
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          @ ROW(), col SAY  ;
            SUBSTR(tab_destab, 1,  ;
            18)
ENDCASE
RETURN
*
PROCEDURE ayuda
PARAMETER opc2
DO CASE
     CASE opc2 = 1
          SELECT st_itecn
          campoa = '" "+codent+" "+noment+" "+CODTEC'
          campob = '" "+noment+" "+codent+" "+CODTEC'
          titulo = 'AYUDA DE TECNICOS'
          DO ayuda2 WITH campoa,  ;
             campob, titulo,  ;
             'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
          SET ORDER TO codigo
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc2 = 2
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'TALL'
          titulo = 'AYUDA DE TALLER'
          campo = 'tab_codtab + "  " + tab_destab'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc2 = 3
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'EMIS'
          titulo = 'AYUDA DE EMISOR'
          campo = 'tab_codtab + "  " + tab_destab'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
RETURN
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
          ON KEY LABEL F6 DO AYUDA WITH;
2
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 3
          ON KEY LABEL F6 DO AYUDA WITH;
3
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 4
          ON KEY LABEL F6
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

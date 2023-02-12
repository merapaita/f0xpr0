*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' TIEMPO DESDE CIERRE O/R A CLIENTE '
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_entind
SELECT 3
USE SHARED st_imode ORDER codigo
@ 02, 01 SAY DATE()
STORE SPACE(4) TO tipgar1,  ;
      tipgar2, emiini, emifin
STORE DATE() TO fecha1, fecha2
STORE 'Resumen' TO w_tipo
STORE 'Pantalla ' TO output
DO WHILE .T.
     @ 07, 01 CLEAR TO 13, 77
     @ 04, 30 SAY SPACE(30)
     @ 03, 01 TO 11, 77
     @ 04, 05 SAY SPACE(50)
     SET CURSOR ON
     @ 04, 03 SAY  ;
       'Tipo Aten.Del:'
     @ 04, 43 SAY 'Al :'
     @ 05, 03 SAY  ;
       'Emisor    Del:'
     @ 05, 43 SAY 'Al :'
     @ 06, 03 SAY  ;
       'Fecha     Del:'
     @ 06, 43 SAY 'Al :'
     @ 07, 03 SAY  ;
       'Ordenado por :'
     @ 08, 03 SAY  ;
       'Detalle/Resumen   :'
     @ 09, 03 SAY  ;
       'Pantalla/Impresora:'
     @ 04, 18 GET tipgar1 PICTURE  ;
       '@!' VALID valida(tipgar1, ;
       1) WHEN antes(1)
     @ 04, 48 GET tipgar2 RANGE  ;
       tipgar1 PICTURE '@!' VALID  ;
       valida(tipgar2,2) WHEN  ;
       antes(1)
     @ 05, 18 GET emiini PICTURE  ;
       '@!' VALID valida(emiini, ;
       3) WHEN antes(3)
     @ 05, 48 GET emifin RANGE  ;
       emiini PICTURE '@!' VALID  ;
       valida(emifin,4) WHEN  ;
       antes(3)
     @ 06, 18 GET fecha1 WHEN  ;
       antes(2)
     @ 06, 48 GET fecha2 RANGE  ;
       fecha1
     @ 07, 18 GET w_opcion  ;
       DEFAULT 1 SIZE 1, 12, 0  ;
       PICTURE  ;
       '@*RHN Tipo Atenci¢n ; Modelo   '
     @ 08, 22 GET w_tipo PICTURE  ;
       '@m Detalle,Resumen'
     @ 09, 22 GET output PICTURE  ;
       '@m Impresora,Pantalla'
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     CREATE CURSOR gara (numdoc C  ;
            (8), numsol C (8),  ;
            fecemi D (8), fecfin  ;
            D (8), indori C (4),  ;
            codmar C (4), codmod  ;
            C (15), numser C (20),  ;
            codtec C (9), auxest  ;
            C (4), fecest D (8),  ;
            observ M (10),  ;
            codtall C (4),  ;
            tab_empres C (25),  ;
            entfac C (9), fecent  ;
            D (8), horent C (8),  ;
            codart C (4))
     SELECT st_iorep
     SET RELATION TO 'EMIS' + codemi INTO;
ge_tab0, codmar + codmod INTO st_imode;
ADDITIVE
     SET NEAR ON
     SEEK DTOS(fecha1)
     SET NEAR OFF
     STORE 0 TO w_reg
     SCAN WHILE fecent <= fecha2  ;
          .AND.  .NOT. EOF()
          IF indest <> 'N' .AND.  ;
             (indori >= tipgar1  ;
             .AND. indori <=  ;
             tipgar2) .AND.  ;
             (codemi >= emiini  ;
             .AND. codemi <=  ;
             emifin)
               SELECT gara
               APPEND BLANK
               REPLACE numdoc  ;
                       WITH  ;
                       st_iorep.numdoc,  ;
                       numsol  ;
                       WITH  ;
                       st_iorep.numsol,  ;
                       fecemi  ;
                       WITH  ;
                       st_iorep.fecemi,  ;
                       fecfin  ;
                       WITH  ;
                       st_iorep.fecfin,  ;
                       indori  ;
                       WITH  ;
                       st_iorep.indori,  ;
                       codmar  ;
                       WITH  ;
                       st_iorep.codmar,  ;
                       codmod  ;
                       WITH  ;
                       st_iorep.codmod,  ;
                       numser  ;
                       WITH  ;
                       st_iorep.numser,  ;
                       codtec  ;
                       WITH  ;
                       st_iorep.codtec,  ;
                       auxest  ;
                       WITH  ;
                       st_iorep.auxest,  ;
                       fecest  ;
                       WITH  ;
                       st_iorep.fecest,  ;
                       observ  ;
                       WITH  ;
                       st_iorep.observ,  ;
                       codtall  ;
                       WITH  ;
                       st_iorep.codtall,  ;
                       tab_empres  ;
                       WITH  ;
                       ge_tab0.tab_empres,  ;
                       entfac  ;
                       WITH  ;
                       st_iorep.entfac,  ;
                       fecent  ;
                       WITH  ;
                       st_iorep.fecent,  ;
                       horent  ;
                       WITH  ;
                       st_iorep.horent,  ;
                       codart  ;
                       WITH  ;
                       st_imode.codcla
               w_reg = w_reg + 1
          ENDIF
          SELECT st_iorep
     ENDSCAN
     IF w_reg = 0
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO error WITH  ;
             '*** No hay informaci¢n para el Reporte ***'
     ELSE
          SELECT gara
          IF w_opcion = 1
               index on indori+tab_empres+dtos(fecent)+entfac+numdoc;
to &filidx  
          ELSE
               index on codart+codmod+dtos(fecent)+entfac+numdoc;
to &filidx  
               SET RELATION TO codmar;
+ codmod INTO st_imode
          ENDIF
          GOTO TOP
          IF output = 'Impresora'
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               SET DEVICE TO PRINTER
               ??? CHR(15)
               IF w_tipo =  ;
                  'Resumen'
                    REPORT FORMAT  ;
                           PORL4128  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           PORL4128  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
               SET PRINTER TO
               SET DEVICE TO SCREEN
          ELSE
               filtxt = SYS(3) +  ;
                        '.TXT'
               IF w_tipo =  ;
                  'Resumen'
                    REPO FORM  PORL4128;
TO FILE &filtxt NOCONSOLE SUMMARY
               ELSE
                    REPO FORM  PORL4128;
TO FILE &filtxt NOCONSOLE 
               ENDIF
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               modi comm &filtxt window;
pantall noedit
               SET SYSMENU OFF
               dele file &filtxt
          ENDIF
     ENDIF
ENDDO
ON KEY
CLOSE DATABASES
DO sacawin
RETURN
*
FUNCTION valida
PARAMETER val, opc
DO CASE
     CASE opc = 1 .OR. opc = 2
          IF opc = 1
               IF LASTKEY() = 5  ;
                  .OR. LASTKEY() =  ;
                  19
                    ON KEY LABEL f6 do;
ayuda with 1
                    RETURN .F.
               ENDIF
          ENDIF
          IF EMPTY(val)
               ON KEY LABEL f6 do ayuda;
with 1
               DO error WITH  ;
                  ' *** No se Permiten Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'INGA' + val
          IF  .NOT. FOUND()
               ON KEY LABEL f6 do ayuda;
with 1
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          IF opc = 1
               @ ROW(), 23 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 18)
          ELSE
               @ ROW(), 53 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 18)
          ENDIF
     CASE opc = 3 .OR. opc = 4
          IF EMPTY(val)
               ON KEY LABEL f6 do ayuda;
with 3
               DO error WITH  ;
                  ' *** No se Permiten Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + val
          IF  .NOT. FOUND()
               ON KEY LABEL f6 do ayuda;
with 3
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          IF opc = 3
               @ ROW(), 23 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 18)
          ELSE
               @ ROW(), 53 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 18)
          ENDIF
ENDCASE
RETURN
*
PROCEDURE ayuda
PARAMETER opc
ON KEY
SELECT ge_tab0
DO CASE
     CASE opc = 1
          SET FILTER TO tab_codpre ==;
'INGA'
          titulo = 'AYUDA DE TIPO DE ATENCION'
     CASE opc = 3
          SET FILTER TO tab_codpre ==;
'EMIS'
          titulo = 'AYUDA DE EMISORES'
ENDCASE
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'ESC'
SET FILTER TO
RETURN
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
          ON KEY LABEL f6 do ayuda with;
1
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'ESC'
     CASE opc = 2
          ON KEY LABEL f6
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 3
          ON KEY LABEL f6 do ayuda with;
3
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'ESC'
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

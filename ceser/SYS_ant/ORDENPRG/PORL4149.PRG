*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_fecind
SELECT 3
USE ST_ITECN ORDER CODIGO
@ 02, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' TIEMPO DESDE O/R A CIERRE O/R x TECNICO '
STORE SPACE(4) TO tipgar1,  ;
      tipgar2, talle1, talle2,  ;
      w_codem1, w_codem2
STORE 0 TO w_tecini, w_tecfin
STORE DATE() TO fecha1, fecha2
STORE 'Detalle' TO tipo
STORE 'Impresora' TO output
STORE 0 TO mtot
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
     @ 04, 03 SAY 'Del Fecha :'
     @ 04, 39 SAY 'Al  Fecha :'
     @ 05, 03 SAY 'Del T?cni.:'
     @ 05, 39 SAY 'Al  T?cni.:'
     @ 06, 03 SAY 'Del T.Aten:'
     @ 06, 39 SAY 'Al  T.Aten:'
     @ 07, 03 SAY 'Del Emisor:'
     @ 07, 39 SAY 'Al  Emisor:'
     @ 08, 03 SAY 'Del Taller:'
     @ 08, 39 SAY 'Al  Taller:'
     @ 09, 03 SAY  ;
       'Detalle/Resumen   :'
     @ 10, 03 SAY  ;
       'Pantalla/Impresora:'
     @ 04, 16 GET fecha1 WHEN  ;
       antes(5)
     @ 04, 52 GET fecha2 RANGE  ;
       fecha1 WHEN antes(5)
     @ 05, 16 GET w_tecini  ;
       PICTURE '999999999' VALID  ;
       oovalid(w_tecini) WHEN  ;
       antes(1)
     @ 05, 52 GET w_tecfin RANGE  ;
       w_tecini PICTURE  ;
       '999999999' VALID  ;
       oovalid(w_tecfin) WHEN  ;
       antes(1)
     @ 06, 16 GET tipgar1 PICTURE  ;
       '@!' VALID valid2(tipgar1, ;
       22,'INGA') WHEN antes(2)
     @ 06, 52 GET tipgar2 RANGE  ;
       tipgar1 PICTURE '@!' VALID  ;
       valid2(tipgar2,57,'INGA')  ;
       WHEN antes(2)
     @ 07, 16 GET w_codem1  ;
       PICTURE '@!' VALID  ;
       valid2(w_codem1,22,'EMIS')  ;
       WHEN antes(3)
     @ 07, 52 GET w_codem2 RANGE  ;
       w_codem1 PICTURE '@!'  ;
       VALID valid2(w_codem2,57, ;
       'EMIS') WHEN antes(3)
     @ 08, 16 GET talle1 PICTURE  ;
       '@!' VALID valid2(talle1, ;
       22,'TALL') WHEN antes(4)
     @ 08, 52 GET talle2 RANGE  ;
       talle1 VALID valid2(talle2, ;
       57,'TALL') WHEN antes(4)
     @ 09, 22 GET tipo PICTURE  ;
       '@m Detalle,Resumen' WHEN  ;
       antes(5)
     @ 10, 22 GET output PICTURE  ;
       '@m Impresora,Pantalla'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
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
            codtall C (4), entfac  ;
            C (9), tab_empres C  ;
            (25))
     SELECT st_iorep
     SET RELATION TO 'EMIS' + codemi INTO;
ge_tab0
     SET NEAR ON
     SEEK DTOS(fecha1)
     SET NEAR OFF
     STORE 0 TO w_reg
     SCAN WHILE fecfin <= fecha2  ;
          .AND.  .NOT. EOF()
          IF indest <> 'N' .AND.  ;
             (indori >= tipgar1  ;
             .AND. indori <=  ;
             tipgar2) .AND.  ;
             (codtall >= talle1  ;
             .AND. codtall <=  ;
             talle2) .AND.  ;
             (codemi >= w_codem1  ;
             .AND. codemi <=  ;
             w_codem2) .AND.  ;
             (codtec >=  ;
             STR(w_tecini, 9)  ;
             .AND. codtec <=  ;
             STR(w_tecfin, 9))
               SELECT gara
               APPEND BLANK
               REPLACE numdoc  ;
                       WITH  ;
                       st_iorep.numdoc,  ;
                       numsol  ;
                       WITH  ;
                       st_iorep.numsol
               REPLACE fecemi  ;
                       WITH  ;
                       st_iorep.fecemi,  ;
                       fecfin  ;
                       WITH  ;
                       st_iorep.fecfin
               REPLACE indori  ;
                       WITH  ;
                       st_iorep.indori,  ;
                       codmar  ;
                       WITH  ;
                       st_iorep.codmar
               REPLACE codmod  ;
                       WITH  ;
                       st_iorep.codmod,  ;
                       numser  ;
                       WITH  ;
                       st_iorep.numser
               REPLACE codtec  ;
                       WITH  ;
                       st_iorep.codtec,  ;
                       auxest  ;
                       WITH  ;
                       st_iorep.auxest
               REPLACE fecest  ;
                       WITH  ;
                       st_iorep.fecest,  ;
                       observ  ;
                       WITH  ;
                       st_iorep.observ
               REPLACE codtall  ;
                       WITH  ;
                       st_iorep.codtall,  ;
                       tab_empres  ;
                       WITH  ;
                       ge_tab0.tab_empres
               REPLACE entfac  ;
                       WITH  ;
                       st_iorep.entfac
               w_reg = w_reg + 1
          ENDIF
          SELECT st_iorep
     ENDSCAN
     IF w_reg = 0
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO error WITH  ;
             '*** No hay informaci?n para el Reporte ***'
     ELSE
          = ptotal()
          filidx = SYS(3)
          SELECT gara
          index on codtec+indori+codtall+dtos(fecfin)+entfac+numdoc;
to &filidx  
          GOTO TOP
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          IF output = 'Impresora'
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               ??? CHR(15)
               IF tipo =  ;
                  'Resumen'
                    REPORT FORMAT  ;
                           PORL4149  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           PORL4149  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
               SET PRINTER TO
          ELSE
               DO mensa WITH  ;
                  '*** C o p i a n d o ... ***',  ;
                  'COLO'
               filtxt = SYS(3) +  ;
                        '.TXT'
               IF tipo =  ;
                  'Resumen'
                    REPO FORM  PORL4149;
TO FILE &filtxt NOCONSOLE SUMMARY
               ELSE
                    REPO FORM PORL4149;
TO FILE &filtxt NOCONSOLE 
               ENDIF
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               DO mensa WITH  ;
                  '*** C o p i a n d o ... ***',  ;
                  'SACA'
               modi comm &filtxt window;
pantall noedit
               SET SYSMENU OFF
               dele file &filtxt
          ENDIF
     ENDIF
ENDDO
ON KEY
DO sacawin
RETURN
*
FUNCTION valid2
PARAMETER w_codt, val, tal
IF EMPTY(w_codt)
     DO error WITH  ;
        ' *** No se Permiten Blancos ***'
     RETURN .F.
ENDIF
SELECT ge_tab0
SEEK tal + w_codt
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** C?digo de Tabla No Existe ***'
     RETURN .F.
ENDIF
@ ROW(), val SAY  ;
  SUBSTR(tab_destab, 1, 16)
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
'INGA'
          titulo = 'AYUDA DE GARANTIA'
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
     CASE opc2 = 4
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
          ON KEY LABEL F6 DO AYUDA WITH;
4
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 5
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
FUNCTION ptotal
SELECT st_iorep
mtot = 0
COUNT FOR (fecemi >= fecha1 .AND.  ;
      fecemi <= fecha2) .AND.  ;
      (indori >= tipgar1 .AND.  ;
      indori <= tipgar2) .AND.  ;
      (codtall >= talle1 .AND.  ;
      codtall <= talle2) .AND.  ;
      (codemi >= w_codem1 .AND.  ;
      codemi <= w_codem2) .AND.  ;
      (codtec >= STR(w_tecini, 9)  ;
      .AND. codtec <=  ;
      STR(w_tecfin, 9)) .AND.  ;
      indest <> 'N' TO mtot
SELECT gara
RETURN mtot
*
FUNCTION oovalid
PARAMETER w_tec
SELECT st_itecn
SEEK STR(w_tec, 9)
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** C?digo de T?cnico No Existe ***'
     RETURN .F.
ENDIF
@ ROW(), COL() + 1 SAY  ;
  SUBSTR(noment, 1, 12)
*
*** 
*** ReFox - retrace your steps ... 
***

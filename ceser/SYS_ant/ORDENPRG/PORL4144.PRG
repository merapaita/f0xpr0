*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
CLOSE DATABASES
SELECT 1
USE SHARED GE_TAB0 ORDER CODIGO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' Desde S/S Hasta Cierre por Emisor '
STORE SPACE(4) TO tipgar1,  ;
      tipgar2, wrk_estado
STORE DATE() TO fecha1, fecha2
STORE 'Detalle' TO tipo
STORE 'Impresora' TO output
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
     @ 04, 03 SAY 'Desde Emisor:'
     @ 05, 03 SAY 'Hasta Emisor:'
     @ 07, 03 SAY 'Estado      :'
     @ 08, 03 SAY  ;
       'Detalle/Res£men   :'
     @ 09, 03 SAY  ;
       'Pantalla/Impresora:'
     @ 04, 18 GET tipgar1 PICTURE  ;
       '@!' VALID valida(tipgar1, ;
       1) WHEN antes(1)
     @ 05, 18 GET tipgar2 RANGE  ;
       tipgar1 PICTURE '@!' VALID  ;
       valida(tipgar2,1)
     @ 07, 18 GET wrk_estado  ;
       PICTURE '@!' VALID  ;
       valida(wrk_estado,2) WHEN  ;
       antes(2)
     @ 08, 22 GET tipo PICTURE  ;
       '@m Detalle,Res£men' WHEN  ;
       antes(3)
     @ 09, 22 GET output PICTURE  ;
       '@m Impresora,Pantalla'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     IF tipo <> 'Res£men'
          SELECT DISTINCT  ;
                 st_iorep.numdoc,  ;
                 st_iorep.fecemi,  ;
                 st_iorep.fecfin,  ;
                 st_iorep.codemi,  ;
                 st_iorep.codent,  ;
                 st_iorep.indori,  ;
                 st_iorep.auxest,  ;
                 st_iorep.codmod,  ;
                 st_iorep.numser,  ;
                 st_iorep.numsol,  ;
                 st_iorep.fecest,  ;
                 st_isrep.fecemi  ;
                 FROM ST_ISREP,  ;
                 ST_IOREP,  ;
                 ST_MVORD WHERE  ;
                 st_iorep.numsol =  ;
                 st_isrep.numdoc  ;
                 AND  ;
                 st_mvord.orden =  ;
                 st_iorep.numdoc  ;
                 AND  ;
                 st_iorep.auxest =  ;
                 wrk_estado AND  ;
                 VAL(st_isrep.codemi) >=  ;
                 VAL(tipgar1) =  ;
                 .T. AND  ;
                 VAL(st_isrep.codemi) <=  ;
                 VAL(tipgar2) =  ;
                 .T. ORDER BY  ;
                 st_iorep.numsol  ;
                 INTO CURSOR  ;
                 porl4142
          xxsele = SELECT()
          DO usedbf WITH  ;
             'st_imode', 'CLASE'
          DO usedbf WITH  ;
             'GE_TAB0', 'CODIGO'
          DO usedbf WITH  ;
             'ST_MVORD',  ;
             'ESTADO'
          SET ORDER TO estado
          DO usedbf WITH  ;
             'ST_ICLPR',  ;
             'codigo'
          IF output = 'Impresora'
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               SELECT (xxsele)
               ??? CHR(15)
               REPORT FORMAT  ;
                      PORL4144 TO  ;
                      PRINTER  ;
                      NOCONSOLE
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
               SET PRINTER TO
          ELSE
               DO usedbf WITH  ;
                  'st_imode',  ;
                  'CLASE'
               DO usedbf WITH  ;
                  'GE_TAB0',  ;
                  'CODIGO'
               DO usedbf WITH  ;
                  'ST_MVORD',  ;
                  'ESTADO'
               SET ORDER TO estado
               DO usedbf WITH  ;
                  'ST_ICLPR',  ;
                  'codigo'
               SELECT (xxsele)
               REPORT FORMAT  ;
                      PORL4142 TO  ;
                      FILE  ;
                      text4.txt  ;
                      NOCONSOLE
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               MODIFY COMMAND  ;
                      text4.txt  ;
                      NOEDIT  ;
                      WINDOW  ;
                      pantall
               SET SYSMENU OFF
          ENDIF
     ELSE
          IF tipo = 'Res£men'
               SELECT DISTINCT  ;
                      st_iorep.numdoc,  ;
                      st_iorep.fecemi,  ;
                      st_iorep.fecfin,  ;
                      st_iorep.codemi,  ;
                      st_iorep.indori,  ;
                      st_iorep.auxest,  ;
                      st_iorep.codmod,  ;
                      st_iorep.numsol,  ;
                      st_iorep.fecest,  ;
                      st_isrep.fecemi  ;
                      FROM  ;
                      ST_ISREP,  ;
                      ST_IOREP,  ;
                      ST_MVORD  ;
                      WHERE  ;
                      st_iorep.numsol =  ;
                      st_isrep.numdoc  ;
                      AND  ;
                      st_mvord.orden =  ;
                      st_iorep.numdoc  ;
                      AND  ;
                      (st_isrep.fecemi >=  ;
                      fecha1 =  ;
                      .T. AND  ;
                      (st_isrep.fecemi) <=  ;
                      fecha2 =  ;
                      .T. AND  ;
                      VAL(st_isrep.codemi) >=  ;
                      VAL(tipgar1) =  ;
                      .T. AND  ;
                      VAL(st_isrep.codemi) <=  ;
                      VAL(tipgar2) =  ;
                      .T.) ORDER  ;
                      BY  ;
                      st_iorep.codemi,  ;
                      st_iorep.indori,  ;
                      st_iorep.codmod,  ;
                      st_isrep.fecemi  ;
                      INTO DBF  ;
                      MOVIMXR.DBF
               xxsele = SELECT()
               DO usedbf WITH  ;
                  'st_imode',  ;
                  'CLASE'
               DO usedbf WITH  ;
                  'GE_TAB0',  ;
                  'CODIGO'
               DO usedbf WITH  ;
                  'ST_MVORD',  ;
                  'ESTADO'
               SET ORDER TO estado
               IF output =  ;
                  'Impresora'
                    DO mensa WITH  ;
                       '** Un momento, Por Favor ... **',  ;
                       'SACA'
                    DO mensa WITH  ;
                       '*** I m p r i m i e n d o ... ***',  ;
                       'COLO'
                    SELECT (xxsele)
                    REPORT FORMAT  ;
                           porl4143.FRX  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
                    DO mensa WITH  ;
                       '*** I m p r i m i e n d o ... ***',  ;
                       'SACA'
                    SET PRINTER TO
               ELSE
                    DO usedbf  ;
                       WITH  ;
                       'st_imode',  ;
                       'CLASE'
                    DO usedbf  ;
                       WITH  ;
                       'GE_TAB0',  ;
                       'CODIGO'
                    DO usedbf  ;
                       WITH  ;
                       'ST_MVORD',  ;
                       'ESTADO'
                    SET ORDER TO estado
                    SELECT (xxsele)
                    REPORT FORMAT  ;
                           porl4143.FRX  ;
                           SUMMARY  ;
                           TO  ;
                           FILE  ;
                           text4.TXT  ;
                           NOCONSOLE
                    DO mensa WITH  ;
                       '** Un momento, Por Favor ... **',  ;
                       'SACA'
                    SET SYSMENU ON
                    KEYBOARD '{CTRL+F10}'
                    MODIFY COMMAND  ;
                           text4.txt  ;
                           NOEDIT  ;
                           WINDOW  ;
                           pantall
                    SET SYSMENU OFF
               ENDIF
          ENDIF
     ENDIF
ENDDO
CLOSE DATABASES
DO saca_win
RETURN
*
FUNCTION valida
PARAMETER wrk_codtab, val
DO CASE
     CASE val = 1
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
          SELECT ge_tab0
          SEEK 'EMIS' +  ;
               wrk_codtab
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          @ ROW(), 25 SAY  ;
            tab_destab
     CASE val = 2
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF LASTKEY() = 27
               RETURN
          ENDIF
          IF EMPTY(wrk_codtab)
               DO error WITH  ;
                  ' *** No se Permiten Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'ESOR' +  ;
               wrk_codtab
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          @ ROW(), 25 SAY  ;
            tab_destab
ENDCASE
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT ge_tab0
IF opc = 1
     SET FILTER TO tab_codpre == 'EMIS'
ELSE
     SET FILTER TO tab_codpre == 'ESOR'
ENDIF
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE TABLAS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'ESC'
RETURN
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          ON KEY LABEL F6 DO AYUDA WITH;
1
     CASE opc = 2
          ON KEY LABEL F6 DO AYUDA WITH;
2
     CASE opc = 3
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
ENDCASE
RETURN
*
FUNCTION dtoc2
PARAMETER wk_par
wk_aux = STR(YEAR(wk_par), 4) +  ;
         STR(MONTH(wk_par), 2) +  ;
         STR(DAY(wk_par), 2)
RETURN wk_aux
*
FUNCTION oofchest
PARAMETER wrk_numord, wrk_estado
narea = SELECT()
SELECT st_mvord
SEEK wrk_numord + wrk_estado
IF FOUND()
     wrk_fecest = st_mvord.dia
ELSE
     wrk_fecest = {}
ENDIF
SELECT (narea)
RETURN wrk_fecest
*
PROCEDURE usedbf
PARAMETER dbf, indice
IF  .NOT. USED(dbf)
     SELECT 0
     use &dbf order &indice shared
ELSE
     SELECT (dbf)
     set order to &indice shared
ENDIF
*
FUNCTION fec_sal
PARAMETER ordenx, tipo
fecha_sal = {}
xxsele = SELECT()
SELECT st_mvord
ON ERROR WAIT WINDOW " SE HA PRODUCIONO ERROR EN FEC_SAL"
DO CASE
     CASE tipo = 'FGAR'
          SEEK ordenx + '100 '
          IF FOUND()
               fecha_sal = dia
          ENDIF
          SEEK ordenx + '023 '
          IF FOUND() .AND. (dia <  ;
             fecha_sal)
               fecha_sal = dia
          ENDIF
     CASE tipo = 'GARA'
          SEEK ordenx + '080 '
          IF FOUND()
               fecha_sal = dia
          ENDIF
          SEEK ordenx + '023 '
          IF FOUND() .AND. (dia <  ;
             fecha_sal)
               fecha_sal = dia
          ENDIF
     CASE tipo = 'FREC' .OR. tipo =  ;
          'GREC'
          SEEK ordenx + '022 '
          IF FOUND()
               fecha_sal = dia
          ENDIF
ENDCASE
SELECT (xxsele)
ON ERROR
RETURN fecha_sal
*
FUNCTION oodescla
PARAMETER ccodcla
narea = SELECT()
SELECT st_imode
SEEK ALLTRIM(ccodcla)
IF FOUND()
     wrk_descla = st_imode.nommod
ELSE
     wrk_descla = 'no hay ' +  ;
                  ccodcla +  ;
                  SPACE(30)
ENDIF
SELECT (narea)
RETURN wrk_descla
*
FUNCTION oonord
PARAMETER wrk_orde
SELECT st_iorep
SEEK wrk_orde
IF FOUND()
     wrk_ord = st_iorep.numdoc
ELSE
     wrk_ord = SPACE(8)
ENDIF
RETURN wrk_ord
*
FUNCTION oodesmes
PARAMETER nummes
DO CASE
     CASE nummes = 1
          wrk_desmes = 'ENERO'
     CASE nummes = 2
          wrk_desmes = 'FEBRERO'
     CASE nummes = 3
          wrk_desmes = 'MARZO'
     CASE nummes = 4
          wrk_desmes = 'ABRIL'
     CASE nummes = 5
          wrk_desmes = 'MAYO'
     CASE nummes = 6
          wrk_desmes = 'JUNIO'
     CASE nummes = 7
          wrk_desmes = 'JULIO'
     CASE nummes = 8
          wrk_desmes = 'AGOSTO'
     CASE nummes = 9
          wrk_desmes = 'SETIEMBRE'
     CASE nummes = 10
          wrk_desmes = 'OCTUBRE'
     CASE nummes = 11
          wrk_desmes = 'NOVIEMBRE'
     CASE nummes = 12
          wrk_desmes = 'DICIEMBRE'
ENDCASE
RETURN wrk_desmes
*
FUNCTION ootab
PARAMETER var1, var2
narea = SELECT()
SELECT ge_tab0
SEEK var1 + var2
IF FOUND()
     wrk_despag = ge_tab0.tab_destab
ELSE
     wrk_despag = SPACE(30)
ENDIF
SELECT (narea)
RETURN wrk_despag
*
FUNCTION oodesemi
PARAMETER ccodemi
narea = SELECT()
wrk_busca = 'EMIS'
SELECT ge_tab0
SEEK wrk_busca + ccodemi
IF FOUND()
     wrk_desemi = ge_tab0.tab_destab
ELSE
     wrk_desemi = ' '
ENDIF
SELECT (narea)
RETURN wrk_desemi
*
FUNCTION oodescli
PARAMETER ccodcli
narea = SELECT()
wrk_busca = 'C'
SELECT st_iclpr
SEEK wrk_busca + ccodcli
IF FOUND()
     wrk_descli = st_iclpr.noment
ELSE
     wrk_descli = SPACE(30)
ENDIF
SELECT (narea)
RETURN wrk_descli
*
*** 
*** ReFox - retrace your steps ... 
***

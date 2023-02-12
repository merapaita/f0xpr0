*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL4132>'
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' SALIDA DE ARTICULOS POR FECHAS '
SELECT 1
USE SHARED ST_IOREP ORDER CODIGO
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 3
USE SHARED ST_MVORD ORDER  ;
    MVO_TECNIC
SET RELATION TO orden INTO st_iorep
SELECT 4
USE SHARED ST_ICLPR ORDER CODIGO
STORE 0 TO conto, nco
STORE SPACE(4) TO emisor1,  ;
      emisor2
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
STORE DATE() TO fecha1, fecha2
@ 3, 2 TO 9, 77
DO esc_modo WITH 'S'
STORE .T. TO sigue
DO WHILE sigue
     @ 4, 30 SAY SPACE(30)
     @ 4, 05 SAY SPACE(50)
     @ 8, 40 SAY SPACE(20)
     SET CURSOR ON
     @ 04, 03 SAY  ;
       'Desde Fecha  :' GET  ;
       fecha1 WHEN antes(2)
     @ 04, 30 SAY  ;
       'Hasta Fecha  :' GET  ;
       fecha2 RANGE fecha1 WHEN  ;
       antes(2)
     @ 05, 03 SAY  ;
       'Desde Emisor :' GET  ;
       emisor1 PICTURE '@!' VALID  ;
       valida(emisor1,1) WHEN  ;
       antes(1)
     @ 06, 03 SAY  ;
       'Hasta Emisor :' GET  ;
       emisor2 RANGE emisor1  ;
       PICTURE '@!' VALID  ;
       valida(emisor2,1) WHEN  ;
       antes(1)
     @ 07, 03 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'  ;
       WHEN antes(3)
     @ 08, 03 SAY  ;
       'Por Detalle/Res£men   :'  ;
       GET tipo PICTURE  ;
       '@m Detalle,Res£men' WHEN  ;
       antes(3)
     READ
     IF LASTKEY() = 27
          sigue = .F.
          LOOP
     ENDIF
     IF output = 'Impresora'
          nco = 1
          @ 08, 40 SAY  ;
            'Copias Nro.:' GET  ;
            nco PICTURE '99'  ;
            VALID nco >= 0
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     CREATE CURSOR SALIDA (orden  ;
            C (8), fecemi D,  ;
            numsol C (8), codemi  ;
            C (4), codmar C (4),  ;
            dia D, limpia C (1),  ;
            codmod C (20), numser  ;
            C (20), codent C (11),  ;
            indori C (4), codtec  ;
            C (9), codtall C (4),  ;
            estado C (4))
     wrk_file = SUBSTR(f_indice(),  ;
                1, 8) + '.IDX'
     index on orden to &WRK_FILE
     SELECT st_mvord
     SET ORDER TO mvo_tecnic
     SET NEAR ON
     SEEK '020 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE (estado = '020 ')  ;
          .AND. (dia >= fecha1  ;
          .AND. dia <= fecha2)  ;
          .AND.  .NOT. EOF()
          IF FOUND() .AND.  ;
             (st_iorep.codtall <  ;
             '010 ' .OR.  ;
             (st_iorep.codtall >  ;
             '050 ' .AND.  ;
             st_iorep.codtall <  ;
             '060 ')) .AND.  ;
             st_iorep.codemi >=  ;
             emisor1 .AND.  ;
             st_iorep.codemi <=  ;
             emisor2
               SELECT salida
               APPEND BLANK
               REPLACE orden WITH  ;
                       st_mvord.orden,  ;
                       dia WITH  ;
                       st_mvord.dia,  ;
                       numsol  ;
                       WITH  ;
                       st_iorep.numsol,  ;
                       codmar  ;
                       WITH  ;
                       st_iorep.codmar
               REPLACE codmod  ;
                       WITH  ;
                       st_iorep.codmod,  ;
                       numser  ;
                       WITH  ;
                       st_iorep.numser,  ;
                       codtec  ;
                       WITH  ;
                       st_iorep.codtec,  ;
                       codent  ;
                       WITH  ;
                       st_iorep.codent
               REPLACE indori  ;
                       WITH  ;
                       st_iorep.indori,  ;
                       codtall  ;
                       WITH  ;
                       st_iorep.codtall,  ;
                       fecemi  ;
                       WITH  ;
                       st_iorep.fecemi
               REPLACE codemi  ;
                       WITH  ;
                       st_iorep.codemi,  ;
                       estado  ;
                       WITH  ;
                       st_mvord.estado
          ENDIF
          SELECT st_mvord
     ENDSCAN
     SET NEAR ON
     SEEK '022 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE estado +  ;
          DTOS(dia) <= '022 ' +  ;
          DTOS(fecha2) .AND.   ;
          .NOT. EOF()
          IF (st_iorep.codtall <  ;
             '010 ' .OR.  ;
             (st_iorep.codtall >  ;
             '050 ' .AND.  ;
             st_iorep.codtall <  ;
             '060 ')) .AND.  ;
             st_iorep.codemi >=  ;
             emisor1 .AND.  ;
             st_iorep.codemi <=  ;
             emisor2
               SELECT salida
               SEEK st_mvord.orden
               IF  .NOT. FOUND()
                    APPEND BLANK
                    REPLACE orden  ;
                            WITH  ;
                            st_mvord.orden,  ;
                            dia  ;
                            WITH  ;
                            st_mvord.dia,  ;
                            numsol  ;
                            WITH  ;
                            st_iorep.numsol,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar
                    REPLACE codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            numser  ;
                            WITH  ;
                            st_iorep.numser,  ;
                            codtec  ;
                            WITH  ;
                            st_iorep.codtec,  ;
                            codent  ;
                            WITH  ;
                            st_iorep.codent
                    REPLACE indori  ;
                            WITH  ;
                            st_iorep.indori,  ;
                            codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            fecemi  ;
                            WITH  ;
                            st_iorep.fecemi
                    REPLACE codemi  ;
                            WITH  ;
                            st_iorep.codemi,  ;
                            estado  ;
                            WITH  ;
                            st_mvord.estado
               ENDIF
               SELECT st_mvord
          ENDIF
     ENDSCAN
     SET NEAR ON
     SEEK '023 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE estado +  ;
          DTOS(dia) <= '023 ' +  ;
          DTOS(fecha2) .AND.   ;
          .NOT. EOF()
          IF (st_iorep.codtall <  ;
             '010 ' .OR.  ;
             (st_iorep.codtall >  ;
             '050 ' .AND.  ;
             st_iorep.codtall <  ;
             '060 ')) .AND.  ;
             st_iorep.codemi >=  ;
             emisor1 .AND.  ;
             st_iorep.codemi <=  ;
             emisor2
               SELECT salida
               SEEK st_mvord.orden
               IF  .NOT. FOUND()
                    APPEND BLANK
                    REPLACE orden  ;
                            WITH  ;
                            st_mvord.orden,  ;
                            dia  ;
                            WITH  ;
                            st_mvord.dia,  ;
                            numsol  ;
                            WITH  ;
                            st_iorep.numsol,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar
                    REPLACE codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            numser  ;
                            WITH  ;
                            st_iorep.numser,  ;
                            codtec  ;
                            WITH  ;
                            st_iorep.codtec,  ;
                            codent  ;
                            WITH  ;
                            st_iorep.codent
                    REPLACE indori  ;
                            WITH  ;
                            st_iorep.indori,  ;
                            codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            fecemi  ;
                            WITH  ;
                            st_iorep.fecemi
                    REPLACE codemi  ;
                            WITH  ;
                            st_iorep.codemi,  ;
                            estado  ;
                            WITH  ;
                            st_mvord.estado
               ENDIF
               SELECT st_mvord
          ENDIF
     ENDSCAN
     SET NEAR ON
     SEEK '024 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE estado +  ;
          DTOS(dia) <= '024 ' +  ;
          DTOS(fecha2) .AND.   ;
          .NOT. EOF()
          IF (st_iorep.codtall <  ;
             '010 ' .OR.  ;
             (st_iorep.codtall >  ;
             '050 ' .AND.  ;
             st_iorep.codtall <  ;
             '060 ')) .AND.  ;
             st_iorep.codemi >=  ;
             emisor1 .AND.  ;
             st_iorep.codemi <=  ;
             emisor2
               SELECT salida
               SEEK st_mvord.orden
               IF  .NOT. FOUND()
                    APPEND BLANK
                    REPLACE orden  ;
                            WITH  ;
                            st_mvord.orden,  ;
                            dia  ;
                            WITH  ;
                            st_mvord.dia,  ;
                            numsol  ;
                            WITH  ;
                            st_iorep.numsol,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar
                    REPLACE codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            numser  ;
                            WITH  ;
                            st_iorep.numser,  ;
                            codtec  ;
                            WITH  ;
                            st_iorep.codtec,  ;
                            codent  ;
                            WITH  ;
                            st_iorep.codent
                    REPLACE indori  ;
                            WITH  ;
                            st_iorep.indori,  ;
                            codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            fecemi  ;
                            WITH  ;
                            st_iorep.fecemi
                    REPLACE codemi  ;
                            WITH  ;
                            st_iorep.codemi,  ;
                            estado  ;
                            WITH  ;
                            st_mvord.estado
               ENDIF
               SELECT st_mvord
          ENDIF
     ENDSCAN
     SET NEAR ON
     SEEK '025 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE estado +  ;
          DTOS(dia) <= '025 ' +  ;
          DTOS(fecha2) .AND.   ;
          .NOT. EOF()
          IF (st_iorep.codtall <  ;
             '010 ' .OR.  ;
             (st_iorep.codtall >  ;
             '050 ' .AND.  ;
             st_iorep.codtall <  ;
             '060 ')) .AND.  ;
             st_iorep.codemi >=  ;
             emisor1 .AND.  ;
             st_iorep.codemi <=  ;
             emisor2
               SELECT salida
               SEEK st_mvord.orden
               IF  .NOT. FOUND()
                    APPEND BLANK
                    REPLACE orden  ;
                            WITH  ;
                            st_mvord.orden,  ;
                            dia  ;
                            WITH  ;
                            st_mvord.dia,  ;
                            numsol  ;
                            WITH  ;
                            st_iorep.numsol,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar
                    REPLACE codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            numser  ;
                            WITH  ;
                            st_iorep.numser,  ;
                            codtec  ;
                            WITH  ;
                            st_iorep.codtec,  ;
                            codent  ;
                            WITH  ;
                            st_iorep.codent
                    REPLACE indori  ;
                            WITH  ;
                            st_iorep.indori,  ;
                            codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            fecemi  ;
                            WITH  ;
                            st_iorep.fecemi
                    REPLACE codemi  ;
                            WITH  ;
                            st_iorep.codemi,  ;
                            estado  ;
                            WITH  ;
                            st_mvord.estado
               ENDIF
               SELECT st_mvord
          ENDIF
     ENDSCAN
     SET NEAR ON
     SEEK '030 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE estado +  ;
          DTOS(dia) <= '030 ' +  ;
          DTOS(fecha2) .AND.   ;
          .NOT. EOF()
          IF (st_iorep.codtall <  ;
             '010 ' .OR.  ;
             (st_iorep.codtall >  ;
             '050 ' .AND.  ;
             st_iorep.codtall <  ;
             '060 ')) .AND.  ;
             st_iorep.codemi >=  ;
             emisor1 .AND.  ;
             st_iorep.codemi <=  ;
             emisor2
               SELECT salida
               SEEK st_mvord.orden
               IF  .NOT. FOUND()
                    APPEND BLANK
                    REPLACE orden  ;
                            WITH  ;
                            st_mvord.orden,  ;
                            dia  ;
                            WITH  ;
                            st_mvord.dia,  ;
                            numsol  ;
                            WITH  ;
                            st_iorep.numsol,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar
                    REPLACE codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            numser  ;
                            WITH  ;
                            st_iorep.numser,  ;
                            codtec  ;
                            WITH  ;
                            st_iorep.codtec,  ;
                            codent  ;
                            WITH  ;
                            st_iorep.codent
                    REPLACE indori  ;
                            WITH  ;
                            st_iorep.indori,  ;
                            codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            fecemi  ;
                            WITH  ;
                            st_iorep.fecemi
                    REPLACE codemi  ;
                            WITH  ;
                            st_iorep.codemi,  ;
                            estado  ;
                            WITH  ;
                            st_mvord.estado
               ENDIF
               SELECT st_mvord
          ENDIF
     ENDSCAN
     SET NEAR ON
     SEEK '100 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE estado +  ;
          DTOS(dia) <= '100 ' +  ;
          DTOS(fecha2) .AND.   ;
          .NOT. EOF()
          IF (st_iorep.codtall <  ;
             '010 ' .OR.  ;
             (st_iorep.codtall >  ;
             '050 ' .AND.  ;
             st_iorep.codtall <  ;
             '060 ')) .AND.  ;
             st_iorep.codemi >=  ;
             emisor1 .AND.  ;
             st_iorep.codemi <=  ;
             emisor2
               SELECT salida
               SEEK st_mvord.orden
               IF  .NOT. FOUND()
                    APPEND BLANK
                    REPLACE orden  ;
                            WITH  ;
                            st_mvord.orden,  ;
                            dia  ;
                            WITH  ;
                            st_mvord.dia,  ;
                            numsol  ;
                            WITH  ;
                            st_iorep.numsol,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar
                    REPLACE codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            numser  ;
                            WITH  ;
                            st_iorep.numser,  ;
                            codtec  ;
                            WITH  ;
                            st_iorep.codtec,  ;
                            codent  ;
                            WITH  ;
                            st_iorep.codent
                    REPLACE indori  ;
                            WITH  ;
                            st_iorep.indori,  ;
                            codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            fecemi  ;
                            WITH  ;
                            st_iorep.fecemi
                    REPLACE codemi  ;
                            WITH  ;
                            st_iorep.codemi,  ;
                            estado  ;
                            WITH  ;
                            st_mvord.estado
               ENDIF
               SELECT st_mvord
          ENDIF
     ENDSCAN
     SELECT st_mvord
     SET ORDER TO estado
     SELECT salida
     GOTO TOP
     SCAN WHILE  .NOT. EOF()
          IF salida.estado =  ;
             '100 '
               SELECT st_mvord
               SEEK salida.orden +  ;
                    '020 '
               IF FOUND() .AND.  ;
                  salida.dia <>  ;
                  st_mvord.dia
                    SELECT salida
                    REPLACE limpia  ;
                            WITH  ;
                            'S'
               ENDIF
               SELECT st_mvord
               SEEK salida.orden +  ;
                    '022 '
               IF FOUND() .AND.  ;
                  salida.dia <>  ;
                  st_mvord.dia
                    SELECT salida
                    REPLACE limpia  ;
                            WITH  ;
                            'S'
               ENDIF
               SELECT st_mvord
               SEEK salida.orden +  ;
                    '023 '
               IF FOUND() .AND.  ;
                  salida.dia <>  ;
                  st_mvord.dia
                    SELECT salida
                    REPLACE limpia  ;
                            WITH  ;
                            'S'
               ENDIF
               SELECT st_mvord
               SEEK salida.orden +  ;
                    '024 '
               IF FOUND() .AND.  ;
                  salida.dia <>  ;
                  st_mvord.dia
                    SELECT salida
                    REPLACE limpia  ;
                            WITH  ;
                            'S'
               ENDIF
               SELECT st_mvord
               SEEK salida.orden +  ;
                    '024 '
               IF FOUND() .AND.  ;
                  salida.dia <>  ;
                  st_mvord.dia
                    SELECT salida
                    REPLACE limpia  ;
                            WITH  ;
                            'S'
               ENDIF
               SELECT st_mvord
               SEEK salida.orden +  ;
                    '025 '
               IF FOUND() .AND.  ;
                  salida.dia <>  ;
                  st_mvord.dia
                    SELECT salida
                    REPLACE limpia  ;
                            WITH  ;
                            'S'
               ENDIF
               SELECT st_mvord
               SEEK salida.orden +  ;
                    '030 '
               IF FOUND() .AND.  ;
                  salida.dia <>  ;
                  st_mvord.dia
                    SELECT salida
                    REPLACE limpia  ;
                            WITH  ;
                            'S'
               ENDIF
          ENDIF
          SELECT salida
     ENDSCAN
     DELETE ALL FOR limpia = 'S'
     index on dtos(dia)+codemi+indori+orden;
to &WRK_FILE
     SET RELATION TO 'C' + codent INTO;
st_iclpr
     GOTO TOP
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          SET DEVICE TO PRINTER
          SET PRINTER ON
          ?? CHR(27) + CHR(15)
          FOR i = 1 TO nco
               IF tipo =  ;
                  'Res£men'
                    REPORT FORMAT  ;
                           porl4132  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           porl4132  ;
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
          filtxt = SYS(3) +  ;
                   '.TXT'
          IF tipo = 'Res£men'
               repo form porl4132 to file;
&filtxt noconsole summary
          ELSE
               repo form porl4132 to file;
&filtxt noconsole 
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &filtxt window pantall;
noedit
          dele file &FILTXT
          SET SYSMENU OFF
     ENDIF
ENDDO
CLOSE DATABASES
DO sacawin
RETURN
*
FUNCTION valida
PARAMETER wrk_codtab, val
IF EMPTY(wrk_codtab)
     DO error WITH  ;
        ' *** No se Permiten Blancos ***'
     RETURN .F.
ENDIF
SELECT ge_tab0
SEEK 'EMIS' + wrk_codtab
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** C¢digo de Tabla No Existe ***'
     RETURN .F.
ENDIF
@ ROW(), 25 SAY tab_destab
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT ge_tab0
SET FILTER TO tab_codpre == 'EMIS'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE EMISOR'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'ESC'
SET FILTER TO
RETURN
*
PROCEDURE antes
PARAMETER opci
DO CASE
     CASE opci = 1
          ON KEY LABEL f6 do ayuda
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opci = 2
          ON KEY LABEL f6
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opci = 3
          ON KEY LABEL f6
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
RETURN
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
*** 
*** ReFox - retrace your steps ... 
***

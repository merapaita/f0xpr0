*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = PROGRAM()
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' PRODUCCION MENSUAL POR TECNICO '
@ 3, 2 TO 8, 77
CLOSE DATABASES
SELECT 2
USE SHARED st_iorep ORDER codigo
SELECT 1
USE SHARED st_mvord ORDER  ;
    mvo_tecnic
SET RELATION TO orden INTO st_iorep
SELECT 3
USE SHARED st_itecn ORDER codigo
SELECT 4
CREATE CURSOR produc (dia D (8),  ;
       tecnico C (9), estado C  ;
       (4), orden C (9), codtall  ;
       C (4), codmar C (4),  ;
       codmod C (20), numser C  ;
       (20), observ M (10))
SET RELATION TO tecnico INTO st_itecn
SELECT 7
USE SHARED ge_tab0 ORDER codigo
STORE DATE() TO fecha1, fecha2
STORE 'Detalle' TO tipo
STORE 'Impresora' TO output
STORE '005 ' TO talle1, talle2
ON KEY LABEL f6 do ayuda
@ 7, 1 CLEAR TO 13, 77
@ 3, 2 TO 8, 77
DO WHILE .T.
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 05 SAY 'Desde Taller:'
     @ 04, 40 SAY 'Hasta:'
     @ 05, 05 SAY 'Desde Fecha :'
     @ 05, 40 SAY 'Hasta:'
     @ 04, 18 GET talle1 VALID  ;
       valtab('TALL',talle1,22, ;
       18) WHEN colocaf6()
     @ 04, 47 GET talle2 RANGE  ;
       talle1 VALID valtab('TALL', ;
       talle2,51,18) WHEN  ;
       colocaf6()
     @ 05, 18 GET fecha1
     @ 05, 47 GET fecha2 RANGE  ;
       fecha1
     @ 06, 05 SAY  ;
       'Por Detalle/Res£men:' GET  ;
       tipo PICTURE  ;
       '@m Detalle,Res£men'
     @ 07, 05 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Impresora,Pantalla'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT produc
     wrk_file = SUBSTR(f_indice(),  ;
                1, 8) + '.IDX'
     SELECT produc
     DELETE ALL
     INDEX ON TECNICO+ORDEN TO &WRK_FILE
     SELECT st_mvord
     SET NEAR ON
     SEEK '010 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE estado = '010 '  ;
          .AND. dia <= fecha2  ;
          .AND.  .NOT. EOF()
          IF (st_iorep.codtall >=  ;
             talle1 .AND.  ;
             st_iorep.codtall <=  ;
             talle2)
               SELECT produc
               IF orden <>  ;
                  st_mvord.orden
                    APPEND BLANK
                    REPLACE dia  ;
                            WITH  ;
                            st_mvord.dia
                    REPLACE tecnico  ;
                            WITH  ;
                            st_mvord.tecnico
                    REPLACE estado  ;
                            WITH  ;
                            st_mvord.estado
                    REPLACE orden  ;
                            WITH  ;
                            st_mvord.orden
                    REPLACE codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar,  ;
                            codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            observ  ;
                            WITH  ;
                            st_iorep.observ
               ENDIF
               SELECT st_mvord
          ENDIF
     ENDSCAN
     SET NEAR ON
     SEEK '021 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE estado = '021'  ;
          .AND. dia <= fecha2  ;
          .AND.  .NOT. EOF()
          IF (st_iorep.codtall >=  ;
             talle1 .AND.  ;
             st_iorep.codtall <=  ;
             talle2)
               SELECT produc
               SEEK st_mvord.tecnico +  ;
                    st_mvord.orden
               IF  .NOT. FOUND()
                    APPEND BLANK
                    REPLACE dia  ;
                            WITH  ;
                            st_mvord.dia
                    REPLACE tecnico  ;
                            WITH  ;
                            st_mvord.tecnico
                    REPLACE estado  ;
                            WITH  ;
                            st_mvord.estado
                    REPLACE orden  ;
                            WITH  ;
                            st_mvord.orden
                    REPLACE codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar,  ;
                            codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            observ  ;
                            WITH  ;
                            st_iorep.observ
               ENDIF
               SELECT st_mvord
          ENDIF
     ENDSCAN
     SET NEAR ON
     SEEK '018 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE estado = '018'  ;
          .AND. dia <= fecha2  ;
          .AND.  .NOT. EOF()
          IF (st_iorep.codtall >=  ;
             talle1 .AND.  ;
             st_iorep.codtall <=  ;
             talle2)
               SELECT produc
               SEEK st_mvord.tecnico +  ;
                    st_mvord.orden
               IF  .NOT. FOUND()
                    APPEND BLANK
                    REPLACE dia  ;
                            WITH  ;
                            st_mvord.dia
                    REPLACE tecnico  ;
                            WITH  ;
                            st_mvord.tecnico
                    REPLACE estado  ;
                            WITH  ;
                            st_mvord.estado
                    REPLACE orden  ;
                            WITH  ;
                            st_mvord.orden
                    REPLACE codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar,  ;
                            codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            observ  ;
                            WITH  ;
                            st_iorep.observ
               ENDIF
               SELECT st_mvord
          ENDIF
     ENDSCAN
     SET NEAR ON
     SEEK '026 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE estado = '026'  ;
          .AND. dia <= fecha2  ;
          .AND.  .NOT. EOF()
          IF (st_iorep.codtall >=  ;
             talle1 .AND.  ;
             st_iorep.codtall <=  ;
             talle2)
               SELECT produc
               SEEK st_mvord.tecnico +  ;
                    st_mvord.orden
               IF  .NOT. FOUND()
                    APPEND BLANK
                    REPLACE dia  ;
                            WITH  ;
                            st_mvord.dia
                    REPLACE tecnico  ;
                            WITH  ;
                            st_mvord.tecnico
                    REPLACE estado  ;
                            WITH  ;
                            st_mvord.estado
                    REPLACE orden  ;
                            WITH  ;
                            st_mvord.orden
                    REPLACE codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar,  ;
                            codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            observ  ;
                            WITH  ;
                            st_iorep.observ
               ENDIF
               SELECT st_mvord
          ENDIF
     ENDSCAN
     SET NEAR ON
     SEEK '027 ' + DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE estado = '027'  ;
          .AND. dia <= fecha2  ;
          .AND.  .NOT. EOF()
          IF (st_iorep.codtall >=  ;
             talle1 .AND.  ;
             st_iorep.codtall <=  ;
             talle2)
               SELECT produc
               SEEK st_mvord.tecnico +  ;
                    st_mvord.orden
               IF  .NOT. FOUND()
                    APPEND BLANK
                    REPLACE dia  ;
                            WITH  ;
                            st_mvord.dia
                    REPLACE tecnico  ;
                            WITH  ;
                            st_mvord.tecnico
                    REPLACE estado  ;
                            WITH  ;
                            st_mvord.estado
                    REPLACE orden  ;
                            WITH  ;
                            st_mvord.orden
                    REPLACE codtall  ;
                            WITH  ;
                            st_iorep.codtall,  ;
                            codmar  ;
                            WITH  ;
                            st_iorep.codmar,  ;
                            codmod  ;
                            WITH  ;
                            st_iorep.codmod,  ;
                            observ  ;
                            WITH  ;
                            st_iorep.observ
               ENDIF
               SELECT st_mvord
          ENDIF
     ENDSCAN
     SELECT produc
     IF tipo = 'Detalle'
          INDEX ON CODTALL+ESTADO+TECNICO+DTOS(DIA)+ORDEN;
TO &WRK_FILE
     ELSE
          INDEX ON CODTALL+ESTADO+DTOS(DIA)+TECNICO+ORDEN;
TO &WRK_FILE
     ENDIF
     GOTO TOP
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          ??? CHR(15)
          IF tipo = 'Detalle'
               REPORT FORMAT  ;
                      porl4120 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      porl4137  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          SET PRINTER TO
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          wrk_fil2 = SUBSTR(f_texto(),  ;
                     1, 8) +  ;
                     '.TXT'
          IF tipo = 'Detalle'
               REPO FORM porl4120 TO file;
&wrk_fil2 NOCONSOLE
          ELSE
               REPO FORM porl4137 to file;
&wrk_fil2 noconsole summary
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &wrk_fil2 window pantall;
noedit
          DELE FILE &wrk_fil2
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
CLOSE DATABASES
if file('&wrk_file')
     DELE FILE &wrk_file
ENDIF
ON KEY LABEL f6
RETURN
*
PROCEDURE antes
PARAMETER opc
DO esc_indica WITH 1, 'AYU',  ;
   'BUS', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
RETURN
*
PROCEDURE ayuda
SELECT ge_tab0
SET FILTER TO tab_codpre == 'TALL'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE TALLER'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
RETURN
*
FUNCTION f_texto
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'TEMPO' +  ;
           LTRIM(STR(cor))
     IF FILE(ret + '.TXT')
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
FUNCTION f_indice
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'TEMPO' +  ;
           LTRIM(STR(cor))
     IF FILE(ret + '.IDX')
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
FUNCTION valtab
PARAMETER clave, codig, colu,  ;
          largo
IF EMPTY(codig)
     RETURN .F.
ENDIF
SELECT ge_tab0
codaux = clave + codig
SEEK '&codaux'
IF  .NOT. FOUND()
     DO error WITH  ;
        '** Codigo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
ENDIF
DO sacaf6
RETURN .T.
*
*** 
*** ReFox - retrace your steps ... 
***

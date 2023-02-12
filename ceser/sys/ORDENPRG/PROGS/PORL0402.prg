*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0402>'
tit_prg = ' INFORMES '
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INFORME DE MODELOS '
SELECT 1
USE st_imode ORDER CODIGO
SELECT 2
USE GE_TAB0 ORDER CODIGO
DO WHILE .T.
     ON KEY LABEL F6 DO AYUDA01
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     STORE SPACE(4) TO wk_codmar,  ;
           wk_codmar2
     @ 03, 02 CLEAR TO 07, 76
     @ 03, 02 TO 07, 76
     @ 04, 05 SAY  ;
       'Desde La  Marca :'
     @ 06, 05 SAY  ;
       'Hasta La  Marca :'
     @ 4, 25 GET wk_codmar  ;
       PICTURE '@!' VALID  ;
       entra1_1(wk_codmar,'MARC')  ;
       WHEN colocaf6()
     @ 6, 25 GET wk_codmar2  ;
       PICTURE '@!' VALID  ;
       entra2_2(wk_codmar2, ;
       wk_codmar,'MARC') WHEN  ;
       colocaf6()
     SET CURSOR ON
     READ
     SET CURSOR OFF
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_imode.codmar,  ;
            st_imode.codmod,  ;
            st_imode.nommod,  ;
            st_imode.codcla,  ;
            st_imode.mesgar,  ;
            st_imode.codent,  ;
            st_imode.linea FROM  ;
            ST_IMODE WHERE  ;
            BETWEEN(st_imode.codmar,  ;
            wk_codmar,  ;
            wk_codmar2) ORDER BY  ;
            st_imode.codmar,  ;
            st_imode.codmod INTO  ;
            CURSOR QUERY
     COUNT TO wrk_totreg
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     IF wrk_totreg = 0
          DO error WITH  ;
             '*** No Existen Registros a Listar ***'
          LOOP
     ENDIF
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'IMP', 'VER', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     DO WHILE LASTKEY()<>27 .AND.  ;
        LASTKEY()<>-6 .AND.  ;
        LASTKEY()<>-4
          key = INKEY(0, 'H')
     ENDDO
     IF LASTKEY() = 27
          LOOP
     ENDIF
     IF key == -6
          sw_impre = 0
          DO impresora WITH  ;
             sw_impre
          IF sw_impre <> 1
               LOOP
          ENDIF
          DO esc_modo WITH 'P'
          DO mensa WITH  ;
             '***  I M P R I M I E N D O  ***',  ;
             'SACA'
          SET DEVICE TO PRINTER
          SET CONSOLE OFF
          SET PRINTER ON
          ??? CHR(27) + CHR(15)
          REPORT FORMAT PORL0402  ;
                 TO PRINTER  ;
                 NOCONSOLE
          ??? CHR(27) + CHR(80)
          SET PRINTER OFF
          SET PRINTER TO
          SET CONSOLE ON
          SET DEVICE TO SCREEN
          DO mensa WITH  ;
             '***  I M P R I M I E N D O  ***',  ;
             'SACA'
     ENDIF
     IF key == -4
          DO esc_indica WITH 1,  ;
             'AYU', 'ARR', 'ABA',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'ANT', 'STE',  ;
             'ESC'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'COLO'
          wrk_archi = f_archivo()
          wrk_file = SUBSTR(wrk_archi,  ;
                     1, 8) +  ;
                     '.TXT'
          REPO FORM PORL0402 TO FILE;
 &wrk_file  NOCONSOLE
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODI COMM  &wrk_file;
 NOEDIT WIND PANTALL
          SET SYSMENU OFF
          ERASE FILE &wrk_file
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
     ENDIF
ENDDO
DO saca_win
ON KEY LABEL F6
ON KEY LABEL F10
*
PROCEDURE ayuda01
ON KEY LABEL F6
IF ROW() == 4
     SELECT 2
     SET FILTER TO tab_codpre == 'MARC'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE MARCAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF ROW() == 6
     SELECT 2
     SET FILTER TO tab_codpre == 'MARC'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE MARCAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
ON KEY LABEL F6 do ayuda01
RETURN
*
FUNCTION entra1_1
PARAMETER cod, cc
SELECT 2
IF cod == SPACE(4)
     entaux = cc
     SEEK '&ENTAUX'
     aux = tab_codtab
     KEYBOARD '{CTRL+Y}' + aux  ;
              PLAIN
     RETURN .F.
ENDIF
entaux = cc + cod
SEEK '&ENTAUX'
IF  .NOT. FOUND()
     ACTIVATE WINDOW trabajo
     DO error WITH  ;
        '** C¢digo No Existente **'
     RETURN .F.
ENDIF
@ 4, 30 SAY SUBSTR(tab_destab, 1,  ;
  40)
SELECT 2
RETURN .T.
*
FUNCTION entra2_2
PARAMETER cod, codx, cc
IF LASTKEY() == 5
     RETURN .T.
ENDIF
SELECT 2
IF cod == SPACE(4)
     entaux = cc
     SEEK '&ENTAUX'
     DO WHILE tab_codpre==cc
          SKIP
     ENDDO
     SKIP -1
     aux = tab_codtab
     KEYBOARD '{CTRL+Y}' + aux  ;
              PLAIN
     RETURN .F.
ENDIF
entaux = cc + cod
SEEK '&ENTAUX'
IF  .NOT. FOUND()
     ACTIVATE WINDOW trabajo
     DO error WITH  ;
        '** C¢digo No Existente **'
     RETURN .F.
ENDIF
IF cod < codx
     DO error WITH  ;
        '** C¢digo debe ser Mayor **'
     RETURN .F.
ENDIF
@ 6, 30 SAY SUBSTR(tab_destab, 1,  ;
  40)
SELECT 2
RETURN .T.
*
*** 
*** ReFox - retrace your steps ... 
***

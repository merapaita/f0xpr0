*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0404>'
tit_prg = 'INFORMES'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA02
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' CLASIFICACION DE TECNICOS'
@ 3, 2 CLEAR TO 7, 76
@ 3, 2 TO 7, 76
@ 4, 5 SAY  ;
  'Desde El T‚cnico      :'
@ 5, 5 SAY  ;
  'Hasta El T‚cnico      :'
@ 6, 5 SAY  ;
  'Por Pantalla/Impresora:'
CLOSE DATABASES
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 4
USE SHARED st_itecn ORDER codigo
ppas = .T.
STORE 0 TO wk_code1, wk_code2,  ;
      cod
STORE 'Impresora' TO output
DO WHILE ppas
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     @ 4, 30 GET wk_code1 PICTURE  ;
       '999999999' VALID  ;
       entra1_1(wk_code1) WHEN  ;
       colocaf6()
     @ 5, 30 GET wk_code2 RANGE  ;
       wk_code1 PICTURE  ;
       '999999999' VALID  ;
       entra2_2(wk_code2) WHEN  ;
       colocaf6()
     @ 6, 30 GET output PICTURE  ;
       '@m Pantalla,Impresora'
     SET CURSOR ON
     READ
     SET CURSOR OFF
     IF LASTKEY() = 27
          ppas = .F.
          LOOP
     ENDIF
     SELECT st_itecn
     SET NEAR ON
     code1 = STR(wk_code1, 9)
     code2 = STR(wk_code2, 9)
     SEEK code1
     SET NEAR OFF
     DO mensa WITH  ;
        '*** Un momento, Por Favor ... ***',  ;
        'COLO'
     IF output = 'Impresora'
          ??? CHR(15)
          REPORT FORMAT porl0404  ;
                 TO PRINTER  ;
                 NOCONSOLE WHILE  ;
                 (codent >= code1  ;
                 .AND. codent <=  ;
                 code2)
          ??? CHR(27) + CHR(80)
          DO mensa WITH  ;
             '*** Un momento, Por Favor ... ***',  ;
             'SACA'
     ELSE
          REPORT FORMAT porl0404  ;
                 TO FILE  ;
                 text5.txt  ;
                 NOCONSOLE WHILE  ;
                 (codent >= code1  ;
                 .AND. codent <=  ;
                 code2)
          DO mensa WITH  ;
             '*** Un momento, Por Favor ... ***',  ;
             'SACA'
          SET SYSMENU ON
          MODIFY FILE text5.txt  ;
                 NOEDIT WINDOW  ;
                 pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
ON KEY LABEL F6
ON KEY LABEL F10
RETURN
*
PROCEDURE ayuda02
ON KEY LABEL F6
IF (ROW() == 04 .OR. ROW() == 05)  ;
   .AND. COL() > 10
     SELECT st_itecn
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE TECNICOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
ENDIF
ON KEY LABEL F6 do ayuda02
RETURN
*
FUNCTION entra1_1
PARAMETER wk_code1
SELECT st_itecn
SEEK STR(wk_code1, 9)
IF  .NOT. FOUND()
     ACTIVATE WINDOW trabajo
     DO error WITH  ;
        '** C¢digo No Existente **'
     RETURN .F.
ENDIF
@ 4, 40 SAY SUBSTR(noment, 1, 35)
RETURN .T.
*
FUNCTION entra2_2
PARAMETER wk_code2
SELECT st_itecn
SEEK STR(wk_code2, 9)
IF  .NOT. FOUND()
     ACTIVATE WINDOW trabajo
     DO error WITH  ;
        '** C¢digo No Existente **'
     RETURN .F.
ELSE
     @ 5, 40 SAY SUBSTR(noment, 1,  ;
       35)
ENDIF
RETURN .T.
*
*** 
*** ReFox - retrace your steps ... 
***

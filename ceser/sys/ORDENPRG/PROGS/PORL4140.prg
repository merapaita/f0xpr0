*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'INGRESO DE ORDENES POR LINEA '
STORE DATE() TO fecha1, fecha2
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_imode ORDER codigo
SELECT 3
USE st_iorep ORDER ord_fecdoc
SET RELATION TO codmar + codmod INTO st_imode
STORE .T. TO pas
DO WHILE pas
     @ 07, 01 CLEAR TO 13, 77
     @ 3, 2 TO 08, 77
     DO esc_modo WITH 'S'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 03 SAY 'Desde Fecha:'  ;
       GET fecha1
     @ 05, 03 SAY 'Hasta Fecha:'  ;
       GET fecha2 RANGE fecha1
     @ 06, 03 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'
     @ 07, 03 SAY  ;
       'Por Detalle/Res£men:' GET  ;
       tipo PICTURE  ;
       '@m Detalle,Res£men'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_iorep
     SET NEAR ON
     SEEK DTOS(fecha1)
     SET NEAR OFF
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          SET PRINTER ON
          ??? CHR(15)
          IF tipo = 'Res£men'
               REPORT FORMAT  ;
                      PORL4141 TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR indest <>  ;
                      'N' WHILE  ;
                      (fecemi >=  ;
                      fecha1  ;
                      .AND.  ;
                      fecemi <=  ;
                      fecha2)
          ELSE
               REPORT FORMAT  ;
                      PORL4140 TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR indest <>  ;
                      'N' WHILE  ;
                      (fecemi >=  ;
                      fecha1  ;
                      .AND.  ;
                      fecemi <=  ;
                      fecha2)
          ENDIF
          SET PRINTER OFF
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          w_fildoc = SUBSTR(f_archivo(),  ;
                     1, 8) +  ;
                     '.DOC'
          IF tipo = 'Res£men'
               REPO FORM PORL4141 TO FILE;
&w_fildoc while (fecemi>=fecha1 and fecemi<=fecha2);
for indest#'N' NOCONSOLE
          ELSE
               SET SYSMENU ON
               MODIFY REPORT  ;
                      porl4140
               REPO FORM PORL4140 TO FILE;
&w_fildoc while (fecemi>=fecha1 and fecemi<=fecha2);
for indest#'N' NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '*** Un momento, Por Favor ... ***',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODI COMM &w_fildoc WINDOW PANTALL;
NOEDIT
          SET SYSMENU OFF
          dele file &w_fildoc
     ENDIF
ENDDO
DO sacawin
CLOSE DATABASES
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ESTADO DE ARTICULOS POR EMISOR (ESTADO ACTUAL)'
ppas = .T.
CLOSE DATABASES
SELECT 1
USE SHARED ST_IOREP ORDER  ;
    ORD_ESEM
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 4
USE SHARED st_iclpr ORDER codent
STORE 'Pantalla ' TO output
STORE SPACE(4) TO wrk_est1,  ;
      wrk_est2
STORE DATE() TO fecha1, fecha2
STORE SPACE(4) TO emisor, emisor2
STORE 'Detalle' TO tipo
@ 07, 01 CLEAR TO 13, 77
ON KEY LABEL f6 do ayuda with row()
DO WHILE ppas
     @ 03, 02 TO 10, 77
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wk_key = 0
     SET CURSOR ON
     @ 04, 04 SAY  ;
       'C¢digo de Estado :'
     @ 05, 04 SAY  ;
       'C¢digo de Estado :'
     @ 04, 23 GET wrk_est1  ;
       PICTURE '@!' VALID  ;
       valida(wrk_est1)
     @ 05, 23 GET wrk_est2  ;
       PICTURE '@!' VALID  ;
       valida(wrk_est2)
     @ 06, 04 SAY 'Desde Emisor:'  ;
       GET emisor VALID  ;
       valda(emisor)
     @ 06, 44 SAY 'Hasta:' GET  ;
       emisor2 VALID  ;
       valda(emisor2)
     @ 07, 04 SAY 'Desde Fecha :'
     @ 07, 44 SAY 'Hasta:'
     @ 07, 18 GET fecha1
     @ 07, 51 GET fecha2 RANGE  ;
       fecha1
     @ 08, 04 SAY  ;
       'Por Impresora/Pantalla:'  ;
       GET output PICTURE  ;
       '@m Pantalla ,Impresora'
     @ 09, 04 SAY  ;
       'Por Detalle/Res£men:' GET  ;
       tipo PICTURE  ;
       '@m Detalle,Res£men'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY LABEL F6
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_iclpr
     SET ORDER TO codent
     SELECT st_iorep
     SET RELATION TO codent INTO st_iclpr
     SET NEAR ON
     SEEK wrk_est1 + emisor
     SET NEAR OFF
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     IF output = 'Impresora'
          IF tipo = 'Detalle'
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               REPORT FORMAT  ;
                      PORL4074 TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR (indest <>  ;
                      'A' .AND.  ;
                      indest <>  ;
                      'N') .AND.  ;
                      (fecest >=  ;
                      fecha1  ;
                      .AND.  ;
                      fecest <=  ;
                      fecha2)  ;
                      .AND.  ;
                      (codemi >=  ;
                      emisor  ;
                      .AND.  ;
                      codemi <=  ;
                      emisor2)  ;
                      WHILE  ;
                      (auxest >=  ;
                      wrk_est1  ;
                      .AND.  ;
                      auxest <=  ;
                      wrk_est2)
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ELSE
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               REPORT FORMAT  ;
                      PORL4074  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR (indest <>  ;
                      'A' .AND.  ;
                      indest <>  ;
                      'N') .AND.  ;
                      (fecest >=  ;
                      fecha1  ;
                      .AND.  ;
                      fecest <=  ;
                      fecha2)  ;
                      .AND.  ;
                      (codemi >=  ;
                      emisor  ;
                      .AND.  ;
                      codemi <=  ;
                      emisor2)  ;
                      WHILE  ;
                      (auxest >=  ;
                      wrk_est1  ;
                      .AND.  ;
                      auxest <=  ;
                      wrk_est2)
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDIF
     ELSE
          wrk_fil2 = SUBSTR(f_texto(),  ;
                     1, 8) +  ;
                     '.TXT'
          IF tipo = 'Detalle'
               REPO FORM PORL4074 TO FILE;
&wrk_fil2 while (auxest>=wrk_est1 and;
auxest<=wrk_est2) for (indest#'A' and;
indest#'N') and (fecest>=fecha1 and fecest<=fecha2);
and (codemi>=emisor and codemi<=emisor2);
NOCONSOLE
          ELSE
               REPO FORM PORL4074 TO FILE;
&wrk_fil2 while (auxest>=wrk_est1 and;
auxest<=wrk_est2) for (indest#'A' and;
indest#'N') and (fecest>=fecha1 and fecest<=fecha2);
and (codemi>=emisor and codemi<=emisor2);
NOCONSOLE SUMMARY
          ENDIF
          DO mensa WITH  ;
             '*** Un momento, Por Favor ... ***',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &wrk_fil2 WINDOW PANTALL;
noedit
          DELE FILE &wrk_fil2
          SET SYSMENU OFF
     ENDIF
ENDDO
DO sacawin
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT 2
titulo = 'AYUDA DE ESTADOS'
IF opc < 6
     SET FILTER TO tab_codpre == 'ESOR'
ELSE
     SET FILTER TO tab_codpre == 'EMIS'
ENDIF
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
RETURN
*
FUNCTION valida
PARAMETER op
SELECT 2
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .F.
ENDIF
IF EMPTY(op)
     DO error WITH  ;
        'No se Permiten Blancos'
     RETURN .F.
ENDIF
SEEK 'ESOR' + op
IF  .NOT. FOUND()
     DO error WITH  ;
        'C¢digo de Estado No Existe'
     RETURN .F.
ENDIF
@ ROW(), 29 SAY SUBSTR(tab_destab,  ;
  1, 24)
RETURN
*
FUNCTION valda
PARAMETER op
SELECT 2
SEEK 'EMIS' + op
IF  .NOT. FOUND()
     DO error WITH  ;
        'C¢digo de Emisor No Existe'
     RETURN .F.
ENDIF
IF COL() > 40
     @ ROW(), 55 SAY  ;
       SUBSTR(tab_destab, 1, 19)
ELSE
     @ ROW(), 22 SAY  ;
       SUBSTR(tab_destab, 1, 19)
ENDIF
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
*** 
*** ReFox - retrace your steps ... 
***

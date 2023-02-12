*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
DEFINE POPUP tabla FROM 07, 20 TO  ;
       15, 62 PROMPT FIELDS ' ' +  ;
       tab_codtab + ' ³' +  ;
       tab_destab TITLE '¯' +  ;
       ' TABLAS GENERALES ' + '®'  ;
       IN screen COLOR SCHEME 8
ON SELECTION POPUP tabla DEAC POPUP tablax
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
ind_tit = ' CONSULTA '
DO saycenter WITH 1, ind_tit
DO saycenter WITH 2,  ;
   ' TABLAS GENERALES '
DO esc_modo WITH 'C'
DO esc_indica WITH 1, 'AYU',  ;
   'MBV', 'BBB', 'SEL'
DO esc_indica WITH 2, 'BBB',  ;
   'ANT', 'STE', 'ESC'
USE GE_TAB0 ORDER CODIGO
SET FILTER TO tab_codpre = 'TABL'
ON KEY LABEL ENTER DO CONSULTA WITH TAB_CODTAB,;
TAB_DESTAB
DO WHILE LASTKEY()<>27
     ACTIVATE POPUP tabla
ENDDO
ON KEY
CLOSE DATABASES
DO saca_win
RETURN
*
PROCEDURE consulta
PARAMETER opc, titulo
DEFINE POPUP detalle1 FROM 07, 18  ;
       TO 15, 62 PROMPT FIELDS  ;
       ' ' + tab_codtab + ' ³' +  ;
       tab_destab + '³' +  ;
       STR(tab_factor, 6, 2)  ;
       TITLE '¯ ' +  ;
       ALLTRIM(titulo) + ' ®' IN  ;
       screen COLOR SCHEME 8
ON SELECTION POPUP detalle1 DEAC POPUP;
DETTAB
DEFINE POPUP detalle2 FROM 07, 20  ;
       TO 15, 62 PROMPT FIELDS  ;
       ' ' + tab_codtab + ' ³' +  ;
       tab_destab TITLE '¯ ' +  ;
       ALLTRIM(titulo) + ' ®' IN  ;
       screen COLOR SCHEME 8
ON SELECTION POPUP detalle2 DEAC POPUP;
DETTAX
ON KEY
SET FILTER TO tab_codpre = opc
SUM tab_factor TO wrk_total
IF wrk_total > 0
     ACTIVATE POPUP detalle1
ELSE
     ACTIVATE POPUP detalle2
ENDIF
SET FILTER TO tab_codpre = 'TABL'
ON KEY LABEL ENTER DO CONSULTA WITH TAB_CODTAB,;
TAB_DESTAB
*
*** 
*** ReFox - retrace your steps ... 
***

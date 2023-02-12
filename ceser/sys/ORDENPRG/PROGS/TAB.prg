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
   ' MANO DE OBRA '
DO esc_modo WITH 'C'
DO esc_indica WITH 1, 'AYU',  ;
   'MBV', 'BBB', 'SEL'
DO esc_indica WITH 2, 'BBB',  ;
   'ANT', 'STE', 'ESC'
DEFINE POPUP tabla FROM 07, 20 TO  ;
       15, 62 PROMPT FIELDS ' ' +  ;
       tab_codtab + ' ³' +  ;
       tab_destab IN screen COLOR  ;
       SCHEME 8
ON SELECTION POPUP tabla DEAC POPUP tablax
DEFINE POPUP detalle FROM 07, 10  ;
       TO 15, 70 PROMPT FIELDS  ;
       ' ' + mo_codart + '³' +  ;
       SUBSTR(mo_descri, 1, 26) +  ;
       '³' + TRANSFORM(mo_monmog,  ;
       '9,999.99') + '³' +  ;
       TRANSFORM(mo_monmof,  ;
       '9,999.99') + '³' +  ;
       TRANSFORM(mo_monmax,  ;
       '9,999.99') TITLE  ;
       'COD.ÄDESCRIPCIONÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄGTIAÄÄÄÄF/GTIAÄÄÄÄÄMAX.'  ;
       IN screen COLOR SCHEME 8
ON SELECTION POPUP detalle DEAC POPUP;
DETTAB
SELECT 1
USE GE_TAB0 ORDER CODIGO
SELECT 2
USE ST_MOBRA
wrk_file = f_archivo()
CREA TABLE  &wrk_file (MO_CODMAR C(4),;
  MO_CODART C(4),   MO_MONMOG N(7,2),;
 MO_MONMOF N(7,2), MO_MONMAX N(7,2), MO_DESCRI;
C(30)   ) 
SELECT 3
USE &wrk_file EXCLU
APPEND FROM ST_MOBRA
INDEX ON mo_codmar + mo_codart  ;
      TAG codigo
SET ORDER TO CODIGO
GOTO TOP
SCAN WHILE  .NOT. EOF()
     REPLACE mo_descri WITH  ;
             ootab('CLAS', ;
             mo_codart)
ENDSCAN
SELECT 1
SET FILTER TO tab_codpre = 'MARC'
ON KEY LABEL ENTER DO CONSULTA WITH TAB_CODTAB
DO WHILE LASTKEY()<>27
     ACTIVATE POPUP tabla
ENDDO
ON KEY
CLOSE DATABASES
DO saca_win
RETURN
*
PROCEDURE consulta
PARAMETER opc
ON KEY
SELECT 3
SET FILTER TO mo_codmar = opc
ACTIVATE POPUP detalle
SELECT 1
ON KEY LABEL ENTER DO CONSULTA WITH TAB_CODTAB
*
FUNCTION ootab
PARAMETER var1, var2
narea = SELECT()
= ooareat('GE_TAB0','CODIGO')
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

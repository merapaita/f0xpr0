*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 03,  ;
       02 TO 11, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 12, 02  ;
       TO 17, 77 NONE COLOR  ;
       SCHEME 8
DEFINE WINDOW detalles FROM 12,  ;
       02 TO 20, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW totales FROM 21, 02  ;
       TO 21, 77 NONE COLOR  ;
       SCHEME 6
DEFINE POPUP detalle FROM 12, 02  ;
       TO 20, 77 PROMPT FIELDS  ;
       '  ' + cons.alm_codalm +  ;
       '  ' + SUBSTR(ootab('ALMA', ;
       cons.alm_codalm), 1, 20) +  ;
       '  ' + cons.alm_ubicac +  ;
       '  ' +  ;
       TRANSFORM(cons.alm_stkfis,  ;
       '999,999') + '  ' +  ;
       TRANSFORM(cons.alm_stkres,  ;
       '999,999') + '  ' +  ;
       TRANSFORM((cons.alm_stkfis -  ;
       cons.alm_stkres),  ;
       '999,999') TITLE  ;
       'COD.컴DESCRIPCION컴컴컴컴컴훃BICAC.컴컴컴FISICO컴컴RESERV.컴횯ISPON.컴컴'  ;
       IN screen COLOR SCHEME 8
SELECT 1
USE SHARED GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 3
USE SHARED GC_UNI00 ORDER CODIGO
SELECT 4
USE SHARED GC_ALM00 ORDER CODIGO
SELECT 5
USE SHARED GC_DLP00 ORDER CODIGO
SELECT 6
USE SHARED GC_PAR00
wrk_lispre = par_codlis
SELECT 7
USE SHARED GC_CMV00 ORDER  ;
    cmv_feinmo
SELECT 8
USE SHARED GC_KAR00 ORDER CODIGO
SELECT 9
USE SHARED GC_NVE00
SET ORDER TO codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'CONSULTAS',  ;
   'STOCK ACTUAL', 'CONSULTA'
@ 02, 63 SAY PROGRAM()
STORE SPACE(14) TO wrk_codigo
DO WHILE .T.
     wrk_selec = SELECT()
     wrk_campo = 'WRK_CODIGO'
     STORE 0 TO wrk_promed,  ;
           wrk_totpro
     ON KEY LABEL F6 DO PRODUC WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO;

     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     @ 01, 39 CLEAR
     @ 02, 00 CLEAR
     @ 00, 01 SAY 'Producto : '
     @ 00, 12 GET wrk_codigo  ;
       PICTURE '@!' COLOR SCHEME  ;
       8
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          RELEASE WINDOW cabecera,  ;
                  detalle,  ;
                  detalles,  ;
                  totales
          DEACTIVATE WINDOW  ;
                     tablas
          DO p_footer WITH  ;
             '100000000001000000001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     ON KEY
     SELECT 1
     SET ORDER TO CODIGO
     SEEK wrk_codigo
     IF  .NOT. FOUND()
          DO p_mensaje WITH  ;
             'C줰igo no Existe'
          LOOP
     ENDIF
     wrk_descri = pro_descri
     wrk_unimed = pro_unimed
     wrk_stktra = pro_stktra
     wrk_stksol = pro_stksol
     wrk_stkbor = pro_stkbor
     wrk_modelo = SUBSTR(pro_modelo,  ;
                  1, 14)
     wrk_prefob = pro_coremo
     SELECT 2
     SEEK 'UNID' + wrk_unimed
     IF FOUND()
          wrk_destab = tab_destab
     ELSE
          wrk_destab = 'ERROR'
     ENDIF
     SELECT 5
     SEEK wrk_lispre + wrk_codigo +  ;
          wrk_unimed
     IF FOUND()
          wrk_precio = dlp_prcigv
     ELSE
          wrk_precio = 0
     ENDIF
     SELECT gc_alm00
     SEEK wrk_codigo
     IF FOUND()
          wrk_stkfis = alm_stkfis
          wrk_stkres = alm_stkres
     ELSE
          wrk_stkfis = 0
          wrk_stkres = 0
     ENDIF
     wrk_totalf = oototstk(wrk_codigo)
     SUM FOR alm_codpro =  ;
         wrk_codigo alm_stkres TO  ;
         wrk_totalr
     wrk_dispon = wrk_totalf -  ;
                  wrk_totalr
     DO promedio
     @ 00, 28 SAY wrk_descri
     @ 00, 65 SAY ootc2(DATE(), ;
       'SOL ','DOL ','2') PICTURE  ;
       '99,999.99'
     @ 01, 01 SAY 'Reemplazo: ' +  ;
       gc_pro00.pro_codree
     @ 01, 39 SAY  ;
       'Nro. Parte        : ' +  ;
       gc_pro00.pro_numpar
     @ 02, 01 SAY  ;
       'Unidad Inventario : ' +  ;
       wrk_unimed
     @ 02, 27 SAY wrk_destab
     @ 02, 39 SAY  ;
       'Modelo            : ' +  ;
       wrk_modelo
     @ 03, 01 SAY  ;
       'Stock Disponible  :'
     @ 03, 21 SAY wrk_totalf  ;
       PICTURE '99,999,999.99'  ;
       COLOR W+/N* 
     @ 03, 39 SAY  ;
       'Promedio Mensual  :'
     @ 03, 60 SAY wrk_promed  ;
       PICTURE '999,999.99'
     @ 04, 39 SAY  ;
       'Stock Solicitado  :'
     @ 04, 60 SAY wrk_stksol  ;
       PICTURE '999,999.99'
     @ 05, 39 SAY  ;
       'Stock BackOrder   :'
     @ 05, 60 SAY wrk_stkbor  ;
       PICTURE '999,999.99'
     @ 06, 39 SAY  ;
       'Stock en Tr쟮sito :'
     @ 06, 60 SAY wrk_stktra  ;
       PICTURE '999,999.99'
     IF xnnn <> 'A1'
          @ 06, 01 SAY  ;
            'Precio F.O.B.     :'
          @ 06, 21 SAY wrk_prefob  ;
            PICTURE  ;
            '99,999,999.99'
     ENDIF
     @ 04,01 SAY "Precio         &sys_simmon:"
     precioso = ROUND(wrk_precio *  ;
                ootc2(DATE(), ;
                'SOL ','DOL ', ;
                '2'), 2)
     @ 04, 21 SAY precioso  ;
       PICTURE '99,999,999.99'  ;
       COLOR W+/N 
     @ 05,01 SAY "Precio Inc.&sys_codimp&sys_simomb:"
     @ 05, 21 SAY wrk_precio  ;
       PICTURE '99,999,999.99'  ;
       COLOR W+/N 
     DO carga
     LOOP
     RELEASE WINDOW cabecera,  ;
             detalle
     DEACTIVATE WINDOW tablas
     RESTORE SCREEN
     CLOSE DATABASES
     RETURN
ENDDO
*
PROCEDURE carga
DO WHILE .T.
     SELECT 3
     SEEK wrk_codigo
     IF FOUND()
          COUNT FOR uni_codpro =  ;
                wrk_codigo TO  ;
                wrk_reg
     ELSE
          wrk_reg = 0
     ENDIF
     wrk_totreg = wrk_reg + 1
     DIMENSION uniarray(  ;
               wrk_totreg, 4)
     DIMENSION arrayuno(  ;
               wrk_totreg)
     uniarray( 1, 1) = wrk_unimed
     uniarray( 1, 2) = 1
     uniarray( 1, 3) = 1
     SELECT 2
     SEEK 'UNID' + uniarray(1,1)
     IF FOUND()
          uniarray( 1, 4) =  ;
                  tab_destab
     ELSE
          uniarray( 1, 4) =  ;
                  wrk_destab
     ENDIF
     IF gc_pro00.pro_estope = 'B'
          arrayuno( 1) =  ;
                  '     **********  R E G I S T R O  -  B L O Q U E A D O  **********'
     ELSE
          arrayuno( 1) = '  ' +  ;
                  uniarray(1,1) +  ;
                  '            ' +  ;
                  SUBSTR(uniarray(1, ;
                  4), 1, 20) +  ;
                  ' ' +  ;
                  STR(uniarray(1, ;
                  3)) + '      ' +  ;
                  STR(uniarray(1, ;
                  2))
     ENDIF
     IF wrk_totreg > 1
          a = 2
          SELECT 3
          GOTO TOP
          LOCATE FOR uni_codpro =  ;
                 wrk_codigo
          DO WHILE FOUND()
               uniarray( a, 1) =  ;
                       uni_unialt
               uniarray( a, 2) =  ;
                       uni_facto1
               uniarray( a, 3) =  ;
                       uni_facto2
               SELECT 2
               SEEK 'UNID' +  ;
                    uniarray(a, ;
                    1)
               IF FOUND()
                    uniarray( a,  ;
                            4) =  ;
                            tab_destab
               ELSE
                    uniarray( a,  ;
                            4) =  ;
                            'ERROR               '
               ENDIF
               arrayuno( a) =  ;
                       '  ' +  ;
                       uniarray(a, ;
                       1) +  ;
                       '            ' +  ;
                       SUBSTR(uniarray(a, ;
                       4), 1, 20) +  ;
                       ' ' +  ;
                       STR(uniarray(a, ;
                       3)) +  ;
                       '      ' +  ;
                       STR(uniarray(a, ;
                       2))
               SELECT 3
               a = a + 1
               CONTINUE
          ENDDO
     ENDIF
     DO p_footer WITH  ;
        '100000000000000000111',  ;
        2
     ON KEY LABEL F9 DO NO_VENTA WITH;
wrk_codigo
     ACTIVATE WINDOW detalle
     STORE 1 TO opcion
     @ 00, 00 GET opcion DEFAULT  ;
       arrayuno(1) SIZE 12, 76  ;
       FROM arrayuno COLOR SCHEME  ;
       8
     READ
     IF LASTKEY() = 27
          DEACTIVATE WINDOW  ;
                     detalle,  ;
                     cabecera
          RETURN
     ENDIF
     IF LASTKEY() = 13
          DO p_footer WITH  ;
             '100000000000000000101',  ;
             2
          DEACTIVATE WINDOW  ;
                     detalle
          wrk_unibus = SUBSTR(arrayuno(opcion),  ;
                       3, 6)
          SELECT gc_alm00
          SEEK wrk_codigo
          IF FOUND()
               COUNT FOR  ;
                     alm_codpro =  ;
                     wrk_codigo  ;
                     TO wrk_reg
          ELSE
               wrk_reg = 0
          ENDIF
          IF wrk_reg = 0
               DO p_mensaje WITH  ;
                  ' No existe Stock en Ning즢 Almac굈 '
               LOOP
          ENDIF
          SELECT gc_alm00.alm_codpro,  ;
                 gc_alm00.alm_codalm,  ;
                 gc_alm00.alm_stkfis,  ;
                 gc_alm00.alm_stkres,  ;
                 gc_alm00.alm_ubicac  ;
                 FROM GC_ALM00  ;
                 WHERE  ;
                 gc_alm00.alm_codpro =  ;
                 wrk_codigo ORDER  ;
                 BY  ;
                 gc_alm00.alm_codalm  ;
                 INTO CURSOR  ;
                 CONS
          ACTIVATE POPUP detalle
          DEACTIVATE WINDOW  ;
                     detalles
          DEACTIVATE POPUP  ;
                     detalle
     ENDIF
     LOOP
ENDDO
RETURN
*
PROCEDURE promedio
SELECT gc_kar00
SEEK wrk_codigo
SET FILTER TO kar_codpro = wrk_codigo;
.AND. kar_fecdoc > GOMONTH(DATE(), -6);
.AND. (kar_codmov <> 'TRBO';
.AND. kar_codmov <> 'IFC ';
.AND. kar_codmov <> 'IFO ';
.AND. kar_codmov <> 'IFL ';
.AND. kar_codmov <> 'IFE ';
.AND. kar_codmov <> 'IFP ';
.AND. kar_codmov <> 'COI ';
.AND. SUBSTR(kar_codmov, 1, 1) <> 'A';
.AND. kar_codmov <> 'EMER')
STORE 0 TO wrk_totpro, wrk_totdev
SUM FOR kar_codmov <> 'IFD '  ;
    kar_cantid TO wrk_totpro
SUM FOR kar_codmov = 'IFD '  ;
    kar_cantid TO wrk_totdev
wrk_promed = (wrk_totpro -  ;
             wrk_totdev) / 6
SET FILTER TO
RETURN
*
PROCEDURE no_venta
PARAMETER wrk_codigo
SELECT gc_nve00
SEEK wrk_codigo
IF  .NOT. FOUND()
     APPEND BLANK
     REPLACE nve_codpro WITH  ;
             wrk_codigo
ENDIF
STORE MONTH(DATE()) TO mes
DO CASE
     CASE mes = 1
          REPLACE nve_mes1 WITH  ;
                  nve_mes1 + 1
     CASE mes = 2
          REPLACE nve_mes2 WITH  ;
                  nve_mes2 + 1
     CASE mes = 3
          REPLACE nve_mes3 WITH  ;
                  nve_mes3 + 1
     CASE mes = 4
          REPLACE nve_mes4 WITH  ;
                  nve_mes4 + 1
     CASE mes = 5
          REPLACE nve_mes5 WITH  ;
                  nve_mes5 + 1
     CASE mes = 6
          REPLACE nve_mes6 WITH  ;
                  nve_mes6 + 1
     CASE mes = 7
          REPLACE nve_mes7 WITH  ;
                  nve_mes7 + 1
     CASE mes = 8
          REPLACE nve_mes8 WITH  ;
                  nve_mes8 + 1
     CASE mes = 9
          REPLACE nve_mes9 WITH  ;
                  nve_mes9 + 1
     CASE mes = 10
          REPLACE nve_mes10 WITH  ;
                  nve_mes10 + 1
     CASE mes = 11
          REPLACE nve_mes11 WITH  ;
                  nve_mes11 + 1
     CASE mes = 12
          REPLACE nve_mes12 WITH  ;
                  nve_mes12 + 1
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'CONSULTA'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' CONSULTA DE STOCK POR ALMACENES '
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 06,  ;
       02 TO 12, 74 NONE COLOR  ;
       SCHEME 7
DEFINE WINDOW detalle FROM 13, 02  ;
       TO 17, 74 NONE COLOR  ;
       SCHEME 7
DEFINE WINDOW detalles FROM 13,  ;
       02 TO 16, 74 NONE COLOR  ;
       SCHEME 7
DEFINE WINDOW tituloc FROM 13, 02  ;
       TO 13, 74 NONE COLOR  ;
       SCHEME 8
DEFINE POPUP detalle FROM 13, 02  ;
       TO 19, 74 PROMPT FIELDS  ;
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
DO esc_modo WITH 'C'
DO esc_indica WITH 1, 'AYU',  ;
   'BUS', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
= ooopen('GC_PAR00',0)
wrk_lispre = par_codlis
STORE SPACE(14) TO wrk_codigo,  ;
      w_codree
DO WHILE .T.
     wrk_selec = SELECT()
     wrk_campo = 'WRK_CODIGO'
     ON KEY LABEL F6 DO AYUDA
     ACTIVATE WINDOW cabecera
     @ 01, 39 CLEAR
     @ 02, 00 CLEAR
     @ 00, 01 SAY 'Producto :'
     @ 00, 12 GET wrk_codigo  ;
       PICTURE '@!' COLOR SCHEME  ;
       8
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          RELEASE WINDOW cabecera,  ;
                  detalle,  ;
                  tituloc,  ;
                  detalles,  ;
                  totales
          DEACTIVATE WINDOW  ;
                     tablas
          ON KEY
          DO sacawin
          RETURN
     ENDIF
     ON KEY
     = ooopen('GC_PRO00', ;
       'CODIGO')
     SET ORDER TO CODIGO
     SEEK wrk_codigo
     IF  .NOT. FOUND()
          DO error WITH  ;
             '** C줰igo No Existe **'
          = ooclose('GC_PRO00')
          LOOP
     ENDIF
     wrk_descri = pro_descri
     wrk_unimed = pro_unimed
     wrk_stktra = pro_stktra
     wrk_modelo = pro_modelo
     wrk_prefob = pro_coremo
     wrk_stkbor = pro_stkbor
     wrk_stksol = pro_stksol
     w_codree = pro_codree
     w_numpar = pro_numpar
     = ooclose('GC_PRO00')
     = ooopen('GE_TAB0','CODIGO')
     SEEK 'UNID' + wrk_unimed
     IF FOUND()
          wrk_destab = tab_destab
     ELSE
          wrk_destab = 'ERROR'
     ENDIF
     = ooclose('GE_TAB0')
     = ooopen('GC_DLP00', ;
       'CODIGO')
     SEEK wrk_lispre + wrk_codigo +  ;
          wrk_unimed
     IF FOUND()
          wrk_precio = dlp_prcigv
     ELSE
          wrk_precio = 0
     ENDIF
     = ooclose('GC_DLP00')
     = ooopen('GC_ALM00', ;
       'CODIGO')
     SEEK wrk_codigo
     IF FOUND()
          wrk_stkfis = alm_stkfis
          wrk_stkres = alm_stkres
     ELSE
          wrk_stkfis = 0
          wrk_stkres = 0
     ENDIF
     SUM FOR alm_codpro =  ;
         wrk_codigo alm_stkfis TO  ;
         wrk_totalf
     SUM FOR alm_codpro =  ;
         wrk_codigo alm_stkres TO  ;
         wrk_totalr
     = ooclose('GC_ALM00')
     wrk_dispon = wrk_totalf -  ;
                  wrk_totalr
     SELECT 20
     USE SHARED gc_cmv00 ORDER  ;
         cmv_feinmo
     @ 00, 28 SAY wrk_descri  ;
       COLOR W/N 
     @ 01, 01 SAY 'Reemplazo: ' +  ;
       w_codree COLOR W/N 
     @ 01, 39 SAY  ;
       'N즡ero Parte      : ' +  ;
       w_numpar COLOR W/N 
     @ 02, 01 SAY  ;
       'Unidad Inventario : ' +  ;
       wrk_unimed COLOR W/N 
     @ 02, 27 SAY wrk_destab  ;
       COLOR W/N 
     @ 02, 39 SAY  ;
       'Modelo            : ' +  ;
       SUBSTR(wrk_modelo, 1, 14)  ;
       COLOR W/N 
     @ 03, 01 SAY  ;
       'Stock F죛ico      : '  ;
       COLOR W/N 
     @ 03, 24 SAY wrk_totalf  ;
       PICTURE '999,999' COLOR W/ ;
       N 
     @ 03, 39 SAY  ;
       'Stock Disponible  : '  ;
       COLOR W/N 
     @ 03, 61 SAY wrk_dispon  ;
       PICTURE '999,999' COLOR W+/ ;
       N* 
     @ 04, 01 SAY  ;
       'Stock Reservado   : '  ;
       COLOR W/N 
     @ 04, 24 SAY wrk_stkres  ;
       PICTURE '999,999' COLOR W/ ;
       N 
     @ 04, 39 SAY  ;
       'Stock Solicitado  : '  ;
       COLOR W/N 
     @ 04, 61 SAY wrk_stksol  ;
       PICTURE '999,999' COLOR W/ ;
       N 
     @ 05, 01 SAY  ;
       'Precio      S/.   :'  ;
       COLOR W/N 
     precioso = ROUND(wrk_precio *  ;
                ootc2(DATE(), ;
                'SOL ','DOL ', ;
                '2'), 2)
     @ 05, 24 SAY precioso  ;
       PICTURE '999,999.99' COLOR  ;
       W/N 
     @ 05, 39 SAY  ;
       'Stock en Tr쟮sito : '  ;
       COLOR W/N 
     @ 05, 61 SAY wrk_stktra  ;
       PICTURE '999,999' COLOR W/ ;
       N 
     @ 06, 01 SAY  ;
       'Precio Inc. IGV $ :'  ;
       COLOR W/N 
     @ 06, 24 SAY wrk_precio  ;
       PICTURE '999,999.99' COLOR  ;
       W/N 
     @ 06, 39 SAY  ;
       'Stock BackOrder   :'  ;
       COLOR W/N 
     @ 06, 61 SAY wrk_stkbor  ;
       PICTURE '999,999' COLOR W/ ;
       N 
     DO carga
     LOOP
     RELEASE WINDOW cabecera,  ;
             detalle, tituloc
     CLOSE DATABASES
     RETURN
ENDDO
RETURN
*
PROCEDURE carga
DEACTIVATE WINDOW tituloc
DEACTIVATE WINDOW detalle
wrk_unibus = 'UNID'
= ooopen('GC_ALM00','CODIGO')
SEEK wrk_codigo
IF FOUND()
     COUNT FOR alm_codpro =  ;
           wrk_codigo TO wrk_reg
ELSE
     wrk_reg = 0
ENDIF
= ooclose('GC_ALM00')
IF wrk_reg = 0
     DO error WITH  ;
        '*** No existe Stock en ning즢 Almacen ***'
     CLOSE DATABASES
     DEACTIVATE WINDOW tituloc
     RETURN
ENDIF
ACTIVATE WINDOW detalles
SELECT gc_alm00.alm_codpro,  ;
       gc_alm00.alm_codalm,  ;
       gc_alm00.alm_stkfis,  ;
       gc_alm00.alm_stkres,  ;
       gc_alm00.alm_ubicac FROM  ;
       GC_ALM00 WHERE  ;
       gc_alm00.alm_codpro =  ;
       wrk_codigo ORDER BY  ;
       gc_alm00.alm_codalm INTO  ;
       CURSOR CONS
ACTIVATE POPUP detalle
DEACTIVATE WINDOW detalles
DEACTIVATE POPUP detalle
DEACTIVATE WINDOW detalles
CLOSE DATABASES
RETURN
*
PROCEDURE ayuda
ON KEY
wrk_select = SELECT()
USE IN 14 gc_pro00
USE IN 15 gc_alm00
SELECT gc_pro00
wrk_selpro = SELECT()
wrk_campo = gc_pro00.pro_codpro
DO producxx WITH wrk_campo,  ;
   wrk_select, wrk_selpro
IF LASTKEY() <> 27
     KEYBOARD wrk_campo
ENDIF
SELECT (wrk_selpro)
ON KEY LABEL F6 do ayuda
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

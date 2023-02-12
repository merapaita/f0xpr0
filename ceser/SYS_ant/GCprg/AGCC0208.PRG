*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
SET CURSOR ON
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 09, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 10, 02  ;
       TO 21, 77 IN screen COLOR  ;
       SCHEME 7
DEFINE POPUP buscar FROM 05, 23  ;
       TO 09, 46 PROMPT FIELDS  ;
       gc_hlp00.hlp_codlis + '³' +  ;
       gc_hlp00.hlp_deslis MARK  ;
       CHR(16) MARGIN IN screen  ;
       SCROLL COLOR SCHEME 7
ON SELECTION POPUP buscar DEAC POPUP buscar
SELECT 1
USE GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 2
USE GC_HLP00 ORDER CODIGO
SELECT 3
USE GC_DLP00 ORDER CODIGO
SELECT 4
USE GC_ALM00 ORDER CODIGO
ACTIVATE WINDOW tablas
DO p_prestab WITH 'CONSULTAS',  ;
   'LISTA DE PRECIOS',  ;
   'CONSULTA'
@ 2, 63 SAY PROGRAM()
DO WHILE .T.
     wrk_selec = SELECT()
     wrk_campo = 'WRK_CODIGO'
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     STORE rge_lispre TO  ;
           wrk_codlis
     ON KEY LABEL F6 DO BUSCAR WITH wrk_codlis
     @ 00, 01 SAY  ;
       'C¢digo de Lista :' GET  ;
       wrk_codlis PICTURE '@!'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DEACTIVATE WINDOW  ;
                     tablas,  ;
                     codigo,  ;
                     footer
          RELEASE WINDOW cabecera,  ;
                  detalle
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     ON KEY
     SELECT 2
     SEEK wrk_codlis
     IF  .NOT. FOUND()
          DO p_mensaje WITH  ;
             'Codigo de Lista no Existe'
          LOOP
     ENDIF
     DO p_footer WITH  ;
        '100000000000001110001',  ;
        2
     ACTIVATE WINDOW cabecera
     @ 01, 01 SAY  ;
       'Descripci¢n     : ' +  ;
       hlp_deslis
     @ 02, 01 SAY  ;
       'Vigencia Desde  : ' +  ;
       DTOC(hlp_vigdes)
     @ 02, 40 SAY  ;
       'Vigencia Hasta  : ' +  ;
       DTOC(hlp_vighas)
     wrk_desmon = oodesmon(hlp_molipr)
     @ 03, 01 SAY  ;
       'Moneda          : ' +  ;
       hlp_molipr + wrk_desmon
     SELECT 3
     COUNT FOR dlp_codlis =  ;
           wrk_codlis TO  ;
           wrk_conta
     IF wrk_conta = 0
          DO p_mensaje WITH  ;
             'No existen Items a Consultar'
          LOOP
     ENDIF
     ACTIVATE WINDOW detalle
     SET COLOR TO W/N*
     @ 04, 20 SAY  ;
       'Espere un Momento, Por Favor .....'
     SET COLOR TO W+/N
     SELECT 4
     SET FILTER TO gc_alm00.alm_codalm;
= rge_codalm
     SELECT 3
     SET FILTER TO gc_dlp00.dlp_codlis;
= wrk_codlis
     GOTO TOP
     SET RELATION TO gc_dlp00.dlp_codpro;
INTO gc_pro00, gc_dlp00.dlp_codpro INTO;
gc_alm00
     ACTIVATE WINDOW detalle
     BROWSE FIELDS dlp_codpro :H =  ;
            'Codigo',  ;
            gc_pro00.pro_descri :  ;
            35 :H =  ;
            '     Descripcion  ',  ;
            dlp_prsigv :H =  ;
            ' Valor ', dlp_prcigv  ;
            :H = '  Precio  ',  ;
            stock =  ;
            (gc_alm00.alm_stkfis -  ;
            gc_alm00.alm_stkres)  ;
            :H = ' Stock '  ;
            NOAPPEND NOEDIT IN  ;
            detalle NOLGRID
     DEACTIVATE WINDOW detalle
ENDDO
*
PROCEDURE buscar
PARAMETER wrk_codlis
ON KEY
SELECT DISTINCT  ;
       gc_hlp00.hlp_codlis,  ;
       gc_hlp00.hlp_deslis FROM  ;
       GC_HLP00 ORDER BY  ;
       gc_hlp00.hlp_codlis INTO  ;
       CURSOR LISTAS
ACTIVATE POPUP buscar
wrk_codlis = gc_hlp00.hlp_codlis
ON KEY LABEL F6 DO BUSCAR WITH wrk_codlis
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

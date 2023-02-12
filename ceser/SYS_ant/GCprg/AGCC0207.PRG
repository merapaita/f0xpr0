*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 09, 77 IN screen  ;
       COLOR SCHEME 7
DEFINE WINDOW detalle FROM 10, 02  ;
       TO 20, 77 IN screen COLOR  ;
       SCHEME 7
SELECT 1
USE GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 2
USE GC_TPE00
ACTIVATE WINDOW tablas
DO p_prestab WITH 'CONSULTAS',  ;
   'STOCKS CRITICOS', 'CONSULTA'
@ 2, 63 SAY PROGRAM()
DO WHILE .T.
     ON KEY
     STORE SPACE(14) TO  ;
           wrk_codi01
     STORE SPACE(14) TO  ;
           wrk_codi02
     DO p_footer WITH  ;
        '100000000000001000011',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     opcion = 1
     @ 01, 05 GET opcion FUNCTION  ;
       '*R En Cero;Bajo el M¡nimo;Sobre el M ximo'
     READ
     IF LASTKEY() = 5 .OR.  ;
        LASTKEY() = 19
          LOOP
     ENDIF
     IF LASTKEY() = 27
          CLOSE DATABASES
          RELEASE WINDOW cabecera,  ;
                  detalle, titu
          DEACTIVATE WINDOW  ;
                     tablas
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          DO p_tempos
          RETURN
     ENDIF
     opcion1 = 1
     @ 01, 45 GET opcion1  ;
       FUNCTION  ;
       '*R C¢digo;Consolidado'
     READ
     IF LASTKEY() = 5 .OR.  ;
        LASTKEY() = 19
          LOOP
     ENDIF
     IF LASTKEY() = 27
          LOOP
     ENDIF
     DO CASE
          CASE opcion = 1
               IF opcion1 = 1
                    wrk_valor = 'ESTAD02.SUM_ALM_ST = 0'
               ELSE
                    wrk_valor = 'ESTAD.SUM_ALM_ST = 0'
               ENDIF
          CASE opcion = 2
               IF opcion1 = 1
                    wrk_valor = 'ESTAD02.SUM_ALM_ST < ESTAD02.PRO_STKMIN'
               ELSE
                    wrk_valor = 'ESTAD.SUM_ALM_ST < ESTAD.PRO_STKMIN'
               ENDIF
          CASE opcion = 3
               IF opcion1 = 1
                    wrk_valor = 'ESTAD02.SUM_ALM_ST > ESTAD02.PRO_STKMAX'
               ELSE
                    wrk_valor = 'ESTAD.SUM_ALM_ST > ESTAD.PRO_STKMAX'
               ENDIF
     ENDCASE
     DO CASE
          CASE opcion1 = 2
               ACTIVATE WINDOW  ;
                        detalle
               SET COLOR TO W/N*
               @ 04, 33 SAY  ;
                 'Wait....'
               SET COLOR TO W+/N
               SELECT gc_pro00.pro_codpro,  ;
                      gc_pro00.pro_descri,  ;
                      gc_pro00.pro_unimed,  ;
                      SUM(gc_alm00.alm_stkfis),  ;
                      gc_pro00.pro_stkmin,  ;
                      gc_pro00.pro_stkmax,  ;
                      gc_pro00.pro_critic  ;
                      FROM  ;
                      GC_PRO00,  ;
                      GC_ALM00  ;
                      WHERE  ;
                      gc_pro00.pro_codpro =  ;
                      gc_alm00.alm_codpro  ;
                      GROUP BY  ;
                      gc_pro00.pro_codpro  ;
                      ORDER BY  ;
                      gc_pro00.pro_codpro  ;
                      INTO CURSOR  ;
                      ESTAD
               COUNT TO wrk_conta;
 FOR &WRK_VALOR
               IF wrk_conta = 0
                    DO p_mensaje  ;
                       WITH  ;
                       'No Existen Datos a Consultar'
                    CLEAR
                    LOOP
               ENDIF
               BROWSE  FIELDS  ESTAD.PRO_CODPRO;
   :H = 'PRODUCTO',  ESTAD.PRO_DESCRI;
 :20 :H = 'DESCRIPCION',  ESTAD.PRO_UNIMED;
   :H = 'UND',  ESTAD.SUM_ALM_ST;
 :7 :H = 'STOCK',  ESTAD.PRO_STKMIN;
 :7 :H = 'STK MIN',  ESTAD.PRO_STKMAX;
 :7 :H = 'STK MAX',  ESTAD.PRO_CRITIC;
 :7 :H = 'STK CRIT' FOR  &WRK_VALOR;
  NOMODIFY IN WIND DETALLE
               CLEAR
          CASE opcion1 = 1
               ACTIVATE WINDOW  ;
                        cabecera
               @ 01, 45 CLEAR TO  ;
                 01, 65
               @ 02, 45 CLEAR TO  ;
                 02, 65
               wrk_selec = SELECT()
               wrk_campo = 'WRK_CODI01'
               ON KEY LABEL F6 DO PRODUC;
WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO 
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        cabecera
               @ 00, 38 SAY  ;
                 'Rango :'
               @ 01, 46 SAY  ;
                 'Desde :'
               @ 02, 46 SAY  ;
                 'Hasta :'
               @ 01, 54 GET  ;
                 wrk_codi01  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 consis(wrk_codi01)
               READ
               IF LASTKEY() = 27
                    LOOP
               ENDIF
               wrk_selec = SELECT()
               wrk_campo = 'WRK_CODI02'
               ON KEY LABEL F6 DO PRODUC;
WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO 
               @ 02, 54 GET  ;
                 wrk_codi02  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 consis(wrk_codi02)
               READ
               IF LASTKEY() = 27
                    LOOP
               ENDIF
               DO p_footer WITH  ;
                  '100000000000001000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        detalle
               SET COLOR TO W/N*
               @ 04, 33 SAY  ;
                 'Wait....'
               SET COLOR TO W+/N
               SELECT gc_pro00.pro_codpro,  ;
                      gc_pro00.pro_descri,  ;
                      gc_pro00.pro_unimed,  ;
                      SUM(gc_alm00.alm_stkfis),  ;
                      gc_pro00.pro_stkmin,  ;
                      gc_pro00.pro_stkmax,  ;
                      gc_pro00.pro_critic  ;
                      FROM  ;
                      GC_PRO00,  ;
                      GC_ALM00  ;
                      WHERE  ;
                      gc_pro00.pro_codpro =  ;
                      gc_alm00.alm_codpro  ;
                      AND  ;
                      (gc_pro00.pro_codpro >=  ;
                      wrk_codi01  ;
                      AND  ;
                      gc_pro00.pro_codpro <=  ;
                      wrk_codi02)  ;
                      GROUP BY  ;
                      gc_pro00.pro_codpro  ;
                      ORDER BY  ;
                      gc_pro00.pro_codpro  ;
                      INTO CURSOR  ;
                      ESTAD02
               COUNT TO wrk_conta;
 FOR &WRK_VALOR
               IF wrk_conta = 0
                    DO p_mensaje  ;
                       WITH  ;
                       'No Existen Datos a Consultar'
                    CLEAR
                    LOOP
               ENDIF
               BROWSE  FIELDS  ESTAD02.PRO_CODPRO;
  :H = 'PRODUCTO',  ESTAD02.PRO_DESCRI;
 :20 :H = 'DESCRIPCION',  ESTAD02.PRO_UNIMED;
  :H = 'UND',  ESTAD02.SUM_ALM_ST;
 :7 :H = 'STOCK',  ESTAD02.PRO_STKMIN;
 :7 :H = 'STK MIN',  ESTAD02.PRO_STKMAX;
 :7 :H = 'STK MAX',  ESTAD02.PRO_CRITIC;
 :7 :H = 'STK CRIT' FOR  &WRK_VALOR;
  NOMODIFY IN WIND DETALLE
               CLEAR
     ENDCASE
ENDDO
*
FUNCTION consis
PARAMETER wrk_valor
ON KEY
SELECT 1
SET ORDER TO CODIGO
SEEK wrk_valor
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        'Codigo no Existe'
     RETURN .F.
ENDIF
*
*** 
*** ReFox - retrace your steps ... 
***

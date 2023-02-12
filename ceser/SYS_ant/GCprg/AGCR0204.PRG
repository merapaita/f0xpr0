*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW rangos FROM 11, 56  ;
       TO 14, 71 IN screen COLOR  ;
       SCHEME 7
SELECT 1
USE GC_HLP00 ORDER CODIGO
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE GC_ALM00 ORDER CODIGO
SELECT 4
USE GC_DLP00
SELECT 3
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'STOCK POR ALMACENES',  ;
   'REPORTE'
@ 2, 63 SAY PROGRAM()
wrk_progra = PROGRAM()
m.currarea = SELECT()
STORE SPACE(4) TO wrk_desalm,  ;
      wrk_hasalm, wrk_desmar,  ;
      wrk_hasmar
STORE SPACE(4) TO wrk_deslin,  ;
      wrk_haslin, wrk_descat,  ;
      wrk_hascat
STORE SPACE(4) TO wrk_dessub,  ;
      wrk_hassub, wrk_desrub,  ;
      wrk_hasrub
STORE SPACE(4) TO wrk_dessur,  ;
      wrk_hassur
DIMENSION tabla[ 7]
tabla[ 1] = 'ALMA'
tabla[ 2] = 'MARC'
tabla[ 3] = 'LINE'
tabla[ 4] = 'CATE'
tabla[ 5] = 'SUCA'
tabla[ 6] = 'RUBR'
tabla[ 7] = 'SURU'
DIMENSION vari1[ 7]
vari1[ 1] = 'wrk_desalm'
vari1[ 2] = 'wrk_desmar'
vari1[ 3] = 'wrk_deslin'
vari1[ 4] = 'wrk_descat'
vari1[ 5] = 'wrk_dessub'
vari1[ 6] = 'wrk_desrub'
vari1[ 7] = 'wrk_dessur'
DIMENSION vari2[ 7]
vari2[ 1] = 'wrk_hasalm'
vari2[ 2] = 'wrk_hasmar'
vari2[ 3] = 'wrk_haslin'
vari2[ 4] = 'wrk_hascat'
vari2[ 5] = 'wrk_hassub'
vari2[ 6] = 'wrk_hasrub'
vari2[ 7] = 'wrk_hassur'
DO inic_var
DO WHILE .T.
     IF  .NOT. WEXIST('stock')  ;
         .OR.  ;
         UPPER(WTITLE('STOCK')) ==  ;
         'STOCK.PJX' .OR.  ;
         UPPER(WTITLE('STOCK')) ==  ;
         'STOCK.SCX' .OR.  ;
         UPPER(WTITLE('STOCK')) ==  ;
         'STOCK.MNX' .OR.  ;
         UPPER(WTITLE('STOCK')) ==  ;
         'STOCK.PRG' .OR.  ;
         UPPER(WTITLE('STOCK')) ==  ;
         'STOCK.FRX' .OR.  ;
         UPPER(WTITLE('STOCK')) ==  ;
         'STOCK.QPR'
          DEFINE WINDOW stock  ;
                 FROM 05, 03 TO  ;
                 20, 76 NOFLOAT  ;
                 NOCLOSE SHADOW  ;
                 IN screen  ;
                 NOMINIMIZE COLOR  ;
                 SCHEME 1
     ENDIF
     DO p_footer WITH  ;
        '100000000000001000011',  ;
        1
     IF WVISIBLE('stock')
          ACTIVATE WINDOW SAME  ;
                   stock
     ELSE
          ACTIVATE WINDOW NOSHOW  ;
                   stock
     ENDIF
     CLEAR
     @ 00, 01 SAY 'OPCIONES :'
     @ 01, 10 SAY  ;
       'Almacenes      :' SIZE 1,  ;
       16, 0
     @ 03, 10 SAY  ;
       'Marcas         :' SIZE 1,  ;
       16, 0
     @ 05, 10 SAY  ;
       'Lineas         :' SIZE 1,  ;
       16, 0
     @ 07, 10 SAY  ;
       'Categorias     :' SIZE 1,  ;
       16, 0
     @ 09, 10 SAY  ;
       'Sub-Categorias :' SIZE 1,  ;
       16, 0
     @ 11, 10 SAY  ;
       'Rubros         :' SIZE 1,  ;
       16, 0
     @ 13, 10 SAY  ;
       'Sub-Rubros     :' SIZE 1,  ;
       16, 0
     @ 13, 65 GET ok DEFAULT 1  ;
       SIZE 1, 6 FUNCTION  ;
       '*t \!OK' VALID  ;
       despues(8)
     @ 01, 28 GET wrk_alma  ;
       DEFAULT 1 SIZE 1, 10, 0  ;
       PICTURE  ;
       '@*RHN Todos;Rangos' VALID  ;
       despues(1) WHEN antes(1)
     @ 03, 28 GET wrk_marca  ;
       DEFAULT 1 SIZE 1, 10, 0  ;
       PICTURE  ;
       '@*RHN Todos;Rangos' VALID  ;
       despues(2)
     @ 05, 28 GET wrk_linea  ;
       DEFAULT 1 SIZE 1, 10, 0  ;
       PICTURE  ;
       '@*RHN Todos;Rangos' VALID  ;
       despues(3)
     @ 07, 28 GET wrk_catego  ;
       DEFAULT 1 SIZE 1, 10, 0  ;
       PICTURE  ;
       '@*RHN Todos;Rangos' VALID  ;
       despues(4)
     @ 09, 28 GET wrk_subcat  ;
       DEFAULT 1 SIZE 1, 10, 0  ;
       PICTURE  ;
       '@*RHN Todos;Rangos' VALID  ;
       despues(5)
     @ 11, 28 GET wrk_rubro  ;
       DEFAULT 1 SIZE 1, 10, 0  ;
       PICTURE  ;
       '@*RHN Todos;Rangos' VALID  ;
       despues(6)
     @ 13, 28 GET wrk_subrub  ;
       DEFAULT 1 SIZE 1, 10, 0  ;
       PICTURE  ;
       '@*RHN Todos;Rangos' VALID  ;
       despues(7)
     IF  .NOT. WVISIBLE('stock')
          ACTIVATE WINDOW stock
     ENDIF
     READ CYCLE
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DEACTIVATE WINDOW  ;
                     tablas,  ;
                     codigo,  ;
                     footer
          RELEASE WINDOW cabecera,  ;
                  detalle, rangos,  ;
                  winmensaje
          RELEASE WINDOW stock
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     SELECT (m.currarea)
ENDDO
*
PROCEDURE inic_var
SELECT 2
FOR a = 1 TO 7
     SET FILTER TO tab_codpre = tabla(a)
     GOTO TOP
     &vari1[A] = tab_codtab
     GOTO BOTTOM
     &vari2[A] = tab_codtab
ENDFOR
SET FILTER TO
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
     CASE opc = 2
ENDCASE
*
FUNCTION despues
PARAMETER item
DO CASE
     CASE item = 1
          IF wrk_alma = 1
               SELECT 2
               SET FILTER TO tab_codpre;
= 'ALMA'
               GOTO TOP
               wrk_desalm = tab_codtab
               GOTO BOTTOM
               wrk_hasalm = tab_codtab
               SET FILTER TO
               @ 01, 52 SAY  ;
                 SPACE(20)
               RETURN .T.
          ENDIF
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW rangos
          CLEAR
          @ 00, 01 SAY 'Desde :'
          @ 01, 01 SAY 'Hasta :'
          DO WHILE .T.
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               wrk_param = 'GC_ALM00.ALM_CODPRO'
               wrk_busca = 'ALMA'
               wrk_var = 'wrk_desalm'
               wrk_detemp = wrk_desalm
               wrk_hatemp = wrk_hasalm
               @ 00, 09 GET  ;
                 wrk_desalm VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_desalm)
               READ
               IF LASTKEY() = 27
                    wrk_desalm = wrk_detemp
                    DEACTIVATE WINDOW  ;
                               rangos
                    RETURN
               ENDIF
               SELECT 2
               SEEK 'ALMA' +  ;
                    wrk_desalm
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Almac‚n No Existe'
                    LOOP
               ENDIF
               wrk_var = 'wrk_hasalm'
               @ 01, 09 GET  ;
                 wrk_hasalm VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_hasalm)
               READ
               IF LASTKEY() = 27
                    wrk_desalm = wrk_detemp
                    wrk_hasalm = wrk_hatemp
                    RETURN
               ENDIF
               ON KEY
               SELECT 2
               SEEK 'ALMA' +  ;
                    wrk_hasalm
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Almac‚n No Existe'
                    LOOP
               ENDIF
               IF wrk_desalm >  ;
                  wrk_hasalm
                    DO p_mensaje  ;
                       WITH  ;
                       'Rango Pedido Incorrecto'
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        stock
               @ 01, 52 SAY  ;
                 wrk_desalm +  ;
                 ' - ' +  ;
                 wrk_hasalm
               DO p_footer WITH  ;
                  '100000000000001000011',  ;
                  1
               EXIT
          ENDDO
     CASE item = 2
          IF wrk_marca = 1
               SELECT 2
               SET FILTER TO tab_codpre;
= 'MARC'
               GOTO TOP
               wrk_desmar = tab_codtab
               GOTO BOTTOM
               wrk_hasmar = tab_codtab
               SET FILTER TO
               @ 03, 52 SAY  ;
                 SPACE(20)
               RETURN .T.
          ENDIF
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW rangos
          CLEAR
          @ 00, 01 SAY 'Desde :'
          @ 01, 01 SAY 'Hasta :'
          DO WHILE .T.
               wrk_param = 'GC_PRO00.PRO_MARCA'
               wrk_busca = 'MARC'
               wrk_var = 'wrk_desmar'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               wrk_detemp = wrk_desmar
               wrk_hatemp = wrk_hasmar
               @ 00, 09 GET  ;
                 wrk_desmar VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_desmar)
               READ
               IF LASTKEY() = 27
                    wrk_desmar = wrk_detemp
                    RETURN
               ENDIF
               SELECT 2
               SEEK 'MARC' +  ;
                    wrk_desmar
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Marca No Existe'
                    LOOP
               ENDIF
               wrk_var = 'wrk_hasmar'
               @ 01, 09 GET  ;
                 wrk_hasmar VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_hasmar)
               READ
               IF LASTKEY() = 27
                    wrk_desmar = wrk_detemp
                    wrk_hasmar = wrk_hatemp
                    RETURN
               ENDIF
               ON KEY
               SELECT 2
               SEEK 'MARC' +  ;
                    wrk_hasmar
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Marca No Existe'
                    LOOP
               ENDIF
               IF wrk_desmar >  ;
                  wrk_hasmar
                    DO p_mensaje  ;
                       WITH  ;
                       'Rango Pedido Incorrecto'
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        stock
               @ 03, 52 SAY  ;
                 wrk_desmar +  ;
                 ' - ' +  ;
                 wrk_hasmar
               DO p_footer WITH  ;
                  '100000000000001000011',  ;
                  1
               EXIT
          ENDDO
     CASE item = 3
          IF wrk_linea = 1
               SELECT 2
               SET FILTER TO tab_codpre;
= 'LINE'
               GOTO TOP
               wrk_deslin = tab_codtab
               GOTO BOTTOM
               wrk_haslin = tab_codtab
               SET FILTER TO
               @ 05, 52 SAY  ;
                 SPACE(20)
               RETURN .T.
          ENDIF
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW rangos
          CLEAR
          @ 00, 01 SAY 'Desde :'
          @ 01, 01 SAY 'Hasta :'
          DO WHILE .T.
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               wrk_param = 'GC_PRO00.PRO_LINEA'
               wrk_busca = 'LINE'
               wrk_var = 'wrk_deslin'
               wrk_detemp = wrk_deslin
               wrk_hatemp = wrk_haslin
               @ 00, 09 GET  ;
                 wrk_deslin VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_deslin)
               READ
               IF LASTKEY() = 27
                    wrk_deslin = wrk_detemp
                    RETURN
               ENDIF
               SELECT 2
               SEEK 'LINE' +  ;
                    wrk_deslin
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de L¡nea No Existe'
                    LOOP
               ENDIF
               wrk_var = 'wrk_haslin'
               @ 01, 09 GET  ;
                 wrk_haslin VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_haslin)
               READ
               IF LASTKEY() = 27
                    wrk_deslin = wrk_detemp
                    wrk_haslin = wrk_hatemp
                    RETURN
               ENDIF
               ON KEY
               SELECT 2
               SEEK 'LINE' +  ;
                    wrk_haslin
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de L¡nea No Existe'
                    LOOP
               ENDIF
               IF wrk_deslin >  ;
                  wrk_haslin
                    DO p_mensaje  ;
                       WITH  ;
                       'Rango Pedido Incorrecto'
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        stock
               @ 05, 52 SAY  ;
                 wrk_deslin +  ;
                 ' - ' +  ;
                 wrk_haslin
               DO p_footer WITH  ;
                  '100000000000001000011',  ;
                  1
               EXIT
          ENDDO
     CASE item = 4
          IF wrk_catego = 1
               SELECT 2
               SET FILTER TO tab_codpre;
= 'CATE'
               GOTO TOP
               wrk_descat = tab_codtab
               GOTO BOTTOM
               wrk_hascat = tab_codtab
               SET FILTER TO
               @ 07, 52 SAY  ;
                 SPACE(20)
               RETURN .T.
          ENDIF
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW rangos
          CLEAR
          @ 00, 01 SAY 'Desde :'
          @ 01, 01 SAY 'Hasta :'
          DO WHILE .T.
               wrk_param = 'GC_PRO00.PRO_CATEGO'
               wrk_busca = 'CATE'
               wrk_var = 'wrk_descat'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               wrk_detemp = wrk_descat
               wrk_hatemp = wrk_hascat
               @ 00, 09 GET  ;
                 wrk_descat VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_descat)
               READ
               IF LASTKEY() = 27
                    wrk_descat = wrk_detemp
                    RETURN
               ENDIF
               SELECT 2
               SEEK 'CATE' +  ;
                    wrk_descat
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Categor¡a No Existe'
                    LOOP
               ENDIF
               wrk_var = 'wrk_hascat'
               @ 01, 09 GET  ;
                 wrk_hascat VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_hascat)
               READ
               IF LASTKEY() = 27
                    wrk_descat = wrk_detemp
                    wrk_hascat = wrk_hatemp
                    RETURN
               ENDIF
               ON KEY
               SELECT 2
               SEEK 'CATE' +  ;
                    wrk_hascat
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Categor¡a No Existe'
                    LOOP
               ENDIF
               IF wrk_descat >  ;
                  wrk_hascat
                    DO p_mensaje  ;
                       WITH  ;
                       'Rango Pedido Incorrecto'
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        stock
               @ 07, 52 SAY  ;
                 wrk_descat +  ;
                 ' - ' +  ;
                 wrk_hascat
               DO p_footer WITH  ;
                  '100000000000001000011',  ;
                  1
               EXIT
          ENDDO
     CASE item = 5
          IF wrk_subcat = 1
               SELECT 2
               SET FILTER TO tab_codpre;
= 'SUCA'
               GOTO TOP
               wrk_dessub = tab_codtab
               GOTO BOTTOM
               wrk_hassub = tab_codtab
               SET FILTER TO
               @ 09, 52 SAY  ;
                 SPACE(20)
               RETURN .T.
          ENDIF
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW rangos
          CLEAR
          @ 00, 01 SAY 'Desde :'
          @ 01, 01 SAY 'Hasta :'
          DO WHILE .T.
               wrk_param = 'GC_PRO00.PRO_SUBCAT'
               wrk_busca = 'SUCA'
               wrk_var = 'wrk_dessub'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               wrk_detemp = wrk_dessub
               wrk_hatemp = wrk_hassub
               @ 00, 09 GET  ;
                 wrk_dessub VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_dessub)
               READ
               IF LASTKEY() = 27
                    wrk_dessub = wrk_detemp
                    RETURN
               ENDIF
               SELECT 2
               SEEK 'SUCA' +  ;
                    wrk_dessub
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Sub_Categor¡a No Existe'
                    LOOP
               ENDIF
               wrk_var = 'wrk_hassub'
               @ 01, 09 GET  ;
                 wrk_hassub VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_hassub)
               READ
               IF LASTKEY() = 27
                    wrk_dessub = wrk_detemp
                    wrk_hassub = wrk_hatemp
                    RETURN
               ENDIF
               ON KEY
               SELECT 2
               SEEK 'SUCA' +  ;
                    wrk_hassub
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Sub-Categor¡a No Existe'
                    LOOP
               ENDIF
               IF wrk_dessub >  ;
                  wrk_hassub
                    DO p_mensaje  ;
                       WITH  ;
                       'Rango Pedido Incorrecto'
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        stock
               @ 09, 52 SAY  ;
                 wrk_dessub +  ;
                 ' - ' +  ;
                 wrk_hassub
               DO p_footer WITH  ;
                  '100000000000001000011',  ;
                  1
               EXIT
          ENDDO
     CASE item = 6
          IF wrk_rubro = 1
               SELECT 2
               SET FILTER TO tab_codpre;
= 'RUBR'
               GOTO TOP
               wrk_desrub = tab_codtab
               GOTO BOTTOM
               wrk_hasrub = tab_codtab
               SET FILTER TO
               @ 11, 52 SAY  ;
                 SPACE(20)
               RETURN .T.
          ENDIF
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW rangos
          CLEAR
          @ 00, 01 SAY 'Desde :'
          @ 01, 01 SAY 'Hasta :'
          DO WHILE .T.
               wrk_param = 'GC_PRO00.PRO_RUBRO'
               wrk_busca = 'RUBR'
               wrk_var = 'wrk_desrub'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               wrk_detemp = wrk_desrub
               wrk_hatemp = wrk_hasrub
               @ 00, 09 GET  ;
                 wrk_desrub VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_desrub)
               READ
               IF LASTKEY() = 27
                    wrk_desrub = wrk_detemp
                    RETURN
               ENDIF
               SELECT 2
               SEEK 'RUBR' +  ;
                    wrk_desrub
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Rubro No Existe'
                    LOOP
               ENDIF
               wrk_var = 'wrk_hasrub'
               @ 01, 09 GET  ;
                 wrk_hasrub VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_hasrub)
               READ
               IF LASTKEY() = 27
                    wrk_desrub = wrk_detemp
                    wrk_hasrub = wrk_hatemp
                    RETURN
               ENDIF
               ON KEY
               SELECT 2
               SEEK 'RUBR' +  ;
                    wrk_hasrub
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Rubro No Existe'
                    LOOP
               ENDIF
               IF wrk_desrub >  ;
                  wrk_hasrub
                    DO p_mensaje  ;
                       WITH  ;
                       'Rango Pedido Incorrecto'
                    LOOP
               ENDIF
               DO p_footer WITH  ;
                  '100000000000001000011',  ;
                  1
               ACTIVATE WINDOW  ;
                        stock
               @ 11, 52 SAY  ;
                 wrk_desrub +  ;
                 ' - ' +  ;
                 wrk_hasrub
               EXIT
          ENDDO
     CASE item = 7
          IF wrk_subrub = 1
               SELECT 2
               SET FILTER TO tab_codpre;
= 'SURU'
               GOTO TOP
               wrk_dessur = tab_codtab
               GOTO BOTTOM
               wrk_hassur = tab_codtab
               SET FILTER TO
               @ 13, 52 SAY  ;
                 SPACE(20)
               RETURN .T.
          ENDIF
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW rangos
          CLEAR
          @ 00, 01 SAY 'Desde :'
          @ 01, 01 SAY 'Hasta :'
          DO WHILE .T.
               wrk_param = 'GC_PRO00.PRO_SUBRUB'
               wrk_busca = 'SURU'
               wrk_var = 'wrk_dessur'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               wrk_detemp = wrk_dessur
               wrk_hatemp = wrk_hassur
               @ 00, 09 GET  ;
                 wrk_dessur VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_dessur)
               READ
               IF LASTKEY() = 27
                    wrk_dessur = wrk_detemp
                    RETURN
               ENDIF
               SELECT 2
               SEEK 'SURU' +  ;
                    wrk_dessur
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Sub-Rubro No Existe'
                    LOOP
               ENDIF
               wrk_var = 'wrk_hassur'
               @ 01, 09 GET  ;
                 wrk_hassur VALID   ;
                 .NOT.  ;
                 EMPTY(wrk_hassur)
               READ
               IF LASTKEY() = 27
                    wrk_dessur = wrk_detemp
                    wrk_hassur = wrk_hatemp
                    RETURN
               ENDIF
               ON KEY
               SELECT 2
               SEEK 'SURU' +  ;
                    wrk_hassur
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo de Sub-Rubro No Existe'
                    LOOP
               ENDIF
               IF wrk_dessur >  ;
                  wrk_hassur
                    DO p_mensaje  ;
                       WITH  ;
                       'Rango Pedido Incorrecto'
                    LOOP
               ENDIF
               DO p_footer WITH  ;
                  '100000000000001000011',  ;
                  1
               ACTIVATE WINDOW  ;
                        stock
               @ 13, 52 SAY  ;
                 wrk_dessur +  ;
                 ' - ' +  ;
                 wrk_hassur
               EXIT
          ENDDO
     CASE item = 8
          DO imprime
ENDCASE
*
PROCEDURE imprime
PARAMETER wrk_param, wrk_desde,  ;
          wrk_hasta
= ooaviso( ;
  'PREPARANDO IMPRESION . . .')
SELECT gc_alm00.alm_codalm,  ;
       gc_pro00.pro_marca,  ;
       gc_pro00.pro_linea,  ;
       gc_pro00.pro_catego,  ;
       gc_pro00.pro_subcat,  ;
       gc_pro00.pro_coremo,  ;
       gc_pro00.pro_subrub,  ;
       gc_alm00.alm_codpro,  ;
       gc_alm00.alm_stkfis,  ;
       gc_alm00.alm_stkres,  ;
       gc_pro00.pro_descri,  ;
       gc_pro00.pro_modelo FROM  ;
       GC_ALM00, GC_PRO00 WHERE  ;
       gc_pro00.pro_codpro =  ;
       gc_alm00.alm_codpro AND  ;
       (BETWEEN(gc_alm00.alm_codalm,  ;
       wrk_desalm, wrk_hasalm)  ;
       AND  ;
       BETWEEN(gc_pro00.pro_marca,  ;
       wrk_desmar, wrk_hasmar)  ;
       AND  ;
       BETWEEN(gc_pro00.pro_linea,  ;
       wrk_deslin, wrk_haslin)  ;
       AND  ;
       BETWEEN(gc_pro00.pro_catego,  ;
       wrk_descat, wrk_hascat)  ;
       AND  ;
       BETWEEN(gc_pro00.pro_subcat,  ;
       wrk_dessub, wrk_hassub)  ;
       AND  ;
       BETWEEN(gc_pro00.pro_rubro,  ;
       wrk_desrub, wrk_hasrub)  ;
       AND  ;
       BETWEEN(gc_pro00.pro_subrub,  ;
       wrk_dessur, wrk_hassur))  ;
       ORDER BY  ;
       gc_alm00.alm_codalm,  ;
       gc_pro00.pro_subcat,  ;
       gc_pro00.pro_descri INTO  ;
       CURSOR QUERY
COUNT TO wrk_conta
IF wrk_conta = 0
     DEACTIVATE WINDOW winmensaje
     DO p_mensaje WITH  ;
        'No Existen Registros a Listar'
     RETURN
ENDIF
DO WHILE LASTKEY()<>27
     IF SYS(13) = 'OFFLINE'
          swt = f_yesno( ;
                'Impresora fuera de Linea. Intentar nuevamente ' ;
                )
          IF swt
               LOOP
          ELSE
               EXIT
          ENDIF
     ENDIF
     = ooaviso( ;
       'I M P R I M I E N D O . . .' ;
       )
     SET SYSMENU ON
     MODIFY REPORT AGCR0204
     DEACTIVATE WINDOW winmensaje
     EXIT
ENDDO
DO p_mensaje WITH  ;
   ' FIN DE IMPRESION '
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
SET CENTURY ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 10, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 11, 02  ;
       TO 21, 77 TITLE  ;
       ' STOCK VALORIZADO DE TOMA INVENTARIO '  ;
       COLOR SCHEME 18
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED gc_pro00 ORDER codigo
SELECT 3
USE SHARED gc_inv00 ORDER codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'PROCESO',  ;
   'REPORTE TOMA INVENTARIO VALORIZADO',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
w_program = PROGRAM()
ACTIVATE WINDOW detalle
STORE SPACE(1) TO w_busca, w_var
STORE rge_codalm TO w_almini,  ;
      w_almfin
STORE 'P' TO w_prini, w_prfin
DO WHILE .T.
     ACTIVATE WINDOW cabecera
     STORE 1 TO w_copia
     CLEAR
     @ 00, 01 SAY 'Almac‚n :'
     @ 00, 40 SAY 'Propiedad:'
     @ 00, 11 GET w_almini SIZE 1,  ;
       4, 0 PICTURE '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_almini) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 01, 11 GET w_almfin RANGE  ;
       w_almini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       .AND.  .NOT.  ;
       EMPTY(w_almfin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 00, 51 GET w_prini SIZE 1,  ;
       4, 0 FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_prini) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 01, 51 GET w_prfin SIZE 1,  ;
       4, 0 RANGE w_prini  ;
       FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_prfin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 02, 01 SAY 'Informe :'
     @ 03, 11 GET nop1 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVN Resumen ;Detalle'  ;
       WHEN oowhen(VARREAD())  ;
       COLOR SCHEME 24
     @ 02, 40 SAY 'Salida en:'
     @ 03, 51 GET opc DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVTN Pantalla ;Impresora'  ;
       VALID oovalid(VARREAD())  ;
       COLOR SCHEME 24
     READ CYCLE
     IF LASTKEY() = 27
          EXIT
     ENDIF
     = ooimprime(nop1,opc)
ENDDO
RELEASE WINDOW detalle, cabecera
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen
DO CASE
     CASE cwhen = 'W_ALMINI'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          w_busca = 'ALMA'
          w_var = 'W_ALMINI'
          ON KEY LABEL f6 do busca2 with;
w_busca,w_var,'ALMACENES',2 
     CASE cwhen = 'W_ALMFIN'
          ACTIVATE WINDOW  ;
                   cabecera
          w_busca = 'ALMA'
          w_var = 'W_ALMFIN'
          ON KEY LABEL f6 do busca2 with;
w_busca,w_var,'ALMACENES',2
     CASE cwhen = 'W_PRINI'
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_var = 'W_PRINI'
          ON KEY LABEL f6 do busca2 with;
"PROP",wrk_var,'PROPIEDAD',2
     CASE cwhen = 'W_PRFIN'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_var = 'W_PRFIN'
          ON KEY LABEL f6 do busca2 with;
"PROP",wrk_var,'PROPIEDAD',2
     CASE cwhen = 'NOP1'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'OPC'
          IF opc = 2
               @ 04, 65 SAY  ;
                 'Copias:' GET  ;
                 w_copia PICTURE  ;
                 '99' VALID   ;
                 .NOT.  ;
                 EMPTY(w_copia)  ;
                 .AND. w_copia >  ;
                 0 .AND. w_copia <  ;
                 100
               READ
               IF LASTKEY() = 27
                    @ 04, 65 SAY  ;
                      SPACE(10)
                    RETURN .F.
               ENDIF
          ENDIF
     CASE cvalid = 'W_ALMINI'  ;
          .OR. cvalid =  ;
          'W_ALMFIN'
          SELECT ge_tab0
          IF cvalid = 'W_ALMINI'
               SEEK 'ALMA' +  ;
                    w_almini
          ELSE
               SEEK 'ALMA' +  ;
                    w_almfin
          ENDIF
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Almac‚n no Existe'
               RETURN .F.
          ELSE
               IF w_almini >  ;
                  tab_codtab
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 17 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            23)
     CASE cvalid = 'W_PRINI' .OR.  ;
          cvalid = 'W_PRFIN'
          IF cvalid = 'W_PRFIN'
               IF w_prfin <  ;
                  w_prini
                    RETURN .F.
               ENDIF
          ENDIF
          SELECT ge_tab0
          IF cvalid = 'W_PRINI'
               SEEK 'PROP' +  ;
                    w_prini
          ELSE
               SEEK 'PROP' +  ;
                    w_prfin
          ENDIF
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo no Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 56 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
ENDCASE
RETURN
*
PROCEDURE ooimprime
PARAMETER nop1, opc
= ooaviso('P R O C E S A N D O')
SELECT DISTINCT  ;
       gc_inv00.alm_codpro,  ;
       gc_inv00.alm_codalm,  ;
       gc_inv00.alm_stkfis,  ;
       gc_inv00.alm_stkres,  ;
       gc_inv00.alm_ubicac,  ;
       gc_inv00.alm_conteo,  ;
       gc_inv00.alm_coprdo,  ;
       gc_inv00.alm_coprso FROM  ;
       gc_inv00 WHERE  ;
       BETWEEN(gc_inv00.alm_codalm,  ;
       w_almini, w_almfin) AND  ;
       BETWEEN(gc_inv00.alm_propie,  ;
       SUBSTR(w_prini, 1, 1),  ;
       SUBSTR(w_prfin, 1, 1))  ;
       ORDER BY  ;
       gc_inv00.alm_codalm,  ;
       gc_inv00.alm_codpro INTO  ;
       CURSOR query
SET RELATION TO alm_codpro INTO gc_pro00
IF opc = 1
     w_txt = f_texto()
     = ooaviso( ;
       'C  O  P  I  A  N  D  O . . .' ;
       )
     IF nop1 = 1
          repo form agcp237a to file &w_txt;
summary noconsole
     ELSE
          repo form agcp0237 to file &w_txt;
noconsole
     ENDIF
     DEACTIVATE WINDOW winmensaje
     SET SYSMENU ON
     modi comm &w_txt wind detalle
     SET SYSMENU OFF
     ACTIVATE WINDOW detalle
     CLEAR
     erase &w_txt
ELSE
     DO WHILE .T.
          IF SYS(13) = 'OFFLINE'
               swt = f_yesno( ;
                     'Impresora fuera de Linea. Intentar nuevamente ' ;
                     )
               IF swt
                    LOOP
               ELSE
                    DEACTIVATE WINDOW  ;
                               winmensaje
                    RETURN
               ENDIF
          ELSE
               EXIT
          ENDIF
     ENDDO
     = ooaviso( ;
       'I M P R I M I E N D O . . .' ;
       )
     SET DEVICE TO PRINTER
     @ 0, 0 SAY CHR(27) + CHR(15)
     FOR a = 1 TO w_copia
          IF nop1 = 1
               REPORT FORMAT  ;
                      agcp237a  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      agcp0237 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
     ENDFOR
     SET PRINTER TO
     SET PRINTER OFF
     SET DEVICE TO SCREEN
     RELEASE WINDOW winmensaje
     DO p_mensaje WITH  ;
        ' FIN DE IMPRESION '
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

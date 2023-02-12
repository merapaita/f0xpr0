*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
SET CENTURY ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 11, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 12, 02  ;
       TO 21, 77 TITLE  ;
       ' STOCK VALORIZADO '  ;
       COLOR SCHEME 7
SELECT 1
USE ge_tab0 ORDER codigo
SELECT 2
USE gc_pro00 ORDER marca
SELECT 3
USE gc_alm00 ORDER codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'STOCK FISICO VALORIZADO',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
w_program = PROGRAM()
ACTIVATE WINDOW detalle
STORE rge_codalm TO w_alma1,  ;
      w_alma2
STORE 'P' TO w_prini, w_prfin
STORE '' TO w_dire
DO WHILE .T.
     ACTIVATE WINDOW cabecera
     STORE 1 TO w_copia
     CLEAR
     @ 00, 01 SAY 'Almac‚n:'
     @ 00, 40 SAY 'Propiedad:'
     @ 00, 11 GET w_alma1 SIZE 1,  ;
       4, 0 FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_alma1) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 01, 11 GET w_alma2 SIZE 1,  ;
       4, 0 RANGE w_alma1  ;
       FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_alma2) WHEN  ;
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
     @ 02, 01 SAY 'Informe:'
     @ 03, 11 GET nop1 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVN Resumen ;Detalle'  ;
       WHEN oowhen(VARREAD())  ;
       COLOR SCHEME 24
     @ 02, 40 SAY 'Salida en:'
     @ 03, 51 GET opc DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVTN Pantalla ;Impresora; Archivo'  ;
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
     CASE cwhen = 'W_ALMA1'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_var = 'W_ALMA1'
          ON KEY LABEL f6 do busca2 with;
"ALMA",wrk_var,'ALMACENES',2
     CASE cwhen = 'W_ALMA2'
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_var = 'W_ALMA2'
          ON KEY LABEL f6 do busca2 with;
"ALMA",wrk_var,'ALMACENES',2
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
                 'Copias:'
               @ 04, 72 GET  ;
                 w_copia PICTURE  ;
                 '99' VALID   ;
                 .NOT.  ;
                 EMPTY(w_copia)  ;
                 .AND. w_copia >  ;
                 0 .AND. w_copia <  ;
                 100 COLOR SCHEME  ;
                 8
               READ
          ENDIF
     CASE cvalid = 'W_ALMA1' .OR.  ;
          cvalid = 'W_ALMA2'
          IF cvalid = 'W_ALMA2'
               IF w_alma2 <  ;
                  w_alma1
                    RETURN .F.
               ENDIF
          ENDIF
          SELECT ge_tab0
          IF cvalid = 'W_ALMA1'
               SEEK 'ALMA' +  ;
                    w_alma1
          ELSE
               SEEK 'ALMA' +  ;
                    w_alma2
          ENDIF
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Almac‚n no Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 16 SAY  ;
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
= ooaviso( ;
  'P R O C E S A N D O . . .')
SELECT DISTINCT  ;
       gc_pro00.pro_codpro,  ;
       gc_pro00.pro_descri,  ;
       gc_pro00.pro_marca,  ;
       gc_pro00.pro_coprmb,  ;
       gc_pro00.pro_coprmo,  ;
       gc_pro00.pro_coremo,  ;
       pro_ultcom,  ;
       gc_alm00.alm_codpro,  ;
       gc_alm00.alm_stkfis,  ;
       gc_alm00.alm_codalm,  ;
       gc_alm00.alm_ubicac FROM  ;
       gc_pro00, gc_alm00 WHERE  ;
       gc_pro00.pro_codpro =  ;
       gc_alm00.alm_codpro AND  ;
       BETWEEN(gc_alm00.alm_codalm,  ;
       w_alma1, w_alma2) AND  ;
       gc_alm00.alm_stkfis > 0  ;
       AND  ;
       BETWEEN(gc_pro00.pro_propie,  ;
       w_prini, w_prfin) ORDER BY  ;
       gc_pro00.pro_marca,  ;
       gc_pro00.pro_codpro,  ;
       gc_alm00.alm_codalm INTO  ;
       CURSOR query
DO CASE
     CASE opc = 1
          w_txt = f_texto()
          IF nop1 = 1
               repo form agcr0237 to file;
&w_txt summary noconsole
          ELSE
               SET SYSMENU ON
               repo form agcr237a to file;
&w_txt noconsole
          ENDIF
          DEACTIVATE WINDOW  ;
                     winmensaje
          SET SYSMENU ON
          modi comm &w_txt wind detalle;
noedit
          SET SYSMENU OFF
          erase &w_txt
          DEACTIVATE WINDOW  ;
                     winmensaje
          ACTIVATE WINDOW detalle
          CLEAR
     CASE opc = 2
          DO WHILE .T.
               IF SYS(13) =  ;
                  'OFFLINE'
                    swt = f_yesno( ;
                          'Impresora fuera de Linea. Intentar nuevamente ' ;
                          )
                    IF swt
                         LOOP
                    ELSE
                         DEACTIVATE  ;
                          WINDOW  ;
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
          @ 0, 0 SAY CHR(27) +  ;
            CHR(15)
          FOR a = 1 TO w_copia
               IF nop1 = 1
                    REPORT FORMAT  ;
                           agcr0237  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           agcr237a  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
          ENDFOR
          SET PRINTER TO
          SET PRINTER OFF
          SET DEVICE TO SCREEN
          RELEASE WINDOW  ;
                  winmensaje
     CASE opc = 3
          = ooruta()
          ACTIVATE WINDOW detalle
          @ 05, 10 SAY '>>> ' +  ;
            w_dire + ' <<<'
          = ooaviso( ;
            'C O P I A N D O . . .' ;
            )
          SELECT query
          IF nop1 = 1
               repo form agcr0237 to file;
&w_dire summary noconsole
          ELSE
               repo form agcr237a to file;
&w_dire noconsole
          ENDIF
          RELEASE WINDOW  ;
                  winmensaje
          ACTIVATE WINDOW detalle
          CLEAR
ENDCASE
DO p_mensaje WITH  ;
   ' FIN DE IMPRESION '
RETURN
*
PROCEDURE ooruta
SELECT ge_tab0
SEEK 'RUTA' + 'COMM'
IF FOUND()
     w_ruta = tab_destab
ELSE
     w_ruta = SPACE(30)
ENDIF
w_ruta = ALLTRIM(w_ruta)
w_dire = w_ruta + 'SV' +  ;
         ALLTRIM(STR(MONTH(DATE()))) +  ;
         rge_codalm + '.TXT'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

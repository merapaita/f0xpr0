*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
SET CENTURY ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 09, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 10, 02  ;
       TO 21, 77 COLOR SCHEME 7
SELECT 1
USE gc_pro00 ORDER codigo
SELECT 2
USE gc_alm00 ORDER codpro
SELECT 3
CREATE CURSOR curvas (codigo C  ;
       (14), despro C (40), curva  ;
       C (4), rcom C (1), marca C  ;
       (4), stock N (9, 2), cpsol  ;
       N (9, 2), cpdol N (9, 2))
INDEX ON marca + curva + rcom TAG  ;
      codigo
SELECT 4
USE ge_tab0 ORDER codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'CURVA DE VENTAS', 'REPORTE'
@ 02, 63 SAY PROGRAM()
w_program = PROGRAM()
ACTIVATE WINDOW detalle
sigue = .T.
DO WHILE sigue
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     STORE 1 TO w_copia
     SET CURSOR ON
     @ 00, 01 SAY 'Tipo :'
     @ 00, 40 SAY 'Destino :'
     @ 01, 01 GET nop1 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVN Resumen ;Detalle'  ;
       COLOR SCHEME 12
     @ 01, 40 GET opc DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVTN Pantalla ;Impresora ;Archivo en A:'  ;
       COLOR SCHEME 12
     READ CYCLE
     IF LASTKEY() = 27
          sigue = .F.
     ELSE
          = proceso()
          = imprime()
     ENDIF
ENDDO
CLOSE DATABASES
ON KEY
DEACTIVATE WINDOW tablas, footer,  ;
           detalle
RELEASE WINDOW cabecera,  ;
        winmensaje, detalle
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE proceso
= ooaviso( ;
  ' T R A B A J A N D O . . .')
SELECT gc_pro00
GOTO TOP
SCAN WHILE  .NOT. EOF()
     w_codigo = pro_codpro
     w_despro = pro_descri
     w_curva = pro_nivcit
     w_rcom = pro_rcom
     w_cpdol = pro_coprmo
     w_cpsol = pro_coprmb
     w_marca = pro_marca
     SELECT gc_alm00
     SEEK w_codigo
     IF FOUND()
          w_stock = 0
          SCAN WHILE alm_codpro =  ;
               w_codigo
               w_stock = alm_stkfis +  ;
                         w_stock
          ENDSCAN
          IF w_stock <> 0
               SELECT 3
               APPEND BLANK
               REPLACE codigo  ;
                       WITH  ;
                       w_codigo
               REPLACE despro  ;
                       WITH  ;
                       w_despro
               REPLACE curva WITH  ;
                       w_curva
               REPLACE rcom WITH  ;
                       w_rcom
               REPLACE marca WITH  ;
                       w_marca
               REPLACE stock WITH  ;
                       w_stock
               REPLACE cpsol WITH  ;
                       w_cpsol
               REPLACE cpdol WITH  ;
                       w_cpdol
          ENDIF
     ENDIF
     SELECT gc_pro00
ENDSCAN
SELECT 3
*
PROCEDURE imprime
DO CASE
     CASE opc = 1
          w_file = SUBSTR(f_archivo(),  ;
                   1, 8) +  ;
                   '.doc'
          = ooaviso( ;
            'C  O  P  I  A  N  D  O . . .' ;
            )
          IF nop1 = 1
               REPO FORM AGCR0244 TO FILE;
&w_file SUMMARY NOCONSOLE 
          ELSE
               REPO FORM AGCR0244 TO FILE;
&w_file NOCONSOLE
          ENDIF
          SET SYSMENU ON
          MODI COMM &w_file WIND DETALLE;
NOEDIT
          SET SYSMENU OFF
          ERASE FILE &w_file
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
               ENDIF
               EXIT
          ENDDO
          = ooaviso( ;
            'I M P R I M I E N D O . . .' ;
            )
          SET DEVICE TO PRINTER
          @ 00, 00 SAY CHR(27) +  ;
            CHR(15)
          FOR a = 1 TO w_copia
               IF nop1 = 1
                    REPORT FORMAT  ;
                           AGCR0244  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           AGCR0244  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
          ENDFOR
          SET DEVICE TO PRINTER
          @ 00, 00 SAY CHR(27) +  ;
            CHR(18)
          SET PRINTER TO
          SET DEVICE TO SCREEN
          DEACTIVATE WINDOW  ;
                     winmensaje
     CASE opc = 3
          = ooaviso( ;
            'G R A B A N D O . . .' ;
            )
          IF nop1 = 1
               REPORT FORMAT  ;
                      AGCR0244  ;
                      SUMMARY TO  ;
                      FILE  ;
                      A:CURVRABC.TXT  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      AGCR0244 TO  ;
                      FILE  ;
                      A:CURVDABC.TXT  ;
                      NOCONSOLE
          ENDIF
          DEACTIVATE WINDOW  ;
                     winmensaje
          ACTIVATE WINDOW detalle
          CLEAR
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

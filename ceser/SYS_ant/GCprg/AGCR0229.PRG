*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
SET CENTURY ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 08, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 09, 02  ;
       TO 21, 77 COLOR SCHEME 7
SELECT 1
USE GC_HVE00 ORDER hve_fecdoc
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE GC_CLI00 ORDER CODIGO
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'VENTAS A CREDITO', 'REPORTE'
w_program = PROGRAM()
@ 02, 63 SAY PROGRAM()
ACTIVATE WINDOW detalle
STORE .T. TO sigue
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     w_valigv = ROUND(tab_factor /  ;
                100, 2)
ELSE
     DO p_mensaje WITH  ;
        '** No Esta Definido el '+ ;
        sys_codimp
     sigue = .F.
ENDIF
STORE DATE() TO w_dia, w_dia2
DO WHILE sigue
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     STORE 0 TO nreg
     STORE 1 TO w_copia
     w_mes = MONTH(DATE())
     w_ano = YEAR(DATE())
     SET CURSOR ON
     @ 00, 01 SAY 'Per¡odo :'
     @ 00, 48 SAY 'Destino :'
     @ 01, 01 GET nop1 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVN Diario ;Mensual'  ;
       VALID oovalid(1,nop1)  ;
       COLOR SCHEME 12
     @ 01, 48 GET opc DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVTN Pantalla ;Impresora'  ;
       VALID oovalid(2,opc) COLOR  ;
       SCHEME 12
     READ CYCLE
     IF LASTKEY() = 27
          sigue = .F.
     ELSE
          = proceso(nop1)
          IF nreg = 0
               LOOP
          ELSE
               = imprime()
          ENDIF
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
FUNCTION oovalid
PARAMETER cvalid, op
IF cvalid = 1
     DO CASE
          CASE op = 1
               @ 01, 13 SAY  ;
                 'Del:' COLOR  ;
                 SCHEME 8 GET  ;
                 w_dia PICTURE  ;
                 '@D'
               @ 01, 29 SAY 'Al:'  ;
                 COLOR SCHEME 8  ;
                 GET w_dia2 RANGE  ;
                 w_dia PICTURE  ;
                 '@D'
               READ
               IF LASTKEY() = 27
                    @ 01, 10 SAY  ;
                      SPACE(35)
                    RETURN .F.
               ENDIF
          CASE op = 2
               @ 02, 12 SAY  ;
                 'Mes :'
               @ 02, 21 SAY  ;
                 'A¤o :'
               @ 02, 17 GET w_mes  ;
                 RANGE 1,12  ;
                 PICTURE '99'  ;
                 COLOR SCHEME 8
               @ 02, 26 GET w_ano  ;
                 RANGE 1900  ;
                 PICTURE '9999'  ;
                 COLOR SCHEME 8
               READ
               IF LASTKEY() = 27
                    @ 02, 10 SAY  ;
                      SPACE(20)
                    RETURN .F.
               ENDIF
     ENDCASE
ELSE
     IF op = 2
          @ 02, 52 SAY 'Copias :'  ;
            GET w_copia RANGE 1, ;
            10 PICTURE '99' VALID   ;
            .NOT. EMPTY(w_copia)
          READ
          IF LASTKEY() = 27
               @ 02, 50 SAY  ;
                 SPACE(15)
               RETURN .F.
          ENDIF
     ENDIF
ENDIF
RETURN
*
PROCEDURE proceso
PARAMETER opcion
= ooaviso( ;
  ' T R A B A J A N D O . . .')
CREATE CURSOR credis (hve_tipdoc  ;
       C (4), hve_nrodoc C (10),  ;
       hve_fecdoc D (8),  ;
       hve_codent C (11),  ;
       hve_solrep N (9, 2),  ;
       hve_solmob N (9, 2),  ;
       hve_solfle N (9, 2),  ;
       hve_solnet N (9, 2),  ;
       hve_soldes N (9, 2),  ;
       hve_solvta N (9, 2),  ;
       hve_soligv N (9, 2),  ;
       hve_solgen N (9, 2),  ;
       hve_pagcts N (9, 2),  ;
       hve_mtocan N (9, 2),  ;
       hve_tippag C (4),  ;
       hve_totoim N (9, 2),  ;
       hve_estdoc C (4),  ;
       hve_codmon C (4))
INDEX ON hve_codent +  ;
      DTOS(hve_fecdoc) +  ;
      hve_nrodoc TAG codigo
SELECT gc_hve00
SET NEAR ON
IF opcion = 1
     SEEK DTOS(w_dia)
ELSE
     w_fecini = '01/' + STR(w_mes,  ;
                2) + '/' +  ;
                STR(w_ano, 4)
     w_fecini = CTOD(w_fecini)
     SEEK DTOS(w_fecini)
ENDIF
SET NEAR OFF
IF opcion = 1
     SCAN WHILE hve_fecdoc <=  ;
          w_dia2 .AND.  .NOT.  ;
          EOF()
          DO carga
     ENDSCAN
ELSE
     SCAN WHILE MONTH(hve_fecdoc) =  ;
          MONTH(w_fecini) .AND.   ;
          .NOT. EOF()
          DO carga
     ENDSCAN
ENDIF
SELECT credis
COUNT TO nreg
DEACTIVATE WINDOW winmensaje
IF nreg = 0
     DO p_mensaje WITH  ;
        ' NO SE ENCONTRO INFORMACION '
ENDIF
RETURN
*
PROCEDURE imprime
IF opc = 1
     w_file = SUBSTR(f_archivo(),  ;
              1, 8) + '.doc'
     = ooaviso( ;
       'C  O  P  I  A  N  D  O . . .' ;
       )
     REPO FORM AGCR0229 TO FILE &w_file;
NOCONSOLE
     REPORT FORMAT AGCR0229 TO  ;
            FILE CREDITO.TXT
     SET SYSMENU ON
     MODI COMM &w_file WIND DETALLE NOEDIT
     SET SYSMENU OFF
     ERASE FILE &w_file
     DEACTIVATE WINDOW winmensaje
     ACTIVATE WINDOW detalle
     CLEAR
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
          REPORT FORMAT AGCR0229  ;
                 TO PRINTER  ;
                 NOCONSOLE
     ENDFOR
     SET DEVICE TO PRINTER
     @ 00, 00 SAY CHR(27) +  ;
       CHR(18)
     SET PRINTER TO
     SET DEVICE TO SCREEN
     DEACTIVATE WINDOW winmensaje
ENDIF
RETURN
*
PROCEDURE carga
IF gc_hve00.hve_tipdoc = 'FACT'  ;
   .OR. gc_hve00.hve_tipdoc =  ;
   'BOLE'
     IF gc_hve00.hve_estdoc <>  ;
        'A' .AND.  ;
        gc_hve00.hve_tippag =  ;
        '002 '
          SELECT credis
          APPEND BLANK
          REPLACE hve_tipdoc WITH  ;
                  gc_hve00.hve_tipdoc,  ;
                  hve_nrodoc WITH  ;
                  gc_hve00.hve_nrodoc
          REPLACE hve_fecdoc WITH  ;
                  gc_hve00.hve_fecdoc,  ;
                  hve_tippag WITH  ;
                  gc_hve00.hve_tippag
          REPLACE hve_codent WITH  ;
                  gc_hve00.hve_codent,  ;
                  hve_estdoc WITH  ;
                  gc_hve00.hve_estdoc
          REPLACE hve_mtocan WITH  ;
                  gc_hve00.hve_mtocan,  ;
                  hve_solrep WITH  ;
                  gc_hve00.hve_solrep
          REPLACE hve_solfle WITH  ;
                  gc_hve00.hve_solfle,  ;
                  hve_soldes WITH  ;
                  gc_hve00.hve_soldes,  ;
                  hve_codmon WITH  ;
                  gc_hve00.hve_codmon
          REPLACE hve_solvta WITH  ;
                  gc_hve00.hve_solvta,  ;
                  hve_soligv WITH  ;
                  gc_hve00.hve_soligv,  ;
                  hve_solmob WITH  ;
                  gc_hve00.hve_solmob
          REPLACE hve_solgen WITH  ;
                  gc_hve00.hve_solgen,  ;
                  hve_pagcts WITH  ;
                  gc_hve00.hve_pagcts,  ;
                  hve_totoim WITH  ;
                  gc_hve00.hve_totoim
          SELECT gc_hve00
     ENDIF
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

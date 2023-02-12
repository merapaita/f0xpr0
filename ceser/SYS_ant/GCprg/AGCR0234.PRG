*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 21, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 04, 02  ;
       TO 21, 77 TITLE  ;
       ' REGISTRO DE VENTAS MENSUAL '  ;
       COLOR SCHEME 7
SELECT 1
USE GC_HVE00 ORDER CODIGO
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE GC_CLI00 ORDER CODIGO
SELECT 4
USE ST_ICLPR
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     w_valigv = tab_factor
     w_igv = 1 + (w_valigv / 100)
ELSE
     w_valigv = 0
ENDIF
w_progra = PROGRAM()
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'REGISTRO DE VENTAS',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
DO WHILE .T.
     DO p_footer WITH  ;
        '100000000001011000001',  ;
        2
     ACTIVATE WINDOW cabecera
     wrk_mes = MONTH(DATE())
     wrk_ano = YEAR(DATE())
     wrk_dia = DATE()
     STORE 1 TO w_proce, w_orden,  ;
           w_tipo, w_desti
     CLEAR
     @ 00, 01 SAY 'Mes :'
     @ 00, 22 SAY 'A¤o :'
     @ 01, 00 TO 01, 75
     @ 02, 01 SAY 'Procesar :'
     @ 02, 35 SAY 'Orden  :'
     @ 05, 01 SAY 'Tipo     :'
     @ 05, 35 SAY 'Salida :'
     @ 07, 00 TO 07, 75
     @ 00, 07 GET wrk_mes RANGE 1, ;
       12 PICTURE '99' VALID  ;
       oovalid(VARREAD()) COLOR  ;
       SCHEME 8
     @ 00, 28 GET wrk_ano RANGE  ;
       1995 PICTURE '9999' VALID  ;
       oovalid(VARREAD()) COLOR  ;
       SCHEME 8
     @ 02, 11 GET w_proce DEFAULT  ;
       1 SIZE 1, 13, 0 PICTURE  ;
       '@*RVN Real;consistencia '  ;
       COLOR SCHEME 12
     @ 02, 43 GET w_orden DEFAULT  ;
       1 SIZE 1, 14, 0 PICTURE  ;
       '@*RVN Por Documento;Por Emisor'  ;
       COLOR SCHEME 12
     @ 05, 11 GET w_tipo DEFAULT  ;
       1 SIZE 1, 8, 0 PICTURE  ;
       '@*RVN Res£men ;Detalle'  ;
       COLOR SCHEME 12
     @ 05, 43 GET w_desti DEFAULT  ;
       1 SIZE 1, 10, 0 PICTURE  ;
       '@*RVTN Pantalla ;Impresora'  ;
       COLOR SCHEME 12
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF w_desti = 2
          w_copias = 1
          @ 06, 58 SAY 'Copias :'
          @ 06, 67 GET w_copias  ;
            RANGE 1,10 PICTURE  ;
            '99' COLOR SCHEME 8
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     = ooaviso( ;
       ' T R A B A J A N D O . . .' ;
       )
     w_file = SUBSTR(f_archivo(),  ;
              1, 8) + '.DBF'
     SELECT gc_hve00
     COPY STRU TO &w_file
     SELECT 5
     USE &w_file EXCLU
     SELECT gc_hve00
     SET ORDER TO HVE_FECDOC
     a = '01/' + IIF(wrk_mes < 10,  ;
         '0' + STR(wrk_mes, 1, 1),  ;
         STR(wrk_mes)) + '/' +  ;
         STR(wrk_ano, 4)
     SET NEAR ON
     SEEK DTOS(CTOD(a))
     SET NEAR OFF
     SCAN WHILE  ;
          MONTH(gc_hve00.hve_fecdoc) =  ;
          wrk_mes .AND.  ;
          YEAR(gc_hve00.hve_fecdoc) =  ;
          wrk_ano
          IF gc_hve00.hve_tipdoc <>  ;
             'ORDE' .AND.  ;
             gc_hve00.hve_tipdoc <>  ;
             'LIQU'
               SCATTER MEMVAR
               SELECT 5
               APPEND BLANK
               GATHER MEMVAR
          ENDIF
          SELECT gc_hve00
     ENDSCAN
     SELECT 5
     COUNT TO nreg
     DEACTIVATE WINDOW winmensaje
     IF nreg = 0
          DO p_mensaje WITH  ;
             ' NO SE ENCONTRO INFORMACION '
          LOOP
     ENDIF
     IF w_proce = 1
          DO procesa
     ENDIF
     DO CASE
          CASE w_orden = 1 .AND.  ;
               w_proce = 1
               INDEX ON  ;
                     hve_tipdoc +  ;
                     hve_nrodoc  ;
                     TAG codigo2
          CASE w_orden = 2 .AND.  ;
               w_proce = 1
               INDEX ON  ;
                     SUBSTR(hve_nrodoc,  ;
                     1, 4) +  ;
                     hve_tipdoc +  ;
                     hve_nrodoc  ;
                     TAG codigo2
          CASE w_proce = 2
               INDEX ON  ;
                     DTOC(hve_fecdoc) +  ;
                     hve_tipent +  ;
                     hve_tipdoc  ;
                     TAG codigo2
     ENDCASE
     = ooimprime()
     SELECT 5
     USE
     DELETE FILE SUBSTR(w_file, 1,  ;
            8) + '.DBF'
     DELETE FILE SUBSTR(w_file, 1,  ;
            8) + '.CDX'
ENDDO
ON KEY
DEACTIVATE WINDOW tablas,  ;
           cabecera, detalle
RELEASE WINDOW winpedido, detalle,  ;
        cabecera
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'WRK_MES'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          @ ROW(), 11 SAY  ;
            SPACE(10)
          @ ROW(), 11 SAY  ;
            oodesmes(wrk_mes)
ENDCASE
RETURN
*
PROCEDURE ooimprime
SELECT 5
SET ORDER TO CODIGO2
IF w_desti = 1
     wrk_file = SUBSTR(f_archivo(),  ;
                1, 8) + '.doc'
     = ooaviso( ;
       'C  O  P  I  A  N  D  O . . .' ;
       )
     DO CASE
          CASE w_proce = 1 .AND.  ;
               w_tipo = 2
               REPO FORM AGCR0241 TO FILE;
&wrk_file  NOCONSOLE
          CASE w_proce = 1 .AND.  ;
               w_tipo = 1
               REPO FORM AGCR0241 TO FILE;
&wrk_file summary NOCONSOLE
          CASE w_proce = 2
               REPO FORM AGCR0234 TO FILE;
&wrk_file summary NOCONSOLE FOR HVE_ESTDOC;
<> "A" AND HVE_ESTDOC <> "N"
     ENDCASE
     SET SYSMENU ON
     MODI COMM &wrk_file WIND DETALLE;
noedit
     SET SYSMENU OFF
     DELE FILE &wrk_file
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
     SELECT 5
     SET ORDER TO CODIGO2
     DO CASE
          CASE w_proce = 1 .AND.  ;
               w_tipo = 2
               REPORT FORMAT  ;
                      AGCR0241 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          CASE w_proce = 1 .AND.  ;
               w_tipo = 1
               REPORT FORMAT  ;
                      AGCR0241  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          CASE w_proce = 2
               REPORT FORMAT  ;
                      AGCR0234 TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR  ;
                      hve_estdoc <>  ;
                      'A' .AND.  ;
                      hve_estdoc <>  ;
                      'N'
     ENDCASE
ENDIF
DEACTIVATE WINDOW winmensaje
CLEAR
RETURN
*
PROCEDURE procesa
SELECT 5
REPLACE hve_cargo WITH 1 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        (SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0009' .OR.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0050')
REPLACE hve_cargo WITH 2 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0048'
REPLACE hve_cargo WITH 3 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0339'
REPLACE hve_cargo WITH 4 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        (SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0073' .OR.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0076')
REPLACE hve_cargo WITH 5 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0309'
REPLACE hve_cargo WITH 6 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0307'
REPLACE hve_cargo WITH 7 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0335'
REPLACE hve_cargo WITH 8 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0336'
REPLACE hve_cargo WITH 9 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0316'
REPLACE hve_cargo WITH 10 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0315'
REPLACE hve_cargo WITH 11 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0329'
REPLACE hve_cargo WITH 12 FOR  ;
        MONTH(hve_fecdoc) =  ;
        wrk_mes .AND.  ;
        YEAR(hve_fecdoc) =  ;
        wrk_ano .AND.  ;
        SUBSTR(hve_nrodoc, 1, 4) =  ;
        '0328'
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
CLOSE DATABASES
SET CENTURY ON
DEFINE WINDOW cabecera FROM 03,  ;
       02 TO 09, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 10, 02  ;
       TO 21, 77 TITLE  ;
       'COSTO DE VENTAS - PRINCIPAL'  ;
       COLOR SCHEME 7
SELECT 1
USE SHARED gc_pro00 ORDER codigo
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
USE SHARED gc_hve00 ORDER  ;
    hve_fecdoc
SELECT 4
USE SHARED gc_dve00 ORDER codigo
SET RELATION TO dve_propar INTO gc_pro00
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'COSTO DE VENTAS', 'REPORTE'
@ 02, 63 SAY PROGRAM()
w_program = PROGRAM()
ACTIVATE WINDOW detalle
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE 'P' TO w_proini, w_profin
STORE SPACE(1) TO w_dire
DO WHILE .T.
     ACTIVATE WINDOW cabecera
     SET CURSOR ON
     STORE 1 TO w_copia, opc1,  ;
           nop1
     @ 01, 24 SAY SPACE(20)
     @ 04, 30 SAY SPACE(30)
     @ 00, 01 SAY 'Per¡odo:'
     @ 01, 01 SAY 'Del:'
     @ 02, 01 SAY 'Al :'
     @ 01, 47 SAY 'Del:'
     @ 02, 47 SAY 'Al :'
     @ 00, 24 SAY 'Orden:'
     @ 00, 47 SAY 'Propiedad:'
     @ 03, 01 SAY  ;
       'Tipo de Informe:'
     @ 04, 01 SAY  ;
       'Tipo de Salida :'
     @ 01, 06 GET w_fecini  ;
       PICTURE '@D' VALID  .NOT.  ;
       EMPTY(w_fecini) COLOR  ;
       SCHEME 8
     @ 02, 06 GET w_fecfin RANGE  ;
       w_fecini PICTURE '@D'  ;
       VALID  .NOT.  ;
       EMPTY(w_fecfin) COLOR  ;
       SCHEME 8
     @ 01, 24 GET opc2 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVN  Diario ;Tipo de Atenci¢n'  ;
       WHEN oowhen(VARREAD())  ;
       COLOR SCHEME 24
     @ 01, 52 GET w_proini SIZE 1,  ;
       4, 0 PICTURE '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_proini) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 02, 52 GET w_profin SIZE 1,  ;
       4, 0 RANGE w_proini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_profin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 03, 18 GET opc3 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RHN  Detalle  ;Resumen  '  ;
       WHEN oowhen(VARREAD())  ;
       COLOR SCHEME 24
     @ 04, 18 GET opc1 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RHTN Pantalla ;Impresora; Archivo'  ;
       VALID oovalid(VARREAD())  ;
       COLOR SCHEME 24
     READ CYCLE
     IF LASTKEY() = 27
          EXIT
     ENDIF
     = ooaviso( ;
       ' T R A B A J A N D O . . .' ;
       )
     CREATE CURSOR query  ;
            (hve_tipdoc C (4),  ;
            hve_nrodoc C (10),  ;
            hve_fecdoc D (8),  ;
            dve_propar C (14),  ;
            dve_cantid N (9, 2),  ;
            dve_coprmo N (9, 2),  ;
            dve_coprmb N (9, 2),  ;
            dve_total N (9, 2),  ;
            hve_indori C (4),  ;
            hve_estdoc C (1),  ;
            hve_codemi C (4))
     SELECT gc_hve00
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     SCAN WHILE hve_fecdoc <=  ;
          w_fecfin .AND.  .NOT.  ;
          EOF()
          IF (hve_tipdoc = 'BOLE'  ;
             .OR. hve_tipdoc =  ;
             'FACT' .OR.  ;
             hve_tipdoc = 'LIQU')  ;
             .AND. hve_estdoc <>  ;
             'A'
               SELECT gc_dve00
               SEEK gc_hve00.hve_tipdoc +  ;
                    gc_hve00.hve_nrodoc
               IF FOUND()
                    SCAN WHILE  ;
                         dve_tipdoc =  ;
                         gc_hve00.hve_tipdoc  ;
                         .AND.  ;
                         dve_nrodoc =  ;
                         gc_hve00.hve_nrodoc  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         SELECT gc_pro00
                         IF gc_pro00.pro_propie >=  ;
                            SUBSTR(w_proini,  ;
                            1, 1)  ;
                            .AND.  ;
                            gc_pro00.pro_propie <=  ;
                            SUBSTR(w_profin,  ;
                            1,  ;
                            1)
                              SELECT  ;
                               query
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               hve_tipdoc  ;
                               WITH  ;
                               gc_hve00.hve_tipdoc,  ;
                               hve_nrodoc  ;
                               WITH  ;
                               gc_hve00.hve_nrodoc,  ;
                               hve_codemi  ;
                               WITH  ;
                               gc_hve00.hve_codemi
                              REPLACE  ;
                               hve_fecdoc  ;
                               WITH  ;
                               gc_hve00.hve_fecdoc,  ;
                               dve_cantid  ;
                               WITH  ;
                               gc_dve00.dve_cantid
                              REPLACE  ;
                               hve_indori  ;
                               WITH  ;
                               gc_hve00.hve_indori,  ;
                               hve_estdoc  ;
                               WITH  ;
                               gc_hve00.hve_estdoc,  ;
                               dve_propar  ;
                               WITH  ;
                               gc_dve00.dve_propar
                              REPLACE  ;
                               dve_total  ;
                               WITH  ;
                               gc_dve00.dve_total,  ;
                               dve_coprmo  ;
                               WITH  ;
                               gc_dve00.dve_coprmo,  ;
                               dve_coprmb  ;
                               WITH  ;
                               gc_dve00.dve_coprmb
                         ENDIF
                         SELECT gc_dve00
                    ENDSCAN
               ENDIF
          ENDIF
          SELECT gc_hve00
     ENDSCAN
     SELECT query
     COUNT TO nreg
     DEACTIVATE WINDOW winmensaje
     IF nreg = 0
          DO p_mensaje WITH  ;
             ' NO SE ENCONTRO INFORMACION '
          LOOP
     ELSE
          SET RELATION TO dve_propar INTO;
gc_pro00
     ENDIF
     IF opc2 = 1
          w_idx = f_indice()
          index on dtos(hve_fecdoc)+hve_tipdoc+hve_nrodoc;
to &w_idx
          GOTO TOP
     ELSE
          w_idx = f_indice()
          IF opc3 = 1
               index on hve_indori+dtos(hve_fecdoc)+hve_tipdoc+hve_nrodoc+dve_propar;
to &w_idx
          ELSE
               index on hve_indori+hve_codemi+dtos(hve_fecdoc)+hve_tipdoc+hve_nrodoc+dve_propar;
to &w_idx
          ENDIF
          GOTO TOP
     ENDIF
     DO CASE
          CASE opc1 = 1
               = ooimprime(1)
          CASE opc1 = 2
               = ooimprime(2)
          CASE opc1 = 3
               = ooimprime(3)
     ENDCASE
ENDDO
RELEASE WINDOW winpedido, detalle,  ;
        cabecera
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
erase &w_idx   
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen
DO CASE
     CASE cwhen = 'W_PROINI'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_var = 'W_PROINI'
          ON KEY LABEL f6 do busca2 with;
"PROP",wrk_var,'PROPIEDAD',2
     CASE cwhen = 'W_PROFIN'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_var = 'W_PROFIN'
          ON KEY LABEL f6 do busca2 with;
"PROP",wrk_var,'PROPIEDAD',2
     CASE cwhen = 'OPC2' .OR.  ;
          cwhen = 'OPC3'
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
     CASE cvalid = 'W_PROINI'  ;
          .OR. cvalid =  ;
          'W_PROFIN'
          IF cvalid = 'W_PROFIN'
               IF w_profin <  ;
                  w_proini
                    RETURN .F.
               ENDIF
          ENDIF
          SELECT ge_tab0
          IF cvalid = 'W_PROINI'
               SEEK 'PROP' +  ;
                    w_proini
          ELSE
               SEEK 'PROP' +  ;
                    w_profin
          ENDIF
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo no Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
     CASE cvalid = 'OPC1'
          IF opc1 = 2
               @ 04, 47 SAY  ;
                 'Copias:'
               @ 04, 55 GET  ;
                 w_copia PICTURE  ;
                 '99' VALID   ;
                 .NOT.  ;
                 EMPTY(w_copia)  ;
                 .AND. w_copia >  ;
                 0 .AND. w_copia <  ;
                 10 COLOR SCHEME  ;
                 8
               READ
          ENDIF
ENDCASE
RETURN
*
PROCEDURE ooimprime
PARAMETER opc
DO WHILE LASTKEY()<>27
     DO CASE
          CASE opc = 1
               = ooaviso( ;
                 'C O P I A N D O . . .' ;
                 )
               w_file = f_texto()
               IF opc2 = 1
                    IF opc3 = 1
                         repo form agcr232A.frx;
to file &w_file  noconsole
                    ELSE
                         repo form agcr232A.frx;
to file &w_file  noconsole summary
                    ENDIF
               ELSE
                    IF opc3 = 1
                         repo form agcr0232.frx;
to file &w_file  noconsole
                    ELSE
                         repo form agcr232b.frx;
to file &w_file  noconsole summary
                    ENDIF
               ENDIF
               modi comm &w_file noedit;
wind detalle
               erase &w_file
               RELEASE WINDOW  ;
                       winmensaje
               ACTIVATE WINDOW  ;
                        detalle
               CLEAR
          CASE opc = 2
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
                         EXIT
                    ENDIF
               ENDIF
               = ooaviso( ;
                 'I M P R I M I E N D O . . .' ;
                 )
               SET DEVICE TO PRINTER
               @ 00, 00 SAY  ;
                 CHR(27) +  ;
                 CHR(15)
               FOR a = 1 TO  ;
                   w_copia
                    IF opc2 = 1
                         IF opc3 =  ;
                            1
                              REPORT  ;
                               FORMAT  ;
                               agcr232A.frx  ;
                               ENVIRONMENT  ;
                               PDSETUP  ;
                               NOEJECT  ;
                               TO  ;
                               PRINTER  ;
                               NOCONSOLE
                         ELSE
                              REPORT  ;
                               FORMAT  ;
                               agcr232A.frx  ;
                               ENVIRONMENT  ;
                               PDSETUP  ;
                               NOEJECT  ;
                               SUMMARY  ;
                               TO  ;
                               PRINTER  ;
                               NOCONSOLE
                         ENDIF
                    ELSE
                         IF opc3 =  ;
                            1
                              REPORT  ;
                               FORMAT  ;
                               agcr0232.frx  ;
                               ENVIRONMENT  ;
                               PDSETUP  ;
                               NOEJECT  ;
                               TO  ;
                               PRINTER  ;
                               NOCONSOLE
                         ELSE
                              REPORT  ;
                               FORMAT  ;
                               agcr232b.frx  ;
                               ENVIRONMENT  ;
                               PDSETUP  ;
                               NOEJECT  ;
                               SUMMARY  ;
                               TO  ;
                               PRINTER  ;
                               NOCONSOLE
                         ENDIF
                    ENDIF
               ENDFOR
               SET PRINTER TO
               SET PRINTER OFF
               SET DEVICE TO SCREEN
               RELEASE WINDOW  ;
                       winmensaje
          CASE opc = 3
               = ooruta()
               = ooaviso( ;
                 'C O P I A N D O . . .' ;
                 )
               SELECT query
               IF opc2 = 1
                    IF opc3 = 1
                         repo form agcr232A.frx;
to file &w_dire  noconsole
                    ELSE
                         repo form agcr232A.frx;
to file &w_dire  noconsole summary
                    ENDIF
               ELSE
                    IF opc3 = 1
                         repo form agcr0232.frx;
to file &w_dire  
                    ELSE
                         repo form agcr232b.frx;
to file &w_dire  noconsole summary
                    ENDIF
               ENDIF
               RELEASE WINDOW  ;
                       winmensaje
               ACTIVATE WINDOW  ;
                        detalle
               CLEAR
     ENDCASE
     EXIT
ENDDO
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
w_dire = w_ruta + 'IV' +  ;
         ALLTRIM(STR(MONTH(w_fecini))) +  ;
         rge_codalm + '.TXT'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

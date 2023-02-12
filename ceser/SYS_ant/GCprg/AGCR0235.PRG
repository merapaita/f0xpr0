*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
SET CENTURY ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 02,  ;
       02 TO 09, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 10, 02  ;
       TO 21, 77 TITLE  ;
       'COSTO DE VENTAS - SUCURSALES'  ;
       COLOR SCHEME 7
SELECT 1
USE SHARED gc_pro00 ORDER codigo
SELECT 2
USE SHARED gc_kar00 ORDER fecdoc
SELECT 3
USE SHARED ge_tab0 ORDER codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'COSTO DE VENTAS - SUCURSALES',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
w_program = PROGRAM()
ACTIVATE WINDOW detalle
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE SPACE(4) TO w_alma1,  ;
      w_alma2
STORE 'P' TO w_proini, w_profin
DO WHILE .T.
     ACTIVATE WINDOW cabecera
     SET CURSOR ON
     STORE 1 TO w_copia, opc1,  ;
           nop1
     @ 00, 01 SAY 'Per¡odo :'
     @ 01, 01 SAY 'Del:'
     @ 02, 01 SAY 'Al :'
     @ 00, 18 SAY 'Almac‚n :'
     @ 01, 18 SAY 'Del:'
     @ 02, 18 SAY 'Al :'
     @ 00, 52 SAY 'Propiedad:'
     @ 01, 52 SAY 'Del:'
     @ 02, 52 SAY 'Al :'
     @ 03, 01 SAY  ;
       'Orden          :'
     @ 04, 01 SAY  ;
       'Tipo de Informe:'
     @ 05, 01 SAY  ;
       'Tipo de Salida :'
     @ 01, 05 GET w_fecini  ;
       PICTURE '@D' VALID  .NOT.  ;
       EMPTY(w_fecini) COLOR  ;
       SCHEME 8
     @ 02, 05 GET w_fecfin RANGE  ;
       w_fecini PICTURE '@D'  ;
       VALID  .NOT.  ;
       EMPTY(w_fecfin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 01, 23 GET w_alma1  ;
       FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 02, 23 GET w_alma2 RANGE  ;
       w_alma1 FUNCTION '@!'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())  ;
       COLOR SCHEME 8
     @ 01, 56 GET w_proini SIZE 1,  ;
       4, 0 FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 02, 56 GET w_profin SIZE 1,  ;
       4, 0 RANGE w_proini  ;
       FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 03, 18 GET opc2 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RHN  Sucursal ;Tipo de Atenci¢n'  ;
       WHEN oowhen(VARREAD())  ;
       COLOR SCHEME 24
     @ 04, 18 GET opc3 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RHN  Detalle  ;Resumen  '  ;
       COLOR SCHEME 24
     @ 05, 18 GET opc1 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RHTN Pantalla ;Impresora'  ;
       COLOR SCHEME 24
     READ CYCLE
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF opc1 = 2
          @ 05, 47 SAY 'Copias :'  ;
            GET w_copia PICTURE  ;
            '99' VALID  .NOT.  ;
            EMPTY(w_copia) .AND.  ;
            w_copia > 0 .AND.  ;
            w_copia < 10
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     = ooaviso( ;
       ' T R A B A J A N D O . . .' ;
       )
     CREATE CURSOR kar (tipdoc C  ;
            (4), fecdoc D (8),  ;
            nrodoc C (10), codmov  ;
            C (4), cantid N (9,  ;
            2), importe N (9, 2),  ;
            tidore C (4), nrdore  ;
            C (10), cosuni N (9,  ;
            2), codpro C (14),  ;
            cosant N (10, 2),  ;
            cosanb N (10, 2),  ;
            almdes C (4))
     SELECT gc_kar00
     SET RELATION TO kar_codpro INTO gc_pro00
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     SCAN WHILE kar_fecdoc <=  ;
          w_fecfin .AND.  .NOT.  ;
          EOF()
          IF kar_tipdoc = 'PLAN'  ;
             .AND.  ;
             SUBSTR(kar_codmov, 1,  ;
             1) = 'E' .AND.  ;
             kar_tidore <> 'PLAN'  ;
             .AND. kar_cantid >  ;
             0
               IF (kar_almdes >=  ;
                  w_alma1 .AND.  ;
                  kar_almdes <=  ;
                  w_alma2) .AND.  ;
                  (gc_pro00.pro_propie >=  ;
                  SUBSTR(w_proini,  ;
                  1, 1) .AND.  ;
                  gc_pro00.pro_propie <=  ;
                  SUBSTR(w_profin,  ;
                  1, 1))
                    SELECT kar
                    APPEND BLANK
                    REPLACE tipdoc  ;
                            WITH  ;
                            gc_kar00.kar_tipdoc,  ;
                            fecdoc  ;
                            WITH  ;
                            gc_kar00.kar_fecdoc,  ;
                            nrodoc  ;
                            WITH  ;
                            gc_kar00.kar_nrodoc,  ;
                            codmov  ;
                            WITH  ;
                            gc_kar00.kar_codmov,  ;
                            cantid  ;
                            WITH  ;
                            gc_kar00.kar_cantid,  ;
                            importe  ;
                            WITH  ;
                            gc_kar00.kar_import,  ;
                            tidore  ;
                            WITH  ;
                            gc_kar00.kar_tidore,  ;
                            nrdore  ;
                            WITH  ;
                            gc_kar00.kar_nrdore,  ;
                            cosuni  ;
                            WITH  ;
                            gc_kar00.kar_cosuni,  ;
                            codpro  ;
                            WITH  ;
                            gc_kar00.kar_codpro,  ;
                            cosant  ;
                            WITH  ;
                            gc_kar00.kar_cosant,  ;
                            cosanb  ;
                            WITH  ;
                            gc_kar00.kar_cosanb,  ;
                            almdes  ;
                            WITH  ;
                            gc_kar00.kar_almdes
               ENDIF
          ENDIF
          SELECT gc_kar00
     ENDSCAN
     SELECT kar
     COUNT TO nreg
     IF nreg = 0
          RELEASE WINDOW  ;
                  winmensaje
          DO p_mensaje WITH  ;
             ' NO SE ENCONTRO INFORMACION '
          LOOP
     ENDIF
     SET RELATION TO codpro INTO gc_pro00
     IF opc2 = 1
          INDEX ON almdes +  ;
                DTOS(fecdoc) +  ;
                tipdoc + nrodoc  ;
                TAG codigo
     ELSE
          INDEX ON almdes +  ;
                nrdore +  ;
                DTOS(fecdoc) +  ;
                tipdoc + nrodoc  ;
                TAG codigo
     ENDIF
     IF opc1 = 1
          = ooimprime(2)
     ELSE
          = ooimprime(3)
     ENDIF
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
PROCEDURE ooimprime
PARAMETER opc
GOTO TOP
IF opc = 2
     = ooaviso( ;
       'C O P I A N D O . . .')
     w_txt = f_texto()
     IF opc2 = 1
          IF opc3 = 1
               report form agcr0235.frx;
to file &w_txt  noconsole
          ELSE
               report form agcr0235.frx;
to file &w_txt  noconsole summary
          ENDIF
     ELSE
          IF opc3 = 1
               report form agcr235a.frx;
to file &w_txt  noconsole
          ELSE
               report form agcr235a.frx;
to file &w_txt  noconsole summary
          ENDIF
     ENDIF
     SET SYSMENU ON
     modi comm &w_txt noedit wind detalle
     SET SYSMENU OFF
     delete file &w_txt
     RELEASE WINDOW winmensaje
     ACTIVATE WINDOW detalle
     CLEAR
ELSE
     = ooaviso( ;
       'I M P R I M I E N D O . . .' ;
       )
     SET PRINTER ON
     SET DEVICE TO PRINTER
     @ 0, 0 SAY CHR(27) + CHR(15)
     FOR t = 1 TO w_copia
          IF opc2 = 1
               IF opc3 = 1
                    REPORT FORMAT  ;
                           agcr0235.frx  ;
                           NOEJECT  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           agcr0235.frx  ;
                           NOEJECT  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
          ELSE
               IF opc3 = 1
                    REPORT FORMAT  ;
                           agcr235a.frx  ;
                           NOEJECT  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           agcr235a.frx  ;
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
     RELEASE WINDOW winmensaje
     DO p_mensaje WITH  ;
        ' FIN DE IMPRESION '
ENDIF
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
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_var = 'W_ALMA2'
          ON KEY LABEL f6 do busca2 with;
"ALMA",wrk_var,'ALMACENES',2
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
          cwhen = 'W_FECFIN'
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
     CASE cvalid = 'W_ALMA1' .OR.  ;
          cvalid = 'W_ALMA2'
          SELECT ge_tab0
          IF cvalid = 'W_ALMA1'
               IF EMPTY(w_alma1)
                    DO p_mensaje  ;
                       WITH  ;
                       'No se aceptan Blancos'
                    RETURN .F.
               ENDIF
               IF w_alma1 =  ;
                  rge_codalm
                    DO p_mensaje  ;
                       WITH  ;
                       'No se puede usar como Sucursal'
                    RETURN .F.
               ENDIF
               SEEK 'ALMA' +  ;
                    w_alma1
          ELSE
               IF EMPTY(w_alma1)
                    DO p_mensaje  ;
                       WITH  ;
                       'No se aceptan Blancos'
                    RETURN .F.
               ENDIF
               IF w_alma2 =  ;
                  rge_codalm
                    DO p_mensaje  ;
                       WITH  ;
                       'No se puede usar como Sucursal'
                    RETURN .F.
               ENDIF
               SEEK 'ALMA' +  ;
                    w_alma2
          ENDIF
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Almac‚n no Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 28 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
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
          @ ROW(), 61 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            10)
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

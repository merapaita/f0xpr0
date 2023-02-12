*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE POPUP bus_mov FROM 06, 10  ;
       TO 10, 46 PROMPT FIELDS  ;
       query.tab_codtab + '³' +  ;
       query.tab_destab IN screen  ;
       COLOR SCHEME 8
ON SELECTION POPUP bus_mov DEAC POPUP;
bus_mov
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 21, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 04, 02  ;
       TO 21, 77 TITLE  ;
       ' INGRESO DE PRODUCTOS '  ;
       COLOR SCHEME 7
SELECT 1
USE GE_TAB0 ORDER CODIGO
SELECT 2
USE GC_PRO00 ORDER MARCA
SELECT 3
USE GC_KAR00 ORDER FECDOC
SELECT 4
USE GC_CLI00 ORDER CODIGO
w_progra = PROGRAM()
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'REGISTRO DE COMPRAS',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
STORE SPACE(4) TO w_codini,  ;
      w_codfin
STORE SPACE(11) TO w_prvini,  ;
      w_prvfin
STORE rge_codalm TO w_almini,  ;
      w_almfin
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE 1 TO w_copia, w_tipo,  ;
      w_orden, w_desti
DO WHILE .T.
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 00, 01 SAY 'Per¡odo :'  ;
       COLOR N/W 
     @ 01, 07 SAY 'Del'
     @ 02, 07 SAY ' Al'
     @ 00, 34 SAY 'Tipo de Mov.:'  ;
       COLOR N/W 
     @ 01, 44 SAY 'Del'
     @ 02, 44 SAY ' Al'
     @ 04, 01 SAY 'Almac‚n :'  ;
       COLOR N/W 
     @ 05, 07 SAY 'Del'
     @ 06, 07 SAY ' Al'
     @ 04, 34 SAY 'Orden       :'  ;
       COLOR N/W 
     @ 08, 01 SAY 'Tipo    :'  ;
       COLOR N/W 
     @ 12, 01 SAY 'Destino :'  ;
       COLOR N/W 
     @ 01, 11 GET w_fecini  ;
       PICTURE '@D' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_fecini) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 02, 11 GET w_fecfin RANGE  ;
       w_fecini PICTURE '@D'  ;
       VALID oovalid(VARREAD())  ;
       .AND.  .NOT.  ;
       EMPTY(w_fecfin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 01, 48 GET w_codini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_codini) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 02, 48 GET w_codfin RANGE  ;
       w_codini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       .AND.  .NOT.  ;
       EMPTY(w_codfin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 05, 11 GET w_almini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_almini) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 06, 11 GET w_almfin RANGE  ;
       w_almini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       .AND.  .NOT.  ;
       EMPTY(w_almfin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 05, 48 GET w_orden DEFAULT  ;
       1 SIZE 1, 10, 0 PICTURE  ;
       '@*RVN Movimiento ;Proveedor'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())  ;
       COLOR SCHEME 12
     @ 09, 11 GET w_tipo DEFAULT  ;
       1 SIZE 1, 7, 0 PICTURE  ;
       '@*RVN Detalle ;Resumen'  ;
       WHEN oowhen(VARREAD())  ;
       COLOR SCHEME 12
     @ 13, 11 GET w_desti DEFAULT  ;
       1 SIZE 1, 7, 0 PICTURE  ;
       '@*RVTN Pantalla ;Impresora'  ;
       COLOR SCHEME 12
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF w_desti = 2
          SET CURSOR ON
          @ 14, 26 SAY 'Copias :'
          @ 14, 35 GET w_copia  ;
            RANGE 1,10 PICTURE  ;
            '99' COLOR SCHEME 8
          READ
          IF LASTKEY() = 27
               @ 14, 26 SAY  ;
                 SPACE(20)
               LOOP
          ENDIF
     ENDIF
     = ooaviso( ;
       'C  O  P  I  A  N  D  O . . .' ;
       )
     SELECT gc_kar00
     wrk_file = SUBSTR(f_archivo(),  ;
                1, 8) + '.DBF'
     COPY STRU TO &wrk_file
     SELECT 5
     USE &wrk_file
     IF w_orden = 1
          INDEX ON kar_codmov +  ;
                DTOS(kar_fecdoc)  ;
                TAG codigo2
     ELSE
          INDEX ON kar_codent +  ;
                DTOS(kar_fecdoc)  ;
                TAG codigo2
     ENDIF
     SET ORDER TO CODIGO2
     SET NEAR ON
     SELECT gc_kar00
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     SELECT gc_kar00
     SCAN WHILE kar_fecdoc <=  ;
          w_fecfin .AND.  .NOT.  ;
          EOF()
          IF kar_codmov >=  ;
             w_codini .AND.  ;
             kar_codmov <=  ;
             w_codfin
               IF kar_codmov <>  ;
                  'IFD ' .AND.  ;
                  kar_codmov <>  ;
                  'IFE ' .AND.  ;
                  kar_codmov <>  ;
                  'IFO '
                    IF kar_almrec >=  ;
                       w_almini  ;
                       .AND.  ;
                       kar_almrec <=  ;
                       w_almfin
                         IF w_orden =  ;
                            2
                              IF SUBSTR(kar_codent,  ;
                                 1,  ;
                                 11) >=  ;
                                 w_prvini  ;
                                 .AND.  ;
                                 SUBSTR(kar_codent,  ;
                                 1,  ;
                                 11) <=  ;
                                 w_prvfin
                                   = oograba()
                              ENDIF
                         ELSE
                              = oograba()
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
          SELECT gc_kar00
     ENDSCAN
     SELECT 5
     COUNT TO w_canti
     IF w_canti = 0
          DEACTIVATE WINDOW  ;
                     winmensaje
          DO p_mensaje WITH  ;
             'No hay informaci¢n...'
     ELSE
          = ooimprime()
     ENDIF
     SELECT 5
     USE
     DELE FILE &wrk_file
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
PROCEDURE oograba
SCATTER MEMVAR
SELECT 5
APPEND BLANK
GATHER MEMVAR
*
PROCEDURE oowhen
PARAMETER cwhen
DO CASE
     CASE cwhen = 'W_FECINI'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
     CASE cwhen = 'W_FECFIN'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
     CASE cwhen = 'W_CODINI'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          ON KEY LABEL F6 DO bus_mov WITH;
w_codini
     CASE cwhen = 'W_CODFIN'
          ON KEY LABEL F6 DO bus_mov WITH;
w_codfin
     CASE cwhen = 'W_ALMINI'
          wrk_busca = 'ALMA'
          wrk_var = 'w_almini'
          ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
     CASE cwhen = 'W_ALMFIN'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_busca = 'ALMA'
          wrk_var = 'w_almfin'
          ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
     CASE cwhen = 'W_PRVINI'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_busca = 'P'
          wrk_var = 'w_prvini'
          ON KEY LABEL F6 DO entidad WITH;
WRK_BUSCA,WRK_VAR
     CASE cwhen = 'W_PRVFIN'
          wrk_busca = 'P'
          wrk_var = 'w_prvfin'
          ON KEY LABEL F6 DO entidad WITH;
WRK_BUSCA,WRK_VAR
     CASE cwhen = 'W_ORDEN'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
     CASE cwhen = 'W_TIPO'
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
     CASE cvalid = 'W_FECINI'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
     CASE cvalid = 'W_CODINI'  ;
          .OR. cvalid =  ;
          'W_CODFIN'
          SELECT ge_tab0
          IF cvalid = 'W_CODINI'
               SEEK 'RECE' +  ;
                    w_codini
          ELSE
               SEEK 'RECE' +  ;
                    w_codfin
          ENDIF
          IF w_codini = 'IFD '  ;
             .OR. w_codini =  ;
             'IFE ' .OR. w_codini =  ;
             'IFO ' .OR. w_codfin =  ;
             'IFD ' .OR. w_codfin =  ;
             'IFE ' .OR. w_codfin =  ;
             'IFO '
               DO p_mensaje WITH  ;
                  'C¢digo no es para registro de Compras'
               RETURN .F.
          ENDIF
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Movimiento No Existe'
               RETURN .F.
          ENDIF
          IF cvalid = 'W_CODFIN'
               IF w_codfin <  ;
                  w_codini
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo mayor al Inicial'
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 53 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
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
                  'C¢digo de Almac‚n No Existe'
               RETURN .F.
          ENDIF
          IF cvalid = 'W_ALMFIN'
               IF w_almfin <  ;
                  w_almini
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo mayor al Inicial'
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 16 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            18)
          IF cvalid = 'W_ALMFIN'
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        cabecera
          ENDIF
     CASE cvalid = 'W_ORDEN'
          IF w_orden = 1
               @ 08, 48 SAY  ;
                 SPACE(30)
               @ 09, 48 SAY  ;
                 SPACE(30)
               @ 10, 48 SAY  ;
                 SPACE(30)
               @ 11, 48 SAY  ;
                 SPACE(27)
          ELSE
               @ 08, 48 SAY  ;
                 'Prov. Inicial :'
               @ 10, 48 SAY  ;
                 'Prov. Final   :'
               @ 08, 63 GET  ;
                 w_prvini PICTURE  ;
                 '@!' VALID  ;
                 oovali2(VARREAD())  ;
                 .AND.  .NOT.  ;
                 EMPTY(w_prvini)  ;
                 WHEN  ;
                 oowhen(VARREAD())  ;
                 COLOR SCHEME 08
               @ 10, 63 GET  ;
                 w_prvfin RANGE  ;
                 w_prvini PICTURE  ;
                 '@!' VALID  ;
                 oovali2(VARREAD())  ;
                 .AND.  .NOT.  ;
                 EMPTY(w_prvfin)  ;
                 WHEN  ;
                 oowhen(VARREAD())  ;
                 COLOR SCHEME 08
               READ
               IF LASTKEY() = 27
                    @ 08, 48 SAY  ;
                      SPACE(30)
                    @ 09, 48 SAY  ;
                      SPACE(30)
                    @ 10, 48 SAY  ;
                      SPACE(30)
                    @ 11, 48 SAY  ;
                      SPACE(27)
                    w_orden = 1
                    RETURN .F.
               ENDIF
          ENDIF
ENDCASE
RETURN
*
FUNCTION oovali2
PARAMETER cvali
IF cvali = 'W_PRVINI' .OR. cvali =  ;
   'W_PRVFIN'
     IF cvali = 'W_PRVINI'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
     ENDIF
     SELECT gc_cli00
     SET ORDER TO CODIGO
     IF cvali = 'W_PRVINI'
          SEEK 'P' + w_prvini
     ELSE
          SEEK 'P' + w_prvfin
     ENDIF
     IF  .NOT. FOUND()
          DO p_mensaje WITH  ;
             'C¢digo de Proveedor No Existe'
          RETURN .F.
     ENDIF
     IF cvali = 'W_PRVFIN'
          IF w_prvfin < w_prvini
               DO p_mensaje WITH  ;
                  'C¢digo mayor al Inicial'
               RETURN .F.
          ENDIF
     ENDIF
     @ ROW(), 48 SAY  ;
       SUBSTR(cli_razsoc, 1, 25)
ENDIF
*
PROCEDURE ooimprime
SELECT 5
GOTO TOP
IF w_desti = 1
     wrk_file = SUBSTR(f_archivo(),  ;
                1, 8) + '.doc'
     = ooaviso( ;
       'C  O  P  I  A  N  D  O . . .' ;
       )
     IF w_tipo = 1
          REPO FORM AGCR0243 TO FILE &wrk_file;
NOCONSOLE
     ELSE
          REPO FORM AGCR243A TO FILE &wrk_file;
SUMMARY NOCONSOLE
     ENDIF
     SET SYSMENU ON
     MODI COMM &wrk_file WIND DETALLE;
noedit
     SET SYSMENU OFF
     ERASE FILE &wrk_file
     DEACTIVATE WINDOW winmensaje
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
          ELSE
               EXIT
          ENDIF
     ENDDO
     = ooaviso( ;
       'I M P R I M I E N D O . . .' ;
       )
     SET DEVICE TO PRINTER
     SET PRINTER ON
     @ PROW(), PCOL() SAY CHR(15)
     FOR a = 1 TO w_copia
          IF w_tipo = 1
               REPORT FORMAT  ;
                      AGCR0243 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      AGCR243A  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
     ENDFOR
     SET PRINTER TO
     SET PRINTER OFF
     SET DEVICE TO SCREEN
     RELEASE WINDOW winmensaje
ENDIF
RETURN
*
PROCEDURE bus_mov
PARAMETER wrk_tipo
ON KEY
SELECT DISTINCT  ;
       ge_tab0.tab_codtab,  ;
       ge_tab0.tab_destab FROM  ;
       GE_TAB0 WHERE  ;
       ge_tab0.tab_codpre =  ;
       'RECE' AND  ;
       ge_tab0.tab_codtab <>  ;
       'IFD ' AND tab_codtab <>  ;
       'IFE ' AND tab_codtab <>  ;
       'IFO ' ORDER BY  ;
       ge_tab0.tab_codtab INTO  ;
       CURSOR QUERY
ACTIVATE POPUP bus_mov
IF LASTKEY() = 27
     wrk_tipo = SPACE(4)
ELSE
     wrk_tipo = query.tab_codtab
     KEYBOARD '{ENTER}'
ENDIF
ON KEY LABEL F6 DO bus_mov WITH wrk_tipo
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

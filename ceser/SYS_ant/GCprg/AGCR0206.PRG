*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET DELETED OFF
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE POPUP bus_mov FROM 05, 34  ;
       TO 10, 71 PROMPT FIELDS  ;
       query.tab_codtab + '³' +  ;
       query.tab_destab IN screen  ;
       COLOR SCHEME 8
ON SELECTION POPUP bus_mov DEAC POPUP;
bus_mov
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 19, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 04, 02  ;
       TO 21, 77 TITLE  ;
       ' MOV. POR TIPO DE TRANSACCION '  ;
       COLOR SCHEME 7
SELECT 1
USE GE_TAB0 ORDER CODIGO
SELECT 2
USE GC_PRO00 ORDER MARCA
SELECT 3
USE GC_KAR00 ORDER FECDOC
wrk_progra = PROGRAM()
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'MOV. POR TIPO DE TRANSACCION',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
STORE SPACE(4) TO w_codini,  ;
      w_codfin, w_almini,  ;
      w_almfin, w_codigo
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE 1 TO w_copia, w_tipo
DO WHILE .T.
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 00, 01 SAY  ;
       'C¢digo de Trans.:' COLOR  ;
       N/W 
     @ 01, 15 SAY 'Del'
     @ 02, 15 SAY ' Al'
     @ 00, 40 SAY 'Almac‚n :'  ;
       COLOR N/W 
     @ 01, 46 SAY 'Del'
     @ 02, 46 SAY ' Al'
     @ 04, 01 SAY  ;
       'Per¡odo         :' COLOR  ;
       N/W 
     @ 05, 15 SAY 'Del'
     @ 06, 15 SAY ' Al'
     @ 04, 40 SAY 'Tipo    :'  ;
       COLOR N/W 
     @ 08, 01 SAY  ;
       'Destino         :' COLOR  ;
       N/W 
     @ 01, 19 GET w_codini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_codini) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 02, 19 GET w_codfin RANGE  ;
       w_codini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       .AND.  .NOT.  ;
       EMPTY(w_codfin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 01, 50 GET w_almini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_almini) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 02, 50 GET w_almfin RANGE  ;
       w_almini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       .AND.  .NOT.  ;
       EMPTY(w_almfin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 05, 19 GET w_fecini  ;
       PICTURE '@D' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_fecini) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 06, 19 GET w_fecfin RANGE  ;
       w_fecini PICTURE '@D'  ;
       VALID oovalid(VARREAD())  ;
       .AND.  .NOT.  ;
       EMPTY(w_fecfin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 05, 50 GET w_tipo DEFAULT  ;
       1 SIZE 1, 7, 0 PICTURE  ;
       '@*RVN Detalle ;Resumen'  ;
       COLOR SCHEME 12
     @ 08, 19 GET w_desti DEFAULT  ;
       1 SIZE 1, 7, 0 PICTURE  ;
       '@*RHTN Pantalla ;Impresora'  ;
       COLOR SCHEME 12
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF w_desti = 2
          SET CURSOR ON
          @ 08, 50 SAY 'Copias :'
          @ 08, 59 GET w_copia  ;
            RANGE 1,10 PICTURE  ;
            '99' COLOR SCHEME 8
          READ
          IF LASTKEY() = 27
               @ 08, 40 SAY  ;
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
     SELECT 4
     USE &wrk_file
     INDEX ON kar_codmov +  ;
           kar_codpro TAG  ;
           codigo2
     SET ORDER TO CODIGO2
     SELECT gc_kar00
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     SELECT gc_kar00
     SCAN WHILE kar_fecdoc <=  ;
          w_fecfin
          IF kar_codmov >=  ;
             w_codini .AND.  ;
             kar_codmov <=  ;
             w_codfin
               a = 0
               IF kar_almrec =  ;
                  SPACE(4)
                    IF kar_almdes >=  ;
                       w_almini  ;
                       .AND.  ;
                       kar_almdes <=  ;
                       w_almfin
                         SCATTER MEMVAR
                         a = 1
                    ENDIF
               ENDIF
               IF kar_almdes =  ;
                  SPACE(4)
                    IF kar_almrec >=  ;
                       w_almini  ;
                       .AND.  ;
                       kar_almrec <=  ;
                       w_almfin
                         SCATTER MEMVAR
                         a = 1
                    ENDIF
               ENDIF
               IF kar_almdes <>  ;
                  SPACE(4) .AND.  ;
                  kar_almrec <>  ;
                  SPACE(4)
                    IF kar_almdes >=  ;
                       w_almfin  ;
                       .AND.  ;
                       kar_almdes <=  ;
                       w_almfin
                         SCATTER MEMVAR
                         a = 1
                    ELSE
                         IF kar_almrec >=  ;
                            w_almfin  ;
                            .AND.  ;
                            kar_almrec <=  ;
                            w_almfin
                              SCATTER  ;
                               MEMVAR
                              a =  ;
                               1
                         ENDIF
                    ENDIF
               ENDIF
               IF a = 1
                    SELECT 4
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
          ENDIF
          SELECT gc_kar00
     ENDSCAN
     SELECT 4
     = ooimprime()
     SELECT 4
     USE
     DELE FILE &wrk_file
ENDDO
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
RELEASE WINDOW detalle, cabecera
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen
DO CASE
     CASE cwhen = 'W_CODINI'
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
          wrk_busca = 'ALMA'
          wrk_var = 'w_almfin'
          ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
     CASE cwhen = 'W_FECINI'
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
     CASE cvalid = 'W_CODINI'  ;
          .OR. cvalid =  ;
          'W_CODFIN'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          DO CASE
               CASE SUBSTR(w_codini,  ;
                    1, 1) = 'A'  ;
                    .OR. w_codini =  ;
                    'EMER' .OR.  ;
                    SUBSTR(w_codfin,  ;
                    1, 1) = 'A'  ;
                    .OR. w_codfin =  ;
                    'EMER'
                    w_codigo = 'AJUS'
               CASE SUBSTR(w_codini,  ;
                    1, 1) = 'I'  ;
                    .OR.  ;
                    SUBSTR(w_codfin,  ;
                    1, 1) = 'I'
                    w_codigo = 'RECE'
               CASE SUBSTR(w_codini,  ;
                    1, 1) = 'E'  ;
                    .OR.  ;
                    SUBSTR(w_codfin,  ;
                    1, 1) = 'E'
                    w_codigo = 'DESP'
               CASE SUBSTR(w_codini,  ;
                    1, 1) = 'T'  ;
                    .OR.  ;
                    SUBSTR(w_codfin,  ;
                    1, 1) = 'T'
                    w_codigo = 'TRAS'
               CASE SUBSTR(w_codini,  ;
                    1, 1) = 'C'  ;
                    .OR.  ;
                    SUBSTR(w_codfin,  ;
                    1, 1) = 'C'
                    w_codigo = 'CONS'
               OTHERWISE
          ENDCASE
          SELECT ge_tab0
          IF cvalid = 'W_CODINI'
               SEEK w_codigo +  ;
                    w_codini
          ELSE
               SEEK w_codigo +  ;
                    w_codfin
          ENDIF
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Transacci¢n No Existe'
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
          @ ROW(), 24 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            18)
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
          @ ROW(), 55 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            18)
ENDCASE
RETURN
*
PROCEDURE ooimprime
SELECT 4
IF w_desti = 1
     wrk_file = SUBSTR(f_archivo(),  ;
                1, 8) + '.doc'
     = ooaviso( ;
       'C  O  P  I  A  N  D  O . . .' ;
       )
     IF w_tipo = 1
          REPO FORM AGCR0206 TO FILE &wrk_file;
NOCONSOLE
     ELSE
          REPO FORM AGCR0206 TO FILE &wrk_file;
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
                      AGCR0206 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      AGCR0206  ;
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
DO p_mensaje WITH  ;
   ' FIN DE IMPRESION '
RETURN
*
PROCEDURE bus_mov
PARAMETER w_tipo2
ON KEY
SELECT DISTINCT  ;
       ge_tab0.tab_codtab,  ;
       ge_tab0.tab_destab FROM  ;
       GE_TAB0 WHERE  ;
       ge_tab0.tab_codpre =  ;
       'RECE' OR  ;
       (ge_tab0.tab_codpre =  ;
       'DESP') OR  ;
       (ge_tab0.tab_codpre =  ;
       'TRAS') OR  ;
       (ge_tab0.tab_codpre =  ;
       'CONS') OR  ;
       (ge_tab0.tab_codpre =  ;
       'AJUS') ORDER BY  ;
       ge_tab0.tab_codtab INTO  ;
       CURSOR QUERY
ACTIVATE POPUP bus_mov
IF LASTKEY() = 27
     w_tipo2 = SPACE(4)
ELSE
     w_tipo2 = query.tab_codtab
     KEYBOARD '{ENTER}'
ENDIF
ON KEY LABEL F6 DO bus_mov WITH w_tipo2
RETURN
*
FUNCTION detabla
PARAMETER valor
DO CASE
     CASE SUBSTR(valor, 1, 1) =  ;
          'A' .OR. valor =  ;
          'EMER'
          w_valor = 'AJUS'
     CASE SUBSTR(valor, 1, 1) =  ;
          'I'
          w_valor = 'RECE'
     CASE SUBSTR(valor, 1, 1) =  ;
          'E'
          w_valor = 'DESP'
     CASE SUBSTR(valor, 1, 1) =  ;
          'T'
          w_valor = 'TRAS'
     CASE SUBSTR(valor, 1, 1) =  ;
          'C'
          w_valor = 'CONS'
ENDCASE
SELECT ge_tab0
SEEK w_valor + valor
IF FOUND()
     RETURN SUBSTR(tab_destab, 1,  ;
            30)
ELSE
     RETURN SPACE(20)
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

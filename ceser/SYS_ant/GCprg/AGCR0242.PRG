*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
CLOSE DATABASES
SET CENTURY ON
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
       ' AJUSTE DE INVENTARIO '  ;
       COLOR SCHEME 7
SELECT 1
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 2
USE SHARED GC_PRO00 ORDER CODIGO
SELECT 3
USE SHARED GC_KAR00 ORDER FECDOC
SET RELATION TO kar_codpro INTO gc_pro00
SELECT 4
USE SHARED GC_CMV00 ORDER  ;
    cmv_feinmo
w_program = PROGRAM()
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'REPORTE DE AJUSTES',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
STORE SPACE(4) TO w_codini,  ;
      w_codfin
STORE rge_codalm TO w_almini,  ;
      w_almfin
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE 'P' TO w_proini, w_profin
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
       'Propiedad       :' COLOR  ;
       N/W 
     @ 05, 15 SAY 'Del:'
     @ 06, 15 SAY 'Al :'
     @ 04, 40 SAY 'Per¡odo :'  ;
       COLOR N/W 
     @ 05, 46 SAY 'Del'
     @ 06, 46 SAY ' Al'
     @ 08, 01 SAY  ;
       'Tipo            :' COLOR  ;
       N/W 
     @ 09, 01 SAY  ;
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
     @ 05, 19 GET w_proini SIZE 1,  ;
       4, 0 FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 06, 19 GET w_profin SIZE 1,  ;
       4, 0 RANGE w_proini  ;
       FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 05, 50 GET w_fecini  ;
       PICTURE '@D' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_fecini) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 06, 50 GET w_fecfin RANGE  ;
       w_fecini PICTURE '@D'  ;
       VALID oovalid(VARREAD())  ;
       .AND.  .NOT.  ;
       EMPTY(w_fecfin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 08
     @ 08, 19 GET w_tipo DEFAULT  ;
       1 SIZE 1, 7, 0 PICTURE  ;
       '@*RHN Detalle  ;Res£men'  ;
       COLOR SCHEME 24
     @ 09, 19 GET w_desti DEFAULT  ;
       1 SIZE 1, 7, 0 PICTURE  ;
       '@*RHTN Pantalla ;Impresora'  ;
       COLOR SCHEME 24
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF w_desti = 2
          SET CURSOR ON
          @ 09, 46 SAY 'Copias :'
          @ 09, 55 GET w_copia  ;
            RANGE 1,10 PICTURE  ;
            '99' COLOR SCHEME 8
          READ
          IF LASTKEY() = 27
               @ 09, 40 SAY  ;
                 SPACE(20)
               LOOP
          ENDIF
     ENDIF
     = ooaviso( ;
       'T R A B A J A  N  D  O . . .' ;
       )
     SELECT gc_kar00
     w_fil = SUBSTR(f_archivo(),  ;
             1, 8) + '.DBF'
     COPY STRU TO &w_fil
     SELECT 5
     USE &w_fil
     INDEX ON kar_codmov +  ;
           DTOS(kar_fecdoc) TAG  ;
           codigo2
     SET ORDER TO CODIGO2
     SELECT gc_kar00
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     SCAN WHILE kar_fecdoc <=  ;
          w_fecfin .AND.  .NOT.  ;
          EOF()
          IF kar_codmov >=  ;
             w_codini .AND.  ;
             kar_codmov <=  ;
             w_codfin .AND.  ;
             (gc_pro00.pro_propie >=  ;
             SUBSTR(w_proini, 1,  ;
             1) .AND.  ;
             gc_pro00.pro_propie <=  ;
             SUBSTR(w_profin, 1,  ;
             1))
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
                    SELECT 5
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
          ENDIF
          SELECT gc_kar00
     ENDSCAN
     SELECT 5
     COUNT TO nreg
     IF nreg = 0
          RELEASE WINDOW  ;
                  winmensaje
          DO p_mensaje WITH  ;
             ' NO SE ENCONTRO INFORMACION '
          USE
          erase &w_fil
          LOOP
     ELSE
          SET RELATION TO kar_codpro INTO;
gc_pro00
     ENDIF
     = ooimprime()
     SELECT 5
     USE
     erase &w_fil
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
     CASE cwhen = 'W_CODINI'
          ON KEY LABEL F6 DO bus_mov WITH;
w_codini
     CASE cwhen = 'W_CODFIN'
          ON KEY LABEL F6 DO bus_mov WITH;
w_codfin
     CASE cwhen = 'W_ALMINI'
          wrk_busca = 'ALMA'
          wrk_var = 'w_almini'
          ON KEY LABEL F6 DO BUSCA2;
 WITH WRK_BUSCA,wrk_var,'ALMACENES',2
     CASE cwhen = 'W_ALMFIN'
          wrk_busca = 'ALMA'
          wrk_var = 'w_almfin'
          ON KEY LABEL F6 DO BUSCA2;
 WITH WRK_BUSCA,wrk_var,'ALMACENES',2
     CASE cwhen = 'W_FECINI'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
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
          SELECT ge_tab0
          IF cvalid = 'W_CODINI'
               SEEK 'AJUS' +  ;
                    w_codini
          ELSE
               SEEK 'AJUS' +  ;
                    w_codfin
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
          @ ROW(), 20 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
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
          @ ROW(), 24 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            13)
ENDCASE
RETURN
*
PROCEDURE ooimprime
SELECT 5
GOTO TOP
IF w_desti = 1
     w_txt = f_texto()
     = ooaviso( ;
       'C  O  P  I  A  N  D  O . . .' ;
       )
     IF w_tipo = 1
          REPO FORM AGCR0242 TO FILE &w_txt;
NOCONSOLE
     ELSE
          REPO FORM AGCR0242 TO FILE &w_txt;
SUMMARY NOCONSOLE
     ENDIF
     SET SYSMENU ON
     MODI COMM &w_txt WIND DETALLE noedit
     SET SYSMENU OFF
     erase &w_txt
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
                      AGCR0242 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      AGCR0242  ;
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
PARAMETER w_tipo
ON KEY
SELECT DISTINCT  ;
       ge_tab0.tab_codtab,  ;
       ge_tab0.tab_destab FROM  ;
       GE_TAB0 WHERE  ;
       ge_tab0.tab_codpre =  ;
       'AJUS' AND  ;
       SUBSTR(ge_tab0.tab_codtab,  ;
       1, 1) = 'A' ORDER BY  ;
       ge_tab0.tab_codtab INTO  ;
       CURSOR QUERY
ACTIVATE POPUP bus_mov
IF LASTKEY() = 27
     w_tipo = SPACE(4)
ELSE
     w_tipo = query.tab_codtab
     KEYBOARD '{ENTER}'
ENDIF
ON KEY LABEL F6 DO bus_mov WITH w_tipo
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

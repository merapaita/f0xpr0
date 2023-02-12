*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW ped1 FROM 10, 25 TO  ;
       16, 58 IN screen COLOR  ;
       SCHEME 10
DEFINE POPUP busca FROM 13, 20 TO  ;
       19, 60 PROMPT FIELDS  ;
       tab_codtab + ' ³' +  ;
       tab_destab TITLE  ;
       'AYUDA DE TABLAS' IN  ;
       screen COLOR SCHEME 12
DEFINE POPUP clien FROM 14, 20 TO  ;
       20, 60 PROMPT FIELDS ' ' +  ;
       cli_codigo + ' ³' +  ;
       cli_razsoc IN screen COLOR  ;
       SCHEME 10
ON SELECTION POPUP busca do CHOICE0 WITH;
1
ON SELECTION POPUP clien do CHOICE0 WITH;
2      
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'ANALISIS DE EXISTENCIAS',  ;
   'REPORTE'
w_program = PROGRAM()
SET CENTURY ON
@ 02, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100000000000001000011', 2
CLOSE DATABASES
SELECT 1
USE SHARED gc_cli00 ORDER codigo
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
USE SHARED gc_pro00 ORDER codigo
SELECT 4
USE SHARED gc_alm00 ORDER codigo
wrk_selpro = SELECT()
DEFINE WINDOW winpedido FROM 4,  ;
       01 TO 21, 78 NOFLOAT  ;
       NOCLOSE NOMINIMIZE NONE  ;
       COLOR SCHEME 1
STORE rge_codalm TO w_codalm
STORE SPACE(30) TO w_desalm,  ;
      w_prod, w_proh
w_titulo = SPACE(25)
w_campo = SPACE(14)
STORE 'Proveedor ' TO w_orden
STORE SPACE(09) TO w_prove1,  ;
      w_prove2
STORE 0 TO nop1, nop2, nop3,  ;
      w_can1, w_can2, w_can3,  ;
      w_tot, w_imp1, w_imp2,  ;
      w_imp3
STORE SPACE(1) TO w_dir, w_modo,  ;
      w_mov1, w_mov2
= ooscreen(1)
SET CURSOR ON
@ 05, 03 GET nop1 DEFAULT 1 SIZE  ;
  1, 18, 0 PICTURE  ;
  '@*RVN Stock en Cero;Bajo Cr¡tico ;Sobre M ximo;Inactivos'  ;
  VALID oovalid(VARREAD()) COLOR  ;
  SCHEME 12
@ 05, 30 GET nop2 DEFAULT 1 SIZE  ;
  1, 10, 0 PICTURE  ;
  '@*RVN Detalle;Res£men' VALID  ;
  oovalid(VARREAD()) COLOR SCHEME  ;
  12
@ 05, 56 GET nop3 DEFAULT 1 SIZE  ;
  1, 10, 0 PICTURE  ;
  '@*RVN Por Pantalla;Por Impresora'  ;
  VALID oovalid(VARREAD()) COLOR  ;
  SCHEME 12
READ CYCLE
RELEASE WINDOW winpedido
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
PARAMETER cwhen, nsalta
DO CASE
     CASE cwhen = 'NOP2' .OR.  ;
          cwhen = 'NOP3'
          ON KEY LABEL F6 WAIT WINDOW;
"" NOWAIT
          DO p_footer WITH  ;
             '100000000000001000011',  ;
             2
          ACTIVATE WINDOW  ;
                   winpedido
     CASE (cwhen = 'NOP1') .OR.  ;
          (cwhen = 'NOP4')
          DO p_footer WITH  ;
             '100010000000001000011',  ;
             2
          ACTIVATE WINDOW  ;
                   winpedido
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'NOP1'
          DO CASE
               CASE nop1 = 1
                    w_titulo = ' PRODUCTOS   STOCK CERO '
                    w_campo = 'STOCK MINIMO'
                    @ 1, 24 SAY  ;
                      SPACE(50)
                    @ 2, 24 SAY  ;
                      SPACE(50)
                    @ 3, 24 SAY  ;
                      SPACE(50)
                    DO ingcod
               CASE nop1 = 2
                    w_titulo = ' PRODUCTOS BAJO CRITICO '
                    w_campo = 'STOCK CRITICO'
                    @ 1, 24 SAY  ;
                      SPACE(50)
                    @ 2, 24 SAY  ;
                      SPACE(50)
                    @ 3, 24 SAY  ;
                      SPACE(50)
               CASE nop1 = 3
                    w_titulo = ' PRODUCTOS SOBRE MAXIMO '
                    w_campo = 'STOCK MAXIMO'
                    @ 1, 24 SAY  ;
                      SPACE(50)
                    @ 2, 24 SAY  ;
                      SPACE(50)
                    @ 3, 24 SAY  ;
                      SPACE(50)
               CASE nop1 = 4
                    w_titulo = '  PRODUCTOS  INACTIVOS  '
                    @ 1, 24 SAY  ;
                      SPACE(50)
                    @ 2, 24 SAY  ;
                      SPACE(50)
                    @ 3, 24 SAY  ;
                      SPACE(50)
                    DO ingcod
          ENDCASE
     CASE cvalid = 'NOP2'
          DO CASE
               CASE nop2 = 1
                    w_modo = 'D'
               CASE nop2 = 2
                    w_modo = 'R'
          ENDCASE
     CASE cvalid = 'NOP3'
          DO CASE
               CASE nop3 = 1
                    w_dir = 'P'
               CASE nop3 = 2
                    w_dir = 'I'
          ENDCASE
          IF (nop1 = 1 .OR. nop1 =  ;
             4) .AND. w_codalm =  ;
             SPACE(4)
               DO p_mensaje WITH  ;
                  'Falta ingresar almac‚n'
               RETURN .F.
          ENDIF
          IF nop1 = 4 .AND.  ;
             (w_prove1 = SPACE(9)  ;
             .OR. w_prove2 =  ;
             SPACE(9))
               DO p_mensaje WITH  ;
                  ' No se permite blancos'
               RETURN .F.
          ENDIF
          DO proceso
     CASE cvalid = 'W_CODALM'
          SELECT ge_tab0
          SEEK 'ALMA' + w_codalm
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo no existe '
               RETURN .F.
          ELSE
               w_desalm = tab_destab
          ENDIF
     CASE cvalid = 'W_PROVE1'
          IF EMPTY(w_prove1)
               DO p_mensaje WITH  ;
                  'No se permite blancos'
               RETURN .F.
          ENDIF
          SELECT gc_cli00
          SEEK 'P' + w_prove1
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo no existe '
               RETURN .F.
          ELSE
               w_prod = cli_razsoc
          ENDIF
     CASE cvalid = 'W_PROVE2'
          IF EMPTY(w_prove2)
               DO p_mensaje WITH  ;
                  'No se permite blancos'
               RETURN .F.
          ENDIF
          SELECT gc_cli00
          SEEK 'P' + w_prove2
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo no existe '
               RETURN .F.
          ELSE
               IF w_prove2 <  ;
                  w_prove1
                    DO p_mensaje  ;
                       WITH  ;
                       ' C¢digo menor que el inicio'
                    RETURN .F.
               ELSE
                    w_proh = cli_razsoc
               ENDIF
          ENDIF
     CASE cvalid = 'W_MOV1'
          IF EMPTY(w_mov1)
               DO p_mensaje WITH  ;
                  'No se permite Blancos'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'NICR' + w_mov1
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo no existe '
               RETURN .F.
          ENDIF
     CASE cvalid = 'W_MOV2'
          IF EMPTY(w_mov2)
               DO p_mensaje WITH  ;
                  'No se permite Blancos'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'NICR' + w_mov2
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo no existe '
               RETURN .F.
          ELSE
          ENDIF
ENDCASE
RETURN
*
PROCEDURE ingcod
DO p_footer WITH  ;
   '100010000000001000011', 2
ACTIVATE WINDOW ped1
IF nop1 = 1
     ON KEY LABEL f6 do oobuscar with;
1
     @ 01, 02 SAY  ;
       'CODIGO DE ALMACEN:'
     @ 01, 22 GET w_codalm  ;
       PICTURE '9999' VALID  ;
       oovalid(VARREAD())
     READ
ENDIF
IF LASTKEY() = 27
     DEACTIVATE WINDOW ped1
     ACTIVATE WINDOW winpedido
     ON KEY LABEL f6 wait window "" nowait
     DO p_footer WITH  ;
        '100000000000001000011',  ;
        2
     RETURN
ENDIF
IF nop1 = 4
     @ 00, 00 SAY  ;
       'PROVEEDOR  Del:'
     @ 01, 11 SAY 'Al :'
     @ 02, 00 SAY  ;
       'MOVIMIENTO Del:'
     @ 03, 11 SAY 'Al :'
     @ 04, 00 SAY  ;
       'ORDENADO   Por:'
     tmp_1 = 0
     DO WHILE .T. .AND. tmp_1=0
          w_busca = 'P'
          w_var = 'W_PROVE1'
          ON KEY LABEL f6 do entidad with;
w_busca,w_var
          @ 00, 15 GET w_prove1  ;
            PICTURE '@!' VALID  ;
            oovalid(VARREAD())  ;
            COLOR W/N,N/W 
          READ
          IF LASTKEY() = 27
               tmp_1 = 1
               EXIT
          ENDIF
          w_busca = 'P'
          w_var = 'W_PROVE2'
          ON KEY LABEL f6 do entidad with;
w_busca,w_var
          @ 01, 15 GET w_prove2  ;
            PICTURE '@!' VALID  ;
            oovalid(VARREAD())  ;
            COLOR W/N,N/W 
          READ
          IF LASTKEY() = 27
               tmp_1 = 1
               EXIT
          ENDIF
          ON KEY LABEL f6 do oobuscar;
with 3
          @ 02, 15 GET w_mov1  ;
            PICTURE '@!' VALID  ;
            oovalid(VARREAD())  ;
            COLOR W/N,N/W 
          @ 03, 15 GET w_mov2  ;
            RANGE w_mov1 PICTURE  ;
            '@!' VALID  ;
            oovalid(VARREAD())  ;
            COLOR W/N,N/W 
          @ 04, 15 GET w_orden  ;
            PICTURE  ;
            '@m Proveedor ,Movimiento'
          READ
          IF LASTKEY() = 27
               tmp_1 = 1
               EXIT
          ENDIF
          tmp_1 = 1
     ENDDO
ENDIF
IF LASTKEY() <> 27
     DEACTIVATE WINDOW ped1
     ACTIVATE WINDOW winpedido
     IF nop1 = 1
          @ 01, 24 SAY  ;
            'ALMACEN        : ' +  ;
            w_codalm + '      ' +  ;
            SUBSTR(w_desalm, 1,  ;
            20) COLOR N/W 
     ENDIF
     IF nop1 = 4
          @ 02, 24 SAY  ;
            'PROVEEDOR Del: ' +  ;
            w_prove1 + ' ' +  ;
            SUBSTR(w_prod, 1, 20)  ;
            COLOR N/W 
          @ 03, 34 SAY 'Al : ' +  ;
            w_prove2 + ' ' +  ;
            SUBSTR(w_proh, 1, 20)  ;
            COLOR N/W 
     ENDIF
ELSE
     STORE SPACE(9) TO w_prove1,  ;
           w_prove2
ENDIF
DEACTIVATE WINDOW ped1
ACTIVATE WINDOW winpedido
ON KEY LABEL f6 wait window "" nowait
DO p_footer WITH  ;
   '100000000000001000011', 2
RETURN
*
PROCEDURE oobuscar
PARAMETER num
IF num = 1
     SELECT ge_tab0
     SET FILTER TO tab_codpre = 'ALMA'
     clave = 'TAB_CODTAB'
     ACTIVATE POPUP busca
     DEACTIVATE POPUP busca
ENDIF
IF num = 2
     SELECT gc_cli00
     SET NEAR ON
     SEEK 'P'
     SET NEAR OFF
     SET FILTER TO cli_tpper = 'P'
     clave = 'CLI_CODIGO'
     ACTIVATE POPUP clien
     DEACTIVATE POPUP clien
ENDIF
IF num = 3
     SELECT ge_tab0
     SET FILTER TO tab_codpre = 'NICR'
     clave = 'TAB_CODTAB'
     ACTIVATE POPUP busca
     DEACTIVATE POPUP busca
ENDIF
SET FILTER TO
RETURN
*
PROCEDURE choice0
PARAMETER opc
IF LASTKEY() == 13
     keyboard &clave
     IF opc = 1
          DEACTIVATE POPUP busca
     ELSE
          DEACTIVATE POPUP clien
     ENDIF
     SET FILTER TO
ENDIF
RETURN
*
PROCEDURE proceso
= ooaviso( ;
  ' P R O C E S A N D O ')
CREATE CURSOR repor (codigo C  ;
       (14), descri C (40), prove  ;
       C (9), modelo C (20),  ;
       marca C (4), codree C (14),  ;
       ubica C (10), stock_fis N  ;
       (9, 2), stock_min N (9, 2),  ;
       stock_max N (9, 2),  ;
       stock_crit N (9, 2),  ;
       precio N (9, 2), importe N  ;
       (9, 2), nivcit C (1),  ;
       subcat C (4))
STORE 0 TO n, w_tot, sum_alm__a,  ;
      w_can1, w_imp1, w_can2,  ;
      w_imp2, w_can3, w_imp3
DO CASE
     CASE nop1 = 1
          SELECT gc_alm00
          SET ORDER TO codalm
          SEEK w_codalm
          SCAN WHILE alm_codalm =  ;
               w_codalm .AND.   ;
               .NOT. EOF()
               sum_alm__a = alm_stkfis
               SELECT gc_pro00
               SEEK gc_alm00.alm_codpro
               IF (pro_nivcit <>  ;
                  'F' .AND.  ;
                  pro_nivcit <>  ;
                  'H' .AND.  ;
                  pro_stkmin >  ;
                  gc_alm00.alm_stkfis)  ;
                  .AND.  ;
                  sum_alm__a = 0
                    SELECT repor
                    APPEND BLANK
                    REPLACE codigo  ;
                            WITH  ;
                            gc_pro00.pro_codpro,  ;
                            descri  ;
                            WITH  ;
                            gc_pro00.pro_descri,  ;
                            modelo  ;
                            WITH  ;
                            gc_pro00.pro_modelo,  ;
                            codree  ;
                            WITH  ;
                            gc_pro00.pro_codree,  ;
                            prove  ;
                            WITH  ;
                            gc_pro00.pro_codpve
                    REPLACE ubica  ;
                            WITH  ;
                            gc_alm00.alm_ubicac,  ;
                            stock_fis  ;
                            WITH  ;
                            sum_alm__a,  ;
                            stock_min  ;
                            WITH  ;
                            gc_pro00.pro_stkmin,  ;
                            stock_crit  ;
                            WITH  ;
                            gc_pro00.pro_critic,  ;
                            marca  ;
                            WITH  ;
                            gc_pro00.pro_marca
                    REPLACE precio  ;
                            WITH  ;
                            gc_pro00.pro_coprmo,  ;
                            importe  ;
                            WITH  ;
                            stock_fis *  ;
                            precio,  ;
                            subcat  ;
                            WITH  ;
                            gc_pro00.pro_subcat
               ENDIF
               SELECT gc_alm00
          ENDSCAN
          SET ORDER TO codigo
          SELECT repor
          w_idx = f_indice()
          index on codigo to &w_idx
     CASE nop1 = 2
          SELECT gc_pro00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SELECT gc_alm00
               SUM FOR alm_codpro =  ;
                   gc_pro00.pro_codpro  ;
                   alm_stkfis TO  ;
                   sum_alm__a
               SEEK gc_pro00.pro_codpro +  ;
                    rge_codalm
               SELECT gc_pro00
               IF (pro_nivcit <>  ;
                  'F' .AND.  ;
                  pro_nivcit <>  ;
                  'H') .AND.  ;
                  (sum_alm__a <  ;
                  pro_critic)  ;
                  .AND.  ;
                  (sum_alm__a >  ;
                  0)
                    SELECT repor
                    APPEND BLANK
                    REPLACE codigo  ;
                            WITH  ;
                            gc_pro00.pro_codpro,  ;
                            descri  ;
                            WITH  ;
                            gc_pro00.pro_descri,  ;
                            modelo  ;
                            WITH  ;
                            gc_pro00.pro_modelo,  ;
                            codree  ;
                            WITH  ;
                            gc_pro00.pro_codree,  ;
                            prove  ;
                            WITH  ;
                            gc_pro00.pro_codpve
                    REPLACE ubica  ;
                            WITH  ;
                            gc_alm00.alm_ubicac,  ;
                            stock_fis  ;
                            WITH  ;
                            sum_alm__a,  ;
                            stock_min  ;
                            WITH  ;
                            gc_pro00.pro_stkmin,  ;
                            stock_crit  ;
                            WITH  ;
                            gc_pro00.pro_critic,  ;
                            marca  ;
                            WITH  ;
                            gc_pro00.pro_marca
                    REPLACE precio  ;
                            WITH  ;
                            gc_pro00.pro_coprmo,  ;
                            importe  ;
                            WITH  ;
                            stock_fis *  ;
                            precio,  ;
                            subcat  ;
                            WITH  ;
                            gc_pro00.pro_subcat
               ENDIF
               SELECT gc_pro00
          ENDSCAN
     CASE nop1 = 3
          SELECT gc_pro00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SELECT gc_alm00
               SUM FOR alm_codpro =  ;
                   gc_pro00.pro_codpro  ;
                   alm_stkfis TO  ;
                   sum_alm__a
               SEEK gc_pro00.pro_codpro +  ;
                    rge_codalm
               SELECT gc_pro00
               IF (sum_alm__a >  ;
                  pro_stkmax)  ;
                  .AND.  ;
                  (sum_alm__a >  ;
                  0) .AND.  ;
                  pro_nivcit <>  ;
                  'D' .AND.  ;
                  pro_nivcit <>  ;
                  'E' .AND.  ;
                  pro_nivcit <>  ;
                  'F' .AND.  ;
                  pro_nivcit <>  ;
                  'N'
                    SELECT repor
                    APPEND BLANK
                    REPLACE codigo  ;
                            WITH  ;
                            gc_pro00.pro_codpro,  ;
                            descri  ;
                            WITH  ;
                            gc_pro00.pro_descri,  ;
                            modelo  ;
                            WITH  ;
                            gc_pro00.pro_modelo,  ;
                            codree  ;
                            WITH  ;
                            gc_pro00.pro_codree,  ;
                            prove  ;
                            WITH  ;
                            gc_pro00.pro_codpve
                    REPLACE ubica  ;
                            WITH  ;
                            gc_alm00.alm_ubicac,  ;
                            stock_fis  ;
                            WITH  ;
                            sum_alm__a,  ;
                            stock_min  ;
                            WITH  ;
                            gc_pro00.pro_stkmin,  ;
                            stock_crit  ;
                            WITH  ;
                            gc_pro00.pro_critic,  ;
                            marca  ;
                            WITH  ;
                            gc_pro00.pro_marca
                    REPLACE precio  ;
                            WITH  ;
                            gc_pro00.pro_coprmo,  ;
                            importe  ;
                            WITH  ;
                            stock_fis *  ;
                            precio,  ;
                            stock_max  ;
                            WITH  ;
                            gc_pro00.pro_stkmax,  ;
                            subcat  ;
                            WITH  ;
                            gc_pro00.pro_subcat
               ENDIF
               SELECT gc_pro00
          ENDSCAN
     CASE nop1 = 4
          SELECT gc_pro00
          SET ORDER TO pro_codpve
          SET NEAR OFF
          SEEK w_prove1
          SET NEAR ON
          SCAN WHILE pro_codpve <=  ;
               w_prove2 .AND.   ;
               .NOT. EOF()
               SELECT gc_alm00
               SUM FOR alm_codpro =  ;
                   gc_pro00.pro_codpro  ;
                   alm_stkfis TO  ;
                   sum_alm__a
               SEEK gc_pro00.pro_codpro +  ;
                    rge_codalm
               w_tot = w_tot +  ;
                       sum_alm__a
               SELECT gc_pro00
               IF pro_nivcit >=  ;
                  w_mov1 .AND.  ;
                  pro_nivcit <=  ;
                  w_mov2 .AND.  ;
                  sum_alm__a > 0
                    SELECT repor
                    APPEND BLANK
                    REPLACE codigo  ;
                            WITH  ;
                            gc_pro00.pro_codpro,  ;
                            descri  ;
                            WITH  ;
                            gc_pro00.pro_descri,  ;
                            modelo  ;
                            WITH  ;
                            gc_pro00.pro_modelo,  ;
                            codree  ;
                            WITH  ;
                            gc_pro00.pro_codree,  ;
                            prove  ;
                            WITH  ;
                            gc_pro00.pro_codpve
                    REPLACE ubica  ;
                            WITH  ;
                            gc_alm00.alm_ubicac,  ;
                            stock_fis  ;
                            WITH  ;
                            sum_alm__a,  ;
                            stock_min  ;
                            WITH  ;
                            gc_pro00.pro_stkmin,  ;
                            stock_crit  ;
                            WITH  ;
                            gc_pro00.pro_critic,  ;
                            marca  ;
                            WITH  ;
                            gc_pro00.pro_marca
                    REPLACE precio  ;
                            WITH  ;
                            gc_pro00.pro_coprmo,  ;
                            importe  ;
                            WITH  ;
                            stock_fis *  ;
                            precio,  ;
                            nivcit  ;
                            WITH  ;
                            gc_pro00.pro_nivcit,  ;
                            subcat  ;
                            WITH  ;
                            gc_pro00.pro_subcat
                    IF nivcit =  ;
                       'D'
                         w_can1 =  ;
                          w_can1 +  ;
                          stock_fis
                         w_imp1 =  ;
                          w_imp1 +  ;
                          importe
                    ELSE
                         IF nivcit =  ;
                            'E'
                              w_can2 =  ;
                               w_can2 +  ;
                               stock_fis
                              w_imp2 =  ;
                               w_imp2 +  ;
                               importe
                         ELSE
                              w_can3 =  ;
                               w_can3 +  ;
                               stock_fis
                              w_imp3 =  ;
                               w_imp3 +  ;
                               importe
                         ENDIF
                    ENDIF
               ENDIF
               SELECT gc_pro00
          ENDSCAN
          w_idx = f_indice()
          SET ORDER TO codigo
          SELECT repor
          IF w_orden =  ;
             'Proveedor '
               index on prove+nivcit+codigo;
to &w_idx
          ELSE
               index on nivcit+prove+codigo;
to &w_idx
          ENDIF
ENDCASE
RELEASE WINDOW winmensaje
= ooimprime()
DO p_footer WITH  ;
   '100000000000000000011', 2
ON KEY LABEL f7 wait window "" nowait;

RETURN
*
PROCEDURE ooimprime
SELECT repor
GOTO TOP
SET RELATION TO 'P' + prove INTO gc_cli00;
ADDITIVE
IF w_dir = 'I'
     DO WHILE LASTKEY()<>27
          IF SYS(13) = 'OFFLINE'
               swt = f_yesno( ;
                     'Impresora fuera de Linea. Intentar nuevamente ' ;
                     )
               IF swt
                    LOOP
               ELSE
                    DEACTIVATE WINDOW  ;
                               winmensaje
                    EXIT
               ENDIF
          ENDIF
          = ooaviso( ;
            ' I M P R I M I E N D O . . .' ;
            )
          SET DEVICE TO PRINTER
          @ 00, 00 SAY CHR(27) +  ;
            CHR(15)
          ??? CHR(15)
          DO CASE
               CASE nop1 = 1 .OR.  ;
                    nop1 = 2 .OR.  ;
                    nop1 = 3
                    IF w_modo =  ;
                       'R'
                         REPORT FORMAT  ;
                                AGCR0207  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                AGCR0207  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               CASE nop1 = 4
                    IF w_modo =  ;
                       'R'
                         IF w_orden =  ;
                            'Proveedor '
                              REPORT  ;
                               FORMAT  ;
                               AGCR0239  ;
                               SUMMARY  ;
                               TO  ;
                               PRINTER  ;
                               NOCONSOLE
                         ELSE
                              REPORT  ;
                               FORMAT  ;
                               AGCR0240  ;
                               SUMMARY  ;
                               TO  ;
                               PRINTER  ;
                               NOCONSOLE
                         ENDIF
                    ELSE
                         IF w_orden =  ;
                            'Proveedor '
                              REPORT  ;
                               FORMAT  ;
                               AGCR0239  ;
                               TO  ;
                               PRINTER  ;
                               NOCONSOLE
                         ELSE
                              REPORT  ;
                               FORMAT  ;
                               AGCR0240  ;
                               TO  ;
                               PRINTER  ;
                               NOCONSOLE
                         ENDIF
                    ENDIF
          ENDCASE
          @ 00, 0 SAY CHR(27) +  ;
            CHR(18)
          SET DEVICE TO SCREEN
          RELEASE WINDOW  ;
                  winmensaje
          EXIT
     ENDDO
     DO p_mensaje WITH  ;
        ' FIN DE IMPRESION '
ELSE
     w_txt = f_texto()
     DO CASE
          CASE nop1 = 1 .OR. nop1 =  ;
               2 .OR. nop1 = 3
               IF w_modo = 'R'
                    repo form AGCR0207;
TO FILE &w_txt NOCONSOLE SUMMARY
               ELSE
                    repo form AGCR0207;
TO FILE &w_txt NOCONSOLE
               ENDIF
          CASE nop1 = 4
               IF w_modo = 'R'
                    IF w_orden =  ;
                       'Proveedor '
                         REPO FORM AGCR0239;
TO FILE &w_txt NOCONSOLE SUMMARY
                    ELSE
                         REPO FORM AGCR0240;
TO FILE &w_txt NOCONSOLE SUMMARY
                    ENDIF
               ELSE
                    IF w_orden =  ;
                       'Proveedor '
                         REPO FORM AGCR0239;
TO FILE &w_txt NOCONSOLE
                         REPORT FORMAT  ;
                                AGCR0239  ;
                                TO  ;
                                FILE  ;
                                INACT.TXT
                    ELSE
                         REPO FORM AGCR0240;
TO FILE &w_txt NOCONSOLE
                         REPORT FORMAT  ;
                                AGCR0240  ;
                                TO  ;
                                FILE  ;
                                INACT.TXT
                    ENDIF
               ENDIF
     ENDCASE
     SET SYSMENU ON
     KEYBOARD '{CTRL+F10}'
     modi comm &w_txt window pantall noedit
     SET SYSMENU OFF
     erase &w_txt      
ENDIF
RETURN
*
PROCEDURE ooscreen
PARAMETER w_screen
DO CASE
     CASE w_screen = 1
          ACTIVATE WINDOW  ;
                   winpedido
          @ 01, 02 SAY  ;
            'Datos del Reporte :'  ;
            SIZE 1, 20, 0 COLOR  ;
            SCHEME 6
          @ 01, 24 SAY SPACE(50)
          @ 02, 24 SAY SPACE(50)
          @ 00, 00 TO 9, 77
ENDCASE
STORE 1 TO nop1, nop2, nop3
w_dir = 'P'
w_modo = 'D'
w_campo = 'STOCK_MINIMO'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

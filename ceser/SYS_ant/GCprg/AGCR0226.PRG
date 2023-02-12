*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PUBLIC wrk_campo, wrk_selpro,  ;
       wrk_selec, wrk_cad001,  ;
       wrk_cad002, wrk_codpro,  ;
       wrk_cadest, wrk_estado,  ;
       wrk_arc_01, wrk_arc_02,  ;
       calias_1, calias_2,  ;
       dfecini, dfecfin
DEFINE POPUP busca FROM 12, 27 TO  ;
       18, 53 PROMPT FIELDS ' ' +  ;
       tab_codtab + ' ³' +  ;
       tab_destab IN screen COLOR  ;
       SCHEME 12
DEFINE POPUP clien FROM 12, 27 TO  ;
       18, 53 PROMPT FIELDS ' ' +  ;
       cli_codigo + ' ³' +  ;
       cli_razsoc IN screen COLOR  ;
       SCHEME 12
ON SELECTION POPUP clien DEAC POPUP CLIEN;
      
ON SELECTION POPUP busca DEAC POPUP BUSCA
SET CENTURY ON
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   ' - KARDEX - ', 'REPORTE'
wrk_progra = PROGRAM()
@ 02, 63 SAY PROGRAM()
dfecini = DATE()
dfecfin = DATE()
output = 'Pantalla '
wrk_modo = 'Detalle'
wrk_codpro = SPACE(14)
DO WHILE .T.
     DO p_footer WITH  ;
        '100000000000001000011',  ;
        2
     SELECT 1
     USE GC_ALM00 ORDER 1
     SELECT 2
     USE GE_TAB0 ORDER 1
     SELECT 3
     USE GC_PRO00 ORDER 1
     wrk_selpro = SELECT()
     SELECT 5
     USE SHARED GC_CMV00 ORDER 1
     SELECT 4
     USE GC_KAR00 ORDER  ;
         KAR_PROING
     DEFINE WINDOW winpedido FROM  ;
            4, 1 TO 21, 78  ;
            NOFLOAT NOCLOSE  ;
            NOMINIMIZE NONE COLOR  ;
            SCHEME 1
     wrk_nop2 = 1
     wrk_cad002 = '.AND. GC_PRO00.PRO_CODPRO <> SPACE(14) '
     wrk_descri = SPACE(20)
     = ooscreen(1)
     SET CURSOR ON
     @ 1, 22 GET wrk_codpro  ;
       DEFAULT ' ' SIZE 1, 14, 0  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 2, 22 GET wrk_alma DEFAULT  ;
       rge_codalm SIZE 1, 4, 0  ;
       PICTURE '9999' VALID  ;
       oovalid(VARREAD())
     @ 3, 22 GET wrk_mone DEFAULT  ;
       'DOL ' SIZE 1, 4, 0  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD())
     @ 4, 22 GET dfecini DEFAULT  ;
       DATE() SIZE 1, 10, 0 VALID  ;
       oovalid(VARREAD())
     @ 4, 66 GET dfecfin DEFAULT  ;
       DATE() SIZE 1, 10, 0 VALID  ;
       oovalid(VARREAD())
     @ 5, 22 GET wrk_modo SIZE 1,  ;
       10, 0 PICTURE  ;
       '@M Detalle,Res£men' WHEN  ;
       oowhen(VARREAD())
     @ 5, 66 GET output SIZE 1,  ;
       10, 0 PICTURE  ;
       '@M Pantalla,Impresora'  ;
       WHEN oowhen(VARREAD())
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     restc = ootc2(DATE(),'SOL ', ;
             'DOL ','2')
     IF restc = -1 .AND. wrk_mone =  ;
        'SOL '
          DO p_mensaje WITH  ;
             'NO EXISTE TIPO DE CAMBIO'
          EXIT
     ENDIF
     DO proceso
ENDDO
RELEASE WINDOW winpedido
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
ERASE &WRK_IDX 
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen, nsalta
DO CASE
     CASE cwhen = 'WRK_CODPRO'
          wrk_selec = SELECT()
          wrk_campo = 'wrk_CodPro'
          ON KEY LABEL F6 DO PRODUC WITH;
WRK_CAMPO,WRK_SELEC,WRK_SELPRO
          DO p_footer WITH  ;
             '100010000000000000011',  ;
             2
          ACTIVATE WINDOW  ;
                   winpedido
     CASE (cwhen = 'NOP2') .OR.  ;
          (cwhen = 'OUTPUT')
          ON KEY LABEL F6 WAIT WINDOW;
"" NOWAIT
          DO p_footer WITH  ;
             '100000000000001000011',  ;
             2
          ACTIVATE WINDOW  ;
                   winpedido
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'NOP2'
          IF nop2 = 2
               SHOW GET  ;
                    wrk_codpro  ;
                    DISABLE
               wrk_cad002 = '.AND. GC_PRO00.PRO_CODPRO <> SPACE(14) '
          ELSE
               SHOW GET  ;
                    wrk_codpro  ;
                    ENABLE
               wrk_cad002 = '.AND. GC_PRO00.PRO_CODPRO = wrk_CodPro'
          ENDIF
     CASE cvalid = 'WRK_ALMA'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          wrk_busca = 'ALMA'
          = ooareat('GE_TAB0', ;
            'CODIGO')
          SEEK wrk_busca +  ;
               wrk_alma
          IF FOUND()
               @ 2, 42 SAY  ;
                 SUBSTR(ge_tab0.tab_destab,  ;
                 1, 20) COLOR N/W 
               SELECT gc_alm00
               SEEK wrk_codpro +  ;
                    wrk_alma
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'CODIGO NO EXISTE EN ALMACEN'
                    RETURN .F.
               ENDIF
          ELSE
               DO p_mensaje WITH  ;
                  ' CODIGO DE ALMACEN NO EXISTE '
               RETURN .F.
          ENDIF
     CASE cvalid = 'WRK_MONE'
          wrk_busca = 'MONE'
          = ooareat('GE_TAB0', ;
            'CODIGO')
          SEEK wrk_busca +  ;
               wrk_mone
          IF FOUND()
               @ 3, 42 SAY  ;
                 SUBSTR(ALLTRIM(ge_tab0.tab_destab),  ;
                 1, 19) COLOR N/W 
          ELSE
               DO p_mensaje WITH  ;
                  ' CODIGO DE MONEDA NO EXISTE '
               RETURN .F.
          ENDIF
     CASE cvalid = 'WRK_CODPRO'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          wrk_area = SELECT()
          = ooareat('GC_PRO00', ;
            'CODIGO')
          SEEK wrk_codpro
          IF FOUND()
               wrk_descri = gc_pro00.pro_descri
               @ 1, 42 SAY  ;
                 SUBSTR(wrk_descri,  ;
                 1, 20) COLOR N/W 
          ELSE
               DO p_mensaje WITH  ;
                  ' CODIGO DE PRODUCTO NO EXISTE '
               SELECT (wrk_area)
               RETURN .F.
          ENDIF
          SELECT (wrk_area)
ENDCASE
RETURN
*
PROCEDURE proceso
= ooaviso( ;
  ' T R A B A J A N D O . . .')
wrk_num = 0
CREATE CURSOR KARD (fecmov D,  ;
       tipdoc C (4), fecdoc D,  ;
       nrodoc C (10), lindet C  ;
       (2), codmov C (4), almrec  ;
       C (4), cantid N (9, 2),  ;
       importe N (9, 2), saldo N  ;
       (9, 2), imptot N (9, 2),  ;
       precio N (9, 2), tip C (1),  ;
       num N (5), tidore C (4),  ;
       nrdore C (10), cosuni N (9,  ;
       2))
SELECT gc_kar00
SEEK wrk_codpro
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        'NO TIENE MOVIMIENTO'
ENDIF
wrk_stock = gc_alm00.alm_stkfis
wrk_saldo = gc_alm00.alm_stkfis
SCAN WHILE kar_codpro =  ;
     wrk_codpro .AND.  .NOT.  ;
     EOF()
     IF (kar_almdes = wrk_alma  ;
        .OR. kar_almrec =  ;
        wrk_alma) .AND.  ;
        (kar_fecing <= DATE()  ;
        .AND. kar_fecing >=  ;
        dfecini)
          wrk_saldo = wrk_stock
          DO CASE
               CASE (SUBSTR(kar_codmov,  ;
                    1, 1) = 'E'  ;
                    .OR.  ;
                    kar_codmov =  ;
                    'COI ' .OR.  ;
                    kar_codmov =  ;
                    'TRBO' .OR.  ;
                    kar_codmov =  ;
                    'AFNE') .AND.  ;
                    kar_almdes =  ;
                    wrk_alma
                    wrk_tip = 'S'
                    wrk_stock = wrk_stock +  ;
                                kar_cantid
               CASE (SUBSTR(kar_codmov,  ;
                    1, 1) = 'I'  ;
                    .OR.  ;
                    kar_codmov =  ;
                    'AFPO' .OR.  ;
                    kar_codmov =  ;
                    'AVPO' .OR.  ;
                    kar_codmov =  ;
                    'TRBO' .OR.  ;
                    kar_codmov =  ;
                    'AVNE' .OR.  ;
                    kar_codmov =  ;
                    'AFNE') .AND.  ;
                    kar_almrec =  ;
                    wrk_alma
                    wrk_tip = 'I'
                    wrk_stock = wrk_stock -  ;
                                kar_cantid
          ENDCASE
          SELECT kard
          APPEND BLANK
          REPLACE fecmov WITH  ;
                  gc_kar00.kar_fecing,  ;
                  tipdoc WITH  ;
                  gc_kar00.kar_tipdoc,  ;
                  fecdoc WITH  ;
                  gc_kar00.kar_fecdoc,  ;
                  nrodoc WITH  ;
                  gc_kar00.kar_nrodoc
          REPLACE lindet WITH  ;
                  gc_kar00.kar_lindet,  ;
                  codmov WITH  ;
                  gc_kar00.kar_codmov,  ;
                  almrec WITH  ;
                  gc_kar00.kar_almrec,  ;
                  cantid WITH  ;
                  gc_kar00.kar_cantid
          REPLACE saldo WITH  ;
                  wrk_saldo, tip  ;
                  WITH wrk_tip
          REPLACE tidore WITH  ;
                  gc_kar00.kar_tidore,  ;
                  nrdore WITH  ;
                  gc_kar00.kar_nrdore
          IF wrk_mone = 'SOL'
               REPLACE importe  ;
                       WITH  ;
                       gc_kar00.kar_cantid *  ;
                       gc_kar00.kar_cosanb,  ;
                       imptot  ;
                       WITH  ;
                       wrk_saldo *  ;
                       gc_kar00.kar_cosanb
               REPLACE cosuni  ;
                       WITH  ;
                       gc_kar00.kar_cosanb
          ELSE
               REPLACE importe  ;
                       WITH  ;
                       gc_kar00.kar_cantid *  ;
                       gc_kar00.kar_cosant,  ;
                       imptot  ;
                       WITH  ;
                       wrk_saldo *  ;
                       gc_kar00.kar_cosant
               REPLACE cosuni  ;
                       WITH  ;
                       gc_kar00.kar_cosant
          ENDIF
     ENDIF
     SELECT gc_kar00
ENDSCAN
SELECT kard
COUNT TO wrk_conta
IF wrk_conta = 0
     DEACTIVATE WINDOW winmensaje
     DO p_mensaje WITH  ;
        'No Existen Registros a Listar'
     RETURN
ENDIF
GOTO TOP
SCAN WHILE  .NOT. EOF()
     REPLACE num WITH wrk_conta
     wrk_conta = wrk_conta - 1
ENDSCAN
= ooimprime()
RETURN
*
FUNCTION movix
PARAMETER cmovi, wrk_tipdoc,  ;
          wrk_nrodoc, wrk_tipalm
carea = SELECT()
= ooareat('GC_HIP00','CODIGO')
SEEK wrk_tipdoc + wrk_nrodoc
IF FOUND()
     IF wrk_tipalm = 1
          RETURN hip_almdes
     ENDIF
     IF wrk_tipalm = 2
          RETURN hip_almrec
     ENDIF
ENDIF
RETURN
*
FUNCTION oomovix
PARAMETER cmovi, wrk_tipdoc,  ;
          wrk_nrodoc, wrk_tipalm
carea = SELECT()
DO CASE
     CASE cmovi = 'EVTA' .OR.  ;
          cmovi = 'IFD'
          = ooareat('GC_HVE00', ;
            'CODIGO')
          SEEK wrk_tipdoc +  ;
               wrk_nrodoc
          IF FOUND()
               IF wrk_tipalm = 1
                    wrk_return = hve_almdes
               ELSE
                    wrk_return = ' '
               ENDIF
               SELECT (carea)
               RETURN wrk_return
          ELSE
               SELECT (carea)
               RETURN ' '
          ENDIF
     CASE cmovi = 'IFE' .OR.  ;
          cmovi = 'IFC' .OR.  ;
          cmovi = 'IFL' .OR.  ;
          cmovi = 'IFD' .OR.  ;
          cmovi = 'IFP' .OR.  ;
          cmovi = 'IFO' .OR.  ;
          cmovi = 'IFG' .OR.  ;
          cmovi = 'IFPO' .OR.  ;
          cmovi = 'IFNE' .OR.  ;
          cmovi = 'AVPO' .OR.  ;
          cmovi = 'AVNE' .OR.  ;
          cmovi = 'COI' .OR.  ;
          cmovi = 'ECOM' .OR.  ;
          cmovi = 'EDEM' .OR.  ;
          cmovi = 'EPRD' .OR.  ;
          cmovi = 'EMER' .OR.  ;
          cmovi = 'EDEV' .OR.  ;
          cmovi = 'EGAR' .OR.  ;
          cmovi = 'EGTA' .OR.  ;
          cmovi = 'EPDI' .OR.  ;
          cmovi = 'EPFI' .OR.  ;
          cmovi = 'EPRO' .OR.  ;
          cmovi = 'EPSE' .OR.  ;
          cmovi = 'TRBO'
          = ooareat('GC_HIP00', ;
            'CODIGO')
          SEEK wrk_tipdoc +  ;
               wrk_nrodoc
          IF FOUND()
               IF wrk_tipalm = 1
                    wrk_return = hip_almrec
                    SELECT (carea)
                    RETURN wrk_return
               ELSE
                    wrk_return = hip_almdes
                    SELECT (carea)
                    RETURN wrk_return
               ENDIF
          ELSE
               SELECT (carea)
               RETURN ' '
          ENDIF
ENDCASE
SELECT (carea)
RETURN
*
PROCEDURE ooimprime
wrk_idx = f_indice()
SELECT kard
INDEX ON NUM TO &WRK_IDX 
GOTO TOP
IF output = 'Impresora'
     DO WHILE LASTKEY()<>27
          IF SYS(13) = 'OFFLINE'
               swt = f_yesno( ;
                     'Impresora fuera de Linea. Intentar nuevamente ' ;
                     )
               IF swt
                    LOOP
               ELSE
                    EXIT
               ENDIF
          ENDIF
          = ooaviso( ;
            ' I M P R I M I E N D O...' ;
            )
          SET DEVICE TO PRINTER
          @ 0, 0 SAY CHR(27) +  ;
            CHR(15)
          IF wrk_modo = 'Detalle'
               REPORT FORMAT  ;
                      AGCR0226 TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR fecmov <=  ;
                      dfecfin
          ELSE
               REPORT FORMAT  ;
                      AGCR0226  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR fecmov <=  ;
                      dfecfin
          ENDIF
          @ 0, 0 SAY CHR(27) +  ;
            CHR(18)
          SET DEVICE TO SCREEN
          EXIT
     ENDDO
ELSE
     wrk_txt = f_texto()
     IF wrk_modo = 'Detalle'
          REPO FORM AGCR0226 TO FILE &WRK_TXT;
NOCONSOLE for fecmov<=dfecfin
     ELSE
          REPO FORM AGCR0226 TO FILE &WRK_TXT;
NOCONSOLE  SUMMARY for fecmov<=dfecfin
     ENDIF
     SET SYSMENU ON
     KEYBOARD '{CTRL+F10}'
     modi comm &WRK_TXT noedit WINDOW;
PANTALL 
     SET SYSMENU OFF
     delete file &wrk_txt
ENDIF
DEACTIVATE WINDOW winmensaje
RETURN
*
PROCEDURE ooscreen
PARAMETER wrk_screen
DO CASE
     CASE wrk_screen = 1
          ACTIVATE WINDOW  ;
                   winpedido
          @ 01, 02 SAY  ;
            'C¢digo             :'
          @ 02, 02 SAY  ;
            'Almac‚n            :'
          @ 03, 02 SAY  ;
            'Moneda             :'
          @ 04, 02 SAY  ;
            'Fecha Desde        :'  ;
            SIZE 1, 36
          @ 04, 42 SAY  ;
            'Fecha Hasta           :'
          @ 05, 02 SAY  ;
            'Por Detalle/Res£men:'
          @ 05, 42 SAY  ;
            'Por Pantalla/Impresora:'
          @ 06, 00 TO 17, 77
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

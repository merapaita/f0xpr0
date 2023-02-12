*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PUBLIC wrk_campo, wrk_selpro,  ;
       wrk_selec, wrk_ranpro,  ;
       wrk_randes, wrk_ranalm,  ;
       wrk_ranlin, wrk_ranmar,  ;
       wrk_ranrub, wrk_ransru,  ;
       wrk_rancat, wrk_ranprc,  ;
       wrk_ranprp, wrk_ransca,  ;
       wrk_select, wrk_cat_1,  ;
       wrk_cat_2, wrk_scat_1,  ;
       wrk_scat_2
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
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'REPORTE DE EXISTENCIA',  ;
   'REPORTE'
wrk_progra = PROGRAM()
@ 2, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100010000000001000011', 2
= ooopen('GC_PRO00',1)
wrk_selpro = SELECT()
wrk_ranpro = ' .AND. (PRO_CODPRO >= wrk_Cod_1 .AND. PRO_CODPRO <= wrk_Cod_2) '
wrk_randes = ' '
wrk_ranalm = ' .AND. (ALM_CODALM >= wrk_Alma_1 .AND. ALM_CODALM <= wrk_Alma_2) '
wrk_ranlin = ' '
wrk_ranmar = ' '
wrk_ranrub = ' '
wrk_ransru = ' '
wrk_ranprc = ' '
wrk_ranprp = ' '
wrk_rancat = ' '
wrk_ransca = ' '
nop1 = 1
nop2 = 1
nop3 = 1
DEFINE WINDOW winmarco FROM 4, 1  ;
       TO 21, 78 FLOAT CLOSE  ;
       MINIMIZE NONE COLOR SCHEME  ;
       2
= ooopen('GE_TAB0',1)
= ooopen('GC_PRO00',1)
wrk_selpro = SELECT()
= ooopen('GC_ALM00',1)
= ooopen('GC_HVE00',1)
m.hve_almdes = '0001'
SET CURSOR ON
ACTIVATE WINDOW SAME winmarco
= ooscreen(1)
@ 01, 17 GET nop1 DEFAULT 1 SIZE  ;
  1, 20, 0 PICTURE  ;
  '@*RHN Valorizadas     ;Consolidadas'  ;
  VALID oovalid(VARREAD()) COLOR  ;
  SCHEME 12
@ 02, 17 GET nop2 DEFAULT 1 SIZE  ;
  1, 11, 0 PICTURE  ;
  '@*RHN Ponderado;Reposici¢n;Ult. Compra;Mon. Base'  ;
  COLOR SCHEME 12
@ 04, 01 GET nop3 DEFAULT 1 SIZE  ;
  1, 19, 0 PICTURE  ;
  '@*RVN Por C¢digo;Por Descripci¢n'  ;
  VALID oovalid(VARREAD()) COLOR  ;
  SCHEME 12
@ 04, 29 GET wrk_cod_1 DEFAULT  ;
  ' ' SIZE 1, 14 VALID ooproduc(4, ;
  43,33,VARREAD()) COLOR SCHEME  ;
  6
@ 5, 29 GET wrk_cod_2 DEFAULT ' '  ;
  SIZE 1, 14 VALID ooproduc(5,43, ;
  33,VARREAD()) COLOR SCHEME 6
@ 4, 29 GET wrk_des_1 DEFAULT ' '  ;
  SIZE 1, 14 DISABLE VALID  ;
  ooproduc(5,43,33,VARREAD())  ;
  COLOR SCHEME 6
@ 5, 29 GET wrk_des_2 DEFAULT ' '  ;
  SIZE 1, 14 DISABLE VALID  ;
  ooproduc(5,43,33,VARREAD())  ;
  COLOR SCHEME 6
@ 7, 19 GET wrk_alma_1 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(7, ;
  19,24,19,'ALMA',VARREAD())  ;
  COLOR SCHEME 6
@ 7, 51 GET wrk_alma_2 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(7, ;
  51,56,20,'ALMA',VARREAD())  ;
  COLOR SCHEME 6
@ 9, 19 GET wrk_lin_1 DEFAULT ' '  ;
  SIZE 1, 4 VALID oorangos(9,19, ;
  24,19,'LINE',VARREAD()) COLOR  ;
  SCHEME 6
@ 9, 51 GET wrk_lin_2 DEFAULT ' '  ;
  SIZE 1, 4 VALID oorangos(9,51, ;
  56,20,'LINE',VARREAD()) COLOR  ;
  SCHEME 6
@ 10, 19 GET wrk_mar_1 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(10, ;
  19,24,19,'MARC',VARREAD())  ;
  COLOR SCHEME 6
@ 10, 51 GET wrk_mar_2 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(10, ;
  51,56,20,'MARC',VARREAD())  ;
  COLOR SCHEME 6
@ 12, 19 GET wrk_prc_1 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(12, ;
  19,24,19,'PROC',VARREAD())  ;
  COLOR SCHEME 6
@ 12, 51 GET wrk_prc_2 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(12, ;
  51,56,20,'PROC',VARREAD())  ;
  COLOR SCHEME 6
@ 13, 19 GET wrk_prp_1 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(13, ;
  19,24,19,'PROP',VARREAD())  ;
  COLOR SCHEME 6
@ 13, 51 GET wrk_prp_2 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(13, ;
  51,56,20,'PROP',VARREAD())  ;
  COLOR SCHEME 6
@ 15, 19 GET wrk_rub_1 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(15, ;
  19,24,19,'RUBR',VARREAD())  ;
  COLOR SCHEME 6
@ 15, 51 GET wrk_rub_2 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(15, ;
  51,56,20,'RUBR',VARREAD())  ;
  COLOR SCHEME 6
@ 16, 19 GET wrk_srub_1 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(16, ;
  19,24,19,'SURU',VARREAD())  ;
  COLOR SCHEME 6
@ 16, 51 GET wrk_srub_2 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(16, ;
  51,56,20,'SURU',VARREAD())  ;
  COLOR SCHEME 6
@ 17, 31 GET nop12 DEFAULT 1 SIZE  ;
  1, 11, 0 PICTURE  ;
  '@*HN Procesar' VALID  ;
  ooproceso(nop1) COLOR SCHEME  ;
  12
READ CYCLE COLOR SCHEME 6
RELEASE WINDOW winmarco
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'NOP1'
          IF nop1 = 2
               SHOW GET nop2  ;
                    DISABLE COLOR  ;
                    W/W 
          ELSE
               SHOW GET nop2  ;
                    ENABLE COLOR  ;
                    SCHEME 12
          ENDIF
     CASE cvalid = 'NOP3'
          IF nop3 = 2
               wrk_randes = ' .AND. (PRO_DESCRI >= wrk_Des_1 .AND. PRO_DESCRI <= wrk_Des_2) '
               wrk_ranpro = ' '
               SHOW GET wrk_cod_1  ;
                    DISABLE
               SHOW GET wrk_cod_2  ;
                    DISABLE
               SHOW GET wrk_des_1  ;
                    ENABLE
               SHOW GET wrk_des_2  ;
                    ENABLE
               @ 4, 43 CLEAR TO 5,  ;
                 66
          ELSE
               wrk_randes = ' '
               wrk_ranpro = ' .AND. (PRO_CODPRO >= wrk_Cod_1 .AND. PRO_CODPRO <= wrk_Cod_2) '
               SHOW GET wrk_cod_1  ;
                    ENABLE
               SHOW GET wrk_cod_2  ;
                    ENABLE
               SHOW GET wrk_des_1  ;
                    DISABLE
               SHOW GET wrk_des_2  ;
                    DISABLE
          ENDIF
ENDCASE
RETURN
*
PROCEDURE ooproceso
PARAMETER npop1
= ooaviso( ;
  ' T R A B A J A N D O . . .')
DO CASE
     CASE npop1 = 1
          SELECT DISTINCT GC_PRO00.*,;
GC_ALM00.ALM_CODALM,  SUM(GC_ALM00.ALM_STKFIS);
 FROM GC_PRO00, GC_ALM00  WHERE GC_PRO00.PRO_CODPRO;
= GC_ALM00.ALM_CODPRO  and   GC_alm00.alm_stkfis;
<> 0  &wrk_RanPro   &wrk_RanDes;
  &wrk_RanAlm   &wrk_RanLin   &wrk_RanMar;
  &wrk_RanRub   &wrk_RanSRub  &wrk_RanPRC;
  &wrk_RanPRP   GROUP BY GC_PRO00.PRO_CODPRO,;
GC_ALM00.ALM_CODALM  ORDER BY GC_ALM00.ALM_CODALM,;
GC_PRO00.PRO_LINEA,  GC_PRO00.PRO_PROCED,;
GC_PRO00.PRO_CODPRO  INTO CURSOR RepExiste
     CASE npop1 = 2
          wrk_arc_01 = 'TMP' +  ;
                       SUBSTR(SYS(3),  ;
                       1, 5) +  ;
                       '.DBF'
          = ooaviso( ;
            ' T R A B A J A N D O . . .' ;
            )
          SELECT DISTINCT *,  ;
                 SUM(gc_tpe00.tpe_cansol),  ;
                 SUM(IIF(gc_tpe00.tpe_indreg =  ;
                 'P',  ;
                 gc_tpe00.tpe_cansol,  ;
                 0)),  ;
                 SUM(IIF(gc_tpe00.tpe_indreg =  ;
                 'T',  ;
                 gc_tpe00.tpe_cansol,  ;
                 0)),  ;
                 SUM(IIF(gc_tpe00.tpe_indreg =  ;
                 'P' AND  ;
                 gc_tpe00.tpe_indori =  ;
                 'C',  ;
                 gc_tpe00.tpe_cansol,  ;
                 0)),  ;
                 SUM(IIF(gc_tpe00.tpe_indreg =  ;
                 'P' AND  ;
                 gc_tpe00.tpe_indori =  ;
                 'V',  ;
                 gc_tpe00.tpe_cansol,  ;
                 0)) FROM  ;
                 GC_TPE00 GROUP  ;
                 BY  ;
                 gc_tpe00.tpe_codpro  ;
                 ORDER BY  ;
                 gc_tpe00.tpe_codpro  ;
                 INTO DBF  ;
                 (wrk_arc_01)
          calias_1 = ALIAS()
          wrk_arc_02 = 'TMP' +  ;
                       SUBSTR(SYS(3),  ;
                       4, 5) +  ;
                       '.DBF'
          wrk_cad001 = SUBSTR(wrk_arc_01,  ;
                       1, 8) +  ;
                       '.TPE_CODPRO = GC_ALM00.ALM_CODPRO .AND. ' +  ;
                       SUBSTR(wrk_arc_01,  ;
                       1, 8) +  ;
                       '.TPE_CODPRO = GC_PRO00.PRO_CODPRO '
          wrk_codigo = SUBSTR(wrk_arc_01,  ;
                       1, 8) +  ;
                       '.TPE_CODPRO'
          wrk_caddat = SUBSTR(wrk_arc_01,  ;
                       1, 8) +  ;
                       '.*'
          SELECT DISTINCT &wrk_CadDat,;
GC_PRO00.*, GC_ALM00.ALM_CODALM,;
 SUM(GC_ALM00.ALM_STKFIS),  SUM(GC_ALM00.ALM_STKRES);
FROM &wrk_Arc_01, GC_ALM00, GC_PRO00 WHERE;
&wrk_Cad001  &wrk_RanPro   &wrk_RanDes;
  &wrk_RanAlm   &wrk_RanLin   &wrk_RanMar;
  &wrk_RanRub   &wrk_RanSRub  &wrk_RanPRC;
  &wrk_RanPRP   GROUP BY &wrk_Codigo ORDER;
BY GC_PRO00.PRO_LINEA,  GC_PRO00.PRO_MARCA,;
GC_PRO00.PRO_RUBRO,  GC_PRO00.PRO_CODPRO,;
&wrk_Codigo  INTO TABLE (wrk_Arc_02)
          calias_2 = ALIAS()
          SELECT (calias_2)
ENDCASE
COUNT TO nreg
IF nreg = 0
     DO p_mensaje WITH  ;
        ' NO SE ENCONTRO INFORMACION '
     DEACTIVATE WINDOW winmensaje
     RETURN
ELSE
     = ooimprime()
ENDIF
IF npop1 = 2
     SELECT (calias_1)
     USE
     ERASE (wrk_arc_01)
     SELECT (calias_2)
     USE
     ERASE (wrk_arc_02)
ENDIF
RETURN
*
PROCEDURE ooproduc
PARAMETER nx, ny1, nlon, cvarread
IF (LASTKEY() = 5) .OR.  ;
   (LASTKEY() = 24)
     RETURN
ENDIF
wrk_selec = SELECT()
wrk_campo = cvarread
ON KEY LABEL ESC
DO oopopprod WITH wrk_campo,  ;
   wrk_selec, wrk_selpro,  ;
   cvarread
= ooareat('GC_PRO00','CODIGO')
SEEK &cVarRead
IF FOUND()
     @ nx, ny1 SAY  ;
       SUBSTR(pro_descri, 1,  ;
       nlon)
ENDIF
RETURN
*
PROCEDURE ooimprime
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
     @ 0, 0 SAY CHR(27) + CHR(15)
     IF (nop1 = 1)
          REPORT FORMAT  ;
                 AGCR0222.FRX  ;
                 ENVIRONMENT  ;
                 PDSETUP NOEJECT  ;
                 TO PRINTER  ;
                 NOCONSOLE
     ELSE
          REPORT FORMAT  ;
                 AGCR222A.FRX  ;
                 ENVIRONMENT  ;
                 PDSETUP NOEJECT  ;
                 TO PRINTER  ;
                 NOCONSOLE
     ENDIF
     SET DEVICE TO PRINTER
     @ 0, 0 SAY CHR(27) + CHR(18)
     SET DEVICE TO SCREEN
     RELEASE WINDOW winmensaje
     EXIT
ENDDO
DO p_mensaje WITH  ;
   ' FIN DE IMPRESION '
RETURN
*
PROCEDURE ooscreen
PARAMETER nscreen
DO CASE
     CASE nscreen = 1
          @ 07, 13 SAY 'Del '  ;
            SIZE 1, 05, 0
          @ 07, 46 SAY 'Al '  ;
            SIZE 1, 05, 0
          @ 09, 13 SAY 'Del '  ;
            SIZE 1, 05, 0
          @ 01, 01 SAY  ;
            'Tipos Reporte :'  ;
            SIZE 1, 15, 0
          @ 02, 01 SAY  ;
            'Tipo Costeo   :'  ;
            SIZE 1, 15, 0
          @ 15, 13 SAY 'Del '  ;
            SIZE 1, 05, 0
          @ 10, 13 SAY 'Del '  ;
            SIZE 1, 05, 0
          @ 10, 46 SAY 'Al '  ;
            SIZE 1, 05, 0
          @ 12, 13 SAY 'Del '  ;
            SIZE 1, 05, 0
          @ 12, 01 SAY  ;
            'Procedencia:' SIZE 1,  ;
            11, 0
          @ 13, 46 SAY 'Al '  ;
            SIZE 1, 05, 0
          @ 07, 01 SAY  ;
            'Almacenes :' SIZE 1,  ;
            11, 0
          @ 15, 01 SAY  ;
            'Rubros    :' SIZE 1,  ;
            11, 0
          @ 09, 01 SAY  ;
            'Lineas    :' SIZE 1,  ;
            11, 0
          @ 04, 23 SAY 'Del '  ;
            SIZE 1, 05, 0
          @ 05, 23 SAY 'Al  '  ;
            SIZE 1, 05, 0
          @ 16, 13 SAY 'Del '  ;
            SIZE 1, 05, 0
          @ 16, 46 SAY 'Al '  ;
            SIZE 1, 05, 0
          @ 09, 46 SAY 'Al '  ;
            SIZE 1, 05, 0
          @ 10, 01 SAY  ;
            'Marcas    :' SIZE 1,  ;
            11, 0
          @ 12, 46 SAY 'Al '  ;
            SIZE 1, 05, 0
          @ 13, 01 SAY  ;
            'Propiedad :' SIZE 1,  ;
            11, 0
          @ 13, 13 SAY 'Del '  ;
            SIZE 1, 05, 0
          @ 15, 46 SAY 'Al '  ;
            SIZE 1, 05, 0
          @ 16, 01 SAY  ;
            'SubRubro  :' SIZE 1,  ;
            11, 0
          @ 00, 00 TO 18, 79
          @ 03, 01 TO 3, 78
          @ 06, 01 TO 6, 78
          @ 08, 01 TO 8, 78
          @ 06, 00 SAY 'Ã' SIZE 1,  ;
            1, 0
          @ 03, 00 SAY 'Ã' SIZE 1,  ;
            1, 0
          @ 03, 77 SAY '´' SIZE 1,  ;
            1, 0
          @ 06, 77 SAY '´' SIZE 1,  ;
            1, 0
          @ 08, 00 SAY 'Ã' SIZE 1,  ;
            1, 0
          @ 08, 77 SAY '´' SIZE 1,  ;
            1, 0
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

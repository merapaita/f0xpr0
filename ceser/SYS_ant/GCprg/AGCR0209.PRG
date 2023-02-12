*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PUBLIC wrk_campo, wrk_selpro,  ;
       wrk_selec, wrk_ranpro,  ;
       wrk_randes, wrk_ranalm,  ;
       wrk_ranlin, wrk_ranmar,  ;
       wrk_ranrub, wrk_ransru,  ;
       wrk_rancat, wrk_ransca,  ;
       wrk_select, wrk_mes1,  ;
       wrk_mes2, wrk_ano1,  ;
       wrk_ano2, ntotmes
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
   'ROTACION DE INVENTARIO',  ;
   'REPORTE'
wrk_progra = PROGRAM()
@ 2, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100000000000001000011', 2
= ooopen('GC_KAR00',1)
= ooopen('GC_PRO00',1)
wrk_selpro = SELECT()
DEFINE WINDOW winexiste FROM 4, 1  ;
       TO 21, 78 NOFLOAT NOCLOSE  ;
       NOMINIMIZE NONE COLOR  ;
       SCHEME 1
wrk_ranpro = ' .AND. (PRO_CODPRO >= wrk_Cod_1 .AND. PRO_CODPRO <= wrk_Cod_2) '
wrk_randes = ' '
wrk_ranalm = ' .AND. (ALM_CODALM >= wrk_Alma_1 .AND. ALM_CODALM <= wrk_Alma_2) '
wrk_ranlin = ' '
wrk_ranmar = ' '
wrk_ranrub = ' '
wrk_ransru = ' '
wrk_rancat = ' '
wrk_ransca = ' '
ntotmes = 0
wrk_mes1 = MONTH(DATE())
wrk_mes2 = MONTH(DATE())
wrk_ano1 = YEAR(DATE())
wrk_ano2 = YEAR(DATE())
wrk_progra = PROGRAM()
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
ACTIVATE WINDOW SAME winmarco
= ooscreen(1)
@ 1, 24 GET wrk_mes1 DEFAULT 0  ;
  SIZE 1, 2, 0 VALID  ;
  oovalid(VARREAD())
@ 1, 27 GET wrk_ano1 DEFAULT 0  ;
  SIZE 1, 4, 0 VALID  ;
  oovalid(VARREAD())
@ 2, 24 GET wrk_mes2 DEFAULT 0  ;
  SIZE 1, 2, 0 VALID  ;
  oovalid(VARREAD())
@ 2, 27 GET wrk_ano2 DEFAULT 0  ;
  SIZE 1, 4, 0 VALID  ;
  oovalid(VARREAD())
@ 4, 1 GET nop3 DEFAULT 1 SIZE 1,  ;
  19, 0 PICTURE  ;
  '@*RVN Por C¢digo;Por Descripci¢n'  ;
  VALID oovalid(VARREAD()) COLOR  ;
  SCHEME 12
@ 4, 29 GET wrk_cod_1 DEFAULT ' '  ;
  SIZE 1, 14 VALID ooproduc(4,43, ;
  33,VARREAD()) COLOR SCHEME 6
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
@ 12, 19 GET wrk_cat_1 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(12, ;
  19,24,19,'CATE',VARREAD())  ;
  COLOR SCHEME 6
@ 12, 51 GET wrk_cat_2 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(12, ;
  51,56,20,'CATE',VARREAD())  ;
  COLOR SCHEME 6
@ 13, 19 GET wrk_scat_1 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(13, ;
  19,24,19,'SUCA',VARREAD())  ;
  COLOR SCHEME 6
@ 13, 51 GET wrk_scat_2 DEFAULT  ;
  ' ' SIZE 1, 4 VALID oorangos(13, ;
  51,56,20,'SUCA',VARREAD())  ;
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
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'WRK_MES1'
          IF wrk_mes1 > 12 .OR.  ;
             wrk_mes1 < 1
               DO p_mensaje WITH  ;
                  ' MES NO VALIDO '
               RETURN .F.
          ENDIF
     CASE cvalid = 'WRK_MES2'
          IF wrk_mes2 > 12 .OR.  ;
             wrk_mes2 < 1
               DO p_mensaje WITH  ;
                  ' MES NO VALIDO '
               RETURN .F.
          ENDIF
     CASE cvalid = 'WRK_ANO1'
          IF (wrk_ano1 < 1990)  ;
             .OR. (wrk_ano1 >  ;
             YEAR(DATE()))
               DO p_mensaje WITH  ;
                  ' A¥O NO VALIDO '
               RETURN .F.
          ENDIF
     CASE cvalid = 'WRK_ANO2'
          IF (wrk_ano2 < 1990)  ;
             .OR. (wrk_ano2 >  ;
             YEAR(DATE()))
               DO p_mensaje WITH  ;
                  ' A¥O NO VALIDO '
               RETURN .F.
          ENDIF
          IF wrk_ano2 < wrk_ano1
               DO p_mensaje WITH  ;
                  ' A¥O INICIAL MAYOR QUE A¥O FINAL'
               RETURN .F.
          ENDIF
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
wrk_arc_01 = 'TMP' +  ;
             SUBSTR(SYS(3), 1, 5) +  ;
             '.DBF'
= ooaviso( ;
  ' T R A B A J A N D O . . .')
ntotmes = oocalmes()
SELECT DISTINCT GC_PRO00.*, GC_ALM00.ALM_CODALM,;
 GC_ALM00.ALM_STKFIS, GC_ALM00.ALM_STKRES;
FROM GC_ALM00, GC_PRO00 WHERE (GC_PRO00.PRO_CODPRO;
= GC_ALM00.ALM_CODPRO)  &wrk_RanPro;
  &wrk_RanDes   &wrk_RanAlm   &wrk_RanLin;
  &wrk_RanMar   &wrk_RanRub   &wrk_RanSRub;
 &wrk_RanCat   &wrk_RanSCat  GROUP BY;
GC_PRO00.PRO_CODPRO ORDER BY GC_ALM00.ALM_CODALM,;
GC_PRO00.PRO_LINEA,  GC_PRO00.PRO_CATEGO,;
GC_PRO00.PRO_MARCA,  GC_PRO00.PRO_RUBRO,;
GC_PRO00.PRO_CODPRO  INTO TABLE (wrk_Arc_01)
calias_1 = ALIAS()
SELECT (calias_1)
COUNT TO nreg
IF nreg = 0
     DO p_mensaje WITH  ;
        ' NO SE ENCONTRO INFORMACION '
     DEACTIVATE WINDOW winmensaje
     RETURN
ENDIF
= ooimprime()
SELECT (calias_1)
USE
ERASE (wrk_arc_01)
RETURN
*
FUNCTION oocalven
PARAMETER ccodpro
wrk_venta = 0
= ooarea('GC_KAR00')
SET ORDER TO 1
GOTO TOP
SEEK ccodpro
DO WHILE (kar_codpro=ccodpro)  ;
   .AND. (kar_codmov='EVTA')  ;
   .AND. (MONTH(kar_fecing)>= ;
   wrk_mes1 .AND.  ;
   MONTH(kar_fecing)<=wrk_mes2)  ;
   .AND. (YEAR(kar_fecing)>= ;
   wrk_ano1 .AND.  ;
   YEAR(kar_fecing)<=wrk_ano2)
     wrk_venta = wrk_venta +  ;
                 kar_cantid
     SKIP
ENDDO
SELECT (calias_1)
RETURN wrk_venta
*
FUNCTION oocalmes
IF wrk_ano1 <> wrk_ano2
     nmes1 = 12 - wrk_mes1
     nptotmes = nmes1 + wrk_mes2
ELSE
     nptotmes = wrk_mes2 -  ;
                wrk_mes1
ENDIF
RETURN nptotmes
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
DO produc WITH wrk_campo,  ;
   wrk_selec, wrk_selpro
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
     MODIFY REPORT AGCR0209.FRX
     REPORT FORMAT AGCR0209.FRX  ;
            ENVIRONMENT PDSETUP  ;
            NOEJECT TO PRINTER  ;
            NOCONSOLE
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
          @ 7, 13 SAY 'Del '  ;
            SIZE 1, 5, 0
          @ 7, 46 SAY 'Al ' SIZE  ;
            1, 5, 0
          @ 9, 13 SAY 'Del '  ;
            SIZE 1, 5, 0
          @ 1, 3 SAY  ;
            'Fecha Inicio mm/aa :'  ;
            SIZE 1, 25, 0
          @ 1, 26 SAY '/' SIZE 1,  ;
            1, 0 COLOR W+/N 
          @ 2, 3 SAY  ;
            'Fecha Final  mm/aa :'  ;
            SIZE 1, 25, 0
          @ 2, 26 SAY '/' SIZE 1,  ;
            1, 0 COLOR W+/N 
          @ 15, 13 SAY 'Del '  ;
            SIZE 1, 5, 0
          @ 10, 13 SAY 'Del '  ;
            SIZE 1, 5, 0
          @ 10, 46 SAY 'Al '  ;
            SIZE 1, 5, 0
          @ 12, 13 SAY 'Del '  ;
            SIZE 1, 5, 0
          @ 12, 1 SAY  ;
            'Categoria :' SIZE 1,  ;
            11, 0
          @ 13, 46 SAY 'Al '  ;
            SIZE 1, 5, 0
          @ 7, 1 SAY  ;
            'Almacenes :' SIZE 1,  ;
            11, 0
          @ 15, 1 SAY  ;
            'Rubros    :' SIZE 1,  ;
            11, 0
          @ 9, 1 SAY  ;
            'Lineas    :' SIZE 1,  ;
            11, 0
          @ 4, 23 SAY 'Del '  ;
            SIZE 1, 5, 0
          @ 5, 23 SAY 'Al  '  ;
            SIZE 1, 5, 0
          @ 16, 13 SAY 'Del '  ;
            SIZE 1, 5, 0
          @ 16, 46 SAY 'Al '  ;
            SIZE 1, 5, 0
          @ 9, 46 SAY 'Al ' SIZE  ;
            1, 5, 0
          @ 10, 1 SAY  ;
            'Marcas    :' SIZE 1,  ;
            11, 0
          @ 12, 46 SAY 'Al '  ;
            SIZE 1, 5, 0
          @ 13, 1 SAY  ;
            'SubCateg. :' SIZE 1,  ;
            11, 0
          @ 13, 13 SAY 'Del '  ;
            SIZE 1, 5, 0
          @ 15, 46 SAY 'Al '  ;
            SIZE 1, 5, 0
          @ 16, 1 SAY  ;
            'SubRubro  :' SIZE 1,  ;
            11, 0
          @ 0, 0 TO 18, 79
          @ 3, 1 TO 3, 78
          @ 6, 1 TO 6, 78
          @ 8, 1 TO 8, 78
          @ 6, 0 SAY 'Ã' SIZE 1,  ;
            1, 0
          @ 3, 0 SAY 'Ã' SIZE 1,  ;
            1, 0
          @ 3, 77 SAY '´' SIZE 1,  ;
            1, 0
          @ 6, 77 SAY '´' SIZE 1,  ;
            1, 0
          @ 8, 0 SAY 'Ã' SIZE 1,  ;
            1, 0
          @ 8, 77 SAY '´' SIZE 1,  ;
            1, 0
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

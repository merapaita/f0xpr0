*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
SELECT 1
USE SHARED gc_pro00 ORDER  ;
    PRO_CODPVE
SELECT 2
USE SHARED gc_din00 ORDER codigo
SELECT 3
USE SHARED gc_cli00 ORDER codigo
SELECT 4
USE SHARED gc_nve00 ORDER codigo
SELECT 5
USE SHARED ge_tab0 ORDER codigo
SELECT 6
USE SHARED gc_alm00 ORDER codigo
SET RELATION TO 'ALMA' + alm_codalm INTO;
ge_tab0 ADDITIVE
SELECT 7
USE SHARED gc_dlp00 ORDER codigo
SELECT 20
STORE 0 TO w_tc, w_tv
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
w_tv = ootc2(DATE(),'SOL ','DOL ', ;
       '2')
SELECT gc_pro00
w_selec = SELECT()
w_selpro = SELECT()
SET RELATION TO pro_codpro INTO gc_din00,;
pro_codpro INTO gc_nve00, pro_codpro +;
rge_codalm INTO gc_alm00, 'P' + pro_codpve;
INTO gc_cli00, 'PUBL' + pro_codpro INTO;
gc_dlp00 ADDITIVE
DEFINE WINDOW vent_0 FROM 22, 00  ;
       TO 24, 79 IN screen COLOR  ;
       SCHEME 20
DEFINE WINDOW vent_1 FROM 00, 00  ;
       TO 21, 79 TITLE  ;
       'ÄÄÄÄÄÄÄÄÄÄÄÄÄ Consulta de Repuestos  ÄÄÄÄÄÄÄÄÄÄÄÄÄ'  ;
       IN screen COLOR SCHEME 24
DEFINE WINDOW vent_3 FROM 14, 10  ;
       TO 17, 30 TITLE  ;
       'ÄÄÄ Seleccione ÄÄÄÄ' IN  ;
       screen COLOR SCHEME 12
DEFINE WINDOW vent_2 FROM 18, 10  ;
       TO 20, 60 TITLE  ;
       'ÄÄÄ Ingrese el Texto de B£squeda ÄÄÄÄ'  ;
       IN screen COLOR SCHEME 12
DEFINE WINDOW vent_4 FROM 14, 01  ;
       TO 20, 65 TITLE  ;
       'ÄÄÄ  Stock en Almacenes ÄÄÄ'  ;
       IN screen COLOR SCHEME 8
DEFINE WINDOW vent_5 FROM 14, 01  ;
       TO 20, 78 TITLE  ;
       'ÄÄÄ  Ultimas Compras ÄÄÄ'  ;
       IN screen COLOR SCHEME 8
DEFINE WINDOW vent_6 FROM 14, 01  ;
       TO 20, 78 TITLE  ;
       'ÄÄÄ  Ultimas Compras de Proveedores ÄÄÄ'  ;
       IN screen COLOR SCHEME 8
DO p_footer WITH  ;
   '1000100100000001100011101',  ;
   2
@ 00, 01 SAY '[ F1 ]' COLOR N/W 
@ 00, 08 SAY 'Ayuda      '
@ 00, 21 SAY '[ F5 ]' COLOR N/W 
@ 00, 28 SAY 'Almac‚n    '
@ 00, 41 SAY '[ F6 ]' COLOR N/W 
@ 00, 48 SAY 'Buscar     '
@ 00, 61 SAY '[ F8 ]' COLOR N/W 
@ 00, 68 SAY 'Compras    '
@ 01, 01 SAY '[ F9 ]' COLOR N/W 
@ 01, 08 SAY 'Ult.Compras'
@ 01, 21 SAY '[End ]' COLOR N/W 
@ 01, 28 SAY 'Ultimo     '
@ 01, 41 SAY '[ Up ]' COLOR N/W 
@ 01, 48 SAY 'Avanza P g.'
codigo = SPACE(14)
w_codigo = SPACE(14)
STORE 0 TO w_totalf, w_totalr
w_alma = rge_codalm
w_desalm = ootab('ALMA',w_alma)
ACTIVATE WINDOW vent_1
DO dato
tmp_x = 0
DO WHILE tmp_x<>27
     tmp_x = INKEY(0, 'H')
     DO CASE
          CASE tmp_x = -7
               DO compra
          CASE tmp_x = -5
               DO rut_1
          CASE tmp_x = -4
               DO alma
          CASE tmp_x = 1
               GOTO TOP
               DO dato
          CASE tmp_x = -8
               DO ucompra
          CASE tmp_x = 3
               SKIP
               IF EOF()
                    GOTO BOTTOM
               ENDIF
               DO dato
          CASE tmp_x = 6
               GOTO BOTTOM
               DO dato
          CASE tmp_x = 18
               SKIP -1
               IF BOF()
                    GOTO TOP
               ENDIF
               DO dato
     ENDCASE
ENDDO
RELEASE WINDOW vent_0, vent_1,  ;
        vent_2, vent_3, vent_4
SET RELATION TO
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE rut_1
ACTIVATE WINDOW vent_3
@ 00, 00 GET opcion DEFAULT 1  ;
  SIZE 1, 10, 0 PICTURE  ;
  '@*+VN C¢digo;Descripci¢n'  ;
  VALID rut_2(opcion) COLOR  ;
  SCHEME 10
READ CYCLE
IF LASTKEY() <> 27
     ACTIVATE WINDOW vent_2
     @ 00, 00 GET codigo PICTURE  ;
       '@!'
     READ
     SEEK codigo
ENDIF
DEACTIVATE WINDOW vent_2, vent_3
ACTIVATE WINDOW vent_1
SELECT gc_pro00
SET NEAR OFF
DO dato
RETURN
*
PROCEDURE rut_2
PARAMETER opc
SET NEAR ON
DO CASE
     CASE opc = 1
          codigo = SPACE(14)
          SELECT gc_pro00
          SET ORDER TO codigo
          CLEAR READ
     CASE opc = 2
          codigo = SPACE(40)
          SELECT gc_pro00
          SET ORDER TO descri
          CLEAR READ
ENDCASE
RETURN
*
PROCEDURE dato
SELECT gc_pro00
@ 00, 00 SAY 'C¢digo.........: ' +  ;
  pro_codpro
@ 00, 32 SAY 'N.Pte:' +  ;
  pro_numpar
w_codigo = pro_codpro
@ 01, 00 SAY 'Descri.Espa¤ol.: ' +  ;
  SUBSTR(pro_descri, 1, 35)
IF gc_pro00.pro_estope = 'B'
     @ 02, 00 SAY  ;
       '  < < < R E G I S T R O   B L O Q U E A D O > > >'  ;
       COLOR W+/N 
ELSE
     @ 02, 00 SAY  ;
       'Descrip.Ingl‚s.: ' +  ;
       SUBSTR(gc_din00.din_descri,  ;
       1, 35)
ENDIF
@ 03, 00 SAY 'Reemplazo......: ' +  ;
  gc_pro00.pro_codree
@ 04, 00 SAY 'Modelo.........: ' +  ;
  pro_modelo
@ 05, 00 SAY 'Unidad.........: ' +  ;
  pro_unimed
@ 06, 00 SAY 'Peso Gramos....: '
@ 06, 16 SAY pro_peso PICTURE  ;
  '9,999,999.99'
@ 07, 00 SAY 'Categor¡a......: ' +  ;
  pro_catego
@ 08, 00 SAY 'SubCategor¡a...: ' +  ;
  pro_subcat
@ 09, 00 SAY 'Partida........: ' +  ;
  pro_parara
IF xnnn <> 'A1'
     @ 10, 00 SAY  ;
       'Margen.........: ' +  ;
       pro_clacom
     SELECT ge_tab0
     SEEK 'CMCO' +  ;
          gc_pro00.pro_clacom
     IF FOUND()
          @ 10, 18 SAY tab_factor  ;
            PICTURE '999,999.99'
          @ 10, 30 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     ELSE
          @ 10, 18 SAY SPACE(40)
     ENDIF
ENDIF
SELECT gc_pro00
@ 11, 00 SAY 'Proveedor......: ' +  ;
  pro_codpve
@ 12, 00 SAY 'Nomb.Proveedor.: ' +  ;
  gc_cli00.cli_razsoc
IF xnnn <> 'A1'
     @ 13, 00 SAY  ;
       'Precio Fob.....:'
     @ 13, 16 SAY pro_coremo  ;
       PICTURE '9,999,999.99'
ENDIF
@ 14, 00 SAY 'P/' +  ;
  ALLTRIM(sys_codimp) + '.' +  ;
  ALLTRIM(sys_simomb) +  ;
  '......:'
@ 14, 16 SAY gc_dlp00.dlp_prcigv  ;
  PICTURE '9,999,999.99' COLOR W+/ ;
  N 
@ 15, 00 SAY 'P/' +  ;
  ALLTRIM(sys_codimp) + '.' +  ;
  ALLTRIM(sys_simmon) +  ;
  '......:'
precioso = ROUND(gc_dlp00.dlp_prcigv *  ;
           w_tv, 2)
@ 15, 16 SAY precioso PICTURE  ;
  '9,999,999.99' COLOR W+/N 
SELECT gc_pro00
IF xnnn <> 'A1'
     @ 13, 30 SAY 'C.P.' +  ;
       ALLTRIM(sys_simomb) +  ;
       '.:'
     @ 13, 41 SAY pro_coprmo  ;
       PICTURE '999,999.99'
     @ 14, 30 SAY 'C.P.' +  ;
       ALLTRIM(sys_simmon) +  ;
       '.:'
     @ 14, 41 SAY pro_coprmb  ;
       PICTURE '999,999.99'
     @ 15, 30 SAY 'U.C.' +  ;
       ALLTRIM(sys_simomb) +  ;
       '.:'
     @ 15, 41 SAY pro_ulcomb  ;
       PICTURE '999,999.99'
ENDIF
@ 16, 01 SAY 'Mes1' + SPACE(5) +  ;
  'Mes2' + SPACE(5) + 'Mes3' +  ;
  SPACE(5) + 'Mes4' + SPACE(5) +  ;
  'Mes5' + SPACE(4) + 'Mes6'
@ 17, 00 SAY gc_nve00.nve_cmes1  ;
  PICTURE '9,999'
@ 17, 08 SAY gc_nve00.nve_cmes2  ;
  PICTURE '9,999'
@ 17, 17 SAY gc_nve00.nve_cmes3  ;
  PICTURE '9,999'
@ 17, 27 SAY gc_nve00.nve_cmes4  ;
  PICTURE '9,999'
@ 17, 36 SAY gc_nve00.nve_cmes5  ;
  PICTURE '9,999'
@ 17, 44 SAY gc_nve00.nve_cmes6  ;
  PICTURE '9,999'
@ 18, 00 SAY 'NVM1'
@ 18, 05 SAY 'NVM2'
@ 18, 10 SAY 'NVM3'
@ 18, 15 SAY 'NVM4'
@ 18, 20 SAY 'NVM5'
@ 18, 25 SAY 'NVM6'
@ 18, 30 SAY 'NVM7'
@ 18, 35 SAY 'NVM8'
@ 18, 40 SAY 'NVM9'
@ 18, 45 SAY 'NV10'
@ 18, 50 SAY 'NV11'
@ 18, 55 SAY 'NV12'
@ 19, 00 SAY gc_nve00.nve_mes1  ;
  PICTURE '999'
@ 19, 05 SAY gc_nve00.nve_mes2  ;
  PICTURE '999'
@ 19, 10 SAY gc_nve00.nve_mes3  ;
  PICTURE '999'
@ 19, 15 SAY gc_nve00.nve_mes4  ;
  PICTURE '999'
@ 19, 20 SAY gc_nve00.nve_mes5  ;
  PICTURE '999'
@ 19, 25 SAY gc_nve00.nve_mes6  ;
  PICTURE '999'
@ 19, 30 SAY gc_nve00.nve_mes7  ;
  PICTURE '999'
@ 19, 35 SAY gc_nve00.nve_mes8  ;
  PICTURE '999'
@ 19, 40 SAY gc_nve00.nve_mes9  ;
  PICTURE '999'
@ 19, 45 SAY gc_nve00.nve_mes10  ;
  PICTURE '999'
@ 19, 50 SAY gc_nve00.nve_mes11  ;
  PICTURE '999'
@ 19, 55 SAY gc_nve00.nve_mes12  ;
  PICTURE '999'
@ 00, 53 SAY 'Stock M ximo..:'
@ 00, 68 SAY pro_stkmax PICTURE  ;
  '99,999,999'
@ 01, 53 SAY 'Stock Cr¡tico.:'
@ 01, 68 SAY pro_critic PICTURE  ;
  '99,999,999'
@ 02, 53 SAY 'Stock M¡nimo..:'
@ 02, 68 SAY pro_stkmin PICTURE  ;
  '99,999,999'
@ 03, 53 SAY 'Nivel.........:'
@ 03, 77 SAY pro_nivcit
@ 04, 53 SAY 'Rang.de Compra:'
@ 04, 77 SAY pro_rcom
@ 05, 53 SAY 'Fecha U.Compra:'
@ 05, 68 SAY pro_ultcom
@ 06, 53 SAY 'Fecha U.Venta.:'
@ 06, 68 SAY pro_ultven
@ 07, 53 SAY 'C.Solicitada..:'
@ 07, 68 SAY pro_stksol PICTURE  ;
  '99,999,999'
SELECT gc_alm00
@ 14, 68 SAY gc_alm00.alm_ubicac
GOTO TOP
SUM FOR f.alm_codpro = w_codigo  ;
    .AND. f.alm_codalm <>  ;
    rge_codalm f.alm_stkfis TO  ;
    w_totalf
GOTO TOP
SUM FOR f.alm_codpro = w_codigo  ;
    .AND. f.alm_codalm =  ;
    rge_codalm f.alm_stkfis TO  ;
    w_totals
GOTO TOP
SUM FOR gc_alm00.alm_codpro =  ;
    w_codigo gc_alm00.alm_stkres  ;
    TO w_totalr
SELECT gc_pro00
@ 08, 53 SAY 'C.Tr nsito....:'
@ 08, 68 SAY pro_stktra PICTURE  ;
  '99,999,999'
@ 09, 53 SAY 'C.Backorder...:'
@ 09, 68 SAY pro_stkbor PICTURE  ;
  '99,999,999'
@ 10, 53 SAY 'S.F.' +  ;
  SUBSTR(w_desalm, 1, 10) +  ;
  '..:'
@ 10, 68 SAY w_totals PICTURE  ;
  '99,999,999'
@ 11, 53 SAY 'S.F.Otros.....:'
@ 11, 68 SAY w_totalf PICTURE  ;
  '99,999,999'
@ 12, 53 SAY 'S.Reservado...:'
@ 12, 68 SAY w_totalr PICTURE  ;
  '99,999,999'
@ 13, 53 SAY 'S.Da¤ado......:'
@ 13, 71 SAY pro_dana PICTURE  ;
  '999,999'
@ 14, 53 SAY 'Ubicaci¢n.....:'
@ 15, 53 SAY 'Lote de Compra:'
@ 15, 68 SAY pro_lotcom PICTURE  ;
  '999,999.99'
@ 16, 53 SAY 'C¢digo de Edad:'
@ 16, 75 SAY pro_edad PICTURE  ;
  '999'
@ 17, 53 SAY 'Fecha Creaci¢n:'
@ 17, 68 SAY pro_crepro
@ 18, 63 SAY 'EOP.:'
@ 18, 77 SAY pro_estope
@ 19, 63 SAY 'FPM.:'
@ 19, 70 SAY (gc_nve00.nve_cmes1 +  ;
  gc_nve00.nve_cmes2 +  ;
  gc_nve00.nve_cmes3 +  ;
  gc_nve00.nve_cmes4 +  ;
  gc_nve00.nve_cmes5 +  ;
  gc_nve00.nve_cmes6) / 6 PICTURE  ;
  '9,999.99'
RETURN
*
PROCEDURE alma
ACTIVATE WINDOW vent_4
SELECT gc_alm00
SEEK w_codigo
BROWSE FIELDS alm_codalm : 4 :H =  ;
       'CODI', ge_tab0.tab_destab  ;
       : 20 :H = 'ALMACEN',  ;
       alm_ubicac : 10 :H =  ;
       'UBICACION', alm_stkfis :P =  ;
       '999,999' :H = 'FISICO',  ;
       alm_stkres :P = '999,999'  ;
       :H = 'RESERVA', v =  ;
       alm_stkfis - alm_stkres :P =  ;
       '999,999' :H = 'DISPONIB'  ;
       NOEDIT KEY w_codigo IN  ;
       vent_4
DEACTIVATE WINDOW vent_4
SELECT gc_pro00
RETURN
*
PROCEDURE compra
SELECT 20
USE SHARED gc_hip00 ORDER codigo
SET RELATION TO 'P' + hip_codent INTO;
gc_cli00
SELECT 21
USE SHARED gc_kar00 ORDER  ;
    kar_proing
SET RELATION TO kar_tipdoc + kar_nrodoc;
INTO gc_hip00
CREATE CURSOR compra (codigo C  ;
       (14), fecha D, nrodoc C  ;
       (10), provee C (9), despro  ;
       C (30), precio N (6, 2),  ;
       cantid N (6, 0), total N  ;
       (9, 2), telefono N (8,  ;
       0))
SELECT gc_kar00
SEEK w_codigo
m = 0
SCAN WHILE w_codigo = kar_codpro  ;
     .AND.  .NOT. EOF() .AND. m <  ;
     3
     SELECT gc_hip00
     SEEK gc_kar00.kar_tipdoc +  ;
          gc_kar00.kar_nrodoc
     IF FOUND() .AND.  ;
        (SUBSTR(hip_codmov, 1, 1) =  ;
        'I' .AND.  ;
        ALLTRIM(hip_codmov) <>  ;
        'IFD')
          m = m + 1
          SELECT compra
          APPEND BLANK
          REPLACE codigo WITH  ;
                  gc_kar00.kar_codpro
          REPLACE nrodoc WITH  ;
                  gc_kar00.kar_nrodoc
          REPLACE precio WITH  ;
                  gc_kar00.kar_import
          REPLACE cantid WITH  ;
                  gc_kar00.kar_cantid
          REPLACE total WITH  ;
                  ROUND(cantid *  ;
                  precio, 2)
          REPLACE provee WITH  ;
                  gc_hip00.hip_codent
          REPLACE fecha WITH  ;
                  gc_hip00.hip_fecha
          SELECT gc_cli00
          SEEK 'P' +  ;
               gc_hip00.hip_codent
          IF FOUND()
               SELECT compra
               REPLACE telefono  ;
                       WITH  ;
                       gc_cli00.cli_telefo
               REPLACE despro  ;
                       WITH  ;
                       gc_cli00.cli_razsoc
          ENDIF
     ENDIF
     SELECT gc_kar00
ENDSCAN
SELECT compra
COUNT TO n
IF n = 0
     DO p_mensaje WITH  ;
        ' No Hay Compras Para Este C¢digo'
     SELECT gc_pro00
     RETURN
ENDIF
ACTIVATE WINDOW vent_5
GOTO TOP
BROWSE FIELDS fecha :P =  ;
       '99/99/99' :H = ' FECHA ',  ;
       nrodoc :P = '9999999999'  ;
       :H = ' NRODOC ', despro :  ;
       15 :H = ' PROVEEDOR ',  ;
       precio :P = '999,999.99'  ;
       :H = 'P.UNITARIO', cantid  ;
       :P = '999,999' :H =  ;
       'CANTID.', total :P =  ;
       '99,999.99' :H =  ;
       '    TOTAL', telefono :H =  ;
       'TELEFONO' NOEDIT IN  ;
       vent_5
DEACTIVATE WINDOW vent_5
SELECT gc_pro00
RETURN
*
PROCEDURE ucompra
SELECT 20
USE GC_UCO00 ORDER CODIGO
SEEK w_codigo
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        'No hay compras a Proveedores para ‚ste C¢digo'
     SELECT gc_pro00
     RETURN
ENDIF
CREATE CURSOR ucompra (provee C  ;
       (9), despro C (20), docum  ;
       C (10), fecha D, cantid N  ;
       (6), precio N (9, 2))
IF gc_uco00.uco_prov01 <>  ;
   SPACE(9)
     APPEND BLANK
     REPLACE provee WITH  ;
             gc_uco00.uco_prov01
     REPLACE despro WITH  ;
             oodespve(gc_uco00.uco_prov01)
     REPLACE docum WITH  ;
             gc_uco00.uco_ndoc01
     REPLACE fecha WITH  ;
             gc_uco00.uco_fdoc01
     REPLACE cantid WITH  ;
             gc_uco00.uco_cant01
     REPLACE precio WITH  ;
             gc_uco00.uco_prec01
ENDIF
IF gc_uco00.uco_prov02 <>  ;
   SPACE(9)
     APPEND BLANK
     REPLACE provee WITH  ;
             gc_uco00.uco_prov02
     REPLACE despro WITH  ;
             oodespve(gc_uco00.uco_prov02)
     REPLACE docum WITH  ;
             gc_uco00.uco_ndoc02
     REPLACE fecha WITH  ;
             gc_uco00.uco_fdoc02
     REPLACE cantid WITH  ;
             gc_uco00.uco_cant02
     REPLACE precio WITH  ;
             gc_uco00.uco_prec02
ENDIF
IF gc_uco00.uco_prov03 <>  ;
   SPACE(9)
     APPEND BLANK
     REPLACE provee WITH  ;
             gc_uco00.uco_prov03
     REPLACE despro WITH  ;
             oodespve(gc_uco00.uco_prov03)
     REPLACE docum WITH  ;
             gc_uco00.uco_ndoc03
     REPLACE fecha WITH  ;
             gc_uco00.uco_fdoc03
     REPLACE cantid WITH  ;
             gc_uco00.uco_cant03
     REPLACE precio WITH  ;
             gc_uco00.uco_prec03
ENDIF
IF gc_uco00.uco_prov04 <>  ;
   SPACE(9)
     APPEND BLANK
     REPLACE provee WITH  ;
             gc_uco00.uco_prov04
     REPLACE despro WITH  ;
             oodespve(gc_uco00.uco_prov04)
     REPLACE docum WITH  ;
             gc_uco00.uco_ndoc04
     REPLACE fecha WITH  ;
             gc_uco00.uco_fdoc04
     REPLACE cantid WITH  ;
             gc_uco00.uco_cant04
     REPLACE precio WITH  ;
             gc_uco00.uco_prec04
ENDIF
IF gc_uco00.uco_prov05 <>  ;
   SPACE(9)
     APPEND BLANK
     REPLACE provee WITH  ;
             gc_uco00.uco_prov05
     REPLACE despro WITH  ;
             oodespve(gc_uco00.uco_prov05)
     REPLACE docum WITH  ;
             gc_uco00.uco_ndoc05
     REPLACE fecha WITH  ;
             gc_uco00.uco_fdoc05
     REPLACE cantid WITH  ;
             gc_uco00.uco_cant05
     REPLACE precio WITH  ;
             gc_uco00.uco_prec05
ENDIF
ACTIVATE WINDOW vent_6
SELECT ucompra
GOTO TOP
BROWSE NOEDIT IN vent_6
DEACTIVATE WINDOW vent_6
SELECT gc_pro00
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
CLOSE DATABASES
DEFINE WINDOW ayuda FROM 04, 15  ;
       TO 12, 70 TITLE  ;
       'AYUDA DE DOCUMENTOS'  ;
       DOUBLE COLOR SCHEME 7
DEFINE WINDOW cabe FROM 04, 02 TO  ;
       12, 77 COLOR SCHEME 7
DEFINE WINDOW totales FROM 20, 02  ;
       TO 21, 77 NONE COLOR  ;
       SCHEME 6
DEFINE POPUP gastos FROM 15, 27  ;
       TO 19, 54 PROMPT FIELDS  ;
       SUBSTR(tab_destab, 1, 10) +  ;
       '=' + TRANSFORM(gas_valor,  ;
       '@999,999.99') TITLE  ;
       'GASTOS' MARK CHR(16)  ;
       MARGIN IN screen SCROLL  ;
       COLOR SCHEME 9
DEFINE POPUP detalle FROM 13, 02  ;
       TO 19, 77 PROMPT FIELDS  ;
       codigo + ' ³' +  ;
       SUBSTR(descri, 1, 25) +  ;
       '³' + STR(cantid, 6) + '³' +  ;
       TRANSFORM(import,  ;
       '@ 999,999.99') + '³' +  ;
       TRANSFORM(total,  ;
       '@ 9,999,999.99') TITLE  ;
       'PRODUCTOÄÄÄÄÄÄÄÄÄDESCRIPCIONÄÄÄÄÄÄÄÄÄÄÄÄÄCANT.ÄÄÄÄP. UNIT.ÄÄÄÄÄÄTOTAL'  ;
       MARK CHR(16) MARGIN IN  ;
       screen SCROLL COLOR SCHEME  ;
       8
ON SELECTION POPUP detalle DEAC POPUP;
detalle
SELECT 1
USE GE_TAB0 ORDER CODIGO
SELECT 2
USE GC_CLI00 ORDER CODIGO
SELECT 3
USE GC_VND00 ORDER CODIGO
SELECT 4
USE GC_HIP00 ORDER CODIGO
SELECT 5
USE GC_GAS00 ORDER CODIGO
SELECT 6
USE GC_PRO00 ORDER CODIGO
SELECT 7
USE GC_DIP00 ORDER CODIGO
ACTIVATE WINDOW tablas
DO p_prestab WITH 'CONSULTAS',  ;
   'MOVIMIENTO DE ALMACEN',  ;
   'CONSULTA'
@ 2, 63 SAY PROGRAM()
cont = 0
DO WHILE .T.
     DO p_footer WITH  ;
        '100000000000001110001',  ;
        2
     ACTIVATE WINDOW cabe
     STORE SPACE(4) TO wrk_tipdoc
     STORE SPACE(10) TO  ;
           wrk_numdoc
     @ 00, 01 SAY  ;
       'Tipo De Doc. :'
     @ 00, 48 SAY  ;
       'Nro. Doc.    :'
     @ 00, 16 GET wrk_tipdoc  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 00, 63 GET wrk_numdoc  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
ENDDO
DEACTIVATE WINDOW cabe, totales,  ;
           tablas
RELEASE WINDOW cabe, totales
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE oowhen
PARAMETER opc
DO CASE
     CASE opc = 'WRK_TIPDOC'
     CASE opc = 'WRK_NUMDOC'
          ON KEY LABEL F6 DO ooverhip
ENDCASE
*
FUNCTION oovalid
PARAMETER opc
DO CASE
     CASE opc = 'WRK_TIPDOC'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(wrk_tipdoc)
               DO p_mensaje WITH  ;
                  'No se Permiten Blancos'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' +  ;
               wrk_tipdoc
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'Tipo de Documento No Existe'
               RETURN .F.
          ENDIF
          @ 00, 21 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     CASE opc = 'WRK_NUMDOC'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(wrk_numdoc)
               DO p_mensaje WITH  ;
                  'No se Permiten Blancos'
               RETURN .F.
          ENDIF
          wrk_numdoc = f_ceros(wrk_numdoc, ;
                       10,2)
          @ 00, 63 SAY wrk_numdoc  ;
            PICTURE '@!' COLOR N/ ;
            W 
          SELECT gc_hip00
          SEEK wrk_tipdoc +  ;
               wrk_numdoc
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'Tipo de Documento No Existe'
               RETURN .F.
          ENDIF
          ON KEY
          DO proceso
ENDCASE
*
PROCEDURE proceso
@ 01, 01 SAY 'Fecha        : ' +  ;
  DTOC(hip_fecdoc)
@ 02, 01 SAY 'Cod. Movim.  : ' +  ;
  hip_codmov + '       ' +  ;
  ootab('RECE',hip_codmov)
@ 03, 01 SAY 'Alm. Recep.  : ' +  ;
  hip_almrec + '       ' +  ;
  ootab('ALMA',hip_almrec)
@ 04, 01 SAY 'Proveedor    : ' +  ;
  hip_codent + '  ' +  ;
  oodespve(hip_codent)
@ 05, 01 SAY 'Moneda       : ' +  ;
  hip_mondoc + '       ' +  ;
  ootab('MONE',hip_mondoc)
@ 06, 01 SAY 'Tipo Doc. Ref: ' +  ;
  hip_tidore
@ 06, 48 SAY 'Nro. Doc. Ref: ' +  ;
  hip_nrdore
ACTIVATE WINDOW totales
@ 00, 00 SAY  ;
  '   TOTAL NETOÄÄÄÄÄTOT. IGVÄÄÄTOT.IMPORTEÄÄÄTOT. DSCTO.ÄÄTOTAL GENERAL       '  ;
  COLOR SCHEME 7
@ 01, 02 SAY hip_totnet PICTURE  ;
  '999,999.99'
@ 01, 15 SAY hip_totigv PICTURE  ;
  '999,999.99'
@ 01, 29 SAY hip_totoim PICTURE  ;
  '999,999.99'
@ 01, 43 SAY hip_totdes PICTURE  ;
  '999,999.99'
@ 01, 58 SAY hip_totgen PICTURE  ;
  '999,999.99'
SELECT gc_dip00
SEEK wrk_tipdoc + wrk_numdoc
wrk_file = SUBSTR(SYS(3), 1, 8) +  ;
           '.DBF'
SELECT 8
CREA TABLE &wrk_file (codigo C(14), descri;
C(30), cantid N(6), import N(7,2), total;
N(7,2))
SELECT gc_dip00
DO WHILE dip_tipdoc=wrk_tipdoc  ;
   .AND. dip_nrodoc=wrk_numdoc  ;
   .AND.  .NOT. EOF()
     wrk_codpro = dip_propar
     wrk_cantid = dip_cantid
     wrk_import = dip_import
     wrk_total = dip_total
     wrk_descri = oodespro(dip_propar)
     SELECT 8
     APPEND BLANK
     REPLACE codigo WITH  ;
             wrk_codpro
     REPLACE descri WITH  ;
             wrk_descri
     REPLACE cantid WITH  ;
             wrk_cantid
     REPLACE import WITH  ;
             wrk_import
     REPLACE total WITH wrk_total
     SELECT gc_dip00
     SKIP
ENDDO
IF gc_hip00.hip_codmov = 'IFC'
ENDIF
SELECT 8
DO WHILE LASTKEY()<>27
     ACTIVATE POPUP detalle
ENDDO
SELECT 8
USE
DELE FILE &wrk_file
*
PROCEDURE gastos
ON KEY
SELECT 5
SEEK wrk_tipdoc + wrk_numdoc
SELECT ge_tab0.tab_destab,  ;
       gc_gas00.gas_valor FROM  ;
       GE_TAB0, GC_GAS00 WHERE  ;
       gc_gas00.gas_codgas =  ;
       ge_tab0.tab_codtab AND  ;
       gc_gas00.gas_tipdoc =  ;
       wrk_tipdoc AND  ;
       gc_gas00.gas_nrodoc =  ;
       wrk_numdoc AND  ;
       ge_tab0.tab_codpre =  ;
       'GAST' AND  ;
       ge_tab0.tab_codtab =  ;
       gc_gas00.gas_codgas INTO  ;
       CURSOR GAST
DO WHILE LASTKEY()<>-6
     ACTIVATE POPUP gastos
ENDDO
DEACTIVATE WINDOW gastos
SELECT 8
ON KEY
*
PROCEDURE ooverhip
SELECT gc_hip00
SET NEAR ON
SEEK wrk_tipdoc + wrk_numdoc
SET NEAR OFF
ACTIVATE WINDOW ayuda
ON KEY LABEL 'ENTER' DO TOMA
BROWSE FIELDS hip_tipdoc :H =  ;
       'TIPO', hip_nrodoc :H =  ;
       'NUMERO', hip_fecdoc :H =  ;
       'FECHA', hip_codmov :H =  ;
       'MOVIM.', hip_almrec :H =  ;
       'RECE', hip_tidore :H =  ;
       'D.REF', hip_nrdore :H =  ;
       'N.REFER.' NOEDIT
ON KEY
DEACTIVATE WINDOW ayuda
RETURN
*
PROCEDURE toma
ON KEY
wrk_tipdoc = hip_tipdoc
wrk_numdoc = hip_nrodoc
KEYBOARD '{ESC}'
KEYBOARD '{ENTER}'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

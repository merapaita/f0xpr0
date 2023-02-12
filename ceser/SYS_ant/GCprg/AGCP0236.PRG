*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 10, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 11, 02  ;
       TO 21, 77 NONE COLOR  ;
       SCHEME 7
SELECT 1
USE GC_INV00 ORDER CODIGO
SELECT 2
USE GC_KAR00 ORDER CODIGO
SELECT 3
USE GC_HIP00 ORDER CODIGO
SELECT 4
USE GC_DIP00 ORDER CODIGO
SELECT 5
USE GE_TAB0 ORDER CODIGO
SELECT 6
USE GC_ALM00 ORDER CODIGO
SELECT 7
USE GC_PRO00 ORDER CODIGO
ACTIVATE WINDOW tablas
DO p_prestab WITH 'PROCESO',  ;
   'ACTUALIZACION DE INVENTARIO',  ;
   'PROCESO'
@ 02, 63 SAY PROGRAM()
wrk_progra = PROGRAM()
DO WHILE .T.
     ON KEY
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     STORE SPACE(4) TO wrk_codalm,  ;
           wrk_tipdoc
     STORE SPACE(9) TO wrk_numdoc
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 01, 02 SAY  ;
       'Almac‚n      :'
     @ 02, 02 SAY  ;
       'Tipo de Doc. :'
     @ 03, 02 SAY  ;
       'Nro. de Doc. :'
     @ 01, 17 GET wrk_codalm  ;
       PICTURE '@!' VALID  ;
       despues(1) WHEN antes(1)  ;
       COLOR SCHEME 8
     @ 02, 17 GET wrk_tipdoc  ;
       PICTURE '@!' VALID  ;
       despues(2) WHEN antes(2)  ;
       COLOR SCHEME 8
     @ 03, 17 GET wrk_numdoc  ;
       PICTURE '@!' VALID  ;
       despues(3) WHEN antes(3)  ;
       COLOR SCHEME 8
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DEACTIVATE WINDOW  ;
                     tablas,  ;
                     codigo,  ;
                     footer
          RELEASE WINDOW cabecera,  ;
                  detalle,  ;
                  winmensaje
          DO p_footer WITH  ;
             '100000000001001110001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
ENDDO
RETURN
*
PROCEDURE antes
PARAMETER wrk_opc0
DO CASE
     CASE wrk_opc0 = 1
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_busca = 'ALMA'
          wrk_var = 'wrk_codalm'
          ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
     CASE wrk_opc0 = 2
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_busca = 'DOCU'
          wrk_var = 'wrk_tipdoc'
          ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
     CASE wrk_opc0 = 1
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          ON KEY
ENDCASE
RETURN
*
FUNCTION despues
PARAMETER wrk_opc
DO CASE
     CASE wrk_opc = 1
          IF LASTKEY() = 19 .OR.  ;
             LASTKEY() = 5
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SET ORDER TO CODIGO
          SEEK 'ALMA' +  ;
               wrk_codalm
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Almac‚n No Existe'
               RETURN .F.
          ENDIF
          @ 01, 22 SAY tab_destab
     CASE wrk_opc = 2
          IF LASTKEY() = 19 .OR.  ;
             LASTKEY() = 5
               RETURN .T.
          ENDIF
          SELECT ge_tab0
          SET ORDER TO CODIGO
          SEEK 'DOCU' +  ;
               wrk_tipdoc
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Almac‚n No Existe'
               RETURN .F.
          ENDIF
          @ 02, 22 SAY tab_destab
     CASE wrk_opc = 3
          IF LASTKEY() = 19 .OR.  ;
             LASTKEY() = 5
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_numdoc)
               DO p_mensaje WITH  ;
                  'No se Permiten Blancos'
               RETURN .F.
          ENDIF
          SELECT gc_hip00
          wrk_nrodoc = wrk_numdoc +  ;
                       '1'
          SEEK wrk_tipdoc +  ;
               wrk_nrodoc
          IF FOUND()
               DO p_mensaje WITH  ;
                  'Documento ya Existe'
               RETURN .F.
          ENDIF
          rpt = f_yesno( ;
                'CONFIRMA EL PROCESO ?' ;
                )
          IF  .NOT. rpt
               RETURN .F.
          ENDIF
          DO procesa
ENDCASE
RETURN
*
PROCEDURE procesa
= ooaviso( ;
  ' P R O C E S A N D O . . .')
wrk_file01 = SUBSTR(f_archivo(),  ;
             1, 8) + '.DBF'
SELECT DISTINCT GC_INV00.ALM_CODPRO, GC_INV00.ALM_CODALM,;
GC_INV00.ALM_STKFIS, GC_INV00.ALM_STKRES,;
GC_INV00.ALM_CONTEO, GC_INV00.ALM_COPRDO,;
GC_INV00.ALM_COPRSO FROM GC_INV00 WHERE;
GC_INV00.ALM_STKFIS-GC_INV00.ALM_STKRES>0;
AND GC_INV00.ALM_CODALM LIKE wrk_codalm;
INTO TABLE &wrk_file01
COUNT TO wrk_conta1
wrk_file02 = SUBSTR(f_archivo(),  ;
             1, 8) + '.DBF'
SELECT DISTINCT GC_INV00.ALM_CODPRO, GC_INV00.ALM_CODALM,;
GC_INV00.ALM_STKFIS, GC_INV00.ALM_STKRES,;
GC_INV00.ALM_CONTEO, GC_INV00.ALM_COPRDO,;
GC_INV00.ALM_COPRSO FROM GC_INV00 WHERE;
GC_INV00.ALM_STKFIS-GC_INV00.ALM_STKRES;
< 0 AND GC_INV00.ALM_CODALM LIKE wrk_codalm;
INTO TABLE &wrk_file02
COUNT TO wrk_conta2
IF wrk_conta1 = 0 .AND.  ;
   wrk_conta2 = 0
     DEACTIVATE WINDOW winmensaje
     DO p_mensaje WITH  ;
        'No Existen Registros a Procesar'
     RETURN
ENDIF
IF wrk_conta1 > 0
     SELECT gc_hip00
     APPEND BLANK
     DO rbloquea
     REPLACE hip_tipdoc WITH  ;
             wrk_tipdoc
     REPLACE hip_nrodoc WITH  ;
             wrk_numdoc + '1'
     REPLACE hip_fecdoc WITH  ;
             DATE()
     REPLACE hip_fecvct WITH  ;
             DATE()
     REPLACE hip_codmov WITH  ;
             'AFNE'
     REPLACE hip_almdes WITH  ;
             wrk_codalm
     REPLACE hip_estdoc WITH 'V'
     REPLACE hip_usuari WITH  ;
             clave
     REPLACE hip_fecha WITH  ;
             DATE()
     REPLACE hip_hora WITH TIME()
     UNLOCK
     SELECT 10
     GOTO TOP
     DO WHILE  .NOT. EOF()
          wrk_codpro = alm_codpro
          wrk_codalm = alm_codalm
          wrk_cantid = alm_conteo
          wrk_coprdo = alm_coprdo
          wrk_coprso = alm_coprso
          wrk_stkant = 0
          SELECT gc_alm00
          SEEK wrk_codpro +  ;
               wrk_codalm
          IF FOUND()
               wrk_stkant = alm_stkfis
               wrk_difinv = alm_stkfis -  ;
                            wrk_cantid
               DO rbloquea
               REPLACE alm_stkfis  ;
                       WITH  ;
                       alm_stkfis -  ;
                       wrk_difinv
               UNLOCK
          ENDIF
          SELECT gc_kar00
          APPEND BLANK
          DO rbloquea
          REPLACE kar_codpro WITH  ;
                  wrk_codpro
          REPLACE kar_fecing WITH  ;
                  DATE()
          REPLACE kar_horing WITH  ;
                  TIME()
          REPLACE kar_tipdoc WITH  ;
                  wrk_tipdoc
          REPLACE kar_nrodoc WITH  ;
                  wrk_numdoc +  ;
                  '1'
          REPLACE kar_fecdoc WITH  ;
                  DATE()
          REPLACE kar_codmov WITH  ;
                  'AFNE'
          REPLACE kar_unimed WITH  ;
                  oounipro(wrk_codpro)
          REPLACE kar_stkant WITH  ;
                  wrk_stkant
          REPLACE kar_import WITH  ;
                  oocosuni(wrk_codpro)
          REPLACE kar_cosant WITH  ;
                  wrk_coprdo
          REPLACE kar_cosanb WITH  ;
                  wrk_coprso
          REPLACE kar_cosuni WITH  ;
                  oocosuni(wrk_codpro)
          REPLACE kar_almdes WITH  ;
                  wrk_codalm
          REPLACE kar_cantid WITH  ;
                  wrk_difinv
          REPLACE kar_usuari WITH  ;
                  clave
          REPLACE kar_fecha WITH  ;
                  DATE()
          REPLACE kar_hora WITH  ;
                  TIME()
          UNLOCK
          SELECT gc_dip00
          APPEND BLANK
          DO rbloquea
          REPLACE dip_tipdoc WITH  ;
                  wrk_tipdoc
          REPLACE dip_nrodoc WITH  ;
                  wrk_numdoc +  ;
                  '1'
          REPLACE dip_propar WITH  ;
                  wrk_codpro
          REPLACE dip_unimed WITH  ;
                  oounipro(wrk_codpro)
          REPLACE dip_cantid WITH  ;
                  wrk_difinv
          REPLACE dip_import WITH  ;
                  oocosuni(wrk_codpro)
          REPLACE dip_usuari WITH  ;
                  clave
          REPLACE dip_fecha WITH  ;
                  DATE()
          REPLACE dip_hora WITH  ;
                  TIME()
          UNLOCK
          SELECT gc_pro00
          SEEK wrk_codpro
          IF FOUND()
               DO rbloquea
               REPLACE pro_ultoin  ;
                       WITH  ;
                       DATE()
               REPLACE pro_ultmov  ;
                       WITH  ;
                       DATE()
               UNLOCK
          ENDIF
          SELECT 10
          SKIP
     ENDDO
     SELECT 10
     USE
     ERASE FILE &wrk_file01
ENDIF
IF wrk_conta2 > 0
     SELECT gc_hip00
     APPEND BLANK
     DO rbloquea
     REPLACE hip_tipdoc WITH  ;
             wrk_tipdoc
     REPLACE hip_nrodoc WITH  ;
             wrk_numdoc + '2'
     REPLACE hip_fecdoc WITH  ;
             DATE()
     REPLACE hip_fecvct WITH  ;
             DATE()
     REPLACE hip_codmov WITH  ;
             'AFPO'
     REPLACE hip_almrec WITH  ;
             wrk_codalm
     REPLACE hip_estdoc WITH 'V'
     REPLACE hip_usuari WITH  ;
             clave
     REPLACE hip_fecha WITH  ;
             DATE()
     REPLACE hip_hora WITH TIME()
     UNLOCK
     SELECT 11
     GOTO TOP
     DO WHILE  .NOT. EOF()
          wrk_codpro = alm_codpro
          wrk_codalm = alm_codalm
          wrk_cantid = alm_conteo
          wrk_coprdo = alm_coprdo
          wrk_coprso = alm_coprso
          wrk_stkant = 0
          SELECT gc_alm00
          SEEK wrk_codpro +  ;
               wrk_codalm
          IF  .NOT. FOUND()
               APPEND BLANK
               DO rbloquea
               REPLACE alm_codpro  ;
                       WITH  ;
                       wrk_codpro
               REPLACE alm_codalm  ;
                       WITH  ;
                       wrk_codalm
               REPLACE alm_stkfis  ;
                       WITH 0
               REPLACE alm_usuari  ;
                       WITH  ;
                       clave
               REPLACE alm_fecha  ;
                       WITH  ;
                       DATE()
               REPLACE alm_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          wrk_stkant = alm_stkfis
          wrk_difinv = wrk_cantid -  ;
                       alm_stkfis
          DO rbloquea
          REPLACE alm_stkfis WITH  ;
                  alm_stkfis +  ;
                  wrk_difinv
          UNLOCK
          SELECT gc_kar00
          APPEND BLANK
          DO rbloquea
          REPLACE kar_codpro WITH  ;
                  wrk_codpro
          REPLACE kar_fecing WITH  ;
                  DATE()
          REPLACE kar_horing WITH  ;
                  TIME()
          REPLACE kar_tipdoc WITH  ;
                  wrk_tipdoc
          REPLACE kar_nrodoc WITH  ;
                  wrk_numdoc +  ;
                  '2'
          REPLACE kar_fecdoc WITH  ;
                  DATE()
          REPLACE kar_codmov WITH  ;
                  'AFPO'
          REPLACE kar_unimed WITH  ;
                  oounipro(wrk_codpro)
          REPLACE kar_stkant WITH  ;
                  wrk_stkant
          REPLACE kar_import WITH  ;
                  oocosuni(wrk_codpro)
          REPLACE kar_cosant WITH  ;
                  wrk_coprdo
          REPLACE kar_cosanb WITH  ;
                  wrk_coprso
          REPLACE kar_cosuni WITH  ;
                  oocosuni(wrk_codpro)
          REPLACE kar_almrec WITH  ;
                  wrk_codalm
          REPLACE kar_cantid WITH  ;
                  wrk_difinv
          REPLACE kar_usuari WITH  ;
                  clave
          REPLACE kar_fecha WITH  ;
                  DATE()
          REPLACE kar_hora WITH  ;
                  TIME()
          UNLOCK
          SELECT gc_dip00
          APPEND BLANK
          DO rbloquea
          REPLACE dip_tipdoc WITH  ;
                  wrk_tipdoc
          REPLACE dip_nrodoc WITH  ;
                  wrk_numdoc +  ;
                  '2'
          REPLACE dip_propar WITH  ;
                  wrk_codpro
          REPLACE dip_unimed WITH  ;
                  oounipro(wrk_codpro)
          REPLACE dip_cantid WITH  ;
                  wrk_difinv
          REPLACE dip_usuari WITH  ;
                  clave
          REPLACE dip_fecha WITH  ;
                  DATE()
          REPLACE dip_hora WITH  ;
                  TIME()
          UNLOCK
          SELECT gc_pro00
          SEEK wrk_codpro
          IF FOUND()
               DO rbloquea
               REPLACE pro_ultoin  ;
                       WITH  ;
                       DATE()
               REPLACE pro_ultmov  ;
                       WITH  ;
                       DATE()
               UNLOCK
          ENDIF
          SELECT 11
          SKIP
     ENDDO
     SELECT 11
     USE
     ERASE FILE &wrk_file02
ENDIF
DEACTIVATE WINDOW winmensaje
DO p_mensaje WITH  ;
   ' FIN DE PROCESO '
*
*** 
*** ReFox - retrace your steps ... 
***

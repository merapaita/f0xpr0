*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
SET CENTURY ON
CLEAR
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW produ FROM 12, 02  ;
       TO 20, 76 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW gas FROM 19, 26 TO  ;
       21, 54 IN screen NONE
DEFINE WINDOW t0 FROM 02, 64 TO  ;
       02, 73 IN screen NONE
DEFINE WINDOW a0 FROM 04, 01 TO  ;
       12, 78 IN screen
DEFINE WINDOW detalle FROM 12, 00  ;
       TO 19, 79 IN screen COLOR  ;
       SCHEME 10
ACTIVATE WINDOW tablas
flag = USED('gc_par00')
DO p_prestab WITH 'PROCESO',  ;
   'DESPACHO DE PRODUCTOS',  ;
   'SELECCION', flag
@ 02, 63 SAY PROGRAM()
CLOSE DATABASES
SELECT 1
USE SHARED gc_hip00 ORDER codigo
SELECT 2
USE SHARED gc_dip00 ORDER codigo
SELECT 3
USE SHARED ge_tab0 ALIAS ge_tab0  ;
    ORDER codigo
w_sele1 = SELECT()
SELECT 4
USE SHARED gc_gas00 ALIAS  ;
    gc_gas00 ORDER codigo
SELECT 5
USE SHARED gc_kar00 ORDER docpro
SELECT 6
USE SHARED gc_alm00 ORDER codigo
SELECT 7
USE SHARED gc_pro00 ORDER codigo
w_selpro = SELECT()
SELECT 9
USE SHARED gc_cli00 ORDER codigo
SELECT 10
USE SHARED gc_vnd00 ORDER codigo
SELECT 11
USE SHARED gc_dlp00 ORDER codigo
STORE 0 TO w_swf, tcp, w_tecla
STORE SPACE(04) TO w_var, w_busca,  ;
      w_mone
STORE 0 TO totimpor, tcn,  ;
      w_tipcav, c
SET CURSOR ON
SELECT 20
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
SEEK DTOS(DATE()) + '1' +  ;
     rge_monbas + 'DOL '
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        '**No Existe Tipo de Cambio para esta Fecha**'
     w_swf = 1
ELSE
     w_tipcav = cmv_tipcav
ENDIF
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     w_facigv = ROUND(tab_factor /  ;
                100, 2)
ELSE
     DO p_mensaje WITH  ;
        '** No Esta Definido el '+ ;
        sys_codimp
     w_swf = 1
ENDIF
w_fin = 0
DO WHILE w_swf<>1
     IF w_fin = 0
          DO pinta1
          DO pinta2
          STORE 0 TO tmp_1, w_sw3,  ;
                w_totcan,  ;
                w_tottem,  ;
                w_totpre,  ;
                w_almant, w_lin,  ;
                w_gas, w_stkant,  ;
                w_bus2, w_bus1,  ;
                w_count, p, m,  ;
                w_opcion,  ;
                w_tecla
          STORE 0 TO w_impor,  ;
                timpor, w_tfob,  ;
                tflete, tseguro,  ;
                tfob, tcif, tadva,  ;
                tgastos, fflete,  ;
                fseguro, fadv,  ;
                fgastos, fadva,  ;
                w_proant,  ;
                w_proanb, tcn,  ;
                w_exi, w_fin
          STORE 1 TO w_swfin
          STORE SPACE(01) TO  ;
                w_tiprov,  ;
                w_razsoc,  ;
                w_tipmon, w_tipo,  ;
                w_ref
          STORE SPACE(30) TO  ;
                w_desref,  ;
                w_desmov,  ;
                w_desalm,  ;
                w_desmon,  ;
                w_desent,  ;
                w_desdoc, w_direc,  ;
                w_desttra,  ;
                w_trans, w_despro,  ;
                w_dirtra
          STORE SPACE(14) TO  ;
                w_codexi
          STORE SPACE(4) TO  ;
                w_tdoc, w_ructra
          SCATTER BLANK TO cod
          SET CURSOR ON
          SELECT gc_hip00
          DIMENSION des(  ;
                    FCOUNT())
          SCATTER BLANK TO des
          des( 1) = 'GUIA'
          w_tdoc = 'GUIA'
          des( 11) = 'DOL '
          des( 13) = SPACE(4)
          w_alma = SPACE(4)
          des( 14) = SPACE(10)
          des( 9) = 'A   '
          des( 31) = 'T   '
     ENDIF
     ACTIVATE WINDOW t0
     @ 00, 0 SAY 'SELECCION'
     DO proceso
ENDDO
DEACTIVATE WINDOW detalle, tablas,  ;
           titpass, a0, titposs
RELEASE WINDOW t0, a0, a1, c0, a2
RELEASE WINDOW winmensaje
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE proceso
ACTIVATE WINDOW a0
@ 00, 67 SAY w_tipcav PICTURE  ;
  '9,999.99'
@ 00, 13 GET des( 1) PICTURE '@!'  ;
  VALID despues(VARREAD()) WHEN  ;
  antes(VARREAD()) COLOR W/N,N/W 
@ 00, 50 GET des( 2) PICTURE '@!'  ;
  VALID despues(VARREAD()) WHEN  ;
  antes(VARREAD()) COLOR W/N,N/W 
@ 01, 13 GET des( 3) PICTURE  ;
  '99/99/9999' VALID  ;
  despues(VARREAD()) WHEN  ;
  antes(VARREAD()) COLOR W/N,N/W 
@ 01, 50 GET des( 4) PICTURE  ;
  '99/99/9999' VALID  ;
  despues(VARREAD()) WHEN  ;
  antes(VARREAD()) COLOR W/N,N/W 
@ 02, 13 GET des( 5) PICTURE '@!'  ;
  VALID despues(VARREAD()) WHEN  ;
  antes(VARREAD()) COLOR W/N,N/W 
@ 02, 50 GET des( 30) PICTURE  ;
  '@!' VALID despues(VARREAD())  ;
  WHEN antes(VARREAD()) COLOR W/N, ;
  N/W 
w_alma = des(7)
@ 03, 13 GET des( 7) PICTURE '@!'  ;
  VALID despues(VARREAD()) WHEN  ;
  antes(VARREAD()) COLOR W/N,N/W 
w_mone = des(11)
@ 03, 50 GET des( 11) PICTURE  ;
  '@!' VALID despues(VARREAD())  ;
  WHEN antes(VARREAD()) COLOR W/N, ;
  N/W 
@ 04, 13 GET des( 9) PICTURE '@!'  ;
  VALID despues(VARREAD()) WHEN  ;
  antes(VARREAD()) COLOR W/N,N/W 
@ 04, 50 GET des( 10) PICTURE  ;
  '@!' VALID despues(VARREAD())  ;
  WHEN antes(VARREAD()) COLOR W/N, ;
  N/W 
@ 05, 13 GET des( 31) PICTURE  ;
  '@!' VALID despues(VARREAD())  ;
  WHEN antes(VARREAD()) COLOR W/N, ;
  N/W 
@ 05, 50 GET des( 29) PICTURE  ;
  '@!' VALID despues(VARREAD())  ;
  WHEN antes(VARREAD()) COLOR W/N, ;
  N/W 
w_tipo = des(13)
w_ref = des(14)
@ 06, 13 GET des( 13) PICTURE  ;
  '@!' VALID despues(VARREAD())  ;
  WHEN antes(VARREAD()) COLOR W/N, ;
  N/W 
@ 06, 50 GET des( 14) PICTURE  ;
  '@!' VALID despues(VARREAD())  ;
  WHEN antes(VARREAD()) COLOR W/N, ;
  N/W 
READ CYCLE
IF LASTKEY() = 27 .AND. w_var =  ;
   'des(1)'
     CLEAR READ
     w_swf = 1
ENDIF
RETURN
*
PROCEDURE antes
PARAMETER var
DO CASE
     CASE var = 'DES(1)'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          w_busca = 'DOCU'
          w_var = 'des(1)'
          ON KEY LABEL F6 do busca with;
w_busca,w_var
     CASE var = 'DES(2)'
          ON KEY
          w_var = 'des(2)'
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW a0
     CASE var = 'DES(3)'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          w_var = 'des(3)'
          IF w_sw3 = 0
               des( 3) = DATE()
          ENDIF
     CASE var = 'DES(4)'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          w_var = 'des(4)'
          IF w_sw3 = 0
               des( 4) = DATE()
          ENDIF
     CASE var = 'DES(5)'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          w_busca = 'DESP'
          w_var = 'des(5)'
          ON KEY LABEL F6 do busca with;
w_busca,w_var
     CASE var = 'DES(7)'
          ON KEY
          w_busca = 'ALMA'
          w_var = 'des(7)'
          ON KEY LABEL F6 do busca with;
w_busca,w_var
     CASE var = 'DES(9)'
          ON KEY
          w_busca = 'ENTI'
          w_var = 'des(9)'
          ON KEY LABEL F6 do busca with;
w_busca,w_var
     CASE var = 'DES(10)'
          ON KEY
          w_busca = des(9)
          w_var = 'des(10)'
          ON KEY LABEL F6 do entidad with;
w_busca,w_var
     CASE var = 'DES(11)'
          ON KEY
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          w_busca = 'MONE'
          w_var = 'des(11)'
          ON KEY LABEL F6 do busca with;
w_busca,w_var
     CASE var = 'DES(12)'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          w_var = 'des(12)'
          IF w_sw3 = 0
               des( 12) = DATE()
          ENDIF
     CASE var = 'DES(13)'
          ON KEY
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          w_busca = 'DOCU'
          w_var = 'des(13)'
          ON KEY LABEL F6 do busca with;
w_busca,w_var
     CASE var = 'DES(14)'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY
          w_var = 'des(14)'
          ON KEY LABEL F6 do ooverdoc;

          ACTIVATE WINDOW a0
     CASE var = 'DES(29)'
          ON KEY
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          w_busca = des(31)
          w_var = 'des(29)'
          ON KEY LABEL F6 do entidad with;
w_busca,w_var
     CASE var = 'DES(30)'
          ON KEY
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          w_busca = 'MTRA'
          w_var = 'des(30)'
          ON KEY LABEL F6 do busca with;
w_busca,w_var
     CASE var = 'DES(31)'
          ON KEY
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          w_busca = 'ENTI'
          w_var = 'des(31)'
          ON KEY LABEL F6 do busca with;
w_busca,w_var
ENDCASE
RETURN
*
FUNCTION despues
PARAMETER var
DO CASE
     CASE var = 'DES(1)'
          IF des(1) = 'AJUS' .OR.  ;
             des(1) = 'ABON' .OR.  ;
             des(1) = 'NCRE' .OR.  ;
             des(1) = 'NDEB' .OR.  ;
             des(1) = 'ORDE' .OR.  ;
             des(1) = 'RCLR' .OR.  ;
             des(1) = 'USOT'
               DO p_mensaje WITH  ;
                  'Tipo de Documento no Permitido'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' + des(1)
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'Tipo de Documento no Existe'
               RETURN .F.
          ELSE
               w_desdoc = SUBSTR(tab_destab,  ;
                          1, 30)
               @ 00, 18 SAY  ;
                 SUBSTR(w_desdoc,  ;
                 1, 18)
          ENDIF
          w_tdoc = des(1)
     CASE var = 'DES(2)'
          des( 2) =  ;
             f_ceros(des(2),10, ;
             2)
          @ 00, 50 SAY des(2)
          IF des(2) =  ;
             '0000000000'
               DO p_mensaje WITH  ;
                  'Documento en Blanco'
               RETURN .F.
          ENDIF
          SELECT gc_hip00
          SEEK des(1) + des(2)
          IF FOUND()
               w_sw3 = 1
               DO p_mensaje WITH  ;
                  'Documento ya fue Ingresado'
               RETURN .F.
          ELSE
               w_sw3 = 0
          ENDIF
     CASE var = 'DES(3)'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN
          ENDIF
     CASE var = 'DES(4)'
          IF des(4) < des(3)
               DO p_mensaje WITH  ;
                  ' Fecha de Vcto. no puede ser menor a la Fecha de Recepci¢n.'
               RETURN .F.
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN
          ENDIF
     CASE var = 'DES(5)'
          SELECT ge_tab0
          SEEK w_busca + des(5)
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'Tipo de Movimiento no Existe'
               RETURN .F.
          ELSE
               w_desmov = SUBSTR(tab_destab,  ;
                          1, 30)
               @ 02, 18 SAY  ;
                 SUBSTR(w_desmov,  ;
                 1, 18)
          ENDIF
     CASE var = 'DES(7)'
          IF w_tecla = 5 .AND.  ;
             w_fin = 1 .AND.  ;
             w_alma <> des(7)
               DO p_mensaje WITH  ;
                  ' Si modifica cabecera,no puede modificar almac‚n '
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'ALMA' + des(7)
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'Almac‚n no Existe'
               RETURN .F.
          ELSE
               w_desalm = SUBSTR(tab_destab,  ;
                          1, 30)
               @ 03, 18 SAY  ;
                 SUBSTR(w_desalm,  ;
                 1, 18)
          ENDIF
     CASE var = 'DES(9)'
          SELECT ge_tab0
          SEEK 'ENTI' + des(9)
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'No Existe'
               RETURN .F.
          ELSE
               w_desent = SUBSTR(tab_destab,  ;
                          1, 30)
               @ 04, 18 SAY  ;
                 SUBSTR(w_desent,  ;
                 1, 18)
          ENDIF
          IF des(9) = 'P' .OR.  ;
             des(9) = 'C' .OR.  ;
             des(9) = 'I'
               des( 10) =  ;
                  SPACE(11)
          ELSE
               des( 10) =  ;
                  SPACE(5)
          ENDIF
     CASE var = 'DES(10)'
          IF des(9) = 'P' .OR.  ;
             des(9) = 'C' .OR.  ;
             des(9) = 'I'
               SELECT gc_cli00
               SET ORDER TO codigo
               SEEK ALLTRIM(des(9)) +  ;
                    des(10)
          ELSE
               SELECT gc_vnd00
               SET ORDER TO codigo
               SEEK ALLTRIM(des(9)) +  ;
                    SUBSTR(des(10),  ;
                    1, 5)
          ENDIF
          IF  .NOT. FOUND()
               IF des(9) = 'P'  ;
                  .OR. des(9) =  ;
                  'C' .OR. des(9) =  ;
                  'I'
                    DO p_mensaje  ;
                       WITH  ;
                       'Proveedor o Cliente no Existe'
                    RETURN .F.
               ELSE
                    DO p_mensaje  ;
                       WITH  ;
                       'Administrativo o Vendedor no Existe'
                    RETURN .F.
               ENDIF
          ELSE
               IF des(9) = 'P'  ;
                  .OR. des(9) =  ;
                  'C' .OR. des(9) =  ;
                  'I'
                    w_razsoc = SUBSTR(cli_razsoc,  ;
                               1,  ;
                               30)
                    w_direc = cli_calle +  ;
                              cli_numero
                    @ 04, 60 SAY  ;
                      SUBSTR(cli_razsoc,  ;
                      1, 15)
                    IF des(9) =  ;
                       'P'
                         w_tiprov =  ;
                          cli_tipo
                    ELSE
                         w_tiprov =  ;
                          SPACE(04)
                    ENDIF
               ELSE
                    w_razsoc = SUBSTR(vnd_nombre,  ;
                               1,  ;
                               30)
                    w_direc = vnd_direc
                    @ 04, 60 SAY  ;
                      SUBSTR(vnd_nombre,  ;
                      1, 15)
               ENDIF
          ENDIF
     CASE var = 'DES(11)'
          IF w_mone <> des(11)  ;
             .AND. w_tecla = 5  ;
             .AND. w_fin = 1
               DO p_mensaje WITH  ;
                  ' Si modifica cabecera,no puede modificar moneda '
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'MONE' + des(11)
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'Moneda no Existe'
               RETURN .F.
          ELSE
               w_desmon = SUBSTR(tab_destab,  ;
                          1, 30)
               @ 03, 55 SAY  ;
                 SUBSTR(w_desmon,  ;
                 1, 15)
               IF des(11) =  ;
                  'DOL '
                    w_tipmon = sys_simomb
               ELSE
                    w_tipmon = sys_simmon
               ENDIF
          ENDIF
     CASE var = 'DES(12)'
          SELECT 20
          USE SHARED gc_cmv00  ;
              ORDER codigo_2
          SEEK DTOC(des(12)) +  ;
               '1' + rge_monbas +  ;
               'DOL '
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  '**No Existe Tipo de Cambio para esta Fecha**'
               RETURN .F.
          ELSE
               w_tipcav = cmv_tipcav
          ENDIF
     CASE var = 'DES(29)'
          IF des(31) = 'P' .OR.  ;
             des(31) = 'C' .OR.  ;
             des(31) = 'I'
               SELECT gc_cli00
               SET ORDER TO codigo
               SEEK ALLTRIM(des(31)) +  ;
                    des(29)
          ELSE
               SELECT gc_vnd00
               SET ORDER TO codigo
               SEEK ALLTRIM(des(31)) +  ;
                    SUBSTR(des(29),  ;
                    1, 5)
          ENDIF
          IF  .NOT. FOUND()
               IF des(31) = 'P'  ;
                  .OR. des(31) =  ;
                  'C' .OR.  ;
                  des(31) = 'I'
                    DO p_mensaje  ;
                       WITH  ;
                       'Proveedor o Cliente no Existe'
                    RETURN .F.
               ELSE
                    DO p_mensaje  ;
                       WITH  ;
                       'Administrativo o Vendedor no Existe'
                    RETURN .F.
               ENDIF
          ELSE
               IF des(31) = 'P'  ;
                  .OR. des(31) =  ;
                  'C' .OR.  ;
                  des(31) = 'I'
                    w_trans = SUBSTR(cli_razsoc,  ;
                              1,  ;
                              30)
                    @ 05, 60 SAY  ;
                      SUBSTR(cli_razsoc,  ;
                      1, 15)
                    IF des(31) =  ;
                       'P'
                         w_tiprov =  ;
                          cli_tipo
                    ELSE
                         w_tiprov =  ;
                          SPACE(04)
                    ENDIF
               ELSE
                    w_trans = SUBSTR(vnd_nombre,  ;
                              1,  ;
                              30)
                    @ 05, 60 SAY  ;
                      SUBSTR(vnd_nombre,  ;
                      1, 15)
                    w_dirtra = vnd_direc
                    w_ructra = SUBSTR(vnd_observ,  ;
                               1,  ;
                               8)
               ENDIF
          ENDIF
     CASE var = 'DES(30)'
          SELECT ge_tab0
          SEEK 'MTRA' + des(30)
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'Motivo de traslado no Existe'
               RETURN .F.
          ELSE
               w_desmtra = SUBSTR(tab_destab,  ;
                           1,  ;
                           30)
               @ 02, 55 SAY  ;
                 SUBSTR(w_desmtra,  ;
                 1, 20)
          ENDIF
     CASE var = 'DES(31)'
          SELECT ge_tab0
          SEEK 'ENTI' + des(31)
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'No Existe'
               RETURN .F.
          ELSE
               w_desttra = SUBSTR(tab_destab,  ;
                           1,  ;
                           30)
               @ 05, 18 SAY  ;
                 SUBSTR(w_desttra,  ;
                 1, 18)
          ENDIF
          IF des(31) = 'P' .OR.  ;
             des(31) = 'C' .OR.  ;
             des(31) = 'I'
               des( 29) =  ;
                  SPACE(9)
          ELSE
               des( 29) =  ;
                  SPACE(5)
          ENDIF
     CASE var = 'DES(13)'
          IF w_tecla = 5 .AND.  ;
             w_fin = 1 .AND.  ;
             w_tipo <> des(13)  ;
             .AND. w_tipo =  ;
             'PLAN'
               DO p_mensaje WITH  ;
                  ' Si modifica cabecera,no puede modificar tipo de Doc.Ref. '
               RETURN .F.
          ENDIF
          @ 06, 18 SAY SPACE(18)
          IF EMPTY(des(13))
               RETURN
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' + des(13)
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'Tipo de Documento no Existe'
               RETURN .F.
          ELSE
               IF des(13) =  ;
                  'PLAN' .AND.  ;
                  des(5) <>  ;
                  'EVSU'
                    DO p_mensaje  ;
                       WITH  ;
                       'Tipo de Movimiento no Permitido para '+ ;
                       des(13)
                    RETURN .F.
               ELSE
                    w_desref = SUBSTR(tab_destab,  ;
                               1,  ;
                               30)
                    @ 06, 18 SAY  ;
                      SUBSTR(w_desref,  ;
                      1, 18)
               ENDIF
          ENDIF
     CASE var = 'DES(14)'
          IF w_tecla = 5 .AND.  ;
             w_fin = 1 .AND.  ;
             w_tipo <> des(13)  ;
             .AND. w_tipo =  ;
             'PLAN'
               DO p_mensaje WITH  ;
                  ' Si modifica cabecera,no puede modificar Doc.Ref. '
               RETURN .F.
          ENDIF
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN
          ENDIF
          IF des(13) = 'PLAN'  ;
             .AND.  ;
             EMPTY(des(14))
               DO p_mensaje WITH  ;
                  'No Puede Estar en Blanco N§ Doc Ref.'
               RETURN .F.
          ENDIF
          IF des(14) <> SPACE(10)
               des( 14) =  ;
                  f_ceros(des(14), ;
                  10,2)
               @ 06, 50 SAY  ;
                 des(14) COLOR N/ ;
                 W 
          ENDIF
          IF des(13) = 'PLAN'
               SELECT 20
               USE SHARED  ;
                   gc_hre00 ORDER  ;
                   hre_codtip
               SEEK des(7) +  ;
                    des(13) +  ;
                    des(14)
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'No existe Documento de Referencia'
                    RETURN .F.
               ELSE
                    IF hre_indest <>  ;
                       'V'
                         DO p_mensaje  ;
                            WITH  ;
                            'Documento de Referencia no Esta Vigente '
                         RETURN .F.
                    ELSE
                         IF w_fin =  ;
                            0  ;
                            .AND.  ;
                            w_tecla <>  ;
                            5
                              DO carmat
                              IF m =  ;
                                 0
                                   DO p_mensaje WITH 'Documento de Referencia no tiene Productos Confirmados'
                                   RETURN .F.
                              ENDIF
                         ELSE
                              SELECT  ;
                               despa
                              GOTO  ;
                               TOP
                              SCAN  ;
                               WHILE   ;
                               .NOT.  ;
                               EOF()
                                   SELECT gc_alm00
                                   SEEK despa.codigo + des(7)
                                   IF FOUND()
                                        DO rbloquea
                                        REPLACE alm_stkres WITH alm_stkres + despa.cantidad
                                        REPLACE alm_usuari WITH clave, alm_fecha WITH DATE(), alm_hora WITH TIME()
                                        UNLOCK
                                   ENDIF
                                   SELECT despa
                              ENDSCAN
                         ENDIF
                    ENDIF
               ENDIF
          ELSE
               IF w_fin = 0 .AND.  ;
                  w_tecla <> 5
                    DO carque
               ELSE
                    SELECT despa
                    GOTO TOP
                    SCAN WHILE   ;
                         .NOT.  ;
                         EOF()
                         SELECT gc_alm00
                         SEEK despa.codigo +  ;
                              des(7)
                         IF FOUND()
                              DO rbloquea
                              REPLACE  ;
                               alm_stkres  ;
                               WITH  ;
                               alm_stkres +  ;
                               despa.cantidad
                              REPLACE  ;
                               alm_usuari  ;
                               WITH  ;
                               clave,  ;
                               alm_fecha  ;
                               WITH  ;
                               DATE(),  ;
                               alm_hora  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                         ENDIF
                         SELECT despa
                    ENDSCAN
               ENDIF
          ENDIF
          IF LASTKEY() = 27
               DO limpia
               tmp_1 = 0
               ACTIVATE WINDOW a0
               ON KEY
          ELSE
               DO detalle
          ENDIF
          IF LASTKEY() = -1 .OR.  ;
             w_swfin = 0
               CLEAR READ
          ENDIF
ENDCASE
RETURN
*
PROCEDURE detalle
ON KEY
DEFINE WINDOW pidpass FROM 19, 01  ;
       TO 21, 78 IN screen NONE  ;
       COLOR SCHEME 20
DO WHILE LASTKEY()<>27 .AND.  ;
   LASTKEY()<>-1
     DO cartec
     w_swfin = 1
     DO WHILE w_swfin=1
          w_tecla = 0
          SELECT despa
          ACTIVATE WINDOW detalle
          BROWSE FIELDS codigo :  ;
                 14 :R :H =  ;
                 'C¢digo  ',  ;
                 descri : 15 :R  ;
                 :H =  ;
                 'Descripci¢n',  ;
                 docref : 10 :R  ;
                 :H = 'Doc. Ref.',  ;
                 cantidad : 7 :R  ;
                 :H = 'Cantid.',  ;
                 tempre :R :P =  ;
                 '9,999,999.99'  ;
                 :H =  ;
                 'Precio Unit.',  ;
                 total :R :P =  ;
                 '99,999,999.99'  ;
                 :H = '  Total  '  ;
                 NOEDIT NOCLEAR  ;
                 SAVE IN detalle
          IF LASTKEY() = 27
               IF w_tecla = 5
                    w_fin = 1
               ELSE
                    w_fin = 0
                    DEACTIVATE WINDOW  ;
                               detalle
               ENDIF
               DO limpia
               w_swfin = 0
          ENDIF
     ENDDO
ENDDO
tmp_1 = 0
ACTIVATE WINDOW a0
ON KEY
RETURN
*
PROCEDURE modifica
w_fin = 1
w_tecla = 5
KEYBOARD '{esc}'
RETURN
*
PROCEDURE p_lee
PARAMETER w_opcion
IF w_opcion = 1
     SELECT despa
     GOTO TOP
     COUNT TO ncon
     IF ncon >= 16 .AND. des(1) =  ;
        'GUIA'
          DO p_mensaje WITH  ;
             ' No Puede Ingresar m s de 16 Item '
          RETURN
     ENDIF
ENDIF
ON KEY
SET CURSOR ON
swt = .T.
STORE 0 TO tmp_fin, tmp_1
STORE SPACE(10) TO w_docref
IF (m = 0 .AND. w_opcion = 2)
     DO cartec
     RETURN
ENDIF
w_reg = RECNO()
DO WHILE swt .AND. (LASTKEY()=13  ;
   .OR. LASTKEY()=-2)
     ACTIVATE WINDOW pidpass
     @ 00, 00 SAY  ;
       'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
     @ 01, 00 SAY  ;
       '³              ³               ³          ³       ³            ³             ³'
     @ 02, 00 SAY  ;
       'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
     SELECT despa
     DIMENSION pro( FCOUNT())
     IF w_opcion = 1
          SCATTER BLANK TO pro
          w_docref = SPACE(10)
          w_cantid = 0
     ELSE
          SCATTER TO pro
          w_docref = pro(4)
          w_cantid = pro(5)
          @ 01, 01 SAY pro(1)  ;
            PICTURE '@!'
          @ 01, 16 SAY pro(3)  ;
            PICTURE '@!'
          @ 01, 32 SAY pro(4)  ;
            PICTURE '@!'
          @ 01, 43 SAY pro(5)  ;
            PICTURE '999,999'
          @ 01, 51 SAY pro(6)  ;
            PICTURE  ;
            '9,999,999.99'
          @ 01, 64 SAY pro(8)  ;
            PICTURE  ;
            '99,999,999.99'
     ENDIF
     tmp_err = 1
     DO WHILE tmp_1=0
          IF w_opcion = 1
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        pidpass
               w_selec = SELECT()
               w_campo = 'pro(1)'
               w_codalm = des(7)
               ON KEY LABEL f6 do produc2;
with w_campo,w_selec,w_codalm 
               @ 01, 01 GET pro(  ;
                 1) PICTURE '@!'
               READ
               IF LASTKEY() = 27
                    tmp_1 = 1
                    EXIT
               ENDIF
               SELECT gc_pro00
               SET ORDER TO codigo
               SEEK pro(1)
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'Producto No Existe'
                    LOOP
               ELSE
                    IF pro_estope =  ;
                       'B'
                         DO p_mensaje  ;
                            WITH  ;
                            'Producto Esta Bloqueado'
                         LOOP
                    ENDIF
                    pro( 3) =  ;
                       SUBSTR(pro_descri,  ;
                       1, 15)
                    w_despro = pro_descri
                    pro( 2) =  ;
                       pro_unimed
                    SELECT gc_dlp00
                    SEEK rge_lispre +  ;
                         pro(01)
                    IF FOUND()
                         IF des(11) =  ;
                            'DOL '
                              pro(  ;
                                 6) =  ;
                                 dlp_prcigv
                         ELSE
                              pro(  ;
                                 6) =  ;
                                 ROUND(dlp_prcigv *  ;
                                 w_tipcav,  ;
                                 2)
                         ENDIF
                    ENDIF
                    @ 01, 16 SAY  ;
                      pro(3)
                    @ 01, 51 SAY  ;
                      pro(6)  ;
                      PICTURE  ;
                      '9,999,999.99'
               ENDIF
          ENDIF
          DO WHILE tmp_1=0
               @ 01, 32 GET pro(  ;
                 4) PICTURE '@!'
               READ
               IF LASTKEY() = 27
                    tmp_1 = 1
                    EXIT
               ENDIF
               n = 0
               SELECT despa
               COUNT FOR pro(1) =  ;
                     codigo .AND.  ;
                     pro(4) =  ;
                     docref TO n
               IF w_opcion = 1
                    IF n = 1
                         DO p_mensaje  ;
                            WITH  ;
                            'El Producto ya Existe'
                         LOOP
                    ENDIF
               ELSE
                    IF n = 1  ;
                       .AND.  ;
                       w_docref <>  ;
                       pro(4)
                         DO p_mensaje  ;
                            WITH  ;
                            'El Producto ya Existe'
                         LOOP
                    ENDIF
               ENDIF
               DO WHILE tmp_1=0
                    DO p_footer  ;
                       WITH  ;
                       '100000000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             pidpass
                    ON KEY
                    @ 01, 43 GET  ;
                      pro( 5)  ;
                      PICTURE  ;
                      '999,999'  ;
                      VALID  ;
                      valcan()
                    READ
                    IF LASTKEY() =  ;
                       27
                         tmp_1 = 1
                         EXIT
                    ENDIF
                    SELECT gc_alm00
                    SEEK pro(1) +  ;
                         des(7)
                    IF FOUND()
                         DO rbloquea
                         REPLACE alm_stkres  ;
                                 WITH  ;
                                 (alm_stkres +  ;
                                 pro(5)) -  ;
                                 w_cantid
                         REPLACE alm_usuari  ;
                                 WITH  ;
                                 clave,  ;
                                 alm_fecha  ;
                                 WITH  ;
                                 DATE(),  ;
                                 alm_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                    ENDIF
                    pro( 8) =  ;
                       ROUND(pro(5) *  ;
                       pro(6),  ;
                       2)
                    @ 01, 64 SAY  ;
                      pro(8)  ;
                      PICTURE  ;
                      '99,999,999.99'
                    DO WHILE  ;
                       tmp_1=0
                         DO p_footer  ;
                            WITH  ;
                            '100000000000000000101',  ;
                            2
                         ACTIVATE  ;
                          WINDOW  ;
                          pidpass
                         SET SYSMENU ON
                         ON KEY
                         ON KEY LABEL;
f9 do calcu
                         @ 01, 51  ;
                           GET  ;
                           pro(  ;
                           6)  ;
                           PICTURE  ;
                           '9,999,999.99'  ;
                           VALID  ;
                           pro(6) >=  ;
                           0
                         READ
                         IF LASTKEY() =  ;
                            27
                              SET SYSMENU;
OFF
                              tmp_1 =  ;
                               1
                              EXIT
                         ENDIF
                         IF pro(6) <  ;
                            0
                              DO p_mensaje  ;
                                 WITH  ;
                                 'IMPORTE NEGATIVO.'
                              LOOP
                         ENDIF
                         pro( 8) =  ;
                            ROUND(pro(5) *  ;
                            pro(6),  ;
                            2)
                         @ 01, 64  ;
                           SAY  ;
                           pro(8)  ;
                           PICTURE  ;
                           '99,999,999.99'
                         ON KEY LABEL;
f9
                         SET SYSMENU OFF
                         tmp_1 = 1
                    ENDDO
               ENDDO
          ENDDO
     ENDDO
     IF LASTKEY() <> 27
          SELECT despa
          IF w_opcion = 1
               m = m + 1
               APPEND BLANK
               REPLACE codigo  ;
                       WITH  ;
                       pro(1)
               REPLACE despro  ;
                       WITH  ;
                       w_despro
               REPLACE descri  ;
                       WITH  ;
                       pro(3)
               REPLACE docref  ;
                       WITH  ;
                       pro(4)
               w_docref = pro(4)
          ENDIF
          GOTO TOP
          LOCATE FOR pro(1) =  ;
                 codigo .AND.  ;
                 w_docref =  ;
                 docref
          REPLACE unidad WITH  ;
                  pro(2)
          REPLACE docref WITH  ;
                  pro(4)
          REPLACE cantidad WITH  ;
                  pro(5)
          REPLACE tempre WITH  ;
                  pro(6)
          IF des(11) = 'DOL '
               REPLACE precio  ;
                       WITH  ;
                       pro(6)
          ELSE
               REPLACE precio  ;
                       WITH  ;
                       ROUND(pro(6) /  ;
                       w_tipcav,  ;
                       2)
          ENDIF
          REPLACE total WITH  ;
                  pro(8)
          REPLACE nrodo WITH  ;
                  pro(9)
          DEACTIVATE WINDOW  ;
                     pidpass,  ;
                     pidposs
     ENDIF
     EXIT
ENDDO
DEACTIVATE WINDOW pidpass,  ;
           pidposs
DO cartec
ACTIVATE WINDOW detalle
SELECT despa
RETURN
*
FUNCTION valcan
IF pro(5) = 0
     DO p_mensaje WITH  ;
        'Cantidad en Cero..'
     RETURN .F.
ENDIF
IF pro(5) < 0
     DO p_mensaje WITH  ;
        'Cantidad Negativa.'
     RETURN .F.
ENDIF
SELECT gc_alm00
SEEK pro(1) + des(7)
IF FOUND()
     IF (alm_stkfis - alm_stkres) <=  ;
        0 .AND. w_opcion = 1
          DO p_mensaje WITH  ;
             ' *** No hay stock Disponible *** '
          RETURN .F.
     ENDIF
     IF pro(5) > (alm_stkfis -  ;
        alm_stkres + w_cantid)
          w_can = alm_stkfis -  ;
                  alm_stkres +  ;
                  w_cantid
          DO p_mensaje WITH  ;
             'Cantidad  Exede el Stock'+ ;
             STR(w_can, 4)
          RETURN .F.
     ENDIF
ELSE
     DO p_mensaje WITH  ;
        ' C¢digo no existe en Almac‚n '
     RETURN .F.
ENDIF
RETURN .T.
*
PROCEDURE calcu
ACTIVATE SCREEN
KEYBOARD '{ALT+S}+{C}'
ACTIVATE WINDOW pidpass
RETURN
*
PROCEDURE p_borra
ON KEY
SELECT gc_alm00
SEEK despa.codigo + des(7)
IF FOUND()
     DO rbloquea
     REPLACE alm_stkres WITH  ;
             alm_stkres -  ;
             despa.cantidad
     REPLACE alm_usuari WITH  ;
             clave, alm_fecha  ;
             WITH DATE(),  ;
             alm_hora WITH  ;
             TIME()
     UNLOCK
ENDIF
SELECT despa
DELETE
m = m - 1
DO cartec
RETURN
*
PROCEDURE crea
ON KEY
STORE 0 TO w_igv, w_totcan,  ;
      w_totpre, w_tottem
DEFINE WINDOW tot FROM 16, 20 TO  ;
       21, 60 TITLE 'TOTALES' IN  ;
       screen
ACTIVATE WINDOW tot
@ 01, 0 SAY ' TOTAL CANTIDAD :'
@ 02, 0 SAY ' MONTO TOTAL    :'
@03,0 say " MONTO &sys_codimp     :"
SELECT despa
GOTO TOP
SCAN WHILE  .NOT. EOF()
     w_totcan = w_totcan +  ;
                cantidad
     w_tottem = w_tottem + total
     w_totpre = w_totpre +  ;
                ROUND(cantidad *  ;
                precio, 2)
ENDSCAN
w_igv = ROUND(w_tottem * w_facigv,  ;
        2)
@ 1, 20 SAY w_totcan PICTURE  ;
  '99,999,999'
@ 2, 20 SAY w_tottem PICTURE  ;
  '99,999,999.99'
@ 3, 20 SAY w_igv PICTURE  ;
  '99,999,999.99'
w_key = 1
w_swfin = 1
DO WHILE w_key<>27 .AND. w_key<>- ;
   1
     DO p_footer WITH  ;
        '110001000000000000001',  ;
        2
     w_key = INKEY(0)
     IF w_key = 27
          DEACTIVATE WINDOW tot
          w_swfin = 1
          SELECT despa
     ELSE
          IF w_key = -6 .OR.  ;
             w_key = -1
               IF LASTKEY() = -6
                    = imprime()
               ELSE
                    w_swfin = 0
                    w_fin = 0
               ENDIF
          ENDIF
     ENDIF
ENDDO
IF w_swfin = 0
     = ooaviso( ;
       ' G R A B A N D O . . .')
     DO grades
     DEACTIVATE WINDOW winmensaje
     DEACTIVATE WINDOW tot
     DEACTIVATE WINDOW titpass,  ;
                pidpass, titposs,  ;
                pidposs, a0
     DEACTIVATE WINDOW detalle
     CLEAR READ
     CLEAR GETS
ELSE
     DEACTIVATE WINDOW tot
     DO cartec
     ACTIVATE WINDOW detalle
     SELECT despa
ENDIF
RETURN
*
PROCEDURE pinta1
ACTIVATE WINDOW a0
@ 00, 00 CLEAR TO 08, 74
@ 00, 00 SAY 'Tipo Doc.. :'
@ 00, 37 SAY 'Nro. Doc.  :'
@ 00, 64 SAY 'TC:'
RETURN
*
PROCEDURE pinta2
ACTIVATE WINDOW a0
@ 01, 00 SAY 'Fecha..... :'
@ 01, 37 SAY 'Fecha Vcto.:'
@ 02, 00 SAY 'Cod. Mov.  :'
@ 02, 37 SAY 'Mov. Trasl.:'
@ 03, 00 SAY 'Alm. Desp. :'
@ 03, 37 SAY 'Moneda     :'
@ 04, 00 SAY 'Tip. Ent.  :'
@ 04, 37 SAY 'C¢d. Ent.  :'
@ 05, 00 SAY 'Tip. Trans.:'
@ 05, 37 SAY 'C¢d. Trans.:'
@ 06, 00 SAY 'Tip.  Ref. :'
@ 06, 37 SAY 'Nro.  Ref. :'
RETURN
*
PROCEDURE carmat
DO carque
m = 0
STORE SPACE(2) TO w_lindet
w_nf = 65
w_pf = 0
w_recno = 0
SELECT 20
USE SHARED gc_dre00 ORDER codigo
SEEK des(13) + des(14)
IF FOUND()
     SCAN WHILE dre_tiprep =  ;
          des(13) .AND.  ;
          dre_nrorep = des(14)  ;
          .AND.  .NOT. EOF()
          IF SUBSTR(dre_codmov, 1,  ;
             1) = 'E' .AND.  ;
             dre_codmov <> 'EDEV'  ;
             .AND. dre_codmov <>  ;
             'COI ' .AND.  ;
             dre_codalm = des(7)
               w_docpro = dre_codpro
               w_tipdoc = dre_tipdoc
               w_nrodoc = dre_nrodoc
               w_cantid = dre_canpro
               SELECT gc_pro00
               SEEK gc_dre00.dre_codpro
               IF  .NOT. FOUND()
                    w_codexi = gc_dre00.dre_codpro
                    w_exi = 1
                    DO p_mens2  ;
                       WITH  ;
                       ' C¢digo no Existe en Ficha :'+ ;
                       w_codexi
               ELSE
                    SELECT gc_alm00
                    SEEK gc_dre00.dre_codpro +  ;
                         des(7)
                    IF  .NOT.  ;
                        FOUND()
                         w_codexi =  ;
                          gc_dre00.dre_codpro
                         w_exi = 1
                         DO p_mens2  ;
                            WITH  ;
                            ' C¢digo no Existe en Almac‚n :'+ ;
                            w_codexi
                    ELSE
                         w_fla = .T.
                         IF w_cantid >  ;
                            (alm_stkfis -  ;
                            alm_stkres)
                              IF (alm_stkfis -  ;
                                 alm_stkres) >  ;
                                 0
                                   w_cantid = alm_stkfis - alm_stkres
                                   DO p_mens2 WITH ' No hay el total de stock para este C¢digo :'+gc_dre00.dre_codpro+' solo :'+STR(w_cantid, 4)
                                   w_fla = .T.
                              ELSE
                                   DO p_mens2 WITH ' No hay stock para este C¢digo :'+gc_dre00.dre_codpro
                                   w_fla = .F.
                              ENDIF
                         ENDIF
                         IF w_fla
                              DO rbloquea
                              REPLACE  ;
                               alm_stkres  ;
                               WITH  ;
                               alm_stkres +  ;
                               w_cantid
                              REPLACE  ;
                               alm_usuari  ;
                               WITH  ;
                               clave,  ;
                               alm_fecha  ;
                               WITH  ;
                               DATE(),  ;
                               alm_hora  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                              m =  ;
                               m +  ;
                               1
                              IF m >=  ;
                                 100
                                   IF w_pf = 9
                                        w_nf = w_nf + 1
                                        w_pf = 1
                                   ELSE
                                        w_pf = w_pf + 1
                                   ENDIF
                                   w_lindet = CHR(w_nf) + STR(w_pf, 1)
                              ELSE
                                   w_lindet = STR(m, 2)
                              ENDIF
                              SELECT  ;
                               despa
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               descri  ;
                               WITH  ;
                               SUBSTR(gc_pro00.pro_descri,  ;
                               1,  ;
                               15)
                              REPLACE  ;
                               despro  ;
                               WITH  ;
                               SUBSTR(gc_pro00.pro_descri,  ;
                               1,  ;
                               40)
                              REPLACE  ;
                               unidad  ;
                               WITH  ;
                               gc_pro00.pro_unimed
                              REPLACE  ;
                               codigo  ;
                               WITH  ;
                               gc_dre00.dre_codpro
                              REPLACE  ;
                               cantidad  ;
                               WITH  ;
                               w_cantid
                              REPLACE  ;
                               nitem  ;
                               WITH  ;
                               w_lindet
                              SELECT  ;
                               gc_dlp00
                              SEEK  ;
                               rge_lispre +  ;
                               gc_dre00.dre_codpro
                              IF FOUND()
                                   SELECT despa
                                   REPLACE precio WITH gc_dlp00.dlp_prcigv
                                   IF des(11) = 'DOL '
                                        REPLACE tempre WITH gc_dlp00.dlp_prcigv
                                   ELSE
                                        REPLACE tempre WITH ROUND(gc_dlp00.dlp_prcigv * w_tipcav, 2)
                                   ENDIF
                              ENDIF
                              SELECT  ;
                               despa
                              REPLACE  ;
                               total  ;
                               WITH  ;
                               ROUND(cantidad *  ;
                               tempre,  ;
                               2)
                              SELECT  ;
                               gc_dre00
                              IF gc_dre00.dre_codmov =  ;
                                 'EPRD'
                                   DO CASE
                                        CASE SUBSTR(dre_indori, 1, 1) = 'G' .AND. dre_codtal < '011 '
                                             w_docref = 'G-'
                                        CASE SUBSTR(dre_indori, 1, 1) = 'F' .AND. dre_codtal < '011 '
                                             w_docref = 'F-'
                                        CASE SUBSTR(dre_indori, 1, 1) = 'P' .AND. dre_codtal < '011 '
                                             w_docref = 'P-'
                                        CASE SUBSTR(dre_indori, 1, 1) = 'G' .AND. dre_codtal > '010 '
                                             w_docref = 'DG'
                                        CASE SUBSTR(dre_indori, 1, 1) = 'F' .AND. dre_codtal > '010 '
                                             w_docref = 'DF'
                                        CASE SUBSTR(dre_indori, 1, 1) = 'P' .AND. dre_codtal > '010 '
                                             w_docref = 'DP'
                                        OTHERWISE
                                             w_docref = '  '
                                   ENDCASE
                                   w_docref = w_docref + ALLTRIM(dre_numord)
                              ELSE
                                   w_docref = SUBSTR(dre_tipdoc, 1, 1) + '/' + SUBSTR(dre_nrodoc, 3, 8)
                              ENDIF
                              SELECT  ;
                               despa
                              REPLACE  ;
                               docref  ;
                               WITH  ;
                               w_docref
                         ENDIF
                    ENDIF
               ENDIF
               SELECT gc_dre00
          ENDIF
     ENDSCAN
ENDIF
RETURN
*
PROCEDURE imprime
DEFINE WINDOW ped_imp FROM 16, 40  ;
       TO 20, 60 IN screen COLOR  ;
       SCHEME 20
ACTIVATE WINDOW ped_imp
@ 1, 2 SAY 'N§ DE COPIAS:'
@ 1, 16 GET _PCOPIES RANGE 1,99  ;
  PICTURE '99'
READ
IF LASTKEY() = 27
     DEACTIVATE WINDOW ped_imp
     RETURN
ENDIF
DEACTIVATE WINDOW ped_imp
ON KEY
DO p_mensaje WITH  ;
   'Impresi¢n en proceso. Espere......'
STORE 70 TO aux_lin
STORE 0 TO aux_sw1, aux_con1,  ;
      aux_con2, aux_tv, aux_ta
aux_pep = 'PEDID'
tmp_pep = f_reporte(aux_pep)
SET DEVICE TO PRINTER
set printer to &tmp_pep
IF des(1) = 'GUIA'
     DO imprim
ELSE
     DO imprimir
ENDIF
SET DEVICE TO SCREEN
SET PRINTER TO
ON KEY
rpt = .T.
rpt = f_yesno( ;
      'Prepare la Impresora y Confirme Acci¢n' ;
      )
IF rpt .OR. LASTKEY() = 27
     PRINTJOB
     stat = SYS(13)
     DO WHILE stat='OFFLINE'
          swt = f_yesno( ;
                'La Impresora No Esta Lista, Verifique Por Favor' ;
                )
          IF swt
               stat = SYS(13)
          ELSE
               DEACTIVATE POPUP  ;
                          impresora
               SET CURSOR OFF
               RETURN
          ENDIF
     ENDDO
     IF des(1) = 'GUIA'
          SET DEVICE TO PRINTER
          DO imprim
     ELSE
          DO imprimir
     ENDIF
     ENDPRINTJOB
ENDIF
_PCOPIES = 1
erase  &tmp_pep  
RETURN
*
PROCEDURE imprim
DO r_titu
aux_lin = aux_lin + 1
STORE 1 TO w_count
SELECT despa
GOTO TOP
SCAN WHILE  .NOT. EOF()
     IF aux_lin > 41
          DO r_titu
     ENDIF
     aux_lin = aux_lin + 1
     @ aux_lin, 00 SAY w_count  ;
       PICTURE '9,999'
     @ aux_lin, 06 SAY codigo
     @ aux_lin, 21 SAY  ;
       SUBSTR(despro, 1, 38)
     SELECT gc_alm00
     SEEK despa.codigo + des(7)
     IF FOUND()
          w_ubica = SUBSTR(gc_alm00.alm_ubicac,  ;
                    1, 5)
     ELSE
          STORE SPACE(5) TO  ;
                w_ubica
     ENDIF
     SELECT despa
     @ aux_lin, 75 SAY w_ubica
     @ aux_lin, 088 SAY docref
     @ aux_lin, 099 SAY cantidad  ;
       PICTURE '999,999'
     @ aux_lin, 110 SAY tempre  ;
       PICTURE '9,999,999.99'
     @ aux_lin, 125 SAY total  ;
       PICTURE '999,999,999.99'
     w_count = w_count + 1
ENDSCAN
EJECT
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE r_titu
@ 00, 00 SAY CHR(27) + 'F'
@ 00, 00 SAY CHR(18)
@ 03, 54 SAY 'R.U.C.  ' +  ;
  rge_numruc
@ 04, 00 SAY CHR(15)
@ 10, 03 SAY  ;
  'Domicilio fiscal : AV. REP. DE PANAMA 4123 - SURQUILLO'
DO CASE
     CASE des(30) = '001'
          @ 12, 20 SAY 'X'
     CASE des(30) = '002'
          @ 13, 20 SAY 'X'
     CASE des(30) = '003'
          @ 14, 20 SAY 'X'
     CASE des(30) = '004'
          @ 12, 50 SAY 'X'
     CASE des(30) = '005'
          @ 13, 50 SAY 'X'
     CASE des(30) = '006'
          @ 14, 50 SAY 'X'
     CASE des(30) = '007'
          @ 12, 62 SAY 'X'
     CASE des(30) = '008'
          @ 13, 62 SAY 'X'
ENDCASE
@ 15, 095 SAY des(1)
@ 15, 105 SAY des(2)
@ 17, 21 SAY rge_abrev
@ 17, 113 SAY des(3)
@ 18, 21 SAY ALLTRIM(rge_calle) +  ;
  ' ' + ALLTRIM(rge_distri) +  ;
  ' - ' + rge_provin
@ 20, 096 SAY 'MONEDA: '
@ 20, 110 SAY w_desmon
@ 22, 021 SAY w_trans
@ 22, 113 SAY w_ructra
@ 23, 021 SAY w_dirtra
@ 25, 118 SAY ALLTRIM(w_tipmon)
@ 25, 135 SAY ALLTRIM(w_tipmon)
aux_lin = 25
SELECT despa
RETURN
*
PROCEDURE imprimir
SELECT despa
GOTO TOP
??? CHR(27) + CHR(15)
REPORT FORMAT agcp2222 TO PRINTER  ;
       NOCONSOLE
??? CHR(27) + CHR(80)
SET RELATION TO
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE carque
CREATE CURSOR despa (codigo C  ;
       (14), unidad C (4), descri  ;
       C (15), docref C (10),  ;
       cantidad N (10, 0), tempre  ;
       N (12, 2), precio N (12,  ;
       2), total N (12, 2), nrodo  ;
       C (10), tipdoc C (4),  ;
       nitem C (2), fob N (12, 2),  ;
       flete N (9, 2), seguro N  ;
       (9, 2), cif N (9, 2), adva  ;
       N (9, 2), gastos N (9, 2),  ;
       totnac N (12, 2), cosalm N  ;
       (12, 2), cosuni N (12, 2),  ;
       cospro N (12, 2), codprp C  ;
       (14), despro C (40))
RETURN
*
PROCEDURE cartec
ON KEY
IF m = 0
     DO p_footer WITH  ;
        '101010000000000000001001',  ;
        2
     ON KEY LABEL F3 do p_lee with 1
ELSE
     DO p_footer WITH  ;
        '111100000000101000001001',  ;
        2
     ON KEY LABEL F3 do p_lee with 1
     ON KEY LABEL F2 do crea 
     ON KEY LABEL F4 do p_borra 
ENDIF
ON KEY LABEL enter do p_lee with 2
ON KEY LABEL f5 do modifica
ON KEY LABEL ctrl+w ??
RETURN
*
FUNCTION ooverdoc
PARAMETER w_var
ON KEY
SELECT 20
USE SHARED gc_hre00 ORDER  ;
    hre_codtip
COUNT FOR hre_codalm = des(7)  ;
      .AND. hre_tiprep = des(13)  ;
      .AND. hre_indest = 'V' TO  ;
      n
IF n = 0
     DO p_mensaje WITH  ;
        ' No Hay Documentos Generados'
     RETURN
ENDIF
SELECT DISTINCT  ;
       TRANSFORM(hre_nrorep,  ;
       '@!') + '³' +  ;
       DTOC(hre_fecemi) + '³' +  ;
       'Vigente' FROM gc_hre00  ;
       WHERE (hre_tiprep =  ;
       des(13) AND hre_codalm =  ;
       des(7) AND hre_indest =  ;
       'V') INTO ARRAY  ;
       arrayorden
ACTIVATE WINDOW marco
ACTIVATE WINDOW busqueda
@ 00, 00 SAY  ;
  '  # Factura   Fecha     Estado      Tot.General    '  ;
  COLOR N/W 
@ 01, 00 GET getorden DEFAULT  ;
  arrayorden SIZE 12, 51 FROM  ;
  arrayorden VALID  ;
  oovalor(getorden) COLOR SCHEME  ;
  8
READ CYCLE
IF LASTKEY() = 27
     w_var = SPACE(10)
ENDIF
DEACTIVATE WINDOW busqueda, marco
des( 14) = w_var
RETURN w_var
*
FUNCTION oovalor
PARAMETER cdato
w_var = SUBSTR(cdato, 1, 10)
CLEAR READ
RETURN w_var
*
PROCEDURE grades
STORE 0 TO tcp
tcp = w_tipcav
STORE 0 TO w_cosuni, w_nue,  ;
      w_total, w_stkant, w_ant,  ;
      w_nue, w_prmo, w_prmb,  ;
      w_almant
IF des(13) = 'PLAN'
     SELECT 20
     USE SHARED gc_hre00 ORDER  ;
         hre_codtip
     SEEK des(7) + des(13) +  ;
          des(14)
     IF FOUND()
          DO rbloquea
          REPLACE hre_indest WITH  ;
                  'C'
          REPLACE hre_usuari WITH  ;
                  clave
          REPLACE hre_fecha WITH  ;
                  DATE()
          REPLACE hre_hora WITH  ;
                  TIME()
          UNLOCK
     ENDIF
ENDIF
SELECT despa
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT gc_pro00
     SEEK despa.codigo
     IF FOUND()
          DO rbloquea
          REPLACE pro_ultven WITH  ;
                  DATE()
          REPLACE pro_ultmov WITH  ;
                  DATE()
          REPLACE pro_usuari WITH  ;
                  clave
          REPLACE pro_fecha WITH  ;
                  DATE()
          REPLACE pro_hora WITH  ;
                  TIME()
          w_proant = pro_coprmo
          w_proanb = pro_coprmb
          w_cosuni = pro_ulcomb
          UNLOCK
     ELSE
          w_proant = 0
          w_proanb = 0
          w_cosuni = 0
     ENDIF
     SELECT gc_alm00
     SEEK despa.codigo + des(7)
     IF FOUND()
          w_almant = alm_stkfis
     ELSE
          w_almant = 0
     ENDIF
     SELECT gc_kar00
     APPEND BLANK
     DO rbloquea
     REPLACE kar_tipdoc WITH  ;
             des(1)
     REPLACE kar_nrodoc WITH  ;
             des(2)
     REPLACE kar_codpro WITH  ;
             despa.codigo
     REPLACE kar_unimed WITH  ;
             despa.unidad
     REPLACE kar_fecing WITH  ;
             DATE()
     REPLACE kar_horing WITH  ;
             TIME()
     REPLACE kar_lindet WITH  ;
             despa.nitem
     REPLACE kar_fecdoc WITH  ;
             des(3)
     REPLACE kar_codmov WITH  ;
             des(5)
     REPLACE kar_codmon WITH  ;
             des(11)
     REPLACE kar_tipent WITH  ;
             des(9)
     REPLACE kar_codent WITH  ;
             des(10)
     REPLACE kar_stkant WITH  ;
             w_almant
     REPLACE kar_cantid WITH  ;
             despa.cantidad
     REPLACE kar_import WITH  ;
             ROUND(despa.precio /  ;
             (1 + w_facigv), 2)
     REPLACE kar_cosuni WITH  ;
             w_cosuni
     REPLACE kar_cosant WITH  ;
             w_proant
     REPLACE kar_cosanb WITH  ;
             w_proanb
     REPLACE kar_almdes WITH  ;
             des(7)
     REPLACE kar_usuari WITH  ;
             clave
     REPLACE kar_fecha WITH  ;
             DATE()
     REPLACE kar_hora WITH TIME()
     IF des(13) = 'PLAN'
          REPLACE kar_tidore WITH  ;
                  despa.tipdoc
          REPLACE kar_nrdore WITH  ;
                  despa.docref
     ELSE
          REPLACE kar_tidore WITH  ;
                  SPACE(4)
          REPLACE kar_nrdore WITH  ;
                  despa.docref
     ENDIF
     UNLOCK
     SELECT gc_alm00
     SEEK despa.codigo + des(7)
     IF  .NOT. FOUND()
          APPEND BLANK
          DO rbloquea
          REPLACE alm_codpro WITH  ;
                  despa.codigo
          REPLACE alm_codalm WITH  ;
                  des(7)
          UNLOCK
     ENDIF
     DO rbloquea
     REPLACE alm_stkfis WITH  ;
             alm_stkfis -  ;
             despa.cantidad
     REPLACE alm_stkres WITH  ;
             alm_stkres -  ;
             despa.cantidad
     REPLACE alm_usuari WITH  ;
             clave
     REPLACE alm_fecha WITH  ;
             DATE()
     REPLACE alm_hora WITH TIME()
     UNLOCK
     SELECT gc_dip00
     APPEND BLANK
     DO rbloquea
     REPLACE dip_tipdoc WITH  ;
             des(1)
     REPLACE dip_nrodoc WITH  ;
             des(2)
     REPLACE dip_propar WITH  ;
             despa.codigo
     REPLACE dip_unimed WITH  ;
             despa.unidad
     REPLACE dip_cantid WITH  ;
             despa.cantidad
     REPLACE dip_docref WITH  ;
             despa.docref
     REPLACE dip_import WITH  ;
             ROUND(despa.precio /  ;
             (1 + w_facigv), 2)
     REPLACE dip_total WITH  ;
             ROUND(dip_cantid *  ;
             dip_import, 2)
     REPLACE dip_usuari WITH  ;
             clave
     REPLACE dip_fecha WITH  ;
             DATE()
     REPLACE dip_hora WITH TIME()
     UNLOCK
     w_total = w_total +  ;
               dip_total
     SELECT despa
ENDSCAN
SELECT gc_hip00
APPEND BLANK
DO rbloquea
GATHER FROM des
REPLACE hip_totnet WITH w_total
REPLACE hip_totigv WITH  ;
        ROUND(w_total * w_facigv,  ;
        2)
REPLACE hip_totoim WITH  ;
        (hip_totnet +  ;
        hip_totigv)
REPLACE hip_totgen WITH  ;
        (hip_totnet +  ;
        hip_totigv)
REPLACE hip_estdoc WITH 'C'
REPLACE hip_usuari WITH clave
REPLACE hip_fecha WITH DATE()
REPLACE hip_hora WITH TIME()
UNLOCK
RETURN
*
PROCEDURE p_mens2
PARAMETER mens
SET CURSOR OFF
l = LEN(mens)
DEFINE WINDOW mensj FROM 11, (80 -  ;
       l) / 2 - 4 TO 15, (80 + l) /  ;
       2 + 4 COLOR SCHEME 12
ACTIVATE WINDOW mensj
@ 01, (WCOLS('MENSJ') - l) / 2  ;
  SAY mens
READ
RELEASE WINDOW mensj
SET CURSOR ON
RETURN
*
PROCEDURE limpia
SELECT despa
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT gc_alm00
     SEEK despa.codigo + des(7)
     IF FOUND()
          DO rbloquea
          REPLACE alm_stkres WITH  ;
                  alm_stkres -  ;
                  despa.cantidad
          REPLACE alm_usuari WITH  ;
                  clave,  ;
                  alm_fecha WITH  ;
                  DATE(),  ;
                  alm_hora WITH  ;
                  TIME()
          UNLOCK
     ENDIF
     SELECT despa
ENDSCAN
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

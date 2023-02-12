*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER w_opcion
SET CENTURY ON
ON KEY
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW produ FROM 12, 02  ;
       TO 20, 76 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW t0 FROM 02, 64 TO  ;
       02, 73 IN screen NONE
DEFINE WINDOW t1 FROM 14, 03 TO  ;
       14, 76 IN screen NONE
DEFINE WINDOW a0 FROM 04, 01 TO  ;
       11, 78 IN screen
DEFINE WINDOW a2 FROM 11, 01 TO  ;
       11, 78 IN screen NONE
DEFINE WINDOW a3 FROM 11, 01 TO  ;
       21, 78 IN screen NONE
DEFINE POPUP detalle FROM 13, 03  ;
       TO 18, 76 PROMPT FIELDS  ;
       dip_propar + '³' +  ;
       dip_unimed + '³' + exp_7 +  ;
       '³' + STR(dip_cantid, 10) +  ;
       '³' + STR(dip_precio, 10,  ;
       2) + '³' + STR(dip_, 6, 2) +  ;
       '³' + STR(dip_total, 11,  ;
       2) IN screen COLOR SCHEME  ;
       8
ON SELECTION POPUP detalle deac popup;
detalle
ACTIVATE WINDOW tablas
flag = USED('gc_par00')
IF w_opcion = 1
     DO p_prestab WITH 'PROCESO',  ;
        'FACTURA AUTOMATICA',  ;
        'SELECCION', flag
ELSE
     DO p_prestab WITH 'PROCESO',  ;
        ' FACTURA  MANUAL ',  ;
        'SELECCION', flag
ENDIF
@ 02, 63 SAY PROGRAM()
SELECT 1
USE SHARED gc_hve00 ORDER codigo
w_selec = SELECT()
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
USE SHARED gc_uni00 ORDER codigo
SELECT 4
USE SHARED gc_cli00 ORDER codigo
SELECT 5
USE SHARED gc_vnd00 ORDER codigo
SELECT 6
USE SHARED gc_hlp00 ORDER codigo
SELECT 7
USE SHARED gc_dlp00 ORDER codigo
SELECT 8
USE SHARED gc_pro00 ORDER codigo
w_selpro = SELECT()
SELECT 9
USE SHARED gc_alm00 ORDER codigo
SELECT 20
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
STORE 0 TO w_fin, w_mod
w_tipcam = ootc2(DATE(),'SOL ', ;
           'DOL ','2')
USE
w_facigv = facigv()
DO WHILE w_fin<>1
     IF w_mod = 0
          DIMENSION producto( 1),  ;
                    descri( 1),  ;
                    cantid( 1),  ;
                    preuni( 1),  ;
                    tempre( 1),  ;
                    candes( 1),  ;
                    factor( 1),  ;
                    canres( 1),  ;
                    deta( 1)
          STORE 0 TO m, w_ref, c,  ;
                w_telefo, xm,  ;
                w_fin, w_preuni,  ;
                w_tempre,  ;
                w_factor, w_paso,  ;
                pagctas, pagctad,  ;
                w_sw, subtot,  ;
                totnet, w_canres,  ;
                nro_pro, w_swex,  ;
                tran_1, tran_ant,  ;
                t_c, w_mal,  ;
                w_flete,  ;
                w_embala
          STORE 0 TO w_tempo,  ;
                w_flete, w_embala,  ;
                w_mobra, w_totdes,  ;
                w_pordes,  ;
                imp_subtot
          STORE 0 TO w_solpre,  ;
                w_solmob,  ;
                w_solfle,  ;
                w_soldes,  ;
                w_solemb,  ;
                imp_subsol,  ;
                subtot
          STORE 1 TO z, p
          STORE ' ' TO w_campo,  ;
                w_lismon,  ;
                w_desmon,  ;
                w_desven,  ;
                w_codent
          STORE SPACE(4) TO tipo,  ;
                w_busca, w_var,  ;
                w_almdes,  ;
                w_codmon,  ;
                w_tipdoc,  ;
                w_tidore,  ;
                w_codalm
          STORE SPACE(10) TO  ;
                w_nrdore
     ENDIF
     ACTIVATE WINDOW t0
     @ 00, 00 SAY 'SELECCION'  ;
       COLOR N+/W 
     SET CURSOR ON
     IF w_mod = 0
          DO pinta1
          SELECT gc_hve00
          SCATTER BLANK MEMVAR
     ENDIF
     ON KEY
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW a0
     IF w_tipcam = -1
          DO p_mensaje WITH  ;
             'No Existe Tipo de Cambio'
          EXIT
     ENDIF
     @ 00, 13 GET m.hve_tipdoc  ;
       DEFAULT ' ' SIZE 1, 04  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     READ
     IF LASTKEY() <> 27
          DO det_cab WITH 0
     ELSE
          IF w_mod = 1
               w_mod = 0
               DEACTIVATE WINDOW  ;
                          a2, a3
               FOR x = 1 TO m
                    w_cantid = 0
                    SELECT gc_alm00
                    SEEK producto(x) +  ;
                         m.hve_almdes
                    IF FOUND()
                         w_cantid =  ;
                          cantid(x)
                         IF factor(x) =  ;
                            1
                              IF w_cantid >  ;
                                 canres(x)
                                   w_cantid = w_cantid - canres(x)
                              ELSE
                                   w_cantid = 0
                              ENDIF
                         ENDIF
                         DO rbloquea
                         REPLACE alm_stkres  ;
                                 WITH  ;
                                 alm_stkres -  ;
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
               ENDFOR
          ENDIF
          w_fin = 1
     ENDIF
ENDDO
ON KEY LABEL f6
ON KEY
DEACTIVATE WINDOW tablas
RELEASE WINDOW t0, a0, a1, c0, a2,  ;
        a3, a4
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE det_cab
PARAMETER w_opc
IF w_mod = 0
     m.hve_codemi = rge_punemi
     m.hve_almdes = rge_codalm
     m.hve_codmon = rge_monbas
     m.hve_tippag = '001 '
     m.hve_lispre = rge_lispre
ENDIF
@ 1, 13 GET m.hve_codent DEFAULT  ;
  ' ' SIZE 1, 11 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD())
@ 2, 13 GET m.hve_codvnd DEFAULT  ;
  ' ' SIZE 1, 5 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD())
@ 3, 13 GET m.hve_codemi DEFAULT  ;
  rge_punemi SIZE 1, 4 PICTURE  ;
  '@!' VALID oovalid(VARREAD())  ;
  WHEN oowhen(VARREAD())
@ 3, 54 GET m.hve_almdes DEFAULT  ;
  '0001' SIZE 1, 4 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD())
@ 4, 13 GET m.hve_codmon DEFAULT  ;
  rge_lispre SIZE 1, 4 PICTURE  ;
  '@!' VALID oovalid(VARREAD())  ;
  WHEN oowhen(VARREAD())
@ 4, 54 GET m.hve_tippag DEFAULT  ;
  '001 ' SIZE 1, 4 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD())
@ 5, 13 GET m.hve_tidore DEFAULT  ;
  ' ' SIZE 1, 4 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD())
@ 5, 54 GET m.hve_nrdore DEFAULT  ;
  0 SIZE 1, 10 PICTURE  ;
  '@ 9999999999' VALID  ;
  oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD())
IF w_opc = 0
     READ CYCLE
     IF LASTKEY() <> 27
          DO detalle
          IF LASTKEY() = 27 .AND.  ;
             w_mod = 0
               ACTIVATE WINDOW a0
               = limpia()
               CLEAR GETS
          ENDIF
     ELSE
          IF w_mod = 1
               w_mod = 0
               DEACTIVATE WINDOW  ;
                          a2, a3
               FOR x = 1 TO m
                    w_cantid = 0
                    SELECT gc_alm00
                    SEEK producto(x) +  ;
                         m.hve_almdes
                    IF FOUND()
                         w_cantid =  ;
                          cantid(x)
                         IF factor(x) =  ;
                            1
                              IF w_cantid >  ;
                                 canres(x)
                                   w_cantid = w_cantid - canres(x)
                              ELSE
                                   w_cantid = 0
                              ENDIF
                         ENDIF
                         DO rbloquea
                         REPLACE alm_stkres  ;
                                 WITH  ;
                                 alm_stkres -  ;
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
               ENDFOR
          ENDIF
          SELECT gc_hve00
          SCATTER BLANK MEMVAR
          CLEAR GETS
          = limpia()
     ENDIF
ELSE
     CLEAR GETS
ENDIF
RETURN
*
PROCEDURE detalle
ON KEY
IF w_paso = 0 .AND. w_mod = 0
     = carque()
ENDIF
= cartec()
w_cantid = 0
ACTIVATE WINDOW a3
@ 00, 00 SAY  ;
  '   C¢digo       Descripci¢n            Cantidad    Pre.Unit.         Total    '  ;
  COLOR SCHEME 8
w_sig = 0
w_mod = 0
DO WHILE w_sig=0 .AND. LASTKEY()<>- ;
   1
     SHOW GET arrayitem
     @ 1, 0 GET arrayitem DEFAULT  ;
       deta(p) SIZE 09, 78 FROM  ;
       deta VALID p_lee(2) WHEN  ;
       posi() COLOR SCHEME 9
     READ CYCLE
     IF LASTKEY() = 27 .OR.  ;
        LASTKEY() = -4
          IF w_mod = 0
               FOR x = 1 TO m
                    w_cantid = 0
                    SELECT gc_alm00
                    SEEK producto(x) +  ;
                         m.hve_almdes
                    IF FOUND()
                         w_cantid =  ;
                          cantid(x)
                         IF factor(x) =  ;
                            1
                              IF w_cantid >  ;
                                 canres(x)
                                   w_cantid = w_cantid - canres(x)
                              ELSE
                                   w_cantid = 0
                              ENDIF
                         ENDIF
                         DO rbloquea
                         REPLACE alm_stkres  ;
                                 WITH  ;
                                 alm_stkres -  ;
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
               ENDFOR
          ENDIF
          w_sig = 1
     ENDIF
ENDDO
SHOW GET arrayitem
DEACTIVATE WINDOW a2
SHOW WINDOW a3
IF w_mod = 0
     DEACTIVATE WINDOW a2, a3
     ACTIVATE WINDOW a0
     DO pinta1
ELSE
     ACTIVATE WINDOW a0
ENDIF
RETURN
*
FUNCTION p_lee
PARAMETER aux_opcion
CLEAR GETS
w_sale = 0
w_sw = 0
IF m = 0 .AND. aux_opcion = 2
     DO cartec
     RETURN
ENDIF
DO WHILE w_sale=0
     w_sale = 1
     SHOW GET arrayitem
     SET CURSOR ON
     ON KEY
     swt = .T.
     STORE 0 TO tmp_fin, tmp_1
     IF aux_opcion = 2
          pos = ASCAN(deta,  ;
                arrayitem)
          p = pos
          DEFINE WINDOW a4 FROM  ;
                 12 + p, 02 TO 12 +  ;
                 p, 77 IN screen  ;
                 NONE
          w_codpro = producto(pos)
          w_descri = descri(pos)
          w_cantid = cantid(pos)
          w_preuni = preuni(pos)
          w_tempre = tempre(pos)
          w_candes = candes(pos)
          w_canres = canres(pos)
          w_factor = factor(pos)
          ACTIVATE WINDOW a4
          @ 0, 1 SAY arrayitem
     ELSE
          SHOW GET enable
          IF m < 7
               DEFINE WINDOW a4  ;
                      FROM 12 + m +  ;
                      1, 02 TO 12 +  ;
                      m + 1, 77  ;
                      IN screen  ;
                      NONE
          ELSE
               DEFINE WINDOW a4  ;
                      FROM 019,  ;
                      02 TO 019,  ;
                      77 IN  ;
                      screen  ;
                      NONE
          ENDIF
          w_codpro = '              '
          w_descri = '              '
          w_cantid = 0
          w_preuni = 0
          w_tempre = 0
          w_canres = 0
          w_candes = 0
          w_factor = 0
          ACTIVATE WINDOW a4
          @ 00, 01 SAY  ;
            '              ³                    ³         ³             ³              '
          w_sale = 0
     ENDIF
     IF aux_opcion = 1
          IF m.hve_tipdoc =  ;
             'FACT'
               IF nro_pro = 13
                    DO p_mensaje  ;
                       WITH  ;
                       'Grabe --- no Puede Ingresar Mas de 13 Productos'
                    DO cartec
                    DEACTIVATE WINDOW  ;
                               a4
                    ACTIVATE WINDOW  ;
                             a3
                    RETURN .F.
               ENDIF
          ELSE
               IF nro_pro = 14
                    DO p_mensaje  ;
                       WITH  ;
                       'Grabe --- no Puede Ingresar Mas de 14 Productos'
                    DO cartec
                    DEACTIVATE WINDOW  ;
                               a4
                    ACTIVATE WINDOW  ;
                             a3
                    RETURN .F.
               ENDIF
          ENDIF
          @ 00, 01 GET w_codpro  ;
            PICTURE '@!' VALID  ;
            vali2a() .AND.  ;
            vali2b() WHEN  ;
            when2a()
     ENDIF
     ON KEY
     @ 00, 37 GET w_cantid  ;
       DEFAULT 0 PICTURE  ;
       '9,999,999' VALID vali2c()  ;
       WHEN when2c()
     READ
     IF LASTKEY() <> 27
          IF w_preuni = 0
               w_tem = pre_pro(w_codpro)
               IF m.hve_codmon =  ;
                  'DOL '
                    w_preuni = w_tem
                    w_tempre = ROUND(w_preuni *  ;
                               w_tipcam,  ;
                               2)
               ELSE
                    w_preuni = ROUND(w_tem *  ;
                               w_tipcam,  ;
                               2)
                    w_tempre = w_tem
               ENDIF
          ENDIF
          IF w_preuni = 0
               DO p_mensaje WITH  ;
                  'El Precio no Puede ser Cero'
               LOOP
          ENDIF
          @ 00, 47 SAY w_preuni  ;
            PICTURE  ;
            '99,999,999.99'
          w_candes = ROUND(w_cantid *  ;
                     w_preuni,  ;
                     2)
          @ 00, 61 SAY w_candes  ;
            PICTURE  ;
            '999,999,999.99'
          IF aux_opcion = 1
               nro_pro = nro_pro +  ;
                         1
               m = m + 1
               DIMENSION deta[ m]
               DIMENSION producto[  ;
                         m]
               DIMENSION descri[  ;
                         m]
               DIMENSION cantid[  ;
                         m]
               DIMENSION preuni[  ;
                         m]
               DIMENSION tempre[  ;
                         m]
               DIMENSION candes[  ;
                         m]
               DIMENSION canres[  ;
                         m]
               DIMENSION factor[  ;
                         m]
               producto[ m] =  ;
                       w_codpro
               SELECT gc_pro00
               SEEK w_codpro
               descri[ m] =  ;
                     SUBSTR(pro_descri,  ;
                     1, 20)
               cantid[ m] =  ;
                     w_cantid
               preuni[ m] =  ;
                     w_preuni
               tempre[ m] =  ;
                     w_tempre
               candes[ m] =  ;
                     w_candes
               canres[ m] = 0
               factor[ m] = 0
               deta[ m] =  ;
                   w_codpro + '³' +  ;
                   w_descri + '³' +  ;
                   TRANSFORM(w_cantid,  ;
                   '@ 9,999,999') +  ;
                   '³' +  ;
                   TRANSFORM(w_preuni,  ;
                   '@ 99,999,999.99' ;
                   ) + '³' +  ;
                   TRANSFORM(w_candes,  ;
                   '@ 999,999,999.99' ;
                   )
               arrayitem = w_codpro +  ;
                           '³' +  ;
                           w_descri +  ;
                           '³' +  ;
                           TRANSFORM(w_cantid,  ;
                           '@ 9,999,999' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(w_preuni,  ;
                           '@ 99,999,999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(w_candes,  ;
                           '@ 999,999,999.99' ;
                           )
               p = m
               pos = m
          ELSE
               producto[ pos] =  ;
                       w_codpro
               cantid[ pos] =  ;
                     w_cantid
               preuni[ pos] =  ;
                     w_preuni
               tempre[ pos] =  ;
                     w_tempre
               candes[ pos] =  ;
                     w_candes
               canres[ pos] =  ;
                     w_canres
               deta[ pos] =  ;
                   w_codpro + '³' +  ;
                   w_descri + '³' +  ;
                   TRANSFORM(w_cantid,  ;
                   '@ 9,999,999') +  ;
                   '³' +  ;
                   TRANSFORM(w_preuni,  ;
                   '@ 99,999,999.99' ;
                   ) + '³' +  ;
                   TRANSFORM(w_candes,  ;
                   '@ 999,999,999.99' ;
                   )
               arrayitem = w_codpro +  ;
                           '³' +  ;
                           w_descri +  ;
                           '³' +  ;
                           TRANSFORM(w_cantid,  ;
                           '@ 9,999,999' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(w_preuni,  ;
                           '@ 99,999,999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(w_candes,  ;
                           '@ 999,999,999.99' ;
                           )
               w_sale = 1
          ENDIF
     ELSE
          IF w_swex = 0
               w_sale = 1
          ENDIF
     ENDIF
ENDDO
DO cartec
DEACTIVATE WINDOW a4
ACTIVATE WINDOW a3
RETURN
*
PROCEDURE p_borra
STORE 0 TO w_sw, w_cantid
ON KEY
aux_swt = .T.
DO WHILE aux_swt
     pos = ASCAN(deta, arrayitem)
     w_codpro = producto(pos)
     w_cantid = cantid(pos)
     SELECT gc_alm00
     SEEK w_codpro + m.hve_almdes
     IF FOUND()
          IF factor(pos) = 1
               IF w_cantid >  ;
                  canres(pos)
                    w_cantid = w_cantid -  ;
                               canres(pos)
               ELSE
                    w_cantid = 0
               ENDIF
          ENDIF
          DO rbloquea
          REPLACE alm_stkres WITH  ;
                  alm_stkres -  ;
                  w_cantid
          REPLACE alm_usuari WITH  ;
                  clave,  ;
                  alm_fecha WITH  ;
                  DATE(),  ;
                  alm_hora WITH  ;
                  TIME()
          UNLOCK
     ENDIF
     nro_pro = nro_pro - 1
     = ADEL(producto, pos)
     = ADEL(descri, pos)
     = ADEL(cantid, pos)
     = ADEL(preuni, pos)
     = ADEL(tempre, pos)
     = ADEL(candes, pos)
     = ADEL(canres, pos)
     = ADEL(factor, pos)
     = ADEL(deta, pos)
     IF p = m
          p = p - 1
     ENDIF
     IF p = 7 .AND. m > 7
          p = 7
     ENDIF
     m = m - 1
     IF m = 0
          DIMENSION deta[ 1]
          deta[ 1] = ' '
          arrayitem = ' '
     ELSE
          DIMENSION deta[ m]
          DIMENSION producto[ m]
          DIMENSION descri[ m]
          DIMENSION cantid[ m]
          DIMENSION preuni[ m]
          DIMENSION tempre[ m]
          DIMENSION candes[ m]
          DIMENSION canres[ m]
          DIMENSION factor[ m]
          w_codpro = producto(p)
          w_descri = descri(p)
          w_cantid = cantid(p)
          w_preuni = preuni(p)
          w_tempre = tempre(p)
          w_candes = candes(p)
          w_canres = canres(p)
          w_factor = factor(p)
          arrayitem = w_codpro +  ;
                      '³' +  ;
                      w_descri +  ;
                      '³' +  ;
                      TRANSFORM(w_cantid,  ;
                      '@ 9,999,999' ;
                      ) + '³' +  ;
                      TRANSFORM(w_preuni,  ;
                      '@ 99,999,999.99' ;
                      ) + '³' +  ;
                      TRANSFORM(w_candes,  ;
                      '@ 999,999,999.99' ;
                      )
     ENDIF
     UNLOCK
     EXIT
ENDDO
DO cartec
DEACTIVATE WINDOW a4
ACTIVATE WINDOW a3
RETURN
*
PROCEDURE crea
ON KEY
DO gastos
STORE 0 TO w_predol, w_presol,  ;
      w_fmp
STORE 1 TO p
IF LASTKEY() = 27
     DO cartec
     ACTIVATE WINDOW a3
     RETURN
ENDIF
SELECT 20
USE
USE SHARED gc_hpv00 ORDER codigo
SELECT 21
USE SHARED gc_dpv00 ORDER codigo
IF w_ref = 1
     SELECT gc_hve00
     SEEK m.hve_tidore +  ;
          m.hve_nrdore
     IF FOUND()
          DO rbloquea
          REPLACE hve_estdoc WITH  ;
                  'C'
          REPLACE hve_usuari WITH  ;
                  clave
          REPLACE hve_fecha WITH  ;
                  DATE()
          REPLACE hve_hora WITH  ;
                  TIME()
          UNLOCK
     ENDIF
     SELECT gc_hpv00
     SEEK m.hve_tidore +  ;
          m.hve_nrdore
     DO rbloquea
     REPLACE hpv_indest WITH 'F'
     REPLACE hpv_fecfin WITH  ;
             DATE()
     REPLACE hpv_horfin WITH  ;
             TIME()
     REPLACE hpv_usuari WITH  ;
             clave
     REPLACE hpv_fecha WITH  ;
             DATE()
     REPLACE hpv_hora WITH TIME()
     UNLOCK
ENDIF
IF w_opcion = 1
     m.hve_nrodoc = ' '
     DO status_imp
     DO actual1
     m.hve_nrodoc = ALLTRIM(STR(m.hve_nrodoc,  ;
                    10))
     m.hve_nrodoc = f_ceros(m.hve_nrodoc, ;
                    10,2)
     DO imprime
     CLEAR READ
     = ooactprn()
ELSE
     IF m.hve_tipdoc = 'LIQU'
          SELECT ge_tab0
          SET ORDER TO descr
          SEEK m.hve_tipdoc +  ;
               ALLTRIM(SUBSTR(m.hve_codent,  ;
               1, 12))
          IF FOUND()
               DO rbloquea
               REPLACE tab_factor  ;
                       WITH  ;
                       tab_factor +  ;
                       1
               m.hve_nrodoc = ALLTRIM(STR(tab_factor,  ;
                              10))
               UNLOCK
          ENDIF
          SET ORDER TO codigo
          m.hve_nrodoc = f_ceros(m.hve_nrodoc, ;
                         10,2)
     ENDIF
ENDIF
SELECT gc_hve00
APPEND BLANK
m.hve_tipent = 'C'
m.hve_codmov = 'EVTA'
m.hve_fechtc = m.hve_fecdoc
m.hve_indori = 'VENT'
m.hve_codent = m.hve_codent
DO rbloquea
GATHER MEMVAR
REPLACE hve_cosrep WITH  ;
        ROUND(imp_subtot /  ;
        w_facigv, 2)
REPLACE hve_solrep WITH  ;
        ROUND(imp_subsol /  ;
        w_facigv, 2)
REPLACE hve_flete WITH  ;
        m.hve_flete
REPLACE hve_embala WITH  ;
        m.hve_embala
REPLACE hve_totnet WITH  ;
        m.hve_totnet
REPLACE hve_totdes WITH  ;
        m.hve_totdes
REPLACE hve_totgen WITH  ;
        m.hve_totgen
REPLACE hve_pagctd WITH pagctad
REPLACE hve_totvta WITH  ;
        m.hve_totvta
REPLACE hve_totigv WITH  ;
        m.hve_totigv
REPLACE hve_totoim WITH  ;
        m.hve_totoim
REPLACE hve_estdoc WITH 'C'
REPLACE hve_solfle WITH  ;
        m.hve_solfle
REPLACE hve_solnet WITH  ;
        m.hve_solnet
REPLACE hve_soldes WITH  ;
        m.hve_soldes
REPLACE hve_solgen WITH  ;
        m.hve_solgen
REPLACE hve_pagcts WITH pagctas
REPLACE hve_solvta WITH  ;
        m.hve_solvta
REPLACE hve_soligv WITH  ;
        m.hve_soligv
REPLACE hve_mtocan WITH  ;
        m.hve_mtocan
REPLACE hve_tipcam WITH w_tipcam
REPLACE hve_usuari WITH clave
REPLACE hve_fecha WITH DATE()
REPLACE hve_hora WITH TIME()
UNLOCK
STORE 0 TO x1, x2, x3, x4, x5, x6,  ;
      x7, x8, x9, x10, x11, x12,  ;
      x13, x14
x1 = 'V'
x2 = m.hve_tipdoc
x3 = m.hve_nrodoc
x5 = SUBSTR(DTOC(m.hve_fecdoc), 7,  ;
     4)
x6 = SUBSTR(DTOC(m.hve_fecdoc), 4,  ;
     2)
x7 = m.hve_codemi
x9 = '   '
x10 = STR(VAL(m.hve_codent), 11)
x11 = m.hve_codvnd
x12 = m.hve_tippag
control = 25
FOR x = 1 TO m
     SELECT gc_alm00
     SEEK producto(x) +  ;
          m.hve_almdes
     IF FOUND()
          w_almant = alm_stkfis
          DO rbloquea
          REPLACE alm_stkfis WITH  ;
                  alm_stkfis -  ;
                  cantid(x)
          REPLACE alm_stkres WITH  ;
                  alm_stkres -  ;
                  cantid(x)
          REPLACE alm_usuari WITH  ;
                  clave
          REPLACE alm_fecha WITH  ;
                  DATE()
          REPLACE alm_hora WITH  ;
                  TIME()
          UNLOCK
     ELSE
          w_almant = 0
     ENDIF
     w_presol = 0
     w_predol = 0
     w_unimed = SPACE(4)
     IF m.hve_tipdoc = 'FACT'
          IF m.hve_codmon =  ;
             'DOL '
               w_predol = preuni(x)
               w_presol = tempre(x)
          ELSE
               w_predol = tempre(x)
               w_presol = preuni(x)
          ENDIF
     ELSE
          IF m.hve_codmon =  ;
             'DOL '
               w_predol = ROUND(preuni(x) /  ;
                          w_facigv,  ;
                          2)
               w_presol = ROUND(tempre(x) /  ;
                          w_facigv,  ;
                          2)
          ELSE
               w_predol = ROUND(tempre(x) /  ;
                          w_facigv,  ;
                          2)
               w_presol = ROUND(preuni(x) /  ;
                          w_facigv,  ;
                          2)
          ENDIF
     ENDIF
     SELECT gc_pro00
     SEEK producto(x)
     IF FOUND()
          DO rbloquea
          REPLACE pro_ultven WITH  ;
                  DATE()
          REPLACE pro_ultmov WITH  ;
                  DATE()
          REPLACE pro_fecha WITH  ;
                  DATE()
          REPLACE pro_usuari WITH  ;
                  clave
          REPLACE pro_hora WITH  ;
                  TIME()
          UNLOCK
          w_proant = pro_coprmo
          w_proanb = pro_coprmb
          w_cosuni = pro_ulcomb
          w_unimed = pro_unimed
     ENDIF
     SELECT 20
     USE
     USE SHARED gc_kar00
     APPEND BLANK
     DO rbloquea
     REPLACE kar_codpro WITH  ;
             producto(x)
     REPLACE kar_fecing WITH  ;
             DATE()
     REPLACE kar_horing WITH  ;
             TIME()
     REPLACE kar_tipdoc WITH  ;
             m.hve_tipdoc
     REPLACE kar_nrodoc WITH  ;
             m.hve_nrodoc
     REPLACE kar_lindet WITH  ;
             STR(x, 2)
     REPLACE kar_fecdoc WITH  ;
             m.hve_fecdoc
     REPLACE kar_codmov WITH  ;
             'EVTA'
     REPLACE kar_unimed WITH  ;
             w_unimed
     REPLACE kar_codmon WITH  ;
             m.hve_codmon
     REPLACE kar_tipent WITH 'C'
     REPLACE kar_codent WITH  ;
             m.hve_codent
     REPLACE kar_tidore WITH  ;
             m.hve_tidore
     REPLACE kar_nrdore WITH  ;
             m.hve_nrdore
     REPLACE kar_almdes WITH  ;
             m.hve_almdes
     REPLACE kar_stkant WITH  ;
             w_almant
     REPLACE kar_cantid WITH  ;
             cantid(x)
     REPLACE kar_import WITH  ;
             w_predol
     REPLACE kar_cosant WITH  ;
             w_proant
     REPLACE kar_cosanb WITH  ;
             w_proanb
     REPLACE kar_cosuni WITH  ;
             w_cosuni
     REPLACE kar_usuari WITH  ;
             clave
     REPLACE kar_fecha WITH  ;
             DATE()
     REPLACE kar_hora WITH TIME()
     UNLOCK
     SELECT 20
     USE
     USE SHARED gc_dve00 ORDER  ;
         codigo
     APPEND BLANK
     DO rbloquea
     REPLACE dve_tipdoc WITH  ;
             m.hve_tipdoc
     REPLACE dve_nrodoc WITH  ;
             m.hve_nrodoc
     REPLACE dve_propar WITH  ;
             producto(x)
     REPLACE dve_unimed WITH  ;
             w_unimed
     REPLACE dve_cantid WITH  ;
             cantid(x)
     REPLACE dve_pordes WITH 0
     REPLACE dve_import WITH  ;
             w_predol
     REPLACE dve_total WITH  ;
             ROUND(dve_import *  ;
             cantid(x), 2)
     REPLACE dve_impors WITH  ;
             w_presol
     REPLACE dve_totals WITH  ;
             ROUND(dve_impors *  ;
             cantid(x), 2)
     REPLACE dve_coprmo WITH  ;
             w_proant
     REPLACE dve_coprmb WITH  ;
             w_proanb
     REPLACE dve_usuari WITH  ;
             clave
     REPLACE dve_fecha WITH  ;
             DATE()
     REPLACE dve_hora WITH TIME()
     UNLOCK
     w_candes = dve_total
     SELECT 20
     USE
     USE SHARED gc_nve00 ORDER  ;
         codigo
     w_fmp = 0
     SEEK producto(x)
     IF FOUND()
          w_fmp = ROUND((nve_cmes1 +  ;
                  nve_cmes2 +  ;
                  nve_cmes3 +  ;
                  nve_cmes4 +  ;
                  nve_cmes5 +  ;
                  nve_cmes6) / 6,  ;
                  2)
          w_fmp = ROUND((w_fmp *  ;
                  2), 2)
          IF cantid(x) > w_fmp
               cadena = 'nve_emes' +  ;
                        ALLTRIM(STR(MONTH(DATE())))
               DO rbloquea
               replace &cadena with &cadena+(cantid[x]-w_fmp)
               REPLACE nve_usuari  ;
                       WITH  ;
                       clave
               REPLACE nve_fecha  ;
                       WITH  ;
                       DATE()
               REPLACE nve_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
          ENDIF
     ENDIF
     x4 = x4 + 1
     x8 = producto(x)
     x13 = w_candes
     x14 = cantid(x)
     = gra_est(x1,x2,x3,x4,x5,x6, ;
       x7,x8,x9,x10,x11,x12,x13, ;
       x14)
     SELECT gc_dpv00
     SEEK m.hve_tidore +  ;
          m.hve_nrdore +  ;
          producto(x)
     IF FOUND()
          DO rbloquea
          REPLACE dpv_antres WITH  ;
                  dpv_stkres
          IF cantid(x) >  ;
             dpv_stkres
               REPLACE dpv_stkres  ;
                       WITH 0
          ELSE
               REPLACE dpv_stkres  ;
                       WITH  ;
                       dpv_stkres -  ;
                       cantid(x)
          ENDIF
          REPLACE dpv_candes WITH  ;
                  dpv_candes +  ;
                  cantid(x)
          REPLACE dpv_indest WITH  ;
                  'F'
          REPLACE dpv_fecfin WITH  ;
                  DATE()
          REPLACE dpv_horfin WITH  ;
                  TIME()
          REPLACE dpv_usuari WITH  ;
                  clave
          REPLACE dpv_fecha WITH  ;
                  DATE()
          REPLACE dpv_hora WITH  ;
                  TIME()
          UNLOCK
     ENDIF
ENDFOR
DEACTIVATE WINDOW a4
ACTIVATE WINDOW a3
w_xyz = 1
= limpia()
RETURN
*
PROCEDURE imprime
ON KEY
DO p_mensaje WITH  ;
   'Impresi¢n en Proceso. Espere......'
STORE 70 TO aux_lin
STORE 0 TO aux_sw1, aux_con1,  ;
      aux_con2, aux_tv, aux_ta
ON KEY
SET PRINTER ON
SET DEVICE TO PRINTER
IF m.hve_tipdoc = 'BOLE'
     set print to &RGE_LPTBOL
ELSE
     set print to &rge_lptfac
ENDIF
DO imprimir
SET DEVICE TO SCREEN
SET PRINTER TO
IF m.hve_almdes = rge_codalm
     DO pedido
ENDIF
RETURN
*
PROCEDURE imprimir
DO r_titulo
STORE 0 TO w_flemb, w_item,  ;
      w_precio
control = 21
FOR x = 1 TO m
     IF cantid(x) > 0
          aux_lin = aux_lin + 1
          w_item = w_item + 1
          @ aux_lin, 00 SAY  ;
            w_item PICTURE '99'
          SELECT gc_pro00
          SEEK producto(x)
          @ aux_lin, 15 SAY  ;
            SUBSTR(pro_descri, 1,  ;
            26)
          @ aux_lin, 43 SAY  ;
            ooubica(producto(x), ;
            m.hve_almdes)
          @ aux_lin, 47 SAY  ;
            cantid(x) PICTURE  ;
            '999,999'
          @ aux_lin, 55 SAY  ;
            preuni(x) PICTURE  ;
            '999,999.99'
          w_x1 = ROUND(preuni(x) *  ;
                 cantid(x), 2)
          @ aux_lin, 66 SAY w_x1  ;
            PICTURE  ;
            '999,999,999.99'
     ENDIF
ENDFOR
IF m.hve_codmon = 'DOL '
     w_flemb = m.hve_flete +  ;
               m.hve_embala
     w_cosrep = imp_subtot
     w_totdes = m.hve_totdes
     w_totnet = m.hve_totnet
     w_totvta = m.hve_totvta
     w_totigv = m.hve_totigv
     w_totimp = m.hve_totoim
     w_pagcta = pagctad
     w_simmon = ALLTRIM(sys_simomb)
ELSE
     w_flemb = m.hve_solfle +  ;
               w_solemb
     w_cosrep = imp_subsol
     w_totdes = m.hve_soldes
     w_totnet = m.hve_solnet
     w_totvta = m.hve_solvta
     w_totigv = m.hve_soligv
     w_totimp = m.hve_mtocan
     w_pagcta = pagctas
     w_simmon = ALLTRIM(sys_simmon)
ENDIF
IF m.hve_tipdoc = 'FACT'
     IF w_flemb <> 0
          @ 34, 00 SAY  ;
            'FLET/EMB:'
          @ 34, 09 SAY w_flemb  ;
            PICTURE  ;
            '999,999,999.99'
     ENDIF
     @ 35, 06 SAY w_simmon
     @ 35, 09 SAY w_cosrep  ;
       PICTURE '999,999,999.99'
     @ 36, 62 SAY w_simmon
     @ 36, 66 SAY w_totvta  ;
       PICTURE '999,999,999.99'
     @ 37, 09 SAY w_totdes  ;
       PICTURE '999,999,999.99'
     @ 37, 66 SAY w_totigv  ;
       PICTURE '999,999,999.99'
     IF pagctas > 0
          @ 38, 09 SAY w_pagcta  ;
            PICTURE  ;
            '999,999,999.99'
     ENDIF
     @ 38, 66 SAY w_totimp  ;
       PICTURE '999,999,999.99'
     @ 40, 01 SAY  ;
       oonumlet(w_totimp, ;
       m.hve_codmon)
     IF pagctas > 0
          @ 42, 40 SAY  ;
            m.hve_tidore
          @ 42, 45 SAY  ;
            m.hve_nrdore
     ENDIF
     @ 44, 63 SAY m.hve_nrodoc
ELSE
     IF w_flemb <> 0
          @ 35, 09 SAY w_flemb  ;
            PICTURE  ;
            '999,999,999.99'
     ENDIF
     @ 36, 06 SAY w_simmon
     @ 36, 09 SAY w_cosrep  ;
       PICTURE '999,999,999.99'
     @ 36, 62 SAY w_simmon
     @ 36, 66 SAY w_totimp +  ;
       w_pagcta PICTURE  ;
       '999,999,999.99'
     IF pagctas > 0
          @ 37, 66 SAY w_pagcta  ;
            PICTURE  ;
            '999,999,999.99'
     ENDIF
     @ 38, 09 SAY w_totdes  ;
       PICTURE '999,999,999.99'
     @ 38, 66 SAY w_totimp  ;
       PICTURE '999,999,999.99'
     @ 40, 01 SAY  ;
       oonumlet(w_totimp, ;
       m.hve_codmon)
     IF pagctas > 0
          @ 42, 43 SAY  ;
            m.hve_tidore
          @ 42, 48 SAY  ;
            m.hve_nrdore PICTURE  ;
            '@!'
     ENDIF
     @ 44, 63 SAY m.hve_nrodoc
ENDIF
EJECT
SET DEVICE TO SCREEN
SET PRINTER TO
SET PRINTER OFF
RETURN
*
PROCEDURE r_titulo
ON KEY
@ 00, 00 SAY CHR(27) + 'F'
@ 00, 00 SAY CHR(18)
IF m.hve_tipdoc = 'FACT'
     @ 03, 56 SAY 'R.U.C.  ' +  ;
       rge_numruc
     @ 11, 00 SAY m.hve_fecdoc  ;
       PICTURE '99/99/9999'
     @ 11, 15 SAY m.hve_codemi
     SELECT gc_cli00
     SEEK 'C' + m.hve_codent
     IF  .NOT. FOUND()
          @ 13, 00 SAY ' '
     ELSE
          @ 13, 00 SAY cli_razsoc
          @ 13, 57 SAY  ;
            m.hve_codent
          @ 15, 00 SAY cli_calle +  ;
            ' ' + cli_numero
          SELECT ge_tab0
          SEEK 'DIST' +  ;
               gc_cli00.cli_distri
          IF FOUND()
               @ 15, 32 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 25)
          ENDIF
          @ 15, 65 SAY  ;
            gc_cli00.cli_telefo
     ENDIF
     SELECT ge_tab0
     SEEK 'FPAG' + m.hve_tippag
     w_despag = SUBSTR(tab_destab,  ;
                1, 15)
     @ 17, 00 SAY m.hve_almdes
     @ 17, 12 SAY SUBSTR(w_desven,  ;
       1, 14)
     @ 17, 28 SAY w_desmon
     @ 17, 45 SAY w_despag
ELSE
     @ 02, 56 SAY 'R.U.C.  ' +  ;
       rge_numruc
     @ 11, 00 SAY m.hve_fecdoc  ;
       PICTURE '99/99/9999'
     @ 11, 15 SAY m.hve_codemi
     SELECT gc_cli00
     SEEK 'C' + m.hve_codent
     IF  .NOT. FOUND()
          @ 13, 00 SAY ' '
     ELSE
          @ 13, 00 SAY cli_razsoc
          @ 13, 57 SAY  ;
            m.hve_codent
          @ 15, 00 SAY cli_calle +  ;
            ' ' + cli_numero
          SELECT ge_tab0
          SEEK 'DIST' +  ;
               gc_cli00.cli_distri
          IF FOUND()
               @ 15, 30 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 25)
          ENDIF
          @ 15, 65 SAY  ;
            gc_cli00.cli_telefo
     ENDIF
     SELECT ge_tab0
     SEEK 'FPAG' + m.hve_tippag
     w_despag = SUBSTR(tab_destab,  ;
                1, 15)
     @ 17, 00 SAY m.hve_almdes
     @ 17, 12 SAY SUBSTR(w_desven,  ;
       1, 14)
     @ 17, 28 SAY w_desmon
     @ 17, 45 SAY w_despag
ENDIF
IF m.hve_tipdoc = 'FACT'
     aux_lin = 20
ELSE
     aux_lin = 20
ENDIF
RETURN
*
PROCEDURE pinta1
DEACTIVATE WINDOW titposs
ACTIVATE WINDOW a0
@ 00, 00 CLEAR TO 06, 74
@ 00, 00 SAY 'Tipo Doc.   :'
@ 00, 44 SAY 'Nro.     :'
@ 00, 66 SAY 'TC:'
@ 00, 69 SAY w_tipcam PICTURE  ;
  '9999.99'
@ 01, 00 SAY 'Cliente     :'
@ 01, 44 SAY 'Tel‚fono :'
@ 02, 00 SAY 'Vendedor    :'
@ 03, 00 SAY 'Emisor      :'
@ 03, 44 SAY 'Almac‚n  :'
@ 04, 00 SAY 'C¢d.Mon.    :'
@ 04, 44 SAY 'Form.Pago:'
@ 05, 00 SAY 'Tip.Doc.Ref.:'
@ 05, 44 SAY 'Nro.Ref. :'
SELECT ge_tab0
RETURN
*
PROCEDURE cartec
ON KEY
IF m = 0
     DO p_footer WITH  ;
        '101000000000100000001001',  ;
        2
ELSE
     DO p_footer WITH  ;
        '111100000000101000001001',  ;
        2
     ON KEY LABEL f2 do crea 
     ON KEY LABEL f4 do p_borra 
ENDIF
ON KEY LABEL f3 do p_lee  with 1
ON KEY LABEL f5 do modifica
RETURN
*
PROCEDURE modifica
w_mod = 1
KEYBOARD '{ESC}'
RETURN
*
PROCEDURE posi
SHOW GET arrayitem
IF LASTKEY() = 19 .OR. LASTKEY() =  ;
   4 .OR. LASTKEY() = 9
     RETURN
ENDIF
IF LASTKEY() = 24
     IF m < 8 .AND. p = m
          p = 1
     ELSE
          p = p + 1
     ENDIF
ENDIF
IF LASTKEY() = 5
     IF m < 8
          IF p = 1
               p = m
          ELSE
               p = p - 1
          ENDIF
     ELSE
          p = p - 1
     ENDIF
ENDIF
IF LASTKEY() = 6 .OR. LASTKEY() =  ;
   3
     IF m < 8
          p = m
     ELSE
          p = 7
     ENDIF
ENDIF
IF LASTKEY() = 1 .OR. LASTKEY() =  ;
   18
     p = 1
ENDIF
IF p > 7
     p = 7
ENDIF
IF p < 1
     p = 1
ENDIF
w_sale = 0
SHOW GET arrayitem
RETURN
*
PROCEDURE when2a
DO p_footer WITH  ;
   '100010000000000000001', 2
w_selec = SELECT()
w_campo = 'w_codpro'
w_codalm = m.hve_almdes
ON KEY LABEL f6 do produc2 with w_campo,w_selec,w_codalm
RETURN
*
PROCEDURE when2c
ON KEY LABEL f6
tran_ant = w_cantid
RETURN
*
FUNCTION pre_pro
PARAMETER w_codpro
w_precio = 0
SELECT gc_dlp00
SEEK m.hve_lispre + w_codpro
IF FOUND()
     IF m.hve_tipdoc = 'FACT'
          w_precio = dlp_prsigv
     ELSE
          w_precio = ROUND((dlp_prsigv *  ;
                     w_facigv),  ;
                     2)
     ENDIF
ENDIF
RETURN w_precio
*
FUNCTION vali2a
IF LASTKEY() = 19 .OR. LASTKEY() =  ;
   5
     RETURN .F.
ENDIF
SELECT gc_pro00
SET ORDER TO codigo
SEEK w_codpro
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        'Producto No Existe'
     RETURN .F.
ELSE
     IF pro_estope = 'B'
          DO p_mensaje WITH  ;
             'Producto esta bloqueado'
          RETURN .F.
     ENDIF
     w_descri = SUBSTR(pro_descri,  ;
                1, 20)
     @ 00, 16 SAY w_descri
ENDIF
RETURN
*
FUNCTION vali2b
pos1 = ASCAN(producto, w_codpro)
IF LASTKEY() = 19 .OR. LASTKEY() =  ;
   5
     RETURN
ENDIF
w_swex = 0
IF pos1 <> 0
     DO p_mensaje WITH  ;
        'El Producto Ya Existe'
     RETURN .F.
ENDIF
RETURN
*
FUNCTION vali2c
DO p_footer WITH  ;
   '100000000000000000001', 2
IF LASTKEY() = 19 .OR. LASTKEY() =  ;
   5
     RETURN .F.
ENDIF
ACTIVATE WINDOW a4
ON KEY
IF w_cantid = 0
     DO p_mensaje WITH  ;
        'Cantidad en Cero..'
     RETURN .F.
ENDIF
IF w_cantid < 0
     DO p_mensaje WITH  ;
        'Cantidad Negativa.'
     RETURN .F.
ENDIF
= transito_2(w_cantid)
IF w_mal = 1
     w_cantid = tran_ant
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE transito_2
PARAMETER can_2
SELECT gc_alm00
SEEK w_codpro + m.hve_almdes
IF FOUND()
     DO rbloquea
     IF can_2 <= (alm_stkfis -  ;
        alm_stkres + tran_ant)
          IF w_factor = 1
               IF can_2 >=  ;
                  w_canres
                    IF tran_ant <=  ;
                       w_canres
                         REPLACE alm_stkres  ;
                                 WITH  ;
                                 (alm_stkres -  ;
                                 w_canres) +  ;
                                 can_2
                    ELSE
                         REPLACE alm_stkres  ;
                                 WITH  ;
                                 (alm_stkres -  ;
                                 tran_ant) +  ;
                                 can_2
                    ENDIF
               ELSE
                    IF tran_ant >=  ;
                       w_canres
                         REPLACE alm_stkres  ;
                                 WITH  ;
                                 (alm_stkres -  ;
                                 tran_ant) +  ;
                                 w_canres
                    ELSE
                         REPLACE alm_stkres  ;
                                 WITH  ;
                                 alm_stkres
                    ENDIF
               ENDIF
          ELSE
               REPLACE alm_stkres  ;
                       WITH  ;
                       (alm_stkres -  ;
                       tran_ant) +  ;
                       can_2
          ENDIF
          REPLACE alm_usuari WITH  ;
                  clave
          REPLACE alm_fecha WITH  ;
                  DATE()
          REPLACE alm_hora WITH  ;
                  TIME()
          w_mal = 0
     ELSE
          DO p_mensaje WITH  ;
             'Cantidad Solicitada Mayor que Stock de Almacen '
          w_mal = 1
     ENDIF
     UNLOCK
ELSE
     DO p_mensaje WITH  ;
        'Producto No Existe En El Almac‚n'
     w_mal = 1
ENDIF
RETURN
*
FUNCTION vali3ca
PARAMETER opc
IF m.hve_pordes < 0
     DO p_mensaje WITH  ;
        'El Porcentaje De Descuento No Puede Ser Negativo'
     RETURN .F.
ENDIF
IF m.hve_pordes > rge_pormax  ;
   .AND. xnnn <> 'A7' .AND. xnnn <>  ;
   'A5'
     DO p_mensaje WITH  ;
        'El Descuento es Mayor que el Definido en los Parametros'
     RETURN .F.
ENDIF
IF m.hve_pordes > 0
     IF opc = 1
          m.hve_totdes = ROUND((m.hve_totnet *  ;
                         m.hve_pordes) /  ;
                         100, 2)
          m.hve_soldes = ROUND(m.hve_totdes *  ;
                         w_tipcam,  ;
                         2)
     ELSE
          m.hve_soldes = ROUND((m.hve_solnet *  ;
                         m.hve_pordes) /  ;
                         100, 2)
          m.hve_totdes = ROUND(m.hve_soldes /  ;
                         w_tipcam,  ;
                         2)
     ENDIF
ENDIF
RETURN
*
FUNCTION vali3c
PARAMETER opc
IF opc = 1
     w_pordes = ROUND((m.hve_totdes /  ;
                m.hve_totnet) *  ;
                100, 2)
ELSE
     w_pordes = ROUND((m.hve_soldes /  ;
                m.hve_solnet) *  ;
                100, 2)
ENDIF
IF w_pordes > rge_pormax .AND.  ;
   xnnn <> 'A7' .AND. xnnn <>  ;
   'A5'
     DO p_mensaje WITH  ;
        'El Descuento es Mayor que el Definido en los Parametros'
     RETURN .F.
ENDIF
IF (m.hve_totdes > m.hve_totgen)  ;
   .OR. (m.hve_soldes >  ;
   m.hve_solnet)
     DO p_mensaje WITH  ;
        'Ojo el Monto excede del 100 %, el resultado puede ser Negativo'
     RETURN .F.
ENDIF
IF m.hve_totdes <> 0 .OR.  ;
   m.hve_soldes <> 0
     IF opc = 1
          m.hve_pordes = ROUND((m.hve_totdes /  ;
                         m.hve_totnet) *  ;
                         100, 2)
          m.hve_soldes = ROUND(m.hve_totdes *  ;
                         w_tipcam,  ;
                         2)
     ELSE
          m.hve_pordes = ROUND((m.hve_soldes /  ;
                         m.hve_solnet) *  ;
                         100, 2)
          m.hve_totdes = ROUND(m.hve_soldes /  ;
                         w_tipcam,  ;
                         2)
     ENDIF
ENDIF
IF opc = 1
     @ 4, 21 SAY m.hve_pordes  ;
       PICTURE '999.99'
ELSE
     @ 4, 35 SAY m.hve_pordes  ;
       PICTURE '999.99'
ENDIF
m.hve_totgen = m.hve_totnet -  ;
               m.hve_totdes
m.hve_totoim = m.hve_totgen -  ;
               pagctad
m.hve_totvta = ROUND((m.hve_totoim /  ;
               w_facigv), 2)
m.hve_totigv = m.hve_totoim -  ;
               m.hve_totvta
m.hve_solgen = m.hve_solnet -  ;
               m.hve_soldes
m.hve_mtocan = m.hve_solgen -  ;
               pagctas
m.hve_solvta = ROUND((m.hve_mtocan /  ;
               w_facigv), 2)
m.hve_soligv = m.hve_mtocan -  ;
               m.hve_solvta
@ 3, 14 SAY m.hve_totnet PICTURE  ;
  '99,999,999.99'
@ 5, 14 SAY m.hve_totdes PICTURE  ;
  '99,999,999.99'
@ 7, 14 SAY m.hve_totvta PICTURE  ;
  '99,999,999.99'
@ 8, 14 SAY m.hve_totigv PICTURE  ;
  '99,999,999.99'
@ 9, 14 SAY m.hve_totoim PICTURE  ;
  '99,999,999.99'
@ 3, 28 SAY m.hve_solnet PICTURE  ;
  '99,999,999.99'
@ 5, 28 SAY m.hve_soldes PICTURE  ;
  '99,999,999.99'
@ 7, 28 SAY m.hve_solvta PICTURE  ;
  '99,999,999.99'
@ 8, 28 SAY m.hve_soligv PICTURE  ;
  '99,999,999.99'
@ 9, 28 SAY m.hve_mtocan PICTURE  ;
  '99,999,999.99'
DO p_footer WITH  ;
   '110000000000000000001', 2
ACTIVATE WINDOW gasto
= INKEY(0, 'H')
IF LASTKEY() = -1
     CLEAR READ
ELSE
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW gasto
ENDIF
RETURN
*
PROCEDURE vali3d
PARAMETER opc
IF opc = 1
     m.hve_solfle = ROUND(m.hve_flete *  ;
                    w_tipcam, 2)
     w_solemb = ROUND(m.hve_embala *  ;
                w_tipcam, 2)
ELSE
     m.hve_flete = ROUND(m.hve_solfle /  ;
                   w_tipcam, 2)
     m.hve_embala = ROUND(w_solemb /  ;
                    w_tipcam, 2)
ENDIF
m.hve_totnet = imp_subtot +  ;
               m.hve_flete +  ;
               m.hve_embala +  ;
               m.hve_cargo
m.hve_totgen = m.hve_totnet -  ;
               m.hve_totdes
m.hve_totoim = m.hve_totgen -  ;
               pagctad
m.hve_totvta = ROUND((m.hve_totoim /  ;
               w_facigv), 2)
m.hve_totigv = m.hve_totoim -  ;
               m.hve_totvta
m.hve_solnet = imp_subsol +  ;
               m.hve_solfle +  ;
               w_solemb +  ;
               ROUND(m.hve_cargo *  ;
               w_tipcam, 2)
m.hve_solgen = ROUND((m.hve_solnet -  ;
               m.hve_soldes), 2)
m.hve_mtocan = ROUND((m.hve_solgen -  ;
               pagctas), 2)
m.hve_solvta = ROUND((m.hve_mtocan /  ;
               w_facigv), 2)
m.hve_soligv = m.hve_mtocan -  ;
               m.hve_solvta
@ 3, 14 SAY m.hve_totnet PICTURE  ;
  '99,999,999.99'
@ 5, 14 SAY m.hve_totdes PICTURE  ;
  '99,999,999.99'
@ 7, 14 SAY m.hve_totvta PICTURE  ;
  '99,999,999.99'
@ 8, 14 SAY m.hve_totigv PICTURE  ;
  '99,999,999.99'
@ 9, 14 SAY m.hve_totoim PICTURE  ;
  '99,999,999.99'
@ 3, 28 SAY m.hve_solnet PICTURE  ;
  '99,999,999.99'
@ 5, 28 SAY m.hve_soldes PICTURE  ;
  '99,999,999.99'
@ 7, 28 SAY m.hve_solvta PICTURE  ;
  '99,999,999.99'
@ 8, 28 SAY m.hve_soligv PICTURE  ;
  '99,999,999.99'
@ 9, 28 SAY m.hve_mtocan PICTURE  ;
  '99,999,999.99'
DO p_footer WITH  ;
   '110000000000000000001', 2
ACTIVATE WINDOW gasto
= INKEY(0, 'H')
IF LASTKEY() = -1
     CLEAR READ
ELSE
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW gasto
ENDIF
RETURN
*
PROCEDURE gastos
ON KEY
STORE 0 TO imp_subtot, totnet
STORE 0 TO w_solemb, imp_subsol,  ;
      subtot
STORE 0 TO m.hve_flete,  ;
      m.hve_solfle, m.hve_embala,  ;
      m.hve_pordes, m.hve_totdes,  ;
      m.hve_soldes
DEFINE WINDOW gasto FROM 09, 20  ;
       TO 20, 63 IN screen
DO p_footer WITH  ;
   '100000000000000000001', 2
ACTIVATE WINDOW gasto
@ 0, 00 SAY  ;
  'T O T A L E S         US$.          S/.   '
@ 1, 00 SAY 'Flete .......:'
@ 2, 00 SAY 'Embalaje.....:'
@ 3, 00 SAY 'Sub Total+igv:'
@ 4, 00 SAY '% Descuento..:'
@ 5, 00 SAY 'Descuento....:'
@ 6, 00 SAY 'Pago a Cta...:'
@ 7, 00 SAY 'Valor venta..:'
@ 8, 00 SAY 'Igv..........:'
@ 9, 00 SAY 'Saldo a Pagar:'
FOR y = 1 TO m
     IF m.hve_codmon = 'DOL '
          imp_subtot = imp_subtot +  ;
                       ROUND(preuni(y) *  ;
                       cantid(y),  ;
                       2)
          imp_subsol = imp_subsol +  ;
                       ROUND(tempre(y) *  ;
                       cantid(y),  ;
                       2)
     ELSE
          imp_subtot = imp_subtot +  ;
                       ROUND(tempre(y) *  ;
                       cantid(y),  ;
                       2)
          imp_subsol = imp_subsol +  ;
                       ROUND(preuni(y) *  ;
                       cantid(y),  ;
                       2)
     ENDIF
ENDFOR
IF m.hve_tipdoc = 'FACT'
     imp_subtot = ROUND((imp_subtot *  ;
                  w_facigv), 2)
     imp_subsol = ROUND((imp_subsol *  ;
                  w_facigv), 2)
ENDIF
subtot = imp_subtot
totnet = imp_subtot
m.hve_totnet = imp_subtot
m.hve_totgen = ROUND((m.hve_totnet -  ;
               m.hve_totdes), 2)
m.hve_totoim = ROUND((m.hve_totgen -  ;
               pagctad), 2)
m.hve_totvta = ROUND((m.hve_totoim /  ;
               w_facigv), 2)
m.hve_totigv = m.hve_totoim -  ;
               m.hve_totvta
m.hve_solnet = imp_subsol
m.hve_solgen = ROUND((m.hve_solnet -  ;
               m.hve_soldes), 2)
m.hve_mtocan = ROUND((m.hve_solgen -  ;
               pagctas), 2)
m.hve_solvta = ROUND((m.hve_mtocan /  ;
               w_facigv), 2)
m.hve_soligv = m.hve_mtocan -  ;
               m.hve_solvta
IF m.hve_codmon = 'DOL '
     m.hve_totdes = ROUND((m.hve_totnet *  ;
                    m.hve_pordes) /  ;
                    100, 2)
     @ 1, 14 GET m.hve_flete  ;
       PICTURE '99,999,999.99'  ;
       VALID m.hve_flete >= 0
     @ 2, 14 GET m.hve_embala  ;
       PICTURE '99,999,999.99'  ;
       VALID m.hve_embala >= 0  ;
       .AND. vali3d(1)
     @ 3, 14 SAY m.hve_totnet  ;
       PICTURE '99,999,999.99'
     @ 4, 21 GET m.hve_pordes  ;
       PICTURE '999.99' VALID  ;
       vali3ca(1)
     @ 5, 14 GET m.hve_totdes  ;
       PICTURE '99,999,999.99'  ;
       VALID m.hve_totdes >= 0  ;
       .AND. vali3c(1)
ELSE
     w_solemb = ROUND(m.hve_embala *  ;
                w_tipcam, 2)
     m.hve_soldes = ROUND((m.hve_solnet *  ;
                    m.hve_pordes) /  ;
                    100, 2)
     @ 1, 28 GET m.hve_solfle  ;
       PICTURE '99,999,999.99'  ;
       VALID m.hve_solfle >= 0
     @ 2, 28 GET w_solemb PICTURE  ;
       '99,999,999.99' VALID  ;
       w_solemb >= 0 .AND.  ;
       vali3d(2)
     @ 3, 28 SAY imp_subsol  ;
       PICTURE '99,999,999.99'
     @ 4, 35 GET m.hve_pordes  ;
       PICTURE '999.99' VALID  ;
       vali3ca(2)
     @ 5, 28 GET m.hve_soldes  ;
       PICTURE '99,999,999.99'  ;
       VALID m.hve_soldes >= 0  ;
       .AND. vali3c(2)
ENDIF
@ 3, 14 SAY m.hve_totnet PICTURE  ;
  '99,999,999.99'
@ 5, 14 SAY m.hve_totdes PICTURE  ;
  '99,999,999.99'
@ 6, 14 SAY pagctad PICTURE  ;
  '99,999,999.99'
@ 7, 14 SAY m.hve_totvta PICTURE  ;
  '99,999,999.99'
@ 8, 14 SAY m.hve_totigv PICTURE  ;
  '99,999,999.99'
@ 9, 14 SAY m.hve_totoim PICTURE  ;
  '99,999,999.99'
@ 3, 28 SAY m.hve_solnet PICTURE  ;
  '99,999,999.99'
@ 5, 28 SAY m.hve_soldes PICTURE  ;
  '99,999,999.99'
@ 6, 28 SAY pagctas PICTURE  ;
  '99,999,999.99'
@ 7, 28 SAY m.hve_solvta PICTURE  ;
  '99,999,999.99'
@ 8, 28 SAY m.hve_soligv PICTURE  ;
  '99,999,999.99'
@ 9, 28 SAY m.hve_mtocan PICTURE  ;
  '99,999,999.99'
READ CYCLE
DEACTIVATE WINDOW gasto
ACTIVATE WINDOW a3
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen
ON KEY LABEL esc
DO CASE
     CASE cwhen = 'HVE_TIPDOC'
          w_busca = 'DOCU'
          w_var = 'M.HVE_TIPDOC'
          ON KEY LABEL F6 do busca with;
w_busca,w_var 
          STORE DATE() TO  ;
                m.hve_fecdoc,  ;
                m.hve_fecvct,  ;
                m.hve_fecent
     CASE cwhen = 'HVE_NRODOC'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
     CASE cwhen = 'HVE_CODENT'  ;
          .OR. cwhen =  ;
          'HVE_CODVND'
          DO CASE
               CASE cwhen =  ;
                    'HVE_CODENT'
                    w_busca = 'C'
                    w_var = 'M.HVE_CODENT'
               OTHERWISE
                    w_busca = 'V'
                    w_var = 'M.HVE_CODVND'
          ENDCASE
          ON KEY LABEL f6 do entidad with;
w_busca,w_var            
     CASE cwhen = 'HVE_CODEMI'
          w_busca = 'EMIS'
          w_var = 'M.HVE_CODEMI'
          ON KEY LABEL F6 do busca with;
w_busca,w_var 
     CASE cwhen = 'HVE_ALMDES'
          w_busca = 'ALMA'
          w_var = 'M.HVE_ALMDES'
          ON KEY LABEL f6 do busca with;
w_busca,w_var
     CASE cwhen = 'HVE_CODMON'
          w_busca = 'MONE'
          w_var = 'M.HVE_CODMON'
          ON KEY LABEL f6 do busca with;
w_busca,w_var
     CASE cwhen = 'HVE_TIPPAG'
          w_busca = 'FPAG'
          w_var = 'M.HVE_TIPPAG'
          ON KEY LABEL f6 do busca with;
w_busca,w_var
     CASE cwhen = 'HVE_TIDORE'
          w_busca = 'DOCU'
          w_var = 'M.HVE_TIDORE'
          ON KEY LABEL f6 do busca with;
w_busca,w_var
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
     CASE cwhen = 'HVE_NRDORE'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'HVE_TIPDOC'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN .F.
          ENDIF
          w_busca = 'DOCU'
          SELECT ge_tab0
          SEEK w_busca +  ;
               m.hve_tipdoc
          @ 0, 17 SAY REPLICATE( ;
            ' ', 24)
          IF FOUND()
               IF m.hve_tipdoc =  ;
                  'FACT' .OR.  ;
                  m.hve_tipdoc =  ;
                  'BOLE' .OR.  ;
                  m.hve_tipdoc =  ;
                  'LIQU' .OR.  ;
                  m.hve_tipdoc =  ;
                  'GUIA'
                    @ 0, 23 SAY  ;
                      SUBSTR(ALLTRIM(ge_tab0.tab_destab),  ;
                      1, 24)  ;
                      COLOR W+/N 
               ELSE
                    DO p_mensaje  ;
                       WITH  ;
                       ' Tipo de documento no valido '
                    RETURN .F.
               ENDIF
          ELSE
               DO p_mensaje WITH  ;
                  ' Tipo De Documento No Existe '
               RETURN .F.
          ENDIF
          IF w_opcion = 1
               CLEAR READ
          ENDIF
          IF w_opcion = 2 .AND.  ;
             m.hve_tipdoc <>  ;
             'LIQU'
               @ 0, 54 GET  ;
                 m.hve_nrodoc  ;
                 DEFAULT 0 SIZE 1,  ;
                 10 PICTURE  ;
                 '@ 9999999999'  ;
                 VALID  ;
                 oovalid(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())
               READ
          ENDIF
     CASE cvalid = 'HVE_NRODOC'
          IF  .NOT.  ;
              EMPTY(m.hve_nrodoc)
               SELECT gc_hve00
               m.hve_nrodoc = f_ceros(m.hve_nrodoc, ;
                              10, ;
                              2)
               SEEK m.hve_tipdoc +  ;
                    m.hve_nrodoc
               IF FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       ' N£mero de Documento ya Existe '
                    RETURN .F.
               ENDIF
          ELSE
               DO p_mensaje WITH  ;
                  ' Es Necesario N£mero de Documento '
               RETURN .F.
          ENDIF
          @ 0, 54 SAY  ;
            m.hve_nrodoc PICTURE  ;
            '@ 9999999999'
          CLEAR READ
     CASE cvalid = 'HVE_CODENT'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN .F.
          ENDIF
          IF w_codent <>  ;
             m.hve_codent .AND.  ;
             w_mod = 1 .AND.  ;
             w_paso = 1
               DO p_mensaje WITH  ;
                  ' No se puede cambiar Cilente si tiene Doc.Ref. '
               RETURN .F.
          ENDIF
          w_busca = 'C'
          SELECT gc_cli00
          SET ORDER TO codigo
          SEEK w_busca +  ;
               m.hve_codent
          @ 1, 25 SAY REPLICATE( ;
            ' ', 19)
          @ 1, 54 SAY REPLICATE( ;
            ' ', 8)
          IF FOUND()
               w_telefo = gc_cli00.cli_telefo
               @ 1, 25 SAY  ;
                 SUBSTR(gc_cli00.cli_razsoc,  ;
                 1, 18) COLOR W+/ ;
                 N 
               @ 1, 54 SAY  ;
                 w_telefo
          ELSE
               swt = f_yesno( ;
                     ' Cliente no Existe, Desea ingresarlo ' ;
                     )
               IF swt
                    SCATTER BLANK  ;
                            MEMVAR
                    m.cli_tpper =  ;
                     'C'
                    m.cli_codigo =  ;
                     hve_codent
                    IF  .NOT.  ;
                        EMPTY(m.cli_codigo)
                         = ooclientes()
                    ENDIF
               ENDIF
               RETURN .F.
          ENDIF
          w_codent = m.hve_codent
     CASE cvalid = 'HVE_CODVND'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          w_busca = 'V'
          SELECT gc_vnd00
          SET ORDER TO codigo
          SEEK w_busca +  ;
               m.hve_codvnd
          @ 2, 23 SAY REPLICATE( ;
            ' ', 20)
          IF FOUND()
               w_desven = SUBSTR(ALLTRIM(gc_vnd00.vnd_nombre),  ;
                          1, 18)
               @ 2, 23 SAY  ;
                 SUBSTR(ALLTRIM(gc_vnd00.vnd_nombre),  ;
                 1, 20) COLOR W+/ ;
                 N 
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo de vendedor no existe '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_CODEMI'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          w_busca = 'EMIS'
          SELECT ge_tab0
          SEEK w_busca +  ;
               m.hve_codemi
          @ 3, 23 SAY REPLICATE( ;
            ' ', 20)
          IF FOUND()
               @ 3, 23 SAY  ;
                 SUBSTR(ALLTRIM(ge_tab0.tab_destab),  ;
                 1, 20) COLOR W+/ ;
                 N 
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo de emisor No Existe '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_ALMDES'
          IF w_almdes <>  ;
             m.hve_almdes .AND.  ;
             w_mod = 1
               DO p_mensaje WITH  ;
                  ' No se puede cambiar Almac‚n,si es Modificaci¢n '
               RETURN .F.
          ENDIF
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          w_busca = 'ALMA'
          SELECT ge_tab0
          SEEK w_busca +  ;
               m.hve_almdes
          @ 3, 59 SAY REPLICATE( ;
            ' ', 15)
          IF FOUND()
               @ 3, 59 SAY  ;
                 SUBSTR(ALLTRIM(ge_tab0.tab_destab),  ;
                 1, 15) COLOR W+/ ;
                 N 
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo de Almac‚n No Existe '
               RETURN .F.
          ENDIF
          w_almdes = m.hve_almdes
     CASE cvalid = 'HVE_CODMON'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          w_busca = 'MONE'
          SELECT ge_tab0
          SEEK w_busca +  ;
               m.hve_codmon
          @ 4, 23 SAY REPLICATE( ;
            ' ', 15)
          IF FOUND()
               @ 4, 23 SAY  ;
                 SUBSTR(ALLTRIM(ge_tab0.tab_destab),  ;
                 1, 15) COLOR W+/ ;
                 N 
               w_desmon = ALLTRIM(tab_destab)
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo de Moneda No Existe '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_TIPPAG'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          w_busca = 'FPAG'
          SELECT ge_tab0
          SEEK w_busca +  ;
               m.hve_tippag
          @ 4, 59 SAY REPLICATE( ;
            ' ', 15)
          IF FOUND()
               @ 4, 59 SAY  ;
                 SUBSTR(ALLTRIM(ge_tab0.tab_destab),  ;
                 1, 15) COLOR W+/ ;
                 N 
          ELSE
               DO p_mensaje WITH  ;
                  'Forma de Pago No Existe '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_LISPRE'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          SELECT gc_hlp00
          SEEK m.hve_lispre
          @ 4, 59 SAY REPLICATE( ;
            ' ', 15)
          IF FOUND()
               @ 4, 59 SAY  ;
                 SUBSTR(ALLTRIM(gc_hlp00.hlp_deslis),  ;
                 1, 15) COLOR W+/ ;
                 N 
               w_lismon = hlp_molipr
          ELSE
               DO p_mensaje WITH  ;
                  ' Lista de Precios No Existe '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_FECDOC'
          IF w_tipcam = -1
               DO p_mensaje WITH  ;
                  'No Existe Tipo De Cambio'
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_TIDORE'
          IF w_tidore <>  ;
             m.hve_tidore .AND.  ;
             w_mod = 1
               DO p_mensaje WITH  ;
                  ' No se puede cambiar Tipo. Doc. Ref.,si es Modificaci¢n '
               RETURN .F.
          ENDIF
          @ 5, 23 SAY REPLICATE( ;
            ' ', 20)
          IF m.hve_tidore =  ;
             SPACE(4)
               RETURN .T.
          ENDIF
          w_busca = 'DOCU'
          SELECT ge_tab0
          SEEK w_busca +  ;
               m.hve_tidore
          IF FOUND()
               @ 5, 23 SAY  ;
                 SUBSTR(ALLTRIM(ge_tab0.tab_destab),  ;
                 1, 20) COLOR W+/ ;
                 N 
          ELSE
               DO p_mensaje WITH  ;
                  ' Documento No Existe '
               RETURN .F.
          ENDIF
          w_tidore = m.hve_tidore
     CASE cvalid = 'HVE_NRDORE'
          IF w_nrdore <>  ;
             m.hve_nrdore .AND.  ;
             w_mod = 1
               DO p_mensaje WITH  ;
                  ' No se puede cambiar Doc. Ref.,si es Modificaci¢n '
               RETURN .F.
          ENDIF
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          IF  .NOT.  ;
              EMPTY(m.hve_tidore)  ;
              .AND.  ;
              EMPTY(m.hve_nrdore)
               DO p_mensaje WITH  ;
                  'Ingresar N£mero de Documento'
               RETURN .F.
          ENDIF
          IF  .NOT.  ;
              EMPTY(m.hve_tidore)  ;
              .AND.  .NOT.  ;
              EMPTY(m.hve_nrdore)
               m.hve_nrdore = f_ceros(m.hve_nrdore, ;
                              10, ;
                              2)
               @ 5, 54 SAY  ;
                 m.hve_nrdore
               SELECT gc_hve00
               SEEK m.hve_tidore +  ;
                    m.hve_nrdore
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       ' Doc. Ref. No Existe '
                    RETURN .F.
               ELSE
                    w_ped = 0
                    w_flag = 0
                    IF hve_estdoc =  ;
                       'A'
                         DO p_mensaje  ;
                            WITH  ;
                            ' Doc. Ref. Esta Anulado '
                         RETURN .F.
                    ELSE
                         SELECT 21
                         USE SHARED  ;
                             gc_dpv00  ;
                             ORDER  ;
                             codigo
                         SEEK m.hve_tidore +  ;
                              m.hve_nrdore
                         IF FOUND()
                              w_ped =  ;
                               0
                              SCAN  ;
                               WHILE  ;
                               dpv_tipdoc =  ;
                               m.hve_tidore  ;
                               .AND.  ;
                               dpv_nrodoc =  ;
                               m.hve_nrdore  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF()
                                   w_cantid = dpv_cantid - dpv_candes
                                   IF w_cantid > 0
                                        w_ped = w_ped + 1
                                        SELECT gc_alm00
                                        SEEK gc_dpv00.dpv_codpro + m.hve_almdes
                                        IF FOUND()
                                             IF gc_dpv00.dpv_stkres > 0
                                                  IF w_cantid >= gc_dpv00.dpv_stkres
                                                       w_flag = 1
                                                       EXIT
                                                  ENDIF
                                             ELSE
                                                  IF (alm_stkfis - alm_stkres) > 0
                                                       w_flag = 1
                                                       EXIT
                                                  ENDIF
                                             ENDIF
                                        ENDIF
                                        SELECT gc_dpv00
                                   ENDIF
                              ENDSCAN
                         ENDIF
                         IF w_ped =  ;
                            0
                              DO p_mensaje  ;
                                 WITH  ;
                                 ' Doc. Ref. No Tiene Pendientes '
                              RETURN  ;
                               .F.
                         ELSE
                              IF w_flag =  ;
                                 0
                                   DO p_mensaje WITH ' No Hay Stock Para Repuestos de Doc. Ref.'
                                   RETURN .F.
                              ENDIF
                         ENDIF
                    ENDIF
                    SELECT gc_hve00
                    IF hve_codmov <>  ;
                       'PCTA'  ;
                       .OR.  ;
                       hve_indori <>  ;
                       'VENT'
                         DO p_mensaje  ;
                            WITH  ;
                            ' Doc. Ref. No es Pago A Cuenta de Venta'
                         RETURN .F.
                    ENDIF
                    IF hve_codent <>  ;
                       m.hve_codent
                         DO p_mensaje  ;
                            WITH  ;
                            'No Es El Mismo Cliente'
                         RETURN .F.
                    ENDIF
                    IF hve_estdoc =  ;
                       'V'
                         pagctas =  ;
                          hve_solgen
                         pagctad =  ;
                          hve_totgen
                         @ 02, 44  ;
                           SAY  ;
                           'Pag.Cta  :'
                         IF m.hve_codmon =  ;
                            'DOL '
                              @ 02,  ;
                                54  ;
                                SAY  ;
                                pagctad  ;
                                PICTURE  ;
                                '$$$,$$$,$$$.99'  ;
                                COLOR  ;
                                N/ ;
                                W 
                         ELSE
                              @ 02,  ;
                                54  ;
                                SAY  ;
                                pagctas  ;
                                PICTURE  ;
                                '999,999,999.99'  ;
                                COLOR  ;
                                N/ ;
                                W 
                         ENDIF
                    ELSE
                         pagctas =  ;
                          0
                         pagctad =  ;
                          0
                    ENDIF
                    w_paso = 1
                    IF w_mod = 0
                         DO carmat
                    ENDIF
               ENDIF
          ENDIF
          IF w_mod = 1 .AND.  ;
             w_codmon <>  ;
             m.hve_codmon .OR.  ;
             w_tipdoc <>  ;
             m.hve_tipdoc
               FOR x = 1 TO m
                    w_tem = pre_pro(producto(x))
                    IF m.hve_codmon =  ;
                       'DOL '
                         w_preuni =  ;
                          w_tem
                         w_tempre =  ;
                          ROUND(w_preuni *  ;
                          w_tipcam,  ;
                          2)
                    ELSE
                         w_preuni =  ;
                          ROUND(w_tem *  ;
                          w_tipcam,  ;
                          2)
                         w_tempre =  ;
                          w_tem
                    ENDIF
                    preuni[ x] =  ;
                          w_preuni
                    tempre[ x] =  ;
                          w_tempre
                    w_candes = ROUND(cantid(x) *  ;
                               w_preuni,  ;
                               2)
                    deta[ x] =  ;
                        producto(x) +  ;
                        '³' +  ;
                        descri(x) +  ;
                        '³' +  ;
                        TRANSFORM(cantid(x),  ;
                        '@ 9,999,999' ;
                        ) + '³' +  ;
                        TRANSFORM(preuni(x),  ;
                        '@ 99,999,999.99' ;
                        ) + '³' +  ;
                        TRANSFORM(w_candes,  ;
                        '@ 999,999,999.99' ;
                        )
                    arrayitem = producto(x) +  ;
                                '³' +  ;
                                descri(x) +  ;
                                '³' +  ;
                                TRANSFORM(cantid(x),  ;
                                '@ 9,999,999' ;
                                ) +  ;
                                '³' +  ;
                                TRANSFORM(preuni(x),  ;
                                '@ 99,999,999.99' ;
                                ) +  ;
                                '³' +  ;
                                TRANSFORM(w_candes,  ;
                                '@ 999,999,999.99' ;
                                )
               ENDFOR
          ENDIF
          ON KEY LABEL f6 wait window;
"" nowait
          w_fin = 0
          w_nrdore = m.hve_nrdore
          w_codmon = m.hve_codmon
          w_tipdoc = m.hve_tipdoc
          CLEAR READ
ENDCASE
RETURN
*
PROCEDURE carmat
STORE 0 TO m, w_cantid, w_stkres,  ;
      w_sig, w_factor
SELECT 20
USE
USE SHARED gc_hpv00 ORDER codigo
SEEK m.hve_tidore + m.hve_nrdore
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        ' Doc. Ref. No Tiene Repuestos'
     RETURN
ELSE
     SELECT 21
     USE SHARED gc_dpv00 ORDER  ;
         codigo
     SEEK m.hve_tidore +  ;
          m.hve_nrdore
     IF FOUND()
          w_ref = 1
          STORE 0 TO nro_pro, m,  ;
                pos
          SCAN WHILE dpv_tipdoc =  ;
               m.hve_tidore .AND.  ;
               dpv_nrodoc =  ;
               m.hve_nrdore .AND.   ;
               .NOT. EOF()
               w_sig = 0
               w_factor = 0
               SELECT gc_alm00
               SEEK gc_dpv00.dpv_codpro +  ;
                    m.hve_almdes
               IF FOUND()
                    w_stkres = alm_stkres
                    w_cantid = gc_dpv00.dpv_cantid -  ;
                               gc_dpv00.dpv_candes
                    IF w_cantid >  ;
                       0
                         IF gc_dpv00.dpv_stkres >  ;
                            0
                              IF w_cantid >  ;
                                 gc_dpv00.dpv_stkres
                                   IF (alm_stkfis - alm_stkres) >= (w_cantid - gc_dpv00.dpv_stkres)
                                        w_stkres = alm_stkres + (w_cantid - gc_dpv00.dpv_stkres)
                                        w_factor = 1
                                   ELSE
                                        w_cantid = gc_dpv00.dpv_stkres
                                   ENDIF
                              ENDIF
                              w_sig =  ;
                               1
                         ELSE
                              IF (alm_stkfis -  ;
                                 alm_stkres) >  ;
                                 0
                                   IF (alm_stkfis - alm_stkres) >= w_cantid
                                        w_stkres = alm_stkres + w_cantid
                                   ELSE
                                        w_cantid = (alm_stkfis - alm_stkres)
                                        w_stkres = alm_stkres + w_cantid
                                   ENDIF
                                   w_sig = 1
                                   w_factor = 1
                              ENDIF
                         ENDIF
                         IF w_sig =  ;
                            1
                              nro_pro =  ;
                               nro_pro +  ;
                               1
                              m =  ;
                               m +  ;
                               1
                              DIMENSION  ;
                               deta[  ;
                               m]
                              DIMENSION  ;
                               producto[  ;
                               m]
                              DIMENSION  ;
                               descri[  ;
                               m]
                              DIMENSION  ;
                               cantid[  ;
                               m]
                              DIMENSION  ;
                               preuni[  ;
                               m]
                              DIMENSION  ;
                               tempre[  ;
                               m]
                              DIMENSION  ;
                               candes[  ;
                               m]
                              DIMENSION  ;
                               canres[  ;
                               m]
                              DIMENSION  ;
                               factor[  ;
                               m]
                              producto[  ;
                               m] =  ;
                               gc_dpv00.dpv_codpro
                              cantid[  ;
                               m] =  ;
                               w_cantid
                              w_tem =  ;
                               pre_pro(producto(m))
                              IF m.hve_codmon =  ;
                                 'DOL '
                                   preuni[ m] = w_tem
                                   tempre[ m] = ROUND(w_tem * w_tipcam, 2)
                              ELSE
                                   preuni[ m] = ROUND(w_tem * w_tipcam, 2)
                                   tempre[ m] = w_tem
                              ENDIF
                              candes[  ;
                               m] =  ;
                               ROUND(cantid(m) *  ;
                               preuni(m),  ;
                               2)
                              canres[  ;
                               m] =  ;
                               gc_dpv00.dpv_stkres
                              factor[  ;
                               m] =  ;
                               1
                              SELECT  ;
                               gc_pro00
                              SEEK  ;
                               producto(m)
                              descri[  ;
                               m] =  ;
                               SUBSTR(pro_descri,  ;
                               1,  ;
                               20)
                              deta[  ;
                               m] =  ;
                               producto(m) +  ;
                               '³' +  ;
                               descri(m) +  ;
                               '³' +  ;
                               TRANSFORM(cantid(m),  ;
                               '@ 9,999,999' ;
                               ) +  ;
                               '³' +  ;
                               TRANSFORM(preuni(m),  ;
                               '@ 99,999,999.99' ;
                               ) +  ;
                               '³' +  ;
                               TRANSFORM(candes(m),  ;
                               '@ 999,999,999.99' ;
                               )
                              arrayitem =  ;
                               producto(m) +  ;
                               '³' +  ;
                               descri(m) +  ;
                               '³' +  ;
                               TRANSFORM(cantid(m),  ;
                               '@ 9,999,999' ;
                               ) +  ;
                               '³' +  ;
                               TRANSFORM(preuni(m),  ;
                               '@ 99,999,999.99' ;
                               ) +  ;
                               '³' +  ;
                               TRANSFORM(candes(m),  ;
                               '@ 999,999,999.99' ;
                               )
                              p =  ;
                               m
                              pos =  ;
                               m
                              SELECT  ;
                               gc_alm00
                              DO rbloquea
                              REPLACE  ;
                               alm_stkres  ;
                               WITH  ;
                               w_stkres
                              REPLACE  ;
                               alm_usuari  ;
                               WITH  ;
                               clave
                              REPLACE  ;
                               alm_fecha  ;
                               WITH  ;
                               DATE()
                              REPLACE  ;
                               alm_hora  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                         ENDIF
                    ENDIF
               ENDIF
               SELECT gc_dpv00
          ENDSCAN
     ENDIF
ENDIF
RETURN
*
PROCEDURE limpia
@ 0, 17 SAY REPLICATE(' ', 24)
@ 1, 25 SAY REPLICATE(' ', 40)
@ 2, 25 SAY REPLICATE(' ', 24)
@ 3, 17 SAY REPLICATE(' ', 24)
@ 3, 59 SAY REPLICATE(' ', 15)
@ 4, 17 SAY REPLICATE(' ', 24)
@ 4, 59 SAY REPLICATE(' ', 15)
@ 5, 17 SAY REPLICATE(' ', 24)
DIMENSION producto( 1), descri(  ;
          1), cantid( 1), preuni(  ;
          1), tempre( 1), candes(  ;
          1), canres( 1), factor(  ;
          1), deta( 1)
STORE 0 TO m, c, xm, cantid( 1),  ;
      preuni( 1), tempre( 1),  ;
      factor( 1), canres( 1),  ;
      candes( 1)
STORE 1 TO z, p
STORE ' ' TO w_campo, w_lismon,  ;
      producto( 1), descri( 1)
STORE SPACE(60) TO deta( 1)
RETURN
*
PROCEDURE carque
ON KEY
DIMENSION producto( 1), descri(  ;
          1), cantid( 1), preuni(  ;
          1), candes( 1), tempre(  ;
          1), canres( 1), factor(  ;
          1), deta( 1)
m = 0
deta[ 1] = SPACE(60)
xm = m
RETURN
*
PROCEDURE pedido
SET PRINTER ON
SET DEVICE TO PRINTER
set print to &rge_lptped
@ PROW(), PCOL() SAY CHR(18)
@ PROW(), PCOL() SAY CHR(27) +  ;
  'C' + CHR(33)
linea = 13
w_it = 0
@ 01, 33 SAY 'PEDIDO DE VENTA'
@ 01, 65 SAY m.hve_tipdoc + ' ' +  ;
  m.hve_nrodoc
@ 02, 33 SAY '==============='
@ 04, 60 SAY DATE()
@ 04, 72 SAY TIME()
SELECT gc_cli00
SEEK 'C' + m.hve_codent
IF  .NOT. FOUND()
     @ 05, 03 SAY ' '
ELSE
     @ 05, 03 SAY  ;
       SUBSTR(cli_razsoc, 1, 35)
     @ 05, 42 SAY m.hve_codent
ENDIF
@ 07, 05 SAY 'Almac‚n : ' +  ;
  rge_codalm
@ 11, 00 SAY  ;
  '-----------------------------------------------------------------------------'
@ 12, 00 SAY  ;
  'IT NRO. DE PARTE    DESCRIPCION                      UBICACION          CANT.'
@ 13, 00 SAY  ;
  '-----------------------------------------------------------------------------'
FOR x = 1 TO m
     IF cantid(x) > 0
          linea = linea + 1
          w_it = w_it + 1
          @ linea, 00 SAY w_it  ;
            PICTURE '99'
          @ linea, 03 SAY  ;
            producto(x)
          SELECT gc_pro00
          SEEK producto(x)
          @ linea, 20 SAY  ;
            SUBSTR(pro_descri, 1,  ;
            26)
          @ linea, 55 SAY  ;
            ooubica(producto(x), ;
            m.hve_almdes)
          @ linea, 68 SAY  ;
            cantid(x) PICTURE  ;
            '999,999'
     ENDIF
ENDFOR
EJECT
SET DEVICE TO SCREEN
SET PRINTER TO
SET PRINTER OFF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

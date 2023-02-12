*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
DEFINE WINDOW t0 FROM 02, 64 TO  ;
       02, 73 IN screen NONE
DEFINE WINDOW t1 FROM 14, 03 TO  ;
       14, 76 IN screen NONE
DEFINE WINDOW a0 FROM 04, 01 TO  ;
       12, 78 IN screen
DEFINE WINDOW a2 FROM 11, 01 TO  ;
       11, 78 IN screen NONE
DEFINE WINDOW a3 FROM 12, 01 TO  ;
       21, 78 IN screen NONE
DEFINE POPUP detalle FROM 13, 03  ;
       TO 18, 76 PROMPT FIELDS  ;
       dip_propar + '³' +  ;
       dip_unimed + '³' + exp_7 +  ;
       '³' + STR(dip_cantid, 10) +  ;
       '³' + STR(dip_precio, 10,  ;
       2) + '³' + STR(dip_pordes,  ;
       6, 2) + '³' +  ;
       STR(dip_total, 11, 2) IN  ;
       screen COLOR SCHEME 8
ON SELECTION POPUP detalle DEAC POPUP;
DETALLE
ACTIVATE WINDOW tablas
flag = USED('GC_PAR00')
DO p_prestab WITH 'PROCESO',  ;
   'FACTURA AUTOMATICA',  ;
   'SELECCION', flag
@ 2, 63 SAY PROGRAM()
SELECT 1
USE SHARED GC_HVE00 ORDER CODIGO
SELECT 2
USE SHARED GC_DVE00 ORDER CODIGO
w_selec = SELECT()
SELECT 3
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 4
USE SHARED GC_UNI00 ORDER CODIGO
SELECT 6
USE SHARED GC_KAR00 ORDER CODIGO
SELECT 7
USE SHARED GC_CLI00 ORDER CODIGO
SELECT 8
USE SHARED GC_VND00 ORDER CODIGO
SELECT 9
USE SHARED GC_HLP00 ORDER CODIGO
SELECT 10
USE SHARED GC_DLP00 ORDER CODIGO
SELECT 11
USE SHARED GC_PRO00 ORDER CODIGO
w_selpro = SELECT()
SELECT 12
USE SHARED GC_ALM00 ORDER CODIGO
SELECT 13
USE SHARED GC_TPE00 ORDER TPE_KEY
SELECT 14
USE SHARED GC_CMV00 ORDER  ;
    cmv_feinmo
SELECT 15
USE SHARED gc_nve00 ORDER codigo
w_fin = 0
w_nuevo = 1
w_tipcam = ootc2(DATE(),'SOL ', ;
           'DOL ','2')
w_facigv = facigv()
DO WHILE w_fin<>1
     DIMENSION producto( 1),  ;
               unimed( 1),  ;
               descri( 1),  ;
               cantid( 1),  ;
               preuni( 1),  ;
               pordes( 1),  ;
               candes( 1),  ;
               prouni( 1),  ;
               facuni( 1),  ;
               stkres( 1), deta(  ;
               1)
     STORE 0 TO m, c, xm, w_fin,  ;
           w_paso, fac_uni,  ;
           pagctas, pagctad, w_sw,  ;
           m.hve_totimp,  ;
           imp_totimp, imp_totgen,  ;
           imp_totvta, imp_totigv,  ;
           imp_subsol, imp_subtot,  ;
           imp_totdes, subtot,  ;
           totnet, nro_pro,  ;
           w_swex, tran_1,  ;
           tran_ant, t_c, w_mal,  ;
           w_flete, w_embala
     STORE 1 TO z, p
     STORE ' ' TO w_campo,  ;
           w_lismon
     STORE SPACE(4) TO tipo
     ACTIVATE WINDOW t0
     SET COLOR TO N+/W
     @ 0, 0 SAY 'SELECCION'
     SET CURSOR ON
     DO pinta1
     ON KEY
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW a0
     IF w_tipcam = -1
          DO p_mensaje WITH  ;
             'NO EXISTE TIPO DE CAMBIO'
          EXIT
     ENDIF
     SELECT gc_hve00
     IF w_nuevo = 1
          SCATTER BLANK MEMVAR
     ENDIF
     w_nuevo = w_nuevo + 1
     @ 0, 12 GET m.hve_tipdoc  ;
       DEFAULT ' ' SIZE 1, 04  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD(),0)
     READ CYCLE
     IF LASTKEY() <> 27
          DO det_cab WITH 0
     ELSE
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
@ 1, 12 GET m.hve_codent DEFAULT  ;
  ' ' SIZE 1, 9 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD(),0)
@ 2, 12 GET m.hve_codvnd DEFAULT  ;
  ' ' SIZE 1, 5 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD(),0)
m.hve_codemi = rge_punemi
@ 3, 12 GET m.hve_codemi DEFAULT  ;
  rge_punemi SIZE 1, 4 PICTURE  ;
  '@!' VALID oovalid(VARREAD())  ;
  WHEN oowhen(VARREAD(),0)
m.hve_almdes = SPACE(4)
@ 3, 54 GET m.hve_almdes DEFAULT  ;
  '0001' SIZE 1, 4 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD(),0)
m.hve_tippag = '001 '
@ 4, 12 GET m.hve_tippag DEFAULT  ;
  '001 ' SIZE 1, 4 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD(),0)
m.hve_lispre = rge_lispre
@ 4, 54 GET m.hve_lispre DEFAULT  ;
  rge_lispre SIZE 1, 4 PICTURE  ;
  '@!' VALID oovalid(VARREAD())  ;
  WHEN oowhen(VARREAD(),0)
@ 6, 14 GET m.hve_tidore DEFAULT  ;
  ' ' SIZE 1, 4 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD(),0)
@ 6, 59 GET m.hve_nrdore DEFAULT  ;
  0 SIZE 1, 10 PICTURE  ;
  '@ 9999999999' VALID  ;
  oovalid(VARREAD())
IF w_opc = 0
     READ CYCLE
     IF LASTKEY() <> 27
          DO detalle
          IF LASTKEY() = 27
               ACTIVATE WINDOW a0
               = limpia()
               CLEAR GETS
          ENDIF
     ELSE
          SELECT gc_hve00
          SCATTER BLANK MEMVAR
          = limpia()
     ENDIF
ELSE
     CLEAR GETS
ENDIF
w_paso = 0
RETURN
*
PROCEDURE detalle
ON KEY
FOR x = 1 TO 2
     SIZE WINDOW a0 BY -1, 0
ENDFOR
ACTIVATE WINDOW a2
@ 00, 00 SAY  ;
  '     PRODUCTO    UNI.  DESCRIPCION    CANTIDAD   PRE UNIT   %DES.    TOTAL    '  ;
  COLOR SCHEME 8
IF w_paso = 0
     = carque()
ENDIF
= cartec()
ACTIVATE WINDOW a3
w_sig = 0
DO WHILE w_sig=0 .AND. LASTKEY()<>- ;
   1
     @ 0, 0 GET arrayitem DEFAULT  ;
       deta(p) SIZE 09, 78 FROM  ;
       deta VALID p_lee(2) WHEN  ;
       posi() COLOR SCHEME 9
     READ CYCLE
     IF LASTKEY() = 27
          FOR x = 1 TO m
               SELECT 12
               SEEK producto(x) +  ;
                    m.hve_almdes
               IF FOUND()
                    DO rbloquea
                    REPLACE alm_stkres  ;
                            WITH  ;
                            alm_stkres -  ;
                            (cantid(x) *  ;
                            facuni(x))
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
          w_sig = 1
     ENDIF
ENDDO
DEACTIVATE WINDOW a2, a3
ACTIVATE WINDOW a0
FOR x = 1 TO 2
     SIZE WINDOW a0 BY 1, 0
ENDFOR
DO pinta1
RETURN
*
FUNCTION p_lee
PARAMETER aux_opcion
CLEAR GETS
w_sale = 0
w_sw = 0
w_paso = 1
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
          DEFINE WINDOW a4 FROM  ;
                 12 + p, 02 TO 12 +  ;
                 p, 77 IN screen  ;
                 NONE
          pos = ASCAN(deta,  ;
                arrayitem)
          w_codpro = producto(pos)
          w_unimed = unimed(pos)
          w_descri = descri(pos)
          w_cantid = cantid(pos)
          w_preuni = preuni(pos)
          w_pordes = pordes(pos)
          w_candes = candes(pos)
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
          w_unimed = '    '
          w_descri = '              '
          w_cantid = 0
          w_preuni = 0
          w_pordes = 0
          w_candes = 0
          ACTIVATE WINDOW a4
          @ 00, 01 SAY  ;
            '              ³    ³              ³       .00³       .00³   .00³        .00'
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
            vali2a() WHEN  ;
            when2a()
          @ 00, 16 GET w_unimed  ;
            PICTURE '@!' VALID  ;
            vali2b() WHEN  ;
            when2b()
     ENDIF
     ON KEY
     @ 00, 36 GET w_cantid  ;
       DEFAULT 0 PICTURE  ;
       '999,999.99' VALID  ;
       vali2c() WHEN when2c()
     READ
     IF LASTKEY() <> 27
          = pre_pro()
          IF w_preuni = 0
               DO p_mensaje WITH  ;
                  'EL PRECIO NO PUEDE SER CERO'
               LOOP
          ENDIF
          @ 00, 47 SAY w_preuni  ;
            PICTURE '999,999.99'
          IF w_pordes <> 0
               w_candes = ROUND(w_cantid *  ;
                          w_preuni,  ;
                          2) -  ;
                          ROUND(ROUND(w_cantid *  ;
                          w_preuni,  ;
                          2) *  ;
                          (w_pordes /  ;
                          100),  ;
                          2)
          ELSE
               w_candes = ROUND(w_cantid *  ;
                          w_preuni,  ;
                          2)
          ENDIF
          m.hve_totgen = ROUND(m.hve_totgen +  ;
                         w_candes,  ;
                         2)
          @ 00, 66 SAY w_candes  ;
            PICTURE '999,999.99'
          IF aux_opcion = 1
               nro_pro = nro_pro +  ;
                         1
               m = m + 1
               DIMENSION deta[ m]
               DIMENSION producto[  ;
                         m]
               DIMENSION unimed[  ;
                         m]
               DIMENSION prouni[  ;
                         m]
               DIMENSION descri[  ;
                         m]
               DIMENSION cantid[  ;
                         m]
               DIMENSION preuni[  ;
                         m]
               DIMENSION pordes[  ;
                         m]
               DIMENSION candes[  ;
                         m]
               DIMENSION facuni[  ;
                         m]
               DIMENSION stkres[  ;
                         m]
               producto[ m] =  ;
                       w_codpro
               unimed[ m] =  ;
                     w_unimed
               prouni[ m] =  ;
                     w_codpro +  ;
                     w_unimed
               SELECT gc_pro00
               SEEK w_codpro
               descri[ m] =  ;
                     SUBSTR(pro_descri,  ;
                     1, 14)
               cantid[ m] =  ;
                     w_cantid
               preuni[ m] =  ;
                     w_preuni
               pordes[ m] =  ;
                     w_pordes
               candes[ m] =  ;
                     w_candes
               deta[ m] =  ;
                   w_codpro + '³' +  ;
                   w_unimed + '³' +  ;
                   w_descri + '³' +  ;
                   TRANSFORM(w_cantid,  ;
                   '@ 999,999.99' ;
                   ) + '³' +  ;
                   TRANSFORM(w_preuni,  ;
                   '@ 999,999.99' ;
                   ) + '³' +  ;
                   TRANSFORM(w_pordes,  ;
                   '@ 999.99') +  ;
                   '³' +  ;
                   TRANSFORM(w_candes,  ;
                   '@ 9999,999.99' ;
                   )
               arrayitem = w_codpro +  ;
                           '³' +  ;
                           w_unimed +  ;
                           '³' +  ;
                           w_descri +  ;
                           '³' +  ;
                           TRANSFORM(w_cantid,  ;
                           '@ 999,999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(w_preuni,  ;
                           '@ 999,999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(w_pordes,  ;
                           '@ 999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(w_candes,  ;
                           '@ 9999,999.99' ;
                           )
               p = m
               pos = m
          ELSE
               producto[ pos] =  ;
                       w_codpro
               unimed[ pos] =  ;
                     w_unimed
               cantid[ pos] =  ;
                     w_cantid
               preuni[ pos] =  ;
                     w_preuni
               pordes[ pos] =  ;
                     w_pordes
               candes[ pos] =  ;
                     w_candes
               deta[ pos] =  ;
                   w_codpro + '³' +  ;
                   w_unimed + '³' +  ;
                   w_descri + '³' +  ;
                   TRANSFORM(w_cantid,  ;
                   '@ 999,999.99' ;
                   ) + '³' +  ;
                   TRANSFORM(w_preuni,  ;
                   '@ 999,999.99' ;
                   ) + '³' +  ;
                   TRANSFORM(w_pordes,  ;
                   '@ 999.99') +  ;
                   '³' +  ;
                   TRANSFORM(w_candes,  ;
                   '@ 9999,999.99' ;
                   )
               arrayitem = w_codpro +  ;
                           '³' +  ;
                           w_unimed +  ;
                           '³' +  ;
                           w_descri +  ;
                           '³' +  ;
                           TRANSFORM(w_cantid,  ;
                           '@ 999,999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(w_preuni,  ;
                           '@ 999,999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(w_pordes,  ;
                           '@ 999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(w_candes,  ;
                           '@ 9999,999.99' ;
                           )
               w_sale = 1
          ENDIF
          facuni[ pos] = fac_uni
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
w_sw = 0
ON KEY
aux_swt = .T.
DO WHILE aux_swt
     pos = ASCAN(deta, arrayitem)
     w_codpro = producto(pos)
     w_unimed = unimed(pos)
     SELECT gc_alm00
     SEEK w_codpro + m.hve_almdes
     IF FOUND()
          DO rbloquea
          REPLACE alm_stkres WITH  ;
                  alm_stkres -  ;
                  (cantid(pos) *  ;
                  fac_uni)
          REPLACE alm_usuari WITH  ;
                  clave,  ;
                  alm_fecha WITH  ;
                  DATE(),  ;
                  alm_hora WITH  ;
                  TIME()
          UNLOCK
     ENDIF
     m.hve_totgen = m.hve_totgen -  ;
                    candes(pos)
     nro_pro = nro_pro - 1
     = ADEL(producto, pos)
     = ADEL(unimed, pos)
     = ADEL(prouni, pos)
     = ADEL(descri, pos)
     = ADEL(cantid, pos)
     = ADEL(preuni, pos)
     = ADEL(pordes, pos)
     = ADEL(candes, pos)
     = ADEL(facuni, pos)
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
          DIMENSION unimed[ m]
          DIMENSION prouni[ m]
          DIMENSION descri[ m]
          DIMENSION cantid[ m]
          DIMENSION preuni[ m]
          DIMENSION pordes[ m]
          DIMENSION candes[ m]
          DIMENSION facuni[ m]
          w_codpro = producto(p)
          w_unimed = unimed(p)
          w_descri = descri(p)
          w_cantid = cantid(p)
          w_preuni = preuni(p)
          w_pordes = pordes(p)
          w_candes = candes(p)
          arrayitem = w_codpro +  ;
                      '³' +  ;
                      w_unimed +  ;
                      '³' +  ;
                      w_descri +  ;
                      '³' +  ;
                      TRANSFORM(w_cantid,  ;
                      '@ 999,999.99' ;
                      ) + '³' +  ;
                      TRANSFORM(w_preuni,  ;
                      '@ 999,999.99' ;
                      ) + '³' +  ;
                      TRANSFORM(w_pordes,  ;
                      '@ 999.99') +  ;
                      '³' +  ;
                      TRANSFORM(w_candes,  ;
                      '@ 9999,999.99' ;
                      )
     ENDIF
     UNLOCK
     EXIT
ENDDO
DO cartec
DEACTIVATE WINDOW a4
ACTIVATE WINDOW a3
IF LASTKEY() = 27 .AND. w_sale =  ;
   1
     CLEAR READ
ENDIF
RETURN
*
PROCEDURE crea
ON KEY
= vali3a()
DO gastos
STORE 0 TO w_presol, w_fmp
STORE 1 TO p
IF LASTKEY() = 27
     RETURN
ENDIF
m.hve_nrodoc = ' '
DO WHILE m.hve_nrodoc=' '
     m.hve_nrodoc = oocorrel(m.hve_tipdoc)
     IF m.hve_nrodoc = ' '
          DO p_mensaje WITH  ;
             'Impresi¢n Ocupada Espere......[Presione Enter]'
     ENDIF
ENDDO
SELECT gc_hve00
APPEND BLANK
m.hve_tipent = 'C'
m.hve_codmov = 'EVTA'
m.hve_codmon = w_lismon
m.hve_fechtc = m.hve_fecdoc
m.hve_indori = 'VENT'
m.hve_codent = m.hve_codent
DO rbloquea
GATHER MEMVAR
REPLACE hve_cosrep WITH  ;
        ROUND(totnet / w_facigv,  ;
        2)
REPLACE hve_flete WITH  ;
        m.hve_flete
REPLACE hve_embala WITH  ;
        m.hve_embala
REPLACE hve_totnet WITH  ;
        m.hve_totnet
REPLACE hve_totdes WITH  ;
        ABS(m.hve_totdes)
REPLACE hve_totgen WITH  ;
        m.hve_totgen
REPLACE hve_pagctd WITH pagctad
REPLACE hve_totvta WITH  ;
        ABS(m.hve_totvta)
REPLACE hve_totigv WITH  ;
        ABS(m.hve_totigv)
REPLACE hve_totoim WITH  ;
        ABS(m.hve_totimp)
REPLACE hve_estdoc WITH 'C'
REPLACE hve_solrep WITH  ;
        ROUND(subtot / w_facigv,  ;
        2)
REPLACE hve_solfle WITH w_flete
REPLACE hve_solnet WITH  ;
        imp_subtot
REPLACE hve_soldes WITH  ;
        ABS(imp_totdes)
REPLACE hve_solgen WITH  ;
        imp_totgen
REPLACE hve_pagcts WITH pagctas
REPLACE hve_solvta WITH  ;
        imp_totvta
REPLACE hve_soligv WITH  ;
        imp_totigv
REPLACE hve_mtocan WITH  ;
        imp_totimp
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
     2)
x6 = SUBSTR(DTOC(m.hve_fecdoc), 4,  ;
     2)
x7 = m.hve_codemi
x9 = '   '
x10 = STR(VAL(m.hve_codent), 9)
x11 = m.hve_codvnd
x12 = m.hve_tippag
control = 25
FOR x = 1 TO m
     SELECT gc_alm00
     SEEK producto(x) +  ;
          m.hve_almdes
     IF FOUND()
          wrk_almant = alm_stkfis
          DO rbloquea
          REPLACE alm_stkfis WITH  ;
                  alm_stkfis -  ;
                  (cantid(x) *  ;
                  facuni(x))
          REPLACE alm_stkres WITH  ;
                  alm_stkres -  ;
                  (cantid(x) *  ;
                  facuni(x))
          REPLACE alm_usuari WITH  ;
                  clave,  ;
                  alm_fecha WITH  ;
                  DATE(),  ;
                  alm_hora WITH  ;
                  TIME()
          UNLOCK
     ELSE
          wrk_almant = 0
     ENDIF
     w_presol = 0
     IF m.hve_tipdoc = 'BOLE'
          w_presol = ROUND(preuni(x) /  ;
                     w_facigv,  ;
                     2)
     ELSE
          w_presol = preuni(x)
     ENDIF
     SELECT gc_dve00
     APPEND BLANK
     DO rbloquea
     REPLACE dve_tipdoc WITH  ;
             m.hve_tipdoc
     REPLACE dve_nrodoc WITH  ;
             m.hve_nrodoc
     REPLACE dve_propar WITH  ;
             producto(x)
     REPLACE dve_unimed WITH  ;
             unimed(x)
     REPLACE dve_cantid WITH  ;
             cantid(x)
     REPLACE dve_pordes WITH 0
     REPLACE dve_import WITH  ;
             w_presol
     REPLACE dve_total WITH  ;
             ROUND(cantid(x) *  ;
             w_presol, 2)
     REPLACE dve_impors WITH  ;
             ROUND(w_presol *  ;
             w_tipcam, 2)
     REPLACE dve_totals WITH  ;
             ROUND(dve_impors *  ;
             cantid(x), 2)
     REPLACE dve_coprmo WITH  ;
             oocospro(producto(x))
     REPLACE dve_coprmb WITH  ;
             oocosprb(producto(x))
     REPLACE dve_usuari WITH  ;
             clave
     REPLACE dve_fecha WITH  ;
             DATE()
     REPLACE dve_hora WITH TIME()
     UNLOCK
     w_candes = dve_total
     SELECT gc_kar00
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
             unimed(x)
     REPLACE kar_codmon WITH  ;
             w_lismon
     REPLACE kar_tipent WITH 'C'
     REPLACE kar_codent WITH  ;
             m.hve_codent
     REPLACE kar_tidore WITH  ;
             m.hve_tidore
     REPLACE kar_nrdore WITH  ;
             m.hve_nrdore
     REPLACE kar_stkant WITH  ;
             wrk_almant
     REPLACE kar_cantid WITH  ;
             cantid(x)
     UNLOCK
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
          wrk_proant = pro_coprmo
          wrk_proanb = pro_coprmb
          wrk_cosuni = pro_ulcomb
     ENDIF
     SELECT gc_kar00
     DO rbloquea
     REPLACE kar_almdes WITH  ;
             m.hve_almdes
     REPLACE kar_import WITH  ;
             w_presol
     REPLACE kar_cosant WITH  ;
             wrk_proant
     REPLACE kar_cosanb WITH  ;
             wrk_proanb
     REPLACE kar_cosuni WITH  ;
             wrk_cosuni
     REPLACE kar_usuari WITH  ;
             clave
     REPLACE kar_fecha WITH  ;
             DATE()
     REPLACE kar_hora WITH TIME()
     UNLOCK
     SELECT gc_nve00
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
     SELECT gc_dve00
ENDFOR
DO imprime
DEACTIVATE WINDOW a4
ACTIVATE WINDOW a3
wrk_xyz = 1
CLEAR READ
= ooactprn(m.hve_tipdoc)
= limpia()
w_nuevo = 1
RETURN
*
PROCEDURE imprime
ON KEY
DO p_mensaje WITH  ;
   'Impresi¢n en proceso. Espere......'
STORE 70 TO aux_lin
STORE 0 TO aux_sw1, aux_con1,  ;
      aux_con2, aux_tv, aux_ta
ON KEY
SET PRINTER ON
SET DEVICE TO PRINTER
IF m.hve_tipdoc = 'BOLE'
     SET PRINT  TO &RGE_LPTBOL
ELSE
     SET PRINT  TO &RGE_LPTFAC
ENDIF
DO imprimir
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE imprimir
DO r_titulo
STORE 0 TO wrk_flemb, wrk_item,  ;
      w_precio
control = 21
FOR x = 1 TO m
     IF cantid(x) > 0
          aux_lin = aux_lin + 1
          wrk_item = wrk_item + 1
          @ aux_lin, 00 SAY  ;
            wrk_item PICTURE  ;
            '99'
          @ aux_lin, 03 SAY  ;
            producto(x)
          SELECT gc_pro00
          SEEK producto(x)
          @ aux_lin, 18 SAY  ;
            SUBSTR(pro_descri, 1,  ;
            21)
          @ aux_lin, 42 SAY  ;
            ooubica(producto(x), ;
            m.hve_almdes)
          @ aux_lin, 46 SAY  ;
            cantid(x) PICTURE  ;
            '999,999'
          IF m.hve_tipdoc =  ;
             'FACT'
               w_precio = ROUND(preuni(x) *  ;
                          w_tipcam,  ;
                          2)
          ELSE
               w_precio = ROUND(ROUND(ROUND(preuni(x) /  ;
                          w_facigv,  ;
                          2) *  ;
                          w_tipcam,  ;
                          2) *  ;
                          w_facigv,  ;
                          2)
          ENDIF
          @ aux_lin, 53 SAY  ;
            w_precio PICTURE  ;
            '999,999.99'
          wrk_x1 = ROUND(w_precio *  ;
                   cantid(x), 2)
          @ aux_lin, 66 SAY  ;
            wrk_x1 PICTURE  ;
            '999,999,999.99'
     ENDIF
ENDFOR
wrk_flemb = w_flete + w_embala
IF m.hve_tipdoc = 'FACT'
     IF wrk_flemb <> 0
          @ 34, 00 SAY  ;
            'FLET./EMB.:'
          @ 34, 12 SAY wrk_flemb  ;
            PICTURE '999,999.99'
     ENDIF
     @ 35, 08 SAY 'S/.'
     @ 35, 12 SAY imp_subtot -  ;
       wrk_flemb PICTURE  ;
       '999,999.99'
     @ 35, 62 SAY 'S/.'
     @ 35, 67 SAY imp_totvta  ;
       PICTURE '999,999.99'
     @ 36, 67 SAY imp_totigv  ;
       PICTURE '999,999.99'
     @ 37, 12 SAY ABS(imp_totdes)  ;
       PICTURE '999,999.99'
     @ 37, 67 SAY imp_totimp  ;
       PICTURE '999,999.99'
     IF pagctas <> 0
          @ 39, 00 SAY 'P.Cta:'
          @ 39, 12 SAY pagctas  ;
            PICTURE '999,999.99'
     ENDIF
     @ 40, 01 SAY  ;
       oonumlet(imp_totimp)
     @ 42, 40 SAY m.hve_tidore
     @ 42, 45 SAY m.hve_nrdore
     @ 44, 60 SAY m.hve_nrodoc
ELSE
     IF wrk_flemb <> 0
          @ 35, 10 SAY wrk_flemb  ;
            PICTURE '999,999.99'
     ENDIF
     @ 36, 07 SAY 'S/.'
     @ 36, 10 SAY imp_subtot -  ;
       wrk_flemb PICTURE  ;
       '999,999.99'
     @ 36, 62 SAY 'S/.'
     @ 36, 67 SAY imp_totimp +  ;
       pagctas PICTURE  ;
       '999,999.99'
     IF pagctas > 0
          @ 37, 67 SAY pagctas  ;
            PICTURE '999,999.99'
     ENDIF
     @ 38, 10 SAY ABS(imp_totdes)  ;
       PICTURE '999,999.99'
     @ 38, 67 SAY imp_totimp  ;
       PICTURE '999,999.99'
     @ 40, 01 SAY  ;
       oonumlet(imp_totimp)
     @ 41, 43 SAY m.hve_tidore
     @ 41, 48 SAY m.hve_nrdore  ;
       PICTURE '@!'
     @ 44, 60 SAY m.hve_nrodoc
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
     @ 11, 00 SAY m.hve_fecdoc  ;
       PICTURE '99/99/99'
     @ 11, 15 SAY m.hve_codemi
     SELECT gc_cli00
     SEEK 'C' + m.hve_codent
     IF  .NOT. FOUND()
          @ 13, 05 SAY ' '
     ELSE
          @ 13, 10 SAY cli_razsoc
     ENDIF
     @ 13, 48 SAY m.hve_codent
     SELECT gc_cli00
     SELECT ge_tab0
     SEEK 'DIST' +  ;
          gc_cli00.cli_distri
     IF FOUND()
          @ 15, 40 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     ENDIF
     SEEK 'FPAG' + m.hve_tippag
     wrk_despag = SUBSTR(tab_destab,  ;
                  1, 15)
     @ 15, 65 SAY  ;
       gc_cli00.cli_telefo
     @ 17, 01 SAY m.hve_almdes
     @ 17, 28 SAY 'N. SOLES'
     @ 17, 45 SAY wrk_despag
ELSE
     @ 11, 00 SAY m.hve_fecdoc  ;
       PICTURE '99/99/99'
     @ 11, 15 SAY m.hve_codemi
     SELECT gc_cli00
     SEEK 'C' + m.hve_codent
     IF  .NOT. FOUND()
          @ 13, 05 SAY ' '
     ELSE
          @ 13, 10 SAY cli_razsoc
     ENDIF
     SELECT ge_tab0
     SEEK 'DIST' +  ;
          gc_cli00.cli_distri
     IF FOUND()
          @ 15, 40 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     ENDIF
     SEEK 'FPAG' + m.hve_tippag
     wrk_despag = SUBSTR(tab_destab,  ;
                  1, 15)
     @ 15, 65 SAY  ;
       gc_cli00.cli_telefo
     @ 17, 01 SAY m.hve_almdes
     @ 17, 28 SAY 'N. SOLES'
     @ 17, 45 SAY wrk_despag
ENDIF
IF m.hve_tipdoc = 'FACT'
     aux_lin = 20
ELSE
     aux_lin = 19
ENDIF
RETURN
*
PROCEDURE pinta1
DEACTIVATE WINDOW titposs
ACTIVATE WINDOW a0
@ 00, 00 CLEAR TO 08, 75
@ 00, 00 SAY 'Tipo Doc. :'
@ 00, 44 SAY 'Nro.    :'
@ 00, 66 SAY 'TC:'
@ 00, 69 SAY w_tipcam PICTURE  ;
  '9999.99'
@ 01, 00 SAY 'Cliente   :'
@ 02, 00 SAY 'Vendedor  :'
@ 03, 00 SAY 'Emisor    :'
@ 03, 44 SAY 'Almac‚n :'
@ 04, 00 SAY 'F. Pago   :'
@ 04, 44 SAY 'L. Prec.:'
@ 05, 00 SAY 'F. Emisi¢n:'
@ 05, 44 SAY 'F. Vcto.:'
@ 06, 00 SAY 'T. Doc. Ref.:'
@ 06, 44 SAY 'Nro. Doc Ref.:'
SELECT ge_tab0
RETURN
*
PROCEDURE cartec
ON KEY
IF m = 0
     DO p_footer WITH  ;
        '101000000000000000001',  ;
        2
ELSE
     DO p_footer WITH  ;
        '111100000000101000001',  ;
        2
     ON KEY LABEL F2 DO CREA 
     ON KEY LABEL F4 DO P_BORRA 
ENDIF
ON KEY LABEL F3 DO P_LEE    WITH 1
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
SELECT gc_dve00
w_selec = SELECT()
w_campo = 'w_codpro'
ON KEY LABEL F6 DO PRODUC WITH w_campo,w_selec,w_selpro
RETURN
*
PROCEDURE when2b
DO p_footer WITH  ;
   '100010000000000000001', 2
wrk_var = 'w_unimed'
wrk_busca = w_codpro
ON KEY LABEL F6 DO UNIALT WITH WRK_BUSCA,WRK_VAR
RETURN
*
PROCEDURE when2c
tran_ant = w_cantid
RETURN
*
PROCEDURE pre_pro
SELECT gc_dlp00
SEEK m.hve_lispre + w_codpro +  ;
     w_unimed
IF FOUND()
     IF w_preuni = 0
          IF m.hve_tipdoc =  ;
             'FACT'
               w_preuni = dlp_prsigv
          ELSE
               w_preuni = ROUND((dlp_prsigv *  ;
                          w_facigv),  ;
                          2)
          ENDIF
     ENDIF
ENDIF
RETURN
*
FUNCTION vali2a
IF LASTKEY() = 19 .OR. LASTKEY() =  ;
   5
     RETURN .F.
ENDIF
SELECT gc_pro00
SET ORDER TO CODIGO
SEEK w_codpro
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        'PRODUCTO NO EXISTE'
     RETURN .F.
ELSE
     IF pro_estope = 'B'
          DO p_mensaje WITH  ;
             'PRODUCTO ESTA BLOQUEADO'
          RETURN .F.
     ENDIF
     w_unimed = pro_unimed
     w_descri = SUBSTR(pro_descri,  ;
                1, 14)
     @ 00, 21 SAY w_descri
ENDIF
RETURN
*
FUNCTION vali2b
pos1 = ASCAN(prouni, w_codpro +  ;
       w_unimed)
IF LASTKEY() = 19 .OR. LASTKEY() =  ;
   5
     RETURN
ENDIF
w_swex = 0
IF pos1 <> 0
     w_swex = 1
     DO p_mensaje WITH  ;
        'EL PRODUCTO YA EXISTE'
     RETURN .F.
ENDIF
SELECT gc_uni00
SEEK w_codpro + w_unimed
IF  .NOT. FOUND()
     SELECT gc_pro00
     SEEK w_codpro
     IF FOUND()
          IF w_unimed <>  ;
             pro_unimed
               DO p_mensaje WITH  ;
                  'UNIDAD NO EXISTE EN EL PRODUCTO NI COMO ALTERNATIVO'
               RETURN .F.
          ELSE
               uni_facto1 = 1
               uni_facto2 = 1
          ENDIF
     ENDIF
ENDIF
fac_uni = uni_facto1 / uni_facto2
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
        'CANTIDAD EN CERO..'
     RETURN .F.
ENDIF
IF w_cantid < 0
     DO p_mensaje WITH  ;
        'CANTIDAD NEGATIVA.'
     RETURN .F.
ENDIF
= transito_2(w_cantid)
IF w_mal = 1
     w_cantid = tran_ant
     RETURN .F.
ENDIF
RETURN
*
FUNCTION vali3ca
IF m.hve_pordes > rge_pormax
     DO p_mensaje WITH  ;
        'EL DESCUENTO ES MAYOR QUE EL DEFINIDO EN LOS PARAMETROS'
     RETURN .F.
ENDIF
IF m.hve_pordes > 0
     m.hve_totdes = ROUND((m.hve_totnet *  ;
                    (m.hve_pordes /  ;
                    100)), 2)
ENDIF
wrk_totdes = ROUND(m.hve_totdes *  ;
             w_tipcam, 2)
RETURN
*
PROCEDURE vali3c
m.hve_totdes = ROUND(wrk_totdes /  ;
               w_tipcam, 2)
IF m.hve_totdes > m.hve_totgen
     DO p_mensaje WITH  ;
        'OJO el Monto excede del 100 %, el resultado puede ser NEGATIVO'
ENDIF
IF m.hve_totdes <> 0
     m.hve_pordes = ROUND(((m.hve_totdes *  ;
                    100) /  ;
                    m.hve_totnet),  ;
                    2)
ENDIF
@ 4, 21 GET m.hve_pordes PICTURE  ;
  '999.99' VALID vali3ca()
CLEAR GET
m.hve_totgen = ROUND((m.hve_totnet -  ;
               m.hve_totdes), 2)
m.hve_totimp = ROUND((m.hve_totgen -  ;
               pagctad), 2)
m.hve_totvta = ROUND((m.hve_totimp /  ;
               w_facigv), 2)
m.hve_totigv = m.hve_totimp -  ;
               m.hve_totvta
imp_totdes = ROUND(wrk_totdes, 2)
imp_totgen = ROUND((imp_subtot -  ;
             imp_totdes), 2)
imp_totimp = ROUND((imp_totgen -  ;
             pagctas), 2)
imp_totvta = ROUND((imp_totimp /  ;
             w_facigv), 2)
imp_totigv = imp_totimp -  ;
             imp_totvta
@ 3, 14 SAY imp_subtot PICTURE  ;
  '99,999,999.99'
@ 7, 14 SAY imp_totvta PICTURE  ;
  '99,999,999.99'
@ 8, 14 SAY imp_totigv PICTURE  ;
  '99,999,999.99'
@ 9, 14 SAY imp_totimp PICTURE  ;
  '99,999,999.99'
@ 3, 28 SAY m.hve_totnet PICTURE  ;
  '99,999,999.99'
@ 5, 28 SAY m.hve_totdes PICTURE  ;
  '99,999,999.99'
@ 7, 28 SAY m.hve_totvta PICTURE  ;
  '99,999,999.99'
@ 8, 28 SAY m.hve_totigv PICTURE  ;
  '99,999,999.99'
@ 9, 28 SAY m.hve_totimp PICTURE  ;
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
m.hve_flete = ROUND(w_flete /  ;
              w_tipcam, 2)
m.hve_embala = ROUND(w_embala /  ;
               w_tipcam, 2)
m.hve_totdes = ROUND(wrk_totdes /  ;
               w_tipcam, 2)
m.hve_totnet = ROUND(totnet +  ;
               (m.hve_cargo +  ;
               m.hve_flete +  ;
               m.hve_embala), 2)
m.hve_totgen = ROUND((m.hve_totnet -  ;
               m.hve_totdes), 2)
m.hve_totimp = ROUND((m.hve_totgen -  ;
               pagctad), 2)
m.hve_totvta = ROUND((m.hve_totimp /  ;
               w_facigv), 2)
m.hve_totigv = m.hve_totimp -  ;
               m.hve_totvta
imp_subtot = ROUND(subtot +  ;
             (w_flete + w_embala),  ;
             2)
imp_totdes = ROUND(wrk_totdes, 2)
imp_totgen = ROUND((imp_subtot -  ;
             imp_totdes), 2)
imp_totimp = ROUND((imp_totgen -  ;
             pagctas), 2)
imp_totvta = ROUND((imp_totimp /  ;
             w_facigv), 2)
imp_totigv = imp_totimp -  ;
             imp_totvta
@ 3, 14 SAY imp_subtot PICTURE  ;
  '99,999,999.99'
@ 7, 14 SAY imp_totvta PICTURE  ;
  '99,999,999.99'
@ 8, 14 SAY imp_totigv PICTURE  ;
  '99,999,999.99'
@ 9, 14 SAY imp_totimp PICTURE  ;
  '99,999,999.99'
@ 3, 28 SAY m.hve_totnet PICTURE  ;
  '99,999,999.99'
@ 5, 28 SAY m.hve_totdes PICTURE  ;
  '99,999,999.99'
@ 7, 28 SAY m.hve_totvta PICTURE  ;
  '99,999,999.99'
@ 8, 28 SAY m.hve_totigv PICTURE  ;
  '99,999,999.99'
@ 9, 28 SAY m.hve_totimp PICTURE  ;
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
PROCEDURE vali3a
IF w_sw = 0
     m.hve_totgen = 0
     FOR x = 1 TO m
          m.hve_totgen = m.hve_totgen +  ;
                         candes(x)
     ENDFOR
     IF m.hve_tipdoc = 'FACT'
          m.hve_totgen = ROUND((m.hve_totgen *  ;
                         w_facigv),  ;
                         2)
     ENDIF
     m.hve_totnet = m.hve_totgen
     w_sw = 1
ENDIF
m.hve_totvta = ROUND(m.hve_totgen /  ;
               w_facigv, 2)
m.hve_totigv = m.hve_totgen -  ;
               m.hve_totvta
RETURN
*
FUNCTION facigv
SELECT ge_tab0
SET ORDER TO CODIGO
SEEK 'IGV ' + 'IGV '
IF  .NOT. FOUND()
     IF LASTKEY() = 19 .OR.  ;
        LASTKEY() = 5
          RETURN
     ENDIF
     DO p_mensaje WITH  ;
        'Tipo de I.G.V. no existe en Tablas'
     RETURN .F.
ELSE
     w_facigv = (1 + (tab_factor /  ;
                100))
ENDIF
RETURN w_facigv
*
PROCEDURE gastos
ON KEY
STORE 0 TO wrk_tempo, w_flete,  ;
      w_embala, wrk_totdes,  ;
      w_presol, imp_subtot,  ;
      subtot
DEFINE WINDOW gasto FROM 09, 20  ;
       TO 20, 63 IN screen
DO p_footer WITH  ;
   '100000000000000000001', 2
ACTIVATE WINDOW gasto
@ 0, 00 SAY  ;
  'T O T A L E S          S/.         US$'
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
     IF m.hve_tipdoc = 'FACT'
          w_presol = ROUND(preuni(y) *  ;
                     w_tipcam,  ;
                     2)
     ELSE
          w_presol = ROUND(ROUND(ROUND(preuni(y) /  ;
                     w_facigv, 2) *  ;
                     w_tipcam, 2) *  ;
                     w_facigv,  ;
                     2)
     ENDIF
     imp_subtot = imp_subtot +  ;
                  ROUND(w_presol *  ;
                  cantid(y), 2)
ENDFOR
IF m.hve_tipdoc = 'FACT'
     imp_subtot = ROUND((imp_subtot *  ;
                  w_facigv), 2)
ENDIF
subtot = imp_subtot
totnet = ROUND(m.hve_totnet, 2)
m.hve_totgen = ROUND((m.hve_totnet -  ;
               m.hve_totdes), 2)
m.hve_totimp = ROUND((m.hve_totgen -  ;
               pagctad), 2)
m.hve_totvta = ROUND((m.hve_totimp /  ;
               w_facigv), 2)
m.hve_totigv = m.hve_totimp -  ;
               m.hve_totvta
w_flete = ROUND(m.hve_flete *  ;
          w_tipcam, 2)
@ 1, 14 GET w_flete PICTURE  ;
  '99,999,999.99'
w_embala = ROUND(m.hve_embala *  ;
           w_tipcam, 2)
@ 2, 14 GET w_embala PICTURE  ;
  '99,999,999.99' VALID vali3d()
@ 3, 14 SAY imp_subtot PICTURE  ;
  '99,999,999.99'
@ 4, 21 GET m.hve_pordes PICTURE  ;
  '999.99' VALID vali3ca()
wrk_totdes = ROUND(m.hve_totdes *  ;
             w_tipcam, 2)
@ 5, 14 GET wrk_totdes PICTURE  ;
  '99,999,999.99' VALID vali3c()
imp_totdes = ROUND(wrk_totdes, 2)
imp_totgen = ROUND((imp_subtot -  ;
             imp_totdes), 2)
imp_totimp = ROUND((imp_totgen -  ;
             pagctas), 2)
imp_totvta = ROUND((imp_totimp /  ;
             w_facigv), 2)
imp_totigv = imp_totimp -  ;
             imp_totvta
@ 6, 14 SAY pagctas PICTURE  ;
  '99,999,999.99'
@ 7, 14 SAY imp_totvta PICTURE  ;
  '99,999,999.99'
@ 8, 14 SAY imp_totigv PICTURE  ;
  '99,999,999.99'
@ 9, 14 SAY imp_totimp PICTURE  ;
  '99,999,999.99'
@ 3, 28 SAY m.hve_totnet PICTURE  ;
  '99,999,999.99'
@ 5, 28 SAY m.hve_totdes PICTURE  ;
  '99,999,999.99'
@ 6, 28 SAY pagctad PICTURE  ;
  '99,999,999.99'
@ 7, 28 SAY m.hve_totvta PICTURE  ;
  '99,999,999.99'
@ 8, 28 SAY m.hve_totigv PICTURE  ;
  '99,999,999.99'
@ 9, 28 SAY m.hve_totimp PICTURE  ;
  '99,999,999.99'
READ CYCLE
DEACTIVATE WINDOW gasto
ACTIVATE WINDOW a3
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen, nsalta
ON KEY LABEL ESC
DO CASE
     CASE cwhen = 'HVE_TIPDOC'
          wrk_busca = 'DOCU'
          wrk_var = 'M.HVE_TIPDOC'
          ON KEY LABEL F6 DO BUSCA WITH;
WRK_BUSCA,WRK_VAR 
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
                    wrk_busca = 'C'
                    wrk_var = 'M.HVE_CODENT'
               OTHERWISE
                    wrk_busca = 'V'
                    wrk_var = 'M.HVE_CODVND'
          ENDCASE
          ON KEY LABEL F6 DO ENTIDAD WITH;
WRK_BUSCA,WRK_VAR            
     CASE cwhen = 'HVE_CODEMI'
          wrk_busca = 'EMIS'
          wrk_var = 'M.HVE_CODEMI'
          ON KEY LABEL F6 DO BUSCA WITH;
WRK_BUSCA,WRK_VAR 
     CASE cwhen = 'HVE_ALMDES'
          wrk_busca = 'ALMA'
          wrk_var = 'M.HVE_ALMDES'
          ON KEY LABEL F6 DO BUSCA WITH;
WRK_BUSCA,WRK_VAR
     CASE cwhen = 'HVE_TIPPAG'
          wrk_busca = 'FPAG'
          wrk_var = 'M.HVE_TIPPAG'
          ON KEY LABEL F6 DO BUSCA WITH;
WRK_BUSCA,WRK_VAR
     CASE cwhen = 'HVE_LISPRE'
          wrk_var = 'M.HVE_LISPRE'
          ON KEY LABEL F6 DO BUSHLP WITH;
WRK_VAR
     CASE cwhen = 'HVE_TIDORE'
          wrk_busca = 'DOCU'
          wrk_var = 'M.HVE_TIDORE'
          ON KEY LABEL F6 DO BUSCA WITH;
WRK_BUSCA,WRK_VAR
ENDCASE
IF nsalta = 1
     KEYBOARD CHR(13)
ENDIF
IF nsalta = 3
     ON KEY LABEL ESC DO ooSalir WITH;
1
     ON KEY LABEL F6 WAIT WINDOW "" NOWAIT;
    
ENDIF
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
          wrk_busca = 'DOCU'
          SELECT ge_tab0
          SEEK wrk_busca +  ;
               m.hve_tipdoc
          @ 0, 17 SAY REPLICATE( ;
            ' ', 24)
          IF FOUND()
               IF m.hve_tipdoc =  ;
                  'FACT' .OR.  ;
                  m.hve_tipdoc =  ;
                  'BOLE' .OR.  ;
                  m.hve_tipdoc =  ;
                  'GUIA'
                    @ 0, 17 SAY  ;
                      SUBSTR(ALLTRIM(ge_tab0.tab_destab),  ;
                      1, 24)  ;
                      COLOR W+/N 
               ELSE
                    DO p_mensaje  ;
                       WITH  ;
                       ' TIPO DE DOCUMENTO NO VALIDO '
                    RETURN .F.
               ENDIF
          ELSE
               DO p_mensaje WITH  ;
                  ' TIPO DE DOCUMENTO NO EXISTE '
               RETURN .F.
          ENDIF
          CLEAR READ
     CASE cvalid = 'HVE_NRODOC'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          SELECT gc_hve00
          m.hve_nrodoc = f_ceros(m.hve_nrodoc, ;
                         10,2)
          SEEK m.hve_tipdoc +  ;
               m.hve_nrodoc
          IF  .NOT.  ;
              EMPTY(m.hve_nrodoc)
               IF FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       ' NUMERO DE DOCUMENTO YA EXISTE '
                    RETURN .F.
               ENDIF
          ELSE
               DO p_mensaje WITH  ;
                  ' ES NECESARIO NUMERO DE DOCUMENTO '
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
          wrk_busca = 'C'
          SELECT gc_cli00
          SEEK wrk_busca +  ;
               m.hve_codent
          @ 1, 25 SAY REPLICATE( ;
            ' ', 40)
          IF FOUND()
               @ 1, 25 SAY  ;
                 gc_cli00.cli_razsoc  ;
                 COLOR W+/N 
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
     CASE cvalid = 'HVE_CODVND'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          wrk_busca = 'V'
          SELECT gc_vnd00
          SEEK wrk_busca +  ;
               m.hve_codvnd
          @ 2, 25 SAY REPLICATE( ;
            ' ', 24)
          IF FOUND()
               @ 2, 25 SAY  ;
                 SUBSTR(ALLTRIM(gc_vnd00.vnd_nombre),  ;
                 1, 24) COLOR W+/ ;
                 N 
          ELSE
               DO p_mensaje WITH  ;
                  ' CODIGO DE VENDEDOR NO EXISTE '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_CODEMI'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          wrk_busca = 'EMIS'
          SELECT ge_tab0
          SEEK wrk_busca +  ;
               m.hve_codemi
          @ 3, 17 SAY REPLICATE( ;
            ' ', 24)
          IF FOUND()
               @ 3, 17 SAY  ;
                 SUBSTR(ALLTRIM(ge_tab0.tab_destab),  ;
                 1, 24) COLOR W+/ ;
                 N 
          ELSE
               DO p_mensaje WITH  ;
                  ' CODIGO DE EMISOR NO EXISTE '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_ALMDES'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          wrk_busca = 'ALMA'
          SELECT ge_tab0
          SEEK wrk_busca +  ;
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
                  ' CODIGO DE ALMACEN NO EXISTE '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_TIPPAG'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          wrk_busca = 'FPAG'
          SELECT ge_tab0
          SEEK wrk_busca +  ;
               m.hve_tippag
          @ 4, 17 SAY REPLICATE( ;
            ' ', 24)
          IF FOUND()
               @ 4, 17 SAY  ;
                 SUBSTR(ALLTRIM(ge_tab0.tab_destab),  ;
                 1, 24) COLOR W+/ ;
                 N 
          ELSE
               DO p_mensaje WITH  ;
                  'FORMA DE PAGO NO EXISTE '
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
                  ' LISTA DE PRECIOS NO EXISTE '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_FECDOC'
          IF w_tipcam = -1
               DO p_mensaje WITH  ;
                  'NO EXISTE TIPO DE CAMBIO'
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_TIDORE'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          wrk_busca = 'DOCU'
          SELECT ge_tab0
          SEEK wrk_busca +  ;
               m.hve_tidore
          @ 6, 17 SAY REPLICATE( ;
            ' ', 24)
          IF FOUND()
               @ 6, 22 SAY  ;
                 SUBSTR(ALLTRIM(ge_tab0.tab_destab),  ;
                 1, 24) COLOR W+/ ;
                 N 
          ENDIF
     CASE cvalid = 'HVE_NRDORE'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN
          ENDIF
          IF  .NOT.  ;
              EMPTY(m.hve_tidore)  ;
              .AND.  ;
              EMPTY(m.hve_nrdore)
               DO p_mensaje WITH  ;
                  'INGRESAR NUMERO DE DOCUMENTO'
               RETURN .F.
          ENDIF
          IF  .NOT.  ;
              EMPTY(m.hve_tidore)  ;
              .AND.  .NOT.  ;
              EMPTY(m.hve_nrdore)
               m.hve_nrdore = f_ceros(m.hve_nrdore, ;
                              10, ;
                              2)
               @ 6, 59 SAY  ;
                 m.hve_nrdore
               SELECT gc_hve00
               SEEK m.hve_tidore +  ;
                    m.hve_nrdore
               IF FOUND() .AND.  ;
                  hve_codent =  ;
                  m.hve_codent
                    pagctas = hve_solgen
                    pagctad = hve_totgen
                    @ 02, 52 SAY  ;
                      'PAGCTA:'
                    @ 02, 60 SAY  ;
                      pagctad  ;
                      PICTURE  ;
                      '$,$$$,$$$.99'  ;
                      COLOR N/W 
               ELSE
                    DO p_mensaje  ;
                       WITH  ;
                       'NO ES EL MISMO CLIENTE'
                    RETURN .F.
               ENDIF
          ENDIF
          ON KEY LABEL F6 WAIT WINDOW;
"" NOWAIT
          ON KEY LABEL ESC DO ooSalir;
WITH 1
          w_fin = 0
          CLEAR READ
ENDCASE
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
@ 6, 17 SAY REPLICATE(' ', 24)
DIMENSION producto( 1), unimed(  ;
          1), descri( 1), cantid(  ;
          1), preuni( 1), pordes(  ;
          1), candes( 1), prouni(  ;
          1), facuni( 1), stkres(  ;
          1), deta( 1)
STORE 0 TO m, c, xm, fac_uni,  ;
      producto( 1), unimed( 1),  ;
      descri( 1), cantid( 1),  ;
      preuni( 1), pordes( 1),  ;
      candes( 1), prouni( 1),  ;
      facuni( 1), stkres( 1)
STORE 1 TO z, p
STORE ' ' TO w_campo, w_lismon
STORE SPACE(60) TO deta( 1)
RETURN
*
PROCEDURE carque
ON KEY
DIMENSION producto( 1), xproduc(  ;
          1)
m = 0
deta[ 1] = SPACE(60)
xm = m
RETURN
*
PROCEDURE transito_1
PARAMETER can_1
SELECT gc_alm00
SEEK w_codpro + m.hve_almdes
IF FOUND()
     tran_1 = alm_stkres
     tran_1 = tran_1 - can_1
     tran_ant = can_1
ENDIF
RETURN
*
PROCEDURE transito_2
PARAMETER can_2
SELECT gc_alm00
SEEK w_codpro + m.hve_almdes
IF FOUND()
     DO rbloquea
     IF (can_2 * fac_uni) <=  ;
        (alm_stkfis - alm_stkres +  ;
        tran_ant)
          REPLACE alm_stkres WITH  ;
                  alm_stkres +  ;
                  (can_2 *  ;
                  fac_uni) -  ;
                  tran_ant
          REPLACE alm_usuari WITH  ;
                  clave,  ;
                  alm_fecha WITH  ;
                  DATE(),  ;
                  alm_hora WITH  ;
                  TIME()
          w_mal = 0
     ELSE
          DO p_mensaje WITH  ;
             'CANTIDAD SOLICITADA MAYOR QUE STOCK DE ALMACEN '
          w_mal = 1
     ENDIF
     UNLOCK
ELSE
     DO p_mensaje WITH  ;
        'PRODUCTO NO EXISTE EN EL ALMACEN'
     w_mal = 1
ENDIF
RETURN
*
PROCEDURE produc
PARAMETER w_campo, w_selec,  ;
          w_selpro
ON KEY
wrk_order = ORDER()
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW produ FROM 12, 02  ;
       TO 20, 76 IN screen COLOR  ;
       SCHEME 8
DEFINE POPUP prod FROM 16, 31  ;
       COLOR SCHEME 8
DEFINE BAR 1 OF prod PROMPT  ;
       '\<Codigo'
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripci¢n'
DEFINE BAR 3 OF prod PROMPT  ;
       '\<Sub-Categ.'
DEFINE BAR 4 OF prod PROMPT  ;
       '\<Nro. Parte'
ON SELECTION POPUP prod do buspro with;
bar(),w_selpro
DEFINE POPUP produ FROM 15, 18 TO  ;
       20, 73 PROMPT FIELDS  ;
       pro_codpro + '³' +  ;
       SUBSTR(pro_descri, 1, 20) +  ;
       '³' + SUBSTR(pro_modelo, 1,  ;
       20) + '³' + pro_subcat IN  ;
       screen COLOR SCHEME 8
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
ON KEY LABEL f6 do produc with w_campo,w_selec,w_selpro
IF LASTKEY() <> 27
     &w_campo = substr(gc_pro00.pro_codpro,1,14)
ENDIF
SELECT (w_selec)
KEYBOARD CHR(13)
RETURN
*
PROCEDURE buspro
PARAMETER bar, w_selpro
ON KEY
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, -1
ENDFOR
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY -1, 0
ENDFOR
ACTIVATE WINDOW pide
SELECT (w_selpro)
IF bar = 1
     w_codpro = SPACE(14)
     SET ORDER TO codigo
     @ 00, 00 SAY 'C¢digo :'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 2
     w_codpro = SPACE(40)
     SET ORDER TO descri
     @ 00, 00 SAY 'Descr. :'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 3
     w_codpro = SPACE(4)
     SET ORDER TO subcat
     @ 00, 00 SAY 'Sub-Cat :'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 4
     w_codpro = SPACE(10)
     SET ORDER TO parara
     @ 00, 00 SAY 'Partida :'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
READ
SELECT gc_alm00
SET ORDER TO CODIGO
SET FILTER TO alm_codalm = hve_almdes
SELECT gc_pro00
SET RELATION TO pro_codpro INTO gc_alm00
IF LASTKEY() <> 27
     SET NEAR ON
     SEEK w_codpro
     w_codpro = SPACE(14)
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do tomacod
     BROWSE FIELDS pro_codpro :R  ;
            :H = 'Cod. Produc.',  ;
            pro_descri :R : 25 :H =  ;
            'Descripci¢n',  ;
            pro_modelo :R : 10 :H =  ;
            'Modelo', pro_codree  ;
            :R :H = 'Reemplazo',  ;
            gc_alm00.alm_stkfis  ;
            :R : 5 :P = '9999' :H =  ;
            'F¡sic',  ;
            gc_alm00.alm_stkres  ;
            :R : 5 :P = '9999' :H =  ;
            'Reser', pro_subcat  ;
            :R :H = 'Sub-Cat' IN  ;
            produ
ENDIF
ON KEY
SET FILTER TO
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
SET NEAR OFF
RETURN
*
FUNCTION oocorrel
PARAMETER tipo
narea = SELECT()
DO WHILE .T.
     = ooopen('st_iparg',0)
     IF RLOCK()
          IF tipo = 'FACT'
               IF sys_lptfac =  ;
                  '0'
                    REPLACE sys_numfac  ;
                            WITH  ;
                            sys_numfac +  ;
                            1
                    REPLACE sys_lptfac  ;
                            WITH  ;
                            '1'
                    x = f_ceros(sys_numfac, ;
                        10,1)
                    = ooclose('st_iparg')
                    RETURN x
               ELSE
                    = ooclose('st_iparg')
                    RETURN ' '
               ENDIF
          ENDIF
          IF tipo = 'BOLE'
               IF sys_lptbol =  ;
                  '0'
                    REPLACE sys_nrobol  ;
                            WITH  ;
                            sys_nrobol +  ;
                            1
                    REPLACE sys_lptbol  ;
                            WITH  ;
                            '1'
                    x = f_ceros(sys_nrobol, ;
                        10,1)
                    = ooclose('st_iparg')
                    RETURN x
               ELSE
                    = ooclose('st_iparg')
                    RETURN ' '
               ENDIF
          ENDIF
          EXIT
          = ooclose('st_iparg')
     ENDIF
ENDDO
= ooclose('st_iparg')
RETURN
*
FUNCTION oonumlet
PARAMETER ncifra
PRIVATE numero, longitud, digito,  ;
        deci, ccifra
ccifra = TRIM(STR(ncifra, 12, 2))
numero = ''
deci = RIGHT(ccifra, 2)
ccifra = SUBSTR(ccifra, 1,  ;
         (LEN(ccifra) - 3))
DO WHILE .T.
     longitud = LEN(ccifra)
     digito = LEFT(ccifra, 1)
     DO CASE
          CASE longitud = 6
               DO CASE
                    CASE digito =  ;
                         '1'
                         numero =  ;
                          numero +  ;
                          'CIENTO '
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'DOSCIENTOS '
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TRESCIENTOS '
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUATROCIENTOS '
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'QUINIENTOS '
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SEISCIENTOS '
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SETECIENTOS '
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHOCIENTOS '
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NOVECIENTOS '
               ENDCASE
          CASE longitud = 5
               DO CASE
                    CASE digito =  ;
                         '1'
                         DO CASE
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '0'
                                   numero = numero + 'DIEZ MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '1'
                                   numero = numero + 'ONCE MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '2'
                                   numero = numero + 'DOCE MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '3'
                                   numero = numero + 'TRECE MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '4'
                                   numero = numero + 'CATORCE MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '5'
                                   numero = numero + 'QUINCE MIL'
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '6'
                                   numero = numero + 'DIECISEIS MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '7'
                                   numero = numero + 'DIECISIETE MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '8'
                                   numero = numero + 'DIECIOCHO MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '9'
                                   numero = numero + 'DIECINUEVE MIL '
                         ENDCASE
                         ccifra =  ;
                          RIGHT(ccifra,  ;
                          (LEN(ccifra) -  ;
                          1))
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'VEINTI'
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TREINTI'
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUARENTI'
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'CINCUENTI'
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SESENTI'
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SETENTI'
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHENTI'
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NOVENTI'
               ENDCASE
          CASE longitud = 4
               DO CASE
                    CASE digito =  ;
                         '0'
                         numero =  ;
                          SUBSTR(numero,  ;
                          1,  ;
                          (LEN(numero) -  ;
                          1))
                         IF RIGHT(numero,  ;
                            5) =  ;
                            'VEINT'
                              numero =  ;
                               numero +  ;
                               'EMIL '
                         ELSE
                              numero =  ;
                               numero +  ;
                               'AMIL '
                         ENDIF
                    CASE digito =  ;
                         '1'
                         IF LEN(numero) =  ;
                            0
                              numero =  ;
                               numero +  ;
                               'MIL '
                         ELSE
                              numero =  ;
                               numero +  ;
                               'UNMIL '
                         ENDIF
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'DOSMIL '
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TRESMIL '
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUATROMIL '
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'CINCOMIL '
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SEISMIL '
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SIETEMIL '
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHOMIL '
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NUEVEMIL '
               ENDCASE
          CASE longitud = 3
               DO CASE
                    CASE digito =  ;
                         '1'
                         numero =  ;
                          numero +  ;
                          'CIENTO '
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'DOSCIENTOS '
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TRESCIENTOS '
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUATROCIENTOS '
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'QUINIENTOS '
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SEISCIENTOS '
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SETECIENTOS '
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHOCIENTOS '
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NOVECIENTOS '
               ENDCASE
          CASE longitud = 2
               DO CASE
                    CASE digito =  ;
                         '1'
                         DO CASE
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '0'
                                   numero = numero + 'DIEZ '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '1'
                                   numero = numero + 'ONCE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '2'
                                   numero = numero + 'DOCE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '3'
                                   numero = numero + 'TRECE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '4'
                                   numero = numero + 'CATORCE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '5'
                                   numero = numero + 'QUINCE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '6'
                                   numero = numero + 'DIECISEIS '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '7'
                                   numero = numero + 'DIECISIETE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '8'
                                   numero = numero + 'DIECIOCHO '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '9'
                                   numero = numero + 'DIECINUEVE '
                         ENDCASE
                         ccifra =  ;
                          RIGHT(ccifra,  ;
                          (LEN(ccifra) -  ;
                          1))
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'VEINTI'
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TREINTI'
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUARENTI'
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'CINCUENTI'
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SESENTI'
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SETENTI'
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHENTI'
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NOVENTI'
               ENDCASE
          CASE longitud = 1
               DO CASE
                    CASE digito =  ;
                         '0'
                         numero =  ;
                          SUBSTR(numero,  ;
                          1,  ;
                          (LEN(numero) -  ;
                          1))
                         IF RIGHT(numero,  ;
                            5) =  ;
                            'VEINT'
                              numero =  ;
                               numero +  ;
                               'E '
                         ELSE
                              numero =  ;
                               numero +  ;
                               'A '
                         ENDIF
                    CASE digito =  ;
                         '1'
                         numero =  ;
                          numero +  ;
                          'UNO '
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'DOS '
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TRES '
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUATRO '
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'CINCO '
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SEIS '
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SIETE '
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHO '
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NUEVE '
               ENDCASE
     ENDCASE
     IF LEN(ccifra) > 1
          ccifra = RIGHT(ccifra,  ;
                   (LEN(ccifra) -  ;
                   1))
     ELSE
          EXIT
     ENDIF
ENDDO
numero = 'SON: ' + numero + 'Y ' +  ;
         deci +  ;
         '/100 NUEVOS SOLES'
RETURN (numero)
*
PROCEDURE ooactprn
PARAMETER tipo
narea = SELECT()
DO WHILE .T.
     = ooopen('st_iparg',0)
     IF RLOCK()
          IF tipo = 'FACT'
               REPLACE sys_lptfac  ;
                       WITH '0'
          ENDIF
          IF tipo = 'BOLE'
               REPLACE sys_lptbol  ;
                       WITH '0'
          ENDIF
          = ooclose('st_iparg')
          EXIT
     ENDIF
ENDDO
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

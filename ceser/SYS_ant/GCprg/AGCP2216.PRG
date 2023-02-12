*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
w_salir = 0
w_ind = 0
w_indm = 0
w_numpar = SPACE(14)
STORE SPACE(14) TO w_codpro
STORE SPACE(01) TO w_inorig,  ;
      w_campo
STORE 0 TO w_canped, w_precio,  ;
      w_totcan, fila, ntab,  ;
      w_selec, w_facigv, w_item
STORE SPACE(10) TO w_busca, w_var,  ;
      w_docref, w_pedido, w_idx3,  ;
      w_dadm
DEFINE WINDOW winpedido FROM 05,  ;
       01 TO 21, 78 NOFLOAT  ;
       NOCLOSE NOMINIMIZE NONE
DEFINE WINDOW detalle FROM 08, 01  ;
       TO 20, 78 IN screen COLOR  ;
       SCHEME 10
DEFINE POPUP detalle FROM 03, 01  ;
       TO 14, 78 PROMPT FIELDS  ;
       codigo + '³' +  ;
       SUBSTR(descri, 1, 29) +  ;
       '³' + TRANSFORM(cansol,  ;
       '@ 999,999') + '³' +  ;
       tconcep + '³' + docref +  ;
       '³' + codres IN winpedido  ;
       COLOR SCHEME 18
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
ON SELECTION POPUP clien deac popup clien;
      
ON SELECTION POPUP busca deac popup busca
DEFINE WINDOW winped FROM 19, 02  ;
       TO 21, 77 TITLE  ;
       '  D E T A L L E S  '
DEFINE WINDOW windoc FROM 12, 62  ;
       TO 19, 77 TITLE 'DOC.REF.'  ;
       COLOR SCHEME 10
DEFINE WINDOW wintot FROM 19, 02  ;
       TO 21, 77 IN screen NONE
ACTIVATE WINDOW tablas
DO p_prestab WITH 'PROCESO',  ;
   'PEDIDO SUGERIDO DE NECESIDADES',  ;
   'MANTENCION'
@ 02, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100010000000000000001', 2
SELECT 1
USE SHARED gc_hco00 ORDER codigo
SELECT 2
USE SHARED gc_dco00 ORDER codigp
SELECT 3
USE SHARED gc_tpe00 ORDER  ;
    tpe_indreg
SELECT 4
USE SHARED gc_pro00 ORDER codigo
w_selpro = SELECT()
SELECT 5
USE SHARED gc_alm00 ORDER codigo
SELECT 6
USE gc_pdt00
SELECT 7
USE SHARED ge_tab0 ORDER codigo
SELECT 8
USE SHARED gc_ord00 ORDER  ;
    ord_docprp
SELECT 9
USE SHARED gc_cli00 ORDER codigo
SELECT 10
USE SHARED gc_vnd00 ORDER codigo
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     w_facigv = ROUND(tab_factor /  ;
                100, 2)
ELSE
     DO p_mensaje WITH  ;
        '** No Esta Definido el '+ ;
        sys_codimp
     w_salir = 1
ENDIF
= ooscreen(2)
SET CURSOR ON
SELECT gc_pdt00
SCATTER BLANK MEMVAR
DO WHILE w_salir=0
     sigue = .T.
     w_c = 0
     m.pdt_fecsol = DATE()
     @ 00, 22 GET m.pdt_codalm  ;
       DEFAULT SPACE(4) PICTURE  ;
       '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(1)
     @ 01, 22 GET m.pdt_nrosol  ;
       DEFAULT SPACE(10) PICTURE  ;
       '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(2)
     @ 01, 63 GET m.pdt_fecsol  ;
       DEFAULT DATE() PICTURE  ;
       '@D' VALID  ;
       oovalid(VARREAD())
     READ CYCLE
     IF (LASTKEY() = 27)
          CLEAR READ
          EXIT
     ELSE
          SHOW GET m.hco_nrodoc  ;
               ENABLE
          w_salir = 0
          LOOP
     ENDIF
ENDDO
CLOSE DATABASES
if file('&w_idx1')
     erase &w_idx1
ENDIF
if file('&w_idx2')
     erase &w_idx2
ENDIF
if file('&w_idx3')
     erase &w_idx3
ENDIF
ON KEY
RELEASE WINDOW winpedido, winped,  ;
        windoc
DEACTIVATE WINDOW tablas
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'PDT_CODALM'
          SELECT ge_tab0
          IF  .NOT. SEEK('ALMA' +  ;
              m.pdt_codalm)
               DO p_mensaje WITH  ;
                  ' C¢digo de Almac‚n no v lido '
               RETURN .F.
          ELSE
               @ 00, 27 SAY  ;
                 REPLICATE(' ',  ;
                 5)
               @ 00, 27 SAY  ;
                 tab_destab
          ENDIF
     CASE cvalid = 'PDT_NROSOL'
          IF EMPTY(m.pdt_nrosol)
               DO p_mensaje WITH  ;
                  ' Es necesario n£mero de documento '
               RETURN .F.
          ELSE
               SELECT gc_pdt00
               SET ORDER TO NROSOL
               SEEK m.pdt_nrosol
               IF FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       ' Documento ya existe '
                    RETURN .F.
               ENDIF
          ENDIF
     CASE cvalid = 'PDT_FECSOL'
          IF EMPTY(m.pdt_fecsol)
               DO p_mensaje WITH  ;
                  ' Es fecha de documento '
               RETURN .F.
          ENDIF
          CLEAR GETS
          CLEAR READ
          DO createm
          DO detalle
          CLEAR READ
          CLEAR GETS
     CASE cvalid = 'HCO_CODENT'
          w_busca = 'P'
          SELECT gc_cli00
          SET ORDER TO codigo
          IF  .NOT. SEEK(w_busca +  ;
              m.hco_codent)
               swt = f_yesno( ;
                     ' Proveedor no Existe, Desea ingresarlo ' ;
                     )
               IF swt
                    SCATTER BLANK  ;
                            MEMVAR
                    m.cli_tpper =  ;
                     'P'
                    m.cli_codigo =  ;
                     m.hco_codent
                    IF  .NOT.  ;
                        EMPTY(m.cli_codigo)
                         = ooclientes()
                    ENDIF
               ENDIF
               RETURN .F.
          ELSE
               @ 01, 21 SAY  ;
                 SPACE(20)
               @ 01, 21 SAY  ;
                 LEFT(gc_cli00.cli_razsoc,  ;
                 20)
          ENDIF
     CASE cvalid = 'HCO_CODTRA'
          SELECT ge_tab0
          IF  .NOT. SEEK('VTRA' +  ;
              m.hco_codtra)
               DO p_mensaje WITH  ;
                  ' V¡a Transporte no v lido '
               RETURN .F.
          ELSE
               @ 2, 37 SAY  ;
                 REPLICATE(' ',  ;
                 9)
               @ 2, 37 SAY  ;
                 SUBSTR(ALLTRIM(tab_destab),  ;
                 1, 9)
          ENDIF
     CASE cvalid = 'HCO_CODADM'
          w_busca = 'A'
          SELECT gc_vnd00
          SET ORDER TO codigo
          IF  .NOT. SEEK(w_busca +  ;
              SUBSTR(m.hco_codadm,  ;
              1, 5))
               DO p_mensaje WITH  ;
                  'C¢digo de Administrativo No Existe'
               RETURN .F.
          ELSE
               @ 03, 32 SAY  ;
                 SPACE(20)
               w_dadm = LEFT(gc_vnd00.vnd_nombre,  ;
                        20)
               @ 03, 32 SAY  ;
                 LEFT(gc_vnd00.vnd_nombre,  ;
                 20)
          ENDIF
     CASE cvalid = 'HCO_FECDOC'
          IF m.hco_fecdoc =  ;
             CTOD('  /  /  ')
               DO p_mensaje WITH  ;
                  ' Es necesario fecha del documento '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HCO_FECLLE'
          IF m.hco_feclle =  ;
             CTOD('  /  /  ')
               DO p_mensaje WITH  ;
                  ' Es necesario fecha de llegada'
               RETURN .F.
          ENDIF
          IF m.hco_feclle <  ;
             m.hco_fecdoc
               DO p_mensaje WITH  ;
                  ' La fecha de llegada es menor que la del documento'
               RETURN .F.
          ENDIF
     CASE cvalid = 'HCO_FECMON'
          IF m.hco_fecmon =  ;
             CTOD('  /  /  ')
               DO p_mensaje WITH  ;
                  ' Es necesario fecha de paridad '
               RETURN .F.
          ELSE
               CLEAR GETS
               CLEAR READ
               DO detalle
               CLEAR READ
               CLEAR GETS
          ENDIF
     CASE cvalid = 'HCO_PORDES'
          IF m.hco_pordes >  ;
             rge_pormax
               DO p_mensaje WITH  ;
                  ' El porcentaje de descuento es mayor que el definido en los par metros'
               RETURN .F.
          ENDIF
          IF m.hco_pordes <> 0
               m.hco_totdes = m.hco_totnet *  ;
                              (m.hco_pordes /  ;
                              100)
          ENDIF
     CASE cvalid = 'HCO_TOTDES'
          IF m.hco_totdes <> 0
               m.hco_pordes = (m.hco_totdes /  ;
                              m.hco_totnet) *  ;
                              100
          ENDIF
          IF m.hco_pordes >  ;
             rge_pormax
               DO p_mensaje WITH  ;
                  ' El porcentaje de descuento es mayor que el definido en los par metros'
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
PROCEDURE oowhen
PARAMETER nwhen
DO CASE
     CASE nwhen = 1
          w_busca = 'ALMA'
          w_var = 'M.PDT_CODALM'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL F6 do busca with;
w_busca,w_var
     CASE nwhen = 2
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
ENDCASE
RETURN
*
PROCEDURE detalle
ON KEY
sigue = .T.
w_c = 0
DO WHILE sigue
     DO oopasar
     ACTIVATE WINDOW detalle
     SELECT mante
     GOTO TOP
     BROWSE FIELDS codigo : 14 :R  ;
            :H = 'C¢digo  ',  ;
            descri : 30 :R :H =  ;
            'Descripci¢n', cansol  ;
            : 7 :R :H =  ;
            'C.Solic.' :P =  ;
            '99,999', tconcep : 4  ;
            :R :H = 'Concepto',  ;
            docref : 10 :R :H =  ;
            'Doc.Ref.', codres :  ;
            4 :R :H = 'Cod.Resp.'  ;
            NOEDIT IN detalle
     IF LASTKEY() = 27
          DEACTIVATE WINDOW  ;
                     detalle
          sigue = .F.
     ENDIF
ENDDO
ON KEY LABEL enter
ACTIVATE WINDOW winpedido
@ 05, 00 CLEAR TO 14, 78
RETURN
*
PROCEDURE ooinicio
SHOW GETS ENABLE
ACTIVATE WINDOW winpedido
@ 01, 21 SAY SPACE(20)
@ 03, 22 SAY SPACE(40)
@ 02, 37 SAY SPACE(10)
RETURN
*
PROCEDURE oopasar
ON KEY
SELECT mante
COUNT TO w_coun
IF w_coun = 0
     DO p_footer WITH  ;
        '101000000000000000001',  ;
        2
     ON KEY LABEL enter
     ON KEY LABEL f2
     ON KEY LABEL f7
     ON KEY LABEL f4
ELSE
     DO p_footer WITH  ;
        '111101000000000000011',  ;
        2
     ON KEY LABEL f2 do grabar with 1
     ON KEY LABEL f7 do grabar with 2
     ON KEY LABEL f4 do ooRead 
     ON KEY LABEL enter do oodatped;
               
ENDIF
ON KEY LABEL f3 do ooNuevo
ON KEY LABEL f6
RETURN
*
PROCEDURE ooread
w_cod = codigo
COUNT TO n
IF n = 0
     DO p_mensaje WITH  ;
        ' No Hay Registros Para Anular'
ELSE
     LOCATE FOR codigo = w_cod
     SELECT mante
     IF  .NOT. EOF()
          SKIP
          w_cod = codigo
          SKIP -1
     ELSE
          SKIP -1
          w_cod = codigo
          SKIP
     ENDIF
     DELETE
     DO oopasar
     SELECT mante
     LOCATE FOR codigo = w_cod
     ACTIVATE WINDOW detalle
ENDIF
RETURN
*
PROCEDURE grabar
PARAMETER ntab
ON KEY
ACTIVATE WINDOW winpedido
DO oograbar WITH ntab
SELECT gc_hco00
SCATTER BLANK MEMVAR
DEACTIVATE WINDOW detalle
DO ooinicio
= ooscreen(2)
SHOW GET m.hco_nrodoc ENABLE
ACTIVATE WINDOW winpedido
CLEAR GETS
CLEAR READ
RETURN
*
PROCEDURE oocargaped
PARAMETER aux_nrodoc
SELECT gc_dco00
SET ORDER TO nrodoc
w_ind = 1
w_indm = 0
w_totcan = 0
w_item = 0
m.hco_totnet = 0
SEEK aux_nrodoc
IF FOUND()
     SCAN WHILE dco_nrodoc =  ;
          aux_nrodoc .AND.  .NOT.  ;
          EOF()
          SELECT gc_pro00
          SEEK gc_dco00.dco_codprp
          IF FOUND()
               w_indm = w_indm +  ;
                        1
               SELECT mante
               APPEND BLANK
               REPLACE codigo  ;
                       WITH  ;
                       gc_pro00.pro_codpro
               REPLACE descri  ;
                       WITH  ;
                       gc_pro00.pro_descri
               REPLACE parara  ;
                       WITH  ;
                       gc_pro00.pro_parara
               REPLACE modelo  ;
                       WITH  ;
                       gc_pro00.pro_modelo
               REPLACE unidad  ;
                       WITH  ;
                       gc_pro00.pro_unimed
               REPLACE cansol  ;
                       WITH  ;
                       gc_dco00.dco_cansol
               REPLACE precio  ;
                       WITH  ;
                       gc_dco00.dco_valpre
               m.hco_totnet = m.hco_totnet +  ;
                              (cansol *  ;
                              precio)
               w_totcan = w_totcan +  ;
                          cansol
               SELECT gc_ord00
               SET ORDER TO ord_docprp
               SEEK aux_nrodoc +  ;
                    gc_dco00.dco_codprp
               fila = 0
               IF FOUND()
                    SELECT mante
                    REPLACE ord_ref  ;
                            WITH  ;
                            gc_ord00.ord_inorig +  ;
                            gc_ord00.ord_docref
                    SELECT gc_ord00
                    SCAN WHILE  ;
                         ord_nrodoc =  ;
                         aux_nrodoc  ;
                         .AND.  ;
                         ord_codprp =  ;
                         gc_dco00.dco_codprp  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         SELECT docu
                         APPEND BLANK
                         REPLACE codigo  ;
                                 WITH  ;
                                 gc_ord00.ord_codprp
                         REPLACE inorig  ;
                                 WITH  ;
                                 gc_ord00.ord_inorig
                         REPLACE doc  ;
                                 WITH  ;
                                 gc_ord00.ord_docref
                         SELECT gc_ord00
                    ENDSCAN
               ENDIF
               w_indm = w_indm +  ;
                        1
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo no se encontr¢ en la ficha '+ ;
                  gc_dco00.dco_codprp
          ENDIF
          SELECT gc_dco00
     ENDSCAN
ENDIF
= oototalnet()
RETURN
*
PROCEDURE oodatped
SELECT mante
fila = 0
cdemas = codigo + '³' +  ;
         SUBSTR(descri, 1, 30) +  ;
         '³' + TRANSFORM(cansol,  ;
         '@ 999,999') + '³' +  ;
         tconcep + '³' + docref +  ;
         '³' + codres + ' '
w_cansol = cansol
w_concep = tconcep
w_docref = docref
w_codres = codres
w_codpro = codigo
IF w_codpro <> SPACE(14)
     ON KEY
     ACTIVATE WINDOW winped
     @ 00, 00 SAY cdemas COLOR  ;
       SCHEME 6
     @ 00, 46 GET w_cansol  ;
       PICTURE '@ 999,999' VALID  ;
       oovalnue(3)
     @ 00, 54 GET w_concep
     @ 00, 59 GET w_docref
     @ 00, 70 GET w_codres
     READ
     IF LASTKEY() <> 27
          SELECT mante
          REPLACE cansol WITH  ;
                  w_cansol
          REPLACE tconcep WITH  ;
                  w_concep
          REPLACE docref WITH  ;
                  w_docref
          REPLACE codres WITH  ;
                  w_codres
     ENDIF
     DEACTIVATE WINDOW winped,  ;
                windoc
     aux_salir = 1
     DO oopasar
     SELECT mante
     LOCATE FOR codigo = w_codpro
     ACTIVATE WINDOW detalle
ENDIF
RETURN
*
FUNCTION valref
PARAMETER ind
IF ind = 1
     SELECT ge_tab0
     SEEK 'OPED' + docu.inorig
     IF  .NOT. FOUND()
          SELECT docu
          DO p_mensaje WITH  ;
             ' Tipo de Documento No existe '
          RETURN .F.
     ENDIF
ENDIF
IF ind = 2
     IF EMPTY(doc)
          RETURN .F.
     ENDIF
ENDIF
RETURN
*
PROCEDURE oonuevo
ON KEY
w_codpro = SPACE(14)
w_descri = SPACE(30)
w_cansol = 0
w_concep = SPACE(4)
w_docref = SPACE(10)
w_codres = SPACE(4)
ACTIVATE WINDOW winped
@ 00, 00 SAY  ;
  '              ³                              ³       ³    ³          ³    '  ;
  COLOR SCHEME 6
DO WHILE .T.
     w_codpro = SPACE(14)
     @ 00, 00 GET w_codpro  ;
       PICTURE '@!' VALID  ;
       oovalnue(2) WHEN  ;
       oovalnue(1) COLOR N/W 
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     SELECT gc_pro00
     SET ORDER TO codigo
     SEEK w_codpro
     IF pro_rcom = 'H' .OR.  ;
        pro_rcom = 'Y'
          IF pro_rcom = 'H'
               DO p_mensaje WITH  ;
                  ' C¢digo tiene Remplazo   '
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo en Fin de Servicio'
          ENDIF
          aux_salir = 1
          LOOP
     ELSE
          EXIT
     ENDIF
ENDDO
IF LASTKEY() <> 27
     @ 00, 15 SAY LEFT(w_descri,  ;
       30) COLOR SCHEME 6
     @ 00, 47 GET w_cansol  ;
       PICTURE '@ 99,999' VALID  ;
       oovalnue(3) COLOR SCHEME  ;
       6
     @ 00, 54 GET w_concep  ;
       PICTURE '@!'
     @ 00, 59 GET w_docref  ;
       PICTURE '@!'
     @ 00, 70 GET w_codres  ;
       PICTURE '@!'
     READ
     IF LASTKEY() <> 27
          IF LASTKEY() = 27 .OR.  ;
             LASTKEY() = 23
               DEACTIVATE WINDOW  ;
                          windoc
          ENDIF
          IF  .NOT.  ;
              EMPTY(w_codpro)  ;
              .AND.  .NOT.  ;
              EMPTY(w_cansol)
               SELECT mante
               APPEND BLANK
               REPLACE codigo  ;
                       WITH  ;
                       w_codpro
               REPLACE descri  ;
                       WITH  ;
                       w_descri
               REPLACE cansol  ;
                       WITH  ;
                       w_cansol
               REPLACE tconcep  ;
                       WITH  ;
                       w_concep
               REPLACE docref  ;
                       WITH  ;
                       w_docref
               REPLACE codres  ;
                       WITH  ;
                       w_codres
               w_indm = w_indm +  ;
                        1
          ENDIF
     ENDIF
ENDIF
DEACTIVATE WINDOW winped
aux_salir = 1
DO oopasar
SELECT mante
LOCATE FOR codigo = w_codpro
ACTIVATE WINDOW detalle
RETURN
*
FUNCTION oovalnue
PARAMETER nvalnue, vari
DO CASE
     CASE nvalnue = 1
          w_selec = SELECT()
          w_campo = 'w_codpro'
          ON KEY LABEL f6 do produc with;
w_campo,w_selec,w_selpro
     CASE nvalnue = 2
          IF EMPTY(w_codpro)
               DO p_mensaje WITH  ;
                  ' C¢digo de Producto No V lido'
               RETURN .F.
          ENDIF
          SELECT gc_pro00
          SET ORDER TO codigo
          SEEK w_codpro
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  ' C¢digo de Producto No Existe en la Ficha'
               RETURN .F.
          ENDIF
          w_otro = w_codpro
          SELECT mante
          LOCATE FOR codigo =  ;
                 w_codpro
          IF FOUND()
               DO p_mensaje WITH  ;
                  ' Producto repetido del Pedido '
               RETURN .F.
          ENDIF
          SELECT gc_pro00
          w_descri = pro_descri
     CASE nvalnue = 3
          IF EMPTY(w_cansol) .OR.  ;
             w_cansol <= 0
               DO p_mensaje WITH  ;
                  ' Cantidad debe ser Mayor a Cero '
               RETURN .F.
          ENDIF
     CASE nvalnue = 4
          w_busca = 'TCON'
          w_var = inorig
          ON KEY LABEL f6 do busca with;
w_busca,w_var
     CASE nvalnue = 5
          w_busca = 'CRES'
          w_var = inorig
          ON KEY LABEL f6 do busca with;
w_busca,w_var
ENDCASE
RETURN
*
PROCEDURE oototalnet
m.hco_totnet = 0
w_totcan = 0
SELECT mante
GOTO TOP
w_item = 0
SCAN WHILE  .NOT. EOF()
     w_item = w_item + 1
     m.hco_totnet = m.hco_totnet +  ;
                    (cansol *  ;
                    precio)
     w_totcan = w_totcan + cansol
ENDSCAN
m.hco_totnet = ROUND(m.hco_totnet,  ;
               2)
SHOW GET w_item
SHOW GET w_totcan
SHOW GET m.hco_totnet DISABLE
= oogeneral()
RETURN
*
PROCEDURE oogeneral
SHOW GET m.hco_pordes
SHOW GET m.hco_totdes
m.hco_totgen = m.hco_totnet -  ;
               m.hco_totdes
SHOW GET m.hco_totgen DISABLE
m.hco_totgen = m.hco_totnet -  ;
               m.hco_totdes
RETURN
*
PROCEDURE oograbar
PARAMETER ngrabar
IF w_indm = 0
     DO p_mensaje WITH  ;
        'No ha Ingresado Ning£n Item '
     RETURN
ENDIF
IF EMPTY(m.hco_nrodoc)
     DO p_mensaje WITH  ;
        ' Falta n£mero de documento '
     RETURN
ENDIF
SELECT mante
IF LASTKEY() = 27
     RETURN
ENDIF
IF ngrabar = 2
     DO p_mensaje1 WITH  ;
        ' Imprimiendo...'
     DO ooprint
ENDIF
DO p_mensaje1 WITH  ;
   ' Grabando.....'
DO graba1
DO p_mensaje WITH  ;
   ' Datos grabados '
RELEASE WINDOW mensj
DEACTIVATE WINDOW footer
sigue = .F.
RETURN
*
PROCEDURE graba1
SELECT gc_hco00
SEEK m.hco_nrodoc
IF  .NOT. FOUND()
     APPEND BLANK
ENDIF
m.hco_indest = 'S'
m.hco_usuari = clave
m.hco_fecha = DATE()
m.hco_hora = TIME()
w_codigo = SPACE(14)
SELECT gc_dco00
SET ORDER TO codigp
SEEK m.hco_nrodoc
IF FOUND()
     SCAN WHILE m.hco_nrodoc =  ;
          dco_nrodoc .AND.  .NOT.  ;
          EOF()
          w_codigo = dco_codprp
          SELECT mante
          LOCATE FOR codigo =  ;
                 w_codigo
          IF  .NOT. FOUND()
               SELECT gc_pro00
               SEEK w_codigo
               IF FOUND() .AND.  ;
                  gc_dco00.dco_indest <>  ;
                  'U'
                    DO rbloquea
                    REPLACE pro_stksol  ;
                            WITH  ;
                            pro_stksol -  ;
                            gc_dco00.dco_cansol
                    UNLOCK
               ENDIF
               SELECT gc_dco00
               DO rbloquea
               DELETE
               UNLOCK
               SELECT gc_ord00
               SET ORDER TO ord_docprp
               SEEK m.hco_nrodoc +  ;
                    w_codigo
               IF FOUND()
                    SCAN WHILE  ;
                         ord_nrodoc =  ;
                         m.hco_nrodoc  ;
                         .AND.  ;
                         ord_codprp =  ;
                         w_codigo  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         DO rbloquea
                         DELETE
                         UNLOCK
                    ENDSCAN
               ENDIF
          ENDIF
          SELECT gc_dco00
     ENDSCAN
ENDIF
SELECT mante
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT gc_dco00
     SEEK m.hco_nrodoc +  ;
          mante.codigo
     IF FOUND()
          SELECT gc_pro00
          SEEK mante.codigo
          IF FOUND()
               DO rbloquea
               IF gc_dco00.dco_indest =  ;
                  'U'
                    REPLACE pro_stksol  ;
                            WITH  ;
                            pro_stksol +  ;
                            mante.cansol
               ELSE
                    REPLACE pro_stksol  ;
                            WITH  ;
                            (pro_stksol -  ;
                            gc_dco00.dco_cansol) +  ;
                            mante.cansol
               ENDIF
               UNLOCK
          ENDIF
     ELSE
          SELECT gc_pro00
          SEEK mante.codigo
          IF FOUND()
               DO rbloquea
               REPLACE pro_stksol  ;
                       WITH  ;
                       pro_stksol +  ;
                       mante.cansol
               UNLOCK
          ENDIF
          SELECT gc_dco00
          APPEND BLANK
     ENDIF
     SELECT gc_dco00
     DO rbloquea
     REPLACE dco_nrodoc WITH  ;
             m.hco_nrodoc
     REPLACE dco_codprp WITH  ;
             mante.codigo
     REPLACE dco_coduni WITH  ;
             mante.unidad
     REPLACE dco_cansol WITH  ;
             mante.cansol
     REPLACE dco_valpre WITH  ;
             mante.precio
     REPLACE dco_pordes WITH 0
     REPLACE dco_conpre WITH  ;
             mante.precio
     REPLACE dco_inorig WITH  ;
             SUBSTR(mante.ord_ref,  ;
             1, 1)
     REPLACE dco_docref WITH  ;
             SUBSTR(mante.ord_ref,  ;
             2, 10)
     REPLACE dco_feclle WITH  ;
             m.hco_feclle
     REPLACE dco_hora WITH TIME()
     REPLACE dco_fecha WITH  ;
             DATE()
     REPLACE dco_usuari WITH  ;
             clave
     REPLACE dco_indest WITH 'S'
     UNLOCK
     SELECT docu
     GOTO TOP
     COUNT FOR codigo =  ;
           mante.codigo .AND.  ;
           inorig <> ' ' .AND.  ;
           doc <> SPACE(10) TO  ;
           w_con
     IF w_con > 0
          DIMENSION dref( w_con)
          SEEK mante.codigo
          IF FOUND()
               i = 0
               SCAN WHILE codigo =  ;
                    mante.codigo  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF inorig <>  ;
                       SPACE(1)  ;
                       .AND. doc <>  ;
                       SPACE(10)
                         i = i +  ;
                             1
                         dref( i) =  ;
                             inorig +  ;
                             doc
                    ENDIF
               ENDSCAN
          ENDIF
     ENDIF
     SELECT gc_ord00
     SEEK m.hco_nrodoc +  ;
          mante.codigo
     x = 0
     SCAN WHILE (ord_nrodoc =  ;
          m.hco_nrodoc .AND.  ;
          ord_codprp =  ;
          mante.codigo) .AND.   ;
          .NOT. EOF()
          x = x + 1
          DO rbloquea
          IF x > w_con
               DELETE
          ELSE
               REPLACE ord_inorig  ;
                       WITH  ;
                       SUBSTR(dref(x),  ;
                       1, 1)
               REPLACE ord_docref  ;
                       WITH  ;
                       SUBSTR(dref(x),  ;
                       2, 10)
               REPLACE ord_fecha  ;
                       WITH  ;
                       DATE(),  ;
                       ord_hora  ;
                       WITH  ;
                       TIME(),  ;
                       ord_usuari  ;
                       WITH  ;
                       clave
               REPLACE ord_indest  ;
                       WITH 'S'
          ENDIF
          UNLOCK
     ENDSCAN
     x = x + 1
     DO WHILE x<w_con+1
          IF SUBSTR(dref(x), 1,  ;
             1) <> SPACE(1) .AND.  ;
             SUBSTR(dref(x), 2,  ;
             10) <> SPACE(10)
               APPEND BLANK
               DO rbloquea
               REPLACE ord_nrodoc  ;
                       WITH  ;
                       m.hco_nrodoc,  ;
                       ord_codprp  ;
                       WITH  ;
                       mante.codigo
               REPLACE ord_inorig  ;
                       WITH  ;
                       SUBSTR(dref(x),  ;
                       1, 1)
               REPLACE ord_docref  ;
                       WITH  ;
                       SUBSTR(dref(x),  ;
                       2, 10)
               REPLACE ord_fecha  ;
                       WITH  ;
                       DATE(),  ;
                       ord_hora  ;
                       WITH  ;
                       TIME(),  ;
                       ord_usuari  ;
                       WITH  ;
                       clave
               REPLACE ord_indest  ;
                       WITH 'S'
               UNLOCK
          ENDIF
          x = x + 1
     ENDDO
     SELECT gc_dco00
ENDSCAN
SELECT gc_hco00
DO rbloquea
GATHER MEMVAR
UNLOCK
RETURN
*
PROCEDURE ooobserva
DEFINE WINDOW mensaje FROM  ;
       INT((SROWS() - 7) / 2),  ;
       INT((SCOLS() - 65) / 2) TO  ;
       INT((SROWS() - 7) / 2) + 6,  ;
       INT((SCOLS() - 65) / 2) +  ;
       64 NOFLOAT NOCLOSE TITLE  ;
       ' Observaci¢n del Pedido '  ;
       NOMINIMIZE COLOR SCHEME 1
ACTIVATE WINDOW SAME mensaje
IF m.hco_indest = 'U'
     w_pedido = SPACE(10)
     @ 00, 02 SAY 'N§ PEDIDO:'  ;
       GET w_pedido PICTURE '@!'  ;
       VALID nrodoc()
     READ
ENDIF
@ 01, 02 SAY 'OBSERVAC.:'
@ 02, 02 GET m.hco_obser SIZE 1,  ;
  60
READ
@ 03, 25 GET nop DEFAULT 1 SIZE 1,  ;
  11, 1 PICTURE '@*HN Continuar'  ;
  VALID confirma()
RELEASE WINDOW mensaje
RETURN
*
PROCEDURE confirma
CLEAR READ
RETURN
*
FUNCTION nrodoc
IF w_pedido = SPACE(10)
     RETURN .F.
ENDIF
SELECT gc_hco00
SEEK w_pedido
IF FOUND()
     DO p_mensaje WITH  ;
        'N£mero de pedido existe'
     w_pedido = SPACE(10)
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE ooscreen
PARAMETER w_screen
DO CASE
     CASE w_screen = 1
          ACTIVATE WINDOW marco
          @ 00, 0 TO 0, 78
          @ 01, 0 SAY  ;
            '  C¢digo Proveedor :'
          @ 02, 0 TO 2, 78
          @ 03, 0 SAY  ;
            ' C¢digo         Descripci¢n                    Stk.Total   Ul.Compra   Cost.Rep'
     CASE w_screen = 2
          ACTIVATE WINDOW SAME  ;
                   winpedido
          CLEAR
          @ 00, 01 SAY  ;
            'Oficina........... :'
          @ 01, 01 SAY  ;
            'Nro. de Solicitud  :'
          @ 01, 42 SAY  ;
            'Fecha de Solicitud :'
          @ 03, 01 SAY  ;
            '  C¢digo           Descripci¢n          Cant.   Tipo    Doc.       Cod. '  ;
            COLOR N/W 
          @ 04, 01 SAY  ;
            '                                                Conc    Ref.       Rpta '  ;
            COLOR N/W 
ENDCASE
RETURN
*
PROCEDURE ooprint
SELECT mante
w_idx1 = f_indice()
index on parara+codigo to &w_idx1 additive
GOTO TOP
??? CHR(27) + CHR(80)
DO CASE
     CASE m.hco_codent =  ;
          '10000004'
          REPORT FORMAT agcp2216  ;
                 TO PRINTER  ;
                 NOCONSOLE
     CASE m.hco_codent =  ;
          '10032253'
          REPORT FORMAT agcp0212  ;
                 TO PRINTER  ;
                 NOCONSOLE
     CASE m.hco_codent =  ;
          '10000002'
          SELECT ge_tab0
          SEEK 'RUTA' + 'COMM'
          IF FOUND()
               w_ruta = ALLTRIM(tab_destab)
          ELSE
               w_ruta = SPACE(0)
          ENDIF
          w_ruta = w_ruta +  ;
                   m.hco_nrodoc
          SELECT mante
          SET SYSMENU ON
          MODIFY REPORT agcp212a
          repo form agcp212a to file &w_ruta;
noconsole
     OTHERWISE
          SELECT ge_tab0
          SEEK 'RUTA' + 'COMM'
          IF FOUND()
               w_ruta = ALLTRIM(tab_destab)
          ELSE
               w_ruta = SPACE(0)
          ENDIF
          w_ruta = w_ruta +  ;
                   m.hco_nrodoc
          SELECT mante
          REPORT FORMAT agcp2212  ;
                 TO PRINTER  ;
                 NOCONSOLE
          repo form agcp2212 to file &w_ruta;
noconsole
ENDCASE
SELECT mante
RETURN
*
FUNCTION ubica
PARAMETER codi
f = SELECT()
SELECT gc_alm00
SEEK codi
dato = alm_ubicac
SELECT (f)
RETURN dato
*
PROCEDURE centro
PARAMETER cadena, linea, xcolor
PRIVATE lcadena
STORE LEN(cadena) TO lcadena
@ linea, (80 - lcadena) / 2 SAY  ;
  cadena
RETURN
*
PROCEDURE oobuscar
ON KEY
SELECT gc_hco00
SET FILTER TO hco_indest = 'S';
.OR. hco_indest = 'U'
campo = 'transform(hco_nrodoc, "@ 9999999999") + "³" + dtoc(hco_fecdoc) + "³" + iif(hco_indest="S","SOLICITADO ","SUGERIDO   ") + "³" + transform(hco_totgen,"999,999.99")'
titulo = 'Ayuda de documentos'
DO ayuda1 WITH campo, titulo,  ;
   'HCO_NRODOC'
SELECT gc_hco00
SET FILTER TO
ON KEY LABEL F6 do oobuscar
RETURN
*
PROCEDURE ayuda1
PARAMETER campo, mensaje, clave
ACTIVATE WINDOW busqueda
define popup ayu0 from 0,0 to 11,50 promp;
field &campo title mensaje color scheme;
8
ON SELECTION POPUP ayu0 do choice0
ACTIVATE POPUP ayu0 NOWAIT
ACTIVATE POPUP ayu0
DEACTIVATE POPUP ayu0
DEACTIVATE WINDOW busqueda
RETURN
*
PROCEDURE choice0
IF LASTKEY() == 13
     keyboard &clave
     DEACTIVATE POPUP ayu0
ENDIF
RETURN
*
PROCEDURE createm
CREATE CURSOR mante (codigo C  ;
       (14), descri C (40),  ;
       cansol N (6, 0), tconcep C  ;
       (4), docref C (10), codres  ;
       C (04))
RETURN
*
PROCEDURE p_mensaje1
PARAMETER mens
SET CURSOR OFF
l = LEN(mens)
DEFINE WINDOW mensj FROM 11, (80 -  ;
       l) / 2 - 4 TO 15, (80 + l) /  ;
       2 + 4 COLOR SCHEME 12
ACTIVATE WINDOW mensj
@ 1, (WCOLS('MENSJ') - l) / 2 SAY  ;
  mens
= INKEY(0.5 )
RETURN
*
FUNCTION oonumpar
PARAMETER opc
SELECT gc_pro00
SEEK opc
IF FOUND()
     w_numpar = 'xxx'
ELSE
     w_numpar = SPACE(14)
ENDIF
RETURN w_numpar
*
*** 
*** ReFox - retrace your steps ... 
***

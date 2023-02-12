*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
SET SYSMENU ON
SET CENTURY ON
w_salir = 0
w_ind = 0
w_desm = 0
STORE SPACE(14) TO w_codpro,  ;
      w_codrem
STORE SPACE(01) TO w_inorig,  ;
      w_campo
STORE SPACE(13) TO w_parara
w_deta = 0
STORE 0 TO w_canped, w_cancel,  ;
      w_cansol, w_cancon,  ;
      w_precio, w_totcan, fila,  ;
      ntab, w_selec, w_facigv,  ;
      var1, w_reg, w_canlim,  ;
      w_item, w_dscto
STORE SPACE(10) TO w_busca, w_var,  ;
      w_docref, w_pedido, w_idx3,  ;
      w_dadm
DEFINE WINDOW winpedido FROM 05,  ;
       01 TO 21, 78 NOFLOAT  ;
       NOCLOSE NOMINIMIZE NONE
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
DEFINE WINDOW winped FROM 13, 00  ;
       TO 16, 77 TITLE  ;
       '  D E T A L L E S  '
DEFINE WINDOW windoc FROM 07, 60  ;
       TO 16, 77 TITLE 'DOC.REF.'  ;
       COLOR SCHEME 10
ACTIVATE WINDOW tablas
DO p_prestab WITH 'PROCESO',  ;
   'CONFIRMACION DE PEDIDO',  ;
   'MANTENCION'
@ 02, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100010000000000000001', 2
SELECT 1
USE SHARED gc_hco00 ORDER codigo
SELECT 2
USE SHARED gc_dco00 ORDER codigp
SELECT 3
USE SHARED gc_pro00 ORDER codigo
w_selpro = SELECT()
SELECT 4
USE SHARED gc_alm00 ORDER codigo
SELECT 5
USE SHARED ge_tab0 ORDER codigo
SELECT 6
USE SHARED gc_ord00 ORDER  ;
    ord_docprp
SELECT 7
USE SHARED gc_cli00 ORDER codigo
SELECT 8
USE SHARED gc_nfa00 ORDER  ;
    nfa_facprp
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
SELECT gc_hco00
SCATTER BLANK MEMVAR
DO WHILE w_salir=0
     sigue = .T.
     w_desm = 0
     @ 00, 18 GET m.hco_nrodoc  ;
       DEFAULT SPACE(10) PICTURE  ;
       '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(4) COLOR SCHEME 12
     @ 01, 18 GET m.hco_codent  ;
       DEFAULT SPACE(10) PICTURE  ;
       '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(8) COLOR SCHEME 12
     @ 02, 18 GET m.hco_codmon  ;
       DEFAULT 'DOL ' PICTURE  ;
       '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(1) COLOR SCHEME 12
     @ 03, 18 GET w_dscto PICTURE  ;
       '99.99' COLOR SCHEME 12
     @ 00, 68 GET m.hco_fecdoc  ;
       DEFAULT DATE() PICTURE  ;
       '@D' DISABLE VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(0) COLOR SCHEME 12
     @ 01, 68 GET m.hco_feclle  ;
       DEFAULT DATE() PICTURE  ;
       '@D' DISABLE VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(0) COLOR SCHEME 12
     @ 02, 68 GET m.hco_fecmon  ;
       DEFAULT DATE() PICTURE  ;
       '@D' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(0) COLOR SCHEME 12
     @ 03, 68 GET m.hco_feccon  ;
       DEFAULT DATE() PICTURE  ;
       '@D' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(0) COLOR SCHEME 12
     @ 16, 02 GET w_item DEFAULT  ;
       0 PICTURE '999' DISABLE  ;
       COLOR SCHEME 12
     @ 16, 10 GET w_totcan  ;
       DEFAULT 0 PICTURE  ;
       '9,999,999' DISABLE COLOR  ;
       SCHEME 12
     @ 16, 22 GET m.hco_totnet  ;
       DEFAULT 0 PICTURE  ;
       '9,999,999.99' DISABLE  ;
       COLOR SCHEME 12
     @ 16, 40 GET m.hco_pordes  ;
       DEFAULT 0 PICTURE '99.99'  ;
       DISABLE VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(0) COLOR SCHEME 12
     @ 16, 52 GET m.hco_totdes  ;
       DEFAULT 0 PICTURE  ;
       '999,999.99' DISABLE VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(0) COLOR SCHEME 12
     @ 16, 65 GET m.hco_totgen  ;
       DEFAULT 0 PICTURE  ;
       '9,999,999.99' DISABLE  ;
       COLOR SCHEME 12
     READ CYCLE
     IF (LASTKEY() = 27) .AND.  ;
        var1 = 1
          w_salir = 1
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
     CASE cvalid = 'HCO_NRODOC'
          m.hco_nrodoc = f_ceros(m.hco_nrodoc, ;
                         10,2)
          SHOW GET m.hco_nrodoc  ;
               ENABLE
          IF m.hco_nrodoc =  ;
             '0000000000'
               DO p_mensaje WITH  ;
                  ' Es necesario n£mero de documento '
               KEYBOARD '{CTRL+Y}'
               RETURN .F.
          ELSE
               IF m.hco_nrodoc =  ;
                  '0000000009'
                    @ 04, 00 SAY  ;
                      'Nro.Pedido     C¢digo     Descrip.  C.Bor. C.Conf.  P.Conf. C.Canc. Fecha LLeg'  ;
                      COLOR N/W 
                    w_deta = 1
                    DEFINE POPUP  ;
                           detalle  ;
                           FROM  ;
                           05, 01  ;
                           TO 14,  ;
                           78  ;
                           PROMPT  ;
                           FIELDS  ;
                           pedido +  ;
                           '³' +  ;
                           codigo +  ;
                           '³' +  ;
                           SUBSTR(descri,  ;
                           1, 9) +  ;
                           '³' +  ;
                           TRANSFORM(canbor,  ;
                           '@ 99,999' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(cancof,  ;
                           '@ 99,999' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(precio,  ;
                           '@ 99,999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(cancel,  ;
                           '@ 9999' ;
                           ) +  ;
                           '³' +  ;
                           DTOC(feclle)  ;
                           IN  ;
                           winpedido  ;
                           COLOR  ;
                           SCHEME  ;
                           18
                    DO ooinicio
                    m.hco_codent =  ;
                     '10000001'
                    DO createm
                    DO llenatem
                    IF w_reg = 0
                         RETURN .F.
                    ENDIF
               ELSE
                    w_deta = 0
                    @ 04, 00 SAY  ;
                      'Nro.Pedido     C¢digo     Descrip.  C.Sol. C.Conf.  P.Conf. C.Canc. Doc.Ref.  '  ;
                      COLOR N/W 
                    DEFINE POPUP  ;
                           detalle  ;
                           FROM  ;
                           05, 01  ;
                           TO 14,  ;
                           78  ;
                           PROMPT  ;
                           FIELDS  ;
                           pedido +  ;
                           '³' +  ;
                           codigo +  ;
                           '³' +  ;
                           SUBSTR(descri,  ;
                           1, 9) +  ;
                           '³' +  ;
                           TRANSFORM(cansol,  ;
                           '@ 99,999' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(cancof,  ;
                           '@ 99,999' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(precio,  ;
                           '@ 99,999.99' ;
                           ) +  ;
                           '³' +  ;
                           TRANSFORM(cancel,  ;
                           '@ 9999' ;
                           ) +  ;
                           '³' +  ;
                           IIF(w_deta =  ;
                           0,  ;
                           ord_ref,  ;
                           DTOC(feclle))  ;
                           IN  ;
                           winpedido  ;
                           COLOR  ;
                           SCHEME  ;
                           18
                    SELECT gc_hco00
                    IF SEEK(m.hco_nrodoc)
                         SCATTER MEMVAR
                         IF m.hco_indest =  ;
                            'A'
                              DO p_mensaje  ;
                                 WITH  ;
                                 ' Pedido se encuentra anulado '
                              RETURN  ;
                               .F.
                         ENDIF
                         IF m.hco_indest =  ;
                            'C'
                              DO p_mensaje  ;
                                 WITH  ;
                                 ' Pedido se encuentra cerrado '
                              RETURN  ;
                               .F.
                         ENDIF
                         IF m.hco_numfac <>  ;
                            m.hco_nrodoc
                              DO p_mensaje  ;
                                 WITH  ;
                                 ' Nro. Factura Diferente al Nro. de Documento'
                              RETURN  ;
                               .F.
                         ENDIF
                         SELECT gc_cli00
                         SET ORDER TO;
codigo
                         SEEK 'P' +  ;
                              m.hco_codent
                         IF FOUND()
                              @ 01,  ;
                                18  ;
                                SAY  ;
                                hco_codent  ;
                                COLOR  ;
                                SCHEME  ;
                                12
                              @ 01,  ;
                                30  ;
                                SAY  ;
                                LEFT(gc_cli00.cli_razsoc,  ;
                                16)
                         ELSE
                              @ 01,  ;
                                30  ;
                                SAY  ;
                                SPACE(16)
                         ENDIF
                         DO createm
                         = oocargaped(m.hco_nrodoc)
                         IF w_ind =  ;
                            1
                              DO p_mensaje  ;
                                 WITH  ;
                                 ' C¢digo no se encontr¢ en la ficha '+ ;
                                 gc_dco00.dco_codprp
                              RETURN  ;
                               .F.
                         ENDIF
                    ELSE
                         DO createm
                         DO ooinicio
                    ENDIF
               ENDIF
          ENDIF
          SHOW GETS ENABLE
          w_salir = 0
     CASE cvalid = 'HCO_CODENT'
          IF EMPTY(m.hco_codent)
               DO p_mensaje WITH  ;
                  ' No puede estar en Blanco '
               RETURN .F.
          ELSE
               w_busca = 'P'
               SELECT gc_cli00
               SET ORDER TO codigo
               IF  .NOT.  ;
                   SEEK(w_busca +  ;
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
                    @ 01, 30 SAY  ;
                      SPACE(16)
                    @ 01, 30 SAY  ;
                      LEFT(gc_cli00.cli_razsoc,  ;
                      16)
               ENDIF
          ENDIF
     CASE cvalid = 'HCO_CODMON'
          SELECT ge_tab0
          IF  .NOT. SEEK('MONE' +  ;
              m.hco_codmon)
               DO p_mensaje WITH  ;
                  ' C¢digo de moneda no v lido '
               RETURN .F.
          ELSE
               @ 2, 23 SAY  ;
                 REPLICATE(' ',  ;
                 5)
               @ 2, 23 SAY  ;
                 SUBSTR(ALLTRIM(tab_destab),  ;
                 1, 5)
          ENDIF
     CASE cvalid = 'HCO_CODTRA'
          SELECT ge_tab0
          IF  .NOT. SEEK('VTRA' +  ;
              m.hco_codtra)
               DO p_mensaje WITH  ;
                  ' V¡a Transporte no v lido '
               RETURN .F.
          ELSE
               @ 3, 23 SAY  ;
                 REPLICATE(' ',  ;
                 9)
               @ 3, 23 SAY  ;
                 SUBSTR(ALLTRIM(tab_destab),  ;
                 1, 9)
          ENDIF
     CASE cvalid = 'HCO_FECDOC'
          IF m.hco_fecdoc =  ;
             CTOD('  /  /    ')
               DO p_mensaje WITH  ;
                  ' Es necesario fecha del documento '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HCO_FECLLE'
          IF m.hco_feclle =  ;
             CTOD('  /  /    ')
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
             CTOD('  /  /    ')
               DO p_mensaje WITH  ;
                  ' Es necesario fecha de paridad '
               RETURN .F.
          ELSE
               IF m.hco_codmon <>  ;
                  rge_monbas
                    SELECT 20
                    USE SHARED  ;
                        gc_cmv00  ;
                        ORDER  ;
                        cmv_feinmo
                    SEEK DTOS(m.hco_fecmon) +  ;
                         '1' +  ;
                         'SOL ' +  ;
                         'DOL '
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            ' Fecha de paridad no ha sido creada '
                         RETURN .F.
                    ENDIF
               ENDIF
          ENDIF
     CASE cvalid = 'HCO_FECCON'
          IF m.hco_feccon =  ;
             CTOD('  /  /    ')
               DO p_mensaje WITH  ;
                  ' Es necesario fecha de Confirmaci¢n'
               RETURN .F.
          ENDIF
          IF m.hco_feccon <  ;
             m.hco_fecdoc
               DO p_mensaje WITH  ;
                  ' La fecha de Confirmaci¢n no puede ser menor que la del documento'
               RETURN .F.
          ELSE
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
var1 = 0
DO CASE
     CASE nwhen = 0
          ON KEY
          SHOW GET m.hco_nrodoc  ;
               DISABLE
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
     CASE nwhen = 1
          w_busca = 'MONE'
          w_var = 'M.HCO_CODMON'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL F6 do busca with;
w_busca,w_var
     CASE nwhen = 2
          w_busca = 'VTRA'
          w_var = 'M.HCO_CODTRA'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL f6 do busca with;
w_busca,w_var            
     CASE nwhen = 3
          ON KEY LABEL spacebar do marca
          ON KEY LABEL f6 do bustip
     CASE nwhen = 4
          var1 = 1
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL F6 do oobuscar;
with 1
     CASE nwhen = 5
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL F6 do oobuscar;
with 2
     CASE nwhen = 8
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          w_busca = 'P'
          w_var = 'M.HCO_CODENT'
          ON KEY LABEL F6 do entidad with;
w_busca,w_var            
     CASE nwhen = 9
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          w_busca = 'A'
          w_var = 'm.hco_codadm'
          ON KEY LABEL f6 do entidad with;
w_busca,w_var      
     CASE nwhen = 10
          ON KEY LABEL spacebar
          ON KEY LABEL f6
ENDCASE
RETURN
*
PROCEDURE detalle
sigue = .T.
DO WHILE sigue
     DO oopasar
     SELECT mante
     ACTIVATE POPUP detalle
     IF LASTKEY() = 27
          DEACTIVATE POPUP  ;
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
m.hco_fecdoc = DATE()
m.hco_fecmon = DATE()
m.hco_feclle = DATE()
m.hco_feccon = DATE()
m.hco_codmon = 'DOL '
m.hco_indest = 'P'
m.hco_codtra = '0001'
m.hco_codent = SPACE(11)
m.hco_pordes = 0
m.hco_totgen = 0
m.hco_totdes = 0
m.hco_totnet = 0
w_totcan = 0
w_item = 0
SHOW GETS ENABLE
ACTIVATE WINDOW winpedido
@ 01, 28 SAY SPACE(18)
@ 02, 28 SAY SPACE(18)
@ 03, 23 SAY SPACE(20)
@ 16, 02 SAY w_item PICTURE '999'
@ 16, 10 SAY w_totcan PICTURE  ;
  '9,999,999'
RETURN
*
PROCEDURE oopasar
SELECT mante
COUNT TO w_coun
IF w_coun = 0
     DO p_footer WITH  ;
        '101000000000000000001',  ;
        2
     ON KEY LABEL enter
     ON KEY LABEL f2
     ON KEY LABEL f7
     ON KEY LABEL f3 do pedido
ELSE
     IF m.hco_nrodoc =  ;
        '0000000009'
          DO p_footer WITH  ;
             '110001000000000000011',  ;
             2
     ELSE
          DO p_footer WITH  ;
             '111001000000000000011',  ;
             2
          ON KEY LABEL f3 do pedido
     ENDIF
     ON KEY LABEL f2 do grabar with 1
     ON KEY LABEL f7 do grabar with 2
     ON KEY LABEL enter do oodatped;
               
ENDIF
ON KEY LABEL f6
RETURN
*
PROCEDURE oocargaped
PARAMETER aux_nrodoc
SELECT gc_nfa00
w_ind = 0
w_totcan = 0
w_item = 0
m.hco_totnet = 0
SEEK aux_nrodoc
IF FOUND()
     SCAN WHILE nfa_numfac =  ;
          aux_nrodoc .AND.  .NOT.  ;
          EOF()
          SELECT gc_dco00
          SEEK gc_nfa00.nfa_nrodoc +  ;
               gc_nfa00.nfa_codprp
          IF FOUND()
               SELECT gc_pro00
               SEEK gc_nfa00.nfa_codprp
               IF FOUND()
                    w_reg = w_reg +  ;
                            1
                    SELECT mante
                    APPEND BLANK
                    REPLACE pedido  ;
                            WITH  ;
                            gc_nfa00.nfa_nrodoc
                    REPLACE codigo  ;
                            WITH  ;
                            gc_pro00.pro_codpro
                    REPLACE descri  ;
                            WITH  ;
                            gc_pro00.pro_descri
                    REPLACE parara  ;
                            WITH  ;
                            gc_pro00.pro_parara
                    REPLACE codrem  ;
                            WITH  ;
                            gc_dco00.dco_codpro
                    REPLACE modelo  ;
                            WITH  ;
                            gc_pro00.pro_modelo
                    REPLACE unidad  ;
                            WITH  ;
                            gc_pro00.pro_unimed
                    REPLACE cansol  ;
                            WITH  ;
                            gc_dco00.dco_cansol
                    REPLACE cancon  ;
                            WITH  ;
                            gc_nfa00.nfa_cancon
                    REPLACE cancof  ;
                            WITH  ;
                            gc_nfa00.nfa_cancon
                    REPLACE cancel  ;
                            WITH  ;
                            gc_nfa00.nfa_cancel
                    REPLACE cancen  ;
                            WITH  ;
                            gc_nfa00.nfa_cancel
                    REPLACE canlim  ;
                            WITH  ;
                            gc_dco00.dco_canbor +  ;
                            cancon +  ;
                            cancel
                    REPLACE precio  ;
                            WITH  ;
                            gc_dco00.dco_conpre
                    REPLACE canbor  ;
                            WITH  ;
                            gc_dco00.dco_canbor
                    REPLACE indest  ;
                            WITH  ;
                            gc_nfa00.nfa_indest
                    m.hco_totnet =  ;
                     m.hco_totnet +  ;
                     (cancof *  ;
                     precio)
                    w_totcan = w_totcan +  ;
                               cancof
               ENDIF
          ELSE
               w_reg = w_reg - 1
               w_ind = 1
               RETURN
          ENDIF
          SELECT gc_ord00
          SEEK gc_nfa00.nfa_nrodoc +  ;
               gc_nfa00.nfa_codprp
          fila = 0
          IF FOUND()
               k = 0
               SCAN WHILE  ;
                    ord_nrodoc =  ;
                    gc_nfa00.nfa_nrodoc  ;
                    .AND.  ;
                    ord_codprp =  ;
                    gc_nfa00.nfa_codprp  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF ord_numfac =  ;
                       m.hco_numfac  ;
                       .AND. k =  ;
                       0
                         SELECT mante
                         REPLACE ord_ref  ;
                                 WITH  ;
                                 gc_ord00.ord_inorig +  ;
                                 gc_ord00.ord_docref
                         k = 1
                    ENDIF
                    SELECT docu
                    APPEND BLANK
                    REPLACE pedido  ;
                            WITH  ;
                            gc_ord00.ord_nrodoc
                    REPLACE codigo  ;
                            WITH  ;
                            gc_ord00.ord_codprp
                    REPLACE inorig  ;
                            WITH  ;
                            gc_ord00.ord_inorig
                    REPLACE oriant  ;
                            WITH  ;
                            gc_ord00.ord_inorig
                    REPLACE doc  ;
                            WITH  ;
                            gc_ord00.ord_docref
                    REPLACE docant  ;
                            WITH  ;
                            gc_ord00.ord_docref
                    REPLACE numfac  ;
                            WITH  ;
                            gc_ord00.ord_numfac
                    IF numfac =  ;
                       m.hco_numfac
                         REPLACE marca  ;
                                 WITH  ;
                                 'û'
                    ELSE
                         REPLACE marca  ;
                                 WITH  ;
                                 SPACE(1)
                    ENDIF
                    SELECT gc_ord00
               ENDSCAN
          ENDIF
          w_reg = w_reg + 1
          SELECT gc_nfa00
     ENDSCAN
ENDIF
SELECT mante
SHOW POPUP detalle
= oototalnet()
RETURN
*
PROCEDURE oodatped
fila = 0
ON KEY
DO p_footer WITH  ;
   '100000000000000000001', 2
cdemas = pedido + '³' + codigo +  ;
         '³' + SUBSTR(descri, 1,  ;
         9) + '³' +  ;
         TRANSFORM(cansol,  ;
         '@ 99,999') + '³' +  ;
         TRANSFORM(cancof,  ;
         '@ 99,999') + '³' +  ;
         TRANSFORM(precio,  ;
         '@ 99,999.99') + '³' +  ;
         TRANSFORM(cancel,  ;
         '@ 9999') + '³' +  ;
         IIF(w_deta = 0, ord_ref,  ;
         DTOC(feclle))
w_pedido = pedido
w_cansol = cansol
w_canped = cancof
w_precio = precio
w_cancel = cancel
w_cancon = cancon
w_canlim = canlim
w_codrem = codrem
w_unidad = unidad
w_feclle = feclle
w_inorig = SUBSTR(ord_ref, 1, 1)
w_docref = SUBSTR(ord_ref, 2, 10)
w_codpro = codigo
w_parara = parara
ACTIVATE WINDOW IN winpedido  ;
         winped
@ 00, 00 SAY cdemas COLOR SCHEME  ;
  6
@ 01, 00 SAY  ;
  'C¢digo de Remplazo  :' +  ;
  codrem COLOR SCHEME 6
@ 01, 35 SAY '³' +  ;
  TRANSFORM(mante.canlim,  ;
  '99,999') + '³' COLOR SCHEME 6
IF m.hco_nrodoc = '0000000009'
     @ 00, 65 GET w_feclle  ;
       PICTURE '@d'
     READ
     IF LASTKEY() <> 27
          SELECT mante
          REPLACE feclle WITH  ;
                  w_feclle
     ENDIF
ELSE
     @ 00, 43 GET w_canped  ;
       PICTURE '@ 99,999' VALID  ;
       oovalnue(3)
     @ 00, 50 GET w_precio  ;
       PICTURE '@ 99,999.99'  ;
       VALID oovalnue(4)
     @ 00, 60 GET w_cancel  ;
       PICTURE '@ 9999' VALID  ;
       oovalnue(7)
     @ 00, 65 SAY w_inorig COLOR  ;
       SCHEME 6
     @ 00, 66 SAY w_docref COLOR  ;
       SCHEME 6
     READ
     IF LASTKEY() <> 27
          SELECT docu
          GOTO TOP
          w_nreg = 0
          x = w_canped + w_cancel
          w_desm = 0
          COUNT FOR (pedido =  ;
                w_pedido .AND.  ;
                codigo =  ;
                w_codpro) .AND.  ;
                (numfac =  ;
                m.hco_nrodoc .OR.  ;
                numfac =  ;
                SPACE(10)) TO  ;
                w_nreg
          COUNT FOR (pedido =  ;
                w_pedido .AND.  ;
                codigo =  ;
                w_codpro) .AND.  ;
                marca = 'û' TO  ;
                w_desm
          IF (w_canped +  ;
             w_cancel) <= w_nreg
               SEEK w_pedido +  ;
                    w_codpro
               SCAN WHILE  ;
                    w_pedido =  ;
                    pedido .AND.  ;
                    w_codpro =  ;
                    codigo .AND.   ;
                    .NOT. EOF()
                    IF marca =  ;
                       'û'
                         IF x > 0
                              x =  ;
                               x -  ;
                               1
                         ELSE
                              REPLACE  ;
                               marca  ;
                               WITH  ;
                               ' '
                         ENDIF
                    ENDIF
               ENDSCAN
          ELSE
               FOR i = w_nreg + 1  ;
                   TO (w_canped +  ;
                   w_cancel)
                    APPEND BLANK
                    REPLACE pedido  ;
                            WITH  ;
                            w_pedido
                    REPLACE codigo  ;
                            WITH  ;
                            w_codpro
               ENDFOR
          ENDIF
          ACTIVATE WINDOW IN  ;
                   winpedido  ;
                   windoc
          GOTO TOP
          BROWSE FOR codigo =  ;
                 mante.codigo  ;
                 .AND. pedido =  ;
                 mante.pedido  ;
                 FIELDS marca : 1  ;
                 :R :W = .F. :H =  ;
                 'Marca', inorig  ;
                 : 1 :P = '!' :W =  ;
                 oowhen(3) :V =  ;
                 valref(1) :H =  ;
                 'T', doc : 10 :W =  ;
                 oowhen(10) :V =  ;
                 valref(2) :H =  ;
                 ' Doc.Ref. ' :P =  ;
                 '@!' IN windoc  ;
                 NOLGRID
          IF LASTKEY() = 27
               DEACTIVATE WINDOW  ;
                          windoc
               SELECT docu
               SEEK w_pedido +  ;
                    w_codpro
               h = 0
               w_inorig = SPACE(1)
               w_docref = SPACE(10)
               SCAN WHILE pedido =  ;
                    w_pedido  ;
                    .AND. codigo =  ;
                    w_codpro  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF marca =  ;
                       'û'
                         IF h = 0
                              w_inorig =  ;
                               inorig
                              w_docref =  ;
                               doc
                              h =  ;
                               1
                         ENDIF
                         IF oriant =  ;
                            SPACE(1)  ;
                            .AND.  ;
                            docant =  ;
                            SPACE(10)
                              REPLACE  ;
                               oriant  ;
                               WITH  ;
                               inorig
                              REPLACE  ;
                               docant  ;
                               WITH  ;
                               doc
                         ENDIF
                    ENDIF
               ENDSCAN
          ENDIF
          fla = .T.
          ACTIVATE WINDOW IN  ;
                   winpedido  ;
                   winped
          @ 00, 65 SAY w_inorig  ;
            COLOR SCHEME 6
          @ 00, 66 SAY w_docref  ;
            COLOR SCHEME 6
          DO WHILE fla
               @ 01, 21 GET  ;
                 w_codrem  ;
                 FUNCTION '@!'  ;
                 VALID valrem()  ;
                 WHEN oovalnue(6)  ;
                 COLOR N/W 
               READ
               IF LASTKEY() <> 27  ;
                  .AND. w_codrem <>  ;
                  SPACE(14)
                    SELECT gc_pro00
                    SET ORDER TO codigo
                    SEEK w_codrem
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            ' C¢digo de Producto No Existe en la Ficha'
                         LOOP
                    ENDIF
                    IF pro_rcom =  ;
                       'Y'
                         DO p_mensaje  ;
                            WITH  ;
                            ' C¢digo de Producto Esta en Fin de Servicio'
                         LOOP
                    ENDIF
               ENDIF
               fla = .F.
          ENDDO
          IF LASTKEY() <> 27
               SELECT mante
               SEEK w_pedido +  ;
                    w_parara +  ;
                    w_codpro
               REPLACE codrem  ;
                       WITH  ;
                       w_codrem
               REPLACE cancof  ;
                       WITH  ;
                       w_canped
               REPLACE precio  ;
                       WITH  ;
                       w_precio
               REPLACE cancel  ;
                       WITH  ;
                       w_cancel
               REPLACE canbor  ;
                       WITH  ;
                       canlim -  ;
                       (cancof +  ;
                       cancel)
               REPLACE ord_ref  ;
                       WITH  ;
                       w_inorig +  ;
                       w_docref
          ENDIF
     ENDIF
ENDIF
DEACTIVATE WINDOW winped
= oototalnet()
SELECT mante
DO oopasar
RETURN
*
FUNCTION valrem
IF w_codpro <> w_codrem .AND.  ;
   w_codrem <> SPACE(14)
     SELECT gc_nfa00
     SET ORDER TO nfa_docprp
     SEEK w_pedido + w_codpro +  ;
          'P'
     IF FOUND() .AND. nfa_numfac <>  ;
        m.hco_nrodoc
          SET ORDER TO nfa_facprp
          DO p_mensaje WITH  ;
             ' No se Puede Modificar el C¢digo de Reemplazo'
          RETURN .F.
     ELSE
          SET ORDER TO nfa_facprp
     ENDIF
ENDIF
RETURN
*
FUNCTION valref
PARAMETER ind
IF ind = 1 .AND. docu.inorig <>  ;
   SPACE(1)
     SELECT ge_tab0
     SEEK 'OPED' + docu.inorig
     IF  .NOT. FOUND()
          SELECT docu
          DO p_mensaje WITH  ;
             ' Tipo de Documento No existe '
          RETURN .F.
     ENDIF
ENDIF
RETURN
*
PROCEDURE marca
IF (w_canped + w_cancel) <= 0
     DO p_mensaje WITH  ;
        'Cantidad Confirmada + Cancelada es Igual Cero '
ELSE
     IF marca = 'û'
          REPLACE marca WITH  ;
                  SPACE(1)
          w_desm = w_desm - 1
     ELSE
          IF numfac <>  ;
             m.hco_nrodoc .AND.  ;
             numfac <> SPACE(10)
               DO p_mensaje WITH  ;
                  'Ya se confirm¢ con otro Nro.Factura'
          ELSE
               IF w_desm <  ;
                  (w_canped +  ;
                  w_cancel)
                    REPLACE marca  ;
                            WITH  ;
                            'û'
                    w_desm = w_desm +  ;
                             1
               ENDIF
          ENDIF
     ENDIF
ENDIF
RETURN
*
PROCEDURE bustip
w_busca = 'OPED'
w_var = 'w_inorig'
DO busca WITH w_busca, w_var
IF LASTKEY() = 13
     SELECT docu
     REPLACE inorig WITH w_inorig
ENDIF
ON KEY LABEL spacebar do marca
ON KEY LABEL f6 do bustip
RETURN
*
FUNCTION pedido
DEFINE WINDOW pan1 FROM 13, 30 TO  ;
       15, 50 IN screen COLOR  ;
       SCHEME 7
ACTIVATE WINDOW pan1
@ 00, 00 SAY 'PEDIDO: '
w_nrodoc = SPACE(10)
flag = .F.
ON KEY
@ 00, 08 GET w_nrodoc PICTURE  ;
  '@!' WHEN oowhen(5)
READ
IF LASTKEY() = 27 .OR.  ;
   EMPTY(w_nrodoc)
     DEACTIVATE WINDOW pan1
ELSE
     SELECT gc_hco00
     SEEK w_nrodoc
     IF  .NOT. FOUND()
          DO p_mensaje WITH  ;
             'C¢digo No Existe'
          DEACTIVATE WINDOW pan1
     ELSE
          DO CASE
               CASE hco_indest =  ;
                    'S' .OR.  ;
                    hco_indest =  ;
                    'P' .OR.  ;
                    hco_indest =  ;
                    'C' .OR.  ;
                    hco_indest =  ;
                    'R'
                    flag = .T.
               CASE hco_indest =  ;
                    'U'
                    DO p_mensaje  ;
                       WITH  ;
                       'Pedido en Sugerido'
                    flag = .F.
               CASE hco_indest =  ;
                    'A'
                    DO p_mensaje  ;
                       WITH  ;
                       'Pedido Anulado'
                    flag = .F.
          ENDCASE
     ENDIF
     IF  .NOT. flag
          DEACTIVATE WINDOW pan1
     ELSE
          DEACTIVATE WINDOW pan1
          c = 0
          SELECT gc_dco00
          SEEK w_nrodoc
          SCAN WHILE dco_nrodoc =  ;
               w_nrodoc .AND.   ;
               .NOT. EOF()
               IF (dco_indest =  ;
                  'S' .AND.  ;
                  dco_cansol > 0)  ;
                  .OR.  ;
                  (dco_indest <>  ;
                  'S' .AND.  ;
                  dco_canbor >  ;
                  0)
                    SELECT gc_pro00
                    SEEK gc_dco00.dco_codprp
                    IF FOUND()
                         c = c +  ;
                             1
                         w_reg = w_reg +  ;
                                 1
                         SELECT mante
                         LOCATE FOR  ;
                                w_nrodoc =  ;
                                pedido  ;
                                .AND.  ;
                                gc_dco00.dco_codprp =  ;
                                codigo
                         IF  .NOT.  ;
                             FOUND()
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               pedido  ;
                               WITH  ;
                               gc_dco00.dco_nrodoc
                              REPLACE  ;
                               codigo  ;
                               WITH  ;
                               gc_pro00.pro_codpro
                              REPLACE  ;
                               descri  ;
                               WITH  ;
                               gc_pro00.pro_descri
                              REPLACE  ;
                               parara  ;
                               WITH  ;
                               gc_pro00.pro_parara
                              REPLACE  ;
                               codrem  ;
                               WITH  ;
                               gc_dco00.dco_codpro
                              REPLACE  ;
                               modelo  ;
                               WITH  ;
                               gc_pro00.pro_modelo
                              REPLACE  ;
                               unidad  ;
                               WITH  ;
                               gc_pro00.pro_unimed
                              REPLACE  ;
                               cansol  ;
                               WITH  ;
                               gc_dco00.dco_cansol
                              REPLACE  ;
                               cancon  ;
                               WITH  ;
                               0
                              REPLACE  ;
                               cancen  ;
                               WITH  ;
                               0
                              REPLACE  ;
                               cancof  ;
                               WITH  ;
                               0
                              REPLACE  ;
                               cancel  ;
                               WITH  ;
                               0
                              IF gc_dco00.dco_indest =  ;
                                 'S'
                                   REPLACE canlim WITH gc_dco00.dco_cansol
                              ELSE
                                   REPLACE canlim WITH gc_dco00.dco_canbor
                              ENDIF
                              REPLACE  ;
                               canbor  ;
                               WITH  ;
                               gc_dco00.dco_canbor
                              REPLACE  ;
                               precio  ;
                               WITH  ;
                               gc_dco00.dco_valpre
                              DO CASE
                                   CASE gc_dco00.dco_indest = 'S' .OR. gc_dco00.dco_indest = 'P'
                                        REPLACE indest WITH 'P'
                                   CASE gc_dco00.dco_indest = 'C' .OR. gc_dco00.dco_indest = 'R'
                                        REPLACE indest WITH 'R'
                              ENDCASE
                              m.hco_totnet =  ;
                               m.hco_totnet +  ;
                               (cancof *  ;
                               precio)
                              w_totcan =  ;
                               w_totcan +  ;
                               cancof
                              SELECT  ;
                               gc_ord00
                              SEEK  ;
                               gc_dco00.dco_nrodoc +  ;
                               gc_dco00.dco_codprp
                              IF FOUND()
                                   SCAN WHILE ord_nrodoc = gc_dco00.dco_nrodoc .AND. ord_codprp = gc_dco00.dco_codprp .AND.  .NOT. EOF()
                                        SELECT docu
                                        APPEND BLANK
                                        REPLACE pedido WITH gc_ord00.ord_nrodoc
                                        REPLACE codigo WITH gc_ord00.ord_codprp
                                        REPLACE inorig WITH gc_ord00.ord_inorig
                                        REPLACE oriant WITH gc_ord00.ord_inorig
                                        REPLACE doc WITH gc_ord00.ord_docref
                                        REPLACE docant WITH gc_ord00.ord_docref
                                        REPLACE numfac WITH gc_ord00.ord_numfac
                                        REPLACE marca WITH SPACE(1)
                                        SELECT gc_ord00
                                   ENDSCAN
                              ENDIF
                         ENDIF
                    ELSE
                         w_reg = w_reg +  ;
                                 1
                         DO p_mensaje  ;
                            WITH  ;
                            ' C¢digo no se encontr¢ en la ficha '+ ;
                            gc_dco00.dco_codprp
                         EXIT
                    ENDIF
                    SELECT gc_dco00
               ENDIF
          ENDSCAN
          IF c = 0
               DO p_mensaje WITH  ;
                  'No Tiene Registros para El Proceso'
          ENDIF
     ENDIF
ENDIF
DEACTIVATE WINDOW pan1
ON KEY LABEL f6
= oototalnet()
SELECT mante
DO oopasar
RETURN .T.
*
PROCEDURE llenatem
DO p_mensaje1 WITH  ;
   ' Espere un momento ...'
STORE 0 TO c, w_reg
SELECT gc_dco00
GOTO TOP
SCAN WHILE  .NOT. EOF()
     IF dco_canbor > 0 .AND.  ;
        (dco_indest = 'P' .OR.  ;
        dco_indest = 'C' .OR.  ;
        dco_indest = 'R')
          SELECT gc_pro00
          SEEK gc_dco00.dco_codprp
          IF FOUND()
               c = c + 1
               w_reg = w_reg + 1
               SELECT mante
               APPEND BLANK
               REPLACE pedido  ;
                       WITH  ;
                       gc_dco00.dco_nrodoc
               REPLACE codigo  ;
                       WITH  ;
                       gc_pro00.pro_codpro
               REPLACE descri  ;
                       WITH  ;
                       gc_pro00.pro_descri
               REPLACE parara  ;
                       WITH  ;
                       gc_pro00.pro_parara
               REPLACE codrem  ;
                       WITH  ;
                       gc_dco00.dco_codpro
               REPLACE modelo  ;
                       WITH  ;
                       gc_pro00.pro_modelo
               REPLACE unidad  ;
                       WITH  ;
                       gc_pro00.pro_unimed
               REPLACE cansol  ;
                       WITH  ;
                       gc_dco00.dco_cansol
               REPLACE canbor  ;
                       WITH  ;
                       gc_dco00.dco_canbor
               REPLACE cancel  ;
                       WITH  ;
                       gc_dco00.dco_cancel
               REPLACE cancof  ;
                       WITH 0
               REPLACE feclle  ;
                       WITH  ;
                       gc_dco00.dco_feclle
               REPLACE precio  ;
                       WITH  ;
                       gc_dco00.dco_valpre
               m.hco_totnet = m.hco_totnet +  ;
                              (cancof *  ;
                              precio)
               w_totcan = w_totcan +  ;
                          cancof
          ELSE
               w_reg = w_reg - 1
               DO p_mensaje WITH  ;
                  ' C¢digo no se encontr¢ en la ficha '+ ;
                  gc_dco00.dco_nrodoc+ ;
                  ' '+ ;
                  gc_dco00.dco_codprp
          ENDIF
     ENDIF
     SELECT gc_dco00
ENDSCAN
SELECT mante
IF RECCOUNT() = 0 .OR. c = 0
     DO p_mensaje WITH  ;
        'No Tiene Registros para el Proceso'
     RETURN
ELSE
     RELEASE WINDOW mensj
ENDIF
GOTO TOP
SHOW POPUP detalle
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
          ON KEY LABEL f6
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
          SEEK w_pedido +  ;
               w_parara +  ;
               w_codpro
          IF FOUND()
               DO p_mensaje WITH  ;
                  ' Producto repetido del Pedido '
               RETURN .F.
          ENDIF
          SELECT gc_pro00
          w_descri = pro_descri
          w_unimed = pro_unimed
          w_parara = pro_parara
          w_modelo = pro_modelo
          w_des = despro(w_codpro)
          w_precio = ROUND(pro_coremo *  ;
                     ((100 -  ;
                     w_des) /  ;
                     100), 2)
     CASE nvalnue = 3
          IF w_canped < 0
               DO p_mensaje WITH  ;
                  ' Cantidad debe ser Mayor o Igual a Cero  '
               RETURN .F.
          ENDIF
          IF w_canped > w_canlim
               DO p_mensaje WITH  ;
                  ' Cantidad no debe ser Mayor a:'+ ;
                  STR(w_canlim, 6,  ;
                  0)
               RETURN .F.
          ENDIF
     CASE nvalnue = 4
          IF w_precio < 0
               DO p_mensaje WITH  ;
                  ' Precio debe ser Positivo'
               RETURN .F.
          ENDIF
          IF w_dscto > 0
               w_precio = w_precio *  ;
                          ((100 -  ;
                          w_dscto) /  ;
                          100)
          ENDIF
          @ 00, 50 SAY w_precio  ;
            PICTURE  ;
            '@ 99,999.99'
     CASE nvalnue = 5
          w_busca = 'OPED'
          w_var = inorig
          ON KEY LABEL f6 do busca with;
w_busca,w_var
     CASE nvalnue = 6
          w_selec = SELECT()
          w_campo = 'w_codrem'
          ON KEY LABEL f6 do produc with;
w_campo,w_selec,w_selpro
     CASE nvalnue = 7
          IF w_cancel < 0
               DO p_mensaje WITH  ;
                  ' Cantidad  debe ser Mayor o Igual a Cero  '
               RETURN .F.
          ELSE
               IF w_cancel >  ;
                  (w_canlim -  ;
                  w_canped)
                    DO p_mensaje  ;
                       WITH  ;
                       'Cantidad No debe ser Mayor a: '+ ;
                       STR((w_canlim- ;
                       w_canped),  ;
                       6, 0)
                    RETURN .F.
               ENDIF
          ENDIF
ENDCASE
RETURN
*
PROCEDURE oototalnet
m.hco_totnet = 0
w_totcan = 0
w_item = 0
SELECT mante
GOTO TOP
SCAN WHILE  .NOT. EOF()
     w_item = w_item + 1
     m.hco_totnet = m.hco_totnet +  ;
                    (cancof *  ;
                    precio)
     w_totcan = w_totcan + cancof
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
PROCEDURE grabar
PARAMETER ntab
ON KEY
ACTIVATE WINDOW winpedido
@ 16, 40 GET m.hco_pordes PICTURE  ;
  '@ 99.99' VALID  ;
  oovalid(VARREAD()) COLOR W/N,W+/ ;
  N 
@ 16, 52 GET m.hco_totdes PICTURE  ;
  '@ 999,999.99' VALID  ;
  oovalid(VARREAD()) COLOR W/N,W+/ ;
  N 
READ
IF LASTKEY() <> 27
     m.hco_totgen = m.hco_totnet -  ;
                    ROUND(m.hco_totdes,  ;
                    2)
     SHOW GET m.hco_totgen  ;
          DISABLE
     m.hco_totigv = m.hco_totgen *  ;
                    w_facigv
     m.hco_totoim = m.hco_totgen +  ;
                    m.hco_totigv
     DO oograbar WITH ntab
     SELECT gc_hco00
     SCATTER BLANK MEMVAR
     DO ooinicio
     DEACTIVATE POPUP detalle
     DO ooscreen(2)
     SHOW GET m.hco_nrodoc ENABLE
     ACTIVATE WINDOW winpedido
     CLEAR GETS
     CLEAR READ
ELSE
     DO oopasar
ENDIF
RETURN
*
PROCEDURE oograbar
PARAMETER ngrabar
IF w_reg = 0
     DO p_mensaje WITH  ;
        'No ha Ingresado Ning£n Item '
     RETURN
ENDIF
IF EMPTY(m.hco_nrodoc)
     DO p_mensaje WITH  ;
        ' Falta n£mero de documento '
     RETURN
ENDIF
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
RELEASE WINDOW mensj
DEACTIVATE WINDOW footer
sigue = .F.
RETURN
*
PROCEDURE graba1
IF m.hco_indest = 'S' .OR.  ;
   m.hco_indest = 'P'
     m.hco_indest = 'P'
ENDIF
m.hco_indes2 = m.hco_indest
m.hco_usuari = clave
m.hco_fecha = DATE()
m.hco_hora = TIME()
m.hco_numfac = m.hco_nrodoc
w_vacio = 0
SELECT mante
GOTO TOP
IF m.hco_nrodoc <> '0000000009'
     SCAN WHILE  .NOT. EOF()
          REPLACE canbor WITH  ;
                  canlim -  ;
                  (cancof +  ;
                  cancel)
          SELECT gc_dco00
          SEEK mante.pedido +  ;
               mante.codigo
          SELECT gc_pro00
          IF EMPTY(gc_dco00.dco_codpro)
               IF EMPTY(mante.codrem)
                    SEEK mante.codigo
                    IF FOUND()
                         DO rbloquea
                         REPLACE pro_stksol  ;
                                 WITH  ;
                                 (pro_stksol -  ;
                                 gc_dco00.dco_cansol) +  ;
                                 (gc_dco00.dco_cancon +  ;
                                 gc_dco00.dco_canbor +  ;
                                 gc_dco00.dco_cancel)
                         REPLACE pro_stkbor  ;
                                 WITH  ;
                                 (pro_stkbor -  ;
                                 gc_dco00.dco_canbor) +  ;
                                 mante.canbor
                         REPLACE pro_stktra  ;
                                 WITH  ;
                                 (pro_stktra -  ;
                                 gc_dco00.dco_cancon) +  ;
                                 mante.cancof
                         REPLACE pro_usuari  ;
                                 WITH  ;
                                 clave,  ;
                                 pro_hora  ;
                                 WITH  ;
                                 TIME(),  ;
                                 pro_fecha  ;
                                 WITH  ;
                                 DATE()
                         IF m.hco_codent <>  ;
                            '37270628   '
                              REPLACE  ;
                               pro_coremo  ;
                               WITH  ;
                               mante.precio
                         ENDIF
                         UNLOCK
                    ENDIF
                    SELECT mante
                    REPLACE codrem  ;
                            WITH  ;
                            codigo
               ELSE
                    SEEK (mante.codigo)
                    IF FOUND()
                         DO rbloquea
                         REPLACE pro_stksol  ;
                                 WITH  ;
                                 (pro_stksol -  ;
                                 gc_dco00.dco_cansol)
                         REPLACE pro_codree  ;
                                 WITH  ;
                                 mante.codrem
                         REPLACE pro_rcom  ;
                                 WITH  ;
                                 'H'
                         REPLACE pro_usuari  ;
                                 WITH  ;
                                 clave,  ;
                                 pro_hora  ;
                                 WITH  ;
                                 TIME(),  ;
                                 pro_fecha  ;
                                 WITH  ;
                                 DATE()
                         UNLOCK
                    ENDIF
                    SEEK (mante.codrem)
                    IF FOUND()
                         DO rbloquea
                         REPLACE pro_stkbor  ;
                                 WITH  ;
                                 (pro_stkbor +  ;
                                 mante.canbor)
                         REPLACE pro_stktra  ;
                                 WITH  ;
                                 (pro_stktra +  ;
                                 mante.cancof)
                         IF pro_rcom =  ;
                            'H'
                              REPLACE  ;
                               pro_rcom  ;
                               WITH  ;
                               pro_nivcit
                         ENDIF
                         REPLACE pro_usuari  ;
                                 WITH  ;
                                 clave,  ;
                                 pro_hora  ;
                                 WITH  ;
                                 TIME(),  ;
                                 pro_fecha  ;
                                 WITH  ;
                                 DATE()
                         IF m.hco_codent <>  ;
                            '37270628   '
                              REPLACE  ;
                               pro_coremo  ;
                               WITH  ;
                               mante.precio
                         ENDIF
                         UNLOCK
                    ENDIF
               ENDIF
          ELSE
               IF gc_dco00.dco_codpro =  ;
                  mante.codrem
                    SEEK (mante.codrem)
                    IF FOUND()
                         DO rbloquea
                         REPLACE pro_stkbor  ;
                                 WITH  ;
                                 (pro_stkbor -  ;
                                 gc_dco00.dco_canbor) +  ;
                                 mante.canbor
                         REPLACE pro_stktra  ;
                                 WITH  ;
                                 (pro_stktra -  ;
                                 mante.cancon) +  ;
                                 mante.cancof
                         REPLACE pro_usuari  ;
                                 WITH  ;
                                 clave,  ;
                                 pro_hora  ;
                                 WITH  ;
                                 TIME(),  ;
                                 pro_fecha  ;
                                 WITH  ;
                                 DATE()
                         IF m.hco_codent <>  ;
                            '37270628   '
                              REPLACE  ;
                               pro_coremo  ;
                               WITH  ;
                               mante.precio
                         ENDIF
                         UNLOCK
                    ENDIF
               ELSE
                    SEEK (gc_dco00.dco_codpro)
                    IF FOUND()
                         DO rbloquea
                         REPLACE pro_stkbor  ;
                                 WITH  ;
                                 (pro_stkbor -  ;
                                 gc_dco00.dco_canbor)
                         REPLACE pro_stktra  ;
                                 WITH  ;
                                 (pro_stktra -  ;
                                 mante.cancon)
                         REPLACE pro_codree  ;
                                 WITH  ;
                                 mante.codrem
                         REPLACE pro_rcom  ;
                                 WITH  ;
                                 'H'
                         REPLACE pro_usuari  ;
                                 WITH  ;
                                 clave,  ;
                                 pro_hora  ;
                                 WITH  ;
                                 TIME(),  ;
                                 pro_fecha  ;
                                 WITH  ;
                                 DATE()
                         UNLOCK
                    ENDIF
                    SEEK (mante.codrem)
                    IF FOUND()
                         DO rbloquea
                         REPLACE pro_stkbor  ;
                                 WITH  ;
                                 (pro_stkbor +  ;
                                 mante.canbor)
                         REPLACE pro_stktra  ;
                                 WITH  ;
                                 (pro_stktra +  ;
                                 mante.cancof)
                         IF pro_rcom =  ;
                            'H'
                              REPLACE  ;
                               pro_rcom  ;
                               WITH  ;
                               pro_nivcit
                         ENDIF
                         REPLACE pro_usuari  ;
                                 WITH  ;
                                 clave,  ;
                                 pro_hora  ;
                                 WITH  ;
                                 TIME(),  ;
                                 pro_fecha  ;
                                 WITH  ;
                                 DATE()
                         IF m.hco_codent <>  ;
                            '37270628   '
                              REPLACE  ;
                               pro_coremo  ;
                               WITH  ;
                               mante.precio
                         ENDIF
                         UNLOCK
                    ENDIF
               ENDIF
          ENDIF
          SELECT gc_dco00
          IF SEEK(mante.pedido +  ;
             mante.codigo)
               SELECT gc_nfa00
               IF mante.codigo =  ;
                  mante.codrem
                    SEEK m.hco_numfac +  ;
                         mante.pedido +  ;
                         mante.codrem
               ELSE
                    SEEK m.hco_numfac +  ;
                         mante.pedido +  ;
                         gc_dco00.dco_codprp
               ENDIF
               STORE 0 TO w_ask
               IF FOUND()
                    DO rbloquea
                    IF mante.cancof >  ;
                       0 .OR.  ;
                       mante.cancel >  ;
                       0
                         IF mante.cancof >  ;
                            0
                              STORE  ;
                               1  ;
                               TO  ;
                               w_ask
                         ENDIF
                         REPLACE nfa_numfac  ;
                                 WITH  ;
                                 m.hco_numfac,  ;
                                 nfa_codpro  ;
                                 WITH  ;
                                 mante.codrem
                         REPLACE nfa_cancon  ;
                                 WITH  ;
                                 mante.cancof,  ;
                                 nfa_cancel  ;
                                 WITH  ;
                                 mante.cancel
                         REPLACE nfa_indest  ;
                                 WITH  ;
                                 mante.indest,  ;
                                 nfa_indes2  ;
                                 WITH  ;
                                 mante.indest
                         REPLACE nfa_fecha  ;
                                 WITH  ;
                                 DATE(),  ;
                                 nfa_hora  ;
                                 WITH  ;
                                 TIME(),  ;
                                 nfa_usuari  ;
                                 WITH  ;
                                 clave
                    ELSE
                         DELETE
                    ENDIF
                    UNLOCK
               ELSE
                    IF mante.cancof >  ;
                       0 .OR.  ;
                       mante.cancel >  ;
                       0
                         STORE 1  ;
                               TO  ;
                               w_ask
                         APPEND BLANK
                         DO rbloquea
                         REPLACE nfa_numfac  ;
                                 WITH  ;
                                 m.hco_numfac,  ;
                                 nfa_nrodoc  ;
                                 WITH  ;
                                 mante.pedido
                         REPLACE nfa_codpro  ;
                                 WITH  ;
                                 mante.codrem,  ;
                                 nfa_codprp  ;
                                 WITH  ;
                                 mante.codigo
                         REPLACE nfa_cancon  ;
                                 WITH  ;
                                 mante.cancof,  ;
                                 nfa_cancel  ;
                                 WITH  ;
                                 mante.cancel
                         REPLACE nfa_indest  ;
                                 WITH  ;
                                 mante.indest,  ;
                                 nfa_indes2  ;
                                 WITH  ;
                                 mante.indest
                         REPLACE nfa_fecha  ;
                                 WITH  ;
                                 DATE(),  ;
                                 nfa_usuari  ;
                                 WITH  ;
                                 clave,  ;
                                 nfa_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                    ENDIF
               ENDIF
               SELECT gc_dco00
               DO rbloquea
               REPLACE dco_numfac  ;
                       WITH  ;
                       m.hco_numfac
               REPLACE dco_codprp  ;
                       WITH  ;
                       mante.codigo
               REPLACE dco_codpro  ;
                       WITH  ;
                       mante.codrem
               REPLACE dco_coduni  ;
                       WITH  ;
                       mante.unidad
               REPLACE dco_cancon  ;
                       WITH  ;
                       (dco_cancon -  ;
                       mante.cancon) +  ;
                       mante.cancof
               REPLACE dco_cancel  ;
                       WITH  ;
                       (dco_cancel -  ;
                       mante.cancen) +  ;
                       mante.cancel
               REPLACE dco_canbor  ;
                       WITH  ;
                       mante.canbor
               REPLACE dco_conpre  ;
                       WITH  ;
                       mante.precio
               REPLACE dco_inorig  ;
                       WITH  ;
                       SUBSTR(mante.ord_ref,  ;
                       1, 1)
               REPLACE dco_docref  ;
                       WITH  ;
                       SUBSTR(mante.ord_ref,  ;
                       2, 10)
               IF w_ask = 1
                    REPLACE dco_feclle  ;
                            WITH  ;
                            m.hco_feclle
               ELSE
                    IF dco_indest =  ;
                       'S'
                         REPLACE dco_feclle  ;
                                 WITH  ;
                                 CTOD( ;
                                 '  /  /    ' ;
                                 )
                         REPLACE dco_numfac  ;
                                 WITH  ;
                                 SPACE(10)
                    ENDIF
               ENDIF
               REPLACE dco_indest  ;
                       WITH  ;
                       mante.indest
               REPLACE dco_usuari  ;
                       WITH  ;
                       clave
               REPLACE dco_hora  ;
                       WITH  ;
                       TIME()
               REPLACE dco_fecha  ;
                       WITH  ;
                       DATE()
               UNLOCK
          ENDIF
          SELECT docu
          SEEK mante.pedido +  ;
               mante.codigo
          IF FOUND()
               SCAN WHILE (pedido =  ;
                    mante.pedido  ;
                    .AND. codigo =  ;
                    mante.codigo)  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF inorig <>  ;
                       SPACE(1)  ;
                       .AND. doc <>  ;
                       SPACE(10)
                         SELECT gc_ord00
                         SEEK mante.pedido +  ;
                              docu.codigo
                         w_enc = .F.
                         SCAN WHILE  ;
                              mante.pedido =  ;
                              ord_nrodoc  ;
                              .AND.  ;
                              docu.codigo =  ;
                              ord_codprp  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              IF docu.oriant =  ;
                                 ord_inorig  ;
                                 .AND.  ;
                                 docu.docant =  ;
                                 ord_docref
                                   w_enc = .T.
                                   EXIT
                              ENDIF
                         ENDSCAN
                         IF w_enc
                              IF docu.marca =  ;
                                 'û'
                                   DO rbloquea
                                   REPLACE ord_numfac WITH m.hco_numfac
                                   REPLACE ord_feclle WITH m.hco_feclle
                                   REPLACE ord_inorig WITH docu.inorig, ord_docref WITH docu.doc
                                   REPLACE ord_codpro WITH mante.codrem, ord_indest WITH mante.indest
                                   REPLACE ord_fecha WITH DATE(), ord_hora WITH TIME(), ord_usuari WITH clave
                                   UNLOCK
                              ELSE
                                   IF m.hco_numfac = ord_numfac
                                        DO rbloquea
                                        REPLACE ord_numfac WITH SPACE(10), ord_feclle WITH {}
                                        UNLOCK
                                   ENDIF
                              ENDIF
                         ELSE
                              IF docu.marca =  ;
                                 'û'
                                   APPEND BLANK
                                   DO rbloquea
                                   REPLACE ord_nrodoc WITH mante.pedido, ord_codprp WITH docu.codigo
                                   REPLACE ord_codpro WITH mante.codrem, ord_numfac WITH m.hco_numfac
                                   REPLACE ord_inorig WITH docu.inorig, ord_docref WITH docu.doc
                                   REPLACE ord_feclle WITH m.hco_feclle, ord_indest WITH mante.indest
                                   REPLACE ord_fecha WITH DATE(), ord_hora WITH TIME(), ord_usuari WITH clave
                                   UNLOCK
                              ELSE
                                   APPEND BLANK
                                   DO rbloquea
                                   REPLACE ord_nrodoc WITH mante.pedido, ord_codprp WITH docu.codigo
                                   REPLACE ord_codpro WITH mante.codrem, ord_inorig WITH docu.inorig, ord_docref WITH docu.doc
                                   REPLACE ord_indest WITH 'S'
                                   REPLACE ord_fecha WITH DATE(), ord_hora WITH TIME(), ord_usuari WITH clave
                                   UNLOCK
                              ENDIF
                         ENDIF
                         SELECT docu
                    ENDIF
               ENDSCAN
          ENDIF
          SELECT gc_hco00
          SEEK mante.pedido
          IF FOUND()
               DO rbloquea
               IF (hco_indest =  ;
                  'S' .AND.  ;
                  (mante.cancof +  ;
                  mante.cancel) >  ;
                  0) .OR.  ;
                  (hco_indest =  ;
                  'P' .AND.  ;
                  hco_numfac =  ;
                  m.hco_numfac)
                    REPLACE hco_feccon  ;
                            WITH  ;
                            m.hco_feccon
                    REPLACE hco_feclle  ;
                            WITH  ;
                            m.hco_feclle
                    REPLACE hco_numfac  ;
                            WITH  ;
                            m.hco_numfac
                    REPLACE hco_indest  ;
                            WITH  ;
                            'P'
                    REPLACE hco_indes2  ;
                            WITH  ;
                            'P'
               ENDIF
               UNLOCK
          ENDIF
          SELECT mante
     ENDSCAN
     SELECT gc_hco00
     SEEK m.hco_nrodoc
     IF  .NOT. FOUND()
          APPEND BLANK
     ENDIF
     DO rbloquea
     GATHER MEMVAR
     UNLOCK
     DO p_mensaje WITH  ;
        ' DATOS DEL PEDIDO '+ ;
        m.hco_nrodoc+ ;
        ' CONFIRMADOS '
ELSE
     SCAN WHILE  .NOT. EOF()
          SELECT gc_dco00
          SEEK mante.pedido +  ;
               mante.codigo
          IF FOUND()
               DO rbloquea
               REPLACE dco_feclle  ;
                       WITH  ;
                       mante.feclle
               UNLOCK
          ENDIF
          SELECT mante
     ENDSCAN
ENDIF
DO ooinicio
CLEAR READ
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
@ 00, 02 SAY 'NUMFAC:' COLOR W/N, ;
  N/W  GET m.hco_numfac VALID  ;
  numfac()
@ 01, 02 GET m.hco_obser DEFAULT  ;
  ' ' SIZE 1, 60 COLOR W/N,N/W 
@ 03, 25 GET nop DEFAULT 1 SIZE 1,  ;
  11, 1 PICTURE '@*HN Continuar'  ;
  VALID confirma()
READ CYCLE
RELEASE WINDOW mensaje
*
PROCEDURE confirma
CLEAR READ
RETURN
*
FUNCTION numfac
m.hco_numfac = f_ceros(m.hco_numfac, ;
               10,2)
IF m.hco_numfac = '0000000000'
     DO p_mensaje WITH  ;
        'Documento en Blanco'
     RETURN .F.
ENDIF
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
            'Nro. Factura...:'
          @ 00, 47 SAY  ;
            'Fecha Documento.....:'
          @ 01, 01 SAY  ;
            'Proveedor......:'
          @ 01, 47 SAY  ;
            'Fecha LLegada.......:'
          @ 02, 01 SAY  ;
            'Moneda.........:'
          @ 02, 47 SAY  ;
            'Fecha Moneda........:'
          @ 03, 01 SAY  ;
            'Descuento (%) .:'
          @ 03, 47 SAY  ;
            'Fecha Confirmaci¢n..:'
          @ 04, 00 SAY  ;
            'Nro.Pedido     C¢digo     Descrip.  C.Sol. C.Conf.  P.Conf. C.Canc. Doc.Ref.  '  ;
            COLOR N/W 
          @ 15, 00 SAY  ;
            ' Item   Total Cant.     Total Neto   %Descuento    Total Dscto   Total General'  ;
            COLOR N/W 
ENDCASE
RETURN
*
PROCEDURE ooprint
SELECT mante
GOTO TOP
??? CHR(15)
REPORT FORMAT agcp2213 TO PRINTER  ;
       NOCONSOLE FOR cancof +  ;
       cancel > 0
??? CHR(27) + CHR(80)
RETURN
*
PROCEDURE createm
CREATE CURSOR docu (pedido C (10),  ;
       codigo C (14), marca C (1),  ;
       inorig C (1), oriant C (1),  ;
       docant C (10), doc C (10),  ;
       numfac C (10))
w_idx2 = f_indice()
index on pedido+codigo to &w_idx2
CREATE CURSOR mante (pedido C  ;
       (10), codigo C (14),  ;
       descri C (40), unidad C  ;
       (4), cansol N (6, 0),  ;
       cancon N (6, 0), cancof N  ;
       (6, 0), precio N (10, 2),  ;
       cancel N (6, 0), canbor N  ;
       (6, 0), ord_ref C (11),  ;
       codrem C (14), modelo C  ;
       (20), indest C (1), canlim  ;
       N (6, 0), cancen N (6, 0),  ;
       parara C (13), feclle D)
w_idx3 = f_indice()
index on pedido+parara+codigo to &w_idx3
RETURN
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
PARAMETER opc
ON KEY
SELECT gc_hco00
IF opc = 1
     SET FILTER TO (hco_indest <> 'C';
.AND. hco_numfac = hco_nrodoc;
.AND. hco_indest <> 'A')
     campo = 'transform(hco_numfac, "@ 9999999999") + "³" + transform(hco_nrodoc, "@ 9999999999") + "³" + dtoc(hco_fecdoc) + "³" + iif(hco_indest="S","SOLICITADO",iif(hco_indest="P","PROCESO   ",iif(hco_indest="R","REPROCESO","SUGERIDO")))'
ELSE
     SET FILTER TO (hco_indest = 'S';
.OR. hco_indest = 'P';
.OR. hco_indest = 'C';
.OR. hco_indest = 'R');
.AND. hco_numfac <> hco_nrodoc
     campo = 'transform(hco_nrodoc, "@ 9999999999") + "³" + dtoc(hco_fecdoc) + "³" + iif(hco_indest="S","SOLICITADO",iif(hco_indest="P","PROCESO   ",iif(hco_indest="C","CERRADO   ","REPROCESO "))) + "³" + transform(hco_totgen,"999,999.99")'
ENDIF
titulo = 'Ayuda de documentos'
DO ayuda1 WITH campo, titulo,  ;
   'HCO_NRODOC'
SELECT gc_hco00
SET FILTER TO
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
PROCEDURE p_mensaje1
PARAMETER mens
l = LEN(mens)
DEFINE WINDOW mensj FROM 11, (80 -  ;
       l) / 2 - 4 TO 15, (80 + l) /  ;
       2 + 4 COLOR SCHEME 12
ACTIVATE WINDOW mensj
@ 01, (WCOLS('MENSJ') - l) / 2  ;
  SAY mens COLOR W/N* 
= INKEY(0.5 )
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

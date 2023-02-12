*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
STORE SPACE(10) TO w_campo
DEFINE WINDOW marco FROM 08, 26  ;
       TO 20, 51 DOUBLE
DEFINE WINDOW orden FROM 09, 32  ;
       TO 13, 45 NONE COLOR  ;
       SCHEME 20
DEFINE WINDOW detalle FROM 09, 00  ;
       TO 18, 76 IN screen COLOR  ;
       SCHEME 21
DEFINE WINDOW det1 FROM 12, 06 TO  ;
       20, 76 IN screen COLOR  ;
       SCHEME 10
DEFINE WINDOW pant1 FROM 05, 05  ;
       TO 08, 71 IN screen
DEFINE WINDOW ven2 FROM 18, 10 TO  ;
       20, 30 TITLE  ;
       'INGRESE CODIGO' IN screen  ;
       COLOR SCHEME 12
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 50 IN screen COLOR  ;
       SCHEME 8
tit_prg = 'CONSULTA'
wrk_progra = PROGRAM()
DO crea_win
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'CONSULTA POR N§ DE FACTURA'
ON KEY
SET CURSOR ON
SELECT 1
USE SHARED gc_pro00 ORDER codigo
SELECT 2
USE SHARED gc_hco00 ORDER codigo
w_selpro = SELECT()
SELECT 3
USE SHARED gc_dco00 ORDER codigo
SELECT 4
USE SHARED gc_ord00 ORDER  ;
    ord_docprp
SELECT 5
USE SHARED gc_nfa00 ORDER  ;
    nfa_numfac
SET RELATION TO nfa_nrodoc + nfa_codpro;
INTO gc_dco00
SELECT 6
USE SHARED ge_tab0 ORDER codigo
SELECT 7
USE SHARED gc_cli00 ORDER codigo
STORE .T. TO sigue
STORE 0 TO n, m
STORE 1 TO w_bar
DO WHILE sigue
     w_codigo = SPACE(10)
     ON KEY LABEL f6 do ordena
     DO esc_indica WITH 1, 'AYU',  ;
        'BUS', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     ACTIVATE WINDOW pant1
     CLEAR
     IF w_bar = 1 .OR. w_bar = 3
          @ 00, 01 SAY  ;
            'Nø FACTURA:' COLOR W/ ;
            N 
     ELSE
          @ 00, 01 SAY  ;
            'Nø PEDIDO :' COLOR W/ ;
            N 
     ENDIF
     @ 00, 13 GET w_codigo  ;
       FUNCTION '@!'
     READ
     IF LASTKEY() = 27
          IF w_bar = 3
               STORE SPACE(10) TO  ;
                     w_codigo,  ;
                     w_codpro
          ENDIF
          DEACTIVATE WINDOW pant1
          sigue = .F.
     ELSE
          SELECT gc_hco00
          IF w_bar = 1 .OR. w_bar =  ;
             3
               w_codigo = f_ceros(w_codigo, ;
                          10,2)
          ENDIF
          IF w_bar = 3
               SET ORDER TO hco_numfac
               SEEK w_codigo
          ELSE
               SET ORDER TO codigo
               SEEK w_codigo
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'Documento no Existe '
               LOOP
          ENDIF
          DO CASE
               CASE LASTKEY() = 1
                    GOTO TOP
               CASE LASTKEY() = 6
                    GOTO BOTTOM
               CASE LASTKEY() = 3
                    IF EOF()
                         GOTO BOTTOM
                    ELSE
                         SKIP 1
                    ENDIF
               CASE LASTKEY() =  ;
                    18
                    IF BOF()
                         GOTO TOP
                    ELSE
                         SKIP -1
                    ENDIF
          ENDCASE
          IF w_bar = 1 .OR. w_bar =  ;
             3
               @ 00, 01 SAY  ;
                 'Nø FACTURA:'  ;
                 COLOR W/N 
               w_codigo = hco_numfac
          ELSE
               @ 00, 01 SAY  ;
                 'Nø PEDIDO :'  ;
                 COLOR W/N 
               w_codigo = hco_nrodoc
          ENDIF
          @ 00, 13 SAY w_codigo  ;
            COLOR N/W 
          DO llena WITH w_bar
          IF n = 0
               DO error WITH  ;
                  'No hay Registros para Consultar'
          ELSE
               DO muestra
          ENDIF
     ENDIF
ENDDO
RELEASE WINDOW cabecera, detalle,  ;
        det1, ven2, pide
DEACTIVATE WINDOW tablas
DEACTIVATE WINDOW pant1
DO mensa WITH '   ', 'SACA'
DO sacawin
CLEAR READ
CLEAR GETS
ON KEY
CLOSE DATABASES
ACTIVATE SCREEN
RETURN
*
PROCEDURE llena
PARAMETER w_bar
CREATE CURSOR facped (numfac C  ;
       (10), nrodoc C (10),  ;
       fecdoc D, feccon D, codent  ;
       C (20), indest C (1),  ;
       estado C (10), feclle D,  ;
       codprp C (14), codpro C  ;
       (14), descri C (40),  ;
       canped N (9, 2), cancon N  ;
       (9, 2), fecll2 D (8),  ;
       candes N (9, 2), canbor N  ;
       (9, 2), cancel N (9, 2),  ;
       pfob N (9, 2), inorig C  ;
       (1), desinor C (10),  ;
       docref C (10))
STORE 0 TO n, m
DO CASE
     CASE w_bar = 1 .OR. w_bar =  ;
          3
          SELECT gc_nfa00
          SEEK w_codigo
          SCAN WHILE nfa_numfac =  ;
               w_codigo .AND.   ;
               .NOT. EOF()
               n = n + 1
               SELECT facped
               APPEND BLANK
               REPLACE numfac  ;
                       WITH  ;
                       gc_hco00.hco_numfac,  ;
                       nrodoc  ;
                       WITH  ;
                       gc_nfa00.nfa_nrodoc,  ;
                       fecdoc  ;
                       WITH  ;
                       gc_hco00.hco_fecdoc,  ;
                       codprp  ;
                       WITH  ;
                       gc_nfa00.nfa_codprp,  ;
                       codpro  ;
                       WITH  ;
                       gc_nfa00.nfa_codpro,  ;
                       fecll2  ;
                       WITH  ;
                       gc_dco00.dco_feclle
               REPLACE canped  ;
                       WITH  ;
                       gc_dco00.dco_cansol,  ;
                       cancon  ;
                       WITH  ;
                       gc_nfa00.nfa_cancon,  ;
                       pfob WITH  ;
                       gc_dco00.dco_conpre,  ;
                       feclle  ;
                       WITH  ;
                       gc_hco00.hco_feclle,  ;
                       feccon  ;
                       WITH  ;
                       gc_hco00.hco_feccon
               REPLACE candes  ;
                       WITH  ;
                       gc_dco00.dco_candes,  ;
                       canbor  ;
                       WITH  ;
                       gc_dco00.dco_canbor,  ;
                       cancel  ;
                       WITH  ;
                       gc_dco00.dco_cancel,  ;
                       inorig  ;
                       WITH  ;
                       gc_dco00.dco_inorig,  ;
                       docref  ;
                       WITH  ;
                       gc_dco00.dco_docref
               SELECT gc_cli00
               SEEK 'P' +  ;
                    gc_hco00.hco_codent
               IF FOUND()
                    SELECT facped
                    REPLACE codent  ;
                            WITH  ;
                            SUBSTR(gc_cli00.cli_razsoc,  ;
                            1,  ;
                            20)
               ENDIF
               SELECT gc_pro00
               IF EMPTY(facped.codpro)
                    SEEK facped.codprp
                    IF FOUND()
                         SELECT facped
                         REPLACE descri  ;
                                 WITH  ;
                                 gc_pro00.pro_descri
                    ENDIF
               ELSE
                    SEEK facped.codpro
                    IF FOUND()
                         SELECT facped
                         REPLACE descri  ;
                                 WITH  ;
                                 gc_pro00.pro_descri
                    ENDIF
               ENDIF
               SELECT ge_tab0
               SEEK 'IMPO' +  ;
                    gc_nfa00.nfa_indest
               IF FOUND()
                    SELECT facped
                    REPLACE estado  ;
                            WITH  ;
                            SUBSTR(ge_tab0.tab_destab,  ;
                            1,  ;
                            10)
               ENDIF
               SELECT ge_tab0
               SEEK 'OPED' +  ;
                    gc_dco00.dco_inorig
               IF FOUND()
                    SELECT facped
                    REPLACE desinor  ;
                            WITH  ;
                            SUBSTR(ge_tab0.tab_destab,  ;
                            1,  ;
                            10)
               ENDIF
               SELECT gc_nfa00
          ENDSCAN
     CASE w_bar = 2
          SELECT gc_dco00
          SET ORDER TO codigp
          SEEK w_codigo
          SCAN WHILE dco_nrodoc =  ;
               w_codigo .AND.   ;
               .NOT. EOF()
               n = n + 1
               SELECT facped
               APPEND BLANK
               REPLACE numfac  ;
                       WITH  ;
                       gc_dco00.dco_numfac,  ;
                       nrodoc  ;
                       WITH  ;
                       gc_dco00.dco_nrodoc,  ;
                       fecdoc  ;
                       WITH  ;
                       gc_hco00.hco_fecdoc,  ;
                       feccon  ;
                       WITH  ;
                       gc_hco00.hco_feccon,  ;
                       fecll2  ;
                       WITH  ;
                       gc_dco00.dco_feclle
               REPLACE codprp  ;
                       WITH  ;
                       gc_dco00.dco_codprp,  ;
                       codpro  ;
                       WITH  ;
                       gc_dco00.dco_codpro,  ;
                       canped  ;
                       WITH  ;
                       gc_dco00.dco_cansol,  ;
                       cancon  ;
                       WITH  ;
                       gc_dco00.dco_cancon,  ;
                       pfob WITH  ;
                       gc_dco00.dco_conpre
               REPLACE indest  ;
                       WITH  ;
                       gc_dco00.dco_indest,  ;
                       feclle  ;
                       WITH  ;
                       gc_hco00.hco_feclle,  ;
                       candes  ;
                       WITH  ;
                       gc_dco00.dco_candes,  ;
                       canbor  ;
                       WITH  ;
                       gc_dco00.dco_canbor,  ;
                       cancel  ;
                       WITH  ;
                       gc_dco00.dco_cancel
               REPLACE inorig  ;
                       WITH  ;
                       gc_dco00.dco_inorig,  ;
                       docref  ;
                       WITH  ;
                       gc_dco00.dco_docref
               SELECT gc_cli00
               SEEK 'P' +  ;
                    gc_hco00.hco_codent
               IF FOUND()
                    SELECT facped
                    REPLACE codent  ;
                            WITH  ;
                            SUBSTR(gc_cli00.cli_razsoc,  ;
                            1,  ;
                            20)
               ENDIF
               SELECT gc_pro00
               IF EMPTY(facped.codpro)
                    SEEK facped.codprp
                    IF FOUND()
                         SELECT facped
                         REPLACE descri  ;
                                 WITH  ;
                                 gc_pro00.pro_descri
                    ENDIF
               ELSE
                    SEEK facped.codpro
                    IF FOUND()
                         SELECT facped
                         REPLACE descri  ;
                                 WITH  ;
                                 gc_pro00.pro_descri
                    ENDIF
               ENDIF
               SELECT ge_tab0
               SEEK 'IMPO' +  ;
                    gc_dco00.dco_indest
               IF FOUND()
                    SELECT facped
                    REPLACE estado  ;
                            WITH  ;
                            SUBSTR(ge_tab0.tab_destab,  ;
                            1,  ;
                            10)
               ENDIF
               SELECT ge_tab0
               SEEK 'OPED' +  ;
                    gc_dco00.dco_inorig
               IF FOUND()
                    SELECT facped
                    REPLACE desinor  ;
                            WITH  ;
                            SUBSTR(ge_tab0.tab_destab,  ;
                            1,  ;
                            10)
               ENDIF
               SELECT gc_dco00
          ENDSCAN
          SET ORDER TO codigo
ENDCASE
RETURN
*
PROCEDURE muestra
DO esc_indica WITH 1, 'AYU',  ;
   'INI', 'ARR', 'INF'
DO esc_indica WITH 2, 'BUS',  ;
   'FIN', 'ABA', 'ESC'
ACTIVATE WINDOW pant1, detalle
ON KEY LABEL home do inicio
ON KEY LABEL end do final
ON KEY LABEL f6 do buscar
ON KEY LABEL f8 do verdoc
ON KEY LABEL ctrl+w ??
SELECT facped
GOTO TOP
IF w_bar = 1 .OR. w_bar = 3
     ACTIVATE WINDOW pant1
     @ 01, 01 SAY 'Proveedor : ' +  ;
       facped.codent COLOR W/N 
     @ 00, 40 SAY 'Fecha Doc.: ' +  ;
       DTOC(facped.fecdoc) COLOR  ;
       W/N 
     @ 01, 40 SAY 'Fecha Lle.: ' +  ;
       DTOC(facped.feclle) COLOR  ;
       W/N 
     ACTIVATE WINDOW detalle
     BROWSE FIELDS nrodoc : 10 :H =  ;
            'N§ Pedido', codprp :  ;
            14 :H = 'C¢digo   ',  ;
            codpro : 14 :H =  ;
            'Reemplazo ', descri  ;
            : 20 :H =  ;
            'Descripci¢n ',  ;
            cancon :P = '999,999'  ;
            :H = 'C.Conf.',  ;
            desinor : 10 :H =  ;
            'Origen', docref :V =  ;
            verdoc(INKEY(0)) : 10  ;
            :H = 'Doc.Ref.',  ;
            fecll2 :H =  ;
            'Fec.LLe.', estado :H =  ;
            'Estado' : 10, canped  ;
            :P = '999,999' :H =  ;
            'C.Soli.', candes :P =  ;
            '999,999' :H =  ;
            'C.Desp.', canbor :P =  ;
            '999,999' :H =  ;
            'C.Back.', cancel :P =  ;
            '999,999' :H =  ;
            'C.Canc.' NOEDIT IN  ;
            detalle NOLGRID
ELSE
     ACTIVATE WINDOW pant1
     @ 01, 01 SAY 'Proveedor : ' +  ;
       facped.codent COLOR W/N 
     @ 00, 40 SAY 'Fecha Doc.: ' +  ;
       DTOC(facped.fecdoc) COLOR  ;
       W/N 
     @ 01, 40 SAY 'Fecha Lle.: ' +  ;
       DTOC(facped.feclle) COLOR  ;
       W/N 
     ACTIVATE WINDOW detalle
     BROWSE FIELDS numfac : 10 :H =  ;
            'N§ Factura ', codprp  ;
            : 14 :H = 'C¢digo   ',  ;
            codpro : 14 :H =  ;
            'Reemplazo ', descri  ;
            : 20 :H =  ;
            'Descripci¢n ',  ;
            cancon :P = '999,999'  ;
            :H = 'C.Conf.',  ;
            desinor : 10 :H =  ;
            'Origen', docref :V =  ;
            verdoc(INKEY(0)) : 10  ;
            :H = 'Doc.Ref.',  ;
            estado :H = 'Estado'  ;
            : 10, candes :P =  ;
            '999,999' :H =  ;
            'C.Desp.', fecll2 : 8  ;
            :H = 'Fec.Lle.',  ;
            canped :P = '999,999'  ;
            :H = 'C.Soli.',  ;
            canbor :P = '999,999'  ;
            :H = 'C.Back.',  ;
            cancel :P = '999,999'  ;
            :H = 'C.Canc.' NOEDIT  ;
            IN detalle NOLGRID
ENDIF
IF LASTKEY() = 27
     DEACTIVATE WINDOW detalle
ENDIF
ON KEY LABEL home
ON KEY LABEL end
ON KEY LABEL f6
ON KEY LABEL f8
ON KEY LABEL ctrl+w
RETURN
*
PROCEDURE inicio
GOTO TOP
RETURN
*
PROCEDURE final
GOTO BOTTOM
RETURN
*
PROCEDURE verdoc
CREATE CURSOR refe (estado C (10),  ;
       docu C (10), numfac C (10),  ;
       feclle D (8), desinor C  ;
       (10), codpro C (14))
SELECT gc_ord00
SEEK facped.nrodoc +  ;
     facped.codprp
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** No Hay Documento de Referencia ***'
     RETURN
ELSE
     SCAN WHILE ord_nrodoc =  ;
          facped.nrodoc .AND.  ;
          ord_codprp =  ;
          facped.codprp .AND.   ;
          .NOT. EOF()
          SELECT refe
          APPEND BLANK
          REPLACE docu WITH  ;
                  gc_ord00.ord_docref,  ;
                  numfac WITH  ;
                  gc_ord00.ord_numfac,  ;
                  feclle WITH  ;
                  gc_ord00.ord_feclle,  ;
                  codpro WITH  ;
                  gc_ord00.ord_codpro
          SELECT ge_tab0
          SEEK 'IMPO' +  ;
               gc_ord00.ord_indest
          IF FOUND()
               SELECT refe
               REPLACE estado  ;
                       WITH  ;
                       SUBSTR(ge_tab0.tab_destab,  ;
                       1, 10)
          ENDIF
          SELECT ge_tab0
          SEEK 'OPED' +  ;
               gc_ord00.ord_inorig
          IF FOUND()
               SELECT refe
               REPLACE desinor  ;
                       WITH  ;
                       SUBSTR(ge_tab0.tab_destab,  ;
                       1, 10)
          ENDIF
          SELECT gc_ord00
     ENDSCAN
     SELECT refe
     GOTO TOP
     ACTIVATE WINDOW det1
     BROWSE FIELDS numfac :H =  ;
            'Nø Factura', feclle  ;
            :P = '99/99/99' :H =  ;
            'Fec.LLe.', desinor :  ;
            10 :H = 'Origen ',  ;
            docu :H =  ;
            'Doc. Ref. ', estado  ;
            : 10 :H =  ;
            'Estado    ', codpro  ;
            : 14 :H = 'Reemplazo'  ;
            NOEDIT IN det1  ;
            NOLGRID
     IF LASTKEY() = 27
          DEACTIVATE WINDOW det1
     ENDIF
ENDIF
SELECT facped
RETURN
*
PROCEDURE ordena
ON KEY
ACTIVATE SCREEN
DEFINE WINDOW produ FROM 12, 18  ;
       TO 23, 73 IN screen COLOR  ;
       SCHEME 8
DEFINE POPUP prod FROM 16, 31  ;
       COLOR SCHEME 8
DEFINE BAR 1 OF prod PROMPT  ;
       '\<FACTURA '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Nø PEDIDO '
DEFINE BAR 3 OF prod PROMPT  ;
       'F\<ECHA '
ON SELECTION POPUP prod do buscli with;
bar(),w_selpro,w_campo
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
IF LASTKEY() <> 27
     w_campo = gc_hco00.hco_numfac
ENDIF
SELECT gc_hco00
RETURN
*
PROCEDURE buscar
STORE SPACE(14) TO codi
ACTIVATE WINDOW ven2
@ 00, 00 GET codi PICTURE '@!'  ;
  COLOR SCHEME 10
READ
IF LASTKEY() = 27
     DEACTIVATE WINDOW ven2
     RETURN
ENDIF
SELECT facped
LOCATE FOR codprp = ALLTRIM(codi)
DEACTIVATE WINDOW ven2
RETURN
*
PROCEDURE buscli
PARAMETER bar, w_selpro,  ;
          wrk_marca
ON KEY
ACTIVATE WINDOW pide
SELECT gc_hco00
w_bar = BAR()
IF BAR() = 1
     STORE 0 TO w_codpro
     SET ORDER TO hco_numfac
     @ 00, 00 SAY 'FACTURA :'
     @ 00, 09 GET w_codpro VALID  ;
       vali_doc(w_codpro)
ENDIF
IF BAR() = 2
     STORE SPACE(10) TO w_codpro
     SET ORDER TO codigo
     @ 00, 00 SAY 'NøPEDIDO:'
     @ 00, 09 GET w_codpro  ;
       PICTURE '@!'
ENDIF
IF BAR() = 3
     STORE {} TO w_codpro
     SET ORDER TO hco_fecdoc
     @ 00, 00 SAY 'FEC.DOC.:'
     @ 00, 09 GET w_codpro
ENDIF
READ
IF LASTKEY() <> 27
     IF BAR() = 1
          w_codpro = f_ceros(w_codpro, ;
                     10,1)
          w_codigo = w_codpro
     ELSE
          w_codigo = w_codpro
     ENDIF
     SELECT gc_hco00
     SET NEAR ON
     SET FILTER TO
     IF BAR() = 3
          SET ORDER TO hco_fecdoc
          SEEK DTOS(w_codigo)
     ELSE
          SET ORDER TO codigo
          SEEK w_codigo
     ENDIF
     SET NEAR OFF
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do capcod
     IF BAR() = 1 .OR. BAR() = 3
          SET FILTER TO hco_numfac = hco_nrodoc;
.AND. ;
.NOT. EMPTY(gc_hco00.hco_numfac)
          BROWSE REST FIELDS  ;
                 hco_numfac :R :H =  ;
                 'Nø FACTURA',  ;
                 gc_hco00.hco_fecdoc  ;
                 :H = 'FEC.DOC.',  ;
                 gc_hco00.hco_feclle,  ;
                 gc_hco00.hco_indest  ;
                 :H = 'ESTADO' IN  ;
                 produ
     ELSE
          SET FILTER TO hco_numfac <>;
hco_nrodoc
          BROWSE REST FIELDS  ;
                 hco_nrodoc :R :H =  ;
                 'Nø PEDIDO ',  ;
                 gc_hco00.hco_numfac  ;
                 : 10 :H =  ;
                 'Nø FACTURA',  ;
                 gc_hco00.hco_fecdoc  ;
                 :H = 'Fec.Doc.',  ;
                 gc_hco00.hco_feclle,  ;
                 gc_hco00.hco_indest  ;
                 :H = 'Estado'  ;
                 FREEZE  ;
                 hco_nrodoc IN  ;
                 produ
     ENDIF
     IF LASTKEY() = 27
          ACTIVATE WINDOW pant1
          CLEAR
          IF w_bar = 1 .OR. w_bar =  ;
             3
               @ 00, 01 SAY  ;
                 'Nø FACTURA:'  ;
                 COLOR W/N 
          ELSE
               @ 00, 01 SAY  ;
                 'Nø PEDIDO :'  ;
                 COLOR W/N 
          ENDIF
          IF BAR() = 3
               SET ORDER TO hco_numfac
               STORE SPACE(10) TO  ;
                     w_codpro,  ;
                     w_codigo
          ENDIF
     ENDIF
ENDIF
ON KEY
DEACTIVATE WINDOW pide, muestr,  ;
           produ
RETURN
*
PROCEDURE capcod
ON KEY
IF BAR() = 1 .OR. BAR() = 3
     w_campo = gc_hco00.hco_numfac
     w_codigo = gc_hco00.hco_numfac
ELSE
     w_codigo = gc_hco00.hco_nrodoc
ENDIF
DEACTIVATE WINDOW pide, produ
DEACTIVATE POPUP prod
RETURN
*
FUNCTION vali_doc
PARAMETER w_num
w_num = f_ceros(w_num,10,1)
IF w_num = '0000000000'
     DO error WITH  ;
        '** Debe Ingresar N£mero **'
     KEYBOARD '{CTRL+Y}'
     RETURN .F.
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

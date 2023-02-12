*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
ind_prg = '<PORM0263>'
tit_prg = 'PROCESOS'
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ANULACION DE DOC. DE VENTA '
CLOSE DATABASES
SELECT 1
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 2
USE SHARED GC_HVE00 ORDER codigo
SELECT 3
USE SHARED GC_DVE00 ORDER codigo
SELECT 4
USE SHARED ST_IOREP ORDER codigo
SELECT 5
USE SHARED ST_ISREP ORDER codigo
SELECT 6
USE SHARED ST_ICLPR ORDER CODIGO
SELECT 7
USE SHARED GC_CMV00
SELECT 8
USE SHARED ST_MVORD ORDER ORDIA
STORE 0 TO w_facigv, wk_igv
SELECT ge_tab0
wrk_varbus = '"IGV " + "IGV "'
SEEK &wrk_varbus
IF FOUND()
     wk_igv = tab_factor
     w_facigv = tab_factor / 100
ELSE
     DO error WITH  ;
        '**No Definido el IGV**'
     RETURN
ENDIF
wrk_numord = SPACE(8)
valor = .T.
DO WHILE valor
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     STORE SPACE(1) TO wrk_tipdoc
     STORE SPACE(10) TO  ;
           wrk_numdoc,  ;
           wrk_nrdore
     STORE SPACE(4) TO wrk_coddoc,  ;
           wrk_tidore
     @ 04, 10 CLEAR TO 14, 67
     @ 04, 08 TO 14, 70
     @ 05, 10 SAY  ;
       'Tipo de Doc... :'
     @ 05, 40 SAY  ;
       'Nro. de Doc... :'
     @ 07, 10 SAY  ;
       'Cliente ...... :'
     @ 08, 10 SAY  ;
       'Fecha ........ :'
     @ 08, 40 SAY  ;
       'Nro.Doc.Ref... :'
     @ 09, 40 SAY  ;
       'Moneda         :'
     @ 10, 10 SAY  ;
       'Repuestos .... :'
     @ 11, 10 SAY  ;
       'Mano de Obra.. :'
     @ 12, 10 SAY  ;
       'Flete ........ :'
     @ 13, 10 SAY  ;
       'Sub Total .... :'
     @ 11, 40 SAY  ;
       'Descuentos ... :'
     @ 12, 40 SAY  ;
       'A Cuenta ..... :'
     @ 13, 40 SAY  ;
       'Total Pagado . :'
     SET CURSOR ON
     @ 05, 28 GET wrk_coddoc  ;
       PICTURE '@!' VALID  ;
       despues(1) WHEN antes(1)
     @ 05, 58 GET wrk_numdoc  ;
       PICTURE '@!' VALID  ;
       despues(2) WHEN antes(2)
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'ANU', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     ON KEY LABEL F6 DO AYUDA
     DO WHILE .T.
          = INKEY(0, 'H')
          IF LASTKEY() = 27
               EXIT
          ENDIF
          IF LASTKEY() = -1
               DEFINE WINDOW  ;
                      error FROM  ;
                      17, 10 TO  ;
                      19, 66
               ACTIVATE WINDOW  ;
                        error
               CLEAR
               wk_sinoes = 'N'
               @ 0, 15 SAY  ;
                 'Confirma Anular ?  (S/N)'  ;
                 GET wk_sinoes  ;
                 PICTURE '!'  ;
                 VALID wk_sinoes $  ;
                 'SN'
               READ
               IF LASTKEY() == 27
                    DEACTIVATE WINDOW  ;
                               error
                    wk_sinoes = 'N'
                    EXIT
               ENDIF
               DEACTIVATE WINDOW  ;
                          error
               IF wk_sinoes = 'S'  ;
                  .OR. wk_sinoes =  ;
                  's'
                    DO anula
                    EXIT
               ENDIF
          ENDIF
     ENDDO
ENDDO
ON KEY LABEL F6
CLOSE DATABASES
DO saca_win
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA
ENDCASE
RETURN
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               ON KEY LABEL F6 DO AYUDA
               RETURN .F.
          ENDIF
          IF EMPTY(wrk_coddoc)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               ON KEY LABEL F6 DO AYUDA
               RETURN .F.
          ENDIF
          IF wrk_coddoc <> 'FACT'  ;
             .AND. wrk_coddoc <>  ;
             'BOLE' .AND.  ;
             wrk_coddoc <>  ;
             'LIQU'
               DO error WITH  ;
                  '*** Tipo de Documento No Existe ***'
               ON KEY LABEL F6 DO AYUDA
               RETURN .F.
          ENDIF
     CASE opc = 2
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_numdoc)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               ON KEY LABEL F6 DO AYUDA
               RETURN .F.
          ENDIF
          wrk_numdoc = f_ceros(wrk_numdoc, ;
                       10,2)
          @ 05, 58 SAY wrk_numdoc
          SELECT gc_hve00
          SEEK wrk_coddoc +  ;
               wrk_numdoc
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'Nro. de Documento No Existe'
               ON KEY LABEL F6 DO AYUDA
               RETURN .F.
          ENDIF
          IF hve_estdoc = 'A'
               DO error WITH  ;
                  '*** Documento se Encuentra Anulado ***'
               RETURN .F.
          ENDIF
          IF hve_estdoc = 'C'  ;
             .OR. hve_estdoc =  ;
             'V'
               DO error WITH  ;
                  '***Documento no es de Orden de Reparaci¢n***'
               RETURN .F.
          ENDIF
          IF nivell <> 'A7'
               IF hve_fecdoc <>  ;
                  DATE()
                    DO error WITH  ;
                       '*** La Fecha del Documento es Diferente a la de Hoy ***'
                    RETURN .F.
               ENDIF
          ENDIF
          wrk_nrdore = hve_nrdore
          IF hve_tidore = 'SOLI'  ;
             .AND. hve_codmov =  ;
             'PCTA'
               SELECT st_isrep
               SEEK SUBSTR(wrk_nrdore,  ;
                    1, 8)
               IF FOUND()
                    IF indest =  ;
                       'F'
                         DO error  ;
                            WITH  ;
                            '**El Documento pertenece a una Solicitud Facturada**'
                         RETURN .F.
                    ENDIF
               ENDIF
          ENDIF
          SELECT gc_hve00
          w_codmon = hve_codmon
          @ 09, 58 SAY hve_codmon
          @ 08, 28 SAY hve_fecdoc
          @ 08, 58 SAY hve_numore
          IF w_codmon = 'DOL '
               @ 10, 28 SAY  ;
                 ROUND(hve_cosrep *  ;
                 (1 + w_facigv),  ;
                 2) PICTURE  ;
                 '999,999.99'
               @ 11, 28 SAY  ;
                 ROUND(hve_cosmob *  ;
                 (1 + w_facigv),  ;
                 2) PICTURE  ;
                 '999,999.99'
               @ 12, 28 SAY  ;
                 hve_flete  ;
                 PICTURE  ;
                 '999,999.99'
               @ 13, 28 SAY  ;
                 hve_totnet  ;
                 PICTURE  ;
                 '999,999.99'
               @ 11, 58 SAY  ;
                 hve_totdes  ;
                 PICTURE  ;
                 '999,999.99'
               @ 12, 58 SAY  ;
                 hve_pagctd  ;
                 PICTURE  ;
                 '999,999.99'
               @ 13, 58 SAY  ;
                 hve_totgen -  ;
                 hve_pagctd  ;
                 PICTURE  ;
                 '999,999.99'  ;
                 COLOR SCHEME 8
          ELSE
               @ 10, 28 SAY  ;
                 ROUND(hve_solrep *  ;
                 (1 + w_facigv),  ;
                 2) PICTURE  ;
                 '999,999.99'
               @ 11, 28 SAY  ;
                 ROUND(hve_solmob *  ;
                 (1 + w_facigv),  ;
                 2) PICTURE  ;
                 '999,999.99'
               @ 12, 28 SAY  ;
                 ROUND(hve_solfle *  ;
                 (1 + w_facigv),  ;
                 2) PICTURE  ;
                 '999,999.99'
               @ 13, 28 SAY  ;
                 hve_solnet  ;
                 PICTURE  ;
                 '999,999.99'
               @ 11, 58 SAY  ;
                 hve_soldes  ;
                 PICTURE  ;
                 '999,999.99'
               @ 12, 58 SAY  ;
                 hve_pagcts  ;
                 PICTURE  ;
                 '999,999.99'
               @ 13, 58 SAY  ;
                 hve_mtocan  ;
                 PICTURE  ;
                 '999,999.99'  ;
                 COLOR SCHEME 8
          ENDIF
          wrk_tidore = hve_tidore
          wrk_nrdore = hve_nrdore
          wrk_numord = hve_numore
          wrk_codcli = hve_codent
          IF wrk_tidore = 'ORDE'
               SELECT st_iorep
               SEEK SUBSTR(wrk_nrdore,  ;
                    1, 8)
               IF FOUND()
                    @ 08, 40 SAY  ;
                      'Nro. O/R ..... :'
                    @ 08, 58 SAY  ;
                      wrk_nrdore  ;
                      COLOR  ;
                      SCHEME 8
               ENDIF
          ELSE
               IF wrk_tidore =  ;
                  'SOLI'
                    SELECT st_isrep
                    wrk_codcli = codent
                    @ 8, 40 SAY  ;
                      'Nro. S/S ..... :'
                    @ 8, 58 SAY  ;
                      wrk_nrdore  ;
                      COLOR  ;
                      SCHEME 8
               ENDIF
          ENDIF
          SELECT st_iclpr
          SEEK 'C' + wrk_codcli
          IF  .NOT. FOUND()
               wrk_nomcli = SPACE(10)
          ELSE
               wrk_nomcli = noment
          ENDIF
          @ 07, 28 SAY  ;
            SUBSTR(wrk_nomcli, 1,  ;
            28) COLOR SCHEME 8
ENDCASE
*
PROCEDURE anula
DO mensa WITH  ;
   '*****  G R A B A N D O  *****',  ;
   'COLO'
STORE SPACE(4) TO wrk_auxest
STORE DATE() TO wrk_fecha
STORE SPACE(8) TO wrk_hora
SELECT gc_hve00
SEEK wrk_coddoc + wrk_numdoc
IF FOUND()
     DO rbloquea
     REPLACE hve_estdoc WITH 'A'
     REPLACE hve_usuari WITH  ;
             users
     REPLACE hve_fecha WITH  ;
             DATE()
     REPLACE hve_hora WITH TIME()
     UNLOCK
ENDIF
IF hve_indori = 'FGAR' .OR.  ;
   hve_indori = 'FREC'
     IF hve_tidore = 'ORDE'
          wrk_auxest = SPACE(4)
          SELECT st_iorep
          SEEK SUBSTR(wrk_nrdore,  ;
               1, 8)
          IF FOUND()
               SELECT st_mvord
               SEEK st_iorep.numdoc +  ;
                    st_iorep.auxest
               IF FOUND()
                    DELETE
                    IF SEEK(st_iorep.numdoc +  ;
                       '027 ')
                         wrk_auxest =  ;
                          estado
                    ELSE
                         IF SEEK(st_iorep.numdoc +  ;
                            '026 ' ;
                            )
                              wrk_auxest =  ;
                               estado
                         ELSE
                              IF SEEK(st_iorep.numdoc +  ;
                                 '021 ' ;
                                 )
                                   wrk_auxest = estado
                              ELSE
                                   IF SEEK(st_iorep.numdoc + '018 ')
                                        wrk_auxest = estado
                                   ELSE
                                        IF SEEK(st_iorep.numdoc + '010 ')
                                             wrk_auxest = estado
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
               SELECT st_iorep
               DO rbloquea
               REPLACE indest  ;
                       WITH 'C'
               REPLACE auxest  ;
                       WITH  ;
                       wrk_auxest
               REPLACE codfabo  ;
                       WITH  ;
                       SPACE(4)
               REPLACE numfabo  ;
                       WITH  ;
                       SPACE(10)
               REPLACE fecfabo  ;
                       WITH  ;
                       CTOD(SPACE(8))
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
               SELECT st_isrep
               SEEK st_iorep.numsol
               IF FOUND()
                    DO rbloquea
                    REPLACE indest  ;
                            WITH  ;
                            'C'
                    REPLACE user  ;
                            WITH  ;
                            users
                    REPLACE date  ;
                            WITH  ;
                            DATE()
                    REPLACE time  ;
                            WITH  ;
                            TIME()
                    UNLOCK
               ENDIF
          ENDIF
     ELSE
          SELECT st_isrep
          SEEK SUBSTR(wrk_nrdore,  ;
               1, 8)
          IF FOUND()
               DO rbloquea
               REPLACE monabo  ;
                       WITH  ;
                       monabo -  ;
                       gc_hve00.hve_totoim
               IF indest = 'F'  ;
                  .OR. indest =  ;
                  'B'
                    REPLACE indest  ;
                            WITH  ;
                            'P'
               ENDIF
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          SELECT gc_hve00
          IF hve_codcta = '004 '
               SELECT st_iorep
               SET ORDER TO ord_numsol
               SEEK SUBSTR(wrk_nrdore,  ;
                    1, 8)
               IF FOUND()
                    DO rbloquea
                    REPLACE auxest  ;
                            WITH  ;
                            '003'
                    REPLACE user  ;
                            WITH  ;
                            users,  ;
                            date  ;
                            WITH  ;
                            DATE()
                    REPLACE time  ;
                            WITH  ;
                            TIME()
                    UNLOCK
                    SELECT st_mvord
                    SEEK st_iorep.numdoc +  ;
                         '007 '
                    IF FOUND()
                         DO rbloquea
                         DELETE
                         UNLOCK
                    ENDIF
               ENDIF
               SELECT st_iorep
               SET ORDER TO codigo
          ENDIF
     ENDIF
ELSE
     CREATE CURSOR gara_c (numord  ;
            C (8), codfac C (4),  ;
            numfac C (10), fecfac  ;
            D)
     SELECT st_iorep
     SET ORDER TO ord_numfab
     SEEK gc_hve00.hve_tipdoc +  ;
          gc_hve00.hve_nrodoc
     SCAN WHILE (codfabo =  ;
          gc_hve00.hve_tipdoc  ;
          .AND. numfabo =  ;
          gc_hve00.hve_nrodoc)  ;
          .AND.  .NOT. EOF()
          SELECT gara_c
          APPEND BLANK
          REPLACE numord WITH  ;
                  st_iorep.numdoc
          REPLACE codfac WITH  ;
                  st_iorep.codfabo
          REPLACE numfac WITH  ;
                  st_iorep.numfabo
          REPLACE fecfac WITH  ;
                  st_iorep.fecfabo
          wrk_auxest = SPACE(4)
          SELECT st_mvord
          SEEK st_iorep.numdoc +  ;
               st_iorep.auxest
          IF FOUND()
               wrk_fecha = date
               wrk_hora = hora
               DELETE
               IF SEEK(st_iorep.numdoc +  ;
                  '080 ')
                    DELETE
                    IF SEEK(st_iorep.numdoc +  ;
                       '010 ')
                         APPEND BLANK
                         DO rbloquea
                         REPLACE orden  ;
                                 WITH  ;
                                 st_iorep.numdoc
                         REPLACE dia  ;
                                 WITH  ;
                                 wrk_fecha
                         REPLACE hora  ;
                                 WITH  ;
                                 wrk_hora
                         IF st_iorep.indori =  ;
                            'GARA'
                              REPLACE  ;
                               estado  ;
                               WITH  ;
                               '020 '
                         ELSE
                              IF st_iorep.indori =  ;
                                 'PVEN'
                                   REPLACE estado WITH '030 '
                              ELSE
                                   REPLACE estado WITH '022 '
                              ENDIF
                         ENDIF
                         REPLACE destado  ;
                                 WITH  ;
                                 SUBSTR(ootab('ESOR', ;
                                 estado),  ;
                                 1,  ;
                                 30)
                         REPLACE date  ;
                                 WITH  ;
                                 DATE()
                         REPLACE user  ;
                                 WITH  ;
                                 users
                         REPLACE time  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                         wrk_auxest =  ;
                          estado
                    ELSE
                         APPEND BLANK
                         DO rbloquea
                         REPLACE orden  ;
                                 WITH  ;
                                 st_iorep.numdoc
                         REPLACE dia  ;
                                 WITH  ;
                                 wrk_fecha
                         REPLACE hora  ;
                                 WITH  ;
                                 wrk_hora
                         REPLACE estado  ;
                                 WITH  ;
                                 '023 '
                         REPLACE destado  ;
                                 WITH  ;
                                 SUBSTR(ootab('ESOR', ;
                                 estado),  ;
                                 1,  ;
                                 30)
                         REPLACE date  ;
                                 WITH  ;
                                 DATE()
                         REPLACE user  ;
                                 WITH  ;
                                 users
                         REPLACE time  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                         wrk_auxest =  ;
                          estado
                    ENDIF
               ELSE
                    IF SEEK(st_iorep.numdoc +  ;
                       '030 ')
                         wrk_auxest =  ;
                          estado
                    ELSE
                         IF SEEK(st_iorep.numdoc +  ;
                            '023 ' ;
                            )
                              wrk_auxest =  ;
                               estado
                         ELSE
                              IF SEEK(st_iorep.numdoc +  ;
                                 '022 ' ;
                                 )
                                   wrk_auxest = estado
                              ELSE
                                   IF SEEK(st_iorep.numdoc + '020 ')
                                        wrk_auxest = estado
                                   ELSE
                                        IF SEEK(st_iorep.numdoc + '029 ')
                                             wrk_auxest = estado
                                        ELSE
                                             IF SEEK(st_iorep.numdoc + '028 ')
                                                  wrk_auxest = estado
                                             ELSE
                                                  IF SEEK(st_iorep.numdoc + '027 ')
                                                       wrk_auxest = estado
                                                  ELSE
                                                       IF SEEK(st_iorep.numdoc + '026 ')
                                                            wrk_auxest = estado
                                                       ELSE
                                                            IF SEEK(st_iorep.numdoc + '021 ')
                                                                 wrk_auxest = estado
                                                            ELSE
                                                                 IF SEEK(st_iorep.numdoc + '018 ')
                                                                      wrk_auxest = estado
                                                                 ELSE
                                                                      IF SEEK(st_iorep.numdoc + '010 ')
                                                                           wrk_auxest = estado
                                                                      ENDIF
                                                                 ENDIF
                                                            ENDIF
                                                       ENDIF
                                                  ENDIF
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_isrep
          SEEK st_iorep.numsol
          IF FOUND()
               DO rbloquea
               REPLACE indest  ;
                       WITH 'P'
               REPLACE user WITH  ;
                       users
               REPLACE date WITH  ;
                       DATE()
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          SELECT st_iorep
          DO rbloquea
          REPLACE indest WITH 'C',  ;
                  auxest WITH  ;
                  wrk_auxest
          REPLACE fecfabo WITH  ;
                  CTOD(SPACE(8))
          REPLACE user WITH users
          REPLACE date WITH  ;
                  DATE()
          REPLACE time WITH  ;
                  TIME()
          UNLOCK
     ENDSCAN
     SELECT st_iorep
     SET ORDER TO codigo
     SELECT gara_c
     GOTO TOP
     SCAN WHILE  .NOT. EOF()
          SELECT st_iorep
          SEEK gara_c.numord
          IF FOUND() .AND.  ;
             (codfabo =  ;
             gc_hve00.hve_tipdoc  ;
             .AND. numfabo =  ;
             gc_hve00.hve_nrodoc)
               DO rbloquea
               REPLACE codfabo  ;
                       WITH  ;
                       SPACE(4)
               REPLACE numfabo  ;
                       WITH  ;
                       SPACE(10)
               REPLACE fecfabo  ;
                       WITH  ;
                       CTOD(SPACE(8))
               UNLOCK
          ENDIF
     ENDSCAN
ENDIF
DO mensa WITH  ;
   '*****  G R A B A N D O  *****',  ;
   'SACA'
RETURN
*
PROCEDURE ayuda
ON KEY
SELECT gc_hve00
SET FILTER TO hve_tipdoc = wrk_coddoc;
.AND. hve_estdoc = 'O'
campoa = 'HVE_nROdoc+" "+dtoc(HVE_fecDOC)+" "+IIF(HVE_TIDORE="ORDE",SPACE(8)+HVE_NRDORE,HVE_NRDORE+SPACE(8))+" "+STR(HVE_SOLREP,9)+"     "+HVE_ESTDOC'
doc = wrk_coddoc
DO ayuda8 WITH campoa, doc
SET FILTER TO
ON KEY LABEL F6 do ayuda
RETURN
*
PROCEDURE ayuda8
PARAMETER campo1, doc
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'BBB', 'ESC'
DO choice
ACTIVATE WINDOW trabajo
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE choice
DEFINE WINDOW ayu3 FROM 0, 50 TO  ;
       2, 79 SHADOW
ACTIVATE WINDOW TOP ayu3
wk_numx = SPACE(10)
@ 0, 1 SAY 'Numero Desde :' GET  ;
  wk_numx PICTURE '9999999999'
FOR j = 1 TO 12
     ZOOM WINDOW ayu3 NORM FROM j,  ;
          50 TO 2 + j, 79
ENDFOR
FOR j = 1 TO 30
     ZOOM WINDOW ayu3 NORM FROM  ;
          12, 50 - j TO 14, 79 -  ;
          j
ENDFOR
READ
wk_numx = f_ceros(wk_numx,10,2)
@ 0, 16 SAY wk_numx COLOR N/W 
a = INKEY(0)
IF LASTKEY() == 27
     DEACTIVATE WINDOW ayu3
     RETURN
ENDIF
wk_inix = doc + wk_numx
DEFINE WINDOW ayu4 FROM 6, 6 TO 7,  ;
       70 SHADOW
ACTIVATE WINDOW TOP ayu4
ventana = SUBSTR(DBF(),  ;
          LEN(DBF()) - 11, 8)
FOR j = 1 TO 08
     ZOOM WINDOW ayu4 NORM FROM 6,  ;
          6 TO 7 + j, 70
ENDFOR
SET NEAR ON
SEEK wk_inix
ON KEY LABEL ENTER do choice2
browse field cer = " " :H="", uno = &campo1;
:H="  Numero   Fch Emis.    S/S     O/R     Monto(S/.) Estado";
in window ayu4   freeze cer REST 
DEACTIVATE WINDOW ayu3
ON KEY LABEL ENTER
DEACTIVATE WINDOW ayu4
SET NEAR OFF
RETURN
*
PROCEDURE choice2
IF LASTKEY() == 13
     wk_aux = ALLTRIM(hve_nrodoc)
     IF LEN(wk_aux) < 8
          wk_aux = wk_aux +  ;
                   CHR(13)
     ENDIF
     KEYBOARD CHR(27) + wk_aux
ENDIF
RETURN
*
PROCEDURE fech
SELECT gc_cmv00
SEEK DTOC(wk_fpari) + '1' +  ;
     'SOL ' + 'DOL '
IF  .NOT. FOUND()
     DO error WITH  ;
        '**No Existe Tipo de Cambio para esta Fecha**'
     error = .T.
ELSE
     wk_valpari = cmv_tipcav
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

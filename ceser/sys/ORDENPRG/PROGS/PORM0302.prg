*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
tit_prg = ' PROCESOS '
@ 2, 1 SAY DATE() COLOR SCHEME 8
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' REGISTRO DE CAMBIOS Y DEVOLUCIONES '
CLOSE DATABASES
SELECT 1
USE ge_tab0 ORDER codigo
SELECT 2
USE st_isrep ORDER codigo
SELECT 3
USE st_iclpr ORDER codigo
SELECT 4
USE st_iorep ORDER ord_numsol
SELECT 5
USE ST_CAMBI ORDER CODIGO
SELECT 6
USE ST_IMODE ORDER CODIGO
valor = .T.
DO WHILE valor
     STORE SPACE(4) TO w_tipdoc,  ;
           w_tienda, w_motivo,  ;
           w_tipref, w_docsal
     STORE SPACE(8) TO w_numero
     STORE SPACE(23) TO w_observ
     STORE SPACE(10) TO w_nroaut,  ;
           w_codart, w_nroref,  ;
           w_nrosal
     STORE DATE() TO w_fecdoc
     STORE {} TO w_fecsal
     STORE 0 TO w_numsol,  ;
           w_nrodoc
     w_indica = 0
     @ 03, 00 CLEAR TO 15, 74
     @ 03, 00 TO 03, 74
     @ 04, 01 SAY 'Tipo Doc.:'  ;
       COLOR W+/N 
     @ 04, 39 SAY 'Nro. Doc.:'  ;
       COLOR W+/N 
     @ 05, 01 SAY 'Fecha Doc:'  ;
       COLOR W+/N 
     @ 05, 39 SAY 'Tienda   :'  ;
       COLOR W+/N 
     @ 06, 01 SAY 'Motivo   :'  ;
       COLOR W+/N 
     @ 06, 39 SAY 'Nro. Aut.:'  ;
       COLOR W+/N 
     @ 07, 01 SAY 'Doc. Ref.:'  ;
       COLOR W+/N 
     @ 07, 39 SAY 'Nro. Ref.:'  ;
       COLOR W+/N 
     @ 08, 01 SAY 'S/Serv.  :'  ;
       COLOR W+/N 
     @ 08, 39 SAY 'Cod. Art.:'  ;
       COLOR W+/N 
     @ 09, 01 SAY 'Doc. Sal.:'  ;
       COLOR W+/N 
     @ 09, 39 SAY 'Nro. Sal.:'  ;
       COLOR W+/N 
     @ 10, 01 SAY 'Fecha Sal:'  ;
       COLOR W+/N 
     @ 10, 39 SAY 'Observ.  :'  ;
       COLOR W+/N 
     @ 11, 00 TO 11, 74
     @ 12, 01 SAY 'Cliente   :'
     @ 13, 01 SAY 'Art¡culo  :'
     @ 13, 41 SAY 'Modelo :'
     @ 14, 01 SAY 'Marca     :'
     @ 14, 41 SAY 'Serie  :'
     SET CURSOR ON
     ON KEY LABEL F6 do ayuda12
     @ 04, 12 GET w_tipdoc  ;
       PICTURE '@!' VALID  ;
       despues(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 04, 50 GET w_nrodoc  ;
       PICTURE '9999999999' VALID  ;
       despues(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 05, 12 GET w_fecdoc  ;
       PICTURE '@D' WHEN  ;
       antes(VARREAD())
     @ 05, 50 GET w_tienda  ;
       PICTURE '@!' VALID  ;
       despues(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 06, 12 GET w_motivo  ;
       PICTURE '@!' VALID  ;
       despues(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 06, 50 GET w_nroaut  ;
       PICTURE '@!' WHEN  ;
       antes(VARREAD())
     @ 07, 12 GET w_tipref  ;
       PICTURE '@!' VALID  ;
       despues(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 07, 50 GET w_nroref  ;
       PICTURE '9999999999' VALID  ;
       despues(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 08, 12 GET w_numsol  ;
       PICTURE '99999999' VALID  ;
       despues(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 08, 50 GET w_codart  ;
       PICTURE '@!' WHEN  ;
       antes(VARREAD())
     @ 09, 12 GET w_docsal  ;
       PICTURE '@!' VALID  ;
       despues(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 09, 50 GET w_nrosal  ;
       PICTURE '9999999999' VALID  ;
       despues(VARREAD()) WHEN  ;
       antes(VARREAD())
     @ 10, 12 GET w_fecsal  ;
       PICTURE '@D' WHEN  ;
       antes(VARREAD())
     @ 10, 50 GET w_observ  ;
       PICTURE '@!'
     READ
     DO esc_indica WITH 2, 'BBB',  ;
        'GRA', 'BBB', 'ESC'
     DO WHILE .T.
          = INKEY(0, 'H')
          IF LASTKEY() = 27
               STORE .F. TO valor
               EXIT
          ENDIF
          IF LASTKEY() = -1
               ON KEY
               DO graba
               EXIT
          ENDIF
     ENDDO
ENDDO
CLOSE DATABASES
ON KEY LABEL f6
DO sacawin
RETURN
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 'W_TIPDOC'
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 do ayuda12
     CASE opc = 'W_NRODOC'
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6
     CASE opc = 'W_FECDOC'
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6
     CASE opc = 'W_TIENDA'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 do ayuda12
     CASE opc = 'W_MOTIVO'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 do ayuda12
     CASE opc = 'W_NROAUT'
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6
     CASE opc = 'W_TIPREF'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 do ayuda12
     CASE opc = 'W_NROREF'
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6
     CASE opc = 'W_DOCSAL'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 do ayuda12
     CASE opc = 'W_NROSAL'
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6
     CASE opc = 'W_FECSAL'
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6
     CASE opc = 'W_NUMSOL'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 do ayuda12
     CASE opc = 'W_CODART'
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6
ENDCASE
RETURN
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 'W_TIPDOC'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(w_tipdoc)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' + w_tipdoc
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** Tipo de Documento No Existe ***'
               RETURN .F.
          ELSE
               IF w_tipdoc <>  ;
                  'NCRE' .AND.  ;
                  w_tipdoc <>  ;
                  'NCAM'
                    DO error WITH  ;
                       '*** No es Documento de N/Cr‚dito o N/Cambio ***'
                    RETURN .F.
               ENDIF
          ENDIF
          @ 04, 17 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            21)
     CASE opc = 'W_NRODOC'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(w_nrodoc)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT st_cambi
          SEEK w_tipdoc +  ;
               STR(w_nrodoc, 10)
          IF FOUND()
               w_indica = 1
               w_tienda = tienda
               w_motivo = motivo
               w_tipref = tipref
               w_nroref = docref
               w_numsol = VAL(numsol)
               w_codart = codart
               w_observ = observ
               @ 05, 12 SAY  ;
                 w_fecdoc
               @ 05, 50 SAY  ;
                 w_tienda
               @ 05, 55 SAY  ;
                 SUBSTR(ootabla('TDAS', ;
                 w_tienda,1), 1,  ;
                 21)
               @ 06, 12 SAY  ;
                 w_motivo
               @ 06, 17 SAY  ;
                 SUBSTR(ootabla('MCAM', ;
                 w_motivo,1), 1,  ;
                 21)
               @ 06, 50 SAY  ;
                 w_nroaut
               @ 07, 12 SAY  ;
                 w_tipref
               @ 07, 17 SAY  ;
                 SUBSTR(ootabla('DOCU', ;
                 w_tipref,1), 1,  ;
                 21)
               @ 07, 50 SAY  ;
                 w_nroref
               @ 08, 12 SAY  ;
                 w_numsol PICTURE  ;
                 '99999999'
               @ 08, 50 SAY  ;
                 w_codart
               @ 09, 12 SAY  ;
                 w_observ
               w_numero = STR(w_numsol,  ;
                          8)
               SELECT st_isrep
               SEEK w_numero
               IF FOUND()
                    IF indest <>  ;
                       'A   '
                         w_codent =  ;
                          codent
                         w_codcli =  ;
                          codent
                         w_indori =  ;
                          indori
                         w_marca =  ;
                          codmar
                         w_serie =  ;
                          numser
                         w_codmod =  ;
                          codmod
                         @ 13, 50  ;
                           SAY  ;
                           codmod
                         @ 14, 13  ;
                           SAY  ;
                           SUBSTR(ootab('MARC', ;
                           codmar),  ;
                           1,  ;
                           20)
                         @ 14, 50  ;
                           SAY  ;
                           numser
                         SELECT st_iclpr
                         SEEK 'C' +  ;
                              w_codent
                         IF FOUND()
                              w_noment =  ;
                               noment
                              w_nomcal =  ;
                               nomcal
                              w_numtel =  ;
                               numte1
                              w_coddis =  ;
                               nomdis
                              @ 11,  ;
                                13  ;
                                SAY  ;
                                w_noment
                              @ 12,  ;
                                13  ;
                                SAY  ;
                                ALLTRIM(w_nomcal) +  ;
                                ' - ' +  ;
                                ootab('DIST', ;
                                w_coddis)
                         ENDIF
                         SELECT st_imode
                         SEEK w_marca +  ;
                              w_codmod
                         IF FOUND()
                              @ 13,  ;
                                13  ;
                                SAY  ;
                                SUBSTR(nommod,  ;
                                1,  ;
                                25)
                              w_nommod =  ;
                               nommod
                         ELSE
                              w_nommod =  ;
                               ' '
                         ENDIF
                    ENDIF
               ENDIF
               KEYBOARD '{SPACEBAR}'
               DO WHILE .T.
                    = INKEY('H',  ;
                      0)
                    IF LASTKEY() =  ;
                       27
                         RETURN .F.
                    ENDIF
                    IF LASTKEY() =  ;
                       13
                         RETURN
                    ENDIF
               ENDDO
          ENDIF
     CASE opc = 'W_TIENDA'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(w_tienda)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'TDAS' + w_tienda
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tienda No Existe ***'
               RETURN .F.
          ENDIF
          @ 05, 55 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            19)
     CASE opc = 'W_MOTIVO'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(w_motivo)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'MCAM' + w_motivo
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Motivo No Existe ***'
               RETURN .F.
          ENDIF
          @ 06, 17 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            21)
     CASE opc = 'W_TIPREF'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(w_tipref)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' + w_tipref
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** Tipo de Documento No Existe ***'
               RETURN .F.
          ENDIF
          @ 07, 17 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            21)
     CASE opc = 'W_NROREF'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(w_nroref)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
     CASE opc = 'W_NUMSOL'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(w_numsol)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          w_numero = STR(w_numsol,  ;
                     8)
          SELECT st_isrep
          SET ORDER TO CODIGO
          SEEK w_numero
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** N£mero de Solicitud No Existe ***'
               RETURN .F.
          ENDIF
          IF indest = 'A   '
               DO error WITH  ;
                  '*** S/Servicio se Encuentra Anulada ***'
               RETURN .F.
          ENDIF
          w_codent = codent
          w_codcli = codent
          w_indori = indori
          w_marca = codmar
          w_serie = numser
          w_codmod = codmod
          @ 13, 50 SAY codmod
          @ 14, 13 SAY  ;
            SUBSTR(ootab('MARC', ;
            codmar), 1, 20)
          @ 14, 50 SAY numser
          SELECT st_iclpr
          SEEK 'C' + w_codent
          IF FOUND()
               w_noment = noment
               w_nomcal = nomcal
               w_numtel = numte1
               w_coddis = nomdis
               @ 12, 13 SAY  ;
                 w_noment
          ENDIF
          SELECT st_imode
          SEEK w_marca + w_codmod
          IF FOUND()
               @ 13, 13 SAY  ;
                 SUBSTR(nommod, 1,  ;
                 25)
               w_nommod = nommod
          ELSE
               w_nommod = ' '
          ENDIF
     CASE opc = 'W_DOCSAL'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF  .NOT.  ;
              EMPTY(w_docsal)
               SELECT ge_tab0
               SEEK 'DOCU' +  ;
                    w_docsal
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '*** Tipo de Documento No Existe ***'
                    RETURN .F.
               ENDIF
               @ 08, 17 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 21)
          ENDIF
     CASE opc = 'W_NROSAL'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
     CASE opc = 'W_OBSERV'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'GRA',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
RETURN
*
PROCEDURE ayuda12
ON KEY LABEL F6
DO CASE
     CASE VARREAD() = 'W_NUMSOL'
          SELECT st_isrep
          wrk_origen = 'SS'
          campoa = 'numdoc+"  "+dtoc(fecemi)+"  "+codent+"  "+codmod+"   "+numser+" "+subst(indest,1,2)+" "+indori'
          DO ayuda7 WITH campoa,  ;
             wrk_origen
          ON KEY LABEL F6 do ayuda12
     CASE VARREAD() = 'W_TIPDOC'  ;
          .OR. VARREAD() =  ;
          'W_DOCSAL'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'DOCU'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'TIPO DE DOCUMENTO'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
     CASE VARREAD() = 'W_TIPREF'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'DOCU'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'TIPO DE DOCUMENTO'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
     CASE VARREAD() = 'W_TIENDA'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'TDAS'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'TIPO DE DOCUMENTO'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
     CASE VARREAD() = 'W_MOTIVO'
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'MCAM'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'TIPO DE DOCUMENTO'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          SET FILTER TO
ENDCASE
RETURN
*
PROCEDURE graba
SELECT st_cambi
IF w_indica = 0
     APPEND BLANK
ENDIF
REPLACE tipdoc WITH w_tipdoc,  ;
        nrodoc WITH STR(w_nrodoc,  ;
        10), fecdoc WITH  ;
        w_fecdoc
REPLACE tienda WITH w_tienda,  ;
        motivo WITH w_motivo,  ;
        nroaut WITH w_nroaut
REPLACE tipref WITH w_tipref,  ;
        docref WITH w_nroref,  ;
        codart WITH w_codart
REPLACE numsol WITH w_numero,  ;
        observ WITH w_observ
REPLACE docsal WITH w_docsal,  ;
        nrosal WITH w_nrosal,  ;
        fecsal WITH w_fecsal
REPLACE user WITH users, time  ;
        WITH TIME(), date WITH  ;
        DATE()
w_indica = 0
*
*** 
*** ReFox - retrace your steps ... 
***

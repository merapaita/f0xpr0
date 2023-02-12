*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
tit_prg = 'MANTENCION'
@ 02, 01 SAY DATE() COLOR SCHEME  ;
  8
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INVENTARIO DE MERCADERIA'
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER  ;
    ord_esem
SELECT 2
USE SHARED ge_tab0 ORDER codigo
a = .T.
DO WHILE a
     @ 04, 01 TO 12, 72
     @ 05, 03 SAY  ;
       'Estado del Inventario.... :'
     @ 06, 03 SAY  ;
       'Grupo de Ingreso :'
     DO esc_modo WITH 'I'
     STORE SPACE(04) TO w_estado,  ;
           w_talini, w_talfin,  ;
           w_emiini, w_emifin,  ;
           w_garini, w_garfin
     SET CURSOR ON
     @ 05, 32 GET w_estado  ;
       PICTURE '@!' VALID  ;
       despues(1) WHEN antes(1)
     READ
     IF LASTKEY() = 27
          a = .F.
     ELSE
          g = .T.
          DO WHILE g
               STORE 1 TO w_opc2
               IF w_estado =  ;
                  'INTA' .OR.  ;
                  w_estado =  ;
                  'INVE'
                    @ 07, 03 SAY  ;
                      'Taller'
                    @ 07, 10 SAY  ;
                      'Del:' GET  ;
                      w_talini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(4)  ;
                      WHEN  ;
                      antes(4)
                    @ 08, 10 SAY  ;
                      ' Al:' GET  ;
                      w_talfin  ;
                      RANGE  ;
                      w_talini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(5)  ;
                      WHEN  ;
                      antes(4)
               ENDIF
               IF w_estado =  ;
                  'INRE' .OR.  ;
                  w_estado =  ;
                  'INVE'
                    @ 07, 38 SAY  ;
                      'Emisor'
                    @ 07, 45 SAY  ;
                      'Del:' GET  ;
                      w_emiini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(2)  ;
                      WHEN  ;
                      antes(2)
                    @ 08, 45 SAY  ;
                      ' Al:' GET  ;
                      w_emifin  ;
                      RANGE  ;
                      w_emiini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(3)  ;
                      WHEN  ;
                      antes(2)
                    @ 09, 38 SAY  ;
                      'Atenc.'
                    @ 09, 45 SAY  ;
                      'Del:' GET  ;
                      w_garini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(6)  ;
                      WHEN  ;
                      antes(3)
                    @ 10, 45 SAY  ;
                      ' Al:' GET  ;
                      w_garfin  ;
                      RANGE  ;
                      w_garini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(7)  ;
                      WHEN  ;
                      antes(3)
               ENDIF
               @ 10, 03 SAY  ;
                 'Procesa?:' GET  ;
                 w_opc2 PICTURE  ;
                 '@*RVT No;Si'  ;
                 WHEN antes(5)
               READ
               IF LASTKEY() = 27
                    @ 07, 03  ;
                      CLEAR TO 11,  ;
                      70
                    g = .F.
               ELSE
                    IF w_opc2 = 2
                         w_arch =  ;
                          f_archivo()
                         create table;
&w_arch (numdoc c(8),numsol c(8),fecemi;
d(8),indori c(4),codmar c(4),codmod c(15),numser;
c(20),codtec c(9),auxest c(4),fecest d(8),codtall;
c(4),codemi c(4),pass c(5),codent c(11),conteo;
c(1),inve c(4),repo c(1),user c(8),fecha;
d(8),hora c(8),fecinv d(8),horinv c(8))
                         DO mensa  ;
                            WITH  ;
                            '** Un momento por Favor..... **',  ;
                            'COLO'
                         CREATE CURSOR  ;
                                esta  ;
                                (estado  ;
                                C  ;
                                (4))
                         SELECT ge_tab0
                         IF w_estado =  ;
                            'INTA'  ;
                            .OR.  ;
                            w_estado =  ;
                            'INVE'
                              SET NEAR;
ON
                              SEEK  ;
                               'INTA'
                              SET NEAR;
OFF
                              SCAN  ;
                               WHILE  ;
                               tab_codpre =  ;
                               'INTA'  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF()
                                   SELECT esta
                                   APPEND BLANK
                                   REPLACE estado WITH ge_tab0.tab_codtab
                                   SELECT ge_tab0
                              ENDSCAN
                         ENDIF
                         IF w_estado =  ;
                            'INRE'  ;
                            .OR.  ;
                            w_estado =  ;
                            'INVE'
                              SET NEAR;
ON
                              SEEK  ;
                               'INRE'
                              SET NEAR;
OFF
                              SCAN  ;
                               WHILE  ;
                               tab_codpre =  ;
                               'INRE'  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF()
                                   SELECT esta
                                   APPEND BLANK
                                   REPLACE estado WITH ge_tab0.tab_codtab
                                   SELECT ge_tab0
                              ENDSCAN
                         ENDIF
                         SELECT esta
                         GOTO TOP
                         SCAN WHILE   ;
                              .NOT.  ;
                              EOF()
                              SELECT  ;
                               st_iorep
                              SEEK  ;
                               esta.estado
                              SCAN  ;
                               WHILE  ;
                               auxest =  ;
                               esta.estado  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF()
                                   IF indest <> 'N'
                                        sigue = .F.
                                        IF w_estado = 'INTA' .OR. w_estado = 'INVE'
                                             IF codtall >= w_talini .AND. codtall <= w_talfin
                                                  sigue = .T.
                                             ENDIF
                                        ENDIF
                                        IF w_estado = 'INRE' .OR. w_estado = 'INVE'
                                             IF (codemi >= w_emiini .AND. codemi <= w_emifin) .AND. (indori >= w_garini .AND. indori <= w_garfin)
                                                  sigue = .T.
                                             ENDIF
                                        ENDIF
                                        IF indest <> 'N' .AND. (codtall <= '011 ' .OR. (codtall > '050 ' .AND. codtall < '060 ')) .AND. sigue
                                             sele &w_arch
                                             APPEND BLANK
                                             REPLACE numdoc WITH st_iorep.numdoc, indori WITH st_iorep.indori, numsol WITH st_iorep.numsol, codemi WITH st_iorep.codemi, codmar WITH st_iorep.codmar, codmod WITH st_iorep.codmod, numser WITH st_iorep.numser, auxest WITH st_iorep.auxest, codtall WITH st_iorep.codtall, fecemi WITH st_iorep.fecemi, fecest WITH st_iorep.fecest, codtec WITH st_iorep.codtec, codent WITH st_iorep.codent, fecha WITH DATE(), hora WITH TIME(), user WITH users, inve WITH w_estado, fecinv WITH DATE(), horinv WITH TIME()
                                        ENDIF
                                        SELECT st_iorep
                                   ENDIF
                              ENDSCAN
                              SELECT  ;
                               esta
                         ENDSCAN
                         sele &w_arch
                         COUNT TO  ;
                               w_reg
                         IF w_reg =  ;
                            0
                              DO mensa  ;
                                 WITH  ;
                                 '** Un momento, Por Favor ... **',  ;
                                 'SACA'
                              DO error  ;
                                 WITH  ;
                                 '*** No hay informaci¢n para el Reporte ***'
                         ELSE
                              INDEX  ;
                               ON  ;
                               numsol +  ;
                               codmar +  ;
                               codmod +  ;
                               numser  ;
                               TAG  ;
                               numsol
                              INDEX  ;
                               ON  ;
                               numdoc +  ;
                               codmar +  ;
                               codmod +  ;
                               numser  ;
                               TAG  ;
                               numord
                              INDEX  ;
                               ON  ;
                               codtall +  ;
                               codmar +  ;
                               codmod +  ;
                               numser  ;
                               TAG  ;
                               codtal
                              INDEX  ;
                               ON  ;
                               codemi +  ;
                               codmar +  ;
                               codmod +  ;
                               numser  ;
                               TAG  ;
                               codemi
                              INDEX  ;
                               ON  ;
                               codmar +  ;
                               codmod +  ;
                               numser  ;
                               TAG  ;
                               codmar
                              ? CHR(7)
                              ? CHR(7)
                              do error;
with '*** Anota el nombre del archivo Generado &w_arch ***'
                              DO mensa  ;
                                 WITH  ;
                                 '** Un momento, Por Favor ... **',  ;
                                 'SACA'
                              STORE  ;
                               SPACE(5)  ;
                               TO  ;
                               w_clave
                              DO WHILE  ;
                                 EMPTY(w_clave)
                                   @ 11, 20 SAY 'Ingrese Clave:' COLOR N/W,W/W  GET w_clave PICTURE '@!' VALID  .NOT. EMPTY(w_clave) .AND. LEN(ALLTRIM(w_clave)) > 1
                                   READ
                              ENDDO
                              REPLACE  ;
                               pass  ;
                               WITH  ;
                               w_clave  ;
                               ALL
                              @ 07,  ;
                                03  ;
                                CLEAR  ;
                                TO  ;
                                11,  ;
                                70
                              g =  ;
                               .F.
                         ENDIF
                    ENDIF
               ENDIF
          ENDDO
     ENDIF
ENDDO
CLOSE DATABASES
DO sacawin
RETURN
*
PROCEDURE antes
PARAMETER opc2
DO CASE
     CASE opc2 = 1
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 do ayuda with;
1
     CASE opc2 = 2
          ON KEY LABEL F6 do ayuda with;
2
     CASE opc2 = 3
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          ON KEY LABEL F6 do ayuda with;
3
     CASE opc2 = 4
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          ON KEY LABEL F6 do ayuda with;
4
     CASE opc2 = 5
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          ON KEY LABEL F6
ENDCASE
RETURN
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(w_estado)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'INME' + w_estado
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Estado no Existe ***'
               RETURN .F.
          ENDIF
          @ 05, 38 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     CASE opc = 2
          IF EMPTY(w_emiini)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + w_emiini
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Estado No Existe ***'
               RETURN .F.
          ENDIF
          @ 07, 55 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE opc = 3
          IF EMPTY(w_emifin)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + w_emifin
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Estado No Existe ***'
               RETURN .F.
          ENDIF
          @ 08, 55 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE opc = 4
          IF EMPTY(w_talini)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          IF (w_talini > '010 '  ;
             .AND. w_talini <  ;
             '020 ') .OR.  ;
             (w_talini > '060 '  ;
             .AND. w_talini <  ;
             '070 ')
               DO error WITH  ;
                  '*** No hay inventario de Domicilio ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'TALL' + w_talini
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo no Existe ***'
               RETURN .F.
          ENDIF
          @ 07, 20 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE opc = 5
          IF EMPTY(w_talfin)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          IF (w_talfin > '010 '  ;
             .AND. w_talfin <  ;
             '020 ') .OR.  ;
             (w_talfin > '060 '  ;
             .AND. w_talfin <  ;
             '070 ')
               DO error WITH  ;
                  '*** No hay inventario de Domicilio ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'TALL' + w_talfin
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo no Existe ***'
               RETURN .F.
          ENDIF
          @ 08, 20 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE opc = 6
          IF EMPTY(w_garini)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'INGA' + w_garini
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo no Existe ***'
               RETURN .F.
          ENDIF
          @ 09, 55 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE opc = 7
          IF EMPTY(w_garfin)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'INGA' + w_garfin
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo no Existe ***'
               RETURN .F.
          ENDIF
          @ 10, 55 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
ENDCASE
RETURN
*
PROCEDURE ayuda
PARAMETER opc1
SELECT ge_tab0
DO CASE
     CASE opc1 = 1
          SET FILTER TO tab_codpre = 'INME'
     CASE opc1 = 2
          SET FILTER TO tab_codpre = 'EMIS'
     CASE opc1 = 3
          SET FILTER TO tab_codpre = 'INGA'
     CASE opc1 = 4
          SET FILTER TO tab_codpre = 'TALL'
ENDCASE
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE TABLAS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
RETURN
*
FUNCTION f_archivo
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'INVEN' +  ;
           LTRIM(STR(cor))
     IF FILE(ret + '.DBF')
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
*** 
*** ReFox - retrace your steps ... 
***

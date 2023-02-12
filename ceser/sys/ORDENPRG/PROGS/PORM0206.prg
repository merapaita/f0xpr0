*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
tit_prg = 'MANTENCION'
@ 2, 1 SAY DATE() COLOR SCHEME 8
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INVENTARIO DE MERCADERIA'
CLOSE DATABASES
SELECT 1
USE ST_ISREP ORDER CODIGO
SELECT 2
USE ST_IOREP ORDER CODIGO
SELECT 3
USE ST_ICLPR ORDER CODIGO
SELECT 4
USE ST_ISERI ORDER SER_CODMAR
SELECT 5
USE GE_TAB0 ORDER CODIGO
DO WHILE .T.
     STORE SPACE(4) TO wrk_codemi,  ;
           wrk_estado, wrk_tipgar,  ;
           wrk_codmar, wrk_codemi,  ;
           wrk_indest, wrk_estado,  ;
           wrk_coddes
     STORE SPACE(20) TO  ;
           wrk_numser,  ;
           wrk_docgar
     STORE SPACE(8) TO wrk_numsol
     STORE SPACE(9) TO wrk_codent,  ;
           wrk_modelo,  ;
           wrk_codpro
     STORE CTOD('  /  /  ') TO  ;
           wrk_fecemi, wrk_fecest,  ;
           wrk_fecvta
     @ 04, 01 TO 10, 72
     @ 05, 03 SAY  ;
       'Estado del Art¡culo ..... :'  ;
       COLOR W+/N 
     @ 07, 03 SAY 'Ingreso por :'  ;
       COLOR W+/N 
     @ 11, 01 TO 14, 72
     @ 12, 03 SAY 'Cliente..:'
     @ 12, 41 SAY 'Garant¡a.:'
     @ 13, 03 SAY 'Modelo...:'
     @ 13, 41 SAY 'Serie ...:'
     a = 1
     DO WHILE a=1
          SET CURSOR ON
          @ 05, 32 GET wrk_estado  ;
            PICTURE '@!' VALID  ;
            despues(1) WHEN  ;
            antes(1)
          READ
          IF LASTKEY() = 27
               a = 0
               CLOSE DATABASES
               DO saca_win
               RETURN
          ENDIF
          DO WHILE .T.
               @ 07, 18 GET  ;
                 wrk_opcion  ;
                 DEFAULT 1 SIZE 1,  ;
                 10, 0 PICTURE  ;
                 '@*RVTN S/Servicio;O/Reparaci¢n;Serie'  ;
                 VALID  ;
                 despues(2)
               READ
               IF LASTKEY() = 27
                    EXIT
               ENDIF
          ENDDO
     ENDDO
ENDDO
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
     CASE opc = 2
     CASE opc = 3
ENDCASE
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(wrk_estado)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT 5
          SEEK 'ESOR' +  ;
               wrk_estado
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Estado No Existe ***'
               RETURN .F.
          ENDIF
          @ 05, 38 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     CASE opc = 2
          DO WHILE .T.
               DO CASE
                    CASE wrk_opcion =  ;
                         1
                         STORE 0  ;
                               TO  ;
                               wrk_numero
                         @ 08, 38  ;
                           CLEAR  ;
                           TO 08,  ;
                           70
                         @ 08, 38  ;
                           SAY  ;
                           'N£mero '  ;
                           GET  ;
                           wrk_numero  ;
                           PICTURE  ;
                           '99999999'  ;
                           VALID   ;
                           .NOT.  ;
                           EMPTY(wrk_numero)
                         READ
                         IF LASTKEY() =  ;
                            27
                              EXIT
                         ENDIF
                         SELECT 1
                         SET ORDER TO;
CODIGO
                         SEEK STR(wrk_numero,  ;
                              8)
                    CASE wrk_opcion =  ;
                         2
                         STORE 0  ;
                               TO  ;
                               wrk_numero
                         @ 08, 38  ;
                           CLEAR  ;
                           TO 08,  ;
                           70
                         @ 08, 38  ;
                           SAY  ;
                           'N£mero '  ;
                           GET  ;
                           wrk_numero  ;
                           PICTURE  ;
                           '99999999'  ;
                           VALID   ;
                           .NOT.  ;
                           EMPTY(wrk_numero)
                         READ
                         IF LASTKEY() =  ;
                            27
                              EXIT
                         ENDIF
                         SELECT 2
                         SET ORDER TO;
CODIGO
                         SEEK STR(wrk_numero,  ;
                              8)
                    CASE wrk_opcion =  ;
                         3
                         STORE SPACE(20)  ;
                               TO  ;
                               wrk_numero
                         @ 08, 38  ;
                           CLEAR  ;
                           TO 08,  ;
                           70
                         @ 08, 38  ;
                           SAY  ;
                           'N£mero '  ;
                           GET  ;
                           wrk_numero  ;
                           PICTURE  ;
                           '@!'  ;
                           VALID   ;
                           .NOT.  ;
                           EMPTY(wrk_numero)
                         READ
                         IF LASTKEY() =  ;
                            27
                              EXIT
                         ENDIF
                         SELECT 1
                         SET ORDER TO;
SOL_SERIE
                         SEEK wrk_numero
               ENDCASE
               IF FOUND()
                    wrk_numser = numser
                    wrk_tipgar = indori
                    wrk_modelo = codmod
                    wrk_codmar = codmar
                    wrk_fecemi = fecemi
                    wrk_codemi = codemi
                    wrk_codent = codent
                    wrk_indest = indest
                    IF wrk_opcion =  ;
                       2
                         wrk_numsol =  ;
                          numsol
                         wrk_estord =  ;
                          auxest
                         wrk_fecest =  ;
                          fecest
                    ELSE
                         wrk_coddes =  ;
                          coddes
                    ENDIF
                    IF wrk_opcion =  ;
                       1 .AND.  ;
                       wrk_indest <>  ;
                       'V'
                         SELECT 2
                         SET ORDER TO;
ORD_NUMSOL
                         SEEK STR(wrk_numero,  ;
                              8)
                         IF FOUND()
                              wrk_numsol =  ;
                               numsol
                              wrk_estord =  ;
                               auxest
                              wrk_fecest =  ;
                               fecest
                         ENDIF
                    ENDIF
                    IF wrk_tipgar =  ;
                       'GARA'  ;
                       .OR.  ;
                       wrk_tipgar =  ;
                       'GREC'
                         SELECT 4
                         SEEK wrk_codmar +  ;
                              wrk_modelo +  ;
                              wrk_numser
                         IF FOUND()
                              wrk_codpro =  ;
                               codent
                              wrk_fecvta =  ;
                               fecvta
                              wrk_docgar =  ;
                               docgar
                         ENDIF
                    ENDIF
                    SELECT 5
                    SEEK 'ESOR' +  ;
                         wrk_estord
                    IF FOUND()
                         wrk_desest =  ;
                          tab_destab
                    ENDIF
                    @ 12, 14 SAY  ;
                      wrk_codent
                    @ 12, 52 SAY  ;
                      wrk_tipgar +  ;
                      '  ' +  ;
                      wrk_codpro
                    @ 13, 14 SAY  ;
                      wrk_modelo
                    @ 13, 52 SAY  ;
                      wrk_numser
                    @ 14, 04 SAY  ;
                      wrk_desest  ;
                      COLOR N/W* 
                    @ 14, 60 SAY  ;
                      wrk_fecest  ;
                      COLOR N/W* 
                    DEFINE WINDOW  ;
                           grabar  ;
                           FROM  ;
                           12, 25  ;
                           TO 14,  ;
                           52
                    ACTIVATE WINDOW  ;
                             grabar
                    @ 00, 02 GET  ;
                      okcancel  ;
                      DEFAULT 1  ;
                      SIZE 1, 10,  ;
                      1 FUNCTION  ;
                      '*H \!OK;\?Cancel'
                    READ CYCLE
                    IF okcancel =  ;
                       2
                         WAIT WINDOW  ;
                              NOWAIT  ;
                              'Cancelado'
                    ELSE
                         WAIT WINDOW  ;
                              NOWAIT  ;
                              'A Grabar'
                    ENDIF
                    DEACTIVATE WINDOW  ;
                               grabar
               ENDIF
          ENDDO
ENDCASE
*
*** 
*** ReFox - retrace your steps ... 
***

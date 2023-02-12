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
   ' REGISTRO DE RESULTADOS'
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_iclpr ORDER codigo
STORE .T. TO a
DO WHILE a
     @ 04, 01 TO 09, 72
     @ 05, 03 SAY  ;
       'Archivo de Inventario: INVEN'
     @ 05, 38 SAY 'Ingreso por :'
     @ 10, 01 TO 14, 72
     DO pinta
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     STORE 1 TO w_estado
     STORE SPACE(08) TO w_arch
     SET CURSOR ON
     @ 05, 32 GET w_estado RANGE  ;
       1,999 PICTURE '999' VALID  ;
       despues(1)
     READ
     IF LASTKEY() = 27
          a = .F.
     ELSE
          STORE SPACE(05) TO  ;
                w_clave
          @ 06, 03 SAY  ;
            'Ingrese la clave     :'  ;
            COLOR W+/N,W/W  GET  ;
            w_clave PICTURE '@!'  ;
            VALID  .NOT.  ;
            EMPTY(w_clave) .AND.  ;
            LEN(ALLTRIM(w_clave)) >  ;
            1
          READ
          IF LASTKEY() = 27
               g = .F.
          ELSE
               IF w_clave <> pass
                    DO error WITH  ;
                       '*** Clave Incorrecta ***'
                    g = .F.
               ELSE
                    @ 07, 03 SAY  ;
                      'Inventario Al: ' +  ;
                      DTOC(fecinv)
                    @ 08, 12 SAY  ;
                      'Hora: ' +  ;
                      horinv
                    w_inve = inve
                    g = .T.
               ENDIF
          ENDIF
          DO WHILE g
               @ 06, 38 GET  ;
                 w_opcion DEFAULT  ;
                 1 SIZE 1, 10, 0  ;
                 PICTURE  ;
                 '@*RVTN S/Servicio;O/Reparaci¢n;Serie'
               READ
               IF LASTKEY() = 27
                    @ 06, 03  ;
                      CLEAR TO 09,  ;
                      70
                    g = .F.
               ELSE
                    STORE .T. TO  ;
                          ff
                    DO WHILE ff
                         @ 08, 50  ;
                           CLEAR  ;
                           TO 08,  ;
                           70
                         DO CASE
                              CASE  ;
                               w_opcion =  ;
                               1  ;
                               .OR.  ;
                               w_opcion =  ;
                               2
                                   STORE 0 TO w_numero
                                   @ 08, 50 GET w_numero PICTURE '99999999' VALID  .NOT. EMPTY(w_numero) .AND. w_numero > 0
                              CASE  ;
                               w_opcion =  ;
                               3
                                   STORE SPACE(20) TO w_numero
                                   @ 08, 50 GET w_numero PICTURE '@!' VALID  .NOT. EMPTY(w_numero)
                         ENDCASE
                         READ
                         IF LASTKEY() =  ;
                            27
                              @ 08,  ;
                                50  ;
                                CLEAR  ;
                                TO  ;
                                08,  ;
                                70
                              @ 11,  ;
                                02  ;
                                CLEAR  ;
                                TO  ;
                                13,  ;
                                71
                              DO pinta
                              ff =  ;
                               .F.
                         ELSE
                              DO procesa
                         ENDIF
                    ENDDO
               ENDIF
          ENDDO
     ENDIF
ENDDO
CLOSE DATABASES
DO sacawin
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
               RETURN .F.
          ENDIF
          w_arch = 'INVEN' +  ;
                   LTRIM(STR(w_estado)) +  ;
                   '.DBF'
          IF FILE(w_arch)
               SELECT 3
               use &w_arch shared
          ELSE
               do error with "*** Archivo &w_arch no Existe ***"
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
PROCEDURE procesa
sele '&w_arch'
IF w_opcion = 1
     w_num = STR(w_numero, 8)
     SET ORDER TO numsol
ELSE
     IF w_opcion = 2
          w_num = STR(w_numero,  ;
                  8)
          SET ORDER TO numord
     ELSE
          SET ORDER TO numser
     ENDIF
ENDIF
SEEK w_num
IF FOUND()
     w_codent = codent
     w_codmar = codmar
     SELECT st_iclpr
     SEEK 'C' + w_codent
     IF FOUND()
          @ 11, 14 SAY  ;
            SUBSTR(noment, 1,  ;
            26)
     ENDIF
     SELECT ge_tab0
     SEEK 'MARC' + w_codmar
     IF FOUND()
          @ 11, 52 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     ENDIF
     sele '&w_arch'
     @ 12, 14 SAY numsol
     @ 12, 52 SAY codmod
     @ 13, 14 SAY numdoc
     @ 13, 52 SAY numser
ELSE
     @ 10, 01 TO 14, 72
     @ 11, 02 CLEAR TO 13, 71
     DO pinta
ENDIF
DEFINE WINDOW grabar FROM 12, 25  ;
       TO 14, 52
ACTIVATE WINDOW grabar
@ 00, 02 GET okcancel DEFAULT 1  ;
  SIZE 1, 10, 1 FUNCTION  ;
  '*H \!OK;\?Cancel'
READ CYCLE
IF okcancel = 1
     sele '&w_arch'
     SEEK w_num
     IF  .NOT. FOUND()
          APPEND BLANK
          DO rbloquea
          IF w_opcion = 1
               REPLACE numsol  ;
                       WITH  ;
                       STR(w_numero,  ;
                       8)
          ELSE
               IF w_opcion = 2
                    REPLACE numdoc  ;
                            WITH  ;
                            STR(w_numero,  ;
                            8)
               ELSE
                    REPLACE numser  ;
                            WITH  ;
                            w_numser
               ENDIF
          ENDIF
          REPLACE pass WITH  ;
                  w_clave, conteo  ;
                  WITH '2', inve  ;
                  WITH w_inve
     ELSE
          DO rbloquea
          REPLACE conteo WITH '1'
     ENDIF
     REPLACE fecha WITH DATE(),  ;
             hora WITH TIME(),  ;
             user WITH users
     UNLOCK
ENDIF
DEACTIVATE WINDOW grabar
RETURN
*
PROCEDURE pinta
@ 11, 03 SAY 'Cliente..:'
@ 11, 41 SAY 'Marca....:'
@ 12, 03 SAY 'S/S......:'
@ 12, 41 SAY 'Modelo...:'
@ 13, 03 SAY 'O/R......:'
@ 13, 41 SAY 'Serie....:'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

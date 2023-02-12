*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
wrk_progra = PROGRAM()
DO crea_win
@ 02, 01 SAY DATE()
DO saycenter WITH 1, ' REPORTE'
DO saycenter WITH 2,  ;
   ' ELIMINACION DE ARCHIVO '
CLOSE DATABASES
STORE .T. TO a
DO WHILE a
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 01 TO 07, 72
     STORE 1 TO w_estado, w_copia
     STORE SPACE(08) TO w_arch
     STORE SPACE(05) TO w_pass
     SET CURSOR ON
     @ 05, 03 SAY  ;
       'Archivo de Inventario: INVEN'
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
               f = .F.
          ELSE
               IF w_clave <>  ;
                  w_pass
                    DO error WITH  ;
                       '*** Clave Incorrecta ***'
                    f = .F.
               ELSE
                    f = .T.
               ENDIF
          ENDIF
          DO WHILE f
               erase &w_arch
               STORE .F. TO f
               IF FILE(w_arch)
                    do error with "*** Archivo &w_arch en uso, No se puede Borrar ***"
               ELSE
                    do error with "*** Archivo &w_arch Borrado ***"
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
          IF EMPTY(w_estado)
               RETURN .F.
          ENDIF
          STORE SPACE(05) TO  ;
                w_pass
          w_arch = 'INVEN' +  ;
                   LTRIM(STR(w_estado)) +  ;
                   '.DBF'
          IF FILE(w_arch)
               use &w_arch
               w_pass = pass
               CLOSE DATABASES
          ELSE
               do error with "*** Archivo &w_arch no Existe ***"
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

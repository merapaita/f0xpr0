*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 08, 77 IN screen  ;
       COLOR SCHEME 7
ACTIVATE WINDOW tablas
DO p_prestab WITH  ;
   'ADMINISTRACION',  ;
   'ACTUALIZACION MOV. DE SUCURSAL',  ;
   'TRASFERENCIA'
@ 02, 63 SAY PROGRAM()
STORE SPACE(4) TO w_codalm
STORE SPACE(1) TO w_pro
ACTIVATE WINDOW cabecera
DO WHILE .T.
     USE ge_tab0 ORDER codigo
     SEEK 'RUTA' + 'COMM'
     IF FOUND()
          w_ruta = tab_destab
     ELSE
          w_ruta = SPACE(30)
     ENDIF
     CLOSE DATABASES
     @ 00, 04 SAY  ;
       'Almac‚n de Origen  :'
     @ 01, 04 SAY  ;
       'Direct. de Comunic.:'
     @ 02, 04 SAY  ;
       'PROCESAR [S/N]     :'
     @ 00, 25 GET w_codalm  ;
       PICTURE '@!' VALID  ;
       oovalid() COLOR SCHEME 8
     @ 01, 25 GET w_ruta PICTURE  ;
       '@!' VALID  .NOT.  ;
       EMPTY(w_ruta) COLOR SCHEME  ;
       8
     @ 02, 25 GET w_pro PICTURE  ;
       '@!' VALID w_pro $ 'SN'  ;
       COLOR SCHEME 8
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF w_pro = 'N'
          LOOP
     ENDIF
     DO proceso
ENDDO
DEACTIVATE WINDOW cabecera,  ;
           tablas
RELEASE WINDOW cabecera
CLOSE DATABASES
ON KEY
RETURN
*
PROCEDURE proceso
w_ruta = ALLTRIM(w_ruta)
w_file = w_ruta + 'MOV' +  ;
         w_codalm + '.ZIP'
if !FILE("&w_file")
     do p_mensaje with "ARCHIVO &w_file. NO EXISTE"
     RETURN
ENDIF
!PKUNZIP -o &w_file > NUL
= ooaviso('INVENTARIOS...')
@ 01, 23 SAY 'GC_HRE00.DBF'
IF FILE('C_HRE00.DBF')
     IF FILE('C_DRE00.DBF')
          USE EXCLUSIVE GC_HRE00
          DELETE FOR hre_codalm =  ;
                 w_codalm
          APPEND FROM C_HRE00.DBF  ;
                 FOR hre_codalm =  ;
                 w_codalm
          USE
          USE EXCLUSIVE GC_DRE00
          DELETE FOR dre_codalm =  ;
                 w_codalm
          APPEND FROM C_DRE00.DBF  ;
                 FOR dre_codalm =  ;
                 w_codalm
          USE
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_DRE00 NO  EXISTE'
     ENDIF
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_HRE00 NO  EXISTE'
ENDIF
RUN ERASE C_*.DBF > NUL
DEACTIVATE WINDOW winmensaje
RETURN
*
FUNCTION oovalid
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .T.
ENDIF
IF EMPTY(w_codalm)
     DO p_mensaje WITH  ;
        'No se Permiten Blancos'
     RETURN .F.
ENDIF
IF rge_codalm = w_codalm
     DO p_mensaje WITH  ;
        'Almac‚n es igual al de Trabajo'
     RETURN .F.
ENDIF
USE ge_tab0 ORDER codigo
SEEK 'ALMA' + w_codalm
IF FOUND()
     @ 00, 30 SAY tab_destab
     RETURN
ELSE
     DO p_mensaje WITH  ;
        'C¢digo de Almac‚n No Existe'
     RETURN .F.
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

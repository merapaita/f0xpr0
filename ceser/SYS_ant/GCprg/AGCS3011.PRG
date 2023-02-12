*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 09, 77 IN screen  ;
       COLOR SCHEME 7
ACTIVATE WINDOW tablas
DO p_prestab WITH  ;
   'ADMINISTRACION',  ;
   'GENERACION DE PRODUCTOS',  ;
   'TRASFERENCIA'
@ 02, 63 SAY PROGRAM()
STORE DATE() TO wrk_fecfin,  ;
      wrk_fecini
wrk_pro = SPACE(1)
ACTIVATE WINDOW cabecera
DO WHILE .T.
     USE GE_TAB0 ORDER CODIGO
     SEEK 'RUTA' + 'COMM'
     IF FOUND()
          wrk_ruta = tab_destab
     ELSE
          wrk_ruta = SPACE(30)
     ENDIF
     CLOSE DATABASES
     @ 00, 04 SAY  ;
       'Periodo Inicial    :'
     @ 01, 04 SAY  ;
       'Periodo Final      :'
     @ 02, 04 SAY  ;
       'Direct. de Comunic.:'
     @ 03, 04 SAY  ;
       'PROCESAR [S/N]     :'
     @ 00, 25 GET wrk_fecini  ;
       PICTURE '@D' COLOR SCHEME  ;
       8
     @ 01, 25 GET wrk_fecfin  ;
       PICTURE '@D' VALID  ;
       wrk_fecfin >= wrk_fecini  ;
       COLOR SCHEME 8
     @ 02, 25 GET wrk_ruta  ;
       PICTURE '@!' VALID  .NOT.  ;
       EMPTY(wrk_ruta) COLOR  ;
       SCHEME 8
     @ 03, 25 GET wrk_pro PICTURE  ;
       '!' VALID wrk_pro $ 'SN'  ;
       COLOR SCHEME 8
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF wrk_pro = 'N'
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
= ooaviso('PROCESANDO . . . .')
@ 01, 23 SAY 'GC_PRO00.DBF'
IF FILE('GC_PRO00.DBF')
     USE SHARED GC_PRO00 ORDER  ;
         CODIGO
     COPY TO C_PRO00 STRUCTURE
     USE C_PRO00
     APPEND FROM GC_PRO00 FOR  ;
            pro_fecha >=  ;
            wrk_fecini .AND.  ;
            pro_fecha <=  ;
            wrk_fecfin
     REPLACE pro_coprmo WITH 0  ;
             ALL FOR pro_propie =  ;
             'C'
     REPLACE pro_coprmb WITH 0  ;
             ALL FOR pro_propie =  ;
             'C'
     REPLACE pro_coremo WITH 0  ;
             ALL FOR pro_propie =  ;
             'C'
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_PRO00 NO  EXISTE'
ENDIF
@ 01, 23 SAY 'GC_DLP00.DBF'
IF FILE('GC_DLP00.DBF')
     USE SHARED GC_DLP00 ORDER 1
     COPY TO C_DLP00 STRUCTURE
     USE C_DLP00
     APPEND FROM GC_DLP00 FOR  ;
            dlp_fecha >=  ;
            wrk_fecini .AND.  ;
            dlp_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_DLP00 NO  EXISTE'
ENDIF
@ 01, 23 SAY 'GC_DIN00.DBF'
IF FILE('GC_DIN00.DBF')
     USE SHARED GC_DIN00 ORDER 1
     COPY TO C_DIN00 STRUCTURE
     USE C_DIN00
     APPEND FROM GC_DIN00 FOR  ;
            din_fecha >=  ;
            wrk_fecini .AND.  ;
            din_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_DIN00 NO  EXISTE'
ENDIF
@ 01, 23 SAY 'GC_ALM00.DBF'
IF FILE('GC_ALM00.DBF')
     USE SHARED gc_alm00 ORDER 3
     COPY TO c_almace FOR  ;
          alm_codalm = rge_codalm  ;
          .OR. alm_codalm =  ;
          '0090'
     USE
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_ALM00 NO EXISTE'
ENDIF
CLOSE DATABASES
= ooaviso( ;
  'EMPAQUETANDO ARCHIVOS...')
wrk_ruta = ALLTRIM(wrk_ruta)
!PKZIP -oex "&wrk_ruta.BASES.ZIP" C*.DBF;
> NUL
RUN ERASE C_*.DBF > NUL
DEACTIVATE WINDOW winmensaje
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

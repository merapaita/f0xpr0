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
   'ACTUALIZACION DE PRODUCTOS',  ;
   'TRASFERENCIA'
@ 2, 63 SAY PROGRAM()
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
       'Direct. de Comunic.:'
     @ 02, 04 SAY  ;
       'PROCESAR [S/N]     :'
     @ 00, 25 GET wrk_ruta  ;
       PICTURE '@!' VALID  .NOT.  ;
       EMPTY(wrk_ruta) COLOR  ;
       SCHEME 8
     @ 02, 25 GET wrk_pro PICTURE  ;
       '@!' VALID wrk_pro $ 'SN'  ;
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
wrk_ruta = ALLTRIM(wrk_ruta)
wrk_file = wrk_ruta + 'BASES.ZIP'
IF !FILE("&wrk_file")
     DO P_MENSAJE WITH "ARCHIVO &wrk_file. NO EXISTE"
     RETURN
ENDIF
!PKUNZIP -o "&wrk_ruta.BASES.ZIP";
 
= ooaviso('ACTUALIZANDO...')
@ 01, 23 SAY 'GC_PRO00.DBF'
IF FILE('C_PRO00.DBF')
     IF FILE('GC_PRO00.DBF')
          SELECT 1
          USE SHARED 'GC_PRO00'  ;
              ORDER CODIGO
          SELECT 2
          USE 'C_PRO00'
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SELECT gc_pro00
               SEEK c_pro00.pro_codpro
               IF FOUND()
                    DO rbloquea
               ELSE
                    APPEND BLANK
                    DO rbloquea
                    REPLACE pro_codpro  ;
                            WITH  ;
                            c_pro00.pro_codpro
               ENDIF
               REPLACE pro_descri  ;
                       WITH  ;
                       c_pro00.pro_descri,  ;
                       pro_modelo  ;
                       WITH  ;
                       c_pro00.pro_modelo,  ;
                       pro_proced  ;
                       WITH  ;
                       c_pro00.pro_proced
               REPLACE pro_propie  ;
                       WITH  ;
                       c_pro00.pro_propie,  ;
                       pro_impigv  ;
                       WITH  ;
                       c_pro00.pro_impigv,  ;
                       pro_impotr  ;
                       WITH  ;
                       c_pro00.pro_impotr
               REPLACE pro_nivcit  ;
                       WITH  ;
                       c_pro00.pro_nivcit,  ;
                       pro_moneda  ;
                       WITH  ;
                       c_pro00.pro_moneda,  ;
                       pro_estope  ;
                       WITH  ;
                       c_pro00.pro_estope
               REPLACE pro_unimed  ;
                       WITH  ;
                       c_pro00.pro_unimed,  ;
                       pro_codpve  ;
                       WITH  ;
                       c_pro00.pro_codpve,  ;
                       pro_linea  ;
                       WITH  ;
                       c_pro00.pro_linea
               REPLACE pro_catego  ;
                       WITH  ;
                       c_pro00.pro_catego,  ;
                       pro_subcat  ;
                       WITH  ;
                       c_pro00.pro_subcat,  ;
                       pro_marca  ;
                       WITH  ;
                       c_pro00.pro_marca
               REPLACE pro_rubro  ;
                       WITH  ;
                       c_pro00.pro_rubro,  ;
                       pro_subrub  ;
                       WITH  ;
                       c_pro00.pro_subrub,  ;
                       pro_clacom  ;
                       WITH  ;
                       c_pro00.pro_clacom
               REPLACE pro_numpar  ;
                       WITH  ;
                       c_pro00.pro_numpar,  ;
                       pro_parara  ;
                       WITH  ;
                       c_pro00.pro_parara,  ;
                       pro_porinf  ;
                       WITH  ;
                       c_pro00.pro_porinf
               REPLACE pro_ulcomb  ;
                       WITH  ;
                       c_pro00.pro_ulcomb,  ;
                       pro_coprmb  ;
                       WITH  ;
                       c_pro00.pro_coprmb,  ;
                       pro_coprmo  ;
                       WITH  ;
                       c_pro00.pro_coprmo
               REPLACE pro_coremo  ;
                       WITH  ;
                       c_pro00.pro_coremo,  ;
                       pro_crepro  ;
                       WITH  ;
                       c_pro00.pro_crepro,  ;
                       pro_stktra  ;
                       WITH  ;
                       c_pro00.pro_stktra
               REPLACE pro_stkmin  ;
                       WITH  ;
                       c_pro00.pro_stkmin,  ;
                       pro_critic  ;
                       WITH  ;
                       c_pro00.pro_critic,  ;
                       pro_mesegu  ;
                       WITH  ;
                       c_pro00.pro_mesegu
               REPLACE pro_lotcom  ;
                       WITH  ;
                       c_pro00.pro_lotcom,  ;
                       pro_stkmax  ;
                       WITH  ;
                       c_pro00.pro_stkmax,  ;
                       pro_codree  ;
                       WITH  ;
                       c_pro00.pro_codree
               REPLACE pro_peso  ;
                       WITH  ;
                       c_pro00.pro_peso,  ;
                       pro_dana  ;
                       WITH  ;
                       c_pro00.pro_dana,  ;
                       pro_rcom  ;
                       WITH  ;
                       c_pro00.pro_rcom
               REPLACE pro_edad  ;
                       WITH  ;
                       c_pro00.pro_edad,  ;
                       pro_stkbor  ;
                       WITH  ;
                       c_pro00.pro_stkbor,  ;
                       pro_stksol  ;
                       WITH  ;
                       c_pro00.pro_stksol
               REPLACE pro_ultcos  ;
                       WITH  ;
                       c_pro00.pro_ultcos,  ;
                       pro_usuari  ;
                       WITH clave,  ;
                       pro_fecha  ;
                       WITH  ;
                       DATE(),  ;
                       pro_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
               SELECT c_pro00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_PRO00 NO EXISTE '
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_DLP00.DBF'
IF FILE('C_DLP00.DBF')
     IF FILE('GC_DLP00.DBF')
          SELECT 1
          USE SHARED 'GC_DLP00'  ;
              ORDER CODIGO
          SELECT 2
          USE 'C_DLP00'
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_dlp00
               SEEK c_dlp00.dlp_codlis +  ;
                    c_dlp00.dlp_codpro +  ;
                    c_dlp00.dlp_unimed
               IF  .NOT. FOUND()
                    APPEND BLANK
               ENDIF
               DO rbloquea
               GATHER MEMVAR
               UNLOCK
               SELECT c_dlp00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_DLP00 NO EXISTE '
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_DIN00.DBF'
IF FILE('C_DIN00.DBF')
     IF FILE('GC_DIN00.DBF')
          SELECT 1
          USE SHARED 'GC_DIN00'  ;
              ORDER CODIGO
          SELECT 2
          USE 'C_DIN00'
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_din00
               SEEK c_din00.din_codpro
               IF  .NOT. FOUND()
                    APPEND BLANK
               ENDIF
               DO rbloquea
               GATHER MEMVAR
               UNLOCK
               SELECT c_din00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_DIN00 NO EXISTE '
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_ALM00.DBF'
IF FILE('C_ALMACE.DBF')
     IF FILE('GC_ALM00.DBF')
          SELECT 1
          USE SHARED GC_ALM00  ;
              ORDER CODIGO
          SELECT 2
          USE C_ALMACE
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SELECT 1
               SEEK c_almace.alm_codpro +  ;
                    c_almace.alm_codalm
               IF  .NOT. FOUND()
                    APPEND BLANK
                    DO rbloquea
                    REPLACE alm_codpro  ;
                            WITH  ;
                            c_almace.alm_codpro
                    REPLACE alm_codalm  ;
                            WITH  ;
                            c_almace.alm_codalm
               ELSE
                    DO rbloquea
               ENDIF
               REPLACE alm_stkfis  ;
                       WITH  ;
                       c_almace.alm_stkfis,  ;
                       alm_stkres  ;
                       WITH  ;
                       c_almace.alm_stkres,  ;
                       alm_ubicac  ;
                       WITH  ;
                       c_almace.alm_ubicac
               REPLACE alm_usuari  ;
                       WITH clave,  ;
                       alm_fecha  ;
                       WITH  ;
                       DATE(),  ;
                       alm_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
               SELECT c_almace
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_ALM00 NO EXISTE '
     ENDIF
ENDIF
CLOSE DATABASES
RUN ERASE C*.DBF > NUL
RELEASE WINDOW winmensaje
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

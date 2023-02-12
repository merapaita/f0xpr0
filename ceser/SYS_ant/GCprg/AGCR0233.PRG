*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 08, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 09, 02  ;
       TO 21, 77 TITLE  ;
       ' COMPRA, CONSUMO/DEV. DE SUCURSAL '  ;
       COLOR SCHEME 7
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED gc_pro00 ORDER marca
SELECT 3
USE SHARED st_iorep ORDER codigo
SELECT 4
USE SHARED gc_kar00 ORDER fecha
SELECT 5
USE EXCLUSIVE gc_hre00
SELECT 6
USE EXCLUSIVE gc_dre00
SELECT 7
USE SHARED gc_par00
w_progra = PROGRAM()
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'COMPRA, CONSUMO/DEV. DE SUCURSAL',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
STORE DATE() TO w_fecini,  ;
      w_fecfin
DO WHILE .T.
     ACTIVATE WINDOW cabecera
     STORE 1 TO w_copia
     STORE 0 TO w_totdev,  ;
           w_totegr, w_totcom,  ;
           w_toting, w_totcoi,  ;
           w_par01, w_cont,  ;
           w_cont2, w_cont3,  ;
           w_cont4, w_cont5
     CLEAR
     @ 00, 01 SAY 'Per¡odo :'  ;
       COLOR N/W 
     @ 01, 05 SAY 'Del :'
     @ 02, 05 SAY 'Al  :'
     @ 00, 26 SAY 'Destino :'  ;
       COLOR N/W 
     @ 01, 12 GET w_fecini  ;
       PICTURE '@D' VALID  ;
       oovalid(3) .AND.  .NOT.  ;
       EMPTY(w_fecini) COLOR  ;
       SCHEME 08
     @ 02, 12 GET w_fecfin RANGE  ;
       w_fecini PICTURE '@D'  ;
       VALID oovalid(4) .AND.   ;
       .NOT. EMPTY(w_fecfin)  ;
       COLOR SCHEME 08
     @ 01, 34 GET opc DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVTN Pantalla ;Impresora'  ;
       VALID oovalid(opc) COLOR  ;
       SCHEME 12
     READ CYCLE
     IF LASTKEY() = 27
          EXIT
     ENDIF
     SELECT gc_hre00
     ZAP
     SELECT gc_dre00
     ZAP
     = ooaviso( ;
       'P R O C E S A N D O . . .' ;
       )
     SELECT gc_par00
     w_numpla = par_numpla
     w_numcom = par_numcom
     SELECT gc_kar00
     SET RELATION TO SUBSTR(kar_nrdore,;
1, 8) INTO st_iorep
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     DO WHILE kar_fecdoc<= ;
        w_fecfin .AND.  .NOT.  ;
        EOF()
          IF kar_cantid > 0 .AND.  ;
             (SUBSTR(kar_codmov,  ;
             1, 1) = 'E' .OR.  ;
             kar_codmov = 'IFD '  ;
             .OR. kar_codmov =  ;
             'IFL ' .OR.  ;
             kar_codmov = 'COI '  ;
             .OR. kar_codmov =  ;
             'IFO ')
               w_fecdoc = kar_fecing
               w_tipdoc = kar_tipdoc
               w_nrodoc = kar_nrodoc
               w_codmov = kar_codmov
               w_codpro = kar_codpro
               w_canpro = kar_cantid
               w_valpro = kar_import
               w_totite = (w_valpro *  ;
                          w_canpro)
               w_docref = kar_nrdore
               w_indori = st_iorep.indori
               w_codtal = st_iorep.codtall
               w_codemi = st_iorep.codemi
               DO CASE
                    CASE SUBSTR(w_codmov,  ;
                         1, 1) =  ;
                         'E'
                         w_tippla =  ;
                          'PLAN'
                         IF w_cont =  ;
                            0
                              w_numpla =  ;
                               w_numpla +  ;
                               1
                              w_nropla =  ;
                               STR(w_numpla,  ;
                               10)
                              w_plaegr =  ;
                               f_ceros(w_nropla, ;
                               10, ;
                               2)
                              w_cont =  ;
                               w_cont +  ;
                               1
                         ENDIF
                         w_totegr =  ;
                          w_totegr +  ;
                          1
                    CASE w_codmov =  ;
                         'IFL '
                         w_tippla =  ;
                          'RCLR'
                         IF w_cont2 =  ;
                            0
                              w_numcom =  ;
                               w_numcom +  ;
                               1
                              w_nrocom =  ;
                               STR(w_numcom +  ;
                               1,  ;
                               10)
                              w_placom =  ;
                               f_ceros(w_nrocom, ;
                               10, ;
                               2)
                              w_cont2 =  ;
                               w_cont2 +  ;
                               1
                         ENDIF
                         w_totcom =  ;
                          w_totcom +  ;
                          1
                    CASE w_codmov =  ;
                         'IFO '
                         w_tippla =  ;
                          'PLAN'
                         IF w_cont4 =  ;
                            0
                              w_numpla =  ;
                               w_numpla +  ;
                               1
                              w_nropla =  ;
                               STR(w_numpla,  ;
                               10)
                              w_plaing =  ;
                               f_ceros(w_nropla, ;
                               10, ;
                               2)
                              w_cont4 =  ;
                               w_cont4 +  ;
                               1
                         ENDIF
                         w_toting =  ;
                          w_toting +  ;
                          1
                    CASE w_codmov =  ;
                         'COI '
                         w_tippla =  ;
                          'PLAN'
                         IF w_cont3 =  ;
                            0
                              w_numpla =  ;
                               w_numpla +  ;
                               1
                              w_nropla =  ;
                               STR(w_numpla,  ;
                               10)
                              w_placoi =  ;
                               f_ceros(w_nropla, ;
                               10, ;
                               2)
                              w_cont3 =  ;
                               w_cont3 +  ;
                               1
                         ENDIF
                         w_totcoi =  ;
                          w_totcoi +  ;
                          1
                    CASE w_codmov =  ;
                         'IFD '
                         w_tippla =  ;
                          'PLAN'
                         IF w_cont5 =  ;
                            0
                              w_numpla =  ;
                               w_numpla +  ;
                               1
                              w_nropla =  ;
                               STR(w_numpla,  ;
                               10)
                              w_pladev =  ;
                               f_ceros(w_nropla, ;
                               10, ;
                               2)
                              w_cont5 =  ;
                               w_cont5 +  ;
                               1
                         ENDIF
                         w_totdev =  ;
                          w_totdev +  ;
                          1
               ENDCASE
               SELECT gc_dre00
               APPEND BLANK
               REPLACE dre_tiprep  ;
                       WITH  ;
                       w_tippla
               DO CASE
                    CASE SUBSTR(w_codmov,  ;
                         1, 1) =  ;
                         'E'
                         REPLACE dre_nrorep  ;
                                 WITH  ;
                                 w_plaegr
                    CASE w_codmov =  ;
                         'IFL '
                         REPLACE dre_nrorep  ;
                                 WITH  ;
                                 w_placom
                    CASE w_codmov =  ;
                         'IFO '
                         REPLACE dre_nrorep  ;
                                 WITH  ;
                                 w_plaing
                    CASE w_codmov =  ;
                         'IFD '
                         REPLACE dre_nrorep  ;
                                 WITH  ;
                                 w_pladev
                    CASE w_codmov =  ;
                         'COI '
                         REPLACE dre_nrorep  ;
                                 WITH  ;
                                 w_placoi
               ENDCASE
               REPLACE dre_tipdoc  ;
                       WITH  ;
                       w_tipdoc
               REPLACE dre_nrodoc  ;
                       WITH  ;
                       w_nrodoc
               REPLACE dre_fecdoc  ;
                       WITH  ;
                       w_fecdoc
               REPLACE dre_codalm  ;
                       WITH  ;
                       rge_codalm
               REPLACE dre_codmov  ;
                       WITH  ;
                       w_codmov
               REPLACE dre_codpro  ;
                       WITH  ;
                       w_codpro
               REPLACE dre_canpro  ;
                       WITH  ;
                       w_canpro
               REPLACE dre_valpro  ;
                       WITH  ;
                       w_valpro
               REPLACE dre_totite  ;
                       WITH  ;
                       w_totite
               REPLACE dre_numord  ;
                       WITH  ;
                       w_docref
               REPLACE dre_indori  ;
                       WITH  ;
                       w_indori
               REPLACE dre_codtal  ;
                       WITH  ;
                       w_codtal
               REPLACE dre_codemi  ;
                       WITH  ;
                       w_codemi
               REPLACE dre_usuari  ;
                       WITH  ;
                       clave
               REPLACE dre_fecha  ;
                       WITH  ;
                       DATE()
               REPLACE dre_hora  ;
                       WITH  ;
                       TIME()
          ENDIF
          SELECT gc_kar00
          SKIP
     ENDDO
     SELECT gc_hre00
     IF w_totegr > 0
          APPEND BLANK
          REPLACE hre_nrorep WITH  ;
                  w_plaegr
          REPLACE hre_tiprep WITH  ;
                  'PLAN'
          REPLACE hre_codalm WITH  ;
                  rge_codalm
          REPLACE hre_fecemi WITH  ;
                  DATE()
          REPLACE hre_indest WITH  ;
                  'V'
          REPLACE hre_usuari WITH  ;
                  clave
          REPLACE hre_fecha WITH  ;
                  DATE()
          REPLACE hre_hora WITH  ;
                  TIME()
          w_par01 = 1
     ENDIF
     IF w_totdev > 0
          APPEND BLANK
          REPLACE hre_nrorep WITH  ;
                  w_pladev
          REPLACE hre_tiprep WITH  ;
                  'PLAN'
          REPLACE hre_codalm WITH  ;
                  rge_codalm
          REPLACE hre_fecemi WITH  ;
                  DATE()
          REPLACE hre_indest WITH  ;
                  'V'
          REPLACE hre_usuari WITH  ;
                  clave
          REPLACE hre_fecha WITH  ;
                  DATE()
          REPLACE hre_hora WITH  ;
                  TIME()
          w_par01 = 1
     ENDIF
     IF w_toting > 0
          APPEND BLANK
          REPLACE hre_nrorep WITH  ;
                  w_plaing
          REPLACE hre_tiprep WITH  ;
                  'PLAN'
          REPLACE hre_codalm WITH  ;
                  rge_codalm
          REPLACE hre_fecemi WITH  ;
                  DATE()
          REPLACE hre_indest WITH  ;
                  'V'
          REPLACE hre_usuari WITH  ;
                  clave
          REPLACE hre_fecha WITH  ;
                  DATE()
          REPLACE hre_hora WITH  ;
                  TIME()
          w_par01 = 1
     ENDIF
     IF w_totcoi > 0
          APPEND BLANK
          REPLACE hre_nrorep WITH  ;
                  w_placoi
          REPLACE hre_tiprep WITH  ;
                  'PLAN'
          REPLACE hre_codalm WITH  ;
                  rge_codalm
          REPLACE hre_fecemi WITH  ;
                  DATE()
          REPLACE hre_indest WITH  ;
                  'V'
          REPLACE hre_usuari WITH  ;
                  clave
          REPLACE hre_fecha WITH  ;
                  DATE()
          REPLACE hre_hora WITH  ;
                  TIME()
          w_par01 = 1
     ENDIF
     IF w_par01 = 1 .AND. opc = 2
          SELECT gc_par00
          DO rbloquea
          REPLACE par_numpla WITH  ;
                  w_numpla
          UNLOCK
     ENDIF
     IF w_totcom > 0
          SELECT gc_hre00
          APPEND BLANK
          REPLACE hre_codalm WITH  ;
                  rge_codalm
          REPLACE hre_tiprep WITH  ;
                  'RCLR'
          REPLACE hre_nrorep WITH  ;
                  w_placom
          REPLACE hre_fecemi WITH  ;
                  DATE()
          REPLACE hre_indest WITH  ;
                  'V'
          REPLACE hre_usuari WITH  ;
                  clave
          REPLACE hre_fecha WITH  ;
                  DATE()
          REPLACE hre_hora WITH  ;
                  TIME()
          IF opc = 2
               SELECT gc_par00
               DO rbloquea
               REPLACE par_numcom  ;
                       WITH  ;
                       w_numcom
               UNLOCK
          ENDIF
     ENDIF
     = ooimprime()
     IF opc = 2
          = envio()
     ENDIF
ENDDO
RELEASE WINDOW winpedido, detalle,  ;
        cabecera
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
IF cvalid = 2
     SET CURSOR ON
     @ 02, 50 SAY 'Copias :' GET  ;
       w_copia PICTURE '99' VALID   ;
       .NOT. EMPTY(w_copia) .AND.  ;
       w_copia > 0 .AND. w_copia <  ;
       5
     READ
     IF LASTKEY() = 27
          @ 01, 19 SAY SPACE(20)
          RETURN .F.
     ENDIF
ENDIF
RETURN
*
PROCEDURE ooimprime
IF opc = 1
     w_file = SUBSTR(f_archivo(),  ;
              1, 8) + '.doc'
     = ooaviso( ;
       'C  O  P  I  A  N  D  O . . .' ;
       )
     SELECT gc_dre00
     SET ORDER TO codigo
     REPO FORM AGCR0233 TO FILE &w_file;
 NOCONSOLE 
     SET SYSMENU ON
     MODI COMM &w_file WIND DETALLE NOEDIT
     SET SYSMENU OFF
     ERASE FILE &w_file
     DEACTIVATE WINDOW winmensaje
     ACTIVATE WINDOW detalle
     CLEAR
ELSE
     DO WHILE .T.
          IF SYS(13) = 'OFFLINE'
               swt = f_yesno( ;
                     'Impresora fuera de Linea. Intentar nuevamente ' ;
                     )
               IF swt
                    LOOP
               ELSE
                    DEACTIVATE WINDOW  ;
                               winmensaje
                    RETURN
               ENDIF
          ELSE
               EXIT
          ENDIF
     ENDDO
     = ooaviso( ;
       'I M P R I M I E N D O . . .' ;
       )
     SELECT gc_dre00
     SET ORDER TO codigo
     FOR a = 1 TO w_copia
          SET DEVICE TO PRINTER
          ??? CHR(27) + CHR(15)
          REPORT FORMAT AGCR0233  ;
                 TO PRINTER  ;
                 NOCONSOLE
          SET DEVICE TO SCREEN
     ENDFOR
     SET PRINTER TO
     RELEASE WINDOW winmensaje
ENDIF
DO p_mensaje WITH  ;
   ' FIN DE IMPRESION '
RETURN
*
PROCEDURE envio
SELECT gc_hre00
COPY TO C_HRE00 STRUCTURE
SELECT gc_dre00
COPY TO C_DRE00 STRUCTURE
SELECT 11
USE C_HRE00
APPEND FROM gc_hre00
USE
SELECT 12
USE C_DRE00
APPEND FROM gc_dre00
USE
SELECT ge_tab0
SEEK 'RUTA' + 'COMM'
IF FOUND()
     w_ruta = tab_destab
ELSE
     w_ruta = SPACE(30)
ENDIF
w_ruta = ALLTRIM(w_ruta)
w_file = w_ruta + 'MOV' +  ;
         rge_codalm + '.ZIP'
!PKZIP -oex &w_file C_HRE00.*  C_DRE00.*;
> NUL
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

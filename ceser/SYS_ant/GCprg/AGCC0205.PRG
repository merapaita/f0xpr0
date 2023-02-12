*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 08, 77 IN screen  ;
       COLOR W/N 
DEFINE WINDOW detalle FROM 09, 02  ;
       TO 20, 77 IN screen COLOR  ;
       SCHEME 10
DEFINE WINDOW titu FROM 10, 02 TO  ;
       11, 77 IN screen NONE  ;
       COLOR SCHEME 8
DEFINE WINDOW ven1 FROM 10, 15 TO  ;
       18, 67 IN screen COLOR  ;
       SCHEME 10
SELECT 1
USE SHARED gc_pro00 ORDER codigo
w_selpro = SELECT()
SELECT 2
USE SHARED gc_kar00 ORDER  ;
    kar_codpro
SELECT 3
USE SHARED st_imode ORDER codigo
SELECT 4
USE SHARED st_iorep ORDER codigo
SET RELATION TO codmar + codmod INTO st_imode
SELECT 5
USE SHARED gc_alm00 ORDER codigo
SELECT 6
USE SHARED ge_tab0 ORDER codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'CONSULTAS',  ;
   'MODELOS UTILIZADOS',  ;
   'CONSULTA'
@ 02, 63 SAY PROGRAM()
SELECT gc_pro00
DO WHILE .T.
     STORE 0 TO stkbas, stksuc
     w_codigo = SPACE(14)
     w_codmod = SPACE(20)
     w_selec = SELECT()
     w_campo = 'w_codigo'
     ON KEY LABEL f7
     ON KEY LABEL F6 do produc with w_campo,w_selec,w_selpro;

     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW detalle,  ;
              cabecera
     CLEAR
     @ 00, 01 SAY  ;
       'C¢digo......: ' COLOR W+/ ;
       N 
     @ 01, 01 SAY  ;
       'Descripci¢n.: '
     @ 02, 01 SAY  ;
       'Reemplazo...: '
     @ 00, 40 SAY  ;
       'Nro.de Parte......: '
     @ 01, 40 SAY 'Stock ' +  ;
       SUBSTR(ootab('ALMA', ;
       rge_codalm), 1, 12) + ':'
     @ 02, 40 SAY  ;
       'Stock Otros.......: '
     @ 00, 15 GET w_codigo  ;
       PICTURE '@!' VALID  ;
       valcod(w_codigo) COLOR W+/ ;
       N 
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          ERASE indmod.idx
          ON KEY LABEL f6
          ON KEY
          RELEASE WINDOW ven1,  ;
                  cabecera,  ;
                  detalle, titu
          DEACTIVATE WINDOW  ;
                     tablas
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     IF w_codigo = SPACE(14)
          LOOP
     ELSE
          w_codmod = pro_modelo
          @ 01, 15 SAY  ;
            SUBSTR(pro_descri, 1,  ;
            20)
          @ 02, 15 SAY pro_codree
          @ 00, 61 SAY pro_numpar
          SELECT gc_alm00
          SUM FOR alm_codpro =  ;
              w_codigo .AND.  ;
              alm_codalm =  ;
              rge_codalm  ;
              alm_stkfis TO  ;
              stkbas
          SUM FOR alm_codpro =  ;
              w_codigo .AND.  ;
              alm_codalm <>  ;
              rge_codalm  ;
              alm_stkfis TO  ;
              stksuc
          @ 01, 61 SAY stkbas  ;
            PICTURE  ;
            '999,999,999'
          @ 02, 61 SAY stksuc  ;
            PICTURE  ;
            '999,999,999'
          CREATE CURSOR modelo  ;
                 (codmod C (15),  ;
                 nommod C (20),  ;
                 codmar C (4),  ;
                 nrdore C (10),  ;
                 fecdoc D, cantid  ;
                 N (7, 2), precio  ;
                 N (9, 2), total  ;
                 N (9, 2))
          DO llena
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          @ 00, 21 SAY '[ Up ]'  ;
            COLOR N/W 
          @ 00, 28 SAY  ;
            'Avanza P g.'
          @ 00, 41 SAY '[Down]'  ;
            COLOR N/W 
          @ 00, 48 SAY  ;
            'Retroc P g.'
          @ 00, 61 SAY '[ F8 ]'  ;
            COLOR N/W 
          @ 00, 68 SAY  ;
            'Doc. Ref.  '
          @ 01, 01 SAY '[Home]'  ;
            COLOR N/W 
          @ 01, 08 SAY  ;
            'Principio  '
          @ 01, 21 SAY '[End ]'  ;
            COLOR N/W 
          @ 01, 28 SAY  ;
            'Ultimo    '
          @ 01, 41 SAY '[ F7 ]'  ;
            COLOR N/W 
          @ 01, 48 SAY 'Imprimir'
          ACTIVATE WINDOW detalle
          SELECT modelo
          IF RECCOUNT() = 0
               DO p_mensaje WITH  ;
                  'NO HAY INFORMACION PARA CONSULTAR'
          ELSE
               ON KEY
               ON KEY LABEL f7 do imprimir
               ON KEY LABEL F8 do detalle
               ON KEY LABEL home do inicio
               ON KEY LABEL end do final
               SELECT mod1
               GOTO TOP
               BROWSE FIELDS codi  ;
                      :H =  ;
                      'CODIGO-MODELO',  ;
                      mod :H =  ;
                      'MODELO',  ;
                      marca :H =  ;
                      'MARCA',  ;
                      cant :P =  ;
                      '99,999' :H =  ;
                      'CANTID.',  ;
                      prepro :P =  ;
                      '999,999.99'  ;
                      :H =  ;
                      'PRECIO ' +  ;
                      sys_simomb,  ;
                      totpre :P =  ;
                      '999,999.99'  ;
                      :H =  ;
                      'TOTAL ' +  ;
                      sys_simomb  ;
                      NOEDIT IN  ;
                      detalle  ;
                      NOLGRID
               IF LASTKEY() = 27
                    ON KEY
                    DEACTIVATE WINDOW  ;
                               detalle
                    LOOP
               ENDIF
          ENDIF
     ENDIF
     ERASE indmod.idx
ENDDO
CLOSE DATABASES
SET RELATION TO
ON KEY LABEL f6
ON KEY LABEL f7
RETURN
*
PROCEDURE inicio
GOTO TOP
RETURN
*
PROCEDURE final
GOTO BOTTOM
RETURN
*
FUNCTION valcod
PARAMETER codigo
fla = .T.
SELECT gc_pro00
SET ORDER TO codigo
IF codigo <> SPACE(4)
     SEEK codigo
     IF  .NOT. FOUND()
          DO p_mensaje WITH  ;
             'CODIGO NO EXISTE'
          w_codigo = SPACE(14)
          @ 00, 30 SAY  ;
            REPLICATE(' ', 15)
          fla = .F.
     ELSE
          IF pro_rcom = 'H'
               DO p_mensaje WITH  ;
                  'FIN DE SERVICIO'
          ENDIF
     ENDIF
ENDIF
RETURN fla
*
PROCEDURE llena
w_nrdore = SPACE(10)
SELECT gc_kar00
SEEK w_codigo + 'ORDE'
IF FOUND()
     DO WHILE kar_codpro=w_codigo  ;
        .AND. kar_tidore='ORDE'  ;
        .AND.  .NOT. EOF()
          IF kar_codmov = 'EPRD'
               w_recno = RECNO()
               w_tipo = kar_tipdoc
               w_numdoc = kar_nrodoc
               w_cantid = kar_cantid
               w_nrdore = kar_nrdore
               SET ORDER TO docpro
               SEEK w_tipo +  ;
                    w_numdoc +  ;
                    w_codigo
               IF FOUND()
                    SCAN WHILE  ;
                         kar_tipdoc =  ;
                         w_tipo  ;
                         .AND.  ;
                         kar_nrodoc =  ;
                         w_numdoc  ;
                         .AND.  ;
                         kar_codpro =  ;
                         w_codigo  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         IF kar_cantid <=  ;
                            w_cantid  ;
                            .AND.  ;
                            kar_codmov =  ;
                            'IFD '
                              w_cantid =  ;
                               w_cantid -  ;
                               kar_cantid
                         ENDIF
                    ENDSCAN
               ENDIF
               SET ORDER TO kar_codpro
               GOTO w_recno
               IF w_cantid > 0
                    w_nrdore = SPACE(8 -  ;
                               LEN(ALLTRIM(w_nrdore))) +  ;
                               ALLTRIM(w_nrdore)
                    SELECT st_iorep
                    SEEK w_nrdore
                    IF FOUND()
                         SELECT modelo
                         APPEND BLANK
                         REPLACE codmod  ;
                                 WITH  ;
                                 st_iorep.codmod,  ;
                                 nommod  ;
                                 WITH  ;
                                 st_imode.nommod
                         REPLACE codmar  ;
                                 WITH  ;
                                 st_iorep.codmar,  ;
                                 nrdore  ;
                                 WITH  ;
                                 gc_kar00.kar_nrdore
                         REPLACE fecdoc  ;
                                 WITH  ;
                                 gc_kar00.kar_fecdoc,  ;
                                 cantid  ;
                                 WITH  ;
                                 w_cantid
                         REPLACE precio  ;
                                 WITH  ;
                                 gc_kar00.kar_import,  ;
                                 total  ;
                                 WITH  ;
                                 cantid *  ;
                                 precio
                    ENDIF
               ENDIF
               SELECT gc_kar00
               SKIP
          ENDIF
     ENDDO
     CREATE CURSOR mod1 (codi C  ;
            (15), mod C (20),  ;
            marca C (4), cant N  ;
            (7, 2), prepro N (9,  ;
            2), totpre N (9, 2))
     SELECT modelo
     INDEX ON codmod + nrdore TO  ;
           inmod
     GOTO TOP
     IF RECCOUNT() > 0
          w_cantid = 0
          w_total = 0
          w_codmod = codmod
          w_mod = nommod
          w_marca = codmar
          DO WHILE  .NOT. EOF()
               IF w_codmod =  ;
                  codmod .AND.   ;
                  .NOT. EOF()
                    w_cantid = w_cantid +  ;
                               cantid
                    w_total = w_total +  ;
                              total
                    SKIP
               ELSE
                    SELECT mod1
                    APPEND BLANK
                    REPLACE codi  ;
                            WITH  ;
                            w_codmod,  ;
                            mod  ;
                            WITH  ;
                            w_mod,  ;
                            marca  ;
                            WITH  ;
                            w_marca
                    REPLACE cant  ;
                            WITH  ;
                            w_cantid,  ;
                            prepro  ;
                            WITH  ;
                            w_total /  ;
                            w_cantid,  ;
                            totpre  ;
                            WITH  ;
                            w_total
                    w_cantid = 0
                    w_total = 0
                    SELECT modelo
                    w_codmod = codmod
                    w_mod = nommod
                    w_marca = codmar
               ENDIF
          ENDDO
          SELECT mod1
          APPEND BLANK
          REPLACE codi WITH  ;
                  w_codmod, mod  ;
                  WITH w_mod,  ;
                  marca WITH  ;
                  w_marca
          REPLACE cant WITH  ;
                  w_cantid,  ;
                  prepro WITH  ;
                  w_total /  ;
                  w_cantid,  ;
                  totpre WITH  ;
                  w_total
     ENDIF
ENDIF
RETURN
*
PROCEDURE detalle
SELECT modelo
GOTO TOP
ACTIVATE WINDOW ven1
ON KEY LABEL f7
ON KEY LABEL f8
BROWSE FOR codmod = mod1.codi  ;
       FIELDS nrdore :H =  ;
       'DOC.REFER.', fecdoc :H =  ;
       'FECHA', cantid :P =  ;
       '999,999' :H = 'CANTID.',  ;
       precio :P = '99,999.99' :H =  ;
       'PRECIO ' + sys_simomb,  ;
       total :P = '999,999.99' :H =  ;
       'TOTAL ' + sys_simomb  ;
       NOEDIT IN ven1 NOLGRID
IF LASTKEY() = 27
     DEACTIVATE WINDOW ven1
ENDIF
ON KEY LABEL f7 do imprimir
ON KEY LABEL f8 do detalle
RETURN
*
PROCEDURE imprimir
SELECT modelo
GOTO TOP
SET RELATION TO 'MARC' + codmar INTO ge_tab0;
ADDITIVE
??? CHR(15)
REPORT FORMAT agcc0205 TO PRINTER  ;
       NOCONSOLE
??? CHR(27) + CHR(80)
DEACTIVATE WINDOW ven1
DEACTIVATE WINDOW detalle
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

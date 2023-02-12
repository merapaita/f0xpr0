*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
IF config_prg == 1
     ind_prg = '<PORM0171>'
     tit_prg = 'MANTENCION'
ELSE
     ind_prg = '<PORQ0309>'
     tit_prg = 'CONSULTA'
ENDIF
@ 24, 69 SAY ind_prg
PUBLIC es8
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA04
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' FICHA DE N§ SERIES'
@ 3, 2 CLEAR TO 7, 77
@ 3, 2 TO 7, 77
@ 4, 5 SAY 'Codigo Proveedor  :'
@ 5, 5 SAY 'Marca             :'
@ 6, 5 SAY 'Fecha Ingreso     :'
ppas = .T.
DO WHILE ppas
     @ 8, 2 CLEAR TO 13, 77
     @ 4, 35 SAY SPACE(30)
     @ 5, 35 SAY SPACE(30)
     @ 3, 2 TO 7, 77
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'RAC',  ;
        'BBB', 'IGN', 'ESC'
     wk_codent = 0
     wk_codmar = SPACE(4)
     wk_fecing = DATE()
     efecin = 1
     @ 4, 25 GET wk_codent  ;
       PICTURE '999999999' VALID  ;
       provee(wk_codent,35) WHEN  ;
       colocaf6()
     @ 5, 25 GET wk_codmar  ;
       PICTURE '!!!!' VALID  ;
       valtab('MARC',wk_codmar,35, ;
       30) WHEN colocaf6()
     @ 6, 25 GET wk_fecing
     SET CURSOR ON
     READ
     SET CURSOR OFF
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               ppas = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               LOOP
     ENDCASE
     SELECT 1
     USE SHARED st_iseri ORDER  ;
         CODIGO
     SEEK STR(wk_codent, 9) +  ;
          wk_codmar +  ;
          DTOC(wk_fecing)
     IF  .NOT. FOUND() .AND.  ;
         config_prg <> 1
          USE
          DO error WITH  ;
             '** Ingreso de Series NO EXISTE **'
          LOOP
     ENDIF
     es8 = da_nombre()
     copy to &es8 for codent == str(wk_codent,9);
.and. codmar == wk_codmar;
.and. fecing == wk_fecing
     use &es8 EXCLUSIVE     
     APPEND BLANK
     GOTO TOP
     DO esc_modo WITH 'I'
     IF config_prg == 1
          DO esc_indica WITH 1,  ;
             'AYU', 'MD3', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'MBV', 'ELI', 'IGN',  ;
             'ESC'
     ELSE
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'MBV', 'BBB', 'IGN',  ;
             'ESC'
     ENDIF
     @ 7, 2 TO 13, 77
     @ 7, 2 SAY 'Ã'
     @ 7, 74 SAY '´'
     @ 8, 3 SAY  ;
       'Numero Serie         Modelo          Doc.Gtia.       Fch.Vta.  Fch.Venc'
     campo = 'NUMSER + " " + MODELO + " " + DOCGAR + " " + dtoc(FECVTA) + "  " + dtoc(FECGAR)'
     DO dbedit WITH 09, 3, 11, 73,  ;
        campo, 'MYFUNC2()'
     CLOSE DATABASES
ENDDO
DO saca_win
@ 24, 69 SAY SPACE(10)
IF  .NOT. EMPTY(es8)
     x3 = es8 + '.DBF'
     ERASE &X3
ENDIF
RELEASE es8
RETURN
*
FUNCTION myfunc2
retval = 1
IF config_prg == 1 .AND.  ;
   (LASTKEY() == -2 .OR.  ;
   (LASTKEY() == -3 .AND. RECNO() <>  ;
   RECCOUNT()))
     SAVE SCREEN TO p0102x
     x1 = ROW()
     x2 = COL()
     key_aux = LASTKEY()
     DO CASE
          CASE key_aux == -2
               DO esc_indica WITH  ;
                  1, 'AYU', 'BBB',  ;
                  'BBB', 'BBB'
               DO esc_indica WITH  ;
                  2, 'BBB', 'BBB',  ;
                  'IGN', 'ESC'
               DO esc_modo WITH  ;
                  'I'
               varx1 = numser
               varx2 = modelo
               varx3 = fecvta
               varx4 = fecgar
               vardoc = docgar
               varx5 = 0
               @ x1, x2 SAY ''
               @ ROW(), COL() GET  ;
                 varx1 PICTURE  ;
                 '@!' VALID  ;
                 codalf6(varx1)
               ?? ' '
               @ ROW(), COL() GET  ;
                 varx2 PICTURE  ;
                 '@!' VALID  ;
                 codmod2(varx2,0, ;
                 0) WHEN  ;
                 colocaf6()
               ?? ' '
               @ ROW(), COL() GET  ;
                 vardoc PICTURE  ;
                 '@!'
               ?? ' '
               @ ROW(), COL() GET  ;
                 varx3 VALID  ;
                 codalf2(SUBSTR(DTOC(varx3),  ;
                 1, 2) +  ;
                 SUBSTR(DTOC(varx3),  ;
                 4, 2) +  ;
                 SUBSTR(DTOC(varx3),  ;
                 7, 2))
               ?? '  '
               @ ROW(), COL() GET  ;
                 varx4 WHEN  ;
                 calcu1()
               SET CURSOR ON
               READ
               SET CURSOR OFF
          CASE key_aux == -3
               DO esc_modo WITH  ;
                  'E'
     ENDCASE
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'GRA',  ;
        'BBB', 'IGN', 'ESC'
     key_auy = LASTKEY()
     DO WHILE key_auy<>-1 .AND.  ;
        key_auy<>-9 .AND. key_auy<> ;
        27
          key_auy = INKEY(0)
     ENDDO
     IF key_auy <> 27 .AND.  ;
        key_auy <> -9
          dbrep = .T.
          regis = RECNO() - 1
          SELECT 2
          USE SHARED st_iseri  ;
              ORDER CODIGO
          SEEK STR(wk_codent, 9) +  ;
               wk_codmar +  ;
               DTOC(wk_fecing)
          SKIP regis
          IF  .NOT. FOUND() .OR.  ;
              STR(wk_codent, 9) +  ;
              wk_codmar +  ;
              DTOC(wk_fecing) <>  ;
              codent + codmar +  ;
              DTOC(fecing)
               APPEND BLANK
          ENDIF
          SELECT 1
          DO CASE
               CASE key_aux == -2
                    sw_sn = .T.
                    DO WHILE  ;
                       sw_sn
                         IF RLOCK()
                              REPLACE  ;
                               codent  ;
                               WITH  ;
                               STR(wk_codent,  ;
                               9),  ;
                               codmar  ;
                               WITH  ;
                               wk_codmar,  ;
                               fecing  ;
                               WITH  ;
                               wk_fecing
                              REPLACE  ;
                               numser  ;
                               WITH  ;
                               varx1,  ;
                               modelo  ;
                               WITH  ;
                               varx2,  ;
                               fecvta  ;
                               WITH  ;
                               varx3,  ;
                               fecgar  ;
                               WITH  ;
                               varx4,  ;
                               docgar  ;
                               WITH  ;
                               vardoc
                              IF RECNO() ==  ;
                                 RECCOUNT()
                                   APPEND BLANK
                              ENDIF
                              SELECT  ;
                               2
                              REPLACE  ;
                               codent  ;
                               WITH  ;
                               STR(wk_codent,  ;
                               9),  ;
                               codmar  ;
                               WITH  ;
                               wk_codmar,  ;
                               fecing  ;
                               WITH  ;
                               wk_fecing
                              REPLACE  ;
                               numser  ;
                               WITH  ;
                               varx1,  ;
                               modelo  ;
                               WITH  ;
                               varx2,  ;
                               fecvta  ;
                               WITH  ;
                               varx3,  ;
                               fecgar  ;
                               WITH  ;
                               varx4,  ;
                               docgar  ;
                               WITH  ;
                               vardoc
                              retval =  ;
                               2
                              EXIT
                         ELSE
                              sw_sn =  ;
                               f_yesno1( ;
                               'Registro Bloqueado, Reintentar' ;
                               )
                         ENDIF
                    ENDDO
               CASE key_aux == -3
                    sw_sn = .T.
                    DO WHILE  ;
                       sw_sn
                         IF RLOCK()
                              DELETE
                              PACK
                              SELECT  ;
                               2
                              DELETE
                              retval =  ;
                               2
                              EXIT
                         ELSE
                              sw_sn =  ;
                               f_yesno1( ;
                               'Registro Bloqueado, Reintentar' ;
                               )
                         ENDIF
                    ENDDO
          ENDCASE
          USE
          SELECT 1
     ENDIF
     RESTORE SCREEN FROM p0102x
     IF config_prg == 1
          DO esc_indica WITH 1,  ;
             'AYU', 'MD3', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'MBV', 'ELI', 'IGN',  ;
             'ESC'
     ELSE
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'IGN',  ;
             'ESC'
     ENDIF
     @ x1, x2 SAY ''
ENDIF
IF LASTKEY() == 27
     KEYBOARD '{ESC}' PLAIN
     retval = 0
ENDIF
IF LASTKEY() == -9
     retval = 0
ENDIF
RETURN (retval)
*
FUNCTION calcu1
varx4 = varx3 + (varx5 * 30)
RETURN .T.
*
PROCEDURE ayuda04
ON KEY LABEL F6
IF ROW() == 04
     USE SHARED st_iclpr ORDER  ;
         CODIGO
     SET FILTER TO indent == 'P'
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE PROVEEDORES'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
     USE
ENDIF
IF ROW() == 05
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'MARC'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE MARCAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF ROW() > 8 .AND. COL() > 29
     SELECT 4
     USE SHARED st_imode ORDER  ;
         CODIGO
     SET FILTER TO codmar == wk_codmar
     GOTO TOP
     campoa = 'codmod+" "+nommod'
     campob = 'nommod+" "+codmod'
     titulo = 'AYUDA DE MODELOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo, 'codmod'
     USE
     SELECT 1
ENDIF
ON KEY LABEL F6 do ayuda04
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

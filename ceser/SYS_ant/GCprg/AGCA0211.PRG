*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW prin FROM 8, 30 TO  ;
       16, 70 TITLE  ;
       'MINIMOS Y MAXIMOS' IN  ;
       screen
DEFINE WINDOW ven1 FROM 17, 30 TO  ;
       20, 70 IN screen
CLOSE DATABASES
SELECT 1
USE SHARED gc_pro00 ORDER  ;
    pro_proced
SELECT 2
USE SHARED gc_nve00 ORDER codigo
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 4
USE SHARED gc_cli00 ORDER codigo
SELECT 8
USE SHARED gc_imp00 ORDER  ;
    imp_tippro
ACTIVATE WINDOW ven1
@ 01, 2 SAY  ;
  '[F6] BUSCAR             [ESC] SALIR'  ;
  COLOR W+/N 
sigue = .T.
STORE SPACE(1) TO w_proce
w_prove = SPACE(11)
w_descri = SPACE(20)
DO WHILE sigue
     ACTIVATE WINDOW prin
     STORE SPACE(1) TO v1
     STORE 0 TO var1, var2, var3,  ;
           var4, var5, var6,  ;
           var7
     SET CURSOR ON
     @ 01, 02 SAY  ;
       'PROCEDENCIA  : '
     @ 01, 16 GET w_proce PICTURE  ;
       '@!' WHEN bus() COLOR N/W 
     @ 04, 02 SAY SPACE(28)
     READ
     IF LASTKEY() = 27
          sigue = .F.
          DEACTIVATE WINDOW ven1,  ;
                     prin
          LOOP
     ENDIF
     SELECT ge_tab0
     SEEK 'PROC' + w_proce
     IF  .NOT. FOUND()
          DO x_mensaje WITH  ;
             'C¢digo no Existe'
          DEACTIVATE WINDOW mensj
          LOOP
     ENDIF
     @ 03, 02 SAY  ;
       'PROVEEDOR    :'
     @ 03, 16 GET w_prove PICTURE  ;
       '@!' WHEN bus()
     READ
     IF LASTKEY() = 27
          LOOP
     ENDIF
     SELECT gc_cli00
     SEEK 'P' + w_prove
     IF  .NOT. FOUND()
          DO x_mensaje WITH  ;
             'C¢digo no Existe'
          DEACTIVATE WINDOW mensj
          @ 3, 2 SAY SPACE(30)
          @ 5, 02 SAY SPACE(30)
          LOOP
     ELSE
          w_descri = SUBSTR(cli_razsoc,  ;
                     1, 30)
          @ 05, 02 SAY w_descri  ;
            COLOR N/W 
     ENDIF
     IF (w_proce = 'I' .OR.  ;
        w_proce = 'N') .AND.  ;
        w_prove <> SPACE(11)
          rpt = f_yesno( ;
                'Desea procesar ? ' ;
                )
          IF rpt
               DO p_mensaje WITH  ;
                  'Procesando Stock M ximos, Cr¡ticos y M¡nimos...'
               SELECT gc_imp00
               SEEK w_proce +  ;
                    w_prove +  ;
                    'D'
               IF FOUND()
                    var1 = imp_nivcri
               ENDIF
               SEEK w_proce +  ;
                    w_prove +  ;
                    'C'
               IF FOUND()
                    var2 = imp_nivcri
               ENDIF
               SEEK w_proce +  ;
                    w_prove +  ;
                    'B'
               IF FOUND()
                    var3 = imp_nivcri
               ENDIF
               SEEK w_proce +  ;
                    w_prove +  ;
                    'A'
               IF FOUND()
                    var4 = imp_nivcri
               ENDIF
               SEEK w_proce +  ;
                    w_prove +  ;
                    'S'
               IF FOUND()
                    var5 = imp_nivcri
               ENDIF
               SEEK w_proce +  ;
                    w_prove +  ;
                    'E'
               IF FOUND()
                    var6 = imp_nivcri
               ENDIF
               SEEK w_proce +  ;
                    w_prove +  ;
                    'F'
               IF FOUND()
                    var7 = imp_nivcri
               ENDIF
               SELECT gc_pro00
               SEEK w_proce +  ;
                    w_prove
               IF FOUND()
                    SCAN WHILE  ;
                         pro_proced =  ;
                         w_proce  ;
                         .AND.  ;
                         pro_codpve =  ;
                         w_prove  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         STORE pro_codpro  ;
                               TO  ;
                               w_codigo
                         STORE ' '  ;
                               TO  ;
                               nivel
                         STORE 0  ;
                               TO  ;
                               fp6m,  ;
                               stkmax,  ;
                               stkmin
                         SELECT gc_nve00
                         SEEK w_codigo
                         IF FOUND()
                              fp6m =  ;
                               (nve_cmes1 +  ;
                               nve_cmes2 +  ;
                               nve_cmes3 +  ;
                               nve_cmes4 +  ;
                               nve_cmes5 +  ;
                               nve_cmes6) /  ;
                               6
                         ENDIF
                         DO CASE
                              CASE  ;
                               fp6m >=  ;
                               var2  ;
                               .AND.  ;
                               fp6m <  ;
                               var3
                                   STORE 'C' TO nivel
                              CASE  ;
                               fp6m >=  ;
                               var3  ;
                               .AND.  ;
                               fp6m <  ;
                               var4
                                   STORE 'B' TO nivel
                              CASE  ;
                               fp6m >=  ;
                               var4  ;
                               .AND.  ;
                               fp6m <  ;
                               var5
                                   STORE 'A' TO nivel
                              CASE  ;
                               fp6m >=  ;
                               var5
                                   STORE 'S' TO nivel
                         ENDCASE
                         SELECT gc_pro00
                         IF pro_nivcit <>  ;
                            'F'  ;
                            .AND.  ;
                            pro_nivcit <>  ;
                            'E'  ;
                            .AND.  ;
                            pro_nivcit <>  ;
                            'D'  ;
                            .AND.  ;
                            pro_nivcit <>  ;
                            'G'  ;
                            .AND.  ;
                            pro_nivcit <>  ;
                            'N'
                              DO rbloquea
                              REPLACE  ;
                               pro_nivcit  ;
                               WITH  ;
                               nivel
                              IF pro_rcom <>  ;
                                 'Z'  ;
                                 .AND.  ;
                                 pro_rcom <>  ;
                                 'Y'  ;
                                 .AND.  ;
                                 pro_rcom <>  ;
                                 'H'  ;
                                 .AND.  ;
                                 pro_nivcit <>  ;
                                 'F'  ;
                                 .AND.  ;
                                 pro_nivcit <>  ;
                                 'E'  ;
                                 .AND.  ;
                                 pro_nivcit <>  ;
                                 'D'  ;
                                 .AND.  ;
                                 pro_nivcit <>  ;
                                 'G'  ;
                                 .AND.  ;
                                 pro_nivcit <>  ;
                                 'N'
                                   REPLACE pro_rcom WITH pro_nivcit
                              ENDIF
                              UNLOCK
                         ENDIF
                         IF pro_rcom =  ;
                            'H'
                              v1 =  ;
                               pro_nivcit
                         ELSE
                              v1 =  ;
                               pro_rcom
                         ENDIF
                         IF pro_nivcit <>  ;
                            'F'
                              SELECT  ;
                               gc_imp00
                              SEEK  ;
                               w_proce +  ;
                               w_prove +  ;
                               v1
                              IF FOUND()
                                   x = imp_x
                                   y = imp_stomin
                                   z = imp_critic
                                   stkmax = ROUND(fp6m * x, 0)
                                   stkmin = ROUND(fp6m * y, 0)
                                   stkcri = ROUND(stkmax * z, 0)
                                   SELECT gc_pro00
                                   DO rbloquea
                                   REPLACE pro_stkmax WITH stkmax
                                   REPLACE pro_stkmin WITH stkmin
                                   REPLACE pro_critic WITH stkcri
                                   REPLACE pro_hora WITH TIME()
                                   REPLACE pro_fecha WITH DATE()
                                   REPLACE pro_usuari WITH clave
                                   UNLOCK
                              ENDIF
                         ENDIF
                         SELECT gc_pro00
                    ENDSCAN
                    DEACTIVATE WINDOW  ;
                               mensj
                    ?? CHR(07)
                    ?? CHR(07)
                    DEACTIVATE WINDOW  ;
                               prin
                    DEACTIVATE WINDOW  ;
                               ven1
               ENDIF
               DEACTIVATE WINDOW  ;
                          mensj
          ENDIF
     ENDIF
ENDDO
ON KEY LABEL f6
CLOSE DATABASES
ACTIVATE SCREEN
RETURN
*
PROCEDURE bus
IF VARREAD() = 'W_PROCE'
     ON KEY LABEL f6 do ayudar with 1
ENDIF
IF VARREAD() = 'W_PROVE'
     ON KEY LABEL f6 do ayudar with 2;
  
ENDIF
RETURN
*
FUNCTION f_yesno
PARAMETER mens, def
PRIVATE ALL
l = IIF(LEN(mens) < 18, 18,  ;
    LEN(mens))
sdef = IIF(def = .T.,  ;
       '\?\<Ok;\!\<Cancel',  ;
       '\?\<No;\!\<Si')
DEFINE WINDOW wyesno FROM 15, 35  ;
       TO 20, 65 DOUBLE COLOR  ;
       SCHEME 12
ACTIVATE WINDOW wyesno
@ 01, 8 SAY mens
@ 03, 2 GET vok DEFAULT 1 SIZE 1,  ;
  10, 4 PICTURE '@*HT ' + sdef
READ CYCLE
RELEASE WINDOW wyesno
RETURN IIF(def = .T., IIF(vok = 1,  ;
       .T., .F.), IIF(vok = 1,  ;
       .F., .T.))
*
PROCEDURE p_mensaje
PARAMETER mens
SET CURSOR OFF
l = LEN(mens)
DEFINE WINDOW mensj FROM 11, (80 -  ;
       l) / 2 - 4 TO 15, (80 + l) /  ;
       2 + 4 COLOR SCHEME 12
ACTIVATE WINDOW mensj
@ 1, (WCOLS('MENSJ') - l) / 2 SAY  ;
  mens
= INKEY(1.5 )
RETURN
*
PROCEDURE x_mensaje
PARAMETER mens
SET CURSOR OFF
l = LEN(mens)
DEFINE WINDOW mensj FROM 15, 35  ;
       TO 20, 65 COLOR SCHEME 12
ACTIVATE WINDOW mensj
@ 1, (WCOLS('MENSJ') - l) / 2 SAY  ;
  mens
= INKEY(1.5 )
RETURN
*
PROCEDURE ayudar
PARAMETER num
IF num = 1
     w_busca = 'PROC'
     w_var = 'w_proce'
     DO busca WITH w_busca, w_var
ELSE
     w_busca = 'P'
     w_var = 'w_prove'
     DO entidad WITH w_busca,  ;
        w_var
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

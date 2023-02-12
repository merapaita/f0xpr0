*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'INGRESO DE ARTICULOS POR MODELO'
STORE DATE() TO fecha1, fecha2
STORE 1 TO copia
STORE SPACE(4) TO marc
STORE 'Impresora' TO output
ON KEY LABEL F6 do ayuda01
STORE .T. TO pas
STORE SPACE(15) TO wk_codmod,  ;
      wrk_campo, wrk_selec,  ;
      wrk_selpro
STORE 'Detalle' TO opc
DO WHILE pas
     CLOSE DATABASES
     SELECT 2
     USE SHARED st_imode ALIAS  ;
         st_imode ORDER codigo
     SELECT 7
     USE SHARED ge_tab0 ALIAS  ;
         ge_tab0 ORDER codigo
     @ 07, 01 CLEAR TO 13, 77
     @ 3, 2 TO 10, 77
     DO esc_modo WITH 'S'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 03 SAY 'Marca      :'  ;
       GET marc VALID  ;
       valta('MARC',marc,30,30)  ;
       WHEN colocaf6()
     @ 05, 03 SAY 'Modelo     :'  ;
       GET wk_codmod PICTURE '@!'  ;
       VALID codalf(wk_codmod)  ;
       WHEN colocaf6()
     @ 06, 03 SAY 'Desde Fecha:'  ;
       GET fecha1
     @ 07, 03 SAY 'Hasta Fecha:'  ;
       GET fecha2 RANGE fecha1
     @ 08, 03 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'
     @ 09, 03 SAY  ;
       'Por Detalle/Res£men' GET  ;
       opc PICTURE  ;
       '@m Detalle,Res£men'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT DISTINCT  ;
            st_isrep.numdoc,  ;
            st_isrep.fecemi,  ;
            st_isrep.codemi,  ;
            st_isrep.codent,  ;
            st_isrep.indori,  ;
            st_isrep.indest,  ;
            st_isrep.codmar,  ;
            st_isrep.codmod,  ;
            st_isrep.numser,  ;
            st_isrep.monabo,  ;
            st_isrep.desace,  ;
            st_isrep.coddes,  ;
            st_iseri.modelo,  ;
            st_iseri.codmar,  ;
            st_iseri.numser,  ;
            st_iseri.fecvta,  ;
            st_iseri.docgar,  ;
            st_iorep.numdoc,  ;
            st_iorep.numsol,  ;
            st_iorep.observ FROM  ;
            ST_ISREP, ST_ISERI,  ;
            ST_IOREP WHERE  ;
            st_isrep.codmar +  ;
            st_isrep.codmod +  ;
            st_isrep.numser =  ;
            st_iseri.codmar +  ;
            st_iseri.modelo +  ;
            st_iseri.numser AND  ;
            st_isrep.numdoc =  ;
            st_iorep.numsol AND  ;
            st_isrep.fecemi >=  ;
            fecha1 AND  ;
            st_isrep.fecemi <=  ;
            fecha2 AND  ;
            st_isrep.codmar =  ;
            marc AND  ;
            st_isrep.codmod =  ;
            wk_codmod ORDER BY  ;
            st_isrep.indori,  ;
            st_isrep.fecemi,  ;
            st_iseri.fecvta,  ;
            st_isrep.numdoc INTO  ;
            CURSOR ING
     COPY TO ing
     CLOSE DATABASES
     SET DELETED OFF
     USE SHARED ing
     INDEX ON numdoc_a +  ;
           DTOS(fecvta) TO  ;
           idxmod
     GOTO TOP
     DO WHILE  .NOT. EOF()
          STORE numdoc_a TO nss
          DO WHILE numdoc_a=nss
               DELETE
               SKIP
               IF nss <> numdoc_a
                    SKIP -1
                    RECALL
                    SKIP
               ENDIF
          ENDDO
     ENDDO
     SET DELETED ON
     INDEX ON indori +  ;
           STR(MONTH(fecemi)) +  ;
           STR(MONTH(fecvta)) +  ;
           DTOS(fecvta) +  ;
           numdoc_a TO IDXMOD
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          IF opc = 'Detalle'
               REPORT FORMAT  ;
                      PORL4095 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      PORL4095  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          IF opc = 'Detalle'
               REPORT FORMAT  ;
                      PORL4095 TO  ;
                      FILE  ;
                      TEXT1.TXT  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      PORL4095  ;
                      SUMMARY TO  ;
                      FILE  ;
                      TEXT1.TXT  ;
                      NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          MODIFY FILE TEXT1.TXT  ;
                 NOEDIT WINDOW  ;
                 pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SET FILTER TO tab_codpre == 'MARC'
titulo = 'AYUDA DE MARCAS'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
RETURN
*
FUNCTION valta
PARAMETER clave, codig, colu,  ;
          largo
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .F.
ENDIF
SELECT ge_tab0
codaux = clave + codig
SEEK '&codaux'
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** Codigo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
ENDIF
DO sacaf6
RETURN .T.
*
PROCEDURE ayuda01
IF ROW() == 4
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'MARC'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE MARCAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
ENDIF
IF ROW() == 5
     wrk_select = SELECT()
     SELECT st_imode
     SET FILTER TO codmar == marc
     wrk_selpro = SELECT()
     wrk_campo = st_imode.codmar
     DO pro2 WITH wrk_campo,  ;
        wrk_select, wrk_selpro,  ;
        1
     IF LASTKEY() <> 27
          wk_codmod = wrk_campo
          KEYBOARD wk_codmod
     ENDIF
     SELECT (wrk_select)
     SELECT 2
ENDIF
*
PROCEDURE pro2
PARAMETER wrk_campo, wrk_selec,  ;
          wrk_selpro, wrk_nropro
wrk_order = ORDER()
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
IF wrk_nropro = 1
     DEFINE WINDOW produ FROM 12,  ;
            18 TO 20, 73 IN  ;
            screen COLOR SCHEME  ;
            8
ELSE
     DEFINE WINDOW produ FROM 12,  ;
            18 TO 25, 73 IN  ;
            screen COLOR SCHEME  ;
            8
ENDIF
DEFINE POPUP prod FROM 16, 31
DEFINE BAR 1 OF prod PROMPT  ;
       '\<Codigo '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripcion '
ON SELECTION POPUP prod do buspro2 with;
bar(), wrk_selpro, wrk_campo
IF wrk_nropro = 1
     DEFINE POPUP produ FROM 15,  ;
            18 TO 20, 73 PROMPT  ;
            FIELDS pro_codpro +  ;
            '³' +  ;
            SUBSTR(pro_descri, 1,  ;
            20) + '³' +  ;
            SUBSTR(pro_modelo, 1,  ;
            20) IN screen
ELSE
     DEFINE POPUP produ FROM 15,  ;
            18 TO 25, 73 PROMPT  ;
            FIELDS pro_codpro +  ;
            '³' +  ;
            SUBSTR(pro_descri, 1,  ;
            20) + '³' +  ;
            SUBSTR(pro_modelo, 1,  ;
            20) IN screen
ENDIF
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
IF LASTKEY() <> 27
     wrk_campo = st_imode.codmod
ENDIF
SELECT (wrk_selec)
ON KEY LABEL f6 do ayuda01
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

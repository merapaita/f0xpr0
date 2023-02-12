*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'INGRESO DE ORDENES POR MODELO'
STORE DATE() TO fecha1, fecha2
STORE 1 TO copia
STORE SPACE(4) TO marc
STORE 'Impresora' TO output
ON KEY LABEL F6 do ayuda01
STORE .T. TO pas
STORE SPACE(15) TO wk_codm1,  ;
      wk_codm2, wrk_campo,  ;
      wrk_selec, wrk_selpro
STORE 'Detalle' TO tipo
DO WHILE pas
     CLOSE DATABASES
     SELECT 2
     USE SHARED st_imode ALIAS  ;
         st_imode ORDER codigo
     SELECT 3
     USE SHARED st_iorep ALIAS  ;
         st_iorep ORDER  ;
         ord_MAMOSE
     SET RELATION TO codmar + codmod INTO;
st_imode
     SELECT 7
     USE SHARED ge_tab0 ALIAS  ;
         ge_tab0 ORDER codigo
     @ 07, 01 CLEAR TO 13, 77
     @ 03, 02 TO 10, 77
     DO esc_modo WITH 'S'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 03 SAY 'Marca      :'  ;
       GET marc VALID valta(1, ;
       marc) WHEN colocaf6()
     @ 05, 03 SAY 'Modelo     :'  ;
       GET wk_codm1 PICTURE '@!'  ;
       VALID valta(2,wk_codm1)  ;
       WHEN colocaf6()
     @ 05, 40 SAY 'Modelo     :'  ;
       GET wk_codm2 RANGE  ;
       wk_codm1 PICTURE '@!'  ;
       VALID valta(2,wk_codm2)  ;
       WHEN colocaf6()
     @ 06, 03 SAY 'Desde Fecha:'  ;
       GET fecha1 WHEN sacaf6()
     @ 07, 03 SAY 'Hasta Fecha:'  ;
       GET fecha2 RANGE fecha1
     @ 08, 03 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'
     @ 09, 03 SAY  ;
       'Por Detalle/Res£men   :'  ;
       GET tipo PICTURE  ;
       '@m Detalle,Res£men'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_iorep
     SET NEAR ON
     SEEK marc + wk_codm1
     SET NEAR OFF
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          IF tipo = 'Detalle'
               REPORT FORMAT  ;
                      PORl4093 TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR (fecemi >=  ;
                      fecha1  ;
                      .AND.  ;
                      fecemi <=  ;
                      fecha2)  ;
                      .AND.  ;
                      indest <>  ;
                      'A' WHILE  ;
                      (codmar =  ;
                      marc .AND.  ;
                      (codmod >=  ;
                      wk_codm1  ;
                      .AND.  ;
                      codmod <=  ;
                      wk_codm2))
          ELSE
               REPORT FORMAT  ;
                      PORl4093  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR (fecemi >=  ;
                      fecha1  ;
                      .AND.  ;
                      fecemi <=  ;
                      fecha2)  ;
                      .AND.  ;
                      indest <>  ;
                      'A' WHILE  ;
                      (codmar =  ;
                      marc .AND.  ;
                      (codmod >=  ;
                      wk_codm1  ;
                      .AND.  ;
                      codmod <=  ;
                      wk_codm2))
          ENDIF
          SET PRINTER TO
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          wrk_fil2 = SUBSTR(f_texto(),  ;
                     1, 8) +  ;
                     '.TXT'
          IF tipo = 'Detalle'
               REPO FORM PORl4093 TO FILE;
&wrk_fil2 while (codmar=marc and (codmod>=wk_codm1;
and codmod<=wk_codm2)) for (fecemi>=fecha1;
and fecemi<=fecha2) and indest#'A' NOCONSOLE
          ELSE
               REPO FORM PORl4093 TO FILE;
&wrk_fil2 while (codmar=marc and (codmod>=wk_codm1;
and codmod<=wk_codm2)) for (fecemi>=fecha1;
and fecemi<=fecha2) and indest#'A' NOCONSOLE;
SUMMARY
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODI COMM &wrk_fil2 NOEDIT WINDOW;
PANTALL
          DELE FILE &wrk_fil2
          SET SYSMENU OFF
     ENDIF
ENDDO
DO sacawin
CLOSE DATABASES
ON KEY LABEL f6
RETURN
*
FUNCTION valta
PARAMETER clave, codig
IF clave = 1
     IF LASTKEY() = 5 .OR.  ;
        LASTKEY() = 19
          RETURN .F.
     ENDIF
ENDIF
IF clave = 1
     SELECT ge_tab0
     codaux = 'MARC' + codig
ELSE
     SELECT st_imode
     codaux = marc + codig
ENDIF
seek '&codaux'
IF  .NOT. FOUND()
     DO error WITH  ;
        '** C¢digo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF clave = 1
     @ ROW(), 25 SAY  ;
       SUBSTR(tab_destab, 1, 30)
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
     SET FILTER TO
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
ENDIF
RETURN
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
       '\<C¢digo '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripci¢n '
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
FUNCTION f_texto
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'TEMPO' +  ;
           LTRIM(STR(cor))
     IF FILE(ret + '.TXT')
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
*** 
*** ReFox - retrace your steps ... 
***

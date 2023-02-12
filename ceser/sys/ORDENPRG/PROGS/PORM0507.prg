*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
IF config_prg == 1
     tit_prg = ' MANTENCION '
ELSE
     tit_prg = ' CONSULTA '
ENDIF
CLOSE DATABASES
SELECT 1
USE PASSWORD ORDER USUARIO
SELECT 2
USE GE_TAB0 ORDER CODIGO
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' MAESTRO DE USUARIOS '
ppas = .T.
DO WHILE ppas
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     ON KEY LABEL F6 DO AYUDA WITH VARREAD()
     STORE SPACE(04) TO  ;
           wrk_nivel1, wrk_nivel2,  ;
           wrk_nivel3,  ;
           wrk_codare
     STORE SPACE(10) TO  ;
           wrk_codusu, wrk_clave,  ;
           wrk_campo
     STORE SPACE(15) TO wrk_area
     STORE SPACE(30) TO  ;
           wrk_nombre
     STORE DATE() + 30 TO  ;
           wrk_fecfin
     STORE 0 TO wrk_salir
     wrk_select = SELECT()
     wrk_selpro = SELECT()
     SET CURSOR ON
     @ 07, 02 CLEAR TO 13, 72
     @ 04, 02 TO 06, 72
     @ 05, 05 SAY  ;
       'C¢digo de Usuario     :'
     @ 05, 30 GET wrk_codusu  ;
       PICTURE '@!' VALID  .NOT.  ;
       EMPTY(wrk_codusu) WHEN  ;
       colocaf6()
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     @ 06, 02 TO 13, 72
     @ 06, 02 SAY 'Ã'
     @ 06, 72 SAY '´'
     @ 07, 05 SAY  ;
       'Nombre del Usuario ...:'
     @ 08, 05 SAY  ;
       'Area de Trabajo ......:'
     @ 09, 05 SAY  ;
       'Nivel O/Reparaciones .:'
     @ 10, 05 SAY  ;
       'Nivel Inventarios ....:'
     @ 11, 05 SAY  ;
       'Fecha Fin de Uso .....:'
     @ 12, 05 SAY  ;
       'Password .............:'
     SELECT password
     SET ORDER TO USUARIO
     SEEK wrk_codusu
     IF FOUND()
          DO esc_modo WITH 'C'
          DO esc_indica WITH 1,  ;
             'AYU', 'MOD', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          wrk_nombre = nombre
          wrk_area = area
          wrk_nivel1 = nivelo
          wrk_nivel2 = nivel
          wrk_nivel3 = nivel_prg
          wrk_fecfin = fecfin
          @ 07, 30 SAY wrk_nombre
          @ 08, 30 SAY wrk_area
          @ 09, 30 SAY wrk_nivel1
          @ 10, 30 SAY wrk_nivel2
          @ 11, 30 SAY wrk_fecfin
          @ 12, 30 SAY wrk_clave
          DO WHILE LASTKEY()<>27  ;
             .AND. LASTKEY()<>-2
               wrk_inkey = INKEY(0,  ;
                           'H')
               IF wrk_inkey = 27
                    wrk_salir = 1
                    EXIT
               ENDIF
               IF wrk_inkey = -2
                    EXIT
               ENDIF
          ENDDO
     ENDIF
     IF wrk_salir = 1
          LOOP
     ENDIF
     DO esc_modo WITH 'M'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 07, 30 GET wrk_nombre  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(wrk_nombre)  ;
       WHEN oowhen(VARREAD())
     @ 08, 30 GET wrk_area  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(wrk_area) WHEN  ;
       oowhen(VARREAD())
     @ 09, 30 GET wrk_nivel1  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) .AND.  ;
       colocaf6()
     @ 10, 30 GET wrk_nivel2  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       colocaf6()
     @ 11, 30 GET wrk_fecfin  ;
       PICTURE '@D' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(wrk_fecfin)  ;
       WHEN oowhen(VARREAD())
     @ 12, 30 GET wrk_clave  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(wrk_clave)  ;
       WHEN oowhen(VARREAD())  ;
       COLOR W/W,W/W 
     READ
     IF LASTKEY() = 27
          LOOP
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'GRA', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO WHILE .T.
          wrk_inkey = INKEY(0,  ;
                      'H')
          IF wrk_inkey = -1
               DO oograba
               EXIT
          ENDIF
          IF wrk_inkey = 27
               EXIT
          ENDIF
     ENDDO
ENDDO
CLOSE DATABASES
DO sacawin
ON KEY
RETURN
*
PROCEDURE oowhen
PARAMETER opc3
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
DO CASE
     CASE opc3 = 'WRK_NOMBRE'
          ON KEY
     CASE opc3 = 'WRK_NIVEL1'
          ON KEY LABEL F6 DO AYUDA WITH;
VARREAD()
     CASE opc3 = 'WRK_FECFIN'
          ON KEY
ENDCASE
*
FUNCTION oovalid
PARAMETER wrk_opc
IF wrk_opc = 'WRK_NOMBRE'
     IF LASTKEY() = 5 .OR.  ;
        LASTKEY() = 19
          RETURN .F.
     ENDIF
ENDIF
SELECT ge_tab0
IF wrk_opc = 'WRK_NIVEL1'
     SEEK 'NIOR' + wrk_nivel1
     IF  .NOT. FOUND()
          DO error WITH  ;
             '*** C¢digo no existe ***'
          RETURN .F.
     ENDIF
     @ 09, 35 SAY tab_destab
ENDIF
IF wrk_opc = 'WRK_NIVEL2'
     SEEK 'NIGC' + wrk_nivel2
     IF  .NOT. FOUND()
          DO error WITH  ;
             '*** C¢digo no existe ***'
          RETURN .F.
     ENDIF
     @ 10, 35 SAY tab_destab
ENDIF
RETURN
*
PROCEDURE oograba
SELECT password
SEEK wrk_codusu
IF  .NOT. FOUND()
     DO rbloquea
     APPEND BLANK
ENDIF
DO mensa WITH  ;
   '*** G r a b a n d o ... ***',  ;
   'COLO'
REPLACE codigo WITH wrk_codusu
REPLACE nombre WITH wrk_nombre
REPLACE area WITH wrk_area
REPLACE nivelo WITH wrk_nivel1
REPLACE nivel WITH wrk_nivel2
REPLACE fecfin WITH wrk_fecfin
REPLACE abrevid WITH  ;
        ooclave(wrk_clave)
REPLACE usuario WITH users
REPLACE fecha WITH DATE()
REPLACE hora WITH TIME()
UNLOCK
DO mensa WITH  ;
   '*** G r a b a n d o ... ***',  ;
   'SACA'
RETURN
*
PROCEDURE ayuda
PARAMETER opc
IF opc = 'WRK_CODUSU'
     wrk_select = SELECT()
     SELECT password
     wrk_selpro = SELECT()
     wrk_campo = password.codigo
     DO usuario WITH wrk_campo,  ;
        wrk_select, wrk_selpro
     SELECT (wrk_select)
ENDIF
IF opc = 'WRK_NIVEL1'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'NIOR'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'NIVELES DE O/R'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF opc = 'WRK_NIVEL2'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'NIGC'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'NIVELES DE INVENTARIOS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
RETURN
*
PROCEDURE usuario
PARAMETER wrk_campo, wrk_select,  ;
          wrk_selpro
ON KEY
wrk_order = ORDER()
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen
DEFINE WINDOW produ FROM 12, 18  ;
       TO 19, 73 IN screen
DEFINE POPUP prod FROM 16, 31
DEFINE BAR 1 OF prod PROMPT  ;
       '\<C¢digo '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripci¢n '
ON SELECTION POPUP prod do bususu with;
bar(),wrk_selpro
DEFINE POPUP produ FROM 15, 18 TO  ;
       19, 73 PROMPT FIELDS  ;
       codigo + '³' + nombre IN  ;
       screen
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
ON KEY LABEL f6 do usuario with wrk_campo,wrk_select,wrk_selpro
IF LASTKEY() <> 27
     wrk_campo = password.codigo
     ON KEY
ENDIF
SELECT (wrk_select)
RETURN
*
PROCEDURE bususu
PARAMETER bar, wrk_selpro
ON KEY
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, -1
ENDFOR
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY -1, 0
ENDFOR
ACTIVATE WINDOW pide
SELECT (wrk_selpro)
IF bar = 1
     wrk_codusu = SPACE(10)
     SET ORDER TO USUARIO
     @ 00, 00 SAY 'C¢digo :'
     @ 00, 09 GET wrk_codusu  ;
       PICTURE '@!'
ENDIF
IF bar = 2
     wrk_codusu = SPACE(30)
     SET ORDER TO NOMBRE
     @ 00, 00 SAY 'Descr. :'
     @ 00, 09 GET wrk_codusu  ;
       PICTURE '@!'
ENDIF
READ
IF LASTKEY() <> 27
     SET NEAR ON
     SEEK wrk_codusu
     SET NEAR OFF
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do tomacod4
     BROWSE FIELDS codigo :R :H =  ;
            'C¢digo', nombre :R :  ;
            30 :H = 'Nombre'  ;
            FREEZE codigo IN  ;
            produ
ENDIF
ON KEY
wrk_codusu = SPACE(10)
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
RETURN
*
PROCEDURE tomacod4
ON KEY
wrk_codusu = password.codigo
KEYBOARD '{ENTER}'
DEACTIVATE WINDOW pide, produ
DEACTIVATE POPUP prod
RETURN
*
FUNCTION ooclave
PARAMETER wrk_cla
wrk_total = LEN(ALLTRIM(wrk_cla))
b = wrk_total
wrk_clafin = ' '
FOR c = 1 TO wrk_total
     wrk_clafin = wrk_clafin +  ;
                  SUBSTR(wrk_cla,  ;
                  b, 1)
     b = b - 1
ENDFOR
RETURN ALLTRIM(wrk_clafin)
*
*** 
*** ReFox - retrace your steps ... 
***

*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
CLOSE ALL
CLEAR ALL
worker = IIF(LTRIM(UPPER(GETENV('WORKER'))) =  ;
         'ON', .T., .F.)
SET PROCEDURE TO Sai_lib
DO fox_ambi
PUBLIC vcoddep, vnumdep, vconex,  ;
       vmaq, vllave, vusurec, fmv,  ;
       vsistema, sistema,  ;
       sistctrl
vsistema = '1'
vcoddep = '070100'
vnumdep = 6
DO lcolores
IF selper()
ELSE
     RETURN
ENDIF
rutapr = '..\DATAPR'
desa = '    CPresupuestal'
IF worker
     IF yesno( ;
        '¨ Datos de Prueba ?')
          desa = ' CPresu (PRUEBA)'
          SET PATH TO &RUTAPR
     ENDIF
ENDIF
vconex = ALLTRIM(LEFT(SYS(0), 8))
vmaq = ALLTRIM(RIGHT(SYS(0), 2))
vllave = ALLTRIM(SYS(2))
vuser_id = SPACE(5)
IF  .NOT. FILE(SET('PATH') +  ;
    '\USUARIO.CDX')
     DO standby WITH  ;
        ' Error en INDEXA '
ENDIF
PUBLIC vcoddep
sistema = '4'
sistctrl = sistema
vconex = ALLTRIM(LEFT(SYS(0), 8))
vmaq = ALLTRIM(RIGHT(SYS(0), 2))
vllave = ALLTRIM(SYS(2))
USE IN 0 USUARIO ALIAS usua ORDER  ;
    USUARIO1
IF  .NOT. clave()
     CLOSE DATABASES
     DO standby WITH  ;
        '!! Acceso Denegado !!',  ;
        10, 15
     IF worker
          CANCEL
     ENDIF
     QUIT
ENDIF
spac1 = LEN(ALLTRIM(usua.nombre))
desuse = SUBSTR(usua.nombre, 1,  ;
         spac1)
vuser_id = ALLTRIM(usuario)
vcoddep = coddep
vnumdep = numdep
vflag = flag
vnombre = ALLTRIM(nombre)
USE
USE IteUsu ALIAS iteu ORDER  ;
    IteUsu1
SELECT iteu
SET FILTER TO sistema = '4'
vusucla = CHRTRAN(vuser_id, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~')
vacceso = ''
SEEK vusucla
SCAN WHILE vusucla =  ;
     ALLTRIM(iteu.usucla)
     vacceso = vacceso +  ;
               iteu.modulo
ENDSCAN
SET FILTER TO
IF vuser_id $ 'VICTOR'
     fmv = .T.
ENDIF
USE
USE parmae ALIAS parma ORDER  ;
    parmae1
SEEK 'EMPRES'
IF  .NOT. EOF()
     cia = ALLTRIM(descri)
     detaw = ALLTRIM(descriaux)
     direw = ALLTRIM(descriau2)
     telew = codigoaux
     rucw = nument
ELSE
ENDIF
USE
subc = ' '
titu = ' Sistema de Contabilidad Presupuestal ' +  ;
       '(' +  ;
       ALLTRIM(SUBSTR(SYS(0), 1,  ;
       10)) + ')'
vsistema = '1'
= savcon(1)
escolor = ISCOLOR()
IF escolor
     SET COLOR TO, N/W, B/N
     c_panta = 8
     c_borde = 10
     c_popup = 4
     c_texto = 'N/W'
     c_fondo = 1
ELSE
     c_panta = 8
     c_borde = 7
     c_popup = 3
     c_texto = 'N+/W'
     c_fondo = 1
ENDIF
SET MESSAGE TO 23 CENTER
@ 1, 0, 23, 79 BOX '°°°°°°°°°'
@ 1, 1, 23, 24 BOX '±±±±±±±±±'
spac = (80 - (LEN(cia) +  ;
       LEN(titu) + 10)) / 2
rotulo1 = cia + SPACE(spac) +  ;
          titu + SPACE(spac) +  ;
          SPACE(11)
spac = (79 - (LEN(desa) +  ;
       LEN(desuse) + 10)) / 2
rotulo2 = desa + SPACE(spac) +  ;
          '® ' + desuse + ' ¯' +  ;
          SPACE(spac) +  ;
          DTOC(DATE())
DO logos WITH rotulo1, rotulo2,  ;
   50
SAVE SCREEN TO principal
IF worker
     ON ERROR
ELSE
     ON ERROR DO FOX_ERRS WITH PROGRAM()
ENDIF
SAVE SCREEN TO pantalla
SET MESSAGE TO 23 CENTER
SET CLOCK ON
STORE .T. TO ven_sistem
STORE .F. TO esc_tecla
= SYS(2002, 1)
l_col = '3/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B '
IF escolor
     DEFINE POPUP menu FROM 5,2 TO 15,21;
SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP menu FROM 5, 2  ;
            TO 15, 21 COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF menu PROMPT  ;
       '\<Afectaci¢n      '  ;
       MESSAGE ''
DEFINE BAR 2 OF menu PROMPT  ;
       '\<Documentos     '  ;
       MESSAGE ''
DEFINE BAR 3 OF menu PROMPT  ;
       '\<Consultas      '  ;
       MESSAGE ''
DEFINE BAR 4 OF menu PROMPT  ;
       '\<Procesos       '  ;
       MESSAGE ''
DEFINE BAR 5 OF menu PROMPT  ;
       '\<Tablas         '  ;
       MESSAGE ''
DEFINE BAR 6 OF menu PROMPT  ;
       '\<Utilitarios    '  ;
       MESSAGE ''
DEFINE BAR 7 OF menu PROMPT  ;
       '\<Salida          '
IF escolor
     DEFINE POPUP pop_07 FROM  6,26 SHADOW;
COLOR &l_col
ELSE
     DEFINE POPUP pop_07 FROM 6,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_07 PROMPT  ;
       '\<Registro H/Control    '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_07 PROMPT  ;
       '\- '
DEFINE BAR 3 OF pop_07 PROMPT  ;
       'Registro \<H/Modificaci¢n'
IF escolor
     DEFINE POPUP pop_01 FROM  6,26 SHADOW;
COLOR &l_col
ELSE
     DEFINE POPUP pop_01 FROM 6,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_01 PROMPT  ;
       'Orden de \<Compra     '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_01 PROMPT  ;
       'Orden de ser\<Vicio   '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_01 PROMPT  ;
       'Comprobante de Pa\<Go '  ;
       MESSAGE ''
DEFINE BAR 4 OF pop_01 PROMPT  ;
       '\<Relaci¢n de H/C     '  ;
       MESSAGE ''
IF escolor
     DEFINE POPUP pop_02 FROM  7,26 SHADOW;
COLOR &l_col
ELSE
     DEFINE POPUP pop_02 FROM 7,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02 PROMPT  ;
       '\<Saldo presupuestal       '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_02 PROMPT  ;
       'Saldo \<Calendario         '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_02 PROMPT  ;
       'Calendario Vs.\<Ejecuci¢n '  ;
       MESSAGE ''
IF escolor
     DEFINE POPUP pop_03 FROM  8,26 SHADOW;
COLOR &l_col
ELSE
     DEFINE POPUP pop_03 FROM 8,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_03 PROMPT  ;
       '\<Estado ejecuci¢n mensual E-5 '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_03 PROMPT  ;
       '\<Listado E-5 (Inversi¢n)      '  ;
       MESSAGE ''
IF escolor
     DEFINE POPUP pop_04 FROM 09,26 SHADOW;
COLOR &l_col
ELSE
     DEFINE POPUP pop_04 FROM 09,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_04 PROMPT  ;
       '\<Tipo de Documento'  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_04 PROMPT  ;
       '\<Auxiliares       '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_04 PROMPT  ;
       '\<Correlativo H/C  '  ;
       MESSAGE ''
IF escolor
     DEFINE POPUP pop_05 FROM 10,26 SHADOW;
COLOR &l_col
ELSE
     DEFINE POPUP pop_05 FROM 10,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_05 PROMPT  ;
       '\<Cuadro necesidades'  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_05 PROMPT  ;
       '\<Pe.co.sa.         '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_05 PROMPT  ;
       '\<Solicitud Servicio'  ;
       MESSAGE ''
IF escolor
     DEFINE POPUP pop_06 FROM 11,26 MARGIN;
 SHADOW COLOR &l_col
ELSE
     DEFINE POPUP pop_06 FROM 11,  ;
            26 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_06 PROMPT  ;
       '\<Indexar archivos             '
DEFINE BAR 2 OF pop_06 PROMPT  ;
       '\<Backup de los archivos       '
DEFINE BAR 3 OF pop_06 PROMPT  ;
       'De\<puraci¢n de backups        '
DEFINE BAR 4 OF pop_06 PROMPT  ;
       '\-'
DEFINE BAR 5 OF pop_06 PROMPT  ;
       '\<Seguridad         '  ;
       MESSAGE  ;
       '®® Acceso RESTRINGIDO ¯¯'
DEFINE BAR 6 OF pop_06 PROMPT  ;
       '\<Seguridad Opciones'  ;
       MESSAGE  ;
       '®® Acceso RESTRINGIDO ¯¯'
DEFINE BAR 7 OF pop_06 PROMPT  ;
       '\<Usuarios CPRESU   '
IF escolor
     DEFINE POPUP pop_02e FROM 11,56 SHADOW;
COLOR &l_col
ELSE
     DEFINE POPUP pop_02e FROM 11,  ;
            56 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02e PROMPT  ;
       '\<Saldos calendario '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_02e PROMPT  ;
       '\<Saldos c. Anal¡t. '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_02e PROMPT  ;
       '\<Auxliar Standard  '  ;
       MESSAGE ''
DEFINE BAR 4 OF pop_02e PROMPT  ;
       'saldos asi\<Gnaci¢n '  ;
       MESSAGE ''
DEFINE BAR 5 OF pop_02e PROMPT  ;
       '\<Compromisos del mes'  ;
       MESSAGE ''
DEFINE BAR 6 OF pop_02e PROMPT  ;
       '\<Compromisos x Componente'  ;
       MESSAGE ''
IF escolor
     DEFINE POPUP pop_02f FROM 11,56 SHADOW;
COLOR &l_col
ELSE
     DEFINE POPUP pop_02f FROM 11,  ;
            56 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02f PROMPT  ;
       '\<Acumulados  ' MESSAGE  ;
       ''
DEFINE BAR 2 OF pop_02f PROMPT  ;
       '\<Mensualizado' MESSAGE  ;
       ''
ON SELECTION POPUP menu DO ACT_MENUS
IF  .NOT. vflag $ 'J*'
     DO abrepas
ENDIF
SAVE SCREEN TO principal
ACTIVATE POPUP menu
DO WHILE EMPTY(PROMPT())
     ACTIVATE POPUP menu
ENDDO
ON KEY
RETURN
*
PROCEDURE act_menus
DO CASE
     CASE BAR() = 1
          ON SELECTION POPUP pop_07 DO;
 menu_07
          ACTIVATE POPUP pop_07
     CASE BAR() = 2
          ON SELECTION POPUP pop_01 DO;
 menu_01
          ACTIVATE POPUP pop_01
     CASE BAR() = 3
          ON SELECTION POPUP pop_02 DO;
 menu_02
          ACTIVATE POPUP pop_02
     CASE BAR() = 4
          ON SELECTION POPUP pop_03 DO;
 menu_03
          ACTIVATE POPUP pop_03
     CASE BAR() = 5
          ON SELECTION POPUP pop_04 DO;
 menu_04
          ACTIVATE POPUP pop_04
     CASE BAR() = 6
          ON SELECTION POPUP pop_06 DO;
 menu_06
          ACTIVATE POPUP pop_06
     CASE BAR() = 7
          DO salmenu
ENDCASE
RESTORE SCREEN FROM principal
RETURN
*
PROCEDURE menu_01
choice = BAR()
DO CASE
     CASE choice = 1
          DO con_ocom WITH '',  ;
             '1'
     CASE choice = 2
          DO con_oser WITH '',  ;
             '1'
     CASE choice = 3
          DO concpau2
     CASE choice = 4
          DO conhc
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_01 DO menu_01
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_01, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_07
choice = BAR()
DO CASE
     CASE choice = 1
          DO reghc4 WITH 1, '1'
     CASE choice = 3
          DO hojmod1 WITH '1'
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_07 DO menu_07
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_07, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_07a
choice = BAR()
DO CASE
     CASE choice = 1
          DO reghc WITH 1, '1'
     CASE choice = 2
          DO regehc WITH 1, '2'
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_07 DO menu_07
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_07, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02
choice = BAR()
DO CASE
     CASE choice = 1
          ON SELECTION POPUP pop_02f DO;
 menu_02f
          ACTIVATE POPUP pop_02f
     CASE choice = 2
          ON SELECTION POPUP pop_02e DO;
 menu_02e
          ACTIVATE POPUP pop_02e
     CASE choice = 3
          DO ejecal
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02 DO menu_02
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_03
choice = BAR()
DO CASE
     CASE choice = 1
          DO rege51
     CASE choice = 2
          DO rege51ie
ENDCASE
RESTORE SCREEN FROM principal
SHOW POPUP pop_03, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_04
choice = BAR()
DO CASE
     CASE choice = 1
          DO tipdoc
     CASE choice = 2
          DO manaux2
     CASE choice = 3
          DO manpar WITH 'HOJCON',  ;
             'Correlativo Hoja de Control',  ;
             'Correlaivos Hojas de Control',  ;
             4,  ;
             'Valor siguiente'
ENDCASE
RESTORE SCREEN FROM principal
SHOW POPUP pop_04, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_05
choice = BAR()
DO CASE
     CASE choice = 1
     CASE choice = 2
     CASE choice = 3
          DO regsol
ENDCASE
RESTORE SCREEN FROM principal
SHOW POPUP pop_05, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_06
choice = BAR()
DO CASE
     CASE choice = 1
          = poperror( ;
            'Aseg£rese que no est‚n usando el sistema, para poder ' +  ;
            'realizar el proceso de reordenamiento sin ning£n problema' ;
            )
          DO indexcp
     CASE choice = 2
          = poperror( ;
            'Aseg£rese que no est‚n usando el sistema, para poder ' +  ;
            'realizar la copia de seguridad sin ning£n problema' ;
            )
          DO backup
     CASE choice = 3
          DO resbk
     CASE choice = 5
          DO segcp WITH vuser_id
     CASE choice = 6
          DO segopc WITH vuser_id
     CASE choice = 6
          DO contusu
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_06 DO menu_06
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_06, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02e
choice = BAR()
DO CASE
     CASE choice = 1
          DO consal1
     CASE choice = 2
          DO consal2
     CASE choice = 3
          DO salaxs
     CASE choice = 4
          DO sasina
     CASE choice = 5
          DO concomp
     CASE choice = 6
          DO concomp2
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02e DO menu_02e
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02e, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02f
choice = BAR()
DO CASE
     CASE choice = 1
          DO conpre
     CASE choice = 2
          DO conpre1
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02f DO menu_02f
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02f, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
DEACTIVATE WINDOW ALL
DEACTIVATE MENU
RETURN
*
PROCEDURE salmenu
STORE yesno( ;
      '¨ Estamos terminando la sesi¢n ?' ;
      ) TO in_secoems
IF in_secoems
     SET COLOR TO
     CLOSE DATABASES
     HIDE POPUP ALL
     = savcon(2)
     IF worker
          CLEAR
          SET HELP ON
          ON KEY
          CANCEL
     ELSE
          RESTORE SCREEN FROM  ;
                  principal
          DO clrscr
     ENDIF
     QUIT
ELSE
     RETURN
ENDIF
*
PROCEDURE abrepas
SET SKIP OF BAR 1 OF pop_07 ;
.NOT. 'A' $ vacceso
SET SKIP OF BAR 3 OF pop_07 ;
.NOT. 'B' $ vacceso
SET SKIP OF BAR 1 OF pop_01 ;
.NOT. 'C' $ vacceso
SET SKIP OF BAR 2 OF pop_01 ;
.NOT. 'D' $ vacceso
SET SKIP OF BAR 3 OF pop_01 ;
.NOT. 'E' $ vacceso
SET SKIP OF BAR 4 OF pop_01 ;
.NOT. 'F' $ vacceso
SET SKIP OF BAR 1 OF pop_02 ;
.NOT. 'G' $ vacceso
SET SKIP OF BAR 2 OF pop_02 ;
.NOT. 'H' $ vacceso
SET SKIP OF BAR 3 OF pop_02 ;
.NOT. 'I' $ vacceso
SET SKIP OF BAR 1 OF pop_03 ;
.NOT. 'J' $ vacceso
SET SKIP OF BAR 2 OF pop_03 ;
.NOT. 'K' $ vacceso
SET SKIP OF BAR 1 OF pop_04 ;
.NOT. 'L' $ vacceso
SET SKIP OF BAR 2 OF pop_04 ;
.NOT. 'M' $ vacceso
SET SKIP OF BAR 3 OF pop_04 ;
.NOT. 'N' $ vacceso
SET SKIP OF BAR 1 OF pop_06 ;
.NOT. 'O' $ vacceso
SET SKIP OF BAR 2 OF pop_06 ;
.NOT. 'P' $ vacceso
SET SKIP OF BAR 3 OF pop_06 ;
.NOT. 'Q' $ vacceso
SET SKIP OF BAR 5 OF pop_06 ;
.NOT. 'R' $ vacceso
SET SKIP OF BAR 6 OF pop_06 ;
.NOT. 'S' $ vacceso
vopcion = .T.
FOR i = 1 TO 3
     IF  .NOT. SKPBAR('pop_07',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 1 OF menu vopcion
vopcion = .T.
FOR i = 1 TO 4
     IF  .NOT. SKPBAR('pop_01',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 2 OF menu vopcion
vopcion = .T.
FOR i = 1 TO 3
     IF  .NOT. SKPBAR('pop_02',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 3 OF menu vopcion
vopcion = .T.
FOR i = 1 TO 2
     IF  .NOT. SKPBAR('pop_03',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 4 OF menu vopcion
vopcion = .T.
FOR i = 1 TO 3
     IF  .NOT. SKPBAR('pop_04',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 5 OF menu vopcion
vopcion = .T.
FOR i = 1 TO 6
     IF  .NOT. SKPBAR('pop_06',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 6 OF menu vopcion
RETURN
*
PROCEDURE lcolores
SET COLOR OF SCHEME 1 TO W/N, N+/W, W+/N,;
W+/N, W/N, B+/N, W+/N, -, W+/N, W/N
SET COLOR OF SCHEME 10 TO G/N, GR+/B,;
G/N, G/N, GR+/N, W+/BG, BG+/B, -, W+/N,;
W/N
SET COLOR OF SCHEME 21 TO W/N, N+/W, W+/N,;
W+/N, W/N, B+/N, W+/N, -, W+/N, W/N
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

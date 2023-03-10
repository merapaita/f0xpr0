SET EXCLUSIVE OFF
SET ESCAPE OFF
CLEAR
CLOSE ALL
CLEAR ALL
SET TALK OFF
SET ECHO OFF
worker = IIF(LTRIM(UPPER(GETENV('WORKER'))) =  ;
         'ON', .T., .F.)
SET PROCEDURE TO libsis
DO fox_ambi
DO colores
SET CENTURY ON
PUBLIC vcoddep, vnumdep, sistem
vnumdep = 6
sistem = '1'
*ruta = '..\Data'
*ruta = '..\..\..\sisdata\comven\Data'
ruta = 'h:\comven\Data'
rutapr = '..\Datapr'
rutabk = '..\BackUp'
SET PATH TO &ruta
IF worker
     IF yesno( ;
        '? Datos de Prueba ?')
          SET PATH TO &RutaPr
     ENDIF
ENDIF
vuser_id = ALLTRIM(LEFT(SYS(0),  ;
           15))
PUBLIC vcoddep, vconex, vmaq,  ;
       vllav, vusurec, vflag
vconex = ALLTRIM(LEFT(SYS(0), 8))
vmaq = ALLTRIM(RIGHT(SYS(0), 2))
vllav = ALLTRIM(SYS(2))
= savcon()
USE Usuario ALIAS usua ORDER  ;
    Usuario1
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
vnombre = 'Analista: Marco E. Rodriguez Abad.'
cia = ' S.B.P. '
mruc = '20147082861'
subc = '    Compras y Ventas    '
desa = SET('PATH')
titu = ' Registro de Compras y Ventas' +  ;
       '(' +  ;
       ALLTRIM(SUBSTR(SYS(0), 1,  ;
       10)) + ')'
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
@ 1, 0, 23, 79 BOX '?????????'
@ 1, 1, 23, 30 BOX '?????????'
spac = (80 - (LEN(cia) +  ;
       LEN(titu) + 10)) / 2
rotulo1 = cia + SPACE(spac) +  ;
          titu + SPACE(spac) +  ;
          SPACE(11)
usuarios = '?Hecho: ' + vnombre +  ;
           ' ?'
fecha = DTOC(DATE()) + SPACE(2)
rotulo2 = PADR(desa, 17, ' ') +  ;
          PADC(usuarios, 46, ' ') +  ;
          PADL(fecha, 17, ' ')
DO logos WITH rotulo1, rotulo2,  ;
   50
SAVE SCREEN TO principal
IF worker
     ON ERROR
ELSE
     ON ERROR DO FOX_ERRS WITH PROGRAM()
ENDIF
SAVE SCREEN TO pantalla
SET CLOCK ON
STORE .T. TO ven_sistem
STORE .F. TO esc_tecla
= SYS(2002, 1)
l_col = 'W+/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B '
IF escolor
	DEFINE POPUP menu FROM 5,2 TO 18,27 SHADOW COLOR &L_COL
ELSE
    DEFINE POPUP menu FROM 5, 2 TO 18, 27 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF menu PROMPT ' \<Compras            ' MESSAGE ''
DEFINE BAR 2 OF menu PROMPT ' \<Ventas             ' MESSAGE ''
DEFINE BAR 3 OF menu PROMPT ' \<Proceso Estructuras' MESSAGE ''
DEFINE BAR 4 OF menu PROMPT ' \<Catalogo Proveedores' MESSAGE ''
DEFINE BAR 5 OF menu PROMPT ' \<Util. y Param.     '  MESSAGE ''
DEFINE BAR 6 OF menu PROMPT ' \<S a l i d a        '
IF escolor
    DEFINE POPUP pop_03 FROM 13,32 MARGIN SHADOW COLOR &l_col
ELSE
    DEFINE POPUP pop_03 FROM 13,32 MARGIN COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_03 PROMPT '\<Indexar archivos           '
DEFINE BAR 2 OF pop_03 PROMPT '\<Backup de los archivos     '
DEFINE BAR 3 OF pop_03 PROMPT 'De\<puraci?n de backups      '
DEFINE BAR 4 OF pop_03 PROMPT 'Manten. de Correlativos      '
ON SELECTION POPUP menu DO ACT_MENUS
SAVE SCREEN TO principal
ACTIVATE POPUP menu
DO WHILE EMPTY(PROMPT())
     ACTIVATE POPUP menu
ENDDO
ON KEY
RETURN
*
PROCEDURE act_menus
choice = BAR()
DO CASE
    CASE choice = 1
        DO regcom
    CASE choice = 2
        DO regven
    CASE choice = 3
        DO creafil
    CASE choice = 4
        DO maeprv
    CASE choice = 5
        ON SELECTION POPUP pop_03 DO menu_03
        ACTIVATE POPUP pop_03
    CASE choice = 6
        DO salmenu
ENDCASE
IF  .NOT. escolor
    RESTORE SCREEN FROM principal
     ON SELECTION POPUP menu DO Act_Menus
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02
choice = BAR()
DO CASE
     CASE choice = 1
     CASE choice = 2
     CASE choice = 3
     CASE choice = 5
     CASE choice = 6
     CASE choice = 7
     CASE choice = 8
     CASE choice = 9
     CASE choice = 10
ENDCASE
RESTORE SCREEN FROM principal
SHOW POPUP pop_02, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_03
choice = BAR()
DO CASE
     CASE choice = 1
          = poperror( ;
            'Aseg?rese que no est?n usando el sistema, para poder ' +  ;
            'realizar el proceso de reordenamiento sin ning?n problema' ;
            )
     CASE choice = 2
          = poperror( ;
            'Aseg?rese que no est?n usando el sistema, para poder ' +  ;
            'realizar la copia de seguridad sin ning?n problema' ;
            )
          DO backup
     CASE choice = 3
          DO depbk
     CASE choice = 4
          DO ctrlacc WITH  ;
             vuser_id
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_03 DO menu_03
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_03, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE salmenu
STORE yesno( ;
      '? Estamos terminando la sesi?n ?' ;
      ) TO in_secoems
IF in_secoems
     SET COLOR TO
     CLOSE DATABASES
     HIDE POPUP ALL
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
SET SKIP OF BAR 1 OF menu ;
.NOT. 'A' $ vacceso
SET SKIP OF BAR 2 OF menu ;
.NOT. 'B' $ vacceso
SET SKIP OF BAR 3 OF menu ;
.NOT. 'C' $ vacceso
SET SKIP OF BAR 4 OF menu ;
.NOT. 'D' $ vacceso
SET SKIP OF BAR 1 OF pop_02 ;
.NOT. 'E' $ vacceso
SET SKIP OF BAR 2 OF pop_02 ;
.NOT. 'F' $ vacceso
SET SKIP OF BAR 3 OF pop_02 ;
.NOT. 'G' $ vacceso
SET SKIP OF BAR 4 OF pop_02 ;
.NOT. 'H' $ vacceso
SET SKIP OF BAR 1 OF pop_03 ;
.NOT. 'I' $ vacceso
SET SKIP OF BAR 1 OF pop_04 ;
.NOT. 'L' $ vacceso
SET SKIP OF BAR 2 OF pop_04 ;
.NOT. 'M' $ vacceso
SET SKIP OF BAR 3 OF pop_04 ;
.NOT. 'N' $ vacceso
SET SKIP OF BAR 4 OF pop_04 ;
.NOT. 'O' $ vacceso
SET SKIP OF BAR 5 OF pop_04 ;
.NOT. 'P' $ vacceso
SET SKIP OF BAR 6 OF pop_04 ;
.NOT. 'Q' $ vacceso
SET SKIP OF BAR 7 OF pop_04 ;
.NOT. 'Y' $ vacceso
SET SKIP OF BAR 8 OF pop_04 ;
.NOT. 'Z' $ vacceso
SET SKIP OF BAR 1 OF pop_05 ;
.NOT. 'R' $ vacceso
SET SKIP OF BAR 2 OF pop_05 ;
.NOT. 'S' $ vacceso
SET SKIP OF BAR 3 OF pop_05 ;
.NOT. 'T' $ vacceso
SET SKIP OF BAR 1 OF pop_01 ;
.NOT. 'U' $ vacceso
SET SKIP OF BAR 2 OF pop_01 ;
.NOT. 'V' $ vacceso
SET SKIP OF BAR 3 OF pop_01 ;
.NOT. 'W' $ vacceso
SET SKIP OF BAR 5 OF pop_01 ;
.NOT. 'X' $ vacceso
SET SKIP OF BAR 6 OF pop_01 ;
.NOT. '5' $ vacceso
SET SKIP OF BAR 1 OF pop_06 ;
.NOT. 'a' $ vacceso
SET SKIP OF BAR 2 OF pop_06 ;
.NOT. 'b' $ vacceso
SET SKIP OF BAR 3 OF pop_06 ;
.NOT. 'c' $ vacceso
SET SKIP OF BAR 5 OF pop_06 ;
.NOT. 'd' $ vacceso
SET SKIP OF BAR 6 OF pop_06 ;
.NOT. 'e' $ vacceso
SET SKIP OF BAR 7 OF pop_06 ;
.NOT. 'f' $ vacceso
vopcion = .T.
FOR i = 1 TO 4
     IF  .NOT. SKPBAR('pop_02',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 5 OF menu vopcion
vopcion = .T.
FOR i = 1 TO 4
     IF  .NOT. SKPBAR('pop_02',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 6 OF menu vopcion
vopcion = .T.
FOR i = 1 TO 8
     IF  .NOT. SKPBAR('pop_04',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 7 OF menu vopcion
vopcion = .T.
FOR i = 1 TO 3
     IF  .NOT. SKPBAR('pop_05',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 8 OF menu vopcion
vopcion = .T.
FOR i = 1 TO 6
     IF  .NOT. SKPBAR('pop_01',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 9 OF menu vopcion
vopcion = .T.
FOR i = 1 TO 7
     IF  .NOT. SKPBAR('pop_06',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 10 OF menu vopcion
RETURN
*
PROCEDURE colores
SET COLOR OF SCHEME 1 TO W/N, N+/W, W+/N,;
W+/N, W/N, B+/N, W+/N, -, W+/N, W/N
SET COLOR OF SCHEME 8 TO G/N, GR+/B, G/N,;
G/N, GR+/N, W+/BG, BG+/B, -, W+/N, W/N
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

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
IF selper()
ELSE
     RETURN
ENDIF
rutapr = '..\DataPr'
IF worker
     IF yesno( ;
        '¨ Datos de Prueba ?')
          SET PATH TO &rutapr
     ENDIF
ENDIF
vuser_id = ALLTRIM(LEFT(SYS(0),  ;
           15))
IF  .NOT. FILE('USUARIO.CDX')
     DO standby WITH  ;
        ' Error en INDEXA '
ENDIF
PUBLIC vconex, vmaq, vllav,  ;
       vsistema, vusuario,  ;
       vusucla, vusurec
vconex = ALLTRIM(LEFT(SYS(0), 8))
vmaq = ALLTRIM(RIGHT(SYS(0), 2))
vllav = ALLTRIM(SYS(2))
USE Parmae ALIAS parma ORDER  ;
    Parmae1
SEEK 'SISTEM01'
vsistema = ALLTRIM(parma.codigo)
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
vusuario = ALLTRIM(usuario)
vnombre = ALLTRIM(nombre)
vflag = flag
USE
USE iteusu ALIAS iteu ORDER  ;
    iteusu1
SELECT iteu
SET FILTER TO sistema = '1'
vusucla = CHRTRAN(vusuario, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
vacceso = ' '
SEEK vusucla
SCAN WHILE vusucla =  ;
     ALLTRIM(iteu.usucla)
     vacceso = vacceso +  ;
               iteu.modulo
ENDSCAN
SET FILTER TO
USE
= savcon(1)
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
DO lcolores
SET MESSAGE TO 23 CENTER
@ 1, 0, 23, 79 BOX '°°°°°°°°°'
@ 1, 1, 23, 30 BOX '±±±±±±±±±'
IF SET('Path') = rutapr
     desa = ' P R U E B A '
ELSE
     desa = ' Presu'
ENDIF
titu = ' Sistema de Planificaci¢n y Presupuesto ' +  ;
       vconex
spac = (80 - (LEN(cia) +  ;
       LEN(titu) + 10)) / 2
rotulo1 = cia + SPACE(spac) +  ;
          titu + SPACE(spac) +  ;
          SPACE(11)
usuarios = '® USER: ' + vnombre +  ;
           ' ¯'
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
     DEFINE POPUP menu FROM 5,2 TO 15,30;
SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP menu FROM 5, 2  ;
            TO 15, 30 COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF menu PROMPT  ;
       ' asignaci¢n \<Presupuestal '  ;
       MESSAGE ''
DEFINE BAR 2 OF menu PROMPT  ;
       ' asignaci¢n \<Calendarios  '  ;
       MESSAGE ''
DEFINE BAR 3 OF menu PROMPT  ;
       ' \<Modific. presupuestales '  ;
       MESSAGE ''
DEFINE BAR 4 OF menu PROMPT  ;
       ' \<Ejecuci¢n               '  ;
       MESSAGE ''
DEFINE BAR 5 OF menu PROMPT  ;
       ' cat logos y ta\<Blas      '  ;
       MESSAGE ''
DEFINE BAR 6 OF menu PROMPT  ;
       ' \<Utilitarios             '  ;
       MESSAGE ''
DEFINE BAR 7 OF menu PROMPT  ;
       ' co\<Nsultas               '  ;
       MESSAGE ''
DEFINE BAR 8 OF menu PROMPT  ;
       ' \<Salida                   '
IF escolor
     DEFINE POPUP pop_07a FROM 05,32 MARGIN;
 SHADOW COLOR &l_col
ELSE
     DEFINE POPUP pop_07a FROM 05,  ;
            32 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_07a PROMPT  ;
       ' \<Presupuesto Egresos '
DEFINE BAR 2 OF pop_07a PROMPT  ;
       ' \<Presupuesto Ingresos'
IF escolor
     DEFINE POPUP pop_07 FROM 05,32 MARGIN;
 SHADOW COLOR &l_col
ELSE
     DEFINE POPUP pop_07 FROM 05,  ;
            32 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_07 PROMPT  ;
       ' \<A n u a l  '
DEFINE BAR 2 OF pop_07 PROMPT  ;
       ' \<Trimestral - mensual'
IF escolor
     DEFINE POPUP pop_08 FROM 05,32 MARGIN;
 SHADOW COLOR &l_col
ELSE
     DEFINE POPUP pop_08 FROM 05,  ;
            32 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_08 PROMPT  ;
       ' \<Transferencias de partidas     '
DEFINE BAR 2 OF pop_08 PROMPT  ;
       ' \<Cr‚ditos suplementarios        '
IF escolor
     DEFINE POPUP pop_09 FROM 05,32 MARGIN;
 SHADOW COLOR &l_col
ELSE
     DEFINE POPUP pop_09 FROM 05,  ;
            32 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_09 PROMPT  ;
       ' \<Asigna calendarios       '
DEFINE BAR 2 OF pop_09 PROMPT  ;
       ' \<Trabaja con calendarios  '
IF escolor
     DEFINE POPUP pop_02 FROM 09,32 MARGIN;
 SHADOW COLOR &l_col
ELSE
     DEFINE POPUP pop_02 FROM 09,  ;
            32 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_02 PROMPT  ;
       ' E-\<5 '
DEFINE BAR 2 OF pop_02 PROMPT  ;
       ' E-5 \<E '
DEFINE BAR 3 OF pop_02 PROMPT  ;
       ' \<Asigna al Mes ==> 1 '
DEFINE BAR 4 OF pop_02 PROMPT  ;
       'Actualiza Preupuesto '
IF escolor
     DEFINE POPUP pop_03 FROM 10,32 MARGIN;
 SHADOW COLOR &l_col
ELSE
     DEFINE POPUP pop_03 FROM 10,  ;
            32 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_03 PROMPT  ;
       ' EV-\<1 '
IF escolor
     DEFINE POPUP pop_04 FROM 08,32;
 SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP pop_04 FROM 08,  ;
            32 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_04 PROMPT  ;
       ' \<Unidad gestora '
DEFINE BAR 2 OF pop_04 PROMPT  ;
       ' unidad \<Ejecutora'
DEFINE BAR 3 OF pop_04 PROMPT  ;
       ' \<Funci¢n        '
DEFINE BAR 4 OF pop_04 PROMPT  ;
       ' \<Programas      '
DEFINE BAR 5 OF pop_04 PROMPT  ;
       ' \<Subprogramas   '
DEFINE BAR 6 OF pop_04 PROMPT  ;
       ' \<Actividad/proyecto   '
DEFINE BAR 7 OF pop_04 PROMPT  ;
       ' \<Componente     '
DEFINE BAR 8 OF pop_04 PROMPT  ;
       ' ca\<Dena Funcional'
DEFINE BAR 9 OF pop_04 PROMPT  ;
       ' c\<Lasificador         '
DEFINE BAR 10 OF pop_04 PROMPT  ;
       ' \<Ingresos       '
DEFINE BAR 11 OF pop_04 PROMPT  ;
       ' f\<Uentes        '
DEFINE BAR 12 OF pop_04 PROMPT  ;
       ' \<Registro Clasificador    '
DEFINE BAR 13 OF pop_04 PROMPT  ;
       ' Asi\<Gnaciones presupuestales'
IF escolor
     DEFINE POPUP pop_04a FROM 17,58 SHADOW;
COLOR &l_col
ELSE
     DEFINE POPUP pop_04a FROM 17,  ;
            58 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_04a PROMPT  ;
       ' \<Categ.del gasto '
DEFINE BAR 2 OF pop_04a PROMPT  ;
       ' \<Grupo gen‚rico  '
DEFINE BAR 3 OF pop_04a PROMPT  ;
       ' \<Mod. de aplicac.'
DEFINE BAR 4 OF pop_04a PROMPT  ;
       ' \<Espec. del gasto'
IF escolor
     DEFINE POPUP pop_05 FROM 12,32 SHADOW;
COLOR &l_col
ELSE
     DEFINE POPUP pop_05 FROM 12,  ;
            32 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_05 PROMPT  ;
       '\<Cuadro necesidades   '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_05 PROMPT  ;
       '\<Pe.co.sa             '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_05 PROMPT  ;
       '\<Solicitud servicios  '  ;
       MESSAGE ''
IF escolor
     DEFINE POPUP pop_01 FROM 13,32 MARGIN;
 SHADOW COLOR &l_col
ELSE
     DEFINE POPUP pop_01 FROM 13,  ;
            32 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_01 PROMPT  ;
       '\<Indexar archivos           '
DEFINE BAR 2 OF pop_01 PROMPT  ;
       '\<Backup de los archivos     '
DEFINE BAR 3 OF pop_01 PROMPT  ;
       'De\<puraci¢n de backups      '
DEFINE BAR 4 OF pop_01 PROMPT  ;
       '\-'
DEFINE BAR 5 OF pop_01 PROMPT  ;
       '\<Seguridad         '  ;
       MESSAGE  ;
       '®® Acceso RESTRINGIDO ¯¯'
DEFINE BAR 6 OF pop_01 PROMPT  ;
       '\<Usuarios PRESU    '
IF escolor
     DEFINE POPUP pop_06 FROM 14,32 MARGIN;
 SHADOW COLOR &l_col
ELSE
     DEFINE POPUP pop_06 FROM 14,  ;
            32 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_06 PROMPT  ;
       'saldo \<Calendario mensual'
DEFINE BAR 2 OF pop_06 PROMPT  ;
       'saldo \<Presupuestal      '
DEFINE BAR 3 OF pop_06 PROMPT  ;
       '\<Marco Calendario  '
DEFINE BAR 4 OF pop_06 PROMPT  ;
       'marco \<Ejecuci¢n   '
DEFINE BAR 5 OF pop_06 PROMPT  ;
       'ejecuci¢n \<+ calendario'
DEFINE BAR 6 OF pop_06 PROMPT  ;
       '\<Listado E-5  '
DEFINE BAR 7 OF pop_06 PROMPT  ;
       'Listado \<E-5 (Proyectos)'
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
choice = BAR()
DO CASE
     CASE choice = 1
          ON SELECTION POPUP pop_07a DO;
 menu_07a
          ACTIVATE POPUP pop_07a
     CASE choice = 2
          ON SELECTION POPUP pop_09 DO;
 menu_09
          ACTIVATE POPUP pop_09
     CASE choice = 3
          ON SELECTION POPUP pop_08 DO;
 menu_08
          ACTIVATE POPUP pop_08
     CASE choice = 4
          ON SELECTION POPUP pop_02 DO;
 menu_02
          ACTIVATE POPUP pop_02
     CASE choice = 5
          ON SELECTION POPUP pop_04 DO;
 menu_04
          ACTIVATE POPUP pop_04
     CASE choice = 6
          ON SELECTION POPUP pop_01 DO;
 menu_01
          ACTIVATE POPUP pop_01
     CASE choice = 7
          ON SELECTION POPUP pop_06 DO;
 menu_06
          ACTIVATE POPUP pop_06
     CASE choice = 8
          DO salmenu
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_01 DO menu_01
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_07
choice = BAR()
DO CASE
     CASE choice = 1
          DO regprea
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
          DO regprea
     CASE choice = 2
          DO regpreai
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_07a DO menu_07a
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_07a, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_08
choice = BAR()
DO CASE
     CASE choice = 1
          DO transpa
     CASE choice = 2
          DO cresu
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_08 DO menu_08
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_08, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_09
choice = BAR()
DO CASE
     CASE choice = 1
          DO asical
     CASE choice = 2
          DO regcal
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_09 DO menu_09
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_09, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02
choice = BAR()
DO CASE
     CASE choice = 1
          DO rege51
     CASE choice = 2
          DO rege51ie
     CASE choice = 3
          DO asiuno
     CASE choice = 4
          DO actpre
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
PROCEDURE menu_04
choice = BAR()
DO CASE
     CASE choice = 1
          DO manpar1 WITH  ;
             'UNIDADES GESTORAS',  ;
             'UNIGES',  ;
             'Unid. Gestora', '',  ;
             'Descripci¢n'
     CASE choice = 2
          DO manpar1 WITH  ;
             'UNIDADES EJECUTORA',  ;
             'UNIEJE',  ;
             'Unid. Gestora',  ;
             'Uni. Ejec.',  ;
             'Descripci¢n'
     CASE choice = 3
          DO manpar1 WITH  ;
             'FUNCIONES',  ;
             'CODFUN', 'Funcion',  ;
             '', 'Descripci¢n'
     CASE choice = 4
          DO manpar1 WITH  ;
             'PROGRAMAS',  ;
             'CODPRG', 'Funci¢n',  ;
             'Programa',  ;
             'Descripci¢n'
     CASE choice = 5
          DO manpar1 WITH  ;
             'SUB PROGRAMAS',  ;
             'CODSPR', 'Programa',  ;
             'Sub Programa',  ;
             'Descripci¢n'
     CASE choice = 6
          DO manpar1 WITH  ;
             '1 ACTIVIDADES 2 PROYECTOS',  ;
             'ACTPRY',  ;
             'Act./Proy.', '',  ;
             'Descripci¢n'
     CASE choice = 7
          DO manpar1 WITH  ;
             'COMPONENTES',  ;
             'CODCOM',  ;
             'Componente', '',  ;
             'Descripci¢n'
     CASE choice = 8
          DO cadfun WITH '1'
     CASE choice = 9
          ON SELECTION POPUP pop_04a DO;
 menu_04a
          ACTIVATE POPUP pop_04a
     CASE choice = 10
          DO ingreso
     CASE choice = 11
          DO manpar1 WITH  ;
             'FUENTES DE FINANCIAMIENTO',  ;
             'CODFTE',  ;
             'Fte. Financiamiento',  ;
             '', 'Descripci¢n'
     CASE choice = 12
          DO catasien
     CASE choice = 13
          DO catasi
ENDCASE
RESTORE SCREEN FROM principal
SHOW POPUP pop_04, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_04a
choice = BAR()
DO CASE
     CASE choice = 1
          DO manpar1 WITH  ;
             'CATEGORIAS DEL GASTO',  ;
             'CATGAS',  ;
             'Categ. del Gasto',  ;
             '', 'Descripci¢n'
     CASE choice = 2
          DO manpar1 WITH  ;
             'GRUPO GENERICO',  ;
             'GRUGEN',  ;
             'Grupo Generico', '',  ;
             'Descripci¢n'
     CASE choice = 3
          DO manpar1 WITH  ;
             'MODALIDAD DE APLICACIàN',  ;
             'MODAPL',  ;
             'Mod. de Aplicaci¢n',  ;
             '', 'Descripci¢n'
     CASE choice = 4
          DO manpar1 WITH  ;
             'ESPECÖFICA DEL GASTO',  ;
             'ESPGAS',  ;
             'Espec. del Gasto',  ;
             '', 'Descripci¢n'
ENDCASE
RESTORE SCREEN FROM principal
SHOW POPUP pop_04a, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_05
choice = BAR()
DO CASE
     CASE choice = 1
          DO regcdr
     CASE choice = 2
          DO regpec WITH 2, ' '
     CASE choice = 3
          DO regsol
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_05 DO menu_05
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_05, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_01
choice = BAR()
DO CASE
     CASE choice = 1
          DO indexpre
     CASE choice = 2
          = poperror( ;
            'Aseg£rese que no est‚n usando el sistema, para poder ' +  ;
            'realizar la copia de seguridad sin ning£n problema' ;
            )
          DO backup
     CASE choice = 3
          DO depbk
     CASE choice = 5
          DO segpre WITH vusuario
          RESTORE SCREEN FROM  ;
                  principal
     CASE choice = 6
          DO contusu
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
PROCEDURE menu_06
choice = BAR()
DO CASE
     CASE choice = 1
          DO consal
     CASE choice = 2
          DO conpre
     CASE choice = 3
          DO marcal
     CASE choice = 4
          DO mareje
     CASE choice = 5
          DO ejecal
     CASE choice = 6
          DO lise51
     CASE choice = 7
          DO lise51ie
ENDCASE
RESTORE SCREEN FROM principal
SHOW POPUP pop_06, menu
HIDE WINDOW ALL
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
PROCEDURE salmenu2
IF yesno( ;
   ' Estamos terminando la sesi¢n ' ;
   )
     SET COLOR TO
     = elicon()
     CLOSE DATABASES
     HIDE POPUP ALL
     RESTORE SCREEN FROM  ;
             principal
     DO clrscr1
     SET HELP ON
     DEACTIVATE WINDOW ALL
     IF worker
          CANCEL
     ENDIF
     QUIT
ENDIF
RETURN
*
FUNCTION salta
PARAMETER cbar
lborra = .F.
IF  .NOT. USED('IteU')
     USE IN 0 IteUsu ALIAS iteu  ;
         ORDER IteUsu1
     lborra = .T.
ENDIF
SELECT iteu
mret =  .NOT. SEEK(vusuario +  ;
        vsistema + cbar)
IF FOUND()
     cclamod = ALLTRIM(clamod)
ENDIF
IF lborra
     USE
ENDIF
RETURN mret
*
PROCEDURE abrepas
SET SKIP OF BAR 1 OF pop_07a ;
.NOT. 'A' $ vacceso
SET SKIP OF BAR 2 OF pop_07a ;
.NOT. 'B' $ vacceso
vopcion = .T.
FOR i = 1 TO 2
     IF  .NOT. SKPBAR('pop_07a',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 1 OF menu vopcion
SET SKIP OF BAR 1 OF pop_09 ;
.NOT. 'C' $ vacceso
SET SKIP OF BAR 2 OF pop_09 ;
.NOT. 'D' $ vacceso
vopcion = .T.
FOR i = 1 TO 2
     IF  .NOT. SKPBAR('pop_09',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 2 OF menu vopcion
SET SKIP OF BAR 1 OF pop_08 ;
.NOT. 'E' $ vacceso
SET SKIP OF BAR 2 OF pop_08 ;
.NOT. 'F' $ vacceso
vopcion = .T.
FOR i = 1 TO 2
     IF  .NOT. SKPBAR('pop_08',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 3 OF menu vopcion
SET SKIP OF BAR 1 OF pop_02 ;
.NOT. 'G' $ vacceso
SET SKIP OF BAR 2 OF pop_02 ;
.NOT. 'H' $ vacceso
SET SKIP OF BAR 3 OF pop_02 ;
.NOT. 'I' $ vacceso
SET SKIP OF BAR 4 OF pop_02 ;
.NOT. 'J' $ vacceso
vopcion = .T.
FOR i = 1 TO 4
     IF  .NOT. SKPBAR('pop_02',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 4 OF menu vopcion
SET SKIP OF BAR 1 OF pop_04 ;
.NOT. 'K' $ vacceso
SET SKIP OF BAR 2 OF pop_04 ;
.NOT. 'L' $ vacceso
SET SKIP OF BAR 3 OF pop_04 ;
.NOT. 'M' $ vacceso
SET SKIP OF BAR 4 OF pop_04 ;
.NOT. 'N' $ vacceso
SET SKIP OF BAR 5 OF pop_04 ;
.NOT. 'O' $ vacceso
SET SKIP OF BAR 6 OF pop_04 ;
.NOT. 'P' $ vacceso
SET SKIP OF BAR 7 OF pop_04 ;
.NOT. 'Q' $ vacceso
SET SKIP OF BAR 8 OF pop_04 ;
.NOT. 'R' $ vacceso
SET SKIP OF BAR 9 OF pop_04 ;
.NOT. 'S' $ vacceso
SET SKIP OF BAR 10 OF pop_04 ;
.NOT. 'T' $ vacceso
SET SKIP OF BAR 11 OF pop_04 ;
.NOT. 'U' $ vacceso
SET SKIP OF BAR 12 OF pop_04 ;
.NOT. 'V' $ vacceso
vopcion = .T.
FOR i = 1 TO 12
     IF  .NOT. SKPBAR('pop_04',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 5 OF menu vopcion
SET SKIP OF BAR 1 OF pop_01 ;
.NOT. 'W' $ vacceso
SET SKIP OF BAR 2 OF pop_01 ;
.NOT. 'X' $ vacceso
SET SKIP OF BAR 3 OF pop_01 ;
.NOT. 'Y' $ vacceso
SET SKIP OF BAR 5 OF pop_01 ;
.NOT. 'Z' $ vacceso
SET SKIP OF BAR 6 OF pop_01 ;
.NOT. '1' $ vacceso
vopcion = .T.
FOR i = 1 TO 6
     IF  .NOT. SKPBAR('pop_01',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 6 OF menu vopcion
SET SKIP OF BAR 1 OF pop_06 ;
.NOT. '2' $ vacceso
SET SKIP OF BAR 2 OF pop_06 ;
.NOT. '3' $ vacceso
SET SKIP OF BAR 3 OF pop_06 ;
.NOT. '4' $ vacceso
SET SKIP OF BAR 4 OF pop_06 ;
.NOT. '5' $ vacceso
SET SKIP OF BAR 5 OF pop_06 ;
.NOT. '6' $ vacceso
SET SKIP OF BAR 6 OF pop_06 ;
.NOT. '7' $ vacceso
SET SKIP OF BAR 7 OF pop_06 ;
.NOT. '8' $ vacceso
vopcion = .T.
FOR i = 1 TO 7
     IF  .NOT. SKPBAR('pop_06',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 7 OF menu vopcion
RETURN
*
PROCEDURE xabrepas
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
RETURN
*
PROCEDURE lcolores
SET COLOR OF SCHEME 1 TO W/N, N+/W, W+/N,;
W+/N, W/N, B+/N, W+/N, -, W+/N, W/N
SET COLOR OF SCHEME 5 TO W/N, N+/W, W+/N,;
W+/N, W/N, B+/N, W+/N, -, W+/N, W/N
SET COLOR OF SCHEME 10 TO W/N, N+/W, W+/N,;
W+/N, W/N, B+/N, W+/N, -, W+/N, W/N
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***

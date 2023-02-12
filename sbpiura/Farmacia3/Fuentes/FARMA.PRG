****************************
* Farma.Prg
* Sistema de Farmacia
* A.S. Marco Rodriguez Abad
****************************

SET EXCLUSIVE OFF
SET ESCAPE OFF
CLEAR
CLOSE ALL
CLEAR ALL
SET TALK OFF
SET ECHO OFF
SET CENTURY ON
SET PROCEDURE TO Far_lib
IF !FILE('C:\WINDOWS\IsDiskIn.Bin')
	DO standby WITH ' COPIA NO AUTORIZADA '
	RETURN
ENDIF
DO fox_ambi
PUBLIC vusurec
ruta = '..\DATA'
rutapr = '..\DATAPR'
worker = IIF(LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T., .F.)
SET PATH TO &ruta
desa = '  '
IF worker
	IF yesno('Datos de Prueba?')
		desa = ' P R U E B A '
		SET PATH TO &rutapr
	ENDIF
ENDIF
DO lcolores
IF SET('PATH') = ruta
     IF selper()
     ELSE
          RETURN
     ENDIF
ENDIF
*DO ctrldb WITH 1
sistema = '7'
vuser_id = ''
vmaq = ALLTRIM(LEFT(SYS(0), 15))
IF !FILE('USUARIO.CDX')
     DO standby WITH  ;
        ' Error en INDEXA '
ENDIF
vconex = ALLTRIM(LEFT(SYS(0), 8))
vmaq = ALLTRIM(RIGHT(SYS(0), 2))
vllav = ALLTRIM(SYS(2))
USE Usuario ALIAS usua ORDER  Usuario1
IF !clave()
     CLOSE DATABASES
     DO standby WITH '!! Acceso Denegado !!',10, 15
     DO ctrldb WITH 2
     IF worker
          CANCEL
     ENDIF
     QUIT
ENDIF
vuser_id = ALLTRIM(usuario)
vnombre = ALLTRIM(nombre)
vcoddep = coddep
vnumdep = numdep
vflag = flag
USE
USE iteusu ALIAS iteu ORDER  ;
    iteusu1
SELECT iteu
SET FILTER TO sistema = '6'
vusucla = CHRTRAN(vuser_id, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
vacceso = ' '
SEEK vusucla
SET FILTER TO
USE
= savcon(1)
USE parmae ALIAS parma ORDER  ;
    parmae1
SEEK 'EMPRES'
IF !EOF()
     cia = ALLTRIM(descri)
     detaw = ALLTRIM(descriaux)
     direw = ALLTRIM(descriau2)
     telew = codigoaux
     IF SEEK('RUCEMP01')
          rucw = ALLTRIM(descriaux)
     ELSE
          rucw = '           '
     ENDIF
ELSE
     = poperror( ;
       'El Sistema ha encontrado UN ERROR.... NO EXISTE datos generales de la empresa' +  ;
       '               CONSULTE con su Administrador de Sistemas      Por Seguridad se ABANDORA del Sistema' ;
       )
     CLOSE DATABASES
     RETURN
ENDIF
USE
subc = ' '
titu = '   Modulo de Control de Kardex '
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
spac = (80 - (LEN(cia) + LEN(titu) + 10)) / 2
rotulo1 = cia + SPACE(spac) + titu + SPACE(spac)
usuarios = '® USER: ' + vnombre + ' ¯'
fecha = DTOC(DATE()) + SPACE(2)
rotulo2 = PADR(desa, 17, ' ') + PADC(usuarios, 46, ' ') + PADL(fecha, 17, ' ')
DO logos WITH rotulo1, rotulo2,50
SAVE SCREEN TO principal
IF worker
     ON ERROR
ELSE
     ON ERROR DO fox_errs WITH PROGRAM()
ENDIF
SAVE SCREEN TO pantalla
SET CLOCK ON
STORE .T. TO ven_sistem
STORE .F. TO esc_tecla
= SYS(2002, 1)
l_col = 'W+/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B '
IF escolor
     DEFINE POPUP menu FROM 5,2 TO 17,25 SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP menu FROM 5, 2 TO 17, 25 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF menu PROMPT ' \<Compras            ' MESSAGE ''
DEFINE BAR 2 OF menu PROMPT ' \<Ventas             ' MESSAGE ''
DEFINE BAR 3 OF menu PROMPT ' \<Existencias        ' MESSAGE ''
DEFINE BAR 4 OF menu PROMPT ' \<Consultas          ' MESSAGE ''
DEFINE BAR 5 OF menu PROMPT ' \<Procesos           ' MESSAGE ''
DEFINE BAR 6 OF menu PROMPT ' \<Catalogos y Tablas ' MESSAGE ''
DEFINE BAR 7 OF menu PROMPT ' \<Utilitarios        ' MESSAGE ''
DEFINE BAR 8 OF menu PROMPT ' \<Salida              ' MESSAGE ''
IF escolor
	DEFINE POPUP pop_01 FROM 5,27 SHADOW color &l_col
ELSE
	DEFINE POPUP pop_01 FROM 5,27 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_01 PROMPT '\<Orden de Compra      ' MESSAGE ''
IF escolor
     DEFINE POPUP pop_02 FROM 5,27 SHADOW color &l_col
ELSE
     DEFINE POPUP pop_02 FROM 5,27 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_02 PROMPT '\<Orden de Pedido      ' MESSAGE ''
DEFINE BAR 2 OF pop_02 PROMPT '\<Comprobante de Pago  ' MESSAGE ''
DEFINE BAR 3 OF pop_02 PROMPT '\-                     ' MESSAGE ''
DEFINE BAR 4 OF pop_02 PROMPT '\<Donaciones           ' MESSAGE ''
IF escolor
     DEFINE POPUP pop_03 FROM 6,27 SHADOW color &l_col
ELSE
     DEFINE POPUP pop_03 FROM 6,27 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_03 PROMPT '\<Stock Actual       '  MESSAGE ''
DEFINE BAR 2 OF pop_03 PROMPT '\<Kardex             '  MESSAGE ''
DEFINE BAR 3 OF pop_03 PROMPT '\<Inventario Inicial '  MESSAGE ''
IF escolor
     DEFINE POPUP pop_03a FROM 6,27 SHADOW color &l_col
ELSE
     DEFINE POPUP pop_03a FROM 6,27 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_03a PROMPT '\<Caja                         ' MESSAGE ''
DEFINE BAR 2 OF pop_03a PROMPT '\<Ventas por T‚cnicos          ' MESSAGE ''
DEFINE BAR 3 OF pop_03a PROMPT '\<Estad¡stica de Medicamentos  ' MESSAGE ''
DEFINE BAR 4 OF pop_03a PROMPT '\<Reporte de Utilidades        ' MESSAGE ''
IF escolor
     DEFINE POPUP pop_04 FROM 7,27 SHADOW;
color &l_col
ELSE
     DEFINE POPUP pop_04 FROM 7, 27 COLOR SCHEME  c_popup
ENDIF
DEFINE BAR 1 OF pop_04 PROMPT '\<Cerrar Periodo      ' MESSAGE ''
DEFINE BAR 2 OF pop_04 PROMPT '\<Importar Kardex     ' MESSAGE ''
IF escolor
     DEFINE POPUP pop_05 FROM 8,27 SHADOW color &l_col
ELSE
     DEFINE POPUP pop_05 FROM 8,27 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_05 PROMPT '\<Grupos Genericos '  MESSAGE ''
DEFINE BAR 2 OF pop_05 PROMPT '\<Laboratorios     '  MESSAGE ''
DEFINE BAR 3 OF pop_05 PROMPT '\<Medicamentos     '  MESSAGE ''
DEFINE BAR 4 OF pop_05 PROMPT '\<Precios          '  MESSAGE ''
DEFINE BAR 5 OF pop_05 PROMPT '\-                 '  MESSAGE ''
DEFINE BAR 6 OF pop_05 PROMPT '\<Proveedores      '  MESSAGE ''
IF escolor
     DEFINE POPUP pop_06 FROM 9,27 SHADOW color &l_col
ELSE
     DEFINE POPUP pop_06 FROM 9, 27 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_06 PROMPT '\<Indexar Archivos   ' MESSAGE ''
DEFINE BAR 2 OF pop_06 PROMPT '\<Backup de Archivos ' MESSAGE ''
DEFINE BAR 3 OF pop_06 PROMPT '\<Deputaci¢n Backups ' MESSAGE ''
DEFINE BAR 4 OF pop_06 PROMPT '\<Correlativos       ' MESSAGE ''
DEFINE BAR 5 OF pop_06 PROMPT '\<Seguridad Usuarios ' MESSAGE ''
ON SELECTION POPUP menu do act_menus
IF !vflag $ 'J*'
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
*------------------
choice = BAR()
DO CASE
     CASE choice = 1
          ON SELECTION POPUP pop_01 DO menu_01
          ACTIVATE POPUP pop_01
     CASE choice = 2
          ON SELECTION POPUP pop_02 do menu_02
          ACTIVATE POPUP pop_02
     CASE choice = 3
          ON SELECTION POPUP pop_03 do menu_03
          ACTIVATE POPUP pop_03
     CASE choice = 4
          ON SELECTION POPUP pop_03a do menu_03a
          ACTIVATE POPUP pop_03a
     CASE choice = 5
          ON SELECTION POPUP pop_04 DO menu_04
          ACTIVATE POPUP pop_04
     CASE choice = 6
          ON SELECTION POPUP pop_05 do menu_05
          ACTIVATE POPUP pop_05
     CASE choice = 7
          ON SELECTION POPUP pop_06 DO menu_06
          ACTIVATE POPUP pop_06
     CASE BAR() = 8
          DO salmenu
ENDCASE
RESTORE SCREEN FROM principal
RETURN
*
PROCEDURE menu_01
choice = BAR()
DO CASE
     CASE choice = 1
          DO fordcom
ENDCASE
IF !escolor
     RESTORE SCREEN FROM principal
     ON SELECTION POPUP pop_01 do menu_01
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_01, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02
*----------------
choice = BAR()
DO CASE
	CASE choice = 1
		DO fordped
	CASE choice = 2
		DO fcompag
	CASE choice = 4
		DO fdonac
ENDCASE
IF !escolor
	RESTORE SCREEN FROM principal
	ON SELECTION POPUP pop_02 do menu_02
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_03
*----------------
choice = BAR()
DO CASE
     CASE choice = 1
          DO stkact
     CASE choice = 2
          DO repkar
     CASE choice = 3
          DO invini
ENDCASE
IF !escolor
     ON SELECTION POPUP pop_03 do menu_03
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_03, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_03a
*-----------------
choice = BAR()
DO CASE
     CASE choice = 1
          DO repcaj
     CASE choice = 2
          DO reptec
     CASE choice = 3
          DO estmed
     CASE choice = 4
          DO RepUtil
     CASE choice = 3
ENDCASE
IF !escolor
     ON SELECTION POPUP pop_03a do menu_03a
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_03a, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_04
*----------------
choice = BAR()
DO CASE
     CASE choice = 1
          DO cieper
     CASE choice = 2
          DO impkar
ENDCASE
IF !escolor
     ON SELECTION POPUP pop_04 do menu_04
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_04, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_05
*----------------
choice = BAR()
DO CASE
     CASE choice = 1
          DO manpar1 WITH 'GRUPOS GENERICOS','CODGEB', 'Codigo','', 'Sistema'
     CASE choice = 2
          DO reglab
     CASE choice = 3
          DO regmed
     CASE choice = 4
          DO regpre
     CASE choice = 6
          DO promae
ENDCASE
IF !escolor
     ON SELECTION POPUP pop_05 do menu_05
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_05, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_06
choice = BAR()
DO CASE
     CASE choice = 1
          DO indexa
     CASE choice = 2
          DO backup
     CASE choice = 3
          DO depbk
     CASE choice = 4
          DO manpar1 WITH 'CORRELATIVOS', 'CORREL', 'Codigo', '', 'Descripci¢n', '', '', '', 'Correlativo'
     CASE choice = 5
          DO ctrlacc WITH vuser_id
ENDCASE
IF !escolor
     ON SELECTION POPUP pop_06 do menu_06
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_06, menu
HIDE WINDOW ALL
RETURN
*
PROCEDURE salmenu
STORE yesno('¨ Estamos terminando la sesi¢n ?') TO in_secoems
IF in_secoems
     SET COLOR TO
     CLOSE DATABASES
     HIDE POPUP ALL
     = savcon(2)
     DO ctrldb WITH 2
     IF worker
          CLEAR
          SET HELP ON
          ON KEY
          SET EXCLUSIVE OFF
          CANCEL
     ELSE
          RESTORE SCREEN FROM principal
          DO clrscr
          QUIT
     ENDIF
ELSE
     RETURN
ENDIF
*
FUNCTION fenc
PARAMETER ccodigo
USE IteUsuOP
SUSPEND
RETURN .T.
*
PROCEDURE abrepas
USE IteUsuOp ORDER IteUsuOp2
SET SKIP OF BAR 1 OF pop_01 ;
.NOT. SEEK(vusucla + '0001')
vopcion = .T.
FOR i = 1 TO 1
     IF !SKPBAR('pop_01', i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 1 OF menu vopcion
SET SKIP OF BAR 1 OF pop_02 .NOT. SEEK(vusucla + '0002')
SET SKIP OF BAR 2 OF pop_02 .NOT. SEEK(vusucla + '0003')
SET SKIP OF BAR 4 OF pop_02 .NOT. SEEK(vusucla + '0004')
vopcion = .T.
FOR i = 1 TO 4
     IF !SKPBAR('pop_02', i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 2 OF menu vopcion
SET SKIP OF BAR 1 OF pop_03 .NOT. SEEK(vusucla + '0005')
SET SKIP OF BAR 2 OF pop_03 .NOT. SEEK(vusucla + '0006')
SET SKIP OF BAR 3 OF pop_03 .NOT. SEEK(vusucla + '0007')
vopcion = .T.
FOR i = 1 TO 3
     IF !SKPBAR('pop_03', i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 3 OF menu vopcion
SET SKIP OF BAR 1 OF pop_04 .NOT. SEEK(vusucla + '0008')
SET SKIP OF BAR 2 OF pop_04 .NOT. SEEK(vusucla + '0009')
vopcion = .T.
FOR i = 1 TO 2
     IF !SKPBAR('pop_04', i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 5 OF menu vopcion
SET SKIP OF BAR 1 OF pop_05 .NOT. SEEK(vusucla + '0010')
SET SKIP OF BAR 2 OF pop_05 .NOT. SEEK(vusucla + '0011')
SET SKIP OF BAR 3 OF pop_05 .NOT. SEEK(vusucla + '0012')
SET SKIP OF BAR 5 OF pop_05 .NOT. SEEK(vusucla + '0013')
vopcion = .T.
FOR i = 1 TO 5
     IF !SKPBAR('pop_05', i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 6 OF menu vopcion
SET SKIP OF BAR 1 OF pop_06 .NOT. SEEK(vusucla + '0014')
SET SKIP OF BAR 2 OF pop_06 .NOT. SEEK(vusucla + '0015')
SET SKIP OF BAR 3 OF pop_06 .NOT. SEEK(vusucla + '0016')
SET SKIP OF BAR 4 OF pop_06 .NOT. SEEK(vusucla + '0017')
SET SKIP OF BAR 5 OF pop_06 .NOT. SEEK(vusucla + '0018')
vopcion = .T.
FOR i = 1 TO 5
     IF !SKPBAR('pop_06', i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 7 OF menu vopcion
USE IN iteusuop
RETURN
*
PROCEDURE lcolores
SET COLOR OF SCHEME 1 TO W/N, N+/W, W+/N,W+/N, W/N, B+/N, W+/N, -, W+/N, W/N
SET COLOR OF SCHEME 10 TO G/N, GR+/B,G/W, G/N, GR+/N, W+/BG, BG+/B, N+/N, W+/N,W/N
SET COLOR OF SCHEME 08 TO G/N, GR+/B,GR+/W, G/N, GR+/N, W+/BG, BG+/B, N+/N,W+/N, W/N
SET COLOR OF SCHEME 21 TO W/N, N+/W, W+/N,W+/N, W/N, B+/N, W+/N, -, W+/N, W/N
RETURN

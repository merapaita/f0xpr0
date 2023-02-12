CLEAR
CLOSE ALL
CLEAR ALL
worker = IIF(LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T., .F.)
SET PROCEDURE TO sai_lib
DO fox_ambi
DEFINE WINDOW _inve FROM 10, 27 TO 15, 77 FLOAT SHADOW DOUBLE
DO lcolores
IF selper()
ELSE
     RETURN
ENDIF
rutapr = 'F:\BENE2004\DATAPR'
desa = ' SISTEMAS '
IF worker
     IF yesno('Datos de Prueba?')
          desa = ' P R U E B A '
          SET PATH TO &rutapr
     ENDIF
ENDIF
vuser_id = ALLTRIM(LEFT(SYS(0),15))
IF  .NOT. FILE('USUARIO.CDX')
     DO standby WITH ' Error en INDEXA '
ENDIF
PUBLIC vcoddep, vconex, vmaq, vllave, vusurec, vflag, vnumdep, sistema, vsistema, sistctrl
sistema = '2'
sistctrl = sistema
vconex = ALLTRIM(LEFT(SYS(0), 8))
vmaq = ALLTRIM(RIGHT(SYS(0), 2))
vllav = ALLTRIM(SYS(2))
USE usuario ALIAS usua ORDER usuario1
IF  .NOT. clave()
     CLOSE DATABASES
     DO standby WITH '!! Acceso Denegado !!', 10, 15
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
USE iteusu ALIAS iteu ORDER iteusu1
SELECT iteu
SET FILTER TO sistema = '2'
vusucla = CHRTRAN(vuser_id, 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789','XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
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
     IF SEEK('RUCEMP01')
          rucw = ALLTRIM(descriaux)
     ELSE
          rucw = '           '
     ENDIF
ELSE
ENDIF
USE
subc = ' '
titu = '   Sistema de Abastecimientos '
vsistema = '1'
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
     ON ERROR do fox_errs with program()
ENDIF
SAVE SCREEN TO pantalla
SET CLOCK ON
STORE .T. TO ven_sistem
STORE .F. TO esc_tecla
= SYS(2002, 1)
USE PARMAE ALIAS parma ORDER  ;
    Parmae1
SEEK 'UNIEJE'
IF FOUND()
     vueje = ALLTRIM(codigoaux)
ENDIF
SEEK 'UNIGES'
IF FOUND()
     vuges = ALLTRIM(codigo)
ENDIF
l_col = 'W+/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B '
IF escolor
     define popup menu1 from 5,2 to 17,22;
shadow color &l_col
ELSE
     DEFINE POPUP menu1 FROM 5, 2  ;
            TO 17, 22 COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF menu1 PROMPT  ;
       ' \<Obtenciones    '  ;
       MESSAGE  ;
       'Men£ de Opciones para OBTENCIONES : Pecosas, O/C '
DEFINE BAR 2 OF menu1 PROMPT  ;
       ' ser\<Vicios      '  ;
       MESSAGE  ;
       'Men£ de Opciones para Procesos de SERVICIOS : Solicitud Servicio, O/S'
DEFINE BAR 3 OF menu1 PROMPT  ;
       ' \<Cat logos      '  ;
       MESSAGE  ;
       'Men£ de Opciones para Actualizar : Proveedores, Art¡culos,Servicios'
DEFINE BAR 4 OF menu1 PROMPT  ;
       ' ta\<Blas         '  ;
       MESSAGE  ;
       'Men£ de Opciones para Actualizar las TABLAS maestras del Sistema '
DEFINE BAR 5 OF menu1 PROMPT  ;
       ' \<Utilitarios    '  ;
       MESSAGE  ;
       'Men£ de UTILITARIOS de Mantenimiento y Seguridad del Sistema'
DEFINE BAR 6 OF menu1 PROMPT  ;
       ' co\<Nsultas      '  ;
       MESSAGE  ;
       'Men£ de Consultas de Documentaci¢n Administrativa '
DEFINE BAR 7 OF menu1 PROMPT  ;
       ' \<Despachos      '  ;
       MESSAGE  ;
       'Registra y Actualiza los DESPACHOS DE PECOSAS DE STOCK'
DEFINE BAR 8 OF menu1 PROMPT  ;
       ' \<Polizas        '  ;
       MESSAGE  ;
       'Actualiza Registro de POLIZAS DE ENTRADA Y SALIDA'
DEFINE BAR 9 OF menu1 PROMPT  ;
       ' \<Kardex         '  ;
       MESSAGE  ;
       'Permite Actualizar Saldo Inicial y Consultas de Stocks disponibles'
DEFINE BAR 10 OF menu1 PROMPT  ;
       ' \<Nota Ent. Alm. '  ;
       MESSAGE  ;
       'Permite ingresar Notas de Entrada a Almacen (Donaciones)'
DEFINE BAR 11 OF menu1 PROMPT  ;
       ' \<Salida        '  ;
       MESSAGE  ;
       'SALE DEL SISTEMA'
IF escolor
     define popup pop_02a from 5,26 shadow;
color &l_col
ELSE
     DEFINE POPUP pop_02a FROM 5,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02a PROMPT  ;
       '\<Cuadro necesidades'  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_02a PROMPT  ;
       '\<Verifica cuadro   '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_02a PROMPT  ;
       'c\<Onsolidaci¢n     '  ;
       MESSAGE ''
IF escolor
     define popup pop_02b from 5,26 shadow;
color &l_col
ELSE
     DEFINE POPUP pop_02b FROM 5,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02b PROMPT  ;
       '\<Registro pedidos-PECOSAS'  ;
       MESSAGE  ;
       'Registra y Genera PECOSAS de Stock y de Compra'
DEFINE BAR 2 OF pop_02b PROMPT  ;
       '\<Solicitud cotizaci¢n  '  ;
       MESSAGE  ;
       'Elabora Solicitudes de COTIZACION PARA COMPRAS'
DEFINE BAR 3 OF pop_02b PROMPT  ;
       'recepci¢n \<Cotizaciones'  ;
       MESSAGE ''
DEFINE BAR 4 OF pop_02b PROMPT  ;
       '\<Adjudicaci¢n          '  ;
       MESSAGE ''
DEFINE BAR 5 OF pop_02b PROMPT  ;
       '\<Orden de compra       '  ;
       MESSAGE  ;
       'Registra y Genera ORDENES DE COMPRA'
DEFINE BAR 6 OF pop_02b PROMPT  ;
       '\<Parte de anulaci¢n    '  ;
       MESSAGE ''
DEFINE BAR 7 OF pop_02b PROMPT  ;
       'parte de re\<Baja       '  ;
       MESSAGE ''
IF escolor
     define popup pop_02cc from 12,26;
shadow color &l_col
ELSE
     DEFINE POPUP pop_02cc FROM  ;
            12, 26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02cc PROMPT  ;
       '\<Entrada        '  ;
       MESSAGE  ;
       'Registra y Genera POLIZAS DE ENTRADA'
DEFINE BAR 2 OF pop_02cc PROMPT  ;
       '\<Salida         '  ;
       MESSAGE  ;
       'Registra y Genera POLIZAS DE SALIDA'
IF escolor
     define popup pop_02c from 07,26 shadow;
color &l_col
ELSE
     DEFINE POPUP pop_02c FROM 07,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02c PROMPT  ;
       '\<Gu¡a internamiento       '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_02c PROMPT  ;
       '\<Despacho                 '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_02c PROMPT  ;
       '\<Nota de entrada almac‚n  '  ;
       MESSAGE ''
DEFINE BAR 4 OF pop_02c PROMPT  ;
       '\<P¢lizas                 '  ;
       MESSAGE ''
IF escolor
     define popup pop_02ca from 10,54;
shadow color &l_col
ELSE
     DEFINE POPUP pop_02ca FROM  ;
            10, 54 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02ca PROMPT  ;
       '\<Entrada  '
DEFINE BAR 2 OF pop_02ca PROMPT  ;
       '\<Salida   '
IF escolor
     define popup pop_02d from 06,26 shadow;
color &l_col
ELSE
     DEFINE POPUP pop_02d FROM 06,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02d PROMPT  ;
       '\<Solicitud de servicio     '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_02d PROMPT  ;
       'so\<Licitud cotizaci¢n servicios'  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_02d PROMPT  ;
       'recepci¢n \<Cotizaciones    '  ;
       MESSAGE ''
DEFINE BAR 4 OF pop_02d PROMPT  ;
       '\<Adjudicaci¢n              '  ;
       MESSAGE ''
DEFINE BAR 5 OF pop_02d PROMPT  ;
       'o\<Rden de Servicio         '  ;
       MESSAGE ''
DEFINE BAR 6 OF pop_02d PROMPT  ;
       '\<Parte de anulaci¢n        '  ;
       MESSAGE ''
DEFINE BAR 7 OF pop_02d PROMPT  ;
       'parte re\<Baja              '  ;
       MESSAGE ''
IF escolor
     define popup pop_02e from;
 9,26 shadow color &l_col
ELSE
     DEFINE POPUP pop_02e FROM 9,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02e PROMPT  ;
       '\<Existencias valoradas   '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_02e PROMPT  ;
       '\<Control visible almacen '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_02e PROMPT  ;
       '\<Stock                   '  ;
       MESSAGE ''
DEFINE BAR 4 OF pop_02e PROMPT  ;
       's\<Aldos inventario       '  ;
       MESSAGE ''
DEFINE BAR 5 OF pop_02e PROMPT  ;
       'sa\<ldos inicial          '  ;
       MESSAGE ''
DEFINE BAR 6 OF pop_02e PROMPT  ;
       '\<Reporte Estado Inicial de Productos '  ;
       MESSAGE ''
IF escolor
     define popup pop_02i from 10,26 shadow;
color &l_col
ELSE
     DEFINE POPUP pop_02i FROM 10,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02i PROMPT  ;
       '\<Registro de vales  '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_02i PROMPT  ;
       '\<Agrupa vales       '  ;
       MESSAGE ''
IF escolor
     define popup pop_02f from 07,26 shadow;
color &l_col
ELSE
     DEFINE POPUP pop_02f FROM 07,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02f PROMPT  ;
       '\<Proveedores    '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_02f PROMPT  ;
       '\<Art¡culos      '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_02f PROMPT  ;
       '\<Servicios      '  ;
       MESSAGE ''
IF escolor
     define popup pop_02g from 08,26 shadow;
color &l_col
ELSE
     DEFINE POPUP pop_02g FROM 08,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02g PROMPT  ;
       '\<Grupos gen‚ricos  '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_02g PROMPT  ;
       '\<Dependencias      '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_02g PROMPT  ;
       '\<Tipo de servicios '  ;
       MESSAGE ''
DEFINE BAR 4 OF pop_02g PROMPT  ;
       '\<Procedencia       '  ;
       MESSAGE ''
DEFINE BAR 5 OF pop_02g PROMPT  ;
       'd\<Estino           '  ;
       MESSAGE ''
DEFINE BAR 6 OF pop_02g PROMPT  ;
       '\<Unidad medida     '  ;
       MESSAGE ''
DEFINE BAR 7 OF pop_02g PROMPT  ;
       '\<Jefaturas         '  ;
       MESSAGE  ;
       'Actualiza la tabla de JEFES DE OFICINAS'
IF escolor
     define popup pop_02h from 09,26 margin;
 shadow color &l_col
ELSE
     DEFINE POPUP pop_02h FROM 09,  ;
            26 MARGIN COLOR  ;
            SCHEME c_popup
ENDIF
DEFINE BAR 1 OF pop_02h PROMPT  ;
       '\<Indexar archivos     '
DEFINE BAR 2 OF pop_02h PROMPT  ;
       '\<Backup de los archivos'
DEFINE BAR 3 OF pop_02h PROMPT  ;
       'De\<puraci¢n de backups '
DEFINE BAR 4 OF pop_02h PROMPT  ;
       '\<Correlativos         '
DEFINE BAR 5 OF pop_02h PROMPT  ;
       '\-'
DEFINE BAR 6 OF pop_02h PROMPT  ;
       '\<Seguridad         '  ;
       MESSAGE  ;
       '®® Acceso RESTRINGIDO ¯¯'
DEFINE BAR 7 OF pop_02h PROMPT  ;
       '\<Seguridad Opciones'  ;
       MESSAGE  ;
       '®® Acceso RESTRINGIDO ¯¯'
DEFINE BAR 8 OF pop_02h PROMPT  ;
       '\<Usuario ABAS      '
IF escolor
     define popup pop_02ha from 12,54;
shadow color &l_col
ELSE
     DEFINE POPUP pop_02ha FROM  ;
            12, 54 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02ha PROMPT  ;
       '\<Proveedores        '  ;
       MESSAGE  ;
       'Actualiza el correlativo de REGISTRO DE PROVEEDORES'
DEFINE BAR 2 OF pop_02ha PROMPT  ;
       'orden de \<Compra    '  ;
       MESSAGE  ;
       'Actualiza el correlativo de ORDENES DE COMPRA '
DEFINE BAR 3 OF pop_02ha PROMPT  ;
       'orden de \<Servicio  '  ;
       MESSAGE  ;
       'Actualiza el correlativo de ORDENES DE SERVICIO'
DEFINE BAR 4 OF pop_02ha PROMPT  ;
       'p\<E.co.sa           '  ;
       MESSAGE  ;
       'Actualiza el correlativo de PECOSAS '
DEFINE BAR 5 OF pop_02ha PROMPT  ;
       's\<Olicitud Servicio '  ;
       MESSAGE  ;
       'Actualiza el correlativo de SOLICITUDES DE SERVICIO'
DEFINE BAR 6 OF pop_02ha PROMPT  ;
       'c\<Uadro Comnparativo'  ;
       MESSAGE  ;
       'Actualiza el correlativo de CUADRO COMPARATIVO'
DEFINE BAR 7 OF pop_02ha PROMPT  ;
       'P¢liza de En\<Trada  '  ;
       MESSAGE  ;
       'Actualiza el correlativo de P¢liza de ENTRADA '
DEFINE BAR 8 OF pop_02ha PROMPT  ;
       'P¢liza de Sa\<Lida   '  ;
       MESSAGE  ;
       'Actualiza el correlativo de P¢liza de SALIDA '
DEFINE BAR 9 OF pop_02ha PROMPT  ;
       'Cotizaci¢n de \<Bienes'  ;
       MESSAGE  ;
       'Actualiza el correlativo de Solicitudes de Cotizaciones de BIENES'
IF escolor
     define popup pop_02j from 3,26 shadow;
color &l_col
ELSE
     DEFINE POPUP pop_02j FROM 3,  ;
            26 COLOR SCHEME  ;
            c_popup
ENDIF
DEFINE BAR 1 OF pop_02j PROMPT  ;
       '\<a. Pe.co.sa.            '  ;
       MESSAGE ''
DEFINE BAR 2 OF pop_02j PROMPT  ;
       '\<b. Orden de Compra      '  ;
       MESSAGE ''
DEFINE BAR 3 OF pop_02j PROMPT  ;
       '\<c. Solicitud de Servicio'  ;
       MESSAGE ''
DEFINE BAR 4 OF pop_02j PROMPT  ;
       '\<d. Orden de Servicio    '  ;
       MESSAGE ''
DEFINE BAR 5 OF pop_02j PROMPT  ;
       '\<e. Proveedores          '  ;
       MESSAGE ''
DEFINE BAR 6 OF pop_02j PROMPT  ;
       '\<f. Art¡culos            '  ;
       MESSAGE ''
DEFINE BAR 7 OF pop_02j PROMPT  ;
       '\<g. P¢lizas de Entrada   '  ;
       MESSAGE ''
DEFINE BAR 8 OF pop_02j PROMPT  ;
       '\<h. P¢lizas de Salida    '  ;
       MESSAGE ''
DEFINE BAR 9 OF pop_02j PROMPT  ;
       '\<i. Parte Anulacion O/C  '  ;
       MESSAGE ''
DEFINE BAR 10 OF pop_02j PROMPT  ;
       '\<j. Parte Anulacion O/S  '  ;
       MESSAGE ''
DEFINE BAR 11 OF pop_02j PROMPT  ;
       '\<k. Parte Rebaja O/C     '  ;
       MESSAGE ''
DEFINE BAR 12 OF pop_02j PROMPT  ;
       '\<l. Parte Rebaja O/S     '  ;
       MESSAGE ''
DEFINE BAR 13 OF pop_02j PROMPT  ;
       '\-'
DEFINE BAR 14 OF pop_02j PROMPT  ;
       '\<m. Hojas de Control     '  ;
       MESSAGE ''
DEFINE BAR 15 OF pop_02j PROMPT  ;
       '\<n. Comprobantes de Pago '  ;
       MESSAGE ''
DEFINE BAR 16 OF pop_02j PROMPT  ;
       '\<o. Cheques              '  ;
       MESSAGE ''
DEFINE BAR 17 OF pop_02j PROMPT  ;
       '\<p. Hojas Modificaci¢n   '  ;
       MESSAGE ''
DEFINE BAR 18 OF pop_02j PROMPT  ;
       '\<q. Seguimiento Doc.Fte  '  ;
       MESSAGE ''
ON SELECTION POPUP menu1 do act_menus
IF  .NOT. vflag $ 'J*'
     DO abrepas
ENDIF
SAVE SCREEN TO principal
ACTIVATE POPUP menu1
DO WHILE EMPTY(PROMPT())
     ACTIVATE POPUP menu1
ENDDO
ON KEY
RETURN
*
PROCEDURE act_menus
*------------------
choice = BAR()
DO CASE
     CASE choice = 1
          ON SELECTION POPUP pop_02b do;
 menu_02b
          ACTIVATE POPUP pop_02b
     CASE choice = 2
          ON SELECTION POPUP pop_02d do;
 menu_02d
          ACTIVATE POPUP pop_02d
     CASE choice = 3
          ON SELECTION POPUP pop_02f do;
 menu_02f
          ACTIVATE POPUP pop_02f
     CASE choice = 4
          ON SELECTION POPUP pop_02g do;
 menu_02g
          ACTIVATE POPUP pop_02g
     CASE choice = 5
          ON SELECTION POPUP pop_02h do;
 menu_02h
          ACTIVATE POPUP pop_02h
     CASE choice = 6
          ON SELECTION POPUP pop_02j do;
 menu_02j
          ACTIVATE POPUP pop_02j
     CASE choice = 7
          DO pecsal WITH vsistema
     CASE choice = 8
          ON SELECTION POPUP pop_02cc;
do  menu_02cc
          ACTIVATE POPUP pop_02cc
     CASE choice = 9
          ON SELECTION POPUP pop_02e DO;
 menu_02e
          ACTIVATE POPUP pop_02e
     CASE choice = 10
          DO notalm
     CASE BAR() = 11
          DO salmenu
ENDCASE
RESTORE SCREEN FROM principal
RETURN
*
PROCEDURE menu_02a
choice = BAR()
DO CASE
     CASE choice = 1
          DO regcdr
     CASE choice = 2
          DO vercdr
     CASE choice = 3
          DO concdr
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02a do menu_02a
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02a, menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02b
choice = BAR()
DO CASE
     CASE choice = 1
          DO regpec WITH 2, ' ',  ;
             vsistema
     CASE choice = 2
          DO solcot WITH 1
     CASE choice = 3
          DO regcot WITH 1
     CASE choice = 4
          DO adjcot WITH 1
     CASE choice = 5
          DO ordcom WITH vsistema,  ;
             1
     CASE choice = 6
          DO anupar WITH 1
     CASE choice = 7
          DO rebpar WITH 1,  ;
             vsistema
     CASE choice = 8
          DO consal
     CASE choice = 9
          DO conpre
     CASE choice = 10
          DO conord WITH 'O/C'
     CASE choice = 11
          DO conord1 WITH 'O/C'
     CASE choice = 12
          DO conordb WITH 'O/C'
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02b do menu_02b
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02b, menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02c
choice = BAR()
DO CASE
     CASE choice = 1
          DO reggui WITH vsistema
     CASE choice = 2
          DO pecsal WITH vsistema
     CASE choice = 3
          DO notalm
     CASE choice = 4
          ON SELECTION POPUP pop_02ca;
do  menu_02ca
          ACTIVATE POPUP pop_02ca
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02c do menu_02c
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02c, menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02cc
choice = BAR()
DO CASE
     CASE choice = 1
          DO polent1
     CASE choice = 2
          DO polsal
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02cc do menu_02cc
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02cc, menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02ca
choice = BAR()
DO CASE
     CASE choice = 1
          DO polent1
     CASE choice = 2
          DO polsal
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02ca do menu_02ca
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02ca, pop_02c,  ;
     menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02d
choice = BAR()
DO CASE
     CASE choice = 1
          DO regsol WITH vsistema
     CASE choice = 2
          DO solcot WITH 2
     CASE choice = 3
          DO regcot WITH 2
     CASE choice = 4
          DO adjcot WITH 2
     CASE choice = 5
          DO ordser WITH vsistema,  ;
             1
     CASE choice = 6
          DO anupar WITH 2
     CASE choice = 7
          DO rebpar WITH 2,  ;
             vsistema
     CASE choice = 8
          DO consal
     CASE choice = 9
          DO conpre
     CASE choice = 10
          DO conord WITH 'O/S'
     CASE choice = 11
          DO conordb WITH 'O/S'
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02d do menu_02d
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02d, menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02e
choice = BAR()
DO CASE
     CASE choice = 1
          DO regkar
     CASE choice = 2
          DO regalm
     CASE choice = 3
          DO presenta
     CASE choice = 4
          DO stockin
     CASE choice = 5
          DO saldini
     CASE choice = 6
          DO reptotal
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02e do menu_02e
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02e, menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02i
choice = BAR()
DO CASE
     CASE choice = 1
          DO regval
     CASE choice = 2
          DO agrupa
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02i do menu_02i
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02i, menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02f
choice = BAR()
DO CASE
     CASE choice = 1
          DO promae2
     CASE choice = 2
          DO artmae
     CASE choice = 3
          DO sermae
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02f do menu_02f
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02f, menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02g
choice = BAR()
DO CASE
     CASE choice = 1
          DO manpar WITH 'CODGEB',  ;
             'Grupo Gen‚rico',  ;
             'Grupos Gen‚ricos',  ;
             1,  ;
             'Valor siguiente'
     CASE choice = 2
          DO manpar WITH 'CODDEP',  ;
             'Dependencia',  ;
             'Dependencias', 1,  ;
             'Valor siguiente'
     CASE choice = 3
          DO manpar WITH 'TIPSER',  ;
             'Tipo de Servicio',  ;
             'Tipos de Documentos'
     CASE choice = 4
          DO manpar WITH 'CODPRC',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             1,  ;
             'Valor siguiente'
     CASE choice = 5
          DO manpar WITH 'CODDET',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             1,  ;
             'Valor siguiente'
     CASE choice = 6
          DO manpar WITH 'CODUNI',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             1,  ;
             'Valor siguiente'
     CASE choice = 7
          DO manjef
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02g do menu_02g
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02g, menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02h
choice = BAR()
DO CASE
     CASE choice = 1
          = poperror( ;
            'Aseg£rese que no est‚n usando el sistema, para poder ' +  ;
            'realizar el proceso de reordenamiento sin ning£n problema' ;
            )
          DO indexaba
     CASE choice = 2
          = poperror( ;
            'Aseg£rese que no est‚n usando el sistema, para poder ' +  ;
            'realizar la copia de seguridad sin ning£n problema' ;
            )
          DO backupx
     CASE choice = 3
          DO restorex
     CASE choice = 4
          ON SELECTION POPUP pop_02ha;
do  menu_02ha
          ACTIVATE POPUP pop_02ha
     CASE choice = 6
          DO segabas WITH  ;
             vuser_id
     CASE choice = 7
          DO segabop WITH  ;
             vuser_id
     CASE choice = 8
          DO contusu
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02h do menu_02h
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02h, menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02ha
choice = BAR()
DO CASE
     CASE choice = 1
          DO manpar WITH  ;
             'CORRELPROVEE',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             8,  ;
             'Valor siguiente'
     CASE choice = 2
          DO manpar WITH  ;
             'CORRELORDENC',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             8,  ;
             'Valor siguiente'
     CASE choice = 3
          DO manpar WITH  ;
             'CORRELORDENS',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             8,  ;
             'Valor siguiente'
     CASE choice = 4
          DO manpar WITH  ;
             'CORRELPECOSA',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             8,  ;
             'Valor siguiente'
     CASE choice = 5
          DO manpar WITH  ;
             'CORRELSOLSER',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             8,  ;
             'Valor siguiente'
     CASE choice = 6
          DO manpar WITH  ;
             'CORRELCCC',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             8,  ;
             'Valor siguiente'
     CASE choice = 7
          DO manpar WITH  ;
             'CORRELPOLENT',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             8,  ;
             'Valor siguiente'
     CASE choice = 8
          DO manpar WITH  ;
             'CORRELPOLSAL',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             8,  ;
             'Valor siguiente'
     CASE choice = 9
          DO manpar WITH  ;
             'CORRELSOLCOT',  ;
             'Tipo de Documento',  ;
             'Tipos de Documentos',  ;
             8,  ;
             'Valor siguiente'
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02h do menu_02h
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02h, menu1
HIDE WINDOW ALL
RETURN
*
PROCEDURE menu_02j
choice = BAR()
DO CASE
     CASE choice = 1
          DO conpec
     CASE choice = 2
          DO conoc
     CASE choice = 3
          DO conss
     CASE choice = 4
          DO conos
     CASE choice = 5
          DO conprv
     CASE choice = 6
          DO conart
     CASE choice = 7
          DO conpent
     CASE choice = 8
          DO conpsal
     CASE choice = 9
          DO conpan WITH 1
     CASE choice = 10
          DO conpan WITH 2
     CASE choice = 11
          DO conprb WITH 1
     CASE choice = 12
          DO conprb WITH 2
     CASE choice = 14
          DO conhc
     CASE choice = 15
          DO concp
     CASE choice = 16
          DO conch
     CASE choice = 17
          DO conhm
     CASE choice = 18
          DO condoc
ENDCASE
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
     ON SELECTION POPUP pop_02j do menu_02j
ENDIF
RESTORE SCREEN FROM principal
SHOW POPUP pop_02j, menu1
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
          SET EXCLUSIVE OFF
          CANCEL
     ELSE
          RESTORE SCREEN FROM  ;
                  principal
          DO clrscr
          QUIT
     ENDIF
ELSE
     RETURN
ENDIF
*
PROCEDURE salmenu2
IF yesno( ;
   ' Estamos terminando la sesi¢n ' ;
   )
     SET COLOR TO
     CLOSE DATABASES
     CLOSE PROCEDURE
     HIDE POPUP ALL
     RESTORE SCREEN FROM  ;
             principal
     DO clrscr1
     SET HELP ON
     DEACTIVATE WINDOW ALL
     IF worker
          SET EXCLUSIVE OFF
          CLEAR ALL
          CANCEL
     ENDIF
     QUIT
ENDIF
RETURN
*
PROCEDURE abrepas
SET SKIP OF BAR 1 OF pop_02a ;
.NOT. 'A' $ vacceso
SET SKIP OF BAR 2 OF pop_02a ;
.NOT. 'B' $ vacceso
SET SKIP OF BAR 3 OF pop_02a ;
.NOT. 'C' $ vacceso
SET SKIP OF BAR 1 OF pop_02b ;
.NOT. 'D' $ vacceso
SET SKIP OF BAR 2 OF pop_02b ;
.NOT. 'E' $ vacceso
SET SKIP OF BAR 3 OF pop_02b ;
.NOT. 'F' $ vacceso
SET SKIP OF BAR 4 OF pop_02b ;
.NOT. 'G' $ vacceso
SET SKIP OF BAR 5 OF pop_02b ;
.NOT. 'H' $ vacceso
SET SKIP OF BAR 6 OF pop_02b ;
.NOT. 'I' $ vacceso
SET SKIP OF BAR 7 OF pop_02b ;
.NOT. 'J' $ vacceso
SET SKIP OF BAR 8 OF pop_02b ;
.NOT. 'K' $ vacceso
SET SKIP OF BAR 9 OF pop_02b ;
.NOT. 'L' $ vacceso
SET SKIP OF BAR 10 OF pop_02b ;
.NOT. 'M' $ vacceso
SET SKIP OF BAR 11 OF pop_02b ;
.NOT. 'N' $ vacceso
SET SKIP OF BAR 12 OF pop_02b ;
.NOT. '¥' $ vacceso
SET SKIP OF BAR 7 OF menu1 ;
.NOT. 'P' $ vacceso
SET SKIP OF BAR 10 OF menu1 ;
.NOT. 'P' $ vacceso
SET SKIP OF BAR 1 OF pop_02cc ;
.NOT. 'R' $ vacceso
SET SKIP OF BAR 2 OF pop_02cc ;
.NOT. 'S' $ vacceso
SET SKIP OF BAR 1 OF pop_02d ;
.NOT. 'V' $ vacceso
SET SKIP OF BAR 2 OF pop_02d ;
.NOT. 'W' $ vacceso
SET SKIP OF BAR 3 OF pop_02d ;
.NOT. 'X' $ vacceso
SET SKIP OF BAR 4 OF pop_02d ;
.NOT. 'Y' $ vacceso
SET SKIP OF BAR 5 OF pop_02d ;
.NOT. 'Z' $ vacceso
SET SKIP OF BAR 6 OF pop_02d ;
.NOT. 'a' $ vacceso
SET SKIP OF BAR 7 OF pop_02d ;
.NOT. 'b' $ vacceso
SET SKIP OF BAR 8 OF pop_02d ;
.NOT. 'c' $ vacceso
SET SKIP OF BAR 9 OF pop_02d ;
.NOT. 'd' $ vacceso
SET SKIP OF BAR 10 OF pop_02d ;
.NOT. 'e' $ vacceso
SET SKIP OF BAR 1 OF pop_02e ;
.NOT. 'g' $ vacceso
SET SKIP OF BAR 2 OF pop_02e ;
.NOT. 'h' $ vacceso
SET SKIP OF BAR 3 OF pop_02e ;
.NOT. 'i' $ vacceso
SET SKIP OF BAR 4 OF pop_02e ;
.NOT. 'j' $ vacceso
SET SKIP OF BAR 4 OF pop_02e ;
.NOT. 'j' $ vacceso
SET SKIP OF BAR 4 OF pop_02e ;
.NOT. 'j' $ vacceso
SET SKIP OF BAR 1 OF pop_02f ;
.NOT. 'k' $ vacceso
SET SKIP OF BAR 2 OF pop_02f ;
.NOT. 'l' $ vacceso
SET SKIP OF BAR 3 OF pop_02f ;
.NOT. 'm' $ vacceso
SET SKIP OF BAR 4 OF pop_02f ;
.NOT. 'n' $ vacceso
SET SKIP OF BAR 5 OF pop_02f ;
.NOT. '¤' $ vacceso
SET SKIP OF BAR 6 OF pop_02f ;
.NOT. 'o' $ vacceso
SET SKIP OF BAR 7 OF pop_02f ;
.NOT. 'p' $ vacceso
SET SKIP OF BAR 8 OF pop_02f ;
.NOT. 'q' $ vacceso
SET SKIP OF BAR 1 OF pop_02i ;
.NOT. 'r' $ vacceso
SET SKIP OF BAR 2 OF pop_02i ;
.NOT. 's' $ vacceso
SET SKIP OF BAR 1 OF pop_02g ;
.NOT. 't' $ vacceso
SET SKIP OF BAR 2 OF pop_02g ;
.NOT. 'u' $ vacceso
SET SKIP OF BAR 3 OF pop_02g ;
.NOT. 'v' $ vacceso
SET SKIP OF BAR 4 OF pop_02g ;
.NOT. 'w' $ vacceso
SET SKIP OF BAR 5 OF pop_02g ;
.NOT. 'x' $ vacceso
SET SKIP OF BAR 6 OF pop_02g ;
.NOT. 'y' $ vacceso
SET SKIP OF BAR 7 OF pop_02g ;
.NOT. 'f' $ vacceso
SET SKIP OF BAR 1 OF pop_02h ;
.NOT. 'z' $ vacceso
SET SKIP OF BAR 2 OF pop_02h ;
.NOT. '1' $ vacceso
SET SKIP OF BAR 3 OF pop_02h ;
.NOT. '2' $ vacceso
SET SKIP OF BAR 4 OF pop_02h ;
.NOT. '3' $ vacceso
SET SKIP OF BAR 6 OF pop_02h ;
.NOT. '4' $ vacceso
SET SKIP OF BAR 7 OF pop_02h ;
.NOT. '5' $ vacceso
SET SKIP OF BAR 1 OF pop_02j ;
.NOT. '«' $ vacceso
SET SKIP OF BAR 2 OF pop_02j ;
.NOT. '¬' $ vacceso
SET SKIP OF BAR 3 OF pop_02j ;
.NOT. '@' $ vacceso
SET SKIP OF BAR 4 OF pop_02j ;
.NOT. '#' $ vacceso
SET SKIP OF BAR 5 OF pop_02j ;
.NOT. '$' $ vacceso
SET SKIP OF BAR 6 OF pop_02j ;
.NOT. '%' $ vacceso
SET SKIP OF BAR 7 OF pop_02j ;
.NOT. '^' $ vacceso
SET SKIP OF BAR 8 OF pop_02j ;
.NOT. '&' $ vacceso
SET SKIP OF BAR 9 OF pop_02j ;
.NOT. '*' $ vacceso
SET SKIP OF BAR 10 OF pop_02j ;
.NOT. '(' $ vacceso
SET SKIP OF BAR 11 OF pop_02j ;
.NOT. ')' $ vacceso
SET SKIP OF BAR 12 OF pop_02j ;
.NOT. '-' $ vacceso
SET SKIP OF BAR 14 OF pop_02j ;
.NOT. '+' $ vacceso
SET SKIP OF BAR 15 OF pop_02j ;
.NOT. '=' $ vacceso
SET SKIP OF BAR 16 OF pop_02j ;
.NOT. '_' $ vacceso
SET SKIP OF BAR 17 OF pop_02j ;
.NOT. '/' $ vacceso
SET SKIP OF BAR 18 OF pop_02j ;
.NOT. '›' $ vacceso
vopcion = .T.
FOR i = 1 TO 7
     IF  .NOT. SKPBAR('pop_02b',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 1 OF menu1 vopcion
vopcion = .T.
FOR i = 1 TO 2
     IF  .NOT. SKPBAR('pop_02cc',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 8 OF menu1 vopcion
vopcion = .T.
FOR i = 1 TO 7
     IF  .NOT. SKPBAR('pop_02d',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 2 OF menu1 vopcion
vopcion = .T.
FOR i = 1 TO 3
     IF  .NOT. SKPBAR('pop_02f',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 3 OF menu1 vopcion
vopcion = .T.
FOR i = 1 TO 6
     IF  .NOT. SKPBAR('pop_02g',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 4 OF menu1 vopcion
vopcion = .T.
FOR i = 1 TO 7
     IF  .NOT. SKPBAR('pop_02h',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 5 OF menu1 vopcion
vopcion = .T.
FOR i = 1 TO 17
     IF  .NOT. SKPBAR('pop_02j',  ;
         i)
          vopcion = .F.
          EXIT
     ENDIF
ENDFOR
SET SKIP OF BAR 6 OF menu1 vopcion
RETURN
*
FUNCTION lastday
PARAMETER zmes, zano
IF PARAMETERS() = 1
     zano = YEAR(DATE()) - 1900
ENDIF
vdia = '31'
vmes = PADL(ALLTRIM(STR(zmes)), 2,  ;
       '0')
IF  .NOT. BETWEEN(vmes, '01',  ;
    '12')
     RETURN CTOD('')
ENDIF
vano = ALLTRIM(STR(zano))
vcentury = SET('CENTURY')
IF  .NOT. BETWEEN(vano, '00',  ;
    '99')
     SET CENTURY ON
ENDIF
vret = CTOD(vdia + '/' + vmes +  ;
       '/' + vano)
DO WHILE EMPTY(vret)
     vdia = STR(VAL(vdia) - 1, 2)
     vret = CTOD(vdia + '/' +  ;
            vmes + '/' + vano)
ENDDO
SET CENTURY ON
RETURN vret
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

USE IN 1 ParMae ALIAS parma ORDER  ;
    ParMae1
USE IN 2 ArtMae ALIAS artmae  ;
    ORDER ArtMae1
USE IN 3 IteArt ALIAS iteart  ;
    ORDER IteArt2
USE IN 4 Cuentas ALIAS cuenta  ;
    ORDER Cuentas1
SELECT iteart
SET RELATION TO tipart + LEFT(codart,;
6) INTO artmae
PRIVATE vmens01, vmens02, vmens03,  ;
        vmens04, vmens05, vmens06,  ;
        vmens07, vmens08, lagr
vmens01 = 'Cat logo de Medicamentos '
vmens02 = 'Revisi¢n de Medicamentos'
vmens03 = 'C¢digo del Medicamento: '
vmens04 = 'Dicho Medicamento no fue encontrado.'
vmens05 = 'No existe Medicamento anterior.'
vmens06 = 'No existe Medicamento siguiente.'
vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste Medicamento?'
vmens08 = 'No hay registros para procesar'
SELECT iteart
GOTO TOP
DO inicia
DO pantalla
DO vista
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     ACTIVATE MENU nmenu
ENDDO
DO fin_opcion
RETURN
*
PROCEDURE inicia
ACTIVATE SCREEN
vtempo = ''
vtempo = PADC(vtempo, 79, ' ')
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01 +  ;
       '  ' + '[F8] Elimina Item'  ;
       FOOTER '' DOUBLE COLOR  ;
       SCHEME 10
DEFINE WINDOW wind_1 FROM 05, 00  ;
       TO 23, 63 TITLE  ;
       'Detalles: ' DOUBLE COLOR  ;
       SCHEME 10
DEFINE MENU nmenu COLOR SCHEME 3
DEFINE PAD revis OF nmenu PROMPT  ;
       '\<Revisa' AT 24, 00
DEFINE PAD busca OF nmenu PROMPT  ;
       '\<Busca' AT 24, 08
DEFINE PAD anter OF nmenu PROMPT  ;
       '\<Anterior' AT 24, 15
DEFINE PAD proxi OF nmenu PROMPT  ;
       '\<Siguiente' AT 24, 25
DEFINE PAD corri OF nmenu PROMPT  ;
       '\<Corrige' AT 24, 36
DEFINE PAD ingre OF nmenu PROMPT  ;
       '\<Ingresa' AT 24, 45
DEFINE PAD elimi OF nmenu PROMPT  ;
       '\<Elimina' AT 24, 54
DEFINE PAD lista OF nmenu PROMPT  ;
       '\<Lista ' AT 24, 63
DEFINE PAD termi OF nmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF nmenu DO revis
ON SELECTION PAD busca OF nmenu DO busca
ON SELECTION PAD anter OF nmenu DO anter
ON SELECTION PAD proxi OF nmenu DO proxi
ON SELECTION PAD corri OF nmenu DO corri
ON SELECTION PAD ingre OF nmenu DO ingre
ON SELECTION PAD elimi OF nmenu DO elimi
ON SELECTION PAD lista OF nmenu DO lista
ON SELECTION PAD termi OF nmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_0
CLEAR
@ 0, 45 SAY 'Fecha de Registro: '
@ 2, 1 SAY '             Tipo: '
@ 3, 1 SAY '   Grupo Generico:'
@ 4, 1 SAY '      Laboratorio:'
@ 5, 1 SAY '          Detalle:'
@ 6, 1 SAY '      Correlativo:'
@ 7, 1 SAY '      Descripci¢n:'
@ 8, 1 SAY '         Menudeo?:'
@ 9, 1 SAY '           Unidad:'
@ 10, 1 SAY '     Presentaci¢n:'
@ 11, 1 SAY '         Fraccion:'
@ 12, 1 SAY 'Producto Generico:'
RETURN
*
PROCEDURE vista
SELECT iteart
DO pantalla
IF EOF()
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 0, 65 SAY DTOC(m.fecreg)
@ 2, 21 SAY m.tipart
@ 3, 21 SAY val_gg1(21)
@ 4, 21 SAY val_esp('B' +  ;
  m.codgen + '.' + m.codcla,' ', ;
  21)
@ 5, 21 SAY m.coddet
@ 6, 21 SAY m.codart
@ 7, 21 SAY LEFT(m.descri, 40)
@ 8, 21 SAY m.lfracc
@ 9, 21 SAY m.coduni
@ 10, 21 SAY m.unifrac
@ 11, 21 SAY m.fraccion
@ 12, 21 SAY m.lgeneric
IF  .NOT. vflag $ 'J*'
     DO subopc
ENDIF
RETURN
*
PROCEDURE revis
ON KEY LABEL F10 KEYBOARD CHR(23)
ACTIVATE WINDOW wind_0
SELECT iteart
IF  .NOT. EOF()
     BROWSE NOOPTIMIZE FIELDS  ;
            descri :H =  ;
            'Medicamento' : 40,  ;
            artmae.descri :H =  ;
            'Laboratorio' : 40,  ;
            lfracc :H = 'Frac.',  ;
            fraccion :H =  ;
            'Cantidad' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE NOCLEAR  ;
            WINDOW wind_0  ;
            NOREFRESH
ELSE
     CLEAR
     @ 4, 25 SAY  ;
       'No hay Detalles de este Catalogo'
ENDIF
SELECT iteart
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
DEFINE POPUP plista FROM 20, 40  ;
       COLOR SCHEME c_popup
DEFINE BAR 1 OF plista PROMPT  ;
       '\<a. Por Codigo      '
DEFINE BAR 2 OF plista PROMPT  ;
       '\<b. Por Nombre      '
ON SELECTION POPUP plista DEACTIVATE POPUP
ACTIVATE POPUP plista
cord1 = ORDER()
DO CASE
     CASE BAR() = 1
          vbusca = SPACE(12)
          vnombre = 'Codigo :'
          SET ORDER TO IteArt1
     CASE BAR() = 2
          vbusca = SPACE(30)
          vnombre = 'Nombre : '
          SET ORDER TO IteArt2
     OTHERWISE
          vbusca = ''
          vnombre = ''
          SET ORDER TO
ENDCASE
IF LASTKEY() <> 27
     DEFINE WINDOW lista FROM 09,  ;
            12 TO 16, 68 FLOAT  ;
            TITLE  ;
            ' °° B£squeda °° '  ;
            DOUBLE COLOR SCHEME  ;
            5
     ACTIVATE WINDOW lista
     @ 3, 2 SAY vnombre GET  ;
       vbusca
     READ VALID val_read()
     DEACTIVATE WINDOW lista
ENDIF
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
ELSE
     SEEK UPPER(ALLTRIM(vbusca))
     IF  .NOT. FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ENDIF
ENDIF
SET ORDER TO &cOrd1
DO vista
RETURN
*
PROCEDURE anter
SELECT iteart
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  .NOT. BOF()
     SKIP -1
ENDIF
IF BOF()
     GOTO TOP
     DO standby WITH vmens05
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE proxi
SELECT iteart
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  .NOT. EOF()
     SKIP
ENDIF
IF EOF()
     DO standby WITH vmens06
     GOTO BOTTOM
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE corri
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
DO pantalla
SELECT iteart
SCATTER MEMVAR
@ 0, 45 SAY m.fecreg
@ 2, 21 GET m.tipart DISABLE
@ 3, 21 GET m.codgen WHEN .F.
@ 3, 25 SAY val_gg1(21)
@ 4, 21 GET m.codcla DISABLE
@ 4, 25 SAY val_esp('B' +  ;
  m.codgen + '.' + m.codcla,' ', ;
  23)
@ 5, 21 GET m.coddet DISABLE
@ 6, 21 GET m.codart DISABLE
@ 7, 21 GET m.descri FUNCTION  ;
  'S40' VALID cproper(m.descri)
@ 8, 21 GET m.lfracc DISABLE
@ 9, 21 GET m.coduni DISABLE
@ 10, 21 GET m.unifrac DISABLE
@ 11, 21 GET m.fraccion DISABLE
@ 12, 21 GET m.lgeneric PICTURE  ;
  '@M S,N'
READ VALID val_read()
IF LASTKEY() <> 27
     IF f_lock(1)
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH  ;
        'Cancela Informacion'
ENDIF
DEACTIVATE WINDOW wind_1
UNLOCK ALL
SELECT iteart
DO vista
RETURN
*
PROCEDURE ingre
ACTIVATE WINDOW wind_0
DO pantalla
SELECT iteart
SCATTER BLANK MEMVAR
m.codgen = '62'
m.tipart = 'B'
m.lfracc = 'N'
m.fraccion = 1
m.fecreg = DATE()
m.lgeneric = 'N'
@ 0, 45 SAY m.fecreg
@ 2, 21 SAY m.tipart
@ 3, 21 GET m.codgen VALID  ;
  val_gg1(21) WHEN .F.
@ 4, 21 GET m.codcla VALID  ;
  val_esp('B' + m.codgen + '.' +  ;
  m.codcla,' ',23) .AND.  ;
  val_det()
@ 5, 21 GET m.coddet DISABLE
@ 6, 21 GET m.codart DISABLE
@ 7, 21 GET m.descri FUNCTION  ;
  'S40' VALID cproper(m.descri)
@ 8, 21 GET m.lfracc PICTURE  ;
  '@M S,N' VALID vlot()
@ 9, 21 GET m.coduni VALID  ;
  cproper(m.coduni)
@ 10, 21 GET m.unifrac VALID  ;
  cproper(m.unifrac)
@ 11, 21 GET m.fraccion VALID  ;
  m.fraccion >= 1 WHEN m.lfracc =  ;
  'S'
@ 12, 21 GET m.lgeneric PICTURE  ;
  '@M S,N'
READ VALID val_read()
IF LASTKEY() <> 27
     IF f_appd()
          GATHER MEMVAR
          UNLOCK
     ENDIF
ELSE
     DO standby WITH  ;
        'Cancela Informacion'
ENDIF
DEACTIVATE WINDOW wind_1
SELECT iteart
FLUSH
DO vista
RETURN
*
PROCEDURE elimi
PRIVATE codart, lborra, cmsg
SELECT iteart
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
m.codart = codart
IF yesno( ;
   '¨ Desea ELIMINAR ‚ste Medicamento ?' ;
   )
     lborra = .T.
     USE IN 0 IteII ALIAS iteii  ;
         ORDER IteII3
     SELECT iteii
     IF SEEK(m.codart)
          cmsg = 'Este Medicamento existe en Inventario Inicial. no puede ser eliminado'
          lborra = .F.
     ENDIF
     USE
     USE IN 0 FIteOP ALIAS fiteop  ;
         ORDER FIteOP2
     SELECT fiteop
     IF SEEK(m.codart)
          cmsg = 'Este Medicamento existe en Orden de Pedido. no puede ser eliminado'
          lborra = .F.
     ENDIF
     USE
     USE IN 0 FIteOC ALIAS fiteoc  ;
         ORDER FIteOC2
     SELECT fiteoc
     IF SEEK(m.codart)
          cmsg = 'Este Medicamento existe en Orden de Compra. no puede ser eliminado'
          lborra = .F.
     ENDIF
     USE
     USE IN 0 FIteDon ALIAS  ;
         itedon ORDER FIteDon2
     SELECT itedon
     IF SEEK(m.codart)
          cmsg = 'Este Medicamento existe en Donaciones. no puede ser eliminado'
          lborra = .F.
     ENDIF
     USE
     IF lborra
          SELECT iteart
          DELETE NEXT 1
          SKIP -1
          IF BOF()
               GOTO TOP
          ENDIF
     ELSE
          DO standby WITH cmsg
     ENDIF
ENDIF
SELECT iteart
UNLOCK ALL
DO vista
RETURN
*
PROCEDURE lista
PRIVATE xord
SELECT iteart
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vreg = RECNO()
xord = ORDER()
SET ORDER TO IteArt1
xrep = 3
vord = 3
vtogg = 2
vtoge = 1
lmedgen = 0
m.codgen = '62'
m.coddet = SPACE(7)
mgg = ''
mge = SPACE(7)
okcancel = 1
DEFINE WINDOW wlista FROM 3, 15  ;
       TO 20, 70 FLOAT TITLE  ;
       'Listado de Articulos'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 00, 01 SAY  ;
  '               Reporte: ' GET  ;
  xrep FUNCTION  ;
  '^ Grupo Generico;Grupo espec¡fico;Detallado'
@ 06, 01 SAY  ;
  'Todas los Laboratorios: ' GET  ;
  vtoge SIZE 1, 10, 2 FUNCTION  ;
  '*RNH \<Si;\<No' WHEN xrep = 3  ;
  .AND. vtogg = 2
@ 07, 01 SAY  ;
  '           Laboratorio: ' GET  ;
  m.coddet VALID val_ge() WHEN  ;
  xrep = 3 .AND. vtogg = 2 .AND.  ;
  vtoge = 2
@ 09, 01 SAY  ;
  '          Ordenado por: ' GET  ;
  vord FUNCTION  ;
  '^ Codigo;Alfabeticamente'
@ 12, 01 SAY  ;
  '     Medicina Generica: ' GET  ;
  lmedgen FUNCTION '*C'
@ 15, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
DEACTIVATE WINDOW wlista
RELEASE WINDOW wlista
IF LASTKEY() <> 27 .AND. okcancel =  ;
   1
     DO CASE
          CASE xrep = 1
               SELECT parma
               SET FILTER TO tipo = 'CODGEB'
               GOTO TOP
               DO reporte WITH 2,  ;
                  'Medic1',  ;
                  'Reporte de Articulos - Gupos Gen‚ricos',  ;
                  2, .F., .T.
               SET FILTER TO
          CASE xrep = 2
               SELECT artmae
               IF vtogg = 2
                    SET FILTER TO artmae.codart;
= 'B' + m.codgen
               ELSE
                    SET FILTER TO artmae.codart;
= 'B'
               ENDIF
               DO reporte WITH 2,  ;
                  'Medic2',  ;
                  'Reporte de Articulos - Grupos Especificos',  ;
                  2, .F., .T.
               SET FILTER TO
          CASE xrep = 3
               ccadena = ''
               IF vtogg = 2
                    IF vtoge = 2
                         ccadena =  ;
                          'iteart.codart = ALLTRIM(m.coddet)'
                    ELSE
                         ccadena =  ;
                          'iteart.codart = ALLTRIM(m.codgen)'
                    ENDIF
               ENDIF
               ccadena = ccadena +  ;
                         ' AND lGeneric=' +  ;
                         IIF(lmedgen =  ;
                         1, "'S'",  ;
                         "'N'")
               SET FILTER TO &cCadena
               IF vord = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'Medic3',  ;
                       'Reporte de Articulos - Detallado',  ;
                       2, .F.,  ;
                       .T.
               ELSE
                    SET ORDER TO IteArt2
                    DO reporte  ;
                       WITH 2,  ;
                       'Medic4',  ;
                       'Reporte de Articulos - Detallado',  ;
                       2, .F.,  ;
                       .T.
               ENDIF
               SET FILTER TO
     ENDCASE
ELSE
     DO standby WITH  ;
        'Proceso Cancelado'
ENDIF
SELECT iteart
SET ORDER TO (xord)
GOTO vreg
KEYBOARD CHR(27)
DO vista
RETURN
*
FUNCTION wcodgen
mret = .F.
IF (xrep = 2 .OR. xrep = 3) .AND.  ;
   vtogg = 2
     m.codgen = '62'
     mret = .T.
ENDIF
SHOW GET m.codgen
RETURN mret
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RESTORE SCREEN FROM principal
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE vlot
IF m.lfracc = 'N'
     m.fraccion = 1
ENDIF
SHOW GET m.fraccion
RETURN
*

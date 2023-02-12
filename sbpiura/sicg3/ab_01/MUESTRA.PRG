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
vmens01 = 'Cat logo de Articulos '
vmens02 = 'Revisi¢n de Art¡culos'
vmens03 = 'C¢digo del Art¡culo: '
vmens04 = 'Dicho Art¡culo no fue encontrado.'
vmens05 = 'No existe Art¡culo anterior.'
vmens06 = 'No existe Art¡culo siguiente.'
vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste Art¡culo?'
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
       '   ' + '[F8] Elimina'  ;
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
@ 4, 1 SAY ' Grupo Especifico:'
@ 5, 1 SAY '          Detalle:'
@ 6, 1 SAY '      Correlativo:'
@ 7, 1 SAY '      Descripci¢n:'
@ 8, 1 SAY ' Fraccionamiento?:'
@ 9, 1 SAY '           Unidad:'
@ 10, 1 SAY '  Unidad Fraccion:'
@ 11, 1 SAY '         Fraccion:'
@ 12, 1 SAY '  Cuenta Contable:'
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
@ 8, 21 SAY m.lote
@ 9, 21 SAY m.coduni
@ 10, 21 SAY m.unifrac
@ 11, 21 SAY m.fraccion
@ 12, 21 SAY val_fun('Cuenta', ;
  'Cuenta',"Cuenta+' '+DescRi", ;
  m.cuenta,3,12,21)
IF  .NOT. vflag $ 'J*'
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
            'Articulos' : 40,  ;
            artmae.descri :H =  ;
            'Grupos' : 40, lote  ;
            :H = 'Lote', fraccion  ;
            :H = 'Cantidad'  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW wind_0  ;
            NOREFRESH
ELSE
     CLEAR
     @ 4, 25 SAY  ;
       'No hay Detalles de este Catalogo'
ENDIF
SELECT iteart
DO vista
ON KEY LABEL F8
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
@ 8, 21 GET m.lote DISABLE
@ 9, 21 GET m.coduni DISABLE
@ 10, 21 GET m.unifrac DISABLE
@ 11, 21 GET m.fraccion DISABLE
@ 12, 21 GET m.cuenta VALID  ;
  val_fun('Cuenta','Cuenta', ;
  "Cuenta+' '+DescRi",m.cuenta,1, ;
  12,21)
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
FUNCTION ingre
ACTIVATE WINDOW wind_0
DO pantalla
SELECT iteart
SCATTER BLANK MEMVAR
m.codgen = SPACE(2)
m.tipart = 'B'
m.lote = 'N'
m.fraccion = 1
m.fecreg = DATE()
@ 0, 45 SAY m.fecreg
@ 2, 21 SAY m.tipart
@ 3, 21 GET m.codgen VALID  ;
  val_gg1(21)
@ 4, 21 GET m.codcla VALID  ;
  val_esp('B' + m.codgen + '.' +  ;
  m.codcla,' ',23) .AND.  ;
  val_det()
@ 5, 21 GET m.coddet DISABLE
@ 6, 21 GET m.codart DISABLE
@ 7, 21 GET m.descri FUNCTION  ;
  'S40' VALID cproper(m.descri)
@ 8, 21 GET m.lote PICTURE  ;
  '@M S,N' VALID vlot()
@ 9, 21 GET m.coduni VALID  ;
  cproper(m.coduni)
@ 10, 21 GET m.unifrac VALID  ;
  cproper(m.unifrac)
@ 11, 21 GET m.fraccion VALID  ;
  m.fraccion >= 1 WHEN m.lote =  ;
  'S'
@ 12, 21 GET m.cuenta VALID  ;
  val_fun('Cuenta','Cuenta', ;
  "Cuenta+' '+DescRi",m.cuenta,1, ;
  12,21)
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
ACTIVATE WINDOW wind_1
SELECT iteart
SCATTER BLANK MEMVAR
m.tipart = 'B'
m.lote = 'N'
m.fraccion = 1
m.fecreg = DATE()
@ 0, 30 SAY 'Fecha de Registro: ' +  ;
  DTOC(m.fecreg)
@ 2, 1 SAY '             Tipo: ' +  ;
  m.tipart
@ 3, 1 SAY '   Grupo Generico:'  ;
  GET m.codgen VALID val_gg()
@ 4, 1 SAY ' Grupo Especifico:'  ;
  GET m.codcla VALID val_esp('B' +  ;
  m.codgen + '.' + m.codcla,' ', ;
  23) .AND. val_det()
@ 5, 1 SAY '          Detalle:'  ;
  GET m.coddet DISABLE
@ 6, 1 SAY '      Correlativo:'  ;
  GET m.codart DISABLE
@ 7, 1 SAY '      Descripci¢n:'  ;
  GET m.descri FUNCTION 'S40'  ;
  VALID cproper(m.descri)
@ 8, 1 SAY '             Lote:'  ;
  GET m.lote PICTURE '@M S,N'  ;
  VALID vlot()
@ 9, 1 SAY '           Unidad:'  ;
  GET m.coduni
@ 10, 1 SAY '  Unidad Fraccion:'  ;
  GET m.unifrac
@ 11, 1 SAY '         Fraccion:'  ;
  GET m.fraccion VALID m.fraccion >=  ;
  1 WHEN m.lote = 'S'
@ 12, 1 SAY '  Cuenta Contable:'  ;
  GET m.cuenta VALID  ;
  val_fun('Cuenta','Cuenta', ;
  "Cuenta+' '+DescRi",m.cuenta,1, ;
  12,20)
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
RETURN
IF m.lote = 'N'
     m.fraccion = 1
ENDIF
SHOW GET m.fraccion
RETURN
PRIVATE mret
mret = ''
IF EMPTY(m.codgen)
     mret = val_para(m.codgen, ;
            'CODGEB','A',20,30)
ELSE
     IF  .NOT. SEEK('CODGEBB' +  ;
         m.codgen, 'Parma')
          mret = val_para(m.codgen, ;
                 'CODGEB','A',20, ;
                 30)
     ENDIF
ENDIF
m.codgen = SUBSTR(parma.codigo, 2,  ;
           2)
RETURN
PARAMETER xcod, mvariable, mcol
PRIVATE mfiltro
malias = ALIAS()
v_fun = .F.
DO CASE
     CASE PARAMETERS() = 1
          mvariable = ' '
          mcol = 0
     CASE PARAMETERS() = 2
          mcol = 0
ENDCASE
SELECT artmae
GOTO TOP
IF  .NOT. EMPTY(xcod)
     SEEK xcod
     v_fun = FOUND()
ENDIF
IF EMPTY(xcod) .OR.  .NOT. v_fun
     GOTO TOP
     ACTIVATE SCREEN
     ON KEY LABEL F10 KEYBOARD CHR(23)
     DEFINE WINDOW _busart FROM 2,  ;
            01 TO 22, 78
     ACTIVATE WINDOW _busart
     BROWSE FIELDS codart :H =  ;
            'C¢digo', descri :H =  ;
            'Nombre' : 40,  ;
            descri1 :H =  ;
            'Descripcion' : 40  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            WINDOW _busart KEY  ;
            LEFT(xcod, 3) TITLE  ;
            '²²²² [F10] Selecciona ²²²²'  ;
            NOLGRID
     ON KEY LABEL F10
     DEACTIVATE WINDOW _busart
     RELEASE WINDOW _busart
     IF LASTKEY() <> 27
          m.codcla = SUBSTR(artmae.codart,  ;
                     5, 3)
          mdescri = descri
          v_fun = .T.
     ELSE
          v_fun = .F.
          ON KEY LABEL F10 KEYBOARD CHR(23)
          UNLOCK ALL
          SELECT (malias)
          RETURN v_fun
     ENDIF
ELSE
     mdescri = descri
ENDIF
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
SELECT (malias)
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mdescri
          RETURN .T.
     CASE mvariable == 'A'
          @ ROW(), mcol SAY  ;
            mdescri
          RETURN m.codcla
     CASE mvariable == 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescri
     CASE mvariable == 'D'
          RETURN mdescri
     CASE mvariable == 'Z'
          RETURN mdescri
     CASE mvariable == 'C'
          RETURN .T.
     OTHERWISE
          RETURN .T.
ENDCASE
RETURN
PRIVATE malias
malias = ALIAS()
SELECT iteart
xord = ORDER()
SET ORDER TO IteArt1
SEEK m.tipart + m.codgen + '.' +  ;
     m.codcla
lagr = .F.
IF FOUND()
     xcoddet = 0
     SCAN WHILE iteart.tipart +  ;
          iteart.codgen + '.' +  ;
          iteart.codcla =  ;
          m.tipart + m.codgen +  ;
          '.' + m.codcla
          xcoddet = xcoddet + 1
          IF xcoddet =  ;
             VAL(coddet)
          ELSE
               m.coddet = PADL(ALLTRIM(STR(xcoddet)),  ;
                          4,  ;
                          '0')
               lagr = .T.
               EXIT
          ENDIF
     ENDSCAN
     IF  .NOT. lagr
          m.coddet = PADL(ALLTRIM(STR(xcoddet +  ;
                     1)), 4,  ;
                     '0')
     ENDIF
ELSE
     m.coddet = '0001'
ENDIF
SET ORDER TO (xord)
m.codart = m.codgen + '.' +  ;
           m.codcla + '.' +  ;
           m.coddet
SHOW GETS
RETURN .T.
ACTIVATE WINDOW wind_1
SELECT iteart
SCATTER MEMVAR
@ 0, 30 SAY 'Fecha de Registro: ' +  ;
  DTOC(m.fecreg)
@ 2, 1 SAY '             Tipo:'  ;
  GET m.tipart DISABLE
@ 3, 1 SAY '   Grupo Generico:'  ;
  GET m.codgen DISABLE
@ 4, 1 SAY ' Gripo Especifico:'  ;
  GET m.codcla DISABLE
@ 5, 1 SAY '          Detalle:'  ;
  GET m.coddet DISABLE
@ 6, 1 SAY '      Correlativo:'  ;
  GET m.codart DISABLE
@ 7, 1 SAY '      Descripci¢n:'  ;
  GET m.descri FUNCTION 'S40'  ;
  VALID cproper(m.descri)
@ 8, 1 SAY '             Lote:'  ;
  GET m.lote DISABLE
@ 9, 1 SAY '           Unidad:'  ;
  GET m.coduni DISABLE
@ 10, 1 SAY '  Unidad Fraccion:'  ;
  GET m.unifrac DISABLE
@ 11, 1 SAY '         Fracci¢n:'  ;
  GET m.fraccion DISABLE
@ 12, 1 SAY '  Cuenta Contable:'  ;
  GET m.cuenta VALID  ;
  val_fun('Cuenta','Cuenta', ;
  "Cuenta+' '+DescRi",m.cuenta,1, ;
  12,20)
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
RETURN
*
PROCEDURE elimi
SELECT iteart
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ELIMINAR ‚ste Art¡culo ?' ;
           )
velimina = .T.
cart = codart
IF velimina
     USE IN 0 IteII ALIAS iteii  ;
         ORDER IteII3
     SELECT iteii
     IF SEEK(cart)
          DO standby WITH  ;
             'Existe un Item con este articulo en Inv. Inicial. No se puede Eliminar este Articulo'
          velimina = .F.
     ELSE
          velimina = .T.
     ENDIF
     USE IN iteii
     IF velimina
          USE IN 0 IteGI ALIAS  ;
              itegi ORDER IteGI2
          SELECT itegi
          IF SEEK(cart)
               DO standby WITH  ;
                  'Existe un Item con este articulo en G/I. No se puede Eliminar este Articulo'
               velimina = .F.
          ELSE
               velimina = .T.
          ENDIF
          USE IN itegi
     ENDIF
     IF velimina
          USE IN 0 ItePeco ALIAS  ;
              itepeco ORDER  ;
              ItePeco2
          SELECT itepeco
          IF SEEK(cart)
               DO standby WITH  ;
                  'Existe un Item con este articulo en Pecosas. No se puede Eliminar este Articulo'
               velimina = .F.
          ELSE
               velimina = .T.
          ENDIF
          USE IN itepeco
     ENDIF
     IF velimina
          USE IN 0 IteNea ALIAS  ;
              itenea ORDER  ;
              IteNea2
          SELECT itenea
          IF SEEK(cart)
               DO standby WITH  ;
                  'Existe un Item con este articulo en G/I. No se puede Eliminar este Articulo'
               velimina = .F.
          ELSE
               velimina = .T.
          ENDIF
          USE IN itenea
     ENDIF
     IF velimina
          SELECT iteart
          SET ORDER TO IteArt1
          IF SEEK('B' + cart)
               DELETE NEXT 1
               SKIP -1
               IF BOF()
                    GOTO TOP
               ENDIF
          ENDIF
          SET ORDER TO ITEART2
     ELSE
          DO standby WITH  ;
             'No Puede Borrar este Articulo por encontrarse registrado en otro documento'
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
vtogg = 1
vtoge = 1
m.codgen = SPACE(3)
m.codcla = SPACE(7)
mgg = ''
mge = SPACE(7)
okcancel = 1
DEFINE WINDOW wlista FROM 3, 15  ;
       TO 20, 70 FLOAT TITLE  ;
       'Listado de Articulos'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 01, 01 SAY  ;
  '                   Reporte: '  ;
  GET xrep FUNCTION  ;
  '^ Grupo Generico;Grupo espec¡fico;Detallado'
@ 05, 01 SAY  ;
  'Todas los Grupos Genericos: '  ;
  GET vtogg SIZE 1, 10, 2  ;
  FUNCTION '*RNH \<Si;\<No' WHEN  ;
  BETWEEN(xrep, 2, 3)
@ 07, 01 SAY  ;
  '                     Grupo: '  ;
  GET m.codgen VALID val_gg()  ;
  WHEN BETWEEN(xrep, 2, 3) .AND.  ;
  vtogg = 2
@ 09, 01 SAY  ;
  '   Todas los Grupos Espec.: '  ;
  GET vtoge SIZE 1, 10, 2  ;
  FUNCTION '*RNH \<Si;\<No' WHEN  ;
  xrep = 3 .AND. vtogg = 2
@ 11, 01 SAY  ;
  '                    Grupo : '  ;
  GET mge VALID val_esp('B' +  ;
  m.codgen + '.' + m.codcla,' ', ;
  23) WHEN xrep = 3 .AND. vtogg =  ;
  2 .AND. vtoge = 2
@ 14, 10 GET okcancel DEFAULT 1  ;
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
                  'Artic1',  ;
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
                  'Artic2',  ;
                  'Reporte de Articulos - Grupos Especificos',  ;
                  2, .F., .T.
               SET FILTER TO
          CASE xrep = 3
               IF vtogg = 2
                    IF vtoge = 2
                         SET FILTER TO;
iteart.codart = m.codgen + m.coddet
                    ELSE
                         SET FILTER TO;
iteart.codart = m.codgen
                    ENDIF
               ENDIF
               DO reporte WITH 2,  ;
                  'Artic3',  ;
                  'Reporte de Articulos - Detallado',  ;
                  2, .F., .T.
               SET FILTER TO
     ENDCASE
ENDIF
SELECT iteart
SET ORDER TO (xord)
GOTO vreg
KEYBOARD CHR(27)
DO vista
RETURN
RETURN
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
PRIVATE malias
ON KEY LABEL F5
malias = ALIAS()
SELECT iteart
IF  .NOT. USED('Cuenta')
     USE IN 0 Cuentas ALIAS  ;
         cuenta ORDER Cuentas1
ENDIF
DEFINE WINDOW w_art FROM 05, 05  ;
       TO 20, 70 TITLE  ;
       'Ingresando Articulo'  ;
       DOUBLE COLOR SCHEME 5
m.tipart = 'B'
m.codgen = SPACE(2)
m.codcla = SPACE(3)
m.coddet = SPACE(4)
m.codart = SPACE(12)
m.descri = SPACE(60)
m.coduni = SPACE(10)
m.unifrac = SPACE(10)
m.lote = SPACE(1)
m.fraccion = 0
m.cuenta = SPACE(10)
ACTIVATE WINDOW w_art
@ 01, 1 SAY '             Tipo: ' +  ;
  m.tipart
@ 02, 1 SAY '   Grupo Generico:'  ;
  GET m.codgen VALID val_gg()
@ 03, 1 SAY ' Grupo Especifico:'  ;
  GET m.codcla VALID val_esp('B' +  ;
  m.codgen + '.' + m.codcla,' ', ;
  23) .AND. val_det()
@ 04, 1 SAY '          Detalle:'  ;
  GET m.coddet DISABLE
@ 05, 1 SAY '      Correlativo:'  ;
  GET m.codart DISABLE
@ 06, 1 SAY '      Descripci¢n:'  ;
  GET m.descri FUNCTION 'S40'  ;
  VALID cproper(m.descri)
@ 07, 1 SAY '             Lote:'  ;
  GET m.lote PICTURE '@M S,N'  ;
  VALID val_lote1()
@ 08, 1 SAY '           Unidad:'  ;
  GET m.coduni
@ 09, 1 SAY '  Unidad Fraccion:'  ;
  GET m.unifrac
@ 10, 1 SAY '         Fraccion:'  ;
  GET m.fraccion VALID (m.lote =  ;
  'S' .AND. m.fraccion >= 1) .OR.  ;
  (m.lote = 'N' .AND. m.fraccion =  ;
  1)
@ 11, 1 SAY '  Cuenta Contable:'  ;
  GET m.cuenta VALID  ;
  val_fun('Cuenta','Cuenta', ;
  "Cuenta+' '+DescRi",m.cuenta,2, ;
  11,20)
READ VALID val_read()
DEACTIVATE WINDOW w_art
RELEASE WINDOW w_art
IF LASTKEY() <> 27
     IF f_appd()
          GATHER MEMVAR
     ENDIF
ENDIF
USE IN cuenta
SELECT (malias)
ON KEY LABEL F5 DO Agr_Art
RETURN
*
PROCEDURE vlot
IF m.lote = 'N'
     m.fraccion = 1
ENDIF
SHOW GET m.fraccion
RETURN
*

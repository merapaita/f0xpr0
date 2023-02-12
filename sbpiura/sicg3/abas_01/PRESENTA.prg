DEACTIVATE WINDOW ALL
SET DATE DMY
SET TALK OFF
SET ECHO OFF
SET SAFETY OFF
SET DELETED ON
SET ESCAPE OFF
SET CLOCK ON
CLOSE DATABASES
SET CURSOR ON
CLOSE INDEX
USE IN 1 Iteart ALIAS iteart  ;
    ORDER IteArt3
USE IN 2 stock ALIAS stock ORDER  ;
    Stockx
ok = 0
orco = 0
depa = 0
sumcanti = 0
saldoz = 0
puntero = 0
nume = 1
acu = 2
SELECT iteart
SET ORDER TO ITEART3
SET RELATION TO codart INTO stock ADDITIVE
SELECT iteart
SET ORDER TO ITEART3
PRIVATE reg_editad, nuevo_reg,  ;
        dep_ant
m.reg_editad = .F.
m.nuevo_reg = .F.
m.dep_ant = SPACE(5)
PUBLIC art, queda, mmm, fechamr,  ;
       tipdocr, numdocr, fuenter,  ;
       cantidr, cosmedr
mmm = 'HOLA'
art = SPACE(11)
DEFINE WINDOW w_stockali FROM 14,  ;
       0 TO 23, 79 DOUBLE COLOR  ;
       SCHEME 10
DEFINE WINDOW wind_0 FROM 05, 05  ;
       TO 18, 70 TITLE  ;
       'AcTualizaci¢n de Stock'  ;
       DOUBLE COLOR SCHEME 16
GOTO TOP
SCATTER MEMVAR
anda1 = codart
SELECT stock
SEEK anda1
IF FOUND()
     DO ver_stocka
ELSE
     DO nohay
ENDIF
SELECT iteart
DO inicia
RETURN
*
PROCEDURE inicia
DEFINE WINDOW w_iteart FROM 0, 0  ;
       TO 13, 79 TITLE  ;
       'Registro de Articulos'  ;
       DOUBLE COLOR SCHEME 14
IF WVISIBLE('W_IteArt')
     ACTIVATE WINDOW SAME  ;
              w_iteart
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              w_iteart
ENDIF
DO pantalla
@ 11, 0 GET m.prim DEFAULT 1 SIZE  ;
  1, 9, 1 PICTURE  ;
  '@*VN \<Primero ' VALID  ;
  valor_btn('TOP') MESSAGE  ;
  'Primer registro'
@ 11, 9 GET m.sigue DEFAULT 1  ;
  SIZE 1, 10, 1 PICTURE  ;
  '@*VN \<Siguiente' VALID  ;
  valor_btn('NEXT') MESSAGE  ;
  'Siguiente Art¡culo'
@ 11, 20 GET m.anter DEFAULT 1  ;
  SIZE 1, 10, 1 PICTURE  ;
  '@*VN \<Anterior' VALID  ;
  valor_btn('PREV') MESSAGE  ;
  'Art¡culo anterior'
@ 11, 30 GET m.ulti DEFAULT 1  ;
  SIZE 1, 8, 1 PICTURE  ;
  '@*VN \<Ultimo' VALID  ;
  valor_btn('END') MESSAGE  ;
  'Ultimo Art¡culo'
@ 11, 38 GET m.encon DEFAULT 1  ;
  SIZE 1, 11, 1 PICTURE  ;
  '@*HN \<Buscar' VALID  ;
  valor_btn('LOCATE') MESSAGE  ;
  'Busca un Art¡culo.'
@ 11, 59 GET m.editar DEFAULT 1  ;
  SIZE 1, 9, 1 PICTURE  ;
  '@*HN \<Imprimir' VALID  ;
  valor_btn('IMPRIMIR') MESSAGE  ;
  'Presenta el Art¡culo por Tipo.'
@ 11, 68 GET m.cerrar DEFAULT 1  ;
  SIZE 1, 10, 1 PICTURE  ;
  '@*HN \<Cerrar' VALID  ;
  valor_btn('EXIT') MESSAGE  ;
  'Cierra la pantalla.'
SET CLOCK ON
IF  .NOT. WVISIBLE('W_IteArt')
     ACTIVATE WINDOW w_iteart
ENDIF
READ VALID .F. CYCLE ACTIVATE  ;
     readact() DEACTIVATE  ;
     readdeac() MODAL NOLOCK
RETURN
*
FUNCTION readdeac
IF reg_editad
     ACTIVATE WINDOW 'W_IteArt'
     WAIT WINDOW NOWAIT  ;
          'Por favor, finalice su edici¢n.'
ENDIF
IF  .NOT. WVISIBLE(WOUTPUT())
     CLEAR READ
     RETURN .T.
ENDIF
RETURN .F.
*
PROCEDURE readact
IF  .NOT. reg_editad
     SELECT iteart
     SHOW GETS
ENDIF
RETURN
*
PROCEDURE valor_btn
PARAMETER m.nombre_btn
ON KEY LABEL F9 DO ENTREMOV
DO CASE
     CASE m.nombre_btn = 'TOP'
          GOTO TOP
          WAIT WINDOW NOWAIT  ;
               'Principio de archivo.'
          ubica = codart
          DO pantalla
          DO detalle
     CASE m.nombre_btn = 'PREV'
          IF  .NOT. BOF()
               SKIP -1
               ubica = codart
               DO pantalla
               DO detalle
          ENDIF
          IF BOF()
               WAIT WINDOW NOWAIT  ;
                    'Principio de archivo.'
               GOTO TOP
               DO pantalla
          ENDIF
     CASE m.nombre_btn = 'NEXT'
          IF  .NOT. EOF()
               SKIP 1
               ubica = codart
               DO pantalla
               DO detalle
          ENDIF
          IF EOF()
               WAIT WINDOW NOWAIT  ;
                    'Fin de archivo.'
               GOTO BOTTOM
               DO pantalla
          ENDIF
     CASE m.nombre_btn = 'END'
          GOTO BOTTOM
          ubica = codart
          DO pantalla
          WAIT WINDOW NOWAIT  ;
               'Fin de archivo.'
          DO detalle
     CASE m.nombre_btn = 'LOCATE'
          DO buscaw
          ubica = codart
          DO pantalla2
          DO detalle
     CASE m.nombre_btn = 'SALDO'
          ubica = codart
          DO elegir
          DO detalle
     CASE m.nombre_btn =  ;
          'IMPRIMIR'
          DO imprix
     CASE m.nombre_btn = 'EXIT'  ;
          .AND. reg_editad
          IF nuevo_reg
               = edithand('CANCEL')
          ENDIF
          reg_editad = .F.
          nuevo_reg = .F.
          UNLOCK
          WAIT WINDOW NOWAIT  ;
               'Edici¢n cancelada.'
          _CUROBJ = OBJNUM(agregar)
     CASE m.nombre_btn = 'EXIT'
          CLEAR READ
          RELEASE WINDOW w_iteart
          RELEASE WINDOW  ;
                  w_stockali
          CLOSE DATABASES
          SET CLOCK OFF
          CLEAR
          RETURN
ENDCASE
SCATTER MEMVAR
SHOW GETS
RETURN
*
PROCEDURE pantalla
SET ORDER TO ITEART3
artb = codart
inicial = cantini
queda = cantini
ACTIVATE WINDOW w_iteart
@ 0, 2 SAY 'Tipo Articulo   : '  ;
  GET tipart DISABLE
@ 0, 40 SAY 'Cod. Articulo   : '  ;
  COLOR 'G+/r' GET codart  ;
  DISABLE
@ 1, 2 SAY 'Fecha Registro  : '  ;
  GET fecreg DISABLE
@ 1, 40 SAY 'Cantidad Inicial: '  ;
  GET cantini DISABLE
@ 2, 2 SAY 'Cod. Generico   : '  ;
  GET codgen DISABLE
@ 3, 2 SAY 'Descripci¢n     : '  ;
  GET descri DISABLE
@ 4, 2 SAY 'Cod. Clave      : '  ;
  GET codcla DISABLE
@ 5, 2 SAY 'Cod. Detenc.    : '  ;
  GET coddet DISABLE
@ 6, 2 SAY 'Cod. Unico      : '  ;
  GET coduni DISABLE
@ 7, 2 SAY 'Precio Unitario : '  ;
  GET preuni DISABLE
@ 8, 2 SAY 'Marca           : '  ;
  GET marca DISABLE
@ 9, 2 SAY 'Cod. Contable   : '  ;
  GET cod_cont DISABLE
@ 10, 2 SAY 'Cod. Partida    : '  ;
  GET codpart DISABLE
CLEAR
@ 0, 2 SAY 'Tipo Articulo   : ' +  ;
  tipart
@ 0, 40 SAY 'Cod. Articulo   : ' +  ;
  codart COLOR 'G+/r'
@ 1, 2 SAY 'Fecha Registro  : ' +  ;
  DTOC(fecreg)
@ 1, 40 SAY 'Cantidad Inicial: ' +  ;
  STR(cantini)
@ 2, 2 SAY 'Cod. Generico   : ' +  ;
  codgen
@ 3, 2 SAY 'Descripci¢n     : ' +  ;
  SUBSTR(descri, 1, 50)
@ 4, 2 SAY 'Cod. Clave      : ' +  ;
  codcla
@ 5, 2 SAY 'Cod. Detenc.    : ' +  ;
  coddet
@ 6, 2 SAY 'Cod. Unico      : ' +  ;
  coduni
@ 7, 2 SAY 'Precio Unitario : ' +  ;
  STR(preuni)
@ 8, 2 SAY 'Marca           : ' +  ;
  marca
@ 9, 2 SAY 'Cod. Contable   : ' +  ;
  cod_cont
@ 10, 2 SAY 'Cod. Partida    : ' +  ;
  codpart
SELECT stock
entra = 0
sale = 0
DO WHILE  .NOT. EOF()
     IF codart = artb .AND.  ;
        tipdoc = 'O/C'
          entra = entra +  ;
                  cantidad
     ELSE
          IF codart = artb .AND.  ;
             tipdoc = 'DESP'
               sale = sale +  ;
                      cantidad
          ENDIF
     ENDIF
     SKIP
ENDDO
@ 09, 50 CLEAR TO 09, 77
@ 7, 50 SAY 'Entradas(E) :' +  ;
  STR(entra)
@ 8, 50 SAY 'Salidas (S) :' +  ;
  STR(sale)
@ 9, 50 SAY 'Stock Actual:' +  ;
  STR((inicial + entra) - sale)
SELECT iteart
RETURN
*
PROCEDURE pantalla2
SET ORDER TO ITEART3
queda = cantini
@ 0, 2 SAY 'Tipo Articulo   : '  ;
  GET tipart DISABLE
@ 0, 40 SAY 'Cod. Articulo   : '  ;
  COLOR 'G+/r' GET codart  ;
  DISABLE
@ 1, 2 SAY 'Fecha Registro  : '  ;
  GET fecreg DISABLE
@ 1, 40 SAY 'Cantidad Inicial: '  ;
  GET cantini DISABLE
@ 2, 2 SAY 'Cod. Generico   : '  ;
  GET codgen DISABLE
@ 3, 2 SAY 'Descripci¢n     : '  ;
  GET descri DISABLE
@ 4, 2 SAY 'Cod. Clave      : '  ;
  GET codcla DISABLE
@ 5, 2 SAY 'Cod. Detenc.    : '  ;
  GET coddet DISABLE
@ 6, 2 SAY 'Cod. Unico      : '  ;
  GET coduni DISABLE
@ 7, 2 SAY 'Precio Unitario : '  ;
  GET preuni DISABLE
@ 8, 2 SAY 'Marca           : '  ;
  GET marca DISABLE
@ 9, 2 SAY 'Cod. Contable   : '  ;
  GET cod_cont DISABLE
@ 10, 2 SAY 'Cod. Partida    : '  ;
  GET codpart DISABLE
*
PROCEDURE panta3
artb = codart
inicial = cantini
queda = cantini
@ 0, 2 SAY 'Tipo Articulo   : '  ;
  GET tipart DISABLE
@ 0, 40 SAY 'Cod. Articulo   : '  ;
  COLOR 'G+/r' GET codart  ;
  DISABLE
@ 1, 2 SAY 'Fecha Registro  : '  ;
  GET fecreg DISABLE
@ 1, 40 SAY 'Cantidad Inicial: '  ;
  GET cantini DISABLE
@ 2, 2 SAY 'Cod. Generico   : '  ;
  GET codgen DISABLE
@ 3, 2 SAY 'Descripci¢n     : '  ;
  GET descri DISABLE
@ 4, 2 SAY 'Cod. Clave      : '  ;
  GET codcla DISABLE
@ 5, 2 SAY 'Cod. Detenc.    : '  ;
  GET coddet DISABLE
@ 6, 2 SAY 'Cod. Unico      : '  ;
  GET coduni DISABLE
@ 7, 2 SAY 'Precio Unitario : '  ;
  GET preuni DISABLE
@ 8, 2 SAY 'Marca           : '  ;
  GET marca DISABLE
@ 9, 2 SAY 'Cod. Contable   : '  ;
  GET cod_cont DISABLE
@ 10, 2 SAY 'Cod. Partida    : '  ;
  GET codpart DISABLE
SELECT stock
entra = 0
sale = 0
DO WHILE  .NOT. EOF()
     IF codart = artb .AND.  ;
        tipdoc = 'O/C'
          entra = entra +  ;
                  cantidad
     ELSE
          IF codart = artb .AND.  ;
             tipdoc = 'DESP'
               sale = sale +  ;
                      cantidad
          ENDIF
     ENDIF
     SKIP
ENDDO
@ 09, 50 CLEAR TO 09, 77
@ 7, 50 SAY 'Entradas(E) :' +  ;
  STR(entra)
@ 8, 50 SAY 'Salidas (S) :' +  ;
  STR(sale)
@ 9, 50 SAY 'Stock Actual:' +  ;
  STR((inicial + entra) - sale)
SELECT iteart
RETURN
*
PROCEDURE edithand
PARAMETER accion
DO CASE
     CASE m.accion = 'ADD'
          SCATTER BLANK MEMVAR
     CASE m.accion = 'SAVE'
          INSERT INTO IteArt FROM  ;
                 MEMVAR
     CASE m.accion = 'CANCEL'
ENDCASE
RETURN
*
PROCEDURE ver_stocka
SELECT stock
SEEK art
BROWSE NOOPTIMIZE FIELDS fechamov  ;
       :H = 'Fecha Mov.', tipdoc  ;
       :H = 'Dcto.', numdoc :H =  ;
       'N§Dcto.', fuente :H =  ;
       'Fuente', cant1 =  ;
       IIF(tipomov = 'E',  ;
       cantidad, '') :H =  ;
       'Entrada  ', cant2 =  ;
       IIF(tipomov = 'S',  ;
       cantidad, '') :H =  ;
       'Salida   ', cosmed :H =  ;
       'Precio' :P = '99,999.99',  ;
       canti = cosmed +  ;
       iteart.preuni :H =  ;
       'Precio promedio' :P =  ;
       '99,999.99', observa =  ;
       SUBSTR(stock.observa, 1,  ;
       20) :H = 'Observaci¢n'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NOCLEAR WINDOW w_stockali  ;
       NOWAIT TITLE  ;
       '®F9¯ Ver movimientos    REGISTRO DE STOCK    ®Esc¯ Salir'  ;
       TIMEOUT 0.005  NOREFRESH
ON KEY LABEL F10
SELECT iteart
RETURN
*
PROCEDURE nohay
ACTIVATE WINDOW w_stockali
CLEAR
@ 05, 25 SAY  ;
  'Art¡culo No Tiene Movimientos ...'
RETURN
*
PROCEDURE elegir
SELECT iteart
SET ORDER TO ITEART3
ACTIVATE SCREEN
entra = 0
sale = 0
artp = codart
ini = cantini
queda = cantini
SEEK codart
BROWSE NOOPTIMIZE FIELDS codart  ;
       :H = 'Codigo' :P =  ;
       '!!!!!!!!!!' :W = .F.,  ;
       descri :H = 'Descripci¢n'  ;
       : 44 :W = .F., cantini :H =  ;
       'Cantidad' :P =  ;
       '99,999.99', preuni :H =  ;
       'Precio' :P = '99,999.99'  ;
       NOMENU NOAPPEND NODELETE  ;
       NOCLEAR WINDOW wind_0  ;
       NOREFRESH COLOR SCHEME 05
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
RETURN
*
PROCEDURE buscaw
DEFINE POPUP _mm FROM 16, 54
DEFINE BAR 1 OF _mm PROMPT  ;
       ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT  ;
       ' Busqueda por \<Nombre '
DEFINE BAR 3 OF _mm PROMPT  ;
       ' T\<odos los Art¡culos '
ON SELECTION POPUP _mm DEACTIVATE POPUP
PRIVATE orden
orden = ORDER()
_cod = SPACE(11)
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
     CASE BAR() = 1
          ACTIVATE WINDOW _funbus
          @ 01, 02 SAY 'C¢digo: '  ;
            GET _cod PICTURE  ;
            '!!.!!!.!!!'
          READ
          DEACTIVATE WINDOW  ;
                     _funbus
          DO ver_stocka
          IF LASTKEY() <> 27
               SET ORDER TO ITEART3
               SEEK ALLTRIM(_cod)
          ENDIF
     CASE BAR() = 2
          ACTIVATE WINDOW _funbus
          _cod = SPACE(40)
          @ 01, 02 SAY 'Nombre: '  ;
            GET _cod PICTURE  ;
            '@S30'
          READ
          DEACTIVATE WINDOW  ;
                     _funbus
          IF LASTKEY() <> 27
               SET ORDER TO IteArt2
               SEEK UPPER(ALLTRIM(_cod))
          ENDIF
     CASE BAR() = 3
          SET ORDER TO IteArt3
          DEFINE WINDOW w_buscar  ;
                 FROM 0, 0 TO 20,  ;
                 60 GROW FLOAT  ;
                 CLOSE ZOOM  ;
                 SYSTEM COLOR  ;
                 SCHEME 05
          MOVE WINDOW w_buscar  ;
               CENTER
          BROWSE NOMENU NOEDIT  ;
                 NODELETE WINDOW  ;
                 w_buscar TITLE  ;
                 'Encontrar registro'
          RELEASE WINDOW w_buscar
     OTHERWISE
          RELEASE POPUP _mm
          SET ORDER TO (orden)
          RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
DO pantalla
DO suma2
RETURN
*
PROCEDURE suma2
inicial = cantini
SELECT stock
entra = 0
sale = 0
DO WHILE  .NOT. EOF()
     IF codart = _cod .AND.  ;
        tipdoc = 'O/C'
          entra = entra +  ;
                  cantidad
     ELSE
          IF codart = _cod .AND.  ;
             tipdoc = 'DESP'
               sale = sale +  ;
                      cantidad
          ENDIF
     ENDIF
     SKIP
ENDDO
@ 09, 50 CLEAR TO 09, 77
@ 7, 50 SAY 'Entradas(E) :' +  ;
  STR(entra)
@ 8, 50 SAY 'Salidas (S) :' +  ;
  STR(sale)
@ 9, 50 SAY 'Stock Actual:' +  ;
  STR((inicial + entra) - sale)
SELECT iteart
RETURN
*
PROCEDURE suma
inicial = cantini
SELECT stock
entra = 0
sale = 0
DO WHILE  .NOT. EOF()
     IF codart = codart .AND.  ;
        tipdoc = 'O/C'
          entra = entra +  ;
                  cantidad
     ELSE
          IF codart = codart  ;
             .AND. tipdoc =  ;
             'DESP'
               sale = sale +  ;
                      cantidad
          ENDIF
     ENDIF
     SKIP
ENDDO
@ 9, 50 CLEAR TO 09, 77
@ 7, 50 SAY 'Entradas(E) :' +  ;
  STR(entra)
@ 8, 50 SAY 'Salidas (S) :' +  ;
  STR(sale)
@ 9, 50 SAY 'Stock Actual:' +  ;
  STR((inicial + entra) - sale)
SELECT iteart
RETURN
*
PROCEDURE suma3
inicial = cantini
cod1 = codart
SELECT stock
entra = 0
sale = 0
DO WHILE  .NOT. EOF()
     IF codart = cod1 .AND.  ;
        tipdoc = 'O/C'
          entra = entra +  ;
                  cantidad
     ELSE
          IF codart = cod1 .AND.  ;
             tipdoc = 'DESP'
               sale = sale +  ;
                      cantidad
          ENDIF
     ENDIF
     SKIP
ENDDO
@ 7, 50 SAY 'Entradas(E) :' +  ;
  STR(entra)
@ 8, 50 SAY 'Salidas (S) :' +  ;
  STR(sale)
@ 9, 50 SAY 'Stock Actual:' +  ;
  STR((inicial + entra) - sale)
SELECT 1
RETURN
*
PROCEDURE entremov
busmovc = codart
ubica = codart
SELECT 2
SEEK busmovc
acu = 2
IF FOUND()
     BROWSE NOOPTIMIZE FIELDS  ;
            fechamov :H =  ;
            'Fecha Mov.', tipdoc  ;
            :H = 'Dcto.', numdoc  ;
            :H = 'N§Dcto.',  ;
            fuente :H = 'Fuente',  ;
            cant1 = IIF(tipomov =  ;
            'E', cantidad, '') :H =  ;
            'Entrada  ', cant2 =  ;
            IIF(tipomov = 'S',  ;
            cantidad, '') :H =  ;
            'Salida   ', cant3 =  ;
            val_can() :H =  ;
            'Saldo   ', cosmed :H =  ;
            'Precio' :P =  ;
            '99,999.99', prome =  ;
            (cosmed +  ;
            iteart.preuni) / acu  ;
            :H =  ;
            'Precio promedio',  ;
            observa =  ;
            SUBSTR(stock.observa,  ;
            1, 20) :H =  ;
            'Observaci¢n' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NOCLEAR WINDOW  ;
            w_stockali NOWAIT KEY  ;
            busmovc TITLE  ;
            '®F9¯ Ver movimientos    REGISTRO DE STOCK    ®Esc¯ Salir'  ;
            TIMEOUT 0.005   ;
            NOREFRESH
     acu = acu + 1
ELSE
     WAIT WINDOW NOWAIT  ;
          ' No Se Han Registrado Movimientos '
ENDIF
SELECT iteart
RETURN
*
FUNCTION val_can
SELECT iteart
IF stock.tipdoc = 'O/C' .AND.  ;
   FOUND() .AND. ok = 0
     orco = iteart.cantini +  ;
            stock.cantidad
     ok = 1
ELSE
     IF stock.tipdoc = 'DESP'
          depa = depa +  ;
                 stock.cantidad +  ;
                 orco
     ENDIF
ENDIF
SELECT stock
RETURN (orco + depa)
*
FUNCTION val_saldo
PARAMETER can, cantix
saldoz = saldoz + can + cantix
RETURN saldoz
*
PROCEDURE detalle
SELECT 2
SEEK ubica
IF FOUND()
     ACTIVATE WINDOW w_stockali
     DO ver_stocka
ELSE
     ACTIVATE WINDOW w_stockali
     CLEAR
     @ 05, 25 SAY  ;
       'Art¡culo No Tiene Movimientos ...'
     SET COLOR TO
ENDIF
SELECT 1
*
PROCEDURE imprix
DEFINE POPUP lismenu FROM 17, 54
DEFINE BAR 1 OF lismenu PROMPT  ;
       ' \<Art¡culo a la vista'
DEFINE BAR 2 OF lismenu PROMPT  ;
       ' \<Total de Articulos '
ON SELECTION POPUP lismenu DEACTIVATE;
POPUP
ACTIVATE POPUP lismenu
DO CASE
     CASE BAR() = 1
          SELECT iteart
          SET RELATION TO codart INTO;
stock
          SELECT stock
          SET FILTER TO tipdoc = 'INV'
          SEEK m.codart
          fechamr = fechamov
          tipdocr = tipdoc
          numdocr = numdoc
          fuenter = fuente
          cantidr = cantidad
          cosmedr = cosmed
          SET FILTER TO
          SET FILTER TO codart = m.codart;
.AND. tipdoc <> 'INV'
          GOTO TOP
          IF  .NOT. EOF()
               IF iteart.cantini =  ;
                  0
                    DO reporte  ;
                       WITH 2,  ;
                       'Imp2',  ;
                       ' Lista de un Producto ',  ;
                       1, .F.,  ;
                       .T.
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'Impini',  ;
                       ' *Lista de un Producto ',  ;
                       1, .F.,  ;
                       .T.
                    RETURN
                    SET FILTER TO
               ENDIF
               SET FILTER TO
          ELSE
               DO standby WITH  ;
                  'Este Art¡culo No Tiene Movimientos (E/S)...'
          ENDIF
          SET FILTER TO
     CASE BAR() = 2
          SELECT iteart
          GOTO TOP
          SET RELATION TO codart INTO;
stock
          SELECT iteart
          GOTO TOP
          IF  .NOT. EOF()
               DO reporte WITH 2,  ;
                  'Total',  ;
                  ' Lista de todos los Producto ',  ;
                  1, .F., .T.
          ENDIF
          SET FILTER TO
     OTHERWISE
ENDCASE
RELEASE POPUP lismenu
RETURN
*
FUNCTION val_entrad
SET FILTER TO
toma = ALIAS()
canti = 0
SELECT stock
SET FILTER TO stock.codart = iteart.codart;
.AND. stock.tipomov = 'E'
GOTO TOP
DO WHILE  .NOT. EOF()
     canti = canti +  ;
             stock.cantidad
     SKIP
ENDDO
SET FILTER TO
SELECT iteart
RETURN canti
*
FUNCTION val_salida
canti1 = 0
SELECT stock
GOTO TOP
SET FILTER TO stock.codart = iteart.codart;
.AND. stock.tipomov = 'S'
SEEK iteart.codart
DO WHILE  .NOT. EOF()
     canti1 = canti1 +  ;
              stock.cantidad
     SKIP
ENDDO
SET FILTER TO
SELECT iteart
RETURN canti1
*
FUNCTION val_conte
SELECT iteart
SET RELATION TO codart INTO stock
SELECT stock
SET FILTER TO stock.codart = iteart.codart
COUNT FOR stock.codart =  ;
      iteart.codart TO con
RETURN con
*
FUNCTION val_por1
nume = nume + 1
RETURN nume
*
FUNCTION val_anti
SELECT stock
SET FILTER TO stock.codart = iteart.codart
SKIP -1
SKIP 1
RETURN preanti
*
FUNCTION val_oc
SELECT iteart
SET RELATION TO codart INTO stock
SELECT stock
SET FILTER TO stock.codart = iteart.codart
COUNT FOR stock.codart =  ;
      iteart.codart .AND.  ;
      stock.tipdoc = 'O/C' TO  ;
      con1
RETURN con1
*
FUNCTION val_pe
SELECT iteart
SET RELATION TO codart INTO stock
SELECT stock
SET FILTER TO stock.codart = iteart.codart
COUNT FOR stock.codart =  ;
      iteart.codart .AND.  ;
      stock.tipdoc = 'DESP' TO  ;
      con2
RETURN con2
*
PROCEDURE vacio
RETURN
*

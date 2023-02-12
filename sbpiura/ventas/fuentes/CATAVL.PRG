CLOSE DATABASES
USE IN 1 Clientes ALIAS clien  ;
    ORDER Clientes1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
vmens01 = 'Registro de Avales'
vmens02 = ' Avales : REVISION '
vmens04 = 'Dicho Aval no fue encontrado'
vmens05 = 'No existe Aval anterior'
vmens06 = 'No existe Aval siguiente'
vmens07 = '¨ Desea Anular ‚ste Aval ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Aval ha sido anulado'
SELECT clien
SET FILTER TO tipcli = 'A'
GOTO BOTT
SCATTER BLANK MEMVAR
DO inicia
HIDE POPUP ALL
DO pantalla
DO vista
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     ACTIVATE MENU mmenu
ENDDO
DO fin_opcion
RETURN
*
PROCEDURE inicia
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       FOOTER  ;
       'F3 -> Act. Estado Aval'  ;
       DOUBLE COLOR SCHEME 10
DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis OF mmenu PROMPT  ;
       '\<Revisa' AT 24, 00
DEFINE PAD busca OF mmenu PROMPT  ;
       '\<Busca' AT 24, 08
DEFINE PAD anter OF mmenu PROMPT  ;
       '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu PROMPT  ;
       '\<Siguiente' AT 24, 25
DEFINE PAD corri OF mmenu PROMPT  ;
       '\<Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT  ;
       '\<Ingresa' AT 24, 45
DEFINE PAD elimi OF mmenu PROMPT  ;
       '\<Eliminar' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Listar ' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO corri
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD elimi OF mmenu DO elimi
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_0
CLEAR
@ 2, 10 SAY '       Codigo :'
@ 4, 10 SAY '  Nombre Aval :'
@ 6, 10 SAY '       D.N.I. :'
@ 8, 10 SAY '    Direcci¢n :'
@ 10, 10 SAY 'Ctro. Trabajo :'
RETURN
*
PROCEDURE vista
SELECT clien
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 1, 50 SAY estavl()
@ 2, 30 SAY m.codcli
@ 4, 30 SAY m.nomcli
@ 6, 30 SAY m.dnicli
@ 8, 30 SAY m.dircli
@ 10, 30 SAY m.centrb
IF  .NOT. vflag $ 'J*'
     DO subopc
ENDIF
RETURN
*
PROCEDURE revis
ON KEY LABEL F3
SELECT clien
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE FIELDS nomcli :H = 'Aval',  ;
       dircli :H = 'Direcci¢n'  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW wind_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELECT clien
DO vista
RETURN
*
PROCEDURE busca
ON KEY LABEL F3
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
cord1 = ORDER()
vnomcli = SPACE(50)
DEFINE POPUP plista FROM 20, 40  ;
       COLOR SCHEME c_popup
DEFINE BAR 1 OF plista PROMPT  ;
       '\<a. Por Codigo      '
DEFINE BAR 2 OF plista PROMPT  ;
       '\<b. Por Nombre      '
ON SELECTION POPUP plista DEACTIVATE POPUP
ACTIVATE POPUP plista
DO CASE
     CASE BAR() = 1
          vbusca = SPACE(6)
          vnombre = 'Codigo :'
          SET ORDER TO Clientes1
     CASE BAR() = 2
          vbusca = SPACE(30)
          vnombre = 'Nombre : '
          SET ORDER TO Clientes2
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
     SEEK ALLTRIM(vbusca)
     IF  .NOT. FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
SET ORDER TO &cOrd1
RETURN
*
PROCEDURE anter
SELECT clien
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
SELECT clien
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
ON KEY LABEL F3
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
SELECT clien
SCATTER MEMVAR
DO pantalla
@ 2, 30 SAY m.codcli
@ 4, 30 GET m.nomcli FUNCTION  ;
  '!S30'
@ 6, 30 GET m.dnicli PICTURE  ;
  '99999999'
@ 8, 30 GET m.dircli FUNCTION  ;
  '!S30'
@ 10, 30 GET m.centrb FUNCTION  ;
  '!S30'
READ VALID val_read()
IF LASTKEY() <> 27
     IF f_lock(1)
          SELECT clien
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT clien
DO vista
RETURN
*
PROCEDURE ingre
ON KEY LABEL F3
SELECT clien
vtemp = IIF(EOF(), -1, RECNO())
SCATTER BLANK MEMVAR
DO pantalla
= cor_clien()
@ 2, 30 SAY m.codcli
@ 4, 30 GET m.nomcli FUNCTION  ;
  '!S30'
@ 6, 30 GET m.dnicli PICTURE  ;
  '99999999'
@ 8, 30 GET m.dircli FUNCTION  ;
  '!S30'
@ 10, 30 GET m.centrb FUNCTION  ;
  '!S30'
READ VALID val_read()
IF LASTKEY() = 27
     IF vtemp = -1
          GOTO TOP
     ELSE
          GOTO vtemp
     ENDIF
     DO vista
     RETURN
ELSE
     m.tipcli = 'A'
     IF f_appd()
          GATHER MEMVAR
          SELECT parma
          SEEK 'CORRELCLIENT'
          REPLACE nument WITH  ;
                  parma.nument +  ;
                  1
     ENDIF
     UNLOCK ALL
ENDIF
SELECT clien
DO vista
RETURN
*
FUNCTION estavl
DO CASE
     CASE m.estado = '00'
          @ ROW(), COL() SAY  ;
            SPACE(9)
     CASE m.estado = '10'
          @ ROW(), COL() SAY  ;
            'CON SALDO' COLOR  ;
            SCHEME 5
ENDCASE
RETURN
IF SEEK('CORRELCLIENT' + m.codcli,  ;
   'Parma')
     m.codcli = PADL(ALLTRIM(STR(parma.nument +  ;
                1)), 6, '0')
ELSE
     DO standby WITH  ;
        'Parametro de correlativo no existe; favor avisar a sistemas'
ENDIF
IF EMPTY(m.codcli)
     DO standby WITH  ;
        'El Codigo esta vacio'
     RETURN .F.
ELSE
     nreg = RECNO()
     IF SEEK(m.codcli)
          DO standby WITH  ;
             'Ya esta Registrado este Cliente'
          RETURN .F.
     ENDIF
ENDIF
RETURN .T.
*
PROCEDURE elimi
ON KEY LABEL F3
DO standby WITH  ;
   'revisar antes opciones de difuntos y ocup. fut. '
SELECT clien
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ELIMINAR ‚ste Cliente ?' ;
           )
velimina = .T.
IF velimina
     USE IN 0 Ventas ALIAS ventas  ;
         ORDER Ventas3
     SELECT ventas
     IF SEEK(m.codcli)
          DO standby WITH  ;
             'Existe el Aval dentro de una venta. No se puede Eliminar.'
          velimina = .F.
     ELSE
          velimina = .T.
     ENDIF
     IF velimina
          SET ORDER TO Ventas2
          IF SEEK(m.codcli)
               DO standby WITH  ;
                  'Existe el Aval dentro de una venta como Cliente. No se puede Eliminar.'
               velimina = .F.
          ELSE
               velimina = .T.
          ENDIF
     ENDIF
     USE IN ventas
     IF velimina
          SELECT clien
          IF SEEK(m.codcli)
               DELETE NEXT 1
               SKIP -1
               IF BOF()
                    GOTO TOP
               ENDIF
          ENDIF
     ELSE
          DO standby WITH  ;
             'No Puede Eliminar este aval por encontrarse registrado con una venta'
     ENDIF
ENDIF
SELECT clien
UNLOCK ALL
DO vista
RETURN
*
PROCEDURE lista
PRIVATE vord, nreg, cord
SELECT clien
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
DEFINE WINDOW lis FROM 5, 15 TO  ;
       18, 65 FLOAT TITLE  ;
       'Listado de Clientes'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
vord = 1
cord = ORDER()
nreg = RECNO()
@ 02, 10 SAY 'Ordenado por : '  ;
  GET vord SIZE 1, 10, 6 FUNCTION  ;
  '^ Codigo;Nombre'
@ 10, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF LASTKEY() <> 27
     DO CASE
          CASE vord = 1
               SET ORDER TO Clientes1
          CASE vord = 2
               SET ORDER TO Clientes2
     ENDCASE
     DO reporte WITH 2, 'Aval',  ;
        'Listado de Avales'
ENDIF
SELECT clien
SET ORDER TO &cOrd
GOTO nreg
DO vista
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE actestavl
PRIVATE calias
calias = ALIAS()
USE IN 0 Ventas ORDER Ventas3
cest = m.estado
SELECT ventas
IF cest = '00'
     IF SEEK(m.codavl)
          SCAN WHILE codavl =  ;
               m.codavl
               IF estado = '20'
                    cest = '10'
               ENDIF
          ENDSCAN
     ENDIF
ELSE
     IF SEEK(m.codcli)
          SCAN WHILE codcli =  ;
               m.codcli
               IF estado <> '20'
                    cest = '00'
               ENDIF
          ENDSCAN
     ENDIF
ENDIF
USE IN ventas
SELECT (calias)
REPLACE estado WITH cest
DO vista
RETURN
*
FUNCTION fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_0
RELEASE MENU mmenu
RETURN
PARAMETER wfila, wcolumna
calias = ALIAS()
SELECT cement
lbusca = .F.
IF vtocem <> 1
     IF  .NOT. EMPTY(mcodcem)
          IF  .NOT. SEEK(mcodcem)
               lbusca = .T.
          ENDIF
     ELSE
          lbusca = .T.
     ENDIF
ENDIF
IF lbusca
     DEFINE POPUP v__xx FROM 2,  ;
            40 TO 17, 79 PROMPT  ;
            FIELDS nomcem
     ON SELECTION POPUP v__xx DEACTIVATE;
POPUP
     ACTIVATE POPUP v__xx
     RELEASE POPUP v__xx
ENDIF
mcodcem = cement.codcem
@ wfila, wcolumna SAY mcodcem +  ;
  '   ' + cement.nomcem
SET FILTER TO
SELECT (calias)
RETURN
calias = ALIAS()
SELECT cuartel
IF vtocem <> 1
     SET FILTER TO codcem = mcodcem
ENDIF
SELECT (calias)
RETURN
PARAMETER wfila, wcolumna
calias = ALIAS()
SELECT cuartel
lbusca = .F.
IF vtocem <> 1
     IF vtocuar <> 1
          IF  .NOT.  ;
              EMPTY(mcodcuar)
               IF  .NOT.  ;
                   SEEK(mcodcem +  ;
                   mcodcuar)
                    lbusca = .T.
               ENDIF
          ELSE
               lbusca = .T.
          ENDIF
     ENDIF
ENDIF
IF lbusca
     DEFINE POPUP v__xx FROM 2,  ;
            40 TO 17, 79 PROMPT  ;
            FIELDS nomcuar
     ON SELECTION POPUP v__xx DEACTIVATE;
POPUP
     ACTIVATE POPUP v__xx
     RELEASE POPUP v__xx
ENDIF
mcodcuar = cuartel.codcuar
@ wfila, wcolumna SAY mcodcuar +  ;
  '   ' + cuartel.nomcuar
SET FILTER TO
SELECT (calias)
RETURN
IF SEEK(m.codcem, 'Cement')
     mret = cement.nomcem
ELSE
     mret = 'Error; revise'
ENDIF
RETURN mret
*
PROCEDURE subopc
PRIVATE calias
calias = ALIAS()
USE IN 0 IteUsuOp ORDER IteUsuOp2
SET SKIP OF PAD revis OF mmenu;
 .NOT. SEEK(vusucla + '0043', 'IteUsuOp')
SET SKIP OF PAD busca OF mmenu;
 .NOT. SEEK(vusucla + '0044', 'IteUsuOp')
SET SKIP OF PAD anter OF mmenu;
 .NOT. SEEK(vusucla + '0045', 'IteUsuOp')
SET SKIP OF PAD proxi OF mmenu;
 .NOT. SEEK(vusucla + '0046', 'IteUsuOp')
SET SKIP OF PAD corri OF mmenu;
 .NOT. SEEK(vusucla + '0047', 'IteUsuOp')
SET SKIP OF PAD ingre OF mmenu;
 .NOT. SEEK(vusucla + '0048', 'IteUsuOp')
SET SKIP OF PAD elimi OF mmenu;
 .NOT. SEEK(vusucla + '0049', 'IteUsuOp')
SET SKIP OF PAD lista OF mmenu;
 .NOT. SEEK(vusucla + '0050', 'IteUsuOp')
USE IN iteusuop
SELECT (calias)
RETURN
*

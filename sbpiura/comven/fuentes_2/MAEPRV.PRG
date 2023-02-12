PARAMETER sistema
USE IN 1 MaePrv ALIAS maeprv ORDER MaePrv1
USE IN 2 parmae ALIAS parma    ORDER PARMAE1

vmens01 = ' Items de Sueldos'
vmens02 = ' Registro de Sueldos de Trabajadores'
vmens04 = 'Dicha persona no fue encontrado'
vmens05 = 'No existe personal anterior'
vmens06 = 'No existe personal siguiente'
vmens08 = 'No hay registros para procesar'
SELECT maeprv
GOTO BOTTOM
HIDE POPUP ALL
DO inicia
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa           Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_1 FROM 00, 00 TO 23, 79 TITLE vmens01 DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_cte1 FROM 00, 00 TO 23, 79 TITLE ' Revisi¢n del Personal de la S.B.P' DOUBLE COLOR SCHEME 10
DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis OF mmenu PROMPT '\<Revisa' AT 24, 00
DEFINE PAD busca OF mmenu PROMPT '\<Busca' AT 24, 08
DEFINE PAD anter OF mmenu PROMPT '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu PROMPT '\<Siguiente' AT 24, 25
DEFINE PAD corri OF mmenu PROMPT '\<Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT '\<Ingresa' AT 24, 45
DEFINE PAD lista OF mmenu PROMPT '\<Listar' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO corri
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_1
CLEAR
@ 1, 2 SAY '         Id. : '
@ 3, 2 SAY '   Tip. Doc. : '
@ 5, 2 SAY '      Codigo : '
@ 7, 2 SAY ' Descripci¢n : '
RETURN
*
PROCEDURE vista
SELECT maeprv
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@ 1, 22 SAY m.codprv
@ 3, 22 SAY val_para(m.tipdoc, 'TDIDEN','V',59,10)
@ 5, 22 SAY m.rucprv
@ 7, 22 SAY m.nomprv
RETURN
*
PROCEDURE revis
ACTIVATE SCREEN
IF EOF()
     DO standby WITH 'Archivo: vac¡o. No hay registros para procesar'
     RETURN
ENDIF
vtemp = RECNO()
DEFINE POPUP Busmenu FROM 15,50 SHADOW COLOR &L_COL
DEFINE BAR 1 OF busmenu PROMPT ' ordenado por:  \<Codigo  '
DEFINE BAR 2 OF busmenu PROMPT ' ordenado por:  \<R.U.C. '
DEFINE BAR 3 OF busmenu PROMPT ' ordenado por:  \<Descripci¢n'
ON SELECTION POPUP busmenu DEACTIVATE POPUP
ACTIVATE POPUP busmenu
DO CASE
     CASE BAR() = 1
          SET ORDER TO MaePrv1
     CASE BAR() = 2
          SET ORDER TO MaePrv2
     CASE BAR() = 3
          SET ORDER TO MaePrv3
ENDCASE
IF LASTKEY() = 27
     SET ORDER TO MaePrv1
     DO vista
     RETURN
ENDIF
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS rucprv :H = 'L.E.', nomprv :H = 'Descripci¢n'  ;
       NOMENU NOAPPEND NOEDIT NODELETE WINDOW wind_cte1
ON KEY LABEL F10
SET ORDER TO MaePrv1
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa           Listar  Termina '
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
DO vista
RETURN
*
PROCEDURE busca
ACTIVATE SCREEN
IF EOF()
     DO standby WITH 'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
DEFINE POPUP Busmenu FROM 15,54 SHADOW COLOR &L_COL
DEFINE BAR 1 OF busmenu PROMPT ' busca por:  \<Codigo  '
DEFINE BAR 2 OF busmenu PROMPT ' busca por:  \<R.U.C.  '
DEFINE BAR 3 OF busmenu PROMPT ' busca por:  \<Descripci¢n'
ON SELECTION POPUP busmenu DEACTIVATE;
POPUP
ACTIVATE POPUP busmenu
SELECT maeprv
vtemp = RECNO()
DO CASE
     CASE BAR() = 1
          ACTIVATE WINDOW standby
          STORE SPACE(8) TO vbusca
          @ 1, 2 SAY 'C¢digo: ' GET vbusca PICTURE '9999'
          READ
          DEACTIVATE WINDOW standby
          IF LASTKEY() <> 27
               vbusca = UPPER(ALLTRIM(vbusca))
               SET ORDER TO MaePrv1
          ENDIF
     CASE BAR() = 2
          ACTIVATE WINDOW standby
          STORE SPACE(11) TO vbusca
          @ 1, 1 SAY ' N§ Cuenta: ' GET vbusca PICTURE '99999999999'
          READ
          DEACTIVATE WINDOW standby
          IF LASTKEY() <> 27
               vbusca = TRIM(UPPER(vbusca))
               SET ORDER TO MaePrv2
          ENDIF
     CASE BAR() = 3
          ACTIVATE WINDOW standby
          STORE SPACE(50) TO vbusca
          @ 1, 1 SAY ' Nombre: ' GET vbusca PICTURE '@S30'
          READ
          DEACTIVATE WINDOW standby
          IF LASTKEY() <> 27
               vbusca = TRIM(UPPER(vbusca))
               SET ORDER TO MaePrv3
          ENDIF
     OTHERWISE
          RETURN
ENDCASE
IF EMPTY(vbusca) .OR. LASTKEY() = 27
     GOTO vtemp
ELSE
     SEEK vbusca
     IF  .NOT. FOUND()
          DO standby WITH 'Dicha Persona no fue encontrado'
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
SET ORDER TO MaePrv2
DO vista
RETURN
*
PROCEDURE anter
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
DO pantalla
SELECT maeprv
SCATTER MEMVAR
nreg = RECNO()
@ 1, 22 GET m.codprv WHEN .F.
@ 3, 22 GET m.tipdoc VALID val_para(m.tipdoc,'TDIDEN', ' ',22,27)
@ 5, 22 GET m.rucprv PICTURE '99999999999'
@ 7, 22 GET m.nomprv FUNCTION "S50"
READ VALID val_read() and val_ing()
IF LASTKEY() = 27
     GOTO nreg
     DO vista
     RETURN
ENDIF
IF LASTKEY() <> 27
     IF f_lock(1)
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH 'Proceso cancelado'
     SELECT maeprv
     GOTO nreg
ENDIF
UNLOCK ALL
SELECT maeprv
DO vista
RETURN
*
PROCEDURE ingre
DO pantalla
SELECT maeprv
SCATTER BLANK MEMVAR
nreg = RECNO()
@ 1, 22 GET m.codprv WHEN .F.
@ 3, 22 GET m.tipdoc VALID val_para(m.tipdoc,'TDIDEN', ' ',22,27)
@ 5, 22 GET m.rucprv PICTURE '99999999999'
@ 7, 22 GET m.nomprv FUNCTION "S50"
READ VALID val_read() AND val_ing()
IF LASTKEY() = 27
     GOTO nreg
     DO vista
     RETURN
ENDIF
IF LASTKEY() <> 27
    m.codprv = corprv()
    IF f_appd()
         GATHER MEMVAR
    ENDIF
ELSE
     DO standby WITH 'Proceso cancelado'
     SELECT maeprv
     GOTO nreg
ENDIF
UNLOCK ALL
SELECT maeprv
DO vista
RETURN

FUNCTION val_ing
*---------------
PRIVATE mret
mret = .T.
IF LASTKEY() <> 27
	IF (m.tipdoc = '6' .AND. LEN(ALLTRIM(m.rucprv)) <> 11) OR ;
	   (m.tipdoc = '1' .AND. LEN(ALLTRIM(m.rucprv)) <> 8) 
	   DO standby WITH 'Documento del proveedor Errado'
	   mret = .T.
	ENDIF
ELSE
   mret = .T.
ENDIF
RETURN mret

*
PROCEDURE lista
SELECT maeprv
IF EOF()
     RETURN
ENDIF
vtemp = RECNO()
GOTO TOP
SELECT maeprv
GOTO vtemp
RETURN
*
PROCEDURE termi
ven_accion = .F.
HIDE WINDOW wind_1
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_1
RELEASE WINDOW wind_cte1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*

PARAMETER sistema
USE IN 1 Compras ALIAS compras ORDER Compras01
USE IN 2 pComPag ALIAS pcompag ORDER pComPag01
USE IN 3 parmae ALIAS parma    ORDER PARMAE1
USE IN 4 MaePrv ALIAS maeprv   ORDER MaePrv2
PUBLIC m.tipcom
vmens01 = ' Registro de Compras'
vmens02 = ' Registro de Compras'
vmens04 = 'Dicha compra no fue encontrada'
vmens05 = 'No existe Compra anterior'
vmens06 = 'No existe Compra siguiente'
vmens08 = 'No hay registros para procesar'
SELECT compras
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
DEFINE WINDOW wind_0 FROM 00, 00 TO 23, 79 TITLE vmens01 DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00 TO 23, 79 TITLE vmens02 DOUBLE COLOR SCHEME 10
DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis OF mmenu PROMPT '\<Revisa' AT 24, 00
DEFINE PAD busca OF mmenu PROMPT '\<Busca' AT 24, 08
DEFINE PAD anter OF mmenu PROMPT '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu PROMPT '\<Siguiente' AT 24, 25
DEFINE PAD corri OF mmenu PROMPT '\<Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT '\<Ingresa' AT 24, 45
DEFINE PAD elimi OF mmenu PROMPT 'e\<Limina' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT '\<Listar' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT '\<Termina' AT 24, 71
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
@ 0, 50 SAY 'Estado : '
@ 1, 2 SAY '    Declaracion : '
@ 2, 2 SAY '    Fcha Compra : '
@ 2,45 SAY ' Fcha Cancelado : '

@ 3, 2 SAY '   Tip Comprob. : '
@ 5, 2 SAY 'Compr. sss-num : '
@ 5, 32 SAY '-'

@ 6, 2 SAY ' Doc.Referencia : '
@ 7, 2 SAY 'Compr. sss-num : '
@ 7, 32 SAY '-'

@ 9, 2 SAY '    Tip.Doc.Prv : '
@10, 2 SAY '  Num. Doc. Prv : '



@ 13, 2 SAY '  Destino Oper. : '
@ 14, 2 SAY '   Tipo Adquis. : '
@ 15, 2 SAY ' Base Imponible : '
@ 16, 2 SAY '            IGV : '
@ 17, 2 SAY '         ICBPER : '
@ 18, 2 SAY '          Otros : '
@ 19, 2 SAY '          Monto : '
@ 20, 2 SAY ' Cod. Doc. Ref. : '

RETURN
*
PROCEDURE vista
SELECT compras
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 0, 59 SAY val_para(m.estado, 'ESTCOM',' ',59,10)
@ 1, 22 SAY m.anyo
@ 1, 27 SAY m.mes
@ 1, 30 SAY m.periodo
@ 1, 39 SAY m.correl
@ 2, 22 SAY m.feccom
@ 2, 65 SAY m.feccan
@ 3, 22 SAY val_tcom(3,27)
@ 3, 50 SAY iif(!m.lrest,SPACE(14),'*RESTAURANTES*')
@ 5, 22 SAY m.serie
@ 5, 33 SAY m.numcom
@ 5, 45 SAY m.numcomf

@ 6, 22 SAY IIF(!EMPTY(m.tipdocrf), val_tdocrf(6,27), SPACE(40))
@ 7, 22 SAY m.serdocrf
@ 7, 33 SAY m.numdocrf
@ 7, 45 SAY m.fecdocrf

@ 9, 22 SAY val_para(m.tipdocpr, 'TDIDEN','V',22,40)
@10, 22 SAY m.numdocpr
@ 11, 22 SAY val_ruc(m.numdocpr, 12,22)

@ 13, 22 SAY val_para(m.desope, 'DESOPE','V',22,40)
@ 14, 22 SAY val_para(m.tipadq, 'TIPADQ',' ',22,40)
@ 15, 22 SAY m.basimp
@ 16, 22 SAY m.igv
@ 17, 22 SAY m.icbper
@ 18, 22 SAY m.otros
@ 19, 22 SAY m.monto
@ 20, 22 SAY m.numdr
RETURN
*
PROCEDURE revis
ACTIVATE SCREEN
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar'
     RETURN
ENDIF
vtemp = RECNO()
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS anyo :H = 'A', mes :H = 'M', periodo, correl,  ;
       tipcom, serie : 5, numcom, basimp, igv, otros, monto  ;
       NOMENU NOAPPEND NOEDIT NODELETE WINDOW wind_1
ON KEY LABEL F10
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa           Listar  Termina '
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F3
DO vista
RETURN
*
PROCEDURE busca
ACTIVATE SCREEN
IF EOF()
     DO standby WITH 'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
SELECT compras
vtemp = RECNO()
ACTIVATE WINDOW standby
STORE SPACE(8) TO vbusca
STORE SPACE(4) TO vanyo, vcorr
STORE SPACE(2) TO vmes
@ 1, 2 SAY  'Ano-mes-C¢digo:     -  -'
@ 1, 18 GET vanyo PICTURE '9999'
@ 1, 23 GET vmes PICTURE '99'
@ 1, 26 GET vcorr PICTURE '9999'
READ
DEACTIVATE WINDOW standby
IF LASTKEY() <> 27
     vbusca = vanyo + vmes + '00' + PADL(ALLTRIM(vcorr), 4, '0')
     SET ORDER TO Compras02
ENDIF
IF EMPTY(vbusca) .OR. LASTKEY() = 27
     GOTO vtemp
ELSE
     SEEK vbusca
     IF  !FOUND()
          DO standby WITH 'Dicha Compra no fue encontrada'
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
SET ORDER TO Compras01
DO vista
RETURN
*
PROCEDURE anter
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  !BOF()
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
IF  !EOF()
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
FUNCTION corri
*-------------
DO pantalla
SELECT compras
SCATTER MEMVAR
nreg = RECNO()
@ 0, 59 GET m.estado WHEN .F.
@ 1, 22 GET m.anyo WHEN .F.
@ 1, 27 GET m.mes WHEN .F.
@ 1, 30 GET m.periodo WHEN .F.
@ 1, 39 GET m.correl WHEN .F.
@ 2, 22 GET m.feccom
@ 2, 65 GET m.feccan
@ 3, 22 GET m.tipcom VALID val_tcom(3,27)
@ 3, 50 GET m.lrest  FUNCTION '*C Restaurantes'

@ 5, 22 GET m.serie
@ 5, 33 GET m.numcom
@ 5, 45 GET m.numcomf

@ 6, 22 GET m.tipdocrf VALID val_tdocrf(6,27) WHEN m.tipcom = '07'
@ 7, 22 GET m.serdocrf WHEN  m.tipcom = '07'
@ 7, 33 GET m.numdocrf WHEN  m.tipcom = '07'
@ 7, 55 GET m.fecdocrf WHEN  m.tipcom = '07'

@  9, 22 GET m.tipdocpr VALID val_para(m.tipdocpr,'TDIDEN', ' ',22,27)
@ 10, 22 GET m.numdocpr VALID valnumdoc()


@ 13, 22 GET m.desope VALID   val_para(m.desope,'DESOPE',' ',  22,27)
@ 14, 22 GET m.tipadq VALID   val_para(m.tipadq,'TIPADQ',' ',  22,27)
@ 15, 22 GET m.basimp

@ 16, 22 GET m.igv WHEN val_mto()
@ 17, 22 GET m.icbper WHEN  val_mto()
@ 18, 22 GET m.otros WHEN  val_mto()
@ 19, 22 GET m.monto WHEN  val_mto() .AND. .F.
@ 20, 22 GET m.numdr
READ VALID val_read() .AND. val_ing()
IF LASTKEY() = 27
     GOTO nreg
     DO vista
     RETURN
ENDIF
IF LASTKEY() <> 27
     IF f_lock(1)
          m.tusercr = 'C'
          m.usercr = vuser_id
          m.dusercr = DATE()
          m.husercr = TIME()
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH 'Proceso cancelado'
     SELECT ordse
     GOTO asd
ENDIF
UNLOCK ALL
SELECT compras
DO vista
RETURN
RETURN .T.
*
PROCEDURE valnumdoc
calias = ALIAS()
IF m.tipdocpr = '6' OR m.tipdocpr = '1'
     = val_ruc(m.numdocpr,12,22)
ENDIF
RETURN
*
PROCEDURE ingre
*--------------
DO pantalla
SELECT compras
SCATTER BLANK MEMVAR
nreg = RECNO()
m.anyo = STR(YEAR(DATE()), 4)
m.mes = PADL(MONTH(DATE()), 2, '0')
m.periodo = m.anyo + m.mes + '00'
m.im = 'I'
m.feccom = DATE()
m.feccan = DATE()
@ 0, 59 GET m.estado VALID val_para(m.estado,'ESTCOM',' ', 22,27) .AND. val_im()
@ 1, 22 GET m.anyo VALID valper()
@ 1, 27 GET m.mes VALID valper()
@ 1, 30 GET m.periodo VALID valcor()
@ 1, 39 GET m.correl WHEN .F.
@ 2, 22 GET m.feccom
@ 2, 65 GET m.feccan
@ 3, 22 GET m.tipcom VALID val_tcom(3,27)
@ 3, 50 GET m.lrest  FUNCTION '*C Restaurantes'
@ 5, 22 GET m.serie
@ 5, 33 GET m.numcom
@ 5, 45 GET m.numcomf

@ 6, 22 GET m.tipdocrf VALID val_tdocrf(6,27) WHEN m.tipcom = '07'
@ 7, 22 GET m.serdocrf WHEN  m.tipcom = '07'
@ 7, 33 GET m.numdocrf WHEN  m.tipcom = '07'
@ 7, 55 GET m.fecdocrf WHEN  m.tipcom = '07'


@  9, 22 GET m.tipdocpr VALID val_para(m.tipdocpr,'TDIDEN', ' ',22,27)
@ 10, 22 GET m.numdocpr VALID valnumdoc()
@ 13, 22 GET m.desope VALID   val_para(m.desope,'DESOPE',' ', 22,27)

@ 14, 22 GET m.tipadq VALID   val_para(m.tipadq,'TIPADQ',' ', 22,27)
@ 15, 22 GET m.basimp
@ 16, 22 GET m.igv WHEN val_mto()
@ 17, 22 GET m.icbper WHEN val_mto()
@ 18, 22 GET m.otros WHEN val_mto()
@ 19, 22 GET m.monto WHEN val_mto()
@ 20, 22 GET m.numdr
READ VALID val_read() .AND. val_ing()
IF LASTKEY() = 27
     GOTO nreg
     DO vista
     RETURN
ENDIF
IF LASTKEY() <> 27
     IF f_appd()
          m.tuser = 'I'
          m.user = vuser_id
          m.duser = DATE()
          m.huser = TIME()
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH 'Proceso cancelado'
     SELECT ordse
     GOTO asd
ENDIF
UNLOCK ALL
SELECT compras
DO vista
RETURN
*
FUNCTION val_ing
*---------------
PRIVATE mret
mret = .T.
IF LASTKEY() <> 27
   DO CASE
      CASE !BETWEEN(VAL(m.anyo), YEAR(DATE()) - 1, YEAR(DATE()))
         DO standby WITH 'A¤o de Declaracion errado'
         mret = .F.
      CASE  !BETWEEN(VAL(m.mes), 1, 12)
         DO standby WITH 'Mes de Daclaracion Errado'
         mret = .F.
      CASE EMPTY(m.correl)
         DO standby WITH 'Correlativo no debe estar vacio'
         mret = .F.
      CASE EMPTY(m.correl)
         DO standby WITH 'Correlativo no debe estar vacio'
         mret = .F.
      CASE errestado()
         mret = .F.
      CASE errfeccan()
         mret = .F.
      CASE errserie()
         mret = .F.
      CASE errnumcom()
         mret = .F.
      CASE EMPTY(m.numdocpr)
         DO standby WITH 'Documento del proveedor no debe estar vacio'
         mret = .F.
      CASE errdocprv()
         mret = .F.
      CASE EMPTY(m.monto)
         DO standby WITH 'Monto no debe ser 0'
         mret = .F.
      CASE m.basimp + m.igv + m.icbper + m.otros <> m.monto
         DO standby WITH 'Importes mal calculados'
         mret = .F.
      CASE EMPTY(m.numdr)
         DO standby WITH 'Debe Registrar el documento de Referencia'
         mret = .F.
      OTHERWISE
         mret = .T.
   ENDCASE
ELSE
   mret = .T.
ENDIF
RETURN mret
*
FUNCTION errdocprv
*-----------------
PRIVATE mret
mret = .F.
IF !((m.tipdocpr = '6' .AND. LEN(ALLTRIM(m.numdocpr)) = 11) OR ;
   (m.tipdocpr = '1' .AND. LEN(ALLTRIM(m.numdocpr)) = 8)) 
   DO standby WITH 'Documento del proveedor Errado'
   mret = .T.
ENDIF
IF (m.tipcom = '01' .OR. m.tipcom = '03') .AND. m.tipdocpr = '0'
   DO standby WITH ('Documento errado')
   mret = .T.
ENDIF
if (m.tipdocpr = '6')
	if !valNumRuc(m.numdocpr)
	   DO standby WITH ('El Ruc no es Valido')
	   mRet = .T.
	ENDIF
endif
RETURN mret
*
FUNCTION errestado
PRIVATE mret
mret = .F.
IF m.tipcom = '03'
   IF  !(m.estado = '0' .OR. estado = '9')
      DO standby WITH 'Estado Errado'
      mret = .T.
   ENDIF
ENDIF
RETURN mret
*
FUNCTION errserie
*----------------
PRIVATE mret
mret = .F.
IF EMPTY(m.serie)
	DO standby WITH 'Serie del Comprobante no debe estar vacio'
    mret = .T.
ELSE
    IF (m.tipcom = '01' .OR. m.tipcom = '03') .AND. LEN(ALLTRIM(m.serie)) <> 4
        DO standby WITH 'La Serie Debe tener 4 Digitos'
        mret = .T.
    ENDIF
ENDIF
RETURN mret
*
FUNCTION errnumcom
*-----------------
PRIVATE mret
mret = .F.
IF EMPTY(m.numcom)
	DO standby WITH 'Numero del Comprobante no debe estar vacio'
    mret = .T.
ENDIF

IF (m.tipcom = '01' .OR. m.tipcom = '03') .AND. !betw(LEN(ALLTRIM(m.numcom)),7,9)
    DO standby WITH 'Error en Longitud del Comprobante'
    mret = .T.
ENDIF

*      AT(' ', ALLTRIM(m.numcom)) <> 0 .OR. AT('-', m.numcom) <> 0
*         DO standby WITH 'Numero Comprobante Errado o Vacio'
*         mret = .F.


RETURN mret


FUNCTION errfeccom
PRIVATE mret
mret = .F.
IF EMPTY(m.feccom)
     DO standby WITH  ;
        'Fecha Comprobante no debe estar vacio'
     mret = .T.
ELSE
     IF m.estado = '1' .AND.  ;
        (PADL(MONTH(m.feccom), 2,  ;
        '0') <> SUBSTR(m.periodo,  ;
        5, 2) .OR.  ;
        STR(YEAR(m.feccom), 4) <>  ;
        LEFT(m.periodo, 4))
          DO standby WITH  ;
             'Fecha Comprobante no coincide con periodo'
          mret = .T.
     ENDIF
     xper = CTOD(PADL(DAY(DATE()),  ;
            2, '0') + '/' +  ;
            SUBSTR(m.periodo, 5,  ;
            2) + '/' +  ;
            LEFT(m.periodo, 4))
     IF m.estado = '6'
          IF (xper - m.feccom) >  ;
             365
               DO standby WITH  ;
                  'Comprobante esta fuera del periodo'
               mret = .T.
          ENDIF
     ENDIF
     IF m.estado = '7'
          IF (xper - m.feccom) <  ;
             365
               DO standby WITH  ;
                  'Comprobante esta fuera del periodo'
               mret = .T.
          ENDIF
     ENDIF
ENDIF
RETURN mret
*
FUNCTION errfeccan
PRIVATE mret
mret = .F.
IF  !EMPTY(m.feccan)
     IF m.estado = '1' .AND.  ;
        (PADL(MONTH(m.feccan), 2,  ;
        '0') <> SUBSTR(m.periodo,  ;
        5, 2) .OR.  ;
        STR(YEAR(m.feccan), 4) <>  ;
        LEFT(m.periodo, 4))
          DO standby WITH  ;
             'Fecha Cancelacion Comprobante no coincide con periodo'
          mret = .T.
     ENDIF
     xper = CTOD('01/' +  ;
            SUBSTR(m.periodo, 5,  ;
            2) + '/' +  ;
            LEFT(m.periodo, 4))
     IF m.estado = '6'
          IF (xper - m.feccan) >  ;
             365
               DO standby WITH  ;
                  'Cancelacion Comprobante esta fuera del periodo'
               mret = .T.
          ENDIF
     ENDIF
     IF m.estado = '7'
          IF (xper - m.feccan) <  ;
             365
               DO standby WITH  ;
                  'Comprobante esta fuera del periodo'
               mret = .T.
          ENDIF
     ENDIF
ELSE
     IF m.tipcom = '14'
          DO standby WITH  ;
             'Fecha no debe ser vacia cuando registramos recibos'
          mret = .T.
     ENDIF
ENDIF
RETURN mret
*
FUNCTION val_im
m.im = 'I'
IF m.estado = '9'
ENDIF
IF m.im = 'M'
     m.periodo = ''
ENDIF
SHOW GET m.periodo
RETURN .T.
*
FUNCTION valper
IF m.im = 'I'
     m.periodo = m.anyo + m.mes +  ;
                 '00'
     SHOW GET m.periodo
ELSE
ENDIF
RETURN .T.
*
PROCEDURE valcor
PRIVATE cord
cord = ORDER()
SET ORDER TO Compras02
IF m.im = 'I'
     m.correl = '0'
     IF SEEK(m.periodo)
          SCAN WHILE periodo =  ;
               m.periodo
               m.correl = correl
          ENDSCAN
     ENDIF
     m.correl = PADL(ALLTRIM(STR(VAL(m.correl) +  ;
                1)), 4, '0')
ELSE
     IF SEEK(m.periodo +  ;
        m.correl)
     ELSE
          = val_com()
     ENDIF
ENDIF
SET ORDER TO (cord)
SHOW GET m.correl
RETURN
*
FUNCTION val_tcom
PARAMETER _fil, _col
PRIVATE cali
cali = ALIAS()
SELECT pcompag
IF SEEK(m.tipcom)
     @ _fil, _col SAY  ;
       PADR(ALLTRIM(descri), 40,  ;
       ' ')
     mret = .T.
ELSE
     cord = ORDER()
     SET ORDER TO pComPag02
     m.tipcom = SPACE(2)
     DEFINE POPUP v__xx FROM 2,  ;
            40 TO 17, 79 PROMPT  ;
            FIELDS descri
     ON SELECTION POPUP v__xx DEACTIVATE;
POPUP
     ACTIVATE POPUP v__xx
     RELEASE POPUP v__xx
     IF LASTKEY() <> 27
          m.tipcom = codigo
          @ _fil, _col SAY  ;
            LEFT(descri, 40)
          mret = .T.
     ELSE
          mret = .F.
     ENDIF
     SET ORDER TO &cOrd
ENDIF
SELECT (cali)
RETURN mret
*
FUNCTION val_mto
*---------------
IF m.tipadq = '1'
	_igv = iif(m.lrest=.F.,0.18,0.10)
    m.igv = ROUND(m.basimp * _igv , 2)
ELSE
     m.igv = 0
ENDIF
m.monto = m.basimp + m.igv + m.icbper + m.otros
SHOW GET m.igv
SHOW GET m.monto
RETURN .T.
*
PROCEDURE elimi
PRIVATE lelimina
lelimina = .T.
IF yesno( ;
   'Desea Eliminar este registro' ;
   )
     IF  !ve_passw('ELICOM')
          DO standby WITH  ;
             'Proceso Cancelado'
          lelimina = .F.
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso Cancelado'
     lelimina = .F.
ENDIF
IF lelimina
     SELECT compras
     DELETE NEXT 1
     SKIP -1
     IF BOF()
          GOTO TOP
     ENDIF
ENDIF
DO vista
RETURN
*
PROCEDURE lista
*--------------
m.anyo = STR(YEAR(DATE()), 4)
m.mes = PADL((MONTH(DATE())), 2,'0')
DEFINE WINDOW wlista FROM 3, 15 TO 20, 70 FLOAT TITLE 'Proceso de Creacion de Ficheros' DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 03, 01 SAY '  A¤o del Proceso :' GET m.anyo
@ 05, 01 SAY '  Mes del proceso :' GET m.mes
@ 10, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
DO espera WITH 1, 'Procesando Ficheros'
IF LASTKEY() <> 27 .AND. okcancel <> 2
     SELECT compras
     SET RELATION TO numdocpr INTO maeprv
     nreg = RECNO()
     SET FILTER TO anyo = m.anyo .AND. mes = m.mes
     DO reporte WITH 2, 'RegCom', 'Reporte de Compras', 2, .F., .T.
     SET RELATION TO
     SET FILTER TO
     GOTO nreg
     DO espera WITH 2
ELSE
     DO standby WITH 'Proceso Cancelado.'
ENDIF
DO vista
RETURN
*
PROCEDURE termi
ven_accion = .F.
ON KEY LABEL F2
ON KEY LABEL F4
ON KEY LABEL F9
HIDE WINDOW wind_1
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_1
RELEASE WINDOW wind_3
RELEASE WINDOW wind_4
RELEASE WINDOW wind_cte1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION val_com
PRIVATE cord
ON KEY LABEL F5
ON KEY LABEL F8
malias = ALIAS()
v_fun = .F.
v_ent = .F.
SELECT compras
GOTO TOP
cord = ORDER()
SET ORDER TO Compras02
GOTO TOP
ACTIVATE SCREEN
ON KEY LABEL F10 KEYBOARD CHR(23)
DEFINE WINDOW _busart FROM 2, 01  ;
       TO 22, 78
ACTIVATE WINDOW _busart
BROWSE FIELDS periodo :H = 'Per',  ;
       tipcom :H = 'T.Com', x =  ;
       serie + '-' + numcom :H =  ;
       'C/P' : 15, basimp :H =  ;
       'BI', igv :H = 'IGV',  ;
       monto NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       _busart TITLE  ;
       '²²²² [F10] Selecciona   [F2] Buscar ²²²²   [F5] Agrega  ²²²²'
ON KEY LABEL F10
DEACTIVATE WINDOW _busart
RELEASE WINDOW _busart
IF LASTKEY() = 27
     v_fun = .F.
     v_ent = .F.
ELSE
     m.periodo = periodo
     m.correl = correl
     m.feccom = feccom
     m.feccan = feccan
     m.tipcom = tipcom
     m.serie = serie
     m.numcom = numcom
     m.numcomf = numcomf
     m.tipdocpr = tipdocpr
     m.numdocpr = numdocpr
     m.desope = desope
     m.tipadq = tipadq
     m.monto = monto
     m.basimp = basimp
     m.igv = igv
     SHOW GETS
     v_fun = .T.
     v_ent = .T.
ENDIF
SET ORDER TO (cord)
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
SELECT (malias)
IF  !v_ent
     RETURN v_fun
ELSE
     RETURN v_ent
ENDIF
RETURN .T.
*

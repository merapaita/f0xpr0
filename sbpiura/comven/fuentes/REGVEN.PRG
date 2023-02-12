PARAMETER sistema
USE IN 1 Ventas ALIAS ventas  ;
    ORDER Ventas01
USE IN 2 pComPag ALIAS pcompag  ;
    ORDER pComPag01
USE IN 3 parmae ALIAS parma ORDER  ;
    PARMAE1
USE IN 4 MaePrv ALIAS maeprv  ;
    ORDER MaePrv2
PUBLIC m.tipcom
vmens01 = 'Registro de Ventas'
vmens02 = 'Registro de Ventas'
vmens04 = 'Dicha Venta no fue encontrada'
vmens05 = 'No existe Venta anterior'
vmens06 = 'No existe Venta siguiente'
vmens08 = 'No hay registros para procesar'
SELECT ventas
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
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 23, 79 TITLE vmens02  ;
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
       'e\<Limina' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Listar' AT 24, 63
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
@ 0, 50 SAY 'Estado : '
@ 1, 2 SAY '    Declaracion : '
@ 3, 2 SAY '     Fcha Venta : '
@ 3, 45 SAY ' Fcha Cancelado : '
@ 4, 2 SAY '   Tip Comprob. : '
@ 5, 2 SAY 'Compr. sss-num. : '
@ 5, 32 SAY '-'
@ 6, 2 SAY 'Doc. Referencia : '
@ 7, 2 SAY '   Num.Doc.Ref. : '
@ 7, 45 SAY ' Fecha :'
@ 10, 2 SAY '    Tip.Doc.Cli : '
@ 11, 2 SAY '  Num. Doc. Cli : '
@ 13, 2 SAY '    Tipo Ventas : '
@ 14, 2 SAY ' Base Imponible : '
@ 15, 2 SAY '            IGV : '
@ 16, 2 SAY '          Monto : '
RETURN
*
PROCEDURE vista
SELECT ventas
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 0, 59 SAY val_para(m.estado, ;
  'ESTVEN',' ',59,10)
@ 1, 22 SAY m.anyo
@ 1, 27 SAY m.mes
@ 1, 31 SAY m.periodo
@ 1, 41 SAY m.correl
@ 3, 22 SAY m.fecven
@ 3, 63 SAY m.feccan
@ 4, 22 SAY val_tcom(4,27)
@ 5, 22 SAY m.serie
@ 5, 33 SAY m.numcom
@ 5, 45 SAY m.numcomf
@ 6, 22 SAY IIF( .NOT. EMPTY(m.tipdocrf), val_tdocrf(6,27), SPACE(40))
@ 7, 22 SAY m.serdocrf
@ 7, 33 SAY m.numdocrf
@ 7, 55 SAY m.fecdocrf
@ 10, 22 SAY val_para(m.tipdoccl, ;
  'TDIDEN','V',22,40)
@ 11, 22 SAY m.numdoccl
@ 13, 22 SAY val_para(m.tipven, ;
  'TIPADQ',' ',22,40)
@ 14, 22 SAY m.basimp
@ 15, 22 SAY m.igv
@ 16, 22 SAY m.monto
RETURN
*
PROCEDURE total
ACTIVATE WINDOW wind_3
SELECT totapor
SEEK m.lib_ele + vperiodo +  ;
     vmes_sue
@ 0, 0 SAY 'Ingresos :'
@ 1, 0 SAY 'S / A    :'
@ 2, 0 SAY 'Subsidios:'
@ 3, 0 SAY 'Egresos  :'
@ 4, 14 SAY '----------'
@ 5, 0 SAY 'Sueldo  :'
@ 6, 0 SAY 'C.E.M.  :'
@ 7, 0 SAY 'I.E.S.  :'
@ 0, 14 SAY tot_ing PICTURE  ;
  '9,999,999.99'
@ 1, 14 SAY tot_sa PICTURE  ;
  '9,999,999.99'
@ 2, 14 SAY tot_sub PICTURE  ;
  '9,999,999.99'
@ 3, 14 SAY tot_des PICTURE  ;
  '9,999,999.99'
@ 4, 14 SAY '----------'
@ 5, 14 SAY tot_ing + tot_sub +  ;
  tot_sa - tot_des PICTURE  ;
  '9,999,999.99'
@ 6, 14 SAY apo_01 PICTURE  ;
  '9,999,999.99'
@ 7, 14 SAY apo_02 PICTURE  ;
  '9,999,999.99'
SELECT permae
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
BROWSE FIELDS anyo :H = 'A', mes  ;
       :H = 'M', periodo, correl,  ;
       tipcom, serie : 5, numcom,  ;
       basimp : 8, igv : 7, monto  ;
       : 8 NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW wind_1
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
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
SELECT ventas
vtemp = RECNO()
ACTIVATE WINDOW standby
STORE SPACE(8) TO vbusca
STORE SPACE(4) TO vanyo, vcorr
STORE SPACE(2) TO vmes
@ 1, 2 SAY  ;
  'Ano-mes-C¢digo:     -  -'
@ 1, 18 GET vanyo PICTURE '9999'
@ 1, 23 GET vmes PICTURE '99'
@ 1, 26 GET vcorr PICTURE '9999'
READ
DEACTIVATE WINDOW standby
IF LASTKEY() <> 27
     vbusca = vanyo + vmes + '00' +  ;
              PADL(ALLTRIM(vcorr),  ;
              4, '0')
     SET ORDER TO Ventas02
ENDIF
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
     GOTO vtemp
ELSE
     SEEK vbusca
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Dicha Venta no fue encontrada'
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
SET ORDER TO Ventas01
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
FUNCTION corri
DO pantalla
SELECT ventas
SCATTER MEMVAR
nreg = RECNO()
@ 0, 59 GET m.estado WHEN .F.
@ 1, 22 GET m.anyo WHEN .F.
@ 1, 27 GET m.mes WHEN .F.
@ 1, 31 GET m.periodo WHEN .F.
@ 1, 41 GET m.correl WHEN .F.
@ 3, 22 GET m.fecven
@ 3, 63 GET m.feccan
@ 4, 22 GET m.tipcom VALID val_tcom(4,27)
@ 5, 22 GET m.serie
@ 5, 33 GET m.numcom
@ 5, 45 GET m.numcomf
@ 6, 22 GET m.tipdocrf VALID val_tdocrf(6,27) WHEN m.tipcom = '07'
@ 7, 22 GET m.serdocrf WHEN  m.tipcom = '07'
@ 7, 33 GET m.numdocrf WHEN  m.tipcom = '07'
@ 7, 55 GET m.fecdocrf WHEN  m.tipcom = '07'
@ 10, 22 GET m.tipdoccl VALID val_para(m.tipdoccl,'TDIDEN',' ',22,27)
@ 11, 22 GET m.numdoccl VALID valnumdoc()
@ 13, 22 GET m.tipven VALID   val_para(m.tipven,'TIPADQ',' ', 22,27)
@ 16, 22 GET m.monto VALID val_mto()
@ 14, 22 GET m.basimp VALID val_mto()
@ 15, 22 GET m.igv VALID val_mto()
READ VALID val_read() .AND.  ;
     val_ing()
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
     DO standby WITH  ;
        'Proceso cancelado'
     SELECT ordse
     GOTO asd
ENDIF
UNLOCK ALL
SELECT ventas
DO vista
RETURN .T.
*
PROCEDURE ingre
DO pantalla
SELECT ventas
SCATTER BLANK MEMVAR
nreg = RECNO()
m.anyo = STR(YEAR(DATE()), 4)
m.mes = PADL(MONTH(DATE()), 2,  ;
        '0')
m.periodo = m.anyo + m.mes + '00'
m.im = 'I'
m.fecven = DATE()
m.feccan = DATE()
@ 0, 59 GET m.estado VALID  ;
  val_para(m.estado,'ESTVEN',' ', ;
  59,10) .AND. val_im()
@ 1, 22 GET m.anyo VALID valper()
@ 1, 27 GET m.mes VALID valper()
@ 1, 31 GET m.periodo WHEN  ;
  valcor() .AND. .F.
@ 1, 41 GET m.correl WHEN .F.
@ 3, 22 GET m.fecven VALID  ;
  vfecven() .AND. valcor()
@ 3, 63 GET m.feccan
@ 4, 22 GET m.tipcom VALID  ;
  val_tcom(4,27)
@ 5, 22 GET m.serie
@ 5, 33 GET m.numcom
@ 5, 45 GET m.numcomf
@ 6, 22 GET m.tipdocrf VALID  ;
  val_tdocrf(6,27) WHEN m.tipcom =  ;
  '07'
@ 7, 22 GET m.serdocrf WHEN  ;
  m.tipcom = '07'
@ 7, 33 GET m.numdocrf WHEN  ;
  m.tipcom = '07'
@ 7, 55 GET m.fecdocrf WHEN  ;
  m.tipcom = '07'
@ 10, 22 GET m.tipdoccl VALID  ;
  val_para(m.tipdoccl,'TDIDEN', ;
  ' ',22,27)
@ 11, 22 GET m.numdoccl VALID  ;
  valnumdoc()
@ 13, 22 GET m.tipven VALID  ;
  val_para(m.tipven,'TIPADQ',' ', ;
  22,27)
@ 16, 22 GET m.monto VALID  ;
  val_mto()
@ 14, 22 GET m.basimp VALID  ;
  val_mto()
@ 15, 22 GET m.igv VALID  ;
  val_mto()
READ VALID val_read() .AND.  ;
     val_ing()
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
     DO standby WITH  ;
        'Proceso cancelado'
     SELECT ordse
     GOTO asd
ENDIF
UNLOCK ALL
SELECT ventas
DO vista
RETURN
*
PROCEDURE vfecven
IF m.estado = '8'
     m.periodo = STR(YEAR(m.fecven),  ;
                 4) +  ;
                 PADL(MONTH(m.fecven),  ;
                 2, '0') + '00'
     SHOW GET m.periodo
ENDIF
*
PROCEDURE valnumdoc
calias = ALIAS()
IF m.tipdoccl = '6' OR m.tipdoccl = '1'
     = val_ruc(m.numdoccl,12,22)
ENDIF
RETURN
*
FUNCTION val_ing
*---------------
mret = .F.
IF LASTKEY() <> 27
	DO CASE
    	CASE !BETWEEN(VAL(m.anyo), YEAR(DATE()) - 1, YEAR(DATE()))
    		DO standby WITH 'A¤o de Declaracion errado'
    		mret = .F.
    	CASE !BETWEEN(VAL(m.mes), 1, 12)
    		DO standby WITH 'Mes de Daclaracion Errado'
    		mret = .F.
    	CASE EMPTY(m.correl)
    		DO standby WITH 'Correlativo no debe estar vacio'
    		mret = .F.
    	CASE errfecven()
    		mret = .F.
    	CASE errfeccan()
    		mret = .F.
    	CASE EMPTY(m.serie)
    		DO standby WITH 'Serie del Comprobante no debe estar vacio'
    		mret = .F.
    	CASE EMPTY(m.numcom)
    		DO standby WITH 'Numero Comprobante no debe estar vacio'
    		mret = .F.
    	CASE errdocrf()
    		mret = .F.
    	CASE errdoccli()
    		mret = .F.
    	CASE EMPTY(m.monto) .AND. m.estado <> '2'
    		DO standby WITH 'Monto no debe ser 0'
    		mret = .F.
    	CASE m.basimp + m.igv <> m.monto
    		DO standby WITH 'Importes mal calculados'
    		mret = .F.
    	CASE m.estado = '2' .AND.  .NOT. EMPTY(m.monto)
    		DO standby WITH 'Documento Anulado, Monto debe ser 0'
    		mret = .F.
    	CASE m.tipven = '2' .AND.  .NOT. EMPTY(m.igv)
    		DO standby WITH 'Documento Exonerado, IGV debe ser 0'
    		mret = .F.
    	OTHERWISE
    		mret = .T.
     ENDCASE
ELSE
     mret = .T.
ENDIF
RETURN mret
*
FUNCTION errdocrf
PRIVATE mret
mret = .F.
IF m.tipcom = '07'
     DO CASE
          CASE EMPTY(m.tipdocrf)
               DO standby WITH  ;
                  'Tipo Doc. Referencia no debe estar vacio'
               mret = .T.
          CASE EMPTY(m.serdocrf)
               DO standby WITH  ;
                  'Serie Doc. Referencia no debe estar vacio'
               mret = .T.
          CASE EMPTY(m.numdocrf)
               DO standby WITH  ;
                  'Doc. Referencia no debe estar vacio'
               mret = .T.
          CASE EMPTY(m.fecdocrf)  ;
               .OR. m.fecdocrf >  ;
               m.fecven
               DO standby WITH  ;
                  'Fecha Doc. Referencia Errado'
               mret = .T.
     ENDCASE
ENDIF
RETURN mret
*
FUNCTION errdoccli
*-----------------
PRIVATE mret
mret = .F.
*SET STEP ON
IF EMPTY(m.numdoccl)
    DO standby WITH 'Documento del Cliente no debe estar vacio'
    mret = .T.
ELSE
    IF (m.tipdoccl = '6' .AND. LEN(ALLTRIM(m.numdoccl)) <> 11)
        DO standby WITH 'Documento del proveedor Errado'
        mret = .T.
    ENDIF
    IF (m.tipdoccl = '1' .AND. LEN(ALLTRIM(m.numdoccl)) <> 8)
        DO standby WITH 'Documento del proveedor Errado'
        mret = .T.
    ENDIF
    IF (m.tipcom = '01') .AND. m.tipdoccl = '0'
        DO standby WITH ('Documento errado')
        mret = .T.
    ENDIF
	if (m.tipdoccl = '6')
		if !valNumRuc(m.numdoccl)
		   	DO standby WITH ('El Ruc no es Valido')
			mRet = .T.
		ENDIF
	ENDIF
ENDIF
RETURN mret
*
FUNCTION errfecven
PRIVATE mret
mret = .F.
IF EMPTY(m.fecven)
     DO standby WITH  ;
        'Fecha Comprobante no debe estar vacio'
     mret = .T.
ELSE
     IF (m.estado = '1' .OR.  ;
        m.estado = '2') .AND.  ;
        (PADL(MONTH(m.fecven), 2,  ;
        '0') <> SUBSTR(m.periodo,  ;
        5, 2) .OR.  ;
        STR(YEAR(m.fecven), 4) <>  ;
        LEFT(m.periodo, 4))
          DO standby WITH  ;
             'Fecha Comprobante no coincide con periodo'
          mret = .T.
     ENDIF
     xper = CTOD('01/' +  ;
            SUBSTR(m.periodo, 5,  ;
            2) + '/' +  ;
            LEFT(m.periodo, 4))
     IF m.estado = '8'
          IF (xper - m.fecven) >  ;
             30
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
IF  .NOT. EMPTY(m.feccan)
     IF (m.estado = '1' .OR.  ;
        m.estado = '2') .AND.  ;
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
     IF m.estado = '8'
          IF (xper - m.feccan) >  ;
             30
               DO standby WITH  ;
                  'Cancelacion Comprobante esta fuera del periodo'
               mret = .T.
          ENDIF
     ENDIF
ENDIF
RETURN mret
*
PROCEDURE elimi
PRIVATE lelimina
lelimina = .T.
IF yesno( ;
   'Desea Eliminar este registro' ;
   )
     IF  .NOT. ve_passw('ELIVEN')
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
     SELECT ventas
     DELETE NEXT 1
     SKIP -1
     IF BOF()
          GOTO TOP
     ENDIF
ENDIF
DO vista
RETURN
*
FUNCTION val_im
m.im = 'I'
IF m.estado = '8'
     m.im = 'J'
ENDIF
IF m.estado = '9'
     m.im = 'M'
ENDIF
IF m.im = 'M'
     m.periodo = ''
ENDIF
SHOW GET m.periodo
SHOW GET m.periodo
RETURN .T.
RETURN .T.
*
FUNCTION valper
m.periodo = m.anyo + m.mes + '00'
SHOW GET m.periodo
RETURN .T.
*
PROCEDURE valcor
PRIVATE cord
cord = ORDER()
SET ORDER TO Ventas02
IF m.im = 'I' .OR. m.im = 'J'
     m.correl = '0'
     IF SEEK(m.periodo)
          SCAN WHILE periodo =  ;
               m.periodo
               m.correl = correl
          ENDSCAN
     ENDIF
     m.correl = PADL(ALLTRIM(STR(VAL(m.correl) +  ;
                1)), 4, '0')
     SHOW GET m.correl
ELSE
     IF SEEK(m.periodo +  ;
        m.correl)
     ELSE
          = val_ven()
     ENDIF
ENDIF
SET ORDER TO (cord)
RETURN
*
FUNCTION val_tcom
PARAMETER _fil, _col
PRIVATE cali
cali = ALIAS()
SELECT pcompag
IF SEEK(m.tipcom)
     @ _fil, _col SAY  ;
       ALLTRIM(descri)
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
DO CASE
     CASE m.estado = '2'
          m.basimp = 0
          m.igv = 0
          m.monto = 0
     CASE m.tipven = '2'
          m.igv = 0
          m.basimp = m.monto
     OTHERWISE
          m.basimp = ROUND(m.monto /  ;
                     1.18 , 2)
          m.igv = m.monto -  ;
                  m.basimp
ENDCASE
SHOW GET m.basimp
SHOW GET m.igv
SHOW GET m.monto
RETURN .T.
*
PROCEDURE lista
m.anyo = STR(YEAR(DATE()), 4)
m.mes = PADL((MONTH(DATE())), 2,  ;
        '0')
DEFINE WINDOW wlista FROM 3, 15  ;
       TO 20, 70 FLOAT TITLE  ;
       'Reporte de Registro de Ventas'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 03, 01 SAY  ;
  '  A¤o del Proceso :' GET  ;
  m.anyo
@ 05, 01 SAY  ;
  '  Mes del proceso :' GET  ;
  m.mes
@ 10, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
DO espera WITH 1,  ;
   'Procesando Ficheros'
IF LASTKEY() <> 27 .AND. okcancel <>  ;
   2
     SELECT ventas
     nreg = RECNO()
     SET FILTER TO anyo = m.anyo;
.AND. mes = m.mes
     DO reporte WITH 2, 'RegVen',  ;
        'Reporte de Compras', 2,  ;
        .F., .T.
     SET FILTER TO
     GOTO nreg
     DO espera WITH 2
ELSE
     DO standby WITH  ;
        'Proceso Cancelado.'
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
FUNCTION val_ven
PRIVATE cord
ON KEY LABEL F5
ON KEY LABEL F8
malias = ALIAS()
v_fun = .F.
v_ent = .F.
SELECT ventas
GOTO TOP
cord = ORDER()
SET ORDER TO Ventas02
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
     m.fecven = fecven
     m.feccan = feccan
     m.tipcom = tipcom
     m.serie = serie
     m.numcom = numcom
     m.numcomf = numcomf
     m.tipdoccl = tipdoc
     m.numdoccl = numdoccl
     m.tipven = tipven
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
IF  .NOT. v_ent
     RETURN v_fun
ELSE
     RETURN v_ent
ENDIF
RETURN .T.
*

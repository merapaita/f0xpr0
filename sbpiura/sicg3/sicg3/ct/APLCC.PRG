**--------------------------------------
** APLCC.PRG
** Programa de aplicaci¢n de cuentas contables 
** en documentos fuentes con la finalidad de 
** corregir los asientos generados en pecosas
** AUTOR : A.S. Marco Rodriguez Abad - 10/01/2005
** Estados : 00 Pendiente de Contabilizar
**           20 Contabilizado
**--------------------------------------
PUBLIC cProc,OkCancel,xAst

USE Parmae IN 4 ALIAS Parma	 ORDER TAG Parmae1

DO Pantalla
DO Escoge

IF !EMPTY(cProc)
	DO &cProc
ENDIF

RETURN

PROCEDURE Pantalla
*-----------------
DEFINE WINDOW wEscoge FROM 3,15 TO 18,65 FLOAT DOUBLE TITLE '  Documentos Fuentes  ' COLOR SCHEME 5
ACTIVATE WINDOW wEscoge

PUBLIC xPeriodo,xtipdoc,xcodDoc,xMes
xPeriodo = YEAR(DATE())
xTipDoc = SPACE(3)
xCodDoc = SPACE(3)
xMes = SPACE(2)
xAst = 1
OkCancel = 1

*@ 01,01 SAY '          Periodo : ' GET xPeriodo		PICTURE '9999'
@ 03,01 SAY ' Mes Verificaci¢n : ' GET xMes			PICTURE '99' VALID val_para(xMes,'FECMES',' ',22,22)
@ 05,01 SAY '   Tipo Documento : ' GET xTipDoc		VALID val_para(xTipDoc,'TIPDOC',' ',22,22) AND Valcod(xTipDoc)
@ 08,01 SAY '    Tipo Asientos : '
@ 07,22 GET xAst 	FUNCTION '^ Asts Patrimoniales;Asts Presupuestales;Asts Orden'

@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

ACTIVATE WINDOW wEscoge
RELEASE  WINDOW wEscoge

RETURN

FUNCTION Valcod
*--------------
PARAMETERS xTipDoc
SELE PARMA
SEEK "TIPDOC"+xTipdoc
IF FOUND()
	xCodDoc = ALLTRIM(Parma.CodigoAux)
ENDIF

PROCEDURE Escoge
*---------------
IF LASTKEY()=27 OR OkCancel = 2
	cProc = ''
	RETURN
ENDIF
*IF !EMPTY(xPeriodo)
*	cRuta = "H:\SicgData\Data"+IIF(xPeriodo<2000,RIGHT(STR(xPeriodo,4),2),STR(xPeriodo,4))
* ELSE
*	DO StandBy WITH "Ruta Especificada no es correcta"
*	RETURN .T.
*ENDIF
*SET PATH TO &cRuta
*set step on
DO CASE
	CASE xAst = 1
		cProc = 'AstPat'
	CASE xAst = 2
		cProc = 'AstPre'
	CASE xAst = 3
		cProc = 'AstOrd'
	OTHERWISE
		cProc = ''
ENDCASE

PROCEDURE ASTPAT
*---------------
*SACA LAS CUENTAS PATRIMONIALES DE ASTPAT
*
ON KEY LABEL F2  DO CorAstPat
ON KEY LABEL F5  DO AgrAstPat
ON KEY LABEL F8  DO EliAstPat
ON KEY LABEL F10 KEYBOARD(CHR(23))
ON KEY LABEL ESCAPE DO Nada
CLOS DATA
USE ASTPAT  IN 1
USE Cajas   IN 2 ORDER TAG Cajas1		ALIAS Caja
USE Cuentas IN 3 ORDER TAG Cuentas1		ALIAS Cuen

DO Espera WITH 1,"POR FAVOR ESPERE UN MOMENTO"
INDEX ON TIPDOC+NUMMES+NUMREF TO xIdx FOR NUMMES=xMes AND TipDoc=xTipDoc
DO Espera WITH 2

DEFINE WINDOW wAplCC FROM 1,0 TO 23,79 COLOR SCHEME 10 TITLE "®F2¯Corrige   ®F5¯Agrega   ®F8¯Elimina.  ®F10¯Sale."
SELE AstPat

BROWSE NOEDIT NOAPPEND  WINDOW wAplCC FIELDS ;
   Periodo	:H='A¤o',;
   TipDoc	:H='Doc',;
   Fecha	:H='Fecha',;
   NumMes	:H='Mes',;
   NumRef	:H='Ref',;
   CodCta	:H='Cuenta',;
   TipCta	:H='T',;
   MtoDeb	:H='Mto.D' :P='99,999,999.9999',;
   MtoHab	:H='Mto.H' :P='99,999,999.9999',;
   Ret		:H='Ret',;
   CodCTC	:H='Cta. Cte.'

CLOS DATA

ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ON KEY LABEL ESCAPE

RETURN

PROCEDURE CorAstPat
*------------------
DEFINE WINDOW Wind_3 FROM 02,04 TO 20,71 DOUBLE ;
TITLE "Edici¢n de Asientos" COLOR SCHEME 10
ACTIVATE WINDOW Wind_3
*- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ON KEY LABEL ESCAPE

SELE AstPat

SCATTER MEMVAR

@  1,1  SAY '          Periodo: ' + m.Periodo
@  2,1  SAY 'Tipo de Documento: ' + m.TipDoc

@  4,1  SAY '   Num. Documento:' GET m.NumMes	WHEN .F.
@  4,23 GET m.NumRef	WHEN .F.
@  4,30 SAY '   Fecha:' GET m.Fecha  WHEN .F.
@  6,1  SAY '   Tipo de Cuenta:' GET m.TipCta	PICTURE "@M D,H"
@  7,1  SAY '  Cuenta Contable:' GET m.CodCta	VALID v_cta(7,20,m.codcta)
@  9,1  SAY '       Monto Debe:' GET m.MtoDeb	WHEN m.TipCta='D'
@ 10,1  SAY '      Monto Haber:' GET m.MtoHab	WHEN m.TipCta='H'
@ 11,1  SAY '    Retencion S/N:' GET m.Ret		PICTURE "@M S,N, " WHEN m.TipDoc='C/P'
@ 13,1  SAY '   Cta. Corriente:' GET m.CodCtc	WHEN m.TipDoc='C/P'	VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,13,20)

READ VALID val_read()

IF LASTKEY()#27
	IF f_Lock(1)
		GATHER MEMVAR
	ENDIF
 ELSE
	DO StandBy WITH "Cancela Informacion"
ENDIF

xReg = RECNO()
xDeb = 0
xHab = 0

IF SEEK(m.TipDoc+m.NumMes+m.NumRef)
	SCAN WHILE TipDoc=m.TipDoc AND NumMes=m.NumMes AND NumRef=m.NumRef
		IF TipCta = "D"
			xdeb = xdeb + MtoDeb
		ELSE
			xhab = xhab + MtoHab
		ENDIF
	ENDSCAN
ENDIF

IF xdeb # xHab
	DO StandBy WITH "OJO. No Cuadra debe con Haber. Verifique..."
ENDIF
GO xReg

DEACTIVATE WINDOW Wind_3
RELEASE WINDOW Wind_3

ON KEY LABEL F2  DO CorAstPat
ON KEY LABEL F5  DO AgrAstPat
ON KEY LABEL F8  DO EliAstPat
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL ESCAPE DO Nada

RETURN

PROCEDURE AgrAstPat
*------------------
DEFINE WINDOW Wind_3 FROM 02,04 TO 20,71 DOUBLE ;
TITLE "Agregar Asientos" COLOR SCHEME 10
ACTIVATE WINDOW Wind_3
*- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ON KEY LABEL ESCAPE

SELE AstPat

SCATTER MEMVAR

@  1,1  SAY '          Periodo: ' + m.Periodo
@  2,1  SAY 'Tipo de Documento: ' + m.TipDoc

@  4,1  SAY '   Num. Documento:' GET m.NumMes	WHEN .F.
@  4,23 GET m.NumRef	WHEN .F.
@  4,30 SAY '   Fecha:' GET m.Fecha  WHEN .F.
@  6,1  SAY '   Tipo de Cuenta:' GET m.TipCta	PICTURE "@M D,H"
@  7,1  SAY '  Cuenta Contable:' GET m.CodCta	VALID v_cta(7,20,m.CodCta)
@  9,1  SAY '       Monto Debe:' GET m.MtoDeb	WHEN m.TipCta='D'
@ 10,1  SAY '      Monto Haber:' GET m.MtoHab	WHEN m.TipCta='H'
@ 11,1  SAY '    Retencion S/N:' GET m.Ret		PICTURE "@M S,N, " WHEN m.TipDoc='C/P'
@ 13,1  SAY '   Cta. Corriente:' GET m.CodCtc	WHEN m.TipDoc='C/P'	VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,13,20)

READ VALID val_read()

IF LASTKEY()#27
	IF f_Appd()
		GATHER MEMVAR
	ENDIF
 ELSE
	DO StandBy WITH "Cancela Informacion"
ENDIF

xReg = RECNO()
xDeb = 0
xHab = 0

IF SEEK(m.TipDoc+m.NumMes+m.NumRef)
	SCAN WHILE TipDoc=m.TipDoc AND NumMes=m.NumMes AND NumRef=m.NumRef
		IF TipCta = "D"
			xdeb = xdeb + MtoDeb
		ELSE
			xhab = xhab + MtoHab
		ENDIF
	ENDSCAN
ENDIF

IF xdeb # xHab
	DO StandBy WITH "OJO. No Cuadra debe con Haber. Verifique..."
ENDIF
GO xReg

DEACTIVATE WINDOW Wind_3
RELEASE WINDOW Wind_3

ON KEY LABEL F2  DO CorAstPat
ON KEY LABEL F5  DO AgrAstPat
ON KEY LABEL F8  DO EliAstPat
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL ESCAPE DO Nada

RETURN

PROCEDURE EliAstPat
*------------------
IF YesNo("¨Esta Seguro de Eliminar este Item?")
	DELE NEXT 1
ENDIF

xReg = RECNO()
xDeb = 0
xHab = 0

SCATTER MEMVAR

IF SEEK(m.TipDoc+m.NumMes+m.NumRef)
	SCAN WHILE TipDoc=m.TipDoc AND NumMes=m.NumMes AND NumRef=m.NumRef
		IF TipCta = "D"
			xdeb = xdeb + MtoDeb
		ELSE
			xhab = xhab + MtoHab
		ENDIF
	ENDSCAN
ENDIF

IF xdeb # xHab
	DO StandBy WITH "OJO. No Cuadra debe con Haber. Verifique..."
ENDIF
GO xReg

RETURN


PROCEDURE ASTPRE
*---------------
*SACA LAS CUENTAS PRESUPUESTALES DE ASTPRE
*
ON KEY LABEL F2  DO CorAstPre
ON KEY LABEL F5  DO AgrAstPre
ON KEY LABEL F8  DO EliAstPre
ON KEY LABEL F10 KEYBOARD(CHR(23))
ON KEY LABEL ESCAPE DO Nada

CLOS DATA
USE ASTPre  IN 1
USE MaePre  IN 2 ORDER TAG MaePre1
USE Cuentas IN 3 ORDER TAG Cuentas1		ALIAS Cuen

DO Espera WITH 1,"POR FAVOR ESPERE UN MOMENTO"
INDEX ON TIPDOC+NUMMES+NUMREF TO xIdx FOR NUMMES=xMes AND TipDoc=xTipDoc
DO Espera WITH 2

DEFINE WINDOW wAplCC FROM 1,0 TO 23,79 COLOR SCHEME 10 TITLE "®F2¯Corrige   ®F5¯Agrega   ®F8¯Elimina.  ®F10¯Sale."
SELE AstPre

BROWSE NOEDIT NOAPPEND  WINDOW wAplCC FIELDS ;
   Periodo	:H='A¤o',;
   TipDoc	:H='Doc',;
   FecRef	:H='Fecha',;
   NumMes	:H='Mes',;
   NumRef	:H='Ref',;
   CodCad	:H='Cadena',;
   Cuenta	:H='Cuenta',;
   CtaDeb	:H='Debe',;
   CtaHab	:H='Haber',;
   Tipo		:H='T',;
   ValDeb	:H='Mto.D' :P='999,999.9999',;
   ValHab	:H='Mto.H' :P='999,999.9999'

CLOS DATA

ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ON KEY LABEL ESCAPE

RETURN

PROCEDURE CorAstPre
*------------------
DEFINE WINDOW Wind_3 FROM 02,04 TO 20,71 DOUBLE ;
TITLE "Edici¢n de Asientos" COLOR SCHEME 10
ACTIVATE WINDOW Wind_3
*- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ON KEY LABEL ESCAPE

SELE AstPre

SCATTER MEMVAR

@  1,1  SAY '          Periodo: ' + m.Periodo
@  2,1  SAY 'Tipo de Documento: ' + m.TipDoc

@  4,1  SAY '   Num. Documento:' GET m.NumMes	WHEN .F.
@  4,23 GET m.NumRef	WHEN .F.
@  4,30 SAY '   Fecha:' GET m.FecRef  WHEN .F.
@  5,1  SAY ' Cadena Funcional:' GET m.CodCad	VALID val_CODCAD(m.codcad,m.periodo,' ',20)
@  7,1  SAY '   Tipo de Cuenta:' GET m.Tipo		PICTURE "@M D,H"
@  8,1  SAY '  Cuenta Contable:' GET m.Cuenta	VALID v_cta(8,20,m.Cuenta) AND AgrCtaPre()
@  9,1  SAY '      Cuenta Debe:' GET m.CtaDeb	WHEN .F.
@ 10,1  SAY '     Cuenta Haber:' GET m.CtaHab	WHEN .F.

@ 11,1  SAY '       Monto Debe:' GET m.ValDeb	WHEN m.Tipo='D'
@ 12,1  SAY '      Monto Haber:' GET m.ValHab	WHEN m.Tipo='H'

READ VALID val_read()

IF LASTKEY()#27
	IF f_Lock(1)
		GATHER MEMVAR
	ENDIF
 ELSE
	DO StandBy WITH "Cancela Informacion"
ENDIF

xReg = RECNO()
xDeb = 0
xHab = 0

IF SEEK(m.TipDoc+m.NumMes+m.NumRef)
	SCAN WHILE TipDoc=m.TipDoc AND NumMes=m.NumMes AND NumRef=m.NumRef
		IF Tipo = "D"
			xdeb = xdeb + ValDeb
		ELSE
			xhab = xhab + ValHab
		ENDIF
	ENDSCAN
ENDIF

IF xdeb # xHab
	DO StandBy WITH "OJO. No Cuadra debe con Haber. Verifique..."
ENDIF
GO xReg

DEACTIVATE WINDOW Wind_3
RELEASE WINDOW Wind_3

ON KEY LABEL F2  DO CorAstPre
ON KEY LABEL F5  DO AgrAstPre
ON KEY LABEL F8  DO EliAstPre
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL ESCAPE DO Nada

RETURN

PROCEDURE AgrAstPre
*------------------
DEFINE WINDOW Wind_3 FROM 02,04 TO 20,71 DOUBLE ;
TITLE "Agregar Asientos" COLOR SCHEME 10
ACTIVATE WINDOW Wind_3
*- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10

SELE AstPre

SCATTER MEMVAR

@  1,1  SAY '          Periodo: ' + m.Periodo
@  2,1  SAY 'Tipo de Documento: ' + m.TipDoc

@  4,1  SAY '   Num. Documento:' GET m.NumMes	WHEN .F.
@  4,23 GET m.NumRef	WHEN .F.
@  4,30 SAY '   Fecha:' GET m.FecRef  WHEN .F.
@  5,1  SAY ' Cadena Funcional:' GET m.CodCad	VALID val_CODCAD(m.codcad,m.periodo,' ',20)
@  7,1  SAY '   Tipo de Cuenta:' GET m.Tipo		PICTURE "@M D,H"
@  8,1  SAY '  Cuenta Contable:' GET m.Cuenta	VALID v_cta(8,20,m.Cuenta) AND AgrCtaPre()
@  9,1  SAY '      Cuenta Debe:' GET m.CtaDeb	WHEN .F.
@ 10,1  SAY '     Cuenta Haber:' GET m.CtaHab	WHEN .F.

@ 11,1  SAY '       Monto Debe:' GET m.ValDeb	WHEN m.Tipo='D'
@ 12,1  SAY '      Monto Haber:' GET m.ValHab	WHEN m.Tipo='H'

READ VALID val_read()

IF LASTKEY()#27
	IF f_Appd()
		GATHER MEMVAR
	ENDIF
 ELSE
	DO StandBy WITH "Cancela Informacion"
ENDIF

xReg = RECNO()
xDeb = 0
xHab = 0

IF SEEK(m.TipDoc+m.NumMes+m.NumRef)
	SCAN WHILE TipDoc=m.TipDoc AND NumMes=m.NumMes AND NumRef=m.NumRef
		IF Tipo = "D"
			xdeb = xdeb + ValDeb
		ELSE
			xhab = xhab + ValHab
		ENDIF
	ENDSCAN
ENDIF

IF xdeb # xHab
	DO StandBy WITH "OJO. No Cuadra debe con Haber. Verifique..."
ENDIF
GO xReg

DEACTIVATE WINDOW Wind_3
RELEASE WINDOW Wind_3

ON KEY LABEL F2  DO CorAstPre
ON KEY LABEL F5  DO AgrAstPre
ON KEY LABEL F8  DO EliAstPre
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL ESCAPE DO Nada

RETURN

FUNCTION AgrCtaPre
*-----------------
PRIVATE mRet
mret = .F.
IF m.codCad#SUBSTR(m.cuenta,5,4)
	DO StandBy WITH "La cadena funcional no coincide con la Cuenta Contable."
	mRet = .F.
ELSE
	IF m.Tipo="D"
		m.CtaDeb = m.Cuenta
		m.CtaHab = SPACE(10)
		m.ValHab = 0
	ELSE
		m.CtaDeb = SPACE(10)
		m.CtaHab = m.Cuenta
		m.ValDeb = 0
	ENDIF
	SHOW GET m.CtaDeb
	SHOW GET m.ctaHab
	SHOW GET m.ValDeb
	SHOW GET m.ValHab
	mRet = .T.
ENDIF

RETURN mRet

PROCEDURE EliAstPre
*------------------
IF YesNo("¨Esta Seguro de Eliminar este Item?")
	DELE NEXT 1
ENDIF

xReg = RECNO()
xDeb = 0
xHab = 0

SCATTER MEMVAR

IF SEEK(m.TipDoc+m.NumMes+m.NumRef)
	SCAN WHILE TipDoc=m.TipDoc AND NumMes=m.NumMes AND NumRef=m.NumRef
		IF Tipo = "D"
			xdeb = xdeb + ValDeb
		ELSE
			xhab = xhab + ValHab
		ENDIF
	ENDSCAN
ENDIF

IF xdeb # xHab
	DO StandBy WITH "OJO. No Cuadra debe con Haber. Verifique..."
ENDIF
GO xReg

RETURN

PROCEDURE ASTORD
*---------------
*SACA LAS CUENTAS PATRIMONIALES DE ASTORD
*
ON KEY LABEL F2  DO CorAstOrd
ON KEY LABEL F5  DO AgrAstOrd
ON KEY LABEL F8  DO EliAstOrd
ON KEY LABEL F10 KEYBOARD(CHR(23))
ON KEY LABEL ESCAPE DO Nada

CLOS DATA
USE ASTOrd  IN 1
USE Cuentas IN 3 ORDER TAG Cuentas1		ALIAS Cuen

DO Espera WITH 1,"POR FAVOR ESPERE UN MOMENTO"
INDEX ON TIPDOC+NUMMES+NUMREF TO xIdx FOR NUMMES=xMes AND TipDoc=xTipDoc
DO Espera WITH 2

DEFINE WINDOW wAplCC FROM 1,0 TO 23,79 COLOR SCHEME 10 TITLE "®F2¯Corrige   ®F5¯Agrega   ®F8¯Elimina.  ®F10¯Sale."
SELE AstOrd

BROWSE NOEDIT NOAPPEND  WINDOW wAplCC FIELDS ;
   Periodo	:H='A¤o',;
   TipDoc	:H='Doc',;
   Fecha	:H='Fecha',;
   NumMes	:H='Mes',;
   NumRef	:H='Ref',;
   CodCta	:H='Cuenta',;
   TipCta	:H='T',;
   MtoDeb	:H='Mto.D' :P='999,999.9999',;
   MtoHab	:H='Mto.H' :P='999,999.9999',;
   NumCP	:H='C/P'

CLOS DATA

ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ON KEY LABEL ESCAPE

RETURN

PROCEDURE CorAstOrd
*------------------
DEFINE WINDOW Wind_3 FROM 02,04 TO 20,71 DOUBLE ;
TITLE "Edici¢n de Asientos" COLOR SCHEME 10
ACTIVATE WINDOW Wind_3
*- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ON KEY LABEL ESCAPE

SELE AstOrd

SCATTER MEMVAR

@  1,1  SAY '          Periodo: ' + m.Periodo
@  2,1  SAY 'Tipo de Documento: ' + m.TipDoc

@  4,1  SAY '   Num. Documento:' GET m.NumMes	WHEN .F.
@  4,23 GET m.NumRef	WHEN .F.
@  4,30 SAY '   Fecha:' GET m.Fecha   WHEN .F.
@  6,1  SAY '   Tipo de Cuenta:' GET m.TipCta	PICTURE "@M D,H"
@  8,1  SAY '  Cuenta Contable:' GET m.CodCta	VALID v_cta(8,20,m.CodCta) AND CorMtoOrd()
@ 10,1  SAY '       Monto Debe:' GET m.MtoDeb	WHEN m.TipCta='D'
@ 12,1  SAY '      Monto Haber:' GET m.MtoHab	WHEN m.TipCta='H'

READ VALID val_read()

IF LASTKEY()#27
	IF f_Lock(1)
		GATHER MEMVAR
	ENDIF
 ELSE
	DO StandBy WITH "Cancela Informacion"
ENDIF

xReg = RECNO()
xDeb = 0
xHab = 0

IF SEEK(m.TipDoc+m.NumMes+m.NumRef)
	SCAN WHILE TipDoc=m.TipDoc AND NumMes=m.NumMes AND NumRef=m.NumRef
		IF TipCta = "D"
			xdeb = xdeb + MtoDeb
		ELSE
			xhab = xhab + MtoHab
		ENDIF
	ENDSCAN
ENDIF

IF xdeb # xHab
	DO StandBy WITH "OJO. No Cuadra debe con Haber. Verifique..."
ENDIF
GO xReg

DEACTIVATE WINDOW Wind_3
RELEASE WINDOW Wind_3

ON KEY LABEL F2  DO CorAstOrd
ON KEY LABEL F5  DO AgrAstOrd
ON KEY LABEL F8  DO EliAstOrd
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL ESCAPE DO Nada

RETURN

PROCEDURE AgrAstOrd
*------------------
DEFINE WINDOW Wind_3 FROM 02,04 TO 20,71 DOUBLE ;
TITLE "Agregar Asientos" COLOR SCHEME 10
ACTIVATE WINDOW Wind_3
*- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ON KEY LABEL ESCAPE

SELE AstOrd

SCATTER MEMVAR

@  1,1  SAY '          Periodo: ' + m.Periodo
@  2,1  SAY 'Tipo de Documento: ' + m.TipDoc

@  4,1  SAY '   Num. Documento:' GET m.NumMes	WHEN .F.
@  4,23 GET m.NumRef	WHEN .F.
@  4,30 SAY '   Fecha:' GET m.Fecha   WHEN .F.
@  6,1  SAY '   Tipo de Cuenta:' GET m.TipCta	PICTURE "@M D,H"
@  8,1  SAY '  Cuenta Contable:' GET m.CodCta	VALID v_cta(8,20,m.CodCta) AND CorMtoOrd()
@ 10,1  SAY '       Monto Debe:' GET m.MtoDeb	WHEN m.TipCta='D'
@ 12,1  SAY '      Monto Haber:' GET m.MtoHab	WHEN m.TipCta='H'

READ VALID val_read()

IF LASTKEY()#27
	IF f_Appd()
		GATHER MEMVAR
	ENDIF
 ELSE
	DO StandBy WITH "Cancela Informacion"
ENDIF

xReg = RECNO()
xDeb = 0
xHab = 0

IF SEEK(m.TipDoc+m.NumMes+m.NumRef)
	SCAN WHILE TipDoc=m.TipDoc AND NumMes=m.NumMes AND NumRef=m.NumRef
		IF TipCta = "D"
			xdeb = xdeb + MtoDeb
		ELSE
			xhab = xhab + MtoHab
		ENDIF
	ENDSCAN
ENDIF

IF xdeb # xHab
	DO StandBy WITH "OJO. No Cuadra debe con Haber. Verifique..."
ENDIF
GO xReg

DEACTIVATE WINDOW Wind_3
RELEASE WINDOW Wind_3

ON KEY LABEL F2  DO CorAstOrd
ON KEY LABEL F5  DO AgrAstOrd
ON KEY LABEL F8  DO EliAstOrd
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL ESCAPE DO Nada

RETURN

PROCEDURE EliAstOrd
*------------------
IF YesNo("¨Esta Seguro de Eliminar este Item?")
	DELE NEXT 1
ENDIF

xReg = RECNO()
xDeb = 0
xHab = 0

SCATTER MEMVAR

IF SEEK(m.TipDoc+m.NumMes+m.NumRef)
	SCAN WHILE TipDoc=m.TipDoc AND NumMes=m.NumMes AND NumRef=m.NumRef
		IF TipCta = "D"
			xdeb = xdeb + MtoDeb
		ELSE
			xhab = xhab + MtoHab
		ENDIF
	ENDSCAN
ENDIF

IF xdeb # xHab
	DO StandBy WITH "OJO. No Cuadra debe con Haber. Verifique..."
ENDIF
GO xReg

RETURN

FUNCTION CorMtoOrd
*-----------------
IF m.TipCta = "D"
	m.MtoHab = 0
ELSE
	m.MtoDeb = 0
ENDIF
	SHOW GET m.MtoDeb
	SHOW GET m.MtoHab
RETURN

FUNCTION v_cta
*-------------
* Muestra el tipo de auxiliar posible.
* Verifica que la cuenta sea de detalle y no sea autom tica.
* Si la cuenta no existe permite agregarla en el plan de cuentas.
*----------------------------------------------
PARAMETERS _Fil,_Col,_cta
PRIVATE mAlias
mAlias = ALIAS()
SELECT Cuen
_f_ = SEEK(_Cta) .AND. !EMPTY(_Cta)
@ _Fil,_Col+11 SAY SUBSTR(Cuen.Descri,1,35)
IF _f_ .OR. EMPTY(_Cta)
	IF EMPTY(_Cta)
		SELECT Cuen
		SET ORDER TO TAG Cuentas4
		_f_ = Val_Fun('Cuen','Cuenta',"Cuenta+' '+Descri",_Cta,1,_fIL,_Col+11,'SUBSTR(Descri,1,35)')
		SET ORDER TO TAG Cuentas1
		SELECT (mAlias)
		IF !_f_
			_msg1 = "Indique el n£mero de cuenta!!!"
			RETURN .F.
		ENDIF
	ENDIF
	
	SELECT (mAlias)
	IF UPPER(Cuen.Detalle)  # 'S'
		_msg1 = "Cuenta no es de Detalle"
		RETURN .F.
	ENDIF
	
ENDIF

RETURN _f_

PROCEDURE NADA
*-------------
RETURN
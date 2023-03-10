** ----------------------------------
** COMERR.PRG
** Reporte de errores
** AUTOR : UDEP - 1994
** ----------------------------------
* Abrir Archivos de trabajo
USE Compro  IN 1 ORDER TAG Compro1	ALIAS Comp
USE Diario  IN 2 ORDER TAG Diario1	ALIAS Diar
USE parmae  IN 3 ORDER TAG parmae1	ALIAS parma
USE auxil   IN 4 ORDER TAG auxil1	ALIAS auxi
USE Cuentas IN 5 ORDER TAG Cuentas1	ALIAS Cuen

STORE 0   TO xano, xMes,vdebe, vhaber
DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
DEFINE WINDOW wInicio FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE ' ?? Listado de Errores ?? ' COLOR SCHEME 5
ACTIVATE WINDOW wInicio
SELECT Parma
SEEK "MESANO"+"ACTUA"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
xAno     = IIF(FOUND(), YEAR(vmesact), YEAR(DATE()))
@ 1,01 SAY '                Mes : ' GET xMes PICTURE '99' VALID xMes > 0 .AND. xMes <= 12
@ 3,01 SAY '                A?o : ' GET xAno PICTURE '9,999'
@ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ
DEACTIVATE WINDOW wInicio
RELEASE WINDOWS 

IF OKCANCEL=1 .AND. LASTKEY()<>27
	SELECT Diar
	SET RELA TO Comprob+ComAdi INTO Comp
	DO REPPRG WITH "Lista"," Documentos Errados "
ENDIF
CLOSE DATA
RETURN


PROCEDURE Lista
*---------------
PARAMETER _desti

IF _desti=2
	SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINT

STORE 0 TO linea, pagina

* Verifica que las cuentas que llevan Doc.Refer. tengan dicho documento.
SELECT Diar
GO TOP
SCAN
	vError = ''
	SELECT Cuen
	IF SEEK(Diar.Cuenta) .AND. Cuen.RefObl
		IF EMPTY(Diar.Registro)
			vError = vError + IIF(EMPTY(vError),'Falta ','') + 'CODIGO Auxil.'
		 ELSE
			SELECT Auxi
			IF !SEEK(Diar.TipAux + SUBSTR(Diar.Registro,1,6))
				vError = vError + IIF(!EMPTY(vError),'','ERROR ');
						+ 'Auxiliar no existe. '
			ENDIF
		ENDIF
	ENDIF
	SELECT Comp
	IF YEAR(Comp.Fecha) # YEAR(vMesAct)
		vError = vError + IIF(!EMPTY(vError),'','ERROR ');
						+ 'Fecha del Comprobante errada. '
	ENDIF
	SELECT Diar
	IF !EMPTY(vError)
		IF linea = 0 .OR. linea > 55
			DO Titulo
		ENDIF
		@ linea, 0 SAY Diar.Cuenta
		@ linea,12 SAY Diar.Comprob
		@ linea,28 SAY Diar.ComAdi
		@ linea,32 SAY SUBSTR(Diar.TipDcr + '-' + Diar.DocRef,1,13)
		@ linea,45 SAY SUBSTR(Diar.Registro,1,6)
		@ linea,51 SAY Diar.FecEmi
		@ linea,62 SAY SUBSTR(Comp.Glosa1,1,19)
		@ linea,84 SAY Diar.Tipo
		@ linea,86 SAY Montos PICTURE '99999,999,999.99'
		@ linea,104 SAY IIF(LEN(ALLTRIM(vError))>35,SUBSTR(vError,1,35),vError)
		
		linea = linea + 1
		x = 0
		DO WHILE .T.
			x = x + 36
			IF LEN(ALLTRIM(SUBSTR(vError,x))) > 0
				@ linea,102 SAY SUBSTR(vError,x)
				linea = linea + 1
			 ELSE
				EXIT
			ENDIF
		ENDDO
	ENDIF
ENDSCAN
SUSP
* Verifica que las provisiones o las cancelaciones esten correctas.
SET ORDER TO TAG Diario4
GO TOP
DO WHILE !EOF()
	vLider     = .F.
	vCuenta    = SPACE(LEN(Diar.Cuenta))
	vTipo      = SPACE(LEN(Diar.Tipo))
	vTipAux    = Diar.TipAux
	vRegistro  = Diar.Registro
	vMonto     = 0
	vReg       = RECNO()
	vError = ''
	
	vSaldo = vMonto
	SEEK vTipAux + vRegistro
	SCAN WHILE vTipAux = Diar.TipAux .AND. vRegistro = Diar.Registro
		vError = ''
		IF vCuenta # Diar.Cuenta
			vError = vError + IIF(EMPTY(vError),'ERROR ','');
							+ 'Cuenta distinta a provisi?n. '
		ENDIF
		IF vTipo = Diar.Tipo
			vError = vError + IIF(EMPTY(vError),'ERROR ','');
							+ 'Provisi?n es al ' + IIF(vTipo = 'D','Debe', 'Haber') + '. '
		ENDIF
		IF Diar.Tipo # vTipo
			vSaldo = vSaldo - Diar.Montos
		 ELSE
			vSaldo = vSaldo + Diar.Montos
		ENDIF
		IF !EMPTY(vError)
			IF linea = 0 .OR. linea > 55
				DO Titulo
			ENDIF
			@ linea, 0 SAY Diar.Cuenta
			@ linea,12 SAY Diar.Comprob
			@ linea,23 SAY Diar.ComAdi
			@ linea,38 SAY SUBSTR(Diar.TipDcr + '-' + Diar.DocRef,1,13)
			@ linea,45 SAY Diar.Registro
			@ linea,59 SAY Diar.FecEmi
			@ linea,79 SAY SUBSTR(Comp.Glosa1,1,19)
			@ linea,83 SAY Diar.Tipo
			@ linea,87 SAY Diar.Montos PICTURE '99999,999,999.99'
			@ linea,110 SAY IIF(LEN(ALLTRIM(vError))>35,SUBSTR(vError,1,35),vError)
			linea = linea + 1
			x = 0
			DO WHILE .T.
				x = x + 36
				IF LEN(ALLTRIM(SUBSTR(vError,x))) > 0
					@ linea,100 SAY SUBSTR(vError,x)
					linea = linea + 1
				 ELSE
					EXIT
				ENDIF
			ENDDO
		ENDIF
	ENDSCAN
	IF vSaldo < 0
		vError = vError + IIF(EMPTY(vError),'ERROR ',', ');
						+ 'Cancelaci?n mayor que saldo. '
		@ linea,  0 SAY vCuenta
		@ linea, 23 SAY SUBSTR(vTipDcr + '-' + vDocRef,1,13)
		@ linea, 38 SAY SUBSTR(vRegistro,1,6)
		@ linea,100 SAY IIF(LEN(ALLTRIM(vError))>35,SUBSTR(vError,1,35),vError)
		linea = linea + 1
		x = 0
		DO WHILE .T.
			x = x + 36
			IF LEN(ALLTRIM(SUBSTR(vError,x))) > 0
				@ linea,100 SAY SUBSTR(vError,x)
				linea = linea + 1
			 ELSE
				EXIT
			ENDIF
		ENDDO
	ENDIF
	SELECT Diar
ENDDO
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN


PROCEDURE Titulo
*---------------
pagina = pagina  + 1
vTitulo = ' L I S T A D O   D E   E R R O R E S   E N   D O C U M E N T O S '
vMes    = MFecha(xMes, xAno)
@ 1,1   SAY CHR(15)
@ 1,2   SAY cia
@ 1,125 SAY 'Pag.'
@ 1,129 SAY pagina   PICTURE '##,###'
@ 2,INT((134 - LEN(vtitulo))/2) SAY vTitulo
@ 2,125 SAY DATE()
@ 3,INT((134 - LEN(vMes))/2) SAY vMes
@ 3,125 SAY TIME()
@ 5,0   SAY REPLICATE('-',135)
@ 6,0   SAY '  Cuenta  '
@ 6,12  SAY '  Docum. '
@ 6,25  SAY 'DocAdi'
@ 6,32  SAY 'Doc.Ref'
@ 6,46  SAY 'Doc.AdiR'
@ 6,49  SAY 'C?digo'
@ 6,55  SAY ' Fecha  '
@ 6,67  SAY '      Concepto     '
@ 6,87  SAY 'D/H'
@ 6,91  SAY '          Monto'
@ 6,104 SAY '              ERRORES'
@ 7,0   SAY REPLICATE('-',135)
linea = 8
RETURN


PROCEDURE Salida
*---------------
RELEASE    WINDOW Consis
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


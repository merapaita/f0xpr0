PROCEDURE Xrepprg
*---------------
** Programa   : REPPRG.PRG
** Descripcion: Impresion de reportes utilizando programa reporteador.
** Par metros :
**               _PrgRpt : Programa reporteador.
PARAMETER  _prgrpt, _tit, _copia, _wp
PRIVATE  resul
resul = 'OK'
IF PARAMETERS()=2
	_copia = 1
ENDIF

IF PARAMETERS()=3
	_wp = .F.
ENDIF

**-- Environment
SET ESCAPE ON
ON ESCAPE STORE .F. TO printing

**-- Verifica Existencia de windows
IF !WEXIST("MSG2USE")
	DEFINE WINDOW msg2use   FROM 12,02 TO 16,77 DOUBLE FLOAT SHADOW COLOR SCHEME 7
ENDIF

**-- PREGUNTA SI SE IMPRIME O VISUALIZA
ACTIVATE WINDOW msg2user
_dest = 'IMPRESORA'
@ 01,5 SAY 'DESTINO DE IMPRESION : PANTALLA/IMPRESORA' GET _dest PICTURE "@M IMPRESORA,PANTALLA"
READ
DEACTIVATE WINDOW msg2user

IF MOD(READKEY(),256)=12
	ON KEY
	SET ESCAPE OFF
	ACTIVATE SCREEN
	RETURN
ENDIF
impre = (_dest='IMPRESORA')

IF !impre
	p_fil = SYS(3)+".LST"
ENDIF

printing = .T.
IF impre
	IF !EMPTY(LEFT(SYS(0),18))
		IF !yesno("IMPRIME EN IMPRESORA LOCAL ?  <NO = IMPRESORA DE RED>")
			**-- Impresora de red.
			SET PRINTER TO \\ibm_pc\printq_0=lpt1
			SET PRINTER TO \\spooler\nb
		ENDIF
		IF !ready2pr()
			printing = .F.
		ENDIF
	ENDIF
ENDIF

IF printing

	**-- VENTANA DE GENERACION DE REPORTE
	DEFINE WINDOW _repo FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _tit ;
		FOOTER ' Pag: [Pg-Up]  Pag: [Pg-Dn]  Inicio: [Ctrl+Home]  Final: [Ctrl+End] '
	ACTIVATE WINDOW _repo
	CLEAR

	**-- SI SE IMPRIME O VISUALIZA
	IF impre
		DO &_prgrpt WITH 1
	ELSE
		@ 02,20  SAY  '** Reporte en ejecuci¢n **'

		DO &_prgrpt WITH 2

		IF _wp
			ACTIVATE SCREEN
			RESTORE SCREEN FROM pantalla
			RUN FOXSWAP wp &p_fil
		ELSE
			MODIFY COMMAND (p_fil) NOEDIT WINDOW _repo
		ENDIF
		IF resul = 'OK'
			IF yesno("¨ Imprime el Reporte ?")
				FOR v = 1 TO _copia
					IF ready2pr()
						!TYPE &p_fil>prn
					ENDIF
				ENDFOR
			ENDIF
		ELSE
			resul = 'OK'
		ENDIF
		ERASE (p_fil)
	ENDIF
	DEACTIVATE WINDOW _repo
ELSE
	DO standby WITH 'El reporte ha sido cancelado'
ENDIF

ON ESCAPE
SET ESCAPE OFF
ACTIVATE SCREEN

RETURN

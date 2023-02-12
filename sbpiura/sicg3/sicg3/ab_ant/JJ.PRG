PROCEDURE reporte
*----------------
** Programa   : REPORTE.PRG
** Descripcion: Impresion de reportes utilizando archivos de reporte.
** Par metros :
**               _Tipo : tipo de reporte (1=con ambiente, 2=sin ambiente)
**               _Form : archivo de reporte
**               _Tit  : t¡tulo de la ventana
**               Num_C : N£mero de Copias.
**                 _wp : Si se envia a Word Perfect
**               Ran_pg: Si se pregunta un rango de paginas.
** -----------------------------------------------------------------------
PARAMETER _tipo, _form, _tit, num_c, _wp, ran_pg

**-- Environment
PRIVATE _wndold

_wndold = WOUTPUT()

SET ESCAPE ON
ON ESCAPE STORE .F. TO printing
_conso = SET("CONSOLE")
SET CONSOLE ON

**-- Verifica Existencia de windows

DEFINE WINDOW msg2use   FROM 07,02 TO 11,77 DOUBLE FLOAT SHADOW

*- Si no va a WP
_wp = .F.
IF PARAMETERS()<5
	_wp = .F.
ENDIF

*- Numero de copias
IF PARAMETER()<4
	num_c = 1
ENDIF
IF num_c # 1
	DEFINE WINDOW _xyx FROM 15,40 TO 18,75 COLOR SCHEME 10 TITLE " # DE COPIAS "
	ACTIVATE WINDOW _xyx
	CLEAR
	@ 1,5 SAY "Copias : " GET num_c VALID(num_c>0) PICTURE "99"
	READ
	RELEASE WINDOW _xyx

	IF LASTKEY()=27
		IF !EMPTY(_wndold)
			ACTIVATE WINDOW &_wndold
		ELSE
			ACTIVATE SCREEN
		ENDIF
		SET CONSOLE &_conso
		RETURN
	ENDIF
	_ncopies = num_c
ENDIF

*- Rango de Paginas
IF PARAMETER() >= 6 .AND. ran_pg
	DEFINE WINDOW _xyx FROM 15,40 TO 18,75 COLOR SCHEME 10 TITLE " Rango de P ginas "
	ACTIVATE WINDOW _xyx
	CLEAR
	vpbpage = 1
	vpepage = 32767
	@ 0,5 SAY "Inicio : " GET vpbpage PICTURE "99,999" VALID vpbpage <= vpepage
	@ 1,5 SAY "   Fin : " GET vpepage PICTURE "99,999" VALID vpbpage <= vpepage;
		.AND. vpepage <= 32767
	READ
	RELEASE WINDOW _xyx

	IF LASTKEY()=27
		IF !EMPTY(_wndold)
			ACTIVATE WINDOW &_wndold
		ELSE
			ACTIVATE SCREEN
		ENDIF
		SET CONSOLE &_conso
		RETURN
	ENDIF
ENDIF


**-- PREGUNTA SI SE IMPRIME O VISUALIZA

_dest = 'Pantalla '
IF !_wp


	ACTIVATE WINDOW msg2use
	_dest = 'Impresora'
	TITLE = ' DESTINO DE IMPRESION '
	p_fil = SPACE(8)
	_dest1 = 1
	@ 01,5 SAY 'Destino de impresi¢n : Pantalla/Impresora/Archivo' GET _dest PICTURE "@M Impresora,Pantalla,Archivo"
	READ
	IF _dest = 'Archivo  '
		CLEAR
		@ 01,20 SAY 'Nombre del Archivo :' GET p_fil PICTURE "NNNNNNNN" VALID !EMPTY(p_fil)
		READ
	ENDIF
	p_fil = ALLTRIM(p_fil) + ".LST"
	RELEASE WINDOW msg2use
ENDIF

IF LASTKEY() = 27
	IF !EMPTY(_wndold)
		ACTIVATE WINDOW &_wndold
	ELSE
		ACTIVATE SCREEN
	ENDIF
	SET CONSOLE &_conso
	RETURN
ENDIF
impre = (_dest='Impresora')

IF !impre .AND. _dest = "Pantalla "
	p_fil = SYS(3)+".LST"
ENDIF


printing = .T.
IF impre
	IF !EMPTY(LEFT(SYS(0),15))
		IF !yesno("¨Imprime en impresora local?")
			**-- Impresora de red.
			SET PRINTER TO \\ibm_pc\printq_0=lpt1
			SET PRINTER TO \\spooler\nb
		ENDIF
	ENDIF
	IF !ready2pr()
		printing = .F.
	ENDIF
ENDIF


IF printing
	**-- VENTANA DE GENERACION DE REPORTE
	DEFINE WINDOW _repo FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _tit
	ACTIVATE WINDOW _repo
	CLEAR

	**-- SI SE IMPRIME O VISUALIZA
	IF impre
		p_fil = SYS(3) + ".LST"
		SET PRINTER TO &p_fil

		IF _tipo = 1
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				REPORT FORM &_form ENVIRONMENT NOEJECT TO PRINT
				ENDPRINTJOB
			ELSE
				REPORT FORM &_form ENVIRONMENT NOEJECT TO pri
			ENDIF
		ELSE
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				REPORT FORM &_form NOEJECT TO PRINT
				ENDPRINTJOB
			ELSE

				REPORT FORM &_form NOEJECT TO PRINT
			ENDIF
		ENDIF
		SET PRINTER TO           && Si era la impresora server, libera el spool

		IF !_wp
			FOR i=1 TO num_c
				IF ready2pr() .AND. IIF(num_c>1,yesno("Copia "+STR(i,2)+" .¨Prepare el papel. Listo?"),.T.)
					!TYPE &p_fil >prn
				ENDIF
				IF LASTKEY()=27
					EXIT
				ENDIF
			ENDFOR
		ELSE
			!FOXSWAP wp &p_fil
		ENDIF

		ERASE &p_fil

	ELSE

		IF _tipo = 1
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				REPORT FORM &_form ENVIRONMENT TO FILE (p_fil) NOEJECT
				ENDPRINTJOB
			ELSE
				REPORT FORM &_form ENVIRONMENT TO FILE (p_fil) NOEJECT
			ENDIF
		ELSE
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				
				REPORT FORM &_form TO FILE (p_fil) NOEJECT
				ENDPRINTJOB
			ELSE
				REPORT FORM &_form TO FILE (p_fil) NOEJECT
			ENDIF
		ENDIF

		IF !_wp
			IF _dest = "Pantalla "
				MODIFY COMMAND (p_fil) NOEDIT WINDOW _repo

				IF yesno("¨Imprime el reporte ? ")
					FOR i=1 TO num_c
						IF ready2pr() .AND. IIF(num_c>1,yesno("Copia "+STR(i,2)+" .¨Prepare el papel. Listo?"),.T.)
							!TYPE &p_fil >prn
						ENDIF
						IF LASTKEY()=27
							EXIT
						ENDIF
					ENDFOR
				ENDIF
			ENDIF
		ELSE
			!FOXSWAP wp &p_fil
		ENDIF

		IF _dest # "Archivo  "
			ERASE (p_fil)
		ENDIF
	ENDIF

	*   SS = INKEY(10)
	DEACTIVATE WINDOW _repo
ELSE
	DO standby WITH 'EL REPORTE HA SIDO CANCELADO.'
ENDIF
RELEASE WINDOW msg2use
ON ESCAPE
SET ESCAPE OFF

IF _tipo = 1
	CLOSE DATA
ENDIF

IF !EMPTY(_wndold)
	ACTIVATE WINDOW &_wndold
ELSE
	ACTIVATE SCREEN
ENDIF

SET CONSOLE &_conso

RETURN

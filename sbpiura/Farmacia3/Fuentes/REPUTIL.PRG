*--------------------------------
* RepUtil.Prg
* Autor : Marco E. Rodriguez Abad
*--------------------------------
CLOS DATA
USE Kardexv IN 1 ORDER TAG Kardexv1 ALIAS Kardex
USE IteII   IN 2 ORDER TAG IteII1   ALIAS IteII
USE fIteOC  IN 3 ORDER TAG fIteOC1  ALIAS IteOC
USE fIteOP  IN 4 ORDER TAG fIteOP1  ALIAS IteOP

vDbf = SYS(3)+'.Dbf'
vIdx = SYS(3)+'.Idx'
CREATE TABLE (vDbf) (CodArt C(12),DesArt C(60), CanCom N(5), PreComFr N(7,2), CanVen N(5), PreVenFr N(7,2))
USE (vDbf) IN 5 ALIAS Util
INDEX ON CodArt TO (vIdx)

DO inicia
DO salida
RETURN

PROCEDURE Inicia
*---------------
DEFINE WINDOW wlista FROM 3, 15 TO 10, 70 FLOAT TITLE  ;
       'Reporte de Utilidades de Ventas' DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista

mfecha = DATE()
@ 01, 01 SAY '      Fecha de Corte: ' GET mfecha VALID mfecha <= DATE()
@ 03, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION '*TH \!\<OK;\?\<Cancela'
READ CYCLE

RELEASE WINDOW wlista
DO espera WITH 1,'Procesando Utilidades'
IF LASTKEY()#27 AND okcancel#2
*set step on
	SELE Kardex
	SET FILTER TO Fecha<=mFecha
	GO TOP
	*set step on
	SCAN
*		if codart='62.032.0006'
*			SET STEP ON
*		ENDIF
		SCATTER MEMVAR BLANK
		m.Periodo = Periodo
		m.Item     = Item
		m.CodArt = CodArt
		m.DesArt = DesArt
		m.TipDoc  = TipDoc
		m.CanCom   = 0
		m.PreComFr = 0
		m.CanVen   = 0
		m.PreVenFr = 0
		DO CASE
			CASE m.TipDoc='INV'
				m.NumInv   = NumDoc
				SELE IteII
				IF SEEK(m.Periodo+m.NumInv+m.Item+m.CodArt)
					m.CanCom   = TotCan
					m.PreComFr = PreCom / totCan
				ENDIF
			CASE TipDoc='OPF'
				m.NumPed   = NumDoc
				SELE IteOP
				IF SEEK(m.Periodo+m.NumPed+m.Item+m.CodArt)
					m.CanVen   = TotCan
					m.PreVenFr = Total / totCan
				ENDIF
			CASE TipDoc='OCF'
		ENDCASE
		SELE Util
		IF !SEEK(m.CodArt)
			IF f_Appd()
				GATHER MEMVAR
			ENDIF
		ELSE
			DO CASE
				CASE m.TipDoc='INV'
					IF f_lock(1)
						REPLACE PreComFr WITH (PreComFr * CanCom + m.PreComFr * m.CanCom) / (CanCom + m.CanCom)
						REPLACE CanCom WITH CanCom+m.CanCom
					ENDIF
				CASE m.TipDoc='OPF'
					IF f_lock(1)
						REPLACE PreVenFr WITH (PreVenFr * CanVen + m.PreVenFr * m.CanVen) / (CanVen + m.CanVen)
						REPLACE CanVen WITH CanVen+m.CanVen
					ENDIF
			ENDCASE
		ENDIF
		SELE Kardex
	ENDSCAN
	DO espera WITH 2
	
	SELE Util
	IF !EOF()
		SET FILTER TO !EMPTY(PreVenFr) AND !EMPTY(CanVen)
		DO reporte WITH 2, 'Util', 'Reporte de Utilidades', 2, .F., .T.
	ENDIF
ENDIF
RETURN

PROCEDURE salida
*---------------
ACTIVATE SCREEN
CLOSE DATABASES
ERASE (vDbf)
ERASE (vIdx)
RETURN

*******************************
* DEPURA.PRG
* Prog para depurar la tablas
* m s importantes del sistema
* Integrado
*******************************
*cancel
********************************************
* POR FAVOR ANTES SACAR COPIA DE SEGURIDAD *
********************************************
*CANCEL
*SET STEP ON
set exclusive on
SET PATH TO \BENE2001\data
ON ERROR DO ctrlerr WITH ERROR()

DEFINE WINDOW Working   FROM 02,40 TO 23,76 FLOAT   COLOR SCHEME 1
ACTIVATE WINDOW Working

CLOSE DATA
SET EXCLUSIVE ON
SET TALK ON
_con = SET('CONSOLE')
SET CONSOLE ON

ACTIVATE WINDOW Working

*DO Limpia WITH "OrdCom"
*DO Limpia WITH "IteOC"
*DO Limpia WITH "IteOC1"
*DO Limpia WITH "OrdSer"
*DO Limpia WITH "IteOS1"
     DO Limpia WITH "Pecosa"
     DO Limpia WITH "ItePec"
*DO Limpia WITH "SolSer"
*DO Limpia WITH "Poliza"
*DO Limpia WITH "ItePol"
*DO Limpia WITH "IteArt"**************** no
*DO Limpia WITH "ArtMae"**************** no
*----------------------
*DO Limpia WITH "HojCon"
     DO Limpia WITH "IteHC"
*DO Limpia WITH "HojMod"
*----------------------
      DO Limpia WITH "ComPag"
DO Limpia WITH "IteCP"
DO Limpia WITH "Cheque"
DO Limpia WITH "Reten"
DO Limpia WITH "RecIng"
DO Limpia WITH "IteRI"
DO Limpia WITH "AstPat"
DO Limpia WITH "AstPre"
DO Limpia WITH "AstOrd"
*DO Limpia WITH "Auxil"**************** podria no ser
*DO Limpia WITH "Promae"*************** podria no ser
*----------------------
DO Limpia WITH "Compro"
DO Limpia WITH "Diario"
DO Limpia WITH "Saldona"
DO Limpia WITH "Folios"
*----------------------
DO Limpia WITH "StkAlm"
DO Limpia WITH "VenAlm"
DO Limpia WITH "DetVenAl"
DO Limpia WITH "MovFar"
DO Limpia WITH "VenBol"
DO Limpia WITH "DetBol"
*----------------------


RELEASE WINDOW Working
SET TALK OFF
SET EXCLUSIV OFF
ON ERROR 	&& restaurar el controlador de errores del sistema
RETURN

PROCEDURE Limpia
*---------------
PARAMETERS cArch

?'Abriendo Tabla '+cArch
lAbrio = USED(cArch)
DO WHILE !lAbrio
	USE (cArch)
	lAbrio = USED(cArch)
ENDDO

IF USED(cArch)
	PACK
	REINDEX
ENDIF

PROCEDURE ctrlerr
*----------------
PARAMETER merror
IF ERROR()=1705
	WAIT WIND MESSAGE()+ 'cArch... ESPERE O PRESIONE <ESC> PARA CANCELAR' TIMEOUT 0.5
	?? CHR(7)
	IF LASTKEY()=27
		CANCEL
	ENDIF
ENDIF

IF ERROR()=1
	WAIT WIND MESSAGE()+ 'cArch... ESPERE O PRESIONE <ESC> PARA CANCELAR' TIMEOUT 0.5
	?? CHR(7)
	IF LASTKEY()=27
		CANCEL
	ENDIF
ENDIF

RETURN

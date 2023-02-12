worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO Con_LIB
DO Fox_ambi
cia = 'SOCIEDAD DE BENEFICENCIA DE PIURA'

SET PATH TO H:\BeneData\SicgData\DATA2005
PUBLIC vNumMes,cAno,vOpcion,OkCancel,cProc

CLOS DATA

DO Pantalla
DO Escoge
IF !EMPTY(cProc)
	DO &cProc
ENDIF

RETURN


PROCEDURE Pantalla
*-----------------
USE Parmae IN 0 ORDER TAG Parmae1 ALIAS Parma
SELE Parma
DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1 TO vOpcion,OkCancel
cMes  = SPAC(2)
cAno  = YEAR(DATE())
@ 02,01 SAY "       Opciones : " 
@ 01,20 GET vOpcion  FUNCTION '^ Resumen de Afectaciones'
*@ 05,01 SAY "      Mes a Reportar : "
*@ 05,25 GET cMes Valid Val_para(cMes,'FECMES',' ',22)
@ 07,01 SAY "  Periodo a Reportar : "
@ 07,25 GET cAno PICTURE '9,999'
@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
USE IN Parma
RETURN

PROCEDURE Escoge
*---------------
IF LASTKEY()=27 OR OkCancel = 2
	cProc = ''
	RETURN
ENDIF
IF !EMPTY(cAno)
	cRuta = "H:\BeneData\SicgData\DATA"+IIF(cAno<2000,RIGHT(STR(cano,4),2),STR(cano,4))
 ELSE
	DO StandBy WITH "Ruta Especificada no es correcta"
	RETURN .T.
ENDIF
SET PATH TO &cRuta
DO CASE
	CASE vOpcion = 1
		cProc = 'ResHC'
	OTHERWISE
		cProc = ''
ENDCASE

RETURN

PROCEDURE ResHC
*--------------
*PARAMETERS vNumMes,vCodCad,vCodPart,vCodCla

vIdx  = SYS(3)+'.Idx'
vIdx1 = SYS(3)+'.Idx'

CLOS DATA
USE HOJCON IN 1 			&& order tag hojcon1
USE ITEHC  IN 2 ORDER TAG IteHC1
USE Clase  IN 3 ORDER TAG Clase2
USE ITECLA IN 4 ORDER TAG Itecla1
USE MaePre IN 5 ORDER TAG MaePre1
USE Parmae IN 6 ORDER TAG Parmae1 ALIAS Parma

SELE MaePre
INDEX ON CodCad TO (vIdx1)

SELE HojCon

SET RELATION TO NUMMES+NUMHC   INTO IteHC  ADDITIVE
SET SKIP TO IteHC
SELE itehc
SET RELATION TO CodCad         INTO MaePre ADDITIVE
SET RELATION TO CodPart        INTO Clase  ADDITIVE
SET RELATION TO CodPArt+CodCla INTO IteCla ADDITIVE

DEFINE WINDOW ResHC FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW ResHC

STORE 1 TO vOpcion,OkCancel
vNumMes1  = SPAC(2)
vNumMes2  = SPAC(2)
vCodCad  = SPAC(4)
vCodPart = SPACE(8)
vCodCla  = SPACE(4)
xAno = STR(cAno,4)

@ 01,01 SAY "  Meses a Reportar : "
@ 01,25 GET vNumMes1 Valid Val_para(vNumMes1,'FECMES',' ',22)
@ 01,40 GET vNumMes2 Valid Val_para(vNumMes2,'FECMES',' ',22)
@ 03,01 SAY "  Cadena : "
@ 03,25 GET vCodCad PICTURE "@!"
@ 05,01 SAY "  Partida : "
@ 05,25 GET vCodPart PICTURE "@!"
@ 07,01 SAY "  Calse : "
@ 07,25 GET vCodCla PICTURE "@!"
@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW ResHC

SELE hojcon
*INDEX ON NumMes+CodCad+IteHC.CodPart+NumHC TO (vIdx) FOR BETW(VAL(NumMes),01,12) AND IIF(!EMPTY(vCodCad),CodCad=vCodCad,.T.) AND IIF(!EMPTY(vCodPart),ALLTRIM(IteHC.CodPart)=ALLTRIM(vCodPart),.T.) AND IIF(!EMPTY(vCodCla),ALLTRIM(IteHC.CodCla)=ALLTRIM(vCodCla),.T.)
INDEX ON NumMes+CodCad+IteHC.CodPart+NumHC TO (vIdx) FOR BETWEEN(VAL(NumMes),VAL(vNumMes1),VAL(vNumMes2)) AND IIF(!EMPTY(vCodCad),CodCad=vCodCad,.T.) AND IIF(!EMPTY(vCodPart),IteHC.CodPart=vCodPart,.T.) AND IIF(!EMPTY(vCodCla),IteHC.CodCla=vCodCla,.T.)
*SET FILTER TO NumMes=vNumMes AND IIF(!EMPTY(vCodCad),CodCad=vCodCad,.T.) AND IIF(!EMPTY(vCodPart),IteHC.CodPart=vCodPart,.T.) AND IIF(!EMPTY(vCodCla),IteHC.CodCla=vCodCla,.T.)

do reporte with 2,"ResHC", "lista de afectaciones",2,.f.,.t.
CLOS DATA
ERASE (vIdx)
RETURN

PARAMETERS vNumMes,vCodCad,vCodPart,vCodCla

vIdx  = SYS(3)+'.Idx'
vIdx1 = SYS(3)+'.Idx'

CLOS DATA
USE HOJCON IN 1 			&& order tag hojcon1
USE ITEHC  IN 2 ORDER TAG IteHC1
USE Clase  IN 3 ORDER TAG Clase2
USE ITECLA IN 4 ORDER TAG Itecla1
USE MaePre IN 5 ORDER TAG MaePre1

SELE MaePre
INDEX ON CodCad TO (vIdx1)

SELE HojCon

SET RELATION TO NUMMES+NUMHC   INTO IteHC  ADDITIVE
SET SKIP TO IteHC
SELE itehc
SET RELATION TO CodCad         INTO MaePre ADDITIVE
SET RELATION TO CodPart        INTO Clase  ADDITIVE
SET RELATION TO CodPArt+CodCla INTO IteCla ADDITIVE

SELE hojcon
INDEX ON NumMes+CodCad+IteHC.CodPart+NumHC TO (vIdx) FOR NumMes=vNumMes AND IIF(!EMPTY(vCodCad),CodCad=vCodCad,.T.) AND IIF(!EMPTY(vCodPart),IteHC.CodPart=vCodPart,.T.) AND IIF(!EMPTY(vCodCla),IteHC.CodCla=vCodCla,.T.)
*SET FILTER TO NumMes=vNumMes AND IIF(!EMPTY(vCodCad),CodCad=vCodCad,.T.) AND IIF(!EMPTY(vCodPart),IteHC.CodPart=vCodPart,.T.) AND IIF(!EMPTY(vCodCla),IteHC.CodCla=vCodCla,.T.)

do reporte with 2,"ResHC", "lista de afectaciones",2,.f.,.t.
CLOS DATA
ERASE (vIdx)
RETURN

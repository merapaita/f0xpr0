PARAMETERS vNumMes,vCodPart

CLOS DATA
USE RecIng  IN 1 ORDER TAG Recing1
USE IteRI   IN 2 ORDER TAG IteRI1
USE Ingreso IN 3 ORDER TAG Ingreso1

SET RELATION TO recing.periodo+ recing.nummes+ recing.numri INTO ITERI ADDITIVE
SELE Iteri
SET RELATION TO Iteri.CodPart INTO Ingreso ADDITIVE
SELE RecIng
SET SKIP TO IteRI

SET FILTER TO NUMMES=vNumMes AND Iteri.COdPart=vCOdPart

DO Reporte WITH 2,"ResRI","Resumen de recibos de Ingreso Por Servicio",2,.F.,.T.

CLOS DATA

RETURN
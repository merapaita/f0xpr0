*************************************
* rutina para ver el seguimiento de 
* su c/p con h/c y o/s o o/c
* para que la sra francisca ddj
*************************************

SET DELE ON
SET EXCLU OFF
SET PATH TO H:\SicgData\DATA2010

CREATE TABLE Tempo (Periodo C(2),NumMes C(2), NumCP C(4), FecCp D(8),TipDoc C(2),NHC C(8),COd C(30),FHC D(8),TipDocHC C(3),NumRefHC C(4),DesRefHC C(40),NomPre C(40),ImpCP N(10,2),ImpHC N(10,2),ImpOC N(10,2),ImpOS N(10,2))

CLOS DATA
USE compag IN 1 ORDER TAG Compag1
USE hojcon IN 2 ORDER TAG HojCon1
USE OrdCom IN 3 ORDER TAG OrdCom1
USE Ordser IN 4 ORDER TAG Ordser1
USE Tempo  IN 5 ALIAS RESUMEN

SET RELATION TO NumMesHC + numHC INTO HojCon ADDITIVE
SELE HojCon
SET RELATION TO Periodo + NumRef INTO OrdCom ADDITIVE
SET RELATION TO Periodo + NumRef INTO OrdSer ADDITIVE

SELE ComPag
GO TOP

SCAN
	m.Periodo  = ComPag.Periodo
	m.NumMes   = Compag.NumMes
	m.NumCP    = ComPag.NumCP
	m.FecCP    = ComPag.FecCP
	m.TipDoc   = ComPag.TipDoc
	m.NHC      = IIF(ComPag.TipDoc = 'HC',NumRef,"")
	m.COD      = IIF(TipDoc # 'HC',NumRef+' '+Refer,'')
	m.fhc      = IIF(TipDoc = 'HC',HojCon.FecHC,FecRef)
	m.TipDocHC = HojCon.TipDoc
	m.NumRefHC = HojCon.NumRef
	m.DesrefHC = HojCon.DesRef
	m.NomPre   = ComPag.NomPre
	m.ImpCP    = ComPag.Import
	m.ImpHC    = HojCon.ImpTot
	m.ImpOC    = IIF(HojCon.TipDoc = 'O/C',OrdCom.ValTot,"")
	m.ImpOS    = IIF(HojCon.TipDoc = 'O/S',OrdSer.ValTot,"")
	
	SELE Resumen
	APPEND BLANK
	GATHER MEMVAR
	SELE ComPag
	
ENDSCAN
* BROW NORM NODELETE NOMODIFY FIELD PERIODO,NUMMES,NUMCP,FECCP,TipDoc,;
*	NHC  = IIF(TipDoc = 'HC',NumRef,"")	:8,;
*	COD  = IIF(TipDoc # 'HC',NumRef+' '+Refer,''),;
*	fhc  = IIF(TipDoc = 'HC',HojCon.FecHC,FecRef),;
*	HojCon.TipDoc,;
*	HojCon.NumRef,;
*	HojCon.DesRef,;
*	NomPre,;
*	Import	:H="Imp CP",;
*	HojCon.ImpTot	:H="Imp HC",;
*	ImpOC = IIF(HojCon.TipDoc = 'O/C',OrdCom.ValTot,"")	:H="Imp OC",;
*	ImpOS = IIF(HojCon.TipDoc = 'O/S',OrdSer.ValTot,"")	:H="Imp OS"
	
*CLOS DATA
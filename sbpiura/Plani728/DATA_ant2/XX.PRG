SET DELE ON
CLOS DATA
USE PERSONAL IN 1 ORDER TAG PERSONA3
USE totapor  IN 2 ORDER TAG totapor1
SET RELATION TO LIB_ELE INTO totapor
SET FILTER TO !(ESTADO ='03' OR EMPTY(APE_NOM))
GO TOP
SET STEP ON
SCAN
	SELE totapor
	seek PERSONAL.LIB_ELE+'07'
	if found()
		mapo01 = apo_01
		mapo02 = apo_02
	 ELSE
		mapo01 = 0
		mapo02 = 0
	endif
	seek PERSONAL.LIB_ELE+'05'
	if found()
		replace apo_01 WITH mApo01
		replace apo_02 WITH mApo02
	endif
	sele personal
ENDSCAN

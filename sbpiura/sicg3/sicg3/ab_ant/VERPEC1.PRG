CLOSE DATA
SELE 1
USE ITEPEC ORDER TAG 1
SELE 2
USE PECOSA ORDER TAG 1
SELE 1
SET RELA TO PERIODO+NUMPEC INTO 2
GO TOP
SCAN FOR ITEPEC.CODFTE#PECOSA.CODFTE
? ITEPEC.NUMPEC,ITEPEC.CODFTE,PECOSA.CODFTE
ENDSCAN

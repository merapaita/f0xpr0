
do Pla_lib
set path to ..\data

CLOS DATA
USE Personal IN 1 ORDER TAG
USE ItePer   IN 2 ORDER TAG ItePer1
USE TotApor  IN 3 ORDER TAG TotApor1
USE ParMae   IN 4 ORDER TAG ParMae1
USE Planilla IN 5 ORDER TAG Planilla1

INDEX ON NUM_PLA TO XX
SET FILTER TO Estado='00' AND Con_Per='5' AND Rec_Per='1'

SELE ItePer
SET FILTER TO Mes_Sue='07'

SELE Personal
SET RELATION TO Lib_Ele INTO ItePer
SET RELATION TO Lib_Ele INTO TotApor ADDITIVE



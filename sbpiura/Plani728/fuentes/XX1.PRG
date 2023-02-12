SET PATH TO ..\DATA
CLOS DATA
USE PERSONAL IN 1 ORDER TAG PERSONA3
USE ITEPER  IN 2 ORDER TAG ITEPER1
USE TOTAPOR IN 3 ORDER TAG TOTAPOR1
SET RELATION TO Lib_Ele into iteper
set relation to lib_ele into totApor additive
sele personal
*brow last nowait
sele iteper
*brow last nowait
SET FILTER TO MES_SUE='11'
sele TOTAPOR
*brow last nowait
SELE PERSONAL

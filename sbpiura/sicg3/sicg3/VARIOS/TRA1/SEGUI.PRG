cProc = "AbrTablas"
IF !EMPTY(cProc)
	DO &cProc
ENDIF

PROCEDURE AbrTablas
*------------------
CLOS DATA
USE Ventas   IN 1
USE PagACta  IN 1
USE Creditos IN 1
USE KarCre   IN 1
USE Subvenc  IN 1
USE Caja     IN 1
